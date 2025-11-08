pt: Zcu.PerThread,
target: *const std.Target,
air: Air,
nav_index: InternPool.Nav.Index,

saved_registers: std.enums.EnumSet(Register),
instructions: std.ArrayList(Instruction),
nav_relocs: std.ArrayListUnmanaged(Mir.Reloc.Nav),
uav_relocs: std.ArrayListUnmanaged(Mir.Reloc.Uav),
global_relocs: std.ArrayListUnmanaged(Mir.Reloc.Global),

def_order: std.AutoArrayHashMapUnmanaged(Air.Inst.Index, void),
blocks: std.AutoArrayHashMapUnmanaged(Air.Inst.Index, Block),
loops: std.AutoArrayHashMapUnmanaged(Air.Inst.Index, Loop),
active_loops: std.ArrayListUnmanaged(Loop.Index),
loop_live: struct {
    set: std.AutoArrayHashMapUnmanaged(struct { Loop.Index, Air.Inst.Index }, void),
    list: std.ArrayListUnmanaged(Air.Inst.Index),
},
dom_start: u32,
dom_len: u32,
dom: std.ArrayListUnmanaged(DomInt),

returns: bool,
stack_size: u26,
stack_align: InternPool.Alignment,

live_registers: LiveRegisters,
live_values: std.AutoHashMapUnmanaged(Air.Inst.Index, Value.Index),
values: std.ArrayListUnmanaged(Value),

pub const LiveRegisters = std.enums.EnumArray(Register, Value.Index);

pub const CallAbiIterator = struct {
    next_gp: Register,
    next_addr: u24,

    pub const gp_start: Register = .a0; // argument 0
    pub const gp_end: Register = .a7; // last argument register

    pub const init: CallAbiIterator = .{
        .next_gp = gp_start,
        .next_addr = 0,
    };

    pub fn param(it: *CallAbiIterator, isel: *Select, ty: ZigType) !?Value.Index {
        const zcu = isel.pt.zcu;
        const ip = &zcu.intern_pool;

        if (ty.isNoReturn(zcu) or !ty.hasRuntimeBitsIgnoreComptime(zcu)) return null;
        try isel.values.ensureUnusedCapacity(zcu.gpa, Value.max_parts);
        const wip_vi = isel.initValue(ty);
        type_key: switch (ip.indexToKey(ty.toIntern())) {
            else => |t| return isel.fail("CallAbiIterator.param({f}) {t}", .{ isel.fmtType(ty), t }),
            .int_type => |int_type| switch (int_type.bits) {
                0 => unreachable,
                1...31 => {
                    wip_vi.setSignedness(isel, int_type.signedness);
                    it.integer(isel, wip_vi);
                },
                32...64 => it.integer(isel, wip_vi),
                65...128 => it.integers(isel, wip_vi, @splat(@divExact(wip_vi.size(isel), 2))),
                else => it.indirect(isel, wip_vi),
            },
            .ptr_type => |ptr_type| switch (ptr_type.flags.size) {
                .one, .many, .c => continue :type_key .{ .int_type = .{
                    .signedness = .unsigned,
                    .bits = 64,
                } },
                .slice => it.integers(isel, wip_vi, @splat(8)),
            },
            .opt_type => |child_type| if (ty.optionalReprIsPayload(zcu))
                continue :type_key ip.indexToKey(child_type)
            else switch (ZigType.fromInterned(child_type).abiSize(zcu)) {
                0 => continue :type_key .{ .simple_type = .bool },
                1...7 => it.integer(isel, wip_vi),
                8...15 => |child_size| it.integers(isel, wip_vi, .{ 8, child_size - 7 }),
                else => return isel.fail("CallAbiIterator.param({f})", .{isel.fmtType(ty)}),
            },
            .error_union_type => |error_union_type| switch (wip_vi.size(isel)) {
                0 => unreachable,
                1...8 => it.integer(isel, wip_vi),
                9...16 => {
                    var sizes: [2]u64 = @splat(0);
                    const payload_ty: ZigType = .fromInterned(error_union_type.payload_type);
                    {
                        const error_set_ty: ZigType = .fromInterned(error_union_type.error_set_type);
                        const offset = codegen.errUnionErrorOffset(payload_ty, zcu);
                        const end = offset % 8 + error_set_ty.abiSize(zcu);
                        const part_index: usize = @intCast(offset / 8);
                        sizes[part_index] = @max(sizes[part_index], @min(end, 8));
                        if (end > 8) sizes[part_index + 1] = @max(sizes[part_index + 1], end - 8);
                    }
                    {
                        const offset = codegen.errUnionPayloadOffset(payload_ty, zcu);
                        const end = offset % 8 + payload_ty.abiSize(zcu);
                        const part_index: usize = @intCast(offset / 8);
                        sizes[part_index] = @max(sizes[part_index], @min(end, 8));
                        if (end > 8) sizes[part_index + 1] = @max(sizes[part_index + 1], end - 8);
                    }
                    it.integers(isel, wip_vi, sizes);
                },
                else => it.indirect(isel, wip_vi),
            },
            .simple_type => |simple_type| switch (simple_type) {
                .usize,
                .isize,
                .c_char,
                .c_short,
                .c_ushort,
                .c_int,
                .c_uint,
                .c_long,
                .c_ulong,
                .c_longlong,
                .c_ulonglong,
                => continue :type_key .{ .int_type = ty.intInfo(zcu) },
                .void,
                .type,
                .comptime_int,
                .comptime_float,
                .noreturn,
                .null,
                .undefined,
                .enum_literal,
                .adhoc_inferred_error_set,
                .generic_poison,
                => unreachable,
                .bool => continue :type_key .{ .int_type = .{ .signedness = .unsigned, .bits = 1 } },
                else => |t| return isel.fail("CallAbiIterator.param simple '{t}'", .{t}),
            },
            .enum_type => continue :type_key ip.indexToKey(ip.loadEnumType(ty.toIntern()).tag_ty),
            .error_set_type,
            .inferred_error_set_type,
            => continue :type_key .{ .simple_type = .anyerror },
            .tuple_type => |tuple_type| {
                const size = wip_vi.size(isel);
                switch (size) {
                    0 => unreachable,
                    1...8 => it.integer(isel, wip_vi),
                    9...16 => {
                        var part_offset: u64 = 0;
                        var part_sizes: [2]u64 = undefined;
                        var parts_len: Value.PartsLen = 0;
                        var next_field_end: u64 = 0;
                        var field_index: usize = 0;
                        while (part_offset < size) {
                            const field_end = next_field_end;
                            const next_field_begin = while (field_index < tuple_type.types.len) {
                                defer field_index += 1;
                                if (tuple_type.values.get(ip)[field_index] != .none) continue;
                                const field_ty: ZigType = .fromInterned(tuple_type.types.get(ip)[field_index]);
                                const next_field_begin = field_ty.abiAlignment(zcu).forward(field_end);
                                next_field_end = next_field_begin + field_ty.abiSize(zcu);
                                break next_field_begin;
                            } else std.mem.alignForward(u64, size, 8);
                            while (next_field_begin - part_offset >= 8) {
                                const part_size = @min(field_end - part_offset, 8);
                                part_sizes[parts_len] = part_size;
                                assert(part_offset + part_size <= size);
                                parts_len += 1;
                                part_offset += part_size;
                                if (part_offset >= field_end) part_offset = next_field_begin;
                            }
                        }
                        assert(parts_len == part_sizes.len);
                        it.integers(isel, wip_vi, part_sizes);
                    },
                    else => it.indirect(isel, wip_vi),
                }
            },
            .undef,
            .simple_value,
            .variable,
            .@"extern",
            .func,
            .int,
            .err,
            .error_union,
            .enum_literal,
            .enum_tag,
            .empty_enum_value,
            .float,
            .ptr,
            .slice,
            .opt,
            .aggregate,
            .un,
            .memoized_call,
            => unreachable, // values, not types
        }
        return wip_vi.ref(isel);
    }

    pub fn ret(it: *CallAbiIterator, isel: *Select, ty: ZigType) !?Value.Index {
        const wip_vi = try it.param(isel, ty) orelse return null;
        switch (wip_vi.parent(isel)) {
            .unallocated, .stack_slot => {},
            .value, .constant => unreachable,
            .address => |address_vi| {
                assert(address_vi.hint(isel) == gp_start);
                address_vi.setHint(isel, gp_end);
            },
        }
        return wip_vi;
    }

    pub const FundamentalDataType = enum {
        half,
        single,
        double,
        quad,
        vector64,
        vector128,

        fn log2Size(fdt: FundamentalDataType) u3 {
            return switch (fdt) {
                .half => 1,
                .single => 2,
                .double, .vector64 => 3,
                .quad, .vector128 => 4,
            };
        }
        fn size(fdt: FundamentalDataType) u64 {
            return @as(u64, 1) << fdt.log2Size();
        }
    };
    fn homogeneousAggregateBaseType(zcu: *Zcu, initial_ty: InternPool.Index) ?FundamentalDataType {
        const ip = &zcu.intern_pool;
        var ty = initial_ty;
        return type_key: switch (ip.indexToKey(ty)) {
            else => null,
            .array_type => |array_type| {
                ty = array_type.child;
                continue :type_key ip.indexToKey(ty);
            },
            .vector_type => switch (ZigType.fromInterned(ty).abiSize(zcu)) {
                else => null,
                8 => .vector64,
                16 => .vector128,
            },
            .simple_type => |simple_type| switch (simple_type) {
                .f16 => .half,
                .f32 => .single,
                .f64 => .double,
                .f128 => .quad,
                .c_longdouble => switch (zcu.getTarget().cTypeBitSize(.longdouble)) {
                    else => unreachable,
                    16 => .half,
                    32 => .single,
                    64 => .double,
                    80 => null,
                    128 => .quad,
                },
                else => null,
            },
            .struct_type => homogeneousStructBaseType(zcu, &ip.loadStructType(ty)),
            .tuple_type => |tuple_type| homogeneousTupleBaseType(zcu, tuple_type),
        };
    }
    fn homogeneousStructBaseType(zcu: *Zcu, loaded_struct: *const InternPool.LoadedStructType) ?FundamentalDataType {
        const ip = &zcu.intern_pool;
        var common_fdt: ?FundamentalDataType = null;
        for (0.., loaded_struct.field_types.get(ip)) |field_index, field_ty| {
            if (loaded_struct.fieldIsComptime(ip, field_index)) continue;
            if (loaded_struct.fieldAlign(ip, field_index) != .none) return null;
            if (!ZigType.fromInterned(field_ty).hasRuntimeBits(zcu)) continue;
            const fdt = homogeneousAggregateBaseType(zcu, field_ty);
            if (common_fdt == null) common_fdt = fdt else if (fdt != common_fdt) return null;
        }
        return common_fdt;
    }
    fn homogeneousTupleBaseType(zcu: *Zcu, tuple_type: InternPool.Key.TupleType) ?FundamentalDataType {
        const ip = &zcu.intern_pool;
        var common_fdt: ?FundamentalDataType = null;
        for (tuple_type.values.get(ip), tuple_type.types.get(ip)) |field_val, field_ty| {
            if (field_val != .none) continue;
            const fdt = homogeneousAggregateBaseType(zcu, field_ty);
            if (common_fdt == null) common_fdt = fdt else if (fdt != common_fdt) return null;
        }
        return common_fdt;
    }

    fn stack(it: *CallAbiIterator, isel: *Select, wip_vi: Value.Index) void {
        it.next_addr = @intCast(wip_vi.alignment(isel).forward(it.next_addr));
        const parent_vi = switch (wip_vi.parent(isel)) {
            .unallocated, .stack_slot => wip_vi,
            .address, .constant => unreachable,
            .value => |parent_vi| parent_vi,
        };
        switch (parent_vi.parent(isel)) {
            .unallocated => parent_vi.setParent(isel, .{ .stack_slot = .{
                .base = .sp,
                .offset = it.next_addr,
            } }),
            .stack_slot => {},
            .address, .value, .constant => unreachable,
        }
        it.next_addr += @intCast(wip_vi.size(isel));
    }

    fn integer(it: *CallAbiIterator, isel: *Select, wip_vi: Value.Index) void {
        assert(wip_vi.size(isel) <= 8);
        const natural_alignment = wip_vi.alignment(isel);
        assert(natural_alignment.order(.@"16").compare(.lte));
        wip_vi.setAlignment(isel, natural_alignment.maxStrict(.@"8"));
        if (it.next_gp == gp_end) return it.stack(isel, wip_vi);
        wip_vi.setHint(isel, it.next_gp);
        it.next_gp = @enumFromInt(@intFromEnum(it.next_gp) + 1);
    }

    fn integers(it: *CallAbiIterator, isel: *Select, wip_vi: Value.Index, part_sizes: [2]u64) void {
        assert(wip_vi.size(isel) <= 16);
        const natural_alignment = wip_vi.alignment(isel);
        assert(natural_alignment.order(.@"16").compare(.lte));
        wip_vi.setAlignment(isel, natural_alignment.maxStrict(.@"8"));
        if (natural_alignment == .@"16") it.next_gp = @enumFromInt(std.mem.alignForward(
            @typeInfo(Register).@"enum".tag_type,
            @intFromEnum(it.next_gp),
            2,
        ));
        if (it.next_gp == gp_end) std.debug.panic("TODO: pass large by stack", .{});
        wip_vi.setParts(isel, part_sizes.len);
        for (0.., part_sizes) |part_index, part_size|
            it.integer(isel, wip_vi.addPart(isel, 8 * part_index, part_size));
    }

    fn indirect(it: *CallAbiIterator, isel: *Select, wip_vi: Value.Index) void {
        const wip_address_vi = isel.initValue(.usize);
        wip_vi.setParent(isel, .{ .address = wip_address_vi });
        it.integer(isel, wip_address_vi);
    }
};

pub const Block = struct {
    live_registers: LiveRegisters,
    target_label: u32,

    pub const main: Air.Inst.Index = @enumFromInt(
        std.math.maxInt(@typeInfo(Air.Inst.Index).@"enum".tag_type),
    );

    fn branch(target_block: *const Block, isel: *Select) !void {
        if (isel.instructions.items.len > target_block.target_label) {
            try isel.emit(.jal(.zero, @intCast((isel.instructions.items.len + 1 - target_block.target_label) * 4)));
        }
        try isel.merge(&target_block.live_registers, .{});
    }
};

const DomInt = u8;

pub const Loop = struct {
    def_order: u32,
    dom: u32,
    depth: u32,
    live: u32,
    live_registers: LiveRegisters,
    repeat_list: u32,

    pub const invalid: Air.Inst.Index = @enumFromInt(
        std.math.maxInt(@typeInfo(Air.Inst.Index).@"enum".tag_type),
    );

    pub const Index = enum(u32) {
        _,

        fn inst(li: Loop.Index, isel: *Select) Air.Inst.Index {
            return isel.loops.keys()[@intFromEnum(li)];
        }

        fn get(li: Loop.Index, isel: *Select) *Loop {
            return &isel.loops.values()[@intFromEnum(li)];
        }
    };

    pub const empty_list: u32 = std.math.maxInt(u32);

    fn branch(target_loop: *Loop, isel: *Select) !void {
        try isel.instructions.ensureUnusedCapacity(isel.pt.zcu.gpa, 1);
        const repeat_list_tail = target_loop.repeat_list;
        target_loop.repeat_list = @intCast(isel.instructions.items.len);
        isel.instructions.appendAssumeCapacity(@bitCast(repeat_list_tail));
        try isel.merge(&target_loop.live_registers, .{});
    }
};

pub fn deinit(isel: *Select) void {
    const gpa = isel.pt.zcu.gpa;

    isel.def_order.deinit(gpa);
    isel.blocks.deinit(gpa);
    isel.loops.deinit(gpa);
    isel.active_loops.deinit(gpa);
    isel.loop_live.set.deinit(gpa);
    isel.loop_live.list.deinit(gpa);
    isel.dom.deinit(gpa);

    isel.instructions.deinit(gpa);
    isel.nav_relocs.deinit(gpa);
    isel.uav_relocs.deinit(gpa);
    isel.global_relocs.deinit(gpa);

    isel.live_values.deinit(gpa);
    isel.values.deinit(gpa);

    isel.* = undefined;
}

/// NOTE: directly copied from aarch64/Select.zig
pub fn analyze(isel: *Select, air_body: []const Air.Inst.Index) !void {
    const zcu = isel.pt.zcu;
    const ip = &zcu.intern_pool;
    const gpa = zcu.gpa;
    const air_tags = isel.air.instructions.items(.tag);
    const air_data = isel.air.instructions.items(.data);
    var air_body_index: usize = 0;
    var air_inst_index = air_body[air_body_index];
    const initial_def_order_len = isel.def_order.count();
    air_tag: switch (air_tags[@intFromEnum(air_inst_index)]) {
        // No "scalarize" legalizations are enabled, so these instructions never appear.
        .legalize_vec_elem_val => unreachable,
        .legalize_vec_store_elem => unreachable,
        // No "soft_" legalizations are enabled.
        .legalize_compiler_rt_call => {
            const inst_data = air_data[@intFromEnum(air_inst_index)].legalize_compiler_rt_call;
            const extra = isel.air.extraData(Air.Call, inst_data.payload);
            const args: []const Air.Inst.Ref = @ptrCast(isel.air.extra.items[extra.end..][0..extra.data.args_len]);

            var param_it: CallAbiIterator = .init;
            for (args) |arg| {
                const restore_values_len = isel.values.items.len;
                defer isel.values.shrinkRetainingCapacity(restore_values_len);
                const param_vi = param_vi: {
                    const param_ty = isel.air.typeOf(arg, ip);
                    break :param_vi try param_it.param(isel, param_ty);
                } orelse continue;
                defer param_vi.deref(isel);
                const passed_vi = switch (param_vi.parent(isel)) {
                    .unallocated, .stack_slot => param_vi,
                    .value, .constant => unreachable,
                    .address => |address_vi| address_vi,
                };
                switch (passed_vi.parent(isel)) {
                    .unallocated => {},
                    .stack_slot => |stack_slot| {
                        assert(stack_slot.base == .sp);
                        isel.stack_size = @max(
                            isel.stack_size,
                            stack_slot.offset + @as(u26, @intCast(passed_vi.size(isel))),
                        );
                    },
                    .value, .constant, .address => unreachable,
                }
                try isel.analyzeUse(arg);
            }

            var ret_it: CallAbiIterator = .init;
            if (try ret_it.ret(isel, inst_data.func.returnType())) |ret_vi| {
                tracking_log.debug("${d} <- %{d}", .{ @intFromEnum(ret_vi), @intFromEnum(air_inst_index) });
                switch (ret_vi.parent(isel)) {
                    .unallocated, .stack_slot => {},
                    .value, .constant => unreachable,
                    .address => |address_vi| {
                        defer address_vi.deref(isel);
                        const ret_value = ret_vi.get(isel);
                        ret_value.flags.parent_tag = .unallocated;
                        ret_value.parent_payload = .{ .unallocated = {} };
                    },
                }
                try isel.live_values.putNoClobber(gpa, air_inst_index, ret_vi);
                try isel.def_order.putNoClobber(gpa, air_inst_index, {});
            }

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },

        .arg,
        .ret_addr,
        .frame_addr,
        .err_return_trace,
        .save_err_return_trace_index,
        .runtime_nav_ptr,
        .c_va_start,
        => {
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .add,
        .add_safe,
        .add_optimized,
        .add_wrap,
        .add_sat,
        .sub,
        .sub_safe,
        .sub_optimized,
        .sub_wrap,
        .sub_sat,
        .mul,
        .mul_safe,
        .mul_optimized,
        .mul_wrap,
        .mul_sat,
        .div_float,
        .div_float_optimized,
        .div_trunc,
        .div_trunc_optimized,
        .div_floor,
        .div_floor_optimized,
        .div_exact,
        .div_exact_optimized,
        .rem,
        .rem_optimized,
        .mod,
        .mod_optimized,
        .max,
        .min,
        .bit_and,
        .bit_or,
        .shr,
        .shr_exact,
        .shl,
        .shl_exact,
        .shl_sat,
        .xor,
        .cmp_lt,
        .cmp_lt_optimized,
        .cmp_lte,
        .cmp_lte_optimized,
        .cmp_eq,
        .cmp_eq_optimized,
        .cmp_gte,
        .cmp_gte_optimized,
        .cmp_gt,
        .cmp_gt_optimized,
        .cmp_neq,
        .cmp_neq_optimized,
        .bool_and,
        .bool_or,
        .array_elem_val,
        .slice_elem_val,
        .ptr_elem_val,
        => {
            const bin_op = air_data[@intFromEnum(air_inst_index)].bin_op;

            try isel.analyzeUse(bin_op.lhs);
            try isel.analyzeUse(bin_op.rhs);
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .ptr_add,
        .ptr_sub,
        .add_with_overflow,
        .sub_with_overflow,
        .mul_with_overflow,
        .shl_with_overflow,
        .slice,
        .slice_elem_ptr,
        .ptr_elem_ptr,
        => {
            const ty_pl = air_data[@intFromEnum(air_inst_index)].ty_pl;
            const bin_op = isel.air.extraData(Air.Bin, ty_pl.payload).data;

            try isel.analyzeUse(bin_op.lhs);
            try isel.analyzeUse(bin_op.rhs);
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .alloc => {
            const ty = air_data[@intFromEnum(air_inst_index)].ty;

            isel.stack_align = isel.stack_align.maxStrict(ty.ptrAlignment(zcu));
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .inferred_alloc,
        .inferred_alloc_comptime,
        .wasm_memory_size,
        .wasm_memory_grow,
        .work_item_id,
        .work_group_size,
        .work_group_id,
        => unreachable,
        .ret_ptr => {
            const ty = air_data[@intFromEnum(air_inst_index)].ty;

            if (isel.live_values.get(Block.main)) |ret_vi| switch (ret_vi.parent(isel)) {
                .unallocated, .stack_slot => isel.stack_align = isel.stack_align.maxStrict(ty.ptrAlignment(zcu)),
                .value, .constant => unreachable,
                .address => |address_vi| try isel.live_values.putNoClobber(gpa, air_inst_index, address_vi.ref(isel)),
            };
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .assembly => {
            const ty_pl = air_data[@intFromEnum(air_inst_index)].ty_pl;
            const extra = isel.air.extraData(Air.Asm, ty_pl.payload);
            const operands: []const Air.Inst.Ref = @ptrCast(isel.air.extra.items[extra.end..][0 .. extra.data.flags.outputs_len + extra.data.inputs_len]);

            for (operands) |operand| if (operand != .none) try isel.analyzeUse(operand);
            if (ty_pl.ty != .void_type) try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .not,
        .clz,
        .ctz,
        .popcount,
        .byte_swap,
        .bit_reverse,
        .abs,
        .load,
        .fptrunc,
        .fpext,
        .intcast,
        .intcast_safe,
        .trunc,
        .optional_payload,
        .optional_payload_ptr,
        .optional_payload_ptr_set,
        .wrap_optional,
        .unwrap_errunion_payload,
        .unwrap_errunion_err,
        .unwrap_errunion_payload_ptr,
        .unwrap_errunion_err_ptr,
        .errunion_payload_ptr_set,
        .wrap_errunion_payload,
        .wrap_errunion_err,
        .struct_field_ptr_index_0,
        .struct_field_ptr_index_1,
        .struct_field_ptr_index_2,
        .struct_field_ptr_index_3,
        .get_union_tag,
        .ptr_slice_len_ptr,
        .ptr_slice_ptr_ptr,
        .array_to_slice,
        .int_from_float,
        .int_from_float_optimized,
        .int_from_float_safe,
        .int_from_float_optimized_safe,
        .float_from_int,
        .splat,
        .error_set_has_value,
        .addrspace_cast,
        .c_va_arg,
        .c_va_copy,
        => {
            const ty_op = air_data[@intFromEnum(air_inst_index)].ty_op;

            try isel.analyzeUse(ty_op.operand);
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .bitcast => {
            const ty_op = air_data[@intFromEnum(air_inst_index)].ty_op;
            maybe_noop: {
                if (ty_op.ty.toInterned().? != isel.air.typeOf(ty_op.operand, ip).toIntern()) break :maybe_noop;
                if (true) break :maybe_noop;
                if (ty_op.operand.toIndex()) |src_air_inst_index| {
                    if (isel.hints.get(src_air_inst_index)) |hint_vpsi| {
                        try isel.hints.putNoClobber(gpa, air_inst_index, hint_vpsi);
                    }
                }
            }
            try isel.analyzeUse(ty_op.operand);
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        inline .block, .dbg_inline_block => |air_tag| {
            const ty_pl = air_data[@intFromEnum(air_inst_index)].ty_pl;
            const extra = isel.air.extraData(switch (air_tag) {
                else => comptime unreachable,
                .block => Air.Block,
                .dbg_inline_block => Air.DbgInlineBlock,
            }, ty_pl.payload);
            const result_ty = ty_pl.ty.toInterned().?;

            if (result_ty == .noreturn_type) {
                try isel.analyze(@ptrCast(isel.air.extra.items[extra.end..][0..extra.data.body_len]));

                air_body_index += 1;
                break :air_tag;
            }

            assert(!(try isel.blocks.getOrPut(gpa, air_inst_index)).found_existing);
            try isel.analyze(@ptrCast(isel.air.extra.items[extra.end..][0..extra.data.body_len]));
            const block_entry = isel.blocks.pop().?;
            assert(block_entry.key == air_inst_index);

            if (result_ty != .void_type) try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .loop => {
            const ty_pl = air_data[@intFromEnum(air_inst_index)].ty_pl;
            const extra = isel.air.extraData(Air.Block, ty_pl.payload);

            const initial_dom_start = isel.dom_start;
            const initial_dom_len = isel.dom_len;
            isel.dom_start = @intCast(isel.dom.items.len);
            isel.dom_len = @intCast(isel.blocks.count());
            try isel.active_loops.append(gpa, @enumFromInt(isel.loops.count()));
            try isel.loops.putNoClobber(gpa, air_inst_index, .{
                .def_order = @intCast(isel.def_order.count()),
                .dom = isel.dom_start,
                .depth = isel.dom_len,
                .live = 0,
                .live_registers = undefined,
                .repeat_list = undefined,
            });
            try isel.dom.appendNTimes(gpa, 0, std.math.divCeil(usize, isel.dom_len, @bitSizeOf(DomInt)) catch unreachable);
            try isel.analyze(@ptrCast(isel.air.extra.items[extra.end..][0..extra.data.body_len]));
            for (
                isel.dom.items[initial_dom_start..].ptr,
                isel.dom.items[isel.dom_start..][0 .. std.math.divCeil(usize, initial_dom_len, @bitSizeOf(DomInt)) catch unreachable],
            ) |*initial_dom, loop_dom| initial_dom.* |= loop_dom;
            isel.dom_start = initial_dom_start;
            isel.dom_len = initial_dom_len;
            assert(isel.active_loops.pop().?.inst(isel) == air_inst_index);

            air_body_index += 1;
        },
        .repeat, .trap, .unreach => air_body_index += 1,
        .br => {
            const br = air_data[@intFromEnum(air_inst_index)].br;
            const block_index = isel.blocks.getIndex(br.block_inst).?;
            if (block_index < isel.dom_len) isel.dom.items[isel.dom_start + block_index / @bitSizeOf(DomInt)] |= @as(DomInt, 1) << @truncate(block_index);
            try isel.analyzeUse(br.operand);

            air_body_index += 1;
        },
        .breakpoint, .dbg_stmt, .dbg_empty_stmt, .dbg_var_ptr, .dbg_var_val, .dbg_arg_inline, .c_va_end => {
            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .call,
        .call_always_tail,
        .call_never_tail,
        .call_never_inline,
        => {
            const pl_op = air_data[@intFromEnum(air_inst_index)].pl_op;
            const extra = isel.air.extraData(Air.Call, pl_op.payload);
            const args: []const Air.Inst.Ref = @ptrCast(isel.air.extra.items[extra.end..][0..extra.data.args_len]);
            const callee_ty = isel.air.typeOf(pl_op.operand, ip);
            const func_info = switch (ip.indexToKey(callee_ty.toIntern())) {
                else => unreachable,
                .func_type => |func_type| func_type,
                .ptr_type => |ptr_type| ip.indexToKey(ptr_type.child).func_type,
            };

            try isel.analyzeUse(pl_op.operand);
            var param_it: CallAbiIterator = .init;
            for (args, 0..) |arg, arg_index| {
                const restore_values_len = isel.values.items.len;
                defer isel.values.shrinkRetainingCapacity(restore_values_len);
                const param_vi = param_vi: {
                    const param_ty = isel.air.typeOf(arg, ip);
                    if (arg_index >= func_info.param_types.len) {
                        assert(func_info.is_var_args);
                        return isel.fail("TODO: var args", .{});
                    }
                    break :param_vi try param_it.param(isel, param_ty);
                } orelse continue;
                defer param_vi.deref(isel);
                const passed_vi = switch (param_vi.parent(isel)) {
                    .unallocated, .stack_slot => param_vi,
                    .value, .constant => unreachable,
                    .address => |address_vi| address_vi,
                };
                switch (passed_vi.parent(isel)) {
                    .unallocated => {},
                    .stack_slot => |stack_slot| {
                        assert(stack_slot.base == .sp);
                        isel.stack_size = @max(
                            isel.stack_size,
                            stack_slot.offset + @as(u26, @intCast(passed_vi.size(isel))),
                        );
                    },
                    .value, .constant, .address => unreachable,
                }
                try isel.analyzeUse(arg);
            }

            var ret_it: CallAbiIterator = .init;
            if (try ret_it.ret(isel, isel.air.typeOfIndex(air_inst_index, ip))) |ret_vi| {
                tracking_log.debug("${d} <- %{d}", .{ @intFromEnum(ret_vi), @intFromEnum(air_inst_index) });
                switch (ret_vi.parent(isel)) {
                    .unallocated, .stack_slot => {},
                    .value, .constant => unreachable,
                    .address => |address_vi| {
                        defer address_vi.deref(isel);
                        const ret_value = ret_vi.get(isel);
                        ret_value.flags.parent_tag = .unallocated;
                        ret_value.parent_payload = .{ .unallocated = {} };
                    },
                }
                try isel.live_values.putNoClobber(gpa, air_inst_index, ret_vi);
                try isel.def_order.putNoClobber(gpa, air_inst_index, {});
            }

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .sqrt,
        .sin,
        .cos,
        .tan,
        .exp,
        .exp2,
        .log,
        .log2,
        .log10,
        .floor,
        .ceil,
        .round,
        .trunc_float,
        .neg,
        .neg_optimized,
        .is_null,
        .is_non_null,
        .is_null_ptr,
        .is_non_null_ptr,
        .is_err,
        .is_non_err,
        .is_err_ptr,
        .is_non_err_ptr,
        .is_named_enum_value,
        .tag_name,
        .error_name,
        .cmp_lt_errors_len,
        => {
            const un_op = air_data[@intFromEnum(air_inst_index)].un_op;

            try isel.analyzeUse(un_op);
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .cmp_vector, .cmp_vector_optimized => {
            const ty_pl = air_data[@intFromEnum(air_inst_index)].ty_pl;
            const extra = isel.air.extraData(Air.VectorCmp, ty_pl.payload).data;

            try isel.analyzeUse(extra.lhs);
            try isel.analyzeUse(extra.rhs);
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .cond_br => {
            const pl_op = air_data[@intFromEnum(air_inst_index)].pl_op;
            const extra = isel.air.extraData(Air.CondBr, pl_op.payload);

            try isel.analyzeUse(pl_op.operand);

            try isel.analyze(@ptrCast(isel.air.extra.items[extra.end..][0..extra.data.then_body_len]));
            try isel.analyze(@ptrCast(isel.air.extra.items[extra.end + extra.data.then_body_len ..][0..extra.data.else_body_len]));

            air_body_index += 1;
        },
        .switch_br => {
            const switch_br = isel.air.unwrapSwitch(air_inst_index);

            try isel.analyzeUse(switch_br.operand);

            var cases_it = switch_br.iterateCases();
            while (cases_it.next()) |case| try isel.analyze(case.body);
            if (switch_br.else_body_len > 0) try isel.analyze(cases_it.elseBody());

            air_body_index += 1;
        },
        .loop_switch_br => {
            const switch_br = isel.air.unwrapSwitch(air_inst_index);

            const initial_dom_start = isel.dom_start;
            const initial_dom_len = isel.dom_len;
            isel.dom_start = @intCast(isel.dom.items.len);
            isel.dom_len = @intCast(isel.blocks.count());
            try isel.active_loops.append(gpa, @enumFromInt(isel.loops.count()));
            try isel.loops.putNoClobber(gpa, air_inst_index, .{
                .def_order = @intCast(isel.def_order.count()),
                .dom = isel.dom_start,
                .depth = isel.dom_len,
                .live = 0,
                .live_registers = undefined,
                .repeat_list = undefined,
            });
            try isel.dom.appendNTimes(gpa, 0, std.math.divCeil(usize, isel.dom_len, @bitSizeOf(DomInt)) catch unreachable);

            var cases_it = switch_br.iterateCases();
            while (cases_it.next()) |case| try isel.analyze(case.body);
            if (switch_br.else_body_len > 0) try isel.analyze(cases_it.elseBody());

            for (
                isel.dom.items[initial_dom_start..].ptr,
                isel.dom.items[isel.dom_start..][0 .. std.math.divCeil(usize, initial_dom_len, @bitSizeOf(DomInt)) catch unreachable],
            ) |*initial_dom, loop_dom| initial_dom.* |= loop_dom;
            isel.dom_start = initial_dom_start;
            isel.dom_len = initial_dom_len;
            assert(isel.active_loops.pop().?.inst(isel) == air_inst_index);

            air_body_index += 1;
        },
        .switch_dispatch => {
            const br = air_data[@intFromEnum(air_inst_index)].br;

            try isel.analyzeUse(br.operand);

            air_body_index += 1;
        },
        .@"try", .try_cold => {
            const pl_op = air_data[@intFromEnum(air_inst_index)].pl_op;
            const extra = isel.air.extraData(Air.Try, pl_op.payload);

            try isel.analyzeUse(pl_op.operand);
            try isel.analyze(@ptrCast(isel.air.extra.items[extra.end..][0..extra.data.body_len]));
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .try_ptr, .try_ptr_cold => {
            const ty_pl = air_data[@intFromEnum(air_inst_index)].ty_pl;
            const extra = isel.air.extraData(Air.TryPtr, ty_pl.payload);

            try isel.analyzeUse(extra.data.ptr);
            try isel.analyze(@ptrCast(isel.air.extra.items[extra.end..][0..extra.data.body_len]));
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .ret, .ret_safe, .ret_load => {
            const un_op = air_data[@intFromEnum(air_inst_index)].un_op;
            isel.returns = true;

            const block_index = 0;
            assert(isel.blocks.keys()[block_index] == Block.main);
            if (isel.dom_len > 0) isel.dom.items[isel.dom_start] |= 1 << block_index;

            try isel.analyzeUse(un_op);

            air_body_index += 1;
        },
        .store,
        .store_safe,
        .set_union_tag,
        .memset,
        .memset_safe,
        .memcpy,
        .memmove,
        .atomic_store_unordered,
        .atomic_store_monotonic,
        .atomic_store_release,
        .atomic_store_seq_cst,
        => {
            const bin_op = air_data[@intFromEnum(air_inst_index)].bin_op;

            try isel.analyzeUse(bin_op.lhs);
            try isel.analyzeUse(bin_op.rhs);

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .struct_field_ptr, .struct_field_val => {
            const ty_pl = air_data[@intFromEnum(air_inst_index)].ty_pl;
            const extra = isel.air.extraData(Air.StructField, ty_pl.payload).data;

            try isel.analyzeUse(extra.struct_operand);
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .slice_len => {
            const ty_op = air_data[@intFromEnum(air_inst_index)].ty_op;

            try isel.analyzeUse(ty_op.operand);
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            const slice_vi = try isel.use(ty_op.operand);
            var len_part_it = slice_vi.field(isel.air.typeOf(ty_op.operand, ip), 8, 8);
            if (try len_part_it.only(isel)) |len_part_vi|
                try isel.live_values.putNoClobber(gpa, air_inst_index, len_part_vi.ref(isel));

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .slice_ptr => {
            const ty_op = air_data[@intFromEnum(air_inst_index)].ty_op;

            try isel.analyzeUse(ty_op.operand);
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            const slice_vi = try isel.use(ty_op.operand);
            var ptr_part_it = slice_vi.field(isel.air.typeOf(ty_op.operand, ip), 0, 8);
            if (try ptr_part_it.only(isel)) |ptr_part_vi|
                try isel.live_values.putNoClobber(gpa, air_inst_index, ptr_part_vi.ref(isel));

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .reduce, .reduce_optimized => {
            const reduce = air_data[@intFromEnum(air_inst_index)].reduce;

            try isel.analyzeUse(reduce.operand);
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .shuffle_one => {
            const extra = isel.air.unwrapShuffleOne(zcu, air_inst_index);

            try isel.analyzeUse(extra.operand);
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .shuffle_two => {
            const extra = isel.air.unwrapShuffleTwo(zcu, air_inst_index);

            try isel.analyzeUse(extra.operand_a);
            try isel.analyzeUse(extra.operand_b);
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .select, .mul_add => {
            const pl_op = air_data[@intFromEnum(air_inst_index)].pl_op;
            const bin_op = isel.air.extraData(Air.Bin, pl_op.payload).data;

            try isel.analyzeUse(pl_op.operand);
            try isel.analyzeUse(bin_op.lhs);
            try isel.analyzeUse(bin_op.rhs);
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .cmpxchg_weak, .cmpxchg_strong => {
            const ty_pl = air_data[@intFromEnum(air_inst_index)].ty_pl;
            const extra = isel.air.extraData(Air.Cmpxchg, ty_pl.payload).data;

            try isel.analyzeUse(extra.ptr);
            try isel.analyzeUse(extra.expected_value);
            try isel.analyzeUse(extra.new_value);
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .atomic_load => {
            const atomic_load = air_data[@intFromEnum(air_inst_index)].atomic_load;

            try isel.analyzeUse(atomic_load.ptr);
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .atomic_rmw => {
            const pl_op = air_data[@intFromEnum(air_inst_index)].pl_op;
            const extra = isel.air.extraData(Air.AtomicRmw, pl_op.payload).data;

            try isel.analyzeUse(extra.operand);
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .aggregate_init => {
            const ty_pl = air_data[@intFromEnum(air_inst_index)].ty_pl;
            const elements: []const Air.Inst.Ref = @ptrCast(isel.air.extra.items[ty_pl.payload..][0..@intCast(ty_pl.ty.toType().arrayLen(zcu))]);

            for (elements) |element| try isel.analyzeUse(element);
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .union_init => {
            const ty_pl = air_data[@intFromEnum(air_inst_index)].ty_pl;
            const extra = isel.air.extraData(Air.UnionInit, ty_pl.payload).data;

            try isel.analyzeUse(extra.init);
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .prefetch => {
            const prefetch = air_data[@intFromEnum(air_inst_index)].prefetch;

            try isel.analyzeUse(prefetch.ptr);

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .field_parent_ptr => {
            const ty_pl = air_data[@intFromEnum(air_inst_index)].ty_pl;
            const extra = isel.air.extraData(Air.FieldParentPtr, ty_pl.payload).data;

            try isel.analyzeUse(extra.field_ptr);
            try isel.def_order.putNoClobber(gpa, air_inst_index, {});

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
        .set_err_return_trace => {
            const un_op = air_data[@intFromEnum(air_inst_index)].un_op;

            try isel.analyzeUse(un_op);

            air_body_index += 1;
            air_inst_index = air_body[air_body_index];
            continue :air_tag air_tags[@intFromEnum(air_inst_index)];
        },
    }
    assert(air_body_index == air_body.len);
    isel.def_order.shrinkRetainingCapacity(initial_def_order_len);
}

fn analyzeUse(isel: *Select, air_ref: Air.Inst.Ref) !void {
    const air_inst_index = air_ref.toIndex() orelse return;
    const def_order_index = isel.def_order.getIndex(air_inst_index).?;

    // Loop liveness
    var active_loop_index = isel.active_loops.items.len;
    while (active_loop_index > 0) {
        const prev_active_loop_index = active_loop_index - 1;
        const active_loop = isel.active_loops.items[prev_active_loop_index];
        if (def_order_index >= active_loop.get(isel).def_order) break;
        active_loop_index = prev_active_loop_index;
    }
    if (active_loop_index < isel.active_loops.items.len) {
        const active_loop = isel.active_loops.items[active_loop_index];
        const loop_live_gop =
            try isel.loop_live.set.getOrPut(isel.pt.zcu.gpa, .{ active_loop, air_inst_index });
        if (!loop_live_gop.found_existing) active_loop.get(isel).live += 1;
    }
}

pub fn finishAnalysis(isel: *Select) !void {
    const gpa = isel.pt.zcu.gpa;

    // Loop Liveness
    if (isel.loops.count() > 0) {
        try isel.loops.ensureUnusedCapacity(gpa, 1);

        const loop_live_len: u32 = @intCast(isel.loop_live.set.count());
        if (loop_live_len > 0) {
            try isel.loop_live.list.resize(gpa, loop_live_len);

            const loops = isel.loops.values();
            for (loops[1..], loops[0 .. loops.len - 1]) |*loop, prev_loop| loop.live += prev_loop.live;
            assert(loops[loops.len - 1].live == loop_live_len);

            for (isel.loop_live.set.keys()) |entry| {
                const loop, const inst = entry;
                const loop_live = &loop.get(isel).live;
                loop_live.* -= 1;
                isel.loop_live.list.items[loop_live.*] = inst;
            }
            assert(loops[0].live == 0);
        }

        const invalid_gop = isel.loops.getOrPutAssumeCapacity(Loop.invalid);
        assert(!invalid_gop.found_existing);
        invalid_gop.value_ptr.live = loop_live_len;
    }
}

pub fn verify(isel: *Select, check_values: bool) void {
    if (!std.debug.runtime_safety) return;
    assert(isel.blocks.count() == 1 and isel.blocks.keys()[0] == Select.Block.main);
    assert(isel.active_loops.items.len == 0);
    assert(isel.dom_start == 0 and isel.dom_len == 0);
    var live_reg_it = isel.live_registers.iterator();
    while (live_reg_it.next()) |live_reg_entry| switch (live_reg_entry.value.*) {
        _ => {
            isel.dumpValues(.all);
            unreachable;
        },
        .allocating, .free => {},
    };
    if (check_values) for (isel.values.items) |value| if (value.refs != 0) {
        isel.dumpValues(.only_referenced);
        unreachable;
    };
}

pub fn layout(
    isel: *Select,
    incoming: CallAbiIterator,
    mod: *const Package.Module,
) !usize {
    const zcu = isel.pt.zcu;
    const ip = &zcu.intern_pool;
    const nav = ip.getNav(isel.nav_index);
    wip_mir_log.debug("{f}<body>:\n", .{nav.fqn.fmt(ip)});

    const stack_size: u24 = @intCast(InternPool.Alignment.@"16".forward(isel.stack_size));

    _ = incoming;
    _ = mod;

    const Save = struct {
        needs_restore: bool,
        register: Register,
        offset: u10,
    };

    var saves_buf: [10]Save = undefined;

    const saves, const saves_size, const frame_record_offset = saves: {
        var saves_len: usize = 0;
        var saves_size: u10 = 0;
        var save_reg: Register = undefined;

        save_reg = .s0; // ignore fp
        while (save_reg != .s1) : (save_reg = @enumFromInt(@intFromEnum(save_reg) + 1)) {
            if (!isel.saved_registers.contains(save_reg)) continue;
            saves_buf[saves_len] = .{
                .needs_restore = true,
                .register = save_reg,
                .offset = saves_size,
            };
            saves_len += 1;
            saves_size += 8;
        }

        save_reg = .s2;
        while (save_reg != .s11) : (save_reg = @enumFromInt(@intFromEnum(save_reg) + 1)) {
            if (!isel.saved_registers.contains(save_reg)) continue;
            saves_buf[saves_len] = .{
                .needs_restore = true,
                .register = save_reg,
                .offset = saves_size,
            };
            saves_len += 1;
            saves_size += 8;
        }

        saves_size = std.mem.alignForward(u10, saves_size, 16);
        const frame_record_offset = saves_size;
        saves_buf[saves_len] = .{
            .needs_restore = true,
            .register = .fp,
            .offset = saves_size,
        };
        saves_len += 1;
        saves_size += 8;

        saves_size = std.mem.alignForward(u10, saves_size, 8);
        saves_buf[saves_len] = .{
            .needs_restore = true,
            .register = .ra,
            .offset = saves_size,
        };
        saves_len += 1;
        saves_size += 8;

        assert(InternPool.Alignment.@"16".check(saves_size));
        break :saves .{ saves_buf[0..saves_len], saves_size, frame_record_offset };
    };
    _ = frame_record_offset;

    const stack_size_lo = std.math.cast(u11, stack_size) orelse
        return isel.fail("TODO: layout stack shim for larger stack sizes", .{});

    {
        wip_mir_log.debug("{f}<prologue>:", .{nav.fqn.fmt(ip)});

        try isel.emit(.addi(.sp, .sp, -@as(i12, saves_size)));
        for (saves) |save| try isel.emit(.sd(save.register, .sp, save.offset));

        if (stack_size_lo > 0) try isel.emit(.addi(.sp, .sp, -@as(i12, stack_size_lo)));
        try isel.emit(.addi(.fp, .sp, stack_size_lo));
    }

    if (isel.stack_align != .@"16") return isel.fail("TODO: layout align stack pointer", .{});

    const epilogue = isel.instructions.items.len;
    if (isel.returns) {
        try isel.emit(.ret);

        try isel.emit(.addi(.sp, .sp, saves_size));
        for (saves) |save| if (save.needs_restore) try isel.emit(.ld(
            save.register,
            .sp,
            save.offset,
        ));
        if (stack_size_lo > 0) try isel.emit(.addi(.sp, .sp, stack_size_lo));

        wip_mir_log.debug("{f}<epilogue>:\n", .{nav.fqn.fmt(ip)});
    }
    return epilogue;
}

fn fmtDom(isel: *Select, inst: Air.Inst.Index, start: u32, len: u32) struct {
    isel: *Select,
    inst: Air.Inst.Index,
    start: u32,
    len: u32,
    pub fn format(data: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.print("%{d} -> {{", .{@intFromEnum(data.inst)});
        var first = true;
        for (data.isel.blocks.keys()[0..data.len], 0..) |block_inst_index, dom_index| {
            if (@as(u1, @truncate(data.isel.dom.items[
                data.start + dom_index / @bitSizeOf(DomInt)
            ] >> @truncate(dom_index))) == 0) continue;
            if (first) {
                first = false;
            } else {
                try writer.writeByte(',');
            }
            switch (block_inst_index) {
                Block.main => try writer.writeAll(" %main"),
                else => try writer.print(" %{d}", .{@intFromEnum(block_inst_index)}),
            }
        }
        if (!first) try writer.writeByte(' ');
        try writer.writeByte('}');
    }
} {
    return .{ .isel = isel, .inst = inst, .start = start, .len = len };
}

fn fmtLoopLive(isel: *Select, loop_inst: Air.Inst.Index) struct {
    isel: *Select,
    inst: Air.Inst.Index,
    pub fn format(data: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
        const loops = data.isel.loops.values();
        const loop_index = data.isel.loops.getIndex(data.inst).?;
        const live_insts =
            data.isel.loop_live.list.items[loops[loop_index].live..loops[loop_index + 1].live];

        try writer.print("%{d} <- {{", .{@intFromEnum(data.inst)});
        var first = true;
        for (live_insts) |live_inst| {
            if (first) {
                first = false;
            } else {
                try writer.writeByte(',');
            }
            try writer.print(" %{d}", .{@intFromEnum(live_inst)});
        }
        if (!first) try writer.writeByte(' ');
        try writer.writeByte('}');
    }
} {
    return .{ .isel = isel, .inst = loop_inst };
}

pub fn dumpValues(isel: *Select, which: enum { only_referenced, all }) void {
    errdefer |err| @panic(@errorName(err));
    const stderr, _ = std.debug.lockStderrWriter(&.{});
    defer std.debug.unlockStderrWriter();

    const zcu = isel.pt.zcu;
    const gpa = zcu.gpa;
    const ip = &zcu.intern_pool;
    const nav = ip.getNav(isel.nav_index);

    var reverse_live_values: std.AutoArrayHashMapUnmanaged(Value.Index, std.ArrayListUnmanaged(Air.Inst.Index)) = .empty;
    defer {
        for (reverse_live_values.values()) |*list| list.deinit(gpa);
        reverse_live_values.deinit(gpa);
    }
    {
        try reverse_live_values.ensureTotalCapacity(gpa, isel.live_values.count());
        var live_val_it = isel.live_values.iterator();
        while (live_val_it.next()) |live_val_entry| switch (live_val_entry.value_ptr.*) {
            _ => {
                const gop = reverse_live_values.getOrPutAssumeCapacity(live_val_entry.value_ptr.*);
                if (!gop.found_existing) gop.value_ptr.* = .empty;
                try gop.value_ptr.append(gpa, live_val_entry.key_ptr.*);
            },
            .allocating, .free => unreachable,
        };
    }

    var reverse_live_registers: std.AutoHashMapUnmanaged(Value.Index, Register) = .empty;
    defer reverse_live_registers.deinit(gpa);
    {
        try reverse_live_registers.ensureTotalCapacity(gpa, @typeInfo(Register).@"enum".fields.len);
        var live_reg_it = isel.live_registers.iterator();
        while (live_reg_it.next()) |live_reg_entry| switch (live_reg_entry.value.*) {
            _ => reverse_live_registers.putAssumeCapacityNoClobber(live_reg_entry.value.*, live_reg_entry.key),
            .allocating, .free => {},
        };
    }

    var roots: std.AutoArrayHashMapUnmanaged(Value.Index, u32) = .empty;
    defer roots.deinit(gpa);
    {
        try roots.ensureTotalCapacity(gpa, isel.values.items.len);
        var vi: Value.Index = @enumFromInt(isel.values.items.len);
        while (@intFromEnum(vi) > 0) {
            vi = @enumFromInt(@intFromEnum(vi) - 1);
            if (which == .only_referenced and vi.get(isel).refs == 0) continue;
            while (true) switch (vi.parent(isel)) {
                .unallocated, .stack_slot, .constant => break,
                .value => |parent_vi| vi = parent_vi,
                .address => |address_vi| break roots.putAssumeCapacity(address_vi, 0),
            };
            roots.putAssumeCapacity(vi, 0);
        }
    }

    try stderr.print("# Begin {s} Value Dump: {f}:\n", .{ @typeName(Select), nav.fqn.fmt(ip) });
    while (roots.pop()) |root_entry| {
        const vi = root_entry.key;
        const value = vi.get(isel);
        try stderr.splatByteAll(' ', 2 * (@as(usize, 1) + root_entry.value));
        try stderr.print("${d}", .{@intFromEnum(vi)});
        {
            var first = true;
            if (reverse_live_values.get(vi)) |aiis| for (aiis.items) |aii| {
                if (aii == Block.main) {
                    try stderr.print("{s}%main", .{if (first) " <- " else ", "});
                } else {
                    try stderr.print("{s}%{d}", .{ if (first) " <- " else ", ", @intFromEnum(aii) });
                }
                first = false;
            };
            if (reverse_live_registers.get(vi)) |ra| {
                try stderr.print("{s}{t}", .{ if (first) " <- " else ", ", ra });
                first = false;
            }
        }
        try stderr.writeByte(':');
        switch (value.flags.parent_tag) {
            .unallocated => if (value.offset_from_parent != 0) try stderr.print(" +0x{x}", .{value.offset_from_parent}),
            .stack_slot => {
                try stderr.print(" [{t}, #{s}0x{x}", .{
                    value.parent_payload.stack_slot.base,
                    if (value.parent_payload.stack_slot.offset < 0) "-" else "",
                    @abs(value.parent_payload.stack_slot.offset),
                });
                if (value.offset_from_parent != 0) try stderr.print("+0x{x}", .{value.offset_from_parent});
                try stderr.writeByte(']');
            },
            .value => try stderr.print(" ${d}+0x{x}", .{ @intFromEnum(value.parent_payload.value), value.offset_from_parent }),
            .address => try stderr.print(" ${d}[0x{x}]", .{ @intFromEnum(value.parent_payload.address), value.offset_from_parent }),
            .constant => try stderr.print(" <{f}, {f}>", .{
                isel.fmtType(value.parent_payload.constant.typeOf(zcu)),
                isel.fmtConstant(value.parent_payload.constant),
            }),
        }
        try stderr.print(" align({t})", .{value.flags.alignment});
        switch (value.flags.location_tag) {
            .large => try stderr.print(" size=0x{x} large", .{value.location_payload.large.size}),
            .small => {
                const loc = value.location_payload.small;
                try stderr.print(" size=0x{x}", .{loc.size});
                switch (loc.signedness) {
                    .unsigned => {},
                    .signed => try stderr.writeAll(" signed"),
                }
                if (loc.hint != .zero) try stderr.print(" hint={t}", .{loc.hint});
                if (loc.register != .zero) try stderr.print(" loc={t}", .{loc.register});
            },
        }
        try stderr.print(" refs={d}\n", .{value.refs});

        var part_index = value.flags.parts_len_minus_one;
        if (part_index > 0) while (true) : (part_index -= 1) {
            roots.putAssumeCapacityNoClobber(
                @enumFromInt(@intFromEnum(value.parts) + part_index),
                root_entry.value + 1,
            );
            if (part_index == 0) break;
        };
    }
    try stderr.print("# End {s} Value Dump: {f}\n\n", .{ @typeName(Select), nav.fqn.fmt(ip) });
}

fn writeToMemory(isel: *Select, constant: Constant, buffer: []u8) error{OutOfMemory}!bool {
    const zcu = isel.pt.zcu;
    const ip = &zcu.intern_pool;
    if (try isel.writeKeyToMemory(ip.indexToKey(constant.toIntern()), buffer)) return true;
    constant.writeToMemory(isel.pt, buffer) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ReinterpretDeclRef, error.Unimplemented, error.IllDefinedMemoryLayout => return false,
    };
    return true;
}

fn writeKeyToMemory(isel: *Select, constant_key: InternPool.Key, buffer: []u8) error{OutOfMemory}!bool {
    const zcu = isel.pt.zcu;
    const ip = &zcu.intern_pool;
    switch (constant_key) {
        .int_type,
        .ptr_type,
        .array_type,
        .vector_type,
        .opt_type,
        .anyframe_type,
        .error_union_type,
        .simple_type,
        .struct_type,
        .tuple_type,
        .union_type,
        .opaque_type,
        .enum_type,
        .func_type,
        .error_set_type,
        .inferred_error_set_type,

        .enum_literal,
        .empty_enum_value,
        .memoized_call,
        => unreachable, // not a runtime value
        .err => |err| {
            const error_int = ip.getErrorValueIfExists(err.name).?;
            switch (buffer.len) {
                else => unreachable,
                inline 1...4 => |size| std.mem.writeInt(
                    @Type(.{ .int = .{ .signedness = .unsigned, .bits = 8 * size } }),
                    buffer[0..size],
                    @intCast(error_int),
                    isel.target.cpu.arch.endian(),
                ),
            }
        },
        .error_union => |error_union| {
            const error_union_type = ip.indexToKey(error_union.ty).error_union_type;
            const error_set_ty: ZigType = .fromInterned(error_union_type.error_set_type);
            const payload_ty: ZigType = .fromInterned(error_union_type.payload_type);
            const error_set = buffer[@intCast(codegen.errUnionErrorOffset(payload_ty, zcu))..][0..@intCast(error_set_ty.abiSize(zcu))];
            switch (error_union.val) {
                .err_name => |err_name| if (!try isel.writeKeyToMemory(.{ .err = .{
                    .ty = error_set_ty.toIntern(),
                    .name = err_name,
                } }, error_set)) return false,
                .payload => |payload| {
                    if (!try isel.writeToMemory(
                        .fromInterned(payload),
                        buffer[@intCast(codegen.errUnionPayloadOffset(payload_ty, zcu))..][0..@intCast(payload_ty.abiSize(zcu))],
                    )) return false;
                    @memset(error_set, 0);
                },
            }
        },
        .opt => |opt| {
            const child_size: usize = @intCast(ZigType.fromInterned(ip.indexToKey(opt.ty).opt_type).abiSize(zcu));
            switch (opt.val) {
                .none => if (!ZigType.fromInterned(opt.ty).optionalReprIsPayload(zcu)) {
                    buffer[child_size] = @intFromBool(false);
                } else @memset(buffer[0..child_size], 0x00),
                else => |child_constant| {
                    if (!try isel.writeToMemory(.fromInterned(child_constant), buffer[0..child_size])) return false;
                    if (!ZigType.fromInterned(opt.ty).optionalReprIsPayload(zcu)) buffer[child_size] = @intFromBool(true);
                },
            }
        },
        .aggregate => |aggregate| switch (ip.indexToKey(aggregate.ty)) {
            else => unreachable,
            .array_type => |array_type| {
                var elem_offset: usize = 0;
                const elem_size: usize = @intCast(ZigType.fromInterned(array_type.child).abiSize(zcu));
                const len_including_sentinel: usize = @intCast(array_type.lenIncludingSentinel());
                switch (aggregate.storage) {
                    .bytes => |bytes| @memcpy(buffer[0..len_including_sentinel], bytes.toSlice(len_including_sentinel, ip)),
                    .elems => |elems| for (elems) |elem| {
                        if (!try isel.writeToMemory(.fromInterned(elem), buffer[elem_offset..][0..elem_size])) return false;
                        elem_offset += elem_size;
                    },
                    .repeated_elem => |repeated_elem| for (0..len_including_sentinel) |_| {
                        if (!try isel.writeToMemory(.fromInterned(repeated_elem), buffer[elem_offset..][0..elem_size])) return false;
                        elem_offset += elem_size;
                    },
                }
            },
            .vector_type => return false,
            .struct_type => {
                const loaded_struct = ip.loadStructType(aggregate.ty);
                switch (loaded_struct.layout) {
                    .auto => {
                        var field_offset: u64 = 0;
                        var field_it = loaded_struct.iterateRuntimeOrder(ip);
                        while (field_it.next()) |field_index| {
                            if (loaded_struct.fieldIsComptime(ip, field_index)) continue;
                            const field_ty: ZigType = .fromInterned(loaded_struct.field_types.get(ip)[field_index]);
                            field_offset = field_ty.structFieldAlignment(
                                loaded_struct.fieldAlign(ip, field_index),
                                loaded_struct.layout,
                                zcu,
                            ).forward(field_offset);
                            const field_size = field_ty.abiSize(zcu);
                            if (!try isel.writeToMemory(.fromInterned(switch (aggregate.storage) {
                                .bytes => unreachable,
                                .elems => |elems| elems[field_index],
                                .repeated_elem => |repeated_elem| repeated_elem,
                            }), buffer[@intCast(field_offset)..][0..@intCast(field_size)])) return false;
                            field_offset += field_size;
                        }
                    },
                    .@"extern", .@"packed" => return false,
                }
            },
            .tuple_type => |tuple_type| {
                var field_offset: u64 = 0;
                for (tuple_type.types.get(ip), tuple_type.values.get(ip), 0..) |field_type, field_value, field_index| {
                    if (field_value != .none) continue;
                    const field_ty: ZigType = .fromInterned(field_type);
                    field_offset = field_ty.abiAlignment(zcu).forward(field_offset);
                    const field_size = field_ty.abiSize(zcu);
                    if (!try isel.writeToMemory(.fromInterned(switch (aggregate.storage) {
                        .bytes => unreachable,
                        .elems => |elems| elems[field_index],
                        .repeated_elem => |repeated_elem| repeated_elem,
                    }), buffer[@intCast(field_offset)..][0..@intCast(field_size)])) return false;
                    field_offset += field_size;
                }
            },
        },
        else => return false,
    }
    return true;
}

pub fn body(isel: *Select, air_body: []const Air.Inst.Index) error{ OutOfMemory, CodegenFail }!void {
    const zcu = isel.pt.zcu;
    const ip = &zcu.intern_pool;
    const gpa = zcu.gpa;

    {
        var live_reg_it = isel.live_registers.iterator();
        while (live_reg_it.next()) |live_reg_entry| switch (live_reg_entry.value.*) {
            _ => {
                const ra = &live_reg_entry.value.get(isel).location_payload.small.register;
                assert(ra.* == live_reg_entry.key);
                ra.* = .zero;
                live_reg_entry.value.* = .free;
            },
            .allocating => live_reg_entry.value.* = .free,
            .free => {},
        };
    }

    var air: struct {
        isel: *Select,
        tag_items: []const Air.Inst.Tag,
        data_items: []const Air.Inst.Data,
        body: []const Air.Inst.Index,
        body_index: u32,
        inst_index: Air.Inst.Index,

        fn tag(it: *@This(), inst_index: Air.Inst.Index) Air.Inst.Tag {
            return it.tag_items[@intFromEnum(inst_index)];
        }

        fn data(it: *@This(), inst_index: Air.Inst.Index) Air.Inst.Data {
            return it.data_items[@intFromEnum(inst_index)];
        }

        fn next(it: *@This()) ?Air.Inst.Tag {
            if (it.body_index == 0) {
                @branchHint(.unlikely);
                return null;
            }
            it.body_index -= 1;
            it.inst_index = it.body[it.body_index];
            wip_mir_log.debug("{f}", .{it.fmtAir(it.inst_index)});
            return it.tag(it.inst_index);
        }

        fn fmtAir(it: @This(), inst: Air.Inst.Index) struct {
            isel: *Select,
            inst: Air.Inst.Index,
            pub fn format(fmt_air: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
                fmt_air.isel.air.writeInst(writer, fmt_air.inst, fmt_air.isel.pt, null);
            }
        } {
            return .{ .isel = it.isel, .inst = inst };
        }
    } = .{
        .isel = isel,
        .tag_items = isel.air.instructions.items(.tag),
        .data_items = isel.air.instructions.items(.data),
        .body = air_body,
        .body_index = @intCast(air_body.len),
        .inst_index = undefined,
    };
    air_tag: switch (air.next().?) {
        else => |air_tag| return isel.fail("unimplemented {t}", .{air_tag}),

        // No "scalarize" legalizations are enabled, so these instructions never appear.
        .legalize_vec_elem_val => unreachable,
        .legalize_vec_store_elem => unreachable,

        .legalize_compiler_rt_call => {
            const inst_data = air.data(air.inst_index).legalize_compiler_rt_call;
            const extra = isel.air.extraData(Air.Call, inst_data.payload);
            const args: []const Air.Inst.Ref = @ptrCast(isel.air.extra.items[extra.end..][0..extra.data.args_len]);

            try call.prepareReturn(isel);
            const maybe_def_ret_vi = isel.live_values.fetchRemove(air.inst_index);
            var maybe_ret_addr_vi: ?Value.Index = null;
            if (maybe_def_ret_vi) |def_ret_vi| {
                defer def_ret_vi.value.deref(isel);

                var ret_it: CallAbiIterator = .init;
                const ret_vi = try ret_it.ret(isel, isel.air.typeOfIndex(air.inst_index, ip));
                defer ret_vi.?.deref(isel);
                switch (ret_vi.?.parent(isel)) {
                    .unallocated, .stack_slot => if (ret_vi.?.hint(isel)) |ret_ra| {
                        try call.returnLiveIn(isel, def_ret_vi.value, ret_ra);
                    } else {
                        var def_ret_part_it = def_ret_vi.value.parts(isel);
                        var ret_part_it = ret_vi.?.parts(isel);
                        while (def_ret_part_it.next()) |ret_part_vi| {
                            try call.returnLiveIn(isel, ret_part_vi, ret_part_it.next().?.hint(isel).?);
                        }
                    },
                    .value, .constant => unreachable,
                    .address => |address_vi| {
                        maybe_ret_addr_vi = address_vi;
                        _ = try def_ret_vi.value.defAddr(isel, isel.air.typeOfIndex(air.inst_index, ip), .{
                            .expected_live_registers = &call.caller_saved_regs,
                        });
                    },
                }
            }
            try call.finishReturn(isel);

            try call.prepareCallee(isel);
            try isel.emit(.jalr(.ra, .ra, 0));
            try isel.global_relocs.append(gpa, .{
                .name = inst_data.func.name(isel.target).ptr,
                .reloc = .{ .label = @intCast(isel.instructions.items.len) },
            });
            try isel.emit(.auipc(.ra, 0));
            try call.finishCallee(isel);

            try call.prepareParams(isel);
            if (maybe_ret_addr_vi) |ret_addr_vi| try call.paramAddress(
                isel,
                maybe_def_ret_vi.?.value,
                ret_addr_vi.hint(isel).?,
            );
            var param_it: CallAbiIterator = .init;
            for (args) |arg| {
                const param_ty = isel.air.typeOf(arg, ip);
                const param_vi = (try param_it.param(isel, param_ty)) orelse continue;
                defer param_vi.deref(isel);
                const arg_vi = try isel.use(arg);
                switch (param_vi.parent(isel)) {
                    .unallocated => if (param_vi.hint(isel)) |param_ra| {
                        try call.paramLiveOut(isel, arg_vi, param_ra);
                    } else {
                        var param_part_it = param_vi.parts(isel);
                        var arg_part_it = arg_vi.parts(isel);
                        if (arg_part_it.only()) |_| {
                            try isel.values.ensureUnusedCapacity(gpa, param_part_it.remaining);
                            arg_vi.setParts(isel, param_part_it.remaining);
                            while (param_part_it.next()) |param_part_vi| _ = arg_vi.addPart(
                                isel,
                                param_part_vi.get(isel).offset_from_parent,
                                param_part_vi.size(isel),
                            );
                            param_part_it = param_vi.parts(isel);
                            arg_part_it = arg_vi.parts(isel);
                        }
                        while (param_part_it.next()) |param_part_vi| {
                            const arg_part_vi = arg_part_it.next().?;
                            assert(arg_part_vi.get(isel).offset_from_parent ==
                                param_part_vi.get(isel).offset_from_parent);
                            assert(arg_part_vi.size(isel) == param_part_vi.size(isel));
                            try call.paramLiveOut(isel, arg_part_vi, param_part_vi.hint(isel).?);
                        }
                    },
                    .stack_slot => |stack_slot| try arg_vi.store(
                        isel,
                        param_ty,
                        stack_slot.base,
                        .{ .offset = @intCast(stack_slot.offset) },
                    ),
                    .value, .constant => unreachable,
                    .address => |address_vi| try call.paramAddress(isel, arg_vi, address_vi.hint(isel).?),
                }
            }
            try call.finishParams(isel);

            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },

        .arg => {
            const arg_vi = isel.live_values.fetchRemove(air.inst_index).?.value;
            defer arg_vi.deref(isel);
            switch (arg_vi.parent(isel)) {
                .unallocated, .stack_slot => if (arg_vi.hint(isel)) |arg_reg| {
                    try arg_vi.defLiveIn(isel, arg_reg, comptime &.initFill(.free));
                } else {
                    var arg_part_it = arg_vi.parts(isel);
                    while (arg_part_it.next()) |arg_part| {
                        try arg_part.defLiveIn(isel, arg_part.hint(isel).?, comptime &.initFill(.free));
                    }
                },
                .value, .constant => unreachable,
                .address => |address_vi| try address_vi.defLiveIn(isel, address_vi.hint(isel).?, comptime &.initFill(.free)),
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .alloc => {
            if (isel.live_values.fetchRemove(air.inst_index)) |ptr_vi| unused: {
                defer ptr_vi.value.deref(isel);
                const ptr_reg = try ptr_vi.value.defReg(isel) orelse break :unused;

                const ty = air.data(air.inst_index).ty;
                const slot_size = ty.childType(zcu).abiSize(zcu);
                const slot_align = ty.ptrAlignment(zcu);
                const slot_offset = slot_align.forward(isel.stack_size);
                isel.stack_size = @intCast(slot_offset + slot_size);

                const lo11 = std.math.cast(u11, slot_offset) orelse
                    return isel.fail("TODO: stack shims", .{});
                try isel.emit(.addi(ptr_reg, .sp, lo11));
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .store, .store_safe => {
            const bin_op = air.data(air.inst_index).bin_op;
            const ptr_ty = isel.air.typeOf(bin_op.lhs, ip);
            const ptr_info = ptr_ty.ptrInfo(zcu);
            if (ptr_info.packed_offset.host_size > 0) return isel.fail("packed store", .{});
            if (bin_op.rhs.toInterned()) |rhs_val| if (ip.isUndef(rhs_val))
                break :air_tag if (air.next()) |next_air_tag| continue :air_tag next_air_tag;

            const src_vi = try isel.use(bin_op.rhs);
            const size = src_vi.size(isel);

            if (ZigType.fromInterned(ptr_info.child).zigTypeTag(zcu) != .@"union") switch (size) {
                0 => unreachable,
                1...Value.max_parts => {
                    const ptr_vi = try isel.use(bin_op.lhs);
                    const ptr_mat = try ptr_vi.matReg(isel);
                    try src_vi.store(
                        isel,
                        isel.air.typeOf(bin_op.rhs, ip),
                        ptr_mat.r,
                        .{ .@"volatile" = ptr_info.flags.is_volatile },
                    );
                    try ptr_mat.finish(isel);

                    break :air_tag if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
                },
                else => {},
            };

            try call.prepareReturn(isel);
            try call.finishReturn(isel);

            try call.prepareCallee(isel);
            try isel.emit(.jalr(.ra, .ra, 0));
            try isel.global_relocs.append(gpa, .{
                .name = "memcpy",
                .reloc = .{ .label = @intCast(isel.instructions.items.len) },
            });
            try isel.emit(.auipc(.ra, 0));
            try call.finishCallee(isel);

            try call.prepareParams(isel);
            const ptr_vi = try isel.use(bin_op.lhs);
            try isel.movImmediate(.a2, size);
            try call.paramAddress(isel, src_vi, .a1);
            try call.paramLiveOut(isel, ptr_vi, .a0);
            try call.finishParams(isel);

            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .load => {
            const ty_op = air.data(air.inst_index).ty_op;
            const ptr_ty = isel.air.typeOf(ty_op.operand, ip);
            const ptr_info = ptr_ty.ptrInfo(zcu);

            if (ptr_info.packed_offset.host_size > 0) return isel.fail("packed load", .{});
            if (ptr_info.flags.is_volatile) _ = try isel.use(air.inst_index.toRef());

            if (isel.live_values.fetchRemove(air.inst_index)) |dst_vi| unused: {
                defer dst_vi.value.deref(isel);
                const size = dst_vi.value.size(isel);
                if (size <= Value.max_parts and ip.zigTypeTag(ptr_info.child) != .@"union") {
                    const ptr_vi = try isel.use(ty_op.operand);
                    const ptr_mat = try ptr_vi.matReg(isel);
                    _ = try dst_vi.value.load(
                        isel,
                        ty_op.ty.toType(),
                        ptr_mat.r,
                        .{ .@"volatile" = ptr_info.flags.is_volatile },
                    );
                    try ptr_mat.finish(isel);
                } else {
                    try dst_vi.value.defAddr(isel, .fromInterned(ptr_info.child), .{}) orelse break :unused;

                    try call.prepareReturn(isel);
                    try call.finishReturn(isel);

                    try call.prepareCallee(isel);
                    try isel.emit(.jalr(.ra, .ra, 0));
                    try isel.emit(.auipc(.ra, 0));
                    try isel.global_relocs.append(gpa, .{
                        .name = "memcpy",
                        .reloc = .{ .label = @intCast(isel.instructions.items.len - 1) },
                    });
                    try call.finishCallee(isel);

                    try call.prepareParams(isel);
                    const ptr_vi = try isel.use(ty_op.operand);
                    try isel.movImmediate(.a2, size);
                    try call.paramLiveOut(isel, ptr_vi, .a1);
                    try call.paramAddress(isel, dst_vi.value, .a0);
                    try call.finishParams(isel);
                }
            }

            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },

        // math
        .add_safe => unreachable,
        .sub_safe => unreachable,
        .add, .add_optimized, .add_wrap, .sub, .sub_optimized, .sub_wrap => |air_tag| {
            if (isel.live_values.fetchRemove(air.inst_index)) |res_vi| {
                defer res_vi.value.deref(isel);

                const bin_op = air.data(air.inst_index).bin_op;
                const ty = isel.air.typeOf(bin_op.lhs, ip);
                if (ty.isRuntimeFloat()) return isel.fail("TODO: add/sub floats {f}", .{isel.fmtType(ty)});

                try res_vi.value.addOrSubtract(
                    isel,
                    ty,
                    try isel.use(bin_op.lhs),
                    switch (air_tag) {
                        else => unreachable,
                        .add, .add_wrap => .add,
                        .sub, .sub_wrap => .sub,
                    },
                    try isel.use(bin_op.rhs),
                    .{
                        .overflow = switch (air_tag) {
                            else => unreachable,
                            .add, .sub => .@"unreachable",
                            .add_wrap, .sub_wrap => .wrap,
                        },
                    },
                );
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .add_with_overflow, .sub_with_overflow => |air_tag| {
            if (isel.live_values.fetchRemove(air.inst_index)) |res_vi| {
                defer res_vi.value.deref(isel);

                const ty_pl = air.data(air.inst_index).ty_pl;
                const bin_op = isel.air.extraData(Air.Bin, ty_pl.payload).data;
                const ty = isel.air.typeOf(bin_op.lhs, ip);
                const lhs_vi = try isel.use(bin_op.lhs);
                const rhs_vi = try isel.use(bin_op.rhs);
                const ty_size = lhs_vi.size(isel);
                var overflow_it = res_vi.value.field(ty_pl.ty.toType(), ty_size, 1);
                const overflow_vi = try overflow_it.only(isel);
                var wrapped_it = res_vi.value.field(ty_pl.ty.toType(), 0, ty_size);
                const wrapped_vi = try wrapped_it.only(isel);
                try wrapped_vi.?.addOrSubtract(isel, ty, lhs_vi, switch (air_tag) {
                    else => unreachable,
                    .add_with_overflow => .add,
                    .sub_with_overflow => .sub,
                }, rhs_vi, .{
                    .overflow = if (try overflow_vi.?.defReg(isel)) |overflow_reg| .{ .reg = overflow_reg } else .wrap,
                });
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .shr => |air_tag| {
            if (isel.live_values.fetchRemove(air.inst_index)) |res_vi| unused: {
                defer res_vi.value.deref(isel);

                const bin_op = air.data(air.inst_index).bin_op;
                const ty = isel.air.typeOf(bin_op.lhs, ip);
                if (!ty.isAbiInt(zcu)) return isel.fail("bad {t} {f}", .{ air_tag, isel.fmtType(ty) });
                const int_info = ty.intInfo(zcu);
                switch (int_info.bits) {
                    0 => unreachable,
                    64 => {
                        const result_reg = try res_vi.value.defReg(isel) orelse break :unused;

                        const lhs_vi = try isel.use(bin_op.lhs);
                        const rhs_vi = try isel.use(bin_op.rhs);
                        const lhs_mat = try lhs_vi.matReg(isel);
                        const rhs_mat = try rhs_vi.matReg(isel);

                        try isel.emit(switch (int_info.signedness) {
                            .unsigned => .srl(result_reg, lhs_mat.r, rhs_mat.r),
                            .signed => .sra(result_reg, lhs_mat.r, rhs_mat.r),
                        });

                        try rhs_mat.finish(isel);
                        try lhs_mat.finish(isel);
                    },
                    else => return isel.fail("too big {t} {f}", .{ air_tag, isel.fmtType(ty) }),
                }
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .max, .min => |air_tag| {
            if (isel.live_values.fetchRemove(air.inst_index)) |res_vi| unused: {
                defer res_vi.value.deref(isel);

                const bin_op = air.data(air.inst_index).bin_op;
                const ty = isel.air.typeOf(bin_op.lhs, ip);
                if (ty.isRuntimeFloat()) return isel.fail("TODO: float max/min", .{});
                const int_info = ty.intInfo(zcu);
                if (int_info.bits > 64) return isel.fail("too big: {t} {f}", .{ air_tag, isel.fmtType(ty) });

                const result_vi = res_vi.value;
                const lhs_vi = try isel.use(bin_op.lhs);
                const rhs_vi = try isel.use(bin_op.rhs);

                const dst_reg = try result_vi.defReg(isel) orelse break :unused;
                const dst_lock: RegLock = switch (dst_reg) {
                    .zero => .empty,
                    else => isel.lockReg(dst_reg),
                };
                defer dst_lock.unlock(isel);

                const mask_reg = try isel.allocIntReg();
                defer isel.freeReg(mask_reg);

                const lhs_mat = try lhs_vi.matReg(isel);
                const rhs_mat = try rhs_vi.matReg(isel);

                try isel.emit(.xor(dst_reg, switch (air_tag) {
                    .min => rhs_mat.r,
                    .max => lhs_mat.r,
                    else => unreachable,
                }, mask_reg));
                try isel.emit(.@"and"(mask_reg, dst_reg, mask_reg));
                try isel.emit(.xor(dst_reg, lhs_mat.r, rhs_mat.r));
                try isel.emit(.sub(mask_reg, .zero, mask_reg));
                try isel.emit(switch (int_info.signedness) {
                    .signed => .slt(mask_reg, lhs_mat.r, rhs_mat.r),
                    .unsigned => .sltu(mask_reg, lhs_mat.r, rhs_mat.r),
                });

                try lhs_mat.finish(isel);
                try rhs_mat.finish(isel);
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .bit_and, .bit_or, .xor, .bool_and, .bool_or => |air_tag| {
            if (isel.live_values.fetchRemove(air.inst_index)) |res_vi| {
                defer res_vi.value.deref(isel);

                const bin_op = air.data(air.inst_index).bin_op;
                const ty = isel.air.typeOf(bin_op.lhs, ip);
                const int_info: std.builtin.Type.Int = if (ty.toIntern() == .bool_type)
                    .{ .signedness = .unsigned, .bits = 1 }
                else if (ty.isAbiInt(zcu))
                    ty.intInfo(zcu)
                else
                    return isel.fail("bad {t} {f}", .{ air_tag, isel.fmtType(ty) });
                if (int_info.bits > 64) return isel.fail("too big {t} {f}", .{ air_tag, isel.fmtType(ty) });

                const lhs_vi = try isel.use(bin_op.lhs);
                const rhs_vi = try isel.use(bin_op.rhs);
                var offset = res_vi.value.size(isel);
                while (offset > 0) {
                    const size = @min(offset, 8);
                    offset -= size;
                    var res_part_it = res_vi.value.field(ty, offset, size);
                    const res_part_vi = try res_part_it.only(isel);
                    const res_part_reg = try res_part_vi.?.defReg(isel) orelse continue;
                    var lhs_part_it = lhs_vi.field(ty, offset, size);
                    const lhs_part_vi = try lhs_part_it.only(isel);
                    const lhs_part_mat = try lhs_part_vi.?.matReg(isel);
                    var rhs_part_it = rhs_vi.field(ty, offset, size);
                    const rhs_part_vi = try rhs_part_it.only(isel);
                    const rhs_part_mat = try rhs_part_vi.?.matReg(isel);
                    try isel.emit(switch (air_tag) {
                        else => unreachable,
                        .bit_and, .bool_and => switch (size) {
                            else => unreachable,
                            1, 2, 4, 8 => .@"and"(res_part_reg, lhs_part_mat.r, rhs_part_mat.r),
                        },
                        .bit_or, .bool_or => switch (size) {
                            else => unreachable,
                            1, 2, 4, 8 => .@"or"(res_part_reg, lhs_part_mat.r, rhs_part_mat.r),
                        },
                        .xor => switch (size) {
                            else => unreachable,
                            1, 2, 4, 8 => .xor(res_part_reg, lhs_part_mat.r, rhs_part_mat.r),
                        },
                    });
                    try rhs_part_mat.finish(isel);
                    try lhs_part_mat.finish(isel);
                }
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .ptr_add, .ptr_sub => |air_tag| {
            if (isel.live_values.fetchRemove(air.inst_index)) |res_vi| unused: {
                defer res_vi.value.deref(isel);
                const res_ra = try res_vi.value.defReg(isel) orelse break :unused;

                const ty_pl = air.data(air.inst_index).ty_pl;
                const bin_op = isel.air.extraData(Air.Bin, ty_pl.payload).data;
                const elem_size = ty_pl.ty.toType().elemType2(zcu).abiSize(zcu);

                const base_vi = try isel.use(bin_op.lhs);
                var base_part_it = base_vi.field(ty_pl.ty.toType(), 0, 8);
                const base_part_vi = try base_part_it.only(isel);
                const base_part_mat = try base_part_vi.?.matReg(isel);
                const index_vi = try isel.use(bin_op.rhs);
                try isel.elemPtr(res_ra, base_part_mat.r, switch (air_tag) {
                    else => unreachable,
                    .ptr_add => .add,
                    .ptr_sub => .sub,
                }, elem_size, index_vi);
                try base_part_mat.finish(isel);
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .cmp_lt, .cmp_lte, .cmp_eq, .cmp_gte, .cmp_gt, .cmp_neq => |air_tag| {
            if (isel.live_values.fetchRemove(air.inst_index)) |res_vi| unused: {
                defer res_vi.value.deref(isel);

                const bin_op = air.data(air.inst_index).bin_op;
                const ty = isel.air.typeOf(bin_op.lhs, ip);
                try isel.cmp(
                    try res_vi.value.defReg(isel) orelse break :unused,
                    ty,
                    try isel.use(bin_op.lhs),
                    air_tag.toCmpOp().?,
                    try isel.use(bin_op.rhs),
                );
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .slice => {
            if (isel.live_values.fetchRemove(air.inst_index)) |slice_vi| {
                defer slice_vi.value.deref(isel);
                const ty_pl = air.data(air.inst_index).ty_pl;
                const bin_op = isel.air.extraData(Air.Bin, ty_pl.payload).data;
                var ptr_part_it = slice_vi.value.field(ty_pl.ty.toType(), 0, 8);
                const ptr_part_vi = try ptr_part_it.only(isel);
                try ptr_part_vi.?.move(isel, bin_op.lhs);
                var len_part_it = slice_vi.value.field(ty_pl.ty.toType(), 8, 8);
                const len_part_vi = try len_part_it.only(isel);
                try len_part_vi.?.move(isel, bin_op.rhs);
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .slice_len => {
            if (isel.live_values.fetchRemove(air.inst_index)) |len_vi| {
                defer len_vi.value.deref(isel);
                const ty_op = air.data(air.inst_index).ty_op;
                const slice_vi = try isel.use(ty_op.operand);
                var len_part_it = slice_vi.field(isel.air.typeOf(ty_op.operand, ip), 8, 8);
                const len_part_vi = try len_part_it.only(isel);
                try len_vi.value.copy(isel, ty_op.ty.toType(), len_part_vi.?);
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .slice_ptr => {
            if (isel.live_values.fetchRemove(air.inst_index)) |ptr_vi| {
                defer ptr_vi.value.deref(isel);
                const ty_op = air.data(air.inst_index).ty_op;
                const slice_vi = try isel.use(ty_op.operand);
                var ptr_part_it = slice_vi.field(isel.air.typeOf(ty_op.operand, ip), 0, 8);
                const ptr_part_vi = try ptr_part_it.only(isel);
                try ptr_vi.value.copy(isel, ty_op.ty.toType(), ptr_part_vi.?);
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .ptr_slice_len_ptr => {
            if (isel.live_values.fetchRemove(air.inst_index)) |dst_vi| unused: {
                defer dst_vi.value.deref(isel);
                const ty_op = air.data(air.inst_index).ty_op;
                const dst_reg = try dst_vi.value.defReg(isel) orelse break :unused;
                const src_vi = try isel.use(ty_op.operand);
                const src_mat = try src_vi.matReg(isel);
                try isel.emit(.addi(dst_reg, src_mat.r, 8));
                try src_mat.finish(isel);
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .ptr_slice_ptr_ptr => {
            if (isel.live_values.fetchRemove(air.inst_index)) |dst_vi| {
                defer dst_vi.value.deref(isel);
                const ty_op = air.data(air.inst_index).ty_op;
                try dst_vi.value.move(isel, ty_op.operand);
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .ptr_elem_val => {
            if (isel.live_values.fetchRemove(air.inst_index)) |elem_vi| unused: {
                defer elem_vi.value.deref(isel);

                const bin_op = air.data(air.inst_index).bin_op;
                const ptr_ty = isel.air.typeOf(bin_op.lhs, ip);
                const ptr_info = ptr_ty.ptrInfo(zcu);
                const elem_size = elem_vi.value.size(isel);
                const elem_is_vector = elem_vi.value.isVector(isel);
                if (elem_is_vector) return isel.fail("TODO: ptr_elem_val vector {f}", .{isel.fmtType(ptr_ty)});

                const elem_ptr_reg = try isel.allocIntReg();
                defer isel.freeReg(elem_ptr_reg);
                if (!try elem_vi.value.load(
                    isel,
                    ptr_ty.elemType2(zcu),
                    elem_ptr_reg,
                    .{ .@"volatile" = ptr_info.flags.is_volatile },
                )) break :unused;
                const base_vi = try isel.use(bin_op.lhs);
                const base_mat = try base_vi.matReg(isel);
                const index_vi = try isel.use(bin_op.rhs);
                try isel.elemPtr(elem_ptr_reg, base_mat.r, .add, elem_size, index_vi);
                try base_mat.finish(isel);
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .ptr_elem_ptr => {
            if (isel.live_values.fetchRemove(air.inst_index)) |elem_ptr_vi| unused: {
                defer elem_ptr_vi.value.deref(isel);
                const elem_ptr_ra = try elem_ptr_vi.value.defReg(isel) orelse break :unused;

                const ty_pl = air.data(air.inst_index).ty_pl;
                const bin_op = isel.air.extraData(Air.Bin, ty_pl.payload).data;
                const elem_size = ty_pl.ty.toType().childType(zcu).abiSize(zcu);

                const base_vi = try isel.use(bin_op.lhs);
                const base_mat = try base_vi.matReg(isel);
                const index_vi = try isel.use(bin_op.rhs);
                try isel.elemPtr(elem_ptr_ra, base_mat.r, .add, elem_size, index_vi);
                try base_mat.finish(isel);
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .array_elem_val => {
            if (isel.live_values.fetchRemove(air.inst_index)) |elem_vi| unused: {
                defer elem_vi.value.deref(isel);

                const bin_op = air.data(air.inst_index).bin_op;
                const array_ty = isel.air.typeOf(bin_op.lhs, ip);
                const elem_ty = array_ty.childType(zcu);
                const elem_size = elem_ty.abiSize(zcu);
                if (elem_size <= 16 and array_ty.arrayLenIncludingSentinel(zcu) <= Value.max_parts) if (bin_op.rhs.toInterned()) |index_val| {
                    const elem_offset = elem_size * Constant.fromInterned(index_val).toUnsignedInt(zcu);
                    const array_vi = try isel.use(bin_op.lhs);
                    var elem_part_it = array_vi.field(array_ty, elem_offset, elem_size);
                    const elem_part_vi = try elem_part_it.only(isel);
                    try elem_vi.value.copy(isel, elem_ty, elem_part_vi.?);
                    break :unused;
                };
                switch (elem_size) {
                    0 => unreachable,
                    1, 2, 3, 8 => {
                        const elem_reg = try elem_vi.value.defReg(isel) orelse break :unused;
                        const array_ptr_reg = try isel.allocIntReg();
                        defer isel.freeReg(array_ptr_reg);
                        const index_vi = try isel.use(bin_op.rhs);
                        try isel.loadReg(elem_reg, elem_size, elem_vi.value.signedness(isel), array_ptr_reg, 0);
                        try isel.elemPtr(array_ptr_reg, array_ptr_reg, .add, elem_size, index_vi);
                        const array_vi = try isel.use(bin_op.lhs);
                        try array_vi.address(isel, 0, array_ptr_reg);
                    },
                    else => return isel.fail("TODO: array elem val: {f}", .{isel.fmtType(array_ty)}),
                }
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .struct_field_val => {
            if (isel.live_values.fetchRemove(air.inst_index)) |field_vi| {
                defer field_vi.value.deref(isel);

                const ty_pl = air.data(air.inst_index).ty_pl;
                const extra = isel.air.extraData(Air.StructField, ty_pl.payload).data;
                const agg_ty = isel.air.typeOf(extra.struct_operand, ip);
                const field_ty = ty_pl.ty.toType();
                const field_bit_offset, const field_bit_size, const is_packed = switch (agg_ty.containerLayout(zcu)) {
                    .auto, .@"extern" => .{
                        8 * agg_ty.structFieldOffset(extra.field_index, zcu),
                        8 * field_ty.abiSize(zcu),
                        false,
                    },
                    .@"packed" => .{
                        if (zcu.typeToPackedStruct(agg_ty)) |loaded_struct|
                            zcu.structPackedFieldBitOffset(loaded_struct, extra.field_index)
                        else
                            0,
                        field_ty.bitSize(zcu),
                        true,
                    },
                };
                if (is_packed) return isel.fail("packed field of {f}", .{
                    isel.fmtType(agg_ty),
                });

                const agg_vi = try isel.use(extra.struct_operand);
                switch (agg_ty.zigTypeTag(zcu)) {
                    else => unreachable,
                    .@"struct" => {
                        var agg_part_it = agg_vi.field(agg_ty, @divExact(field_bit_offset, 8), @divExact(field_bit_size, 8));
                        while (try agg_part_it.next(isel)) |agg_part| {
                            var field_part_it = field_vi.value.field(ty_pl.ty.toType(), agg_part.offset, agg_part.vi.size(isel));
                            const field_part_vi = try field_part_it.only(isel);
                            if (field_part_vi.? == agg_part.vi) continue;
                            var field_subpart_it = field_part_vi.?.parts(isel);
                            const field_part_offset = if (field_subpart_it.only()) |field_subpart_vi|
                                field_subpart_vi.get(isel).offset_from_parent
                            else
                                0;
                            while (field_subpart_it.next()) |field_subpart_vi| {
                                const field_subpart_ra = try field_subpart_vi.defReg(isel) orelse continue;
                                const field_subpart_offset, const field_subpart_size = field_subpart_vi.position(isel);
                                var agg_subpart_it = agg_part.vi.field(
                                    field_ty,
                                    agg_part.offset + field_subpart_offset - field_part_offset,
                                    field_subpart_size,
                                );
                                const agg_subpart_vi = try agg_subpart_it.only(isel);
                                try agg_subpart_vi.?.liveOut(isel, field_subpart_ra);
                            }
                        }
                    },
                    .@"union" => return isel.fail("TODO: struct_field_val union", .{}),
                }
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .struct_field_ptr_index_0,
        .struct_field_ptr_index_1,
        .struct_field_ptr_index_2,
        .struct_field_ptr_index_3,
        => |air_tag| {
            if (isel.live_values.fetchRemove(air.inst_index)) |dst_vi| unused: {
                defer dst_vi.value.deref(isel);
                const ty_op = air.data(air.inst_index).ty_op;
                switch (codegen.fieldOffset(
                    isel.air.typeOf(ty_op.operand, ip),
                    ty_op.ty.toType(),
                    switch (air_tag) {
                        else => unreachable,
                        .struct_field_ptr_index_0 => 0,
                        .struct_field_ptr_index_1 => 1,
                        .struct_field_ptr_index_2 => 2,
                        .struct_field_ptr_index_3 => 3,
                    },
                    zcu,
                )) {
                    0 => try dst_vi.value.move(isel, ty_op.operand),
                    else => |field_offset| {
                        const dst_reg = try dst_vi.value.defReg(isel) orelse break :unused;
                        const src_vi = try isel.use(ty_op.operand);
                        const src_mat = try src_vi.matReg(isel);
                        const lo11 = std.math.cast(u11, field_offset) orelse
                            return isel.fail("TODO: struct_field_ptr_index large", .{});
                        if (lo11 > 0) try isel.emit(.addi(dst_reg, src_mat.r, lo11));
                        try src_mat.finish(isel);
                    },
                }
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .slice_elem_val => {
            if (isel.live_values.fetchRemove(air.inst_index)) |elem_vi| unused: {
                defer elem_vi.value.deref(isel);

                const bin_op = air.data(air.inst_index).bin_op;
                const slice_ty = isel.air.typeOf(bin_op.lhs, ip);
                const elem_size = elem_vi.value.size(isel);
                const ptr_info = slice_ty.ptrInfo(zcu);

                const elem_ptr_ra = try isel.allocIntReg();
                defer isel.freeReg(elem_ptr_ra);
                if (!try elem_vi.value.load(isel, slice_ty.elemType2(zcu), elem_ptr_ra, .{
                    .@"volatile" = ptr_info.flags.is_volatile,
                })) break :unused;
                const slice_vi = try isel.use(bin_op.lhs);
                var ptr_part_it = slice_vi.field(slice_ty, 0, 8);
                const ptr_part_vi = try ptr_part_it.only(isel);
                const ptr_part_mat = try ptr_part_vi.?.matReg(isel);
                const index_vi = try isel.use(bin_op.rhs);
                try isel.elemPtr(elem_ptr_ra, ptr_part_mat.r, .add, elem_size, index_vi);
                try ptr_part_mat.finish(isel);
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .slice_elem_ptr => {
            if (isel.live_values.fetchRemove(air.inst_index)) |elem_ptr_vi| unused: {
                defer elem_ptr_vi.value.deref(isel);
                const elem_ptr_ra = try elem_ptr_vi.value.defReg(isel) orelse break :unused;

                const ty_pl = air.data(air.inst_index).ty_pl;
                const bin_op = isel.air.extraData(Air.Bin, ty_pl.payload).data;
                const elem_size = ty_pl.ty.toType().childType(zcu).abiSize(zcu);

                const slice_vi = try isel.use(bin_op.lhs);
                var ptr_part_it = slice_vi.field(isel.air.typeOf(bin_op.lhs, ip), 0, 8);
                const ptr_part_vi = try ptr_part_it.only(isel);
                const ptr_part_mat = try ptr_part_vi.?.matReg(isel);
                const index_vi = try isel.use(bin_op.rhs);
                try isel.elemPtr(elem_ptr_ra, ptr_part_mat.r, .add, elem_size, index_vi);
                try ptr_part_mat.finish(isel);
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .unwrap_errunion_err => {
            if (isel.live_values.fetchRemove(air.inst_index)) |error_set_vi| {
                defer error_set_vi.value.deref(isel);

                const ty_op = air.data(air.inst_index).ty_op;
                const error_union_ty = isel.air.typeOf(ty_op.operand, ip);

                const error_union_vi = try isel.use(ty_op.operand);
                var error_set_part_it = error_union_vi.field(
                    error_union_ty,
                    codegen.errUnionErrorOffset(error_union_ty.errorUnionPayload(zcu), zcu),
                    error_set_vi.value.size(isel),
                );
                const error_set_part_vi = try error_set_part_it.only(isel);
                try error_set_vi.value.copy(isel, ty_op.ty.toType(), error_set_part_vi.?);
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .is_err, .is_non_err => |air_tag| {
            if (isel.live_values.fetchRemove(air.inst_index)) |is_vi| unused: {
                defer is_vi.value.deref(isel);
                const is_reg = try is_vi.value.defReg(isel) orelse break :unused;

                const un_op = air.data(air.inst_index).un_op;
                const error_union_ty = isel.air.typeOf(un_op, ip);
                const error_union_info = ip.indexToKey(error_union_ty.toIntern()).error_union_type;
                const error_set_ty: ZigType = .fromInterned(error_union_info.error_set_type);
                const payload_ty: ZigType = .fromInterned(error_union_info.payload_type);
                const error_set_offset = codegen.errUnionErrorOffset(payload_ty, zcu);
                const error_set_size = error_set_ty.abiSize(zcu);

                switch (air_tag) {
                    .is_err => try isel.emit(.sltiu(is_reg, is_reg, 1)),
                    else => {},
                }

                const error_union_vi = try isel.use(un_op);
                var error_set_part_it = error_union_vi.field(error_union_ty, error_set_offset, error_set_size);
                const error_set_part_vi = try error_set_part_it.only(isel);
                const error_set_part_mat = try error_set_part_vi.?.matReg(isel);

                try isel.emit(.sltiu(is_reg, error_set_part_mat.r, 1));
                try error_set_part_mat.finish(isel);
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .wrap_errunion_err => {
            if (isel.live_values.fetchRemove(air.inst_index)) |error_union_vi| {
                defer error_union_vi.value.deref(isel);

                const ty_op = air.data(air.inst_index).ty_op;
                const error_union_ty = ty_op.ty.toType();
                const error_union_info = ip.indexToKey(error_union_ty.toIntern()).error_union_type;
                const error_set_ty: ZigType = .fromInterned(error_union_info.error_set_type);
                const payload_ty: ZigType = .fromInterned(error_union_info.payload_type);
                const error_set_offset = codegen.errUnionErrorOffset(payload_ty, zcu);
                const payload_offset = codegen.errUnionPayloadOffset(payload_ty, zcu);
                const error_set_size = error_set_ty.abiSize(zcu);
                const payload_size = payload_ty.abiSize(zcu);

                var error_set_part_it = error_union_vi.value.field(error_union_ty, error_set_offset, error_set_size);
                const error_set_part_vi = try error_set_part_it.only(isel);
                try error_set_part_vi.?.move(isel, ty_op.operand);
                if (payload_size > 0) {
                    var payload_part_it = error_union_vi.value.field(error_union_ty, payload_offset, payload_size);
                    const payload_part_vi = try payload_part_it.only(isel);
                    try payload_part_vi.?.defUndef(isel, payload_ty, .{});
                }
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .@"try", .try_cold => {
            const pl_op = air.data(air.inst_index).pl_op;
            const extra = isel.air.extraData(Air.Try, pl_op.payload);
            const error_union_ty = isel.air.typeOf(pl_op.operand, ip);
            const error_union_info = ip.indexToKey(error_union_ty.toIntern()).error_union_type;
            const payload_ty: ZigType = .fromInterned(error_union_info.payload_type);

            const error_union_vi = try isel.use(pl_op.operand);
            if (isel.live_values.fetchRemove(air.inst_index)) |payload_vi| {
                defer payload_vi.value.deref(isel);

                var payload_part_it = error_union_vi.field(
                    error_union_ty,
                    codegen.errUnionPayloadOffset(payload_ty, zcu),
                    payload_vi.value.size(isel),
                );
                const payload_part_vi = try payload_part_it.only(isel);
                try payload_vi.value.copy(isel, payload_ty, payload_part_vi.?);
            }

            const cont_label = isel.instructions.items.len;
            const cont_live_registers = isel.live_registers;
            try isel.body(@ptrCast(isel.air.extra.items[extra.end..][0..extra.data.body_len]));
            try isel.merge(&cont_live_registers, .{});

            var error_set_part_it = error_union_vi.field(
                error_union_ty,
                codegen.errUnionErrorOffset(payload_ty, zcu),
                ZigType.fromInterned(error_union_info.error_set_type).abiSize(zcu),
            );
            const error_set_part_vi = try error_set_part_it.only(isel);
            const error_set_part_mat = try error_set_part_vi.?.matReg(isel);
            try isel.emit(.beq(
                error_set_part_mat.r,
                .zero,
                @intCast((isel.instructions.items.len + 1 - cont_label) * 4),
            ));
            try error_set_part_mat.finish(isel);

            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },

        // control flow
        .br => {
            const br = air.data(air.inst_index).br;
            try isel.blocks.getPtr(br.block_inst).?.branch(isel);
            if (isel.live_values.get(br.block_inst)) |dst_vi| try dst_vi.move(isel, br.operand);
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .block => {
            const ty_pl = air.data(air.inst_index).ty_pl;
            const extra = isel.air.extraData(Air.Block, ty_pl.payload);
            try isel.block(air.inst_index, ty_pl.ty.toType(), @ptrCast(
                isel.air.extra.items[extra.end..][0..extra.data.body_len],
            ));
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .repeat => {
            const repeat = air.data(air.inst_index).repeat;
            try isel.loops.getPtr(repeat.loop_inst).?.branch(isel);
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .cond_br => {
            const pl_op = air.data(air.inst_index).pl_op;
            const extra = isel.air.extraData(Air.CondBr, pl_op.payload);

            try isel.body(@ptrCast(isel.air.extra.items[extra.end + extra.data.then_body_len ..][0..extra.data.else_body_len]));
            const else_label = isel.instructions.items.len;
            const else_live_registers = isel.live_registers;
            try isel.body(@ptrCast(isel.air.extra.items[extra.end..][0..extra.data.then_body_len]));
            try isel.merge(&else_live_registers, .{});

            const cond_vi = try isel.use(pl_op.operand);
            const cond_mat = try cond_vi.matReg(isel);
            try isel.emit(.beq(
                cond_mat.r,
                .zero,
                @intCast((isel.instructions.items.len + 1 - else_label) * 4),
            ));
            try cond_mat.finish(isel);

            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .loop => {
            const ty_pl = air.data(air.inst_index).ty_pl;
            const extra = isel.air.extraData(Air.Block, ty_pl.payload);
            const loops = isel.loops.values();
            const loop_index = isel.loops.getIndex(air.inst_index).?;
            const loop = &loops[loop_index];

            tracking_log.debug("{f}", .{
                isel.fmtDom(air.inst_index, loop.dom, @intCast(isel.blocks.count())),
            });
            tracking_log.debug("{f}", .{isel.fmtLoopLive(air.inst_index)});
            assert(loop.depth == isel.blocks.count());

            if (false) {
                // loops are dumb...
                for (isel.loop_live.list.items[loop.live..loops[loop_index + 1].live]) |live_inst| {
                    const live_vi = try isel.use(live_inst.toRef());
                    try live_vi.mat(isel);
                }

                // IT'S DOM TIME!!!
                for (isel.blocks.values(), 0..) |*dom_block, dom_index| {
                    if (@as(u1, @truncate(isel.dom.items[
                        loop.dom + dom_index / @bitSizeOf(DomInt)
                    ] >> @truncate(dom_index))) == 0) continue;
                    var live_reg_it = dom_block.live_registers.iterator();
                    while (live_reg_it.next()) |live_reg_entry| switch (live_reg_entry.value.*) {
                        _ => |live_vi| try live_vi.mat(isel),
                        .allocating => unreachable,
                        .free => {},
                    };
                }
            }

            loop.live_registers = isel.live_registers;
            loop.repeat_list = Loop.empty_list;
            try isel.body(@ptrCast(isel.air.extra.items[extra.end..][0..extra.data.body_len]));
            try isel.merge(&loop.live_registers, .{ .fill_extra = true });

            var repeat_label = loop.repeat_list;
            assert(repeat_label != Loop.empty_list);
            while (repeat_label != Loop.empty_list) {
                const instruction = &isel.instructions.items[repeat_label];
                const next_repeat_label = instruction.*;
                instruction.* = .jal(.zero, -@as(i21, @intCast((isel.instructions.items.len - 1 - repeat_label) * 4)));
                repeat_label = @bitCast(next_repeat_label);
            }

            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .call => {
            const pl_op = air.data(air.inst_index).pl_op;
            const extra = isel.air.extraData(Air.Call, pl_op.payload);
            const args: []const Air.Inst.Ref = @ptrCast(isel.air.extra.items[extra.end..][0..extra.data.args_len]);
            const callee_ty = isel.air.typeOf(pl_op.operand, ip);
            const func_info = switch (ip.indexToKey(callee_ty.toIntern())) {
                else => unreachable,
                .func_type => |func_type| func_type,
                .ptr_type => |ptr_type| ip.indexToKey(ptr_type.child).func_type,
            };

            try call.prepareReturn(isel);
            const maybe_def_ret_vi = isel.live_values.fetchRemove(air.inst_index);
            var maybe_ret_addr_vi: ?Value.Index = null;
            if (maybe_def_ret_vi) |def_ret_vi| {
                defer def_ret_vi.value.deref(isel);

                var ret_it: CallAbiIterator = .init;
                const ret_vi = try ret_it.ret(isel, isel.air.typeOfIndex(air.inst_index, ip));
                defer ret_vi.?.deref(isel);
                switch (ret_vi.?.parent(isel)) {
                    .unallocated, .stack_slot => if (ret_vi.?.hint(isel)) |ret_ra| {
                        try call.returnLiveIn(isel, def_ret_vi.value, ret_ra);
                    } else {
                        var def_ret_part_it = def_ret_vi.value.parts(isel);
                        var ret_part_it = ret_vi.?.parts(isel);
                        while (def_ret_part_it.next()) |ret_part_vi| {
                            try call.returnLiveIn(isel, ret_part_vi, ret_part_it.next().?.hint(isel).?);
                        }
                    },
                    .value, .constant => unreachable,
                    .address => |address_vi| {
                        maybe_ret_addr_vi = address_vi;
                        _ = try def_ret_vi.value.defAddr(isel, isel.air.typeOfIndex(air.inst_index, ip), .{
                            .expected_live_registers = &call.caller_saved_regs,
                        });
                    },
                }
            }
            try call.finishReturn(isel);

            try call.prepareCallee(isel);
            if (pl_op.operand.toInterned()) |ct_callee| {
                try isel.emit(.jalr(.ra, .ra, 0));
                { // load the symbol address into RA, reloc applies to these two instructions

                    try isel.emit(.addi(.ra, .ra, 0));
                    try isel.emit(.lui(.ra, 0));
                }

                try isel.nav_relocs.append(gpa, switch (ip.indexToKey(ct_callee)) {
                    else => unreachable,
                    inline .@"extern", .func => |func| .{
                        .nav = func.owner_nav,
                        .reloc = .{ .label = @intCast(isel.instructions.items.len - 1) },
                    },
                    .ptr => |ptr| .{
                        .nav = ptr.base_addr.nav,
                        .reloc = .{
                            .label = @intCast(isel.instructions.items.len - 1),
                            .addend = ptr.byte_offset,
                        },
                    },
                });
            } else {
                const callee_vi = try isel.use(pl_op.operand);
                const callee_mat = try callee_vi.matReg(isel);
                try isel.emit(.jalr(.ra, callee_mat.r, 0));
                try callee_mat.finish(isel);
            }
            try call.finishCallee(isel);

            try call.prepareParams(isel);
            if (maybe_ret_addr_vi) |ret_addr_vi| try call.paramAddress(
                isel,
                maybe_def_ret_vi.?.value,
                ret_addr_vi.hint(isel).?,
            );
            var param_it: CallAbiIterator = .init;
            for (args, 0..) |arg, arg_index| {
                const param_ty = isel.air.typeOf(arg, ip);
                const param_vi = param_vi: {
                    if (arg_index >= func_info.param_types.len) {
                        assert(func_info.is_var_args);
                        return isel.fail("TODO: var arg support", .{});
                    }
                    break :param_vi try param_it.param(isel, param_ty);
                } orelse continue;
                defer param_vi.deref(isel);
                const arg_vi = try isel.use(arg);
                switch (param_vi.parent(isel)) {
                    .unallocated => if (param_vi.hint(isel)) |param_ra| {
                        try call.paramLiveOut(isel, arg_vi, param_ra);
                    } else {
                        var param_part_it = param_vi.parts(isel);
                        var arg_part_it = arg_vi.parts(isel);
                        if (arg_part_it.only()) |_| {
                            try isel.values.ensureUnusedCapacity(gpa, param_part_it.remaining);
                            arg_vi.setParts(isel, param_part_it.remaining);
                            while (param_part_it.next()) |param_part_vi| _ = arg_vi.addPart(
                                isel,
                                param_part_vi.get(isel).offset_from_parent,
                                param_part_vi.size(isel),
                            );
                            param_part_it = param_vi.parts(isel);
                            arg_part_it = arg_vi.parts(isel);
                        }
                        while (param_part_it.next()) |param_part_vi| {
                            const arg_part_vi = arg_part_it.next().?;
                            assert(arg_part_vi.get(isel).offset_from_parent ==
                                param_part_vi.get(isel).offset_from_parent);
                            assert(arg_part_vi.size(isel) == param_part_vi.size(isel));
                            try call.paramLiveOut(isel, arg_part_vi, param_part_vi.hint(isel).?);
                        }
                    },
                    .stack_slot => |stack_slot| try arg_vi.store(
                        isel,
                        param_ty,
                        stack_slot.base,
                        .{ .offset = @intCast(stack_slot.offset) },
                    ),
                    .value, .constant => unreachable,
                    .address => |address_vi| try call.paramAddress(isel, arg_vi, address_vi.hint(isel).?),
                }
            }
            try call.finishParams(isel);

            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .ret, .ret_safe => {
            assert(isel.blocks.keys()[0] == Block.main);
            try isel.blocks.values()[0].branch(isel);
            if (isel.live_values.get(Block.main)) |ret_vi| {
                const un_op = air.data(air.inst_index).un_op;
                const src_vi = try isel.use(un_op);
                switch (ret_vi.parent(isel)) {
                    .unallocated, .stack_slot => if (ret_vi.hint(isel)) |ret_ra| {
                        try src_vi.liveOut(isel, ret_ra);
                    } else {
                        var ret_part_it = ret_vi.parts(isel);
                        var src_part_it = src_vi.parts(isel);
                        if (src_part_it.only()) |_| {
                            try isel.values.ensureUnusedCapacity(gpa, ret_part_it.remaining);
                            src_vi.setParts(isel, ret_part_it.remaining);
                            while (ret_part_it.next()) |ret_part_vi| {
                                const src_part_vi = src_vi.addPart(
                                    isel,
                                    ret_part_vi.get(isel).offset_from_parent,
                                    ret_part_vi.size(isel),
                                );
                                switch (ret_part_vi.signedness(isel)) {
                                    .signed => src_part_vi.setSignedness(isel, .signed),
                                    .unsigned => {},
                                }
                                if (ret_part_vi.isVector(isel)) src_part_vi.setIsVector(isel);
                            }
                            ret_part_it = ret_vi.parts(isel);
                            src_part_it = src_vi.parts(isel);
                        }
                        while (ret_part_it.next()) |ret_part_vi| {
                            const src_part_vi = src_part_it.next().?;
                            assert(ret_part_vi.get(isel).offset_from_parent == src_part_vi.get(isel).offset_from_parent);
                            assert(ret_part_vi.size(isel) == src_part_vi.size(isel));
                            try src_part_vi.liveOut(isel, ret_part_vi.hint(isel).?);
                        }
                    },
                    .value, .constant => unreachable,
                    // .address => |address_vi| {
                    //     const ptr_mat = try address_vi.matReg(isel);
                    //     try src_vi.store(isel, isel.air.typeOf(un_op, ip), ptr_mat.ra, .{});
                    //     try ptr_mat.finish(isel);
                    // },
                    .address => return isel.fail("TODO: ret address", .{}),
                }
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .not => {
            if (isel.live_values.fetchRemove(air.inst_index)) |res_vi| unused: {
                defer res_vi.value.deref(isel);

                const ty_op = air.data(air.inst_index).ty_op;
                const res_reg = try res_vi.value.defReg(isel) orelse break :unused;
                const src_vi = try isel.use(ty_op.operand);
                const src_mat = try src_vi.matReg(isel);

                if (ty_op.ty == .bool_type) {
                    try isel.emit(.xori(res_reg, src_mat.r, 1));
                } else {
                    const ty = ty_op.ty.toType();
                    const int_info = ty.intInfo(zcu);
                    if (!std.math.isPowerOfTwo(int_info.bits)) return isel.fail("TODO: non-pow2 knot {f}", .{isel.fmtType(ty)});

                    try isel.emit(.xori(res_reg, src_mat.r, -1));
                }

                try src_mat.finish(isel);
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .intcast => |air_tag| {
            if (isel.live_values.fetchRemove(air.inst_index)) |dst_vi| unused: {
                defer dst_vi.value.deref(isel);
                const ty_op = air.data(air.inst_index).ty_op;
                const dst_ty = ty_op.ty.toType();
                const dst_int_info = dst_ty.intInfo(zcu);
                const src_ty = isel.air.typeOf(ty_op.operand, ip);
                const src_int_info = src_ty.intInfo(zcu);

                if ((dst_int_info.bits <= 8 and src_int_info.bits <= 8) or
                    (dst_int_info.bits > 8 and dst_int_info.bits <= 16 and
                        src_int_info.bits > 8 and src_int_info.bits <= 16) or
                    (dst_int_info.bits > 16 and dst_int_info.bits <= 32 and
                        src_int_info.bits > 16 and src_int_info.bits <= 32) or
                    (dst_int_info.bits > 32 and dst_int_info.bits <= 64 and
                        src_int_info.bits > 32 and src_int_info.bits <= 64) or
                    (dst_int_info.bits > 64 and dst_int_info.bits > 64 and
                        (dst_int_info.bits - 1) / 128 == (dst_int_info.bits - 1) / 128))
                {
                    try dst_vi.value.move(isel, ty_op.operand);
                } else if (dst_int_info.bits <= 64 and src_int_info.bits <= 32) {
                    const dst_reg = try dst_vi.value.defReg(isel) orelse break :unused;
                    const src_vi = try isel.use(ty_op.operand);
                    const src_mat = try src_vi.matReg(isel);
                    const offset: u6 = @intCast(64 - src_int_info.bits);
                    try isel.emit(switch (src_int_info.signedness) {
                        .unsigned => .srli(dst_reg, dst_reg, offset),
                        .signed => .srai(dst_reg, dst_reg, offset),
                    });
                    try isel.emit(.slli(dst_reg, src_mat.r, offset));
                    try src_mat.finish(isel);
                } else return isel.fail("too big {t} {f} {f}", .{ air_tag, isel.fmtType(dst_ty), isel.fmtType(src_ty) });
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .bitcast => {
            if (isel.live_values.fetchRemove(air.inst_index)) |dst_vi| unused: {
                defer dst_vi.value.deref(isel);
                const ty_op = air.data(air.inst_index).ty_op;
                const dst_ty = ty_op.ty.toType();
                const dst_tag = dst_ty.zigTypeTag(zcu);
                const src_ty = isel.air.typeOf(ty_op.operand, ip);
                const src_tag = src_ty.zigTypeTag(zcu);
                if (dst_ty.isAbiInt(zcu) and (src_tag == .bool or src_ty.isAbiInt(zcu))) {
                    const dst_int_info = dst_ty.intInfo(zcu);
                    const src_int_info: std.builtin.Type.Int = if (src_tag == .bool) .{ .signedness = undefined, .bits = 1 } else src_ty.intInfo(zcu);
                    if (dst_tag != .@"struct" and src_tag != .@"struct" and src_tag != .bool and dst_int_info.signedness == src_int_info.signedness)
                        try dst_vi.value.move(isel, ty_op.operand)
                    else switch (dst_int_info.bits) {
                        0 => unreachable,
                        1...31 => |dst_bits| {
                            const dst_reg = try dst_vi.value.defReg(isel) orelse break :unused;
                            const src_vi = try isel.use(ty_op.operand);
                            const src_mat = try src_vi.matReg(isel);
                            const offset: u6 = @intCast(64 - dst_bits);
                            try isel.emit(switch (dst_int_info.signedness) {
                                .unsigned => .srli(dst_reg, dst_reg, offset),
                                .signed => .srai(dst_reg, dst_reg, offset),
                            });
                            try isel.emit(.slli(dst_reg, src_mat.r, offset));
                        },
                        32, 64 => try dst_vi.value.move(isel, ty_op.operand),
                        else => return isel.fail("TODO: bitcast {f} {f}", .{ isel.fmtType(dst_ty), isel.fmtType(src_ty) }),
                    }
                } else if ((dst_ty.isPtrAtRuntime(zcu) or dst_ty.isAbiInt(zcu)) and (src_ty.isPtrAtRuntime(zcu) or src_ty.isAbiInt(zcu))) {
                    try dst_vi.value.move(isel, ty_op.operand);
                } else {
                    return isel.fail("TODO: bitcast {f} -> {f}", .{ isel.fmtType(src_ty), isel.fmtType(dst_ty) });
                }
            }
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },

        // debug instructions
        .dbg_stmt => if (air.next()) |next_air_tag| continue :air_tag next_air_tag,
        .dbg_empty_stmt => {
            try isel.emit(.noop);
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .dbg_inline_block => {
            const ty_pl = air.data(air.inst_index).ty_pl;
            const extra = isel.air.extraData(Air.DbgInlineBlock, ty_pl.payload);
            try isel.block(air.inst_index, ty_pl.ty.toType(), @ptrCast(
                isel.air.extra.items[extra.end..][0..extra.data.body_len],
            ));
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .dbg_var_ptr, .dbg_var_val, .dbg_arg_inline => {
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },

        // misc.
        .unreach => if (air.next()) |next_air_tag| continue :air_tag next_air_tag,
        .trap => {
            try isel.emit(.unimp);
            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
        .assembly => {
            const ty_pl = air.data(air.inst_index).ty_pl;
            const extra = isel.air.extraData(Air.Asm, ty_pl.payload);
            var extra_index = extra.end;
            const outputs: []const Air.Inst.Ref = @ptrCast(isel.air.extra.items[extra_index..][0..extra.data.flags.outputs_len]);
            extra_index += outputs.len;
            const inputs: []const Air.Inst.Ref = @ptrCast(isel.air.extra.items[extra_index..][0..extra.data.inputs_len]);
            extra_index += inputs.len;

            var as: Assemble = .{
                .source = undefined,
                .operands = .empty,
            };
            defer as.operands.deinit(gpa);

            for (outputs) |output| {
                const extra_bytes = std.mem.sliceAsBytes(isel.air.extra.items[extra_index..]);
                const constraint = std.mem.sliceTo(std.mem.sliceAsBytes(isel.air.extra.items[extra_index..]), 0);
                const name = std.mem.sliceTo(extra_bytes[constraint.len + 1 ..], 0);
                // This equation accounts for the fact that even if we have exactly 4 bytes
                // for the string, we still use the next u32 for the null terminator.
                extra_index += (constraint.len + name.len + (2 + 3)) / 4;

                switch (output) {
                    else => return isel.fail("invalid constraint: '{s}'", .{constraint}),
                    .none => if (std.mem.startsWith(u8, constraint, "={") and std.mem.endsWith(u8, constraint, "}")) {
                        const output_reg = Register.parse(constraint["={".len .. constraint.len - "}".len]) orelse
                            return isel.fail("invalid constraint: '{s}'", .{constraint});
                        if (isel.live_values.fetchRemove(air.inst_index)) |output_vi| {
                            defer output_vi.value.deref(isel);
                            try output_vi.value.defLiveIn(isel, output_reg, comptime &.initFill(.free));
                            isel.freeReg(output_reg);
                        }
                        if (!std.mem.eql(u8, name, "_")) {
                            const operand_gop = try as.operands.getOrPut(gpa, name);
                            if (operand_gop.found_existing) return isel.fail("duplicate output name: '{s}'", .{name});
                            operand_gop.value_ptr.* = .{ .register = switch (ty_pl.ty.toType().abiSize(zcu)) {
                                0 => unreachable,
                                1...8 => output_reg,
                                else => return isel.fail("too big output type: '{f}'", .{isel.fmtType(ty_pl.ty.toType())}),
                            } };
                        }
                    },
                }
            }

            const input_mats = try gpa.alloc(Value.Materialize, inputs.len);
            defer gpa.free(input_mats);
            const inputs_extra_index = extra_index;
            for (inputs, input_mats) |input, *input_mat| {
                const extra_bytes = std.mem.sliceAsBytes(isel.air.extra.items[extra_index..]);
                const constraint = std.mem.sliceTo(extra_bytes, 0);
                const name = std.mem.sliceTo(extra_bytes[constraint.len + 1 ..], 0);
                // This equation accounts for the fact that even if we have exactly 4 bytes
                // for the string, we still use the next u32 for the null terminator.
                extra_index += (constraint.len + name.len + (2 + 3)) / 4;

                if (std.mem.startsWith(u8, constraint, "{") and std.mem.endsWith(u8, constraint, "}")) {
                    const input_reg = Register.parse(constraint["{".len .. constraint.len - "}".len]) orelse
                        return isel.fail("invalid constraint: '{s}'", .{constraint});
                    input_mat.* = .{ .vi = try isel.use(input), .r = input_reg };
                    if (!std.mem.eql(u8, name, "_")) {
                        const operand_gop = try as.operands.getOrPut(gpa, name);
                        if (operand_gop.found_existing) return isel.fail("duplicate input name: '{s}'", .{name});
                        const input_ty = isel.air.typeOf(input, ip);
                        operand_gop.value_ptr.* = .{ .register = switch (input_ty.abiSize(zcu)) {
                            0 => unreachable,
                            1...8 => input_reg,
                            else => return isel.fail("too big input type: '{f}'", .{
                                isel.fmtType(isel.air.typeOf(input, ip)),
                            }),
                        } };
                    }
                } else return isel.fail("invalid constraint: '{s}'", .{constraint});
            }

            const clobbers = ip.indexToKey(extra.data.clobbers).aggregate;
            const clobbers_ty: ZigType = .fromInterned(clobbers.ty);
            for (0..clobbers_ty.structFieldCount(zcu)) |field_index| {
                switch (switch (clobbers.storage) {
                    .bytes => unreachable,
                    .elems => |elems| elems[field_index],
                    .repeated_elem => |repeated_elem| repeated_elem,
                }) {
                    else => unreachable,
                    .bool_false => continue,
                    .bool_true => {},
                }
                const clobber_name = clobbers_ty.structFieldName(field_index, zcu).toSlice(ip).?;
                if (std.mem.eql(u8, clobber_name, "memory")) continue;
                const clobber_reg = Register.parse(clobber_name) orelse
                    return isel.fail("unable to parse clobber: '{s}'", .{clobber_name});
                const live_vi = isel.live_registers.getPtr(clobber_reg);
                switch (live_vi.*) {
                    _ => {},
                    .allocating => return isel.fail("clobbered twice: '{s}'", .{clobber_name}),
                    .free => live_vi.* = .allocating,
                }
            }
            for (0..clobbers_ty.structFieldCount(zcu)) |field_index| {
                switch (switch (clobbers.storage) {
                    .bytes => unreachable,
                    .elems => |elems| elems[field_index],
                    .repeated_elem => |repeated_elem| repeated_elem,
                }) {
                    else => unreachable,
                    .bool_false => continue,
                    .bool_true => {},
                }
                const clobber_name = clobbers_ty.structFieldName(field_index, zcu).toSlice(ip).?;
                if (std.mem.eql(u8, clobber_name, "memory")) continue;
                const clobber_ra = Register.parse(clobber_name).?;
                const live_vi = isel.live_registers.getPtr(clobber_ra);
                switch (live_vi.*) {
                    _ => {
                        if (!try isel.fill(clobber_ra))
                            return isel.fail("unable to clobber: '{s}'", .{clobber_name});
                        assert(live_vi.* == .free);
                        live_vi.* = .allocating;
                    },
                    .allocating => {},
                    .free => unreachable,
                }
            }

            as.source = std.mem.sliceAsBytes(isel.air.extra.items[extra_index..])[0..extra.data.source_len :0];
            const asm_start = isel.instructions.items.len;
            while (as.nextInstruction() catch |err| switch (err) {
                error.InvalidSyntax => {
                    const remaining_source = std.mem.span(as.source);
                    return isel.fail("unable to assemble: '{s}'", .{std.mem.trim(
                        u8,
                        as.source[0 .. std.mem.indexOfScalar(u8, remaining_source, '\n') orelse remaining_source.len],
                        &std.ascii.whitespace,
                    )});
                },
            }) |instruction| try isel.emit(instruction);
            std.mem.reverse(Instruction, isel.instructions.items[asm_start..]);

            extra_index = inputs_extra_index;
            for (input_mats) |input_mat| {
                const extra_bytes = std.mem.sliceAsBytes(isel.air.extra.items[extra_index..]);
                const constraint = std.mem.sliceTo(extra_bytes, 0);
                const name = std.mem.sliceTo(extra_bytes[constraint.len + 1 ..], 0);
                // This equation accounts for the fact that even if we have exactly 4 bytes
                // for the string, we still use the next u32 for the null terminator.
                extra_index += (constraint.len + name.len + (2 + 3)) / 4;

                if (std.mem.startsWith(u8, constraint, "{") and std.mem.endsWith(u8, constraint, "}")) {
                    try input_mat.vi.liveOut(isel, input_mat.r);
                } else return isel.fail("TODO: assembly handle input, '{s}' '{s}'", .{ constraint, name });
            }

            for (0..clobbers_ty.structFieldCount(zcu)) |field_index| {
                switch (switch (clobbers.storage) {
                    .bytes => unreachable,
                    .elems => |elems| elems[field_index],
                    .repeated_elem => |repeated_elem| repeated_elem,
                }) {
                    else => unreachable,
                    .bool_false => continue,
                    .bool_true => {},
                }
                const clobber_name = clobbers_ty.structFieldName(field_index, zcu).toSlice(ip).?;
                if (std.mem.eql(u8, clobber_name, "memory")) continue;
                isel.freeReg(Register.parse(clobber_name).?);
            }

            if (air.next()) |next_air_tag| continue :air_tag next_air_tag;
        },
    }
}

const TryAllocRegResult = union(enum) {
    allocated: Register,
    fill_candidate: Register,
    out_of_registers,
};

fn tryAllocIntReg(isel: *Select) TryAllocRegResult {
    var failed_result: TryAllocRegResult = .out_of_registers;
    // TODO: we're ignoring t0-t2, but too lazy to add it rn
    var ra: Register = .a0;
    while (true) : (ra = @enumFromInt(@intFromEnum(ra) + 1)) {
        switch (@intFromEnum(ra)) {
            else => {},
            @intFromEnum(Register.s0)...@intFromEnum(Register.s1), //
            @intFromEnum(Register.s2)...@intFromEnum(Register.s11),
            => continue, // saved registers
        }
        const live_vi = isel.live_registers.getPtr(ra);
        switch (live_vi.*) {
            _ => switch (failed_result) {
                .allocated => unreachable,
                .fill_candidate => {},
                .out_of_registers => failed_result = .{ .fill_candidate = ra },
            },
            .allocating => {},
            .free => {
                live_vi.* = .allocating;
                isel.saved_registers.insert(ra);
                return .{ .allocated = ra };
            },
        }
        if (ra == Register.t6) return failed_result;
    }
}

fn allocIntReg(isel: *Select) !Register {
    switch (isel.tryAllocIntReg()) {
        .allocated => |ra| return ra,
        .fill_candidate => |ra| {
            assert(try isel.fillMemory(ra));
            const live_vi = isel.live_registers.getPtr(ra);
            assert(live_vi.* == .free);
            live_vi.* = .allocating;
            return ra;
        },
        .out_of_registers => return isel.fail("ran out of registers", .{}),
    }
}

const RegLock = struct {
    r: Register,
    const empty: RegLock = .{ .r = .zero };

    fn unlock(lock: RegLock, isel: *Select) void {
        switch (lock.r) {
            else => |r| isel.freeReg(r),
            .zero => {},
        }
    }
};

fn lockReg(isel: *Select, r: Register) RegLock {
    assert(r != .zero);
    const live_vi = isel.live_registers.getPtr(r);
    assert(live_vi.* == .free);
    live_vi.* = .allocating;
    return .{ .r = r };
}

fn tryLockReg(isel: *Select, r: Register) RegLock {
    assert(r != .zero);
    const live_vi = isel.live_registers.getPtr(r);
    switch (live_vi.*) {
        _ => unreachable,
        .allocating => return .{ .r = .zero },
        .free => {
            live_vi.* = .allocating;
            return .{ .r = r };
        },
    }
}

fn freeReg(isel: *Select, ra: Register) void {
    assert(ra != .zero);
    const live_vi = isel.live_registers.getPtr(ra);
    assert(live_vi.* == .allocating);
    live_vi.* = .free;
}

fn use(isel: *Select, air_ref: Air.Inst.Ref) !Value.Index {
    const zcu = isel.pt.zcu;
    const ip = &zcu.intern_pool;
    try isel.values.ensureUnusedCapacity(zcu.gpa, 1);
    const vi, const ty = if (air_ref.toIndex()) |air_inst_index| vi_ty: {
        const live_gop = try isel.live_values.getOrPut(zcu.gpa, air_inst_index);
        if (live_gop.found_existing) return live_gop.value_ptr.*;
        const ty = isel.air.typeOf(air_ref, ip);
        const vi = isel.initValue(ty);
        tracking_log.debug("${d} <- %{d}", .{
            @intFromEnum(vi),
            @intFromEnum(air_inst_index),
        });
        live_gop.value_ptr.* = vi.ref(isel);
        break :vi_ty .{ vi, ty };
    } else vi_ty: {
        const constant: Constant = .fromInterned(air_ref.toInterned().?);
        const ty = constant.typeOf(zcu);
        const vi = isel.initValue(ty);
        tracking_log.debug("${d} <- <{f}, {f}>", .{
            @intFromEnum(vi),
            isel.fmtType(ty),
            isel.fmtConstant(constant),
        });
        vi.setParent(isel, .{ .constant = constant });
        break :vi_ty .{ vi, ty };
    };
    if (ty.isAbiInt(zcu)) {
        const int_info = ty.intInfo(zcu);
        if (int_info.bits <= 32) vi.setSignedness(isel, int_info.signedness);
    } else if (vi.size(isel) <= 32 and
        CallAbiIterator.homogeneousAggregateBaseType(zcu, ty.toIntern()) != null)
        return isel.fail("TODO: use homogeneous {f}", .{isel.fmtType(ty)});

    return vi;
}

fn initValue(isel: *Select, ty: ZigType) Value.Index {
    const zcu = isel.pt.zcu;
    return isel.initValueAdvanced(ty.abiAlignment(zcu), 0, ty.abiSize(zcu));
}
fn initValueAdvanced(
    isel: *Select,
    parent_alignment: InternPool.Alignment,
    offset_from_parent: u64,
    size: u64,
) Value.Index {
    defer isel.values.addOneAssumeCapacity().* = .{
        .refs = 0,
        .flags = .{
            .alignment = .fromLog2Units(@min(parent_alignment.toLog2Units(), @ctz(offset_from_parent))),
            .parent_tag = .unallocated,
            .location_tag = if (size > 16) .large else .small,
            .parts_len_minus_one = 0,
        },
        .offset_from_parent = offset_from_parent,
        .parent_payload = .{ .unallocated = {} },
        .location_payload = if (size > 16) .{ .large = .{
            .size = size,
        } } else .{ .small = .{
            .size = @intCast(size),
            .signedness = .unsigned,
            .is_vector = false,
            .hint = .zero,
            .register = .zero,
        } },
        .parts = undefined,
    };
    return @enumFromInt(isel.values.items.len);
}

fn fmtType(isel: *Select, ty: ZigType) ZigType.Formatter {
    return ty.fmt(isel.pt);
}

fn fmtConstant(isel: *Select, constant: Constant) @typeInfo(@TypeOf(Constant.fmtValue)).@"fn".return_type.? {
    return constant.fmtValue(isel.pt);
}

fn block(
    isel: *Select,
    air_inst_index: Air.Inst.Index,
    res_ty: ZigType,
    air_body: []const Air.Inst.Index,
) !void {
    if (res_ty.toIntern() != .noreturn_type) {
        isel.blocks.putAssumeCapacityNoClobber(air_inst_index, .{
            .live_registers = isel.live_registers,
            .target_label = @intCast(isel.instructions.items.len),
        });
    }
    try isel.body(air_body);
    if (res_ty.toIntern() != .noreturn_type) {
        const block_entry = isel.blocks.pop().?;
        assert(block_entry.key == air_inst_index);
        if (isel.live_values.fetchRemove(air_inst_index)) |result_vi| result_vi.value.deref(isel);
    }
}

fn fill(isel: *Select, dst: Register) error{ OutOfMemory, CodegenFail }!bool {
    switch (dst) {
        else => {},
        .zero => return false,
    }
    const dst_live_vi = isel.live_registers.getPtr(dst);
    const dst_vi = switch (dst_live_vi.*) {
        _ => |dst_vi| dst_vi,
        .allocating => return false,
        .free => return true,
    };
    const src_ra = src_ra: {
        if (dst_vi.hint(isel)) |hint_ra| {
            assert(dst_live_vi.* == dst_vi);
            dst_live_vi.* = .allocating;
            defer dst_live_vi.* = dst_vi;
            if (try isel.fill(hint_ra)) {
                isel.saved_registers.insert(hint_ra);
                break :src_ra hint_ra;
            }
        }
        switch (if (dst_vi.isVector(isel)) return isel.fail("TODO: fill vector", .{}) else isel.tryAllocIntReg()) {
            .allocated => |ra| break :src_ra ra,
            inline .fill_candidate, .out_of_registers => return isel.fillMemory(dst),
        }
    };
    try dst_vi.liveIn(isel, src_ra, comptime &.initFill(.free));
    const src_live_vi = isel.live_registers.getPtr(src_ra);
    assert(src_live_vi.* == .allocating);
    src_live_vi.* = dst_vi;
    return true;
}

fn fillMemory(isel: *Select, dst_reg: Register) error{ OutOfMemory, CodegenFail }!bool {
    const dst_live_vi = isel.live_registers.getPtr(dst_reg);
    const dst_vi = switch (dst_live_vi.*) {
        _ => |dst_vi| dst_vi,
        .allocating => return false,
        .free => return true,
    };
    const dst_vi_reg = &dst_vi.get(isel).location_payload.small.register;
    assert(dst_vi_reg.* == dst_reg);
    const base_reg = if (dst_reg.isVector()) return isel.fail("TODO: fillMemory vector", .{}) else dst_reg;
    defer if (base_reg != dst_reg) isel.freeReg(base_reg);
    try isel.loadReg(dst_reg, dst_vi.size(isel), dst_vi.signedness(isel), base_reg, 0);
    dst_vi_reg.* = .zero;
    try dst_vi.address(isel, 0, base_reg);
    dst_live_vi.* = .free;
    return true;
}

/// Merges possibly differing value tracking into a consistent state.
///
/// At a conditional branch, if a value is expected in the same register on both
/// paths, or only expected in a register on only one path, tracking is updated:
///
///     $0 -> r0 // final state is now consistent with both paths
///      b.cond else
///     then:
///     $0 -> r0 // updated if not already consistent with else
///      ...
///      b end
///     else:
///     $0 -> r0
///      ...
///     end:
///
/// At a conditional branch, if a value is expected in different registers on
/// each path, mov instructions are emitted:
///
///     $0 -> r0 // final state is now consistent with both paths
///      b.cond else
///     then:
///     $0 -> r0 // updated to be consistent with else
///      mov x1, x0 // emitted to merge the inconsistent states
///     $0 -> r1
///      ...
///      b end
///     else:
///     $0 -> r0
///      ...
///     end:
///
/// At a loop, a value that is expected in a register at the repeats is updated:
///
///     $0 -> r0 // final state is now consistent with all paths
///     loop:
///     $0 -> r0 // updated to be consistent with the repeats
///      ...
///     $0 -> r0
///      b.cond loop
///      ...
///     $0 -> r0
///      b loop
///
/// At a loop, a value that is expected in a register at the top is filled:
///
///     $0 -> [sp, #A] // final state is now consistent with all paths
///     loop:
///     $0 -> [sp, #A] // updated to be consistent with the repeats
///      ldr x0, [sp, #A] // emitted to merge the inconsistent states
///     $0 -> r0
///      ...
///     $0 -> [sp, #A]
///      b.cond loop
///      ...
///     $0 -> [sp, #A]
///      b loop
///
/// At a loop, if a value that is expected in different registers on each path,
/// mov instructions are emitted:
///
///     $0 -> r0 // final state is now consistent with all paths
///     loop:
///     $0 -> r0 // updated to be consistent with the repeats
///      mov x1, x0 // emitted to merge the inconsistent states
///     $0 -> r1
///      ...
///     $0 -> r0
///      b.cond loop
///      ...
///     $0 -> r0
///      b loop
fn merge(
    isel: *Select,
    expected_live_registers: *const LiveRegisters,
    comptime opts: struct { fill_extra: bool = false },
) !void {
    var live_reg_it = isel.live_registers.iterator();
    while (live_reg_it.next()) |live_reg_entry| {
        const ra = live_reg_entry.key;
        const actual_vi = live_reg_entry.value;
        const expected_vi = expected_live_registers.get(ra);
        switch (expected_vi) {
            else => switch (actual_vi.*) {
                _ => {},
                .allocating => unreachable,
                .free => actual_vi.* = .allocating,
            },
            .free => {},
        }
    }
    live_reg_it = isel.live_registers.iterator();
    while (live_reg_it.next()) |live_reg_entry| {
        const ra = live_reg_entry.key;
        const actual_vi = live_reg_entry.value;
        const expected_vi = expected_live_registers.get(ra);
        switch (expected_vi) {
            _ => {
                switch (actual_vi.*) {
                    _ => _ = if (opts.fill_extra) {
                        assert(try isel.fillMemory(ra));
                        assert(actual_vi.* == .free);
                    },
                    .allocating => actual_vi.* = .free,
                    .free => unreachable,
                }
                try expected_vi.liveIn(isel, ra, expected_live_registers);
            },
            .allocating => if (if (opts.fill_extra) try isel.fillMemory(ra) else try isel.fill(ra)) {
                assert(actual_vi.* == .free);
                actual_vi.* = .allocating;
            },
            .free => if (opts.fill_extra) assert(try isel.fillMemory(ra) and actual_vi.* == .free),
        }
    }
    live_reg_it = isel.live_registers.iterator();
    while (live_reg_it.next()) |live_reg_entry| {
        const ra = live_reg_entry.key;
        const actual_vi = live_reg_entry.value;
        const expected_vi = expected_live_registers.get(ra);
        switch (expected_vi) {
            _ => {
                assert(actual_vi.* == .allocating and expected_vi.register(isel) == ra);
                actual_vi.* = expected_vi;
            },
            .allocating => assert(actual_vi.* == .allocating),
            .free => if (opts.fill_extra) assert(actual_vi.* == .free),
        }
    }
}

const call = struct {
    const param_reg: Value.Index = @enumFromInt(@intFromEnum(Value.Index.allocating) - 2);
    const callee_clobbered_reg: Value.Index = @enumFromInt(@intFromEnum(Value.Index.allocating) - 1);

    const caller_saved_regs: LiveRegisters = .init(.{
        // Argument registers.
        .a0 = param_reg,
        .a1 = param_reg,
        .a2 = param_reg,
        .a3 = param_reg,
        .a4 = param_reg,
        .a5 = param_reg,
        .a6 = param_reg,
        .a7 = param_reg,

        .ra = callee_clobbered_reg,
        .sp = .free,

        // Doesn't apply, not modified.
        .zero = .free,
        .gp = .free,
        .tp = .free,

        // Saved registers.
        .s0 = .free,
        .s1 = .free,
        .s2 = .free,
        .s3 = .free,
        .s4 = .free,
        .s5 = .free,
        .s6 = .free,
        .s7 = .free,
        .s8 = .free,
        .s9 = .free,
        .s10 = .free,
        .s11 = .free,

        // Temporary registers.
        .t0 = callee_clobbered_reg,
        .t1 = callee_clobbered_reg,
        .t2 = callee_clobbered_reg,
        .t3 = callee_clobbered_reg,
        .t4 = callee_clobbered_reg,
        .t5 = callee_clobbered_reg,
        .t6 = callee_clobbered_reg,
    });

    fn prepareReturn(isel: *Select) !void {
        var live_reg_it = isel.live_registers.iterator();
        while (live_reg_it.next()) |live_reg_entry| switch (caller_saved_regs.get(live_reg_entry.key)) {
            else => unreachable,
            param_reg, callee_clobbered_reg => switch (live_reg_entry.value.*) {
                _ => {},
                .allocating => unreachable,
                .free => live_reg_entry.value.* = .allocating,
            },
            .free => {},
        };
    }

    fn returnLiveIn(isel: *Select, vi: Value.Index, reg: Register) !void {
        try vi.defLiveIn(isel, reg, &caller_saved_regs);
    }

    fn finishReturn(isel: *Select) !void {
        var live_reg_it = isel.live_registers.iterator();
        while (live_reg_it.next()) |live_reg_entry| {
            switch (live_reg_entry.value.*) {
                _ => |live_vi| switch (live_vi.size(isel)) {
                    else => unreachable,
                    1, 2, 4, 8 => {},
                    16 => return isel.fail("TODO: finishReturn 16 bytes", .{}),
                    // 16 => {
                    //     assert(try isel.fillMemory(live_reg_entry.key));
                    //     assert(live_reg_entry.value.* == .free);
                    //     switch (caller_saved_regs.get(live_reg_entry.key)) {
                    //         else => unreachable,
                    //         param_reg, callee_clobbered_reg => live_reg_entry.value.* = .allocating,
                    //         .free => {},
                    //     }
                    //     continue;
                    // },
                },
                .allocating, .free => {},
            }
            switch (caller_saved_regs.get(live_reg_entry.key)) {
                else => unreachable,
                param_reg, callee_clobbered_reg => switch (live_reg_entry.value.*) {
                    _ => {
                        assert(try isel.fill(live_reg_entry.key));
                        assert(live_reg_entry.value.* == .free);
                        live_reg_entry.value.* = .allocating;
                    },
                    .allocating => {},
                    .free => unreachable,
                },
                .free => {},
            }
        }
    }

    fn prepareCallee(isel: *Select) !void {
        var live_reg_it = isel.live_registers.iterator();
        while (live_reg_it.next()) |live_reg_entry| switch (caller_saved_regs.get(live_reg_entry.key)) {
            else => unreachable,
            param_reg => assert(live_reg_entry.value.* == .allocating),
            callee_clobbered_reg => isel.freeReg(live_reg_entry.key),
            .free => {},
        };
    }

    fn finishCallee(_: *Select) !void {}

    fn prepareParams(_: *Select) !void {}

    fn paramLiveOut(isel: *Select, vi: Value.Index, reg: Register) !void {
        isel.freeReg(reg);
        try vi.liveOut(isel, reg);
        const live_vi = isel.live_registers.getPtr(reg);
        if (live_vi.* == .free) live_vi.* = .allocating;
    }

    fn paramAddress(isel: *Select, vi: Value.Index, reg: Register) !void {
        isel.freeReg(reg);
        try vi.address(isel, 0, reg);
        const live_vi = isel.live_registers.getPtr(reg);
        if (live_vi.* == .free) live_vi.* = .allocating;
    }

    fn finishParams(isel: *Select) !void {
        var live_reg_it = isel.live_registers.iterator();
        while (live_reg_it.next()) |live_reg_entry| switch (caller_saved_regs.get(live_reg_entry.key)) {
            else => unreachable,
            param_reg => switch (live_reg_entry.value.*) {
                _ => {},
                .allocating => live_reg_entry.value.* = .free,
                .free => unreachable,
            },
            callee_clobbered_reg, .free => {},
        };
    }
};

fn emit(isel: *Select, instruction: Instruction) !void {
    wip_mir_log.debug("  | {f}", .{instruction});
    try isel.instructions.append(isel.pt.zcu.gpa, instruction);
}

fn emitLiteral(isel: *Select, bytes: []const u8) !void {
    const words: []align(1) const u32 = @ptrCast(bytes);
    const literals = try isel.literals.addManyAsSlice(isel.pt.zcu.gpa, words.len);
    switch (isel.target.cpu.arch.endian()) {
        .little => @memcpy(literals, words),
        .big => for (words, 0..) |word, word_index| {
            literals[literals.len - 1 - word_index] = @byteSwap(word);
        },
    }
}

pub fn fail(isel: *Select, comptime format: []const u8, args: anytype) error{ OutOfMemory, CodegenFail } {
    @branchHint(.cold);
    return isel.pt.zcu.codegenFail(isel.nav_index, format, args);
}

// dst = src
fn movImmediate(isel: *Select, dst_reg: Register, src_imm: u64) !void {
    if (src_imm == 0) return try isel.emit(.addi(dst_reg, .zero, 0));
    if (std.math.cast(i12, src_imm)) |casted| return try isel.emit(.addi(dst_reg, .zero, casted));
    const gpa = isel.pt.zcu.gpa;

    try isel.emit(.ld(dst_reg, dst_reg, 0));
    try isel.emit(.lui(dst_reg, 0));

    const value = try isel.pt.intValue_u64(.u64, src_imm);
    try isel.uav_relocs.append(gpa, .{
        .uav = .{ .val = value.toIntern(), .orig_ty = ZigType.ptr_usize.toIntern() },
        .reloc = .{
            .label = @intCast(isel.instructions.items.len - 1),
            .addend = 0,
        },
    });
}

fn elemPtr(
    isel: *Select,
    elem_ptr_reg: Register,
    base_reg: Register,
    op: enum { add, sub },
    elem_size: u64,
    index_vi: Value.Index,
) !void {
    const index_mat = try index_vi.matReg(isel);
    switch (elem_size) {
        0 => unreachable,
        1 => try isel.emit(switch (op) {
            .add => .add(elem_ptr_reg, base_reg, index_mat.r),
            .sub => .sub(elem_ptr_reg, base_reg, index_mat.r),
        }),
        2, 4, 8 => {
            const scratch = try isel.allocIntReg();
            defer isel.freeReg(scratch);
            try isel.emit(switch (op) {
                .add => .add(elem_ptr_reg, base_reg, scratch),
                .sub => .sub(elem_ptr_reg, base_reg, scratch),
            });
            try isel.emit(.slli(scratch, index_mat.r, @intCast(@ctz(elem_size))));
        },
        else => {
            if (!isel.target.cpu.has(.riscv, .m)) return isel.fail("TODO: elemPtr without M extension", .{});
            const scratch = try isel.allocIntReg();
            defer isel.freeReg(scratch);

            try isel.emit(.add(elem_ptr_reg, base_reg, scratch));
            try isel.emit(.mul(scratch, index_mat.r, scratch));
            try isel.movImmediate(scratch, elem_size);
        },
    }
    try index_mat.finish(isel);
}

fn storeReg(
    isel: *Select,
    src_reg: Register,
    size: u64,
    base_reg: Register,
    offset: i65,
) !void {
    if (size <= 8) if (std.math.cast(i12, offset)) |casted| {
        return try isel.emit(switch (size) {
            0 => unreachable,
            1 => .sb(src_reg, base_reg, casted),
            2 => .sh(src_reg, base_reg, casted),
            3 => {
                const high_bits = try isel.allocIntReg();
                defer isel.freeReg(high_bits);
                try isel.storeReg(high_bits, 1, base_reg, casted + 2);
                try isel.emit(.srai(high_bits, src_reg, 16));
                return try isel.storeReg(src_reg, 2, base_reg, casted);
            },
            4 => .sw(src_reg, base_reg, casted),
            8 => .sd(src_reg, base_reg, casted),
            else => return isel.fail("TODO: storeReg small size {d}, r: {t}, offset: {d}", .{ size, src_reg, offset }),
        });
    };

    switch (size) {
        0 => unreachable,
        else => return isel.fail("TODO: storeReg size {d}, r: {t}, offset: {d}", .{ size, src_reg, offset }),
    }
}

fn loadReg(
    isel: *Select,
    dst_reg: Register,
    size: u64,
    signedness: std.builtin.Signedness,
    base_reg: Register,
    offset: i65,
) !void {
    if (std.math.cast(i12, offset)) |casted| {
        const inst = switch (size) {
            0 => unreachable,
            1 => switch (signedness) {
                .unsigned => &Instruction.lbu,
                .signed => &Instruction.lb,
            },
            2 => switch (signedness) {
                .unsigned => &Instruction.lhu,
                .signed => &Instruction.lh,
            },
            3 => {
                const low_bits = try isel.allocIntReg();
                defer isel.freeReg(low_bits);
                try isel.emit(.@"or"(dst_reg, dst_reg, low_bits));
                try isel.emit(.slli(dst_reg, dst_reg, 16));
                try isel.loadReg(dst_reg, 1, signedness, base_reg, offset + 2);
                return try isel.loadReg(low_bits, 2, .unsigned, base_reg, offset);
            },
            4 => switch (signedness) {
                .unsigned => &Instruction.lwu,
                .signed => &Instruction.lw,
            },
            8 => &Instruction.ld,
            else => return isel.fail("TODO: loadReg small size {d}", .{size}),
        };
        return try isel.emit(inst(dst_reg, base_reg, casted));
    }

    switch (size) {
        0 => unreachable,
        else => return isel.fail("TODO: loadReg size {d}, r: {t}, offset: {d}", .{ size, dst_reg, offset }),
    }
}

fn cmp(
    isel: *Select,
    result_reg: Register,
    ty: ZigType,
    orig_lhs_vi: Value.Index,
    op: std.math.CompareOperator,
    orig_rhs_vi: Value.Index,
) !void {
    var lhs_vi = orig_lhs_vi;
    var rhs_vi = orig_rhs_vi;
    if (!ty.isRuntimeFloat()) {
        const int_info: std.builtin.Type.Int = if (ty.toIntern() == .bool_type)
            .{ .signedness = .unsigned, .bits = 1 }
        else if (ty.isAbiInt(isel.pt.zcu))
            ty.intInfo(isel.pt.zcu)
        else if (ty.isPtrAtRuntime(isel.pt.zcu))
            .{ .signedness = .unsigned, .bits = 64 }
        else
            return isel.fail("bad cmp_{t} {f}", .{ op, isel.fmtType(ty) });
        if (int_info.bits > 128) return isel.fail("bad cmp_{t} {f}", .{ op, isel.fmtType(ty) });
        if (int_info.bits > 64 and (op != .eq or op != .neq)) return isel.fail("TODO: cmp_{t} {f}", .{ op, isel.fmtType(ty) });

        const lhs_mat = try lhs_vi.matReg(isel);
        const rhs_mat = try rhs_vi.matReg(isel);

        const less_than = switch (int_info.signedness) {
            .unsigned => &Instruction.sltu,
            .signed => &Instruction.slt,
        };

        cmp: switch (op) {
            .eq => {
                try isel.emit(.sltiu(result_reg, result_reg, 1));
                try isel.emit(.xor(result_reg, lhs_mat.r, rhs_mat.r));
            },
            .neq => {
                try isel.emit(.sltu(result_reg, .zero, result_reg));
                try isel.emit(.xor(result_reg, lhs_mat.r, rhs_mat.r));
            },
            .gt => try isel.emit(less_than(result_reg, rhs_mat.r, lhs_mat.r)),
            .lt => try isel.emit(less_than(result_reg, lhs_mat.r, rhs_mat.r)),
            .gte => {
                try isel.emit(.xori(result_reg, result_reg, 1));
                continue :cmp .lt;
            },
            .lte => {
                try isel.emit(.xori(result_reg, result_reg, 1));
                continue :cmp .gt;
            },
        }

        try lhs_mat.finish(isel);
        try rhs_mat.finish(isel);
    } else return isel.fail("TODO: cmp floats", .{});
}

pub const Value = struct {
    refs: u32,
    flags: Flags,
    offset_from_parent: u64,
    parent_payload: Parent.Payload,
    location_payload: Location.Payload,
    parts: Value.Index,

    pub const max_parts = 16;
    pub const PartsLen = std.math.IntFittingRange(0, Value.max_parts);

    pub const Flags = packed struct(u32) {
        alignment: InternPool.Alignment,
        parent_tag: Parent.Tag,
        location_tag: Location.Tag,
        parts_len_minus_one: std.math.IntFittingRange(0, Value.max_parts - 1),
        unused: u18 = 0,
    };

    pub const Location = union(enum(u1)) {
        large: struct {
            size: u64,
        },
        small: struct {
            size: u5,
            signedness: std.builtin.Signedness,
            is_vector: bool,
            hint: Register,
            register: Register,
        },

        pub const Tag = @typeInfo(Location).@"union".tag_type.?;
        pub const Payload = @Type(.{ .@"union" = .{
            .layout = .auto,
            .tag_type = null,
            .fields = @typeInfo(Location).@"union".fields,
            .decls = &.{},
        } });
    };

    pub const Parent = union(enum(u3)) {
        unallocated: void,
        stack_slot: Indirect,
        address: Value.Index,
        value: Value.Index,
        constant: Constant,

        pub const Tag = @typeInfo(Parent).@"union".tag_type.?;
        pub const Payload = @Type(.{ .@"union" = .{
            .layout = .auto,
            .tag_type = null,
            .fields = @typeInfo(Parent).@"union".fields,
            .decls = &.{},
        } });
    };

    pub const Indirect = packed struct(u32) {
        base: Register,
        // TODO: change the type. we can only shift by 11 bits in one
        // instruction and need stack shims for larger offsets
        offset: i27,

        pub fn withOffset(ind: Indirect, offset: i27) Indirect {
            return .{
                .base = ind.base,
                .offset = ind.offset + offset,
            };
        }
    };

    const Materialize = struct {
        vi: Value.Index,
        r: Register,

        fn finish(mat: Value.Materialize, isel: *Select) error{ OutOfMemory, CodegenFail }!void {
            const live_vi = isel.live_registers.getPtr(mat.r);
            assert(live_vi.* == .allocating);
            var vi = mat.vi;
            var offset: u64 = 0;
            const size = mat.vi.size(isel);

            free: while (true) {
                if (vi.register(isel)) |reg| {
                    if (reg != mat.r) {
                        if (vi == mat.vi) break :free try isel.emit(switch (size) {
                            else => unreachable,
                            1...8 => .addi(mat.r, reg, 0),
                        });
                        return isel.fail("TODO: Materialize.finish register {}", .{vi == mat.vi});
                    }
                    mat.vi.get(isel).location_payload.small.register = mat.r;
                    live_vi.* = mat.vi;
                    return;
                }
                offset += vi.get(isel).offset_from_parent;
                switch (vi.parent(isel)) {
                    .unallocated => {
                        mat.vi.get(isel).location_payload.small.register = mat.r;
                        live_vi.* = mat.vi;
                        return;
                    },
                    .stack_slot => |stack_slot| break :free try isel.loadReg(
                        mat.r,
                        size,
                        mat.vi.signedness(isel),
                        stack_slot.base,
                        @as(i65, stack_slot.offset) + offset,
                    ),
                    .constant => |initial| {
                        const zcu = isel.pt.zcu;
                        const ip = &zcu.intern_pool;
                        var constant = initial.toIntern();
                        var constant_key = ip.indexToKey(constant);
                        while (true) {
                            constant_key: switch (constant_key) {
                                .int_type,
                                .ptr_type,
                                .array_type,
                                .vector_type,
                                .opt_type,
                                .anyframe_type,
                                .error_union_type,
                                .simple_type,
                                .struct_type,
                                .tuple_type,
                                .union_type,
                                .opaque_type,
                                .enum_type,
                                .func_type,
                                .error_set_type,
                                .inferred_error_set_type,

                                .enum_literal,
                                .empty_enum_value,
                                .memoized_call,
                                => unreachable, // not a runtime value

                                .undef => break :free try isel.emit(if (mat.r.isVector())
                                    return isel.fail("TODO: mat finish undef vector", .{})
                                else switch (size) {
                                    else => unreachable,
                                    1...8 => break :free {}, // TODO: set to undef
                                }),
                                .simple_value => |simple_value| switch (simple_value) {
                                    .undefined, .void, .null, .empty_tuple, .@"unreachable" => unreachable,
                                    .true => continue :constant_key .{ .int = .{
                                        .ty = .bool_type,
                                        .storage = .{ .u64 = 1 },
                                    } },
                                    .false => continue :constant_key .{ .int = .{
                                        .ty = .bool_type,
                                        .storage = .{ .u64 = 0 },
                                    } },
                                },
                                .int => |int| break :free switch (int.storage) {
                                    .u64 => |imm| try isel.movImmediate(switch (size) {
                                        else => unreachable,
                                        1...8 => mat.r,
                                    }, @bitCast(std.math.shr(u64, imm, 8 * offset))),
                                    .i64 => |imm| switch (size) {
                                        else => unreachable,
                                        1...8 => try isel.movImmediate(mat.r, @bitCast(std.math.shr(i64, imm, 8 * offset))),
                                    },
                                    .big_int => |big_int| {
                                        assert(size == 8);
                                        var imm: u64 = 0;
                                        const limb_bits = @bitSizeOf(std.math.big.Limb);
                                        const limbs = @divExact(64, limb_bits);
                                        var limb_index: usize = @intCast(@divExact(offset, @divExact(limb_bits, 8)) + limbs);
                                        for (0..limbs) |_| {
                                            limb_index -= 1;
                                            if (limb_index >= big_int.limbs.len) continue;
                                            if (limb_bits < 64) imm <<= limb_bits;
                                            imm |= big_int.limbs[limb_index];
                                        }
                                        if (!big_int.positive) {
                                            limb_index = @min(limb_index, big_int.limbs.len);
                                            imm = while (limb_index > 0) {
                                                limb_index -= 1;
                                                if (big_int.limbs[limb_index] != 0) break ~imm;
                                            } else -%imm;
                                        }
                                        try isel.movImmediate(mat.r, imm);
                                    },
                                    else => |t| return isel.fail("TODO: finish int mat {t}", .{t}),
                                },
                                .enum_tag => |enum_tag| continue :constant_key .{ .int = ip.indexToKey(enum_tag.int).int },
                                .ptr => |ptr| {
                                    assert(offset == 0 and size == 8);
                                    break :free switch (ptr.base_addr) {
                                        .nav => |nav| if (ZigType.fromInterned(ip.getNav(nav).typeOf(ip)).isFnOrHasRuntimeBits(zcu)) {
                                            try isel.emit(.addi(mat.r, mat.r, 0));
                                            try isel.emit(.lui(mat.r, 0));

                                            try isel.nav_relocs.append(zcu.gpa, .{
                                                .nav = nav,
                                                .reloc = .{
                                                    .label = @intCast(isel.instructions.items.len - 1),
                                                    .addend = ptr.byte_offset,
                                                },
                                            });
                                        },
                                        .uav => |uav| if (ZigType.fromInterned(ip.typeOf(uav.val)).isFnOrHasRuntimeBits(zcu)) {
                                            try isel.emit(.addi(mat.r, mat.r, 0));
                                            try isel.emit(.lui(mat.r, 0));

                                            try isel.uav_relocs.append(zcu.gpa, .{
                                                .uav = uav,
                                                .reloc = .{
                                                    .label = @intCast(isel.instructions.items.len - 1),
                                                    .addend = ptr.byte_offset,
                                                },
                                            });
                                        } else return isel.fail("TODO: mat undefined ptr uav", .{}),
                                        else => |t| return isel.fail("TODO: mat ptr: {t}", .{t}),
                                    };
                                },
                                .err => |err| continue :constant_key .{ .int = .{
                                    .ty = err.ty,
                                    .storage = .{ .u64 = ip.getErrorValueIfExists(err.name).? },
                                } },
                                .opt => |opt| {
                                    const child_ty = ip.indexToKey(opt.ty).opt_type;
                                    const child_size = ZigType.fromInterned(child_ty).abiSize(zcu);
                                    if (offset == child_size and size == 1) {
                                        offset = 0;
                                        continue :constant_key .{ .simple_value = switch (opt.val) {
                                            .none => .false,
                                            else => .true,
                                        } };
                                    }
                                    const opt_ty: ZigType = .fromInterned(opt.ty);
                                    if (offset + size <= child_size) continue :constant_key switch (opt.val) {
                                        .none => if (opt_ty.optionalReprIsPayload(zcu)) .{ .int = .{
                                            .ty = opt.ty,
                                            .storage = .{ .u64 = 0 },
                                        } } else .{ .undef = child_ty },
                                        else => |child| {
                                            constant = child;
                                            constant_key = ip.indexToKey(constant);
                                            continue :constant_key constant_key;
                                        },
                                    };
                                },
                                .error_union => |error_union| {
                                    const error_union_type = ip.indexToKey(error_union.ty).error_union_type;
                                    const error_set_ty: ZigType = .fromInterned(error_union_type.error_set_type);
                                    const payload_ty: ZigType = .fromInterned(error_union_type.payload_type);
                                    const error_set_offset = codegen.errUnionErrorOffset(payload_ty, zcu);
                                    const error_set_size = error_set_ty.abiSize(zcu);
                                    if (offset >= error_set_offset and offset + size <= error_set_offset + error_set_size) {
                                        offset -= error_set_offset;
                                        continue :constant_key switch (error_union.val) {
                                            .err_name => |err_name| .{ .err = .{
                                                .ty = error_union_type.error_set_type,
                                                .name = err_name,
                                            } },
                                            .payload => .{ .int = .{
                                                .ty = error_union_type.error_set_type,
                                                .storage = .{ .u64 = 0 },
                                            } },
                                        };
                                    }
                                    const payload_offset = codegen.errUnionPayloadOffset(payload_ty, zcu);
                                    const payload_size = payload_ty.abiSize(zcu);
                                    if (offset >= payload_offset and offset + size <= payload_offset + payload_size) {
                                        offset -= payload_offset;
                                        switch (error_union.val) {
                                            .err_name => continue :constant_key .{ .undef = error_union_type.payload_type },
                                            .payload => |payload| {
                                                constant = payload;
                                                constant_key = ip.indexToKey(constant);
                                                continue :constant_key constant_key;
                                            },
                                        }
                                    }
                                },
                                .slice => |slice| switch (offset) {
                                    0 => continue :constant_key switch (ip.indexToKey(slice.ptr)) {
                                        else => unreachable,
                                        .undef => |undef| .{ .undef = undef },
                                        .ptr => |ptr| .{ .ptr = ptr },
                                    },
                                    else => {
                                        assert(offset == @divExact(isel.target.ptrBitWidth(), 8));
                                        offset = 0;
                                        continue :constant_key .{ .int = ip.indexToKey(slice.len).int };
                                    },
                                },
                                .aggregate => |aggregate| switch (ip.indexToKey(aggregate.ty)) {
                                    else => unreachable,
                                    .tuple_type => |tuple_type| {
                                        var field_offset: u64 = 0;
                                        for (tuple_type.types.get(ip), tuple_type.values.get(ip), 0..) |field_type, field_value, field_index| {
                                            if (field_value != .none) continue;
                                            const field_ty: ZigType = .fromInterned(field_type);
                                            field_offset = field_ty.abiAlignment(zcu).forward(field_offset);
                                            const field_size = field_ty.abiSize(zcu);
                                            if (offset >= field_offset and offset + size <= field_offset + field_size) {
                                                offset -= field_offset;
                                                constant = switch (aggregate.storage) {
                                                    .bytes => unreachable,
                                                    .elems => |elems| elems[field_index],
                                                    .repeated_elem => |repeated_elem| repeated_elem,
                                                };
                                                constant_key = ip.indexToKey(constant);
                                                continue :constant_key constant_key;
                                            }
                                            field_offset += field_size;
                                        }
                                    },
                                },
                                else => |t| return isel.fail("TODO: finish constant mat {t}", .{t}),
                            }
                            var buffer: [16]u8 = @splat(0);
                            if (ZigType.fromInterned(constant_key.typeOf()).abiSize(zcu) <= buffer.len and
                                try isel.writeToMemory(.fromInterned(constant), &buffer))
                            {
                                constant_key = if (mat.r.isVector())
                                    return isel.fail("TODO: finish constant key vector", .{})
                                else
                                    .{ .int = .{
                                        .ty = .u64_type,
                                        .storage = .{ .u64 = switch (size) {
                                            else => unreachable,
                                            inline 1...8 => |ct_size| std.mem.readInt(
                                                @Type(.{ .int = .{ .signedness = .unsigned, .bits = 8 * ct_size } }),
                                                buffer[@intCast(offset)..][0..ct_size],
                                                isel.target.cpu.arch.endian(),
                                            ),
                                        } },
                                    } };
                                offset = 0;
                                continue;
                            }
                            return isel.fail("unsupported value <{f}, {f}>", .{
                                isel.fmtType(.fromInterned(constant_key.typeOf())),
                                isel.fmtConstant(.fromInterned(constant)),
                            });
                        }
                    },
                    .value => |parent_vi| vi = parent_vi,
                    else => |t| return isel.fail("TODO: finish mat {t}", .{t}),
                }
            }
            live_vi.* = .free;
        }
    };

    pub const Index = enum(u32) {
        allocating = std.math.maxInt(u32) - 1,
        free = std.math.maxInt(u32) - 0,
        _,

        fn get(vi: Value.Index, isel: *Select) *Value {
            return &isel.values.items[@intFromEnum(vi)];
        }

        fn setAlignment(vi: Value.Index, isel: *Select, new_alignment: InternPool.Alignment) void {
            vi.get(isel).flags.alignment = new_alignment;
        }

        pub fn alignment(vi: Value.Index, isel: *Select) InternPool.Alignment {
            return vi.get(isel).flags.alignment;
        }

        pub fn setParent(vi: Value.Index, isel: *Select, new_parent: Parent) void {
            const value = vi.get(isel);
            assert(value.flags.parent_tag == .unallocated);
            value.flags.parent_tag = new_parent;
            value.parent_payload = switch (new_parent) {
                .unallocated => unreachable,
                inline else => |payload, tag| @unionInit(Parent.Payload, @tagName(tag), payload),
            };
            if (value.refs > 0) switch (new_parent) {
                .unallocated => unreachable,
                .stack_slot, .constant => {},
                .address, .value => |parent_vi| _ = parent_vi.ref(isel),
            };
        }

        pub fn location(vi: Value.Index, isel: *Select) Location {
            const value = vi.get(isel);
            return switch (value.flags.location_tag) {
                inline else => |tag| @unionInit(
                    Location,
                    @tagName(tag),
                    @field(value.location_payload, @tagName(tag)),
                ),
            };
        }

        pub fn position(vi: Value.Index, isel: *Select) struct { u64, u64 } {
            return .{ vi.get(isel).offset_from_parent, vi.size(isel) };
        }

        pub fn size(vi: Value.Index, isel: *Select) u64 {
            return switch (vi.location(isel)) {
                inline else => |loc| loc.size,
            };
        }

        fn setHint(vi: Value.Index, isel: *Select, new_hint: Register) void {
            vi.get(isel).location_payload.small.hint = new_hint;
        }

        pub fn hint(vi: Value.Index, isel: *Select) ?Register {
            return switch (vi.location(isel)) {
                .large => null,
                .small => |loc| switch (loc.hint) {
                    .zero => null,
                    else => |hint_reg| hint_reg,
                },
            };
        }

        pub fn changeStackSlot(vi: Value.Index, isel: *Select, new_stack_slot: Indirect) void {
            const value = vi.get(isel);
            assert(value.flags.parent_tag == .stack_slot);
            value.flags.parent_tag = .unallocated;
            vi.setParent(isel, .{ .stack_slot = new_stack_slot });
        }

        pub fn parent(vi: Value.Index, isel: *Select) Parent {
            const value = vi.get(isel);
            return switch (value.flags.parent_tag) {
                inline else => |tag| @unionInit(
                    Parent,
                    @tagName(tag),
                    @field(value.parent_payload, @tagName(tag)),
                ),
            };
        }

        pub fn valueParent(initial_vi: Value.Index, isel: *Select) struct { u64, Value.Index } {
            var offset: u64 = 0;
            var vi = initial_vi;
            parent: switch (vi.parent(isel)) {
                else => return .{ offset, vi },
                .value => |parent_vi| {
                    offset += vi.position(isel)[0];
                    vi = parent_vi;
                    continue :parent parent_vi.parent(isel);
                },
            }
        }

        fn partAtOffset(vi: Value.Index, isel: *Select, offset: u64) Value.Index {
            const SearchPartIndex = std.math.IntFittingRange(0, Value.max_parts * 2 - 1);
            const value = vi.get(isel);
            var last: SearchPartIndex = value.flags.parts_len_minus_one;
            if (last == 0) return vi;
            var first: SearchPartIndex = 0;
            last += 1;
            while (true) {
                const mid = (first + last) / 2;
                const mid_vi: Value.Index = @enumFromInt(@intFromEnum(value.parts) + mid);
                if (mid == first) return mid_vi;
                if (offset < mid_vi.get(isel).offset_from_parent) last = mid else first = mid;
            }
        }

        fn field(
            vi: Value.Index,
            ty: ZigType,
            field_offset: u64,
            field_size: u64,
        ) Value.FieldPartIterator {
            assert(field_size > 0);
            return .{
                .vi = vi,
                .ty = ty,
                .field_offset = field_offset,
                .field_size = field_size,
                .next_offset = 0,
            };
        }

        fn ref(initial_vi: Value.Index, isel: *Select) Value.Index {
            var vi = initial_vi;
            while (true) {
                const refs = &vi.get(isel).refs;
                refs.* += 1;
                if (refs.* > 1) return initial_vi;
                switch (vi.parent(isel)) {
                    .unallocated, .stack_slot, .constant => {},
                    .address, .value => |parent_vi| {
                        vi = parent_vi;
                        continue;
                    },
                }
                return initial_vi;
            }
        }

        pub fn deref(initial_vi: Value.Index, isel: *Select) void {
            var vi = initial_vi;
            while (true) {
                const refs = &vi.get(isel).refs;
                refs.* -= 1;
                if (refs.* > 0) return;
                switch (vi.parent(isel)) {
                    .unallocated, .constant => {},
                    .stack_slot => {
                        // reuse stack slot
                    },
                    .address, .value => |parent_vi| {
                        vi = parent_vi;
                        continue;
                    },
                }
                return;
            }
        }

        fn setSignedness(vi: Value.Index, isel: *Select, new_signedness: std.builtin.Signedness) void {
            const value = vi.get(isel);
            assert(value.location_payload.small.size <= 4);
            value.location_payload.small.signedness = new_signedness;
        }

        pub fn signedness(vi: Value.Index, isel: *Select) std.builtin.Signedness {
            const value = vi.get(isel);
            return switch (value.flags.location_tag) {
                .large => .unsigned,
                .small => value.location_payload.small.signedness,
            };
        }

        fn setIsVector(vi: Value.Index, isel: *Select) void {
            const is_vector = &vi.get(isel).location_payload.small.is_vector;
            assert(!is_vector.*);
            is_vector.* = true;
        }

        pub fn isVector(vi: Value.Index, isel: *Select) bool {
            const value = vi.get(isel);
            return switch (value.flags.location_tag) {
                .large => false,
                .small => value.location_payload.small.is_vector,
            };
        }

        pub fn register(vi: Value.Index, isel: *Select) ?Register {
            return switch (vi.location(isel)) {
                .large => null,
                .small => |loc| switch (loc.register) {
                    .zero => null,
                    else => |reg| reg,
                },
            };
        }

        pub fn isUsed(vi: Value.Index, isel: *Select) bool {
            return vi.valueParent(isel)[1].parent(isel) != .unallocated or vi.hasRegisterRecursive(isel);
        }

        fn hasRegisterRecursive(vi: Value.Index, isel: *Select) bool {
            if (vi.register(isel)) |_| return true;
            var part_it = vi.parts(isel);
            if (part_it.only() == null) while (part_it.next()) |part_vi| if (part_vi.hasRegisterRecursive(isel)) return true;
            return false;
        }

        fn setParts(vi: Value.Index, isel: *Select, parts_len: Value.PartsLen) void {
            assert(parts_len > 1);
            const value = vi.get(isel);
            assert(value.flags.parts_len_minus_one == 0);
            value.parts = @enumFromInt(isel.values.items.len);
            value.flags.parts_len_minus_one = @intCast(parts_len - 1);
        }

        fn addPart(vi: Value.Index, isel: *Select, part_offset: u64, part_size: u64) Value.Index {
            const part_vi = isel.initValueAdvanced(vi.alignment(isel), part_offset, part_size);
            tracking_log.debug("${d} <- ${d}[{d}]", .{
                @intFromEnum(part_vi),
                @intFromEnum(vi),
                part_offset,
            });
            part_vi.setParent(isel, .{ .value = vi });
            return part_vi;
        }

        pub fn parts(vi: Value.Index, isel: *Select) Value.PartIterator {
            const value = vi.get(isel);
            return switch (value.flags.parts_len_minus_one) {
                0 => .initOne(vi),
                else => |parts_len_minus_one| .{
                    .vi = value.parts,
                    .remaining = @as(Value.PartsLen, parts_len_minus_one) + 1,
                },
            };
        }

        pub fn liveIn(
            vi: Value.Index,
            isel: *Select,
            src_reg: Register,
            expected_live_registers: *const LiveRegisters,
        ) !void {
            const src_live_vi = isel.live_registers.getPtr(src_reg);
            if (vi.register(isel)) |dst_reg| {
                const dst_live_vi = isel.live_registers.getPtr(dst_reg);
                assert(dst_live_vi.* == vi);
                if (dst_reg == src_reg) {
                    src_live_vi.* = .allocating;
                    return;
                }
                dst_live_vi.* = .allocating;
                if (try isel.fill(src_reg)) {
                    assert(src_live_vi.* == .free);
                    src_live_vi.* = .allocating;
                }
                assert(src_live_vi.* == .allocating);
                try isel.emit(switch (dst_reg.isVector()) {
                    false => switch (src_reg.isVector()) {
                        false => switch (vi.size(isel)) {
                            else => unreachable,
                            1...8 => .addi(dst_reg, src_reg, 0),
                        },
                        true => return isel.fail("TODO: liveIn vector register to non", .{}),
                    },
                    true => return isel.fail("TODO: liveIn vector register", .{}),
                });
                assert(dst_live_vi.* == .allocating);
                dst_live_vi.* = switch (expected_live_registers.get(dst_reg)) {
                    _ => .allocating,
                    .allocating => .allocating,
                    .free => .free,
                };
            } else if (try isel.fill(src_reg)) {
                assert(src_live_vi.* == .free);
                src_live_vi.* = .allocating;
            }
            assert(src_live_vi.* == .allocating);
            vi.get(isel).location_payload.small.register = src_reg;
        }

        pub fn defLiveIn(
            vi: Value.Index,
            isel: *Select,
            src_reg: Register,
            expected_live_registers: *const LiveRegisters,
        ) !void {
            try vi.liveIn(isel, src_reg, expected_live_registers);
            const offset_from_parent, const parent_vi = vi.valueParent(isel);
            switch (parent_vi.parent(isel)) {
                .unallocated => {},
                .stack_slot => |stack_slot| if (stack_slot.base != Register.fp) try isel.storeReg(
                    src_reg,
                    vi.size(isel),
                    stack_slot.base,
                    @as(i65, stack_slot.offset) + offset_from_parent,
                ),
                else => unreachable,
            }
            try vi.spillReg(isel, src_reg, 0, expected_live_registers);
        }

        fn spillReg(
            vi: Value.Index,
            isel: *Select,
            src_reg: Register,
            start_offset: u64,
            expected_live_registers: *const LiveRegisters,
        ) !void {
            _ = expected_live_registers;

            assert(isel.live_registers.get(src_reg) == .allocating);
            var part_it = vi.parts(isel);
            if (part_it.only()) |part_vi| {
                const dst_reg = part_vi.register(isel) orelse return;
                if (dst_reg == src_reg) return;

                return isel.fail("TOOD: spillReg only one {}", .{part_vi});
            }

            return isel.fail("TODO: spillReg {t} -> ({})", .{ src_reg, start_offset });
        }

        fn defAddr(
            def_vi: Value.Index,
            isel: *Select,
            root_ty: ZigType,
            opts: struct {
                root_vi: Value.Index = .free,
                wrap: ?std.builtin.Type.Int = null,
                expected_live_registers: *const LiveRegisters = &.initFill(.free),
            },
        ) !?void {
            if (!def_vi.isUsed(isel)) return null;
            const offset_from_parent: i65, const parent_vi = def_vi.valueParent(isel);
            const stack_slot, const allocated = switch (parent_vi.parent(isel)) {
                .unallocated => .{ parent_vi.allocStackSlot(isel), true },
                .stack_slot => |stack_slot| .{ stack_slot, false },
                else => unreachable,
            };
            _ = try def_vi.load(isel, root_ty, stack_slot.base, .{
                .root_vi = opts.root_vi,
                .offset = @intCast(stack_slot.offset + offset_from_parent),
                .split = false,
                .wrap = opts.wrap,
                .expected_live_registers = opts.expected_live_registers,
            });
            if (allocated) parent_vi.setParent(isel, .{ .stack_slot = stack_slot });
        }

        fn defReg(def_vi: Value.Index, isel: *Select) !?Register {
            var vi = def_vi;
            var offset: i65 = 0;
            var def_reg: ?Register = null;
            while (true) {
                if (vi.register(isel)) |r| {
                    vi.get(isel).location_payload.small.register = .zero;
                    const live_vi = isel.live_registers.getPtr(r);
                    assert(live_vi.* == vi);
                    if (def_reg == null and vi != def_vi) {
                        return isel.fail("TODO: defReg create new", .{});
                    }
                    live_vi.* = .free;
                    def_reg = r;
                }
                offset += vi.get(isel).offset_from_parent;
                switch (vi.parent(isel)) {
                    else => unreachable,
                    .unallocated => return def_reg,
                    .stack_slot => |stack_slot| {
                        offset += stack_slot.offset;
                        const def_is_vector = def_vi.isVector(isel);
                        const ra = def_reg orelse if (def_is_vector)
                            return isel.fail("TODO: defReg vector register", .{})
                        else
                            try isel.allocIntReg();
                        defer if (def_reg == null) isel.freeReg(ra);
                        try isel.storeReg(ra, def_vi.size(isel), stack_slot.base, offset);
                        return ra;
                    },
                    .value => |parent_vi| vi = parent_vi,
                }
            }
        }

        pub fn defUndef(def_vi: Value.Index, isel: *Select, root_ty: ZigType, opts: struct {
            root_vi: Value.Index = .free,
            offset: u64 = 0,
            split: bool = true,
        }) !void {
            const root_vi = switch (opts.root_vi) {
                _ => |root_vi| root_vi,
                .allocating => unreachable,
                .free => def_vi,
            };
            var part_it = def_vi.parts(isel);
            if (part_it.only()) |part_vi| only: {
                const part_size = part_vi.size(isel);
                const part_is_vector = part_vi.isVector(isel);
                if (part_is_vector) return isel.fail("TODO: defUndef vector", .{});
                if (part_size > 8) {
                    if (!opts.split) return;
                    var subpart_it = root_vi.field(root_ty, opts.offset, part_size - 1);
                    _ = try subpart_it.next(isel);
                    part_it = def_vi.parts(isel);
                    assert(part_it.only() == null);
                    break :only;
                }
                return if (try part_vi.defReg(isel)) |_| try isel.emit(switch (part_size) {
                    else => |t| return isel.fail("TODO: defUndef reg {d}", .{t}),
                });
            }
            while (part_it.next()) |part_vi| try part_vi.defUndef(
                isel,
                root_ty,
                .{ .root_vi = root_vi },
            );
        }

        fn matReg(vi: Value.Index, isel: *Select) !Value.Materialize {
            const mat_reg = mat_reg: {
                if (vi.register(isel)) |mat_reg| {
                    vi.get(isel).location_payload.small.register = .zero;
                    const live_vi = isel.live_registers.getPtr(mat_reg);
                    assert(live_vi.* == vi);
                    live_vi.* = .allocating;
                    break :mat_reg mat_reg;
                }
                if (vi.hint(isel)) |hint_ra| {
                    const live_vi = isel.live_registers.getPtr(hint_ra);
                    if (live_vi.* == .free) {
                        live_vi.* = .allocating;
                        isel.saved_registers.insert(hint_ra);
                        break :mat_reg hint_ra;
                    }
                }
                break :mat_reg if (vi.isVector(isel))
                    return isel.fail("TODO: matReg vectors", .{})
                else
                    try isel.allocIntReg();
            };
            assert(isel.live_registers.get(mat_reg) == .allocating);
            return .{ .vi = vi, .r = mat_reg };
        }

        fn move(dst_vi: Value.Index, isel: *Select, src_ref: Air.Inst.Ref) !void {
            try dst_vi.copy(
                isel,
                isel.air.typeOf(src_ref, &isel.pt.zcu.intern_pool),
                try isel.use(src_ref),
            );
        }

        fn copy(dst_vi: Value.Index, isel: *Select, ty: ZigType, src_vi: Value.Index) !void {
            try dst_vi.copyAdvanced(isel, src_vi, .{
                .ty = ty,
                .dst_vi = dst_vi,
                .dst_offset = 0,
                .src_vi = src_vi,
                .src_offset = 0,
            });
        }

        fn copyAdvanced(dst_vi: Value.Index, isel: *Select, src_vi: Value.Index, root: struct {
            ty: ZigType,
            dst_vi: Value.Index,
            dst_offset: u64,
            src_vi: Value.Index,
            src_offset: u64,
        }) !void {
            if (dst_vi == src_vi) return;
            var dst_part_it = dst_vi.parts(isel);
            if (dst_part_it.only()) |dst_part_vi| {
                var src_part_it = src_vi.parts(isel);
                if (src_part_it.only()) |src_part_vi| only: {
                    const src_part_size = src_part_vi.size(isel);
                    if (src_part_vi.isVector(isel)) return isel.fail("TODO: copyAdvanced vector {f}", .{isel.fmtType(root.ty)});
                    if (src_part_size > 8) {
                        var subpart_it = root.src_vi.field(root.ty, root.src_offset, src_part_size - 1);
                        _ = try subpart_it.next(isel);
                        src_part_it = src_vi.parts(isel);
                        assert(src_part_it.only() == null);
                        break :only;
                    }
                    return src_part_vi.liveOut(isel, try dst_part_vi.defReg(isel) orelse return);
                }
                while (src_part_it.next()) |src_part_vi| {
                    const src_part_offset, const src_part_size = src_part_vi.position(isel);
                    var dst_field_it = root.dst_vi.field(root.ty, root.dst_offset + src_part_offset, src_part_size);
                    const dst_field_vi = try dst_field_it.only(isel);
                    try dst_field_vi.?.copyAdvanced(isel, src_part_vi, .{
                        .ty = root.ty,
                        .dst_vi = root.dst_vi,
                        .dst_offset = root.dst_offset + src_part_offset,
                        .src_vi = root.src_vi,
                        .src_offset = root.src_offset + src_part_offset,
                    });
                }
            } else while (dst_part_it.next()) |dst_part_vi| {
                const dst_part_offset, const dst_part_size = dst_part_vi.position(isel);
                var src_field_it = root.src_vi.field(root.ty, root.src_offset + dst_part_offset, dst_part_size);
                const src_part_vi = try src_field_it.only(isel);
                try dst_part_vi.copyAdvanced(isel, src_part_vi.?, .{
                    .ty = root.ty,
                    .dst_vi = root.dst_vi,
                    .dst_offset = root.dst_offset + dst_part_offset,
                    .src_vi = root.src_vi,
                    .src_offset = root.src_offset + dst_part_offset,
                });
            }
        }

        fn liveOut(vi: Value.Index, isel: *Select, r: Register) !void {
            assert(try isel.fill(r));
            const live_vi = isel.live_registers.getPtr(r);
            assert(live_vi.* == .free);
            live_vi.* = .allocating;
            try Value.Materialize.finish(.{ .vi = vi, .r = r }, isel);
        }

        fn allocStackSlot(vi: Value.Index, isel: *Select) Value.Indirect {
            const offset = vi.alignment(isel).forward(isel.stack_size);
            isel.stack_size = @intCast(offset + vi.size(isel));
            tracking_log.debug("${d} -> [sp, #0x{x}]", .{ @intFromEnum(vi), @abs(offset) });
            return .{
                .base = .sp,
                .offset = @intCast(offset),
            };
        }

        fn address(initial_vi: Value.Index, isel: *Select, initial_offset: u64, ptr_reg: Register) !void {
            var vi = initial_vi;
            var offset: i65 = vi.get(isel).offset_from_parent + initial_offset;
            parent: switch (vi.parent(isel)) {
                .unallocated => {
                    const stack_slot = vi.allocStackSlot(isel);
                    vi.setParent(isel, .{ .stack_slot = stack_slot });
                    continue :parent .{ .stack_slot = stack_slot };
                },
                .stack_slot => |stack_slot| {
                    offset += stack_slot.offset;
                    const lo12: i12 = std.math.cast(i12, offset) orelse
                        return isel.fail("TODO: address larger stack slot", .{});
                    try isel.emit(.addi(ptr_reg, stack_slot.base, lo12));
                },
                .value => |parent_vi| {
                    vi = parent_vi;
                    offset += vi.get(isel).offset_from_parent;
                    continue :parent vi.parent(isel);
                },
                .constant => |constant| {
                    const pt = isel.pt;
                    const zcu = pt.zcu;
                    try isel.emit(.addi(ptr_reg, ptr_reg, 0));
                    try isel.emit(.lui(ptr_reg, 0));
                    try isel.uav_relocs.append(zcu.gpa, .{
                        .uav = .{
                            .val = constant.toIntern(),
                            .orig_ty = (try pt.singleConstPtrType(constant.typeOf(zcu))).toIntern(),
                        },
                        .reloc = .{
                            .label = @intCast(isel.instructions.items.len - 1),
                            .addend = @intCast(offset),
                        },
                    });
                },
                .address => |address_vi| try address_vi.liveOut(isel, ptr_reg),
            }
        }

        const MemoryAccessOptions = struct {
            root_vi: Value.Index = .free,
            offset: u64 = 0,
            @"volatile": bool = false,
            split: bool = true,
            wrap: ?std.builtin.Type.Int = null,
            expected_live_registers: *const LiveRegisters = &.initFill(.free),
        };

        fn load(
            vi: Value.Index,
            isel: *Select,
            root_ty: ZigType,
            base_reg: Register,
            opts: MemoryAccessOptions,
        ) !bool {
            const root_vi = switch (opts.root_vi) {
                _ => |root_vi| root_vi,
                .allocating => unreachable,
                .free => vi,
            };
            var part_it = vi.parts(isel);
            if (part_it.only()) |part_vi| only: {
                const part_size = part_vi.size(isel);
                const part_is_vector = part_vi.isVector(isel);
                if (part_is_vector) return isel.fail("TODO: load vector", .{});
                if (part_size > 8) {
                    if (!opts.split) return false;
                    var subpart_it = root_vi.field(root_ty, opts.offset, part_size - 1);
                    _ = try subpart_it.next(isel);
                    part_it = vi.parts(isel);
                    assert(part_it.only() == null);
                    break :only;
                }
                const part_reg = if (try part_vi.defReg(isel)) |part_reg|
                    part_reg
                else if (opts.@"volatile")
                    .zero
                else
                    return false;
                if (part_reg != .zero) {
                    const live_vi = isel.live_registers.getPtr(part_reg);
                    assert(live_vi.* == .free);
                    live_vi.* = .allocating;
                }
                if (opts.wrap != null) return isel.fail("TODO: load wrap", .{});

                try isel.loadReg(part_reg, part_size, part_vi.signedness(isel), base_reg, opts.offset);
                if (part_reg != .zero) {
                    const live_vi = isel.live_registers.getPtr(part_reg);
                    assert(live_vi.* == .allocating);
                    switch (opts.expected_live_registers.get(part_reg)) {
                        _ => {},
                        .allocating => unreachable,
                        .free => live_vi.* = .free,
                    }
                }
                return true;
            }
            var used = false;
            while (part_it.next()) |part_vi| used |= try part_vi.load(isel, root_ty, base_reg, .{
                .root_vi = root_vi,
                .offset = opts.offset + part_vi.get(isel).offset_from_parent,
                .@"volatile" = opts.@"volatile",
                .split = opts.split,
                .wrap = switch (part_it.remaining) {
                    else => null,
                    0 => if (opts.wrap) |wrap| .{
                        .signedness = wrap.signedness,
                        .bits = @intCast(wrap.bits - 8 * part_vi.position(isel)[0]),
                    } else null,
                },
                .expected_live_registers = opts.expected_live_registers,
            });
            return used;
        }

        fn store(
            vi: Value.Index,
            isel: *Select,
            root_ty: ZigType,
            base_reg: Register,
            opts: MemoryAccessOptions,
        ) !void {
            const root_vi = switch (opts.root_vi) {
                _ => |root_vi| root_vi,
                .allocating => unreachable,
                .free => vi,
            };
            var part_it = vi.parts(isel);
            if (part_it.only()) |part_vi| only: {
                const part_size = part_vi.size(isel);
                const part_is_vector = part_vi.isVector(isel);
                if (part_is_vector) return isel.fail("TODO: vectors", .{});
                if (part_size > 8) {
                    if (!opts.split) return;
                    var subpart_it = root_vi.field(root_ty, opts.offset, part_size - 1);
                    _ = try subpart_it.next(isel);
                    part_it = vi.parts(isel);
                    assert(part_it.only() == null);
                    break :only;
                }
                const part_mat = try part_vi.matReg(isel);
                try isel.storeReg(part_mat.r, part_size, base_reg, opts.offset);
                return part_mat.finish(isel);
            }
            while (part_it.next()) |part_vi| try part_vi.store(isel, root_ty, base_reg, .{
                .root_vi = root_vi,
                .offset = opts.offset + part_vi.get(isel).offset_from_parent,
                .@"volatile" = opts.@"volatile",
                .split = opts.split,
                .wrap = switch (part_it.remaining) {
                    else => null,
                    0 => if (opts.wrap) |wrap| .{
                        .signedness = wrap.signedness,
                        .bits = @intCast(wrap.bits - 8 * part_vi.position(isel)[0]),
                    } else null,
                },
                .expected_live_registers = opts.expected_live_registers,
            });
        }

        const AddOrSubtractOptions = struct {
            overflow: Overflow,

            const Overflow = union(enum) {
                @"unreachable",
                wrap,
                reg: Register,

                // fn defCond(overflow: Overflow, isel: *Select, cond: codegen.aarch64.encoding.ConditionCode) !void {
                //     switch (overflow) {
                //         .@"unreachable" => unreachable,
                //         .panic => |panic_id| {
                //             const skip_label = isel.instructions.items.len;
                //             try isel.emitPanic(panic_id);
                //             try isel.emit(.@"b."(
                //                 cond.invert(),
                //                 @intCast((isel.instructions.items.len + 1 - skip_label) << 2),
                //             ));
                //         },
                //         .wrap => {},
                //         .ra => |overflow_ra| try isel.emit(.csinc(overflow_ra.w(), .wzr, .wzr, cond.invert())),
                //     }
                // }
            };
        };

        fn addOrSubtract(
            res_vi: Value.Index,
            isel: *Select,
            ty: ZigType,
            lhs_vi: Value.Index,
            op: enum { add, sub },
            rhs_vi: Value.Index,
            opts: AddOrSubtractOptions,
        ) !void {
            const zcu = isel.pt.zcu;
            if (!ty.isAbiInt(zcu)) return isel.fail("bad {t} {f}", .{ op, isel.fmtType(ty) });
            const int_info = ty.intInfo(zcu);
            if (int_info.bits > 64) return isel.fail("too big {t} {f}", .{ op, isel.fmtType(ty) });

            const need_wrap = switch (opts.overflow) {
                .@"unreachable" => false,
                .wrap, .reg => true,
            };
            const need_carry = switch (opts.overflow) {
                .@"unreachable", .wrap => false,
                .reg => true,
            };

            const result_reg = (try res_vi.defReg(isel)).?;

            const lhs_mat = try lhs_vi.matReg(isel);
            const rhs_mat = try rhs_vi.matReg(isel);

            if (need_carry) {
                return isel.fail("TODO: carry", .{});
            }

            if (need_wrap and int_info.bits != 64) {
                // mask the result with (1 << N) - 1
                try isel.emit(.@"and"(result_reg, result_reg, .t1));
                try isel.emit(.srli(.t1, .t1, @intCast(64 - int_info.bits)));
                try isel.emit(.addi(.t1, .zero, -1));
            }

            try isel.emit(switch (op) {
                .add => .add(result_reg, lhs_mat.r, rhs_mat.r),
                .sub => .sub(result_reg, lhs_mat.r, rhs_mat.r),
            });

            try rhs_mat.finish(isel);
            try lhs_mat.finish(isel);
        }
    };

    pub const PartIterator = struct {
        vi: Value.Index,
        remaining: Value.PartsLen,

        fn initOne(vi: Value.Index) PartIterator {
            return .{ .vi = vi, .remaining = 1 };
        }

        pub fn next(it: *PartIterator) ?Value.Index {
            if (it.remaining == 0) return null;
            it.remaining -= 1;
            defer it.vi = @enumFromInt(@intFromEnum(it.vi) + 1);
            return it.vi;
        }

        pub fn peek(it: PartIterator) ?Value.Index {
            var it_mut = it;
            return it_mut.next();
        }

        pub fn only(it: PartIterator) ?Value.Index {
            return if (it.remaining == 1) it.vi else null;
        }
    };

    const FieldPartIterator = struct {
        vi: Value.Index,
        ty: ZigType,
        field_offset: u64,
        field_size: u64,
        next_offset: u64,

        fn next(it: *FieldPartIterator, isel: *Select) !?struct { offset: u64, vi: Value.Index } {
            const next_offset = it.next_offset;
            const next_part_size = it.field_size - next_offset;
            if (next_part_size == 0) return null;
            var next_part_offset = it.field_offset + next_offset;

            const zcu = isel.pt.zcu;
            const ip = &zcu.intern_pool;
            var vi = it.vi;
            var ty = it.ty;

            var ty_size = vi.size(isel);
            _ = &ty_size;

            assert(ty_size == ty.abiSize(zcu));
            var offset: u64 = 0;
            var size = ty_size;
            assert(next_part_offset + next_part_size <= size);
            while (next_part_offset > 0 or next_part_size < size) {
                const part_vi = vi.partAtOffset(isel, next_part_offset);
                if (part_vi != vi) {
                    vi = part_vi;
                    const part_offset, size = part_vi.position(isel);
                    assert(part_offset <= next_part_offset and part_offset + size > next_part_offset);
                    offset += part_offset;
                    next_part_offset -= part_offset;
                    continue;
                }
                try isel.values.ensureUnusedCapacity(zcu.gpa, Value.max_parts);
                type_key: switch (ip.indexToKey(ty.toIntern())) {
                    .int_type => |int_type| switch (int_type.bits) {
                        0 => unreachable,
                        1...64 => unreachable,
                        65...256 => |bits| if (offset == 0 and size == ty_size) {
                            const parts_len = std.math.divCeil(u16, bits, 64) catch unreachable;
                            vi.setParts(isel, @intCast(parts_len));
                            for (0..parts_len) |part_index| _ = vi.addPart(isel, 8 * part_index, 8);
                        },
                        else => return isel.fail("Value.FieldPartIterator.next({f})", .{isel.fmtType(ty)}),
                    },
                    .ptr_type => |ptr_type| switch (ptr_type.flags.size) {
                        .one, .many, .c => unreachable,
                        .slice => if (offset == 0 and size == ty_size) {
                            vi.setParts(isel, 2);
                            _ = vi.addPart(isel, 0, 8);
                            _ = vi.addPart(isel, 8, 8);
                        } else unreachable,
                    },
                    .struct_type => {
                        const loaded_struct = ip.loadStructType(ty.toIntern());
                        switch (loaded_struct.layout) {
                            .auto, .@"extern" => {},
                            .@"packed" => continue :type_key .{
                                .int_type = ip.indexToKey(loaded_struct.backingIntTypeUnordered(ip)).int_type,
                            },
                        }
                        const min_part_log2_stride: u5 = if (size > 16) 4 else if (size > 8) 3 else 0;
                        if (loaded_struct.field_types.len > Value.max_parts and
                            (std.math.divCeil(u64, size, @as(u64, 1) << min_part_log2_stride) catch unreachable) > Value.max_parts)
                            return isel.fail("Value.FieldPartIterator.next({f})", .{isel.fmtType(ty)});
                        const alignment = vi.alignment(isel);
                        const Part = struct { offset: u64, size: u64, signedness: ?std.builtin.Signedness, is_vector: bool };
                        var parts: [Value.max_parts]Part = undefined;
                        var parts_len: Value.PartsLen = 0;
                        var field_end: u64 = 0;
                        var field_it = loaded_struct.iterateRuntimeOrder(ip);
                        while (field_it.next()) |field_index| {
                            const field_ty: ZigType = .fromInterned(loaded_struct.field_types.get(ip)[field_index]);
                            const field_begin = switch (loaded_struct.fieldAlign(ip, field_index)) {
                                .none => field_ty.abiAlignment(zcu),
                                else => |field_align| field_align,
                            }.forward(field_end);
                            if (field_begin >= offset + size) break;
                            const field_size = field_ty.abiSize(zcu);
                            field_end = field_begin + field_size;
                            if (field_end <= offset) continue;
                            if (offset >= field_begin and offset + size <= field_begin + field_size) {
                                ty = field_ty;
                                ty_size = field_size;
                                offset -= field_begin;
                                continue :type_key ip.indexToKey(field_ty.toIntern());
                            }
                            const field_signedness = if (field_ty.isAbiInt(zcu)) field_signedness: {
                                const field_int_info = field_ty.intInfo(zcu);
                                break :field_signedness if (field_int_info.bits <= 16) field_int_info.signedness else null;
                            } else null;
                            const field_is_vector = field_size <= 16 and
                                CallAbiIterator.homogeneousAggregateBaseType(zcu, field_ty.toIntern()) != null;
                            if (parts_len > 0) combine: {
                                const prev_part = &parts[parts_len - 1];
                                const combined_size = field_end - prev_part.offset;
                                if (combined_size > @as(u64, 1) << @min(
                                    min_part_log2_stride,
                                    alignment.toLog2Units(),
                                    @ctz(prev_part.offset),
                                )) break :combine;
                                prev_part.size = combined_size;
                                prev_part.signedness = null;
                                prev_part.is_vector &= field_is_vector;
                                continue;
                            }
                            parts[parts_len] = .{
                                .offset = field_begin,
                                .size = field_size,
                                .signedness = field_signedness,
                                .is_vector = field_is_vector,
                            };
                            parts_len += 1;
                        }
                        vi.setParts(isel, parts_len);
                        for (parts[0..parts_len]) |part| {
                            const subpart_vi = vi.addPart(isel, part.offset - offset, part.size);
                            if (part.signedness) |signedness| subpart_vi.setSignedness(isel, signedness);
                            if (part.is_vector) subpart_vi.setIsVector(isel);
                        }
                    },
                    .tuple_type => |tuple_type| {
                        const min_part_log2_stride: u5 = if (size > 16) 4 else if (size > 8) 3 else 0;
                        if (tuple_type.types.len > Value.max_parts and
                            (std.math.divCeil(u64, size, @as(u64, 1) << min_part_log2_stride) catch unreachable) > Value.max_parts)
                            return isel.fail("Value.FieldPartIterator.next({f})", .{isel.fmtType(ty)});
                        const alignment = vi.alignment(isel);
                        const Part = struct { offset: u64, size: u64, is_vector: bool };
                        var parts: [Value.max_parts]Part = undefined;
                        var parts_len: Value.PartsLen = 0;
                        var field_end: u64 = 0;
                        for (tuple_type.types.get(ip), tuple_type.values.get(ip)) |field_type, field_value| {
                            if (field_value != .none) continue;
                            const field_ty: ZigType = .fromInterned(field_type);
                            const field_begin = field_ty.abiAlignment(zcu).forward(field_end);
                            if (field_begin >= offset + size) break;
                            const field_size = field_ty.abiSize(zcu);
                            if (field_size == 0) continue;
                            field_end = field_begin + field_size;
                            if (field_end <= offset) continue;
                            if (offset >= field_begin and offset + size <= field_begin + field_size) {
                                ty = field_ty;
                                ty_size = field_size;
                                offset -= field_begin;
                                continue :type_key ip.indexToKey(field_ty.toIntern());
                            }
                            const field_is_vector = field_size <= 16 and
                                CallAbiIterator.homogeneousAggregateBaseType(zcu, field_ty.toIntern()) != null;
                            if (parts_len > 0) combine: {
                                const prev_part = &parts[parts_len - 1];
                                const combined_size = field_end - prev_part.offset;
                                if (combined_size > @as(u64, 1) << @min(
                                    min_part_log2_stride,
                                    alignment.toLog2Units(),
                                    @ctz(prev_part.offset),
                                )) break :combine;
                                prev_part.size = combined_size;
                                prev_part.is_vector &= field_is_vector;
                                continue;
                            }
                            parts[parts_len] = .{ .offset = field_begin, .size = field_size, .is_vector = field_is_vector };
                            parts_len += 1;
                        }
                        vi.setParts(isel, parts_len);
                        for (parts[0..parts_len]) |part| {
                            const subpart_vi = vi.addPart(isel, part.offset - offset, part.size);
                            if (part.is_vector) subpart_vi.setIsVector(isel);
                        }
                    },
                    else => return isel.fail("Value.FieldPartIterator.next({f})", .{isel.fmtType(ty)}),
                }
            }
            it.next_offset = next_offset + size;
            return .{ .offset = next_part_offset - next_offset, .vi = vi };
        }

        fn only(it: *FieldPartIterator, isel: *Select) !?Value.Index {
            const part = try it.next(isel);
            assert(part.?.offset == 0);
            return if (try it.next(isel)) |_| null else part.?.vi;
        }
    };
};

const Select = @This();
const std = @import("std");

const assert = std.debug.assert;

const Zcu = @import("../../Zcu.zig");
const Air = @import("../../Air.zig");
const InternPool = @import("../../InternPool.zig");
const ZigType = @import("../../Type.zig");
const Constant = @import("../../Value.zig");
const Package = @import("../../Package.zig");
const codegen = @import("../../codegen.zig");

const Assemble = @import("Assemble.zig");
const encoding = @import("encoding.zig");
const Mir = @import("Mir.zig");

const Register = encoding.Register;
const Instruction = encoding.Instruction;

const tracking_log = std.log.scoped(.tracking);
const wip_mir_log = std.log.scoped(.@"wip-mir");
