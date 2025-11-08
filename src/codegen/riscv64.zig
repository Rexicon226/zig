const Mir = @import("riscv64_2/Mir.zig");
const Select = @import("riscv64_2/Select.zig");

pub fn legalizeFeatures(_: *const std.Target) *const Air.Legalize.Features {
    return comptime &.initMany(&.{
        .expand_intcast_safe,
        .expand_int_from_float_safe,
        .expand_int_from_float_optimized_safe,
        .expand_add_safe,
        .expand_sub_safe,
        .expand_mul_safe,

        .soft_f16,
        .soft_f32,
        .soft_f64,
        .soft_f80,
        .soft_f128,

        .soft_bigint,
    });
}

pub fn generate(
    _: *link.File,
    pt: Zcu.PerThread,
    _: Zcu.LazySrcLoc,
    func_index: InternPool.Index,
    air: *const Air,
    liveness: *const ?Air.Liveness,
) !Mir {
    const zcu = pt.zcu;
    const gpa = zcu.gpa;
    const ip = &zcu.intern_pool;
    const func = zcu.funcInfo(func_index);
    const func_type = ip.indexToKey(func.ty).func_type;
    assert(liveness.* == null);

    const mod = zcu.navFileScope(func.owner_nav).mod.?;
    var isel: Select = .{
        .pt = pt,
        .air = air.*,
        .target = &mod.resolved_target.result,
        .nav_index = func.owner_nav,

        .saved_registers = comptime .initEmpty(),
        .instructions = .empty,
        .nav_relocs = .empty,
        .uav_relocs = .empty,
        .global_relocs = .empty,

        .def_order = .empty,
        .blocks = .empty,
        .loops = .empty,
        .active_loops = .empty,
        .loop_live = .{
            .set = .empty,
            .list = .empty,
        },
        .dom_start = 0,
        .dom_len = 0,
        .dom = .empty,

        .returns = false,
        .stack_size = 0,
        // 18.2 RVG Calling Convention:
        // In the standard RISC-V calling convention, the stack grows downward and the
        // stack pointer is always kept 16-byte aligned.
        .stack_align = .@"16",

        .live_registers = comptime .initFill(.free),
        .live_values = .empty,
        .values = .empty,
    };
    defer isel.deinit();

    const air_main_body = air.getMainBody();
    var param_it: Select.CallAbiIterator = .init;
    const air_args = for (air_main_body, 0..) |air_inst_index, body_index| {
        if (air.instructions.items(.tag)[@intFromEnum(air_inst_index)] != .arg) break air_main_body[0..body_index];
        const param_ty = air.instructions.items(.data)[@intFromEnum(air_inst_index)].arg.ty.toType();
        const param_vi = try param_it.param(&isel, param_ty);
        tracking_log.debug("${d} <- %{d}", .{ @intFromEnum(param_vi.?), @intFromEnum(air_inst_index) });
        try isel.live_values.putNoClobber(gpa, air_inst_index, param_vi.?);
    } else unreachable;

    const saved_gra_start = if (mod.strip) param_it.next_gp else Select.CallAbiIterator.gp_start;
    const saved_gra_end = if (false) Select.CallAbiIterator.gp_end else param_it.next_gp;
    const saved_gra_len = @intFromEnum(saved_gra_end) - @intFromEnum(saved_gra_start);

    const frame_record = 2;
    const named_stack_args: Select.Value.Indirect = .{
        .base = .fp,
        .offset = 8 * std.mem.alignForward(u7, frame_record + saved_gra_len, 2),
    };
    // const gr_top = named_stack_args;

    // translate arg locations from caller-based to callee-based
    for (air_args) |air_inst_index| {
        assert(air.instructions.items(.tag)[@intFromEnum(air_inst_index)] == .arg);
        const arg_vi = isel.live_values.get(air_inst_index).?;
        const passed_vi = switch (arg_vi.parent(&isel)) {
            .unallocated, .stack_slot => arg_vi,
            .value, .constant => unreachable,
            .address => |address_vi| address_vi,
        };
        switch (passed_vi.parent(&isel)) {
            .unallocated => {},
            .stack_slot => |stack_slot| {
                assert(stack_slot.base == .sp);
                passed_vi.changeStackSlot(&isel, named_stack_args.withOffset(stack_slot.offset));
            },
            .address, .value, .constant => unreachable,
        }
    }

    ret: {
        var ret_it: Select.CallAbiIterator = .init;
        const ret_vi = try ret_it.ret(&isel, .fromInterned(func_type.return_type)) orelse break :ret;
        tracking_log.debug("${d} <- %main", .{@intFromEnum(ret_vi)});
        try isel.live_values.putNoClobber(gpa, Select.Block.main, ret_vi);
    }

    assert(!(try isel.blocks.getOrPut(gpa, Select.Block.main)).found_existing);
    try isel.analyze(air_main_body);
    try isel.finishAnalysis();
    isel.verify(false);

    isel.blocks.values()[0] = .{
        .live_registers = isel.live_registers,
        .target_label = @intCast(isel.instructions.items.len),
    };
    try isel.body(air_main_body);
    if (isel.live_values.fetchRemove(Select.Block.main)) |ret_vi| {
        switch (ret_vi.value.parent(&isel)) {
            .unallocated, .stack_slot => {},
            .value, .constant => unreachable,
            .address => |address_vi| try address_vi.liveIn(
                &isel,
                address_vi.hint(&isel).?,
                comptime &.initFill(.free),
            ),
        }
        ret_vi.value.deref(&isel);
    }
    isel.verify(true);

    const prologue = isel.instructions.items.len;
    const epilogue = try isel.layout(param_it, mod);

    const instructions = try isel.instructions.toOwnedSlice(gpa);
    var mir: Mir = .{
        .prologue = instructions[prologue..epilogue],
        .body = instructions[0..prologue],
        .epilogue = instructions[epilogue..],
        .literals = &.{},
        .nav_relocs = &.{},
        .uav_relocs = &.{},
        .global_relocs = &.{},
    };
    errdefer mir.deinit(gpa);
    mir.nav_relocs = try isel.nav_relocs.toOwnedSlice(gpa);
    mir.uav_relocs = try isel.uav_relocs.toOwnedSlice(gpa);
    mir.global_relocs = try isel.global_relocs.toOwnedSlice(gpa);

    return mir;
}

const Air = @import("../Air.zig");
const assert = std.debug.assert;
const InternPool = @import("../InternPool.zig");
const link = @import("../link.zig");
const std = @import("std");
const tracking_log = std.log.scoped(.tracking);
const Zcu = @import("../Zcu.zig");
