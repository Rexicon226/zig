const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");

const mem = std.mem;
const math = std.math;
const assert = std.debug.assert;
const Allocator = mem.Allocator;

const Air = @import("../../Air.zig");
const Mir = @import("Mir.zig");
const Emit = @import("Emit.zig");
const Liveness = @import("../../Liveness.zig");
const Type = @import("../../Type.zig");
const Value = @import("../../Value.zig");
const link = @import("../../link.zig");
const Zcu = @import("../../Zcu.zig");
const Package = @import("../../Package.zig");
const InternPool = @import("../../InternPool.zig");
const Compilation = @import("../../Compilation.zig");
const target_util = @import("../../target.zig");
const trace = @import("../../tracy.zig").trace;
const codegen = @import("../../codegen.zig");

const ErrorMsg = Zcu.ErrorMsg;
const Target = std.Target;

const log = std.log.scoped(.riscv_codegen);
const tracking_log = std.log.scoped(.tracking);
const verbose_tracking_log = std.log.scoped(.verbose_tracking);
const wip_mir_log = std.log.scoped(.wip_mir);
const selection_log = std.log.scoped(.select);

const Alignment = InternPool.Alignment;
const CodeGenError = codegen.CodeGenError;

const bits = @import("bits.zig");
const abi = @import("abi.zig");
const Lower = @import("Lower.zig");
const mnem_import = @import("mnem.zig");
const Mnemonic = mnem_import.Mnemonic;
const Pseudo = mnem_import.Pseudo;
const encoding = @import("encoding.zig");

const Register = bits.Register;
const CSR = bits.CSR;
const Immediate = bits.Immediate;
const Memory = bits.Memory;
const FrameIndex = bits.FrameIndex;
const RegisterManager = abi.RegisterManager;
const RegisterLock = RegisterManager.RegisterLock;

const InnerError = CodeGenError || error{OutOfRegisters};

const CodeGen = @This();

pt: Zcu.PerThread,
air: Air,
liveness: Liveness,
bin_file: *link.File,
gpa: Allocator,

mod: *Package.Module,
target: *const std.Target,
debug_output: link.File.DebugInfoOutput,
args: []MCValue,
ret_mcv: InstTracking,
fn_type: Type,
arg_index: usize,
src_loc: Zcu.LazySrcLoc,

mir_instructions: std.MultiArrayList(Mir.Inst) = .{},

owner: Owner,

/// Byte offset within the source file of the ending curly.
end_di_line: u32,
end_di_column: u32,

scope_generation: u32,

/// The value is an offset into the `Function` `code` from the beginning.
/// To perform the reloc, write 32-bit signed little-endian integer
/// which is a relative jump, based on the address following the reloc.
epilogue_relocs: std.ArrayListUnmanaged(Mir.Inst.Index) = .empty,

reused_operands: std.StaticBitSet(Liveness.bpi - 1) = undefined,

/// Whenever there is a runtime branch, we push a Branch onto this stack,
/// and pop it off when the runtime branch joins. This provides an "overlay"
/// of the table of mappings from instructions to `MCValue` from within the branch.
/// This way we can modify the `MCValue` for an instruction in different ways
/// within different branches. Special consideration is needed when a branch
/// joins with its parent, to make sure all instructions have the same MCValue
/// across each runtime branch upon joining.
branch_stack: *std.ArrayList(Branch),

// Currently set vector properties, null means they haven't been set yet in the function.
avl: ?u64,
vtype: ?bits.VType,

// Key is the block instruction
blocks: std.AutoHashMapUnmanaged(Air.Inst.Index, BlockData) = .empty,
register_manager: RegisterManager = .{},

const_tracking: ConstTrackingMap = .{},
inst_tracking: InstTrackingMap = .{},

frame_allocs: std.MultiArrayList(FrameAlloc) = .{},
free_frame_indices: std.AutoArrayHashMapUnmanaged(FrameIndex, void) = .empty,
frame_locs: std.MultiArrayList(Mir.FrameLoc) = .{},

loops: std.AutoHashMapUnmanaged(Air.Inst.Index, struct {
    /// The state to restore before branching.
    state: State,
    /// The branch target.
    jmp_target: Mir.Inst.Index,
}) = .{},

next_temp_index: Temp.Index = @enumFromInt(0),
temp_type: [Temp.Index.max]Type = undefined,

const SymbolOffset = struct { sym_index: u32, off: i32 = 0 };
const RegisterOffset = struct { reg: Register, off: i32 = 0 };
pub const FrameAddr = struct { index: FrameIndex, off: i32 = 0 };

const Owner = union(enum) {
    nav_index: InternPool.Nav.Index,
    lazy_sym: link.File.LazySymbol,

    fn getSymbolIndex(owner: Owner, cg: *CodeGen) !u32 {
        const pt = cg.pt;
        switch (owner) {
            .nav_index => |nav_index| {
                const elf_file = cg.bin_file.cast(.elf).?;
                return elf_file.zigObjectPtr().?.getOrCreateMetadataForNav(pt.zcu, nav_index);
            },
            .lazy_sym => |lazy_sym| {
                const elf_file = cg.bin_file.cast(.elf).?;
                return elf_file.zigObjectPtr().?.getOrCreateMetadataForLazySymbol(elf_file, pt, lazy_sym) catch |err|
                    cg.fail("{s} creating lazy symbol", .{@errorName(err)});
            },
        }
    }
};

const MCValue = union(enum) {
    /// No runtime bits. `void` types, empty structs, u0, enums with 1 tag, etc.
    /// TODO Look into deleting this tag and using `dead` instead, since every use
    /// of MCValue.none should be instead looking at the type and noticing it is 0 bits.
    none,
    /// Control flow will not allow this value to be observed.
    unreach,
    /// No more references to this value remain.
    /// The payload is the value of scope_generation at the point where the death occurred
    dead: u32,
    /// The value is undefined. Contains a symbol index to an undefined constant. Null means
    /// set the undefined value via immediate instead of a load.
    undef: ?u32,
    /// A pointer-sized integer that fits in a register.
    /// If the type is a pointer, this is the pointer address in virtual address space.
    immediate: u64,
    /// The value doesn't exist in memory yet.
    load_symbol: SymbolOffset,
    /// A TLV value.
    load_tlv: u32,
    /// The address of the memory location not-yet-allocated by the linker.
    lea_symbol: SymbolOffset,
    /// The address of a TLV value.
    lea_tlv: u32,
    /// The value is in a target-specific register.
    register: Register,
    /// The value is split across two registers
    register_pair: [2]Register,
    /// The value is in memory at a hard-coded address.
    /// If the type is a pointer, it means the pointer address is at this memory location.
    memory: u64,
    /// The value stored at an offset from a frame index
    /// Payload is a frame address.
    load_frame: FrameAddr,
    /// The address of an offset from a frame index
    /// Payload is a frame address.
    lea_frame: FrameAddr,
    air_ref: Air.Inst.Ref,
    /// The value is in memory at a constant offset from the address in a register.
    indirect: RegisterOffset,
    /// The value is a constant offset from the value in a register.
    register_offset: RegisterOffset,
    /// This indicates that we have already allocated a frame index for this instruction,
    /// but it has not been spilled there yet in the current control flow.
    /// Payload is a frame index.
    reserved_frame: FrameIndex,

    fn isMemory(mcv: MCValue) bool {
        return switch (mcv) {
            .memory, .indirect, .load_frame, .load_symbol => true,
            else => false,
        };
    }

    fn isBase(mcv: MCValue) bool {
        return switch (mcv) {
            .memory, .indirect, .load_frame => true,
            else => false,
        };
    }

    fn isImmediate(mcv: MCValue) bool {
        return switch (mcv) {
            .immediate => true,
            else => false,
        };
    }

    fn isRegister(mcv: MCValue) bool {
        return switch (mcv) {
            .register => true,
            .register_offset => |reg_off| return reg_off.off == 0,
            else => false,
        };
    }

    fn isMutable(mcv: MCValue) bool {
        return switch (mcv) {
            .none => unreachable,
            .unreach => unreachable,
            .dead => unreachable,

            .immediate,
            .memory,
            .lea_frame,
            .undef,
            .lea_symbol,
            .lea_tlv,
            .air_ref,
            .reserved_frame,
            => false,

            .register,
            .register_pair,
            .register_offset,
            .load_symbol,
            .load_tlv,
            .indirect,
            .load_frame,
            => true,
        };
    }

    fn address(mcv: MCValue) MCValue {
        return switch (mcv) {
            .none,
            .unreach,
            .dead,
            .immediate,
            .lea_frame,
            .register_offset,
            .register_pair,
            .register,
            .undef,
            .air_ref,
            .lea_symbol,
            .lea_tlv,
            .reserved_frame,
            => unreachable, // not in memory

            .load_symbol => |sym_off| .{ .lea_symbol = sym_off },
            .load_tlv => |sym| .{ .lea_tlv = sym },
            .memory => |addr| .{ .immediate = addr },
            .load_frame => |off| .{ .lea_frame = off },
            .indirect => |reg_off| switch (reg_off.off) {
                0 => .{ .register = reg_off.reg },
                else => .{ .register_offset = reg_off },
            },
        };
    }

    fn deref(mcv: MCValue) MCValue {
        return switch (mcv) {
            .none,
            .unreach,
            .dead,
            .memory,
            .indirect,
            .undef,
            .air_ref,
            .register_pair,
            .load_frame,
            .load_symbol,
            .load_tlv,
            .reserved_frame,
            => unreachable, // not a pointer

            .immediate => |addr| .{ .memory = addr },
            .register => |reg| .{ .indirect = .{ .reg = reg } },
            .register_offset => |reg_off| .{ .indirect = reg_off },
            .lea_frame => |off| .{ .load_frame = off },
            .lea_symbol => |sym_off| .{ .load_symbol = sym_off },
            .lea_tlv => |sym| .{ .load_tlv = sym },
        };
    }

    fn offset(mcv: MCValue, off: i32) MCValue {
        return switch (mcv) {
            .none,
            .unreach,
            .dead,
            .undef,
            .air_ref,
            .reserved_frame,
            => unreachable, // not valid
            .register_pair,
            .memory,
            .indirect,
            .load_symbol,
            .lea_symbol,
            .lea_tlv,
            .load_tlv,
            => switch (off) {
                0 => mcv,
                else => unreachable,
            },
            .load_frame => |frame| .{ .load_frame = .{ .index = frame.index, .off = frame.off + off } },
            .immediate => |imm| .{ .immediate = @bitCast(@as(i64, @bitCast(imm)) +% off) },
            .register => |reg| .{ .register_offset = .{ .reg = reg, .off = off } },
            .register_offset => |reg_off| .{ .register_offset = .{ .reg = reg_off.reg, .off = reg_off.off + off } },
            .lea_frame => |frame_addr| .{
                .lea_frame = .{ .index = frame_addr.index, .off = frame_addr.off + off },
            },
        };
    }

    fn mem(mcv: MCValue, cg: *CodeGen, mod: Memory.Mod) !Memory {
        _ = mod;
        return switch (mcv) {
            .none,
            .unreach,
            .dead,
            .undef,
            .immediate,
            .register,
            .register_pair,
            .register_offset,
            .load_tlv,
            .lea_tlv,
            .lea_frame,
            .reserved_frame,
            .lea_symbol,
            // => unreachable,
            => std.debug.panic("{s}", .{@tagName(mcv)}),
            else => return cg.fail("TODO: MCValue.mem {s}", .{@tagName(mcv)}),
        };
    }

    fn getReg(mcv: MCValue) ?Register {
        return switch (mcv) {
            .register => |reg| reg,
            .register_offset, .indirect => |ro| ro.reg,
            else => null,
        };
    }

    fn getRegs(mcv: *const MCValue) []const Register {
        return switch (mcv.*) {
            .register => |*reg| reg[0..1],
            .register_pair => |*regs| regs,
            .register_offset, .indirect => |*ro| (&ro.reg)[0..1],
            else => &.{},
        };
    }

    pub fn format(
        mcv: MCValue,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        switch (mcv) {
            .none, .unreach, .dead, .undef => try writer.print("({s})", .{@tagName(mcv)}),
            .immediate => |pl| try writer.print("0x{x}", .{pl}),
            .memory => |pl| try writer.print("0x{x}", .{pl}),
            .register => |reg| try writer.print("{s}", .{@tagName(reg)}),
            .register_pair => |pl| try writer.print("{s}:{s}", .{ @tagName(pl[1]), @tagName(pl[0]) }),
            .air_ref => |pl| try writer.print("(air:0x{x})", .{@intFromEnum(pl)}),
            .reserved_frame => |pl| try writer.print("(dead:{})", .{pl}),
            .lea_frame => |pl| try writer.print("{} + 0x{x}", .{ pl.index, pl.off }),
            .load_frame => |pl| try writer.print("[{} + 0x{x}]", .{ pl.index, pl.off }),
            .load_symbol => |pl| try writer.print("[sym:{} + 0x{x}]", .{ pl.sym_index, pl.off }),
            .lea_symbol => |pl| try writer.print("sym:{} + 0x{x}", .{ pl.sym_index, pl.off }),
            .indirect => |pl| try writer.print("[{s} + 0x{x}]", .{ @tagName(pl.reg), pl.off }),
            else => try writer.print("TODO: format {s}", .{@tagName(mcv)}),
        }
    }
};

const Branch = struct {
    inst_table: std.AutoArrayHashMapUnmanaged(Air.Inst.Index, MCValue) = .empty,

    fn deinit(cg: *Branch, gpa: Allocator) void {
        cg.inst_table.deinit(gpa);
        cg.* = undefined;
    }
};

const InstTrackingMap = std.AutoArrayHashMapUnmanaged(Air.Inst.Index, InstTracking);
const ConstTrackingMap = std.AutoArrayHashMapUnmanaged(InternPool.Index, InstTracking);

const InstTracking = struct {
    long: MCValue,
    short: MCValue,

    fn init(result: MCValue) InstTracking {
        return .{ .long = switch (result) {
            .none,
            .unreach,
            .undef,
            .immediate,
            .memory,
            .load_frame,
            .lea_frame,
            .load_tlv,
            .lea_tlv,
            .load_symbol,
            .lea_symbol,
            => result,
            .dead,
            .reserved_frame,
            .air_ref,
            => unreachable,
            .register,
            .register_pair,
            .register_offset,
            .indirect,
            => .none,
        }, .short = result };
    }

    fn getReg(inst_tracking: InstTracking) ?Register {
        return inst_tracking.short.getReg();
    }

    fn getRegs(inst_tracking: *const InstTracking) []const Register {
        return inst_tracking.short.getRegs();
    }

    fn spill(inst_tracking: *InstTracking, cg: *CodeGen, inst: Air.Inst.Index) !void {
        if (std.meta.eql(inst_tracking.long, inst_tracking.short)) return; // Already spilled
        // Allocate or reuse frame index
        switch (inst_tracking.long) {
            .none => inst_tracking.long = try cg.allocRegOrMem(
                cg.typeOfIndex(inst),
                inst,
                false,
            ),
            .load_frame => {},
            .reserved_frame => |index| inst_tracking.long = .{ .load_frame = .{ .index = index } },
            else => unreachable,
        }
        tracking_log.debug("spill {} from {} to {}", .{ inst, inst_tracking.short, inst_tracking.long });
        try cg.genCopy(cg.typeOfIndex(inst), inst_tracking.long, inst_tracking.short);
    }

    fn reuseFrame(inst_tracking: *InstTracking) void {
        inst_tracking.* = .init(switch (inst_tracking.long) {
            .none => switch (inst_tracking.short) {
                .dead => .none,
                else => |short| short,
            },
            .reserved_frame => |index| .{ .load_frame = .{ .index = index } },
            else => |long| long,
        });
    }

    fn trackSpill(inst_tracking: *InstTracking, function: *CodeGen, inst: Air.Inst.Index) !void {
        try function.freeValue(inst_tracking.short);
        inst_tracking.reuseFrame();
        tracking_log.debug("{} => {} (spilled)", .{ inst, inst_tracking.* });
    }

    fn verifyMaterialize(inst_tracking: InstTracking, target: InstTracking) void {
        switch (inst_tracking.long) {
            .none,
            .load_frame,
            .reserved_frame,
            => switch (target.long) {
                .none,
                .load_frame,
                .reserved_frame,
                => {},
                else => unreachable,
            },
            .unreach,
            .undef,
            .immediate,
            .memory,
            .lea_frame,
            .load_symbol,
            .lea_symbol,
            .load_tlv,
            .lea_tlv,
            => assert(std.meta.eql(inst_tracking.long, target.long)),
            .dead,
            .register,
            .register_pair,
            .register_offset,
            .indirect,
            .air_ref,
            => unreachable,
        }
    }

    fn materialize(
        inst_tracking: *InstTracking,
        function: *CodeGen,
        inst: Air.Inst.Index,
        target: InstTracking,
    ) !void {
        inst_tracking.verifyMaterialize(target);
        try inst_tracking.materializeUnsafe(function, inst, target);
    }

    fn materializeUnsafe(
        inst_tracking: InstTracking,
        function: *CodeGen,
        inst: Air.Inst.Index,
        target: InstTracking,
    ) !void {
        const ty = function.typeOfIndex(inst);
        if ((inst_tracking.long == .none or inst_tracking.long == .reserved_frame) and target.long == .load_frame)
            try function.genCopy(ty, target.long, inst_tracking.short);
        try function.genCopy(ty, target.short, inst_tracking.short);
    }

    fn trackMaterialize(inst_tracking: *InstTracking, inst: Air.Inst.Index, target: InstTracking) void {
        inst_tracking.verifyMaterialize(target);
        // Don't clobber reserved frame indices
        inst_tracking.long = if (target.long == .none) switch (inst_tracking.long) {
            .load_frame => |addr| .{ .reserved_frame = addr.index },
            .reserved_frame => inst_tracking.long,
            else => target.long,
        } else target.long;
        inst_tracking.short = target.short;
        tracking_log.debug("{} => {} (materialize)", .{ inst, inst_tracking.* });
    }

    fn resurrect(inst_tracking: *InstTracking, cg: *CodeGen, inst: Air.Inst.Index, scope_generation: u32) !void {
        switch (inst_tracking.short) {
            .dead => |die_generation| if (die_generation >= scope_generation) {
                inst_tracking.reuseFrame();
                try cg.getValue(inst_tracking.short, inst);
                tracking_log.debug("{} => {} (resurrect)", .{ inst, inst_tracking.* });
            },
            else => {},
        }
    }

    fn die(inst_tracking: *InstTracking, cg: *CodeGen, inst: Air.Inst.Index) !void {
        if (inst_tracking.short == .dead) return;
        try cg.freeValue(inst_tracking.short);
        if (inst_tracking.long == .none) inst_tracking.long = inst_tracking.short;
        inst_tracking.short = .{ .dead = cg.scope_generation };
        tracking_log.debug("{} => {} (death)", .{ inst, inst_tracking.* });
    }

    fn reuse(
        inst_tracking: *InstTracking,
        cg: *CodeGen,
        new_inst: ?Air.Inst.Index,
        old_inst: Air.Inst.Index,
    ) void {
        inst_tracking.short = .{ .dead = cg.scope_generation };
        tracking_log.debug("{?} => {} (reuse {})", .{ new_inst, inst_tracking.*, old_inst });
    }

    fn liveOut(inst_tracking: *InstTracking, cg: *CodeGen, inst: Air.Inst.Index) void {
        for (inst_tracking.getRegs()) |reg| {
            if (cg.register_manager.isRegFree(reg)) {
                tracking_log.debug("{} => {} (live-out)", .{ inst, inst_tracking.* });
                continue;
            }

            const index = RegisterManager.indexOfRegIntoTracked(reg).?;
            const tracked_inst = cg.register_manager.registers[index];
            const tracking = cg.getResolvedInstValue(tracked_inst);

            // Disable death.
            var found_reg = false;
            var remaining_reg: Register = .none;
            for (tracking.getRegs()) |tracked_reg| if (tracked_reg.id() == reg.id()) {
                assert(!found_reg);
                found_reg = true;
            } else {
                assert(remaining_reg == .none);
                remaining_reg = tracked_reg;
            };
            assert(found_reg);
            tracking.short = switch (remaining_reg) {
                .none => .{ .dead = cg.scope_generation },
                else => .{ .register = remaining_reg },
            };

            // Perform side-effects of freeValue manually.
            cg.register_manager.freeReg(reg);

            tracking_log.debug("{} => {} (live-out {})", .{ inst, inst_tracking.*, tracked_inst });
        }
    }

    pub fn format(
        inst_tracking: InstTracking,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        if (!std.meta.eql(inst_tracking.long, inst_tracking.short)) try writer.print("|{}| ", .{inst_tracking.long});
        try writer.print("{}", .{inst_tracking.short});
    }
};

const FrameAlloc = struct {
    abi_size: u31,
    spill_pad: u3,
    abi_align: Alignment,
    ref_count: u16,

    fn init(alloc_abi: struct { size: u64, pad: u3 = 0, alignment: Alignment }) FrameAlloc {
        return .{
            .abi_size = @intCast(alloc_abi.size),
            .spill_pad = alloc_abi.pad,
            .abi_align = alloc_abi.alignment,
            .ref_count = 0,
        };
    }
    fn initType(ty: Type, zcu: *Zcu) FrameAlloc {
        return init(.{
            .size = ty.abiSize(zcu),
            .alignment = ty.abiAlignment(zcu),
        });
    }
    fn initSpill(ty: Type, zcu: *Zcu) FrameAlloc {
        const abi_size = ty.abiSize(zcu);
        const spill_size = if (abi_size < 8)
            math.ceilPowerOfTwoAssert(u64, abi_size)
        else
            std.mem.alignForward(u64, abi_size, 8);
        return init(.{
            .size = spill_size,
            .pad = @intCast(spill_size - abi_size),
            .alignment = ty.abiAlignment(zcu).maxStrict(
                Alignment.fromNonzeroByteUnits(@min(spill_size, 8)),
            ),
        });
    }
};

const BlockData = struct {
    relocs: std.ArrayListUnmanaged(Mir.Inst.Index) = .empty,
    state: State,

    fn deinit(bd: *BlockData, gpa: Allocator) void {
        bd.relocs.deinit(gpa);
        bd.* = undefined;
    }
};

const State = struct {
    registers: RegisterManager.TrackedRegisters,
    reg_tracking: [RegisterManager.RegisterBitSet.bit_length]InstTracking,
    free_registers: RegisterManager.RegisterBitSet,
    next_temp_index: Temp.Index,
    inst_tracking_len: u32,
    scope_generation: u32,
};

fn initRetroactiveState(cg: *CodeGen) State {
    const scope_generation = cg.scope_generation + 1;
    cg.scope_generation = scope_generation;

    var state: State = undefined;
    state.next_temp_index = @enumFromInt(0);
    state.inst_tracking_len = @intCast(cg.inst_tracking.count());
    state.scope_generation = scope_generation;
    return state;
}

fn saveRetroactiveState(cg: *CodeGen, state: *State) !void {
    const free_registers = cg.register_manager.free_registers;
    var it = free_registers.iterator(.{ .kind = .unset });
    while (it.next()) |index| {
        const tracked_inst = cg.register_manager.registers[index];
        state.registers[index] = tracked_inst;
        state.reg_tracking[index] = cg.inst_tracking.get(tracked_inst).?;
    }
    state.free_registers = free_registers;
}

fn saveState(cg: *CodeGen) !State {
    var state = cg.initRetroactiveState();
    try cg.saveRetroactiveState(&state);
    return state;
}

fn restoreState(cg: *CodeGen, state: State, deaths: []const Air.Inst.Index, comptime opts: struct {
    emit_instructions: bool,
    update_tracking: bool,
    resurrect: bool,
    close_scope: bool,
}) !void {
    if (opts.close_scope) {
        for (
            cg.inst_tracking.keys()[@intFromEnum(state.next_temp_index)..@intFromEnum(cg.next_temp_index)],
            cg.inst_tracking.values()[@intFromEnum(state.next_temp_index)..@intFromEnum(cg.next_temp_index)],
        ) |inst, *tracking| try tracking.die(cg, inst);
        cg.next_temp_index = state.next_temp_index;
        for (
            cg.inst_tracking.keys()[state.inst_tracking_len..],
            cg.inst_tracking.values()[state.inst_tracking_len..],
        ) |inst, *tracking| try tracking.die(cg, inst);
        cg.inst_tracking.shrinkRetainingCapacity(state.inst_tracking_len);
    }

    if (opts.resurrect) {
        for (
            cg.inst_tracking.keys()[0..@intFromEnum(state.next_temp_index)],
            cg.inst_tracking.values()[0..@intFromEnum(state.next_temp_index)],
        ) |inst, *tracking| try tracking.resurrect(cg, inst, state.scope_generation);
        for (
            cg.inst_tracking.keys()[Temp.Index.max..state.inst_tracking_len],
            cg.inst_tracking.values()[Temp.Index.max..state.inst_tracking_len],
        ) |inst, *tracking| try tracking.resurrect(cg, inst, state.scope_generation);
    }
    for (deaths) |death| try cg.processDeath(death);

    const ExpectedContents = [@typeInfo(RegisterManager.TrackedRegisters).array.len]RegisterLock;
    var stack align(@max(@alignOf(ExpectedContents), @alignOf(std.heap.StackFallbackAllocator(0)))) =
        if (opts.update_tracking) {} else std.heap.stackFallback(@sizeOf(ExpectedContents), cg.gpa);

    var reg_locks = if (opts.update_tracking) {} else try std.ArrayList(RegisterLock).initCapacity(
        stack.get(),
        @typeInfo(ExpectedContents).array.len,
    );
    defer if (!opts.update_tracking) {
        for (reg_locks.items) |lock| cg.register_manager.unlockReg(lock);
        reg_locks.deinit();
    };

    for (
        0..,
        cg.register_manager.registers,
        state.registers,
        state.reg_tracking,
    ) |reg_i, current_slot, target_slot, reg_tracking| {
        const reg_index: RegisterManager.TrackedIndex = @intCast(reg_i);
        const current_maybe_inst = if (cg.register_manager.isRegIndexFree(reg_index)) null else current_slot;
        const target_maybe_inst = if (state.free_registers.isSet(reg_index)) null else target_slot;
        if (std.debug.runtime_safety) if (target_maybe_inst) |target_inst|
            assert(cg.inst_tracking.getIndex(target_inst).? < state.inst_tracking_len);
        if (opts.emit_instructions) {
            if (current_maybe_inst) |current_inst|
                try cg.inst_tracking.getPtr(current_inst).?.spill(cg, current_inst);
            if (target_maybe_inst) |target_inst|
                try cg.inst_tracking.getPtr(target_inst).?.materialize(cg, target_inst, reg_tracking);
        }
        if (opts.update_tracking) {
            if (current_maybe_inst) |current_inst| {
                try cg.inst_tracking.getPtr(current_inst).?.trackSpill(cg, current_inst);
                cg.register_manager.freeRegIndex(reg_index);
            }
            if (target_maybe_inst) |target_inst| {
                cg.register_manager.getRegIndexAssumeFree(reg_index, target_inst);
                cg.inst_tracking.getPtr(target_inst).?.trackMaterialize(target_inst, reg_tracking);
            }
        } else if (target_maybe_inst) |_|
            try reg_locks.append(cg.register_manager.lockRegIndexAssumeUnused(reg_index));
    }

    if (opts.update_tracking and std.debug.runtime_safety) {
        assert(cg.register_manager.free_registers.eql(state.free_registers));
        var used_reg_it = state.free_registers.iterator(.{ .kind = .unset });
        while (used_reg_it.next()) |index|
            assert(cg.register_manager.registers[index] == state.registers[index]);
    }
}

const CallView = enum(u1) {
    callee,
    caller,
};

pub fn generate(
    bin_file: *link.File,
    pt: Zcu.PerThread,
    src_loc: Zcu.LazySrcLoc,
    func_index: InternPool.Index,
    air: Air,
    liveness: Liveness,
    code: *std.ArrayListUnmanaged(u8),
    debug_output: link.File.DebugInfoOutput,
) CodeGenError!void {
    const zcu = pt.zcu;
    const comp = zcu.comp;
    const gpa = zcu.gpa;
    const ip = &zcu.intern_pool;
    const cg = zcu.funcInfo(func_index);
    const fn_type = Type.fromInterned(cg.ty);
    const mod = zcu.navFileScope(cg.owner_nav).mod;

    var branch_stack = std.ArrayList(Branch).init(gpa);
    defer {
        assert(branch_stack.items.len == 1);
        branch_stack.items[0].deinit(gpa);
        branch_stack.deinit();
    }
    try branch_stack.append(.{});

    var function: CodeGen = .{
        .gpa = gpa,
        .air = air,
        .pt = pt,
        .mod = mod,
        .bin_file = bin_file,
        .liveness = liveness,
        .target = &mod.resolved_target.result,
        .debug_output = debug_output,
        .owner = .{ .nav_index = cg.owner_nav },
        .args = undefined, // populated after `resolveCallingConventionValues`
        .ret_mcv = undefined, // populated after `resolveCallingConventionValues`
        .fn_type = fn_type,
        .arg_index = 0,
        .branch_stack = &branch_stack,
        .src_loc = src_loc,
        .end_di_line = cg.rbrace_line,
        .end_di_column = cg.rbrace_column,
        .scope_generation = 0,
        .avl = null,
        .vtype = null,
    };
    defer {
        function.frame_allocs.deinit(gpa);
        function.free_frame_indices.deinit(gpa);
        function.frame_locs.deinit(gpa);
        function.loops.deinit(gpa);
        var block_it = function.blocks.valueIterator();
        while (block_it.next()) |block| block.deinit(gpa);
        function.blocks.deinit(gpa);
        function.inst_tracking.deinit(gpa);
        function.const_tracking.deinit(gpa);
        function.epilogue_relocs.deinit(gpa);
        function.mir_instructions.deinit(gpa);
    }
    try function.inst_tracking.ensureTotalCapacity(gpa, Temp.Index.max);
    for (0..Temp.Index.max) |temp_index| {
        const temp: Temp.Index = @enumFromInt(temp_index);
        function.inst_tracking.putAssumeCapacityNoClobber(temp.toIndex(), .init(.none));
    }

    wip_mir_log.debug("{}:", .{fmtNav(cg.owner_nav, ip)});

    try function.frame_allocs.resize(gpa, FrameIndex.named_count);
    function.frame_allocs.set(
        @intFromEnum(FrameIndex.stack_frame),
        FrameAlloc.init(.{ .size = 0, .alignment = .@"1" }),
    );
    function.frame_allocs.set(
        @intFromEnum(FrameIndex.call_frame),
        FrameAlloc.init(.{ .size = 0, .alignment = .@"1" }),
    );

    const fn_info = zcu.typeToFunc(fn_type).?;
    var call_info = function.resolveCallingConventionValues(fn_info, &.{}, .args_frame) catch |err| switch (err) {
        error.CodegenFail => return error.CodegenFail,
        else => |e| return e,
    };
    defer call_info.deinit(&function);

    function.args = call_info.args;
    function.ret_mcv = call_info.return_value;
    function.frame_allocs.set(@intFromEnum(FrameIndex.ret_addr), FrameAlloc.init(.{
        .size = Type.u64.abiSize(zcu),
        .alignment = Type.u64.abiAlignment(zcu).min(call_info.stack_align),
    }));
    function.frame_allocs.set(@intFromEnum(FrameIndex.base_ptr), FrameAlloc.init(.{
        .size = Type.u64.abiSize(zcu),
        .alignment = Alignment.min(
            call_info.stack_align,
            Alignment.fromNonzeroByteUnits(function.target.stackAlignment()),
        ),
    }));
    function.frame_allocs.set(@intFromEnum(FrameIndex.args_frame), FrameAlloc.init(.{
        .size = call_info.stack_byte_count,
        .alignment = call_info.stack_align,
    }));
    function.frame_allocs.set(@intFromEnum(FrameIndex.spill_frame), FrameAlloc.init(.{
        .size = 0,
        .alignment = Type.u64.abiAlignment(zcu),
    }));

    function.gen() catch |err| switch (err) {
        error.CodegenFail => return error.CodegenFail,
        error.OutOfRegisters => return function.fail("ran out of registers (Zig compiler bug)", .{}),
        else => |e| return e,
    };

    var mir: Mir = .{
        .instructions = function.mir_instructions.toOwnedSlice(),
        .frame_locs = function.frame_locs.toOwnedSlice(),
    };
    defer mir.deinit(gpa);

    var emit: Emit = .{
        .lower = .{
            .pt = pt,
            .allocator = gpa,
            .mir = mir,
            .cc = fn_info.cc,
            .src_loc = src_loc,
            .output_mode = comp.config.output_mode,
            .link_mode = comp.config.link_mode,
            .pic = mod.pic,
        },
        .bin_file = bin_file,
        .debug_output = debug_output,
        .code = code,
        .prev_di_pc = 0,
        .prev_di_line = cg.lbrace_line,
        .prev_di_column = cg.lbrace_column,
    };
    defer emit.deinit();

    emit.emitMir() catch |err| switch (err) {
        error.LowerFail, error.EmitFail => return function.failMsg(emit.lower.err_msg.?),
        error.InvalidInstruction => |e| return function.fail("emit MIR failed: {s} (Zig compiler bug)", .{@errorName(e)}),
        else => |e| return e,
    };
}

pub fn generateLazy(
    bin_file: *link.File,
    pt: Zcu.PerThread,
    src_loc: Zcu.LazySrcLoc,
    lazy_sym: link.File.LazySymbol,
    code: *std.ArrayListUnmanaged(u8),
    debug_output: link.File.DebugInfoOutput,
) CodeGenError!void {
    const comp = bin_file.comp;
    const gpa = comp.gpa;
    const mod = comp.root_mod;
    const zcu = comp.zcu.?;

    var function: CodeGen = .{
        .gpa = gpa,
        .air = undefined,
        .pt = pt,
        .mod = mod,
        .bin_file = bin_file,
        .liveness = undefined,
        .target = &mod.resolved_target.result,
        .debug_output = debug_output,
        .owner = .{ .lazy_sym = lazy_sym },
        .args = undefined, // populated after `resolveCallingConventionValues`
        .ret_mcv = undefined, // populated after `resolveCallingConventionValues`
        .fn_type = undefined,
        .arg_index = 0,
        .branch_stack = undefined,
        .src_loc = src_loc,
        .end_di_line = undefined,
        .end_di_column = undefined,
        .scope_generation = 0,
        .avl = null,
        .vtype = null,
    };
    defer {
        function.mir_instructions.deinit(gpa);
        function.frame_allocs.deinit(gpa);
    }

    try function.frame_allocs.resize(gpa, FrameIndex.named_count);
    function.frame_allocs.set(
        @intFromEnum(FrameIndex.stack_frame),
        FrameAlloc.init(.{
            .size = 0,
            .alignment = .@"1",
        }),
    );
    function.frame_allocs.set(
        @intFromEnum(FrameIndex.call_frame),
        FrameAlloc.init(.{ .size = 0, .alignment = .@"1" }),
    );
    function.frame_allocs.set(@intFromEnum(FrameIndex.ret_addr), FrameAlloc.init(.{
        .size = Type.u64.abiSize(zcu),
        .alignment = Type.u64.abiAlignment(zcu),
    }));
    function.frame_allocs.set(@intFromEnum(FrameIndex.base_ptr), FrameAlloc.init(.{
        .size = Type.u64.abiSize(zcu),
        .alignment = Alignment.fromNonzeroByteUnits(function.target.stackAlignment()),
    }));
    function.frame_allocs.set(@intFromEnum(FrameIndex.args_frame), FrameAlloc.init(.{
        .size = 0,
        .alignment = .@"1",
    }));
    function.frame_allocs.set(@intFromEnum(FrameIndex.spill_frame), FrameAlloc.init(.{
        .size = 0,
        .alignment = Type.u64.abiAlignment(zcu),
    }));

    function.genLazy(lazy_sym) catch |err| switch (err) {
        error.CodegenFail => return error.CodegenFail,
        error.OutOfRegisters => return function.fail("ran out of registers (Zig compiler bug)", .{}),
        else => |e| return e,
    };

    var mir: Mir = .{
        .instructions = function.mir_instructions.toOwnedSlice(),
        .frame_locs = function.frame_locs.toOwnedSlice(),
    };
    defer mir.deinit(gpa);

    var emit: Emit = .{
        .lower = .{
            .pt = pt,
            .allocator = gpa,
            .mir = mir,
            .cc = .auto,
            .src_loc = src_loc,
            .output_mode = comp.config.output_mode,
            .link_mode = comp.config.link_mode,
            .pic = mod.pic,
        },
        .bin_file = bin_file,
        .debug_output = debug_output,
        .code = code,
        .prev_di_pc = undefined, // no debug info yet
        .prev_di_line = undefined, // no debug info yet
        .prev_di_column = undefined, // no debug info yet
    };
    defer emit.deinit();

    emit.emitMir() catch |err| switch (err) {
        error.LowerFail, error.EmitFail => return function.failMsg(emit.lower.err_msg.?),
        error.InvalidInstruction => |e| return function.fail("emit MIR failed: {s} (Zig compiler bug)", .{@errorName(e)}),
        else => |e| return e,
    };
}

const FormatWipMirData = struct {
    cg: *CodeGen,
    inst: Mir.Inst.Index,
};
fn formatWipMir(
    data: FormatWipMirData,
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    const inst = data.cg.mir_instructions.get(data.inst);
    try writer.print("{}", .{inst});
}
fn fmtWipMir(cg: *CodeGen, inst: Mir.Inst.Index) std.fmt.Formatter(formatWipMir) {
    return .{ .data = .{ .cg = cg, .inst = inst } };
}

const FormatNavData = struct {
    ip: *const InternPool,
    nav_index: InternPool.Nav.Index,
};
fn formatNav(
    data: FormatNavData,
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    try writer.print("{}", .{data.ip.getNav(data.nav_index).fqn.fmt(data.ip)});
}
fn fmtNav(nav_index: InternPool.Nav.Index, ip: *const InternPool) std.fmt.Formatter(formatNav) {
    return .{ .data = .{
        .ip = ip,
        .nav_index = nav_index,
    } };
}

const FormatAirData = struct {
    cg: *CodeGen,
    inst: Air.Inst.Index,
};
fn formatAir(
    data: FormatAirData,
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    @import("../../print_air.zig").dumpInst(
        data.inst,
        data.cg.pt,
        data.cg.air,
        data.cg.liveness,
    );
}
fn fmtAir(cg: *CodeGen, inst: Air.Inst.Index) std.fmt.Formatter(formatAir) {
    return .{ .data = .{ .cg = cg, .inst = inst } };
}

const FormatTrackingData = struct {
    cg: *CodeGen,
};
fn formatTracking(
    data: FormatTrackingData,
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    var it = data.cg.inst_tracking.iterator();
    while (it.next()) |entry| try writer.print("\n{} = {}", .{ entry.key_ptr.*, entry.value_ptr.* });
}
fn fmtTracking(cg: *CodeGen) std.fmt.Formatter(formatTracking) {
    return .{ .data = .{ .cg = cg } };
}

fn addInst(cg: *CodeGen, inst: Mir.Inst) error{OutOfMemory}!Mir.Inst.Index {
    const gpa = cg.gpa;
    try cg.mir_instructions.ensureUnusedCapacity(gpa, 1);
    const result_index: Mir.Inst.Index = @intCast(cg.mir_instructions.len);
    cg.mir_instructions.appendAssumeCapacity(inst);
    if (switch (inst.tag) {
        else => true,
        .pseudo_dbg_prologue_end,
        .pseudo_dbg_line_column,
        .pseudo_dbg_epilogue_begin,
        .pseudo_dead,
        => false,
    }) wip_mir_log.debug("{}", .{cg.fmtWipMir(result_index)});
    return result_index;
}

fn asmPseudo(cg: *CodeGen, mnem: Mnemonic) error{OutOfMemory}!Mir.Inst.Index {
    return cg.addInst(.{
        .tag = mnem,
        .data = .none,
    });
}

fn asmNone(
    cg: *CodeGen,
    mnem: Mnemonic,
) !void {
    _ = try cg.addInst(.{
        .tag = mnem,
        .data = .none,
    });
}

fn asmRType(
    cg: *CodeGen,
    mnem: Mnemonic,
    rd: Register,
    rs1: Register,
    rs2: Register,
) !void {
    _ = try cg.addInst(.{
        .tag = mnem,
        .data = .{ .r_type = .{
            .rd = rd,
            .rs1 = rs1,
            .rs2 = rs2,
        } },
    });
}

fn asmIType(
    cg: *CodeGen,
    mnem: Mnemonic,
    rd: Register,
    rs1: Register,
    imm12: Immediate,
) !void {
    _ = try cg.addInst(.{
        .tag = mnem,
        .data = .{ .i_type = .{
            .rd = rd,
            .rs1 = rs1,
            .imm12 = imm12,
        } },
    });
}

fn asmUType(
    cg: *CodeGen,
    mnem: Mnemonic,
    rd: Register,
    imm20: Immediate,
) !void {
    _ = try cg.addInst(.{
        .tag = mnem,
        .data = .{ .u_type = .{
            .rd = rd,
            .imm20 = imm20,
        } },
    });
}

/// Returns a temporary register that contains the value of the `reg` csr.
///
/// Caller's duty to lock the return register is needed.
fn getCsr(cg: *CodeGen, csr: CSR) !Register {
    assert(cg.hasFeature(.zicsr));
    const dst_reg = try cg.register_manager.allocReg(null, cg.regTempClassForType(Type.u64));
    _ = try cg.addInst(.{
        .tag = .csrrs,
        .data = .{ .csr = .{
            .csr = csr,
            .rd = dst_reg,
            .rs1 = .x0,
        } },
    });
    return dst_reg;
}

fn setVl(cg: *CodeGen, dst_reg: Register, avl: u64, options: bits.VType) !void {
    if (cg.avl == avl) if (cg.vtype) |vtype| {
        // it's already set, we don't need to do anything
        if (vtype == options) return;
    };

    cg.avl = avl;
    cg.vtype = options;

    if (avl == 0) {
        // the caller means to do "vsetvli zero, zero ..." which keeps the avl to whatever it was before
        const options_int: u12 = @as(u12, 0) | @as(u8, @bitCast(options));
        try cg.asmIType(.vsetvli, dst_reg, .zero, .u(options_int));
    } else {
        // if the avl can fit into u5 we can use vsetivli otherwise use vsetvli
        if (avl <= std.math.maxInt(u5)) {
            const options_int: u12 = (~@as(u12, 0) << 10) | @as(u8, @bitCast(options));
            try cg.asmIType(.vsetivli, dst_reg, @enumFromInt(avl), .u(options_int));
        } else {
            const options_int: u12 = @as(u12, 0) | @as(u8, @bitCast(options));
            const temp_reg = try cg.copyToTmpRegister(.u64, .{ .immediate = avl });
            try cg.asmIType(.vsetvli, dst_reg, temp_reg, .u(options_int));
        }
    }
}

const required_features = [_]Target.riscv.Feature{
    .d,
    .m,
    .a,
    .zicsr,
    .v,
    .zbb,
};

fn gen(cg: *CodeGen) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const fn_info = zcu.typeToFunc(cg.fn_type).?;

    inline for (required_features) |feature| {
        if (!cg.hasFeature(feature)) {
            return cg.fail(
                "target missing required feature {s}",
                .{@tagName(feature)},
            );
        }
    }

    if (fn_info.cc != .naked) {
        _ = try cg.asmPseudo(.pseudo_dbg_prologue_end);

        const backpatch_stack_adjust = try cg.asmPseudo(.pseudo_dead);
        const backpatch_ra_spill = try cg.asmPseudo(.pseudo_dead);
        const backpatch_fp_spill = try cg.asmPseudo(.pseudo_dead);
        const backpatch_fp_add = try cg.asmPseudo(.pseudo_dead);
        const backpatch_spill_callee_preserved_regs = try cg.asmPseudo(.pseudo_dead);

        switch (cg.ret_mcv.long) {
            .none, .unreach => {},
            .indirect => {
                // The address where to store the return value for the caller is in a
                // register which the callee is free to clobber. Therefore, we purposely
                // spill it to stack immediately.
                const frame_index = try cg.allocFrameIndex(.initSpill(Type.u64, zcu));
                try cg.genSetMem(
                    .{ .frame = frame_index },
                    0,
                    Type.u64,
                    cg.ret_mcv.long.address().offset(-cg.ret_mcv.short.indirect.off),
                );
                cg.ret_mcv.long = .{ .load_frame = .{ .index = frame_index } };
                tracking_log.debug("spill {} to {}", .{ cg.ret_mcv.long, frame_index });
            },
            else => unreachable,
        }

        try cg.genBody(cg.air.getMainBody());

        if (cg.epilogue_relocs.items.len > 0) {
            const epilogue_relocs_last_index = cg.epilogue_relocs.items.len - 1;
            const relocs = if (cg.epilogue_relocs.items[epilogue_relocs_last_index] == cg.mir_instructions.len - 1) epilogue_relocs: {
                _ = cg.mir_instructions.pop();
                break :epilogue_relocs cg.epilogue_relocs.items[0..epilogue_relocs_last_index];
            } else cg.epilogue_relocs.items;
            for (relocs) |epilogue_reloc| cg.performReloc(epilogue_reloc);
        }

        _ = try cg.asmPseudo(.pseudo_dbg_epilogue_begin);

        const backpatch_restore_callee_preserved_regs = try cg.asmPseudo(.pseudo_dead);
        const backpatch_ra_restore = try cg.asmPseudo(.pseudo_dead);
        const backpatch_fp_restore = try cg.asmPseudo(.pseudo_dead);
        const backpatch_stack_alloc_restore = try cg.asmPseudo(.pseudo_dead);

        // ret
        try cg.asmIType(.jalr, .zero, .ra, .s(0));

        const frame_layout = try cg.computeFrameLayout();
        const need_save_reg = frame_layout.save_reg_list.count() > 0;

        // prologue
        cg.mir_instructions.set(backpatch_stack_adjust, .{
            .tag = .pseudo_large_addi,
            .data = .{
                .i_type = .{
                    .rd = .sp,
                    .rs1 = .sp,
                    .imm12 = Immediate.s(-frame_layout.stack_adjust),
                },
            },
        });
        cg.mir_instructions.set(backpatch_ra_spill, .{
            .tag = .pseudo_store_rm,
            .data = .{ .rm = .{
                .r = .ra,
                .m = .{
                    .base = .{ .frame = .ret_addr },
                    .mod = .{ .size = .double, .unsigned = false },
                },
            } },
        });
        cg.mir_instructions.set(backpatch_fp_spill, .{
            .tag = .pseudo_store_rm,
            .data = .{ .rm = .{
                .r = .s0,
                .m = .{
                    .base = .{ .frame = .base_ptr },
                    .mod = .{ .size = .double, .unsigned = false },
                },
            } },
        });
        cg.mir_instructions.set(backpatch_fp_add, .{
            .tag = .pseudo_large_addi,
            .data = .{ .i_type = .{
                .rd = .s0,
                .rs1 = .sp,
                .imm12 = Immediate.s(frame_layout.stack_adjust),
            } },
        });

        // epilogue
        cg.mir_instructions.set(backpatch_ra_restore, .{
            .tag = .pseudo_load_rm,
            .data = .{ .rm = .{
                .r = .ra,
                .m = .{
                    .base = .{ .frame = .ret_addr },
                    .mod = .{ .size = .double, .unsigned = false },
                },
            } },
        });
        cg.mir_instructions.set(backpatch_fp_restore, .{
            .tag = .pseudo_load_rm,
            .data = .{ .rm = .{
                .r = .s0,
                .m = .{
                    .base = .{ .frame = .base_ptr },
                    .mod = .{ .size = .double, .unsigned = false },
                },
            } },
        });

        cg.mir_instructions.set(backpatch_stack_alloc_restore, .{
            .tag = .pseudo_large_addi,
            .data = .{ .i_type = .{
                .rd = .sp,
                .rs1 = .sp,
                .imm12 = Immediate.s(@intCast(frame_layout.stack_adjust)),
            } },
        });

        if (need_save_reg) {
            cg.mir_instructions.set(backpatch_spill_callee_preserved_regs, .{
                .tag = .pseudo_spill_regs,
                .data = .{ .reg_list = frame_layout.save_reg_list },
            });

            cg.mir_instructions.set(backpatch_restore_callee_preserved_regs, .{
                .tag = .pseudo_restore_regs,
                .data = .{ .reg_list = frame_layout.save_reg_list },
            });
        }
    } else {
        _ = try cg.asmPseudo(.pseudo_dbg_prologue_end);
        try cg.genBody(cg.air.getMainBody());
        _ = try cg.asmPseudo(.pseudo_dbg_epilogue_begin);
    }

    // Drop them off at the rbrace.
    _ = try cg.addInst(.{
        .tag = .pseudo_dbg_line_column,
        .data = .{ .pseudo_dbg_line_column = .{
            .line = cg.end_di_line,
            .column = cg.end_di_column,
        } },
    });
}

fn genLazy(cg: *CodeGen, lazy_sym: link.File.LazySymbol) InnerError!void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const ip = &zcu.intern_pool;
    switch (Type.fromInterned(lazy_sym.ty).zigTypeTag(zcu)) {
        .@"enum" => {
            const enum_ty = Type.fromInterned(lazy_sym.ty);
            wip_mir_log.debug("{}.@tagName:", .{enum_ty.fmt(pt)});

            const backpatch_stack_adjust = try cg.asmPseudo(.pseudo_dead);
            const backpatch_fp_spill = try cg.asmPseudo(.pseudo_dead);
            const backpatch_fp_add = try cg.asmPseudo(.pseudo_dead);
            const backpatch_spill_callee_preserved_regs = try cg.asmPseudo(.pseudo_dead);

            const param_regs = abi.Registers.Integer.function_arg_regs;
            const ret_ptr_reg = param_regs[0];
            const enum_mcv: MCValue = .{ .register = param_regs[1] };

            const epilogue_relocs = try cg.gpa.alloc(Mir.Inst.Index, enum_ty.enumFieldCount(zcu));
            defer cg.gpa.free(epilogue_relocs);

            const data_reg, const data_lock = try cg.allocReg(.int);
            defer cg.register_manager.unlockReg(data_lock);

            const elf_file = cg.bin_file.cast(.elf).?;
            const zo = elf_file.zigObjectPtr().?;
            const sym_index = zo.getOrCreateMetadataForLazySymbol(elf_file, pt, .{
                .kind = .const_data,
                .ty = enum_ty.toIntern(),
            }) catch |err|
                return cg.fail("{s} creating lazy symbol", .{@errorName(err)});

            try cg.genSetReg(Type.u64, data_reg, .{ .lea_symbol = .{ .sym_index = sym_index } });

            const cmp_reg, const cmp_lock = try cg.allocReg(.int);
            defer cg.register_manager.unlockReg(cmp_lock);

            var data_off: i32 = 0;
            const tag_names = enum_ty.enumFields(zcu);
            for (epilogue_relocs, 0..) |*exitlude_jump_reloc, tag_index| {
                const tag_name_len = tag_names.get(ip)[tag_index].length(ip);
                const tag_val = try pt.enumValueFieldIndex(enum_ty, @intCast(tag_index));
                const tag_mcv = try cg.genTypedValue(tag_val);

                _ = try cg.genBinOp(
                    .cmp_eq,
                    enum_mcv,
                    enum_ty,
                    tag_mcv,
                    enum_ty,
                    cmp_reg,
                );
                const skip_reloc = try cg.condBr(Type.bool, .{ .register = cmp_reg });
                try cg.genSetMem(
                    .{ .reg = ret_ptr_reg },
                    0,
                    Type.u64,
                    .{ .register_offset = .{ .reg = data_reg, .off = data_off } },
                );
                try cg.genSetMem(
                    .{ .reg = ret_ptr_reg },
                    8,
                    Type.u64,
                    .{ .immediate = tag_name_len },
                );

                exitlude_jump_reloc.* = try cg.addInst(.{
                    .tag = .pseudo_j,
                    .data = .{ .j_type = .{
                        .rd = .zero,
                        .inst = undefined,
                    } },
                });
                cg.performReloc(skip_reloc);

                data_off += @intCast(tag_name_len + 1);
            }

            _ = try cg.addInst(.{
                .tag = .unimp,
                .data = .none,
            });

            for (epilogue_relocs) |reloc| cg.performReloc(reloc);

            const backpatch_restore_callee_preserved_regs = try cg.asmPseudo(.pseudo_dead);
            const backpatch_fp_restore = try cg.asmPseudo(.pseudo_dead);
            const backpatch_stack_alloc_restore = try cg.asmPseudo(.pseudo_dead);

            const frame_layout = try cg.computeFrameLayout();
            const need_save_reg = frame_layout.save_reg_list.count() > 0;

            if (need_save_reg) {
                cg.mir_instructions.set(backpatch_spill_callee_preserved_regs, .{
                    .tag = .pseudo_spill_regs,
                    .data = .{ .reg_list = frame_layout.save_reg_list },
                });

                cg.mir_instructions.set(backpatch_restore_callee_preserved_regs, .{
                    .tag = .pseudo_restore_regs,
                    .data = .{ .reg_list = frame_layout.save_reg_list },
                });
            }
            cg.mir_instructions.set(backpatch_fp_add, .{
                .tag = .addi,
                .data = .{ .i_type = .{
                    .rd = .s0,
                    .rs1 = .sp,
                    .imm12 = Immediate.s(@intCast(frame_layout.stack_adjust)),
                } },
            });
            cg.mir_instructions.set(backpatch_stack_alloc_restore, .{
                .tag = .addi,
                .data = .{ .i_type = .{
                    .rd = .sp,
                    .rs1 = .sp,
                    .imm12 = Immediate.s(@intCast(frame_layout.stack_adjust)),
                } },
            });
            cg.mir_instructions.set(backpatch_stack_adjust, .{
                .tag = .addi,
                .data = .{ .i_type = .{
                    .rd = .sp,
                    .rs1 = .sp,
                    .imm12 = Immediate.s(-@as(i32, @intCast(frame_layout.stack_adjust))),
                } },
            });
            cg.mir_instructions.set(backpatch_fp_spill, .{
                .tag = .pseudo_store_rm,
                .data = .{ .rm = .{
                    .r = .s0,
                    .m = .{
                        .base = .{ .frame = .base_ptr },
                        .mod = .{ .size = .double, .unsigned = false },
                    },
                } },
            });
            cg.mir_instructions.set(backpatch_fp_restore, .{
                .tag = .pseudo_load_rm,
                .data = .{ .rm = .{
                    .r = .s0,
                    .m = .{
                        .base = .{ .frame = .base_ptr },
                        .mod = .{ .size = .double, .unsigned = false },
                    },
                } },
            });

            try cg.asmIType(.jalr, .zero, .ra, .s(0));
        },
        else => return cg.fail(
            "TODO implement {s} for {}",
            .{ @tagName(lazy_sym.kind), Type.fromInterned(lazy_sym.ty).fmt(pt) },
        ),
    }
}

fn genBody(cg: *CodeGen, body: []const Air.Inst.Index) InnerError!void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const ip = &zcu.intern_pool;
    const air_tags = cg.air.instructions.items(.tag);
    const air_datas = cg.air.instructions.items(.data);

    cg.arg_index = 0;
    for (body) |inst| switch (air_tags[@intFromEnum(inst)]) {
        .arg => {
            wip_mir_log.debug("{}", .{cg.fmtAir(inst)});
            verbose_tracking_log.debug("before: {}", .{cg.fmtTracking()});

            cg.reused_operands = .initEmpty();
            try cg.inst_tracking.ensureUnusedCapacity(cg.gpa, 1);

            try cg.airArg(inst);

            verbose_tracking_log.debug("after: {}", .{cg.fmtTracking()});

            try cg.resetTemps();
            cg.checkInvariantsAfterAirInst();
        },
        else => break,
    };
    cg.arg_index = 0;

    for (body) |inst| {
        if (cg.liveness.isUnused(inst) and !cg.air.mustLower(inst, ip)) continue;
        wip_mir_log.debug("{}", .{cg.fmtAir(inst)});
        verbose_tracking_log.debug("{}", .{cg.fmtTracking()});

        cg.reused_operands = .initEmpty();
        try cg.inst_tracking.ensureUnusedCapacity(cg.gpa, 1);
        const tag = air_tags[@intFromEnum(inst)];
        switch (tag) {
            // zig fmt: off
            .add_wrap,
            .sub_wrap,
            .add_sat,

            .mul_wrap,
            .div_trunc, 
            .div_exact,
            .rem,

            .shl,
            .shl_exact,
            .shr,
            .shr_exact,
            => try cg.airBinOp(inst, tag),

            // zig fmt: on
            .add, .add_optimized => {
                const bin_op = air_datas[@intFromEnum(inst)].bin_op;
                var ops = try cg.tempsFromOperands(inst, .{ bin_op.lhs, bin_op.rhs });
                var res: [1]Temp = undefined;
                cg.select(&res, &.{cg.typeOf(bin_op.lhs)}, &ops, comptime &.{ .{
                    .src_constraints = .{
                        .{ .exact_int = 1 },
                        .{ .exact_int = 1 },
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .int_reg, .none } },
                        .{ .src = .{ .to_mut_int_reg, .to_int_reg, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .xor, .dst0b, .src0b, .src1b },
                    } },
                }, .{
                    .src_constraints = .{
                        .{ .exact_unsigned_int = 32 },
                        .{ .exact_unsigned_int = 32 },
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .int_reg, .none } },
                        .{ .src = .{ .to_mut_int_reg, .to_int_reg, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .addw, .dst0w, .src0w, .src1w },
                        .{ ._, .slli, .dst0w, .dst0w, .ui(32) },
                        .{ ._, .srli, .dst0w, .dst0w, .ui(32) },
                    } },
                }, .{
                    .src_constraints = .{
                        .{ .exact_int = 32 },
                        .{ .exact_int = 32 },
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .int_reg, .none } },
                        .{ .src = .{ .to_mut_int_reg, .to_int_reg, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .addw, .dst0w, .src0w, .src1w },
                    } },
                }, .{
                    .src_constraints = .{
                        .{ .int = .double },
                        .{ .int = .double },
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .int_reg, .none } },
                        .{ .src = .{ .to_mut_int_reg, .to_int_reg, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .add, .dst0d, .src0d, .src1d },
                    } },
                }, .{
                    .required_features = .{ .f, null, null, null },
                    .src_constraints = .{
                        .{ .float = .word },
                        .{ .float = .word },
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_float_reg, .float_reg, .none } },
                        .{ .src = .{ .to_mut_float_reg, .to_float_reg, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .fadds, .dst0d, .src0d, .src1d },
                    } },
                }, .{
                    .required_features = .{ .d, null, null, null },
                    .src_constraints = .{
                        .{ .float = .double },
                        .{ .float = .double },
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_float_reg, .float_reg, .none } },
                        .{ .src = .{ .to_mut_float_reg, .to_float_reg, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .faddd, .dst0d, .src0d, .src1d },
                    } },
                } }) catch |err| switch (err) {
                    error.SelectFailed => return cg.fail("failed to select add {} {} {}", .{
                        cg.typeOf(bin_op.lhs).fmt(pt),
                        ops[0].tracking(cg),
                        ops[1].tracking(cg),
                    }),
                    else => |e| return e,
                };
                try res[0].finish(inst, &.{ bin_op.lhs, bin_op.rhs }, &ops, cg);
            },
            .sub, .sub_optimized => {
                const bin_op = air_datas[@intFromEnum(inst)].bin_op;
                var ops = try cg.tempsFromOperands(inst, .{ bin_op.lhs, bin_op.rhs });
                var res: [1]Temp = undefined;
                cg.select(&res, &.{cg.typeOf(bin_op.lhs)}, &ops, comptime &.{ .{
                    .src_constraints = .{
                        .{ .exact_unsigned_int = 1 },
                        .{ .exact_unsigned_int = 1 },
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .int_reg, .none } },
                        .{ .src = .{ .to_mut_int_reg, .to_int_reg, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .xor, .dst0b, .src0b, .src1b },
                    } },
                }, .{
                    .src_constraints = .{
                        .{ .exact_int = 32 },
                        .{ .exact_int = 32 },
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .int_reg, .none } },
                        .{ .src = .{ .to_mut_int_reg, .to_int_reg, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .subw, .dst0b, .src0b, .src1b },
                    } },
                }, .{
                    .src_constraints = .{
                        .{ .int = .double },
                        .{ .int = .double },
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .int_reg, .none } },
                        .{ .src = .{ .to_mut_int_reg, .to_int_reg, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .sub, .dst0b, .src0b, .src1b },
                    } },
                }, .{
                    .required_features = .{ .f, null, null, null },
                    .src_constraints = .{
                        .{ .float = .word },
                        .{ .float = .word },
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_float_reg, .float_reg, .none } },
                        .{ .src = .{ .to_mut_float_reg, .to_float_reg, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .fsubs, .dst0d, .src0d, .src1d },
                    } },
                }, .{
                    .required_features = .{ .d, null, null, null },
                    .src_constraints = .{
                        .{ .float = .double },
                        .{ .float = .double },
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_float_reg, .float_reg, .none } },
                        .{ .src = .{ .to_mut_float_reg, .to_float_reg, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .fsubd, .dst0d, .src0d, .src1d },
                    } },
                } }) catch |err| switch (err) {
                    error.SelectFailed => return cg.fail("failed to select sub {} {} {}", .{
                        cg.typeOf(bin_op.lhs).fmt(pt),
                        ops[0].tracking(cg),
                        ops[1].tracking(cg),
                    }),
                    else => |e| return e,
                };
                try res[0].finish(inst, &.{ bin_op.lhs, bin_op.rhs }, &ops, cg);
            },
            .mul, .mul_optimized => {
                const bin_op = air_datas[@intFromEnum(inst)].bin_op;
                var ops = try cg.tempsFromOperands(inst, .{ bin_op.lhs, bin_op.rhs });
                var res: [1]Temp = undefined;
                cg.select(&res, &.{cg.typeOf(bin_op.lhs)}, &ops, comptime &.{ .{
                    .src_constraints = .{
                        .{ .exact_unsigned_int = 1 },
                        .{ .exact_unsigned_int = 1 },
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .int_reg, .none } },
                        .{ .src = .{ .to_mut_int_reg, .to_int_reg, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .@"and", .dst0b, .src0b, .src1b },
                    } },
                }, .{
                    .required_features = .{ .m, null, null, null },
                    .src_constraints = .{
                        .{ .exact_int = 32 },
                        .{ .exact_int = 32 },
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .int_reg, .none } },
                        .{ .src = .{ .to_mut_int_reg, .to_int_reg, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .mulw, .dst0h, .src0h, .src1w },
                    } },
                }, .{
                    .required_features = .{ .m, null, null, null },
                    .src_constraints = .{
                        .{ .int = .double },
                        .{ .int = .double },
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .int_reg, .none } },
                        .{ .src = .{ .to_mut_int_reg, .to_int_reg, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .mul, .dst0d, .src0d, .src1d },
                    } },
                }, .{
                    .required_features = .{ .f, null, null, null },
                    .src_constraints = .{
                        .{ .float = .word },
                        .{ .float = .word },
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_float_reg, .float_reg, .none } },
                        .{ .src = .{ .to_mut_float_reg, .to_float_reg, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .fmuls, .dst0d, .src0d, .src1d },
                    } },
                }, .{
                    .required_features = .{ .d, null, null, null },
                    .src_constraints = .{
                        .{ .float = .double },
                        .{ .float = .double },
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_float_reg, .float_reg, .none } },
                        .{ .src = .{ .to_mut_float_reg, .to_float_reg, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .fmuld, .dst0d, .src0d, .src1d },
                    } },
                } }) catch |err| switch (err) {
                    error.SelectFailed => return cg.fail("failed to select mul {} {} {}", .{
                        cg.typeOf(bin_op.lhs).fmt(pt),
                        ops[0].tracking(cg),
                        ops[1].tracking(cg),
                    }),
                    else => |e| return e,
                };
                try res[0].finish(inst, &.{ bin_op.lhs, bin_op.rhs }, &ops, cg);
            },
            .bit_and,
            .bit_or,
            .xor,
            .bool_and,
            .bool_or,
            => |air_tag| {
                const bin_op = air_datas[@intFromEnum(inst)].bin_op;
                var ops = try cg.tempsFromOperands(inst, .{ bin_op.lhs, bin_op.rhs });
                var res: [1]Temp = undefined;
                cg.select(&res, &.{cg.typeOf(bin_op.lhs)}, &ops, switch (@as(Mnemonic, switch (air_tag) {
                    else => unreachable,
                    .bit_and, .bool_and => .@"and",
                    .bit_or, .bool_or => .@"or",
                    .xor => .xor,
                })) {
                    else => unreachable,
                    inline .@"and", .@"or", .xor => |mir_tag| comptime &.{.{
                        .src_constraints = .{
                            .{ .int = .double },
                            .{ .int = .double },
                            .any,
                        },
                        .dst_temps = .{ .{ .ref = .src0 }, .unused },
                        .patterns = &.{
                            .{ .src = .{ .mut_int_reg, .int_reg, .none } },
                            .{ .src = .{ .to_mut_int_reg, .to_int_reg, .none } },
                        },
                        .each = .{ .once = &.{
                            .{ ._, mir_tag, .dst0d, .src0d, .src1d },
                        } },
                    }},
                }) catch |err| switch (err) {
                    error.SelectFailed => return cg.fail("failed to select {s} {} {} {}", .{
                        @tagName(air_tag),
                        cg.typeOf(bin_op.lhs).fmt(pt),
                        ops[0].tracking(cg),
                        ops[1].tracking(cg),
                    }),
                    else => |e| return e,
                };
                try res[0].finish(inst, &.{ bin_op.lhs, bin_op.rhs }, &ops, cg);
            },
            .cmp_lt,
            .cmp_lt_optimized,
            .cmp_lte,
            .cmp_lte_optimized,
            .cmp_gte,
            .cmp_gte_optimized,
            .cmp_gt,
            .cmp_gt_optimized,
            => |air_tag| {
                const bin_op = air_datas[@intFromEnum(inst)].bin_op;
                const cmp_op = air_tag.toCmpOp().?;
                var ops = try cg.tempsFromOperands(inst, .{ bin_op.lhs, bin_op.rhs });
                var res: [1]Temp = undefined;
                (if (cg.floatBits(cg.typeOf(bin_op.lhs))) |float_bits| err: {
                    if (float_bits != 32 and float_bits != 64)
                        return cg.fail("TODO: {s} float {d} bits", .{ @tagName(air_tag), float_bits });
                    switch (cmp_op) {
                        else => unreachable,
                        .lt, .lte => {},
                        .gt, .gte => std.mem.swap(Temp, &ops[0], &ops[1]),
                    }
                    break :err cg.select(&res, &.{.bool}, &ops, switch (switch (cmp_op) {
                        else => unreachable,
                        .lt, .gt => false,
                        .lte, .gte => true,
                    }) {
                        inline false, true => |equal| comptime &.{ .{
                            .required_features = .{ .f, null, null, null },
                            .src_constraints = .{
                                .{ .float = .word },
                                .{ .float = .word },
                                .any,
                            },
                            .dst_temps = .{ .{ .rc = .int }, .unused },
                            .patterns = &.{
                                .{ .src = .{ .float_reg, .float_reg, .none } },
                                .{ .src = .{ .to_float_reg, .to_float_reg, .none } },
                            },
                            .each = .{ .once = &.{
                                .{ ._, if (equal) .fles else .flts, .dst0b, .src0w, .src1w },
                            } },
                        }, .{
                            .required_features = .{ .d, null, null, null },
                            .src_constraints = .{
                                .{ .float = .double },
                                .{ .float = .double },
                                .any,
                            },
                            .dst_temps = .{ .{ .rc = .int }, .unused },
                            .patterns = &.{
                                .{ .src = .{ .float_reg, .float_reg, .none } },
                                .{ .src = .{ .to_float_reg, .to_float_reg, .none } },
                            },
                            .each = .{ .once = &.{
                                .{ ._, if (equal) .fled else .fltd, .dst0b, .src0d, .src1d },
                            } },
                        } },
                    });
                } else err: {
                    res[0] = ops[0].cmpInts(cmp_op, &ops[1], cg) catch |err| break :err err;
                }) catch |err| switch (err) {
                    error.SelectFailed => return cg.fail("failed to select {s} {} {} {}", .{
                        @tagName(air_tag),
                        cg.typeOf(bin_op.lhs).fmt(pt),
                        ops[0].tracking(cg),
                        ops[1].tracking(cg),
                    }),
                    else => |e| return e,
                };
                try res[0].finish(inst, &.{ bin_op.lhs, bin_op.rhs }, &ops, cg);
            },
            .cmp_eq,
            .cmp_eq_optimized,
            .cmp_neq,
            .cmp_neq_optimized,
            => |air_tag| {
                const bin_op = air_datas[@intFromEnum(inst)].bin_op;
                const cmp_op = air_tag.toCmpOp().?;
                var ops = try cg.tempsFromOperands(inst, .{ bin_op.lhs, bin_op.rhs });
                var res: [1]Temp = undefined;
                (if (cg.floatBits(cg.typeOf(bin_op.lhs))) |float_bits| err: {
                    if (float_bits != 32 and float_bits != 64)
                        return cg.fail("TODO: {s} float {d} bits", .{ @tagName(air_tag), float_bits });
                    cg.select(&res, &.{.bool}, &ops, comptime &.{ .{
                        .required_features = .{ .f, null, null, null },
                        .src_constraints = .{
                            .{ .float = .word },
                            .{ .float = .word },
                            .any,
                        },
                        .dst_temps = .{ .{ .rc = .int }, .unused },
                        .patterns = &.{
                            .{ .src = .{ .float_reg, .float_reg, .none } },
                            .{ .src = .{ .to_float_reg, .to_float_reg, .none } },
                        },
                        .each = .{ .once = &.{
                            .{ ._, .feqs, .dst0b, .src0w, .src1w },
                        } },
                    }, .{
                        .required_features = .{ .d, null, null, null },
                        .src_constraints = .{
                            .{ .float = .double },
                            .{ .float = .double },
                            .any,
                        },
                        .dst_temps = .{ .{ .rc = .int }, .unused },
                        .patterns = &.{
                            .{ .src = .{ .float_reg, .float_reg, .none } },
                            .{ .src = .{ .to_float_reg, .to_float_reg, .none } },
                        },
                        .each = .{ .once = &.{
                            .{ ._, .feqd, .dst0b, .src0d, .src1d },
                        } },
                    } }) catch |err| break :err err;
                    if (switch (air_tag) {
                        .cmp_eq, .cmp_eq_optimized => false,
                        .cmp_neq, .cmp_neq_optimized => true,
                        else => unreachable,
                    }) {
                        res[0].negate(cg) catch |err| break :err err;
                    }
                } else if (cg.intInfo(cg.typeOf(bin_op.lhs))) |int_info| err: {
                    _ = int_info;
                    res[0] = ops[0].cmpInts(cmp_op, &ops[1], cg) catch |err| break :err err;
                } else error.SelectFailed) catch |err| switch (err) {
                    error.SelectFailed => return cg.fail("failed to select {s} {} {} {}", .{
                        @tagName(air_tag),
                        cg.typeOf(bin_op.lhs).fmt(pt),
                        ops[0].tracking(cg),
                        ops[1].tracking(cg),
                    }),
                    else => |e| return e,
                };
                try res[0].finish(inst, &.{ bin_op.lhs, bin_op.rhs }, &ops, cg);
            },
            .min, .max => |air_tag| {
                const bin_op = air_datas[@intFromEnum(inst)].bin_op;
                var ops = try cg.tempsFromOperands(inst, .{ bin_op.lhs, bin_op.rhs });
                var res: [1]Temp = undefined;
                (if (cg.floatBits(cg.typeOf(bin_op.lhs))) |float_bits| err: {
                    if (float_bits != 32 and float_bits != 64)
                        return cg.fail("TODO: {s} float {d} bits", .{ @tagName(air_tag), float_bits });

                    break :err cg.select(&res, &.{cg.typeOf(bin_op.lhs)}, &ops, switch (air_tag) {
                        inline .min, .max => |comptime_tag| comptime &.{ .{
                            .required_features = .{ .f, null, null, null },
                            .src_constraints = .{
                                .{ .float = .word },
                                .{ .float = .word },
                                .any,
                            },
                            .dst_temps = .{ .{ .ref = .src0 }, .unused },
                            .patterns = &.{
                                .{ .src = .{ .float_reg, .float_reg, .none } },
                                .{ .src = .{ .to_float_reg, .to_float_reg, .none } },
                            },
                            .each = .{ .once = &.{
                                .{ ._, switch (comptime_tag) {
                                    .min => .fmins,
                                    .max => .fmaxs,
                                    else => unreachable,
                                }, .dst0b, .src0d, .src1d },
                            } },
                        }, .{
                            .required_features = .{ .d, null, null, null },
                            .src_constraints = .{
                                .{ .float = .double },
                                .{ .float = .double },
                                .any,
                            },
                            .dst_temps = .{ .{ .ref = .src0 }, .unused },
                            .patterns = &.{
                                .{ .src = .{ .float_reg, .float_reg, .none } },
                                .{ .src = .{ .to_float_reg, .to_float_reg, .none } },
                            },
                            .each = .{ .once = &.{
                                .{ ._, switch (comptime_tag) {
                                    .min => .fmind,
                                    .max => .fmaxd,
                                    else => unreachable,
                                }, .dst0b, .src0d, .src1d },
                            } },
                        } },
                        else => unreachable,
                    });
                } else if (cg.intInfo(cg.typeOf(bin_op.lhs))) |int_info| err: {
                    const signed = int_info.signedness == .signed;
                    break :err cg.select(&res, &.{cg.typeOf(bin_op.lhs)}, &ops, switch (air_tag) {
                        inline .min, .max => |comptime_tag| switch (signed) {
                            inline else => |is_signed| comptime &.{ .{
                                .required_features = .{ .zbb, null, null, null },
                                .src_constraints = .{
                                    .{ .int = .double },
                                    .{ .int = .double },
                                    .any,
                                },
                                .dst_temps = .{ .{ .ref = .src0 }, .unused },
                                .patterns = &.{
                                    .{ .src = .{ .mut_int_reg, .int_reg, .none } },
                                    .{ .src = .{ .to_mut_int_reg, .to_int_reg, .none } },
                                },
                                .each = .{ .once = &.{
                                    .{ ._, switch (comptime_tag) {
                                        .min => if (is_signed) .min else .minu,
                                        .max => if (is_signed) .max else .maxu,
                                        else => unreachable,
                                    }, .dst0d, .src0d, .src1d },
                                } },
                            }, .{
                                .src_constraints = .{
                                    .{ .int = .double },
                                    .{ .int = .double },
                                    .any,
                                },
                                .dst_temps = .{ .{ .rc = .int }, .unused },
                                .extra_temps = .{
                                    .{ .type = .u64, .kind = .{ .rc = .int } },
                                    .unused,
                                    .unused,
                                    .unused,
                                    .unused,
                                    .unused,
                                },
                                .patterns = &.{
                                    .{ .src = .{ .mut_int_reg, .int_reg, .none } },
                                    .{ .src = .{ .to_mut_int_reg, .to_int_reg, .none } },
                                },
                                .each = .{ .once = &.{
                                    .{ ._, if (is_signed) .slt else .sltu, .tmp0d, .src0d, .src1d },
                                    .{ ._, .sub, .tmp0d, .zero, .tmp0d },
                                    .{ ._, .xor, .dst0d, .src0d, .src1d },
                                    .{ ._, .@"and", .tmp0d, .dst0d, .tmp0d },
                                    .{ ._, .xor, .dst0d, switch (comptime_tag) {
                                        .min => .src1d,
                                        .max => .src0d,
                                        else => unreachable,
                                    }, .tmp0d },
                                } },
                            } },
                        },
                        else => unreachable,
                    });
                } else error.SelectFailed) catch |err| switch (err) {
                    error.SelectFailed => return cg.fail("failed to select {s} {} {} {}", .{
                        @tagName(air_tag),
                        cg.typeOf(bin_op.lhs).fmt(pt),
                        ops[0].tracking(cg),
                        ops[1].tracking(cg),
                    }),
                    else => |e| return e,
                };
                try res[0].finish(inst, &.{ bin_op.lhs, bin_op.rhs }, &ops, cg);
            },
            .clz => |air_tag| {
                const ty_op = air_datas[@intFromEnum(inst)].ty_op;
                const val_ty = ty_op.ty.toType();
                var ops = try cg.tempsFromOperands(inst, .{ty_op.operand});
                var res: [1]Temp = undefined;
                cg.select(&res, &.{val_ty}, &ops, comptime &.{ .{
                    .required_features = .{ .zbb, null, null, null },
                    .src_constraints = .{
                        .{ .signed_less_than_int = 12 },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .andi, .dst0w, .src0w, .sa(.src0, .add_umax) },
                        .{ ._, .clz, .dst0w, .dst0w, ._ },
                        .{ ._, .addi, .dst0w, .dst0w, .sia(-64, .src0, .bit_size) },
                    } },
                }, .{
                    .required_features = .{ .zbb, null, null, null },
                    .src_constraints = .{
                        .{ .exact_int = 32 },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .clzw, .dst0w, .src0w, ._ },
                    } },
                }, .{
                    .required_features = .{ .zbb, null, null, null },
                    .src_constraints = .{
                        .{ .unsigned_int = .double },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .clz, .dst0d, .src0d, ._ },
                        .{ ._, .addi, .dst0d, .dst0d, .sia(-64, .src0, .bit_size) },
                    } },
                }, .{
                    .required_features = .{ .zbb, null, null, null },
                    .src_constraints = .{
                        .{ .signed_int = .double },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .slli, .dst0d, .src0d, .uia(64, .src0, .sub_bit_size) },
                        .{ ._, .srli, .dst0d, .dst0d, .uia(64, .src0, .sub_bit_size) },
                        .{ ._, .clz, .dst0d, .dst0d, ._ },
                        .{ ._, .addi, .dst0d, .dst0d, .sia(-64, .src0, .bit_size) },
                    } },
                }, .{
                    .required_features = .{ .zbb, null, null, null },
                    .src_constraints = .{
                        .{ .exact_int = 64 },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .clz, .dst0d, .dst0d, ._ },
                    } },
                } }) catch |err| switch (err) {
                    error.SelectFailed => return cg.fail("failed to select {s} {} {}", .{
                        @tagName(air_tag),
                        cg.typeOf(ty_op.operand).fmt(pt),
                        ops[0].tracking(cg),
                    }),
                    else => |e| return e,
                };
                try res[0].finish(inst, &.{ty_op.operand}, &ops, cg);
            },
            .ctz => |air_tag| {
                const ty_op = air_datas[@intFromEnum(inst)].ty_op;
                const val_ty = ty_op.ty.toType();
                var ops = try cg.tempsFromOperands(inst, .{ty_op.operand});
                var res: [1]Temp = undefined;
                cg.select(&res, &.{val_ty}, &ops, comptime &.{ .{
                    .required_features = .{ .zbb, null, null, null },
                    .src_constraints = .{
                        .{ .exact_int = 32 },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .ctzw, .dst0w, .src0w, ._ },
                    } },
                }, .{
                    .required_features = .{ .zbb, null, null, null },
                    .src_constraints = .{
                        .{ .exact_int = 64 },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .extra_temps = .{
                        .{ .type = .u64, .kind = .{ .rc = .int } },
                        .unused,
                        .unused,
                        .unused,
                        .unused,
                        .unused,
                    },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .ctz, .dst0d, .src0d, ._ },
                    } },
                }, .{
                    .required_features = .{ .zbb, null, null, null },
                    .src_constraints = .{
                        .{ .int = .double },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .extra_temps = .{
                        .{ .type = .u64, .kind = .{ .rc = .int } },
                        .unused,
                        .unused,
                        .unused,
                        .unused,
                        .unused,
                    },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .addi, .tmp0d, .zero, .ui(1) },
                        .{ ._, .slli, .tmp0d, .tmp0d, .ua(.src0, .bit_size) },
                        .{ ._, .@"or", .dst0d, .src0d, .tmp0d },
                        .{ ._, .ctz, .dst0d, .src0d, ._ },
                    } },
                } }) catch |err| switch (err) {
                    error.SelectFailed => return cg.fail("failed to select {s} {} {}", .{
                        @tagName(air_tag),
                        cg.typeOf(ty_op.operand).fmt(pt),
                        ops[0].tracking(cg),
                    }),
                    else => |e| return e,
                };
                try res[0].finish(inst, &.{ty_op.operand}, &ops, cg);
            },
            .not => |air_tag| {
                const ty_op = air_datas[@intFromEnum(inst)].ty_op;
                const val_ty = ty_op.ty.toType();
                var ops = try cg.tempsFromOperands(inst, .{ty_op.operand});
                var res: [1]Temp = undefined;
                cg.select(&res, &.{val_ty}, &ops, comptime &.{ .{
                    .src_constraints = .{
                        .{ .signed_int = .double },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .xori, .dst0d, .src0d, .si(-1) },
                    } },
                }, .{
                    .src_constraints = .{
                        .{ .unsigned_less_than_int = 12 },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .xori, .dst0h, .src0h, .ua(.src0, .add_umax) },
                    } },
                }, .{
                    .src_constraints = .{
                        .{ .unsigned_less_than_int = 32 },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .extra_temps = .{
                        .{ .type = .u64, .kind = .{ .rc = .int } },
                        .unused,
                        .unused,
                        .unused,
                        .unused,
                        .unused,
                    },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{
                        .once = &.{
                            .{ ._, .lui, .tmp0d, .ua(.src0, .add_not_mask), ._ },
                            .{ ._, .addiw, .tmp0d, .tmp0d, .si(-1) },
                            .{ ._, .xor, .dst0d, .src0d, .tmp0d },
                        },
                    },
                }, .{
                    .src_constraints = .{
                        .{ .exact_unsigned_int = 64 },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .xori, .dst0d, .src0d, .si(-1) },
                    } },
                }, .{
                    .src_constraints = .{
                        .{ .unsigned_int = .double },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .extra_temps = .{
                        .{ .type = .u64, .kind = .{ .rc = .int } },
                        .unused,
                        .unused,
                        .unused,
                        .unused,
                        .unused,
                    },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .addi, .tmp0d, .zero, .si(-1) },
                        .{ ._, .srli, .tmp0d, .tmp0d, .uia(64, .src0, .sub_bit_size) },
                        .{ ._, .xor, .dst0d, .src0d, .tmp0d },
                    } },
                } }) catch |err| switch (err) {
                    error.SelectFailed => return cg.fail("failed to select {s} {} {}", .{
                        @tagName(air_tag),
                        cg.typeOf(ty_op.operand).fmt(pt),
                        ops[0].tracking(cg),
                    }),
                    else => |e| return e,
                };
                try res[0].finish(inst, &.{ty_op.operand}, &ops, cg);
            },
            .popcount => |air_tag| {
                const ty_op = air_datas[@intFromEnum(inst)].ty_op;
                const val_ty = ty_op.ty.toType();
                var ops = try cg.tempsFromOperands(inst, .{ty_op.operand});
                var res: [1]Temp = undefined;
                cg.select(&res, &.{val_ty}, &ops, comptime &.{ .{
                    .src_constraints = .{
                        .{ .exact_int = 1 },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .andi, .dst0w, .src0w, .ui(1) },
                    } },
                }, .{
                    .required_features = .{ .zbb, null, null, null },
                    .src_constraints = .{
                        .{ .less_than_int = 12 },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .andi, .dst0h, .src0h, .ua(.src0, .add_umax) },
                        .{ ._, .cpopw, .dst0w, .dst0h, ._ },
                    } },
                }, .{
                    .required_features = .{ .zbb, null, null, null },
                    .src_constraints = .{
                        .{ .exact_int = 32 },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .cpopw, .dst0w, .src0w, ._ },
                    } },
                }, .{
                    .required_features = .{ .zbb, null, null, null },
                    .src_constraints = .{
                        .{ .int = .word },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .slli, .dst0w, .src0w, .uia(64, .src0, .sub_bit_size) },
                        .{ ._, .srli, .dst0w, .dst0w, .uia(64, .src0, .sub_bit_size) },
                        .{ ._, .cpopw, .dst0w, .dst0w, ._ },
                    } },
                }, .{
                    .required_features = .{ .zbb, null, null, null },
                    .src_constraints = .{
                        .{ .exact_int = 64 },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .cpop, .dst0d, .src0d, ._ },
                    } },
                }, .{
                    .required_features = .{ .zbb, null, null, null },
                    .src_constraints = .{
                        .{ .int = .double },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .slli, .dst0d, .src0d, .uia(64, .src0, .sub_bit_size) },
                        .{ ._, .srli, .dst0d, .dst0d, .uia(64, .src0, .sub_bit_size) },
                        .{ ._, .cpop, .dst0d, .dst0d, ._ },
                    } },
                } }) catch |err| switch (err) {
                    error.SelectFailed => return cg.fail("failed to select {s} {} {}", .{
                        @tagName(air_tag),
                        cg.typeOf(ty_op.operand).fmt(pt),
                        ops[0].tracking(cg),
                    }),
                    else => |e| return e,
                };
                try res[0].finish(inst, &.{ty_op.operand}, &ops, cg);
            },
            .abs => |air_tag| {
                const ty_op = air_datas[@intFromEnum(inst)].ty_op;
                const val_ty = ty_op.ty.toType();
                var ops = try cg.tempsFromOperands(inst, .{ty_op.operand});
                var res: [1]Temp = undefined;
                cg.select(&res, &.{val_ty}, &ops, comptime &.{ .{
                    .src_constraints = .{
                        .{ .signed_int = .double },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .extra_temps = .{
                        .{ .type = .u64, .kind = .{ .rc = .int } },
                        .unused,
                        .unused,
                        .unused,
                        .unused,
                        .unused,
                    },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .sub, .tmp0d, .zero, .src0d },
                        .{ ._, .max, .dst0d, .src0d, .tmp0d },
                    } },
                }, .{
                    .src_constraints = .{
                        .{ .unsigned_int = .double },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{} },
                }, .{
                    .src_constraints = .{
                        .{ .unsigned_int = .double },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .none, .none } },
                        .{ .src = .{ .to_mut_int_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{} },
                }, .{
                    .required_features = .{ .f, null, null, null },
                    .src_constraints = .{
                        .{ .float = .word },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_float_reg, .none, .none } },
                        .{ .src = .{ .to_mut_float_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .fsgnjxs, .dst0w, .src0w, .src0w },
                    } },
                }, .{
                    .required_features = .{ .d, null, null, null },
                    .src_constraints = .{
                        .{ .float = .double },
                        .any,
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_float_reg, .none, .none } },
                        .{ .src = .{ .to_mut_float_reg, .none, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .fsgnjxd, .dst0d, .src0d, .src0d },
                    } },
                } }) catch |err| switch (err) {
                    error.SelectFailed => return cg.fail("failed to select {s} {} {}", .{
                        @tagName(air_tag),
                        cg.typeOf(ty_op.operand).fmt(pt),
                        ops[0].tracking(cg),
                    }),
                    else => |e| return e,
                };
                try res[0].finish(inst, &.{ty_op.operand}, &ops, cg);
            },
            .load => b: {
                if (true) break :b try cg.airLoad(inst);

                const ty_op = air_datas[@intFromEnum(inst)].ty_op;
                const val_ty = ty_op.ty.toType();
                const ptr_ty = cg.typeOf(ty_op.operand);
                const ptr_info = ptr_ty.ptrInfo(zcu);
                if (ptr_info.packed_offset.host_size > 0 and
                    (ptr_info.flags.vector_index == .none or val_ty.toIntern() == .bool_type))
                    @panic("TODO: bool vector load");
                var ops = try cg.tempsFromOperands(inst, .{ty_op.operand});
                const res = try ops[0].load(val_ty, .{
                    .disp = switch (ptr_info.flags.vector_index) {
                        .none => 0,
                        .runtime => unreachable,
                        else => |vector_index| @intCast(val_ty.abiSize(zcu) * @intFromEnum(vector_index)),
                    },
                }, cg);
                try res.finish(inst, &.{ty_op.operand}, &ops, cg);
            },
            .block => {
                const ty_pl = air_datas[@intFromEnum(inst)].ty_pl;
                const extra = cg.air.extraData(Air.Block, ty_pl.payload);
                try cg.lowerBlock(inst, @ptrCast(cg.air.extra[extra.end..][0..extra.data.body_len]));
            },
            .trap => _ = try cg.addInst(.{
                .tag = .unimp,
                .data = .none,
            }),
            .error_name => |air_tag| {
                if (true) return cg.fail("TODO: error_name", .{});

                const un_op = air_datas[@intFromEnum(inst)].un_op;
                var ops = try cg.tempsFromOperands(inst, .{un_op});
                var res: [2]Temp = undefined;
                cg.select(&res, &.{ .slice_const_u8_sentinel_0, .usize }, &ops, comptime &.{.{
                    .src_constraints = .{ .{ .int = .half }, .any, .any },
                    .patterns = &.{
                        .{ .src = .{ .mem, .none, .none } },
                        .{ .src = .{ .to_int_reg, .none, .none } },
                    },
                    .extra_temps = .{
                        .{ .type = .anyerror, .kind = .{ .lazy_symbol = .{ .kind = .const_data } } },
                        .{ .type = .u32, .kind = .{ .mut_rc = .{ .ref = .src0, .rc = .int } } },
                        .unused,
                        .unused,
                        .unused,
                        .unused,
                    },
                    .dst_temps = .{ .{ .rc = .int }, .{ .rc = .int } },
                    .each = .{ .once = &.{} },
                }}) catch |err| switch (err) {
                    error.SelectFailed => return cg.fail("failed to select {s} {} {}", .{
                        @tagName(air_tag),
                        cg.typeOf(un_op).fmt(pt),
                        ops[0].tracking(cg),
                    }),
                    else => |e| return e,
                };
                const orig_res = res;
                try res[0].toPair(&res[1], cg);
                for (&ops) |*op| for (orig_res) |orig_r| {
                    if (op.index != orig_r.index) continue;
                    op.* = res[0];
                    break;
                };
                try res[0].finish(inst, &.{un_op}, &ops, cg);
            },
            .int_from_float, .int_from_float_optimized => |air_tag| {
                const ty_op = air_datas[@intFromEnum(inst)].ty_op;
                var ops = try cg.tempsFromOperands(inst, .{ty_op.operand});
                var res: [1]Temp = undefined;
                cg.select(&res, &.{ty_op.ty.toType()}, &ops, comptime &.{.{
                    .required_features = .{ .f, null, null, null },
                    .src_constraints = .{ .{ .float = .word }, .any, .any },
                    .dst_constraints = .{ .{ .int = .half }, .any },
                    .patterns = &.{
                        .{ .src = .{ .to_float_reg, .none, .none } },
                    },
                    .dst_temps = .{ .{ .rc = .int }, .unused },
                    .each = .{ .once = &.{
                        .{ ._, .fcvtlus, .dst0h, .src0w, ._ },
                    } },
                }}) catch |err| switch (err) {
                    error.SelectFailed => return cg.fail("failed to select {s} {} {} {}", .{
                        @tagName(air_tag),
                        ty_op.ty.toType().fmt(pt),
                        cg.typeOf(ty_op.operand).fmt(pt),
                        ops[0].tracking(cg),
                    }),
                    else => |e| return e,
                };
                try res[0].finish(inst, &.{ty_op.operand}, &ops, cg);
            },
            // zig fmt: off
                        
            .ptr_add,
            .ptr_sub => try cg.airPtrArithmetic(inst, tag),

            .mod,
            .div_float, 
            .div_floor, 
            => return cg.fail("TODO: {s}", .{@tagName(tag)}),

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
            => try cg.airUnaryMath(inst, tag),

            .add_with_overflow => try cg.airAddWithOverflow(inst),
            .sub_with_overflow => try cg.airSubWithOverflow(inst),
            .mul_with_overflow => try cg.airMulWithOverflow(inst),
            .shl_with_overflow => try cg.airShlWithOverflow(inst),

            .sub_sat         => try cg.airSubSat(inst),
            .mul_sat         => try cg.airMulSat(inst),
            .shl_sat         => try cg.airShlSat(inst),

            .add_safe,
            .sub_safe,
            .mul_safe,
            .intcast_safe,
            => return cg.fail("TODO implement safety_checked_instructions", .{}),

            .cmp_vector => try cg.airCmpVector(inst),
            .cmp_lt_errors_len => try cg.airCmpLtErrorsLen(inst),

            .slice           => try cg.airSlice(inst),
            .array_to_slice  => try cg.airArrayToSlice(inst),

            .slice_ptr       => try cg.airSlicePtr(inst),
            .slice_len       => try cg.airSliceLen(inst),

            .alloc           => try cg.airAlloc(inst),
            .ret_ptr         => try cg.airRetPtr(inst),
            .arg             => {},
            .assembly        => try cg.airAsm(inst),
            .bitcast         => try cg.airBitCast(inst),
            .br              => try cg.airBr(inst),
            .repeat          => try cg.airRepeat(inst),
            .switch_dispatch => try cg.airSwitchDispatch(inst),
            .breakpoint      => try cg.airBreakpoint(),
            .ret_addr        => try cg.airRetAddr(inst),
            .frame_addr      => try cg.airFrameAddress(inst),
            .cond_br         => try cg.airCondBr(inst),
            .dbg_stmt        => try cg.airDbgStmt(inst),
            .dbg_empty_stmt  => {},
            .fptrunc         => try cg.airFptrunc(inst),
            .fpext           => try cg.airFpext(inst),
            .intcast         => try cg.airIntCast(inst),
            .trunc           => try cg.airTrunc(inst),
            .is_non_null     => try cg.airIsNonNull(inst),
            .is_non_null_ptr => try cg.airIsNonNullPtr(inst),
            .is_null         => try cg.airIsNull(inst),
            .is_null_ptr     => try cg.airIsNullPtr(inst),
            .is_non_err      => try cg.airIsNonErr(inst),
            .is_non_err_ptr  => try cg.airIsNonErrPtr(inst),
            .is_err          => try cg.airIsErr(inst),
            .is_err_ptr      => try cg.airIsErrPtr(inst),
            .loop            => try cg.airLoop(inst),
            .ret             => try cg.airRet(inst, false),
            .ret_safe        => try cg.airRet(inst, true),
            .ret_load        => try cg.airRetLoad(inst),
            .store           => try cg.airStore(inst, false),
            .store_safe      => try cg.airStore(inst, true),
            .struct_field_ptr=> try cg.airStructFieldPtr(inst),
            .struct_field_val=> try cg.airStructFieldVal(inst),
            .float_from_int  => try cg.airFloatFromInt(inst),
            .cmpxchg_strong  => try cg.airCmpxchg(inst, .strong),
            .cmpxchg_weak    => try cg.airCmpxchg(inst, .weak),
            .atomic_rmw      => try cg.airAtomicRmw(inst),
            .atomic_load     => try cg.airAtomicLoad(inst),
            .memcpy          => try cg.airMemcpy(inst),
            .memset          => try cg.airMemset(inst, false),
            .memset_safe     => try cg.airMemset(inst, true),
            .set_union_tag   => try cg.airSetUnionTag(inst),
            .get_union_tag   => try cg.airGetUnionTag(inst),
            .byte_swap       => try cg.airByteSwap(inst),
            .bit_reverse     => try cg.airBitReverse(inst),
            .tag_name        => try cg.airTagName(inst),
            .splat           => try cg.airSplat(inst),
            .select          => try cg.airSelect(inst),
            .shuffle         => try cg.airShuffle(inst),
            .reduce          => try cg.airReduce(inst),
            .aggregate_init  => try cg.airAggregateInit(inst),
            .union_init      => try cg.airUnionInit(inst),
            .prefetch        => try cg.airPrefetch(inst),
            .mul_add         => try cg.airMulAdd(inst),
            .addrspace_cast  => return cg.fail("TODO: addrspace_cast", .{}),

            .@"try"          =>  try cg.airTry(inst),
            .try_cold        =>  try cg.airTry(inst),
            .try_ptr         =>  return cg.fail("TODO: try_ptr", .{}),
            .try_ptr_cold    =>  return cg.fail("TODO: try_ptr_cold", .{}),

            .dbg_var_ptr,
            .dbg_var_val,
            .dbg_arg_inline,
            => try cg.airDbgVar(inst),

            .dbg_inline_block => try cg.airDbgInlineBlock(inst),

            .call              => try cg.airCall(inst, .auto),
            .call_always_tail  => try cg.airCall(inst, .always_tail),
            .call_never_tail   => try cg.airCall(inst, .never_tail),
            .call_never_inline => try cg.airCall(inst, .never_inline),

            .atomic_store_unordered => try cg.airAtomicStore(inst, .unordered),
            .atomic_store_monotonic => try cg.airAtomicStore(inst, .monotonic),
            .atomic_store_release   => try cg.airAtomicStore(inst, .release),
            .atomic_store_seq_cst   => try cg.airAtomicStore(inst, .seq_cst),
            .struct_field_ptr_index_0 => try cg.airStructFieldPtrIndex(inst, 0),
            .struct_field_ptr_index_1 => try cg.airStructFieldPtrIndex(inst, 1),
            .struct_field_ptr_index_2 => try cg.airStructFieldPtrIndex(inst, 2),
            .struct_field_ptr_index_3 => try cg.airStructFieldPtrIndex(inst, 3),

            .field_parent_ptr => try cg.airFieldParentPtr(inst),

            .switch_br       => try cg.airSwitchBr(inst),
            .loop_switch_br  => try cg.airLoopSwitchBr(inst),

            .ptr_slice_len_ptr => try cg.airPtrSliceLenPtr(inst),
            .ptr_slice_ptr_ptr => try cg.airPtrSlicePtrPtr(inst),

            .array_elem_val      => try cg.airArrayElemVal(inst),
            
            .slice_elem_val      => try cg.airSliceElemVal(inst),
            .slice_elem_ptr      => try cg.airSliceElemPtr(inst),

            .ptr_elem_val        => try cg.airPtrElemVal(inst),
            .ptr_elem_ptr        => try cg.airPtrElemPtr(inst),

            .inferred_alloc, .inferred_alloc_comptime => unreachable,
            .unreach  => {},

            .optional_payload           => try cg.airOptionalPayload(inst),
            .optional_payload_ptr       => try cg.airOptionalPayloadPtr(inst),
            .optional_payload_ptr_set   => try cg.airOptionalPayloadPtrSet(inst),
            .unwrap_errunion_err        => try cg.airUnwrapErrErr(inst),
            .unwrap_errunion_payload    => try cg.airUnwrapErrPayload(inst),
            .unwrap_errunion_err_ptr    => try cg.airUnwrapErrErrPtr(inst),
            .unwrap_errunion_payload_ptr=> try cg.airUnwrapErrPayloadPtr(inst),
            .errunion_payload_ptr_set   => try cg.airErrUnionPayloadPtrSet(inst),
            .err_return_trace           => try cg.airErrReturnTrace(inst),
            .set_err_return_trace       => try cg.airSetErrReturnTrace(inst),
            .save_err_return_trace_index=> try cg.airSaveErrReturnTraceIndex(inst),

            .wrap_optional         => try cg.airWrapOptional(inst),
            .wrap_errunion_payload => try cg.airWrapErrUnionPayload(inst),
            .wrap_errunion_err     => try cg.airWrapErrUnionErr(inst),

            
            .div_float_optimized,
            .div_trunc_optimized,
            .div_floor_optimized,
            .div_exact_optimized,
            .rem_optimized,
            .mod_optimized,
            .neg_optimized,
            .cmp_vector_optimized,
            .reduce_optimized,
            => return cg.fail("TODO implement optimized float mode", .{}),

            .is_named_enum_value => return cg.fail("TODO implement is_named_enum_value", .{}),
            .error_set_has_value => return cg.fail("TODO implement error_set_has_value", .{}),
            .vector_store_elem => return cg.fail("TODO implement vector_store_elem", .{}),

            .c_va_arg => return cg.fail("TODO implement c_va_arg", .{}),
            .c_va_copy => return cg.fail("TODO implement c_va_copy", .{}),
            .c_va_end => return cg.fail("TODO implement c_va_end", .{}),
            .c_va_start => return cg.fail("TODO implement c_va_start", .{}),

            .wasm_memory_size => unreachable,
            .wasm_memory_grow => unreachable,

            .work_item_id => unreachable,
            .work_group_size => unreachable,
            .work_group_id => unreachable,
            // zig fmt: on
        }

        try cg.resetTemps();
        cg.checkInvariantsAfterAirInst();
    }
    verbose_tracking_log.debug("{}", .{cg.fmtTracking()});
}

fn checkInvariantsAfterAirInst(cg: *CodeGen) void {
    assert(!cg.register_manager.lockedRegsExist());

    if (std.debug.runtime_safety) {
        // check consistency of tracked registers
        var it = cg.register_manager.free_registers.iterator(.{ .kind = .unset });
        while (it.next()) |index| {
            const tracked_inst = cg.register_manager.registers[index];
            const tracking = cg.getResolvedInstValue(tracked_inst);
            for (tracking.getRegs()) |reg| {
                if (RegisterManager.indexOfRegIntoTracked(reg).? == index) break;
            } else return std.debug.panic(
                \\{} {} takes up these regs: {any}, however this reg {any}, don't use it
            , .{
                tracked_inst,
                tracking,
                tracking.getRegs(),
                RegisterManager.regAtTrackedIndex(@intCast(index)),
            });
        }
    }
}

fn getValue(cg: *CodeGen, value: MCValue, inst: ?Air.Inst.Index) !void {
    for (value.getRegs()) |reg| try cg.register_manager.getReg(reg, inst);
}

fn getValueIfFree(cg: *CodeGen, value: MCValue, inst: ?Air.Inst.Index) void {
    for (value.getRegs()) |reg| if (cg.register_manager.isRegFree(reg))
        cg.register_manager.getRegAssumeFree(reg, inst);
}

fn freeValue(cg: *CodeGen, value: MCValue) !void {
    switch (value) {
        .register => |reg| cg.register_manager.freeReg(reg),
        .register_pair => |regs| for (regs) |reg| cg.register_manager.freeReg(reg),
        .register_offset => |reg_off| cg.register_manager.freeReg(reg_off.reg),
        else => {}, // TODO process stack allocation death
    }
}

fn feed(cg: *CodeGen, bt: *Liveness.BigTomb, operand: Air.Inst.Ref) !void {
    if (bt.feed()) if (operand.toIndex()) |inst| {
        log.debug("feed inst: %{}", .{inst});
        try cg.processDeath(inst);
    };
}

/// Asserts there is already capacity to insert into top branch inst_table.
fn processDeath(cg: *CodeGen, inst: Air.Inst.Index) !void {
    try cg.inst_tracking.getPtr(inst).?.die(cg, inst);
}

fn finishAirResult(cg: *CodeGen, inst: Air.Inst.Index, result: MCValue) void {
    if (cg.liveness.isUnused(inst) and cg.air.instructions.items(.tag)[@intFromEnum(inst)] != .arg) switch (result) {
        .none, .dead, .unreach => {},
        else => unreachable, // Why didn't the result die?
    } else {
        tracking_log.debug("{} => {} (birth)", .{ inst, result });
        cg.inst_tracking.putAssumeCapacityNoClobber(inst, .init(result));
        // In some cases, an operand may be reused as the result.
        // If that operand died and was a register, it was freed by
        // processDeath, so we have to "re-allocate" the register.
        cg.getValueIfFree(result, inst);
    }
}

fn finishAir(
    cg: *CodeGen,
    inst: Air.Inst.Index,
    result: MCValue,
    operands: [Liveness.bpi - 1]Air.Inst.Ref,
) !void {
    const tomb_bits = cg.liveness.getTombBits(inst);
    for (0.., operands) |op_index, op| {
        if (tomb_bits & @as(Liveness.Bpi, 1) << @intCast(op_index) == 0) continue;
        if (cg.reused_operands.isSet(op_index)) continue;
        try cg.processDeath(op.toIndexAllowNone() orelse continue);
    }
    cg.finishAirResult(inst, result);
}

const FrameLayout = struct {
    stack_adjust: i32,
    need_stack_adjust: bool,
    save_reg_list: Mir.RegisterList,
};

fn setFrameLoc(
    cg: *CodeGen,
    frame_index: FrameIndex,
    base: Register,
    offset: *i32,
    comptime aligned: bool,
) void {
    const frame_i = @intFromEnum(frame_index);
    if (aligned) {
        const alignment: InternPool.Alignment = cg.frame_allocs.items(.abi_align)[frame_i];
        offset.* = math.sign(offset.*) * @as(i32, @intCast(alignment.backward(@intCast(@abs(offset.*)))));
    }
    cg.frame_locs.set(frame_i, .{ .base = base, .disp = offset.* });
    offset.* += cg.frame_allocs.items(.abi_size)[frame_i];
}

fn computeFrameLayout(cg: *CodeGen) !FrameLayout {
    const frame_allocs_len = cg.frame_allocs.len;
    try cg.frame_locs.resize(cg.gpa, frame_allocs_len);
    const stack_frame_order = try cg.gpa.alloc(FrameIndex, frame_allocs_len - FrameIndex.named_count);
    defer cg.gpa.free(stack_frame_order);

    const frame_size = cg.frame_allocs.items(.abi_size);
    const frame_align = cg.frame_allocs.items(.abi_align);

    for (stack_frame_order, FrameIndex.named_count..) |*frame_order, frame_index|
        frame_order.* = @enumFromInt(frame_index);

    {
        const SortContext = struct {
            frame_align: @TypeOf(frame_align),
            pub fn lessThan(context: @This(), lhs: FrameIndex, rhs: FrameIndex) bool {
                return context.frame_align[@intFromEnum(lhs)].compare(.gt, context.frame_align[@intFromEnum(rhs)]);
            }
        };
        const sort_context = SortContext{ .frame_align = frame_align };
        mem.sort(FrameIndex, stack_frame_order, sort_context, SortContext.lessThan);
    }

    var save_reg_list = Mir.RegisterList{};
    for (abi.Registers.all_callee_preserved) |reg| {
        if (cg.register_manager.isRegAllocated(reg)) {
            save_reg_list.push(&abi.Registers.all_callee_preserved, reg);
        }
    }

    const total_alloc_size: i32 = blk: {
        var i: i32 = 0;
        for (stack_frame_order) |frame_index| {
            i += frame_size[@intFromEnum(frame_index)];
        }
        break :blk i;
    };

    const saved_reg_size = save_reg_list.size();
    frame_size[@intFromEnum(FrameIndex.spill_frame)] = @intCast(saved_reg_size);

    // The total frame size is calculated by the amount of s registers you need to save * 8, as each
    // register is 8 bytes, the total allocation sizes, and 16 more register for the spilled ra and s0
    // register. Finally we align the frame size to the alignment of the base pointer.
    const args_frame_size = frame_size[@intFromEnum(FrameIndex.args_frame)];
    const spill_frame_size = frame_size[@intFromEnum(FrameIndex.spill_frame)];
    const call_frame_size = frame_size[@intFromEnum(FrameIndex.call_frame)];

    const stack_adjust = total_alloc_size + args_frame_size + spill_frame_size + call_frame_size;
    const acc_frame_size: i32 = std.mem.alignForward(
        i32,
        64 + stack_adjust,
        @intCast(frame_align[@intFromEnum(FrameIndex.base_ptr)].toByteUnits().?),
    );
    log.debug("frame size: {}", .{stack_adjust});

    // store the ra at total_size - 8, so it's the very first thing in the stack
    // relative to the fp
    cg.frame_locs.set(
        @intFromEnum(FrameIndex.ret_addr),
        .{ .base = .sp, .disp = acc_frame_size - 8 },
    );
    cg.frame_locs.set(
        @intFromEnum(FrameIndex.base_ptr),
        .{ .base = .sp, .disp = acc_frame_size - 16 },
    );

    // now we grow the stack frame from the bottom of total frame in order to
    // not need to know the size of the first allocation. Stack offsets point at the "bottom"
    // of variables.
    var s0_offset: i32 = -acc_frame_size;
    cg.setFrameLoc(.stack_frame, .s0, &s0_offset, true);
    for (stack_frame_order) |frame_index| cg.setFrameLoc(frame_index, .s0, &s0_offset, true);
    cg.setFrameLoc(.args_frame, .s0, &s0_offset, true);
    cg.setFrameLoc(.call_frame, .s0, &s0_offset, true);
    cg.setFrameLoc(.spill_frame, .s0, &s0_offset, true);

    return .{
        .stack_adjust = @intCast(acc_frame_size),
        .need_stack_adjust = stack_adjust != 0,
        .save_reg_list = save_reg_list,
    };
}

fn memSize(cg: *CodeGen, ty: Type) Memory.Size {
    const pt = cg.pt;
    const zcu = pt.zcu;
    return switch (ty.zigTypeTag(zcu)) {
        .float => Memory.Size.fromBitSize(ty.floatBits(cg.target.*)),
        else => Memory.Size.fromByteSize(ty.abiSize(zcu)),
    };
}

fn splitType(cg: *CodeGen, ty: Type) ![2]Type {
    const zcu = cg.pt.zcu;
    const classes = mem.sliceTo(&abi.classifyCallingConvention(ty, zcu), .none);
    var parts: [2]Type = undefined;
    if (classes.len == 2) for (&parts, classes, 0..) |*part, class, part_i| {
        part.* = switch (class) {
            .integer => switch (part_i) {
                0 => Type.u64,
                1 => part: {
                    const elem_size = ty.abiAlignment(zcu).minStrict(.@"8").toByteUnits().?;
                    const elem_ty = try cg.pt.intType(.unsigned, @intCast(elem_size * 8));
                    break :part switch (@divExact(ty.abiSize(zcu) - 8, elem_size)) {
                        1 => elem_ty,
                        else => |len| try cg.pt.arrayType(.{ .len = len, .child = elem_ty.toIntern() }),
                    };
                },
                else => unreachable,
            },
            else => return cg.fail("TODO: splitType class {}", .{class}),
        };
    } else if (parts[0].abiSize(zcu) + parts[1].abiSize(zcu) == ty.abiSize(zcu)) return parts;
    return cg.fail("TODO implement splitType for {}, got classes {any}", .{ ty.fmt(cg.pt), classes });
}

/// Truncates the value in the register in place.
/// Clobbers any remaining bits.
fn truncateRegister(cg: *CodeGen, ty: Type, reg: Register) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const int_info = if (ty.isAbiInt(zcu)) ty.intInfo(zcu) else std.builtin.Type.Int{
        .signedness = .unsigned,
        .bits = @intCast(ty.bitSize(zcu)),
    };
    assert(reg.class() == .int);

    const shift = math.cast(u6, 64 - int_info.bits % 64) orelse return;
    switch (int_info.signedness) {
        .signed => {
            try cg.asmIType(.slli, reg, reg, .u(shift));
            try cg.asmIType(.srai, reg, reg, .u(shift));
        },
        .unsigned => {
            const mask = ~@as(u64, 0) >> shift;
            if (mask < 256) {
                try cg.asmIType(.andi, reg, reg, .u(mask));
            } else {
                try cg.asmIType(.slli, reg, reg, .u(shift));
                try cg.asmIType(.srli, reg, reg, .u(shift));
            }
        },
    }
}

fn allocFrameIndex(cg: *CodeGen, alloc: FrameAlloc) !FrameIndex {
    const frame_allocs_slice = cg.frame_allocs.slice();
    const frame_size = frame_allocs_slice.items(.abi_size);
    const frame_align = frame_allocs_slice.items(.abi_align);

    const stack_frame_align = &frame_align[@intFromEnum(FrameIndex.stack_frame)];
    stack_frame_align.* = stack_frame_align.max(alloc.abi_align);

    for (cg.free_frame_indices.keys(), 0..) |frame_index, free_i| {
        const abi_size = frame_size[@intFromEnum(frame_index)];
        if (abi_size != alloc.abi_size) continue;
        const abi_align = &frame_align[@intFromEnum(frame_index)];
        abi_align.* = abi_align.max(alloc.abi_align);

        _ = cg.free_frame_indices.swapRemoveAt(free_i);
        return frame_index;
    }
    const frame_index: FrameIndex = @enumFromInt(cg.frame_allocs.len);
    try cg.frame_allocs.append(cg.gpa, alloc);
    log.debug("allocated frame {}", .{frame_index});
    return frame_index;
}

/// Use a pointer instruction as the basis for allocating stack memory.
fn allocMemPtr(cg: *CodeGen, inst: Air.Inst.Index) !FrameIndex {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const ptr_ty = cg.typeOfIndex(inst);
    const val_ty = ptr_ty.childType(zcu);
    return cg.allocFrameIndex(FrameAlloc.init(.{
        .size = math.cast(u32, val_ty.abiSize(zcu)) orelse {
            return cg.fail("type '{}' too big to fit into stack frame", .{val_ty.fmt(pt)});
        },
        .alignment = ptr_ty.ptrAlignment(zcu).max(.@"1"),
    }));
}

fn typeRegClass(cg: *CodeGen, ty: Type) abi.RegisterClass {
    const pt = cg.pt;
    const zcu = pt.zcu;
    return switch (ty.zigTypeTag(zcu)) {
        .float => .float,
        .vector => .vector,
        else => .int,
    };
}

fn regSetForRegClass(rc: abi.RegisterClass) RegisterManager.RegisterBitSet {
    return switch (rc) {
        .int => abi.Registers.Integer.general_purpose,
        .float => abi.Registers.Float.general_purpose,
        .vector => abi.Registers.Vector.general_purpose,
    };
}

fn regGeneralClassForType(cg: *CodeGen, ty: Type) RegisterManager.RegisterBitSet {
    return switch (ty.zigTypeTag(cg.pt.zcu)) {
        .float => abi.Registers.Float.general_purpose,
        .vector => abi.Registers.Vector.general_purpose,
        else => abi.Registers.Integer.general_purpose,
    };
}

fn regTempClassForType(cg: *CodeGen, ty: Type) RegisterManager.RegisterBitSet {
    return switch (ty.zigTypeTag(cg.pt.zcu)) {
        .float => abi.Registers.Float.temporary,
        .vector => abi.Registers.Vector.general_purpose, // there are no temporary vector registers
        else => abi.Registers.Integer.temporary,
    };
}

fn allocRegOrMem(cg: *CodeGen, elem_ty: Type, inst: ?Air.Inst.Index, reg_ok: bool) !MCValue {
    const pt = cg.pt;
    const zcu = pt.zcu;

    const bit_size = elem_ty.bitSize(zcu);
    const min_size: u64 = switch (elem_ty.zigTypeTag(zcu)) {
        .float => if (cg.hasFeature(.d)) 64 else 32,
        .vector => cg.vectorBits(),
        else => 64,
    };

    if (reg_ok and bit_size <= min_size) {
        if (cg.register_manager.tryAllocReg(inst, cg.regGeneralClassForType(elem_ty))) |reg| {
            return .{ .register = reg };
        }
    }

    const frame_index = try cg.allocFrameIndex(FrameAlloc.initSpill(elem_ty, zcu));
    return .{ .load_frame = .{ .index = frame_index } };
}

/// Allocates a register from the general purpose set and returns the Register and the Lock.
///
/// Up to the caller to unlock the register later.
fn allocReg(cg: *CodeGen, reg_class: abi.RegisterClass) !struct { Register, RegisterLock } {
    if (reg_class == .float and !cg.hasFeature(.f))
        std.debug.panic("allocReg class == float where F isn't enabled", .{});
    if (reg_class == .vector and !cg.hasFeature(.v))
        std.debug.panic("allocReg class == vector where V isn't enabled", .{});

    const class = switch (reg_class) {
        .int => abi.Registers.Integer.general_purpose,
        .float => abi.Registers.Float.general_purpose,
        .vector => abi.Registers.Vector.general_purpose,
    };

    const reg = try cg.register_manager.allocReg(null, class);
    const lock = cg.register_manager.lockRegAssumeUnused(reg);
    return .{ reg, lock };
}

/// Used when register allocating a type which requires a length multiplier of
/// of more than `m1`.
///
/// This function will allocate enough vector registers to store `ty`, but will return
/// only the first one, as that's the one that should be addressed in instructions.
///
/// Assumes that `vtype` was setup before calling this function. Changing `vlmul` before
/// unlocking these registers will lead to unexpected results.
fn allocVecReg(cg: *CodeGen, ty: Type) !struct { Register, []const RegisterLock } {
    assert(cg.typeRegClass(ty) == .vector);
    const zcu = cg.pt.zcu;

    const vb = cg.vectorBits();
    const ty_bits = ty.bitSize(zcu);
    const num_regs = math.divCeil(u32, @intCast(ty_bits), vb) catch unreachable;

    var locks = std.ArrayList(RegisterLock).init(cg.gpa);
    const base_reg = try cg.register_manager.allocReg(null, abi.Registers.Vector.general_purpose);
    for (0..num_regs) |i| {
        const next_reg: Register = @enumFromInt(@intFromEnum(base_reg) + i);
        try cg.register_manager.getReg(next_reg, null);
        const lock = cg.register_manager.lockRegAssumeUnused(next_reg);
        try locks.append(lock);
    }

    return .{ base_reg, try locks.toOwnedSlice() };
}

/// Similar to `allocReg` but will copy the MCValue into the Register unless `operand` is already
/// a register, in which case it will return a possible lock to that register.
fn promoteReg(cg: *CodeGen, ty: Type, operand: MCValue) !struct { Register, ?RegisterLock } {
    if (operand == .register) {
        const op_reg = operand.register;
        return .{ op_reg, cg.register_manager.lockReg(operand.register) };
    }

    const class = cg.typeRegClass(ty);
    const reg, const lock = try cg.allocReg(class);
    try cg.genSetReg(ty, reg, operand);
    return .{ reg, lock };
}

fn elemOffset(cg: *CodeGen, index_ty: Type, index: MCValue, elem_size: u64) !Register {
    const reg: Register = blk: {
        switch (index) {
            .immediate => |imm| {
                // Optimisation: if index MCValue is an immediate, we can multiply in `comptime`
                // and set the register directly to the scaled offset as an immediate.
                const reg = try cg.register_manager.allocReg(null, cg.regGeneralClassForType(index_ty));
                try cg.genSetReg(index_ty, reg, .{ .immediate = imm * elem_size });
                break :blk reg;
            },
            else => {
                const reg = try cg.copyToTmpRegister(index_ty, index);
                const lock = cg.register_manager.lockRegAssumeUnused(reg);
                defer cg.register_manager.unlockReg(lock);

                const result_reg, const result_lock = try cg.allocReg(.int);
                defer cg.register_manager.unlockReg(result_lock);

                try cg.genBinOp(
                    .mul,
                    .{ .register = reg },
                    index_ty,
                    .{ .immediate = elem_size },
                    index_ty,
                    result_reg,
                );

                break :blk result_reg;
            },
        }
    };
    return reg;
}

pub fn spillInstruction(cg: *CodeGen, reg: Register, inst: Air.Inst.Index) !void {
    const tracking = cg.inst_tracking.getPtr(inst) orelse return;
    for (tracking.getRegs()) |tracked_reg| {
        if (tracked_reg.id() == reg.id()) break;
    } else unreachable; // spilled reg not tracked with spilled instruciton
    try tracking.spill(cg, inst);
    try tracking.trackSpill(cg, inst);
}

pub fn spillRegisters(cg: *CodeGen, comptime registers: []const Register) !void {
    inline for (registers) |reg| try cg.register_manager.getKnownReg(reg, null);
}

/// Copies a value to a register without tracking the register. The register is not considered
/// allocated. A second call to `copyToTmpRegister` may return the same register.
/// This can have a side effect of spilling instructions to the stack to free up a register.
fn copyToTmpRegister(cg: *CodeGen, ty: Type, mcv: MCValue) !Register {
    log.debug("copyToTmpRegister ty: {}", .{ty.fmt(cg.pt)});
    const reg = try cg.register_manager.allocReg(null, cg.regTempClassForType(ty));
    try cg.genSetReg(ty, reg, mcv);
    return reg;
}

/// Allocates a new register and copies `mcv` into it.
/// `reg_owner` is the instruction that gets associated with the register in the register table.
/// This can have a side effect of spilling instructions to the stack to free up a register.
fn copyToNewRegister(cg: *CodeGen, reg_owner: Air.Inst.Index, mcv: MCValue) !MCValue {
    const ty = cg.typeOfIndex(reg_owner);
    const reg = try cg.register_manager.allocReg(reg_owner, cg.regGeneralClassForType(ty));
    try cg.genSetReg(cg.typeOfIndex(reg_owner), reg, mcv);
    return MCValue{ .register = reg };
}

fn airAlloc(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const result = MCValue{ .lea_frame = .{ .index = try cg.allocMemPtr(inst) } };
    return cg.finishAir(inst, result, .{ .none, .none, .none });
}

fn airRetPtr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const result: MCValue = switch (cg.ret_mcv.long) {
        .none => .{ .lea_frame = .{ .index = try cg.allocMemPtr(inst) } },
        .load_frame => .{ .register_offset = .{
            .reg = (try cg.copyToNewRegister(
                inst,
                cg.ret_mcv.long,
            )).register,
            .off = cg.ret_mcv.short.indirect.off,
        } },
        else => |t| return cg.fail("TODO: airRetPtr {s}", .{@tagName(t)}),
    };
    return cg.finishAir(inst, result, .{ .none, .none, .none });
}

fn airFptrunc(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else return cg.fail("TODO implement airFptrunc for {}", .{cg.target.cpu.arch});
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn airFpext(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else return cg.fail("TODO implement airFpext for {}", .{cg.target.cpu.arch});
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn airIntCast(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const src_ty = cg.typeOf(ty_op.operand);
    const dst_ty = cg.typeOfIndex(inst);

    const result: MCValue = result: {
        const src_int_info = src_ty.intInfo(zcu);
        const dst_int_info = dst_ty.intInfo(zcu);

        const min_ty = if (dst_int_info.bits < src_int_info.bits) dst_ty else src_ty;

        const src_mcv = try cg.resolveInst(ty_op.operand);

        const src_storage_bits: u16 = switch (src_mcv) {
            .register => 64,
            .load_frame => src_int_info.bits,
            else => return cg.fail("airIntCast from {s}", .{@tagName(src_mcv)}),
        };

        const dst_mcv = if (dst_int_info.bits <= src_storage_bits and
            math.divCeil(u16, dst_int_info.bits, 64) catch unreachable ==
                math.divCeil(u32, src_storage_bits, 64) catch unreachable and
            cg.reuseOperand(inst, ty_op.operand, 0, src_mcv)) src_mcv else dst: {
            const dst_mcv = try cg.allocRegOrMem(dst_ty, inst, true);
            try cg.genCopy(min_ty, dst_mcv, src_mcv);
            break :dst dst_mcv;
        };

        if (dst_int_info.bits <= src_int_info.bits)
            break :result dst_mcv;

        if (dst_int_info.bits > 64 or src_int_info.bits > 64)
            break :result null; // TODO

        break :result dst_mcv;
    } orelse return cg.fail("TODO: implement airIntCast from {} to {}", .{
        src_ty.fmt(pt), dst_ty.fmt(pt),
    });

    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn airTrunc(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    if (cg.liveness.isUnused(inst))
        return cg.finishAir(inst, .unreach, .{ ty_op.operand, .none, .none });
    // we assume no zeroext in the "Zig ABI", so it's fine to just not truncate it.
    const operand = try cg.resolveInst(ty_op.operand);

    // we can do it just to be safe, but this shouldn't be needed for no-runtime safety modes
    switch (operand) {
        .register => |reg| try cg.truncateRegister(cg.typeOf(ty_op.operand), reg),
        else => {},
    }

    return cg.finishAir(inst, operand, .{ ty_op.operand, .none, .none });
}

fn airSlice(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const ty_pl = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_pl;
    const bin_op = cg.air.extraData(Air.Bin, ty_pl.payload).data;

    const slice_ty = cg.typeOfIndex(inst);
    const frame_index = try cg.allocFrameIndex(FrameAlloc.initSpill(slice_ty, zcu));

    const ptr_ty = cg.typeOf(bin_op.lhs);
    try cg.genSetMem(.{ .frame = frame_index }, 0, ptr_ty, .{ .air_ref = bin_op.lhs });

    const len_ty = cg.typeOf(bin_op.rhs);
    try cg.genSetMem(
        .{ .frame = frame_index },
        @intCast(ptr_ty.abiSize(zcu)),
        len_ty,
        .{ .air_ref = bin_op.rhs },
    );

    const result = MCValue{ .load_frame = .{ .index = frame_index } };
    return cg.finishAir(inst, result, .{ bin_op.lhs, bin_op.rhs, .none });
}

fn airBinOp(cg: *CodeGen, inst: Air.Inst.Index, tag: Air.Inst.Tag) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const bin_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].bin_op;
    const dst_mcv = try cg.binOp(inst, tag, bin_op.lhs, bin_op.rhs);

    const dst_ty = cg.typeOfIndex(inst);
    if (dst_ty.isAbiInt(zcu)) {
        const abi_size: u32 = @intCast(dst_ty.abiSize(zcu));
        const bit_size: u32 = @intCast(dst_ty.bitSize(zcu));
        if (abi_size * 8 > bit_size) {
            const dst_lock = switch (dst_mcv) {
                .register => |dst_reg| cg.register_manager.lockRegAssumeUnused(dst_reg),
                else => null,
            };
            defer if (dst_lock) |lock| cg.register_manager.unlockReg(lock);

            if (dst_mcv.isRegister()) {
                try cg.truncateRegister(dst_ty, dst_mcv.getReg().?);
            } else {
                const tmp_reg, const tmp_lock = try cg.allocReg(.int);
                defer cg.register_manager.unlockReg(tmp_lock);

                const hi_ty = try pt.intType(.unsigned, @intCast((dst_ty.bitSize(zcu) - 1) % 64 + 1));
                const hi_mcv = dst_mcv.address().offset(@intCast(bit_size / 64 * 8)).deref();
                try cg.genSetReg(hi_ty, tmp_reg, hi_mcv);
                try cg.truncateRegister(dst_ty, tmp_reg);
                try cg.genCopy(hi_ty, hi_mcv, .{ .register = tmp_reg });
            }
        }
    }

    return cg.finishAir(inst, dst_mcv, .{ bin_op.lhs, bin_op.rhs, .none });
}

fn binOp(
    cg: *CodeGen,
    maybe_inst: ?Air.Inst.Index,
    air_tag: Air.Inst.Tag,
    lhs_air: Air.Inst.Ref,
    rhs_air: Air.Inst.Ref,
) !MCValue {
    _ = maybe_inst;
    const pt = cg.pt;
    const zcu = pt.zcu;
    const lhs_ty = cg.typeOf(lhs_air);
    const rhs_ty = cg.typeOf(rhs_air);

    if (lhs_ty.isRuntimeFloat()) libcall: {
        const float_bits = lhs_ty.floatBits(cg.target.*);
        const type_needs_libcall = switch (float_bits) {
            16 => true,
            32, 64 => false,
            80, 128 => true,
            else => unreachable,
        };
        if (!type_needs_libcall) break :libcall;
        return cg.fail("binOp libcall runtime-float ops", .{});
    }

    // don't have support for certain sizes of addition
    switch (lhs_ty.zigTypeTag(zcu)) {
        .vector => {}, // works differently and fails in a different place
        else => if (lhs_ty.bitSize(zcu) > 64) return cg.fail("TODO: binOp > 64 bits", .{}),
    }

    const lhs_mcv = try cg.resolveInst(lhs_air);
    const rhs_mcv = try cg.resolveInst(rhs_air);

    const class_for_dst_ty: abi.RegisterClass = switch (air_tag) {
        // will always return int register no matter the input
        .cmp_eq,
        .cmp_neq,
        .cmp_lt,
        .cmp_lte,
        .cmp_gt,
        .cmp_gte,
        => .int,

        else => cg.typeRegClass(lhs_ty),
    };

    const dst_reg, const dst_lock = try cg.allocReg(class_for_dst_ty);
    defer cg.register_manager.unlockReg(dst_lock);

    try cg.genBinOp(
        air_tag,
        lhs_mcv,
        lhs_ty,
        rhs_mcv,
        rhs_ty,
        dst_reg,
    );

    return .{ .register = dst_reg };
}

/// Does the same thing as binOp however is meant to be used internally to the backend.
///
/// The `dst_reg` argument is meant to be caller-locked. Asserts that the binOp result can be
/// fit into the register.
///
/// Assumes that the `dst_reg` class is correct.
fn genBinOp(
    cg: *CodeGen,
    tag: Air.Inst.Tag,
    lhs_mcv: MCValue,
    lhs_ty: Type,
    rhs_mcv: MCValue,
    rhs_ty: Type,
    dst_reg: Register,
) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const bit_size = lhs_ty.bitSize(zcu);

    const is_unsigned = lhs_ty.isUnsignedInt(zcu);

    const lhs_reg, const maybe_lhs_lock = try cg.promoteReg(lhs_ty, lhs_mcv);
    const rhs_reg, const maybe_rhs_lock = try cg.promoteReg(rhs_ty, rhs_mcv);

    defer if (maybe_lhs_lock) |lock| cg.register_manager.unlockReg(lock);
    defer if (maybe_rhs_lock) |lock| cg.register_manager.unlockReg(lock);

    switch (tag) {
        .add,
        .add_wrap,
        .sub,
        .sub_wrap,
        .mul,
        .mul_wrap,
        .rem,
        .div_trunc,
        .div_exact,
        .xor,
        => {
            switch (tag) {
                .rem,
                .div_trunc,
                .div_exact,
                => {
                    if (!math.isPowerOfTwo(bit_size)) {
                        try cg.truncateRegister(lhs_ty, lhs_reg);
                        try cg.truncateRegister(rhs_ty, rhs_reg);
                    }
                },
                else => {
                    if (!math.isPowerOfTwo(bit_size))
                        return cg.fail(
                            "TODO: genBinOp verify if needs to truncate {s} non-pow 2, found {}",
                            .{ @tagName(tag), bit_size },
                        );
                },
            }

            switch (lhs_ty.zigTypeTag(zcu)) {
                .int => {
                    const mnem: Mnemonic = switch (tag) {
                        .add, .add_wrap => switch (bit_size) {
                            8, 16, 64 => .add,
                            32 => .addw,
                            else => unreachable,
                        },
                        .sub, .sub_wrap => switch (bit_size) {
                            8, 16, 32 => .subw,
                            64 => .sub,
                            else => unreachable,
                        },
                        .mul, .mul_wrap => switch (bit_size) {
                            8, 16, 64 => .mul,
                            32 => .mulw,
                            else => unreachable,
                        },
                        .rem => switch (bit_size) {
                            8, 16, 32 => if (is_unsigned) .remuw else .remw,
                            else => if (is_unsigned) .remu else .rem,
                        },
                        .div_trunc, .div_exact => switch (bit_size) {
                            8, 16, 32 => if (is_unsigned) .divuw else .divw,
                            else => if (is_unsigned) .divu else .div,
                        },
                        .xor => .xor,
                        else => unreachable,
                    };

                    _ = try cg.addInst(.{
                        .tag = mnem,
                        .data = .{
                            .r_type = .{
                                .rd = dst_reg,
                                .rs1 = lhs_reg,
                                .rs2 = rhs_reg,
                            },
                        },
                    });
                },
                .float => {
                    const mir_tag: Mnemonic = switch (tag) {
                        .add => switch (bit_size) {
                            32 => .fadds,
                            64 => .faddd,
                            else => unreachable,
                        },
                        .sub => switch (bit_size) {
                            32 => .fsubs,
                            64 => .fsubd,
                            else => unreachable,
                        },
                        .mul => switch (bit_size) {
                            32 => .fmuls,
                            64 => .fmuld,
                            else => unreachable,
                        },
                        else => return cg.fail("TODO: genBinOp {s} Float", .{@tagName(tag)}),
                    };

                    _ = try cg.addInst(.{
                        .tag = mir_tag,
                        .data = .{
                            .r_type = .{
                                .rd = dst_reg,
                                .rs1 = lhs_reg,
                                .rs2 = rhs_reg,
                            },
                        },
                    });
                },
                .vector => {
                    const num_elem = lhs_ty.vectorLen(zcu);
                    const elem_size = lhs_ty.childType(zcu).bitSize(zcu);

                    const child_ty = lhs_ty.childType(zcu);

                    const mir_tag: Mnemonic = switch (tag) {
                        .add => switch (child_ty.zigTypeTag(zcu)) {
                            .int => .vaddvv,
                            .float => .vfaddvv,
                            else => unreachable,
                        },
                        .sub => switch (child_ty.zigTypeTag(zcu)) {
                            .int => .vsubvv,
                            .float => .vfsubvv,
                            else => unreachable,
                        },
                        .mul => switch (child_ty.zigTypeTag(zcu)) {
                            .int => .vmulvv,
                            .float => .vfmulvv,
                            else => unreachable,
                        },
                        else => return cg.fail("TODO: genBinOp {s} Vector", .{@tagName(tag)}),
                    };

                    const vsew = bits.VSew.fromBits(elem_size) orelse
                        return cg.fail("TODO: genBinOp > 64 bit elements, found {d}", .{elem_size});
                    try cg.setVl(.zero, num_elem, .{
                        .vsew = vsew,
                        .vlmul = .m1,
                        .vma = true,
                        .vta = true,
                    });

                    _ = try cg.addInst(.{
                        .tag = mir_tag,
                        .data = .{
                            .r_type = .{
                                .rd = dst_reg,
                                .rs1 = rhs_reg,
                                .rs2 = lhs_reg,
                            },
                        },
                    });
                },
                else => unreachable,
            }
        },

        .add_sat,
        => {
            if (bit_size != 64 or !is_unsigned)
                return cg.fail("TODO: genBinOp ty: {}", .{lhs_ty.fmt(pt)});

            const tmp_reg = try cg.copyToTmpRegister(rhs_ty, .{ .register = rhs_reg });
            const tmp_lock = cg.register_manager.lockRegAssumeUnused(tmp_reg);
            defer cg.register_manager.unlockReg(tmp_lock);

            try cg.asmRType(.add, tmp_reg, rhs_reg, lhs_reg);
            try cg.asmRType(.sltu, dst_reg, tmp_reg, lhs_reg);
            try cg.asmRType(.sub, dst_reg, .zero, dst_reg);
            try cg.asmRType(.@"or", dst_reg, dst_reg, tmp_reg);
        },

        .ptr_add,
        .ptr_sub,
        => {
            const tmp_reg = try cg.copyToTmpRegister(rhs_ty, .{ .register = rhs_reg });
            const tmp_mcv = MCValue{ .register = tmp_reg };
            const tmp_lock = cg.register_manager.lockRegAssumeUnused(tmp_reg);
            defer cg.register_manager.unlockReg(tmp_lock);

            // RISC-V has no immediate mul, so we copy the size to a temporary register
            const elem_size = lhs_ty.elemType2(zcu).abiSize(zcu);
            const elem_size_reg = try cg.copyToTmpRegister(Type.u64, .{ .immediate = elem_size });

            try cg.genBinOp(
                .mul,
                tmp_mcv,
                rhs_ty,
                .{ .register = elem_size_reg },
                Type.u64,
                tmp_reg,
            );

            try cg.genBinOp(
                switch (tag) {
                    .ptr_add => .add,
                    .ptr_sub => .sub,
                    else => unreachable,
                },
                lhs_mcv,
                Type.u64, // we know it's a pointer, so it'll be usize.
                tmp_mcv,
                Type.u64,
                dst_reg,
            );
        },

        .shr,
        .shr_exact,
        .shl,
        .shl_exact,
        => {
            if (bit_size > 64) return cg.fail("TODO: genBinOp shift > 64 bits, {}", .{bit_size});
            try cg.truncateRegister(rhs_ty, rhs_reg);

            const mir_tag: Mnemonic = switch (tag) {
                .shl, .shl_exact => switch (bit_size) {
                    1...31, 33...64 => .sll,
                    32 => .sllw,
                    else => unreachable,
                },
                .shr, .shr_exact => switch (bit_size) {
                    1...31, 33...64 => .srl,
                    32 => .srlw,
                    else => unreachable,
                },
                else => unreachable,
            };

            try cg.asmRType(mir_tag, dst_reg, lhs_reg, rhs_reg);
        },

        // TODO: move the isel logic out of lower and into here.
        .cmp_eq,
        .cmp_neq,
        .cmp_lt,
        .cmp_lte,
        .cmp_gt,
        .cmp_gte,
        => {
            assert(lhs_reg.class() == rhs_reg.class());
            if (lhs_reg.class() == .int) {
                try cg.truncateRegister(lhs_ty, lhs_reg);
                try cg.truncateRegister(rhs_ty, rhs_reg);
            }

            _ = try cg.addInst(.{
                .tag = .pseudo_compare,
                .data = .{
                    .compare = .{
                        .op = switch (tag) {
                            .cmp_eq => .eq,
                            .cmp_neq => .neq,
                            .cmp_lt => .lt,
                            .cmp_lte => .lte,
                            .cmp_gt => .gt,
                            .cmp_gte => .gte,
                            else => unreachable,
                        },
                        .rd = dst_reg,
                        .rs1 = lhs_reg,
                        .rs2 = rhs_reg,
                        .ty = lhs_ty,
                    },
                },
            });
        },

        // A branchless @min/@max sequence.
        //
        // Assume that a0 and a1 are the lhs and rhs respectively.
        // Also assume that a2 is the destination register.
        //
        // Algorithm:
        // slt s0, a0, a1
        // sub s0, zero, s0
        // xor a2, a0, a1
        // and s0, a2, s0
        // xor a2, a0, s0 # a0 is @min, a1 is @max
        //
        // "slt s0, a0, a1" will set s0 to 1 if a0 is less than a1, and 1 otherwise.
        //
        // "sub s0, zero, s0" will set all the bits of s0 to 1 if it was 1, otherwise it'll remain at 0.
        //
        // "xor a2, a0, a1" stores the bitwise XOR of a0 and a1 in a2. Effectively getting the difference between them.
        //
        // "and a0, a2, s0" here we mask the result of the XOR with the negated s0. If a0 < a1, s0 is -1, which
        // doesn't change the bits of a2. If a0 >= a1, s0 is 0, nullifying a2.
        //
        // "xor a2, a0, s0" the final XOR operation adjusts a2 to be the minimum value of a0 and a1. If a0 was less than
        // a1, s0 was -1, flipping all the bits in a2 and effectively restoring a0. If a0 was greater than or equal to a1,
        // s0 was 0, leaving a2 unchanged as a0.
        .min, .max => {
            switch (lhs_ty.zigTypeTag(zcu)) {
                .int => {
                    const int_info = lhs_ty.intInfo(zcu);

                    const mask_reg, const mask_lock = try cg.allocReg(.int);
                    defer cg.register_manager.unlockReg(mask_lock);

                    try cg.asmRType(
                        if (int_info.signedness == .unsigned) .sltu else .slt,
                        mask_reg,
                        lhs_reg,
                        rhs_reg,
                    );
                    try cg.asmRType(.sub, mask_reg, .zero, mask_reg);
                    try cg.asmRType(.xor, dst_reg, lhs_reg, rhs_reg);
                    try cg.asmRType(.@"and", mask_reg, dst_reg, mask_reg);
                    try cg.asmRType(
                        .xor,
                        dst_reg,
                        if (tag == .min) rhs_reg else lhs_reg,
                        mask_reg,
                    );
                },
                else => |t| return cg.fail("TODO: genBinOp min/max for {s}", .{@tagName(t)}),
            }
        },
        else => return cg.fail("TODO: genBinOp {}", .{tag}),
    }
}

fn airPtrArithmetic(cg: *CodeGen, inst: Air.Inst.Index, tag: Air.Inst.Tag) !void {
    const ty_pl = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_pl;
    const bin_op = cg.air.extraData(Air.Bin, ty_pl.payload).data;
    const dst_mcv = try cg.binOp(inst, tag, bin_op.lhs, bin_op.rhs);
    return cg.finishAir(inst, dst_mcv, .{ bin_op.lhs, bin_op.rhs, .none });
}

fn airAddWithOverflow(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const ty_pl = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_pl;
    const extra = cg.air.extraData(Air.Bin, ty_pl.payload).data;

    const rhs_ty = cg.typeOf(extra.rhs);
    const lhs_ty = cg.typeOf(extra.lhs);

    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        switch (lhs_ty.zigTypeTag(zcu)) {
            .vector => return cg.fail("TODO implement add with overflow for Vector type", .{}),
            .int => {
                const int_info = lhs_ty.intInfo(zcu);

                const tuple_ty = cg.typeOfIndex(inst);
                const result_mcv = try cg.allocRegOrMem(tuple_ty, inst, false);
                const offset = result_mcv.load_frame;

                if (int_info.bits >= 8 and math.isPowerOfTwo(int_info.bits)) {
                    const add_result = try cg.binOp(null, .add, extra.lhs, extra.rhs);

                    try cg.genSetMem(
                        .{ .frame = offset.index },
                        offset.off + @as(i32, @intCast(tuple_ty.structFieldOffset(0, zcu))),
                        lhs_ty,
                        add_result,
                    );

                    const trunc_reg = try cg.copyToTmpRegister(lhs_ty, add_result);
                    const trunc_reg_lock = cg.register_manager.lockRegAssumeUnused(trunc_reg);
                    defer cg.register_manager.unlockReg(trunc_reg_lock);

                    const overflow_reg, const overflow_lock = try cg.allocReg(.int);
                    defer cg.register_manager.unlockReg(overflow_lock);

                    // if the result isn't equal after truncating it to the given type,
                    // an overflow must have happened.
                    try cg.truncateRegister(lhs_ty, trunc_reg);
                    try cg.genBinOp(
                        .cmp_neq,
                        add_result,
                        lhs_ty,
                        .{ .register = trunc_reg },
                        rhs_ty,
                        overflow_reg,
                    );

                    try cg.genSetMem(
                        .{ .frame = offset.index },
                        offset.off + @as(i32, @intCast(tuple_ty.structFieldOffset(1, zcu))),
                        Type.u1,
                        .{ .register = overflow_reg },
                    );

                    break :result result_mcv;
                } else {
                    return cg.fail("TODO: airAddWithOverflow {d}", .{int_info.bits});
                }
            },
            else => unreachable,
        }
    };

    return cg.finishAir(inst, result, .{ extra.lhs, extra.rhs, .none });
}

fn airSubWithOverflow(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const ty_pl = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_pl;
    const extra = cg.air.extraData(Air.Bin, ty_pl.payload).data;

    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const lhs = try cg.resolveInst(extra.lhs);
        const rhs = try cg.resolveInst(extra.rhs);
        const lhs_ty = cg.typeOf(extra.lhs);
        const rhs_ty = cg.typeOf(extra.rhs);

        const int_info = lhs_ty.intInfo(zcu);

        if (!math.isPowerOfTwo(int_info.bits) or int_info.bits < 8) {
            return cg.fail("TODO: airSubWithOverflow non-power of 2 and less than 8 bits", .{});
        }

        if (int_info.bits > 64) {
            return cg.fail("TODO: airSubWithOverflow > 64 bits", .{});
        }

        const tuple_ty = cg.typeOfIndex(inst);
        const result_mcv = try cg.allocRegOrMem(tuple_ty, inst, false);
        const offset = result_mcv.load_frame;

        const dest_mcv = try cg.binOp(null, .sub, extra.lhs, extra.rhs);
        assert(dest_mcv == .register);
        const dest_reg = dest_mcv.register;

        try cg.genSetMem(
            .{ .frame = offset.index },
            offset.off + @as(i32, @intCast(tuple_ty.structFieldOffset(0, zcu))),
            lhs_ty,
            .{ .register = dest_reg },
        );

        const lhs_reg, const lhs_lock = try cg.promoteReg(lhs_ty, lhs);
        defer if (lhs_lock) |lock| cg.register_manager.unlockReg(lock);

        const rhs_reg, const rhs_lock = try cg.promoteReg(rhs_ty, rhs);
        defer if (rhs_lock) |lock| cg.register_manager.unlockReg(lock);

        const overflow_reg = try cg.copyToTmpRegister(Type.u64, .{ .immediate = 0 });

        const overflow_lock = cg.register_manager.lockRegAssumeUnused(overflow_reg);
        defer cg.register_manager.unlockReg(overflow_lock);

        switch (int_info.signedness) {
            .unsigned => {
                try cg.asmRType(.sltu, overflow_reg, lhs_reg, rhs_reg);
                try cg.genSetMem(
                    .{ .frame = offset.index },
                    offset.off + @as(i32, @intCast(tuple_ty.structFieldOffset(1, zcu))),
                    Type.u1,
                    .{ .register = overflow_reg },
                );

                break :result result_mcv;
            },
            .signed => {
                switch (int_info.bits) {
                    64 => {
                        try cg.asmRType(.slt, overflow_reg, overflow_reg, rhs_reg);
                        try cg.asmRType(.slt, rhs_reg, rhs_reg, lhs_reg);
                        try cg.asmRType(.xor, lhs_reg, overflow_reg, rhs_reg);

                        try cg.genBinOp(
                            .cmp_neq,
                            .{ .register = overflow_reg },
                            Type.u64,
                            .{ .register = rhs_reg },
                            Type.u64,
                            overflow_reg,
                        );

                        try cg.genSetMem(
                            .{ .frame = offset.index },
                            offset.off + @as(i32, @intCast(tuple_ty.structFieldOffset(1, zcu))),
                            Type.u1,
                            .{ .register = overflow_reg },
                        );

                        break :result result_mcv;
                    },
                    else => |int_bits| return cg.fail("TODO: airSubWithOverflow signed {}", .{int_bits}),
                }
            },
        }
    };

    return cg.finishAir(inst, result, .{ extra.lhs, extra.rhs, .none });
}

fn airMulWithOverflow(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const ty_pl = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_pl;
    const extra = cg.air.extraData(Air.Bin, ty_pl.payload).data;

    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const lhs = try cg.resolveInst(extra.lhs);
        const rhs = try cg.resolveInst(extra.rhs);
        const lhs_ty = cg.typeOf(extra.lhs);
        const rhs_ty = cg.typeOf(extra.rhs);

        const tuple_ty = cg.typeOfIndex(inst);

        // genSetReg needs to support register_offset src_mcv for this to be true.
        const result_mcv = try cg.allocRegOrMem(tuple_ty, inst, false);

        const result_off: i32 = @intCast(tuple_ty.structFieldOffset(0, zcu));
        const overflow_off: i32 = @intCast(tuple_ty.structFieldOffset(1, zcu));

        const dest_reg, const dest_lock = try cg.allocReg(.int);
        defer cg.register_manager.unlockReg(dest_lock);

        try cg.genBinOp(
            .mul,
            lhs,
            lhs_ty,
            rhs,
            rhs_ty,
            dest_reg,
        );

        try cg.genCopy(
            lhs_ty,
            result_mcv.offset(result_off),
            .{ .register = dest_reg },
        );

        switch (lhs_ty.zigTypeTag(zcu)) {
            else => |x| return cg.fail("TODO: airMulWithOverflow {s}", .{@tagName(x)}),
            .int => {
                if (std.debug.runtime_safety) assert(lhs_ty.eql(rhs_ty, zcu));

                const trunc_reg = try cg.copyToTmpRegister(lhs_ty, .{ .register = dest_reg });
                const trunc_reg_lock = cg.register_manager.lockRegAssumeUnused(trunc_reg);
                defer cg.register_manager.unlockReg(trunc_reg_lock);

                const overflow_reg, const overflow_lock = try cg.allocReg(.int);
                defer cg.register_manager.unlockReg(overflow_lock);

                // if the result isn't equal after truncating it to the given type,
                // an overflow must have happened.
                try cg.truncateRegister(cg.typeOf(extra.lhs), trunc_reg);
                try cg.genBinOp(
                    .cmp_neq,
                    .{ .register = dest_reg },
                    lhs_ty,
                    .{ .register = trunc_reg },
                    rhs_ty,
                    overflow_reg,
                );

                try cg.genCopy(
                    lhs_ty,
                    result_mcv.offset(overflow_off),
                    .{ .register = overflow_reg },
                );

                break :result result_mcv;
            },
        }
    };

    return cg.finishAir(inst, result, .{ extra.lhs, extra.rhs, .none });
}

fn airShlWithOverflow(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const bin_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].bin_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else return cg.fail("TODO implement airShlWithOverflow", .{});
    return cg.finishAir(inst, result, .{ bin_op.lhs, bin_op.rhs, .none });
}

fn airSubSat(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const bin_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].bin_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else return cg.fail("TODO implement airSubSat", .{});
    return cg.finishAir(inst, result, .{ bin_op.lhs, bin_op.rhs, .none });
}

fn airMulSat(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const bin_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].bin_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else return cg.fail("TODO implement airMulSat", .{});
    return cg.finishAir(inst, result, .{ bin_op.lhs, bin_op.rhs, .none });
}

fn airShlSat(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const bin_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].bin_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else return cg.fail("TODO implement airShlSat", .{});
    return cg.finishAir(inst, result, .{ bin_op.lhs, bin_op.rhs, .none });
}

fn airOptionalPayload(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const zcu = cg.pt.zcu;
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const result: MCValue = result: {
        const pl_ty = cg.typeOfIndex(inst);
        if (!pl_ty.hasRuntimeBitsIgnoreComptime(zcu)) break :result .none;

        const opt_mcv = try cg.resolveInst(ty_op.operand);
        if (cg.reuseOperand(inst, ty_op.operand, 0, opt_mcv)) {
            switch (opt_mcv) {
                .register => |pl_reg| try cg.truncateRegister(pl_ty, pl_reg),
                else => {},
            }
            break :result opt_mcv;
        }

        const pl_mcv = try cg.allocRegOrMem(pl_ty, inst, true);
        try cg.genCopy(pl_ty, pl_mcv, opt_mcv);
        break :result pl_mcv;
    };
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn airOptionalPayloadPtr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else return cg.fail("TODO implement .optional_payload_ptr for {}", .{cg.target.cpu.arch});
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn airOptionalPayloadPtrSet(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const zcu = cg.pt.zcu;

    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const dst_ty = cg.typeOfIndex(inst);
        const src_ty = cg.typeOf(ty_op.operand);
        const opt_ty = src_ty.childType(zcu);
        const src_mcv = try cg.resolveInst(ty_op.operand);

        if (opt_ty.optionalReprIsPayload(zcu)) {
            break :result if (cg.reuseOperand(inst, ty_op.operand, 0, src_mcv))
                src_mcv
            else
                try cg.copyToNewRegister(inst, src_mcv);
        }

        const dst_mcv: MCValue = if (src_mcv.isRegister() and
            cg.reuseOperand(inst, ty_op.operand, 0, src_mcv))
            src_mcv
        else
            try cg.copyToNewRegister(inst, src_mcv);

        const pl_ty = dst_ty.childType(zcu);
        const pl_abi_size: i32 = @intCast(pl_ty.abiSize(zcu));
        try cg.genSetMem(
            .{ .reg = dst_mcv.getReg().? },
            pl_abi_size,
            Type.bool,
            .{ .immediate = 1 },
        );
        break :result dst_mcv;
    };
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn airUnwrapErrErr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const pt = cg.pt;
    const zcu = pt.zcu;
    const err_union_ty = cg.typeOf(ty_op.operand);
    const err_ty = err_union_ty.errorUnionSet(zcu);
    const payload_ty = err_union_ty.errorUnionPayload(zcu);
    const operand = try cg.resolveInst(ty_op.operand);

    const result: MCValue = result: {
        if (err_ty.errorSetIsEmpty(zcu)) {
            break :result .{ .immediate = 0 };
        }

        if (!payload_ty.hasRuntimeBitsIgnoreComptime(zcu)) {
            break :result operand;
        }

        const err_off: u32 = @intCast(errUnionErrorOffset(payload_ty, zcu));

        switch (operand) {
            .register => |reg| {
                const eu_lock = cg.register_manager.lockReg(reg);
                defer if (eu_lock) |lock| cg.register_manager.unlockReg(lock);

                const result = try cg.copyToNewRegister(inst, operand);
                if (err_off > 0) {
                    try cg.genBinOp(
                        .shr,
                        result,
                        err_union_ty,
                        .{ .immediate = @as(u6, @intCast(err_off * 8)) },
                        Type.u8,
                        result.register,
                    );
                }
                break :result result;
            },
            .load_frame => |frame_addr| break :result .{ .load_frame = .{
                .index = frame_addr.index,
                .off = frame_addr.off + @as(i32, @intCast(err_off)),
            } },
            else => return cg.fail("TODO implement unwrap_err_err for {}", .{operand}),
        }
    };

    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn airUnwrapErrPayload(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const operand_ty = cg.typeOf(ty_op.operand);
    const operand = try cg.resolveInst(ty_op.operand);
    const result = try cg.genUnwrapErrUnionPayloadMir(operand_ty, operand);
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn genUnwrapErrUnionPayloadMir(
    cg: *CodeGen,
    err_union_ty: Type,
    err_union: MCValue,
) !MCValue {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const payload_ty = err_union_ty.errorUnionPayload(zcu);

    const result: MCValue = result: {
        if (!payload_ty.hasRuntimeBitsIgnoreComptime(zcu)) break :result .none;

        const payload_off: u31 = @intCast(errUnionPayloadOffset(payload_ty, zcu));
        switch (err_union) {
            .load_frame => |frame_addr| break :result .{ .load_frame = .{
                .index = frame_addr.index,
                .off = frame_addr.off + payload_off,
            } },
            .register => |reg| {
                const eu_lock = cg.register_manager.lockReg(reg);
                defer if (eu_lock) |lock| cg.register_manager.unlockReg(lock);

                const result_reg = try cg.copyToTmpRegister(err_union_ty, err_union);
                if (payload_off > 0) {
                    try cg.genBinOp(
                        .shr,
                        .{ .register = result_reg },
                        err_union_ty,
                        .{ .immediate = @as(u6, @intCast(payload_off * 8)) },
                        Type.u8,
                        result_reg,
                    );
                }
                break :result .{ .register = result_reg };
            },
            else => return cg.fail("TODO implement genUnwrapErrUnionPayloadMir for {}", .{err_union}),
        }
    };

    return result;
}

// *(E!T) -> E
fn airUnwrapErrErrPtr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else return cg.fail("TODO implement unwrap error union error ptr for {}", .{cg.target.cpu.arch});
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

// *(E!T) -> *T
fn airUnwrapErrPayloadPtr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else return cg.fail("TODO implement unwrap error union payload ptr for {}", .{cg.target.cpu.arch});
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

// *(E!T) => *T
fn airErrUnionPayloadPtrSet(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const zcu = cg.pt.zcu;
        const src_ty = cg.typeOf(ty_op.operand);
        const src_mcv = try cg.resolveInst(ty_op.operand);

        // `src_reg` contains the pointer to the error union
        const src_reg = switch (src_mcv) {
            .register => |reg| reg,
            else => try cg.copyToTmpRegister(src_ty, src_mcv),
        };
        const src_lock = cg.register_manager.lockRegAssumeUnused(src_reg);
        defer cg.register_manager.unlockReg(src_lock);

        // we set the place of where the error would have been to 0
        const eu_ty = src_ty.childType(zcu);
        const pl_ty = eu_ty.errorUnionPayload(zcu);
        const err_ty = eu_ty.errorUnionSet(zcu);
        const err_off: i32 = @intCast(errUnionErrorOffset(pl_ty, zcu));
        try cg.genSetMem(.{ .reg = src_reg }, err_off, err_ty, .{ .immediate = 0 });

        const dst_reg, const dst_lock = if (cg.reuseOperand(inst, ty_op.operand, 0, src_mcv))
            .{ src_reg, null }
        else
            try cg.allocReg(.int);
        defer if (dst_lock) |lock| cg.register_manager.unlockReg(lock);

        // move the pointer to be at the payload
        const pl_off = errUnionPayloadOffset(pl_ty, zcu);
        try cg.genBinOp(
            .add,
            .{ .register = src_reg },
            Type.u64,
            .{ .immediate = pl_off },
            Type.u64,
            dst_reg,
        );

        break :result .{ .register = dst_reg };
    };
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn airErrReturnTrace(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const result: MCValue = if (cg.liveness.isUnused(inst))
        .unreach
    else
        return cg.fail("TODO implement airErrReturnTrace for {}", .{cg.target.cpu.arch});
    return cg.finishAir(inst, result, .{ .none, .none, .none });
}

fn airSetErrReturnTrace(cg: *CodeGen, inst: Air.Inst.Index) !void {
    _ = inst;
    return cg.fail("TODO implement airSetErrReturnTrace for {}", .{cg.target.cpu.arch});
}

fn airSaveErrReturnTraceIndex(cg: *CodeGen, inst: Air.Inst.Index) !void {
    _ = inst;
    return cg.fail("TODO implement airSaveErrReturnTraceIndex for {}", .{cg.target.cpu.arch});
}

fn airWrapOptional(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const result: MCValue = result: {
        const pl_ty = cg.typeOf(ty_op.operand);
        if (!pl_ty.hasRuntimeBits(zcu)) break :result .{ .immediate = 1 };

        const opt_ty = cg.typeOfIndex(inst);
        const pl_mcv = try cg.resolveInst(ty_op.operand);
        const same_repr = opt_ty.optionalReprIsPayload(zcu);
        if (same_repr and cg.reuseOperand(inst, ty_op.operand, 0, pl_mcv)) break :result pl_mcv;

        const pl_lock: ?RegisterLock = switch (pl_mcv) {
            .register => |reg| cg.register_manager.lockRegAssumeUnused(reg),
            else => null,
        };
        defer if (pl_lock) |lock| cg.register_manager.unlockReg(lock);

        const opt_mcv = try cg.allocRegOrMem(opt_ty, inst, false);
        try cg.genCopy(pl_ty, opt_mcv, pl_mcv);

        if (!same_repr) {
            const pl_abi_size: i32 = @intCast(pl_ty.abiSize(zcu));
            switch (opt_mcv) {
                .load_frame => |frame_addr| {
                    try cg.genCopy(pl_ty, opt_mcv, pl_mcv);
                    try cg.genSetMem(
                        .{ .frame = frame_addr.index },
                        frame_addr.off + pl_abi_size,
                        Type.u8,
                        .{ .immediate = 1 },
                    );
                },
                else => unreachable,
            }
        }
        break :result opt_mcv;
    };
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

/// T to E!T
fn airWrapErrUnionPayload(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;

    const eu_ty = ty_op.ty.toType();
    const pl_ty = eu_ty.errorUnionPayload(zcu);
    const err_ty = eu_ty.errorUnionSet(zcu);
    const operand = try cg.resolveInst(ty_op.operand);

    const result: MCValue = result: {
        if (!pl_ty.hasRuntimeBitsIgnoreComptime(zcu)) break :result .{ .immediate = 0 };

        const frame_index = try cg.allocFrameIndex(FrameAlloc.initSpill(eu_ty, zcu));
        const pl_off: i32 = @intCast(errUnionPayloadOffset(pl_ty, zcu));
        const err_off: i32 = @intCast(errUnionErrorOffset(pl_ty, zcu));
        try cg.genSetMem(.{ .frame = frame_index }, pl_off, pl_ty, operand);
        try cg.genSetMem(.{ .frame = frame_index }, err_off, err_ty, .{ .immediate = 0 });
        break :result .{ .load_frame = .{ .index = frame_index } };
    };

    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

/// E to E!T
fn airWrapErrUnionErr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;

    const eu_ty = ty_op.ty.toType();
    const pl_ty = eu_ty.errorUnionPayload(zcu);
    const err_ty = eu_ty.errorUnionSet(zcu);

    const result: MCValue = result: {
        if (!pl_ty.hasRuntimeBitsIgnoreComptime(zcu)) break :result try cg.resolveInst(ty_op.operand);

        const frame_index = try cg.allocFrameIndex(FrameAlloc.initSpill(eu_ty, zcu));
        const pl_off: i32 = @intCast(errUnionPayloadOffset(pl_ty, zcu));
        const err_off: i32 = @intCast(errUnionErrorOffset(pl_ty, zcu));
        try cg.genSetMem(.{ .frame = frame_index }, pl_off, pl_ty, .{ .undef = null });
        const operand = try cg.resolveInst(ty_op.operand);
        try cg.genSetMem(.{ .frame = frame_index }, err_off, err_ty, operand);
        break :result .{ .load_frame = .{ .index = frame_index } };
    };
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn airTry(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pl_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].pl_op;
    const extra = cg.air.extraData(Air.Try, pl_op.payload);
    const body: []const Air.Inst.Index = @ptrCast(cg.air.extra[extra.end..][0..extra.data.body_len]);
    const operand_ty = cg.typeOf(pl_op.operand);
    const result = try cg.genTry(inst, pl_op.operand, body, operand_ty, false);
    return cg.finishAir(inst, result, .{ .none, .none, .none });
}

fn genTry(
    cg: *CodeGen,
    inst: Air.Inst.Index,
    operand: Air.Inst.Ref,
    body: []const Air.Inst.Index,
    operand_ty: Type,
    operand_is_ptr: bool,
) !MCValue {
    _ = operand_is_ptr;

    const liveness_cond_br = cg.liveness.getCondBr(inst);

    const operand_mcv = try cg.resolveInst(operand);
    const is_err_mcv = try cg.isErr(null, operand_ty, operand_mcv);

    // A branch to the false section. Uses beq. 1 is the default "true" state.
    const reloc = try cg.condBr(Type.anyerror, is_err_mcv);

    if (cg.liveness.operandDies(inst, 0)) {
        if (operand.toIndex()) |operand_inst| try cg.processDeath(operand_inst);
    }

    const state = try cg.saveState();

    for (liveness_cond_br.else_deaths) |death| try cg.processDeath(death);
    try cg.genBody(body);
    try cg.restoreState(state, &.{}, .{
        .emit_instructions = false,
        .update_tracking = true,
        .resurrect = true,
        .close_scope = true,
    });

    cg.performReloc(reloc);

    for (liveness_cond_br.then_deaths) |death| try cg.processDeath(death);

    const result = if (cg.liveness.isUnused(inst))
        .unreach
    else
        try cg.genUnwrapErrUnionPayloadMir(operand_ty, operand_mcv);

    return result;
}

fn airSlicePtr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const result = result: {
        const src_mcv = try cg.resolveInst(ty_op.operand);
        if (cg.reuseOperand(inst, ty_op.operand, 0, src_mcv)) break :result src_mcv;

        const dst_mcv = try cg.allocRegOrMem(cg.typeOfIndex(inst), inst, true);
        const dst_ty = cg.typeOfIndex(inst);
        try cg.genCopy(dst_ty, dst_mcv, src_mcv);
        break :result dst_mcv;
    };
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn airSliceLen(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const ty = cg.typeOfIndex(inst);
        const src_mcv = try cg.resolveInst(ty_op.operand);
        const len_mcv: MCValue = switch (src_mcv) {
            .register_pair => |regs| .{ .register = regs[1] },
            .load_frame => |frame_addr| .{ .load_frame = .{
                .index = frame_addr.index,
                .off = frame_addr.off + 8,
            } },
            else => return cg.fail("TODO implement slice_len for {}", .{src_mcv}),
        };
        if (cg.reuseOperand(inst, ty_op.operand, 0, src_mcv)) {
            switch (src_mcv) {
                .register_pair => |regs| try cg.freeValue(.{ .register = regs[0] }),
                .load_frame => {},
                else => unreachable,
            }
            break :result len_mcv;
        }
        const dst_mcv = try cg.allocRegOrMem(ty, inst, true);
        try cg.genCopy(cg.typeOfIndex(inst), dst_mcv, len_mcv);
        break :result dst_mcv;
    };
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn airPtrSliceLenPtr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const src_mcv = try cg.resolveInst(ty_op.operand);

        const dst_reg, const dst_lock = try cg.allocReg(.int);
        defer cg.register_manager.unlockReg(dst_lock);
        const dst_mcv: MCValue = .{ .register = dst_reg };

        try cg.genCopy(Type.u64, dst_mcv, src_mcv.offset(8));
        break :result dst_mcv;
    };
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn airPtrSlicePtrPtr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;

    const opt_mcv = try cg.resolveInst(ty_op.operand);
    const dst_mcv = if (cg.reuseOperand(inst, ty_op.operand, 0, opt_mcv))
        opt_mcv
    else
        try cg.copyToNewRegister(inst, opt_mcv);
    return cg.finishAir(inst, dst_mcv, .{ ty_op.operand, .none, .none });
}

fn airSliceElemVal(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const bin_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].bin_op;

    const result: MCValue = result: {
        const elem_ty = cg.typeOfIndex(inst);
        if (!elem_ty.hasRuntimeBitsIgnoreComptime(zcu)) break :result .none;

        const slice_ty = cg.typeOf(bin_op.lhs);
        const slice_ptr_field_type = slice_ty.slicePtrFieldType(zcu);
        const elem_ptr = try cg.genSliceElemPtr(bin_op.lhs, bin_op.rhs);
        const dst_mcv = try cg.allocRegOrMem(elem_ty, inst, false);
        try cg.load(dst_mcv, elem_ptr, slice_ptr_field_type);
        break :result dst_mcv;
    };
    return cg.finishAir(inst, result, .{ bin_op.lhs, bin_op.rhs, .none });
}

fn airSliceElemPtr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_pl = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_pl;
    const extra = cg.air.extraData(Air.Bin, ty_pl.payload).data;
    const dst_mcv = try cg.genSliceElemPtr(extra.lhs, extra.rhs);
    return cg.finishAir(inst, dst_mcv, .{ extra.lhs, extra.rhs, .none });
}

fn genSliceElemPtr(cg: *CodeGen, lhs: Air.Inst.Ref, rhs: Air.Inst.Ref) !MCValue {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const slice_ty = cg.typeOf(lhs);
    const slice_mcv = try cg.resolveInst(lhs);
    const slice_mcv_lock: ?RegisterLock = switch (slice_mcv) {
        .register => |reg| cg.register_manager.lockRegAssumeUnused(reg),
        else => null,
    };
    defer if (slice_mcv_lock) |lock| cg.register_manager.unlockReg(lock);

    const elem_ty = slice_ty.childType(zcu);
    const elem_size = elem_ty.abiSize(zcu);

    const index_ty = cg.typeOf(rhs);
    const index_mcv = try cg.resolveInst(rhs);
    const index_mcv_lock: ?RegisterLock = switch (index_mcv) {
        .register => |reg| cg.register_manager.lockRegAssumeUnused(reg),
        else => null,
    };
    defer if (index_mcv_lock) |lock| cg.register_manager.unlockReg(lock);

    const offset_reg = try cg.elemOffset(index_ty, index_mcv, elem_size);
    const offset_reg_lock = cg.register_manager.lockRegAssumeUnused(offset_reg);
    defer cg.register_manager.unlockReg(offset_reg_lock);

    const addr_reg, const addr_lock = try cg.allocReg(.int);
    defer cg.register_manager.unlockReg(addr_lock);
    try cg.genSetReg(Type.u64, addr_reg, slice_mcv);

    try cg.asmRType(.add, addr_reg, addr_reg, offset_reg);

    return .{ .register = addr_reg };
}

fn airArrayElemVal(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const bin_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].bin_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const result_ty = cg.typeOfIndex(inst);

        const array_ty = cg.typeOf(bin_op.lhs);
        const array_mcv = try cg.resolveInst(bin_op.lhs);

        const index_mcv = try cg.resolveInst(bin_op.rhs);
        const index_ty = cg.typeOf(bin_op.rhs);

        const elem_ty = array_ty.childType(zcu);
        const elem_abi_size = elem_ty.abiSize(zcu);

        const addr_reg, const addr_reg_lock = try cg.allocReg(.int);
        defer cg.register_manager.unlockReg(addr_reg_lock);

        switch (array_mcv) {
            .register => {
                const frame_index = try cg.allocFrameIndex(FrameAlloc.initType(array_ty, zcu));
                try cg.genSetMem(.{ .frame = frame_index }, 0, array_ty, array_mcv);
                try cg.genSetReg(Type.u64, addr_reg, .{ .lea_frame = .{ .index = frame_index } });
            },
            .load_frame => |frame_addr| {
                try cg.genSetReg(Type.u64, addr_reg, .{ .lea_frame = frame_addr });
            },
            else => try cg.genSetReg(Type.u64, addr_reg, array_mcv.address()),
        }

        const dst_mcv = try cg.allocRegOrMem(result_ty, inst, false);

        if (array_ty.isVector(zcu)) {
            // we need to load the vector, vslidedown to get the element we want
            // and store that element in a load frame.
            const src_reg, const src_locks = try cg.allocVecReg(array_ty);
            defer {
                for (src_locks) |lock| cg.register_manager.unlockReg(lock);
                cg.gpa.free(src_locks);
            }

            // load the vector into a temporary register
            try cg.genCopy(array_ty, .{ .register = src_reg }, .{ .indirect = .{ .reg = addr_reg } });

            // we need to construct a 1xbitSize vector because of how lane splitting works in RISC-V
            const single_ty = try pt.vectorType(.{ .child = elem_ty.toIntern(), .len = 1 });

            // we can do a shortcut here where we don't need a vslicedown
            // and can just copy to the frame index.
            if (!(index_mcv == .immediate and index_mcv.immediate == 0)) {
                const index_reg = try cg.copyToTmpRegister(Type.u64, index_mcv);
                try cg.asmRType(.vslidedownvx, src_reg, index_reg, src_reg);
            }

            try cg.genCopy(single_ty, dst_mcv, .{ .register = src_reg });
            break :result dst_mcv;
        }

        const offset_reg = try cg.elemOffset(index_ty, index_mcv, elem_abi_size);
        const offset_lock = cg.register_manager.lockRegAssumeUnused(offset_reg);
        defer cg.register_manager.unlockReg(offset_lock);

        try cg.asmRType(.add, addr_reg, addr_reg, offset_reg);
        try cg.genCopy(elem_ty, dst_mcv, .{ .indirect = .{ .reg = addr_reg } });

        break :result dst_mcv;
    };
    return cg.finishAir(inst, result, .{ bin_op.lhs, bin_op.rhs, .none });
}

fn airPtrElemVal(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const is_volatile = false; // TODO
    const pt = cg.pt;
    const zcu = pt.zcu;
    const bin_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].bin_op;
    const base_ptr_ty = cg.typeOf(bin_op.lhs);

    const result: MCValue = if (!is_volatile and cg.liveness.isUnused(inst)) .unreach else result: {
        const elem_ty = base_ptr_ty.elemType2(zcu);
        if (!elem_ty.hasRuntimeBitsIgnoreComptime(zcu)) break :result .none;

        const base_ptr_mcv = try cg.resolveInst(bin_op.lhs);
        const base_ptr_lock: ?RegisterLock = switch (base_ptr_mcv) {
            .register => |reg| cg.register_manager.lockRegAssumeUnused(reg),
            else => null,
        };
        defer if (base_ptr_lock) |lock| cg.register_manager.unlockReg(lock);

        const index_mcv = try cg.resolveInst(bin_op.rhs);
        const index_lock: ?RegisterLock = switch (index_mcv) {
            .register => |reg| cg.register_manager.lockRegAssumeUnused(reg),
            else => null,
        };
        defer if (index_lock) |lock| cg.register_manager.unlockReg(lock);

        const elem_ptr_reg, const elem_ptr_lock = if (base_ptr_mcv.isRegister() and
            cg.liveness.operandDies(inst, 0))
            .{ base_ptr_mcv.register, null }
        else blk: {
            const reg, const lock = try cg.allocReg(.int);
            try cg.genSetReg(base_ptr_ty, reg, base_ptr_mcv);
            break :blk .{ reg, lock };
        };
        defer if (elem_ptr_lock) |lock| cg.register_manager.unlockReg(lock);

        try cg.genBinOp(
            .ptr_add,
            base_ptr_mcv,
            base_ptr_ty,
            index_mcv,
            Type.u64,
            elem_ptr_reg,
        );

        const dst_mcv = try cg.allocRegOrMem(cg.typeOfIndex(inst), inst, true);
        const dst_lock = switch (dst_mcv) {
            .register => |reg| cg.register_manager.lockRegAssumeUnused(reg),
            else => null,
        };
        defer if (dst_lock) |lock| cg.register_manager.unlockReg(lock);

        try cg.load(dst_mcv, .{ .register = elem_ptr_reg }, base_ptr_ty);
        break :result dst_mcv;
    };
    return cg.finishAir(inst, result, .{ bin_op.lhs, bin_op.rhs, .none });
}

fn airPtrElemPtr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const ty_pl = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_pl;
    const extra = cg.air.extraData(Air.Bin, ty_pl.payload).data;

    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const elem_ptr_ty = cg.typeOfIndex(inst);
        const base_ptr_ty = cg.typeOf(extra.lhs);

        if (elem_ptr_ty.ptrInfo(zcu).flags.vector_index != .none) {
            @panic("audit");
        }

        const base_ptr_mcv = try cg.resolveInst(extra.lhs);
        const base_ptr_lock: ?RegisterLock = switch (base_ptr_mcv) {
            .register => |reg| cg.register_manager.lockRegAssumeUnused(reg),
            else => null,
        };
        defer if (base_ptr_lock) |lock| cg.register_manager.unlockReg(lock);

        const index_mcv = try cg.resolveInst(extra.rhs);
        const index_lock: ?RegisterLock = switch (index_mcv) {
            .register => |reg| cg.register_manager.lockRegAssumeUnused(reg),
            else => null,
        };
        defer if (index_lock) |lock| cg.register_manager.unlockReg(lock);

        const result_reg, const result_lock = try cg.allocReg(.int);
        defer cg.register_manager.unlockReg(result_lock);

        try cg.genBinOp(
            .ptr_add,
            base_ptr_mcv,
            base_ptr_ty,
            index_mcv,
            Type.u64,
            result_reg,
        );

        break :result MCValue{ .register = result_reg };
    };

    return cg.finishAir(inst, result, .{ extra.lhs, extra.rhs, .none });
}

fn airSetUnionTag(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const bin_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].bin_op;
    _ = bin_op;
    return cg.fail("TODO implement airSetUnionTag for {}", .{cg.target.cpu.arch});
    // return cg.finishAir(inst, result, .{ bin_op.lhs, bin_op.rhs, .none });
}

fn airGetUnionTag(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;

    const tag_ty = cg.typeOfIndex(inst);
    const union_ty = cg.typeOf(ty_op.operand);
    const layout = union_ty.unionGetLayout(zcu);

    if (layout.tag_size == 0) {
        return cg.finishAir(inst, .none, .{ ty_op.operand, .none, .none });
    }

    const operand = try cg.resolveInst(ty_op.operand);

    const frame_mcv = try cg.allocRegOrMem(union_ty, null, false);
    try cg.genCopy(union_ty, frame_mcv, operand);

    const tag_abi_size = tag_ty.abiSize(zcu);
    const result_reg, const result_lock = try cg.allocReg(.int);
    defer cg.register_manager.unlockReg(result_lock);

    switch (frame_mcv) {
        .load_frame => {
            if (tag_abi_size <= 8) {
                const off: i32 = if (layout.tag_align.compare(.lt, layout.payload_align))
                    @intCast(layout.payload_size)
                else
                    0;

                try cg.genCopy(
                    tag_ty,
                    .{ .register = result_reg },
                    frame_mcv.offset(off),
                );
            } else {
                return cg.fail(
                    "TODO implement get_union_tag for ABI larger than 8 bytes and operand {}, tag {}",
                    .{ frame_mcv, tag_ty.fmt(pt) },
                );
            }
        },
        else => return cg.fail("TODO: airGetUnionTag {s}", .{@tagName(operand)}),
    }

    return cg.finishAir(inst, .{ .register = result_reg }, .{ ty_op.operand, .none, .none });
}

fn airByteSwap(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const pt = cg.pt;
        const zcu = pt.zcu;
        const ty = cg.typeOf(ty_op.operand);
        const operand = try cg.resolveInst(ty_op.operand);

        switch (ty.zigTypeTag(zcu)) {
            .int => {
                const int_bits = ty.intInfo(zcu).bits;

                // bytes are no-op
                if (int_bits == 8 and cg.reuseOperand(inst, ty_op.operand, 0, operand)) {
                    return cg.finishAir(inst, operand, .{ ty_op.operand, .none, .none });
                }

                const dest_mcv = try cg.copyToNewRegister(inst, operand);
                const dest_reg = dest_mcv.register;

                switch (int_bits) {
                    16 => {
                        const temp_reg, const temp_lock = try cg.allocReg(.int);
                        defer cg.register_manager.unlockReg(temp_lock);

                        try cg.asmIType(.srli, temp_reg, dest_reg, .s(8));
                        try cg.asmIType(.slli, dest_reg, dest_reg, .s(8));
                        try cg.asmRType(.@"or", dest_reg, dest_reg, temp_reg);
                    },
                    else => return cg.fail("TODO: {d} bits for airByteSwap", .{int_bits}),
                }

                break :result dest_mcv;
            },
            else => return cg.fail("TODO: airByteSwap {}", .{ty.fmt(pt)}),
        }
    };
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn airBitReverse(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else return cg.fail("TODO implement airBitReverse for {}", .{cg.target.cpu.arch});
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn airUnaryMath(cg: *CodeGen, inst: Air.Inst.Index, tag: Air.Inst.Tag) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const un_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].un_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const ty = cg.typeOf(un_op);

        const operand = try cg.resolveInst(un_op);
        const operand_bit_size = ty.bitSize(zcu);

        if (!math.isPowerOfTwo(operand_bit_size))
            return cg.fail("TODO: airUnaryMath non-pow 2", .{});

        const operand_reg, const operand_lock = try cg.promoteReg(ty, operand);
        defer if (operand_lock) |lock| cg.register_manager.unlockReg(lock);

        const dst_class = cg.typeRegClass(ty);
        const dst_reg, const dst_lock = try cg.allocReg(dst_class);
        defer cg.register_manager.unlockReg(dst_lock);

        switch (ty.zigTypeTag(zcu)) {
            .float => {
                assert(dst_class == .float);

                switch (operand_bit_size) {
                    16, 80, 128 => return cg.fail("TODO: airUnaryMath Float bit-size {}", .{operand_bit_size}),
                    32, 64 => {},
                    else => unreachable,
                }

                switch (tag) {
                    .sqrt => {
                        _ = try cg.addInst(.{
                            .tag = if (operand_bit_size == 64) .fsqrtd else .fsqrts,
                            .data = .{
                                .r_type = .{
                                    .rd = dst_reg,
                                    .rs1 = operand_reg,
                                    .rs2 = .f0, // unused, spec says it's 0
                                },
                            },
                        });
                    },

                    else => return cg.fail("TODO: airUnaryMath Float {s}", .{@tagName(tag)}),
                }
            },
            .int => {
                assert(dst_class == .int);

                switch (tag) {
                    else => return cg.fail("TODO: airUnaryMath Float {s}", .{@tagName(tag)}),
                }
            },
            else => return cg.fail("TODO: airUnaryMath ty: {}", .{ty.fmt(pt)}),
        }

        break :result MCValue{ .register = dst_reg };
    };

    return cg.finishAir(inst, result, .{ un_op, .none, .none });
}

fn reuseOperand(
    cg: *CodeGen,
    inst: Air.Inst.Index,
    operand: Air.Inst.Ref,
    op_index: Liveness.OperandInt,
    mcv: MCValue,
) bool {
    return cg.reuseOperandAdvanced(inst, operand, op_index, mcv, inst);
}

fn reuseOperandAdvanced(
    cg: *CodeGen,
    inst: Air.Inst.Index,
    operand: Air.Inst.Ref,
    op_index: Liveness.OperandInt,
    mcv: MCValue,
    maybe_tracked_inst: ?Air.Inst.Index,
) bool {
    if (!cg.liveness.operandDies(inst, op_index))
        return false;

    switch (mcv) {
        .register,
        .register_pair,
        => for (mcv.getRegs()) |reg| {
            // If it's in the registers table, need to associate the register(s) with the
            // new instruction.
            if (maybe_tracked_inst) |tracked_inst| {
                if (!cg.register_manager.isRegFree(reg)) {
                    if (RegisterManager.indexOfRegIntoTracked(reg)) |index| {
                        cg.register_manager.registers[index] = tracked_inst;
                    }
                }
            } else cg.register_manager.freeReg(reg);
        },
        .load_frame => |frame_addr| if (frame_addr.index.isNamed()) return false,
        else => return false,
    }

    // Prevent the operand deaths processing code from deallocating it.
    cg.reused_operands.set(op_index);
    const op_inst = operand.toIndex().?;
    cg.getResolvedInstValue(op_inst).reuse(cg, maybe_tracked_inst, op_inst);

    return true;
}

fn airLoad(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const elem_ty = cg.typeOfIndex(inst);

    const result: MCValue = result: {
        if (!elem_ty.hasRuntimeBits(zcu))
            break :result .none;

        const ptr = try cg.resolveInst(ty_op.operand);
        const is_volatile = cg.typeOf(ty_op.operand).isVolatilePtr(zcu);
        if (cg.liveness.isUnused(inst) and !is_volatile)
            break :result .unreach;

        const elem_size = elem_ty.abiSize(zcu);

        const dst_mcv: MCValue = blk: {
            // The MCValue that holds the pointer can be re-used as the value.
            // - "ptr" is 8 bytes, and if the element is more than that, we cannot reuse it.
            //
            // - "ptr" will be stored in an integer register, so the type that we're gonna
            // load into it must also be a type that can be inside of an integer register
            if (elem_size <= 8 and
                (if (ptr == .register) cg.typeRegClass(elem_ty) == ptr.register.class() else true) and
                cg.reuseOperand(inst, ty_op.operand, 0, ptr))
            {
                break :blk ptr;
            } else {
                break :blk try cg.allocRegOrMem(elem_ty, inst, true);
            }
        };

        try cg.load(dst_mcv, ptr, cg.typeOf(ty_op.operand));
        break :result dst_mcv;
    };
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn load(cg: *CodeGen, dst_mcv: MCValue, ptr_mcv: MCValue, ptr_ty: Type) InnerError!void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const dst_ty = ptr_ty.childType(zcu);

    log.debug(
        "loading {}:{} into {}",
        .{ ptr_mcv, ptr_ty.fmt(pt), dst_mcv },
    );

    switch (ptr_mcv) {
        .none,
        .undef,
        .unreach,
        .dead,
        .register_pair,
        .reserved_frame,
        => unreachable, // not a valid pointer

        .immediate,
        .register,
        .register_offset,
        .lea_frame,
        .lea_symbol,
        .lea_tlv,
        => try cg.genCopy(dst_ty, dst_mcv, ptr_mcv.deref()),

        .memory,
        .indirect,
        .load_symbol,
        .load_frame,
        .load_tlv,
        => {
            const addr_reg = try cg.copyToTmpRegister(ptr_ty, ptr_mcv);
            const addr_lock = cg.register_manager.lockRegAssumeUnused(addr_reg);
            defer cg.register_manager.unlockReg(addr_lock);

            try cg.genCopy(dst_ty, dst_mcv, .{ .indirect = .{ .reg = addr_reg } });
        },
        .air_ref => |ptr_ref| try cg.load(dst_mcv, try cg.resolveInst(ptr_ref), ptr_ty),
    }
}

fn airStore(cg: *CodeGen, inst: Air.Inst.Index, safety: bool) !void {
    if (safety) {
        // TODO if the value is undef, write 0xaa bytes to dest
    } else {
        // TODO if the value is undef, don't lower this instruction
    }
    const bin_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].bin_op;
    const ptr = try cg.resolveInst(bin_op.lhs);
    const value = try cg.resolveInst(bin_op.rhs);
    const ptr_ty = cg.typeOf(bin_op.lhs);

    try cg.store(ptr, value, ptr_ty);

    return cg.finishAir(inst, .none, .{ bin_op.lhs, bin_op.rhs, .none });
}

/// Loads `value` into the "payload" of `pointer`.
fn store(cg: *CodeGen, ptr_mcv: MCValue, src_mcv: MCValue, ptr_ty: Type) !void {
    const zcu = cg.pt.zcu;
    const src_ty = ptr_ty.childType(zcu);
    log.debug(
        "storing {}:{} in {}:{}",
        .{ src_mcv, src_ty.fmt(cg.pt), ptr_mcv, ptr_ty.fmt(cg.pt) },
    );

    switch (ptr_mcv) {
        .none => unreachable,
        .undef => unreachable,
        .unreach => unreachable,
        .dead => unreachable,
        .register_pair => unreachable,
        .reserved_frame => unreachable,

        .immediate,
        .register,
        .register_offset,
        .lea_symbol,
        .lea_frame,
        .lea_tlv,
        => try cg.genCopy(src_ty, ptr_mcv.deref(), src_mcv),

        .memory,
        .indirect,
        .load_symbol,
        .load_frame,
        .load_tlv,
        => {
            const addr_reg = try cg.copyToTmpRegister(ptr_ty, ptr_mcv);
            const addr_lock = cg.register_manager.lockRegAssumeUnused(addr_reg);
            defer cg.register_manager.unlockReg(addr_lock);

            try cg.genCopy(src_ty, .{ .indirect = .{ .reg = addr_reg } }, src_mcv);
        },
        .air_ref => |ptr_ref| try cg.store(try cg.resolveInst(ptr_ref), src_mcv, ptr_ty),
    }
}

fn airStructFieldPtr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_pl = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_pl;
    const extra = cg.air.extraData(Air.StructField, ty_pl.payload).data;
    const result = try cg.fieldPtr(inst, extra.struct_operand, extra.field_index);
    return cg.finishAir(inst, result, .{ extra.struct_operand, .none, .none });
}

fn airStructFieldPtrIndex(cg: *CodeGen, inst: Air.Inst.Index, index: u8) !void {
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const result = try cg.fieldPtr(inst, ty_op.operand, index);
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn fieldPtr(cg: *CodeGen, inst: Air.Inst.Index, operand: Air.Inst.Ref, index: u32) !MCValue {
    const ptr_field_ty = cg.typeOfIndex(inst);
    const src_mcv = try cg.resolveInst(operand);
    const dst_mcv = if (switch (src_mcv) {
        .immediate, .lea_frame => true,
        .register, .register_offset => cg.reuseOperand(inst, operand, 0, src_mcv),
        else => false,
    }) src_mcv else try cg.copyToNewRegister(inst, src_mcv);
    return dst_mcv.offset(cg.fieldOffset(cg.typeOf(operand), ptr_field_ty, index));
}

fn fieldOffset(cg: *CodeGen, ptr_agg_ty: Type, ptr_field_ty: Type, field_index: u32) i32 {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const agg_ty = ptr_agg_ty.childType(zcu);
    return switch (agg_ty.containerLayout(zcu)) {
        .auto, .@"extern" => @intCast(agg_ty.structFieldOffset(field_index, zcu)),
        .@"packed" => @divExact(@as(i32, ptr_agg_ty.ptrInfo(zcu).packed_offset.bit_offset) +
            (if (zcu.typeToStruct(agg_ty)) |loaded_struct| pt.structPackedFieldBitOffset(loaded_struct, field_index) else 0) -
            ptr_field_ty.ptrInfo(zcu).packed_offset.bit_offset, 8),
    };
}

fn airStructFieldVal(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;

    const ty_pl = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_pl;
    const extra = cg.air.extraData(Air.StructField, ty_pl.payload).data;
    const operand = extra.struct_operand;
    const index = extra.field_index;

    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const src_mcv = try cg.resolveInst(operand);
        const struct_ty = cg.typeOf(operand);
        const field_ty = struct_ty.fieldType(index, zcu);
        if (!field_ty.hasRuntimeBitsIgnoreComptime(zcu)) break :result .none;

        const field_off: u32 = switch (struct_ty.containerLayout(zcu)) {
            .auto, .@"extern" => @intCast(struct_ty.structFieldOffset(index, zcu) * 8),
            .@"packed" => if (zcu.typeToStruct(struct_ty)) |struct_type|
                pt.structPackedFieldBitOffset(struct_type, index)
            else
                0,
        };

        switch (src_mcv) {
            .dead, .unreach => unreachable,
            .register => |src_reg| {
                const src_reg_lock = cg.register_manager.lockRegAssumeUnused(src_reg);
                defer cg.register_manager.unlockReg(src_reg_lock);

                const dst_reg = if (field_off == 0)
                    (try cg.copyToNewRegister(inst, src_mcv)).register
                else
                    try cg.copyToTmpRegister(Type.u64, .{ .register = src_reg });

                const dst_mcv: MCValue = .{ .register = dst_reg };
                const dst_lock = cg.register_manager.lockReg(dst_reg);
                defer if (dst_lock) |lock| cg.register_manager.unlockReg(lock);

                if (field_off > 0) {
                    try cg.asmIType(.srli, dst_reg, dst_reg, .u(@intCast(field_off)));
                }

                if (field_off == 0) {
                    try cg.truncateRegister(field_ty, dst_reg);
                }

                break :result if (field_off == 0) dst_mcv else try cg.copyToNewRegister(inst, dst_mcv);
            },
            .load_frame => {
                const field_abi_size: u32 = @intCast(field_ty.abiSize(zcu));
                if (field_off % 8 == 0) {
                    const field_byte_off = @divExact(field_off, 8);
                    const off_mcv = src_mcv.address().offset(@intCast(field_byte_off)).deref();
                    const field_bit_size = field_ty.bitSize(zcu);

                    if (field_abi_size <= 8) {
                        const dst_ty = if (field_ty.isRuntimeFloat())
                            field_ty
                        else
                            try pt.intType(
                                if (field_ty.isAbiInt(zcu)) field_ty.intInfo(zcu).signedness else .unsigned,
                                @intCast(field_bit_size),
                            );

                        const dst_reg, const dst_lock = try cg.allocReg(cg.typeRegClass(dst_ty));
                        const dst_mcv = MCValue{ .register = dst_reg };
                        defer cg.register_manager.unlockReg(dst_lock);

                        try cg.genCopy(dst_ty, dst_mcv, off_mcv);
                        break :result try cg.copyToNewRegister(inst, dst_mcv);
                    }

                    const container_abi_size: u32 = @intCast(struct_ty.abiSize(zcu));
                    const dst_mcv = if (field_byte_off + field_abi_size <= container_abi_size and
                        cg.reuseOperand(inst, operand, 0, src_mcv))
                        off_mcv
                    else dst: {
                        const dst_mcv = try cg.allocRegOrMem(cg.typeOfIndex(inst), inst, true);
                        try cg.genCopy(field_ty, dst_mcv, off_mcv);
                        break :dst dst_mcv;
                    };
                    if (field_abi_size * 8 > field_bit_size and dst_mcv.isMemory()) {
                        const tmp_reg, const tmp_lock = try cg.allocReg(.int);
                        defer cg.register_manager.unlockReg(tmp_lock);

                        const hi_mcv =
                            dst_mcv.address().offset(@intCast(field_bit_size / 64 * 8)).deref();
                        try cg.genSetReg(Type.u64, tmp_reg, hi_mcv);
                        try cg.genCopy(Type.u64, hi_mcv, .{ .register = tmp_reg });
                    }
                    break :result dst_mcv;
                }

                return cg.fail("TODO: airStructFieldVal load_frame field_off non multiple of 8", .{});
            },
            else => return cg.fail("TODO: airStructField {s}", .{@tagName(src_mcv)}),
        }
    };

    return cg.finishAir(inst, result, .{ extra.struct_operand, .none, .none });
}

fn airFieldParentPtr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    _ = inst;
    return cg.fail("TODO implement codegen airFieldParentPtr", .{});
}

fn genArgDbgInfo(cg: *const CodeGen, inst: Air.Inst.Index, mcv: MCValue) InnerError!void {
    const arg = cg.air.instructions.items(.data)[@intFromEnum(inst)].arg;
    const ty = arg.ty.toType();
    if (arg.name == .none) return;

    switch (cg.debug_output) {
        .dwarf => |dw| switch (mcv) {
            .register => |reg| dw.genLocalDebugInfo(
                .local_arg,
                arg.name.toSlice(cg.air),
                ty,
                .{ .reg = reg.dwarfNum() },
            ) catch |err| return cg.fail("failed to generate debug info: {s}", .{@errorName(err)}),
            .load_frame => {},
            else => {},
        },
        .plan9 => {},
        .none => {},
    }
}

fn airArg(cg: *CodeGen, inst: Air.Inst.Index) InnerError!void {
    // we skip over args that have no bits
    var arg_index = cg.arg_index;
    while (cg.args[arg_index] == .none) arg_index += 1;
    cg.arg_index = arg_index + 1;

    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const src_mcv = cg.args[arg_index];
        const arg_ty = cg.typeOfIndex(inst);
        switch (src_mcv) {
            .register, .register_pair, .load_frame => {
                for (src_mcv.getRegs()) |reg| cg.register_manager.getRegAssumeFree(reg, inst);
                break :result src_mcv;
            },
            .indirect => |reg_off| {
                cg.register_manager.getRegAssumeFree(reg_off.reg, inst);
                defer cg.register_manager.freeReg(reg_off.reg);
                const dst_mcv = try cg.allocRegOrMem(cg.typeOfIndex(inst), inst, false);
                try cg.genCopy(arg_ty, dst_mcv, src_mcv);
                break :result dst_mcv;
            },
            else => return cg.fail("TODO implement arg for {}", .{src_mcv}),
        }
    };

    return cg.finishAir(inst, result, .{ .none, .none, .none });
}

fn airBreakpoint(cg: *CodeGen) !void {
    _ = try cg.addInst(.{
        .tag = .ebreak,
        .data = .none,
    });
}

fn airRetAddr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const dst_mcv = try cg.allocRegOrMem(cg.typeOfIndex(inst), inst, true);
    try cg.genCopy(Type.u64, dst_mcv, .{ .load_frame = .{ .index = .ret_addr } });
    return cg.finishAir(inst, dst_mcv, .{ .none, .none, .none });
}

fn airFrameAddress(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const dst_mcv = try cg.allocRegOrMem(cg.typeOfIndex(inst), inst, true);
    try cg.genCopy(Type.u64, dst_mcv, .{ .lea_frame = .{ .index = .base_ptr } });
    return cg.finishAir(inst, dst_mcv, .{ .none, .none, .none });
}

fn airCall(cg: *CodeGen, inst: Air.Inst.Index, modifier: std.builtin.CallModifier) !void {
    if (modifier == .always_tail) return cg.fail("TODO implement tail calls for riscv64", .{});
    const pl_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].pl_op;
    const callee = pl_op.operand;
    const extra = cg.air.extraData(Air.Call, pl_op.payload);
    const arg_refs: []const Air.Inst.Ref = @ptrCast(cg.air.extra[extra.end..][0..extra.data.args_len]);

    const expected_num_args = 8;
    const ExpectedContents = extern struct {
        vals: [expected_num_args][@sizeOf(MCValue)]u8 align(@alignOf(MCValue)),
    };
    var stack align(@max(@alignOf(ExpectedContents), @alignOf(std.heap.StackFallbackAllocator(0)))) =
        std.heap.stackFallback(@sizeOf(ExpectedContents), cg.gpa);
    const allocator = stack.get();

    const arg_tys = try allocator.alloc(Type, arg_refs.len);
    defer allocator.free(arg_tys);
    for (arg_tys, arg_refs) |*arg_ty, arg_ref| arg_ty.* = cg.typeOf(arg_ref);

    const arg_vals = try allocator.alloc(MCValue, arg_refs.len);
    defer allocator.free(arg_vals);
    for (arg_vals, arg_refs) |*arg_val, arg_ref| arg_val.* = .{ .air_ref = arg_ref };

    const call_ret = try cg.genCall(.{ .air = callee }, arg_tys, arg_vals);

    var bt = cg.liveness.iterateBigTomb(inst);
    try cg.feed(&bt, pl_op.operand);
    for (arg_refs) |arg_ref| try cg.feed(&bt, arg_ref);

    const result = if (cg.liveness.isUnused(inst)) .unreach else call_ret;
    return cg.finishAirResult(inst, result);
}

fn spillCallerPreservedRegs(cg: *CodeGen, cc: std.builtin.CallingConvention.Tag) !void {
    switch (cc) {
        inline .auto, .riscv64_lp64 => inline for (comptime abi.Registers.all_caller_preserved) |reg|
            try cg.register_manager.getKnownReg(reg, null),
        else => unreachable,
    }
}

fn genCall(
    cg: *CodeGen,
    info: union(enum) {
        air: Air.Inst.Ref,
        lib: struct {
            return_type: InternPool.Index,
            param_types: []const InternPool.Index,
            lib: ?[]const u8 = null,
            callee: []const u8,
        },
    },
    arg_tys: []const Type,
    args: []const MCValue,
) !MCValue {
    const pt = cg.pt;
    const zcu = pt.zcu;

    const fn_ty = switch (info) {
        .air => |callee| fn_info: {
            const callee_ty = cg.typeOf(callee);
            break :fn_info switch (callee_ty.zigTypeTag(zcu)) {
                .@"fn" => callee_ty,
                .pointer => callee_ty.childType(zcu),
                else => unreachable,
            };
        },
        .lib => |lib| try pt.funcType(.{
            .param_types = lib.param_types,
            .return_type = lib.return_type,
            .cc = cg.target.cCallingConvention().?,
        }),
    };

    const fn_info = zcu.typeToFunc(fn_ty).?;

    const allocator = cg.gpa;

    const var_args = try allocator.alloc(Type, args.len - fn_info.param_types.len);
    defer allocator.free(var_args);
    for (var_args, arg_tys[fn_info.param_types.len..]) |*var_arg, arg_ty| var_arg.* = arg_ty;

    var call_info = try cg.resolveCallingConventionValues(fn_info, var_args, .call_frame);
    defer call_info.deinit(cg);

    // We need a properly aligned and sized call frame to be able to call this function.
    {
        const needed_call_frame = FrameAlloc.init(.{
            .size = call_info.stack_byte_count,
            .alignment = call_info.stack_align,
        });
        const frame_allocs_slice = cg.frame_allocs.slice();
        const stack_frame_size =
            &frame_allocs_slice.items(.abi_size)[@intFromEnum(FrameIndex.call_frame)];
        stack_frame_size.* = @max(stack_frame_size.*, needed_call_frame.abi_size);
        const stack_frame_align =
            &frame_allocs_slice.items(.abi_align)[@intFromEnum(FrameIndex.call_frame)];
        stack_frame_align.* = stack_frame_align.max(needed_call_frame.abi_align);
    }

    try cg.spillCallerPreservedRegs(fn_info.cc);

    var reg_locks = std.ArrayList(?RegisterLock).init(allocator);
    defer reg_locks.deinit();
    try reg_locks.ensureTotalCapacity(8);
    defer for (reg_locks.items) |reg_lock| if (reg_lock) |lock| cg.register_manager.unlockReg(lock);

    const frame_indices = try allocator.alloc(FrameIndex, args.len);
    defer allocator.free(frame_indices);

    switch (call_info.return_value.long) {
        .none, .unreach => {},
        .indirect => |reg_off| try cg.register_manager.getReg(reg_off.reg, null),
        else => unreachable,
    }
    for (call_info.args, args, arg_tys, frame_indices) |dst_arg, src_arg, arg_ty, *frame_index| {
        switch (dst_arg) {
            .none => {},
            .register => |reg| {
                try cg.register_manager.getReg(reg, null);
                try reg_locks.append(cg.register_manager.lockReg(reg));
            },
            .register_pair => |regs| {
                for (regs) |reg| try cg.register_manager.getReg(reg, null);
                try reg_locks.appendSlice(&cg.register_manager.lockRegs(2, regs));
            },
            .load_frame => {
                try cg.genCopy(arg_ty, dst_arg, src_arg);
                try cg.freeValue(src_arg);
            },
            .indirect => |reg_off| {
                frame_index.* = try cg.allocFrameIndex(FrameAlloc.initType(arg_ty, zcu));
                try cg.genSetMem(.{ .frame = frame_index.* }, 0, arg_ty, src_arg);
                try cg.register_manager.getReg(reg_off.reg, null);
                try reg_locks.append(cg.register_manager.lockReg(reg_off.reg));
            },
            else => return cg.fail("TODO: genCall set arg {s}", .{@tagName(dst_arg)}),
        }
    }

    switch (call_info.return_value.long) {
        .none, .unreach => {},
        .indirect => |reg_off| {
            const ret_ty = Type.fromInterned(fn_info.return_type);
            const frame_index = try cg.allocFrameIndex(.initSpill(ret_ty, zcu));
            try cg.genSetReg(Type.u64, reg_off.reg, .{
                .lea_frame = .{ .index = frame_index, .off = -reg_off.off },
            });
            call_info.return_value.short = .{ .load_frame = .{ .index = frame_index } };
            try reg_locks.append(cg.register_manager.lockReg(reg_off.reg));
        },
        else => unreachable,
    }

    // If the function call returns a value in a register, we want to make sure
    // we own that register, and spill anything that was using it before.
    switch (call_info.return_value.short) {
        .register => |reg| try cg.register_manager.getReg(reg, null),
        else => {},
    }

    for (call_info.args, arg_tys, args, frame_indices) |dst_arg, arg_ty, src_arg, frame_index| {
        switch (dst_arg) {
            .none, .load_frame => {},
            .register_pair => try cg.genCopy(arg_ty, dst_arg, src_arg),
            .register => |dst_reg| try cg.genSetReg(
                arg_ty,
                dst_reg,
                src_arg,
            ),
            .indirect => |reg_off| try cg.genSetReg(Type.u64, reg_off.reg, .{
                .lea_frame = .{ .index = frame_index, .off = -reg_off.off },
            }),
            else => return cg.fail("TODO: genCall actual set {s}", .{@tagName(dst_arg)}),
        }
    }

    // Due to incremental compilation, how function calls are generated depends
    // on linking.
    switch (info) {
        .air => |callee| {
            if (try cg.air.value(callee, pt)) |func_value| {
                const func_key = zcu.intern_pool.indexToKey(func_value.ip_index);
                switch (switch (func_key) {
                    else => func_key,
                    .ptr => |ptr| if (ptr.byte_offset == 0) switch (ptr.base_addr) {
                        .nav => |nav| zcu.intern_pool.indexToKey(zcu.navValue(nav).toIntern()),
                        else => func_key,
                    } else func_key,
                }) {
                    .func => |func_val| {
                        const elf_file = cg.bin_file.cast(.elf).?;
                        const zo = elf_file.zigObjectPtr().?;
                        const sym_index = try zo.getOrCreateMetadataForNav(zcu, func_val.owner_nav);

                        if (cg.mod.pic) {
                            return cg.fail("TODO: genCall pic", .{});
                        } else {
                            try cg.genSetReg(Type.u64, .ra, .{ .lea_symbol = .{ .sym_index = sym_index } });
                            try cg.asmIType(.jalr, .ra, .ra, .s(0));
                        }
                    },
                    .@"extern" => |@"extern"| {
                        const lib_name = @"extern".lib_name.toSlice(&zcu.intern_pool);
                        const name = @"extern".name.toSlice(&zcu.intern_pool);
                        const atom_index = try cg.owner.getSymbolIndex(cg);

                        const elf_file = cg.bin_file.cast(.elf).?;
                        _ = try cg.addInst(.{
                            .tag = .pseudo_extern_fn_reloc,
                            .data = .{ .reloc = .{
                                .register = .ra,
                                .atom_index = atom_index,
                                .sym_index = try elf_file.getGlobalSymbol(name, lib_name),
                            } },
                        });
                    },
                    else => return cg.fail("TODO implement calling bitcasted functions", .{}),
                }
            } else {
                assert(cg.typeOf(callee).zigTypeTag(zcu) == .pointer);
                const addr_reg, const addr_lock = try cg.allocReg(.int);
                defer cg.register_manager.unlockReg(addr_lock);

                try cg.genSetReg(Type.u64, addr_reg, .{ .air_ref = callee });
                try cg.asmIType(.jalr, .ra, addr_reg, .s(0));
            }
        },
        .lib => return cg.fail("TODO: lib cg calls", .{}),
    }

    // reset the vector settings as they might have changed in the function
    cg.avl = null;
    cg.vtype = null;

    return call_info.return_value.short;
}

fn airRet(cg: *CodeGen, inst: Air.Inst.Index, safety: bool) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const un_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].un_op;

    if (safety) {
        // safe
    } else {
        // not safe
    }

    const ret_ty = cg.fn_type.fnReturnType(zcu);
    switch (cg.ret_mcv.short) {
        .none => {},
        .register,
        .register_pair,
        => {
            if (ret_ty.isVector(zcu)) {
                const bit_size = ret_ty.totalVectorBits(zcu);

                // set the vtype to hold the entire vector's contents in a single element
                const vsew = bits.VSew.fromBits(bit_size) orelse unreachable;
                try cg.setVl(.zero, 0, .{
                    .vsew = vsew,
                    .vlmul = .m1,
                    .vma = true,
                    .vta = true,
                });
            }

            try cg.genCopy(ret_ty, cg.ret_mcv.short, .{ .air_ref = un_op });
        },
        .indirect => |reg_off| {
            try cg.register_manager.getReg(reg_off.reg, null);
            const lock = cg.register_manager.lockRegAssumeUnused(reg_off.reg);
            defer cg.register_manager.unlockReg(lock);

            try cg.genSetReg(Type.u64, reg_off.reg, cg.ret_mcv.long);
            try cg.genSetMem(
                .{ .reg = reg_off.reg },
                reg_off.off,
                ret_ty,
                .{ .air_ref = un_op },
            );
        },
        else => unreachable,
    }

    cg.ret_mcv.liveOut(cg, inst);
    try cg.finishAir(inst, .unreach, .{ un_op, .none, .none });

    // Just add space for an instruction, reloced this later
    const index = try cg.addInst(.{
        .tag = .pseudo_j,
        .data = .{ .j_type = .{
            .rd = .zero,
            .inst = undefined,
        } },
    });
    try cg.epilogue_relocs.append(cg.gpa, index);
}

fn airRetLoad(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const un_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].un_op;
    const ptr = try cg.resolveInst(un_op);

    const ptr_ty = cg.typeOf(un_op);
    switch (cg.ret_mcv.short) {
        .none => {},
        .register, .register_pair => try cg.load(cg.ret_mcv.short, ptr, ptr_ty),
        .indirect => |reg_off| try cg.genSetReg(ptr_ty, reg_off.reg, ptr),
        else => unreachable,
    }
    cg.ret_mcv.liveOut(cg, inst);
    try cg.finishAir(inst, .unreach, .{ un_op, .none, .none });

    // Just add space for an instruction, reloced this later
    const index = try cg.addInst(.{
        .tag = .pseudo_j,
        .data = .{ .j_type = .{
            .rd = .zero,
            .inst = undefined,
        } },
    });

    try cg.epilogue_relocs.append(cg.gpa, index);
}

fn airCmp(cg: *CodeGen, inst: Air.Inst.Index, tag: Air.Inst.Tag) !void {
    const bin_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].bin_op;
    const pt = cg.pt;
    const zcu = pt.zcu;
    const ip = &zcu.intern_pool;

    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const lhs_ty = cg.typeOf(bin_op.lhs);

        switch (lhs_ty.zigTypeTag(zcu)) {
            .int,
            .@"enum",
            .bool,
            .pointer,
            .error_set,
            .optional,
            .@"struct",
            => {
                const int_ty = switch (lhs_ty.zigTypeTag(zcu)) {
                    .@"enum" => lhs_ty.intTagType(zcu),
                    .int => lhs_ty,
                    .bool => Type.u1,
                    .pointer => Type.u64,
                    .error_set => Type.anyerror,
                    .optional => blk: {
                        const payload_ty = lhs_ty.optionalChild(zcu);
                        if (!payload_ty.hasRuntimeBitsIgnoreComptime(zcu)) {
                            break :blk Type.u1;
                        } else if (lhs_ty.isPtrLikeOptional(zcu)) {
                            break :blk Type.u64;
                        } else {
                            return cg.fail("TODO riscv cmp non-pointer optionals", .{});
                        }
                    },
                    .@"struct" => blk: {
                        const struct_obj = ip.loadStructType(lhs_ty.toIntern());
                        assert(struct_obj.layout == .@"packed");
                        const backing_index = struct_obj.backingIntTypeUnordered(ip);
                        break :blk Type.fromInterned(backing_index);
                    },
                    else => unreachable,
                };

                const int_info = int_ty.intInfo(zcu);
                if (int_info.bits <= 64) {
                    break :result try cg.binOp(inst, tag, bin_op.lhs, bin_op.rhs);
                } else {
                    return cg.fail("TODO riscv cmp for ints > 64 bits", .{});
                }
            },
            .float => {
                const float_bits = lhs_ty.floatBits(cg.target.*);
                const float_reg_size: u32 = if (cg.hasFeature(.d)) 64 else 32;
                if (float_bits > float_reg_size) {
                    return cg.fail("TODO: airCmp float > 64/32 bits", .{});
                }
                break :result try cg.binOp(inst, tag, bin_op.lhs, bin_op.rhs);
            },
            else => unreachable,
        }
    };

    return cg.finishAir(inst, result, .{ bin_op.lhs, bin_op.rhs, .none });
}

fn airCmpVector(cg: *CodeGen, inst: Air.Inst.Index) !void {
    _ = inst;
    return cg.fail("TODO implement airCmpVector for {}", .{cg.target.cpu.arch});
}

fn airCmpLtErrorsLen(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const un_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].un_op;
    const operand = try cg.resolveInst(un_op);
    _ = operand;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else return cg.fail("TODO implement airCmpLtErrorsLen for {}", .{cg.target.cpu.arch});
    return cg.finishAir(inst, result, .{ un_op, .none, .none });
}

fn airDbgStmt(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const dbg_stmt = cg.air.instructions.items(.data)[@intFromEnum(inst)].dbg_stmt;

    _ = try cg.addInst(.{
        .tag = .pseudo_dbg_line_column,
        .data = .{ .pseudo_dbg_line_column = .{
            .line = dbg_stmt.line,
            .column = dbg_stmt.column,
        } },
    });
}

fn airDbgInlineBlock(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_pl = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_pl;
    const extra = cg.air.extraData(Air.DbgInlineBlock, ty_pl.payload);
    try cg.lowerBlock(inst, @ptrCast(cg.air.extra[extra.end..][0..extra.data.body_len]));
}

fn airDbgVar(cg: *CodeGen, inst: Air.Inst.Index) InnerError!void {
    const pl_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].pl_op;
    const operand = pl_op.operand;
    const ty = cg.typeOf(operand);
    const mcv = try cg.resolveInst(operand);
    const name: Air.NullTerminatedString = @enumFromInt(pl_op.payload);

    const tag = cg.air.instructions.items(.tag)[@intFromEnum(inst)];
    cg.genVarDbgInfo(tag, ty, mcv, name.toSlice(cg.air)) catch |err|
        return cg.fail("failed to generate variable debug info: {s}", .{@errorName(err)});

    return cg.finishAir(inst, .unreach, .{ operand, .none, .none });
}

fn genVarDbgInfo(
    cg: CodeGen,
    tag: Air.Inst.Tag,
    ty: Type,
    mcv: MCValue,
    name: []const u8,
) !void {
    switch (cg.debug_output) {
        .dwarf => |dwarf| {
            const loc: link.File.Dwarf.Loc = switch (mcv) {
                .register => |reg| .{ .reg = reg.dwarfNum() },
                .memory => |address| .{ .constu = address },
                .immediate => |x| .{ .constu = x },
                .none => .empty,
                else => blk: {
                    // log.warn("TODO generate debug info for {}", .{mcv});
                    break :blk .empty;
                },
            };
            try dwarf.genLocalDebugInfo(switch (tag) {
                else => unreachable,
                .dbg_var_ptr, .dbg_var_val => .local_var,
                .dbg_arg_inline => .local_arg,
            }, name, ty, loc);
        },
        .plan9 => {},
        .none => {},
    }
}

fn airCondBr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pl_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].pl_op;
    const cond = try cg.resolveInst(pl_op.operand);
    const cond_ty = cg.typeOf(pl_op.operand);
    const extra = cg.air.extraData(Air.CondBr, pl_op.payload);
    const then_body: []const Air.Inst.Index = @ptrCast(cg.air.extra[extra.end..][0..extra.data.then_body_len]);
    const else_body: []const Air.Inst.Index = @ptrCast(cg.air.extra[extra.end + then_body.len ..][0..extra.data.else_body_len]);
    const liveness_cond_br = cg.liveness.getCondBr(inst);

    // If the condition dies here in this condbr instruction, process
    // that death now instead of later as this has an effect on
    // whether it needs to be spilled in the branches
    if (cg.liveness.operandDies(inst, 0)) {
        if (pl_op.operand.toIndex()) |op_inst| try cg.processDeath(op_inst);
    }

    const state = try cg.saveState();
    const reloc = try cg.condBr(cond_ty, cond);

    for (liveness_cond_br.then_deaths) |death| try cg.processDeath(death);
    try cg.genBody(then_body);
    try cg.restoreState(state, &.{}, .{
        .emit_instructions = false,
        .update_tracking = true,
        .resurrect = true,
        .close_scope = true,
    });

    cg.performReloc(reloc);

    for (liveness_cond_br.else_deaths) |death| try cg.processDeath(death);
    try cg.genBody(else_body);
    try cg.restoreState(state, &.{}, .{
        .emit_instructions = false,
        .update_tracking = true,
        .resurrect = true,
        .close_scope = true,
    });
}

fn condBr(cg: *CodeGen, cond_ty: Type, condition: MCValue) !Mir.Inst.Index {
    const cond_reg = try cg.copyToTmpRegister(cond_ty, condition);

    return try cg.addInst(.{
        .tag = .beq,
        .data = .{
            .b_type = .{
                .rs1 = cond_reg,
                .rs2 = .zero,
                .inst = undefined,
            },
        },
    });
}

fn isNull(cg: *CodeGen, inst: Air.Inst.Index, opt_ty: Type, opt_mcv: MCValue) !MCValue {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const pl_ty = opt_ty.optionalChild(zcu);

    const some_info: struct { off: i32, ty: Type } = if (opt_ty.optionalReprIsPayload(zcu))
        .{ .off = 0, .ty = if (pl_ty.isSlice(zcu)) pl_ty.slicePtrFieldType(zcu) else pl_ty }
    else
        .{ .off = @intCast(pl_ty.abiSize(zcu)), .ty = Type.bool };

    const return_mcv = try cg.allocRegOrMem(cg.typeOfIndex(inst), inst, true);
    assert(return_mcv == .register); // should not be larger 8 bytes
    const return_reg = return_mcv.register;

    switch (opt_mcv) {
        .none,
        .unreach,
        .dead,
        .undef,
        .immediate,
        .register_offset,
        .lea_frame,
        .lea_symbol,
        .reserved_frame,
        .air_ref,
        .register_pair,
        => unreachable,

        .register => |opt_reg| {
            if (some_info.off == 0) {
                _ = try cg.addInst(.{
                    .tag = .pseudo_compare,
                    .data = .{
                        .compare = .{
                            .op = .eq,
                            .rd = return_reg,
                            .rs1 = opt_reg,
                            .rs2 = try cg.copyToTmpRegister(
                                some_info.ty,
                                .{ .immediate = 0 },
                            ),
                            .ty = Type.bool,
                        },
                    },
                });
                return return_mcv;
            }
            assert(some_info.ty.ip_index == .bool_type);
            const bit_offset: u7 = @intCast(some_info.off * 8);

            try cg.genBinOp(
                .shr,
                .{ .register = opt_reg },
                Type.u64,
                .{ .immediate = bit_offset },
                Type.u8,
                return_reg,
            );
            try cg.truncateRegister(Type.u8, return_reg);
            try cg.genBinOp(
                .cmp_eq,
                .{ .register = return_reg },
                Type.u64,
                .{ .immediate = 0 },
                Type.u8,
                return_reg,
            );

            return return_mcv;
        },

        .load_frame => {
            const opt_reg = try cg.copyToTmpRegister(
                some_info.ty,
                opt_mcv.address().offset(some_info.off).deref(),
            );
            const opt_reg_lock = cg.register_manager.lockRegAssumeUnused(opt_reg);
            defer cg.register_manager.unlockReg(opt_reg_lock);

            _ = try cg.addInst(.{
                .tag = .pseudo_compare,
                .data = .{
                    .compare = .{
                        .op = .eq,
                        .rd = return_reg,
                        .rs1 = opt_reg,
                        .rs2 = try cg.copyToTmpRegister(
                            some_info.ty,
                            .{ .immediate = 0 },
                        ),
                        .ty = Type.bool,
                    },
                },
            });
            return return_mcv;
        },

        else => return cg.fail("TODO: isNull {}", .{opt_mcv}),
    }
}

fn airIsNull(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const un_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].un_op;
    const operand = try cg.resolveInst(un_op);
    const ty = cg.typeOf(un_op);
    const result = try cg.isNull(inst, ty, operand);
    return cg.finishAir(inst, result, .{ un_op, .none, .none });
}

fn airIsNullPtr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const un_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].un_op;
    const operand = try cg.resolveInst(un_op);
    _ = operand;
    const ty = cg.typeOf(un_op);
    _ = ty;

    if (true) return cg.fail("TODO: airIsNullPtr", .{});

    return cg.finishAir(inst, .unreach, .{ un_op, .none, .none });
}

fn airIsNonNull(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const un_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].un_op;
    const operand = try cg.resolveInst(un_op);
    const ty = cg.typeOf(un_op);
    const result = try cg.isNull(inst, ty, operand);
    assert(result == .register);

    _ = try cg.addInst(.{
        .tag = .pseudo_not,
        .data = .{
            .rr = .{
                .rd = result.register,
                .rs = result.register,
            },
        },
    });

    return cg.finishAir(inst, result, .{ un_op, .none, .none });
}

fn airIsNonNullPtr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const un_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].un_op;
    const operand = try cg.resolveInst(un_op);
    _ = operand;
    const ty = cg.typeOf(un_op);
    _ = ty;

    if (true) return cg.fail("TODO: airIsNonNullPtr", .{});

    return cg.finishAir(inst, .unreach, .{ un_op, .none, .none });
}

fn airIsErr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const un_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].un_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const operand = try cg.resolveInst(un_op);
        const operand_ty = cg.typeOf(un_op);
        break :result try cg.isErr(inst, operand_ty, operand);
    };
    return cg.finishAir(inst, result, .{ un_op, .none, .none });
}

fn airIsErrPtr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const un_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].un_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const operand_ptr = try cg.resolveInst(un_op);
        const operand: MCValue = blk: {
            if (cg.reuseOperand(inst, un_op, 0, operand_ptr)) {
                // The MCValue that holds the pointer can be re-used as the value.
                break :blk operand_ptr;
            } else {
                break :blk try cg.allocRegOrMem(cg.typeOfIndex(inst), inst, true);
            }
        };
        try cg.load(operand, operand_ptr, cg.typeOf(un_op));
        const operand_ptr_ty = cg.typeOf(un_op);
        const operand_ty = operand_ptr_ty.childType(zcu);

        break :result try cg.isErr(inst, operand_ty, operand);
    };
    return cg.finishAir(inst, result, .{ un_op, .none, .none });
}

/// Generates a compare instruction which will indicate if `eu_mcv` is an error.
///
/// Result is in the return register.
fn isErr(cg: *CodeGen, maybe_inst: ?Air.Inst.Index, eu_ty: Type, eu_mcv: MCValue) !MCValue {
    _ = maybe_inst;
    const zcu = cg.pt.zcu;
    const err_ty = eu_ty.errorUnionSet(zcu);
    if (err_ty.errorSetIsEmpty(zcu)) return MCValue{ .immediate = 0 }; // always false
    const err_off: u31 = @intCast(errUnionErrorOffset(eu_ty.errorUnionPayload(zcu), zcu));

    const return_reg, const return_lock = try cg.allocReg(.int);
    defer cg.register_manager.unlockReg(return_lock);

    switch (eu_mcv) {
        .register => |reg| {
            const eu_lock = cg.register_manager.lockReg(reg);
            defer if (eu_lock) |lock| cg.register_manager.unlockReg(lock);

            try cg.genCopy(eu_ty, .{ .register = return_reg }, eu_mcv);

            if (err_off > 0) {
                try cg.genBinOp(
                    .shr,
                    .{ .register = return_reg },
                    eu_ty,
                    .{ .immediate = @as(u6, @intCast(err_off * 8)) },
                    Type.u8,
                    return_reg,
                );
            }

            try cg.genBinOp(
                .cmp_neq,
                .{ .register = return_reg },
                Type.anyerror,
                .{ .immediate = 0 },
                Type.u8,
                return_reg,
            );
        },
        .load_frame => |frame_addr| {
            try cg.genBinOp(
                .cmp_neq,
                .{ .load_frame = .{
                    .index = frame_addr.index,
                    .off = frame_addr.off + err_off,
                } },
                Type.anyerror,
                .{ .immediate = 0 },
                Type.anyerror,
                return_reg,
            );
        },
        else => return cg.fail("TODO implement isErr for {}", .{eu_mcv}),
    }

    return .{ .register = return_reg };
}

fn airIsNonErr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const un_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].un_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const operand = try cg.resolveInst(un_op);
        const ty = cg.typeOf(un_op);
        break :result try cg.isNonErr(inst, ty, operand);
    };
    return cg.finishAir(inst, result, .{ un_op, .none, .none });
}

fn isNonErr(cg: *CodeGen, inst: Air.Inst.Index, eu_ty: Type, eu_mcv: MCValue) !MCValue {
    const is_err_res = try cg.isErr(inst, eu_ty, eu_mcv);
    switch (is_err_res) {
        .register => |reg| {
            _ = try cg.addInst(.{
                .tag = .pseudo_not,
                .data = .{
                    .rr = .{
                        .rd = reg,
                        .rs = reg,
                    },
                },
            });
            return is_err_res;
        },
        // always false case
        .immediate => |imm| {
            assert(imm == 0);
            return MCValue{ .immediate = @intFromBool(imm == 0) };
        },
        else => unreachable,
    }
}

fn airIsNonErrPtr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const un_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].un_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const operand_ptr = try cg.resolveInst(un_op);
        const operand: MCValue = blk: {
            if (cg.reuseOperand(inst, un_op, 0, operand_ptr)) {
                // The MCValue that holds the pointer can be re-used as the value.
                break :blk operand_ptr;
            } else {
                break :blk try cg.allocRegOrMem(cg.typeOfIndex(inst), inst, true);
            }
        };
        const operand_ptr_ty = cg.typeOf(un_op);
        const operand_ty = operand_ptr_ty.childType(zcu);

        try cg.load(operand, operand_ptr, cg.typeOf(un_op));
        break :result try cg.isNonErr(inst, operand_ty, operand);
    };
    return cg.finishAir(inst, result, .{ un_op, .none, .none });
}

fn airLoop(cg: *CodeGen, inst: Air.Inst.Index) !void {
    // A loop is a setup to be able to jump back to the beginning.
    const ty_pl = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_pl;
    const loop = cg.air.extraData(Air.Block, ty_pl.payload);
    const body: []const Air.Inst.Index = @ptrCast(cg.air.extra[loop.end..][0..loop.data.body_len]);

    const state = try cg.saveState();

    try cg.loops.putNoClobber(cg.gpa, inst, .{
        .state = state,
        .jmp_target = @intCast(cg.mir_instructions.len),
    });
    defer assert(cg.loops.remove(inst));

    try cg.genBody(body);
}

/// Send control flow to the `index` of `cg.code`.
fn jump(cg: *CodeGen, index: Mir.Inst.Index) !Mir.Inst.Index {
    return cg.addInst(.{
        .tag = .pseudo_j,
        .data = .{ .j_type = .{
            .rd = .zero,
            .inst = index,
        } },
    });
}

fn lowerBlock(cg: *CodeGen, inst: Air.Inst.Index, body: []const Air.Inst.Index) !void {
    // A block is a setup to be able to jump to the end.
    const inst_tracking_i = cg.inst_tracking.count();
    cg.inst_tracking.putAssumeCapacityNoClobber(inst, .init(.unreach));

    try cg.blocks.putNoClobber(cg.gpa, inst, .{ .state = cg.initRetroactiveState() });
    const liveness = cg.liveness.getBlock(inst);

    // TODO emit debug info lexical block
    try cg.genBody(body);

    var block_data = cg.blocks.fetchRemove(inst).?;
    defer block_data.value.deinit(cg.gpa);
    if (block_data.value.relocs.items.len > 0) {
        try cg.restoreState(block_data.value.state, liveness.deaths, .{
            .emit_instructions = false,
            .update_tracking = true,
            .resurrect = true,
            .close_scope = true,
        });
        for (block_data.value.relocs.items) |reloc| cg.performReloc(reloc);
    }

    if (std.debug.runtime_safety) assert(cg.inst_tracking.getIndex(inst).? == inst_tracking_i);
    const tracking = &cg.inst_tracking.values()[inst_tracking_i];
    if (cg.liveness.isUnused(inst)) try tracking.die(cg, inst);
    cg.getValueIfFree(tracking.short, inst);
}

fn airSwitchBr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const switch_br = cg.air.unwrapSwitch(inst);
    const condition = try cg.resolveInst(switch_br.operand);

    // If the condition dies here in this switch instruction, process
    // that death now instead of later as this has an effect on
    // whether it needs to be spilled in the branches
    if (cg.liveness.operandDies(inst, 0)) {
        if (switch_br.operand.toIndex()) |op_inst| try cg.processDeath(op_inst);
    }

    try cg.lowerSwitchBr(inst, switch_br, condition);
}

fn lowerSwitchBr(
    cg: *CodeGen,
    inst: Air.Inst.Index,
    switch_br: Air.UnwrappedSwitch,
    condition: MCValue,
) !void {
    const condition_ty = cg.typeOf(switch_br.operand);
    const liveness = try cg.liveness.getSwitchBr(cg.gpa, inst, switch_br.cases_len + 1);
    defer cg.gpa.free(liveness.deaths);

    const state = try cg.saveState();

    var it = switch_br.iterateCases();
    while (it.next()) |case| {
        var relocs = try cg.gpa.alloc(Mir.Inst.Index, case.items.len + case.ranges.len);
        defer cg.gpa.free(relocs);

        for (case.items, relocs[0..case.items.len]) |item, *reloc| {
            const item_mcv = try cg.resolveInst(item);

            const cond_lock = switch (condition) {
                .register => cg.register_manager.lockRegAssumeUnused(condition.register),
                else => null,
            };
            defer if (cond_lock) |lock| cg.register_manager.unlockReg(lock);

            const cmp_reg, const cmp_lock = try cg.allocReg(.int);
            defer cg.register_manager.unlockReg(cmp_lock);

            try cg.genBinOp(
                .cmp_neq,
                condition,
                condition_ty,
                item_mcv,
                condition_ty,
                cmp_reg,
            );

            reloc.* = try cg.condBr(condition_ty, .{ .register = cmp_reg });
        }

        for (case.ranges, relocs[case.items.len..]) |range, *reloc| {
            const min_mcv = try cg.resolveInst(range[0]);
            const max_mcv = try cg.resolveInst(range[1]);
            const cond_lock = switch (condition) {
                .register => cg.register_manager.lockRegAssumeUnused(condition.register),
                else => null,
            };
            defer if (cond_lock) |lock| cg.register_manager.unlockReg(lock);

            const temp_cmp_reg, const temp_cmp_lock = try cg.allocReg(.int);
            defer cg.register_manager.unlockReg(temp_cmp_lock);

            // is `condition` less than `min`? is "true", we've failed
            try cg.genBinOp(
                .cmp_gte,
                condition,
                condition_ty,
                min_mcv,
                condition_ty,
                temp_cmp_reg,
            );

            // if the compare was true, we will jump to the fail case and fall through
            // to the next checks
            const lt_fail_reloc = try cg.condBr(condition_ty, .{ .register = temp_cmp_reg });
            try cg.genBinOp(
                .cmp_gt,
                condition,
                condition_ty,
                max_mcv,
                condition_ty,
                temp_cmp_reg,
            );

            reloc.* = try cg.condBr(condition_ty, .{ .register = temp_cmp_reg });
            cg.performReloc(lt_fail_reloc);
        }

        const skip_case_reloc = try cg.jump(undefined);

        for (liveness.deaths[case.idx]) |operand| try cg.processDeath(operand);

        for (relocs) |reloc| cg.performReloc(reloc);
        try cg.genBody(case.body);
        try cg.restoreState(state, &.{}, .{
            .emit_instructions = false,
            .update_tracking = true,
            .resurrect = true,
            .close_scope = true,
        });

        cg.performReloc(skip_case_reloc);
    }

    if (switch_br.else_body_len > 0) {
        const else_body = it.elseBody();

        const else_deaths = liveness.deaths.len - 1;
        for (liveness.deaths[else_deaths]) |operand| try cg.processDeath(operand);

        try cg.genBody(else_body);
        try cg.restoreState(state, &.{}, .{
            .emit_instructions = false,
            .update_tracking = true,
            .resurrect = true,
            .close_scope = true,
        });
    }
}

fn airLoopSwitchBr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const switch_br = cg.air.unwrapSwitch(inst);
    const condition = try cg.resolveInst(switch_br.operand);

    const mat_cond = if (condition.isMutable() and
        cg.reuseOperand(inst, switch_br.operand, 0, condition))
        condition
    else mat_cond: {
        const ty = cg.typeOf(switch_br.operand);
        const mat_cond = try cg.allocRegOrMem(ty, inst, true);
        try cg.genCopy(ty, mat_cond, condition);
        break :mat_cond mat_cond;
    };
    cg.inst_tracking.putAssumeCapacityNoClobber(inst, InstTracking.init(mat_cond));

    // If the condition dies here in this switch instruction, process
    // that death now instead of later as this has an effect on
    // whether it needs to be spilled in the branches
    if (cg.liveness.operandDies(inst, 0)) {
        if (switch_br.operand.toIndex()) |op_inst| try cg.processDeath(op_inst);
    }

    const state = try cg.saveState();

    try cg.loops.putNoClobber(cg.gpa, inst, .{
        .state = state,
        .jmp_target = @intCast(cg.mir_instructions.len),
    });
    defer assert(cg.loops.remove(inst));

    // Stop tracking block result without forgetting tracking info
    try cg.freeValue(mat_cond);

    try cg.lowerSwitchBr(inst, switch_br, mat_cond);

    try cg.processDeath(inst);
}

fn airSwitchDispatch(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const br = cg.air.instructions.items(.data)[@intFromEnum(inst)].br;

    const block_ty = cg.typeOfIndex(br.block_inst);
    const block_tracking = cg.inst_tracking.getPtr(br.block_inst).?;
    const loop_data = cg.loops.getPtr(br.block_inst).?;
    done: {
        try cg.getValue(block_tracking.short, null);
        const src_mcv = try cg.resolveInst(br.operand);

        if (cg.reuseOperandAdvanced(inst, br.operand, 0, src_mcv, br.block_inst)) {
            try cg.getValue(block_tracking.short, br.block_inst);
            // .long = .none to avoid merging operand and block result stack frames.
            const current_tracking: InstTracking = .{ .long = .none, .short = src_mcv };
            try current_tracking.materializeUnsafe(cg, br.block_inst, block_tracking.*);
            for (current_tracking.getRegs()) |src_reg| cg.register_manager.freeReg(src_reg);
            break :done;
        }

        try cg.getValue(block_tracking.short, br.block_inst);
        const dst_mcv = block_tracking.short;
        try cg.genCopy(block_ty, dst_mcv, try cg.resolveInst(br.operand));
        break :done;
    }

    // Process operand death so that it is properly accounted for in the State below.
    if (cg.liveness.operandDies(inst, 0)) {
        if (br.operand.toIndex()) |op_inst| try cg.processDeath(op_inst);
    }

    try cg.restoreState(loop_data.state, &.{}, .{
        .emit_instructions = true,
        .update_tracking = false,
        .resurrect = false,
        .close_scope = false,
    });

    // Emit a jump with a relocation. It will be patched up after the block ends.
    // Leave the jump offset undefined
    _ = try cg.jump(loop_data.jmp_target);

    // Stop tracking block result without forgetting tracking info
    try cg.freeValue(block_tracking.short);
}

fn performReloc(cg: *CodeGen, inst: Mir.Inst.Index) void {
    const tag = cg.mir_instructions.items(.tag)[inst];
    const target: Mir.Inst.Index = @intCast(cg.mir_instructions.len);

    switch (tag) {
        .beq,
        .bne,
        => cg.mir_instructions.items(.data)[inst].b_type.inst = target,
        .jal => cg.mir_instructions.items(.data)[inst].j_type.inst = target,
        .pseudo_j => cg.mir_instructions.items(.data)[inst].j_type.inst = target,
        else => std.debug.panic("TODO: performReloc {s}", .{@tagName(tag)}),
    }
}

fn airBr(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const zcu = cg.pt.zcu;
    const br = cg.air.instructions.items(.data)[@intFromEnum(inst)].br;

    const block_ty = cg.typeOfIndex(br.block_inst);
    const block_unused =
        !block_ty.hasRuntimeBitsIgnoreComptime(zcu) or cg.liveness.isUnused(br.block_inst);
    const block_tracking = cg.inst_tracking.getPtr(br.block_inst).?;
    const block_data = cg.blocks.getPtr(br.block_inst).?;
    const first_br = block_data.relocs.items.len == 0;
    const block_result = result: {
        if (block_unused) break :result .none;

        if (!first_br) try cg.getValue(block_tracking.short, null);
        const src_mcv = try cg.resolveInst(br.operand);

        if (cg.reuseOperandAdvanced(inst, br.operand, 0, src_mcv, br.block_inst)) {
            if (first_br) break :result src_mcv;

            try cg.getValue(block_tracking.short, br.block_inst);
            // .long = .none to avoid merging operand and block result stack frames.
            const current_tracking: InstTracking = .{ .long = .none, .short = src_mcv };
            try current_tracking.materializeUnsafe(cg, br.block_inst, block_tracking.*);
            for (current_tracking.getRegs()) |src_reg| cg.register_manager.freeReg(src_reg);
            break :result block_tracking.short;
        }

        const dst_mcv = if (first_br) try cg.allocRegOrMem(block_ty, br.block_inst, true) else dst: {
            try cg.getValue(block_tracking.short, br.block_inst);
            break :dst block_tracking.short;
        };
        try cg.genCopy(block_ty, dst_mcv, try cg.resolveInst(br.operand));
        break :result dst_mcv;
    };

    // Process operand death so that it is properly accounted for in the State below.
    if (cg.liveness.operandDies(inst, 0)) {
        if (br.operand.toIndex()) |op_inst| try cg.processDeath(op_inst);
    }

    if (first_br) {
        block_tracking.* = InstTracking.init(block_result);
        try cg.saveRetroactiveState(&block_data.state);
    } else try cg.restoreState(block_data.state, &.{}, .{
        .emit_instructions = true,
        .update_tracking = false,
        .resurrect = false,
        .close_scope = false,
    });

    // Emit a jump with a relocation. It will be patched up after the block ends.
    // Leave the jump offset undefined
    const jmp_reloc = try cg.jump(undefined);
    try block_data.relocs.append(cg.gpa, jmp_reloc);

    // Stop tracking block result without forgetting tracking info
    try cg.freeValue(block_tracking.short);
}

fn airRepeat(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const loop_inst = cg.air.instructions.items(.data)[@intFromEnum(inst)].repeat.loop_inst;
    const repeat_info = cg.loops.get(loop_inst).?;
    try cg.restoreState(repeat_info.state, &.{}, .{
        .emit_instructions = true,
        .update_tracking = false,
        .resurrect = false,
        .close_scope = true,
    });
    _ = try cg.jump(repeat_info.jmp_target);
}

fn airAsm(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_pl = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_pl;
    const extra = cg.air.extraData(Air.Asm, ty_pl.payload);
    const clobbers_len: u31 = @truncate(extra.data.flags);
    var extra_i: usize = extra.end;
    const outputs: []const Air.Inst.Ref =
        @ptrCast(cg.air.extra[extra_i..][0..extra.data.outputs_len]);
    extra_i += outputs.len;
    const inputs: []const Air.Inst.Ref = @ptrCast(cg.air.extra[extra_i..][0..extra.data.inputs_len]);
    extra_i += inputs.len;

    var result: MCValue = .none;
    var args = std.ArrayList(MCValue).init(cg.gpa);
    try args.ensureTotalCapacity(outputs.len + inputs.len);
    defer {
        for (args.items) |arg| if (arg.getReg()) |reg| cg.register_manager.unlockReg(.{
            .tracked_index = RegisterManager.indexOfRegIntoTracked(reg) orelse continue,
        });
        args.deinit();
    }
    var arg_map = std.StringHashMap(u8).init(cg.gpa);
    try arg_map.ensureTotalCapacity(@intCast(outputs.len + inputs.len));
    defer arg_map.deinit();

    var outputs_extra_i = extra_i;
    for (outputs) |output| {
        const extra_bytes = mem.sliceAsBytes(cg.air.extra[extra_i..]);
        const constraint = mem.sliceTo(mem.sliceAsBytes(cg.air.extra[extra_i..]), 0);
        const name = mem.sliceTo(extra_bytes[constraint.len + 1 ..], 0);
        // This equation accounts for the fact that even if we have exactly 4 bytes
        // for the string, we still use the next u32 for the null terminator.
        extra_i += (constraint.len + name.len + (2 + 3)) / 4;

        const is_read = switch (constraint[0]) {
            '=' => false,
            '+' => read: {
                if (output == .none) return cg.fail(
                    "read-write constraint unsupported for asm result: '{s}'",
                    .{constraint},
                );
                break :read true;
            },
            else => return cg.fail("invalid constraint: '{s}'", .{constraint}),
        };
        const is_early_clobber = constraint[1] == '&';
        const rest = constraint[@as(usize, 1) + @intFromBool(is_early_clobber) ..];
        const arg_mcv: MCValue = arg_mcv: {
            const arg_maybe_reg: ?Register = if (mem.eql(u8, rest, "m"))
                if (output != .none) null else return cg.fail(
                    "memory constraint unsupported for asm result: '{s}'",
                    .{constraint},
                )
            else if (mem.startsWith(u8, rest, "{") and mem.endsWith(u8, rest, "}"))
                parseRegName(rest["{".len .. rest.len - "}".len]) orelse
                    return cg.fail("invalid register constraint: '{s}'", .{constraint})
            else if (rest.len == 1 and std.ascii.isDigit(rest[0])) {
                const index = std.fmt.charToDigit(rest[0], 10) catch unreachable;
                if (index >= args.items.len) return cg.fail("constraint out of bounds: '{s}'", .{
                    constraint,
                });
                break :arg_mcv args.items[index];
            } else return cg.fail("invalid constraint: '{s}'", .{constraint});
            break :arg_mcv if (arg_maybe_reg) |reg| .{ .register = reg } else arg: {
                const ptr_mcv = try cg.resolveInst(output);
                switch (ptr_mcv) {
                    .immediate => |addr| if (math.cast(i32, @as(i64, @bitCast(addr)))) |_|
                        break :arg ptr_mcv.deref(),
                    .register, .register_offset, .lea_frame => break :arg ptr_mcv.deref(),
                    else => {},
                }
                break :arg .{ .indirect = .{ .reg = try cg.copyToTmpRegister(Type.usize, ptr_mcv) } };
            };
        };
        if (arg_mcv.getReg()) |reg| if (RegisterManager.indexOfRegIntoTracked(reg)) |_| {
            _ = cg.register_manager.lockReg(reg);
        };
        if (!mem.eql(u8, name, "_"))
            arg_map.putAssumeCapacityNoClobber(name, @intCast(args.items.len));
        args.appendAssumeCapacity(arg_mcv);
        if (output == .none) result = arg_mcv;
        if (is_read) try cg.load(arg_mcv, .{ .air_ref = output }, cg.typeOf(output));
    }

    for (inputs) |input| {
        const input_bytes = mem.sliceAsBytes(cg.air.extra[extra_i..]);
        const constraint = mem.sliceTo(input_bytes, 0);
        const name = mem.sliceTo(input_bytes[constraint.len + 1 ..], 0);
        // This equation accounts for the fact that even if we have exactly 4 bytes
        // for the string, we still use the next u32 for the null terminator.
        extra_i += (constraint.len + name.len + (2 + 3)) / 4;

        const ty = cg.typeOf(input);
        const input_mcv = try cg.resolveInst(input);
        const arg_mcv: MCValue = if (mem.eql(u8, constraint, "X"))
            input_mcv
        else if (mem.startsWith(u8, constraint, "{") and mem.endsWith(u8, constraint, "}")) arg: {
            const reg = parseRegName(constraint["{".len .. constraint.len - "}".len]) orelse
                return cg.fail("invalid register constraint: '{s}'", .{constraint});
            try cg.register_manager.getReg(reg, null);
            try cg.genSetReg(ty, reg, input_mcv);
            break :arg .{ .register = reg };
        } else if (mem.eql(u8, constraint, "r")) arg: {
            switch (input_mcv) {
                .register => break :arg input_mcv,
                else => {},
            }
            const temp_reg = try cg.copyToTmpRegister(ty, input_mcv);
            break :arg .{ .register = temp_reg };
        } else return cg.fail("invalid input constraint: '{s}'", .{constraint});
        if (arg_mcv.getReg()) |reg| if (RegisterManager.indexOfRegIntoTracked(reg)) |_| {
            _ = cg.register_manager.lockReg(reg);
        };
        if (!mem.eql(u8, name, "_"))
            arg_map.putAssumeCapacityNoClobber(name, @intCast(args.items.len));
        args.appendAssumeCapacity(arg_mcv);
    }

    {
        var clobber_i: u32 = 0;
        while (clobber_i < clobbers_len) : (clobber_i += 1) {
            const clobber = std.mem.sliceTo(std.mem.sliceAsBytes(cg.air.extra[extra_i..]), 0);
            // This equation accounts for the fact that even if we have exactly 4 bytes
            // for the string, we still use the next u32 for the null terminator.
            extra_i += clobber.len / 4 + 1;

            if (std.mem.eql(u8, clobber, "") or std.mem.eql(u8, clobber, "memory")) {
                // nothing really to do
            } else {
                try cg.register_manager.getReg(parseRegName(clobber) orelse
                    return cg.fail("invalid clobber: '{s}'", .{clobber}), null);
            }
        }
    }

    const Label = struct {
        target: Mir.Inst.Index = undefined,
        pending_relocs: std.ArrayListUnmanaged(Mir.Inst.Index) = .empty,

        const Kind = enum { definition, reference };

        fn isValid(kind: Kind, name: []const u8) bool {
            for (name, 0..) |c, i| switch (c) {
                else => return false,
                '$' => if (i == 0) return false,
                '.' => {},
                '0'...'9' => if (i == 0) switch (kind) {
                    .definition => if (name.len != 1) return false,
                    .reference => {
                        if (name.len != 2) return false;
                        switch (name[1]) {
                            else => return false,
                            'B', 'F', 'b', 'f' => {},
                        }
                    },
                },
                '@', 'A'...'Z', '_', 'a'...'z' => {},
            };
            return name.len > 0;
        }
    };
    var labels: std.StringHashMapUnmanaged(Label) = .empty;
    defer {
        var label_it = labels.valueIterator();
        while (label_it.next()) |label| label.pending_relocs.deinit(cg.gpa);
        labels.deinit(cg.gpa);
    }

    const asm_source = std.mem.sliceAsBytes(cg.air.extra[extra_i..])[0..extra.data.source_len];
    var line_it = mem.tokenizeAny(u8, asm_source, "\n\r;");
    next_line: while (line_it.next()) |line| {
        var mnem_it = mem.tokenizeAny(u8, line, " \t");
        const mnem_str = while (mnem_it.next()) |mnem_str| {
            if (mem.startsWith(u8, mnem_str, "#")) continue :next_line;
            if (mem.startsWith(u8, mnem_str, "//")) continue :next_line;
            if (!mem.endsWith(u8, mnem_str, ":")) break mnem_str;
            const label_name = mnem_str[0 .. mnem_str.len - ":".len];
            if (!Label.isValid(.definition, label_name))
                return cg.fail("invalid label: '{s}'", .{label_name});

            const label_gop = try labels.getOrPut(cg.gpa, label_name);
            if (!label_gop.found_existing) label_gop.value_ptr.* = .{} else {
                const anon = std.ascii.isDigit(label_name[0]);
                if (!anon and label_gop.value_ptr.pending_relocs.items.len == 0)
                    return cg.fail("redefined label: '{s}'", .{label_name});
                for (label_gop.value_ptr.pending_relocs.items) |pending_reloc|
                    cg.performReloc(pending_reloc);
                if (anon)
                    label_gop.value_ptr.pending_relocs.clearRetainingCapacity()
                else
                    label_gop.value_ptr.pending_relocs.clearAndFree(cg.gpa);
            }
            label_gop.value_ptr.target = @intCast(cg.mir_instructions.len);
        } else continue;

        const instruction: union(enum) { mnem: Mnemonic, pseudo: Pseudo } =
            if (std.meta.stringToEnum(Mnemonic, mnem_str)) |mnem|
                .{ .mnem = mnem }
            else if (std.meta.stringToEnum(Pseudo, mnem_str)) |pseudo|
                .{ .pseudo = pseudo }
            else
                return cg.fail("invalid mnem str '{s}'", .{mnem_str});

        var ops: [3]InstructionOperand = .{.none} ** 3;
        var last_op = false;
        var op_it = mem.splitAny(u8, mnem_it.rest(), ",(");
        next_op: for (&ops) |*op| {
            const op_str = while (!last_op) {
                const full_str = op_it.next() orelse break :next_op;
                const code_str = if (mem.indexOfScalar(u8, full_str, '#') orelse
                    mem.indexOf(u8, full_str, "//")) |comment|
                code: {
                    last_op = true;
                    break :code full_str[0..comment];
                } else full_str;
                const trim_str = mem.trim(u8, code_str, " \t*");
                if (trim_str.len > 0) break trim_str;
            } else break;

            if (parseRegName(op_str)) |reg| {
                op.* = .{ .reg = reg };
            } else if (std.fmt.parseInt(i12, op_str, 10)) |int| {
                op.* = .{ .imm = Immediate.s(int) };
            } else |_| if (mem.startsWith(u8, op_str, "%[")) {
                const mod_index = mem.indexOf(u8, op_str, "]@");
                const modifier = if (mod_index) |index|
                    op_str[index + "]@".len ..]
                else
                    "";

                op.* = switch (args.items[
                    arg_map.get(op_str["%[".len .. mod_index orelse op_str.len - "]".len]) orelse
                        return cg.fail("no matching constraint: '{s}'", .{op_str})
                ]) {
                    .lea_symbol => |sym_off| if (mem.eql(u8, modifier, "plt")) blk: {
                        assert(sym_off.off == 0);
                        break :blk .{ .sym_index = sym_off };
                    } else return cg.fail("invalid modifier: '{s}'", .{modifier}),
                    .register => |reg| if (modifier.len == 0)
                        .{ .reg = reg }
                    else
                        return cg.fail("invalid modified '{s}'", .{modifier}),
                    else => return cg.fail("invalid constraint: '{s}'", .{op_str}),
                };
            } else if (mem.endsWith(u8, op_str, ")")) {
                const reg = op_str[0 .. op_str.len - ")".len];
                const addr_reg = parseRegName(reg) orelse
                    return cg.fail("expected valid register, found '{s}'", .{reg});

                op.* = .{ .reg = addr_reg };
            } else if (Label.isValid(.reference, op_str)) {
                const anon = std.ascii.isDigit(op_str[0]);
                const label_gop = try labels.getOrPut(cg.gpa, op_str[0..if (anon) 1 else op_str.len]);
                if (!label_gop.found_existing) label_gop.value_ptr.* = .{};
                if (anon and (op_str[1] == 'b' or op_str[1] == 'B') and !label_gop.found_existing)
                    return cg.fail("undefined label: '{s}'", .{op_str});
                const pending_relocs = &label_gop.value_ptr.pending_relocs;
                if (if (anon)
                    op_str[1] == 'f' or op_str[1] == 'F'
                else
                    !label_gop.found_existing or pending_relocs.items.len > 0)
                    try pending_relocs.append(cg.gpa, @intCast(cg.mir_instructions.len));
                op.* = .{ .inst = label_gop.value_ptr.target };
            } else return cg.fail("invalid operand: '{s}'", .{op_str});
        } else if (op_it.next()) |op_str| return cg.fail("extra operand: '{s}'", .{op_str});

        switch (instruction) {
            .mnem => |mnem| {
                cg.asmOps(mnem, ops) catch |err| switch (err) {
                    error.InvalidInstruction => return cg.fail(
                        "invalid instruction: {s} {s} {s} {s}",
                        .{
                            @tagName(mnem),
                            @tagName(ops[0]),
                            @tagName(ops[1]),
                            @tagName(ops[2]),
                        },
                    ),
                    else => |e| return e,
                };
            },
            .pseudo => |pseudo| {
                (@as(error{InvalidInstruction}!void, switch (pseudo) {
                    .li => blk: {
                        if (ops[0] != .reg or ops[1] != .imm) {
                            break :blk error.InvalidInstruction;
                        }

                        const reg = ops[0].reg;
                        const imm = ops[1].imm;

                        try cg.genSetReg(Type.usize, reg, .{ .immediate = imm.asBits(u64) });
                    },
                    .mv => blk: {
                        if (ops[0] != .reg or ops[1] != .reg) {
                            break :blk error.InvalidInstruction;
                        }

                        const dst = ops[0].reg;
                        const src = ops[1].reg;

                        if (dst.class() != .int or src.class() != .int) {
                            return cg.fail("pseudo instruction 'mv' only works on integer registers", .{});
                        }

                        try cg.genSetReg(Type.usize, dst, .{ .register = src });
                    },
                    .tail => blk: {
                        if (ops[0] != .sym_index) {
                            break :blk error.InvalidInstruction;
                        }

                        const sym_offset = ops[0].sym_index;
                        assert(sym_offset.off == 0);

                        const random_link_reg, const lock = try cg.allocReg(.int);
                        defer cg.register_manager.unlockReg(lock);

                        _ = try cg.addInst(.{
                            .tag = .pseudo_extern_fn_reloc,
                            .data = .{ .reloc = .{
                                .register = random_link_reg,
                                .atom_index = try cg.owner.getSymbolIndex(cg),
                                .sym_index = sym_offset.sym_index,
                            } },
                        });
                    },
                    .ret => try cg.asmIType(.jalr, .zero, .ra, .s(0)),
                    .beqz => blk: {
                        if (ops[0] != .reg or ops[1] != .inst) {
                            break :blk error.InvalidInstruction;
                        }

                        _ = try cg.addInst(.{
                            .tag = .beq,
                            .data = .{ .b_type = .{
                                .rs1 = ops[0].reg,
                                .rs2 = .zero,
                                .inst = ops[1].inst,
                            } },
                        });
                    },
                })) catch |err| {
                    switch (err) {
                        error.InvalidInstruction => return cg.fail(
                            "invalid instruction: {s} {s} {s} {s}",
                            .{
                                @tagName(pseudo),
                                @tagName(ops[0]),
                                @tagName(ops[1]),
                                @tagName(ops[2]),
                            },
                        ),
                        else => |e| return e,
                    }
                };
            },
        }
    }

    var label_it = labels.iterator();
    while (label_it.next()) |label| if (label.value_ptr.pending_relocs.items.len > 0)
        return cg.fail("undefined label: '{s}'", .{label.key_ptr.*});

    for (outputs, args.items[0..outputs.len]) |output, arg_mcv| {
        const extra_bytes = mem.sliceAsBytes(cg.air.extra[outputs_extra_i..]);
        const constraint =
            mem.sliceTo(mem.sliceAsBytes(cg.air.extra[outputs_extra_i..]), 0);
        const name = mem.sliceTo(extra_bytes[constraint.len + 1 ..], 0);
        // This equation accounts for the fact that even if we have exactly 4 bytes
        // for the string, we still use the next u32 for the null terminator.
        outputs_extra_i += (constraint.len + name.len + (2 + 3)) / 4;

        if (output == .none) continue;
        if (arg_mcv != .register) continue;
        if (constraint.len == 2 and std.ascii.isDigit(constraint[1])) continue;
        try cg.store(.{ .air_ref = output }, arg_mcv, cg.typeOf(output));
    }

    simple: {
        var buf = [1]Air.Inst.Ref{.none} ** (Liveness.bpi - 1);
        var buf_index: usize = 0;
        for (outputs) |output| {
            if (output == .none) continue;

            if (buf_index >= buf.len) break :simple;
            buf[buf_index] = output;
            buf_index += 1;
        }
        if (buf_index + inputs.len > buf.len) break :simple;
        @memcpy(buf[buf_index..][0..inputs.len], inputs);
        return cg.finishAir(inst, result, buf);
    }
    var bt = cg.liveness.iterateBigTomb(inst);
    for (outputs) |output| if (output != .none) try cg.feed(&bt, output);
    for (inputs) |input| try cg.feed(&bt, input);
    return cg.finishAirResult(inst, result);
}

/// Sets the value of `dst_mcv` to the value of `src_mcv`.
fn genCopy(cg: *CodeGen, ty: Type, dst_mcv: MCValue, src_mcv: MCValue) !void {
    // There isn't anything to store
    if (dst_mcv == .none) return;

    if (!dst_mcv.isMutable()) {
        // panic so we can see the trace
        return std.debug.panic("tried to genCopy immutable: {s}", .{@tagName(dst_mcv)});
    }

    const zcu = cg.pt.zcu;

    switch (dst_mcv) {
        .register => |reg| return cg.genSetReg(ty, reg, src_mcv),
        .register_offset => |dst_reg_off| try cg.genSetReg(ty, dst_reg_off.reg, switch (src_mcv) {
            .none,
            .unreach,
            .dead,
            .undef,
            => unreachable,
            .immediate,
            .register,
            .register_offset,
            => src_mcv.offset(-dst_reg_off.off),
            else => .{ .register_offset = .{
                .reg = try cg.copyToTmpRegister(ty, src_mcv),
                .off = -dst_reg_off.off,
            } },
        }),
        .indirect => |reg_off| try cg.genSetMem(
            .{ .reg = reg_off.reg },
            reg_off.off,
            ty,
            src_mcv,
        ),
        .load_frame => |frame_addr| try cg.genSetMem(
            .{ .frame = frame_addr.index },
            frame_addr.off,
            ty,
            src_mcv,
        ),
        .load_symbol, .load_tlv => {
            const addr_reg, const addr_lock = try cg.allocReg(.int);
            defer cg.register_manager.unlockReg(addr_lock);

            try cg.genSetReg(Type.u64, addr_reg, dst_mcv.address());
            try cg.genCopy(ty, .{ .indirect = .{ .reg = addr_reg } }, src_mcv);
        },
        .memory => return cg.fail("TODO: genCopy memory", .{}),
        .register_pair => |dst_regs| {
            const src_info: ?struct { addr_reg: Register, addr_lock: ?RegisterLock } = switch (src_mcv) {
                .register_pair, .memory, .indirect, .load_frame => null,
                .load_symbol => src: {
                    const src_addr_reg, const src_addr_lock = try cg.promoteReg(Type.u64, src_mcv.address());
                    errdefer cg.register_manager.unlockReg(src_addr_lock);

                    break :src .{ .addr_reg = src_addr_reg, .addr_lock = src_addr_lock };
                },
                .air_ref => |src_ref| return cg.genCopy(
                    ty,
                    dst_mcv,
                    try cg.resolveInst(src_ref),
                ),
                else => return cg.fail("genCopy register_pair src: {}", .{src_mcv}),
            };

            defer if (src_info) |info| {
                if (info.addr_lock) |lock| {
                    cg.register_manager.unlockReg(lock);
                }
            };

            var part_disp: i32 = 0;
            for (dst_regs, try cg.splitType(ty), 0..) |dst_reg, dst_ty, part_i| {
                try cg.genSetReg(dst_ty, dst_reg, switch (src_mcv) {
                    .register_pair => |src_regs| .{ .register = src_regs[part_i] },
                    .memory, .indirect, .load_frame => src_mcv.address().offset(part_disp).deref(),
                    .load_symbol => .{ .indirect = .{
                        .reg = src_info.?.addr_reg,
                        .off = part_disp,
                    } },
                    else => unreachable,
                });
                part_disp += @intCast(dst_ty.abiSize(zcu));
            }
        },
        else => return std.debug.panic("TODO: genCopy to {s} from {s}", .{ @tagName(dst_mcv), @tagName(src_mcv) }),
    }
}

fn genInlineMemcpy(
    cg: *CodeGen,
    dst_ptr: MCValue,
    src_ptr: MCValue,
    len: MCValue,
) !void {
    const regs = try cg.register_manager.allocRegs(4, .{null} ** 4, abi.Registers.Integer.temporary);
    const locks = cg.register_manager.lockRegsAssumeUnused(4, regs);
    defer for (locks) |lock| cg.register_manager.unlockReg(lock);

    const count = regs[0];
    const tmp = regs[1];
    const src = regs[2];
    const dst = regs[3];

    try cg.genSetReg(Type.u64, count, len);
    try cg.genSetReg(Type.u64, src, src_ptr);
    try cg.genSetReg(Type.u64, dst, dst_ptr);

    // if count is 0, there's nothing to copy
    _ = try cg.addInst(.{
        .tag = .beq,
        .data = .{ .b_type = .{
            .rs1 = count,
            .rs2 = .zero,
            .inst = @intCast(cg.mir_instructions.len + 9),
        } },
    });

    // lb tmp, 0(src)
    const first_inst = try cg.addInst(.{
        .tag = .lb,
        .data = .{
            .i_type = .{
                .rd = tmp,
                .rs1 = src,
                .imm12 = Immediate.s(0),
            },
        },
    });

    // sb tmp, 0(dst)
    try cg.asmIType(.sb, dst, tmp, .s(0));

    // dec count by 1
    try cg.asmIType(.addi, count, count, .s(-1));

    // branch if count is 0
    _ = try cg.addInst(.{
        .tag = .beq,
        .data = .{
            .b_type = .{
                .inst = @intCast(cg.mir_instructions.len + 4), // points after the last inst
                .rs1 = count,
                .rs2 = .zero,
            },
        },
    });

    // increment the pointers
    try cg.asmIType(.addi, src, src, .s(1));
    try cg.asmIType(.addi, dst, dst, .s(1));

    // jump back to start of loop
    _ = try cg.addInst(.{
        .tag = .pseudo_j,
        .data = .{ .j_type = .{
            .rd = .zero,
            .inst = first_inst,
        } },
    });
}

fn genInlineMemset(
    cg: *CodeGen,
    dst_ptr: MCValue,
    src_value: MCValue,
    len: MCValue,
) !void {
    const regs = try cg.register_manager.allocRegs(3, .{null} ** 3, abi.Registers.Integer.temporary);
    const locks = cg.register_manager.lockRegsAssumeUnused(3, regs);
    defer for (locks) |lock| cg.register_manager.unlockReg(lock);

    const count = regs[0];
    const src = regs[1];
    const dst = regs[2];

    try cg.genSetReg(Type.u64, count, len);
    try cg.genSetReg(Type.u64, src, src_value);
    try cg.genSetReg(Type.u64, dst, dst_ptr);

    // sb src, 0(dst)
    const first_inst = try cg.addInst(.{
        .tag = .sb,
        .data = .{
            .i_type = .{
                .rd = dst,
                .rs1 = src,
                .imm12 = Immediate.s(0),
            },
        },
    });

    // dec count by 1
    try cg.asmIType(.addi, count, count, .s(-1));

    // branch if count is 0
    _ = try cg.addInst(.{
        .tag = .beq,
        .data = .{
            .b_type = .{
                .inst = @intCast(cg.mir_instructions.len + 3), // points after the last inst
                .rs1 = count,
                .rs2 = .zero,
            },
        },
    });

    // increment the pointers
    try cg.asmIType(.addi, dst, dst, .s(1));

    // jump back to start of loop
    _ = try cg.addInst(.{
        .tag = .pseudo_j,
        .data = .{ .j_type = .{
            .rd = .zero,
            .inst = first_inst,
        } },
    });
}

/// Sets the value of `src_mcv` into `reg`. Assumes you have a lock on it.
fn genSetReg(cg: *CodeGen, ty: Type, reg: Register, src_mcv: MCValue) InnerError!void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const abi_size: u32 = @intCast(ty.abiSize(zcu));
    const bit_size = if (ty.zigTypeTag(zcu) == .vector)
        ty.childType(zcu).bitSize(zcu)
    else
        ty.bitSize(zcu);

    const max_size: u32 = switch (reg.class()) {
        .int => 64,
        .float => if (cg.hasFeature(.d)) 64 else 32,
        .vector => @intCast(ty.bitSize(zcu)), // it can be larger than a register because of the multiplier
    };
    if (bit_size > max_size) return std.debug.panic(
        "tried to set {s} reg with size {}",
        .{ @tagName(reg.class()), abi_size },
    );
    const dst_reg_class = reg.class();

    switch (src_mcv) {
        .unreach,
        .none,
        .dead,
        => unreachable,
        .undef => |sym_index| {
            if (!cg.wantSafety())
                return;

            if (sym_index) |index| {
                return cg.genSetReg(ty, reg, .{ .load_symbol = .{ .sym_index = index } });
            }

            switch (abi_size) {
                1 => return cg.genSetReg(ty, reg, .{ .immediate = 0xAA }),
                2 => return cg.genSetReg(ty, reg, .{ .immediate = 0xAAAA }),
                3...4 => return cg.genSetReg(ty, reg, .{ .immediate = 0xAAAAAAAA }),
                5...8 => return cg.genSetReg(ty, reg, .{ .immediate = 0xAAAAAAAAAAAAAAAA }),
                else => unreachable,
            }
        },
        .immediate => |unsigned_x| {
            assert(dst_reg_class == .int);

            const x: i64 = @bitCast(unsigned_x);
            if (std.math.cast(i12, x)) |casted| {
                try cg.asmIType(.addi, reg, .zero, .s(casted));
            } else if (std.math.cast(i32, x)) |casted| {
                const lo12: i12 = @truncate(casted);
                const carry: i32 = if (lo12 < 0) 1 else 0;
                const hi20: i20 = @truncate((casted >> 12) +% carry);

                try cg.asmUType(.lui, reg, .s(hi20));
                try cg.asmIType(.addiw, reg, reg, .s(lo12));
            } else {
                const value = try pt.intValue_u64(.u64, unsigned_x);
                const imm_mcv = try cg.lowerUav(value);
                try cg.genSetReg(ty, reg, imm_mcv);
            }
        },
        .register => |src_reg| {
            // If the registers are the same, nothing to do.
            if (src_reg.id() == reg.id())
                return;

            if (src_reg.class() == .vector and reg.class() == .int) {
                assert(ty.isVector(zcu)); // pass in the vector type
                const elem_bits = ty.childType(zcu).bitSize(zcu);
                try cg.setVl(.zero, 0, .{
                    .vsew = bits.VSew.fromBits(elem_bits) orelse
                        return cg.fail(
                            "TODO: genSetReg vec -> int bits {d}, make sure to pass in the vector type itself",
                            .{elem_bits},
                        ),
                    .vlmul = try cg.suggestedVlMul(ty),
                    .vta = true,
                    .vma = true,
                });
            }

            const dst_class = reg.class();
            const src_class = src_reg.class();

            switch (src_class) {
                .float => switch (dst_class) {
                    .float => try cg.asmRType(
                        if (cg.hasFeature(.d)) .fsgnjnd else .fsgnjns,
                        reg,
                        src_reg,
                        src_reg,
                    ),
                    .int, .vector => return cg.fail("TODO: lowerMir pseudo_mv float -> {s}", .{@tagName(dst_class)}),
                },
                .int => switch (dst_class) {
                    .int => try cg.asmIType(.addi, reg, src_reg, .s(0)),
                    .vector => try cg.asmRType(.vmvvx, reg, src_reg, .zero),
                    .float => return cg.fail("TODO: lowerMir pseudo_mv int -> {s}", .{@tagName(dst_class)}),
                },
                .vector => switch (dst_class) {
                    .int => try cg.asmRType(.vmvxs, reg, .zero, src_reg),
                    .float, .vector => return cg.fail("TODO: lowerMir pseudo_mv vector -> {s}", .{@tagName(dst_class)}),
                },
            }
        },
        // useful in cases like slice_ptr, which can easily reuse the operand
        // but we need to get only the pointer out.
        .register_pair => |pair| try cg.genSetReg(ty, reg, .{ .register = pair[0] }),
        .load_frame => |frame| {
            if (reg.class() == .vector) {
                // vectors don't support an offset memory load so we need to put the true
                // address into a register before loading from it.
                const addr_reg, const addr_lock = try cg.allocReg(.int);
                defer cg.register_manager.unlockReg(addr_lock);

                try cg.genCopy(Type.u64, .{ .register = addr_reg }, src_mcv.address());
                try cg.genCopy(ty, .{ .register = reg }, .{ .indirect = .{ .reg = addr_reg } });
            } else {
                _ = try cg.addInst(.{
                    .tag = .pseudo_load_rm,
                    .data = .{ .rm = .{
                        .r = reg,
                        .m = .{
                            .base = .{ .frame = frame.index },
                            .mod = .{
                                .size = cg.memSize(ty),
                                .unsigned = ty.isUnsignedInt(zcu),
                                .disp = frame.off,
                            },
                        },
                    } },
                });
            }
        },
        .memory => |addr| {
            try cg.genSetReg(ty, reg, .{ .immediate = addr });
            try cg.asmIType(.ld, reg, reg, .u(0));
        },
        .lea_frame, .register_offset => {
            _ = try cg.addInst(.{
                .tag = .pseudo_lea_rm,
                .data = .{
                    .rm = .{
                        .r = reg,
                        .m = switch (src_mcv) {
                            .register_offset => |reg_off| .{
                                .base = .{ .reg = reg_off.reg },
                                .mod = .{
                                    .size = .byte, // the size doesn't matter
                                    .disp = reg_off.off,
                                    .unsigned = false,
                                },
                            },
                            .lea_frame => |frame| .{
                                .base = .{ .frame = frame.index },
                                .mod = .{
                                    .size = .byte, // the size doesn't matter
                                    .disp = frame.off,
                                    .unsigned = false,
                                },
                            },
                            else => unreachable,
                        },
                    },
                },
            });
        },
        .indirect => |reg_off| {
            const signed = if (cg.intInfo(ty)) |info| info.signedness == .signed else false;
            const load_tag: Mnemonic = switch (reg.class()) {
                .float => switch (abi_size) {
                    1 => unreachable, // Zig does not support 8-bit floats
                    2 => return cg.fail("TODO: genSetReg indirect 16-bit float", .{}),
                    4 => .flw,
                    8 => .fld,
                    else => return std.debug.panic("TODO: genSetReg for float size {d}", .{abi_size}),
                },
                .int => switch (abi_size) {
                    1...1 => if (signed) .lb else .lbu,
                    2...2 => if (signed) .lh else .lhu,
                    3...4 => if (signed) .lw else .lwu,
                    5...8 => .ld,
                    else => return std.debug.panic("TODO: genSetReg for int size {d}", .{abi_size}),
                },
                .vector => {
                    assert(reg_off.off == 0);

                    // There is no vector instruction for loading with an offset to a base register,
                    // so we need to get an offset register containing the address of the vector first
                    // and load from it.
                    const len = ty.vectorLen(zcu);
                    const elem_ty = ty.childType(zcu);
                    const elem_size = elem_ty.bitSize(zcu);

                    try cg.setVl(.zero, len, .{
                        .vsew = bits.VSew.fromBits(elem_size) orelse unreachable,
                        .vlmul = try cg.suggestedVlMul(ty),
                        .vma = true,
                        .vta = true,
                    });

                    _ = try cg.addInst(.{
                        .tag = .pseudo_load_rm,
                        .data = .{ .rm = .{
                            .r = reg,
                            .m = .{
                                .base = .{ .reg = reg_off.reg },
                                .mod = .{
                                    .size = cg.memSize(elem_ty),
                                    .unsigned = false,
                                    .disp = 0,
                                },
                            },
                        } },
                    });

                    return;
                },
            };

            try cg.asmIType(load_tag, reg, reg_off.reg, .s(reg_off.off));
        },
        .lea_symbol => |sym_off| {
            assert(sym_off.off == 0);
            const atom_index = try cg.owner.getSymbolIndex(cg);

            _ = try cg.addInst(.{
                .tag = .pseudo_load_symbol,
                .data = .{ .reloc = .{
                    .register = reg,
                    .atom_index = atom_index,
                    .sym_index = sym_off.sym_index,
                } },
            });
        },
        .load_symbol => {
            const addr_reg, const addr_lock = try cg.allocReg(.int);
            defer cg.register_manager.unlockReg(addr_lock);

            try cg.genSetReg(Type.u64, addr_reg, src_mcv.address());
            try cg.genSetReg(ty, reg, .{ .indirect = .{ .reg = addr_reg } });
        },
        .lea_tlv => |sym_index| {
            const atom_index = try cg.owner.getSymbolIndex(cg);
            _ = try cg.addInst(.{
                .tag = .pseudo_load_tlv,
                .data = .{ .reloc = .{
                    .register = reg,
                    .atom_index = atom_index,
                    .sym_index = sym_index,
                } },
            });
        },
        .load_tlv => {
            const addr_reg, const addr_lock = try cg.allocReg(.int);
            defer cg.register_manager.unlockReg(addr_lock);

            try cg.genSetReg(ty, addr_reg, src_mcv.address());
            try cg.genSetReg(ty, reg, .{ .indirect = .{ .reg = addr_reg } });
        },
        .air_ref => |ref| try cg.genSetReg(ty, reg, try cg.resolveInst(ref)),
        else => return cg.fail("TODO: genSetReg {s}", .{@tagName(src_mcv)}),
    }
}

fn genSetMem(
    cg: *CodeGen,
    base: Memory.Base,
    disp: i32,
    ty: Type,
    src_mcv: MCValue,
) InnerError!void {
    const pt = cg.pt;
    const zcu = pt.zcu;

    const abi_size: u32 = @intCast(ty.abiSize(zcu));
    const dst_ptr_mcv: MCValue = switch (base) {
        .reg => |base_reg| .{ .register_offset = .{ .reg = base_reg, .off = disp } },
        .frame => |base_frame_index| .{ .lea_frame = .{ .index = base_frame_index, .off = disp } },
    };
    switch (src_mcv) {
        .none,
        .unreach,
        .dead,
        .reserved_frame,
        => unreachable,
        .undef => |sym_index| {
            if (sym_index) |index| {
                return cg.genSetMem(base, disp, ty, .{ .load_symbol = .{ .sym_index = index } });
            }

            try cg.genInlineMemset(
                dst_ptr_mcv,
                src_mcv,
                .{ .immediate = abi_size },
            );
        },
        .register_offset,
        .memory,
        .indirect,
        .load_frame,
        .lea_frame,
        .load_symbol,
        .lea_symbol,
        => switch (abi_size) {
            0 => {},
            1, 2, 4, 8 => {
                const reg = try cg.register_manager.allocReg(null, abi.Registers.Integer.temporary);
                const src_lock = cg.register_manager.lockRegAssumeUnused(reg);
                defer cg.register_manager.unlockReg(src_lock);

                try cg.genSetReg(ty, reg, src_mcv);
                try cg.genSetMem(base, disp, ty, .{ .register = reg });
            },
            else => try cg.genInlineMemcpy(
                dst_ptr_mcv,
                src_mcv.address(),
                .{ .immediate = abi_size },
            ),
        },
        .register => |reg| {
            if (reg.class() == .vector) {
                const addr_reg = try cg.copyToTmpRegister(Type.u64, dst_ptr_mcv);

                const num_elem = ty.vectorLen(zcu);
                const elem_size = ty.childType(zcu).bitSize(zcu);
                const vsew = bits.VSew.fromBits(elem_size) orelse unreachable;

                try cg.setVl(.zero, num_elem, .{
                    .vsew = vsew,
                    .vlmul = .m1,
                    .vma = true,
                    .vta = true,
                });

                _ = try cg.addInst(.{
                    .tag = .pseudo_store_rm,
                    .data = .{ .rm = .{
                        .r = reg,
                        .m = .{
                            .base = .{ .reg = addr_reg },
                            .mod = .{
                                .disp = 0,
                                .size = cg.memSize(ty.childType(zcu)),
                                .unsigned = false,
                            },
                        },
                    } },
                });

                return;
            }

            const mem_size = switch (base) {
                .frame => |base_fi| mem_size: {
                    assert(disp >= 0);
                    const frame_abi_size = cg.frame_allocs.items(.abi_size)[@intFromEnum(base_fi)];
                    const frame_spill_pad = cg.frame_allocs.items(.spill_pad)[@intFromEnum(base_fi)];
                    assert(frame_abi_size - frame_spill_pad - disp >= abi_size);
                    break :mem_size if (frame_abi_size - frame_spill_pad - disp == abi_size)
                        frame_abi_size
                    else
                        abi_size;
                },
                else => abi_size,
            };
            const src_size = math.ceilPowerOfTwoAssert(u32, abi_size);
            const src_align = Alignment.fromNonzeroByteUnits(math.ceilPowerOfTwoAssert(u32, src_size));
            if (src_size > mem_size) {
                const frame_index = try cg.allocFrameIndex(FrameAlloc.init(.{
                    .size = src_size,
                    .alignment = src_align,
                }));
                const frame_mcv: MCValue = .{ .load_frame = .{ .index = frame_index } };
                _ = try cg.addInst(.{
                    .tag = .pseudo_store_rm,
                    .data = .{ .rm = .{
                        .r = reg,
                        .m = .{
                            .base = .{ .frame = frame_index },
                            .mod = .{
                                .size = Memory.Size.fromByteSize(src_size),
                                .unsigned = false,
                            },
                        },
                    } },
                });
                try cg.genSetMem(base, disp, ty, frame_mcv);
                try cg.freeValue(frame_mcv);
            } else _ = try cg.addInst(.{
                .tag = .pseudo_store_rm,
                .data = .{ .rm = .{
                    .r = reg,
                    .m = .{
                        .base = base,
                        .mod = .{
                            .size = cg.memSize(ty),
                            .disp = disp,
                            .unsigned = false,
                        },
                    },
                } },
            });
        },
        .register_pair => |src_regs| {
            var part_disp: i32 = disp;
            for (try cg.splitType(ty), src_regs) |src_ty, src_reg| {
                try cg.genSetMem(base, part_disp, src_ty, .{ .register = src_reg });
                part_disp += @intCast(src_ty.abiSize(zcu));
            }
        },
        .immediate => {
            // TODO: remove this lock in favor of a copyToTmpRegister when we load 64 bit immediates with
            // a register allocation.
            const reg, const reg_lock = try cg.promoteReg(ty, src_mcv);
            defer if (reg_lock) |lock| cg.register_manager.unlockReg(lock);

            return cg.genSetMem(base, disp, ty, .{ .register = reg });
        },
        .air_ref => |src_ref| try cg.genSetMem(base, disp, ty, try cg.resolveInst(src_ref)),
        else => return cg.fail("TODO: genSetMem {s}", .{@tagName(src_mcv)}),
    }
}

fn airBitCast(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;

    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const result = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const src_mcv = try cg.resolveInst(ty_op.operand);

        const src_ty = cg.typeOf(ty_op.operand);
        if (src_ty.toIntern() == .bool_type) break :result src_mcv;
        const dst_ty = cg.typeOfIndex(inst);

        const src_lock = if (src_mcv.getReg()) |reg| cg.register_manager.lockReg(reg) else null;
        defer if (src_lock) |lock| cg.register_manager.unlockReg(lock);

        const dst_mcv = if (dst_ty.abiSize(zcu) <= src_ty.abiSize(zcu) and src_mcv != .register_pair and
            cg.reuseOperand(inst, ty_op.operand, 0, src_mcv)) src_mcv else dst: {
            const dst_mcv = try cg.allocRegOrMem(dst_ty, inst, true);
            try cg.genCopy(switch (math.order(dst_ty.abiSize(zcu), src_ty.abiSize(zcu))) {
                .lt => dst_ty,
                .eq => if (!dst_mcv.isMemory() or src_mcv.isMemory()) dst_ty else src_ty,
                .gt => src_ty,
            }, dst_mcv, src_mcv);
            break :dst dst_mcv;
        };

        if (dst_ty.isAbiInt(zcu) and src_ty.isAbiInt(zcu) and
            dst_ty.intInfo(zcu).signedness == src_ty.intInfo(zcu).signedness) break :result dst_mcv;

        const abi_size = dst_ty.abiSize(zcu);
        const bit_size = dst_ty.bitSize(zcu);
        if (abi_size * 8 <= bit_size) break :result dst_mcv;

        return cg.fail("TODO: airBitCast {} to {}", .{ src_ty.fmt(pt), dst_ty.fmt(pt) });
    };
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn airArrayToSlice(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;

    const slice_ty = cg.typeOfIndex(inst);
    const ptr_ty = cg.typeOf(ty_op.operand);
    const ptr = try cg.resolveInst(ty_op.operand);
    const array_ty = ptr_ty.childType(zcu);
    const array_len = array_ty.arrayLen(zcu);

    const frame_index = try cg.allocFrameIndex(FrameAlloc.initSpill(slice_ty, zcu));
    try cg.genSetMem(.{ .frame = frame_index }, 0, ptr_ty, ptr);
    try cg.genSetMem(
        .{ .frame = frame_index },
        @intCast(ptr_ty.abiSize(zcu)),
        Type.u64,
        .{ .immediate = array_len },
    );

    const result = MCValue{ .load_frame = .{ .index = frame_index } };
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn airFloatFromInt(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const pt = cg.pt;
        const zcu = pt.zcu;

        const operand = try cg.resolveInst(ty_op.operand);

        const src_ty = cg.typeOf(ty_op.operand);
        const dst_ty = ty_op.ty.toType();

        const src_reg, const src_lock = try cg.promoteReg(src_ty, operand);
        defer if (src_lock) |lock| cg.register_manager.unlockReg(lock);

        const is_unsigned = dst_ty.isUnsignedInt(zcu);
        const src_bits = src_ty.bitSize(zcu);
        const dst_bits = dst_ty.bitSize(zcu);

        switch (src_bits) {
            32, 64 => {},
            else => try cg.truncateRegister(src_ty, src_reg),
        }

        const int_zcu: Mir.FcvtOp = switch (src_bits) {
            8, 16, 32 => if (is_unsigned) .wu else .w,
            64 => if (is_unsigned) .lu else .l,
            else => return cg.fail("TODO: airFloatFromInt src size: {d}", .{src_bits}),
        };

        const float_zcu: enum { s, d } = switch (dst_bits) {
            32 => .s,
            64 => .d,
            else => return cg.fail("TODO: airFloatFromInt dst size {d}", .{dst_bits}),
        };

        const dst_reg, const dst_lock = try cg.allocReg(.int);
        defer cg.register_manager.unlockReg(dst_lock);

        _ = try cg.addInst(.{
            .tag = switch (float_zcu) {
                .s => switch (int_zcu) {
                    .l => .fcvtsl,
                    .lu => .fcvtslu,
                    .w => .fcvtsw,
                    .wu => .fcvtswu,
                },
                .d => switch (int_zcu) {
                    .l => .fcvtdl,
                    .lu => .fcvtdlu,
                    .w => .fcvtdw,
                    .wu => .fcvtdwu,
                },
            },
            .data = .{ .rr = .{
                .rd = dst_reg,
                .rs = src_reg,
            } },
        });

        break :result .{ .register = dst_reg };
    };
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn airIntFromFloat(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const pt = cg.pt;
        const zcu = pt.zcu;

        const operand = try cg.resolveInst(ty_op.operand);
        const src_ty = cg.typeOf(ty_op.operand);
        const dst_ty = ty_op.ty.toType();

        const is_unsigned = dst_ty.isUnsignedInt(zcu);
        const src_bits = src_ty.bitSize(zcu);
        const dst_bits = dst_ty.bitSize(zcu);

        const float_zcu: enum { s, d } = switch (src_bits) {
            32 => .s,
            64 => .d,
            else => return cg.fail("TODO: airIntFromFloat src size {d}", .{src_bits}),
        };

        const int_zcu: Mir.FcvtOp = switch (dst_bits) {
            32 => if (is_unsigned) .wu else .w,
            8, 16, 64 => if (is_unsigned) .lu else .l,
            else => return cg.fail("TODO: airIntFromFloat dst size: {d}", .{dst_bits}),
        };

        const src_reg, const src_lock = try cg.promoteReg(src_ty, operand);
        defer if (src_lock) |lock| cg.register_manager.unlockReg(lock);

        const dst_reg, const dst_lock = try cg.allocReg(.int);
        defer cg.register_manager.unlockReg(dst_lock);

        _ = try cg.addInst(.{
            .tag = switch (float_zcu) {
                .s => switch (int_zcu) {
                    .l => .fcvtls,
                    .lu => .fcvtlus,
                    .w => .fcvtws,
                    .wu => .fcvtwus,
                },
                .d => switch (int_zcu) {
                    .l => .fcvtld,
                    .lu => .fcvtlud,
                    .w => .fcvtwd,
                    .wu => .fcvtwud,
                },
            },
            .data = .{ .rr = .{
                .rd = dst_reg,
                .rs = src_reg,
            } },
        });

        break :result .{ .register = dst_reg };
    };
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn airCmpxchg(cg: *CodeGen, inst: Air.Inst.Index, strength: enum { weak, strong }) !void {
    _ = strength; // TODO: do something with this

    const pt = cg.pt;
    const zcu = pt.zcu;
    const ty_pl = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_pl;
    const extra = cg.air.extraData(Air.Cmpxchg, ty_pl.payload).data;

    const ptr_ty = cg.typeOf(extra.ptr);
    const val_ty = cg.typeOf(extra.expected_value);
    const val_abi_size: u32 = @intCast(val_ty.abiSize(pt.zcu));

    switch (val_abi_size) {
        1, 2, 4, 8 => {},
        else => return cg.fail("TODO: airCmpxchg Int size {}", .{val_abi_size}),
    }

    const lr_order: struct { aq: Mir.Barrier, rl: Mir.Barrier } = switch (extra.successOrder()) {
        .unordered,
        => unreachable,

        .monotonic,
        .release,
        => .{ .aq = .none, .rl = .none },
        .acquire,
        .acq_rel,
        => .{ .aq = .aq, .rl = .none },
        .seq_cst => .{ .aq = .aq, .rl = .rl },
    };

    const sc_order: struct { aq: Mir.Barrier, rl: Mir.Barrier } = switch (extra.failureOrder()) {
        .unordered,
        .release,
        .acq_rel,
        => unreachable,

        .monotonic,
        .acquire,
        .seq_cst,
        => switch (extra.successOrder()) {
            .release,
            .seq_cst,
            => .{ .aq = .none, .rl = .rl },
            else => .{ .aq = .none, .rl = .none },
        },
    };

    const ptr_mcv = try cg.resolveInst(extra.ptr);
    const ptr_reg, const ptr_lock = try cg.promoteReg(ptr_ty, ptr_mcv);
    defer if (ptr_lock) |lock| cg.register_manager.unlockReg(lock);

    const exp_mcv = try cg.resolveInst(extra.expected_value);
    const exp_reg, const exp_lock = try cg.promoteReg(val_ty, exp_mcv);
    defer if (exp_lock) |lock| cg.register_manager.unlockReg(lock);
    try cg.truncateRegister(val_ty, exp_reg);

    const new_mcv = try cg.resolveInst(extra.new_value);
    const new_reg, const new_lock = try cg.promoteReg(val_ty, new_mcv);
    defer if (new_lock) |lock| cg.register_manager.unlockReg(lock);
    try cg.truncateRegister(val_ty, new_reg);

    const branch_reg, const branch_lock = try cg.allocReg(.int);
    defer cg.register_manager.unlockReg(branch_lock);

    const fallthrough_reg, const fallthrough_lock = try cg.allocReg(.int);
    defer cg.register_manager.unlockReg(fallthrough_lock);

    const jump_back = try cg.addInst(.{
        .tag = if (val_ty.bitSize(zcu) <= 32) .lrw else .lrd,
        .data = .{ .amo = .{
            .aq = lr_order.aq,
            .rl = lr_order.rl,
            .rd = branch_reg,
            .rs1 = ptr_reg,
            .rs2 = .zero,
        } },
    });
    try cg.truncateRegister(val_ty, branch_reg);

    const jump_forward = try cg.addInst(.{
        .tag = .bne,
        .data = .{ .b_type = .{
            .rs1 = branch_reg,
            .rs2 = exp_reg,
            .inst = undefined,
        } },
    });

    _ = try cg.addInst(.{
        .tag = if (val_ty.bitSize(zcu) <= 32) .scw else .scd,
        .data = .{ .amo = .{
            .aq = sc_order.aq,
            .rl = sc_order.rl,
            .rd = fallthrough_reg,
            .rs1 = ptr_reg,
            .rs2 = new_reg,
        } },
    });
    try cg.truncateRegister(Type.bool, fallthrough_reg);

    _ = try cg.addInst(.{
        .tag = .bne,
        .data = .{ .b_type = .{
            .rs1 = fallthrough_reg,
            .rs2 = .zero,
            .inst = jump_back,
        } },
    });

    cg.performReloc(jump_forward);

    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const dst_mcv = try cg.allocRegOrMem(cg.typeOfIndex(inst), inst, false);

        const tmp_reg, const tmp_lock = try cg.allocReg(.int);
        defer cg.register_manager.unlockReg(tmp_lock);

        try cg.genBinOp(
            .cmp_neq,
            .{ .register = branch_reg },
            val_ty,
            .{ .register = exp_reg },
            val_ty,
            tmp_reg,
        );

        try cg.genCopy(val_ty, dst_mcv, .{ .register = branch_reg });
        try cg.genCopy(
            Type.bool,
            dst_mcv.address().offset(@intCast(val_abi_size)).deref(),
            .{ .register = tmp_reg },
        );

        break :result dst_mcv;
    };

    return cg.finishAir(inst, result, .{ extra.ptr, extra.expected_value, extra.new_value });
}

fn airAtomicRmw(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const pl_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].pl_op;
    const extra = cg.air.extraData(Air.AtomicRmw, pl_op.payload).data;

    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const op = extra.op();
        const order = extra.ordering();

        const ptr_ty = cg.typeOf(pl_op.operand);
        const ptr_mcv = try cg.resolveInst(pl_op.operand);

        const val_ty = cg.typeOf(extra.operand);
        const val_size = val_ty.abiSize(zcu);
        const val_mcv = try cg.resolveInst(extra.operand);

        if (!math.isPowerOfTwo(val_size))
            return cg.fail("TODO: airAtomicRmw non-pow 2", .{});

        switch (val_ty.zigTypeTag(pt.zcu)) {
            .@"enum", .int => {},
            inline .bool, .float, .pointer => |ty| return cg.fail("TODO: airAtomicRmw {s}", .{@tagName(ty)}),
            else => unreachable,
        }

        const method: enum { amo, loop } = switch (val_size) {
            1, 2 => .loop,
            4, 8 => .amo,
            else => unreachable,
        };

        const ptr_register, const ptr_lock = try cg.promoteReg(ptr_ty, ptr_mcv);
        defer if (ptr_lock) |lock| cg.register_manager.unlockReg(lock);

        const val_register, const val_lock = try cg.promoteReg(val_ty, val_mcv);
        defer if (val_lock) |lock| cg.register_manager.unlockReg(lock);

        const result_mcv = try cg.allocRegOrMem(val_ty, inst, true);
        assert(result_mcv == .register); // should fit into 8 bytes
        const result_reg = result_mcv.register;

        const aq, const rl = switch (order) {
            .unordered => unreachable,
            .monotonic => .{ false, false },
            .acquire => .{ true, false },
            .release => .{ false, true },
            .acq_rel => .{ true, true },
            .seq_cst => .{ true, true },
        };

        switch (method) {
            .amo => {
                const is_d = val_ty.abiSize(zcu) == 8;
                const is_un = val_ty.isUnsignedInt(zcu);

                const mnem: Mnemonic = switch (op) {
                    // zig fmt: off
                .Xchg => if (is_d) .amoswapd  else .amoswapw,
                .Add  => if (is_d) .amoaddd   else .amoaddw,
                .And  => if (is_d) .amoandd   else .amoandw,
                .Or   => if (is_d) .amoord    else .amoorw,
                .Xor  => if (is_d) .amoxord   else .amoxorw,
                .Max  => if (is_d) if (is_un) .amomaxud else .amomaxd else if (is_un) .amomaxuw else .amomaxw,
                .Min  => if (is_d) if (is_un) .amominud else .amomind else if (is_un) .amominuw else .amominw,
                else => return cg.fail("TODO: airAtomicRmw amo {s}", .{@tagName(op)}),
                // zig fmt: on
                };

                _ = try cg.addInst(.{
                    .tag = mnem,
                    .data = .{ .amo = .{
                        .rd = result_reg,
                        .rs1 = ptr_register,
                        .rs2 = val_register,
                        .aq = if (aq) .aq else .none,
                        .rl = if (rl) .rl else .none,
                    } },
                });
            },
            .loop => {
                // where we'll jump back when the sc fails
                const jump_back = try cg.addInst(.{
                    .tag = .lrw,
                    .data = .{ .amo = .{
                        .rd = result_reg,
                        .rs1 = ptr_register,
                        .rs2 = .zero,
                        .aq = if (aq) .aq else .none,
                        .rl = if (rl) .rl else .none,
                    } },
                });

                const after_reg, const after_lock = try cg.allocReg(.int);
                defer cg.register_manager.unlockReg(after_lock);

                switch (op) {
                    .Add, .Sub => |tag| {
                        _ = try cg.genBinOp(
                            switch (tag) {
                                .Add => .add,
                                .Sub => .sub,
                                else => unreachable,
                            },
                            .{ .register = result_reg },
                            val_ty,
                            .{ .register = val_register },
                            val_ty,
                            after_reg,
                        );
                    },

                    else => return cg.fail("TODO: airAtomicRmw loop {s}", .{@tagName(op)}),
                }

                _ = try cg.addInst(.{
                    .tag = .scw,
                    .data = .{ .amo = .{
                        .rd = after_reg,
                        .rs1 = ptr_register,
                        .rs2 = after_reg,
                        .aq = if (aq) .aq else .none,
                        .rl = if (rl) .rl else .none,
                    } },
                });

                _ = try cg.addInst(.{
                    .tag = .bne,
                    .data = .{ .b_type = .{
                        .inst = jump_back,
                        .rs1 = after_reg,
                        .rs2 = .zero,
                    } },
                });
            },
        }
        break :result result_mcv;
    };

    return cg.finishAir(inst, result, .{ pl_op.operand, extra.operand, .none });
}

fn airAtomicLoad(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const atomic_load = cg.air.instructions.items(.data)[@intFromEnum(inst)].atomic_load;
    const order: std.builtin.AtomicOrder = atomic_load.order;

    const ptr_ty = cg.typeOf(atomic_load.ptr);
    const elem_ty = ptr_ty.childType(zcu);
    const ptr_mcv = try cg.resolveInst(atomic_load.ptr);

    const bit_size = elem_ty.bitSize(zcu);
    if (bit_size > 64) return cg.fail("TODO: airAtomicStore > 64 bits", .{});

    const result_mcv = try cg.allocRegOrMem(elem_ty, inst, true);
    assert(result_mcv == .register); // should be less than 8 bytes

    if (order == .seq_cst) {
        _ = try cg.addInst(.{
            .tag = .fence,
            .data = .{ .fence = .{
                .pred = .rw,
                .succ = .rw,
            } },
        });
    }

    try cg.load(result_mcv, ptr_mcv, ptr_ty);

    switch (order) {
        // Don't guarnetee other memory operations to be ordered after the load.
        .unordered => {},
        .monotonic => {},
        // Make sure all previous reads happen before any reading or writing accurs.
        .seq_cst, .acquire => {
            _ = try cg.addInst(.{
                .tag = .fence,
                .data = .{ .fence = .{
                    .pred = .r,
                    .succ = .rw,
                } },
            });
        },
        else => unreachable,
    }

    return cg.finishAir(inst, result_mcv, .{ atomic_load.ptr, .none, .none });
}

fn airAtomicStore(cg: *CodeGen, inst: Air.Inst.Index, order: std.builtin.AtomicOrder) !void {
    const bin_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].bin_op;

    const ptr_ty = cg.typeOf(bin_op.lhs);
    const ptr_mcv = try cg.resolveInst(bin_op.lhs);

    const val_ty = cg.typeOf(bin_op.rhs);
    const val_mcv = try cg.resolveInst(bin_op.rhs);

    const bit_size = val_ty.bitSize(cg.pt.zcu);
    if (bit_size > 64) return cg.fail("TODO: airAtomicStore > 64 bits", .{});

    switch (order) {
        .unordered, .monotonic => {},
        .release, .seq_cst => {
            _ = try cg.addInst(.{
                .tag = .fence,
                .data = .{ .fence = .{
                    .pred = .rw,
                    .succ = .w,
                } },
            });
        },
        else => unreachable,
    }

    try cg.store(ptr_mcv, val_mcv, ptr_ty);
    return cg.finishAir(inst, .unreach, .{ bin_op.lhs, bin_op.rhs, .none });
}

fn airMemset(cg: *CodeGen, inst: Air.Inst.Index, safety: bool) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const bin_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].bin_op;

    result: {
        if (!safety and (try cg.resolveInst(bin_op.rhs)) == .undef) break :result;

        const dst_ptr = try cg.resolveInst(bin_op.lhs);
        const dst_ptr_ty = cg.typeOf(bin_op.lhs);
        const dst_ptr_lock: ?RegisterLock = switch (dst_ptr) {
            .register => |reg| cg.register_manager.lockRegAssumeUnused(reg),
            else => null,
        };
        defer if (dst_ptr_lock) |lock| cg.register_manager.unlockReg(lock);

        const src_val = try cg.resolveInst(bin_op.rhs);
        const elem_ty = cg.typeOf(bin_op.rhs);
        const src_val_lock: ?RegisterLock = switch (src_val) {
            .register => |reg| cg.register_manager.lockRegAssumeUnused(reg),
            else => null,
        };
        defer if (src_val_lock) |lock| cg.register_manager.unlockReg(lock);

        const elem_abi_size: u31 = @intCast(elem_ty.abiSize(zcu));

        if (elem_abi_size == 1) {
            const ptr: MCValue = switch (dst_ptr_ty.ptrSize(zcu)) {
                // TODO: this only handles slices stored in the stack
                .slice => if (dst_ptr == .register_pair)
                    .{ .register = dst_ptr.register_pair[0] }
                else
                    dst_ptr,
                .one => dst_ptr,
                .c, .many => unreachable,
            };
            const len: MCValue = switch (dst_ptr_ty.ptrSize(zcu)) {
                // TODO: this only handles slices stored in the stack
                .slice => if (dst_ptr == .register_pair)
                    .{ .register = dst_ptr.register_pair[1] }
                else
                    dst_ptr.address().offset(8).deref(),
                .one => .{ .immediate = dst_ptr_ty.childType(zcu).arrayLen(zcu) },
                .c, .many => unreachable,
            };
            const len_lock: ?RegisterLock = switch (len) {
                .register => |reg| cg.register_manager.lockRegAssumeUnused(reg),
                else => null,
            };
            defer if (len_lock) |lock| cg.register_manager.unlockReg(lock);

            try cg.genInlineMemset(ptr, src_val, len);
            break :result;
        }

        // Store the first element, and then rely on memcpy copying forwards.
        // Length zero requires a runtime check - so we handle arrays specially
        // here to elide it.
        switch (dst_ptr_ty.ptrSize(zcu)) {
            .slice => return cg.fail("TODO: airMemset Slices", .{}),
            .one => {
                const elem_ptr_ty = try pt.singleMutPtrType(elem_ty);

                const len = dst_ptr_ty.childType(zcu).arrayLen(zcu);

                assert(len != 0); // prevented by Sema
                try cg.store(dst_ptr, src_val, elem_ptr_ty);

                const second_elem_ptr_reg, const second_elem_ptr_lock = try cg.allocReg(.int);
                defer cg.register_manager.unlockReg(second_elem_ptr_lock);

                const second_elem_ptr_mcv: MCValue = .{ .register = second_elem_ptr_reg };

                try cg.genSetReg(Type.u64, second_elem_ptr_reg, .{ .register_offset = .{
                    .reg = try cg.copyToTmpRegister(Type.u64, dst_ptr),
                    .off = elem_abi_size,
                } });

                const bytes_to_copy: MCValue = .{ .immediate = elem_abi_size * (len - 1) };
                try cg.genInlineMemcpy(second_elem_ptr_mcv, dst_ptr, bytes_to_copy);
            },
            .c, .many => unreachable,
        }
    }
    return cg.finishAir(inst, .unreach, .{ bin_op.lhs, bin_op.rhs, .none });
}

fn airMemcpy(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const bin_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].bin_op;

    const dst_ptr = try cg.resolveInst(bin_op.lhs);
    const src_ptr = try cg.resolveInst(bin_op.rhs);

    const dst_ty = cg.typeOf(bin_op.lhs);

    const len_mcv: MCValue = switch (dst_ty.ptrSize(zcu)) {
        .slice => len: {
            const len_reg, const len_lock = try cg.allocReg(.int);
            defer cg.register_manager.unlockReg(len_lock);

            const elem_size = dst_ty.childType(zcu).abiSize(zcu);
            try cg.genBinOp(
                .mul,
                .{ .immediate = elem_size },
                Type.u64,
                dst_ptr.address().offset(8).deref(),
                Type.u64,
                len_reg,
            );
            break :len .{ .register = len_reg };
        },
        .one => len: {
            const array_ty = dst_ty.childType(zcu);
            break :len .{ .immediate = array_ty.arrayLen(zcu) * array_ty.childType(zcu).abiSize(zcu) };
        },
        else => |size| return cg.fail("TODO: airMemcpy size {s}", .{@tagName(size)}),
    };
    const len_lock: ?RegisterLock = switch (len_mcv) {
        .register => |reg| cg.register_manager.lockRegAssumeUnused(reg),
        else => null,
    };
    defer if (len_lock) |lock| cg.register_manager.unlockReg(lock);

    try cg.genInlineMemcpy(dst_ptr, src_ptr, len_mcv);

    return cg.finishAir(inst, .unreach, .{ bin_op.lhs, bin_op.rhs, .none });
}

fn airTagName(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;

    const un_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].un_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const enum_ty = cg.typeOf(un_op);

        const param_regs = abi.Registers.Integer.function_arg_regs;
        const dst_mcv = try cg.allocRegOrMem(Type.u64, inst, false);
        try cg.genSetReg(Type.u64, param_regs[0], dst_mcv.address());

        const operand = try cg.resolveInst(un_op);
        try cg.genSetReg(enum_ty, param_regs[1], operand);

        const lazy_sym: link.File.LazySymbol = .{ .kind = .code, .ty = enum_ty.toIntern() };
        const elf_file = cg.bin_file.cast(.elf).?;
        const zo = elf_file.zigObjectPtr().?;
        const sym_index = zo.getOrCreateMetadataForLazySymbol(elf_file, pt, lazy_sym) catch |err|
            return cg.fail("{s} creating lazy symbol", .{@errorName(err)});

        if (cg.mod.pic) {
            return cg.fail("TODO: airTagName pic", .{});
        } else {
            try cg.genSetReg(Type.u64, .ra, .{ .lea_symbol = .{ .sym_index = sym_index } });
            try cg.asmIType(.jalr, .ra, .ra, .s(0));
        }

        break :result dst_mcv;
    };
    return cg.finishAir(inst, result, .{ un_op, .none, .none });
}

fn airSplat(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const vector_ty = cg.typeOfIndex(inst);
        const vector_len = vector_ty.vectorLen(zcu);
        const scalar_ty = cg.typeOf(ty_op.operand);
        const src_mcv = try cg.resolveInst(ty_op.operand);

        const dst_reg, const dst_lock = try cg.allocReg(.vector);
        const src_reg, const src_lock = try cg.promoteReg(scalar_ty, src_mcv);
        defer if (src_lock) |lock| cg.register_manager.unlockReg(lock);
        defer cg.register_manager.unlockReg(dst_lock);

        switch (scalar_ty.zigTypeTag(zcu)) {
            .int => {
                switch (scalar_ty.intInfo(zcu).bits) {
                    8, 16, 32, 64 => |b| {
                        const vsew = bits.VSew.fromBits(b) orelse unreachable;

                        try cg.setVl(.zero, vector_len, .{
                            .vlmul = .m1,
                            .vsew = vsew,
                            .vma = true,
                            .vta = true,
                        });

                        try cg.asmRType(.vmvvx, dst_reg, src_reg, .zero);
                    },
                    else => |b| return cg.fail("TODO: implement airSplat for Int {d}", .{b}),
                }
            },
            else => return cg.fail("TODO implement airSplat for {}", .{vector_ty.fmt(pt)}),
        }

        break :result .{ .register = dst_reg };
    };
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn airSelect(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pl_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].pl_op;
    const extra = cg.air.extraData(Air.Bin, pl_op.payload).data;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else return cg.fail("TODO implement airSelect for riscv64", .{});
    return cg.finishAir(inst, result, .{ pl_op.operand, extra.lhs, extra.rhs });
}

fn airShuffle(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_op;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else return cg.fail("TODO implement airShuffle for riscv64", .{});
    return cg.finishAir(inst, result, .{ ty_op.operand, .none, .none });
}

fn airReduce(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;

    const reduce = cg.air.instructions.items(.data)[@intFromEnum(inst)].reduce;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else result: {
        const operand_ty = cg.typeOf(reduce.operand);
        const operand = try cg.resolveInst(reduce.operand);
        const elem_ty = operand_ty.childType(zcu);

        if (!elem_ty.isInt(zcu)) {
            return cg.fail("TODO: airReduce {}", .{operand_ty.fmt(pt)});
        }

        const dst_mcv = try cg.allocRegOrMem(elem_ty, inst, true);

        const src_reg, const src_locks = if (operand == .register and operand.register.class() == .vector)
            .{ operand.register, null }
        else blk: {
            const src_reg, const src_locks = try cg.allocVecReg(operand_ty);
            try cg.genCopy(operand_ty, .{ .register = src_reg }, operand);
            break :blk .{ src_reg, src_locks };
        };
        defer {
            if (src_locks) |locks| {
                for (locks) |lock| cg.register_manager.unlockReg(lock);
                cg.gpa.free(locks);
            }
        }

        const mask_reg, const mask_lock = try cg.allocReg(.vector);
        defer cg.register_manager.unlockReg(mask_lock);

        try cg.setVl(.zero, operand_ty.vectorLen(zcu), .{
            .vlmul = try cg.suggestedVlMul(operand_ty),
            .vsew = bits.VSew.fromBits(elem_ty.bitSize(zcu)) orelse
                return cg.fail("TODO: airReduce elem_ty size {}", .{elem_ty.bitSize(zcu)}),
            .vma = true,
            .vta = true,
        });

        // TODO: optimize this to use vmv.s.x instead of a full-lane move
        try cg.genSetReg(operand_ty, mask_reg, .{ .register = .zero });

        const operation: std.builtin.ReduceOp = reduce.operation;
        try cg.asmRType(
            switch (operation) {
                // zig fmt: off
                .Add => .vredsumvs,
                .And => .vredandvs,
                .Max => if (elem_ty.isUnsignedInt(zcu)) .vredmaxuvs else .vredmaxvs,
                .Min => if (elem_ty.isUnsignedInt(zcu)) .vredminuvs else .vredminvs,
                .Or  => .vredorvs,
                .Xor => .vredxorvs,
                .Mul => return cg.fail("TODO: airReduce Mul", .{}),
                // zig fmt: on
            },
            mask_reg,
            switch (operation) {
                .Add => mask_reg,
                else => src_reg,
            },
            src_reg,
        );

        try cg.genCopy(operand_ty, dst_mcv, .{ .register = mask_reg });

        break :result dst_mcv;
    };
    return cg.finishAir(inst, result, .{ reduce.operand, .none, .none });
}

fn airAggregateInit(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const result_ty = cg.typeOfIndex(inst);
    const len: usize = @intCast(result_ty.arrayLen(zcu));
    const ty_pl = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_pl;
    const elements: []const Air.Inst.Ref = @ptrCast(cg.air.extra[ty_pl.payload..][0..len]);

    const result: MCValue = result: {
        switch (result_ty.zigTypeTag(zcu)) {
            .@"struct" => {
                const frame_index = try cg.allocFrameIndex(FrameAlloc.initSpill(result_ty, zcu));
                if (result_ty.containerLayout(zcu) == .@"packed") {
                    const struct_obj = zcu.typeToStruct(result_ty).?;
                    try cg.genInlineMemset(
                        .{ .lea_frame = .{ .index = frame_index } },
                        .{ .immediate = 0 },
                        .{ .immediate = result_ty.abiSize(zcu) },
                    );

                    for (elements, 0..) |elem, elem_i_usize| {
                        const elem_i: u32 = @intCast(elem_i_usize);
                        if ((try result_ty.structFieldValueComptime(pt, elem_i)) != null) continue;

                        const elem_ty = result_ty.fieldType(elem_i, zcu);
                        const elem_bit_size: u32 = @intCast(elem_ty.bitSize(zcu));
                        if (elem_bit_size > 64) {
                            return cg.fail(
                                "TODO airAggregateInit implement packed structs with large fields",
                                .{},
                            );
                        }

                        const elem_abi_size: u32 = @intCast(elem_ty.abiSize(zcu));
                        const elem_abi_bits = elem_abi_size * 8;
                        const elem_off = pt.structPackedFieldBitOffset(struct_obj, elem_i);
                        const elem_byte_off: i32 = @intCast(elem_off / elem_abi_bits * elem_abi_size);
                        const elem_bit_off = elem_off % elem_abi_bits;
                        const elem_mcv = try cg.resolveInst(elem);

                        _ = elem_byte_off;
                        _ = elem_bit_off;

                        const elem_lock = switch (elem_mcv) {
                            .register => |reg| cg.register_manager.lockReg(reg),
                            .immediate => |imm| lock: {
                                if (imm == 0) continue;
                                break :lock null;
                            },
                            else => null,
                        };
                        defer if (elem_lock) |lock| cg.register_manager.unlockReg(lock);

                        return cg.fail("TODO: airAggregateInit packed structs", .{});
                    }
                } else for (elements, 0..) |elem, elem_i| {
                    if ((try result_ty.structFieldValueComptime(pt, elem_i)) != null) continue;

                    const elem_ty = result_ty.fieldType(elem_i, zcu);
                    const elem_off: i32 = @intCast(result_ty.structFieldOffset(elem_i, zcu));
                    const elem_mcv = try cg.resolveInst(elem);
                    try cg.genSetMem(.{ .frame = frame_index }, elem_off, elem_ty, elem_mcv);
                }
                break :result .{ .load_frame = .{ .index = frame_index } };
            },
            .array => {
                const elem_ty = result_ty.childType(zcu);
                const frame_index = try cg.allocFrameIndex(FrameAlloc.initSpill(result_ty, zcu));
                const elem_size: u32 = @intCast(elem_ty.abiSize(zcu));

                for (elements, 0..) |elem, elem_i| {
                    const elem_mcv = try cg.resolveInst(elem);
                    const elem_off: i32 = @intCast(elem_size * elem_i);
                    try cg.genSetMem(
                        .{ .frame = frame_index },
                        elem_off,
                        elem_ty,
                        elem_mcv,
                    );
                }
                if (result_ty.sentinel(zcu)) |sentinel| try cg.genSetMem(
                    .{ .frame = frame_index },
                    @intCast(elem_size * elements.len),
                    elem_ty,
                    try cg.genTypedValue(sentinel),
                );
                break :result .{ .load_frame = .{ .index = frame_index } };
            },
            else => return cg.fail("TODO: airAggregate {}", .{result_ty.fmt(pt)}),
        }
    };

    if (elements.len <= Liveness.bpi - 1) {
        var buf = [1]Air.Inst.Ref{.none} ** (Liveness.bpi - 1);
        @memcpy(buf[0..elements.len], elements);
        return cg.finishAir(inst, result, buf);
    }
    var bt = cg.liveness.iterateBigTomb(inst);
    for (elements) |elem| try cg.feed(&bt, elem);
    return cg.finishAirResult(inst, result);
}

fn airUnionInit(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const ty_pl = cg.air.instructions.items(.data)[@intFromEnum(inst)].ty_pl;
    const extra = cg.air.extraData(Air.UnionInit, ty_pl.payload).data;
    _ = extra;
    return cg.fail("TODO implement airUnionInit for riscv64", .{});
    // return cg.finishAir(inst, result, .{ extra.ptr, extra.expected_value, extra.new_value });
}

fn airPrefetch(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const prefetch = cg.air.instructions.items(.data)[@intFromEnum(inst)].prefetch;
    // TODO: RISC-V does have prefetch instruction variants.
    // see here: https://raw.githubusercontent.com/riscv/riscv-CMOs/master/specifications/cmobase-v1.0.1.pdf
    return cg.finishAir(inst, .unreach, .{ prefetch.ptr, .none, .none });
}

fn airMulAdd(cg: *CodeGen, inst: Air.Inst.Index) !void {
    const pl_op = cg.air.instructions.items(.data)[@intFromEnum(inst)].pl_op;
    const extra = cg.air.extraData(Air.Bin, pl_op.payload).data;
    const result: MCValue = if (cg.liveness.isUnused(inst)) .unreach else {
        return cg.fail("TODO implement airMulAdd for riscv64", .{});
    };
    return cg.finishAir(inst, result, .{ extra.lhs, extra.rhs, pl_op.operand });
}

fn resolveInst(cg: *CodeGen, ref: Air.Inst.Ref) InnerError!MCValue {
    const pt = cg.pt;
    const zcu = pt.zcu;

    // If the type has no codegen bits, no need to store it.
    const inst_ty = cg.typeOf(ref);
    if (!inst_ty.hasRuntimeBits(zcu))
        return .none;

    const mcv = if (ref.toIndex()) |inst| mcv: {
        break :mcv cg.inst_tracking.getPtr(inst).?.short;
    } else mcv: {
        const ip_index = ref.toInterned().?;
        const gop = try cg.const_tracking.getOrPut(cg.gpa, ip_index);
        if (!gop.found_existing) gop.value_ptr.* = InstTracking.init(
            try cg.genTypedValue(Value.fromInterned(ip_index)),
        );
        break :mcv gop.value_ptr.short;
    };

    return mcv;
}

fn getResolvedInstValue(cg: *CodeGen, inst: Air.Inst.Index) *InstTracking {
    const tracking = cg.inst_tracking.getPtr(inst).?;
    return switch (tracking.short) {
        .none, .unreach, .dead => unreachable,
        else => tracking,
    };
}

fn genResult(cg: *CodeGen, res: codegen.GenResult) InnerError!MCValue {
    return switch (res) {
        .mcv => |mcv| switch (mcv) {
            .none => .none,
            .undef => .{ .undef = null },
            .immediate => |imm| .{ .immediate = imm },
            .memory => |addr| .{ .memory = addr },
            .load_symbol => |sym_index| .{ .load_symbol = .{ .sym_index = sym_index } },
            .lea_symbol => |sym_index| .{ .lea_symbol = .{ .sym_index = sym_index } },
            .load_tlv => |sym_index| .{ .lea_tlv = sym_index },
            .load_got,
            .load_direct,
            .lea_direct,
            => {
                return cg.fail("TODO: genTypedValue {s}", .{@tagName(mcv)});
            },
        },
        .fail => |msg| return cg.failMsg(msg),
    };
}

fn genTypedValue(self: *CodeGen, val: Value) InnerError!MCValue {
    return self.genResult(try codegen.genTypedValue(self.bin_file, self.pt, self.src_loc, val, self.target.*));
}

fn lowerUav(self: *CodeGen, val: Value) InnerError!MCValue {
    return self.genResult(try self.bin_file.lowerUav(self.pt, val.toIntern(), .none, self.src_loc));
}

const CallMCValues = struct {
    args: []MCValue,
    return_value: InstTracking,
    stack_byte_count: u31,
    stack_align: Alignment,

    fn deinit(call: *CallMCValues, cg: *CodeGen) void {
        cg.gpa.free(call.args);
        call.* = undefined;
    }
};

/// Caller must call `CallMCValues.deinit`.
fn resolveCallingConventionValues(
    cg: *CodeGen,
    fn_info: InternPool.Key.FuncType,
    var_args: []const Type,
    stack_frame_base: FrameIndex,
) !CallMCValues {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const ip = &zcu.intern_pool;

    const param_types = try cg.gpa.alloc(Type, fn_info.param_types.len + var_args.len);
    defer cg.gpa.free(param_types);

    for (param_types[0..fn_info.param_types.len], fn_info.param_types.get(ip)) |*dest, src| {
        dest.* = Type.fromInterned(src);
    }
    for (param_types[fn_info.param_types.len..], var_args) |*param_ty, arg_ty|
        param_ty.* = cg.promoteVarArg(arg_ty);

    const cc = fn_info.cc;
    var result: CallMCValues = .{
        .args = try cg.gpa.alloc(MCValue, param_types.len),
        // These undefined values must be populated before returning from this function.
        .return_value = undefined,
        .stack_byte_count = 0,
        .stack_align = undefined,
    };
    errdefer cg.gpa.free(result.args);

    const ret_ty: Type = .fromInterned(fn_info.return_type);
    switch (cc) {
        .naked => {
            assert(result.args.len == 0);
            result.return_value = .init(.unreach);
            result.stack_align = .@"8";
        },
        .riscv64_lp64 => {
            var ret_int_reg_i: u32 = 0;
            var param_int_reg_i: u32 = 0;

            result.stack_align = .fromByteUnits(16);

            // Return values
            if (ret_ty.zigTypeTag(zcu) == .noreturn) {
                result.return_value = InstTracking.init(.unreach);
            } else if (!ret_ty.hasRuntimeBitsIgnoreComptime(zcu)) {
                result.return_value = InstTracking.init(.none);
            } else {
                var ret_tracking: [2]InstTracking = undefined;
                var ret_tracking_i: usize = 0;
                var ret_float_reg_i: usize = 0;

                const classes = mem.sliceTo(&abi.classifyCallingConvention(ret_ty, zcu), .none);

                for (classes) |class| switch (class) {
                    .integer => {
                        const ret_int_reg = abi.Registers.Integer.function_ret_regs[ret_int_reg_i];
                        ret_int_reg_i += 1;

                        ret_tracking[ret_tracking_i] = .init(.{ .register = ret_int_reg });
                        ret_tracking_i += 1;
                    },
                    .float => {
                        const ret_float_reg = abi.Registers.Float.function_ret_regs[ret_float_reg_i];
                        ret_float_reg_i += 1;

                        ret_tracking[ret_tracking_i] = .init(.{ .register = ret_float_reg });
                        ret_tracking_i += 1;
                    },
                    .memory => {
                        const ret_int_reg = abi.Registers.Integer.function_ret_regs[ret_int_reg_i];
                        ret_int_reg_i += 1;
                        const ret_indirect_reg = abi.Registers.Integer.function_arg_regs[param_int_reg_i];
                        param_int_reg_i += 1;

                        ret_tracking[ret_tracking_i] = .{
                            .short = .{ .indirect = .{ .reg = ret_int_reg } },
                            .long = .{ .indirect = .{ .reg = ret_indirect_reg } },
                        };
                        ret_tracking_i += 1;
                    },
                    else => return cg.fail("TODO: C calling convention return class {}", .{class}),
                };

                result.return_value = switch (ret_tracking_i) {
                    else => return cg.fail("ty {} took {} tracking return indices", .{ ret_ty.fmt(pt), ret_tracking_i }),
                    1 => ret_tracking[0],
                    2 => InstTracking.init(.{ .register_pair = .{
                        ret_tracking[0].short.register, ret_tracking[1].short.register,
                    } }),
                };
            }

            var param_float_reg_i: usize = 0;
            for (param_types, result.args) |ty, *arg| {
                if (!ty.hasRuntimeBitsIgnoreComptime(zcu)) {
                    assert(cc == .auto);
                    arg.* = .none;
                    continue;
                }

                var arg_mcv: [2]MCValue = undefined;
                var arg_mcv_i: usize = 0;

                const classes = mem.sliceTo(&abi.classifyCallingConvention(ty, zcu), .none);
                for (classes) |class| switch (class) {
                    .integer => {
                        const param_int_regs = abi.Registers.Integer.function_arg_regs;
                        if (param_int_reg_i >= param_int_regs.len) break;

                        const param_int_reg = param_int_regs[param_int_reg_i];
                        param_int_reg_i += 1;

                        arg_mcv[arg_mcv_i] = .{ .register = param_int_reg };
                        arg_mcv_i += 1;
                    },
                    .float => {
                        const param_float_regs = abi.Registers.Float.function_arg_regs;
                        if (param_float_reg_i >= param_float_regs.len) break;

                        const param_float_reg = param_float_regs[param_float_reg_i];
                        param_float_reg_i += 1;

                        arg_mcv[arg_mcv_i] = .{ .register = param_float_reg };
                        arg_mcv_i += 1;
                    },
                    .memory => {
                        const param_int_regs = abi.Registers.Integer.function_arg_regs;

                        const param_int_reg = param_int_regs[param_int_reg_i];
                        param_int_reg_i += 1;

                        arg_mcv[arg_mcv_i] = .{ .indirect = .{ .reg = param_int_reg } };
                        arg_mcv_i += 1;
                    },
                    else => return cg.fail("TODO: C calling convention arg class {}", .{class}),
                } else {
                    arg.* = switch (arg_mcv_i) {
                        else => return cg.fail("ty {} took {} tracking arg indices", .{ ty.fmt(pt), arg_mcv_i }),
                        1 => arg_mcv[0],
                        2 => .{ .register_pair = .{ arg_mcv[0].register, arg_mcv[1].register } },
                    };
                    continue;
                }
                return cg.fail("TODO: pass args by stack", .{});
            }
        },
        .auto => {
            result.stack_align = .@"16";

            if (zcu.comp.config.any_error_tracing) {
                @panic("TODO: error return tracing!");
            }

            var param_int_regs: []const Register = &abi.Registers.Integer.function_arg_regs;

            result.return_value = if (ret_ty.isNoReturn(zcu))
                .init(.unreach)
            else if (!ret_ty.hasRuntimeBitsIgnoreComptime(zcu))
                .init(.none)
            else return_value: {
                const ret_regs = abi.Registers.Integer.function_ret_regs;
                const ret_size: u21 = @intCast(ret_ty.abiSize(zcu));
                switch (cg.typeRegClass(ret_ty)) {
                    .int => if (ret_size <= 8)
                        break :return_value .init(.{ .register = ret_regs[0] })
                    else if (ret_ty.isSliceAtRuntime(zcu))
                        break :return_value .init(.{ .register_pair = ret_regs[0..2].* }),
                    else => {}, // TODO: pass returns through float and vector registers
                }
                // reserve the first parameter register for the intedirect return
                const ret_indirect_reg = param_int_regs[0];
                param_int_regs = param_int_regs[1..];
                break :return_value .{
                    .short = .{ .indirect = .{ .reg = ret_regs[0] } },
                    .long = .{ .indirect = .{ .reg = ret_indirect_reg } },
                };
            };

            // Input params
            for (param_types, result.args) |param_ty, *arg| {
                if (!param_ty.hasRuntimeBitsIgnoreComptime(zcu)) {
                    arg.* = .none;
                    continue;
                }
                const param_size: u31 = @intCast(param_ty.abiSize(zcu));
                switch (cg.typeRegClass(param_ty)) {
                    .int => if (param_int_regs.len >= 1 and param_size <= 8) {
                        arg.* = .{ .register = param_int_regs[0] };
                        param_int_regs = param_int_regs[1..];
                        continue;
                    } else if (param_int_regs.len >= 2 and param_ty.isSliceAtRuntime(zcu)) {
                        arg.* = .{ .register_pair = param_int_regs[0..2].* };
                        param_int_regs = param_int_regs[2..];
                        continue;
                    },
                    else => {}, // TODO: pass arguments through float and vector registers
                }
                const param_align = param_ty.abiAlignment(zcu);
                result.stack_byte_count = @intCast(param_align.forward(result.stack_byte_count));
                result.stack_align = result.stack_align.max(param_align);
                arg.* = .{ .load_frame = .{
                    .index = stack_frame_base,
                    .off = result.stack_byte_count,
                } };
                result.stack_byte_count += param_size;
            }
        },
        else => return cg.fail("TODO implement function parameters for {} on riscv64", .{cc}),
    }

    result.stack_byte_count = @intCast(result.stack_align.forward(result.stack_byte_count));
    return result;
}

fn wantSafety(cg: *CodeGen) bool {
    return switch (cg.mod.optimize_mode) {
        .Debug => true,
        .ReleaseSafe => true,
        .ReleaseFast => false,
        .ReleaseSmall => false,
    };
}

fn fail(cg: *const CodeGen, comptime format: []const u8, args: anytype) error{ OutOfMemory, CodegenFail } {
    @branchHint(.cold);
    const zcu = cg.pt.zcu;
    switch (cg.owner) {
        .nav_index => |i| return zcu.codegenFail(i, format, args),
        .lazy_sym => |s| return zcu.codegenFailType(s.ty, format, args),
    }
    return error.CodegenFail;
}

fn failMsg(cg: *const CodeGen, msg: *ErrorMsg) error{ OutOfMemory, CodegenFail } {
    @branchHint(.cold);
    const zcu = cg.pt.zcu;
    switch (cg.owner) {
        .nav_index => |i| return zcu.codegenFailMsg(i, msg),
        .lazy_sym => |s| return zcu.codegenFailTypeMsg(s.ty, msg),
    }
    return error.CodegenFail;
}

fn parseRegName(name: []const u8) ?Register {
    // The `fp` alias for `s0` is awkward to fit into the current `Register` scheme, so for now we
    // special-case it here.
    if (std.mem.eql(u8, name, "fp")) return .s0;

    return std.meta.stringToEnum(Register, name);
}

fn typeOf(cg: *CodeGen, inst: Air.Inst.Ref) Type {
    return cg.air.typeOf(inst, &cg.pt.zcu.intern_pool);
}

fn typeOfIndex(cg: *CodeGen, inst: Air.Inst.Index) Type {
    const zcu = cg.pt.zcu;
    return switch (cg.air.instructions.items(.tag)[@intFromEnum(inst)]) {
        .loop_switch_br => cg.typeOf(cg.air.unwrapSwitch(inst).operand),
        else => cg.air.typeOfIndex(inst, &zcu.intern_pool),
    };
}

fn hasFeature(cg: *CodeGen, feature: Target.riscv.Feature) bool {
    return Target.riscv.featureSetHas(cg.target.cpu.features, feature);
}

fn vectorBits(cg: *CodeGen) u32 {
    var vec_bit_length: u32 = 256;
    inline for (.{
        .zvl32b,
        .zvl64b,
        .zvl128b,
        .zvl256b,
        .zvl512b,
        .zvl1024b,
        .zvl2048b,
        .zvl4096b,
        .zvl8192b,
        .zvl16384b,
        .zvl32768b,
        .zvl65536b,
    }) |feat| {
        if (std.Target.riscv.featureSetHas(cg.target.cpu.features, feat)) {
            const name = @tagName(feat);
            vec_bit_length = std.fmt.parseInt(u32, name[3 .. name.len - 1], 10) catch unreachable;
        }
    }
    return vec_bit_length;
}

fn suggestedVlMul(cg: *CodeGen, ty: Type) !bits.VlMul {
    const zcu = cg.pt.zcu;
    assert(ty.isVector(zcu)); // pass in the vector type

    const vb = cg.vectorBits();
    const ty_bits = ty.bitSize(zcu);

    const regs = math.divCeil(u32, @intCast(ty_bits), vb) catch unreachable;
    const mul = try math.ceilPowerOfTwo(u32, regs);

    return switch (mul) {
        1 => .m1,
        2 => .m2,
        4 => .m4,
        8 => .m8,
        else => return cg.fail("suggestedVlMul mul {d}", .{mul}),
    };
}

pub fn errUnionPayloadOffset(payload_ty: Type, zcu: *Zcu) u64 {
    if (!payload_ty.hasRuntimeBitsIgnoreComptime(zcu)) return 0;
    const payload_align = payload_ty.abiAlignment(zcu);
    const error_align = Type.anyerror.abiAlignment(zcu);
    if (payload_align.compare(.gte, error_align) or !payload_ty.hasRuntimeBitsIgnoreComptime(zcu)) {
        return 0;
    } else {
        return payload_align.forward(Type.anyerror.abiSize(zcu));
    }
}

pub fn errUnionErrorOffset(payload_ty: Type, zcu: *Zcu) u64 {
    if (!payload_ty.hasRuntimeBitsIgnoreComptime(zcu)) return 0;
    const payload_align = payload_ty.abiAlignment(zcu);
    const error_align = Type.anyerror.abiAlignment(zcu);
    if (payload_align.compare(.gte, error_align) and payload_ty.hasRuntimeBitsIgnoreComptime(zcu)) {
        return error_align.forward(payload_ty.abiSize(zcu));
    } else {
        return 0;
    }
}

fn promoteInt(cg: *CodeGen, ty: Type) Type {
    const pt = cg.pt;
    const zcu = pt.zcu;
    const int_info: InternPool.Key.IntType = switch (ty.toIntern()) {
        .bool_type => .{ .signedness = .unsigned, .bits = 1 },
        else => if (ty.isAbiInt(zcu)) ty.intInfo(zcu) else return ty,
    };
    for ([_]Type{
        Type.c_int,      Type.c_uint,
        Type.c_long,     Type.c_ulong,
        Type.c_longlong, Type.c_ulonglong,
    }) |promote_ty| {
        const promote_info = promote_ty.intInfo(zcu);
        if (int_info.signedness == .signed and promote_info.signedness == .unsigned) continue;
        if (int_info.bits + @intFromBool(int_info.signedness == .unsigned and
            promote_info.signedness == .signed) <= promote_info.bits) return promote_ty;
    }
    return ty;
}

fn promoteVarArg(cg: *CodeGen, ty: Type) Type {
    if (!ty.isRuntimeFloat()) return cg.promoteInt(ty);
    switch (ty.floatBits(cg.target.*)) {
        32, 64 => return Type.f64,
        else => |float_bits| {
            assert(float_bits == cg.target.cTypeBitSize(.longdouble));
            return Type.c_longdouble;
        },
    }
}

fn floatBits(cg: *CodeGen, ty: Type) ?u16 {
    return if (ty.isRuntimeFloat()) ty.floatBits(cg.target.*) else null;
}

fn intInfo(cg: *CodeGen, ty: Type) ?std.builtin.Type.Int {
    const zcu = cg.pt.zcu;
    const ip = &zcu.intern_pool;
    var ty_index = ty.ip_index;
    while (true) switch (ip.indexToKey(ty_index)) {
        .int_type => |int_type| return int_type,
        .ptr_type => |ptr_type| return switch (ptr_type.flags.size) {
            .one, .many, .c => .{ .signedness = .unsigned, .bits = cg.target.ptrBitWidth() },
            .slice => null,
        },
        .opt_type => |opt_child| return if (!Type.fromInterned(opt_child).hasRuntimeBitsIgnoreComptime(zcu))
            .{ .signedness = .unsigned, .bits = 1 }
        else switch (ip.indexToKey(opt_child)) {
            .ptr_type => |ptr_type| switch (ptr_type.flags.size) {
                .one, .many => switch (ptr_type.flags.is_allowzero) {
                    false => .{ .signedness = .unsigned, .bits = cg.target.ptrBitWidth() },
                    true => null,
                },
                .slice, .c => null,
            },
            else => null,
        },
        .error_union_type => |error_union_type| return if (!Type.fromInterned(error_union_type.payload_type)
            .hasRuntimeBitsIgnoreComptime(zcu)) .{ .signedness = .unsigned, .bits = zcu.errorSetBits() } else null,
        .simple_type => |simple_type| return switch (simple_type) {
            .bool => .{ .signedness = .unsigned, .bits = 1 },
            .anyerror => .{ .signedness = .unsigned, .bits = zcu.errorSetBits() },
            .isize => .{ .signedness = .signed, .bits = cg.target.ptrBitWidth() },
            .usize => .{ .signedness = .unsigned, .bits = cg.target.ptrBitWidth() },
            .c_char => .{ .signedness = cg.target.charSignedness(), .bits = cg.target.cTypeBitSize(.char) },
            .c_short => .{ .signedness = .signed, .bits = cg.target.cTypeBitSize(.short) },
            .c_ushort => .{ .signedness = .unsigned, .bits = cg.target.cTypeBitSize(.short) },
            .c_int => .{ .signedness = .signed, .bits = cg.target.cTypeBitSize(.int) },
            .c_uint => .{ .signedness = .unsigned, .bits = cg.target.cTypeBitSize(.int) },
            .c_long => .{ .signedness = .signed, .bits = cg.target.cTypeBitSize(.long) },
            .c_ulong => .{ .signedness = .unsigned, .bits = cg.target.cTypeBitSize(.long) },
            .c_longlong => .{ .signedness = .signed, .bits = cg.target.cTypeBitSize(.longlong) },
            .c_ulonglong => .{ .signedness = .unsigned, .bits = cg.target.cTypeBitSize(.longlong) },
            .f16, .f32, .f64, .f80, .f128, .c_longdouble => null,
            .anyopaque,
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
        },
        .struct_type => {
            const loaded_struct = ip.loadStructType(ty_index);
            switch (loaded_struct.layout) {
                .auto, .@"extern" => return null,
                .@"packed" => ty_index = loaded_struct.backingIntTypeUnordered(ip),
            }
        },
        .union_type => return switch (ip.loadUnionType(ty_index).flagsUnordered(ip).layout) {
            .auto, .@"extern" => null,
            .@"packed" => .{ .signedness = .unsigned, .bits = @intCast(ty.bitSize(zcu)) },
        },
        .enum_type => ty_index = ip.loadEnumType(ty_index).tag_ty,
        .error_set_type, .inferred_error_set_type => return .{ .signedness = .unsigned, .bits = zcu.errorSetBits() },
        else => return null,
    };
}

const Temp = struct {
    index: Air.Inst.Index,

    const Index = enum(u5) {
        _,

        const max = std.math.maxInt(@typeInfo(Index).@"enum".tag_type);

        fn toIndex(index: Index) Air.Inst.Index {
            return .fromTargetIndex(@intFromEnum(index));
        }

        fn fromIndex(index: Air.Inst.Index) Index {
            return @enumFromInt(index.toTargetIndex());
        }

        fn tracking(index: Index, cg: *CodeGen) *InstTracking {
            return &cg.inst_tracking.values()[@intFromEnum(index)];
        }

        fn isValid(index: Index, cg: *CodeGen) bool {
            return index.tracking(cg).short != .dead;
        }

        fn typeOf(index: Index, cg: *CodeGen) Type {
            assert(index.isValid(cg));
            return cg.temp_type[@intFromEnum(index)];
        }
    };

    fn unwrap(temp: Temp, cg: *CodeGen) union(enum) {
        ref: Air.Inst.Ref,
        temp: Index,
    } {
        switch (temp.index.unwrap()) {
            .ref => |ref| return .{ .ref = ref },
            .target => |target_index| {
                const temp_index: Index = @enumFromInt(target_index);
                assert(temp_index.isValid(cg));
                return .{ .temp = temp_index };
            },
        }
    }

    fn typeOf(temp: Temp, cg: *CodeGen) Type {
        return switch (temp.unwrap(cg)) {
            .ref => switch (cg.air.instructions.items(.tag)[@intFromEnum(temp.index)]) {
                .loop_switch_br => cg.typeOf(cg.air.unwrapSwitch(temp.index).operand),
                else => cg.air.typeOfIndex(temp.index, &cg.pt.zcu.intern_pool),
            },
            .temp => |temp_index| temp_index.typeOf(cg),
        };
    }

    fn die(temp: Temp, cg: *CodeGen) InnerError!void {
        switch (temp.unwrap(cg)) {
            .ref => {},
            .temp => |temp_index| try temp_index.tracking(cg).die(cg, temp_index.toIndex()),
        }
    }

    fn finish(
        temp: Temp,
        inst: Air.Inst.Index,
        op_refs: []const Air.Inst.Ref,
        op_temps: []const Temp,
        cg: *CodeGen,
    ) InnerError!void {
        const tomb_bits = cg.liveness.getTombBits(inst);
        for (0.., op_refs, op_temps) |op_index, op_ref, op_temp| {
            if (op_temp.index != temp.index) try op_temp.die(cg);
            if (tomb_bits & @as(Liveness.Bpi, 1) << @intCast(op_index) == 0) continue;
            if (cg.reused_operands.isSet(op_index)) continue;
            try cg.processDeath(op_ref.toIndexAllowNone() orelse continue);
        }
        if (cg.liveness.isUnused(inst)) try temp.die(cg) else switch (temp.unwrap(cg)) {
            .ref => {
                const result = try cg.allocRegOrMem(cg.typeOfIndex(inst), inst, true);
                try cg.genCopy(cg.typeOfIndex(inst), result, temp.tracking(cg).short);
                tracking_log.debug("{} => {} (birth)", .{ inst, result });
                cg.inst_tracking.putAssumeCapacityNoClobber(inst, .init(result));
            },
            .temp => |temp_index| {
                const temp_tracking = temp_index.tracking(cg);
                tracking_log.debug("{} => {} (birth)", .{ inst, temp_tracking.short });
                cg.inst_tracking.putAssumeCapacityNoClobber(inst, .init(temp_tracking.short));
                assert(cg.reuseTemp(inst, temp_index.toIndex(), temp_tracking));
            },
        }
    }

    fn isMut(temp: Temp, cg: *CodeGen) bool {
        return switch (temp.unwrap(cg)) {
            .ref => false,
            .temp => |temp_index| switch (temp_index.tracking(cg).short) {
                .none,
                .unreach,
                .dead,
                .undef,
                .immediate,
                .register_offset,
                .memory,
                .load_symbol,
                .lea_symbol,
                .indirect,
                .lea_frame,
                .reserved_frame,
                .air_ref,
                .load_tlv,
                .lea_tlv,
                => false,
                .register,
                .register_pair,
                => true,
                .load_frame => |frame_addr| !frame_addr.index.isNamed(),
            },
        };
    }

    const AccessOptions = struct {
        disp: i32 = 0,
        safe: bool = false,
    };

    fn load(ptr: *Temp, val_ty: Type, opts: AccessOptions, cg: *CodeGen) InnerError!Temp {
        const val = try cg.tempAlloc(val_ty);
        try ptr.toOffset(opts.disp, cg);
        while (try ptr.toLea(cg)) {}
        const val_mcv = val.tracking(cg).short;
        switch (val_mcv) {
            .register => |val_reg| try ptr.loadReg(val_ty, val_reg, cg),
            .memory, .indirect, .load_frame, .load_symbol => {
                var val_ptr = try cg.tempInit(.usize, val_mcv.address());
                var len = try cg.tempInit(.usize, .{ .immediate = val_ty.abiSize(cg.pt.zcu) });
                try val_ptr.memcpy(ptr, &len, cg);
                try val_ptr.die(cg);
                try len.die(cg);
            },
            else => |mcv| std.debug.panic("{s}: {}\n", .{ @src().fn_name, mcv }),
        }
        return val;
    }

    fn loadReg(ptr: *Temp, dst_ty: Type, dst_reg: Register, cg: *CodeGen) InnerError!void {
        // hack around linker relocation bugs
        switch (ptr.tracking(cg).short) {
            else => {},
            .lea_symbol => @panic("TODO"),
        }
        const mem_value = ptr.tracking(cg).short.deref();
        try cg.genSetReg(dst_ty, dst_reg, mem_value);
    }

    fn getOffset(temp: Temp, off: i32, cg: *CodeGen) InnerError!Temp {
        _ = off;
        const new_temp_index = cg.next_temp_index;
        cg.temp_type[@intFromEnum(new_temp_index)] = .usize;
        cg.next_temp_index = @enumFromInt(@intFromEnum(new_temp_index) + 1);
        switch (temp.tracking(cg).short) {
            else => |mcv| std.debug.panic("{s}: {}\n", .{ @src().fn_name, mcv }),
        }
        return .{ .index = new_temp_index.toIndex() };
    }

    fn toOffset(temp: *Temp, off: i32, cg: *CodeGen) InnerError!void {
        if (off == 0) return;
        switch (temp.unwrap(cg)) {
            .ref => {},
            .temp => |temp_index| {
                const temp_tracking = temp_index.tracking(cg);
                switch (temp_tracking.short) {
                    else => {},
                    .register => |reg| {
                        try cg.freeValue(temp_tracking.long);
                        temp_tracking.* = .init(.{ .register_offset = .{
                            .reg = reg,
                            .off = off,
                        } });
                        return;
                    },
                    .register_offset => |reg_off| {
                        try cg.freeValue(temp_tracking.long);
                        temp_tracking.* = .init(.{ .register_offset = .{
                            .reg = reg_off.reg,
                            .off = reg_off.off + off,
                        } });
                        return;
                    },
                    .lea_symbol => |sym_off| {
                        assert(std.meta.eql(temp_tracking.long.lea_symbol, sym_off));
                        temp_tracking.* = .init(.{ .lea_symbol = .{
                            .sym_index = sym_off.sym_index,
                            .off = sym_off.off + off,
                        } });
                        return;
                    },
                    .lea_frame => |frame_addr| {
                        assert(std.meta.eql(temp_tracking.long.lea_frame, frame_addr));
                        temp_tracking.* = .init(.{ .lea_frame = .{
                            .index = frame_addr.index,
                            .off = frame_addr.off + off,
                        } });
                        return;
                    },
                }
            },
        }
        const new_temp = try temp.getOffset(off, cg);
        try temp.die(cg);
        temp.* = new_temp;
    }

    fn toLea(temp: *Temp, cg: *CodeGen) InnerError!bool {
        switch (temp.tracking(cg).short) {
            .none,
            .unreach,
            .dead,
            .undef,
            .register_pair,
            .reserved_frame,
            .air_ref,
            => unreachable, // not a valid pointer
            .immediate,
            .register,
            .register_offset,
            .lea_tlv,
            .lea_frame,
            => return false,
            .memory,
            .indirect,
            .load_symbol,
            .load_tlv,
            .load_frame,
            => return temp.toRegClass(true, .int, cg),
            .lea_symbol => |sym_off| {
                const off = sym_off.off;
                if (off == 0) return false;
                try temp.toOffset(-off, cg);
                while (try temp.toRegClass(true, .int, cg)) {}
                try temp.toOffset(off, cg);
                return true;
            },
        }
    }

    fn toMemory(temp: *Temp, mut: bool, cg: *CodeGen) InnerError!bool {
        const temp_tracking = temp.tracking(cg);
        if ((!mut or temp.isMut(cg)) and temp_tracking.short.isMemory()) return false;
        const new_temp_index = cg.next_temp_index;
        const ty = temp.typeOf(cg);
        cg.temp_type[@intFromEnum(new_temp_index)] = ty;
        const new_frame_index = try cg.allocFrameIndex(.initSpill(ty, cg.pt.zcu));
        try cg.genSetMem(.{ .frame = new_frame_index }, 0, ty, temp_tracking.short);
        new_temp_index.tracking(cg).* = .init(.{ .load_frame = .{ .index = new_frame_index } });
        try temp.die(cg);
        cg.next_temp_index = @enumFromInt(@intFromEnum(new_temp_index) + 1);
        temp.* = .{ .index = new_temp_index.toIndex() };
        return true;
    }

    fn toBase(temp: *Temp, mut: bool, cg: *CodeGen) InnerError!bool {
        const temp_tracking = temp.tracking(cg);
        if ((!mut or temp.isMut(cg)) and temp_tracking.short.isBase()) return false;
        if (try temp.toMemory(mut, cg)) return true;
        const new_temp_index = cg.next_temp_index;
        cg.temp_type[@intFromEnum(new_temp_index)] = temp.typeOf(cg);
        const new_reg = try cg.register_manager.allocReg(
            new_temp_index.toIndex(),
            abi.Registers.Integer.general_purpose,
        );
        try cg.genSetReg(.usize, new_reg, temp_tracking.short.address());
        new_temp_index.tracking(cg).* = .init(.{ .indirect = .{ .reg = new_reg } });
        try temp.die(cg);
        cg.next_temp_index = @enumFromInt(@intFromEnum(new_temp_index) + 1);
        temp.* = .{ .index = new_temp_index.toIndex() };
        return true;
    }

    fn toRegClass(temp: *Temp, mut: bool, rc: abi.RegisterClass, cg: *CodeGen) InnerError!bool {
        const val = temp.tracking(cg).short;
        if (!mut or temp.isMut(cg)) switch (val) {
            else => {},
            .register => |reg| if (reg.class() == rc) return false,
            .register_offset => |reg_off| if (reg_off.reg.class() == rc and reg_off.off == 0) return false,
        };
        const ty = temp.typeOf(cg);
        const new_temp_index = cg.next_temp_index;
        cg.temp_type[@intFromEnum(new_temp_index)] = ty;
        const new_reg = try cg.register_manager.allocReg(new_temp_index.toIndex(), regSetForRegClass(rc));
        try cg.genSetReg(ty, new_reg, val);
        new_temp_index.tracking(cg).* = .init(.{ .register = new_reg });
        try temp.die(cg);
        cg.next_temp_index = @enumFromInt(@intFromEnum(new_temp_index) + 1);
        temp.* = .{ .index = new_temp_index.toIndex() };
        return true;
    }

    fn toPair(first_temp: *Temp, second_temp: *Temp, cg: *CodeGen) InnerError!void {
        while (true) for ([_]*Temp{ first_temp, second_temp }) |part_temp| {
            if (try part_temp.toRegClass(true, .int, cg)) break;
        } else break;
        const first_temp_tracking = first_temp.unwrap(cg).temp.tracking(cg);
        const second_temp_tracking = second_temp.unwrap(cg).temp.tracking(cg);
        const result: MCValue = .{ .register_pair = .{
            first_temp_tracking.short.register,
            second_temp_tracking.short.register,
        } };
        const result_temp_index = cg.next_temp_index;
        const result_temp: Temp = .{ .index = result_temp_index.toIndex() };
        assert(cg.reuseTemp(result_temp.index, first_temp.index, first_temp_tracking));
        assert(cg.reuseTemp(result_temp.index, second_temp.index, second_temp_tracking));
        cg.temp_type[@intFromEnum(result_temp_index)] = .slice_const_u8;
        result_temp_index.tracking(cg).* = .init(result);
        first_temp.* = result_temp;
        second_temp.* = result_temp;
    }

    /// Assumes `temp` is an integer.
    fn negate(temp: *Temp, cg: *CodeGen) Select.Error!void {
        while (try temp.toRegClass(true, .int, cg)) {}
        const val = temp.tracking(cg).short;
        switch (val) {
            .register => |reg| try cg.asmIType(.xori, reg, reg, .s(1)),
            else => return cg.fail("TODO: negate {s}", .{@tagName(val)}),
        }
    }

    /// Supports any `op` using `cg.intInfo(lhs.typeOf(cg)).?.signedness` as the signedness.
    /// Returns `error.SelectFailed` when `cg.intInfo(lhs.typeOf(cg)) == null`.
    fn cmpInts(lhs: *Temp, op: std.math.CompareOperator, rhs: *Temp, cg: *CodeGen) Select.Error!Temp {
        var ops: [2]Temp = .{ lhs.*, rhs.* };
        var res: [1]Temp = undefined;
        switch (op) {
            .lt, .lte, .gte, .gt => {
                const commute = switch (op) {
                    .lt, .gte => false,
                    .lte, .gt => true,
                    else => unreachable,
                };
                if (commute) std.mem.swap(Temp, &ops[0], &ops[1]);
                try cg.select(&res, &.{.bool}, &ops, comptime &.{ .{
                    .src_constraints = .{
                        .{ .signed_int = .double },
                        .{ .signed_int = .double },
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .int_reg, .none } },
                        .{ .src = .{ .to_mut_int_reg, .to_int_reg, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .slt, .dst0d, .src0d, .src1d },
                    } },
                }, .{
                    .src_constraints = .{
                        .{ .unsigned_int = .double },
                        .{ .unsigned_int = .double },
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .int_reg, .none } },
                        .{ .src = .{ .to_mut_int_reg, .to_int_reg, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .sltu, .dst0d, .src0d, .src1d },
                    } },
                } });
            },
            inline .eq, .neq => |cmp_op| {
                try cg.select(&res, &.{.bool}, &ops, comptime &.{.{
                    .src_constraints = .{
                        .{ .int = .double },
                        .{ .int = .double },
                        .any,
                    },
                    .dst_temps = .{ .{ .ref = .src0 }, .unused },
                    .patterns = &.{
                        .{ .src = .{ .mut_int_reg, .int_reg, .none } },
                        .{ .src = .{ .to_mut_int_reg, .to_int_reg, .none } },
                    },
                    .each = .{ .once = &.{
                        .{ ._, .xor, .dst0d, .src0d, .src1d },
                        switch (cmp_op) {
                            .eq => .{ ._, .sltiu, .dst0d, .dst0d, .ui(1) },
                            .neq => .{ ._, .sltu, .dst0d, .zero, .dst0d },
                            else => unreachable,
                        },
                    } },
                }});
            },
        }
        if (switch (op) {
            .lt, .gt, .eq, .neq => false,
            .lte, .gte => true,
        }) {
            try res[0].negate(cg);
        }
        lhs.*, rhs.* = ops;
        return res[0];
    }

    fn tracking(temp: Temp, cg: *CodeGen) InstTracking {
        return cg.inst_tracking.get(temp.index).?;
    }
};

fn resetTemps(cg: *CodeGen) InnerError!void {
    var any_valid = false;
    for (0..@intFromEnum(cg.next_temp_index)) |temp_index| {
        const temp: Temp.Index = @enumFromInt(temp_index);
        if (temp.isValid(cg)) {
            any_valid = true;
            tracking_log.err("failed to kill {}: {}", .{
                temp.toIndex(),
                cg.temp_type[temp_index].fmt(cg.pt),
            });
        }
        cg.temp_type[temp_index] = undefined;
    }
    if (any_valid) return cg.fail("failed to kill all temps", .{});
    cg.next_temp_index = @enumFromInt(0);
}

fn reuseTemp(
    cg: *CodeGen,
    new_inst: Air.Inst.Index,
    old_inst: Air.Inst.Index,
    tracking: *InstTracking,
) bool {
    switch (tracking.short) {
        .register,
        .register_pair,
        .indirect,
        => for (tracking.short.getRegs()) |tracked_reg| {
            if (RegisterManager.indexOfRegIntoTracked(tracked_reg)) |tracked_index| {
                cg.register_manager.registers[tracked_index] = new_inst;
            }
        },
        .load_frame => |frame_addr| if (frame_addr.index.isNamed()) return false,
        else => {},
    }
    tracking.reuse(cg, new_inst, old_inst);
    return true;
}

fn tempInit(cg: *CodeGen, ty: Type, value: MCValue) InnerError!Temp {
    const temp_index = cg.next_temp_index;
    temp_index.tracking(cg).* = .init(value);
    cg.temp_type[@intFromEnum(temp_index)] = ty;
    try cg.getValue(value, temp_index.toIndex());
    cg.next_temp_index = @enumFromInt(@intFromEnum(temp_index) + 1);
    return .{ .index = temp_index.toIndex() };
}

fn tempFromOperand(
    cg: *CodeGen,
    inst: Air.Inst.Index,
    op_index: Liveness.OperandInt,
    op_ref: Air.Inst.Ref,
    ignore_death: bool,
) InnerError!Temp {
    const zcu = cg.pt.zcu;
    const ip = &zcu.intern_pool;

    if (ignore_death or !cg.liveness.operandDies(inst, op_index)) {
        if (op_ref.toIndex()) |op_inst| return .{ .index = op_inst };
        const val = op_ref.toInterned().?;
        const gop = try cg.const_tracking.getOrPut(cg.gpa, val);
        if (!gop.found_existing) gop.value_ptr.* = .init(init: {
            const const_mcv = try cg.genTypedValue(.fromInterned(val));
            switch (const_mcv) {
                .lea_tlv => return cg.fail("TODO: lea tlv tempFromOperand", .{}),
                else => break :init const_mcv,
            }
        });
        return cg.tempInit(.fromInterned(ip.typeOf(val)), gop.value_ptr.short);
    }

    const temp_index = cg.next_temp_index;
    const temp: Temp = .{ .index = temp_index.toIndex() };
    const op_inst = op_ref.toIndex().?;
    const tracking = cg.getResolvedInstValue(op_inst);
    temp_index.tracking(cg).* = tracking.*;
    if (!cg.reuseTemp(temp.index, op_inst, tracking)) return .{ .index = op_ref.toIndex().? };
    cg.temp_type[@intFromEnum(temp_index)] = cg.typeOf(op_ref);
    cg.next_temp_index = @enumFromInt(@intFromEnum(temp_index) + 1);
    return temp;
}

inline fn tempsFromOperands(cg: *CodeGen, inst: Air.Inst.Index, op_refs: anytype) InnerError![op_refs.len]Temp {
    var temps: [op_refs.len]Temp = undefined;
    inline for (&temps, 0.., op_refs) |*temp, op_index, op_ref| {
        temp.* = try cg.tempFromOperand(inst, op_index, op_ref, inline for (0..op_index) |prev_op_index| {
            if (op_ref == op_refs[prev_op_index]) break true;
        } else false);
    }
    return temps;
}

const InstructionOperand = union(enum) {
    none,
    reg: Register,
    mem: Memory,
    imm: Immediate,
    inst: Mir.Inst.Index,
    sym_index: SymbolOffset,
};

fn asmOps(cg: *CodeGen, mnem: Mnemonic, ops: [3]InstructionOperand) !void {
    return switch (ops[0]) {
        .none => cg.asmNone(mnem),
        .reg => |reg1| switch (ops[1]) {
            .reg => |reg2| switch (ops[2]) {
                .imm => |imm1| cg.asmIType(mnem, reg1, reg2, imm1),
                .reg => |reg3| cg.asmRType(mnem, reg1, reg2, reg3),
                .none => cg.asmRType(
                    mnem,
                    reg1,
                    reg2,
                    switch (mnem) {
                        // zig fmt: off
                        .clz, .clzw   => @enumFromInt(0b00000),
                        .ctz, .ctzw   => @enumFromInt(0b00001),
                        .cpop, .cpopw => @enumFromInt(0b00010),
                        // zig fmt: on
                        else => return error.InvalidInstruction,
                    },
                ),
                else => error.InvalidInstruction,
            },
            .imm => |imm1| switch (ops[2]) {
                .reg => |reg2| switch (mnem) {
                    .sd => cg.asmIType(mnem, reg2, reg1, imm1),
                    .ld => cg.asmIType(mnem, reg1, reg2, imm1),
                    else => error.InvalidInstruction,
                },
                .none => cg.asmUType(mnem, reg1, imm1),
                else => error.InvalidInstruction,
            },
            .none => switch (mnem) {
                .jalr => cg.asmIType(mnem, .ra, reg1, Immediate.s(0)),
                else => error.InvalidInstruction,
            },
            else => error.InvalidInstruction,
        },
        else => error.InvalidInstruction,
    };
}

fn tempAlloc(cg: *CodeGen, ty: Type) InnerError!Temp {
    const temp_index = cg.next_temp_index;
    temp_index.tracking(cg).* = .init(try cg.allocRegOrMem(ty, temp_index.toIndex(), true));
    cg.temp_type[@intFromEnum(temp_index)] = ty;
    cg.next_temp_index = @enumFromInt(@intFromEnum(temp_index) + 1);
    return .{ .index = temp_index.toIndex() };
}

fn tempAllocReg(cg: *CodeGen, ty: Type, rs: RegisterManager.RegisterBitSet) InnerError!Temp {
    const temp_index = cg.next_temp_index;
    temp_index.tracking(cg).* =
        .init(.{ .register = try cg.register_manager.allocReg(temp_index.toIndex(), rs) });
    cg.temp_type[@intFromEnum(temp_index)] = ty;
    cg.next_temp_index = @enumFromInt(@intFromEnum(temp_index) + 1);
    return .{ .index = temp_index.toIndex() };
}

const Select = struct {
    cg: *CodeGen,
    types: [@intFromEnum(Select.Operand.Ref.none)]Type,
    temps: [@intFromEnum(Select.Operand.Ref.none)]Temp,
    labels: [@intFromEnum(Label._)]struct {
        backward: ?Mir.Inst.Index,
        forward: [1]?Mir.Inst.Index,
    },

    const Error = InnerError || error{SelectFailed};

    const Instruction = struct {
        Label,
        Mnemonic,
        Select.Operand,
        Select.Operand,
        Select.Operand,
    };
    const Label = enum { @"0:", @"1:", @"2:", @"3:", @"4:", @"_" };
    const Case = struct {
        required_features: [4]?std.Target.riscv.Feature = @splat(null),
        src_constraints: [@intFromEnum(Select.Operand.Ref.none) - @intFromEnum(Select.Operand.Ref.src0)]Constraint = @splat(.any),
        dst_constraints: [@intFromEnum(Select.Operand.Ref.src0) - @intFromEnum(Select.Operand.Ref.dst0)]Constraint = @splat(.any),
        patterns: []const Select.Pattern,
        call_frame: packed struct(u16) { size: u10 = 0, alignment: InternPool.Alignment } = .{ .size = 0, .alignment = .none },
        extra_temps: [@intFromEnum(Select.Operand.Ref.dst0) - @intFromEnum(Select.Operand.Ref.tmp0)]TempSpec = @splat(.unused),
        dst_temps: [@intFromEnum(Select.Operand.Ref.src0) - @intFromEnum(Select.Operand.Ref.dst0)]TempSpec.Kind = @splat(.unused),
        each: union(enum) {
            once: []const Instruction,
        },
    };

    const Pattern = struct {
        src: [@intFromEnum(Select.Operand.Ref.none) - @intFromEnum(Select.Operand.Ref.src0)]Src,
        commute: struct { u8, u8 } = .{ 0, 0 },

        const Src = union(enum) {
            none,
            mem,
            imm8,
            imm16,
            imm32,
            int_reg,
            to_int_reg,
            mut_int_reg,
            to_mut_int_reg,
            float_reg,
            to_float_reg,
            mut_float_reg,
            to_mut_float_reg,

            fn matches(src: Src, temp: Temp, cg: *CodeGen) bool {
                const float_byte_size: u16 = if (cg.hasFeature(.d)) 8 else 4;
                return switch (src) {
                    .none => temp.tracking(cg).short == .none,
                    .imm8 => switch (temp.tracking(cg).short) {
                        .immediate => |imm| std.math.cast(u8, imm) != null,
                        else => false,
                    },
                    .imm16 => switch (temp.tracking(cg).short) {
                        .immediate => |imm| std.math.cast(u16, imm) != null,
                        else => false,
                    },
                    .imm32 => switch (temp.tracking(cg).short) {
                        .immediate => |imm| std.math.cast(u32, imm) != null,
                        else => false,
                    },
                    .mem => temp.tracking(cg).short.isMemory(),
                    .to_int_reg, .to_mut_int_reg => temp.typeOf(cg).abiSize(cg.pt.zcu) <= 8,
                    .int_reg => temp.typeOf(cg).abiSize(cg.pt.zcu) <= 8 and
                        switch (temp.tracking(cg).short) {
                            .register => |reg| reg.class() == .int,
                            else => false,
                        },
                    .mut_int_reg => temp.isMut(cg) and temp.typeOf(cg).abiSize(cg.pt.zcu) <= 8 and
                        switch (temp.tracking(cg).short) {
                            .register => |reg| reg.class() == .int,
                            else => false,
                        },
                    .to_float_reg, .to_mut_float_reg => temp.typeOf(cg).abiSize(cg.pt.zcu) <= float_byte_size,
                    .float_reg => temp.typeOf(cg).abiSize(cg.pt.zcu) <= float_byte_size and
                        switch (temp.tracking(cg).short) {
                            .register => |reg| reg.class() == .float,
                            else => false,
                        },
                    .mut_float_reg => temp.isMut(cg) and temp.typeOf(cg).abiSize(cg.pt.zcu) <= float_byte_size and
                        switch (temp.tracking(cg).short) {
                            .register => |reg| reg.class() == .float,
                            else => false,
                        },
                };
            }

            fn convert(src: Src, temp: *Temp, cg: *CodeGen) InnerError!bool {
                return switch (src) {
                    .none, .imm8, .imm16, .imm32 => false,
                    .mem => try temp.toBase(false, cg),
                    .int_reg, .to_int_reg => try temp.toRegClass(false, .int, cg),
                    .mut_int_reg, .to_mut_int_reg => try temp.toRegClass(true, .int, cg),
                    .float_reg, .to_float_reg => try temp.toRegClass(false, .float, cg),
                    .mut_float_reg, .to_mut_float_reg => try temp.toRegClass(true, .float, cg),
                };
            }
        };
    };

    const Constraint = union(enum) {
        any,
        int: Memory.Size,
        float: Memory.Size,
        exact_int: u16,
        unsigned_int: Memory.Size,
        signed_int: Memory.Size,
        less_than_int: u16,
        unsigned_less_than_int: u16,
        signed_less_than_int: u16,
        exact_signed_int: u64,
        exact_unsigned_int: u64,

        fn accepts(constraint: Constraint, ty: Type, cg: *CodeGen) bool {
            return switch (constraint) {
                .any => true,
                .int => |size| if (cg.intInfo(ty)) |int_info| size.bitSize() >= int_info.bits else false,
                .float => |size| if (cg.floatBits(ty)) |float_bits| size.bitSize() == float_bits else false,
                .exact_int => |bit_size| if (cg.intInfo(ty)) |int_info| bit_size == int_info.bits else false,
                .unsigned_int => |size| if (cg.intInfo(ty)) |int_info| switch (int_info.signedness) {
                    .signed => false,
                    .unsigned => int_info.bits <= size.bitSize(),
                } else false,
                .signed_int => |size| if (cg.intInfo(ty)) |int_info| switch (int_info.signedness) {
                    .signed => int_info.bits <= size.bitSize(),
                    .unsigned => false,
                } else false,
                .less_than_int => |size| if (cg.intInfo(ty)) |int_info| int_info.bits < size else false,
                .unsigned_less_than_int => |size| if (cg.intInfo(ty)) |int_info| switch (int_info.signedness) {
                    .signed => false,
                    .unsigned => int_info.bits < size,
                } else false,
                .signed_less_than_int => |size| if (cg.intInfo(ty)) |int_info| switch (int_info.signedness) {
                    .signed => int_info.bits < size,
                    .unsigned => false,
                } else false,
                .exact_signed_int => |bit_size| if (cg.intInfo(ty)) |int_info| switch (int_info.signedness) {
                    .signed => bit_size == int_info.bits,
                    .unsigned => false,
                } else false,
                .exact_unsigned_int => |bit_size| if (cg.intInfo(ty)) |int_info| switch (int_info.signedness) {
                    .signed => false,
                    .unsigned => bit_size == int_info.bits,
                } else false,
            };
        }
    };

    const TempSpec = struct {
        type: Type = .noreturn,
        kind: Kind,

        const unused: TempSpec = .{ .kind = .unused };

        const Kind = union(enum) {
            unused,
            any,
            reg: Register,
            ref: Select.Operand.Ref,
            mut_rc: struct { ref: Select.Operand.Ref, rc: abi.RegisterClass },
            rc: abi.RegisterClass,
            lazy_symbol: struct { kind: link.File.LazySymbol.Kind, ref: Select.Operand.Ref = .none },

            fn lock(kind: Kind, cg: *CodeGen) ![2]?RegisterLock {
                var reg_locks: [2]?RegisterLock = @splat(null);
                const regs: [2]Register = switch (kind) {
                    else => return reg_locks,
                    .reg => |reg| if (reg.class() == .int and reg.id() == 0)
                        return reg_locks // no need to lock x0, since it can't be mutated
                    else
                        .{ reg, .none },
                };
                for (regs, &reg_locks) |reg, *reg_lock| {
                    if (reg == .none) continue;
                    const reg_index = RegisterManager.indexOfRegIntoTracked(reg) orelse continue;
                    try cg.register_manager.getRegIndex(reg_index, null);
                    reg_lock.* = cg.register_manager.lockRegIndex(reg_index);
                }
                return reg_locks;
            }
        };

        fn create(spec: TempSpec, s: *const Select) InnerError!struct { Temp, bool } {
            const cg = s.cg;
            const pt = cg.pt;
            return switch (spec.kind) {
                .unused => .{ undefined, false },
                .any => .{ try cg.tempAlloc(spec.type), true },
                .reg => |reg| .{ try cg.tempInit(spec.type, .{ .register = reg }), true },
                .ref => |ref| .{ ref.tempOf(s), false },
                .rc => |rc| .{ try cg.tempAllocReg(spec.type, regSetForRegClass(rc)), true },
                .mut_rc => |ref_rc| {
                    const temp = ref_rc.ref.tempOf(s);
                    if (temp.isMut(cg)) switch (temp.tracking(cg).short) {
                        .register => |reg| if (reg.class() == ref_rc.rc) return .{ temp, false },
                        .register_offset => |reg_off| if (reg_off.off == 0 and reg_off.reg.class() == ref_rc.rc) return .{ temp, false },
                        else => {},
                    };
                    return .{ try cg.tempAllocReg(spec.type, regSetForRegClass(ref_rc.rc)), true };
                },
                .lazy_symbol => |lazy_symbol_spec| {
                    const ty = if (lazy_symbol_spec.ref == .none) spec.type else lazy_symbol_spec.ref.typeOf(s);
                    const lazy_symbol: link.File.LazySymbol = .{
                        .kind = lazy_symbol_spec.kind,
                        .ty = ty.toIntern(),
                    };
                    const elf_file = cg.bin_file.cast(.elf).?;
                    const sym_index = elf_file.zigObjectPtr().?.getOrCreateMetadataForLazySymbol(elf_file, pt, lazy_symbol) catch |err|
                        return cg.fail("{s} creating lazy symbol", .{@errorName(err)});
                    return .{ try cg.tempInit(.usize, .{ .lea_symbol = .{ .sym_index = sym_index } }), true };
                },
            };
        }
    };

    const Operand = struct {
        flags: struct {
            tag: Tag,
            adjust: Adjust = .none,
            base: Ref.Sized = .none,
            unused: u3 = 0,
        },
        imm: i32 = 0,

        const Tag = enum(u3) {
            none,
            ref,
            zero,
            mem,
            simm,
            uimm,
        };

        const Adjust = packed struct(u10) {
            sign: enum(u1) { neg, pos },
            lhs: enum(u5) {
                none,
                bit_size,
                umax,
                not_mask,
            },
            op: enum(u2) { mul },
            rhs: Memory.Scale,

            const none: Adjust = .{ .sign = .pos, .lhs = .none, .op = .mul, .rhs = .@"1" };
            const bit_size: Adjust = .{ .sign = .pos, .lhs = .bit_size, .op = .mul, .rhs = .@"1" };
            const sub_bit_size: Adjust = .{ .sign = .neg, .lhs = .bit_size, .op = .mul, .rhs = .@"1" };
            const add_umax: Adjust = .{ .sign = .pos, .lhs = .umax, .op = .mul, .rhs = .@"1" };
            const add_not_mask: Adjust = .{ .sign = .pos, .lhs = .not_mask, .op = .mul, .rhs = .@"1" };
        };

        const Ref = enum(u5) {
            tmp0,
            tmp1,
            tmp2,
            tmp3,
            tmp4,
            tmp5,
            dst0,
            dst1,
            src0,
            src1,
            src2,
            none,

            const Sized = packed struct(u9) {
                ref: Ref,
                size: Memory.Size,

                const none: Sized = .{ .ref = .none, .size = .none };

                const tmp0: Sized = .{ .ref = .tmp0, .size = .none };
                const tmp0b: Sized = .{ .ref = .tmp0, .size = .byte };
                const tmp0h: Sized = .{ .ref = .tmp0, .size = .half };
                const tmp0w: Sized = .{ .ref = .tmp0, .size = .word };
                const tmp0d: Sized = .{ .ref = .tmp0, .size = .double };

                const tmp1: Sized = .{ .ref = .tmp1, .size = .none };
                const tmp1b: Sized = .{ .ref = .tmp1, .size = .byte };
                const tmp1h: Sized = .{ .ref = .tmp1, .size = .half };
                const tmp1w: Sized = .{ .ref = .tmp1, .size = .word };
                const tmp1d: Sized = .{ .ref = .tmp1, .size = .double };

                const tmp2: Sized = .{ .ref = .tmp2, .size = .none };
                const tmp2b: Sized = .{ .ref = .tmp2, .size = .byte };
                const tmp2h: Sized = .{ .ref = .tmp2, .size = .half };
                const tmp2w: Sized = .{ .ref = .tmp2, .size = .word };
                const tmp2d: Sized = .{ .ref = .tmp2, .size = .double };

                const tmp3: Sized = .{ .ref = .tmp3, .size = .none };
                const tmp3b: Sized = .{ .ref = .tmp3, .size = .byte };
                const tmp3h: Sized = .{ .ref = .tmp3, .size = .half };
                const tmp3w: Sized = .{ .ref = .tmp3, .size = .word };
                const tmp3d: Sized = .{ .ref = .tmp3, .size = .double };

                const tmp4: Sized = .{ .ref = .tmp4, .size = .none };
                const tmp4b: Sized = .{ .ref = .tmp4, .size = .byte };
                const tmp4h: Sized = .{ .ref = .tmp4, .size = .half };
                const tmp4w: Sized = .{ .ref = .tmp4, .size = .word };
                const tmp4d: Sized = .{ .ref = .tmp4, .size = .double };

                const tmp5: Sized = .{ .ref = .tmp5, .size = .none };
                const tmp5b: Sized = .{ .ref = .tmp5, .size = .byte };
                const tmp5h: Sized = .{ .ref = .tmp5, .size = .half };
                const tmp5w: Sized = .{ .ref = .tmp5, .size = .word };
                const tmp5d: Sized = .{ .ref = .tmp5, .size = .double };

                const dst0: Sized = .{ .ref = .dst0, .size = .none };
                const dst0b: Sized = .{ .ref = .dst0, .size = .byte };
                const dst0h: Sized = .{ .ref = .dst0, .size = .half };
                const dst0w: Sized = .{ .ref = .dst0, .size = .word };
                const dst0d: Sized = .{ .ref = .dst0, .size = .double };

                const dst1: Sized = .{ .ref = .dst1, .size = .none };
                const dst1b: Sized = .{ .ref = .dst1, .size = .byte };
                const dst1h: Sized = .{ .ref = .dst1, .size = .half };
                const dst1w: Sized = .{ .ref = .dst1, .size = .word };
                const dst1d: Sized = .{ .ref = .dst1, .size = .double };

                const src0: Sized = .{ .ref = .src0, .size = .none };
                const src0b: Sized = .{ .ref = .src0, .size = .byte };
                const src0h: Sized = .{ .ref = .src0, .size = .half };
                const src0w: Sized = .{ .ref = .src0, .size = .word };
                const src0d: Sized = .{ .ref = .src0, .size = .double };

                const src1: Sized = .{ .ref = .src1, .size = .none };
                const src1b: Sized = .{ .ref = .src1, .size = .byte };
                const src1h: Sized = .{ .ref = .src1, .size = .half };
                const src1w: Sized = .{ .ref = .src1, .size = .word };
                const src1d: Sized = .{ .ref = .src1, .size = .double };

                const src2: Sized = .{ .ref = .src2, .size = .none };
                const src2b: Sized = .{ .ref = .src2, .size = .byte };
                const src2h: Sized = .{ .ref = .src2, .size = .half };
                const src2w: Sized = .{ .ref = .src2, .size = .word };
                const src2d: Sized = .{ .ref = .src2, .size = .double };
            };

            fn typeOf(ref: Ref, s: *const Select) Type {
                return s.types[@intFromEnum(ref)];
            }

            fn valueOf(ref: Ref, s: *const Select) MCValue {
                return s.temps[@intFromEnum(ref)].tracking(s.cg).short;
            }

            fn tempOf(ref: Ref, s: *const Select) Temp {
                return s.temps[@intFromEnum(ref)];
            }
        };

        const @"_": Operand = .{ .flags = .{ .tag = .none } };
        const zero: Operand = .{ .flags = .{ .tag = .zero } };

        const tmp0b: Operand = .{ .flags = .{ .tag = .ref, .base = .tmp0b } };
        const tmp0h: Operand = .{ .flags = .{ .tag = .ref, .base = .tmp0h } };
        const tmp0w: Operand = .{ .flags = .{ .tag = .ref, .base = .tmp0w } };
        const tmp0d: Operand = .{ .flags = .{ .tag = .ref, .base = .tmp0d } };

        const tmp1b: Operand = .{ .flags = .{ .tag = .ref, .base = .tmp1b } };
        const tmp1h: Operand = .{ .flags = .{ .tag = .ref, .base = .tmp1h } };
        const tmp1w: Operand = .{ .flags = .{ .tag = .ref, .base = .tmp1w } };
        const tmp1d: Operand = .{ .flags = .{ .tag = .ref, .base = .tmp1d } };

        const dst0b: Operand = .{ .flags = .{ .tag = .ref, .base = .dst0b } };
        const dst0h: Operand = .{ .flags = .{ .tag = .ref, .base = .dst0h } };
        const dst0w: Operand = .{ .flags = .{ .tag = .ref, .base = .dst0w } };
        const dst0d: Operand = .{ .flags = .{ .tag = .ref, .base = .dst0d } };

        const dst1b: Operand = .{ .flags = .{ .tag = .ref, .base = .dst1b } };
        const dst1h: Operand = .{ .flags = .{ .tag = .ref, .base = .dst1h } };
        const dst1w: Operand = .{ .flags = .{ .tag = .ref, .base = .dst1w } };
        const dst1d: Operand = .{ .flags = .{ .tag = .ref, .base = .dst1d } };

        const src0b: Operand = .{ .flags = .{ .tag = .ref, .base = .src0b } };
        const src0h: Operand = .{ .flags = .{ .tag = .ref, .base = .src0h } };
        const src0w: Operand = .{ .flags = .{ .tag = .ref, .base = .src0w } };
        const src0d: Operand = .{ .flags = .{ .tag = .ref, .base = .src0d } };

        const src1b: Operand = .{ .flags = .{ .tag = .ref, .base = .src1b } };
        const src1h: Operand = .{ .flags = .{ .tag = .ref, .base = .src1h } };
        const src1w: Operand = .{ .flags = .{ .tag = .ref, .base = .src1w } };
        const src1d: Operand = .{ .flags = .{ .tag = .ref, .base = .src1d } };

        const src2b: Operand = .{ .flags = .{ .tag = .ref, .base = .src2b } };
        const src2h: Operand = .{ .flags = .{ .tag = .ref, .base = .src2h } };
        const src2w: Operand = .{ .flags = .{ .tag = .ref, .base = .src2w } };
        const src2d: Operand = .{ .flags = .{ .tag = .ref, .base = .src2d } };

        fn ui(imm: u32) Operand {
            return .{ .flags = .{ .tag = .uimm }, .imm = @bitCast(imm) };
        }
        fn ua(base: Ref.Sized, adjust: Adjust) Operand {
            return .{ .flags = .{ .tag = .uimm, .adjust = adjust, .base = base } };
        }
        fn uia(imm: u32, base: Ref.Sized, adjust: Adjust) Operand {
            return .{ .flags = .{ .tag = .uimm, .adjust = adjust, .base = base }, .imm = @bitCast(imm) };
        }

        fn si(imm: i32) Operand {
            return .{ .flags = .{ .tag = .simm }, .imm = @bitCast(imm) };
        }
        fn sa(base: Ref.Sized, adjust: Adjust) Operand {
            return .{ .flags = .{ .tag = .simm, .adjust = adjust, .base = base } };
        }
        fn sia(imm: i32, base: Ref.Sized, adjust: Adjust) Operand {
            return .{ .flags = .{ .tag = .simm, .adjust = adjust, .base = base }, .imm = @bitCast(imm) };
        }

        fn adjustedImm(op: Operand, comptime SignedImm: type, s: *const Select) SignedImm {
            const zcu = s.cg.pt.zcu;
            const ref = op.flags.base.ref;
            const UnsignedImm = @Type(.{
                .int = .{ .signedness = .unsigned, .bits = @typeInfo(SignedImm).int.bits },
            });
            const lhs: SignedImm = switch (op.flags.adjust.lhs) {
                .none => 0,
                .bit_size => @intCast(ref.typeOf(s).scalarType(zcu).bitSize(zcu)),
                .not_mask => @bitCast(@as(UnsignedImm, 1) << @intCast(ref.typeOf(s).scalarType(zcu).bitSize(zcu) - 12)),
                .umax => @bitCast(@as(UnsignedImm, std.math.maxInt(UnsignedImm)) >> @truncate(
                    -%op.flags.base.ref.typeOf(s).scalarType(s.cg.pt.zcu).bitSize(s.cg.pt.zcu),
                )),
            };
            const rhs = op.flags.adjust.rhs.toLog2();
            const op_res = op_res: switch (op.flags.adjust.op) {
                .mul => {
                    const op_res = @shlWithOverflow(lhs, rhs);
                    assert(op_res[1] == 0);
                    break :op_res op_res[0];
                },
            };
            return switch (op.flags.adjust.sign) {
                .neg => op.imm - op_res,
                .pos => op.imm + op_res,
            };
        }

        fn lower(op: Operand, s: *Select) InnerError!InstructionOperand {
            return switch (op.flags.tag) {
                .none => .none,
                .ref => switch (op.flags.base.ref.valueOf(s)) {
                    .lea_symbol => return s.cg.fail("TODO: operand lower lea_symbol", .{}),
                    .immediate => |imm| .{ .imm = switch (op.flags.base.size) {
                        .byte => if (std.math.cast(i8, @as(i64, @bitCast(imm)))) |simm| .s(simm) else .u(@as(u8, @intCast(imm))),
                        .half => if (std.math.cast(i16, @as(i64, @bitCast(imm)))) |simm| .s(simm) else .u(@as(u16, @intCast(imm))),
                        .word => if (std.math.cast(i32, @as(i64, @bitCast(imm)))) |simm| .s(simm) else .u(@as(u32, @intCast(imm))),
                        .double => if (std.math.cast(i32, @as(i64, @bitCast(imm)))) |simm| .s(simm) else .u(imm),
                        else => unreachable,
                    } },
                    .register => |reg| .{ .reg = s.lowerReg(reg) },
                    else => |mcv| .{ .mem = try mcv.mem(s.cg, .{ .size = op.flags.base.size }) },
                },
                .mem => .{ .mem = try op.flags.base.ref.valueOf(s).mem(s.cg, .{
                    .size = op.flags.base.size,
                    .disp = 0,
                    .unsigned = false,
                }) },
                .zero => .{ .reg = .zero },
                .simm => .{ .imm = .s(op.adjustedImm(i32, s)) },
                .uimm => .{ .imm = .u(@bitCast(op.adjustedImm(i64, s))) },
            };
        }
    };

    fn lowerReg(s: *Select, reg: Register) Register {
        _ = s;
        // TODO: will need some special handling of vector registers here, that will *not* be fun.
        return reg;
    }

    fn emitLabel(s: *Select, label_index: Label) void {
        assert(@intFromEnum(label_index) < @intFromEnum(Label._));
        const label = &s.labels[@intFromEnum(label_index)];
        for (&label.forward) |*reloc| {
            if (reloc.*) |r| s.cg.performReloc(r);
            reloc.* = null;
        }
        label.backward = @intCast(s.cg.mir_instructions.len);
    }

    fn emit(s: *Select, inst: Instruction) InnerError!void {
        const mir_tag = inst[1];
        switch (inst[0]) {
            .@"0:", .@"1:", .@"2:", .@"3:", .@"4:" => |label| s.emitLabel(label),
            ._ => {},
        }
        var mir_ops: [3]InstructionOperand = undefined;
        inline for (&mir_ops, 2..) |*mir_op, inst_index| mir_op.* = try inst[inst_index].lower(s);
        s.cg.asmOps(mir_tag, mir_ops) catch |err| switch (err) {
            error.InvalidInstruction => {
                return s.cg.fail(
                    "invalid instruction: '{s} {s} {s} {s}'",
                    .{
                        @tagName(mir_tag),
                        @tagName(mir_ops[0]),
                        @tagName(mir_ops[1]),
                        @tagName(mir_ops[2]),
                    },
                );
            },
            else => |e| return e,
        };
    }
};

fn select(
    cg: *CodeGen,
    dst_temps: []Temp,
    dst_tys: []const Type,
    src_temps: []Temp,
    cases: []const Select.Case,
) Select.Error!void {
    cases: for (cases) |case| {
        for (case.required_features) |required_feature| if (required_feature) |feature| if (!cg.hasFeature(feature)) continue :cases;
        for (case.src_constraints[0..src_temps.len], src_temps) |src_constraint, src_temp| if (!src_constraint.accepts(src_temp.typeOf(cg), cg)) continue :cases;
        for (case.dst_constraints[0..dst_temps.len], dst_tys) |dst_constraint, dst_ty| if (!dst_constraint.accepts(dst_ty, cg)) continue :cases;
        if (std.debug.runtime_safety) {
            for (case.src_constraints[src_temps.len..]) |src_constraint| assert(src_constraint == .any);
            for (case.dst_constraints[dst_temps.len..]) |dst_constraint| assert(dst_constraint == .any);
        }
        patterns: for (case.patterns) |pattern| {
            for (pattern.src[0..src_temps.len], src_temps) |src_pattern, src_temp| {
                const result = src_pattern.matches(src_temp, cg);
                selection_log.debug(
                    "matching {s} -> {}, result {}",
                    .{
                        @tagName(src_pattern),
                        src_temp.tracking(cg).short,
                        result,
                    },
                );
                if (!result) continue :patterns;
            }
            if (std.debug.runtime_safety) for (pattern.src[src_temps.len..]) |src_pattern| assert(src_pattern == .none);

            var s: Select = .{
                .cg = cg,
                .temps = undefined,
                .types = undefined,
                .labels = @splat(.{ .forward = @splat(null), .backward = null }),
            };
            const s_tmp_types = s.types[@intFromEnum(Select.Operand.Ref.tmp0)..@intFromEnum(Select.Operand.Ref.dst0)];
            const s_tmp_temps = s.temps[@intFromEnum(Select.Operand.Ref.tmp0)..@intFromEnum(Select.Operand.Ref.dst0)];
            const s_dst_types = s.types[@intFromEnum(Select.Operand.Ref.dst0)..@intFromEnum(Select.Operand.Ref.src0)];
            const s_dst_temps = s.temps[@intFromEnum(Select.Operand.Ref.dst0)..@intFromEnum(Select.Operand.Ref.src0)];
            const s_src_types = s.types[@intFromEnum(Select.Operand.Ref.src0)..@intFromEnum(Select.Operand.Ref.none)];
            const s_src_temps = s.temps[@intFromEnum(Select.Operand.Ref.src0)..@intFromEnum(Select.Operand.Ref.none)];

            for (s_tmp_types, case.extra_temps) |*ty, spec| ty.* = spec.type;
            @memcpy(s_dst_types[0..dst_tys.len], dst_tys);
            for (s_src_types[0..src_temps.len], src_temps) |*ty, temp| ty.* = temp.typeOf(cg);
            std.mem.swap(Type, &s_src_types[pattern.commute[0]], &s_src_types[pattern.commute[1]]);

            @memcpy(s_src_temps[0..src_temps.len], src_temps);
            std.mem.swap(Temp, &s_src_temps[pattern.commute[0]], &s_src_temps[pattern.commute[1]]);
            var dst_locks: [s_dst_temps.len][2]?RegisterLock = @splat(@splat(null));
            for (dst_locks[0..dst_temps.len], case.dst_temps[0..dst_temps.len]) |*dst_lock, dst_kind| dst_lock.* = try dst_kind.lock(cg);
            var tmp_locks: [s_tmp_temps.len][2]?RegisterLock = @splat(@splat(null));
            for (&tmp_locks, case.extra_temps) |*tmp_lock, tmp_spec| tmp_lock.* = try tmp_spec.kind.lock(cg);

            while (true) for (pattern.src[0..src_temps.len], src_temps) |src_pattern, *src_temp| {
                if (try src_pattern.convert(src_temp, cg)) break;
            } else break;
            @memcpy(s_src_temps[0..src_temps.len], src_temps);
            std.mem.swap(Temp, &s_src_temps[pattern.commute[0]], &s_src_temps[pattern.commute[1]]);

            var tmp_owned: [s_tmp_temps.len]bool = @splat(false);
            for (s_tmp_temps, &tmp_owned, case.extra_temps) |*temp, *owned, tmp_spec| temp.*, owned.* = try tmp_spec.create(&s);
            for (dst_temps, dst_tys, case.dst_temps[0..dst_temps.len]) |*dst_temp, dst_ty, tmp_kind| dst_temp.*, _ = try Select.TempSpec.create(.{ .type = dst_ty, .kind = tmp_kind }, &s);
            @memcpy(s_dst_temps[0..dst_temps.len], dst_temps);

            switch (case.each) {
                .once => |body| {
                    for (body) |inst| try s.emit(inst);
                    s.emitLabel(.@"0:");
                },
            }

            for (tmp_locks) |locks| for (locks) |lock| if (lock) |reg| cg.register_manager.unlockReg(reg);
            for (dst_locks) |locks| for (locks) |lock| if (lock) |reg| cg.register_manager.unlockReg(reg);
            for (tmp_owned, s_tmp_temps) |owned, temp| if (owned) try temp.die(cg);
            return;
        }
    }

    return error.SelectFailed;
}
