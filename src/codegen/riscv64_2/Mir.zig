prologue: []const Instruction,
body: []const Instruction,
epilogue: []const Instruction,

literals: []const u32,
nav_relocs: []const Reloc.Nav,
uav_relocs: []const Reloc.Uav,
global_relocs: []const Reloc.Global,

pub const Reloc = struct {
    label: u32,
    addend: u64 align(@alignOf(u32)) = 0,

    pub const Nav = struct {
        nav: InternPool.Nav.Index,
        reloc: Reloc,
    };

    pub const Uav = struct {
        uav: InternPool.Key.Ptr.BaseAddr.Uav,
        reloc: Reloc,
    };

    pub const Global = struct {
        name: [*:0]const u8,
        reloc: Reloc,
    };

    pub const Literal = struct {
        label: u32,
    };
};

pub fn deinit(mir: *Mir, gpa: std.mem.Allocator) void {
    assert(mir.body.ptr + mir.body.len == mir.prologue.ptr);
    assert(mir.prologue.ptr + mir.prologue.len == mir.epilogue.ptr);
    gpa.free(mir.body.ptr[0 .. mir.body.len + mir.prologue.len + mir.epilogue.len]);
    gpa.free(mir.literals);
    gpa.free(mir.nav_relocs);
    gpa.free(mir.uav_relocs);
    gpa.free(mir.global_relocs);
    mir.* = undefined;
}

pub fn emit(
    mir: Mir,
    lf: *link.File,
    pt: Zcu.PerThread,
    src_loc: Zcu.LazySrcLoc,
    func_index: InternPool.Index,
    atom_index: u32,
    w: *std.Io.Writer,
    debug_output: link.File.DebugInfoOutput,
) !void {
    const zcu = pt.zcu;
    const ip = &zcu.intern_pool;
    const func = zcu.funcInfo(func_index);
    const nav = ip.getNav(func.owner_nav);
    const mod = zcu.navFileScope(func.owner_nav).mod.?;
    const target = &mod.resolved_target.result;
    mir_log.debug("{f}:", .{nav.fqn.fmt(ip)});

    _ = debug_output;

    const func_align = switch (nav.status.fully_resolved.alignment) {
        .none => switch (mod.optimize_mode) {
            .Debug, .ReleaseSafe, .ReleaseFast => target_util.defaultFunctionAlignment(target),
            .ReleaseSmall => target_util.minFunctionAlignment(target),
        },
        else => |a| a.maxStrict(target_util.minFunctionAlignment(target)),
    };

    const code_len = mir.prologue.len + mir.body.len + mir.epilogue.len;
    const literals_align_gap = -%code_len & (@divExact(
        @as(u5, @intCast(func_align.minStrict(.@"16").toByteUnits().?)),
        Instruction.minSize(target),
    ) - 1);
    try w.rebase(w.end, Instruction.max_size * (code_len + literals_align_gap + mir.literals.len));
    emitInstructionsForward(w, mir.prologue) catch unreachable;
    emitInstructionsBackward(w, mir.body) catch unreachable;
    const body_end: u32 = @intCast(w.end);
    emitInstructionsBackward(w, mir.epilogue) catch unreachable;
    w.splatByteAll(0, Instruction.max_size * literals_align_gap) catch unreachable;
    w.writeAll(@ptrCast(mir.literals)) catch unreachable;

    mir.emitRelocations(
        lf,
        pt,
        atom_index,
        src_loc,
        func_index,
        body_end,
    ) catch |err| return zcu.codegenFail(func.owner_nav, "emit MIR failed: {s}", .{@errorName(err)});
}

fn emitRelocations(
    mir: *const Mir,
    lf: *link.File,
    pt: Zcu.PerThread,
    atom_index: u32,
    src_loc: Zcu.LazySrcLoc,
    func_index: InternPool.Index,
    body_end: u32,
) !void {
    const zcu = pt.zcu;
    const ip = &zcu.intern_pool;
    const func = zcu.funcInfo(func_index);
    const mod = zcu.navFileScope(func.owner_nav).mod.?;

    for (mir.nav_relocs) |nav_reloc| try emitReloc(
        lf,
        zcu,
        atom_index,
        switch (try @import("../../codegen.zig").genNavRef(
            lf,
            pt,
            src_loc,
            nav_reloc.nav,
            &mod.resolved_target.result,
        )) {
            .sym_index => |sym_index| sym_index,
            .fail => |em| return zcu.codegenFailMsg(func.owner_nav, em),
        },
        mir.body[nav_reloc.reloc.label],
        body_end - Instruction.max_size * (1 + nav_reloc.reloc.label),
        nav_reloc.reloc.addend,
        if (ip.getNav(nav_reloc.nav).getExtern(ip)) |_| .got_load else .direct,
    );
    for (mir.uav_relocs) |uav_reloc| try emitReloc(
        lf,
        zcu,
        atom_index,
        switch (try lf.lowerUav(
            pt,
            uav_reloc.uav.val,
            ZigType.fromInterned(uav_reloc.uav.orig_ty).ptrAlignment(zcu),
            src_loc,
        )) {
            .sym_index => |sym_index| sym_index,
            .fail => |em| return zcu.codegenFailMsg(func.owner_nav, em),
        },
        mir.body[uav_reloc.reloc.label],
        body_end - Instruction.max_size * (1 + uav_reloc.reloc.label),
        uav_reloc.reloc.addend,
        .direct,
    );
    for (mir.global_relocs) |global_reloc| try emitReloc(
        lf,
        zcu,
        atom_index,
        if (lf.cast(.elf)) |ef|
            try ef.getGlobalSymbol(std.mem.span(global_reloc.name), null)
        else if (lf.cast(.elf2)) |mf|
            @intFromEnum(try mf.globalSymbol(.{
                .name = std.mem.span(global_reloc.name),
                .lib_name = "compiler_rt", // TODO: don't hard code, but maybe don't touch bin_file?
                .type = .FUNC,
            }))
        else
            unreachable,
        mir.body[global_reloc.reloc.label],
        body_end - Instruction.max_size * (1 + global_reloc.reloc.label),
        global_reloc.reloc.addend,
        .direct,
    );
}

fn emitInstructionsForward(w: *std.Io.Writer, instructions: []const Instruction) !void {
    for (instructions) |instruction| try emitInstruction(w, instruction);
}
fn emitInstructionsBackward(w: *std.Io.Writer, instructions: []const Instruction) !void {
    var instruction_index = instructions.len;
    while (instruction_index > 0) {
        instruction_index -= 1;
        try emitInstruction(w, instructions[instruction_index]);
    }
}
fn emitInstruction(w: *std.Io.Writer, instruction: Instruction) !void {
    mir_log.debug("    {f}", .{instruction});
    instruction.write(try w.writableArray(Instruction.max_size));
}

fn emitReloc(
    lf: *link.File,
    zcu: *Zcu,
    atom_index: u32,
    sym_index: u32,
    instruction: Instruction,
    offset: u32,
    addend: u64,
    kind: enum { direct, got_load },
) !void {
    _ = addend;
    _ = kind;

    const gpa = zcu.gpa;

    switch (instruction.r.opcode) {
        else => unreachable,
        // load_symbol_reloc
        .LUI => if (lf.cast(.elf2)) |elf| {
            try elf.addReloc(
                @enumFromInt(atom_index),
                offset,
                @enumFromInt(sym_index),
                0,
                .{ .RISCV = .HI20 },
            );

            try elf.addReloc(
                @enumFromInt(atom_index),
                offset + 4,
                @enumFromInt(sym_index),
                0,
                .{ .RISCV = .LO12_I },
            );
        } else if (lf.cast(.elf)) |ef| {
            const zo = ef.zigObjectPtr().?;
            const atom = zo.symbol(atom_index).atom(ef).?;

            try atom.addReloc(gpa, .{
                .r_offset = offset,
                .r_info = (@as(u64, @intCast(sym_index)) << 32) | @intFromEnum(std.elf.R_RISCV.HI20),
                .r_addend = 0,
            }, zo);

            try atom.addReloc(gpa, .{
                .r_offset = offset + 4,
                .r_info = (@as(u64, @intCast(sym_index)) << 32) | @intFromEnum(std.elf.R_RISCV.LO12_I),
                .r_addend = 0,
            }, zo);
        } else unreachable,
        .AUIPC => if (lf.cast(.elf)) |ef| {
            const zo = ef.zigObjectPtr().?;
            const atom = zo.symbol(atom_index).atom(ef).?;

            try atom.addReloc(gpa, .{
                .r_offset = offset,
                .r_info = (@as(u64, @intCast(sym_index)) << 32) | @intFromEnum(std.elf.R_RISCV.CALL_PLT),
                .r_addend = 0,
            }, zo);
        } else if (lf.cast(.elf2)) |elf| {
            try elf.addReloc(
                @enumFromInt(atom_index),
                offset,
                @enumFromInt(sym_index),
                0,
                .{ .RISCV = .CALL_PLT },
            );
        } else unreachable,
    }
}

const Mir = @This();
const std = @import("std");
const assert = std.debug.assert;

const link = @import("../../link.zig");
const Zcu = @import("../../Zcu.zig");
const InternPool = @import("../../InternPool.zig");
const target_util = @import("../../target.zig");
const ZigType = @import("../../Type.zig");

const Instruction = @import("encoding.zig").Instruction;

const mir_log = std.log.scoped(.mir);
