case: Case = .lower,
mnemonic_operands_separator: []const u8 = " ",
operands_separator: []const u8 = ", ",
enable_aliases: bool = true,

pub const Case = enum {
    lower,
    upper,
    pub fn convert(case: Case, c: u8) u8 {
        return switch (case) {
            .lower => std.ascii.toLower(c),
            .upper => std.ascii.toUpper(c),
        };
    }
};

pub fn printInstruction(dis: Disassemble, inst: Instruction, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    _ = dis;

    // check constant instructions first, they're all declared inside of the Instruction namespace
    inline for (@typeInfo(Instruction).@"union".decls) |decl| {
        const info = @typeInfo(@TypeOf(@field(Instruction, decl.name)));
        if (info == .@"union" and info.@"union".layout == .@"packed") {
            if (@as(u32, @bitCast(inst)) == @as(u32, @bitCast(@field(Instruction, decl.name)))) {
                try writer.writeAll(decl.name);
                return;
            }
        }
    }

    // TODO: can be generalized across "which specific field" we need to check probably
    const decoded = inst.decode();
    switch (decoded) {
        .r => |r| switch (inst.r.opcode) {
            else => @panic("TODO"),
            .OP_64 => @panic("TODO: rv128i"),
            .OP, .OP_32 => {
                inline for (@typeInfo(Instruction.R).@"struct".decls) |decl| {
                    const field = @field(Instruction.R, decl.name);
                    const key = @as(u32, field.funct3) + field.funct7;
                    if (@as(u32, r.funct3) + r.funct7 == key) break try printDecoded(writer, decl.name, decoded);
                } else std.debug.panic("failed to find encoding for: 0b{b}", .{@as(u32, @bitCast(inst))});
            },
        },
        .i => |i| switch (inst.i.opcode) {
            .OP_IMM_64 => @panic("TODO: rv128i"),
            .JALR => try printDecoded(writer, "jalr", decoded),
            .LOAD, .OP_IMM, .OP_IMM_32 => {
                inline for (@typeInfo(Instruction.I).@"struct".decls) |decl| {
                    const entry = @field(Instruction.I, decl.name);
                    if (inst.i.opcode == entry.opcode)
                        if (i.funct3 == entry.funct3)
                            break try printDecoded(writer, decl.name, decoded);
                } else std.debug.panic("failed to find encoding for: 0b{b}", .{@as(u32, @bitCast(inst))});
            },
            else => @panic("TODO"),
        },
        .u => switch (inst.u.opcode) {
            .LUI => try printDecoded(writer, "lui", decoded),
            .AUIPC => try printDecoded(writer, "auipc", decoded),
            else => unreachable,
        },
        .j => {
            std.debug.assert(inst.j.opcode == .JAL);
            try printDecoded(writer, "jal", decoded);
        },
        inline .b, .s => |s, tag| {
            const T = switch (tag) {
                .s => Instruction.S,
                .b => Instruction.B,
                else => unreachable,
            };
            inline for (@typeInfo(T).@"struct".decls) |decl| {
                const entry = @field(T, decl.name);
                if (s.funct3 == entry.funct3)
                    break try printDecoded(writer, decl.name, decoded);
            } else std.debug.panic("failed to find encoding for: 0b{b}", .{@as(u32, @bitCast(inst))});
        },
    }
}

fn printDecoded(writer: *std.Io.Writer, name: []const u8, inst: Instruction.Decoded) std.Io.Writer.Error!void {
    try writer.print("{s} ", .{name});
    switch (inst) {
        .r => |r| try writer.print("{t}, {t}, {t}", .{ r.rd, r.rs1, r.rs2 }),
        .i => |i| switch (i.opcode) {
            else => try writer.print("{t}, {t}, {d}", .{ i.rd, i.rs1, @as(i12, @bitCast(i.imm12)) }),
            .LOAD => try writer.print("{t}, {d}({t})", .{ i.rd, @as(i12, @bitCast(i.imm12)), i.rs1 }),
        },
        .s => |s| {
            std.debug.assert(s.opcode == .STORE);
            const imm: u12 = (@as(u12, s.imm7) << 5) | s.imm5;
            try writer.print("{t}, {d}({t})", .{ s.rs2, @as(i12, @bitCast(imm)), s.rs1 });
        },
        .u => |u| try writer.print("{t}, {d}", .{ u.rd, u.imm20 }),
        .b => |_| try writer.print("todo b", .{}),
        .j => |_| try writer.print("todo j", .{}),
    }
}

const Disassemble = @This();
const std = @import("std");

const encoding = @import("encoding.zig");
const Instruction = encoding.Instruction;
