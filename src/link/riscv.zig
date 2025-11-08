pub fn writeSetSub6(comptime op: enum { set, sub }, code: *[1]u8, addend: anytype) void {
    const mask: u8 = 0b11_000000;
    const actual: i8 = @truncate(addend);
    var value: u8 = mem.readInt(u8, code, .little);
    switch (op) {
        .set => value = (value & mask) | @as(u8, @bitCast(actual & ~mask)),
        .sub => value = (value & mask) | (@as(u8, @bitCast(@as(i8, @bitCast(value)) -| actual)) & ~mask),
    }
    mem.writeInt(u8, code, value, .little);
}

pub fn writeSubUleb(code: []u8, addend: i64) void {
    var reader: std.Io.Reader = .fixed(code);
    const value = reader.takeLeb128(u64) catch unreachable;
    overwriteUleb(code, value -% @as(u64, @intCast(addend)));
}

pub fn writeSetUleb(code: []u8, addend: i64) void {
    overwriteUleb(code, @intCast(addend));
}

fn overwriteUleb(code: []u8, addend: u64) void {
    var value: u64 = addend;
    var i: usize = 0;

    while (true) {
        const byte = code[i];
        if (byte & 0x80 == 0) break;
        code[i] = 0x80 | @as(u8, @truncate(value & 0x7f));
        i += 1;
        value >>= 7;
    }
    code[i] = @truncate(value & 0x7f);
}

pub fn writeAddend(
    comptime Int: type,
    comptime op: enum { add, sub },
    code: *[@typeInfo(Int).int.bits / 8]u8,
    value: anytype,
) void {
    var V: Int = mem.readInt(Int, code, .little);
    const addend: Int = @truncate(value);
    switch (op) {
        .add => V +|= addend, // TODO: I think saturating arithmetic is correct here
        .sub => V -|= addend,
    }
    mem.writeInt(Int, code, V, .little);
}

pub fn writeInstU(code: *[4]u8, value: u32) void {
    var data: Instruction = .{ .u = mem.bytesToValue(@FieldType(Instruction, "u"), code) };
    const compensated: u32 = @bitCast(@as(i32, @bitCast(value)) + 0x800);
    data.u.imm20 = bitSlice(compensated, 31, 12);
    data.write(code);
}

pub fn writeInstI(code: *[4]u8, value: u32) void {
    var data: Instruction = .{ .i = mem.bytesToValue(@FieldType(Instruction, "i"), code) };
    data.i.imm12 = bitSlice(value, 11, 0);
    data.write(code);
}

pub fn writeInstS(code: *[4]u8, value: u32) void {
    var data: Instruction = .{ .s = mem.bytesToValue(@FieldType(Instruction, "s"), code) };
    data.s.imm5 = bitSlice(value, 4, 0);
    data.s.imm7 = bitSlice(value, 11, 5);
    data.write(code);
}

pub fn writeInstJ(code: *[4]u8, value: u32) void {
    var data: Instruction = .{ .j = mem.bytesToValue(@FieldType(Instruction, "j"), code) };
    data.J.imm1_10 = bitSlice(value, 10, 1);
    data.J.imm11 = bitSlice(value, 11, 11);
    data.J.imm12_19 = bitSlice(value, 19, 12);
    data.J.imm20 = bitSlice(value, 20, 20);
    data.write(code);
}

pub fn writeInstB(code: *[4]u8, value: u32) void {
    var data: Instruction = .{ .b = mem.bytesToValue(@FieldType(Instruction, "b"), code) };
    data.B.imm1_4 = bitSlice(value, 4, 1);
    data.B.imm5_10 = bitSlice(value, 10, 5);
    data.B.imm11 = bitSlice(value, 11, 11);
    data.B.imm12 = bitSlice(value, 12, 12);
    data.write(code);
}

fn bitSlice(
    value: anytype,
    comptime high: comptime_int,
    comptime low: comptime_int,
) std.math.IntFittingRange(0, 1 << high - low) {
    return @truncate((value >> low) & (1 << (high - low + 1)) - 1);
}

pub const Eflags = packed struct(u32) {
    rvc: bool,
    fabi: FloatAbi,
    rve: bool,
    tso: bool,
    _reserved: u19 = 0,
    _unused: u8 = 0,

    pub const FloatAbi = enum(u2) {
        soft = 0b00,
        single = 0b01,
        double = 0b10,
        quad = 0b11,
    };
};

const mem = std.mem;
const std = @import("std");

const encoding = @import("../codegen/riscv64_2/encoding.zig");
const Instruction = encoding.Instruction;
