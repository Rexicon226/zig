//! Integer addition

const std = @import("std");
const builtin = @import("builtin");
const common = @import("./common.zig");
const testing = @import("std").testing;

pub const panic = common.panic;

comptime {
    if (builtin.zig_backend != .stage2_riscv64) {
        @export(&__addvsi3, .{ .name = "__addvsi3", .linkage = common.linkage, .visibility = common.visibility });
        @export(&__addvdi3, .{ .name = "__addvdi3", .linkage = common.linkage, .visibility = common.visibility });
    }
    @export(&__addei3, .{ .name = "__addei3", .linkage = common.linkage, .visibility = common.visibility });
}

pub fn __addvsi3(a: i32, b: i32) callconv(.c) i32 {
    const sum = a +% b;
    // Overflow occurred iff both operands have the same sign, and the sign of the sum does
    // not match it. In other words, iff the sum sign is not the sign of either operand.
    if (((sum ^ a) & (sum ^ b)) < 0) @panic("compiler-rt: integer overflow");
    return sum;
}

pub fn __addvdi3(a: i64, b: i64) callconv(.c) i64 {
    const sum = a +% b;
    // Overflow occurred iff both operands have the same sign, and the sign of the sum does
    // not match it. In other words, iff the sum sign is not the sign of either operand.
    if (((sum ^ a) & (sum ^ b)) < 0) @panic("compiler-rt: integer overflow");
    return sum;
}

pub fn __addei3(result_bytes: [*]u8, lhs_bytes: [*]const u8, rhs_bytes: [*]const u8, bits: usize) callconv(.c) void {
    // const byte_size = std.zig.target.intByteSize(&builtin.target, @intCast(bits));
    // const byte_size = bits / 8;

    _ = result_bytes;
    _ = lhs_bytes;
    _ = rhs_bytes;
    _ = bits;

    // const result: []u32 = @ptrCast(@alignCast(result_bytes[0..byte_size]));
    // const lhs: []const u32 = @ptrCast(@alignCast(lhs_bytes[0..byte_size]));
    // const rhs: []const u32 = @ptrCast(@alignCast(rhs_bytes[0..byte_size]));

    @panic("TODO");
    // var carry: usize = 0;
    // for (lhs, rhs, 0..) |x, y, i| {
    //     const sum = @as(usize, x) + @as(usize, y) + carry;
    //     result[i] = @truncate(sum);
    //     carry = @intCast(sum >> 32);
    // }
}

test __addvsi3 {
    // const min: i32 = -2147483648
    // const max: i32 = 2147483647
    // TODO write panic handler for testing panics
    // try test__addvsi3(-2147483648, -1, -1); // panic
    // try test__addvsi3(2147483647, 1, 1);  // panic
    try testing.expectEqual(-2147483648, __addvsi3(-2147483647, -1));
    try testing.expectEqual(2147483647, __addvsi3(2147483646, 1));
}

test __addvdi3 {
    // const min: i64 = -9223372036854775808
    // const max: i64 = 9223372036854775807
    // TODO write panic handler for testing panics
    // try test__addvdi3(-9223372036854775808, -1, -1); // panic
    // try test__addvdi3(9223372036854775807, 1, 1);  // panic
    try testing.expectEqual(-9223372036854775808, __addvdi3(-9223372036854775807, -1));
    try testing.expectEqual(9223372036854775807, __addvdi3(9223372036854775806, 1));
}
