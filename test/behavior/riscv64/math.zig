const builtin = @import("builtin");
const inf = math.inf;
const math = std.math;
const fmax = math.floatMax;
const fmin = math.floatMin;
const imax = math.maxInt;
const imin = math.minInt;
const nan = math.nan;
const next = math.nextAfter;
const std = @import("std");
const tmin = math.floatTrueMin;

const Gpr = u64;
// nothing supports more than 64 bits right now
const max_bits = 64;

fn Scalar(comptime Type: type) type {
    return switch (@typeInfo(Type)) {
        else => Type,
        .vector => |info| info.child,
    };
}

fn AddOneBit(comptime Type: type) type {
    const ResultScalar = switch (@typeInfo(Scalar(Type))) {
        .int => |int| @Type(.{ .int = .{ .signedness = int.signedness, .bits = @min(1 + int.bits, max_bits) } }),
        .float => Scalar(Type),
        else => @compileError(@typeName(Type)),
    };
    return switch (@typeInfo(Type)) {
        else => ResultScalar,
        .vector => |vector| @Vector(vector.len, ResultScalar),
    };
}

// inline to avoid a runtime `@splat`
inline fn splat(comptime Type: type, scalar: Scalar(Type)) Type {
    return switch (@typeInfo(Type)) {
        else => scalar,
        .vector => @splat(scalar),
    };
}
// inline to avoid a runtime `@select`
inline fn select(cond: anytype, lhs: anytype, rhs: @TypeOf(lhs)) @TypeOf(lhs) {
    return switch (@typeInfo(@TypeOf(cond))) {
        .bool => if (cond) lhs else rhs,
        .vector => @select(Scalar(@TypeOf(lhs)), cond, lhs, rhs),
        else => @compileError(@typeName(@TypeOf(cond))),
    };
}
fn sign(rhs: anytype) switch (@typeInfo(@TypeOf(rhs))) {
    else => bool,
    .vector => |vector| @Vector(vector.len, bool),
} {
    const ScalarInt = @Type(.{ .int = .{
        .signedness = .unsigned,
        .bits = @bitSizeOf(Scalar(@TypeOf(rhs))),
    } });
    const VectorInt = switch (@typeInfo(@TypeOf(rhs))) {
        else => ScalarInt,
        .vector => |vector| @Vector(vector.len, ScalarInt),
    };
    return @as(VectorInt, @bitCast(rhs)) & splat(VectorInt, @as(ScalarInt, 1) << @bitSizeOf(ScalarInt) - 1) != splat(VectorInt, 0);
}
fn boolAnd(lhs: anytype, rhs: @TypeOf(lhs)) @TypeOf(lhs) {
    switch (@typeInfo(@TypeOf(lhs))) {
        .bool => return lhs and rhs,
        .vector => |vector| switch (vector.child) {
            bool => {
                const Bits = @Type(.{ .int = .{ .signedness = .unsigned, .bits = vector.len } });
                const lhs_bits: Bits = @bitCast(lhs);
                const rhs_bits: Bits = @bitCast(rhs);
                return @bitCast(lhs_bits & rhs_bits);
            },
            else => {},
        },
        else => {},
    }
    @compileError("unsupported boolAnd type: " ++ @typeName(@TypeOf(lhs)));
}
fn boolOr(lhs: anytype, rhs: @TypeOf(lhs)) @TypeOf(lhs) {
    switch (@typeInfo(@TypeOf(lhs))) {
        .bool => return lhs or rhs,
        .vector => |vector| switch (vector.child) {
            bool => {
                const Bits = @Type(.{ .int = .{ .signedness = .unsigned, .bits = vector.len } });
                const lhs_bits: Bits = @bitCast(lhs);
                const rhs_bits: Bits = @bitCast(rhs);
                return @bitCast(lhs_bits | rhs_bits);
            },
            else => {},
        },
        else => {},
    }
    @compileError("unsupported boolOr type: " ++ @typeName(@TypeOf(lhs)));
}

// noinline for a more helpful stack trace
noinline fn checkExpected(expected: anytype, actual: @TypeOf(expected), comptime compare: Compare) !void {
    const Expected = @TypeOf(expected);
    const unexpected = unexpected: switch (@typeInfo(Scalar(Expected))) {
        else => expected != actual,
        .float => switch (compare) {
            .strict, .relaxed => {
                const unequal = boolAnd(expected != actual, boolOr(expected == expected, actual == actual));
                break :unexpected switch (compare) {
                    .strict => boolOr(unequal, sign(expected) != sign(actual)),
                    .relaxed => unequal,
                    .approx, .approx_int => comptime unreachable,
                };
            },
            .approx, .approx_int => {
                const epsilon = math.floatEps(Scalar(Expected));
                const tolerance = @sqrt(epsilon);
                break :unexpected @abs(expected - actual) > @max(
                    @abs(expected) * splat(Expected, tolerance),
                    splat(Expected, switch (compare) {
                        .strict, .relaxed => comptime unreachable,
                        .approx => tolerance,
                        .approx_int => 1,
                    }),
                );
            },
        },
    };
    if (switch (@typeInfo(Expected)) {
        else => unexpected,
        .vector => @reduce(.Or, unexpected),
    }) return error.Unexpected;
}

const Compare = enum { strict, relaxed, approx, approx_int };
fn unary(comptime op: anytype, comptime opts: struct {
    libc_name: ?[]const u8 = null,
    compare: Compare = .relaxed,
}) type {
    return struct {
        // noinline so that `mem_arg` is on the stack
        noinline fn testArgKinds(
            comptime Type: type,
            comptime imm_arg: Type,
            mem_arg: Type,
        ) !void {
            const expected = expected: {
                if (opts.libc_name) |libc_name| libc: {
                    const libc_func = @extern(*const fn (Scalar(Type)) callconv(.c) Scalar(Type), .{
                        .name = switch (Scalar(Type)) {
                            f16 => "__" ++ libc_name ++ "h",
                            f32 => libc_name ++ "f",
                            f64 => libc_name,
                            f80 => "__" ++ libc_name ++ "x",
                            f128 => libc_name ++ "q",
                            else => break :libc,
                        },
                    });
                    switch (@typeInfo(Type)) {
                        else => break :expected libc_func(imm_arg),
                        .vector => |vector| {
                            var res: Type = undefined;
                            inline for (0..vector.len) |i| res[i] = libc_func(imm_arg[i]);
                            break :expected res;
                        },
                    }
                }
                break :expected comptime op(Type, imm_arg);
            };
            var reg_arg = mem_arg;
            _ = .{&reg_arg};
            try checkExpected(expected, op(Type, reg_arg), opts.compare);
            try checkExpected(expected, op(Type, mem_arg), opts.compare);
            if (opts.libc_name == null) try checkExpected(expected, op(Type, imm_arg), opts.compare);
        }
        // noinline for a more helpful stack trace
        noinline fn testArgs(comptime Type: type, comptime imm_arg: Type) !void {
            try testArgKinds(
                Type,
                imm_arg,
                imm_arg,
            );
        }

        fn testInts() !void {
            try testArgs(i1, -1);
            try testArgs(i1, 0);
            try testArgs(u1, 0);
            try testArgs(u1, 1 << 0);

            try testArgs(i2, -1 << 1);
            try testArgs(i2, -1);
            try testArgs(i2, 0);
            try testArgs(u2, 0);
            try testArgs(u2, 1 << 0);
            try testArgs(u2, 1 << 1);

            try testArgs(i3, -1 << 2);
            try testArgs(i3, -1);
            try testArgs(i3, 0);
            try testArgs(u3, 0);
            try testArgs(u3, 1 << 0);
            try testArgs(u3, 1 << 1);
            try testArgs(u3, 1 << 2);

            try testArgs(i4, -1 << 3);
            try testArgs(i4, -1);
            try testArgs(i4, 0);
            try testArgs(u4, 0);
            try testArgs(u4, 1 << 0);
            try testArgs(u4, 1 << 1);
            try testArgs(u4, 1 << 2);
            try testArgs(u4, 1 << 3);

            try testArgs(i5, -1 << 4);
            try testArgs(i5, -1);
            try testArgs(i5, 0);
            try testArgs(u5, 0);
            try testArgs(u5, 1 << 0);
            try testArgs(u5, 1 << 1);
            try testArgs(u5, 1 << 3);
            try testArgs(u5, 1 << 4);

            try testArgs(i7, -1 << 6);
            try testArgs(i7, -1);
            try testArgs(i7, 0);
            try testArgs(u7, 0);
            try testArgs(u7, 1 << 0);
            try testArgs(u7, 1 << 1);
            try testArgs(u7, 1 << 5);
            try testArgs(u7, 1 << 6);

            try testArgs(i8, -1 << 7);
            try testArgs(i8, -1);
            try testArgs(i8, 0);
            try testArgs(u8, 0);
            try testArgs(u8, 1 << 0);
            try testArgs(u8, 1 << 1);
            try testArgs(u8, 1 << 6);
            try testArgs(u8, 1 << 7);

            try testArgs(i9, -1 << 8);
            try testArgs(i9, -1);
            try testArgs(i9, 0);
            try testArgs(u9, 0);
            try testArgs(u9, 1 << 0);
            try testArgs(u9, 1 << 1);
            try testArgs(u9, 1 << 7);
            try testArgs(u9, 1 << 8);

            try testArgs(i11, -1 << 8);
            try testArgs(i11, -1);
            try testArgs(i11, 0);
            try testArgs(u11, 0);
            try testArgs(u11, 1 << 0);
            try testArgs(u11, 1 << 1);
            try testArgs(u11, 1 << 7);
            try testArgs(u11, 1 << 8);

            try testArgs(i15, -1 << 14);
            try testArgs(i15, -1);
            try testArgs(i15, 0);
            try testArgs(u15, 0);
            try testArgs(u15, 1 << 0);
            try testArgs(u15, 1 << 1);
            try testArgs(u15, 1 << 13);
            try testArgs(u15, 1 << 14);

            try testArgs(i16, -1 << 15);
            try testArgs(i16, -1);
            try testArgs(i16, 0);
            try testArgs(u16, 0);
            try testArgs(u16, 1 << 0);
            try testArgs(u16, 1 << 1);
            try testArgs(u16, 1 << 14);
            try testArgs(u16, 1 << 15);

            try testArgs(i17, -1 << 16);
            try testArgs(i17, -1);
            try testArgs(i17, 0);
            try testArgs(u17, 0);
            try testArgs(u17, 1 << 0);
            try testArgs(u17, 1 << 1);
            try testArgs(u17, 1 << 15);
            try testArgs(u17, 1 << 16);

            try testArgs(i31, -1 << 30);
            try testArgs(i31, -1);
            try testArgs(i31, 0);
            try testArgs(u31, 0);
            try testArgs(u31, 1 << 0);
            try testArgs(u31, 1 << 1);
            try testArgs(u31, 1 << 29);
            try testArgs(u31, 1 << 30);

            try testArgs(i32, -1 << 31);
            try testArgs(i32, -1);
            try testArgs(i32, 0);
            try testArgs(u32, 0);
            try testArgs(u32, 1 << 0);
            try testArgs(u32, 1 << 1);
            try testArgs(u32, 1 << 30);
            try testArgs(u32, 1 << 31);

            try testArgs(i33, -1 << 32);
            try testArgs(i33, -1);
            try testArgs(i33, 0);
            try testArgs(u33, 0);
            try testArgs(u33, 1 << 0);
            try testArgs(u33, 1 << 1);
            try testArgs(u33, 1 << 31);
            try testArgs(u33, 1 << 32);

            try testArgs(i63, -1 << 62);
            try testArgs(i63, -1);
            try testArgs(i63, 0);
            try testArgs(u63, 0);
            try testArgs(u63, 1 << 0);
            try testArgs(u63, 1 << 1);
            try testArgs(u63, 1 << 61);
            try testArgs(u63, 1 << 62);

            try testArgs(i64, -1 << 63);
            try testArgs(i64, -1);
            try testArgs(i64, 0);
            try testArgs(u64, 0);
            try testArgs(u64, 1 << 0);
            try testArgs(u64, 1 << 1);
            try testArgs(u64, 1 << 62);
            try testArgs(u64, 1 << 63);
        }

        fn testFloats() !void {
            try testArgs(f32, -nan(f32));
            try testArgs(f32, -inf(f32));
            try testArgs(f32, -fmax(f32));
            try testArgs(f32, -1e1);
            try testArgs(f32, -1e0);
            try testArgs(f32, -1e-1);
            try testArgs(f32, -fmin(f32));
            try testArgs(f32, -tmin(f32));
            try testArgs(f32, -0.0);
            try testArgs(f32, 0.0);
            try testArgs(f32, tmin(f32));
            try testArgs(f32, fmin(f32));
            try testArgs(f32, 1e-1);
            try testArgs(f32, 1e0);
            try testArgs(f32, 1e1);
            try testArgs(f32, fmax(f32));
            try testArgs(f32, inf(f32));
            try testArgs(f32, nan(f32));

            try testArgs(f64, -nan(f64));
            try testArgs(f64, -inf(f64));
            try testArgs(f64, -fmax(f64));
            try testArgs(f64, -1e1);
            try testArgs(f64, -1e0);
            try testArgs(f64, -1e-1);
            try testArgs(f64, -fmin(f64));
            try testArgs(f64, -tmin(f64));
            try testArgs(f64, -0.0);
            try testArgs(f64, 0.0);
            try testArgs(f64, tmin(f64));
            try testArgs(f64, fmin(f64));
            try testArgs(f64, 1e-1);
            try testArgs(f64, 1e0);
            try testArgs(f64, 1e1);
            try testArgs(f64, fmax(f64));
            try testArgs(f64, inf(f64));
            try testArgs(f64, nan(f64));
        }
    };
}

fn binary(
    comptime op: anytype,
    comptime opts: struct { compare: Compare = .relaxed },
) type {
    return struct {
        noinline fn testArgKinds(
            comptime Type: type,
            comptime imm_lhs: Type,
            mem_lhs: Type,
            comptime imm_rhs: Type,
            mem_rhs: Type,
        ) !void {
            const expected = comptime op(Type, imm_lhs, imm_rhs);
            var reg_lhs = mem_lhs;
            var reg_rhs = mem_rhs;
            _ = .{ &reg_lhs, &reg_rhs };

            const result = op(Type, reg_lhs, reg_rhs);
            @breakpoint();
            std.debug.print("{}\n", .{result == expected});
            try checkExpected(expected, result, opts.compare);
            try checkExpected(expected, op(Type, reg_lhs, mem_rhs), opts.compare);
            try checkExpected(expected, op(Type, reg_lhs, imm_rhs), opts.compare);
            try checkExpected(expected, op(Type, mem_lhs, reg_rhs), opts.compare);
            try checkExpected(expected, op(Type, mem_lhs, mem_rhs), opts.compare);
            try checkExpected(expected, op(Type, mem_lhs, imm_rhs), opts.compare);
            try checkExpected(expected, op(Type, imm_lhs, reg_rhs), opts.compare);
            try checkExpected(expected, op(Type, imm_lhs, mem_rhs), opts.compare);
        }

        // noinline for a more helpful stack trace
        noinline fn testArgs(comptime Type: type, comptime imm_lhs: Type, comptime imm_rhs: Type) !void {
            try testArgKinds(
                Type,
                imm_lhs,
                imm_lhs,
                imm_rhs,
                imm_rhs,
            );
        }

        fn testInts() !void {
            try testArgs(u1, 0x1, 0x1);
            try testArgs(i2, 0x0, -0x2);
            try testArgs(u2, 0x2, 0x1);
            try testArgs(i3, 0x1, -0x3);
            try testArgs(u3, 0x6, 0x1);
            try testArgs(i4, 0x6, 0x3);
            try testArgs(u4, 0x8, 0x5);
            try testArgs(i5, -0x9, -0xd);
            try testArgs(u5, 0x5, 0x13);
            try testArgs(i7, 0x34, 0x1d);
            try testArgs(u7, 0x31, 0x56);
            try testArgs(i8, -0x57, -0x70);
            try testArgs(u8, 0x12, 0xd6);
            try testArgs(i9, -0x8a, -0xa0);
            try testArgs(u9, 0xf8, 0x95);
            try testArgs(i15, -0x790, 0x116f);
            try testArgs(u15, 0x548b, 0x4cd6);
            try testArgs(i16, -0x2d17, -0x5c17);
            try testArgs(u16, 0xadc0, 0xb223);
            try testArgs(i17, 0xe543, 0xaad5);
            try testArgs(u17, 0x9515, 0xa3c1);
            try testArgs(i31, -0x28858a2f, 0x369e917a);
            try testArgs(u31, 0x32bab794, 0x75464e7f);
            try testArgs(i32, 0x79e74e44, 0x61fe4ab1);
            try testArgs(u32, 0xc82f8e2, 0x5dde37e2);
            try testArgs(i33, -0xa4cbaa13, -0x4d20ee61);
            try testArgs(u33, 0x17461d437, 0x16cbc228f);
            try testArgs(i63, 0x333220e16b1e53fb, 0x121a0d970a5a4504);
            try testArgs(u63, 0x2dcd94e2ae4aa2af, 0x5f401e6e287a4dd7);
            try testArgs(u64, 0x430970421452be50, 0xb4b5e96f4183b5fc);
        }

        fn testFloats() !void {
            @setEvalBranchQuota(21_700);

            // try testArgs(f32, -nan(f32), -nan(f32));
            // try testArgs(f32, -nan(f32), -inf(f32));
            // try testArgs(f32, -nan(f32), -fmax(f32));
            // try testArgs(f32, -nan(f32), -1e1);
            // try testArgs(f32, -nan(f32), -1e0);
            // try testArgs(f32, -nan(f32), -1e-1);
            // try testArgs(f32, -nan(f32), -fmin(f32));
            // try testArgs(f32, -nan(f32), -tmin(f32));
            // try testArgs(f32, -nan(f32), -0.0);
            // try testArgs(f32, -nan(f32), 0.0);
            // try testArgs(f32, -nan(f32), tmin(f32));
            // try testArgs(f32, -nan(f32), fmin(f32));
            // try testArgs(f32, -nan(f32), 1e-1);
            // try testArgs(f32, -nan(f32), 1e0);
            // try testArgs(f32, -nan(f32), 1e1);
            // try testArgs(f32, -nan(f32), fmax(f32));
            // try testArgs(f32, -nan(f32), inf(f32));
            // try testArgs(f32, -nan(f32), nan(f32));

            // try testArgs(f32, -inf(f32), -nan(f32));
            // try testArgs(f32, -inf(f32), -inf(f32));
            // try testArgs(f32, -inf(f32), -fmax(f32));
            // try testArgs(f32, -inf(f32), -1e1);
            // try testArgs(f32, -inf(f32), -1e0);
            // try testArgs(f32, -inf(f32), -1e-1);
            // try testArgs(f32, -inf(f32), -fmin(f32));
            // try testArgs(f32, -inf(f32), -tmin(f32));
            // try testArgs(f32, -inf(f32), -0.0);
            // try testArgs(f32, -inf(f32), 0.0);
            // try testArgs(f32, -inf(f32), tmin(f32));
            // try testArgs(f32, -inf(f32), fmin(f32));
            // try testArgs(f32, -inf(f32), 1e-1);
            // try testArgs(f32, -inf(f32), 1e0);
            // try testArgs(f32, -inf(f32), 1e1);
            // try testArgs(f32, -inf(f32), fmax(f32));
            // try testArgs(f32, -inf(f32), inf(f32));
            // try testArgs(f32, -inf(f32), nan(f32));

            // try testArgs(f32, -fmax(f32), -nan(f32));
            // try testArgs(f32, -fmax(f32), -inf(f32));
            // try testArgs(f32, -fmax(f32), -fmax(f32));
            // try testArgs(f32, -fmax(f32), -1e1);
            // try testArgs(f32, -fmax(f32), -1e0);
            // try testArgs(f32, -fmax(f32), -1e-1);
            // try testArgs(f32, -fmax(f32), -fmin(f32));
            // try testArgs(f32, -fmax(f32), -tmin(f32));
            // try testArgs(f32, -fmax(f32), -0.0);
            // try testArgs(f32, -fmax(f32), 0.0);
            // try testArgs(f32, -fmax(f32), tmin(f32));
            // try testArgs(f32, -fmax(f32), fmin(f32));
            // try testArgs(f32, -fmax(f32), 1e-1);
            // try testArgs(f32, -fmax(f32), 1e0);
            // try testArgs(f32, -fmax(f32), 1e1);
            // try testArgs(f32, -fmax(f32), fmax(f32));
            // try testArgs(f32, -fmax(f32), inf(f32));
            // try testArgs(f32, -fmax(f32), nan(f32));

            // try testArgs(f32, -1e1, -nan(f32));
            // try testArgs(f32, -1e1, -inf(f32));
            // try testArgs(f32, -1e1, -fmax(f32));
            try testArgs(f32, -1e1, -1e1);
            // try testArgs(f32, -1e1, -1e0);
            // try testArgs(f32, -1e1, -1e-1);
            // try testArgs(f32, -1e1, -fmin(f32));
            // try testArgs(f32, -1e1, -tmin(f32));
            // try testArgs(f32, -1e1, -0.0);
            // try testArgs(f32, -1e1, 0.0);
            // try testArgs(f32, -1e1, tmin(f32));
            // try testArgs(f32, -1e1, fmin(f32));
            // try testArgs(f32, -1e1, 1e-1);
            // try testArgs(f32, -1e1, 1e0);
            // try testArgs(f32, -1e1, 1e1);
            // try testArgs(f32, -1e1, fmax(f32));
            // try testArgs(f32, -1e1, inf(f32));
            // try testArgs(f32, -1e1, nan(f32));

            // try testArgs(f32, -1e0, -nan(f32));
            // try testArgs(f32, -1e0, -inf(f32));
            // try testArgs(f32, -1e0, -fmax(f32));
            // try testArgs(f32, -1e0, -1e1);
            // try testArgs(f32, -1e0, -1e0);
            // try testArgs(f32, -1e0, -1e-1);
            // try testArgs(f32, -1e0, -fmin(f32));
            // try testArgs(f32, -1e0, -tmin(f32));
            // try testArgs(f32, -1e0, -0.0);
            // try testArgs(f32, -1e0, 0.0);
            // try testArgs(f32, -1e0, tmin(f32));
            // try testArgs(f32, -1e0, fmin(f32));
            // try testArgs(f32, -1e0, 1e-1);
            // try testArgs(f32, -1e0, 1e0);
            // try testArgs(f32, -1e0, 1e1);
            // try testArgs(f32, -1e0, fmax(f32));
            // try testArgs(f32, -1e0, inf(f32));
            // try testArgs(f32, -1e0, nan(f32));

            // try testArgs(f32, -1e-1, -nan(f32));
            // try testArgs(f32, -1e-1, -inf(f32));
            // try testArgs(f32, -1e-1, -fmax(f32));
            // try testArgs(f32, -1e-1, -1e1);
            // try testArgs(f32, -1e-1, -1e0);
            // try testArgs(f32, -1e-1, -1e-1);
            // try testArgs(f32, -1e-1, -fmin(f32));
            // try testArgs(f32, -1e-1, -tmin(f32));
            // try testArgs(f32, -1e-1, -0.0);
            // try testArgs(f32, -1e-1, 0.0);
            // try testArgs(f32, -1e-1, tmin(f32));
            // try testArgs(f32, -1e-1, fmin(f32));
            // try testArgs(f32, -1e-1, 1e-1);
            // try testArgs(f32, -1e-1, 1e0);
            // try testArgs(f32, -1e-1, 1e1);
            // try testArgs(f32, -1e-1, fmax(f32));
            // try testArgs(f32, -1e-1, inf(f32));
            // try testArgs(f32, -1e-1, nan(f32));

            // try testArgs(f32, -fmin(f32), -nan(f32));
            // try testArgs(f32, -fmin(f32), -inf(f32));
            // try testArgs(f32, -fmin(f32), -fmax(f32));
            // try testArgs(f32, -fmin(f32), -1e1);
            // try testArgs(f32, -fmin(f32), -1e0);
            // try testArgs(f32, -fmin(f32), -1e-1);
            // try testArgs(f32, -fmin(f32), -fmin(f32));
            // try testArgs(f32, -fmin(f32), -tmin(f32));
            // try testArgs(f32, -fmin(f32), -0.0);
            // try testArgs(f32, -fmin(f32), 0.0);
            // try testArgs(f32, -fmin(f32), tmin(f32));
            // try testArgs(f32, -fmin(f32), fmin(f32));
            // try testArgs(f32, -fmin(f32), 1e-1);
            // try testArgs(f32, -fmin(f32), 1e0);
            // try testArgs(f32, -fmin(f32), 1e1);
            // try testArgs(f32, -fmin(f32), fmax(f32));
            // try testArgs(f32, -fmin(f32), inf(f32));
            // try testArgs(f32, -fmin(f32), nan(f32));

            // try testArgs(f32, -tmin(f32), -nan(f32));
            // try testArgs(f32, -tmin(f32), -inf(f32));
            // try testArgs(f32, -tmin(f32), -fmax(f32));
            // try testArgs(f32, -tmin(f32), -1e1);
            // try testArgs(f32, -tmin(f32), -1e0);
            // try testArgs(f32, -tmin(f32), -1e-1);
            // try testArgs(f32, -tmin(f32), -fmin(f32));
            // try testArgs(f32, -tmin(f32), -tmin(f32));
            // try testArgs(f32, -tmin(f32), -0.0);
            // try testArgs(f32, -tmin(f32), 0.0);
            // try testArgs(f32, -tmin(f32), tmin(f32));
            // try testArgs(f32, -tmin(f32), fmin(f32));
            // try testArgs(f32, -tmin(f32), 1e-1);
            // try testArgs(f32, -tmin(f32), 1e0);
            // try testArgs(f32, -tmin(f32), 1e1);
            // try testArgs(f32, -tmin(f32), fmax(f32));
            // try testArgs(f32, -tmin(f32), inf(f32));
            // try testArgs(f32, -tmin(f32), nan(f32));

            // try testArgs(f32, -0.0, -nan(f32));
            // try testArgs(f32, -0.0, -inf(f32));
            // try testArgs(f32, -0.0, -fmax(f32));
            // try testArgs(f32, -0.0, -1e1);
            // try testArgs(f32, -0.0, -1e0);
            // try testArgs(f32, -0.0, -1e-1);
            // try testArgs(f32, -0.0, -fmin(f32));
            // try testArgs(f32, -0.0, -tmin(f32));
            // try testArgs(f32, -0.0, -0.0);
            // try testArgs(f32, -0.0, 0.0);
            // try testArgs(f32, -0.0, tmin(f32));
            // try testArgs(f32, -0.0, fmin(f32));
            // try testArgs(f32, -0.0, 1e-1);
            // try testArgs(f32, -0.0, 1e0);
            // try testArgs(f32, -0.0, 1e1);
            // try testArgs(f32, -0.0, fmax(f32));
            // try testArgs(f32, -0.0, inf(f32));
            // try testArgs(f32, -0.0, nan(f32));

            // try testArgs(f32, 0.0, -nan(f32));
            // try testArgs(f32, 0.0, -inf(f32));
            // try testArgs(f32, 0.0, -fmax(f32));
            // try testArgs(f32, 0.0, -1e1);
            // try testArgs(f32, 0.0, -1e0);
            // try testArgs(f32, 0.0, -1e-1);
            // try testArgs(f32, 0.0, -fmin(f32));
            // try testArgs(f32, 0.0, -tmin(f32));
            // try testArgs(f32, 0.0, -0.0);
            // try testArgs(f32, 0.0, 0.0);
            // try testArgs(f32, 0.0, tmin(f32));
            // try testArgs(f32, 0.0, fmin(f32));
            // try testArgs(f32, 0.0, 1e-1);
            // try testArgs(f32, 0.0, 1e0);
            // try testArgs(f32, 0.0, 1e1);
            // try testArgs(f32, 0.0, fmax(f32));
            // try testArgs(f32, 0.0, inf(f32));
            // try testArgs(f32, 0.0, nan(f32));

            // try testArgs(f32, tmin(f32), -nan(f32));
            // try testArgs(f32, tmin(f32), -inf(f32));
            // try testArgs(f32, tmin(f32), -fmax(f32));
            // try testArgs(f32, tmin(f32), -1e1);
            // try testArgs(f32, tmin(f32), -1e0);
            // try testArgs(f32, tmin(f32), -1e-1);
            // try testArgs(f32, tmin(f32), -fmin(f32));
            // try testArgs(f32, tmin(f32), -tmin(f32));
            // try testArgs(f32, tmin(f32), -0.0);
            // try testArgs(f32, tmin(f32), 0.0);
            // try testArgs(f32, tmin(f32), tmin(f32));
            // try testArgs(f32, tmin(f32), fmin(f32));
            // try testArgs(f32, tmin(f32), 1e-1);
            // try testArgs(f32, tmin(f32), 1e0);
            // try testArgs(f32, tmin(f32), 1e1);
            // try testArgs(f32, tmin(f32), fmax(f32));
            // try testArgs(f32, tmin(f32), inf(f32));
            // try testArgs(f32, tmin(f32), nan(f32));

            // try testArgs(f32, fmin(f32), -nan(f32));
            // try testArgs(f32, fmin(f32), -inf(f32));
            // try testArgs(f32, fmin(f32), -fmax(f32));
            // try testArgs(f32, fmin(f32), -1e1);
            // try testArgs(f32, fmin(f32), -1e0);
            // try testArgs(f32, fmin(f32), -1e-1);
            // try testArgs(f32, fmin(f32), -fmin(f32));
            // try testArgs(f32, fmin(f32), -tmin(f32));
            // try testArgs(f32, fmin(f32), -0.0);
            // try testArgs(f32, fmin(f32), 0.0);
            // try testArgs(f32, fmin(f32), tmin(f32));
            // try testArgs(f32, fmin(f32), fmin(f32));
            // try testArgs(f32, fmin(f32), 1e-1);
            // try testArgs(f32, fmin(f32), 1e0);
            // try testArgs(f32, fmin(f32), 1e1);
            // try testArgs(f32, fmin(f32), fmax(f32));
            // try testArgs(f32, fmin(f32), inf(f32));
            // try testArgs(f32, fmin(f32), nan(f32));

            // try testArgs(f32, 1e-1, -nan(f32));
            // try testArgs(f32, 1e-1, -inf(f32));
            // try testArgs(f32, 1e-1, -fmax(f32));
            // try testArgs(f32, 1e-1, -1e1);
            // try testArgs(f32, 1e-1, -1e0);
            // try testArgs(f32, 1e-1, -1e-1);
            // try testArgs(f32, 1e-1, -fmin(f32));
            // try testArgs(f32, 1e-1, -tmin(f32));
            // try testArgs(f32, 1e-1, -0.0);
            // try testArgs(f32, 1e-1, 0.0);
            // try testArgs(f32, 1e-1, tmin(f32));
            // try testArgs(f32, 1e-1, fmin(f32));
            // try testArgs(f32, 1e-1, 1e-1);
            // try testArgs(f32, 1e-1, 1e0);
            // try testArgs(f32, 1e-1, 1e1);
            // try testArgs(f32, 1e-1, fmax(f32));
            // try testArgs(f32, 1e-1, inf(f32));
            // try testArgs(f32, 1e-1, nan(f32));

            // try testArgs(f32, 1e0, -nan(f32));
            // try testArgs(f32, 1e0, -inf(f32));
            // try testArgs(f32, 1e0, -fmax(f32));
            // try testArgs(f32, 1e0, -1e1);
            // try testArgs(f32, 1e0, -1e0);
            // try testArgs(f32, 1e0, -1e-1);
            // try testArgs(f32, 1e0, -fmin(f32));
            // try testArgs(f32, 1e0, -tmin(f32));
            // try testArgs(f32, 1e0, -0.0);
            // try testArgs(f32, 1e0, 0.0);
            // try testArgs(f32, 1e0, tmin(f32));
            // try testArgs(f32, 1e0, fmin(f32));
            // try testArgs(f32, 1e0, 1e-1);
            // try testArgs(f32, 1e0, 1e0);
            // try testArgs(f32, 1e0, 1e1);
            // try testArgs(f32, 1e0, fmax(f32));
            // try testArgs(f32, 1e0, inf(f32));
            // try testArgs(f32, 1e0, nan(f32));

            // try testArgs(f32, 1e1, -nan(f32));
            // try testArgs(f32, 1e1, -inf(f32));
            // try testArgs(f32, 1e1, -fmax(f32));
            // try testArgs(f32, 1e1, -1e1);
            // try testArgs(f32, 1e1, -1e0);
            // try testArgs(f32, 1e1, -1e-1);
            // try testArgs(f32, 1e1, -fmin(f32));
            // try testArgs(f32, 1e1, -tmin(f32));
            // try testArgs(f32, 1e1, -0.0);
            // try testArgs(f32, 1e1, 0.0);
            // try testArgs(f32, 1e1, tmin(f32));
            // try testArgs(f32, 1e1, fmin(f32));
            // try testArgs(f32, 1e1, 1e-1);
            // try testArgs(f32, 1e1, 1e0);
            // try testArgs(f32, 1e1, 1e1);
            // try testArgs(f32, 1e1, fmax(f32));
            // try testArgs(f32, 1e1, inf(f32));
            // try testArgs(f32, 1e1, nan(f32));

            // try testArgs(f32, fmax(f32), -nan(f32));
            // try testArgs(f32, fmax(f32), -inf(f32));
            // try testArgs(f32, fmax(f32), -fmax(f32));
            // try testArgs(f32, fmax(f32), -1e1);
            // try testArgs(f32, fmax(f32), -1e0);
            // try testArgs(f32, fmax(f32), -1e-1);
            // try testArgs(f32, fmax(f32), -fmin(f32));
            // try testArgs(f32, fmax(f32), -tmin(f32));
            // try testArgs(f32, fmax(f32), -0.0);
            // try testArgs(f32, fmax(f32), 0.0);
            // try testArgs(f32, fmax(f32), tmin(f32));
            // try testArgs(f32, fmax(f32), fmin(f32));
            // try testArgs(f32, fmax(f32), 1e-1);
            // try testArgs(f32, fmax(f32), 1e0);
            // try testArgs(f32, fmax(f32), 1e1);
            // try testArgs(f32, fmax(f32), fmax(f32));
            // try testArgs(f32, fmax(f32), inf(f32));
            // try testArgs(f32, fmax(f32), nan(f32));

            // try testArgs(f32, inf(f32), -nan(f32));
            // try testArgs(f32, inf(f32), -inf(f32));
            // try testArgs(f32, inf(f32), -fmax(f32));
            // try testArgs(f32, inf(f32), -1e1);
            // try testArgs(f32, inf(f32), -1e0);
            // try testArgs(f32, inf(f32), -1e-1);
            // try testArgs(f32, inf(f32), -fmin(f32));
            // try testArgs(f32, inf(f32), -tmin(f32));
            // try testArgs(f32, inf(f32), -0.0);
            // try testArgs(f32, inf(f32), 0.0);
            // try testArgs(f32, inf(f32), tmin(f32));
            // try testArgs(f32, inf(f32), fmin(f32));
            // try testArgs(f32, inf(f32), 1e-1);
            // try testArgs(f32, inf(f32), 1e0);
            // try testArgs(f32, inf(f32), 1e1);
            // try testArgs(f32, inf(f32), fmax(f32));
            // try testArgs(f32, inf(f32), inf(f32));
            // try testArgs(f32, inf(f32), nan(f32));

            // try testArgs(f32, nan(f32), -nan(f32));
            // try testArgs(f32, nan(f32), -inf(f32));
            // try testArgs(f32, nan(f32), -fmax(f32));
            // try testArgs(f32, nan(f32), -1e1);
            // try testArgs(f32, nan(f32), -1e0);
            // try testArgs(f32, nan(f32), -1e-1);
            // try testArgs(f32, nan(f32), -fmin(f32));
            // try testArgs(f32, nan(f32), -tmin(f32));
            // try testArgs(f32, nan(f32), -0.0);
            // try testArgs(f32, nan(f32), 0.0);
            // try testArgs(f32, nan(f32), tmin(f32));
            // try testArgs(f32, nan(f32), fmin(f32));
            // try testArgs(f32, nan(f32), 1e-1);
            // try testArgs(f32, nan(f32), 1e0);
            // try testArgs(f32, nan(f32), 1e1);
            // try testArgs(f32, nan(f32), fmax(f32));
            // try testArgs(f32, nan(f32), inf(f32));
            // try testArgs(f32, nan(f32), nan(f32));

            // try testArgs(f64, -nan(f64), -nan(f64));
            // try testArgs(f64, -nan(f64), -inf(f64));
            // try testArgs(f64, -nan(f64), -fmax(f64));
            // try testArgs(f64, -nan(f64), -1e1);
            // try testArgs(f64, -nan(f64), -1e0);
            // try testArgs(f64, -nan(f64), -1e-1);
            // try testArgs(f64, -nan(f64), -fmin(f64));
            // try testArgs(f64, -nan(f64), -tmin(f64));
            // try testArgs(f64, -nan(f64), -0.0);
            // try testArgs(f64, -nan(f64), 0.0);
            // try testArgs(f64, -nan(f64), tmin(f64));
            // try testArgs(f64, -nan(f64), fmin(f64));
            // try testArgs(f64, -nan(f64), 1e-1);
            // try testArgs(f64, -nan(f64), 1e0);
            // try testArgs(f64, -nan(f64), 1e1);
            // try testArgs(f64, -nan(f64), fmax(f64));
            // try testArgs(f64, -nan(f64), inf(f64));
            // try testArgs(f64, -nan(f64), nan(f64));

            // try testArgs(f64, -inf(f64), -nan(f64));
            // try testArgs(f64, -inf(f64), -inf(f64));
            // try testArgs(f64, -inf(f64), -fmax(f64));
            // try testArgs(f64, -inf(f64), -1e1);
            // try testArgs(f64, -inf(f64), -1e0);
            // try testArgs(f64, -inf(f64), -1e-1);
            // try testArgs(f64, -inf(f64), -fmin(f64));
            // try testArgs(f64, -inf(f64), -tmin(f64));
            // try testArgs(f64, -inf(f64), -0.0);
            // try testArgs(f64, -inf(f64), 0.0);
            // try testArgs(f64, -inf(f64), tmin(f64));
            // try testArgs(f64, -inf(f64), fmin(f64));
            // try testArgs(f64, -inf(f64), 1e-1);
            // try testArgs(f64, -inf(f64), 1e0);
            // try testArgs(f64, -inf(f64), 1e1);
            // try testArgs(f64, -inf(f64), fmax(f64));
            // try testArgs(f64, -inf(f64), inf(f64));
            // try testArgs(f64, -inf(f64), nan(f64));

            // try testArgs(f64, -fmax(f64), -nan(f64));
            // try testArgs(f64, -fmax(f64), -inf(f64));
            // try testArgs(f64, -fmax(f64), -fmax(f64));
            // try testArgs(f64, -fmax(f64), -1e1);
            // try testArgs(f64, -fmax(f64), -1e0);
            // try testArgs(f64, -fmax(f64), -1e-1);
            // try testArgs(f64, -fmax(f64), -fmin(f64));
            // try testArgs(f64, -fmax(f64), -tmin(f64));
            // try testArgs(f64, -fmax(f64), -0.0);
            // try testArgs(f64, -fmax(f64), 0.0);
            // try testArgs(f64, -fmax(f64), tmin(f64));
            // try testArgs(f64, -fmax(f64), fmin(f64));
            // try testArgs(f64, -fmax(f64), 1e-1);
            // try testArgs(f64, -fmax(f64), 1e0);
            // try testArgs(f64, -fmax(f64), 1e1);
            // try testArgs(f64, -fmax(f64), fmax(f64));
            // try testArgs(f64, -fmax(f64), inf(f64));
            // try testArgs(f64, -fmax(f64), nan(f64));

            // try testArgs(f64, -1e1, -nan(f64));
            // try testArgs(f64, -1e1, -inf(f64));
            // try testArgs(f64, -1e1, -fmax(f64));
            // try testArgs(f64, -1e1, -1e1);
            // try testArgs(f64, -1e1, -1e0);
            // try testArgs(f64, -1e1, -1e-1);
            // try testArgs(f64, -1e1, -fmin(f64));
            // try testArgs(f64, -1e1, -tmin(f64));
            // try testArgs(f64, -1e1, -0.0);
            // try testArgs(f64, -1e1, 0.0);
            // try testArgs(f64, -1e1, tmin(f64));
            // try testArgs(f64, -1e1, fmin(f64));
            // try testArgs(f64, -1e1, 1e-1);
            // try testArgs(f64, -1e1, 1e0);
            // try testArgs(f64, -1e1, 1e1);
            // try testArgs(f64, -1e1, fmax(f64));
            // try testArgs(f64, -1e1, inf(f64));
            // try testArgs(f64, -1e1, nan(f64));

            // try testArgs(f64, -1e0, -nan(f64));
            // try testArgs(f64, -1e0, -inf(f64));
            // try testArgs(f64, -1e0, -fmax(f64));
            // try testArgs(f64, -1e0, -1e1);
            // try testArgs(f64, -1e0, -1e0);
            // try testArgs(f64, -1e0, -1e-1);
            // try testArgs(f64, -1e0, -fmin(f64));
            // try testArgs(f64, -1e0, -tmin(f64));
            // try testArgs(f64, -1e0, -0.0);
            // try testArgs(f64, -1e0, 0.0);
            // try testArgs(f64, -1e0, tmin(f64));
            // try testArgs(f64, -1e0, fmin(f64));
            // try testArgs(f64, -1e0, 1e-1);
            // try testArgs(f64, -1e0, 1e0);
            // try testArgs(f64, -1e0, 1e1);
            // try testArgs(f64, -1e0, fmax(f64));
            // try testArgs(f64, -1e0, inf(f64));
            // try testArgs(f64, -1e0, nan(f64));

            // try testArgs(f64, -1e-1, -nan(f64));
            // try testArgs(f64, -1e-1, -inf(f64));
            // try testArgs(f64, -1e-1, -fmax(f64));
            // try testArgs(f64, -1e-1, -1e1);
            // try testArgs(f64, -1e-1, -1e0);
            // try testArgs(f64, -1e-1, -1e-1);
            // try testArgs(f64, -1e-1, -fmin(f64));
            // try testArgs(f64, -1e-1, -tmin(f64));
            // try testArgs(f64, -1e-1, -0.0);
            // try testArgs(f64, -1e-1, 0.0);
            // try testArgs(f64, -1e-1, tmin(f64));
            // try testArgs(f64, -1e-1, fmin(f64));
            // try testArgs(f64, -1e-1, 1e-1);
            // try testArgs(f64, -1e-1, 1e0);
            // try testArgs(f64, -1e-1, 1e1);
            // try testArgs(f64, -1e-1, fmax(f64));
            // try testArgs(f64, -1e-1, inf(f64));
            // try testArgs(f64, -1e-1, nan(f64));

            // try testArgs(f64, -fmin(f64), -nan(f64));
            // try testArgs(f64, -fmin(f64), -inf(f64));
            // try testArgs(f64, -fmin(f64), -fmax(f64));
            // try testArgs(f64, -fmin(f64), -1e1);
            // try testArgs(f64, -fmin(f64), -1e0);
            // try testArgs(f64, -fmin(f64), -1e-1);
            // try testArgs(f64, -fmin(f64), -fmin(f64));
            // try testArgs(f64, -fmin(f64), -tmin(f64));
            // try testArgs(f64, -fmin(f64), -0.0);
            // try testArgs(f64, -fmin(f64), 0.0);
            // try testArgs(f64, -fmin(f64), tmin(f64));
            // try testArgs(f64, -fmin(f64), fmin(f64));
            // try testArgs(f64, -fmin(f64), 1e-1);
            // try testArgs(f64, -fmin(f64), 1e0);
            // try testArgs(f64, -fmin(f64), 1e1);
            // try testArgs(f64, -fmin(f64), fmax(f64));
            // try testArgs(f64, -fmin(f64), inf(f64));
            // try testArgs(f64, -fmin(f64), nan(f64));

            // try testArgs(f64, -tmin(f64), -nan(f64));
            // try testArgs(f64, -tmin(f64), -inf(f64));
            // try testArgs(f64, -tmin(f64), -fmax(f64));
            // try testArgs(f64, -tmin(f64), -1e1);
            // try testArgs(f64, -tmin(f64), -1e0);
            // try testArgs(f64, -tmin(f64), -1e-1);
            // try testArgs(f64, -tmin(f64), -fmin(f64));
            // try testArgs(f64, -tmin(f64), -tmin(f64));
            // try testArgs(f64, -tmin(f64), -0.0);
            // try testArgs(f64, -tmin(f64), 0.0);
            // try testArgs(f64, -tmin(f64), tmin(f64));
            // try testArgs(f64, -tmin(f64), fmin(f64));
            // try testArgs(f64, -tmin(f64), 1e-1);
            // try testArgs(f64, -tmin(f64), 1e0);
            // try testArgs(f64, -tmin(f64), 1e1);
            // try testArgs(f64, -tmin(f64), fmax(f64));
            // try testArgs(f64, -tmin(f64), inf(f64));
            // try testArgs(f64, -tmin(f64), nan(f64));

            // try testArgs(f64, -0.0, -nan(f64));
            // try testArgs(f64, -0.0, -inf(f64));
            // try testArgs(f64, -0.0, -fmax(f64));
            // try testArgs(f64, -0.0, -1e1);
            // try testArgs(f64, -0.0, -1e0);
            // try testArgs(f64, -0.0, -1e-1);
            // try testArgs(f64, -0.0, -fmin(f64));
            // try testArgs(f64, -0.0, -tmin(f64));
            // try testArgs(f64, -0.0, -0.0);
            // try testArgs(f64, -0.0, 0.0);
            // try testArgs(f64, -0.0, tmin(f64));
            // try testArgs(f64, -0.0, fmin(f64));
            // try testArgs(f64, -0.0, 1e-1);
            // try testArgs(f64, -0.0, 1e0);
            // try testArgs(f64, -0.0, 1e1);
            // try testArgs(f64, -0.0, fmax(f64));
            // try testArgs(f64, -0.0, inf(f64));
            // try testArgs(f64, -0.0, nan(f64));

            // try testArgs(f64, 0.0, -nan(f64));
            // try testArgs(f64, 0.0, -inf(f64));
            // try testArgs(f64, 0.0, -fmax(f64));
            // try testArgs(f64, 0.0, -1e1);
            // try testArgs(f64, 0.0, -1e0);
            // try testArgs(f64, 0.0, -1e-1);
            // try testArgs(f64, 0.0, -fmin(f64));
            // try testArgs(f64, 0.0, -tmin(f64));
            // try testArgs(f64, 0.0, -0.0);
            // try testArgs(f64, 0.0, 0.0);
            // try testArgs(f64, 0.0, tmin(f64));
            // try testArgs(f64, 0.0, fmin(f64));
            // try testArgs(f64, 0.0, 1e-1);
            // try testArgs(f64, 0.0, 1e0);
            // try testArgs(f64, 0.0, 1e1);
            // try testArgs(f64, 0.0, fmax(f64));
            // try testArgs(f64, 0.0, inf(f64));
            // try testArgs(f64, 0.0, nan(f64));

            // try testArgs(f64, tmin(f64), -nan(f64));
            // try testArgs(f64, tmin(f64), -inf(f64));
            // try testArgs(f64, tmin(f64), -fmax(f64));
            // try testArgs(f64, tmin(f64), -1e1);
            // try testArgs(f64, tmin(f64), -1e0);
            // try testArgs(f64, tmin(f64), -1e-1);
            // try testArgs(f64, tmin(f64), -fmin(f64));
            // try testArgs(f64, tmin(f64), -tmin(f64));
            // try testArgs(f64, tmin(f64), -0.0);
            // try testArgs(f64, tmin(f64), 0.0);
            // try testArgs(f64, tmin(f64), tmin(f64));
            // try testArgs(f64, tmin(f64), fmin(f64));
            // try testArgs(f64, tmin(f64), 1e-1);
            // try testArgs(f64, tmin(f64), 1e0);
            // try testArgs(f64, tmin(f64), 1e1);
            // try testArgs(f64, tmin(f64), fmax(f64));
            // try testArgs(f64, tmin(f64), inf(f64));
            // try testArgs(f64, tmin(f64), nan(f64));

            // try testArgs(f64, fmin(f64), -nan(f64));
            // try testArgs(f64, fmin(f64), -inf(f64));
            // try testArgs(f64, fmin(f64), -fmax(f64));
            // try testArgs(f64, fmin(f64), -1e1);
            // try testArgs(f64, fmin(f64), -1e0);
            // try testArgs(f64, fmin(f64), -1e-1);
            // try testArgs(f64, fmin(f64), -fmin(f64));
            // try testArgs(f64, fmin(f64), -tmin(f64));
            // try testArgs(f64, fmin(f64), -0.0);
            // try testArgs(f64, fmin(f64), 0.0);
            // try testArgs(f64, fmin(f64), tmin(f64));
            // try testArgs(f64, fmin(f64), fmin(f64));
            // try testArgs(f64, fmin(f64), 1e-1);
            // try testArgs(f64, fmin(f64), 1e0);
            // try testArgs(f64, fmin(f64), 1e1);
            // try testArgs(f64, fmin(f64), fmax(f64));
            // try testArgs(f64, fmin(f64), inf(f64));
            // try testArgs(f64, fmin(f64), nan(f64));

            // try testArgs(f64, 1e-1, -nan(f64));
            // try testArgs(f64, 1e-1, -inf(f64));
            // try testArgs(f64, 1e-1, -fmax(f64));
            // try testArgs(f64, 1e-1, -1e1);
            // try testArgs(f64, 1e-1, -1e0);
            // try testArgs(f64, 1e-1, -1e-1);
            // try testArgs(f64, 1e-1, -fmin(f64));
            // try testArgs(f64, 1e-1, -tmin(f64));
            // try testArgs(f64, 1e-1, -0.0);
            // try testArgs(f64, 1e-1, 0.0);
            // try testArgs(f64, 1e-1, tmin(f64));
            // try testArgs(f64, 1e-1, fmin(f64));
            // try testArgs(f64, 1e-1, 1e-1);
            // try testArgs(f64, 1e-1, 1e0);
            // try testArgs(f64, 1e-1, 1e1);
            // try testArgs(f64, 1e-1, fmax(f64));
            // try testArgs(f64, 1e-1, inf(f64));
            // try testArgs(f64, 1e-1, nan(f64));

            // try testArgs(f64, 1e0, -nan(f64));
            // try testArgs(f64, 1e0, -inf(f64));
            // try testArgs(f64, 1e0, -fmax(f64));
            // try testArgs(f64, 1e0, -1e1);
            // try testArgs(f64, 1e0, -1e0);
            // try testArgs(f64, 1e0, -1e-1);
            // try testArgs(f64, 1e0, -fmin(f64));
            // try testArgs(f64, 1e0, -tmin(f64));
            // try testArgs(f64, 1e0, -0.0);
            // try testArgs(f64, 1e0, 0.0);
            // try testArgs(f64, 1e0, tmin(f64));
            // try testArgs(f64, 1e0, fmin(f64));
            // try testArgs(f64, 1e0, 1e-1);
            // try testArgs(f64, 1e0, 1e0);
            // try testArgs(f64, 1e0, 1e1);
            // try testArgs(f64, 1e0, fmax(f64));
            // try testArgs(f64, 1e0, inf(f64));
            // try testArgs(f64, 1e0, nan(f64));

            // try testArgs(f64, 1e1, -nan(f64));
            // try testArgs(f64, 1e1, -inf(f64));
            // try testArgs(f64, 1e1, -fmax(f64));
            // try testArgs(f64, 1e1, -1e1);
            // try testArgs(f64, 1e1, -1e0);
            // try testArgs(f64, 1e1, -1e-1);
            // try testArgs(f64, 1e1, -fmin(f64));
            // try testArgs(f64, 1e1, -tmin(f64));
            // try testArgs(f64, 1e1, -0.0);
            // try testArgs(f64, 1e1, 0.0);
            // try testArgs(f64, 1e1, tmin(f64));
            // try testArgs(f64, 1e1, fmin(f64));
            // try testArgs(f64, 1e1, 1e-1);
            // try testArgs(f64, 1e1, 1e0);
            // try testArgs(f64, 1e1, 1e1);
            // try testArgs(f64, 1e1, fmax(f64));
            // try testArgs(f64, 1e1, inf(f64));
            // try testArgs(f64, 1e1, nan(f64));

            // try testArgs(f64, fmax(f64), -nan(f64));
            // try testArgs(f64, fmax(f64), -inf(f64));
            // try testArgs(f64, fmax(f64), -fmax(f64));
            // try testArgs(f64, fmax(f64), -1e1);
            // try testArgs(f64, fmax(f64), -1e0);
            // try testArgs(f64, fmax(f64), -1e-1);
            // try testArgs(f64, fmax(f64), -fmin(f64));
            // try testArgs(f64, fmax(f64), -tmin(f64));
            // try testArgs(f64, fmax(f64), -0.0);
            // try testArgs(f64, fmax(f64), 0.0);
            // try testArgs(f64, fmax(f64), tmin(f64));
            // try testArgs(f64, fmax(f64), fmin(f64));
            // try testArgs(f64, fmax(f64), 1e-1);
            // try testArgs(f64, fmax(f64), 1e0);
            // try testArgs(f64, fmax(f64), 1e1);
            // try testArgs(f64, fmax(f64), fmax(f64));
            // try testArgs(f64, fmax(f64), inf(f64));
            // try testArgs(f64, fmax(f64), nan(f64));

            // try testArgs(f64, inf(f64), -nan(f64));
            // try testArgs(f64, inf(f64), -inf(f64));
            // try testArgs(f64, inf(f64), -fmax(f64));
            // try testArgs(f64, inf(f64), -1e1);
            // try testArgs(f64, inf(f64), -1e0);
            // try testArgs(f64, inf(f64), -1e-1);
            // try testArgs(f64, inf(f64), -fmin(f64));
            // try testArgs(f64, inf(f64), -tmin(f64));
            // try testArgs(f64, inf(f64), -0.0);
            // try testArgs(f64, inf(f64), 0.0);
            // try testArgs(f64, inf(f64), tmin(f64));
            // try testArgs(f64, inf(f64), fmin(f64));
            // try testArgs(f64, inf(f64), 1e-1);
            // try testArgs(f64, inf(f64), 1e0);
            // try testArgs(f64, inf(f64), 1e1);
            // try testArgs(f64, inf(f64), fmax(f64));
            // try testArgs(f64, inf(f64), inf(f64));
            // try testArgs(f64, inf(f64), nan(f64));

            // try testArgs(f64, nan(f64), -nan(f64));
            // try testArgs(f64, nan(f64), -inf(f64));
            // try testArgs(f64, nan(f64), -fmax(f64));
            // try testArgs(f64, nan(f64), -1e1);
            // try testArgs(f64, nan(f64), -1e0);
            // try testArgs(f64, nan(f64), -1e-1);
            // try testArgs(f64, nan(f64), -fmin(f64));
            // try testArgs(f64, nan(f64), -tmin(f64));
            // try testArgs(f64, nan(f64), -0.0);
            // try testArgs(f64, nan(f64), 0.0);
            // try testArgs(f64, nan(f64), tmin(f64));
            // try testArgs(f64, nan(f64), fmin(f64));
            // try testArgs(f64, nan(f64), 1e-1);
            // try testArgs(f64, nan(f64), 1e0);
            // try testArgs(f64, nan(f64), 1e1);
            // try testArgs(f64, nan(f64), fmax(f64));
            // try testArgs(f64, nan(f64), inf(f64));
            // try testArgs(f64, nan(f64), nan(f64));
        }
    };
}

inline fn equal(comptime Type: type, lhs: Type, rhs: Type) @TypeOf(lhs == rhs) {
    return lhs == rhs;
}
test equal {
    const test_equal = binary(equal, .{});
    // try test_equal.testInts();
    try test_equal.testFloats();
}

// inline fn notEqual(comptime Type: type, lhs: Type, rhs: Type) @TypeOf(lhs != rhs) {
//     return lhs != rhs;
// }
// test notEqual {
//     const test_not_equal = binary(notEqual, .{});
//     try test_not_equal.testInts();
//     try test_not_equal.testFloats();
// }

// inline fn lessThan(comptime Type: type, lhs: Type, rhs: Type) @TypeOf(lhs < rhs) {
//     return lhs < rhs;
// }
// test lessThan {
//     const test_less_than = binary(lessThan, .{});
//     try test_less_than.testInts();
//     try test_less_than.testFloats();
// }

// inline fn lessThanOrEqual(comptime Type: type, lhs: Type, rhs: Type) @TypeOf(lhs <= rhs) {
//     return lhs <= rhs;
// }
// test lessThanOrEqual {
//     const test_less_than_or_equal = binary(lessThanOrEqual, .{});
//     try test_less_than_or_equal.testInts();
//     try test_less_than_or_equal.testFloats();
// }

// inline fn greaterThan(comptime Type: type, lhs: Type, rhs: Type) @TypeOf(lhs > rhs) {
//     return lhs > rhs;
// }
// test greaterThan {
//     const test_greater_than = binary(greaterThan, .{});
//     try test_greater_than.testInts();
//     try test_greater_than.testFloats();
// }

// inline fn greaterThanOrEqual(comptime Type: type, lhs: Type, rhs: Type) @TypeOf(lhs >= rhs) {
//     return lhs >= rhs;
// }
// test greaterThanOrEqual {
//     const test_greater_than_or_equal = binary(greaterThanOrEqual, .{});
//     try test_greater_than_or_equal.testInts();
//     try test_greater_than_or_equal.testFloats();
// }

// inline fn addUnsafe(comptime Type: type, lhs: Type, rhs: Type) AddOneBit(Type) {
//     @setRuntimeSafety(false);
//     return @as(AddOneBit(Type), lhs) + rhs;
// }
// test addUnsafe {
//     const test_add_unsafe = binary(addUnsafe, .{});
//     try test_add_unsafe.testInts();
//     // try test_add_unsafe.testFloats();
// }

// inline fn subUnsafe(comptime Type: type, lhs: Type, rhs: Type) AddOneBit(Type) {
//     @setRuntimeSafety(false);
//     switch (@typeInfo(Scalar(Type))) {
//         else => @compileError(@typeName(Type)),
//         .int => |int| switch (int.signedness) {
//             .signed => {},
//             .unsigned => return @as(AddOneBit(Type), @max(lhs, rhs)) - @min(lhs, rhs),
//         },
//         .float => {},
//     }
//     return @as(AddOneBit(Type), lhs) - rhs;
// }
// test subUnsafe {
//     const test_sub_unsafe = binary(subUnsafe, .{});
//     try test_sub_unsafe.testInts();
//     try test_sub_unsafe.testFloats();
// }

// inline fn bitAnd(comptime Type: type, lhs: Type, rhs: Type) @TypeOf(lhs & rhs) {
//     return lhs & rhs;
// }
// test bitAnd {
//     const test_bit_and = binary(bitAnd, .{});
//     try test_bit_and.testInts();
// }

// inline fn bitNot(comptime Type: type, rhs: Type) @TypeOf(~rhs) {
//     return ~rhs;
// }
// test bitNot {
//     const test_bit_not = unary(bitNot, .{});
//     try test_bit_not.testInts();
// }

// inline fn min(comptime Type: type, lhs: Type, rhs: Type) Type {
//     return @min(lhs, rhs);
// }
// test min {
//     const test_min = binary(min, .{});
//     try test_min.testInts();
//     try test_min.testFloats();
// }

// inline fn max(comptime Type: type, lhs: Type, rhs: Type) Type {
//     return @max(lhs, rhs);
// }
// test max {
//     const test_max = binary(max, .{});
//     try test_max.testInts();
//     try test_max.testFloats();
// }

// inline fn clz(comptime Type: type, rhs: Type) @TypeOf(@clz(rhs)) {
//     return @clz(rhs);
// }
// test clz {
//     const test_clz = unary(clz, .{});
//     try test_clz.testInts();
// }

// inline fn ctz(comptime Type: type, rhs: Type) @TypeOf(@ctz(rhs)) {
//     return @ctz(rhs);
// }
// test ctz {
//     const test_ctz = unary(ctz, .{});
//     try test_ctz.testInts();
// }

// inline fn popCount(comptime Type: type, rhs: Type) @TypeOf(@popCount(rhs)) {
//     return @popCount(rhs);
// }
// test popCount {
//     const test_pop_count = unary(popCount, .{});
//     try test_pop_count.testInts();
// }

// inline fn abs(comptime Type: type, rhs: Type) @TypeOf(@abs(rhs)) {
//     return @abs(rhs);
// }
// test abs {
//     const test_abs = unary(abs, .{});
//     try test_abs.testInts();
//     try test_abs.testFloats();
// }
