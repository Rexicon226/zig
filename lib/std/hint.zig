const std = @import("std.zig");
const builtin = @import("builtin");
const mem = std.mem;

/// Force an evaluation of the expression; this tries to prevent
/// the compiler from optimizing the computation away even if the
/// result eventually gets discarded.
// TODO: use @declareSideEffect() when it is available - https://github.com/ziglang/zig/issues/6168
pub fn doNotOptimizeAway(val: anytype) void {
    if (@inComptime()) return;

    const max_gp_register_bits = @bitSizeOf(c_long);
    const t = @typeInfo(@TypeOf(val));
    switch (t) {
        .Void, .Null, .ComptimeInt, .ComptimeFloat => return,
        .Enum => doNotOptimizeAway(@intFromEnum(val)),
        .Bool => doNotOptimizeAway(@intFromBool(val)),
        .Int => {
            const bits = t.Int.bits;
            if (bits <= max_gp_register_bits and builtin.zig_backend != .stage2_c) {
                const val2 = @as(
                    std.meta.Int(t.Int.signedness, @max(8, std.math.ceilPowerOfTwoAssert(u16, bits))),
                    val,
                );
                asm volatile (""
                    :
                    : [val2] "r" (val2),
                );
            } else doNotOptimizeAway(&val);
        },
        .Float => {
            if ((t.Float.bits == 32 or t.Float.bits == 64) and builtin.zig_backend != .stage2_c) {
                asm volatile (""
                    :
                    : [val] "rm" (val),
                );
            } else doNotOptimizeAway(&val);
        },
        .Pointer => {
            if (builtin.zig_backend == .stage2_c) {
                doNotOptimizeAwayC(val);
            } else {
                asm volatile (""
                    :
                    : [val] "m" (val),
                    : "memory"
                );
            }
        },
        .Array => {
            if (t.Array.len * @sizeOf(t.Array.child) <= 64) {
                for (val) |v| doNotOptimizeAway(v);
            } else doNotOptimizeAway(&val);
        },
        else => doNotOptimizeAway(&val),
    }
}

/// .stage2_c doesn't support asm blocks yet, so use volatile stores instead
var deopt_target: if (builtin.zig_backend == .stage2_c) u8 else void = undefined;
fn doNotOptimizeAwayC(ptr: anytype) void {
    const dest = @as(*volatile u8, @ptrCast(&deopt_target));
    for (mem.asBytes(ptr)) |b| {
        dest.* = b;
    }
    dest.* = 0;
}

/// Signals to the processor that the caller is inside a busy-wait spin-loop.
pub inline fn spinLoopHint() void {
    switch (builtin.target.cpu.arch) {
        // No-op instruction that can hint to save (or share with a hardware-thread)
        // pipelining/power resources
        // https://software.intel.com/content/www/us/en/develop/articles/benefitting-power-and-performance-sleep-loops.html
        .x86, .x86_64 => asm volatile ("pause" ::: "memory"),

        // No-op instruction that serves as a hardware-thread resource yield hint.
        // https://stackoverflow.com/a/7588941
        .powerpc64, .powerpc64le => asm volatile ("or 27, 27, 27" ::: "memory"),

        // `isb` appears more reliable for releasing execution resources than `yield`
        // on common aarch64 CPUs.
        // https://bugs.java.com/bugdatabase/view_bug.do?bug_id=8258604
        // https://bugs.mysql.com/bug.php?id=100664
        .aarch64, .aarch64_be, .aarch64_32 => asm volatile ("isb" ::: "memory"),

        // `yield` was introduced in v6k but is also available on v6m.
        // https://www.keil.com/support/man/docs/armasm/armasm_dom1361289926796.htm
        .arm, .armeb, .thumb, .thumbeb => {
            const can_yield = comptime std.Target.arm.featureSetHasAny(builtin.target.cpu.features, .{
                .has_v6k, .has_v6m,
            });
            if (can_yield) {
                asm volatile ("yield" ::: "memory");
            } else {
                asm volatile ("" ::: "memory");
            }
        },
        // Memory barrier to prevent the compiler from optimizing away the spin-loop
        // even if no hint_instruction was provided.
        else => asm volatile ("" ::: "memory"),
    }
}

test doNotOptimizeAway {
    comptime doNotOptimizeAway("test");

    doNotOptimizeAway(null);
    doNotOptimizeAway(true);
    doNotOptimizeAway(0);
    doNotOptimizeAway(0.0);
    doNotOptimizeAway(@as(u1, 0));
    doNotOptimizeAway(@as(u3, 0));
    doNotOptimizeAway(@as(u8, 0));
    doNotOptimizeAway(@as(u16, 0));
    doNotOptimizeAway(@as(u32, 0));
    doNotOptimizeAway(@as(u64, 0));
    doNotOptimizeAway(@as(u128, 0));
    doNotOptimizeAway(@as(u13, 0));
    doNotOptimizeAway(@as(u37, 0));
    doNotOptimizeAway(@as(u96, 0));
    doNotOptimizeAway(@as(u200, 0));
    doNotOptimizeAway(@as(f32, 0.0));
    doNotOptimizeAway(@as(f64, 0.0));
    doNotOptimizeAway([_]u8{0} ** 4);
    doNotOptimizeAway([_]u8{0} ** 100);
    doNotOptimizeAway(@as(std.builtin.Endian, .little));
}

test spinLoopHint {
    for (0..10) |_| {
        spinLoopHint();
    }
}
