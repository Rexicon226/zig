const std = @import("std");
const Allocator = std.mem.Allocator;

const stack_size = 32 * 1024 * 1024; // 32mib stack size taken from build.zig

const MemoryState = @This();

var stack_memory: [stack_size]u8 = undefined;
var fba = std.heap.FixedBufferAllocator.init(&stack_memory);
const allocator = fba.allocator();

undefined: std.ArrayListUnmanaged(usize) = .{},

pub fn init() !MemoryState {
    const ms: MemoryState = .{};
    return ms;
}

// Don't need to deinit as it's stack memory
pub fn deinit(ms: *MemoryState) void {
    ms.undefined.deinit(allocator);
}

pub fn trackUndefined(ms: *MemoryState, addr: usize) !void {
    @setRuntimeSafety(false);

    try ms.undefined.append(allocator, addr);
}

/// Removes an address from the tracked if found.
pub fn removeUndefined(ms: *MemoryState, addr: usize) void {
    @setRuntimeSafety(false);

    for (ms.undefined.items, 0..) |tracked, i| {
        if (addr == tracked) {
            _ = ms.undefined.orderedRemove(i);
        }
    }
}

pub fn checkUndefined(ms: *MemoryState, addr: usize) bool {
    @setRuntimeSafety(false);

    for (ms.undefined.items) |tracked| {
        if (addr == tracked) return true;
    }
    return false;
}
