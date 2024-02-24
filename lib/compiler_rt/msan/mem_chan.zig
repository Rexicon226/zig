const std = @import("std");
const MemoryState = @import("MemoryState.zig");

// The global MemoryState. (rip globals)
var ms: MemoryState = MemoryState.init() catch
    @panic("failed to initialize memchan MemoryState");

const log = std.log.scoped(.memchan);

/// Adds the address as a known undefined.
pub fn trackUndefined(addr: usize) callconv(.C) void {
    @setRuntimeSafety(false);

    log.err("Tracking {x}", .{addr});
    ms.trackUndefined(addr) catch @panic("OOM");
}

/// Removes an address from the tracked undefined values. Return true if it was removed.
/// and false if it wasn't found.
pub fn removeUndefined(addr: usize) callconv(.C) void {
    @setRuntimeSafety(false);

    ms.removeUndefined(addr);
}

/// Returns `false` if the address is undefined.
pub fn checkUndefined(addr: usize) callconv(.C) void {
    @setRuntimeSafety(false);

    log.err("Checking {x}", .{addr});
    const is_undefined = ms.checkUndefined(addr);
    if (is_undefined) {
        log.err("attempted to dereference undefined pointer", .{});
        std.os.exit(1);
    }
}

comptime {
    // NOTE: what if we make the linkage weak? maybe the user wants to override the memory sanatizer?
    @export(trackUndefined, .{ .name = "__memchan_trackUndefined", .linkage = .Strong });
    @export(removeUndefined, .{ .name = "__memchan_removeUndefined", .linkage = .Strong });
    @export(checkUndefined, .{ .name = "__memchan_checkUndefined", .linkage = .Strong });
}
