//! Private roc-unicode test host.
//!
//! Reads all stdin, transfers it to `run! : Str => Str`, prints the returned
//! response, counts Roc allocations, and fails the process if allocations leak.

const std = @import("std");
const builtin = @import("builtin");
const abi = @import("roc_platform_abi.zig");

pub const std_options: std.Options = .{
    .allow_stack_tracing = false,
};

const HostEnv = struct {
    gpa: std.heap.DebugAllocator(.{}),
    roc_env: abi.RocEnv,
};

extern fn roc_run(input: abi.RocStr) callconv(.c) abi.RocStr;

var g_roc_host: ?*abi.RocHost = null;
var g_alloc_count: u64 = 0;

comptime {
    if (!builtin.is_test) {
        @export(&main, .{ .name = "main" });
        @export(&hostedAllocCount, .{ .name = "roc_host_alloc_count", .visibility = .hidden });
        @export(&hostAlloc, .{ .name = "roc_alloc", .visibility = .hidden });
        @export(&hostDealloc, .{ .name = "roc_dealloc", .visibility = .hidden });
        @export(&hostRealloc, .{ .name = "roc_realloc", .visibility = .hidden });
        @export(&hostDbg, .{ .name = "roc_dbg", .visibility = .hidden });
        @export(&hostExpectFailed, .{ .name = "roc_expect_failed", .visibility = .hidden });
        @export(&hostCrashed, .{ .name = "roc_crashed", .visibility = .hidden });

        if (builtin.os.tag == .windows) {
            @export(&__main, .{ .name = "__main" });
        }
    }
}

fn __main() callconv(.c) void {}

fn hostedAllocCount() callconv(.c) u64 {
    return g_alloc_count;
}

fn hostAlloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    g_alloc_count += 1;
    return abi.DefaultAllocators.rocAlloc(g_roc_host.?, length, alignment);
}

fn hostDealloc(ptr: *anyopaque, alignment: usize) callconv(.c) void {
    abi.DefaultAllocators.rocDealloc(g_roc_host.?, ptr, alignment);
}

fn hostRealloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    g_alloc_count += 1;
    return abi.DefaultAllocators.rocRealloc(g_roc_host.?, ptr, new_length, alignment);
}

fn hostDbg(bytes: [*]const u8, len: usize) callconv(.c) void {
    abi.DefaultHandlers.rocDbg(g_roc_host.?, bytes, len);
}

fn hostExpectFailed(bytes: [*]const u8, len: usize) callconv(.c) void {
    abi.DefaultHandlers.rocExpectFailed(g_roc_host.?, bytes, len);
}

fn hostCrashed(bytes: [*]const u8, len: usize) callconv(.c) void {
    abi.DefaultHandlers.rocCrashed(g_roc_host.?, bytes, len);
}

fn main(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    _ = argc;
    _ = argv;
    platformMain() catch |err| {
        std.debug.print("HOST ERROR: {s}\n", .{@errorName(err)});
        return 1;
    };
    return 0;
}

fn platformMain() !void {
    const io = std.Io.Threaded.global_single_threaded.io();
    var host_env = HostEnv{
        .gpa = std.heap.DebugAllocator(.{}){},
        .roc_env = undefined,
    };
    host_env.roc_env = .{
        .allocator = host_env.gpa.allocator(),
        .roc_io = abi.RocIo.default(),
    };

    var roc_host = abi.makeRocHost(&host_env.roc_env);
    g_roc_host = &roc_host;

    var stdin_buffer: [4096]u8 = undefined;
    var stdin_reader = std.Io.File.stdin().readerStreaming(io, &stdin_buffer);
    const input = try stdin_reader.interface.allocRemaining(host_env.gpa.allocator(), .unlimited);

    // Host-side input setup is deliberately excluded from app telemetry.
    const roc_input = abi.RocStr.fromSlice(input, &roc_host);
    g_alloc_count = 0;

    var result = roc_run(roc_input);
    const stdout = std.Io.File.stdout();
    try stdout.writeStreamingAll(io, result.asSlice());
    try stdout.writeStreamingAll(io, "\n");

    // Drop the result and input scratch data before checking for leaks.
    result.decref(&roc_host);
    result = abi.RocStr.empty();
    host_env.gpa.allocator().free(input);

    const leak_status = host_env.gpa.deinit();
    if (leak_status == .leak) {
        return error.RocAllocationLeak;
    }
}
