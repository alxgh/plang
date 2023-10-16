const std = @import("std");
const Chunk = @import("Chunk.zig");
const VM = @import("VM.zig");

const stderr = std.io.getStdErr();

pub fn main() !void {
    var args = std.process.args();
    // try chunk.disassmble("test chunk");
    var len = args.inner.count;
    if (len > 2) {
        try stderr.writer().print("Usage: plan [script]", .{});
    }
    _ = args.skip();
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        _ = gpa.deinit();
    }

    var vm = VM.init(allocator);
    defer vm.deinit();
    if (args.next()) |file_name| {
        try runFile(allocator, &vm, file_name);
    } else {
        try repl(allocator, &vm);
    }
}

fn runFile(allocator: std.mem.Allocator, vm: *VM, file_name: []const u8) !void {
    var file = try std.fs.cwd().openFile(file_name, .{});
    var data = try file.reader().readAllAlloc(allocator, 1000 * 1000 * 1000);
    defer allocator.free(data);
    try vm.interpret(data);
}

fn repl(allocator: std.mem.Allocator, vm: *VM) !void {
    _ = vm;
    _ = allocator;
}
