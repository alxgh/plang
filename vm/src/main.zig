const std = @import("std");
const Chunk = @import("Chunk.zig");
const VM = @import("VM.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var chunk = Chunk.init(allocator);
    defer chunk.deinit();
    var constant = try chunk.addConstant(1.2);
    try chunk.writeOp(.Constant, 1);
    try chunk.write(@intCast(constant), 18);
    try chunk.writeOp(.Negate, 25);
    try chunk.writeOp(.Constant, 1);
    try chunk.write(@intCast(constant), 18);
    try chunk.writeOp(.Divide, 25);
    try chunk.writeOp(.Return, 25);
    // try chunk.disassmble("test chunk");
    var vm = VM.init(allocator, &chunk);
    defer vm.deinit();
    try vm.interpret();
}
