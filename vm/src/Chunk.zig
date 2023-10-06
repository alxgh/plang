const std = @import("std");
const Values = @import("Values.zig");

pub const OpCode = enum(u8) {
    Return = 0,
    Constant,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
};

const Self = @This();
const Code = std.ArrayList(u8);
const Lines = std.ArrayList(u64);
const Allocator = std.mem.Allocator;
const stdout = std.io.getStdOut();
const stderr = std.io.getStdErr();

code: Code,
allocator: Allocator,
constants: Values,
lines: Lines,

pub fn init(allocator: Allocator) Self {
    return .{
        .allocator = allocator,
        .code = Code.init(allocator),
        .constants = Values.init(allocator),
        .lines = Lines.init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.code.deinit();
    self.lines.deinit();
    self.constants.deinit();
}

pub fn len(self: *Self) usize {
    return self.code.items.len;
}

pub fn getByte(self: *Self, idx: usize) u8 {
    return self.code.items[idx];
}

pub fn write(self: *Self, byte: u8, line: u64) Allocator.Error!void {
    try self.code.append(byte);
    try self.lines.append(line);
}

pub fn writeOp(self: *Self, byte: OpCode, line: u64) Allocator.Error!void {
    return self.write(@intFromEnum(byte), line);
}

pub fn addConstant(self: *Self, value: Values.Value) !usize {
    return self.constants.write(value);
}

pub fn disassmble(self: *Self, name: []const u8) !void {
    try stdout.writer().print("Running program: {s}\n", .{name});
    var i: usize = 0;
    while (i < self.code.items.len) {
        i = try self.disInstr(i);
    }
}

pub fn disInstr(self: *Self, offset: usize) !usize {
    try stdout.writer().print("offset: {d:0>4} = ", .{offset});
    if (offset > 0 and self.lines.items[offset] == self.lines.items[offset - 1]) {
        try stdout.writer().print("   | ", .{});
    } else {
        try stdout.writer().print("{d:0>4} ", .{self.lines.items[offset]});
    }
    switch (@as(OpCode, @enumFromInt(self.code.items[offset]))) {
        .Return => {
            //simpleInstruction
            try stdout.writer().print("OP_RETURN\n", .{});
            return offset + 1;
        },
        .Negate, .Add, .Subtract, .Multiply, .Divide => |v| {
            //simpleInstruction
            try stdout.writer().print("{s}\n", .{@tagName(v)});
            return offset + 1;
        },
        .Constant => {
            // constantInstr
            const constant = self.code.items[offset + 1];
            try stdout.writer().print("OP_CONSTANT: {d:0>4} ", .{offset});
            try stdout.writer().print("{d}", .{self.constants.values.items[@as(usize, constant)]});
            try stdout.writer().print("\n", .{});
            return offset + 2;
        },
    }
}
