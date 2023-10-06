const std = @import("std");
const Values = @import("Values.zig");
const Chunk = @import("Chunk.zig");
const env = @import("env.zig");

const stdout = std.io.getStdOut();
const stderr = std.io.getStdErr();

const StackSize = 256;

pub const Result = struct {};

pub const Error = error{
    Compile,
    Runtime,
};

pub const StackError = error{
    Full,
    Empty,
};

const Self = @This();

allocator: std.mem.Allocator,
chunk: *Chunk,
ip: usize,
stack: [StackSize]Values.Value = undefined,
stack_pointer: usize = 0,

pub fn init(allocator: std.mem.Allocator, chunk: *Chunk) Self {
    return .{
        .allocator = allocator,
        .chunk = chunk,
        .ip = 0,
    };
}

pub fn deinit(self: *Self) void {
    _ = self;
}

fn readByte(self: *Self) u8 {
    var byte = self.chunk.getByte(self.ip);
    self.ip += 1;
    return byte;
}

fn readOp(self: *Self) Chunk.OpCode {
    return @enumFromInt(self.readByte());
}

fn readConst(self: *Self) Values.Value {
    return self.chunk.constants.values.items[self.readByte()];
}

pub fn interpret(self: *Self) (std.os.WriteError || Error || StackError)!void {
    var writer = stdout.writer();
    while (self.ip < self.chunk.len()) {
        if (env.Debug) {
            try writer.print("===STACK===\n", .{});
            for (0..self.stack_pointer) |sp| {
                try writer.print("[{}]", .{self.stack[sp]});
            }
            try writer.print("\n===STACK-END===\n", .{});
            _ = try self.chunk.disInstr(self.ip);
        }
        var instruction = self.readByte();
        const op = @as(Chunk.OpCode, @enumFromInt(instruction));
        switch (op) {
            .Return => {
                // just return
                const ret = try self.pop();
                try writer.print("{}\n", .{ret});
                return;
            },
            .Constant => {
                var constant = self.readConst();
                try self.push(constant);
                try writer.print("{}\n", .{constant});
            },
            .Negate => {
                try self.push((try self.pop()) * -1);
            },
            .Add, .Subtract, .Multiply, .Divide => {
                const left = try self.pop();
                const right = try self.pop();
                try self.push(switch (op) {
                    .Add => left + right,
                    .Subtract => left - right,
                    .Multiply => left * right,
                    .Divide => left / right,
                    else => unreachable,
                });
            },
        }
    }
}

pub fn push(self: *Self, value: Values.Value) StackError!void {
    if (self.stack_pointer == StackSize) {
        return StackError.Full;
    }
    self.stack[self.stack_pointer] = value;
    self.stack_pointer += 1;
}

pub fn pop(self: *Self) StackError!Values.Value {
    if (self.stack_pointer == 0) {
        return StackError.Empty;
    }
    self.stack_pointer -= 1;
    return self.stack[self.stack_pointer];
}

pub fn resetStack(self: *Self) void {
    self.stack_pointer = 0;
}
