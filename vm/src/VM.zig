const std = @import("std");
const Values = @import("Values.zig");
const Chunk = @import("Chunk.zig");
const Compiler = @import("Compiler.zig");
const env = @import("env.zig");

const stdout = std.io.getStdOut();
const stderr = std.io.getStdErr();

const StackSize = 256;

pub const Result = struct {};

pub const Error = error{
    Runtime,
    InvalidChunk,
    InvalidOperand,
};

pub const StackError = error{
    Full,
    Empty,
};

const Self = @This();

allocator: std.mem.Allocator,
chunk: ?*Chunk = null,
ip: usize,
stack: [StackSize]Values.Value = undefined,
stack_pointer: usize = 0,

pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .allocator = allocator,
        .ip = 0,
    };
}

pub fn deinit(self: *Self) void {
    _ = self;
}

fn readByte(self: *Self) u8 {
    var byte = self.chunk.?.getByte(self.ip);
    self.ip += 1;
    return byte;
}

fn readOp(self: *Self) Chunk.OpCode {
    return @enumFromInt(self.readByte());
}

fn readConst(self: *Self) Values.Value {
    return self.chunk.?.constants.values.items[self.readByte()];
}

pub fn interpret(self: *Self, source: []const u8) !void {
    var compiler = Compiler.init(self.allocator, source);
    defer compiler.deinit();
    var chunk = Chunk.init(self.allocator);
    defer chunk.deinit();
    try compiler.start(&chunk);
    self.chunk = &chunk;
    self.run() catch |err| {
        return err;
    };
}

const RunError = (std.os.WriteError || Error || StackError);

pub fn run(self: *Self) RunError!void {
    if (self.chunk == null) {
        return Error.InvalidChunk;
    }
    var writer = stdout.writer();
    var chunk = self.chunk.?;
    while (self.ip < chunk.len()) {
        if (env.DebugExecutionTrace) {
            try writer.print("===STACK===\n", .{});
            for (0..self.stack_pointer) |sp| {
                try writer.print("[{}]", .{self.stack[sp]});
            }
            try writer.print("\n===STACK-END===\n", .{});
            _ = try chunk.disInstr(self.ip);
        }
        var instruction = self.readByte();
        const op = @as(Chunk.OpCode, @enumFromInt(instruction));
        switch (op) {
            .Return => {
                // just return
                const ret: Values.Value = self.pop() catch .{ .Number = 0 };
                try writer.print("{}\n", .{ret});
                return;
            },
            .Constant => {
                var constant = self.readConst();
                try self.push(constant);
                try writer.print("{}\n", .{constant});
            },
            .Negate => {
                const v = try self.pop();
                if (v != .Number) {
                    return Error.InvalidOperand;
                }
                try self.push(Values.numberValue((v.Number) * -1));
            },
            .Add, .Subtract, .Multiply, .Divide => {
                const right_v = try self.pop();
                const left_v = try self.pop();
                if (right_v != .Number or left_v != .Number) {
                    return Error.InvalidOperand;
                }

                const left = left_v.Number;
                const right = right_v.Number;
                try self.push(Values.numberValue(switch (op) {
                    .Add => left + right,
                    .Subtract => left - right,
                    .Multiply => left * right,
                    .Divide => left / right,
                    else => unreachable,
                }));
            },
            .False, .True => {
                try self.push(Values.boolValue(switch (op) {
                    .True => true,
                    .False => false,
                    else => unreachable,
                }));
            },
            .Nil => {
                try self.push(Values.numberValue(0));
            },
            .Not => {
                const v = try self.pop();
                if (v != .Bool) {
                    return Error.InvalidOperand;
                }
                try self.push(Values.boolValue(!v.Bool));
            },
            .Greater => {
                const right_v = try self.pop();
                const left_v = try self.pop();
                if (left_v != .Number or right_v != .Number) {
                    return Error.InvalidOperand;
                }
                try self.push(Values.boolValue(left_v.Number > right_v.Number));
            },
            .Less => {
                const right_v = try self.pop();
                const left_v = try self.pop();
                if (left_v != .Number or right_v != .Number) {
                    return Error.InvalidOperand;
                }
                try self.push(Values.boolValue(left_v.Number < right_v.Number));
            },
            .Equal => {
                const right_v = try self.pop();
                const left_v = try self.pop();
                if (left_v != .Number or right_v != .Number) {
                    return Error.InvalidOperand;
                }
                try self.push(Values.boolValue(left_v.Number == right_v.Number));
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
