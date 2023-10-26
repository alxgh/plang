const std = @import("std");
const Values = @import("Values.zig");
const Chunk = @import("Chunk.zig");
const Objects = @import("Objects.zig");
const Scanner = @import("Scanner.zig");
const Compiler = @import("Compiler.zig");
const util = @import("./util.zig");
const env = @import("env.zig");

const stdout = std.io.getStdOut();
const stderr = std.io.getStdErr();

const MaxFrames = 64;
const StackSize = MaxFrames * 256;

pub const Result = struct {};

pub const Error = error{
    Runtime,
    InvalidChunk,
    InvalidOperand,
    UnsupportedOperation,
    InvalidByte,
    UndefinedVariable,
};

pub const StackError = error{
    Full,
    Empty,
};

const CallFrame = struct {
    function: Values.FunctionObject,
    ip: usize = 0,
    vm_stack_idx: usize = 0,
};

const Self = @This();

allocator: std.mem.Allocator,
// chunk: ?*Chunk = null,
// ip: usize,
stack: [StackSize]Values.Value = undefined,
stack_pointer: usize = 0,
globals: std.StringHashMap(Values.Value),
objects: Objects,
call_frames: [MaxFrames]CallFrame = undefined,
frame_cnt: usize = 0,
current_frame: *CallFrame = undefined,

pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .allocator = allocator,
        // .ip = 0,
        .globals = std.StringHashMap(Values.Value).init(allocator),
        .objects = Objects.init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.globals.deinit();
    self.objects.deinit();
}

fn readByte(self: *Self) u8 {
    var byte = self.current_frame.function.chunk.getByte(self.current_frame.ip);
    self.current_frame.ip += 1;
    return byte;
}

fn readu16(self: *Self) u16 {
    var n = self.current_frame.function.chunk.getu16(self.current_frame.ip);
    self.current_frame.ip += 2;
    return n;
}

fn readOp(self: *Self) Chunk.OpCode {
    return @enumFromInt(self.readByte());
}

fn readConst(self: *Self) Values.Value {
    return self.current_frame.function.chunk.constants.values.items[self.readByte()];
}

pub fn interpret(self: *Self, source: []const u8) !void {
    var scanner = Scanner.init(self.allocator, source);
    defer scanner.deinit();
    var compiler = try Compiler.init(self.allocator, &scanner, &self.objects, .Script, "");
    defer compiler.deinit();
    var chunk = Chunk.init(self.allocator);
    defer chunk.deinit();
    var function = try compiler.start();
    try self.push(.{
        .Object = function,
    });
    try self.pushCallFrame(.{
        .function = function.value.Function,
    });
    return self.run();
}

pub fn pushCallFrame(self: *Self, frame: CallFrame) StackError!void {
    if (self.stack_pointer == StackSize) {
        return StackError.Full;
    }
    self.call_frames[self.frame_cnt] = frame;
    self.frame_cnt += 1;
}

pub fn popCallFrame(self: *Self) StackError!CallFrame {
    if (self.frame_cnt == 0) {
        return StackError.Empty;
    }
    self.frame_cnt -= 1;
    return self.call_frames[self.frame_cnt];
}
const RunError = (std.os.WriteError || Error || StackError || std.mem.Allocator.Error);

pub fn run(self: *Self) RunError!void {
    self.current_frame = &self.call_frames[self.frame_cnt - 1];
    var writer = stdout.writer();
    while (self.current_frame.ip < self.current_frame.function.chunk.len()) {
        if (env.DebugExecutionTrace) {
            try writer.print(util.Color.Green.bgWrap("===STACK==="), .{});
            for (0..self.stack_pointer) |sp| {
                try writer.print("\n{}", .{self.stack[sp]});
            }
            try writer.print("\n" ++ util.Color.Green.bgWrap("===STACK-END===") ++ "\n", .{});

            try writer.print("\n" ++ util.Color.Cyan.bgWrap("===GLOBAL-VARS===") ++ "\n", .{});
            var it = self.globals.iterator();
            while (it.next()) |e| {
                try writer.print(util.Color.Magenta.wrap("Name") ++ ": {s}, " ++ util.Color.Magenta.wrap("Value") ++ ": {}\n", .{ e.key_ptr.*, e.value_ptr.* });
            }
            try writer.print("\n" ++ util.Color.Cyan.bgWrap("===GLOBAL-VARS-END===") ++ "\n", .{});

            _ = try self.current_frame.function.chunk.disInstr(self.current_frame.ip);
        }
        var instruction = self.readByte();
        const op = @as(Chunk.OpCode, @enumFromInt(instruction));
        switch (op) {
            .Pop => {
                try self.popAndIgnore();
            },
            .Return => {
                // just return
                self.popAndIgnore() catch {};
                return;
            },
            .Constant => {
                var constant = self.readConst();
                try self.push(constant);
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

                if (@intFromEnum(left_v) != @intFromEnum(right_v)) {
                    return Error.InvalidOperand;
                }
                switch (left_v) {
                    .Number => |left| {
                        const right = right_v.Number;
                        try self.push(Values.numberValue(switch (op) {
                            .Add => left + right,
                            .Subtract => left - right,
                            .Multiply => left * right,
                            .Divide => left / right,
                            else => unreachable,
                        }));
                    },
                    .Object => |obj| {
                        if (@intFromEnum(left_v.Object.value) != @intFromEnum(right_v.Object.value)) {
                            return Error.InvalidOperand;
                        }
                        switch (obj.value) {
                            .String => {
                                var str = try self.allocator.alloc(u8, left_v.Object.value.String.len + right_v.Object.value.String.len);
                                std.mem.copy(u8, str[0..left_v.Object.value.String.len], left_v.Object.value.String);
                                std.mem.copy(u8, str[left_v.Object.value.String.len..], right_v.Object.value.String);
                                var object = try self.objects.allocObj();
                                object.*.value.String = str;
                                try self.push(Values.objValue(object));
                            },
                            else => unreachable,
                        }
                    },
                    else => return Error.UnsupportedOperation,
                }
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
                if (@intFromEnum(left_v) != @intFromEnum(right_v)) {
                    return Error.InvalidOperand;
                }
                switch (left_v) {
                    .Number => {
                        try self.push(Values.boolValue(left_v.Number == right_v.Number));
                    },
                    .Object => |obj| {
                        if (@intFromEnum(left_v.Object.value) != @intFromEnum(right_v.Object.value)) {
                            return Error.InvalidOperand;
                        }
                        switch (obj.value) {
                            .String => {
                                try self.push(Values.boolValue(std.mem.eql(u8, left_v.Object.value.String, right_v.Object.value.String)));
                            },
                            .Function => return Error.InvalidOperand,
                        }
                    },
                    else => return Error.UnsupportedOperation,
                }
            },
            .Print => {
                const ret: Values.Value = self.pop() catch .{ .Number = 0 };
                try Values.print(writer, ret);
            },
            .DefineGlobal => {
                const name = self.readConst();
                if (name != .Object or name.Object.value != .String) {
                    return Error.InvalidByte;
                }
                const value = try self.pop();
                try self.globals.put(name.Object.value.String, value);
            },
            .GetGlobal => {
                const name = self.readConst();
                var value: ?Values.Value = null;
                if (name != .Object or name.Object.value != .String) {
                    return Error.InvalidByte;
                }
                value = self.globals.get(name.Object.value.String);
                if (value) |v| {
                    try self.push(v);
                } else {
                    return Error.UndefinedVariable;
                }
            },
            .SetGlobal => {
                const name = self.readConst();
                if (name != .Object or name.Object.value != .String) {
                    return Error.InvalidByte;
                }
                const var_name = name.Object.value.String;
                if (!self.globals.contains(var_name)) {
                    return Error.UndefinedVariable;
                }
                try self.globals.put(var_name, try self.peek(0));
            },
            .SetLocal => {
                const slot = self.readByte();
                const idx = self.current_frame.vm_stack_idx + @as(usize, @intCast(slot));
                self.stack[idx] = try self.peek(0);
            },
            .GetLocal => {
                const slot = self.readByte();
                const idx = self.current_frame.vm_stack_idx + @as(usize, @intCast(slot));
                try self.push(self.stack[idx]);
            },
            .Jump => {
                var offset = self.readu16();
                self.current_frame.ip += offset;
            },
            .JumpIfFalse => {
                var offset = self.readu16();
                const e = try self.peek(0);
                if (e != .Bool or e.Bool == false) {
                    self.current_frame.ip += offset;
                }
            },
            .BackJump => {
                var offset = self.readu16();
                self.current_frame.ip -= offset;
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

pub fn peek(self: *Self, n: usize) StackError!Values.Value {
    if (self.stack_pointer == 0) {
        return StackError.Empty;
    }
    var pp = self.stack_pointer - (1 + n);
    if (pp < 0) {
        return StackError.Empty;
    }
    return self.stack[pp];
}

pub fn popAndIgnore(self: *Self) StackError!void {
    _ = try self.pop();
}

pub fn resetStack(self: *Self) void {
    self.stack_pointer = 0;
}
