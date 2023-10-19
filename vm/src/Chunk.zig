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
    Nil,
    True,
    False,
    Not,
    Equal,
    Greater,
    Less,

    Print,
    Pop,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,

    // JumpIfTrue,
    JumpIfFalse,

    pub fn byte(oc: OpCode) u8 {
        return @intFromEnum(oc);
    }
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
obj: ?*Values.Object = null,

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
    var obj = self.obj;
    while (true) {
        if (obj) |o| {
            switch (o.value) {
                .String => |str| {
                    self.allocator.free(str);
                },
            }
            obj = o.parent;
            self.allocator.destroy(o);
            continue;
        }
        break;
    }
}

pub fn len(self: *Self) usize {
    return self.code.items.len;
}

pub fn getByte(self: *Self, idx: usize) u8 {
    return self.code.items[idx];
}

pub fn getu16(self: *Self, offset: usize) u16 {
    const upper = self.code.items[offset];
    const lower = self.code.items[offset + 1];
    return @intCast((@as(u16, @intCast(upper)) << 8) | lower);
}

pub fn write(self: *Self, byte: u8, line: u64) Allocator.Error!void {
    try self.code.append(byte);
    try self.lines.append(line);
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
        .Constant, .DefineGlobal, .GetGlobal, .SetGlobal => |v| {
            // constantInstr
            const constant = self.code.items[offset + 1];
            try stdout.writer().print("{}: {d:0>4} ", .{ v, offset });
            try Values.print(stdout.writer(), self.constants.values.items[@as(usize, constant)]);
            return offset + 2;
        },
        .Negate, .Add, .Subtract, .Multiply, .Divide, .Nil, .True, .False, .Not, .Pop, .Greater, .Less, .Equal, .Print => |v| {
            try stdout.writer().print("{s}\n", .{@tagName(v)});
            return offset + 1;
        },
        .GetLocal, .SetLocal => |v| {
            const slot = self.code.items[offset + 1];
            try stdout.writer().print("{}: {d:0>4}\n", .{ v, slot });
            return offset + 2;
        },
        .JumpIfFalse => |v| {
            const addr = offset + 3 + self.getu16(offset + 1);
            try stdout.writer().print("{}: {d:0>4}\n", .{ v, addr });
            return offset + 3;
        },
    }
}

pub fn allocObj(self: *Self) !*Values.Object {
    var obj = try self.allocator.create(Values.Object);
    var po = self.obj;
    obj.*.parent = po;
    self.obj = obj;
    return obj;
}

pub fn allocStr(self: *Self, str: Values.StringObject) !*Values.Object {
    var obj = try self.allocObj();
    var strObj = try self.allocator.alloc(u8, str.len);
    std.mem.copy(u8, strObj, str);
    obj.*.value.String = strObj;
    return obj;
}
