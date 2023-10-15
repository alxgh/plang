const std = @import("std");
const env = @import("env.zig");
const Scanner = @import("Scanner.zig");
const Chunk = @import("Chunk.zig");
const Values = @import("Values.zig");
const Allocator = std.mem.Allocator;

const stderr = std.io.getStdErr().writer();

const Precedence = enum(u8) {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
};

const ParseFn = *const fn (*Self) anyerror!void;

const ParseRule = struct {
    prefix: ?ParseFn = null,
    infix: ?ParseFn = null,
    precedence: Precedence = .None,
};

fn getRule(op_t: Scanner.TokenType) *const ParseRule {
    return switch (op_t) {
        .LeftParen => &.{ .prefix = grouping },
        .Minus => &.{ .prefix = unary, .infix = binary, .precedence = .Term },
        .Plus => &.{ .infix = binary, .precedence = .Term },
        .Slash => &.{ .infix = binary, .precedence = .Factor },
        .Star => &.{ .infix = binary, .precedence = .Factor },
        .Num => &.{ .prefix = number },
        .False => &.{ .prefix = literal },
        .True => &.{ .prefix = literal },
        .Nil => &.{ .prefix = literal },
        .Bang => &.{ .prefix = unary },
        .Greater => &.{ .infix = binary, .precedence = .Comparison },
        .Gte => &.{ .infix = binary, .precedence = .Comparison },
        .Less => &.{ .infix = binary, .precedence = .Comparison },
        .Lte => &.{ .infix = binary, .precedence = .Comparison },
        .EqEq => &.{ .infix = binary, .precedence = .Equality },
        .BangEq => &.{ .infix = binary, .precedence = .Comparison },
        else => &.{},
    };
}

const Self = @This();

allocator: Allocator,
scanner: Scanner,
current: ?Scanner.Token = null,
prev: ?Scanner.Token = null,
chunk: *Chunk = undefined,

pub fn init(allocator: Allocator, source: []const u8) Self {
    return .{
        .allocator = allocator,
        .scanner = Scanner.init(allocator, source),
    };
}

pub fn deinit(self: *Self) void {
    self.scanner.deinit();
}

pub fn start(self: *Self, chunk: *Chunk) !void {
    self.chunk = chunk;
    try self.advance();
    try self.expression();
    try self.end();
}

fn end(self: *Self) !void {
    try self.emitOpByte(.Return);
    if (env.DebugPrintCode) {
        try self.chunk.disassmble("code");
    }
}

fn emitConst(self: *Self, val: Values.Value) !void {
    const c = try self.makeConst(val);

    try self.emitMulti(&[_]u8{ Chunk.OpCode.Constant.byte(), @as(u8, @intCast(c)) });
}

fn makeConst(self: *Self, val: Values.Value) !usize {
    const c = try self.chunk.addConstant(val);
    if (c > 255) {
        try self.err("Maximum number of consts in one chunk!", error.MaxConstReached);
    }
    return c;
}

fn advance(self: *Self) !void {
    self.prev = self.current;
    if (try self.scanner.next()) |token| {
        self.current = token;
    } else {
        try self.errAtCurrent("Unexpected EOF.", error.UnexpectedEof);
    }
}

fn consume(self: *Self, token_type: Scanner.TokenType, msg: []const u8) !void {
    if (self.current.?.t == token_type) {
        try self.advance();
        return;
    }
    try self.errAtCurrent(msg, error.UnexpectedToken);
    unreachable;
}

fn emitByte(self: *Self, byte: u8) !void {
    try self.chunk.write(byte, self.current.?.line);
}

fn emitOpByte(self: *Self, op: Chunk.OpCode) !void {
    try self.chunk.writeOp(op, self.current.?.line);
}

fn emitMulti(self: *Self, bytes: []const u8) !void {
    for (bytes) |byte| {
        try self.emitByte(byte);
    }

    // const t = @TypeOf(bytes);
    // const ti = @typeInfo(t);
    // if (ti != .Struct) {
    //     @compileError("emitMulti accpets struct.");
    // }
    // const fields = ti.Struct.fields;
    // inline for (fields) |field| {
    //     if (field.type == u8) {
    //         try self.emitByte(@field(bytes, field.name));
    //     } else if (field.type == Chunk.OpCode) {} else {
    //         @compileError("Invalid arg type, must be either 'u8' or 'Chunk.OpCode'");
    //     }
    // }
}

fn errAtCurrent(self: *Self, msg: []const u8, e: anyerror) anyerror!void {
    return self.errAt(msg, self.current.?, e);
}

fn err(self: *Self, msg: []const u8, e: anyerror) anyerror!void {
    return self.errAt(msg, self.prev.?, e);
}

fn errAt(self: *Self, msg: []const u8, token: Scanner.Token, e: anyerror) anyerror!void {
    _ = self;
    try stderr.print("Error at token: '{s}', line '{}': \n\t{s}\n", .{ token.lexeme, token.line, msg });
    return e;
}

fn expression(self: *Self) !void {
    try self.parsePrecedence(.Assignment);
}

fn number(self: *Self) !void {
    try self.emitConst(Values.numberValue(self.prev.?.literal.?.Number));
}

fn grouping(self: *Self) !void {
    try self.expression();
    try self.consume(.RightParen, "Expect ')' after expression.");
    return;
}

fn unary(self: *Self) !void {
    var prev_t = self.prev.?.t;

    try self.parsePrecedence(.Unary);

    try switch (prev_t) {
        .Minus => self.emitOpByte(.Negate),
        .Bang => self.emitOpByte(.Not),
        else => unreachable,
    };
}

fn parsePrecedence(self: *Self, p: Precedence) !void {
    try self.advance();
    var prefix_rule = getRule(self.prev.?.t).prefix;
    if (prefix_rule == null) {
        try self.err("Expect expression.", error.ExpectExpr);
        unreachable;
    }

    try prefix_rule.?(self);

    while (@intFromEnum(p) < @intFromEnum(getRule(self.current.?.t).precedence)) {
        try self.advance();
        var infix_rule = getRule(self.prev.?.t).infix;
        try infix_rule.?(self);
    }
}

fn binary(self: *Self) !void {
    var prev_t = self.prev.?.t;
    var rule = getRule(prev_t);
    try self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));
    try self.emitMulti(switch (prev_t) {
        .Plus => &[_]u8{Chunk.OpCode.Add.byte()},
        .Gte => &[_]u8{ Chunk.OpCode.Greater.byte(), Chunk.OpCode.Not.byte() },
        .Minus => &[_]u8{Chunk.OpCode.Subtract.byte()},
        .Star => &[_]u8{Chunk.OpCode.Multiply.byte()},
        .Slash => &[_]u8{Chunk.OpCode.Divide.byte()},
        .Greater => &[_]u8{Chunk.OpCode.Greater.byte()},
        .Less => &[_]u8{Chunk.OpCode.Less.byte()},
        .Lte => &[_]u8{ Chunk.OpCode.Less.byte(), Chunk.OpCode.Not.byte() },
        .Eq => &[_]u8{Chunk.OpCode.Equal.byte()},
        .BangEq => &[_]u8{ Chunk.OpCode.Equal.byte(), Chunk.OpCode.Not.byte() },
        else => unreachable,
    });
}

fn literal(self: *Self) !void {
    try self.emitOpByte(switch (self.prev.?.t) {
        .False => .False,
        .True => .True,
        .Nil => .Nil,
        else => unreachable,
    });
}
