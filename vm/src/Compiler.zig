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

const ParseFn = *const fn (*Self, can_assign: bool) anyerror!void;

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
        .Str => &.{ .prefix = string },
        .Iden => &.{ .prefix = variable },
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
    while (!self.check(.EOF)) {
        try self.declaration();
    }
    try self.end();
}

fn end(self: *Self) !void {
    try self.emitOpByte(.Return);
    if (env.DebugPrintCode) {
        try self.chunk.disassmble("code");
    }
}

fn emitConst(self: *Self, val: Values.Value) !usize {
    const c = try self.makeConst(val);

    try self.emitMulti(&[_]u8{ Chunk.OpCode.Constant.byte(), @as(u8, @intCast(c)) });

    return c;
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

fn match(self: *Self, t: Scanner.TokenType) !bool {
    if (!self.check(t)) {
        return false;
    }
    try self.advance();
    return true;
}

fn check(self: *Self, t: Scanner.TokenType) bool {
    return self.current.?.t == t;
}

fn emitByte(self: *Self, byte: u8) !void {
    try self.chunk.write(byte, self.prev.?.line);
}

fn emitOpByte(self: *Self, op: Chunk.OpCode) !void {
    try self.chunk.write(op.byte(), self.prev.?.line);
}

fn emitMulti(self: *Self, bytes: []const u8) !void {
    for (bytes) |byte| {
        try self.emitByte(byte);
    }
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

fn declaration(self: *Self) !void {
    if (try self.match(.Var)) {
        return self.varDecl();
    }
    return self.statement();
}

fn varDecl(self: *Self) !void {
    var global = try self.parseVar("Expect variable name");
    if (try self.match(.Eq)) {
        try self.expression();
    } else {
        try self.emitOpByte(.Nil);
    }
    try self.consume(.Semicolon, "Expect ';' after variable declaration");
    try self.defGlobalVar(@intCast(global));
}

fn parseVar(self: *Self, errMsg: []const u8) !usize {
    try self.consume(.Iden, errMsg);
    return self.idenConst(self.prev.?);
}

fn statement(self: *Self) !void {
    if (try self.match(.Print)) {
        try self.printStmt();
    } else {
        try self.expressionStmt();
    }
}

fn printStmt(self: *Self) !void {
    try self.expression();
    try self.consume(.Semicolon, "Expect ';' after value.");
    try self.emitOpByte(.Print);
}

fn expressionStmt(self: *Self) !void {
    try self.expression();
    try self.consume(.Semicolon, "Expect ';' after expression.");
    try self.emitOpByte(.Pop);
}

fn expression(self: *Self) !void {
    try self.parsePrecedence(.Assignment);
}

fn number(self: *Self, can_assign: bool) !void {
    _ = can_assign;
    _ = try self.emitConst(Values.numberValue(self.prev.?.literal.?.Number));
}

fn grouping(self: *Self, can_assign: bool) !void {
    _ = can_assign;
    try self.expression();
    try self.consume(.RightParen, "Expect ')' after expression.");
    return;
}

fn unary(self: *Self, can_assign: bool) !void {
    _ = can_assign;
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

    var can_assign = @intFromEnum(p) <= @intFromEnum(Precedence.Assignment);

    try prefix_rule.?(self, can_assign);

    while (@intFromEnum(p) < @intFromEnum(getRule(self.current.?.t).precedence)) {
        try self.advance();
        var infix_rule = getRule(self.prev.?.t).infix;
        try infix_rule.?(self, can_assign);
    }
    if (can_assign and try self.match(.Eq)) {
        return error.InvalidAssignment;
    }
}

fn binary(self: *Self, can_assign: bool) !void {
    _ = can_assign;
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
        .EqEq => &[_]u8{Chunk.OpCode.Equal.byte()},
        .BangEq => &[_]u8{ Chunk.OpCode.Equal.byte(), Chunk.OpCode.Not.byte() },
        else => unreachable,
    });
}

fn literal(self: *Self, can_assign: bool) !void {
    _ = can_assign;
    try self.emitOpByte(switch (self.prev.?.t) {
        .False => .False,
        .True => .True,
        .Nil => .Nil,
        else => unreachable,
    });
}

fn variable(self: *Self, can_assign: bool) !void {
    try self.namedVar(self.prev.?, can_assign);
}

fn namedVar(self: *Self, tok: Scanner.Token, can_assign: bool) !void {
    const arg = try self.idenConst(tok);
    if (can_assign and try self.match(.Eq)) {
        try self.expression();
        try self.emitMulti(&[_]u8{ Chunk.OpCode.SetGlobal.byte(), @intCast(arg) });
    } else {
        try self.emitMulti(&[_]u8{ Chunk.OpCode.GetGlobal.byte(), @intCast(arg) });
    }
}

fn string(self: *Self, can_assign: bool) !void {
    _ = can_assign;
    _ = try self.emitConst(Values.objValue(try self.chunk.allocStr(self.prev.?.literal.?.String)));
}

fn idenConst(self: *Self, t: Scanner.Token) !usize {
    return self.makeConst(Values.objValue(try self.chunk.allocStr(t.lexeme)));
}

fn defGlobalVar(self: *Self, global: u8) !void {
    try self.emitMulti(&[_]u8{ Chunk.OpCode.DefineGlobal.byte(), global });
}
