const MaxJump = 2 ^ 16;
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
        // .Or => &.{ .infix = or_, .precedence = .Or },
        // .And => &.{ .infix = and_, .precedence = .And },
        else => &.{},
    };
}

const Local = struct {
    name: Scanner.Token,
    depth: i64,
};

const Self = @This();

allocator: Allocator,
scanner: Scanner,
current: ?Scanner.Token = null,
prev: ?Scanner.Token = null,
chunk: *Chunk = undefined,
locals: [256]Local = undefined,
local_cnt: usize = 0,
scope_depth: i64 = 0,

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
    } else if (try self.match(.If)) {
        return self.ifStmt();
    }
    return self.statement();
}

fn ifStmt(self: *Self) !void {
    try self.consume(.LeftParen, "Expect '(' after 'if'.");
    try self.expression();
    try self.consume(.RightParen, "Expect ')' after condition.");
    var jump = try self.emitJump(.JumpIfFalse);
    try self.statement();
    try self.patchJump(jump);
}

fn emitJump(self: *Self, jump_type: Chunk.OpCode) !usize {
    try self.emitMulti(&[_]u8{ jump_type.byte(), 0xff, 0xff });
    return self.chunk.code.items.len - 2;
}

fn patchJump(self: *Self, offset: usize) !void {
    const jump = self.chunk.code.items.len - offset - 2;
    if (jump > MaxJump) {
        return error.MaxJumpLines;
    }
    self.chunk.code.items[offset] = @as(u8, @intCast(jump >> 8)) & 0xff;
    self.chunk.code.items[offset + 1] = @as(u8, @intCast(jump)) & 0xff;
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
    try self.declVar();
    if (self.scope_depth > 0) return 0;
    return self.idenConst(self.prev.?);
}

fn declVar(self: *Self) !void {
    if (self.scope_depth == 0) return;
    var name = self.prev.?;
    try self.addLocal(name);
}

fn addLocal(self: *Self, name: Scanner.Token) !void {
    if (self.local_cnt == 256) {
        return error.TooManyLocalVarsInBlock;
    }
    var i = @as(i64, @intCast(self.local_cnt)) - 1;
    while (i >= 0) : (i -= 1) {
        const local = self.locals[@intCast(i)];
        if (local.depth != -1 and local.depth < self.scope_depth) {
            break;
        }
        if (std.mem.eql(u8, local.name.lexeme, name.lexeme)) {
            return error.CannotRedefineVariableInSameScope;
        }
    }
    var local = Local{
        .name = name,
        .depth = -1,
    };
    self.locals[self.local_cnt] = local;
    self.local_cnt += 1;
}
fn beginScope(self: *Self) void {
    self.scope_depth += 1;
}

fn endScope(self: *Self) !void {
    self.scope_depth -= 1;
    while (self.local_cnt > 0 and self.locals[self.local_cnt - 1].depth > self.scope_depth) {
        try self.emitOpByte(.Pop);
        if (self.local_cnt == 0) {
            break;
        }
        self.local_cnt -= 1;
    }
}

fn statement(self: *Self) !void {
    if (try self.match(.Print)) {
        try self.printStmt();
    } else if (try self.match(.LeftBrace)) {
        self.beginScope();
        try self.block();
        try self.endScope();
    } else {
        try self.expressionStmt();
    }
}

fn block(self: *Self) !void {
    while (!self.check(.RightBrace) and !self.check(.EOF)) {
        try self.declaration();
    }
    try self.consume(.RightBrace, "Expect '}' after block.");
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
    var get_op: Chunk.OpCode = .GetLocal;
    var set_op: Chunk.OpCode = .SetLocal;
    var arg = try self.resolveLocal(tok);
    if (arg == -1) {
        arg = @intCast(try self.idenConst(tok));
        get_op = .GetGlobal;
        set_op = .SetGlobal;
    }
    if (can_assign and try self.match(.Eq)) {
        try self.expression();
        try self.emitMulti(&[_]u8{ set_op.byte(), @intCast(arg) });
    } else {
        try self.emitMulti(&[_]u8{ get_op.byte(), @intCast(arg) });
    }
}

fn resolveLocal(self: *Self, name: Scanner.Token) !i64 {
    var i = @as(i64, @intCast(self.local_cnt)) - 1;
    while (i >= 0) : (i -= 1) {
        var local = self.locals[@intCast(i)];
        if (std.mem.eql(u8, local.name.lexeme, name.lexeme)) {
            if (local.depth == -1) {
                return error.CantUseInItsOwnInitializer;
            }
            return i;
        }
    }
    return -1;
}

fn string(self: *Self, can_assign: bool) !void {
    _ = can_assign;
    _ = try self.emitConst(Values.objValue(try self.chunk.allocStr(self.prev.?.literal.?.String)));
}

fn idenConst(self: *Self, t: Scanner.Token) !usize {
    return self.makeConst(Values.objValue(try self.chunk.allocStr(t.lexeme)));
}

fn defGlobalVar(self: *Self, global: u8) !void {
    if (self.scope_depth > 0) {
        self.locals[self.local_cnt - 1].depth = self.scope_depth;
        return;
    }
    try self.emitMulti(&[_]u8{ Chunk.OpCode.DefineGlobal.byte(), global });
}
