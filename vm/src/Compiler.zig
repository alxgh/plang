const MaxJump = 65536;
const std = @import("std");
const env = @import("env.zig");
const Scanner = @import("Scanner.zig");
const Chunk = @import("Chunk.zig");
const Values = @import("Values.zig");
const Objects = @import("Objects.zig");
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
        .LeftParen => &.{ .prefix = grouping, .infix = call, .precedence = .Call },
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
        .Or => &.{ .infix = or_, .precedence = .Or },
        .And => &.{ .infix = and_, .precedence = .And },
        .Break => &.{ .prefix = break_ },
        .Continue => &.{ .prefix = continue_ },
        else => &.{},
    };
}

const Local = struct {
    name: Scanner.Token,
    depth: i64,
};

const Self = @This();

const JumpList = struct {
    scope: i64,
    offset: usize,
};

pub const FunctionType = enum {
    Function,
    Script,
};

allocator: Allocator,
scanner: *Scanner,
current: ?Scanner.Token = null,
prev: ?Scanner.Token = null,
locals: [256]Local = undefined,
local_cnt: usize = 0,
scope_depth: i64 = 0,
break_jumps: std.ArrayList(JumpList),
cont_jumps: std.ArrayList(JumpList),
objects: *Objects,
function: *Values.FunctionObject = undefined,
function_obj: *Values.Object = undefined,
function_type: FunctionType = .Script,

pub fn init(allocator: Allocator, scanner: *Scanner, objects: *Objects, func_type: FunctionType, name: []const u8) !Self {
    var function_obj = try objects.allocFunction(name, 0, allocator);
    var c = Self{
        .allocator = allocator,
        .objects = objects,
        .scanner = scanner,
        .break_jumps = std.ArrayList(JumpList).init(allocator),
        .cont_jumps = std.ArrayList(JumpList).init(allocator),
        .function_type = func_type,
        .function = &function_obj.value.Function,
        .function_obj = function_obj,
    };

    try c.addLocal(Scanner.Token{ .t = .Str, .line = 0, .lexeme = "" });
    return c;
}

pub fn deinit(self: *Self) void {
    self.scanner.deinit();
    self.break_jumps.deinit();
    self.cont_jumps.deinit();
}

pub fn start(self: *Self) !*Values.Object {
    try self.advance();
    while (!self.check(.EOF)) {
        try self.declaration();
    }
    return self.end();
}

fn emitReturn(self: *Self) !void {
    try self.emitOpByte(.Nil);
    try self.emitOpByte(.Return);
}

fn end(self: *Self) !*Values.Object {
    try self.emitReturn();
    if (env.DebugPrintCode) {
        try self.function.chunk.disassmble(if (self.function.name.len != 0) self.function.name else "<script>");
    }
    return self.function_obj;
}

fn emitConst(self: *Self, val: Values.Value) !usize {
    const c = try self.makeConst(val);

    try self.emitMulti(&[_]u8{ Chunk.OpCode.Constant.byte(), @as(u8, @intCast(c)) });

    return c;
}

fn makeConst(self: *Self, val: Values.Value) !usize {
    const c = try self.function.chunk.addConstant(val);
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
    try self.function.chunk.write(byte, self.prev.?.line);
}

fn emitOpByte(self: *Self, op: Chunk.OpCode) !void {
    try self.function.chunk.write(op.byte(), self.prev.?.line);
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
    if (try self.match(.Fn)) {
        return self.fnDecl();
    } else if (try self.match(.Fn)) {
        return self.varDecl();
    }
    return self.statement();
}

fn fnDecl(self: *Self) !void {
    var global = try self.parseVar("Expect function name");
    self.markInitialized();
    try self.functionDef(.Function);
    try self.defGlobalVar(@intCast(global));
}

fn functionDef(self: *Self, function_type: FunctionType) !void {
    _ = function_type;
    var compiler = try Self.init(self.allocator, self.scanner, self.objects, .Function, self.prev.?.lexeme);
    defer compiler.deinit();
    compiler.prev = self.prev;
    compiler.current = self.current;
    compiler.beginScope();
    try compiler.consume(.LeftParen, "Expect '(' after function name.");
    if (!compiler.check(.RightParen)) {
        while (true) {
            compiler.function.arity += 1;
            if (compiler.function.arity > 255) {
                return compiler.errAtCurrent("Can't have more than 255 as function paramteres.", error.MaxParamExceede);
            }
            var constant = try compiler.parseVar("Expect parameter name");
            try compiler.defGlobalVar(@intCast(constant));
            if (try compiler.match(.Comma)) continue;
            break;
        }
    }
    try compiler.consume(.RightParen, "Expect ')' after parameters.");
    try compiler.consume(.LeftBrace, "Expect '{' before function body.");
    try compiler.block();
    var fn_obj = try compiler.end();
    self.prev = compiler.prev;
    self.current = compiler.current;
    _ = try self.emitConst(Values.objValue(fn_obj));
}

fn ifStmt(self: *Self) !void {
    try self.consume(.LeftParen, "Expect '(' after 'if'.");
    try self.expression();
    try self.consume(.RightParen, "Expect ')' after condition.");
    var jump = try self.emitJump(.JumpIfFalse);
    try self.emitOpByte(.Pop);
    try self.statement();
    const else_jump = try self.emitJump(.Jump);
    try self.patchJump(jump);
    try self.emitOpByte(.Pop);
    if (try self.match(.Else)) {
        try self.statement();
    }
    try self.patchJump(else_jump);
}

fn emitJump(self: *Self, jump_type: Chunk.OpCode) !usize {
    try self.emitMulti(&[_]u8{ jump_type.byte(), 0xff, 0xff });
    return self.function.chunk.code.items.len - 2;
}

fn patchJump(self: *Self, offset: usize) !void {
    const jump = self.function.chunk.code.items.len - offset - 2;
    if (jump > MaxJump) {
        return error.MaxJumpLines;
    }
    self.function.chunk.code.items[offset] = @as(u8, @intCast(jump >> 8)) & 0xff;
    self.function.chunk.code.items[offset + 1] = @as(u8, @intCast(jump)) & 0xff;
}

fn createBackwardJump(self: *Self, offset: usize) !void {
    const j = try self.emitJump(.BackJump);
    const jump = self.function.chunk.code.items.len - offset;
    if (jump > MaxJump) {
        return error.MaxJumpLines;
    }
    self.function.chunk.code.items[j] = @as(u8, @intCast(jump >> 8)) & 0xff;
    self.function.chunk.code.items[j + 1] = @as(u8, @intCast(jump)) & 0xff;
}

fn patchBackwardJump(self: *Self, jump_offset: usize, offset: usize) !void {
    const jump = jump_offset - offset + 2;
    if (jump > MaxJump) {
        return error.MaxJumpLines;
    }
    self.function.chunk.code.items[jump_offset] = @as(u8, @intCast(jump >> 8)) & 0xff;
    self.function.chunk.code.items[jump_offset + 1] = @as(u8, @intCast(jump)) & 0xff;
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
    if (try self.match(.Return)) {
        return self.retStmt();
    } else if (try self.match(.Print)) {
        try self.printStmt();
    } else if (try self.match(.LeftBrace)) {
        self.beginScope();
        try self.block();
        try self.endScope();
        return;
    } else if (try self.match(.If)) {
        return self.ifStmt();
    } else if (try self.match(.While)) {
        return self.whileStmt();
    } else if (try self.match(.For)) {
        return self.forStmt();
    } else {
        try self.expressionStmt();
    }
}

fn retStmt(self: *Self) !void {
    if (try self.match(.Semicolon)) {
        try self.emitReturn();
        return;
    }
    try self.expression();
    try self.consume(.Semicolon, "Expect ';' after return value");
    try self.emitOpByte(.Return);
}

fn forStmt(self: *Self) !void {
    self.beginScope();
    try self.consume(.LeftParen, "Expect '(' after for");
    if (try self.match(.Var)) {
        try self.varDecl();
    } else {
        try self.consume(.Semicolon, "Expect ; after loop initiation");
    }

    const loop_start = self.function.chunk.code.items.len;

    // if (!self.check(.Semicolon)) {
    try self.expression();
    const exit_jump = try self.emitJump(.JumpIfFalse);
    try self.emitOpByte(.Pop);
    // }

    try self.consume(.Semicolon, "Expect ; after loop increment");
    const start_jump = try self.emitJump(.Jump);

    const condition_start = self.function.chunk.code.items.len;

    if (!self.check(.RightParen)) {
        try self.expression();
        try self.emitOpByte(.Pop);
    }
    try self.createBackwardJump(loop_start);

    try self.consume(.RightParen, "Expect ')' after for clause");
    try self.patchJump(start_jump);
    try self.statement();
    try self.createBackwardJump(condition_start);
    if (self.cont_jumps.items.len > 0) {
        var i: i64 = @intCast(self.cont_jumps.items.len - 1);
        while (i >= 0) : (i -= 1) {
            if (self.scope_depth <= self.cont_jumps.items[@intCast(i)].scope) {
                const jump = self.cont_jumps.pop();
                try self.patchBackwardJump(jump.offset, condition_start);
            }
        }
    }
    try self.patchJump(exit_jump);
    if (self.break_jumps.items.len > 0) {
        var i: i64 = @intCast(self.break_jumps.items.len - 1);
        while (i >= 0) : (i -= 1) {
            if (self.scope_depth <= self.break_jumps.items[@intCast(i)].scope) {
                const jump = self.break_jumps.pop();
                try self.patchJump(jump.offset);
            }
        }
    }
    try self.emitOpByte(.Pop);
    try self.endScope();
}

fn whileStmt(self: *Self) !void {
    var loop_start = self.function.chunk.code.items.len;
    try self.consume(.LeftParen, "Expect '(' after while stmt");
    try self.expression();
    try self.consume(.RightParen, "Expect ')' after condition");
    const exit_jump = try self.emitJump(.JumpIfFalse);
    try self.emitOpByte(.Pop);
    try self.statement();
    try self.createBackwardJump(loop_start);
    try self.patchJump(exit_jump);
    try self.emitOpByte(.Pop);
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

fn and_(self: *Self, _: bool) !void {
    var j = try self.emitJump(.JumpIfFalse);
    try self.emitOpByte(.Pop);
    try self.parsePrecedence(.And);

    try self.patchJump(j);
}

fn or_(self: *Self, _: bool) !void {
    var else_j = try self.emitJump(.JumpIfFalse);
    var then_j = try self.emitJump(.Jump);
    try self.patchJump(else_j);
    try self.emitOpByte(.Pop);
    try self.parsePrecedence(.And);

    try self.patchJump(then_j);
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

fn call(self: *Self, _: bool) !void {
    const arg_c = try self.argList();
    try self.emitOpByte(.Call);
    try self.emitByte(@intCast(arg_c));
}

fn argList(self: *Self) !usize {
    var arg_c: usize = 0;

    if (!self.check(.RightParen)) {
        while (true) {
            try self.expression();
            if (arg_c == 255) try self.err("Can't have more than 255 arguments.", error.MaxArgExceeded);
            arg_c += 1;

            if (try self.match(.Comma)) continue;
            break;
        }
    }
    try self.consume(.RightParen, "Expect ')' after arguments.");

    return arg_c;
}

fn binary(self: *Self, can_assign: bool) !void {
    _ = can_assign;
    var prev_t = self.prev.?.t;
    var rule = getRule(prev_t);
    try self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));
    try self.emitMulti(switch (prev_t) {
        .Plus => &[_]u8{Chunk.OpCode.Add.byte()},
        .Gte => &[_]u8{ Chunk.OpCode.Less.byte(), Chunk.OpCode.Not.byte() },
        .Minus => &[_]u8{Chunk.OpCode.Subtract.byte()},
        .Star => &[_]u8{Chunk.OpCode.Multiply.byte()},
        .Slash => &[_]u8{Chunk.OpCode.Divide.byte()},
        .Greater => &[_]u8{Chunk.OpCode.Greater.byte()},
        .Less => &[_]u8{Chunk.OpCode.Less.byte()},
        .Lte => &[_]u8{ Chunk.OpCode.Greater.byte(), Chunk.OpCode.Not.byte() },
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

fn break_(self: *Self, can_assign: bool) !void {
    _ = can_assign;
    // try self.emitOpByte(.Pop);
    try self.break_jumps.append(.{ .scope = self.scope_depth, .offset = try self.emitJump(.Jump) });
}

fn continue_(self: *Self, can_assign: bool) !void {
    _ = can_assign;
    try self.cont_jumps.append(.{ .scope = self.scope_depth, .offset = try self.emitJump(.BackJump) });
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
    _ = try self.emitConst(Values.objValue(try self.objects.allocStr(self.prev.?.literal.?.String)));
}

fn idenConst(self: *Self, t: Scanner.Token) !usize {
    return self.makeConst(Values.objValue(try self.objects.allocStr(t.lexeme)));
}

fn defGlobalVar(self: *Self, global: u8) !void {
    if (self.scope_depth > 0) {
        self.markInitialized();
        return;
    }
    try self.emitMulti(&[_]u8{ Chunk.OpCode.DefineGlobal.byte(), global });
}

fn markInitialized(self: *Self) void {
    if (self.scope_depth == 0) return;
    self.locals[self.local_cnt - 1].depth = self.scope_depth;
}
