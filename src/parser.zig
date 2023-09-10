const std = @import("std");
const scanner = @import("./scanner.zig");
const expr = @import("./expr.zig");
const tokens = @import("./tokens.zig");

pub const Error = error{
    ParserFail,
};

const ErrorSet = Error || std.mem.Allocator.Error;

const Self = @This();

tokens_iterator: *scanner.TokenIterator,
allocator: std.mem.Allocator,
err_msg: []const u8 = undefined,
err_token: ?tokens.Token = null,

pub fn init(allocator: std.mem.Allocator, t: *scanner.TokenIterator) Self {
    return .{
        .tokens_iterator = t,
        .allocator = allocator,
    };
}

pub fn parse(self: *Self) ErrorSet!*expr.Expr {
    return try self.expression();
}

fn sync(self: *Self) void {
    self.tokens_iterator.seekBy(1);

    while (ErrorSet!self.tokens_iterator.finished()) {
        if (self.prev().tt == .semicolon) return;
        if (self.tokens_iterator.peek()) |token| {
            blk: {
                switch (token.tt) {
                    .class, .fn_tok, .var_tok, .for_tok, .if_tok, .while_tok, .ret_tok => return,
                    else => break :blk,
                }
            }
        }

        self.tokens_iterator.seekBy(1);
    }
}

fn binaryExpr(self: *Self, l: *expr.Expr, op: tokens.Token, r: *expr.Expr) ErrorSet!*expr.Binary {
    var be = try self.allocator.create(expr.Binary);
    be.e = expr.Expr{ .t = .binary };
    be.left = l;
    be.op = op;
    be.right = r;
    return be;
}

fn unaryExpr(self: *Self, op: tokens.Token, r: *expr.Expr) ErrorSet!*expr.Unary {
    var be = try self.allocator.create(expr.Unary);
    be.e = expr.Expr{ .t = .unary };
    be.op = op;
    be.right = r;
    return be;
}

fn groupingExpr(self: *Self, e: *expr.Expr) ErrorSet!*expr.Grouping {
    var ge = try self.allocator.create(expr.Grouping);
    ge.e = expr.Expr{ .t = .grouping };
    ge.mid = e;
    return ge;
}

fn litExpr(self: *Self, t: tokens.Token) ErrorSet!*expr.Literal {
    var ge = try self.allocator.create(expr.Literal);
    ge.e = expr.Expr{ .t = .literal };
    ge.v = t;
    return ge;
}

fn expression(self: *Self) ErrorSet!*expr.Expr {
    return try self.equality();
}

fn equality(self: *Self) ErrorSet!*expr.Expr {
    var e = try self.comparison();

    while (self.match(.{ .bang_eq, .eq_eq })) {
        var op = self.prev();
        var right = try self.comparison();
        e = &(try self.binaryExpr(e, op.?, right)).e;
    }

    return e;
}

fn comparison(self: *Self) ErrorSet!*expr.Expr {
    var e = try self.term();

    while (self.match(.{ .greater, .gte, .less, .lte })) {
        var op = self.prev();
        var right = try self.term();
        e = &(try self.binaryExpr(e, op.?, right)).e;
    }
    return e;
}

fn term(self: *Self) ErrorSet!*expr.Expr {
    var e = try self.factor();
    while (self.match(.{ .minus, .plus })) {
        var op = self.prev();
        var right = try self.factor();

        e = &(try self.binaryExpr(e, op.?, right)).e;
    }
    return e;
}

fn factor(self: *Self) ErrorSet!*expr.Expr {
    var e = try self.unary();
    while (self.match(.{ .slash, .star })) {
        var op = self.prev();
        var right = try self.factor();
        std.debug.print("right: {}\n", .{@fieldParentPtr(expr.Literal, "e", right)});
        std.debug.print("left: {}\n", .{@fieldParentPtr(expr.Literal, "e", e)});
        e = &(try self.binaryExpr(e, op.?, right)).e;
    }
    return e;
}

fn unary(self: *Self) ErrorSet!*expr.Expr {
    if (self.match(.{ .bang, .minus })) {
        var op = self.prev();
        var right = try self.primary();
        var ue = try self.unaryExpr(op.?, right);
        return &ue.e;
    }

    return try self.primary();
}

fn primary(self: *Self) ErrorSet!*expr.Expr {
    if (self.match(.{ .false_tok, .num, .str, .true_tok, .nil_tok })) {
        var lit = try self.litExpr(self.prev().?);
        return &lit.e;
    }

    if (self.match(.{.l_paren})) {
        var e = try self.expression();
        _ = try self.consume(.r_paren, "Expect ')' after expression");
        var ge = try self.groupingExpr(e);
        return &ge.e;
    }

    try self.panic(self.tokens_iterator.peek(), "Expect expression.");
    unreachable;
}

fn match(self: *Self, token_types: anytype) bool {
    const ti = @typeInfo(@TypeOf(token_types));
    if (ti != .Struct) {
        @compileError("input should be a tuple");
    }
    const fields = ti.Struct.fields;

    inline for (fields) |field| {
        if (self.check(@field(token_types, field.name))) {
            _ = self.advance();
            return true;
        }
    }

    return false;
}

fn prev(self: *Self) ?tokens.Token {
    return self.tokens_iterator.prev();
}

fn check(self: *Self, tt: tokens.Tokens) bool {
    if (self.tokens_iterator.peek()) |token| {
        return token.tt == tt;
    }
    return false;
}

fn advance(self: *Self) ?tokens.Token {
    if (!self.tokens_iterator.finished()) {
        self.tokens_iterator.seekBy(1);
    }
    return self.prev();
}

fn consume(self: *Self, tt: tokens.Tokens, err_msg: []const u8) ErrorSet!?tokens.Token {
    if (self.check(tt)) return self.advance();

    try self.panic(self.tokens_iterator.peek().?, err_msg);
    unreachable;
}

fn panic(self: *Self, token: ?tokens.Token, err_msg: []const u8) ErrorSet!void {
    self.err_msg = err_msg;
    self.err_token = token;
    return error.ParserFail;
}
