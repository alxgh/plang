const std = @import("std");
const scanner = @import("./scanner.zig");
const stmt = @import("./stmt.zig");
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
root: ?*expr.Expr = null,
stmts: std.ArrayList(*stmt.Stmt) = undefined,

pub fn init(allocator: std.mem.Allocator, t: *scanner.TokenIterator) Self {
    return .{
        .tokens_iterator = t,
        .allocator = allocator,
    };
}

pub fn deinit(self: *Self) ErrorSet!void {
    defer self.stmts.deinit();
    var queue = std.ArrayList(*expr.Expr).init(self.allocator);
    defer queue.deinit();

    for (self.stmts.items) |s| {
        switch (s.t) {
            .print => {
                var p = stmt.PrintConv.from(s);
                try queue.append(p.e);
                self.allocator.destroy(p);
            },
            .expression => {
                var p = stmt.ExpressionConv.from(s);
                try queue.append(p.e);
                self.allocator.destroy(p);
            },
            .variable => {
                var p = stmt.VarConv.from(s);
                if (p.initializer) |initializer| {
                    try queue.append(initializer);
                }
                self.allocator.destroy(p);
            },
        }
    }
    while (queue.popOrNull()) |e| {
        // std.debug.print("{}\n", .{e});
        if (e != undefined) {
            switch (e.t) {
                .literal => {
                    self.allocator.destroy(expr.LiteralConv.from(e));
                },
                .grouping => {
                    var ge = expr.GroupingConv.from(e);
                    try queue.append(ge.mid);
                    self.allocator.destroy(ge);
                },
                .unary => {
                    var ue = expr.UnaryConv.from(e);
                    // std.debug.print("{}\n", .{ue});
                    try queue.append(ue.right);
                    self.allocator.destroy(ue);
                },
                .binary => {
                    var be = expr.BinaryConv.from(e);
                    try queue.append(be.right);
                    try queue.append(be.left);
                    self.allocator.destroy(be);
                },
                .variable => {
                    var ve = expr.VariableConv.from(e);
                    self.allocator.destroy(ve);
                },
                .assign => {
                    var ae = expr.AssignConv.from(e);
                    try queue.append(ae.value);
                    self.allocator.destroy(ae);
                },
            }
        }
    }
}

pub fn parse(self: *Self) ErrorSet!std.ArrayList(*stmt.Stmt) {
    var stmts = std.ArrayList(*stmt.Stmt).init(self.allocator);
    while (!self.tokens_iterator.finished() and self.tokens_iterator.peek().?.tt != .eof) {
        try stmts.append(try self.declaration());
    }
    self.stmts = stmts;
    return stmts;
}

pub fn declaration(self: *Self) !*stmt.Stmt {
    // errdefer self.sync();
    if (self.match(.{.var_tok})) {
        return self.varDeclaration() catch undefined;
    }
    return self.statement() catch undefined;
}

pub fn varDeclaration(self: *Self) !*stmt.Stmt {
    var name = try self.consume(.iden, "Expect variable name after 'var'");
    var initializer: ?*expr.Expr = null;
    if (self.match(.{.eq})) {
        initializer = try self.expression();
    }
    _ = try self.consume(.semicolon, "Expect ';' after var declaration.'");
    if (self.newVarStmt(name.?, initializer)) |s| {
        return &s.s;
    } else |err| {
        return err;
    }
}

pub fn statement(self: *Self) !*stmt.Stmt {
    if (self.match(.{.print_tok})) {
        return try self.printStmt();
    }
    return try self.exprStmt();
}

pub fn printStmt(self: *Self) !*stmt.Stmt {
    var e = try self.expression();
    _ = try self.consume(.semicolon, "Expect ';' after value");
    if (self.newPrintStmt(e)) |s| {
        return &s.s;
    } else |err| {
        return err;
    }
}

pub fn exprStmt(self: *Self) !*stmt.Stmt {
    var e = try self.expression();
    _ = try self.consume(.semicolon, "Expect ';' after value");
    if (self.newExprStmt(e)) |s| {
        return &s.s;
    } else |err| {
        return err;
    }
}

fn sync(self: *Self) void {
    self.tokens_iterator.seekBy(1);

    while (!self.tokens_iterator.finished()) {
        if (self.prev()) |t| {
            if (t.tt == .semicolon) {
                return;
            }
        }
        if (self.tokens_iterator.peek()) |token| {
            blk: {
                switch (token.tt) {
                    .class, .fn_tok, .var_tok, .for_tok, .if_tok, .while_tok, .ret_tok, .print_tok => return,
                    else => break :blk,
                }
            }
        }

        self.tokens_iterator.seekBy(1);
    }
}

fn newPrintStmt(self: *Self, e: *expr.Expr) ErrorSet!*stmt.Print {
    var ps = try self.allocator.create(stmt.Print);
    ps.s = .{ .t = .print };
    ps.e = e;
    return ps;
}

fn newExprStmt(self: *Self, e: *expr.Expr) ErrorSet!*stmt.Expression {
    var ps = try self.allocator.create(stmt.Expression);
    ps.s = .{ .t = .expression };
    ps.e = e;
    return ps;
}

fn newVarStmt(self: *Self, name: tokens.Token, initializer: ?*expr.Expr) ErrorSet!*stmt.Var {
    var ps = try self.allocator.create(stmt.Var);
    ps.s = .{ .t = .variable };
    ps.initializer = initializer;
    ps.name = name;
    return ps;
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

fn varExpr(self: *Self, name: tokens.Token) ErrorSet!*expr.Variable {
    var ve = try self.allocator.create(expr.Variable);
    ve.e = expr.Expr{ .t = .variable };
    ve.name = name;
    return ve;
}

fn assignExpr(self: *Self, name: tokens.Token, value: *expr.Expr) ErrorSet!*expr.Assign {
    var ae = try self.allocator.create(expr.Assign);
    ae.e = expr.Expr{ .t = .assign };
    ae.name = name;
    ae.value = value;
    return ae;
}

fn expression(self: *Self) ErrorSet!*expr.Expr {
    return try self.assignment();
}

fn assignment(self: *Self) ErrorSet!*expr.Expr {
    var e = try self.equality();
    if (self.match(.{.eq})) {
        var eq = self.prev();
        var val = try self.assignment();
        if (e.t == .variable) {
            var name = expr.VariableConv.from(e).name;

            return &(try self.assignExpr(name.?, val)).e;
        }
        try self.panic(eq, "Invalid assignment target.");
    }
    return e;
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

    if (self.match(.{.iden})) {
        var variable = try self.varExpr(self.prev().?);
        return &variable.e;
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