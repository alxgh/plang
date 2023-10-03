const std = @import("std");
const tokens = @import("../tokens.zig");
const stmt = @import("../stmt.zig");
const expr = @import("../expr.zig");
const Interpreter = @import("Interpreter.zig");
const Visitor = @import("visitor.zig").Visitor(void);
const Self = @This();
const Allocator = std.mem.Allocator;
const Scope = std.StringHashMap(bool);
const Scopes = std.ArrayList(*Scope);

// interpreter: Interpreter,
allocator: Allocator,
scopes: Scopes,
interpreter: *Interpreter,

pub fn init(allocator: Allocator, interpreter: *Interpreter) Self {
    return .{
        .allocator = allocator,
        .scopes = Scopes.init(allocator),
        .interpreter = interpreter,
    };
}

pub fn deinit(self: *Self) void {
    self.scopes.deinit();
}

pub fn start(self: *Self, stmts: std.ArrayList(*stmt.Stmt)) void {
    var visitor = Visitor{
        .ctx = self,
        .visitBinaryFn = visitBinaryExpr,
        .visitGroupingFn = visitGroupingExpr,
        .visitLiteralFn = Noop(*expr.Literal).visit,
        .visitUnaryFn = visitUnaryExpr,
        .visitVariableFn = visitVariableExpr,
        .visitAssignFn = visitAssignExpr,
        .visitLogicalFn = visitLogicalExpr,
        .visitCallFn = visitCallExpr,

        .visitPrintStmtFn = visitPrintStmt,
        .visitExprStmtFn = visitExprStmt,
        .visitVarStmtFn = visitVarStmt,
        .visitBlockStmtFn = visitBlockStmt,
        .visitIfStmtFn = visitIfStmt,
        .visitWhileStmtFn = visitWhileStmt,
        .visitFunctionStmtFn = visitFunctionStmt,
        .visitReturnStmtFn = visitReturnStmt,
    };

    for (stmts.items) |s| {
        _ = visitor.acceptStmt(s);
    }
}

fn Noop(comptime T: type) type {
    return struct {
        fn visit(ctx: *anyopaque, v: *Visitor, e: T) void {
            _ = e;
            _ = v;
            _ = ctx;
        }
    };
}

fn visitBlockStmt(ctx: *anyopaque, v: *Visitor, s: *stmt.Block) void {
    var self: *Self = @ptrCast(@alignCast(ctx));
    self.beginScope();
    self.resolve(v, s.statements);
    self.endScope();
}

fn visitReturnStmt(ctx: *anyopaque, v: *Visitor, s: *stmt.Return) void {
    var self: *Self = @ptrCast(@alignCast(ctx));
    if (s.value) |rv| {
        self.resolveExpr(v, rv);
    }
}

fn visitPrintStmt(ctx: *anyopaque, v: *Visitor, s: *stmt.Print) void {
    var self: *Self = @ptrCast(@alignCast(ctx));
    self.resolveExpr(v, s.e);
}

fn visitExprStmt(ctx: *anyopaque, v: *Visitor, s: *stmt.Expression) void {
    var self: *Self = @ptrCast(@alignCast(ctx));
    self.resolveExpr(v, s.e);
}

fn visitIfStmt(ctx: *anyopaque, v: *Visitor, s: *stmt.If) void {
    var self: *Self = @ptrCast(@alignCast(ctx));
    self.resolveExpr(v, s.cond);
    self.resolveStmt(v, s.then_branch);
    if (s.else_branch) |eb| {
        self.resolveStmt(v, eb);
    }
}

fn visitWhileStmt(ctx: *anyopaque, v: *Visitor, s: *stmt.While) void {
    var self: *Self = @ptrCast(@alignCast(ctx));
    self.resolveExpr(v, s.cond);
    self.resolveStmt(v, s.loop_statement);
}

fn visitVarStmt(ctx: *anyopaque, v: *Visitor, s: *stmt.Var) void {
    var self: *Self = @ptrCast(@alignCast(ctx));
    self.declare(s.name);
    if (s.initializer) |initializer| {
        self.resolveExpr(v, initializer);
    }
    self.define(s.name);
}

fn visitFunctionStmt(ctx: *anyopaque, v: *Visitor, s: *stmt.Function) void {
    var self: *Self = @ptrCast(@alignCast(ctx));

    self.declare(s.name);
    self.define(s.name);
    self.resolveFunction(v, s);
}

fn visitVariableExpr(ctx: *anyopaque, v: *Visitor, e: *expr.Variable) void {
    _ = v;
    var self: *Self = @ptrCast(@alignCast(ctx));
    if (self.scopes.getLastOrNull()) |scope| {
        if (scope.get(e.name.?.lexeme) == false) {
            // TODO: error handling
            @panic("can't read local variable in its own initializer");
        }
    }
    self.resolveLocal(&e.e, e.name.?);
}

fn visitAssignExpr(ctx: *anyopaque, v: *Visitor, e: *expr.Assign) void {
    var self: *Self = @ptrCast(@alignCast(ctx));
    self.resolveExpr(v, e.value);
    self.resolveLocal(&e.e, e.name.?);
}

fn visitBinaryExpr(ctx: *anyopaque, v: *Visitor, e: *expr.Binary) void {
    var self: *Self = @ptrCast(@alignCast(ctx));
    self.resolveExpr(v, e.left);
    self.resolveExpr(v, e.right);
}

fn visitCallExpr(ctx: *anyopaque, v: *Visitor, e: *expr.Call) void {
    var self: *Self = @ptrCast(@alignCast(ctx));
    self.resolveExpr(v, e.callee);
    for (e.arguments.items) |a| {
        self.resolveExpr(v, a);
    }
}

fn visitGroupingExpr(ctx: *anyopaque, v: *Visitor, e: *expr.Grouping) void {
    var self: *Self = @ptrCast(@alignCast(ctx));
    self.resolveExpr(v, e.mid);
}

fn visitLogicalExpr(ctx: *anyopaque, v: *Visitor, e: *expr.Logical) void {
    var self: *Self = @ptrCast(@alignCast(ctx));
    self.resolveExpr(v, e.left);
    self.resolveExpr(v, e.right);
}

fn visitUnaryExpr(ctx: *anyopaque, v: *Visitor, e: *expr.Unary) void {
    var self: *Self = @ptrCast(@alignCast(ctx));
    self.resolveExpr(v, e.right);
}

fn resolve(self: *Self, v: *Visitor, stmts: std.ArrayList(*stmt.Stmt)) void {
    for (stmts.items) |s| {
        self.resolveStmt(v, s);
    }
}

fn resolveStmt(self: *Self, v: *Visitor, s: *stmt.Stmt) void {
    _ = self;
    v.acceptStmt(s);
}

fn resolveExpr(self: *Self, v: *Visitor, e: *expr.Expr) void {
    _ = self;
    v.acceptExpr(e);
}

fn resolveFunction(self: *Self, v: *Visitor, s: *stmt.Function) void {
    self.beginScope();
    for (s.parameters.items) |p| {
        self.declare(p);
        self.define(p);
    }
    self.resolve(v, s.body.statements);
    self.endScope();
}

fn beginScope(self: *Self) void {
    var scope = self.allocator.create(Scope) catch @panic("OOM");
    scope.* = Scope.init(self.allocator);
    self.scopes.append(scope) catch @panic("OOM");
}

fn endScope(self: *Self) void {
    // popOrNull for safety.
    // Does not matter if you end a non-existing scope.
    if (self.scopes.popOrNull()) |scope| {
        scope.deinit();
        self.allocator.destroy(scope);
    }
}

fn declare(self: *Self, name: tokens.Token) void {
    var scope = self.scopes.getLastOrNull();
    if (scope) |s| {
        s.put(name.lexeme, false) catch @panic("OOM");
    }
}

fn define(self: *Self, name: tokens.Token) void {
    var scope = self.scopes.getLastOrNull();
    if (scope) |s| {
        s.put(name.lexeme, true) catch @panic("OOM");
    }
}

fn resolveLocal(self: *Self, e: *expr.Expr, name: tokens.Token) void {
    if (self.scopes.items.len == 0) {
        return;
    }
    var i: i64 = @intCast(self.scopes.items.len - 1);
    while (i >= 0) : (i -= 1) {
        var scope = self.scopes.items[@intCast(i)];
        if (scope.contains(name.lexeme)) {
            self.interpreter.resolve(e, @as(u64, self.scopes.items.len - 1 - @as(usize, @intCast(i))));
        }
    }
}
