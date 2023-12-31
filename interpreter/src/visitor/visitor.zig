const std = @import("std");
const expr = @import("../expr.zig");
const stmt = @import("../stmt.zig");
const tokens = @import("../tokens.zig");

pub fn Visitor(comptime resultT: type) type {
    return struct {
        const Self = @This();
        ctx: *anyopaque,
        // expressions
        visitBinaryFn: *const fn (*anyopaque, *Self, *expr.Binary) resultT,
        visitGroupingFn: *const fn (*anyopaque, *Self, *expr.Grouping) resultT,
        visitLiteralFn: *const fn (*anyopaque, *Self, *expr.Literal) resultT,
        visitUnaryFn: *const fn (*anyopaque, *Self, *expr.Unary) resultT,
        visitVariableFn: *const fn (*anyopaque, *Self, *expr.Variable) resultT,
        visitAssignFn: *const fn (*anyopaque, *Self, *expr.Assign) resultT,
        visitLogicalFn: *const fn (*anyopaque, *Self, *expr.Logical) resultT,
        visitCallFn: *const fn (*anyopaque, *Self, *expr.Call) resultT,

        // statements
        visitExprStmtFn: *const fn (*anyopaque, *Self, *stmt.Expression) resultT,
        visitPrintStmtFn: *const fn (*anyopaque, *Self, *stmt.Print) resultT,
        visitVarStmtFn: *const fn (*anyopaque, *Self, *stmt.Var) resultT,
        visitBlockStmtFn: *const fn (*anyopaque, *Self, *stmt.Block) resultT,
        visitIfStmtFn: *const fn (*anyopaque, *Self, *stmt.If) resultT,
        visitWhileStmtFn: *const fn (*anyopaque, *Self, *stmt.While) resultT,
        visitFunctionStmtFn: *const fn (*anyopaque, *Self, *stmt.Function) resultT,
        visitReturnStmtFn: *const fn (*anyopaque, *Self, *stmt.Return) resultT,

        pub fn visitBinary(self: *Self, e: *expr.Binary) resultT {
            return self.visitBinaryFn(self.ctx, self, e);
        }

        pub fn visitGrouping(self: *Self, e: *expr.Grouping) resultT {
            return self.visitGroupingFn(self.ctx, self, e);
        }

        pub fn visitLiteral(self: *Self, e: *expr.Literal) resultT {
            return self.visitLiteralFn(self.ctx, self, e);
        }

        pub fn visitUnary(self: *Self, e: *expr.Unary) resultT {
            return self.visitUnaryFn(self.ctx, self, e);
        }

        pub fn visitVariable(self: *Self, e: *expr.Variable) resultT {
            return self.visitVariableFn(self.ctx, self, e);
        }

        pub fn visitAssign(self: *Self, e: *expr.Assign) resultT {
            return self.visitAssignFn(self.ctx, self, e);
        }

        pub fn visitLogical(self: *Self, e: *expr.Logical) resultT {
            return self.visitLogicalFn(self.ctx, self, e);
        }

        pub fn visitCall(self: *Self, e: *expr.Call) resultT {
            return self.visitCallFn(self.ctx, self, e);
        }

        pub fn acceptExpr(self: *Self, e: *expr.Expr) resultT {
            return switch (e.t) {
                .literal => expr.LiteralConv.from(e).accept(self, resultT),
                .binary => expr.BinaryConv.from(e).accept(self, resultT),
                .grouping => expr.GroupingConv.from(e).accept(self, resultT),
                .unary => expr.UnaryConv.from(e).accept(self, resultT),
                .variable => expr.VariableConv.from(e).accept(self, resultT),
                .assign => expr.AssignConv.from(e).accept(self, resultT),
                .logical => expr.LogicalConv.from(e).accept(self, resultT),
                .call => expr.CallConv.from(e).accept(self, resultT),
            };
        }

        pub fn visitPrintStmt(self: *Self, s: *stmt.Print) resultT {
            return self.visitPrintStmtFn(self.ctx, self, s);
        }

        pub fn visitExprStmt(self: *Self, s: *stmt.Expression) resultT {
            return self.visitExprStmtFn(self.ctx, self, s);
        }

        pub fn visitVarStmt(self: *Self, s: *stmt.Var) resultT {
            return self.visitVarStmtFn(self.ctx, self, s);
        }

        pub fn visitBlockStmt(self: *Self, s: *stmt.Block) resultT {
            return self.visitBlockStmtFn(self.ctx, self, s);
        }

        pub fn visitIfStmt(self: *Self, s: *stmt.If) resultT {
            return self.visitIfStmtFn(self.ctx, self, s);
        }

        pub fn visitWhileStmt(self: *Self, s: *stmt.While) resultT {
            return self.visitWhileStmtFn(self.ctx, self, s);
        }

        pub fn visitFunctionStmt(self: *Self, s: *stmt.Function) resultT {
            return self.visitFunctionStmtFn(self.ctx, self, s);
        }
        pub fn visitReturnStmt(self: *Self, s: *stmt.Return) resultT {
            return self.visitReturnStmtFn(self.ctx, self, s);
        }

        pub fn acceptStmt(self: *Self, s: *stmt.Stmt) resultT {
            return switch (s.t) {
                .expression => stmt.ExpressionConv.from(s).accept(self, resultT),
                .print => stmt.PrintConv.from(s).accept(self, resultT),
                .variable => stmt.VarConv.from(s).accept(self, resultT),
                .block => stmt.BlockConv.from(s).accept(self, resultT),
                .If => stmt.IfConv.from(s).accept(self, resultT),
                .While => stmt.WhileConv.from(s).accept(self, resultT),
                .Function => stmt.FunctionConv.from(s).accept(self, resultT),
                .Return => stmt.ReturnConv.from(s).accept(self, resultT),
            };
        }
    };
}
