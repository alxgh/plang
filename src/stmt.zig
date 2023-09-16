const std = @import("std");
const expr = @import("./expr.zig");
const tokens = @import("./tokens.zig");

pub const StmtType = enum {
    expression,
    print,
    variable,
    block,
};

pub const Stmt = struct {
    t: StmtType,
};

fn Conv(comptime T: type) type {
    return struct {
        pub fn from(e: *Stmt) *T {
            return @fieldParentPtr(T, "s", e);
        }
        pub fn to(be: T) *Stmt {
            return be.s;
        }
    };
}

pub const Expression = struct {
    s: Stmt = .{ .t = .expression },

    e: *expr.Expr,

    pub fn accept(self: *Expression, v: anytype, comptime T: type) T {
        return v.visitExprStmt(self);
    }
};

pub const ExpressionConv = Conv(Expression);

pub const Print = struct {
    s: Stmt = .{ .t = .print },

    e: *expr.Expr,

    pub fn accept(self: *Print, v: anytype, comptime T: type) T {
        return v.visitPrintStmt(self);
    }
};

pub const PrintConv = Conv(Print);

pub const Var = struct {
    s: Stmt = .{ .t = .variable },

    name: tokens.Token,
    initializer: ?*expr.Expr,

    pub fn accept(self: *Var, v: anytype, comptime T: type) T {
        return v.visitVarStmt(self);
    }
};

pub const VarConv = Conv(Var);

pub const Block = struct {
    s: Stmt = .{ .t = .block },
    statements: std.ArrayList(*Stmt),

    pub fn accept(self: *Block, v: anytype, comptime T: type) T {
        return v.visitBlockStmt(self);
    }
};

pub const BlockConv = Conv(Block);
