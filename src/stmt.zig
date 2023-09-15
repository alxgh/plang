const std = @import("std");
const expr = @import("./expr.zig");

pub const StmtType = enum {
    expression,
    print,
};

pub const Stmt = struct {
    t: StmtType,
};

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
