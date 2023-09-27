const std = @import("std");
const expr = @import("./expr.zig");
const tokens = @import("./tokens.zig");

pub const StmtType = enum {
    expression,
    print,
    variable,
    block,
    If,
    While,
    Function,
    Return,
};

pub const Stmt = struct {
    t: StmtType,
};

fn Conv(comptime T: type) type {
    return struct {
        pub fn from(s: *Stmt) *T {
            return @fieldParentPtr(T, "s", s);
        }
        pub fn to(be: *T) *Stmt {
            return &be.s;
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

pub const If = struct {
    s: Stmt = .{ .t = .If },
    cond: *expr.Expr,
    then_branch: *Stmt,
    else_branch: ?*Stmt,

    pub fn accept(self: *If, v: anytype, comptime T: type) T {
        return v.visitIfStmt(self);
    }
};

pub const IfConv = Conv(If);

pub const While = struct {
    s: Stmt = .{ .t = .While },
    cond: *expr.Expr,
    loop_statement: *Stmt,

    pub fn accept(self: *While, v: anytype, comptime T: type) T {
        return v.visitWhileStmt(self);
    }
};

pub const WhileConv = Conv(While);

pub const Function = struct {
    s: Stmt = .{ .t = .Function },
    name: tokens.Token,
    parameters: std.ArrayList(tokens.Token),
    body: *Block,

    pub fn accept(self: *Function, v: anytype, comptime T: type) T {
        return v.visitFunctionStmt(self);
    }
};

pub const FunctionConv = Conv(Function);

pub const Return = struct {
    s: Stmt = .{ .t = .Function },
    keyword: tokens.Token,
    value: ?*expr.Expr,

    pub fn accept(self: *Return, v: anytype, comptime T: type) T {
        return v.visitReturnStmt(self);
    }
};

pub const ReturnConv = Conv(Return);
