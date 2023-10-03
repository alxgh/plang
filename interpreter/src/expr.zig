const std = @import("std");
const tokens = @import("./tokens.zig");
const Token = tokens.Token;
const Visitor = @import("./visitor/visitor.zig");

pub const ExpType = enum { literal, grouping, unary, binary, variable, assign, logical, call };

pub const Expr = struct {
    t: ExpType,
    id: u64,
};

var exprId: u64 = @as(u64, 0);

pub fn newExpr(t: ExpType) Expr {
    var eid = exprId;
    exprId += 1;
    return .{
        .t = t,
        .id = eid,
    };
}

fn Conv(comptime T: type) type {
    return struct {
        pub fn from(e: *Expr) *T {
            return @fieldParentPtr(T, "e", e);
        }
        pub fn to(be: T) *Expr {
            return be.e;
        }
    };
}

pub const Binary = struct {
    e: Expr,

    left: *Expr,
    op: Token,
    right: *Expr,

    pub fn accept(self: *Binary, v: anytype, comptime T: type) T {
        return v.visitBinary(self);
    }
};
pub const BinaryConv = Conv(Binary);

pub const Literal = struct {
    e: Expr,

    v: ?tokens.Token,

    pub fn accept(self: *Literal, v: anytype, comptime T: type) T {
        return v.visitLiteral(self);
    }
};
pub const LiteralConv = Conv(Literal);

pub const Grouping = struct {
    e: Expr,

    mid: *Expr,
    pub fn accept(self: *Grouping, v: anytype, comptime T: type) T {
        return v.visitGrouping(self);
    }
};
pub const GroupingConv = Conv(Grouping);

pub const Unary = struct {
    e: Expr,

    op: Token,
    right: *Expr,
    pub fn accept(self: *Unary, v: anytype, comptime T: type) T {
        return v.visitUnary(self);
    }
};

pub const UnaryConv = Conv(Unary);

pub const Variable = struct {
    e: Expr,

    name: ?tokens.Token,

    pub fn accept(self: *Variable, v: anytype, comptime T: type) T {
        return v.visitVariable(self);
    }
};

pub const VariableConv = Conv(Variable);

pub const Assign = struct {
    e: Expr,

    name: ?tokens.Token,
    value: *Expr,

    pub fn accept(self: *Assign, v: anytype, comptime T: type) T {
        return v.visitAssign(self);
    }
};

pub const AssignConv = Conv(Assign);

pub const Logical = struct {
    e: Expr,

    op: tokens.Token,
    left: *Expr,
    right: *Expr,

    pub fn accept(self: *Logical, v: anytype, comptime T: type) T {
        return v.visitLogical(self);
    }
};

pub const LogicalConv = Conv(Logical);

pub const Call = struct {
    e: Expr,

    callee: *Expr,
    paren: Token, // used for error reporting
    arguments: std.ArrayList(*Expr),

    pub fn accept(self: *Call, v: anytype, comptime T: type) T {
        return v.visitCall(self);
    }
};

pub const CallConv = Conv(Call);
