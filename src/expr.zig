const std = @import("std");
const tokens = @import("./tokens.zig");
const Token = tokens.Token;
const visitor = @import("./visitor/visitor.zig");

pub const ExpType = enum { literal, grouping, unary, binary };

pub const Expr = struct {
    t: ExpType,
};

pub const Binary = struct {
    e: Expr = .{ .t = .binary },

    left: *Expr,
    op: *Token,
    right: *Expr,

    pub fn accept(self: *Binary, comptime v: *visitor.Visitor) void {
        v.visitBinary(self);
    }
};
pub const BinaryConv = Conv(Binary);

pub const Literal = struct {
    e: Expr = .{ .t = .literal },

    v: ?*tokens.Token,
    pub fn accept(self: *Literal, comptime v: *visitor.Visitor) void {
        v.visitLiteral(self);
    }
};
pub const LiteralConv = Conv(Literal);

pub const Grouping = struct {
    e: Expr = .{ .t = .grouping },

    mid: *Expr,
    pub fn accept(self: *Grouping, comptime v: *visitor.Visitor) void {
        v.visitGrouping(self);
    }
};
pub const GroupingConv = Conv(Grouping);

pub const Unary = struct {
    e: Expr = .{ .t = .unary },

    op: *Token,
    right: *Expr,
    pub fn accept(self: *Unary, comptime v: *visitor.Visitor) void {
        v.visitUnary(self);
    }
};
pub const UnaryConv = Conv(Unary);

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
