const std = @import("std");
const expr = @import("../expr.zig");
const tokens = @import("../tokens.zig");
pub fn Visitor(comptime T: type) type {
    return struct {
        const Self = @This();
        visitBinaryFn: fn (comptime *Self, *expr.Binary) T,
        visitGroupingFn: fn (comptime *Self, *expr.Grouping) T,
        visitLiteralFn: fn (comptime *Self, *expr.Literal) T,
        visitUnaryFn: fn (comptime *Self, *expr.Unary) T,

        pub fn visitBinary(comptime self: *Self, e: *expr.Binary) T {
            self.visitBinaryFn(self, e);
        }

        pub fn visitGrouping(comptime self: *Self, e: *expr.Grouping) T {
            self.visitGroupingFn(self, e);
        }

        pub fn visitLiteral(comptime self: *Self, e: *expr.Literal) T {
            self.visitLiteralFn(self, e);
        }

        pub fn visitUnary(comptime self: *Self, e: *expr.Unary) T {
            self.visitUnaryFn(self, e);
        }

        pub fn accept(comptime self: *Self, e: *expr.Expr) T {
            _ = switch (e.t) {
                .literal => expr.LiteralConv.from(e).accept(self),
                .binary => expr.BinaryConv.from(e).accept(self),
                .grouping => expr.GroupingConv.from(e).accept(self, T),
                .unary => expr.UnaryConv.from(e).accept(self),
            };
            return;
        }
    };
}
