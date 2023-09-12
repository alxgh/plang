const std = @import("std");
const expr = @import("../expr.zig");
const tokens = @import("../tokens.zig");
pub fn Visitor(comptime resultT: type) type {
    return struct {
        const Self = @This();
        ctx: *anyopaque,
        visitBinaryFn: *const fn (*anyopaque, *Self, *expr.Binary) resultT,
        visitGroupingFn: *const fn (*anyopaque, *Self, *expr.Grouping) resultT,
        visitLiteralFn: *const fn (*anyopaque, *Self, *expr.Literal) resultT,
        visitUnaryFn: *const fn (*anyopaque, *Self, *expr.Unary) resultT,

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

        pub fn accept(self: *Self, e: *expr.Expr) resultT {
            return switch (e.t) {
                .literal => expr.LiteralConv.from(e).accept(self, resultT),
                .binary => expr.BinaryConv.from(e).accept(self, resultT),
                .grouping => expr.GroupingConv.from(e).accept(self, resultT),
                .unary => expr.UnaryConv.from(e).accept(self, resultT),
            };
        }
    };
}
