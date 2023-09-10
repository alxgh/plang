const std = @import("std");
const expr = @import("../expr.zig");
const tokens = @import("../tokens.zig");
pub const Visitor = struct {
    const Self = @This();
    visitBinaryFn: fn (comptime *Visitor, *expr.Binary) void,
    visitGroupingFn: fn (comptime *Visitor, *expr.Grouping) void,
    visitLiteralFn: fn (comptime *Visitor, *expr.Literal) void,
    visitUnaryFn: fn (comptime *Visitor, *expr.Unary) void,

    pub fn visitBinary(comptime self: *Self, e: *expr.Binary) void {
        self.visitBinaryFn(self, e);
    }

    pub fn visitGrouping(comptime self: *Self, e: *expr.Grouping) void {
        self.visitGroupingFn(self, e);
    }

    pub fn visitLiteral(comptime self: *Self, e: *expr.Literal) void {
        self.visitLiteralFn(self, e);
    }

    pub fn visitUnary(comptime self: *Self, e: *expr.Unary) void {
        self.visitUnaryFn(self, e);
    }

    pub fn accept(comptime self: *Self, e: *expr.Expr) void {
        _ = switch (e.t) {
            .literal => expr.LiteralConv.from(e).accept(self),
            .binary => expr.BinaryConv.from(e).accept(self),
            .grouping => expr.GroupingConv.from(e).accept(self),
            .unary => expr.UnaryConv.from(e).accept(self),
        };
        return;
    }
};

pub const ASTPrinter = struct {
    pub fn print(e: *expr.Expr) void {
        comptime var visitor = Visitor{
            .visitBinaryFn = binary,
            .visitGroupingFn = grouping,
            .visitLiteralFn = literal,
            .visitUnaryFn = unary,
        };

        std.debug.print("hot\n", .{});
        visitor.accept(e);
        std.debug.print("\n", .{});
        return;
    }

    fn binary(comptime visitor: *Visitor, e: *expr.Binary) void {
        paren(visitor, e.op.lexeme, .{ e.left, e.right });
    }

    fn grouping(comptime visitor: *Visitor, e: *expr.Grouping) void {
        paren(
            visitor,
            "group",
            .{e.mid},
        );
    }

    pub fn literal(comptime visitor: *Visitor, e: *expr.Literal) void {
        _ = visitor;

        if (e.v) |v| {
            switch (v.tt) {
                .num => {
                    std.debug.print("{d}", .{tokens.LiteralVal(f64).from_lit(e.v.?.literal.?).val});
                },
                .str => {
                    std.debug.print("{d}", .{tokens.LiteralVal(f64).from_lit(e.v.?.literal.?).val});
                },
                else => unreachable,
            }
        }
    }

    pub fn unary(comptime visitor: *Visitor, e: *expr.Unary) void {
        paren(visitor, e.op.lexeme, .{e.right});
    }

    fn paren(comptime visitor: *Visitor, str: []const u8, exprs: anytype) void {
        const tinfo = @typeInfo(@TypeOf(exprs));
        if (tinfo != .Struct) {
            @compileError("paren->exprs should be a tuple.");
        }

        const fields = tinfo.Struct.fields;

        std.debug.print("(", .{});
        std.debug.print("{s}", .{str});
        inline for (fields) |field| {
            std.debug.print(" ", .{});
            visitor.accept(@field(exprs, field.name));
        }

        std.debug.print(")", .{});
    }
};

test "printer" {
    var lup = tokens.Token{ .lexeme = "-", .tt = .minus, .line = 1 };
    var luetl = tokens.LiteralVal(f64){ .val = 123 };
    var luett = tokens.Token{ .lexeme = "123", .line = 1, .tt = .num, .literal = &luetl.literal };
    var lue = expr.Literal{ .v = luett };
    var lu = expr.Unary{ .op = lup, .right = &lue.e };
    var bop = tokens.Token{ .tt = .star, .lexeme = "*", .line = 1 };
    var rumtl = tokens.LiteralVal(f64){ .val = 45.67 };
    var rumtt = tokens.Token{ .lexeme = "45.67", .line = 1, .tt = .num, .literal = &rumtl.literal };
    var rum = expr.Literal{ .v = rumtt };
    var ru = expr.Grouping{ .mid = &rum.e };
    var e = expr.Binary{ .left = &lu.e, .op = bop, .right = &ru.e };
    ASTPrinter.print(&e.e);
}
