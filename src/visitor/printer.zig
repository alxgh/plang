const std = @import("std");
const tokens = @import("../tokens.zig");
const expr = @import("../expr.zig");
const Visitor = @import("./visitor.zig").Visitor(void);
const Self = @This();
pub fn print(e: *expr.Expr) void {
    var visitor = Visitor{
        .ctx = undefined,
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

fn binary(ctx: *anyopaque, visitor: *Visitor, e: *expr.Binary) void {
    _ = ctx;
    paren(visitor, e.op.lexeme, .{ e.left, e.right });
}

fn grouping(ctx: *anyopaque, visitor: *Visitor, e: *expr.Grouping) void {
    _ = ctx;
    paren(
        visitor,
        "group",
        .{e.mid},
    );
}

pub fn literal(ctx: *anyopaque, visitor: *Visitor, e: *expr.Literal) void {
    _ = ctx;
    _ = visitor;

    if (e.v) |v| {
        switch (v.tt) {
            .num => {
                std.debug.print("{d}", .{tokens.LiteralVal(f64).from_lit(e.v.?.literal.?).val});
            },
            .str => {
                std.debug.print("{d}", .{tokens.LiteralVal(f64).from_lit(e.v.?.literal.?).val});
            },
            .true_tok => {
                std.debug.print("true", .{});
            },
            .false_tok => {
                std.debug.print("false", .{});
            },
            else => unreachable,
        }
    }
}

pub fn unary(ctx: *anyopaque, visitor: *Visitor, e: *expr.Unary) void {
    _ = ctx;
    paren(visitor, e.op.lexeme, .{e.right});
}

fn paren(visitor: *Visitor, str: []const u8, exprs: anytype) void {
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
    Self.print(&e.e);
}
