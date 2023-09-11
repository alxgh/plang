const std = @import("std");
const expr = @import("../expr.zig");
const Visitor = @import("./visitor.zig");

const Self = @This();

pub fn run(e: *expr.Expr) void {
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

fn binary(visitor: *Visitor, e: *expr.Binary) void {
    _ = e;
    _ = visitor;
}

fn grouping(visitor: *Visitor, e: *expr.Grouping) void {
    _ = e;
    _ = visitor;
}

fn unary(visitor: *Visitor, e: *expr.Unary) void {
    _ = e;
    _ = visitor;
}

fn literal(visitor: *Visitor, e: *expr.Literal) void {
    _ = e;
    _ = visitor;
}
