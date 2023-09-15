const std = @import("std");
const expr = @import("../expr.zig");
const stmt = @import("../stmt.zig");
const tokens = @import("../tokens.zig");

const ResultType = enum {
    double,
    boolean,
    string,
};

const Result = struct {
    t: ResultType,
};

fn ResultObject(comptime T: type) type {
    return struct {
        r: Result,
        val: T,
    };
}

const DoubleValueType = f64;
const BooleanValueType = bool;
const StringValueType = []const u8;

pub const DoubleResult = ResultObject(DoubleValueType);
pub const BooleanResult = ResultObject(BooleanValueType);
pub const StringResult = ResultObject(StringValueType);

const Self = @This();

fn doubleRes(self: *Self, val: DoubleValueType) !*DoubleResult {
    var double_result = try self.allocator.create(DoubleResult);
    double_result.r = Result{ .t = .double };
    double_result.val = val;
    return double_result;
}

fn booleanRes(self: *Self, val: BooleanValueType) !*BooleanResult {
    var double_result = try self.allocator.create(BooleanResult);
    double_result.r = Result{ .t = .boolean };
    double_result.val = val;
    return double_result;
}

fn strRes(self: *Self, val: StringValueType) !*StringResult {
    var str_result = try self.allocator.create(StringResult);
    str_result.r = Result{ .t = .string };
    str_result.val = val;
    return str_result;
}

const Visitor = @import("./visitor.zig").Visitor(*Result);

allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .allocator = allocator,
    };
}

pub fn interpret(self: *Self, stmts: std.ArrayList(*stmt.Stmt)) void {
    var visitor = Visitor{
        .ctx = self,
        .visitBinaryFn = binary,
        .visitGroupingFn = grouping,
        .visitLiteralFn = literal,
        .visitUnaryFn = unary,

        .visitPrintStmtFn = printStmt,
        .visitExprStmtFn = exprStmt,
    };

    for (stmts.items) |s| {
        var r = visitor.acceptStmt(s);
        switch (r.t) {
            .boolean => self.allocator.destroy(@fieldParentPtr(BooleanResult, "r", r)),
            .string => self.allocator.destroy(@fieldParentPtr(StringResult, "r", r)),
            .double => self.allocator.destroy(@fieldParentPtr(DoubleResult, "r", r)),
        }
    }
}

fn eval(self: *Self, visitor: *Visitor, e: *expr.Expr) *Result {
    _ = self;
    return visitor.acceptExpr(e);
}

fn exprStmt(ctx: *anyopaque, visitor: *Visitor, s: *stmt.Expression) *Result {
    const self: *Self = @ptrCast(@alignCast(ctx));
    return self.eval(visitor, s.e);
}

fn printStmt(ctx: *anyopaque, visitor: *Visitor, s: *stmt.Print) *Result {
    const self: *Self = @ptrCast(@alignCast(ctx));
    var out = self.eval(visitor, s.e);
    // printing somethign here...
    switch (out.t) {
        .double => {
            std.debug.print("{d}\n", .{@fieldParentPtr(DoubleResult, "r", out).val});
        },
        .boolean => {
            std.debug.print("{}\n", .{@fieldParentPtr(BooleanResult, "r", out).val});
        },
        .string => {
            std.debug.print("{s}\n", .{@fieldParentPtr(StringResult, "r", out).val});
        },
    }
    return out;
}

fn binary(ctx: *anyopaque, visitor: *Visitor, e: *expr.Binary) *Result {
    const self: *Self = @ptrCast(@alignCast(ctx));
    var left = eval(self, visitor, e.left);
    var right = eval(self, visitor, e.right);

    if (left.t != right.t) {
        // TODO: error handling mechanism.
        return undefined;
    }

    switch (left.t) {
        .double => {
            var left_double = @fieldParentPtr(DoubleResult, "r", left);
            var right_double = @fieldParentPtr(DoubleResult, "r", right);
            defer {
                self.allocator.destroy(left_double);
                self.allocator.destroy(right_double);
            }
            switch (e.op.tt) {
                else => unreachable,
                .minus, .plus, .star, .slash => {
                    var res = doubleRes(self, switch (e.op.tt) {
                        .minus => left_double.val - right_double.val,
                        .plus => left_double.val + right_double.val,
                        .star => left_double.val * right_double.val,
                        .slash => left_double.val / right_double.val,
                        else => unreachable,
                    }) catch return undefined;
                    return &res.r;
                },
                .greater, .gte, .less, .lte, .bang_eq, .eq_eq => {
                    var res = booleanRes(self, switch (e.op.tt) {
                        .greater => left_double.val > right_double.val,
                        .gte => left_double.val >= right_double.val,
                        .less => left_double.val < right_double.val,
                        .lte => left_double.val <= right_double.val,
                        .bang_eq => left_double.val != right_double.val,
                        .eq_eq => left_double.val == right_double.val,
                        else => unreachable,
                    }) catch return undefined;
                    return &res.r;
                },
            }
        },
        .boolean => {
            var left_boolean = @fieldParentPtr(BooleanResult, "r", left);
            var right_boolean = @fieldParentPtr(BooleanResult, "r", right);
            defer {
                self.allocator.destroy(left_boolean);
                self.allocator.destroy(right_boolean);
            }
            switch (e.op.tt) {
                .bang_eq, .eq_eq => {
                    if (booleanRes(self, switch (e.op.tt) {
                        .bang_eq => left_boolean.val != right_boolean.val,
                        .eq_eq => left_boolean.val == right_boolean.val,
                        else => unreachable,
                    })) |res| {
                        return &res.r;
                    } else |_| {
                        return undefined;
                    }
                },
                else => unreachable,
            }
        },
        .string => {},
    }
    return undefined;
}

fn grouping(ctx: *anyopaque, visitor: *Visitor, e: *expr.Grouping) *Result {
    const self: *Self = @ptrCast(@alignCast(ctx));
    return eval(self, visitor, e.mid);
}

fn unary(ctx: *anyopaque, visitor: *Visitor, e: *expr.Unary) *Result {
    const self: *Self = @ptrCast(@alignCast(ctx));
    var right = eval(self, visitor, e.right);
    switch (e.op.tt) {
        .bang => {
            if (right.t == .boolean) {
                var boolean = @fieldParentPtr(BooleanResult, "r", right);
                boolean.val = !boolean.val;
            }
        },
        .minus => {
            if (right.t == .double) {
                var double = @fieldParentPtr(DoubleResult, "r", right);
                double.val *= -1;
            }
        },
        else => unreachable,
    }
    return right;
}

fn literal(ctx: *anyopaque, visitor: *Visitor, e: *expr.Literal) *Result {
    const self: *Self = @ptrCast(@alignCast(ctx));
    _ = visitor;
    switch (e.v.?.tt) {
        .str => {
            var lit = tokens.StringLiteral.from_lit(e.v.?.literal.?);
            if (strRes(self, lit.val)) |res| {
                return &res.r;
            } else |_| {
                return undefined;
            }
        },
        .num => {
            var lit = tokens.NumberLiteral.from_lit(e.v.?.literal.?);
            if (doubleRes(self, lit.val)) |res| {
                return &res.r;
            } else |_| {
                return undefined;
            }
        },
        .true_tok => {
            if (booleanRes(self, true)) |res| {
                return &res.r;
            } else |_| {
                @panic("sakjdkjalsjd");
            }
        },
        .false_tok => {
            if (booleanRes(self, false)) |res| {
                return &res.r;
            } else |_| {
                @panic("sakjdkjalsjd");
            }
        },
        else => unreachable,
    }
    return undefined;
}
