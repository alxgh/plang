const std = @import("std");
const expr = @import("../expr.zig");
const stmt = @import("../stmt.zig");
const tokens = @import("../tokens.zig");
const Env = @import("../Env.zig");
const Rc = @import("../util/mem.zig").Rc;

const ResultType = enum {
    double,
    boolean,
    string,
    nil,
};

fn decr(r: *Result) void {
    switch (r.t) {
        .boolean => @fieldParentPtr(BooleanResult, "r", r).rc.decr(),
        .string => @fieldParentPtr(StringResult, "r", r).rc.decr(),
        .double => @fieldParentPtr(DoubleResult, "r", r).rc.decr(),
        .nil => @fieldParentPtr(NilResult, "r", r).rc.decr(),
    }
}

fn incr(r: *Result) void {
    switch (r.t) {
        .boolean => @fieldParentPtr(BooleanResult, "r", r).rc.incr(),
        .string => @fieldParentPtr(StringResult, "r", r).rc.incr(),
        .double => @fieldParentPtr(DoubleResult, "r", r).rc.incr(),
        .nil => @fieldParentPtr(NilResult, "r", r).rc.incr(),
    }
}

const Result = struct {
    t: ResultType,
    env_val: Env.Value = Env.Value{ .t = .boolean },
};

fn ResultObject(comptime T: type) type {
    return struct {
        const RSelf = @This();
        const RcSelf = Rc(RSelf);

        r: Result,
        val: T,
        rc: RcSelf,

        pub fn alloc(
            allocator: std.mem.Allocator,
            result: Result,
            val: T,
        ) !*RSelf {
            const r = try allocator.create(RSelf);
            r.r = result;
            r.val = val;
            r.rc = RcSelf.init(allocator, r);
            return r;
        }
    };
}

const DoubleValueType = f64;
const BooleanValueType = bool;
const StringValueType = []const u8;

pub const DoubleResult = ResultObject(DoubleValueType);
pub const BooleanResult = ResultObject(BooleanValueType);
pub const StringResult = ResultObject(StringValueType);
pub const NilResult = ResultObject(void);

const Self = @This();

fn doubleRes(self: *Self, val: DoubleValueType) !*DoubleResult {
    return DoubleResult.alloc(self.allocator, .{ .t = .double }, val);
}

fn booleanRes(self: *Self, val: BooleanValueType) !*BooleanResult {
    return BooleanResult.alloc(self.allocator, .{ .t = .boolean }, val);
}

fn strRes(self: *Self, val: StringValueType) !*StringResult {
    return StringResult.alloc(self.allocator, .{ .t = .string }, val);
}

fn nilRes(self: *Self) !*NilResult {
    return NilResult.alloc(self.allocator, .{ .t = .nil }, undefined);
}

fn resToEnv(t: ResultType) Env.ValueType {
    return @enumFromInt(@intFromEnum(t));
}

const Visitor = @import("./visitor.zig").Visitor(*Result);

allocator: std.mem.Allocator,
env: Env,

pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .allocator = allocator,
        .env = Env.init(allocator, null),
    };
}

pub fn interpret(self: *Self, stmts: std.ArrayList(*stmt.Stmt)) void {
    var visitor = Visitor{
        .ctx = self,
        .visitBinaryFn = binary,
        .visitGroupingFn = grouping,
        .visitLiteralFn = literal,
        .visitUnaryFn = unary,
        .visitVariableFn = variable,
        .visitAssignFn = assign,
        .visitLogicalFn = logical,

        .visitPrintStmtFn = printStmt,
        .visitExprStmtFn = exprStmt,
        .visitVarStmtFn = varStmt,
        .visitBlockStmtFn = blockStmt,
        .visitIfStmtFn = ifStmt,
        .visitWhileStmtFn = whileStmt,
    };

    for (stmts.items) |s| {
        var r = visitor.acceptStmt(s);
        decr(r);
    }
}

pub fn deinit(self: *Self) void {
    defer self.env.deinit(envdeinit);
}

fn envdeinit(v: *Env.Value) void {
    decr(@fieldParentPtr(Result, "env_val", v));
}

fn eval(self: *Self, visitor: *Visitor, e: *expr.Expr) *Result {
    _ = self;
    return visitor.acceptExpr(e);
}

fn exprStmt(ctx: *anyopaque, visitor: *Visitor, s: *stmt.Expression) *Result {
    const self: *Self = @ptrCast(@alignCast(ctx));
    return self.eval(visitor, s.e);
}

fn ifStmt(ctx: *anyopaque, visitor: *Visitor, s: *stmt.If) *Result {
    const self: *Self = @ptrCast(@alignCast(ctx));

    var cond = self.eval(visitor, s.cond);
    defer decr(cond);

    if (cond.t == .boolean) {
        var condBool = @fieldParentPtr(BooleanResult, "r", cond);
        if (condBool.val) {
            return visitor.acceptStmt(s.then_branch);
        }
    }

    if (s.else_branch) |else_branch| {
        return visitor.acceptStmt(else_branch);
    }

    if (nilRes(self)) |r| {
        return &r.r;
    } else |_| {
        @panic("err");
    }
}

fn whileStmt(ctx: *anyopaque, visitor: *Visitor, s: *stmt.While) *Result {
    const self: *Self = @ptrCast(@alignCast(ctx));
    var out_res: *Result = undefined;
    if (nilRes(self)) |r| {
        out_res = &r.r;
    } else |_| {
        @panic("err");
    }

    while (true) {
        var cond = self.eval(visitor, s.cond);
        defer decr(cond);

        if (cond.t != .boolean) {
            return out_res;
        }

        var condBool = @fieldParentPtr(BooleanResult, "r", cond);
        if (!condBool.val) {
            return out_res;
        }
        var lr = visitor.acceptStmt(s.loop_statement);
        decr(lr);
    }
    unreachable;
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
        .nil => {
            std.debug.print("nil\n", .{});
        },
    }
    return out;
}

fn varStmt(ctx: *anyopaque, visitor: *Visitor, s: *stmt.Var) *Result {
    const self: *Self = @ptrCast(@alignCast(ctx));
    var value: ?*Env.Value = null;
    var res: *Result = undefined;
    if (s.initializer != null) {
        res = self.eval(visitor, s.initializer.?);
        res.env_val.t = resToEnv(res.t);
        incr(res);
        value = &res.env_val;
    }
    self.env.define(s.name.lexeme, value) catch undefined;
    return res;
}

fn blockStmt(ctx: *anyopaque, visitor: *Visitor, s: *stmt.Block) *Result {
    const self: *Self = @ptrCast(@alignCast(ctx));
    var prev_env = self.env;
    defer {
        self.env = prev_env;
    }
    self.env = Env.init(self.allocator, &prev_env);
    defer self.env.deinit(envdeinit);
    for (s.statements.items) |statement| {
        var r = visitor.acceptStmt(statement);
        decr(r);
    }
    var r = nilRes(self) catch @panic("shit");
    return &r.r;
}

fn variable(ctx: *anyopaque, visitor: *Visitor, e: *expr.Variable) *Result {
    _ = visitor;

    const self: *Self = @ptrCast(@alignCast(ctx));
    var value = self.env.get(e.name.?.lexeme);
    if (value) |val| {
        var r = @fieldParentPtr(Result, "env_val", val);
        incr(r);
        return r;
        // return switch (val.t) {
        //     .boolean => @fieldParentPtr(BooleanResult, "r", r),
        //     .string => @fieldParentPtr(StringResult, "r", r),
        //     .double => @fieldParentPtr(DoubleResult, "r", r),
        // };
    }
    return undefined;
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
                left_double.rc.decr();
                right_double.rc.decr();
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
                left_boolean.rc.decr();
                right_boolean.rc.decr();
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
        .string, .nil => {},
    }
    unreachable;
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
                if (booleanRes(self, !boolean.val)) |v| {
                    right = &v.r;
                } else |_| {
                    @panic("shit!");
                }
                boolean.rc.decr();
            }
        },
        .minus => {
            if (right.t == .double) {
                var double = @fieldParentPtr(DoubleResult, "r", right);
                if (doubleRes(self, -double.val)) |v| {
                    right = &v.r;
                } else |_| {
                    @panic("shit!");
                }
                double.rc.decr();
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

fn assign(ctx: *anyopaque, visitor: *Visitor, e: *expr.Assign) *Result {
    const self: *Self = @ptrCast(@alignCast(ctx));
    var res = self.eval(visitor, e.value);
    res.env_val.t = resToEnv(res.t);
    var value = &res.env_val;
    var prev = self.env.assign(e.name.?.lexeme, value) catch @panic("unhandled");
    if (prev) |val| {
        var r = @fieldParentPtr(Result, "env_val", val);
        decr(r);
    }
    incr(res);
    return res;
}

fn logical(ctx: *anyopaque, visitor: *Visitor, e: *expr.Logical) *Result {
    const self: *Self = @ptrCast(@alignCast(ctx));
    var left = self.eval(visitor, e.left);
    var right = self.eval(visitor, e.right);
    defer {
        decr(right);
        decr(left);
    }
    if (right.t != .boolean or left.t != .boolean) {
        if (self.nilRes()) |res| {
            return &res.r;
        } else |_| {
            @panic("err");
        }
    }
    var left_boolean = @fieldParentPtr(BooleanResult, "r", left);
    var right_boolean = @fieldParentPtr(BooleanResult, "r", right);
    var r = switch (e.op.tt) {
        .and_tok => left_boolean.val and right_boolean.val,
        .or_tok => left_boolean.val or right_boolean.val,
        else => unreachable,
    };
    if (self.booleanRes(r)) |res| {
        return &res.r;
    } else |_| {
        @panic("err");
    }
    unreachable;
}
