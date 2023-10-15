const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Value = union(enum) {
    Bool: bool,
    Number: f64,
};

pub fn numberValue(val: f64) Value {
    return .{ .Number = val };
}

pub fn boolValue(val: bool) Value {
    return .{ .Bool = val };
}

const ArrayList = std.ArrayList(Value);

const Self = @This();

allocator: Allocator,
values: ArrayList,

pub fn init(allocator: Allocator) Self {
    return .{
        .allocator = allocator,
        .values = ArrayList.init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.values.deinit();
}

pub fn write(self: *Self, value: Value) Allocator.Error!usize {
    try self.values.append(value);
    return self.values.items.len - 1;
}