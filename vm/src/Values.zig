const std = @import("std");
pub const Value = f64;
const ArrayList = std.ArrayList(Value);
const Allocator = std.mem.Allocator;

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
