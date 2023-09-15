const std = @import("std");

pub const ValueType = enum {
    double,
    boolean,
    string,
};

pub const Value = struct {
    t: ValueType,
};

const ValuesType = std.StringArrayHashMap(?*Value);
const Self = @This();

// allocator: std.mem.Allocator,
values: ValuesType,

pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .values = ValuesType.init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    _ = self;
    return;
}

pub fn define(self: *Self, name: []const u8, val: ?*Value) !void {
    try self.values.put(name, val);
}

pub fn get(self: *Self, name: []const u8) ?*Value {
    // runtime error: variable not found???
    if (self.values.get(name)) |v| {
        return v;
    }
    return null;
}
