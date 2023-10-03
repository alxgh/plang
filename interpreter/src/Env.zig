const std = @import("std");

pub const ValueType = enum {
    double,
    boolean,
    string,
    nil,
    func,
};

pub const Value = struct {
    t: ValueType,
};

const ValuesType = std.StringArrayHashMap(?*Value);
const Self = @This();

// allocator: std.mem.Allocator,
values: ValuesType,
enclosing: ?*Self,

pub fn init(allocator: std.mem.Allocator, enclosing: ?*Self) Self {
    return .{
        .values = ValuesType.init(allocator),
        .enclosing = enclosing,
    };
}

pub fn deinit(self: *Self, comptime f: fn (*Value) void) void {
    var it = self.values.iterator();
    while (it.next()) |e| {
        if (e.value_ptr.*) |v| {
            f(v);
        }
    }
    self.values.deinit();
    return;
}

pub fn define(self: *Self, name: []const u8, val: ?*Value) !void {
    try self.values.put(name, val);
}

pub fn assign(self: *Self, name: []const u8, val: ?*Value) !?*Value {
    if (self.values.contains(name)) {
        var prev = self.values.get(name);
        try self.values.put(name, val);
        return prev.?;
    }
    if (self.enclosing) |enc| {
        return enc.assign(name, val);
    }
    return error.NotDefined;
}

pub fn assignAtDistance(self: *Self, distance: u64, name: []const u8, val: ?*Value) !?*Value {
    var e = self.ancestor(distance);
    if (e.values.contains(name)) {
        var prev = self.values.get(name);
        try self.values.put(name, val);
        return prev.?;
    }
    if (e.enclosing) |enc| {
        return enc.assign(name, val);
    }
    return error.NotDefined;
}

pub fn get(self: *Self, name: []const u8) ?*Value {
    // runtime error: variable not found???
    if (self.values.get(name)) |v| {
        return v;
    }

    if (self.enclosing) |enc| {
        return enc.get(name);
    }

    return null;
}

pub fn getAtDist(self: *Self, distance: u64, name: []const u8) ?*Value {
    return self.ancestor(distance).values.get(name).?;
}

pub fn ancestor(self: *Self, distance: u64) *Self {
    var e = self;
    for (0..distance) |i| {
        _ = i;
        e = e.enclosing.?;
    }
    return e;
}
