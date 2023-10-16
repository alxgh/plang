const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Value = union(enum) {
    Bool: bool,
    Number: f64,
    Object: *Object,
};

pub const Object = union(enum) {
    String: StringObject,
};

pub const StringObject = []const u8;

pub fn numberValue(val: f64) Value {
    return .{ .Number = val };
}

pub fn boolValue(val: bool) Value {
    return .{ .Bool = val };
}

pub fn objValue(obj: *Object) Value {
    return .{ .Object = obj };
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

pub fn print(writer: anytype, v: Value) !void {
    switch (v) {
        .Object => |obj| {
            switch (obj.*) {
                .String => |str| try writer.print("{s}\n", .{str}),
            }
        },
        else => try writer.print("{}\n", .{v}),
    }
}
