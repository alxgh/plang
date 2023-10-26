const std = @import("std");
const Values = @import("Values.zig");
const Chunk = @import("Chunk.zig");
const Allocator = std.mem.Allocator;

const Self = @This();
allocator: Allocator,
root: ?*Values.Object = null,

pub fn init(allocator: Allocator) Self {
    return .{
        .allocator = allocator,
    };
}

pub fn allocObj(self: *Self) !*Values.Object {
    var obj = try self.allocator.create(Values.Object);
    var po = self.root;
    obj.*.parent = po;
    self.root = obj;
    return obj;
}

pub fn allocStr(self: *Self, str: Values.StringObject) !*Values.Object {
    var obj = try self.allocObj();
    var str_obj = try self.allocator.alloc(u8, str.len);
    std.mem.copy(u8, str_obj, str);
    obj.*.value = .{ .String = str_obj };
    return obj;
}

pub fn allocFunction(self: *Self, name: Values.StringObject, arity: u64, allocator: ?Allocator) !*Values.Object {
    var func_allocator = self.allocator;
    if (allocator) |all| {
        func_allocator = all;
    }
    var obj = try self.allocStr(name);
    var str_obj = obj.value.String;
    var chunk = try self.allocator.create(Chunk);
    chunk.* = Chunk.init(func_allocator);
    obj.value = .{
        .Function = .{
            .name = str_obj,
            .arity = arity,
            .chunk = chunk,
        },
    };
    return obj;
}

pub fn deinit(self: *Self) void {
    var obj = self.root;
    while (true) {
        if (obj) |o| {
            switch (o.value) {
                .String => |str| {
                    self.allocator.free(str);
                },
                .Function => |func| {
                    func.chunk.deinit();
                    self.allocator.destroy(func.chunk);
                    self.allocator.free(func.name);
                },
            }
            obj = o.parent;
            self.allocator.destroy(o);
            continue;
        }
        break;
    }
}
