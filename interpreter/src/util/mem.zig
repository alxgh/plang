const std = @import("std");
pub fn Rc(comptime T: type) type {
    return struct {
        const Self = @This();

        allocator: std.mem.Allocator,
        ptr: *T,
        cnt: u64 = 1,

        pub fn init(allocator: std.mem.Allocator, ptr: *T) Self {
            return .{ .allocator = allocator, .ptr = ptr };
        }

        pub fn incr(self: *Self) void {
            self.cnt += 1;
        }

        pub fn decr(self: *Self) void {
            self.cnt -= 1;
            if (self.cnt == 0) {
                self.allocator.destroy(self.ptr);
            }
        }
    };
}

const testing = @import("std").testing;
test "rc" {
    const S = struct {
        d: f64,
        rc: Rc(@This()),
    };
    var s = try testing.allocator.create(S);
    // defer testing.allocator.destroy(s);
    s.rc = Rc(S).init(testing.allocator, s);
    s.rc.incr();
    s.rc.decr();
    s.rc.decr();
}
