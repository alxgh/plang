pub fn rep_err(line: u64, where: []const u8, msg: []const u8) void {
    report("ERROR", line, where, msg);
}

pub fn report(level: []const u8, line: u64, where: []const u8, msg: []const u8) void {
    std.debug.print("{s}: (line: {}) -> {s}: {s}\n", .{ level, line, where, msg });
}

const std = @import("std");
