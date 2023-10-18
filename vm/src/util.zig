const std = @import("std");

pub const Color = enum(i64) {
    Reset = 0,
    Default = 39,
    Black = 30,
    DarkRed = 31,
    DarkGreen = 32,
    DarkYellow = 33,
    DarkBlue = 34,
    DarkMagenta = 35,
    DarkCyan = 36,
    LightGray = 37,
    DarkGray = 90,
    Red = 91,
    Green = 92,
    Orange = 93,
    Blue = 94,
    Magenta = 95,
    Cyan = 96,
    White = 97,
    pub fn char(comptime c: Color) []const u8 {
        return comptime std.fmt.comptimePrint("\u{001B}[{}m", .{@intFromEnum(c)});
    }

    pub fn wrap(comptime c: Color, comptime text: []const u8) []const u8 {
        return comptime std.fmt.comptimePrint("{s}{s}{s}", .{ c.char(), text, Color.Reset.char() });
    }

    pub fn bg(comptime c: Color) []const u8 {
        return comptime std.fmt.comptimePrint("\u{001B}[{}m", .{if (c == .Reset) 0 else @intFromEnum(c) + 10});
    }

    pub fn bgWrap(comptime c: Color, comptime text: []const u8) []const u8 {
        return comptime std.fmt.comptimePrint("{s} {s} {s}", .{ c.bg(), text, Color.Reset.bg() });
    }
};
