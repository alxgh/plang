const std = @import("std");
const os = @import("os");
const scanner = @import("./scanner.zig");
const parser = @import("./parser.zig");
const ASTPrinter = @import("./visitor/printer.zig");
const Interpreter = @import("./visitor/interpreter.zig");

fn run(data: []const u8) !void {
    std.debug.print("runningg....", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        var check = gpa.deinit();
        if (check == .leak) {
            @panic("Mem leak");
        }
    }
    var sc = scanner.scanner.init(allocator, data);
    defer sc.deinit();
    var toks = try sc.scan();

    while (true) {
        if (toks.next()) |tok| {
            std.debug.print("{}\n ", .{tok});
            if (tok.literal) |lit| {
                switch (tok.tt) {
                    .str, .iden => {
                        std.debug.print("value: {s}\n", .{scanner.StringLiteral.from_lit(lit).val});
                    },
                    .num => {
                        std.debug.print("value: {d}\n", .{scanner.NumberLiteral.from_lit(lit).val});
                    },
                    else => {},
                }
            }
        } else {
            break;
        }
    }

    toks.reset();
    var p = parser.init(allocator, &toks);
    defer {
        p.deinit() catch |err| {
            @panic(@errorName(err));
        };
    }

    var exp = p.parse() catch {
        std.debug.print("Parser faced error: {s} -> {?}", .{ p.err_msg, p.err_token });
        return;
    };

    ASTPrinter.print(exp);
    var interpreter = Interpreter.init(allocator);
    var res = interpreter.parse(exp);
    switch (res.t) {
        .double => {
            std.debug.print("final res: {}\n", .{@fieldParentPtr(Interpreter.DoubleResult, "r", res)});
        },
        .boolean => {
            std.debug.print("final res: {}\n", .{@fieldParentPtr(Interpreter.BooleanResult, "r", res)});
        },
        else => undefined,
    }
}

fn run_file(file_name: []const u8) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        var check = gpa.deinit();
        if (check == .leak) {
            @panic("Mem leak");
        }
    }
    var file = try std.fs.cwd().openFile(file_name, .{});
    var data = try file.reader().readAllAlloc(allocator, 1 * 1000 * 1000);
    defer allocator.free(data);
    try run(data);
}

fn run_prompt() !void {
    const stdin = std.io.getStdIn().reader();
    var bw = std.io.bufferedReader(stdin);
    const reader = bw.reader();
    var buffer: [1000]u8 = undefined;
    while (true) {
        var bytes_read = try reader.readUntilDelimiter(&buffer, '\n');
        std.debug.print("given prompt: {s}\n", .{bytes_read});
        try run(bytes_read);
    }
}

pub fn main() !void {
    var args = std.process.args();
    _ = args.skip();
    var len = args.inner.count;
    if (len > 2) {
        std.debug.print("Usage: plan [script]", .{});
    }
    if (args.next()) |file_name| {
        std.debug.print("file name: {s}", .{file_name});
        try run_file(file_name);
    } else {
        std.debug.print("prompt mode!", .{});
        try run_prompt();
    }
}
