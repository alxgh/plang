const std = @import("std");
const tokens = @import("./tokens.zig");
const reporter = @import("./reporter.zig");

pub const StringLiteral = tokens.LiteralVal([]const u8);
pub const NumberLiteral = tokens.LiteralVal(f64);

pub const sign = enum {
    pos,
    neg,
};

fn is_digit(c: u8) bool {
    return c >= 0x030 and c <= 0x39;
}

fn is_alpha(c: u8) bool {
    return (c >= 0x41 and c <= 0x5A) or (c >= 0x61 and c <= 0x7A) or c == 0x5F;
}

fn is_alpha_numeric(c: u8) bool {
    return is_digit(c) or is_alpha(c);
}

pub const scanner = struct {
    const Self = @This();
    source: []const u8,
    allocator: std.mem.Allocator,
    start: u64 = 0,
    curr: u64 = 0,
    line: u64 = 1,
    tokensList: std.ArrayList(tokens.Token),

    pub fn init(allocator: std.mem.Allocator, src: []const u8) Self {
        return .{
            .source = src,
            .allocator = allocator,
            .tokensList = std.ArrayList(tokens.Token).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.tokensList.items) |tok| {
            switch (tok.tt) {
                .str, .iden => {
                    var strlit = StringLiteral.from_lit(tok.literal.?);
                    self.allocator.free(strlit.val);
                    self.allocator.destroy(strlit);
                },
                .num => {
                    self.allocator.destroy(NumberLiteral.from_lit(tok.literal.?));
                },
                else => {},
            }
        }
        self.tokensList.deinit();
    }

    pub fn scan(self: *Self) !TokenIterator {
        while (!self.finished()) {
            self.start = self.curr;
            try self.scan_tok();
        }
        try self.tokensList.append(tokens.Token{
            .tt = .eof,
            .lexeme = self.source[self.start..self.curr],
            .line = self.line,
        });
        return TokenIterator{
            .buffer = self.tokensList.items,
            .allocator = self.allocator,
        };
    }

    fn finished(self: *Self) bool {
        return self.curr >= self.source.len;
    }

    fn advance(self: *Self) u8 {
        self.curr += 1;
        return self.source[self.curr - 1];
    }

    fn scan_tok(self: *Self) !void {
        var c = self.advance();
        var t: ?struct { tt: tokens.Tokens, l: ?*tokens.literal } = switch (c) {
            '(' => .{ .tt = .l_paren, .l = null },
            ')' => .{ .tt = .r_paren, .l = null },
            '{' => .{ .tt = .l_brace, .l = null },
            '}' => .{ .tt = .r_brace, .l = null },
            ',' => .{ .tt = .comma, .l = null },
            '.' => .{ .tt = .dot, .l = null },
            '-' => .{ .tt = .minus, .l = null },
            '+' => .{ .tt = .plus, .l = null },
            ';' => .{ .tt = .semicolon, .l = null },
            '*' => .{ .tt = .star, .l = null },
            '/' => .{
                .tt = blk: {
                    // if (self.check("/")) {
                    //     while (self.check("/")) {}
                    // }
                    break :blk .slash;
                },
                .l = null,
            },
            '!' => .{ .tt = blk: {
                if (self.check('=')) {
                    break :blk .bang_eq;
                }
                break :blk .bang;
            }, .l = null },
            '=' => .{ .tt = blk: {
                if (self.check('=')) {
                    break :blk .eq_eq;
                }
                break :blk .eq;
            }, .l = null },
            '<' => .{ .tt = blk: {
                if (self.check('=')) {
                    break :blk .lte;
                }
                break :blk .less;
            }, .l = null },
            '>' => .{ .tt = blk: {
                if (self.check('=')) {
                    break :blk .gte;
                }
                break :blk .greater;
            }, .l = null },
            // ignore white space
            ' ' => null,
            '\t' => null,
            '\r' => null,

            else => blk: {
                switch (c) {
                    '\n' => {
                        self.line += 1;
                    },
                    '"' => {
                        try self.str();
                    },
                    0x30...0x39 => {
                        try self.num(.pos);
                    },
                    0x41...0x5A, 0x61...0x7A, 0x5F => {
                        try self.identifier();
                    },
                    else => {
                        reporter.rep_err(self.line, "", "Unexpected char");
                    },
                }
                break :blk null;
            },
        };
        if (t) |tok| {
            try self.add_tok(tok.tt, tok.l);
        }
    }

    fn num(self: *Self, s: sign) !void {
        _ = s;
        while (!self.finished()) {
            if (self.peek()) |peeked| {
                if (is_digit(peeked)) {
                    _ = self.advance();
                    continue;
                }
                break;
            }
        }
        if (self.peek() == '.') {
            if (self.peekn(1)) |p1| {
                if (is_digit(p1)) {
                    _ = self.advance();
                    while (!self.finished()) {
                        if (self.peek()) |peeked| {
                            if (is_digit(peeked)) {
                                _ = self.advance();
                                continue;
                            }
                            break;
                        }
                    }
                }
            }
        }

        var numlit = try self.allocator.create(NumberLiteral);
        numlit.val = try std.fmt.parseFloat(f64, self.source[self.start..self.curr]);
        try self.add_tok(.num, &numlit.literal);
    }

    fn identifier(self: *Self) !void {
        while (!self.finished()) {
            if (self.peek()) |peeked| {
                if (is_alpha_numeric(peeked)) {
                    _ = self.advance();
                } else {
                    break;
                }
            }
        }
        var lit = self.source[self.start..self.curr];
        var tt: tokens.Tokens = .iden;
        if (tokens.keywords.get(lit)) |infered| {
            tt = infered;
        }
        if (tt == .iden) {
            var strlit = try self.allocator.create(StringLiteral);
            var buf = try self.allocator.alloc(u8, lit.len);
            @memcpy(buf, lit);
            strlit.val = buf;

            try self.add_tok(tt, &strlit.literal);
        } else {
            try self.add_tok(tt, null);
        }
    }

    fn str(self: *Self) !void {
        while (!self.finished()) {
            if (self.check('"')) {
                break;
            }
            if (self.check('\n')) {
                reporter.rep_err(self.line, "where", "Expected \" found \n");
                break;
            }
            _ = self.advance();
        }
        var lit = self.source[self.start + 1 .. self.curr - 1];
        var strlit = try self.allocator.create(StringLiteral);
        var buf = try self.allocator.alloc(u8, lit.len);
        @memcpy(buf, lit);
        strlit.val = buf;
        try self.add_tok(.str, &strlit.literal);
    }

    fn check(self: *Self, ch: u8) bool {
        if (self.finished()) {
            return false;
        }
        if (self.peek() != ch) {
            return false;
        }
        self.curr += 1;
        return true;
    }

    fn peek(self: *Self) ?u8 {
        if (self.finished()) return null;
        return self.source[self.curr];
    }

    fn peekn(self: *Self, n: u64) ?u8 {
        if (self.curr + n >= self.source.len) {
            return null;
        }
        return self.source[self.curr + n];
    }

    fn add_tok(self: *Self, tok: tokens.Tokens, literal: ?*tokens.literal) !void {
        try self.tokensList.append(tokens.Token{
            .tt = tok,
            .lexeme = self.source[self.start..self.curr],
            .line = self.line,
            .literal = literal,
        });
    }
};

const Token = tokens.Token;
const TokenIndex = usize;

pub const TokenIterator = struct {
    buffer: []const Token,
    pos: TokenIndex = 0,
    allocator: std.mem.Allocator,

    pub fn next(self: *TokenIterator) ?Token {
        const token = self.peek() orelse return null;
        self.pos += 1;
        return token;
    }

    pub fn finished(self: *TokenIterator) bool {
        return self.pos >= self.buffer.len;
    }

    pub fn peek(self: *TokenIterator) ?Token {
        if (self.finished()) return null;
        return self.buffer[self.pos];
    }

    pub fn prev(self: *TokenIterator) ?Token {
        if (self.pos == 0) {
            return null;
        }
        return self.buffer[self.pos - 1];
    }

    pub fn reset(self: *TokenIterator) void {
        self.pos = 0;
    }

    pub fn seekTo(self: *TokenIterator, pos: TokenIndex) void {
        self.pos = pos;
    }

    pub fn seekBy(self: *TokenIterator, offset: isize) void {
        const new_pos = @as(isize, @bitCast(self.pos)) + offset;
        if (new_pos < 0) {
            self.pos = 0;
        } else {
            self.pos = @as(usize, @intCast(new_pos));
        }
    }
};
