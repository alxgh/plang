const std = @import("std");
const Allocator = std.mem.Allocator;
const perfC = @cImport({
    @cInclude("perf.c");
});

fn isDigit(c: u8) bool {
    return c >= 0x030 and c <= 0x39;
}

fn isAlpha(c: u8) bool {
    return (c >= 0x41 and c <= 0x5A) or (c >= 0x61 and c <= 0x7A) or c == 0x5F;
}

fn isAlphaNumeric(c: u8) bool {
    return isDigit(c) or isAlpha(c);
}

const Error = error{
    UnterminatedString,
};

pub const TokenType = enum {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens.
    Bang,
    BangEq,
    Eq,
    EqEq,
    Greater,
    Gte,
    Less,
    Lte,
    // Literals.
    Iden,
    Str,
    Num,
    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fn,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    EOF,
};

const identifierMap = std.ComptimeStringMap(TokenType, .{
    .{ "while", .While },
    .{ "var", .Var },
    .{ "true", .True },
    .{ "this", .This },
    .{ "super", .Super },
    .{ "return", .Return },
    .{ "print", .Print },
    .{ "or", .Or },
    .{ "nil", .Nil },
    .{ "if", .If },
    .{ "fn", .Fn },
    .{ "for", .For },
    .{ "false", .False },
    .{ "class", .Class },
    .{ "and", .And },
    .{ "else", .Else },
});

const LiteralType = enum {
    String,
    Double,
    Char,
};

const TokenLiteral = union(LiteralType) {
    String: []const u8,
    Double: f64,
    Char: u8,
};

pub const Token = struct {
    t: TokenType,
    line: u64,
    lexeme: []const u8,
    literal: ?TokenLiteral = null,
};

const Self = @This();

allocator: Allocator,
source: []const u8,
idx: usize = 0,
line: u64 = 1,
start: usize = 0,
ended: bool = false,

pub fn init(allocator: Allocator, source: []const u8) Self {
    return .{
        .allocator = allocator,
        .source = source,
    };
}

pub fn deinit(self: *Self) void {
    _ = self;
}

pub fn next(self: *Self) !?Token {
    if (self.isAtEnd()) {
        if (self.ended) {
            return null;
        }
        self.ended = true;
        return self.make(.EOF, null);
    }
    var c: ?u8 = null;

    var going = true;
    while (going) {
        self.start = self.idx;
        c = self.advance();
        if (c == null) {
            return null;
        }
        switch (c.?) {
            ' ', '\r', '\t' => {},
            '\n' => {
                self.line += 1;
            },
            else => {
                going = false;
            },
        }
    }
    var token_type: ?TokenType = switch (c.?) {
        '(' => .LeftParen,
        ')' => .RightParen,
        '{' => .LeftBrace,
        '}' => .RightBrace,
        ';' => .Semicolon,
        ',' => .Comma,
        '.' => .Dot,
        '-' => .Minus,
        '+' => .Plus,
        '/' => blk: {
            if (self.match('/')) {
                while (!self.isAtEnd() and self.peek() != '\n') {
                    _ = self.advance();
                }
                break :blk null;
            }
            break :blk .Slash;
        },
        '*' => .Star,
        '!' => self.whether('=', .BangEq, .Bang),
        '=' => self.whether('=', .EqEq, .Eq),
        '<' => self.whether('=', .Lte, .Less),
        '>' => self.whether('=', .Gte, .Greater),
        '"' => return self.string(),
        0x30...0x39 => return self.num(),
        0x41...0x5A, 0x61...0x7A, 0x5F => return self.identifier(),
        else => null,
    };
    if (token_type) |tt| {
        return self.make(tt, null);
    }
    return null;
}

fn num(self: *Self) !?Token {
    while (!self.isAtEnd() and isDigit(self.peek())) {
        _ = self.advance();
    }
    if (!self.isAtEnd() and self.peek() == '.') {
        _ = self.advance();
        while (!self.isAtEnd() and isDigit(self.peek())) {
            _ = self.advance();
        }
    }
    return self.make(.Num, TokenLiteral{ .Double = try std.fmt.parseFloat(f64, self.source[self.start..self.idx]) });
}

fn identifier(self: *Self) !?Token {
    while (isAlphaNumeric(self.peek())) {
        _ = self.advance();
    }
    // TODO
    return self.make(identifierMap.get(self.source[self.start..self.idx]) orelse .Iden, null);
}

fn string(self: *Self) !?Token {
    var slash = false;
    var start = self.idx;
    while (!self.isAtEnd() and (!slash and self.peek() != '"')) {
        slash = false;
        const curr = self.advance();
        if (curr == '\\') {
            slash = true;
        }
    }
    if (self.isAtEnd()) {
        return Error.UnterminatedString;
    }
    const str = self.source[start..self.idx];
    _ = self.advance();
    return self.make(.Str, TokenLiteral{ .String = str });
}

fn make(self: *Self, tt: TokenType, literal: ?TokenLiteral) Token {
    return Token{
        .lexeme = self.source[self.start..self.idx],
        .line = self.line,
        .t = tt,
        .literal = literal,
    };
}

fn whether(self: *Self, next_char: u8, accept: TokenType, reject: TokenType) TokenType {
    if (self.match(next_char)) {
        return accept;
    }
    return reject;
}

fn advance(self: *Self) ?u8 {
    if (self.isAtEnd()) {
        return null;
    }
    self.idx += 1;
    return self.source[self.idx - 1];
}

fn match(self: *Self, char: u8) bool {
    if (self.isAtEnd() or self.source[self.idx] != char) return false;
    _ = self.advance().?;
    return true;
}

fn peek(self: *Self) u8 {
    return self.source[self.idx];
}

fn isAtEnd(self: *Self) bool {
    return self.idx >= self.source.len;
}
