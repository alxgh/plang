pub const Tokens = enum(u64) {
    l_paren = 0,
    r_paren,
    l_brace,
    r_brace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,

    at, // @
    bang, // !
    bang_eq,
    eq,
    eq_eq,
    greater,
    gte,
    less,
    lte,

    num,
    iden,
    str,

    class,
    fn_tok,
    and_tok,
    or_tok,
    if_tok,
    else_tok,
    false_tok,
    true_tok,
    for_tok,
    nil_tok,
    ret_tok,
    super_tok,
    this,
    var_tok,
    while_tok,

    eof,
};

pub const keywords = std.ComptimeStringMap(Tokens, .{
    .{ "and", .and_tok },
    .{ "or", .or_tok },
    .{ "class", .class },
    .{ "if", .if_tok },
    .{ "else", .else_tok },
    .{ "for", .for_tok },
    .{ "fn", .fn_tok },
    .{ "nil", .nil_tok },
    .{ "ret", .ret_tok },
    .{ "super", .super_tok },
    .{ "this", .this },
    .{ "true", .true_tok },
    .{ "false", .false_tok },
    .{ "var", .var_tok },
    .{ "while", .while_tok },
});

pub const literal = struct {};

pub fn LiteralVal(comptime T: type) type {
    return struct {
        const Self = @This();
        val: T,
        literal: literal = literal{},

        pub fn to_lit(self: *Self) literal {
            return self.literal;
        }

        pub fn from_lit(lit: *literal) *Self {
            return @fieldParentPtr(Self, "literal", lit);
        }
    };
}

pub const Token = struct {
    const Self = @This();
    tt: Tokens,
    lexeme: []const u8,
    literal: ?*literal = undefined, // Object in Java...
    line: u64,

    pub fn write(self: Self, writer: anytype) @TypeOf(writer).Error!void {
        writer.print("{} {} {}", .{ self.ff, self.lexeme, self.line });
    }
};

const std = @import("std");
