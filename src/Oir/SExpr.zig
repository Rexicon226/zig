//! Represents a S-Expression

tag: Tag,
data: Data,

const Data = union(enum) {
    atom: []const u8,
    list: []const SExpr,
};

pub const Parser = struct {
    buf: []const u8,
    index: u32 = 0,

    pub fn parse(parser: *Parser, allocator: std.mem.Allocator) !SExpr {
        while (parser.index < parser.buf.len) {
            const c = parser.eat();
            switch (c) {
                '(' => {
                    // we expect to have a expression type next, i.e (mul ...
                    const tag_start = parser.index;
                    while (parser.peak() != ' ') : (parser.index += 1) {
                        if (parser.index == parser.buf.len) {
                            @panic("expected space after tag expression");
                        }
                    }
                    const tag_end = parser.index;
                    const tag_string = parser.buf[tag_start..tag_end];

                    const tag = std.meta.stringToEnum(Tag, tag_string) orelse
                        std.debug.panic("unknown tag: '{s}'", .{tag_string});

                    // next we need a list of operands to that tag, i.e (mul ?x 2)
                    var expr_list: std.ArrayListUnmanaged(SExpr) = .{};
                    while (parser.peak() != ')') {
                        if (parser.index == parser.buf.len) {
                            @panic("no ending ')'");
                        }
                        if (parser.peak() != ' ') {
                            @panic("expected space after arg expr");
                        }
                        _ = parser.eat();
                        const expr = try parser.parse(allocator);
                        try expr_list.append(allocator, expr);
                    }
                    assert(parser.eat() == ')');
                    const list = try expr_list.toOwnedSlice(allocator);
                    if (list.len == 0)
                        std.debug.panic("tag '{s}' needs some arguments", .{@tagName(tag)});
                    return .{ .tag = tag, .data = .{ .list = list } };
                },
                '?', '0'...'9' => {
                    // this is either a e.g ?x or a constant e.g 10
                    const isChar = std.ascii.isAlphabetic(parser.peak());
                    if (isChar) {
                        // this must be a variable declartion
                        assert(c == '?'); // you must have a ? before a variable

                        // find the end of it
                        const ident_start = parser.index - 1; // include the '?' as it's used further in the pipeline
                        while (!isKeyChar(parser.peak())) : (parser.index += 1) {}
                        const ident_end = parser.index;

                        const ident = parser.buf[ident_start..ident_end];
                        return .{ .tag = .constant, .data = .{ .atom = ident } };
                    } else {
                        assert(std.ascii.isDigit(c)); // sanity check

                        const constant_start = parser.index - 1; // include the first character
                        while (!isKeyChar(parser.peak())) : (parser.index += 1) {}
                        const constant_end = parser.index;

                        const constant = parser.buf[constant_start..constant_end];
                        for (constant) |d| assert(std.ascii.isDigit(d)); // everything must be a digit

                        return .{ .tag = .constant, .data = .{ .atom = constant } };
                    }
                },
                else => std.debug.panic("unexpected token: '{c}'", .{c}),
            }
        }
        @panic("unexpected end of expression");
    }

    inline fn peak(parser: *const Parser) u8 {
        return parser.buf[parser.index];
    }

    inline fn eat(parser: *Parser) u8 {
        const char = parser.peak();
        parser.index += 1;
        return char;
    }

    inline fn isKeyChar(c: u8) bool {
        return c == ' ' or c == '(' or c == ')';
    }
};

pub fn format(
    expr: SExpr,
    comptime fmt: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    assert(fmt.len == 0);

    switch (expr.data) {
        .atom => |name| try writer.writeAll(name),
        .list => |list| {
            try writer.print("({s} ", .{@tagName(expr.tag)});
            for (list, 0..) |sub_expr, i| {
                try writer.print("{}", .{sub_expr});
                if (i < list.len - 1) try writer.writeAll(" ");
            }
            try writer.writeAll(")");
        },
    }
}

const SExpr = @This();
const std = @import("std");
const Oir = @import("../Oir.zig");

const assert = std.debug.assert;

const Node = Oir.Node;
const Tag = Node.Tag;
