source: [*:0]const u8,
operands: std.StringHashMapUnmanaged(Operand),

pub const Operand = union(enum) {
    register: Register,
};

pub fn nextInstruction(as: *Assemble) !?Instruction {
    const original_source = while (true) {
        const original_source = as.source;
        var token_buf: [token_buf_len]u8 = undefined;
        const source_token = try as.nextToken(&token_buf, .{});
        switch (source_token.len) {
            0 => return null,
            else => switch (source_token[0]) {
                else => break original_source,
                '\n', ';' => {},
            },
        }
    };

    log.debug(
        \\.
        \\=========================
        \\= Assembling "{f}"
        \\=========================
        \\
    , .{std.zig.fmtString(std.mem.span(original_source))});
    for (matchers) |matcher| {
        as.source = original_source;
        if (try matcher(as)) |result| return result;
    }
    as.source = original_source;
    log.debug("Nothing matched!\n", .{});
    return error.InvalidSyntax;
}

fn zonCast(comptime Result: type, zon_value: anytype, symbols: anytype) Result {
    const ZonValue = @TypeOf(zon_value);
    const Symbols = @TypeOf(symbols);
    switch (@typeInfo(ZonValue)) {
        .void, .bool, .int, .float, .pointer, .comptime_float, .comptime_int, .@"enum" => return zon_value,
        .@"struct" => |zon_struct| switch (@typeInfo(Result)) {
            .pointer => |result_pointer| {
                comptime assert(result_pointer.size == .slice and result_pointer.is_const);
                const elems = comptime blk: {
                    var temp_elems: [zon_value.len]result_pointer.child = undefined;
                    for (&temp_elems, zon_value) |*elem, zon_elem| elem.* = zonCast(result_pointer.child, zon_elem, symbols);
                    break :blk temp_elems;
                };
                return &elems;
            },
            .@"struct" => |result_struct| {
                comptime var used_zon_fields = 0;
                var result: Result = undefined;
                inline for (result_struct.fields) |result_field| @field(result, result_field.name) = if (@hasField(ZonValue, result_field.name)) result: {
                    used_zon_fields += 1;
                    break :result zonCast(@FieldType(Result, result_field.name), @field(zon_value, result_field.name), symbols);
                } else result_field.defaultValue() orelse @compileError(std.fmt.comptimePrint("missing zon field '{s}': {} <- {any}", .{ result_field.name, Result, zon_value }));
                if (used_zon_fields != zon_struct.fields.len) @compileError(std.fmt.comptimePrint("unused zon field: {} <- {any}", .{ Result, zon_value }));
                return result;
            },
            .@"union" => {
                if (zon_struct.fields.len != 1) @compileError(std.fmt.comptimePrint("{} <- {any}", .{ Result, zon_value }));
                const field_name = zon_struct.fields[0].name;
                return @unionInit(
                    Result,
                    field_name,
                    zonCast(@FieldType(Result, field_name), @field(zon_value, field_name), symbols),
                );
            },
            else => @compileError(std.fmt.comptimePrint("unsupported zon type: {} <- {any}", .{ Result, zon_value })),
        },
        .enum_literal => if (@hasField(Symbols, @tagName(zon_value))) {
            const symbol = @field(symbols, @tagName(zon_value));
            const Symbol = @TypeOf(symbol);
            switch (@typeInfo(Result)) {
                .@"enum" => switch (@typeInfo(Symbol)) {
                    .int => |info| {
                        var buf: [
                            std.fmt.count("{d}", .{switch (info.signedness) {
                                .signed => std.math.minInt(Symbol),
                                .unsigned => std.math.maxInt(Symbol),
                            }})
                        ]u8 = undefined;
                        return std.meta.stringToEnum(Result, std.fmt.bufPrint(&buf, "{d}", .{symbol}) catch unreachable).?;
                    },
                    else => return symbol,
                },
                else => return symbol,
            }
        } else {
            const Container = switch (@typeInfo(Result)) {
                else => struct {},
                .@"struct", .@"enum", .@"union", .@"opaque" => Result,
                .optional => |info| info.child,
                .error_union => |info| info.payload,
            };
            return if (@hasDecl(Container, @tagName(zon_value))) @field(Container, @tagName(zon_value)) else zon_value;
        },
        else => @compileError(std.fmt.comptimePrint("unsupported zon type: {} <- {any}", .{ Result, zon_value })),
    }
}

const SymbolSpec = union(enum) {};

const matchers = matchers: {
    const instructions = @import("instructions.zon");
    var mut_matchers: [instructions.len]*const fn (as: *Assemble) error{InvalidSyntax}!?Instruction = undefined;
    for (instructions, &mut_matchers) |instruction, *matcher| matcher.* = struct {
        fn match(as: *Assemble) !?Instruction {
            comptime for (@typeInfo(@TypeOf(instruction)).@"struct".fields) |field| {
                if (std.mem.eql(u8, field.name, "requires")) continue;
                if (std.mem.eql(u8, field.name, "pattern")) continue;
                if (std.mem.eql(u8, field.name, "symbols")) continue;
                if (std.mem.eql(u8, field.name, "encode")) continue;
                @compileError("unexpected field '" ++ field.name ++ "'");
            };
            if (@hasField(@TypeOf(instruction), "requires")) _ = zonCast(
                []const std.Target.aarch64.Feature,
                instruction.requires,
                .{},
            );
            var symbols: Symbols: {
                const symbols = @typeInfo(@TypeOf(instruction.symbols)).@"struct".fields;
                var symbol_fields: [symbols.len]std.builtin.Type.StructField = undefined;
                for (&symbol_fields, symbols) |*symbol_field, symbol| {
                    const Storage = zonCast(SymbolSpec, @field(instruction.symbols, symbol.name), .{}).Storage();
                    symbol_field.* = .{
                        .name = symbol.name,
                        .type = Storage,
                        .default_value_ptr = null,
                        .is_comptime = false,
                        .alignment = @alignOf(Storage),
                    };
                }
                break :Symbols @Type(.{ .@"struct" = .{
                    .layout = .auto,
                    .fields = &symbol_fields,
                    .decls = &.{},
                    .is_tuple = false,
                } });
            } = undefined;
            const Symbol = std.meta.FieldEnum(@TypeOf(instruction.symbols));
            comptime var unused_symbols: std.enums.EnumSet(Symbol) = .initFull();
            comptime var pattern_as: Assemble = .{ .source = instruction.pattern, .operands = undefined };
            inline while (true) {
                comptime var ct_token_buf: [token_buf_len]u8 = undefined;
                var token_buf: [token_buf_len]u8 = undefined;
                const pattern_token = comptime pattern_as.nextToken(&ct_token_buf, .{ .placeholders = true }) catch |err|
                    @compileError(@errorName(err) ++ " while parsing '" ++ instruction.pattern ++ "'");
                const source_token = try as.nextToken(&token_buf, .{ .operands = true });
                log.debug("\"{f}\" -> \"{f}\"", .{
                    std.zig.fmtString(pattern_token),
                    std.zig.fmtString(source_token),
                });
                if (pattern_token.len == 0) {
                    comptime var unused_symbol_it = unused_symbols.iterator();
                    inline while (comptime unused_symbol_it.next()) |unused_symbol|
                        @compileError(@tagName(unused_symbol) ++ " unused while parsing '" ++ instruction.pattern ++ "'");
                    switch (source_token.len) {
                        0 => {},
                        else => switch (source_token[0]) {
                            else => {
                                log.debug("'{s}' not matched...", .{instruction.pattern});
                                return null;
                            },
                            '\n', ';' => {},
                        },
                    }
                    const encode = @field(Instruction, @tagName(instruction.encode[0]));
                    const Encode = @TypeOf(encode);
                    if (@typeInfo(Encode) == .@"fn") {
                        var args: std.meta.ArgsTuple(Encode) = undefined;
                        inline for (&args, @typeInfo(Encode).@"fn".params, 1..instruction.encode.len) |*arg, param, encode_index|
                            arg.* = zonCast(param.type.?, instruction.encode[encode_index], symbols);
                        return @call(.auto, encode, args);
                    } else {
                        // hardcoded instructions
                        return encode;
                    }
                } else if (pattern_token[0] == '<') {
                    const symbol_name = comptime pattern_token[1 .. std.mem.indexOfScalarPos(u8, pattern_token, 1, '|') orelse
                        pattern_token.len - 1];
                    const symbol = @field(Symbol, symbol_name);
                    const symbol_ptr = &@field(symbols, symbol_name);
                    const symbol_value = zonCast(SymbolSpec, @field(instruction.symbols, symbol_name), .{}).parse(source_token) orelse {
                        log.debug("'{s}' not matched...", .{instruction.pattern});
                        return null;
                    };
                    if (comptime unused_symbols.contains(symbol)) {
                        log.debug("{s} = {any}", .{ symbol_name, symbol_value });
                        symbol_ptr.* = symbol_value;
                        comptime unused_symbols.remove(symbol);
                    } else if (symbol_ptr.* != symbol_value) {
                        log.debug("'{s}' not matched...", .{instruction.pattern});
                        return null;
                    }
                } else if (!toUpperEqlAssertUpper(source_token, pattern_token)) {
                    log.debug("'{s}' not matched...", .{instruction.pattern});
                    return null;
                }
            }
        }
    }.match;
    break :matchers mut_matchers;
};

fn toUpperEqlAssertUpper(lhs: []const u8, rhs: []const u8) bool {
    if (lhs.len != rhs.len) return false;
    for (lhs, rhs) |l, r| {
        assert(!std.ascii.isLower(r));
        if (std.ascii.toUpper(l) != r) return false;
    }
    return true;
}

const token_buf_len = "x10".len;
fn nextToken(as: *Assemble, buf: *[token_buf_len]u8, comptime opts: struct {
    operands: bool = false,
    placeholders: bool = false,
}) ![]const u8 {
    _ = buf;
    _ = opts;

    // const invalid_syntax: u8 = 1;
    while (true) switch (as.source[0]) {
        0 => return as.source[0..0],
        '\t', '\n' + 1...'\r', ' ' => as.source = as.source[1..],
        'A'...'Z', 'a'...'z' => {
            var index: usize = 1;
            while (more: switch (as.source[index]) {
                'A'...'Z', 'a'...'z' => switch (as.source[0]) {
                    else => true,
                    '.' => {
                        index = 1;
                        break :more false;
                    },
                },
                else => false,
            }) index += 1;
            defer as.source = as.source[index..];
            return as.source[0..index];
        },
        else => {
            if (!@inComptime()) log.debug("invalid token \"{f}\"", .{std.zig.fmtString(std.mem.span(as.source))});
            return error.InvalidSyntax;
        },
    };
}

const Assemble = @This();
const std = @import("std");
const assert = std.debug.assert;

const encoding = @import("encoding.zig");
const Instruction = encoding.Instruction;
const Register = encoding.Register;

const log = std.log.scoped(.@"asm");
