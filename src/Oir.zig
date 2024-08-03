//! Optimizable Intermediate Representation

air: Air,
pt: Zcu.PerThread,
allocator: std.mem.Allocator,
air_to_node: std.AutoHashMapUnmanaged(Air.Inst.Index, Node.Index) = .{},

/// The single point of truth when it comes to Nodes.
/// This should only ever be added to.
nodes: std.ArrayListUnmanaged(Node) = .{},
classes: std.ArrayListUnmanaged(Class) = .{},

extra: std.ArrayListUnmanaged(u32) = .{},

const ClassMap = std.AutoHashMapUnmanaged(Node.Index, Class.Index);

pub const Error = Rewrite.Error;

pub const Node = struct {
    tag: Tag,

    data: Data = .none,

    /// Nodes only have edges to other Classes.
    out: std.ArrayListUnmanaged(Class.Index) = .{},

    const Index = enum(u32) {
        _,
    };

    pub const Tag = enum(u8) {
        arg,
        add,
        sub,
        mul,
        shl,
        div_trunc,
        div_exact,
        ret,
        constant,

        pub fn isCommutative(tag: Tag) bool {
            return switch (tag) {
                .add,
                .mul,
                => true,
                .div_trunc,
                .div_exact,
                .shl,
                => false,
                else => false, // not a node that can have this property
            };
        }
    };

    const Data = union(enum) {
        none: void,
        constant: i64,
    };
};

/// A Class contains an N amount of Nodes as children.
const Class = struct {
    bag: std.ArrayListUnmanaged(Node.Index) = .{},

    const Index = enum(u32) {
        _,
    };
};

pub fn fromAir(air: Air, pt: Zcu.PerThread) void {
    errdefer |err| std.debug.panic("fromAir {s}", .{@errorName(err)});

    var arena = std.heap.ArenaAllocator.init(pt.zcu.gpa);
    defer arena.deinit();
    const allocator = arena.allocator();

    var oir: Oir = .{
        .air = air,
        .pt = pt,
        .allocator = allocator,
    };

    const body = air.getMainBody();
    const tags = air.instructions.items(.tag);
    const data = air.instructions.items(.data);

    for (body) |inst| {
        const tag: Air.Inst.Tag = tags[@intFromEnum(inst)];
        const id: ?Node.Index = switch (tag) {
            .arg => blk: {
                const node: Node = .{
                    .tag = .arg,
                };
                break :blk try oir.add(node);
            },
            .ret,
            .ret_safe,
            => blk: {
                var node: Node = .{
                    .tag = .ret,
                };

                const op = data[@intFromEnum(inst)].un_op;
                const op_id = try oir.resolveNode(op);
                const class_id = try oir.findClass(op_id);
                try node.out.append(oir.allocator, class_id);

                break :blk try oir.add(node);
            },
            .mul,
            .div_trunc,
            .div_exact,
            .shl,
            => blk: {
                var node: Node = .{
                    .tag = switch (tag) {
                        .mul => .mul,
                        .div_trunc => .div_trunc,
                        .div_exact => .div_exact,
                        .shl => .shl,
                        else => unreachable,
                    },
                };

                const bin_op = data[@intFromEnum(inst)].bin_op;
                inline for (.{ bin_op.lhs, bin_op.rhs }) |ref| {
                    const id = try oir.resolveNode(ref);
                    const class_id = try oir.findClass(id);
                    try node.out.append(oir.allocator, class_id);
                }

                break :blk try oir.add(node);
            },
            else => null,
        };

        if (id) |i| try oir.air_to_node.put(allocator, inst, i);
    }

    try oir.rebuild();

    // var parser: SExpr.Parser = .{ .buf = "(div_exact (mul ?x ?y) ?x)" };
    // const expr = try parser.parse(allocator);
    // std.debug.print("expr: {}\n", .{expr});

    // apply rewrites
    inline for (rewrites) |rewrite| {
        try oir.applyRewrite(rewrite);
    }

    const dot_file = try std.fs.cwd().createFile("out.dot", .{});
    defer dot_file.close();
    try oir.dumpGraphyViz(dot_file.writer());
}

/// Adds an ENode to the EGraph.
fn add(oir: *Oir, node: Node) !Node.Index {
    // TODO: do real hashconsing instead of whatever this is
    if (node.tag == .constant) {
        // there's a chance we can dedup this
        const val = node.data.constant;
        for (oir.nodes.items, 0..) |existing_node, i| {
            if (existing_node.tag == .constant and existing_node.data.constant == val) {
                return @enumFromInt(i);
            }
        }
    }

    const node_id: Node.Index = @enumFromInt(oir.nodes.items.len);
    try oir.nodes.append(oir.allocator, node);

    // we initially set each node as its own e-class
    var class: Class = .{};
    try class.bag.append(oir.allocator, node_id);
    try oir.classes.append(oir.allocator, class);

    log.debug("birth node %{d}", .{@intFromEnum(node_id)});
    return node_id;
}

/// Unions two EClasses given their indices. `a` is the root being migrated into.
fn @"union"(oir: *Oir, a: Class.Index, b: Class.Index) !void {
    if (a != b) {
        log.debug("unioning class %{d} -> %{d}", .{ @intFromEnum(b), @intFromEnum(a) });

        const class_a = &oir.classes.items[@intFromEnum(a)];
        const class_b = &oir.classes.items[@intFromEnum(b)];

        // Replace all connections to class_b with class_a
        for (oir.nodes.items) |*node| {
            for (node.out.items) |*node_class_idx| {
                if (node_class_idx.* == b) {
                    node_class_idx.* = a;
                }
            }
        }

        // Move all nodes inside of class_b into class_a
        for (class_b.bag.items) |node_id| {
            try class_a.bag.append(oir.allocator, node_id);

            // TODO: if a node in the bag references class_b here, we need to change the reference to class_a
        }
        class_b.bag.clearRetainingCapacity();
    }
}

fn rebuild(oir: *Oir) !void {
    const old_node_len = oir.nodes.items.len;
    const old_classes_len = oir.classes.items.len;

    const start = try std.time.Instant.now();

    const end = try std.time.Instant.now();
    const elapsed = end.since(start);

    _ = old_node_len;
    _ = old_classes_len;
    _ = elapsed;

    // log.debug(
    //     \\ Rebuild Complete in  {}
    //     \\      Old: Node count {}, Class count {}
    //     \\      New: Node count {}, Class count {}
    // , .{
    //     std.fmt.fmtDuration(elapsed),
    //     old_node_len,
    //     old_classes_len,
    //     oir.nodes.items.len,
    //     oir.classes.items.len,
    // });
}

fn findClass(oir: *Oir, inst: Node.Index) !Class.Index {
    // TODO: use a hashmap for this
    for (oir.classes.items, 0..) |class, i| {
        for (class.bag.items) |node| {
            if (node == inst) return @enumFromInt(i);
        }
    }
    unreachable; // what did you do wrong now
}

fn getNode(oir: *Oir, node_idx: Node.Index) Node {
    return oir.nodes.items[@intFromEnum(node_idx)];
}

fn getClass(oir: *Oir, class_idx: Class.Index) Class {
    return oir.classes.items[@intFromEnum(class_idx)];
}

fn applyRewrite(oir: *Oir, rewrite: Rewrite) !void {
    var parser: SExpr.Parser = .{ .buf = rewrite.pattern };
    const expr = try parser.parse(oir.allocator);

    std.debug.print("trying: {}\n", .{expr});

    const matches = try oir.search(expr);
    for (matches) |idx| {
        log.debug("applying rewrite: {s} to %{d}", .{ rewrite.name, @intFromEnum(idx) });
        try rewrite.func(oir, idx);
    }
}

fn search(oir: *Oir, pattern: SExpr) ![]const Node.Index {
    var matches: std.ArrayListUnmanaged(Node.Index) = .{};
    for (0..oir.nodes.items.len) |node_idx| {
        var bindings: std.StringHashMapUnmanaged(Node.Index) = .{};
        defer bindings.deinit(oir.allocator);

        if (try oir.match(
            @enumFromInt(node_idx),
            pattern,
            &bindings,
        )) {
            try matches.append(oir.allocator, @enumFromInt(node_idx));
        }

        try oir.rebuild();
    }
    return matches.toOwnedSlice(oir.allocator);
}

/// Performs E-Matching starting from `node_idx`.
///
/// The outer-most s-expr tag will be the root of the tree.
///
/// Example:
/// Given an S-Expr `(mul ?x 2)`, and a tree:
/// ```txt
/// %0 = arg(0)
/// %1 = const(2)
/// %2 = mul(%0, %1)
/// ```
///
/// `node_idx` passed into this function must be `%2` for the match to happen.
fn match(
    oir: *Oir,
    node_idx: Node.Index,
    pattern: SExpr,
    /// relates atom strings to node indices, which uses uniqueness invariant to
    /// always refer to same or different things
    bindings: *std.StringHashMapUnmanaged(Node.Index),
) Rewrite.Error!bool {
    const root_node = oir.getNode(node_idx);

    switch (pattern.data) {
        .atom => |atom_string| {
            // this is a variable
            if (atom_string[0] == '?') {
                const variable_ident = atom_string[1..];
                const gop = try bindings.getOrPut(oir.allocator, variable_ident);
                if (gop.found_existing) {
                    // when we have the same node index, it must mean it's the same thing
                    return gop.value_ptr.* == node_idx;
                } else {
                    gop.value_ptr.* = node_idx; // set that atom constant to equal this node_idx
                    return true;
                }
            } else {
                if (root_node.tag == .constant) {
                    const value = try std.fmt.parseInt(i64, atom_string, 10);
                    return value == root_node.data.constant;
                } else return false;
            }
        },
        .list => |list| {
            if (list.len == 0) return false;
            if (root_node.tag != pattern.tag) return false;
            if (list.len != root_node.out.items.len) return false;

            if (pattern.tag.isCommutative()) {
                // we need to check both ways now. both (mul ?x ?y) and (mul ?y ?x) can be viable patterns
                // we iterate through the nodes, so mul(%0, %1) as an example. both equivalence classes
                // are allowed to match with both sub expressions in the pattern

                // is this the first class in the node's out bag
                var is_first: bool = true;
                // has a node before us already matched with the first sub_pattern
                var matched_first: bool = false;
                for (root_node.out.items) |class_idx| {
                    // class_idx will be either %0 or %1
                    // because the pattern is commutative, it can match with either ?x or ?y
                    // assuming that it's the first one.
                    // if this is the second class_idx, the first class_idx must have matched with something
                    // and now we must match with the other one.
                    // better explained,
                    // %0 = ?y means %1 *must* = ?x

                    // we order the one that can still be matched first
                    const sub_patterns = if (is_first) list else &.{
                        list[@intFromBool(matched_first)],
                        list[@intFromBool(!matched_first)],
                    };

                    const class = oir.getClass(class_idx);
                    for (class.bag.items) |class_node_idx| {
                        const node = oir.getNode(class_node_idx);
                        std.debug.print("node: {}\n", .{node.tag});
                    }

                    for (sub_patterns, 0..) |sub_pattern, i| {
                        // if we've already matched the first, we don't want to pollute the second pattern's bindings
                        if (is_first and matched_first) continue;

                        const result = try oir.matchClass(class_idx, sub_pattern, bindings);

                        std.debug.print("{} pattern: {}, result: {}\n", .{ is_first, sub_pattern, result });
                        // if this is the first go, we still have a chance to match with the second pattern
                        if (is_first) {
                            if (i == 0) {
                                // if we matched, we matched
                                matched_first = result;
                            }
                            // if we matched the first one, great, we can continue to the
                            // second class. otherwise, there was no match.
                            if (!matched_first and !result) {
                                return false;
                            }
                        } else {
                            // we weren't the first class. it doesn't matter the result of the
                            // first class, since this will make an invalid expression.
                            // the ordering above ensures that we've checked the one possible option here.
                            //
                            // TODO: we are assuming that there are only two arguments, but that is fine for now
                            assert(i == 0);
                            return result;
                        }
                    }
                    is_first = false;
                }
                return true;
            } else {
                // we iterate through the node's classes, and then through each class.
                // we are trying to get each element of the list to get at least one match
                // inside of the class
                for (root_node.out.items, list) |class_idx, sub_pattern| {
                    const class = oir.getClass(class_idx);
                    for (class.bag.items) |class_node_idx| {
                        const node = oir.getNode(class_node_idx);
                        std.debug.print("node: {}\n", .{node.tag});
                    }

                    const result = try oir.matchClass(class_idx, sub_pattern, bindings);

                    std.debug.print("result: {}, pattern: {}\n", .{ result, sub_pattern });
                    if (!result) return false;
                }
                return true;
            }

            return true;
        },
    }
}

fn matchClass(
    oir: *Oir,
    class_idx: Class.Index,
    sub_pattern: SExpr,
    bindings: *std.StringHashMapUnmanaged(Node.Index),
) !bool {
    const class = oir.getClass(class_idx);
    var found_match: bool = false;
    for (class.bag.items) |class_node_idx| {
        const is_match = try oir.match(
            class_node_idx,
            sub_pattern,
            bindings,
        );
        if (!found_match) found_match = is_match;
    }
    return found_match;
}

const Rewrite = struct {
    const Error = std.mem.Allocator.Error || std.fmt.ParseIntError;

    name: []const u8,
    pattern: []const u8,
    func: fn (*Oir, Node.Index) Rewrite.Error!void,
};

const rewrites: []const Rewrite = &.{
    .{ .name = "mul-pow2-1-const", .pattern = "(mul ?x 2)", .func = mulPow2OneConst },
    .{ .name = "div-exact-mul-associate", .pattern = "(div_exact (mul ?y ?x) ?z)", .func = divExactMulAssociate },
    .{ .name = "div-exact-same-operand", .pattern = "(div_exact ?x ?x)", .func = divExactSameOperand },
    .{ .name = "mul-by-1", .pattern = "(mul 1 ?x)", .func = mulByOne },
};

/// `x * 2` -> `x << 1`
///
/// Given mul root index.
fn mulPow2OneConst(oir: *Oir, node_idx: Node.Index) !void {
    const node = oir.getNode(node_idx);
    assert(node.tag == .mul);
    assert(node.out.items.len == 2); // is a bin_op

    const lhs_class_id = node.out.items[0];
    const rhs_class_id = node.out.items[1];

    const lhs_class = oir.getClass(lhs_class_id);
    const rhs_class = oir.getClass(rhs_class_id);
    var meta: ?struct { i64, Class.Index } = null;

    inline for (.{ lhs_class, rhs_class }, 0..) |class, i| {
        for (class.bag.items) |child_idx| {
            const child = oir.getNode(child_idx);
            if (child.tag != .constant) continue;
            const val = child.data.constant;
            if (val <= 0) continue; // todo this
            if (std.math.isPowerOfTwo(@as(u32, @intCast(val)))) {
                // given the equality invariance, all other nodes in this class
                // must equal to this value, hence we can assume that we've found the constant
                meta = .{ val, .{ lhs_class_id, rhs_class_id }[(i + 1) & 1] };
                break;
            }
        }
        if (meta != null) break; // we only want to find one constant
    }
    if (meta == null) {
        // we never found a viable constant node
        return;
    }

    const val, const other_class = meta.?;

    const shift_node_id = try oir.add(.{
        .tag = .shl,
        .data = .none,
    });

    const shift_amount_id = try oir.add(.{
        .tag = .constant,
        .data = .{ .constant = std.math.log2(@as(u32, @intCast(val))) },
    });
    const shift_class_id = try oir.findClass(shift_amount_id);
    const shift_node = &oir.nodes.items[@intFromEnum(shift_node_id)];
    try shift_node.out.append(oir.allocator, other_class);
    try shift_node.out.append(oir.allocator, shift_class_id);

    const old_class_id = try oir.findClass(node_idx);
    const new_class_id = try oir.findClass(shift_node_id);

    try oir.@"union"(old_class_id, new_class_id);
}

/// - `(div_exact (mul ?x ?y) ?z)` -> `(mul ?x (div_exact ?y ?z))`
///
/// Given div_exact root index.
fn divExactMulAssociate(oir: *Oir, node_idx: Node.Index) !void {
    const node = oir.getNode(node_idx);
    assert(node.tag == .div_exact);

    const mul_class_id = node.out.items[0];
    const z_class_id = node.out.items[1];

    const mul_node_id = try oir.add(.{
        .tag = .mul,
        .data = .none,
    });
    const div_node_id = try oir.add(.{
        .tag = .div_exact,
        .data = .none,
    });

    const mul_class = &oir.classes.items[@intFromEnum(mul_class_id)];

    var y_class_id: ?Class.Index = null;
    var x_class_id: ?Class.Index = null;
    for (mul_class.bag.items) |bag_node_idx| {
        const bag_node = oir.getNode(bag_node_idx);
        if (bag_node.tag == .mul) {
            y_class_id = bag_node.out.items[1];
            x_class_id = bag_node.out.items[0];
        }
    }

    const div_class_id = try oir.findClass(div_node_id);

    const div_node = &oir.nodes.items[@intFromEnum(div_node_id)];
    try div_node.out.append(oir.allocator, y_class_id.?);
    try div_node.out.append(oir.allocator, z_class_id);

    // add the (div_exact to the mul node
    const mul_node = &oir.nodes.items[@intFromEnum(mul_node_id)];
    try mul_node.out.append(oir.allocator, div_class_id);
    try mul_node.out.append(oir.allocator, x_class_id.?);

    const old_class_id = try oir.findClass(node_idx);
    const new_class_id = try oir.findClass(mul_node_id);

    try oir.@"union"(old_class_id, new_class_id);
}

fn divExactSameOperand(oir: *Oir, node_idx: Node.Index) !void {
    const node = oir.getNode(node_idx);
    assert(node.tag == .div_exact);
    const class_idx = try oir.findClass(node_idx);

    const one = try oir.add(.{
        .tag = .constant,
        .data = .{ .constant = 1 },
    });

    const class = &oir.classes.items[@intFromEnum(class_idx)];
    try class.bag.append(oir.allocator, one);

    const one_class_idx = try oir.findClass(one);

    try oir.@"union"(class_idx, one_class_idx);
}

fn mulByOne(oir: *Oir, node_idx: Node.Index) !void {
    const node = oir.getNode(node_idx);
    assert(node.tag == .mul);

    const lhs_class_id = node.out.items[0];
    const rhs_class_id = node.out.items[1];

    const lhs_class = oir.getClass(lhs_class_id);
    const rhs_class = oir.getClass(rhs_class_id);
    var meta: ?Class.Index = null;

    inline for (.{ lhs_class, rhs_class }, 0..) |class, i| {
        for (class.bag.items) |child_idx| {
            const child = oir.getNode(child_idx);
            if (child.tag != .constant) continue;
            const val = child.data.constant;
            if (val == 1) {
                // given the equality invariance, all other nodes in this class
                // must equal to this value, hence we can assume that we've found the constant
                meta = .{ lhs_class_id, rhs_class_id }[(i + 1) & 1];
                break;
            }
        }
        if (meta != null) break; // we only want to find one constant
    }
    if (meta == null) {
        unreachable; // something went wrong
    }

    // this is the class of the argument which is equal to the mul class
    const other_class = meta.?;
    const mul_class = try oir.findClass(node_idx);

    try oir.@"union"(mul_class, other_class);
}

fn resolveNode(oir: *Oir, ref: Air.Inst.Ref) !Node.Index {
    if (ref.toIndex()) |idx| {
        return oir.air_to_node.get(idx).?;
    } else {
        const maybe_val = try oir.air.value(ref, oir.pt);
        const val = maybe_val.?.toSignedInt(oir.pt);
        const node: Node = .{
            .tag = .constant,
            .data = .{ .constant = val },
        };
        return oir.add(node);
    }
}

fn dumpGraphyViz(
    oir: *Oir,
    file_writer: anytype,
) !void {
    try file_writer.writeAll(
        \\digraph G {
        \\  graph [fontsize=14 compound=true]
        \\  node [shape=box, stlye=filled];
        \\
    );

    for (oir.classes.items, 0..) |class, i| {
        try file_writer.print(
            \\  subgraph cluster_{d} {{
            \\    style=dotted
            \\
        ,
            .{i},
        );

        for (class.bag.items) |node_idx| {
            const node = oir.getNode(node_idx);
            try file_writer.print("    node{d} [label=\"", .{@intFromEnum(node_idx)});
            switch (node.tag) {
                .constant => {
                    const val = node.data.constant;
                    try file_writer.print("constant:{d}", .{val});
                },
                .arg => {
                    try file_writer.print("arg({d})", .{@intFromEnum(node_idx)});
                },
                else => try file_writer.writeAll(@tagName(node.tag)),
            }
            try file_writer.writeAll("\", color=\"grey\"];\n");
        }

        try file_writer.writeAll("  }\n");
    }

    for (oir.nodes.items, 0..) |node, i| {
        for (node.out.items) |class_idx| {
            const class = oir.getClass(class_idx);
            if (class.bag.items.len == 0) std.debug.panic(
                "node %{d} connects to empty class %{d} empty for some reason",
                .{ i, @intFromEnum(class_idx) },
            );
            const node_idx = class.bag.items[0];
            try file_writer.print(
                "  node{d} -> node{d} [lhead=\"cluster_{d}\"];\n",
                .{ i, @intFromEnum(node_idx), @intFromEnum(class_idx) },
            );
        }
    }

    try file_writer.writeAll("}\n");
}

const log = std.log.scoped(.oir);
const assert = std.debug.assert;

const Oir = @This();
const SExpr = @import("Oir/SExpr.zig");

const Air = @import("Air.zig");
const Type = @import("Type.zig");
const Zcu = @import("Zcu.zig");
const std = @import("std");
