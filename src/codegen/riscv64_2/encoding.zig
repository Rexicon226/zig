pub const Register = enum(u5) {
    /// Hard-wired zero
    zero,
    /// Return address. Caller saved.
    ra,
    /// Stack pointer. Callee saved.
    sp,
    /// Global Pointer
    gp,
    /// Thread Pointer
    tp,
    /// Temporary 0. Caller saved.
    t0,
    /// Temporary 1. Caller saved.
    t1,
    /// Temporary 2. Caller saved.
    t2,
    /// Saved register 0. Callee saved.
    s0,
    /// Saved register 1. Callee saved.
    s1,
    /// Function argument 0. Return value 0. Caller saved.
    a0,
    /// Function argument 1. Return value 1. Caller saved.
    a1,
    /// Function argument 2. Caller saved.
    a2,
    /// Function argument 3. Caller saved.
    a3,
    /// Function argument 4. Caller saved.
    a4,
    /// Function argument 5. Caller saved.
    a5,
    /// Function argument 6. Caller saved.
    a6,
    /// Function argument 7. Caller saved.
    a7,
    /// Saved register 2. Callee saved.
    s2,
    /// Saved register 3. Callee saved.
    s3,
    /// Saved register 4. Callee saved.
    s4,
    /// Saved register 5. Callee saved.
    s5,
    /// Saved register 6. Callee saved.
    s6,
    /// Saved register 7. Callee saved.
    s7,
    /// Saved register 8. Callee saved.
    s8,
    /// Saved register 9. Callee saved.
    s9,
    /// Saved register 10. Callee saved.
    s10,
    /// Saved register 11. Callee saved.
    s11,
    /// Temporary register 3. Caller Saved.
    t3,
    /// Temporary register 4. Caller Saved.
    t4,
    /// Temporary register 5. Caller Saved.
    t5,
    /// Temporary register 6. Caller Saved.
    t6,

    // aliases

    pub const x0: Register = .zero;
    pub const x1: Register = .ra;
    pub const x2: Register = .sp;
    pub const x3: Register = .gp;
    pub const x4: Register = .tp;
    pub const x5: Register = .t0;
    pub const x6: Register = .t1;
    pub const x7: Register = .t2;
    pub const x8: Register = .s0;
    pub const x9: Register = .s1;
    pub const x10: Register = .a0;
    pub const x11: Register = .a1;
    pub const x12: Register = .a2;
    pub const x13: Register = .a3;
    pub const x14: Register = .a4;
    pub const x15: Register = .a5;
    pub const x16: Register = .a6;
    pub const x17: Register = .a7;
    pub const x18: Register = .s2;
    pub const x19: Register = .s3;
    pub const x20: Register = .s4;
    pub const x21: Register = .s5;
    pub const x22: Register = .s6;
    pub const x23: Register = .s7;
    pub const x24: Register = .s8;
    pub const x25: Register = .s9;
    pub const x26: Register = .s10;
    pub const x27: Register = .s11;
    pub const x28: Register = .t3;
    pub const x29: Register = .t4;
    pub const x30: Register = .t5;
    pub const x31: Register = .t6;

    /// Frame Pointer.
    pub const fp: Register = .s0;

    pub fn isVector(r: Register) bool {
        _ = r; // we don't define them yet
        return false;
    }

    pub fn parse(reg: []const u8) ?Register {
        if (reg.len == 0) return null; // impossible
        if (std.mem.eql(u8, reg, "fp")) return .s0; // alternative name
        return switch (std.ascii.toLower(reg[0])) {
            'x' => if (std.fmt.parseInt(u5, reg[1..], 10)) |n| switch (n) {
                inline 0...31 => |i| @enumFromInt(i),
            } else |_| null,
            else => null,
        };
    }
};

pub const Instruction = packed union {
    r: R,
    i: I,
    s: S,
    u: U,

    b: B,
    j: J,

    pub const R = packed struct(u32) {
        opcode: OpCode,
        rd: Register,
        funct3: u3,
        rs1: Register,
        rs2: Register,
        funct7: u7,

        const Definition = struct {
            opcode: OpCode,
            funct3: u3,
            funct7: u7,

            pub fn encode(d: Definition, rd: Register, rs1: Register, rs2: Register) Instruction {
                return .{ .r = .{
                    .opcode = d.opcode,
                    .funct3 = d.funct3,
                    .funct7 = d.funct7,
                    .rd = rd,
                    .rs1 = rs1,
                    .rs2 = rs2,
                } };
            }
        };

        pub const add: Definition = .{ .opcode = .OP, .funct3 = 0b000, .funct7 = 0b0000000 };
        pub const sub: Definition = .{ .opcode = .OP, .funct3 = 0b000, .funct7 = 0b0100000 };

        pub const addw: Definition = .{ .opcode = .OP_32, .funct3 = 0b000, .funct7 = 0b0000000 };
        pub const subw: Definition = .{ .opcode = .OP_32, .funct3 = 0b000, .funct7 = 0b0100000 };

        pub const mul: Definition = .{ .opcode = .OP, .funct3 = 0b000, .funct7 = 0b0000001 };

        pub const div: Definition = .{ .opcode = .OP, .funct3 = 0b100, .funct7 = 0b0000001 };
        pub const divu: Definition = .{ .opcode = .OP, .funct3 = 0b101, .funct7 = 0b0000001 };

        pub const sll: Definition = .{ .opcode = .OP, .funct3 = 0b001, .funct7 = 0b0000000 };
        pub const srl: Definition = .{ .opcode = .OP, .funct3 = 0b101, .funct7 = 0b0000000 };
        pub const sra: Definition = .{ .opcode = .OP, .funct3 = 0b101, .funct7 = 0b0100000 };

        pub const @"and": Definition = .{ .opcode = .OP, .funct3 = 0b111, .funct7 = 0b0000000 };
        pub const @"or": Definition = .{ .opcode = .OP, .funct3 = 0b110, .funct7 = 0b0000000 };
        pub const xor: Definition = .{ .opcode = .OP, .funct3 = 0b100, .funct7 = 0b0000000 };

        pub const sltu: Definition = .{ .opcode = .OP, .funct3 = 0b011, .funct7 = 0b0000000 };
        pub const slt: Definition = .{ .opcode = .OP, .funct3 = 0b010, .funct7 = 0b0000000 };
    };

    pub const I = packed struct(u32) {
        opcode: OpCode,
        rd: Register,
        funct3: u3,
        rs1: Register,
        imm12: u12,

        const Definition = struct {
            opcode: OpCode,
            funct3: u3,

            pub fn encode(d: Definition, rd: Register, rs1: Register, imm12: u12) Instruction {
                return .{ .i = .{
                    .opcode = d.opcode,
                    .funct3 = d.funct3,
                    .rd = rd,
                    .rs1 = rs1,
                    .imm12 = imm12,
                } };
            }
        };

        const Shift = struct {
            opcode: OpCode,
            funct3: u3,
            shtyp: u6,

            pub fn encode(s: Shift, rd: Register, rs1: Register, imm6: u6) Instruction {
                return .{ .i = .{
                    .opcode = s.opcode,
                    .funct3 = s.funct3,
                    .rd = rd,
                    .rs1 = rs1,
                    .imm12 = (@as(u12, s.shtyp) << 6) | imm6,
                } };
            }
        };

        pub const addi: Definition = .{ .opcode = .OP_IMM, .funct3 = 0b000 };
        pub const addiw: Definition = .{ .opcode = .OP_IMM_32, .funct3 = 0b000 };

        pub const sltiu: Definition = .{ .opcode = .OP_IMM, .funct3 = 0b011 };

        pub const slli: Shift = .{ .opcode = .OP_IMM, .funct3 = 0b001, .shtyp = 0b000000 };
        pub const srli: Shift = .{ .opcode = .OP_IMM, .funct3 = 0b101, .shtyp = 0b000000 };
        pub const srai: Shift = .{ .opcode = .OP_IMM, .funct3 = 0b101, .shtyp = 0b010000 };

        pub const xori: Definition = .{ .opcode = .OP_IMM, .funct3 = 0b100 };

        pub const jalr: Definition = .{ .opcode = .JALR, .funct3 = 0b000 };

        pub const system: Definition = .{ .opcode = .SYSTEM, .funct3 = 0b000 };
        pub const none: Definition = .{ .opcode = .NONE, .funct3 = 0b000 };

        pub const lb: Definition = .{ .opcode = .LOAD, .funct3 = 0b000 };
        pub const lh: Definition = .{ .opcode = .LOAD, .funct3 = 0b001 };
        pub const lw: Definition = .{ .opcode = .LOAD, .funct3 = 0b010 };
        pub const ld: Definition = .{ .opcode = .LOAD, .funct3 = 0b011 };
        pub const lbu: Definition = .{ .opcode = .LOAD, .funct3 = 0b100 };
        pub const lhu: Definition = .{ .opcode = .LOAD, .funct3 = 0b101 };
        pub const lwu: Definition = .{ .opcode = .LOAD, .funct3 = 0b110 };
    };

    pub const S = packed struct(u32) {
        opcode: OpCode,
        imm5: u5,
        funct3: u3,
        rs1: Register,
        rs2: Register,
        imm7: u7,

        const Definition = struct {
            funct3: u3,

            pub fn encode(d: Definition, rs1: Register, rs2: Register, imm12: u12) Instruction {
                return .{ .s = .{
                    .opcode = .STORE,
                    .funct3 = d.funct3,
                    .rs1 = rs1,
                    .rs2 = rs2,
                    .imm5 = @truncate(imm12),
                    .imm7 = @truncate(imm12 >> 5),
                } };
            }
        };

        pub const sb: Definition = .{ .funct3 = 0b000 };
        pub const sh: Definition = .{ .funct3 = 0b001 };
        pub const sw: Definition = .{ .funct3 = 0b010 };
        pub const sd: Definition = .{ .funct3 = 0b011 };
    };

    const U = packed struct(u32) {
        opcode: OpCode,
        rd: Register,
        imm20: u20,
    };

    pub const B = packed struct(u32) {
        opcode: OpCode,
        imm11: u1,
        imm1_4: u4,
        funct3: u3,
        rs1: Register,
        rs2: Register,
        imm5_10: u6,
        imm12: u1,

        const Definition = struct {
            funct3: u3,

            pub fn encode(d: Definition, rs1: Register, rs2: Register, offset: u13) Instruction {
                std.debug.assert(offset % 2 == 0);
                return .{ .b = .{
                    .opcode = .BRANCH,
                    .funct3 = d.funct3,
                    .rs1 = rs1,
                    .rs2 = rs2,
                    .imm1_4 = @truncate(offset >> 1),
                    .imm5_10 = @truncate(offset >> 5),
                    .imm11 = @truncate(offset >> 11),
                    .imm12 = @truncate(offset >> 12),
                } };
            }
        };

        pub const beq: Definition = .{ .funct3 = 0b000 };
        pub const neq: Definition = .{ .funct3 = 0b001 };
    };

    const J = packed struct(u32) {
        opcode: OpCode,
        rd: Register,
        imm12_19: u8,
        imm11: u1,
        imm1_10: u10,
        imm20: u1,
    };

    pub const Decoded = union(enum) {
        r: R,
        i: I,
        u: U,
        s: S,
        b: B,
        j: J,
    };
    pub fn decode(inst: Instruction) Decoded {
        return switch (inst.r.opcode) { // opcode always in the same place
            .OP, .OP_32, .OP_64, .OP_FP, .AMO => .{ .r = inst.r },
            .OP_IMM, .OP_IMM_32, .OP_IMM_64, .LOAD, .LOAD_FP, .JALR, .SYSTEM, .MISC_MEM => .{ .i = inst.i },
            .LUI, .AUIPC => .{ .u = inst.u },
            .STORE => .{ .s = inst.s },
            .BRANCH => .{ .b = inst.b },
            .JAL => .{ .j = inst.j },
            else => @panic("TODO"),
        };
    }

    const OpCode = enum(u7) {
        LOAD = 0b0000011,
        LOAD_FP = 0b0000111,
        MISC_MEM = 0b0001111,
        OP_IMM = 0b0010011,
        AUIPC = 0b0010111,
        OP_IMM_32 = 0b0011011,
        STORE = 0b0100011,
        STORE_FP = 0b0100111,
        AMO = 0b0101111,
        OP_V = 0b1010111,
        OP = 0b0110011,
        OP_32 = 0b0111011,
        LUI = 0b0110111,
        MADD = 0b1000011,
        MSUB = 0b1000111,
        NMSUB = 0b1001011,
        NMADD = 0b1001111,
        OP_FP = 0b1010011,
        OP_IMM_64 = 0b1011011,
        BRANCH = 0b1100011,
        JALR = 0b1100111,
        JAL = 0b1101111,
        SYSTEM = 0b1110011,
        OP_64 = 0b1111011,
        NONE = 0b00000000,
    };

    pub fn add(dest: Register, src1: Register, src2: Register) Instruction {
        return R.add.encode(dest, src1, src2);
    }
    pub fn sub(dest: Register, src1: Register, src2: Register) Instruction {
        return R.sub.encode(dest, src1, src2);
    }
    pub fn mul(dest: Register, src1: Register, src2: Register) Instruction {
        return R.mul.encode(dest, src1, src2);
    }
    pub fn divu(dest: Register, src1: Register, src2: Register) Instruction {
        return R.divu.encode(dest, src1, src2);
    }
    pub fn sll(dest: Register, src1: Register, src2: Register) Instruction {
        return R.sll.encode(dest, src1, src2);
    }
    pub fn srl(dest: Register, src1: Register, src2: Register) Instruction {
        return R.srl.encode(dest, src1, src2);
    }
    pub fn sra(dest: Register, src1: Register, src2: Register) Instruction {
        return R.sra.encode(dest, src1, src2);
    }
    pub fn addw(dest: Register, src1: Register, src2: Register) Instruction {
        return R.addw.encode(dest, src1, src2);
    }
    pub fn subw(dest: Register, src1: Register, src2: Register) Instruction {
        return R.subw.encode(dest, src1, src2);
    }
    pub fn @"and"(dest: Register, src1: Register, src2: Register) Instruction {
        return R.@"and".encode(dest, src1, src2);
    }
    pub fn @"or"(dest: Register, src1: Register, src2: Register) Instruction {
        return R.@"or".encode(dest, src1, src2);
    }
    pub fn xor(dest: Register, src1: Register, src2: Register) Instruction {
        return R.xor.encode(dest, src1, src2);
    }
    pub fn sltu(dest: Register, src1: Register, src2: Register) Instruction {
        return R.sltu.encode(dest, src1, src2);
    }
    pub fn slt(dest: Register, src1: Register, src2: Register) Instruction {
        return R.slt.encode(dest, src1, src2);
    }

    pub fn addi(dest: Register, src1: Register, imm: i12) Instruction {
        return I.addi.encode(dest, src1, @bitCast(imm));
    }
    pub fn addiw(dest: Register, src1: Register, imm: i12) Instruction {
        return I.addiw.encode(dest, src1, @bitCast(imm));
    }
    /// rv32i variant if `imm < 32` else rv64i
    pub fn slli(dest: Register, src1: Register, imm: u6) Instruction {
        return I.slli.encode(dest, src1, imm);
    }
    /// rv32i variant if `imm < 32` else rv64i
    pub fn srli(dest: Register, src1: Register, imm: u6) Instruction {
        return I.srli.encode(dest, src1, imm);
    }
    /// rv32i variant if `imm < 32` else rv64i
    pub fn srai(dest: Register, src1: Register, imm: u6) Instruction {
        return I.srai.encode(dest, src1, imm);
    }
    pub fn xori(dest: Register, src1: Register, imm: i12) Instruction {
        return I.xori.encode(dest, src1, @bitCast(imm));
    }

    pub fn jalr(dest: Register, src1: Register, imm: u12) Instruction {
        return I.jalr.encode(dest, src1, imm);
    }
    pub fn sltiu(dest: Register, src1: Register, imm: u12) Instruction {
        return I.sltiu.encode(dest, src1, imm);
    }
    pub fn lb(dest: Register, src1: Register, imm: i12) Instruction {
        return I.lb.encode(dest, src1, @bitCast(imm));
    }
    pub fn lbu(dest: Register, src1: Register, imm: i12) Instruction {
        return I.lbu.encode(dest, src1, @bitCast(imm));
    }
    pub fn lh(dest: Register, src1: Register, imm: i12) Instruction {
        return I.lh.encode(dest, src1, @bitCast(imm));
    }
    pub fn lhu(dest: Register, src1: Register, imm: i12) Instruction {
        return I.lhu.encode(dest, src1, @bitCast(imm));
    }
    pub fn lw(dest: Register, src1: Register, imm: i12) Instruction {
        return I.lw.encode(dest, src1, @bitCast(imm));
    }
    pub fn lwu(dest: Register, src1: Register, imm: i12) Instruction {
        return I.lwu.encode(dest, src1, @bitCast(imm));
    }
    pub fn ld(dest: Register, src1: Register, imm: i12) Instruction {
        return I.ld.encode(dest, src1, @bitCast(imm));
    }

    pub fn sb(src1: Register, base: Register, imm: i12) Instruction {
        return S.sb.encode(base, src1, @bitCast(imm));
    }
    pub fn sh(src1: Register, base: Register, imm: i12) Instruction {
        return S.sh.encode(base, src1, @bitCast(imm));
    }
    pub fn sw(src1: Register, base: Register, imm: i12) Instruction {
        return S.sw.encode(base, src1, @bitCast(imm));
    }
    pub fn sd(src1: Register, base: Register, imm: i12) Instruction {
        return S.sd.encode(base, src1, @bitCast(imm));
    }

    pub fn lui(dest: Register, imm: u20) Instruction {
        return .{ .u = .{ .opcode = .LUI, .rd = dest, .imm20 = imm } };
    }

    pub fn auipc(dest: Register, imm: u20) Instruction {
        return .{ .u = .{ .opcode = .AUIPC, .rd = dest, .imm20 = imm } };
    }

    pub fn beq(src1: Register, src2: Register, target: u13) Instruction {
        return B.beq.encode(src1, src2, target);
    }
    pub fn bne(src1: Register, src2: Register, target: u13) Instruction {
        return B.bne.encode(src1, src2, target);
    }

    pub fn jal(dest: Register, offset: i21) Instruction {
        const umm: u21 = @bitCast(offset);
        return .{ .j = .{
            .opcode = .JAL,
            .rd = dest,
            .imm1_10 = @truncate(umm >> 1),
            .imm11 = @truncate(umm >> 11),
            .imm12_19 = @truncate(umm >> 12),
            .imm20 = @truncate(umm >> 20),
        } };
    }

    // hardcoded instructions

    /// NO-OP HINT instruction - ADDI rd=x0, and either rs1≠x0 or imm≠0
    pub const noop: Instruction = .addi(.x0, .x0, 0);
    pub const ret: Instruction = .jalr(.x0, .ra, 0);
    pub const ecall: Instruction = I.system.encode(.x0, .x0, 0x000);
    pub const ebreak: Instruction = I.system.encode(.x0, .x0, 0x001);
    pub const unimp: Instruction = I.none.encode(.x0, .x0, 0x000);

    /// Returns the smallest possible size for an instruction given the enabled feature set.
    pub fn minSize(target: *const std.Target) u32 {
        if (target.cpu.hasAny(.riscv, &.{ .c, .zca })) return 2;
        return 4;
    }
    /// Maximum possible size for an instruction. Always 4, since we're allowe to use
    /// full-sized instructions when the compressed feature is enabled.
    pub const max_size = @divExact(@bitSizeOf(Backing), 8);
    pub const Backing = u32;

    pub fn format(inst: Instruction, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        const dis: Disassemble = .{};
        try dis.printInstruction(inst, writer);
    }
    // TODO: account for compressed extension
    pub fn write(inst: Instruction, mem: *[max_size]u8) void {
        std.mem.writeInt(Backing, mem, @bitCast(inst), .little);
    }
};

const std = @import("std");
const Disassemble = @import("Disassemble.zig");
