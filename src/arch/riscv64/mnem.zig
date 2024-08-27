pub const Mnemonic = enum(u16) {
    // Arithmetics
    addi,
    add,
    addw,

    sub,
    subw,

    // Bits
    xori,
    xor,
    @"or",

    @"and",
    andi,

    slt,
    sltu,
    sltiu,

    slli,
    srli,
    srai,

    slliw,
    srliw,
    sraiw,

    sll,
    srl,
    sra,

    sllw,
    srlw,
    sraw,

    // Control Flow
    jalr,
    jal,

    beq,
    bne,

    // Memory
    lui,
    auipc,

    ld,
    lw,
    lh,
    lb,
    lbu,
    lhu,
    lwu,

    sd,
    sw,
    sh,
    sb,

    // System
    ebreak,
    ecall,
    unimp,
    nop,

    // M extension
    mul,
    mulh,
    mulhu,
    mulhsu,
    mulw,

    div,
    divu,
    divw,
    divuw,

    rem,
    remu,
    remw,
    remuw,

    // F extension (32-bit float)
    /// Floating-Point Add Single
    fadds,
    /// Floating-Point Subtract Single
    fsubs,
    /// Floating-Point Multiply Single
    fmuls,
    /// Floating-Point Divide Single
    fdivs,

    /// Floating-Point Minimum Single
    fmins,
    /// Floating-Point Maximum Single
    fmaxs,

    /// Floating-Point Square Root Double
    fsqrts,

    /// Floating-Point Load Word (Single)
    flw,
    /// Floating-Point Load Word (Single)
    fsw,

    /// Floating-Point Equal Single
    feqs,
    /// Floating-Point Less than Single
    flts,
    /// Floating-Point Less than OR Equal Single
    fles,

    // D extension (64-bit float)
    /// Floating-Point Add Double
    faddd,
    /// Floating-Point Subtract Double
    fsubd,
    /// Floating-Point Multiply Double
    fmuld,
    /// Floating-Point Divide Double
    fdivd,

    /// Floating-Point Minimum Double
    fmind,
    /// Floating-Point Maximum Double
    fmaxd,

    /// Floating-Point Square Root Double
    fsqrtd,

    /// Floating-Point Load Double
    fld,
    /// Floating-Point Store Double
    fsd,

    /// Floating-Point Equal Double
    feqd,
    /// Floating-Point Less than Double
    fltd,
    /// Floating-Point Less than OR Equal Double
    fled,

    /// Floating-Point Convert Wide to Single
    fcvtws,
    /// Floating-Point Convert Unsigned Wide to Single
    fcvtwus,
    /// Floating-Point Convert Long to Single
    fcvtls,
    /// Floating-Point Convert Unsigned Long to Single
    fcvtlus,

    /// Floating-Point Convert Wide to Double
    fcvtwd,
    /// Floating-Point Convert Unsigned Wide to Double
    fcvtwud,
    /// Floating-Point Convert Long to Double
    fcvtld,
    /// Floating-Point Convert Unsigned Long to Double
    fcvtlud,

    /// Floating-Point Convert Single to Wide
    fcvtsw,
    /// Floating-Point Convert Single to Unsigned Wide
    fcvtswu,
    /// Floating-Point Convert Single to Long
    fcvtsl,
    /// Floating-Point Convert Single to Unsigned Long
    fcvtslu,

    /// Floating-Point Convert Double to Wide
    fcvtdw,
    /// Floating-Point Convert Double to Unsigned Wide
    fcvtdwu,
    /// Floating-Point Convert Double to Long
    fcvtdl,
    /// Floating-Point Convert Double to Unsigned Long
    fcvtdlu,

    /// Floating-Point Sign Inject Negate Single
    fsgnjns,
    /// Floating-Point Sign Inject Negate Double
    fsgnjnd,

    /// Floating-Point Sign Inject Absolute Single
    fsgnjxs,
    /// Floating-Point Sign Inject Absolute Double
    fsgnjxd,

    // Zicsr Extension Instructions
    csrrs,

    // V Extension Instructions
    vsetvli,
    vsetivli,
    vaddvv,
    vadcvv,
    vfaddvv,
    vsubvv,
    vfsubvv,
    vmulvv,
    vfmulvv,
    vslidedownvx,

    vle8v,
    vle16v,
    vle32v,
    vle64v,

    vse8v,
    vse16v,
    vse32v,
    vse64v,

    vmvvx,
    vmvxs,

    vredsumvs,
    vredandvs,
    vredorvs,
    vredxorvs,
    vredminuvs,
    vredminvs,
    vredmaxuvs,
    vredmaxvs,

    // Zbb Extension Instructions
    clz,
    clzw,
    cpop,
    cpopw,

    // A Extension Instructions
    fence,
    fencetso,

    lrw,
    scw,
    amoswapw,
    amoaddw,
    amoandw,
    amoorw,
    amoxorw,
    amomaxw,
    amominw,
    amomaxuw,
    amominuw,

    lrd,
    scd,
    amoswapd,
    amoaddd,
    amoandd,
    amoord,
    amoxord,
    amomaxd,
    amomind,
    amomaxud,
    amominud,

    // Pseudo-instructions. Used for anything that isn't 1:1 with an
    // assembly instruction.

    /// Pseudo-instruction that will generate a backpatched
    /// function prologue.
    pseudo_prologue,
    /// Pseudo-instruction that will generate a backpatched
    /// function epilogue
    pseudo_epilogue,

    /// Pseudo-instruction: End of prologue
    pseudo_dbg_prologue_end,
    /// Pseudo-instruction: Beginning of epilogue
    pseudo_dbg_epilogue_begin,
    /// Pseudo-instruction: Update debug line
    pseudo_dbg_line_column,

    /// Pseudo-instruction that loads from memory into a register.
    pseudo_load_rm,
    /// Pseudo-instruction that stores from a register into memory
    pseudo_store_rm,
    /// Pseudo-instruction that loads the address of memory into a register.
    pseudo_lea_rm,
    /// Jumps. Uses `inst` payload.
    pseudo_j,
    /// Dead inst, ignored by the emitter.
    pseudo_dead,
    /// Loads the address of a value that hasn't yet been allocated in memory.
    pseudo_load_symbol,
    /// Loads the address of a TLV.
    pseudo_load_tlv,

    /// Moves the value of rs1 to rd.
    pseudo_mv,

    pseudo_restore_regs,
    pseudo_spill_regs,

    pseudo_compare,
    pseudo_not,
    pseudo_extern_fn_reloc,
};

pub const Pseudo = enum(u8) {
    li,
    mv,
    tail,
    beqz,
    ret,
};
