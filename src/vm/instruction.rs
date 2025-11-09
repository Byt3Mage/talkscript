macro_rules! define_opcodes {
    ($($name:ident = $value:expr),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        #[repr(transparent)]
        pub struct Opcode(pub u8);

        impl Opcode {
            $(pub const $name: Self = Self($value);)*
        }
    };
}

define_opcodes! {
    // Move between registers
    MOV = 0,
    // Move from immediate 16-bit value
    MOV_IMM = 1,
    // Move from value in constants table
    MOV_CST = 2,

    // Integer arithmetic
    IADD = 10,
    ISUB = 11,
    IMUL = 12,
    IDIV = 13,
    IREM = 14,

    // Integer comparisons
    IEQ = 15,
    INE = 16,
    ILT = 17,
    IGT = 18,
    ILE = 19,
    IGE = 20,

    // Floating-point arithmetic
    FADD = 30,
    FSUB = 31,
    FMUL = 32,
    FDIV = 33,
    FREM = 34,

    // Floating-point comparisons
    FEQ = 35,
    FNE = 36,
    FLT = 37,
    FGT = 38,
    FLE = 39,
    FGE = 40,

    // Load operations
    LOAD_INT = 50,
    LOAD_FLT = 51,
    LOAD_BOL = 52,
    LOAD_PTR = 53,

    // Jumps
    JMP      = 60,  // Unconditional jump
    JMPIF    = 61,  // Conditional: jump if rA is true
    JMPIFNOT = 62,  // Conditional: jump if rA is false

    // Call and return
    CALL      = 70,
    RET       = 71,
    TAIL_CALL = 72,

    // Heap
    ALLOC_STRUCT     = 80,
    GET_STRUCT_FIELD = 81,
    SET_STRUCT_FIELD = 82,

    // End program
    HALT = 255,
}

#[derive(Debug, Clone, Copy)]
pub struct Instruction(u32);

impl Instruction {
    #[inline(always)]
    pub fn opcode(self) -> Opcode {
        Opcode((self.0 & 0xFF) as u8)
    }

    // --- ABC Format ---
    #[inline(always)]
    pub fn a(self) -> u8 {
        ((self.0 >> 8) & 0xFF) as u8
    }

    #[inline(always)]
    pub fn b(self) -> u8 {
        ((self.0 >> 16) & 0xFF) as u8
    }

    #[inline(always)]
    pub fn c(self) -> u8 {
        ((self.0 >> 24) & 0xFF) as u8
    }

    #[inline(always)]
    pub fn encode_abc(Opcode(op): Opcode, a: u8, b: u8, c: u8) -> Self {
        Instruction((op as u32) | ((a as u32) << 8) | ((b as u32) << 16) | ((c as u32) << 24))
    }

    // --- ABx Format ---
    #[inline(always)]
    pub fn bx(self) -> u16 {
        ((self.0 >> 16) & 0xFFFF) as u16
    }

    #[inline(always)]
    pub fn encode_abx(Opcode(op): Opcode, a: u8, bx: u16) -> Self {
        Instruction((op as u32) | ((a as u32) << 8) | ((bx as u32) << 16))
    }

    // --- Ax Format ---
    #[inline(always)]
    pub fn ax(self) -> u32 {
        self.0 >> 8
    }

    #[inline(always)]
    pub fn encode_ax(Opcode(op): Opcode, ax: u32) -> Self {
        debug_assert!(ax <= 0xFFFFFF, "Ax field must fit in 24 bits");
        Instruction((op as u32) | (ax << 8))
    }
}

#[inline(always)]
pub fn mov(r_dst: u8, r_src: u8) -> Instruction {
    Instruction::encode_abc(Opcode::MOV, r_dst, r_src, 0)
}

// --- Integer arithmetic ---
#[inline(always)]
pub fn iadd(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::IADD, r_dst, r_a, r_b)
}

#[inline(always)]
pub fn isub(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::ISUB, r_dst, r_a, r_b)
}

#[inline(always)]
pub fn imul(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::IMUL, r_dst, r_a, r_b)
}

#[inline(always)]
pub fn idiv(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::IDIV, r_dst, r_a, r_b)
}

#[inline(always)]
pub fn irem(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::IREM, r_dst, r_a, r_b)
}

// --- Integer comparisons ---
#[inline(always)]
pub fn ieq(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::IEQ, r_dst, r_a, r_b)
}

#[inline(always)]
pub fn ine(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::INE, r_dst, r_a, r_b)
}

#[inline(always)]
pub fn ilt(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::ILT, r_dst, r_a, r_b)
}

#[inline(always)]
pub fn igt(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::IGT, r_dst, r_a, r_b)
}

#[inline(always)]
pub fn ile(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::ILE, r_dst, r_a, r_b)
}

#[inline(always)]
pub fn ige(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::IGE, r_dst, r_a, r_b)
}

// Floating-point arithmetic
#[inline(always)]
pub fn fadd(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::FADD, r_dst, r_a, r_b)
}

#[inline(always)]
pub fn fsub(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::FSUB, r_dst, r_a, r_b)
}

#[inline(always)]
pub fn fmul(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::FMUL, r_dst, r_a, r_b)
}

#[inline(always)]
pub fn fdiv(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::FDIV, r_dst, r_a, r_b)
}

#[inline(always)]
pub fn frem(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::FREM, r_dst, r_a, r_b)
}

// Floating-point comparisons
#[inline(always)]
pub fn feq(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::FEQ, r_dst, r_a, r_b)
}

#[inline(always)]
pub fn fne(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::FNE, r_dst, r_a, r_b)
}

#[inline(always)]
pub fn flt(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::FLT, r_dst, r_a, r_b)
}

#[inline(always)]
pub fn fgt(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::FGT, r_dst, r_a, r_b)
}

#[inline(always)]
pub fn fle(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::FLE, r_dst, r_a, r_b)
}

#[inline(always)]
pub fn fge(r_dst: u8, r_a: u8, r_b: u8) -> Instruction {
    Instruction::encode_abc(Opcode::FGE, r_dst, r_a, r_b)
}

// Load operations (ABx)
#[inline(always)]
pub fn load_int(r_dst: u8, index: u16) -> Instruction {
    Instruction::encode_abx(Opcode::LOAD_INT, r_dst, index)
}

#[inline(always)]
pub fn load_flt(r_dst: u8, index: u16) -> Instruction {
    Instruction::encode_abx(Opcode::LOAD_FLT, r_dst, index)
}

#[inline(always)]
pub fn load_bol(r_dst: u8, index: u16) -> Instruction {
    Instruction::encode_abx(Opcode::LOAD_BOL, r_dst, index)
}

#[inline(always)]
pub fn load_ptr(r_dst: u8, index: u16) -> Instruction {
    Instruction::encode_abx(Opcode::LOAD_PTR, r_dst, index)
}

// Jumps
#[inline(always)]
pub fn jmp(target: u32) -> Instruction {
    Instruction::encode_ax(Opcode::JMP, target)
}

#[inline(always)]
pub fn jmp_if(r_cond: u8, target: u16) -> Instruction {
    Instruction::encode_abx(Opcode::JMPIF, r_cond, target)
}

#[inline(always)]
pub fn jmp_if_not(r_cond: u8, target: u16) -> Instruction {
    Instruction::encode_abx(Opcode::JMPIFNOT, r_cond, target)
}

// Call and return
#[inline(always)]
pub fn call(r_ret: u8, func: u16) -> Instruction {
    Instruction::encode_abx(Opcode::CALL, r_ret, func)
}

#[inline(always)]
pub fn tail_call(func: u32) -> Instruction {
    Instruction::encode_ax(Opcode::TAIL_CALL, func)
}

#[inline(always)]
pub fn ret() -> Instruction {
    Instruction::encode_ax(Opcode::RET, 0)
}

// End program
#[inline(always)]
pub fn halt() -> Instruction {
    Instruction::encode_ax(Opcode::HALT, 0)
}
