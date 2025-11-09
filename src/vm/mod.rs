use crate::vm::instruction::{Instruction, Opcode};
use crate::vm::vm_types::{FunctionInfo, VMType};

pub mod heap;
pub mod instruction;
pub mod vm_types;

type Value = u64;

#[derive(Debug, Copy, Clone)]
pub enum VMError {
    Error,
}

pub type VMResult = Result<Value, VMError>;

pub struct Frame {
    /// Where in the instructions to return after this call.
    ret_pc: usize,
    /// Offset into [VM] registers where this function starts.
    base_reg: usize,
    /// Register in caller to place return value
    ret_reg: u8,
    /// Number of registers allocated for this function.
    num_regs: u8,
}

pub struct Constants {
    values: Vec<Value>,
    functions: Vec<FunctionInfo>,
}

impl Constants {
    pub fn new() -> Self {
        Self {
            values: vec![],
            functions: vec![],
        }
    }

    #[inline]
    pub fn push_value(&mut self, value: impl VMType) -> usize {
        let idx = self.values.len();
        self.values.push(value.to_value());
        idx
    }

    #[inline]
    pub fn push_function(&mut self, func: FunctionInfo) -> usize {
        let idx = self.functions.len();
        self.functions.push(func);
        idx
    }
}

pub struct VM {
    pub registers: Vec<u64>,
    pub constants: Constants,
    pub call_stack: Vec<Frame>,
    /// base register index of current frame
    pub base_reg: usize,
    pub program: Vec<Instruction>,
    pub pc: usize,
}

impl VM {
    #[inline(always)]
    fn get_reg(&self, reg: u8) -> u64 {
        self.registers[self.base_reg + reg as usize]
    }

    #[inline(always)]
    fn set_reg(&mut self, reg: u8, value: u64) {
        self.registers[self.base_reg + reg as usize] = value;
    }

    #[inline(always)]
    fn exec_mov(&mut self, instr: Instruction) {
        let dst = instr.a();
        let src = instr.b();
        self.set_reg(dst, self.get_reg(src));
    }

    #[inline(always)]
    fn exec_iadd(&mut self, instr: Instruction) {
        let a = self.get_reg(instr.b()) as i64;
        let b = self.get_reg(instr.c()) as i64;
        self.set_reg(instr.a(), (a + b) as u64);
    }

    #[inline(always)]
    fn exec_isub(&mut self, instr: Instruction) {
        let a = self.get_reg(instr.b()) as i64;
        let b = self.get_reg(instr.c()) as i64;
        self.set_reg(instr.a(), (a - b) as u64);
    }

    #[inline(always)]
    fn exec_imul(&mut self, instr: Instruction) {
        let a = self.get_reg(instr.b()) as i64;
        let b = self.get_reg(instr.c()) as i64;
        self.set_reg(instr.a(), (a * b) as u64);
    }

    #[inline(always)]
    fn exec_idiv(&mut self, instr: Instruction) {
        let a = self.get_reg(instr.b()) as i64;
        let b = self.get_reg(instr.c()) as i64;
        self.set_reg(instr.a(), (a / b) as u64);
    }

    #[inline(always)]
    fn exec_irem(&mut self, instr: Instruction) {
        let a = self.get_reg(instr.b()) as i64;
        let b = self.get_reg(instr.c()) as i64;
        self.set_reg(instr.a(), (a % b) as u64);
    }

    #[inline(always)]
    fn exec_ieq(&mut self, instr: Instruction) {
        let a = self.get_reg(instr.b()) as i64;
        let b = self.get_reg(instr.c()) as i64;
        self.set_reg(instr.a(), (a == b) as u64);
    }

    #[inline(always)]
    fn exec_ine(&mut self, instr: Instruction) {
        let a = self.get_reg(instr.b()) as i64;
        let b = self.get_reg(instr.c()) as i64;
        self.set_reg(instr.a(), (a != b) as u64);
    }

    #[inline(always)]
    fn exec_ilt(&mut self, instr: Instruction) {
        let a = self.get_reg(instr.b()) as i64;
        let b = self.get_reg(instr.c()) as i64;
        self.set_reg(instr.a(), (a < b) as u64);
    }

    #[inline(always)]
    fn exec_igt(&mut self, instr: Instruction) {
        let a = self.get_reg(instr.b()) as i64;
        let b = self.get_reg(instr.c()) as i64;
        self.set_reg(instr.a(), (a > b) as u64);
    }

    #[inline(always)]
    fn exec_ile(&mut self, instr: Instruction) {
        let a = self.get_reg(instr.b()) as i64;
        let b = self.get_reg(instr.c()) as i64;
        self.set_reg(instr.a(), (a <= b) as u64);
    }

    #[inline(always)]
    fn exec_ige(&mut self, instr: Instruction) {
        let a = self.get_reg(instr.b()) as i64;
        let b = self.get_reg(instr.c()) as i64;
        self.set_reg(instr.a(), (a >= b) as u64);
    }

    #[inline(always)]
    fn exec_fadd(&mut self, instr: Instruction) {
        let a = f64::from_bits(self.get_reg(instr.b()));
        let b = f64::from_bits(self.get_reg(instr.c()));
        self.set_reg(instr.a(), (a + b).to_bits());
    }

    #[inline(always)]
    fn exec_fsub(&mut self, instr: Instruction) {
        let a = f64::from_bits(self.get_reg(instr.b()));
        let b = f64::from_bits(self.get_reg(instr.c()));
        self.set_reg(instr.a(), (a - b).to_bits());
    }

    #[inline(always)]
    fn exec_load(&mut self, instr: Instruction) {
        self.set_reg(instr.a(), self.constants.values[instr.bx() as usize]);
    }

    #[inline(always)]
    fn exec_jump(&mut self, instr: Instruction) {
        self.pc = instr.ax() as usize;
    }

    #[inline(always)]
    fn exec_jump_if(&mut self, instr: Instruction) {
        if self.get_reg(instr.a()) != 0 {
            self.pc = instr.bx() as usize;
        }
    }

    #[inline(always)]
    fn exec_jump_if_not(&mut self, instr: Instruction) {
        if self.get_reg(instr.a()) == 0 {
            self.pc = instr.bx() as usize;
        }
    }

    fn exec_call(&mut self, instr: Instruction) {
        let ret_reg = instr.a();
        let func = &self.constants.functions[instr.bx() as usize];
        let base = self.registers.len();

        self.registers.resize(base + func.num_regs as usize, 0);

        // Copy args from caller into callee registers
        // Call convention states that args are in caller registers: ret + 1..=ret + arity
        let nargs = func.arity as usize;
        let start = self.base_reg + (ret_reg as usize + 1);

        self.registers.copy_within(start..start + nargs, base + 1);

        // Push call frame
        self.call_stack.push(Frame {
            ret_pc: self.pc,
            ret_reg,
            base_reg: base,
            num_regs: func.num_regs,
        });

        // Jump to function entry point
        self.pc = func.entry_pc;
        self.base_reg = base;
    }

    fn exec_tail_call(&mut self, instr: Instruction) {
        let func = &self.constants.functions[instr.ax() as usize];

        self.registers
            .resize(self.base_reg + func.num_regs as usize, 0);

        if let Some(frame) = self.call_stack.last_mut() {
            frame.num_regs = func.num_regs;
        }

        self.pc = func.entry_pc;
    }

    pub fn run_function(&mut self, func: &FunctionInfo, args: &[Value]) -> VMResult {
        assert_eq!(func.arity as usize, args.len(), "Incorrect argument count");

        // Create a new register window
        let base_reg = self.registers.len();
        self.registers.resize(base_reg + func.num_regs as usize, 0);

        // Copy arguments into callee registers
        let dst_start = base_reg + 1;
        self.registers[dst_start..dst_start + args.len()].copy_from_slice(args);

        // Push synthetic frame with ret_pc = usize::MAX
        self.call_stack.push(Frame {
            ret_pc: usize::MAX,
            ret_reg: 0,
            base_reg,
            num_regs: func.num_regs,
        });

        // Start execution
        self.pc = func.entry_pc;
        self.base_reg = base_reg;
        self.run()
    }

    pub fn run(&mut self) -> VMResult {
        while self.pc < self.program.len() {
            let instr = self.program[self.pc];
            self.pc += 1;

            match instr.opcode() {
                // Integer arithmetic
                Opcode::IADD => self.exec_iadd(instr),
                Opcode::ISUB => self.exec_isub(instr),
                Opcode::IMUL => self.exec_imul(instr),
                Opcode::IDIV => self.exec_idiv(instr),
                Opcode::IREM => self.exec_irem(instr),

                // Integer comparisons
                Opcode::IEQ => self.exec_ieq(instr),
                Opcode::INE => self.exec_ine(instr),
                Opcode::ILT => self.exec_ilt(instr),
                Opcode::IGT => self.exec_igt(instr),
                Opcode::ILE => self.exec_ile(instr),
                Opcode::IGE => self.exec_ige(instr),

                // Floating point arithmetic
                Opcode::FADD => self.exec_fadd(instr),
                Opcode::FSUB => self.exec_fsub(instr),

                // Load operations
                Opcode::LOAD_INT => self.exec_load(instr),

                // Jump operations
                Opcode::JMP => self.exec_jump(instr),
                Opcode::JMPIF => self.exec_jump_if(instr),
                Opcode::JMPIFNOT => self.exec_jump_if_not(instr),

                // Call and Return
                Opcode::CALL => self.exec_call(instr),
                Opcode::TAIL_CALL => self.exec_tail_call(instr),
                Opcode::RET => {
                    let frame = self.call_stack.pop().expect("Return with empty call stack");
                    let ret_val = self.registers[frame.base_reg];

                    if frame.ret_pc == usize::MAX {
                        return Ok(ret_val);
                    }

                    if let Some(caller) = self.call_stack.last() {
                        self.base_reg = caller.base_reg;
                        self.pc = frame.ret_pc;
                        self.set_reg(frame.ret_reg, ret_val);
                        self.registers.truncate(frame.base_reg);
                    }
                }

                Opcode::MOV => self.exec_mov(instr),

                Opcode::HALT => break,
                _ => panic!("Unknown opcode: {:?}", instr.opcode()),
            }
        }

        Err(VMError::Error)
    }
}
