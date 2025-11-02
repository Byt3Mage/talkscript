use crate::vm::instruction::{Instruction, Opcode};
use crate::vm::instruction::{ile, imul, isub, jmp_if_not, load, mov, ret, tail_call};
use crate::vm::vm_types::FunctionInfo;
use std::time::Instant;

mod heap;
pub mod instruction;
mod stack;
pub(crate) mod vm_types;

#[derive(Debug, Copy, Clone)]
pub enum VMError {
    Error,
}

pub type VMResult = Result<u64, VMError>;

struct CallFrame {
    /// Where in the instructions to return after this call.
    ret_pc: usize,
    /// Offset into [VM] registers where this function starts.
    base_reg: usize,
    /// Register in caller to place return value
    ret_reg: u8,
    /// Number of registers allocated for this function.
    num_regs: u8,
}

struct Constants {
    values: Vec<u64>,
    functions: Vec<FunctionInfo>,
}

impl Constants {
    fn new() -> Self {
        Self {
            values: vec![],
            functions: vec![],
        }
    }
}

pub struct VM {
    registers: Vec<u64>,
    constants: Constants,
    call_stack: Vec<CallFrame>,
    base_reg: usize,
    program: Vec<Instruction>,
    pc: usize,
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

        // Create a new register window
        let new_base_reg = self.registers.len();
        self.registers
            .resize(new_base_reg + func.num_regs as usize, 0);

        // Copy args from caller into callee registers
        // Call convention states that args are in caller registers: ret + 1..=ret + arity
        let nargs = func.arity as usize;
        let src_start = self.base_reg + (ret_reg as usize + 1);
        self.registers
            .copy_within(src_start..src_start + nargs, new_base_reg + 1);

        // Push call frame
        self.call_stack.push(CallFrame {
            ret_pc: self.pc,
            ret_reg,
            base_reg: new_base_reg,
            num_regs: func.num_regs,
        });

        // Jump to function entry point
        self.pc = func.entry_pc;
        self.base_reg = new_base_reg;
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

    pub fn run_function(&mut self, func: &FunctionInfo, args: &[u64]) -> VMResult {
        assert_eq!(func.arity as usize, args.len(), "Incorrect argument count");

        // Create a new register window
        let base_reg = self.registers.len();
        self.registers.resize(base_reg + func.num_regs as usize, 0);

        // Copy arguments into callee registers
        let dst_start = base_reg + 1;
        self.registers[dst_start..dst_start + args.len()].copy_from_slice(args);

        // Push synthetic frame with ret_pc = usize::MAX
        self.call_stack.push(CallFrame {
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
                Opcode::LOAD => self.exec_load(instr),

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

#[test]
fn test_vm() {
    fn fact(n: i32, acc: i32) -> i32 {
        if n <= 1 { acc } else { fact(n - 1, acc * n) }
    }

    println!("{}", fact(7, 1));

    let program = vec![
        // fn fact(n: int, acc: int) -> int
        // r1 = n, r2 = acc
        load(3, 0),       //r3 = 1
        ile(4, 1, 3),     //r3 = n <= 1
        jmp_if_not(4, 5), // jum to tail call
        mov(0, 2),
        ret(),
        // tail_call:
        isub(4, 1, 3), //r4 = n - 1
        imul(5, 1, 2), //r5 = n * acc
        mov(1, 4),
        mov(2, 5),
        tail_call(0),
    ];

    let mut constants = Constants::new();
    constants.values.push(1i64 as u64);

    constants.functions.push(FunctionInfo {
        entry_pc: 0,
        arity: 2,
        num_regs: 6,
    });

    let mut vm = VM {
        registers: vec![],
        constants,
        call_stack: vec![],
        base_reg: 0,
        pc: 0,
        program,
    };

    let main = FunctionInfo {
        entry_pc: 0,
        arity: 2,
        num_regs: 6,
    };

    let start = Instant::now();
    let result = vm.run_function(&main, &[7i64 as u64, 1i64 as u64]).unwrap();
    let result = result as i64;
    let duration = start.elapsed();
    println!("fibonacci({}) = {} | Time: {:?}", 3, result, duration);
}
