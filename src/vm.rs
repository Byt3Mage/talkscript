use std::sync::Arc;

use thiserror::Error;

use crate::vm::{
    async_runtime::{Scheduler, Task, WaitReason},
    heap::{GCPtr, Heap},
    instruction::*,
    object::{AsValue, GCTask, Value},
    unit::{CallInfo, FuncInfo, NativeFuncInfo, Unit},
};

pub mod allocator;
pub mod async_runtime;
pub mod function;
pub mod heap;
pub mod instruction;
mod memory;
pub mod object;
pub mod safepoint;
pub mod unit;

#[derive(Debug, Copy, Clone, Error)]
pub enum VMError {
    #[error("Illegal opcode received: {0:?}")]
    IllegalOp(Opcode),
    #[error("Operation stack expected, but the call stack is empty")]
    EmptyCallStack,
    #[error("Invalid argument count: expected {exp}, got {got}")]
    InvalidArgCount { exp: u8, got: u8 },
    #[error("Attempted to await in a cancelled async task")]
    TaskCancelled,
    #[error("Await instruction received outside of an async task")]
    IllegalAwait,
    #[error("Program counter out of bounds")]
    PCOutOfBounds,
    #[error("Invalid conversion")]
    ValueConversionFailed,
}

pub type VMResult<T> = Result<T, VMError>;

struct CallerInfo {
    /// Owner compilation unit of our caller
    unit: Arc<Unit>,
    /// PC in caller instructions to return to
    ret_pc: usize,
    /// Caller registers base offset
    base_reg: usize,
    /// Start register in caller to put return value
    ret_reg: u8,
}

struct Frame {
    /// Saved caller state. None means we are a top level function with no caller
    caller_info: Option<CallerInfo>,
    callee_info: CallInfo,
}

pub struct VM {
    // VM state
    regs: Vec<Value>,
    call_stack: Vec<Frame>,
    native_call_ret: Box<[Value; u8::MAX as usize]>,
    heap: Heap,

    pc: usize,
    base_reg: usize,

    // Async runtime
    scheduler: Scheduler,

    // execution context
    unit: Arc<Unit>,
    bytecode: Arc<[Instruction]>,
    constants: Arc<[Value]>,
    functions: Arc<[FuncInfo]>,
    native_functions: Arc<[NativeFuncInfo]>,
}

impl VM {
    pub fn reset(&mut self) {
        self.regs.clear();
        self.call_stack.clear();
        self.base_reg = 0;
        todo!("reset heap");
    }

    #[inline(always)]
    fn reg(&self, reg: u8) -> Value {
        self.regs[self.base_reg + reg as usize]
    }

    #[inline(always)]
    fn reg_mut(&mut self, reg: u8) -> &mut Value {
        &mut self.regs[self.base_reg + reg as usize]
    }

    #[inline(always)]
    fn set_reg(&mut self, reg: u8, value: impl AsValue) {
        self.regs[self.base_reg + reg as usize] = value.into_value();
    }

    #[inline(always)]
    fn set_reg_raw(&mut self, reg: u8, value: Value) {
        self.regs[self.base_reg + reg as usize] = value;
    }

    #[inline(always)]
    fn two_reg<T: AsValue>(&self, reg_a: u8, reg_b: u8) -> (T, T) {
        (self.reg(reg_a).get(), self.reg(reg_b).get())
    }

    #[inline(always)]
    fn exec_mov(&mut self, i: Instruction) {
        self.set_reg_raw(i.a(), self.reg(i.b()));
    }

    #[inline(always)]
    fn exec_load(&mut self, i: Instruction) {
        self.set_reg_raw(i.a(), self.constants[i.bx() as usize]);
    }

    #[inline(always)]
    fn exec_not(&mut self, i: Instruction) {
        let dst = self.reg_mut(i.a());
        let val: bool = dst.get();
        dst.set(!val);
    }

    #[inline(always)]
    fn exec_ineg(&mut self, i: Instruction) {
        let dst = self.reg_mut(i.a());
        let val: i64 = dst.get();
        dst.set(-val);
    }

    #[inline(always)]
    fn exec_fneg(&mut self, i: Instruction) {
        let dst = self.reg_mut(i.a());
        let val: f64 = dst.get();
        dst.set(-val);
    }

    #[inline(always)]
    fn exec_iadd(&mut self, i: Instruction) {
        let (a, b) = self.two_reg::<i64>(i.b(), i.c());
        self.set_reg(i.a(), a + b);
    }

    #[inline(always)]
    fn exec_isub(&mut self, i: Instruction) {
        let (a, b) = self.two_reg::<i64>(i.b(), i.c());
        self.set_reg(i.a(), a - b);
    }

    #[inline(always)]
    fn exec_imul(&mut self, i: Instruction) {
        let (a, b) = self.two_reg::<i64>(i.b(), i.c());
        self.set_reg(i.a(), a * b);
    }

    #[inline(always)]
    fn exec_idiv(&mut self, i: Instruction) {
        let (a, b) = self.two_reg::<i64>(i.b(), i.c());
        self.set_reg(i.a(), a / b);
    }

    #[inline(always)]
    fn exec_irem(&mut self, i: Instruction) {
        let (a, b) = self.two_reg::<i64>(i.b(), i.c());
        self.set_reg(i.a(), a & b);
    }

    #[inline(always)]
    fn exec_iadd_imm(&mut self, i: Instruction) {
        let dst = self.reg_mut(i.a());
        dst.set(dst.get::<i64>() + i.imm_i16());
    }

    #[inline(always)]
    fn exec_isub_imm(&mut self, i: Instruction) {
        let dst = self.reg_mut(i.a());
        dst.set(dst.get::<i64>() - i.imm_i16());
    }

    #[inline(always)]
    fn exec_imul_imm(&mut self, i: Instruction) {
        let dst = self.reg_mut(i.a());
        dst.set(dst.get::<i64>() * i.imm_i16());
    }

    #[inline(always)]
    fn exec_idiv_imm(&mut self, i: Instruction) {
        let dst = self.reg_mut(i.a());
        dst.set(dst.get::<i64>() / i.imm_i16());
    }

    #[inline(always)]
    fn exec_irem_imm(&mut self, i: Instruction) {
        let dst = self.reg_mut(i.a());
        dst.set(dst.get::<i64>() % i.imm_i16());
    }

    #[inline(always)]
    fn exec_ieq(&mut self, i: Instruction) {
        let (a, b) = self.two_reg::<i64>(i.b(), i.c());
        self.set_reg(i.a(), a == b);
    }

    #[inline(always)]
    fn exec_ine(&mut self, i: Instruction) {
        let (a, b) = self.two_reg::<i64>(i.b(), i.c());
        self.set_reg(i.a(), a != b);
    }

    #[inline(always)]
    fn exec_ilt(&mut self, i: Instruction) {
        let (a, b) = self.two_reg::<i64>(i.b(), i.c());
        self.set_reg(i.a(), a < b);
    }

    #[inline(always)]
    fn exec_igt(&mut self, i: Instruction) {
        let (a, b) = self.two_reg::<i64>(i.b(), i.c());
        self.set_reg(i.a(), a > b);
    }

    #[inline(always)]
    fn exec_ile(&mut self, i: Instruction) {
        let (a, b) = self.two_reg::<i64>(i.b(), i.c());
        self.set_reg(i.a(), a <= b);
    }

    #[inline(always)]
    fn exec_ige(&mut self, i: Instruction) {
        let (a, b) = self.two_reg::<i64>(i.b(), i.c());
        self.set_reg(i.a(), a >= b);
    }

    #[inline(always)]
    fn exec_fadd(&mut self, i: Instruction) {
        let (a, b) = self.two_reg::<f64>(i.b(), i.c());
        self.set_reg(i.a(), a + b);
    }

    #[inline(always)]
    fn exec_fsub(&mut self, i: Instruction) {
        let (a, b) = self.two_reg::<f64>(i.b(), i.c());
        self.set_reg(i.a(), a - b);
    }

    #[inline(always)]
    fn exec_fmul(&mut self, i: Instruction) {
        let (a, b) = self.two_reg::<f64>(i.b(), i.c());
        self.set_reg(i.a(), a * b);
    }

    #[inline(always)]
    fn exec_fdiv(&mut self, i: Instruction) {
        let (a, b) = self.two_reg::<f64>(i.b(), i.c());
        self.set_reg(i.a(), a / b);
    }

    #[inline(always)]
    fn exec_jump(&mut self, i: Instruction) {
        self.pc = i.ax() as usize;
    }

    #[inline(always)]
    fn exec_jump_true(&mut self, i: Instruction) {
        let cond = self.reg(i.a()).get::<bool>();
        if cond {
            self.pc = i.bx() as usize;
        }
    }

    #[inline(always)]
    fn exec_jump_false(&mut self, i: Instruction) {
        let cond = self.reg(i.a()).get::<bool>();
        if !cond {
            self.pc = i.bx() as usize;
        }
    }

    fn exec_call(&mut self, i: Instruction) {
        let func = &self.functions[i.bx() as usize];
        let cinfo = &func.call_info;

        // create register window
        let base = self.regs.len();
        self.regs.resize(base + cinfo.nreg as usize, Value::zero());

        // Copy args from caller into callee registers
        // Call convention: args are in caller registers[ret..ret + narg]
        let ret_reg = i.a();
        let start = self.base_reg + ret_reg as usize;
        let end = start + cinfo.narg as usize;
        self.regs.copy_within(start..end, base);

        self.call_stack.push(Frame {
            caller_info: Some(CallerInfo {
                unit: Arc::clone(&self.unit),
                ret_pc: self.pc,
                base_reg: self.base_reg,
                ret_reg,
            }),
            callee_info: cinfo.clone(),
        });

        self.base_reg = base;
        self.pc = cinfo.entry_pc;

        // Switch context if calling external function
        if let Some(id) = func.unit_id {
            self.unit = Arc::clone(&self.unit.imports[id]);
            self.bytecode = Arc::clone(&self.unit.bytecode);
            self.constants = Arc::clone(&self.unit.constants);
            self.functions = Arc::clone(&self.unit.functions);
            self.native_functions = Arc::clone(&self.unit.native_functions);
        }
    }

    fn exec_tail_call(&mut self, i: Instruction) -> VMResult<()> {
        let func = &self.functions[i.bx() as usize];
        let cinfo = &func.call_info;

        // Reuse register window: shrink or grow in-place
        let base = self.base_reg;
        self.regs.resize(base + cinfo.nreg as usize, Value::zero());

        // copy args into the same window
        let start = self.base_reg + i.a() as usize;
        let end = start + cinfo.narg as usize;
        self.regs.copy_within(start..end, base);

        // Reuse current frame and update the callee info.
        let frame = self.call_stack.last_mut().ok_or(VMError::EmptyCallStack)?;
        frame.callee_info = cinfo.clone();

        self.base_reg = base; // redundant, but the consistency makes me feel better
        self.pc = cinfo.entry_pc;

        // if cross-unit tailcall, switch context
        if let Some(id) = func.unit_id {
            self.unit = Arc::clone(&self.unit.imports[id]);
            self.bytecode = Arc::clone(&self.unit.bytecode);
            self.constants = Arc::clone(&self.unit.constants);
            self.functions = Arc::clone(&self.unit.functions);
            self.native_functions = Arc::clone(&self.unit.native_functions);
        }

        Ok(())
    }

    fn exec_ret(&mut self) -> VMResult<Option<Frame>> {
        let frame = self.call_stack.pop().ok_or(VMError::EmptyCallStack)?;

        match frame.caller_info {
            // No caller means top level function, exit run loop
            None => Ok(Some(frame)),
            // return to caller
            Some(caller_info) => {
                // copy return values to caller's registers
                let start = self.base_reg + frame.callee_info.ret_reg as usize;
                let range = start..(start + frame.callee_info.nret as usize);
                self.regs.copy_within(range, caller_info.ret_reg as usize);

                // clear register window
                self.regs.truncate(self.base_reg);

                // If we just returned from external function, switch context
                if !Arc::ptr_eq(&self.unit, &caller_info.unit) {
                    self.unit = caller_info.unit;
                    self.bytecode = Arc::clone(&self.unit.bytecode);
                    self.constants = Arc::clone(&self.unit.constants);
                    self.functions = Arc::clone(&self.unit.functions);
                    self.native_functions = Arc::clone(&self.unit.native_functions);
                }

                self.base_reg = caller_info.base_reg;
                self.pc = caller_info.ret_pc;

                Ok(None)
            }
        }
    }

    fn exec_call_native(&mut self, i: Instruction) -> VMResult<()> {
        let func = &self.unit.native_functions[i.bx() as usize];
        let narg = func.narg as usize;
        let nret = func.nret as usize;

        // Immutably borrow args from caller registers
        // Call convention: args are in caller registers[ret..ret + narg]
        let ret = self.base_reg + i.a() as usize;
        let args = &self.regs[ret..ret + narg];
        let results = &mut self.native_call_ret[..nret];

        // Call native function
        (func.func)(args, results)?;

        // Copy results back into caller registers
        self.regs[ret..ret + nret].copy_from_slice(results);

        Ok(())
    }

    fn exec_spawn_task(&mut self, i: Instruction) {
        let dst = i.a();
        let func = &self.functions[i.bx() as usize];
        let cinfo = func.call_info.clone();

        let start = self.base_reg + dst as usize;
        let args = &self.regs[start..start + cinfo.narg as usize];
        let ptr = self.heap.alloc_task(Task::new(cinfo, args));

        self.scheduler.ready_queue.push_back(ptr);
        self.set_reg(dst, ptr);
    }

    pub fn exec_await(&mut self, i: Instruction) -> VMResult<bool> {
        let mut ptr = self.scheduler.current_task.ok_or(VMError::IllegalAwait)?;

        if ptr.as_ref::<GCTask>().get().is_cancelled() {
            return Err(VMError::TaskCancelled);
        }

        let task: GCPtr = self.reg(i.b()).get();

        if task.as_ref::<GCTask>().get().is_complete() {
            //TODO: copy results into caller registers
            return Ok(true);
        }

        self.scheduler.suspend(ptr, WaitReason::AwaitingTask(task));

        let task = ptr.as_mut::<GCTask>().get_mut();
        std::mem::swap(&mut self.regs, &mut task.registers);
        std::mem::swap(&mut self.call_stack, &mut task.call_stack);
        task.base_reg = self.base_reg;
        task.pc = self.pc - 1; // pc pushed back so await is retried on resume

        Ok(false)
    }

    fn run<const SYNC: bool>(&mut self) -> VMResult<Option<Frame>> {
        while self.pc < self.bytecode.len() {
            let i = self.bytecode[self.pc];
            self.pc += 1;

            match i.op() {
                // Move operations
                Opcode::MOV => self.exec_mov(i),
                Opcode::LOAD => self.exec_load(i),

                // Unary operations
                Opcode::NOT => self.exec_not(i),
                Opcode::INEG => self.exec_ineg(i),
                Opcode::FNEG => self.exec_fneg(i),

                // Integer arithmetic
                Opcode::IADD => self.exec_iadd(i),
                Opcode::ISUB => self.exec_isub(i),
                Opcode::IMUL => self.exec_imul(i),
                Opcode::IDIV => self.exec_idiv(i),
                Opcode::IREM => self.exec_irem(i),

                // Immediate integer arithmetic
                Opcode::IADD_IMM => self.exec_iadd_imm(i),
                Opcode::ISUB_IMM => self.exec_isub_imm(i),
                Opcode::IMUL_IMM => self.exec_imul_imm(i),
                Opcode::IDIV_IMM => self.exec_idiv_imm(i),
                Opcode::IREM_IMM => self.exec_irem_imm(i),

                // Integer comparisons
                Opcode::IEQ => self.exec_ieq(i),
                Opcode::INE => self.exec_ine(i),
                Opcode::ILT => self.exec_ilt(i),
                Opcode::IGT => self.exec_igt(i),
                Opcode::ILE => self.exec_ile(i),
                Opcode::IGE => self.exec_ige(i),

                // Floating point arithmetic
                Opcode::FADD => self.exec_fadd(i),
                Opcode::FSUB => self.exec_fsub(i),
                Opcode::FMUL => self.exec_fmul(i),
                Opcode::FDIV => self.exec_fdiv(i),

                // Jump operations
                Opcode::JMP => self.exec_jump(i),
                Opcode::JMP_T => self.exec_jump_true(i),
                Opcode::JMP_F => self.exec_jump_false(i),

                Opcode::CALL => self.exec_call(i),
                Opcode::TAIL_CALL => self.exec_tail_call(i)?,
                Opcode::CALL_NATIVE => self.exec_call_native(i)?,
                Opcode::RET => {
                    if let Some(frame) = self.exec_ret()? {
                        return Ok(Some(frame));
                    }
                }
                Opcode::SPAWN => self.exec_spawn_task(i),
                Opcode::AWAIT => {
                    if SYNC {
                        return Err(VMError::IllegalAwait);
                    }

                    if !self.exec_await(i)? {
                        return Ok(None);
                    }
                }

                Opcode::HALT => return Ok(None),
                op => return Err(VMError::IllegalOp(op)),
            }
        }
        Err(VMError::PCOutOfBounds)
    }

    pub fn execute(&mut self, func_id: u16, args: &[Value]) -> VMResult<&[Value]> {
        // Reset VM state before running top level function.
        self.reset();

        let func = &self.functions[func_id as usize];
        let cinfo = &func.call_info;
        let argc = args.len() as u8;

        if cinfo.narg != argc {
            return Err(VMError::InvalidArgCount {
                exp: cinfo.narg,
                got: argc,
            });
        }

        self.regs.resize(cinfo.nreg as usize, Value::zero());
        self.regs[..argc as usize].copy_from_slice(args);

        self.base_reg = 0;
        self.pc = cinfo.entry_pc;

        // Push synthetic frame
        self.call_stack.push(Frame {
            caller_info: None,
            callee_info: cinfo.clone(),
        });

        match self.run::<true>()? {
            Some(frame) => {
                let start = self.base_reg + frame.callee_info.ret_reg as usize;
                let range = start..(start + frame.callee_info.nret as usize);
                Ok(&self.regs[range])
            }
            None => todo!("handle missing frame"),
        }
    }

    fn collect_roots(&self) -> Vec<GCPtr> {
        self.regs.iter().filter_map(|v| v.try_get()).collect()
    }
}
