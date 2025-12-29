use std::collections::VecDeque;

use ahash::{AHashMap, AHashSet};

use crate::vm::{
    Frame,
    heap::GCPtr,
    object::{GCTask, Value},
    unit::CallInfo,
};

pub enum WaitReason {
    AwaitingTask(GCPtr),
    Timer(u64),
    Io(u64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TaskStatus {
    Ready,
    Waiting,
    Completed,
    Cancelled,
}

pub struct Scheduler {
    pub(crate) current_task: Option<GCPtr>,
    pub(crate) ready_queue: VecDeque<GCPtr>,
    pub(crate) waiting: AHashMap<GCPtr, WaitReason>,
    pub(crate) completed: AHashSet<GCPtr>,
}

impl Scheduler {
    pub fn new() -> Self {
        Self {
            current_task: None,
            ready_queue: VecDeque::new(),
            waiting: AHashMap::new(),
            completed: AHashSet::new(),
        }
    }

    #[inline]
    pub(super) fn next_ready(&mut self) -> Option<GCPtr> {
        self.ready_queue.pop_front()
    }

    pub(super) fn suspend(&mut self, mut task: GCPtr, reason: WaitReason) {
        task.as_mut::<GCTask>().get_mut().status = TaskStatus::Waiting;
        self.waiting.insert(task, reason);
    }

    pub(super) fn complete(&mut self, mut task: GCPtr) {
        task.as_mut::<GCTask>().get_mut().status = TaskStatus::Completed;
        self.completed.insert(task);
        self.wake_waiters(task);
    }

    pub(super) fn cancel(&mut self, mut task: GCPtr) -> bool {
        match task.as_ref::<GCTask>().get().status {
            TaskStatus::Completed => return false,
            TaskStatus::Cancelled => return true,
            TaskStatus::Waiting => _ = self.waiting.remove(&task),
            TaskStatus::Ready => {}
        }

        task.as_mut::<GCTask>().get_mut().status = TaskStatus::Cancelled;
        self.completed.insert(task);
        true
    }

    fn wake_waiters(&mut self, completed_task: GCPtr) {
        let mut to_wake = Vec::new();

        for (&task, reason) in &self.waiting {
            if let WaitReason::AwaitingTask(awaited_task) = reason
                && *awaited_task == completed_task
            {
                to_wake.push(task);
            }
        }

        for mut task in to_wake {
            self.waiting.remove(&task);
            task.as_mut::<GCTask>().get_mut().status = TaskStatus::Ready;
            self.ready_queue.push_back(task);
        }
    }

    pub(super) fn is_done(&self, task: GCPtr) -> bool {
        self.completed.contains(&task)
    }
}

pub(super) struct Task {
    pub(super) registers: Vec<Value>,
    pub(super) call_stack: Vec<Frame>,
    pub(super) base_reg: usize,
    pub(super) call_info: CallInfo,
    pub(super) pc: usize,
    pub(super) status: TaskStatus,
}

impl Task {
    pub(super) fn new(call_info: CallInfo, args: &[Value]) -> Self {
        let mut registers = vec![Value::zero(); call_info.nreg as usize];
        registers[..args.len()].copy_from_slice(args);

        Self {
            registers,
            pc: call_info.entry_pc,
            call_info,
            call_stack: vec![],
            base_reg: 0,
            status: TaskStatus::Ready,
        }
    }

    #[inline(always)]
    pub(super) fn is_complete(&self) -> bool {
        self.status == TaskStatus::Completed
    }

    #[inline(always)]
    pub(super) fn is_cancelled(&self) -> bool {
        self.status == TaskStatus::Cancelled
    }
}
