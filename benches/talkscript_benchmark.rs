use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};
use talkscript::vm::{
    Constants, VM,
    instruction::*,
    vm_types::{FunctionInfo, VMType},
};

fn my_benchmark(c: &mut Criterion) {
    let program = vec![
        // sum(n) -> int:
        // r1 = n
        // r2 = sum
        // r3 = i
        load_int(4, 0), // r4 = 1
        // loop:
        ilt(5, 3, 1),     // r3 < n
        jmp_if_not(5, 6), // if r5 == false, exit loop
        iadd(2, 2, 3),    // sum += 1
        iadd(3, 3, 4),    // i += 1
        jmp(1),           // go back to loop
        //loop end
        mov(0, 2),
        ret(),
    ];

    let mut constants = Constants::new();
    constants.push_value(1i64);
    constants.push_function(FunctionInfo {
        entry_pc: 0,
        arity: 1,
        num_regs: 5,
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
        arity: 1,
        num_regs: 6,
    };

    c.bench_function("factorial", |b| {
        b.iter(|| {
            let result = black_box(vm.run_function(&main, &[100i64.to_value()]).unwrap());
            assert_eq!(result as i64, 4950);
        })
    });
}

criterion_group!(benches, my_benchmark);
criterion_main!(benches);
