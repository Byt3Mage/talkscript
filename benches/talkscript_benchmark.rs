use criterion::{Criterion, criterion_group, criterion_main};

fn my_benchmark(c: &mut Criterion) {
    c.bench_function("fib 20", |b| b.iter(|| fibonacci(20)));
}

fn fibonacci(n: u64) -> u64 {
    match n {
        0 => 0,
        1 => 1,
        n => fibonacci(n - 1) + fibonacci(n - 2),
    }
}

criterion_group!(benches, my_benchmark);
criterion_main!(benches);
