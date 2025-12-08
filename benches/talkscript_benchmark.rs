use criterion::{Criterion, criterion_group, criterion_main};

fn my_benchmark(c: &mut Criterion) {
    c.bench_function("allocation", |b| b.iter(|| {}));
}

criterion_group!(benches, my_benchmark);
criterion_main!(benches);
