use criterion::{criterion_group, criterion_main, Criterion};

fn small_map(c: &mut Criterion) {
    // Elixir: %{a: 73, b: 8248, c: "hello", d: :ok, e: 99, f: 24242.1234999}
    let small_map = [
        131, 116, 0, 0, 0, 6, 100, 0, 1, 97, 97, 73, 100, 0, 1, 98, 98, 0, 0, 32, 56, 100, 0, 1,
        99, 109, 0, 0, 0, 5, 104, 101, 108, 108, 111, 100, 0, 1, 100, 100, 0, 2, 111, 107, 100, 0,
        1, 101, 97, 99, 100, 0, 1, 102, 70, 64, 215, 172, 135, 231, 108, 31, 228,
    ];

    c.bench_function("decode_small_map", move |b| {
        b.iter(|| etf::parse(&small_map))
    });
}

fn big_list(c: &mut Criterion) {
    let big_list = std::fs::read("fixtures/big_list.etf").unwrap();

    c.bench_function("decode_big_list", move |b| b.iter(|| etf::parse(&big_list)));
}

fn big_map(c: &mut Criterion) {
    let big_map = std::fs::read("fixtures/big_map.etf").unwrap();

    c.bench_function("decode_big_map", move |b| b.iter(|| etf::parse(&big_map)));
}

criterion_group!(benches, small_map, big_list, big_map);
criterion_main!(benches);
