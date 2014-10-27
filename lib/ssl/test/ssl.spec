{suites,"../ssl_test",all}.
{skip_cases, "../ssl_test",
    ssl_bench_SUITE, [setup_sequential, setup_concurrent, payload_simple],
    "Benchmarks run separately"}.
