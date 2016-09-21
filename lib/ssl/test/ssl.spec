{suites,"../ssl_test",all}.
{skip_cases, "../ssl_test",
    ssl_bench_SUITE, [setup_sequential, setup_concurrent, payload_simple,
	use_pem_cache, bypass_pem_cache],
    "Benchmarks run separately"}.
