{suites,"../crypto_test",all}.

{skip_suites, "../crypto_test", [crypto_bench_SUITE
	],
 "Benchmarks run separately"}.

