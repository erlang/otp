{suites,"../crypto_test",all}.
{skip_suites, "../crypto_test", [crypto_bench_SUITE],
 "Benchmarks run separately"}.
{skip_cases, "../crypto_test", engine_SUITE,
 [multiple_engine_load],"Broken with openssl 1.1.1f and docker"}.
