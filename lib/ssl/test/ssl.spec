{suites,"../ssl_test",all}.
{skip_suites, "../ssl_test",
     [ssl_bench_SUITE, ssl_dist_bench_SUITE],
     "Benchmarks run separately"}.
