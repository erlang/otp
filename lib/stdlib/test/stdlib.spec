{suites,"../stdlib_test",all}.
{skip_groups,"../stdlib_test",stdlib_bench_SUITE,
             [base64,gen_server,gen_statem,unicode],
             "Benchmark only"}.
