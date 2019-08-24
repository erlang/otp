{config, "../test_server/ts.config"}.
{config, "../test_server/ts.unix.config"}.

{suites,"../kernel_test", all}.
{skip_suites,"../kernel_test",[logger_stress_SUITE],"Benchmarks only"}.
