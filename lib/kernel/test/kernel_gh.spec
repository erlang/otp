{suites,"../kernel_test", all}.
{skip_suites,"../kernel_test",[logger_stress_SUITE],"Benchmarks only"}.
{skip_cases, "../kernel_test", global_SUITE,
 [many_nodes, lost_connection2, simple_resolve2],"Broken in docker"}.
