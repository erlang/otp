{suites,"../ssh_test",all}.
{skip_suites, "../ssh_test",
 [ssh_bench_SUITE, ssh_upgrade_SUITE], "Benchmarks run separately"}.
{event_handler, {cte_track, []}}.
{enable_builtin_hooks, false}.
{ct_hooks, [{cth_log_redirect, [{mode, replace}]}]}.
