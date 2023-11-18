{suites,"../inets_test", all}.
{skip_suites, "../inets_test", [httpd_bench_SUITE],
    "Benchmarks run separately"}.
{event_handler, {cte_track, []}}.
{enable_builtin_hooks, false}.
{ct_hooks, [{cth_log_redirect, [{mode, replace}]}]}.
