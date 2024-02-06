% {merge_tests,false}.
{alias,dir,"../ssl_test"}.

{suites,dir,all}.
{skip_groups,all_nodes, dir, openssl_session_ticket_SUITE, 'openssl_server',
 {cases,[openssl_server_hrr]},"Unstable testcases"}.
{skip_groups,dir,ssl_bench_SUITE,setup,"Benchmarks run separately"}.
{skip_groups,dir,ssl_bench_SUITE,payload,"Benchmarks run separately"}.
{skip_groups,dir,ssl_bench_SUITE,pem_cache,"Benchmarks run separately"}.
{skip_groups,dir,ssl_dist_bench_SUITE,setup,"Benchmarks run separately"}.
{skip_groups,dir,ssl_dist_bench_SUITE,roundtrip,"Benchmarks run separately"}.
{skip_groups,dir,ssl_dist_bench_SUITE,throughput,"Benchmarks run separately"}.
{skip_groups,dir,ssl_dist_bench_SUITE,sched_utilization,"Benchmarks run separately"}.

{event_handler, {cte_track, []}}.
{enable_builtin_hooks, false}.
{ct_hooks, [{cth_log_redirect, [{mode, replace}]}]}.
