%% Needed to compile ,unicode_util_SUITE and string_SUITE...
{cases,"../stdlib_test",unicode_util_SUITE, []}.
{cases,"../stdlib_test",string_SUITE, []}.
{skip_suites,"../stdlib_test",unicode_util_SUITE, "bench only"}.
{skip_suites,"../stdlib_test",string_SUITE, "bench only"}.

{suites,"../stdlib_test",[stdlib_bench_SUITE]}.
{skip_groups,"../stdlib_test",stdlib_bench_SUITE,
             [gen_server_comparison,gen_statem_comparison],
             "Not a benchmark"}.
{groups,"../stdlib_test",ets_SUITE,[benchmark]}.
