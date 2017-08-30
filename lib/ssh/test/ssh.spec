{suites,"../ssh_test",all}.

{skip_suites, "../ssh_test", [ssh_bench_SUITE,
	                      ssh_upgrade_SUITE
                             ],
 "Benchmarks run separately"}.


