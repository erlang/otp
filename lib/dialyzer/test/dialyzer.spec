{alias, tests, "../dialyzer_test"}.

{suites, tests, all}.

{skip_cases, tests, small_SUITE, "Needs compiler in plt"}.
