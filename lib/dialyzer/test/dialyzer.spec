{alias, tests, "."}.

{suites, tests, all}.

{skip_cases, tests, opaque_tests_SUITE, crash,
    "Dialyzer team is working on this one"}.

{skip_cases, tests, opaque_tests_SUITE, inf_loop1, "Unsupported"}.

{skip_cases, tests, r9c_tests_SUITE, mnesia,
    "Dialyzer team is working on this one"}.

{skip_cases, tests, small_tests_SUITE, non_existing,
    "Dialyzer team is working on this one"}.