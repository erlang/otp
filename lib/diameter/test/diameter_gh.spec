{suites, "../diameter_test", all}.
{skip_suites, "../diameter_test", [diameter_gen_sctp_SUITE], "SCTP does not work on Github actions"}.
