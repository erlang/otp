{suites, "../diameter_test", all}.
{skip_cases,"../diameter_test",diameter_gen_sctp_SUITE,
 [send_one_from_many,send_many_from_one],
 "SCTP does not work on Github actions"}.
