
{config, "../cfgs/groups_2.1.cfg"}.
{alias, groups_2, "../groups_2"}.

{suites, groups_2, groups_21_SUITE}.
{skip_groups, groups_2, groups_21_SUITE,
    [test_group_1b, test_group_7], "Skip tg_1b & tg_7"}.
{skip_cases, groups_2, groups_21_SUITE,
    [testcase_1b, testcase_3a], "Skip tc_1b & tc_3a"}.

{groups, groups_2, groups_22_SUITE,
    test_group_1a}.
{skip_cases, groups_2, groups_22_SUITE,
    testcase_1a, "Skip tc_1a"}.

{groups, groups_2, groups_22_SUITE,
    test_group_1b}.
{skip_cases, groups_2, groups_22_SUITE,
    testcase_1b, "Skip tc_1b"}.
{skip_groups, groups_2, groups_21_SUITE,
    [test_group_3], "Skip tg_3"}.

{groups, groups_2, groups_22_SUITE,
    test_group_5}.
{skip_cases, groups_2, groups_22_SUITE,
    [testcase_7a, testcase_7b], "Skip tc_7a & tc_7b"}.
