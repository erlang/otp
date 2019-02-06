{define,'Dir',"../emulator_test"}.
{suites,'Dir',[smoke_test_SUITE]}.
{suites,'Dir',[time_SUITE]}.
{skip_cases,'Dir',time_SUITE,
    [univ_to_local,local_to_univ],"Depends on CET timezone"}.
{skip_cases,'Dir',time_SUITE,
    [consistency],"Not reliable in October and March"}.
{cases,'Dir',crypto_SUITE,[t_md5]}.
{cases,'Dir',float_SUITE,[fpe,cmp_integer]}.
{cases,'Dir',erts_debug_SUITE,[df]}.
