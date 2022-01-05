{enable_builtin_hooks, false}.
{suites,"../emulator_test",all}.
{skip_groups,"../emulator_test",hash_SUITE,[phash2_benchmark],"Benchmark only"}.
{skip_cases,"../emulator_test",nif_SUITE,[t_dynamic_resource_call],"Unstable TC"}.
