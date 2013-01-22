{suites,"../mnesia_test",all}.
{skip_cases,"../mnesia_test",mnesia_measure_test,
            [ram_meter],
            "Takes to long time"}.
{skip_cases,"../mnesia_test",mnesia_measure_test,
            [disc_meter],
            "Takes to long time"}.
{skip_cases,"../mnesia_test",mnesia_measure_test,
            [disc_only_meter],
            "Takes to long time"}.
{skip_cases,"../mnesia_test",mnesia_measure_test,[cost],"Takes to long time"}.
{skip_cases,"../mnesia_test",mnesia_measure_test,
            [dbn_meters],
            "Takes to long time"}.
{skip_cases,"../mnesia_test",mnesia_measure_test,
            [ram_tpcb,disc_tpcb,disc_only_tpcb],
            "Takes to long time"}.
{skip_cases,"../mnesia_test",mnesia_measure_test,
            [reader_disturbed_by_node_down,writer_disturbed_by_node_down,
             reader_disturbed_by_node_up,writer_disturbed_by_node_up,
             reader_disturbed_by_schema_ops,writer_disturbed_by_schema_ops,
             reader_disturbed_by_checkpoint,writer_disturbed_by_checkpoint,
             reader_disturbed_by_dump_log,writer_disturbed_by_dump_log,
             reader_disturbed_by_backup,writer_disturbed_by_backup,
             reader_disturbed_by_restore,writer_disturbed_by_restore,
             reader_competing_with_reader,reader_competing_with_writer,
             writer_competing_with_reader,writer_competing_with_writer],
            "Not yet implemented"}.
{skip_cases,"../mnesia_test",mnesia_measure_test,
            [measure_resource_consumption,determine_resource_leakage],
            "Not yet implemented"}.
{skip_cases,"../mnesia_test",mnesia_measure_test,
            [determine_system_limits,performance_at_min_config,
             performance_at_max_config,performance_at_full_load,
             resource_consumption_at_min_config,
             resource_consumption_at_max_config,
             resource_consumption_at_full_load],
            "Not yet implemented"}.
{skip_cases,"../mnesia_test",mnesia_measure_test,
            [ram_tpcb,disc_tpcb,disc_only_tpcb],
            "Takes too much time and memory"}.
{skip_cases,"../mnesia_test",mnesia_measure_test,
            [measure_all_api_functions],
            "Not yet implemented"}.
{skip_cases,"../mnesia_test",mnesia_examples_test,
            [company],
            "Not yet implemented"}.
{skip_cases,"../mnesia_test",mnesia_config_test,
            [ignore_fallback_at_startup],
            "Not yet implemented"}.
{skip_cases,"../mnesia_test",mnesia_evil_backup,
            [local_backup_checkpoint],
            "Not yet implemented"}.
{skip_cases,"../mnesia_test",mnesia_config_test,
            [max_wait_for_decision],
            "Not yet implemented"}.
{skip_cases,"../mnesia_test",mnesia_recovery_test,
            [after_full_disc_partition],
            "Not yet implemented"}.
{skip_cases,"../mnesia_test",mnesia_recovery_test,
            [system_upgrade],
            "Not yet implemented"}.
{skip_cases,"../mnesia_test",mnesia_consistency_test,
            [consistency_after_change_table_copy_type],
            "Not yet implemented"}.
{skip_cases,"../mnesia_test",mnesia_consistency_test,
            [consistency_after_transform_table_ram,
             consistency_after_transform_table_disc,
             consistency_after_transform_table_disc_only],
            "Not yet implemented"}.
{skip_cases,"../mnesia_test",mnesia_consistency_test,
            [consistency_after_rename_of_node],
            "Not yet implemented"}.
