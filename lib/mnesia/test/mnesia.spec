{topcase, {dir, "../mnesia_test"}}.
{require_nodenames, 2}.
{skip, {mnesia_measure_test, ram_meter, "Takes to long time"}}.
{skip, {mnesia_measure_test, disc_meter, "Takes to long time"}}.
{skip, {mnesia_measure_test, disc_only_meter, "Takes to long time"}}.
{skip, {mnesia_measure_test, cost, "Takes to long time"}}.
{skip, {mnesia_measure_test, dbn_meters, "Takes to long time"}}.
{skip, {mnesia_measure_test, tpcb, "Takes to long time"}}.
{skip, {mnesia_measure_test, prediction, "Not yet implemented"}}.
{skip, {mnesia_measure_test, consumption, "Not yet implemented"}}.
{skip, {mnesia_measure_test, scalability, "Not yet implemented"}}.
{skip, {mnesia_measure_test, tpcb, "Takes too much time and memory"}}.
{skip, {mnesia_measure_test, measure_all_api_functions, "Not yet implemented"}}.
{skip, {mnesia_measure_test, mnemosyne_vs_mnesia_kernel, "Not yet implemented"}}.
{skip, {mnesia_examples_test, company, "Not yet implemented"}}.
{skip, {mnesia_config_test, ignore_fallback_at_startup, "Not yet implemented"}}.
{skip, {mnesia_evil_backup, local_backup_checkpoint, "Not yet implemented"}}.
{skip, {mnesia_config_test, max_wait_for_decision, "Not yet implemented"}}.
{skip, {mnesia_recovery_test, after_full_disc_partition, "Not yet implemented"}}.
{skip, {mnesia_recovery_test, system_upgrade, "Not yet implemented"}}.
{skip, {mnesia_consistency_test, consistency_after_change_table_copy_type, "Not yet implemented"}}.
{skip, {mnesia_consistency_test, consistency_after_transform_table, "Not yet implemented"}}.
{skip, {mnesia_consistency_test, consistency_after_rename_of_node, "Not yet implemented"}}.
