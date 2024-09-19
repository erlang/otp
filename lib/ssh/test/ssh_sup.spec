{suites,"../ssh_test",
 [ssh_sup_SUITE, ssh_connection_SUITE, ssh_to_openssh_SUITE]}.
{event_handler, {cte_track, []}}.
{enable_builtin_hooks, false}.
{ct_hooks, [{cth_log_redirect, [{mode, replace}]}]}.
