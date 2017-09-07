@ECHO OFF
CALL werl -env ERL_CRASH_DUMP_SECONDS 0 -s crashdump_viewer script_start %*
