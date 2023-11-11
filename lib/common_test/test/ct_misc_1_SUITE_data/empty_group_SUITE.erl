-module(empty_group_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

all() -> [{group, one_testcase}, {group, zero_testcases}].

groups() -> [{one_testcase, [t1]}, {zero_testcases, []}].

t1(_Config) ->
  ok.
