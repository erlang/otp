%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(subgroups_1_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, subgroup_return_fail},
     {group, subgroup_init_fail},
     {group, subgroup_after_failed_case},
     {group, case_after_subgroup_return_fail},
     {group, case_after_subgroup_fail_init}].

groups() ->
    [{return_fail, [], [failing_tc]},
     {fail_init, [], [ok_tc]},
     {ok_group, [], [ok_tc]},

     {subgroup_return_fail, [sequence], [{group, return_fail}, {group, ok_group}]},

     {subgroup_init_fail, [sequence], [{group, fail_init}, {group, ok_group}]},

     {subgroup_after_failed_case, [sequence], [failing_tc, {group, ok_group}]},

     {case_after_subgroup_return_fail, [sequence], [{group, return_fail}, ok_tc]},

     {case_after_subgroup_fail_init, [sequence], [{group, fail_init}, ok_tc]}
    ].

failed_subgroup(subgroup_return_fail) -> return_fail;
failed_subgroup(subgroup_init_fail) -> fail_init;
failed_subgroup(case_after_subgroup_return_fail) -> return_fail;
failed_subgroup(case_after_subgroup_fail_init) -> fail_init;
failed_subgroup(_) -> undefined.

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(fail_init, Config) ->
    ct:comment(fail_init),
    exit(init_per_group_fails_on_purpose);

init_per_group(Group, Config) ->
    ct:comment(Group),
    [{Group,failed_subgroup(Group)} | Config].

end_per_group(subgroup_after_failed_case, Config) ->
    ct:comment(subgroup_after_failed_case),
    Status = ?config(tc_group_result, Config),
    [{subgroups_1_SUITE,failing_tc}] = proplists:get_value(failed, Status),
    {return_group_result,failed};

end_per_group(Group, Config) when Group == subgroup_return_fail;
				  Group == subgroup_init_fail;
				  Group == case_after_subgroup_return_fail;
				  Group == case_after_subgroup_fail_init ->
    ct:comment(Group),
    Status = ?config(tc_group_result, Config),
    Failed = proplists:get_value(failed, Status),
    true = lists:member({group_result,?config(Group,Config)}, Failed),
    {return_group_result,failed};

end_per_group(return_fail, Config) ->
    ct:comment(return_fail),
    Status = ?config(tc_group_result, Config),
    [{subgroups_1_SUITE,failing_tc}] = proplists:get_value(failed, Status),
    {return_group_result,failed};

end_per_group(Group, _Config) ->
    ct:comment(Group),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(failing_tc, Config) ->
    {failed,_} = proplists:get_value(tc_status, Config),
    ok;

end_per_testcase(_TestCase, _Config) ->
    ok.

failing_tc(_Config) ->
    2=3.

ok_tc(_Config) ->
    ok.
