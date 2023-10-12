%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2020. All Rights Reserved.
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


-module(update_result_post_end_tc_cth).


-include_lib("common_test/src/ct_util.hrl").
-include_lib("common_test/include/ct_event.hrl").


%% CT Hooks
-compile(export_all).

init(Id, Opts) ->
    empty_cth:init(Id, Opts).

pre_init_per_suite(Suite, Config, State) ->
    empty_cth:pre_init_per_suite(Suite,Config,State).

post_init_per_suite(Suite,Config,Return,State) ->
    empty_cth:post_init_per_suite(Suite,Config,Return,State).

pre_end_per_suite(Suite,Config,State) ->
    empty_cth:pre_end_per_suite(Suite,Config,State).

post_end_per_suite(Suite,Config,Return,State) ->
    empty_cth:post_end_per_suite(Suite,Config,Return,State).

pre_init_per_group(Suite,Group,Config,State) ->
    empty_cth:pre_init_per_group(Suite,Group,Config,State).

post_init_per_group(Suite,Group,Config,Return,State) ->
    empty_cth:post_init_per_group(Suite,Group,Config,Return,State).

pre_end_per_group(Suite,Group,Config,State) ->
    empty_cth:pre_end_per_group(Suite,Group,Config,State).

post_end_per_group(Suite,Group,Config,Return,State) ->
    empty_cth:post_end_per_group(Suite,Group,Config,Return,State).

pre_init_per_testcase(Suite,TC,Config,State) ->
    empty_cth:pre_init_per_testcase(Suite,TC,Config,State).

post_end_per_testcase(Suite,TC,Config,Return,State) ->
    empty_cth:post_end_per_testcase(Suite,TC,Config,Return,State),
    change_result(TC,Config,State).

on_tc_fail(Suite,TC, Reason, State) ->
    empty_cth:on_tc_fail(Suite,TC,Reason,State).

on_tc_skip(Suite,TC, Reason, State) ->
    empty_cth:on_tc_skip(Suite,TC,Reason,State).

terminate(State) ->
    empty_cth:terminate(State).

%%%-----------------------------------------------------------------
%%% 
change_result(tc_ok_to_fail,_Config,State) ->
    {{fail, "Test failure"}, State};
change_result(tc_ok_to_skip,_Config,State) ->
    {{skip, "Test skipped"}, State};
change_result(tc_fail_to_ok,Config,State) ->
    {lists:keydelete(tc_status,1,Config),State};
change_result(tc_fail_to_skip,Config,State) ->
    {{skip,"Test skipped"},State};
change_result(tc_timetrap_to_ok,Config,State) ->
    {lists:keydelete(tc_status,1,Config),State};
change_result(tc_timetrap_to_skip,Config,State) ->
    {{skip,"Test skipped"},State};
change_result(tc_skip_to_fail,_Config,State) ->
    {{fail, "Test failure"}, State};
change_result(end_fail_to_fail,_Config,State) ->
    {{fail, "Test failure"}, State};
change_result(end_fail_to_skip,_Config,State) ->
    {{skip, "Test skipped"}, State};
change_result(end_timetrap_to_fail,_Config,State) ->
    {{fail, "Test failure"}, State};
change_result(end_timetrap_to_skip,_Config,State) ->
    {{skip, "Test skipped"}, State}.
