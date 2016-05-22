%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

-module(verify_data_dir_cth).

-include_lib("common_test/src/ct_util.hrl").

%% CT Hooks
-compile(export_all).

-define(val(K, L), proplists:get_value(K, L)).

check_dirs(State,Config) ->
    DataDirName = ?val(data_dir_name, State),
    %% check priv_dir
    PrivDir = proplists:get_value(priv_dir, Config),
    "log_private" = filename:basename(PrivDir),
    {ok,_} = file:list_dir(PrivDir),
    
    %% check data_dir
    DataDir = proplists:get_value(data_dir, Config),
    DataDirName = filename:basename(DataDir),
    ok.

id(_Opts) ->
    ?MODULE.

init(Id, _Opts) ->
    {ok, _State} = empty_cth:init(Id, []),
    {ok, [{data_dir_name,"ct_data_dir_SUITE_data"}]}.

pre_init_per_suite(Suite,Config,State) ->
    check_dirs(State,Config),
    empty_cth:pre_init_per_suite(Suite,Config,State).

post_init_per_suite(Suite,Config,Return,State) ->
    check_dirs(State,Return),
    empty_cth:post_init_per_suite(Suite,Config,Return,State).

pre_end_per_suite(Suite,Config,State) ->
    check_dirs(State,Config),
    empty_cth:pre_end_per_suite(Suite,Config,State).

post_end_per_suite(Suite,Config,Return,State) ->
    check_dirs(State,Config),
    empty_cth:post_end_per_suite(Suite,Config,Return,State).

pre_init_per_group(Group,Config,State) ->
    check_dirs(State,Config),
    empty_cth:pre_init_per_group(Group,Config,State).

post_init_per_group(Group,Config,Return,State) ->
    check_dirs(State,Return),
    empty_cth:post_init_per_group(Group,Config,Return,State).

pre_end_per_group(Group,Config,State) ->
    check_dirs(State,Config),
    empty_cth:pre_end_per_group(Group,Config,State).

post_end_per_group(Group,Config,Return,State) ->
    check_dirs(State,Config),
    empty_cth:post_end_per_group(Group,Config,Return,State).

pre_init_per_testcase(TC,Config,State) ->
    check_dirs(State,Config),
    empty_cth:pre_init_per_testcase(TC,Config,State).

post_init_per_testcase(TC,Config,Return,State) ->
    check_dirs(State,Config),
    empty_cth:post_init_per_testcase(TC,Config,Return,State).

pre_end_per_testcase(TC,Config,State) ->
    check_dirs(State,Config),
    empty_cth:pre_end_per_testcase(TC,Config,State).

post_end_per_testcase(TC,Config,Return,State) ->
    check_dirs(State,Config),
    empty_cth:post_end_per_testcase(TC,Config,Return,State).

on_tc_fail(TC, Reason, State) ->
    empty_cth:on_tc_fail(TC,Reason,State).

on_tc_skip(TC, Reason, State) ->
    empty_cth:on_tc_skip(TC,Reason,State).

terminate(State) ->
    empty_cth:terminate(State).
