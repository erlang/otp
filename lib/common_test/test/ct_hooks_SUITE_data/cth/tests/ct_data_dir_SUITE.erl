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

-module(ct_data_dir_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").

-define(data_dir_name, atom_to_list(?MODULE)++"_data").

suite() ->
    [{timetrap, {seconds,1}},
     {ct_hooks, [verify_data_dir_cth]}].

all() ->
    [test_case_1, {group,test_group}].

groups() ->
    [{test_group,[],[test_case_2]}].

init_per_testcase(_, Config) ->
    check_dirs(Config),
    Config.

end_per_testcase(_, Config) ->
    check_dirs(Config),
    ok.

test_case_1(Config) ->
    check_dirs(Config),
    ok.

test_case_2(Config) ->
    check_dirs(Config),
    ok.

check_dirs(Config) ->
    %% check priv_dir
    PrivDir = proplists:get_value(priv_dir, Config),
    "log_private" = filename:basename(PrivDir),
    {ok,_} = file:list_dir(PrivDir),
    
    %% check data_dir
    DataDir = proplists:get_value(data_dir, Config),
    DataDirName = ?data_dir_name,
    DataDirName = filename:basename(DataDir),
    ok.
    

