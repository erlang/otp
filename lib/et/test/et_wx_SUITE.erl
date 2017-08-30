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

-module(et_wx_SUITE).

-export([all/0, suite/0,
         init_per_testcase/2, end_per_testcase/2,
         init_per_suite/1, end_per_suite/1]).
-export([start_all_windows/1]).

-include("et_test_lib.hrl").

%% Initialization functions.
init_per_suite(Config) ->
    et_test_lib:wx_init_per_suite(Config).

end_per_suite(Config) ->
    et_test_lib:wx_end_per_suite(Config).

init_per_testcase(Func,Config) ->
    et_test_lib:init_per_testcase(Func,Config).
end_per_testcase(Func,Config) -> 
    et_test_lib:end_per_testcase(Func,Config).

%% SUITE specification

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() -> 
    [start_all_windows].

%% The test cases

%% Display all windows and see if something crashes
start_all_windows(TestInfo) when is_atom(TestInfo) ->
    et_test_lib:tc_info(TestInfo);
start_all_windows(_Config) ->
    process_flag(trap_exit, true),
    {ok, ViewerPid} = ?msym({ok, _}, et_viewer:start_link([])),
    CollectorPid = et_viewer:get_collector_pid(ViewerPid),      
    ?msym({ok, _}, et_collector:report_event(CollectorPid,
					     60,
					     some_from_actor,
					     some_to_actor,
					     some_label, 
					     "Some details")),
    timer:sleep(timer:seconds(1)),

    {ok, EventPid1} = ?msym({ok, _}, et_viewer:open_event(ViewerPid, 1)),
    {ok, EventPid2} = ?msym({ok, _}, et_viewer:open_event(ViewerPid, 1)),
    timer:sleep(timer:seconds(10)),

    ?msym(alive, process_state(ViewerPid)),
    ?msym(alive, process_state(CollectorPid)),
    ?msym(alive, process_state(EventPid1)),
    ?msym(alive, process_state(EventPid2)),

    ?m(ok, et_wx_contents_viewer:stop(EventPid1)),
    timer:sleep(timer:seconds(1)),

    ?msym(alive, process_state(ViewerPid)),
    ?msym(alive, process_state(CollectorPid)),
    ?msym(dead,  process_state(EventPid1)),
    ?msym(alive, process_state(EventPid2)),

    ?m(ok, et_viewer:stop(ViewerPid)),
    timer:sleep(timer:seconds(1)),

    ?msym(dead, process_state(ViewerPid)),
    ?msym(dead, process_state(CollectorPid)),
    ?msym(dead, process_state(EventPid1)),
    ?msym(dead, process_state(EventPid2)),

    ?m([], et_test_lib:flush()),

    ok.

process_state(Pid) ->
    case process_info(Pid, group_leader) of
	{group_leader, _} -> alive;
	undefined         -> dead
    end.
