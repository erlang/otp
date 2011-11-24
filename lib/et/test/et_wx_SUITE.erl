%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%

-module(et_wx_SUITE).

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, 
	 init_per_suite/1, end_per_suite/1, 
	 init_per_testcase/2, end_per_testcase/2]).

-compile(export_all).

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
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [start_all_windows].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


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
