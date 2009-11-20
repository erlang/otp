%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
%%
-module(erl_internal_SUITE).

-export([all/1]).

-export([behav/1]).

-export([init_per_testcase/2, fin_per_testcase/2]).

-include("test_server.hrl").

all(suite) -> [behav].

-define(default_timeout, ?t:minutes(2)).

init_per_testcase(_Case, Config) ->
    ?line Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

fin_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

behav(suite) -> [];
behav(doc) ->
    ["Check that the behaviour callbacks are correctly defined"];
behav(_) ->
    ?line check_behav_list([{start,2}, {stop,1}], 
			   application:behaviour_info(callbacks)),
    ?line check_behav_list([{init,1}, {handle_call,3}, {handle_cast,2},
			    {handle_info,2}, {terminate,2}, {code_change,3}],
			   gen_server:behaviour_info(callbacks)),
    ?line check_behav_list([{init,1}, {handle_event,3}, {handle_sync_event,4},
			    {handle_info,3}, {terminate,3}, {code_change,4}],
			   gen_fsm:behaviour_info(callbacks)),
    ?line check_behav_list([{init,1}, {handle_event,2}, {handle_call,2},
			    {handle_info,2}, {terminate,2}, {code_change,3}],
			   gen_event:behaviour_info(callbacks)),
    ?line check_behav_list( [{init,1}, {terminate,2}], 
			   supervisor_bridge:behaviour_info(callbacks)),
    ?line check_behav_list([{init,1}],
			   supervisor:behaviour_info(callbacks)),
    ok.

check_behav_list([], []) -> ok;
check_behav_list([L | L1], L2) ->
    ?line true = lists:member(L, L2),
    ?line L3 = lists:delete(L, L2),
    check_behav_list(L1, L3).


