%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
-module(erl_internal_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([behav/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,2}}].

all() -> 
    [behav].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%% Check that the behaviour callbacks are correctly defined.
behav(_) ->
    Modules = [application, gen_server, gen_fsm, gen_event,
               supervisor_bridge, supervisor],
    lists:foreach(fun check_behav/1, Modules).

check_behav(Module) ->
    Callbacks = callbacks(Module),
    Optional = optional_callbacks(Module),
    check_behav_list(Callbacks, Module:behaviour_info(callbacks)),
    check_behav_list(Optional, Module:behaviour_info(optional_callbacks)).

check_behav_list([], []) -> ok;
check_behav_list([L | L1], L2) ->
    true = lists:member(L, L2),
    L3 = lists:delete(L, L2),
    check_behav_list(L1, L3).

callbacks(application) ->
    [{start,2}, {stop,1}];
callbacks(gen_server) ->
    [{init,1}, {handle_call,3}, {handle_cast,2},
     {handle_info,2}, {terminate,2}, {code_change,3},
     {format_status,2}];
callbacks(gen_fsm) ->
    [{init,1}, {handle_event,3}, {handle_sync_event,4},
     {handle_info,3}, {terminate,3}, {code_change,4},
     {format_status,2}];
callbacks(gen_event) ->
    [{init,1}, {handle_event,2}, {handle_call,2},
     {handle_info,2}, {terminate,2}, {code_change,3},
     {format_status,2}];
callbacks(supervisor_bridge) ->
    [{init,1}, {terminate,2}];
callbacks(supervisor) ->
    [{init,1}].

optional_callbacks(application) ->
    [];
optional_callbacks(gen_server) ->
    [{format_status,2}];
optional_callbacks(gen_fsm) ->
    [{format_status,2}];
optional_callbacks(gen_event) ->
    [{format_status,2}];
optional_callbacks(supervisor_bridge) ->
    [];
optional_callbacks(supervisor) ->
    [].
