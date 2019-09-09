%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2019. All Rights Reserved.
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

%%
%%----------------------------------------------------------------------
%% Purpose: Test application config
%%----------------------------------------------------------------------

-module(megaco_SUITE).

-export([
         suite/0,
         all/0,
         groups/0,

         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,

         t/0, t/1,
         init/0
        ]).

-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).

%% Test server callbacks
init_per_testcase(Case, Config) ->
    megaco_test_lib:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->
    megaco_test_lib:end_per_testcase(Case, Config).

init() ->
    process_flag(trap_exit, true),
    megaco_test_lib:flush().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

suite() -> [{ct_hooks, [{ts_install_cth, [{nodenames,1}]}]}].

all() -> 
    [{group, app_test}, 
     {group, appup_test},
     {group, config}, 
     {group, flex}, 
     {group, udp},
     {group, tcp}, 
     {group, examples}, 
     {group, digit_map},
     {group, mess}, 
     {group, measure},
     {group, binary_term_id}, 
     {group, codec}, 
     {group, sdp},
     {group, mib}, 
     {group, trans}, 
     {group, actions},
     {group, load}, 
     {group, pending_limit},
     {group, segmented}, 
     {group, timer}].

groups() -> 
    [{tickets,        [], [{group, mess}, {group, codec}]},
     {app_test,       [], [{megaco_app_test,            all}]},
     {appup_test,     [], [{megaco_appup_test,          all}]},
     {config,         [], [{megaco_config_test,         all}]},
     {call_flow,      [], [{megaco_call_flow_test,      all}]},
     {digit_map,      [], [{megaco_digit_map_test,      all}]},
     {mess,           [], [{megaco_mess_test,           all}]},
     {udp,            [], [{megaco_udp_test,            all}]},
     {tcp,            [], [{megaco_tcp_test,            all}]},
     {examples,       [], [{megaco_examples_test,       all}]},
     {measure,        [], [{megaco_measure_test,        all}]},
     {binary_term_id, [], [{megaco_binary_term_id_test, all}]},
     {codec,          [], [{megaco_codec_test,          all}]},
     {sdp,            [], [{megaco_sdp_test,            all}]},
     {mib,            [], [{megaco_mib_test,            all}]},
     {trans,          [], [{megaco_trans_test,          all}]},
     {actions,        [], [{megaco_actions_test,        all}]},
     {load,           [], [{megaco_load_test,           all}]},
     {pending_limit,  [], [{megaco_pending_limit_test,  all}]},
     {segmented,      [], [{megaco_segment_test,        all}]},
     {timer,          [], [{megaco_timer_test,          all}]},
     {flex,           [], [{megaco_flex_test,           all}]}].

init_per_suite(Config) ->
    io:format("~w:init_per_suite -> entry with"
	      "~n   Config:     ~p"
              "~n   OS Type:    ~p"
              "~n   OS Version: ~s"
	      "~n", 
              [?MODULE, 
               Config, 
               os:type(), 
               case os:version() of
                   {Major, Minor, Release} ->
                       ?F("~w.~w.~w", [Major, Minor, Release]);
                   Str when is_list(Str) ->
                       Str
               end]),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Skippable = [{unix, [{darwin, fun(V) when (V > {9, 8, 0}) ->
                                          %% This version is OK: No Skip
                                          false;
                                     (_V) ->
                                          %% This version is *not* ok: Skip
                                          true
                                  end}]}],
    case ?OS_BASED_SKIP(Skippable) of
        true ->
            {skip, "***OLD*** Darwin"};
        false ->
            Config
    end.

end_per_group(_GroupName, Config) ->
    Config.
























