%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2011. All Rights Reserved.
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

%%
%%----------------------------------------------------------------------
%% Purpose: Test application config
%%----------------------------------------------------------------------

-module(megaco_SUITE).

-compile(export_all).

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
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.
























