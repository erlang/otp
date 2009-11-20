%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2009. All Rights Reserved.
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

fin_per_testcase(Case, Config) ->
    megaco_test_lib:fin_per_testcase(Case, Config).

init() ->
    process_flag(trap_exit, true),
    megaco_test_lib:flush().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

all(suite) ->
    [
     app_test,
     appup_test,
     config,
     flex,
     udp,
     tcp,
     examples,
     %% call_flow,
     digit_map,
     mess,
     measure,
     binary_term_id,
     codec,
     sdp,
     mib,
     trans,
     actions,
     load,
     pending_limit,
     segmented,
     timer
    ].

tickets(suite) ->
    [
     mess,
     codec
     ].

app_test(suite) ->
    [{megaco_app_test, all}].

appup_test(suite) ->
    [{megaco_appup_test, all}].

config(suite) ->
    [{megaco_config_test, all}].

call_flow(suite) ->
    [{megaco_call_flow_test, all}].

digit_map(suite) ->
    [{megaco_digit_map_test, all}].

mess(suite) ->
    [{megaco_mess_test, all}].

udp(suite) ->
    [{megaco_udp_test, all}].

tcp(suite) ->
    [{megaco_tcp_test, all}].

examples(suite) ->
    [{megaco_examples_test, all}].

measure(suite) ->
    [{megaco_measure_test, all}].

binary_term_id(suite) ->
    [{megaco_binary_term_id_test, all}].

codec(suite) ->
    [{megaco_codec_test, all}].

sdp(suite) ->
    [{megaco_sdp_test, all}].

mib(suite) ->
    [{megaco_mib_test, all}].

trans(suite) ->
    [{megaco_trans_test, all}].

actions(suite) ->
    [{megaco_actions_test, all}].

load(suite) ->
    [{megaco_load_test, all}].

pending_limit(suite) ->
    [{megaco_pending_limit_test, all}].

segmented(suite) ->
    [{megaco_segment_test, all}].

timer(suite) ->
    [{megaco_timer_test, all}].

flex(suite) ->
    [{megaco_flex_test, all}].

