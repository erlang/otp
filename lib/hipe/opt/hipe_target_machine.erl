%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%		  INTERFACE TO TARGET MACHINE MODEL
%%
%% Interfaces the instruction scheduler to the (resource) machine model.

-module(hipe_target_machine).
-export([init_resources/1,
	 init_instr_resources/2,
	 resources_available/4,
	 advance_cycle/1
	]).
-export([raw_latency/2,
	 war_latency/2,
	 waw_latency/2,
	 %% m_raw_latency/2,
	 %% m_war_latency/2,
	 %% m_waw_latency/2,
	 m_raw_latency/0,
	 m_war_latency/0,
	 m_waw_latency/0,
	 br_to_unsafe_latency/2,
	 unsafe_to_br_latency/2,
	 br_br_latency/2
	]).

-define(target,hipe_ultra_mod2).

init_resources(X) ->
    ?target:init_resources(X).

init_instr_resources(X,Y) ->
    ?target:init_instr_resources(X,Y).

resources_available(X,Y,Z,W) ->
    ?target:resources_available(X,Y,Z,W).

advance_cycle(X) ->
    ?target:advance_cycle(X).

raw_latency(From,To) ->
    ?target:raw_latency(From,To).

war_latency(From,To) ->
    ?target:war_latency(From,To).

waw_latency(From,To) ->
    ?target:waw_latency(From,To).

%% m_raw_latency(From,To) ->
%%     ?target:m_raw_latency(From,To).

%% m_war_latency(From,To) ->
%%     ?target:m_war_latency(From,To).

%% m_waw_latency(From,To) ->
%%     ?target:m_waw_latency(From,To).

m_raw_latency() ->
    ?target:m_raw_latency().

m_war_latency() ->
    ?target:m_war_latency().

m_waw_latency() ->
    ?target:m_waw_latency().

br_to_unsafe_latency(Br,U) ->
    ?target:br_to_unsafe_latency(Br,U).

unsafe_to_br_latency(U,Br) ->
    ?target:unsafe_to_br_latency(U,Br).

br_br_latency(Br1,Br2) ->
    ?target:br_br_latency(Br1,Br2).
