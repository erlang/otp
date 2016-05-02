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
%%		       ULTRASPARC MACHINE MODEL
%%
%% This module is used by the scheduler.
%% The following interface is used:
%%   ...
%%
%% NOTES:
%% - the machine model is simple (on the verge of simplistic)
%%   * all FUs are pipelined => model only one cycle at a time
%%   * instruction latencies are mostly 1
%%   * floating point is left for later (I _think_ it works, but ...)
%% - conservative: instructions that require multiple resources are
%%   modelled as 'single'; instead, they could reserve IEU+BR or whatever
%% - possibly inefficient: I think machine state model could be turned into
%%   a bitvector.

-module(hipe_ultra_mod2).
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

-include("../sparc/hipe_sparc.hrl").

-define(debug(Str,Args),ok).
%-define(debug(Str,Args),io:format(Str,Args)).

-define(debug_ultra(Str,Args),ok).
%-define(debug_ultra(Str,Args),io:format(Str,Args)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Straightforward and somewhat simplistic model for UltraSparc:
%% - only one cycle at a time is modelled
%% - resources are simplified:
%%   * ieu0, ieu1, ieu, mem, br, single
%%   * per-cycle state = done | { I0, I1, NumI, X, Mem, Br }
%%   * unoptimized representation (could be bit vector)

init_resources(_Size) ->
    ?debug_ultra('init res ~p~n',[_Size]),
    empty_state().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_instr_resources(N,Nodes) ->
    ultra_instr_rsrcs(Nodes,hipe_vectors:new(N, '')).

ultra_instr_rsrcs([],I_res) -> I_res;
ultra_instr_rsrcs([N|Ns],I_res) ->
    ultra_instr_rsrcs(Ns,ultra_instr_type(N,I_res)).

ultra_instr_type({N,I},I_res) ->
    hipe_vectors:set(I_res,N-1,instr_type(I)).

instr_type(I) ->
    case I of
	#move{} ->
	    ieu;
	#multimove{} -> %% TODO: expand multimoves before scheduling
	    ieu;
	#alu{} ->
	    case hipe_sparc:alu_operator(I) of
		'>>' -> ieu0;
		'<<' -> ieu0;
		_ -> ieu
	    end;
	#alu_cc{} ->
	    ieu1;
	#sethi{} ->
	    ieu;
	#load{} ->
	    mem;
	#store{} ->
	    mem;
	#b{} ->
	    br;
	#br{} ->
	    br;
	#goto{} ->
	    br;
	#jmp_link{} ->         % imprecise; should be mem+br?
	    single;
	#jmp{} ->              % imprecise
	    br;
	#call_link{} ->        % imprecise; should be mem+br?
	    single;
	#cmov_cc{} ->          % imprecise
	    single;
	#cmov_r{} ->           % imprecise
	    single;
	#load_atom{} ->        % should be resolved to sethi/or
	    single;
	#load_address{} ->     % should be resolved to sethi/or
	    single;
	#load_word_index{} ->  % should be resolved to sethi/or
	    single;
	%% uncommon types:
	#label{} ->
	    none;
	#nop{} ->
	    none;
	#comment{} ->
	    none;
	_ ->
	    exit({ultrasparc_instr_type,{cant_schedule,I}})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resources_available(_Cycle, I, Rsrc, I_res) ->
    res_avail(instruction_resource(I_res, I), Rsrc).

instruction_resource(I_res, I) ->
    hipe_vectors:get(I_res, I-1).

%% The following function checks resource availability.
%% * all function units are assumed to be fully pipelined, so only
%%   one cycle at a time is modelled.
%% * for IEU0 and IEU1, these must precede all generic IEU instructions
%%   (handled by X bit)
%% * at most 2 integer instructions can issue in a cycle
%% * mem is straightforward
%% * br closes the cycle (= returns done).
%% * single requires an entirely empty state and closes the cycle

res_avail(ieu0, { free, I1, NumI, free, Mem, Br })
  when is_integer(NumI), NumI < 2 ->
    { yes, { occ, I1, NumI+1, free, Mem, Br }};
res_avail(ieu1, { _I0, free, NumI, free, Mem, Br })
  when is_integer(NumI), NumI < 2 ->
    { yes, { free, occ, NumI+1, free, Mem, Br }};
res_avail(ieu, { I0, I1, NumI, _X, Mem, Br })
  when is_integer(NumI), NumI < 2 ->
    { yes, { I0, I1, NumI+1, occ, Mem, Br }};
res_avail(mem, { I0, I1, NumI, X, free, Br }) ->
    { yes, { I0, I1, NumI, X, occ, Br }};
res_avail(br, { _I0, _I1, _NumI, _X, _Mem, free }) ->
    { yes, done };
res_avail(single, { free, free, 0, free, free, free }) ->
    { yes, done };
res_avail(_, _) ->
    no.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

advance_cycle(_Rsrc) ->
    empty_state().

empty_state() -> { free, free, 0, free, free, free }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Latencies are taken from UltraSparc hardware manual
%%
%% *** UNFINISHED ***
%% more precisely, they are taken from my memory of the US-manual
%% at the moment.
%%
%% Note: all ld/st are assumed to hit in the L1 cache (D-cache),
%%   which is sort of imprecise.

raw_latency(alu, store) -> 0;
raw_latency(load, _) -> 2;            % only if load is L1 hit
raw_latency(alu_cc, b) -> 0;
raw_latency(_I0, _I1) ->
    1.

war_latency(_I0, _I1) ->
    0.

waw_latency(_I0, _I1) ->
    1.

%% *** UNFINISHED ***
%% At present, all load/stores are assumed to hit in the L1 cache,
%% which isn't really satisfying.

%% m_raw_latency(_St, _Ld) ->
%%     1.
%% 
%% m_war_latency(_Ld, _St) ->
%%     1.
%% 
%% m_waw_latency(_St1, _St2) ->
%%     1.

%% Use these for 'default latencies' = do not permit reordering.

m_raw_latency() ->
    1.

m_war_latency() ->
    1.

m_waw_latency() ->
    1.

br_to_unsafe_latency(_BrTy, _UTy) ->
    0.

unsafe_to_br_latency(_UTy, _BrTy) ->
    0.

br_br_latency(_BrTy1, _BrTy2) ->
    0.
