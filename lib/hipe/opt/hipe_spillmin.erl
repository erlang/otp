%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
%% ==========================================================================
%%  Module  :  hipe_spillmin
%%  Purpose :  Driver module for minimizing the number of stack slots used
%%	       by a function. This is done using an algorithm for register
%%             allocation. The implementation is target-independent and
%%             requires a target-specific interface module as argument.  
%% 
%% $Id$
%% ==========================================================================
%% Exported functions (short description):
%%
%%  stackalloc(CFG, StackSlots, SpillIndex, Options, Target, TempMap) -> 
%%      {Coloring, NumberOfSpills}
%%    Takes a CFG and the TempMap from register allocation and returns 
%%    a coloring of stack slots.  
%%    StackSlots should be a list of used stack slots, usually empty at
%%    first call to function.
%%    SpillIndex is the the first position we will spill to, usually 0.
%%    TempMap is the TempMap from the register allocation
%%
%%    The Coloring will be in the form of the "allocation datastructure"
%%    described below, that is, a list of tuples on the form
%%      {Name, {spill, SpillIndex}}
%%    The NumberOfSpills is either 0 indicating no spill or the 
%%    SpillIndex of the last spilled register.
%%
%%  mapmerge(Map, SpillMap) -> NewMap
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_spillmin).
-export([stackalloc/6, mapmerge/2]).

%%-define(DEBUG, 1).
-define(HIPE_INSTRUMENT_COMPILER, true).

%%---------------------------------------------------------------------------

-include("../main/hipe.hrl").
-include("../flow/cfg.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% stackalloc(CFG, StackSlots, SpillIndex, Options, Target, TempMap) 
%%   Calculates an allocation of stack slots using either a linear scan
%%   or a graph coloring allocation algorithm.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec stackalloc(#cfg{}, [_], non_neg_integer(),
		 comp_options(), module(), hipe_temp_map()) ->
                                {hipe_spill_map(), non_neg_integer()}.
 
stackalloc(CFG, StackSlots, SpillIndex, Options, Target, TempMap) ->
  case proplists:get_bool(spillmin_color, Options) of
    false ->
      ?option_time(hipe_spillmin_scan:stackalloc(CFG, StackSlots, SpillIndex,
						 Options, Target, TempMap),
		   "Spill minimize, linear scan", Options);
    true ->
      ?option_time(hipe_spillmin_color:stackalloc(CFG, StackSlots, SpillIndex,
						  Options, Target, TempMap),
		   "Spill minimize, graph coloring", Options)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% mapmerge(Map, SpillMap)
%%
%% stackalloc/6 will only return the subset of the tempmap that contains 
%% the spilled temporaries. This function is used to merge the old 
%% complete tempmap with the new spill information.
%% Map is the old map (a list of [{R0, C1}, {R1, C2}, ...]).
%% SpillMap is the new "spill" map.
%% !! Warning, the function does not work with the maps in another order !! 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Combines the map with allocated spills with a map from the register
%% allocator

-spec mapmerge(hipe_map(), hipe_spill_map()) -> hipe_map().

mapmerge(TempMap, SpillMap) ->
  mapmerge(TempMap, SpillMap, []).

mapmerge([], _, Ack) ->
  lists:reverse(Ack);
mapmerge([{T1, _}|T1s], [{T2, C}|T2s], Ack) when T1 =:= T2 -> 
  mapmerge(T1s, T2s, [{T1, C}|Ack]);
mapmerge([{_, unknown}|T1s], T2s, Ack) ->
  mapmerge(T1s, T2s, Ack);
mapmerge([T1|T1s], T2s, Ack) -> 
  mapmerge(T1s, T2s, [T1|Ack]).
