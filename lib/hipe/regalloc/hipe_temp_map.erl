%% -*- erlang-indent-level: 2 -*-
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
%% ===========================================================================
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% ===========================================================================
%%  Module   :	hipe_temp_map
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-07-24 Erik Johansson (happi@it.uu.se): Created.
%% ===========================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_temp_map).

-export([cols2tuple/3, find/2, is_spilled/2, to_substlist/1]).

-include("../main/hipe.hrl").

-type target_context() :: any().

%%----------------------------------------------------------------------------
%% Convert a list of [{R0, C1}, {R1, C2}, ...] to a temp_map
%% (Currently implemented as a tuple) tuple {C1, C2, ...}.
%%
%% The indices (Ri) must be unique but do not have to be sorted and 
%% they can be sparse.
%% Note that the first allowed index is 0 -- this will be mapped to 
%% element 1
%%----------------------------------------------------------------------------

-spec cols2tuple(hipe_map(), module(), target_context()) -> hipe_temp_map().

cols2tuple(Map, TgtMod, TgtCtx) ->
  SortedMap = lists:keysort(1, Map),
  cols2tuple(0, SortedMap, [], TgtMod, TgtCtx).

%% sorted_cols2tuple(Map, TgtMod, TgtCtx) ->
%%   ?ASSERT(Map =:= lists:keysort(1, Map)),
%%   cols2tuple(0, Map, [], TgtMod, TgtCtx). 

%% Build a dense mapping 
cols2tuple(_, [], Vs, _, _) ->
  %% Done reverse the list and convert to tuple.
  list_to_tuple(lists:reverse(Vs));
cols2tuple(N, [{R, C}|Ms], Vs, TgtMod, TgtCtx) when N =:= R ->
  %% N makes sure the mapping is dense. N is he next key.
  cols2tuple(N+1, Ms, [C|Vs], TgtMod, TgtCtx);
cols2tuple(N, SourceMapping=[{R,_}|_], Vs, TgtMod, TgtCtx) when N < R ->
  %% The source was sparse, make up some placeholders...
  Val = 	      
    case TgtMod:is_precoloured(N, TgtCtx) of
      %% If it is precoloured, we know what to map it to.
      true -> {reg, N};
      false -> unknown
    end,
  cols2tuple(N+1, SourceMapping, [Val|Vs], TgtMod, TgtCtx).

%%
%% True if temp Temp is spilled.
%%
-spec is_spilled(non_neg_integer(), hipe_temp_map()) -> boolean().

is_spilled(Temp, Map) ->
  case find(Temp, Map) of
    {reg, _R} -> false;
    {fp_reg, _R}-> false;
    {spill, _N} -> true;
    unknown -> false
  end.
    
%% %% True if temp Temp is allocated to a reg.
%% in_reg(Temp, Map) ->
%%   case element(Temp+1, Map) of
%%     {reg, _R} -> true;
%%     {fp_reg, _R}-> false;
%%     {spill, _N} -> false;
%%     unknown -> false
%%   end.
%%
%% %% True if temp Temp is allocated to a fp_reg.
%% in_fp_reg(Temp, Map) ->
%%   case element(Temp+1, Map) of
%%     {fp_reg, _R} -> true;
%%     {reg, _R} -> false;
%%     {spill, _N} -> false;
%%     unknown -> false
%%   end.

%% Returns the inf temp Temp is mapped to.
find(Temp, Map) when Temp < tuple_size(Map) -> element(Temp+1, Map);
find(_,    Map) when is_tuple(Map) -> unknown. % consistency with cols2tuple/3


%%
%% Converts a temp_map tuple back to a (sorted) key-list.
%%
-spec to_substlist(hipe_temp_map()) -> hipe_map().

to_substlist(Map) ->
  T = tuple_to_list(Map),
  mapping(T, 0).

mapping([R|Rs], Temp) ->
  [{Temp, R}| mapping(Rs, Temp+1)];
mapping([], _) ->
  [].
