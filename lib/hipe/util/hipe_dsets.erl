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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%@doc
%%	       IMMUTABLE DISJOINT SETS OF ARBITRARY TERMS
%%
%% The disjoint set forests data structure, for elements of arbitrary types.
%% Note that the find operation mutates the set.
%%
%% We could do this more efficiently if we restricted the elements to integers,
%% and used the (mutable) hipe arrays. For arbitrary terms ETS could be used,
%% for a persistent interface (which isn't that nice when even accessors return
%% modified copies), the array module could be used.
-module(hipe_dsets).

-export([new/1, find/2, union/3, to_map/1, to_rllist/1]).
-export_type([dsets/1]).

-opaque dsets(X) :: #{X => {node, X} | {root, non_neg_integer()}}.

-spec new([E]) -> dsets(E).
new(Elems) -> maps:from_list([{E,{root,0}} || E <- Elems]).

-spec find(E, dsets(E)) -> {E, dsets(E)}.
find(E, DS0) ->
  case DS0 of
    #{E := {root,_}} -> {E, DS0};
    #{E := {node,N}} ->
      case find(N, DS0) of
	{N, _}=T -> T;
	{R, DS1} -> {R, DS1#{E := {node,R}}}
      end;
    _ -> error(badarg, [E, DS0])
  end.

-spec union(E, E, dsets(E)) -> dsets(E).
union(X, Y, DS0) ->
  {XRoot, DS1} = find(X, DS0),
  case find(Y, DS1) of
    {XRoot, DS2} -> DS2;
    {YRoot, DS2} ->
      #{XRoot := {root,XRR}, YRoot := {root,YRR}} = DS2,
      if XRR < YRR -> DS2#{XRoot := {node,YRoot}};
	 XRR > YRR -> DS2#{YRoot := {node,XRoot}};
	 true -> DS2#{YRoot := {node,XRoot}, XRoot := {root,XRR+1}}
      end
  end.

-spec to_map(dsets(E)) -> {#{Elem::E => Root::E}, dsets(E)}.
to_map(DS) ->
  to_map(maps:keys(DS), DS, #{}).

to_map([], DS, Acc) -> {Acc, DS};
to_map([K|Ks], DS0, Acc) ->
  {KR, DS} = find(K, DS0),
  to_map(Ks, DS, Acc#{K => KR}).

-spec to_rllist(dsets(E)) -> {[{Root::E, Elems::[E]}], dsets(E)}.
to_rllist(DS0) ->
  {Lists, DS} = to_rllist(maps:keys(DS0), #{}, DS0),
  {maps:to_list(Lists), DS}.

to_rllist([], Acc, DS) -> {Acc, DS};
to_rllist([E|Es], Acc, DS0) ->
  {ERoot, DS} = find(E, DS0),
  to_rllist(Es, map_append(ERoot, E, Acc), DS).

map_append(Key, Elem, Map) ->
  case Map of
    #{Key := List} -> Map#{Key := [Elem|List]};
    #{} -> Map#{Key => [Elem]}
  end.
