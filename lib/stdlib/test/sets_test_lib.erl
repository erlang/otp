%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2015. All Rights Reserved.
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

-module(sets_test_lib).

-export([new/2]).

new(Mod, Eq) ->
    fun	(add_element, {El,S}) -> add_element(Mod, El, S);
	(del_element, {El,S}) -> del_element(Mod, El, S);
	(empty, []) -> Mod:new();
	(equal, {S1,S2}) -> Eq(S1, S2);
	(filter, {F,S}) -> filter(Mod, F, S);
	(fold, {F,A,S}) -> fold(Mod, F, A, S);
	(from_list, L) -> Mod:from_list(L);
	(intersection, {S1,S2}) -> intersection(Mod, Eq, S1, S2);
	(intersection, Ss) -> intersection(Mod, Eq, Ss);
	(is_empty, S) -> Mod:is_empty(S);
	(is_set, S) -> Mod:is_set(S);
	(is_subset, {S,Set}) -> is_subset(Mod, Eq, S, Set);
        (iterator, S) -> Mod:iterator(S);
        (iterator_from, {Start, S}) -> Mod:iterator_from(Start, S);
	(module, []) -> Mod;
        (next, I) -> Mod:next(I);
	(singleton, E) -> singleton(Mod, E);
	(size, S) -> Mod:size(S);
	(subtract, {S1,S2}) -> subtract(Mod, S1, S2);
	(to_list, S) -> Mod:to_list(S);
	(union, {S1,S2}) -> union(Mod, Eq, S1, S2);
	(union, Ss) -> union(Mod, Eq, Ss)
    end.

singleton(Mod, E) ->
    case erlang:function_exported(Mod, singleton, 1) of
	true -> Mod:singleton(E);
	false -> Mod:from_list([E])
    end.

add_element(Mod, El, S0) ->
    S = Mod:add_element(El, S0),
    true = Mod:is_element(El, S),
    false = Mod:is_empty(S),
    true = Mod:is_set(S),
    S.

del_element(Mod, El, S0) ->
    S = Mod:del_element(El, S0),
    false = Mod:is_element(El, S),
    true = Mod:is_set(S),
    S.

intersection(Mod, Equal, S1, S2) ->
    S = Mod:intersection(S1, S2),
    true = Equal(S, Mod:intersection(S2, S1)),
    Disjoint = Mod:is_empty(S),
    Disjoint = Mod:is_disjoint(S1, S2),
    Disjoint = Mod:is_disjoint(S2, S1),
    S.

intersection(Mod, Equal, Ss) ->
    S = Mod:intersection(Ss),
    true = Equal(S, Mod:intersection(lists:reverse(Ss))),
    S.

subtract(Mod, S1, S2) ->
    S = Mod:subtract(S1, S2),
    true = Mod:is_set(S),
    true = Mod:size(S) =< Mod:size(S1),
    S.
	    
union(Mod, Equal, S1, S2) ->
    S = Mod:union(S1, S2),
    true = Equal(S, Mod:union(S2, S1)),
    true = Mod:is_set(S),
    S.

union(Mod, Equal, Ss) ->
    S = Mod:union(Ss),
    true = Equal(S, Mod:union(lists:reverse(Ss))),
    S.
		 
is_subset(Mod, Equal, S, Set) ->
    case Mod:is_subset(S, Set) of
	false -> false;
	true ->
	    case Mod:is_subset(Set, S) of
		false -> ok;
		true ->
		    %% The sets are subsets of each other.
		    %% They must be equal.
		    true = Equal(S, Set)
	    end,
	    true
    end.

fold(Mod, F, A, S) ->
    true = Mod:is_set(S),
    Mod:fold(F, A, S).

filter(Mod, F, S) ->
    true = Mod:is_set(S),
    Mod:filter(F, S).
