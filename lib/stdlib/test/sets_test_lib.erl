%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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

-module(sets_test_lib, [Mod,Equal]).

-export([module/0,equal/2,empty/0,from_list/1,to_list/1,singleton/1,
	 add_element/2,del_element/2,size/1,is_empty/1,is_set/1,
	 intersection/1,intersection/2,subtract/2,
	 union/1,union/2,is_subset/2,fold/3,filter/2]).

module() ->
    Mod.

equal(X, Y) ->
    Equal(X, Y).

empty() ->
    Mod:new().

from_list(L) ->
    Mod:from_list(L).

to_list(S) ->
    Mod:to_list(S).

singleton(E) ->
    case erlang:function_exported(Mod, singleton, 1) of
	true -> Mod:singleton(E);
	false -> from_list([E])
    end.

add_element(El, S0) ->
    S = Mod:add_element(El, S0),
    true = Mod:is_element(El, S),
    false = is_empty(S),
    true = Mod:is_set(S),
    S.

del_element(El, S0) ->
    S = Mod:del_element(El, S0),
    false = Mod:is_element(El, S),
    true = Mod:is_set(S),
    S.

size(S) ->
    Mod:size(S).

is_empty(S) ->
    true = Mod:is_set(S),
    case erlang:function_exported(Mod, is_empty, 1) of
	true -> Mod:is_empty(S);
	false -> Mod:size(S) == 0
    end.

is_set(S) ->
    Mod:is_set(S).

intersection(S1, S2) ->
    S = Mod:intersection(S1, S2),
    true = Equal(S, Mod:intersection(S2, S1)),
    Disjoint = is_empty(S),
    Disjoint = Mod:is_disjoint(S1, S2),
    Disjoint = Mod:is_disjoint(S2, S1),
    S.

intersection(Ss) ->
    S = Mod:intersection(Ss),
    true = Equal(S, Mod:intersection(lists:reverse(Ss))),
    S.

subtract(S1, S2) ->
    S = Mod:subtract(S1, S2),
    true = Mod:is_set(S),
    true = Mod:size(S) =< Mod:size(S1),
    S.
	    
union(S1, S2) ->
    S = Mod:union(S1, S2),
    true = Equal(S, Mod:union(S2, S1)),
    true = Mod:is_set(S),
    S.

union(Ss) ->
    S = Mod:union(Ss),
    true = Equal(S, Mod:union(lists:reverse(Ss))),
    S.
		 
is_subset(S, Set) ->
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

fold(F, A, S) ->
    true = Mod:is_set(S),
    Mod:fold(F, A, S).

filter(F, S) ->
    true = Mod:is_set(S),
    Mod:filter(F, S).
