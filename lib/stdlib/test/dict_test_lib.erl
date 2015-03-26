%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2013. All Rights Reserved.
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

-module(dict_test_lib).

-export([new/2]).

new(Mod, Eq) ->
    fun (enter, {K,V,D}) -> enter(Mod, K, V, D);
	(empty, []) -> empty(Mod);
	(equal, {D1,D2}) -> Eq(D1, D2);
	(from_list, L) -> from_list(Mod, L);
	(module, []) -> Mod;
	(size, D) -> Mod:size(D);
	(is_empty, D) -> Mod:is_empty(D);
    (iterate_from, {S, D}) -> iterate_from(Mod, S, D);
    (iterate, D) -> iterate(Mod, D);
	(to_list, D) -> to_list(Mod, D)
    % (to_list_ordered, D) -> to_list_ordered(Mod, D)
    end.

empty(Mod) ->
    case erlang:function_exported(Mod, new, 0) of
	false -> Mod:empty();
	true -> Mod:new()
    end.

to_list(Mod, D) ->
    Mod:to_list(D).

% to_list_ordered(Mod, D) ->
    % L = Mod:to_list(D),
    % case Mod of
        % dict -> lists:sort(L);
        % _ -> L
    % end.

from_list(Mod, L) ->
    case erlang:function_exported(Mod, from_orddict, 1) of
	false ->
	    Mod:from_list(L);
	true ->
	    %% The gb_trees module has no from_list/1 function.
	    %%
	    %% The keys in S are not unique. To make sure
	    %% that we pick the same key/value pairs as
	    %% dict/orddict, first convert the list to an orddict.
	    Orddict = orddict:from_list(L),
	    Mod:from_orddict(Orddict)
    end.

iterate(Mod, Dict) ->
    case erlang:function_exported(Mod, iterator, 1) of
        true ->
            lists:reverse(iterate_tree(Mod, Dict));
        false ->
            Mod:to_list(Dict)
    end.

iterate_from(Mod, Start, Dict) ->
    case erlang:function_exported(Mod, iterator_from, 2) of
        true ->
            lists:reverse(iterate_tree(Mod, Start, Dict));
        false ->
            Filter = fun({K, _}) -> K >= Start end,
            lists:filter(Filter, Mod:to_list(Dict))
    end.

iterate_tree(Mod, Tree) ->
    I = Mod:iterator(Tree),
    iterate_tree_1(Mod, Mod:next(I), []).

iterate_tree(Mod, Start, Tree) ->
    I = Mod:iterator_from(Start, Tree),
    iterate_tree_1(Mod, Mod:next(I), []).

iterate_tree_1(_, none, R) ->
    R;
iterate_tree_1(Mod, {K, V, I}, R) ->
    iterate_tree_1(Mod, Mod:next(I), [{K, V} | R]).

%% Store new value into dictionary or update previous value in dictionary.
enter(Mod, Key, Val, Dict) ->
    case erlang:function_exported(Mod, store, 3) of
	false ->
	    Mod:enter(Key, Val, Dict);
	true ->
	    Mod:store(Key, Val, Dict)
    end.
