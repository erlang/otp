%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
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
        (iterator, S) -> Mod:iterator(S);
        (iterator_from, {Start, S}) -> Mod:iterator_from(Start, S);
        (next, I) -> Mod:next(I);
	(to_list, D) -> to_list(Mod, D);
	(erase, {K,D}) -> erase(Mod, K, D);
	(take, {K,D}) -> take(Mod, K, D)
    end.

empty(Mod) ->
    case erlang:function_exported(Mod, new, 0) of
	false -> Mod:empty();
	true -> Mod:new()
    end.

to_list(Mod, D) ->
    Mod:to_list(D).

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

%% Store new value into dictionary or update previous value in dictionary.
enter(Mod, Key, Val, Dict) ->
    case erlang:function_exported(Mod, store, 3) of
	false ->
	    Mod:enter(Key, Val, Dict);
	true ->
	    Mod:store(Key, Val, Dict)
    end.

erase(Mod, Key, Val) when Mod =:= dict; Mod =:= orddict ->
    Mod:erase(Key, Val);
erase(gb_trees, Key, Val) ->
    gb_trees:delete_any(Key, Val).

take(gb_trees, Key, Val) ->
    Res = try
	      gb_trees:take(Key, Val)
	  catch
	      error:_ ->
		  error
	  end,
    Res = gb_trees:take_any(Key, Val);
take(Mod, Key, Val) ->
    Mod:take(Key, Val).
