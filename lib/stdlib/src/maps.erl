%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
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

-module(maps).

-export([
	foldl/3,
	foldr/3,
	map/2,
	size/1,
	without/2
    ]).


%%% BIFs
-export([
	get/2,
	find/2,
	from_list/1,
	is_key/2,
	keys/1,
	merge/2,
	new/0,
	put/3,
	remove/2,
	to_list/1,
	update/3,
	values/1
    ]).

-type map() :: term(). %% FIXME: remove when erl_bif_types knows map().

%% Shadowed by erl_bif_types: maps:get/3
-spec get(Key,Map) -> Value when
    Key :: term(),
    Map :: map(),
    Value :: term().

get(_,_) -> erlang:nif_error(undef).


%% Shadowed by erl_bif_types: maps:find/3
-spec find(Key,Map) -> {ok, Value} | error when
    Key :: term(),
    Map :: map(),
    Value :: term().

find(_,_) -> erlang:nif_error(undef).


%% Shadowed by erl_bif_types: maps:from_list/1
-spec from_list([{Key,Value}]) -> Map when
    Key :: term(),
    Value :: term(),
    Map :: map().

from_list(_) -> erlang:nif_error(undef).


%% Shadowed by erl_bif_types: maps:is_key/2
-spec is_key(Key,Map) -> boolean() when
    Key :: term(),
    Map :: map().

is_key(_,_) -> erlang:nif_error(undef).


%% Shadowed by erl_bif_types: maps:keys/1
-spec keys(Map) -> Keys when
    Map :: map(),
    Keys :: [Key],
    Key :: term().

keys(_) -> erlang:nif_error(undef).


%% Shadowed by erl_bif_types: maps:merge/2
-spec merge(Map1,Map2) -> Map3 when
    Map1 :: map(),
    Map2 :: map(),
    Map3 :: map().

merge(_,_) -> erlang:nif_error(undef).



%% Shadowed by erl_bif_types: maps:new/0
-spec new() -> Map when
    Map :: map().

new() -> erlang:nif_error(undef).


%% Shadowed by erl_bif_types: maps:put/3
-spec put(Key,Value,Map1) -> Map2 when
    Key :: term(),
    Value :: term(),
    Map1 :: map(),
    Map2 :: map().

put(_,_,_) -> erlang:nif_error(undef).


%% Shadowed by erl_bif_types: maps:put/3
-spec remove(Key,Map1) -> Map2 when
    Key :: term(),
    Map1 :: map(),
    Map2 :: map().

remove(_,_) -> erlang:nif_error(undef).


%% Shadowed by erl_bif_types: maps:to_list/1
-spec to_list(Map) -> [{Key,Value}] when
    Map :: map(),
    Key :: term(),
    Value :: term().

to_list(_) -> erlang:nif_error(undef).


%% Shadowed by erl_bif_types: maps:update/3
-spec update(Key,Value,Map1) -> Map2 when
    Key :: term(),
    Value :: term(),
    Map1 :: map(),
    Map2 :: map().

update(_,_,_) -> erlang:nif_error(undef).


%% Shadowed by erl_bif_types: maps:values/1
-spec values(Map) -> Keys when
    Map :: map(),
    Keys :: [Key],
    Key :: term().

values(_) -> erlang:nif_error(undef).


%%% End of BIFs

-spec foldl(Fun,Init,Map) -> Acc when
    Fun :: fun((K, V, AccIn) -> AccOut),
    Init :: term(),
    Acc :: term(),
    AccIn :: term(),
    AccOut :: term(),
    Map :: map(),
    K :: term(),
    V :: term().

foldl(Fun, Init, Map) ->
    lists:foldl(fun({K,V},A) -> Fun(K,V,A) end,Init,maps:to_list(Map)).

-spec foldr(Fun,Init,Map) -> Acc when
    Fun :: fun((K,V,AccIn) -> AccOut),
    Init :: term(),
    Acc :: term(),
    AccIn :: term(),
    AccOut :: term(),
    Map :: map(),
    K :: term(),
    V :: term().

foldr(Fun, Init, Map) ->
    lists:foldr(fun({K,V},A) -> Fun(K,V,A) end,Init,maps:to_list(Map)).


-spec map(Fun,Map1) -> Map2 when
    Fun :: fun((K, V1) -> V2),
    Map1 :: map(),
    Map2 :: map(),
    K :: term(),
    V1 :: term(),
    V2 :: term().

map(Fun, Map) ->
    maps:from_list(lists:map(fun
		({K,V}) ->
		    {K,Fun(K,V)}
	    end,maps:to_list(Map))).


-spec size(Map) -> non_neg_integer() when
    Map :: map().

size(Map) ->
    erlang:map_size(Map).

-spec without(Ks,Map1) -> Map2 when
    Ks :: [K],
    Map1 :: map(),
    Map2 :: map(),
    K :: term().

without(Ks, M) ->
    maps:from_list([{K,V}||{K,V} <- maps:to_list(M), not lists:member(K, Ks)]).
