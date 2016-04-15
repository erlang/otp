%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
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

-module(maps).

-export([get/3, filter/2,fold/3,
         map/2, size/1,
         update_with/3, update_with/4,
         without/2, with/2]).

%% BIFs
-export([get/2, find/2, from_list/1,
         is_key/2, keys/1, merge/2,
         new/0, put/3, remove/2, take/2,
         to_list/1, update/3, values/1]).

%% Shadowed by erl_bif_types: maps:get/2
-spec get(Key,Map) -> Value when
    Key :: term(),
    Map :: map(),
    Value :: term().

get(_,_) -> erlang:nif_error(undef).


-spec find(Key,Map) -> {ok, Value} | error when
    Key :: term(),
    Map :: map(),
    Value :: term().

find(_,_) -> erlang:nif_error(undef).

%% Shadowed by erl_bif_types: maps:from_list/1
-spec from_list(List) -> Map when
    List :: [{Key,Value}],
    Key :: term(),
    Value :: term(),
    Map :: map().

from_list(_) -> erlang:nif_error(undef).


%% Shadowed by erl_bif_types: maps:is_key/2
-spec is_key(Key,Map) -> boolean() when
    Key :: term(),
    Map :: map().

is_key(_,_) -> erlang:nif_error(undef).


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


-spec remove(Key,Map1) -> Map2 when
    Key :: term(),
    Map1 :: map(),
    Map2 :: map().

remove(_,_) -> erlang:nif_error(undef).

-spec take(Key,Map1) -> {Value,Map2} | error when
    Key :: term(),
    Map1 :: map(),
    Value :: term(),
    Map2 :: map().

take(_,_) -> erlang:nif_error(undef).

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


-spec values(Map) -> Values when
    Map :: map(),
    Values :: [Value],
    Value :: term().

values(_) -> erlang:nif_error(undef).

%% End of BIFs

-spec update_with(Key,Fun,Map1) -> Map2 when
      Key :: term(),
      Map1 :: map(),
      Map2 :: map(),
      Fun :: fun((Value1 :: term()) -> Value2 :: term()).

update_with(Key,Fun,Map) when is_function(Fun,1), is_map(Map) ->
    try maps:get(Key,Map) of
        Val -> maps:update(Key,Fun(Val),Map)
    catch
        error:{badkey,_} ->
            erlang:error({badkey,Key},[Key,Fun,Map])
    end;
update_with(Key,Fun,Map) ->
    erlang:error(error_type(Map),[Key,Fun,Map]).


-spec update_with(Key,Fun,Init,Map1) -> Map2 when
      Key :: term(),
      Map1 :: Map1,
      Map2 :: Map2,
      Fun :: fun((Value1 :: term()) -> Value2 :: term()),
      Init :: term().

update_with(Key,Fun,Init,Map) when is_function(Fun,1), is_map(Map) ->
    case maps:find(Key,Map) of
        {ok,Val} -> maps:update(Key,Fun(Val),Map);
        error -> maps:put(Key,Init,Map)
    end;
update_with(Key,Fun,Init,Map) ->
    erlang:error(error_type(Map),[Key,Fun,Init,Map]).


-spec get(Key, Map, Default) -> Value | Default when
        Key :: term(),
        Map :: map(),
        Value :: term(),
        Default :: term().

get(Key,Map,Default) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            Value;
        error ->
            Default
    end;
get(Key,Map,Default) ->
    erlang:error({badmap,Map},[Key,Map,Default]).


-spec filter(Pred,Map1) -> Map2 when
      Pred :: fun((Key, Value) -> boolean()),
      Key  :: term(),
      Value :: term(),
      Map1 :: map(),
      Map2 :: map().

filter(Pred,Map) when is_function(Pred,2), is_map(Map) ->
    maps:from_list([{K,V}||{K,V}<-maps:to_list(Map),Pred(K,V)]);
filter(Pred,Map) ->
    erlang:error(error_type(Map),[Pred,Map]).


-spec fold(Fun,Init,Map) -> Acc when
    Fun :: fun((K, V, AccIn) -> AccOut),
    Init :: term(),
    Acc :: term(),
    AccIn :: term(),
    AccOut :: term(),
    Map :: map(),
    K :: term(),
    V :: term().

fold(Fun,Init,Map) when is_function(Fun,3), is_map(Map) ->
    lists:foldl(fun({K,V},A) -> Fun(K,V,A) end,Init,maps:to_list(Map));
fold(Fun,Init,Map) ->
    erlang:error(error_type(Map),[Fun,Init,Map]).

-spec map(Fun,Map1) -> Map2 when
    Fun :: fun((K, V1) -> V2),
    Map1 :: map(),
    Map2 :: map(),
    K :: term(),
    V1 :: term(),
    V2 :: term().

map(Fun,Map) when is_function(Fun, 2), is_map(Map) ->
    maps:from_list([{K,Fun(K,V)}||{K,V}<-maps:to_list(Map)]);
map(Fun,Map) ->
    erlang:error(error_type(Map),[Fun,Map]).


-spec size(Map) -> non_neg_integer() when
    Map :: map().

size(Map) when is_map(Map) ->
    erlang:map_size(Map);
size(Val) ->
    erlang:error({badmap,Val},[Val]).


-spec without(Ks,Map1) -> Map2 when
    Ks :: [K],
    Map1 :: map(),
    Map2 :: map(),
    K :: term().

without(Ks,M) when is_list(Ks), is_map(M) ->
    lists:foldl(fun(K, M1) -> ?MODULE:remove(K, M1) end, M, Ks);
without(Ks,M) ->
    erlang:error(error_type(M),[Ks,M]).


-spec with(Ks, Map1) -> Map2 when
    Ks :: [K],
    Map1 :: map(),
    Map2 :: map(),
    K :: term().

with(Ks,Map1) when is_list(Ks), is_map(Map1) ->
    Fun = fun(K, List) ->
      case ?MODULE:find(K, Map1) of
          {ok, V} ->
              [{K, V} | List];
          error ->
              List
      end
    end,
    ?MODULE:from_list(lists:foldl(Fun, [], Ks));
with(Ks,M) ->
    erlang:error(error_type(M),[Ks,M]).


error_type(M) when is_map(M) -> badarg;
error_type(V) -> {badmap, V}.
