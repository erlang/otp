%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2018. All Rights Reserved.
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
         map/2, size/1, new/0,
         update_with/3, update_with/4,
         without/2, with/2,
         iterator/1, next/1]).

%% BIFs
-export([get/2, find/2, from_list/1,
         is_key/2, keys/1, merge/2,
         put/3, remove/2, take/2,
         to_list/1, update/3, values/1]).

-opaque iterator(Key, Value) :: {Key, Value, iterator(Key, Value)} | none
                              | nonempty_improper_list(integer(), #{Key => Value}).

-type iterator() :: iterator(term(), term()).

-export_type([iterator/2, iterator/0]).

-dialyzer({no_improper_lists, iterator/1}).

-define(IS_ITERATOR(I), is_tuple(I) andalso tuple_size(I) == 3; I == none; is_integer(hd(I)) andalso is_map(tl(I))).

%% Shadowed by erl_bif_types: maps:get/2
-spec get(Key,Map) -> Value when
    Key :: term(),
    Map :: map(),
    Value :: term().

get(_,_) -> erlang:nif_error(undef).

-spec find(Key,Map) -> {ok, Value} | error when
    Map :: #{Key => Value, _ => _}.

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
    Map :: #{Key => _},
    Keys :: [Key].

keys(_) -> erlang:nif_error(undef).


%% Shadowed by erl_bif_types: maps:merge/2
-spec merge(Map1,Map2) -> Map3 when
    Map1 :: map(),
    Map2 :: map(),
    Map3 :: map().

merge(_,_) -> erlang:nif_error(undef).


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
    Map1 :: #{Key => Value, _ => _},
    Map2 :: #{_ => _}.

take(_,_) -> erlang:nif_error(undef).

-spec to_list(Map) -> [{Key,Value}] when
    Map :: #{Key => Value}.

to_list(Map) when is_map(Map) ->
    to_list_internal(erts_internal:map_next(0, Map, []));
to_list(Map) ->
    erlang:error({badmap,Map},[Map]).

to_list_internal([Iter, Map | Acc]) when is_integer(Iter) ->
    to_list_internal(erts_internal:map_next(Iter, Map, Acc));
to_list_internal(Acc) ->
    Acc.

%% Shadowed by erl_bif_types: maps:update/3
-spec update(Key,Value,Map1) -> Map2 when
    Map1 :: #{Key := _, _ => _},
    Map2 :: #{Key := Value, _ => _}.

update(_,_,_) -> erlang:nif_error(undef).


-spec values(Map) -> Values when
    Map :: #{_ => Value},
    Values :: [Value].

values(_) -> erlang:nif_error(undef).

%% End of BIFs

-spec new() -> Map when
    Map :: #{}.

new() -> #{}.

-spec update_with(Key,Fun,Map1) -> Map2 when
      Map1 :: #{Key := Value1, _ => _},
      Map2 :: #{Key := Value2, _ => _},
      Fun :: fun((Value1) -> Value2).

update_with(Key,Fun,Map) when is_function(Fun,1), is_map(Map) ->
    case Map of
        #{Key := Value} -> Map#{Key := Fun(Value)};
        #{} -> erlang:error({badkey,Key},[Key,Fun,Map])
    end;
update_with(Key,Fun,Map) ->
    erlang:error(error_type(Map),[Key,Fun,Map]).


-spec update_with(Key,Fun,Init,Map1) -> Map2 when
      Map1 :: #{Key => Value1, _ => _},
      Map2 :: #{Key := Value2 | Init, _ => _},
      Fun :: fun((Value1) -> Value2).

update_with(Key,Fun,Init,Map) when is_function(Fun,1), is_map(Map) ->
    case Map of
        #{Key := Value} -> Map#{Key := Fun(Value)};
        #{} -> Map#{Key => Init}
    end;
update_with(Key,Fun,Init,Map) ->
    erlang:error(error_type(Map),[Key,Fun,Init,Map]).


-spec get(Key, Map, Default) -> Value | Default when
      Map :: #{Key => Value, _ => _}.

get(Key,Map,Default) when is_map(Map) ->
    case Map of
        #{Key := Value} -> Value;
        #{} -> Default
    end;
get(Key,Map,Default) ->
    erlang:error({badmap,Map},[Key,Map,Default]).


-spec filter(Pred, MapOrIter) -> Map when
      Pred :: fun((Key, Value) -> boolean()),
      MapOrIter :: #{Key => Value} | iterator(Key, Value),
      Map :: #{Key => Value}.

filter(Pred,Map) when is_function(Pred,2), is_map(Map) ->
    maps:from_list(filter_1(Pred, iterator(Map)));
filter(Pred,Iterator) when is_function(Pred,2), ?IS_ITERATOR(Iterator) ->
    maps:from_list(filter_1(Pred, Iterator));
filter(Pred,Map) ->
    erlang:error(error_type(Map),[Pred,Map]).

filter_1(Pred, Iter) ->
    case next(Iter) of
        {K, V, NextIter} ->
            case Pred(K,V) of
                true ->
                    [{K,V} | filter_1(Pred, NextIter)];
                false ->
                    filter_1(Pred, NextIter)
            end;
        none ->
            []
    end.

-spec fold(Fun,Init,MapOrIter) -> Acc when
    Fun :: fun((Key, Value, AccIn) -> AccOut),
    Init :: term(),
    Acc :: AccOut,
    AccIn :: Init | AccOut,
    MapOrIter :: #{Key => Value} | iterator(Key, Value).

fold(Fun,Init,Map) when is_function(Fun,3), is_map(Map) ->
    fold_1(Fun,Init,iterator(Map));
fold(Fun,Init,Iterator) when is_function(Fun,3), ?IS_ITERATOR(Iterator) ->
    fold_1(Fun,Init,Iterator);
fold(Fun,Init,Map) ->
    erlang:error(error_type_iter(Map),[Fun,Init,Map]).

fold_1(Fun, Acc, Iter) ->
    case next(Iter) of
        {K, V, NextIter} ->
            fold_1(Fun, Fun(K,V,Acc), NextIter);
        none ->
            Acc
    end.

-spec map(Fun,MapOrIter) -> Map when
    Fun :: fun((Key, Value1) -> Value2),
    MapOrIter :: #{Key => Value1} | iterator(Key, Value1),
    Map :: #{Key => Value2}.

map(Fun,Map) when is_function(Fun, 2), is_map(Map) ->
    maps:from_list(map_1(Fun, iterator(Map)));
map(Fun,Iterator) when is_function(Fun, 2), ?IS_ITERATOR(Iterator) ->
    maps:from_list(map_1(Fun, Iterator));
map(Fun,Map) ->
    erlang:error(error_type_iter(Map),[Fun,Map]).

map_1(Fun, Iter) ->
    case next(Iter) of
        {K, V, NextIter} ->
            [{K, Fun(K, V)} | map_1(Fun, NextIter)];
        none ->
            []
    end.

-spec size(Map) -> non_neg_integer() when
    Map :: map().

size(Map) when is_map(Map) ->
    erlang:map_size(Map);
size(Val) ->
    erlang:error({badmap,Val},[Val]).

-spec iterator(Map) -> Iterator when
      Map :: #{Key => Value},
      Iterator :: iterator(Key, Value).

iterator(M) when is_map(M) -> [0 | M];
iterator(M) -> erlang:error({badmap, M}, [M]).

-spec next(Iterator) -> {Key, Value, NextIterator} | 'none' when
      Iterator :: iterator(Key, Value),
      NextIterator :: iterator(Key, Value).
next({K, V, I}) ->
    {K, V, I};
next([Path | Map]) when is_integer(Path), is_map(Map) ->
    erts_internal:map_next(Path, Map, iterator);
next(none) ->
    none;
next(Iter) ->
    erlang:error(badarg, [Iter]).

-spec without(Ks,Map1) -> Map2 when
    Ks :: [K],
    Map1 :: map(),
    Map2 :: map(),
    K :: term().

without(Ks,M) when is_list(Ks), is_map(M) ->
    lists:foldl(fun maps:remove/2, M, Ks);
without(Ks,M) ->
    erlang:error(error_type(M),[Ks,M]).

-spec with(Ks, Map1) -> Map2 when
    Ks :: [K],
    Map1 :: #{K => V, _ => _},
    Map2 :: #{K => V}.

with(Ks,Map1) when is_list(Ks), is_map(Map1) ->
    maps:from_list(with_1(Ks, Map1));
with(Ks,M) ->
    erlang:error(error_type(M),[Ks,M]).

with_1([K|Ks], Map) ->
    case Map of
        #{K := V} -> [{K,V}|with_1(Ks, Map)];
        #{} -> with_1(Ks, Map)
    end;
with_1([], _Map) -> [].

error_type(M) when is_map(M) -> badarg;
error_type(V) -> {badmap, V}.

error_type_iter(M) when is_map(M); ?IS_ITERATOR(M) -> badarg;
error_type_iter(V) -> {badmap, V}.
