%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2023. All Rights Reserved.
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

-export([get/3, filter/2, filtermap/2, fold/3, foreach/2,
         map/2, size/1, new/0,
         update_with/3, update_with/4,
         without/2, with/2,
         iterator/1, iterator/2,
         next/1,
         intersect/2, intersect_with/3,
         merge_with/3,
         groups_from_list/2, groups_from_list/3]).

%% Internal
-export([is_iterator_valid/1]).

%% BIFs
-export([get/2, find/2, from_list/1, from_keys/2,
         is_key/2, keys/1, merge/2,
         put/3, remove/2, take/2,
         to_list/1, update/3, values/1]).

-opaque iterator(Key, Value) :: {Key, Value, iterator(Key, Value)} | none
                              | nonempty_improper_list(integer(), #{Key => Value})
                              | nonempty_improper_list(list(Key), #{Key => Value}).

-type iterator() :: iterator(term(), term()).

-type iterator_order(Key) :: undefined | ordered | reversed
                           | fun((A :: Key, B :: Key) -> boolean()).
-type iterator_order() :: iterator_order(term()).

-export_type([iterator/2, iterator/0, iterator_order/1, iterator_order/0]).

-dialyzer({no_improper_lists, [iterator/1, iterator/2]}).

%% We must inline these functions so that the stacktrace points to
%% the correct function.
-compile({inline, [badarg_with_info/1,
                   error_with_info/2]}).

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

%% Shadowed by erl_bif_types: maps:from_keys/2
-spec from_keys(Keys, Value) -> Map when
    Keys :: list(),
    Value :: term(),
    Map :: map().

from_keys(_, _) -> erlang:nif_error(undef).

-spec intersect(Map1,Map2) -> Map3 when
    Map1 :: #{Key => term()},
    Map2 :: #{term() => Value2},
    Map3 :: #{Key => Value2}.

intersect(Map1, Map2) when is_map(Map1), is_map(Map2) ->
    case map_size(Map1) =< map_size(Map2) of
        true ->
            intersect_with_small_map_first(fun intersect_combiner_v2/3, Map1, Map2);
        false ->
            intersect_with_small_map_first(fun intersect_combiner_v1/3, Map2, Map1)
    end;
intersect(Map1, Map2) ->
    error_with_info(error_type_two_maps(Map1, Map2), [Map1,Map2]).

intersect_combiner_v1(_K, V1, _V2) -> V1.
intersect_combiner_v2(_K, _V1, V2) -> V2.

-spec intersect_with(Combiner, Map1, Map2) -> Map3 when
    Map1 :: #{Key => Value1},
    Map2 :: #{term() => Value2},
    Combiner :: fun((Key, Value1, Value2) -> CombineResult),
    Map3 :: #{Key => CombineResult}.

intersect_with(Combiner, Map1, Map2) when is_map(Map1),
                                          is_map(Map2),
                                          is_function(Combiner, 3) ->
    %% Use =< because we want to avoid reversing the combiner if we can
    case map_size(Map1) =< map_size(Map2) of
        true ->
            intersect_with_small_map_first(Combiner, Map1, Map2);
        false ->
            RCombiner = fun(K, V1, V2) -> Combiner(K, V2, V1) end,
            intersect_with_small_map_first(RCombiner, Map2, Map1)
    end;
intersect_with(Combiner, Map1, Map2) ->
    error_with_info(error_type_merge_intersect(Map1, Map2, Combiner),
                    [Combiner, Map1, Map2]).

intersect_with_small_map_first(Combiner, SmallMap, BigMap) ->
    Next = maps:next(maps:iterator(SmallMap)),
    intersect_with_iterate(Next, [], BigMap, Combiner).

intersect_with_iterate({K, V1, Iterator}, Keep, BigMap, Combiner) ->
    Next = maps:next(Iterator),
    case BigMap of
        #{ K := V2 } ->
            V = Combiner(K, V1, V2),
            intersect_with_iterate(Next, [{K,V}|Keep], BigMap, Combiner);
        _ ->
            intersect_with_iterate(Next, Keep, BigMap, Combiner)
    end;
intersect_with_iterate(none, Keep, _BigMap2, _Combiner) ->
    maps:from_list(Keep).

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

-spec merge_with(Combiner, Map1, Map2) -> Map3 when
    Map1 :: #{Key1 => Value1},
    Map2 :: #{Key2 => Value2},
    Combiner :: fun((Key1, Value1, Value2) -> CombineResult),
    Map3 :: #{Key1 => CombineResult, Key1 => Value1, Key2 => Value2}.

merge_with(Combiner, Map1, Map2) when is_map(Map1),
                                 is_map(Map2),
                                 is_function(Combiner, 3) ->
    case map_size(Map1) > map_size(Map2) of
        true ->
            Iterator = maps:iterator(Map2),
            merge_with_1(maps:next(Iterator),
                         Map1,
                         Map2,
                         Combiner);
        false ->
            Iterator = maps:iterator(Map1),
            merge_with_1(maps:next(Iterator),
                         Map2,
                         Map1,
                         fun(K, V1, V2) -> Combiner(K, V2, V1) end)
    end;
merge_with(Combiner, Map1, Map2) ->
    error_with_info(error_type_merge_intersect(Map1, Map2, Combiner),
                    [Combiner, Map1, Map2]).

merge_with_1({K, V2, Iterator}, Map1, Map2, Combiner) ->
    case Map1 of
        #{ K := V1 } ->
            NewMap1 = Map1#{ K := Combiner(K, V1, V2) },
            merge_with_1(maps:next(Iterator), NewMap1, Map2, Combiner);
        #{ } ->
            merge_with_1(maps:next(Iterator), maps:put(K, V2, Map1), Map2, Combiner)
    end;
merge_with_1(none, Result, _, _) ->
    Result.


%% Shadowed by erl_bif_types: maps:put/3
-spec put(Key,Value,Map1) -> Map2 when
    Key :: term(),
    Value :: term(),
    Map1 :: map(),
    Map2 :: map().

put(_,_,_) -> erlang:nif_error(undef).


%% Shadowed by erl_bif_types: maps:remove/2
-spec remove(Key,Map1) -> Map2 when
    Key :: term(),
    Map1 :: map(),
    Map2 :: map().

remove(_,_) -> erlang:nif_error(undef).

-spec take(Key,Map1) -> {Value,Map2} | error when
    Map1 :: #{Key => Value, _ => _},
    Map2 :: #{_ => _}.

take(_,_) -> erlang:nif_error(undef).

-spec to_list(MapOrIterator) -> [{Key, Value}] when
    MapOrIterator :: #{Key => Value} | iterator(Key, Value).

to_list(Map) when is_map(Map) ->
    to_list_internal(erts_internal:map_next(0, Map, []));
to_list(Iter) ->
    try to_list_from_iterator(next(Iter))
    catch
        error:_ ->
            error_with_info({badmap, Iter}, [Iter])
    end.

to_list_from_iterator({Key, Value, NextIter}) ->
    [{Key, Value} | to_list_from_iterator(next(NextIter))];
to_list_from_iterator(none) ->
    [].

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
    error_with_info(error_type(Map), [Key,Fun,Map]).


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
    error_with_info(error_type(Map), [Key,Fun,Init,Map]).


-spec get(Key, Map, Default) -> Value | Default when
      Map :: #{Key => Value, _ => _}.

get(Key,Map,Default) when is_map(Map) ->
    case Map of
        #{Key := Value} -> Value;
        #{} -> Default
    end;
get(Key,Map,Default) ->
    error_with_info({badmap,Map}, [Key,Map,Default]).

-spec filter(Pred, MapOrIter) -> Map when
      Pred :: fun((Key, Value) -> boolean()),
      MapOrIter :: #{Key => Value} | iterator(Key, Value),
      Map :: #{Key => Value}.

filter(Pred, Map) when is_map(Map), is_function(Pred, 2) ->
    maps:from_list(filter_1(Pred, next(iterator(Map)), undefined));
filter(Pred, Iter) when is_function(Pred, 2) ->
    ErrorTag = make_ref(),
    try filter_1(Pred, try_next(Iter, ErrorTag), ErrorTag) of
        Result ->
            maps:from_list(Result)
    catch
        error:ErrorTag ->
            error_with_info({badmap, Iter}, [Pred, Iter])
    end;
filter(Pred, Map) ->
    badarg_with_info([Pred, Map]).

filter_1(Pred, {K, V, Iter}, ErrorTag) ->
    case Pred(K, V) of
        true ->
            [{K,V} | filter_1(Pred, try_next(Iter, ErrorTag), ErrorTag)];
        false ->
            filter_1(Pred, try_next(Iter, ErrorTag), ErrorTag)
    end;
filter_1(_Pred, none, _ErrorTag) ->
    [].

-spec filtermap(Fun, MapOrIter) -> Map when
      Fun :: fun((Key, Value1) -> boolean() | {true, Value2}),
      MapOrIter :: #{Key => Value1} | iterator(Key, Value1),
      Map :: #{Key => Value1 | Value2}.

filtermap(Fun, Map) when is_map(Map), is_function(Fun, 2) ->
    maps:from_list(filtermap_1(Fun, next(iterator(Map)), undefined));
filtermap(Fun, Iter) when is_function(Fun, 2) ->
    ErrorTag = make_ref(),
    try filtermap_1(Fun, try_next(Iter, ErrorTag), ErrorTag) of
        Result ->
            maps:from_list(Result)
    catch
        error:ErrorTag ->
            error_with_info({badmap, Iter}, [Fun, Iter])
    end;
filtermap(Fun, Map) ->
    badarg_with_info([Fun, Map]).

filtermap_1(Fun, {K, V, Iter}, ErrorTag) ->
    case Fun(K, V) of
        true ->
            [{K, V} | filtermap_1(Fun, try_next(Iter, ErrorTag), ErrorTag)];
        {true, NewV} ->
            [{K, NewV} | filtermap_1(Fun, try_next(Iter, ErrorTag), ErrorTag)];
        false ->
            filtermap_1(Fun, try_next(Iter, ErrorTag), ErrorTag)
    end;
filtermap_1(_Fun, none, _ErrorTag) ->
    [].

-spec foreach(Fun,MapOrIter) -> ok when
      Fun :: fun((Key, Value) -> term()),
      MapOrIter :: #{Key => Value} | iterator(Key, Value).

foreach(Fun, Map) when is_map(Map), is_function(Fun, 2) ->
    foreach_1(Fun, next(iterator(Map)), undefined);
foreach(Fun, Iter) when is_function(Fun, 2) ->
    ErrorTag = make_ref(),
    try foreach_1(Fun, try_next(Iter, ErrorTag), ErrorTag)
    catch
        error:ErrorTag ->
            error_with_info({badmap, Iter}, [Fun, Iter])
    end;
foreach(Fun, Map) ->
    badarg_with_info([Fun, Map]).

foreach_1(Fun, {K, V, Iter}, ErrorTag) ->
    Fun(K,V),
    foreach_1(Fun, try_next(Iter, ErrorTag), ErrorTag);
foreach_1(_Fun, none, _ErrorTag) ->
    ok.

-spec fold(Fun,Init,MapOrIter) -> Acc when
    Fun :: fun((Key, Value, AccIn) -> AccOut),
    Init :: term(),
    Acc :: AccOut,
    AccIn :: Init | AccOut,
    MapOrIter :: #{Key => Value} | iterator(Key, Value).

fold(Fun, Init, Map) when is_map(Map), is_function(Fun, 3) ->
    fold_1(Fun, Init, next(iterator(Map)), undefined);
fold(Fun, Init, Iter) when is_function(Fun, 3) ->
    ErrorTag = make_ref(),
    try fold_1(Fun, Init, try_next(Iter, ErrorTag), ErrorTag)
    catch
        error:ErrorTag ->
            error_with_info({badmap, Iter}, [Fun, Init, Iter])
    end;
fold(Fun, Init, Map) ->
    badarg_with_info([Fun, Init, Map]).

fold_1(Fun, Acc, {K, V, Iter}, ErrorTag) ->
    fold_1(Fun, Fun(K,V,Acc), try_next(Iter, ErrorTag), ErrorTag);
fold_1(_Fun, Acc, none, _ErrorTag) ->
    Acc.

-spec map(Fun,MapOrIter) -> Map when
    Fun :: fun((Key, Value1) -> Value2),
    MapOrIter :: #{Key => Value1} | iterator(Key, Value1),
    Map :: #{Key => Value2}.

map(Fun, Map) when is_map(Map), is_function(Fun, 2) ->
    maps:from_list(map_1(Fun, next(iterator(Map)), undefined));
map(Fun, Iter) when is_function(Fun, 2) ->
    ErrorTag = make_ref(),
    try map_1(Fun, try_next(Iter, ErrorTag), ErrorTag) of
        Result ->
            maps:from_list(Result)
    catch
        error:ErrorTag ->
            error_with_info({badmap, Iter}, [Fun, Iter])
    end;
map(Fun, Map) ->
    badarg_with_info([Fun, Map]).


map_1(Fun, {K, V, Iter}, ErrorTag) ->
    [{K, Fun(K, V)} | map_1(Fun, try_next(Iter, ErrorTag), ErrorTag)];
map_1(_Fun, none, _ErrorTag) ->
    [].

-spec size(Map) -> non_neg_integer() when
    Map :: map().

size(Map) ->
    try
        map_size(Map)
    catch
        _:_ ->
            error_with_info({badmap,Map}, [Map])
    end.

-spec iterator(Map) -> Iterator when
      Map :: #{Key => Value},
      Iterator :: iterator(Key, Value).

iterator(M) when is_map(M) -> iterator(M, undefined);
iterator(M) -> error_with_info({badmap, M}, [M]).

-spec iterator(Map, Order) -> Iterator when
      Map :: #{Key => Value},
      Order :: iterator_order(Key),
      Iterator :: iterator(Key, Value).

iterator(M, undefined) when is_map(M) ->
    [0 | M];
iterator(M, ordered) when is_map(M) ->
    CmpFun = fun(A, B) -> erts_internal:cmp_term(A, B) =< 0 end,
    Keys = lists:sort(CmpFun, maps:keys(M)),
    [Keys | M];
iterator(M, reversed) when is_map(M) ->
    CmpFun = fun(A, B) -> erts_internal:cmp_term(B, A) =< 0 end,
    Keys = lists:sort(CmpFun, maps:keys(M)),
    [Keys | M];
iterator(M, CmpFun) when is_map(M), is_function(CmpFun, 2) ->
    Keys = lists:sort(CmpFun, maps:keys(M)),
    [Keys | M];
iterator(M, Order) ->
    badarg_with_info([M, Order]).

-spec next(Iterator) -> {Key, Value, NextIterator} | 'none' when
      Iterator :: iterator(Key, Value),
      NextIterator :: iterator(Key, Value).
next({K, V, I}) ->
    {K, V, I};
next([Path | Map] = Iterator)
  when (is_integer(Path) orelse is_list(Path)), is_map(Map) ->
    try erts_internal:map_next(Path, Map, iterator) of
        Result -> Result
    catch
        error:badarg ->
            badarg_with_info([Iterator])
    end;
next(none) ->
    none;
next(Iter) ->
    badarg_with_info([Iter]).

-spec without(Ks,Map1) -> Map2 when
    Ks :: [K],
    Map1 :: map(),
    Map2 :: map(),
    K :: term().

without(Ks,M) when is_list(Ks), is_map(M) ->
    lists:foldl(fun maps:remove/2, M, Ks);
without(Ks,M) ->
    error_with_info(error_type(M), [Ks,M]).

-spec with(Ks, Map1) -> Map2 when
    Ks :: [K],
    Map1 :: #{K => V, _ => _},
    Map2 :: #{K => V}.

with(Ks,Map1) when is_list(Ks), is_map(Map1) ->
    maps:from_list(with_1(Ks, Map1));
with(Ks,M) ->
    error_with_info(error_type(M), [Ks,M]).

with_1([K|Ks], Map) ->
    case Map of
        #{K := V} -> [{K,V}|with_1(Ks, Map)];
        #{} -> with_1(Ks, Map)
    end;
with_1([], _Map) -> [].

%% groups_from_list/2 & groups_from_list/3

-spec groups_from_list(KeyFun, List) -> GroupsMap when
    KeyFun :: fun((Elem) -> Key),
    GroupsMap :: #{Key => Group},
    Key :: term(),
    List :: [Elem],
    Group :: [Elem],
    Elem :: term().

groups_from_list(Fun, List0) when is_function(Fun, 1) ->
    try lists:reverse(List0) of
        List ->
            groups_from_list_1(Fun, List, #{})
    catch
        error:_ ->
            badarg_with_info([Fun, List0])
    end;
groups_from_list(Fun, List) ->
    badarg_with_info([Fun, List]).

groups_from_list_1(Fun, [H | Tail], Acc) ->
    K = Fun(H),
    NewAcc = case Acc of
                 #{K := Vs} -> Acc#{K := [H | Vs]};
                 #{} -> Acc#{K => [H]}
             end,
    groups_from_list_1(Fun, Tail, NewAcc);
groups_from_list_1(_Fun, [], Acc) ->
    Acc.

-spec groups_from_list(KeyFun, ValueFun, List) -> GroupsMap when
    KeyFun :: fun((Elem) -> Key),
    ValueFun :: fun((Elem) -> Value),
    GroupsMap :: #{Key := Group},
    Key :: term(),
    Value :: term(),
    List :: [Elem],
    Group :: [Value],
    Elem :: term().

groups_from_list(Fun, ValueFun, List0) when is_function(Fun, 1),
                                            is_function(ValueFun, 1) ->
    try lists:reverse(List0) of
        List ->
            groups_from_list_2(Fun, ValueFun, List, #{})
    catch
        error:_ ->
            badarg_with_info([Fun, ValueFun, List0])
    end;
groups_from_list(Fun, ValueFun, List) ->
    badarg_with_info([Fun, ValueFun, List]).

groups_from_list_2(Fun, ValueFun, [H | Tail], Acc) ->
    K = Fun(H),
    V = ValueFun(H),
    NewAcc = case Acc of
                 #{K := Vs} -> Acc#{K := [V | Vs]};
                 #{} -> Acc#{K => [V]}
             end,
    groups_from_list_2(Fun, ValueFun, Tail, NewAcc);
groups_from_list_2(_Fun, _ValueFun, [], Acc) ->
    Acc.

error_type(M) when is_map(M) -> badarg;
error_type(V) -> {badmap, V}.

error_type_two_maps(M1, M2) when is_map(M1) ->
    {badmap, M2};
error_type_two_maps(M1, _M2) ->
    {badmap, M1}.

error_type_merge_intersect(M1, M2, Combiner) when is_function(Combiner, 3) ->
    error_type_two_maps(M1, M2);
error_type_merge_intersect(_M1, _M2, _Combiner) ->
    badarg.

badarg_with_info(Args) ->
    erlang:error(badarg, Args, [{error_info, #{module => erl_stdlib_errors}}]).

error_with_info(Reason, Args) ->
    erlang:error(Reason, Args, [{error_info, #{module => erl_stdlib_errors}}]).

-spec is_iterator_valid(MaybeIter) -> boolean() when
    MaybeIter :: iterator() | term().

is_iterator_valid(Iter) ->
    try is_iterator_valid_1(Iter)
    catch
        error:badarg ->
            false
    end.

is_iterator_valid_1(none) ->
    true;
is_iterator_valid_1({_, _, Next}) ->
    is_iterator_valid_1(next(Next));
is_iterator_valid_1(Iter) ->
    _ = next(Iter),
    true.

try_next({_, _, _} = KVI, _ErrorTag) ->
    KVI;
try_next(none, _ErrorTag) ->
    none;
try_next(Iter, undefined) ->
    next(Iter);
try_next(Iter, ErrorTag) ->
    try next(Iter)
    catch
        error:badarg ->
            error(ErrorTag)
    end.
