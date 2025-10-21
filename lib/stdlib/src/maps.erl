%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2013-2025. All Rights Reserved.
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
-moduledoc """
Maps processing functions.

This module contains functions for maps processing. The Efficiency Guide
contains a chapter that describes
[how to use maps efficiently](`e:system:maps.md`).
""".
-moduledoc(#{since => "OTP 17.0"}).

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

-doc """
An iterator representing the associations in a map with keys of type `Key` and
values of type `Value`.

Created using [`maps:iterator/1`](`iterator/1`) or
[`maps:iterator/2`](`iterator/2`).

Consumed by:

- [`maps:next/1`](`next/1`)
- [`maps:filter/2`](`filter/2`)
- [`maps:filtermap/2`](`filtermap/2`)
- [`maps:fold/3`](`fold/3`)
- [`maps:foreach/2`](`foreach/2`)
- [`maps:map/2`](`map/2`)
- [`maps:to_list/1`](`to_list/1`)
""".
-opaque iterator(Key, Value) :: {Key, Value, iterator(Key, Value)} | none
                              | nonempty_improper_list(integer(), #{Key => Value})
                              | nonempty_improper_list(list(Key), #{Key => Value}).

-type iterator() :: iterator(term(), term()).

-doc """
Key-based iterator order option that can be one of `undefined` (default for
[`maps:iterator/1`](`iterator/1`)), `ordered` (sorted in map-key order),
`reversed` (sorted in reverse map-key order), or a custom sorting function.

Used by [`maps:iterator/2`](`iterator/2`).

The [Expressions section](`e:system:expressions.md#term-comparisons`) contains
descriptions of how terms are ordered.
""".
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
-doc """
Returns value `Value` associated with `Key` if `Map` contains `Key`.

The call fails with a `{badmap,Map}` exception if `Map` is not a map, or with a
`{badkey,Key}` exception if no value is associated with `Key`.

## Examples

```erlang
1> Key = 1337.
2> Map = #{42 => value_two,1337 => "value one","a" => 1}.
3> maps:get(Key, Map).
"value one"
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec get(Key, Map) -> Value when
    Key :: term(),
    Map :: map(),
    Value :: term().

get(_, _) -> erlang:nif_error(undef).

-doc """
Returns a tuple `{ok, Value}`, where `Value` is the value associated with `Key`,
or `error` if no value is associated with `Key` in `Map`.

The call fails with a `{badmap,Map}` exception if `Map` is not a map.

## Examples

```erlang
1> Map = #{"hi" => 42}.
2> Key = "hi".
3> maps:find(Key, Map).
{ok,42}
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec find(Key, Map) -> {ok, Value} | error when
    Map :: #{Key => Value, _ => _}.

find(_, _) -> erlang:nif_error(undef).

%% Shadowed by erl_bif_types: maps:from_list/1
-doc """
Takes a list of key-value tuples and builds a map.

If the same key appears more than once, the last (rightmost) value is
used, and previous values are ignored.

## Examples

```erlang
1> List = [{"a",ignored},{1337,"value two"},{42,value_three},{"a",1}].
2> maps:from_list(List).
#{42 => value_three,1337 => "value two","a" => 1}
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec from_list(List) -> Map when
    List :: [{Key,Value}],
    Key :: term(),
    Value :: term(),
    Map :: map().

from_list(_) -> erlang:nif_error(undef).

%% Shadowed by erl_bif_types: maps:from_keys/2
-doc """
Takes a list of keys and a value and builds a map where all keys are
associated with the same value.

## Examples

```erlang
1> Keys = ["a", "b", "c"].
2> maps:from_keys(Keys, ok).
#{"a" => ok,"b" => ok,"c" => ok}
```
""".
-doc(#{since => <<"OTP 24.0">>}).
-spec from_keys(Keys, Value) -> Map when
    Keys :: list(),
    Value :: term(),
    Map :: map().

from_keys(_, _) -> erlang:nif_error(undef).

-doc """
Computes the intersection of maps `Map1` and `Map2`, producing a
single map `Map3`.

If a key exists in both maps, the value in `Map1` is superseded by the
value in `Map2`. Keys existing in only one of the maps are discarded
along with their values.

The call fails with a `{badmap,Map}` exception if `Map1` or `Map2` is not a map.

## Examples

```erlang
1> Map1 = #{a => "one", b => "two"}.
2> Map2 = #{a => 1, c => 3}.
3> maps:intersect(Map1, Map2).
#{a => 1}
```
""".
-doc(#{since => <<"OTP 24.0">>}).
-spec intersect(Map1, Map2) -> Map3 when
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

-doc """
Computes the intersection of maps `Map1` and `Map2`, producing a
single map `Map3`, where values having the same key are combined using
the `Combiner` fun.

When `Combiner` is applied, the key that exists in both maps is the
first parameter, the value from `Map1` is the second parameter, and
the value from `Map2` is the third parameter.

The call fails with a `{badmap,Map}` exception if `Map1` or `Map2` is not a map.
The call fails with a `badarg` exception if `Combiner` is not a fun that takes
three arguments.

## Examples

```erlang
1> Map1 = #{a => "one", b => "two"}.
2> Map2 = #{a => 1, c => 3}.
3> maps:intersect_with(fun(_Key, Val1, Val2) -> {Val1, Val2} end, Map1, Map2).
#{a => {"one",1}}
```
""".
-doc(#{since => <<"OTP 24.0">>}).
-spec intersect_with(Combiner, Map1, Map2) -> Map3 when
    Map1 :: #{Key => Value1},
    Map2 :: #{term() => Value2},
    Combiner :: fun((Key, Value1, Value2) -> CombineResult),
    Map3 :: #{Key => CombineResult}.

intersect_with(Combiner, Map1, Map2) when is_map(Map1),
                                          is_map(Map2),
                                          is_function(Combiner, 3) ->
    %% Use =< because we want to avoid reversing the combiner if we can.
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
-doc """
Returns `true` if map `Map` contains `Key`; otherwise, returns `false`.

The call fails with a `{badmap,Map}` exception if `Map` is not a map.

## Examples

```erlang
1> Map = #{"42" => value}.
#{"42" => value}
2> maps:is_key("42", Map).
true
3> maps:is_key(value, Map).
false
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec is_key(Key,Map) -> boolean() when
    Key :: term(),
    Map :: map().

is_key(_, _) -> erlang:nif_error(undef).


-doc """
Returns a complete list of keys contained in `Map`, in any order.

The call fails with a `{badmap,Map}` exception if `Map` is not a map.

## Examples

```erlang
1> Map = #{42 => three,1337 => "two","a" => 1}.
2> maps:keys(Map).
[42,1337,"a"]
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec keys(Map) -> Keys when
    Map :: #{Key => _},
    Keys :: [Key].

keys(_) -> erlang:nif_error(undef).


%% Shadowed by erl_bif_types: maps:merge/2
-doc """
Merges maps `Map1` and `Map2` into a single map `Map3`, where values
from `Map2` override those from `Map1` for duplicate keys.

The call fails with a `{badmap,Map}` exception if `Map1` or `Map2` is not a map.

## Examples

```erlang
1> Map1 = #{a => "one", b => "two"}.
2> Map2 = #{a => 1, c => 3}.
3> maps:merge(Map1, Map2).
#{a => 1,b => "two",c => 3}
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec merge(Map1, Map2) -> Map3 when
    Map1 :: map(),
    Map2 :: map(),
    Map3 :: map().

merge(_, _) -> erlang:nif_error(undef).

-doc """
Merges maps `Map1` and `Map2` into a single map `Map3`, combining values for
duplicate keys using the `Combiner` fun.

When `Combiner` is applied, the key that exists in both maps is the
first parameter, the value from `Map1` is the second parameter, and
the value from `Map2` is the third parameter.

The call fails with a `{badmap,Map}` exception if `Map1` or `Map2` is not a map.
The call fails with a `badarg` exception if `Combiner` is not a fun that takes
three arguments.

## Examples

```erlang
1> Map1 = #{a => 3, b => 5}.
2> Map2 = #{a => 4, c => 17}.
3> maps:merge_with(fun(_Key, Val1, Val2) -> Val1 + Val2 end, Map1, Map2).
#{a => 7,b => 5,c => 17}
```
""".
-doc(#{since => <<"OTP 24.0">>}).
-spec merge_with(Combiner, Map1, Map2) -> Map3 when
    Map1 :: #{Key1 => Value1},
    Map2 :: #{Key2 => Value2},
    Combiner :: fun((Key1, Value1, Value2) -> CombineResult),
    Map3 :: #{Key1 => CombineResult, Key1 => Value1, Key2 => Value2}.

merge_with(Combiner, Map1, Map2) when is_map(Map1),
                                 is_map(Map2),
                                 is_function(Combiner, 3) ->
    %% Use >= because we want to avoid reversing the combiner if we can
    case map_size(Map1) >= map_size(Map2) of
        true ->
            Iterator = maps:iterator(Map2),
            merge_with_1(maps:next(Iterator), Map1, Combiner);
        false ->
            Iterator = maps:iterator(Map1),
            merge_with_1(maps:next(Iterator),
                         Map2,
                         fun(K, V1, V2) -> Combiner(K, V2, V1) end)
    end;
merge_with(Combiner, Map1, Map2) ->
    error_with_info(error_type_merge_intersect(Map1, Map2, Combiner),
                    [Combiner, Map1, Map2]).

merge_with_1({K, V2, Iterator}, Map1, Combiner) ->
    case Map1 of
        #{ K := V1 } ->
            NewMap1 = Map1#{ K := Combiner(K, V1, V2) },
            merge_with_1(maps:next(Iterator), NewMap1, Combiner);
        #{ } ->
            merge_with_1(maps:next(Iterator), maps:put(K, V2, Map1), Combiner)
    end;
merge_with_1(none, Result, _) ->
    Result.


%% Shadowed by erl_bif_types: maps:put/3
-doc """
Associates `Key` with `Value` in Map1, replacing any existing value, and
returns a new map `Map2` with the updated association alongside the
original entries from `Map1`.

The call fails with a `{badmap,Map}` exception if `Map1` is not a map.

## Examples

```erlang
1> Map = #{"a" => 1}.
#{"a" => 1}
2> maps:put("a", 42, Map).
#{"a" => 42}
3> maps:put("b", 1337, Map).
#{"a" => 1,"b" => 1337}
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec put(Key, Value, Map1) -> Map2 when
    Key :: term(),
    Value :: term(),
    Map1 :: map(),
    Map2 :: map().

put(_, _, _) -> erlang:nif_error(undef).


%% Shadowed by erl_bif_types: maps:remove/2
-doc """
Removes `Key` and its associated value from `Map1`, if it exists, and
returns a new map `Map2` without `Key`.

The call fails with a `{badmap,Map}` exception if `Map1` is not a map.

## Examples

```erlang
1> Map = #{"a" => 1}.
#{"a" => 1}
2> maps:remove("a", Map).
#{}
3> maps:remove("b", Map).
#{"a" => 1}
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec remove(Key, Map1) -> Map2 when
    Key :: term(),
    Map1 :: map(),
    Map2 :: map().

remove(_, _) -> erlang:nif_error(undef).

-doc """
Removes `Key` and its associated value from `Map1`, if it exists,
returning a tuple with the removed value `Value` and the new map
`Map2`; otherwise, returns error.

The call will fail with a `{badmap,Map}` exception if `Map1` is not a map.

Example:

```erlang
1> Map = #{"a" => "hello", "b" => "world"}.
#{"a" => "hello", "b" => "world"}
2> maps:take("a", Map).
{"hello",#{"b" => "world"}}
3> maps:take("does not exist", Map).
error
```
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec take(Key, Map1) -> {Value,Map2} | error when
    Map1 :: #{Key => Value, _ => _},
    Map2 :: #{_ => _}.

take(_,_) -> erlang:nif_error(undef).

-doc """
Returns a list of pairs representing the key-value associations of
`MapOrIterator`.

Unless `MapOrIter` is an ordered iterator returned by `iterator/2`,
the order of the `{Key, Value}` tuples in the resulting list is not
defined.

The call fails with a `{badmap,Map}` exception if `MapOrIterator` is not a map
or an iterator obtained by a call to `iterator/1` or `iterator/2`.

## Examples

```erlang
1> Map = #{42 => value_three,1337 => "value two","a" => 1}.
2> maps:to_list(Map).
[{42,value_three},{1337,"value two"},{"a",1}]
```

Using an ordered iterator to return an ordered list:

```erlang
1> Map = #{z => 1, y => 2, x => 3}.
2> maps:to_list(maps:iterator(Map, ordered)).
[{x,3},{y,2},{z,1}]
```
""".
-doc(#{since => <<"OTP 17.0">>}).
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
-doc """
If `Key` exists in `Map1`, its value is replaced with `Value`, and the
function returns a new map `Map2` with the updated association.

The call fails with a `{badmap,Map}` exception if `Map1` is not a map, or with a
`{badkey,Key}` exception if no value is associated with `Key`.

## Examples

```erlang
1> Map = #{"a" => 1}.
#{"a" => 1}
2> maps:update("a", 42, Map).
#{"a" => 42}
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec update(Key, Value, Map1) -> Map2 when
    Map1 :: #{Key := _, _ => _},
    Map2 :: #{Key := Value, _ => _}.

update(_, _, _) -> erlang:nif_error(undef).


-doc """
Returns a complete list of values contained in map `Map`, in any order.

The call fails with a `{badmap,Map}` exception if `Map` is not a map.

## Examples

```erlang
1> Map = #{42 => value_three,1337 => "value two","a" => 1}.
2> maps:values(Map).
[value_three,"value two",1]
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec values(Map) -> Values when
    Map :: #{_ => Value},
    Values :: [Value].

values(_) -> erlang:nif_error(undef).

%% End of BIFs

-doc """
Returns a new empty map.

## Examples

```text
1> maps:new().
#{}
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec new() -> Map when
    Map :: #{}.

new() -> #{}.

-doc """
Updates a value in a `Map1` associated with `Key` by calling `Fun` on the old
value to produce a new value.

The call fails with a `{badkey,Key}` exception if `Key` is not present
in the map.

## Examples

```erlang
1> Map = #{counter => 1}.
2> Fun = fun(V) -> V + 1 end.
3> maps:update_with(counter, Fun, Map).
#{counter => 2}
```
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec update_with(Key, Fun, Map1) -> Map2 when
      Map1 :: #{Key := Value1, _ => _},
      Map2 :: #{Key := Value2, _ => _},
      Fun :: fun((Value1) -> Value2).

update_with(Key, Fun, Map) when is_function(Fun, 1), is_map(Map) ->
    case Map of
        #{Key := Value} -> Map#{Key := Fun(Value)};
        #{} -> error({badkey,Key}, [Key,Fun,Map])
    end;
update_with(Key, Fun, Map) ->
    error_with_info(error_type(Map), [Key,Fun,Map]).


-doc """
Updates the value in `Map1` for `Key` by applying `Fun` to the old value or
using `Init` if `Key` is not present in the map.

## Examples

```erlang
1> Map = #{"counter" => 1}.
2> Fun = fun(V) -> V + 1 end.
3> maps:update_with("counter", Fun, 42, Map).
#{"counter" => 2}
4> maps:update_with("new counter", Fun, 42, Map).
#{"counter" => 1,"new counter" => 42}
```
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec update_with(Key, Fun, Init, Map1) -> Map2 when
      Map1 :: #{Key => Value1, _ => _},
      Map2 :: #{Key := Value2 | Init, _ => _},
      Fun :: fun((Value1) -> Value2).

update_with(Key, Fun, Init, Map) when is_function(Fun, 1), is_map(Map) ->
    case Map of
        #{Key := Value} -> Map#{Key := Fun(Value)};
        #{} -> Map#{Key => Init}
    end;
update_with(Key, Fun, Init, Map) ->
    error_with_info(error_type(Map), [Key,Fun,Init,Map]).


-doc """
Returns the value associated with key `Key` in `Map`, or `Default` if
`Key` is not present in the map.

The call fails with a `{badmap,Map}` exception if `Map` is not a map.

## Examples

```erlang
1> Map = #{key1 => val1, key2 => val2}.
#{key1 => val1,key2 => val2}
2> maps:get(key1, Map, "Default value").
val1
3> maps:get(key3, Map, "Default value").
"Default value"
```
""".
-doc(#{since => <<"OTP 17.1">>}).
-spec get(Key, Map, Default) -> Value | Default when
      Map :: #{Key => Value, _ => _}.

get(Key, Map, Default) when is_map(Map) ->
    case Map of
        #{Key := Value} -> Value;
        #{} -> Default
    end;
get(Key, Map, Default) ->
    error_with_info({badmap,Map}, [Key,Map,Default]).

-doc """
Returns a map `Map` where each key-value pair from `MapOrIter` satisfies
the predicate `Pred(Key, Value)`.

Unless `MapOrIter` is an ordered iterator returned by `iterator/2`,
the order of the `Pred(Key, Value)` calls is not defined.

The call fails with a `{badmap,Map}` exception if `MapOrIter` is not a map or
valid iterator, or with `badarg` if `Pred` is not a function of arity 2.

## Examples

```erlang
1> M = #{a => 2, b => 3, "a" => 1, "b" => 2}.
2> Pred = fun(K, V) -> is_atom(K) andalso V rem 2 =:= 0 end.
3> maps:filter(Pred, M).
#{a => 2}
```
""".
-doc(#{since => <<"OTP 18.0">>}).
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

-doc """
Calls `Fun(Key, Value1)` on each key-value pair of `MapOrIter` to
update or remove associations from `MapOrIter`.

If `Fun(Key, Value1)` returns `true`, the association is copied to the result
map. If it returns `false`, the association is not copied. If it returns
`{true, NewValue}`, the value for `Key` is replaced with `NewValue` in the
result map.

Unless `MapOrIter` is an ordered iterator returned by `iterator/2`,
the order of the `Fun(Key, Value1)` calls is not defined.

The call fails with a `{badmap,Map}` exception if `MapOrIter` is not a map or
valid iterator, or with `badarg` if `Fun` is not a function of arity 2.

## Examples

```erlang
1> Fun = fun(K, V) when is_atom(K) -> {true, V*2};
            (_, V) -> V rem 2 =:= 0
   end.
2> Map = #{k1 => 1, "k2" => 2, "k3" => 3}.
3> maps:filtermap(Fun, Map).
#{k1 => 2,"k2" => 2}
```
""".
-doc(#{since => <<"OTP 24.0">>}).
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

-doc """
Calls `Fun(Key, Value)` for every `Key` to `Value` association in
`MapOrIter`.

Unless `MapOrIter` is an ordered iterator returned by `iterator/2`,
the order of the `Fun(Key, Value)` calls is not defined.

The call fails with a `{badmap,Map}` exception if `MapOrIter` is not a map or
valid iterator, or with `badarg` if `Fun` is not a function of arity 2.

## Examples

```erlang
1> Fun = fun(K, V) -> self() ! {K,V} end.
2> Map = #{p => 1, q => 2,x => 10, y => 20, z => 30}.
3> maps:foreach(Fun, maps:iterator(Map, ordered)).
ok
4> [receive X -> X end || _ <- [1,2,3,4,5]].
[{p,1},{q,2},{x,10},{y,20},{z,30}]
```
""".
-doc(#{since => <<"OTP 24.0">>}).
-spec foreach(Fun, MapOrIter) -> ok when
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

-doc """
Calls `Fun(Key, Value, AccIn)` for every `Key` to value `Value` association in
`MapOrIter`, starting with `AccIn` bound to `Acc0`.

The `Fun/3` fun must return a new accumulator, which is passed to the
next call. The function returns the final value of the
accumulator. The initial accumulator value `Init` is returned if the
map is empty.

Unless `MapOrIter` is an ordered iterator returned by `iterator/2`,
the order of the `Fun(Key, Value, AccIn)` calls is not defined.

The call fails with a `{badmap,Map}` exception if `MapOrIter` is not a
map or valid iterator, or with `badarg` if `Fun` is not a function of
arity 3.

## Examples

```erlang
1> Fun = fun(K, V, AccIn) -> AccIn + V end.
2> Map = #{k1 => 1, k2 => 2, k3 => 3}.
3> maps:fold(Fun, 0, Map).
6
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec fold(Fun, Init, MapOrIter) -> Acc when
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

-doc """
Produces a new map `Map` by calling function `Fun(Key, Value1)` for every
`Key` to value `Value1` association in `MapOrIter`.

The `Fun/2` fun must return value `Value2` to be associated with key
`Key` for the new map `Map`.

Unless `MapOrIter` is an ordered iterator returned by `iterator/2`,
the order of the `Fun(Key, Value1)` calls is not defined.

The call fails with a `{badmap,Map}` exception if `MapOrIter` is not a map or
valid iterator, or with `badarg` if `Fun` is not a function of arity 2.

## Examples

```erlang
1> Fun = fun(K,V1) when is_list(K) -> V1*2 end.
2> Map = #{"k1" => 1, "k2" => 2, "k3" => 3}.
3> maps:map(Fun, Map).
#{"k1" => 2,"k2" => 4,"k3" => 6}
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec map(Fun, MapOrIter) -> Map when
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

-doc """
Returns the number of key-value associations in `Map`.

This operation occurs in constant time.

## Examples

```erlang
1> Map = #{42 => value_two,1337 => "value one","a" => 1}.
2> maps:size(Map).
3
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec size(Map) -> non_neg_integer() when
    Map :: map().

size(Map) ->
    try
        map_size(Map)
    catch
        _:_ ->
            error_with_info({badmap,Map}, [Map])
    end.

-doc """
Returns a map iterator `Iterator` that can be used by [`maps:next/1`](`next/1`)
to traverse the key-value associations in a map.

The order of iteration is undefined. When iterating over a map, the
memory usage is guaranteed to be bounded no matter the size of the
map.

The call fails with a `{badmap,Map}` exception if `Map` is not a map.

## Examples

```erlang
1> M = #{ "foo" => 1, "bar" => 2 }.
#{"foo" => 1,"bar" => 2}
2> I = maps:iterator(M).
3> {K1, V1, I2} = maps:next(I), {K1, V1}.
{"bar",2}
4> {K2, V2, I3} = maps:next(I2),{K2, V2}.
{"foo",1}
5> maps:next(I3).
none
```
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec iterator(Map) -> Iterator when
      Map :: #{Key => Value},
      Iterator :: iterator(Key, Value).

iterator(M) when is_map(M) -> iterator(M, undefined);
iterator(M) -> error_with_info({badmap, M}, [M]).

-doc """
Returns a map iterator `Iterator` that can be used by [`maps:next/1`](`next/1`)
to traverse the key-value associations in a map sorted by key using the given
`Order`.

The call fails with a `{badmap,Map}` exception if `Map` is not a map, or
with a `badarg` exception if `Order` is invalid.

## Examples

Ordered iterator:

```erlang
1> M = #{a => 1, b => 2}.
2> OrdI = maps:iterator(M, ordered).
3> {K1, V1, OrdI2} = maps:next(OrdI), {K1, V1}.
{a,1}
4> {K2, V2, OrdI3} = maps:next(OrdI2),{K2, V2}.
{b,2}
5> maps:next(OrdI3).
none
```

Iterator ordered in reverse:

```erlang
1> M = #{a => 1, b => 2}.
2> RevI = maps:iterator(M, reversed).
3> {K2, V2, RevI2} = maps:next(RevI), {K2, V2}.
{b,2}
4> {K1, V1, RevI3} = maps:next(RevI2),{K1, V1}.
{a,1}
5> maps:next(RevI3).
none
6> maps:to_list(RevI).
[{b,2},{a,1}]
```

Using a custom ordering function that orders binaries by size:

```erlang
1> M = #{<<"abcde">> => d, <<"y">> => b, <<"x">> => a, <<"pqr">> => c}.
2> SizeI = fun(A, B) when byte_size(A) < byte_size(B) -> true;
              (A, B) when byte_size(A) > byte_size(B) -> false;
              (A, B) -> A =< B
           end.
3> SizeOrdI = maps:iterator(M, SizeI).
4> maps:to_list(SizeOrdI).
[{<<"x">>,a},{<<"y">>,b},{<<"pqr">>,c},{<<"abcde">>,d}]
```
""".
-doc(#{since => <<"OTP 26.0">>}).
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

-doc """
Returns the next key-value association in `Iterator` and a new iterator for the
remaining associations in the iterator.

If there are no more associations in the iterator, `none` is returned.

## Examples

```erlang
1> Map = #{a => 1, b => 2, c => 3}.
#{a => 1,b => 2,c => 3}
2> I = maps:iterator(Map, ordered).
3> {K1, V1, I1} = maps:next(I), {K1, V1}.
{a,1}
4> {K2, V2, I2} = maps:next(I1), {K2, V2}.
{b,2}
5> {K3, V3, I3} = maps:next(I2), {K3, V3}.
{c,3}
6> maps:next(I3).
none
```
""".
-doc(#{since => <<"OTP 21.0">>}).
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

-doc """
Returns a new map `Map2` without keys `K1` through `Kn` and their associated
values from map `Map1`.

Any key in `Ks` that does not exist in `Map1` is ignored.

## Examples

```erlang
1> Map = #{42 => value_three, 1337 => "value two", "a" => 1}.
2> Keys = ["a",42,"other key"].
3> maps:without(Keys, Map).
#{1337 => "value two"}
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec without(Ks, Map1) -> Map2 when
    Ks :: [K],
    Map1 :: map(),
    Map2 :: map(),
    K :: term().

without(Ks, M) when is_list(Ks), is_map(M) ->
    lists:foldl(fun maps:remove/2, M, Ks);
without(Ks, M) ->
    error_with_info(error_type(M), [Ks,M]).

-doc """
Returns a new map `Map2` with the keys `K1` through `Kn` and their associated
values from map `Map1`.

Any key in `Ks` that does not exist in `Map1` is ignored.

## Examples

```erlang
1> Map = #{42 => value_three,1337 => "value two","a" => 1}.
2> Keys = ["a",42,"other key"].
3> maps:with(Keys, Map).
#{42 => value_three,"a" => 1}
```
""".
-doc(#{since => <<"OTP 17.3">>}).
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

-doc """
Partitions the given `List` into a map of groups.

The result is a map where each key is given by `KeyFun` and each value is a list
of elements from the given `List` for which `KeyFun` returned the same key.

The order of elements within each group list is preserved from the original
list.

## Examples

```erlang
1> EvenOdd = fun(X) when X rem 2 =:= 0 -> even;
                (_) -> odd
             end.
2> maps:groups_from_list(EvenOdd, [1, 2, 3]).
#{even => [2], odd => [1, 3]}
3> maps:groups_from_list(fun length/1, ["ant", "buffalo", "cat", "dingo"]).
#{3 => ["ant", "cat"], 5 => ["dingo"], 7 => ["buffalo"]}
```
""".
-doc(#{since => <<"OTP 25.0">>}).
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

-doc """
Partitions the given `List` into a map of groups.

The result is a map where each key is given by `KeyFun` and each value is a list
of elements from the given `List`, mapped via `ValueFun`, for which `KeyFun`
returned the same key.

The order of elements within each group list is preserved from the original
list.

## Examples

```erlang
1> EvenOdd = fun(X) -> case X rem 2 of 0 -> even; 1 -> odd end end.
2> Square = fun(X) -> X * X end.
3> maps:groups_from_list(EvenOdd, Square, [1, 2, 3]).
#{even => [4], odd => [1, 9]}
4> maps:groups_from_list(
    fun length/1,
    fun lists:reverse/1,
    ["ant", "buffalo", "cat", "dingo"]).
#{3 => ["tna", "tac"],5 => ["ognid"],7 => ["olaffub"]}
```
""".
-doc(#{since => <<"OTP 25.0">>}).
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

-doc false.
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
