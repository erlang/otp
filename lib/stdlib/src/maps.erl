%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2024. All Rights Reserved.
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
`reversed`, or a custom sorting function.

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

_Example:_

```erlang
> Key = 1337,
  Map = #{42 => value_two,1337 => "value one","a" => 1},
  maps:get(Key,Map).
"value one"
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec get(Key,Map) -> Value when
    Key :: term(),
    Map :: map(),
    Value :: term().

get(_,_) -> erlang:nif_error(undef).

-doc """
Returns a tuple `{ok, Value}`, where `Value` is the value associated with `Key`,
or `error` if no value is associated with `Key` in `Map`.

The call fails with a `{badmap,Map}` exception if `Map` is not a map.

_Example:_

```erlang
> Map = #{"hi" => 42},
  Key = "hi",
  maps:find(Key,Map).
{ok,42}
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec find(Key,Map) -> {ok, Value} | error when
    Map :: #{Key => Value, _ => _}.

find(_,_) -> erlang:nif_error(undef).

%% Shadowed by erl_bif_types: maps:from_list/1
-doc """
Takes a list of key-value tuples elements and builds a map. The associations can
be in any order, and both keys and values in the association can be of any term.


If the same key appears more than once, the latter (right-most) value is used
and the previous values are ignored.

_Example:_

```erlang
> List = [{"a",ignored},{1337,"value two"},{42,value_three},{"a",1}],
  maps:from_list(List).
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
Takes a list of keys and a value and builds a map where all keys point to the
same value. The key can be in any order, and keys and value can be of any term.

_Example:_

```erlang
> Keys = ["a", "b", "c"], maps:from_keys(Keys, ok).
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
Intersects two maps into a single map `Map3`. If a key exists in both maps, the
value in `Map1` is superseded by the value in `Map2`.

The call fails with a `{badmap,Map}` exception if `Map1` or `Map2` is not a map.

_Example:_

```erlang
> Map1 = #{a => "value_one", b => "value_two"},
  Map2 = #{a => 1, c => 2},
  maps:intersect(Map1,Map2).
#{a => 1}
```
""".
-doc(#{since => <<"OTP 24.0">>}).
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

-doc """
Intersects two maps into a single map `Map3`. If a key exists in both maps, the
value in `Map1` is combined with the value in `Map2` by the `Combiner` fun.

When `Combiner` is applied the key that exists in both maps is the first parameter,
the value from `Map1` is the second parameter, and the value from `Map2` is the
third parameter.

The call fails with a `{badmap,Map}` exception if `Map1` or `Map2` is not a map.
The call fails with a `badarg` exception if `Combiner` is not a fun that takes
three arguments.

_Example:_

```erlang
> Map1 = #{a => "value_one", b => "value_two"},
  Map2 = #{a => 1, c => 2},
  maps:intersect_with(fun(_Key, Value1, Value2) -> {Value1, Value2} end, Map1, Map2).
#{a => {"value_one",1}}
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
-doc """
Returns `true` if map `Map` contains `Key` and returns `false` if it does not
contain the `Key`.

The call fails with a `{badmap,Map}` exception if `Map` is not a map.

_Example:_

```erlang
> Map = #{"42" => value}.
#{"42" => value}
> maps:is_key("42",Map).
true
> maps:is_key(value,Map).
false
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec is_key(Key,Map) -> boolean() when
    Key :: term(),
    Map :: map().

is_key(_,_) -> erlang:nif_error(undef).


-doc """
Returns a complete list of keys, in any order, which resides within `Map`.

The call fails with a `{badmap,Map}` exception if `Map` is not a map.

_Example:_

```erlang
> Map = #{42 => value_three,1337 => "value two","a" => 1},
  maps:keys(Map).
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
Merges two maps into a single map `Map3`. If two keys exist in both maps, the
value in `Map1` is superseded by the value in `Map2`.

The call fails with a `{badmap,Map}` exception if `Map1` or `Map2` is not a map.

_Example:_

```erlang
> Map1 = #{a => "value_one", b => "value_two"},
  Map2 = #{a => 1, c => 2},
  maps:merge(Map1,Map2).
#{a => 1,b => "value_two",c => 2}
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec merge(Map1,Map2) -> Map3 when
    Map1 :: map(),
    Map2 :: map(),
    Map3 :: map().

merge(_,_) -> erlang:nif_error(undef).

-doc """
Merges two maps into a single map `Map3`. If a key exists in both maps, the
value in `Map1` is combined with the value in `Map2` by the `Combiner` fun.

When `Combiner` is applied the key that exists in both maps is the first parameter,
the value from `Map1` is the second parameter, and the value from `Map2` is the
third parameter.

The call fails with a `{badmap,Map}` exception if `Map1` or `Map2` is not a map.
The call fails with a `badarg` exception if `Combiner` is not a fun that takes
three arguments.

_Example:_

```erlang
> Map1 = #{a => "value_one", b => "value_two"},
  Map2 = #{a => 1, c => 2},
  maps:merge_with(fun(_Key, Value1, Value2) -> {Value1, Value2} end, Map1, Map2).
#{a => {"value_one",1},b => "value_two",c => 2}
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
-doc """
Associates `Key` with value `Value` and inserts the association into map `Map2`.
If key `Key` already exists in map `Map1`, the old associated value is replaced
by value `Value`. The function returns a new map `Map2` containing the new
association and the old associations in `Map1`.

The call fails with a `{badmap,Map}` exception if `Map1` is not a map.

_Example:_

```erlang
> Map = #{"a" => 1}.
#{"a" => 1}
> maps:put("a", 42, Map).
#{"a" => 42}
> maps:put("b", 1337, Map).
#{"a" => 1,"b" => 1337}
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec put(Key,Value,Map1) -> Map2 when
    Key :: term(),
    Value :: term(),
    Map1 :: map(),
    Map2 :: map().

put(_,_,_) -> erlang:nif_error(undef).


%% Shadowed by erl_bif_types: maps:remove/2
-doc """
Removes the `Key`, if it exists, and its associated value from `Map1` and
returns a new map `Map2` without key `Key`.

The call fails with a `{badmap,Map}` exception if `Map1` is not a map.

_Example:_

```erlang
> Map = #{"a" => 1}.
#{"a" => 1}
> maps:remove("a",Map).
#{}
> maps:remove("b",Map).
#{"a" => 1}
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec remove(Key,Map1) -> Map2 when
    Key :: term(),
    Map1 :: map(),
    Map2 :: map().

remove(_,_) -> erlang:nif_error(undef).

-doc """
The function removes the `Key`, if it exists, and its associated value from
`Map1` and returns a tuple with the removed `Value` and the new map `Map2`
without key `Key`. If the key does not exist `error` is returned.

The call will fail with a `{badmap,Map}` exception if `Map1` is not a map.

Example:

```erlang
> Map = #{"a" => "hello", "b" => "world"}.
#{"a" => "hello", "b" => "world"}
> maps:take("a",Map).
{"hello",#{"b" => "world"}}
> maps:take("does not exist",Map).
error
```
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec take(Key,Map1) -> {Value,Map2} | error when
    Map1 :: #{Key => Value, _ => _},
    Map2 :: #{_ => _}.

take(_,_) -> erlang:nif_error(undef).

-doc """
Returns a list of pairs representing the key-value associations of
`MapOrIterator`, where the pairs `[{K1,V1}, ..., {Kn,Vn}]` are returned in
arbitrary order.

The call fails with a `{badmap,Map}` exception if `MapOrIterator` is not a map
or an iterator obtained by a call to `iterator/1` or `iterator/2`.

_Example:_

```erlang
> Map = #{42 => value_three,1337 => "value two","a" => 1},
  maps:to_list(Map).
[{42,value_three},{1337,"value two"},{"a",1}]
```

_Example (using _`iterator/2`_):_

```erlang
> Map = #{ z => 1, y => 2, x => 3 }.
#{x => 3,y => 2,z => 1}
> maps:to_list(maps:iterator(Map, ordered)).
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
If `Key` exists in `Map1`, the old associated value is replaced by value
`Value`. The function returns a new map `Map2` containing the new associated
value.

The call fails with a `{badmap,Map}` exception if `Map1` is not a map, or with a
`{badkey,Key}` exception if no value is associated with `Key`.

_Example:_

```erlang
> Map = #{"a" => 1}.
#{"a" => 1}
> maps:update("a", 42, Map).
#{"a" => 42}
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec update(Key,Value,Map1) -> Map2 when
    Map1 :: #{Key := _, _ => _},
    Map2 :: #{Key := Value, _ => _}.

update(_,_,_) -> erlang:nif_error(undef).


-doc """
Returns a complete list of values, in arbitrary order, contained in map `Map`.

The call fails with a `{badmap,Map}` exception if `Map` is not a map.

_Example:_

```erlang
> Map = #{42 => value_three,1337 => "value two","a" => 1},
  maps:values(Map).
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

_Example:_

```text
> maps:new().
#{}
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec new() -> Map when
    Map :: #{}.

new() -> #{}.

-doc """
Update a value in a `Map1` associated with `Key` by calling `Fun` on the old
value to get a new value. An exception `{badkey,Key}` is generated if `Key` is
not present in the map.

Example:

```erlang
> Map = #{"counter" => 1},
  Fun = fun(V) -> V + 1 end,
  maps:update_with("counter",Fun,Map).
#{"counter" => 2}
```
""".
-doc(#{since => <<"OTP 19.0">>}).
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


-doc """
Update a value in a `Map1` associated with `Key` by calling `Fun` on the old
value to get a new value. If `Key` is not present in `Map1` then `Init` will be
associated with `Key`.

Example:

```erlang
> Map = #{"counter" => 1},
  Fun = fun(V) -> V + 1 end,
  maps:update_with("new counter",Fun,42,Map).
#{"counter" => 1,"new counter" => 42}
```
""".
-doc(#{since => <<"OTP 19.0">>}).
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


-doc """
Returns value `Value` associated with `Key` if `Map` contains `Key`. If no value
is associated with `Key`, `Default` is returned.

The call fails with a `{badmap,Map}` exception if `Map` is not a map.

_Example:_

```erlang
> Map = #{ key1 => val1, key2 => val2 }.
#{key1 => val1,key2 => val2}
> maps:get(key1, Map, "Default value").
val1
> maps:get(key3, Map, "Default value").
"Default value"
```
""".
-doc(#{since => <<"OTP 17.1">>}).
-spec get(Key, Map, Default) -> Value | Default when
      Map :: #{Key => Value, _ => _}.

get(Key,Map,Default) when is_map(Map) ->
    case Map of
        #{Key := Value} -> Value;
        #{} -> Default
    end;
get(Key,Map,Default) ->
    error_with_info({badmap,Map}, [Key,Map,Default]).

-doc """
Returns a map `Map` for which predicate `Pred` holds true in `MapOrIter`.

The call fails with a `{badmap,Map}` exception if `MapOrIter` is not a map or
valid iterator, or with `badarg` if `Pred` is not a function of arity 2.

_Example:_

```erlang
> M = #{a => 2, b => 3, c=> 4, "a" => 1, "b" => 2, "c" => 4},
  Pred = fun(K,V) -> is_atom(K) andalso (V rem 2) =:= 0 end,
  maps:filter(Pred,M).
#{a => 2,c => 4}
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
Returns a map `Map` that is the result of calling `Fun(Key, Value1)` for every
`Key` to value `Value1` association in `MapOrIter` in any order.

If `Fun(Key, Value1)` returns `true`, the association is copied to the result
map. If it returns `false`, the association is not copied. If it returns
`{true, NewValue}`, the value for `Key` is replaced with `NewValue` in the
result map.

The call fails with a `{badmap,Map}` exception if `MapOrIter` is not a map or
valid iterator, or with `badarg` if `Fun` is not a function of arity 2.

_Example:_

```erlang
> Fun = fun(K,V) when is_atom(K) -> {true, V*2}; (_,V) -> (V rem 2) =:= 0 end,
  Map = #{k1 => 1, "k2" => 2, "k3" => 3},
  maps:filtermap(Fun,Map).
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
Calls `fun F(Key, Value)` for every `Key` to value `Value` association in
`MapOrIter` in any order.

The call fails with a `{badmap,Map}` exception if `MapOrIter` is not a map or
valid iterator, or with `badarg` if `Fun` is not a function of arity 2.
""".
-doc(#{since => <<"OTP 24.0">>}).
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

-doc """
Calls `F(Key, Value, AccIn)` for every `Key` to value `Value` association in
`MapOrIter` in any order. Function `fun F/3` must return a new accumulator,
which is passed to the next successive call. This function returns the final
value of the accumulator. The initial accumulator value `Init` is returned if
the map is empty.

The call fails with a `{badmap,Map}` exception if `MapOrIter` is not a map or
valid iterator, or with `badarg` if `Fun` is not a function of arity 3.

_Example:_

```erlang
> Fun = fun(K,V,AccIn) when is_list(K) -> AccIn + V end,
  Map = #{"k1" => 1, "k2" => 2, "k3" => 3},
  maps:fold(Fun,0,Map).
6
```
""".
-doc(#{since => <<"OTP 17.0">>}).
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

-doc """
Produces a new map `Map` by calling function `fun F(Key, Value1)` for every
`Key` to value `Value1` association in `MapOrIter` in any order. Function
`fun Fun/2` must return value `Value2` to be associated with key `Key` for the
new map `Map`.

The call fails with a `{badmap,Map}` exception if `MapOrIter` is not a map or
valid iterator, or with `badarg` if `Fun` is not a function of arity 2.

_Example:_

```erlang
> Fun = fun(K,V1) when is_list(K) -> V1*2 end,
  Map = #{"k1" => 1, "k2" => 2, "k3" => 3},
  maps:map(Fun,Map).
#{"k1" => 2,"k2" => 4,"k3" => 6}
```
""".
-doc(#{since => <<"OTP 17.0">>}).
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

-doc """
Returns the number of key-value associations in `Map`. This operation occurs in
constant time.

_Example:_

```erlang
> Map = #{42 => value_two,1337 => "value one","a" => 1},
  maps:size(Map).
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
to traverse the key-value associations in a map. When iterating over a map, the
memory usage is guaranteed to be bounded no matter the size of the map.

The call fails with a `{badmap,Map}` exception if `Map` is not a map.

_Example:_

```erlang
> M = #{ a => 1, b => 2 }.
#{a => 1,b => 2}
> I = maps:iterator(M), ok.
ok
> {K1, V1, I2} = maps:next(I), {K1, V1}.
{a,1}
> {K2, V2, I3} = maps:next(I2),{K2, V2}.
{b,2}
> maps:next(I3).
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

The call fails with a `{badmap,Map}` exception if `Map` is not a map or if
`Order` is invalid.

_Example (when _`Order`_ is _`ordered`_):_

```erlang
> M = #{ a => 1, b => 2 }.
#{a => 1,b => 2}
> OrdI = maps:iterator(M, ordered), ok.
ok
> {K1, V1, OrdI2} = maps:next(OrdI), {K1, V1}.
{a,1}
> {K2, V2, OrdI3} = maps:next(OrdI2),{K2, V2}.
{b,2}
> maps:next(OrdI3).
none
```

_Example (when _`Order`_ is _`reversed`_):_

```erlang
> M = #{ a => 1, b => 2 }.
#{a => 1,b => 2}
> RevI = maps:iterator(M, reversed), ok.
ok
> {K2, V2, RevI2} = maps:next(RevI), {K2, V2}.
{b,2}
> {K1, V1, RevI3} = maps:next(RevI2),{K1, V1}.
{a,1}
> maps:next(RevI3).
none
```

_Example (when _`Order`_ is an arithmetic sorting function):_

```erlang
> M = #{ -1 => a, -1.0 => b, 0 => c, 0.0 => d }.
#{-1 => a,0 => c,-1.0 => b,0.0 => d}
> ArithOrdI = maps:iterator(M, fun(A, B) -> A =< B end), ok.
ok
> maps:to_list(ArithOrdI).
[{-1,a},{-1.0,b},{0,c},{0.0,d}]
> ArithRevI = maps:iterator(M, fun(A, B) -> B < A end), ok.
ok
> maps:to_list(ArithRevI).
[{0.0,d},{0,c},{-1.0,b},{-1,a}]
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

_Example:_

```erlang
> Map = #{a => 1, b => 2, c => 3}.
#{a => 1,b => 2,c => 3}
> I = maps:iterator(Map), ok.
ok
> {K1, V1, I1} = maps:next(I), {K1, V1}.
{a,1}
> {K2, V2, I2} = maps:next(I1), {K2, V2}.
{b,2}
> {K3, V3, I3} = maps:next(I2), {K3, V3}.
{c,3}
> maps:next(I3).
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
values from map `Map1`. Any key in `Ks` that does not exist in `Map1` is ignored

_Example:_

```erlang
> Map = #{42 => value_three,1337 => "value two","a" => 1},
  Ks = ["a",42,"other key"],
  maps:without(Ks,Map).
#{1337 => "value two"}
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec without(Ks,Map1) -> Map2 when
    Ks :: [K],
    Map1 :: map(),
    Map2 :: map(),
    K :: term().

without(Ks,M) when is_list(Ks), is_map(M) ->
    lists:foldl(fun maps:remove/2, M, Ks);
without(Ks,M) ->
    error_with_info(error_type(M), [Ks,M]).

-doc """
Returns a new map `Map2` with the keys `K1` through `Kn` and their associated
values from map `Map1`. Any key in `Ks` that does not exist in `Map1` is
ignored.

_Example:_

```erlang
> Map = #{42 => value_three,1337 => "value two","a" => 1},
  Ks = ["a",42,"other key"],
  maps:with(Ks,Map).
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

%% groups_from_list/2 & groups_from_list/3

-doc """
Partitions the given `List` into a map of groups.

The result is a map where each key is given by `KeyFun` and each value is a list
of elements from the given `List` for which `KeyFun` returned the same key.

The order of elements within each group list is preserved from the original
list.

_Examples:_

```erlang
> EvenOdd = fun(X) -> case X rem 2 of 0 -> even; 1 -> odd end end,
maps:groups_from_list(EvenOdd, [1, 2, 3]).
#{even => [2], odd => [1, 3]}
> maps:groups_from_list(fun erlang:length/1, ["ant", "buffalo", "cat", "dingo"]).
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

_Examples:_

```erlang
> EvenOdd = fun(X) -> case X rem 2 of 0 -> even; 1 -> odd end end,
> Square = fun(X) -> X * X end,
> maps:groups_from_list(EvenOdd, Square, [1, 2, 3]).
#{even => [4], odd => [1, 9]}
> maps:groups_from_list(
    fun erlang:length/1,
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
