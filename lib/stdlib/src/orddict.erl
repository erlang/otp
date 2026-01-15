%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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

-module(orddict).
-moduledoc """
Key-value dictionary as ordered list.

This module provides a `Key`-`Value` dictionary. An `orddict` is a
representation of a dictionary, where a list of pairs is used to store the keys
and values. The list is ordered after the keys in the
[Erlang term order](`e:system:expressions.md#term-comparisons`).

This module provides the same interface as the `m:dict` module but with a
defined representation. One difference is that while `dict` considers two keys
as different if they do not match (`=:=`), this module considers two keys as
different if and only if they do not compare equal (`==`).

## Notes

[](){: #notes }

Functions [`append/3`](`append/3`) and [`append_list/3`](`append_list/3`) are
included so that keyed values can be stored in a list _accumulator_, for
example:

```erlang
> D0 = orddict:new(),
  D1 = orddict:store(files, [], D0),
  D2 = orddict:append(files, f1, D1),
  D3 = orddict:append(files, f2, D2),
  D4 = orddict:append(files, f3, D3),
  orddict:fetch(files, D4).
[f1,f2,f3]
```

This saves the trouble of first fetching a keyed value, appending a new value to
the list of stored values, and storing the result.

Function [`fetch/2`](`fetch/2`) is to be used if the key is known to be in the
dictionary, otherwise function [`find/2`](`find/2`).

## See Also

`m:dict`, `m:gb_trees`
""".

%% Standard interface.
-export([new/0,is_key/2,to_list/1,from_list/1,size/1,is_empty/1]).
-export([fetch/2,find/2,fetch_keys/1,erase/2,take/2]).
-export([store/3,append/3,append_list/3,update/3,update/4,update_counter/3]).
-export([fold/3,map/2,filter/2,merge/3]).

-export_type([orddict/0, orddict/2]).

%%---------------------------------------------------------------------------

-type orddict() :: orddict(_, _).

-doc "Dictionary as returned by `new/0`.".
-type orddict(Key, Value) :: [{Key, Value}].

%%---------------------------------------------------------------------------

-doc "Creates a new dictionary.".
-spec new() -> orddict(none(), none()).

new() -> [].

-doc "Tests if `Key` is contained in dictionary `Orddict`.".
-spec is_key(Key, Orddict) -> boolean() when
      Orddict :: orddict(Key, Value :: term()).

is_key(Key, [{K,_}|_]) when Key < K -> false;
is_key(Key, [{K,_}|Dict]) when Key > K -> is_key(Key, Dict);
is_key(_Key, [{_K,_Val}|_]) -> true;		%Key == K
is_key(_, []) -> false.

-doc "Converts a dictionary to a list representation.".
-spec to_list(Orddict) -> List when
      Orddict :: orddict(Key, Value),
      List :: [{Key, Value}].

to_list(Dict) -> Dict.

-doc "Converts the `Key`-`Value` list `List` to a dictionary.".
-spec from_list(List) -> Orddict when
      List :: [{Key, Value}],
      Orddict :: orddict(Key, Value).

from_list([]) -> [];
from_list([{_,_}]=Pair) -> Pair;
from_list(Pairs) ->
    lists:ukeysort(1, reverse_pairs(Pairs, [])).

-doc "Returns the number of elements in an `Orddict`.".
-spec size(Orddict) -> non_neg_integer() when
      Orddict :: orddict().

size(D) -> length(D).

-doc "Returns `true` if `Orddict` has no elements, otherwise `false`.".
-doc(#{since => <<"OTP 17.0">>}).
-spec is_empty(Orddict) -> boolean() when
      Orddict :: orddict().

is_empty([]) -> true;
is_empty([_|_]) -> false.

-doc """
Returns the value associated with `Key` in dictionary `Orddict`. This function
assumes that the `Key` is present in the dictionary. An exception is generated
if `Key` is not in the dictionary.

See also section [Notes](`m:orddict#module-notes`).

_Example:_

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:fetch(a, OrdDict1).
1
3> orddict:fetch(missing, OrdDict1).
** exception error: no function clause matching orddict:fetch(missing,[])
```
""".
-spec fetch(Key, Orddict) -> Value when
      Orddict :: orddict(Key, Value).

fetch(Key, [{K,_}|D]) when Key > K -> fetch(Key, D);
fetch(Key, [{K,Value}|_]) when Key == K -> Value.

-doc """
Searches for a key in a dictionary. Returns `{ok, Value}`, where `Value` is the
value associated with `Key`, or `error` if the key is not present in the
dictionary.

See also section [Notes](`m:orddict#module-notes`).

_Example:_

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:find(a, OrdDict1).
{ok,1}
3> orddict:find(c, OrdDict1).
error
```
""".
-spec find(Key, Orddict) -> {'ok', Value} | 'error' when
      Orddict :: orddict(Key, Value).

find(Key, [{K,_}|_]) when Key < K -> error;
find(Key, [{K,_}|D]) when Key > K -> find(Key, D);
find(_Key, [{_K,Value}|_]) -> {ok,Value};	%Key == K
find(_, []) -> error.

-doc """
Returns a list of all keys in a dictionary.

_Example:_

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:fetch_keys(OrdDict1).
[a,b]
```
""".
-spec fetch_keys(Orddict) -> Keys when
      Orddict :: orddict(Key, Value :: term()),
      Keys :: [Key].

fetch_keys([{Key,_}|Dict]) ->
    [Key|fetch_keys(Dict)];
fetch_keys([]) -> [].

-doc """
Erases all items with a specified key from a dictionary.

_Example:_

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:erase(a, OrdDict1).
[{b,2}]
```
""".
-spec erase(Key, Orddict1) -> Orddict2 when
      Orddict1 :: orddict(Key, Value),
      Orddict2 :: orddict(Key, Value).

erase(Key, [{K,_}=E|Dict]) when Key < K -> [E|Dict];
erase(Key, [{K,_}=E|Dict]) when Key > K ->
    [E|erase(Key, Dict)];
erase(_Key, [{_K,_Val}|Dict]) -> Dict;		%Key == K
erase(_, []) -> [].

-doc """
This function returns value from dictionary and new dictionary without this
value. Returns `error` if the key is not present in the dictionary.

_Example:_

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:take(a, OrdDict1).
{1,[{b,2}]}
3> orddict:take(missing, OrdDict1).
error
```
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec take(Key, Orddict) -> {Value, Orddict1} | error when
      Orddict :: orddict(Key, Value),
      Orddict1 :: orddict(Key, Value),
      Key :: term(),
      Value :: term().

take(Key, Dict) ->
    take_1(Key, Dict, []).

take_1(Key, [{K,_}|_], _Acc) when Key < K ->
    error;
take_1(Key, [{K,_}=P|D], Acc) when Key > K ->
    take_1(Key, D, [P|Acc]);
take_1(_Key, [{_K,Value}|D], Acc) ->
    {Value,lists:reverse(Acc, D)};
take_1(_, [], _) -> error.

-doc """
Stores a `Key`-`Value` pair in a dictionary. If the `Key` already exists in
`Orddict1`, the associated value is replaced by `Value`.

_Example:_

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:store(a, 99, OrdDict1).
[{a,99},{b,2}]
3> orddict:store(c, 100, OrdDict1).
[{a,1},{b,2},{c,100}]
```
""".
-spec store(Key, Value, Orddict1) -> Orddict2 when
      Orddict1 :: orddict(Key, Value),
      Orddict2 :: orddict(Key, Value).

store(Key, New, [{K,_}|_]=Dict) when Key < K ->
    [{Key,New}|Dict];
store(Key, New, [{K,_}=E|Dict]) when Key > K ->
    [E|store(Key, New, Dict)];
store(Key, New, [{_K,_Old}|Dict]) ->		%Key == K
    [{Key,New}|Dict];
store(Key, New, []) -> [{Key,New}].

-doc """
Appends a new `Value` to the current list of values associated with `Key`. An
exception is generated if the initial value associated with `Key` is not a list
of values.

See also section [Notes](`m:orddict#module-notes`).

_Example 1:_

```erlang
1> OrdDict1 = orddict:from_list([{x, []}]).
[{x,[]}]
2> OrdDict2 = orddict:append(x, 1, OrdDict1).
[{x,[1]}]
3> OrdDict3 = orddict:append(x, 2, OrdDict2).
[{x,[1,2]}]
4> orddict:append(y, 3, OrdDict3).
[{x,[1,2]},{y,[3]}]
```

_Example 2:_

```erlang
1> OrdDict1 = orddict:from_list([{a, no_list}]).
[{a,no_list}]
2> orddict:append(a, 1, OrdDict1).
** exception error: bad argument
     in operator  ++/2
        called as no_list ++ [1]
```
""".
-spec append(Key, Value, Orddict1) -> Orddict2 when
      Orddict1 :: orddict(Key, Value),
      Orddict2 :: orddict(Key, Value).

append(Key, New, [{K,_}|_]=Dict) when Key < K ->
    [{Key,[New]}|Dict];
append(Key, New, [{K,_}=E|Dict]) when Key > K ->
    [E|append(Key, New, Dict)];
append(Key, New, [{_K,Old}|Dict]) ->		%Key == K
    [{Key,Old ++ [New]}|Dict];
append(Key, New, []) -> [{Key,[New]}].

-doc """
Appends a list of values `ValList` to the current list of values associated with
`Key`. An exception is generated if the initial value associated with `Key` is
not a list of values.

See also section [Notes](`m:orddict#module-notes`).

_Example:_

```erlang
1> OrdDict1 = orddict:from_list([{x, []}]).
[{x,[]}]
2> OrdDict2 = orddict:append_list(x, [1,2], OrdDict1).
[{x,[1,2]}]
3> OrdDict3 = orddict:append_list(y, [3,4], OrdDict2).
[{x,[1,2]},{y,[3,4]}]
```
""".
-spec append_list(Key, ValList, Orddict1) -> Orddict2 when
      ValList :: [Value],
      Orddict1 :: orddict(Key, Value),
      Orddict2 :: orddict(Key, Value).

append_list(Key, NewList, [{K,_}|_]=Dict) when Key < K ->
    [{Key,NewList}|Dict];
append_list(Key, NewList, [{K,_}=E|Dict]) when Key > K ->
    [E|append_list(Key, NewList, Dict)];
append_list(Key, NewList, [{_K,Old}|Dict]) ->		%Key == K
    [{Key,Old ++ NewList}|Dict];
append_list(Key, NewList, []) ->
    [{Key,NewList}].

-doc """
Updates a value in a dictionary by calling `Fun` on the value to get a new
value. An exception is generated if `Key` is not present in the dictionary.

_Example:_

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:update(a, fun (V) -> V + 100 end, OrdDict1).
[{a,101},{b,2}]
```
""".
-spec update(Key, Fun, Orddict1) -> Orddict2 when
      Fun :: fun((Value1 :: Value) -> Value2 :: Value),
      Orddict1 :: orddict(Key, Value),
      Orddict2 :: orddict(Key, Value).

update(Key, Fun, [{K,_}=E|Dict]) when Key > K ->
    [E|update(Key, Fun, Dict)];
update(Key, Fun, [{K,Val}|Dict]) when Key == K ->
    [{Key,Fun(Val)}|Dict].

-doc """
Updates a value in a dictionary by calling `Fun` on the value to get a new
value. If `Key` is not present in the dictionary, `Initial` is stored as the
first value.

For example, [`append/3`](`append/3`) can be defined as follows:

```erlang
append(Key, Val, D) ->
    update(Key, fun (Old) -> Old ++ [Val] end, [Val], D).
```

_Example 1:_

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:update(c, fun (V) -> V + 100 end, 99, OrdDict1).
[{a,1},{b,2},{c,99}]
```

_Example 2:_

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:update(a, fun (V) -> V + 100 end, 99, OrdDict1).
[{a,101},{b,2}]
```
""".
-spec update(Key, Fun, Initial, Orddict1) -> Orddict2 when
      Initial :: Value,
      Fun :: fun((Value1 :: Value) -> Value2 :: Value),
      Orddict1 :: orddict(Key, Value),
      Orddict2 :: orddict(Key, Value).

update(Key, _, Init, [{K,_}|_]=Dict) when Key < K ->
    [{Key,Init}|Dict];
update(Key, Fun, Init, [{K,_}=E|Dict]) when Key > K ->
    [E|update(Key, Fun, Init, Dict)];
update(Key, Fun, _Init, [{_K,Val}|Dict]) ->		%Key == K
    [{Key,Fun(Val)}|Dict];
update(Key, _, Init, []) -> [{Key,Init}].

-doc """
Adds `Increment` to the value associated with `Key` and store this value. If
`Key` is not present in the dictionary, `Increment` is stored as the first
value.

This can be defined as follows, but is faster:

```erlang
update_counter(Key, Incr, D) ->
    update(Key, fun (Old) -> Old + Incr end, Incr, D).
```
""".
-spec update_counter(Key, Increment, Orddict1) -> Orddict2 when
      Orddict1 :: orddict(Key, Value),
      Orddict2 :: orddict(Key, Value),
      Increment :: number().

update_counter(Key, Incr, [{K,_}|_]=Dict) when Key < K ->
    [{Key,Incr}|Dict];
update_counter(Key, Incr, [{K,_}=E|Dict]) when Key > K ->
    [E|update_counter(Key, Incr, Dict)];
update_counter(Key, Incr, [{_K,Val}|Dict]) ->		%Key == K
    [{Key,Val+Incr}|Dict];
update_counter(Key, Incr, []) -> [{Key,Incr}].

-doc """
Calls `Fun` on successive keys and values of `Orddict` together with an extra
argument `Acc` (short for accumulator). `Fun` must return a new accumulator that
is passed to the next call. `Acc0` is returned if the list is empty.

_Example:_

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:fold(fun (K, V, Acc) -> [{K, V+100} | Acc] end, [], OrdDict1).
[{b,102},{a,101}]
```
""".
-spec fold(Fun, Acc0, Orddict) -> Acc1 when
      Fun :: fun((Key, Value, AccIn) -> AccOut),
      Orddict :: orddict(Key, Value),
      Acc0 :: Acc,
      Acc1 :: Acc,
      AccIn :: Acc,
      AccOut :: Acc.

fold(F, Acc, [{Key,Val}|D]) ->
    fold(F, F(Key, Val, Acc), D);
fold(F, Acc, []) when is_function(F, 3) -> Acc.

-doc """
Calls `Fun` on successive keys and values of `Orddict1` to return a new value
for each key.

_Example:_

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:map(fun (_K, V) -> V + 100 end, OrdDict1).
[{a,101},{b,102}]
```
""".
-spec map(Fun, Orddict1) -> Orddict2 when
      Fun :: fun((Key, Value1) -> Value2),
      Orddict1 :: orddict(Key, Value1),
      Orddict2 :: orddict(Key, Value2).

map(F, [{Key,Val}|D]) ->
    [{Key,F(Key, Val)}|map(F, D)];
map(F, []) when is_function(F, 2) -> [].

-doc """
`Orddict2` is a dictionary of all keys and values in `Orddict1` for which
`Pred(Key, Value)` is `true`.

_Example:_

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> orddict:filter(fun (K, V) -> V > 1 end, OrdDict1).
[{b,2}]
```
""".
-spec filter(Pred, Orddict1) -> Orddict2 when
      Pred :: fun((Key, Value) -> boolean()),
      Orddict1 :: orddict(Key, Value),
      Orddict2 :: orddict(Key, Value).

filter(F, [{Key,Val}=E|D]) ->
    case F(Key, Val) of
	true -> [E|filter(F, D)]; 
	false -> filter(F, D)
    end;
filter(F, []) when is_function(F, 2) -> [].

-doc """
Merges two dictionaries, `Orddict1` and `Orddict2`, to create a new dictionary.
All the `Key`-`Value` pairs from both dictionaries are included in the new
dictionary.

If a key occurs in both dictionaries, `Fun` is called with the key
and both values to return a new value.

[`merge/3`](`merge/3`) can be defined as follows, but is faster:

```erlang
merge(Fun, D1, D2) ->
    fold(fun (K, V1, D) ->
                 update(K, fun (V2) -> Fun(K, V1, V2) end, V1, D)
         end, D2, D1).
```

_Example:_

```erlang
1> OrdDict1 = orddict:from_list([{a, 1}, {b, 2}]).
[{a,1},{b,2}]
2> OrdDict2 = orddict:from_list([{b, 7}, {c, 8}]).
[{b,7},{c,8}]
3> orddict:merge(fun (K, V1, V2) -> V1 * V2 end, OrdDict1, OrdDict2).
[{a,1},{b,14},{c,8}]
```
""".
-spec merge(Fun, Orddict1, Orddict2) -> Orddict3 when
      Fun :: fun((Key, Value1, Value2) -> Value),
      Orddict1 :: orddict(Key, Value1),
      Orddict2 :: orddict(Key, Value2),
      Orddict3 :: orddict(Key, Value).

merge(F, [{K1,_}=E1|D1], [{K2,_}=E2|D2]) when K1 < K2 ->
    [E1|merge(F, D1, [E2|D2])];
merge(F, [{K1,_}=E1|D1], [{K2,_}=E2|D2]) when K1 > K2 ->
    [E2|merge(F, [E1|D1], D2)];
merge(F, [{K1,V1}|D1], [{_K2,V2}|D2]) ->	%K1 == K2
    [{K1,F(K1, V1, V2)}|merge(F, D1, D2)];
merge(F, [], D2) when is_function(F, 3) -> D2;
merge(F, D1, []) when is_function(F, 3) -> D1.

reverse_pairs([{_,_}=H|T], Acc) ->
    reverse_pairs(T, [H|Acc]);
reverse_pairs([], Acc) -> Acc.
