%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2015. All Rights Reserved.
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

%% Standard interface.
-export([new/0,is_key/2,to_list/1,from_list/1,size/1,is_empty/1]).
-export([fetch/2,find/2,fetch_keys/1,erase/2]).
-export([store/3,append/3,append_list/3,update/3,update/4,update_counter/3]).
-export([fold/3,map/2,filter/2,merge/3]).

-export_type([orddict/0, orddict/2]).

%%---------------------------------------------------------------------------

-type orddict() :: orddict(_, _).

-type orddict(Key, Value) :: [{Key, Value}].

%%---------------------------------------------------------------------------

-spec new() -> orddict().

new() -> [].

-spec is_key(Key, Orddict) -> boolean() when
      Orddict :: orddict(Key, Value :: term()).

is_key(Key, [{K,_}|_]) when Key < K -> false;
is_key(Key, [{K,_}|Dict]) when Key > K -> is_key(Key, Dict);
is_key(_Key, [{_K,_Val}|_]) -> true;		%Key == K
is_key(_, []) -> false.

-spec to_list(Orddict) -> List when
      Orddict :: orddict(Key, Value),
      List :: [{Key, Value}].

to_list(Dict) -> Dict.

-spec from_list(List) -> Orddict when
      List :: [{Key, Value}],
      Orddict :: orddict(Key, Value).

from_list([]) -> [];
from_list([{_,_}]=Pair) -> Pair;
from_list(Pairs) ->
    lists:ukeysort(1, reverse_pairs(Pairs, [])).

-spec size(Orddict) -> non_neg_integer() when
      Orddict :: orddict().

size(D) -> length(D).

-spec is_empty(Orddict) -> boolean() when
      Orddict :: orddict().

is_empty([]) -> true;
is_empty([_|_]) -> false.

-spec fetch(Key, Orddict) -> Value when
      Orddict :: orddict(Key, Value).

fetch(Key, [{K,_}|D]) when Key > K -> fetch(Key, D);
fetch(Key, [{K,Value}|_]) when Key == K -> Value.

-spec find(Key, Orddict) -> {'ok', Value} | 'error' when
      Orddict :: orddict(Key, Value).

find(Key, [{K,_}|_]) when Key < K -> error;
find(Key, [{K,_}|D]) when Key > K -> find(Key, D);
find(_Key, [{_K,Value}|_]) -> {ok,Value};	%Key == K
find(_, []) -> error.

-spec fetch_keys(Orddict) -> Keys when
      Orddict :: orddict(Key, Value :: term()),
      Keys :: [Key].

fetch_keys([{Key,_}|Dict]) ->
    [Key|fetch_keys(Dict)];
fetch_keys([]) -> [].

-spec erase(Key, Orddict1) -> Orddict2 when
      Orddict1 :: orddict(Key, Value),
      Orddict2 :: orddict(Key, Value).

erase(Key, [{K,_}=E|Dict]) when Key < K -> [E|Dict];
erase(Key, [{K,_}=E|Dict]) when Key > K ->
    [E|erase(Key, Dict)];
erase(_Key, [{_K,_Val}|Dict]) -> Dict;		%Key == K
erase(_, []) -> [].

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

-spec update(Key, Fun, Orddict1) -> Orddict2 when
      Fun :: fun((Value1 :: Value) -> Value2 :: Value),
      Orddict1 :: orddict(Key, Value),
      Orddict2 :: orddict(Key, Value).

update(Key, Fun, [{K,_}=E|Dict]) when Key > K ->
    [E|update(Key, Fun, Dict)];
update(Key, Fun, [{K,Val}|Dict]) when Key == K ->
    [{Key,Fun(Val)}|Dict].

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

-spec map(Fun, Orddict1) -> Orddict2 when
      Fun :: fun((Key, Value1) -> Value2),
      Orddict1 :: orddict(Key, Value1),
      Orddict2 :: orddict(Key, Value2).

map(F, [{Key,Val}|D]) ->
    [{Key,F(Key, Val)}|map(F, D)];
map(F, []) when is_function(F, 2) -> [].

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
