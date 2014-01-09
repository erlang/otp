%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
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

-module(orddict).

%% Standard interface.
-export([new/0,is_key/2,to_list/1,from_list/1,size/1,is_empty/1]).
-export([fetch/2,find/2,fetch_keys/1,erase/2]).
-export([store/3,append/3,append_list/3,update/3,update/4,update_counter/3]).
-export([fold/3,map/2,filter/2,merge/3]).

-export_type([orddict/0]).

%%---------------------------------------------------------------------------

-type orddict() :: [{Key :: term(), Value :: term()}].

%%---------------------------------------------------------------------------

-spec new() -> orddict().

new() -> [].

-spec is_key(Key, Orddict) -> boolean() when
      Key :: term(),
      Orddict :: orddict().

is_key(Key, [{K,_}|_]) when Key < K -> false;
is_key(Key, [{K,_}|Dict]) when Key > K -> is_key(Key, Dict);
is_key(_Key, [{_K,_Val}|_]) -> true;		%Key == K
is_key(_, []) -> false.

-spec to_list(Orddict) -> List when
      Orddict :: orddict(),
      List :: [{Key :: term(), Value :: term()}].

to_list(Dict) -> Dict.

-spec from_list(List) -> Orddict when
      List :: [{Key :: term(), Value :: term()}],
      Orddict :: orddict().

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
      Key :: term(),
      Value :: term(),
      Orddict :: orddict().

fetch(Key, [{K,_}|D]) when Key > K -> fetch(Key, D);
fetch(Key, [{K,Value}|_]) when Key == K -> Value.

-spec find(Key, Orddict) -> {'ok', Value} | 'error' when
      Key :: term(),
      Orddict :: orddict(),
      Value :: term().

find(Key, [{K,_}|_]) when Key < K -> error;
find(Key, [{K,_}|D]) when Key > K -> find(Key, D);
find(_Key, [{_K,Value}|_]) -> {ok,Value};	%Key == K
find(_, []) -> error.

-spec fetch_keys(Orddict) -> Keys when
      Orddict :: orddict(),
      Keys :: [term()].

fetch_keys([{Key,_}|Dict]) ->
    [Key|fetch_keys(Dict)];
fetch_keys([]) -> [].

-spec erase(Key, Orddict1) -> Orddict2 when
      Key :: term(),
      Orddict1 :: orddict(),
      Orddict2 :: orddict().

erase(Key, [{K,_}=E|Dict]) when Key < K -> [E|Dict];
erase(Key, [{K,_}=E|Dict]) when Key > K ->
    [E|erase(Key, Dict)];
erase(_Key, [{_K,_Val}|Dict]) -> Dict;		%Key == K
erase(_, []) -> [].

-spec store(Key, Value, Orddict1) -> Orddict2 when
      Key :: term(),
      Value :: term(),
      Orddict1 :: orddict(),
      Orddict2 :: orddict().

store(Key, New, [{K,_}=E|Dict]) when Key < K ->
    [{Key,New},E|Dict];
store(Key, New, [{K,_}=E|Dict]) when Key > K ->
    [E|store(Key, New, Dict)];
store(Key, New, [{_K,_Old}|Dict]) ->		%Key == K
    [{Key,New}|Dict];
store(Key, New, []) -> [{Key,New}].

-spec append(Key, Value, Orddict1) -> Orddict2 when
      Key :: term(),
      Value :: term(),
      Orddict1 :: orddict(),
      Orddict2 :: orddict().

append(Key, New, [{K,_}=E|Dict]) when Key < K ->
    [{Key,[New]},E|Dict];
append(Key, New, [{K,_}=E|Dict]) when Key > K ->
    [E|append(Key, New, Dict)];
append(Key, New, [{_K,Old}|Dict]) ->		%Key == K
    [{Key,Old ++ [New]}|Dict];
append(Key, New, []) -> [{Key,[New]}].

-spec append_list(Key, ValList, Orddict1) -> Orddict2 when
      Key :: term(),
      ValList :: [Value :: term()],
      Orddict1 :: orddict(),
      Orddict2 :: orddict().

append_list(Key, NewList, [{K,_}=E|Dict]) when Key < K ->
    [{Key,NewList},E|Dict];
append_list(Key, NewList, [{K,_}=E|Dict]) when Key > K ->
    [E|append_list(Key, NewList, Dict)];
append_list(Key, NewList, [{_K,Old}|Dict]) ->		%Key == K
    [{Key,Old ++ NewList}|Dict];
append_list(Key, NewList, []) ->
    [{Key,NewList}].

-spec update(Key, Fun, Orddict1) -> Orddict2 when
      Key :: term(),
      Fun :: fun((Value1 :: term()) -> Value2 :: term()),
      Orddict1 :: orddict(),
      Orddict2 :: orddict().

update(Key, Fun, [{K,_}=E|Dict]) when Key > K ->
    [E|update(Key, Fun, Dict)];
update(Key, Fun, [{K,Val}|Dict]) when Key == K ->
    [{Key,Fun(Val)}|Dict].

-spec update(Key, Fun, Initial, Orddict1) -> Orddict2 when
      Key :: term(),
      Initial :: term(),
      Fun :: fun((Value1 :: term()) -> Value2 :: term()),
      Orddict1 :: orddict(),
      Orddict2 :: orddict().

update(Key, _, Init, [{K,_}=E|Dict]) when Key < K ->
    [{Key,Init},E|Dict];
update(Key, Fun, Init, [{K,_}=E|Dict]) when Key > K ->
    [E|update(Key, Fun, Init, Dict)];
update(Key, Fun, _Init, [{_K,Val}|Dict]) ->		%Key == K
    [{Key,Fun(Val)}|Dict];
update(Key, _, Init, []) -> [{Key,Init}].

-spec update_counter(Key, Increment, Orddict1) -> Orddict2 when
      Key :: term(),
      Increment :: number(),
      Orddict1 :: orddict(),
      Orddict2 :: orddict().

update_counter(Key, Incr, [{K,_}=E|Dict]) when Key < K ->
    [{Key,Incr},E|Dict];
update_counter(Key, Incr, [{K,_}=E|Dict]) when Key > K ->
    [E|update_counter(Key, Incr, Dict)];
update_counter(Key, Incr, [{_K,Val}|Dict]) ->		%Key == K
    [{Key,Val+Incr}|Dict];
update_counter(Key, Incr, []) -> [{Key,Incr}].

-spec fold(Fun, Acc0, Orddict) -> Acc1 when
      Fun :: fun((Key :: term(), Value :: term(), AccIn :: term()) -> AccOut :: term()),
      Acc0 :: term(),
      Acc1 :: term(),
      Orddict :: orddict().

fold(F, Acc, [{Key,Val}|D]) ->
    fold(F, F(Key, Val, Acc), D);
fold(F, Acc, []) when is_function(F, 3) -> Acc.

-spec map(Fun, Orddict1) -> Orddict2 when
      Fun :: fun((Key :: term(), Value1 :: term()) -> Value2 :: term()),
      Orddict1 :: orddict(),
      Orddict2 :: orddict().

map(F, [{Key,Val}|D]) ->
    [{Key,F(Key, Val)}|map(F, D)];
map(F, []) when is_function(F, 2) -> [].

-spec filter(Pred, Orddict1) -> Orddict2 when
      Pred :: fun((Key :: term(), Value :: term()) -> boolean()),
      Orddict1 :: orddict(),
      Orddict2 :: orddict().

filter(F, [{Key,Val}=E|D]) ->
    case F(Key, Val) of
	true -> [E|filter(F, D)]; 
	false -> filter(F, D)
    end;
filter(F, []) when is_function(F, 2) -> [].

-spec merge(Fun, Orddict1, Orddict2) -> Orddict3 when
      Fun :: fun((Key :: term(), Value1 :: term(), Value2 :: term()) -> Value :: term()),
      Orddict1 :: orddict(),
      Orddict2 :: orddict(),
      Orddict3 :: orddict().

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
