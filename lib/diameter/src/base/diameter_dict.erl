%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

%%
%% This module provide OTP's dict interface built on top of ets.
%%
%% Note that while the interface is the same as dict the semantics
%% aren't quite. A Dict here is just a table identifier (although
%% this fact can't be used if you want dict/ets-based implementations
%% to be interchangeable) so changes made to the Dict modify the
%% underlying table. For merge/3, the first argument table is modified.
%%
%% The underlying ets table implementing a dict is deleted when the
%% process from which new() was invoked exits and the dict is only
%% writable from this process.
%%
%% The reason for this is to be able to swap dict/ets-based
%% implementations: the former is easier to debug, the latter is
%% faster for larger tables. It's also just a nice interface even
%% when there's no need for swapability.
%%

-module(diameter_dict).

-export([append/3,
         append_list/3,
         erase/2,
         fetch/2,
         fetch_keys/1,
         filter/2,
         find/2,
         fold/3,
         from_list/1,
         is_key/2,
         map/2,
         merge/3,
         new/0,
         store/3,
         to_list/1,
         update/3,
         update/4,
         update_counter/3]).

%%% ----------------------------------------------------------
%%% EXPORTED INTERNAL FUNCTIONS
%%% ----------------------------------------------------------

append(Key, Value, Dict) ->
    append_list(Key, [Value], Dict).

append_list(Key, ValueList, Dict)
  when is_list(ValueList) ->
    update(Key, fun(V) -> V ++ ValueList end, ValueList, Dict).

erase(Key, Dict) ->
    ets:delete(Dict, Key),
    Dict.

fetch(Key, Dict) ->
    {ok, V} = find(Key, Dict),
    V.

fetch_keys(Dict) ->
    ets:foldl(fun({K,_}, Acc) -> [K | Acc] end, [], Dict).

filter(Pred, Dict) ->
    lists:foreach(fun({K,V}) -> filter(Pred(K,V), K, Dict) end, to_list(Dict)),
    Dict.

find(Key, Dict) ->
    case ets:lookup(Dict, Key) of
        [{Key, V}] ->
            {ok, V};
        [] ->
            error
    end.

fold(Fun, Acc0, Dict) ->
    ets:foldl(fun({K,V}, Acc) -> Fun(K, V, Acc) end, Acc0, Dict).

from_list(List) ->
    lists:foldl(fun store/2, new(), List).

is_key(Key, Dict) ->
    ets:member(Dict, Key).

map(Fun, Dict) ->
    lists:foreach(fun({K,V}) -> store(K, Fun(K,V), Dict) end, to_list(Dict)),
    Dict.

merge(Fun, Dict1, Dict2) ->
    fold(fun(K2,V2,_) ->
                 update(K2, fun(V1) -> Fun(K2, V1, V2) end, V2, Dict1)
         end,
         Dict1,
         Dict2).

new() ->
    ets:new(?MODULE, [set]).

store(Key, Value, Dict) ->
    store({Key, Value}, Dict).

to_list(Dict) ->
    ets:tab2list(Dict).

update(Key, Fun, Dict) ->
    store(Key, Fun(fetch(Key, Dict)), Dict).

update(Key, Fun, Initial, Dict) ->
    store(Key, map(Key, Fun, Dict, Initial), Dict).

update_counter(Key, Increment, Dict)
  when is_integer(Increment) ->
    update(Key, fun(V) -> V + Increment end, Increment, Dict).

%%% ---------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%% ---------------------------------------------------------

store({_,_} = T, Dict) ->
    ets:insert(Dict, T),
    Dict.

filter(true, _, _) ->
    ok;
filter(false, K, Dict) ->
    erase(K, Dict).

map(Key, Fun, Dict, Error) ->
    case find(Key, Dict) of
        {ok, V} ->
            Fun(V);
        error ->
            Error
    end.

