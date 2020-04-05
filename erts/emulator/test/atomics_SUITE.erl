%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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
-module(atomics_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [signed, unsigned, bad, signed_limits, unsigned_limits].

signed(Config) when is_list(Config) ->
    Size = 10,
    Ref = atomics:new(Size,[]),
    #{size:=Size, memory:=Memory} = atomics:info(Ref),
    {_,true} = {Memory, Memory > Size*8},
    {_,true} = {Memory, Memory < Size*max_atomic_sz() + 100},
    [signed_do(Ref, Ix) || Ix <- lists:seq(1, Size)],
    ok.

signed_do(Ref, Ix) ->
    0 = atomics:get(Ref, Ix),
    ok = atomics:put(Ref, Ix, 3),
    ok = atomics:add(Ref, Ix, 14),
    17 = atomics:get(Ref, Ix),
    20 = atomics:add_get(Ref, Ix, 3),
    -3 = atomics:add_get(Ref, Ix, -23),
    17 = atomics:add_get(Ref, Ix, 20),
    ok = atomics:sub(Ref, Ix, 4),
    13 = atomics:get(Ref, Ix),
    -7 = atomics:sub_get(Ref, Ix, 20),
    3  = atomics:sub_get(Ref, Ix, -10),
    3  = atomics:exchange(Ref, Ix, 666),
    ok = atomics:compare_exchange(Ref, Ix, 666, 777),
    777 = atomics:compare_exchange(Ref, Ix, 666, -666),
    ok.

unsigned(Config) when is_list(Config) ->
    Size = 10,
    Ref = atomics:new(Size,[{signed, false}]),
    #{size:=Size, memory:=Memory} = atomics:info(Ref),
    true = Memory > Size*8,
    true = Memory < Size*max_atomic_sz() + 100,
    [unsigned_do(Ref, Ix) || Ix <- lists:seq(1, Size)],
    ok.

unsigned_do(Ref, Ix) ->
    0 = atomics:get(Ref, Ix),
    ok = atomics:put(Ref, Ix, 3),
    ok = atomics:add(Ref, Ix, 14),
    17 = atomics:get(Ref, Ix),
    20 = atomics:add_get(Ref, Ix, 3),
    ok = atomics:sub(Ref, Ix, 7),
    13 = atomics:get(Ref, Ix),
    3  = atomics:sub_get(Ref, Ix, 10),
    3  = atomics:exchange(Ref, Ix, 666),
    ok = atomics:compare_exchange(Ref, Ix, 666, 777),
    777 = atomics:compare_exchange(Ref, Ix, 666, 888),
    ok.

bad(Config) when is_list(Config) ->
    {'EXIT',{badarg,_}} = (catch atomics:new(0,[])),
    {'EXIT',{badarg,_}} = (catch atomics:new(10,[bad])),
    {'EXIT',{badarg,_}} = (catch atomics:new(10,[{signed,bad}])),
    {'EXIT',{badarg,_}} = (catch atomics:new(10,[{signed,true}, bad])),
    {'EXIT',{badarg,_}} = (catch atomics:new(10,[{signed,false} | bad])),
    Ref = atomics:new(10,[]),
    {'EXIT',{badarg,_}} = (catch atomics:get(1742, 7)),
    {'EXIT',{badarg,_}} = (catch atomics:get(make_ref(), 7)),
    {'EXIT',{badarg,_}} = (catch atomics:get(Ref, -1)),
    {'EXIT',{badarg,_}} = (catch atomics:get(Ref, 0)),
    {'EXIT',{badarg,_}} = (catch atomics:get(Ref, 11)),
    {'EXIT',{badarg,_}} = (catch atomics:get(Ref, 7.0)),
    ok.


signed_limits(Config) when is_list(Config) ->
    Bits = 64,
    Max = (1 bsl (Bits-1)) - 1,
    Min = -(1 bsl (Bits-1)),

    Ref = atomics:new(1,[{signed, true}]),
    #{max:=Max, min:=Min} = atomics:info(Ref),
    0 = atomics:get(Ref, 1),
    ok = atomics:add(Ref, 1, Max),
    Min = atomics:add_get(Ref, 1, 1),
    Max = atomics:sub_get(Ref, 1, 1),

    IncrMax = (Max bsl 1) bor 1,
    ok = atomics:put(Ref, 1, 0),
    ok = atomics:add(Ref, 1, IncrMax),
    -1 = atomics:get(Ref, 1),
    {'EXIT',{badarg,_}} = (catch atomics:add(Ref, 1, IncrMax+1)),
    {'EXIT',{badarg,_}} = (catch atomics:add(Ref, 1, Min-1)),

    ok.

unsigned_limits(Config) when is_list(Config) ->
    Bits = 64,
    Max = (1 bsl Bits) - 1,
    Min = 0,

    Ref = atomics:new(1,[{signed,false}]),
    #{max:=Max, min:=Min} = atomics:info(Ref),
    0 = atomics:get(Ref, 1),
    ok = atomics:add(Ref, 1, Max),
    Min = atomics:add_get(Ref, 1, 1),
    Max = atomics:sub_get(Ref, 1, 1),

    atomics:put(Ref, 1, Max),
    io:format("Max=~p~n", [atomics:get(Ref, 1)]),

    {'EXIT',{badarg,_}} = (catch atomics:add(Ref, 1, Max+1)),
    IncrMin = -(1 bsl (Bits-1)),
    ok = atomics:put(Ref, 1, -IncrMin),
    ok = atomics:add(Ref, 1, IncrMin),
    0 = atomics:get(Ref, 1),
    {'EXIT',{badarg,_}} = (catch atomics:add(Ref, 1, IncrMin-1)),

    ok.

max_atomic_sz() ->
    case erlang:system_info({wordsize, external}) of
        4 -> 16;
        8 ->
            EI = erlang:system_info(ethread_info),
            case lists:keyfind("64-bit native atomics", 1, EI) of
                {_, "no", _} -> 16;
                _ -> 8
            end
    end.
