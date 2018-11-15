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
-module(counters_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [basic, bad, limits].

basic(Config) when is_list(Config) ->
    Size = 10,
    [begin
         Ref = counters:new(Size,[Type]),
         #{size:=Size, memory:=Memory} = counters:info(Ref),
         check_memory(Type, Memory, Size),
         [basic_do(Ref, Ix) || Ix <- lists:seq(1, Size)]
     end
     || Type <- [atomics, write_concurrency]],
    ok.

basic_do(Ref, Ix) ->
    0 = counters:get(Ref, Ix),
    ok = counters:add(Ref, Ix, 3),
    3  = counters:get(Ref, Ix),
    ok = counters:add(Ref, Ix, 14),
    17  = counters:get(Ref, Ix),
    ok = counters:add(Ref, Ix, -20),
    -3  = counters:get(Ref, Ix),
    ok = counters:add(Ref, Ix, 100),
    97 = counters:get(Ref, Ix),
    ok = counters:sub(Ref, Ix, 20),
    77 = counters:get(Ref, Ix),
    ok = counters:sub(Ref, Ix, -10),
    87 = counters:get(Ref, Ix),
    ok.

check_memory(atomics, Memory, Size) ->
    {_,true} = {Memory, Memory > Size*8},
    {_,true} = {Memory, Memory < Size*max_atomic_sz() + 100};
check_memory(write_concurrency, Memory, Size) ->
    NScheds = erlang:system_info(schedulers),
    {_,true} = {Memory, Memory > NScheds*Size*8},
    {_,true} = {Memory, Memory < NScheds*(Size+7)*max_atomic_sz() + 100}.

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

bad(Config) when is_list(Config) ->
    {'EXIT',{badarg,_}} = (catch counters:new(0,[])),
    {'EXIT',{badarg,_}} = (catch counters:new(10,[bad])),
    {'EXIT',{badarg,_}} = (catch counters:new(10,[atomic, bad])),
    {'EXIT',{badarg,_}} = (catch counters:new(10,[write_concurrency | bad])),
    Ref = counters:new(10,[]),
    {'EXIT',{badarg,_}} = (catch counters:get(1742, 7)),
    {'EXIT',{badarg,_}} = (catch counters:get(make_ref(), 7)),
    {'EXIT',{badarg,_}} = (catch counters:get(Ref, -1)),
    {'EXIT',{badarg,_}} = (catch counters:get(Ref, 0)),
    {'EXIT',{badarg,_}} = (catch counters:get(Ref, 11)),
    {'EXIT',{badarg,_}} = (catch counters:get(Ref, 7.0)),
    ok.


limits(Config) when is_list(Config) ->
    Bits = 64,
    Max = (1 bsl (Bits-1)) - 1,
    Min = -(1 bsl (Bits-1)),

    Ref = counters:new(1,[]),
    0 = counters:get(Ref, 1),
    ok = counters:add(Ref, 1, Max),
    ok = counters:add(Ref, 1, 1),
    Min = counters:get(Ref, 1),
    ok  = counters:sub(Ref, 1, 1),
    Max = counters:get(Ref, 1),

    IncrMax = (Max bsl 1) bor 1,
    ok = counters:sub(Ref, 1, counters:get(Ref, 1)),
    ok = counters:add(Ref, 1, IncrMax),
    -1 = counters:get(Ref, 1),
    {'EXIT',{badarg,_}} = (catch counters:add(Ref, 1, IncrMax+1)),
    {'EXIT',{badarg,_}} = (catch counters:add(Ref, 1, Min-1)),

    ok.
