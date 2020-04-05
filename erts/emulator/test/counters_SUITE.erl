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

-export([suite/0, all/0]).
-export([basic/1, bad/1, limits/1, indep/1, write_concurrency/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [basic, bad, limits, indep, write_concurrency].

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
    ok = counters:put(Ref, Ix, 0),
    0 = counters:get(Ref, Ix),
    ok = counters:put(Ref, Ix, 123),
    123 = counters:get(Ref, Ix),
    ok = counters:put(Ref, Ix, -321),
    -321 = counters:get(Ref, Ix),
    ok.

check_memory(atomics, Memory, Size) ->
    {_,true} = {Memory, Memory > Size*8},
    {_,true} = {Memory, Memory < Size*max_atomic_sz() + 100};
check_memory(write_concurrency, Memory, Size) ->
    NWords = erlang:system_info(schedulers) + 1,
    {_,true} = {Memory, Memory > NWords*Size*8},
    {_,true} = {Memory, Memory < NWords*(Size+7)*max_atomic_sz() + 100}.

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
    limits_do(counters:new(1,[atomics])),
    limits_do(counters:new(1,[write_concurrency])),
    ok.

limits_do(Ref) ->
    Bits = 64,
    Max = (1 bsl (Bits-1)) - 1,
    Min = -(1 bsl (Bits-1)),

    0 = counters:get(Ref, 1),
    ok = counters:put(Ref, 1, Max),
    Max = counters:get(Ref, 1),
    ok = counters:add(Ref, 1, 1),
    Min = counters:get(Ref, 1),
    ok  = counters:sub(Ref, 1, 1),
    Max = counters:get(Ref, 1),
    ok = counters:put(Ref, 1, Min),
    Min = counters:get(Ref, 1),

    IncrMax = (Max bsl 1) bor 1,
    ok = counters:put(Ref, 1, 0),
    ok = counters:add(Ref, 1, IncrMax),
    -1 = counters:get(Ref, 1),
    {'EXIT',{badarg,_}} = (catch counters:add(Ref, 1, IncrMax+1)),
    {'EXIT',{badarg,_}} = (catch counters:add(Ref, 1, Min-1)),
    {'EXIT',{badarg,_}} = (catch counters:put(Ref, 1, Max+1)),
    {'EXIT',{badarg,_}} = (catch counters:add(Ref, 1, Min-1)),
    ok.


%% Verify that independent workers, using different counters
%% within the same array, do not interfere with each other.
indep(Config) when is_list(Config) ->
    NScheds = erlang:system_info(schedulers),
    Ref = counters:new(NScheds,[write_concurrency]),
    Rounds = 100,
    Papa = self(),
    Pids = [spawn_opt(fun () ->
                               Val = I*197,
                               counters:put(Ref, I, Val),
                               indep_looper(Rounds, Ref, I, Val),
                               Papa ! {self(), done}
                       end,
                      [link, {scheduler, I}])
            || I <- lists:seq(1, NScheds)],
    [receive {P,done} -> ok end || P <- Pids],
    ok.

indep_looper(0, _, _ , _) ->
    ok;
indep_looper(N, Ref, I, Val0) ->
    %%io:format("Val0 = ~p\n", [Val0]),
    Val0 = counters:get(Ref, I),
    Val1 = indep_adder(Ref, I, Val0),
    indep_subber(Ref, I, Val1),
    Val2 = N*7 + I,
    counters:put(Ref, I, Val2),
    indep_looper(N-1, Ref, I, Val2).

indep_adder(Ref, I, Val) when Val < (1 bsl 62) ->
    %%io:format("adder Val = ~p\n", [Val]),
    Incr = abs(Val div 2) + I + 984735,
    counters:add(Ref, I, Incr),
    Res = Val + Incr,
    Res = counters:get(Ref, I),
    indep_adder(Ref, I, Res);
indep_adder(_Ref, _I, Val) ->
    Val.

indep_subber(Ref, I, Val) when Val > -(1 bsl 62) ->
    %%io:format("subber Val = ~p\n", [Val]),
    Decr = (abs(Val div 2) + I + 725634),
    counters:sub(Ref, I, Decr),
    Res = Val - Decr,
    Res = counters:get(Ref, I),
    indep_subber(Ref, I, Res);
indep_subber(_Ref, _I, Val) ->
    Val.



%% Verify write_concurrency yields correct results.
write_concurrency(Config) when is_list(Config) ->
    rand:seed(exs1024s),
    io:format("*** SEED: ~p ***\n", [rand:export_seed()]),
    NScheds = erlang:system_info(schedulers),
    Size = 100,
    Ref = counters:new(Size,[write_concurrency]),
    Rounds = 1000,
    Papa = self(),
    Pids = [spawn_opt(fun Worker() ->
                              receive
                                  {go, Ix, Incr} ->
                                      wc_looper(Rounds, Ref, Ix, Incr),
                                      Papa ! {self(), done, Rounds*Incr},
                                      Worker();
                                  stop ->
                                      ok
                              end
                       end,
                      [link, {scheduler, N}])
            || N <- lists:seq(1, NScheds)],
    [begin
         Base = rand_log64(),
         counters:put(Ref, Index, Base),
         SendList = [{P,{go, Index, rand_log64()}} || P <- Pids],
         [P ! Msg || {P,Msg} <- SendList],
         Added = lists:sum([receive {P,done,Contrib} -> Contrib end || P <- Pids]),
         Result = mask_sint64(Base+Added),
         {_,Result} = {Result, counters:get(Ref, Index)}
     end
     || Index <- lists:seq(1, Size)],

    [begin unlink(P), P ! stop end || P <- Pids],
    ok.

wc_looper(0, _, _, _) ->
    ok;
wc_looper(N, Ref, Ix, Incr) ->
    counters:add(Ref, Ix, Incr),
    wc_looper(N-1, Ref, Ix, Incr).

mask_sint64(X) ->
    SMask = 1 bsl 63,
    UMask = SMask - 1,
    (X band UMask) - (X band SMask).

%% A random signed 64-bit integer
%% with a uniformly distributed number of significant bits.
rand_log64() ->
    Uint = round(math:pow(2, rand:uniform()*63)),
    case rand:uniform(2) of
        1 -> -Uint;
        2 -> Uint
    end.
