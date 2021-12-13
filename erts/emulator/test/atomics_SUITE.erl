%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2021. All Rights Reserved.
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
-export([suite/0, all/0,
         signed/1, unsigned/1, bad/1, signed_limits/1, unsigned_limits/1,
         error_info/1]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [signed, unsigned, bad, signed_limits, unsigned_limits,
     error_info].

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
    {'EXIT',{badarg,_}} = (catch atomics:new(-(1 bsl 64),[])),
    {'EXIT',{badarg,_}} = (catch atomics:new(0,[])),
    {'EXIT',{badarg,_}} = (catch atomics:new(10,[bad])),
    {'EXIT',{badarg,_}} = (catch atomics:new(10,[{signed,bad}])),
    {'EXIT',{badarg,_}} = (catch atomics:new(10,[{signed,true}, bad])),
    {'EXIT',{badarg,_}} = (catch atomics:new(10,[{signed,false} | bad])),

    {'EXIT',{system_limit,_}} = (catch atomics:new(1 bsl 64, [])),

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

error_info(_Config) ->
    Atomics = atomics:new(10, []),
    Huge = 1 bsl 64,

    L = [{add,[bad_ref, 1, 99]},
         {add,[make_ref(), 1, 99]},
         {add,[Atomics, bad_index, bad_value]},
         {add,[Atomics, 1, Huge]},
         {add,[Atomics, 0, 99]},
         {add,[Atomics, 11, 99]},

         {add_get,[make_ref(), 1, 99]},
         {add_get,[Atomics, bad_index, bad_value]},
         {add_get,[Atomics, 0, 99]},
         {add_get,[Atomics, 11, 99]},

         {compare_exchange,[make_ref(), 1, 50, 99]},
         {compare_exchange,[Atomics, bad_index, 50, 99]},
         {compare_exchange,[Atomics, 0, 50, 99]},
         {compare_exchange,[Atomics, 11, 50, 99]},

         {exchange,[make_ref(), 1, 50]},
         {exchange,[Atomics, bad_index, 50]},
         {exchange,[Atomics, 0, 50]},
         {exchange,[Atomics, 11, 50]},
         {exchange,[Atomics, 1, Huge]},

         {get, [make_ref(), 1]},
         {get, [Atomics, 0]},
         {get, [Atomics, bad_index]},
         {get, [Atomics, 11]},

         {info, [make_ref()]},
         {info, [abc]},

         {new, [0, []]},
         {new, [11, bad_option_list]},
         {new, [11, [bad_option]]},
         {new, [1 bsl 64, []]},

         {put, [make_ref(), 1, 42]},
         {put, [Atomics, 1, Huge]},
         {put, [Atomics, 0, 42]},
         {put, [Atomics, 11, 42]},
         {put, [Atomics, 0, abc]},

         {sub, [make_ref(), 1, 99]},
         {sub, [Atomics, bad_index, bad_value]},
         {sub, [Atomics, 0, 99]},
         {sub, [Atomics, 11, 99]},

         {sub_get, [make_ref(), 0, 99]},
         {sub_get, [Atomics, bad_index, bad_value]},
         {sub_get, [Atomics, 0, 99]},
         {sub_get, [Atomics, 11, 99]}
        ],
    do_error_info(L).

do_error_info(L0) ->
    L1 = lists:foldl(fun({_,A}, Acc) when is_integer(A) -> Acc;
                        ({F,A}, Acc) -> [{F,A,[]}|Acc];
                        ({F,A,Opts}, Acc) -> [{F,A,Opts}|Acc]
                     end, [], L0),
    Tests = ordsets:from_list([{F,length(A)} || {F,A,_} <- L1] ++
                                  [{F,A} || {F,A} <- L0, is_integer(A)]),
    Bifs0 = [{F,A} || {F,A} <- atomics:module_info(exports),
                      A =/= 0,
                      F =/= module_info],
    Bifs = ordsets:from_list(Bifs0),
    NYI = [{F,lists:duplicate(A, '*'),nyi} || {F,A} <- Bifs -- Tests],
    L = lists:sort(NYI ++ L1),
    do_error_info(L, []).

do_error_info([{_,Args,nyi}=H|T], Errors) ->
    case lists:all(fun(A) -> A =:= '*' end, Args) of
        true ->
            do_error_info(T, [{nyi,H}|Errors]);
        false ->
            do_error_info(T, [{bad_nyi,H}|Errors])
    end;
do_error_info([{F,Args,Opts}|T], Errors) ->
    eval_bif_error(F, Args, Opts, T, Errors);
do_error_info([], Errors0) ->
    case lists:sort(Errors0) of
        [] ->
            ok;
        [_|_]=Errors ->
            io:format("\n~p\n", [Errors]),
            ct:fail({length(Errors),errors})
    end.

eval_bif_error(F, Args, Opts, T, Errors0) ->
    Module = atomics,
    try apply(Module, F, Args) of
        Result ->
            case lists:member(no_fail, Opts) of
                true ->
                    do_error_info(T, Errors0);
                false ->
                    do_error_info(T, [{should_fail,{F,Args},Result}|Errors0])
            end
    catch
        error:Reason:Stk ->
            SF = fun(Mod, _, _) -> Mod =:= test_server end,
            Str = erl_error:format_exception(error, Reason, Stk, #{stack_trim_fun => SF}),
            BinStr = iolist_to_binary(Str),
            ArgStr = lists:join(", ", [io_lib:format("~p", [A]) || A <- Args]),
            io:format("\n~p:~p(~s)\n~ts", [Module,F,ArgStr,BinStr]),

            case Stk of
                [{Module,ActualF,ActualArgs,Info}|_] ->
                    RE = <<"[*][*][*] argument \\d+:">>,
                    Errors1 = case re:run(BinStr, RE, [{capture, none}]) of
                                  match ->
                                      Errors0;
                                  nomatch when Reason =:= system_limit ->
                                      Errors0;
                                  nomatch ->
                                      [{no_explanation,{F,Args},Info}|Errors0]
                              end,

                    Errors = case {ActualF,ActualArgs} of
                                 {F,Args} ->
                                     Errors1;
                                 _ ->
                                     [{renamed,{F,length(Args)},{ActualF,ActualArgs}}|Errors1]
                             end,

                    do_error_info(T, Errors);
                _ ->
                    Errors = [{renamed,{F,length(Args)},hd(Stk)}|Errors0],
                    do_error_info(T, Errors)
            end
    end.
