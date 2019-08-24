%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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

-module(save_calls_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0, init_per_testcase/2,end_per_testcase/2]).

-export([save_calls_1/1,dont_break_reductions/1]).

-export([do_bopp/1, do_bipp/0, do_bepp/0]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [save_calls_1, dont_break_reductions].

init_per_testcase(dont_break_reductions,Config) ->
    %% Skip on --enable-native-libs as hipe rescedules after each
    %% function call.
    case erlang:system_info(hipe_architecture) of
        undefined ->
            Config;
        Architecture ->
            {lists, ListsBinary, _ListsFilename} = code:get_object_code(lists),
            ChunkName = hipe_unified_loader:chunk_name(Architecture),
            NativeChunk = beam_lib:chunks(ListsBinary, [ChunkName]),
            case NativeChunk of
                {ok,{_,[{_,Bin}]}} when is_binary(Bin) ->
                    {skip,"Does not work for --enable-native-libs"};
                {error, beam_lib, _} -> Config
            end
    end;
init_per_testcase(_,Config) ->
    Config.

end_per_testcase(_,_Config) ->
    ok.

%% Check that save_calls dont break reduction-based scheduling
dont_break_reductions(Config) when is_list(Config) ->
    RPS1 = reds_per_sched(0),
    RPS2 = reds_per_sched(20),
    Diff = abs(RPS1 - RPS2),
    true = (Diff < (0.2 * RPS1)),
    ok.


reds_per_sched(SaveCalls) ->
    Parent = self(),
    HowMany = 10000,
    Pid = spawn(fun() -> 
                        process_flag(save_calls,SaveCalls), 
                        receive 
                            go -> 
                                carmichaels_below(HowMany), 
                                Parent ! erlang:process_info(self(),reductions)
                        end 
                end),
    TH = spawn(fun() -> trace_handler(0,Parent,Pid) end),
    erlang:trace(Pid, true,[running,procs,{tracer,TH}]),
    Pid ! go,
    {Sched,Reds} = receive 
                       {accumulated,X} -> 
                           receive {reductions,Y} -> 
                                       {X,Y} 
                           after 30000 -> 
                                     timeout 
                           end 
                   after 30000 -> 
                             timeout 
                   end,
    Reds div Sched.



trace_handler(Acc,Parent,Client) ->
    receive
        {trace,Client,out,_} ->
            trace_handler(Acc+1,Parent,Client);
        {trace,Client,exit,_} ->
            Parent ! {accumulated, Acc};
        _ ->
            trace_handler(Acc,Parent,Client)
    after 10000 ->
              ok
    end.

%% Test call saving.
save_calls_1(Config) when is_list(Config) ->
    case test_server:is_native(?MODULE) of
        true -> {skipped,"Native code"};
        false -> save_calls_1()
    end.

save_calls_1() ->
    erlang:process_flag(self(), save_calls, 0),
    {last_calls, false} = process_info(self(), last_calls),
    
    erlang:process_flag(self(), save_calls, 10),
    {last_calls, _L1} = process_info(self(), last_calls),
    ?MODULE:do_bipp(),
    {last_calls, L2} = process_info(self(), last_calls),
    L21 = lists:filter(fun is_local_function/1, L2),
    case L21 of
        [{?MODULE,do_bipp,0},
         timeout,
         'send',
         {?MODULE,do_bopp,1},
         'receive',
         timeout,
         {?MODULE,do_bepp,0}] ->
            ok;
        X ->
            ct:fail({l21, X})
    end,
    
    erlang:process_flag(self(), save_calls, 10),
    {last_calls, L3} = process_info(self(), last_calls),
    true = (L3 /= false),
    L31 = lists:filter(fun is_local_function/1, L3),
    [] = L31,
    erlang:process_flag(self(), save_calls, 0),

    %% Also check that it works on another process ...
    Pid = spawn(fun () -> receive after infinity -> ok end end),
    erlang:process_flag(Pid, save_calls, 10),
    {last_calls, L4} = process_info(Pid, last_calls),
    true = (L4 /= false),
    L41 = lists:filter(fun is_local_function/1, L4),
    [] = L41,
    exit(Pid,kill),
    ok.

do_bipp() ->
    do_bopp(0),
    do_bapp(),
    ?MODULE:do_bopp(0),
    do_bopp(3),
    apply(?MODULE, do_bepp, []).

do_bapp() ->
    self() ! heffaklump.

do_bopp(T) ->
    receive
        X -> X
    after T -> ok
    end.

do_bepp() ->
    ok.

is_local_function({?MODULE, _, _}) ->
    true;
is_local_function({_, _, _}) ->
    false;
is_local_function(_) ->
    true.


% Number crunching for reds test.
carmichaels_below(N) ->
    rand:seed(exsplus, {3172,9814,20125}),
    carmichaels_below(1,N).

carmichaels_below(N,N2) when N >= N2 ->
    0;
carmichaels_below(N,N2) ->
    X = case fast_prime(N,10) of
            false -> 0;
            true ->
                case fast_prime2(N,10) of
                    true ->
                        %io:format("Prime: ~p~n",[N]),
                        0;
                    false ->
                        io:format("Carmichael: ~p (dividable by ~p)~n",
                                  [N,smallest_divisor(N)]),
                        1
                end
        end,
    X+carmichaels_below(N+2,N2).

expmod(_,E,_) when E == 0 ->
    1;
expmod(Base,Exp,Mod) when (Exp rem 2) == 0 ->
    X = expmod(Base,Exp div 2,Mod),
    (X*X) rem Mod;
expmod(Base,Exp,Mod) -> 
    (Base * expmod(Base,Exp - 1,Mod)) rem Mod.

uniform(N) ->
    rand:uniform(N-1).

fermat(N) ->    
    R = uniform(N),
    expmod(R,N,N) == R.

do_fast_prime(1,_) ->
    true;
do_fast_prime(_N,0) ->
    true;
do_fast_prime(N,Times) ->
    case fermat(N) of
        true ->
            do_fast_prime(N,Times-1);
        false ->
            false
    end.

fast_prime(N,T) ->
    do_fast_prime(N,T).

expmod2(_,E,_) when E == 0 ->
    1;
expmod2(Base,Exp,Mod) when (Exp rem 2) == 0 ->
    %% Uncomment the code below to simulate scheduling bug!
    %     case erlang:process_info(self(),last_calls) of
    % 	{last_calls,false} -> ok;
    % 	_ -> erlang:yield()
    %     end,
    X = expmod2(Base,Exp div 2,Mod),
    Y=(X*X) rem Mod,
    if 
        Y == 1, X =/= 1, X =/= (Mod - 1) ->
            0;
        true ->
            Y rem Mod
    end;
expmod2(Base,Exp,Mod) -> 
    (Base * expmod2(Base,Exp - 1,Mod)) rem Mod.

miller_rabbin(N) ->
    R = uniform(N),
    expmod2(R,N,N) == R.

do_fast_prime2(1,_) ->
    true;
do_fast_prime2(_N,0) ->
    true;
do_fast_prime2(N,Times) ->
    case miller_rabbin(N) of
        true ->
            do_fast_prime2(N,Times-1);
        false ->
            false
    end.

fast_prime2(N,T) ->
    do_fast_prime2(N,T).

smallest_divisor(N) ->
    find_divisor(N,2).

find_divisor(N,TD) ->
    if 
        TD*TD > N ->
            N;
        true ->
            case divides(TD,N) of
                true ->
                    TD;
                false ->
                    find_divisor(N,TD+1)
            end
    end.

divides(A,B) ->
    (B rem A) == 0.
