%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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

-module(trace_local_SUITE).
-compile({nowarn_deprecated_function, {erlang,hash,2}}).

-export([basic_test/0, bit_syntax_test/0, return_test/0,
	 on_and_off_test/0, stack_grow_test/0,
	 info_test/0, delete_test/1, exception_test/1,
	 not_run/1]).

-export([exported/1, exported_wrap/1, loop/4, apply_slave_async/5,
	 match/2, clause/2, id/1, undef/1, lists_reverse/2]).


%%
%% Define for debug output
%%
%%-define(debug,1).
 
-include_lib("common_test/include/ct.hrl").
-define(DEFAULT_RECEIVE_TIMEOUT, infinity).

-ifdef(debug).
-define(dbgformat(A,B),io:format(A,B)).
-else.
-define(dbgformat(A,B),noop).
-endif.

%%% When run in test server %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([all/0, suite/0,
         basic/1, bit_syntax/1,
         return/1, on_and_off/1, systematic_on_off/1,
         stack_grow/1,info/1, delete/1,
         exception/1, exception_apply/1,
         exception_function/1, exception_apply_function/1,
         exception_nocatch/1, exception_nocatch_apply/1,
         exception_nocatch_function/1, exception_nocatch_apply_function/1,
         exception_meta/1, exception_meta_apply/1,
         exception_meta_function/1, exception_meta_apply_function/1,
         exception_meta_nocatch/1, exception_meta_nocatch_apply/1,
         exception_meta_nocatch_function/1,
         exception_meta_nocatch_apply_function/1,
         concurrency/1,
         init_per_testcase/2, end_per_testcase/2]).

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, Config) ->
    shutdown(),

    %% Reloading the module will clear all trace patterns, and
    %% in a debug-compiled emulator run assertions of the counters
    %% for the number of functions with breakpoints.

    c:l(?MODULE).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() -> 
    case test_server:is_native(trace_local_SUITE) of
        true -> [not_run];
        false ->
            [basic, bit_syntax, return, on_and_off, systematic_on_off,
             stack_grow,
             info, delete, exception, exception_apply,
             exception_function, exception_apply_function,
             exception_nocatch, exception_nocatch_apply,
             exception_nocatch_function,
             exception_nocatch_apply_function, exception_meta,
             exception_meta_apply, exception_meta_function,
             exception_meta_apply_function, exception_meta_nocatch,
             exception_meta_nocatch_apply,
             exception_meta_nocatch_function,
             exception_meta_nocatch_apply_function,
             concurrency]
    end.


not_run(Config) when is_list(Config) -> 
    {skipped,"Native code"}.

%% Tests basic local call-trace
basic(Config) when is_list(Config) ->
    basic_test().

%% OTP-7399: Make sure that code that uses the optimized bit syntax matching
%% can be traced without crashing the emulator.
bit_syntax(Config) when is_list(Config) ->
    bit_syntax_test().

%% Tests the different types of return trace
return(Config) when is_list(Config) ->
    return_test().

%% Tests turning trace parameters on and off,
%% both for trace and trace_pattern
on_and_off(Config) when is_list(Config) ->
    on_and_off_test().

%% Tests the stack growth during return traces
stack_grow(Config) when is_list(Config) ->
    stack_grow_test().

%% Tests the trace_info BIF
info(Config) when is_list(Config) ->
    info_test().

%% Tests putting trace on deleted modules
delete(Config) when is_list(Config) ->
    delete_test(Config).

%% Tests exception_trace
exception(Config) when is_list(Config) ->
    exception_test([]).

%% Tests exception_trace
exception_apply(Config) when is_list(Config) ->
    exception_test([apply]).

%% Tests exception_trace
exception_function(Config) when is_list(Config) ->
    exception_test([function]).

%% Tests exception_trace
exception_apply_function(Config) when is_list(Config) ->
    exception_test([apply,function]).

%% Tests exception_trace
exception_nocatch(Config) when is_list(Config) ->
    exception_test([nocatch]).

%% Tests exception_trace
exception_nocatch_apply(Config) when is_list(Config) ->
    exception_test([nocatch,apply]).

%% Tests exception_trace
exception_nocatch_function(Config) when is_list(Config) ->
    exception_test([nocatch,function]).

%% Tests exception_trace
exception_nocatch_apply_function(Config) when is_list(Config) ->
    exception_test([nocatch,apply,function]).

%% Tests meta exception_trace
exception_meta(Config) when is_list(Config) ->
    exception_test([meta]).

%% Tests meta exception_trace
exception_meta_apply(Config) when is_list(Config) ->
    exception_test([meta,apply]).

%% Tests meta exception_trace
exception_meta_function(Config) when is_list(Config) ->
    exception_test([meta,function]).

%% Tests meta exception_trace
exception_meta_apply_function(Config) when is_list(Config) ->
    exception_test([meta,apply,function]).

%% Tests meta exception_trace
exception_meta_nocatch(Config) when is_list(Config) ->
    exception_test([meta,nocatch]).

%% Tests meta exception_trace
exception_meta_nocatch_apply(Config) when is_list(Config) ->
    exception_test([meta,nocatch,apply]).

%% Tests meta exception_trace
exception_meta_nocatch_function(Config) when is_list(Config) ->
    exception_test([meta,nocatch,function]).

%% Tests meta exception_trace
exception_meta_nocatch_apply_function(Config) when is_list(Config) ->
    exception_test([meta,nocatch,apply,function]).


%%% Message patterns and expect functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(pCT(P,M,F,A),   {trace,   P,call,{M,F,A}}).
-define(pCTT(P,M,F,A),  {trace_ts,P,call,{M,F,A},{_,_,_}}).
-define(pRF(P,M,F,A,V), {trace,   P,return_from,{M,F,A},V}).
-define(pRFT(P,M,F,A,V),{trace_ts,P,return_from,{M,F,A},V,{_,_,_}}).
-define(pEF(P,M,F,A,V), {trace,   P,exception_from,{M,F,A},V}).
-define(pEFT(P,M,F,A,V),{trace_ts,P,exception_from,{M,F,A},V,{_,_,_}}).
-define(pRT(P,M,F,A),   {trace,   P,return_to,{M,F,A}}).
-define(pRTT(P,M,F,A),  {trace_ts,P,return_to,{M,F,A},{_,_,_}}).

-define(CT(M,F,A),    ?pCT(_,M,F,A)    = receive_next()).
-define(CTT(M,F,A),   ?pCTT(_,M,F,A)   = receive_next()).
-define(RF(M,F,A,V),  ?pRF(_,M,F,A,V)  = receive_next()).
-define(RFT(M,F,A,V), ?pRFT(_,M,F,A,V) = receive_next()).
-define(EF(M,F,A,V),  ?pEF(_,M,F,A,V)  = receive_next()).
-define(EFT(M,F,A,V), ?pEFT(_,M,F,A,V) = receive_next()).
-define(RT(M,F,A),    ?pRT(_,M,F,A)    = receive_next()).
-define(RTT(M,F,A),   ?pRTT(_,M,F,A)   = receive_next()).
-define(NM, receive_no_next(100)).

expect() ->
    {Pid,_} = get(slave),
    expect_receive(Pid).

expect(Msg) ->
    {Pid,_} = get(slave),
    expect_pid(Pid, Msg).



expect_pid(_Pid, []) ->
    ok;
expect_pid(Pid, [Line|T]) when is_integer(Line) ->
    put(test_server_loc, {?MODULE,Line}),
    expect_pid(Pid, T);
expect_pid(Pid, [true|[_|_]=T]) ->
    expect_pid(Pid, T);
expect_pid(Pid, [false|[_|T]]) ->
    expect_pid(Pid, T);
expect_pid(Pid, [H|T]) ->
    expect_pid(Pid, H),
    expect_pid(Pid, T);
expect_pid(Pid, Msg) when is_tuple(Msg) ->
    same(Msg, expect_receive(Pid));
expect_pid(Pid, Fun) when is_function(Fun, 1) ->
    case Fun(expect_receive(Pid)) of
        next ->
            expect_pid(Pid, Fun);
        done ->
            ok;
        Other ->
            expect_pid(Pid, Other)
    end.

expect_receive(Pid) when is_pid(Pid) ->
    receive
        Msg when is_tuple(Msg), 
                 element(1, Msg) == trace, 
                 element(2, Msg) =/= Pid;
                 %%
                 is_tuple(Msg), 
                 element(1, Msg) == trace_ts, 
                 element(2, Msg) =/= Pid ->
            expect_receive(Pid);
        Msg ->
            expect_msg(Pid, Msg)
    after 100 ->
              {nm}
    end.

expect_msg(P, ?pCT(P,M,F,Args))       -> {ct,{M,F},Args};
expect_msg(P, ?pCTT(P,M,F,Args))      -> {ctt,{M,F},Args};
expect_msg(P, ?pRF(P,M,F,Arity,V))    -> {rf,{M,F,Arity},V};
expect_msg(P, ?pRFT(P,M,F,Arity,V))   -> {rft,{M,F,Arity},V};
expect_msg(P, ?pEF(P,M,F,Arity,V))    -> {ef,{M,F,Arity},V};
expect_msg(P, ?pEFT(P,M,F,Arity,V))   -> {eft,{M,F,Arity},V};
expect_msg(P, ?pRT(P,M,F,Arity))      -> {rt,{M,F,Arity}};
expect_msg(P, ?pRTT(P,M,F,Arity))     -> {rtt,{M,F,Arity}};
expect_msg(P, Msg) when is_tuple(Msg) ->
    case tuple_to_list(Msg) of
        [trace,P|T] ->
            list_to_tuple([trace|T]);
        [trace_ts,P|[_|_]=T] ->
            list_to_tuple([trace_ts|reverse(tl(reverse(T)))]);
        _ ->
            Msg
    end.

same(A, B) ->
    case [A|B] of
        [X|X] ->
            ok
    end.



%%% tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

basic_test() ->
    setup([call]),
    NumMatches = erlang:trace_pattern({?MODULE,'_','_'},[],[local]),
    NumMatches = erlang:trace_pattern({?MODULE,'_','_'},[],[local]),
    erlang:trace_pattern({?MODULE,slave,'_'},false,[local]),
    [1,1,1,1] = apply_slave(?MODULE,exported_wrap,[1]),
    ?CT(?MODULE,exported_wrap,[1]),
    ?CT(?MODULE,exported,[1]),
    ?CT(?MODULE,local,[1]),
    ?CT(?MODULE,local2,[1]),
    ?CT(?MODULE,local_tail,[1]),
    erlang:trace_pattern({?MODULE,'_','_'},[],[]),
    erlang:trace_pattern({?MODULE,slave,'_'},false,[local]),
    [1,1,1,1] = apply_slave(?MODULE,exported_wrap,[1]),
    ?CT(?MODULE,exported_wrap,[1]),
    [1,1,1,1] = lambda_slave(fun() ->
                                     exported_wrap(1)
                             end),
    ?NM, 
    erlang:trace_pattern({?MODULE,'_','_'},[],[local]),
    erlang:trace_pattern({?MODULE,slave,'_'},false,[local]),
    [1,1,1,1] = lambda_slave(fun() ->
                                     exported_wrap(1)
                             end),
    ?CT(?MODULE,_,_), %% The fun
    ?CT(?MODULE,exported_wrap,[1]),
    ?CT(?MODULE,exported,[1]),
    ?CT(?MODULE,local,[1]),
    ?CT(?MODULE,local2,[1]),
    ?CT(?MODULE,local_tail,[1]),
    erlang:trace_pattern({?MODULE,'_','_'},false,[local]),
    shutdown(),
    ?NM,
    ok.

%% OTP-7399.
bit_syntax_test() ->
    setup([call]),
    erlang:trace_pattern({?MODULE,'_','_'},[],[local]),
    erlang:trace_pattern({?MODULE,slave,'_'},false,[local]),

    lambda_slave(fun() ->
                         6 = bs_sum_a(<<1,2,3>>, 0),
                         10 = bs_sum_b(0, <<1,2,3,4>>),
                         26 = bs_sum_c(<<3:4,5:4,7:4,11:4>>, 0)
                 end),
    ?CT(?MODULE,_,[]),		      %Ignore call to the fun.

    ?CT(?MODULE,bs_sum_a,[<<1,2,3>>,0]),
    ?CT(?MODULE,bs_sum_a,[<<2,3>>,1]),
    ?CT(?MODULE,bs_sum_a,[<<3>>,3]),
    ?CT(?MODULE,bs_sum_a,[<<>>,6]),

    ?CT(?MODULE,bs_sum_b,[0,<<1,2,3,4>>]),
    ?CT(?MODULE,bs_sum_b,[1,<<2,3,4>>]),
    ?CT(?MODULE,bs_sum_b,[3,<<3,4>>]),
    ?CT(?MODULE,bs_sum_b,[6,<<4>>]),
    ?CT(?MODULE,bs_sum_b,[10,<<>>]),

    ?CT(?MODULE,bs_sum_c,[<<3:4,5:4,7:4,11:4>>, 0]),
    ?CT(?MODULE,bs_sum_c,[<<5:4,7:4,11:4>>, 3]),
    ?CT(?MODULE,bs_sum_c,[<<7:4,11:4>>, 8]),
    ?CT(?MODULE,bs_sum_c,[<<11:4>>, 15]),
    ?CT(?MODULE,bs_sum_c,[<<>>, 26]),

    erlang:trace_pattern({?MODULE,'_','_'},false,[local]),
    shutdown(),
    ?NM,

    ok.

bs_sum_a(<<H,T/binary>>, Acc) -> bs_sum_a(T, H+Acc);
bs_sum_a(<<>>, Acc) -> Acc.

bs_sum_b(Acc, <<H,T/binary>>) -> bs_sum_b(H+Acc, T);
bs_sum_b(Acc, <<>>) -> Acc.

bs_sum_c(<<H:4,T/bits>>, Acc) -> bs_sum_c(T, H+Acc);
bs_sum_c(<<>>, Acc) -> Acc.

return_test() ->
    setup([call]),
    erlang:trace_pattern({?MODULE,'_','_'},[{'_',[],[{return_trace}]}],
                         [local]),
    erlang:trace_pattern({erlang,hash,'_'},[{'_',[],[{return_trace}]}],
                         [local]),
    erlang:trace_pattern({?MODULE,slave,'_'},false,[local]),
    [1,1,1,1] = apply_slave(?MODULE,exported_wrap,[1]),
    ?CT(?MODULE,exported_wrap,[1]), 
    ?CT(?MODULE,exported,[1]),
    ?CT(?MODULE,local,[1]),
    ?CT(?MODULE,local2,[1]),
    ?CT(?MODULE,local_tail,[1]),
    ?CT(erlang,hash,[1,1]),
    ?RF(erlang,hash,2,1),
    ?RF(?MODULE,local_tail,1,[1,1]),
    ?RF(?MODULE,local2,1,[1,1]),
    ?RF(?MODULE,local,1,[1,1,1]),
    ?RF(?MODULE,exported,1,[1,1,1,1]),
    ?RF(?MODULE,exported_wrap,1,[1,1,1,1]),
    shutdown(),
    setup([call,return_to]),
    erlang:trace_pattern({?MODULE,'_','_'},[],
                         [local]),
    erlang:trace_pattern({erlang,hash,'_'},[],
                         [local]),
    erlang:trace_pattern({?MODULE,slave,'_'},false,[local]),
    [1,1,1,1] = apply_slave(?MODULE,exported_wrap,[1]),
    ?CT(?MODULE,exported_wrap,[1]), 
    ?CT(?MODULE,exported,[1]),
    ?CT(?MODULE,local,[1]),
    ?CT(?MODULE,local2,[1]),
    ?CT(?MODULE,local_tail,[1]),
    ?CT(erlang,hash,[1,1]),
    ?RT(?MODULE,local_tail,1),
    ?RT(?MODULE,local,1),
    ?RT(?MODULE,exported,1),
    ?RT(?MODULE,slave,2),
    shutdown(),
    setup([call,return_to]),
    erlang:trace_pattern({?MODULE,'_','_'},[{'_',[],[{return_trace}]}],
                         [local]),
    erlang:trace_pattern({erlang,hash,'_'},[{'_',[],[{return_trace}]}],
                         [local]),
    erlang:trace_pattern({?MODULE,slave,'_'},false,[local]),
    [1,1,1,1] = apply_slave(?MODULE,exported_wrap,[1]),
    ?CT(?MODULE,exported_wrap,[1]), 
    ?CT(?MODULE,exported,[1]),
    ?CT(?MODULE,local,[1]),
    ?CT(?MODULE,local2,[1]),
    ?CT(?MODULE,local_tail,[1]),
    ?CT(erlang,hash,[1,1]),
    ?RF(erlang,hash,2,1),
    ?RT(?MODULE,local_tail,1),
    ?RF(?MODULE,local_tail,1,[1,1]),
    ?RF(?MODULE,local2,1,[1,1]),
    ?RT(?MODULE,local,1),
    ?RF(?MODULE,local,1,[1,1,1]),
    ?RT(?MODULE,exported,1),
    ?RF(?MODULE,exported,1,[1,1,1,1]),
    ?RF(?MODULE,exported_wrap,1,[1,1,1,1]),
    ?RT(?MODULE,slave,2),
    shutdown(),
    ?NM,

    %% Test a regression where turning off return_to tracing
    %% on yourself would cause a segfault.
    Pid = setup([call,return_to]),
    erlang:trace_pattern({'_','_','_'},[],[local]),
    apply_slave(erlang,trace,[Pid, false, [all]]),
    shutdown(),

    ok.

on_and_off_test() ->
    Pid = setup([call]),
    1 = erlang:trace_pattern({?MODULE,local_tail,1},[],[local]),
    erlang:trace_pattern({?MODULE,slave,'_'},false,[local]),
    LocalTail = fun() ->
                        local_tail(1)
                end,
    [1,1] = lambda_slave(LocalTail),
    ?CT(?MODULE,local_tail,[1]),
    erlang:trace(Pid,true,[return_to]),
    [1,1] = lambda_slave(LocalTail),
    ?CT(?MODULE,local_tail,[1]),
    ?RT(?MODULE,_,_),
    0 = erlang:trace_pattern({?MODULE,local_tail,1},[],[global]),
    [1,1] = lambda_slave(LocalTail),
    ?NM,
    1 = erlang:trace_pattern({?MODULE,exported_wrap,1},[],[global]),
    [1,1,1,1] = apply_slave(?MODULE,exported_wrap,[1]),
    ?CT(?MODULE,exported_wrap,[1]),
    1 = erlang:trace_pattern({?MODULE,exported_wrap,1},[],[local]),
    [1,1,1,1] = apply_slave(?MODULE,exported_wrap,[1]),
    ?CT(?MODULE,exported_wrap,[1]),
    ?RT(?MODULE,slave,2),
    1 = erlang:trace_pattern({erlang,hash,2},[],[local]),
    [1,1,1,1] = apply_slave(?MODULE,exported_wrap,[1]),
    ?CT(?MODULE,exported_wrap,[1]),
    ?CT(erlang,hash,[1,1]),
    ?RT(?MODULE,local_tail,1),
    ?RT(?MODULE,slave,2),
    erlang:trace(Pid,true,[timestamp]),
    [1,1,1,1] = apply_slave(?MODULE,exported_wrap,[1]),
    ?CTT(?MODULE,exported_wrap,[1]),
    ?CTT(erlang,hash,[1,1]),
    ?RTT(?MODULE,local_tail,1),
    ?RTT(?MODULE,slave,2),
    erlang:trace(Pid,false,[return_to,timestamp]),
    [1,1,1,1] = apply_slave(?MODULE,exported_wrap,[1]),
    ?CT(?MODULE,exported_wrap,[1]),
    ?CT(erlang,hash,[1,1]),
    erlang:trace(Pid,true,[return_to]),
    1 = erlang:trace_pattern({erlang,hash,2},[],[]),
    [1,1,1,1] = apply_slave(?MODULE,exported_wrap,[1]),
    ?CT(?MODULE,exported_wrap,[1]),
    ?CT(erlang,hash,[1,1]),
    ?RT(?MODULE,slave,2),
    1 = erlang:trace_pattern({?MODULE,exported_wrap,1},[],[]),
    [1,1,1,1] = apply_slave(?MODULE,exported_wrap,[1]),
    ?CT(?MODULE,exported_wrap,[1]),
    ?CT(erlang,hash,[1,1]),
    shutdown(),
    erlang:trace_pattern({'_','_','_'},false,[local]),
    N = erlang:trace_pattern({erlang,'_','_'},true,[local]),
    case erlang:trace_pattern({erlang,'_','_'},false,[local]) of
        N -> 
            ok;
        Else ->
            exit({number_mismatch, {expected, N}, {got, Else}})
    end,
    case erlang:trace_pattern({erlang,'_','_'},false,[local]) of
        N -> 
            ok;
        Else2 ->
            exit({number_mismatch, {expected, N}, {got, Else2}})
    end,
    M = erlang:trace_pattern({erlang,'_','_'},true,[]),
    case erlang:trace_pattern({erlang,'_','_'},false,[]) of
        M -> 
            ok;
        Else3 ->
            exit({number_mismatch, {expected, N}, {got, Else3}})
    end,
    case erlang:trace_pattern({erlang,'_','_'},false,[]) of
        M -> 
            ok;
        Else4 ->
            exit({number_mismatch, {expected, N}, {got, Else4}})
    end,
    ?NM,
    ok.

systematic_on_off(Config) when is_list(Config) ->
    setup([call]),
    Local = combinations([local,meta,call_count,call_time]),
    [systematic_on_off_1(Flags) || Flags <- Local],

    %% Make sure that we don't get any trace messages when trace
    %% is supposed to be off.
    receive_no_next(500).

systematic_on_off_1(Local) ->
    io:format("~p\n", [Local]),

    %% Global off.
    verify_trace_info(false, []),
    1 = erlang:trace_pattern({?MODULE,exported_wrap,1}, true, Local),
    verify_trace_info(false, Local),
    1 = erlang:trace_pattern({?MODULE,exported_wrap,1}, false, [global]),
    verify_trace_info(false, Local),
    1 = erlang:trace_pattern({?MODULE,exported_wrap,1}, false, Local),
    verify_trace_info(false, []),

    %% Global on.
    1 = erlang:trace_pattern({?MODULE,exported_wrap,1}, true, [global]),
    verify_trace_info(true, []),
    1 = erlang:trace_pattern({?MODULE,exported_wrap,1}, false, Local),
    verify_trace_info(true, []),
    1 = erlang:trace_pattern({?MODULE,exported_wrap,1}, false, [global]),
    verify_trace_info(false, []),

    %% Implicitly turn off global call trace.
    1 = erlang:trace_pattern({?MODULE,exported_wrap,1}, true, [global]),
    verify_trace_info(true, []),
    1 = erlang:trace_pattern({?MODULE,exported_wrap,1}, true, Local),
    verify_trace_info(false, Local),

    %% Implicitly turn off local call trace.
    1 = erlang:trace_pattern({?MODULE,exported_wrap,1}, true, [global]),
    verify_trace_info(true, []),

    %% Turn off global call trace. Everything should be off now.
    1 = erlang:trace_pattern({?MODULE,exported_wrap,1}, false, [global]),
    verify_trace_info(false, []),

    ok.

verify_trace_info(Global, Local) ->
    case erlang:trace_info({?MODULE,exported_wrap,1}, all) of
        {all,false} ->
            false = Global,
            [] = Local;
        {all,Ps} ->
            io:format("~p\n", [Ps]),
            [verify_trace_info(P, Global, Local) || P <- Ps]
    end,
    global_call(Global, Local),
    local_call(Local),
    ok.

verify_trace_info({traced,global}, true, []) -> ok;
verify_trace_info({traced,local}, false, _) -> ok;
verify_trace_info({match_spec,[]}, _, _) -> ok;
verify_trace_info({meta_match_spec,[]}, _, _) -> ok;
verify_trace_info({LocalFlag,Bool}, _, Local) when is_boolean(Bool) ->
    try
        Bool = lists:member(LocalFlag, Local)
    catch
        error:_ ->
            ct:fail("Line ~p: {~p,~p}, false, ~p\n", [?LINE,LocalFlag,Bool,Local])
    end;
verify_trace_info({meta,Pid}, false, Local) when is_pid(Pid) ->
    true = lists:member(meta, Local);
verify_trace_info({call_time,_}, false, Local) ->
    true = lists:member(call_time, Local);
verify_trace_info({call_count,_}, false, Local) ->
    true = lists:member(call_time, Local).

global_call(Global, Local) ->
    apply_slave(?MODULE, exported_wrap, [global_call]),
    case Global of
        false ->
            recv_local_call(Local, [global_call]);
        true ->
            ?CT(?MODULE, exported_wrap, [global_call])
    end.

local_call(Local) ->
    lambda_slave(fun() -> exported_wrap(local_call) end),
    recv_local_call(Local, [local_call]).

recv_local_call(Local, Args) ->
    case lists:member(local, Local) of
        false ->
            ok;
        true ->
            ?CT(?MODULE, exported_wrap, Args)
    end,
    case lists:member(meta, Local) of
        false ->
            ok;
        true ->
            ?CTT(?MODULE, exported_wrap, Args)
    end,
    ok.

combinations([_]=One) ->
    [One];
combinations([H|T]) ->
    Cs = combinations(T),
    [[H|C] || C <- Cs] ++ Cs.

stack_grow_test() ->    
    setup([call,return_to]),
    1 = erlang:trace_pattern({?MODULE,loop,4},
                             [{'_',[],[{return_trace}]}],[local]),
    erlang:trace_pattern({?MODULE,slave,'_'},false,[local]),
    Num = 1 bsl 15,
    Fun = 
    fun(_F,0) -> ok; 
       (F,N) -> 
            receive _A -> 
                        receive _B -> 
                                    receive _C ->
                                                F(F,N-1) 
                                    end
                        end 
            end 
    end, 
    apply_slave_async(?MODULE,loop,[{hej,hopp},[a,b,c],4.5,Num]),
    Fun(Fun,Num + 1),
    ?NM, 
    ok.


info_test() ->
    Flags1 = lists:sort([call,return_to]),
    Pid = setup(Flags1),
    Prog = [{['$1'],[{is_integer,'$1'}],[{message, false}]},
            {'_',[],[]}],
    erlang:trace_pattern({?MODULE,exported_wrap,1},Prog,[local]),
    erlang:trace_pattern({?MODULE,slave,'_'},false,[local]),
    Self = self(),
    {flags,L} = erlang:trace_info(Pid,flags),
    case lists:sort(L) of
        Flags1 ->
            ok;
        Wrong1 ->
            exit({bad_result, {erlang,trace_info,[Pid,flags]},
                  {expected, Flags1}, {got, Wrong1}})
    end,
    {tracer,Tracer} = erlang:trace_info(Pid,tracer),
    case Tracer of
        Self ->
            ok;
        Wrong2 ->
            exit({bad_result, {erlang,trace_info,[Pid,tracer]},
                  {expected, Self}, {got, Wrong2}})
    end,
    {traced,local} = erlang:trace_info({?MODULE,exported_wrap,1},traced),
    {match_spec, MS} = 
    erlang:trace_info({?MODULE,exported_wrap,1},match_spec),
    case MS of
        Prog ->
            ok;
        Wrong3 ->
            exit({bad_result, {erlang,trace_info,
                               [{?MODULE,exported_wrap,1},
                                match_spec]},
                  {expected, Prog}, {got, Wrong3}})
    end,
    erlang:garbage_collect(self()),
    receive
    after 1 ->
              ok
    end,
    io:format("~p~n",[MS]),
    {match_spec,MS2} = 
    erlang:trace_info({?MODULE,exported_wrap,1},match_spec),
    io:format("~p~n",[MS2]),
    erlang:trace_pattern({?MODULE,exported_wrap,1},[],[]),
    {traced,global} = 
    erlang:trace_info({?MODULE,exported_wrap,1},traced),
    {match_spec,[]} = 
    erlang:trace_info({?MODULE,exported_wrap,1},match_spec),
    {traced,undefined} = 
    erlang:trace_info({?MODULE,exported_wrap,2},traced),
    {match_spec,undefined} = 
    erlang:trace_info({?MODULE,exported_wrap,2},match_spec),
    {traced,false} = erlang:trace_info({?MODULE,exported,1},traced),
    {match_spec,false} = 
    erlang:trace_info({?MODULE,exported,1},match_spec),
    shutdown(),
    ok.

delete_test(Config) ->
    Priv = proplists:get_value(priv_dir, Config),
    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "trace_local_dummy"),
    {ok,trace_local_dummy} = c:c(File, [{outdir,Priv}]),
    code:purge(trace_local_dummy),
    code:delete(trace_local_dummy),
    0 = erlang:trace_pattern({trace_local_dummy,'_','_'},true,[local]),
    ?NM,
    ok.



%%% exception_test %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exception_test(Opts) ->
    {ProcFlags,PatFlags} = 
    case proplists:get_bool(meta, Opts) of
        true  -> {[timestamp],[meta]};
        false -> {[call,return_to,timestamp],[local]}
    end,
    case proplists:get_bool(nocatch, Opts) of
        false ->
            Exceptions = exceptions(),
            exception_test_setup(ProcFlags, PatFlags),
            lists:foreach(
              fun ({Func,Args}) ->
                      exception_test(Opts, Func, Args)
              end,
              Exceptions),
            shutdown();
        true ->
            Exceptions = exceptions(),
            lists:foreach(
              fun ({Func,Args}) ->
                      exception_test_setup(
                        [procs|ProcFlags],
                        PatFlags),
                      exception_test(Opts, Func, Args),
                      shutdown()
              end,
              Exceptions)
    end,
    ok.

exceptions() ->
    Ref = make_ref(),
    N   = 200000,
    LiL = seq(1, N-1, N),	% Long Improper List
    LL  = seq(1, N, []),  	% Long List
    [{{erlang,exit},  [done]},
     {{erlang,error}, [1.0]},
     {{erlang,error}, [Ref,[]]},
     {{erlang,throw}, [4711]},
     {{erlang,'++'},    [[17],seventeen]},
     {{erlang,'++'},    [Ref,[125.125]]},
     {{?MODULE,match},  [ref,Ref]},
     {{?MODULE,match},  [Ref,Ref]},
     {{?MODULE,clause}, [ref,Ref]},
     {{?MODULE,clause}, [Ref,Ref]},
     {{?MODULE,id},     [4711.0]},
     {{?MODULE,undef},  [[Ref|Ref]]},
     {{?MODULE,lists_reverse}, [LiL,[]]},
     {{?MODULE,lists_reverse}, [LL,[]]}].

exception_test_setup(ProcFlags, PatFlags) ->
    Pid = setup(ProcFlags),
    io:format("=== exception_test_setup(~p, ~p): ~p~n", 
              [ProcFlags,PatFlags,Pid]),
    Mprog = [{'_',[],[{exception_trace}]}],
    erlang:trace_pattern({?MODULE,'_','_'}, Mprog, PatFlags),
    erlang:trace_pattern({?MODULE,slave,'_'},false,PatFlags),
    [1,1,1,1,1] =
    [erlang:trace_pattern({erlang,F,A}, Mprog, PatFlags)
     || {F,A} <- [{exit,1},{error,1},{error,2},{throw,1},{'++',2}]],
    1 = erlang:trace_pattern({lists,reverse,2}, Mprog, PatFlags),
    ok.

-record(exc_opts, {nocatch=false, meta=false}).

exception_test(Opts, Func0, Args0) ->
    io:format("=== exception_test(~p, ~p, ~p)~n", 
              [Opts,Func0,abbr(Args0)]),
    Apply = proplists:get_bool(apply, Opts),
    Function = proplists:get_bool(function, Opts),
    Nocatch = proplists:get_bool(nocatch, Opts),
    Meta = proplists:get_bool(meta, Opts),
    ExcOpts = #exc_opts{nocatch=Nocatch,meta=Meta},

    %% Func0 and Args0 are for the innermost call, now we will
    %% wrap them in wrappers...
    {Func1,Args1} =
    case Function of
        true  -> {fun exc/2,[Func0,Args0]};
        false -> {Func0,Args0}
    end,

    {Func,Args} = 
    case Apply of
        true  -> {{erlang,apply},[Func1,Args1]};
        false -> {Func1,Args1}
    end,

    R1 = exc_slave(ExcOpts, Func, Args),
    Stack2 = [{?MODULE,exc_top,3,[]},{?MODULE,slave,2,[]}],
    Stack3 = [{?MODULE,exc,2,[]}|Stack2],
    Rs = 
    case x_exc_top(ExcOpts, Func, Args) of % Emulation
        {crash,{Reason,Stack}}=R when is_list(Stack) ->
            [R,
             {crash,{Reason,Stack++Stack2}},
             {crash,{Reason,Stack++Stack3}}];
        R ->
            [R]
    end,
    exception_validate(R1, Rs),
    case R1 of
        {crash,Crash} ->
            expect({trace_ts,exit,Crash});
        _ when not Meta ->
            expect({rtt,{?MODULE,slave,2}});
        _ ->
            ok
    end,
    expect({nm}).

exception_validate(R0, Rs0) ->
    R = clean_location(R0),
    Rs = [clean_location(E) || E <- Rs0],
    exception_validate_1(R, Rs).

exception_validate_1(R1, [R2|Rs]) ->
    case [R1|R2] of
        [R|R] ->
            ok;
        [{crash,{badarg,[{lists,reverse,[L1a,L1b],_}|T]}}|
         {crash,{badarg,[{lists,reverse,[L2a,L2b],_}|T]}}] ->
            same({crash,{badarg,[{lists,reverse,
                                  [lists:reverse(L1b, L1a),[]],[]}|T]}},
                 {crash,{badarg,[{lists,reverse,
                                  [lists:reverse(L2b, L2a),[]],[]}|T]}});
        _ when is_list(Rs), Rs =/= [] ->
            exception_validate(R1, Rs)
    end.

clean_location({crash,{Reason,Stk0}}) ->
    Stk = [{M,F,A,[]} || {M,F,A,_} <- Stk0],
    {crash,{Reason,Stk}};
clean_location(Term) -> Term.

concurrency(_Config) ->
    N = erlang:system_info(schedulers),

    %% Spawn 2*N processes that spin in a tight infinite loop,
    %% and one process that will turn on and off local call
    %% trace on the infinite_loop/0 function. We expect the
    %% emulator to crash if there is a memory barrier bug or
    %% if an aligned word-sized write is not atomic.

    Ps0 = [spawn_monitor(fun() -> infinite_loop() end) ||
           _ <- lists:seq(1, 2*N)],
    OnAndOff = fun() -> concurrency_on_and_off() end,
    Ps1 = [spawn_monitor(OnAndOff)|Ps0],
    timer:sleep(1000),

    %% Now spawn off N more processes that turn on off and off
    %% a local trace pattern.
    Ps = [spawn_monitor(OnAndOff) || _ <- lists:seq(1, N)] ++ Ps1,
    timer:sleep(1000),

    %% Clean up.
    [exit(Pid, kill) || {Pid,_} <- Ps],
    [receive
         {'DOWN',Ref,process,Pid,killed} -> ok
     end || {Pid,Ref} <- Ps],
    erlang:trace_pattern({?MODULE,infinite_loop,0}, false, [local]),
    ok.

concurrency_on_and_off() ->
    1 = erlang:trace_pattern({?MODULE,infinite_loop,0}, true, [local]),
    1 = erlang:trace_pattern({?MODULE,infinite_loop,0}, false, [local]),
    concurrency_on_and_off().

infinite_loop() ->
    infinite_loop().

%%% Tracee target functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%

loop(D1,D2,D3,0) ->
    io:format("~p~n",[[D1,D2,D3]]),
    0;
loop(D1,D2,D3,N) ->
    max(N,loop(D1,D2,D3,N-1)).

exported_wrap(Val) ->
    exported(Val).

exported(Val) ->
    [Val | local(Val)]. %% Non tail recursive local call

local(Val) ->
    [Val | local2(Val)]. %% Non tail recursive local call

local2(Val) ->
    local_tail(Val). %% Tail recursive call

local_tail(Val) ->
    [Val , erlang:hash(1,1)].



%%% exc_slave/3 tracee target functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%

exc_top(ExcOpts, Func, Args) ->
    case ExcOpts#exc_opts.nocatch of
        false ->
            try exc_jump(Func, Args) of
                Value ->
                    {value,Value}
            catch
                Class:Reason ->
                    {Class,Reason}
            end;
        true ->
            {value,exc_jump(Func, Args)}
    end.

%% x_* functions emulate the non-x_* ones.
%% x_* functions below x_exc_top
%% return {value,Value} or {Class,Reason}.
%% The only possible place for exception
%% is below exc/2.
x_exc_top(ExcOpts, Func, Args) ->
    Rtt = not ExcOpts#exc_opts.meta,
    expect({ctt,{?MODULE,exc_top},[ExcOpts,Func,Args]}),
    case x_exc_jump(ExcOpts, Func, Args) of
        Result when not ExcOpts#exc_opts.nocatch ->
            expect([Rtt,{rtt,{?MODULE,exc_top,3}},
                    ?LINE,{rft,{?MODULE,exc_top,3},Result}]),
            Result;
        {value,_}=Result ->

            expect([Rtt,{rtt,{?MODULE,exc_top,3}},
                    ?LINE,{rft,{?MODULE,exc_top,3},Result}]),
            Result;
        {exit,Reason}=CR ->
            expect({eft,{?MODULE,exc_top,3},CR}),
            {crash,Reason};
        {error,Reason}=CR ->
            expect({eft,{?MODULE,exc_top,3},CR}),
            {crash,{Reason,x_exc_stacktrace()}};
        CR ->
            expect({eft,{?MODULE,exc_top,3},CR}),
            {crash,CR}
    end.

exc_jump(Func, Args) ->
    exc(Func, Args, jump).

x_exc_jump(ExcOpts, Func, Args) ->
    expect({ctt,{?MODULE,exc_jump},[Func,Args]}),
    case x_exc(ExcOpts, Func, Args, jump) of
        {value,Value}=Result ->
            expect({rft,{?MODULE,exc_jump,2},Value}),
            Result;
        CR ->
            expect({eft,{?MODULE,exc_jump,2},CR}),
            CR
    end.

exc(Func, Args, jump) ->
    exc(Func, Args, do);
exc(Func, Args, do) ->
    exc(Func, Args).

x_exc(ExcOpts, Func, Args, jump) ->
    expect({ctt,{?MODULE,exc},[Func,Args,jump]}),
    case x_exc(ExcOpts, Func, Args, do) of
        {value,Value}=Result ->
            expect({rft,{?MODULE,exc,3},Value}),
            Result;
        CR ->
            expect({eft,{?MODULE,exc,3},CR}),
            CR
    end;
x_exc(ExcOpts, Func, Args, do) ->
    expect({ctt,{?MODULE,exc},[Func,Args,do]}),
    case x_exc(ExcOpts, Func, Args) of
        {value,Value}=Result ->
            expect({rft,{?MODULE,exc,3},Value}),
            Result;
        CR ->
            expect({eft,{?MODULE,exc,3},CR}),
            CR
    end.

exc({erlang,apply}, [{M,F},A]) ->
    erlang:apply(M, F, id(A));
exc({erlang,apply}, [F,A]) ->
    erlang:apply(F, id(A));
exc({erlang,error}, [E]) ->
    erlang:error(id(E));
exc({erlang,error}, [E,S]) ->
    erlang:error(E, id(S));
exc({erlang,exit}, [E]) ->
    erlang:exit(id(E));
exc({erlang,throw}, [E]) ->
    erlang:throw(id(E));
exc({erlang,'++'}, [A,B]) ->
    erlang:'++'(A, id(B));
exc({?MODULE,match}, [A,B]) ->
    match(A, id(B));
exc({?MODULE,clause}, [A,B]) ->
    clause(A, id(B));
exc({?MODULE,id}, [E]) ->
    id(id(E));
exc({?MODULE,undef}, [E]) ->
    undef(id(E));
exc({?MODULE,lists_reverse}, [A,B]) ->
    lists_reverse(A, id(B));
exc(Func, [A,B]) when is_function(Func, 2) ->
    Func(A, id(B)).

x_exc(ExcOpts, {erlang,apply}=Func0, [{_,_}=Func,Args]=Args0) ->
    expect({ctt,{?MODULE,exc},[Func0,Args0]}),
    x_exc_body(ExcOpts, Func, Args, true);
x_exc(ExcOpts, {erlang,apply}=Func0, [Func,Args]=Args0) 
  when is_function(Func, 2)->
    expect({ctt,{?MODULE,exc},[Func0,Args0]}),
    x_exc_func(ExcOpts, Func, Args, Args);
x_exc(ExcOpts, {_,_}=Func, Args) ->
    expect({ctt,{?MODULE,exc},[Func,Args]}),
    x_exc_body(ExcOpts, Func, Args, false);
x_exc(ExcOpts, Func0, [_,Args]=Args0)
  when is_function(Func0, 2) ->
    expect({ctt,{?MODULE,exc},[Func0,Args0]}),
    x_exc_func(ExcOpts, Func0, Args0, Args).

x_exc_func(ExcOpts, Func, [Func1,Args1]=Args, Id) ->
    %% Assumes the called fun =:= fun exc/2,
    %% will utterly fail otherwise.
    Rtt = not ExcOpts#exc_opts.meta,
    {module,M} = erlang:fun_info(Func, module),
    {name,F} = erlang:fun_info(Func, name),
    expect([{ctt,{?MODULE,id},[Id]},
            ?LINE,{rft,{?MODULE,id,1},Id},
            ?LINE,Rtt,{rtt,{?MODULE,exc,2}},
            ?LINE,{ctt,{M,F},Args}]),
    case x_exc(ExcOpts, Func1, Args1) of
        {value,Value}=Result ->
            expect([{rft,{M,F,2},Value},
                    ?LINE,{rft,{?MODULE,exc,2},Value}]),
            Result;
        CR ->
            expect([{eft,{M,F,2},CR},
                    ?LINE,{eft,{?MODULE,exc,2},CR}]),
            CR
    end.

x_exc_body(ExcOpts, {M,F}=Func, Args, Apply) ->
    Nocatch = ExcOpts#exc_opts.nocatch,
    Rtt = not ExcOpts#exc_opts.meta,
    Id = case Apply of
             true  -> Args;
             false -> lists:last(Args)
         end,
    expect([{ctt,{?MODULE,id},[Id]},
            ?LINE,{rft,{?MODULE,id,1},Id},
            ?LINE,Rtt,{rtt,{?MODULE,exc,2}},
            ?LINE,{ctt,{M,F},Args}]),
    Arity = length(Args),
    try exc(Func, Args) of
        Value ->
            x_exc_value(Rtt, M, F, Args, Arity, Value),
            case expect() of
                {rtt,{M,F,Arity}} when Rtt, Apply ->
                    %% We may get the above when
                    %% applying a BIF.
                    expect({rft,{?MODULE,exc,2},Value});
                {rtt,{?MODULE,exc,2}} when Rtt, not Apply ->
                    %% We may get the above when
                    %% calling a BIF.
                    expect({rft,{?MODULE,exc,2},Value});
                {rft,{?MODULE,exc,2},Value} ->
                    ok
            end,
            {value,Value}
    catch
        Thrown when Nocatch ->
            CR = {error,{nocatch,Thrown}},
            x_exc_exception(Rtt, M, F, Args, Arity, CR),
            expect({eft,{?MODULE,exc,2},CR}),
            CR;
        Class:Reason ->
            CR = {Class,Reason},
            x_exc_exception(Rtt, M, F, Args, Arity, CR),
            expect({eft,{?MODULE,exc,2},CR}),
            CR
    end.

x_exc_value(Rtt, ?MODULE, lists_reverse, [La,Lb], 2, R) ->
    L = lists:reverse(Lb, La),
    expect([fun ({ctt,{lists,reverse},[L1,L2]}) ->
                    same(L, lists:reverse(L2, L1)),
                    next;
                (Msg) ->
                    same({rft,{lists,reverse,2},R}, Msg),
                    same(R, lists:reverse(L, [])),
                    done
            end,
            ?LINE,Rtt,{rtt,{?MODULE,lists_reverse,2}},
            ?LINE,{rft,{?MODULE,lists_reverse,2},R}]);
x_exc_value(_Rtt, M, F, _, Arity, Value) ->
    expect({rft,{M,F,Arity},Value}).

x_exc_exception(_Rtt, ?MODULE, lists_reverse, [La,Lb], 2, CR) ->
    L = lists:reverse(Lb, La),
    expect([fun ({ctt,{lists,reverse},[L1,L2]}) ->
                    same(L, lists:reverse(L2, L1)),
                    next;
                (Msg) ->
                    same({eft,{lists,reverse,2},CR}, Msg),
                    done
            end,
            ?LINE,{eft,{?MODULE,lists_reverse,2},CR}]);
x_exc_exception(Rtt, ?MODULE, undef, [_], 1, {Class,Reason}=CR) ->
    expect([{ctt,{erlang,Class},[Reason]},
            ?LINE,{eft,{erlang,Class,1},CR},
            ?LINE,Rtt,{rtt,{error_handler,crash,1}},
            ?LINE,{eft,{?MODULE,undef,1},CR}]);
x_exc_exception(_Rtt, M, F, _, Arity, CR) ->
    expect({eft,{M,F,Arity},CR}).

x_exc_stacktrace() ->
    x_exc_stacktrace(erlang:get_stacktrace()).
%% Truncate stacktrace to below exc/2
x_exc_stacktrace([{?MODULE,x_exc,4,_}|_]) -> [];
x_exc_stacktrace([{?MODULE,x_exc_func,4,_}|_]) -> [];
x_exc_stacktrace([{?MODULE,x_exc_body,4,_}|_]) -> [];
x_exc_stacktrace([{?MODULE,exc,2,_}|_]) -> [];
x_exc_stacktrace([H|T]) ->
    [H|x_exc_stacktrace(T)].



match(A, B) ->
    A = B.

clause(A, A) ->
    A.

id(Id) ->
    Id.

undef(X) ->
    ?MODULE:undef(X, X). % undef

lists_reverse(A, B) ->
    lists:reverse(A, B).



%%% Tracee (slave) handling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%

slave(Dest, Sync) ->
    Dest ! Sync,
    receive
        {From,Tag,{apply,M,F,A}} when is_pid(From) ->
            ?dbgformat("Apply: ~p:~p/~p (~p)~n",[M,F,length(A),A]),
            Res = apply(M,F,A),
            ?dbgformat("done Apply: ~p:~p/~p (~p)~n",[M,F,length(A),A]),
            From ! {Tag,Res},
            slave(From, Tag);
        {From,Tag,{lambda,Fun}} when is_pid(From) ->
            Res = Fun(),
            From ! {Tag,Res},
            slave(From, Tag);
        {From,Tag,{exc_top,Catch,Func,Args}} when is_pid(From) ->
            ?dbgformat("Exc: ~p ~p~p ~n",[Catch,Func,Args]),
            Res = exc_top(Catch, Func, Args),
            ?dbgformat("done Exc: ~p ~p~p ~n",[Catch,Func,Args]),
            From ! {Tag,Res},
            slave(From,Tag);
        die ->
            exit(normal)
    end.

setup(ProcFlags) ->
    trace_off(),
    flush(100),
    Self = self(),
    Sync = make_ref(),
    Pid = spawn(fun () -> slave(Self, Sync) end),
    Mref = erlang:monitor(process, Pid),
    receive 
        Sync ->
            put(slave, {Pid,Mref}),
            case ProcFlags of
                [] -> ok;
                _ ->
                    erlang:trace(Pid, true, ProcFlags)
            end,
            Pid
    end.

shutdown() ->
    trace_off(),
    case get(slave) of
        {Pid,Mref} ->
            try erlang:is_process_alive(Pid) of
                true ->
                    Pid ! die,
                    receive
                        {'DOWN',Mref,process,Pid,Reason} ->
                            Reason
                    end;
                _ ->
                    not_alive
            catch _:_ ->
                    undefined
            end;
        _ ->
            undefined
    end.

trace_off() ->
    erlang:trace_pattern({'_','_','_'},false,[]),
    erlang:trace_pattern({'_','_','_'},false,[local]),
    erlang:trace_pattern({'_','_','_'},false,[meta]),
    erlang:trace(all, false, [all]).


apply_slave_async(M,F,A) ->
    {Pid,Mref} = get(slave),
    spawn(?MODULE,apply_slave_async,[M,F,A,Pid,Mref]),
    Pid.

apply_slave_async(M,F,A,Pid,Mref) ->
    Tag = make_ref(),
    Pid ! {self(),Tag,{apply,M,F,A}},
    result(Tag, Mref).

apply_slave(M,F,A) ->
    request({apply,M,F,A}).

lambda_slave(Fun) ->
    request({lambda,Fun}).

exc_slave(Opts, Func, Args) ->
    try request({exc_top,Opts,Func,Args})
    catch
        Reason ->
            {crash,Reason}
    end.

request(Request) ->
    Tag = make_ref(),
    {Pid,Mref} = get(slave),
    Pid ! {self(),Tag,Request},
    result(Tag, Mref).

result(Tag, Mref) ->
    receive
        {Tag,Result} ->
            receive
                Tag ->
                    Result
            end;
        {'DOWN',Mref,process,_Pid,Reason} ->
            throw(Reason)
    end.



%%% Some receive helpers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%

receive_next() ->
    receive_next(?DEFAULT_RECEIVE_TIMEOUT).

receive_next(TO) ->
    receive
        M ->
            M
    after TO ->
              ct:fail(timeout)
    end.

receive_no_next(TO) ->
    receive M ->
                ct:fail({unexpected_message,[M|flush(TO)]})
    after TO ->
              ok
    end.

flush(T) ->
    receive
        M ->
            [M|flush(T)]
    after T ->
              []
    end.



%%% Helpers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%

%% Do not build garbage
%%
seq(M, N, R) when M =< N ->
    seq(M, N-1, [N|R]);
seq(_, _, R) -> R.

%% Do not call traced lists:reverse
reverse(L) ->
    reverse(L, []).
%%
reverse([], R) -> R;
reverse([H|T], R) ->
    reverse(T, [H|R]).

%% Abbreviate large complex terms to avoid croaking printout
%%
abbr(Term) ->
    abbr(Term, 20).
%%
abbr(Tuple, N) when is_tuple(Tuple) ->
    list_to_tuple(abbr_tuple(Tuple, N, 1));
abbr(List, N) when is_list(List) ->
    abbr_list(List, N, []);
abbr(Term, _) -> Term.
%%
abbr_tuple(Tuple, N, J) when J =< size(Tuple) ->
    if J > N; N =< 0 ->
           ['...'];
       true ->
           [abbr(element(J, Tuple), N-1)|abbr_tuple(Tuple, J+1, N)]
    end;
abbr_tuple(_, _, _) ->
    [].
%%
abbr_list(_, 0, R) ->
    case io_lib:printable_list(R) of
        true ->
            reverse(R, "...");
        false ->
            reverse(R, '...')
    end;
abbr_list([H|T], N, R) ->
    M = N-1,
    abbr_list(T, M, [abbr(H, M)|R]);
abbr_list(T, _, R) ->
    reverse(R, T).
