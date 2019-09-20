%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2016. All Rights Reserved.
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

%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Define to run outside of test server
%%%
%%% -define(STANDALONE,1).
%%%
%%%
%%% Define for debug output
%%%
%%% -define(debug,1).

-module(trace_call_time_SUITE).

%% Exported end user tests

-export([seq/3, seq_r/3]).
-export([loaded/1, a_function/1, a_called_function/1, dec/1, nif_dec/1, dead_tracer/1,
        return_stop/1]).

-define(US_ERROR, 10000).
-define(R_ERROR, 0.8).
-define(SINGLE_CALL_US_TIME, 10).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Result examination macros

-define(CT(P,MFA),{trace,P,call,MFA}).
-define(CTT(P, MFA),{trace_ts,P,call,MFA,{_,_,_}}).
-define(RF(P,MFA,V),{trace,P,return_from,MFA,V}).
-define(RFT(P,MFA,V),{trace_ts,P,return_from,MFA,V,{_,_,_}}).
-define(RT(P,MFA),{trace,P,return_to,MFA}).
-define(RTT(P,MFA),{trace_ts,P,return_to,MFA,{_,_,_}}).

-ifdef(debug).
-define(dbgformat(A,B),io:format(A,B)).
-else.
-define(dbgformat(A,B),noop).
-endif.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("common_test/include/ct.hrl").

%% When run in test server.
-export([all/0, suite/0,
	 init_per_testcase/2, end_per_testcase/2, not_run/1]).
-export([basic/1, on_and_off/1, info/1,
         disable_ongoing/1,
	 pause_and_restart/1, scheduling/1, called_function/1, combo/1, 
	 bif/1, nif/1]).

init_per_testcase(_Case, Config) ->
    erlang:trace_pattern({'_','_','_'}, false, [local,meta,call_time,call_count]),
    erlang:trace_pattern(on_load, false, [local,meta,call_time,call_count]),
    timer:now_diff(now(),now()),
    Config.

end_per_testcase(_Case, _Config) ->
    erlang:trace_pattern({'_','_','_'}, false, [local,meta,call_time,call_count]),
    erlang:trace_pattern(on_load, false, [local,meta,call_time,call_count]),
    erlang:trace(all, false, [all]),
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 10}}].

all() -> 
    case test_server:is_native(trace_call_time_SUITE) of
	true -> [not_run];
	false ->
	    [basic, on_and_off, info, pause_and_restart, scheduling,
             disable_ongoing,
	     combo, bif, nif, called_function, dead_tracer, return_stop]
    end.

not_run(Config) when is_list(Config) ->
    {skipped,"Native code"}.

%% Tests basic call time trace
basic(Config) when is_list(Config) ->
    P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    M = 1000,
    %%
    1 = erlang:trace_pattern({?MODULE,seq,  '_'}, true, [call_time]),
    2 = erlang:trace_pattern({?MODULE,seq_r,'_'}, true, [call_time]),
    Pid = setup(),
    {L,  T1} = execute(Pid, fun() -> seq(1, M, fun(X) -> (X+1) end) end),
    ok = check_trace_info({?MODULE, seq,   3}, [{Pid, M, 0, 0}], T1),
    ok = check_trace_info({?MODULE, seq_r, 3}, [], none),

    {Lr, T2} = execute(Pid, fun() -> seq_r(1, M, fun(X) -> (X+1) end) end),
    ok = check_trace_info({?MODULE, seq,   3}, [{Pid, M, 0, 0}], T1),
    ok = check_trace_info({?MODULE, seq_r, 3}, [{Pid, 1, 0, 0}], T2/M),
    ok = check_trace_info({?MODULE, seq_r, 4}, [{Pid, M, 0, 0}], T2),
    L = lists:reverse(Lr),

    %%
    P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    Pid ! quit,
    ok.

%% Tests disable ongoing call_time traced function
disable_ongoing(Config) when is_list(Config) ->
    P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    %%
    1 = erlang:trace_pattern({?MODULE,disong_a, 1}, true, [call_time]),
    1 = erlang:trace_pattern({?MODULE,disong_b, 1}, true, [call_time]),
    1 = erlang:trace_pattern({?MODULE,disong_c, 1}, true, [call_time]),
    1 = erlang:trace_pattern({?MODULE,disong_d, 0}, true, [call_time]),

    Pid = setup(),

    Self = self(),
    Pid  ! {self(), execute, fun() -> disong_a(Self) end},

    c_ready = receive M1 -> M1 end,

    1 = erlang:trace_pattern({?MODULE,disong_b,1}, false, [call_time]),

    Pid ! go_on,

    {Pid, answer, _} = receive M2 -> M2
                       after 1000 -> timeout end,

    P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    Pid ! quit,
    ok.

disong_a(Pid) ->
    ForceStack = id(a),
    disong_b(Pid),
    disong_d(),
    ForceStack.

disong_b(Pid) ->
    ForceStack = id(b),
    disong_c(Pid),
    ForceStack.

disong_c(Pid) ->
    Pid ! c_ready,
    receive
        go_on -> ok
    end.

disong_d() ->
    ok.

id(I) ->
    I.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% "Tests turning trace parameters on and off
on_and_off(Config) when is_list(Config) ->
    P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    M = 100,
    %%
    1 = erlang:trace_pattern({?MODULE,seq,'_'}, true, [call_time]),
    Pid = setup(),
    {L, T1} = execute(Pid, {?MODULE, seq, [1, M, fun(X) -> X+1 end]}),
    ok = check_trace_info({?MODULE, seq, 3}, [{Pid, M, 0, 0}], T1),

    N = erlang:trace_pattern({?MODULE,'_','_'}, true, [call_time]),
    {L, T2} = execute(Pid, fun() -> seq(1, M, fun(X) -> X+1 end) end),
    ok = check_trace_info({?MODULE, seq, 3}, [{Pid, M, 0, 0}], T2),

    P = erlang:trace_pattern({'_','_','_'}, true, [call_time]),
    {L, T3} = execute(Pid, fun() -> seq(1, M, fun(X) -> X+1 end) end),
    ok = check_trace_info({?MODULE, seq, 3}, [{Pid, M, 0, 0}], T3),

    1 = erlang:trace_pattern({?MODULE,seq,'_'}, false, [call_time]),
    ok = check_trace_info({?MODULE, seq, 3}, false, none),
    {L, _T4} = execute(Pid, fun() -> seq(1, M, fun(X) -> X+1 end) end),
    ok = check_trace_info({?MODULE, seq, 3}, false, none),
    ok = check_trace_info({?MODULE, seq_r, 4}, [], none),
    {Lr, T5} = execute(Pid, fun() -> seq_r(1, M, fun(X) -> X+1 end) end),
    ok = check_trace_info({?MODULE, seq_r, 4}, [{Pid,M,0,0}], T5),

    N = erlang:trace_pattern({?MODULE,'_','_'}, false, [call_time]),
    ok = check_trace_info({?MODULE, seq_r, 4}, false, none),
    {Lr, _T6} = execute(Pid, fun() -> seq_r(1, M, fun(X) -> X+1 end) end),
    ok = check_trace_info({?MODULE, seq_r, 4}, false, none),
    L = lists:reverse(Lr),
    %%
    Pid ! quit,
    P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Tests the trace_info BIF
info(Config) when is_list(Config) ->
    P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    %%
    1 = erlang:trace_pattern({?MODULE,seq,3}, true, [call_time]),
    {call_time,[]} = erlang:trace_info({?MODULE,seq,3}, call_time),
    1 = erlang:trace_pattern({?MODULE,seq,'_'}, pause, [call_time]),
    {call_time,[]} = erlang:trace_info({?MODULE,seq,3}, call_time),
    {all,[_|_]=L} = erlang:trace_info({?MODULE,seq,3}, all),
    {value,{call_time,[]}} = lists:keysearch(call_time, 1, L),
    1 = erlang:trace_pattern({?MODULE,seq,'_'}, restart, [call_time]),
    {call_time,[]} = erlang:trace_info({?MODULE,seq,3}, call_time),
    1 = erlang:trace_pattern({?MODULE,seq,'_'}, false, [call_time]),
    {call_time,false} = erlang:trace_info({?MODULE,seq,3}, call_time),
    {all,false} = erlang:trace_info({?MODULE,seq,3}, all),
    %%
    P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Tests pausing and restarting call time counters
pause_and_restart(Config) when is_list(Config) ->
    P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    M = 100,
    Pid = setup(),
    %%
    1 = erlang:trace_pattern({?MODULE,seq,'_'}, true, [call_time]),
    ok = check_trace_info({?MODULE, seq, 3}, [], none),
    {L, T1} = execute(Pid, fun() -> seq(1, M, fun(X) -> X+1 end) end),
    ok = check_trace_info({?MODULE, seq, 3}, [{Pid,M,0,0}], T1),
    1 = erlang:trace_pattern({?MODULE,seq,'_'}, pause, [call_time]),
    ok = check_trace_info({?MODULE, seq, 3}, [{Pid,M,0,0}], T1),
    {L, T2} = execute(Pid, fun() -> seq(1, M, fun(X) -> X+1 end) end),
    ok = check_trace_info({?MODULE, seq, 3}, [{Pid,M,0,0}], T2),
    1 = erlang:trace_pattern({?MODULE,seq,'_'}, restart, [call_time]),
    ok = check_trace_info({?MODULE, seq, 3}, [], none),
    {L, T3} = execute(Pid, fun() -> seq(1, M, fun(X) -> X+1 end) end),
    ok = check_trace_info({?MODULE, seq, 3}, [{Pid,M,0,0}], T3),
    %%
    Pid ! quit,
    P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Tests in/out scheduling of call time counters
scheduling(Config) when is_list(Config) ->
    P  = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    M  = 1000000,
    Np = erlang:system_info(schedulers_online),
    F  = 12,

    %% setup load processes
    %% (single, no internal calls)

    erlang:trace_pattern({?MODULE,loaded,1}, true, [call_time]),

    Pids     = [setup() || _ <- lists:seq(1, F*Np)],
    {_Ls,T1} = execute(Pids, {?MODULE,loaded,[M]}),
    [Pid ! quit || Pid <- Pids],

    %% logic dictates that each process will get ~ 1/F of the schedulers time

    {call_time, CT} = erlang:trace_info({?MODULE,loaded,1}, call_time),

    lists:foreach(fun (Pid) ->
                          ok = case check_process_time(lists:keysearch(Pid, 1, CT), M, F, T1) of
                                   schedule_time_error ->
                                       test_server:comment("Warning: Failed time ratio"),
                                       ok;
                                   Other -> Other
                               end
                  end, Pids),
    P  = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% "Tests combining local call trace and meta trace with call time trace
combo(Config) when is_list(Config) ->
    Self = self(),
    Nbc = 3,
    MetaMs = [{'_',[],[{return_trace}]}],
    Flags = lists:sort([call, return_to]),
    LocalTracer = spawn_link(fun () -> relay_n(5 + Nbc + 3, Self) end),
    MetaTracer = spawn_link(fun () -> relay_n(9 + Nbc + 3, Self) end),
    2 = erlang:trace_pattern({?MODULE,seq_r,'_'}, [], [local]),
    2 = erlang:trace_pattern({?MODULE,seq_r,'_'}, true, [call_time]),
    2 = erlang:trace_pattern({?MODULE,seq_r,'_'}, MetaMs, [{meta,MetaTracer}]),
    2 = erlang:trace_pattern({?MODULE,seq_r,'_'}, true, [call_count]),

    % bifs
    2 = erlang:trace_pattern({erlang, term_to_binary, '_'}, [], [local]),
    2 = erlang:trace_pattern({erlang, term_to_binary, '_'}, true, [call_time]),
    2 = erlang:trace_pattern({erlang, term_to_binary, '_'}, MetaMs, [{meta,MetaTracer}]),
    %% not implemented
    %2 = erlang:trace_pattern({erlang, term_to_binary, '_'}, true, [call_count]),

    1 = erlang:trace(Self, true, [{tracer,LocalTracer} | Flags]),
    %%
    {traced,local} =
    erlang:trace_info({?MODULE,seq_r,3}, traced),
    {match_spec,[]} =
    erlang:trace_info({?MODULE,seq_r,3}, match_spec),
    {meta,MetaTracer} =
    erlang:trace_info({?MODULE,seq_r,3}, meta),
    {meta_match_spec,MetaMs} =
    erlang:trace_info({?MODULE,seq_r,3}, meta_match_spec),
    ok = check_trace_info({?MODULE, seq_r, 3}, [], none),

    %% check empty trace_info for ?MODULE:seq_r/3
    {all,[_|_]=TraceInfo}     = erlang:trace_info({?MODULE,seq_r,3}, all),
    {value,{traced,local}}    = lists:keysearch(traced, 1, TraceInfo),
    {value,{match_spec,[]}}   = lists:keysearch(match_spec, 1, TraceInfo),
    {value,{meta,MetaTracer}} = lists:keysearch(meta, 1, TraceInfo),
    {value,{meta_match_spec,MetaMs}} = lists:keysearch(meta_match_spec, 1, TraceInfo),
    {value,{call_count,0}} = lists:keysearch(call_count, 1, TraceInfo),
    {value,{call_time,[]}} = lists:keysearch(call_time, 1, TraceInfo),

    %% check empty trace_info for erlang:term_to_binary/1
    {all, [_|_] = TraceInfoBif} = erlang:trace_info({erlang, term_to_binary, 1}, all),
    {value,{traced,local}}     = lists:keysearch(traced, 1, TraceInfoBif),
    {value,{match_spec,[]}}    = lists:keysearch(match_spec, 1, TraceInfoBif),
    {value,{meta, MetaTracer}}  = lists:keysearch(meta, 1, TraceInfoBif),
    {value,{meta_match_spec,MetaMs}} = lists:keysearch(meta_match_spec, 1, TraceInfoBif),
    %% not implemented
    {value,{call_count,false}} = lists:keysearch(call_count, 1, TraceInfoBif),
    %{value,{call_count,0}} = lists:keysearch(call_count, 1, TraceInfoBif),
    {value,{call_time,[]}} = lists:keysearch(call_time, 1, TraceInfoBif),

    %%
    [3,2,1] = seq_r(1, 3, fun(X) -> X+1 end),
    T0 = erlang:monotonic_time(),
    with_bif(Nbc),
    T1 = erlang:monotonic_time(),
    TimeB = erlang:convert_time_unit(T1-T0, native, microsecond),
    %%

    List = collect(100),
    {MetaR, LocalR} =
    lists:foldl(
      fun ({P,X}, {M,L}) when P == MetaTracer ->
              {[X|M],L};
          ({P,X}, {M,L}) when P == LocalTracer ->
              {M,[X|L]}
      end,
      {[],[]},
      List),
    Meta = lists:reverse(MetaR),
    Local = lists:reverse(LocalR),

    [?CTT(Self,{?MODULE,seq_r,[1,3,_]}),
     ?CTT(Self,{?MODULE,seq_r,[1,3,_,[]]}),
     ?CTT(Self,{?MODULE,seq_r,[2,3,_,[1]]}),
     ?CTT(Self,{?MODULE,seq_r,[3,3,_,[2,1]]}),
     ?RFT(Self,{?MODULE,seq_r,4},[3,2,1]),
     ?RFT(Self,{?MODULE,seq_r,4},[3,2,1]),
     ?RFT(Self,{?MODULE,seq_r,4},[3,2,1]),
     ?RFT(Self,{?MODULE,seq_r,3},[3,2,1]),
     ?CTT(Self,{erlang,term_to_binary,[3]}), % bif
     ?RFT(Self,{erlang,term_to_binary,1},<<131,97,3>>),
     ?CTT(Self,{erlang,term_to_binary,[2]}),
     ?RFT(Self,{erlang,term_to_binary,1},<<131,97,2>>)
    ] = Meta,

    [?CT(Self,{?MODULE,seq_r,[1,3,_]}),
     ?CT(Self,{?MODULE,seq_r,[1,3,_,[]]}),
     ?CT(Self,{?MODULE,seq_r,[2,3,_,[1]]}),
     ?CT(Self,{?MODULE,seq_r,[3,3,_,[2,1]]}),
     ?RT(Self,{?MODULE,combo,1}),
     ?CT(Self,{erlang,term_to_binary,[3]}), % bif
     ?RT(Self,{?MODULE,with_bif,1}),
     ?CT(Self,{erlang,term_to_binary,[2]}),
     ?RT(Self,{?MODULE,with_bif,1})
    ] = Local,

    ok = check_trace_info({?MODULE, seq_r, 3}, [{Self,1,0,0}], 1),
    ok = check_trace_info({?MODULE, seq_r, 4}, [{Self,3,0,0}], 1),
    ok = check_trace_info({?MODULE, seq_r, 3}, [{Self,1,0,0}], 1),
    ok = check_trace_info({?MODULE, seq_r, 4}, [{Self,3,0,0}], 1),
    ok = check_trace_info({erlang, term_to_binary, 1}, [{self(), Nbc - 1, 0, 0}], TimeB),
    %%
    erlang:trace_pattern({'_','_','_'}, false, [local,meta,call_time]),
    erlang:trace_pattern(on_load, false, [local,meta,call_time]),
    erlang:trace(all, false, [all]),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Tests tracing of bifs
bif(Config) when is_list(Config) ->
    P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    M = 5000000,
    %%
    2 = erlang:trace_pattern({erlang, binary_to_term, '_'}, true, [call_time]),
    2 = erlang:trace_pattern({erlang, term_to_binary, '_'}, true, [call_time]),
    Pid = setup(),
    {L, T1} = execute(Pid, fun() -> with_bif(M) end),

    ok = check_trace_info({erlang, binary_to_term, 1}, [{Pid, M - 1, 0, 0}], T1/2),
    ok = check_trace_info({erlang, term_to_binary, 1}, [{Pid, M - 1, 0, 0}], T1/2),

    % disable term2binary

    2 = erlang:trace_pattern({erlang, term_to_binary, '_'}, false, [call_time]),

    {L, T2} = execute(Pid, fun() -> with_bif(M) end),

    ok = check_trace_info({erlang, binary_to_term, 1}, [{Pid, M*2 - 2, 0, 0}], T1/2 + T2),
    ok = check_trace_info({erlang, term_to_binary, 1}, false, none),

    %%
    P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    Pid ! quit,
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Tests tracing of nifs
nif(Config) when is_list(Config) ->
    load_nif(Config),
    P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    M = 5000000,
    %%
    1 = erlang:trace_pattern({?MODULE, nif_dec,  '_'}, true, [call_time]),
    1 = erlang:trace_pattern({?MODULE, with_nif, '_'}, true, [call_time]),
    Pid = setup(),
    {_, T1} = execute(Pid, fun() -> with_nif(M) end),

    % the nif is called M - 1 times, the last time the function with 'with_nif'
    % returns ok and does not call the nif.
    ok = check_trace_info({?MODULE, nif_dec,  1}, [{Pid, M-1, 0, 0}], T1/2),
    ok = check_trace_info({?MODULE, with_nif, 1}, [{Pid, M, 0, 0}], T1/2),

    %%
    P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    Pid ! quit,
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Tests combining nested function calls and that the time accumulates to the right function
called_function(Config) when is_list(Config) ->
    P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    M = 2100,
    Pid = setup(),
    %%
    1 = erlang:trace_pattern({?MODULE,a_function,'_'}, true, [call_time]),
    {L, T1} = execute(Pid, {?MODULE, a_function, [M]}),
    ok = check_trace_info({?MODULE, a_function, 1}, [{Pid, M, 0, 0}], T1),

    1 = erlang:trace_pattern({?MODULE,a_called_function,'_'}, true, [call_time]),
    {L, T2} = execute(Pid, {?MODULE, a_function, [M]}),
    ok = check_trace_info({?MODULE, a_function, 1}, [{Pid, M+M, 0, 0}], T1 + M*?SINGLE_CALL_US_TIME),
    ok = check_trace_info({?MODULE, a_called_function, 1}, [{Pid, M, 0, 0}], T2),


    1 = erlang:trace_pattern({?MODULE,dec,'_'}, true, [call_time]),
    {L, T3} = execute(Pid, {?MODULE, a_function, [M]}),
    ok = check_trace_info({?MODULE, a_function, 1}, [{Pid, M+M+M, 0, 0}], T1 + (M+M)*?SINGLE_CALL_US_TIME),
    ok = check_trace_info({?MODULE, a_called_function, 1}, [{Pid, M+M, 0, 0}], T2 + M*?SINGLE_CALL_US_TIME ),
    ok = check_trace_info({?MODULE, dec, 1}, [{Pid, M, 0, 0}], T3),

    Pid ! quit,
    P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dead_tracer(Config) when is_list(Config) ->
    Self = self(),
    FirstTracer = tracer(),
    StartTracing = fun() -> turn_on_tracing(Self) end,
    tell_tracer(FirstTracer, StartTracing),
    [1,2,3,4,5,6,7,8] = seq(1, 8, fun(I) -> I + 1 end),
    Ref = erlang:monitor(process, FirstTracer),
    FirstTracer ! quit,
    receive
        {'DOWN',Ref,process,FirstTracer,normal} ->
            ok
    end,
    erlang:yield(),

    %% Collect and check that we only get call_time info for the current process.
    Info1 = collect_all_info(),
    [] = other_than_self(Info1),
    io:format("~p\n", [Info1]),

    %% Note that we have not turned off tracing for the current process,
    %% but that the tracer has terminated. No more call_time information should be recorded.
    [1,2,3] = seq(1, 3, fun(I) -> I + 1 end),
    [] = collect_all_info(),

    %% When we start a second tracer process, that tracer process must
    %% not inherit the tracing flags and the dead tracer (even though
    %% we used set_on_spawn).
    SecondTracer = tracer(),
    tell_tracer(SecondTracer, StartTracing),
    Seq20 = lists:seq(1, 20),
    Seq20 = seq(1, 20, fun(I) -> I + 1 end),
    Info2 = collect_all_info(),
    io:format("~p\n", [Info2]),
    [] = other_than_self(Info2),
    SecondTracer ! quit,

    ok.

other_than_self(Info) ->
    [{Pid,MFA} || {MFA,[{Pid,_,_,_}]} <- Info,
                  Pid =/= self()].

tell_tracer(Tracer, Fun) ->
    Tracer ! {execute,self(),Fun},
    receive
        {Tracer,executed} ->
            ok
    end.

tracer() ->
    spawn_link(fun Loop() ->
                      receive
                          quit ->
                              ok;
                          {execute,From,Fun} ->
                              Fun(),
                              From ! {self(),executed},
                              Loop()
                      end
              end).

turn_on_tracing(Pid) ->
    _ = erlang:trace(Pid, true, [call,set_on_spawn]),
    _ = erlang:trace_pattern({?MODULE,'_','_'}, true, [call_time]),
    _ = now(),
    ok.

collect_all_info() ->
    collect_all_info([{?MODULE,F,A} || {F,A} <- module_info(functions)] ++
                     erlang:system_info(snifs)).

collect_all_info([MFA|T]) ->
    CallTime = erlang:trace_info(MFA, call_time),
    erlang:trace_pattern(MFA, restart, [call_time]),
    case CallTime of
        {call_time,false} ->
            collect_all_info(T);
        {call_time,[]} ->
            collect_all_info(T);
        {call_time,[_|_]=List} ->
            [{MFA,List}|collect_all_info(T)]
    end;
collect_all_info([]) -> [].


%% OTP-16111: Verify call_time does not increase after traced function returns.
return_stop(_Config) ->
    P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    %%
    1 = erlang:trace_pattern({?MODULE,aaa,  '_'}, true, [call_time]),
    1 = erlang:trace_pattern({?MODULE,bbb,  '_'}, true, [call_time]),
    Pid = setup(),
    {aaa, T1} = execute(Pid, fun() -> aaa()  end),
    {call_time, [{Pid, 1, 0, US1}]} = erlang:trace_info({?MODULE,aaa,0}, call_time),
    io:format("T1=~p us, US1=~p us\n", [T1, US1]),
    true = (US1 =< T1),

    {call_time, [{Pid, 1, 0, US1}]} = erlang:trace_info({?MODULE,aaa,0}, call_time),

    execute(Pid, fun() -> loaded(1000000)  end),
    {call_time, [{Pid, 1, 0, US1}]} = erlang:trace_info({?MODULE,aaa,0}, call_time),

    {bbb,  _} = execute(Pid, fun() -> bbb()  end),
    {call_time, [{Pid, 1, 0, US1}]} = erlang:trace_info({?MODULE,aaa,0}, call_time),

    1 = erlang:trace_pattern({?MODULE,spinner,  1}, true, [call_time]),
    1 = erlang:trace_pattern({?MODULE,quicky,  0}, true, [call_time]),
    {spinner,  T2} = execute(Pid, fun() -> spinner(1000000)  end),
    {call_time, [{Pid, 1, SpS, SpUS}]} = erlang:trace_info({?MODULE,spinner,1}, call_time),
    Spinner = SpS*1000000 + SpUS,
    {call_time, [{Pid, 1, 0, Quicky}]} = erlang:trace_info({?MODULE,quicky,0}, call_time),
    io:format("T2=~p us, Spinner=~p us, Quicky=~p us\n", [T2, Spinner, Quicky]),

    %% Before fix: quicky() got attributed the call_time of its caller spinner().
    true = (Quicky =< Spinner),
    true = (Spinner =< T2),

    %%
    P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    Pid ! quit,
    ok.


aaa() ->
    aaa.
bbb() ->
   bbb.

spinner(N) ->
    quicky(),
    loaded(N),
    spinner.

quicky() ->
    done.


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local helpers

load_nif(Config) ->
    Path = proplists:get_value(data_dir, Config),
    ok = erlang:load_nif(filename:join(Path,"trace_nif"), 0).


%% Stack recursive seq
seq(Stop, Stop, Succ) when is_function(Succ) ->
    [Stop];
seq(Start, Stop, Succ) when is_function(Succ) ->
    [Start | seq(Succ(Start), Stop, Succ)].


a_function(1) -> a_called_function(1);
a_function(N) when N > 1 -> a_function(a_called_function(N)).

a_called_function(N) -> dec(N).

with_bif(1) -> ok;
with_bif(N) ->
    with_bif(erlang:binary_to_term(erlang:term_to_binary(N)) - 1).

with_nif(0) -> error;
with_nif(1) -> ok;
with_nif(N) ->
    with_nif(?MODULE:nif_dec(N)).


nif_dec(_) -> 0.

dec(N) ->
    loaded(10000),
    N - 1.

loaded(N) when N > 1 -> loaded(N - 1);
loaded(_) -> 5.


%% Tail recursive seq, result list is reversed
seq_r(Start, Stop, Succ) when is_function(Succ) ->
    seq_r(Start, Stop, Succ, []).

seq_r(Stop, Stop, _, R) ->
    [Stop | R];
seq_r(Start, Stop, Succ, R) ->
    seq_r(Succ(Start), Stop, Succ, [Start | R]).

% Check call time tracing data and print mismatches
check_trace_info(Mfa, [{Pid, C,_,_}] = Expect, Time) ->
    case erlang:trace_info(Mfa, call_time) of
        % Time tests are somewhat problematic. We want to know if Time (EXPECTED_TIME) and S*1000000 + Us (ACTUAL_TIME)
        % is the same.
        % If the ratio EXPECTED_TIME/ACTUAL_TIME is ~ 1 or if EXPECTED_TIME - ACTUAL_TIME is near zero, the test is ok.
        {call_time,[{Pid,C,S,Us}]} when S >= 0, Us >= 0,  abs(1 - Time/(S*1000000 + Us)) < ?R_ERROR; abs(Time - S*1000000 - Us) < ?US_ERROR ->
            ok;
        {call_time,[{Pid,C,S,Us}]} ->
            Sum = S*1000000 + Us,
            io:format("Expected ~p -> {call_time, ~p (Time ~p us)}~n - got ~w s. ~w us. = ~w us. - ~w -> delta ~w (ratio ~.2f, should be 1.0)~n",
                      [Mfa, Expect, Time, S, Us, Sum, Time, Sum - Time, Time/Sum]),
            time_error;
        Other ->
            io:format("Expected ~p -> {call_time, ~p (Time ~p us)}~n - got ~p~n", [ Mfa, Expect, Time, Other]),
            time_count_error
    end;
check_trace_info(Mfa, Expect, _) ->
    case erlang:trace_info(Mfa, call_time) of
        {call_time, Expect} ->
            ok;
        Other ->
            io:format("Expected ~p -> {call_time, ~p}~n - got ~p~n", [Mfa, Expect, Other]),
            result_not_expected_error
    end.


%check process time
check_process_time({value,{Pid, M, S, Us}}, M, F, Time) ->
    Sum = S*1000000 + Us,
    if
        abs(1 - (F/(Time/Sum))) < ?R_ERROR ->
            ok;
        true ->
            io:format("- Pid ~p, Got ratio ~.2f, expected ratio ~w~n", [Pid, Time/Sum,F]),
            schedule_time_error
    end;
check_process_time(Other, M, _, _) ->
    io:format(" - Got ~p, expected count ~w~n", [Other, M]),
    error.



%% Message relay process
relay_n(0, _) ->
    ok;
relay_n(N, Dest) ->
    receive Msg ->
                Dest ! {self(), Msg},
                relay_n(N-1, Dest)
    end.



%% Collect received messages
collect(Time) ->
    Ref = erlang:start_timer(Time, self(), done),
    L = lists:reverse(collect([], Ref)),
    ?dbgformat("Got: ~p~n",[L]),
    L.

collect(A, 0) ->
    receive
        Mess ->
            collect([Mess | A], 0)
    after 0 ->
              A
    end;
collect(A, Ref) ->
    receive
        {timeout, Ref, done} ->
            collect(A, 0);
        Mess ->
            collect([Mess | A], Ref)
    end.

setup() ->
    setup([]).

setup(Opts) ->
    Pid = spawn_link(fun() -> loop() end),
    1 = erlang:trace(Pid, true, [call|Opts]),
    Pid.

execute(Pids, Mfa) when is_list(Pids) ->
    T0 = erlang:monotonic_time(),
    [P  ! {self(), execute, Mfa} || P <- Pids],
    As = [receive {P, answer, Answer} -> Answer end || P <- Pids],
    T1 = erlang:monotonic_time(),
    {As, erlang:convert_time_unit(T1-T0, native, microsecond)};
execute(P, Mfa) ->
    T0 = erlang:monotonic_time(),
    P  ! {self(), execute, Mfa},
    A  = receive {P, answer, Answer} -> Answer end,
    T1 = erlang:monotonic_time(),
    {A, erlang:convert_time_unit(T1-T0, native, microsecond)}.



loop() ->
    receive
        quit ->
            ok;
        {Pid, execute, Fun } when is_function(Fun) ->
            Pid ! {self(), answer, erlang:apply(Fun, [])},
            loop();
        {Pid, execute, {M, F, A}} ->
            Pid ! {self(), answer, erlang:apply(M, F, A)},
            loop()
    end.
