%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011. All Rights Reserved.
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
-export([loaded/1, a_function/1, a_called_function/1, dec/1, nif_dec/1]).

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

-include_lib("test_server/include/test_server.hrl").

%% When run in test server.
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2, not_run/1]).
-export([basic/1, on_and_off/1, info/1,
	 pause_and_restart/1, scheduling/1, called_function/1, combo/1, 
	 bif/1, nif/1]).

init_per_testcase(_Case, Config) ->
    ?line Dog=test_server:timetrap(test_server:seconds(400)),
    erlang:trace_pattern({'_','_','_'}, false, [local,meta,call_time,call_count]),
    erlang:trace_pattern(on_load, false, [local,meta,call_time,call_count]),
    timer:now_diff(now(),now()),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    erlang:trace_pattern({'_','_','_'}, false, [local,meta,call_time,call_count]),
    erlang:trace_pattern(on_load, false, [local,meta,call_time,call_count]),
    erlang:trace(all, false, [all]),
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    case test_server:is_native(trace_call_time_SUITE) of
	true -> [not_run];
	false ->
	    [basic, on_and_off, info, pause_and_restart, scheduling,
	     combo, bif, nif, called_function]
    end.

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


not_run(Config) when is_list(Config) ->
    {skipped,"Native code"}.

basic(suite) ->
    [];
basic(doc) ->
    ["Tests basic call count trace"];
basic(Config) when is_list(Config) ->
    ?line P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    ?line M = 1000,
    %%
    ?line 1 = erlang:trace_pattern({?MODULE,seq,  '_'}, true, [call_time]),
    ?line 2 = erlang:trace_pattern({?MODULE,seq_r,'_'}, true, [call_time]),
    ?line Pid = setup(),
    ?line {L,  T1} = execute(Pid, fun() -> seq(1, M, fun(X) -> (X+1) end) end),
    ?line ok = check_trace_info({?MODULE, seq,   3}, [{Pid, M, 0, 0}], T1),
    ?line ok = check_trace_info({?MODULE, seq_r, 3}, [], none),

    ?line {Lr, T2} = execute(Pid, fun() -> seq_r(1, M, fun(X) -> (X+1) end) end),
    ?line ok = check_trace_info({?MODULE, seq,   3}, [{Pid, M, 0, 0}], T1),
    ?line ok = check_trace_info({?MODULE, seq_r, 3}, [{Pid, 1, 0, 0}], T2/M),
    ?line ok = check_trace_info({?MODULE, seq_r, 4}, [{Pid, M, 0, 0}], T2),
    ?line L = lists:reverse(Lr),

    %%
    ?line P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    ?line Pid ! quit,
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


on_and_off(suite) ->
    [];
on_and_off(doc) ->
    ["Tests turning trace parameters on and off"];
on_and_off(Config) when is_list(Config) ->
    ?line P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    ?line M = 100,
    %%
    ?line 1 = erlang:trace_pattern({?MODULE,seq,'_'}, true, [call_time]),
    ?line Pid = setup(),
    ?line {L, T1} = execute(Pid, {?MODULE, seq, [1, M, fun(X) -> X+1 end]}),
    ?line ok = check_trace_info({?MODULE, seq, 3}, [{Pid, M, 0, 0}], T1),

    ?line N = erlang:trace_pattern({?MODULE,'_','_'}, true, [call_time]),
    ?line {L, T2} = execute(Pid, fun() -> seq(1, M, fun(X) -> X+1 end) end),
    ?line ok = check_trace_info({?MODULE, seq, 3}, [{Pid, M, 0, 0}], T2),

    ?line P = erlang:trace_pattern({'_','_','_'}, true, [call_time]),
    ?line {L, T3} = execute(Pid, fun() -> seq(1, M, fun(X) -> X+1 end) end),
    ?line ok = check_trace_info({?MODULE, seq, 3}, [{Pid, M, 0, 0}], T3),

    ?line 1 = erlang:trace_pattern({?MODULE,seq,'_'}, false, [call_time]),
    ?line ok = check_trace_info({?MODULE, seq, 3}, false, none),
    ?line {L, _T4} = execute(Pid, fun() -> seq(1, M, fun(X) -> X+1 end) end),
    ?line ok = check_trace_info({?MODULE, seq, 3}, false, none),
    ?line ok = check_trace_info({?MODULE, seq_r, 4}, [], none),
    ?line {Lr, T5} = execute(Pid, fun() -> seq_r(1, M, fun(X) -> X+1 end) end),
    ?line ok = check_trace_info({?MODULE, seq_r, 4}, [{Pid,M,0,0}], T5),

    ?line N = erlang:trace_pattern({?MODULE,'_','_'}, false, [call_time]),
    ?line ok = check_trace_info({?MODULE, seq_r, 4}, false, none),
    ?line {Lr, _T6} = execute(Pid, fun() -> seq_r(1, M, fun(X) -> X+1 end) end),
    ?line ok = check_trace_info({?MODULE, seq_r, 4}, false, none),
    ?line L = lists:reverse(Lr),
    %%
    ?line Pid ! quit,
    ?line P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

info(suite) ->
    [];
info(doc) ->
    ["Tests the trace_info BIF"];
info(Config) when is_list(Config) ->
    ?line P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    %%
    ?line 1 = erlang:trace_pattern({?MODULE,seq,3}, true, [call_time]),
    ?line {call_time,[]} = erlang:trace_info({?MODULE,seq,3}, call_time),
    ?line 1 = erlang:trace_pattern({?MODULE,seq,'_'}, pause, [call_time]),
    ?line {call_time,[]} = erlang:trace_info({?MODULE,seq,3}, call_time),
    ?line {all,[_|_]=L} = erlang:trace_info({?MODULE,seq,3}, all),
    ?line {value,{call_time,[]}} = lists:keysearch(call_time, 1, L),
    ?line 1 = erlang:trace_pattern({?MODULE,seq,'_'}, restart, [call_time]),
    ?line {call_time,[]} = erlang:trace_info({?MODULE,seq,3}, call_time),
    ?line 1 = erlang:trace_pattern({?MODULE,seq,'_'}, false, [call_time]),
    ?line {call_time,false} = erlang:trace_info({?MODULE,seq,3}, call_time),
    ?line {all,false} = erlang:trace_info({?MODULE,seq,3}, all),
    %%
    ?line P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pause_and_restart(suite) ->
    [];
pause_and_restart(doc) ->
    ["Tests pausing and restarting call time counters"];
pause_and_restart(Config) when is_list(Config) ->
    ?line P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    ?line M = 100,
    ?line Pid = setup(),
    %%
    ?line 1 = erlang:trace_pattern({?MODULE,seq,'_'}, true, [call_time]),
    ?line ok = check_trace_info({?MODULE, seq, 3}, [], none),
    ?line {L, T1} = execute(Pid, fun() -> seq(1, M, fun(X) -> X+1 end) end),
    ?line ok = check_trace_info({?MODULE, seq, 3}, [{Pid,M,0,0}], T1),
    ?line 1 = erlang:trace_pattern({?MODULE,seq,'_'}, pause, [call_time]),
    ?line ok = check_trace_info({?MODULE, seq, 3}, [{Pid,M,0,0}], T1),
    ?line {L, T2} = execute(Pid, fun() -> seq(1, M, fun(X) -> X+1 end) end),
    ?line ok = check_trace_info({?MODULE, seq, 3}, [{Pid,M,0,0}], T2),
    ?line 1 = erlang:trace_pattern({?MODULE,seq,'_'}, restart, [call_time]),
    ?line ok = check_trace_info({?MODULE, seq, 3}, [], none),
    ?line {L, T3} = execute(Pid, fun() -> seq(1, M, fun(X) -> X+1 end) end),
    ?line ok = check_trace_info({?MODULE, seq, 3}, [{Pid,M,0,0}], T3),
    %%
    ?line Pid ! quit,
    ?line P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scheduling(suite) ->
    [];
scheduling(doc) ->
    ["Tests in/out scheduling of call time counters"];
scheduling(Config) when is_list(Config) ->
    ?line P  = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    ?line M  = 1000000,
    ?line Np = erlang:system_info(schedulers_online),
    ?line F  = 12,

    %% setup load processes
    %% (single, no internal calls)

    ?line erlang:trace_pattern({?MODULE,loaded,1}, true, [call_time]),

    ?line Pids     = [setup() || _ <- lists:seq(1, F*Np)],
    ?line {_Ls,T1} = execute(Pids, {?MODULE,loaded,[M]}),
    ?line [Pid ! quit || Pid <- Pids],

    %% logic dictates that each process will get ~ 1/F of the schedulers time

    ?line {call_time, CT} = erlang:trace_info({?MODULE,loaded,1}, call_time),

    ?line lists:foreach(fun (Pid) ->
	    ?line ok = case check_process_time(lists:keysearch(Pid, 1, CT), M, F, T1) of
		schedule_time_error ->
		    test_server:comment("Warning: Failed time ratio"),
		    ok;
		Other -> Other
	    end
	end, Pids),
    ?line P  = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

combo(suite) ->
    [];
combo(doc) ->
    ["Tests combining local call trace and meta trace with call time trace"];
combo(Config) when is_list(Config) ->
    ?line Self = self(),
    ?line Nbc = 3,
    ?line MetaMs = [{'_',[],[{return_trace}]}],
    ?line Flags = lists:sort([call, return_to]),
    ?line LocalTracer = spawn_link(fun () -> relay_n(5 + Nbc + 3, Self) end),
    ?line MetaTracer = spawn_link(fun () -> relay_n(9 + Nbc + 3, Self) end),
    ?line 2 = erlang:trace_pattern({?MODULE,seq_r,'_'}, [], [local]),
    ?line 2 = erlang:trace_pattern({?MODULE,seq_r,'_'}, true, [call_time]),
    ?line 2 = erlang:trace_pattern({?MODULE,seq_r,'_'}, MetaMs, [{meta,MetaTracer}]),
    ?line 2 = erlang:trace_pattern({?MODULE,seq_r,'_'}, true, [call_count]),

    % bifs
    ?line 2 = erlang:trace_pattern({erlang, term_to_binary, '_'}, [], [local]),
    ?line 2 = erlang:trace_pattern({erlang, term_to_binary, '_'}, true, [call_time]),
    ?line 2 = erlang:trace_pattern({erlang, term_to_binary, '_'}, MetaMs, [{meta,MetaTracer}]),
    %% not implemented
    %?line 2 = erlang:trace_pattern({erlang, term_to_binary, '_'}, true, [call_count]),

    ?line 1 = erlang:trace(Self, true, [{tracer,LocalTracer} | Flags]),
    %%
    ?line {traced,local} =
	erlang:trace_info({?MODULE,seq_r,3}, traced),
    ?line {match_spec,[]} =
	erlang:trace_info({?MODULE,seq_r,3}, match_spec),
    ?line {meta,MetaTracer} =
	erlang:trace_info({?MODULE,seq_r,3}, meta),
    ?line {meta_match_spec,MetaMs} =
	erlang:trace_info({?MODULE,seq_r,3}, meta_match_spec),
    ?line ok = check_trace_info({?MODULE, seq_r, 3}, [], none),

    %% check empty trace_info for ?MODULE:seq_r/3
    ?line {all,[_|_]=TraceInfo}     = erlang:trace_info({?MODULE,seq_r,3}, all),
    ?line {value,{traced,local}}    = lists:keysearch(traced, 1, TraceInfo),
    ?line {value,{match_spec,[]}}   = lists:keysearch(match_spec, 1, TraceInfo),
    ?line {value,{meta,MetaTracer}} = lists:keysearch(meta, 1, TraceInfo),
    ?line {value,{meta_match_spec,MetaMs}} = lists:keysearch(meta_match_spec, 1, TraceInfo),
    ?line {value,{call_count,0}} = lists:keysearch(call_count, 1, TraceInfo),
    ?line {value,{call_time,[]}} = lists:keysearch(call_time, 1, TraceInfo),

    %% check empty trace_info for erlang:term_to_binary/1
    ?line {all, [_|_] = TraceInfoBif} = erlang:trace_info({erlang, term_to_binary, 1}, all),
    ?line {value,{traced,local}}     = lists:keysearch(traced, 1, TraceInfoBif),
    ?line {value,{match_spec,[]}}    = lists:keysearch(match_spec, 1, TraceInfoBif),
    ?line {value,{meta, MetaTracer}}  = lists:keysearch(meta, 1, TraceInfoBif),
    ?line {value,{meta_match_spec,MetaMs}} = lists:keysearch(meta_match_spec, 1, TraceInfoBif),
    %% not implemented
    ?line {value,{call_count,false}} = lists:keysearch(call_count, 1, TraceInfoBif),
    %?line {value,{call_count,0}} = lists:keysearch(call_count, 1, TraceInfoBif),
    ?line {value,{call_time,[]}} = lists:keysearch(call_time, 1, TraceInfoBif),

    %%
    ?line [3,2,1] = seq_r(1, 3, fun(X) -> X+1 end),
    ?line T0 = now(),
    ?line with_bif(Nbc),
    ?line T1 = now(),
    ?line TimeB = timer:now_diff(T1,T0),
    %%

    ?line List = collect(100),
    ?line {MetaR, LocalR} =
	lists:foldl(
	  fun ({P,X}, {M,L}) when P == MetaTracer ->
		  {[X|M],L};
	      ({P,X}, {M,L}) when P == LocalTracer ->
		  {M,[X|L]}
	  end,
	  {[],[]},
	  List),
    ?line Meta = lists:reverse(MetaR),
    ?line Local = lists:reverse(LocalR),

    ?line [?CTT(Self,{?MODULE,seq_r,[1,3,_]}),
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

    ?line [?CT(Self,{?MODULE,seq_r,[1,3,_]}),
	   ?CT(Self,{?MODULE,seq_r,[1,3,_,[]]}),
	   ?CT(Self,{?MODULE,seq_r,[2,3,_,[1]]}),
	   ?CT(Self,{?MODULE,seq_r,[3,3,_,[2,1]]}),
	   ?RT(Self,{?MODULE,combo,1}),
	   ?CT(Self,{erlang,term_to_binary,[3]}), % bif
	   ?RT(Self,{?MODULE,with_bif,1}),
	   ?CT(Self,{erlang,term_to_binary,[2]}),
	   ?RT(Self,{?MODULE,with_bif,1})
	] = Local,

    ?line ok = check_trace_info({?MODULE, seq_r, 3}, [{Self,1,0,0}], 1),
    ?line ok = check_trace_info({?MODULE, seq_r, 4}, [{Self,3,0,0}], 1),
    ?line ok = check_trace_info({?MODULE, seq_r, 3}, [{Self,1,0,0}], 1),
    ?line ok = check_trace_info({?MODULE, seq_r, 4}, [{Self,3,0,0}], 1),
    ?line ok = check_trace_info({erlang, term_to_binary, 1}, [{self(), Nbc - 1, 0, 0}], TimeB),
    %%
    ?line erlang:trace_pattern({'_','_','_'}, false, [local,meta,call_time]),
    ?line erlang:trace_pattern(on_load, false, [local,meta,call_time]),
    ?line erlang:trace(all, false, [all]),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bif(suite) ->
    [];
bif(doc) ->
    ["Tests tracing of bifs"];
bif(Config) when is_list(Config) ->
    ?line P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    ?line M = 1000000,
    %%
    ?line 2 = erlang:trace_pattern({erlang, binary_to_term, '_'}, true, [call_time]),
    ?line 2 = erlang:trace_pattern({erlang, term_to_binary, '_'}, true, [call_time]),
    ?line Pid = setup(),
    ?line {L, T1} = execute(Pid, fun() -> with_bif(M) end),

    ?line ok = check_trace_info({erlang, binary_to_term, 1}, [{Pid, M - 1, 0, 0}], T1/2),
    ?line ok = check_trace_info({erlang, term_to_binary, 1}, [{Pid, M - 1, 0, 0}], T1/2),

    % disable term2binary

    ?line 2 = erlang:trace_pattern({erlang, term_to_binary, '_'}, false, [call_time]),

    ?line {L, T2} = execute(Pid, fun() -> with_bif(M) end),

    ?line ok = check_trace_info({erlang, binary_to_term, 1}, [{Pid, M*2 - 2, 0, 0}], T1/2 + T2),
    ?line ok = check_trace_info({erlang, term_to_binary, 1}, false, none),

    %%
    ?line P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    ?line Pid ! quit,
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nif(suite) ->
    [];
nif(doc) ->
    ["Tests tracing of nifs"];
nif(Config) when is_list(Config) ->
    load_nif(Config),
    ?line P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    ?line M = 1000000,
    %%
    ?line 1 = erlang:trace_pattern({?MODULE, nif_dec,  '_'}, true, [call_time]),
    ?line 1 = erlang:trace_pattern({?MODULE, with_nif, '_'}, true, [call_time]),
    ?line Pid = setup(),
    ?line {_, T1} = execute(Pid, fun() -> with_nif(M) end),

    % the nif is called M - 1 times, the last time the function with 'with_nif'
    % returns ok and does not call the nif.
    ?line ok = check_trace_info({?MODULE, nif_dec,  1}, [{Pid, M-1, 0, 0}], T1/5*4),
    ?line ok = check_trace_info({?MODULE, with_nif, 1}, [{Pid, M, 0, 0}], T1/5),

    %%
    ?line P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    ?line Pid ! quit,
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

called_function(suite) ->
    [];
called_function(doc) ->
    ["Tests combining nested function calls and that the time accumulates to the right function"];
called_function(Config) when is_list(Config) ->
    ?line P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    ?line M = 2100,
    ?line Pid = setup(),
    %%
    ?line 1 = erlang:trace_pattern({?MODULE,a_function,'_'}, true, [call_time]),
    ?line {L, T1} = execute(Pid, {?MODULE, a_function, [M]}),
    ?line ok = check_trace_info({?MODULE, a_function, 1}, [{Pid, M, 0, 0}], T1),

    ?line 1 = erlang:trace_pattern({?MODULE,a_called_function,'_'}, true, [call_time]),
    ?line {L, T2} = execute(Pid, {?MODULE, a_function, [M]}),
    ?line ok = check_trace_info({?MODULE, a_function, 1}, [{Pid, M+M, 0, 0}], T1 + M*?SINGLE_CALL_US_TIME),
    ?line ok = check_trace_info({?MODULE, a_called_function, 1}, [{Pid, M, 0, 0}], T2),


    ?line 1 = erlang:trace_pattern({?MODULE,dec,'_'}, true, [call_time]),
    ?line {L, T3} = execute(Pid, {?MODULE, a_function, [M]}),
    ?line ok = check_trace_info({?MODULE, a_function, 1}, [{Pid, M+M+M, 0, 0}], T1 + (M+M)*?SINGLE_CALL_US_TIME),
    ?line ok = check_trace_info({?MODULE, a_called_function, 1}, [{Pid, M+M, 0, 0}], T2 + M*?SINGLE_CALL_US_TIME ),
    ?line ok = check_trace_info({?MODULE, dec, 1}, [{Pid, M, 0, 0}], T3),

    ?line Pid ! quit,
    ?line P = erlang:trace_pattern({'_','_','_'}, false, [call_time]),
    ok.

%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The Tests
%%%

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local helpers


load_nif(Config) ->
    ?line Path = ?config(data_dir, Config),
    ?line ok = erlang:load_nif(filename:join(Path,"trace_nif"), 0).


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
    ?line Sum = S*1000000 + Us,
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
    Pid = spawn_link(fun() -> loop() end),
    ?line 1 = erlang:trace(Pid, true, [call]),
    Pid.

execute(Pids, Mfa) when is_list(Pids) ->
    T0 = now(),
    [P  ! {self(), execute, Mfa} || P <- Pids],
    As = [receive {P, answer, Answer} -> Answer end || P <- Pids],
    T1 = now(),
    {As, timer:now_diff(T1,T0)};
execute(P, Mfa) ->
    T0 = now(),
    P  ! {self(), execute, Mfa},
    A  = receive {P, answer, Answer} -> Answer end,
    T1 = now(),
    {A, timer:now_diff(T1,T0)}.



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
