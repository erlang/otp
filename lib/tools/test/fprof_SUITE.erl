%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2010. All Rights Reserved.
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
-module(fprof_SUITE).

-include_lib("test_server/include/test_server.hrl").

%% Test server framework exports
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, not_run/1]).

%% Test suites
-export([stack_seq/1, tail_seq/1, create_file_slow/1, spawn_simple/1,
	 imm_tail_seq/1, imm_create_file_slow/1, imm_compile/1,
	 cpu_create_file_slow/1]).

%% Other exports
-export([create_file_slow/2]).


%% Debug exports
-export([parse/1, verify/2]).
-export([spawn_simple_test/3]).


-define(line_trace,true).

%-define(debug,true).
-ifdef(debug).
-define(dbg(Str,Args), io:format(Str,Args)).
-else.
-define(dbg(Str,Args), ok).
-endif.



%%%---------------------------------------------------------------------
%%% Test suites
%%%---------------------------------------------------------------------



suite() -> [{suite_callbacks,[ts_install_scb]}].

all() -> 
case test_server:is_native(fprof_SUITE) of
  true -> [not_run];
  false ->
      [stack_seq, tail_seq, create_file_slow, spawn_simple,
       imm_tail_seq, imm_create_file_slow, imm_compile,
       cpu_create_file_slow]
end.

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.


not_run(Config) when is_list(Config) ->
    {skipped, "Native code"}.

%%%---------------------------------------------------------------------

stack_seq(doc) ->
    ["Tests a stack recursive variant of lists:seq/3"];
stack_seq(suite) ->
    [];
stack_seq(Config) when is_list(Config) ->
    ?line Timetrap = ?t:timetrap(?t:seconds(20)),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line TraceFile = 
	filename:join(PrivDir, ?MODULE_STRING"_stack_seq.trace"),
    ?line AnalysisFile = 
	filename:join(PrivDir, ?MODULE_STRING"_stack_seq.analysis"),
    ?line Start = 1,
    ?line Stop = 1000,
    ?line Succ = fun (X) -> X + 1 end,
    ?line ok = fprof:stop(kill),
    %%
    ?line TS0 = erlang:now(),
    ?line R0 = fprof:apply(fun seq/3, [Start, Stop, Succ], [{file, TraceFile}]),
    ?line TS1 = erlang:now(),
    ?line R = seq(Start, Stop, Succ),
    ?line TS2 = erlang:now(),
    ?line ok = fprof:profile(file, TraceFile),
    ?line ok = fprof:analyse(),
    ?line ok = fprof:analyse(dest, AnalysisFile),
    ?line ok = fprof:stop(),
    ?line R = R0,
    %%
    ?line {ok, [T, P]} = parse(AnalysisFile),
    ?line io:format("~p~n~n~p~n", [P, ets:tab2list(T)]),
    ?line ok = (catch verify(T, P)),
    ?line Proc = pid_to_list(self()),
    ?line case P of
	      [{analysis_options, _},
	       [{totals, _, Acc, _}],
	       [{Proc, _, undefined, _} | _]] ->
		  ok
	  end,
    %%
    ?line check_own_and_acc(TraceFile,AnalysisFile),
    %%
    ?line ets:delete(T),
    ?line file:delete(TraceFile),
    ?line file:delete(AnalysisFile),
    ?line ?t:timetrap_cancel(Timetrap),
    ?line Acc1 = ts_sub(TS1, TS0),
    ?line Acc2 = ts_sub(TS2, TS1),
    ?line io:format("ts:~w, fprof:~w, bare:~w.~n", [Acc, Acc1, Acc2]),
    {comment, io_lib:format("~p times slower", [Acc1/Acc2])}.

%%%---------------------------------------------------------------------

tail_seq(doc) ->
    ["Tests a tail recursive variant of lists:seq/3"];
tail_seq(suite) ->
    [];
tail_seq(Config) when is_list(Config) ->
    ?line Timetrap = ?t:timetrap(?t:seconds(10)),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line TraceFile = 
	filename:join(PrivDir, ?MODULE_STRING"_tail_seq.trace"),
    ?line AnalysisFile = 
	filename:join(PrivDir, ?MODULE_STRING"_tail_seq.analysis"),
    ?line Start = 1,
    ?line Stop = 1000,
    ?line Succ = fun (X) -> X + 1 end,
    ?line ok = fprof:stop(kill),
    %%
    ?line TS0 = erlang:now(),
    ?line R = seq_r(Start, Stop, Succ),
    ?line TS1 = erlang:now(),
    %%
    ?line R1 = fprof:apply(fun seq_r/3, [Start, Stop, Succ], 
			  [{file, TraceFile}]),
    ?line TS2 = erlang:now(),
    ?line ok = fprof:profile([{file,TraceFile}]),
    ?line ok = fprof:analyse(),
    ?line ok = fprof:analyse(dest, AnalysisFile),
    ?line ok = fprof:stop(),
    ?line R = R1,
    %%
    ?line {ok, [T, P]} = parse(AnalysisFile),
    ?line io:format("~p~n~n~p~n", [P, ets:tab2list(T)]),
    ?line ok = verify(T, P),
    ?line Proc = pid_to_list(self()),
    ?line case P of
	      [{analysis_options, _},
	       [{totals, _, Acc, _}],
	       [{Proc, _, undefined, _} | _]] ->
		  ok
	  end,
    %%
    ?line check_own_and_acc(TraceFile,AnalysisFile),
    %%
    ?line ets:delete(T),
    ?line file:delete(TraceFile),
    ?line file:delete(AnalysisFile),
    ?line ?t:timetrap_cancel(Timetrap),
    ?line Acc1 = ts_sub(TS1, TS0),
    ?line Acc2 = ts_sub(TS2, TS1),
    ?line io:format("ts:~w, fprof:~w, bare:~w.~n", [Acc, Acc2, Acc1]),
    {comment, io_lib:format("~p times slower", [Acc2/Acc1])}.

%%%---------------------------------------------------------------------

create_file_slow(doc) ->
    ["Tests the create_file_slow benchmark"];
create_file_slow(suite) ->
    [];
create_file_slow(Config) when is_list(Config) ->
    ?line Timetrap = ?t:timetrap(?t:seconds(40)),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line TraceFile = 
	filename:join(PrivDir, ?MODULE_STRING"_create_file_slow.trace"),
    ?line AnalysisFile = 
	filename:join(PrivDir, ?MODULE_STRING"_create_file_slow.analysis"),
    ?line DataFile = 
	filename:join(PrivDir, ?MODULE_STRING"_create_file_slow.data"),
    ?line ok = fprof:stop(kill),
    %%
    ?line TS0 = erlang:now(),
    ?line ok = create_file_slow(DataFile, 1024),
    ?line TS1 = erlang:now(),
    %%
    ?line ok = file:delete(DataFile),
    ?line TS2 = erlang:now(),
    ?line ok = fprof:apply(?MODULE, create_file_slow, [DataFile, 1024], 
			   [{file, TraceFile}]),
    ?line TS3 = erlang:now(),
    ?line ok = fprof:profile(file, TraceFile),
    ?line ok = fprof:analyse(),
    ?line ok = fprof:analyse(dest, AnalysisFile),
    ?line ok = fprof:stop(),
    %%
    ?line {ok, [T, P]} = parse(AnalysisFile),
    ?line io:format("~p~n~n~p~n", [P, ets:tab2list(T)]),
    ?line ok = verify(T, P),
    ?line Proc = pid_to_list(self()),
    ?line case P of
	      [{analysis_options, _},
	       [{totals, _, Acc, _}],
	       [{Proc, _, undefined, _} | _]] ->
		  ok
	  end,
    %%
    ?line check_own_and_acc(TraceFile,AnalysisFile),
    %%
    ?line ets:delete(T),
    ?line file:delete(DataFile),
    ?line file:delete(TraceFile),
    ?line file:delete(AnalysisFile),
    ?line ?t:timetrap_cancel(Timetrap),
    ?line Acc1 = ts_sub(TS1, TS0),
    ?line Acc3 = ts_sub(TS3, TS2),
    ?line io:format("ts:~w, fprof:~w, bare:~w.~n", [Acc, Acc3, Acc1]),
    {comment, io_lib:format("~p times slower", [Acc3/Acc1])}.



%%%---------------------------------------------------------------------

spawn_simple(doc) ->
    ["Tests process spawn"];
spawn_simple(suite) ->
    [];
spawn_simple(Config) when is_list(Config) ->
    ?line Timetrap = ?t:timetrap(?t:seconds(30)),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line TraceFile = 
	filename:join(PrivDir, ?MODULE_STRING"_spawn_simple.trace"),
    ?line AnalysisFile = 
	filename:join(PrivDir, ?MODULE_STRING"_spawn_simple.analysis"),
    ?line Start = 1,
    ?line Stop = 1000,
    ?line Succ = fun (X) -> X + 1 end,
    ?line ok = fprof:stop(kill),
    %%
    ?line TS0 = erlang:now(),
    ?line {{_, R1}, {_, R2}} = spawn_simple_test(Start, Stop, Succ),
    ?line TS1 = erlang:now(),
    %%
    ?line ok = fprof:trace(start, TraceFile),
    ?line {{P1, R3}, {P2, R4}} = spawn_simple_test(Start, Stop, Succ),
    ?line ok = fprof:trace(stop),
    ?line TS2 = erlang:now(),
    ?line ok = fprof:profile(file, TraceFile),
    ?line ok = fprof:analyse(),
    ?line ok = fprof:analyse(dest, AnalysisFile),
    ?line ok = fprof:stop(),
    ?line R1 = R3,
    ?line R2 = R4,
    %%
    ?line {ok, [T, P]} = parse(AnalysisFile),
    ?line io:format("~p~n~n~p~n", [P, ets:tab2list(T)]),
    ?line ok = verify(T, P),
    ?line Proc1 = pid_to_list(P1),
    ?line Proc2 = pid_to_list(P2),
    ?line Proc0 = pid_to_list(self()),
    ?line io:format("~p~n ~p ~p ~p~n", [P, Proc0, Proc1, Proc2]),
    ?line [{analysis_options, _}, [{totals, _, Acc, _}] | Procs] = P,
    ?line [[{Proc0, _, undefined, _} | _]] = 
	lists:filter(fun ([Pt | _]) when element(1, Pt) == Proc0 -> true;
			 (_) -> false
		     end, Procs),
    ?line [[{Proc1, _, undefined, _},
	    {spawned_by, Proc0},
	    {spawned_as, {erlang, apply, ["#Fun"++_, []]}},
	    {initial_calls, [{erlang, apply, 2}, 
			     {?MODULE, '-spawn_simple_test/3-fun-0-', 4}]} 
	    | _]] = 
	lists:filter(fun ([Pt | _]) when element(1, Pt) == Proc1 -> true;
			 (_) -> false
		     end, Procs),
    ?line [[{Proc2, _, undefined, _},
	    {spawned_by, Proc0},
	    {spawned_as, {erlang, apply, ["#Fun"++_, []]}},
	    {initial_calls, [{erlang, apply, 2}, 
			     {?MODULE, '-spawn_simple_test/3-fun-1-', 4}]} 
	    | _]] = 
	lists:filter(fun ([Pt | _]) when element(1, Pt) == Proc2 -> true;
			 (_) -> false
		     end, Procs),
    ?line 3 = length(Procs),
    ?line R1 = lists:reverse(R2),
    %%
    ?line check_own_and_acc(TraceFile,AnalysisFile),
    %%
    ?line ets:delete(T),
    ?line file:delete(TraceFile),
    ?line file:delete(AnalysisFile),
    ?line ?t:timetrap_cancel(Timetrap),
    ?line Acc1 = ts_sub(TS1, TS0),
    ?line Acc2 = ts_sub(TS2, TS1),
    ?line io:format("ts:~w, fprof:~w, bare:~w.~n", [Acc, Acc2, Acc1]),
    {comment, io_lib:format("~p times slower", [Acc2/Acc1])}.


spawn_simple_test(Start, Stop, Succ) ->
    Parent = self(),
    Seq = 
	spawn_link(
	  fun () ->
		  Parent ! {self(), seq(Start, Stop, Succ)}
	  end),
    SeqR = 
	spawn_link(
	  fun () ->
		  Parent ! {self(), seq_r(Start, Stop, Succ)}
	  end),
    receive {Seq, SeqResult} ->
	    receive {SeqR, SeqRResult} ->
		    {{Seq, SeqResult}, {SeqR, SeqRResult}}
	    end
    end.



%%%---------------------------------------------------------------------

imm_tail_seq(doc) ->
    ["Tests a tail recursive variant of lists:seq/3 ",
     "with immediate trace to profile"];
imm_tail_seq(suite) ->
    [];
imm_tail_seq(Config) when is_list(Config) ->
    ?line Timetrap = ?t:timetrap(?t:seconds(10)),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line AnalysisFile = 
	filename:join(PrivDir, ?MODULE_STRING"_imm_tail_seq.analysis"),
    ?line Start = 1,
    ?line Stop = 1000,
    ?line Succ = fun (X) -> X + 1 end,
    ?line ok = fprof:stop(kill),
    ?line catch eprof:stop(),
    %%
    ?line TS0 = erlang:now(),
    ?line R0 = seq_r(Start, Stop, Succ),
    ?line TS1 = erlang:now(),
    %%
    ?line profiling = eprof:start_profiling([self()]),
    ?line TS2 = erlang:now(),
    ?line R2 = seq_r(Start, Stop, Succ),
    ?line TS3 = erlang:now(),
    ?line profiling_stopped = eprof:stop_profiling(),
    ?line R2 = R0,
    %%
    ?line eprof:analyze(),
    ?line stopped = eprof:stop(),
    %%
    ?line {ok, Tracer} = fprof:profile(start),
    ?line ok = fprof:trace([start, {tracer, Tracer}]),
    ?line TS4 = erlang:now(),
    ?line R4 = seq_r(Start, Stop, Succ),
    ?line TS5 = erlang:now(),
    ?line ok = fprof:trace(stop),
    ?line ok = fprof:analyse(),
    ?line ok = fprof:analyse(dest, AnalysisFile),
    ?line ok = fprof:stop(),
    ?line R4 = R0,
    %%
    ?line {ok, [T, P]} = parse(AnalysisFile),
    ?line io:format("~p~n~n~p~n", [P, ets:tab2list(T)]),
    ?line ok = verify(T, P),
    ?line Proc = pid_to_list(self()),
    ?line case P of
	      [{analysis_options, _},
	       [{totals, _, Acc, _}],
	       [{Proc, _, undefined, _} | _]] ->
		  ok
	  end,
    %%
    ?line ets:delete(T),
    ?line file:delete(AnalysisFile),
    ?line ?t:timetrap_cancel(Timetrap),
    ?line Acc1 = ts_sub(TS1, TS0),
    ?line Acc3 = ts_sub(TS3, TS2),
    ?line Acc5 = ts_sub(TS5, TS4),
    ?line io:format("~p (plain), ~p (eprof), ~p (fprof), ~p (cpu)~n", 
		    [Acc1/1000, Acc3/1000, Acc5/1000, Acc/1000]),
    {comment, io_lib:format("~p/~p (fprof/eprof) times slower", 
			    [Acc5/Acc1, Acc3/Acc1])}.

%%%---------------------------------------------------------------------

imm_create_file_slow(doc) ->
    ["Tests a tail recursive variant of lists:seq/3 ",
     "with immediate trace to profile"];
imm_create_file_slow(suite) ->
    [];
imm_create_file_slow(Config) when is_list(Config) ->
    ?line Timetrap = ?t:timetrap(?t:seconds(60)),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line DataFile = 
	filename:join(PrivDir, ?MODULE_STRING"_imm_create_file_slow.data"),
    ?line AnalysisFile = 
	filename:join(PrivDir, ?MODULE_STRING"_imm_create_file_slow.analysis"),
    ?line ok = fprof:stop(kill),
    %%
    ?line TS0 = erlang:now(),
    ?line ok = create_file_slow(DataFile, 1024),
    ?line TS1 = erlang:now(),
    ?line ok = file:delete(DataFile),
    %%
    ?line {ok, Tracer} = fprof:profile(start),
    ?line TS2 = erlang:now(),
    ?line ok = fprof:apply(?MODULE, create_file_slow, [DataFile, 1024], 
			  [{tracer, Tracer}, continue]),
    ?line TS3 = erlang:now(),
    ?line ok = fprof:profile(stop),
    ?line ok = fprof:analyse(),
    ?line ok = fprof:analyse(dest, AnalysisFile),
    ?line ok = fprof:stop(),
    %%
    ?line {ok, [T, P]} = parse(AnalysisFile),
    ?line io:format("~p~n~n~p~n", [P, ets:tab2list(T)]),
    ?line ok = verify(T, P),
    ?line Proc = pid_to_list(self()),
    ?line case P of
	      [{analysis_options, _},
	       [{totals, _, Acc, _}],
	       [{Proc, _, undefined, _} | _]] ->
		  ok
	  end,
    %%
    ?line ets:delete(T),
    ?line file:delete(DataFile),
    ?line file:delete(AnalysisFile),
    ?line ?t:timetrap_cancel(Timetrap),
    ?line Acc1 = ts_sub(TS1, TS0),
    ?line Acc3 = ts_sub(TS3, TS2),
    ?line io:format("ts:~w, fprof:~w, bare:~w.~n", [Acc, Acc3, Acc1]),
    {comment, io_lib:format("~p times slower", [Acc3/Acc1])}.

%%%---------------------------------------------------------------------

imm_compile(doc) ->
    ["Tests to compile a small source file ",
     "with immediate trace to profile"];
imm_compile(suite) ->
    [];
imm_compile(Config) when is_list(Config) ->
    ?line Timetrap = ?t:timetrap(?t:minutes(20)),
    ?line DataDir = ?config(data_dir, Config),
    ?line SourceFile = filename:join(DataDir, "foo.erl"),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line AnalysisFile = 
	filename:join(PrivDir, ?MODULE_STRING"_imm_compile.analysis"),
    ?line ok = fprof:stop(kill),
    ?line catch eprof:stop(),
    %%
    ?line {ok, foo, _} = compile:file(SourceFile, [binary]),
    ?line TS0 = erlang:now(),
    ?line {ok, foo, _} = compile:file(SourceFile, [binary]),
    ?line TS1 = erlang:now(),
    %%
    ?line profiling = eprof:start_profiling([self()]),
    ?line TS2 = erlang:now(),
    ?line {ok, foo, _} = compile:file(SourceFile, [binary]),
    ?line TS3 = erlang:now(),
    ?line profiling_stopped = eprof:stop_profiling(),
    %%
    ?line eprof:analyze(),
    ?line stopped = eprof:stop(),
    %%
    ?line {ok, Tracer} = fprof:profile(start),
    ?line ok = fprof:trace([start, {tracer, Tracer}]),
    ?line TS4 = erlang:now(),
    ?line {ok, foo, _} = compile:file(SourceFile, [binary]),
    ?line TS5 = erlang:now(),
    ?line ok = fprof:trace(stop),
    %%
    ?line io:format("Analysing...~n"),
    ?line ok = fprof:analyse(dest, AnalysisFile),
    ?line ok = fprof:stop(),
    %%
    ?line {ok, [T, P]} = parse(AnalysisFile),
    ?line io:format("~p~n", [P]),
    ?line Acc1 = ts_sub(TS1, TS0),
    ?line Acc3 = ts_sub(TS3, TS2),
    ?line Acc5 = ts_sub(TS5, TS4),
    ?line io:format("Verifying...~n"),
    ?line ok = verify(T, P),
    ?line case P of
	      [{analysis_options, _},
	       [{totals, _, Acc, _}] | _] ->
		  ok
	  end,
    %%
    ?line ets:delete(T),
    ?line file:delete(AnalysisFile),
    ?line ?t:timetrap_cancel(Timetrap),
    ?line io:format("~p (plain), ~p (eprof), ~p (fprof), ~p(cpu)~n", 
		    [Acc1/1000, Acc3/1000, Acc5/1000, Acc/1000]),
    {comment, io_lib:format("~p/~p (fprof/eprof) times slower", 
			    [Acc5/Acc1, Acc3/Acc1])}.

%%%---------------------------------------------------------------------

cpu_create_file_slow(doc) ->
    ["Tests the create_file_slow benchmark using cpu_time"];
cpu_create_file_slow(suite) ->
    [];
cpu_create_file_slow(Config) when is_list(Config) ->
    ?line Timetrap = ?t:timetrap(?t:seconds(40)),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line TraceFile = 
	filename:join(PrivDir, ?MODULE_STRING"_cpu_create_file_slow.trace"),
    ?line AnalysisFile = 
	filename:join(PrivDir, ?MODULE_STRING"_cpu_create_file_slow.analysis"),
    ?line DataFile = 
	filename:join(PrivDir, ?MODULE_STRING"_cpu_create_file_slow.data"),
    ?line ok = fprof:stop(kill),
    %%
    ?line TS0 = erlang:now(),
    ?line Result = (catch fprof:apply(?MODULE, create_file_slow, 
				      [DataFile, 1024], 
				      [{file, TraceFile}, cpu_time])),
    ?line TS1 = erlang:now(),
    ?line TestResult = 
	case Result of 
	    ok ->
		?line ok = fprof:profile(file, TraceFile),
		?line ok = fprof:analyse(),
		?line ok = fprof:analyse(dest, AnalysisFile),
		?line ok = fprof:stop(),
		%%
		?line {ok, [T, P]} = parse(AnalysisFile),
		?line io:format("~p~n~n~p~n", [P, ets:tab2list(T)]),
		?line ok = verify(T, P),
		?line Proc = pid_to_list(self()),
		?line case P of
			  [{analysis_options, _},
			   [{totals, _, Acc, _}],
			   [{Proc, _, undefined, _} | _]] ->
			      ok
		      end,
		%%
		?line check_own_and_acc(TraceFile,AnalysisFile),
		%%
		?line ets:delete(T),
		?line file:delete(DataFile),
		?line file:delete(TraceFile),
		?line file:delete(AnalysisFile),
		?line Acc1 = ts_sub(TS1, TS0),
		?line io:format("cpu_ts:~w, fprof:~w~n", [Acc, Acc1]),
		{comment, io_lib:format("~p% cpu utilization", 
					[100*Acc/Acc1])};
	    {'EXIT', not_supported} -> 
		case {os:type(), os:version()} of
		    {{unix, sunos}, {Major, Minor, _}}
		    when Major >= 5, Minor >= 7 ->
			test_server:fail(Result);
		    _ ->
			{skipped, "not_supported"}
		end;
	    _ ->
		test_server:fail(Result)
	end,
    ?line ?t:timetrap_cancel(Timetrap),
    TestResult.



%%%---------------------------------------------------------------------
%%% Functions to test
%%%---------------------------------------------------------------------



%% Stack recursive seq
seq(Stop, Stop, Succ) when is_function(Succ) ->
    [Stop];
seq(Start, Stop, Succ) when is_function(Succ) ->
    [Start | seq(Succ(Start), Stop, Succ)].



%% Tail recursive seq, result list is reversed
seq_r(Start, Stop, Succ) when is_function(Succ) ->
    seq_r(Start, Stop, Succ, []).

seq_r(Stop, Stop, _, R) ->
    [Stop | R];
seq_r(Start, Stop, Succ, R) ->
    seq_r(Succ(Start), Stop, Succ, [Start | R]).



create_file_slow(Name, N) when is_integer(N), N >= 0 ->
    {ok, FD} = 
        file:open(Name, [raw, write, delayed_write, binary]),
    if N > 256 ->
            ok = file:write(FD, 
                            lists:map(fun (X) -> <<X:32/unsigned>> end,
                            lists:seq(0, 255))),
            ok = create_file_slow(FD, 256, N);
       true ->
            ok = create_file_slow(FD, 0, N)
    end,
    ok = file:close(FD).

create_file_slow(_FD, M, M) ->
    ok;
create_file_slow(FD, M, N) ->
    ok = file:write(FD, <<M:32/unsigned>>),
    create_file_slow(FD, M+1, N).



%%%---------------------------------------------------------------------
%%% Profile verification functions
%%%---------------------------------------------------------------------



verify(Tab, [{analysis_options, _},
	     [{totals, Cnt, Acc, Own} | _] | Processes]) ->
    Processes_1 = 
	lists:map(
	  fun ([{Proc, Cnt_P, undefined, Own_P} | _]) ->
		  case sum_process(Tab, Proc) of
		      {Proc, Cnt_P, Acc_P, Own_P} = Clocks 
		      when Acc_P >= Own_P ->
			  Clocks;
		      Weird ->
			  throw({error, [?MODULE, ?LINE, Weird]})
		  end
	  end,
	  Processes),
    case lists:foldl(
	   fun ({_, Cnt_P2, Acc_P2, Own_P2}, 
		{totals, Cnt_T, Acc_T, Own_T}) ->
		   {totals, Cnt_P2+Cnt_T, Acc_P2+Acc_T, Own_P2+Own_T}
	   end,
	   {totals, 0, 0, 0},
	   Processes_1) of
	{totals, Cnt, Acc_T, Own} when Acc_T >= Acc ->
	    ok;
	Weird ->
	    throw({error, [?MODULE, ?LINE, Weird]})
    end.
	  


sum_process(Tab, Proc) ->
    ets_select_fold(
      Tab, [{{{Proc, '_'}, '_'}, [], ['$_']}], 100,
      fun ({{P, MFA}, {Callers, {MFA, Cnt, Acc, Own}, Called}},
	   {P, Cnt_P, Acc_P, Own_P}) when P == Proc ->
	      ok = verify_callers(Tab, Proc, MFA, Callers),
	      ok = verify_called(Tab, Proc, MFA, Called),
	      {P, Cnt+Cnt_P, Acc+Acc_P, Own+Own_P};
	  (Weird, Clocks) ->
	      throw({error, [?MODULE, ?LINE, Weird, Clocks]})
      end,
      {Proc, 0, 0, 0}).

verify_callers(_, _, _, []) ->
    ok;
verify_callers(Tab, Proc, MFA, [{Caller, Cnt, Acc, Own} | Tail]) ->
    Id = {Proc, Caller},
    case ets:lookup(Tab, Id) of
	[{Id, {_, {Caller, _, _, _}, Called}}] ->
	    case lists:keysearch(MFA, 1, Called) of
		{value, {MFA, Cnt, Acc, Own}} ->
		    verify_callers(Tab, Proc, MFA, Tail);
		false ->
		    throw({error, [?MODULE, ?LINE, MFA, Id]})
	    end;
	Weird ->
	    throw({error, [?MODULE, ?LINE, Weird]})
    end.
	      
verify_called(_, _, _, []) ->
    ok;
verify_called(Tab, Proc, MFA, [{Called, Cnt, Acc, Own} | Tail]) ->
    Id = {Proc, Called},
    case ets:lookup(Tab, Id) of
	[{Id, {Callers, {Called, _, _, _}, _}}] ->
	    case lists:keysearch(MFA, 1, Callers) of
		{value, {MFA, Cnt, Acc, Own}} ->
		    verify_called(Tab, Proc, MFA, Tail);
		false ->
		    throw({error, [?MODULE, ?LINE, MFA, Id]})
	    end;
	Weird ->
	    throw({error, [?MODULE, ?LINE, Weird]})
    end.



%% Parse a analysis file and return an Ets table with all function entries, 
%% and a list of process entries. Checks the concistency of the function
%% entries when they are read.
parse(Filename) ->
    case file:open(Filename, [read]) of
	{ok, FD} ->
	    Result = parse_stream(FD),
	    file:close(FD),
	    Result;
	Error ->
	    Error
    end.

parse_stream(FD) ->
    Tab = ets:new(fprof_SUITE, []),
    parse_stream(FD, Tab, [], void).

parse_stream(FD, Tab, R, Proc) ->
    case catch io:read(FD, '') of
	{'EXIT', _} ->
	    {error, [?MODULE, ?LINE]};
	{ok, Term} ->
	    case parse_term(Term) of
		{ok, {analysis_options, _} = Term_1}
		when Proc == void ->
		    parse_stream(FD, Tab, [Term_1 | R], analysis_options);
		{ok, [{totals, _, _, _} | _] = Term_1}
		when Proc == analysis_options ->
		    parse_stream(FD, Tab, [Term_1 | R], totals);
		{ok, [{P, _, _, _} | _] = Term_1} ->
		    parse_stream(FD, Tab, [Term_1 | R], P);
		{ok, {_Callers, {MFA, _, _, _}, _Called} = Term_1} 
		when Proc == totals; is_list(Proc) ->
		    ets:insert(Tab, {{Proc, MFA}, Term_1}),
		    parse_stream(FD, Tab, R, Proc);
		{ok, Term_1} ->
		    {error, [?MODULE, ?LINE, Term_1]};
		E ->
		    E
	    end;
	eof ->
	    {ok, [Tab, lists:reverse(R)]};
	Error ->
	    Error
    end.

parse_term({Callers, Func, Called})
  when is_list(Callers), is_list(Called) ->
    Callers_1 = lists:map(fun parse_clocks/1, Callers),
    Func_1 = parse_clocks(Func),
    Called_1 = lists:map(fun parse_clocks/1, Called),
    Result = {Callers_1, Func_1, Called_1},
    case chk_invariant(Result) of
	ok ->
	    {ok, Result};
	Error ->
	    Error
    end;
parse_term([{_, _, _, _} = Clocks | Tail]) ->
    {ok, [parse_clocks(Clocks) | Tail]};
parse_term(Term) ->
    {ok, Term}.

parse_clocks({MFA, Cnt, undefined, Own}) ->
    {MFA, Cnt, undefined, round(Own*1000)};
parse_clocks({MFA, Cnt, Acc, Own}) ->
    {MFA, Cnt, round(Acc*1000), round(Own*1000)};
parse_clocks(Clocks) ->
    Clocks.



chk_invariant({Callers, {MFA, Cnt, Acc, Own}, Called} = Term) ->
    {_, Callers_Cnt, Callers_Acc, Callers_Own} = Callers_Sum = sum(Callers),
%    {_, Called_Cnt, Called_Acc, Called_Own} = Called_Sum = sum(Called),
    case {MFA, 
	  lists:keymember(suspend, 1, Callers),
	  lists:keymember(garbage_collect, 1, Callers),
	  Called} of
	{suspend, false, _, []} ->
	    ok;
	{suspend, _, _, _} = Weird ->
	    {error, [?MODULE, ?LINE, Weird, Term]};
	{garbage_collect, false, false, []} ->
	    ok;
	{garbage_collect, false, false, [{suspend, _, _, _}]} ->
	    ok;
	{garbage_collect, _, _, _} = Weird ->
	    {error, [?MODULE, ?LINE, Weird, Term]};
	{undefined, false, false, _} 
	when Callers == [], Cnt == 0, Acc == 0, Own == 0 ->
	    ok;
	{undefined, _, _, _} = Weird ->
	    {error, [?MODULE, ?LINE, Weird, Term]};
	{_, _, _, _} ->
	    case chk_self_call(Term) of
		true when Callers_Cnt /= Cnt; Callers_Acc /= Acc; 
			  Callers_Own /= Own ->
		    {error, [?MODULE, ?LINE, Callers_Sum, Term]};
% 		true when Called_Acc + Own /= Acc ->
% 		    io:format("WARNING: ~p:~p, ~p, ~p.~n",
% 			      [?MODULE, ?LINE, Term, Called_Sum]),
% 		    {error, [?MODULE, ?LINE, Term, Called_Sum]};
% 		    ok;
		true ->
		    ok;
		false ->
		    {error, [?MODULE, ?LINE, Term]}
	    end
    end.

ts_sub({A, B, C}, {A0, B0, C0}) ->
    ((A - A0)*1000000000000 + (B - B0))*1000000 + C - C0.

sum(Funcs) ->
    {sum, _Cnt, _Acc, _Own} = 
	lists:foldl(
	  fun ({_, C1, A1, O1}, {sum, C2, A2, O2}) ->
		  {sum, C1+C2, A1+A2, O1+O2}
	  end,
	  {sum, 0, 0, 0},
	  Funcs).

chk_self_call({Callers, {MFA, _Cnt, _Acc, _Own}, Called}) ->
    case lists:keysearch(MFA, 1, Callers) of
	false ->
	    true;
	{value, {MFA, C, 0, O}} ->
	    case lists:keysearch(MFA, 1, Called) of
		false ->
		    false;
		{value, {MFA, C, 0, O}} ->
		    true;
		{value, _} ->
		    false
	    end;
	{value, _} ->
	    false
    end.



%%%---------------------------------------------------------------------
%%% Fairly generic support functions
%%%---------------------------------------------------------------------


ets_select_fold(Table, MatchSpec, Limit, Fun, Acc) ->
    ets:safe_fixtable(Table, true),
    ets_select_fold_1(ets:select(Table, MatchSpec, Limit), Fun, Acc).

ets_select_fold_1('$end_of_table', _, Acc) ->
    Acc;
ets_select_fold_1({Matches, Continuation}, Fun, Acc) ->
    ets_select_fold_1(ets:select(Continuation), 
		      Fun, 
		      lists:foldl(Fun, Acc, Matches)).



% ets_select_foreach(Table, MatchSpec, Limit, Fun) ->
%     ets:safe_fixtable(Table, true),
%     ets_select_foreach_1(ets:select(Table, MatchSpec, Limit), Fun).

% ets_select_foreach_1('$end_of_table', _) ->
%     ok;
% ets_select_foreach_1({Matches, Continuation}, Fun) ->
%     lists:foreach(Fun, Matches),
%     ets_select_foreach_1(ets:select(Continuation), Fun).


%%%---------------------------------------------------------------------
%%% Simple smulation of fprof used for checking own and acc times for
%%% each function.
%%% The function 'undefined' is ignored
%%%---------------------------------------------------------------------

%% check_own_and_acc_traced(TraceFile, AnalysisFile) ->
%%     check_own_and_acc(TraceFile, AnalysisFile, fun handle_trace_traced/2).

check_own_and_acc(TraceFile, AnalysisFile) ->
    check_own_and_acc(TraceFile, AnalysisFile, fun handle_trace/2).

check_own_and_acc(TraceFile, AnalysisFile, HandlerFun) ->
    dbg:trace_client(file,TraceFile,{HandlerFun,{init,self()}}),
    receive {result,Result} -> 
	    compare(Result,get_own_and_acc_from_analysis(AnalysisFile))
    end.

%% handle_trace_traced(Trace, Msg) ->
%%     io:format("handle_trace_traced(~p, ~p).", [Trace, Msg]),
%%     handle_trace(Trace, Msg).

handle_trace(Trace,{init,Parent}) ->
    ?dbg("~p",[start]),
    ets:new(fprof_verify_tab,[named_table]),
    handle_trace(Trace,Parent);
handle_trace({trace_ts,Pid,in,MFA,TS},P) ->
    ?dbg("~p",[{{in,Pid,MFA},get(Pid)}]),
    case get(Pid) of
	[suspend|[suspend|_]=NewStack] ->
	    T = ts_sub(TS,get({Pid,last_ts})),
	    update_acc(Pid,NewStack,T),
	    put(Pid,NewStack);
	[suspend|NewStack] = Stack ->
	    T = ts_sub(TS,get({Pid,last_ts})),
	    update_acc(Pid,Stack,T),
	    put(Pid,NewStack);
	[] ->
	    put(Pid,[MFA]),
	    insert(Pid,MFA);
	undefined ->
	    put(first_ts,TS),
	    put(Pid,[MFA]),
	    insert(Pid,MFA)
    end,
    put({Pid,last_ts},TS),
    P;
handle_trace({trace_ts,Pid,out,_MfaOrZero,TS},P) ->
    ?dbg("~p",[{{out,Pid,_MfaOrZero},get(Pid)}]),
    T = ts_sub(TS,get({Pid,last_ts})),
    case get(Pid) of
	[suspend|S] = Stack ->
	    update_acc(Pid,S,T),
	    put(Pid,[suspend|Stack]);
	[MFA|_] = Stack ->
	    insert(Pid,suspend),
	    update_own(Pid,MFA,T),
	    update_acc(Pid,Stack,T),
	    put(Pid,[suspend|Stack]);
	[] ->
	    insert(Pid,suspend),
	    put(Pid,[suspend])
    end,
    put({Pid,last_ts},TS),
    P;
handle_trace({trace_ts,Pid,call,MFA,{cp,Caller},TS},P) ->
    ?dbg("~p",[{{call,Pid,MFA},get(Pid)}]),
    T = ts_sub(TS,get({Pid,last_ts})),
    case get(Pid) of
	[MFA|_] = Stack ->
	    %% recursive
	    update_own(Pid,MFA,T),
	    update_acc(Pid,Stack,T);
	[CallingMFA|_] = Stack when Caller==undefined ->
	    insert(Pid,MFA),
	    update_own(Pid,CallingMFA,T),
	    update_acc(Pid,Stack,T),
	    put(Pid,[MFA|Stack]);
	[] when Caller==undefined ->
	    insert(Pid,MFA),
	    insert(Pid,MFA),
	    put(Pid,[MFA]);
	 Stack0 ->
	    Stack = [CallingMFA|_] = insert_caller(Caller,Stack0,[]),
	    insert(Pid,MFA),
	    insert(Pid,Caller),
	    update_own(Pid,CallingMFA,T),
	    update_acc(Pid,Stack,T),
	    put(Pid,[MFA|Stack])
    end,
    put({Pid,last_ts},TS),
    P;
handle_trace({trace_ts,Pid,return_to,MFA,TS},P) ->
    ?dbg("~p",[{{return_to,Pid,MFA},get(Pid)}]),
    T = ts_sub(TS,get({Pid,last_ts})),
    case get(Pid) of
	[MFA|_] = Stack ->
	    %% recursive
	    update_own(Pid,MFA,T),
	    update_acc(Pid,Stack,T),
	    put(Pid,Stack);
	[ReturnFromMFA,MFA|RestOfStack] = Stack ->
	    update_own(Pid,ReturnFromMFA,T),
	    update_acc(Pid,Stack,T),
	    put(Pid,[MFA|RestOfStack]);
	[ReturnFromMFA|RestOfStack] = Stack ->
	    update_own(Pid,ReturnFromMFA,T),
	    update_acc(Pid,Stack,T),
	    case find_return_to(MFA,RestOfStack) of
		[] when MFA==undefined -> 
		    put(Pid,[]);
		[] -> 
		    insert(Pid,MFA),
		    put(Pid,[MFA]);
		NewStack ->
		    put(Pid,NewStack)
	    end
    end,
    put({Pid,last_ts},TS),
    P;
handle_trace({trace_ts,Pid,gc_start,_,TS},P) ->
    ?dbg("~p",[{{gc_start,Pid},get(Pid)}]),
    case get(Pid) of
	[suspend|_] = Stack ->
	    T = ts_sub(TS,get({Pid,last_ts})),
	    insert(Pid,garbage_collect),
	    update_acc(Pid,Stack,T),
	    put(Pid,[garbage_collect|Stack]);
	[CallingMFA|_] = Stack ->
	    T = ts_sub(TS,get({Pid,last_ts})),
	    insert(Pid,garbage_collect),
	    update_own(Pid,CallingMFA,T),
	    update_acc(Pid,Stack,T),
	    put(Pid,[garbage_collect|Stack]);
	undefined ->
	    put(first_ts,TS),
	    put(Pid,[garbage_collect]),
	    insert(Pid,garbage_collect)
    end,
    put({Pid,last_ts},TS),
    P;
handle_trace({trace_ts,Pid,gc_end,_,TS},P) ->
    ?dbg("~p",[{{gc_end,Pid},get(Pid)}]),
    T = ts_sub(TS,get({Pid,last_ts})),
    case get(Pid) of
	[garbage_collect|RestOfStack] = Stack ->
	    update_own(Pid,garbage_collect,T),
	    update_acc(Pid,Stack,T),
	    put(Pid,RestOfStack)
    end,
    put({Pid,last_ts},TS),
    P;
handle_trace({trace_ts,Pid,spawn,NewPid,{M,F,Args},TS},P) ->
    MFA = {M,F,length(Args)},
    ?dbg("~p",[{{spawn,Pid,NewPid,MFA},get(Pid)}]),
    T = ts_sub(TS,get({Pid,last_ts})),
    put({NewPid,last_ts},TS),
    put(NewPid,[suspend,MFA]),
    insert(NewPid,suspend),
    insert(NewPid,MFA),
    case get(Pid) of
	[SpawningMFA|_] = Stack ->
	    update_own(Pid,SpawningMFA,T),
	    update_acc(Pid,Stack,T)
    end,
    put({Pid,last_ts},TS),
    P;
handle_trace({trace_ts,Pid,exit,_Reason,TS},P) ->
    ?dbg("~p",[{{exit,Pid,_Reason},get(Pid)}]),
    T = ts_sub(TS,get({Pid,last_ts})),
    case get(Pid) of
	[DyingMFA|_] = Stack ->
	    update_own(Pid,DyingMFA,T),
	    update_acc(Pid,Stack,T),
	    put(Pid,[]);
	[] ->
	    ok
    end,
    put({Pid,last_ts},TS),
    P;
handle_trace({trace_ts,_,Link,_,_},P) 
  when Link==link;
       Link==unlink;
       Link==getting_linked;
       Link==getting_unlinked ->
    P;
handle_trace(end_of_trace,P) ->
    ?dbg("~p",['end']),
    Result = ets:tab2list(fprof_verify_tab),
    {TotOwn,ProcOwns} = get_proc_owns(Result,[],0),
    TotAcc = ts_sub(get_last_ts(),get(first_ts)),
    P ! {result,[{totals,TotAcc,TotOwn}|ProcOwns]++Result},
    P;
handle_trace(Other,_P) ->
    exit({unexpected,Other}).

find_return_to(MFA,[MFA|_]=Stack) ->
    Stack;
find_return_to(MFA,[_|Stack]) ->
    find_return_to(MFA,Stack);
find_return_to(_MFA,[]) ->
    [].

insert_caller(MFA,[MFA|Rest],Result) ->
    lists:reverse(Result)++[MFA|Rest];
insert_caller(MFA,[Other|Rest],Result) ->
    insert_caller(MFA,Rest,[Other|Result]);
insert_caller(MFA,[],Result) ->
    lists:reverse([MFA|Result]).

insert(Pid,MFA) ->
    case ets:member(fprof_verify_tab,{Pid,MFA}) of
	false ->
	    ets:insert(fprof_verify_tab,{{Pid,MFA},0,0});
	true ->
	    ok
    end.

update_own(Pid,MFA,T) ->
    ets:update_counter(fprof_verify_tab,{Pid,MFA},{3,T}).

update_acc(Pid,[MFA|Rest],T) ->
    case lists:member(MFA,Rest) of
	true -> 
	    %% Only charge one time for recursive functions
	    ok;
	false -> 
	    ets:update_counter(fprof_verify_tab,{Pid,MFA},{2,T})
    end,
    update_acc(Pid,Rest,T);
update_acc(_Pid,[],_T) ->
    ok.


get_last_ts() ->
    get_last_ts(get(),{0,0,0}).
get_last_ts([{{_,last_ts},TS}|Rest],Last) when TS>Last ->
    get_last_ts(Rest,TS);
get_last_ts([_|Rest],Last) ->
    get_last_ts(Rest,Last);
get_last_ts([],Last) ->
    Last.

get_proc_owns([{{Pid,_MFA},_Acc,Own}|Rest],Result,Sum) ->
    NewResult = 
	case lists:keysearch(Pid,1,Result) of
	    {value,{Pid,undefined,PidOwn}} ->
		lists:keyreplace(Pid,1,Result,{Pid,undefined,PidOwn+Own});
	    false ->
		[{Pid,undefined,Own}|Result]
    end,
    get_proc_owns(Rest,NewResult,Sum+Own);
get_proc_owns([],Result,Sum) ->
    {Sum,Result}.
    

compare([X|Rest],FprofResult) ->
    FprofResult1 = 
	case lists:member(X,FprofResult) of
	    true ->
		?dbg("~p",[X]),
		lists:delete(X,FprofResult);
	    false -> 
		case lists:keysearch(element(1,X),1,FprofResult) of
		    {value,Fprof} ->
			put(compare_error,true),
			io:format("Error: Different values\n"
				  "Fprof:     ~p\n"
				  "Simulator: ~p",[Fprof,X]),
			lists:delete(Fprof,FprofResult);
		    false ->
			put(compare_error,true),
			io:format("Error: Missing in fprof: ~p",[X]),
			FprofResult
		end
	end,
    compare(Rest,FprofResult1);
compare([],Rest) ->
    case {remove_undefined(Rest,[]),get(compare_error)} of
	{[],undefined} -> ok;
	{Error,_} ->
	    case Error of
		[] -> ok;
		_ -> io:format("\nMissing in simulator results:\n~p\n",[Error])
	    end,
	    ?t:fail({error,mismatch_between_simulator_and_fprof})
    end.
    
remove_undefined([{{_Pid,undefined},_,_}|Rest],Result) ->
    remove_undefined(Rest,Result);
remove_undefined([X|Rest],Result) ->
    remove_undefined(Rest,[X|Result]);
remove_undefined([],Result) ->
    Result.
    
get_own_and_acc_from_analysis(Log) ->
    case file:consult(Log) of
	{ok,[_Options,[{totals,_,TotAcc,TotOwn}]|Rest]} ->
	    get_own_and_acc(undefined,Rest,
			    [{totals,m1000(TotAcc),m1000(TotOwn)}]);
	Error ->
	    exit({error,{cant_open,Log,Error}})
    end.

get_own_and_acc(_,[[{PidStr,_,Acc,Own}|_]|Rest],Result) ->
    Pid = list_to_pid(PidStr),
    get_own_and_acc(Pid,Rest,[{Pid,m1000(Acc),m1000(Own)}|Result]);
get_own_and_acc(Pid,[{_Callers,{MFA,_,Acc,Own},_Called}|Rest],Result) ->
    get_own_and_acc(Pid,Rest,[{{Pid,MFA},m1000(Acc),m1000(Own)}|Result]);
get_own_and_acc(_,[],Result) ->
    lists:reverse(Result).

m1000(undefined) ->
    undefined;
m1000(X) ->
    round(X*1000).

