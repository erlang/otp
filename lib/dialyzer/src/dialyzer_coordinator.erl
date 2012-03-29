%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2011. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File        : dialyzer_coordinator.erl
%%% Authors     : Stavros Aronis <aronisstav@gmail.com>
%%%-------------------------------------------------------------------

-module(dialyzer_coordinator).

%%% Export for dialyzer main process
-export([parallel_job/4]).

%%% Exports for all possible workers
-export([wait_activation/0, job_done/3]).

%%% Exports for the typesig and dataflow analysis workers
-export([sccs_to_pids/1, request_activation/1]).

%%% Exports for the compilation workers
-export([get_next_label/2]).

-export_type([coordinator/0, mode/0, init_data/0, result/0]).

%%--------------------------------------------------------------------

-define(MAP, dialyzer_coordinator_map).

-type coordinator() :: {pid(), pid()}. %%opaque
-type timing() :: dialyzer_timing:timing_server().

-type scc()     :: [mfa_or_funlbl()].
-type mode()    :: 'typesig' | 'dataflow' | 'compile' | 'warnings'.

-type compile_jobs()  :: [file:filename()].
-type typesig_jobs()  :: [scc()].
-type dataflow_jobs() :: [module()].
-type warnings_jobs() :: [module()].

-type compile_init_data()  :: dialyzer_analysis_callgraph:compile_init_data().
-type typesig_init_data()  :: dialyzer_succ_typings:typesig_init_data().
-type dataflow_init_data() :: dialyzer_succ_typings:dataflow_init_data().
-type warnings_init_data() :: dialyzer_succ_typings:warnings_init_data().

-type compile_result()  :: dialyzer_analysis_callgraph:compile_result().
-type typesig_result()  :: [mfa_or_funlbl()].
-type dataflow_result() :: [mfa_or_funlbl()].
-type warnings_result() :: [dial_warning()].

-type init_data() :: compile_init_data() | typesig_init_data() |
		     dataflow_init_data() | warnings_init_data().

-type result() :: compile_result() | typesig_result() |
		  dataflow_result() | warnings_result().

-type job() :: scc() | module() | file:filename().
-type job_result() :: dialyzer_analysis_callgraph:one_file_result() |
		      typesig_result() | dataflow_result() | warnings_result().

-record(state, {mode           :: mode(),
		active     = 0 :: integer(),
		result         :: result(),
		next_label = 0 :: integer(),
		init_data      :: init_data(),
		regulator      :: pid()
	       }).

-include("dialyzer.hrl").

%%--------------------------------------------------------------------

-spec parallel_job('compile', compile_jobs(), compile_init_data(), timing()) ->
		      {compile_result(), integer()};
		  ('typesig', typesig_jobs(), typesig_init_data(), timing()) ->
		      typesig_result();
		  ('dataflow', dataflow_jobs(), dataflow_init_data(),
		   timing()) -> dataflow_result();
		  ('warnings', warnings_jobs(), warnings_init_data(),
		   timing()) -> warnings_result().

parallel_job(Mode, Jobs, InitData, Timing) ->
  State = spawn_jobs(Mode, Jobs, InitData, Timing),
  collect_result(State).

spawn_jobs(Mode, Jobs, InitData, Timing) ->
  Collector = self(),
  Regulator = spawn_regulator(),
  Coordinator = {Collector, Regulator},
  TypesigOrDataflow = (Mode =:= 'typesig') orelse (Mode =:= 'dataflow'),
  case TypesigOrDataflow of
    true ->
      ?MAP = ets:new(?MAP, [named_table, {read_concurrency, true}]);
    false -> ok
  end,
  Fold =
    fun(Job, Count) ->
	Pid = dialyzer_worker:launch(Mode, Job, InitData, Coordinator),
	case TypesigOrDataflow of
	  true  -> true = ets:insert(?MAP, {Job, Pid});
	  false -> request_activation(Regulator, Pid)
	end,
	Count + 1
    end,
  JobCount = lists:foldl(Fold, 0, Jobs),
  Unit =
    case Mode of
      'typesig'  -> "SCCs";
      _          -> "modules"
    end,
  dialyzer_timing:send_size_info(Timing, JobCount, Unit),
  InitResult =
    case Mode of
      'compile' -> dialyzer_analysis_callgraph:compile_init_result();
      _ -> []
    end,
  #state{mode = Mode, active = JobCount, result = InitResult, next_label = 0,
	 init_data = InitData, regulator = Regulator}.

collect_result(#state{mode = Mode, active = Active, result = Result,
		      next_label = NextLabel, init_data = InitData,
		      regulator = Regulator} = State) ->
  receive
    {next_label_request, Estimation, Pid} ->
      Pid ! {next_label_reply, NextLabel},
      collect_result(State#state{next_label = NextLabel + Estimation});
    {done, Job, Data} ->
      NewResult = update_result(Mode, InitData, Job, Data, Result),
      case Active of
	1 ->
	  kill_regulator(Regulator),
	  case Mode of
	    'compile' ->
	      {NewResult, NextLabel};
	    X when X =:= 'typesig'; X =:= 'dataflow' ->
	      ets:delete(?MAP),
	      NewResult;
	    'warnings' ->
	      NewResult
	  end;
	N ->
	  collect_result(State#state{result = NewResult, active = N - 1})
      end
  end.

update_result(Mode, InitData, Job, Data, Result) ->
  case Mode of
    'compile' ->
      dialyzer_analysis_callgraph:add_to_result(Job, Data, Result,
						InitData);
    X when X =:= 'typesig'; X =:= 'dataflow' ->
      dialyzer_succ_typings:lookup_names(Data, InitData) ++ Result;
    'warnings' ->
      Data ++ Result
  end.

-spec sccs_to_pids([scc() | module()]) ->
        {[dialyzer_worker:worker()], [scc() | module()]}.

sccs_to_pids(SCCs) ->
  lists:foldl(fun pid_partition/2, {[], []}, SCCs).

pid_partition(SCC, {Pids, Unknown}) ->
  try ets:lookup_element(?MAP, SCC, 2) of
      Result -> {[Result|Pids], Unknown}
  catch
    _:_ -> {Pids, [SCC|Unknown]}
  end.

-spec job_done(job(), job_result(), coordinator()) -> ok.

job_done(Job, Result, {Collector, Regulator}) ->
  Regulator ! done,
  Collector ! {done, Job, Result},
  ok.

-spec get_next_label(integer(), coordinator()) -> integer().

get_next_label(EstimatedSize, {Collector, _Regulator}) ->
  Collector ! {next_label_request, EstimatedSize, self()},
  receive
    {next_label_reply, NextLabel} -> NextLabel
  end.

-spec wait_activation() -> ok.

wait_activation() ->
  receive activate -> ok end.

activate_pid(Pid) ->
  Pid ! activate.

-spec request_activation(coordinator()) -> ok.

request_activation({_Collector, Regulator}) ->
  Regulator ! {req, self()},
  wait_activation().

request_activation(Regulator, Pid) ->
  Regulator ! {req, Pid}.

spawn_regulator() ->
  InitTickets = dialyzer_utils:parallelism(),
  spawn_link(fun() -> regulator_loop(InitTickets, queue:new()) end).

regulator_loop(Tickets, Queue) ->
  receive
    {req, Pid} ->
      case Tickets of
	0 ->
	  regulator_loop(0, queue:in(Pid, Queue));
	N ->
	  activate_pid(Pid),
	  regulator_loop(N-1, Queue)
      end;
    done ->
      {Waiting, NewQueue} = queue:out(Queue),
      NewTickets =
	case Waiting of
	  empty -> Tickets + 1;
	  {value, Pid} ->
	    activate_pid(Pid),
	    Tickets
	end,
      regulator_loop(NewTickets, NewQueue);
    stop -> ok
  end.

kill_regulator(Regulator) ->
  Regulator ! stop.
