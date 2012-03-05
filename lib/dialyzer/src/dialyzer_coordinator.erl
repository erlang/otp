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
-export([parallel_job/3]).

%%% Exports for all possible workers
-export([wait_activation/0, job_done/3]).

%%% Exports for the typesig and dataflow analysis workers
-export([sccs_to_pids/1]).

%%% Exports for the compilation workers
-export([get_next_label/2]).

-export_type([coordinator/0, mode/0, init_data/0]).

%%--------------------------------------------------------------------

-define(MAP, dialyzer_coordinator_map).

-type coordinator() :: pid(). %%opaque

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

-record(state, {active     = 0           :: integer(),
		result                   :: result(),
		next_label = 0           :: integer(),
		tickets    = 0           :: integer(),
		queue      = queue:new() :: queue(),
		init_data                :: init_data()
	       }).

-include("dialyzer.hrl").

%%--------------------------------------------------------------------

-spec parallel_job('compile', compile_jobs(), compile_init_data()) ->
		      {compile_result(), integer()};
		  ('typesig', typesig_jobs(), typesig_init_data()) ->
		      typesig_result();
		  ('dataflow', dataflow_jobs(), dataflow_init_data()) ->
		      dataflow_result();
		  ('warnings', warnings_jobs(), warnings_init_data()) ->
		      warnings_result().

parallel_job(Mode, Jobs, InitData) ->
  State = spawn_jobs(Mode, Jobs, InitData),
  collect_result(Mode, State).

spawn_jobs(Mode, Jobs, InitData) when
    Mode =:= 'typesig'; Mode =:= 'dataflow' ->
  Coordinator = self(),
  ?MAP = ets:new(?MAP, [named_table, {read_concurrency, true}]),
  Fold =
    fun(Job, Count) ->
	Pid = dialyzer_worker:launch(Mode, Job, InitData, Coordinator),
	true = ets:insert(?MAP, {Job, Pid}),
	Count + 1
    end,
  JobCount = lists:foldl(Fold, 0, Jobs),
  Unit =
    case Mode of
      'typesig'  -> "SCCs";
      'dataflow' -> "modules"
    end,
  dialyzer_timing:send_size_info(JobCount, Unit),
  #state{active = JobCount, result = [], init_data = InitData};
spawn_jobs(Mode, Jobs, InitData) when
    Mode =:= 'compile'; Mode =:= 'warnings' ->
  Coordinator = self(),
  Fold =
    fun(Job, {InTickets, InQueue, Count}) ->
	Pid = dialyzer_worker:launch(Mode, Job, InitData, Coordinator),
	NewCount = Count + 1,
	case InTickets of
	  0 -> {InTickets, queue:in(Pid, InQueue), NewCount};
	  N -> activate_pid(Pid), {N-1, InQueue, NewCount}
	end
    end,
  CPUs = erlang:system_info(logical_processors_available),
  InitTickets = 4*CPUs,
  {Tickets, Queue, JobCount} =
    lists:foldl(Fold, {InitTickets, queue:new(), 0}, Jobs),
  dialyzer_timing:send_size_info(JobCount, "modules"),
  InitResult =
    case Mode of
      'warnings' -> [];
      'compile' -> dialyzer_analysis_callgraph:compile_init_result()
    end,
  #state{active = JobCount, result = InitResult, next_label = 0,
	 tickets = Tickets, queue = Queue, init_data = InitData}.

collect_result(Mode, State) ->
  case Mode of
    'compile' -> compile_loop(State);
    'typesig' -> not_fixpoint_loop(State);
    'dataflow' -> not_fixpoint_loop(State);
    'warnings' -> warnings_loop(State)
  end.

compile_loop(#state{active = Active, result = Result,
		    next_label = NextLabel, tickets = Tickets,
		    queue = Queue, init_data = InitData} = State) ->
  receive
    {next_label_request, Estimation, Pid} ->
      Pid ! {next_label_reply, NextLabel},
      compile_loop(State#state{next_label = NextLabel + Estimation});
    {done, Job, Data} ->
      NewResult =
	dialyzer_analysis_callgraph:add_to_result(Job, Data, Result, InitData),
      case Active of
	1 ->
	  {NewResult, NextLabel};
	_ ->
	  NewActive = Active - 1,
	  {NewQueue, NewTickets} = manage_waiting(Queue, Tickets),
	  NewState =
	    State#state{result = NewResult, active = NewActive,
			queue = NewQueue, tickets = NewTickets},
	  compile_loop(NewState)
      end
  end.

not_fixpoint_loop(#state{active = Active, result = Result,
			 init_data = InitData} = State) ->
  receive
    {done, _Job, Data} ->
      FinalData = dialyzer_succ_typings:lookup_names(Data, InitData),
      NewResult = FinalData ++ Result,
      case Active of
	1 ->
	  ets:delete(?MAP),
	  NewResult;
	_ ->
	  NewActive = Active - 1,
	  NewState = State#state{active = NewActive, result = NewResult},
	  not_fixpoint_loop(NewState)
      end
  end.

warnings_loop(#state{active = Active, result = Result, tickets = Tickets,
		     queue = Queue} = State) ->
  receive
    {done, _Job, Data} ->
      NewResult = Data ++ Result,
      case Active of
	1 -> NewResult;
	_ ->
	  NewActive = Active - 1,
	  {NewQueue, NewTickets} = manage_waiting(Queue, Tickets),
	  NewState =
	    State#state{result = NewResult, active = NewActive,
			queue = NewQueue, tickets = NewTickets},
	  warnings_loop(NewState)
      end
  end.

manage_waiting(Queue, Tickets) ->
  {Waiting, NewQueue} = queue:out(Queue),
  NewTickets =
    case Waiting of
      empty -> Tickets + 1;
      {value, Pid} ->
	activate_pid(Pid),
	Tickets
    end,
  {NewQueue, NewTickets}.

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

job_done(Job, Result, Coordinator) ->
  Coordinator ! {done, Job, Result},
  ok.

-spec get_next_label(integer(), coordinator()) -> integer().

get_next_label(EstimatedSize, Coordinator) ->
  Coordinator ! {next_label_request, EstimatedSize, self()},
  receive
    {next_label_reply, NextLabel} -> NextLabel
  end.

-spec wait_activation() -> ok.

wait_activation() ->
  receive activate -> ok end.

activate_pid(Pid) ->
  Pid ! activate.
