%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 2004-2010 held by the authors. All Rights Reserved.
%% Copyright Ericsson AB 2012-2025. All Rights Reserved.
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
%% Original author: Stavros Aronis <aronisstav@gmail.com>
%%
%% Purpose: Spawn and coordinate parallel jobs.

-module(dialyzer_coordinator).
-moduledoc false.

%%% Export for dialyzer main process
-export([parallel_job/4]).

%%% Exports for all workers
-export([request_activation/1, job_done/3]).

%%% Exports for the typesig and dataflow analysis workers
-export([wait_for_success_typings/2]).

%% Exports to handle SCC labels
-export([get_job_label/2, get_job_input/2]).

%%% Exports for the compilation workers
-export([get_next_label/2]).

-export_type([coordinator/0, mode/0, init_data/0, result/0, job/0]).

%%--------------------------------------------------------------------

-type collector()  :: pid().
-type regulator()  :: pid().
-type job_labels_to_pid() :: ets:tid() | 'none'.

-opaque coordinator() :: {collector(), regulator(), job_labels_to_pid()}.
-type timing() :: dialyzer_timing:timing_server().

-type scc()     :: [mfa_or_funlbl()].
-type mode()    :: 'typesig' | 'dataflow' | 'compile' | 'warnings' |
                   'contract_remote_types' | 'record_remote_types'.

-type compile_job()  :: file:filename().
-type typesig_job()  :: {integer(),scc()}.
-type dataflow_job() :: module().
-type warnings_job() :: module().
-type contract_remote_types_job() :: module().
-type record_remote_types_job() :: module().

-type job() :: compile_job() | typesig_job() | dataflow_job() |
               warnings_job() | contract_remote_types_job() |
               record_remote_types_job().

-type compile_init_data()  :: dialyzer_analysis_callgraph:compile_init_data().
-type typesig_init_data()  :: dialyzer_succ_typings:typesig_init_data().
-type dataflow_init_data() :: dialyzer_succ_typings:dataflow_init_data().
-type warnings_init_data() :: dialyzer_succ_typings:warnings_init_data().
-type contract_remote_types_init_data() ::
                      dialyzer_contracts:contract_remote_types_init_data().
-type record_remote_types_init_data() ::
                      dialyzer_utils:record_remote_types_init_data().

-type compile_result()  :: dialyzer_analysis_callgraph:compile_result().
-type typesig_result()  :: [mfa_or_funlbl()].
-type dataflow_result() :: [mfa_or_funlbl()].
-type warnings_result() :: [dial_warning()].
-type contract_remote_types_result() ::
        dialyzer_contracts:contract_remote_types_result().
-type record_remote_types_result() ::
        dialyzer_utils:record_remote_types_result().

-type init_data() :: compile_init_data() | typesig_init_data() |
		     dataflow_init_data() | warnings_init_data() |
                     contract_remote_types_init_data() |
                     record_remote_types_init_data().

-type result() :: compile_result() | typesig_result() |
		  dataflow_result() | warnings_result() |
                  contract_remote_types_result() |
                  record_remote_types_result().

-type job_result() :: dialyzer_analysis_callgraph:one_file_mid_error() |
                      dialyzer_analysis_callgraph:one_file_result_ok() |
                      typesig_result() | dataflow_result() |
                      warnings_result() | contract_remote_types_result() |
                      record_remote_types_result().

-record(state, {mode           :: mode(),
		active     = 0 :: integer(),
		result         :: result(),
		next_label = 0 :: integer(),
                jobs           :: [job()],
                job_fun        :: fun(),
		init_data      :: init_data(),
		regulator      :: regulator(),
		job_labels_to_pid     :: job_labels_to_pid()
	       }).

-include("dialyzer.hrl").

%%--------------------------------------------------------------------
%% API functions for the main dialyzer process.

-spec parallel_job('compile', [compile_job()], compile_init_data(), timing()) ->
		      {compile_result(), integer()};
		  ('typesig', [typesig_job()], typesig_init_data(), timing()) ->
		      typesig_result();
		  ('dataflow', [dataflow_job()], dataflow_init_data(),
		   timing()) -> dataflow_result();
		  ('warnings', [warnings_job()], warnings_init_data(),
		   timing()) -> warnings_result();
                  ('contract_remote_types', [contract_remote_types_job()],
                   contract_remote_types_init_data(), timing()) ->
                      contract_remote_types_result();
                  ('record_remote_types', [record_remote_types_job()],
                   record_remote_types_init_data(), timing()) ->
                      record_remote_types_result().

parallel_job(Mode, Jobs, InitData, Timing) ->
  State = spawn_jobs(Mode, Jobs, InitData, Timing),
  collect_result(State).

%%--------------------------------------------------------------------
%% API functions for workers (dialyzer_worker).

-spec wait_for_success_typings([job_label()], coordinator()) ->
        'ok'.

%% Helper for 'sigtype' and 'dataflow' workers.
wait_for_success_typings(Labels, {_Collector, _Regulator, JobLabelsToPid}) ->
  F = fun(JobLabel) ->
          %% The jobs that job depends on have always been started.
          case ets:lookup_element(JobLabelsToPid, JobLabel, 2, ok) of
            Pid when is_pid(Pid) ->
              Ref = erlang:monitor(process, Pid),
              receive
                {'DOWN', Ref, process, Pid, _Info} ->
                  ok
              end;
            ok ->
              %% Already finished.
              ok
          end
      end,
  lists:foreach(F, Labels).


%%--------------------------------------------------------------------
%% Local functions.

-dialyzer({no_opaque_union, [spawn_jobs/4]}).
spawn_jobs(Mode, Jobs, InitData, Timing) ->
  Collector = self(),
  Regulator = spawn_regulator(),
  JobLabelsToPid =
    if
      Mode =:= 'typesig'; Mode =:= 'dataflow' ->
        ets:new(job_labels_to_pid, [{read_concurrency, true}]);
      true ->
        none
    end,
  Coordinator = {Collector, Regulator, JobLabelsToPid},

  JobFun = job_fun(JobLabelsToPid, Mode, InitData, Coordinator),

  %% Limit the number of processes we start in order to save memory.
  MaxNumberOfInitJobs = 20 * dialyzer_utils:parallelism(),
  RestJobs = launch_jobs(Jobs, JobFun, MaxNumberOfInitJobs),

  Unit =
    case Mode of
      'typesig'  -> "SCCs";
      _          -> "modules"
    end,
  JobCount = length(Jobs),
  dialyzer_timing:send_size_info(Timing, JobCount, Unit),

  InitResult =
    case Mode of
      'compile' -> dialyzer_analysis_callgraph:compile_init_result();
      _ -> []
    end,

  #state{mode = Mode, active = JobCount, result = InitResult,
         next_label = 0, job_fun = JobFun, jobs = RestJobs,
         init_data = InitData, regulator = Regulator,
         job_labels_to_pid = JobLabelsToPid}.

launch_jobs(Jobs, _JobFun, 0) ->
  Jobs;
launch_jobs([Job|Jobs], JobFun, N) ->
  JobFun(Job),
  launch_jobs(Jobs, JobFun, N - 1);
launch_jobs([], _JobFun, _) ->
  [].

job_fun(none, Mode, InitData, Coordinator) ->
  fun(Job) ->
      _ = dialyzer_worker:launch(Mode, Job, InitData, Coordinator),
      ok
  end;
job_fun(JobLabelsToPid, Mode, InitData, Coordinator) ->
  fun(Job) ->
      JobLabel = get_job_label(Mode, Job),
      Pid = dialyzer_worker:launch(Mode, Job, InitData, Coordinator),
      true = ets:insert(JobLabelsToPid, {JobLabel, Pid}),
      ok
  end.

-dialyzer({no_opaque_union, [collect_result/1]}).
collect_result(#state{mode = Mode, active = Active, result = Result,
		      next_label = NextLabel, init_data = InitData,
                      jobs = JobsLeft, job_fun = JobFun,
		      regulator = Regulator, job_labels_to_pid = JobLabelsToPID} = State) ->
  receive
    {next_label_request, Estimation, Pid} ->
      Pid ! {next_label_reply, NextLabel},
      collect_result(State#state{next_label = NextLabel + Estimation});
    {done, Job, Data} ->
      NewResult = update_result(Mode, InitData, Job, Data, Result),
      case Active of
	1 ->
          %% This was the last running job. Clean up and return the result.
	  kill_regulator(Regulator),
	  case Mode of
	    'compile' ->
	      {NewResult, NextLabel};
	    _ ->
              if
                JobLabelsToPID =:= none -> ok;
                true -> ets:delete(JobLabelsToPID)
              end,
	      NewResult
	  end;
	N ->
          if
            JobLabelsToPID =:= none -> ok;
            true -> true = ets:delete(JobLabelsToPID, get_job_label(Mode, Job))
          end,
          NewJobsLeft =
            case JobsLeft of
              [] -> [];
              [NewJob|JobsLeft1] ->
                JobFun(NewJob),
                JobsLeft1
            end,
          NewState = State#state{result = NewResult,
                                 jobs = NewJobsLeft,
                                 active = N - 1},
          collect_result(NewState)
      end
  end.

-dialyzer({no_opaque_union, [update_result/5]}).
update_result(Mode, InitData, Job, Data, Result) ->
  if
    Mode =:= 'compile' ->
      dialyzer_analysis_callgraph:add_to_result(Job, Data, Result,
						InitData);
    Mode =:= 'typesig'; Mode =:= 'dataflow' ->
      dialyzer_succ_typings:add_to_result(Data, Result, InitData);
    true ->
      Data ++ Result
  end.


-type job_label() :: integer() | module().

-type job_input() :: scc() | module().

-spec get_job_label(mode(), job()) -> job_label().

get_job_label(typesig, {Label, _Input}) -> Label;
get_job_label(dataflow, Job) -> Job;
get_job_label(contract_remote_types, Job) -> Job;
get_job_label(record_remote_types, Job) -> Job;
get_job_label(warnings, Job) -> Job;
get_job_label(compile, Job) -> Job.

-spec get_job_input(mode(), job()) -> job_input().

get_job_input(typesig, {_Label, Input}) -> Input;
get_job_input(dataflow, Job) -> Job;
get_job_input(contract_remote_types, Job) -> Job;
get_job_input(record_remote_types, Job) -> Job;
get_job_input(warnings, Job) -> Job;
get_job_input(compile, Job) -> Job.

-spec job_done(job(), job_result(), coordinator()) -> ok.

job_done(Job, Result, {Collector, Regulator, _JobLabelsToPID}) ->
  Regulator ! done,
  Collector ! {done, Job, Result},
  ok.

-spec get_next_label(integer(), coordinator()) -> integer().

get_next_label(EstimatedSize, {Collector, _Regulator, _JobLabelsToPID}) ->
  Collector ! {next_label_request, EstimatedSize, self()},
  receive
    {next_label_reply, NextLabel} -> NextLabel
  end.

%%--------------------------------------------------------------------
%% The regulator server
%%
%% The regulator limits the number of simultaneous running jobs to the
%% number of schedulers. Note that there are usually many more worker
%% processes started, but they are only allowed to do light work (such
%% as monitoring other processes) when they have not been activated.

-spec wait_activation() -> ok.

wait_activation() ->
  receive activate -> ok end.

activate_pid(Pid) ->
  Pid ! activate.

-spec request_activation(coordinator()) -> ok.

request_activation({_Collector, Regulator, _JobLabelsToPID}) ->
  Regulator ! {req, self()},
  wait_activation().

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
      case queue:out(Queue) of
        {empty, NewQueue} ->
          regulator_loop(Tickets + 1, NewQueue);
        {{value, Pid}, NewQueue} ->
          activate_pid(Pid),
          regulator_loop(Tickets, NewQueue)
      end;
    stop -> ok
  end.

kill_regulator(Regulator) ->
  Regulator ! stop.
