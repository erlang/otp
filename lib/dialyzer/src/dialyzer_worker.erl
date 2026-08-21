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

%% Purpose: Run individual jobs in separate processes.

-module(dialyzer_worker).
-moduledoc false.

-export([launch/4]).

-export_type([worker/0]).

-opaque worker() :: pid().

-type mode()        :: dialyzer_coordinator:mode().
-type coordinator() :: dialyzer_coordinator:coordinator().
-type init_data()   :: dialyzer_coordinator:init_data().
-type job()         :: dialyzer_coordinator:job().

-record(state, {
	  mode             :: mode(),
	  job              :: job(),
	  coordinator      :: coordinator(),
	  init_data        :: init_data()
	 }).

%% -define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(X__, Y__), io:format(X__, Y__)).
-else.
-define(debug(X__, Y__), ok).
-endif.

%%--------------------------------------------------------------------

-spec launch(mode(), job(), init_data(), coordinator()) -> worker().

launch(Mode, Job, InitData, Coordinator) ->
  State = #state{mode        = Mode,
		 job         = Job,
		 init_data   = InitData,
		 coordinator = Coordinator},
  spawn_link(fun() -> init(State) end).

%%--------------------------------------------------------------------
%% Local functions.

init(#state{job = Job, mode = Mode, init_data = InitData} = State)
  when Mode =:= 'typesig'; Mode =:= 'dataflow' ->
  wait_for_success_typings(Mode, Job, InitData, State),
  run(State);
init(#state{mode = Mode} = State) when
    Mode =:= 'compile'; Mode =:= 'warnings';
    Mode =:= 'contract_remote_types'; Mode =:= 'record_remote_types' ->
  run(State).

run(#state{coordinator = Coordinator, job = Job} = State) ->
  dialyzer_coordinator:request_activation(Coordinator),
  Result = run_job(State),
  ?debug("~w: Done: ~p\n",[self(), Job]),
  dialyzer_coordinator:job_done(Job, Result, Coordinator).

-dialyzer({no_opaque_union, [run_job/1]}).
run_job(#state{mode = Mode, job = Job, init_data = InitData} = State) ->
  ?debug("~w: ~p: ~p\n", [self(), Mode, Job]),
  StartableJob = dialyzer_coordinator:get_job_input(Mode, Job),
  case Mode of
    compile ->
      case start_compilation(State) of
        {ok, EstimatedSize, Data} ->
          Label = ask_coordinator_for_label(EstimatedSize, State),
          continue_compilation(Label, Data);
        {error, _Reason} = Error ->
          Error
      end;
    _ ->
      StartableJob = dialyzer_coordinator:get_job_input(Mode, Job),
      case Mode of
        typesig -> dialyzer_succ_typings:find_succ_types_for_scc(StartableJob, InitData);
        dataflow -> dialyzer_succ_typings:refine_one_module(StartableJob, InitData);
        contract_remote_types ->
          dialyzer_contracts:process_contract_remote_types_module(StartableJob, InitData);
        record_remote_types ->
          dialyzer_utils:process_record_remote_types_module(StartableJob, InitData);
        warnings ->
          dialyzer_succ_typings:collect_warnings(StartableJob, InitData)
      end
  end.

start_compilation(#state{job = Job, init_data = InitData}) ->
  dialyzer_analysis_callgraph:start_compilation(Job, InitData).

ask_coordinator_for_label(EstimatedSize, #state{coordinator = Coordinator}) ->
  dialyzer_coordinator:get_next_label(EstimatedSize, Coordinator).

continue_compilation(Label, Data) ->
  dialyzer_analysis_callgraph:continue_compilation(Label, Data).

%% Wait for the results of success typings of modules or SCCs that we
%% depend on. ('typesig' or 'dataflow' mode)
wait_for_success_typings(Mode, Job, InitData, #state{coordinator = Coordinator}) ->
  JobLabel = dialyzer_coordinator:get_job_label(Mode, Job),
  DependsOnJobLabels = dialyzer_succ_typings:find_depends_on(JobLabel, InitData),
  ?debug("~w: Deps ~p: ~p\n", [self(), Job, DependsOnJobLabels]),
  dialyzer_coordinator:wait_for_success_typings(DependsOnJobLabels, Coordinator).
