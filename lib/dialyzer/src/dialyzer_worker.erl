%% -*- erlang-indent-level: 2 -*-
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

-module(dialyzer_worker).

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
	  init_data        :: init_data(),
	  depends_on  = [] :: list()
	 }).

-include("dialyzer.hrl").

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

init(#state{job = SCC, mode = Mode, init_data = InitData,
            coordinator = Coordinator} = State) when
    Mode =:= 'typesig'; Mode =:= 'dataflow' ->
  DependsOnSCCs = dialyzer_succ_typings:find_depends_on(SCC, InitData),
  ?debug("~w: Deps ~p: ~p\n", [self(), SCC, DependsOnSCCs]),
  Pids = dialyzer_coordinator:sccs_to_pids(DependsOnSCCs, Coordinator),
  ?debug("~w: PidsDeps ~p\n", [self(), Pids]),
  DependsOn = [{Pid, erlang:monitor(process, Pid)} || Pid <- Pids],
  loop(updating, State#state{depends_on = DependsOn});
init(#state{mode = Mode} = State) when
    Mode =:= 'compile'; Mode =:= 'warnings' ->
  loop(running, State).

loop(updating, #state{mode = Mode} = State) when
    Mode =:= 'typesig'; Mode =:= 'dataflow' ->
  ?debug("~w: Update: ~p\n", [self(), State#state.job]),
  NextStatus =
    case waits_more_success_typings(State) of
      true -> waiting;
      false -> running
    end,
  loop(NextStatus, State);
loop(waiting, #state{mode = Mode} = State) when
    Mode =:= 'typesig'; Mode =:= 'dataflow' ->
  ?debug("~w: Wait: ~p\n", [self(), State#state.job]),
  NewState = wait_for_success_typings(State),
  loop(updating, NewState);
loop(running, #state{mode = 'compile'} = State) ->
  request_activation(State),
  ?debug("Compile: ~s\n",[State#state.job]),
  Result =
    case start_compilation(State) of
      {ok, EstimatedSize, Data} ->
	Label = ask_coordinator_for_label(EstimatedSize, State),
	continue_compilation(Label, Data);
      {error, _Reason} = Error ->
	Error
    end,
  report_to_coordinator(Result, State);
loop(running, #state{mode = 'warnings'} = State) ->
  request_activation(State),
  ?debug("Warning: ~s\n",[State#state.job]),
  Result = collect_warnings(State),
  report_to_coordinator(Result, State);
loop(running, #state{mode = Mode} = State) when
    Mode =:= 'typesig'; Mode =:= 'dataflow' ->
  request_activation(State),
  ?debug("~w: Run: ~p\n", [self(), State#state.job]),
  NotFixpoint = do_work(State),
  report_to_coordinator(NotFixpoint, State).

waits_more_success_typings(#state{depends_on = Depends}) ->
  Depends =/= [].

wait_for_success_typings(#state{depends_on = DependsOn} = State) ->
  receive
    {'DOWN', Ref, process, Pid, _Info} ->
      ?debug("~w: ~p got DOWN: ~p\n", [self(), State#state.job, Pid]),
      State#state{depends_on = DependsOn -- [{Pid, Ref}]}
  after
    5000 ->
      ?debug("~w: Still Waiting ~p:\n   ~p\n",  [self(), State#state.job, DependsOn]),
      State
  end.

request_activation(#state{coordinator = Coordinator}) ->
  dialyzer_coordinator:request_activation(Coordinator).

do_work(#state{mode = Mode, job = Job, init_data = InitData}) ->
  case Mode of
    typesig -> dialyzer_succ_typings:find_succ_types_for_scc(Job, InitData);
    dataflow -> dialyzer_succ_typings:refine_one_module(Job, InitData)
  end.

report_to_coordinator(Result, #state{job = Job, coordinator = Coordinator}) ->
  ?debug("~w: Done: ~p\n",[self(), Job]),
  dialyzer_coordinator:job_done(Job, Result, Coordinator).

start_compilation(#state{job = Job, init_data = InitData}) ->
  dialyzer_analysis_callgraph:start_compilation(Job, InitData).

ask_coordinator_for_label(EstimatedSize, #state{coordinator = Coordinator}) ->
  dialyzer_coordinator:get_next_label(EstimatedSize, Coordinator).

continue_compilation(Label, Data) ->
  dialyzer_analysis_callgraph:continue_compilation(Label, Data).

collect_warnings(#state{job = Job, init_data = InitData}) ->
  dialyzer_succ_typings:collect_warnings(Job, InitData).
