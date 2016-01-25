%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2012. All Rights Reserved.
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

-module(dialyzer_worker).

-export([launch/4]).

-export_type([worker/0]).

-opaque worker() :: pid().

-type mode()        :: dialyzer_coordinator:mode().
-type coordinator() :: dialyzer_coordinator:coordinator().
-type init_data()   :: dialyzer_coordinator:init_data().

-record(state, {
	  mode             :: mode(),
	  job              :: mfa_or_funlbl() | file:filename(),
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

-spec launch(mode(), [mfa_or_funlbl()], init_data(), coordinator()) -> worker().

launch(Mode, Job, InitData, Coordinator) ->
  State = #state{mode        = Mode,
		 job         = Job,
		 init_data   = InitData,
		 coordinator = Coordinator},
  spawn_link(fun() -> init(State) end).

%%--------------------------------------------------------------------

init(#state{mode = Mode} = State) when
    Mode =:= 'typesig'; Mode =:= 'dataflow' ->
  SCC = State#state.job,
  DependsOn = dialyzer_succ_typings:find_depends_on(SCC, State#state.init_data),
  ?debug("Deps ~p: ~p\n",[SCC, DependsOn]),
  loop(updating, State#state{depends_on = DependsOn});
init(#state{mode = Mode} = State) when
    Mode =:= 'compile'; Mode =:= 'warnings' ->
  loop(running, State).

loop(updating, #state{mode = Mode} = State) when
    Mode =:= 'typesig'; Mode =:= 'dataflow' ->
  ?debug("Update: ~p\n",[State#state.job]),
  NextStatus =
    case State#state.depends_on =/= [] of
      true -> waiting; %% Waiting on more success typings.
      false -> running
    end,
  loop(NextStatus, State);
loop(waiting, #state{mode = Mode} = State) when
    Mode =:= 'typesig'; Mode =:= 'dataflow' ->
  ?debug("Wait: ~p\n",[State#state.job]),
  NewState = wait_for_success_typings(State),
  loop(updating, NewState);
loop(running, #state{mode = 'compile'} = State) ->
  dialyzer_coordinator:request_activation(State#state.coordinator),
  ?debug("Compile: ~s\n",[State#state.job]),
  Result =
    case dialyzer_analysis_callgraph:start_compilation(State#state.job,
                                                       State#state.init_data) of
      {ok, EstimatedSize, Data} ->
	Label = dialyzer_coordinator:get_next_label(EstimatedSize,
                                                    State#state.coordinator),
	dialyzer_analysis_callgraph:continue_compilation(Label, Data);
      {error, _Reason} = Error ->
	Error
    end,
  report_to_coordinator(Result, State);
loop(running, #state{mode = 'warnings'} = State) ->
  dialyzer_coordinator:request_activation(State#state.coordinator),
  ?debug("Warning: ~s\n",[State#state.job]),
  Result = dialyzer_succ_typings:collect_warnings(State#state.job,
                                                  State#state.init_data),
  report_to_coordinator(Result, State);
loop(running, #state{mode = Mode} = State) when
    Mode =:= 'typesig'; Mode =:= 'dataflow' ->
  dialyzer_coordinator:request_activation(State#state.coordinator),
  ?debug("Run: ~p\n",[State#state.job]),
  NotFixpoint =
    case Mode of
      typesig ->
        dialyzer_succ_typings:find_succ_types_for_scc(State#state.job,
                                                      State#state.init_data);
      dataflow ->
        dialyzer_succ_typings:refine_one_module(State#state.job,
                                                State#state.init_data)
    end,
  ok = broadcast_done(State),
  report_to_coordinator(NotFixpoint, State).

broadcast_done(#state{job = SCC, init_data = InitData,
		      coordinator = Coordinator}) ->
  RequiredBy = dialyzer_succ_typings:find_required_by(SCC, InitData),
  {Callers, Unknown} =
    dialyzer_coordinator:sccs_to_pids(RequiredBy, Coordinator),
  send_done(Callers, SCC),
  continue_broadcast_done(Unknown, SCC, Coordinator).

send_done(Callers, SCC) ->
  ?debug("Sending ~p: ~p\n",[SCC, Callers]),
  SendSTFun = fun(PID) -> PID ! {done, SCC} end,
  lists:foreach(SendSTFun, Callers).

continue_broadcast_done([], _SCC, _Coordinator) -> ok;
continue_broadcast_done(Rest, SCC, Coordinator) ->
  %% This time limit should be greater than the time required
  %% by the coordinator to spawn all processes.
  timer:sleep(500),
  {Callers, Unknown} = dialyzer_coordinator:sccs_to_pids(Rest, Coordinator),
  send_done(Callers, SCC),
  continue_broadcast_done(Unknown, SCC, Coordinator).

wait_for_success_typings(#state{depends_on = DependsOn} = State) ->
  receive
    {done, SCC} ->
      ?debug("GOT ~p: ~p\n",[State#state.job, SCC]),
      State#state{depends_on = DependsOn -- [SCC]}
  after
    5000 ->
      ?debug("Still Waiting ~p: ~p\n",[State#state.job, DependsOn]),
      State
  end.

report_to_coordinator(Result, #state{job = Job, coordinator = Coordinator}) ->
  ?debug("Done: ~p\n",[Job]),
  dialyzer_coordinator:job_done(Job, Result, Coordinator).
