%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2012. All Rights Reserved.
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

-module(dialyzer_worker).

-export([launch/4, sequential/4]).

-export_type([worker/0]).

-type worker() :: pid(). %%opaque

-type mode()        :: dialyzer_coordinator:mode().
-type coordinator() :: dialyzer_coordinator:coordinator().
-type init_data()   :: dialyzer_coordinator:init_data().
-type result()      :: dialyzer_coordinator:result().

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
  InitState =
    case Mode of
      X when X =:= 'typesig'; X =:= 'dataflow' -> initializing;
      X when X =:= 'compile'; X =:= 'warnings' -> running
    end,
  spawn_link(fun() -> loop(InitState, State) end).

%%--------------------------------------------------------------------

loop(updating, State) ->
  ?debug("Update: ~p\n",[State#state.job]),
  NextStatus =
    case waits_more_success_typings(State) of
      true -> waiting;
      false -> running
    end,
  loop(NextStatus, State);
loop(initializing, #state{job = SCC, init_data = InitData} = State) ->
  DependsOn = dialyzer_succ_typings:find_depends_on(SCC, InitData),
  ?debug("Deps ~p: ~p\n",[State#state.job, DependsOn]),
  loop(updating, State#state{depends_on = DependsOn});
loop(waiting, State) ->
  ?debug("Wait: ~p\n",[State#state.job]),
  NewState = wait_for_success_typings(State),
  loop(updating, NewState);
loop(running, #state{mode = 'compile'} = State) ->
  dialyzer_coordinator:wait_activation(),
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
  dialyzer_coordinator:wait_activation(),
  ?debug("Warning: ~s\n",[State#state.job]),
  Result = collect_warnings(State),
  report_to_coordinator(Result, State);
loop(running, #state{mode = Mode} = State) when
    Mode =:= 'typesig'; Mode =:= 'dataflow' ->
  request_activation(State),
  ?debug("Run: ~p\n",[State#state.job]),
  NotFixpoint = do_work(State),
  ok = broadcast_done(State),
  report_to_coordinator(NotFixpoint, State).

waits_more_success_typings(#state{depends_on = Depends}) ->
  Depends =/= [].

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

request_activation(#state{coordinator = Coordinator}) ->
  dialyzer_coordinator:request_activation(Coordinator).

do_work(#state{mode = Mode, job = Job, init_data = InitData}) ->
  case Mode of
    typesig -> dialyzer_succ_typings:find_succ_types_for_scc(Job, InitData);
    dataflow -> dialyzer_succ_typings:refine_one_module(Job, InitData)
  end.

report_to_coordinator(Result, #state{job = Job, coordinator = Coordinator}) ->
  ?debug("Done: ~p\n",[Job]),
  dialyzer_coordinator:job_done(Job, Result, Coordinator).

start_compilation(#state{job = Job, init_data = InitData}) ->
  dialyzer_analysis_callgraph:start_compilation(Job, InitData).

ask_coordinator_for_label(EstimatedSize, #state{coordinator = Coordinator}) ->
  dialyzer_coordinator:get_next_label(EstimatedSize, Coordinator).

continue_compilation(Label, Data) ->
  dialyzer_analysis_callgraph:continue_compilation(Label, Data).

collect_warnings(#state{job = Job, init_data = InitData}) ->
  dialyzer_succ_typings:collect_warnings(Job, InitData).

%%------------------------------------------------------------------------------

-type extra() :: label() | 'unused'.

-spec sequential(mode(), [mfa_or_funlbl()], init_data(), extra()) -> result().

sequential('compile', Job, InitData, Extra) ->
  case dialyzer_analysis_callgraph:start_compilation(Job, InitData) of
    {ok, EstimatedSize, Data} ->
      {EstimatedSize, continue_compilation(Extra, Data)};
    {error, _Reason} = Error -> {0, Error}
  end;
sequential('typesig', Job, InitData, _Extra) ->
  dialyzer_succ_typings:find_succ_types_for_scc(Job, InitData);
sequential('dataflow', Job, InitData, _Extra) ->
  dialyzer_succ_typings:refine_one_module(Job, InitData);
sequential('warnings', Job, InitData, _Extra) ->
  dialyzer_succ_typings:collect_warnings(Job, InitData).
