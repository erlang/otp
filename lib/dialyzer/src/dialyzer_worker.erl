%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2010. All Rights Reserved.
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

-export([launch/4]).

-export_type([worker/0]).

-type worker() :: pid(). %%opaque

-type mode() :: dialyzer_coordinator:mode().
-type coordinator() :: dialyzer_coordinator:coordinator().
-type servers() :: dialyzer_succ_typings:servers() |
		   dialyzer_analysis_callgraph:servers().
-type data()    :: dialyzer_succ_typings:scc_data() |
		   dialyzer_succ_typings:scc_refine_data().

-record(state, {
	  mode             :: mode(),
	  job              :: mfa_or_funlbl() | file:filename(),
	  coordinator      :: coordinator(),
	  servers          :: servers(),
	  depends_on  = [] :: list(),
	  scc_data         :: data()
	 }).

-include("dialyzer.hrl").

%% -define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(X__, Y__), io:format(X__, Y__)).
-else.
-define(debug(X__, Y__), ok).
-endif.

%%--------------------------------------------------------------------

-spec launch(mode(), [mfa_or_funlbl()], servers(), coordinator()) -> worker().

launch(Mode, Job, Servers, Coordinator) ->
  State = #state{mode        = Mode,
		 job         = Job,
		 servers     = Servers,
		 coordinator = Coordinator},
  InitState =
    case Mode of
      X when X =:= 'typesig'; X =:= 'dataflow' -> initializing;
      'compile' -> running
    end,
  spawn(fun() -> loop(InitState, State) end).

%%--------------------------------------------------------------------

loop(updating, State) ->
  ?debug("Update: ~p\n",[State#state.job]),
  NextStatus =
    case waits_more_success_typings(State) of
      true -> waiting;
      Other ->
	case has_data(State) of
	  false -> getting_data;
	  true ->
	    case Other of
	      imminent -> waiting;
	      false -> running
	    end
	end
    end,
  loop(NextStatus, State);
loop(initializing, #state{job = SCC, servers = Servers} = State) ->
  DependsOn = dialyzer_succ_typings:find_depends_on(SCC, Servers),
  WithoutSelf = DependsOn -- [SCC],
  ?debug("Deps ~p: ~p\n",[State#state.job, WithoutSelf]),
  loop(updating, State#state{depends_on = WithoutSelf});
loop(waiting, State) ->
  ?debug("Wait: ~p\n",[State#state.job]),
  NewState = wait_for_success_typings(State),
  loop(updating, NewState);
loop(getting_data, State) ->
  ?debug("Data: ~p\n",[State#state.job]),
  loop(updating, get_data(State));
loop(running, #state{mode = 'compile'} = State) ->
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
loop(running, #state{mode = Mode} = State) when
    Mode =:= 'typesig'; Mode =:= 'dataflow' ->
  ?debug("Run: ~p\n",[State#state.job]),
  ok = ask_coordinator_for_callers(State),
  NotFixpoint = do_work(State),
  Callers = get_callers_reply_from_coordinator(),
  ok = broadcast_done(State, Callers),
  report_to_coordinator(NotFixpoint, State).

waits_more_success_typings(#state{depends_on = Depends}) ->
  case Depends of
    [] -> false;
    [_] -> imminent;
    _ -> true
  end.

has_data(#state{scc_data = Data}) ->
  case Data of
    undefined -> false;
    _ -> true
  end.

get_data(#state{mode = Mode, job = SCC, servers = Servers} = State) ->
  Data =
    case Mode of
      typesig -> dialyzer_succ_typings:collect_scc_data(SCC, Servers);
      dataflow -> dialyzer_succ_typings:collect_refine_scc_data(SCC, Servers)
    end,
  State#state{scc_data = Data}.

ask_coordinator_for_callers(#state{job = SCC,
				   servers = Servers,
				   coordinator = Coordinator}) ->
  RequiredBy = dialyzer_succ_typings:find_required_by(SCC, Servers),
  WithoutSelf = RequiredBy -- [SCC],
  ?debug("Waiting for me~p: ~p\n",[SCC, WithoutSelf]),
  dialyzer_coordinator:sccs_to_pids_request(WithoutSelf, Coordinator).

get_callers_reply_from_coordinator() ->
  dialyzer_coordinator:sccs_to_pids_reply().

broadcast_done(#state{job = SCC}, Callers) ->
  ?debug("Sending ~p: ~p\n",[SCC, Callers]),
  SendSTFun = fun(PID) -> PID ! {done, SCC} end,
  lists:foreach(SendSTFun, Callers).

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

do_work(#state{mode = Mode, scc_data = SCCData}) ->
  case Mode of
    typesig -> dialyzer_succ_typings:find_succ_types_for_scc(SCCData);
    dataflow -> dialyzer_succ_typings:refine_one_module(SCCData)
  end.

report_to_coordinator(Result, #state{job = Job, coordinator = Coordinator}) ->
  ?debug("Done: ~p\n",[Job]),
  dialyzer_coordinator:job_done(Job, Result, Coordinator).

start_compilation(#state{job = Job, servers = Servers}) ->
  dialyzer_analysis_callgraph:start_compilation(Job, Servers).

ask_coordinator_for_label(EstimatedSize, #state{coordinator = Coordinator}) ->
  dialyzer_coordinator:get_next_label(EstimatedSize, Coordinator).

continue_compilation(Label, Data) ->
  dialyzer_analysis_callgraph:continue_compilation(Label, Data).
