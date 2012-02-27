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
%%%
%%% Description:
%%%
%%% The parallel version of Dialyzer's typesig analysis is spread over 4 modules
%%% with the intention to both minimize the changes on the original code and use
%%% a separate module for every kind of Erlang process that will be running.
%%%
%%% There are therefore 3 kinds of processes:
%%%
%%% - The original Dialyzer backend (in succ_typings module)
%%% - The worker process for the typesig analysis (in typesig and
%%%   worker)
%%% - A coordinator of the worker processes (in coordinator)
%%%
%%% Operation guidelines:
%%%
%%% - The backend requests from the coordinator to spawn a worker for each SCC
%%% - The backend notifies the coordinator when all SCC have been spawned and
%%%   waits for the server to report that the PLT has been updated
%%% - Each worker is responsible to notify all those who wait for it.
%%%
%%%-------------------------------------------------------------------

-module(dialyzer_coordinator).

%%% Exports for all possible uses of coordinator from main process
-export([start/2, all_spawned/1]).

%%% Exports for all possible workers
-export([wait_activation/0, job_done/3]).

%%% Export for the typesig, dataflow and warnings main process
-export([scc_spawn/2]).

%%% Export for the typesig and dataflow analysis main process
-export([receive_not_fixpoint/0]).

%%% Export for warning main process
-export([receive_warnings/0]).

%%% Exports for the typesig and dataflow analysis workers
-export([request_activation/1, sccs_to_pids/1]).

%%% Exports for the compilation main process
-export([compiler_spawn/2, receive_compilation_data/0]).

%%% Exports for the compilation workers
-export([get_next_label/2, compilation_done/3]).

-export_type([coordinator/0, mode/0]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

%%--------------------------------------------------------------------

-define(MAP, dialyzer_coordinator_map).

-type coordinator() :: pid(). %%opaque

-type scc()     :: [mfa_or_funlbl()] | module().
-type mode()    :: 'typesig' | 'dataflow' | 'compile' | 'warnings'.
-type servers() :: dialyzer_succ_typings:servers() |
		   dialyzer_analysis_callgraph:servers() |
		   dialyzer_succ_typings:warning_servers().

-record(state, {parent               :: pid(),
		mode                 :: mode(),
		spawn_count  = 0     :: integer(),
		all_spawned  = false :: boolean(),
		next_label           :: integer(),
		result               :: [mfa_or_funlbl()] |
					dialyzer_analysis_callgraph:result() |
					[dial_warning()],
		init_job_data        :: servers(),
		tickets              :: integer(),
		queue                :: queue()
	       }).

-include("dialyzer.hrl").

%%--------------------------------------------------------------------

-spec start(mode(), servers()) -> coordinator().

start(Mode, Servers) ->
  {ok, Pid} = gen_server:start(?MODULE, {self(), Mode, Servers}, []),
  Pid.

-spec scc_spawn(scc() | module(), coordinator()) -> ok.

scc_spawn(SCC, Coordinator) ->
  cast({scc_spawn, SCC}, Coordinator).

-spec sccs_to_pids([scc()]) -> {[dialyzer_worker:worker()], [scc()]}.

sccs_to_pids(SCCs) ->
  lists:foldl(fun pid_partition/2, {[], []}, SCCs).

pid_partition(SCC, {Pids, Unknown}) ->
  try ets:lookup_element(?MAP, SCC, 2) of
      Result -> {[Result|Pids], Unknown}
  catch
    _:_ -> {Pids, [SCC|Unknown]}
  end.

-spec job_done(scc() | file:filename(), term(), coordinator()) -> ok.

job_done(Job, Result, Coordinator) ->
  cast({done, Job, Result}, Coordinator).

-spec compilation_done(file:filename(),
		       dialyzer_analysis_callgraph:compilation_data(),
		       coordinator()) -> ok.

compilation_done(Filename, CompilationData, Coordinator) ->
  cast({done, Filename, CompilationData}, Coordinator).

-spec all_spawned(coordinator()) -> ok.

all_spawned(Coordinator) ->
  cast(all_spawned, Coordinator).

send_done_to_parent(#state{mode = Mode,
			   parent = Parent,
			   result = Result,
			   next_label = NextLabel}) ->
  Msg =
    case Mode of
      X when X =:= 'typesig'; X =:= 'dataflow' -> {not_fixpoint, Result};
      'compile' -> {compilation_data, Result, NextLabel};
      'warnings' -> {warnings, Result}
    end,
  Parent ! Msg,
  ok.

-spec receive_not_fixpoint() -> [mfa_or_funlbl()].

receive_not_fixpoint() ->
  receive {not_fixpoint, NotFixpoint} -> NotFixpoint end.

-spec receive_compilation_data() ->
        {dialyzer_analysis_callgraph:result(), integer()}.

receive_compilation_data() ->
  receive {compilation_data, CompilationData, NextLabel} ->
      {CompilationData, NextLabel}
  end.

-spec receive_warnings() -> [dial_warning()].

receive_warnings() ->
  receive {warnings, Warnings} -> Warnings end.

-spec compiler_spawn(file:filename(), coordinator()) -> ok.

compiler_spawn(Filename, Coordinator) ->
  cast({compiler_spawn, Filename}, Coordinator).

-spec get_next_label(integer(), coordinator()) -> integer().

get_next_label(EstimatedSize, Coordinator) ->
  call({get_next_label, EstimatedSize}, Coordinator).

-spec request_activation(coordinator()) -> ok.

request_activation(Coordinator) ->
  cast({request_activation, self()}, Coordinator).

-spec wait_activation() -> ok.

wait_activation() ->
  receive activate -> ok end.

activate_pid(Pid) ->
  Pid ! activate.

%%--------------------------------------------------------------------

-spec init({pid(), mode(), servers()}) -> {ok, #state{}}.

init({Parent, Mode, InitJobData}) ->
  BaseTickets = erlang:system_info(logical_processors_available),
  Tickets =
    case Mode of
      'compile' -> 4*BaseTickets;
      'typesig' -> BaseTickets*BaseTickets;
      'dataflow' -> 4*BaseTickets;
      'warnings' -> 4*BaseTickets
    end,
  InitState =
    #state{parent = Parent, mode = Mode, init_job_data = InitJobData,
	   tickets = Tickets, queue = queue:new()},
  State =
    case Mode of
      X when X =:= 'typesig'; X =:= 'dataflow' ->
	?MAP = ets:new(?MAP, [named_table, {read_concurrency, true}]),
	InitState#state{result = []};
      'warnings' ->
	InitState#state{result = []};
      'compile' ->
	InitResult = dialyzer_analysis_callgraph:compile_coordinator_init(),
	InitState#state{result = InitResult, next_label = 0}
    end,
  {ok, State}.

-spec handle_call(Query::term(), From::term(), #state{}) ->
		     {reply, Reply::term(), #state{}}.

handle_call({get_next_label, EstimatedSize}, _From,
	    #state{next_label = NextLabel} = State) ->
  {reply, NextLabel, State#state{next_label = NextLabel + EstimatedSize}}.

-spec handle_cast(Msg::term(), #state{}) ->
		     {noreply, #state{}} | {stop, normal, #state{}}.

handle_cast({done, Job, NewData},
	    #state{mode = Mode,
		   spawn_count = SpawnCount,
		   all_spawned = AllSpawned,
		   result = OldResult,
		   init_job_data = Servers,
		   tickets = Tickets,
		   queue = Queue
		  } = State) ->
  {Waiting, NewQueue} = queue:out(Queue),
  NewTickets =
    case Waiting of
      empty -> Tickets+1;
      {value, Pid} ->
	activate_pid(Pid),
	Tickets
    end,
  NewResult =
    case Mode of
      X when X =:= 'typesig'; X =:= 'dataflow' ->
	FinalData = dialyzer_succ_typings:lookup_names(NewData, Servers),
	FinalData ++ OldResult;
      'compile' ->
	dialyzer_analysis_callgraph:add_to_result(Job, NewData, OldResult);
      'warnings' ->
	NewData ++ OldResult
    end,
  UpdatedState =
    State#state{result = NewResult, tickets = NewTickets, queue = NewQueue},
  Action =
    case AllSpawned of
      false -> reduce;
      true ->
	case SpawnCount of
	  1 -> finish;
	  _ -> reduce
	end
    end,
  case Action of
    reduce ->
      NewState = UpdatedState#state{spawn_count = SpawnCount - 1},
      {noreply, NewState};
    finish ->
      send_done_to_parent(UpdatedState),
      {stop, normal, State}
  end;
handle_cast(all_spawned, #state{spawn_count = SpawnCount} = State) ->
  case SpawnCount of
    0 ->
      send_done_to_parent(State),
      {stop, normal, State};
    _ ->
      NewState = State#state{all_spawned = true},
      {noreply, NewState}
  end;
handle_cast({request_activation, Pid},
	    #state{tickets = Tickets, queue = Queue} = State) ->
  {NewTickets, NewQueue} =
    case Tickets of
      0 -> {Tickets, queue:in(Pid, Queue)};
      N ->
	activate_pid(Pid),
	{N-1, Queue}
    end,
  {noreply, State#state{tickets = NewTickets, queue = NewQueue}};
handle_cast({scc_spawn, SCC},
	    #state{mode = Mode,
		   init_job_data = Servers,
		   spawn_count = SpawnCount,
		   tickets = Tickets,
		   queue = Queue} = State) ->
  Pid = dialyzer_worker:launch(Mode, SCC, Servers, self()),
  {NewTickets, NewQueue} =
    case Mode of
      X when X =:= 'typesig'; X =:= 'dataflow' ->
	true = ets:insert(?MAP, {SCC, Pid}),
	{Tickets, Queue};
    warnings ->
	case Tickets of
	  0 -> {Tickets, queue:in(Pid, Queue)};
	  N ->
	    activate_pid(Pid),
	    {N-1, Queue}
	end
    end,
  {noreply, State#state{spawn_count = SpawnCount + 1,
			tickets = NewTickets,
			queue = NewQueue}};
handle_cast({compiler_spawn, Filename},
	    #state{mode = Mode,
		   init_job_data = Servers,
		   spawn_count = SpawnCount,
		   tickets = Tickets,
		   queue = Queue
		  } = State) ->
  Pid = dialyzer_worker:launch(Mode, Filename, Servers, self()),
  NewSpawnCount = SpawnCount + 1,
  {NewTickets, NewQueue} =
    case Tickets of
      0 -> {Tickets, queue:in(Pid, Queue)};
      N ->
	activate_pid(Pid),
	{N-1, Queue}
    end,
  {noreply, State#state{spawn_count = NewSpawnCount,
			tickets = NewTickets,
			queue = NewQueue}}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.

handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(term(), #state{}) -> ok.

terminate(_Reason, #state{mode = Mode}) ->
  case Mode of
    X when X =:= 'typesig'; X =:= 'dataflow' -> ets:delete(?MAP);
    _ -> true
  end,
  ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------

cast(Message, Coordinator) ->
  gen_server:cast(Coordinator, Message).

call(Message, Coordinator) ->
  gen_server:call(Coordinator, Message, infinity).
