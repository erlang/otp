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

-export([
	 all_spawned/1,
	 scc_done/3,
	 scc_spawn/2,
	 sccs_to_pids_reply/0,
	 sccs_to_pids_request/2,
	 start/2,
	 receive_not_fixpoint/0
	]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-type coordinator() :: pid().
-type map() :: dict().
-type scc() :: [mfa_or_funlbl()].
-type mode()   :: typesig | dataflow.

-record(state, {parent                   :: pid(),
		mode                     :: mode(),
		spawn_count  = 0         :: integer(),
		all_spawned  = false     :: boolean(),
		scc_to_pid   = new_map() :: map(),
		not_fixpoint = []        :: [mfa_or_funlbl()],
		servers                  :: dialyzer_typesig:servers()
	       }).

-include("dialyzer.hrl").

%%--------------------------------------------------------------------

-spec start(mode(), dialyzer_typesig:servers()) -> pid().

start(Mode, Servers) ->
    {ok, Pid} = gen_server:start(?MODULE, {self(), Mode, Servers}, []),
    Pid.

-spec scc_spawn(scc(), coordinator()) -> ok.

scc_spawn(SCC, Coordinator) ->
    cast({scc_spawn, SCC}, Coordinator).

-spec sccs_to_pids_request([scc()], coordinator()) -> ok.

sccs_to_pids_request(SCCs, Coordinator) ->
    cast({sccs_to_pids, SCCs, self()}, Coordinator).

scc_to_pids_request_handle(Worker, SCCs, SCCtoPID) ->
    Pids = [fetch_map(SCC, SCCtoPID) || SCC <- SCCs],
    Worker ! {sccs_to_pids, Pids},
    ok.

-spec sccs_to_pids_reply() -> [dialyzer_worker:worker()].

sccs_to_pids_reply() ->
    receive {sccs_to_pids, Pids} -> Pids end.

-spec scc_done(scc(), scc(), coordinator()) -> ok.

scc_done(SCC, NotFixpoint, Coordinator) ->
    cast({scc_done, SCC, NotFixpoint}, Coordinator).

-spec all_spawned(coordinator()) -> ok.

all_spawned(Coordinator) ->
    cast(all_spawned, Coordinator).

send_done_to_parent(#state{parent = Parent, not_fixpoint = NotFixpoint}) ->
    Parent ! {not_fixpoint, NotFixpoint}.

-spec receive_not_fixpoint() -> dialyzer_plt:plt().

receive_not_fixpoint() ->
    receive {not_fixpoint, NotFixpoint} -> NotFixpoint end.

%%--------------------------------------------------------------------

-spec init({pid(), mode(), dialyzer_succ_typings:servers()}) -> {ok, #state{}}.

init({Parent, Mode, Servers}) ->
    {ok, #state{parent = Parent, mode = Mode, servers = Servers}}.

-spec handle_call(Query::term(), From::term(), #state{}) ->
        {reply, Reply::term(), #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(Msg::term(), #state{}) ->
        {noreply, #state{}} | {stop, normal, #state{}}.

handle_cast({scc_done, _SCC, NotFixpoint},
	    #state{spawn_count = SpawnCount,
		   all_spawned = AllSpawned,
		   not_fixpoint = OldNotFixpoint
		  } = State) ->
    NewNotFixpoint = ordsets:union(OldNotFixpoint, NotFixpoint),
    UpdatedState = State#state{not_fixpoint = NewNotFixpoint},
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
handle_cast({sccs_to_pids, SCCs, Worker},
	    #state{scc_to_pid = SCCtoPID} = State) ->
    scc_to_pids_request_handle(Worker, SCCs, SCCtoPID),
    {noreply, State};
handle_cast({scc_spawn, SCC},
	    #state{mode = Mode,
		   servers = Servers,
		   spawn_count = SpawnCount,
		   scc_to_pid = SCCtoPID
		  } = State) ->
    Pid = dialyzer_worker:launch(Mode, SCC, Servers),
    {noreply,
     State#state{spawn_count = SpawnCount + 1,
		 scc_to_pid = store_map(SCC, Pid, SCCtoPID)}
    }.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.

handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(term(), #state{}) -> ok.

terminate(_Reason, _State) ->
  ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------

cast(Message, Coordinator) ->
    gen_server:cast(Coordinator, Message).

new_map() ->
    dict:new().

store_map(Key, Value, Map) ->
    dict:store(Key, Value, Map).

fetch_map(Key, Map) ->
    dict:fetch(Key, Map).
