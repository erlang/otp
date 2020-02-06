%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(disk_log_server).
-behaviour(gen_server).

-export([start_link/0, start/0, open/1, close/1, 
	 get_log_pids/1, accessible_logs/0]).

%% Local export.
-export([dist_open/1, get_local_pid/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, terminate/2]).
-export([handle_cast/2, code_change/3]). % just to avoid compiler warning

-include("disk_log.hrl").

-compile({inline,[{do_get_log_pids,1}]}).

-record(pending, {log, pid, req, from, attach, clients}). % [{Request,From}]

-record(state, {pending = [] :: [#pending{}]}).

-compile({nowarn_deprecated_function, [{pg2, create, 1}]}).
-compile({nowarn_deprecated_function, [{pg2, join, 2}]}).
-compile({nowarn_deprecated_function, [{pg2, leave, 2}]}).
-compile({nowarn_deprecated_function, [{pg2, which_groups, 0}]}).
-compile({nowarn_deprecated_function, [{pg2, get_members, 1}]}).

%%%-----------------------------------------------------------------
%%% This module implements the disk_log server.  Its primary purpose
%%% is to keep the ets table 'disk_log_names' updated and to handle
%%% distribution data (pids) using the module pg2.
%%%-----------------------------------------------------------------
%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->  
    gen_server:start_link({local, disk_log_server}, disk_log_server, [], []).

start() -> 
    ensure_started().

open({ok, A}) ->
    ensure_started(),
    gen_server:call(disk_log_server, {open, local, A}, infinity);
open(Other) ->
    Other.

%% To be used from this module only.
dist_open(A) ->
    ensure_started(),
    gen_server:call(disk_log_server, {open, distr, A}, infinity).

close(Pid) ->
    gen_server:call(disk_log_server, {close, Pid}, infinity).

get_log_pids(LogName) ->
    do_get_log_pids(LogName).

accessible_logs() ->
    ensure_started(),
    do_accessible_logs().

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%% It would have been really nice to have a tag for disk log groups,
%% like {distributed_disk_log, Log}, but backward compatibility makes
%% it hard to introduce.
-define(group(Log), Log).

init([]) ->
    process_flag(trap_exit, true),
    _ = ets:new(?DISK_LOG_NAME_TABLE, [named_table, set]),
    _= ets:new(?DISK_LOG_PID_TABLE, [named_table, set]),
    {ok, #state{}}.

handle_call({open, W, A}, From, State) ->
    open([{{open, W, A}, From}], State);
handle_call({close, Pid}, _From, State) ->
    Reply = do_close(Pid),
    {reply, Reply, State}.

handle_info({pending_reply, Pid, Result0}, State) ->
    {value, #pending{log = Name, pid = Pid, from = From, 
                     req = Request, attach = Attach,
                     clients = Clients}} = 
        lists:keysearch(Pid, #pending.pid, State#state.pending),
    NP = lists:keydelete(Pid, #pending.pid, State#state.pending),
    State1 = State#state{pending = NP},
    if 
        Attach and (Result0 =:= {error, no_such_log}) ->
            %% The disk_log process has terminated. Try again.
            open([{Request,From} | Clients], State1);
        true -> 
            case Result0 of
                _ when Attach -> 
                    ok;
                {error, _} -> 
                    ok;
                _ ->
                    put(Pid, Name),
                    link(Pid),
                    {_, Locality, _} = Request,
                    ets:insert(?DISK_LOG_PID_TABLE, {Pid, Name}),
                    ets:insert(?DISK_LOG_NAME_TABLE, {Name, Pid, Locality}),
                    if 
                        Locality =:= distr -> 
                            ok = pg2:join(?group(Name), Pid);
                        true ->
                            ok
                    end
            end,
            gen_server:reply(From, result(Request, Result0)),
            open(Clients, State1)
    end;
handle_info({'EXIT', Pid, _Reason}, State) ->
    %% If there are clients waiting to be attached to this log, info
    %% {pending_reply,Pid,{error,no_such_log}} will soon arrive.
    case get(Pid) of
        undefined ->
            ok;
        Name -> 
            erase_log(Name, Pid)
    end,
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.
	    
%% Just to avoid compiler warning.
handle_cast(_, State) ->
    {noreply, State}.

%% Just to avoid compiler warning.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
terminate(_Reason, _) ->
    ok.

%%%-----------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------

ensure_started() ->
    case whereis(disk_log_server) of
	undefined ->
	    LogSup = {disk_log_sup, {disk_log_sup, start_link, []}, permanent,
		      1000, supervisor, [disk_log_sup]},
	    {ok, _} = ensure_child_started(kernel_safe_sup, LogSup),
	    LogServer = {disk_log_server,
			 {disk_log_server, start_link, []},
			 permanent, 2000, worker, [disk_log_server]},
	    {ok, _} = ensure_child_started(kernel_safe_sup, LogServer),
	    ok;
	_ -> ok
    end.

ensure_child_started(Sup,Child) ->
    case supervisor:start_child(Sup, Child) of
	{ok,Pid} ->
	    {ok,Pid};
	{error,{already_started,Pid}} ->
	    {ok,Pid};
	Error ->
	    Error
    end.

open([{Req, From} | L], State) ->
    State2 = case do_open(Req, From, State) of
                 {pending, State1} -> 
                     State1;
                 {Reply, State1} ->
                     gen_server:reply(From, Reply),
                     State1
             end,
    open(L, State2);
open([], State) ->
    {noreply, State}.

%% -> {OpenRet, NewState} | {{node(),OpenRet}, NewState} |
%%    {pending, NewState}
do_open({open, W, #arg{name = Name}=A}=Req, From, State) ->
    case check_pending(Name, From, State, Req) of
        {pending, NewState} -> 
            {pending, NewState};
        false when W =:= local ->
            case A#arg.distributed of
                {true, Nodes} ->
                    Fun = open_distr_rpc_fun(Nodes, A, From),
                    _Pid = spawn(Fun),
                    %% No pending reply is expected, but don't reply yet.
                    {pending, State};
                false ->
                    case get_local_pid(Name) of
                        {local, Pid} ->
                            do_internal_open(Name, Pid, From, Req, true,State);
                        {distributed, _Pid} ->
                            {{error, {node_already_open, Name}}, State};
                        undefined ->
                            start_log(Name, Req, From, State)
                    end
            end;
        false when W =:= distr ->
            ok = pg2:create(?group(Name)),
            case get_local_pid(Name) of
                undefined ->
                    start_log(Name, Req, From, State);
                {local, _Pid} ->
                    {{node(),{error, {node_already_open, Name}}}, State};
                {distributed, Pid} ->
                    do_internal_open(Name, Pid, From, Req, true, State)
            end
    end.

-spec open_distr_rpc_fun([node()], _, _) -> % XXX: underspecified
                                fun(() -> no_return()).

open_distr_rpc_fun(Nodes, A, From) ->
    fun() -> open_distr_rpc(Nodes, A, From) end.

%% Spawning a process is a means to avoid deadlock when
%% disk_log_servers mutually open disk_logs.

open_distr_rpc(Nodes, A, From) ->
    {AllReplies, BadNodes} = rpc:multicall(Nodes, ?MODULE, dist_open, [A]),
    {Ok, Bad} = cr(AllReplies, [], []),
    Old = find_old_nodes(Nodes, AllReplies, BadNodes),
    NotOk = [{BadNode, {error, nodedown}} || BadNode <- BadNodes ++ Old],
    Reply = {Ok, Bad ++ NotOk},
    %% Send the reply to the waiting client:
    gen_server:reply(From, Reply),
    exit(normal).

cr([{badrpc, {'EXIT', _}} | T], Nodes, Bad) ->
    %% This clause can be removed in next release.
    cr(T, Nodes, Bad);
cr([R={_Node, {error, _}} | T], Nodes, Bad) ->  
    cr(T, Nodes, [R | Bad]);
cr([Reply | T], Nodes, Bad) ->  
    cr(T, [Reply | Nodes], Bad);
cr([], Nodes, Bad) -> 
    {Nodes, Bad}.

%% If a "new" node (one that calls dist_open/1) tries to open a log
%% on an old node (one that does not have dist_open/1), then the old
%% node is considered 'down'. In next release, this test will not be
%% needed since all nodes can be assumed to be "new" by then.
%% One more thing: if an old node tries to open a log on a new node,
%% the new node is also considered 'down'.
find_old_nodes(Nodes, Replies, BadNodes) ->
    R = [X || {X, _} <- Replies],
    ordsets:to_list(ordsets:subtract(ordsets:from_list(Nodes), 
                                     ordsets:from_list(R ++ BadNodes))).

start_log(Name, Req, From, State) ->
    Server = self(),
    case supervisor:start_child(disk_log_sup, [Server]) of 
	{ok, Pid} ->
            do_internal_open(Name, Pid, From, Req, false, State);
	Error ->
	    {result(Req, Error), State}
    end.
    
do_internal_open(Name, Pid, From, {open, _W, A}=Req, Attach, State) ->
    Server = self(), 
    F = fun() -> 
                Res = disk_log:internal_open(Pid, A),
                Server ! {pending_reply, Pid, Res}
        end,
    _ = spawn(F),
    PD = #pending{log = Name, pid = Pid, req = Req, 
                  from = From, attach = Attach, clients = []},
    P = [PD | State#state.pending],
    {pending, State#state{pending = P}}.

check_pending(Name, From, State, Req) ->
    case lists:keysearch(Name, #pending.log, State#state.pending) of
        {value, #pending{log = Name, clients = Clients}=P} ->
            NP = lists:keyreplace(Name, #pending.log, State#state.pending, 
                               P#pending{clients = Clients++[{Req,From}]}),
            {pending, State#state{pending = NP}};
        false ->
            false
    end.

result({_, distr, _}, R) ->
    {node(), R};
result({_, local, _}, R) ->
    R.

do_close(Pid) ->
    case get(Pid) of
	undefined ->
	    ok;
	Name ->
            erase_log(Name, Pid),
	    unlink(Pid),
	    ok
    end.

erase_log(Name, Pid) ->
    case get_local_pid(Name) of
        undefined ->
            ok;
        {local, Pid} ->
            true = ets:delete(?DISK_LOG_NAME_TABLE, Name),            
            true = ets:delete(?DISK_LOG_PID_TABLE, Pid);
        {distributed, Pid} ->
            true = ets:delete(?DISK_LOG_NAME_TABLE, Name),
            true = ets:delete(?DISK_LOG_PID_TABLE, Pid),
            ok = pg2:leave(?group(Name), Pid)
    end,
    erase(Pid).

do_accessible_logs() ->
    LocalSpec = {'$1','_',local},
    Local0 = [hd(L) || L <- ets:match(?DISK_LOG_NAME_TABLE, LocalSpec)],
    Local = lists:sort(Local0),
    Groups0 = ordsets:from_list(pg2:which_groups()),
    Groups = ordsets:to_list(ordsets:subtract(Groups0, Local)),
    Dist = [L || L <- Groups, dist_pids(L) =/= []],
    {Local, Dist}.

get_local_pid(LogName) ->
    case ets:lookup(?DISK_LOG_NAME_TABLE, LogName) of
	[{LogName, Pid, local}] ->
	    {local, Pid};
        [{LogName, Pid, distr}] ->
            {distributed, Pid};
	[] -> 
            undefined
    end.

%% Inlined.
do_get_log_pids(LogName) ->
    case catch ets:lookup(?DISK_LOG_NAME_TABLE, LogName) of
	[{LogName, Pid, local}] ->
	    {local, Pid};
	[{LogName, _Pid, distr}] ->
            case pg2:get_members(?group(LogName)) of
                [] -> % The disk_log process has died recently
                    undefined;
                Members -> 
                    {distributed, Members}
            end;
        _EmptyOrError ->
	    case dist_pids(LogName) of
		[] -> undefined;
		Pids  -> {distributed, Pids}
	    end
    end.

dist_pids(LogName) ->
    %% Would be much simpler if disk log group names were tagged.
    GroupName = ?group(LogName),
    case catch pg2:get_members(GroupName) of
	[Pid | _] = Pids -> 
            case rpc:call(node(Pid), ?MODULE, get_local_pid, [LogName]) of
                undefined -> % does not seem to be a disk_log group
                    case catch lists:member(Pid,pg2:get_members(GroupName)) of
                        true -> [];
                        _ -> dist_pids(LogName)
                    end;
                _ -> % badrpc if get_local_pid is not exported
                    Pids
            end;
	_ -> 
            []
    end.
