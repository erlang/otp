%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2020. All Rights Reserved.
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
	 get_log_pid/1, all/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, terminate/2]).
-export([handle_cast/2, code_change/3]). % just to avoid compiler warning

-include("disk_log.hrl").

-compile({inline,[{do_get_log_pid,1}]}).

-record(pending, {log, pid, req, from, attach, clients}). % [{Request,From}]

-record(state, {pending = [] :: [#pending{}]}).

%%%-----------------------------------------------------------------
%%% This module implements the disk_log server.  Its primary purpose
%%% is to keep the ets table 'disk_log_names' updated.
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
    gen_server:call(disk_log_server, {open, A}, infinity);
open(Other) ->
    Other.

close(Pid) ->
    gen_server:call(disk_log_server, {close, Pid}, infinity).

get_log_pid(LogName) ->
    do_get_log_pid(LogName).

all() ->
    ensure_started(),
    do_all().

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init([]) ->
    process_flag(trap_exit, true),
    _ = ets:new(?DISK_LOG_NAME_TABLE, [named_table, set]),
    _= ets:new(?DISK_LOG_PID_TABLE, [named_table, set]),
    {ok, #state{}}.

handle_call({open, A}, From, State) ->
    open([{{open, A}, From}], State);
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
                    ets:insert(?DISK_LOG_PID_TABLE, {Pid, Name}),
                    ets:insert(?DISK_LOG_NAME_TABLE, {Name, Pid})
            end,
            gen_server:reply(From, Result0),
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
do_open({open, #arg{name = Name}}=Req, From, State) ->
    case check_pending(Name, From, State, Req) of
        {pending, NewState} -> 
            {pending, NewState};
        false ->
            case do_get_log_pid(Name) of
                undefined ->
                    start_log(Name, Req, From, State);
                Pid ->
                    do_internal_open(Name, Pid, From, Req, true,State)
            end
    end.

start_log(Name, Req, From, State) ->
    Server = self(),
    case supervisor:start_child(disk_log_sup, [Server]) of 
	{ok, Pid} ->
            do_internal_open(Name, Pid, From, Req, false, State);
	Error ->
	    {Error, State}
    end.
    
do_internal_open(Name, Pid, From, {open, A}=Req, Attach, State) ->
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
    case do_get_log_pid(Name) of
        undefined ->
            ok;
        Pid ->
            true = ets:delete(?DISK_LOG_NAME_TABLE, Name),            
            true = ets:delete(?DISK_LOG_PID_TABLE, Pid)
    end,
    erase(Pid).

do_all() ->
    LocalSpec = {'$1','_'},
    Local0 = [hd(L) || L <- ets:match(?DISK_LOG_NAME_TABLE, LocalSpec)],
    lists:sort(Local0).

%% Inlined.
do_get_log_pid(LogName) ->
    try ets:lookup(?DISK_LOG_NAME_TABLE, LogName) of
	[{LogName, Pid}] ->
	    Pid;
        [] ->
            undefined
    catch
        _:_ ->
            undefined
    end.
