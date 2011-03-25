%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2011. All Rights Reserved.
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

%%% @doc Common Test Framework test execution control module.
%%%
%%% <p>This module is a proxy for calling and handling locks in 
%%%    common test hooks.</p>

-module(ct_hooks_lock).

-behaviour(gen_server).

%% API
-export([start/1, stop/1, request/0, release/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, { id, locked = false, requests = [] }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the server
start(Id) ->
    case gen_server:start({local, ?SERVER}, ?MODULE, Id, []) of
	{error,{already_started, Pid}} ->
	    {ok,Pid};
	Else ->
	    Else
    end.

stop(Id) ->
    try
	gen_server:call(?SERVER, {stop,Id})
    catch exit:{noproc,_} ->
	    stopped
    end.
    
request() ->
    try
	gen_server:call(?SERVER,{request,self()},infinity)
    catch exit:{noproc,_} ->
	    locked
    end.

release() ->
    try
	gen_server:call(?SERVER,{release,self()})
    catch exit:{noproc,_} ->
	    unlocked
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @doc Initiates the server
init(Id) ->
    {ok, #state{ id = Id }}.

%% @doc Handling call messages
handle_call({stop,Id}, _From, #state{ id = Id, requests = Reqs } = State) ->
    [gen_server:reply(Req, locker_stopped) || {Req,_ReqId} <- Reqs],
    {stop, normal, stopped, State};
handle_call({stop,_Id}, _From, State) ->
    {reply, stopped, State};
handle_call({request, Pid}, _From, #state{ locked = false,
					  requests = [] } = State) ->
    Ref = monitor(process, Pid),
    {reply, locked, State#state{ locked = {true, Pid, Ref}} };
handle_call({request, Pid}, From, #state{ requests = Reqs } = State) ->
    {noreply, State#state{ requests = Reqs ++ [{From,Pid}] }};
handle_call({release, Pid}, _From, #state{ locked = {true, Pid, Ref},
					  requests = []} = State) ->
    demonitor(Ref,[flush]),
    {reply, unlocked, State#state{ locked = false }};
handle_call({release, Pid}, _From,
	    #state{ locked = {true, Pid, Ref},
		    requests = [{NextFrom,NextPid}|Rest]} = State) ->
    demonitor(Ref,[flush]),
    gen_server:reply(NextFrom,locked),
    NextRef = monitor(process, NextPid),
    {reply,unlocked,State#state{ locked = {true, NextPid, NextRef},
				 requests = Rest } };
handle_call({release, _Pid}, _From, State) ->
    {reply, not_locked, State}.
    
%% @doc Handling cast messages
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handling all non call/cast messages
handle_info({'DOWN',Ref,process,Pid,_},
	    #state{ locked = {true, Pid, Ref},
		    requests = [{NextFrom,NextPid}|Rest] } = State) ->
    gen_server:reply(NextFrom, locked),
    NextRef = monitor(process, NextPid),
    {noreply,State#state{ locked = {true, NextPid, NextRef},
			  requests = Rest } }.

%% @doc This function is called by a gen_server when it is about to terminate. 
terminate(_Reason, _State) ->
    ok.

%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -------------------------------------------------------------------------
%% Internal Functions
%% -------------------------------------------------------------------------
