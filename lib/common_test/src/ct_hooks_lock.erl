%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2018. All Rights Reserved.
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

init(Id) ->
    ct_util:mark_process(),
    {ok, #state{ id = Id }}.

handle_call({stop,Id}, _From, #state{ id = Id, requests = Reqs } = State) ->
    _ = [gen_server:reply(Req, locker_stopped) || {Req,_ReqId} <- Reqs],
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
    
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN',Ref,process,Pid,_},
	    #state{ locked = {true, Pid, Ref},
		    requests = [{NextFrom,NextPid}|Rest] } = State) ->
    gen_server:reply(NextFrom, locked),
    NextRef = monitor(process, NextPid),
    {noreply,State#state{ locked = {true, NextPid, NextRef},
			  requests = Rest } }.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -------------------------------------------------------------------------
%% Internal Functions
%% -------------------------------------------------------------------------
