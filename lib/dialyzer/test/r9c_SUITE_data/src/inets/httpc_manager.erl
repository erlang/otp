%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Mobile Arts AB
%% Portions created by Mobile Arts are Copyright 2002, Mobile Arts AB
%% All Rights Reserved.''
%%
%% Created : 18 Dec 2001 by Johan Blom <johan.blom@mobilearts.se>
%%

-module(httpc_manager).

-behaviour(gen_server).

-include("http.hrl").

-define(HMACALL, ?MODULE).
-define(HMANAME, ?MODULE).

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0,start/0,
	 request/1,cancel_request/1,
	 next_request/2,
	 register_socket/3,
	 abort_session/3,close_session/2,close_session/3
	]).

%% Debugging only
-export([status/0]).

%% gen_server callbacks
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,
	 code_change/3]).

%%% address_db - ets() Contains mappings from a tuple {Host,Port} to a tuple
%%%  {LastSID,OpenSessions,ets()} where
%%%      LastSid is the last allocated session id,
%%%      OpenSessions is the number of currently open sessions and
%%%      ets() contains mappings from Session Id to #session{}.
%%%
%%% Note:
%%% - Only persistent connections are stored in address_db
%%% - When automatically redirecting, multiple requests are performed.
-record(state,{
	  address_db,   % ets()
	  reqid         % int() Next Request id to use (identifies request).
	 }).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    ensure_started().

start_link() ->
    gen_server:start_link({local,?HMACALL}, ?HMANAME, [], []).


%% Find available session process and store in address_db. If no
%% available, start new handler process.
request(Req) ->
    ensure_started(),
    ClientClose=http_lib:connection_close(Req#request.headers),
    gen_server:call(?HMACALL,{request,ClientClose,Req},infinity).

cancel_request(ReqId) ->
    gen_server:call(?HMACALL,{cancel_request,ReqId},infinity).


%%% Close Session
close_session(Addr,Sid) ->
    gen_server:call(?HMACALL,{close_session,Addr,Sid},infinity).
close_session(Req,Addr,Sid) ->
    gen_server:call(?HMACALL,{close_session,Req,Addr,Sid},infinity).

abort_session(Addr,Sid,Msg) ->
    gen_server:call(?HMACALL,{abort_session,Addr,Sid,Msg},infinity).


%%%  Pick next in request que
next_request(Addr,Sid) ->
    gen_server:call(?HMACALL,{next_request,Addr,Sid},infinity).

%%% Session handler has succeed to set up a new session, now register
%%% the socket
register_socket(Addr,Sid,Socket) ->
    gen_server:cast(?HMACALL,{register_socket,Addr,Sid,Socket}).


%%% Debugging
status() ->
    gen_server:cast(?HMACALL,status).


%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok,#state{address_db=ets:new(address_db,[private]),
	       reqid=0}}.


%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
%%% Note:
%%% - We may have multiple non-persistent connections, each will be handled in
%%%   separate processes, thus don't add such connections to address_db
handle_call({request,false,Req},_From,State) ->
    case ets:lookup(State#state.address_db,Req#request.address) of
	[] ->
	    STab=ets:new(session_db,[private,{keypos,2},set]),
	    case persistent_new_session_request(0,Req,STab,State) of
		{Reply,LastSid,State2} ->
		    ets:insert(State2#state.address_db,
			       {Req#request.address,{LastSid,1,STab}}),
		    {reply,Reply,State2};
		{ErrorReply,State2} ->
		    {reply,ErrorReply,State2}
	    end;
	[{_,{LastSid,OpenS,STab}}] ->
	    case lookup_session_entry(STab) of
		{ok,Session} ->
		    old_session_request(Session,Req,STab,State);
		need_new_session when OpenS<(Req#request.settings)#client_settings.max_sessions ->
		    case persistent_new_session_request(LastSid,Req,
							STab,State) of
			{Reply,LastSid2,State2} ->
			    ets:insert(State2#state.address_db,
				       {Req#request.address,
					{LastSid2,OpenS+1,STab}}),
			    {reply,Reply,State2};
			{ErrorReply,State2} ->
			    {reply,ErrorReply,State2}
		    end;
		need_new_session ->
		    {reply,{error,too_many_sessions},State}
	    end
    end;
handle_call({request,true,Req},_From,State) ->
    {Reply,State2}=not_persistent_new_session_request(Req,State),
    {reply,Reply,State2};
handle_call({cancel_request,true,_ReqId},_From,State) ->
%% FIXME Should be possible to scan through all requests made, but perhaps
%% better to give some more hints (such as Addr etc)
    Reply=ok,
    {reply,Reply,State};
handle_call({next_request,Addr,Sid},_From,State) ->
    case ets:lookup(State#state.address_db,Addr) of
	[] ->
	    {reply,{error,no_connection},State};
	[{_,{_,_,STab}}] ->
	    case ets:lookup(STab,Sid) of
		[] ->
		    {reply,{error,session_not_registered},State};
		[S=#session{pipeline=[],quelength=QueLen}] ->
		    if
			QueLen==1 ->
			    ets:insert(STab,S#session{quelength=0});
			true ->
			    ok
		    end,
		    {reply,no_more_requests,State};
		[S=#session{pipeline=Que}] ->
		    [Req|RevQue]=lists:reverse(Que),
		    ets:insert(STab,S#session{pipeline=lists:reverse(RevQue),
					      quelength=S#session.quelength-1}),
		    {reply,Req,State}
	    end
    end;
handle_call({close_session,Addr,Sid},_From,State) ->
    case ets:lookup(State#state.address_db,Addr) of
	[] ->
	    {reply,{error,no_connection},State};
	[{_,{LastSid,OpenS,STab}}] ->
	    case ets:lookup(STab,Sid) of
		[#session{pipeline=Que}] ->
		    R=handle_close_session(lists:reverse(Que),STab,Sid,State),
		    ets:insert(State#state.address_db,
			       {Addr,{LastSid,OpenS-1,STab}}),
		    {reply,R,State};
		[] ->
		    {reply,{error,session_not_registered},State}
	    end
    end;
handle_call({close_session,Req,Addr,Sid},_From,State) ->
    case ets:lookup(State#state.address_db,Addr) of
	[] ->
	    {reply,{error,no_connection},State};
	[{_,{LastSid,OpenS,STab}}] ->
	    case ets:lookup(STab,Sid) of
		[#session{pipeline=Que}] ->
		    R=handle_close_session([Req|lists:reverse(Que)],
					   STab,Sid,State),
		    ets:insert(State#state.address_db,
			       {Addr,{LastSid,OpenS-1,STab}}),
		    {reply,R,State};
		[] ->
		    {reply,{error,session_not_registered},State}
	    end
    end;
handle_call({abort_session,Addr,Sid,Msg},_From,State) ->
    case ets:lookup(State#state.address_db,Addr) of
	[] ->
	    {reply,{error,no_connection},State};
	[{_,{LastSid,OpenS,STab}}] ->
	    case ets:lookup(STab,Sid) of
		[#session{pipeline=Que}] ->
		    R=abort_request_que(Que,{error,Msg}),
		    ets:delete(STab,Sid),
		    ets:insert(State#state.address_db,
			       {Addr,{LastSid,OpenS-1,STab}}),
		    {reply,R,State};
		[] ->
		    {reply,{error,session_not_registered},State}
	    end
    end.


%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(status, State) ->
    io:format("Status:~n"),
    print_all(lists:sort(ets:tab2list(State#state.address_db))),
    {noreply, State};
handle_cast({register_socket,Addr,Sid,Socket},State) ->
    case ets:lookup(State#state.address_db,Addr) of
	[] ->
	    {noreply,State};
	[{_,{_,_,STab}}] ->
	    case ets:lookup(STab,Sid) of
		[Session] ->
		    ets:insert(STab,Session#session{socket=Socket}),
		    {noreply,State};
		[] ->
		    {noreply,State}
	    end
    end.

print_all([]) ->
    ok;
print_all([{Addr,{LastSid,OpenSessions,STab}}|Rest]) ->
    io:format(" Address:~p LastSid=~p OpenSessions=~p~n",[Addr,LastSid,OpenSessions]),
    SortedList=lists:sort(fun(A,B) ->
				  if
				      A#session.id<B#session.id ->
					  true;
				      true ->
					  false
				  end
			  end,ets:tab2list(STab)),
    print_all2(SortedList),
    print_all(Rest).

print_all2([]) ->
    ok;
print_all2([Session|Rest]) ->
    io:format("   Session:~p~n",[Session#session.id]),
    io:format("     Client close:~p~n",[Session#session.clientclose]),
    io:format("     Socket:~p~n",[Session#session.socket]),
    io:format("     Pipe: length=~p Que=~p~n",[Session#session.quelength,Session#session.pipeline]),
    print_all2(Rest).

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({'EXIT',_Pid,normal}, State) ->
    {noreply, State};
handle_info(Info, State) ->
    io:format("ERROR httpc_manager:handle_info ~p~n",[Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    ets:delete(State#state.address_db).

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%% From RFC 2616, Section 8.1.4
%%%   A client, server, or proxy MAY close the transport connection at any
%%%   time. For example, a client might have started to send a new request
%%%   at the same time that the server has decided to close the "idle"
%%%   connection. From the server's point of view, the connection is being
%%%   closed while it was idle, but from the client's point of view, a
%%%   request is in progress.
%%%
%%%   This means that clients, servers, and proxies MUST be able to recover
%%%   from asynchronous close events. Client software SHOULD reopen the
%%%   transport connection and retransmit the aborted sequence of requests
%%%   without user interaction so long as the request sequence is
%%%   idempotent (see section 9.1.2). Non-idempotent methods or sequences
%%%
%%% FIXME
%%% Note:
%%% - If this happen (server close because of idle) there can't be any requests
%%%   in the que.
%%% - This is the main function for closing of sessions
handle_close_session([],STab,Sid,_State) ->
    ets:delete(STab,Sid);
handle_close_session(Que,STab,Sid,_State) ->
    ets:delete(STab,Sid),
    abort_request_que(Que,{error,aborted_request}).


%%% From RFC 2616, Section 8.1.2.2
%%%   Clients which assume persistent connections and pipeline immediately
%%%   after connection establishment SHOULD be prepared to retry their
%%%   connection if the first pipelined attempt fails. If a client does
%%%   such a retry, it MUST NOT pipeline before it knows the connection is
%%%   persistent. Clients MUST also be prepared to resend their requests if
%%%   the server closes the connection before sending all of the
%%%   corresponding responses.
%%% FIXME! I'm currently not checking if tis is the first attempt on the session
%%% FIXME! Pipeline size must be dynamically variable (e.g. 0 if resend, 2 else)
%%% The que contains requests that have been sent ok previously, but the session
%%% was closed prematurely when reading the response.
%%% Try setup a new session and resend these requests.
%%% Note:
%%% - This MUST be a persistent session
% handle_closed_pipelined_session_que([],_State) ->
%     ok;
% handle_closed_pipelined_session_que(_Que,_State) ->
%     ok.


%%% From RFC 2616, Section 8.2.4
%%%   If an HTTP/1.1 client sends a request which includes a request body,
%%%   but which does not include an Expect request-header field with the
%%%   "100-continue" expectation, and if the client is not directly
%%%   connected to an HTTP/1.1 origin server, and if the client sees the
%%%   connection close before receiving any status from the server, the
%%%   client SHOULD retry the request.  If the client does retry this
%%%   request, it MAY use the following "binary exponential backoff"
%%%   algorithm to be assured of obtaining a reliable response:
%%%   ...
%%% FIXME! I'm currently not checking if a "Expect: 100-continue" has been sent.
% handle_remotely_closed_session_que([],_State) ->
%     ok;
% handle_remotely_closed_session_que(_Que,_State) ->
% %    resend_que(Que,Socket),
%     ok.

%%% Resend all requests in the request que
% resend_que([],_) ->
%     ok;
% resend_que([Req|Que],Socket) ->
%     case catch httpc_handler:http_request(Req,Socket) of
% 	ok ->
% 	    resend_que(Que,Socket);
% 	{error,Reason} ->
% 	    {error,Reason}
%     end.


%%% From RFC 2616,
%%% Section 8.1.2.2:
%%%   Clients SHOULD NOT pipeline requests using non-idempotent methods or
%%%   non-idempotent sequences of methods (see section 9.1.2). Otherwise, a
%%%   premature termination of the transport connection could lead to
%%%   indeterminate results. A client wishing to send a non-idempotent
%%%   request SHOULD wait to send that request until it has received the
%%%   response status for the previous request.
%%% Section 9.1.2:
%%%   Methods can also have the property of "idempotence" in that (aside
%%%   from error or expiration issues) the side-effects of N > 0 identical
%%%   requests is the same as for a single request. The methods GET, HEAD,
%%%   PUT and DELETE share this property. Also, the methods OPTIONS and
%%%   TRACE SHOULD NOT have side effects, and so are inherently idempotent.
%%%
%%% Note that POST and CONNECT are idempotent methods.
%%%
%%% Tries to find an open, free session i STab. Such a session has quelength
%%% less than ?MAX_PIPELINE_LENGTH
%%% Don't care about non-standard, user defined methods.
%%%
%%% Returns {ok,Session} or need_new_session where
%%%  Session is the session that may be used
lookup_session_entry(STab) ->
    MS=[{#session{quelength='$1',max_quelength='$2',
		  id='_',clientclose='_',socket='$3',scheme='_',pipeline='_'},
	 [{'<','$1','$2'},{is_port,'$3'}],
	 ['$_']}],
    case ets:select(STab,MS) of
	[] ->
	    need_new_session;
	SessionList -> % Now check if any of these has an empty pipeline.
	    case lists:keysearch(0,2,SessionList) of
		{value,Session} ->
		    {ok,Session};
		false ->
		    {ok,hd(SessionList)}
	    end
    end.


%%% Returns a tuple {Reply,State} where
%%%  Reply is the response sent back to the application
%%%
%%% Note:
%%% - An {error,einval} from a send should sometimes rather be {error,closed}
%%% - Don't close the session from here, let httpc_handler take care of that.
%old_session_request(Session,Req,STab,State)
%  when (Req#request.settings)#client_settings.max_quelength==0 ->
%    Session1=Session#session{pipeline=[Req]},
%    ets:insert(STab,Session1),
%    {reply,{ok,ReqId},State#state{reqid=ReqId+1}};
old_session_request(Session,Req,STab,State) ->
    ReqId=State#state.reqid,
    Req1=Req#request{id=ReqId},
    case catch httpc_handler:http_request(Req1,Session#session.socket) of
	ok ->
	    Session1=Session#session{pipeline=[Req1|Session#session.pipeline],
				     quelength=Session#session.quelength+1},
	    ets:insert(STab,Session1),
	    {reply,{ok,ReqId},State#state{reqid=ReqId+1}};
	{error,Reason} ->
	    ets:insert(STab,Session#session{socket=undefined}),
%	    http_lib:close(Session#session.sockettype,Session#session.socket),
	    {reply,{error,Reason},State#state{reqid=ReqId+1}}
    end.

%%% Returns atuple {Reply,Sid,State} where
%%%  Reply is the response sent back to the application, and
%%%  Sid is the last used Session Id
persistent_new_session_request(Sid,Req,STab,State) ->
    ReqId=State#state.reqid,
    case setup_new_session(Req#request{id=ReqId},false,Sid) of
	{error,Reason} ->
	    {{error,Reason},State#state{reqid=ReqId+1}};
	{NewSid,Session} ->
	    ets:insert(STab,Session),
	    {{ok,ReqId},NewSid,State#state{reqid=ReqId+1}}
    end.

%%% Returns a tuple {Reply,State} where
%%%  Reply is the response sent back to the application
not_persistent_new_session_request(Req,State) ->
    ReqId=State#state.reqid,
    case setup_new_session(Req#request{id=ReqId},true,undefined) of
	{error,Reason} ->
	    {{error,Reason},State#state{reqid=ReqId+1}};
	ok ->
	    {{ok,ReqId},State#state{reqid=ReqId+1}}
    end.

%%% As there are no sessions available, setup a new session and send the request
%%% on it.
setup_new_session(Req,ClientClose,Sid) ->
    S=#session{id=Sid,clientclose=ClientClose,
	       scheme=Req#request.scheme,
	       max_quelength=(Req#request.settings)#client_settings.max_quelength},
    spawn_link(httpc_handler,init_connection,[Req,S]),
    case ClientClose of
	false ->
	    {Sid+1,S};
	true ->
	    ok
    end.


%%% ----------------------------------------------------------------------------
%%% Abort all requests in the request que.
abort_request_que([],_Msg) ->
    ok;
abort_request_que([#request{from=From,ref=Ref,id=Id}|Que],Msg) ->
    gen_server:cast(From,{Ref,Id,Msg}),
    abort_request_que(Que,Msg);
abort_request_que(#request{from=From,ref=Ref,id=Id},Msg) ->
    gen_server:cast(From,{Ref,Id,Msg}).


%%% --------------------------------
%            C={httpc_manager,{?MODULE,start_link,[]},permanent,1000,
%	       worker,[?MODULE]},
%            supervisor:start_child(inets_sup, C),
ensure_started() ->
    case whereis(?HMANAME) of
        undefined ->
	    start_link();
        _ ->
	    ok
    end.


%%% ============================================================================
%%% This is deprecated code, to be removed

% format_time() ->
%     {_,_,MicroSecs}=TS=now(),
%     {{Y,Mon,D},{H,M,S}}=calendar:now_to_universal_time(TS),
%     lists:flatten(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w,~2.2.0w:~2.2.0w:~6.3.0f",
% 				[Y,Mon,D,H,M,S+(MicroSecs/1000000)])).
