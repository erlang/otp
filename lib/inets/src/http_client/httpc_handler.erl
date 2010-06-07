%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2010. All Rights Reserved.
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
%%

-module(httpc_handler).

-behaviour(gen_server).

-include_lib("inets/src/http_lib/http_internal.hrl").
-include("httpc_internal.hrl").


%%--------------------------------------------------------------------
%% Internal Application API
-export([
	 start_link/2, 
	 connect_and_send/2, 
	 send/2, 
	 cancel/2, 
	 stream/3, 
	 stream_next/1,
	 info/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(timers, 
	{
	  request_timers = [], % [ref()]
	  queue_timer          % ref()
	 }).

-record(state, 
	{
	  request,                   % #request{}
	  session,                   % #tcp_session{} 
	  status_line,               % {Version, StatusCode, ReasonPharse}
	  headers,                   % #http_response_h{}
	  body,                      % binary()
	  mfa,                       % {Moduel, Function, Args}
	  pipeline = queue:new(),    % queue() 
	  keep_alive = queue:new(),  % queue() 
	  status,   % undefined | new | pipeline | keep_alive | close | ssl_tunnel
	  canceled = [],	     % [RequestId]
	  max_header_size = nolimit, % nolimit | integer() 
	  max_body_size = nolimit,   % nolimit | integer()
	  options,                   % #options{}
	  timers = #timers{},        % #timers{}
	  profile_name,              % atom() - id of httpc_manager process.
	  once                       % send | undefined 
	 }).


%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link(Request, Options, ProfileName) -> {ok, Pid}
%%
%%	Request = #request{}
%%	Options =  #options{} 
%%      ProfileName = atom() - id of httpc manager process
%%
%% Description: Starts a http-request handler process. Intended to be
%% called by the httpc profile supervisor or the http manager process
%% if the client is started stand alone form inets.
%%
%% Note: Uses proc_lib and gen_server:enter_loop so that waiting
%% for gen_tcp:connect to timeout in init/1 will not
%% block the httpc manager process in odd cases such as trying to call
%% a server that does not exist. (See OTP-6735) The only API function
%% sending messages to the handler process that can be called before
%% init has compleated is cancel and that is not a problem! (Send and
%% stream will not be called before the first request has been sent and
%% the reply or part of it has arrived.)
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------

start_link(Options, ProfileName) ->
    Args = [Options, ProfileName], 
    gen_server:start_link(?MODULE, Args, []).

connect_and_send(Request, HandlerPid) ->
    call({connect_and_send, Request}, HandlerPid).


%%--------------------------------------------------------------------
%% Function: send(Request, Pid) -> ok 
%%	Request = #request{}
%%      Pid = pid() - the pid of the http-request handler process.
%%
%% Description: Uses this handlers session to send a request. Intended
%% to be called by the httpc manager process.
%%--------------------------------------------------------------------
send(Request, Pid) ->
    call(Request, Pid, 5000).


%%--------------------------------------------------------------------
%% Function: cancel(RequestId, Pid) -> ok
%%	RequestId = ref()
%%      Pid = pid() -  the pid of the http-request handler process.
%%
%% Description: Cancels a request. Intended to be called by the httpc
%% manager process.
%%--------------------------------------------------------------------
cancel(RequestId, Pid) ->
    cast({cancel, RequestId}, Pid).


%%--------------------------------------------------------------------
%% Function: stream_next(Pid) -> ok
%%      Pid = pid() -  the pid of the http-request handler process.
%%
%% Description: Works as inets:setopts(active, once) but for
%% body chunks sent to the user.
%%--------------------------------------------------------------------
stream_next(Pid) ->
    cast(stream_next, Pid).


%%--------------------------------------------------------------------
%% Function: info(Pid) -> [{Key, Val}]
%%      Pid = pid() -  the pid of the http-request handler process.
%%
%% Description: 
%%     Returns various information related to this handler
%%     Used for debugging and testing
%%--------------------------------------------------------------------
info(Pid) ->
    call(info, Pid).


%%--------------------------------------------------------------------
%% Function: stream(BodyPart, Request, Code) -> _
%%	BodyPart = binary()
%%      Request = #request{}
%%      Code = integer()
%%
%% Description: Stream the HTTP body to the caller process (client) 
%%              or to a file. Note that the data that has been stream
%%              does not have to be saved. (We do not want to use up
%%              memory in vain.)
%%--------------------------------------------------------------------
%% Request should not be streamed
stream(BodyPart, Request = #request{stream = none}, _) ->
    ?hcrt("stream - none", []),
    {BodyPart, Request};

%% Stream to caller
stream(BodyPart, Request = #request{stream = Self}, Code) 
  when ((Code =:= 200) orelse  (Code =:= 206)) andalso 
       ((Self =:= self) orelse (Self =:= {self, once})) ->
    ?hcrt("stream - self", [{stream, Self}, {code, Code}]),
    httpc_response:send(Request#request.from, 
			{Request#request.id, stream, BodyPart}),
    {<<>>, Request};

stream(BodyPart, Request = #request{stream = Self}, 404) 
  when (Self =:= self) orelse (Self =:= {self, once}) ->
    ?hcrt("stream - self with 404", [{stream, Self}]),
    httpc_response:send(Request#request.from, 
			{Request#request.id, stream, BodyPart}),
    {<<>>, Request};

%% Stream to file
%% This has been moved to start_stream/3
%% We keep this for backward compatibillity...
stream(BodyPart, Request = #request{stream = Filename}, Code)
  when ((Code =:= 200) orelse (Code =:= 206)) andalso is_list(Filename) -> 
    ?hcrt("stream - filename", [{stream, Filename}, {code, Code}]),
    case file:open(Filename, [write, raw, append, delayed_write]) of
	{ok, Fd} ->
	    ?hcrt("stream - file open ok", [{fd, Fd}]),
	    stream(BodyPart, Request#request{stream = Fd}, 200);
	{error, Reason} ->
	    exit({stream_to_file_failed, Reason})
    end;

%% Stream to file
stream(BodyPart, Request = #request{stream = Fd}, Code)  
  when ((Code =:= 200) orelse (Code =:= 206)) -> 
    ?hcrt("stream to file", [{stream, Fd}, {code, Code}]),
    case file:write(Fd, BodyPart) of
	ok ->
	    {<<>>, Request};
	{error, Reason} ->
	    exit({stream_to_file_failed, Reason})
    end;

stream(BodyPart, Request,_) -> % only 200 and 206 responses can be streamed
    ?hcrt("stream - ignore", [{request, Request}]),
    {BodyPart, Request}.


%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([Options, ProfileName]) -> {ok, State} | 
%%                       {ok, State, Timeout} | ignore | {stop, Reason}
%%
%%	Options =  #options{} 
%%      ProfileName = atom() - id of httpc manager process
%%
%% Description: Initiates the httpc_handler process 
%%
%% Note: The init function may not fail, that will kill the
%% httpc_manager process. We could make the httpc_manager more comlex
%% but we do not want that so errors will be handled by the process
%% sending an init_error message to itself.
%%--------------------------------------------------------------------
init([Options, ProfileName]) ->
    ?hcrv("init - starting", [{options, Options}, {profile, ProfileName}]),
    process_flag(trap_exit, true),
    handle_verbose(Options#options.verbose),
    State = #state{status       = undefined,
		   options      = Options, 
		   profile_name = ProfileName},
    ?hcrd("init - started", []),
    {ok, State}.


%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% Description: Handling call messages
%%--------------------------------------------------------------------


%% This is the first request, the reason the proc was started
handle_call({connect_and_send, #request{address = Address0,
					scheme  = Scheme} = Request}, 
	    _From, 
	    #state{options = #options{proxy = Proxy}, 
		   status  = undefined,
		   session = undefined} = State) ->
    ?hcrv("connect and send", [{address0, Address0}, {proxy, Proxy}]),
    Address = handle_proxy(Address0, Proxy),
    if
	((Address =/= Address0) andalso (Scheme  =:= https)) ->
	    %% This is what we should do if and when ssl supports 
	    %% "socket upgrading"
	    %%send_ssl_tunnel_request(Address, Request,
	    %%		    #state{options = Options,
	    %%		   status = ssl_tunnel});
	    Reason = {failed_connecting, 
		      https_through_proxy_is_not_currently_supported},
	    %% Send a reply to the original caller
	    ErrorResponse = httpc_response:error(Request, Reason), 
	    httpc_response:send(Request#request.from, ErrorResponse),
	    %% Reply to the manager
	    ErrorReply    = {error, Reason},
	    {stop, normal, ErrorReply, State};
	true ->
	    case connect_and_send_first_request(Address, Request, State) of
		{ok, NewState} ->
		    {reply, ok, NewState};
		{stop, Error, NewState} ->
		    {stop, normal, Error, NewState}
	    end
    end;
	
handle_call(#request{address = Addr} = Request, _, 
	    #state{status  = Status,
		   session = #session{type = pipeline} = Session,
		   timers  = Timers,
		   options = #options{proxy = Proxy} = _Options, 
		   profile_name = ProfileName} = State) 
  when Status =/= undefined ->

    ?hcrv("new request on a pipeline session", 
	  [{request, Request}, 
	   {profile, ProfileName}, 
	   {status,  Status}, 
	   {timers,  Timers}]),

    Address = handle_proxy(Addr, Proxy),

    case httpc_request:send(Address, Session, Request) of
        ok ->

	    ?hcrd("request sent", []),

	    %% Activate the request time out for the new request
	    NewState = 
		activate_request_timeout(State#state{request = Request}),

	    ClientClose = 
		httpc_request:is_client_closing(Request#request.headers),

            case State#state.request of
                #request{} -> %% Old request not yet finished
		    ?hcrd("old request still not finished", []),
		    %% Make sure to use the new value of timers in state
		    NewTimers   = NewState#state.timers,
                    NewPipeline = queue:in(Request, State#state.pipeline),
		    NewSession  = 
			Session#session{queue_length = 
					%% Queue + current
					queue:len(NewPipeline) + 1,
					client_close = ClientClose},
		    httpc_manager:insert_session(NewSession, ProfileName),
		    ?hcrd("session updated", []),
                    {reply, ok, State#state{pipeline = NewPipeline,
					    session  = NewSession,
					    timers   = NewTimers}};
		undefined ->
		    %% Note: tcp-message receiving has already been
		    %% activated by handle_pipeline/2. 
		    ?hcrd("no current request", []),
		    cancel_timer(Timers#timers.queue_timer, 
				 timeout_queue),
		    NewSession = 
			Session#session{queue_length = 1,
					client_close = ClientClose},
		    httpc_manager:insert_session(NewSession, ProfileName),
		    Relaxed = 
			(Request#request.settings)#http_options.relaxed, 
		    MFA = {httpc_response, parse, 
			   [State#state.max_header_size, Relaxed]}, 
		    NewTimers = Timers#timers{queue_timer = undefined}, 
		    ?hcrd("session created", []),
		    {reply, ok, NewState#state{request = Request,
					       session = NewSession,
					       mfa     = MFA,
					       timers  = NewTimers}}
	    end;
	{error, Reason} ->
	    ?hcri("failed sending request", [{reason, Reason}]),
	    {reply, {pipeline_failed, Reason}, State}
    end;

handle_call(#request{address = Addr} = Request, _, 
	    #state{status  = Status,
		   session = #session{type = keep_alive} = Session,
		   timers  = Timers,
		   options = #options{proxy = Proxy} = _Options,
		   profile_name = ProfileName} = State) 
  when Status =/= undefined ->
    
    ?hcrv("new request on a keep-alive session", 
	  [{request, Request}, 
	   {profile, ProfileName}, 
	   {status,  Status}]),

    Address = handle_proxy(Addr, Proxy),
    case httpc_request:send(Address, Session, Request) of
	ok ->

	    ?hcrd("request sent", []),

	    %% Activate the request time out for the new request
	    NewState = 
		activate_request_timeout(State#state{request = Request}),

	    ClientClose = 
		httpc_request:is_client_closing(Request#request.headers),

	    case State#state.request of
		#request{} -> %% Old request not yet finished
		    %% Make sure to use the new value of timers in state
		    ?hcrd("old request still not finished", []),
		    NewTimers    = NewState#state.timers,
                    NewKeepAlive = queue:in(Request, State#state.keep_alive),
		    NewSession   = 
			Session#session{queue_length = 
					%% Queue + current
					queue:len(NewKeepAlive) + 1,
					client_close = ClientClose},
		    httpc_manager:insert_session(NewSession, ProfileName),
		    ?hcrd("session updated", []),
                    {reply, ok, State#state{keep_alive = NewKeepAlive,
					    session    = NewSession,
					    timers     = NewTimers}};
		undefined ->
		    %% Note: tcp-message reciving has already been
		    %% activated by handle_pipeline/2. 
		    ?hcrd("no current request", []),
		    cancel_timer(Timers#timers.queue_timer, 
				 timeout_queue),
		    NewSession = 
			Session#session{queue_length = 1,
					client_close = ClientClose},
		    httpc_manager:insert_session(NewSession, ProfileName),
		    Relaxed = 
			(Request#request.settings)#http_options.relaxed,
		    MFA = {httpc_response, parse,
			   [State#state.max_header_size, Relaxed]}, 
		    {reply, ok, NewState#state{request = Request,
					       session = NewSession, 
					       mfa     = MFA}}
	    end;

	{error, Reason} ->
	    ?hcri("failed sending request", [{reason, Reason}]),
	    {reply, {request_failed, Reason}, State}
    end;


handle_call(info, _, State) ->
    Info = handler_info(State), 
    {reply, Info, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% Description: Handling cast messages
%%--------------------------------------------------------------------

%% When the request in process has been canceled the handler process is
%% stopped and the pipelined requests will be reissued or remaining
%% requests will be sent on a new connection. This is is
%% based on the assumption that it is proably cheaper to reissue the
%% requests than to wait for a potentiall large response that we then
%% only throw away. This of course is not always true maybe we could
%% do something smarter here?! If the request canceled is not
%% the one handled right now the same effect will take place in
%% handle_pipeline/2 when the canceled request is on turn, 
%% handle_keep_alive_queue/2 on the other hand will just skip the
%% request as if it was never issued as in this case the request will
%% not have been sent. 
handle_cast({cancel, RequestId}, 
	    #state{request      = #request{id = RequestId} = Request,
		   profile_name = ProfileName,
		   canceled     = Canceled} = State) ->
    ?hcrv("cancel current request", [{request_id, RequestId}, 
				     {profile,    ProfileName},
				     {canceled,   Canceled}]),
    httpc_manager:request_canceled(RequestId, ProfileName),
    ?hcrv("canceled", []),
    {stop, normal, 
     State#state{canceled = [RequestId | Canceled],
		 request  = Request#request{from = answer_sent}}};
handle_cast({cancel, RequestId}, 
	    #state{profile_name = ProfileName,
		   canceled     = Canceled} = State) ->
    ?hcrv("cancel", [{request_id, RequestId}, 
		     {profile, ProfileName},
		     {canceled,   Canceled}]),
    httpc_manager:request_canceled(RequestId, ProfileName),
    ?hcrv("canceled", []),
    {noreply, State#state{canceled = [RequestId | Canceled]}};

handle_cast(stream_next, #state{session = Session} = State) ->
    activate_once(Session), 
    {noreply, State#state{once = once}}.


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Proto, _Socket, Data}, 
	    #state{mfa = {Module, Function, Args}, 
		   request = #request{method = Method, 
				      stream = Stream} = Request, 
		   session = Session, 
		   status_line = StatusLine} = State) 
  when (Proto =:= tcp) orelse 
       (Proto =:= ssl) orelse 
       (Proto =:= httpc_handler) ->

    ?hcri("received data", [{proto,       Proto}, 
			    {module,      Module}, 
			    {function,    Function}, 
			    {method,      Method}, 
			    {stream,      Stream}, 
			    {session,     Session}, 
			    {status_line, StatusLine}]),

    FinalResult = 
	try Module:Function([Data | Args]) of
	    {ok, Result} ->
		?hcrd("data processed - ok", []),
		handle_http_msg(Result, State); 
	    {_, whole_body, _} when Method =:= head ->
		?hcrd("data processed - whole body", []),
		handle_response(State#state{body = <<>>}); 
	    {Module, whole_body, [Body, Length]} ->
		?hcrd("data processed - whole body", [{length, Length}]),
		{_, Code, _} = StatusLine,
		{NewBody, NewRequest} = stream(Body, Request, Code),
		%% When we stream we will not keep the already
		%% streamed data, that would be a waste of memory.
		NewLength = 
		    case Stream of
			none ->   
			    Length;
			_ ->
			    Length - size(Body)			    
		    end,
		
		NewState = next_body_chunk(State),
		NewMFA   = {Module, whole_body, [NewBody, NewLength]}, 
		{noreply, NewState#state{mfa     = NewMFA,
					 request = NewRequest}};
	    NewMFA ->
		?hcrd("data processed - new mfa", []),
		activate_once(Session),
		{noreply, State#state{mfa = NewMFA}}
	catch
	    exit:_Exit ->
		?hcrd("data processing exit", [{exit, _Exit}]),
		ClientReason = {could_not_parse_as_http, Data}, 
		ClientErrMsg = httpc_response:error(Request, ClientReason),
		NewState     = answer_request(Request, ClientErrMsg, State),
		{stop, normal, NewState};
	    error:_Error ->    
		?hcrd("data processing error", [{error, _Error}]),
		ClientReason = {could_not_parse_as_http, Data}, 
		ClientErrMsg = httpc_response:error(Request, ClientReason),
		NewState     = answer_request(Request, ClientErrMsg, State),   
		{stop, normal, NewState}
	
	end,
    ?hcri("data processed", []),
    FinalResult;


handle_info({Proto, Socket, Data}, 
	    #state{mfa          = MFA, 
		   request      = Request, 
		   session      = Session, 
		   status       = Status,
		   status_line  = StatusLine, 
		   profile_name = Profile} = State) 
  when (Proto =:= tcp) orelse 
       (Proto =:= ssl) orelse 
       (Proto =:= httpc_handler) ->

    error_logger:warning_msg("Received unexpected ~p data on ~p"
			     "~n   Data:       ~p"
			     "~n   MFA:        ~p"
			     "~n   Request:    ~p"
			     "~n   Session:    ~p"
			     "~n   Status:     ~p"
			     "~n   StatusLine: ~p"
			     "~n   Profile:    ~p"
			     "~n", 
			     [Proto, Socket, Data, MFA, 
			      Request, Session, Status, StatusLine, Profile]),

    {noreply, State};


%% The Server may close the connection to indicate that the
%% whole body is now sent instead of sending an length
%% indicator.
handle_info({tcp_closed, _}, State = #state{mfa = {_, whole_body, Args}}) ->
    handle_response(State#state{body = hd(Args)}); 
handle_info({ssl_closed, _}, State = #state{mfa = {_, whole_body, Args}}) ->
    handle_response(State#state{body = hd(Args)}); 

%%% Server closes idle pipeline
handle_info({tcp_closed, _}, State = #state{request = undefined}) ->
    {stop, normal, State};
handle_info({ssl_closed, _}, State = #state{request = undefined}) ->
    {stop, normal, State};

%%% Error cases
handle_info({tcp_closed, _}, #state{session = Session0} = State) ->
    Socket  = Session0#session.socket,
    Session = Session0#session{socket = {remote_close, Socket}},
    %% {stop, session_remotly_closed, State};
    {stop, normal, State#state{session = Session}};
handle_info({ssl_closed, _}, #state{session = Session0} = State) ->
    Socket  = Session0#session.socket,
    Session = Session0#session{socket = {remote_close, Socket}},
    %% {stop, session_remotly_closed, State};
    {stop, normal, State#state{session = Session}};
handle_info({tcp_error, _, _} = Reason, State) ->
    {stop, Reason, State};
handle_info({ssl_error, _, _} = Reason, State) ->
    {stop, Reason, State};

%% Timeouts
%% Internally, to a request handling process, a request timeout is
%% seen as a canceled request.
handle_info({timeout, RequestId}, 
	    #state{request  = #request{id = RequestId} = Request,
		   canceled = Canceled} = State) ->
    ?hcri("timeout of current request", [{id, RequestId}]),
    httpc_response:send(Request#request.from, 
			httpc_response:error(Request, timeout)),
    ?hcrv("response (timeout) sent - now terminate", []),
    {stop, normal, 
     State#state{request  = Request#request{from = answer_sent},
		 canceled = [RequestId | Canceled]}};

handle_info({timeout, RequestId}, #state{canceled = Canceled} = State) ->
    ?hcri("timeout", [{id, RequestId}]),
    Filter = 
	fun(#request{id = Id, from = From} = Request) when Id =:= RequestId ->
		?hcrv("found request", [{id, Id}, {from, From}]),
		%% Notify the owner
		Response = httpc_response:error(Request, timeout), 
		httpc_response:send(From, Response),
		?hcrv("response (timeout) sent", []),
		[Request#request{from = answer_sent}];
	   (_) ->
		true
	end,
    case State#state.status of
	pipeline ->
	    ?hcrd("pipeline", []),
	    Pipeline = queue:filter(Filter, State#state.pipeline),
	    {noreply, State#state{canceled = [RequestId | Canceled],
				  pipeline = Pipeline}};
	keep_alive ->
	    ?hcrd("keep_alive", []),
	    KeepAlive = queue:filter(Filter, State#state.keep_alive),
	    {noreply, State#state{canceled   = [RequestId | Canceled],
				  keep_alive = KeepAlive}}
    end;

handle_info(timeout_queue, State = #state{request = undefined}) ->
    {stop, normal, State};

%% Timing was such as the pipeline_timout was not canceled!
handle_info(timeout_queue, #state{timers = Timers} = State) ->
    {noreply, State#state{timers = 
			  Timers#timers{queue_timer = undefined}}};

%% Setting up the connection to the server somehow failed. 
handle_info({init_error, _, ClientErrMsg},
	    State = #state{request = Request}) ->
    NewState = answer_request(Request, ClientErrMsg, State),
    {stop, normal, NewState};


%%% httpc_manager process dies. 
handle_info({'EXIT', _, _}, State = #state{request = undefined}) ->
    {stop, normal, State};
%%Try to finish the current request anyway,
%% there is a fairly high probability that it can be done successfully.
%% Then close the connection, hopefully a new manager is started that
%% can retry requests in the pipeline.
handle_info({'EXIT', _, _}, State) ->
    {noreply, State#state{status = close}}.
    

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> _  (ignored by gen_server)
%% Description: Shutdown the httpc_handler
%%--------------------------------------------------------------------

%% Init error there is no socket to be closed.
terminate(normal, 
	  #state{request = Request, 
		 session = {send_failed, AReason} = Reason} = State) ->
    ?hcrd("terminate", [{send_reason, AReason}, {request, Request}]),
    maybe_send_answer(Request, 
		      httpc_response:error(Request, Reason), 
		      State),
    ok; 

terminate(normal, 
	  #state{request = Request, 
		 session = {connect_failed, AReason} = Reason} = State) ->
    ?hcrd("terminate", [{connect_reason, AReason}, {request, Request}]),
    maybe_send_answer(Request, 
		      httpc_response:error(Request, Reason), 
		      State),
    ok; 

terminate(normal, #state{session = undefined}) ->
    ok;  

%% Init error sending, no session information has been setup but
%% there is a socket that needs closing.
terminate(normal, 
	  #state{session = #session{id = undefined} = Session}) ->  
    close_socket(Session);

%% Socket closed remotely
terminate(normal, 
	  #state{session = #session{socket      = {remote_close, Socket},
				    socket_type = SocketType, 
				    id          = Id}, 
		 profile_name = ProfileName,
		 request      = Request,
		 timers       = Timers,
		 pipeline     = Pipeline}) ->  
    ?hcrt("terminate(normal) - remote close", 
	  [{id, Id}, {profile, ProfileName}]),

    %% Clobber session
    (catch httpc_manager:delete_session(Id, ProfileName)),

    %% Cancel timers
    #timers{request_timers = ReqTmrs, queue_timer = QTmr} = Timers, 
    cancel_timer(QTmr, timeout_queue),
    lists:foreach(fun({_, Timer}) -> cancel_timer(Timer, timeout) end, 
		  ReqTmrs),

    %% Maybe deliver answers to requests
    deliver_answers([Request | queue:to_list(Pipeline)]),

    %% And, just in case, close our side (**really** overkill)
    http_transport:close(SocketType, Socket);

terminate(_, #state{session      = #session{id          = Id,
					    socket      = Socket, 
					    socket_type = SocketType},
		    request      = undefined,
		    profile_name = ProfileName,
		    timers       = Timers,
		    pipeline     = Pipeline,
		    keep_alive   = KeepAlive} = State) -> 
    (catch httpc_manager:delete_session(Id, ProfileName)),

    maybe_retry_queue(Pipeline, State),
    maybe_retry_queue(KeepAlive, State),

    cancel_timer(Timers#timers.queue_timer, timeout_queue),
    http_transport:close(SocketType, Socket);

terminate(Reason, #state{request = undefined}) -> 
    ?hcrt("terminate", [{reason, Reason}]),
    ok;

terminate(Reason, #state{request = Request} = State) -> 
    ?hcrd("terminate", [{reason, Reason}, {request, Request}]),
    NewState = maybe_send_answer(Request, 
				 httpc_response:error(Request, Reason), 
				 State),
    terminate(Reason, NewState#state{request = undefined}).

maybe_retry_queue(Q, State) ->
    case queue:is_empty(Q) of 
	false ->
	    retry_pipeline(queue:to_list(Q), State);
	true ->
	    ok
    end.
    
maybe_send_answer(#request{from = answer_sent}, _Reason, State) ->
    State;
maybe_send_answer(Request, Answer, State) ->
    answer_request(Request, Answer, State).

deliver_answers([]) ->
    ?hcrd("deliver answer done", []),
    ok;
deliver_answers([#request{id = Id, from = From} = Request | Requests]) 
  when is_pid(From) ->
    Response = httpc_response:error(Request, socket_closed_remotely),
    ?hcrd("deliver answer", [{id, Id}, {from, From}, {response, Response}]),
    httpc_response:send(From, Response),
    deliver_answers(Requests);
deliver_answers([Request|Requests]) ->
    ?hcrd("skip deliver answer", [{request, Request}]),
    deliver_answers(Requests).


%%--------------------------------------------------------------------
%% Func: code_change(_OldVsn, State, Extra) -> {ok, NewState}
%% Purpose: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_, #state{request = Request, pipeline = Queue} = State, 
	    [{from, '5.0.1'}, {to, '5.0.2'}]) ->
    Settings = new_http_options(Request#request.settings),
    NewRequest = Request#request{settings = Settings},
    NewQueue = new_queue(Queue, fun new_http_options/1),
    {ok, State#state{request = NewRequest, pipeline = NewQueue}};

code_change(_, #state{request = Request, pipeline = Queue} = State, 
	    [{from, '5.0.2'}, {to, '5.0.1'}]) ->
    Settings = old_http_options(Request#request.settings),
    NewRequest = Request#request{settings = Settings},
    NewQueue = new_queue(Queue, fun old_http_options/1),
    {ok, State#state{request = NewRequest, pipeline = NewQueue}};

code_change(_, State, _) ->
    {ok, State}.

new_http_options({http_options, TimeOut, AutoRedirect, SslOpts,
		  Auth, Relaxed}) ->
    {http_options, "HTTP/1.1", TimeOut, AutoRedirect, SslOpts,
     Auth, Relaxed}.

old_http_options({http_options, _, TimeOut, AutoRedirect,
		  SslOpts, Auth, Relaxed}) ->
    {http_options, TimeOut, AutoRedirect, SslOpts, Auth, Relaxed}.

new_queue(Queue, Fun) ->
    List = queue:to_list(Queue),
    NewList = 
	lists:map(fun(Request) ->
			  Settings = 
			      Fun(Request#request.settings),
			  Request#request{settings = Settings}
		  end, List),
    queue:from_list(NewList).
    

%%%--------------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------

connect(SocketType, ToAddress, 
	#options{ipfamily    = IpFamily,
		 ip          = FromAddress,
		 port        = FromPort,
		 socket_opts = Opts0}, Timeout) ->
    Opts1 = 
	case FromPort of
	    default ->
		Opts0;
	    _ ->
		[{port, FromPort} | Opts0]
	end,
    Opts2 = 
	case FromAddress of
	    default ->
		Opts1;
	    _ ->
		[{ip, FromAddress} | Opts1]
	end,
    case IpFamily of
	inet6fb4 ->
	    Opts3 = [inet6 | Opts2],
	    case http_transport:connect(SocketType, ToAddress, Opts3, Timeout) of
		{error, Reason} when ((Reason =:= nxdomain) orelse 
				      (Reason =:= eafnosupport)) -> 
		    Opts4 = [inet | Opts2], 
		    http_transport:connect(SocketType, ToAddress, Opts4, Timeout);
		Other ->
		    Other
	    end;
	_ ->
	    Opts3 = [IpFamily | Opts2], 
	    http_transport:connect(SocketType, ToAddress, Opts3, Timeout)
    end.
		
connect_and_send_first_request(Address, 
			       #request{settings    = Settings,
					headers     = Headers,
					address     = OrigAddress,
					scheme      = Scheme} = Request, 
			       #state{options = Options} = State) ->

    ?hcrd("connect", 
	  [{address, Address}, {request, Request}, {options, Options}]),

    SocketType  = socket_type(Request),
    ConnTimeout = Settings#http_options.connect_timeout,
    case connect(SocketType, Address, Options, ConnTimeout) of
	{ok, Socket} ->
	    Session = #session{id          = {OrigAddress, self()},
			       scheme      = Scheme,
			       socket      = Socket,
			       socket_type = SocketType}, 
	    ?hcrd("connected - now send first request", [{socket, Socket}]),
	    case httpc_request:send(Address, Session, Request) of
		ok ->
		    ?hcrd("first request sent", []),
		    ClientClose = 
			httpc_request:is_client_closing(Headers),
		    SessionType = httpc_manager:session_type(Options),
		    Session2 =
			Session#session{client_close = ClientClose,
					type         = SessionType},
		    TmpState = 
			State#state{request     = Request, 
				    session     = Session2, 
				    mfa         = init_mfa(Request, State),
				    status_line = init_status_line(Request),
				    headers     = undefined,
				    body        = undefined,
				    status      = new},
		    ?hcrt("activate socket", []),
		    activate_once(Session),
		    NewState = activate_request_timeout(TmpState),
		    {ok, NewState};

		{error, Reason} = Error -> 
		    ?hcrv("failed sending request", [{reason, Reason}]),
		    {stop, Error, 
		     State#state{session = {send_failed, Reason}, 
				 request = Request}}
	    end;

	{error, Reason} = Error -> 
	    ?hcri("connect failed", [{reason, Reason}]),
	    {stop, Error, State#state{session = {connect_failed, Reason},
				       request = Request}}
    end.


handler_info(#state{request     = Request, 
		    session     = Session, 
		    status_line = _StatusLine, 
		    pipeline    = Pipeline, 
		    keep_alive  = KeepAlive, 
		    status      = Status,
		    canceled    = _Canceled, 
		    options     = _Options,
		    timers      = _Timers} = _State) ->

    ?hcrt("handler info", [{request,    Request},
			   {session,    Session}, 
			   {pipeline,   Pipeline}, 
			   {keep_alive, KeepAlive}, 
			   {status,     Status}]),

    %% Info about the current request
    RequestInfo = 
	case Request of
	    undefined ->
		[];
	    #request{id      = Id,
		     started = ReqStarted} ->
		[{id, Id}, {started, ReqStarted}]
	end,

    ?hcrt("handler info", [{request_info, RequestInfo}]),

    %% Info about the current session/socket
    SessionType = Session#session.type, 
    QueueLen    = case SessionType of
		      pipeline ->
			  queue:len(Pipeline);
		      keep_alive ->
			  queue:len(KeepAlive)
		  end,
    Scheme     = Session#session.scheme, 
    Socket     = Session#session.socket, 
    SocketType = Session#session.socket_type, 

    ?hcrt("handler info", [{session_type, SessionType}, 
			   {queue_length, QueueLen}, 
			   {scheme,       Scheme}, 
			   {socket,       Socket}]),

    SocketOpts  = http_transport:getopts(SocketType, Socket), 
    SocketStats = http_transport:getstat(SocketType, Socket), 

    Remote = http_transport:peername(SocketType, Socket), 
    Local  = http_transport:sockname(SocketType, Socket), 

    ?hcrt("handler info", [{remote,       Remote}, 
			   {local,        Local}, 
			   {socket_opts,  SocketOpts}, 
			   {socket_stats, SocketStats}]),

    SocketInfo  = [{remote,       Remote}, 
		   {local,        Local}, 
		   {socket_opts,  SocketOpts},
		   {socket_stats, SocketStats}],

    SessionInfo = 
	[{type,         SessionType},
	 {queue_length, QueueLen},
	 {scheme,       Scheme}, 
	 {socket_info,  SocketInfo}], 
		
    [{status,          Status}, 
     {current_request, RequestInfo},
     {session,         SessionInfo}].



handle_http_msg({Version, StatusCode, ReasonPharse, Headers, Body}, 
		State = #state{request = Request}) ->
    ?hcrt("handle_http_msg", [{headers, Headers}]),
    case Headers#http_response_h.'content-type' of
        "multipart/byteranges" ++ _Param ->
            exit({not_yet_implemented, multypart_byteranges});
        _ ->
	    StatusLine       = {Version, StatusCode, ReasonPharse}, 
	    {ok, NewRequest} = start_stream(StatusLine, Headers, Request), 
            handle_http_body(Body, 
			     State#state{request     = NewRequest,
					 status_line = StatusLine, 
					 headers     = Headers})
    end;
handle_http_msg({ChunkedHeaders, Body}, #state{headers = Headers} = State) ->
    ?hcrt("handle_http_msg", 
	  [{chunked_headers, ChunkedHeaders}, {headers, Headers}]),
    NewHeaders = http_chunk:handle_headers(Headers, ChunkedHeaders),
    handle_response(State#state{headers = NewHeaders, body = Body});
handle_http_msg(Body, #state{status_line = {_,Code, _}} = State) ->
    ?hcrt("handle_http_msg", [{code, Code}]),
    {NewBody, NewRequest} = stream(Body, State#state.request, Code),
    handle_response(State#state{body = NewBody, request = NewRequest}).

handle_http_body(<<>>, State = #state{status_line = {_,304, _}}) ->
    ?hcrt("handle_http_body - 304", []),
    handle_response(State#state{body = <<>>});

handle_http_body(<<>>, State = #state{status_line = {_,204, _}}) ->
    ?hcrt("handle_http_body - 204", []),
    handle_response(State#state{body = <<>>});

handle_http_body(<<>>, State = #state{request = #request{method = head}}) ->
    ?hcrt("handle_http_body - head", []),
    handle_response(State#state{body = <<>>});

handle_http_body(Body, #state{headers       = Headers, 
			      max_body_size = MaxBodySize,
			      status_line   = {_,Code, _},
			      request       = Request} = State) ->
    ?hcrt("handle_http_body", 
	  [{max_body_size, MaxBodySize}, {headers, Headers}, {code, Code}]),
    TransferEnc = Headers#http_response_h.'transfer-encoding',
    case case_insensitive_header(TransferEnc) of
        "chunked" ->
	    ?hcrt("handle_http_body - chunked", []),
	    case http_chunk:decode(Body, State#state.max_body_size, 
				   State#state.max_header_size, 
				   {Code, Request}) of
		{Module, Function, Args} ->
		    ?hcrt("handle_http_body - new mfa", 
			  [{module,   Module}, 
			   {function, Function}, 
			   {args,     Args}]),
		    NewState = next_body_chunk(State),
		    {noreply, NewState#state{mfa = 
					     {Module, Function, Args}}};
		{ok, {ChunkedHeaders, NewBody}} ->
		    ?hcrt("handle_http_body - new body", 
			  [{chunked_headers, ChunkedHeaders}, 
			   {new_body,        NewBody}]),
		    NewHeaders = http_chunk:handle_headers(Headers, 
							   ChunkedHeaders),
		    handle_response(State#state{headers = NewHeaders, 
						body    = NewBody})
	    end;
        Encoding when is_list(Encoding) ->
	    ?hcrt("handle_http_body - encoding", [{encoding, Encoding}]),
	    NewState = answer_request(Request, 
				      httpc_response:error(Request, 
							   unknown_encoding),
				      State),
	    {stop, normal, NewState};
        _ ->
	    ?hcrt("handle_http_body - other", []),
            Length =
                list_to_integer(Headers#http_response_h.'content-length'),
            case ((Length =< MaxBodySize) orelse (MaxBodySize =:= nolimit)) of
                true ->
                    case httpc_response:whole_body(Body, Length) of
                        {ok, Body} ->
			    {NewBody, NewRequest} = 
				stream(Body, Request, Code),
			    handle_response(State#state{body    = NewBody,
							request = NewRequest});
                        MFA ->
			    NewState = next_body_chunk(State),   
			    {noreply, NewState#state{mfa = MFA}}
		    end;
                false ->
		    NewState = 
			answer_request(Request,
				       httpc_response:error(Request, 
							    body_too_big),
				       State),
                    {stop, normal, NewState}
            end
    end.

handle_response(#state{status = new} = State) ->
    ?hcrd("handle response - status = new", []),
    handle_response(try_to_enable_pipeline_or_keep_alive(State));

handle_response(#state{request      = Request,
		       status       = Status,
		       session      = Session, 
		       status_line  = StatusLine,
		       headers      = Headers, 
		       body         = Body,
		       options      = Options,
		       profile_name = ProfileName} = State) 
  when Status =/= new ->
    
    ?hcrd("handle response", [{profile,     ProfileName},
			      {status,      Status},
			      {request,     Request},
			      {session,     Session}, 
			      {status_line, StatusLine}]),

    handle_cookies(Headers, Request, Options, ProfileName),
    case httpc_response:result({StatusLine, Headers, Body}, Request) of
	%% 100-continue
	continue -> 
	    ?hcrd("handle response - continue", []),
	    %% Send request body
	    {_, RequestBody} = Request#request.content,
	    send_raw(Session, RequestBody),
	    %% Wait for next response
	    activate_once(Session),
	    Relaxed = (Request#request.settings)#http_options.relaxed,
	    MFA = {httpc_response, parse,
		   [State#state.max_header_size, Relaxed]}, 
	    {noreply, State#state{mfa         = MFA,
				  status_line = undefined,
				  headers     = undefined,
				  body        = undefined}};

	%% Ignore unexpected 100-continue response and receive the
	%% actual response that the server will send right away. 
	{ignore, Data} -> 
	    ?hcrd("handle response - ignore", [{data, Data}]),
	    Relaxed = (Request#request.settings)#http_options.relaxed,
	    MFA     = {httpc_response, parse,
		       [State#state.max_header_size, Relaxed]}, 
	    NewState = State#state{mfa         = MFA, 
				   status_line = undefined,
				   headers     = undefined,
				   body        = undefined},
	    handle_info({httpc_handler, dummy, Data}, NewState);

	%% On a redirect or retry the current request becomes 
	%% obsolete and the manager will create a new request 
	%% with the same id as the current.
	{redirect, NewRequest, Data} ->
	    ?hcrt("handle response - redirect", 
		  [{new_request, NewRequest}, {data, Data}]), 
	    ok = httpc_manager:redirect_request(NewRequest, ProfileName),
	    handle_queue(State#state{request = undefined}, Data);
	{retry, TimeNewRequest, Data} ->
	    ?hcrt("handle response - retry", 
		  [{time_new_request, TimeNewRequest}, {data, Data}]), 
	    ok = httpc_manager:retry_request(TimeNewRequest, ProfileName),
	    handle_queue(State#state{request = undefined}, Data);
	{ok, Msg, Data} ->
	    ?hcrd("handle response - ok", []),
	    end_stream(StatusLine, Request),
	    NewState = answer_request(Request, Msg, State),
	    handle_queue(NewState, Data); 
	{stop, Msg} ->
	    ?hcrd("handle response - stop", [{msg, Msg}]),
	    end_stream(StatusLine, Request),
	    NewState = answer_request(Request, Msg, State),
	    {stop, normal, NewState}
    end.

handle_cookies(_,_, #options{cookies = disabled}, _) ->
    ok;
%% User wants to verify the cookies before they are stored,
%% so the user will have to call a store command.
handle_cookies(_,_, #options{cookies = verify}, _) ->
    ok;
handle_cookies(Headers, Request, #options{cookies = enabled}, ProfileName) ->
    {Host, _ } = Request#request.address,
    Cookies = httpc_cookie:cookies(Headers#http_response_h.other, 
				  Request#request.path, Host),
    httpc_manager:store_cookies(Cookies, Request#request.address,
				ProfileName).

%% This request could not be pipelined or used as sequential keept alive
%% queue
handle_queue(#state{status = close} = State, _) ->
    {stop, normal, State};

handle_queue(#state{status = keep_alive} = State, Data) ->
    handle_keep_alive_queue(State, Data);

handle_queue(#state{status = pipeline} = State, Data) ->
    handle_pipeline(State, Data).

handle_pipeline(#state{status       = pipeline, 
		       session      = Session,
		       profile_name = ProfileName,
		       options      = #options{pipeline_timeout = TimeOut}} = 
		State, 
		Data) ->

    ?hcrd("handle pipeline", [{profile, ProfileName}, 
			      {session, Session},
			      {timeout, TimeOut}]),

    case queue:out(State#state.pipeline) of
	{empty, _} ->
	    ?hcrd("epmty pipeline queue", []),
	    
	    %% The server may choose too teminate an idle pipeline
	    %% in this case we want to receive the close message
	    %% at once and not when trying to pipeline the next
	    %% request.
	    activate_once(Session), 

	    %% If a pipeline that has been idle for some time is not
	    %% closed by the server, the client may want to close it.
	    NewState   = activate_queue_timeout(TimeOut, State),
	    NewSession = Session#session{queue_length = 0},
	    httpc_manager:insert_session(NewSession, ProfileName),
	    %% Note mfa will be initilized when a new request 
	    %% arrives.
	    {noreply, 
	     NewState#state{request     = undefined, 
			    mfa         = undefined,
			    status_line = undefined,
			    headers     = undefined,
			    body        = undefined}};
	{{value, NextRequest}, Pipeline} ->    
	    case lists:member(NextRequest#request.id, 
			      State#state.canceled) of		
		true ->
		    ?hcrv("next request had been cancelled", []),
		    %% See comment for handle_cast({cancel, RequestId})
		    {stop, normal, 
		     State#state{request = 
				 NextRequest#request{from = answer_sent}}};
		false ->
		    ?hcrv("next request", [{request, NextRequest}]),
		    NewSession = 
			Session#session{queue_length =
					%% Queue + current
					queue:len(Pipeline) + 1},
		    httpc_manager:insert_session(NewSession, ProfileName),
		    Relaxed = 
			(NextRequest#request.settings)#http_options.relaxed,
		    MFA = {httpc_response, 
			   parse,
			   [State#state.max_header_size, Relaxed]}, 
		    NewState = 
			State#state{pipeline    = Pipeline,
				    request     = NextRequest,
				    mfa         = MFA,
				    status_line = undefined,
				    headers     = undefined,
				    body        = undefined},
		    case Data of
			<<>> ->
			    activate_once(Session), 
			    {noreply, NewState};
			_ ->
			    %% If we already received some bytes of
			    %% the next response
			    handle_info({httpc_handler, dummy, Data}, 
					NewState) 
		    end
	    end
    end.

handle_keep_alive_queue(
  #state{status       = keep_alive,
	 session      = Session,
	 profile_name = ProfileName,
	 options      = #options{keep_alive_timeout = TimeOut}} = State, 
  Data) ->

    ?hcrd("handle keep_alive", [{profile, ProfileName}, 
				{session, Session},
				{timeout, TimeOut}]),

    case queue:out(State#state.keep_alive) of
	{empty, _} ->
	    ?hcrd("empty keep_alive queue", []),
	    %% The server may choose too terminate an idle keep_alive session
	    %% in this case we want to receive the close message
	    %% at once and not when trying to send the next
	    %% request.
	    activate_once(Session), 
	    %% If a keep_alive session has been idle for some time is not
	    %% closed by the server, the client may want to close it.
	    NewState = activate_queue_timeout(TimeOut, State),
	    NewSession = Session#session{queue_length = 0},
	    httpc_manager:insert_session(NewSession, ProfileName),
	    %% Note mfa will be initilized when a new request 
	    %% arrives.
	    {noreply, 
	     NewState#state{request     = undefined, 
			    mfa         = undefined,
			    status_line = undefined,
			    headers     = undefined,
			    body        = undefined
			   } 
	    };
	{{value, NextRequest}, KeepAlive} ->    
	    case lists:member(NextRequest#request.id, 
			      State#state.canceled) of		
		true ->
		    ?hcrv("next request has already been canceled", []),
		    handle_keep_alive_queue(
		      State#state{keep_alive = KeepAlive}, Data);
		false ->
		    ?hcrv("next request", [{request, NextRequest}]),
		    Relaxed = 
			(NextRequest#request.settings)#http_options.relaxed,
		    MFA     = {httpc_response, parse, 
			       [State#state.max_header_size, Relaxed]}, 
		    NewState =
			State#state{request     = NextRequest,
				    keep_alive  = KeepAlive,
				    mfa         = MFA,
				    status_line = undefined,
				    headers     = undefined,
				    body        = undefined},
		    case Data of
			<<>> ->
			    activate_once(Session), 
			    {noreply, NewState};
			_ ->
			    %% If we already received some bytes of
			    %% the next response
			    handle_info({httpc_handler, dummy, Data}, 
					NewState) 
		    end
	    end
    end.


case_insensitive_header(Str) when is_list(Str) ->
    http_util:to_lower(Str);
%% Might be undefined if server does not send such a header 
case_insensitive_header(Str) ->
    Str.

activate_once(#session{socket = Socket, socket_type = SocketType}) ->
    http_transport:setopts(SocketType, Socket, [{active, once}]).

close_socket(#session{socket = Socket, socket_type = SocketType}) ->
    http_transport:close(SocketType, Socket).

activate_request_timeout(
  #state{request = #request{timer = undefined} = Request} = State) ->
    Timeout = (Request#request.settings)#http_options.timeout,
    case Timeout of
	infinity ->
	    State;
	_ ->
	    ReqId = Request#request.id, 
	    ?hcrt("activate request timer", 
		  [{request_id,    ReqId}, 
		   {time_consumed, t() - Request#request.started},
		   {timeout,       Timeout}]),
	    Msg       = {timeout, ReqId}, 
	    Ref       = erlang:send_after(Timeout, self(), Msg), 
	    Request2  = Request#request{timer = Ref}, 
	    ReqTimers = [{Request#request.id, Ref} |
			 (State#state.timers)#timers.request_timers],
	    Timers    = #timers{request_timers = ReqTimers}, 
	    State#state{request = Request2, timers = Timers}
    end;

%% Timer is already running! This is the case for a redirect or retry
activate_request_timeout(State) ->
    State.

activate_queue_timeout(infinity, State) ->
    State;
activate_queue_timeout(Time, State) ->
    Ref = erlang:send_after(Time, self(), timeout_queue),
    State#state{timers = #timers{queue_timer = Ref}}.


is_pipeline_enabled_client(#session{type = pipeline}) ->
    true;
is_pipeline_enabled_client(_) ->
    false.

is_keep_alive_enabled_server("HTTP/1." ++ N, _) when (hd(N) >= $1) ->
    true;
is_keep_alive_enabled_server("HTTP/1.0", 
			     #http_response_h{connection = "keep-alive"}) ->
    true;
is_keep_alive_enabled_server(_,_) ->
    false.

is_keep_alive_connection(Headers, #session{client_close = ClientClose}) ->
    (not ((ClientClose) orelse httpc_response:is_server_closing(Headers))).

try_to_enable_pipeline_or_keep_alive(
  #state{session      = Session, 
	 request      = #request{method = Method},
	 status_line  = {Version, _, _},
	 headers      = Headers,
	 profile_name = ProfileName} = State) ->
    ?hcrd("try to enable pipeline or keep-alive", 
	  [{version, Version}, 
	   {headers, Headers}, 
	   {session, Session}]),
    case is_keep_alive_enabled_server(Version, Headers) andalso 
	  is_keep_alive_connection(Headers, Session) of
	true ->
	    case (is_pipeline_enabled_client(Session) andalso 
		  httpc_request:is_idempotent(Method)) of
		true ->
		    httpc_manager:insert_session(Session, ProfileName),
		    State#state{status = pipeline};
		false ->
		    httpc_manager:insert_session(Session, ProfileName),
		    %% Make sure type is keep_alive in session
		    %% as it in this case might be pipeline
		    NewSession = Session#session{type = keep_alive}, 
		    State#state{status  = keep_alive,
				session = NewSession}
	    end;
	false ->
	    State#state{status = close}
    end.

answer_request(#request{id = RequestId, from = From} = Request, Msg, 
	       #state{timers = Timers, profile_name = ProfileName} = State) -> 
    ?hcrt("answer request", [{request, Request}]),
    httpc_response:send(From, Msg),
    RequestTimers = Timers#timers.request_timers,
    TimerRef =
	proplists:get_value(RequestId, RequestTimers, undefined),
    Timer = {RequestId, TimerRef},
    cancel_timer(TimerRef, {timeout, Request#request.id}),
    httpc_manager:request_done(RequestId, ProfileName),
    State#state{request = Request#request{from = answer_sent},
		timers = 
		Timers#timers{request_timers =
			      lists:delete(Timer, RequestTimers)}}.
cancel_timer(undefined, _) ->
    ok;
cancel_timer(Timer, TimeoutMsg) ->
    erlang:cancel_timer(Timer),
    receive 
	TimeoutMsg ->
	    ok
    after 0 ->
	    ok
    end.

retry_pipeline([], _) ->
    ok;

%% Skip requests when the answer has already been sent
retry_pipeline([#request{from = answer_sent}|PipeLine], State) ->
    retry_pipeline(PipeLine, State);

retry_pipeline([Request | PipeLine],  
	      #state{timers       = Timers, 
		     profile_name = ProfileName} = State) ->
    NewState =
	case (catch httpc_manager:retry_request(Request, ProfileName)) of
	    ok ->
		RequestTimers = Timers#timers.request_timers,
		ReqId    = Request#request.id, 
		TimerRef =
		    proplists:get_value(ReqId, RequestTimers, undefined),
		cancel_timer(TimerRef, {timeout, ReqId}),
		NewReqsTimers = lists:delete({ReqId, TimerRef}, RequestTimers),
		NewTimers     = Timers#timers{request_timers = NewReqsTimers},
		State#state{timers = NewTimers};

	    Error ->
		answer_request(Request#request.from,
			       httpc_response:error(Request, Error), State) 
	end,
    retry_pipeline(PipeLine, NewState).

%%% Check to see if the given {Host,Port} tuple is in the NoProxyList
%%% Returns an eventually updated {Host,Port} tuple, with the proxy address
handle_proxy(HostPort = {Host, _Port}, {Proxy, NoProxy}) ->
    case Proxy of
	undefined ->
	    HostPort;
	Proxy ->
	    case is_no_proxy_dest(Host, NoProxy) of
		true ->
		    HostPort;
		false ->
		    Proxy
	    end
    end.

is_no_proxy_dest(_, []) ->
    false;
is_no_proxy_dest(Host, [ "*." ++ NoProxyDomain | NoProxyDests]) ->    
    
    case is_no_proxy_dest_domain(Host, NoProxyDomain) of
	true ->
	    true;
	false ->
	    is_no_proxy_dest(Host, NoProxyDests)
    end;

is_no_proxy_dest(Host, [NoProxyDest | NoProxyDests]) ->
    IsNoProxyDest = case http_util:is_hostname(NoProxyDest) of
			true ->
			    fun is_no_proxy_host_name/2;
			false ->
			    fun is_no_proxy_dest_address/2
		    end,
    
    case IsNoProxyDest(Host, NoProxyDest) of
	true ->
	    true;
	false ->
	    is_no_proxy_dest(Host, NoProxyDests)
    end.

is_no_proxy_host_name(Host, Host) ->
    true;
is_no_proxy_host_name(_,_) ->
    false.

is_no_proxy_dest_domain(Dest, DomainPart) ->
    lists:suffix(DomainPart, Dest).

is_no_proxy_dest_address(Dest, Dest) ->
    true;
is_no_proxy_dest_address(Dest, AddressPart) ->
    lists:prefix(AddressPart, Dest).

init_mfa(#request{settings = Settings}, State) ->
    case Settings#http_options.version of
	"HTTP/0.9" ->
	    {httpc_response, whole_body, [<<>>, -1]};
	_ ->
	    Relaxed = Settings#http_options.relaxed,
	    {httpc_response, parse, [State#state.max_header_size, Relaxed]}
    end.

init_status_line(#request{settings = Settings}) ->
    case Settings#http_options.version of
	"HTTP/0.9" ->
	    {"HTTP/0.9", 200, "OK"};
	_ ->
	    undefined
    end.

socket_type(#request{scheme = http}) ->
    ip_comm;
socket_type(#request{scheme = https, settings = Settings}) ->
    Settings#http_options.ssl.
%% socket_type(http) ->
%%     ip_comm;
%% socket_type(https) ->
%%     {ssl1, []}. %% Dummy value ok for ex setopts that does not use this value

start_stream({_Version, _Code, _ReasonPhrase}, _Headers, 
	     #request{stream = none} = Request) ->
    ?hcrt("start stream - none", []), 
    {ok, Request};
start_stream({_Version, Code, _ReasonPhrase}, Headers, 
	     #request{stream = self} = Request) 
  when (Code =:= 200) orelse (Code =:= 206) ->
    ?hcrt("start stream - self", [{code, Code}]), 
    Msg = httpc_response:stream_start(Headers, Request, ignore),
    httpc_response:send(Request#request.from, Msg),
    {ok, Request};
start_stream({_Version, Code, _ReasonPhrase}, Headers, 
	     #request{stream = {self, once}} = Request) 
  when (Code =:= 200) orelse (Code =:= 206) ->
    ?hcrt("start stream - self:once", [{code, Code}]), 
    Msg = httpc_response:stream_start(Headers, Request, self()),
    httpc_response:send(Request#request.from, Msg),
    {ok, Request};    
start_stream({_Version, Code, _ReasonPhrase}, _Headers, 
	     #request{stream = Filename} = Request)
  when ((Code =:= 200) orelse (Code =:= 206)) andalso is_list(Filename) ->
    ?hcrt("start stream", [{code, Code}, {filename, Filename}]),
    case file:open(Filename, [write, raw, append, delayed_write]) of
        {ok, Fd} ->
            ?hcri("start stream - file open ok", [{fd, Fd}]),
            {ok, Request#request{stream = Fd}};
        {error, Reason} ->
            exit({stream_to_file_failed, Reason})
    end;
start_stream(_StatusLine, _Headers, Request) ->
    ?hcrt("start stream - no op", []),
    {ok, Request}.


%% Note the end stream message is handled by httpc_response and will
%% be sent by answer_request
end_stream(_, #request{stream = none}) ->
    ?hcrt("end stream - none", []), 
    ok;
end_stream(_, #request{stream = self}) ->
    ?hcrt("end stream - self", []), 
    ok;
end_stream(_, #request{stream = {self, once}}) ->
    ?hcrt("end stream - self:once", []), 
    ok;
end_stream({_,200,_}, #request{stream = Fd}) ->
    ?hcrt("end stream - 200", [{stream, Fd}]), 
    case file:close(Fd) of 
	ok ->
	    ok;
	{error, enospc} -> % Could be due to delayed_write
	    file:close(Fd)
    end;
end_stream({_,206,_}, #request{stream = Fd}) ->
    ?hcrt("end stream - 206", [{stream, Fd}]), 
    case file:close(Fd) of
       ok ->
           ok;
       {error, enospc} -> % Could be due to delayed_write
           file:close(Fd)
    end;
end_stream(SL, R) ->
    ?hcrt("end stream", [{status_line, SL}, {request, R}]), 
    ok.


next_body_chunk(#state{request = #request{stream = {self, once}}, 
		       once    = once, 
		       session = Session} = State) ->
    activate_once(Session), 
    State#state{once = inactive};
next_body_chunk(#state{request = #request{stream = {self, once}}, 
		       once = inactive} = State) ->
    State; %% Wait for user to call stream_next
next_body_chunk(#state{session = Session} = State) ->
    activate_once(Session), 
    State.

handle_verbose(verbose) ->
    dbg:p(self(), [r]);
handle_verbose(debug) ->
    dbg:p(self(), [call]),
    dbg:tp(?MODULE, [{'_', [], [{return_trace}]}]);
handle_verbose(trace) ->
    dbg:p(self(), [call]),
    dbg:tpl(?MODULE, [{'_', [], [{return_trace}]}]);
handle_verbose(_) ->
    ok.    


%%% Normaly I do not comment out code, I throw it away. But this might
%%% actually be used one day if ssl is improved.
%% send_ssl_tunnel_request(Address, Request = #request{address = {Host, Port}}, 
%% 			State) ->
%%     %% A ssl tunnel request is a special http request that looks like
%%     %% CONNECT host:port HTTP/1.1
%%     SslTunnelRequest = #request{method = connect, scheme = http,
%% 				headers = 
%% 				#http_request_h{
%% 				  host = Host, 
%% 				  address = Address, 
%% 				  path = Host ++ ":",
%% 				  pquery = integer_to_list(Port),
%% 				  other = [{ "Proxy-Connection", "keep-alive"}]},
%%     Ipv6 = (State#state.options)#options.ipv6,
%%     SocketType = socket_type(SslTunnelRequest),
%%     case http_transport:connect(SocketType, 
%%                                 SslTunnelRequest#request.address, Ipv6) of
%% 	{ok, Socket} ->
%% 	    case httpc_request:send(Address, SslTunnelRequest, Socket) of
%% 		ok ->
%% 		    Session = #tcp_session{id = 
%% 					   {SslTunnelRequest#request.address,
%% 					    self()},
%% 					   scheme = 
%% 					   SslTunnelRequest#request.scheme,
%% 					   socket = Socket},
%% 		    NewState = State#state{mfa = 
%% 					   {httpc_response, parse,
%% 					    [State#state.max_header_size]},
%% 					   request = Request,
%% 					   session = Session},
%% 		    http_transport:setopts(socket_type(
%%                                           SslTunnelRequest#request.scheme), 
%% 					   Socket, 
%% 					   [{active, once}]),
%% 		    {ok, NewState};
%% 		{error, Reason} -> 
%% 		    self() ! {init_error, error_sending, 
%% 			      httpc_response:error(Request, Reason)},
%% 		    {ok, State#state{request = Request,
%% 				     session = #tcp_session{socket = 
%% 							    Socket}}}
%% 	    end;
%% 	{error, Reason} ->
%% 	    self() ! {init_error, error_connecting, 
%% 		      httpc_response:error(Request, Reason)},
%% 	    {ok, State#state{request = Request}}
%%     end.

%% d(F) ->
%%    d(F, []).

%% d(F, A) -> 
%%     d(get(dbg), F, A).

%% d(true, F, A) ->
%%     io:format(user, "~w:~w:" ++ F ++ "~n", [self(), ?MODULE | A]);
%% d(_, _, _) ->
%%     ok.


send_raw(#session{socket = Socket, socket_type = SocketType}, Body) ->
    http_transport:send(SocketType, Socket, Body).



call(Msg, Pid) ->
    Timeout = infinity,
    call(Msg, Pid, Timeout).
call(Msg, Pid, Timeout) ->
    gen_server:call(Pid, Msg, Timeout).

cast(Msg, Pid) ->
    gen_server:cast(Pid, Msg).


%% to(To, Start) when is_integer(Start) andalso (Start >= 0) ->
%%     http_util:timeout(To, Start);
%% to(To, _Start) ->
%%     http_util:timeout(To, t()).

t() ->
    http_util:timestamp().
