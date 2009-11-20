%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
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

-include("httpc_internal.hrl").
-include("http_internal.hrl").


%%--------------------------------------------------------------------
%% Internal Application API
-export([start_link/3, send/2, cancel/2, stream/3, stream_next/1]).

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
	  status = new,   % new | pipeline | keep_alive | close | ssl_tunnel
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
start_link(Request, Options, ProfileName) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [[Request, Options, 
					      ProfileName]])}.


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
    ?hcrt("stream - none", [{body_part, BodyPart}]),
    {BodyPart, Request};

%% Stream to caller
stream(BodyPart, Request = #request{stream = Self}, Code) 
  when ((Code =:= 200) orelse  (Code =:= 206)) andalso 
       ((Self =:= self) orelse (Self =:= {self, once})) ->
    ?hcrt("stream - self", [{stream, Self}, {code, Code}, {body_part, BodyPart}]),
    httpc_response:send(Request#request.from, 
			{Request#request.id, stream, BodyPart}),
    {<<>>, Request};

stream(BodyPart, Request = #request{stream = Self}, 404) 
  when (Self =:= self) orelse (Self =:= {self, once}) ->
    ?hcrt("stream - self with 404", [{stream, Self}, {body_part, BodyPart}]),
    httpc_response:send(Request#request.from,
                       {Request#request.id, stream, BodyPart}),
    {<<>>, Request};

%% Stream to file
%% This has been moved to start_stream/3
%% We keep this for backward compatibillity...
stream(BodyPart, Request = #request{stream = Filename}, Code)
  when ((Code =:= 200) orelse (Code =:= 206)) andalso is_list(Filename) -> 
    ?hcrt("stream - filename", [{stream, Filename}, {code, Code}, {body_part, BodyPart}]),
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
    ?hcrt("stream to file", [{stream, Fd}, {code, Code}, {body_part, BodyPart}]),
    case file:write(Fd, BodyPart) of
	ok ->
	    {<<>>, Request};
	{error, Reason} ->
	    exit({stream_to_file_failed, Reason})
    end;

stream(BodyPart, Request,_) -> % only 200 and 206 responses can be streamed
    ?hcrt("stream - ignore", [{request, Request}, {body_part, BodyPart}]),
    {BodyPart, Request}.


%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([Request, Options, ProfileName]) -> {ok, State} | 
%%                       {ok, State, Timeout} | ignore |{stop, Reason}
%%
%%	Request = #request{}
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
init([Request, Options, ProfileName]) ->
    process_flag(trap_exit, true),

    handle_verbose(Options#options.verbose),
    Address = handle_proxy(Request#request.address, Options#options.proxy),
    {ok, State} =
	case {Address /= Request#request.address, Request#request.scheme} of
	    {true, https} ->
		Error = https_through_proxy_is_not_currently_supported,
		self() ! {init_error, 
			  Error, httpc_response:error(Request, Error)},
		{ok, #state{request = Request, options = Options,
			    status = ssl_tunnel}};
	    %% This is what we should do if and when ssl supports 
	    %% "socket upgrading"
	    %%send_ssl_tunnel_request(Address, Request,
	    %%		    #state{options = Options,
	    %%		   status = ssl_tunnel});
	    {_, _} ->
		send_first_request(Address, Request, 
				   #state{options = Options,
					  profile_name = ProfileName})
	end,
    gen_server:enter_loop(?MODULE, [], State).

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Request, _, State = #state{session = Session =
				       #tcp_session{socket = Socket,
						    type = pipeline},
				       timers = Timers,
				       options = Options,
				       profile_name = ProfileName}) ->
    Address = handle_proxy(Request#request.address, Options#options.proxy),

    case httpc_request:send(Address, Request, Socket) of
        ok ->
	    %% Activate the request time out for the new request
	    NewState = activate_request_timeout(State#state{request =
							     Request}),

	    ClientClose = httpc_request:is_client_closing(
			    Request#request.headers),
            case State#state.request of
                #request{} -> %% Old request no yet finished
		    %% Make sure to use the new value of timers in state
		    NewTimers = NewState#state.timers,
                    NewPipeline = queue:in(Request, State#state.pipeline),
		    NewSession = 
			Session#tcp_session{queue_length = 
					    %% Queue + current
					    queue:len(NewPipeline) + 1,
					    client_close = ClientClose},
		    httpc_manager:insert_session(NewSession, ProfileName),
                    {reply, ok, State#state{pipeline = NewPipeline,
					    session = NewSession,
					    timers = NewTimers}};
		undefined ->
		    %% Note: tcp-message reciving has already been
		    %% activated by handle_pipeline/2. 
		    cancel_timer(Timers#timers.queue_timer, 
				 timeout_queue),
		    NewSession = 
			Session#tcp_session{queue_length = 1,
					    client_close = ClientClose},
		    httpc_manager:insert_session(NewSession, ProfileName),
		    Relaxed = 
			(Request#request.settings)#http_options.relaxed, 
		    {reply, ok, 
		     NewState#state{request = Request,
				    session = NewSession,
				    mfa = {httpc_response, parse,
					   [State#state.max_header_size,
					    Relaxed]},
				    timers = 
				    Timers#timers{queue_timer =
						  undefined}}}
	    end;
	{error, Reason} ->
	    {reply, {pipeline_failed, Reason}, State}
    end;

handle_call(Request, _, #state{session = Session =
			       #tcp_session{type = keep_alive,
					    socket = Socket},
			       timers = Timers,
			       options = Options,
			       profile_name = ProfileName} = State) ->
       
    ClientClose = httpc_request:is_client_closing(Request#request.headers),
    
    Address = handle_proxy(Request#request.address, 
			   Options#options.proxy),
    case httpc_request:send(Address, Request, Socket) of
	ok ->
	    NewState = 
		activate_request_timeout(State#state{request =
						     Request}),

	    case State#state.request of
		#request{} -> %% Old request not yet finished
		    %% Make sure to use the new value of timers in state
		    NewTimers = NewState#state.timers,
                    NewKeepAlive = queue:in(Request, State#state.keep_alive),
		    NewSession = 
			Session#tcp_session{queue_length = 
					    %% Queue + current
					    queue:len(NewKeepAlive) + 1,
					    client_close = ClientClose},
		    httpc_manager:insert_session(NewSession, ProfileName),
                    {reply, ok, State#state{keep_alive = NewKeepAlive,
					    session = NewSession,
					    timers = NewTimers}};
		undefined ->
		    %% Note: tcp-message reciving has already been
		    %% activated by handle_pipeline/2. 
		    cancel_timer(Timers#timers.queue_timer, 
				 timeout_queue),
		    NewSession = 
			Session#tcp_session{queue_length = 1,
					    client_close = ClientClose},
		    httpc_manager:insert_session(NewSession, ProfileName),
		    Relaxed = 
			(Request#request.settings)#http_options.relaxed,
		    {reply, ok, 
		     NewState#state{request = Request,
				    session = NewSession, 
				    mfa = {httpc_response, parse,
					   [State#state.max_header_size,
					    Relaxed]}}}
	    end;
	{error, Reason}    ->
	    {reply, {request_failed, Reason}, State}
    end.

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
handle_cast({cancel, RequestId}, State = #state{request = Request =
						#request{id = RequestId},
						profile_name = ProfileName}) ->
    httpc_manager:request_canceled(RequestId, ProfileName),
    {stop, normal, 
     State#state{canceled = [RequestId | State#state.canceled],
		 request = Request#request{from = answer_sent}}};
handle_cast({cancel, RequestId}, State = #state{profile_name = ProfileName}) ->
    httpc_manager:request_canceled(RequestId, ProfileName),
    {noreply, State#state{canceled = [RequestId | State#state.canceled]}};
handle_cast(stream_next, #state{session = Session} = State) ->
    http_transport:setopts(socket_type(Session#tcp_session.scheme), 
			   Session#tcp_session.socket, [{active, once}]), 
    {noreply, State#state{once = once}}.


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Proto, _Socket, Data}, 
	    #state{mfa = {Module, Function, Args} = MFA, 
		   request = #request{method = Method, 
				      stream = Stream} = Request, 
		   session = Session, 
		   status_line = StatusLine} = State) 
  when (Proto =:= tcp) orelse 
       (Proto =:= ssl) orelse 
       (Proto =:= httpc_handler) ->

    ?hcri("received data", [{proto, Proto}, {data, Data}, {mfa, MFA}, {method, Method}, {stream, Stream}, {session, Session}, {status_line, StatusLine}]),

    FinalResult = 
	try Module:Function([Data | Args]) of
	    {ok, Result} ->
		?hcrd("data processed - ok", [{result, Result}]),
		handle_http_msg(Result, State); 
	    {_, whole_body, _} when Method =:= head ->
		?hcrd("data processed - whole body", []),
		handle_response(State#state{body = <<>>}); 
	    {Module, whole_body, [Body, Length]} ->
		?hcrd("data processed - whole body", [{module, Module}, {body, Body}, {length, Length}]),
		{_, Code, _} = StatusLine,
		{NewBody, NewRequest} = stream(Body, Request, Code),
		%% When we stream we will not keep the already
		%% streamed data, that would be a waste of memory.
		NewLength = case Stream of
				none ->   
				    Length;
				_ ->
				    Length - size(Body)			    
			    end,
		
		NewState = next_body_chunk(State),
		
		{noreply, NewState#state{mfa = {Module, whole_body, 
						[NewBody, NewLength]},
					 request = NewRequest}};
	    NewMFA ->
		?hcrd("data processed", [{new_mfa, NewMFA}]),
		http_transport:setopts(socket_type(Session#tcp_session.scheme), 
				       Session#tcp_session.socket, 
				       [{active, once}]),
		{noreply, State#state{mfa = NewMFA}}
	catch
	    exit:_ ->
		ClientErrMsg = httpc_response:error(Request, 
						    {could_not_parse_as_http, 
						     Data}),
		NewState = answer_request(Request, ClientErrMsg, State),
		{stop, normal, NewState};
	      error:_ ->    
		ClientErrMsg = httpc_response:error(Request, 
						    {could_not_parse_as_http, 
						     Data}),
		NewState = answer_request(Request, ClientErrMsg, State),   
		{stop, normal, NewState}
	
	end,
    ?hcri("data processed", [{result, FinalResult}]),
    FinalResult;


handle_info({Proto, Socket, Data}, 
	    #state{mfa     = MFA, 
		   request = Request, 
		   session = Session, 
		   status  = Status,
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
    Socket  = Session0#tcp_session.socket,
    Session = Session0#tcp_session{socket = {remote_close, Socket}},
    %% {stop, session_remotly_closed, State};
    {stop, normal, State#state{session = Session}};
handle_info({ssl_closed, _}, #state{session = Session0} = State) ->
    Socket  = Session0#tcp_session.socket,
    Session = Session0#tcp_session{socket = {remote_close, Socket}},
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
    httpc_response:send(Request#request.from, 
		       httpc_response:error(Request,timeout)),
    {stop, normal, 
     State#state{request  = Request#request{from = answer_sent},
		 canceled = [RequestId | Canceled]}};

handle_info({timeout, RequestId}, #state{canceled = Canceled} = State) ->
    Filter = 
	fun(#request{id = Id, from = From} = Request) when Id =:= RequestId ->
		%% Notify the owner
		Response = httpc_response:error(Request, timeout), 
		httpc_response:send(From, Response),
		[Request#request{from = answer_sent}];
	   (_) ->
		true
	end,
    case State#state.status of
	pipeline ->
	    Pipeline = queue:filter(Filter, State#state.pipeline),
	    {noreply, State#state{canceled = [RequestId | Canceled],
				  pipeline = Pipeline}};
	keep_alive ->
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
terminate(normal, #state{session = undefined}) ->
    ok;  

%% Init error sending, no session information has been setup but
%% there is a socket that needs closing.
terminate(normal, #state{request = Request,
			 session = #tcp_session{id = undefined,
						socket = Socket}}) ->  
    http_transport:close(socket_type(Request), Socket);

%% Socket closed remotely
terminate(normal, 
	  #state{session = #tcp_session{socket = {remote_close, Socket},
					id     = Id}, 
		 profile_name = ProfileName,
		 request = Request,
		 timers  = Timers,
		 pipeline = Pipeline}) ->  
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
    http_transport:close(socket_type(Request), Socket);

terminate(_, State = #state{session      = Session, 
			    request      = undefined,
			    profile_name = ProfileName,
			    timers       = Timers,
			    pipeline     = Pipeline,
			    keep_alive   = KeepAlive}) -> 
    catch httpc_manager:delete_session(Session#tcp_session.id,
				       ProfileName),

    maybe_retry_queue(Pipeline, State),
    maybe_retry_queue(KeepAlive, State),

    cancel_timer(Timers#timers.queue_timer, timeout_queue),
    Socket = Session#tcp_session.socket, 
    http_transport:close(socket_type(Session#tcp_session.scheme), Socket);

terminate(Reason, State = #state{request = Request}) -> 
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
    ok;
deliver_answers([#request{from = From} = Request | Requests]) 
  when is_pid(From) ->
    Response = httpc_response:error(Request, socket_closed_remotely),
    httpc_response:send(From, Response),
    deliver_answers(Requests);
deliver_answers([_|Requests]) ->
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
    
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

connect(SocketType, ToAddress, #options{ipfamily = IpFamily,
					ip       = FromAddress,
					port     = FromPort}, Timeout) ->
    Opts1 = 
	case FromPort of
	    default ->
		[];
	    _ ->
		[{port, FromPort}]
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
		
    
send_first_request(Address, Request, #state{options = Options} = State) ->
    SocketType  = socket_type(Request),
    ConnTimeout = (Request#request.settings)#http_options.connect_timeout,
    ?hcri("connect", 
	  [{address, Address}, {request, Request}, {options, Options}]),
    case connect(SocketType, Address, Options, ConnTimeout) of
	{ok, Socket} ->
	    ?hcri("connected - now send first request", [{socket, Socket}]),
	    case httpc_request:send(Address, Request, Socket) of
		ok ->
		    ?hcri("first request sent", []),
		    ClientClose = 
			httpc_request:is_client_closing(
			  Request#request.headers),
		    SessionType = httpc_manager:session_type(Options),
		    Session =
			#tcp_session{id = {Request#request.address, self()},
				     scheme = Request#request.scheme,
				     socket = Socket,
				     client_close = ClientClose,
				     type = SessionType},
		    TmpState = State#state{request = Request, 
					   session = Session, 
					   mfa = init_mfa(Request, State),
					   status_line = 
					   init_status_line(Request),
					   headers = undefined,
					   body = undefined,
					   status = new},
		    http_transport:setopts(SocketType, 
					   Socket, [{active, once}]),
		    NewState = activate_request_timeout(TmpState),
		    {ok, NewState};

		{error, Reason} -> 		    
		    %% Commented out in wait of ssl support to avoid
		    %% dialyzer warning
		    %%case State#state.status of
		    %%	new -> % Called from init/1
		    self() ! {init_error, error_sending, 
			      httpc_response:error(Request, Reason)},
		    {ok, State#state{request = Request,
				     session = 
				     #tcp_session{socket = Socket}}}
		    %%ssl_tunnel -> % Not called from init/1
		    %%  NewState = 
		    %%	answer_request(Request, 
		    %%httpc_response:error(Request, 
		    %%Reason),
		    %%			       State),
		    %%	    {stop, normal, NewState}
		    %%    end
	    end;

	{error, Reason} -> 	    
	    %% Commented out in wait of ssl support to avoid
	    %% dialyzer warning
	    %% case State#state.status of
	    %%	new -> % Called from init/1
	    self() ! {init_error, error_connecting, 
		      httpc_response:error(Request, Reason)},
	    {ok, State#state{request = Request}}
	    %%	ssl_tunnel -> % Not called from init/1
	    %%    NewState = 
	    %%	answer_request(Request, 
	    %%		       httpc_response:error(Request, 
	    %%					    Reason),
	    %%		       State),
	    %%    {stop, normal, NewState}
	    %%end
    end.

handle_http_msg({Version, StatusCode, ReasonPharse, Headers, Body}, 
		State = #state{request = Request}) ->
    ?hcrt("handle_http_msg", [{body, Body}]),
    case Headers#http_response_h.'content-type' of
        "multipart/byteranges" ++ _Param ->
            exit(not_yet_implemented);
        _ ->
	    StatusLine = {Version, StatusCode, ReasonPharse}, 
	    {ok, NewRequest} = start_stream(StatusLine, Headers, Request), 
            handle_http_body(Body, 
			     State#state{request     = NewRequest,
					 status_line = StatusLine, 
					 headers     = Headers})
    end;
handle_http_msg({ChunkedHeaders, Body}, 
		State = #state{headers = Headers}) ->
    ?hcrt("handle_http_msg", [{chunked_headers, ChunkedHeaders}, {body, Body}]),
    NewHeaders = http_chunk:handle_headers(Headers, ChunkedHeaders),
    handle_response(State#state{headers = NewHeaders, body = Body});
handle_http_msg(Body, State = #state{status_line = {_,Code, _}}) ->
    ?hcrt("handle_http_msg", [{body, Body}, {code, Code}]),
    {NewBody, NewRequest}= stream(Body, State#state.request, Code),
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

handle_http_body(Body, State = #state{headers = Headers, 
				      max_body_size = MaxBodySize,
				      status_line = {_,Code, _},
				      request = Request}) ->
    ?hcrt("handle_http_body", [{body, Body}, {max_body_size, MaxBodySize}, {code, Code}]),
    TransferEnc = Headers#http_response_h.'transfer-encoding',
    case case_insensitive_header(TransferEnc) of
        "chunked" ->
	    ?hcrt("handle_http_body - chunked", []),
	    case http_chunk:decode(Body, State#state.max_body_size, 
				   State#state.max_header_size, 
				   {Code, Request}) of
		{Module, Function, Args} ->
		    ?hcrt("handle_http_body - new mfa", [{module, Module}, {function, Function}, {args, Args}]),
		    NewState = next_body_chunk(State),
		    {noreply, NewState#state{mfa = 
					     {Module, Function, Args}}};
		{ok, {ChunkedHeaders, NewBody}} ->
		    ?hcrt("handle_http_body - nyew body", [{chunked_headers, ChunkedHeaders}, {new_body, NewBody}]),
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
            case ((Length =< MaxBodySize) or (MaxBodySize == nolimit)) of
                true ->
                    case httpc_response:whole_body(Body, Length) of
                        {ok, Body} ->
			    {NewBody, NewRequest}= stream(Body, Request, Code),
			    handle_response(State#state{body = NewBody,
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

%%% Normaly I do not comment out code, I throw it away. But this might
%%% actually be used on day if ssl is improved.
%% handle_response(State = #state{status = ssl_tunnel,
%% 			       request = Request,
%% 			       options = Options,
%% 			       session = #tcp_session{socket = Socket,
%% 						      scheme = Scheme},
%% 			       status_line = {_, 200, _}}) ->
%%     %%% Insert code for upgrading the socket if and when ssl supports this.  
%%     Address = handle_proxy(Request#request.address, Options#options.proxy),   
%%     send_first_request(Address, Request, State);
%% handle_response(State = #state{status = ssl_tunnel,
%% 			      request = Request}) ->
%%     NewState = answer_request(Request,
%% 			      httpc_response:error(Request,
%% 						   ssl_proxy_tunnel_failed),
%% 			      State),
%%                     {stop, normal, NewState};

handle_response(State = #state{status = new}) ->
   handle_response(try_to_enable_pipeline_or_keep_alive(State));

handle_response(State = 
		#state{request      = Request,
		       status       = Status,
		       session      = Session, 
		       status_line  = StatusLine,
		       headers      = Headers, 
		       body         = Body,
		       options      = Options,
		       profile_name = ProfileName}) when Status =/= new ->
    ?hcrt("handle response", [{status, Status}, {session, Session}, {status_line, StatusLine}, {profile_name, ProfileName}]), 
    handle_cookies(Headers, Request, Options, ProfileName),
    case httpc_response:result({StatusLine, Headers, Body}, Request) of
	%% 100-continue
	continue -> 
	    %% Send request body
	    {_, RequestBody} = Request#request.content,
	    http_transport:send(socket_type(Session#tcp_session.scheme), 
					    Session#tcp_session.socket, 
				RequestBody),
	    %% Wait for next response
	    http_transport:setopts(socket_type(Session#tcp_session.scheme), 
				   Session#tcp_session.socket, 
				   [{active, once}]),
	    Relaxed = (Request#request.settings)#http_options.relaxed,
	    {noreply, 
	     State#state{mfa = {httpc_response, parse,
				[State#state.max_header_size,
				 Relaxed]},
			 status_line = undefined,
			 headers = undefined,
			 body = undefined
			}};
	%% Ignore unexpected 100-continue response and receive the
	%% actual response that the server will send right away. 
	{ignore, Data} -> 
	    Relaxed = (Request#request.settings)#http_options.relaxed,
	    NewState = State#state{mfa = 
				   {httpc_response, parse,
				    [State#state.max_header_size,
				     Relaxed]},
				   status_line = undefined,
				   headers = undefined,
				   body = undefined},
	    handle_info({httpc_handler, dummy, Data}, NewState);
	%% On a redirect or retry the current request becomes 
	%% obsolete and the manager will create a new request 
	%% with the same id as the current.
	{redirect, NewRequest, Data} ->
	    ?hcrt("handle response - redirect", [{new_request, NewRequest}, {data, Data}]), 
	    ok = httpc_manager:redirect_request(NewRequest, ProfileName),
	    handle_queue(State#state{request = undefined}, Data);
	{retry, TimeNewRequest, Data} ->
	    ?hcrt("handle response - retry", [{time_new_request, TimeNewRequest}, {data, Data}]), 
	    ok = httpc_manager:retry_request(TimeNewRequest, ProfileName),
	    handle_queue(State#state{request = undefined}, Data);
	{ok, Msg, Data} ->
	    ?hcrt("handle response - result ok", [{msg, Msg}, {data, Data}]), 
	    end_stream(StatusLine, Request),
	    NewState = answer_request(Request, Msg, State),
	    handle_queue(NewState, Data); 
	{stop, Msg} ->
	    ?hcrt("handle response - result stop", [{msg, Msg}]), 
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
    Cookies = http_cookie:cookies(Headers#http_response_h.other, 
				  Request#request.path, Host),
    httpc_manager:store_cookies(Cookies, Request#request.address,
				ProfileName).

%% This request could not be pipelined or used as sequential keept alive
%% queue
handle_queue(State = #state{status = close}, _) ->
    {stop, normal, State};

handle_queue(State = #state{status = keep_alive}, Data) ->
    handle_keep_alive_queue(State, Data);

handle_queue(State = #state{status = pipeline}, Data) ->
    handle_pipeline(State, Data).

handle_pipeline(State = 
		#state{status = pipeline, session = Session,
		       profile_name = ProfileName,
		       options = #options{pipeline_timeout = TimeOut}}, 
			       Data) ->
    case queue:out(State#state.pipeline) of
	{empty, _} ->
	    %% The server may choose too teminate an idle pipeline
	    %% in this case we want to receive the close message
	    %% at once and not when trying to pipeline the next
	    %% request.
	    http_transport:setopts(socket_type(Session#tcp_session.scheme), 
				   Session#tcp_session.socket, 
				   [{active, once}]),
	    %% If a pipeline that has been idle for some time is not
	    %% closed by the server, the client may want to close it.
	    NewState = activate_queue_timeout(TimeOut, State),
	    NewSession = Session#tcp_session{queue_length = 0},
	    httpc_manager:insert_session(NewSession, ProfileName),
	    %% Note mfa will be initilized when a new request 
	    %% arrives.
	    {noreply, 
	     NewState#state{request = undefined, 
			    mfa = undefined,
			    status_line = undefined,
			    headers = undefined,
			    body = undefined
			   }
	    };
	{{value, NextRequest}, Pipeline} ->    
	    case lists:member(NextRequest#request.id, 
			      State#state.canceled) of		
		true ->
		    %% See comment for handle_cast({cancel, RequestId})
		    {stop, normal, 
		     State#state{request = 
				 NextRequest#request{from = answer_sent}}};
		false ->
		    NewSession = 
			Session#tcp_session{queue_length =
					    %% Queue + current
					    queue:len(Pipeline) + 1},
		    httpc_manager:insert_session(NewSession, ProfileName),
		    Relaxed = 
			(NextRequest#request.settings)#http_options.relaxed,
		    NewState = 
			State#state{pipeline = Pipeline,
				    request = NextRequest,
				    mfa = {httpc_response, parse,
					   [State#state.max_header_size,
					    Relaxed]},
				    status_line = undefined,
				    headers = undefined,
				    body = undefined},
		    case Data of
			<<>> ->
			    http_transport:setopts(
			      socket_type(Session#tcp_session.scheme), 
			      Session#tcp_session.socket, 
			      [{active, once}]),
			    {noreply, NewState};
			_ ->
			    %% If we already received some bytes of
			    %% the next response
			    handle_info({httpc_handler, dummy, Data}, 
					NewState) 
		    end
	    end
    end.

handle_keep_alive_queue(State = #state{status = keep_alive,
				       session = Session,
				       profile_name = ProfileName,
				       options = #options{keep_alive_timeout 
							  = TimeOut}
				      }, 
			Data) ->
    case queue:out(State#state.keep_alive) of
	{empty, _} ->
	    %% The server may choose too terminate an idle keep_alive session
	    %% in this case we want to receive the close message
	    %% at once and not when trying to send the next
	    %% request.
	    http_transport:setopts(socket_type(Session#tcp_session.scheme), 
				   Session#tcp_session.socket, 
				   [{active, once}]),
	    %% If a keep_alive session has been idle for some time is not
	    %% closed by the server, the client may want to close it.
	    NewState = activate_queue_timeout(TimeOut, State),
	    NewSession = Session#tcp_session{queue_length = 0},
	    httpc_manager:insert_session(NewSession, ProfileName),
	    %% Note mfa will be initilized when a new request 
	    %% arrives.
	    {noreply, 
	     NewState#state{request = undefined, 
			    mfa = undefined,
			    status_line = undefined,
			    headers = undefined,
			    body = undefined
			   } 
	    };
	{{value, NextRequest}, KeepAlive} ->    
	    case lists:member(NextRequest#request.id, 
			      State#state.canceled) of		
		true ->
		    handle_keep_alive_queue(State#state{keep_alive = 
							KeepAlive}, Data);
		false ->
		    Relaxed = 
			(NextRequest#request.settings)#http_options.relaxed,
		    NewState =
			State#state{request = NextRequest,
				    keep_alive = KeepAlive,
				    mfa = {httpc_response, parse,
					   [State#state.max_header_size,
					    Relaxed]},
				    status_line = undefined,
				    headers = undefined,
				    body = undefined},
		    case Data of
			<<>> ->
			    http_transport:setopts(
			      socket_type(Session#tcp_session.scheme), 
			      Session#tcp_session.socket, [{active, once}]),
			    {noreply, NewState};
			_ ->
			    %% If we already received some bytes of
			    %% the next response
			    handle_info({httpc_handler, dummy, Data}, 
					NewState) 
		    end
	    end
    end.

call(Msg, Pid, Timeout) ->
    gen_server:call(Pid, Msg, Timeout).

cast(Msg, Pid) ->
    gen_server:cast(Pid, Msg).

case_insensitive_header(Str) when is_list(Str) ->
    http_util:to_lower(Str);
%% Might be undefined if server does not send such a header 
case_insensitive_header(Str) ->
    Str.

activate_request_timeout(State = #state{request = Request}) ->
    Time = (Request#request.settings)#http_options.timeout,
    case Time of
	infinity ->
	    State;
	_ ->
	    Ref = erlang:send_after(Time, self(), 
				    {timeout, Request#request.id}),
	    State#state
	      {timers = 
	       #timers{request_timers = 
		       [{Request#request.id, Ref}|
			(State#state.timers)#timers.request_timers]}}
    end.

activate_queue_timeout(infinity, State) ->
    State;
activate_queue_timeout(Time, State) ->
    Ref = erlang:send_after(Time, self(), timeout_queue),
    State#state{timers = #timers{queue_timer = Ref}}.


is_pipeline_enabled_client(#tcp_session{type = pipeline}) ->
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

is_keep_alive_connection(Headers, Session) ->
    (not ((Session#tcp_session.client_close) or  
	  httpc_response:is_server_closing(Headers))).

try_to_enable_pipeline_or_keep_alive(State = 
				    #state{session = Session, 
					   request = #request{method = Method},
					   status_line = {Version, _, _},
					   headers = Headers,
					   profile_name = ProfileName}) ->
    case (is_keep_alive_enabled_server(Version, Headers) andalso 
	  is_keep_alive_connection(Headers, Session)) of
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
		    State#state{status = keep_alive,
				session = 
				Session#tcp_session{type = keep_alive}}
	    end;
	false ->
	    State#state{status = close}
    end.

answer_request(Request, Msg, #state{timers = Timers} = State) ->    
    httpc_response:send(Request#request.from, Msg),
    RequestTimers = Timers#timers.request_timers,
    TimerRef =
	proplists:get_value(Request#request.id, RequestTimers, undefined),
    Timer = {Request#request.id, TimerRef},
    cancel_timer(TimerRef, {timeout, Request#request.id}),
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
		TimerRef =
		    proplists:get_value(Request#request.id, RequestTimers, 
					 undefined),
		cancel_timer(TimerRef, {timeout, Request#request.id}),
		State#state{timers = Timers#timers{request_timers =
					  lists:delete({Request#request.id,
							TimerRef},
						       RequestTimers)}};
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
    {ssl, Settings#http_options.ssl};
socket_type(http) ->
    ip_comm;
socket_type(https) ->
    {ssl, []}. %% Dummy value ok for ex setops that does not use this value

start_stream({_Version, _Code, _ReasonPhrase}, _Headers, #request{stream = none} = Request) ->
    ?hcrt("start stream - none", []), 
    {ok, Request};
start_stream({_Version, Code, _ReasonPhrase}, Headers, #request{stream = self} = Request) 
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
start_stream({_Version, Code, _ReasonPhrase}, _Headers, #request{stream = Filename} = Request)
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
		       once = once, session = Session} = State) ->
    http_transport:setopts(socket_type(Session#tcp_session.scheme), 
			   Session#tcp_session.socket, 
			   [{active, once}]),
    State#state{once = inactive};
next_body_chunk(#state{request = #request{stream = {self, once}}, 
		       once = inactive} = State) ->
    State; %% Wait for user to call stream_next
next_body_chunk(#state{session = Session} = State) ->
    http_transport:setopts(socket_type(Session#tcp_session.scheme), 
			   Session#tcp_session.socket, 
			   [{active, once}]),
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

