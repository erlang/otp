%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2014. All Rights Reserved.
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
         start_link/4,
         %% connect_and_send/2,
         send/2, 
         cancel/2,
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
          session,                   % #session{}
          status_line,               % {Version, StatusCode, ReasonPharse}
          headers,                   % #http_response_h{}
          body,                      % binary()
          mfa,                       % {Module, Function, Args}
          pipeline = queue:new(),    % queue:queue()
          keep_alive = queue:new(),  % queue:queue()
          status,   % undefined | new | pipeline | keep_alive | close | {ssl_tunnel, Request}
          canceled = [],             % [RequestId]
          max_header_size = nolimit, % nolimit | integer() 
          max_body_size = nolimit,   % nolimit | integer()
          options,                   % #options{}
          timers = #timers{},        % #timers{}
          profile_name,              % atom() - id of httpc_manager process.
          once = inactive            % inactive | once
         }).


%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link(Request, Options, ProfileName) -> {ok, Pid}
%%
%%      Request = #request{}
%%      Options =  #options{} 
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

start_link(Parent, Request, Options, ProfileName) ->
    {ok, proc_lib:start_link(?MODULE, init, [[Parent, Request, Options,
                                              ProfileName]])}.

%%--------------------------------------------------------------------
%% Function: send(Request, Pid) -> ok 
%%      Request = #request{}
%%      Pid = pid() - the pid of the http-request handler process.
%%
%% Description: Uses this handlers session to send a request. Intended
%% to be called by the httpc manager process.
%%--------------------------------------------------------------------
send(Request, Pid) ->
    call(Request, Pid, 5000).


%%--------------------------------------------------------------------
%% Function: cancel(RequestId, Pid) -> ok
%%      RequestId = ref()
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
    try 
	call(info, Pid)
    catch
	_:_ ->
	    []
    end.

%%--------------------------------------------------------------------
%% Function: stream(BodyPart, Request, Code) -> _
%%      BodyPart = binary()
%%      Request = #request{}
%%      Code = integer()
%%
%% Description: Stream the HTTP body to the caller process (client) 
%%              or to a file. Note that the data that has been stream
%%              does not have to be saved. (We do not want to use up
%%              memory in vain.)
%%--------------------------------------------------------------------
%% Request should not be streamed
stream(BodyPart, #request{stream = none} = Request, _) ->
    ?hcrt("stream - none", []),
    {BodyPart, Request};

%% Stream to caller
stream(BodyPart, #request{stream = Self} = Request, Code) 
  when ((Code =:= 200) orelse  (Code =:= 206)) andalso 
       ((Self =:= self) orelse (Self =:= {self, once})) ->
    ?hcrt("stream - self", [{stream, Self}, {code, Code}]),
    httpc_response:send(Request#request.from, 
                        {Request#request.id, stream, BodyPart}),
    {<<>>, Request};

%% Stream to file
%% This has been moved to start_stream/3
%% We keep this for backward compatibillity...
stream(BodyPart, #request{stream = Filename} = Request, Code)
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
stream(BodyPart, #request{stream = Fd} = Request, Code)  
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
%%      Options =  #options{} 
%%      ProfileName = atom() - id of httpc manager process
%%
%% Description: Initiates the httpc_handler process 
%%
%% Note: The init function may not fail, that will kill the
%% httpc_manager process. We could make the httpc_manager more comlex
%% but we do not want that so errors will be handled by the process
%% sending an init_error message to itself.
%%--------------------------------------------------------------------
init([Parent, Request, Options, ProfileName]) ->
    process_flag(trap_exit, true),

    %% Do not let initial tcp-connection block the manager-process
    proc_lib:init_ack(Parent, self()),
    handle_verbose(Options#options.verbose),
    ProxyOptions = handle_proxy_options(Request#request.scheme, Options),
    Address = handle_proxy(Request#request.address, ProxyOptions),
    {ok, State} =
        %% #state.once should initially be 'inactive' because we
        %% activate the socket at first regardless of the state.
        case {Address /= Request#request.address, Request#request.scheme} of
            {true, https} ->
                connect_and_send_upgrade_request(Address, Request,
                                                 #state{options = Options,
                                                        profile_name = ProfileName});
            {_, _} ->
                connect_and_send_first_request(Address, Request,
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
handle_call(#request{address = Addr} = Request, _, 
            #state{status  = Status,
                   session = #session{type = pipeline} = Session,
                   timers  = Timers,
                   options = #options{proxy = Proxy} = _Options, 
                   profile_name = ProfileName} = State0)
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
            State1 =
                activate_request_timeout(State0#state{request = Request}),

            ClientClose = 
                httpc_request:is_client_closing(Request#request.headers),

            case State0#state.request of
                #request{} = OldRequest -> %% Old request not yet finished
                    ?hcrd("old request still not finished", []),
                    %% Make sure to use the new value of timers in state
                    NewTimers = State1#state.timers,
                    NewPipeline = queue:in(Request, State1#state.pipeline),
                    NewSession  = 
                        Session#session{queue_length = 
                                        %% Queue + current
                                        queue:len(NewPipeline) + 1,
                                        client_close = ClientClose},
                    insert_session(NewSession, ProfileName),
                    ?hcrd("session updated", []),
                    {reply, ok, State1#state{
				  request = OldRequest,
				  pipeline = NewPipeline,
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
                    NewTimers = Timers#timers{queue_timer = undefined}, 
                    ?hcrd("session created", []),
		    State = init_wait_for_response_state(Request, State1#state{session = NewSession,
								      timers = NewTimers}),
                    {reply, ok, State}
            end;
        {error, Reason} ->
            ?hcri("failed sending request", [{reason, Reason}]),
            {reply, {pipeline_failed, Reason}, State0}
    end;

handle_call(#request{address = Addr} = Request, _, 
            #state{status  = Status,
                   session = #session{type = keep_alive} = Session,
                   timers  = Timers,
                   options = #options{proxy = Proxy} = _Options,
                   profile_name = ProfileName} = State0)
  when Status =/= undefined ->
    
    ?hcrv("new request on a keep-alive session", 
          [{request, Request}, 
           {profile, ProfileName}, 
           {status,  Status}]),

    ClientClose = httpc_request:is_client_closing(Request#request.headers),

    case State0#state.request of
	#request{} -> %% Old request not yet finished
	    %% Make sure to use the new value of timers in state
	    ?hcrd("old request still not finished", []),
	    NewKeepAlive = queue:in(Request, State0#state.keep_alive),
	    NewSession   =
		Session#session{queue_length =
				    %% Queue + current
				    queue:len(NewKeepAlive) + 1,
				client_close = ClientClose},
	    insert_session(NewSession, ProfileName),
	    ?hcrd("session updated", []),
	    {reply, ok, State0#state{keep_alive = NewKeepAlive,
				    session    = NewSession}};
	undefined ->
	    %% Note: tcp-message receiving has already been
	    %% activated by handle_pipeline/2.
	    ?hcrd("no current request", []),
	    cancel_timer(Timers#timers.queue_timer,
			 timeout_queue),
	    Address = handle_proxy(Addr, Proxy),
	    case httpc_request:send(Address, Session, Request) of
		ok ->
		    ?hcrd("request sent", []),

		    %% Activate the request time out for the new request
		    State1 =
			activate_request_timeout(State0#state{request = Request}),
		    NewTimers = State1#state.timers,
		    NewSession =
			Session#session{queue_length = 1,
					client_close = ClientClose},
		    insert_session(NewSession, ProfileName),
		    State = init_wait_for_response_state(Request, State1#state{session = NewSession,
								      timers = NewTimers}),
		    {reply, ok, State};
		{error, Reason} ->
		    ?hcri("failed sending request", [{reason, Reason}]),
		    {reply, {request_failed, Reason}, State0}
	    end
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
    {stop, normal, 
     State#state{canceled = [RequestId | Canceled],
                 request  = Request#request{from = answer_sent}}};
handle_cast({cancel, RequestId},
            #state{profile_name = ProfileName,
                   request      = #request{id = CurrId},
                   canceled     = Canceled} = State) ->
    ?hcrv("cancel", [{request_id, RequestId},
                     {curr_req_id, CurrId},
                     {profile, ProfileName},
                     {canceled,   Canceled}]),
    {noreply, State#state{canceled = [RequestId | Canceled]}};

handle_cast(stream_next, #state{session = Session} = State) ->
    activate_once(Session), 
    %% Inactivate the #state.once here because we don't want
    %% next_body_chunk/1 to activate the socket twice.
    {noreply, State#state{once = inactive}}.


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
            {Module, decode_size,
             [TotalChunk, HexList,
              {MaxBodySize, BodySoFar, AccLength, MaxHeaderSize}]}
              when BodySoFar =/= <<>> ->
                ?hcrd("data processed - decode_size", []),
                %% The response body is chunk-encoded. Steal decoded
                %% chunks as much as possible to stream.
                {_, Code, _} = StatusLine,
                {NewBody, NewRequest} = stream(BodySoFar, Request, Code),
                NewState = next_body_chunk(State),
                NewMFA   = {Module, decode_size,
                            [TotalChunk, HexList,
                             {MaxBodySize, NewBody, AccLength, MaxHeaderSize}]},
                {noreply, NewState#state{mfa     = NewMFA,
                                         request = NewRequest}};
            {Module, decode_data,
             [ChunkSize, TotalChunk,
              {MaxBodySize, BodySoFar, AccLength, MaxHeaderSize}]}
              when TotalChunk =/= <<>> orelse BodySoFar =/= <<>> ->
                ?hcrd("data processed - decode_data", []),
                %% The response body is chunk-encoded. Steal decoded
                %% chunks as much as possible to stream.
                ChunkSizeToSteal = min(ChunkSize, byte_size(TotalChunk)),
                <<StolenChunk:ChunkSizeToSteal/binary, NewTotalChunk/binary>> = TotalChunk,
                StolenBody   = <<BodySoFar/binary, StolenChunk/binary>>,
                NewChunkSize = ChunkSize - ChunkSizeToSteal,
                {_, Code, _} = StatusLine,

                {NewBody, NewRequest} = stream(StolenBody, Request, Code),
                NewState = next_body_chunk(State),
                NewMFA   = {Module, decode_data,
                            [NewChunkSize, NewTotalChunk,
                             {MaxBodySize, NewBody, AccLength, MaxHeaderSize}]},
                {noreply, NewState#state{mfa     = NewMFA,
                                         request = NewRequest}};
            NewMFA ->
                ?hcrd("data processed - new mfa", []),
                activate_once(Session),
                {noreply, State#state{mfa = NewMFA}}
        catch
            _:_Reason ->
                ?hcrd("data processing exit", [{exit, _Reason}]),
                ClientReason = {could_not_parse_as_http, Data}, 
                ClientErrMsg = httpc_response:error(Request, ClientReason),
                NewState     = answer_request(Request, ClientErrMsg, State),
                {stop, normal, NewState}
        end,
    ?hcri("data processed", [{final_result, FinalResult}]),
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
            #state{request      = #request{id = RequestId} = Request,
                   canceled     = Canceled,
                   profile_name = ProfileName} = State) ->
    ?hcri("timeout of current request", [{id, RequestId}]),
    httpc_response:send(Request#request.from, 
                        httpc_response:error(Request, timeout)),
    httpc_manager:request_done(RequestId, ProfileName),
    ?hcrv("response (timeout) sent - now terminate", []),
    {stop, normal, 
     State#state{request  = Request#request{from = answer_sent},
                 canceled = [RequestId | Canceled]}};

handle_info({timeout, RequestId}, 
            #state{canceled     = Canceled,
                   profile_name = ProfileName} = State) ->
    ?hcri("timeout", [{id, RequestId}]),
    Filter = 
        fun(#request{id = Id, from = From} = Request) when Id =:= RequestId ->
                ?hcrv("found request", [{id, Id}, {from, From}]),
                %% Notify the owner
                httpc_response:send(From, 
                                    httpc_response:error(Request, timeout)),
                httpc_manager:request_done(RequestId, ProfileName),
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

%% Timing was such as the queue_timeout was not canceled!
handle_info(timeout_queue, #state{timers = Timers} = State) ->
    {noreply, State#state{timers = 
                          Timers#timers{queue_timer = undefined}}};

%% Setting up the connection to the server somehow failed. 
handle_info({init_error, Tag, ClientErrMsg},
            State = #state{request = Request}) ->
    ?hcrv("init error", [{tag, Tag}, {client_error, ClientErrMsg}]),
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
                 pipeline     = Pipeline,
                 keep_alive   = KeepAlive} = State) ->  
    ?hcrt("terminate(normal) - remote close", 
          [{id, Id}, {profile, ProfileName}]),

    %% Clobber session
    (catch httpc_manager:delete_session(Id, ProfileName)),

    maybe_retry_queue(Pipeline, State),
    maybe_retry_queue(KeepAlive, State),

    %% Cancel timers
    cancel_timers(Timers),

    %% Maybe deliver answers to requests
    deliver_answer(Request),

    %% And, just in case, close our side (**really** overkill)
    http_transport:close(SocketType, Socket);

terminate(Reason, #state{session = #session{id          = Id,
                                            socket      = Socket, 
                                            socket_type = SocketType},
                    request      = undefined,
                    profile_name = ProfileName,
                    timers       = Timers,
                    pipeline     = Pipeline,
                    keep_alive   = KeepAlive} = State) -> 
    ?hcrt("terminate", 
          [{id, Id}, {profile, ProfileName}, {reason, Reason}]),

    %% Clobber session
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

deliver_answer(#request{id = Id, from = From} = Request) 
  when is_pid(From) ->
    Response = httpc_response:error(Request, socket_closed_remotely),
    ?hcrd("deliver answer", [{id, Id}, {from, From}, {response, Response}]),
    httpc_response:send(From, Response);
deliver_answer(Request) ->
    ?hcrd("skip deliver answer", [{request, Request}]),
    ok.


%%--------------------------------------------------------------------
%% Func: code_change(_OldVsn, State, Extra) -> {ok, NewState}
%% Purpose: Convert process state when code is changed
%%--------------------------------------------------------------------

code_change(_, 
            #state{session      = OldSession, 
                   profile_name = ProfileName} = State, 
            upgrade_from_pre_5_8_1) ->
    case OldSession of
        {session, 
         Id, ClientClose, Scheme, Socket, SocketType, QueueLen, Type} ->
            NewSession = #session{id           = Id, 
                                  client_close = ClientClose, 
                                  scheme       = Scheme, 
                                  socket       = Socket, 
                                  socket_type  = SocketType,
                                  queue_length = QueueLen, 
                                  type         = Type}, 
            insert_session(NewSession, ProfileName), 
            {ok, State#state{session = NewSession}};
        _ -> 
            {ok, State}
    end;

code_change(_, 
            #state{session      = OldSession, 
                   profile_name = ProfileName} = State, 
            downgrade_to_pre_5_8_1) ->
    case OldSession of
        #session{id           = Id, 
                 client_close = ClientClose, 
                 scheme       = Scheme, 
                 socket       = Socket, 
                 socket_type  = SocketType,
                 queue_length = QueueLen, 
                 type         = Type} ->
            NewSession = {session, 
                          Id, ClientClose, Scheme, Socket, SocketType, 
                          QueueLen, Type},
            insert_session(NewSession, ProfileName), 
            {ok, State#state{session = NewSession}};
        _ -> 
            {ok, State}
    end;

code_change(_, State, _) ->
    {ok, State}.


%% new_http_options({http_options, TimeOut, AutoRedirect, SslOpts,
%%                Auth, Relaxed}) ->
%%     {http_options, "HTTP/1.1", TimeOut, AutoRedirect, SslOpts,
%%      Auth, Relaxed}.

%% old_http_options({http_options, _, TimeOut, AutoRedirect,
%%                SslOpts, Auth, Relaxed}) ->
%%     {http_options, TimeOut, AutoRedirect, SslOpts, Auth, Relaxed}.

%% new_queue(Queue, Fun) ->
%%     List = queue:to_list(Queue),
%%     NewList = 
%%      lists:map(fun(Request) ->
%%                        Settings = 
%%                            Fun(Request#request.settings),
%%                        Request#request{settings = Settings}
%%                end, List),
%%     queue:from_list(NewList).
    

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
            case http_transport:connect(SocketType, 
                                        ToAddress, Opts3, Timeout) of
                {error, Reason6} ->
                    Opts4 = [inet | Opts2], 
                    case http_transport:connect(SocketType, 
                                                ToAddress, Opts4, Timeout) of
                        {error, Reason4} ->
                            {error, {failed_connect, 
                                     [{to_address, ToAddress}, 
                                      {inet6, Opts3, Reason6}, 
                                      {inet,  Opts4, Reason4}]}};
                        OK ->
                            OK
                    end;
                OK ->
                    OK
            end;
        _ ->
            Opts3 = [IpFamily | Opts2], 
            case http_transport:connect(SocketType, ToAddress, Opts3, Timeout) of
                {error, Reason} ->
                    {error, {failed_connect, [{to_address, ToAddress}, 
                                              {IpFamily, Opts3, Reason}]}};
                Else ->
                    Else
            end
    end.

connect_and_send_first_request(Address, Request, #state{options = Options} = State) ->
    SocketType  = socket_type(Request),
    ConnTimeout = (Request#request.settings)#http_options.connect_timeout,
    ?hcri("connect",
          [{address, Address}, {request, Request}, {options, Options}]),
    case connect(SocketType, Address, Options, ConnTimeout) of
        {ok, Socket} ->
            ClientClose =
                        httpc_request:is_client_closing(
                          Request#request.headers),
            SessionType = httpc_manager:session_type(Options),
            SocketType  = socket_type(Request),
            Session = #session{id = {Request#request.address, self()},
                               scheme = Request#request.scheme,
                               socket = Socket,
                               socket_type = SocketType,
                               client_close = ClientClose,
                               type = SessionType},
            ?hcri("connected - now send first request", [{socket, Socket}]),

            case httpc_request:send(Address, Session, Request) of
                ok ->
                    ?hcri("first request sent", []),
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
                    self() ! {init_error, error_sending,
                              httpc_response:error(Request, Reason)},
                    {ok, State#state{request = Request,
                                     session =
                                         #session{socket = Socket}}}
            end;
        {error, Reason} ->
            self() ! {init_error, error_connecting,
                      httpc_response:error(Request, Reason)},
            {ok, State#state{request = Request}}
    end.

connect_and_send_upgrade_request(Address, Request, #state{options = Options} = State) ->
    ConnTimeout = (Request#request.settings)#http_options.connect_timeout,
    SocketType = ip_comm,
    case connect(SocketType, Address, Options, ConnTimeout) of
        {ok, Socket} ->
	    SessionType = httpc_manager:session_type(Options),
	    Session = #session{socket = Socket, 
			       socket_type = SocketType,
			       id = {Request#request.address, self()},
                               scheme = http,
                               client_close = false,
			       type = SessionType},
	    ErrorHandler = 
		fun(ERequest, EState, EReason) ->
			self() ! {init_error, error_sending,
				  httpc_response:error(ERequest, EReason)},
			{ok, EState#state{request = ERequest}} end,
	    tls_tunnel(Address, Request, State#state{session = Session}, ErrorHandler);
	{error, Reason} ->
	    self() ! {init_error, error_connecting,
		      httpc_response:error(Request, Reason)},
	    {ok, State#state{request = Request}}
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
handle_http_msg({ChunkedHeaders, Body},
                #state{status_line = {_, Code, _}, headers = Headers} = State) ->
    ?hcrt("handle_http_msg", 
	  [{chunked_headers, ChunkedHeaders}, {headers, Headers}]),
    NewHeaders = http_chunk:handle_headers(Headers, ChunkedHeaders),
    {NewBody, NewRequest} = stream(Body, State#state.request, Code),
    handle_response(State#state{headers = NewHeaders,
                                body    = NewBody,
                                request = NewRequest});
handle_http_msg(Body, #state{status_line = {_,Code, _}} = State) ->
    ?hcrt("handle_http_msg", [{code, Code}]),
    {NewBody, NewRequest} = stream(Body, State#state.request, Code),
    handle_response(State#state{body = NewBody, request = NewRequest}).

handle_http_body(_, #state{status = {ssl_tunnel, _},
			   status_line = {_,200, _}} = State) ->
    tls_upgrade(State);  

handle_http_body(_, #state{status = {ssl_tunnel, Request},
			   status_line = StatusLine} = State) ->
    ClientErrMsg = httpc_response:error(Request,{could_no_establish_ssh_tunnel, StatusLine}),
    NewState     = answer_request(Request, ClientErrMsg, State),
    {stop, normal, NewState};

handle_http_body(<<>>, #state{status_line = {_,304, _}} = State) ->
    ?hcrt("handle_http_body - 304", []),
    handle_response(State#state{body = <<>>});

handle_http_body(<<>>, #state{status_line = {_,204, _}} = State) ->
    ?hcrt("handle_http_body - 204", []),
    handle_response(State#state{body = <<>>});

handle_http_body(<<>>, #state{request = #request{method = head}} = State) ->
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
				   State#state.max_header_size) of
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
                    case Body of
                        <<>> ->
                           handle_response(State#state{headers = NewHeaders,
                                                body    = NewBody});
                        _ ->
                           {NewBody2, NewRequest} =
                                stream(NewBody, Request, Code),
                           handle_response(State#state{headers = NewHeaders,
                                       body    = NewBody2})
                    end
	    end;
        Enc when Enc =:= "identity"; Enc =:= undefined ->
            ?hcrt("handle_http_body - identity", []),
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
            end;
        Encoding when is_list(Encoding) ->
            ?hcrt("handle_http_body - other", [{encoding, Encoding}]),
            NewState = answer_request(Request,
                                      httpc_response:error(Request,
                                                           unknown_encoding),
                                      State),
            {stop, normal, NewState}
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
	    stream_remaining_body(Body, Request, StatusLine),
	    end_stream(StatusLine, Request),
	    NewState = maybe_send_answer(Request, Msg, State),
	    handle_queue(NewState, Data); 
	{stop, Msg} ->
	    ?hcrd("handle response - stop", [{msg, Msg}]),
	    end_stream(StatusLine, Request),
	    NewState = maybe_send_answer(Request, Msg, State),
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

%% This request could not be pipelined or used as sequential keep alive
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
		       options      = #options{pipeline_timeout = TimeOut}} = State,
		Data) ->

    ?hcrd("handle pipeline", [{profile, ProfileName}, 
			      {session, Session},
			      {timeout, TimeOut}]),

    case queue:out(State#state.pipeline) of
	{empty, _} ->
	    ?hcrd("pipeline queue empty", []),
	    handle_empty_queue(Session, ProfileName, TimeOut, State);
	{{value, NextRequest}, Pipeline} ->    
	    ?hcrd("pipeline queue non-empty", []),
	    case lists:member(NextRequest#request.id, 
			      State#state.canceled) of		
		true ->
		    ?hcrv("next request had been cancelled", []),
		    %% See comment for handle_cast({cancel, RequestId})
		    {stop, normal, 
		     State#state{request = 
				 NextRequest#request{from = answer_sent},
				 pipeline = Pipeline}};
		false ->
		    ?hcrv("next request", [{request, NextRequest}]),
		    NewSession = 
			Session#session{queue_length =
					%% Queue + current
					queue:len(Pipeline) + 1},
		    receive_response(NextRequest,
				     NewSession, Data,
				     State#state{pipeline = Pipeline})
	    end
    end.

handle_keep_alive_queue(#state{status       = keep_alive,
			       session      = Session,
			       profile_name = ProfileName,
			       options      = #options{keep_alive_timeout = TimeOut}} = State,
			Data) ->

    ?hcrd("handle keep_alive", [{profile, ProfileName}, 
				{session, Session},
				{timeout, TimeOut}]),

    case queue:out(State#state.keep_alive) of
	{empty, _} ->
	    ?hcrd("keep_alive queue empty", []),
	    handle_empty_queue(Session, ProfileName, TimeOut, State);
	{{value, NextRequest}, KeepAlive} ->    
	    ?hcrd("keep_alive queue non-empty", []),
	    case lists:member(NextRequest#request.id, 
			      State#state.canceled) of		
		true ->
		    ?hcrv("next request has already been canceled", []),
		    handle_keep_alive_queue(
		      State#state{keep_alive = KeepAlive}, Data);
		false ->
		    ?hcrv("next request", [{request, NextRequest}]),
		    #request{address = Address} = NextRequest,
		    case httpc_request:send(Address, Session, NextRequest) of
			ok ->
			    receive_response(NextRequest,
					     Session, <<>>,
					     State#state{keep_alive = KeepAlive});
			{error, Reason} ->
			    {reply, {keep_alive_failed, Reason}, State}
		    end
	    end
    end.

handle_empty_queue(Session, ProfileName, TimeOut, State) ->
    %% The server may choose too terminate an idle pipline| keep_alive session
    %% in this case we want to receive the close message
    %% at once and not when trying to send the next
    %% request.
    activate_once(Session),
    %% If a pipline | keep_alive session has been idle for some time is not
    %% closed by the server, the client may want to close it.
    NewState = activate_queue_timeout(TimeOut, State),
    update_session(ProfileName, Session, #session.queue_length, 0),
    %% Note mfa will be initilized when a new request
    %% arrives.
    {noreply,
     NewState#state{request     = undefined,
		    mfa         = undefined,
		    status_line = undefined,
		    headers     = undefined,
		    body        = undefined
			   }
    }.

receive_response(Request, Session, Data, State) ->
    NewState = init_wait_for_response_state(Request, State),
    gather_data(Data, Session, NewState).

init_wait_for_response_state(Request, State) ->
    Relaxed =
	(Request#request.settings)#http_options.relaxed,
    MFA     = {httpc_response, parse,
	       [State#state.max_header_size, Relaxed]},
    State#state{request     = Request,
		mfa         = MFA,
		status_line = undefined,
		headers     = undefined,
		body        = undefined}.

gather_data(<<>>, Session, State) ->
    activate_once(Session),
    {noreply, State};
gather_data(Data, _, State) ->
    %% If we already received some bytes of
    %% the next response
    handle_info({httpc_handler, dummy, Data}, State).

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
		    insert_session(Session, ProfileName),
		    State#state{status = pipeline};
		false ->
		    insert_session(Session, ProfileName),
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
	       #state{session      = Session, 
		      timers       = Timers, 
		      profile_name = ProfileName} = State) -> 
    ?hcrt("answer request", [{request, Request}, {msg, Msg}]),
    httpc_response:send(From, Msg),
    RequestTimers = Timers#timers.request_timers,
    TimerRef =
	proplists:get_value(RequestId, RequestTimers, undefined),
    Timer = {RequestId, TimerRef},
    cancel_timer(TimerRef, {timeout, Request#request.id}),
    httpc_manager:request_done(RequestId, ProfileName),
    NewSession = maybe_make_session_available(ProfileName, Session), 
    Timers2 = Timers#timers{request_timers = lists:delete(Timer, 
							  RequestTimers)}, 
    State#state{request = Request#request{from = answer_sent},
		session = NewSession, 
		timers  = Timers2}.

maybe_make_session_available(ProfileName, 
			     #session{available = false} = Session) ->
    update_session(ProfileName, Session, #session.available, true),
    Session#session{available = true};
maybe_make_session_available(_ProfileName, Session) ->
    Session.
    
cancel_timers(#timers{request_timers = ReqTmrs, queue_timer = QTmr}) ->
    cancel_timer(QTmr, timeout_queue),
    CancelTimer = fun({_, Timer}) -> cancel_timer(Timer, timeout) end, 
    lists:foreach(CancelTimer, ReqTmrs).

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

handle_proxy_options(https, #options{https_proxy = {HttpsProxy, _} = HttpsProxyOpt}) when 
      HttpsProxy =/= undefined ->
    HttpsProxyOpt;
handle_proxy_options(_, #options{proxy = Proxy}) ->
    Proxy.

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

stream_remaining_body(<<>>, _, _) ->
    ok;
stream_remaining_body(Body, Request, {_, Code, _}) ->
    stream(Body, Request, Code).

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



send_raw(#session{socket = Socket, socket_type = SocketType}, 
	 {ProcessBody, Acc}) when is_function(ProcessBody, 1) ->
    ?hcrt("send raw", [{acc, Acc}]),
    send_raw(SocketType, Socket, ProcessBody, Acc);
send_raw(#session{socket = Socket, socket_type = SocketType}, Body) ->
    http_transport:send(SocketType, Socket, Body).

send_raw(SocketType, Socket, ProcessBody, Acc) ->
    case ProcessBody(Acc) of
        eof ->
            ok;
        {ok, Data, NewAcc} ->
            DataBin = iolist_to_binary(Data),
            ?hcrd("send", [{data, DataBin}]),
            case http_transport:send(SocketType, Socket, DataBin) of
                ok ->
                    send_raw(SocketType, Socket, ProcessBody, NewAcc);
                Error ->
                    Error
            end
    end.

tls_tunnel(Address, Request, #state{session = #session{socket = Socket, 
						       socket_type = SocketType} = Session} = State, 
	   ErrorHandler) ->
    UpgradeRequest = tls_tunnel_request(Request), 
    case httpc_request:send(Address, Session, UpgradeRequest) of
	ok -> 
	    TmpState = State#state{request = UpgradeRequest,
				   %%  session = Session,
				   mfa = init_mfa(UpgradeRequest, State),
				   status_line =
				       init_status_line(UpgradeRequest),
				   headers = undefined,
				   body = undefined},
	    http_transport:setopts(SocketType,
				   Socket, [{active, once}]),
	    NewState = activate_request_timeout(TmpState),
	    {ok, NewState#state{status = {ssl_tunnel, Request}}};
	{error, Reason} ->
	   ErrorHandler(Request, State, Reason)
    end.

tls_tunnel_request(#request{headers = Headers, 
			     settings = Options,
			     address =  {Host, Port}= Adress,
			     ipv6_host_with_brackets = IPV6}) ->
    
    URI = Host ++":" ++ integer_to_list(Port),
    
    #request{
       id =  make_ref(),
       from = self(),
       scheme = http, %% Use tcp-first and then upgrade!
       address = Adress,
       path = URI,
       pquery  = "",
       method = connect,
       headers = #http_request_h{host = host_header(Headers, URI),
				 te = "",
				 pragma = "no-cache",
				 other = [{"Proxy-Connection", " Keep-Alive"}]},
       settings = Options,
       abs_uri = URI,
       stream = false,
       userinfo = "",
       headers_as_is = [],
       started  = http_util:timestamp(),
       ipv6_host_with_brackets = IPV6       
      }.

host_header(#http_request_h{host = Host}, _) ->
    Host;

%% Handles headers_as_is
host_header(_, URI) ->
    {ok, {_, _, Host, _, _, _}} =  http_uri:parse(URI),
    Host.

tls_upgrade(#state{status = 
		       {ssl_tunnel, 
			#request{settings = 
				     #http_options{ssl = {_, TLSOptions} = SocketType}} = Request},
		   session = #session{socket = TCPSocket} = Session0,
		   options = Options} = State) ->

    case ssl:connect(TCPSocket, TLSOptions) of
	{ok, TLSSocket} ->
	    Address = Request#request.address,
	    ClientClose = httpc_request:is_client_closing(Request#request.headers),
	    SessionType = httpc_manager:session_type(Options),
	    Session = Session0#session{
			scheme = https,
			socket = TLSSocket, 
			socket_type = SocketType,
			type = SessionType,
			client_close = ClientClose},
	    httpc_request:send(Address, Session, Request), 
	    http_transport:setopts(SocketType, TLSSocket, [{active, once}]),
	    NewState = State#state{session = Session,
				   request = Request,
				   mfa = init_mfa(Request, State),
				   status_line =
				       init_status_line(Request),
				   headers = undefined,
				   body = undefined,
				   status = new
				  },
	    {noreply, activate_request_timeout(NewState)};
	{error, _Reason} ->
	    {stop, normal, State#state{request = Request}}
    end.

%% ---------------------------------------------------------------------
%% Session wrappers
%% ---------------------------------------------------------------------

insert_session(Session, ProfileName) ->
    httpc_manager:insert_session(Session, ProfileName).


update_session(ProfileName, #session{id = SessionId} = Session, Pos, Value) ->
    try
	begin
	    httpc_manager:update_session(ProfileName, SessionId, Pos, Value)
	end
    catch
	error:undef -> % This could happen during code upgrade
	    Session2 = erlang:setelement(Pos, Session, Value),
	    insert_session(Session2, ProfileName);
	  T:E ->
            error_logger:error_msg("Failed updating session: "
                                   "~n   ProfileName: ~p"
                                   "~n   SessionId:   ~p"
                                   "~n   Pos:         ~p"
                                   "~n   Value:       ~p"
                                   "~nwhen"
                                   "~n   Session (db) info: ~p"
                                   "~n   Session (db):      ~p"
                                   "~n   Session (record):  ~p"
                                   "~n   T: ~p"
                                   "~n   E: ~p", 
                                   [ProfileName, SessionId, Pos, Value, 
                                    (catch httpc_manager:which_session_info(ProfileName)), 
                                    Session, 
                                    (catch httpc_manager:lookup_session(SessionId, ProfileName)),
                                    T, E]),
            exit({failed_updating_session, 
                  [{profile,    ProfileName}, 
                   {session_id, SessionId}, 
                   {pos,        Pos}, 
                   {value,      Value}, 
                   {etype,      T}, 
                   {error,      E}, 
                   {stacktrace, erlang:get_stacktrace()}]})	    
    end.


%% ---------------------------------------------------------------------

call(Msg, Pid) ->
    Timeout = infinity,
    call(Msg, Pid, Timeout).
call(Msg, Pid, Timeout) ->
    gen_server:call(Pid, Msg, Timeout).

cast(Msg, Pid) ->
    gen_server:cast(Pid, Msg).

t() ->
    http_util:timestamp().
