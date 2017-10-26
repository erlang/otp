%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
%%
%% Description: Implements a request handler process for the HTTP server.  
%% 

-module(httpd_request_handler).

-behaviour(gen_server).

%% Application internal API
-export([start_link/2, start_link/3, socket_ownership_transfered/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-include("httpd.hrl").
-include("http_internal.hrl").
-include("httpd_internal.hrl").

-define(HANDSHAKE_TIMEOUT, 5000).

-record(state, {mod,     %% #mod{}
		manager, %% pid()
		status,  %% accept | busy | blocked
		mfa,     %% {Module, Function, Args} 
		max_keep_alive_request = infinity, %% integer() | infinity
		response_sent = false :: boolean(),
		timeout,   %% infinity | integer() > 0
		timer      :: 'undefined' | reference(), % Request timer
		headers,   %% #http_request_h{}
		body,      %% binary()
		data,      %% The total data received in bits, checked after 10s
		byte_limit, %% Bit limit per second before kick out
                chunk 
	       }).

%%====================================================================
%% Application internal API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start() -> {ok, Pid} | ignore | {error,Error}
%% Description: Starts a httpd-request handler process. Intended to be
%% called by the httpd acceptor process.
%%--------------------------------------------------------------------
start_link(Manager, ConfigDB) ->
    start_link(Manager, ConfigDB, 15000).
start_link(Manager, ConfigDB, AcceptTimeout) ->
    proc_lib:start_link(?MODULE, init, [[Manager, ConfigDB,AcceptTimeout]]).


%%--------------------------------------------------------------------
%% socket_ownership_transfered(Pid, SocketType, Socket) -> void()
%%
%% Pid = pid()
%% SocketType = ip_comm | ssl 
%% Socket = socket()
%%
%% Description: Send a message to the request handler process
%% confirming that the socket ownership has now sucssesfully been
%% transfered to it. Intended to be called by the httpd acceptor
%% process.
%%--------------------------------------------------------------------
socket_ownership_transfered(Pid, SocketType, Socket) ->
    Pid ! {socket_ownership_transfered, SocketType, Socket}.

%%--------------------------------------------------------------------
%% Function: init(Args) -> _
%%
%% Description: Initiates the server. Obs special init that uses 
%% gen_server:enter_loop/3. This is used instead of the normal
%% gen_server callback init, as a more complex init than the
%% gen_server provides is needed. 
%%--------------------------------------------------------------------
init([Manager, ConfigDB, AcceptTimeout]) ->
    process_flag(trap_exit, true),
    %% Make sure this process terminates if the httpd manager process
    %% should die!
    %%link(Manager), 
    %% At this point the function httpd_request_handler:start/2 will return.
    proc_lib:init_ack({ok, self()}),
    
    {SocketType, Socket} = await_socket_ownership_transfer(AcceptTimeout),
    
    %%Timeout value is in seconds we want it in milliseconds
    KeepAliveTimeOut = 1000 * httpd_util:lookup(ConfigDB, keep_alive_timeout, 150),
    
    case http_transport:negotiate(SocketType, Socket, ?HANDSHAKE_TIMEOUT) of
	{error, Error} ->
	    exit({shutdown, Error}); %% Can be 'normal'.
	ok ->
	    continue_init(Manager, ConfigDB, SocketType, Socket, KeepAliveTimeOut)
    end.

continue_init(Manager, ConfigDB, SocketType, Socket, TimeOut) ->
    Resolve = http_transport:resolve(),
    
    Peername = httpd_socket:peername(SocketType, Socket),
    InitData = #init_data{peername = Peername, resolve = Resolve},
    Mod = #mod{config_db = ConfigDB,
               socket_type = SocketType,
               socket = Socket,
               init_data = InitData},
    
    MaxHeaderSize = max_header_size(ConfigDB), 
    MaxURISize    = max_uri_size(ConfigDB), 
    NrOfRequest   = max_keep_alive_request(ConfigDB), 
    MaxContentLen = max_content_length(ConfigDB),
    Customize = customize(ConfigDB),
    MaxChunk = max_client_body_chunk(ConfigDB),
    
    {_, Status} = httpd_manager:new_connection(Manager),
    
    MFA = {httpd_request, parse, [[{max_uri, MaxURISize}, {max_header, MaxHeaderSize},
				   {max_version, ?HTTP_MAX_VERSION_STRING}, 
				   {max_method, ?HTTP_MAX_METHOD_STRING},
				   {max_content_length, MaxContentLen},
				   {customize, Customize}
				  ]]}, 

    State = #state{mod                    = Mod, 
		   manager                = Manager, 
		   status                 = Status,
		   timeout                = TimeOut, 
		   max_keep_alive_request = NrOfRequest,
		   mfa                    = MFA,
                   chunk                   = chunk_start(MaxChunk)},
    
    http_transport:setopts(SocketType, Socket, 
			   [binary, {packet, 0}, {active, once}]),
    NewState =  data_receive_counter(activate_request_timeout(State), httpd_util:lookup(ConfigDB, minimum_bytes_per_second, false)),
     gen_server:enter_loop(?MODULE, [], NewState).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Request, From, #state{mod = ModData} = State) ->
    Error = 
	lists:flatten(
	  io_lib:format("Unexpected request: "
			"~n~p"
			"~nto request handler (~p) from ~p"
			"~n", [Request, self(), From])),
    error_log(Error, ModData),
    {stop, {call_api_violation, Request, From}, State}.

%%--------------------------------------------------------------------
%% handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, #state{mod = ModData} = State) ->
    Error = 
	lists:flatten(
	  io_lib:format("Unexpected message: "
			"~n~p"
			"~nto request handler (~p)"
			"~n", [Msg, self()])),
    error_log(Error, ModData),
    {noreply, State}.

%%--------------------------------------------------------------------
%% handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Proto, Socket, Data}, 
	    #state{mfa = {Module, Function, Args},
                   chunk = {ChunkState, _},
		   mod = #mod{socket_type = SockType, 
			      socket = Socket} = ModData} = State) 
  when (((Proto =:= tcp) orelse 
	 (Proto =:= ssl) orelse 
	 (Proto =:= dummy)) andalso is_binary(Data)) ->

    PROCESSED = (catch Module:Function([Data | Args])),
    NewDataSize = case State#state.byte_limit of
		      undefined ->
			  undefined;
		      _ ->
			  State#state.data + byte_size(Data)
		  end,

    case PROCESSED of       
        {ok, Result} ->
	    NewState = case NewDataSize of
			   undefined ->
			       cancel_request_timeout(State);
			   _ ->
			       set_new_data_size(cancel_request_timeout(State), NewDataSize)
		       end,
            handle_msg(Result, NewState);
	{error, {size_error, MaxSize, ErrCode, ErrStr}, Version} ->
	    NewModData =  ModData#mod{http_version = Version},
	    httpd_response:send_status(NewModData, ErrCode, ErrStr),
	    Reason = io_lib:format("~p: ~p max size is ~p~n", 
				   [ErrCode, ErrStr, MaxSize]),
	    error_log(Reason, NewModData),
	    {stop, normal, State#state{response_sent = true, 
				       mod = NewModData}};
        
        {http_chunk = Module, Function, Args} when ChunkState =/= undefined ->
            NewState = handle_chunk(Module, Function, Args, State),
            {noreply, NewState};
	NewMFA ->
	    http_transport:setopts(SockType, Socket, [{active, once}]),
	    case NewDataSize of
		undefined ->
		    {noreply, State#state{mfa = NewMFA}};
		_ ->
		    {noreply, State#state{mfa = NewMFA, data = NewDataSize}}
	    end
    end;

%% Error cases
handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};
handle_info({ssl_closed, _}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, _} = Reason, State) ->
    {stop, {shutdown, Reason}, State};
handle_info({ssl_error, _, _} = Reason, State) ->
    {stop, {shutdown, Reason}, State};

%% Timeouts
handle_info(timeout, #state{mfa = {_, parse, _}} = State) ->
    %% error_log("No request received on keep-alive connection "
    %% 	      "before server side timeout", ModData),
    %% No response should be sent!
    {stop, normal, State#state{response_sent = true}}; 
handle_info(timeout, #state{mod = ModData} = State) ->
    httpd_response:send_status(ModData, 408, "Request timeout"),
    error_log("The client did not send the whole request before the "
	      "server side timeout", ModData),
    {stop, normal, State#state{response_sent = true}};
handle_info(check_data_first, #state{data = Data, byte_limit = Byte_Limit} = State) ->
    case Data >= (Byte_Limit*3) of 
	true ->
	    erlang:send_after(1000, self(), check_data),
	    {noreply, State#state{data = 0}};
	_ ->
	    {stop, normal, State#state{response_sent = true}}
    end;
handle_info(check_data, #state{data = Data, byte_limit = Byte_Limit} = State) ->
    case Data >= Byte_Limit of 
	true ->
	    erlang:send_after(1000, self(), check_data),
	    {noreply, State#state{data = 0}};
	_ ->
	    {stop, normal, State#state{response_sent = true}}
    end;

handle_info({'EXIT', _, Reason}, State) ->
    {stop, Reason, State};

%% Default case
handle_info(Info, #state{mod = ModData} = State) ->
    Error = lists:flatten(
	      io_lib:format("Unexpected info: "
			    "~n~p"
			    "~nto request handler (~p)"
			    "~n", [Info, self()])),
    error_log(Error, ModData),
    {noreply, State}.


%%--------------------------------------------------------------------
%% terminate(Reason, State) -> void()
%%
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, State) when Reason == normal;
			      Reason == shutdown ->
    do_terminate(State);
terminate({shutdown,_}, State) ->
    do_terminate(State);
terminate(Reason, #state{response_sent = false, mod = ModData} = State) ->
    httpd_response:send_status(ModData, 500, none),
     ReasonStr = 
	lists:flatten(io_lib:format("~s - ~p", 
				    [httpd_util:reason_phrase(500), Reason])),
    error_log(ReasonStr, ModData),
    terminate(Reason, State#state{response_sent = true, mod = ModData});
terminate(_Reason, State) ->
    do_terminate(State).

do_terminate(#state{mod = ModData} = State) ->
    cancel_request_timeout(State),
    httpd_socket:close(ModData#mod.socket_type, ModData#mod.socket).

format_status(normal, [_, State]) ->
    [{data, [{"StateData", State}]}];  
format_status(terminate, [_, State]) ->
    Mod = (State#state.mod),
    case Mod#mod.socket_type of
	ip_comm ->
	    [{data, [{"StateData", State}]}];  
	{essl, _} ->
	    %% Do not print ssl options in superviosr reports
	    [{data, [{"StateData", 
		      State#state{mod = Mod#mod{socket_type = 'TLS'}}}]}]
    end.

%%--------------------------------------------------------------------
%% code_change(OldVsn, State, Extra) -> {ok, NewState}
%%
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
set_new_data_size(State, NewData) ->
    State#state{data = NewData}.
await_socket_ownership_transfer(AcceptTimeout) ->
    receive
	{socket_ownership_transfered, SocketType, Socket} ->
	    {SocketType, Socket}
    after AcceptTimeout ->
	    exit(accept_socket_timeout)
    end.


%%% Internal chunking of client body 
handle_msg({{continue, Chunk}, Module, Function, Args}, #state{chunk = {_, CbState}} = State) ->
    handle_internal_chunk(State#state{chunk = {continue, CbState},
                                      body = Chunk}, Module, Function, Args);
handle_msg({continue, Module, Function, Args}, 	#state{mod = ModData} = State) ->
    http_transport:setopts(ModData#mod.socket_type, 
                           ModData#mod.socket, 
                           [{active, once}]),
    {noreply, State#state{mfa = {Module, Function, Args}}};
handle_msg({last, Body}, #state{headers = Headers, chunk = {_, CbState}} = State) -> 
    NewHeaders = Headers#http_request_h{'content-length' = integer_to_list(size(Body))},
    handle_response(State#state{chunk = {last, CbState},
                                headers = NewHeaders,
                                body = Body});
%%% Last data chunked by client
handle_msg({ChunkedHeaders, Body}, #state{headers = Headers , chunk = {ChunkState, CbState}} = State) when ChunkState =/= undefined ->
    NewHeaders = http_chunk:handle_headers(Headers, ChunkedHeaders),
    handle_response(State#state{chunk = {last, CbState},
                                headers = NewHeaders,
                                body = Body});
handle_msg({ChunkedHeaders, Body}, #state{headers = Headers , chunk = {undefined, _}} = State) ->
    NewHeaders = http_chunk:handle_headers(Headers, ChunkedHeaders),
    handle_response(State#state{headers = NewHeaders,
                                body = Body});
handle_msg(Result, State) ->
    handle_http_msg(Result, State).

handle_http_msg({_, _, Version, {_, _}, _}, 
		#state{status = busy, mod = ModData} = State) -> 
    handle_manager_busy(State#state{mod = 
				    ModData#mod{http_version = Version}}),
    {stop, normal, State}; 

handle_http_msg({_, _, Version, {_, _}, _}, 
		#state{status = blocked, mod = ModData} = State) ->
    handle_manager_blocked(State#state{mod = 
				       ModData#mod{http_version = Version}}),
    {stop, normal, State}; 

handle_http_msg({Method, Uri, Version, {RecordHeaders, Headers}, Body},
		#state{status = accept, mod = ModData} = State) ->        
    case httpd_request:validate(Method, Uri, Version) of
	ok  ->
	    {ok, NewModData} = 
		httpd_request:update_mod_data(ModData, Method, Uri,
					      Version, Headers),
      
	    case is_host_specified_if_required(NewModData#mod.absolute_uri,
					       RecordHeaders, Version) of
		true ->
		    handle_body(State#state{headers = RecordHeaders,
					    body = Body,
					    mod = NewModData});
		false ->
		    httpd_response:send_status(ModData#mod{http_version = 
							   Version}, 
					       400, none),
		    {stop, normal, State#state{response_sent = true}}
	    end;
	{error, {not_supported, What}} ->
	    httpd_response:send_status(ModData#mod{http_version = Version},
				       501, {Method, Uri, Version}),
	    Reason = io_lib:format("Not supported: ~p~n", [What]),
	    error_log(Reason, ModData),
	    {stop, normal, State#state{response_sent = true}};
	{error, {bad_request, {forbidden, URI}}} ->
	    httpd_response:send_status(ModData#mod{http_version = Version},
				       403, URI),
	    Reason = io_lib:format("Forbidden URI: ~p~n", [URI]),
	    error_log(Reason, ModData),
	    {stop, normal, State#state{response_sent = true}};
	{error, {bad_request, {malformed_syntax, URI}}} ->
	    httpd_response:send_status(ModData#mod{http_version = Version},
				       400, URI),
	    Reason = io_lib:format("Malformed syntax in URI: ~p~n", [URI]),
	    error_log(Reason, ModData),
	    {stop, normal, State#state{response_sent = true}};
	{error, {bad_version, Ver}} ->
	    httpd_response:send_status(ModData#mod{http_version = "HTTP/0.9"}, 400, Ver),
	    Reason = io_lib:format("Malformed syntax version: ~p~n", [Ver]),
	    error_log(Reason, ModData),
	    {stop, normal, State#state{response_sent = true}}
    end;
handle_http_msg(Body, State) ->
    handle_response(State#state{body = Body}).

handle_manager_busy(#state{mod = #mod{config_db = ConfigDB}} = State) ->
    MaxClients = httpd_util:lookup(ConfigDB, max_clients, 150),
    Reason = io_lib:format("heavy load (>~w processes)", [MaxClients]),
    reject_connection(State, lists:flatten(Reason)).

handle_manager_blocked(State) ->
    Reason = "Server maintenance performed, try again later",
    reject_connection(State, Reason).

reject_connection(#state{mod = ModData} = State, Reason) ->
    httpd_response:send_status(ModData, 503, Reason),
    {stop, normal, State#state{response_sent = true}}. 

is_host_specified_if_required(nohost, #http_request_h{host = undefined}, 
			      "HTTP/1.1") ->
    false;
is_host_specified_if_required(_, _, _) ->
    true.

handle_body(#state{mod = #mod{config_db = ConfigDB}} = State) ->
    MaxHeaderSize = max_header_size(ConfigDB), 
    MaxBodySize   = max_body_size(ConfigDB), 
   
    case handle_expect(State, MaxBodySize) of 
	ok ->
	    handle_body(State, MaxHeaderSize, MaxBodySize);
	Other ->
	    Other
    
    end.
	
handle_body(#state{headers = Headers, body = Body, 
                   chunk =  {ChunkState, CbState}, mod = #mod{config_db = ConfigDB} = ModData} = State,
	    MaxHeaderSize, MaxBodySize) ->
    MaxChunk = max_client_body_chunk(ConfigDB),
    case Headers#http_request_h.'transfer-encoding' of
	"chunked" ->
	    try http_chunk:decode(Body, MaxBodySize, MaxHeaderSize) of
                {Module, Function, Args} ->
		    http_transport:setopts(ModData#mod.socket_type, 
					   ModData#mod.socket, 
					   [{active, once}]),
		    {noreply, State#state{mfa = 
                                              {Module, Function, Args},
                                          chunk = chunk_start(MaxChunk)}};
                {ok, {ChunkedHeaders, NewBody}} ->
		    NewHeaders = http_chunk:handle_headers(Headers, ChunkedHeaders),	
                    handle_response(State#state{headers = NewHeaders,
                                                body = NewBody,
                                                chunk = chunk_finish(ChunkState, CbState, MaxChunk)})
	    catch 
		throw:Error ->
		    httpd_response:send_status(ModData, 400, 
					       "Bad input"),
		    Reason = io_lib:format("Chunk decoding failed: ~p~n", 
					   [Error]),
		    error_log(Reason, ModData),
		    {stop, normal, State#state{response_sent = true}}  
	    end;
	Encoding when is_list(Encoding) ->
	    httpd_response:send_status(ModData, 501, 
				       "Unknown Transfer-Encoding"),
	    Reason = io_lib:format("Unknown Transfer-Encoding: ~p~n", 
				   [Encoding]),
	    error_log(Reason, ModData),
	    {stop, normal, State#state{response_sent = true}};
	_ -> 
	    Length = list_to_integer(Headers#http_request_h.'content-length'),
	    MaxChunk = max_client_body_chunk(ConfigDB),
	    case ((Length =< MaxBodySize) or (MaxBodySize == nolimit)) of
		true ->
		    case httpd_request:body_chunk_first(Body, Length, MaxChunk) of 
                        %% This is the case that the we need more data to complete
                        %% the body but chunking to the mod_esi user is not enabled.
                        {Module, add_chunk = Function,  Args} ->  
                            http_transport:setopts(ModData#mod.socket_type, 
						   ModData#mod.socket, 
						   [{active, once}]),
			    {noreply, State#state{mfa = 
						      {Module, Function, Args}}};
                        %% Chunking to mod_esi user is enabled
                        {ok, {continue, Module, Function, Args}} ->
                                http_transport:setopts(ModData#mod.socket_type, 
						   ModData#mod.socket, 
						   [{active, once}]),
			    {noreply, State#state{mfa = 
						      {Module, Function, Args}}};
                        {ok, {{continue, Chunk}, Module, Function, Args}} ->
                            handle_internal_chunk(State#state{chunk =  chunk_start(MaxChunk), 
                                                              body = Chunk}, Module, Function, Args);                   
                        %% Whole body delivered, if chunking mechanism is enabled the whole
                        %% body fits in one chunk.
                        {ok, NewBody} ->
                            handle_response(State#state{chunk = chunk_finish(ChunkState, 
                                                                             CbState, MaxChunk),
                                                        headers = Headers,
                                                        body = NewBody})
		    end;
		false ->
		    httpd_response:send_status(ModData, 413, "Body too long"),
		    error_log("Body too long", ModData),
		    {stop, normal,  State#state{response_sent = true}}
	    end
    end.

handle_expect(#state{headers = Headers, mod = 
		     #mod{config_db = ConfigDB} = ModData} = State, 
	      MaxBodySize) ->
    Length = list_to_integer(Headers#http_request_h.'content-length'),
    case expect(Headers, ModData#mod.http_version, ConfigDB) of
	continue when (MaxBodySize > Length) orelse (MaxBodySize =:= nolimit) ->
	    httpd_response:send_status(ModData, 100, ""),
	    ok;
	continue when MaxBodySize < Length ->
	    httpd_response:send_status(ModData, 413, "Body too long"),
	    error_log("Body too long", ModData),
	    {stop, normal, State#state{response_sent = true}};
	{break, Value} ->
	    httpd_response:send_status(ModData, 417, 
				       "Unexpected expect value"),
	    Reason = io_lib:format("Unexpected expect value: ~p~n", [Value]),
	    error_log(Reason, ModData),
	    {stop, normal,  State#state{response_sent = true}};
	no_expect_header ->
	    ok;
	http_1_0_expect_header ->
	    httpd_response:send_status(ModData, 400, 
				       "Only HTTP/1.1 Clients "
				       "may use the Expect Header"),
	    error_log("Client with lower version than 1.1 tried to send"
		      "an expect header", ModData),
	    {stop, normal, State#state{response_sent = true}}
    end.

expect(Headers, "HTTP/1.1", _) ->
    case Headers#http_request_h.expect of
	"100-continue" ->
	    continue; 
	undefined ->
	    no_expect_header;
	Other ->
	    {break, Other}
    end;
expect(Headers, _, ConfigDB) ->
    case Headers#http_request_h.expect of
	undefined ->
	    no_expect_header;
	_ ->
	    case httpd_util:lookup(ConfigDB, expect, continue) of
		continue->
		    no_expect_header;
		_ ->
		    http_1_0_expect_header
	    end
    end.

handle_chunk(http_chunk = Module, decode_data = Function, 
             [ChunkSize, TotalChunk, {MaxBodySize, BodySoFar, _AccLength, MaxHeaderSize}],
             #state{chunk = {_, CbState},
                    mod = #mod{socket_type = SockType,
                               socket = Socket} = ModData} = State) ->
    {continue, NewCbState} = httpd_response:handle_continuation(ModData#mod{entity_body = 
                                                                                {continue, BodySoFar, CbState}}),
    http_transport:setopts(SockType, Socket, [{active, once}]),
    State#state{chunk = {continue, NewCbState}, mfa = {Module, Function, [ChunkSize, TotalChunk, {MaxBodySize, <<>>, 0, MaxHeaderSize}]}};

handle_chunk(http_chunk = Module, decode_size = Function, 
             [Data, HexList, _AccSize, {MaxBodySize, BodySoFar, _AccLength, MaxHeaderSize}],
             #state{chunk = {_, CbState},
                    mod = #mod{socket_type = SockType,
                               socket = Socket} = ModData} = State) ->
    {continue, NewCbState} = httpd_response:handle_continuation(ModData#mod{entity_body = {continue, BodySoFar, CbState}}),
    http_transport:setopts(SockType, Socket, [{active, once}]),
    State#state{chunk = {continue, NewCbState}, mfa = {Module, Function, [Data, HexList, 0, {MaxBodySize, <<>>, 0, MaxHeaderSize}]}};
handle_chunk(Module, Function, Args, #state{mod = #mod{socket_type = SockType,
                                                                      socket = Socket}} = State) ->
    http_transport:setopts(SockType, Socket, [{active, once}]),
    State#state{mfa = {Module, Function, Args}}.

handle_internal_chunk(#state{chunk = {ChunkState, CbState}, body = Chunk, 
                             mod = #mod{socket_type = SockType,
                                        socket = Socket} = ModData} = State, Module, Function, Args)->
    Bodychunk = body_chunk(ChunkState, CbState, Chunk),
    {continue, NewCbState} = httpd_response:handle_continuation(ModData#mod{entity_body = Bodychunk}),
    case Args of
        [<<>> | _] ->
            http_transport:setopts(SockType, Socket, [{active, once}]),
            {noreply, State#state{chunk = {continue, NewCbState}, mfa = {Module, Function, Args}}};
        _ ->
            handle_info({dummy, Socket, <<>>}, State#state{chunk = {continue, NewCbState}, 
                                                           mfa = {Module, Function, Args}})
    end.

handle_response(#state{body    = Body, 
                       headers = Headers,
		       mod     = ModData, 
                       chunk   = {last, CbState},
		       max_keep_alive_request = Max} = State) when Max > 0 ->
    {NewBody, Data} = httpd_request:body_data(Headers, Body),
    ok = httpd_response:generate_and_send_response(
           ModData#mod{entity_body = {last, NewBody, CbState}}),     
    handle_next_request(State#state{response_sent = true}, Data);
handle_response(#state{body    = Body, 
		       mod     = ModData, 
		       headers = Headers,
		       max_keep_alive_request = Max} = State) when Max > 0 ->
    {NewBody, Data} = httpd_request:body_data(Headers, Body),
    %% Backwards compatible, may cause memory explosion
    ok = httpd_response:generate_and_send_response(
           ModData#mod{entity_body = binary_to_list(NewBody)}),
    handle_next_request(State#state{response_sent = true}, Data);
handle_response(#state{body    = Body, 
		       headers = Headers, 
		       mod     = ModData} = State) ->
    {NewBody, _} = httpd_request:body_data(Headers, Body),
    ok = httpd_response:generate_and_send_response(
	   ModData#mod{entity_body = NewBody}),
    {stop, normal, State#state{response_sent = true}}.

handle_next_request(#state{mod = #mod{connection = true} = ModData,
			   max_keep_alive_request = Max} = State, Data) ->

    NewModData = #mod{socket_type = ModData#mod.socket_type, 
 		      socket      = ModData#mod.socket, 
 		      config_db   = ModData#mod.config_db, 
 		      init_data   = ModData#mod.init_data},
    MaxHeaderSize = max_header_size(ModData#mod.config_db), 
    MaxURISize    = max_uri_size(ModData#mod.config_db), 
    MaxContentLen = max_content_length(ModData#mod.config_db),
    Customize = customize(ModData#mod.config_db),
    MaxChunk = max_client_body_chunk(ModData#mod.config_db),

    MFA = {httpd_request, parse, [[{max_uri, MaxURISize}, {max_header, MaxHeaderSize},
				   {max_version, ?HTTP_MAX_VERSION_STRING}, 
				   {max_method, ?HTTP_MAX_METHOD_STRING},
				   {max_content_length, MaxContentLen},
				   {customize, Customize}
				  ]]}, 
    TmpState = State#state{mod                    = NewModData,
			   mfa                    = MFA,
			   max_keep_alive_request = decrease(Max),
			   headers                = undefined, 
			   body                   = undefined,
                           chunk                  = chunk_start(MaxChunk),
			   response_sent          = false},
    
    NewState = activate_request_timeout(TmpState),

    case Data of
	<<>> ->
	    http_transport:setopts(ModData#mod.socket_type,
				   ModData#mod.socket, [{active, once}]),
	    {noreply, NewState};
	_ ->
	    handle_info({dummy, ModData#mod.socket, Data}, NewState)
    end;

handle_next_request(State, _) ->
    {stop, normal, State}.

activate_request_timeout(#state{timeout = Time} = State) ->
    Ref = erlang:send_after(Time, self(), timeout),
    State#state{timer = Ref}.
data_receive_counter(State, Byte_limit) ->
    case Byte_limit of
	false ->
	    State#state{data = 0};
	Nr ->
	    erlang:send_after(3000, self(), check_data_first),
	    State#state{data = 0, byte_limit = Nr}
    end.
cancel_request_timeout(#state{timer = undefined} = State) ->
    State;
cancel_request_timeout(#state{timer = Timer} = State) ->
    erlang:cancel_timer(Timer),
    receive 
	timeout ->
	    ok
    after 0 ->
	    ok
    end,
    State#state{timer = undefined}.

decrease(N) when is_integer(N) ->
    N-1;
decrease(N) ->
    N.

error_log(ReasonString,  #mod{config_db = ConfigDB}) ->
    Error = lists:flatten(
	      io_lib:format("Error reading request: ~s", [ReasonString])),
    httpd_util:error_log(ConfigDB, Error).


%%--------------------------------------------------------------------
%% Config access wrapper functions
%%--------------------------------------------------------------------

max_header_size(ConfigDB) ->
    httpd_util:lookup(ConfigDB, max_header_size, ?HTTP_MAX_HEADER_SIZE).

max_client_body_chunk(ConfigDB) ->
    httpd_util:lookup(ConfigDB, max_client_body_chunk, nolimit).

max_uri_size(ConfigDB) ->
    httpd_util:lookup(ConfigDB, max_uri_size, ?HTTP_MAX_URI_SIZE).

max_body_size(ConfigDB) ->
    httpd_util:lookup(ConfigDB, max_body_size, nolimit).

max_keep_alive_request(ConfigDB) ->
    httpd_util:lookup(ConfigDB, max_keep_alive_request, infinity).

max_content_length(ConfigDB) ->    
    httpd_util:lookup(ConfigDB, max_content_length, ?HTTP_MAX_CONTENT_LENGTH).

customize(ConfigDB) ->    
    httpd_util:lookup(ConfigDB, customize, httpd_custom).

chunk_start(nolimit) ->
    {undefined, undefined};
chunk_start(_) ->
    {first, undefined}.
chunk_finish(_, _, nolimit) ->
    {undefined, undefined};
chunk_finish(_, CbState, _) ->
    {last, CbState}.

body_chunk(first, _, Chunk) ->
    {first, Chunk};
body_chunk(ChunkState, CbState, Chunk) ->
    {ChunkState, Chunk, CbState}.
