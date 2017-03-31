%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2017. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Handles an ssl connection, e.i. both the setup
%% e.i. SSL-Handshake, SSL-Alert and SSL-Cipher protocols and delivering
%% data to the application. All data on the connectinon is received and 
%% sent according to the SSL-record protocol.  
%%----------------------------------------------------------------------

-module(tls_connection).

-behaviour(gen_statem).

-include("tls_connection.hrl").
-include("tls_handshake.hrl").
-include("ssl_alert.hrl").
-include("tls_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_api.hrl").
-include("ssl_internal.hrl").
-include("ssl_srp.hrl").
-include_lib("public_key/include/public_key.hrl"). 

%% Internal application API

%% Setup
-export([start_fsm/8, start_link/7, init/1]).

-export([encode_data/3, encode_alert/3]).

%% State transition handling	 
-export([next_record/1, next_event/3, next_event/4]).

%% Handshake handling
-export([renegotiate/2, send_handshake/2, 
	 queue_handshake/2, queue_change_cipher/2,
	 reinit_handshake_data/1,  select_sni_extension/1]).

%% Alert and close handling
-export([send_alert/2, close/5]).

%% Data handling
-export([passive_receive/2, next_record_if_active/1, handle_common_event/4, send/3,
        socket/5]).

%% gen_statem state functions
-export([init/3, error/3, downgrade/3, %% Initiation and take down states
	 hello/3, certify/3, cipher/3, abbreviated/3, %% Handshake states 
	 connection/3]). 
%% gen_statem callbacks
-export([callback_mode/0, terminate/3, code_change/4, format_status/2]).
 
%%====================================================================
%% Internal application API
%%====================================================================	     
start_fsm(Role, Host, Port, Socket, {#ssl_options{erl_dist = false},_, Tracker} = Opts,
	  User, {CbModule, _,_, _} = CbInfo, 
	  Timeout) -> 
    try 
	{ok, Pid} = tls_connection_sup:start_child([Role, Host, Port, Socket, 
						    Opts, User, CbInfo]), 
	{ok, SslSocket} = ssl_connection:socket_control(?MODULE, Socket, Pid, CbModule, Tracker),
	ok = ssl_connection:handshake(SslSocket, Timeout),
	{ok, SslSocket} 
    catch
	error:{badmatch, {error, _} = Error} ->
	    Error
    end;

start_fsm(Role, Host, Port, Socket, {#ssl_options{erl_dist = true},_, Tracker} = Opts,
	  User, {CbModule, _,_, _} = CbInfo, 
	  Timeout) -> 
    try 
	{ok, Pid} = tls_connection_sup:start_child_dist([Role, Host, Port, Socket, 
							 Opts, User, CbInfo]), 
	{ok, SslSocket} = ssl_connection:socket_control(?MODULE, Socket, Pid, CbModule, Tracker),
	ok = ssl_connection:handshake(SslSocket, Timeout),
	{ok, SslSocket} 
    catch
	error:{badmatch, {error, _} = Error} ->
	    Error
    end.

send_handshake(Handshake, State) ->
    send_handshake_flight(queue_handshake(Handshake, State)).

queue_handshake(Handshake, #state{negotiated_version = Version,
				  tls_handshake_history = Hist0,
				  flight_buffer = Flight0,
				  ssl_options = #ssl_options{v2_hello_compatible = V2HComp},				  
				  connection_states = ConnectionStates0} = State0) ->
    {BinHandshake, ConnectionStates, Hist} =
	encode_handshake(Handshake, Version, ConnectionStates0, Hist0, V2HComp),
    State0#state{connection_states = ConnectionStates,
		 tls_handshake_history = Hist,
		 flight_buffer = Flight0 ++ [BinHandshake]}.

send_handshake_flight(#state{socket = Socket,
			     transport_cb = Transport,
			     flight_buffer = Flight} = State0) ->
    send(Transport, Socket, Flight),
    {State0#state{flight_buffer = []}, []}.

queue_change_cipher(Msg, #state{negotiated_version = Version,
				  flight_buffer = Flight0,
				  connection_states = ConnectionStates0} = State0) ->
    {BinChangeCipher, ConnectionStates} =
	encode_change_cipher(Msg, Version, ConnectionStates0),
    State0#state{connection_states = ConnectionStates,
		 flight_buffer = Flight0 ++ [BinChangeCipher]}.

send_alert(Alert, #state{negotiated_version = Version,
			 socket = Socket,
			 transport_cb = Transport,
			 connection_states = ConnectionStates0} = State0) ->
    {BinMsg, ConnectionStates} =
	encode_alert(Alert, Version, ConnectionStates0),
    send(Transport, Socket, BinMsg),
    State0#state{connection_states = ConnectionStates}.

reinit_handshake_data(State) ->
    %% premaster_secret, public_key_info and tls_handshake_info 
    %% are only needed during the handshake phase. 
    %% To reduce memory foot print of a connection reinitialize them.
     State#state{
       premaster_secret = undefined,
       public_key_info = undefined,
       tls_handshake_history = ssl_handshake:init_handshake_history()
     }.

select_sni_extension(#client_hello{extensions = HelloExtensions}) ->
    HelloExtensions#hello_extensions.sni;
select_sni_extension(_) ->
    undefined.

encode_data(Data, Version, ConnectionStates0)->
    tls_record:encode_data(Data, Version, ConnectionStates0).

%%--------------------------------------------------------------------
-spec encode_alert(#alert{}, ssl_record:ssl_version(), ssl_record:connection_states()) -> 
		    {iolist(), ssl_record:connection_states()}.
%%
%% Description: Encodes an alert
%%--------------------------------------------------------------------
encode_alert(#alert{} = Alert, Version, ConnectionStates) ->
    tls_record:encode_alert_record(Alert, Version, ConnectionStates).

%%====================================================================
%% tls_connection_sup API
%%====================================================================

%%--------------------------------------------------------------------
-spec start_link(atom(), host(), inet:port_number(), port(), list(), pid(), tuple()) ->
    {ok, pid()} | ignore |  {error, reason()}.
%%
%% Description: Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link(Role, Host, Port, Socket, Options, User, CbInfo) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [[Role, Host, Port, Socket, Options, User, CbInfo]])}.

init([Role, Host, Port, Socket, Options,  User, CbInfo]) ->
    process_flag(trap_exit, true),
    State0 = initial_state(Role, Host, Port, Socket, Options, User, CbInfo),
    try 
	State = ssl_connection:ssl_config(State0#state.ssl_options, Role, State0),
	gen_statem:enter_loop(?MODULE, [], init, State)
    catch throw:Error ->
	gen_statem:enter_loop(?MODULE, [], error, {Error, State0}) 
    end.

callback_mode() ->
    state_functions.

socket(Pid,  Transport, Socket, Connection, Tracker) ->
    tls_socket:socket(Pid, Transport, Socket, Connection, Tracker).


%%--------------------------------------------------------------------
%% State functions
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
-spec init(gen_statem:event_type(),
	   {start, timeout()} | term(), #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------

init({call, From}, {start, Timeout}, 
     #state{host = Host, port = Port, role = client,
	    ssl_options = #ssl_options{v2_hello_compatible = V2HComp} = SslOpts,
	    session = #session{own_certificate = Cert} = Session0,
	    transport_cb = Transport, socket = Socket,
	    connection_states = ConnectionStates0,
	    renegotiation = {Renegotiation, _},
	    session_cache = Cache,
	    session_cache_cb = CacheCb
	   } = State0) ->
    Timer = ssl_connection:start_or_recv_cancel_timer(Timeout, From),
    Hello = tls_handshake:client_hello(Host, Port, ConnectionStates0, SslOpts,
				       Cache, CacheCb, Renegotiation, Cert),
    
    Version = Hello#client_hello.client_version,
    HelloVersion = tls_record:lowest_protocol_version(SslOpts#ssl_options.versions),
    Handshake0 = ssl_handshake:init_handshake_history(),
    {BinMsg, ConnectionStates, Handshake} =
        encode_handshake(Hello,  HelloVersion, ConnectionStates0, Handshake0, V2HComp),
    send(Transport, Socket, BinMsg),
    State1 = State0#state{connection_states = ConnectionStates,
			  negotiated_version = Version, %% Requested version
			  session =
			      Session0#session{session_id = Hello#client_hello.session_id},
			  tls_handshake_history = Handshake,
			  start_or_recv_from = From,
			  timer = Timer},
    {Record, State} = next_record(State1),
    next_event(hello, Record, State);
init(Type, Event, State) ->
    gen_handshake(ssl_connection, init, Type, Event, State).
 
%%--------------------------------------------------------------------
-spec error(gen_statem:event_type(),
	   {start, timeout()} | term(), #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------

error({call, From}, {start, _Timeout}, {Error, State}) ->
    {stop_and_reply, normal, {reply, From, {error, Error}}, State};
error({call, From}, Msg, State) ->
    handle_call(Msg, From, error, State);
error(_, _, _) ->
     {keep_state_and_data, [postpone]}.
 
%%--------------------------------------------------------------------
-spec hello(gen_statem:event_type(),
	    #hello_request{} | #client_hello{} | #server_hello{} | term(),
	    #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
hello(internal, #client_hello{client_version = ClientVersion} = Hello,
      #state{connection_states = ConnectionStates0,
	     port = Port, session = #session{own_certificate = Cert} = Session0,
	     renegotiation = {Renegotiation, _},
	     session_cache = Cache,
	     session_cache_cb = CacheCb,
	     negotiated_protocol = CurrentProtocol,
	     key_algorithm = KeyExAlg,
	     ssl_options = SslOpts} = State) ->

    case tls_handshake:hello(Hello, SslOpts, {Port, Session0, Cache, CacheCb,
					      ConnectionStates0, Cert, KeyExAlg}, Renegotiation) of
        #alert{} = Alert ->
            ssl_connection:handle_own_alert(Alert, ClientVersion, hello, State);
        {Version, {Type, Session},
	 ConnectionStates, Protocol0, ServerHelloExt, HashSign} ->
	    Protocol = case Protocol0 of
			   undefined -> CurrentProtocol;
			   _ -> Protocol0
		       end,
	    
	    gen_handshake(ssl_connection, hello, internal, {common_client_hello, Type, ServerHelloExt},
				 State#state{connection_states  = ConnectionStates,
					     negotiated_version = Version,
					     hashsign_algorithm = HashSign,
					     session = Session,
					     negotiated_protocol = Protocol})
    end;
hello(internal, #server_hello{} = Hello,
      #state{connection_states = ConnectionStates0,
	     negotiated_version = ReqVersion,
	     role = client,
	     renegotiation = {Renegotiation, _},
	     ssl_options = SslOptions} = State) ->
    case tls_handshake:hello(Hello, SslOptions, ConnectionStates0, Renegotiation) of
	#alert{} = Alert ->
	    ssl_connection:handle_own_alert(Alert, ReqVersion, hello, State);
	{Version, NewId, ConnectionStates, ProtoExt, Protocol} ->
	    ssl_connection:handle_session(Hello, 
					  Version, NewId, ConnectionStates, ProtoExt, Protocol, State)
    end;
hello(info, Event, State) ->
    gen_info(Event, hello, State);
hello(Type, Event, State) ->
    gen_handshake(ssl_connection, hello, Type, Event, State).

%%--------------------------------------------------------------------
-spec abbreviated(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
abbreviated(info, Event, State) ->
    gen_info(Event, abbreviated, State);
abbreviated(Type, Event, State) ->
    gen_handshake(ssl_connection, abbreviated, Type, Event, State).

%%--------------------------------------------------------------------
-spec certify(gen_statem:event_type(), term(), #state{}) ->
		     gen_statem:state_function_result().
%%--------------------------------------------------------------------
certify(info, Event, State) ->
    gen_info(Event, certify, State);
certify(Type, Event, State) ->
    gen_handshake(ssl_connection, certify, Type, Event, State).

%%--------------------------------------------------------------------
-spec cipher(gen_statem:event_type(), term(), #state{}) ->
		    gen_statem:state_function_result().
%%--------------------------------------------------------------------
cipher(info, Event, State) ->
    gen_info(Event, cipher, State);
cipher(Type, Event, State) ->
     gen_handshake(ssl_connection, cipher, Type, Event, State).

%%--------------------------------------------------------------------
-spec connection(gen_statem:event_type(),  
		 #hello_request{} | #client_hello{}| term(), #state{}) ->
			gen_statem:state_function_result().
%%--------------------------------------------------------------------
connection(info, Event, State) ->
    gen_info(Event, connection, State);
connection(internal, #hello_request{},
	   #state{role = client, host = Host, port = Port,
		  session = #session{own_certificate = Cert} = Session0,
		  session_cache = Cache, session_cache_cb = CacheCb,
		  ssl_options = SslOpts,
		  connection_states = ConnectionStates0,
		  renegotiation = {Renegotiation, _}} = State0) ->
    Hello = tls_handshake:client_hello(Host, Port, ConnectionStates0, SslOpts,
				       Cache, CacheCb, Renegotiation, Cert),
    {State1, Actions} = send_handshake(Hello, State0),
    {Record, State} =
	next_record(
	  State1#state{session = Session0#session{session_id
						  = Hello#client_hello.session_id}}),
    next_event(hello, Record, State, Actions);
connection(internal, #client_hello{} = Hello, 
	   #state{role = server, allow_renegotiate = true} = State0) ->
    %% Mitigate Computational DoS attack
    %% http://www.educatedguesswork.org/2011/10/ssltls_and_computational_dos.html
    %% http://www.thc.org/thc-ssl-dos/ Rather than disabling client
    %% initiated renegotiation we will disallow many client initiated
    %% renegotiations immediately after each other.
    erlang:send_after(?WAIT_TO_ALLOW_RENEGOTIATION, self(), allow_renegotiate),
    {Record, State} = next_record(State0#state{allow_renegotiate = false,
					       renegotiation = {true, peer}}),
    next_event(hello, Record, State,  [{next_event, internal, Hello}]);
connection(internal, #client_hello{}, 
	   #state{role = server, allow_renegotiate = false} = State0) ->
    Alert = ?ALERT_REC(?WARNING, ?NO_RENEGOTIATION),
    State1 = send_alert(Alert, State0),
    {Record, State} = ssl_connection:prepare_connection(State1, ?MODULE),
    next_event(connection, Record, State);
connection(Type, Event, State) ->
    ssl_connection:connection(Type, Event, State, ?MODULE).

%%--------------------------------------------------------------------
-spec downgrade(gen_statem:event_type(), term(), #state{}) ->
		       gen_statem:state_function_result().
%%--------------------------------------------------------------------
downgrade(Type, Event, State) ->
     ssl_connection:downgrade(Type, Event, State, ?MODULE).

%%--------------------------------------------------------------------
%% Event handling functions called by state functions to handle
%% common or unexpected events for the state.
%%--------------------------------------------------------------------
handle_call(Event, From, StateName, State) ->
    ssl_connection:handle_call(Event, From, StateName, State, ?MODULE).
 
%% raw data from socket, unpack records
handle_info({Protocol, _, Data}, StateName,
            #state{data_tag = Protocol} = State0) ->
    case next_tls_record(Data, State0) of
	{Record, State} ->
	    next_event(StateName, Record, State);
	#alert{} = Alert ->
	    ssl_connection:handle_normal_shutdown(Alert, StateName, State0), 
	    {stop, {shutdown, own_alert}}
    end;
handle_info({CloseTag, Socket}, StateName,
            #state{socket = Socket, close_tag = CloseTag,
                   socket_options = #socket_options{active = Active},
                   protocol_buffers = #protocol_buffers{tls_cipher_texts = CTs},
		   negotiated_version = Version} = State) ->

    %% Note that as of TLS 1.1,
    %% failure to properly close a connection no longer requires that a
    %% session not be resumed.  This is a change from TLS 1.0 to conform
    %% with widespread implementation practice.

    case (Active == false) andalso (CTs =/= []) of
        false ->
            case Version of
                {1, N} when N >= 1 ->
                    ok;
                _ ->
                    %% As invalidate_sessions here causes performance issues,
                    %% we will conform to the widespread implementation
                    %% practice and go aginst the spec
                    %%invalidate_session(Role, Host, Port, Session)
                    ok
            end,

            ssl_connection:handle_normal_shutdown(?ALERT_REC(?FATAL, ?CLOSE_NOTIFY), StateName, State),
            {stop, {shutdown, transport_closed}};
        true ->
            %% Fixes non-delivery of final TLS record in {active, once}.
            %% Basically allows the application the opportunity to set {active, once} again
            %% and then receive the final message.
            next_event(StateName, no_record, State)
    end;
handle_info(Msg, StateName, State) ->
    ssl_connection:handle_info(Msg, StateName, State).

handle_common_event(internal, #alert{} = Alert, StateName, 
		    #state{negotiated_version = Version} = State) ->
    ssl_connection:handle_own_alert(Alert, Version, StateName, State);

%%% TLS record protocol level handshake messages 
handle_common_event(internal,  #ssl_tls{type = ?HANDSHAKE, fragment = Data}, 
		    StateName, #state{protocol_buffers =
					  #protocol_buffers{tls_handshake_buffer = Buf0} = Buffers,
				      negotiated_version = Version,
				      ssl_options = Options} = State0) ->
    try
	{Packets, Buf} = tls_handshake:get_tls_handshake(Version,Data,Buf0, Options),
	State1 =
	    State0#state{protocol_buffers =
			     Buffers#protocol_buffers{tls_handshake_buffer = Buf}},
	case Packets of
            [] -> 
                assert_buffer_sanity(Buf, Options),
                {Record, State} = next_record(State1),
                next_event(StateName, Record, State);
            _ ->                
                Events = tls_handshake_events(Packets),
                case StateName of
                    connection ->
                        ssl_connection:hibernate_after(StateName, State1, Events);
                    _ ->
                        {next_state, StateName, 
                         State1#state{unprocessed_handshake_events = unprocessed_events(Events)}, Events}
                end
        end
    catch throw:#alert{} = Alert ->
            ssl_connection:handle_own_alert(Alert, Version, StateName, State0)
    end;
%%% TLS record protocol level application data messages 
handle_common_event(internal, #ssl_tls{type = ?APPLICATION_DATA, fragment = Data}, StateName, State) ->
    {next_state, StateName, State, [{next_event, internal, {application_data, Data}}]};
%%% TLS record protocol level change cipher messages
handle_common_event(internal, #ssl_tls{type = ?CHANGE_CIPHER_SPEC, fragment = Data}, StateName, State) ->
    {next_state, StateName, State, [{next_event, internal, #change_cipher_spec{type = Data}}]};
%%% TLS record protocol level Alert messages
handle_common_event(internal, #ssl_tls{type = ?ALERT, fragment = EncAlerts}, StateName,
		    #state{negotiated_version = Version} = State) ->
    try decode_alerts(EncAlerts) of	
	Alerts = [_|_] ->
	    handle_alerts(Alerts,  {next_state, StateName, State});
	[] ->
	    ssl_connection:handle_own_alert(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, empty_alert), 
					    Version, StateName, State);
	#alert{} = Alert ->
	    ssl_connection:handle_own_alert(Alert, Version, StateName, State)
    catch
	_:_ ->
	    ssl_connection:handle_own_alert(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, alert_decode_error),
					    Version, StateName, State)  

    end;
%% Ignore unknown TLS record level protocol messages
handle_common_event(internal, #ssl_tls{type = _Unknown}, StateName, State) ->
    {next_state, StateName, State}.

send(Transport, Socket, Data) ->
   tls_socket:send(Transport, Socket, Data).

%%--------------------------------------------------------------------
%% gen_statem callbacks
%%--------------------------------------------------------------------
terminate(Reason, StateName, State) ->
    catch ssl_connection:terminate(Reason, StateName, State).

format_status(Type, Data) ->
    ssl_connection:format_status(Type, Data).

%%--------------------------------------------------------------------
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State0, {Direction, From, To}) ->
    State = convert_state(State0, Direction, From, To),
    {ok, StateName, State};
code_change(_OldVsn, StateName, State, _) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
encode_handshake(Handshake, Version, ConnectionStates0, Hist0, V2HComp) ->
    Frag = tls_handshake:encode_handshake(Handshake, Version),
    Hist = ssl_handshake:update_handshake_history(Hist0, Frag, V2HComp),
    {Encoded, ConnectionStates} =
        tls_record:encode_handshake(Frag, Version, ConnectionStates0),
    {Encoded, ConnectionStates, Hist}.

encode_change_cipher(#change_cipher_spec{}, Version, ConnectionStates) ->
    tls_record:encode_change_cipher_spec(Version, ConnectionStates).

decode_alerts(Bin) ->
    ssl_alert:decode(Bin).

initial_state(Role, Host, Port, Socket, {SSLOptions, SocketOptions, Tracker}, User,
	      {CbModule, DataTag, CloseTag, ErrorTag}) ->
    #ssl_options{beast_mitigation = BeastMitigation} = SSLOptions,
    ConnectionStates = tls_record:init_connection_states(Role, BeastMitigation),
    
    SessionCacheCb = case application:get_env(ssl, session_cb) of
			 {ok, Cb} when is_atom(Cb) ->
			    Cb;
			 _  ->
			     ssl_session_cache
		     end,
    
    Monitor = erlang:monitor(process, User),

    #state{socket_options = SocketOptions,
	   ssl_options = SSLOptions,	   
	   session = #session{is_resumable = new},
	   transport_cb = CbModule,
	   data_tag = DataTag,
	   close_tag = CloseTag,
	   error_tag = ErrorTag,
	   role = Role,
	   host = Host,
	   port = Port,
	   socket = Socket,
	   connection_states = ConnectionStates,
	   protocol_buffers = #protocol_buffers{},
	   user_application = {Monitor, User},
	   user_data_buffer = <<>>,
	   session_cache_cb = SessionCacheCb,
	   renegotiation = {false, first},
	   allow_renegotiate = SSLOptions#ssl_options.client_renegotiation,
	   start_or_recv_from = undefined,
	   protocol_cb = ?MODULE,
	   tracker = Tracker,
	   flight_buffer = []
	  }.

next_tls_record(Data, #state{protocol_buffers = #protocol_buffers{tls_record_buffer = Buf0,
						tls_cipher_texts = CT0} = Buffers} = State0) ->
    case tls_record:get_tls_records(Data, Buf0) of
	{Records, Buf1} ->
	    CT1 = CT0 ++ Records,
	    next_record(State0#state{protocol_buffers =
					 Buffers#protocol_buffers{tls_record_buffer = Buf1,
								  tls_cipher_texts = CT1}});
	#alert{} = Alert ->
	    Alert
    end.
next_record(#state{unprocessed_handshake_events = N} = State) when N > 0 ->
    {no_record, State#state{unprocessed_handshake_events = N-1}};
					 
next_record(#state{protocol_buffers =
		       #protocol_buffers{tls_packets = [], tls_cipher_texts = [CT | Rest]}
		   = Buffers,
		   connection_states = ConnStates0,
		   ssl_options = #ssl_options{padding_check = Check}} = State) ->
    case tls_record:decode_cipher_text(CT, ConnStates0, Check) of
	{Plain, ConnStates} ->		      
	    {Plain, State#state{protocol_buffers =
				    Buffers#protocol_buffers{tls_cipher_texts = Rest},
				connection_states = ConnStates}};
	#alert{} = Alert ->
	    {Alert, State}
    end;
next_record(#state{protocol_buffers = #protocol_buffers{tls_packets = [], tls_cipher_texts = []},
		   socket = Socket,
		   transport_cb = Transport} = State) ->
    tls_socket:setopts(Transport, Socket, [{active,once}]),
    {no_record, State};
next_record(State) ->
    {no_record, State}.

next_record_if_active(State = 
		      #state{socket_options = 
			     #socket_options{active = false}}) -> 
    {no_record ,State};

next_record_if_active(State) ->
    next_record(State).

passive_receive(State0 = #state{user_data_buffer = Buffer}, StateName) -> 
    case Buffer of
	<<>> ->
	    {Record, State} = next_record(State0),
	    next_event(StateName, Record, State);
	_ ->
	    {Record, State} = ssl_connection:read_application_data(<<>>, State0),
	    next_event(StateName, Record, State)
    end.

next_event(StateName, Record, State) ->
    next_event(StateName, Record, State, []).

next_event(connection = StateName, no_record, State0, Actions) ->
    case next_record_if_active(State0) of
	{no_record, State} ->
	    ssl_connection:hibernate_after(StateName, State, Actions);
	{#ssl_tls{} = Record, State} ->
	    {next_state, StateName, State, [{next_event, internal, {protocol_record, Record}} | Actions]};
	{#alert{} = Alert, State} ->
	    {next_state, StateName, State, [{next_event, internal, Alert} | Actions]}
    end;
next_event(StateName, Record, State, Actions) ->
    case Record of
	no_record ->
	    {next_state, StateName, State, Actions};
	#ssl_tls{} = Record ->
	    {next_state, StateName, State, [{next_event, internal, {protocol_record, Record}} | Actions]};
	#alert{} = Alert ->
	    {next_state, StateName, State, [{next_event, internal, Alert} | Actions]}
    end.

tls_handshake_events(Packets) ->
    lists:map(fun(Packet) ->
		      {next_event, internal, {handshake, Packet}}
	      end, Packets).


renegotiate(#state{role = client} = State, Actions) ->
    %% Handle same way as if server requested
    %% the renegotiation
    Hs0 = ssl_handshake:init_handshake_history(),
    {next_state, connection, State#state{tls_handshake_history = Hs0}, 
     [{next_event, internal, #hello_request{}} | Actions]};

renegotiate(#state{role = server,
		   socket = Socket,
		   transport_cb = Transport,
		   negotiated_version = Version,
		   connection_states = ConnectionStates0} = State0, Actions) ->
    HelloRequest = ssl_handshake:hello_request(),
    Frag = tls_handshake:encode_handshake(HelloRequest, Version),
    Hs0 = ssl_handshake:init_handshake_history(),
    {BinMsg, ConnectionStates} = 
	tls_record:encode_handshake(Frag, Version, ConnectionStates0),
    send(Transport, Socket, BinMsg),
    State1 = State0#state{connection_states = 
			     ConnectionStates,
			 tls_handshake_history = Hs0},
    {Record, State} = next_record(State1),
    next_event(hello, Record, State, Actions).

handle_alerts([], Result) ->
    Result;
handle_alerts(_, {stop,_} = Stop) ->
    Stop;
handle_alerts([Alert | Alerts], {next_state, StateName, State}) ->
     handle_alerts(Alerts, ssl_connection:handle_alert(Alert, StateName, State));
handle_alerts([Alert | Alerts], {next_state, StateName, State, _Actions}) ->
     handle_alerts(Alerts, ssl_connection:handle_alert(Alert, StateName, State)).


%% User closes or recursive call!
close({close, Timeout}, Socket, Transport = gen_tcp, _,_) ->
    tls_socket:setopts(Transport, Socket, [{active, false}]),
    Transport:shutdown(Socket, write),
    _ = Transport:recv(Socket, 0, Timeout),
    ok;
%% Peer closed socket
close({shutdown, transport_closed}, Socket, Transport = gen_tcp, ConnectionStates, Check) ->
    close({close, 0}, Socket, Transport, ConnectionStates, Check);
%% We generate fatal alert
close({shutdown, own_alert}, Socket, Transport = gen_tcp, ConnectionStates, Check) ->
    %% Standard trick to try to make sure all
    %% data sent to the tcp port is really delivered to the
    %% peer application before tcp port is closed so that the peer will
    %% get the correct TLS alert message and not only a transport close.
    %% Will return when other side has closed or after timout millisec
    %% e.g. we do not want to hang if something goes wrong
    %% with the network but we want to maximise the odds that
    %% peer application gets all data sent on the tcp connection.
    close({close, ?DEFAULT_TIMEOUT}, Socket, Transport, ConnectionStates, Check);
close(downgrade, _,_,_,_) ->
    ok;
%% Other
close(_, Socket, Transport, _,_) -> 
    Transport:close(Socket).
	       
convert_state(#state{ssl_options = Options} = State, up, "5.3.5", "5.3.6") ->
    State#state{ssl_options = convert_options_partial_chain(Options, up)};
convert_state(#state{ssl_options = Options} = State, down, "5.3.6", "5.3.5") ->
    State#state{ssl_options = convert_options_partial_chain(Options, down)}.

convert_options_partial_chain(Options, up) ->
    {Head, Tail} = lists:split(5, tuple_to_list(Options)),
    list_to_tuple(Head ++ [{partial_chain, fun(_) -> unknown_ca end}] ++ Tail);
convert_options_partial_chain(Options, down) ->
    list_to_tuple(proplists:delete(partial_chain, tuple_to_list(Options))).

gen_handshake(GenConnection, StateName, Type, Event, 
	      #state{negotiated_version = Version} = State) ->
    try GenConnection:StateName(Type, Event, State, ?MODULE) of
	Result ->
	    Result
    catch 
	_:_ ->
 	    ssl_connection:handle_own_alert(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, 
						       malformed_handshake_data),
					    Version, StateName, State)  
    end.
	    
gen_info(Event, connection = StateName,  #state{negotiated_version = Version} = State) ->
    try handle_info(Event, StateName, State) of
	Result ->
	    Result
    catch 
	_:_ ->
	    ssl_connection:handle_own_alert(?ALERT_REC(?FATAL, ?INTERNAL_ERROR, 
						       malformed_data), 
					    Version, StateName, State)  
    end;

gen_info(Event, StateName, #state{negotiated_version = Version} = State) ->
    try handle_info(Event, StateName, State) of
	Result ->
	    Result
    catch 
        _:_ ->
	    ssl_connection:handle_own_alert(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, 
						       malformed_handshake_data), 
					    Version, StateName, State)  
    end.
	    
unprocessed_events(Events) ->
    %% The first handshake event will be processed immediately
    %% as it is entered first in the event queue and
    %% when it is processed there will be length(Events)-1
    %% handshake events left to process before we should
    %% process more TLS-records received on the socket. 
    erlang:length(Events)-1.


assert_buffer_sanity(<<?BYTE(_Type), ?UINT24(Length), Rest/binary>>, #ssl_options{max_handshake_size = Max}) when 
      Length =< Max ->  
    case size(Rest) of
        N when N < Length ->
            true;
        N when N > Length ->       
            throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, 
                             too_big_handshake_data));
        _ ->
            throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, 
                             malformed_handshake_data))  
    end;  
assert_buffer_sanity(Bin, _) ->
    case size(Bin) of
        N when N < 3 ->
            true;
        _ ->       
            throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, 
                             malformed_handshake_data))
    end.  
