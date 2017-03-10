%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2017. All Rights Reserved.
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
-module(dtls_connection).

%% Internal application API

-behaviour(gen_statem).

-include("dtls_connection.hrl").
-include("dtls_handshake.hrl").
-include("ssl_alert.hrl").
-include("dtls_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_api.hrl").
-include("ssl_internal.hrl").
-include("ssl_srp.hrl").
-include_lib("public_key/include/public_key.hrl"). 

%% Internal application API

%% Setup
-export([start_fsm/8, start_link/7, init/1]).

%% State transition handling	 
-export([next_record/1, next_event/3, next_event/4]).

%% Handshake handling
-export([renegotiate/2, 
	 reinit_handshake_data/1, 
	 send_handshake/2, queue_handshake/2, queue_change_cipher/2,
	 select_sni_extension/1]).

%% Alert and close handling
-export([encode_alert/3,send_alert/2, close/5]).

%% Data handling

-export([encode_data/3, passive_receive/2,  next_record_if_active/1, handle_common_event/4,
	 send/3, socket/5]).

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
	{ok, Pid} = dtls_connection_sup:start_child([Role, Host, Port, Socket, 
						     Opts, User, CbInfo]), 
	{ok, SslSocket} = ssl_connection:socket_control(?MODULE, Socket, Pid, CbModule, Tracker),
	ok = ssl_connection:handshake(SslSocket, Timeout),
	{ok, SslSocket} 
    catch
	error:{badmatch, {error, _} = Error} ->
	    Error
    end.

send_handshake(Handshake, #state{connection_states = ConnectionStates} = States) ->
    #{epoch := Epoch} = ssl_record:current_connection_state(ConnectionStates, write),
    send_handshake_flight(queue_handshake(Handshake, States), Epoch).

queue_handshake(Handshake0, #state{tls_handshake_history = Hist0, 
				   negotiated_version = Version,
				   flight_buffer = #{handshakes := HsBuffer0,
						     change_cipher_spec := undefined,
						     next_sequence := Seq} = Flight0} = State) ->
    Handshake = dtls_handshake:encode_handshake(Handshake0, Version, Seq),
    Hist = update_handshake_history(Handshake0, Handshake, Hist0),
    State#state{flight_buffer = Flight0#{handshakes => [Handshake | HsBuffer0],
					 next_sequence => Seq +1},
		tls_handshake_history = Hist};

queue_handshake(Handshake0, #state{tls_handshake_history = Hist0, 
				   negotiated_version = Version,
				   flight_buffer = #{handshakes_after_change_cipher_spec := Buffer0,
						     next_sequence := Seq} = Flight0} = State) ->
    Handshake = dtls_handshake:encode_handshake(Handshake0, Version, Seq),
    Hist = update_handshake_history(Handshake0, Handshake, Hist0),
    State#state{flight_buffer = Flight0#{handshakes_after_change_cipher_spec => [Handshake | Buffer0],
					 next_sequence => Seq +1},
		tls_handshake_history = Hist}.


send_handshake_flight(#state{socket = Socket,
			     transport_cb = Transport,
			     flight_buffer = #{handshakes := Flight,
					       change_cipher_spec := undefined},
			     negotiated_version = Version,
			     connection_states = ConnectionStates0} = State0, Epoch) ->
    %% TODO remove hardcoded Max size
    {Encoded, ConnectionStates} =
	encode_handshake_flight(lists:reverse(Flight), Version, 1400, Epoch, ConnectionStates0),
    send(Transport, Socket, Encoded),
    start_flight(State0#state{connection_states = ConnectionStates});

send_handshake_flight(#state{socket = Socket,
			     transport_cb = Transport,
			     flight_buffer = #{handshakes := [_|_] = Flight0,
					       change_cipher_spec := ChangeCipher,
					       handshakes_after_change_cipher_spec := []},
			     negotiated_version = Version,
			     connection_states = ConnectionStates0} = State0, Epoch) ->      
    {HsBefore, ConnectionStates1} =
	encode_handshake_flight(lists:reverse(Flight0), Version, 1400, Epoch, ConnectionStates0),
    {EncChangeCipher, ConnectionStates} = encode_change_cipher(ChangeCipher, Version, Epoch, ConnectionStates1),

    send(Transport, Socket, [HsBefore, EncChangeCipher]),
    start_flight(State0#state{connection_states = ConnectionStates});

send_handshake_flight(#state{socket = Socket,
			     transport_cb = Transport,
			     flight_buffer = #{handshakes := [_|_] = Flight0,
					       change_cipher_spec := ChangeCipher,
					       handshakes_after_change_cipher_spec := Flight1},
			     negotiated_version = Version,
			     connection_states = ConnectionStates0} = State0, Epoch) ->      
    {HsBefore, ConnectionStates1} =
	encode_handshake_flight(lists:reverse(Flight0), Version, 1400, Epoch-1, ConnectionStates0),
    {EncChangeCipher, ConnectionStates2} = 
	encode_change_cipher(ChangeCipher, Version, Epoch-1, ConnectionStates1),
    {HsAfter, ConnectionStates} =
	encode_handshake_flight(lists:reverse(Flight1), Version, 1400, Epoch, ConnectionStates2),
    send(Transport, Socket, [HsBefore, EncChangeCipher, HsAfter]),
    start_flight(State0#state{connection_states = ConnectionStates});

send_handshake_flight(#state{socket = Socket,
			     transport_cb = Transport,
			     flight_buffer = #{handshakes := [],
					       change_cipher_spec := ChangeCipher,
					       handshakes_after_change_cipher_spec := Flight1},
			     negotiated_version = Version,
			     connection_states = ConnectionStates0} = State0, Epoch) ->
    {EncChangeCipher, ConnectionStates1} = 
	encode_change_cipher(ChangeCipher, Version, Epoch-1, ConnectionStates0),
    {HsAfter, ConnectionStates} =
	encode_handshake_flight(lists:reverse(Flight1), Version, 1400, Epoch, ConnectionStates1),
    send(Transport, Socket, [EncChangeCipher, HsAfter]),
    start_flight(State0#state{connection_states = ConnectionStates}).

queue_change_cipher(ChangeCipher, #state{flight_buffer = Flight,
					 connection_states = ConnectionStates0} = State) -> 
    ConnectionStates = 
	dtls_record:next_epoch(ConnectionStates0, write), 
    State#state{flight_buffer = Flight#{change_cipher_spec => ChangeCipher},
		connection_states = ConnectionStates}.

send_alert(Alert, #state{negotiated_version = Version,
			 socket = Socket,
			 transport_cb = Transport,
			 connection_states = ConnectionStates0} = State0) ->
    {BinMsg, ConnectionStates} =
	encode_alert(Alert, Version, ConnectionStates0),
    send(Transport, Socket, BinMsg),
    State0#state{connection_states = ConnectionStates}.

close(downgrade, _,_,_,_) ->
    ok;
%% Other
close(_, Socket, Transport, _,_) ->
    dtls_socket:close(Transport,Socket).

reinit_handshake_data(#state{protocol_buffers = Buffers} = State) ->
    State#state{premaster_secret = undefined,
		public_key_info = undefined,
		tls_handshake_history = ssl_handshake:init_handshake_history(),
                flight_state = {retransmit, ?INITIAL_RETRANSMIT_TIMEOUT},
		protocol_buffers =
		    Buffers#protocol_buffers{
		      dtls_handshake_next_seq = 0,
		      dtls_handshake_next_fragments = [],
		      dtls_handshake_later_fragments = []
		     }}.

select_sni_extension(#client_hello{extensions = HelloExtensions}) ->
    HelloExtensions#hello_extensions.sni;
select_sni_extension(_) ->
    undefined.

socket(Pid,  Transport, Socket, Connection, _) ->
    dtls_socket:socket(Pid, Transport, Socket, Connection).

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
    State0 =  initial_state(Role, Host, Port, Socket, Options, User, CbInfo),
    try
	State = ssl_connection:ssl_config(State0#state.ssl_options, Role, State0),
	gen_statem:enter_loop(?MODULE, [], init, State)
    catch
	throw:Error ->
	    gen_statem:enter_loop(?MODULE, [], error, {Error,State0})
    end.

callback_mode() ->
    state_functions.

%%--------------------------------------------------------------------
%% State functions 
%%--------------------------------------------------------------------

init({call, From}, {start, Timeout}, 
     #state{host = Host, port = Port, role = client,
	    ssl_options = SslOpts,
	    session = #session{own_certificate = Cert} = Session0,
	    connection_states = ConnectionStates0,
	    renegotiation = {Renegotiation, _},
	    session_cache = Cache,
	    session_cache_cb = CacheCb
	   } = State0) ->
    Timer = ssl_connection:start_or_recv_cancel_timer(Timeout, From),
    Hello = dtls_handshake:client_hello(Host, Port, ConnectionStates0, SslOpts,
					Cache, CacheCb, Renegotiation, Cert),

    Version = Hello#client_hello.client_version,
    HelloVersion = dtls_record:lowest_protocol_version(SslOpts#ssl_options.versions),
    State1 = prepare_flight(State0#state{negotiated_version = Version}),
    {State2, Actions} = send_handshake(Hello, State1#state{negotiated_version = HelloVersion}),  
    State3 = State2#state{negotiated_version = Version, %% Requested version
			  session =
			      Session0#session{session_id = Hello#client_hello.session_id},
			  start_or_recv_from = From,
			  timer = Timer,
                          flight_state = {retransmit, ?INITIAL_RETRANSMIT_TIMEOUT}
                         },
    {Record, State} = next_record(State3),
    next_event(hello, Record, State, Actions);
init({call, _} = Type, Event, #state{role = server, transport_cb = gen_udp} = State) ->
    ssl_connection:init(Type, Event, 
			State#state{flight_state = {retransmit, ?INITIAL_RETRANSMIT_TIMEOUT}},
			?MODULE);
init({call, _} = Type, Event, #state{role = server} = State) ->
    %% I.E. DTLS over sctp
    ssl_connection:init(Type, Event, State#state{flight_state = reliable}, ?MODULE);
init(Type, Event, State) ->
    ssl_connection:init(Type, Event, State, ?MODULE).
 
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
hello(internal, #client_hello{cookie = <<>>,
			      client_version = Version} = Hello, #state{role = server,
									transport_cb = Transport,
									socket = Socket} = State0) ->
    %% TODO: not hard code key
    {ok, {IP, Port}} = dtls_socket:peername(Transport, Socket),
    Cookie = dtls_handshake:cookie(<<"secret">>, IP, Port, Hello),
    VerifyRequest = dtls_handshake:hello_verify_request(Cookie, Version),
    State1 = prepare_flight(State0#state{negotiated_version = Version}),
    {State2, Actions} = send_handshake(VerifyRequest, State1),
    {Record, State} = next_record(State2),
    next_event(hello, Record, State#state{tls_handshake_history = ssl_handshake:init_handshake_history()}, Actions);
hello(internal, #client_hello{cookie = Cookie} = Hello, #state{role = server,
							       transport_cb = Transport,
							       socket = Socket} = State0) ->
    {ok, {IP, Port}} = dtls_socket:peername(Transport, Socket),
    %% TODO: not hard code key
    case dtls_handshake:cookie(<<"secret">>, IP, Port, Hello) of
	Cookie ->
	    handle_client_hello(Hello, State0);
	_ ->
	    %% Handle bad cookie as new cookie request RFC 6347 4.1.2
	    hello(internal, Hello#client_hello{cookie = <<>>}, State0) 
    end;
hello(internal, #hello_verify_request{cookie = Cookie}, #state{role = client,
							       host = Host, port = Port, 
							       ssl_options = SslOpts,
							       session = #session{own_certificate = OwnCert} 
							       = Session0,
							       connection_states = ConnectionStates0,
							       renegotiation = {Renegotiation, _},
							       session_cache = Cache,
							       session_cache_cb = CacheCb
							      } = State0) ->
    State1 = prepare_flight(State0#state{tls_handshake_history = ssl_handshake:init_handshake_history()}),
    Hello = dtls_handshake:client_hello(Host, Port, Cookie, ConnectionStates0,
					SslOpts,
					Cache, CacheCb, Renegotiation, OwnCert),
    Version = Hello#client_hello.client_version,
    HelloVersion = dtls_record:lowest_protocol_version(SslOpts#ssl_options.versions),
    {State2, Actions} = send_handshake(Hello, State1#state{negotiated_version = HelloVersion}), 
    State3 = State2#state{negotiated_version = Version, %% Requested version
			  session =
			      Session0#session{session_id = 
						   Hello#client_hello.session_id}},
    {Record, State} = next_record(State3),
    next_event(hello, Record, State, Actions);
hello(internal, #server_hello{} = Hello,
      #state{connection_states = ConnectionStates0,
	     negotiated_version = ReqVersion,
	     role = client,
	     renegotiation = {Renegotiation, _},
	     ssl_options = SslOptions} = State) ->
    case dtls_handshake:hello(Hello, SslOptions, ConnectionStates0, Renegotiation) of
	#alert{} = Alert ->
	    ssl_connection:handle_own_alert(Alert, ReqVersion, hello, State);
	{Version, NewId, ConnectionStates, ProtoExt, Protocol} ->
	    ssl_connection:handle_session(Hello, 
					  Version, NewId, ConnectionStates, ProtoExt, Protocol, State)
    end;
hello(internal, {handshake, {#client_hello{cookie = <<>>} = Handshake, _}}, State) ->
    %% Initial hello should not be in handshake history
    {next_state, hello, State, [{next_event, internal, Handshake}]};
hello(internal, {handshake, {#hello_verify_request{} = Handshake, _}}, State) ->
    %% hello_verify should not be in handshake history
    {next_state, hello, State, [{next_event, internal, Handshake}]};
hello(info, Event, State) ->
    handle_info(Event, hello, State);
hello(state_timeout, Event, State) ->
    handle_state_timeout(Event, hello, State);
hello(Type, Event, State) ->
    ssl_connection:hello(Type, Event, State, ?MODULE).

abbreviated(info, Event, State) ->
    handle_info(Event, abbreviated, State);
abbreviated(internal = Type, 
	    #change_cipher_spec{type = <<1>>} = Event, 
	    #state{connection_states = ConnectionStates0} = State) ->
    ConnectionStates1 = dtls_record:save_current_connection_state(ConnectionStates0, read),
    ConnectionStates = dtls_record:next_epoch(ConnectionStates1, read),
    ssl_connection:abbreviated(Type, Event, State#state{connection_states = ConnectionStates}, ?MODULE);
abbreviated(internal = Type, #finished{} = Event, #state{connection_states = ConnectionStates} = State) ->
    ssl_connection:abbreviated(Type, Event, 
                               prepare_flight(State#state{connection_states = ConnectionStates,
                                                          flight_state = connection}), ?MODULE);
abbreviated(state_timeout, Event, State) ->
    handle_state_timeout(Event, abbreviated, State);
abbreviated(Type, Event, State) ->
    ssl_connection:abbreviated(Type, Event, State, ?MODULE).

certify(info, Event, State) ->
    handle_info(Event, certify, State);
certify(internal = Type, #server_hello_done{} = Event, State) ->
    ssl_connection:certify(Type, Event, prepare_flight(State), ?MODULE);
certify(state_timeout, Event, State) ->
    handle_state_timeout(Event, certify, State);
certify(Type, Event, State) ->
    ssl_connection:certify(Type, Event, State, ?MODULE).

cipher(info, Event, State) ->
    handle_info(Event, cipher, State);
cipher(internal = Type, #change_cipher_spec{type = <<1>>} = Event,  
       #state{connection_states = ConnectionStates0} = State) ->
    ConnectionStates1 = dtls_record:save_current_connection_state(ConnectionStates0, read),
    ConnectionStates = dtls_record:next_epoch(ConnectionStates1, read),
    ssl_connection:cipher(Type, Event, State#state{connection_states = ConnectionStates}, ?MODULE);
cipher(internal = Type, #finished{} = Event, #state{connection_states = ConnectionStates} = State) ->
    ssl_connection:cipher(Type, Event, 
			  prepare_flight(State#state{connection_states = ConnectionStates,
                                                     flight_state = connection}), 
                          ?MODULE);
cipher(state_timeout, Event, State) ->
    handle_state_timeout(Event, cipher, State);
cipher(Type, Event, State) ->
     ssl_connection:cipher(Type, Event, State, ?MODULE).

connection(info, Event, State) ->
    handle_info(Event, connection, State);
connection(internal, #hello_request{}, #state{host = Host, port = Port,
				    session = #session{own_certificate = Cert} = Session0,
				    session_cache = Cache, session_cache_cb = CacheCb,
				    ssl_options = SslOpts,
				    connection_states = ConnectionStates0,
				    renegotiation = {Renegotiation, _}} = State0) ->
    Hello = dtls_handshake:client_hello(Host, Port, ConnectionStates0, SslOpts,
					Cache, CacheCb, Renegotiation, Cert),
    {State1, Actions} = send_handshake(Hello, State0),
    {Record, State} =
	next_record(
	  State1#state{session = Session0#session{session_id
						  = Hello#client_hello.session_id}}),
    next_event(hello, Record, State, Actions);
connection(internal, #client_hello{} = Hello, #state{role = server, allow_renegotiate = true} = State) ->
    %% Mitigate Computational DoS attack
    %% http://www.educatedguesswork.org/2011/10/ssltls_and_computational_dos.html
    %% http://www.thc.org/thc-ssl-dos/ Rather than disabling client
    %% initiated renegotiation we will disallow many client initiated
    %% renegotiations immediately after each other.
    erlang:send_after(?WAIT_TO_ALLOW_RENEGOTIATION, self(), allow_renegotiate),
    {next_state, hello, State#state{allow_renegotiate = false}, [{next_event, internal, Hello}]};
connection(internal, #client_hello{}, #state{role = server, allow_renegotiate = false} = State0) ->
    Alert = ?ALERT_REC(?WARNING, ?NO_RENEGOTIATION),
    State1 = send_alert(Alert, State0),
    {Record, State} = ssl_connection:prepare_connection(State1, ?MODULE),
    next_event(connection, Record, State);
connection(Type, Event, State) ->
     ssl_connection:connection(Type, Event, State, ?MODULE).

downgrade(Type, Event, State) ->
     ssl_connection:downgrade(Type, Event, State, ?MODULE).

%%--------------------------------------------------------------------
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------

%% raw data from socket, unpack records
handle_info({Protocol, _, _, _, Data}, StateName,
            #state{data_tag = Protocol} = State0) ->
    case next_dtls_record(Data, State0) of
	{Record, State} ->
	    next_event(StateName, Record, State);
	#alert{} = Alert ->
	    ssl_connection:handle_normal_shutdown(Alert, StateName, State0), 
	    {stop, {shutdown, own_alert}}
    end;
handle_info({CloseTag, Socket}, StateName,
	    #state{socket = Socket, close_tag = CloseTag,
		   negotiated_version = Version} = State) ->
    %% Note that as of DTLS 1.2 (TLS 1.1),
    %% failure to properly close a connection no longer requires that a
    %% session not be resumed.	This is a change from DTLS 1.0 to conform
    %% with widespread implementation practice.
    case Version of
	{254, N} when N =< 253 ->
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
handle_info(Msg, StateName, State) ->
    ssl_connection:handle_info(Msg, StateName, State).

handle_call(Event, From, StateName, State) ->
    ssl_connection:handle_call(Event, From, StateName, State, ?MODULE).

handle_common_event(internal, #alert{} = Alert, StateName, 
		    #state{negotiated_version = Version} = State) ->
    ssl_connection:handle_own_alert(Alert, Version, StateName, State);
%%% DTLS record protocol level handshake messages 
handle_common_event(internal, #ssl_tls{type = ?HANDSHAKE,
				       fragment = Data}, 
		    StateName, 
		    #state{protocol_buffers = Buffers0,
			   negotiated_version = Version} = State0) ->
    try
	case dtls_handshake:get_dtls_handshake(Version, Data, Buffers0) of
	    {[], Buffers} ->
		{Record, State} = next_record(State0#state{protocol_buffers = Buffers}),
		next_event(StateName, Record, State);
	    {Packets, Buffers} ->
		State = State0#state{protocol_buffers = Buffers},
		Events = dtls_handshake_events(Packets),
                {next_state, StateName, 
                 State#state{unprocessed_handshake_events = unprocessed_events(Events)}, Events}
	end
    catch throw:#alert{} = Alert ->
	    ssl_connection:handle_own_alert(Alert, Version, StateName, State0)
    end;
%%% DTLS record protocol level application data messages 
handle_common_event(internal, #ssl_tls{type = ?APPLICATION_DATA, fragment = Data}, StateName, State) ->
    {next_state, StateName, State, [{next_event, internal, {application_data, Data}}]};
%%% DTLS record protocol level change cipher messages
handle_common_event(internal, #ssl_tls{type = ?CHANGE_CIPHER_SPEC, fragment = Data}, StateName, State) ->
    {next_state, StateName, State, [{next_event, internal, #change_cipher_spec{type = Data}}]};
%%% DTLS record protocol level Alert messages
handle_common_event(internal, #ssl_tls{type = ?ALERT, fragment = EncAlerts}, StateName,
		    #state{negotiated_version = Version} = State) ->
    case decode_alerts(EncAlerts) of
	Alerts = [_|_] ->
	    handle_alerts(Alerts,  {next_state, StateName, State});
	#alert{} = Alert ->
	    ssl_connection:handle_own_alert(Alert, Version, StateName, State)
    end;
%% Ignore unknown TLS record level protocol messages
handle_common_event(internal, #ssl_tls{type = _Unknown}, StateName, State) ->
    {next_state, StateName, State}.

handle_state_timeout(flight_retransmission_timeout, StateName, 
                    #state{flight_state = {retransmit, NextTimeout}} = State0) ->
    {State1, Actions} = send_handshake_flight(State0#state{flight_state = {retransmit, NextTimeout}}, 
                                              retransmit_epoch(StateName, State0)),
    {Record, State} = next_record(State1),
    next_event(StateName, Record, State, Actions).

send(Transport, {_, {{_,_}, _} = Socket}, Data) ->
    send(Transport, Socket, Data);
send(Transport, Socket, Data) ->
   dtls_socket:send(Transport, Socket, Data).
%%--------------------------------------------------------------------
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, StateName, State) ->
    ssl_connection:terminate(Reason, StateName, State).

%%--------------------------------------------------------------------
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

format_status(Type, Data) ->
    ssl_connection:format_status(Type, Data).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
handle_client_hello(#client_hello{client_version = ClientVersion} = Hello,
		    #state{connection_states = ConnectionStates0,
			   port = Port, session = #session{own_certificate = Cert} = Session0,
			   renegotiation = {Renegotiation, _},
			   session_cache = Cache,
			   session_cache_cb = CacheCb,
			   negotiated_protocol = CurrentProtocol,
			   key_algorithm = KeyExAlg,
			   ssl_options = SslOpts} = State0) ->
    
    case dtls_handshake:hello(Hello, SslOpts, {Port, Session0, Cache, CacheCb,
					       ConnectionStates0, Cert, KeyExAlg}, Renegotiation) of
	#alert{} = Alert ->
	    ssl_connection:handle_own_alert(Alert, ClientVersion, hello, State0);
	{Version, {Type, Session},
	 ConnectionStates, Protocol0, ServerHelloExt, HashSign} ->
	    Protocol = case Protocol0 of
			   undefined -> CurrentProtocol;
			   _ -> Protocol0
		       end,

	    State = prepare_flight(State0#state{connection_states = ConnectionStates,
						negotiated_version = Version,
						hashsign_algorithm = HashSign,
						session = Session,
						negotiated_protocol = Protocol}),
	    
	    ssl_connection:hello(internal, {common_client_hello, Type, ServerHelloExt},
				 State, ?MODULE)
    end.

encode_handshake_flight(Flight, Version, MaxFragmentSize, Epoch, ConnectionStates) ->
    Fragments = lists:map(fun(Handshake) ->
				  dtls_handshake:fragment_handshake(Handshake, MaxFragmentSize)
			  end, Flight),
    dtls_record:encode_handshake(Fragments, Version, Epoch, ConnectionStates).

encode_change_cipher(#change_cipher_spec{}, Version, Epoch, ConnectionStates) ->
    dtls_record:encode_change_cipher_spec(Version, Epoch, ConnectionStates).

encode_data(Data, Version, ConnectionStates0)->
    dtls_record:encode_data(Data, Version, ConnectionStates0).

encode_alert(#alert{} = Alert, Version, ConnectionStates) ->
    dtls_record:encode_alert_record(Alert, Version, ConnectionStates).

decode_alerts(Bin) ->
    ssl_alert:decode(Bin).

initial_state(Role, Host, Port, Socket, {SSLOptions, SocketOptions, _}, User,
	      {CbModule, DataTag, CloseTag, ErrorTag}) ->
    #ssl_options{beast_mitigation = BeastMitigation} = SSLOptions,
    ConnectionStates = dtls_record:init_connection_states(Role, BeastMitigation),
    
    SessionCacheCb = case application:get_env(ssl, session_cb) of
			 {ok, Cb} when is_atom(Cb) ->
			    Cb;
			 _  ->
			     ssl_session_cache
		     end,
    
    Monitor = erlang:monitor(process, User),

    #state{socket_options = SocketOptions,
	   %% We do not want to save the password in the state so that
	   %% could be written in the clear into error logs.
	   ssl_options = SSLOptions#ssl_options{password = undefined},	   
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
	   flight_buffer = new_flight(),
           flight_state = {retransmit, ?INITIAL_RETRANSMIT_TIMEOUT}
	  }.

next_dtls_record(Data, #state{protocol_buffers = #protocol_buffers{
						   dtls_record_buffer = Buf0,
						   dtls_cipher_texts = CT0} = Buffers} = State0) ->
    case dtls_record:get_dtls_records(Data, Buf0) of
	{Records, Buf1} ->
	    CT1 = CT0 ++ Records,
	    next_record(State0#state{protocol_buffers =
					 Buffers#protocol_buffers{dtls_record_buffer = Buf1,
								  dtls_cipher_texts = CT1}});
	#alert{} = Alert ->
	    Alert
    end.

next_record(#state{unprocessed_handshake_events = N} = State) when N > 0 ->
    {no_record, State#state{unprocessed_handshake_events = N-1}};
					 
next_record(#state{protocol_buffers =
		       #protocol_buffers{dtls_cipher_texts = [CT | Rest]}
		   = Buffers,
		   connection_states = ConnStates0} = State) ->
    case dtls_record:decode_cipher_text(CT, ConnStates0) of
	{Plain, ConnStates} ->		      
	    {Plain, State#state{protocol_buffers =
				    Buffers#protocol_buffers{dtls_cipher_texts = Rest},
				connection_states = ConnStates}};
	#alert{} = Alert ->
	    {Alert, State}
    end;
next_record(#state{role = server,
		   socket = {Listener, {Client, _}},
		   transport_cb = gen_udp} = State) -> 
    dtls_udp_listener:active_once(Listener, Client, self()),
    {no_record, State};
next_record(#state{role = client,
		   socket = {_Server, Socket},
		   transport_cb = Transport} = State) -> 
    dtls_socket:setopts(Transport, Socket, [{active,once}]),
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

next_event(connection = StateName, no_record,
	   #state{connection_states = #{current_read := #{epoch := CurrentEpoch}}} = State0, Actions) ->
    case next_record_if_active(State0) of
	{no_record, State} ->
            ssl_connection:hibernate_after(StateName, State, Actions);
	{#ssl_tls{epoch = CurrentEpoch} = Record, State} ->
	    {next_state, StateName, State, [{next_event, internal, {protocol_record, Record}} | Actions]};
	{#ssl_tls{epoch = Epoch,
		  type = ?HANDSHAKE,
		  version = _Version}, State1} = _Record when Epoch == CurrentEpoch-1 ->
	    {State, MoreActions} = send_handshake_flight(State1, Epoch),
	    {next_state, StateName, State, Actions ++ MoreActions};
	{#ssl_tls{epoch = _Epoch,
		  version = _Version}, State} ->
	    %% TODO maybe buffer later epoch
	    {next_state, StateName, State, Actions};
	{#alert{} = Alert, State} ->
	    {next_state, StateName, State, [{next_event, internal, Alert} | Actions]}
    end;
next_event(StateName, Record, 
	   #state{connection_states = #{current_read := #{epoch := CurrentEpoch}}} = State, Actions) ->
    case Record of
	no_record ->
	    {next_state, StateName, State, Actions};
	#ssl_tls{epoch = CurrentEpoch,
		  version = Version} = Record ->
	    {next_state, StateName, 
	     dtls_version(StateName, Version, State), 
	     [{next_event, internal, {protocol_record, Record}} | Actions]};
	#ssl_tls{epoch = _Epoch,
		 version = _Version} = _Record ->
	    %% TODO maybe buffer later epoch
	    {next_state, StateName, State, Actions};
	#alert{} = Alert ->
	    {next_state, StateName, State, [{next_event, internal, Alert} | Actions]}
    end.

dtls_version(hello, Version, #state{role = server} = State) ->
    State#state{negotiated_version = Version}; %%Inital version
dtls_version(_,_, State) ->
    State.

prepare_flight(#state{flight_buffer = Flight,
		      connection_states = ConnectionStates0,
		      protocol_buffers = 
			  #protocol_buffers{} = Buffers} = State) ->
    ConnectionStates = dtls_record:save_current_connection_state(ConnectionStates0, write),
    State#state{flight_buffer = next_flight(Flight),
		connection_states = ConnectionStates,
		protocol_buffers = Buffers#protocol_buffers{
				     dtls_handshake_next_fragments = [],
				     dtls_handshake_later_fragments = []}}.
new_flight() ->
    #{next_sequence => 0,
      handshakes => [],
      change_cipher_spec => undefined,
      handshakes_after_change_cipher_spec => []}.

next_flight(Flight) ->
    Flight#{handshakes => [],
	    change_cipher_spec => undefined,
	    handshakes_after_change_cipher_spec => []}.
	
start_flight(#state{transport_cb = gen_udp,
		    flight_state = {retransmit, Timeout}} = State) ->
    start_retransmision_timer(Timeout, State);
start_flight(#state{transport_cb = gen_udp,
		    flight_state = connection} = State) ->
    {State, []};
start_flight(State) ->
    %% No retransmision needed i.e DTLS over SCTP
    {State#state{flight_state = reliable}, []}.

start_retransmision_timer(Timeout, State) ->
    {State#state{flight_state = {retransmit, new_timeout(Timeout)}}, 
     [{state_timeout, Timeout, flight_retransmission_timeout}]}.

new_timeout(N) when N =< 30 -> 
    N * 2;
new_timeout(_) -> 
    60.

dtls_handshake_events(Packets) ->
    lists:map(fun(Packet) ->
		      {next_event, internal, {handshake, Packet}}
	      end, Packets).

renegotiate(#state{role = client} = State, Actions) ->
    %% Handle same way as if server requested
    %% the renegotiation
    Hs0 = ssl_handshake:init_handshake_history(),
    {next_state, connection, State#state{tls_handshake_history = Hs0,
					 protocol_buffers = #protocol_buffers{}},
     [{next_event, internal, #hello_request{}} | Actions]};

renegotiate(#state{role = server,
		   connection_states = CS0} = State0, Actions) ->
    HelloRequest = ssl_handshake:hello_request(),
    CS = CS0#{write_msg_seq => 0},
    {State1, MoreActions} = send_handshake(HelloRequest, 
                                           State0#state{connection_states =
                                                        CS}),
    Hs0 = ssl_handshake:init_handshake_history(),
    {Record, State} = next_record(State1#state{tls_handshake_history = Hs0,
					       protocol_buffers = #protocol_buffers{}}),
    next_event(hello, Record, State, Actions ++ MoreActions).

handle_alerts([], Result) ->
    Result;
handle_alerts(_, {stop,_} = Stop) ->
    Stop;
handle_alerts([Alert | Alerts], {next_state, StateName, State}) ->
     handle_alerts(Alerts, ssl_connection:handle_alert(Alert, StateName, State));
handle_alerts([Alert | Alerts], {next_state, StateName, State, _Actions}) ->
     handle_alerts(Alerts, ssl_connection:handle_alert(Alert, StateName, State)).

retransmit_epoch(_StateName, #state{connection_states = ConnectionStates}) ->
    #{epoch := Epoch} = 
	ssl_record:current_connection_state(ConnectionStates, write),
    Epoch.

	    
update_handshake_history(#hello_verify_request{}, _, Hist) ->
    Hist;
update_handshake_history(_, Handshake, Hist) ->
    %% DTLS never needs option "v2_hello_compatible" to be true
    ssl_handshake:update_handshake_history(Hist, iolist_to_binary(Handshake), false).

unprocessed_events(Events) ->
    %% The first handshake event will be processed immediately
    %% as it is entered first in the event queue and
    %% when it is processed there will be length(Events)-1
    %% handshake events left to process before we should
    %% process more TLS-records received on the socket. 
    erlang:length(Events)-1.

