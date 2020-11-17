%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2020. All Rights Reserved.
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

-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/logger.hrl").

-include("tls_connection.hrl").
-include("tls_handshake.hrl").
-include("tls_handshake_1_3.hrl").
-include("ssl_alert.hrl").
-include("tls_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_api.hrl").
-include("ssl_internal.hrl").
-include("ssl_srp.hrl").

%% Internal application API

%% Setup
-export([start_fsm/8,
         init/1,
         pids/1,
         initialize_tls_sender/1]).

%% State transition handling	 
-export([next_event/3,
         next_event/4,
         handle_protocol_record/3]).

%% Handshake handling
-export([renegotiation/2,
         renegotiate/2,
         send_handshake/2,
         send_handshake_flight/1,
	 queue_handshake/2,
         queue_change_cipher/2,
	 reinit/1,
         reinit_handshake_data/1,
         select_sni_extension/1,
         empty_connection_state/2]).

%% Alert and close handling
-export([send_alert/2,
         send_alert_in_connection/2,
         send_sync_alert/2,
         close/5,
         protocol_name/0]).

%% Data handling
-export([socket/4,
         setopts/3,
         getopts/3,
         handle_info/3]).

%% gen_statem state functions
-export([initial_hello/3,
         config_error/3,
         downgrade/3,
	 hello/3,
         user_hello/3,
         wait_ocsp_stapling/3,
         certify/3,
         cipher/3,
         abbreviated/3,
	 connection/3]).

%% gen_statem callbacks
-export([callback_mode/0,
         terminate/3,
         code_change/4,
         format_status/2]).
 
-export([encode_handshake/4]).

-define(DIST_CNTRL_SPAWN_OPTS, [{priority, max}]).

%%====================================================================
%% Internal application API
%%====================================================================	     
%%====================================================================
%% Setup
%%====================================================================
start_fsm(Role, Host, Port, Socket, {#{erl_dist := false},_, Trackers} = Opts,
	  User, {CbModule, _,_, _, _} = CbInfo, 
	  Timeout) -> 
    try 
        {ok, Sender} = tls_sender:start(),
	{ok, Pid} = tls_connection_sup:start_child([Role, Sender, Host, Port, Socket, 
						    Opts, User, CbInfo]), 
	{ok, SslSocket} = ssl_gen_statem:socket_control(?MODULE, Socket, [Pid, Sender], CbModule, Trackers),
        ssl_gen_statem:handshake(SslSocket, Timeout)
    catch
	error:{badmatch, {error, _} = Error} ->
	    Error
    end;

start_fsm(Role, Host, Port, Socket, {#{erl_dist := true},_, Trackers} = Opts,
	  User, {CbModule, _,_, _, _} = CbInfo, 
	  Timeout) -> 
    try 
        {ok, Sender} = tls_sender:start([{spawn_opt, ?DIST_CNTRL_SPAWN_OPTS}]),
	{ok, Pid} = tls_connection_sup:start_child_dist([Role, Sender, Host, Port, Socket, 
							 Opts, User, CbInfo]), 
	{ok, SslSocket} = ssl_gen_statem:socket_control(?MODULE, Socket, [Pid, Sender], CbModule, Trackers),
        ssl_gen_statem:handshake(SslSocket, Timeout)
    catch
	error:{badmatch, {error, _} = Error} ->
	    Error
    end.

init([Role, Sender, Host, Port, Socket, Options,  User, CbInfo]) ->
    State0 = #state{protocol_specific = Map} = initial_state(Role, Sender,
                                                             Host, Port, Socket, Options, User, CbInfo),
    try 
	State1 = #state{static_env = #static_env{session_cache = Cache,
                                                 session_cache_cb = CacheCb
                                                },
                        ssl_options = SslOptions,
                        session = Session0} = ssl_gen_statem:ssl_config(State0#state.ssl_options, Role, State0),
        State = case Role of
                    client ->
                        Session = ssl_session:client_select_session({Host, Port, SslOptions}, Cache, CacheCb, Session0),
                        State1#state{session = Session};
                    server ->
                        State1
                end,
        initialize_tls_sender(State),
        gen_statem:enter_loop(?MODULE, [], initial_hello, State)
    catch throw:Error ->
            EState = State0#state{protocol_specific = Map#{error => Error}},
            gen_statem:enter_loop(?MODULE, [], config_error, EState)
    end.

pids(#state{protocol_specific = #{sender := Sender}}) ->
    [self(), Sender].

%%====================================================================
%% State transition handling
%%====================================================================
next_record(_, #state{handshake_env = 
                       #handshake_env{unprocessed_handshake_events = N} = HsEnv} 
            = State) when N > 0 ->
    {no_record, State#state{handshake_env = 
                                HsEnv#handshake_env{unprocessed_handshake_events = N-1}}};
next_record(_, #state{protocol_buffers =
                          #protocol_buffers{tls_cipher_texts = [_|_] = CipherTexts},
                      connection_states = ConnectionStates,
                      ssl_options = #{padding_check := Check}} = State) ->
    next_record(State, CipherTexts, ConnectionStates, Check);
next_record(connection, #state{protocol_buffers = #protocol_buffers{tls_cipher_texts = []},
                               protocol_specific = #{active_n_toggle := true}
                              } = State) ->
    %% If ssl application user is not reading data wait to activate socket
    flow_ctrl(State); 
  
next_record(_, #state{protocol_buffers = #protocol_buffers{tls_cipher_texts = []},
                      protocol_specific = #{active_n_toggle := true}
                     } = State) ->
    activate_socket(State);
next_record(_, State) ->
    {no_record, State}.

%%% bytes_to_read equals the integer Length arg of ssl:recv
%%% the actual value is only relevant for packet = raw | 0
%%% bytes_to_read = undefined means no recv call is ongoing
flow_ctrl(#state{user_data_buffer = {_,Size,_},
                 socket_options = #socket_options{active = false},
                 bytes_to_read = undefined} = State)  when Size =/= 0 ->
    %% Passive mode wait for new recv request or socket activation
    %% that is preserv some tcp back pressure by waiting to activate
    %% socket
    {no_record, State};
%%%%%%%%%% A packet mode is set and socket is passive %%%%%%%%%%
flow_ctrl(#state{socket_options = #socket_options{active = false,
                                                  packet = Packet}} = State) 
  when ((Packet =/= 0) andalso (Packet =/= raw)) ->
    %% We need more data to complete the packet.
    activate_socket(State);
%%%%%%%%% No packet mode set and socket is passive %%%%%%%%%%%%
flow_ctrl(#state{user_data_buffer = {_,Size,_},
                 socket_options = #socket_options{active = false},
                 bytes_to_read = 0} = State)  when Size == 0 ->
    %% Passive mode no available bytes, get some 
    activate_socket(State);
flow_ctrl(#state{user_data_buffer = {_,Size,_},
                 socket_options = #socket_options{active = false},
                 bytes_to_read = 0} = State)  when Size =/= 0 ->           
    %% There is data in the buffer to deliver
    {no_record, State};
flow_ctrl(#state{user_data_buffer = {_,Size,_}, 
                 socket_options = #socket_options{active = false},
                 bytes_to_read = BytesToRead} = State) when (BytesToRead > 0) ->
    case (Size >= BytesToRead) of
        true -> %% There is enough data bufferd
            {no_record, State};
        false -> %% We need more data to complete the delivery of <BytesToRead> size
            activate_socket(State)
    end;
%%%%%%%%%%% Active mode or more data needed %%%%%%%%%%
flow_ctrl(State) ->
    activate_socket(State).


activate_socket(#state{protocol_specific = #{active_n_toggle := true, active_n := N} = ProtocolSpec,
                       static_env = #static_env{socket = Socket,
                                                close_tag = CloseTag,
                                                transport_cb = Transport}  
                      } = State) ->                                                                                                            
    case tls_socket:setopts(Transport, Socket, [{active, N}]) of
        ok ->
            {no_record, State#state{protocol_specific = ProtocolSpec#{active_n_toggle => false}}}; 
        _ ->
            self() ! {CloseTag, Socket},
            {no_record, State}
    end.

%% Decipher next record and concatenate consecutive ?APPLICATION_DATA records into one
%%
next_record(State, CipherTexts, ConnectionStates, Check) ->
    next_record(State, CipherTexts, ConnectionStates, Check, []).
%%
next_record(#state{connection_env = #connection_env{negotiated_version = {3,4} = Version}} = State,
            [CT|CipherTexts], ConnectionStates0, Check, Acc) ->
    case tls_record:decode_cipher_text(Version, CT, ConnectionStates0, Check) of
        {#ssl_tls{type = ?APPLICATION_DATA, fragment = Fragment}, ConnectionStates} ->
            case CipherTexts of
                [] ->
                    %% End of cipher texts - build and deliver an ?APPLICATION_DATA record
                    %% from the accumulated fragments
                    next_record_done(State, [], ConnectionStates,
                                     #ssl_tls{type = ?APPLICATION_DATA,
                                              fragment = iolist_to_binary(lists:reverse(Acc, [Fragment]))});
                [_|_] ->
                    next_record(State, CipherTexts, ConnectionStates, Check, [Fragment|Acc])
            end;
        {Record, ConnectionStates} when Acc =:= [] ->
            %% Singelton non-?APPLICATION_DATA record - deliver
            next_record_done(State, CipherTexts, ConnectionStates, Record);
        {_Record, _ConnectionStates_to_forget} ->
            %% Not ?APPLICATION_DATA but we have accumulated fragments
            %% -> build an ?APPLICATION_DATA record with concatenated fragments
            %%    and forget about decrypting this record - we'll decrypt it again next time
            %% Will not work for stream ciphers
            next_record_done(State, [CT|CipherTexts], ConnectionStates0,
                             #ssl_tls{type = ?APPLICATION_DATA, fragment = iolist_to_binary(lists:reverse(Acc))});
        #alert{} = Alert ->
            Alert
    end;
next_record(#state{connection_env = #connection_env{negotiated_version = Version}} = State,
            [#ssl_tls{type = ?APPLICATION_DATA} = CT |CipherTexts], ConnectionStates0, Check, Acc) ->
    case tls_record:decode_cipher_text(Version, CT, ConnectionStates0, Check) of
        {#ssl_tls{type = ?APPLICATION_DATA, fragment = Fragment}, ConnectionStates} ->
            case CipherTexts of
                [] ->
                    %% End of cipher texts - build and deliver an ?APPLICATION_DATA record
                    %% from the accumulated fragments
                    next_record_done(State, [], ConnectionStates,
                                     #ssl_tls{type = ?APPLICATION_DATA,
                                              fragment = iolist_to_binary(lists:reverse(Acc, [Fragment]))});
                [_|_] ->
                    next_record(State, CipherTexts, ConnectionStates, Check, [Fragment|Acc])
            end;
        #alert{} = Alert ->
            Alert
    end;
next_record(State, CipherTexts, ConnectionStates, _, [_|_] = Acc) ->
    next_record_done(State, CipherTexts, ConnectionStates,
                     #ssl_tls{type = ?APPLICATION_DATA,
                              fragment = iolist_to_binary(lists:reverse(Acc))});
next_record(#state{connection_env = #connection_env{negotiated_version = Version}} = State,
            [CT|CipherTexts], ConnectionStates0, Check, []) ->
    case tls_record:decode_cipher_text(Version, CT, ConnectionStates0, Check) of      
        {Record, ConnectionStates} ->
            %% Singelton non-?APPLICATION_DATA record - deliver
            next_record_done(State, CipherTexts, ConnectionStates, Record);
        #alert{} = Alert ->
            Alert
    end.

next_record_done(#state{protocol_buffers = Buffers} = State, CipherTexts, ConnectionStates, Record) ->
    {Record,
     State#state{protocol_buffers = Buffers#protocol_buffers{tls_cipher_texts = CipherTexts},
                 connection_states = ConnectionStates}}.

next_event(StateName, Record, State) ->
    next_event(StateName, Record, State, []).
%%
next_event(StateName, no_record, #state{static_env = #static_env{role = Role}} = State0, Actions) ->
    case next_record(StateName, State0) of
 	{no_record, State} ->
            ssl_gen_statem:hibernate_after(StateName, State, Actions);
        {Record, State} ->
            next_event(StateName, Record, State, Actions);
        #alert{} = Alert ->
            ssl_gen_statem:handle_normal_shutdown(Alert#alert{role = Role}, StateName, State0),
	    {stop, {shutdown, own_alert}, State0}
    end;
next_event(StateName,  #ssl_tls{} = Record, State, Actions) ->
    {next_state, StateName, State, [{next_event, internal, {protocol_record, Record}} | Actions]};
next_event(StateName,  #alert{} = Alert, State, Actions) ->
    {next_state, StateName, State, [{next_event, internal, Alert} | Actions]}.

%%% TLS record protocol level application data messages 
handle_protocol_record(#ssl_tls{type = ?APPLICATION_DATA, fragment = Data}, StateName, 
                       #state{start_or_recv_from = From,
                              socket_options = #socket_options{active = false}} = State0) when From =/= undefined ->
    case ssl_gen_statem:read_application_data(Data, State0) of
       {stop, _, _} = Stop->
            Stop;
        {Record, #state{start_or_recv_from = Caller} = State} ->
            TimerAction = case Caller of
                              undefined -> %% Passive recv complete cancel timer
                                  [{{timeout, recv}, infinity, timeout}];
                              _ ->
                                  []
                          end,
            next_event(StateName, Record, State, TimerAction)
    end;
handle_protocol_record(#ssl_tls{type = ?APPLICATION_DATA, fragment = Data}, StateName, State0) ->
    case ssl_gen_statem:read_application_data(Data, State0) of
	{stop, _, _} = Stop->
            Stop;
	{Record, State} ->
            next_event(StateName, Record, State)
    end;
%%% TLS record protocol level handshake messages 
handle_protocol_record(#ssl_tls{type = ?HANDSHAKE, fragment = Data}, 
		    StateName, #state{protocol_buffers =
					  #protocol_buffers{tls_handshake_buffer = Buf0} = Buffers,
                                      connection_env = #connection_env{negotiated_version = Version},
                                      static_env = #static_env{role = Role},
				      ssl_options = Options} = State0) ->
    try
	%% Calculate the effective version that should be used when decoding an incoming handshake
	%% message.
	EffectiveVersion = effective_version(Version, Options, Role),
	{Packets, Buf} = tls_handshake:get_tls_handshake(EffectiveVersion,Data,Buf0, Options),
	State =
	    State0#state{protocol_buffers =
			     Buffers#protocol_buffers{tls_handshake_buffer = Buf}},
	case Packets of
            [] -> 
                assert_buffer_sanity(Buf, Options),
                next_event(StateName, no_record, State);
            _ ->                
                Events = tls_handshake_events(Packets),
                case StateName of
                    connection ->
                        ssl_gen_statem:hibernate_after(StateName, State, Events);
                    _ ->
                        HsEnv = State#state.handshake_env,
                        {next_state, StateName, 
                         State#state{handshake_env = 
                                         HsEnv#handshake_env{unprocessed_handshake_events 
                                                             = unprocessed_events(Events)}}, Events}
                end
        end
    catch throw:#alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, Version, StateName, State0)
    end;
%%% TLS record protocol level change cipher messages
handle_protocol_record(#ssl_tls{type = ?CHANGE_CIPHER_SPEC, fragment = Data}, StateName, State) ->
    {next_state, StateName, State, [{next_event, internal, #change_cipher_spec{type = Data}}]};
%%% TLS record protocol level Alert messages
handle_protocol_record(#ssl_tls{type = ?ALERT, fragment = EncAlerts}, StateName,
                       #state{connection_env = #connection_env{negotiated_version = Version}} = State) ->
    try decode_alerts(EncAlerts) of	
	Alerts = [_|_] ->
	    handle_alerts(Alerts,  {next_state, StateName, State});
	[] ->
	    ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, empty_alert),
					    Version, StateName, State);
        #alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, Version, StateName, State)
    catch
	_:_ ->
	    ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, alert_decode_error),
					    Version, StateName, State)  

    end;
%% Ignore unknown TLS record level protocol messages
handle_protocol_record(#ssl_tls{type = _Unknown}, StateName, State) ->
    {next_state, StateName, State, []}.
%%====================================================================
%% Handshake handling
%%====================================================================
renegotiation(Pid, WriteState) ->
    gen_statem:call(Pid, {user_renegotiate, WriteState}).

renegotiate(#state{static_env = #static_env{role = client},
                   handshake_env = HsEnv} = State, Actions) ->
    %% Handle same way as if server requested
    %% the renegotiation
    Hs0 = ssl_handshake:init_handshake_history(),
    {next_state, connection, State#state{handshake_env = HsEnv#handshake_env{tls_handshake_history = Hs0}}, 
     [{next_event, internal, #hello_request{}} | Actions]};
renegotiate(#state{static_env = #static_env{role = server,
                                            socket = Socket,
                                            transport_cb = Transport},
                   handshake_env = HsEnv,
                   connection_env = #connection_env{negotiated_version = Version},
		   connection_states = ConnectionStates0} = State0, Actions) ->
    HelloRequest = ssl_handshake:hello_request(),
    Frag = tls_handshake:encode_handshake(HelloRequest, Version),
    Hs0 = ssl_handshake:init_handshake_history(),
    {BinMsg, ConnectionStates} = 
	tls_record:encode_handshake(Frag, Version, ConnectionStates0),
    tls_socket:send(Transport, Socket, BinMsg),
    State = State0#state{connection_states = 
			     ConnectionStates,
			 handshake_env = HsEnv#handshake_env{tls_handshake_history = Hs0}},
    next_event(hello, no_record, State, Actions).
	     
send_handshake(Handshake, State) ->
    send_handshake_flight(queue_handshake(Handshake, State)).

queue_handshake(Handshake, #state{handshake_env = #handshake_env{tls_handshake_history = Hist0} = HsEnv,
				  connection_env = #connection_env{negotiated_version = Version},
                                  flight_buffer = Flight0,
                                  ssl_options = #{log_level := LogLevel},
				  connection_states = ConnectionStates0} = State0) ->
    {BinHandshake, ConnectionStates, Hist} =
	encode_handshake(Handshake, Version, ConnectionStates0, Hist0),
    ssl_logger:debug(LogLevel, outbound, 'handshake', Handshake),
    ssl_logger:debug(LogLevel, outbound, 'record', BinHandshake),

    State0#state{connection_states = ConnectionStates,
                 handshake_env = HsEnv#handshake_env{tls_handshake_history = Hist},
		 flight_buffer = Flight0 ++ [BinHandshake]}.

-spec send_handshake_flight(StateIn) -> {StateOut, FlightBuffer} when
      StateIn :: #state{},
      StateOut :: #state{},
      FlightBuffer :: list().
send_handshake_flight(#state{static_env = #static_env{socket = Socket,
                                                      transport_cb = Transport},
			     flight_buffer = Flight} = State0) ->
    tls_socket:send(Transport, Socket, Flight),
    {State0#state{flight_buffer = []}, []}.


queue_change_cipher(Msg, #state{connection_env = #connection_env{negotiated_version = Version},
                                flight_buffer = Flight0,
                                ssl_options = #{log_level := LogLevel},
                                connection_states = ConnectionStates0} = State0) ->
    {BinChangeCipher, ConnectionStates} =
	encode_change_cipher(Msg, Version, ConnectionStates0),
    ssl_logger:debug(LogLevel, outbound, 'record', BinChangeCipher),
    State0#state{connection_states = ConnectionStates,
		 flight_buffer = Flight0 ++ [BinChangeCipher]}.

reinit(#state{protocol_specific = #{sender := Sender},
              connection_env = #connection_env{negotiated_version = Version},
              connection_states = #{current_write := Write}} = State) -> 
    tls_sender:update_connection_state(Sender, Write, Version),
    reinit_handshake_data(State).

reinit_handshake_data(#state{handshake_env = HsEnv} =State) ->
    %% premaster_secret, public_key_info and tls_handshake_info 
    %% are only needed during the handshake phase. 
    %% To reduce memory foot print of a connection reinitialize them.
     State#state{
       handshake_env = HsEnv#handshake_env{tls_handshake_history = ssl_handshake:init_handshake_history(),
                                           public_key_info = undefined,
                                           premaster_secret = undefined}
     }.

select_sni_extension(#client_hello{extensions = #{sni := SNI}}) ->
    SNI;
select_sni_extension(_) ->
    undefined.

empty_connection_state(ConnectionEnd, BeastMitigation) ->
    ssl_record:empty_connection_state(ConnectionEnd, BeastMitigation).

%%====================================================================
%% Alert and close handling
%%====================================================================	     

%%--------------------------------------------------------------------
-spec encode_alert(#alert{}, ssl_record:ssl_version(), ssl_record:connection_states()) -> 
		    {iolist(), ssl_record:connection_states()}.
%%
%% Description: Encodes an alert
%%--------------------------------------------------------------------
encode_alert(#alert{} = Alert, Version, ConnectionStates) ->
    tls_record:encode_alert_record(Alert, Version, ConnectionStates).

send_alert(Alert, #state{static_env = #static_env{socket = Socket,
                                                  transport_cb = Transport},
                         connection_env = #connection_env{negotiated_version = Version},
                         ssl_options = #{log_level := LogLevel},
                         connection_states = ConnectionStates0} = StateData0) ->
    {BinMsg, ConnectionStates} =
        encode_alert(Alert, Version, ConnectionStates0),
    tls_socket:send(Transport, Socket, BinMsg),
    ssl_logger:debug(LogLevel, outbound, 'record', BinMsg),
    StateData0#state{connection_states = ConnectionStates}.

%% If an ALERT sent in the connection state, should cause the TLS
%% connection to end, we need to synchronize with the tls_sender
%% process so that the ALERT if possible (that is the tls_sender process is
%% not blocked) is sent before the connection process terminates and
%% thereby closes the transport socket.
send_alert_in_connection(#alert{level = ?FATAL} = Alert, State) ->
    send_sync_alert(Alert, State);
send_alert_in_connection(#alert{description = ?CLOSE_NOTIFY} = Alert, State) ->
    send_sync_alert(Alert, State);
send_alert_in_connection(Alert,
                         #state{protocol_specific = #{sender := Sender}}) ->
    tls_sender:send_alert(Sender, Alert).
send_sync_alert(
  Alert, #state{protocol_specific = #{sender := Sender}} = State) ->
    try tls_sender:send_and_ack_alert(Sender, Alert)
    catch
        _:_ ->
            throw({stop, {shutdown, own_alert}, State})
    end.

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
    tls_socket:close(Transport, Socket).
protocol_name() ->
    "TLS".

%%====================================================================
%% Data handling
%%====================================================================	     

socket(Pids,  Transport, Socket, Trackers) ->
    tls_socket:socket(Pids, Transport, Socket, ?MODULE, Trackers).

setopts(Transport, Socket, Other) ->
    tls_socket:setopts(Transport, Socket, Other).

getopts(Transport, Socket, Tag) ->
    tls_socket:getopts(Transport, Socket, Tag).

%%--------------------------------------------------------------------
%% State functions
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
-spec initial_hello(gen_statem:event_type(),
                    {start, timeout()} | term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
initial_hello(Type, Event, State) ->
    ssl_gen_statem:?FUNCTION_NAME(Type, Event, State).
 
%%--------------------------------------------------------------------
-spec config_error(gen_statem:event_type(),
	   {start, timeout()} | term(), #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
config_error(Type, Event, State) ->
    ssl_gen_statem:?FUNCTION_NAME(Type, Event, State).
 
%%--------------------------------------------------------------------
-spec hello(gen_statem:event_type(),
	    #hello_request{} | #client_hello{} | #server_hello{} | term(),
	    #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
hello(internal, #client_hello{extensions = Extensions} = Hello, 
      #state{ssl_options = #{handshake := hello},
             handshake_env = HsEnv,
             start_or_recv_from = From} = State) ->
    {next_state, user_hello, State#state{start_or_recv_from = undefined,
                                         handshake_env = HsEnv#handshake_env{hello = Hello}},
     [{reply, From, {ok, Extensions}}]};
hello(internal, #server_hello{extensions = Extensions} = Hello,
      #state{ssl_options = #{
                 handshake := hello},
             handshake_env = HsEnv,
             start_or_recv_from = From} = State) ->   
    {next_state, user_hello,
     State#state{start_or_recv_from = undefined,
                 handshake_env = HsEnv#handshake_env{
                                   hello = Hello}}, [{reply, From, {ok, Extensions}}]};
hello(internal, #client_hello{client_version = ClientVersion} = Hello, #state{ssl_options = SslOpts0,
                                                                              connection_env = CEnv} = State0) ->
    case choose_tls_fsm(SslOpts0, Hello) of
        tls_1_3_fsm ->
            %% Continue in TLS 1.3 'start' state
            {next_state, start, State0, [{change_callback_module, tls_connection_1_3}, {next_event, internal, Hello}]};
        tls_1_0_to_1_2_fsm ->
            case handle_client_hello(Hello, State0) of
                {ServerHelloExt, Type, State} ->
                    {next_state, hello, State, [{next_event, internal, {common_client_hello, Type, ServerHelloExt}}]};
                Alert ->
                    ssl_gen_statem:handle_own_alert(Alert, ClientVersion, hello,
                                                        State0#state{connection_env = CEnv#connection_env{negotiated_version
                                                                                                          = ClientVersion}})
            end
    end;
hello(internal, #server_hello{} = Hello,
      #state{connection_states = ConnectionStates0,
             connection_env = #connection_env{negotiated_version = ReqVersion} = CEnv,
	     static_env = #static_env{role = client},
             handshake_env = #handshake_env{
                                ocsp_stapling_state = OcspState0,
                                renegotiation = {Renegotiation, _}} = HsEnv,
             session = #session{session_id = OldId},
	     ssl_options = SslOptions} = State) ->   
    case tls_handshake:hello(Hello, SslOptions, ConnectionStates0, Renegotiation, OldId) of
        #alert{} = Alert -> 
            ssl_gen_statem:handle_own_alert(Alert, ReqVersion, hello,
                                            State#state{connection_env =
                                                            CEnv#connection_env{negotiated_version = ReqVersion}
                                                       });
        %% Legacy TLS 1.2 and older
        {Version, NewId, ConnectionStates, ProtoExt, Protocol, OcspState} ->
            ssl_connection:handle_session(Hello, 
                                          Version, NewId, ConnectionStates, ProtoExt, Protocol,
                                          State#state{
                                            handshake_env = HsEnv#handshake_env{
                                                              ocsp_stapling_state = maps:merge(OcspState0,OcspState)}});
        %% TLS 1.3
        {next_state, wait_sh, SelectedVersion, OcspState} ->
            %% Continue in TLS 1.3 'wait_sh' state
            {next_state, wait_sh,
             State#state{handshake_env = HsEnv#handshake_env{ocsp_stapling_state =  maps:merge(OcspState0,OcspState)}, 
                         connection_env = CEnv#connection_env{negotiated_version = SelectedVersion}},
             [{change_callback_module, tls_connection_1_3}, {next_event, internal, Hello}]}
    end;
hello(info, Event, State) ->
    handle_info(Event, ?FUNCTION_NAME, State);
hello(Type, Event, State) ->
    ssl_connection:gen_handshake(?FUNCTION_NAME, Type, Event, State).

user_hello(Type, Event, State) ->
    ssl_connection:gen_handshake(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec abbreviated(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
abbreviated(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
abbreviated(Type, Event, State) ->
    ssl_connection:gen_handshake(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec wait_ocsp_stapling(gen_statem:event_type(), term(), #state{}) ->
		     gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_ocsp_stapling(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
wait_ocsp_stapling(Type, Event, State) ->
    ssl_connection:gen_handshake(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec certify(gen_statem:event_type(), term(), #state{}) ->
		     gen_statem:state_function_result().
%%--------------------------------------------------------------------
certify(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
certify(Type, Event, State) ->
    ssl_connection:gen_handshake(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec cipher(gen_statem:event_type(), term(), #state{}) ->
		    gen_statem:state_function_result().
%%--------------------------------------------------------------------
cipher(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
cipher(Type, Event, State) ->
     ssl_connection:gen_handshake(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec connection(gen_statem:event_type(),  
		 #hello_request{} | #client_hello{}| term(), #state{}) ->
			gen_statem:state_function_result().
%%--------------------------------------------------------------------
connection(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
connection({call, From}, {user_renegotiate, WriteState}, 
           #state{connection_states = ConnectionStates} = State) ->
    {next_state,  ?FUNCTION_NAME, State#state{connection_states = ConnectionStates#{current_write => WriteState}}, 
     [{next_event,{call, From}, renegotiate}]};
connection(internal, #hello_request{},
	   #state{static_env = #static_env{role = client,
                                           host = Host,
                                           port = Port,
                                           session_cache = Cache,
                                           session_cache_cb = CacheCb},
                  handshake_env = #handshake_env{
                      renegotiation = {Renegotiation, peer},
                      ocsp_stapling_state = OcspState},
		  session = #session{own_certificates = OwnCerts} = Session0,
		  ssl_options = SslOpts, 
                  protocol_specific = #{sender := Pid},
		  connection_states = ConnectionStates} = State0) ->
    try tls_sender:peer_renegotiate(Pid) of
        {ok, Write} ->
            Session = ssl_session:client_select_session({Host, Port, SslOpts}, Cache, CacheCb, Session0),
            Hello = tls_handshake:client_hello(Host, Port, ConnectionStates, SslOpts,
                                               Session#session.session_id,
                                               Renegotiation, OwnCerts, undefined,
                                               undefined, maps:get(ocsp_nonce, OcspState, undefined)),
            {State, Actions} = send_handshake(Hello, State0#state{connection_states = ConnectionStates#{current_write => Write},
                                                                  session = Session}),
            next_event(hello, no_record, State, Actions)
        catch 
            _:_ ->
                {stop, {shutdown, sender_blocked}, State0}
        end;
connection(internal, #hello_request{},
	   #state{static_env = #static_env{role = client,
                                           host = Host,
                                           port = Port},
                  handshake_env = #handshake_env{
                      renegotiation = {Renegotiation, _},
                      ocsp_stapling_state = OcspState},
		  session = #session{own_certificates = OwnCerts},
		  ssl_options = SslOpts, 
		  connection_states = ConnectionStates} = State0) ->
    Hello = tls_handshake:client_hello(Host, Port, ConnectionStates, SslOpts,
                                       <<>>, Renegotiation, OwnCerts, undefined,
                                       undefined, maps:get(ocsp_nonce, OcspState, undefined)),

    {State, Actions} = send_handshake(Hello, State0),
    next_event(hello, no_record, State, Actions);
connection(internal, #client_hello{} = Hello, 
	   #state{static_env = #static_env{role = server},
                  handshake_env = #handshake_env{allow_renegotiate = true}= HsEnv,
                  connection_states = CS,
                  protocol_specific = #{sender := Sender}
                 } = State) ->
    %% Mitigate Computational DoS attack
    %% http://www.educatedguesswork.org/2011/10/ssltls_and_computational_dos.html
    %% http://www.thc.org/thc-ssl-dos/ Rather than disabling client
    %% initiated renegotiation we will disallow many client initiated
    %% renegotiations immediately after each other.
    erlang:send_after(?WAIT_TO_ALLOW_RENEGOTIATION, self(), allow_renegotiate),
    {ok, Write} = tls_sender:renegotiate(Sender),
    next_event(hello, no_record, State#state{connection_states = CS#{current_write => Write},
                                             handshake_env = HsEnv#handshake_env{renegotiation = {true, peer},
                                                                                 allow_renegotiate = false}
                                            }, 
               [{next_event, internal, Hello}]);
connection(internal, #client_hello{}, 
	   #state{static_env = #static_env{role = server},
                  handshake_env = #handshake_env{allow_renegotiate = false}} = State0) ->
    Alert = ?ALERT_REC(?WARNING, ?NO_RENEGOTIATION),
    send_alert_in_connection(Alert, State0),
    State = reinit_handshake_data(State0),
    next_event(?FUNCTION_NAME, no_record, State);
connection(Type, Event, State) ->
    ssl_connection:?FUNCTION_NAME(Type, Event, State).

%%--------------------------------------------------------------------
-spec downgrade(gen_statem:event_type(), term(), #state{}) ->
		       gen_statem:state_function_result().
%%--------------------------------------------------------------------
downgrade(internal, #alert{description = ?CLOSE_NOTIFY},
	  #state{static_env = #static_env{transport_cb = Transport,
                                          socket = Socket},
		 connection_env = #connection_env{downgrade = {Pid, From}}} = State) ->
    tls_socket:setopts(Transport, Socket, [{active, false}, {packet, 0}, {mode, binary}]),
    Transport:controlling_process(Socket, Pid),
    {stop_and_reply, {shutdown, downgrade},[{reply, From, {ok, Socket}}], State};
downgrade(timeout, downgrade, #state{ connection_env = #connection_env{downgrade = {_, From}}} = State) ->
    {stop_and_reply, {shutdown, normal},[{reply, From, {error, timeout}}], State};
downgrade(info, {CloseTag, Socket},
          #state{static_env = #static_env{socket = Socket, 
                                          close_tag = CloseTag},
                 connection_env = #connection_env{downgrade = {_, From}}} =
              State) ->
    {stop_and_reply, {shutdown, normal},[{reply, From, {error, CloseTag}}], State};
downgrade(info, Info, State) ->
    handle_info(Info, ?FUNCTION_NAME, State);
downgrade(Type, Event, State) ->
     ssl_connection:?FUNCTION_NAME(Type, Event, State).

%--------------------------------------------------------------------
%% gen_statem callbacks
%%--------------------------------------------------------------------
callback_mode() ->
    state_functions.

terminate({shutdown, {sender_died, Reason}}, _StateName,
          #state{static_env = #static_env{socket = Socket, 
                                          transport_cb = Transport}} 
          = State) ->
    ssl_gen_statem:handle_trusted_certs_db(State),
    close(Reason, Socket, Transport, undefined, undefined);
terminate(Reason, StateName, State) ->
    catch ssl_gen_statem:terminate(Reason, StateName, State),
    ensure_sender_terminate(Reason, State).

format_status(Type, Data) ->
    ssl_gen_statem:format_status(Type, Data).

code_change(_OldVsn, StateName, State, _) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
initial_state(Role, Sender, Host, Port, Socket, {SSLOptions, SocketOptions, Trackers}, User,
	      {CbModule, DataTag, CloseTag, ErrorTag, PassiveTag}) ->
    #{beast_mitigation := BeastMitigation,
      erl_dist := IsErlDist,
      client_renegotiation := ClientRenegotiation} = SSLOptions,
    ConnectionStates = tls_record:init_connection_states(Role, BeastMitigation),
    SessionCacheCb = case application:get_env(ssl, session_cb) of
			 {ok, Cb} when is_atom(Cb) ->
			    Cb;
			 _  ->
			     ssl_session_cache
		     end,
    InternalActiveN =  case application:get_env(ssl, internal_active_n) of
                           {ok, N} when is_integer(N) andalso (not IsErlDist) ->
                               N;
                           _  ->
                               ?INTERNAL_ACTIVE_N
                       end,
    UserMonitor = erlang:monitor(process, User),
    InitStatEnv = #static_env{
                     role = Role,
                     transport_cb = CbModule,
                     protocol_cb = ?MODULE,
                     data_tag = DataTag,
                     close_tag = CloseTag,
                     error_tag = ErrorTag,
                     passive_tag = PassiveTag,
                     host = Host,
                     port = Port,
                     socket = Socket,
                     session_cache_cb = SessionCacheCb,
                     trackers = Trackers
                    },  
    #state{
       static_env = InitStatEnv,
       handshake_env = #handshake_env{
                          tls_handshake_history = ssl_handshake:init_handshake_history(),
                          renegotiation = {false, first},
                          allow_renegotiate = ClientRenegotiation
                         },
       connection_env = #connection_env{user_application = {UserMonitor, User}},
       socket_options = SocketOptions,
       ssl_options = SSLOptions,
       session = #session{is_resumable = false},
       connection_states = ConnectionStates,
       protocol_buffers = #protocol_buffers{},
       user_data_buffer = {[],0,[]},
       start_or_recv_from = undefined,
       flight_buffer = [],
       protocol_specific = #{sender => Sender,
                             active_n => InternalActiveN,
                             active_n_toggle => true
                            }
      }.

initialize_tls_sender(#state{static_env = #static_env{
                                             role = Role,
                                             transport_cb = Transport,
                                             socket = Socket,
                                             trackers = Trackers
                                            },
                             connection_env = #connection_env{negotiated_version = Version},
                             socket_options = SockOpts, 
                             ssl_options = #{renegotiate_at := RenegotiateAt,
                                             key_update_at := KeyUpdateAt,
                                             log_level := LogLevel},
                             connection_states = #{current_write := ConnectionWriteState},
                             protocol_specific = #{sender := Sender}}) ->
    Init = #{current_write => ConnectionWriteState,
             role => Role,
             socket => Socket,
             socket_options => SockOpts,
             trackers => Trackers,
             transport_cb => Transport,
             negotiated_version => Version,
             renegotiate_at => RenegotiateAt,
             key_update_at => KeyUpdateAt,
             log_level => LogLevel},
    tls_sender:initialize(Sender, Init).

next_tls_record(Data, StateName,
                         #state{protocol_buffers =
                                    #protocol_buffers{tls_record_buffer = Buf0,
                                                      tls_cipher_texts = CT0} = Buffers,
                                ssl_options = SslOpts} = State0) ->
    Versions =
        %% TLSPlaintext.legacy_record_version is ignored in TLS 1.3 and thus all
        %% record version are accepted when receiving initial ClientHello and
        %% ServerHello. This can happen in state 'hello' in case of all TLS
        %% versions and also in state 'start' when TLS 1.3 is negotiated.
        %% After the version is negotiated all subsequent TLS records shall have
        %% the proper legacy_record_version (= negotiated_version).
        %% Note: TLS record version {3,4} is used internally in TLS 1.3 and at this
        %% point it is the same as the negotiated protocol version.
        %% TODO: Refactor state machine and introduce a record_protocol_version beside
        %% the negotiated_version.
        case StateName of
            State when State =:= hello orelse
                       State =:= start ->
                [tls_record:protocol_version(Vsn) || Vsn <- ?ALL_AVAILABLE_VERSIONS];
            _ ->
                State0#state.connection_env#connection_env.negotiated_version
        end,
    #{current_write := #{max_fragment_length := MaxFragLen}} = State0#state.connection_states,
    case tls_record:get_tls_records(Data, Versions, Buf0, MaxFragLen, SslOpts) of
	{Records, Buf1} ->
	    CT1 = CT0 ++ Records,
	    next_record(StateName, State0#state{protocol_buffers =
					 Buffers#protocol_buffers{tls_record_buffer = Buf1,
								  tls_cipher_texts = CT1}});
	#alert{} = Alert ->
	    handle_record_alert(Alert, State0)
    end.


handle_record_alert(Alert, _) ->
    Alert.

tls_handshake_events(Packets) ->
    lists:map(fun(Packet) ->
		      {next_event, internal, {handshake, Packet}}
	      end, Packets).

%% raw data from socket, upack records
handle_info({Protocol, _, Data}, StateName,
            #state{static_env = #static_env{data_tag = Protocol},
                   connection_env = #connection_env{negotiated_version = Version}} = State0) ->
    case next_tls_record(Data, StateName, State0) of
	{Record, State} ->
	    next_event(StateName, Record, State);
	#alert{} = Alert ->
	    ssl_gen_statem:handle_own_alert(Alert, Version, StateName, State0)
    end;
handle_info({PassiveTag, Socket},  StateName, 
            #state{static_env = #static_env{socket = Socket,
                                            passive_tag = PassiveTag},
                   start_or_recv_from = From,
                   protocol_buffers = #protocol_buffers{tls_cipher_texts = CTs},
                   protocol_specific = PS
                  } = State0) ->
    case (From =/= undefined) andalso (CTs == []) of
        true ->
            {Record, State} = activate_socket(State0#state{protocol_specific = PS#{active_n_toggle => true}}),
            next_event(StateName, Record, State);
        false ->
            next_event(StateName, no_record, 
                       State0#state{protocol_specific = PS#{active_n_toggle => true}})
    end;
handle_info({CloseTag, Socket}, StateName,
            #state{static_env = #static_env{
                                   role = Role,
                                   host = Host,
                                   port = Port,
                                   socket = Socket, 
                                   close_tag = CloseTag},
                   handshake_env = #handshake_env{renegotiation = Type},
                   session = Session} = State) when  StateName =/= connection ->
    ssl_gen_statem:maybe_invalidate_session(Type, Role, Host, Port, Session),
    Alert = ?ALERT_REC(?FATAL, ?CLOSE_NOTIFY, transport_closed),
    ssl_gen_statem:handle_normal_shutdown(Alert#alert{role = Role}, StateName, State),
    {stop, {shutdown, transport_closed}, State};
handle_info({CloseTag, Socket}, StateName,
            #state{static_env = #static_env{
                                   role = Role,
                                   socket = Socket,
                                   close_tag = CloseTag},
                   start_or_recv_from = From,
                   socket_options = #socket_options{active = Active},
                   protocol_specific = PS} = State) ->

    %% Note that as of TLS 1.1,
    %% failure to properly close a connection no longer requires that a
    %% session not be resumed.  This is a change from TLS 1.0 to conform
    %% with widespread implementation practice.

    case (Active == false) andalso (From == undefined) of
        false ->
            %% As invalidate_sessions here causes performance issues,
            %% we will conform to the widespread implementation
            %% practice and go aginst the spec
            %% case Version of
            %%     {3, N} when N >= 1 ->
            %%         ok;
            %%     _ ->
            %%         invalidate_session(Role, Host, Port, Session)
            %%         ok
            %% end,
            Alert = ?ALERT_REC(?FATAL, ?CLOSE_NOTIFY, transport_closed),
            ssl_gen_statem:handle_normal_shutdown(Alert#alert{role = Role}, StateName, State),
            {stop, {shutdown, transport_closed}, State};
        true ->
            %% Wait for next socket operation (most probably
            %% ssl:setopts(S, [{active, true | once | N}]) or
            %% ssl:recv(S, N, Timeout) before closing.  Possible
            %% buffered data will be deliverd by the code handling
            %% these options before closing. In the case of the
            %% peer resetting the connection hard, that is
            %% we do not receive any close ALERT, and an active once (or possible N)
            %% strategy is used by the client we want to later trigger a new
            %% "transport closed" message. This is achieved by setting the internal
            %% active_n_toggle here which will cause
            %% this to happen when tls_connection:activate_socket/1
            %% is called after all data has been deliver.
            {next_state, StateName, State#state{protocol_specific = PS#{active_n_toggle => true}}, []}
    end;
handle_info({'EXIT', Sender, Reason}, _,
            #state{protocol_specific = #{sender := Sender}} = State) ->
    {stop, {shutdown, {sender_died, Reason}}, State};
handle_info(Msg, StateName, State) ->
    ssl_gen_statem:handle_info(Msg, StateName, State).

handle_client_hello(#client_hello{client_version = ClientVersion} = Hello, State0) ->
    case ssl_connection:handle_sni_extension(State0, Hello) of
        #state{connection_states = ConnectionStates0,
                    static_env = #static_env{trackers = Trackers},
                    handshake_env = #handshake_env{
                                       kex_algorithm = KeyExAlg,
                                       renegotiation = {Renegotiation, _},
                                       negotiated_protocol = CurrentProtocol,
                                       sni_guided_cert_selection = SNICertSelection} = HsEnv,
                    connection_env = CEnv,
                    session = #session{own_certificates = OwnCerts} = Session0,
               ssl_options = SslOpts} = State ->
            SessionTracker = proplists:get_value(session_id_tracker, Trackers),
            case tls_handshake:hello(Hello,
                                     SslOpts,
                                     {SessionTracker, Session0,
                                      ConnectionStates0, OwnCerts, KeyExAlg},
                                     Renegotiation) of
                #alert{} = Alert ->
                    Alert;
                {Version, {Type, Session},
                 ConnectionStates, Protocol0, ServerHelloExt0, HashSign} ->
                    Protocol = case Protocol0 of
                                   undefined -> CurrentProtocol;
                                   _ -> Protocol0
                               end,
                    ServerHelloExt =
                        case SNICertSelection of
                            true ->
                                ServerHelloExt0#{sni => #sni{hostname = ""}};
                            false ->
                                ServerHelloExt0
                        end,
                    {ServerHelloExt, Type, State#state{connection_states  = ConnectionStates,
                                                       connection_env = CEnv#connection_env{negotiated_version = Version},
                                                       handshake_env = HsEnv#handshake_env{
                                                                         hashsign_algorithm = HashSign,
                                                                         client_hello_version = ClientVersion,
                                                                         negotiated_protocol = Protocol},
                                                       session = Session
                                                      }}
            end;
        #alert{} = Alert ->
            Alert
    end.

handle_alerts([], Result) ->
    Result;
handle_alerts(_, {stop, _, _} = Stop) ->
    Stop;
handle_alerts([#alert{level = ?WARNING, description = ?CLOSE_NOTIFY} | _Alerts], 
              {next_state, connection = StateName, #state{connection_env = CEnv, 
                                                          socket_options = #socket_options{active = false},
                                                          start_or_recv_from = From} = State}) when From == undefined ->
    {next_state, StateName, State#state{connection_env = CEnv#connection_env{terminated = true}}};
handle_alerts([Alert | Alerts], {next_state, StateName, State}) ->
     handle_alerts(Alerts, ssl_gen_statem:handle_alert(Alert, StateName, State));
handle_alerts([Alert | Alerts], {next_state, StateName, State, _Actions}) ->
     handle_alerts(Alerts, ssl_gen_statem:handle_alert(Alert, StateName, State)).

encode_handshake(Handshake, Version, ConnectionStates0, Hist0) ->
    Frag = tls_handshake:encode_handshake(Handshake, Version),
    Hist = ssl_handshake:update_handshake_history(Hist0, Frag),
    {Encoded, ConnectionStates} =
        tls_record:encode_handshake(Frag, Version, ConnectionStates0),
    {Encoded, ConnectionStates, Hist}.

encode_change_cipher(#change_cipher_spec{}, Version, ConnectionStates) ->
    tls_record:encode_change_cipher_spec(Version, ConnectionStates).

decode_alerts(Bin) ->
    ssl_alert:decode(Bin).

gen_info(Event, connection = StateName,  #state{connection_env = #connection_env{negotiated_version = Version}} = State) ->
    try handle_info(Event, StateName, State) of
	Result ->
	    Result
    catch 
        _:_ ->
	    ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?INTERNAL_ERROR,
						       malformed_data), 
					    Version, StateName, State)  
    end;

gen_info(Event, StateName, #state{connection_env = #connection_env{negotiated_version = Version}} = State) ->
    try handle_info(Event, StateName, State) of
	Result ->
	    Result
    catch 
        _:_ ->
	    ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE,
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


assert_buffer_sanity(<<?BYTE(_Type), ?UINT24(Length), Rest/binary>>, 
                     #{max_handshake_size := Max}) when
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

ensure_sender_terminate(downgrade, _) ->
    ok; %% Do not terminate sender during downgrade phase 
ensure_sender_terminate(_,  #state{protocol_specific = #{sender := Sender}}) ->
    %% Make sure TLS sender dies when connection process is terminated normally
    %% This is needed if the tls_sender is blocked in prim_inet:send 
    Kill = fun() -> 
                   receive 
                   after 5000 ->
                           catch (exit(Sender, kill))
                   end
           end,
    spawn(Kill).

choose_tls_fsm(#{versions := Versions},
               #client_hello{
                  extensions = #{client_hello_versions :=
                                     #client_hello_versions{versions = ClientVersions}
                                }
                 }) ->
    case ssl_handshake:select_supported_version(ClientVersions, Versions) of
        {3,4} ->
            tls_1_3_fsm;
        _Else ->
            tls_1_0_to_1_2_fsm
    end;
choose_tls_fsm(_, _) ->
    tls_1_0_to_1_2_fsm.


%% Special version handling for TLS 1.3 clients:
%% In the shared state 'init' negotiated_version is set to requested version and
%% that is expected by the legacy part of the state machine. However, in order to
%% be able to process new TLS 1.3 extensions, the effective version shall be set
%% {3,4}.
%% When highest supported version is {3,4} the negotiated version is set to {3,3}.
effective_version({3,3} , #{versions := [Version|_]}, client) when Version >= {3,4} ->
    Version;
%% Use highest supported version during startup (TLS server, all versions).
effective_version(undefined, #{versions := [Version|_]}, _) ->
    Version;
%% Use negotiated version in all other cases.
effective_version(Version, _, _) ->
    Version.

