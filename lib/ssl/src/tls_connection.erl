%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2018. All Rights Reserved.
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
-include_lib("kernel/include/logger.hrl").

%% Internal application API

%% Setup
-export([start_fsm/8, start_link/8, init/1, pids/1]).

%% State transition handling	 
-export([next_event/3, next_event/4, 
         handle_protocol_record/3]).

%% Handshake handling
-export([renegotiation/2, renegotiate/2, send_handshake/2,
         send_handshake_flight/1,
	 queue_handshake/2, queue_change_cipher/2,
	 reinit/1, reinit_handshake_data/1, select_sni_extension/1, 
         empty_connection_state/2]).

%% Alert and close handling
-export([send_alert/2, send_alert_in_connection/2,
         send_sync_alert/2,
         encode_alert/3, close/5, protocol_name/0]).

%% Data handling
-export([encode_data/3, next_record/1,
         send/3, socket/5, setopts/3, getopts/3]).

%% gen_statem state functions
-export([init/3, error/3, downgrade/3, %% Initiation and take down states
	 hello/3, user_hello/3, certify/3, cipher/3, abbreviated/3, %% Handshake states 
	 connection/3]).
%% TLS 1.3 state functions (server)
-export([start/3,         %% common state with client
         negotiated/3,
         recvd_ch/3,
         wait_cert/3,     %% common state with client
         wait_cv/3,       %% common state with client
         wait_eoed/3,
         wait_finished/3, %% common state with client
         wait_flight2/3,
         connected/3      %% common state with client
        ]).
%% TLS 1.3 state functions (client)
-export([wait_cert_cr/3,
         wait_ee/3,
         wait_sh/3
        ]).
%% gen_statem callbacks
-export([callback_mode/0, terminate/3, code_change/4, format_status/2]).
 
-export([encode_handshake/4]).

-define(DIST_CNTRL_SPAWN_OPTS, [{priority, max}]).

%%====================================================================
%% Internal application API
%%====================================================================	     
%%====================================================================
%% Setup
%%====================================================================
start_fsm(Role, Host, Port, Socket, {#ssl_options{erl_dist = false},_, Tracker} = Opts,
	  User, {CbModule, _,_, _} = CbInfo, 
	  Timeout) -> 
    try 
        {ok, Sender} = tls_sender:start(),
	{ok, Pid} = tls_connection_sup:start_child([Role, Sender, Host, Port, Socket, 
						    Opts, User, CbInfo]), 
	{ok, SslSocket} = ssl_connection:socket_control(?MODULE, Socket, [Pid, Sender], CbModule, Tracker),
        ssl_connection:handshake(SslSocket, Timeout)
    catch
	error:{badmatch, {error, _} = Error} ->
	    Error
    end;

start_fsm(Role, Host, Port, Socket, {#ssl_options{erl_dist = true},_, Tracker} = Opts,
	  User, {CbModule, _,_, _} = CbInfo, 
	  Timeout) -> 
    try 
        {ok, Sender} = tls_sender:start([{spawn_opt, ?DIST_CNTRL_SPAWN_OPTS}]),
	{ok, Pid} = tls_connection_sup:start_child_dist([Role, Sender, Host, Port, Socket, 
							 Opts, User, CbInfo]), 
	{ok, SslSocket} = ssl_connection:socket_control(?MODULE, Socket, [Pid, Sender], CbModule, Tracker),
        ssl_connection:handshake(SslSocket, Timeout)
    catch
	error:{badmatch, {error, _} = Error} ->
	    Error
    end.

%%--------------------------------------------------------------------
-spec start_link(atom(), pid(), ssl:host(), inet:port_number(), port(), list(), pid(), tuple()) ->
    {ok, pid()} | ignore |  {error, reason()}.
%%
%% Description: Creates a gen_statem process which calls Module:init/1 to
%% initialize. 
%%--------------------------------------------------------------------
start_link(Role, Sender, Host, Port, Socket, Options, User, CbInfo) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [[Role, Sender, Host, Port, Socket, Options, User, CbInfo]])}.

init([Role, Sender, Host, Port, Socket, {SslOpts, _, _} = Options,  User, CbInfo]) ->
    process_flag(trap_exit, true),
    link(Sender),
    case SslOpts#ssl_options.erl_dist of
        true ->
            process_flag(priority, max);
        _ ->
            ok
    end,
    State0 = #state{protocol_specific = Map} = initial_state(Role, Sender,
                                                             Host, Port, Socket, Options, User, CbInfo),
    try 
	State = ssl_connection:ssl_config(State0#state.ssl_options, Role, State0),
        initialize_tls_sender(State),
        gen_statem:enter_loop(?MODULE, [], init, State)
    catch throw:Error ->
            EState = State0#state{protocol_specific = Map#{error => Error}},
            gen_statem:enter_loop(?MODULE, [], error, EState) 
    end.

pids(#state{protocol_specific = #{sender := Sender}}) ->
    [self(), Sender].

%%====================================================================
%% State transition handling
%%====================================================================
next_record(#state{handshake_env = 
                       #handshake_env{unprocessed_handshake_events = N} = HsEnv} 
            = State) when N > 0 ->
    {no_record, State#state{handshake_env = 
                                HsEnv#handshake_env{unprocessed_handshake_events = N-1}}};
next_record(#state{protocol_buffers =
	       #protocol_buffers{tls_packets = [], tls_cipher_texts = [#ssl_tls{type = Type}| _] = CipherTexts0}
                   = Buffers,
                   connection_states = ConnectionStates0,
                   negotiated_version = Version,
                   ssl_options = #ssl_options{padding_check = Check}} = State) ->
    case decode_cipher_texts(Version, Type, CipherTexts0, ConnectionStates0, Check, <<>>) of
        {#ssl_tls{} = Record, ConnectionStates, CipherTexts} ->
            {Record, State#state{protocol_buffers = Buffers#protocol_buffers{tls_cipher_texts = CipherTexts},
                                 connection_states = ConnectionStates}};
        {#alert{} = Alert, ConnectionStates, CipherTexts} ->
            {Alert, State#state{protocol_buffers = Buffers#protocol_buffers{tls_cipher_texts = CipherTexts},
                                connection_states = ConnectionStates}}
    end;            
next_record(#state{protocol_buffers = #protocol_buffers{tls_packets = [], tls_cipher_texts = []},
                   protocol_specific = #{active_n_toggle := true, active_n := N} = ProtocolSpec,
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
     end;
next_record(State) ->
    {no_record, State}.

next_event(StateName, Record, State) ->
    next_event(StateName, Record, State, []).
next_event(StateName, no_record, State0, Actions) ->
    case next_record(State0) of
 	{no_record, State} ->
            {next_state, StateName, State, Actions};
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

decode_cipher_texts(_, Type, [] = CipherTexts, ConnectionStates, _, Acc) ->
    {#ssl_tls{type = Type, fragment = Acc}, ConnectionStates, CipherTexts};
decode_cipher_texts(Version, Type, 
                    [#ssl_tls{type = Type} = CT | CipherTexts], ConnectionStates0, Check, Acc) ->
    case tls_record:decode_cipher_text(Version, CT, ConnectionStates0, Check) of
	{#ssl_tls{type = ?APPLICATION_DATA, fragment = Plain}, ConnectionStates} ->		      
            decode_cipher_texts(Version, Type, CipherTexts, 
                                ConnectionStates, Check, <<Acc/binary, Plain/binary>>);
        {#ssl_tls{type = Type0, fragment = Plain}, ConnectionStates} ->
            {#ssl_tls{type = Type0, fragment = Plain}, ConnectionStates, CipherTexts};
        #alert{} = Alert ->
            {Alert, ConnectionStates0, CipherTexts}
    end;
decode_cipher_texts(_, Type, CipherTexts, ConnectionStates, _, Acc) ->
    {#ssl_tls{type = Type, fragment = Acc}, ConnectionStates, CipherTexts}.

%%% TLS record protocol level application data messages 

handle_protocol_record(#ssl_tls{type = ?APPLICATION_DATA, fragment = Data}, StateName, State0) ->
    case ssl_connection:read_application_data(Data, State0) of
	{stop, _, _} = Stop->
            Stop;
	{Record, State1} ->
            {next_state, StateName, State, Actions} = next_event(StateName, Record, State1), 
            ssl_connection:hibernate_after(StateName, State, Actions)
    end;
%%% TLS record protocol level handshake messages 
handle_protocol_record(#ssl_tls{type = ?HANDSHAKE, fragment = Data}, 
		    StateName, #state{protocol_buffers =
					  #protocol_buffers{tls_handshake_buffer = Buf0} = Buffers,
				      negotiated_version = Version,
				      ssl_options = Options} = State0) ->
    try
	EffectiveVersion = effective_version(Version, Options),
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
                        ssl_connection:hibernate_after(StateName, State, Events);
                    _ ->
                        HsEnv = State#state.handshake_env,
                        {next_state, StateName, 
                         State#state{protocol_buffers = Buffers,
                                     handshake_env = 
                                         HsEnv#handshake_env{unprocessed_handshake_events 
                                                             = unprocessed_events(Events)}}, Events}
                end
        end
    catch throw:#alert{} = Alert ->
            ssl_connection:handle_own_alert(Alert, Version, StateName, State0)
    end;
%%% TLS record protocol level change cipher messages
handle_protocol_record(#ssl_tls{type = ?CHANGE_CIPHER_SPEC, fragment = Data}, StateName, State) ->
    {next_state, StateName, State, [{next_event, internal, #change_cipher_spec{type = Data}}]};
%%% TLS record protocol level Alert messages
handle_protocol_record(#ssl_tls{type = ?ALERT, fragment = EncAlerts}, StateName,
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
		   negotiated_version = Version,
		   connection_states = ConnectionStates0} = State0, Actions) ->
    HelloRequest = ssl_handshake:hello_request(),
    Frag = tls_handshake:encode_handshake(HelloRequest, Version),
    Hs0 = ssl_handshake:init_handshake_history(),
    {BinMsg, ConnectionStates} = 
	tls_record:encode_handshake(Frag, Version, ConnectionStates0),
    send(Transport, Socket, BinMsg),
    State = State0#state{connection_states = 
			     ConnectionStates,
			 handshake_env = HsEnv#handshake_env{tls_handshake_history = Hs0}},
    next_event(hello, no_record, State, Actions).
	     
send_handshake(Handshake, State) ->
    send_handshake_flight(queue_handshake(Handshake, State)).

queue_handshake(Handshake, #state{negotiated_version = Version,
				  handshake_env = #handshake_env{tls_handshake_history = Hist0} = HsEnv,
				  flight_buffer = Flight0,
				  connection_states = ConnectionStates0,
                                  ssl_options = SslOpts} = State0) ->
    {BinHandshake, ConnectionStates, Hist} =
	encode_handshake(Handshake, Version, ConnectionStates0, Hist0),
    ssl_logger:debug(SslOpts#ssl_options.log_level, outbound, 'handshake', Handshake),
    ssl_logger:debug(SslOpts#ssl_options.log_level, outbound, 'tls_record', BinHandshake),

    State0#state{connection_states = ConnectionStates,
                 handshake_env = HsEnv#handshake_env{tls_handshake_history = Hist},
		 flight_buffer = Flight0 ++ [BinHandshake]}.

send_handshake_flight(#state{static_env = #static_env{socket = Socket,
                                                      transport_cb = Transport},
			     flight_buffer = Flight} = State0) ->
    send(Transport, Socket, Flight),
    {State0#state{flight_buffer = []}, []}.

queue_change_cipher(Msg, #state{negotiated_version = Version,
                                flight_buffer = Flight0,
                                connection_states = ConnectionStates0,
                                ssl_options = SslOpts} = State0) ->
    {BinChangeCipher, ConnectionStates} =
	encode_change_cipher(Msg, Version, ConnectionStates0),
    ssl_logger:debug(SslOpts#ssl_options.log_level, outbound, 'tls_record', BinChangeCipher),
    State0#state{connection_states = ConnectionStates,
		 flight_buffer = Flight0 ++ [BinChangeCipher]}.

reinit(#state{protocol_specific = #{sender := Sender},
              negotiated_version = Version,
              connection_states = #{current_write := Write}} = State) -> 
    tls_sender:update_connection_state(Sender, Write, Version),
    reinit_handshake_data(State).

reinit_handshake_data(#state{handshake_env = HsEnv} =State) ->
    %% premaster_secret, public_key_info and tls_handshake_info 
    %% are only needed during the handshake phase. 
    %% To reduce memory foot print of a connection reinitialize them.
     State#state{
       premaster_secret = undefined,
       public_key_info = undefined,
       handshake_env = HsEnv#handshake_env{tls_handshake_history = ssl_handshake:init_handshake_history()}
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

send_alert(Alert, #state{negotiated_version = Version,                         
                         static_env = #static_env{socket = Socket,
                                                  transport_cb = Transport},
                         ssl_options = SslOpts,
                         connection_states = ConnectionStates0} = StateData0) ->
    {BinMsg, ConnectionStates} =
        encode_alert(Alert, Version, ConnectionStates0),
    send(Transport, Socket, BinMsg),
    ssl_logger:debug(SslOpts#ssl_options.log_level, outbound, 'tls_record', BinMsg),
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
    Transport:close(Socket).
protocol_name() ->
    "TLS".

%%====================================================================
%% Data handling
%%====================================================================	     
encode_data(Data, Version, ConnectionStates0)->
    tls_record:encode_data(Data, Version, ConnectionStates0).

send(Transport, Socket, Data) ->
   tls_socket:send(Transport, Socket, Data).

socket(Pids,  Transport, Socket, Connection, Tracker) ->
    tls_socket:socket(Pids, Transport, Socket, Connection, Tracker).

setopts(Transport, Socket, Other) ->
    tls_socket:setopts(Transport, Socket, Other).

getopts(Transport, Socket, Tag) ->
    tls_socket:getopts(Transport, Socket, Tag).

%%--------------------------------------------------------------------
%% State functions
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
-spec init(gen_statem:event_type(),
	   {start, timeout()} | term(), #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------

init({call, From}, {start, Timeout}, 
     #state{static_env = #static_env{role = client,
                                     host = Host,
                                     port = Port,
                                     transport_cb = Transport,
                                     socket = Socket,
                                     session_cache = Cache,
                                     session_cache_cb = CacheCb},
            handshake_env = #handshake_env{renegotiation = {Renegotiation, _}} = HsEnv,
	    ssl_options = SslOpts,
	    session = #session{own_certificate = Cert} = Session0,
	    connection_states = ConnectionStates0
	   } = State0) ->
    KeyShare = maybe_generate_client_shares(SslOpts),
    Timer = ssl_connection:start_or_recv_cancel_timer(Timeout, From),
    Hello = tls_handshake:client_hello(Host, Port, ConnectionStates0, SslOpts,
				       Cache, CacheCb, Renegotiation, Cert, KeyShare),

    HelloVersion = tls_record:hello_version(SslOpts#ssl_options.versions),
    Handshake0 = ssl_handshake:init_handshake_history(),
    {BinMsg, ConnectionStates, Handshake} =
        encode_handshake(Hello,  HelloVersion, ConnectionStates0, Handshake0),
    send(Transport, Socket, BinMsg),
    ssl_logger:debug(SslOpts#ssl_options.log_level, outbound, 'handshake', Hello),
    ssl_logger:debug(SslOpts#ssl_options.log_level, outbound, 'tls_record', BinMsg),

    State = State0#state{connection_states = ConnectionStates,
			  negotiated_version = HelloVersion, %% Requested version
                         session =
                             Session0#session{session_id = Hello#client_hello.session_id},
                         handshake_env = HsEnv#handshake_env{tls_handshake_history = Handshake},
                         start_or_recv_from = From,
                         timer = Timer,
                         key_share = KeyShare},    
    next_event(hello, no_record, State);

init(Type, Event, State) ->
    gen_handshake(?FUNCTION_NAME, Type, Event, State).
 
%%--------------------------------------------------------------------
-spec error(gen_statem:event_type(),
	   {start, timeout()} | term(), #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
error({call, From}, {start, _Timeout}, 
      #state{protocol_specific = #{error := Error}} = State) ->
    {stop_and_reply, {shutdown, normal}, 
     [{reply, From, {error, Error}}], State};

error({call, _} = Call, Msg, State) ->
    gen_handshake(?FUNCTION_NAME, Call, Msg, State);
error(_, _, _) ->
     {keep_state_and_data, [postpone]}.
 
%%--------------------------------------------------------------------
-spec hello(gen_statem:event_type(),
	    #hello_request{} | #client_hello{} | #server_hello{} | term(),
	    #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
hello(internal, #client_hello{extensions = Extensions} = Hello, 
      #state{ssl_options = #ssl_options{handshake = hello},
             start_or_recv_from = From} = State) ->
    {next_state, user_hello, State#state{start_or_recv_from = undefined,
                                              hello = Hello},
     [{reply, From, {ok, Extensions}}]};
hello(internal, #server_hello{extensions = Extensions} = Hello, 
      #state{ssl_options = #ssl_options{handshake = hello},
             start_or_recv_from = From} = State) ->
    {next_state, user_hello, State#state{start_or_recv_from = undefined,
                                         hello = Hello},
     [{reply, From, {ok, Extensions}}]};     
hello(internal, #client_hello{client_version = ClientVersion} = Hello,
      #state{connection_states = ConnectionStates0,
             static_env = #static_env{
                             port = Port,
                             session_cache = Cache,
                             session_cache_cb = CacheCb},
             handshake_env = #handshake_env{renegotiation = {Renegotiation, _}} = HsEnv,
             session = #session{own_certificate = Cert} = Session0,
	     negotiated_protocol = CurrentProtocol,
	     key_algorithm = KeyExAlg,
	     ssl_options = SslOpts} = State) ->

    case choose_tls_version(SslOpts, Hello) of
        'tls_v1.3' ->
            %% Continue in TLS 1.3 'start' state
            {next_state, start, State, [{next_event, internal, Hello}]};
        'tls_v1.2' ->
            case tls_handshake:hello(Hello,
                                     SslOpts,
                                     {Port, Session0, Cache, CacheCb,
                                      ConnectionStates0, Cert, KeyExAlg},
                                     Renegotiation) of
                #alert{} = Alert ->
                    ssl_connection:handle_own_alert(Alert, ClientVersion, hello,
                                                    State#state{negotiated_version
                                                                = ClientVersion});
                {Version, {Type, Session},
                 ConnectionStates, Protocol0, ServerHelloExt, HashSign} ->
                    Protocol = case Protocol0 of
                                   undefined -> CurrentProtocol;
                                   _ -> Protocol0
                               end,
                    gen_handshake(?FUNCTION_NAME,
                                  internal,
                                  {common_client_hello, Type, ServerHelloExt},
                                  State#state{connection_states  = ConnectionStates,
                                              negotiated_version = Version,
                                              hashsign_algorithm = HashSign,
                                              handshake_env = HsEnv#handshake_env{client_hello_version = 
                                                                                      ClientVersion},
                                              session = Session,
                                              negotiated_protocol = Protocol})
            end
    end;
hello(internal, #server_hello{} = Hello,      
      #state{connection_states = ConnectionStates0,
	     negotiated_version = ReqVersion,
	     static_env = #static_env{role = client},
             handshake_env = #handshake_env{renegotiation = {Renegotiation, _}},
	     ssl_options = SslOptions} = State) ->
    case tls_handshake:hello(Hello, SslOptions, ConnectionStates0, Renegotiation) of
	#alert{} = Alert ->
	    ssl_connection:handle_own_alert(Alert, ReqVersion, hello,
                                            State#state{negotiated_version = ReqVersion});
	{Version, NewId, ConnectionStates, ProtoExt, Protocol} ->
	    ssl_connection:handle_session(Hello, 
					  Version, NewId, ConnectionStates, ProtoExt, Protocol, State)
    end;
hello(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
hello(Type, Event, State) ->
    gen_handshake(?FUNCTION_NAME, Type, Event, State).

user_hello(Type, Event, State) ->
    gen_handshake(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec abbreviated(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
abbreviated(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
abbreviated(Type, Event, State) ->
    gen_handshake(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec certify(gen_statem:event_type(), term(), #state{}) ->
		     gen_statem:state_function_result().
%%--------------------------------------------------------------------
certify(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
certify(Type, Event, State) ->
    gen_handshake(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec cipher(gen_statem:event_type(), term(), #state{}) ->
		    gen_statem:state_function_result().
%%--------------------------------------------------------------------
cipher(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
cipher(Type, Event, State) ->
     gen_handshake(?FUNCTION_NAME, Type, Event, State).

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
connection({call, From}, 
           {close, {Pid, _Timeout}}, 
           #state{terminated = closed} = State) ->
    {next_state, downgrade, State#state{terminated = true, downgrade = {Pid, From}}, 
     [{next_event, internal, ?ALERT_REC(?WARNING, ?CLOSE_NOTIFY)}]};
connection({call, From}, 
           {close,{Pid, Timeout}},
           #state{connection_states = ConnectionStates,
                  protocol_specific = #{sender := Sender}
                 } = State0) ->
    case tls_sender:downgrade(Sender, Timeout) of
        {ok, Write} ->
            %% User downgrades connection
            %% When downgrading an TLS connection to a transport connection
            %% we must recive the close alert from the peer before releasing the 
            %% transport socket.
            State = send_alert(?ALERT_REC(?WARNING, ?CLOSE_NOTIFY), 
                               State0#state{connection_states =
                                                ConnectionStates#{current_write => Write}}),
            {next_state, downgrade, State#state{downgrade = {Pid, From},
                                                terminated = true}, [{timeout, Timeout, downgrade}]};
        {error, timeout} ->
            {stop_and_reply, {shutdown, downgrade_fail}, [{reply, From, {error, timeout}}]}
    end;
connection(internal, #hello_request{},
	   #state{static_env = #static_env{role = client,
                                           host = Host,
                                           port = Port,
                                           session_cache = Cache,
                                           session_cache_cb = CacheCb},
                  handshake_env = #handshake_env{renegotiation = {Renegotiation, peer}},
		  session = #session{own_certificate = Cert} = Session0,
		  ssl_options = SslOpts, 
                  protocol_specific = #{sender := Pid},
		  connection_states = ConnectionStates} = State0) ->
    try tls_sender:peer_renegotiate(Pid) of
        {ok, Write} ->
            Hello = tls_handshake:client_hello(Host, Port, ConnectionStates, SslOpts,
                                               Cache, CacheCb, Renegotiation, Cert, undefined),
            {State, Actions} = send_handshake(Hello, State0#state{connection_states = ConnectionStates#{current_write => Write}}),
            next_event(hello, no_record, State#state{session = Session0#session{session_id
                                                                      = Hello#client_hello.session_id}}, Actions)
        catch 
            _:_ ->
                {stop, {shutdown, sender_blocked}, State0}
        end;
connection(internal, #hello_request{},
	   #state{static_env = #static_env{role = client,
                                           host = Host,
                                           port = Port,
                                           session_cache = Cache,
                                           session_cache_cb = CacheCb},
                  handshake_env = #handshake_env{renegotiation = {Renegotiation, _}},
		  session = #session{own_certificate = Cert} = Session0,
		  ssl_options = SslOpts, 
		  connection_states = ConnectionStates} = State0) ->
    Hello = tls_handshake:client_hello(Host, Port, ConnectionStates, SslOpts,
				       Cache, CacheCb, Renegotiation, Cert, undefined),

    {State, Actions} = send_handshake(Hello, State0),
    next_event(hello, no_record, State#state{session = Session0#session{session_id
                                                                        = Hello#client_hello.session_id}}, Actions);
connection(internal, #client_hello{} = Hello, 
	   #state{static_env = #static_env{role = server},
                  handshake_env = HsEnv,
                  allow_renegotiate = true,
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
                                             allow_renegotiate = false,
                                             handshake_env = HsEnv#handshake_env{renegotiation = {true, peer}}
                                            }, 
               [{next_event, internal, Hello}]);
connection(internal, #client_hello{}, 
	   #state{static_env = #static_env{role = server,
                  protocol_cb = Connection},
                  allow_renegotiate = false} = State0) ->
    Alert = ?ALERT_REC(?WARNING, ?NO_RENEGOTIATION),
    send_alert_in_connection(Alert, State0),
    State = Connection:reinit_handshake_data(State0),
    next_event(?FUNCTION_NAME, no_record, State);

connection(Type, Event, State) ->
    ssl_connection:?FUNCTION_NAME(Type, Event, State, ?MODULE).

%%--------------------------------------------------------------------
-spec downgrade(gen_statem:event_type(), term(), #state{}) ->
		       gen_statem:state_function_result().
%%--------------------------------------------------------------------
downgrade(internal, #alert{description = ?CLOSE_NOTIFY},
	  #state{static_env = #static_env{transport_cb = Transport,
                                          socket = Socket},
		 downgrade = {Pid, From}} = State) ->
    tls_socket:setopts(Transport, Socket, [{active, false}, {packet, 0}, {mode, binary}]),
    Transport:controlling_process(Socket, Pid),
    {stop_and_reply, {shutdown, downgrade},[{reply, From, {ok, Socket}}], State};
downgrade(timeout, downgrade, #state{downgrade = {_, From}} = State) ->
    {stop_and_reply, {shutdown, normal},[{reply, From, {error, timeout}}], State};
downgrade(info, {CloseTag, Socket},
          #state{static_env = #static_env{socket = Socket, 
                                          close_tag = CloseTag}, downgrade = {_, From}} =
              State) ->
    {stop_and_reply, {shutdown, normal},[{reply, From, {error, CloseTag}}], State};
downgrade(info, Info, State) ->
    handle_info(Info, ?FUNCTION_NAME, State);
downgrade(Type, Event, State) ->
     ssl_connection:?FUNCTION_NAME(Type, Event, State, ?MODULE).

%%--------------------------------------------------------------------
%% TLS 1.3 state functions
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
-spec start(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
start(info, Event, State) ->
    gen_info_1_3(Event, ?FUNCTION_NAME, State);
start(Type, Event, State) ->
    gen_handshake_1_3(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec negotiated(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
negotiated(info, Event, State) ->
    gen_info_1_3(Event, ?FUNCTION_NAME, State);
negotiated(Type, Event, State) ->
    gen_handshake_1_3(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec recvd_ch(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
recvd_ch(info, Event, State) ->
    gen_info_1_3(Event, ?FUNCTION_NAME, State);
recvd_ch(Type, Event, State) ->
    gen_handshake_1_3(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec wait_cert(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_cert(info, Event, State) ->
    gen_info_1_3(Event, ?FUNCTION_NAME, State);
wait_cert(Type, Event, State) ->
    gen_handshake_1_3(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec wait_cv(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_cv(info, Event, State) ->
    gen_info_1_3(Event, ?FUNCTION_NAME, State);
wait_cv(Type, Event, State) ->
    gen_handshake_1_3(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec wait_eoed(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_eoed(info, Event, State) ->
    gen_info_1_3(Event, ?FUNCTION_NAME, State);
wait_eoed(Type, Event, State) ->
    gen_handshake_1_3(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec wait_finished(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_finished(info, Event, State) ->
    gen_info_1_3(Event, ?FUNCTION_NAME, State);
wait_finished(Type, Event, State) ->
    gen_handshake_1_3(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec wait_flight2(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_flight2(info, Event, State) ->
    gen_info_1_3(Event, ?FUNCTION_NAME, State);
wait_flight2(Type, Event, State) ->
    gen_handshake_1_3(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec connected(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
connected(info, Event, State) ->
    gen_info_1_3(Event, ?FUNCTION_NAME, State);
connected(Type, Event, State) ->
    gen_handshake_1_3(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec wait_cert_cr(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_cert_cr(info, Event, State) ->
    gen_info_1_3(Event, ?FUNCTION_NAME, State);
wait_cert_cr(Type, Event, State) ->
    gen_handshake_1_3(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec wait_ee(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_ee(info, Event, State) ->
    gen_info_1_3(Event, ?FUNCTION_NAME, State);
wait_ee(Type, Event, State) ->
    gen_handshake_1_3(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec wait_sh(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_sh(info, Event, State) ->
    gen_info_1_3(Event, ?FUNCTION_NAME, State);
wait_sh(Type, Event, State) ->
    gen_handshake_1_3(?FUNCTION_NAME, Type, Event, State).

%--------------------------------------------------------------------
%% gen_statem callbacks
%%--------------------------------------------------------------------
callback_mode() ->
    state_functions.

terminate({shutdown, sender_died, Reason}, _StateName,
          #state{static_env = #static_env{socket = Socket, 
                                          transport_cb = Transport}} 
          = State) ->
    ssl_connection:handle_trusted_certs_db(State),
    close(Reason, Socket, Transport, undefined, undefined);
terminate(Reason, StateName, State) ->
    catch ssl_connection:terminate(Reason, StateName, State),
    ensure_sender_terminate(Reason, State).

format_status(Type, Data) ->
    ssl_connection:format_status(Type, Data).

code_change(_OldVsn, StateName, State, _) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
initial_state(Role, Sender, Host, Port, Socket, {SSLOptions, SocketOptions, Tracker}, User,
	      {CbModule, DataTag, CloseTag, ErrorTag}) ->
    #ssl_options{beast_mitigation = BeastMitigation,
                 erl_dist = IsErlDist} = SSLOptions,
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
                     host = Host,
                     port = Port,
                     socket = Socket,
                     session_cache_cb = SessionCacheCb,
                     tracker = Tracker
                    },
    #state{
       static_env = InitStatEnv,
       handshake_env = #handshake_env{
                          tls_handshake_history = ssl_handshake:init_handshake_history(),
                          renegotiation = {false, first}
                         },
       socket_options = SocketOptions,
       ssl_options = SSLOptions,
       session = #session{is_resumable = new},
       connection_states = ConnectionStates,
       protocol_buffers = #protocol_buffers{},
       user_application = {UserMonitor, User},
       user_data_buffer = <<>>,
       allow_renegotiate = SSLOptions#ssl_options.client_renegotiation,
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
                                             protocol_cb = Connection,
                                             socket = Socket,
                                             tracker = Tracker
                                            },
                             socket_options = SockOpts,
                             negotiated_version = Version,
                             ssl_options = #ssl_options{renegotiate_at = RenegotiateAt,
                                                        log_level = LogLevel},
                             connection_states = #{current_write := ConnectionWriteState},
                             protocol_specific = #{sender := Sender}}) ->
    Init = #{current_write => ConnectionWriteState,
             role => Role,
             socket => Socket,
             socket_options => SockOpts,
             tracker => Tracker,
             protocol_cb => Connection,
             transport_cb => Transport,
             negotiated_version => Version,
             renegotiate_at => RenegotiateAt,
             log_level => LogLevel},
    tls_sender:initialize(Sender, Init).
    
next_tls_record(Data, StateName, #state{protocol_buffers = 
                                            #protocol_buffers{tls_record_buffer = Buf0,
                                                              tls_cipher_texts = CT0} = Buffers,
                                        ssl_options = SslOpts} = State0) ->
    case tls_record:get_tls_records(Data,
                                    acceptable_record_versions(StateName, State0),
                                    Buf0, SslOpts) of
	{Records, Buf1} ->
	    CT1 = CT0 ++ Records,
	    next_record(State0#state{protocol_buffers =
					 Buffers#protocol_buffers{tls_record_buffer = Buf1,
								  tls_cipher_texts = CT1}});
	#alert{} = Alert ->
	    handle_record_alert(Alert, State0)
    end.

%% TLS 1.3 Client/Server
%% - Ignore TLSPlaintext.legacy_record_version
%% - Verify that TLSCiphertext.legacy_record_version is set to  0x0303 for all records
%%   other than an initial ClientHello, where it MAY also be 0x0301.
acceptable_record_versions(StateName, #state{negotiated_version = Version}) when StateName =/= hello->
    Version;
acceptable_record_versions(hello, _) ->
    [tls_record:protocol_version(Vsn) || Vsn <- ?ALL_AVAILABLE_VERSIONS].

handle_record_alert(Alert, _) ->
    Alert.

tls_handshake_events(Packets) ->
    lists:map(fun(Packet) ->
		      {next_event, internal, {handshake, Packet}}
	      end, Packets).

%% raw data from socket, upack records
handle_info({Protocol, _, Data}, StateName,
            #state{static_env = #static_env{data_tag = Protocol}} = State0) ->
    case next_tls_record(Data, StateName, State0) of
	{Record, State} ->
	    next_event(StateName, Record, State);
	#alert{} = Alert ->
	    ssl_connection:handle_normal_shutdown(Alert, StateName, State0), 
	    {stop, {shutdown, own_alert}, State0}
    end;
handle_info({tcp_passive, Socket},  StateName, 
            #state{static_env = #static_env{socket = Socket},
                   protocol_specific = PS
                  } = State) ->
    next_event(StateName, no_record, 
               State#state{protocol_specific = PS#{active_n_toggle => true}});
handle_info({CloseTag, Socket}, StateName,
            #state{static_env = #static_env{socket = Socket, close_tag = CloseTag},
                   socket_options = #socket_options{active = Active},
                   protocol_buffers = #protocol_buffers{tls_cipher_texts = CTs},
                   user_data_buffer = Buffer,
                   protocol_specific = PS,
		   negotiated_version = Version} = State) ->

    %% Note that as of TLS 1.1,
    %% failure to properly close a connection no longer requires that a
    %% session not be resumed.  This is a change from TLS 1.0 to conform
    %% with widespread implementation practice.

    case (Active == false) andalso ((CTs =/= []) or (Buffer =/= <<>>)) of
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
            {stop, {shutdown, transport_closed}, State};
        true ->
            %% Fixes non-delivery of final TLS record in {active, once}.
            %% Basically allows the application the opportunity to set {active, once} again
            %% and then receive the final message. Set internal active_n to zero 
            %% to ensure socket close message is sent if there is not enough data to deliver.
            next_event(StateName, no_record, State#state{protocol_specific = PS#{active_n_toggle => true}})
    end;
handle_info({'EXIT', Sender, Reason}, _,
            #state{protocol_specific = #{sender := Sender}} = State) ->
    {stop, {shutdown, sender_died, Reason}, State};
handle_info(Msg, StateName, State) ->
    ssl_connection:StateName(info, Msg, State, ?MODULE).

handle_alerts([], Result) ->
    Result;
handle_alerts(_, {stop, _, _} = Stop) ->
    Stop;
handle_alerts([#alert{level = ?WARNING, description = ?CLOSE_NOTIFY} | _Alerts], 
              {next_state, connection = StateName, #state{user_data_buffer = Buffer,
                                                          socket_options = #socket_options{active = false},
                                                          protocol_buffers = #protocol_buffers{tls_cipher_texts = CTs}} = 
                   State}) when (Buffer =/= <<>>) orelse
                                (CTs =/= []) -> 
    {next_state, StateName, State#state{terminated = true}};
handle_alerts([Alert | Alerts], {next_state, StateName, State}) ->
     handle_alerts(Alerts, ssl_connection:handle_alert(Alert, StateName, State));
handle_alerts([Alert | Alerts], {next_state, StateName, State, _Actions}) ->
     handle_alerts(Alerts, ssl_connection:handle_alert(Alert, StateName, State)).

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

gen_handshake(StateName, Type, Event, 
	      #state{negotiated_version = Version} = State) ->
    try ssl_connection:StateName(Type, Event, State, ?MODULE) of
	Result ->
	    Result
    catch 
	_:_ ->
 	    ssl_connection:handle_own_alert(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, 
						       malformed_handshake_data),
					    Version, StateName, State)  
    end.

gen_handshake_1_3(StateName, Type, Event,
	      #state{negotiated_version = Version} = State) ->
    try tls_connection_1_3:StateName(Type, Event, State, ?MODULE) of
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

gen_info_1_3(Event, connected = StateName,  #state{negotiated_version = Version} = State) ->
    try handle_info(Event, StateName, State) of
	Result ->
	    Result
    catch
	_:_ ->
	    ssl_connection:handle_own_alert(?ALERT_REC(?FATAL, ?INTERNAL_ERROR,
						       malformed_data),
					    Version, StateName, State)
    end;

gen_info_1_3(Event, StateName, #state{negotiated_version = Version} = State) ->
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


assert_buffer_sanity(<<?BYTE(_Type), ?UINT24(Length), Rest/binary>>, 
                     #ssl_options{max_handshake_size = Max}) when 
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

maybe_generate_client_shares(#ssl_options{
                            versions = [Version|_],
                            supported_groups =
                                #supported_groups{
                                  supported_groups = Groups}})
  when Version =:= {3,4} ->
    ssl_cipher:generate_client_shares(Groups);
maybe_generate_client_shares(_) ->
    undefined.

choose_tls_version(#ssl_options{versions = Versions},
                   #client_hello{
                      extensions = #{client_hello_versions :=
                                         #client_hello_versions{versions = ClientVersions}
                                    }
                     }) ->
    case ssl_handshake:select_supported_version(ClientVersions, Versions) of
        {3,4} ->
            'tls_v1.3';
        _Else ->
            'tls_v1.2'
    end;
choose_tls_version(_, _) ->
    'tls_v1.2'.


effective_version(undefined, #ssl_options{versions = [Version|_]}) ->
    Version;
effective_version(Version, _) ->
    Version.
