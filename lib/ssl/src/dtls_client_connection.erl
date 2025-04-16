%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
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

-module(dtls_client_connection).
-moduledoc false.

%%----------------------------------------------------------------------
%% Purpose: DTLS-1-DTLS-1.2 FSM (* = optional)
%%----------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% For UDP transport the following flights are used as retransmission units
%% in case of package loss. Flight timers are handled in state entry functions.
%%
%%    Client                                          Server
%%    ------                                          ------
%%
%%    ClientHello             -------->                           Flight 1
%%
%%                            <-------    HelloVerifyRequest      Flight 2
%%
%%    ClientHello             -------->                           Flight 3
%%
%%                                               ServerHello    \
%%                                              Certificate*     \
%%                                        ServerKeyExchange*      Flight 4
%%                                       CertificateRequest*     /
%%                            <--------      ServerHelloDone    /
%%
%%    Certificate*                                              \
%%    ClientKeyExchange                                          \
%%    CertificateVerify*                                          Flight 5
%%    [ChangeCipherSpec]                                         /
%%    NextProtocol*                                             /
%%    Finished                -------->                        /
%%
%%                                        [ChangeCipherSpec]    \ Flight 6
%%                            <--------             Finished    /
%%
%%                Message Flights for Full Handshake
%%
%%
%%    Client                                           Server
%%    ------                                           ------
%%
%%    ClientHello             -------->                          Abbrev Flight 1
%%
%%                                               ServerHello    \ part 1
%%                                        [ChangeCipherSpec]     Abbrev Flight 2
%%                             <--------             Finished    / part 2
%%
%%    [ChangeCipherSpec]                                         \ Abbrev Flight 3
%%    NextProtocol*                                              /
%%    Finished                 -------->                        /
%%
%%
%%                  Message Flights for Abbbriviated Handshake
%%----------------------------------------------------------------------
%%                                       Start FSM    ---> CONFIG_ERROR
%%                                                     Send error to user
%%                                          |          and shutdown
%%                                          |
%%                                          V
%%                                    INITIAL_HELLO
%%
%%                                          | Send/ Recv Flight 1
%%                                          |
%%                                          |
%%           USER_HELLO                     |
%%  <- Possibly let user provide            V
%%  options after looking at hello ex ->    HELLO
%%                                             | Send Recv Flight 2 to Flight 4 or
%%                                             | Abbrev Flight 1 to Abbrev Flight 2 part 1
%%                                             |
%%                                New session  | Resumed session
%%  WAIT_STAPLING   CERTIFY  <----------------------------------> ABBREVIATED
%%
%%  <- Possibly Receive  --  |                                              |
%%     OCSP Stapel ------>   | Send/ Recv Flight 5                          |
%%                           |                                              |
%%                           V                                              |  Send / Recv Abbrev
%%                                                                          |  Flight part 2
%%                                                                          |  to Abbrev Flight 3
%%                         CIPHER                                           |
%%                           |                                              |
%%                           |  Send/ Recv Flight 6                         |
%%                           |                                              |
%%                           V                                              V
%%                         ----------------------------------------------------
%%                                                    |
%%                                                    |
%%                                                    V
%%                                                 CONNECTION
%%                                                    |
%%                                                    |  Renegotiaton
%%                                                    V
%%                                               GO BACK TO HELLO
%%----------------------------------------------------------------------

%% Internal application API

-behaviour(gen_statem).

-include("dtls_connection.hrl").
-include("dtls_handshake.hrl").
-include("ssl_alert.hrl").
-include("ssl_cipher.hrl").
-include("ssl_internal.hrl").

%% Internal application API

%% Setup
-export([init/1]).

%% gen_statem state functions
-export([initial_hello/3,
         config_error/3,
         downgrade/3,
	 hello/3,
         user_hello/3,
         wait_stapling/3,
         certify/3,
         cipher/3,
         abbreviated/3,
	 connection/3]).

%% gen_statem callbacks
-export([callback_mode/0,
         terminate/3,
         code_change/4,
         format_status/2]).

%% Tracing
-export([handle_trace/3]).

%%====================================================================
%% Internal application API
%%====================================================================

%%====================================================================
%% Setup
%%====================================================================
init([Role, Tab, Host, Port, Socket, Options,  User, CbInfo]) ->
    process_flag(trap_exit, true),
    State0 = dtls_gen_connection:initial_state(Role, Tab, Host, Port, Socket,
                                               Options, User, CbInfo),
    #state{static_env = #static_env{user_socket = UserSocket}} = State0,
    User ! {self(), user_socket, UserSocket},
    try
	State = ssl_gen_statem:init_ssl_config(State0#state.ssl_options,
                                          Role, State0),
	gen_statem:enter_loop(?MODULE, [], initial_hello, State)
    catch
	throw:Error ->
            #state{protocol_specific = Map} = State0,
            EState = State0#state{protocol_specific = Map#{error => Error}},
	    gen_statem:enter_loop(?MODULE, [], config_error, EState)
    end.

%%--------------------------------------------------------------------
%% State functions
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
-spec initial_hello(gen_statem:event_type(),
                    {start, timeout()} | term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
initial_hello(enter, _, State) ->
    {keep_state, State};
initial_hello({call, From}, {start, Timeout},
     #state{static_env = #static_env{host = Host,
                                     port = Port,
                                     socket = {_, Socket},
                                     transport_cb = Transport,
                                     session_cache = Cache,
                                     session_cache_cb = CacheCb},
            protocol_specific = PS,
            handshake_env = #handshake_env{renegotiation = {Renegotiation, _}},
            connection_env = #connection_env{cert_key_alts = CertKeyAlts} = CEnv,
	    ssl_options = #{versions := Versions} = SslOpts,
	    session = Session0,
	    connection_states = ConnectionStates0
	   } = State0) ->
    Packages = maps:get(active_n, PS),
    dtls_socket:setopts(Transport, Socket, [{active,Packages}]),
    CertKeyPairs = ssl_certificate:available_cert_key_pairs(CertKeyAlts),
    Session = ssl_session:client_select_session({Host, Port, SslOpts}, Cache,
                                                CacheCb, Session0, CertKeyPairs),
    Hello = dtls_handshake:client_hello(Host, Port, ConnectionStates0, SslOpts,
					Session#session.session_id, Renegotiation),

    MaxFragEnum = maps:get(max_frag_enum, Hello#client_hello.extensions, undefined),
    ConnectionStates1 = ssl_record:set_max_fragment_length(MaxFragEnum, ConnectionStates0),
    Version = Hello#client_hello.client_version,
    HelloVersion = dtls_record:hello_version(Version, Versions),
    State1 = dtls_gen_connection:prepare_flight(
               State0#state{connection_env = CEnv#connection_env{negotiated_version = Version},
                            connection_states = ConnectionStates1}),
    {State2, Actions} =
        dtls_gen_connection:send_handshake(Hello,
                                           State1#state{connection_env =
                                                            CEnv#connection_env{negotiated_version
                                                                                = HelloVersion}}),
    State = State2#state{connection_env =
                             CEnv#connection_env{negotiated_version = Version}, %% RequestedVersion
                         session = Session,
                         recv = State2#state.recv#recv{from = From},
                         protocol_specific = PS#{active_n_toggle := false}
                        },
    dtls_gen_connection:next_event(hello, no_record, State,
                                   [{{timeout, handshake}, Timeout, close} | Actions]);
initial_hello({call, From}, {start, {Opts, EmOpts}, Timeout},
     #state{static_env = #static_env{role = Role},
            ssl_options = OrigSSLOptions,
            socket_options = SockOpts} = State0) ->
    try
        SslOpts = ssl_config:update_options(Opts, Role, OrigSSLOptions),
	State = ssl_gen_statem:ssl_config(SslOpts, Role, State0),
	initial_hello({call, From}, {start, Timeout},
                      State#state{socket_options =
                                      ssl_config:new_emulated(EmOpts, SockOpts)})
    catch throw:Error ->
	   {stop_and_reply, {shutdown, normal}, {reply, From, {error, Error}}, State0}
    end;
initial_hello(info, Event, State) ->
    dtls_gen_connection:gen_info(Event, ?STATE(initial_hello), State);
initial_hello(Type, Event, State) ->
    ssl_gen_statem:initial_hello(Type, Event, State).

%%--------------------------------------------------------------------
-spec config_error(gen_statem:event_type(),
                   {start, timeout()} | term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
config_error(enter, _, State) ->
    {keep_state, State};
config_error(Type, Event, State) ->
    ssl_gen_statem:config_error(Type, Event, State).

%%--------------------------------------------------------------------
-spec hello(gen_statem:event_type(),
	    #hello_request{} | #client_hello{} | #server_hello{} | term(),
	    #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
hello(enter, _, State0) ->
    {State, Actions} = dtls_gen_connection:handle_flight_timer(State0),
    {keep_state, State, Actions};
hello(internal, #hello_verify_request{cookie = Cookie},
      #state{static_env = #static_env{host = Host,
                                      port = Port},
             handshake_env = #handshake_env{renegotiation = {Renegotiation, _},
                                            stapling_state = StaplingState0} = HsEnv,
             connection_env = CEnv,
             ssl_options = SslOpts,
             session = #session{session_id = Id},
             connection_states = ConnectionStates0,
	     protocol_specific = PS
            } = State0) ->
    OcspNonce = tls_handshake:ocsp_nonce(SslOpts),
    Hello = dtls_handshake:client_hello(Host, Port, Cookie, ConnectionStates0,
					SslOpts, Id, Renegotiation, OcspNonce),
    Version = Hello#client_hello.client_version,
    State1 =
        dtls_gen_connection:prepare_flight(
          State0#state{handshake_env =
                           HsEnv#handshake_env{
                             tls_handshake_history = ssl_handshake:init_handshake_history(),
                             stapling_state = StaplingState0#{ocsp_nonce => OcspNonce}}}),
    {State2, Actions} = dtls_gen_connection:send_handshake(Hello, State1),
    State = State2#state{connection_env =
                             CEnv#connection_env{negotiated_version = Version}, % RequestedVersion
			 protocol_specific = PS#{current_cookie_secret => Cookie}},
    dtls_gen_connection:next_event(?STATE(hello), no_record, State, Actions);
hello(internal, #server_hello{extensions = Extensions},
      #state{handshake_env = #handshake_env{continue_status = pause},
             recv = #recv{from = From} = Recv} = State) ->
    {next_state, user_hello, State#state{recv = Recv#recv{from = undefined}},
     [{postpone, true},{reply, From, {ok, Extensions}}]};
hello(internal, #server_hello{} = Hello,
      #state{handshake_env = #handshake_env{
                                renegotiation = {Renegotiation, _},
                                stapling_state = StaplingState0} = HsEnv,
             connection_states = ConnectionStates0,
             session = #session{session_id = OldId},
             ssl_options = SslOptions} = State) ->
    try
        {Version, NewId, ConnectionStates, ProtoExt, Protocol, StaplingState} =
            dtls_handshake:hello(Hello, SslOptions, ConnectionStates0, Renegotiation, OldId),
        tls_dtls_client_connection:handle_session(
          Hello, Version, NewId, ConnectionStates, ProtoExt, Protocol,
          State#state{handshake_env =
                          HsEnv#handshake_env{
                            stapling_state = maps:merge(StaplingState0, StaplingState)}})
    catch throw:#alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, hello, State)
    end;
hello(internal, {handshake, {#hello_verify_request{} = Handshake, _}}, State) ->
    %% hello_verify should not be in handshake history
    {next_state, ?STATE(hello), State, [{next_event, internal, Handshake}]};
hello(internal,  #change_cipher_spec{type = <<1>>}, State0) ->
    Epoch = dtls_gen_connection:retransmit_epoch(hello, State0),
    {State1, Actions0} = dtls_gen_connection:send_handshake_flight(State0, Epoch),
    %% This will reset the retransmission timer by repeating the enter state event
    case dtls_gen_connection:next_event(?STATE(hello), no_record, State1, Actions0) of
        {next_state, ?STATE(hello), State, Actions} ->
            {repeat_state, State, Actions};
        {next_state, ?STATE(hello), State} ->
            {repeat_state, State, []};
        {stop, _, _} = Stop ->
            Stop
    end;
hello(state_timeout, Event, State) ->
    dtls_gen_connection:handle_state_timeout(Event, ?STATE(hello), State);
hello(info, Event, State) ->
    dtls_gen_connection:gen_info(Event, ?STATE(hello), State);
hello(Type, Event, State) ->
    gen_state(?STATE(hello), Type, Event, State).

%%--------------------------------------------------------------------
-spec user_hello(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
user_hello(enter, _, State) ->
    {keep_state, State};
user_hello(info, Event, State) ->
    dtls_gen_connection:gen_info(Event, ?STATE(user_hello), State);
user_hello(Type, Event, State) ->
    gen_state(?STATE(user_hello), Type, Event, State).

%%--------------------------------------------------------------------
-spec abbreviated(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
abbreviated(enter, _, State0) ->
    {State, Actions} = dtls_gen_connection:handle_flight_timer(State0),
    {keep_state, State, Actions};
abbreviated(state_timeout, Event, State) ->
    dtls_gen_connection:handle_state_timeout(Event, abbreviated, State);
abbreviated(info, Event, State) ->
    dtls_gen_connection:gen_info(Event, ?STATE(abbreviated), State);
abbreviated(internal = Type, #change_cipher_spec{} = Event,
            #state{connection_states = ConnectionStates0} = State) ->
    ConnectionStates1 = dtls_record:save_current_connection_state(ConnectionStates0, read),
    ConnectionStates = dtls_record:next_epoch(ConnectionStates1, read),
    gen_state(?STATE(abbreviated), Type, Event,
                  State#state{connection_states = ConnectionStates});
abbreviated(Type, Event, State) ->
    gen_state(?STATE(abbreviated), Type, Event, State).

%%--------------------------------------------------------------------
-spec wait_stapling(gen_statem:event_type(), term(), #state{}) ->
		     gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_stapling(enter, _Event, State0) ->
    {State, Actions} = dtls_gen_connection:handle_flight_timer(State0),
    {keep_state, State, Actions};
wait_stapling(state_timeout, Event, State) ->
    dtls_gen_connection:handle_state_timeout(Event, ?STATE(wait_stapling), State);
wait_stapling(info, Event, State) ->
    dtls_gen_connection:gen_info(Event, ?STATE(wait_stapling), State);
wait_stapling(Type, Event, State) ->
    gen_state(?STATE(wait_stapling), Type, Event, State).

%%--------------------------------------------------------------------
-spec certify(gen_statem:event_type(), term(), #state{}) ->
		     gen_statem:state_function_result().
%%--------------------------------------------------------------------
certify(enter, _, State0) ->
    {State, Actions} = dtls_gen_connection:handle_flight_timer(State0),
    {keep_state, State, Actions};
certify(internal = Type, #server_hello_done{} = Event, State) ->
   try tls_dtls_client_connection:certify(Type, Event,  dtls_gen_connection:prepare_flight(State))
    catch throw:#alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, ?STATE(certify), State)
    end;
certify(internal,  #change_cipher_spec{type = <<1>>}, State0) ->
    Epoch = dtls_gen_connection:retransmit_epoch(certify, State0),
    {State1, Actions0} = dtls_gen_connection:send_handshake_flight(State0, Epoch),
    %% This will reset the retransmission timer by repeating the enter state event
    case dtls_gen_connection:next_event(?STATE(certify), no_record, State1, Actions0) of
        {next_state, ?STATE(certify), State, Actions} ->
            {repeat_state, State, Actions};
        {next_state, ?STATE(certify), State} ->
            {repeat_state, State, []};
        {stop, _, _} = Stop ->
            Stop
    end;
certify(state_timeout, Event, State) ->
    dtls_gen_connection:handle_state_timeout(Event, ?STATE(certify), State);
certify(info, Event, State) ->
    dtls_gen_connection:gen_info(Event, ?STATE(certify), State);
certify(Type, Event, State) ->
    gen_state(?STATE(certify), Type, Event, State).

%%--------------------------------------------------------------------
-spec cipher(gen_statem:event_type(), term(), #state{}) ->
		    gen_statem:state_function_result().
%%--------------------------------------------------------------------
cipher(enter, _, State0) ->
    {State, Actions} = dtls_gen_connection:handle_flight_timer(State0),
    {keep_state, State, Actions};
cipher(info, Event, State) ->
    dtls_gen_connection:gen_info(Event, ?STATE(cipher), State);
cipher(state_timeout, Event, State) ->
    dtls_gen_connection:handle_state_timeout(Event, ?STATE(cipher), State);
cipher(internal = Type, #change_cipher_spec{type = <<1>>} = Event,
       #state{connection_states = ConnectionStates0} = State) ->
    ConnectionStates1 = dtls_record:save_current_connection_state(ConnectionStates0, read),
    ConnectionStates = dtls_record:next_epoch(ConnectionStates1, read),

     try tls_dtls_server_connection:cipher(Type, Event,
                                           State#state{connection_states = ConnectionStates})
     catch throw:#alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, cipher, State)
     end;
cipher(internal = Type, #finished{} = Event, #state{connection_states = ConnectionStates,
                                                    protocol_specific = PS} = State) ->

    try tls_dtls_client_connection:cipher(Type, Event,
                                          dtls_gen_connection:prepare_flight(
                                            State#state{connection_states = ConnectionStates,
                                                        protocol_specific =
                                                            PS#{flight_state => connection}}))
    catch throw:#alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, cipher, State)
    end;
cipher(Type, Event, State) ->
    gen_state(?STATE(cipher), Type, Event, State).

%%--------------------------------------------------------------------
-spec connection(gen_statem:event_type(),
		 #hello_request{} | #client_hello{}| term(), #state{}) ->
			gen_statem:state_function_result().
%%--------------------------------------------------------------------
connection(enter, _, State) ->
    {keep_state, State};
connection(internal, #hello_request{},
           #state{static_env = #static_env{host = Host,
                                           port = Port,
                                           data_tag = DataTag,
                                           session_cache = Cache,
                                           session_cache_cb = CacheCb
                                          },
                  handshake_env = #handshake_env{renegotiation = {Renegotiation, _}},
                  connection_env = #connection_env{cert_key_alts = CertKeyAlts} = CEnv,
                  session = Session0,
                  ssl_options = #{versions := Versions} = SslOpts,
                  connection_states = ConnectionStates0,
                  protocol_specific = PS
                 } = State0) ->
    #{current_cookie_secret := Cookie} = PS,
    CertKeyPairs = ssl_certificate:available_cert_key_pairs(CertKeyAlts),
    Session = ssl_session:client_select_session({Host, Port, SslOpts}, Cache, CacheCb,
                                                Session0, CertKeyPairs),
    Hello = dtls_handshake:client_hello(Host, Port, Cookie, ConnectionStates0, SslOpts,
					Session#session.session_id, Renegotiation, undefined),
    Version = Hello#client_hello.client_version,
    HelloVersion = dtls_record:hello_version(Version, Versions),
    State1 = dtls_gen_connection:prepare_flight(State0),
    {State2, Actions} =
        dtls_gen_connection:send_handshake(Hello,
                                           State1#state{connection_env =
                                                            CEnv#connection_env{negotiated_version
                                                                                = HelloVersion}}),
    State = State2#state{protocol_specific =
                             PS#{flight_state => dtls_gen_connection:initial_flight_state(DataTag)},
                         session = Session},
    dtls_gen_connection:next_event(hello, no_record, State, Actions);
connection(internal, #hello_request{},
	   #state{static_env = #static_env{host = Host,
                                           port = Port},
                  handshake_env = #handshake_env{
                                     renegotiation = {Renegotiation, _},
                                     stapling_state = StaplingState},
		  ssl_options = SslOpts,
		  connection_states = ConnectionStates,
                  protocol_specific = PS} = State0) ->
    #{current_cookie_secret := Cookie} = PS,
    Hello = dtls_handshake:client_hello(Host, Port, Cookie, ConnectionStates,
					SslOpts,<<>>, Renegotiation, maps:get(ocsp_nonce, StaplingState, undefined)),
    {State, Actions} = dtls_gen_connection:send_handshake(Hello, State0),
    dtls_gen_connection:next_event(hello, no_record, State, Actions);
connection(internal, renegotiate, State0) ->
    State = #state{handshake_env = HsEnv} = dtls_gen_connection:reinit_handshake_data(State0),
    {next_state, ?STATE(connection),
     State#state{handshake_env = HsEnv#handshake_env{renegotiation = {true, internal}}},
     [{next_event, internal, #hello_request{}}]};
connection({call, From}, renegotiate, State0) ->
    State = #state{handshake_env = HsEnv} = dtls_gen_connection:reinit_handshake_data(State0),
    {next_state, ?STATE(connection), State#state{handshake_env = HsEnv#handshake_env{
                                                                   renegotiation = {true, From}}},
     [{next_event, internal, #hello_request{}}]};
connection({call, From}, {application_data, Data}, State) ->
    try
        dtls_gen_connection:send_application_data(Data, From, ?STATE(connection), State)
    catch throw:Error ->
            ssl_gen_statem:hibernate_after(?STATE(connection), State, [{reply, From, Error}])
    end;
connection({call, From}, {downgrade, Pid},
           #state{connection_env = CEnv,
                  static_env = #static_env{transport_cb = Transport,
                                           socket = {_Server, Socket} = DTLSSocket}} = State) ->
    %% For testing purposes, downgrades without noticing the server
    dtls_socket:setopts(Transport, Socket, [{active, false}, {packet, 0}, {mode, binary}]),
    Transport:controlling_process(Socket, Pid),
    {stop_and_reply, {shutdown, normal}, {reply, From, {ok, DTLSSocket}},
     State#state{connection_env = CEnv#connection_env{socket_terminated = true}}};
connection(info, Event, State) ->
    dtls_gen_connection:gen_info(Event, ?STATE(connection), State);
connection(Type, Event, State) ->
    gen_state(?STATE(connection), Type, Event, State).

%%--------------------------------------------------------------------
-spec downgrade(gen_statem:event_type(), term(), #state{}) ->
		       gen_statem:state_function_result().
%%--------------------------------------------------------------------
downgrade(enter, _, State) ->
    {keep_state, State};
downgrade(info, Event, State) ->
    dtls_gen_connection:gen_info(Event, ?STATE(downgrade), State);
downgrade(Type, Event, State) ->
    gen_state(?STATE(downgrade), Type, Event, State).

%%--------------------------------------------------------------------
%% gen_statem callbacks
%%--------------------------------------------------------------------
callback_mode() ->
    [state_functions, state_enter].

terminate(Reason, StateName, State) ->
    ssl_gen_statem:terminate(Reason, StateName, State).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

format_status(Type, Data) ->
    ssl_gen_statem:format_status(Type, Data).

gen_state(StateName, Type, Event, State) ->
    try tls_dtls_client_connection:StateName(Type, Event, State)
    catch
        throw:#alert{}=Alert ->
            dtls_gen_connection:alert_or_reset_connection(Alert, StateName, State);
        error:Reason:ST ->
            ?SSL_LOG(info, handshake_error, [{error, Reason}, {stacktrace, ST}]),
            AlertInfo = case StateName of
                          connection ->
                              malformed_data;
                          _ ->
                              malformed_handshake_data
                      end,
            Alert = ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, AlertInfo),
            dtls_gen_connection:alert_or_reset_connection(Alert, StateName, State)
    end.

%%--------------------------------------------------------------------
%% Tracing
%%--------------------------------------------------------------------
handle_trace(hbn,
             {call, {?MODULE, connection,
                     [_Type = info, Event, _State]}},
             Stack) ->
    {io_lib:format("Type = info Event = ~W ", [Event, 10]), Stack}.
