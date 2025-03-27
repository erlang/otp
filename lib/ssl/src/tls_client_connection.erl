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
%%
%%----------------------------------------------------------------------
%% Purpose: TLS-1.0-TLS-1.2 FSM (* = optional)
%% %%----------------------------------------------------------------------
%%                    TLS Handshake protocol full Handshake
%%  Client                                               Server
%%
%%       ClientHello                  -------->                       Flight 1
%%                                                       ServerHello  \
%%                                                      Certificate*   \
%%                                                ServerKeyExchange*    Flight 2
%%                                               CertificateRequest*   /
%%                                    <--------      ServerHelloDone  /
%%       Certificate*                                                 \
%%       ClientKeyExchange                                             \
%%       CertificateVerify*                                             Flight 3 part 1
%%       [ChangeCipherSpec]                                            /
%%       NextProtocol*
%%       Finished                     -------->                       / Flight 3 part 2
%%                                                [ChangeCipherSpec]
%%                                    <--------             Finished Flight 4
%%       Application Data             <------->     Application Data
%%
%%
%%                    TLS Handshake protocol abbreviated Handshake
%%    Client                                                Server
%%
%%       ClientHello                   -------->                       Abbrev Flight 1
%%                                                        ServerHello  Abbrev Flight 2 part 1
%%                                                 [ChangeCipherSpec]
%%                                     <--------             Finished  Abbrev Flight 2 part 2
%%       [ChangeCipherSpec]
%%       NextProtocol*
%%       Finished                      -------->                       Abbrev Flight 3
%%       Application Data              <------->     Application Data
%%
%%
%%
%%                                       Start FSM    ---> CONFIG_ERROR
%%                                                     Send error to user
%%                                          |          and shutdown
%%                                          |
%%                                          V
%%                                    INITIAL_HELLO
%%
%%                                          | Send/Recv Flight 1
%%                                          |
%%                                          |
%%           USER_HELLO                     |
%%  <- Possibly let user provide            V
%%  options after looking at hello ex ->    HELLO
%%                                             | Send/Recv Flight 2 or Abbrev Flight 1 -
%%                                             | Abbrev Flight 2 part 1
%%                                             |
%%                                New session  | Resumed session
%%       WAIT_STAPLING   CERTIFY  <----------------------------------> ABBREVIATED
%%       WAIT_CERT_VERIFY
%%  <- Possibly Receive  --  |                                              |
%%     OCSP Staple ->        |  Flight 3 part 1                             |
%%                           |                                              |
%%                           V                                              |  Abbrev Flight 2
%%                                                                          | part 2 to Abbrev
%%                                                                          | Flight 3
%%                         CIPHER                                           |
%%                           |                                              |
%%                           |                                              |
%%                           | Fligth 3 part 2 to Flight 4                  |
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

-module(tls_client_connection).
-moduledoc false.

-behaviour(gen_statem).

-include("tls_connection.hrl").
-include("tls_handshake.hrl").
-include("ssl_alert.hrl").
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

init([Role, Sender, Tab, Host, Port, Socket, Options,  User, CbInfo]) ->
    State0 = tls_dtls_gen_connection:initial_state(Role, Sender, Tab, Host, Port,
                                                   Socket, Options, User, CbInfo),
    #state{static_env = #static_env{user_socket = UserSocket}} = State0,
    User ! {self(), user_socket, UserSocket},
    put(tls_role, client),
    try
        State1 = #state{static_env = #static_env{session_cache = Cache,
                                                 session_cache_cb = CacheCb
                                                },
                        connection_env = #connection_env{cert_key_alts = CertKeyAlts},
                        ssl_options = SslOptions,
                        session = Session0} =
            ssl_gen_statem:init_ssl_config(State0#state.ssl_options, Role, State0),
        CertKeyPairs = ssl_certificate:available_cert_key_pairs(CertKeyAlts),
        Session = ssl_session:client_select_session({Host, Port, SslOptions}, Cache,
                                                    CacheCb, Session0, CertKeyPairs),
        State = State1#state{session = Session},
        tls_gen_connection:initialize_tls_sender(State),
        gen_statem:enter_loop(?MODULE, [], initial_hello, State)
    catch throw:Error ->
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
initial_hello({call, From}, {start, Timeout},
              #state{static_env = #static_env{host = Host,
                                              port = Port,
                                              cert_db = CertDbHandle,
                                              cert_db_ref = CertDbRef,
                                              protocol_cb = Connection},
                     handshake_env = #handshake_env{renegotiation = {Renegotiation, _},
                                                    stapling_state = StaplingState0},
                     connection_env = CEnv,
                     ssl_options = #{%% Use highest version in initial ClientHello.
                                     %% Versions is a descending list of supported versions.
                                     versions := [HelloVersion|_] = Versions,
                                     session_tickets := SessionTickets,
                                     early_data := EarlyData} = SslOpts,
                     session = Session,
                     connection_states = ConnectionStates0,
                     recv = Recv0
                    } = State0) ->

    KeyShare = tls_client_connection_1_3:maybe_generate_client_shares(SslOpts),
    %% Update UseTicket in case of automatic session resumption. The automatic ticket handling
    %% also takes it into account if the ticket is suitable for sending early data not exceeding
    %% the max_early_data_size or if it can only be used for session resumption.
    {UseTicket, State1} = tls_client_connection_1_3:maybe_automatic_session_resumption(State0),
    TicketData = tls_handshake_1_3:get_ticket_data(self(), SessionTickets, UseTicket),
    OcspNonce = tls_handshake:ocsp_nonce(SslOpts),
    Hello0 = tls_handshake:client_hello(Host, Port, ConnectionStates0, SslOpts,
                                        Session#session.session_id,
                                        Renegotiation,
                                        KeyShare,
                                        TicketData,
                                        OcspNonce,
                                        CertDbHandle,
                                        CertDbRef
                                       ),

    %% Early Data Indication
    Hello1 = tls_handshake_1_3:maybe_add_early_data_indication(Hello0,
                                                               EarlyData,
                                                               HelloVersion),

    %% Update pre_shared_key extension with binders (TLS 1.3)
    Hello2 = tls_handshake_1_3:maybe_add_binders(Hello1, TicketData, HelloVersion),

    MaxFragEnum = maps:get(max_frag_enum, Hello1#client_hello.extensions, undefined),
    ConnectionStates1 = ssl_record:set_max_fragment_length(MaxFragEnum, ConnectionStates0),
    State2 = State1#state{connection_states = ConnectionStates1,
                          connection_env = CEnv#connection_env{negotiated_version = HelloVersion}},

    State3 = Connection:queue_handshake(Hello2, State2),

    %% RequestedVersion is used as the legacy record protocol version and shall be
    %% {3,3} in case of TLS 1.2 and higher. In all other cases it defaults to the
    %% lowest supported protocol version.
    %%
    %% negotiated_version is also used by the TLS 1.3 state machine and is set after
    %% ServerHello is processed.
    RequestedVersion = tls_record:hello_version(Versions),

    {Ref,Maybe} = tls_gen_connection_1_3:do_maybe(),
    try
        %% Send Early Data
        State4 = Maybe(tls_client_connection_1_3:maybe_send_early_data(State3)),

        {#state{handshake_env = HsEnv1} = State5, _} =
            Connection:send_handshake_flight(State4),

        StaplingKeyPresent = maps:is_key(stapling, SslOpts),
        State = State5#state{
                  connection_env = CEnv#connection_env{
                                     negotiated_version = RequestedVersion},
                  session = Session,
                  handshake_env =
                      HsEnv1#handshake_env{
                        key_share = KeyShare,
                        stapling_state =
                            StaplingState0#{ocsp_nonce => OcspNonce,
                                            configured => StaplingKeyPresent}},
                  recv = State5#state.recv#recv{from = From}
                 },
        NextState = next_statem_state(Versions),
        Connection:next_event(NextState, no_record, State,
                              [{{timeout, handshake}, Timeout, close}])
    catch
        {Ref, #alert{} = Alert} ->
            NewState = State0#state{recv = Recv0#recv{from = From}},
            ssl_gen_statem:handle_own_alert(Alert, init, NewState)
    end;
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
initial_hello(Event, Msg, State) ->
    ssl_gen_statem:initial_hello(Event, Msg, State).

%%--------------------------------------------------------------------
-spec config_error(gen_statem:event_type(),
                   {start, timeout()} | term(), #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
config_error(Type, Event, State) ->
    ssl_gen_statem:config_error(Type, Event, State).

%%--------------------------------------------------------------------
-spec hello(gen_statem:event_type(),
	    #hello_request{} | #server_hello{} | term(),
	    #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
hello(internal, #server_hello{extensions = Extensions},
      #state{handshake_env = #handshake_env{continue_status = pause},
             recv = #recv{from = From}=Recv} = State) ->
    {next_state, user_hello,
     State#state{recv = Recv#recv{from = undefined}},
     [{postpone, true},{reply, From, {ok, Extensions}}]};
hello(internal, #server_hello{} = Hello,
      #state{connection_states = ConnectionStates0,
             connection_env = CEnv,
             handshake_env = #handshake_env{
                                stapling_state = StaplingState0,
                                renegotiation = {Renegotiation, _}} = HsEnv,
             session = #session{session_id = OldId},
	     ssl_options = SslOptions} = State) ->
    try
        case tls_handshake:hello(Hello, SslOptions, ConnectionStates0, Renegotiation, OldId) of
            %% Legacy TLS 1.2 and older
            {Version, NewId, ConnectionStates, ProtoExt, Protocol, StaplingState} ->
                tls_dtls_client_connection:handle_session(
                  Hello, Version, NewId, ConnectionStates, ProtoExt, Protocol,
                  State#state{
                    handshake_env =
                        HsEnv#handshake_env{
                          stapling_state = maps:merge(StaplingState0,StaplingState)}});
            %% TLS 1.3
            {next_state, wait_sh, SelectedVersion, StaplingState} ->
                %% Continue in TLS 1.3 'wait_sh' state
                {next_state, wait_sh,
                 State#state{handshake_env =
                                 HsEnv#handshake_env{stapling_state =
                                                         maps:merge(StaplingState0, StaplingState)},
                             connection_env =
                                 CEnv#connection_env{negotiated_version = SelectedVersion}},
                 [{change_callback_module, tls_client_connection_1_3},
                  {next_event, internal, Hello}]}
        end
    catch throw:#alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, ?STATE(hello), State)
    end;
hello(info, Event, State) ->
    tls_gen_connection:gen_info(Event, ?STATE(hello), State);
hello(Type, Event, State) ->
    gen_state(?STATE(hello), Type, Event, State).

%%--------------------------------------------------------------------
-spec user_hello(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
user_hello(Type, Event, State) ->
    gen_state(?STATE(user_hello), Type, Event, State).

%%--------------------------------------------------------------------
-spec abbreviated(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
abbreviated(info, Event, State) ->
    tls_gen_connection:gen_info(Event, ?STATE(abbreviated), State);
abbreviated(Type, Event, State) ->
    gen_state(?STATE(abbreviated), Type, Event, State).

%%--------------------------------------------------------------------
-spec wait_stapling(gen_statem:event_type(), term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_stapling(info, Event, State) ->
    tls_gen_connection:gen_info(Event, ?STATE(wait_stapling), State);
wait_stapling(Type, Event, State) ->
    gen_state(?STATE(wait_stapling), Type, Event, State).

%%--------------------------------------------------------------------
-spec certify(gen_statem:event_type(), term(), #state{}) ->
		     gen_statem:state_function_result().
%%--------------------------------------------------------------------
certify(info, Event, State) ->
    tls_gen_connection:gen_info(Event, ?STATE(certify), State);
certify(Type, Event, State) ->
    gen_state(?STATE(certify), Type, Event, State).

%%--------------------------------------------------------------------
-spec cipher(gen_statem:event_type(), term(), #state{}) ->
		    gen_statem:state_function_result().
%%--------------------------------------------------------------------
cipher(info, Event, State) ->
    tls_gen_connection:gen_info(Event, ?STATE(cipher), State);
cipher(Type, Event, State) ->
    gen_state(?STATE(cipher), Type, Event, State).

%%--------------------------------------------------------------------
-spec connection(gen_statem:event_type(),
		 #hello_request{} | #client_hello{}| term(), #state{}) ->
			gen_statem:state_function_result().
%%--------------------------------------------------------------------
connection(info, Event, State) ->
    tls_gen_connection:gen_info(Event, ?STATE(connection), State);
connection({call, From}, {user_renegotiate, WriteState},
           #state{connection_states = ConnectionStates} = State) ->
    {next_state, ?STATE(connection),
     State#state{connection_states = ConnectionStates#{current_write => WriteState}},
     [{next_event,{call, From}, renegotiate}]};
connection(internal, #hello_request{},
	   #state{static_env = #static_env{host = Host,
                                           port = Port,
                                           cert_db = CertDbHandle,
                                           cert_db_ref = CertDbRef,
                                           session_cache = Cache,
                                           session_cache_cb = CacheCb},
                  handshake_env = #handshake_env{
                                     renegotiation = {Renegotiation, peer},
                                     stapling_state = StaplingState},
                  connection_env = #connection_env{cert_key_alts = CertKeyAlts},
		  session = Session0,
		  ssl_options = SslOpts,
                  protocol_specific = #{sender := Pid},
		  connection_states = ConnectionStates} = State0) ->
    try tls_sender:peer_renegotiate(Pid) of
        {ok, Write} ->
            CertKeyPairs = ssl_certificate:available_cert_key_pairs(CertKeyAlts),
            Session = ssl_session:client_select_session({Host, Port, SslOpts}, Cache,
                                                        CacheCb, Session0, CertKeyPairs),
            Hello = tls_handshake:client_hello(Host, Port, ConnectionStates, SslOpts,
                                               Session#session.session_id,
                                               Renegotiation, undefined,
                                               undefined, maps:get(ocsp_nonce, StaplingState, undefined),
                                               CertDbHandle, CertDbRef),
            {State, Actions} =
                tls_gen_connection:send_handshake(Hello,
                                                  State0#state{connection_states =
                                                                   ConnectionStates#{current_write
                                                                                     => Write},
                                                               session = Session}),
            tls_gen_connection:next_event(hello, no_record, State, Actions)
        catch
            _:Reason:ST ->
                ?SSL_LOG(info, internal_error, [{error, Reason}, {stacktrace, ST}]),
                {stop, {shutdown, sender_blocked}, State0}
        end;
connection(internal, #hello_request{},
	   #state{static_env = #static_env{host = Host,
                                           port = Port,
                                           cert_db = CertDbHandle,
                                           cert_db_ref = CertDbRef
                                          },
                  handshake_env = #handshake_env{
                                     renegotiation = {Renegotiation, _},
                                     stapling_state = StaplingState},
		  ssl_options = SslOpts,
		  connection_states = ConnectionStates} = State0) ->
    Hello = tls_handshake:client_hello(Host, Port, ConnectionStates, SslOpts,
                                       <<>>, Renegotiation, undefined,
                                       undefined, maps:get(ocsp_nonce, StaplingState, undefined),
                                       CertDbHandle, CertDbRef),

    {State, Actions} = tls_gen_connection:send_handshake(Hello, State0),
    tls_gen_connection:next_event(hello, no_record, State, Actions);

connection(Type, Event, State) ->
    gen_state(?STATE(connection), Type, Event, State).

%%--------------------------------------------------------------------
-spec downgrade(gen_statem:event_type(), term(), #state{}) ->
		       gen_statem:state_function_result().
%%--------------------------------------------------------------------
downgrade(Type, Event, State) ->
     ssl_gen_statem:downgrade(Type, Event, State).

%--------------------------------------------------------------------
%% gen_statem callbacks
%%--------------------------------------------------------------------
callback_mode() ->
    state_functions.

terminate(Reason, StateName, State) ->
    ssl_gen_statem:terminate(Reason, StateName, State).

format_status(Type, Data) ->
    ssl_gen_statem:format_status(Type, Data).

code_change(_OldVsn, StateName, State, _) ->
    {ok, StateName, State}.


%%====================================================================
%% Internal functions
%%====================================================================
gen_state(StateName, Type, Event, State) ->
    try tls_dtls_client_connection:StateName(Type, Event, State)
    catch throw:#alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, StateName, State);
          _:Reason:ST ->
            ?SSL_LOG(info, unexpected_error, [{error, Reason}, {stacktrace, ST}]),
	    ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?INTERNAL_ERROR,
						       unexpected_error),
					    StateName, State)
    end.

next_statem_state([Version]) ->
    case ssl:tls_version(Version) of
        ?TLS_1_3 ->
            wait_sh;
        _  ->
            hello
    end;
next_statem_state(_) ->
    hello.

%%====================================================================
%% Tracing
%%====================================================================
handle_trace(crt,
             {call, {?MODULE, init,
                     [[_Role, _Sender, _Host, _Port, _Socket, _Options, _User, _CbInfo]]}},
             Stack) ->
    {io_lib:format("Host = ~s Port = ~p User = ~p", [_Host, _Port, _User]), Stack};
handle_trace(hbn,
             {call, {?MODULE, connection,
                     [_Type = info, Event, _State]}},
             Stack) ->
    {io_lib:format("Type = info Event = ~W ", [Event, 10]), Stack};
handle_trace(csp,
             {call, {?MODULE, wait_stapling,
                     [Type, Event|_]}}, Stack) ->
    {io_lib:format("Type = ~w Event = ~W", [Type, Event, 10]), Stack}.
