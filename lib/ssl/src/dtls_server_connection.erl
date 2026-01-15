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

-module(dtls_server_connection).
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
%%  WAIT_CERT_VERIFY   CERTIFY  <----------------------------------> ABBREVIATED
%%
%%  <- Possibly Receive  --  |                                              |
%%     CertVerify ------>    | Send/ Recv Flight 5                          |
%%                           |                                              |
%%                           |                                              |  Send / Recv Abbrev
%%                           V                                              |  Flight part 2
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
         certify/3,
         wait_cert_verify/3,
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
              #state{protocol_specific = PS0, recv = Recv} = State) ->
    PS = PS0#{current_cookie_secret => dtls_v1:cookie_secret(),
              previous_cookie_secret => <<>>},
    erlang:send_after(dtls_v1:cookie_timeout(), self(), new_cookie_secret),
    dtls_gen_connection:next_event(hello, no_record,
                                   State#state{recv = Recv#recv{from = From},
                                               protocol_specific = PS},
                                   [{{timeout, handshake}, Timeout, close}]);
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
initial_hello(Type, Event, State) ->
    tls_dtls_server_connection:initial_hello(Type, Event, State).

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
hello(enter, _, State) ->
    {keep_state, State};
hello(internal, #client_hello{cookie = <<>>,
			      client_version = Version} = Hello,
      #state{static_env = #static_env{transport_cb = Transport,
                                      socket = Socket},
             connection_env = CEnv,
             protocol_specific = #{current_cookie_secret := Secret}} = State0) ->
    try tls_dtls_server_connection:handle_sni_extension(State0, Hello) of
        #state{} = State1 ->
            {ok, {IP, Port}} = dtls_socket:peername(Transport, Socket),
            Cookie = dtls_handshake:cookie(Secret, IP, Port, Hello),
            %% FROM RFC 6347 regarding HelloVerifyRequest message:
            %% The server_version field has the same syntax as in TLS.  However, in
            %% order to avoid the requirement to do version negotiation in the
            %% initial handshake, DTLS 1.2 server implementations SHOULD use DTLS
            %% version 1.0 regardless of the version of TLS that is expected to be
            %% negotiated.
            VerifyRequest =
                dtls_handshake:hello_verify_request(Cookie, ?HELLO_VERIFY_REQUEST_VERSION),
            State2 = dtls_gen_connection:prepare_flight(
                       State1#state{connection_env =
                                        CEnv#connection_env{negotiated_version = Version}}),
            {State, Actions} = dtls_gen_connection:send_handshake(VerifyRequest, State2),
            #state{handshake_env = HsEnv} = State,
            NewHSEnv = HsEnv#handshake_env{tls_handshake_history =
                                               ssl_handshake:init_handshake_history()},
            dtls_gen_connection:next_event(hello, no_record,
                                           State#state{handshake_env = NewHSEnv}, Actions)
    catch throw:#alert{} = Alert ->
            dtls_gen_connection:alert_or_reset_connection(Alert, hello, State0)
    end;
hello(internal, #client_hello{extensions = Extensions} = Hello,
      #state{handshake_env = #handshake_env{continue_status = pause},
             recv = #recv{from = From}} = State0) ->
    try tls_dtls_server_connection:handle_sni_extension(State0, Hello) of
        #state{recv = Recv} = State ->
            {next_state, user_hello, State#state{recv = Recv#recv{from = undefined}},
             [{postpone, true}, {reply, From, {ok, Extensions}}]}
    catch throw:#alert{} = Alert ->
            dtls_gen_connection:alert_or_reset_connection(Alert, hello, State0)
    end;
hello(internal, #client_hello{cookie = Cookie} = Hello,
      #state{static_env = #static_env{transport_cb = Transport,
                                      socket = Socket},
             protocol_specific = #{current_cookie_secret := Secret,
                                   previous_cookie_secret := PSecret}
            } = State) ->
    {ok, {IP, Port}} = dtls_socket:peername(Transport, Socket),
    case dtls_handshake:cookie(Secret, IP, Port, Hello) of
	Cookie ->
	    handle_client_hello(Hello, State);
	_ ->
            case dtls_handshake:cookie(PSecret, IP, Port, Hello) of
		Cookie ->
                    handle_client_hello(Hello, State);
                _ ->
                    %% Handle bad cookie as new cookie request RFC 6347 4.1.2
                    hello(internal, Hello#client_hello{cookie = <<>>}, State)
            end
    end;
hello(internal, {handshake, {#client_hello{cookie = <<>>} = Handshake, _}}, State) ->
    %% Initial hello should not be in handshake history
    {next_state, ?STATE(hello), State, [{next_event, internal, Handshake}]};
hello(internal,  #change_cipher_spec{type = <<1>>}, State0) ->
    Epoch = dtls_gen_connection:retransmit_epoch(?STATE(hello), State0),
    {State1, Actions0} =
        dtls_gen_connection:send_handshake_flight(State0, Epoch),
    %% This will reset the retransmission timer by repeating the enter state event
    case dtls_gen_connection:next_event(?STATE(hello), no_record, State1, Actions0) of
        {next_state, ?STATE(hello), State, Actions} ->
            {repeat_state, State, Actions};
        {next_state, ?STATE(hello), State} ->
            {repeat_state, State};
        {stop, _, _} = Stop ->
            Stop
    end;

hello(info, Event, State) ->
    dtls_gen_connection:gen_info(Event, ?STATE(hello), State);
hello(state_timeout, Event, State) ->
    dtls_gen_connection:handle_state_timeout(Event, ?STATE(hello), State);
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
abbreviated(info, Event, State) ->
    dtls_gen_connection:gen_info(Event, ?STATE(abbreviated), State);
abbreviated(state_timeout, Event, State) ->
    dtls_gen_connection:handle_state_timeout(Event, ?STATE(abbreviated), State);
abbreviated(internal = Type, #change_cipher_spec{} = Event,
            #state{connection_states = ConnectionStates0} = State) ->
    ConnectionStates1 = dtls_record:save_current_connection_state(ConnectionStates0, read),
    ConnectionStates = dtls_record:next_epoch(ConnectionStates1, read),
    gen_state(?STATE(abbreviated), Type, Event,
              State#state{connection_states = ConnectionStates});
abbreviated(Type, Event, State) ->
    gen_state(?STATE(abbreviated), Type, Event, State).

%%--------------------------------------------------------------------
-spec certify(gen_statem:event_type(), term(), #state{}) ->
		     gen_statem:state_function_result().
%%--------------------------------------------------------------------
certify(enter, _, State0) ->
    {State, Actions} = dtls_gen_connection:handle_flight_timer(State0),
    {keep_state, State, Actions};
certify(internal,  #change_cipher_spec{type = <<1>>}, State0) ->
    Epoch = dtls_gen_connection:retransmit_epoch(?STATE(certify), State0),
    {State1, Actions0} = dtls_gen_connection:send_handshake_flight(State0, Epoch),
    %% This will reset the retransmission timer by repeating the enter state event
    case dtls_gen_connection:next_event(?STATE(certify), no_record, State1, Actions0) of
        {next_state, ?STATE(certify), State, Actions} ->
            dtls_gen_connection:next_event(?STATE(certify), no_record, State1, Actions0),
            {repeat_state, State, Actions};
        {next_state, ?STATE(certify), State} ->
            {repeat_state, State, Actions0};
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
-spec wait_cert_verify(gen_statem:event_type(), term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_cert_verify(enter, _Event, State0) ->
    {State, Actions} = dtls_gen_connection:handle_flight_timer(State0),
    {keep_state, State, Actions};
wait_cert_verify(state_timeout, Event, State) ->
    dtls_gen_connection:handle_state_timeout(Event, ?STATE(wait_cert_verify), State);
wait_cert_verify(info, Event, State) ->
    dtls_gen_connection:gen_info(Event, ?STATE(wait_cert_verify), State);
wait_cert_verify(Type, Event, State) ->
    gen_state(?STATE(wait_cert_verify), Type, Event, State).

%%--------------------------------------------------------------------
-spec cipher(gen_statem:event_type(), term(), #state{}) ->
		    gen_statem:state_function_result().
%%--------------------------------------------------------------------
cipher(enter, _, State0) ->
    {State, Actions} = dtls_gen_connection:handle_flight_timer(State0),
    {keep_state, State, Actions};
cipher(state_timeout, Event, State) ->
    dtls_gen_connection:handle_state_timeout(Event, ?STATE(cipher), State);
cipher(info, Event, State) ->
    dtls_gen_connection:gen_info(Event, ?STATE(cipher), State);
cipher(internal = Type, #change_cipher_spec{type = <<1>>} = Event,
       #state{connection_states = ConnectionStates0} = State) ->
    ConnectionStates1 = dtls_record:save_current_connection_state(ConnectionStates0, read),
    ConnectionStates = dtls_record:next_epoch(ConnectionStates1, read),
    gen_state(?STATE(cipher), Type, Event, State#state{connection_states = ConnectionStates});
cipher(internal = Type, #finished{} = Event, #state{connection_states = ConnectionStates,
                                                    protocol_specific = PS} = State) ->
    gen_state(?STATE(cipher), Type, Event,
              dtls_gen_connection:prepare_flight(
                State#state{connection_states = ConnectionStates,
                            protocol_specific =
                                PS#{flight_state => connection}}));
cipher(Type, Event, State) ->
    gen_state(?STATE(cipher), Type, Event, State).

%%--------------------------------------------------------------------
-spec connection(gen_statem:event_type(),
		 #hello_request{} | #client_hello{}| term(), #state{}) ->
			gen_statem:state_function_result().
%%--------------------------------------------------------------------
connection(enter, _, #state{connection_states = Cs0,
                            static_env = Env} = State0) ->
    State = case Env of
                #static_env{socket = {Listener, {Client, _}}} ->
                    dtls_packet_demux:connection_setup(Listener, Client),
                    case maps:is_key(previous_cs, Cs0) of
                        false ->
                            State0;
                        true ->
                            Cs = maps:remove(previous_cs, Cs0),
                            State0#state{connection_states = Cs}
                    end;
                _ -> %% client
                    State0
            end,
    {keep_state, State};
connection(internal, #client_hello{} = Hello,
           #state{handshake_env = #handshake_env{allow_renegotiate = true} = HsEnv} = State) ->
    %% Mitigate Computational DoS attack
    %% http://www.educatedguesswork.org/2011/10/ssltls_and_computational_dos.html
    %% http://www.thc.org/thc-ssl-dos/ Rather than disabling client
    %% initiated renegotiation we will disallow many client initiated
    %% renegotiations immediately after each other.
    erlang:send_after(?WAIT_TO_ALLOW_RENEGOTIATION, self(), allow_renegotiate),
    {next_state, hello,
     State#state{handshake_env = HsEnv#handshake_env{renegotiation = {true, peer},
                                                     allow_renegotiate = false}},
     [{next_event, internal, Hello}]};
connection(internal, #client_hello{},
           #state{static_env = #static_env{protocol_cb = Connection},
                  handshake_env = #handshake_env{allow_renegotiate = false}} = State0) ->
    Alert = ?ALERT_REC(?WARNING, ?NO_RENEGOTIATION),
    State1 = dtls_gen_connection:send_alert(Alert, State0),
    {Record, State} = ssl_gen_statem:prepare_connection(State1, Connection),
    dtls_gen_connection:next_event(?STATE(connection), Record, State);
connection(internal, new_connection, #state{ssl_options=SSLOptions,
                                            handshake_env = HsEnv,
                                            static_env =
                                                #static_env{socket = {Listener, {Client, _}}},
                                            connection_states = OldCs} = State) ->
    case maps:get(previous_cs, OldCs, undefined) of
        undefined ->
            case dtls_packet_demux:new_connection(Listener, Client) of
                true ->
                    {keep_state, State};
                false ->
                    BeastMitigation = maps:get(beast_mitigation, SSLOptions, disabled),
                    ConnectionStates0 = dtls_record:init_connection_states(server, BeastMitigation),
                    ConnectionStates = ConnectionStates0#{previous_cs => OldCs},
                    {next_state, hello,
                     State#state{handshake_env =
                                     HsEnv#handshake_env{renegotiation = {false, first}},
                                 connection_states = ConnectionStates}}
            end;
        _ ->
            %% Someone spamming new_connection, just drop them
            {keep_state, State}
    end;
connection(internal, renegotiate, State0) ->
    {#state{handshake_env = HsEnv} = State, Actions} = renegotiate(State0),
    dtls_gen_connection:next_event(hello, no_record,  
                                   State#state{handshake_env = HsEnv#handshake_env{renegotiation = {true, internal}}},
                                   Actions);
connection({call, From}, renegotiate, State0) ->
    {#state{handshake_env = HsEnv}  = State, Actions} = renegotiate(State0),
    dtls_gen_connection:next_event(hello, no_record, 
                                   State#state{handshake_env = HsEnv#handshake_env{renegotiation = {true, From}}}, 
                                   Actions);
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

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
handle_client_hello(#client_hello{client_version = ClientVersion} = Hello, State0) ->
    try
        #state{connection_states = ConnectionStates0,
               static_env = #static_env{trackers = Trackers},
               handshake_env = #handshake_env{kex_algorithm = KeyExAlg,
                                              renegotiation = {Renegotiation, _},
                                              negotiated_protocol = CurrentProtocol} = HsEnv,
               connection_env = #connection_env{cert_key_alts = CertKeyAlts} = CEnv,
               session = Session0,
               ssl_options = SslOpts} =
            tls_dtls_server_connection:handle_sni_extension(State0, Hello),
        SessionTracker = proplists:get_value(session_id_tracker, Trackers),
        {Version, {Type, Session}, ConnectionStates, Protocol0, ServerHelloExt, HashSign} =
            dtls_handshake:hello(Hello, SslOpts, {SessionTracker, Session0,
                                                  ConnectionStates0, CertKeyAlts, KeyExAlg},
                                 Renegotiation),
        Protocol = case Protocol0 of
                       undefined -> CurrentProtocol;
                       _ -> Protocol0
                   end,

        State =
            dtls_gen_connection:prepare_flight(
              State0#state{connection_states =
                               ConnectionStates,
                           connection_env = CEnv#connection_env{negotiated_version = Version},
                           handshake_env = HsEnv#handshake_env{
                                             hashsign_algorithm = HashSign,
                                             client_hello_version = ClientVersion,
                                             negotiated_protocol = Protocol},
                           session = Session}),
        {next_state, hello, State,
         [{next_event, internal, {common_client_hello, Type, ServerHelloExt}}]}
    catch #alert{} = Alert ->
            dtls_gen_connection:alert_or_reset_connection(Alert, hello, State0)
    end.

gen_state(StateName, Type, Event, State) ->
    try tls_dtls_server_connection:StateName(Type, Event, State)
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

renegotiate(State0) ->
    HelloRequest = ssl_handshake:hello_request(),
    State1 = dtls_gen_connection:prepare_flight(State0),
    dtls_gen_connection:send_handshake(HelloRequest, State1).

%%--------------------------------------------------------------------
%% Tracing
%%--------------------------------------------------------------------
handle_trace(hbn,
             {call, {?MODULE, connection,
                     [_Type = info, Event, _State]}},
             Stack) ->
    {io_lib:format("Type = info Event = ~W ", [Event, 10]), Stack}.
