%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2023. All Rights Reserved.
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
%%  WAIT_OCSP_STAPLING   CERTIFY  <----------------------------------> ABBREVIATED
%%     
%%  <- Possibly Receive  --  |                                              |
%%     OCSP Stapel ------>   | Send/ Recv Flight 5                          |
%%                           |                                              |
%%                           V                                              |  Send / Recv Abbrev Flight part 2 
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

-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/logger.hrl").

-include("dtls_connection.hrl").
-include("dtls_handshake.hrl").
-include("ssl_alert.hrl").
-include("dtls_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_api.hrl").
-include("ssl_internal.hrl").
-include("ssl_srp.hrl").

%% Internal application API

%% Setup
-export([init/1]).

-export([renegotiate/2]).

-export([alert_or_reset_connection/3]).  %% Code re-use from dtls_gen_connection.

%% gen_statem state functions
-export([initial_hello/3,
         config_error/3,
         downgrade/3,
	 hello/3,
         user_hello/3,
         wait_ocsp_stapling/3,
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
init([Role, Host, Port, Socket, Options,  User, CbInfo]) ->
    process_flag(trap_exit, true),
    State0 = initial_state(Role, Host, Port, Socket, Options, User, CbInfo),
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
%%====================================================================
%% Handshake
%%====================================================================
renegotiate(#state{static_env = #static_env{role = client}} = State0, Actions) ->
    %% Handle same way as if server requested
    %% the renegotiation
    State = dtls_gen_connection:reinit_handshake_data(State0),
    {next_state, connection, State,
     [{next_event, internal, #hello_request{}} | Actions]};

renegotiate(#state{static_env = #static_env{role = server}} = State0, Actions) ->
    HelloRequest = ssl_handshake:hello_request(),
    State1 = prepare_flight(State0),
    {State, MoreActions} = dtls_gen_connection:send_handshake(HelloRequest, State1),
    dtls_gen_connection:next_event(hello, no_record, State, Actions ++ MoreActions).

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
                                     role = client,
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
    Session = ssl_session:client_select_session({Host, Port, SslOpts}, Cache, CacheCb, Session0, CertKeyPairs),
    Hello = dtls_handshake:client_hello(Host, Port, ConnectionStates0, SslOpts,
					Session#session.session_id, Renegotiation),

    MaxFragEnum = maps:get(max_frag_enum, Hello#client_hello.extensions, undefined),
    ConnectionStates1 = ssl_record:set_max_fragment_length(MaxFragEnum, ConnectionStates0),
    Version = Hello#client_hello.client_version,
    HelloVersion = dtls_record:hello_version(Version, Versions),
    State1 = prepare_flight(State0#state{connection_env = CEnv#connection_env{negotiated_version = Version},
                                         connection_states = ConnectionStates1}),
    {State2, Actions} = 
        dtls_gen_connection:send_handshake(Hello, 
                                           State1#state{connection_env = 
                                                            CEnv#connection_env{negotiated_version = HelloVersion}}),  
    State = State2#state{connection_env = CEnv#connection_env{negotiated_version = Version}, %% RequestedVersion
                         session = Session,
                         start_or_recv_from = From,
                         protocol_specific = PS#{active_n_toggle := false}
                        },
    dtls_gen_connection:next_event(hello, no_record, State, [{{timeout, handshake}, Timeout, close} | Actions]);
initial_hello({call, _} = Type, Event, #state{static_env = #static_env{role = server},
                                              protocol_specific = PS0} = State) ->
    PS = PS0#{current_cookie_secret => dtls_v1:cookie_secret(), previous_cookie_secret => <<>>},
    Result = ssl_gen_statem:?FUNCTION_NAME(Type, Event, State#state{protocol_specific = PS}),
    erlang:send_after(dtls_v1:cookie_timeout(), self(), new_cookie_secret),
    Result;
initial_hello(Type, Event, State) ->
    ssl_gen_statem:?FUNCTION_NAME(Type, Event, State).

%%--------------------------------------------------------------------
-spec config_error(gen_statem:event_type(),
                   {start, timeout()} | term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
config_error(enter, _, State) ->
    {keep_state, State};     
config_error(Type, Event, State) ->
    ssl_gen_statem:?FUNCTION_NAME(Type, Event, State).

%%--------------------------------------------------------------------
-spec hello(gen_statem:event_type(),
	    #hello_request{} | #client_hello{} | #server_hello{} | term(),
	    #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
hello(enter, _, #state{static_env = #static_env{role = server}} = State) ->
    {keep_state, State};     
hello(enter, _, #state{static_env = #static_env{role = client}} = State0) ->
    {State, Actions} = handle_flight_timer(State0),
    {keep_state, State, Actions}; 
hello(internal, #client_hello{cookie = <<>>,
			      client_version = Version} = Hello, 
      #state{static_env = #static_env{role = server,
                                      transport_cb = Transport,
                                      socket = Socket},
             handshake_env = HsEnv,
             connection_env = CEnv,
             protocol_specific = #{current_cookie_secret := Secret}} = State0) ->
    try tls_dtls_connection:handle_sni_extension(State0, Hello) of
        #state{} = State1 ->
            {ok, {IP, Port}} = dtls_socket:peername(Transport, Socket),
            Cookie = dtls_handshake:cookie(Secret, IP, Port, Hello),
            %% FROM RFC 6347 regarding HelloVerifyRequest message:
            %% The server_version field has the same syntax as in TLS.  However, in
            %% order to avoid the requirement to do version negotiation in the
            %% initial handshake, DTLS 1.2 server implementations SHOULD use DTLS
            %% version 1.0 regardless of the version of TLS that is expected to be
            %% negotiated.
            VerifyRequest = dtls_handshake:hello_verify_request(Cookie, ?HELLO_VERIFY_REQUEST_VERSION),
            State2 = prepare_flight(State1#state{connection_env = CEnv#connection_env{negotiated_version = Version}}),
            {State, Actions} = dtls_gen_connection:send_handshake(VerifyRequest, State2),
            dtls_gen_connection:next_event(?FUNCTION_NAME, no_record,
                                           State#state{handshake_env = HsEnv#handshake_env{
                                                                         tls_handshake_history =
                                                                             ssl_handshake:init_handshake_history()}},
                                           Actions)
    catch throw:#alert{} = Alert ->
            alert_or_reset_connection(Alert, ?FUNCTION_NAME, State0)
    end;
hello(internal, #hello_verify_request{cookie = Cookie},
      #state{static_env = #static_env{role = client,
                                      host = Host,
                                      port = Port},
             handshake_env = #handshake_env{renegotiation = {Renegotiation, _},
                                            ocsp_stapling_state = OcspState0} = HsEnv,
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
        prepare_flight(
          State0#state{handshake_env =
                           HsEnv#handshake_env{
                             tls_handshake_history = ssl_handshake:init_handshake_history(),
                             ocsp_stapling_state = OcspState0#{ocsp_nonce => OcspNonce}}}),
    {State2, Actions} = dtls_gen_connection:send_handshake(Hello, State1),
    State = State2#state{connection_env =
                             CEnv#connection_env{negotiated_version = Version}, % RequestedVersion
			 protocol_specific = PS#{current_cookie_secret => Cookie}},
    dtls_gen_connection:next_event(?FUNCTION_NAME, no_record, State, Actions);
hello(internal, #client_hello{extensions = Extensions} = Hello,
      #state{handshake_env = #handshake_env{continue_status = pause},
             start_or_recv_from = From} = State0) ->
    try tls_dtls_connection:handle_sni_extension(State0, Hello) of
        #state{} = State ->
            {next_state, user_hello, State#state{start_or_recv_from = undefined},
             [{postpone, true}, {reply, From, {ok, Extensions}}]}
    catch throw:#alert{} = Alert ->
            alert_or_reset_connection(Alert, ?FUNCTION_NAME, State0)
    end;
hello(internal, #client_hello{cookie = Cookie} = Hello, #state{static_env = #static_env{role = server,
                                                                                        transport_cb = Transport,
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
hello(internal, #server_hello{extensions = Extensions}, 
      #state{handshake_env = #handshake_env{continue_status = pause},
             start_or_recv_from = From} = State) ->
    {next_state, user_hello, State#state{start_or_recv_from = undefined},
     [{postpone, true},{reply, From, {ok, Extensions}}]};
hello(internal, #server_hello{} = Hello,
      #state{
         static_env = #static_env{role = client},
         handshake_env = #handshake_env{
                            renegotiation = {Renegotiation, _},
                            ocsp_stapling_state = OcspState0} = HsEnv,
         connection_states = ConnectionStates0,
         session = #session{session_id = OldId},
         ssl_options = SslOptions} = State) ->
    try
        {Version, NewId, ConnectionStates, ProtoExt, Protocol, OcspState} =
            dtls_handshake:hello(Hello, SslOptions, ConnectionStates0, Renegotiation, OldId),
        tls_dtls_connection:handle_session(
          Hello, Version, NewId, ConnectionStates, ProtoExt, Protocol,
          State#state{handshake_env =
                          HsEnv#handshake_env{
                            ocsp_stapling_state = maps:merge(OcspState0,OcspState)}})
    catch throw:#alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, ?FUNCTION_NAME, State)
    end;
hello(internal, {handshake, {#client_hello{cookie = <<>>} = Handshake, _}}, State) ->
    %% Initial hello should not be in handshake history
    {next_state, ?FUNCTION_NAME, State, [{next_event, internal, Handshake}]};
hello(internal, {handshake, {#hello_verify_request{} = Handshake, _}}, State) ->
    %% hello_verify should not be in handshake history
    {next_state, ?FUNCTION_NAME, State, [{next_event, internal, Handshake}]};
hello(internal,  #change_cipher_spec{type = <<1>>}, State0) ->
    {State1, Actions0} = dtls_gen_connection:send_handshake_flight(State0, retransmit_epoch(?FUNCTION_NAME, State0)),
    {next_state, ?FUNCTION_NAME, State, Actions} = 
        dtls_gen_connection:next_event(?FUNCTION_NAME, no_record, State1, Actions0),
    %% This will reset the retransmission timer by repeating the enter state event
    {repeat_state, State, Actions};
hello(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
hello(state_timeout, Event, State) ->
    handle_state_timeout(Event, ?FUNCTION_NAME, State);
hello(Type, Event, State) ->
    gen_handshake(?FUNCTION_NAME, Type, Event, State).

user_hello(enter, _, State) ->
    {keep_state, State};     
user_hello(Type, Event, State) ->
    gen_handshake(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec abbreviated(gen_statem:event_type(), term(), #state{}) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
abbreviated(enter, _, State0) ->
    {State, Actions} = handle_flight_timer(State0),
    {keep_state, State, Actions}; 
abbreviated(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
abbreviated(internal = Type, 
	    #change_cipher_spec{type = <<1>>} = Event, 
	    #state{connection_states = ConnectionStates0} = State) ->
    ConnectionStates1 = dtls_record:save_current_connection_state(ConnectionStates0, read),
    ConnectionStates = dtls_record:next_epoch(ConnectionStates1, read),
    gen_handshake(?FUNCTION_NAME, Type, Event, State#state{connection_states = ConnectionStates});
abbreviated(internal = Type, #finished{} = Event, #state{connection_states = ConnectionStates,
                                                         protocol_specific = PS} = State) ->
    gen_handshake(?FUNCTION_NAME, Type, Event, 
                  prepare_flight(State#state{connection_states = ConnectionStates,
                                             protocol_specific = PS#{flight_state => connection}}));
abbreviated(state_timeout, Event, State) ->
    handle_state_timeout(Event, ?FUNCTION_NAME, State);
abbreviated(Type, Event, State) ->
    gen_handshake(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec wait_ocsp_stapling(gen_statem:event_type(), term(), #state{}) ->
		     gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_ocsp_stapling(enter, _Event, State0) ->
    {State, Actions} = handle_flight_timer(State0),
    {keep_state, State, Actions};
wait_ocsp_stapling(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
wait_ocsp_stapling(state_timeout, Event, State) ->
    handle_state_timeout(Event, ?FUNCTION_NAME, State);
wait_ocsp_stapling(Type, Event, State) ->
    gen_handshake(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec certify(gen_statem:event_type(), term(), #state{}) ->
		     gen_statem:state_function_result().
%%--------------------------------------------------------------------
certify(enter, _, State0) ->
    {State, Actions} = handle_flight_timer(State0),
    {keep_state, State, Actions}; 
certify(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
certify(internal = Type, #server_hello_done{} = Event, State) ->
    gen_handshake(?FUNCTION_NAME, Type, Event, prepare_flight(State));
certify(internal,  #change_cipher_spec{type = <<1>>}, State0) ->
    {State1, Actions0} = dtls_gen_connection:send_handshake_flight(State0, retransmit_epoch(?FUNCTION_NAME, State0)),
    {next_state, ?FUNCTION_NAME, State, Actions} = 
        dtls_gen_connection:next_event(?FUNCTION_NAME, no_record, State1, Actions0),
    %% This will reset the retransmission timer by repeating the enter state event
    {repeat_state, State, Actions};
certify(state_timeout, Event, State) ->
    handle_state_timeout(Event, ?FUNCTION_NAME, State);
certify(Type, Event, State) ->
    gen_handshake(?FUNCTION_NAME, Type, Event, State).


%%--------------------------------------------------------------------
-spec wait_cert_verify(gen_statem:event_type(), term(), #state{}) ->
          gen_statem:state_function_result().
%%--------------------------------------------------------------------
wait_cert_verify(enter, _Event, State0) ->
    {State, Actions} = handle_flight_timer(State0),
    {keep_state, State, Actions};
wait_cert_verify(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
wait_cert_verify(state_timeout, Event, State) ->
    handle_state_timeout(Event, ?FUNCTION_NAME, State);
wait_cert_verify(Type, Event, State) ->
    gen_handshake(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec cipher(gen_statem:event_type(), term(), #state{}) ->
		    gen_statem:state_function_result().
%%--------------------------------------------------------------------
cipher(enter, _, State0) ->
    {State, Actions} = handle_flight_timer(State0),
    {keep_state, State, Actions}; 
cipher(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
cipher(internal = Type, #change_cipher_spec{type = <<1>>} = Event,  
       #state{connection_states = ConnectionStates0} = State) ->
    ConnectionStates1 = dtls_record:save_current_connection_state(ConnectionStates0, read),
    ConnectionStates = dtls_record:next_epoch(ConnectionStates1, read),
    gen_handshake(?FUNCTION_NAME, Type, Event, State#state{connection_states = ConnectionStates});
cipher(internal = Type, #finished{} = Event, #state{connection_states = ConnectionStates,
                                                   protocol_specific = PS} = State) ->
    gen_handshake(?FUNCTION_NAME, Type, Event,
                  prepare_flight(State#state{connection_states = ConnectionStates,
                                             protocol_specific = PS#{flight_state => connection}}));
cipher(state_timeout, Event, State) ->
    handle_state_timeout(Event, ?FUNCTION_NAME, State);
cipher(Type, Event, State) ->
    gen_handshake(?FUNCTION_NAME, Type, Event, State).

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
connection(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
connection(internal, #hello_request{}, #state{static_env = #static_env{host = Host,
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
    Session = ssl_session:client_select_session({Host, Port, SslOpts}, Cache, CacheCb, Session0, CertKeyPairs),
    Hello = dtls_handshake:client_hello(Host, Port, Cookie, ConnectionStates0, SslOpts,
					Session#session.session_id, Renegotiation, undefined),
    Version = Hello#client_hello.client_version,
    HelloVersion = dtls_record:hello_version(Version, Versions),
    State1 = prepare_flight(State0),
    {State2, Actions} =
        dtls_gen_connection:send_handshake(Hello,
                                           State1#state{connection_env =
                                                            CEnv#connection_env{negotiated_version = HelloVersion}}),
    State = State2#state{protocol_specific = PS#{flight_state => dtls_gen_connection:initial_flight_state(DataTag)},
                         session = Session},
    dtls_gen_connection:next_event(hello, no_record, State, Actions);
connection(internal, #client_hello{} = Hello, 
           #state{static_env = #static_env{role = server},
                  handshake_env = #handshake_env{allow_renegotiate = true} = HsEnv} = State) ->
    %% Mitigate Computational DoS attack
    %% http://www.educatedguesswork.org/2011/10/ssltls_and_computational_dos.html
    %% http://www.thc.org/thc-ssl-dos/ Rather than disabling client
    %% initiated renegotiation we will disallow many client initiated
    %% renegotiations immediately after each other.
    erlang:send_after(?WAIT_TO_ALLOW_RENEGOTIATION, self(), allow_renegotiate),
    {next_state, hello, State#state{handshake_env = HsEnv#handshake_env{renegotiation = {true, peer},
                                                                        allow_renegotiate = false}},
     [{next_event, internal, Hello}]};
connection(internal, #client_hello{}, #state{static_env = #static_env{role = server,
                                                                      protocol_cb = Connection},
                                             handshake_env = #handshake_env{allow_renegotiate = false}} = State0) ->
    Alert = ?ALERT_REC(?WARNING, ?NO_RENEGOTIATION),
    State1 = dtls_gen_connection:send_alert(Alert, State0),
    {Record, State} = ssl_gen_statem:prepare_connection(State1, Connection),
    dtls_gen_connection:next_event(?FUNCTION_NAME, Record, State);
connection(internal, new_connection, #state{ssl_options=SSLOptions,
                                            handshake_env=HsEnv,
                                            static_env = #static_env{socket = {Listener, {Client, _}}},
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
                    {next_state, hello, State#state{handshake_env = HsEnv#handshake_env{renegotiation = {false, first}},
                                                    connection_states = ConnectionStates}}
            end;
        _ ->
            %% Someone spamming new_connection, just drop them
            {keep_state, State}
    end;

connection({call, From}, {application_data, Data}, State) ->
    try
        send_application_data(Data, From, ?FUNCTION_NAME, State)
    catch throw:Error ->
            ssl_gen_statem:hibernate_after(?FUNCTION_NAME, State, [{reply, From, Error}])
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
connection(Type, Event, State) ->
    try
        tls_dtls_connection:?FUNCTION_NAME(Type, Event, State)
    catch throw:#alert{}=Alert ->
            ssl_gen_statem:handle_own_alert(Alert, ?FUNCTION_NAME, State)
    end.

%%TODO does this make sense for DTLS ?
%%--------------------------------------------------------------------
-spec downgrade(gen_statem:event_type(), term(), #state{}) ->
		       gen_statem:state_function_result().
%%--------------------------------------------------------------------
downgrade(enter, _, State) ->
    {keep_state, State};
downgrade(Type, Event, State) ->
    try
        tls_dtls_connection:?FUNCTION_NAME(Type, Event, State)
    catch throw:#alert{}=Alert ->
            ssl_gen_statem:handle_own_alert(Alert, ?FUNCTION_NAME, State)
    end.


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
%%% Internal functions
%%--------------------------------------------------------------------
initial_state(Role, Host, Port, Socket,
              {SSLOptions, SocketOptions, Trackers}, User,
	      {CbModule, DataTag, CloseTag, ErrorTag, PassiveTag}) ->
    put(log_level, maps:get(log_level, SSLOptions)),
    BeastMitigation = maps:get(beast_mitigation, SSLOptions, disabled),
    ConnectionStates = dtls_record:init_connection_states(Role, BeastMitigation),
    #{session_cb := SessionCacheCb} = ssl_config:pre_1_3_session_opts(Role),
    InternalActiveN = ssl_config:get_internal_active_n(),
    Monitor = erlang:monitor(process, User),
    InitStatEnv = #static_env{
                     role = Role,
                     transport_cb = CbModule,
                     protocol_cb = dtls_gen_connection,
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

    #state{static_env = InitStatEnv,
           handshake_env = #handshake_env{
                              tls_handshake_history = ssl_handshake:init_handshake_history(),
                              renegotiation = {false, first},
                              allow_renegotiate = maps:get(client_renegotiation, SSLOptions, undefined)
                             },
           connection_env = #connection_env{user_application = {Monitor, User}},
           socket_options = SocketOptions,
	   ssl_options = SSLOptions,
	   session = #session{is_resumable = false},
	   connection_states = ConnectionStates,
	   protocol_buffers = #protocol_buffers{},
	   user_data_buffer = {[],0,[]},
	   start_or_recv_from = undefined,
	   flight_buffer = dtls_gen_connection:new_flight(),
           protocol_specific = #{active_n => InternalActiveN,
                                 active_n_toggle => true,
                                 flight_state => dtls_gen_connection:initial_flight_state(DataTag),
                                 ignored_alerts => 0,
                                 max_ignored_alerts => 10
                                }
	  }.


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
            tls_dtls_connection:handle_sni_extension(State0, Hello),
        SessionTracker = proplists:get_value(session_id_tracker, Trackers),
        {Version, {Type, Session}, ConnectionStates, Protocol0, ServerHelloExt, HashSign} =
            dtls_handshake:hello(Hello, SslOpts, {SessionTracker, Session0,
                                                  ConnectionStates0, CertKeyAlts, KeyExAlg}, Renegotiation),
        Protocol = case Protocol0 of
                       undefined -> CurrentProtocol;
                       _ -> Protocol0
                   end,

        State = prepare_flight(State0#state{connection_states = ConnectionStates,
                                            connection_env = CEnv#connection_env{negotiated_version = Version},
                                            handshake_env = HsEnv#handshake_env{
                                                              hashsign_algorithm = HashSign,
                                                              client_hello_version = ClientVersion,
                                                              negotiated_protocol = Protocol},
                                            session = Session}),
        {next_state, hello, State, [{next_event, internal, {common_client_hello, Type, ServerHelloExt}}]}
    catch #alert{} = Alert ->
            alert_or_reset_connection(Alert, hello, State0)
    end.


handle_state_timeout(flight_retransmission_timeout, StateName,
                     #state{protocol_specific = 
                                #{flight_state := {retransmit, CurrentTimeout}}} = State0) ->
    {State1, Actions0} = dtls_gen_connection:send_handshake_flight(State0, 
                                                                   retransmit_epoch(StateName, State0)),
    {next_state, StateName, #state{protocol_specific = PS} = State2, Actions} =
        dtls_gen_connection:next_event(StateName, no_record, State1, Actions0),
    State = State2#state{protocol_specific = PS#{flight_state => {retransmit, new_timeout(CurrentTimeout)}}},
    %% This will reset the retransmission timer by repeating the enter state event
    {repeat_state, State, Actions}.

alert_or_reset_connection(Alert, StateName, #state{connection_states = Cs} = State) ->
    case maps:get(previous_cs, Cs, undefined) of
        undefined ->
            ssl_gen_statem:handle_own_alert(Alert, StateName, State);
        PreviousConn ->
            %% There exists an old connection and the new one failed,
            %% reset to the old working one.
            %% The next alert will be sent
            HsEnv0 = State#state.handshake_env,
            HsEnv  = HsEnv0#handshake_env{renegotiation = undefined},
            NewState = State#state{connection_states = PreviousConn,
                                   handshake_env = HsEnv
                                  },
            {next_state, connection, NewState}
    end.

gen_handshake(_, {call, _From}, {application_data, _Data}, _State) ->
    {keep_state_and_data, [postpone]};
gen_handshake(StateName, Type, Event, State) ->
    try tls_dtls_connection:StateName(Type, Event, State)
    catch
        throw:#alert{}=Alert ->
            alert_or_reset_connection(Alert, StateName, State);
        error:Reason:ST ->
            ?SSL_LOG(info, handshake_error, [{error, Reason}, {stacktrace, ST}]),
            Alert = ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, malformed_handshake_data),
            alert_or_reset_connection(Alert, StateName, State)
    end.

gen_info(Event, connection = StateName, State) ->
    try dtls_gen_connection:handle_info(Event, StateName, State)
    catch error:Reason:ST ->
            ?SSL_LOG(info, internal_error, [{error, Reason}, {stacktrace, ST}]),
            Alert = ?ALERT_REC(?FATAL, ?INTERNAL_ERROR, malformed_data),
            alert_or_reset_connection(Alert, StateName, State)
    end;
gen_info(Event, StateName, State) ->
    try dtls_gen_connection:handle_info(Event, StateName, State)
    catch error:Reason:ST ->
            ?SSL_LOG(info, handshake_error, [{error, Reason}, {stacktrace, ST}]),
            Alert = ?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE,malformed_handshake_data),
            alert_or_reset_connection(Alert, StateName, State)
    end.

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

next_flight(Flight) ->
    Flight#{handshakes => [],
	    change_cipher_spec => undefined,
	    handshakes_after_change_cipher_spec => []}.
       
handle_flight_timer(#state{static_env = #static_env{data_tag = udp},
                           protocol_specific = #{flight_state := {retransmit, Timeout}}} = State) ->
    start_retransmision_timer(Timeout, State);
handle_flight_timer(#state{static_env = #static_env{data_tag = udp},
                           protocol_specific = #{flight_state := connection}} = State) ->
    {State, []};
handle_flight_timer(#state{protocol_specific = #{flight_state := reliable}} = State) ->
    %% No retransmision needed i.e DTLS over SCTP
    {State, []}.

start_retransmision_timer(Timeout, #state{protocol_specific = PS} = State) ->
    {State#state{protocol_specific = PS#{flight_state => {retransmit, Timeout}}},
     [{state_timeout, Timeout, flight_retransmission_timeout}]}.

new_timeout(N) when N =< 30000 ->
    N * 2;
new_timeout(_) -> 
    60000.

retransmit_epoch(_StateName, #state{connection_states = ConnectionStates}) ->
    #{epoch := Epoch} = 
	ssl_record:current_connection_state(ConnectionStates, write),
    Epoch.
	    
send_application_data(Data, From, _StateName,
                      #state{static_env = #static_env{socket = Socket,
                                                      transport_cb = Transport},
                             connection_env = #connection_env{negotiated_version = Version},
                             handshake_env = HsEnv,
                             connection_states = ConnectionStates0,
                             ssl_options = #{renegotiate_at := RenegotiateAt,
                                             log_level := LogLevel}} = State0) ->
       
    case time_to_renegotiate(Data, ConnectionStates0, RenegotiateAt) of
	true ->
	    renegotiate(State0#state{handshake_env = HsEnv#handshake_env{renegotiation = {true, internal}}}, 
                        [{next_event, {call, From}, {application_data, Data}}]);
	false ->
	    {Msgs, ConnectionStates} =
                dtls_record:encode_data(Data, Version, ConnectionStates0),
            State = State0#state{connection_states = ConnectionStates},
	    case send_msgs(Transport, Socket, Msgs) of
                ok ->
                    ssl_logger:debug(LogLevel, outbound, 'record', Msgs),
                    ssl_gen_statem:hibernate_after(connection, State, [{reply, From, ok}]);
                Result ->
                    ssl_gen_statem:hibernate_after(connection, State, [{reply, From, Result}])
            end
    end.

send_msgs(Transport, Socket, [Msg|Msgs]) ->
    case dtls_gen_connection:send(Transport, Socket, Msg) of
        ok -> send_msgs(Transport, Socket, Msgs);
        Error -> Error
    end;
send_msgs(_, _, []) ->
    ok.

time_to_renegotiate(_Data, 
		    #{current_write := #{sequence_number := Num}}, 
		    RenegotiateAt) ->
    
    %% We could do test:
    %% is_time_to_renegotiate((erlang:byte_size(_Data) div
    %% ?MAX_PLAIN_TEXT_LENGTH) + 1, RenegotiateAt), but we chose to
    %% have a some what lower renegotiateAt and a much cheaper test
    is_time_to_renegotiate(Num, RenegotiateAt).

is_time_to_renegotiate(N, M) when N < M->
    false;
is_time_to_renegotiate(_,_) ->
    true.

%%%################################################################
%%%#
%%%# Tracing
%%%#
handle_trace(hbn,
             {call, {?MODULE, connection,
                     [_Type = info, Event, _State]}},
             Stack) ->
    {io_lib:format("Type = info Event = ~W ", [Event, 10]), Stack}.
