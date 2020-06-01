%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2020. All Rights Reserved.
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
-include_lib("kernel/include/logger.hrl").

%% Internal application API

%% Setup
-export([start_fsm/8, start_link/7, init/1, pids/1]).

%% State transition handling	 
-export([next_event/3, next_event/4, handle_protocol_record/3]).

%% Handshake handling
-export([renegotiate/2, send_handshake/2, 
         queue_handshake/2, queue_change_cipher/2,
         reinit/1, reinit_handshake_data/1, select_sni_extension/1, empty_connection_state/2]).

%% Alert and close handling
-export([encode_alert/3, send_alert/2, send_alert_in_connection/2, close/5, protocol_name/0]).

%% Data handling
-export([socket/4, setopts/3, getopts/3]).

%% gen_statem state functions
-export([init/3, error/3, downgrade/3, %% Initiation and take down states
	 hello/3, user_hello/3, wait_ocsp_stapling/3, certify/3, cipher/3, abbreviated/3, %% Handshake states
	 connection/3]). 
%% gen_statem callbacks
-export([callback_mode/0, terminate/3, code_change/4, format_status/2]).

%%====================================================================
%% Internal application API
%%====================================================================	
%%====================================================================
%% Setup
%%====================================================================	     
start_fsm(Role, Host, Port, Socket, {#{erl_dist := false},_, Tracker} = Opts,
	  User, {CbModule, _, _, _, _} = CbInfo,
	  Timeout) -> 
    try 
	{ok, Pid} = dtls_connection_sup:start_child([Role, Host, Port, Socket, 
						     Opts, User, CbInfo]), 
	{ok, SslSocket} = ssl_connection:socket_control(?MODULE, Socket, [Pid], CbModule, Tracker),
	ssl_connection:handshake(SslSocket, Timeout)
    catch
	error:{badmatch, {error, _} = Error} ->
	    Error
    end.

%%--------------------------------------------------------------------
-spec start_link(atom(), ssl:host(), inet:port_number(), port(), list(), pid(), tuple()) ->
			{ok, pid()} | ignore |  {error, reason()}.
%%
%% Description: Creates a gen_statem process which calls Module:init/1 to
%% initialize.
%%--------------------------------------------------------------------
start_link(Role, Host, Port, Socket, Options, User, CbInfo) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [[Role, Host, Port, Socket, Options, User, CbInfo]])}.

init([Role, Host, Port, Socket, Options,  User, CbInfo]) ->
    process_flag(trap_exit, true),
    State0 = #state{protocol_specific = Map} = initial_state(Role, Host, Port, Socket, Options, User, CbInfo),
    try
	State = ssl_connection:ssl_config(State0#state.ssl_options, Role, State0),
	gen_statem:enter_loop(?MODULE, [], init, State)
    catch
	throw:Error ->
            EState = State0#state{protocol_specific = Map#{error => Error}},
	    gen_statem:enter_loop(?MODULE, [], error, EState)
    end.

pids(_) ->
    [self()].
    
%%====================================================================
%% State transition handling
%%====================================================================	     
next_record(#state{handshake_env = 
                       #handshake_env{unprocessed_handshake_events = N} = HsEnv} 
            = State) when N > 0 ->
    {no_record, State#state{handshake_env = 
                                HsEnv#handshake_env{unprocessed_handshake_events = N-1}}};
next_record(#state{protocol_buffers =
		       #protocol_buffers{dtls_cipher_texts = [#ssl_tls{epoch = Epoch} = CT | Rest]}
		   = Buffers,
		   connection_states = #{current_read := #{epoch := Epoch}} = ConnectionStates} = State) ->
    CurrentRead = dtls_record:get_connection_state_by_epoch(Epoch, ConnectionStates, read),
    case dtls_record:replay_detect(CT, CurrentRead) of
        false ->
            decode_cipher_text(State#state{connection_states = ConnectionStates}) ;
        true ->
            %% Ignore replayed record
            next_record(State#state{protocol_buffers =
                                        Buffers#protocol_buffers{dtls_cipher_texts = Rest},
                                    connection_states = ConnectionStates})
    end;
next_record(#state{protocol_buffers =
		       #protocol_buffers{dtls_cipher_texts = [#ssl_tls{epoch = Epoch} | Rest]}
		   = Buffers,
		   connection_states = #{current_read := #{epoch := CurrentEpoch}} = ConnectionStates} = State) 
  when Epoch > CurrentEpoch ->
    %% TODO Buffer later Epoch message, drop it for now
    next_record(State#state{protocol_buffers =
                                Buffers#protocol_buffers{dtls_cipher_texts = Rest},
                            connection_states = ConnectionStates});
next_record(#state{protocol_buffers =
		       #protocol_buffers{dtls_cipher_texts = [ _ | Rest]}
		   = Buffers,
		   connection_states = ConnectionStates} = State) ->
    %% Drop old epoch message
    next_record(State#state{protocol_buffers =
                                Buffers#protocol_buffers{dtls_cipher_texts = Rest},
                            connection_states = ConnectionStates});
next_record(#state{static_env = #static_env{role = server,
                                            socket = {Listener, {Client, _}}}} = State) ->
    dtls_packet_demux:active_once(Listener, Client, self()),
    {no_record, State};
next_record(#state{protocol_specific = #{active_n_toggle := true,
                                         active_n := N} = ProtocolSpec,
                   static_env = #static_env{role = client,
                                            socket = {_Server, Socket} = DTLSSocket,
                                            close_tag = CloseTag,
                                            transport_cb = Transport}} = State) ->
    case dtls_socket:setopts(Transport, Socket, [{active,N}]) of
        ok ->
            {no_record, State#state{protocol_specific =
                                        ProtocolSpec#{active_n_toggle => false}}};
 	_ ->
            self() ! {CloseTag, DTLSSocket},
	    {no_record, State}
    end;
next_record(State) ->
    {no_record, State}.

next_event(StateName, Record, State) ->
    next_event(StateName, Record, State, []).

next_event(StateName, no_record,
	   #state{connection_states = #{current_read := #{epoch := CurrentEpoch}}} = State0, Actions) ->
    case next_record(State0) of
	{no_record, State} ->
            ssl_connection:hibernate_after(StateName, State, Actions);
        {#ssl_tls{epoch = CurrentEpoch,
                  type = ?HANDSHAKE,
                  version = Version} = Record, State1} ->
            State = dtls_version(StateName, Version, State1), 
	    {next_state, StateName, State,
	     [{next_event, internal, {protocol_record, Record}} | Actions]};
        {#ssl_tls{epoch = CurrentEpoch} = Record, State} ->
	    {next_state, StateName, State, [{next_event, internal, {protocol_record, Record}} | Actions]};
	{#ssl_tls{epoch = Epoch,
		  type = ?HANDSHAKE,
		  version = _Version}, State1} = _Record when Epoch == CurrentEpoch-1 ->
	    {State, MoreActions} = send_handshake_flight(State1, CurrentEpoch),
            next_event(StateName, no_record, State, Actions ++ MoreActions);
        %% From FLIGHT perspective CHANGE_CIPHER_SPEC is treated as a handshake
        {#ssl_tls{epoch = Epoch,
		  type = ?CHANGE_CIPHER_SPEC,
		  version = _Version}, State1} = _Record when Epoch == CurrentEpoch-1 ->
	    {State, MoreActions} = send_handshake_flight(State1, CurrentEpoch),
            next_event(StateName, no_record, State, Actions ++ MoreActions);
	{#ssl_tls{epoch = _Epoch,
		  version = _Version}, State} ->
	    %% TODO maybe buffer later epoch
            next_event(StateName, no_record, State, Actions); 
	{#alert{} = Alert, State} ->
            Version = State#state.connection_env#connection_env.negotiated_version,
            handle_own_alert(Alert, Version, StateName, State)
    end;
next_event(connection = StateName, Record,
	   #state{connection_states = #{current_read := #{epoch := CurrentEpoch}}} = State0, Actions) ->
    case Record of
        #ssl_tls{epoch = CurrentEpoch,
                 type = ?HANDSHAKE,
                 version = Version} = Record ->
            State = dtls_version(StateName, Version, State0), 
	    {next_state, StateName, State,
	     [{next_event, internal, {protocol_record, Record}} | Actions]};
	#ssl_tls{epoch = CurrentEpoch} ->
	    {next_state, StateName, State0, [{next_event, internal, {protocol_record, Record}} | Actions]};
	#ssl_tls{epoch = Epoch,
                 type = ?HANDSHAKE,
                 version = _Version} when Epoch == CurrentEpoch-1 ->
	    {State, MoreActions} = send_handshake_flight(State0, CurrentEpoch),
            next_event(StateName, no_record, State, Actions ++ MoreActions);
        %% From FLIGHT perspective CHANGE_CIPHER_SPEC is treated as a handshake
        #ssl_tls{epoch = Epoch,
                 type = ?CHANGE_CIPHER_SPEC,
                 version = _Version} when Epoch == CurrentEpoch-1 ->
	    {State, MoreActions} = send_handshake_flight(State0, CurrentEpoch),
            next_event(StateName, no_record, State, Actions ++ MoreActions); 
        _ -> 
            next_event(StateName, no_record, State0, Actions) 
    end;
next_event(StateName, Record, 
	   #state{connection_states = #{current_read := #{epoch := CurrentEpoch}}} = State0, Actions) ->
    case Record of
        #ssl_tls{epoch = CurrentEpoch,
                 version = Version} = Record ->
            State = dtls_version(StateName, Version, State0),
            {next_state, StateName, State, 
             [{next_event, internal, {protocol_record, Record}} | Actions]};
	#ssl_tls{epoch = _Epoch,
		 version = _Version} = _Record ->
	    %% TODO maybe buffer later epoch
            next_event(StateName, no_record, State0, Actions); 
	#alert{} = Alert ->
	    Version = State0#state.connection_env#connection_env.negotiated_version,
            handle_own_alert(Alert, Version, StateName, State0)
    end.

%%% DTLS record protocol level application data messages 

handle_protocol_record(#ssl_tls{type = ?APPLICATION_DATA, fragment = Data}, StateName0, State0) ->
    case ssl_connection:read_application_data(Data, State0) of
	{stop, _, _} = Stop->
            Stop;
	{Record, State1} ->
            {next_state, StateName, State, Actions} = next_event(StateName0, Record, State1), 
            ssl_connection:hibernate_after(StateName, State, Actions)
    end;
%%% DTLS record protocol level handshake messages 
handle_protocol_record(#ssl_tls{type = ?HANDSHAKE,
				       fragment = Data}, 
		    StateName, 
                       #state{protocol_buffers = Buffers0,
                              connection_env = #connection_env{negotiated_version = Version},
                              ssl_options = Options} = State) ->
    try
	case dtls_handshake:get_dtls_handshake(Version, Data, Buffers0, Options) of
	    {[], Buffers} ->
		next_event(StateName, no_record, State#state{protocol_buffers = Buffers});
	    {Packets, Buffers} ->
		HsEnv = State#state.handshake_env,
		Events = dtls_handshake_events(Packets),
                {next_state, StateName, 
                 State#state{protocol_buffers = Buffers,
                             handshake_env = 
                                 HsEnv#handshake_env{unprocessed_handshake_events 
                                                     = unprocessed_events(Events)}}, Events}
	end
    catch throw:#alert{} = Alert ->
	    handle_own_alert(Alert, Version, StateName, State)
    end;
%%% DTLS record protocol level change cipher messages
handle_protocol_record(#ssl_tls{type = ?CHANGE_CIPHER_SPEC, fragment = Data}, StateName, State) ->
    {next_state, StateName, State, [{next_event, internal, #change_cipher_spec{type = Data}}]};
%%% DTLS record protocol level Alert messages
handle_protocol_record(#ssl_tls{type = ?ALERT, fragment = EncAlerts}, StateName,
                       #state{connection_env = #connection_env{negotiated_version = Version}} = State) ->
    case decode_alerts(EncAlerts) of
	Alerts = [_|_] ->
	    handle_alerts(Alerts,  {next_state, StateName, State});
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, StateName, State)
    end;
%% Ignore unknown TLS record level protocol messages
handle_protocol_record(#ssl_tls{type = _Unknown}, StateName, State) ->
    {next_state, StateName, State, []}.

%%====================================================================
%% Handshake handling
%%====================================================================	     

renegotiate(#state{static_env = #static_env{role = client}} = State0, Actions) ->
    %% Handle same way as if server requested
    %% the renegotiation
    State = reinit_handshake_data(State0),
    {next_state, connection, State,
     [{next_event, internal, #hello_request{}} | Actions]};

renegotiate(#state{static_env = #static_env{role = server}} = State0, Actions) ->
    HelloRequest = ssl_handshake:hello_request(),
    State1 = prepare_flight(State0),
    {State, MoreActions} = send_handshake(HelloRequest, State1),
    next_event(hello, no_record, State, Actions ++ MoreActions).

send_handshake(Handshake, #state{connection_states = ConnectionStates} = State) ->
    #{epoch := Epoch} = ssl_record:current_connection_state(ConnectionStates, write),
    send_handshake_flight(queue_handshake(Handshake, State), Epoch).

queue_handshake(Handshake0, #state{handshake_env = #handshake_env{tls_handshake_history = Hist0} = HsEnv, 
                                   connection_env = #connection_env{negotiated_version = Version},
				   flight_buffer = #{handshakes := HsBuffer0,
						     change_cipher_spec := undefined,
						     next_sequence := Seq} = Flight0,
                                   ssl_options = #{log_level := LogLevel}} = State) ->
    Handshake = dtls_handshake:encode_handshake(Handshake0, Version, Seq),
    Hist = update_handshake_history(Handshake0, Handshake, Hist0),
    ssl_logger:debug(LogLevel, outbound, 'handshake', Handshake0),

    State#state{flight_buffer = Flight0#{handshakes => [Handshake | HsBuffer0],
					 next_sequence => Seq +1},
	handshake_env = HsEnv#handshake_env{tls_handshake_history = Hist}};

queue_handshake(Handshake0, #state{handshake_env = #handshake_env{tls_handshake_history = Hist0} = HsEnv, 
                                   connection_env = #connection_env{negotiated_version = Version},
				   flight_buffer = #{handshakes_after_change_cipher_spec := Buffer0,
						     next_sequence := Seq} = Flight0,
                                   ssl_options = #{log_level := LogLevel}} = State) ->
    Handshake = dtls_handshake:encode_handshake(Handshake0, Version, Seq),
    Hist = update_handshake_history(Handshake0, Handshake, Hist0),
    ssl_logger:debug(LogLevel, outbound, 'handshake', Handshake0),

    State#state{flight_buffer = Flight0#{handshakes_after_change_cipher_spec => [Handshake | Buffer0],
					 next_sequence => Seq +1},
                handshake_env = HsEnv#handshake_env{tls_handshake_history = Hist}}.

queue_change_cipher(ChangeCipher, #state{flight_buffer = Flight,
					 connection_states = ConnectionStates0} = State) -> 
    ConnectionStates = 
	dtls_record:next_epoch(ConnectionStates0, write),
    State#state{flight_buffer = Flight#{change_cipher_spec => ChangeCipher},
		connection_states = ConnectionStates}.

reinit(State) ->
    %% To be API compatible with TLS NOOP here
    reinit_handshake_data(State).
reinit_handshake_data(#state{static_env = #static_env{data_tag = DataTag},
                             protocol_buffers = Buffers,
                             protocol_specific = PS,
                             handshake_env = HsEnv} = State) ->
    State#state{handshake_env = HsEnv#handshake_env{tls_handshake_history = ssl_handshake:init_handshake_history(),
                                                    public_key_info = undefined,
                                                    premaster_secret = undefined},
                protocol_specific = PS#{flight_state => initial_flight_state(DataTag)},
		flight_buffer = new_flight(),
                protocol_buffers =
		    Buffers#protocol_buffers{
                      dtls_handshake_next_seq = 0,
		      dtls_handshake_next_fragments = [],
		      dtls_handshake_later_fragments = []
		     }}.

select_sni_extension(#client_hello{extensions = #{sni := SNI}}) ->
    SNI;
select_sni_extension(_) ->
    undefined.

empty_connection_state(ConnectionEnd, BeastMitigation) ->
    Empty = ssl_record:empty_connection_state(ConnectionEnd, BeastMitigation),
    dtls_record:empty_connection_state(Empty).

%%====================================================================
%% Alert and close handling
%%====================================================================	     
encode_alert(#alert{} = Alert, Version, ConnectionStates) ->
    dtls_record:encode_alert_record(Alert, Version, ConnectionStates).

send_alert(Alert, #state{static_env = #static_env{socket = Socket,
                                                  transport_cb = Transport},

                         connection_env = #connection_env{negotiated_version = Version},
			 connection_states = ConnectionStates0,
                         ssl_options = #{log_level := LogLevel}} = State0) ->
    {BinMsg, ConnectionStates} =
	encode_alert(Alert, Version, ConnectionStates0),
    send(Transport, Socket, BinMsg),
    ssl_logger:debug(LogLevel, outbound, 'record', BinMsg),
    State0#state{connection_states = ConnectionStates}.

send_alert_in_connection(Alert, State) ->
    _ = send_alert(Alert, State),
    ok.

close(downgrade, _,_,_,_) ->
    ok;
%% Other
close(_, Socket, Transport, _,_) ->
    dtls_socket:close(Transport,Socket).

protocol_name() ->
    "DTLS".
        
%%====================================================================
%% Data handling
%%====================================================================	 

send(Transport, {Listener, Socket}, Data) when is_pid(Listener) -> % Server socket
    dtls_socket:send(Transport, Socket, Data);
send(Transport, Socket, Data) -> % Client socket
    dtls_socket:send(Transport, Socket, Data).

socket(Pid,  Transport, Socket, _Tracker) ->
    dtls_socket:socket(Pid, Transport, Socket, ?MODULE).

setopts(Transport, Socket, Other) ->
    dtls_socket:setopts(Transport, Socket, Other).

getopts(Transport, Socket, Tag) ->
    dtls_socket:getopts(Transport, Socket, Tag).

%%--------------------------------------------------------------------
%% State functions 
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
-spec init(gen_statem:event_type(),
	   {start, timeout()} | term(), #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
init(enter, _, State) ->
    {keep_state, State};     
init({call, From}, {start, Timeout}, 
     #state{static_env = #static_env{host = Host,
                                     port = Port,
                                     role = client,
                                     session_cache = Cache,
                                     session_cache_cb = CacheCb},
            handshake_env = #handshake_env{renegotiation = {Renegotiation, _}},
            connection_env = CEnv,
	    ssl_options = #{versions := Versions} = SslOpts,
	    session = #session{own_certificate = Cert} = NewSession,
	    connection_states = ConnectionStates0
	   } = State0) ->
    Session = ssl_session:client_select_session({Host, Port, SslOpts}, Cache, CacheCb, NewSession), 
    Hello = dtls_handshake:client_hello(Host, Port, ConnectionStates0, SslOpts,
					Session#session.session_id, Renegotiation, Cert),

    MaxFragEnum = maps:get(max_frag_enum, Hello#client_hello.extensions, undefined),
    ConnectionStates1 = ssl_record:set_max_fragment_length(MaxFragEnum, ConnectionStates0),
    Version = Hello#client_hello.client_version,
    HelloVersion = dtls_record:hello_version(Version, Versions),
    State1 = prepare_flight(State0#state{connection_env = CEnv#connection_env{negotiated_version = Version},
                                         connection_states = ConnectionStates1}),
    {State2, Actions} = send_handshake(Hello, State1#state{connection_env = CEnv#connection_env{negotiated_version = HelloVersion}}),  
    State = State2#state{connection_env = CEnv#connection_env{negotiated_version = Version}, %% RequestedVersion
			  session = Session,
			  start_or_recv_from = From},
    next_event(hello, no_record, State, [{{timeout, handshake}, Timeout, close} | Actions]);
init({call, _} = Type, Event, #state{static_env = #static_env{role = server},
                                     protocol_specific = PS} = State) ->
    Result = gen_handshake(?FUNCTION_NAME, Type, Event, 
                           State#state{protocol_specific = PS#{current_cookie_secret => dtls_v1:cookie_secret(), 
                                                               previous_cookie_secret => <<>>,
                                                               ignored_alerts => 0,
                                                               max_ignored_alerts => 10}}),
    erlang:send_after(dtls_v1:cookie_timeout(), self(), new_cookie_secret),
    Result;
init(Type, Event, State) ->
   gen_handshake(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec error(gen_statem:event_type(),
	   {start, timeout()} | term(), #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
error(enter, _, State) ->
    {keep_state, State};     
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
    {ok, {IP, Port}} = dtls_socket:peername(Transport, Socket),
    Cookie = dtls_handshake:cookie(Secret, IP, Port, Hello),
    %% FROM RFC 6347 regarding HelloVerifyRequest message:
    %% The server_version field has the same syntax as in TLS.  However, in
    %% order to avoid the requirement to do version negotiation in the
    %% initial handshake, DTLS 1.2 server implementations SHOULD use DTLS
    %% version 1.0 regardless of the version of TLS that is expected to be
    %% negotiated.
    VerifyRequest = dtls_handshake:hello_verify_request(Cookie, ?HELLO_VERIFY_REQUEST_VERSION),
    State1 = prepare_flight(State0#state{connection_env = CEnv#connection_env{negotiated_version = Version}}),
    {State, Actions} = send_handshake(VerifyRequest, State1),
    next_event(?FUNCTION_NAME, no_record, 
               State#state{handshake_env = HsEnv#handshake_env{
                                             tls_handshake_history = 
                                                 ssl_handshake:init_handshake_history()}}, 
               Actions);
hello(internal, #hello_verify_request{cookie = Cookie}, #state{static_env = #static_env{role = client,
                                                                                        host = Host,
                                                                                        port = Port},
                                                               handshake_env = #handshake_env{renegotiation = {Renegotiation, _}} = HsEnv,
                                                               connection_env = CEnv,
							       ssl_options = #{ocsp_stapling := OcspStaplingOpt,
                                                   ocsp_nonce := OcspNonceOpt} = SslOpts,
							       session = #session{own_certificate = Cert, session_id = Id},
							       connection_states = ConnectionStates0
							      } = State0) ->
    OcspNonce = tls_handshake:ocsp_nonce(OcspNonceOpt, OcspStaplingOpt),
    Hello = dtls_handshake:client_hello(Host, Port, Cookie, ConnectionStates0,
					SslOpts, Id, Renegotiation, Cert, OcspNonce),
    Version = Hello#client_hello.client_version,
    State1 = prepare_flight(State0#state{handshake_env =  
                                             HsEnv#handshake_env{tls_handshake_history 
                                                                 = ssl_handshake:init_handshake_history()}}),
    
    {State2, Actions} = send_handshake(Hello, State1), 
    State = State2#state{connection_env = CEnv#connection_env{negotiated_version = Version} % RequestedVersion
                        },
    next_event(?FUNCTION_NAME, no_record, State, Actions);
hello(internal, #client_hello{extensions = Extensions} = Hello, 
      #state{ssl_options = #{handshake := hello},
             handshake_env = HsEnv,
             start_or_recv_from = From} = State) ->
    {next_state, user_hello, State#state{start_or_recv_from = undefined,
                                         handshake_env = HsEnv#handshake_env{hello = Hello}},
     [{reply, From, {ok, Extensions}}]};
hello(internal, #server_hello{extensions = Extensions} = Hello, 
      #state{ssl_options = #{
                 handshake := hello,
                 ocsp_stapling := OcspStapling},
             handshake_env = HsEnv,
             start_or_recv_from = From} = State) ->
    %% RFC6066.8, If a server returns a "CertificateStatus" message,
    %% then the server MUST have included an extension of type
    %% "status_request" with empty "extension_data" in the extended
    %% server hello.
    OcspState = HsEnv#handshake_env.ocsp_stapling_state,
    OcspNegotiated = tls_connection:is_ocsp_stapling_negotiated(OcspStapling,
                                                                Extensions,
                                                                State),
    {next_state, user_hello, State#state{start_or_recv_from = undefined,
                                         handshake_env = HsEnv#handshake_env{
                                             hello = Hello,
                                             ocsp_stapling_state = OcspState#{
                                                 ocsp_negotiated => OcspNegotiated}}},
     [{reply, From, {ok, Extensions}}]};     

hello(internal, #client_hello{cookie = Cookie} = Hello, #state{static_env = #static_env{role = server,
                                                                                        transport_cb = Transport,
                                                                                        socket = Socket},
                                                               protocol_specific = #{current_cookie_secret := Secret,
                                                                                     previous_cookie_secret := PSecret}
                                                              } = State0) ->
    {ok, {IP, Port}} = dtls_socket:peername(Transport, Socket),
    case dtls_handshake:cookie(Secret, IP, Port, Hello) of
	Cookie ->
	    handle_client_hello(Hello, State0);
	_ ->
            case dtls_handshake:cookie(PSecret, IP, Port, Hello) of
               	Cookie -> 
                    handle_client_hello(Hello, State0);
                _ ->
                    %% Handle bad cookie as new cookie request RFC 6347 4.1.2
                    hello(internal, Hello#client_hello{cookie = <<>>}, State0) 
            end
    end;
hello(internal, #server_hello{extensions = Extensions} = Hello,
      #state{
         static_env = #static_env{role = client},
         handshake_env = #handshake_env{
             renegotiation = {Renegotiation, _},
             ocsp_stapling_state = OcspState} = HsEnv,
         connection_env = #connection_env{negotiated_version = ReqVersion},
         connection_states = ConnectionStates0,
         session = #session{session_id = OldId},
         ssl_options = SslOptions} = State) ->
    %% check if OCSP stapling is negotiated
    #{ocsp_stapling := OcspStapling} = SslOptions,
    OcspNegotiated = tls_connection:is_ocsp_stapling_negotiated(
        OcspStapling, Extensions, State),

    case dtls_handshake:hello(Hello, SslOptions, ConnectionStates0, Renegotiation, OldId) of
	#alert{} = Alert ->
	    handle_own_alert(Alert, ReqVersion, ?FUNCTION_NAME, State);
	{Version, NewId, ConnectionStates, ProtoExt, Protocol} ->
	    ssl_connection:handle_session(Hello, 
					  Version, NewId, ConnectionStates, ProtoExt, Protocol,
                      State#state{handshake_env = HsEnv#handshake_env{
                          ocsp_stapling_state = OcspState#{
                              ocsp_negotiated => OcspNegotiated}}})
    end;
hello(internal, {handshake, {#client_hello{cookie = <<>>} = Handshake, _}}, State) ->
    %% Initial hello should not be in handshake history
    {next_state, ?FUNCTION_NAME, State, [{next_event, internal, Handshake}]};
hello(internal, {handshake, {#hello_verify_request{} = Handshake, _}}, State) ->
    %% hello_verify should not be in handshake history
    {next_state, ?FUNCTION_NAME, State, [{next_event, internal, Handshake}]};
hello(internal,  #change_cipher_spec{type = <<1>>}, State0) ->
    {State1, Actions0} = send_handshake_flight(State0, retransmit_epoch(?FUNCTION_NAME, State0)),
    {next_state, ?FUNCTION_NAME, State, Actions} = next_event(?FUNCTION_NAME, no_record, State1, Actions0),
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
wait_ocsp_stapling(enter, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
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
    ssl_connection:certify(Type, Event, prepare_flight(State), ?MODULE);
certify(internal,  #change_cipher_spec{type = <<1>>}, State0) ->
    {State1, Actions0} = send_handshake_flight(State0, retransmit_epoch(?FUNCTION_NAME, State0)),
    {next_state, ?FUNCTION_NAME, State, Actions} = next_event(?FUNCTION_NAME, no_record, State1, Actions0),
    %% This will reset the retransmission timer by repeating the enter state event
    {repeat_state, State, Actions};
certify(state_timeout, Event, State) ->
    handle_state_timeout(Event, ?FUNCTION_NAME, State);
certify(Type, Event, State) ->
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
    ssl_connection:?FUNCTION_NAME(Type, Event, State#state{connection_states = ConnectionStates}, ?MODULE);
cipher(internal = Type, #finished{} = Event, #state{connection_states = ConnectionStates,
                                                   protocol_specific = PS} = State) ->
    ssl_connection:?FUNCTION_NAME(Type, Event, 
                                  prepare_flight(State#state{connection_states = ConnectionStates,
                                                             protocol_specific = PS#{flight_state => connection}}), 
                                  ?MODULE);
cipher(state_timeout, Event, State) ->
    handle_state_timeout(Event, ?FUNCTION_NAME, State);
cipher(Type, Event, State) ->
     ssl_connection:?FUNCTION_NAME(Type, Event, State, ?MODULE).

%%--------------------------------------------------------------------
-spec connection(gen_statem:event_type(),  
		 #hello_request{} | #client_hello{}| term(), #state{}) ->
			gen_statem:state_function_result().
%%--------------------------------------------------------------------
connection(enter, _, State) ->
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
                                              connection_env = CEnv,
                                              session = #session{own_certificate = Cert} = Session0,
                                              ssl_options = #{versions := Versions} = SslOpts,
                                              connection_states = ConnectionStates0,
                                              protocol_specific = PS
                                             } = State0) ->
    
    Session = ssl_session:client_select_session({Host, Port, SslOpts}, Cache, CacheCb, Session0),
    Hello = dtls_handshake:client_hello(Host, Port, ConnectionStates0, SslOpts,
					Session#session.session_id, Renegotiation, Cert),
    Version = Hello#client_hello.client_version,
    HelloVersion = dtls_record:hello_version(Version, Versions),
    State1 = prepare_flight(State0),
    {State2, Actions} = send_handshake(Hello, State1#state{connection_env = CEnv#connection_env{negotiated_version = HelloVersion}}),
    State = State2#state{protocol_specific = PS#{flight_state => initial_flight_state(DataTag)},
                         session = Session},
    next_event(hello, no_record, State, Actions);
connection(internal, #client_hello{} = Hello, #state{static_env = #static_env{role = server},
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
connection(internal, #client_hello{}, #state{static_env = #static_env{role = server},
                                             handshake_env = #handshake_env{allow_renegotiate = false}} = State0) ->
    Alert = ?ALERT_REC(?WARNING, ?NO_RENEGOTIATION),
    State1 = send_alert(Alert, State0),
    {Record, State} = ssl_connection:prepare_connection(State1, ?MODULE),
    next_event(?FUNCTION_NAME, Record, State);
connection({call, From}, {application_data, Data}, State) ->
    try
        send_application_data(Data, From, ?FUNCTION_NAME, State)
    catch throw:Error ->
            ssl_connection:hibernate_after(?FUNCTION_NAME, State, [{reply, From, Error}])
    end;
connection(Type, Event, State) ->
     ssl_connection:?FUNCTION_NAME(Type, Event, State, ?MODULE).

%%TODO does this make sense for DTLS ?
%%--------------------------------------------------------------------
-spec downgrade(gen_statem:event_type(), term(), #state{}) ->
		       gen_statem:state_function_result().
%%--------------------------------------------------------------------
downgrade(enter, _, State) ->
    {keep_state, State};
downgrade(Type, Event, State) ->
     ssl_connection:?FUNCTION_NAME(Type, Event, State, ?MODULE).

%%--------------------------------------------------------------------
%% gen_statem callbacks
%%--------------------------------------------------------------------
callback_mode() ->
    [state_functions, state_enter].

terminate(Reason, StateName, State) ->
    ssl_connection:terminate(Reason, StateName, State).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

format_status(Type, Data) ->
    ssl_connection:format_status(Type, Data).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
initial_state(Role, Host, Port, Socket,
              {#{client_renegotiation := ClientRenegotiation} = SSLOptions, SocketOptions, _}, User,
	      {CbModule, DataTag, CloseTag, ErrorTag, PassiveTag}) ->
    #{beast_mitigation := BeastMitigation} = SSLOptions,
    ConnectionStates = dtls_record:init_connection_states(Role, BeastMitigation),
    
    SessionCacheCb = case application:get_env(ssl, session_cb) of
			 {ok, Cb} when is_atom(Cb) ->
			    Cb;
			 _  ->
			     ssl_session_cache
		     end,
    InternalActiveN =  case application:get_env(ssl, internal_active_n) of
                           {ok, N} when is_integer(N) ->
                               N;
                           _  ->
                               ?INTERNAL_ACTIVE_N
                       end,
    Monitor = erlang:monitor(process, User),
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
                     session_cache_cb = SessionCacheCb
                    },

    #state{static_env = InitStatEnv,
           handshake_env = #handshake_env{
                              tls_handshake_history = ssl_handshake:init_handshake_history(),
                              renegotiation = {false, first},
                              allow_renegotiate = ClientRenegotiation
                             },
           connection_env = #connection_env{user_application = {Monitor, User}},
           socket_options = SocketOptions,
	   %% We do not want to save the password in the state so that
	   %% could be written in the clear into error logs.
	   ssl_options = SSLOptions#{password => undefined},
	   session = #session{is_resumable = new},
	   connection_states = ConnectionStates,
	   protocol_buffers = #protocol_buffers{},
	   user_data_buffer = {[],0,[]},
	   start_or_recv_from = undefined,
	   flight_buffer = new_flight(),
           protocol_specific = #{active_n => InternalActiveN,
                                 active_n_toggle => true,
                                 flight_state => initial_flight_state(DataTag)}
	  }.

initial_flight_state(udp)->
    {retransmit, ?INITIAL_RETRANSMIT_TIMEOUT};
initial_flight_state(_) ->
    reliable.

next_dtls_record(Data, StateName, #state{protocol_buffers = #protocol_buffers{
						   dtls_record_buffer = Buf0,
						   dtls_cipher_texts = CT0} = Buffers,
                                         connection_env = #connection_env{negotiated_version = Version},
                                         static_env = #static_env{data_tag = DataTag},
                                         ssl_options = SslOpts} = State0) ->
    case dtls_record:get_dtls_records(Data,
                                      {DataTag, StateName, Version, 
                                       [dtls_record:protocol_version(Vsn) || Vsn <- ?ALL_AVAILABLE_DATAGRAM_VERSIONS]}, 
                                      Buf0, SslOpts) of
	{Records, Buf1} ->
	    CT1 = CT0 ++ Records,
	    next_record(State0#state{protocol_buffers =
					 Buffers#protocol_buffers{dtls_record_buffer = Buf1,
								  dtls_cipher_texts = CT1}});
	#alert{} = Alert ->
	    Alert
    end.


dtls_handshake_events(Packets) ->
    lists:map(fun(Packet) ->
		      {next_event, internal, {handshake, Packet}}
	      end, Packets).

decode_cipher_text(#state{protocol_buffers = #protocol_buffers{dtls_cipher_texts = [ CT | Rest]} = Buffers,
                          connection_states = ConnStates0} = State) ->
    case dtls_record:decode_cipher_text(CT, ConnStates0) of
	{Plain, ConnStates} ->		      
	    {Plain, State#state{protocol_buffers =
				    Buffers#protocol_buffers{dtls_cipher_texts = Rest},
				connection_states = ConnStates}};
	#alert{} = Alert ->
	    {Alert, State}
    end.

dtls_version(hello, Version, #state{static_env = #static_env{role = server},
                                    connection_env = CEnv} = State) ->
    State#state{connection_env = CEnv#connection_env{negotiated_version = Version}}; %%Inital version
dtls_version(_,_, State) ->
    State.

handle_client_hello(#client_hello{client_version = ClientVersion} = Hello,
		    #state{connection_states = ConnectionStates0,
                           static_env = #static_env{port = Port,
                                                     session_cache = Cache,
                                                    session_cache_cb = CacheCb},
                           handshake_env = #handshake_env{kex_algorithm = KeyExAlg,
                                                          renegotiation = {Renegotiation, _},
                                                          negotiated_protocol = CurrentProtocol} = HsEnv,
                           connection_env = CEnv,
			   session = #session{own_certificate = Cert} = Session0,
			   ssl_options = SslOpts} = State0) ->
    
    case dtls_handshake:hello(Hello, SslOpts, {Port, Session0, Cache, CacheCb,
					       ConnectionStates0, Cert, KeyExAlg}, Renegotiation) of
	#alert{} = Alert ->
	    handle_own_alert(Alert, ClientVersion, hello, State0);
	{Version, {Type, Session},
	 ConnectionStates, Protocol0, ServerHelloExt, HashSign} ->
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
	    
	    ssl_connection:hello(internal, {common_client_hello, Type, ServerHelloExt},
				 State, ?MODULE)
    end.


%% raw data from socket, unpack records
handle_info({Protocol, _, _, _, Data}, StateName,
            #state{static_env = #static_env{role = Role,
                                            data_tag = Protocol}} = State0) ->
    case next_dtls_record(Data, StateName, State0) of
	{Record, State} ->
	    next_event(StateName, Record, State);
	#alert{} = Alert ->
	    ssl_connection:handle_normal_shutdown(Alert#alert{role = Role}, StateName, State0), 
            {stop, {shutdown, own_alert}, State0}
    end;

handle_info({PassiveTag, Socket}, StateName,
            #state{static_env = #static_env{socket = {_, Socket},
                                            passive_tag = PassiveTag},
                   protocol_specific = PS} = State) ->
    next_event(StateName, no_record,
               State#state{protocol_specific = PS#{active_n_toggle => true}});

handle_info({CloseTag, Socket}, StateName,
	    #state{static_env = #static_env{
                                   role = Role,
                                   socket = Socket,
                                   close_tag = CloseTag},
                   connection_env = #connection_env{negotiated_version = Version},
                   socket_options = #socket_options{active = Active},
                   protocol_buffers = #protocol_buffers{dtls_cipher_texts = CTs},
                   protocol_specific = PS} = State) ->
    %% Note that as of DTLS 1.2 (TLS 1.1),
    %% failure to properly close a connection no longer requires that a
    %% session not be resumed.	This is a change from DTLS 1.0 to conform
    %% with widespread implementation practice.
    case (Active == false) andalso (CTs =/= []) of
        false ->
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
            Alert = ?ALERT_REC(?FATAL, ?CLOSE_NOTIFY, transport_closed),
            ssl_connection:handle_normal_shutdown(Alert#alert{role = Role}, StateName, State),
            {stop, {shutdown, transport_closed}, State};
        true ->
            %% Fixes non-delivery of final DTLS record in {active, once}.
            %% Basically allows the application the opportunity to set {active, once} again
            %% and then receive the final message.
            next_event(StateName, no_record, State#state{
                                               protocol_specific = PS#{active_n_toggle => true}})
    end;

handle_info(new_cookie_secret, StateName, 
            #state{protocol_specific = #{current_cookie_secret := Secret} = CookieInfo} = State) ->
    erlang:send_after(dtls_v1:cookie_timeout(), self(), new_cookie_secret),
    {next_state, StateName, State#state{protocol_specific = 
                                            CookieInfo#{current_cookie_secret => dtls_v1:cookie_secret(),
                                                        previous_cookie_secret => Secret}}};
handle_info(Msg, StateName, State) ->
    ssl_connection:StateName(info, Msg, State, ?MODULE).

handle_state_timeout(flight_retransmission_timeout, StateName,
                     #state{protocol_specific = 
                                #{flight_state := {retransmit, _NextTimeout}}} = State0) ->
    {State1, Actions0} = send_handshake_flight(State0, 
                                               retransmit_epoch(StateName, State0)),
    {next_state, StateName, State, Actions} = next_event(StateName, no_record, State1, Actions0),
    %% This will reset the retransmission timer by repeating the enter state event
    {repeat_state, State, Actions}.

handle_alerts([], Result) ->
    Result;
handle_alerts(_, {stop, _, _} = Stop) ->
    Stop;
handle_alerts([Alert | Alerts], {next_state, StateName, State}) ->
     handle_alerts(Alerts, ssl_connection:handle_alert(Alert, StateName, State));
handle_alerts([Alert | Alerts], {next_state, StateName, State, _Actions}) ->
     handle_alerts(Alerts, ssl_connection:handle_alert(Alert, StateName, State)).

handle_own_alert(Alert, Version, StateName,
                 #state{static_env = #static_env{data_tag = udp,
                                                 role = Role},
                        ssl_options = #{log_level := LogLevel}} = State0) ->
    case ignore_alert(Alert, State0) of
        {true, State} ->
            log_ignore_alert(LogLevel, StateName, Alert, Role),
            {next_state, StateName, State};
        {false, State} ->
            ssl_connection:handle_own_alert(Alert, Version, StateName, State)
    end;
handle_own_alert(Alert, Version, StateName, State) ->
    ssl_connection:handle_own_alert(Alert, Version, StateName, State).

encode_handshake_flight(Flight, Version, MaxFragmentSize, Epoch, ConnectionStates) ->
    Fragments = lists:map(fun(Handshake) ->
				  dtls_handshake:fragment_handshake(Handshake, MaxFragmentSize)
			  end, Flight),
    dtls_record:encode_handshake(Fragments, Version, Epoch, ConnectionStates).

encode_change_cipher(#change_cipher_spec{}, Version, Epoch, ConnectionStates) ->
    dtls_record:encode_change_cipher_spec(Version, Epoch, ConnectionStates).

decode_alerts(Bin) ->
    ssl_alert:decode(Bin).

gen_handshake(StateName, Type, Event, 
	      #state{connection_env = #connection_env{negotiated_version = Version}} = State) ->
    try ssl_connection:StateName(Type, Event, State, ?MODULE) of
	Result ->
	    Result
    catch 
	_:_ ->
 	    ssl_connection:handle_own_alert(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, 
						       malformed_handshake_data),
					    Version, StateName, State)  
    end.

gen_info(Event, connection = StateName,  #state{connection_env = #connection_env{negotiated_version = Version}} = State) ->
    try handle_info(Event, StateName, State) of
	Result ->
	    Result
    catch 
	_:_ ->
	    ssl_connection:handle_own_alert(?ALERT_REC(?FATAL, ?INTERNAL_ERROR, 
						       malformed_data), 
					    Version, StateName, State)  
    end;

gen_info(Event, StateName, #state{connection_env = #connection_env{negotiated_version = Version}} = State) ->
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

update_handshake_history(#hello_verify_request{}, _, Hist) ->
    Hist;
update_handshake_history(_, Handshake, Hist) ->
    ssl_handshake:update_handshake_history(Hist, iolist_to_binary(Handshake)).
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
    {State#state{protocol_specific = PS#{flight_state => {retransmit, new_timeout(Timeout)}}}, 
     [{state_timeout, Timeout, flight_retransmission_timeout}]}.

new_timeout(N) when N =< 30000 ->
    N * 2;
new_timeout(_) -> 
    60000.

send_handshake_flight(#state{static_env = #static_env{socket = Socket,
                                                      transport_cb = Transport},
                             connection_env = #connection_env{negotiated_version = Version},
                             flight_buffer = #{handshakes := Flight,
					       change_cipher_spec := undefined},
			     connection_states = ConnectionStates0,
                             ssl_options = #{log_level := LogLevel}} = State0,
                      Epoch) ->
    PMTUEstimate = 1400, %% TODO make configurable
    #{current_write := #{max_fragment_length := MaxFragmentLength}} = ConnectionStates0,
    MaxSize = min(MaxFragmentLength, PMTUEstimate),
    {Encoded, ConnectionStates} =
	encode_handshake_flight(lists:reverse(Flight), Version, MaxSize, Epoch, ConnectionStates0),
    send(Transport, Socket, Encoded),
    ssl_logger:debug(LogLevel, outbound, 'record', Encoded),
   {State0#state{connection_states = ConnectionStates}, []};

send_handshake_flight(#state{static_env = #static_env{socket = Socket,
                                                      transport_cb = Transport},
                             connection_env = #connection_env{negotiated_version = Version},
			     flight_buffer = #{handshakes := [_|_] = Flight0,
					       change_cipher_spec := ChangeCipher,
					       handshakes_after_change_cipher_spec := []},
			     connection_states = ConnectionStates0,
                             ssl_options = #{log_level := LogLevel}} = State0,
                      Epoch) ->
    PMTUEstimate = 1400, %% TODO make configurable
    #{current_write := #{max_fragment_length := MaxFragmentLength}} = ConnectionStates0,
    MaxSize = min(MaxFragmentLength, PMTUEstimate),
    {HsBefore, ConnectionStates1} =
	encode_handshake_flight(lists:reverse(Flight0), Version, MaxSize, Epoch, ConnectionStates0),
    {EncChangeCipher, ConnectionStates} = encode_change_cipher(ChangeCipher, Version, Epoch, ConnectionStates1),

    send(Transport, Socket, [HsBefore, EncChangeCipher]),
    ssl_logger:debug(LogLevel, outbound, 'record', [HsBefore]),
    ssl_logger:debug(LogLevel, outbound, 'record', [EncChangeCipher]),
    {State0#state{connection_states = ConnectionStates}, []};

send_handshake_flight(#state{static_env = #static_env{socket = Socket,
                                                      transport_cb = Transport},
                             connection_env = #connection_env{negotiated_version = Version},
			     flight_buffer = #{handshakes := [_|_] = Flight0,
					       change_cipher_spec := ChangeCipher,
					       handshakes_after_change_cipher_spec := Flight1},
			     connection_states = ConnectionStates0,
                             ssl_options = #{log_level := LogLevel}} = State0,
                      Epoch) ->
    PMTUEstimate = 1400, %% TODO make configurable
    #{current_write := #{max_fragment_length := MaxFragmentLength}} = ConnectionStates0,
    MaxSize = min(MaxFragmentLength, PMTUEstimate),
    {HsBefore, ConnectionStates1} =
	encode_handshake_flight(lists:reverse(Flight0), Version, MaxSize, Epoch-1, ConnectionStates0),
    {EncChangeCipher, ConnectionStates2} = 
	encode_change_cipher(ChangeCipher, Version, Epoch-1, ConnectionStates1),
    {HsAfter, ConnectionStates} =
	encode_handshake_flight(lists:reverse(Flight1), Version, MaxSize, Epoch, ConnectionStates2),
    send(Transport, Socket, [HsBefore, EncChangeCipher, HsAfter]),
    ssl_logger:debug(LogLevel, outbound, 'record', [HsBefore]),
    ssl_logger:debug(LogLevel, outbound, 'record', [EncChangeCipher]),
    ssl_logger:debug(LogLevel, outbound, 'record', [HsAfter]),
    {State0#state{connection_states = ConnectionStates}, []};

send_handshake_flight(#state{static_env = #static_env{socket = Socket,
                                                      transport_cb = Transport},
                             connection_env = #connection_env{negotiated_version = Version},
			     flight_buffer = #{handshakes := [],
					       change_cipher_spec := ChangeCipher,
					       handshakes_after_change_cipher_spec := Flight1},
			     connection_states = ConnectionStates0,
                             ssl_options = #{log_level := LogLevel}} = State0,
                      Epoch) ->
    PMTUEstimate = 1400, %% TODO make configurable
    #{current_write := #{max_fragment_length := MaxFragmentLength}} = ConnectionStates0,
    MaxSize = min(MaxFragmentLength, PMTUEstimate),
    {EncChangeCipher, ConnectionStates1} = 
	encode_change_cipher(ChangeCipher, Version, Epoch-1, ConnectionStates0),
    {HsAfter, ConnectionStates} =
	encode_handshake_flight(lists:reverse(Flight1), Version, MaxSize, Epoch, ConnectionStates1),
    send(Transport, Socket, [EncChangeCipher, HsAfter]),
    ssl_logger:debug(LogLevel, outbound, 'record', [EncChangeCipher]),
    ssl_logger:debug(LogLevel, outbound, 'record', [HsAfter]),
    {State0#state{connection_states = ConnectionStates}, []}.

retransmit_epoch(_StateName, #state{connection_states = ConnectionStates}) ->
    #{epoch := Epoch} = 
	ssl_record:current_connection_state(ConnectionStates, write),
    Epoch.
	    
ignore_alert(#alert{level = ?FATAL}, #state{protocol_specific = #{ignored_alerts := N,
                                                  max_ignored_alerts := N}} = State) ->
    {false, State};
ignore_alert(#alert{level = ?FATAL} = Alert, 
             #state{protocol_specific = #{ignored_alerts := N} = PS} = State) ->
    case is_ignore_alert(Alert) of
        true ->
            {true, State#state{protocol_specific = PS#{ignored_alerts => N+1}}};
        false ->
            {false, State}
    end;      
ignore_alert(_, State) ->
    {false, State}.

%% RFC 6347 4.1.2.7.  Handling Invalid Records
%% recommends to silently ignore invalid DTLS records when
%% upd is the transport. Note we do not support compression so no need
%% include ?DECOMPRESSION_FAILURE
is_ignore_alert(#alert{description = ?BAD_RECORD_MAC}) ->
    true;
is_ignore_alert(#alert{description = ?RECORD_OVERFLOW}) ->
    true;
is_ignore_alert(#alert{description = ?DECODE_ERROR}) ->
    true;
is_ignore_alert(#alert{description = ?DECRYPT_ERROR}) ->
    true;
is_ignore_alert(#alert{description = ?ILLEGAL_PARAMETER}) ->
     true;
is_ignore_alert(_) ->
    false.

log_ignore_alert(Level, StateName, #alert{where = Location} = Alert, Role) ->
    ssl_logger:log(info, 
                   Level, #{alert => Alert, 
                            alerter => ignored,
                            statename => StateName,
                            role => Role,  
                            protocol => protocol_name()}, Location).

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
	    case send(Transport, Socket, Msgs) of
                ok ->
                    ssl_logger:debug(LogLevel, outbound, 'record', Msgs),
                    ssl_connection:hibernate_after(connection, State, [{reply, From, ok}]);
                Result ->
                    ssl_connection:hibernate_after(connection, State, [{reply, From, Result}])
            end
    end.

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

