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
-export([next_record/1, next_event/3, next_event/4,  handle_common_event/4]).

%% Handshake handling
-export([renegotiate/2, send_handshake/2, 
         queue_handshake/2, queue_change_cipher/2,
         reinit_handshake_data/1, select_sni_extension/1, empty_connection_state/2]).

%% Alert and close handling
-export([encode_alert/3,send_alert/2, close/5, protocol_name/0]).

%% Data handling
-export([encode_data/3, passive_receive/2, next_record_if_active/1,
	 send/3, socket/5, setopts/3, getopts/3]).

%% gen_statem state functions
-export([init/3, error/3, downgrade/3, %% Initiation and take down states
	 hello/3, certify/3, cipher/3, abbreviated/3, %% Handshake states 
	 connection/3]). 
%% gen_statem callbacks
-export([callback_mode/0, terminate/3, code_change/4, format_status/2]).

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
	{ok, Pid} = dtls_connection_sup:start_child([Role, Host, Port, Socket, 
						     Opts, User, CbInfo]), 
	{ok, SslSocket} = ssl_connection:socket_control(?MODULE, Socket, Pid, CbModule, Tracker),
	ok = ssl_connection:handshake(SslSocket, Timeout),
	{ok, SslSocket} 
    catch
	error:{badmatch, {error, _} = Error} ->
	    Error
    end.

%%--------------------------------------------------------------------
-spec start_link(atom(), host(), inet:port_number(), port(), list(), pid(), tuple()) ->
			{ok, pid()} | ignore |  {error, reason()}.
%%
%% Description: Creates a gen_statem process which calls Module:init/1 to
%% initialize.
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
%%====================================================================
%% State transition handling
%%====================================================================	     
next_record(#state{unprocessed_handshake_events = N} = State) when N > 0 ->
    {no_record, State#state{unprocessed_handshake_events = N-1}};
					 
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

next_event(StateName, Record, State) ->
    next_event(StateName, Record, State, []).

next_event(connection = StateName, no_record,
	   #state{connection_states = #{current_read := #{epoch := CurrentEpoch}}} = State0, Actions) ->
    case next_record_if_active(State0) of
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
	    {State2, MoreActions} = send_handshake_flight(State1, CurrentEpoch),
            {NextRecord, State} = next_record(State2),
            next_event(StateName, NextRecord, State, Actions ++ MoreActions);
        %% From FLIGHT perspective CHANGE_CIPHER_SPEC is treated as a handshake
        {#ssl_tls{epoch = Epoch,
		  type = ?CHANGE_CIPHER_SPEC,
		  version = _Version}, State1} = _Record when Epoch == CurrentEpoch-1 ->
	    {State2, MoreActions} = send_handshake_flight(State1, CurrentEpoch),
	    {NextRecord, State} = next_record(State2),
            next_event(StateName, NextRecord, State, Actions ++ MoreActions);
	{#ssl_tls{epoch = _Epoch,
		  version = _Version}, State1} ->
	    %% TODO maybe buffer later epoch
            {Record, State} = next_record(State1),
            next_event(StateName, Record, State, Actions); 
	{#alert{} = Alert, State} ->
	    {next_state, StateName, State, [{next_event, internal, Alert} | Actions]}
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
	    {State1, MoreActions} = send_handshake_flight(State0, CurrentEpoch),
            {NextRecord, State} = next_record(State1),
            next_event(StateName, NextRecord, State, Actions ++ MoreActions);
        %% From FLIGHT perspective CHANGE_CIPHER_SPEC is treated as a handshake
        #ssl_tls{epoch = Epoch,
                 type = ?CHANGE_CIPHER_SPEC,
                 version = _Version} when Epoch == CurrentEpoch-1 ->
	    {State1, MoreActions} = send_handshake_flight(State0, CurrentEpoch),
            {NextRecord, State} = next_record(State1),
            next_event(StateName, NextRecord, State, Actions ++ MoreActions); 
        _ -> 
            next_event(StateName, no_record, State0, Actions) 
    end;
next_event(StateName, Record, 
	   #state{connection_states = #{current_read := #{epoch := CurrentEpoch}}} = State0, Actions) ->
    case Record of
	no_record ->
	    {next_state, StateName, State0, Actions};
        #ssl_tls{epoch = CurrentEpoch,
                 version = Version} = Record ->
            State = dtls_version(StateName, Version, State0),
            {next_state, StateName,  State,
         [{next_event, internal, {protocol_record, Record}} | Actions]};
	#ssl_tls{epoch = _Epoch,
		 version = _Version} = _Record ->
	    %% TODO maybe buffer later epoch
            {Record, State} = next_record(State0),
            next_event(StateName, Record, State, Actions); 
	#alert{} = Alert ->
	    {next_state, StateName, State0, [{next_event, internal, Alert} | Actions]}
    end.

handle_common_event(internal, #alert{} = Alert, StateName, 
		    #state{negotiated_version = Version} = State) ->
    handle_own_alert(Alert, Version, StateName, State);
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
	    handle_own_alert(Alert, Version, StateName, State0)
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
	    handle_own_alert(Alert, Version, StateName, State)
    end;
%% Ignore unknown TLS record level protocol messages
handle_common_event(internal, #ssl_tls{type = _Unknown}, StateName, State) ->
    {next_state, StateName, State}.

%%====================================================================
%% Handshake handling
%%====================================================================	     

renegotiate(#state{role = client} = State, Actions) ->
    %% Handle same way as if server requested
    %% the renegotiation
    {next_state, connection, State,
     [{next_event, internal, #hello_request{}} | Actions]};

renegotiate(#state{role = server} = State0, Actions) ->
    HelloRequest = ssl_handshake:hello_request(),
    State1 = prepare_flight(State0),
    {State2, MoreActions} = send_handshake(HelloRequest, State1),
    {Record, State} = next_record(State2),
    next_event(hello, Record, State, Actions ++ MoreActions).

send_handshake(Handshake, #state{connection_states = ConnectionStates} = State) ->
    #{epoch := Epoch} = ssl_record:current_connection_state(ConnectionStates, write),
    send_handshake_flight(queue_handshake(Handshake, State), Epoch).

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

queue_change_cipher(ChangeCipher, #state{flight_buffer = Flight,
					 connection_states = ConnectionStates0} = State) -> 
    ConnectionStates = 
	dtls_record:next_epoch(ConnectionStates0, write), 
    State#state{flight_buffer = Flight#{change_cipher_spec => ChangeCipher},
		connection_states = ConnectionStates}.
reinit_handshake_data(#state{protocol_buffers = Buffers} = State) ->
    State#state{premaster_secret = undefined,
		public_key_info = undefined,
		tls_handshake_history = ssl_handshake:init_handshake_history(),
                flight_state = {retransmit, ?INITIAL_RETRANSMIT_TIMEOUT},
		flight_buffer = new_flight(),
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

empty_connection_state(ConnectionEnd, BeastMitigation) ->
    Empty = ssl_record:empty_connection_state(ConnectionEnd, BeastMitigation),
    dtls_record:empty_connection_state(Empty).

%%====================================================================
%% Alert and close handling
%%====================================================================	     
encode_alert(#alert{} = Alert, Version, ConnectionStates) ->
    dtls_record:encode_alert_record(Alert, Version, ConnectionStates).

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

protocol_name() ->
    "DTLS".
        
%%====================================================================
%% Data handling
%%====================================================================	 

encode_data(Data, Version, ConnectionStates0)->
    dtls_record:encode_data(Data, Version, ConnectionStates0).

passive_receive(State0 = #state{user_data_buffer = Buffer}, StateName) ->
    case Buffer of
	<<>> ->
	    {Record, State} = next_record(State0),
	    next_event(StateName, Record, State);
	_ ->
	    {Record, State} = ssl_connection:read_application_data(<<>>, State0),
	    next_event(StateName, Record, State)
    end.
next_record_if_active(State =
		      #state{socket_options =
			     #socket_options{active = false}}) ->
    {no_record ,State};

next_record_if_active(State) ->
    next_record(State).

send(Transport, {_, {{_,_}, _} = Socket}, Data) ->
    send(Transport, Socket, Data);
send(Transport, Socket, Data) ->
   dtls_socket:send(Transport, Socket, Data).

socket(Pid,  Transport, Socket, Connection, _) ->
    dtls_socket:socket(Pid, Transport, Socket, Connection).

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
    HelloVersion = dtls_record:hello_version(Version, SslOpts#ssl_options.versions),
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
    Result = gen_handshake(?FUNCTION_NAME, Type, Event, 
                           State#state{flight_state = {retransmit, ?INITIAL_RETRANSMIT_TIMEOUT},
                                       protocol_specific = #{current_cookie_secret => dtls_v1:cookie_secret(), 
                                                             previous_cookie_secret => <<>>,
                                                             ignored_alerts => 0,
                                                             max_ignored_alerts => 10}}),
    erlang:send_after(dtls_v1:cookie_timeout(), self(), new_cookie_secret),
    Result;

init({call, _} = Type, Event, #state{role = server} = State) ->
    %% I.E. DTLS over sctp
    gen_handshake(?FUNCTION_NAME, Type, Event, State#state{flight_state = reliable});
init(Type, Event, State) ->
   gen_handshake(?FUNCTION_NAME, Type, Event, State).

%%--------------------------------------------------------------------
-spec error(gen_statem:event_type(),
	   {start, timeout()} | term(), #state{}) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
error(enter, _, State) ->
    {keep_state, State};     
error({call, From}, {start, _Timeout}, {Error, State}) ->
    {stop_and_reply, normal, {reply, From, {error, Error}}, State};
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
hello(enter, _, #state{role = server} = State) ->
    {keep_state, State};     
hello(enter, _, #state{role = client} = State0) ->
    {State, Actions} = handle_flight_timer(State0),
    {keep_state, State, Actions}; 
hello(internal, #client_hello{cookie = <<>>,
			      client_version = Version} = Hello, #state{role = server,
									transport_cb = Transport,
									socket = Socket,
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
    State1 = prepare_flight(State0#state{negotiated_version = Version}),
    {State2, Actions} = send_handshake(VerifyRequest, State1),
    {Record, State} = next_record(State2),
    next_event(?FUNCTION_NAME, Record, State#state{tls_handshake_history = ssl_handshake:init_handshake_history()}, Actions);
hello(internal, #client_hello{cookie = Cookie} = Hello, #state{role = server,
							       transport_cb = Transport,
							       socket = Socket,
                                                               protocol_specific = #{current_cookie_secret := Secret,
                                                                                     previous_cookie_secret := PSecret}} = State0) ->
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
  
    Hello = dtls_handshake:client_hello(Host, Port, Cookie, ConnectionStates0,
					SslOpts,
					Cache, CacheCb, Renegotiation, OwnCert),
    Version = Hello#client_hello.client_version,
    State1 = prepare_flight(State0#state{tls_handshake_history = ssl_handshake:init_handshake_history()}),
    
    {State2, Actions} = send_handshake(Hello, State1), 
    State3 = State2#state{negotiated_version = Version, %% Requested version
			  session =
			      Session0#session{session_id = 
						   Hello#client_hello.session_id}},
    {Record, State} = next_record(State3),
    next_event(?FUNCTION_NAME, Record, State, Actions);
hello(internal, #server_hello{} = Hello,
      #state{connection_states = ConnectionStates0,
	     negotiated_version = ReqVersion,
	     role = client,
	     renegotiation = {Renegotiation, _},
	     ssl_options = SslOptions} = State) ->
    case dtls_handshake:hello(Hello, SslOptions, ConnectionStates0, Renegotiation) of
	#alert{} = Alert ->
	    handle_own_alert(Alert, ReqVersion, ?FUNCTION_NAME, State);
	{Version, NewId, ConnectionStates, ProtoExt, Protocol} ->
	    ssl_connection:handle_session(Hello, 
					  Version, NewId, ConnectionStates, ProtoExt, Protocol, State)
    end;
hello(internal, {handshake, {#client_hello{cookie = <<>>} = Handshake, _}}, State) ->
    %% Initial hello should not be in handshake history
    {next_state, ?FUNCTION_NAME, State, [{next_event, internal, Handshake}]};
hello(internal, {handshake, {#hello_verify_request{} = Handshake, _}}, State) ->
    %% hello_verify should not be in handshake history
    {next_state, ?FUNCTION_NAME, State, [{next_event, internal, Handshake}]};
hello(info, Event, State) ->
    gen_info(Event, ?FUNCTION_NAME, State);
hello(state_timeout, Event, State) ->
    handle_state_timeout(Event, ?FUNCTION_NAME, State);
hello(Type, Event, State) ->
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
abbreviated(internal = Type, #finished{} = Event, #state{connection_states = ConnectionStates} = State) ->
    gen_handshake(?FUNCTION_NAME, Type, Event, 
                  prepare_flight(State#state{connection_states = ConnectionStates,
                                             flight_state = connection}));
abbreviated(state_timeout, Event, State) ->
    handle_state_timeout(Event, ?FUNCTION_NAME, State);
abbreviated(Type, Event, State) ->
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
cipher(internal = Type, #finished{} = Event, #state{connection_states = ConnectionStates} = State) ->
    ssl_connection:?FUNCTION_NAME(Type, Event, 
                                  prepare_flight(State#state{connection_states = ConnectionStates,
                                                             flight_state = connection}), 
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
connection(internal, #hello_request{}, #state{host = Host, port = Port,
                                              session = #session{own_certificate = Cert} = Session0,
                                              session_cache = Cache, session_cache_cb = CacheCb,
                                              ssl_options = SslOpts,
                                              connection_states = ConnectionStates0,
                                              renegotiation = {Renegotiation, _}} = State0) ->
    
    Hello = dtls_handshake:client_hello(Host, Port, ConnectionStates0, SslOpts,
					Cache, CacheCb, Renegotiation, Cert),
    Version = Hello#client_hello.client_version,
    HelloVersion = dtls_record:hello_version(Version, SslOpts#ssl_options.versions),
    State1 = prepare_flight(State0),
    {State2, Actions} = send_handshake(Hello, State1#state{negotiated_version = HelloVersion}),
    {Record, State} =
	next_record(
	  State2#state{flight_state = {retransmit, ?INITIAL_RETRANSMIT_TIMEOUT},
                       session = Session0#session{session_id
						  = Hello#client_hello.session_id}}),
    next_event(hello, Record, State, Actions);
connection(internal, #client_hello{} = Hello, #state{role = server, allow_renegotiate = true} = State) ->
    %% Mitigate Computational DoS attack
    %% http://www.educatedguesswork.org/2011/10/ssltls_and_computational_dos.html
    %% http://www.thc.org/thc-ssl-dos/ Rather than disabling client
    %% initiated renegotiation we will disallow many client initiated
    %% renegotiations immediately after each other.
    erlang:send_after(?WAIT_TO_ALLOW_RENEGOTIATION, self(), allow_renegotiate),
    {next_state, hello, State#state{allow_renegotiate = false, renegotiation = {true, peer}},
     [{next_event, internal, Hello}]};
connection(internal, #client_hello{}, #state{role = server, allow_renegotiate = false} = State0) ->
    Alert = ?ALERT_REC(?WARNING, ?NO_RENEGOTIATION),
    State1 = send_alert(Alert, State0),
    {Record, State} = ssl_connection:prepare_connection(State1, ?MODULE),
    next_event(?FUNCTION_NAME, Record, State);
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

dtls_version(hello, Version, #state{role = server} = State) ->
    State#state{negotiated_version = Version}; %%Inital version
dtls_version(_,_, State) ->
    State.

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
	    handle_own_alert(Alert, ClientVersion, hello, State0);
	{Version, {Type, Session},
	 ConnectionStates, Protocol0, ServerHelloExt, HashSign} ->
	    Protocol = case Protocol0 of
			   undefined -> CurrentProtocol;
			   _ -> Protocol0
		       end,

	    State = prepare_flight(State0#state{connection_states = ConnectionStates,
						negotiated_version = Version,
						hashsign_algorithm = HashSign,
                                                client_hello_version = ClientVersion,
						session = Session,
						negotiated_protocol = Protocol}),
	    
	    ssl_connection:hello(internal, {common_client_hello, Type, ServerHelloExt},
				 State, ?MODULE)
    end.


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
	    #state{socket = Socket, 
                   socket_options = #socket_options{active = Active},
                   protocol_buffers = #protocol_buffers{dtls_cipher_texts = CTs},
                   close_tag = CloseTag,
		   negotiated_version = Version} = State) ->
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
            ssl_connection:handle_normal_shutdown(?ALERT_REC(?FATAL, ?CLOSE_NOTIFY), StateName, State),
            {stop, {shutdown, transport_closed}};
        true ->
            %% Fixes non-delivery of final DTLS record in {active, once}.
            %% Basically allows the application the opportunity to set {active, once} again
            %% and then receive the final message.
            next_event(StateName, no_record, State)
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
                    #state{flight_state = {retransmit, NextTimeout}} = State0) ->
    {State1, Actions} = send_handshake_flight(State0#state{flight_state = {retransmit, NextTimeout}}, 
                                              retransmit_epoch(StateName, State0)),
    {Record, State} = next_record(State1),
    next_event(StateName, Record, State, Actions).

handle_alerts([], Result) ->
    Result;
handle_alerts(_, {stop,_} = Stop) ->
    Stop;
handle_alerts([Alert | Alerts], {next_state, StateName, State}) ->
     handle_alerts(Alerts, ssl_connection:handle_alert(Alert, StateName, State));
handle_alerts([Alert | Alerts], {next_state, StateName, State, _Actions}) ->
     handle_alerts(Alerts, ssl_connection:handle_alert(Alert, StateName, State)).

handle_own_alert(Alert, Version, StateName, #state{transport_cb = gen_udp,
                                                   role = Role,
                                                   ssl_options = Options} = State0) ->
    case ignore_alert(Alert, State0) of
        {true, State} ->
            log_ignore_alert(Options#ssl_options.log_alert, StateName, Alert, Role),
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

update_handshake_history(#hello_verify_request{}, _, Hist) ->
    Hist;
update_handshake_history(_, Handshake, Hist) ->
    %% DTLS never needs option "v2_hello_compatible" to be true
    ssl_handshake:update_handshake_history(Hist, iolist_to_binary(Handshake), false).
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
       
handle_flight_timer(#state{transport_cb = gen_udp,
                          flight_state = {retransmit, Timeout}} = State) ->
    start_retransmision_timer(Timeout, State);
handle_flight_timer(#state{transport_cb = gen_udp,
		    flight_state = connection} = State) ->
    {State, []};
handle_flight_timer(State) ->
    %% No retransmision needed i.e DTLS over SCTP
    {State#state{flight_state = reliable}, []}.

start_retransmision_timer(Timeout, State) ->
    {State#state{flight_state = {retransmit, new_timeout(Timeout)}}, 
     [{state_timeout, Timeout, flight_retransmission_timeout}]}.

new_timeout(N) when N =< 30 -> 
    N * 2;
new_timeout(_) -> 
    60.

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
   {State0#state{connection_states = ConnectionStates}, []};

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
    {State0#state{connection_states = ConnectionStates}, []};

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
    {State0#state{connection_states = ConnectionStates}, []};

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

log_ignore_alert(true, StateName, Alert, Role) ->
    Txt = ssl_alert:alert_txt(Alert),
    error_logger:format("DTLS over UDP ~p: In state ~p ignored to send ALERT ~s as DoS-attack mitigation \n", 
                        [Role, StateName, Txt]);
log_ignore_alert(false, _, _,_) ->
    ok.
