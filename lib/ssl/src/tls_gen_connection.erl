%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020-2023. All Rights Reserved.
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
%% Purpose: 
%%----------------------------------------------------------------------

-module(tls_gen_connection).

-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/logger.hrl").

-include("tls_connection.hrl").
-include("tls_handshake.hrl").
-include("tls_record.hrl").
-include("ssl_alert.hrl").
-include("ssl_api.hrl").
-include("ssl_internal.hrl").
-include("tls_record_1_3.hrl").

%% Setup
-export([start_fsm/8,
         pids/1,
         initialize_tls_sender/1]).

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
         empty_connection_state/2,
         encode_handshake/4]).

%% State transition handling	 
-export([next_event/3,
         next_event/4,
         handle_protocol_record/3]).

%% Data handling
-export([socket/4,
         setopts/3,
         getopts/3,
         handle_info/3]).

%% Alert and close handling
-export([send_alert/2,
         send_alert_in_connection/2,
         send_sync_alert/2,
         close/4,
         protocol_name/0]).

%%====================================================================
%% Internal application API
%%====================================================================
%%====================================================================
%% Setup
%%====================================================================
start_fsm(Role, Host, Port, Socket,
          {SSLOpts, _, Trackers} = Opts,
	  User, {CbModule, _, _, _, _} = CbInfo,
	  Timeout) ->
    ErlDist = maps:get(erl_dist, SSLOpts, false),
    SenderSpawnOpts = maps:get(sender_spawn_opts, SSLOpts, []),
    SenderOptions = handle_sender_options(ErlDist, SenderSpawnOpts),
    Starter = start_connection_tree(User, ErlDist, SenderOptions,
                                    Role, [Host, Port, Socket, Opts, User, CbInfo]),
    receive
        {Starter, SockReceiver, SockSender} ->
            socket_control(Socket, SockReceiver, SockSender, CbModule, Trackers, Timeout);
        {Starter, Error} ->
            Error
    end.

handle_sender_options(ErlDist, SpawnOpts) ->
    case ErlDist of
        true ->
            [[{spawn_opt, [{priority, max} | proplists:delete(priority, SpawnOpts)]}]];
        false ->
            [[{spawn_opt, SpawnOpts}]]
    end.

start_connection_tree(User, IsErlDist, SenderOpts, Role, ReceiverOpts) ->
    StartConnectionTree =
        fun() ->
                try start_dyn_connection_sup(IsErlDist) of
                    {ok, DynSup} ->
                        case tls_dyn_connection_sup:start_child(DynSup, sender, SenderOpts) of
                            {ok, Sender} ->
                                case tls_dyn_connection_sup:start_child(DynSup, receiver,
                                                                        [Role, Sender | ReceiverOpts]) of
                                    {ok, Receiver} ->
                                        User ! {self(), Receiver, Sender};
                                    {error, Error} ->
                                        User ! {self(), Error},
                                        exit(DynSup, shutdown)
                                end;
                            {error, Error} ->
                                User ! {self(), Error},
                                exit(DynSup, shutdown)
                        end;
                    {error, Error} ->
                        User ! {self(), Error}
                catch exit:{noproc, _} ->
                        User ! {self(), {error, ssl_not_started}};
                      _:Reason:ST ->  %% Don't hang signal internal error
                        ?SSL_LOG(notice, internal_error, [{error, Reason}, {stacktrace, ST}]),
                        User ! {self(), {error, internal_error}}
                end
        end,
    spawn(StartConnectionTree).

start_dyn_connection_sup(true) ->
    tls_connection_sup:start_child_dist([]);
start_dyn_connection_sup(false) ->
    tls_connection_sup:start_child([]).

socket_control(Socket, SockReceiver, SockSender, CbModule, Trackers, Timeout) ->
    case ssl_gen_statem:socket_control(?MODULE, Socket, [SockReceiver, SockSender], CbModule, Trackers) of
        {ok, SslSocket} ->
            ssl_gen_statem:handshake(SslSocket, Timeout);
        Error ->
            Error
    end.

pids(#state{protocol_specific = #{sender := Sender}}) ->
    [self(), Sender].

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
                                             log_level := LogLevel
                                            } = SSLOpts,
                             connection_states = #{current_write := ConnectionWriteState},
                             protocol_specific = #{sender := Sender}}) ->
    HibernateAfter = maps:get(hibernate_after, SSLOpts, infinity),
    Init = #{current_write => ConnectionWriteState,
             role => Role,
             socket => Socket,
             socket_options => SockOpts,
             erl_dist => maps:get(erl_dist, SSLOpts, false),
             trackers => Trackers,
             transport_cb => Transport,
             negotiated_version => Version,
             renegotiate_at => RenegotiateAt,
             key_update_at => KeyUpdateAt,
             log_level => LogLevel,
             hibernate_after => HibernateAfter},
    tls_sender:initialize(Sender, Init).

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
%% Data handling
%%====================================================================	     

socket(Pids,  Transport, Socket, Trackers) ->
    tls_socket:socket(Pids, Transport, Socket, tls_connection, Trackers).

setopts(Transport, Socket, Other) ->
    tls_socket:setopts(Transport, Socket, Other).

getopts(Transport, Socket, Tag) ->
    tls_socket:getopts(Transport, Socket, Tag).

%% raw data from socket, upack records
handle_info({Protocol, _, Data}, StateName,
            #state{static_env = #static_env{data_tag = Protocol}} = State0) ->
    case next_tls_record(Data, StateName, State0) of
	{Record, State} ->
	    next_event(StateName, Record, State);
	#alert{} = Alert ->
	    ssl_gen_statem:handle_own_alert(Alert, StateName, State0)
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
            %% practice and go against the spec
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
handle_info({ssl_tls, Port, Type, {Major, Minor}, Data}, StateName,
            #state{static_env = #static_env{data_tag = Protocol},
                   ssl_options = #{ktls := true}} = State0) ->
    Len = byte_size(Data),
    handle_info({Protocol, Port, <<Type, Major, Minor, Len:16, Data/binary>>}, StateName, State0);
handle_info(Msg, StateName, State) ->
    ssl_gen_statem:handle_info(Msg, StateName, State).

%%====================================================================
%% State transition handling
%%====================================================================	     
next_event(StateName, Record, State) ->
    next_event(StateName, Record, State, []).

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
handle_protocol_record(#ssl_tls{type = ?APPLICATION_DATA}, StateName,
                       #state{static_env = #static_env{role = server},
                              handshake_env = #handshake_env{renegotiation = {false, first}}
                             } = State) when StateName == initial_hello;
                                             StateName == hello;
                                             StateName == certify;
                                             StateName == wait_cert_verify;
                                             StateName == wait_ocsp_stapling;
                                             StateName == abbreviated;
                                             StateName == cipher
                                             ->
    %% Application data can not be sent before initial handshake pre TLS-1.3.
    Alert = ?ALERT_REC(?FATAL, ?UNEXPECTED_MESSAGE, application_data_before_initial_handshake),
    ssl_gen_statem:handle_own_alert(Alert, StateName, State);
handle_protocol_record(#ssl_tls{type = ?APPLICATION_DATA, early_data = false}, StateName,
                       #state{static_env = #static_env{role = server}
                             } = State) when StateName == start;
                                             StateName == recvd_ch;
                                             StateName == negotiated;
                                             StateName == wait_eoed ->
    Alert = ?ALERT_REC(?FATAL, ?UNEXPECTED_MESSAGE, none_early_application_data_before_handshake),
    ssl_gen_statem:handle_own_alert(Alert, StateName, State);
handle_protocol_record(#ssl_tls{type = ?APPLICATION_DATA}, StateName,
                       #state{static_env = #static_env{role = server}
                             } = State) when StateName == wait_cert;
                                             StateName == wait_cv;
                                             StateName == wait_finished->
    Alert = ?ALERT_REC(?FATAL, ?UNEXPECTED_MESSAGE, application_data_before_handshake_or_intervened_in_post_handshake_auth),
    ssl_gen_statem:handle_own_alert(Alert, StateName, State);
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
handle_protocol_record(#ssl_tls{type = ?HANDSHAKE, fragment = Data}, StateName,
                       #state{ssl_options = Options, protocol_buffers = Buffers} = State0) ->
    try
        {HSPackets, NewHSBuffer, RecordRest} = get_tls_handshakes(Data, StateName, State0),
        State = State0#state{protocol_buffers = Buffers#protocol_buffers{tls_handshake_buffer = NewHSBuffer}},
	case HSPackets of
            [] -> 
                assert_buffer_sanity(NewHSBuffer, Options),
                next_event(StateName, no_record, State);
            _ ->                
                Events = tls_handshake_events(HSPackets, RecordRest),
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
            ssl_gen_statem:handle_own_alert(Alert, StateName, State0)
    end;
%%% TLS record protocol level change cipher messages
handle_protocol_record(#ssl_tls{type = ?CHANGE_CIPHER_SPEC, fragment = Data}, StateName, State) ->
    {next_state, StateName, State, [{next_event, internal, #change_cipher_spec{type = Data}}]};
%%% TLS record protocol level Alert messages
handle_protocol_record(#ssl_tls{type = ?ALERT, fragment = EncAlerts}, StateName,State) ->
    try decode_alerts(EncAlerts) of	
	Alerts = [_|_] ->
	    handle_alerts(Alerts,  {next_state, StateName, State});
	[] ->
	    ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, empty_alert),
					    StateName, State);
        #alert{} = Alert ->
            ssl_gen_statem:handle_own_alert(Alert, StateName, State)
    catch
	_:Reason:ST ->
            ?SSL_LOG(info, handshake_error, [{error, Reason}, {stacktrace, ST}]),
	    ssl_gen_statem:handle_own_alert(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, alert_decode_error),
					    StateName, State)

    end;
%% Ignore unknown TLS record level protocol messages
handle_protocol_record(#ssl_tls{type = _Unknown}, StateName, State) ->
    {next_state, StateName, State, []}.

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
                         connection_env = #connection_env{negotiated_version = Version0},
                         ssl_options = #{log_level := LogLevel,
                                         versions := Versions},
                         connection_states = ConnectionStates0} = StateData0) ->
    Version = available_version(Version0, Versions),
    {BinMsg, ConnectionStates} =
        encode_alert(Alert, Version, ConnectionStates0),
    tls_socket:send(Transport, Socket, BinMsg),
    ssl_logger:debug(LogLevel, outbound, 'record', BinMsg),
    StateData0#state{connection_states = ConnectionStates}.

available_version(undefined, Versions) ->
    [Version| _] = lists:reverse(Versions),
    Version;
available_version(NegotiatedVersion, _) ->
    NegotiatedVersion.

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
        _:Reason:ST ->
            ?SSL_LOG(info, "Send failed", [{error, Reason}, {stacktrace, ST}]),
            throw({stop, {shutdown, own_alert}, State})
    end.

%% User closes or recursive call!
close({close, Timeout}, Socket, Transport = gen_tcp, _) ->
    tls_socket:setopts(Transport, Socket, [{active, false}]),
    Transport:shutdown(Socket, write),
    _ = Transport:recv(Socket, 0, Timeout),
    ok;
%% Peer closed socket
close({shutdown, transport_closed}, Socket, Transport = gen_tcp, ConnectionStates) ->
    close({close, 0}, Socket, Transport, ConnectionStates);
%% We generate fatal alert
close({shutdown, own_alert}, Socket, Transport = gen_tcp, ConnectionStates) ->
    %% Standard trick to try to make sure all
    %% data sent to the tcp port is really delivered to the
    %% peer application before tcp port is closed so that the peer will
    %% get the correct TLS alert message and not only a transport close.
    %% Will return when other side has closed or after timeout millisec
    %% e.g. we do not want to hang if something goes wrong
    %% with the network but we want to maximise the odds that
    %% peer application gets all data sent on the tcp connection.
    close({close, ?DEFAULT_TIMEOUT}, Socket, Transport, ConnectionStates);
%% Other
close(_, Socket, Transport, _) ->
    tls_socket:close(Transport, Socket).
protocol_name() ->
    "TLS".


%%====================================================================
%% Internal functions 
%%====================================================================	     
get_tls_handshakes(Data, StateName, #state{protocol_buffers = #protocol_buffers{tls_handshake_buffer = HSBuffer},
                                           connection_env = #connection_env{negotiated_version = Version},
                                           static_env = #static_env{role = Role},
                                           ssl_options = Options}) ->
    case handle_unnegotiated_version(Version, Options, Data, HSBuffer, Role, StateName) of
        {HSPackets, NewHSBuffer} ->
            %% Common case
            NoRecordRest = <<>>,
            {HSPackets, NewHSBuffer, NoRecordRest};
        {_Packets, _HSBuffer, _RecordRest} = Result ->
            %% Possible coalesced TLS record data from pre TLS-1.3 server
            Result
    end.

tls_handshake_events(HSPackets, <<>>) ->
    lists:map(fun(HSPacket) ->
                      {next_event, internal, {handshake, HSPacket}}
              end, HSPackets);

tls_handshake_events(HSPackets, RecordRest) ->
    %% Coalesced TLS record data to be handled after first handshake message has been handled
    RestEvent = {next_event, internal, {protocol_record, #ssl_tls{type = ?HANDSHAKE, fragment = RecordRest}}},
    FirstHS = tls_handshake_events(HSPackets, <<>>),
    FirstHS ++ [RestEvent].

unprocessed_events(Events) ->
    %% The first handshake event will be processed immediately
    %% as it is entered first in the event queue and
    %% when it is processed there will be length(Events)-1
    %% handshake events left to process before we should
    %% process more TLS-records received on the socket. 
    erlang:length(Events)-1.

encode_handshake(Handshake, Version, ConnectionStates0, Hist0) ->
    Frag = tls_handshake:encode_handshake(Handshake, Version),
    Hist = ssl_handshake:update_handshake_history(Hist0, Frag),
    {Encoded, ConnectionStates} =
        tls_record:encode_handshake(Frag, Version, ConnectionStates0),
    {Encoded, ConnectionStates, Hist}.

encode_change_cipher(#change_cipher_spec{}, Version, ConnectionStates) ->
    tls_record:encode_change_cipher_spec(Version, ConnectionStates).

next_tls_record(Data, StateName,
                #state{protocol_buffers =
                           #protocol_buffers{tls_record_buffer = Buf0,
                                             tls_cipher_texts = CT0} = Buffers,
                       connection_env = #connection_env{
                                           downgrade = Downgrade},
                       ssl_options = SslOpts0} = State0) ->
    Versions =
        %% TLSPlaintext.legacy_record_version is ignored in TLS 1.3 and thus all
        %% record version are accepted when receiving initial ClientHello and
        %% ServerHello. This can happen in state 'hello' in case of all TLS
        %% versions and also in state 'start' when TLS 1.3 is negotiated.
        %% After the version is negotiated all subsequent TLS records shall have
        %% the proper legacy_record_version (= negotiated_version).
        %% Note: TLS record version {3,4} is used internally in TLS 1.3 and at this
        %% point it is the same as the negotiated protocol version. TLS-1.3
        %% uses TLS-1.2 as record version.
        case StateName of
            State when State =:= hello orelse
                       State =:= start ->
                %% Allow any {03,XX} TLS record version for the hello message
                %% for maximum interopability and compliance with TLS-1.2 spec.
                %% This does not allow SSL-3.0 connections, that we do not support
                %% or interfere with TLS-1.3 extensions to handle version negotiation.
                AllHelloVersions = [ 'sslv3' | ?ALL_AVAILABLE_VERSIONS],
                [tls_record:protocol_version_name(Vsn) || Vsn <- AllHelloVersions];
            _ ->
                State0#state.connection_env#connection_env.negotiated_version
        end,
    #{current_write := #{max_fragment_length := MaxFragLen}} = State0#state.connection_states,
    SslOpts = maps:put(downgrade, Downgrade, SslOpts0),
    case tls_record:get_tls_records(Data, Versions, Buf0, MaxFragLen, SslOpts) of
	{Records, Buf1} ->
	    CT1 = CT0 ++ Records,
	    next_record(StateName, State0#state{protocol_buffers =
					 Buffers#protocol_buffers{tls_record_buffer = Buf1,
								  tls_cipher_texts = CT1}});
	#alert{} = Alert ->
	    handle_record_alert(Alert, State0)
    end.

next_record(_, #state{handshake_env = 
                       #handshake_env{unprocessed_handshake_events = N} = HsEnv} 
            = State) when N > 0 ->
    {no_record, State#state{handshake_env = 
                                HsEnv#handshake_env{unprocessed_handshake_events = N-1}}};
next_record(_, #state{protocol_buffers =
                          #protocol_buffers{tls_cipher_texts = [_|_] = CipherTexts},
                      connection_states = ConnectionStates,
                      ssl_options = SslOpts} = State) ->
    %% Do not match this option as it is relevant for TLS-1.0 only
    %% and will not be present otherwise that is we regard it to always be true
    Check = maps:get(padding_check, SslOpts, true),
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

flow_ctrl(#state{ssl_options = #{ktls := true}} = State) ->
    {no_record, State};
%%% bytes_to_read equals the integer Length arg of ssl:recv
%%% the actual value is only relevant for packet = raw | 0
%%% bytes_to_read = undefined means no recv call is ongoing
flow_ctrl(#state{user_data_buffer = {_,Size,_},
                 socket_options = #socket_options{active = false},
                 bytes_to_read = undefined} = State)  when Size =/= 0 ->
    %% Passive mode wait for new recv request or socket activation
    %% that is preserve some tcp back pressure by waiting to activate
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
    next_record(State, CipherTexts, ConnectionStates, Check, [], false).
%%
next_record(#state{connection_env = #connection_env{negotiated_version = ?TLS_1_3 = Version}} = State,
            [CT|CipherTexts], ConnectionStates0, Check, Acc, IsEarlyData) ->
    case tls_record:decode_cipher_text(Version, CT, ConnectionStates0, Check) of
        {Record = #ssl_tls{type = ?APPLICATION_DATA, fragment = Fragment}, ConnectionStates} ->
            case CipherTexts of
                [] ->
                    %% End of cipher texts - build and deliver an ?APPLICATION_DATA record
                    %% from the accumulated fragments
                    next_record_done(State, [], ConnectionStates,
                                     Record#ssl_tls{type = ?APPLICATION_DATA,
                                                    fragment = iolist_to_binary(lists:reverse(Acc, [Fragment]))});
                [_|_] ->
                    next_record(State, CipherTexts, ConnectionStates, Check, [Fragment|Acc], Record#ssl_tls.early_data)
            end;
        {no_record, ConnectionStates} ->
            case CipherTexts of
                [] ->
                    Record = accumulated_app_record(Acc, IsEarlyData),
                    next_record_done(State, [], ConnectionStates, Record);
                [_|_] ->
                    next_record(State, CipherTexts, ConnectionStates, Check, Acc, IsEarlyData)
            end;
        {Record, ConnectionStates} when Acc =:= [] ->
            %% Singleton non-?APPLICATION_DATA record - deliver
            next_record_done(State, CipherTexts, ConnectionStates, Record);
        {_Record, _ConnectionStates_to_forget} ->
            %% Not ?APPLICATION_DATA but we have accumulated fragments
            %% -> build an ?APPLICATION_DATA record with concatenated fragments
            %%    and forget about decrypting this record - we'll decrypt it again next time
            %% Will not work for stream ciphers
            next_record_done(State, [CT|CipherTexts], ConnectionStates0,
                             #ssl_tls{type = ?APPLICATION_DATA, 
                                      early_data = IsEarlyData,
                                      fragment = iolist_to_binary(lists:reverse(Acc))});
        #alert{} = Alert ->
            Alert
    end;
next_record(#state{connection_env = #connection_env{negotiated_version = Version}} = State,
            [#ssl_tls{type = ?APPLICATION_DATA} = CT |CipherTexts], ConnectionStates0, Check, Acc, NotRelevant) ->
    case tls_record:decode_cipher_text(Version, CT, ConnectionStates0, Check) of
        {Record = #ssl_tls{type = ?APPLICATION_DATA, fragment = Fragment}, ConnectionStates} ->
            case CipherTexts of
                [] ->
                    %% End of cipher texts - build and deliver an ?APPLICATION_DATA record
                    %% from the accumulated fragments
                    next_record_done(State, [], ConnectionStates,
                                     Record#ssl_tls{type = ?APPLICATION_DATA,
                                                    fragment = iolist_to_binary(lists:reverse(Acc, [Fragment]))});
                [_|_] ->
                    next_record(State, CipherTexts, ConnectionStates, Check, [Fragment|Acc], NotRelevant)
            end;
        #alert{} = Alert ->
            Alert
    end;
next_record(State, CipherTexts, ConnectionStates, _, [_|_] = Acc, IsEarlyData) ->
    next_record_done(State, CipherTexts, ConnectionStates,
                     #ssl_tls{type = ?APPLICATION_DATA,
                              early_data = IsEarlyData,
                              fragment = iolist_to_binary(lists:reverse(Acc))});
next_record(#state{connection_env = #connection_env{negotiated_version = Version}} = State,
            [CT|CipherTexts], ConnectionStates0, Check, [], _) ->
    case tls_record:decode_cipher_text(Version, CT, ConnectionStates0, Check) of      
        {Record, ConnectionStates} ->
            %% Singleton non-?APPLICATION_DATA record - deliver
            next_record_done(State, CipherTexts, ConnectionStates, Record);
        #alert{} = Alert ->
            Alert
    end.

accumulated_app_record([], _) ->
    no_record;
accumulated_app_record([_|_] = Acc, IsEarlyData) ->
    #ssl_tls{type = ?APPLICATION_DATA,
             early_data = IsEarlyData,
             fragment = iolist_to_binary(lists:reverse(Acc))}.

next_record_done(#state{protocol_buffers = Buffers} = State, CipherTexts, ConnectionStates, Record) ->
    {Record,
     State#state{protocol_buffers = Buffers#protocol_buffers{tls_cipher_texts = CipherTexts},
                 connection_states = ConnectionStates}}.


%% Pre TLS-1.3, on the client side, the connection state variable `negotiated_version` will initially be
%% the requested version. On the server side the same variable is initially undefined.
%% When the client can support TLS-1.3 and one or more prior versions and we are waiting
%% for the server hello the "initial requested version" kept in the connection state variable `negotiated_version`
%% (before the versions is actually negotiated) will always be the value of TLS-1.2 (which is a legacy
%% field in TLS-1.3 client hello). The versions are instead negotiated with an hello extension. When
%% decoding the server_hello messages we want to go through TLS-1.3 decode functions to be able
%% to handle TLS-1.3 extensions if TLS-1.3 will be the negotiated version.
handle_unnegotiated_version(?LEGACY_VERSION , #{versions := [?TLS_1_3 = Version |_]} = Options, Data, Buffer, client, hello) ->
    %% The effective version for decoding the server hello message should be the TLS-1.3. Possible coalesced TLS-1.2
    %% server handshake messages should be decoded with the negotiated version in later state.
    <<_:8, ?UINT24(Length), _/binary>> = Data,
    <<FirstPacket:(Length+4)/binary, RecordRest/binary>> = Data,
    {HSPacket, <<>> = NewHsBuffer} = tls_handshake:get_tls_handshakes(Version, FirstPacket, Buffer, Options),
    {HSPacket, NewHsBuffer, RecordRest};
%% TLS-1.3 RetryRequest
handle_unnegotiated_version(?TLS_1_2 , #{versions := [?TLS_1_3 = Version |_]} = Options, Data, Buffer, client, wait_sh) ->
    tls_handshake:get_tls_handshakes(Version, Data, Buffer, Options);
%% When the `negotiated_version` variable is not yet set use the highest supported version.
handle_unnegotiated_version(undefined, #{versions := [Version|_]} = Options, Data, Buff, _, _) ->
    tls_handshake:get_tls_handshakes(Version, Data, Buff, Options);
%% In all other cases use the version saved in the connection state variable `negotiated_version`
handle_unnegotiated_version(Version, Options, Data, Buff, _, _) ->
    tls_handshake:get_tls_handshakes(Version, Data, Buff, Options).

assert_buffer_sanity(<<?BYTE(_Type), ?UINT24(Length), Rest/binary>>, 
                     #{max_handshake_size := Max}) when
      Length =< Max ->  
    case byte_size(Rest) of
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
    case byte_size(Bin) of
        N when N < 3 ->
            true;
        _ ->       
            throw(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE, 
                             malformed_handshake_data))
    end.  

decode_alerts(Bin) ->
    ssl_alert:decode(Bin).

handle_alerts([], Result) ->
    Result;
handle_alerts(_, {stop, _, _} = Stop) ->
    Stop;
handle_alerts([#alert{level = ?WARNING, description = ?CLOSE_NOTIFY} | _Alerts], 
              {next_state, connection = StateName, #state{connection_env = CEnv, 
                                                          socket_options = #socket_options{active = false},
                                                          start_or_recv_from = From} = State}) when From == undefined ->
    {next_state, StateName, State#state{connection_env = CEnv#connection_env{socket_tls_closed = true}}};
handle_alerts([Alert | Alerts], {next_state, StateName, State}) ->
     handle_alerts(Alerts, ssl_gen_statem:handle_alert(Alert, StateName, State));
handle_alerts([Alert | Alerts], {next_state, StateName, State, _Actions}) ->
     handle_alerts(Alerts, ssl_gen_statem:handle_alert(Alert, StateName, State)).

handle_record_alert(Alert, _) ->
    Alert.

