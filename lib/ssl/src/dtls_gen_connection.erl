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
-module(dtls_gen_connection).

-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/logger.hrl").

-include("dtls_connection.hrl").
-include("dtls_handshake.hrl").
-include("ssl_alert.hrl").
-include("dtls_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_api.hrl").
-include("ssl_internal.hrl").

%% Setup
-export([start_fsm/8,
         pids/1]).

%% Handshake handling
-export([send_handshake/2,
         send_handshake_flight/2,
	 queue_handshake/2,
         queue_change_cipher/2,
	 reinit/1,
         reinit_handshake_data/1,
         select_sni_extension/1,
         empty_connection_state/2]).

%% State transition handling	 
-export([next_event/3,
         next_event/4,
         handle_protocol_record/3,
         new_flight/0,
         initial_flight_state/1
        ]).

%% Data handling
-export([send/3,
         socket/4,
         setopts/3,
         getopts/3,
         handle_info/3]).

%% Alert and close handling
-export([send_alert/2,
         send_alert_in_connection/2,
         close/4,
         protocol_name/0]).


%% See thread @ http://lists.cluenet.de/pipermail/ipv6-ops/2011-June/005755.html
%% 1280 - headers
-define(PMTUEstimate_UDP, 1200).
-define(PMTUEstimate_SCTP, 16384). % 2^14  RFC 6083

%%====================================================================
%% Internal application API
%%====================================================================	     
%%====================================================================
%% Setup
%%====================================================================
start_fsm(Role, Host, Port, Socket, {_,_, Tracker} = Opts,
	  User, {CbModule, _, _, _, _} = CbInfo,
	  Timeout) ->
    try
	{ok, Pid} = dtls_connection_sup:start_child([Role, Host, Port, Socket, 
						     Opts, User, CbInfo]), 
	{ok, SslSocket} = ssl_gen_statem:socket_control(?MODULE, Socket, [Pid], CbModule, Tracker),
	ssl_gen_statem:handshake(SslSocket, Timeout)
    catch
	error:{badmatch, {error, _} = Error} ->
	    Error
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
		   connection_states = #{current_read := #{epoch := Epoch}} = ConnectionStates,
                   static_env = #static_env{data_tag = DataTag}} = State) ->
    CurrentRead = dtls_record:get_connection_state_by_epoch(Epoch, ConnectionStates, read),
    case is_reliable(DataTag) of
        true ->
            decode_cipher_text(State) ;
        false ->
            case dtls_record:replay_detect(CT, CurrentRead) of
                false ->
                    decode_cipher_text(State);
                true ->
                    %% Ignore replayed record
                    next_record(State#state{protocol_buffers = Buffers#protocol_buffers{dtls_cipher_texts = Rest}})
            end
    end;
next_record(#state{protocol_buffers =
		       #protocol_buffers{dtls_cipher_texts = [#ssl_tls{epoch = Epoch} | Rest]}
		   = Buffers,
		   connection_states = #{current_read := #{epoch := CurrentEpoch}}} = State)
  when Epoch > CurrentEpoch ->
    %% TODO Buffer later Epoch message, drop it for now
    next_record(State#state{protocol_buffers = Buffers#protocol_buffers{dtls_cipher_texts = Rest}});
next_record(#state{protocol_buffers = #protocol_buffers{dtls_cipher_texts =
                                                            [#ssl_tls{epoch = Epoch} | Rest]
                                                       } = Buffers
                  } = State) ->
    case Epoch of
        0 -> %% A reconnect (client might have rebooted and re-connected)
            decode_cipher_text(State);
        _ ->
            %% Drop old epoch message
            next_record(State#state{protocol_buffers = Buffers#protocol_buffers{dtls_cipher_texts = Rest}})
    end;
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

is_reliable(udp) ->
    false;
is_reliable(sctp) ->
    true.
   
next_event(StateName, Record, State) ->
    next_event(StateName, Record, State, []).

next_event(StateName, no_record,
	   #state{connection_states = #{current_read := #{epoch := CurrentEpoch}}} = State0, Actions) ->
    case next_record(State0) of
	{no_record, State} ->
            ssl_gen_statem:hibernate_after(StateName, State, Actions);
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
            handle_own_alert(Alert, StateName, State)
    end;
next_event(connection = StateName, Record,
	   #state{connection_states = #{current_read := #{epoch := CurrentEpoch}}} = State0, Actions) ->
    case Record of
        #ssl_tls{epoch = Epoch,
                 type = ?HANDSHAKE,
                 version = Version} = Record
          when Epoch =:= CurrentEpoch; Epoch =:= 0 ->
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
            handle_own_alert(Alert, StateName, State0)
    end.

initial_flight_state(udp)->
    {retransmit, ?INITIAL_RETRANSMIT_TIMEOUT};
initial_flight_state(_) ->
    reliable.

new_flight() ->
    #{next_sequence => 0,
      handshakes => [],
      change_cipher_spec => undefined,
      handshakes_after_change_cipher_spec => []}.

send_handshake_flight(#state{static_env = #static_env{socket = Socket,
                                                      transport_cb = Transport,
                                                      data_tag = DataTag}, 
                             connection_env = #connection_env{negotiated_version = Version},
                             flight_buffer = #{handshakes := Flight,
					       change_cipher_spec := undefined},
			     connection_states = ConnectionStates0,
                             ssl_options = #{log_level := LogLevel}} = State0,
                      Epoch) ->
    #{current_write := #{max_fragment_length := MaxFragmentLength}} = ConnectionStates0,
    MaxSize = mtu(MaxFragmentLength, DataTag),
    {Encoded, ConnectionStates} =
	encode_handshake_flight(lists:reverse(Flight), Version, MaxSize, Epoch, ConnectionStates0),
    send_packets(Transport, Socket, MaxSize, Encoded),
    ssl_logger:debug(LogLevel, outbound, 'record', Encoded),
   {State0#state{connection_states = ConnectionStates}, []};

send_handshake_flight(#state{static_env = #static_env{socket = Socket,
                                                      transport_cb = Transport,
                                                      data_tag = DataTag},
                             connection_env = #connection_env{negotiated_version = Version},
			     flight_buffer = #{handshakes := [_|_] = Flight0,
					       change_cipher_spec := ChangeCipher,
					       handshakes_after_change_cipher_spec := []},
			     connection_states = ConnectionStates0,
                             ssl_options = #{log_level := LogLevel}} = State0,
                      Epoch) ->
    #{current_write := #{max_fragment_length := MaxFragmentLength}} = ConnectionStates0,
    MaxSize = mtu(MaxFragmentLength, DataTag),
    {HsBefore, ConnectionStates1} =
	encode_handshake_flight(lists:reverse(Flight0), Version, MaxSize, Epoch, ConnectionStates0),
    {EncChangeCipher, ConnectionStates} = encode_change_cipher(ChangeCipher, Version, Epoch, ConnectionStates1),
    send_packets(Transport, Socket, MaxSize, HsBefore ++ EncChangeCipher),
    ssl_logger:debug(LogLevel, outbound, 'record', [HsBefore]),
    ssl_logger:debug(LogLevel, outbound, 'record', [EncChangeCipher]),
    {State0#state{connection_states = ConnectionStates}, []};

send_handshake_flight(#state{static_env = #static_env{socket = Socket,
                                                      transport_cb = Transport,
                                                     data_tag = DataTag},
                             connection_env = #connection_env{negotiated_version = Version},
			     flight_buffer = #{handshakes := [_|_] = Flight0,
					       change_cipher_spec := ChangeCipher,
					       handshakes_after_change_cipher_spec := Flight1},
			     connection_states = ConnectionStates0,
                             ssl_options = #{log_level := LogLevel}} = State0,
                      Epoch) ->
    #{current_write := #{max_fragment_length := MaxFragmentLength}} = ConnectionStates0,
    MaxSize = mtu(MaxFragmentLength, DataTag),
    {HsBefore, ConnectionStates1} =
	encode_handshake_flight(lists:reverse(Flight0), Version, MaxSize, Epoch-1, ConnectionStates0),
    {EncChangeCipher, ConnectionStates2} = 
	encode_change_cipher(ChangeCipher, Version, Epoch-1, ConnectionStates1),
    {HsAfter, ConnectionStates} =
	encode_handshake_flight(lists:reverse(Flight1), Version, MaxSize, Epoch, ConnectionStates2),
    send_packets(Transport, Socket, MaxSize, HsBefore ++ EncChangeCipher ++ HsAfter),
    ssl_logger:debug(LogLevel, outbound, 'record', [HsBefore]),
    ssl_logger:debug(LogLevel, outbound, 'record', [EncChangeCipher]),
    ssl_logger:debug(LogLevel, outbound, 'record', [HsAfter]),
    {State0#state{connection_states = ConnectionStates}, []};

send_handshake_flight(#state{static_env = #static_env{socket = Socket,
                                                      transport_cb = Transport,
                                                      data_tag = DataTag},
                             connection_env = #connection_env{negotiated_version = Version},
			     flight_buffer = #{handshakes := [],
					       change_cipher_spec := ChangeCipher,
					       handshakes_after_change_cipher_spec := Flight1},
			     connection_states = ConnectionStates0,
                             ssl_options = #{log_level := LogLevel}} = State0,
                      Epoch) ->
    #{current_write := #{max_fragment_length := MaxFragmentLength}} = ConnectionStates0,
    MaxSize = mtu(MaxFragmentLength, DataTag),
    {EncChangeCipher, ConnectionStates1} = 
	encode_change_cipher(ChangeCipher, Version, Epoch-1, ConnectionStates0),
    {HsAfter, ConnectionStates} =
	encode_handshake_flight(lists:reverse(Flight1), Version, MaxSize, Epoch, ConnectionStates1),
    send_packets(Transport, Socket, MaxSize, EncChangeCipher ++ HsAfter),
    ssl_logger:debug(LogLevel, outbound, 'record', [EncChangeCipher]),
    ssl_logger:debug(LogLevel, outbound, 'record', [HsAfter]),
    {State0#state{connection_states = ConnectionStates}, []}.


mtu(MaxFragmentLength, udp) ->
    min(MaxFragmentLength, ?PMTUEstimate_UDP);
mtu(MaxFragmentLength, sctp) ->
    min(MaxFragmentLength, ?PMTUEstimate_SCTP).

%%% DTLS record protocol level application data messages 

handle_protocol_record(#ssl_tls{type = ?APPLICATION_DATA, fragment = Data}, StateName0, State0) ->
    case ssl_gen_statem:read_application_data(Data, State0) of
	{stop, _, _} = Stop->
            Stop;
	{Record, State1} ->
            {next_state, StateName, State, Actions} = next_event(StateName0, Record, State1), 
            ssl_gen_statem:hibernate_after(StateName, State, Actions)
    end;
%%% DTLS record protocol level handshake messages 
handle_protocol_record(#ssl_tls{type = ?HANDSHAKE, epoch = Epoch, fragment = Data},
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
		HSEvents = dtls_handshake_events(Packets),
                Events = case is_new_connection(Epoch, Packets, State) of
                             true  -> [{next_event, internal, new_connection} | HSEvents];
                             false -> HSEvents
                         end,
                {next_state, StateName,
                 State#state{protocol_buffers = Buffers,
                             handshake_env =
                                 HsEnv#handshake_env{
                                   unprocessed_handshake_events = unprocessed_events(HSEvents)}
                            }, Events}
	end
    catch throw:#alert{} = Alert ->
	    handle_own_alert(Alert, StateName, State)
    end;
%%% DTLS record protocol level change cipher messages
handle_protocol_record(#ssl_tls{type = ?CHANGE_CIPHER_SPEC, fragment = Data}, StateName, State) ->
    {next_state, StateName, State, [{next_event, internal, #change_cipher_spec{type = Data}}]};
%%% DTLS record protocol level Alert messages
handle_protocol_record(#ssl_tls{type = ?ALERT, fragment = EncAlerts}, StateName, State) ->
    case decode_alerts(EncAlerts) of
	Alerts = [_|_] ->
	    handle_alerts(Alerts,  {next_state, StateName, State});
	#alert{} = Alert ->
	    handle_own_alert(Alert, StateName, State)
    end;
%% Ignore unknown TLS record level protocol messages
handle_protocol_record(#ssl_tls{type = _Unknown}, StateName, State) ->
    {next_state, StateName, State, []}.

%%====================================================================
%% Handshake handling
%%====================================================================	     
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

close(downgrade, _,_,_) ->
    ok;
%% Other
close(_, Socket, Transport, _) ->
    dtls_socket:close(Transport,Socket).

protocol_name() ->
    "DTLS".
        
%%====================================================================
%% Data handling
%%====================================================================	     
send(Transport, {Listener, Socket}, Data) when is_pid(Listener) -> 
    %% Server socket
    dtls_socket:send(Transport, Socket, Data);
send(Transport, Socket, Data) -> % Client socket
    dtls_socket:send(Transport, Socket, Data).

send_packets(_Transport, _Socket, _Max, []) ->
    ok;
send_packets(Transport, Socket, Max, Packets) ->
    {Packet, Rest} = pack_packets(Packets, 0, Max, []),
    case send(Transport, Socket, Packet) of
        ok -> send_packets(Transport, Socket, Max, Rest);
        Err -> Err
    end.

pack_packets([P|Rest]=Packets, SoFar, Max, Acc) ->
    Size = erlang:iolist_size(P),
    Next = SoFar + Size,
    if Size > Max, Acc =:= [] ->
            {P, Rest};
       Next < Max ->
            pack_packets(Rest, Next, Max, [P|Acc]);
       true ->
            {lists:reverse(Acc), Packets}
    end;
pack_packets([], _, _, Acc) ->
    {lists:reverse(Acc), []}.

socket(Pid,  Transport, Socket, _Tracker) ->
    dtls_socket:socket(Pid, Transport, Socket, ?MODULE).

setopts(Transport, Socket, Other) ->
    dtls_socket:setopts(Transport, Socket, Other).

getopts(Transport, Socket, Tag) ->
    dtls_socket:getopts(Transport, Socket, Tag).

%% raw data from socket, unpack records
handle_info({Protocol, _, _, _, Data}, StateName,
            #state{static_env = #static_env{data_tag = Protocol}} = State0) ->
    case next_dtls_record(Data, StateName, State0) of
	{Record, State} ->
	    next_event(StateName, Record, State);
	#alert{} = Alert ->
            handle_own_alert(Alert, StateName, State0)
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
            if (?DTLS_GTE(Version, ?DTLS_1_2)) ->
                    ok;
               true ->
                    %% As invalidate_sessions here causes performance issues,
                    %% we will conform to the widespread implementation
                    %% practice and go against the spec
                    %%invalidate_session(Role, Host, Port, Session)
                    ok
            end,
            Alert = ?ALERT_REC(?FATAL, ?CLOSE_NOTIFY, transport_closed),
            ssl_gen_statem:handle_normal_shutdown(Alert#alert{role = Role}, StateName, State),
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
handle_info({socket_reused, Client}, StateName,
            #state{static_env = #static_env{socket = {_, {Client, _}}}} = State) ->
    Alert = ?ALERT_REC(?FATAL, ?CLOSE_NOTIFY, transport_closed),
    ssl_gen_statem:handle_normal_shutdown(Alert#alert{role = server}, StateName, State),
    {stop, {shutdown, transport_closed}, State};

handle_info(Msg, StateName, State) ->
    ssl_gen_statem:handle_info(Msg, StateName, State).

%%====================================================================
%% Internal functions 
%%====================================================================

is_new_connection(0, [{#client_hello{},_Raw}|_],
                  #state{
                     connection_states =
                         #{current_read := #{epoch := CurrentEpoch}}})
  when CurrentEpoch > 0 ->
    true;
is_new_connection(_, _, _) ->
    false.

dtls_handshake_events(Packets) ->
    lists:map(fun(Packet) ->
		      {next_event, internal, {handshake, Packet}}
	      end, Packets).

unprocessed_events(Events) ->
    %% The first handshake event will be processed immediately
    %% as it is entered first in the event queue and
    %% when it is processed there will be length(Events)-1
    %% handshake events left to process before we should
    %% process more TLS-records received on the socket. 
    erlang:length(Events)-1.

encode_handshake_flight(Flight, Version, MaxFragmentSize, Epoch, ConnectionStates) ->
    Encode = fun(Fragment, {Acc, Cs0}) ->
                     {Enc, Cs} = dtls_record:encode_handshake(Fragment, Version, Epoch, Cs0),
                     {[Enc|Acc], Cs}
             end,
    {Rev, Cs} = lists:foldl(fun(Handshake, {Acc,Cs0}) ->
                                    Frags = dtls_handshake:fragment_handshake(Handshake, MaxFragmentSize),
                                    lists:foldl(Encode, {Acc,Cs0}, Frags)
                            end, {[], ConnectionStates}, Flight),
    {lists:reverse(Rev), Cs}.

encode_change_cipher(#change_cipher_spec{}, Version, Epoch, ConnectionStates) ->
    dtls_record:encode_change_cipher_spec(Version, Epoch, ConnectionStates).

update_handshake_history(#hello_verify_request{}, _, Hist) ->
    Hist;
update_handshake_history(_, Handshake, Hist) ->
    ssl_handshake:update_handshake_history(Hist, iolist_to_binary(Handshake)).

next_dtls_record(Data, StateName, #state{protocol_buffers = #protocol_buffers{
						   dtls_record_buffer = Buf0,
						   dtls_cipher_texts = CT0} = Buffers,
                                         connection_env = #connection_env{negotiated_version = Version},
                                         static_env = #static_env{data_tag = DataTag},
                                         ssl_options = SslOpts} = State0) ->
    case dtls_record:get_dtls_records(Data,
                                      {DataTag, StateName, Version, 
                                       [dtls_record:protocol_version_name(Vsn) || Vsn <- ?ALL_AVAILABLE_DATAGRAM_VERSIONS]},
                                      Buf0, SslOpts) of
	{Records, Buf1} ->
	    CT1 = CT0 ++ Records,
	    next_record(State0#state{protocol_buffers =
					 Buffers#protocol_buffers{dtls_record_buffer = Buf1,
								  dtls_cipher_texts = CT1}});
	#alert{} = Alert ->
	    Alert
    end.



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

decode_alerts(Bin) ->
    ssl_alert:decode(Bin).

handle_alerts([], Result) ->
    Result;
handle_alerts(_, {stop, _, _} = Stop) ->
    Stop;
handle_alerts(Alerts, {next_state, StateName, State}) ->
    handle_alerts_or_reset(Alerts, StateName, State);
handle_alerts(Alerts, {next_state, StateName, State, _Actions}) ->
    handle_alerts_or_reset(Alerts, StateName, State).

handle_alerts_or_reset([Alert|Alerts], StateName, #state{connection_states = Cs} = State) ->
    case maps:get(previous_cs, Cs, undefined) of
        undefined ->
            handle_alerts(Alerts, ssl_gen_statem:handle_alert(Alert, StateName, State));
        PreviousConn ->
            %% There exists an old connection and the new one sent alerts,
            %% reset to the old working one.
            HsEnv0 = State#state.handshake_env,
            HsEnv  = HsEnv0#handshake_env{renegotiation = undefined},
            NewState = State#state{connection_states = PreviousConn,
                                   handshake_env = HsEnv
                                  },
            {next_state, connection, NewState}
    end.


handle_own_alert(Alert, StateName,
                 #state{static_env = #static_env{data_tag = udp,
                                                 role = Role},
                        ssl_options = #{log_level := LogLevel}} = State0) ->
    case ignore_alert(Alert, State0) of
        {true, State} ->
            log_ignore_alert(LogLevel, StateName, Alert, Role),
            {next_state, StateName, State};
        {false, State} ->
            dtls_connection:alert_or_reset_connection(Alert, StateName, State)
    end;
handle_own_alert(Alert, StateName, State) ->
    dtls_connection:alert_or_reset_connection(Alert, StateName, State).

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

dtls_version(hello, Version, #state{static_env = #static_env{role = server},
                                    connection_env = CEnv} = State) ->
    State#state{connection_env = CEnv#connection_env{negotiated_version = Version}}; %%Initial version
dtls_version(_,_, State) ->
    State.
