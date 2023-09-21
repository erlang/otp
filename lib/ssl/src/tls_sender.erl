%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2023. All Rights Reserved.
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

-module(tls_sender).

-behaviour(gen_statem).

-include("ssl_internal.hrl").
-include("ssl_alert.hrl").
-include("ssl_handshake.hrl").
-include("ssl_api.hrl").
-include("ssl_record.hrl").
-include("tls_handshake_1_3.hrl").

%% API
-export([start_link/0,
         start_link/1,
         initialize/2,
         send_data/2,
         send_post_handshake/2,
         send_alert/2,
         send_and_ack_alert/2,
         setopts/2,
         renegotiate/1,
         peer_renegotiate/1,
         downgrade/2,
         update_connection_state/3,
         dist_tls_socket/1,
         dist_handshake_complete/3]).

%% gen_statem callbacks
-export([callback_mode/0,
         init/1,
         terminate/3,
         code_change/4]).
-export([init/3,
         connection/3,
         handshake/3,
         death_row/3]).
%% Tracing
-export([handle_trace/3]).

-record(static,
        {connection_pid,
         role,
         socket,
         socket_options,
         erl_dist,
         trackers,
         transport_cb,
         negotiated_version,
         renegotiate_at,
         key_update_at,  %% TLS 1.3
         bytes_sent,     %% TLS 1.3
         dist_handle,
         log_level,
         hibernate_after
        }).

-record(data,
        {static = #static{},
         connection_states = #{}
        }).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
                 ignore |
                 {error, Error :: term()}.
-spec start_link(list()) -> {ok, Pid :: pid()} |
                       ignore |
                       {error, Error :: term()}.

%%  Description: Start sender process to avoid dead lock that 
%%  may happen when a socket is busy (busy port) and the
%%  same process is sending and receiving 
%%--------------------------------------------------------------------
start_link() ->
    gen_statem:start_link(?MODULE, [], []).
start_link(SpawnOpts) ->
    gen_statem:start_link(?MODULE, [], SpawnOpts).

%%--------------------------------------------------------------------
-spec initialize(pid(), map()) -> ok. 
%%  Description: So TLS connection process can initialize it sender
%% process.
%%--------------------------------------------------------------------
initialize(Pid, InitMsg) ->
    gen_statem:call(Pid, {self(), InitMsg}).

%%--------------------------------------------------------------------
-spec send_data(pid(), iodata()) -> ok | {error, term()}.
%%  Description: Send application data
%%--------------------------------------------------------------------
send_data(Pid, AppData) ->
    %% Needs error handling for external API
    call(Pid, {application_data, AppData}).

%%--------------------------------------------------------------------
-spec send_post_handshake(pid(), #key_update{}) -> ok | {error, term()}.
%%  Description: Send post handshake data
%%--------------------------------------------------------------------
send_post_handshake(Pid, HandshakeData) ->
    call(Pid, {post_handshake_data, HandshakeData}).

%%--------------------------------------------------------------------
-spec send_alert(pid(), #alert{}) -> _. 
%% Description: TLS connection process wants to send an Alert
%% in the connection state.
%%--------------------------------------------------------------------
send_alert(Pid, Alert) ->
    gen_statem:cast(Pid, Alert).

%%--------------------------------------------------------------------
-spec send_and_ack_alert(pid(), #alert{}) -> _.
%% Description: TLS connection process wants to send an Alert
%% in the connection state and receive an ack.
%%--------------------------------------------------------------------
send_and_ack_alert(Pid, Alert) ->
    gen_statem:call(Pid, {ack_alert, Alert}, ?DEFAULT_TIMEOUT).
%%--------------------------------------------------------------------
-spec setopts(pid(), [{packet, integer() | atom()}]) -> ok | {error, term()}.
%%  Description: Send application data
%%--------------------------------------------------------------------
setopts(Pid, Opts) ->
    call(Pid, {set_opts, Opts}).

%%--------------------------------------------------------------------
-spec renegotiate(pid()) -> {ok, WriteState::map()} | {error, closed}.
%% Description: So TLS connection process can synchronize the 
%% encryption state to be used when handshaking.
%%--------------------------------------------------------------------
renegotiate(Pid) ->
    %% Needs error handling for external API
    call(Pid, renegotiate).

%%--------------------------------------------------------------------
-spec peer_renegotiate(pid()) -> {ok, WriteState::map()} | {error, term()}.
%% Description: So TLS connection process can synchronize the 
%% encryption state to be used when handshaking.
%%--------------------------------------------------------------------
peer_renegotiate(Pid) ->
     gen_statem:call(Pid, renegotiate, ?DEFAULT_TIMEOUT).

%%--------------------------------------------------------------------
-spec update_connection_state(pid(), WriteState::map(), tls_record:tls_version()) -> ok. 
%% Description: So TLS connection process can synchronize the 
%% encryption state to be used when sending application data. 
%%--------------------------------------------------------------------
update_connection_state(Pid, NewState, Version) ->
    gen_statem:cast(Pid, {new_write, NewState, Version}).

%%--------------------------------------------------------------------
-spec downgrade(pid(), integer()) -> {ok, ssl_record:connection_state()}
                                         | {error, timeout}.
%% Description: So TLS connection process can synchronize the 
%% encryption state to be used when sending application data. 
%%--------------------------------------------------------------------
downgrade(Pid, Timeout) ->
    try gen_statem:call(Pid, downgrade, Timeout) of
        Result ->
            Result    
    catch
        _:_ ->
            {error, timeout}
    end.
%%--------------------------------------------------------------------
-spec dist_handshake_complete(pid(), node(), term()) -> ok. 
%%  Description: Erlang distribution callback 
%%--------------------------------------------------------------------
dist_handshake_complete(ConnectionPid, Node, DHandle) ->
    gen_statem:call(ConnectionPid, {dist_handshake_complete, Node, DHandle}).
%%--------------------------------------------------------------------
-spec dist_tls_socket(pid()) -> {ok, #sslsocket{}}. 
%%  Description: To enable distribution startup to get a proper "#sslsocket{}" 
%%--------------------------------------------------------------------
dist_tls_socket(Pid) ->
    gen_statem:call(Pid, dist_get_tls_socket).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
%%--------------------------------------------------------------------
callback_mode() -> 
    state_functions.

%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
                  gen_statem:init_result(atom()).
%%--------------------------------------------------------------------
init(_) ->
    %% As this process is now correctly supervised
    %% together with the connection process and the significant
    %% child mechanism we want to handle supervisor shutdown
    %% to achieve a normal shutdown avoiding SASL reports.
    process_flag(trap_exit, true),
    {ok, init, #data{}}.

%%--------------------------------------------------------------------
-spec init(gen_statem:event_type(),
           Msg :: term(),
           StateData :: term()) ->
                  gen_statem:event_handler_result(atom()).
%%--------------------------------------------------------------------
init({call, From}, {Pid, #{current_write := WriteState,
                           role := Role,
                           socket := Socket,
                           socket_options := SockOpts,
                           erl_dist := IsErlDist,
                           trackers := Trackers,
                           transport_cb := Transport,
                           negotiated_version := Version,
                           renegotiate_at := RenegotiateAt,
                           key_update_at := KeyUpdateAt,
                           log_level := LogLevel,
                           hibernate_after := HibernateAfter}},
     #data{connection_states = ConnectionStates, static = Static0} = StateData0) ->
    StateData = 
        StateData0#data{connection_states = ConnectionStates#{current_write => WriteState},
                        static = Static0#static{connection_pid = Pid,
                                                role = Role,
                                                socket = Socket,
                                                socket_options = SockOpts,
                                                erl_dist = IsErlDist,
                                                trackers = Trackers,
                                                transport_cb = Transport,
                                                negotiated_version = Version,
                                                renegotiate_at = RenegotiateAt,
                                                key_update_at = KeyUpdateAt,
                                                bytes_sent = 0,
                                                log_level = LogLevel,
                                                hibernate_after = HibernateAfter}},
    {next_state, handshake, StateData, [{reply, From, ok}]};
init(info = Type, Msg, StateData) ->
    handle_common(?FUNCTION_NAME, Type, Msg, StateData);
init(_, _, _) ->
    %% Just in case anything else sneaks through
    {keep_state_and_data, [postpone]}.

%%--------------------------------------------------------------------
-spec connection(gen_statem:event_type(),
           Msg :: term(),
           StateData :: term()) ->
                  gen_statem:event_handler_result(atom()).
%%--------------------------------------------------------------------
connection({call, From}, {application_data, AppData}, 
           #data{static = #static{socket_options = #socket_options{packet = Packet}}} =
               StateData) ->
    case encode_packet(Packet, AppData) of
        {error, _} = Error ->
            {next_state, ?FUNCTION_NAME, StateData, [{reply, From, Error}]};
        Data ->
            send_application_data(Data, From, ?FUNCTION_NAME, StateData)
    end;
connection({call, From}, {post_handshake_data, HSData}, StateData) ->
    send_post_handshake_data(HSData, From, ?FUNCTION_NAME, StateData);
connection({call, From}, {ack_alert, #alert{} = Alert}, StateData0) ->
    StateData = send_tls_alert(Alert, StateData0),
    {next_state, ?FUNCTION_NAME, StateData,
     [{reply,From,ok}]};
connection({call, From}, renegotiate,
           #data{connection_states = #{current_write := Write}} = StateData) ->
    {next_state, handshake, StateData, [{reply, From, {ok, Write}}]};
connection({call, From}, downgrade, #data{connection_states =
                                              #{current_write := Write}} = StateData) ->
    {next_state, death_row, StateData, [{reply,From, {ok, Write}}]};
connection({call, From}, {set_opts, Opts}, StateData) ->
    handle_set_opts(?FUNCTION_NAME, From, Opts, StateData);
connection({call, From}, dist_get_tls_socket, 
           #data{static = #static{transport_cb = Transport,
                                  socket = Socket,
                                  connection_pid = Pid,
                                  trackers = Trackers}} = StateData) ->
    TLSSocket = tls_gen_connection:socket([Pid, self()], Transport, Socket, Trackers),
    hibernate_after(?FUNCTION_NAME, StateData, [{reply, From, {ok, TLSSocket}}]);
connection({call, From}, {dist_handshake_complete, _Node, DHandle},
           #data{static = #static{connection_pid = Pid} = Static} = StateData) ->
    false = erlang:dist_ctrl_set_opt(DHandle, get_size, true),
    ok = erlang:dist_ctrl_input_handler(DHandle, Pid),
    ok = ssl_gen_statem:dist_handshake_complete(Pid, DHandle),
    %% From now on we execute on normal priority
    process_flag(priority, normal),

    case dist_data(DHandle) of
        [] ->
            hibernate_after(?FUNCTION_NAME,
                            StateData#data{
                              static = Static#static{dist_handle = DHandle}},
                            [{reply,From,ok}]);
        Data ->
            {keep_state,
             StateData#data{static = Static#static{dist_handle = DHandle}},
             [{reply,From,ok},
              {next_event, internal,
               {application_packets, {self(),undefined}, Data}}]}
    end;
connection({call, From}, get_application_traffic_secret, State) ->
    CurrentWrite = maps:get(current_write, State#data.connection_states),
    SecurityParams = maps:get(security_parameters, CurrentWrite),
    ApplicationTrafficSecret =
        SecurityParams#security_parameters.application_traffic_secret,
    hibernate_after(?FUNCTION_NAME, State,
                    [{reply, From, {ok, ApplicationTrafficSecret}}]);
connection(internal, {application_packets, From, Data}, StateData) ->
    send_application_data(Data, From, ?FUNCTION_NAME, StateData);
connection(internal, {post_handshake_data, From, HSData}, StateData) ->
    send_post_handshake_data(HSData, From, ?FUNCTION_NAME, StateData);
connection(cast, #alert{} = Alert, StateData0) ->
    StateData = send_tls_alert(Alert, StateData0),
    {next_state, ?FUNCTION_NAME, StateData};
connection(cast, {new_write, WritesState, Version}, 
           #data{connection_states = ConnectionStates, static = Static} = StateData) ->
    hibernate_after(connection,
                    StateData#data{connection_states =
                                       ConnectionStates#{current_write => WritesState},
                                   static =
                                       Static#static{negotiated_version = Version}}, []);
%%
connection(info, dist_data,
           #data{static = #static{dist_handle = DHandle}} = StateData) ->
      case dist_data(DHandle) of
          [] ->
              hibernate_after(?FUNCTION_NAME, StateData, []);
          Data ->
              {keep_state_and_data,
               [{next_event, internal,
                 {application_packets, {self(),undefined}, Data}}]}
      end;
connection(info, tick, StateData) ->  
    consume_ticks(),
    Data = [<<0:32>>], % encode_packet(4, <<>>)
    From = {self(), undefined},
    send_application_data(Data, From, ?FUNCTION_NAME, StateData);
connection(info, {send, From, Ref, Data}, _StateData) -> 
    %% This is for testing only!
    %%
    %% Needed by some OTP distribution
    %% test suites...
    From ! {Ref, ok},
    {keep_state_and_data,
     [{next_event, {call, {self(), undefined}},
       {application_data, erlang:iolist_to_iovec(Data)}}]};
connection(timeout, hibernate, _StateData) ->
    {keep_state_and_data, [hibernate]};
connection(Type, Msg, StateData) ->
    handle_common(?FUNCTION_NAME, Type, Msg, StateData).

%%--------------------------------------------------------------------
-spec handshake(gen_statem:event_type(),
                  Msg :: term(),
                  StateData :: term()) ->
                         gen_statem:event_handler_result(atom()).
%%--------------------------------------------------------------------
handshake({call, From}, {set_opts, Opts}, StateData) ->
    handle_set_opts(?FUNCTION_NAME, From, Opts, StateData);
handshake({call, _}, _, _) ->
    %% Postpone all calls to the connection state
    {keep_state_and_data, [postpone]};
handshake(internal, {application_packets,_,_}, _) ->
    {keep_state_and_data, [postpone]};
handshake(cast, {new_write, WriteState, Version},
          #data{connection_states = ConnectionStates,
                static = #static{key_update_at = KeyUpdateAt0} = Static} = StateData) ->
    KeyUpdateAt = key_update_at(Version, WriteState, KeyUpdateAt0),
    {next_state, connection, 
     StateData#data{connection_states = ConnectionStates#{current_write => WriteState},
                    static = Static#static{negotiated_version = Version,
                                           key_update_at = KeyUpdateAt}}};
handshake(info, dist_data, _) ->
    {keep_state_and_data, [postpone]};
handshake(info, tick, _) ->
    %% Ignore - data is sent anyway during handshake
    consume_ticks(),
    keep_state_and_data;
handshake(info, {send, _, _, _}, _) ->
    %% Testing only, OTP distribution test suites...
    {keep_state_and_data, [postpone]};
handshake(Type, Msg, StateData) ->
    handle_common(?FUNCTION_NAME, Type, Msg, StateData).

%%--------------------------------------------------------------------
-spec death_row(gen_statem:event_type(),
                Msg :: term(),
                StateData :: term()) ->
                       gen_statem:event_handler_result(atom()).
%%--------------------------------------------------------------------
death_row(state_timeout, Reason, _StateData) ->
    {stop, {shutdown, Reason}};
death_row(info = Type, Msg, StateData) ->
    handle_common(?FUNCTION_NAME, Type, Msg, StateData);
death_row(_Type, _Msg, _StateData) ->
    %% Waste all other events
    keep_state_and_data.

%% State entry function that starts shutdown state_timeout
death_row_shutdown(Reason, StateData) ->
    {next_state, death_row, StateData, [{state_timeout, 5000, Reason}]}.

%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
                       any().
%%--------------------------------------------------------------------
terminate(_Reason, _State, _Data) ->
    void.

%%--------------------------------------------------------------------
-spec code_change(
        OldVsn :: term() | {down,term()},
        State :: term(), Data :: term(), Extra :: term()) ->
                         {ok, NewState :: term(), NewData :: term()} |
                         (Reason :: term()).
%% Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_set_opts(StateName, From, Opts,
                #data{static = #static{socket_options = SockOpts} = Static}
                = StateData) ->
    hibernate_after(StateName,
                    StateData#data{
                      static =
                          Static#static{
                            socket_options = set_opts(SockOpts, Opts)}},
                    [{reply, From, ok}]).

handle_common(StateName, {call, From}, {set_opts, Opts},
  #data{static = #static{socket_options = SockOpts} = Static} = StateData) ->
    hibernate_after(StateName,
                    StateData#data{
                      static =
                          Static#static{
                            socket_options = set_opts(SockOpts, Opts)}},
     [{reply, From, ok}]);
handle_common(_StateName, info, {'EXIT', _Sup, shutdown = Reason},
              #data{static = #static{erl_dist = true}} = StateData) ->
    %% When the connection is on its way down operations
    %% begin to fail. We wait to receive possible exit signals
    %% for one of our links to the other involved distribution parties,
    %% in which case we want to use their exit reason
    %% for the connection teardown.
    death_row_shutdown(Reason, StateData);
handle_common(_StateName, info, {'EXIT', _Dist, Reason},
              #data{static = #static{erl_dist = true}} = StateData) ->
    {stop, {shutdown, Reason}, StateData};
handle_common(_StateName, info, {'EXIT', _Sup, shutdown}, StateData) ->
    {stop, shutdown, StateData};
handle_common(StateName, info, Msg,
              #data{static = #static{log_level = Level}} = StateData) ->
    ssl_logger:log(info, Level, #{event => "TLS sender received unexpected info",
                                  reason => [{message, Msg}]}, ?LOCATION),
    hibernate_after(StateName, StateData, []);
handle_common(StateName, Type, Msg,
              #data{static = #static{log_level = Level}} = StateData) ->
    ssl_logger:log(error, Level, #{event => "TLS sender received unexpected event",
                                   reason => [{type, Type}, {message, Msg}]}, ?LOCATION),
    hibernate_after(StateName, StateData, []).

send_tls_alert(#alert{} = Alert,
               #data{static = #static{negotiated_version = Version,
                                      socket = Socket,
                                      transport_cb = Transport,
                                      log_level = LogLevel},
                     connection_states = ConnectionStates0} = StateData0) ->
    {BinMsg, ConnectionStates} =
	tls_record:encode_alert_record(Alert, Version, ConnectionStates0),
    tls_socket:send(Transport, Socket, BinMsg),
    ssl_logger:debug(LogLevel, outbound, 'record', BinMsg),
    StateData0#data{connection_states = ConnectionStates}.

send_application_data(Data, From, StateName,
		       #data{static = #static{connection_pid = Pid,
                                              socket = Socket,
                                              dist_handle = DistHandle,
                                              negotiated_version = Version,
                                              transport_cb = Transport,
                                              renegotiate_at = RenegotiateAt,
                                              key_update_at = KeyUpdateAt,
                                              bytes_sent = BytesSent,
                                              log_level = LogLevel},
                             connection_states = ConnectionStates0} = StateData0) ->
    case time_to_rekey(Version, Data, ConnectionStates0, RenegotiateAt, KeyUpdateAt, BytesSent) of
        key_update ->
            KeyUpdate = tls_handshake_1_3:key_update(update_requested),
            {keep_state_and_data, [{next_event, internal, {post_handshake_data, From, KeyUpdate}},
                                   {next_event, internal, {application_packets, From, Data}}]};
	renegotiate ->
	    tls_dtls_connection:internal_renegotiation(Pid, ConnectionStates0),
            {next_state, handshake, StateData0, 
             [{next_event, internal, {application_packets, From, Data}}]};
        chunk_and_key_update ->
            KeyUpdate = tls_handshake_1_3:key_update(update_requested),
            %% Prevent infinite loop of key updates
            {Chunk, Rest} = chunk_data(Data, KeyUpdateAt),
            {keep_state_and_data, [{next_event, internal, {post_handshake_data, From, KeyUpdate}},
                                   {next_event, internal, {application_packets, From, Chunk}},
                                   {next_event, internal, {application_packets, From, Rest}}]};
	false ->
	    {Msgs, ConnectionStates} = tls_record:encode_data(Data, Version, ConnectionStates0),
            StateData = StateData0#data{connection_states = ConnectionStates},
	    case tls_socket:send(Transport, Socket, Msgs) of
                ok when DistHandle =/=  undefined ->
                    ssl_logger:debug(LogLevel, outbound, 'record', Msgs),
                    StateData1 = update_bytes_sent(Version, StateData, Data),
                    hibernate_after(StateName, StateData1, []);
                Reason when DistHandle =/= undefined ->
                    death_row_shutdown(Reason, StateData);
                ok ->
                    ssl_logger:debug(LogLevel, outbound, 'record', Msgs),
                    StateData1 = update_bytes_sent(Version, StateData, Data),
                    hibernate_after(StateName, StateData1, [{reply, From, ok}]);
                Result ->
                    hibernate_after(StateName, StateData, [{reply, From, Result}])
            end
    end.

%% TLS 1.3 Post Handshake Data
send_post_handshake_data(Handshake, From, StateName,
                         #data{static = #static{socket = Socket,
                                                dist_handle = DistHandle,
                                                negotiated_version = Version,
                                                transport_cb = Transport,
                                                log_level = LogLevel},
                               connection_states = ConnectionStates0} = StateData0) ->
    BinHandshake = tls_handshake:encode_handshake(Handshake, Version),
    {Encoded, ConnectionStates} =
        tls_record:encode_handshake(BinHandshake, Version, ConnectionStates0),
    ssl_logger:debug(LogLevel, outbound, 'handshake', Handshake),
    StateData1 = StateData0#data{connection_states = ConnectionStates},
    case tls_socket:send(Transport, Socket, Encoded) of
        ok when DistHandle =/=  undefined ->
            ssl_logger:debug(LogLevel, outbound, 'record', Encoded),
            StateData = maybe_update_cipher_key(StateData1, Handshake),
            {next_state, StateName, StateData, []};
        Reason when DistHandle =/= undefined ->
            death_row_shutdown(Reason, StateData1);
        ok ->
            ssl_logger:debug(LogLevel, outbound, 'record', Encoded),
            StateData = maybe_update_cipher_key(StateData1, Handshake),
            {next_state, StateName, StateData,  [{reply, From, ok}]};
        Result ->
            {next_state, StateName, StateData1,  [{reply, From, Result}]}
    end.

maybe_update_cipher_key(#data{connection_states = ConnectionStates0,
                              static = Static0} = StateData, #key_update{}) ->
    ConnectionStates = tls_gen_connection_1_3:update_cipher_key(current_write, ConnectionStates0),
    Static = Static0#static{bytes_sent = 0},
    StateData#data{connection_states = ConnectionStates,
                   static = Static};
maybe_update_cipher_key(StateData, _) ->
    StateData.

update_bytes_sent(Version, StateData, _) when ?TLS_LT(Version, ?TLS_1_3) ->
    StateData;
%% Count bytes sent in TLS 1.3 for AES-GCM
update_bytes_sent(_, #data{static = #static{key_update_at = seq_num_wrap}} = StateData, _) ->
    StateData;  %% Chacha20-Poly1305
update_bytes_sent(_, #data{static = #static{bytes_sent = Sent} = Static} = StateData, Data) ->
    StateData#data{static = Static#static{bytes_sent = Sent + iolist_size(Data)}}.  %% AES-GCM

%% For AES-GCM, up to 2^24.5 full-size records (about 24 million) may be
%% encrypted on a given connection while keeping a safety margin of
%% approximately 2^-57 for Authenticated Encryption (AE) security.  For
%% ChaCha20/Poly1305, the record sequence number would wrap before the
%% safety limit is reached.
key_update_at(?TLS_1_3, #{security_parameters :=
                             #security_parameters{
                                bulk_cipher_algorithm = ?CHACHA20_POLY1305}}, _KeyUpdateAt) ->
    seq_num_wrap;
key_update_at(?TLS_1_3, _, KeyUpdateAt) ->
    KeyUpdateAt;
key_update_at(_, _, KeyUpdateAt) ->
    KeyUpdateAt.

-compile({inline, encode_packet/2}).
encode_packet(Packet, Data) ->
    Len = iolist_size(Data),
    case Packet of
        1 when Len < (1 bsl 8) ->  [<<Len:8>>|Data];
        2 when Len < (1 bsl 16) -> [<<Len:16>>|Data];
        4 when Len < (1 bsl 32) -> [<<Len:32>>|Data];
        N when N =:= 1; N =:= 2; N =:= 4 ->
            {error,
             {badarg, {packet_to_large, Len, (1 bsl (Packet bsl 3)) - 1}}};
        _ ->
            Data
    end.

set_opts(SocketOptions, [{packet, N}]) ->
    SocketOptions#socket_options{packet = N}.

time_to_rekey(Version, _Data,
              #{current_write := #{sequence_number := ?MAX_SEQUENCE_NUMBER}},
              _, _, _) when ?TLS_GTE(Version, ?TLS_1_3) ->
    key_update;
time_to_rekey(Version, _Data, _, _, seq_num_wrap, _) when ?TLS_GTE(Version, ?TLS_1_3) ->
    false;
time_to_rekey(Version, Data, _, _, KeyUpdateAt, BytesSent) when ?TLS_GTE(Version, ?TLS_1_3) ->
    DataSize = iolist_size(Data),
    case (BytesSent + DataSize) > KeyUpdateAt of
        true ->
            %% Handle special case that causes an invite loop of key updates.
            case DataSize > KeyUpdateAt of
                true ->
                    chunk_and_key_update;
                false ->
                    key_update
                end;
        false ->
            false
    end;
time_to_rekey(_, _Data,
              #{current_write := #{sequence_number := Num}},
              RenegotiateAt, _, _) ->
    
    %% We could do test:
    %% is_time_to_renegotiate((erlang:byte_size(_Data) div
    %% ?MAX_PLAIN_TEXT_LENGTH) + 1, RenegotiateAt), but we chose to
    %% have a some what lower renegotiateAt and a much cheaper test
    is_time_to_renegotiate(Num, RenegotiateAt).

chunk_data(Data, Size) ->
    {Chunk, Rest} = split_binary(iolist_to_binary(Data), Size),
    {[Chunk], [Rest]}.

is_time_to_renegotiate(N, M) when N < M->
    false;
is_time_to_renegotiate(_,_) ->
    renegotiate.

call(FsmPid, Event) ->
    try gen_statem:call(FsmPid, Event)
    catch
 	exit:{noproc, _} ->
 	    {error, closed};
	exit:{normal, _} ->
	    {error, closed};
	exit:{shutdown,_} ->
	    {error, closed};
        exit:{{shutdown, _},_} ->
	    {error, closed}
    end.

%%-------------- Erlang distribution helpers ------------------------------

%% To avoid livelock, dist_data/2 will check for more bytes coming from
%%  distribution channel, if amount of already collected bytes greater
%%  or equal than the limit defined below.
-define(TLS_BUNDLE_SOFT_LIMIT, 16 * 1024 * 1024).

dist_data(DHandle) ->
    dist_data(DHandle, 0).

dist_data(DHandle, CurBytes) ->
    case erlang:dist_ctrl_get_data(DHandle) of
        none ->
            erlang:dist_ctrl_get_data_notification(DHandle),
            [];
        %% This is encode_packet(4, Data) without Len check
        %% since the emulator will always deliver a Data
        %% smaller than 4 GB, and the distribution will
        %% therefore always have to use {packet,4}
        {Len, Data} when Len + CurBytes >= ?TLS_BUNDLE_SOFT_LIMIT ->
            %% Data is of type iovec(); lets keep it
            %% as an iovec()...
            erlang:dist_ctrl_get_data_notification(DHandle),
            [<<Len:32>> | Data];
        {Len, Data} ->
            Packet = [<<Len:32>> | Data],
            case dist_data(DHandle, CurBytes + Len) of
                [] -> Packet;
                More -> Packet ++ More
            end
    end.


%% Empty the inbox from distribution ticks - do not let them accumulate
consume_ticks() ->
    receive tick -> 
            consume_ticks()
    after 0 -> 
            ok
    end.

hibernate_after(connection = StateName,
		#data{static=#static{hibernate_after = HibernateAfter}} = State,
		Actions) ->
    {next_state, StateName, State, [{timeout, HibernateAfter, hibernate} | Actions]};
hibernate_after(StateName, State, Actions) ->
    {next_state, StateName, State, Actions}.

%%%################################################################
%%%#
%%%# Tracing
%%%#
handle_trace(kdt,
             {call, {?MODULE, time_to_rekey,
                     [_Version, Data, Map, _RenegotiateAt,
                      KeyUpdateAt, BytesSent]}}, Stack) ->
    #{current_write := #{sequence_number := Sn}} = Map,
    DataSize = iolist_size(Data),
    {io_lib:format("~w) (BytesSent:~w + DataSize:~w) > KeyUpdateAt:~w",
                   [Sn, BytesSent, DataSize, KeyUpdateAt]), Stack};
handle_trace(kdt,
             {call, {?MODULE, send_post_handshake_data,
                     [{key_update, update_requested}|_]}}, Stack) ->
    {io_lib:format("KeyUpdate procedure 1/4 - update_requested sent", []), Stack};
handle_trace(kdt,
             {call, {?MODULE, send_post_handshake_data,
                     [{key_update, update_not_requested}|_]}}, Stack) ->
    {io_lib:format("KeyUpdate procedure 3/4 - update_not_requested sent", []), Stack};
handle_trace(hbn,
             {call, {?MODULE, connection,
                     [timeout, hibernate | _]}}, Stack) ->
    {io_lib:format("* * * hibernating * * *", []), Stack};
handle_trace(hbn,
                 {call, {?MODULE, hibernate_after,
                         [_StateName = connection, State, Actions]}},
             Stack) ->
    #data{static=#static{hibernate_after = HibernateAfter}} = State,
    {io_lib:format("* * * maybe hibernating in ~w ms * * * Actions = ~W ",
                   [HibernateAfter, Actions, 10]), Stack};
handle_trace(hbn,
                 {return_from, {?MODULE, hibernate_after, 3},
                  {Cmd, Arg,_State, Actions}},
             Stack) ->
    {io_lib:format("Cmd = ~w Arg = ~w Actions = ~W", [Cmd, Arg, Actions, 10]), Stack};
handle_trace(rle,
                 {call, {?MODULE, init, [Type, Opts, _StateData]}}, Stack0) ->
    {Pid, #{role := Role,
            socket := _Socket,
            key_update_at := KeyUpdateAt,
            erl_dist := IsErlDist,
            trackers := Trackers,
            negotiated_version := _Version}} = Opts,
    {io_lib:format("(*~w) Type = ~w Pid = ~w Trackers = ~w Dist = ~w KeyUpdateAt = ~w",
                   [Role, Type, Pid, Trackers, IsErlDist, KeyUpdateAt]),
     [{role, Role} | Stack0]}.
