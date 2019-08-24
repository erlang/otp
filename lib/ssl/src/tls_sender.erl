%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2019. All Rights Reserved.
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

%% API
-export([start/0, start/1, initialize/2, send_data/2, send_alert/2,
         send_and_ack_alert/2, setopts/2, renegotiate/1, peer_renegotiate/1, downgrade/2,
         update_connection_state/3, dist_tls_socket/1, dist_handshake_complete/3]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([init/3, connection/3, handshake/3, death_row/3]).

-define(SERVER, ?MODULE).

-record(static,
        {connection_pid,
         role,
         socket,
         socket_options,
         tracker,
         transport_cb,
         negotiated_version,
         renegotiate_at,
         connection_monitor,
         dist_handle,
         log_level
        }).

-record(data,
        {static = #static{},
         connection_states = #{}
        }).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
-spec start() -> {ok, Pid :: pid()} |
                 ignore |
                 {error, Error :: term()}.
-spec start(list()) -> {ok, Pid :: pid()} |
                       ignore |
                       {error, Error :: term()}.

%%  Description: Start sender process to avoid dead lock that 
%%  may happen when a socket is busy (busy port) and the
%%  same process is sending and receiving 
%%--------------------------------------------------------------------
start() ->
    gen_statem:start(?MODULE, [], []).
start(SpawnOpts) ->
    gen_statem:start(?MODULE, [], SpawnOpts).

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
-spec send_alert(pid(), #alert{}) -> _. 
%% Description: TLS connection process wants to send an Alert
%% in the connection state.
%%--------------------------------------------------------------------
send_alert(Pid, Alert) ->
    gen_statem:cast(Pid, Alert).

%%--------------------------------------------------------------------
-spec send_and_ack_alert(pid(), #alert{}) -> _.
%% Description: TLS connection process wants to send an Alert
%% in the connection state and recive an ack.
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


-define(HANDLE_COMMON,
        ?FUNCTION_NAME(Type, Msg, StateData) ->
               handle_common(Type, Msg, StateData)).
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
                  gen_statem:init_result(atom()).
%%--------------------------------------------------------------------
init(_) ->
    %% Note: Should not trap exits so that this process
    %% will be terminated if tls_connection process is
    %% killed brutally
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
                           tracker := Tracker,
                           transport_cb := Transport,
                           negotiated_version := Version,
                           renegotiate_at := RenegotiateAt,
                           log_level := LogLevel}},
     #data{connection_states = ConnectionStates, static = Static0} = StateData0) ->
    Monitor = erlang:monitor(process, Pid),
    StateData = 
        StateData0#data{connection_states = ConnectionStates#{current_write => WriteState},
                        static = Static0#static{connection_pid = Pid,
                                                connection_monitor = Monitor,
                                                role = Role,
                                                socket = Socket,
                                                socket_options = SockOpts,
                                                tracker = Tracker,
                                                transport_cb = Transport,
                                                negotiated_version = Version,
                                                renegotiate_at = RenegotiateAt,
                                                log_level = LogLevel}},
    {next_state, handshake, StateData, [{reply, From, ok}]};
init(_, _, _) ->
    %% Just in case anything else sneeks through
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
    handle_set_opts(From, Opts, StateData);
connection({call, From}, dist_get_tls_socket, 
           #data{static = #static{transport_cb = Transport,
                                  socket = Socket,
                                  connection_pid = Pid,
                                  tracker = Tracker}} = StateData) ->
    TLSSocket = tls_connection:socket([Pid, self()], Transport, Socket, Tracker),
    {next_state, ?FUNCTION_NAME, StateData, [{reply, From, {ok, TLSSocket}}]};
connection({call, From}, {dist_handshake_complete, _Node, DHandle},
           #data{static = #static{connection_pid = Pid} = Static} = StateData) ->
    ok = erlang:dist_ctrl_input_handler(DHandle, Pid),
    ok = ssl_connection:dist_handshake_complete(Pid, DHandle),
    %% From now on we execute on normal priority
    process_flag(priority, normal),
    {keep_state, StateData#data{static = Static#static{dist_handle = DHandle}},
     [{reply,From,ok}|
      case dist_data(DHandle) of
          [] ->
              [];
          Data ->
              [{next_event, internal,
               {application_packets,{self(),undefined},erlang:iolist_to_iovec(Data)}}]
      end]};
connection(internal, {application_packets, From, Data}, StateData) ->
    send_application_data(Data, From, ?FUNCTION_NAME, StateData);
%%
connection(cast, #alert{} = Alert, StateData0) ->
    StateData = send_tls_alert(Alert, StateData0),
    {next_state, ?FUNCTION_NAME, StateData};
connection(cast, {new_write, WritesState, Version}, 
           #data{connection_states = ConnectionStates, static = Static} = StateData) ->
    {next_state, connection, 
     StateData#data{connection_states = 
                        ConnectionStates#{current_write => WritesState},
                    static = Static#static{negotiated_version = Version}}};
%%
connection(info, dist_data, #data{static = #static{dist_handle = DHandle}}) ->
    {keep_state_and_data,
      case dist_data(DHandle) of
          [] ->
              [];
          Data ->
              [{next_event, internal,
               {application_packets,{self(),undefined},erlang:iolist_to_iovec(Data)}}]
      end};
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
?HANDLE_COMMON.

%%--------------------------------------------------------------------
-spec handshake(gen_statem:event_type(),
                  Msg :: term(),
                  StateData :: term()) ->
                         gen_statem:event_handler_result(atom()).
%%--------------------------------------------------------------------
handshake({call, From}, {set_opts, Opts}, StateData) ->
    handle_set_opts(From, Opts, StateData);
handshake({call, _}, _, _) ->
    %% Postpone all calls to the connection state
    {keep_state_and_data, [postpone]};
handshake(internal, {application_packets,_,_}, _) ->
    {keep_state_and_data, [postpone]};
handshake(cast, {new_write, WritesState, Version}, 
          #data{connection_states = ConnectionStates, static = Static} = StateData) ->
    {next_state, connection, 
     StateData#data{connection_states = ConnectionStates#{current_write => WritesState},
                    static = Static#static{negotiated_version = Version}}};
handshake(info, dist_data, _) ->
    {keep_state_and_data, [postpone]};
handshake(info, tick, _) ->
    %% Ignore - data is sent anyway during handshake
    consume_ticks(),
    keep_state_and_data;
handshake(info, {send, _, _, _}, _) ->
    %% Testing only, OTP distribution test suites...
    {keep_state_and_data, [postpone]};
?HANDLE_COMMON.

%%--------------------------------------------------------------------
-spec death_row(gen_statem:event_type(),
                Msg :: term(),
                StateData :: term()) ->
                       gen_statem:event_handler_result(atom()).
%%--------------------------------------------------------------------
death_row(state_timeout, Reason, _State) ->
    {stop, {shutdown, Reason}};
death_row(_Type, _Msg, _State) ->
    %% Waste all other events
    keep_state_and_data.

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

handle_set_opts(
  From, Opts, #data{static = #static{socket_options = SockOpts} = Static} = StateData) ->
    {keep_state, StateData#data{static = Static#static{socket_options = set_opts(SockOpts, Opts)}},
     [{reply, From, ok}]}.

handle_common(
  {call, From}, {set_opts, Opts},
  #data{static = #static{socket_options = SockOpts} = Static} = StateData) ->
    {keep_state, StateData#data{static = Static#static{socket_options = set_opts(SockOpts, Opts)}},
     [{reply, From, ok}]};
handle_common(
  info, {'DOWN', Monitor, _, _, Reason},
  #data{static = #static{connection_monitor = Monitor,
                         dist_handle = Handle}} = StateData) when Handle =/= undefined ->
    {next_state, death_row, StateData,
     [{state_timeout, 5000, Reason}]};
handle_common(
  info, {'DOWN', Monitor, _, _, _},
  #data{static = #static{connection_monitor = Monitor}} = StateData) ->
    {stop, normal, StateData};
handle_common(info, Msg, _) ->
    Report =
        io_lib:format("TLS sender: Got unexpected info: ~p ~n", [Msg]),
    error_logger:info_report(Report),
    keep_state_and_data;
handle_common(Type, Msg, _) ->
    Report =
        io_lib:format(
          "TLS sender: Got unexpected event: ~p ~n", [{Type,Msg}]),
    error_logger:error_report(Report),
    keep_state_and_data.

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
                                              log_level = LogLevel},
                             connection_states = ConnectionStates0} = StateData0) ->
    case time_to_renegotiate(Data, ConnectionStates0, RenegotiateAt) of
	true ->
	    ssl_connection:internal_renegotiation(Pid, ConnectionStates0),
            {next_state, handshake, StateData0, 
             [{next_event, internal, {application_packets, From, Data}}]};
	false ->
	    {Msgs, ConnectionStates} = tls_record:encode_data(Data, Version, ConnectionStates0),
            StateData = StateData0#data{connection_states = ConnectionStates},
	    case tls_socket:send(Transport, Socket, Msgs) of
                ok when DistHandle =/=  undefined ->
                    ssl_logger:debug(LogLevel, outbound, 'record', Msgs),
                    {next_state, StateName, StateData, []};
                Reason when DistHandle =/= undefined ->
                    {next_state, death_row, StateData, [{state_timeout, 5000, Reason}]};
                ok ->
                    ssl_logger:debug(LogLevel, outbound, 'record', Msgs),
                    {next_state, StateName, StateData,  [{reply, From, ok}]};
                Result ->
                    {next_state, StateName, StateData,  [{reply, From, Result}]}
            end
    end.

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

call(FsmPid, Event) ->
    try gen_statem:call(FsmPid, Event)
    catch
 	exit:{noproc, _} ->
 	    {error, closed};
	exit:{normal, _} ->
	    {error, closed};
	exit:{{shutdown, _},_} ->
	    {error, closed}
    end.

%%-------------- Erlang distribution helpers ------------------------------

dist_data(DHandle) ->
    case erlang:dist_ctrl_get_data(DHandle) of
        none ->
            erlang:dist_ctrl_get_data_notification(DHandle),
            [];
        %% This is encode_packet(4, Data) without Len check
        %% since the emulator will always deliver a Data
        %% smaller than 4 GB, and the distribution will
        %% therefore always have to use {packet,4}
        Data when is_binary(Data) ->
            Len = byte_size(Data),
            [[<<Len:32>>,Data]|dist_data(DHandle)];
        [BA,BB] = Data ->
            Len = byte_size(BA) + byte_size(BB),
            [[<<Len:32>>|Data]|dist_data(DHandle)];
        Data when is_list(Data) ->
            Len = iolist_size(Data),
            [[<<Len:32>>|Data]|dist_data(DHandle)]
    end.


%% Empty the inbox from distribution ticks - do not let them accumulate
consume_ticks() ->
    receive tick -> 
            consume_ticks()
    after 0 -> 
            ok
    end.
