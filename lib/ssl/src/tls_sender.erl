%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2018. All Rights Reserved.
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

%% API
-export([start/0, initialize/2, send_data/2, send_alert/2, renegotiate/1,
         update_connection_state/3]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([init/3, connection/3, handshake/3]).

-define(SERVER, ?MODULE).

-record(data, {connection_pid,
               connection_states = #{},
               socket,
               socket_options,
               protocol_cb,
               transport_cb,
               negotiated_version,
               renegotiate_at,
               connection_monitor
              }).

%%%===================================================================
%%% API
%%%===================================================================
-spec start() -> {ok, Pid :: pid()} |
                 ignore |
                 {error, Error :: term()}.
start() ->
    gen_statem:start(?MODULE, [], []).

initialize(Pid, InitMsg) ->
    gen_statem:call(Pid, {self(), InitMsg}).

send_data(Pid, AppData) ->
    %% Needs error handling for external API
    call(Pid, {application_data, AppData}).

send_alert(Pid, Alert) ->
    gen_statem:cast(Pid, Alert).

%%--------------------------------------------------------------------
-spec renegotiate(pid()) -> {ok, WriteState::map()} | {error, closed}.
%% Description: So TLS connection process can synchronize the 
%% encryption state to be used when handshaking.
%%--------------------------------------------------------------------
renegotiate(Pid) ->
    %% Needs error handling for external API
    call(Pid, renegotiate).
%%--------------------------------------------------------------------
-spec update_connection_state(pid(), WriteState::map(), tls_record:tls_version()) -> ok. 
%% Description: So TLS connection process can synchronize the 
%% encryption state to be used when sending application data. 
%%--------------------------------------------------------------------
update_connection_state(Pid, NewState, Version) ->
    gen_statem:cast(Pid, {new_write, NewState, Version}).

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
    {ok, init, #data{}}.

%%--------------------------------------------------------------------
-spec init(gen_statem:event_type(),
           Msg :: term(),
           StateData :: term()) ->
                  gen_statem:event_handler_result(atom()).
%%--------------------------------------------------------------------
init({call, From}, {Pid, #{current_write := WriteState,
                           socket := Socket,
                           socket_options := SockOpts,
                           protocol_cb := Connection,
                           transport_cb := Transport,
                           negotiated_version := Version,
                           renegotiate_at := RenegotiateAt}}, 
     #data{connection_states = ConnectionStates} = StateData0) ->    
    Monitor = erlang:monitor(process, Pid),
    StateData = 
        StateData0#data{connection_pid = Pid,
                        connection_monitor = Monitor,
                        connection_states = 
                            ConnectionStates#{current_write => WriteState},
                        socket = Socket,
                        socket_options = SockOpts,
                        protocol_cb = Connection,
                        transport_cb = Transport,
                        negotiated_version = Version,
                        renegotiate_at = RenegotiateAt},          
    {next_state, handshake, StateData, [{reply, From, ok}]};
init(info, Msg, StateData) -> 
    handle_info(Msg, ?FUNCTION_NAME, StateData).
%%--------------------------------------------------------------------
-spec connection(gen_statem:event_type(),
           Msg :: term(),
           StateData :: term()) ->
                  gen_statem:event_handler_result(atom()).
%%--------------------------------------------------------------------
connection({call, From}, renegotiate, 
           #data{connection_states = #{current_write := Write}} = StateData) ->
    {next_state, handshake, StateData, [{reply, From, {ok, Write}}]};
connection({call, From}, {application_data, AppData}, 
           #data{socket_options = SockOpts} = StateData) ->
    case encode_packet(AppData, SockOpts) of
        {error, _} = Error ->
            {next_state, ?FUNCTION_NAME, StateData, [{reply, From, Error}]};
        Data ->
            send_application_data(Data, From, ?FUNCTION_NAME, StateData)
    end;
connection(cast, #alert{} = Alert, StateData0) ->
    StateData = send_tls_alert(Alert, StateData0),
    {next_state, ?FUNCTION_NAME, StateData};
connection(cast, {new_write, WritesState, Version}, 
           #data{connection_states = ConnectionStates0} = StateData) -> 
    {next_state, connection, 
     StateData#data{connection_states = 
                        ConnectionStates0#{current_write => WritesState},
                    negotiated_version = Version}};
connection(info, Msg, StateData) -> 
    handle_info(Msg, ?FUNCTION_NAME, StateData).
%%--------------------------------------------------------------------
-spec handshake(gen_statem:event_type(),
                  Msg :: term(),
                  StateData :: term()) ->
                         gen_statem:event_handler_result(atom()).
%%--------------------------------------------------------------------
handshake({call, _}, _, _) ->
    {keep_state_and_data, [postpone]};
handshake(cast, {new_write, WritesState, Version}, 
          #data{connection_states = ConnectionStates0} = StateData) ->
    {next_state, connection, 
     StateData#data{connection_states = 
                        ConnectionStates0#{current_write => WritesState},
                   negotiated_version = Version}};
handshake(info, Msg, StateData) -> 
    handle_info(Msg, ?FUNCTION_NAME, StateData).

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
handle_info({'DOWN', Monitor, _, _, Reason}, _, 
            #data{connection_monitor = Monitor} = StateData) ->
    {stop, {shutdown, Reason}, StateData};
handle_info(_,_,_) ->
    {keep_state_and_data}.

send_tls_alert(Alert, #data{negotiated_version = Version,
                            socket = Socket,
                            protocol_cb = Connection, 
                            transport_cb = Transport,
                            connection_states = ConnectionStates0} = StateData0) ->
    {BinMsg, ConnectionStates} =
	Connection:encode_alert(Alert, Version, ConnectionStates0),
    Connection:send(Transport, Socket, BinMsg),
    StateData0#data{connection_states = ConnectionStates}.

send_application_data(Data, {FromPid, _} = From, StateName,
		       #data{connection_pid = Pid,
                             socket = Socket,
                             negotiated_version = Version,
                             protocol_cb = Connection,
                             transport_cb = Transport,
                             connection_states = ConnectionStates0,
                             renegotiate_at = RenegotiateAt} = StateData0) ->
    case time_to_renegotiate(Data, ConnectionStates0, RenegotiateAt) of
	true ->
	    ssl_connection:internal_renegotiation(Pid, ConnectionStates0), 
            {next_state, handshake, StateData0, 
             [{next_event, {call, From}, {application_data, Data}}]};
	false ->
	    {Msgs, ConnectionStates} =
                Connection:encode_data(Data, Version, ConnectionStates0),
            StateData = StateData0#data{connection_states = ConnectionStates},
	    case Connection:send(Transport, Socket, Msgs) of
                ok when FromPid =:= Pid ->
                    {next_state, StateName, StateData, []};
                Error when FromPid =:= Pid ->
                    ssl_connection:stop({shutdown, Error}, StateData);
                ok ->
                    {next_state, StateName, StateData,  [{reply, From, ok}]};
                Result ->
                    {next_state, StateName, StateData,  [{reply, From, Result}]}
            end
    end.

encode_packet(Data, #socket_options{packet=Packet}) ->
    case Packet of
	1 -> encode_size_packet(Data, 8,  (1 bsl 8) - 1);
	2 -> encode_size_packet(Data, 16, (1 bsl 16) - 1);
	4 -> encode_size_packet(Data, 32, (1 bsl 32) - 1);
	_ -> Data
    end.

encode_size_packet(Bin, Size, Max) ->
    Len = erlang:byte_size(Bin),
    case Len > Max of
	true  -> 
            {error, {badarg, {packet_to_large, Len, Max}}};
        false -> 
            <<Len:Size, Bin/binary>>
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
