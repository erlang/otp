%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2019. All Rights Reserved.
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
%% Purpose: Common handling of a TLS/SSL/DTLS connection, see also
%% tls_connection.erl and dtls_connection.erl
%%----------------------------------------------------------------------

-module(ssl_connection).

-include("ssl_api.hrl").
-include("ssl_connection.hrl").
-include("ssl_handshake.hrl").
-include("ssl_alert.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_internal.hrl").
-include("ssl_srp.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/logger.hrl").

%% Setup

-export([connect/8, handshake/7, handshake/2, handshake/3, handle_common_event/5,
         handshake_continue/3, handshake_cancel/1,
	 socket_control/4, socket_control/5]).

%% User Events 
-export([send/2, recv/3, close/2, shutdown/2,
	 new_user/2, get_opts/2, set_opts/2, 
	 peer_certificate/1, renegotiation/1, negotiated_protocol/1, prf/5,
	 connection_information/2
	]).

%% Alert and close handling
-export([handle_own_alert/4, handle_alert/3, 
	 handle_normal_shutdown/3, 
         handle_trusted_certs_db/1]).

%% Data handling
-export([read_application_data/2, internal_renegotiation/2]).

%% Help functions for tls|dtls_connection.erl
-export([handle_session/7, ssl_config/3,
	 prepare_connection/2, hibernate_after/3]).

%% General gen_statem state functions with extra callback argument 
%% to determine if it is an SSL/TLS or DTLS gen_statem machine
-export([init/4, error/4, hello/4, user_hello/4, abbreviated/4, certify/4, cipher/4,
         connection/4, downgrade/4]).

%% gen_statem callbacks
-export([terminate/3, format_status/2]).

%% Erlang Distribution export
-export([dist_handshake_complete/2]).

%%====================================================================
%% Setup
%%====================================================================
%%--------------------------------------------------------------------
-spec connect(tls_connection | dtls_connection,
	      ssl:host(), inet:port_number(), 
	      port() | {tuple(), port()}, %% TLS | DTLS  
	      {ssl_options(), #socket_options{},
	       %% Tracker only needed on server side
	       undefined},
	      pid(), tuple(), timeout()) ->
		     {ok, #sslsocket{}} | {error, reason()}.
%%
%% Description: Connect to an ssl server.
%%--------------------------------------------------------------------
connect(Connection, Host, Port, Socket, Options, User, CbInfo, Timeout) ->
    try Connection:start_fsm(client, Host, Port, Socket, Options, User, CbInfo,
			     Timeout)
    catch
	exit:{noproc, _} ->
	    {error, ssl_not_started}
    end.
%%--------------------------------------------------------------------
-spec handshake(tls_connection | dtls_connection,
		 inet:port_number(), port(),
		 {ssl_options(), #socket_options{}, undefined | pid()},
		 pid(), tuple(), timeout()) ->
			{ok, #sslsocket{}} | {error, reason()}.
%%
%% Description: Performs accept on an ssl listen socket. e.i. performs
%%              ssl handshake. 
%%--------------------------------------------------------------------
handshake(Connection, Port, Socket, Opts, User, CbInfo, Timeout) ->
    try Connection:start_fsm(server, "localhost", Port, Socket, Opts, User, 
		  CbInfo, Timeout)
    catch
	exit:{noproc, _} ->
	    {error, ssl_not_started}
    end.	

%%--------------------------------------------------------------------
-spec handshake(#sslsocket{}, timeout()) ->  {ok, #sslsocket{}} |
                                             {ok,  #sslsocket{}, map()}| {error, reason()}.
%%
%% Description: Starts ssl handshake. 
%%--------------------------------------------------------------------
handshake(#sslsocket{pid = [Pid|_]} = Socket, Timeout) ->  
    case call(Pid, {start, Timeout}) of
	connected ->
	    {ok, Socket};
        {ok, Ext} ->
            {ok, Socket, no_records(Ext)};
 	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
-spec handshake(#sslsocket{}, {ssl_options(),#socket_options{}}, timeout()) ->
                       {ok, #sslsocket{}} | {ok, #sslsocket{}, map()} | {error, reason()}.
%%
%% Description: Starts ssl handshake with some new options 
%%--------------------------------------------------------------------
handshake(#sslsocket{pid = [Pid|_]} = Socket, SslOptions, Timeout) ->  
    case call(Pid, {start, SslOptions, Timeout}) of
	connected ->
	    {ok, Socket};
 	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
-spec handshake_continue(#sslsocket{}, [ssl:tls_server_option()],
                         timeout()) ->  {ok,  #sslsocket{}}| {error, reason()}.
%%
%% Description: Continues handshake with new options
%%--------------------------------------------------------------------
handshake_continue(#sslsocket{pid = [Pid|_]} = Socket, SslOptions, Timeout) ->  
    case call(Pid, {handshake_continue, SslOptions, Timeout}) of
	connected ->
	    {ok, Socket};
 	Error ->
	    Error
    end.
%%--------------------------------------------------------------------
-spec handshake_cancel(#sslsocket{}) ->  ok | {error, reason()}.
%%
%% Description: Cancels connection
%%--------------------------------------------------------------------
handshake_cancel(#sslsocket{pid = [Pid|_]}) ->  
    case call(Pid, cancel) of
	closed ->
            ok;
        Error ->
	    Error
    end.
%--------------------------------------------------------------------
-spec socket_control(tls_connection | dtls_connection, port(), [pid()], atom()) -> 
    {ok, #sslsocket{}} | {error, reason()}.  
%%
%% Description: Set the ssl process to own the accept socket
%%--------------------------------------------------------------------	    
socket_control(Connection, Socket, Pid, Transport) ->
    socket_control(Connection, Socket, Pid, Transport, undefined).

%--------------------------------------------------------------------
-spec socket_control(tls_connection | dtls_connection, port(), [pid()], atom(), pid()| atom()) -> 
    {ok, #sslsocket{}} | {error, reason()}.  
%%--------------------------------------------------------------------	    
socket_control(Connection, Socket, Pids, Transport, udp_listener) ->
    %% dtls listener process must have the socket control
    {ok, Connection:socket(Pids, Transport, Socket, undefined)};

socket_control(tls_connection = Connection, Socket, [Pid|_] = Pids, Transport, ListenTracker) ->
    case Transport:controlling_process(Socket, Pid) of
	ok ->
	    {ok, Connection:socket(Pids, Transport, Socket, ListenTracker)};
	{error, Reason}	->
	    {error, Reason}
    end;
socket_control(dtls_connection = Connection, {_, Socket}, [Pid|_] = Pids, Transport, ListenTracker) ->
    case Transport:controlling_process(Socket, Pid) of
	ok ->
	    {ok, Connection:socket(Pids, Transport, Socket, ListenTracker)};
	{error, Reason}	->
	    {error, Reason}
    end.


%%====================================================================
%% User events
%%====================================================================

%%--------------------------------------------------------------------
-spec send(pid(), iodata()) -> ok | {error, reason()}.
%%
%% Description: Sends data over the ssl connection
%%--------------------------------------------------------------------
send(Pid, Data) -> 
    call(Pid, {application_data, 
				    %% iolist_to_iovec should really
				    %% be called iodata_to_iovec()
				    erlang:iolist_to_iovec(Data)}).

%%--------------------------------------------------------------------
-spec recv(pid(), integer(), timeout()) ->  
    {ok, binary() | list()} | {error, reason()}.
%%
%% Description:  Receives data when active = false
%%--------------------------------------------------------------------
recv(Pid, Length, Timeout) -> 
    call(Pid, {recv, Length, Timeout}).

%%--------------------------------------------------------------------
-spec connection_information(pid(), boolean()) -> {ok, list()} | {error, reason()}.
%%
%% Description: Get the SNI hostname
%%--------------------------------------------------------------------
connection_information(Pid, IncludeSecrityInfo) when is_pid(Pid) ->
    call(Pid, {connection_information, IncludeSecrityInfo}).

%%--------------------------------------------------------------------
-spec close(pid(), {close, Timeout::integer() | 
				    {NewController::pid(), Timeout::integer()}}) -> 
		   ok | {ok, port()} | {error, reason()}.  
%%
%% Description:  Close an ssl connection
%%--------------------------------------------------------------------
close(ConnectionPid, How) ->
    case call(ConnectionPid, How) of
	{error, closed} ->
	    ok;
	Other ->
	    Other
    end.
%%--------------------------------------------------------------------
-spec shutdown(pid(), atom()) -> ok | {error, reason()}.  
%%
%% Description: Same as gen_tcp:shutdown/2
%%--------------------------------------------------------------------
shutdown(ConnectionPid, How) ->
    call(ConnectionPid, {shutdown, How}).

%%--------------------------------------------------------------------
-spec new_user(pid(), pid()) ->  ok | {error, reason()}.
%%
%% Description:  Changes process that receives the messages when active = true
%% or once. 
%%--------------------------------------------------------------------
new_user(ConnectionPid, User) ->
    call(ConnectionPid, {new_user, User}).

%%--------------------------------------------------------------------
-spec negotiated_protocol(pid()) -> {ok, binary()} | {error, reason()}.
%%
%% Description:  Returns the negotiated protocol
%%--------------------------------------------------------------------
negotiated_protocol(ConnectionPid) ->
    call(ConnectionPid, negotiated_protocol).

%%--------------------------------------------------------------------
-spec get_opts(pid(), list()) -> {ok, list()} | {error, reason()}.    
%%
%% Description: Same as inet:getopts/2
%%--------------------------------------------------------------------
get_opts(ConnectionPid, OptTags) ->
    call(ConnectionPid, {get_opts, OptTags}).
%%--------------------------------------------------------------------
-spec set_opts(pid(), list()) -> ok | {error, reason()}. 
%%
%% Description:  Same as inet:setopts/2
%%--------------------------------------------------------------------
set_opts(ConnectionPid, Options) ->
    call(ConnectionPid, {set_opts, Options}).

%%--------------------------------------------------------------------
-spec peer_certificate(pid()) -> {ok, binary()| undefined} | {error, reason()}.
%%
%% Description: Returns the peer cert
%%--------------------------------------------------------------------
peer_certificate(ConnectionPid) ->
    call(ConnectionPid, peer_certificate). 

%%--------------------------------------------------------------------
-spec renegotiation(pid()) -> ok | {error, reason()}.
%%
%% Description: Starts a renegotiation of the ssl session.
%%--------------------------------------------------------------------
renegotiation(ConnectionPid) ->
    call(ConnectionPid, renegotiate). 

%%--------------------------------------------------------------------
-spec internal_renegotiation(pid(), ssl_record:connection_states()) -> 
                                    ok. 
%%
%% Description: Starts a renegotiation of the ssl session.
%%--------------------------------------------------------------------
internal_renegotiation(ConnectionPid, #{current_write := WriteState}) ->
    gen_statem:cast(ConnectionPid, {internal_renegotiate, WriteState}). 

dist_handshake_complete(ConnectionPid, DHandle) ->
    gen_statem:cast(ConnectionPid, {dist_handshake_complete, DHandle}).

%%--------------------------------------------------------------------
-spec prf(pid(), binary() | 'master_secret', binary(),
	  [binary() | ssl:prf_random()], non_neg_integer()) ->
		 {ok, binary()} | {error, reason()} | {'EXIT', term()}.
%%
%% Description: use a ssl sessions TLS PRF to generate key material
%%--------------------------------------------------------------------
prf(ConnectionPid, Secret, Label, Seed, WantedLength) ->
    call(ConnectionPid, {prf, Secret, Label, Seed, WantedLength}).

%%====================================================================
%% Alert and close handling
%%====================================================================
handle_own_alert(Alert0, _, StateName,
		 #state{static_env = #static_env{role = Role,
                                                 protocol_cb = Connection},
                        ssl_options = #{log_level := LogLevel}} = State) ->
    try %% Try to tell the other side
        send_alert(Alert0, StateName, State)
    catch _:_ ->  %% Can crash if we are in a uninitialized state
	    ignore
    end,
    try %% Try to tell the local user
        Alert = Alert0#alert{role = Role},
	log_alert(LogLevel, Role, Connection:protocol_name(), StateName, Alert),
	handle_normal_shutdown(Alert,StateName, State)
    catch _:_ ->
	    ok
    end,
    {stop, {shutdown, own_alert}, State}.

handle_normal_shutdown(Alert, StateName, #state{static_env = #static_env{role = Role,
                                                                         socket = Socket,
                                                                         transport_cb = Transport,
                                                                         protocol_cb = Connection,
                                                                         tracker = Tracker},
                                                handshake_env = #handshake_env{renegotiation = {false, first}},
                                                start_or_recv_from = StartFrom} = State) ->
    Pids = Connection:pids(State),
    alert_user(Pids, Transport, Tracker,Socket, StartFrom, Alert, Role, StateName, Connection);

handle_normal_shutdown(Alert, StateName, #state{static_env = #static_env{role = Role,
                                                                         socket = Socket,
                                                                         transport_cb = Transport,
                                                                         protocol_cb = Connection,
                                                                         tracker = Tracker},
                                                connection_env  = #connection_env{user_application = {_Mon, Pid}},
                                                socket_options = Opts,
						start_or_recv_from = RecvFrom} = State) ->
    Pids = Connection:pids(State),
    alert_user(Pids, Transport, Tracker, Socket, StateName, Opts, Pid, RecvFrom, Alert, Role, StateName, Connection).

handle_alert(#alert{level = ?FATAL} = Alert0, StateName,
	     #state{static_env = #static_env{role = Role,
                                             socket = Socket,
                                             host = Host,
                                             port = Port,
                                             tracker = Tracker,
                                             transport_cb = Transport,
                                             protocol_cb = Connection},
                    connection_env  = #connection_env{user_application = {_Mon, Pid}},
		    ssl_options = #{log_level := LogLevel},
                    start_or_recv_from = From,
                    session = Session, 
		    socket_options = Opts} = State) ->
    invalidate_session(Role, Host, Port, Session),
    Alert = Alert0#alert{role = opposite_role(Role)},
    log_alert(LogLevel, Role, Connection:protocol_name(),
              StateName, Alert),
    Pids = Connection:pids(State),
    alert_user(Pids, Transport, Tracker, Socket, StateName, Opts, Pid, From, Alert, Role, StateName, Connection),
    {stop, {shutdown, normal}, State};

handle_alert(#alert{level = ?WARNING, description = ?CLOSE_NOTIFY} = Alert, 
	     downgrade= StateName, State) -> 
    {next_state, StateName, State, [{next_event, internal, Alert}]};
handle_alert(#alert{level = ?WARNING, description = ?CLOSE_NOTIFY} = Alert, 
	    StateName, State) -> 
    handle_normal_shutdown(Alert, StateName, State),
    {stop,{shutdown, peer_close}, State};
handle_alert(#alert{level = ?WARNING, description = ?NO_RENEGOTIATION} = Alert0, StateName, 
	     #state{static_env = #static_env{role = Role,
                                             protocol_cb = Connection},
                    handshake_env = #handshake_env{renegotiation = {true, internal}},
                    ssl_options = #{log_level := LogLevel}} = State) ->
    Alert = Alert0#alert{role = opposite_role(Role)},
    log_alert(LogLevel, Role,
              Connection:protocol_name(), StateName, Alert),
    handle_normal_shutdown(Alert, StateName, State),
    {stop,{shutdown, peer_close}, State};

handle_alert(#alert{level = ?WARNING, description = ?NO_RENEGOTIATION} = Alert, connection = StateName, 
	     #state{static_env = #static_env{role = Role,
                                             protocol_cb = Connection},
                    handshake_env = #handshake_env{renegotiation = {true, From}} = HsEnv,
                    ssl_options = #{log_level := LogLevel}
		   } = State0) ->
    log_alert(LogLevel, Role,
              Connection:protocol_name(), StateName, Alert#alert{role = opposite_role(Role)}),
    gen_statem:reply(From, {error, renegotiation_rejected}),
    State = Connection:reinit_handshake_data(State0),
    Connection:next_event(connection, no_record, State#state{handshake_env = HsEnv#handshake_env{renegotiation = undefined}});

handle_alert(#alert{level = ?WARNING, description = ?NO_RENEGOTIATION} = Alert, StateName, 
	     #state{static_env = #static_env{role = Role,
                                             protocol_cb = Connection},
                    handshake_env = #handshake_env{renegotiation = {true, From}} = HsEnv,
                    ssl_options = #{log_level := LogLevel}
                   } = State0) ->
    log_alert(LogLevel, Role,
              Connection:protocol_name(), StateName, Alert#alert{role = opposite_role(Role)}),
    gen_statem:reply(From, {error, renegotiation_rejected}),
    %% Go back to connection!
    State = Connection:reinit(State0#state{handshake_env = HsEnv#handshake_env{renegotiation = undefined}}),
    Connection:next_event(connection, no_record, State);

%% Gracefully log and ignore all other warning alerts
handle_alert(#alert{level = ?WARNING} = Alert, StateName,
	     #state{static_env = #static_env{role = Role,
                                             protocol_cb = Connection},
                    ssl_options = #{log_level := LogLevel}} = State) ->
    log_alert(LogLevel, Role,
              Connection:protocol_name(), StateName,
              Alert#alert{role = opposite_role(Role)}),
    Connection:next_event(StateName, no_record, State).

%%====================================================================
%% Data handling
%%====================================================================
passive_receive(State0 = #state{user_data_buffer = {_,BufferSize,_}}, StateName, Connection, StartTimerAction) ->
    case BufferSize of
	0 ->
	    Connection:next_event(StateName, no_record, State0, StartTimerAction);
	_ ->
	    case read_application_data(<<>>, State0) of
                {stop, _, _} = ShutdownError ->
                    ShutdownError;
                {Record, State} ->
                    case State#state.start_or_recv_from of
                        undefined ->
                            %% Cancel recv timeout as data has been delivered
                            Connection:next_event(StateName, Record, State, 
                                                  [{{timeout, recv}, infinity, timeout}]);
                        _ ->
                            Connection:next_event(StateName, Record, State, StartTimerAction)
                    end
            end
    end.

read_application_data(
  Data,
  #state{
     user_data_buffer = {Front0,BufferSize0,Rear0},
     connection_env = #connection_env{erl_dist_handle = DHandle}} = State) ->
    %%
    Front = Front0,
    BufferSize = BufferSize0 + byte_size(Data),
    Rear = [Data|Rear0],
    case DHandle of
        undefined ->
            read_application_data(State, Front, BufferSize, Rear);
        _ ->
            try read_application_dist_data(DHandle, Front, BufferSize, Rear) of
                Buffer ->
                    {no_record, State#state{user_data_buffer = Buffer}}
            catch error:_ ->
                    {stop,disconnect,
                     State#state{user_data_buffer = {Front,BufferSize,Rear}}}
            end
    end.


read_application_data(#state{
                         socket_options = SocketOpts,
                         bytes_to_read = BytesToRead,
                         start_or_recv_from = RecvFrom} = State, Front, BufferSize, Rear) ->
    read_application_data(State, Front, BufferSize, Rear, SocketOpts, RecvFrom, BytesToRead).

%% Pick binary from queue front, if empty wait for more data
read_application_data(State, [Bin|Front], BufferSize, Rear, SocketOpts, RecvFrom, BytesToRead) ->
    read_application_data_bin(State, Front, BufferSize, Rear, SocketOpts, RecvFrom, BytesToRead, Bin);
read_application_data(State, [] = Front, BufferSize, [] = Rear, SocketOpts, RecvFrom, BytesToRead) ->
    0 = BufferSize, % Assert
    {no_record, State#state{socket_options = SocketOpts,
                            bytes_to_read = BytesToRead,
                            start_or_recv_from = RecvFrom,
                            user_data_buffer = {Front,BufferSize,Rear}}};
read_application_data(State, [], BufferSize, Rear, SocketOpts, RecvFrom, BytesToRead) ->
    [Bin|Front] = lists:reverse(Rear),
    read_application_data_bin(State, Front, BufferSize, [], SocketOpts, RecvFrom, BytesToRead, Bin).

read_application_data_bin(State, Front, BufferSize, Rear, SocketOpts, RecvFrom, BytesToRead, <<>>) ->
    %% Done with this binary - get next
    read_application_data(State, Front, BufferSize, Rear, SocketOpts, RecvFrom, BytesToRead);
read_application_data_bin(State, Front0, BufferSize0, Rear0, SocketOpts0, RecvFrom, BytesToRead, Bin0) ->
    %% Decode one packet from a binary
    case get_data(SocketOpts0, BytesToRead, Bin0) of
	{ok, Data, Bin} -> % Send data
            BufferSize = BufferSize0 - (byte_size(Bin0) - byte_size(Bin)),
            read_application_data_deliver(
              State, [Bin|Front0], BufferSize, Rear0, SocketOpts0, RecvFrom, Data);
        {more, undefined} ->
            %% We need more data, do not know how much
            if
                byte_size(Bin0) < BufferSize0 ->
                    %% We have more data in the buffer besides the first binary - concatenate all and retry
                    Bin = iolist_to_binary([Bin0,Front0|lists:reverse(Rear0)]),
                    read_application_data_bin(
                      State, [], BufferSize0, [], SocketOpts0, RecvFrom, BytesToRead, Bin);
                true ->
                    %% All data is in the first binary, no use to retry - wait for more
                    {no_record, State#state{socket_options = SocketOpts0,
                                            bytes_to_read = BytesToRead,
                                            start_or_recv_from = RecvFrom,
                                            user_data_buffer = {[Bin0|Front0],BufferSize0,Rear0}}}
            end;
        {more, Size} when Size =< BufferSize0 ->
            %% We have a packet in the buffer - collect it in a binary and decode
            {Data,Front,Rear} = iovec_from_front(Size - byte_size(Bin0), Front0, Rear0, [Bin0]),
            Bin = iolist_to_binary(Data),
            read_application_data_bin(
              State, Front, BufferSize0, Rear, SocketOpts0, RecvFrom, BytesToRead, Bin);
        {more, _Size} ->
            %% We do not have a packet in the buffer - wait for more
            {no_record, State#state{socket_options = SocketOpts0,
                                    bytes_to_read = BytesToRead,
                                    start_or_recv_from = RecvFrom,
                                    user_data_buffer = {[Bin0|Front0],BufferSize0,Rear0}}};
        passive ->
            {no_record, State#state{socket_options = SocketOpts0,
                                    bytes_to_read = BytesToRead,
                                    start_or_recv_from = RecvFrom,
                                    user_data_buffer = {[Bin0|Front0],BufferSize0,Rear0}}};
	{error,_Reason} ->
            %% Invalid packet in packet mode
            #state{
               static_env =
                   #static_env{
                      socket = Socket,
                      protocol_cb = Connection,
                      transport_cb = Transport,
                      tracker = Tracker},
               connection_env =
                   #connection_env{user_application = {_Mon, Pid}}} = State,
            Buffer = iolist_to_binary([Bin0,Front0|lists:reverse(Rear0)]),
	    deliver_packet_error(
              Connection:pids(State), Transport, Socket, SocketOpts0,
              Buffer, Pid, RecvFrom, Tracker, Connection),
            {stop, {shutdown, normal}, State#state{socket_options = SocketOpts0,
                                                   bytes_to_read = BytesToRead,
                                                   start_or_recv_from = RecvFrom,
                                                   user_data_buffer = {[Buffer],BufferSize0,[]}}}
    end.

read_application_data_deliver(State, Front, BufferSize, Rear, SocketOpts0, RecvFrom, Data) ->
    #state{
       static_env =
           #static_env{
              socket = Socket,
              protocol_cb = Connection,
              transport_cb = Transport,
              tracker = Tracker},
       connection_env =
           #connection_env{user_application = {_Mon, Pid}}} = State,
    SocketOpts =
        deliver_app_data(
          Connection:pids(State), Transport, Socket, SocketOpts0, Data, Pid, RecvFrom, Tracker, Connection),
    if
        SocketOpts#socket_options.active =:= false ->
            %% Passive mode, wait for active once or recv
            {no_record,
             State#state{
               user_data_buffer = {Front,BufferSize,Rear},
               start_or_recv_from = undefined,
                bytes_to_read = undefined,
               socket_options = SocketOpts
              }};
        true -> %% Try to deliver more data
            read_application_data(State, Front, BufferSize, Rear, SocketOpts, undefined, undefined)
    end.


read_application_dist_data(DHandle, [Bin|Front], BufferSize, Rear) ->
    read_application_dist_data(DHandle, Front, BufferSize, Rear, Bin);
read_application_dist_data(_DHandle, [] = Front, BufferSize, [] = Rear) ->
    BufferSize = 0,
    {Front,BufferSize,Rear};
read_application_dist_data(DHandle, [], BufferSize, Rear) ->
    [Bin|Front] = lists:reverse(Rear),
    read_application_dist_data(DHandle, Front, BufferSize, [], Bin).
%%
read_application_dist_data(DHandle, Front0, BufferSize, Rear0, Bin0) ->
    case Bin0 of
        %%
        %% START Optimization
        %% It is cheaper to match out several packets in one match operation than to loop for each
        <<SizeA:32, DataA:SizeA/binary,
          SizeB:32, DataB:SizeB/binary,
          SizeC:32, DataC:SizeC/binary,
          SizeD:32, DataD:SizeD/binary, Rest/binary>>
          when 0 < SizeA, 0 < SizeB, 0 < SizeC, 0 < SizeD ->
            %% We have 4 complete packets in the first binary
            erlang:dist_ctrl_put_data(DHandle, DataA),
            erlang:dist_ctrl_put_data(DHandle, DataB),
            erlang:dist_ctrl_put_data(DHandle, DataC),
            erlang:dist_ctrl_put_data(DHandle, DataD),
            read_application_dist_data(
              DHandle, Front0, BufferSize - (4*4+SizeA+SizeB+SizeC+SizeD), Rear0, Rest);
        <<SizeA:32, DataA:SizeA/binary,
          SizeB:32, DataB:SizeB/binary,
          SizeC:32, DataC:SizeC/binary, Rest/binary>>
          when 0 < SizeA, 0 < SizeB, 0 < SizeC ->
            %% We have 3 complete packets in the first binary
            erlang:dist_ctrl_put_data(DHandle, DataA),
            erlang:dist_ctrl_put_data(DHandle, DataB),
            erlang:dist_ctrl_put_data(DHandle, DataC),
            read_application_dist_data(
              DHandle, Front0, BufferSize - (3*4+SizeA+SizeB+SizeC), Rear0, Rest);
        <<SizeA:32, DataA:SizeA/binary,
          SizeB:32, DataB:SizeB/binary, Rest/binary>>
          when 0 < SizeA, 0 < SizeB ->
            %% We have 2 complete packets in the first binary
            erlang:dist_ctrl_put_data(DHandle, DataA),
            erlang:dist_ctrl_put_data(DHandle, DataB),
            read_application_dist_data(
              DHandle, Front0, BufferSize - (2*4+SizeA+SizeB), Rear0, Rest);
        %% END Optimization
        %%
        %% Basic one packet code path
        <<Size:32, Data:Size/binary, Rest/binary>> ->
            %% We have a complete packet in the first binary
            0 < Size andalso erlang:dist_ctrl_put_data(DHandle, Data),
            read_application_dist_data(DHandle, Front0, BufferSize - (4+Size), Rear0, Rest);
        <<Size:32, FirstData/binary>> when 4+Size =< BufferSize ->
            %% We have a complete packet in the buffer
            %% - fetch the missing content from the buffer front
            {Data,Front,Rear} = iovec_from_front(Size - byte_size(FirstData), Front0, Rear0, [FirstData]),
            0 < Size andalso erlang:dist_ctrl_put_data(DHandle, Data),
            read_application_dist_data(DHandle, Front, BufferSize - (4+Size), Rear);
        <<Bin/binary>> ->
            %% In OTP-21 the match context reuse optimization fails if we use Bin0 in recursion, so here we
            %% match out the whole binary which will trick the optimization into keeping the match context
            %% for the first binary contains complete packet code above
            case Bin of
                <<_Size:32, _InsufficientData/binary>> ->
                    %% We have a length field in the first binary but there is not enough data
                    %% in the buffer to form a complete packet - await more data
                    {[Bin|Front0],BufferSize,Rear0};
                <<IncompleteLengthField/binary>> when 4 < BufferSize ->
                    %% We do not have a length field in the first binary but the buffer
                    %% contains enough data to maybe form a packet
                    %% - fetch a tiny binary from the buffer front to complete the length field
                    {LengthField,Front,Rear} =
                        case IncompleteLengthField of
                            <<>> ->
                                iovec_from_front(4, Front0, Rear0, []);
                            _ ->
                                iovec_from_front(
                                  4 - byte_size(IncompleteLengthField), Front0, Rear0, [IncompleteLengthField])
                        end,
                    LengthBin = iolist_to_binary(LengthField),
                    read_application_dist_data(DHandle, Front, BufferSize, Rear, LengthBin);
                <<IncompleteLengthField/binary>> ->
                    %% We do not have enough data in the buffer to even form a length field - await more data
                    case IncompleteLengthField of
                        <<>> ->
                            {Front0,BufferSize,Rear0};
                        _ ->
                            {[IncompleteLengthField|Front0],BufferSize,Rear0}
                    end
            end
    end.

iovec_from_front(0, Front, Rear, Acc) ->
    {lists:reverse(Acc),Front,Rear};
iovec_from_front(Size, [], Rear, Acc) ->
    case Rear of
        %% Avoid lists:reverse/1 for simple cases.
        %% Case clause for [] to avoid infinite loop.
        [_] ->
            iovec_from_front(Size, Rear, [], Acc);
        [Bin2,Bin1] ->
            iovec_from_front(Size, [Bin1,Bin2], [], Acc);
        [Bin3,Bin2,Bin1] ->
            iovec_from_front(Size, [Bin1,Bin2,Bin3], [], Acc);
        [_,_,_|_] = Rear ->
            iovec_from_front(Size, lists:reverse(Rear), [], Acc)
    end;
iovec_from_front(Size, [Bin|Front], Rear, []) ->
    case Bin of
        <<Last:Size/binary>> -> % Just enough
            {[Last],Front,Rear};
        <<Last:Size/binary, Rest/binary>> -> % More than enough, split here
            {[Last],[Rest|Front],Rear};
        <<>> -> % Not enough, skip empty binaries
            iovec_from_front(Size, Front, Rear, []);
        <<_/binary>> -> % Not enough
            BinSize = byte_size(Bin),
            iovec_from_front(Size - BinSize, Front, Rear, [Bin])
    end;
iovec_from_front(Size, [Bin|Front], Rear, Acc) ->
    case Bin of
        <<Last:Size/binary>> -> % Just enough
            {lists:reverse(Acc, [Last]),Front,Rear};
        <<Last:Size/binary, Rest/binary>> -> % More than enough, split here
            {lists:reverse(Acc, [Last]),[Rest|Front],Rear};
        <<>> -> % Not enough, skip empty binaries
            iovec_from_front(Size, Front, Rear, Acc);
        <<_/binary>> -> % Not enough
            BinSize = byte_size(Bin),
            iovec_from_front(Size - BinSize, Front, Rear, [Bin|Acc])
    end.


%%====================================================================
%% Help functions for tls|dtls_connection.erl
%%====================================================================
%%--------------------------------------------------------------------
-spec handle_session(#server_hello{}, ssl_record:ssl_version(),
		     binary(), ssl_record:connection_states(), _,_, #state{}) ->
			    gen_statem:state_function_result().
%%--------------------------------------------------------------------
handle_session(#server_hello{cipher_suite = CipherSuite,
			     compression_method = Compression}, 
	       Version, NewId, ConnectionStates, ProtoExt, Protocol0,
	       #state{session = #session{session_id = OldId},
		      handshake_env = #handshake_env{negotiated_protocol = CurrentProtocol} = HsEnv,
                      connection_env = #connection_env{negotiated_version = ReqVersion} = CEnv} = State0) ->
    #{key_exchange := KeyAlgorithm} =
	ssl_cipher_format:suite_bin_to_map(CipherSuite),
    
    PremasterSecret = make_premaster_secret(ReqVersion, KeyAlgorithm),

    {ExpectNPN, Protocol} = case Protocol0 of
				undefined -> 

				    {false, CurrentProtocol};
				_ -> 
				    {ProtoExt =:= npn, Protocol0}
			    end,

    State = State0#state{connection_states = ConnectionStates,
			 handshake_env = HsEnv#handshake_env{kex_algorithm = KeyAlgorithm,
                                                             premaster_secret = PremasterSecret,
                                                             expecting_next_protocol_negotiation = ExpectNPN,
                                                             negotiated_protocol = Protocol},
                         connection_env = CEnv#connection_env{negotiated_version = Version}},
    
    case ssl_session:is_new(OldId, NewId) of
	true ->
	    handle_new_session(NewId, CipherSuite, Compression,
			       State#state{connection_states = ConnectionStates});
	false ->
	    handle_resumed_session(NewId,
				   State#state{connection_states = ConnectionStates})
    end.

%%--------------------------------------------------------------------
-spec ssl_config(ssl_options(), client | server, #state{}) -> #state{}.
%%--------------------------------------------------------------------
ssl_config(Opts, Role, #state{static_env = InitStatEnv0, 
                              handshake_env = HsEnv,
                              connection_env = CEnv} = State0) ->
    {ok, #{cert_db_ref := Ref, 
           cert_db_handle := CertDbHandle, 
           fileref_db_handle := FileRefHandle, 
           session_cache := CacheHandle, 
           crl_db_info := CRLDbHandle,
           private_key := Key,
           dh_params := DHParams,
           own_certificate := OwnCert}} =
	ssl_config:init(Opts, Role), 
    TimeStamp = erlang:monotonic_time(),
    Session = State0#state.session,
    
    State0#state{session = Session#session{own_certificate = OwnCert,
                                           time_stamp = TimeStamp},
                 static_env = InitStatEnv0#static_env{
                                file_ref_db = FileRefHandle,
                                cert_db_ref = Ref,
                                cert_db = CertDbHandle,
                                crl_db = CRLDbHandle,
                                session_cache = CacheHandle
                               },
                 handshake_env = HsEnv#handshake_env{diffie_hellman_params = DHParams},
                 connection_env = CEnv#connection_env{private_key = Key},
                 ssl_options = Opts}.

%%====================================================================
%% gen_statem general state functions with connection cb argument
%%====================================================================	
%%--------------------------------------------------------------------
-spec init(gen_statem:event_type(),
	   {start, timeout()} |  {start, {list(), list()}, timeout()}| term(),
	    #state{}, tls_connection | dtls_connection) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------

init({call, From}, {start, Timeout}, State0, Connection) ->
    Connection:next_event(hello, no_record, State0#state{start_or_recv_from = From},
                          [{{timeout, handshake}, Timeout, close}]);
init({call, From}, {start, {Opts, EmOpts}, Timeout}, 
     #state{static_env = #static_env{role = Role},
            ssl_options = OrigSSLOptions,
            socket_options = SockOpts} = State0, Connection) ->
    try 
        SslOpts = ssl:handle_options(Opts, OrigSSLOptions),
	State = ssl_config(SslOpts, Role, State0),
	init({call, From}, {start, Timeout}, 
	     State#state{ssl_options = SslOpts, 
                         socket_options = new_emulated(EmOpts, SockOpts)}, Connection)
    catch throw:Error ->
	   {stop_and_reply, {shutdown, normal}, {reply, From, {error, Error}}, State0}
    end;
init({call, From}, {new_user, _} = Msg, State, Connection) ->
    handle_call(Msg, From, ?FUNCTION_NAME, State, Connection);
init({call, From}, _Msg, _State, _Connection) ->
    {keep_state_and_data, [{reply, From, {error, notsup_on_transport_accept_socket}}]}; 
init(_Type, _Event, _State, _Connection) ->
    {keep_state_and_data, [postpone]}.
	
%%--------------------------------------------------------------------
-spec error(gen_statem:event_type(),
	   {start, timeout()} | term(), #state{},
            tls_connection | dtls_connection) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
error({call, From}, {close, _}, State, _Connection) ->
    {stop_and_reply, {shutdown, normal}, {reply, From, ok}, State};
error({call, From}, _Msg, State, _Connection) ->
    {next_state, ?FUNCTION_NAME, State, [{reply, From, {error, closed}}]}.

%%--------------------------------------------------------------------
-spec hello(gen_statem:event_type(),
	    #hello_request{} | #server_hello{} | term(),
	    #state{}, tls_connection | dtls_connection) ->
		   gen_statem:state_function_result().
%%--------------------------------------------------------------------
hello({call, From}, Msg, State, Connection) ->
    handle_call(Msg, From, ?FUNCTION_NAME, State, Connection);
hello(internal, {common_client_hello, Type, ServerHelloExt}, State, Connection) ->
    do_server_hello(Type, ServerHelloExt, State, Connection);
hello(info, Msg, State, _) ->
    handle_info(Msg, ?FUNCTION_NAME, State);
hello(Type, Msg, State, Connection) ->
    handle_common_event(Type, Msg, ?FUNCTION_NAME, State, Connection).

user_hello({call, From}, cancel, #state{connection_env = #connection_env{negotiated_version = Version}} = State, _) ->
    gen_statem:reply(From, ok),
    handle_own_alert(?ALERT_REC(?FATAL, ?USER_CANCELED, user_canceled),
                     Version, ?FUNCTION_NAME, State);
user_hello({call, From}, {handshake_continue, NewOptions, Timeout},
           #state{static_env = #static_env{role = Role},
                  handshake_env = #handshake_env{hello = Hello},
                  ssl_options = Options0} = State0, _Connection) ->
    Options = ssl:handle_options(NewOptions, Options0#{handshake => full}),
    State = ssl_config(Options, Role, State0),
    {next_state, hello, State#state{start_or_recv_from = From}, 
     [{next_event, internal, Hello}, {{timeout, handshake}, Timeout, close}]};
user_hello(_, _, _, _) ->
    {keep_state_and_data, [postpone]}.

%%--------------------------------------------------------------------
-spec abbreviated(gen_statem:event_type(),
		  #hello_request{} | #finished{} | term(),
		  #state{}, tls_connection | dtls_connection) ->
			 gen_statem:state_function_result().
%%--------------------------------------------------------------------
abbreviated({call, From}, Msg, State, Connection) ->
    handle_call(Msg, From, ?FUNCTION_NAME, State, Connection);
abbreviated(internal, #finished{verify_data = Data} = Finished,
	    #state{static_env = #static_env{role = server},
                   handshake_env = #handshake_env{tls_handshake_history = Hist,
                                                  expecting_finished = true} = HsEnv,
		   connection_env = #connection_env{negotiated_version = Version},
		   session = #session{master_secret = MasterSecret},
		   connection_states = ConnectionStates0} =
		State0, Connection) ->
    case ssl_handshake:verify_connection(ssl:tls_version(Version), Finished, client,
					 get_current_prf(ConnectionStates0, write),
					 MasterSecret, Hist) of
        verified ->
	    ConnectionStates =
		ssl_record:set_client_verify_data(current_both, Data, ConnectionStates0),
	    {Record, State} = prepare_connection(State0#state{connection_states = ConnectionStates,
                                                              handshake_env = HsEnv#handshake_env{expecting_finished = false}}, Connection),
	    Connection:next_event(connection, Record, State, [{{timeout, handshake}, infinity, close}]);
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State0)
    end;
abbreviated(internal, #finished{verify_data = Data} = Finished,
	    #state{static_env = #static_env{role = client},
                   handshake_env = #handshake_env{tls_handshake_history = Hist0},
                   connection_env = #connection_env{negotiated_version = Version},
		   session = #session{master_secret = MasterSecret},
		   connection_states = ConnectionStates0} = State0, Connection) ->
    case ssl_handshake:verify_connection(ssl:tls_version(Version), Finished, server,
					 get_pending_prf(ConnectionStates0, write),
					 MasterSecret, Hist0) of
        verified ->
	    ConnectionStates1 =
		ssl_record:set_server_verify_data(current_read, Data, ConnectionStates0),
	    {#state{handshake_env = HsEnv} = State1, Actions} =
		finalize_handshake(State0#state{connection_states = ConnectionStates1},
				   ?FUNCTION_NAME, Connection),
	    {Record, State} = prepare_connection(State1#state{handshake_env = HsEnv#handshake_env{expecting_finished = false}}, Connection),
	    Connection:next_event(connection, Record, State, [{{timeout, handshake}, infinity, close} | Actions]);
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State0)
    end;
%% only allowed to send next_protocol message after change cipher spec
%% & before finished message and it is not allowed during renegotiation
abbreviated(internal, #next_protocol{selected_protocol = SelectedProtocol},
	    #state{static_env = #static_env{role = server},
                   handshake_env = #handshake_env{expecting_next_protocol_negotiation = true} = HsEnv} = State,
	    Connection) ->
    Connection:next_event(?FUNCTION_NAME, no_record, 
			  State#state{handshake_env = HsEnv#handshake_env{negotiated_protocol = SelectedProtocol,
                                                                         expecting_next_protocol_negotiation = false}});
abbreviated(internal, 
	    #change_cipher_spec{type = <<1>>},  
            #state{connection_states = ConnectionStates0,
                   handshake_env = HsEnv} = State, Connection) ->
    ConnectionStates1 =
	ssl_record:activate_pending_connection_state(ConnectionStates0, read, Connection),
    Connection:next_event(?FUNCTION_NAME, no_record, State#state{connection_states = 
                                                                     ConnectionStates1,
                                                                 handshake_env = HsEnv#handshake_env{expecting_finished = true}});
abbreviated(info, Msg, State, _) ->
    handle_info(Msg, ?FUNCTION_NAME, State);
abbreviated(Type, Msg, State, Connection) ->
    handle_common_event(Type, Msg, ?FUNCTION_NAME, State, Connection).
 
%%--------------------------------------------------------------------
-spec certify(gen_statem:event_type(),
	      #hello_request{} | #certificate{} |  #server_key_exchange{} |
	      #certificate_request{} | #server_hello_done{} | #client_key_exchange{} | term(),
	      #state{}, tls_connection | dtls_connection) ->
		     gen_statem:state_function_result().
%%--------------------------------------------------------------------
certify({call, From}, Msg, State, Connection) ->
    handle_call(Msg, From, ?FUNCTION_NAME, State, Connection);
certify(info, Msg, State, _) ->
    handle_info(Msg, ?FUNCTION_NAME, State);
certify(internal, #certificate{asn1_certificates = []},
	#state{static_env = #static_env{role = server},
               connection_env = #connection_env{negotiated_version = Version},
	       ssl_options = #{verify := verify_peer,
                               fail_if_no_peer_cert := true}} =
	    State, _) ->
    Alert =  ?ALERT_REC(?FATAL,?HANDSHAKE_FAILURE),
    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State);
certify(internal, #certificate{asn1_certificates = []},
	#state{static_env = #static_env{role = server},
	       ssl_options = #{verify := verify_peer,
                               fail_if_no_peer_cert := false}} =
	State0, Connection) ->
    Connection:next_event(?FUNCTION_NAME, no_record, State0#state{client_certificate_requested = false});
certify(internal, #certificate{},
	#state{static_env = #static_env{role = server},
               connection_env = #connection_env{negotiated_version = Version},
	       ssl_options = #{verify := verify_none}} =
	    State, _) ->
    Alert =  ?ALERT_REC(?FATAL,?UNEXPECTED_MESSAGE, unrequested_certificate),
    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State);
certify(internal, #certificate{} = Cert,
        #state{static_env = #static_env{
                               role = Role,
                               host = Host,
                               cert_db = CertDbHandle,
                               cert_db_ref = CertDbRef,
                               crl_db = CRLDbInfo},
               connection_env = #connection_env{negotiated_version = Version},
	       ssl_options = Opts} = State, Connection) ->
    case ssl_handshake:certify(Cert, CertDbHandle, CertDbRef, 
			       Opts, CRLDbInfo, Role, Host) of
        {PeerCert, PublicKeyInfo} ->
	    handle_peer_cert(Role, PeerCert, PublicKeyInfo,
			     State#state{client_certificate_requested = false}, Connection);
	#alert{} = Alert ->
            handle_own_alert(Alert, Version, ?FUNCTION_NAME, State)
    end;
certify(internal, #server_key_exchange{exchange_keys = Keys},
        #state{static_env = #static_env{role = client},
               handshake_env = #handshake_env{kex_algorithm = KexAlg,
                                              public_key_info = PubKeyInfo} = HsEnv,
               connection_env = #connection_env{negotiated_version = Version},
               session = Session,
	       connection_states = ConnectionStates} = State, Connection)
  when KexAlg == dhe_dss; 
       KexAlg == dhe_rsa;
       KexAlg == ecdhe_rsa; 
       KexAlg == ecdhe_ecdsa;
       KexAlg == dh_anon; 
       KexAlg == ecdh_anon;
       KexAlg == psk; 
       KexAlg == dhe_psk; 
       KexAlg == ecdhe_psk; 
       KexAlg == rsa_psk;
       KexAlg == srp_dss; 
       KexAlg == srp_rsa; 
       KexAlg == srp_anon ->

    Params = ssl_handshake:decode_server_key(Keys, KexAlg, ssl:tls_version(Version)),

    %% Use negotiated value if TLS-1.2 otherwhise return default
    HashSign = negotiated_hashsign(Params#server_key_params.hashsign, KexAlg, PubKeyInfo, ssl:tls_version(Version)),

    case is_anonymous(KexAlg) of
	true ->
	    calculate_secret(Params#server_key_params.params,
			     State#state{handshake_env = HsEnv#handshake_env{hashsign_algorithm = HashSign}}, Connection);
	false ->
	    case  ssl_handshake:verify_server_key(Params, HashSign, 
						  ConnectionStates, ssl:tls_version(Version), PubKeyInfo) of
		true ->
		    calculate_secret(Params#server_key_params.params,
				     State#state{handshake_env = HsEnv#handshake_env{hashsign_algorithm = HashSign},
                                                 session = session_handle_params(Params#server_key_params.params, Session)},
                    Connection);
		false ->
		    handle_own_alert(?ALERT_REC(?FATAL, ?DECRYPT_ERROR),
						Version, ?FUNCTION_NAME, State)
	    end
    end;
certify(internal, #certificate_request{},
	#state{static_env = #static_env{role = client},
               handshake_env = #handshake_env{kex_algorithm = KexAlg},
               connection_env = #connection_env{negotiated_version = Version}} = State, _)
  when KexAlg == dh_anon; 
       KexAlg == ecdh_anon;
       KexAlg == psk; 
       KexAlg == dhe_psk; 
       KexAlg == ecdhe_psk; 
       KexAlg == rsa_psk;
       KexAlg == srp_dss; 
       KexAlg == srp_rsa; 
       KexAlg == srp_anon ->
    handle_own_alert(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE),
                     Version, ?FUNCTION_NAME, State);
certify(internal, #certificate_request{},
	#state{static_env = #static_env{role = client},
               session = #session{own_certificate = undefined}} = State, Connection) ->
    %% The client does not have a certificate and will send an empty reply, the server may fail 
    %% or accept the connection by its own preference. No signature algorihms needed as there is
    %% no certificate to verify.
    Connection:next_event(?FUNCTION_NAME, no_record, State#state{client_certificate_requested = true});
certify(internal, #certificate_request{} = CertRequest,
	#state{static_env = #static_env{role = client},
               handshake_env = HsEnv,
               connection_env = #connection_env{negotiated_version = Version},
               session = #session{own_certificate = Cert},
               ssl_options = #{signature_algs := SupportedHashSigns}} = State, Connection) ->
    case ssl_handshake:select_hashsign(CertRequest, Cert, 
                                       SupportedHashSigns, ssl:tls_version(Version)) of
	#alert {} = Alert ->
	    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State);
	NegotiatedHashSign -> 	
	    Connection:next_event(?FUNCTION_NAME, no_record,
				  State#state{client_certificate_requested = true,
                                              handshake_env = HsEnv#handshake_env{cert_hashsign_algorithm = NegotiatedHashSign}})
    end;
%% PSK and RSA_PSK might bypass the Server-Key-Exchange
certify(internal, #server_hello_done{},
	#state{static_env = #static_env{role = client},
               session = #session{master_secret = undefined},
               connection_env = #connection_env{negotiated_version = Version},
               handshake_env = #handshake_env{kex_algorithm = KexAlg,
                                              premaster_secret = undefined,
                                              server_psk_identity = PSKIdentity} = HsEnv,
	       ssl_options = #{user_lookup_fun := PSKLookup}} = State0, Connection)
  when KexAlg == psk ->
    case ssl_handshake:premaster_secret({KexAlg, PSKIdentity}, PSKLookup) of
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State0);
	PremasterSecret ->
	    State = master_secret(PremasterSecret,
				  State0#state{handshake_env =
                                                   HsEnv#handshake_env{premaster_secret = PremasterSecret}}),
            client_certify_and_key_exchange(State, Connection)
    end;
certify(internal, #server_hello_done{},
	#state{static_env = #static_env{role = client},
               connection_env = #connection_env{negotiated_version = {Major, Minor}} = Version,
               handshake_env = #handshake_env{kex_algorithm = KexAlg,
                                              premaster_secret = undefined,
                                              server_psk_identity = PSKIdentity} = HsEnv,
               session = #session{master_secret = undefined},
	       ssl_options = #{user_lookup_fun := PSKLookup}} = State0, Connection)
  when KexAlg == rsa_psk ->
    Rand = ssl_cipher:random_bytes(?NUM_OF_PREMASTERSECRET_BYTES-2),
    RSAPremasterSecret = <<?BYTE(Major), ?BYTE(Minor), Rand/binary>>,
    case ssl_handshake:premaster_secret({KexAlg, PSKIdentity}, PSKLookup, 
					RSAPremasterSecret) of
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State0);
	PremasterSecret ->
	    State = master_secret(PremasterSecret, 
				  State0#state{handshake_env = 
                                                   HsEnv#handshake_env{premaster_secret = RSAPremasterSecret}}),
	    client_certify_and_key_exchange(State, Connection)
    end;
%% Master secret was determined with help of server-key exchange msg
certify(internal, #server_hello_done{}, 
	#state{static_env = #static_env{role = client},
               connection_env = #connection_env{negotiated_version = Version},
               handshake_env = #handshake_env{premaster_secret = undefined},
               session = #session{master_secret = MasterSecret} = Session,
	       connection_states = ConnectionStates0} = State0, Connection) ->
    case ssl_handshake:master_secret(ssl:tls_version(Version), Session,
				     ConnectionStates0, client) of
	{MasterSecret, ConnectionStates} ->
	    State = State0#state{connection_states = ConnectionStates},
	    client_certify_and_key_exchange(State, Connection);
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State0)
    end;
%% Master secret is calculated from premaster_secret
certify(internal, #server_hello_done{},
	#state{static_env = #static_env{role = client},
               connection_env = #connection_env{negotiated_version = Version},
               handshake_env = #handshake_env{premaster_secret = PremasterSecret},
               session = Session0,
	       connection_states = ConnectionStates0} = State0, Connection) ->
    case ssl_handshake:master_secret(ssl:tls_version(Version), PremasterSecret,
				     ConnectionStates0, client) of
	{MasterSecret, ConnectionStates} ->
	    Session = Session0#session{master_secret = MasterSecret},
	    State = State0#state{connection_states = ConnectionStates,
				 session = Session},
	    client_certify_and_key_exchange(State, Connection);
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State0)
    end;
certify(internal = Type, #client_key_exchange{} = Msg,
	#state{static_env = #static_env{role = server},
	       client_certificate_requested = true,
	       ssl_options = #{fail_if_no_peer_cert := true}} = State,
	Connection) ->
    %% We expect a certificate here
    handle_common_event(Type, Msg, ?FUNCTION_NAME, State, Connection);
certify(internal, #client_key_exchange{exchange_keys = Keys},
	State = #state{handshake_env = #handshake_env{kex_algorithm = KeyAlg}, 
                       connection_env = #connection_env{negotiated_version = Version}}, Connection) ->
    try
	certify_client_key_exchange(ssl_handshake:decode_client_key(Keys, KeyAlg, ssl:tls_version(Version)),
				    State, Connection)
    catch
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State)
    end;
certify(Type, Msg, State, Connection) ->
    handle_common_event(Type, Msg, ?FUNCTION_NAME, State, Connection).
 
%%--------------------------------------------------------------------
-spec cipher(gen_statem:event_type(),
	     #hello_request{} | #certificate_verify{} | #finished{} | term(),
	     #state{}, tls_connection | dtls_connection) ->
		    gen_statem:state_function_result().
%%--------------------------------------------------------------------
cipher({call, From}, Msg, State, Connection) ->
    handle_call(Msg, From, ?FUNCTION_NAME, State, Connection);
cipher(info, Msg, State, _) ->
    handle_info(Msg, ?FUNCTION_NAME, State);
cipher(internal, #certificate_verify{signature = Signature, 
				     hashsign_algorithm = CertHashSign},
       #state{static_env = #static_env{role = server},
              handshake_env = #handshake_env{tls_handshake_history = Hist,
                                             kex_algorithm = KexAlg,
                                             public_key_info = PubKeyInfo} = HsEnv,
              connection_env = #connection_env{negotiated_version = Version},
	      session = #session{master_secret = MasterSecret}
	     } = State, Connection) ->
    
    TLSVersion = ssl:tls_version(Version),
    %% Use negotiated value if TLS-1.2 otherwhise return default
    HashSign = negotiated_hashsign(CertHashSign, KexAlg, PubKeyInfo, TLSVersion),
    case ssl_handshake:certificate_verify(Signature, PubKeyInfo,
					  TLSVersion, HashSign, MasterSecret, Hist) of
	valid ->
	    Connection:next_event(?FUNCTION_NAME, no_record,
				  State#state{handshake_env = HsEnv#handshake_env{cert_hashsign_algorithm = HashSign}});
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State)
    end;
%% client must send a next protocol message if we are expecting it
cipher(internal, #finished{},
       #state{static_env = #static_env{role = server},
              handshake_env = #handshake_env{expecting_next_protocol_negotiation = true,
                                             negotiated_protocol = undefined},
              connection_env = #connection_env{negotiated_version = Version}} = State0,
       _Connection) ->
    handle_own_alert(?ALERT_REC(?FATAL,?UNEXPECTED_MESSAGE), Version, ?FUNCTION_NAME, State0);
cipher(internal, #finished{verify_data = Data} = Finished,
       #state{static_env = #static_env{role = Role,
                                       host = Host,
                                       port = Port},
              handshake_env = #handshake_env{tls_handshake_history = Hist,
                                             expecting_finished = true} = HsEnv,
              connection_env = #connection_env{negotiated_version = Version},
	      session = #session{master_secret = MasterSecret}
	      = Session0,
              ssl_options = SslOpts,
	      connection_states = ConnectionStates0} = State, Connection) ->
    case ssl_handshake:verify_connection(ssl:tls_version(Version), Finished,
					 opposite_role(Role),
					 get_current_prf(ConnectionStates0, read),
					 MasterSecret, Hist) of
        verified ->
	    Session = handle_session(Role, SslOpts, Host, Port, Session0),
	    cipher_role(Role, Data, Session, 
			State#state{handshake_env = HsEnv#handshake_env{expecting_finished = false}}, Connection);
        #alert{} = Alert ->
	    handle_own_alert(Alert, Version, ?FUNCTION_NAME, State)
    end;
%% only allowed to send next_protocol message after change cipher spec
%% & before finished message and it is not allowed during renegotiation
cipher(internal, #next_protocol{selected_protocol = SelectedProtocol},
       #state{static_env = #static_env{role = server},
              handshake_env = #handshake_env{expecting_finished = true,
                                             expecting_next_protocol_negotiation = true} = HsEnv} = State, Connection) ->
    Connection:next_event(?FUNCTION_NAME, no_record, 
			  State#state{handshake_env = HsEnv#handshake_env{negotiated_protocol = SelectedProtocol,
                                                                          expecting_next_protocol_negotiation = false}});
cipher(internal, #change_cipher_spec{type = <<1>>},  #state{handshake_env = HsEnv, connection_states = ConnectionStates0} =
	   State, Connection) ->
    ConnectionStates =
	ssl_record:activate_pending_connection_state(ConnectionStates0, read, Connection),
    Connection:next_event(?FUNCTION_NAME, no_record, State#state{handshake_env = HsEnv#handshake_env{expecting_finished = true},
                                                                 connection_states = ConnectionStates});
cipher(Type, Msg, State, Connection) ->
    handle_common_event(Type, Msg, ?FUNCTION_NAME, State, Connection).

%%--------------------------------------------------------------------
-spec connection(gen_statem:event_type(), term(), 
		 #state{}, tls_connection | dtls_connection) ->
			gen_statem:state_function_result().
%%--------------------------------------------------------------------
connection({call, RecvFrom}, {recv, N, Timeout},  
	   #state{static_env = #static_env{protocol_cb = Connection},
                  socket_options =
                      #socket_options{active = false}} = State0, Connection) ->
    passive_receive(State0#state{bytes_to_read = N,
                                 start_or_recv_from = RecvFrom}, ?FUNCTION_NAME, Connection,
                    [{{timeout, recv}, Timeout, timeout}]);

connection({call, From}, renegotiate, #state{static_env = #static_env{protocol_cb = Connection},
                                             handshake_env = HsEnv} = State,
	   Connection) ->
    Connection:renegotiate(State#state{handshake_env = HsEnv#handshake_env{renegotiation = {true, From}}}, []);
connection({call, From}, peer_certificate, 
	   #state{session = #session{peer_certificate = Cert}} = State, _) ->
    hibernate_after(?FUNCTION_NAME, State, [{reply, From,  {ok, Cert}}]); 
connection({call, From}, {connection_information, true}, State, _) ->
    Info = connection_info(State) ++ security_info(State),
    hibernate_after(?FUNCTION_NAME, State, [{reply, From, {ok, Info}}]);
connection({call, From}, {connection_information, false}, State, _) ->
    Info = connection_info(State),
    hibernate_after(?FUNCTION_NAME, State, [{reply, From, {ok, Info}}]);
connection({call, From}, negotiated_protocol, 
	   #state{handshake_env = #handshake_env{alpn = undefined,
                                                 negotiated_protocol = undefined}} = State, _) ->
    hibernate_after(?FUNCTION_NAME, State, [{reply, From, {error, protocol_not_negotiated}}]);
connection({call, From}, negotiated_protocol, 
	   #state{handshake_env = #handshake_env{alpn = undefined,
                                                 negotiated_protocol = SelectedProtocol}} = State, _) ->
    hibernate_after(?FUNCTION_NAME, State,
		    [{reply, From, {ok, SelectedProtocol}}]);
connection({call, From}, negotiated_protocol,
	   #state{handshake_env = #handshake_env{alpn = SelectedProtocol,
                                                 negotiated_protocol = undefined}} = State, _) ->
    hibernate_after(?FUNCTION_NAME, State,
		    [{reply, From, {ok, SelectedProtocol}}]);
connection({call, From}, Msg, State, Connection) ->
    handle_call(Msg, From, ?FUNCTION_NAME, State, Connection);
connection(cast, {internal_renegotiate, WriteState}, #state{static_env = #static_env{protocol_cb = Connection},
                                                            handshake_env = HsEnv,
                                                            connection_states = ConnectionStates} 
           = State, Connection) -> 
    Connection:renegotiate(State#state{handshake_env = HsEnv#handshake_env{renegotiation = {true, internal}},
                                       connection_states = ConnectionStates#{current_write => WriteState}}, []);
connection(cast, {dist_handshake_complete, DHandle},
           #state{ssl_options = #{erl_dist := true},
                  connection_env = CEnv,
                  socket_options = SockOpts} = State0, Connection) ->
    process_flag(priority, normal),
    State1 =
        State0#state{
          socket_options = SockOpts#socket_options{active = true},
          connection_env = CEnv#connection_env{erl_dist_handle = DHandle},
          bytes_to_read = undefined},
    {Record, State} = read_application_data(<<>>, State1),
    Connection:next_event(connection, Record, State);
connection(info, Msg, State, _) ->
    handle_info(Msg, ?FUNCTION_NAME, State);
connection(internal, {recv, Timeout}, State, Connection) ->
    passive_receive(State, ?FUNCTION_NAME, Connection, [{{timeout, recv}, Timeout, timeout}]);
connection(Type, Msg, State, Connection) ->
    handle_common_event(Type, Msg, ?FUNCTION_NAME, State, Connection).

%%--------------------------------------------------------------------
-spec downgrade(gen_statem:event_type(), term(), 
		#state{}, tls_connection | dtls_connection) ->
		       gen_statem:state_function_result().
%%--------------------------------------------------------------------
downgrade(Type, Event, State, Connection) ->
    handle_common_event(Type, Event, ?FUNCTION_NAME, State, Connection).

%%--------------------------------------------------------------------
%% Event handling functions called by state functions to handle
%% common or unexpected events for the state.
%%--------------------------------------------------------------------
handle_common_event(internal, {handshake, {#hello_request{} = Handshake, _}}, connection = StateName,  
		    #state{static_env = #static_env{role = client}, 
                           handshake_env = HsEnv} = State, _) ->
    %% Should not be included in handshake history
    {next_state, StateName, State#state{handshake_env = HsEnv#handshake_env{renegotiation = {true, peer}}},
     [{next_event, internal, Handshake}]};
handle_common_event(internal, {handshake, {#hello_request{}, _}}, StateName,
                    #state{static_env = #static_env{role = client}}, _)
  when StateName =/= connection ->
    keep_state_and_data;
handle_common_event(internal, {handshake, {Handshake, Raw}}, StateName,
		    #state{handshake_env = #handshake_env{tls_handshake_history = Hist0}} = State0,
		    Connection) ->
   
    PossibleSNI = Connection:select_sni_extension(Handshake),
    %% This function handles client SNI hello extension when Handshake is
    %% a client_hello, which needs to be determined by the connection callback.
    %% In other cases this is a noop
    State = #state{handshake_env = HsEnv} = handle_sni_extension(PossibleSNI, State0),

    Hist = ssl_handshake:update_handshake_history(Hist0, Raw),
    {next_state, StateName, State#state{handshake_env = HsEnv#handshake_env{tls_handshake_history = Hist}}, 
     [{next_event, internal, Handshake}]};
handle_common_event(internal, {protocol_record, TLSorDTLSRecord}, StateName, State, Connection) -> 
    Connection:handle_protocol_record(TLSorDTLSRecord, StateName, State);
handle_common_event(timeout, hibernate, _, _, _) ->
    {keep_state_and_data, [hibernate]};
handle_common_event(internal, #change_cipher_spec{type = <<1>>}, StateName, 
		    #state{connection_env = #connection_env{negotiated_version = Version}} = State,  _) ->
    handle_own_alert(?ALERT_REC(?FATAL, ?HANDSHAKE_FAILURE), Version, 
				StateName, State);
handle_common_event({timeout, handshake}, close, _StateName, #state{start_or_recv_from = StartFrom} = State, _) ->
    {stop_and_reply,
     {shutdown, user_timeout},
     {reply, StartFrom, {error, timeout}}, State#state{start_or_recv_from = undefined}};
handle_common_event({timeout, recv}, timeout, StateName, #state{start_or_recv_from = RecvFrom} = State, _) ->
    {next_state, StateName, State#state{start_or_recv_from = undefined,
                                        bytes_to_read = undefined}, [{reply, RecvFrom, {error, timeout}}]};
handle_common_event(Type, Msg, StateName, #state{connection_env =
                                                      #connection_env{negotiated_version = Version}} = State, 
		    _) ->
    Alert =  ?ALERT_REC(?FATAL,?UNEXPECTED_MESSAGE, {unexpected_msg, {Type,Msg}}),
    handle_own_alert(Alert, Version, StateName, State).

handle_call({application_data, _Data}, _, _, _, _) ->
    %% In renegotiation priorities handshake, send data when handshake is finished
    {keep_state_and_data, [postpone]};
handle_call({close, _} = Close, From, StateName, #state{connection_env = CEnv} = State, _Connection) ->
    %% Run terminate before returning so that the reuseaddr
    %% inet-option works properly
    Result = terminate(Close, StateName, State),
    {stop_and_reply,
     {shutdown, normal},
     {reply, From, Result}, State#state{connection_env = CEnv#connection_env{terminated = true}}};
handle_call({shutdown, read_write = How}, From, StateName,
	    #state{static_env = #static_env{transport_cb = Transport,
                                            socket = Socket},
                   connection_env = CEnv} = State, _) ->
    try send_alert(?ALERT_REC(?WARNING, ?CLOSE_NOTIFY),
                   StateName, State) of
        _ -> 
            case Transport:shutdown(Socket, How) of
                ok ->
                    {next_state, StateName, State#state{connection_env = 
                                                            CEnv#connection_env{terminated = true}},
                     [{reply, From, ok}]};
                Error ->
                    {stop_and_reply, {shutdown, normal}, {reply, From, Error}, 
                     State#state{connection_env = CEnv#connection_env{terminated = true}}}
            end
    catch
        throw:Return ->
            Return
    end;
handle_call({shutdown, How0}, From, StateName,
	    #state{static_env = #static_env{transport_cb = Transport,
                                            socket = Socket}} = State, _) ->
    case Transport:shutdown(Socket, How0) of
	ok ->
	    {next_state, StateName, State, [{reply, From, ok}]};
	Error ->
            {stop_and_reply, {shutdown, normal}, {reply, From, Error}, State}
    end;
handle_call({recv, _N, _Timeout}, From, _,  
		  #state{socket_options = 
			     #socket_options{active = Active}}, _) when Active =/= false ->
    {keep_state_and_data, [{reply, From, {error, einval}}]};
handle_call({recv, N, Timeout}, RecvFrom, StateName, State, _) ->
    %% Doing renegotiate wait with handling request until renegotiate is
    %% finished. 
    {next_state, StateName, State#state{bytes_to_read = N, start_or_recv_from = RecvFrom}, 
     [{next_event, internal, {recv, RecvFrom}} , {{timeout, recv}, Timeout, timeout}]};
handle_call({new_user, User}, From, StateName, 
            State = #state{connection_env = #connection_env{user_application = {OldMon, _}} = CEnv}, _) ->
    NewMon = erlang:monitor(process, User),
    erlang:demonitor(OldMon, [flush]),
    {next_state, StateName, State#state{connection_env = CEnv#connection_env{user_application = {NewMon, User}}},
     [{reply, From, ok}]};
handle_call({get_opts, OptTags}, From, _,
            #state{static_env = #static_env{socket = Socket,
                                            transport_cb = Transport},
			 socket_options = SockOpts}, Connection) ->
    OptsReply = get_socket_opts(Connection, Transport, Socket, OptTags, SockOpts, []),
    {keep_state_and_data, [{reply, From, OptsReply}]};
handle_call({set_opts, Opts0}, From, StateName, 
	    #state{static_env =  #static_env{socket = Socket,
                                            transport_cb = Transport,
                                            tracker = Tracker},
                   connection_env = 
                       #connection_env{user_application = {_Mon, Pid}},
                   socket_options = Opts1
                  } = State0, Connection) ->
    {Reply, Opts} = set_socket_opts(Connection, Transport, Socket, Opts0, Opts1, []),
    case {proplists:lookup(active, Opts0), Opts} of
        {{_, N}, #socket_options{active=false}} when is_integer(N) ->
            send_user(
              Pid,
              format_passive(
                Connection:pids(State0), Transport, Socket, Tracker, Connection));
        _ ->
            ok
    end,
    State = State0#state{socket_options = Opts},
    handle_active_option(Opts#socket_options.active, StateName, From, Reply, State);
    
handle_call(renegotiate, From, StateName, _, _) when StateName =/= connection ->
    {keep_state_and_data, [{reply, From, {error, already_renegotiating}}]};

handle_call({prf, Secret, Label, Seed, WantedLength}, From, _,
	    #state{connection_states = ConnectionStates,
		   connection_env = #connection_env{negotiated_version = Version}}, _) ->
    #{security_parameters := SecParams} =
	ssl_record:current_connection_state(ConnectionStates, read),
    #security_parameters{master_secret = MasterSecret,
			 client_random = ClientRandom,
			 server_random = ServerRandom,
			 prf_algorithm = PRFAlgorithm} = SecParams,
    Reply = try
		SecretToUse = case Secret of
				  _ when is_binary(Secret) -> Secret;
				  master_secret -> MasterSecret
			      end,
		SeedToUse = lists:reverse(
			      lists:foldl(fun(X, Acc) when is_binary(X) -> [X|Acc];
					     (client_random, Acc) -> [ClientRandom|Acc];
					     (server_random, Acc) -> [ServerRandom|Acc]
					  end, [], Seed)),
		ssl_handshake:prf(ssl:tls_version(Version), PRFAlgorithm, SecretToUse, Label, SeedToUse, WantedLength)
	    catch
		exit:_ -> {error, badarg};
		error:Reason -> {error, Reason}
	    end,
    {keep_state_and_data, [{reply, From, Reply}]};
handle_call(_,_,_,_,_) ->
    {keep_state_and_data, [postpone]}.

handle_info({ErrorTag, Socket, econnaborted}, StateName,  
	    #state{static_env = #static_env{role = Role,
                                            socket = Socket,
                                            transport_cb = Transport,
                                            error_tag = ErrorTag,
                                            tracker = Tracker,
                                            protocol_cb = Connection},
		   start_or_recv_from = StartFrom
		  } = State)  when StateName =/= connection ->
    Pids = Connection:pids(State),
    alert_user(Pids, Transport, Tracker,Socket, 
	       StartFrom, ?ALERT_REC(?FATAL, ?CLOSE_NOTIFY), Role, StateName, Connection),
    {stop, {shutdown, normal}, State};

handle_info({ErrorTag, Socket, Reason}, StateName, #state{static_env = #static_env{socket = Socket,
                                                                                   error_tag = ErrorTag}} = State)  ->
    Report = io_lib:format("SSL: Socket error: ~p ~n", [Reason]),
    ?LOG_ERROR(Report),
    handle_normal_shutdown(?ALERT_REC(?FATAL, ?CLOSE_NOTIFY), StateName, State),
    {stop, {shutdown,normal}, State};

handle_info({'DOWN', MonitorRef, _, _, Reason}, _,
            #state{connection_env = #connection_env{user_application = {MonitorRef, _Pid}},
                   ssl_options = #{erl_dist := true}}) ->
    {stop, {shutdown, Reason}};
handle_info({'DOWN', MonitorRef, _, _, _}, _,
            #state{connection_env = #connection_env{user_application = {MonitorRef, _Pid}}}) ->
    {stop, {shutdown, normal}};
handle_info({'EXIT', Pid, _Reason}, StateName,
            #state{connection_env = #connection_env{user_application = {_MonitorRef, Pid}}} = State) ->
    %% It seems the user application has linked to us
    %% - ignore that and let the monitor handle this
    {next_state, StateName, State};
%%% So that terminate will be run when supervisor issues shutdown
handle_info({'EXIT', _Sup, shutdown}, _StateName, State) ->
    {stop, shutdown, State};
handle_info({'EXIT', Socket, normal}, _StateName, #state{static_env = #static_env{socket = Socket}} = State) ->
    %% Handle as transport close"
    {stop,{shutdown, transport_closed}, State};
handle_info({'EXIT', Socket, Reason}, _StateName, #state{static_env = #static_env{socket = Socket}} = State) ->
    {stop,{shutdown, Reason}, State};

handle_info(allow_renegotiate, StateName, #state{handshake_env = HsEnv} = State) ->
    {next_state, StateName, State#state{handshake_env = HsEnv#handshake_env{allow_renegotiate = true}}};

handle_info(Msg, StateName, #state{static_env = #static_env{socket = Socket, error_tag = Tag}} = State) ->
    Report = io_lib:format("SSL: Got unexpected info: ~p ~n", [{Msg, Tag, Socket}]),
    ?LOG_NOTICE(Report),
    {next_state, StateName, State}.

%%====================================================================
%% general gen_statem callbacks
%%====================================================================
terminate(_, _, #state{connection_env = #connection_env{terminated = true}}) ->
    %% Happens when user closes the connection using ssl:close/1
    %% we want to guarantee that Transport:close has been called
    %% when ssl:close/1 returns unless it is a downgrade where
    %% we want to guarantee that close alert is received before
    %% returning. In both cases terminate has been run manually
    %% before run by gen_statem which will end up here
    ok;
terminate({shutdown, transport_closed} = Reason, 
	  _StateName, #state{static_env = #static_env{protocol_cb = Connection,
                                                      socket = Socket,
                                                      transport_cb = Transport}} = State) ->
    handle_trusted_certs_db(State),
    Connection:close(Reason, Socket, Transport, undefined, undefined);
terminate({shutdown, own_alert}, _StateName, #state{
						static_env = #static_env{protocol_cb = Connection,
                                                                         socket = Socket,
                                                                         transport_cb = Transport}} = State) ->
    handle_trusted_certs_db(State),
    case application:get_env(ssl, alert_timeout) of
	{ok, Timeout} when is_integer(Timeout) ->
	    Connection:close({timeout, Timeout}, Socket, Transport, undefined, undefined);
	_ ->
	    Connection:close({timeout, ?DEFAULT_TIMEOUT}, Socket, Transport, undefined, undefined)
    end;
terminate({shutdown, downgrade = Reason}, downgrade, #state{static_env = #static_env{protocol_cb = Connection,
                                                                                     transport_cb = Transport,
                                                                                     socket = Socket}
                                                           } = State) ->
    handle_trusted_certs_db(State),
    Connection:close(Reason, Socket, Transport, undefined, undefined);
terminate(Reason, connection, #state{static_env = #static_env{
                                                     protocol_cb = Connection,
                                                     transport_cb = Transport,
                                                     socket = Socket},
                                     connection_states = ConnectionStates,
                                     ssl_options = #{padding_check := Check}
                                    } = State) ->
    handle_trusted_certs_db(State),
    Alert = terminate_alert(Reason),
    %% Send the termination ALERT if possible
    catch (ok = Connection:send_alert_in_connection(Alert, State)),
    Connection:close({timeout, ?DEFAULT_TIMEOUT}, Socket, Transport, ConnectionStates, Check);
terminate(Reason, _StateName, #state{static_env = #static_env{transport_cb = Transport,
                                                              protocol_cb = Connection,
                                                              socket = Socket}
				    } = State) ->
    handle_trusted_certs_db(State),
    Connection:close(Reason, Socket, Transport, undefined, undefined).

format_status(normal, [_, StateName, State]) ->
    [{data, [{"State", {StateName, State}}]}];  
format_status(terminate, [_, StateName, State]) ->
    SslOptions = (State#state.ssl_options),
    NewOptions = SslOptions#{password => ?SECRET_PRINTOUT,
                             cert => ?SECRET_PRINTOUT,
                             cacerts => ?SECRET_PRINTOUT,
                             key => ?SECRET_PRINTOUT,
                             dh => ?SECRET_PRINTOUT,
                             psk_identity => ?SECRET_PRINTOUT,
                             srp_identity => ?SECRET_PRINTOUT},
    [{data, [{"State", {StateName, State#state{connection_states = ?SECRET_PRINTOUT,
					       protocol_buffers =  ?SECRET_PRINTOUT,
					       user_data_buffer = ?SECRET_PRINTOUT,
					       handshake_env =  ?SECRET_PRINTOUT,
                                               connection_env = ?SECRET_PRINTOUT,
					       session =  ?SECRET_PRINTOUT,
					       ssl_options = NewOptions,
					       flight_buffer =  ?SECRET_PRINTOUT}
		       }}]}].

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
send_alert(Alert, connection, #state{static_env = #static_env{protocol_cb = Connection}} = State) ->
     Connection:send_alert_in_connection(Alert, State);
send_alert(Alert, _, #state{static_env = #static_env{protocol_cb = Connection}} = State) ->
    Connection:send_alert(Alert, State).

connection_info(#state{static_env = #static_env{protocol_cb = Connection},
                       handshake_env = #handshake_env{sni_hostname = SNIHostname},
                       session = #session{session_id = SessionId,
                                          cipher_suite = CipherSuite, ecc = ECCCurve},
		       connection_env = #connection_env{negotiated_version =  {_,_} = Version}, 
		       ssl_options = Opts}) ->
    RecordCB = record_cb(Connection),
    CipherSuiteDef = #{key_exchange := KexAlg} = ssl_cipher_format:suite_bin_to_map(CipherSuite),
    IsNamedCurveSuite = lists:member(KexAlg,
                                     [ecdh_ecdsa, ecdhe_ecdsa, ecdh_rsa, ecdhe_rsa, ecdh_anon]),
    CurveInfo = case ECCCurve of
		    {namedCurve, Curve} when IsNamedCurveSuite ->
			[{ecc, {named_curve, pubkey_cert_records:namedCurves(Curve)}}];
		    _ ->
			[]
		end,
    [{protocol, RecordCB:protocol_version(Version)},
     {session_id, SessionId},
     {cipher_suite, ssl_cipher_format:suite_legacy(CipherSuiteDef)},
     {selected_cipher_suite, CipherSuiteDef},
     {sni_hostname, SNIHostname} | CurveInfo] ++ ssl_options_list(Opts).

security_info(#state{connection_states = ConnectionStates}) ->
    #{security_parameters :=
	  #security_parameters{client_random = ClientRand, 
                               server_random = ServerRand,
                               master_secret = MasterSecret}} =
	ssl_record:current_connection_state(ConnectionStates, read),
    [{client_random, ClientRand}, {server_random, ServerRand}, {master_secret, MasterSecret}].

do_server_hello(Type, #{next_protocol_negotiation := NextProtocols} =
		    ServerHelloExt,
		#state{connection_env = #connection_env{negotiated_version = Version},
                       handshake_env = HsEnv,
		       session = #session{session_id = SessId},
		       connection_states = ConnectionStates0,
                       ssl_options = #{versions := [HighestVersion|_]}}
		= State0, Connection) when is_atom(Type) ->
    %% TLS 1.3 - Section 4.1.3
    %% Override server random values for TLS 1.3 downgrade protection mechanism.
    ConnectionStates1 = update_server_random(ConnectionStates0, Version, HighestVersion),
    State1 = State0#state{connection_states = ConnectionStates1},
    ServerHello =
	ssl_handshake:server_hello(SessId, ssl:tls_version(Version),
                                   ConnectionStates1, ServerHelloExt),
    State = server_hello(ServerHello,
			 State1#state{handshake_env = HsEnv#handshake_env{expecting_next_protocol_negotiation =
                                                                              NextProtocols =/= undefined}}, Connection),
    case Type of
	new ->
	    new_server_hello(ServerHello, State, Connection);
	resumed ->
	    resumed_server_hello(State, Connection)
    end.

update_server_random(#{pending_read := #{security_parameters := ReadSecParams0} =
                           ReadState0,
                       pending_write := #{security_parameters := WriteSecParams0} =
                           WriteState0} = ConnectionStates,
                     Version, HighestVersion) ->
    ReadRandom = override_server_random(
                   ReadSecParams0#security_parameters.server_random,
                   Version,
                   HighestVersion),
    WriteRandom = override_server_random(
                    WriteSecParams0#security_parameters.server_random,
                    Version,
                    HighestVersion),
    ReadSecParams = ReadSecParams0#security_parameters{server_random = ReadRandom},
    WriteSecParams = WriteSecParams0#security_parameters{server_random = WriteRandom},
    ReadState = ReadState0#{security_parameters => ReadSecParams},
    WriteState = WriteState0#{security_parameters => WriteSecParams},

    ConnectionStates#{pending_read => ReadState, pending_write => WriteState}.

%% TLS 1.3 - Section 4.1.3
%%
%% If negotiating TLS 1.2, TLS 1.3 servers MUST set the last eight bytes
%% of their Random value to the bytes:
%%
%%   44 4F 57 4E 47 52 44 01
%%
%% If negotiating TLS 1.1 or below, TLS 1.3 servers MUST and TLS 1.2
%% servers SHOULD set the last eight bytes of their Random value to the
%% bytes:
%%
%%   44 4F 57 4E 47 52 44 00
override_server_random(<<Random0:24/binary,_:8/binary>> = Random, {M,N}, {Major,Minor})
  when Major > 3 orelse Major =:= 3 andalso Minor >= 4 -> %% TLS 1.3 or above
    if M =:= 3 andalso N =:= 3 ->                         %% Negotating TLS 1.2
            Down = ?RANDOM_OVERRIDE_TLS12,
            <<Random0/binary,Down/binary>>;
       M =:= 3 andalso N < 3 ->                           %% Negotating TLS 1.1 or prior
            Down = ?RANDOM_OVERRIDE_TLS11,
            <<Random0/binary,Down/binary>>;
       true ->
            Random
    end;
override_server_random(<<Random0:24/binary,_:8/binary>> = Random, {M,N}, {Major,Minor})
  when Major =:= 3 andalso Minor =:= 3 ->   %% TLS 1.2
    if M =:= 3 andalso N < 3 ->             %% Negotating TLS 1.1 or prior
            Down = ?RANDOM_OVERRIDE_TLS11,
            <<Random0/binary,Down/binary>>;
       true ->
            Random
    end;
override_server_random(Random, _, _) ->
    Random.

new_server_hello(#server_hello{cipher_suite = CipherSuite,
			      compression_method = Compression,
			      session_id = SessionId},
                 #state{session = Session0,
                        connection_env = #connection_env{negotiated_version = Version}} = State0, Connection) ->
    try server_certify_and_key_exchange(State0, Connection) of
        #state{} = State1 ->
            {State, Actions} = server_hello_done(State1, Connection),
	    Session =
		Session0#session{session_id = SessionId,
				 cipher_suite = CipherSuite,
				 compression_method = Compression},
	    Connection:next_event(certify, no_record, State#state{session = Session}, Actions)
    catch
        #alert{} = Alert ->
	    handle_own_alert(Alert, Version, hello, State0)
    end.

resumed_server_hello(#state{session = Session,
			    connection_states = ConnectionStates0,
			    connection_env = #connection_env{negotiated_version = Version}} = State0, Connection) ->

    case ssl_handshake:master_secret(ssl:tls_version(Version), Session,
				     ConnectionStates0, server) of
	{_, ConnectionStates1} ->
	    State1 = State0#state{connection_states = ConnectionStates1,
				  session = Session},
	    {State, Actions} =
		finalize_handshake(State1, abbreviated, Connection),
	    Connection:next_event(abbreviated, no_record, State, Actions);
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, hello, State0)
    end.

server_hello(ServerHello, State0, Connection) ->
    CipherSuite = ServerHello#server_hello.cipher_suite,
    #{key_exchange := KeyAlgorithm}  = ssl_cipher_format:suite_bin_to_map(CipherSuite),
    #state{handshake_env = HsEnv} = State = Connection:queue_handshake(ServerHello, State0),
    State#state{handshake_env = HsEnv#handshake_env{kex_algorithm = KeyAlgorithm}}.

server_hello_done(State, Connection) ->
    HelloDone = ssl_handshake:server_hello_done(),
    Connection:send_handshake(HelloDone, State).

handle_peer_cert(Role, PeerCert, PublicKeyInfo,
		 #state{handshake_env = HsEnv,
                    session = #session{cipher_suite = CipherSuite} = Session} = State0,
		 Connection) ->
    State1 = State0#state{handshake_env = HsEnv#handshake_env{public_key_info = PublicKeyInfo},
                          session =
                              Session#session{peer_certificate = PeerCert}},
    #{key_exchange := KeyAlgorithm} = ssl_cipher_format:suite_bin_to_map(CipherSuite),
    State = handle_peer_cert_key(Role, PeerCert, PublicKeyInfo, KeyAlgorithm, State1),
    Connection:next_event(certify, no_record, State).

handle_peer_cert_key(client, _,
		     {?'id-ecPublicKey',  #'ECPoint'{point = _ECPoint} = PublicKey,
		      PublicKeyParams},
		     KeyAlg, #state{handshake_env = HsEnv,
                                    session = Session} = State)  when KeyAlg == ecdh_rsa;
                                                                      KeyAlg == ecdh_ecdsa ->
    ECDHKey = public_key:generate_key(PublicKeyParams),
    PremasterSecret = ssl_handshake:premaster_secret(PublicKey, ECDHKey),
    master_secret(PremasterSecret, State#state{handshake_env = HsEnv#handshake_env{kex_keys = ECDHKey},
                                               session = Session#session{ecc = PublicKeyParams}});
handle_peer_cert_key(_, _, _, _, State) ->
    State.

certify_client(#state{static_env = #static_env{role = client,
                                               cert_db = CertDbHandle,
                                               cert_db_ref = CertDbRef},
                      client_certificate_requested = true,
		      session = #session{own_certificate = OwnCert}}
	       = State, Connection) ->
    Certificate = ssl_handshake:certificate(OwnCert, CertDbHandle, CertDbRef, client),
    Connection:queue_handshake(Certificate, State);
certify_client(#state{client_certificate_requested = false} = State, _) ->
    State.

verify_client_cert(#state{static_env = #static_env{role = client},
                          handshake_env = #handshake_env{tls_handshake_history = Hist,
                                                         cert_hashsign_algorithm = HashSign},
                          connection_env = #connection_env{negotiated_version = Version,
                                                           private_key = PrivateKey},
                          client_certificate_requested = true,
			  session = #session{master_secret = MasterSecret,
					     own_certificate = OwnCert}} = State, Connection) ->

    case ssl_handshake:client_certificate_verify(OwnCert, MasterSecret,
						 ssl:tls_version(Version), HashSign, PrivateKey, Hist) of
        #certificate_verify{} = Verified ->
           Connection:queue_handshake(Verified, State);
	ignore ->
	    State;
	#alert{} = Alert ->
	    throw(Alert)
    end;
verify_client_cert(#state{client_certificate_requested = false} = State, _) ->
    State.

client_certify_and_key_exchange(#state{connection_env = #connection_env{negotiated_version = Version}} =
				State0, Connection) ->
    try do_client_certify_and_key_exchange(State0, Connection) of
        State1 = #state{} ->
	    {State2, Actions} = finalize_handshake(State1, certify, Connection),
            State = State2#state{
                      %% Reinitialize
                      client_certificate_requested = false},
	    Connection:next_event(cipher, no_record, State, Actions)
    catch
        throw:#alert{} = Alert ->
	    handle_own_alert(Alert, Version, certify, State0)
    end.

do_client_certify_and_key_exchange(State0, Connection) ->
    State1 = certify_client(State0, Connection),
    State2 = key_exchange(State1, Connection),
    verify_client_cert(State2, Connection).

server_certify_and_key_exchange(State0, Connection) ->
    State1 = certify_server(State0, Connection),
    State2 = key_exchange(State1, Connection),
    request_client_cert(State2, Connection).

certify_client_key_exchange(#encrypted_premaster_secret{premaster_secret= EncPMS},
			    #state{connection_env = #connection_env{private_key = Key}, 
                                   handshake_env = #handshake_env{client_hello_version = {Major, Minor} = Version}}
                            = State, Connection) ->
    FakeSecret = make_premaster_secret(Version, rsa),
    %% Countermeasure for Bleichenbacher attack always provide some kind of premaster secret
    %% and fail handshake later.RFC 5246 section 7.4.7.1.
    PremasterSecret =
        try ssl_handshake:premaster_secret(EncPMS, Key) of
            Secret when erlang:byte_size(Secret) == ?NUM_OF_PREMASTERSECRET_BYTES ->
                case Secret of
                    <<?BYTE(Major), ?BYTE(Minor), Rest/binary>> -> %% Correct
                        <<?BYTE(Major), ?BYTE(Minor), Rest/binary>>;
                    <<?BYTE(_), ?BYTE(_), Rest/binary>> -> %% Version mismatch
                        <<?BYTE(Major), ?BYTE(Minor), Rest/binary>>
                end;
            _ -> %% erlang:byte_size(Secret) =/= ?NUM_OF_PREMASTERSECRET_BYTES
                FakeSecret
        catch 
            #alert{description = ?DECRYPT_ERROR} ->
                FakeSecret
        end,    
    calculate_master_secret(PremasterSecret, State, Connection, certify, cipher);
certify_client_key_exchange(#client_diffie_hellman_public{dh_public = ClientPublicDhKey},
			    #state{handshake_env = #handshake_env{diffie_hellman_params = #'DHParameter'{} = Params,
                                                                  kex_keys = {_, ServerDhPrivateKey}}
				  } = State,
			    Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientPublicDhKey, ServerDhPrivateKey, Params),
    calculate_master_secret(PremasterSecret, State, Connection, certify, cipher);

certify_client_key_exchange(#client_ec_diffie_hellman_public{dh_public = ClientPublicEcDhPoint},
			    #state{handshake_env = #handshake_env{kex_keys = ECDHKey}} = State, Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(#'ECPoint'{point = ClientPublicEcDhPoint}, ECDHKey),
    calculate_master_secret(PremasterSecret, State, Connection, certify, cipher);
certify_client_key_exchange(#client_psk_identity{} = ClientKey,
			    #state{ssl_options = 
				       #{user_lookup_fun := PSKLookup}} = State0,
			    Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientKey, PSKLookup),
    calculate_master_secret(PremasterSecret, State0, Connection, certify, cipher);
certify_client_key_exchange(#client_dhe_psk_identity{} = ClientKey,
			    #state{handshake_env = #handshake_env{diffie_hellman_params = #'DHParameter'{} = Params,
                                                                  kex_keys = {_, ServerDhPrivateKey}},
				   ssl_options = 
				       #{user_lookup_fun := PSKLookup}} = State0,
			    Connection) ->
    PremasterSecret = 
	ssl_handshake:premaster_secret(ClientKey, ServerDhPrivateKey, Params, PSKLookup),
    calculate_master_secret(PremasterSecret, State0, Connection, certify, cipher);
certify_client_key_exchange(#client_ecdhe_psk_identity{} = ClientKey,
			    #state{handshake_env = #handshake_env{kex_keys = ServerEcDhPrivateKey},
				   ssl_options =
				       #{user_lookup_fun := PSKLookup}} = State,
			    Connection) ->
    PremasterSecret =
	ssl_handshake:premaster_secret(ClientKey, ServerEcDhPrivateKey, PSKLookup),
    calculate_master_secret(PremasterSecret, State, Connection, certify, cipher);
certify_client_key_exchange(#client_rsa_psk_identity{} = ClientKey,
			    #state{connection_env = #connection_env{private_key = Key},
				   ssl_options = 
				       #{user_lookup_fun := PSKLookup}} = State0,
			    Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientKey, Key, PSKLookup),
    calculate_master_secret(PremasterSecret, State0, Connection, certify, cipher);
certify_client_key_exchange(#client_srp_public{} = ClientKey,
			    #state{handshake_env = #handshake_env{srp_params = Params,
                                                                  kex_keys = Key}
				  } = State0, Connection) ->
    PremasterSecret = ssl_handshake:premaster_secret(ClientKey, Key, Params),
    calculate_master_secret(PremasterSecret, State0, Connection, certify, cipher).

certify_server(#state{handshake_env = #handshake_env{kex_algorithm = KexAlg}} = 
                   State, _) when KexAlg == dh_anon; 
                                  KexAlg == ecdh_anon; 
                                  KexAlg == psk; 
                                  KexAlg == dhe_psk; 
                                  KexAlg == ecdhe_psk; 
                                  KexAlg == srp_anon  ->
    State;
certify_server(#state{static_env = #static_env{cert_db = CertDbHandle,
                                               cert_db_ref = CertDbRef},
		      session = #session{own_certificate = OwnCert}} = State, Connection) ->
    case ssl_handshake:certificate(OwnCert, CertDbHandle, CertDbRef, server) of
	Cert = #certificate{} ->
	    Connection:queue_handshake(Cert, State);
	Alert = #alert{} ->
	    throw(Alert)
    end.

key_exchange(#state{static_env = #static_env{role = server}, 
                    handshake_env = #handshake_env{kex_algorithm = rsa}} = State,_) ->
    State;
key_exchange(#state{static_env = #static_env{role = server}, 
		    handshake_env = #handshake_env{kex_algorithm = KexAlg,
                                                   diffie_hellman_params = #'DHParameter'{} = Params,
                                                   hashsign_algorithm = HashSignAlgo},
                    connection_env = #connection_env{negotiated_version = Version,
                                                     private_key = PrivateKey},
		    connection_states = ConnectionStates0} = State0, Connection)
  when KexAlg == dhe_dss;
       KexAlg == dhe_rsa;
       KexAlg == dh_anon ->
    DHKeys = public_key:generate_key(Params),
    #{security_parameters := SecParams} =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg = ssl_handshake:key_exchange(server, ssl:tls_version(Version), {dh, DHKeys, Params,
					       HashSignAlgo, ClientRandom,
					       ServerRandom,
					       PrivateKey}),
    #state{handshake_env = HsEnv} = State = Connection:queue_handshake(Msg, State0),
    State#state{handshake_env = HsEnv#handshake_env{kex_keys = DHKeys}};
key_exchange(#state{static_env = #static_env{role = server},
                    handshake_env = #handshake_env{kex_algorithm = KexAlg} = HsEnv,
                    connection_env = #connection_env{private_key = #'ECPrivateKey'{parameters = ECCurve} = Key},
                   session = Session} = State, _)
  when KexAlg == ecdh_ecdsa; 
       KexAlg == ecdh_rsa ->
    State#state{handshake_env = HsEnv#handshake_env{kex_keys = Key},
                session = Session#session{ecc = ECCurve}};
key_exchange(#state{static_env = #static_env{role = server}, 
                    handshake_env = #handshake_env{kex_algorithm = KexAlg,
                                                   hashsign_algorithm = HashSignAlgo},
                    connection_env = #connection_env{negotiated_version = Version,
                                                     private_key = PrivateKey},
		    session = #session{ecc = ECCCurve},
		    connection_states = ConnectionStates0} = State0, Connection)
  when KexAlg == ecdhe_ecdsa; 
       KexAlg == ecdhe_rsa;
       KexAlg == ecdh_anon ->

    ECDHKeys = public_key:generate_key(ECCCurve),
    #{security_parameters := SecParams} = 
	ssl_record:pending_connection_state(ConnectionStates0, read),
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg =  ssl_handshake:key_exchange(server, ssl:tls_version(Version), 
				      {ecdh, ECDHKeys,
				       HashSignAlgo, ClientRandom,
				       ServerRandom,
				       PrivateKey}),
    #state{handshake_env = HsEnv} = State = Connection:queue_handshake(Msg, State0),
    State#state{handshake_env = HsEnv#handshake_env{kex_keys = ECDHKeys}};
key_exchange(#state{static_env = #static_env{role = server}, 
                    handshake_env = #handshake_env{kex_algorithm = psk},
		    ssl_options = #{psk_identity := undefined}} = State, _) ->
    State;
key_exchange(#state{static_env = #static_env{role = server}, 
		    ssl_options = #{psk_identity := PskIdentityHint},
		    handshake_env = #handshake_env{kex_algorithm = psk,
                                                   hashsign_algorithm = HashSignAlgo},     
                    connection_env = #connection_env{negotiated_version = Version,
                                                     private_key = PrivateKey},
             connection_states = ConnectionStates0} = State0, Connection) ->
    #{security_parameters := SecParams} = 
	ssl_record:pending_connection_state(ConnectionStates0, read),
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg = ssl_handshake:key_exchange(server, ssl:tls_version(Version), 
				     {psk, PskIdentityHint,
				      HashSignAlgo, ClientRandom,
				      ServerRandom,
                                      PrivateKey}),
    Connection:queue_handshake(Msg, State0);
key_exchange(#state{static_env = #static_env{role = server},                    
		    ssl_options = #{psk_identity := PskIdentityHint},
		    handshake_env = #handshake_env{kex_algorithm = dhe_psk,
                                                   diffie_hellman_params = #'DHParameter'{} = Params,
                                                   hashsign_algorithm = HashSignAlgo},                                        
                    connection_env = #connection_env{negotiated_version = Version,
                                                     private_key = PrivateKey},
		    connection_states = ConnectionStates0
		   } = State0, Connection) ->
    DHKeys = public_key:generate_key(Params),
    #{security_parameters := SecParams} =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg =  ssl_handshake:key_exchange(server, ssl:tls_version(Version), 
				      {dhe_psk, 
				       PskIdentityHint, DHKeys, Params,
				       HashSignAlgo, ClientRandom,
				       ServerRandom,
				       PrivateKey}),
    #state{handshake_env = HsEnv} = State = Connection:queue_handshake(Msg, State0),
    State#state{handshake_env = HsEnv#handshake_env{kex_keys = DHKeys}};
key_exchange(#state{static_env = #static_env{role = server}, 
		    ssl_options = #{psk_identity := PskIdentityHint},
                    handshake_env = #handshake_env{kex_algorithm = ecdhe_psk,
                                                   hashsign_algorithm = HashSignAlgo},
                    connection_env = #connection_env{negotiated_version = Version,
                                                     private_key = PrivateKey},
                    session = #session{ecc = ECCCurve},
		    connection_states = ConnectionStates0
		   } = State0, Connection) ->
    ECDHKeys = public_key:generate_key(ECCCurve),
    #{security_parameters := SecParams} =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg =  ssl_handshake:key_exchange(server, ssl:tls_version(Version),
				      {ecdhe_psk,
				       PskIdentityHint, ECDHKeys,
				       HashSignAlgo, ClientRandom,
				       ServerRandom,
				       PrivateKey}),
    #state{handshake_env = HsEnv} = State = Connection:queue_handshake(Msg, State0),
    State#state{handshake_env = HsEnv#handshake_env{kex_keys = ECDHKeys}};
key_exchange(#state{static_env = #static_env{role = server}, 
                    handshake_env = #handshake_env{kex_algorithm = rsa_psk},
		    ssl_options = #{psk_identity := undefined}} = State, _) ->
    State;
key_exchange(#state{static_env = #static_env{role = server}, 
		    ssl_options = #{psk_identity := PskIdentityHint},
                    handshake_env = #handshake_env{kex_algorithm = rsa_psk,
                                                   hashsign_algorithm = HashSignAlgo}, 
                    connection_env = #connection_env{negotiated_version = Version,
                                                     private_key = PrivateKey},
		    connection_states = ConnectionStates0
		   } = State0, Connection) ->
    #{security_parameters := SecParams} =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg =  ssl_handshake:key_exchange(server, ssl:tls_version(Version), 
				      {psk, PskIdentityHint,
				       HashSignAlgo, ClientRandom,
				       ServerRandom,
				       PrivateKey}),
    Connection:queue_handshake(Msg, State0);
key_exchange(#state{static_env = #static_env{role = server}, 
		    ssl_options = #{user_lookup_fun := LookupFun},
                    handshake_env = #handshake_env{kex_algorithm = KexAlg,
                                                   hashsign_algorithm = HashSignAlgo}, 
                    connection_env = #connection_env{negotiated_version = Version,
                                                     private_key = PrivateKey},
		    session = #session{srp_username = Username},
		    connection_states = ConnectionStates0
		   } = State0, Connection)
  when KexAlg == srp_dss;
       KexAlg == srp_rsa;
       KexAlg == srp_anon ->
    SrpParams = handle_srp_identity(Username, LookupFun),
    Keys = case generate_srp_server_keys(SrpParams, 0) of
	       Alert = #alert{} ->
		   throw(Alert);
	       Keys0 = {_,_} ->
		   Keys0
	   end,
    #{security_parameters := SecParams} =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,
    Msg =  ssl_handshake:key_exchange(server, ssl:tls_version(Version),
				      {srp, Keys, SrpParams,
				       HashSignAlgo, ClientRandom,
				       ServerRandom,
				       PrivateKey}),
    #state{handshake_env = HsEnv} = State = Connection:queue_handshake(Msg, State0),
    State#state{handshake_env = HsEnv#handshake_env{srp_params = SrpParams,
                                                    kex_keys = Keys}};
key_exchange(#state{static_env = #static_env{role = client},
                    handshake_env = #handshake_env{kex_algorithm = rsa,
                                                   public_key_info = PublicKeyInfo,
                                                   premaster_secret = PremasterSecret},
                    connection_env = #connection_env{negotiated_version = Version}
		   } = State0, Connection) ->
    Msg = rsa_key_exchange(ssl:tls_version(Version), PremasterSecret, PublicKeyInfo),
    Connection:queue_handshake(Msg, State0);
key_exchange(#state{static_env = #static_env{role = client},
                    handshake_env = #handshake_env{kex_algorithm = KexAlg,
                                                   kex_keys = {DhPubKey, _}},
                    connection_env = #connection_env{negotiated_version = Version}
                   } = State0, Connection)
  when KexAlg == dhe_dss;
       KexAlg == dhe_rsa;
       KexAlg == dh_anon ->
    Msg =  ssl_handshake:key_exchange(client, ssl:tls_version(Version), {dh, DhPubKey}),
    Connection:queue_handshake(Msg, State0);

key_exchange(#state{static_env = #static_env{role = client},
                    handshake_env = #handshake_env{kex_algorithm = KexAlg,
                                                   kex_keys = #'ECPrivateKey'{parameters = ECCurve} = Key},
                    connection_env = #connection_env{negotiated_version = Version},
                    session = Session
		   } = State0, Connection)
  when KexAlg == ecdhe_ecdsa; 
       KexAlg == ecdhe_rsa;
       KexAlg == ecdh_ecdsa; 
       KexAlg == ecdh_rsa;
       KexAlg == ecdh_anon ->
    Msg = ssl_handshake:key_exchange(client, ssl:tls_version(Version), {ecdh, Key}),
    Connection:queue_handshake(Msg, State0#state{session = Session#session{ecc = ECCurve}});
key_exchange(#state{static_env = #static_env{role = client},
                    handshake_env = #handshake_env{kex_algorithm = psk},
                    connection_env = #connection_env{negotiated_version = Version},
		    ssl_options = #{psk_identity := PSKIdentity}} = State0, Connection) ->
    Msg =  ssl_handshake:key_exchange(client, ssl:tls_version(Version), 
				      {psk, PSKIdentity}),
    Connection:queue_handshake(Msg, State0);
key_exchange(#state{static_env = #static_env{role = client},
                    handshake_env = #handshake_env{kex_algorithm = dhe_psk,
                                                   kex_keys = {DhPubKey, _}},
                    connection_env = #connection_env{negotiated_version = Version},
		    ssl_options = #{psk_identity := PSKIdentity}} = State0, Connection) ->
    Msg =  ssl_handshake:key_exchange(client, ssl:tls_version(Version),
				      {dhe_psk, 
				       PSKIdentity, DhPubKey}),
    Connection:queue_handshake(Msg, State0);

key_exchange(#state{static_env = #static_env{role = client},
                    handshake_env = #handshake_env{kex_algorithm = ecdhe_psk,
                                                   kex_keys = ECDHKeys},
                    connection_env = #connection_env{negotiated_version = Version},
		    ssl_options = #{psk_identity := PSKIdentity}} = State0, Connection) ->
    Msg =  ssl_handshake:key_exchange(client, ssl:tls_version(Version),
				      {ecdhe_psk,
				       PSKIdentity, ECDHKeys}),
    Connection:queue_handshake(Msg, State0);

key_exchange(#state{static_env = #static_env{role = client},
                    handshake_env = #handshake_env{kex_algorithm = rsa_psk,
                                                   public_key_info = PublicKeyInfo,
                                                   premaster_secret = PremasterSecret},
                    connection_env = #connection_env{negotiated_version = Version},
		    ssl_options = #{psk_identity := PSKIdentity}}
	     = State0, Connection) ->
    Msg = rsa_psk_key_exchange(ssl:tls_version(Version), PSKIdentity,
			       PremasterSecret, PublicKeyInfo),
    Connection:queue_handshake(Msg, State0);
key_exchange(#state{static_env = #static_env{role = client},
                    handshake_env = #handshake_env{kex_algorithm = KexAlg,
                                                   kex_keys = {ClientPubKey, _}},
                    connection_env = #connection_env{negotiated_version = Version}}
	     = State0, Connection)
  when KexAlg == srp_dss;
       KexAlg == srp_rsa;
       KexAlg == srp_anon ->
    Msg =  ssl_handshake:key_exchange(client, ssl:tls_version(Version), {srp, ClientPubKey}),
    Connection:queue_handshake(Msg, State0).

rsa_key_exchange(Version, PremasterSecret, PublicKeyInfo = {Algorithm, _, _})
  when Algorithm == ?rsaEncryption;
       Algorithm == ?md2WithRSAEncryption;
       Algorithm == ?md5WithRSAEncryption;
       Algorithm == ?sha1WithRSAEncryption;
       Algorithm == ?sha224WithRSAEncryption;
       Algorithm == ?sha256WithRSAEncryption;
       Algorithm == ?sha384WithRSAEncryption;
       Algorithm == ?sha512WithRSAEncryption
       ->
    ssl_handshake:key_exchange(client, ssl:tls_version(Version),
			       {premaster_secret, PremasterSecret,
				PublicKeyInfo});
rsa_key_exchange(_, _, _) ->
    throw (?ALERT_REC(?FATAL,?HANDSHAKE_FAILURE, pub_key_is_not_rsa)).

rsa_psk_key_exchange(Version, PskIdentity, PremasterSecret, 
		     PublicKeyInfo = {Algorithm, _, _})
  when Algorithm == ?rsaEncryption;
       Algorithm == ?md2WithRSAEncryption;
       Algorithm == ?md5WithRSAEncryption;
       Algorithm == ?sha1WithRSAEncryption;
       Algorithm == ?sha224WithRSAEncryption;
       Algorithm == ?sha256WithRSAEncryption;
       Algorithm == ?sha384WithRSAEncryption;
       Algorithm == ?sha512WithRSAEncryption
       ->
    ssl_handshake:key_exchange(client, ssl:tls_version(Version),
			       {psk_premaster_secret, PskIdentity, PremasterSecret,
				PublicKeyInfo});
rsa_psk_key_exchange(_, _, _, _) ->
    throw (?ALERT_REC(?FATAL,?HANDSHAKE_FAILURE, pub_key_is_not_rsa)).

request_client_cert(#state{handshake_env = #handshake_env{kex_algorithm = Alg}} = State, _)
  when Alg == dh_anon; 
       Alg == ecdh_anon;
       Alg == psk; 
       Alg == dhe_psk; 
       Alg == ecdhe_psk; 
       Alg == rsa_psk;
       Alg == srp_dss; 
       Alg == srp_rsa; 
       Alg == srp_anon ->
    State;

request_client_cert(#state{static_env = #static_env{cert_db = CertDbHandle,
                                                    cert_db_ref = CertDbRef},
                           connection_env = #connection_env{negotiated_version = Version},
                           ssl_options = #{verify := verify_peer,
                                           signature_algs := SupportedHashSigns},
                           connection_states = ConnectionStates0} = State0, Connection) ->
    #{security_parameters :=
	  #security_parameters{cipher_suite = CipherSuite}} =
	ssl_record:pending_connection_state(ConnectionStates0, read),
    TLSVersion =  ssl:tls_version(Version),
    HashSigns = ssl_handshake:available_signature_algs(SupportedHashSigns, 
						       TLSVersion),
    Msg = ssl_handshake:certificate_request(CipherSuite, CertDbHandle, CertDbRef, 
					    HashSigns, TLSVersion),
    State = Connection:queue_handshake(Msg, State0),
    State#state{client_certificate_requested = true};

request_client_cert(#state{ssl_options = #{verify := verify_none}} =
		    State, _) ->
    State.

calculate_master_secret(PremasterSecret, 
			#state{connection_env = #connection_env{negotiated_version = Version},
			       connection_states = ConnectionStates0,
			       session = Session0} = State0, Connection,
			_Current, Next) ->
    case ssl_handshake:master_secret(ssl:tls_version(Version), PremasterSecret,
				     ConnectionStates0, server) of
	{MasterSecret, ConnectionStates} ->
	    Session = Session0#session{master_secret = MasterSecret},
	    State = State0#state{connection_states = ConnectionStates,
				  session = Session},
	    Connection:next_event(Next, no_record, State);
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, certify, State0)
    end.

finalize_handshake(State0, StateName, Connection) ->
    #state{connection_states = ConnectionStates0} =
	State1 = cipher_protocol(State0, Connection),

    ConnectionStates =
        ssl_record:activate_pending_connection_state(ConnectionStates0,
                                                     write, Connection),

    State2 = State1#state{connection_states = ConnectionStates},
    State = next_protocol(State2, Connection),
    finished(State, StateName, Connection).

next_protocol(#state{static_env = #static_env{role = server}} = State, _) ->
    State;
next_protocol(#state{handshake_env = #handshake_env{negotiated_protocol = undefined}} = State, _) ->
    State;
next_protocol(#state{handshake_env = #handshake_env{expecting_next_protocol_negotiation = false}} = State, _) ->
    State;
next_protocol(#state{handshake_env = #handshake_env{negotiated_protocol = NextProtocol}} = State0, Connection) ->
    NextProtocolMessage = ssl_handshake:next_protocol(NextProtocol),
    Connection:queue_handshake(NextProtocolMessage, State0).

cipher_protocol(State, Connection) ->
    Connection:queue_change_cipher(#change_cipher_spec{}, State).

finished(#state{static_env = #static_env{role = Role},
                handshake_env = #handshake_env{tls_handshake_history = Hist},
                connection_env = #connection_env{negotiated_version = Version},
		session = Session,
                connection_states = ConnectionStates0} = State0, 
         StateName, Connection) ->
    MasterSecret = Session#session.master_secret,
    Finished = ssl_handshake:finished(ssl:tls_version(Version), Role,
				       get_current_prf(ConnectionStates0, write),
				       MasterSecret, Hist),
    ConnectionStates = save_verify_data(Role, Finished, ConnectionStates0, StateName),
    Connection:send_handshake(Finished, State0#state{connection_states =
								 ConnectionStates}).

save_verify_data(client, #finished{verify_data = Data}, ConnectionStates, certify) ->
    ssl_record:set_client_verify_data(current_write, Data, ConnectionStates);
save_verify_data(server, #finished{verify_data = Data}, ConnectionStates, cipher) ->
    ssl_record:set_server_verify_data(current_both, Data, ConnectionStates);
save_verify_data(client, #finished{verify_data = Data}, ConnectionStates, abbreviated) ->
    ssl_record:set_client_verify_data(current_both, Data, ConnectionStates);
save_verify_data(server, #finished{verify_data = Data}, ConnectionStates, abbreviated) ->
    ssl_record:set_server_verify_data(current_write, Data, ConnectionStates).

calculate_secret(#server_dh_params{dh_p = Prime, dh_g = Base, 
				   dh_y = ServerPublicDhKey} = Params,
		 #state{handshake_env = HsEnv} = State, Connection) ->
    Keys = {_, PrivateDhKey} = crypto:generate_key(dh, [Prime, Base]),
    PremasterSecret =
	ssl_handshake:premaster_secret(ServerPublicDhKey, PrivateDhKey, Params),
    calculate_master_secret(PremasterSecret,
			    State#state{handshake_env = HsEnv#handshake_env{kex_keys = Keys}}, 
			    Connection, certify, certify);

calculate_secret(#server_ecdh_params{curve = ECCurve, public = ECServerPubKey},
		     #state{handshake_env = HsEnv,
                            session = Session} = State, Connection) ->
    ECDHKeys = public_key:generate_key(ECCurve),
    PremasterSecret = 
	ssl_handshake:premaster_secret(#'ECPoint'{point = ECServerPubKey}, ECDHKeys),
    calculate_master_secret(PremasterSecret,
			    State#state{handshake_env = HsEnv#handshake_env{kex_keys = ECDHKeys},
					session = Session#session{ecc = ECCurve}},
			    Connection, certify, certify);

calculate_secret(#server_psk_params{
		    hint = IdentityHint},
		 #state{handshake_env = HsEnv} = State, Connection) ->
    %% store for later use
    Connection:next_event(certify, no_record, 
                          State#state{handshake_env = 
                                          HsEnv#handshake_env{server_psk_identity = IdentityHint}});

calculate_secret(#server_dhe_psk_params{
		    dh_params = #server_dh_params{dh_p = Prime, dh_g = Base}} = ServerKey,
		    #state{handshake_env = HsEnv,
                           ssl_options = #{user_lookup_fun := PSKLookup}} =
		     State, Connection) ->
    Keys = {_, PrivateDhKey} =
	crypto:generate_key(dh, [Prime, Base]),
    PremasterSecret = ssl_handshake:premaster_secret(ServerKey, PrivateDhKey, PSKLookup),
    calculate_master_secret(PremasterSecret, State#state{handshake_env = HsEnv#handshake_env{kex_keys = Keys}},
			    Connection, certify, certify);

calculate_secret(#server_ecdhe_psk_params{
                    dh_params = #server_ecdh_params{curve = ECCurve}} = ServerKey,
                 #state{ssl_options = #{user_lookup_fun := PSKLookup}} =
		     #state{handshake_env = HsEnv,
                            session = Session} = State, Connection) ->
    ECDHKeys = public_key:generate_key(ECCurve),

    PremasterSecret = ssl_handshake:premaster_secret(ServerKey, ECDHKeys, PSKLookup),
    calculate_master_secret(PremasterSecret,
			    State#state{handshake_env = HsEnv#handshake_env{kex_keys = ECDHKeys},
					session = Session#session{ecc = ECCurve}},
			    Connection, certify, certify);

calculate_secret(#server_srp_params{srp_n = Prime, srp_g = Generator} = ServerKey,
		 #state{handshake_env = HsEnv,
                        ssl_options = #{srp_identity := SRPId}} = State,
		 Connection) ->
    Keys = generate_srp_client_keys(Generator, Prime, 0),
    PremasterSecret = ssl_handshake:premaster_secret(ServerKey, Keys, SRPId),
    calculate_master_secret(PremasterSecret, State#state{handshake_env = HsEnv#handshake_env{kex_keys = Keys}}, Connection, 
			    certify, certify).

master_secret(#alert{} = Alert, _) ->
    Alert;
master_secret(PremasterSecret, #state{static_env = #static_env{role = Role},
                                      connection_env = #connection_env{negotiated_version = Version},
                                      session = Session,
				      connection_states = ConnectionStates0} = State) ->
    case ssl_handshake:master_secret(ssl:tls_version(Version), PremasterSecret,
				     ConnectionStates0, Role) of
	{MasterSecret, ConnectionStates} ->
	    State#state{
	      session =
		  Session#session{master_secret = MasterSecret},
	      connection_states = ConnectionStates};
	#alert{} = Alert ->
	    Alert
    end.

generate_srp_server_keys(_SrpParams, 10) ->
    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER);
generate_srp_server_keys(SrpParams =
			     #srp_user{generator = Generator, prime = Prime,
				       verifier = Verifier}, N) ->
    try crypto:generate_key(srp, {host, [Verifier, Generator, Prime, '6a']}) of
	Keys ->
	    Keys
    catch
	error:_ ->
	    generate_srp_server_keys(SrpParams, N+1)
    end.

generate_srp_client_keys(_Generator, _Prime, 10) ->
    ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER);
generate_srp_client_keys(Generator, Prime, N) ->

    try crypto:generate_key(srp, {user, [Generator, Prime, '6a']}) of
	Keys ->
	    Keys
    catch
	error:_ ->
	    generate_srp_client_keys(Generator, Prime, N+1)
    end.

handle_srp_identity(Username, {Fun, UserState}) ->
    case Fun(srp, Username, UserState) of
	{ok, {SRPParams, Salt, DerivedKey}}
	  when is_atom(SRPParams), is_binary(Salt), is_binary(DerivedKey) ->
	    {Generator, Prime} = ssl_srp_primes:get_srp_params(SRPParams),
	    Verifier = crypto:mod_pow(Generator, DerivedKey, Prime),
	    #srp_user{generator = Generator, prime = Prime,
		      salt = Salt, verifier = Verifier};
	#alert{} = Alert ->
	    throw(Alert);
	_ ->
	    throw(?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER))
    end.


cipher_role(client, Data, Session, #state{connection_states = ConnectionStates0} = State0,
	    Connection) ->
    ConnectionStates = ssl_record:set_server_verify_data(current_both, Data, 
							 ConnectionStates0),
     {Record, State} = prepare_connection(State0#state{session = Session,
						       connection_states = ConnectionStates},
					 Connection),
    Connection:next_event(connection, Record, State, [{{timeout, handshake}, infinity, close}]);
cipher_role(server, Data, Session,  #state{connection_states = ConnectionStates0} = State0,
	    Connection) ->
    ConnectionStates1 = ssl_record:set_client_verify_data(current_read, Data, 
							  ConnectionStates0),
    {State1, Actions} =
	finalize_handshake(State0#state{connection_states = ConnectionStates1,
					session = Session}, cipher, Connection),
    {Record, State} = prepare_connection(State1, Connection),
    Connection:next_event(connection, Record, State, [{{timeout, handshake}, infinity, close} | Actions]).

is_anonymous(KexAlg) when KexAlg == dh_anon;
                          KexAlg == ecdh_anon;
                          KexAlg == psk;
                          KexAlg == dhe_psk;
                          KexAlg == ecdhe_psk;
                          KexAlg == rsa_psk;
                          KexAlg == srp_anon ->
    true;
is_anonymous(_) ->
    false.

get_current_prf(CStates, Direction) ->
    #{security_parameters := SecParams} = ssl_record:current_connection_state(CStates, Direction),
    SecParams#security_parameters.prf_algorithm.
get_pending_prf(CStates, Direction) ->
    #{security_parameters := SecParams} = ssl_record:pending_connection_state(CStates, Direction),
    SecParams#security_parameters.prf_algorithm.

opposite_role(client) ->
    server;
opposite_role(server) ->
    client.

record_cb(tls_connection) ->
    tls_record;
record_cb(dtls_connection) ->
    dtls_record.

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

get_socket_opts(_, _,_,[], _, Acc) ->
    {ok, Acc};
get_socket_opts(Connection, Transport, Socket, [mode | Tags], SockOpts, Acc) ->
    get_socket_opts(Connection, Transport, Socket, Tags, SockOpts, 
		    [{mode, SockOpts#socket_options.mode} | Acc]);
get_socket_opts(Connection, Transport, Socket, [packet | Tags], SockOpts, Acc) ->
    case SockOpts#socket_options.packet of
	{Type, headers} ->
	    get_socket_opts(Connection, Transport, Socket, Tags, SockOpts, [{packet, Type} | Acc]);
	Type ->
	    get_socket_opts(Connection, Transport, Socket, Tags, SockOpts, [{packet, Type} | Acc])
    end;
get_socket_opts(Connection, Transport, Socket, [header | Tags], SockOpts, Acc) ->
    get_socket_opts(Connection, Transport, Socket, Tags, SockOpts, 
		    [{header, SockOpts#socket_options.header} | Acc]);
get_socket_opts(Connection, Transport, Socket, [active | Tags], SockOpts, Acc) ->
    get_socket_opts(Connection, Transport, Socket, Tags, SockOpts, 
		    [{active, SockOpts#socket_options.active} | Acc]);
get_socket_opts(Connection, Transport, Socket, [Tag | Tags], SockOpts, Acc) ->
    case Connection:getopts(Transport, Socket, [Tag]) of
        {ok, [Opt]} ->
            get_socket_opts(Connection, Transport, Socket, Tags, SockOpts, [Opt | Acc]);
        {error, Reason} ->
            {error, {options, {socket_options, Tag, Reason}}}
    end;
get_socket_opts(_,_, _,Opts, _,_) ->
    {error, {options, {socket_options, Opts, function_clause}}}.

set_socket_opts(_,_,_, [], SockOpts, []) ->
    {ok, SockOpts};
set_socket_opts(ConnectionCb, Transport, Socket, [], SockOpts, Other) ->
    %% Set non emulated options 
    try ConnectionCb:setopts(Transport, Socket, Other) of
	ok ->
	    {ok, SockOpts};
	{error, InetError} ->
	    {{error, {options, {socket_options, Other, InetError}}}, SockOpts}
    catch
	_:Error ->
	    %% So that inet behavior does not crash our process
	    {{error, {options, {socket_options, Other, Error}}}, SockOpts}
    end;

set_socket_opts(ConnectionCb, Transport,Socket, [{mode, Mode}| Opts], SockOpts, Other) 
  when Mode == list; Mode == binary ->
    set_socket_opts(ConnectionCb, Transport, Socket, Opts, 
		    SockOpts#socket_options{mode = Mode}, Other);
set_socket_opts(_, _, _, [{mode, _} = Opt| _], SockOpts, _) ->
    {{error, {options, {socket_options, Opt}}}, SockOpts};
set_socket_opts(ConnectionCb, Transport,Socket, [{packet, Packet}| Opts], SockOpts, Other) 
  when Packet == raw;
       Packet == 0;
       Packet == 1;
       Packet == 2;
       Packet == 4;
       Packet == asn1;
       Packet == cdr;
       Packet == sunrm;
       Packet == fcgi;
       Packet == tpkt;
       Packet == line;
       Packet == http;
       Packet == httph;
       Packet == http_bin;
       Packet == httph_bin ->
    set_socket_opts(ConnectionCb, Transport, Socket, Opts, 
		    SockOpts#socket_options{packet = Packet}, Other);
set_socket_opts(_, _, _, [{packet, _} = Opt| _], SockOpts, _) ->
    {{error, {options, {socket_options, Opt}}}, SockOpts};
set_socket_opts(ConnectionCb, Transport, Socket, [{header, Header}| Opts], SockOpts, Other) 
  when is_integer(Header) ->
    set_socket_opts(ConnectionCb, Transport, Socket, Opts, 
		    SockOpts#socket_options{header = Header}, Other);
set_socket_opts(_, _, _, [{header, _} = Opt| _], SockOpts, _) ->
    {{error,{options, {socket_options, Opt}}}, SockOpts};
set_socket_opts(ConnectionCb, Transport, Socket, [{active, Active}| Opts], SockOpts, Other) 
  when Active == once;
       Active == true;
       Active == false ->
    set_socket_opts(ConnectionCb, Transport, Socket, Opts, 
		    SockOpts#socket_options{active = Active}, Other);
set_socket_opts(ConnectionCb, Transport, Socket, [{active, Active1} = Opt| Opts],
                SockOpts=#socket_options{active = Active0}, Other)
  when Active1 >= -32768, Active1 =< 32767 ->
    Active = if
        is_integer(Active0), Active0 + Active1 < -32768 ->
            error;
        is_integer(Active0), Active0 + Active1 =< 0 ->
            false;
        is_integer(Active0), Active0 + Active1 > 32767 ->
            error;
        Active1 =< 0 ->
            false;
        is_integer(Active0) ->
            Active0 + Active1;
        true ->
            Active1
    end,
    case Active of
        error ->
            {{error, {options, {socket_options, Opt}} }, SockOpts};
        _ ->
            set_socket_opts(ConnectionCb, Transport, Socket, Opts,
                            SockOpts#socket_options{active = Active}, Other)
    end;
set_socket_opts(_,_, _, [{active, _} = Opt| _], SockOpts, _) ->
    {{error, {options, {socket_options, Opt}} }, SockOpts};
set_socket_opts(ConnectionCb, Transport, Socket, [Opt | Opts], SockOpts, Other) ->
    set_socket_opts(ConnectionCb, Transport, Socket, Opts, SockOpts, [Opt | Other]).



hibernate_after(connection = StateName, 
		#state{ssl_options= #{hibernate_after := HibernateAfter}} = State,
		Actions) ->
    {next_state, StateName, State, [{timeout, HibernateAfter, hibernate} | Actions]};
hibernate_after(StateName, State, Actions) ->
    {next_state, StateName, State, Actions}.


terminate_alert(normal) ->
    ?ALERT_REC(?WARNING, ?CLOSE_NOTIFY);
terminate_alert({Reason, _}) when Reason == close;
                                  Reason == shutdown ->
    ?ALERT_REC(?WARNING, ?CLOSE_NOTIFY);
terminate_alert(_) -> 
    ?ALERT_REC(?FATAL, ?INTERNAL_ERROR).

handle_trusted_certs_db(#state{ssl_options = 
				   #{cacertfile := <<>>, cacerts := []}}) ->
    %% No trusted certs specified
    ok;
handle_trusted_certs_db(#state{static_env = #static_env{cert_db_ref = Ref,
                                                        cert_db = CertDb},
                               ssl_options = #{cacertfile := <<>>}}) when CertDb =/= undefined ->
    %% Certs provided as DER directly can not be shared
    %% with other connections and it is safe to delete them when the connection ends.
    ssl_pkix_db:remove_trusted_certs(Ref, CertDb);
handle_trusted_certs_db(#state{static_env = #static_env{file_ref_db = undefined}}) ->
    %% Something went wrong early (typically cacertfile does not
    %% exist) so there is nothing to handle
    ok;
handle_trusted_certs_db(#state{static_env = #static_env{cert_db_ref = Ref,
                                                        file_ref_db = RefDb},
			       ssl_options = #{cacertfile := File}}) ->
    case ssl_pkix_db:ref_count(Ref, RefDb, -1) of
	0 ->
	    ssl_manager:clean_cert_db(Ref, File);
	_ ->
	    ok
    end.

prepare_connection(#state{handshake_env = #handshake_env{renegotiation = Renegotiate}, 
			  start_or_recv_from = RecvFrom} = State0, Connection) 
  when Renegotiate =/= {false, first}, 
       RecvFrom =/= undefined ->
    State = Connection:reinit(State0),   
    {no_record, ack_connection(State)};
prepare_connection(State0, Connection) ->
    State = Connection:reinit(State0),
    {no_record, ack_connection(State)}.

ack_connection(#state{handshake_env = #handshake_env{renegotiation = {true, Initiater}} = HsEnv} = State) when Initiater == peer;
                                                                                                               Initiater == internal ->
    State#state{handshake_env = HsEnv#handshake_env{renegotiation = undefined}};
ack_connection(#state{handshake_env = #handshake_env{renegotiation = {true, From}} = HsEnv} = State) ->    
    gen_statem:reply(From, ok),
    State#state{handshake_env = HsEnv#handshake_env{renegotiation = undefined}};
ack_connection(#state{handshake_env = #handshake_env{renegotiation = {false, first}} = HsEnv, 
		      start_or_recv_from = StartFrom} = State) when StartFrom =/= undefined ->
    gen_statem:reply(StartFrom, connected),
    State#state{handshake_env = HsEnv#handshake_env{renegotiation = undefined}, 
		start_or_recv_from = undefined};
ack_connection(State) ->
    State.

session_handle_params(#server_ecdh_params{curve = ECCurve}, Session) ->
    Session#session{ecc = ECCurve};
session_handle_params(_, Session) ->
    Session.

handle_session(Role = server, #{reuse_sessions := true} = SslOpts,
               Host, Port, Session0) ->
    register_session(Role, host_id(Role, Host, SslOpts), Port, Session0, true);
handle_session(Role = client, #{verify := verify_peer,
                                reuse_sessions := Reuse} = SslOpts,
               Host, Port, Session0) when Reuse =/= false ->
    register_session(Role, host_id(Role, Host, SslOpts), Port, Session0, reg_type(Reuse));
handle_session(server, _, Host, Port, Session) ->
    %% Remove "session of type new" entry from session DB 
    ssl_manager:invalidate_session(Host, Port, Session),
    Session;
handle_session(client, _,_,_, Session) ->
    %% In client case there is no entry yet, so nothing to remove
    Session.

reg_type(save) ->
    true;
reg_type(true) ->
    unique.

register_session(client, Host, Port, #session{is_resumable = new} = Session0, Save) ->
    Session = Session0#session{is_resumable = true},
    ssl_manager:register_session(Host, Port, Session, Save),
    Session;
register_session(server, _, Port, #session{is_resumable = new} = Session0, _) ->
    Session = Session0#session{is_resumable = true},
    ssl_manager:register_session(Port, Session),
    Session;
register_session(_, _, _, Session, _) ->
    Session. %% Already registered

host_id(client, _Host, #{server_name_indication := Hostname}) when is_list(Hostname) ->
    Hostname;
host_id(_, Host, _) ->
    Host.

handle_new_session(NewId, CipherSuite, Compression, 
		   #state{static_env = #static_env{protocol_cb = Connection},
                          session = Session0
			 } = State0) ->
    Session = Session0#session{session_id = NewId,
			       cipher_suite = CipherSuite,
			       compression_method = Compression},
    Connection:next_event(certify, no_record, State0#state{session = Session}).

handle_resumed_session(SessId, #state{static_env = #static_env{host = Host,
                                                               port = Port,
                                                               protocol_cb = Connection,
                                                               session_cache = Cache,
                                                               session_cache_cb = CacheCb},
                                      connection_env = #connection_env{negotiated_version = Version},
                                      connection_states = ConnectionStates0} = State) ->
    Session = CacheCb:lookup(Cache, {{Host, Port}, SessId}),
    case ssl_handshake:master_secret(ssl:tls_version(Version), Session,
				     ConnectionStates0, client) of
	{_, ConnectionStates} ->
	    Connection:next_event(abbreviated, no_record, State#state{
                                                            connection_states = ConnectionStates,
                                                            session = Session});
	#alert{} = Alert ->
	    handle_own_alert(Alert, Version, hello, State)
    end.

make_premaster_secret({MajVer, MinVer}, rsa) ->
    Rand = ssl_cipher:random_bytes(?NUM_OF_PREMASTERSECRET_BYTES-2),
    <<?BYTE(MajVer), ?BYTE(MinVer), Rand/binary>>;
make_premaster_secret(_, _) ->
    undefined.

negotiated_hashsign(undefined, KexAlg, PubKeyInfo, Version) ->
    %% Not negotiated choose default 
    case is_anonymous(KexAlg) of
	true ->
	    {null, anon};
	false ->
	    {PubAlg, _, _} = PubKeyInfo,
	    ssl_handshake:select_hashsign_algs(undefined, PubAlg, Version)
    end;
negotiated_hashsign(HashSign = {_, _}, _, _, _) ->
    HashSign.

ssl_options_list(SslOptions) ->
    L = maps:to_list(SslOptions),
    ssl_options_list(L, []).

ssl_options_list([], Acc) ->
    lists:reverse(Acc);
%% Skip internal options, only return user options
ssl_options_list([{protocol, _}| T], Acc) ->
    ssl_options_list(T, Acc);
ssl_options_list([{erl_dist, _}|T], Acc) ->
    ssl_options_list(T, Acc);
ssl_options_list([{renegotiate_at, _}|T], Acc) ->
    ssl_options_list(T, Acc);
ssl_options_list([{ciphers = Key, Value}|T], Acc) ->
   ssl_options_list(T,
		    [{Key, lists:map(
			     fun(Suite) -> 
				     ssl_cipher_format:suite_bin_to_map(Suite) 
			     end, Value)} 
		     | Acc]);
ssl_options_list([{Key, Value}|T], Acc) ->
   ssl_options_list(T, [{Key, Value} | Acc]).

handle_active_option(false, connection = StateName, To, Reply, State) ->
    hibernate_after(StateName, State, [{reply, To, Reply}]);

handle_active_option(_, connection = StateName, To, _Reply, #state{connection_env = #connection_env{terminated = true},
                                                                   user_data_buffer = {_,0,_}} = State) ->
    handle_normal_shutdown(?ALERT_REC(?FATAL, ?CLOSE_NOTIFY, all_data_deliverd), StateName, 
                           State#state{start_or_recv_from = To}),
    {stop,{shutdown, peer_close}, State};
handle_active_option(_, connection = StateName0, To, Reply, #state{static_env = #static_env{protocol_cb = Connection},
                                                                   user_data_buffer = {_,0,_}} = State0) ->
    case Connection:next_event(StateName0, no_record, State0) of
	{next_state, StateName, State} ->
	    hibernate_after(StateName, State, [{reply, To, Reply}]);
	{next_state, StateName, State, Actions} -> 
	    hibernate_after(StateName, State, [{reply, To, Reply} | Actions]);
	{stop, _, _} = Stop ->
	    Stop
    end;
handle_active_option(_, StateName, To, Reply, #state{user_data_buffer = {_,0,_}} = State) ->
    %% Active once already set 
    {next_state, StateName, State, [{reply, To, Reply}]};

%% user_data_buffer nonempty
handle_active_option(_, StateName0, To, Reply,
                     #state{static_env = #static_env{protocol_cb = Connection}} = State0) ->
    case read_application_data(<<>>, State0) of
	{stop, _, _} = Stop ->
	    Stop;
	{Record, State1} ->
	    %% Note: Renogotiation may cause StateName0 =/= StateName
	    case Connection:next_event(StateName0, Record, State1) of
		{next_state, StateName, State} ->
		    hibernate_after(StateName, State, [{reply, To, Reply}]);
		{next_state, StateName, State, Actions} -> 
		    hibernate_after(StateName, State, [{reply, To, Reply} | Actions]);
		{stop, _, _} = Stop ->
		    Stop
	    end
    end.


%% Picks ClientData 
get_data(#socket_options{active=false}, undefined, _Bin) ->
    %% Recv timed out save buffer data until next recv
    passive;
get_data(#socket_options{active=Active, packet=Raw}, BytesToRead, Bin)
  when Raw =:= raw; Raw =:= 0 ->   %% Raw Mode
    case Bin of
        <<_/binary>> when Active =/= false orelse BytesToRead =:= 0 ->
	    %% Active true or once, or passive mode recv(0)  
	    {ok, Bin, <<>>};
        <<Data:BytesToRead/binary, Rest/binary>> ->
	    %% Passive Mode, recv(Bytes) 
            {ok, Data, Rest};
        <<_/binary>> ->
	    %% Passive Mode not enough data
            {more, BytesToRead}
    end;
get_data(#socket_options{packet=Type, packet_size=Size}, _, Bin) ->
    PacketOpts = [{packet_size, Size}], 
    decode_packet(Type, Bin, PacketOpts).

decode_packet({http, headers}, Buffer, PacketOpts) ->
    decode_packet(httph, Buffer, PacketOpts);
decode_packet({http_bin, headers}, Buffer, PacketOpts) ->
    decode_packet(httph_bin, Buffer, PacketOpts);
decode_packet(Type, Buffer, PacketOpts) ->
    erlang:decode_packet(Type, Buffer, PacketOpts).

%% Just like with gen_tcp sockets, an ssl socket that has been configured with
%% {packet, http} (or {packet, http_bin}) will automatically switch to expect
%% HTTP headers after it sees a HTTP Request or HTTP Response line. We
%% represent the current state as follows:
%%    #socket_options.packet =:= http: Expect a HTTP Request/Response line
%%    #socket_options.packet =:= {http, headers}: Expect HTTP Headers
%% Note that if the user has explicitly configured the socket to expect
%% HTTP headers using the {packet, httph} option, we don't do any automatic
%% switching of states.
deliver_app_data(
  CPids, Transport, Socket,
  #socket_options{active=Active, packet=Type} = SOpts,
  Data, Pid, From, Tracker, Connection) ->
    %%
    send_or_reply(
      Active, Pid, From,
      format_reply(
        CPids, Transport, Socket, SOpts, Data, Tracker, Connection)),
    SO =
        case Data of
            {P, _, _, _}
              when ((P =:= http_request) or (P =:= http_response)),
                   ((Type =:= http) or (Type =:= http_bin)) ->
                SOpts#socket_options{packet={Type, headers}};
            http_eoh when tuple_size(Type) =:= 2 ->
                %% End of headers - expect another Request/Response line
                {Type1, headers} = Type,
                SOpts#socket_options{packet=Type1};
            _ ->
                SOpts
        end,
    case Active of
        once ->
            SO#socket_options{active=false};
        1 ->
            send_user(
              Pid,
              format_passive(
                CPids, Transport, Socket, Tracker, Connection)),
            SO#socket_options{active=false};
        N when is_integer(N) ->
            SO#socket_options{active=N - 1};
	_ ->
	    SO
    end.

format_reply(_, _, _,#socket_options{active = false, mode = Mode, packet = Packet,
				  header = Header}, Data, _, _) ->
    {ok, do_format_reply(Mode, Packet, Header, Data)};
format_reply(CPids, Transport, Socket, #socket_options{active = _, mode = Mode, packet = Packet,
						header = Header}, Data, Tracker, Connection) ->
    {ssl, Connection:socket(CPids, Transport, Socket, Tracker),
     do_format_reply(Mode, Packet, Header, Data)}.

deliver_packet_error(CPids, Transport, Socket, 
                     SO= #socket_options{active = Active}, Data, Pid, From, Tracker, Connection) ->
    send_or_reply(Active, Pid, From, format_packet_error(CPids, 
                                                         Transport, Socket, SO, Data, Tracker, Connection)).

format_packet_error(_, _, _,#socket_options{active = false, mode = Mode}, Data, _, _) ->
    {error, {invalid_packet, do_format_reply(Mode, raw, 0, Data)}};
format_packet_error(CPids, Transport, Socket, #socket_options{active = _, mode = Mode}, 
                    Data, Tracker, Connection) ->
    {ssl_error, Connection:socket(CPids, Transport, Socket, Tracker),
     {invalid_packet, do_format_reply(Mode, raw, 0, Data)}}.

do_format_reply(binary, _, N, Data) when N > 0 ->  % Header mode
    header(N, Data);
do_format_reply(binary, _, _, Data) ->
    Data;
do_format_reply(list, Packet, _, Data)
  when Packet == http; Packet == {http, headers};
       Packet == http_bin; Packet == {http_bin, headers};
       Packet == httph; Packet == httph_bin ->
    Data;
do_format_reply(list, _,_, Data) ->
    binary_to_list(Data).

format_passive(CPids, Transport, Socket, Tracker, Connection) ->
    {ssl_passive, Connection:socket(CPids, Transport, Socket, Tracker)}.

header(0, <<>>) ->
    <<>>;
header(_, <<>>) ->
    [];
header(0, Binary) ->
    Binary;
header(N, Binary) ->
    <<?BYTE(ByteN), NewBinary/binary>> = Binary,
    [ByteN | header(N-1, NewBinary)].

send_or_reply(false, _Pid, From, Data) when From =/= undefined ->
    gen_statem:reply(From, Data);
%% Can happen when handling own alert or tcp error/close and there is
%% no outstanding gen_fsm sync events
send_or_reply(false, no_pid, _, _) ->
    ok;
send_or_reply(_, Pid, _From, Data) ->
    send_user(Pid, Data).

send_user(Pid, Msg) ->
    Pid ! Msg,
    ok.

alert_user(Pids, Transport, Tracker, Socket, connection, Opts, Pid, From, Alert, Role, StateName, Connection) ->
    alert_user(Pids, Transport, Tracker, Socket, Opts#socket_options.active, Pid, From, Alert, Role, StateName, Connection);
alert_user(Pids, Transport, Tracker, Socket,_, _, _, From, Alert, Role, StateName, Connection) ->
    alert_user(Pids, Transport, Tracker, Socket, From, Alert, Role, StateName, Connection).

alert_user(Pids, Transport, Tracker, Socket, From, Alert, Role, StateName, Connection) ->
    alert_user(Pids, Transport, Tracker, Socket, false, no_pid, From, Alert, Role, StateName, Connection).

alert_user(_, _, _, _, false = Active, Pid, From,  Alert, Role, StateName, Connection) when From =/= undefined ->
    %% If there is an outstanding ssl_accept | recv
    %% From will be defined and send_or_reply will
    %% send the appropriate error message.
    ReasonCode = ssl_alert:reason_code(Alert, Role, Connection:protocol_name(), StateName),
    send_or_reply(Active, Pid, From, {error, ReasonCode});
alert_user(Pids, Transport, Tracker, Socket, Active, Pid, From, Alert, Role, StateName, Connection) ->
    case ssl_alert:reason_code(Alert, Role, Connection:protocol_name(), StateName) of
	closed ->
	    send_or_reply(Active, Pid, From,
			  {ssl_closed, Connection:socket(Pids, Transport, Socket, Tracker)});
	ReasonCode ->
	    send_or_reply(Active, Pid, From,
			  {ssl_error, Connection:socket(Pids, Transport, Socket, Tracker), ReasonCode})
    end.

log_alert(Level, Role, ProtocolName, StateName, #alert{role = Role} = Alert) ->
    Txt = ssl_alert:own_alert_txt(Alert),
    Report = ssl_alert:alert_txt(ProtocolName, Role, StateName, Txt),
    ssl_logger:notice(Level, Report);
log_alert(Level, Role, ProtocolName, StateName, Alert) ->
    Txt = ssl_alert:alert_txt(Alert),
    Report = ssl_alert:alert_txt(ProtocolName, Role, StateName, Txt),
    ssl_logger:notice(Level, Report).

invalidate_session(client, Host, Port, Session) ->
    ssl_manager:invalidate_session(Host, Port, Session);
invalidate_session(server, _, Port, Session) ->
    ssl_manager:invalidate_session(Port, Session).

handle_sni_extension(undefined, State) ->
    State;
handle_sni_extension(#sni{hostname = Hostname}, #state{static_env = #static_env{role = Role} = InitStatEnv0,
                                                       handshake_env = HsEnv,
                                                       connection_env = CEnv} = State0) ->
    NewOptions = update_ssl_options_from_sni(State0#state.ssl_options, Hostname),
    case NewOptions of
	undefined ->
	    State0;
	_ ->
	    {ok, #{cert_db_ref := Ref, 
                   cert_db_handle := CertDbHandle, 
                   fileref_db_handle := FileRefHandle, 
                   session_cache := CacheHandle, 
                   crl_db_info := CRLDbHandle,
                   private_key := Key,
                   dh_params := DHParams,
                   own_certificate := OwnCert}} =
                 ssl_config:init(NewOptions, Role),
             State0#state{
               session = State0#state.session#session{own_certificate = OwnCert},
               static_env = InitStatEnv0#static_env{
                                        file_ref_db = FileRefHandle,
                                        cert_db_ref = Ref,
                                        cert_db = CertDbHandle,
                                        crl_db = CRLDbHandle,
                                        session_cache = CacheHandle
                             },
               connection_env = CEnv#connection_env{private_key = Key},
               ssl_options = NewOptions,
               handshake_env = HsEnv#handshake_env{sni_hostname = Hostname,
                                                   diffie_hellman_params = DHParams}
              }
    end.

update_ssl_options_from_sni(#{sni_fun := SNIFun,
                              sni_hosts := SNIHosts} = OrigSSLOptions, SNIHostname) ->
    SSLOption = 
	case SNIFun of
	    undefined ->
		proplists:get_value(SNIHostname, 
				    SNIHosts);
	    SNIFun ->
		SNIFun(SNIHostname)
	end,
    case SSLOption of
        undefined ->
            undefined;
        _ ->
            ssl:handle_options(SSLOption, OrigSSLOptions)
    end.

new_emulated([], EmOpts) ->
    EmOpts;
new_emulated(NewEmOpts, _) ->
    NewEmOpts.

no_records(Extensions) ->
    maps:map(fun(_, Value) ->
                     ssl_handshake:extension_value(Value)
             end, Extensions).  
