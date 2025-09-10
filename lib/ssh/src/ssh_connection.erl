%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
%% Purpose: Details of connection protocol
%%----------------------------------------------------------------------

-module(ssh_connection).

-include_lib("kernel/include/logger.hrl").

-include("ssh.hrl").
-include("ssh_connect.hrl").
-include("ssh_transport.hrl").

%% API
-export([session_channel/2, session_channel/4,
	 exec/4, shell/2, subsystem/4, send/3, send/4, send/5, 
	 send_eof/2, adjust_window/3, setenv/5, close/2, reply_request/4,
	 ptty_alloc/3, ptty_alloc/4]).

%% Potential API currently unsupported and not tested
-export([window_change/4, window_change/6,
	 signal/3, exit_status/3]).

%% Internal SSH application API
-export([channel_data/5,
         handle_msg/4,
         handle_stop/1,

         open_channel/4,

	 channel_adjust_window_msg/2,
	 channel_close_msg/1,
	 channel_open_failure_msg/4,
	 channel_open_msg/5,
	 channel_status_msg/1,
         channel_data_msg/3,
         channel_eof_msg/1,
         channel_failure_msg/1, 
         channel_open_confirmation_msg/4,
         channel_request_msg/4,
         channel_success_msg/1,

         request_global_msg/3,
	 request_failure_msg/0, 
	 request_success_msg/1,

         send_environment_vars/3,

	 encode_ip/1
        ]).

%% For testing only
-export([encode_pty_opts/1, decode_pty_opts/1]).

-type connection_ref() :: ssh:connection_ref().
-type channel_id()     :: ssh:channel_id().

-type req_status() :: success | failure .
-type reason() :: closed | timeout .

-type result() :: req_status() | {error, reason()} .

-type ssh_data_type_code() :: non_neg_integer(). % Only 0 and 1 are used


%%% The SSH Connection Protocol

-export_type([event/0,
              channel_msg/0,
              want_reply/0,
              data_ch_msg/0,
              eof_ch_msg/0,
              signal_ch_msg/0,
              exit_signal_ch_msg/0,
              exit_status_ch_msg/0,
              closed_ch_msg/0,
              env_ch_msg/0,
              pty_ch_msg/0,
              shell_ch_msg/0,
              window_change_ch_msg/0,
              exec_ch_msg/0
             ]).

-type event() :: {ssh_cm, ssh:connection_ref(), channel_msg()}.
-type channel_msg() ::  data_ch_msg()
                      | eof_ch_msg()
                      | closed_ch_msg()
                      | pty_ch_msg()
                      | env_ch_msg()
                      | shell_ch_msg()
                      | exec_ch_msg()
                      | signal_ch_msg()
                      | window_change_ch_msg()
                      | exit_status_ch_msg()
                      | exit_signal_ch_msg()
                        .

-type want_reply() :: boolean().

-type data_ch_msg() :: {data,
                        ssh:channel_id(),
                        ssh_data_type_code(),
                        Data :: binary()
                       } .
-type eof_ch_msg() :: {eof,
                       ssh:channel_id()
                      } .
-type signal_ch_msg() :: {signal,
                          ssh:channel_id(),
                          SignalName :: string()
                         } .
-type exit_signal_ch_msg() :: {exit_signal, ssh:channel_id(),
                               ExitSignal :: string(),
                               ErrorMsg :: string(),
                               LanguageString :: string()} .
-type exit_status_ch_msg() :: {exit_status,
                               ssh:channel_id(),
                               ExitStatus :: non_neg_integer()
                              } .
-type closed_ch_msg() :: {closed,
                          ssh:channel_id()
                         } .
-type env_ch_msg() :: {env,
                       ssh:channel_id(),
                       want_reply(),
                       Var :: string(),
                       Value :: string()
                      } .
-type pty_ch_msg() :: {pty,
                       ssh:channel_id(),
                       want_reply(),
                       {Terminal :: string(),
                        CharWidth :: non_neg_integer(),
                        RowHeight :: non_neg_integer(),
                        PixelWidth :: non_neg_integer(),
                        PixelHeight :: non_neg_integer(),
                        TerminalModes :: [term_mode()]
                       }
                      } .

-type term_mode() :: {Opcode :: atom() | byte(),
                      Value :: non_neg_integer()} .

-type shell_ch_msg() :: {shell,
                         ssh:channel_id(),
                         want_reply()
                        } .
-type window_change_ch_msg() :: {window_change,
                                 ssh:channel_id(),
                                 CharWidth :: non_neg_integer(),
                                 RowHeight :: non_neg_integer(),
                                 PixelWidth :: non_neg_integer(),
                                 PixelHeight :: non_neg_integer()
                                } .
-type exec_ch_msg() :: {exec,
                        ssh:channel_id(),
                        want_reply(),
                        Command :: string()
                       } .

%%% This function is solely to convince all
%%% checks that the type event() exists...
-export([dummy/1]).
-spec dummy(event()) -> false.
dummy(_) -> false.

%%--------------------------------------------------------------------
%%% API
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Description: Opens a channel for a ssh session. A session is a
%% remote execution of a program. The program may be a shell, an
%% application, a system command, or some built-in subsystem.
%% --------------------------------------------------------------------

-spec session_channel(ConnectionRef, Timeout) -> Result when
      ConnectionRef :: ssh:connection_ref(),
      Timeout :: timeout(),
      Result :: {ok, ssh:channel_id()} | {error, reason()} .

session_channel(ConnectionHandler, Timeout) ->
    session_channel(ConnectionHandler, undefined, undefined, Timeout).


-spec session_channel(ConnectionRef, InitialWindowSize, MaxPacketSize, Timeout) -> Result when
      ConnectionRef :: ssh:connection_ref(),
      InitialWindowSize :: pos_integer() | undefined,
      MaxPacketSize :: pos_integer() | undefined,
      Timeout :: timeout(),
      Result :: {ok, ssh:channel_id()} | {error, reason()} .

session_channel(ConnectionHandler, InitialWindowSize, MaxPacketSize, Timeout) ->
    open_channel(ConnectionHandler, "session", <<>>,
                 InitialWindowSize,
                 MaxPacketSize,
                 Timeout).

%%--------------------------------------------------------------------
%% Description: Opens a channel for the given type.
%% --------------------------------------------------------------------
open_channel(ConnectionHandler, Type, ChanData, Timeout) ->
    open_channel(ConnectionHandler, Type, ChanData, undefined, undefined, Timeout).

open_channel(ConnectionHandler, Type, ChanData, InitialWindowSize, MaxPacketSize, Timeout) ->
    case ssh_connection_handler:open_channel(ConnectionHandler, Type, ChanData,
                                             InitialWindowSize, MaxPacketSize,
                                             Timeout) of
        {open, Channel} ->
	    {ok, Channel};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% Description: Will request that the server start the
%% execution of the given command. 
%%--------------------------------------------------------------------
-spec exec(ConnectionRef, ChannelId, Command, Timeout) -> result() when
      ConnectionRef :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id(),
      Command :: string(),
      Timeout :: timeout().

exec(ConnectionHandler, ChannelId, Command, TimeOut) ->
    ssh_connection_handler:request(ConnectionHandler, self(), ChannelId, "exec",
				   true, [?string(Command)], TimeOut).

%%--------------------------------------------------------------------
%% Description: Will request that the user's default shell (typically
%% defined in /etc/passwd in UNIX systems) be started at the other
%% end.
%%--------------------------------------------------------------------
-spec shell(ConnectionRef, ChannelId) -> Result when
      ConnectionRef :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id(),
      Result :: ok | success | failure | {error, timeout} .

shell(ConnectionHandler, ChannelId) ->
    ssh_connection_handler:request(ConnectionHandler, self(), ChannelId,
 				   "shell", false, <<>>, 0).
%%--------------------------------------------------------------------
%%
%% Description: Executes a predefined subsystem.
%%--------------------------------------------------------------------
-spec subsystem(ConnectionRef, ChannelId, Subsystem, Timeout) -> result() when
      ConnectionRef :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id(),
      Subsystem  :: string(),
      Timeout :: timeout().

subsystem(ConnectionHandler, ChannelId, SubSystem, TimeOut) ->
     ssh_connection_handler:request(ConnectionHandler, self(),
				    ChannelId, "subsystem", 
				    true, [?string(SubSystem)], TimeOut).
%%--------------------------------------------------------------------
%% Description: Sends channel data.
%%--------------------------------------------------------------------
-spec send(connection_ref(), channel_id(), iodata()) ->
		  ok | {error, timeout | closed}.

send(ConnectionHandler, ChannelId, Data) ->
    send(ConnectionHandler, ChannelId, 0, Data, infinity).


-spec send(connection_ref(), channel_id(), iodata(), timeout()) -> ok |  {error, reason()};
          (connection_ref(), channel_id(), ssh_data_type_code(), iodata()) -> ok |  {error, reason()}.

send(ConnectionHandler, ChannelId, Data, TimeOut) when is_integer(TimeOut) ->
    send(ConnectionHandler, ChannelId, 0, Data, TimeOut);

send(ConnectionHandler, ChannelId, Data, infinity) ->
    send(ConnectionHandler, ChannelId, 0, Data, infinity);

send(ConnectionHandler, ChannelId, Type, Data) ->
    send(ConnectionHandler, ChannelId, Type, Data, infinity).


-spec send(connection_ref(), channel_id(), ssh_data_type_code(), iodata(), timeout()) -> ok |  {error, reason()}.

send(ConnectionHandler, ChannelId, Type, Data, TimeOut) ->
    ssh_connection_handler:send(ConnectionHandler, ChannelId,
				Type, Data, TimeOut).
%%--------------------------------------------------------------------
-spec send_eof(ConnectionRef, ChannelId) -> ok  | {error, closed} when
      ConnectionRef :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id().
%%
%%
%% Description: Sends eof on the channel <ChannelId>.
%%--------------------------------------------------------------------
send_eof(ConnectionHandler, Channel) ->
    ssh_connection_handler:send_eof(ConnectionHandler, Channel).

%%--------------------------------------------------------------------
-spec adjust_window(ConnectionRef, ChannelId, NumOfBytes) -> ok when
      ConnectionRef :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id(),
      NumOfBytes  :: integer().
%%
%%
%% Description: Adjusts the ssh flowcontrol window.
%%--------------------------------------------------------------------
adjust_window(ConnectionHandler, Channel, Bytes) ->
    ssh_connection_handler:adjust_window(ConnectionHandler, Channel, Bytes).

%%--------------------------------------------------------------------
-spec setenv(ConnectionRef, ChannelId, Var, Value, Timeout) -> success when
      ConnectionRef :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id(),
      Var :: string(),
      Value :: string(),
      Timeout :: timeout().
%%
%%
%% Description: Environment variables may be passed to the shell/command to be
%% started later.
setenv(ConnectionHandler, ChannelId, Var, Value, TimeOut) ->
    setenv(ConnectionHandler, ChannelId, true, Var, Value, TimeOut).

setenv(ConnectionHandler, ChannelId, WantReply, Var, Value, TimeOut) ->
    case ssh_connection_handler:request(ConnectionHandler, ChannelId,
                                        "env", WantReply,
                                        [?string(Var), ?string(Value)], TimeOut) of
        ok when WantReply == false ->
            success;
        Reply ->
            Reply
    end.
%%--------------------------------------------------------------------
-spec close(ConnectionRef, ChannelId) -> ok when
      ConnectionRef :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id().
%%
%%
%% Description: Sends a close message on the channel <ChannelId>.
%%--------------------------------------------------------------------
close(ConnectionHandler, ChannelId) ->
    ssh_connection_handler:close(ConnectionHandler, ChannelId).

%%--------------------------------------------------------------------
-spec reply_request(ConnectionRef, WantReply, Status, ChannelId) -> ok when
      ConnectionRef :: ssh:connection_ref(),
      WantReply :: boolean(),
      Status :: req_status(),
      ChannelId :: ssh:channel_id().
%%
%%
%% Description: Send status replies to requests that want such replies.
%%--------------------------------------------------------------------
reply_request(ConnectionHandler, true, Status, ChannelId) ->
    ssh_connection_handler:reply_request(ConnectionHandler, Status, ChannelId);
reply_request(_,false, _, _) ->
    ok.

%%--------------------------------------------------------------------
%% Description: Sends a ssh connection protocol pty_req.
%%--------------------------------------------------------------------
-spec ptty_alloc(ConnectionRef, ChannelId, Options) -> result() when
      ConnectionRef :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id(),
      Options  :: proplists:proplist().

ptty_alloc(ConnectionHandler, Channel, Options) ->
    ptty_alloc(ConnectionHandler, Channel, Options, infinity).


-spec ptty_alloc(ConnectionRef, ChannelId, Options, Timeout) -> result() when
      ConnectionRef :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id(),
      Options  :: proplists:proplist(),
      Timeout :: timeout().

ptty_alloc(ConnectionHandler, Channel, Options0, TimeOut) ->
    TermData = backwards_compatible(Options0, []), % FIXME
    {Width, PixWidth} = pty_default_dimensions(width, TermData),
    {Height, PixHeight} = pty_default_dimensions(height, TermData),
    pty_req(ConnectionHandler, Channel,
	    proplists:get_value(term, TermData, os:getenv("TERM", ?DEFAULT_TERMINAL)),
	    proplists:get_value(width, TermData, Width),
	    proplists:get_value(height, TermData, Height),
	    proplists:get_value(pixel_widh, TermData, PixWidth),
	    proplists:get_value(pixel_height, TermData, PixHeight),
	    proplists:get_value(pty_opts, TermData, []), TimeOut
	   ).

%%--------------------------------------------------------------------
%% Not yet officially supported! The following functions are part of the
%% initial contributed ssh application. They are untested. Do we want them?
%% Should they be documented and tested?
%%--------------------------------------------------------------------
window_change(ConnectionHandler, Channel, Width, Height) ->
    window_change(ConnectionHandler, Channel, Width, Height, 0, 0).
window_change(ConnectionHandler, Channel, Width, Height,
	      PixWidth, PixHeight) ->
    ssh_connection_handler:request(ConnectionHandler, Channel,
				   "window-change", false, 
				   [?uint32(Width), ?uint32(Height),
				    ?uint32(PixWidth), ?uint32(PixHeight)], 0).

signal(ConnectionHandler, Channel, Sig) ->
    ssh_connection_handler:request(ConnectionHandler, Channel,
				   "signal", false, [?string(Sig)], 0).


-spec exit_status(ConnectionRef, ChannelId, Status) -> ok when
      ConnectionRef :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id(),
      Status  :: integer().
exit_status(ConnectionHandler, Channel, Status) ->
    ssh_connection_handler:request(ConnectionHandler, Channel,
				   "exit-status", false, [?uint32(Status)], 0).

%%--------------------------------------------------------------------
%%% Internal, that is, ssh application internal API
%%--------------------------------------------------------------------

%%%----------------------------------------------------------------
%%% Send data on a channel/connection as result of for example
%%% ssh_connection:send (executed in the ssh_connection_state machine)
%%%

channel_data(ChannelId, DataType, Data0, 
	     #connection{channel_cache = Cache} = Connection,
	     From) ->
    case ssh_client_channel:cache_lookup(Cache, ChannelId) of
	#channel{remote_id = Id, sent_close = false} = Channel0 ->
            Data = ?to_binary(Data0),
	    {SendList, Channel} =
		update_send_window(Channel0#channel{flow_control = From}, DataType,
				   Data, Connection),
	    Replies = 
		lists:map(fun({SendDataType, SendData}) -> 
				  {connection_reply,
				   channel_data_msg(Id,
						    SendDataType,
						    SendData)}
			  end, SendList),
	    FlowCtrlMsgs = flow_control(Replies, Channel, Cache),
	    {Replies ++ FlowCtrlMsgs, Connection};
	_ ->
	    {[{channel_request_reply,From,{error,closed}}], Connection}
    end.

%%%----------------------------------------------------------------
%%% Handle the channel messages on behalf of the ssh_connection_handler
%%% state machine.
%%%
%%% Replies {Reply, UpdatedConnection}
%%%

handle_msg(#ssh_msg_disconnect{code = Code, description = Description}, Connection, _, _SSH) ->
    {disconnect, {Code, Description}, handle_stop(Connection)};

handle_msg(Msg, Connection, server, Ssh = #ssh{authenticated = false}) ->
    %% See RFC4252 6.
    %% Message numbers of 80 and higher are reserved for protocols running
    %% after this authentication protocol, so receiving one of them before
    %% authentication is complete is an error, to which the server MUST
    %% respond by disconnecting, preferably with a proper disconnect message
    %% sent to ease troubleshooting.
    MsgFun = fun(M) ->
                     io_lib:format("Connection terminated. Unexpected message for unauthenticated user."
                                   " Message:  ~w", [M],
                                   [{chars_limit, ssh_lib:max_log_len(Ssh)}])
             end,
    ?LOG_DEBUG(MsgFun, [Msg]),
    {disconnect, {?SSH_DISCONNECT_PROTOCOL_ERROR, "Connection refused"}, handle_stop(Connection)};

handle_msg(#ssh_msg_channel_open_confirmation{recipient_channel = ChannelId, 
					      sender_channel = RemoteId,
					      initial_window_size = WindowSz,
					      maximum_packet_size = PacketSz}, 
	   #connection{channel_cache = Cache} = Connection0, _, _SSH) ->
    
    #channel{remote_id = undefined, user = U} = Channel =
	ssh_client_channel:cache_lookup(Cache, ChannelId), 
    
    if U /= undefined ->
            ssh_client_channel:cache_update(Cache, Channel#channel{
                                             remote_id = RemoteId,
                                             recv_packet_size = max(32768, % rfc4254/5.2
                                                                    min(PacketSz, Channel#channel.recv_packet_size)
                                                                   ),
                                             send_window_size = WindowSz,
                                             send_packet_size = PacketSz}),
            reply_msg(Channel, Connection0, {open, ChannelId});
        true ->
            %% There is no user process so nobody cares about the channel
            %% close it and remove from the cache, reply from the peer will be
            %% ignored
            CloseMsg = channel_close_msg(RemoteId),
            ssh_client_channel:cache_delete(Cache, ChannelId),
            {[{connection_reply, CloseMsg}], Connection0}
    end;
 
handle_msg(#ssh_msg_channel_open_failure{recipient_channel = ChannelId,
					 reason = Reason,
					 description = Descr,
					 lang = Lang},  
	   #connection{channel_cache = Cache} = Connection0, _, _SSH) ->
    Channel = ssh_client_channel:cache_lookup(Cache, ChannelId), 
    ssh_client_channel:cache_delete(Cache, ChannelId),
    reply_msg(Channel, Connection0, {open_error, Reason, Descr, Lang});

handle_msg(#ssh_msg_channel_success{recipient_channel = ChannelId}, Connection, _, _SSH) ->
    reply_msg(ChannelId, Connection, success);

handle_msg(#ssh_msg_channel_failure{recipient_channel = ChannelId}, Connection, _, _SSH) ->
    reply_msg(ChannelId, Connection, failure);

handle_msg(#ssh_msg_channel_eof{recipient_channel = ChannelId}, Connection, _, _SSH) ->
    reply_msg(ChannelId, Connection, {eof, ChannelId});
   
handle_msg(#ssh_msg_channel_close{recipient_channel = ChannelId},   
	   #connection{channel_cache = Cache} = Connection0, _, _SSH) ->

	case ssh_client_channel:cache_lookup(Cache, ChannelId) of
		#channel{sent_close = Closed, remote_id = RemoteId,
			 flow_control = FlowControl} = Channel ->
		ssh_client_channel:cache_delete(Cache, ChannelId),
		{CloseMsg, Connection} = 
		    reply_msg(Channel, Connection0, {closed, ChannelId}),
		ConnReplyMsgs =
		    case Closed of
			true -> [];
			false ->
			    RemoteCloseMsg = channel_close_msg(RemoteId),
			    [{connection_reply, RemoteCloseMsg}]
		    end,

		%% if there was a send() in progress, make it fail
		SendReplyMsgs =
		    case FlowControl of
			undefined -> [];
			From ->
			    [{flow_control, From, {error, closed}}]
		    end,

		Replies = ConnReplyMsgs ++ CloseMsg ++ SendReplyMsgs,
		{Replies, Connection};

	    undefined ->
                %% This may happen among other reasons
                %% - we sent 'channel-close' %% and the peer failed to respond in time
                %% - we tried to open a channel but the handler died prematurely
                %%    and the channel entry was removed from the cache
		{[], Connection0}
	end;

handle_msg(#ssh_msg_channel_data{recipient_channel = ChannelId,
				 data = Data}, 
	   Connection, _, _SSH) ->
    channel_data_reply_msg(ChannelId, Connection, 0, Data);

handle_msg(#ssh_msg_channel_extended_data{recipient_channel = ChannelId,
					  data_type_code = DataType,
					  data = Data}, 
	   Connection, _, _SSH) ->
    channel_data_reply_msg(ChannelId, Connection, DataType, Data);

handle_msg(#ssh_msg_channel_window_adjust{recipient_channel = ChannelId,
					  bytes_to_add = Add},
	   #connection{channel_cache = Cache} = Connection, _, _SSH) ->
    case ssh_client_channel:cache_lookup(Cache, ChannelId) of
        Channel0 = #channel{send_window_size = Size,
                            remote_id = RemoteId} ->
            {SendList, Channel} =  %% TODO: Datatype 0 ?
                update_send_window(Channel0#channel{send_window_size = Size + Add},
                                   0, undefined, Connection),
            Replies = lists:map(fun({Type, Data}) ->
                                        {connection_reply,
                                         channel_data_msg(RemoteId, Type, Data)}
                                end, SendList),
            FlowCtrlMsgs = flow_control(Channel, Cache),
            {Replies ++ FlowCtrlMsgs, Connection};
        undefined ->
            {[], Connection}
    end;
handle_msg(#ssh_msg_channel_open{channel_type = "session" = Type,
				 sender_channel = RemoteId,
				 initial_window_size = WindowSz,
				 maximum_packet_size = PacketSz}, 
	   #connection{options = SSHopts} = Connection0,
	   server, _SSH) ->
    MinAcceptedPackSz =
        ?GET_OPT(minimal_remote_max_packet_size, SSHopts),
    
    if 
	MinAcceptedPackSz =< PacketSz ->
	    try setup_session(Connection0, RemoteId,
			      Type, WindowSz, PacketSz) of
		Result ->
		    Result
	    catch _:_ ->
		    FailMsg = channel_open_failure_msg(RemoteId, 
						       ?SSH_OPEN_CONNECT_FAILED,
						       "Connection refused", "en"),
		    {[{connection_reply, FailMsg}], Connection0}
	    end;

	MinAcceptedPackSz > PacketSz ->
	    FailMsg = channel_open_failure_msg(RemoteId, 
					       ?SSH_OPEN_ADMINISTRATIVELY_PROHIBITED,
					       lists:concat(["Maximum packet size below ",MinAcceptedPackSz,
							      " not supported"]), "en"),
	    {[{connection_reply, FailMsg}], Connection0}
    end;

handle_msg(#ssh_msg_channel_open{channel_type = "forwarded-tcpip",
				 sender_channel = RemoteId,
                                 initial_window_size = WindowSize,
                                 maximum_packet_size = PacketSize,
                                 data = <<?DEC_BIN(ConnectedHost,_L1), ?UINT32(ConnectedPort),
                                          ?DEC_BIN(_OriginHost,_L2), ?UINT32(_OriginPort)
                                        >>
                                },
           #connection{channel_cache = Cache,
                       channel_id_seed = ChId,
                       suggest_window_size = WinSz,
                       suggest_packet_size = PktSz,
                       options = Options,
                       connection_supervisor = ConnectionSup
                      } = C,
	   client, _SSH) ->
    {ReplyMsg, NextChId} =
        case ssh_connection_handler:retrieve(C, {tcpip_forward,ConnectedHost,ConnectedPort}) of
            {ok, {ConnectToHost,ConnectToPort}} ->
                case gen_tcp:connect(ConnectToHost, ConnectToPort, [{active,false}, binary]) of
                    {ok,Sock} ->
                        {ok,Pid} = ssh_connection_sup:start_channel(client, ConnectionSup, self(),
                                                                   ssh_tcpip_forward_client, ChId,
                                                                   [Sock], undefined, Options),
                        ssh_client_channel:cache_update(Cache,
                                                        #channel{type = "forwarded-tcpip",
                                                                 sys = "none",
                                                                 local_id = ChId,
                                                                 remote_id = RemoteId,
                                                                 user = Pid,
                                                                 recv_window_size = WinSz,
                                                                 recv_packet_size = PktSz,
                                                                 send_window_size = WindowSize,
                                                                 send_packet_size = PacketSize,
                                                                 send_buf = queue:new()
                                                                }),
                        gen_tcp:controlling_process(Sock, Pid),
                        inet:setopts(Sock, [{active,once}]),
                        {channel_open_confirmation_msg(RemoteId, ChId, WinSz, PktSz),
                         ChId + 1};

                    {error,Error} ->
                        {channel_open_failure_msg(RemoteId, 
                                                  ?SSH_OPEN_CONNECT_FAILED,
                                                  io_lib:format("Forwarded connection refused: ~p",[Error]),
                                                  "en"),
                         ChId}
                end;

            undefined ->
                {channel_open_failure_msg(RemoteId, 
                                          ?SSH_OPEN_CONNECT_FAILED,
                                          io_lib:format("No forwarding ordered",[]),
                                          "en"),
                 ChId}
        end,
    {[{connection_reply, ReplyMsg}], C#connection{channel_id_seed = NextChId}};

handle_msg(#ssh_msg_channel_open{channel_type = "direct-tcpip",
				 sender_channel = RemoteId,
                                 initial_window_size = WindowSize,
                                 maximum_packet_size = PacketSize,
                                 data = <<?DEC_BIN(HostToConnect,_L1),        ?UINT32(PortToConnect),
                                          ?DEC_BIN(_OriginatorIPaddress,_L2), ?UINT32(_OrignatorPort)
                                        >>
                                }, 
	   #connection{channel_cache = Cache,
                       channel_id_seed = ChId,
                       suggest_window_size = WinSz,
                       suggest_packet_size = PktSz,
                       options = Options,
                       connection_supervisor = ConnectionSup
                      } = C,
	   server, _SSH) ->
    {ReplyMsg, NextChId} =
        case ?GET_OPT(tcpip_tunnel_in, Options) of
            %% May add more to the option, like allowed ip/port pairs to connect to
            false ->
                {channel_open_failure_msg(RemoteId, 
                                          ?SSH_OPEN_CONNECT_FAILED,
                                          "Forwarding disabled", "en"),
                 ChId};

            true ->
                case gen_tcp:connect(binary_to_list(HostToConnect), PortToConnect,
                                     [{active,false}, binary]) of
                    {ok,Sock} ->
                        {ok,Pid} = ssh_connection_sup:start_channel(server, ConnectionSup, self(),
                                                                   ssh_tcpip_forward_srv, ChId,
                                                                   [Sock], undefined, Options),
                        ssh_client_channel:cache_update(Cache,
                                                        #channel{type = "direct-tcpip",
                                                                 sys = "none",
                                                                 local_id = ChId,
                                                                 remote_id = RemoteId,
                                                                 user = Pid,
                                                                 recv_window_size = WinSz,
                                                                 recv_packet_size = PktSz,
                                                                 send_window_size = WindowSize,
                                                                 send_packet_size = PacketSize,
                                                                 send_buf = queue:new()
                                                                }),
                        gen_tcp:controlling_process(Sock, Pid),
                        inet:setopts(Sock, [{active,once}]),

                        {channel_open_confirmation_msg(RemoteId, ChId, WinSz, PktSz),
                         ChId + 1};

                    {error,Error} ->
                        {channel_open_failure_msg(RemoteId, 
                                                  ?SSH_OPEN_CONNECT_FAILED,
                                                  io_lib:format("Forwarded connection refused: ~p",[Error]),
                                                  "en"),
                         ChId}
                end
        end,
    {[{connection_reply, ReplyMsg}], C#connection{channel_id_seed = NextChId}};

handle_msg(#ssh_msg_channel_open{channel_type = "session",
				 sender_channel = RemoteId}, 
	   Connection,
	   client, _SSH) ->
    %% Client implementations SHOULD reject any session channel open
    %% requests to make it more difficult for a corrupt server to attack the
    %% client. See See RFC 4254 6.1.
    FailMsg = channel_open_failure_msg(RemoteId, 
				       ?SSH_OPEN_CONNECT_FAILED,
				       "Connection refused", "en"),
    {[{connection_reply, FailMsg}], Connection};

handle_msg(#ssh_msg_channel_open{sender_channel = RemoteId}, Connection, _, _SSH) ->
    FailMsg = channel_open_failure_msg(RemoteId, 
				       ?SSH_OPEN_ADMINISTRATIVELY_PROHIBITED,
				       "Not allowed", "en"),
    {[{connection_reply, FailMsg}], Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "exit-status",
				    data = Data},
           Connection, _, _SSH) ->
    <<?UINT32(Status)>> = Data,
    reply_msg(ChannelId, Connection, {exit_status, ChannelId, Status});

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "exit-signal",
				    want_reply = false,
				    data = Data},
           #connection{channel_cache = Cache} = Connection0, _, _SSH) ->
    <<?DEC_BIN(SigName, _SigLen),
      ?BOOLEAN(_Core),
      ?DEC_BIN(Err, _ErrLen),
      ?DEC_BIN(Lang, _LangLen)>> = Data,
    case ssh_client_channel:cache_lookup(Cache, ChannelId) of
        #channel{remote_id = RemoteId, sent_close = SentClose} = Channel ->
            {Reply, Connection} =  reply_msg(Channel, Connection0,
                                             {exit_signal, ChannelId,
                                              binary_to_list(SigName),
                                              binary_to_list(Err),
                                              binary_to_list(Lang)}),
            %% Send 'channel-close' only if it has not been sent yet
            %% by e.g. our side also closing the channel or going down
            %% and(!) update the cache
            %% so that the 'channel-close' is not sent twice
            if not SentClose ->
                    CloseMsg = channel_close_msg(RemoteId),
                    ssh_client_channel:cache_update(Cache,
                                            Channel#channel{sent_close = true}),
                    {[{connection_reply, CloseMsg}|Reply], Connection};
                true ->
                    {Reply, Connection}
            end;
        _ ->
            %% Channel already closed by peer
            {[], Connection0}
    end;

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "xon-xoff",
				    want_reply = false,
				    data = Data},
           Connection, _, _SSH) ->
    <<?BOOLEAN(CDo)>> = Data,
    reply_msg(ChannelId, Connection, {xon_xoff, ChannelId, CDo=/= 0});

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "window-change",
				    want_reply = false,
				    data = Data}, 
           Connection0, _, _SSH) ->
    <<?UINT32(Width),?UINT32(Height),
      ?UINT32(PixWidth), ?UINT32(PixHeight)>> = Data,
    reply_msg(ChannelId, Connection0, {window_change, ChannelId,
                                       Width, Height,
                                       PixWidth, PixHeight});

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "signal",
				    data = Data}, 
           Connection0, _, _SSH) ->
    <<?DEC_BIN(SigName, _SigLen)>> = Data,
    reply_msg(ChannelId, Connection0, {signal, ChannelId,
                                       binary_to_list(SigName)});

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "subsystem",
				    want_reply = WantReply,
				    data = Data},
	   #connection{channel_cache = Cache} = Connection, server, _SSH) ->
    <<?DEC_BIN(SsName,_SsLen)>> = Data,
    #channel{remote_id=RemoteId} = Channel = 
	ssh_client_channel:cache_lookup(Cache, ChannelId), 
    Reply =
        case start_subsystem(SsName, Connection, Channel,
                             {subsystem, ChannelId, WantReply, binary_to_list(SsName)}) of
            {ok, Pid} ->
                erlang:monitor(process, Pid),
                ssh_client_channel:cache_update(Cache, Channel#channel{user=Pid}),
                channel_success_msg(RemoteId);
            {error,_Error} ->
                channel_failure_msg(RemoteId)
        end,
    {[{connection_reply,Reply}], Connection};

handle_msg(#ssh_msg_channel_request{request_type = "subsystem"},
	   Connection, client, _SSH) ->
    %% The client SHOULD ignore subsystem requests. See RFC 4254 6.5.
    {[], Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "pty-req",
				    want_reply = WantReply,
				    data = Data},
	   Connection, server, _SSH) ->
    <<?DEC_BIN(BTermName,_TermLen),
      ?UINT32(Width),?UINT32(Height),
      ?UINT32(PixWidth), ?UINT32(PixHeight),
      Modes/binary>> = Data,
    TermName = binary_to_list(BTermName),
    PtyOpts0 = decode_pty_opts(Modes),
    PtyOpts = case proplists:get_value(onlcr, PtyOpts0, undefined) of
                  undefined ->
                      %% If - peer client asked for pty
                      %%    - did not tell if LF->CRLF expansion is wanted
                      %% then
                      %%    - do LF->CRLF expansion
                      [{onlcr,1} | PtyOpts0];
                  _ ->
                      PtyOpts0
              end,
    PtyRequest = {TermName, Width, Height,
		  PixWidth, PixHeight, PtyOpts},
    handle_cli_msg(Connection, ChannelId,
		   {pty, ChannelId, WantReply, PtyRequest});

handle_msg(#ssh_msg_channel_request{request_type = "pty-req"},
	   Connection, client, _SSH) ->
    %% The client SHOULD ignore pty requests. See RFC 4254 6.2.
    {[], Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "shell",
				    want_reply = WantReply},
	   Connection, server, _SSH) ->
    handle_cli_msg(Connection, ChannelId,
		   {shell, ChannelId, WantReply});
 
handle_msg(#ssh_msg_channel_request{request_type = "shell"},
	   Connection, client, _SSH) ->
    %% The client SHOULD ignore shell requests. See RFC 4254 6.5.
    {[], Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "exec",
				    want_reply = WantReply,
				    data = Data},
	   Connection, server, _SSH) ->
    <<?DEC_BIN(Command, _Len)>> = Data,
    handle_cli_msg(Connection, ChannelId,
		   {exec, ChannelId, WantReply, binary_to_list(Command)});
	
handle_msg(#ssh_msg_channel_request{request_type = "exec"},
	   Connection, client, _SSH) ->
    %% The client SHOULD ignore exec requests. See RFC 4254 6.5.
    {[], Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
  				    request_type = "env",
  				    want_reply = WantReply,
  				    data = Data}, 
	   Connection, server, _SSH) ->
    <<?DEC_BIN(Var,_VarLen), ?DEC_BIN(Value,_ValLen)>> = Data,
    handle_cli_msg(Connection, ChannelId,
 		   {env, ChannelId, WantReply, Var, Value});

handle_msg(#ssh_msg_channel_request{request_type = "env"},
	   Connection, client, _SSH) ->
    %% The client SHOULD ignore env requests. 
    {[], Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
                                    want_reply = WantReply},
	   #connection{channel_cache = Cache} = Connection, _, _SSH) ->
    %% Not a valid request_type. All valid types are handling the
    %% parameter checking in their own clauses above.
    %% 
    %% The special ReqType faulty_msg signals that something went
    %% wrong found during decoding.
    %%
    %% RFC4254 5.4 says:
    %% "If 'want reply' is FALSE, no response will be sent to the request.
    %%  Otherwise, the recipient responds with either
    %%  SSH_MSG_CHANNEL_SUCCESS, SSH_MSG_CHANNEL_FAILURE, or request-specific
    %%  continuation messages.  If the request is not recognized or is not
    %%  supported for the channel, SSH_MSG_CHANNEL_FAILURE is returned."
    %%
    case ssh_client_channel:cache_lookup(Cache, ChannelId) of
        #channel{remote_id = RemoteId} when WantReply==true -> 
            FailMsg = channel_failure_msg(RemoteId),
            {[{connection_reply, FailMsg}], Connection};
        _ -> %% Channel has been closed or no reply is wanted
            {[], Connection}
    end;

handle_msg(#ssh_msg_global_request{name = <<"tcpip-forward">>,
				   want_reply = WantReply,
				   data = <<?DEC_BIN(ListenAddrStr,_Len),?UINT32(ListenPort)>>},
           #connection{options = Opts} = Connection, server, _SSH) ->
    case ?GET_OPT(tcpip_tunnel_out, Opts) of
        false ->
            %% This daemon instance has not enabled tcpip_forwarding
            {[{connection_reply, request_failure_msg()}], Connection};

        true ->
            ConnectionSup = ?GET_INTERNAL_OPT(connection_sup, Opts),
            FwdSup = ssh_connection_sup:tcpip_fwd_supervisor(ConnectionSup),
            ConnPid = self(),
            case ssh_tcpip_forward_acceptor:supervised_start(FwdSup,
                                                             {ListenAddrStr, ListenPort},
                                                             undefined,
                                                             "forwarded-tcpip", ssh_tcpip_forward_srv,
                                                             ConnPid) of
                {ok,ListenPort} when WantReply==true ->
                    {[{connection_reply, request_success_msg(<<>>)}], Connection};

                {ok,LPort} when WantReply==true ->
                    {[{connection_reply, request_success_msg(<<?UINT32(LPort)>>)}], Connection};

                {error,_} when WantReply==true ->
                    {[{connection_reply, request_failure_msg()}], Connection};

                _ when WantReply==true ->
                    {[{connection_reply, request_failure_msg()}], Connection};

                _ ->
                    {[], Connection}
            end
    end;

handle_msg(#ssh_msg_global_request{name = _Type,
				   want_reply = WantReply,
				   data = _Data}, Connection, _Role, _SSH) ->
    if WantReply == true ->
	    FailMsg = request_failure_msg(),
	    {[{connection_reply, FailMsg}], Connection};
       true ->
	    {[], Connection}  
    end;

handle_msg(#ssh_msg_request_failure{},
	   #connection{requests = [{_, From} | Rest]} = Connection, _, _SSH) ->
    {[{channel_request_reply, From, {failure, <<>>}}],
     Connection#connection{requests = Rest}};

handle_msg(#ssh_msg_request_failure{},
	   #connection{requests = [{_, From,_} | Rest]} = Connection, _, _SSH) ->
    {[{channel_request_reply, From, {failure, <<>>}}],
     Connection#connection{requests = Rest}};

handle_msg(#ssh_msg_request_success{data = Data},
	   #connection{requests = [{_, From} | Rest]} = Connection, _, _SSH) ->
    {[{channel_request_reply, From, {success, Data}}],
     Connection#connection{requests = Rest}};

handle_msg(#ssh_msg_request_success{data = Data},
	   #connection{requests = [{_, From, Fun} | Rest]} = Connection0, _, _SSH) ->
    Connection = Fun({success,Data}, Connection0),
    {[{channel_request_reply, From, {success, Data}}],
     Connection#connection{requests = Rest}}.


%%%----------------------------------------------------------------
%%% Returns pending responses to be delivered to the peer when a
%%% Channel/Connection closes
%%%
handle_stop(#connection{channel_cache = Cache} = Connection0) ->
    {Connection, Replies} = 
	ssh_client_channel:cache_foldl(
          fun(Channel, {Connection1, Acc}) ->
                  {Reply, Connection2} =
                      reply_msg(Channel, Connection1,
                                {closed, Channel#channel.local_id}),
                  {Connection2, Reply ++ Acc}
          end, {Connection0, []}, Cache),
    ssh_client_channel:cache_delete(Cache),
    {Replies, Connection}.

%%%----------------------------------------------------------------
%%% channel_*_msg(...)
%%% Returns a #ssh_msg_....{} for channel operations.
%%%
channel_adjust_window_msg(ChannelId, Bytes) ->
    #ssh_msg_channel_window_adjust{recipient_channel = ChannelId,
				   bytes_to_add = Bytes}.

channel_close_msg(ChannelId) ->
    #ssh_msg_channel_close {recipient_channel = ChannelId}.

channel_data_msg(ChannelId, 0, Data) ->
    #ssh_msg_channel_data{recipient_channel = ChannelId,
			  data = Data};
channel_data_msg(ChannelId, Type, Data) ->
    #ssh_msg_channel_extended_data{recipient_channel = ChannelId,
				    data_type_code = Type,
				    data = Data}.

channel_eof_msg(ChannelId) ->
    #ssh_msg_channel_eof{recipient_channel = ChannelId}.

channel_failure_msg(ChannelId) ->
    #ssh_msg_channel_failure{recipient_channel = ChannelId}.

channel_open_msg(Type, ChannelId, WindowSize, MaxPacketSize, Data) ->
    #ssh_msg_channel_open{channel_type = Type,
			  sender_channel = ChannelId,
			  initial_window_size = WindowSize,
			  maximum_packet_size = MaxPacketSize,
			  data = Data
			 }.

channel_open_confirmation_msg(RemoteId, LID, WindowSize, PacketSize) ->
    #ssh_msg_channel_open_confirmation{recipient_channel = RemoteId,
				       sender_channel = LID,
				       initial_window_size = WindowSize,
				       maximum_packet_size = PacketSize}.

channel_open_failure_msg(RemoteId, Reason, Description, Lang) ->
    #ssh_msg_channel_open_failure{recipient_channel = RemoteId,
				  reason = Reason,
				  description = Description,
				  lang = Lang}.

channel_status_msg({success, ChannelId}) ->
    channel_success_msg(ChannelId);

channel_status_msg({failure, ChannelId}) ->
    channel_failure_msg(ChannelId).

channel_request_msg(ChannelId, Type, WantReply, Data) ->
    #ssh_msg_channel_request{recipient_channel = ChannelId,
			     request_type = Type,
			     want_reply = WantReply,
			     data = Data}.

channel_success_msg(ChannelId) ->
    #ssh_msg_channel_success{recipient_channel = ChannelId}.

%%%----------------------------------------------------------------
%%% request_*_msg(...)
%%% Returns a #ssh_msg_....{}
%%%
request_global_msg(Name, WantReply, Data) ->
    #ssh_msg_global_request{name = Name,
                            want_reply = WantReply,
                            data = Data}.

request_failure_msg() ->
    #ssh_msg_request_failure{}.

request_success_msg(Data) ->
    #ssh_msg_request_success{data = Data}.

%%%----------------------------------------------------------------
%%%
%%%
encode_ip(Addr) when is_tuple(Addr) ->
    case catch inet_parse:ntoa(Addr) of
	{'EXIT',_} -> false;
	A -> A
    end;
encode_ip(Addr) when is_list(Addr) ->
    case inet_parse:address(Addr) of
	{ok, _} -> Addr;
	Error ->
	    case inet:getaddr(Addr, inet) of
		{ok, A} ->
		    inet_parse:ntoa(A);
		Error -> false
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Internal functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------
%%% Create the channel data when an ssh_msg_open_channel message
%%% of "session" typ is handled
%%%
setup_session(#connection{channel_cache = Cache,
                          channel_id_seed = NewChannelID,
                          suggest_window_size = WinSz,
                          suggest_packet_size = PktSz
			 } = C,
	      RemoteId, Type, WindowSize, PacketSize) when is_integer(WinSz),
                                                           is_integer(PktSz) ->
    NextChannelID = NewChannelID + 1,
    Channel =
        #channel{type = Type,
                 sys = "ssh",
                 local_id = NewChannelID,
                 recv_window_size = WinSz,
                 recv_packet_size = PktSz,
                 send_window_size = WindowSize,
                 send_packet_size = PacketSize,
                 send_buf = queue:new(),
                 remote_id = RemoteId
                },
    ssh_client_channel:cache_update(Cache, Channel),
    OpenConfMsg = channel_open_confirmation_msg(RemoteId, NewChannelID,
						WinSz, 
						PktSz),
                Reply = {connection_reply, OpenConfMsg},
    {[Reply], C#connection{channel_id_seed = NextChannelID}}.


%%%----------------------------------------------------------------
%%% Start a cli or subsystem
%%%
start_cli(#connection{options = Options, 
		      cli_spec = CliSpec,
		      exec = Exec,
		      connection_supervisor = ConnectionSup}, ChannelId) ->
    case CliSpec of
        no_cli ->
            {error, cli_disabled};
        {CbModule, Args} ->
            ssh_connection_sup:start_channel(server, ConnectionSup, self(), CbModule, ChannelId, Args, Exec, Options)
    end.


start_subsystem(BinName, #connection{options = Options,
                                     connection_supervisor = ConnectionSup},
	       #channel{local_id = ChannelId}, _ReplyMsg) ->
    Name = binary_to_list(BinName),
    case check_subsystem(Name, Options) of
	{Callback, Opts} when is_atom(Callback), Callback =/= none ->
            ssh_connection_sup:start_channel(server, ConnectionSup, self(), Callback, ChannelId, Opts, undefined, Options);
        {none, _} ->
            {error, bad_subsystem};
	{_, _} ->
	    {error, legacy_option_not_supported}
    end.


%%% Helpers for starting cli/subsystems
check_subsystem("sftp"= SsName, Options) ->
    case ?GET_OPT(subsystems, Options) of
	no_subsys -> 	% FIXME: Can 'no_subsys' ever be matched?
	    {SsName, {Cb, Opts}} = ssh_sftpd:subsystem_spec([]),
	    {Cb, Opts};
	SubSystems ->
	    proplists:get_value(SsName, SubSystems, {none, []})
    end;

check_subsystem(SsName, Options) ->
    Subsystems = ?GET_OPT(subsystems, Options),
    case proplists:get_value(SsName, Subsystems, {none, []}) of
	Fun when is_function(Fun) ->
	    {Fun, []};
	{_, _} = Value ->
	    Value
    end.

%%%----------------------------------------------------------------
%%%
%%% Send-window handling
%%%

update_send_window(Channel, _, undefined,
		   #connection{channel_cache = Cache}) ->
    do_update_send_window(Channel, Cache);

update_send_window(#channel{send_buf = SendBuffer} = Channel, DataType, Data,
		   #connection{channel_cache = Cache}) ->
    do_update_send_window(Channel#channel{send_buf = queue:in({DataType, Data}, SendBuffer)},
			  Cache).

do_update_send_window(Channel0, Cache) ->
    {SendMsgs, Channel} = get_window(Channel0, []),
    ssh_client_channel:cache_update(Cache, Channel), 
    {SendMsgs, Channel}.

get_window(#channel{send_window_size = 0
		   } = Channel, Acc) ->
    {lists:reverse(Acc), Channel};
get_window(#channel{send_packet_size = 0
		   } = Channel, Acc) ->
    {lists:reverse(Acc), Channel};
get_window(#channel{send_buf = Buffer, 
		    send_packet_size = PacketSize,
		    send_window_size = WindowSize0
		   } = Channel, Acc0) ->
    case queue:out(Buffer) of
	{{value, {_, Data} = Msg}, NewBuffer} ->
	    case handle_send_window(Msg, byte_size(Data), PacketSize, WindowSize0, Acc0) of
		{WindowSize, Acc, {_, <<>>}} ->
		    {lists:reverse(Acc), Channel#channel{send_window_size = WindowSize,
							 send_buf = NewBuffer}};
		{WindowSize, Acc, Rest} ->
		    get_window(Channel#channel{send_window_size = WindowSize,
					       send_buf = queue:in_r(Rest, NewBuffer)}, Acc)
	    end;
	{empty, NewBuffer} ->
	    {[], Channel#channel{send_buf = NewBuffer}}
    end.

handle_send_window(Msg = {Type, Data}, Size, PacketSize, WindowSize, Acc) when Size =< WindowSize ->
    case Size =< PacketSize of
	true ->
	    {WindowSize - Size, [Msg | Acc], {Type, <<>>}};
	false ->
	    <<Msg1:PacketSize/binary, Msg2/binary>> = Data,
	    {WindowSize - PacketSize, [{Type, Msg1} | Acc], {Type, Msg2}}
    end;
handle_send_window({Type, Data}, _, PacketSize, WindowSize, Acc) when WindowSize =< PacketSize ->
    <<Msg1:WindowSize/binary, Msg2/binary>> = Data,
    {WindowSize - WindowSize, [{Type, Msg1} | Acc], {Type, Msg2}};
handle_send_window({Type, Data}, _, PacketSize, WindowSize, Acc) ->
    <<Msg1:PacketSize/binary, Msg2/binary>> = Data,
    {WindowSize - PacketSize, [{Type, Msg1} | Acc], {Type, Msg2}}.

%%%----------------------------------------------------------------
%%%
%%% Flow control
%%% 

flow_control(Channel, Cache) ->
    flow_control([window_adjusted], Channel, Cache).

flow_control([], Channel, Cache) ->
    ssh_client_channel:cache_update(Cache, Channel),
    [];
flow_control([_|_], #channel{flow_control = From,
			     send_buf = Buffer} = Channel, Cache) when From =/= undefined ->
    case queue:is_empty(Buffer) of
	true ->
	    ssh_client_channel:cache_update(Cache, Channel#channel{flow_control = undefined}),
	    [{flow_control, Cache, Channel, From, ok}];
	false ->
	    []
    end;
flow_control(_,_,_) ->
    [].

%%%----------------------------------------------------------------
%%%
%%% Pseudo terminal stuff
%%% 

pty_req(ConnectionHandler, Channel, Term, Width, Height,
	 PixWidth, PixHeight, PtyOpts, TimeOut) ->
    ssh_connection_handler:request(ConnectionHandler,
				   Channel, "pty-req", true,
				   [?string(Term),
				    ?uint32(Width), ?uint32(Height),
				    ?uint32(PixWidth),?uint32(PixHeight),
				    encode_pty_opts(PtyOpts)], TimeOut).

pty_default_dimensions(Dimension, TermData) ->
    case proplists:get_value(Dimension, TermData, 0) of
	N when is_integer(N), N > 0 ->
	    {N, 0};
	_ ->
            PixelDim = list_to_atom("pixel_" ++ atom_to_list(Dimension)),
	    case proplists:get_value(PixelDim, TermData, 0) of
		N when is_integer(N), N > 0 ->
		    {0, N};
		_ ->
		    {?TERMINAL_WIDTH, 0}
	    end
    end.

encode_pty_opts(Opts) ->
    Bin = list_to_binary(encode_pty_opts2(Opts)),
    <<?STRING(Bin)>>.

encode_pty_opts2([]) -> 
    [?TTY_OP_END];
encode_pty_opts2([{vintr,Value} | Opts]) ->
    [?VINTR, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vquit,Value} | Opts]) ->
    [?VQUIT, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{verase,Value} | Opts]) ->
    [?VERASE, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vkill,Value} | Opts]) ->
    [?VKILL, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{veof,Value} | Opts]) ->
    [?VEOF, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{veol,Value} | Opts]) ->
    [?VEOL, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{veol2,Value} | Opts]) ->
    [?VEOL2, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vstart,Value} | Opts]) ->
    [?VSTART, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vstop,Value} | Opts]) ->
    [?VSTOP, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vsusp,Value} | Opts]) ->
    [?VSUSP, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vdsusp,Value} | Opts]) ->
    [?VDSUSP, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vreprint,Value} | Opts]) ->
    [?VREPRINT, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vwerase,Value} | Opts]) ->
    [ ?VWERASE, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vlnext,Value} | Opts]) ->
    [?VLNEXT, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vflush,Value} | Opts]) ->
    [?VFLUSH, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vswtch,Value} | Opts]) ->
    [?VSWTCH, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vstatus,Value} | Opts]) ->
    [?VSTATUS, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vdiscard,Value} | Opts]) ->
    [?VDISCARD, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{ignpar,Value} | Opts]) ->
    [?IGNPAR, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{parmrk,Value} | Opts]) ->
    [?PARMRK, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{inpck,Value} | Opts]) ->
    [?INPCK, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{istrip,Value} | Opts]) ->
    [?ISTRIP, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{inlcr,Value} | Opts]) ->
    [?INLCR, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{igncr,Value} | Opts]) ->
    [?IGNCR, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{icrnl,Value} | Opts]) ->
    [?ICRNL, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{iuclc,Value} | Opts]) ->
    [?IUCLC, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{ixon,Value} | Opts]) ->
    [?IXON, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{ixany,Value} | Opts]) ->
    [?IXANY, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{ixoff,Value} | Opts]) ->
    [?IXOFF, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{imaxbel,Value} | Opts]) ->
    [?IMAXBEL, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{iutf8,Value} | Opts]) ->
    [?IUTF8, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{isig,Value} | Opts]) ->
    [?ISIG, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{icanon,Value} | Opts]) ->
    [?ICANON, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{xcase,Value} | Opts]) ->
    [?XCASE, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{echo,Value} | Opts]) ->
    [?ECHO, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{echoe,Value} | Opts]) ->
    [?ECHOE, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{echok,Value} | Opts]) ->
    [?ECHOK, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{echonl,Value} | Opts]) ->
    [?ECHONL, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{noflsh,Value} | Opts]) ->
    [?NOFLSH, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{tostop,Value} | Opts]) ->
    [?TOSTOP, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{iexten,Value} | Opts]) ->
    [?IEXTEN, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{echoctl,Value} | Opts]) ->
    [?ECHOCTL, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{echoke,Value} | Opts]) ->
    [?ECHOKE, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{pendin,Value} | Opts]) ->
    [?PENDIN, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{opost,Value} | Opts]) ->
    [?OPOST, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{olcuc,Value} | Opts]) ->
    [?OLCUC, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{onlcr,Value} | Opts]) ->
    [?ONLCR, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{ocrnl,Value} | Opts]) ->
    [?OCRNL, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{onocr,Value} | Opts]) ->
    [?ONOCR, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{onlret,Value} | Opts]) ->
    [?ONLRET, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{cs7,Value} | Opts]) ->
    [?CS7, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{cs8,Value} | Opts]) ->
    [?CS8, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{parenb,Value} | Opts]) ->
    [?PARENB, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{parodd,Value} | Opts]) ->
    [?PARODD, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{tty_op_ispeed,Value} | Opts]) ->
    [?TTY_OP_ISPEED, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{tty_op_ospeed,Value} | Opts]) ->
    [?TTY_OP_OSPEED, ?uint32(Value) | encode_pty_opts2(Opts)].

decode_pty_opts(<<>>) ->		     
    [];
decode_pty_opts(<<0, 0, 0, 0>>) ->
    [];
decode_pty_opts(<<?DEC_BIN(Modes,_Len)>>) ->
    decode_pty_opts2(Modes);
decode_pty_opts(Binary) ->
    decode_pty_opts2(Binary).

decode_pty_opts2(<<?TTY_OP_END>>) ->		     
    [];
decode_pty_opts2(<<Code, ?UINT32(Value), Tail/binary>>) ->
    Op = case Code of
	     ?VINTR -> vintr;
	     ?VQUIT -> vquit;
	     ?VERASE -> verase;
	     ?VKILL -> vkill;
	     ?VEOF -> veof;
	     ?VEOL -> veol;
	     ?VEOL2 -> veol2;
	     ?VSTART -> vstart;
	     ?VSTOP -> vstop;
	     ?VSUSP -> vsusp;
	     ?VDSUSP -> vdsusp;
	     ?VREPRINT -> vreprint;
	     ?VWERASE -> vwerase;
	     ?VLNEXT -> vlnext;
	     ?VFLUSH -> vflush;
	     ?VSWTCH -> vswtch;
	     ?VSTATUS -> vstatus;
	     ?VDISCARD -> vdiscard;
	     ?IGNPAR -> ignpar;
	     ?PARMRK -> parmrk;
	     ?INPCK -> inpck;
	     ?ISTRIP -> istrip;
	     ?INLCR -> inlcr;
	     ?IGNCR -> igncr;
	     ?ICRNL -> icrnl;
	     ?IUCLC -> iuclc;
	     ?IXON -> ixon;
	     ?IXANY -> ixany;
	     ?IXOFF -> ixoff;
	     ?IMAXBEL -> imaxbel;
             ?IUTF8 -> iutf8; % RFC 8160
	     ?ISIG -> isig;
	     ?ICANON -> icanon;
	     ?XCASE -> xcase;
	     ?ECHO -> echo;
	     ?ECHOE -> echoe;
	     ?ECHOK -> echok;
	     ?ECHONL -> echonl;
	     ?NOFLSH -> noflsh;
	     ?TOSTOP -> tostop;
	     ?IEXTEN -> iexten;
	     ?ECHOCTL -> echoctl;
	     ?ECHOKE -> echoke;
	     ?PENDIN -> pendin;
	     ?OPOST -> opost;
	     ?OLCUC -> olcuc;
	     ?ONLCR -> onlcr;
	     ?OCRNL -> ocrnl;
	     ?ONOCR -> onocr;
	     ?ONLRET -> onlret;
	     ?CS7 -> cs7;
	     ?CS8 -> cs8;
	     ?PARENB -> parenb;
	     ?PARODD -> parodd;
	     ?TTY_OP_ISPEED -> tty_op_ispeed;
	     ?TTY_OP_OSPEED -> tty_op_ospeed;
	     _ -> Code
	 end,    
    [{Op, Value} | decode_pty_opts2(Tail)].


backwards_compatible([], Acc) ->
    Acc;
backwards_compatible([{hight, Value} | Rest], Acc) ->
    backwards_compatible(Rest, [{height, Value} | Acc]);
backwards_compatible([{pixel_hight, Value} | Rest], Acc) ->
    backwards_compatible(Rest, [{height, Value} | Acc]);
backwards_compatible([Value| Rest], Acc) ->
    backwards_compatible(Rest, [ Value | Acc]).


%%%----------------------------------------------------------------
%%%
%%% Common part of handling channel messages meant for a cli (like "env", "exec" etc)
%%% Called at the finnish of handle_msg(#ssh_msg_channel_request,...)
%%%

handle_cli_msg(C0, ChId, Reply0) ->
    Cache = C0#connection.channel_cache,
    Ch0 = ssh_client_channel:cache_lookup(Cache, ChId),
    case Ch0#channel.user of
        undefined ->
            case start_cli(C0, ChId) of
                {ok, Pid} ->
                    erlang:monitor(process, Pid),
                    Ch = Ch0#channel{user = Pid},
                    ssh_client_channel:cache_update(Cache, Ch),
                    reply_msg(Ch, C0, Reply0);
                {error, _Error} ->
                    Reply = {connection_reply, channel_failure_msg(Ch0#channel.remote_id)},
                    {[Reply], C0}
            end;
        
        _ ->
            reply_msg(Ch0, C0, Reply0)
    end.

%%%----------------------------------------------------------------
%%%
%%% TCP/IP forwarding

%%%----------------------------------------------------------------
%%%
%%% Request response handling on return to the calling ssh_connection_handler
%%% state machine.
%%% 

channel_data_reply_msg(ChannelId, Connection, DataType, Data) ->
    case ssh_client_channel:cache_lookup(Connection#connection.channel_cache, ChannelId) of
	#channel{recv_window_size = Size} = Channel ->
	    WantedSize = Size - byte_size(Data),
	    ssh_client_channel:cache_update(Connection#connection.channel_cache, 
                                     Channel#channel{recv_window_size = WantedSize}),
            reply_msg(Channel, Connection, {data, ChannelId, DataType, Data});
	undefined ->
	    {[], Connection}
    end.


reply_msg(ChId, C, Reply) when is_integer(ChId) ->
    reply_msg(ssh_client_channel:cache_lookup(C#connection.channel_cache, ChId), C, Reply);

reply_msg(Channel, Connection, {open, _} = Reply) ->
    request_reply_or_data(Channel, Connection, Reply);
reply_msg(Channel, Connection, {open_error, _, _, _} = Reply) ->
    request_reply_or_data(Channel, Connection, Reply);
reply_msg(Channel, Connection, success = Reply) ->
    request_reply_or_data(Channel, Connection, Reply);
reply_msg(Channel, Connection, failure = Reply) ->
    request_reply_or_data(Channel, Connection, Reply);
reply_msg(Channel, Connection, {closed, _} = Reply) ->
    request_reply_or_data(Channel, Connection, Reply);
reply_msg(undefined, Connection, _Reply) ->
    {[], Connection};
reply_msg(#channel{user = ChannelPid}, Connection, Reply) ->
    {[{channel_data, ChannelPid, Reply}], Connection}.


request_reply_or_data(#channel{local_id = ChannelId, user = ChannelPid}, 
		      #connection{requests = Requests} = 
		      Connection, Reply) -> 
    case lists:keysearch(ChannelId, 1, Requests) of
	{value, {ChannelId, From}} ->
	    {[{channel_request_reply, From, Reply}],
	     Connection#connection{requests = 
				       lists:keydelete(ChannelId, 1, Requests)}};
	false when (Reply == success) or (Reply == failure) ->
	    {[], Connection};
	false ->
	    {[{channel_data, ChannelPid, Reply}], Connection}
    end.

%%%----------------------------------------------------------------
send_environment_vars(ConnectionHandler, Channel, VarNames) ->
    lists:foldl(
      fun(Var, success) ->
              case os:getenv(Var) of
                  false ->
                      success;
                  Value ->
                      setenv(ConnectionHandler, Channel, false,
                             Var, Value, infinity)
              end
      end, success, VarNames).
