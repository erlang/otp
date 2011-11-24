%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%

%%----------------------------------------------------------------------
%% Purpose: Details of connection protocol
%%----------------------------------------------------------------------

-module(ssh_connection).

-include("ssh.hrl").
-include("ssh_connect.hrl").
-include("ssh_transport.hrl").

-export([session_channel/2, session_channel/4,
	 exec/4, shell/2, subsystem/4, send/3, send/4, send/5, 
	 send_eof/2, adjust_window/3, open_pty/3, open_pty/7,
	 open_pty/9, setenv/5, window_change/4, window_change/6,
	 direct_tcpip/6, direct_tcpip/8, tcpip_forward/3,
	 cancel_tcpip_forward/3, signal/3, exit_status/3, encode_ip/1, close/2,
	 reply_request/4]).

-export([channel_data/6, handle_msg/4, channel_eof_msg/1,
	 channel_close_msg/1, channel_success_msg/1, channel_failure_msg/1, 
	 channel_adjust_window_msg/2, channel_data_msg/3,
	 channel_open_msg/5, channel_open_confirmation_msg/4,
	 channel_open_failure_msg/4, channel_request_msg/4,
	 global_request_msg/3, request_failure_msg/0, 
	 request_success_msg/1, bind/4, unbind/3, unbind_channel/2, 
	 bound_channel/3, messages/0]).

%%--------------------------------------------------------------------
%%% Internal application API
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: session_channel(ConnectionManager 
%%                           [, InitialWindowSize, MaxPacketSize], 
%%                           Timeout) -> {ok, }
%%   ConnectionManager = pid() 
%%   InitialWindowSize = integer()
%%   MaxPacketSize = integer() 
%%
%% Description: Opens a channel for a ssh session. A session is a
%% remote execution of a program. The program may be a shell, an
%% application, a system command, or some built-in subsystem.
%% --------------------------------------------------------------------
session_channel(ConnectionManager, Timeout) ->
    session_channel(ConnectionManager, 
 		    ?DEFAULT_WINDOW_SIZE, ?DEFAULT_PACKET_SIZE,
 		    Timeout).
session_channel(ConnectionManager, InitialWindowSize, 
 		MaxPacketSize, Timeout) ->
    ssh_connection_manager:open_channel(ConnectionManager, "session", <<>>,
					InitialWindowSize,
 					MaxPacketSize, Timeout).
%%--------------------------------------------------------------------
%% Function: exec(ConnectionManager, ChannelId, Command, Timeout) -> 
%%
%%   ConnectionManager = pid() 
%%   ChannelId = integer()
%%   Cmd = string()
%%   Timeout = integer() 
%%
%% Description: Will request that the server start the
%% execution of the given command. 
%%--------------------------------------------------------------------
exec(ConnectionManager, ChannelId, Command, TimeOut) ->
    ssh_connection_manager:request(ConnectionManager, self(), ChannelId, "exec", 
 				   true, [?string(Command)], TimeOut).
%%--------------------------------------------------------------------
%% Function: shell(ConnectionManager, ChannelId) -> 
%%
%%   ConnectionManager = pid() 
%%   ChannelId = integer()
%%
%% Description: Will request that the user's default shell (typically
%% defined in /etc/passwd in UNIX systems) be started at the other
%% end.
%%--------------------------------------------------------------------
shell(ConnectionManager, ChannelId) ->
    ssh_connection_manager:request(ConnectionManager, self(), ChannelId, 
 				   "shell", false, <<>>, 0).
%%--------------------------------------------------------------------
%% Function: subsystem(ConnectionManager, ChannelId, SubSystem, TimeOut) -> 
%%
%% ConnectionManager = pid() 
%% ChannelId = integer()
%% SubSystem = string()
%% TimeOut = integer() 
%%
%%
%% Description: Executes a predefined subsystem.
%%--------------------------------------------------------------------
subsystem(ConnectionManager, ChannelId, SubSystem, TimeOut) ->
     ssh_connection_manager:request(ConnectionManager, self(),
				    ChannelId, "subsystem", 
				    true, [?string(SubSystem)], TimeOut).
%%--------------------------------------------------------------------
%% Function: send(ConnectionManager, ChannelId, Type, Data, [TimeOut]) ->
%%
%%
%% Description: Sends channel data.
%%--------------------------------------------------------------------
send(ConnectionManager, ChannelId, Data) ->
    send(ConnectionManager, ChannelId, 0, Data, infinity).
send(ConnectionManager, ChannelId, Data, TimeOut) when is_integer(TimeOut) ->
    send(ConnectionManager, ChannelId, 0, Data, TimeOut);
send(ConnectionManager, ChannelId, Data, infinity) ->
    send(ConnectionManager, ChannelId, 0, Data, infinity);
send(ConnectionManager, ChannelId, Type, Data) ->
    send(ConnectionManager, ChannelId, Type, Data, infinity).
send(ConnectionManager, ChannelId, Type, Data, TimeOut) ->
    ssh_connection_manager:send(ConnectionManager, ChannelId, 
				Type, Data, TimeOut).
%%--------------------------------------------------------------------
%% Function: send_eof(ConnectionManager, ChannelId) ->
%%
%%
%% Description: Sends eof on the channel <ChannelId>.
%%--------------------------------------------------------------------
send_eof(ConnectionManager, Channel) ->
    ssh_connection_manager:send_eof(ConnectionManager, Channel).

%%--------------------------------------------------------------------
%% Function: adjust_window(ConnectionManager, Channel, Bytes) -> 
%%
%%
%% Description: Adjusts the ssh flowcontrol window.
%%--------------------------------------------------------------------
adjust_window(ConnectionManager, Channel, Bytes) ->
    ssh_connection_manager:adjust_window(ConnectionManager, Channel, Bytes).

%%--------------------------------------------------------------------
%% Function: setenv(ConnectionManager, ChannelId, Var, Value, TimeOut) ->
%%
%%
%% Description: Environment variables may be passed to the shell/command to be
%% started later.
%%--------------------------------------------------------------------
setenv(ConnectionManager, ChannelId, Var, Value, TimeOut) ->
    ssh_connection_manager:request(ConnectionManager, ChannelId, 
	    "env", true, [?string(Var), ?string(Value)], TimeOut).


%%--------------------------------------------------------------------
%% Function: close(ConnectionManager, ChannelId) ->
%%
%%
%% Description: Sends a close message on the channel <ChannelId>.
%%--------------------------------------------------------------------
close(ConnectionManager, ChannelId) ->
    ssh_connection_manager:close(ConnectionManager, ChannelId).


%%--------------------------------------------------------------------
%% Function: reply_request(ConnectionManager, WantReply, Status, CannelId) ->_
%%
%%
%% Description: Send status replies to requests that want such replies.
%%--------------------------------------------------------------------
reply_request(ConnectionManager, true, Status, ChannelId) ->
    ConnectionManager ! {ssh_cm, self(), {Status, ChannelId}},
    ok;
reply_request(_,false, _, _) ->
    ok.


%%--------------------------------------------------------------------
%% Function: window_change(ConnectionManager, Channel, Width, Height) ->
%%
%%
%% Description: Not yet officialy supported.
%%--------------------------------------------------------------------
window_change(ConnectionManager, Channel, Width, Height) ->
    window_change(ConnectionManager, Channel, Width, Height, 0, 0).
window_change(ConnectionManager, Channel, Width, Height, 
	      PixWidth, PixHeight) ->
    ssh_connection_manager:request(ConnectionManager, Channel, 
				   "window-change", false, 
				   [?uint32(Width), ?uint32(Height),
				    ?uint32(PixWidth), ?uint32(PixHeight)], 0).
%%--------------------------------------------------------------------
%% Function: signal(ConnectionManager, Channel, Sig) ->
%%
%%
%% Description:  Not yet officialy supported.
%%--------------------------------------------------------------------
signal(ConnectionManager, Channel, Sig) ->
    ssh_connection_manager:request(ConnectionManager, Channel, 
				   "signal", false, [?string(Sig)], 0).

%%--------------------------------------------------------------------
%% Function: signal(ConnectionManager, Channel, Status) ->
%%
%%
%% Description:  Not yet officialy supported.
%%--------------------------------------------------------------------
exit_status(ConnectionManager, Channel, Status) ->
    ssh_connection_manager:request(ConnectionManager, Channel, 
				   "exit-status", false, [?uint32(Status)], 0).


%%--------------------------------------------------------------------
%% Function: open_pty(ConnectionManager, Channel, TimeOut) ->
%%
%%
%% Description:  Not yet officialy supported.
%%--------------------------------------------------------------------
open_pty(ConnectionManager, Channel, TimeOut) ->
    open_pty(ConnectionManager, Channel, 
	     os:getenv("TERM"), 80, 24, [], TimeOut).

open_pty(ConnectionManager, Channel, Term, Width, Height, PtyOpts, TimeOut) ->
    open_pty(ConnectionManager, Channel, Term, Width, 
	     Height, 0, 0, PtyOpts, TimeOut).

open_pty(ConnectionManager, Channel, Term, Width, Height, 
	 PixWidth, PixHeight, PtyOpts, TimeOut) ->
    ssh_connection_manager:request(ConnectionManager, 
				   Channel, "pty-req", true, 
				   [?string(Term),
				    ?uint32(Width), ?uint32(Height),
				    ?uint32(PixWidth),?uint32(PixHeight),
				    encode_pty_opts(PtyOpts)], TimeOut).


%%--------------------------------------------------------------------
%% Function: direct_tcpip(ConnectionManager, RemoteHost,  
%%                        RemotePort, OrigIP, OrigPort, Timeout) ->
%%
%%
%% Description: Not yet officialy supported.
%%--------------------------------------------------------------------
direct_tcpip(ConnectionManager, RemoteHost, 
	     RemotePort, OrigIP, OrigPort, Timeout) ->
    direct_tcpip(ConnectionManager, RemoteHost, RemotePort, OrigIP, OrigPort,
		 ?DEFAULT_WINDOW_SIZE, ?DEFAULT_PACKET_SIZE, Timeout).

direct_tcpip(ConnectionManager, RemoteIP, RemotePort, OrigIP, OrigPort,
	     InitialWindowSize, MaxPacketSize, Timeout) ->
    case {encode_ip(RemoteIP), encode_ip(OrigIP)} of
	{false, _} -> 
	    {error, einval};
	{_, false} -> 
	    {error, einval};
	{RIP, OIP} ->
	    ssh_connection_manager:open_channel(ConnectionManager,
						"direct-tcpip",
						[?string(RIP), 
						 ?uint32(RemotePort),
						 ?string(OIP),
						 ?uint32(OrigPort)],
						InitialWindowSize, 
						MaxPacketSize,
						Timeout)
    end.
%%--------------------------------------------------------------------
%% Function: tcpip_forward(ConnectionManager, BindIP, BindPort) ->
%%
%%
%% Description: Not yet officialy supported.
%%--------------------------------------------------------------------
tcpip_forward(ConnectionManager, BindIP, BindPort) ->
    case encode_ip(BindIP) of
	false -> 
	    {error, einval};
	IPStr ->
	    ssh_connection_manager:global_request(ConnectionManager, 
						  "tcpip-forward", true,
						  [?string(IPStr),
						   ?uint32(BindPort)])
    end.
%%--------------------------------------------------------------------
%% Function: cancel_tcpip_forward(ConnectionManager, BindIP, Port) ->
%%
%%
%% Description: Not yet officialy supported.
%%--------------------------------------------------------------------
cancel_tcpip_forward(ConnectionManager, BindIP, Port) ->
    case encode_ip(BindIP) of
	false -> 
	    {error, einval};
	IPStr ->
	    ssh_connection_manager:global_request(ConnectionManager, 
						  "cancel-tcpip-forward", true,
						  [?string(IPStr),
						   ?uint32(Port)])
    end.

%%--------------------------------------------------------------------
%%% Internal API
%%--------------------------------------------------------------------
channel_data(ChannelId, DataType, Data, Connection, ConnectionPid, From) 
  when is_list(Data)->
    channel_data(ChannelId, DataType, 
		 list_to_binary(Data), Connection, ConnectionPid, From);

channel_data(ChannelId, DataType, Data, 
	     #connection{channel_cache = Cache} = Connection, ConnectionPid,
	     From) ->
    
    case ssh_channel:cache_lookup(Cache, ChannelId) of
	#channel{remote_id = Id} = Channel0 ->
	    {SendList, Channel} = update_send_window(Channel0, DataType, 
						     Data, Connection),
	    Replies = 
		lists:map(fun({SendDataType, SendData}) -> 
				      {connection_reply, ConnectionPid, 
				       channel_data_msg(Id, 
							SendDataType, 
							SendData)}
			  end, SendList),
	    FlowCtrlMsgs = flow_control(Replies, 
					Channel#channel{flow_control = From}, 
					Cache),
	    {{replies, Replies ++ FlowCtrlMsgs}, Connection};
	undefined ->
	    {noreply, Connection}
    end.

handle_msg(#ssh_msg_channel_open_confirmation{recipient_channel = ChannelId, 
					      sender_channel = RemoteId,
					      initial_window_size = WindowSz,
					      maximum_packet_size = PacketSz}, 
	   #connection{channel_cache = Cache} = Connection0, _, _) ->
    
    #channel{remote_id = undefined} = Channel =
	ssh_channel:cache_lookup(Cache, ChannelId), 
    
    ssh_channel:cache_update(Cache, Channel#channel{
				     remote_id = RemoteId,
				     send_window_size = WindowSz,
				     send_packet_size = PacketSz}),
    {Reply, Connection} = reply_msg(Channel, Connection0, {open, ChannelId}),
    {{replies, [Reply]}, Connection};
 
handle_msg(#ssh_msg_channel_open_failure{recipient_channel = ChannelId,
					 reason = Reason,
					 description = Descr,
					 lang = Lang},  
	   #connection{channel_cache = Cache} = Connection0, _, _) ->
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    ssh_channel:cache_delete(Cache, ChannelId),
    {Reply, Connection} = 
	reply_msg(Channel, Connection0, {open_error, Reason, Descr, Lang}),
    {{replies, [Reply]}, Connection};

handle_msg(#ssh_msg_channel_success{recipient_channel = ChannelId},
	   #connection{channel_cache = Cache} = Connection0, _, _) ->
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    {Reply, Connection} = reply_msg(Channel, Connection0, success),
    {{replies, [Reply]}, Connection};

handle_msg(#ssh_msg_channel_failure{recipient_channel = ChannelId},
	   #connection{channel_cache = Cache} = Connection0, _, _) ->
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    {Reply, Connection} = reply_msg(Channel, Connection0, failure),
    {{replies, [Reply]}, Connection};

handle_msg(#ssh_msg_channel_eof{recipient_channel = ChannelId}, 
	    #connection{channel_cache = Cache} = Connection0, _, _) ->
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    {Reply, Connection} = reply_msg(Channel, Connection0, {eof, ChannelId}),
    {{replies, [Reply]}, Connection};
   
handle_msg(#ssh_msg_channel_close{recipient_channel = ChannelId},   
	   #connection{channel_cache = Cache} = Connection0, 
	   ConnectionPid, _) ->

	case ssh_channel:cache_lookup(Cache, ChannelId) of
	    #channel{sent_close = Closed, remote_id = RemoteId} = Channel ->
		ssh_channel:cache_delete(Cache, ChannelId),
		{CloseMsg, Connection} = 
		    reply_msg(Channel, Connection0, {closed, ChannelId}),
		case Closed of
		    true ->
			{{replies, [CloseMsg]}, Connection};
		    false ->
			RemoteCloseMsg = channel_close_msg(RemoteId),
			{{replies, 
			  [{connection_reply, 
				      ConnectionPid, RemoteCloseMsg},
			   CloseMsg]}, Connection}
		end;
	    undefined ->
		{{replies, []}, Connection0}
	end;

handle_msg(#ssh_msg_channel_data{recipient_channel = ChannelId,
				 data = Data}, 
	   #connection{channel_cache = Cache} = Connection0, _, _) ->
    
    #channel{recv_window_size = Size} = Channel =
	ssh_channel:cache_lookup(Cache, ChannelId), 
    WantedSize = Size - size(Data),
    ssh_channel:cache_update(Cache, Channel#channel{
				      recv_window_size = WantedSize}),
    {Replies, Connection} = 
	channel_data_reply(Cache, Channel, Connection0, 0, Data),
    {{replies, Replies}, Connection};

handle_msg(#ssh_msg_channel_extended_data{recipient_channel = ChannelId,
					  data_type_code = DataType,
					  data = Data}, 
	   #connection{channel_cache = Cache} = Connection0, _, _) ->
    
    #channel{recv_window_size = Size} = Channel =
	ssh_channel:cache_lookup(Cache, ChannelId), 
    WantedSize = Size - size(Data),
    ssh_channel:cache_update(Cache, Channel#channel{
				      recv_window_size = WantedSize}),
    {Replies, Connection} = 
	channel_data_reply(Cache, Channel, Connection0, DataType, Data),
    {{replies, Replies}, Connection};

handle_msg(#ssh_msg_channel_window_adjust{recipient_channel = ChannelId,
					  bytes_to_add = Add}, 
	   #connection{channel_cache = Cache} = Connection, 
	   ConnectionPid, _) ->
    
    #channel{send_window_size = Size} = 
	Channel0 = ssh_channel:cache_lookup(Cache, ChannelId), 

    {SendList, Channel} =  %% TODO: Datatype 0 ?
	update_send_window(Channel0#channel{send_window_size = Size + Add},
			   0, <<>>, Connection),
    
    Replies = lists:map(fun({Type, Data}) -> 
				{connection_reply, ConnectionPid,
				 channel_data_msg(ChannelId, Type, Data)}
			end, SendList),
    FlowCtrlMsgs = flow_control(Channel, Cache),
    {{replies, Replies ++ FlowCtrlMsgs}, Connection};

handle_msg(#ssh_msg_channel_open{channel_type = "session" = Type,
				 sender_channel = ChannelId,
				 initial_window_size = WindowSz,
				 maximum_packet_size = PacketSz}, Connection0, 
	   ConnectionPid, server) ->
   
    try setup_session(Connection0, ConnectionPid, ChannelId,
		     Type, WindowSz, PacketSz) of
	Result ->
	    Result
    catch _:_ ->
	    FailMsg = channel_open_failure_msg(ChannelId, 
					       ?SSH_OPEN_CONNECT_FAILED,
					       "Connection refused", "en"),
	    {{replies, [{connection_reply, ConnectionPid, FailMsg}]}, 
	     Connection0}
    end;

handle_msg(#ssh_msg_channel_open{channel_type = "session",
				 sender_channel = RemoteId}, 
	   Connection, ConnectionPid, client) ->
    %% Client implementations SHOULD reject any session channel open
    %% requests to make it more difficult for a corrupt server to attack the
    %% client. See See RFC 4254 6.1.
    FailMsg = channel_open_failure_msg(RemoteId, 
				       ?SSH_OPEN_CONNECT_FAILED,
				       "Connection refused", "en"),
    {{replies, [{connection_reply, ConnectionPid, FailMsg}]}, 
     Connection};

handle_msg(#ssh_msg_channel_open{channel_type = "forwarded-tcpip" = Type,
				 sender_channel = RemoteId,
				 initial_window_size = RWindowSz,
				 maximum_packet_size = RPacketSz,
				 data = Data}, 
	   #connection{channel_cache = Cache} = Connection0, 
	   ConnectionPid, server) ->
    <<?UINT32(ALen), Address:ALen/binary, ?UINT32(Port),
     ?UINT32(OLen), Orig:OLen/binary, ?UINT32(OrigPort)>> = Data,
    
    case bound_channel(Address, Port, Connection0) of
	undefined ->
	    FailMsg = channel_open_failure_msg(RemoteId, 
					       ?SSH_OPEN_CONNECT_FAILED,
					       "Connection refused", "en"),
	    {{replies, 
	      [{connection_reply, ConnectionPid, FailMsg}]}, Connection0};
	ChannelPid ->
	    {ChannelId, Connection1} = new_channel_id(Connection0),
	    LWindowSz = ?DEFAULT_WINDOW_SIZE,
	    LPacketSz = ?DEFAULT_PACKET_SIZE,
	    Channel = #channel{type = Type,
			       sys = "none",
			       user = ChannelPid,
			       local_id = ChannelId,
			       recv_window_size = LWindowSz,
			       recv_packet_size = LPacketSz,
			       send_window_size = RWindowSz,
			       send_packet_size = RPacketSz},
	    ssh_channel:cache_update(Cache, Channel),
	    OpenConfMsg = channel_open_confirmation_msg(RemoteId, ChannelId,
							LWindowSz, LPacketSz),
	    {OpenMsg, Connection} = 
		reply_msg(Channel, Connection1, 
			  {open,  Channel, {forwarded_tcpip,
					    decode_ip(Address), Port,
					    decode_ip(Orig), OrigPort}}),
	    {{replies, [{connection_reply, ConnectionPid, OpenConfMsg},
			OpenMsg]}, Connection}
    end;

handle_msg(#ssh_msg_channel_open{channel_type = "forwarded-tcpip",
				 sender_channel = RemoteId}, 
	   Connection, ConnectionPid, client) ->
    %% Client implementations SHOULD reject direct TCP/IP open requests for
    %% security reasons. See RFC 4254 7.2.
    FailMsg = channel_open_failure_msg(RemoteId, 
				       ?SSH_OPEN_CONNECT_FAILED,
				       "Connection refused", "en"),
    {{replies, [{connection_reply, ConnectionPid, FailMsg}]}, Connection};


handle_msg(#ssh_msg_channel_open{sender_channel = ChannelId}, Connection, 
	   ConnectionPid, _) ->
    FailMsg = channel_open_failure_msg(ChannelId, 
				       ?SSH_OPEN_ADMINISTRATIVELY_PROHIBITED,
				       "Not allowed", "en"),
    {{replies, [{connection_reply, ConnectionPid, FailMsg}]}, Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "exit-status",
				    data = Data},
	   #connection{channel_cache = Cache} = Connection, _, _) ->
    <<?UINT32(Status)>> = Data,
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    {Reply, Connection} = 
	reply_msg(Channel, Connection, {exit_status, ChannelId, Status}),
    {{replies, [Reply]}, Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "exit-signal",
				    want_reply = false,
				    data = Data},  
	   #connection{channel_cache = Cache} = Connection0, 
	   ConnectionPid, _) ->
    <<?UINT32(SigLen), SigName:SigLen/binary,
     ?BOOLEAN(_Core), 
     ?UINT32(ErrLen), Err:ErrLen/binary,
     ?UINT32(LangLen), Lang:LangLen/binary>> = Data,
    Channel = ssh_channel:cache_lookup(Cache, ChannelId),
    RemoteId =  Channel#channel.remote_id,
    {Reply, Connection} =  reply_msg(Channel, Connection0, 
				     {exit_signal, ChannelId,
				      binary_to_list(SigName),
				      binary_to_list(Err),
				      binary_to_list(Lang)}),
    CloseMsg = channel_close_msg(RemoteId),
    {{replies, [{connection_reply, ConnectionPid, CloseMsg}, Reply]},
     Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "xon-xoff",
				    want_reply = false,
				    data = Data},
	   #connection{channel_cache = Cache} = Connection, _, _) ->
    <<?BOOLEAN(CDo)>> = Data,
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    {Reply, Connection} = 
	reply_msg(Channel, Connection, {xon_xoff, ChannelId, CDo=/= 0}),
    {{replies, [Reply]}, Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "window-change",
				    want_reply = false,
				    data = Data}, 
	   #connection{channel_cache = Cache} = Connection0, _, _) ->
    <<?UINT32(Width),?UINT32(Height),
     ?UINT32(PixWidth), ?UINT32(PixHeight)>> = Data,
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    {Reply, Connection} = 
	reply_msg(Channel, Connection0, {window_change, ChannelId,
					 Width, Height,
					 PixWidth, PixHeight}),
    {{replies, [Reply]}, Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "signal",
				    data = Data}, 
	   #connection{channel_cache = Cache} = Connection0, _, _) ->
    <<?UINT32(SigLen), SigName:SigLen/binary>> = Data,
    
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    {Reply, Connection} = 
	reply_msg(Channel, Connection0, {signal, ChannelId,
					 binary_to_list(SigName)}),
    {{replies, [Reply]}, Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "subsystem",
				    want_reply = WantReply,
				    data = Data},
	   #connection{channel_cache = Cache} = Connection, 
	   ConnectionPid, server) ->
    <<?UINT32(SsLen), SsName:SsLen/binary>> = Data,
    
    #channel{remote_id = RemoteId} = Channel0 = 
	ssh_channel:cache_lookup(Cache, ChannelId), 
    
    ReplyMsg =  {subsystem, ChannelId, WantReply, binary_to_list(SsName)},
    
    try start_subsytem(SsName, Connection, Channel0, ReplyMsg) of
	{ok, Pid} -> 
	    erlang:monitor(process, Pid),
	    Channel = Channel0#channel{user = Pid},
	    ssh_channel:cache_update(Cache, Channel),
	    Reply = {connection_reply, ConnectionPid, 
		     channel_success_msg(RemoteId)}, 
	    {{replies, [Reply]}, Connection}
    catch _:_ ->
	    Reply = {connection_reply, ConnectionPid, 
		     channel_failure_msg(RemoteId)}, 
	    {{replies, [Reply]}, Connection}
    end;	

handle_msg(#ssh_msg_channel_request{request_type = "subsystem"},
	   Connection, _, client) ->
    %% The client SHOULD ignore subsystem requests. See RFC 4254 6.5.
    {{replies, []}, Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "pty-req",
				    want_reply = WantReply,
				    data = Data},
	   #connection{channel_cache = Cache} = Connection, 
	   ConnectionPid, server) ->
    <<?UINT32(TermLen), BTermName:TermLen/binary,
     ?UINT32(Width),?UINT32(Height),
     ?UINT32(PixWidth), ?UINT32(PixHeight),
     Modes/binary>> = Data,
    TermName = binary_to_list(BTermName),
   
    PtyRequest = {TermName, Width, Height,
		  PixWidth, PixHeight, decode_pty_opts(Modes)},
    
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    
    handle_cli_msg(Connection, ConnectionPid, Channel, 
		   {pty, ChannelId, WantReply, PtyRequest});

handle_msg(#ssh_msg_channel_request{request_type = "pty-req"},
	   Connection, _, client) ->
    %% The client SHOULD ignore pty requests. See RFC 4254 6.2.
    {{replies, []}, Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "shell",
				    want_reply = WantReply},
	   #connection{channel_cache = Cache} = Connection, 
	   ConnectionPid, server) ->
      
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    
    handle_cli_msg(Connection, ConnectionPid, Channel, 
		   {shell, ChannelId, WantReply});
 
handle_msg(#ssh_msg_channel_request{request_type = "shell"},
	   Connection, _, client) ->
    %% The client SHOULD ignore shell requests. See RFC 4254 6.5.
    {{replies, []}, Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "exec",
				    want_reply = WantReply,
				    data = Data},
	   #connection{channel_cache = Cache} = Connection, 
	   ConnectionPid, server) ->
    <<?UINT32(Len), Command:Len/binary>> = Data,
     
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    
    handle_cli_msg(Connection, ConnectionPid, Channel, 
		   {exec, ChannelId, WantReply, binary_to_list(Command)});
	
handle_msg(#ssh_msg_channel_request{request_type = "exec"},
	   Connection, _, client) ->
    %% The client SHOULD ignore exec requests. See RFC 4254 6.5.
    {{replies, []}, Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
  				    request_type = "env",
  				    want_reply = WantReply,
  				    data = Data}, 
  	   #connection{channel_cache = Cache} = Connection, 
  	   ConnectionPid, server) ->
    
    <<?UINT32(VarLen), 
     Var:VarLen/binary, ?UINT32(ValueLen), Value:ValueLen/binary>> = Data,
    
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    
    handle_cli_msg(Connection, ConnectionPid, Channel,
 		   {env, ChannelId, WantReply, Var, Value});

handle_msg(#ssh_msg_channel_request{request_type = "env"},
	   Connection, _, client) ->
    %% The client SHOULD ignore env requests. 
    {{replies, []}, Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = _Other,
				    want_reply = WantReply}, Connection,
	   ConnectionPid, _) ->
    ?dbg(true, "ssh_msg ssh_msg_channel_request: Other=~p\n",
	 [_Other]),
    if WantReply == true ->
	    FailMsg = channel_failure_msg(ChannelId),
	    {{replies, [{connection_reply, ConnectionPid, FailMsg}]}, 
	     Connection};
       true ->
	    {noreply, Connection}
    end;

handle_msg(#ssh_msg_global_request{name = _Type,
				   want_reply = WantReply,
				   data = _Data}, Connection,
	   ConnectionPid, _) ->
    if WantReply == true ->
	    FailMsg = request_failure_msg(),
	    {{replies, [{connection_reply, ConnectionPid, FailMsg}]}, 
	     Connection};
       true ->
	    {noreply, Connection}  
    end;

%%% This transport message will also be handled at the connection level
handle_msg(#ssh_msg_disconnect{code = Code,
			      description = Description,
			      language = _Lang }, 
	   #connection{channel_cache = Cache} = Connection0, _, _) ->
    {Connection, Replies} = 
	ssh_channel:cache_foldl(fun(Channel, {Connection1, Acc}) ->
					{Reply, Connection2} =  
					    reply_msg(Channel,
						      Connection1, {closed, Channel#channel.local_id}),
					{Connection2, [Reply | Acc]}
				end, {Connection0, []}, Cache),
    
    ssh_channel:cache_delete(Cache),
    {disconnect, {Code, Description}, {{replies, Replies}, Connection}}.

handle_cli_msg(#connection{channel_cache = Cache} = Connection0,
	       ConnectionPid, 
	       #channel{user = undefined, 
			local_id = ChannelId} = Channel0, Reply0) -> 
    
    case (catch start_cli(Connection0, ChannelId)) of
	{ok, Pid} ->
	    erlang:monitor(process, Pid),
	    Channel = Channel0#channel{user = Pid},
	    ssh_channel:cache_update(Cache, Channel),
	    {Reply, Connection} = reply_msg(Channel, Connection0, Reply0),
	    {{replies, [Reply]}, Connection};
	_ ->
	    Reply = {connection_reply, ConnectionPid, 
		     request_failure_msg()}, 
	    {{replies, [Reply]}, Connection0}
    end;

handle_cli_msg(Connection0, _, Channel, Reply0) -> 
    {Reply, Connection} =  reply_msg(Channel, Connection0, Reply0),
    {{replies, [Reply]}, Connection}.
    

channel_eof_msg(ChannelId) ->
    #ssh_msg_channel_eof{recipient_channel = ChannelId}.

channel_close_msg(ChannelId) ->
    #ssh_msg_channel_close {recipient_channel = ChannelId}.

channel_success_msg(ChannelId) ->
    #ssh_msg_channel_success{recipient_channel = ChannelId}.

channel_failure_msg(ChannelId) ->
    #ssh_msg_channel_failure{recipient_channel = ChannelId}.

channel_adjust_window_msg(ChannelId, Bytes) ->
    #ssh_msg_channel_window_adjust{recipient_channel = ChannelId,
				   bytes_to_add = Bytes}.

channel_data_msg(ChannelId, 0, Data) ->
    #ssh_msg_channel_data{recipient_channel = ChannelId,
			  data = Data};
channel_data_msg(ChannelId, Type, Data) ->
    #ssh_msg_channel_extended_data{recipient_channel = ChannelId,
				    data_type_code = Type,
				    data = Data}.

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

channel_request_msg(ChannelId, Type, WantReply, Data) ->
    #ssh_msg_channel_request{recipient_channel = ChannelId,
			     request_type = Type,
			     want_reply = WantReply,
			     data = Data}.

global_request_msg(Type, WantReply, Data) ->
    #ssh_msg_global_request{name = Type,
			    want_reply = WantReply,
			    data = Data}.
request_failure_msg() ->
    #ssh_msg_request_failure{}.

request_success_msg(Data) ->
    #ssh_msg_request_success{data = Data}.

bind(IP, Port, ChannelPid, Connection) ->
    Binds = [{{IP, Port}, ChannelPid}
	     | lists:keydelete({IP, Port}, 1, 
			       Connection#connection.port_bindings)],
    Connection#connection{port_bindings = Binds}.

unbind(IP, Port, Connection) ->
    Connection#connection{
      port_bindings = 
      lists:keydelete({IP, Port}, 1,
		      Connection#connection.port_bindings)}.
unbind_channel(ChannelPid, Connection) ->
    Binds = [{Bind, ChannelP} || {Bind, ChannelP} 
				     <- Connection#connection.port_bindings, 
				 ChannelP =/= ChannelPid],
    Connection#connection{port_bindings = Binds}.

bound_channel(IP, Port, Connection) ->
    case lists:keysearch({IP, Port}, 1, Connection#connection.port_bindings) of
	{value, {{IP, Port}, ChannelPid}} -> ChannelPid;
	_ -> undefined
    end.

messages() ->
    [ {ssh_msg_global_request, ?SSH_MSG_GLOBAL_REQUEST,
       [string, 
	boolean,
	'...']},
      
      {ssh_msg_request_success, ?SSH_MSG_REQUEST_SUCCESS,
       ['...']},
      
      {ssh_msg_request_failure, ?SSH_MSG_REQUEST_FAILURE,
       []},
      
      {ssh_msg_channel_open, ?SSH_MSG_CHANNEL_OPEN,
       [string,
	uint32,
	uint32,
	uint32,
	'...']},

      {ssh_msg_channel_open_confirmation, ?SSH_MSG_CHANNEL_OPEN_CONFIRMATION,
       [uint32,
	uint32,
	uint32,
	uint32,
	'...']},

      {ssh_msg_channel_open_failure, ?SSH_MSG_CHANNEL_OPEN_FAILURE,
       [uint32,
	uint32,
	string,
	string]},

      {ssh_msg_channel_window_adjust, ?SSH_MSG_CHANNEL_WINDOW_ADJUST,
       [uint32,
	uint32]},

      {ssh_msg_channel_data, ?SSH_MSG_CHANNEL_DATA,
       [uint32,
	binary]},

      {ssh_msg_channel_extended_data, ?SSH_MSG_CHANNEL_EXTENDED_DATA,
       [uint32,
	uint32,
	binary]},

      {ssh_msg_channel_eof, ?SSH_MSG_CHANNEL_EOF,
       [uint32]},

      {ssh_msg_channel_close, ?SSH_MSG_CHANNEL_CLOSE,
       [uint32]},

      {ssh_msg_channel_request, ?SSH_MSG_CHANNEL_REQUEST,
       [uint32,
	string,
	boolean,
	'...']},

      {ssh_msg_channel_success, ?SSH_MSG_CHANNEL_SUCCESS,
       [uint32]},

      {ssh_msg_channel_failure, ?SSH_MSG_CHANNEL_FAILURE,
       [uint32]}
     ].

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

start_channel(Cb, Id, Args, SubSysSup) ->
    start_channel(Cb, Id, Args, SubSysSup, undefined).

start_channel(Cb, Id, Args, SubSysSup, Exec) ->
    ChildSpec = child_spec(Cb, Id, Args, Exec),
    ChannelSup =ssh_subsystem_sup:channel_supervisor(SubSysSup),
    ssh_channel_sup:start_child(ChannelSup, ChildSpec).
    
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
setup_session(#connection{channel_cache = Cache} = Connection0, 
	      ConnectionPid, RemoteId,
	      Type, WindowSize, PacketSize) ->
    {ChannelId, Connection} = new_channel_id(Connection0),

    Channel = #channel{type = Type,
		       sys = "ssh",
		       local_id = ChannelId,
		       recv_window_size = ?DEFAULT_WINDOW_SIZE,
		       recv_packet_size = ?DEFAULT_PACKET_SIZE,
		       send_window_size = WindowSize,
		       send_packet_size = PacketSize,
		       remote_id = RemoteId
		      },
    ssh_channel:cache_update(Cache, Channel),
    OpenConfMsg = channel_open_confirmation_msg(RemoteId, ChannelId,
						?DEFAULT_WINDOW_SIZE, 
						?DEFAULT_PACKET_SIZE),

    {{replies, [{connection_reply, ConnectionPid, OpenConfMsg}]}, Connection}.


check_subsystem("sftp"= SsName, Options) ->
    case proplists:get_value(subsystems, Options, no_subsys) of
	no_subsys -> 	
	    {SsName, {Cb, Opts}} = ssh_sftpd:subsystem_spec([]),
	    {Cb, Opts};
	SubSystems ->
	    proplists:get_value(SsName, SubSystems, {none, []})
    end;

check_subsystem(SsName, Options) ->
    Subsystems = proplists:get_value(subsystems, Options, []),
    case proplists:get_value(SsName, Subsystems, {none, []}) of
	Fun when is_function(Fun) ->
	    {Fun, []};
	{_, _} = Value ->
	    Value
    end.

child_spec(Callback, Id, Args, Exec) ->
    Name = make_ref(),
    StartFunc = {ssh_channel, start_link, [self(), Id, Callback, Args, Exec]},
    Restart = temporary, 
    Shutdown = 3600,
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, [ssh_channel]}.

%% Backwards compatibility
start_cli(#connection{address = Address, port = Port, cli_spec = {Fun, [Shell]},
		      options = Options}, 
	  _ChannelId) when is_function(Fun) ->
    case Fun(Shell, Address, Port, Options) of
	NewFun when is_function(NewFun) ->
	    {ok, NewFun()};
	Pid when is_pid(Pid) ->
	    {ok, Pid}
    end;

start_cli(#connection{cli_spec = {CbModule, Args}, exec = Exec,
		      sub_system_supervisor = SubSysSup}, ChannelId) ->
    start_channel(CbModule, ChannelId, Args, SubSysSup, Exec).

start_subsytem(BinName, #connection{address = Address, port = Port, 
				    options = Options, 
				    sub_system_supervisor = SubSysSup},
	       #channel{local_id = ChannelId, remote_id = RemoteChannelId},
	       ReplyMsg) ->
    Name = binary_to_list(BinName),
    case check_subsystem(Name, Options) of
	{Callback, Opts} when is_atom(Callback), Callback =/= none ->
	    start_channel(Callback, ChannelId, Opts, SubSysSup);
	{Other, _} when Other =/= none ->
	    handle_backwards_compatibility(Other, self(), 
					   ChannelId, RemoteChannelId,
					   Options, Address, Port, 
					   {ssh_cm, self(), ReplyMsg})
    end.

channel_data_reply(_, #channel{local_id = ChannelId} = Channel, 
		   Connection0, DataType, Data) ->
    {Reply, Connection} =
	reply_msg(Channel, Connection0, {data, ChannelId, DataType, Data}),
    {[Reply], Connection}.

new_channel_id(Connection) ->
    ID = Connection#connection.channel_id_seed,
    {ID, Connection#connection{channel_id_seed = ID + 1}}.

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
reply_msg(#channel{user = ChannelPid}, Connection, Reply) ->
    {{channel_data, ChannelPid, Reply}, Connection}.

request_reply_or_data(#channel{local_id = ChannelId, user = ChannelPid}, 
		      #connection{requests = Requests} = 
		      Connection, Reply) -> 
    case lists:keysearch(ChannelId, 1, Requests) of
	{value, {ChannelId, From}} ->
	    {{channel_requst_reply, From, Reply}, 
	     Connection#connection{requests = 
				   lists:keydelete(ChannelId, 1, Requests)}};
	false ->
	    {{channel_data, ChannelPid, Reply}, Connection}
    end.

update_send_window(Channel0, DataType, Data,  
			#connection{channel_cache = Cache}) ->
    Buf0 = if Data == <<>> ->
		   Channel0#channel.send_buf;
	      true ->
		   Channel0#channel.send_buf ++ [{DataType, Data}]
	   end,
    {Buf1, NewSz, Buf2} = get_window(Buf0, 
				     Channel0#channel.send_packet_size,
				     Channel0#channel.send_window_size),

    Channel = Channel0#channel{send_window_size = NewSz, send_buf = Buf2},
    ssh_channel:cache_update(Cache, Channel), 
    {Buf1, Channel}.

get_window(Bs, PSz, WSz) ->
    get_window(Bs, PSz, WSz, []).

get_window(Bs, _PSz, 0, Acc) ->
    {lists:reverse(Acc), 0, Bs};
get_window([B0 = {DataType, Bin} | Bs], PSz, WSz, Acc) ->
    BSz = size(Bin),
    if BSz =< WSz ->  %% will fit into window
	    if BSz =< PSz ->  %% will fit into a packet
		    get_window(Bs, PSz, WSz-BSz, [B0|Acc]);
	       true -> %% split into packet size
		    <<Bin1:PSz/binary, Bin2/binary>> = Bin,
		    get_window([setelement(2, B0, Bin2) | Bs],
			       PSz, WSz-PSz, 
			       [{DataType, Bin1}|Acc])
	    end;
       WSz =< PSz ->  %% use rest of window
	    <<Bin1:WSz/binary, Bin2/binary>> = Bin,
	    get_window([setelement(2, B0, Bin2) | Bs],
		       PSz, WSz-WSz, 
		       [{DataType, Bin1}|Acc]);
       true -> %% use packet size
	    <<Bin1:PSz/binary, Bin2/binary>> = Bin,
	    get_window([setelement(2, B0, Bin2) | Bs],
		       PSz, WSz-PSz, 
		       [{DataType, Bin1}|Acc])
    end;
get_window([], _PSz, WSz, Acc) ->
    {lists:reverse(Acc), WSz, []}.

flow_control(Channel, Cache) ->
    flow_control([window_adjusted], Channel, Cache).
				    
flow_control([], Channel, Cache) ->
    ssh_channel:cache_update(Cache, Channel),
    [];
flow_control([_|_], #channel{flow_control = From} = Channel, Cache) ->
    case From of
	undefined ->
	    [];
	_ ->
	    [{flow_control, Cache,  Channel, From, ok}]
    end.

encode_pty_opts(Opts) ->
    Bin = list_to_binary(encode_pty_opts2(Opts)),
    Len = size(Bin),
    <<?UINT32(Len), Bin/binary>>.

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
decode_pty_opts(<<?UINT32(Len), Modes:Len/binary>>) ->
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

decode_ip(Addr) when is_binary(Addr) ->
    case inet_parse:address(binary_to_list(Addr)) of
	{error,_} -> Addr;
	{ok,A}    -> A
    end.

%% This is really awful and that is why it is beeing phased out. 
handle_backwards_compatibility({_,_,_,_,_,_} = ChildSpec, _, _, _, _,
			       Address, Port, _) ->
    SystemSup = ssh_system_sup:system_supervisor(Address, Port),
    ChannelSup = ssh_system_sup:channel_supervisor(SystemSup),
    ssh_channel_sup:start_child(ChannelSup, ChildSpec);

handle_backwards_compatibility(Module, ConnectionManager, ChannelId,
			       RemoteChannelId, Opts,
			       _, _, Msg) when is_atom(Module) ->
    {ok, SubSystemPid} = gen_server:start_link(Module, [Opts], []),
    SubSystemPid !
	{ssh_cm, ConnectionManager, 
	 {open, ChannelId, RemoteChannelId, {session}}},
    SubSystemPid ! Msg,
    {ok, SubSystemPid}; 

handle_backwards_compatibility(Fun, ConnectionManager, ChannelId, 
			       RemoteChannelId, 
			       _, _, _, Msg) when is_function(Fun) ->
    SubSystemPid = Fun(),
    SubSystemPid ! 
	{ssh_cm, ConnectionManager, 
	 {open, ChannelId, RemoteChannelId, {session}}},
    SubSystemPid ! Msg,
    {ok, SubSystemPid};

handle_backwards_compatibility(ChildSpec, 
			       ConnectionManager, 
			       ChannelId, RemoteChannelId, _, 
			       Address, Port, Msg) ->
    SystemSup = ssh_system_sup:system_supervisor(Address, Port),
    ChannelSup = ssh_system_sup:channel_supervisor(SystemSup),
    {ok, SubSystemPid} 
	= ssh_channel_sup:start_child(ChannelSup, ChildSpec),
    SubSystemPid ! 
	{ssh_cm, ConnectionManager, 
	 {open, ChannelId, RemoteChannelId, {session}}},
    SubSystemPid ! Msg,
    {ok, SubSystemPid}.
