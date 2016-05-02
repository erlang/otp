%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2015. All Rights Reserved.
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

%% Internal application API
-export([channel_data/5, handle_msg/3, channel_eof_msg/1,
	 channel_close_msg/1, channel_success_msg/1, channel_failure_msg/1, 
	 channel_status_msg/1,
	 channel_adjust_window_msg/2, channel_data_msg/3,
	 channel_open_msg/5, channel_open_confirmation_msg/4,
	 channel_open_failure_msg/4, channel_request_msg/4,
	 request_failure_msg/0, 
	 request_success_msg/1, bind/4, unbind/3, unbind_channel/2, 
	 bound_channel/3, encode_ip/1]).

%%--------------------------------------------------------------------
%%% API
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec session_channel(pid(), timeout()) -> {ok, channel_id()} | {error, timeout | closed}.
-spec session_channel(pid(), integer(), integer(), timeout()) -> {ok, channel_id()} | {error, timeout | closed}.

%% Description: Opens a channel for a ssh session. A session is a
%% remote execution of a program. The program may be a shell, an
%% application, a system command, or some built-in subsystem.
%% --------------------------------------------------------------------

session_channel(ConnectionHandler, Timeout) ->
    session_channel(ConnectionHandler,
 		    ?DEFAULT_WINDOW_SIZE, ?DEFAULT_PACKET_SIZE,
 		    Timeout).

session_channel(ConnectionHandler, InitialWindowSize,
 		MaxPacketSize, Timeout) ->
    case ssh_connection_handler:open_channel(ConnectionHandler, "session", <<>>,
					InitialWindowSize,
					MaxPacketSize, Timeout) of
	{open, Channel} ->
	    {ok, Channel};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
-spec exec(pid(), channel_id(), string(), timeout()) -> 
		  success | failure | {error, timeout | closed}.

%% Description: Will request that the server start the
%% execution of the given command. 
%%--------------------------------------------------------------------
exec(ConnectionHandler, ChannelId, Command, TimeOut) ->
    ssh_connection_handler:request(ConnectionHandler, self(), ChannelId, "exec",
				   true, [?string(Command)], TimeOut).

%%--------------------------------------------------------------------
-spec shell(pid(), channel_id()) -> _.

%% Description: Will request that the user's default shell (typically
%% defined in /etc/passwd in UNIX systems) be started at the other
%% end.
%%--------------------------------------------------------------------
shell(ConnectionHandler, ChannelId) ->
    ssh_connection_handler:request(ConnectionHandler, self(), ChannelId,
 				   "shell", false, <<>>, 0).
%%--------------------------------------------------------------------
-spec subsystem(pid(), channel_id(), string(), timeout()) -> 
		       success | failure | {error, timeout | closed}.
%%
%% Description: Executes a predefined subsystem.
%%--------------------------------------------------------------------
subsystem(ConnectionHandler, ChannelId, SubSystem, TimeOut) ->
     ssh_connection_handler:request(ConnectionHandler, self(),
				    ChannelId, "subsystem", 
				    true, [?string(SubSystem)], TimeOut).
%%--------------------------------------------------------------------
-spec send(pid(), channel_id(), iodata()) ->
		  ok | {error, closed}.
-spec send(pid(), channel_id(), integer()| iodata(), timeout() | iodata()) ->
		  ok | {error, timeout} | {error, closed}.
-spec send(pid(), channel_id(), integer(), iodata(), timeout()) ->
		  ok | {error, timeout} | {error, closed}.
%%
%%
%% Description: Sends channel data.
%%--------------------------------------------------------------------
send(ConnectionHandler, ChannelId, Data) ->
    send(ConnectionHandler, ChannelId, 0, Data, infinity).
send(ConnectionHandler, ChannelId, Data, TimeOut) when is_integer(TimeOut) ->
    send(ConnectionHandler, ChannelId, 0, Data, TimeOut);
send(ConnectionHandler, ChannelId, Data, infinity) ->
    send(ConnectionHandler, ChannelId, 0, Data, infinity);
send(ConnectionHandler, ChannelId, Type, Data) ->
    send(ConnectionHandler, ChannelId, Type, Data, infinity).
send(ConnectionHandler, ChannelId, Type, Data, TimeOut) ->
    ssh_connection_handler:send(ConnectionHandler, ChannelId,
				Type, Data, TimeOut).
%%--------------------------------------------------------------------
-spec send_eof(pid(), channel_id()) -> ok | {error, closed}.
%%
%%
%% Description: Sends eof on the channel <ChannelId>.
%%--------------------------------------------------------------------
send_eof(ConnectionHandler, Channel) ->
    ssh_connection_handler:send_eof(ConnectionHandler, Channel).

%%--------------------------------------------------------------------
-spec adjust_window(pid(), channel_id(), integer()) -> ok |  {error, closed}.
%%
%%
%% Description: Adjusts the ssh flowcontrol window.
%%--------------------------------------------------------------------
adjust_window(ConnectionHandler, Channel, Bytes) ->
    ssh_connection_handler:adjust_window(ConnectionHandler, Channel, Bytes).

%%--------------------------------------------------------------------
-spec setenv(pid(), channel_id(), string(), string(), timeout()) ->  
		    success | failure | {error, timeout | closed}.
%%
%%
%% Description: Environment variables may be passed to the shell/command to be
%% started later.
%%--------------------------------------------------------------------
setenv(ConnectionHandler, ChannelId, Var, Value, TimeOut) ->
    ssh_connection_handler:request(ConnectionHandler, ChannelId,
	    "env", true, [?string(Var), ?string(Value)], TimeOut).


%%--------------------------------------------------------------------
-spec close(pid(), channel_id()) -> ok.
%%
%%
%% Description: Sends a close message on the channel <ChannelId>.
%%--------------------------------------------------------------------
close(ConnectionHandler, ChannelId) ->
    ssh_connection_handler:close(ConnectionHandler, ChannelId).

%%--------------------------------------------------------------------
-spec reply_request(pid(), boolean(), success | failure, channel_id()) -> ok.
%%
%%
%% Description: Send status replies to requests that want such replies.
%%--------------------------------------------------------------------
reply_request(ConnectionHandler, true, Status, ChannelId) ->
    ssh_connection_handler:reply_request(ConnectionHandler, Status, ChannelId);
reply_request(_,false, _, _) ->
    ok.

%%--------------------------------------------------------------------
-spec ptty_alloc(pid(), channel_id(), proplists:proplist()) -> 
			success | failiure | {error, closed}.
-spec ptty_alloc(pid(), channel_id(), proplists:proplist(), timeout()) -> 
			success | failiure | {error, timeout} | {error, closed}.

%%
%%
%% Description: Sends a ssh connection protocol pty_req.
%%--------------------------------------------------------------------
ptty_alloc(ConnectionHandler, Channel, Options) ->
    ptty_alloc(ConnectionHandler, Channel, Options, infinity).
ptty_alloc(ConnectionHandler, Channel, Options0, TimeOut) ->
    Options = backwards_compatible(Options0, []),
    {Width, PixWidth} = pty_default_dimensions(width, Options),
    {Height, PixHeight} = pty_default_dimensions(height, Options),
    pty_req(ConnectionHandler, Channel,
	    proplists:get_value(term, Options, os:getenv("TERM", ?DEFAULT_TERMINAL)),
	    proplists:get_value(width, Options, Width),
	    proplists:get_value(height, Options, Height),
	    proplists:get_value(pixel_widh, Options, PixWidth),
	    proplists:get_value(pixel_height, Options, PixHeight),
	    proplists:get_value(pty_opts, Options, []), TimeOut
	   ).
%%--------------------------------------------------------------------
%% Not yet officialy supported! The following functions are part of the
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


exit_status(ConnectionHandler, Channel, Status) ->
    ssh_connection_handler:request(ConnectionHandler, Channel,
				   "exit-status", false, [?uint32(Status)], 0).

%%--------------------------------------------------------------------
%%% Internal API
%%--------------------------------------------------------------------
l2b(L) when is_integer(hd(L)) ->
    try list_to_binary(L)
    of
	B -> B
    catch
	_:_ -> 
	    unicode:characters_to_binary(L)
    end;
l2b([H|T]) -> 
    << (l2b(H))/binary, (l2b(T))/binary >>;
l2b(B) when is_binary(B) ->
    B;
l2b([]) ->
    <<>>.

    

channel_data(ChannelId, DataType, Data, Connection, From)
  when is_list(Data)->
    channel_data(ChannelId, DataType, l2b(Data), Connection, From);

channel_data(ChannelId, DataType, Data, 
	     #connection{channel_cache = Cache} = Connection,
	     From) ->
    case ssh_channel:cache_lookup(Cache, ChannelId) of
	#channel{remote_id = Id, sent_close = false} = Channel0 ->
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
	    {{replies, Replies ++ FlowCtrlMsgs}, Connection};
	_ ->
	    {{replies,[{channel_request_reply,From,{error,closed}}]}, Connection}
    end.

handle_msg(#ssh_msg_channel_open_confirmation{recipient_channel = ChannelId, 
					      sender_channel = RemoteId,
					      initial_window_size = WindowSz,
					      maximum_packet_size = PacketSz}, 
	   #connection{channel_cache = Cache} = Connection0, _) ->
    
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
	   #connection{channel_cache = Cache} = Connection0, _) ->
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    ssh_channel:cache_delete(Cache, ChannelId),
    {Reply, Connection} = 
	reply_msg(Channel, Connection0, {open_error, Reason, Descr, Lang}),
    {{replies, [Reply]}, Connection};

handle_msg(#ssh_msg_channel_success{recipient_channel = ChannelId},
	   #connection{channel_cache = Cache} = Connection0, _) ->
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    case reply_msg(Channel, Connection0, success) of
	{[], Connection} ->
	    {noreply, Connection};
	{Reply, Connection} ->
	    {{replies, [Reply]}, Connection}
    end;

handle_msg(#ssh_msg_channel_failure{recipient_channel = ChannelId},
	   #connection{channel_cache = Cache} = Connection0, _) ->
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    case reply_msg(Channel, Connection0, failure) of
	{[], Connection} ->
	    {noreply, Connection};
	{Reply, Connection} ->
	    {{replies, [Reply]}, Connection}
    end;


handle_msg(#ssh_msg_channel_eof{recipient_channel = ChannelId}, 
	    #connection{channel_cache = Cache} = Connection0, _) ->
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    {Reply, Connection} = reply_msg(Channel, Connection0, {eof, ChannelId}),
    {{replies, [Reply]}, Connection};
   
handle_msg(#ssh_msg_channel_close{recipient_channel = ChannelId},   
	   #connection{channel_cache = Cache} = Connection0, _) ->

	case ssh_channel:cache_lookup(Cache, ChannelId) of
		#channel{sent_close = Closed, remote_id = RemoteId,
			 flow_control = FlowControl} = Channel ->
		ssh_channel:cache_delete(Cache, ChannelId),
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

		Replies = ConnReplyMsgs ++ [CloseMsg] ++ SendReplyMsgs,
		{{replies, Replies}, Connection};

	    undefined ->
		{{replies, []}, Connection0}
	end;

handle_msg(#ssh_msg_channel_data{recipient_channel = ChannelId,
				 data = Data}, 
	   #connection{channel_cache = Cache} = Connection0, _) ->
    
    case ssh_channel:cache_lookup(Cache, ChannelId) of
	#channel{recv_window_size = Size} = Channel ->
	    WantedSize = Size - size(Data),
	    ssh_channel:cache_update(Cache, Channel#channel{
					      recv_window_size = WantedSize}),
	    {Replies, Connection} =
		channel_data_reply(Cache, Channel, Connection0, 0, Data),
	    {{replies, Replies}, Connection};
	undefined ->
	    {noreply, Connection0}
    end;

handle_msg(#ssh_msg_channel_extended_data{recipient_channel = ChannelId,
					  data_type_code = DataType,
					  data = Data}, 
	   #connection{channel_cache = Cache} = Connection0, _) ->
    
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
	   #connection{channel_cache = Cache} = Connection, _) ->
    #channel{send_window_size = Size, remote_id = RemoteId} = 
	Channel0 = ssh_channel:cache_lookup(Cache, ChannelId), 
    
    {SendList, Channel} =  %% TODO: Datatype 0 ?
	update_send_window(Channel0#channel{send_window_size = Size + Add},
			   0, undefined, Connection),
    
    Replies = lists:map(fun({Type, Data}) -> 
				{connection_reply, channel_data_msg(RemoteId, Type, Data)}
			end, SendList),
    FlowCtrlMsgs = flow_control(Channel, Cache),
    {{replies, Replies ++ FlowCtrlMsgs}, Connection};

handle_msg(#ssh_msg_channel_open{channel_type = "session" = Type,
				 sender_channel = RemoteId,
				 initial_window_size = WindowSz,
				 maximum_packet_size = PacketSz}, 
	   #connection{options = SSHopts} = Connection0,
	   server) ->
    MinAcceptedPackSz = proplists:get_value(minimal_remote_max_packet_size, SSHopts, 0),
    
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
		    {{replies, [{connection_reply, FailMsg}]},
		     Connection0}
	    end;

	MinAcceptedPackSz > PacketSz ->
	    FailMsg = channel_open_failure_msg(RemoteId, 
					       ?SSH_OPEN_ADMINISTRATIVELY_PROHIBITED,
					       lists:concat(["Maximum packet size below ",MinAcceptedPackSz,
							      " not supported"]), "en"),
	    {{replies, [{connection_reply, FailMsg}]}, Connection0}
    end;

handle_msg(#ssh_msg_channel_open{channel_type = "session",
				 sender_channel = RemoteId}, 
	   Connection,
	   client) ->
    %% Client implementations SHOULD reject any session channel open
    %% requests to make it more difficult for a corrupt server to attack the
    %% client. See See RFC 4254 6.1.
    FailMsg = channel_open_failure_msg(RemoteId, 
				       ?SSH_OPEN_CONNECT_FAILED,
				       "Connection refused", "en"),
    {{replies, [{connection_reply, FailMsg}]},
     Connection};

handle_msg(#ssh_msg_channel_open{sender_channel = RemoteId}, Connection, _) ->
    FailMsg = channel_open_failure_msg(RemoteId, 
				       ?SSH_OPEN_ADMINISTRATIVELY_PROHIBITED,
				       "Not allowed", "en"),
    {{replies, [{connection_reply, FailMsg}]}, Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "exit-status",
				    data = Data},
	   #connection{channel_cache = Cache} = Connection, _) ->
    <<?UINT32(Status)>> = Data,
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    {Reply, Connection} = 
	reply_msg(Channel, Connection, {exit_status, ChannelId, Status}),
    {{replies, [Reply]}, Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "exit-signal",
				    want_reply = false,
				    data = Data},  
	   #connection{channel_cache = Cache} = Connection0, _) ->
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
    {{replies, [{connection_reply, CloseMsg}, Reply]},
     Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "xon-xoff",
				    want_reply = false,
				    data = Data},
	   #connection{channel_cache = Cache} = Connection, _) ->
    <<?BOOLEAN(CDo)>> = Data,
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    {Reply, Connection} = 
	reply_msg(Channel, Connection, {xon_xoff, ChannelId, CDo=/= 0}),
    {{replies, [Reply]}, Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "window-change",
				    want_reply = false,
				    data = Data}, 
	   #connection{channel_cache = Cache} = Connection0, _) ->
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
	   #connection{channel_cache = Cache} = Connection0, _) ->
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
	   #connection{channel_cache = Cache} = Connection, server) ->
    <<?UINT32(SsLen), SsName:SsLen/binary>> = Data,
    
    #channel{remote_id = RemoteId} = Channel0 = 
	ssh_channel:cache_lookup(Cache, ChannelId), 
    
    ReplyMsg =  {subsystem, ChannelId, WantReply, binary_to_list(SsName)},
    
    try
	{ok, Pid} = start_subsystem(SsName, Connection, Channel0, ReplyMsg),
	erlang:monitor(process, Pid),
	Channel = Channel0#channel{user = Pid},
	ssh_channel:cache_update(Cache, Channel),
	Reply = {connection_reply,
		 channel_success_msg(RemoteId)},
	{{replies, [Reply]}, Connection}
    catch
	_:_ ->
	    ErrorReply = {connection_reply,
			  channel_failure_msg(RemoteId)},
	    {{replies, [ErrorReply]}, Connection}
    end;	

handle_msg(#ssh_msg_channel_request{request_type = "subsystem"},
	   Connection, client) ->
    %% The client SHOULD ignore subsystem requests. See RFC 4254 6.5.
    {{replies, []}, Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "pty-req",
				    want_reply = WantReply,
				    data = Data},
	   #connection{channel_cache = Cache} = Connection, server) ->
    <<?UINT32(TermLen), BTermName:TermLen/binary,
     ?UINT32(Width),?UINT32(Height),
     ?UINT32(PixWidth), ?UINT32(PixHeight),
     Modes/binary>> = Data,
    TermName = binary_to_list(BTermName),
   
    PtyRequest = {TermName, Width, Height,
		  PixWidth, PixHeight, decode_pty_opts(Modes)},
    
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    
    handle_cli_msg(Connection, Channel,
		   {pty, ChannelId, WantReply, PtyRequest});

handle_msg(#ssh_msg_channel_request{request_type = "pty-req"},
	   Connection, client) ->
    %% The client SHOULD ignore pty requests. See RFC 4254 6.2.
    {{replies, []}, Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "shell",
				    want_reply = WantReply},
	   #connection{channel_cache = Cache} = Connection, server) ->
      
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    
    handle_cli_msg(Connection, Channel,
		   {shell, ChannelId, WantReply});
 
handle_msg(#ssh_msg_channel_request{request_type = "shell"},
	   Connection, client) ->
    %% The client SHOULD ignore shell requests. See RFC 4254 6.5.
    {{replies, []}, Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "exec",
				    want_reply = WantReply,
				    data = Data},
	   #connection{channel_cache = Cache} = Connection, server) ->
    <<?UINT32(Len), Command:Len/binary>> = Data,
     
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    
    handle_cli_msg(Connection, Channel,
		   {exec, ChannelId, WantReply, binary_to_list(Command)});
	
handle_msg(#ssh_msg_channel_request{request_type = "exec"},
	   Connection, client) ->
    %% The client SHOULD ignore exec requests. See RFC 4254 6.5.
    {{replies, []}, Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
  				    request_type = "env",
  				    want_reply = WantReply,
  				    data = Data}, 
	   #connection{channel_cache = Cache} = Connection, server) ->
    
    <<?UINT32(VarLen), 
     Var:VarLen/binary, ?UINT32(ValueLen), Value:ValueLen/binary>> = Data,
    
    Channel = ssh_channel:cache_lookup(Cache, ChannelId), 
    
    handle_cli_msg(Connection, Channel,
 		   {env, ChannelId, WantReply, Var, Value});

handle_msg(#ssh_msg_channel_request{request_type = "env"},
	   Connection, client) ->
    %% The client SHOULD ignore env requests. 
    {{replies, []}, Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = _Other,
				    want_reply = WantReply},
	   #connection{channel_cache = Cache} = Connection, _) ->
    if WantReply == true ->
		case ssh_channel:cache_lookup(Cache, ChannelId) of
		    #channel{remote_id = RemoteId}  -> 
			FailMsg = channel_failure_msg(RemoteId),
			{{replies, [{connection_reply, FailMsg}]},
			 Connection};
		    undefined -> %% Chanel has been closed
			{noreply, Connection}
		end;
       true ->
	    {noreply, Connection}
    end;

handle_msg(#ssh_msg_global_request{name = _Type,
				   want_reply = WantReply,
				   data = _Data}, Connection, _) ->
    if WantReply == true ->
	    FailMsg = request_failure_msg(),
	    {{replies, [{connection_reply, FailMsg}]},
	     Connection};
       true ->
	    {noreply, Connection}  
    end;

handle_msg(#ssh_msg_request_failure{},
	   #connection{requests = [{_, From} | Rest]} = Connection, _) ->
    {{replies, [{channel_request_reply, From, {failure, <<>>}}]},
     Connection#connection{requests = Rest}};
handle_msg(#ssh_msg_request_success{data = Data},
	   #connection{requests = [{_, From} | Rest]} = Connection, _) ->
    {{replies, [{channel_request_reply, From, {success, Data}}]},
     Connection#connection{requests = Rest}};

handle_msg(#ssh_msg_disconnect{code = Code,
			       description = Description,
			       language = _Lang },
	   #connection{channel_cache = Cache} = Connection0, _) ->
    {Connection, Replies} = 
	ssh_channel:cache_foldl(fun(Channel, {Connection1, Acc}) ->
					{Reply, Connection2} =  
					    reply_msg(Channel,
						      Connection1,
						      {closed, Channel#channel.local_id}),
					{Connection2, [Reply | Acc]}
				end, {Connection0, []}, Cache),
    
    ssh_channel:cache_delete(Cache),
    {disconnect, {Code, Description}, {{replies, Replies}, Connection}}.

handle_cli_msg(#connection{channel_cache = Cache} = Connection,
	       #channel{user = undefined, 
			remote_id = RemoteId,
			local_id = ChannelId} = Channel0, Reply0) -> 
    
    case (catch start_cli(Connection, ChannelId)) of
	{ok, Pid} ->
	    erlang:monitor(process, Pid),
	    Channel = Channel0#channel{user = Pid},
	    ssh_channel:cache_update(Cache, Channel),
	    {Reply, Connection1} = reply_msg(Channel, Connection, Reply0),
 	    {{replies, [Reply]}, Connection1};
	_Other ->
	    Reply = {connection_reply,
		     channel_failure_msg(RemoteId)},
	    {{replies, [Reply]}, Connection}
    end;

handle_cli_msg(Connection0, Channel, Reply0) ->
    {Reply, Connection} =  reply_msg(Channel, Connection0, Reply0),
    {{replies, [Reply]}, Connection}.

channel_eof_msg(ChannelId) ->
    #ssh_msg_channel_eof{recipient_channel = ChannelId}.

channel_close_msg(ChannelId) ->
    #ssh_msg_channel_close {recipient_channel = ChannelId}.

channel_status_msg({success, ChannelId}) ->
    channel_success_msg(ChannelId);
channel_status_msg({failure, ChannelId}) ->
    channel_failure_msg(ChannelId).

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

start_channel(Cb, Id, Args, SubSysSup, Opts) ->
    start_channel(Cb, Id, Args, SubSysSup, undefined, Opts).

start_channel(Cb, Id, Args, SubSysSup, Exec, Opts) ->
    ChildSpec = child_spec(Cb, Id, Args, Exec),
    ChannelSup = ssh_subsystem_sup:channel_supervisor(SubSysSup),
    assert_limit_num_channels_not_exceeded(ChannelSup, Opts),
    ssh_channel_sup:start_child(ChannelSup, ChildSpec).
    
assert_limit_num_channels_not_exceeded(ChannelSup, Opts) ->
    MaxNumChannels = proplists:get_value(max_channels, Opts, infinity),
    NumChannels = length([x || {_,_,worker,[ssh_channel]} <- 
				   supervisor:which_children(ChannelSup)]),
    if 
	%% Note that NumChannels is BEFORE starting a new one
	NumChannels < MaxNumChannels ->
	    ok;
	true ->
	    throw(max_num_channels_exceeded)
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
setup_session(#connection{channel_cache = Cache
			 } = Connection0, 
	      RemoteId,
	      Type, WindowSize, PacketSize) ->
    {ChannelId, Connection} = new_channel_id(Connection0),

    Channel = #channel{type = Type,
		       sys = "ssh",
		       local_id = ChannelId,
		       recv_window_size = ?DEFAULT_WINDOW_SIZE,
		       recv_packet_size = ?DEFAULT_PACKET_SIZE,
		       send_window_size = WindowSize,
		       send_packet_size = PacketSize,
		       send_buf = queue:new(),
		       remote_id = RemoteId
		      },
    ssh_channel:cache_update(Cache, Channel),
    OpenConfMsg = channel_open_confirmation_msg(RemoteId, ChannelId,
						?DEFAULT_WINDOW_SIZE, 
						?DEFAULT_PACKET_SIZE),

    {{replies, [{connection_reply, OpenConfMsg}]}, Connection}.


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

start_cli(#connection{cli_spec = no_cli}, _) ->
    {error, cli_disabled};
start_cli(#connection{options = Options,
		      cli_spec = {CbModule, Args},
		      exec = Exec,
		      sub_system_supervisor = SubSysSup}, ChannelId) ->
    start_channel(CbModule, ChannelId, Args, SubSysSup, Exec, Options).

start_subsystem(BinName, #connection{options = Options,
				    sub_system_supervisor = SubSysSup},
	       #channel{local_id = ChannelId}, _ReplyMsg) ->
    Name = binary_to_list(BinName),
    case check_subsystem(Name, Options) of
	{Callback, Opts} when is_atom(Callback), Callback =/= none ->
	    start_channel(Callback, ChannelId, Opts, SubSysSup, Options);
	{Other, _} when Other =/= none ->
	    {error, legacy_option_not_supported}
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
reply_msg(undefined, Connection, _Reply) ->
    {noreply, Connection};
reply_msg(#channel{user = ChannelPid}, Connection, Reply) ->
    {{channel_data, ChannelPid, Reply}, Connection}.


request_reply_or_data(#channel{local_id = ChannelId, user = ChannelPid}, 
		      #connection{requests = Requests} = 
		      Connection, Reply) -> 
    case lists:keysearch(ChannelId, 1, Requests) of
	{value, {ChannelId, From}} ->
	    {{channel_request_reply, From, Reply}, 
	     Connection#connection{requests = 
				       lists:keydelete(ChannelId, 1, Requests)}};
	false when (Reply == success) or (Reply == failure) ->
	    {[], Connection};
	false ->
	    {{channel_data, ChannelPid, Reply}, Connection}
    end.

update_send_window(Channel, _, undefined,
		   #connection{channel_cache = Cache}) ->
    do_update_send_window(Channel, Cache);

update_send_window(#channel{send_buf = SendBuffer} = Channel, DataType, Data,
		   #connection{channel_cache = Cache}) ->
    do_update_send_window(Channel#channel{send_buf = queue:in({DataType, Data}, SendBuffer)},
			  Cache).

do_update_send_window(Channel0, Cache) ->
    {SendMsgs, Channel} = get_window(Channel0, []),
    ssh_channel:cache_update(Cache, Channel), 
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
	    case handle_send_window(Msg, size(Data), PacketSize, WindowSize0, Acc0) of		
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

flow_control(Channel, Cache) ->
    flow_control([window_adjusted], Channel, Cache).

flow_control([], Channel, Cache) ->
    ssh_channel:cache_update(Cache, Channel),
    [];
flow_control([_|_], #channel{flow_control = From,
			     send_buf = Buffer} = Channel, Cache) when From =/= undefined ->
    case queue:is_empty(Buffer) of
	true ->
	    ssh_channel:cache_update(Cache, Channel#channel{flow_control = undefined}),
	    [{flow_control, Cache, Channel, From, ok}];
	false ->
	    []
    end;
flow_control(_,_,_) ->
    [].

pty_req(ConnectionHandler, Channel, Term, Width, Height,
	 PixWidth, PixHeight, PtyOpts, TimeOut) ->
    ssh_connection_handler:request(ConnectionHandler,
				   Channel, "pty-req", true,
				   [?string(Term),
				    ?uint32(Width), ?uint32(Height),
				    ?uint32(PixWidth),?uint32(PixHeight),
				    encode_pty_opts(PtyOpts)], TimeOut).

pty_default_dimensions(Dimension, Options) ->
    case proplists:get_value(Dimension, Options, 0) of
	N when is_integer(N), N > 0 ->
	    {N, 0};
	_ ->
	    case proplists:get_value(list_to_atom("pixel_" ++ atom_to_list(Dimension)), Options, 0) of
		N when is_integer(N), N > 0 ->
		    {0, N};
		_ ->
		    {?TERMINAL_WIDTH, 0}
	    end
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


backwards_compatible([], Acc) ->
    Acc;
backwards_compatible([{hight, Value} | Rest], Acc) ->
    backwards_compatible(Rest, [{height, Value} | Acc]);
backwards_compatible([{pixel_hight, Value} | Rest], Acc) ->
    backwards_compatible(Rest, [{height, Value} | Acc]);
backwards_compatible([Value| Rest], Acc) ->
    backwards_compatible(Rest, [ Value | Acc]).
