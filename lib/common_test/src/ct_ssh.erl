%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

%%% @doc SSH/SFTP client module. 
%%%
%%% <p>ct_ssh uses the OTP ssh application and more detailed information 
%%% about e.g. functions, types and options can be found in the 
%%% documentation for this application.</p>
%%%
%%% <p>The <code>Server</code> argument in the SFTP functions should
%%% only be used for SFTP sessions that have been started on existing
%%% SSH connections (i.e. when the original connection type is 
%%% <code>ssh</code>). Whenever the connection type is
%%% <code>sftp</code>, use the SSH connection reference only.</p>
%%%
%%% <p>The following options are valid for specifying an SSH/SFTP
%%% connection (i.e. may be used as config elements):</p> 
%%%
%%% <pre>
%%%
%%%  [{ConnType, Addr},
%%%   {port, Port},
%%%   {user, UserName}
%%%   {password, Pwd}
%%%   {user_dir, String}
%%%   {public_key_alg, PubKeyAlg}
%%%   {connect_timeout, Timeout}
%%%   {key_cb, KeyCallbackMod}]
%%% </pre>
%%%
%%% <p><code>ConnType = ssh | sftp</code>.</p> 
%%% <p>Please see ssh(3) for other types.</p>
%%%
%%% <p>All timeout parameters in ct_ssh functions are values in
%%% milliseconds.</p>
%%%
%%% @type connection() = handle() | ct:target_name()
%%% @type handle() = ct_gen_conn:handle(). Handle for a specific
%%% SSH/SFTP connection.
%%% @type ssh_sftp_return() = term(). A return value from an ssh_sftp function.

-module(ct_ssh).

%% SSH Functions
-export([connect/1, connect/2, connect/3,
 	 disconnect/1,
 	 session_open/1, session_open/2,
 	 session_close/2,
 	 send/3, send/4, send/5,
 	 receive_response/2, receive_response/3, receive_response/4,
 	 send_and_receive/3, send_and_receive/4, send_and_receive/5,
	 send_and_receive/6,
 	 exec/2, exec/3, exec/4,
 	 subsystem/3, subsystem/4]).

%% STFP Functions
-export([sftp_connect/1, 

	 read_file/2, write_file/3, list_dir/2, open/3, opendir/2, 
	 close/2, read/3, pread/4, aread/3, apread/4, write/3, 
	 pwrite/4, awrite/3, apwrite/4, position/3, read_file_info/2, 
	 get_file_info/2, read_link_info/2, write_file_info/3, 
	 read_link/2, make_symlink/3, rename/3, delete/2, make_dir/2, 
	 del_dir/2,

	 read_file/3, write_file/4, list_dir/3, open/4, opendir/3, 
	 close/3, read/4, pread/5, aread/4, apread/5, write/4, 
	 pwrite/5, awrite/4, apwrite/5, position/4, read_file_info/3, 
	 get_file_info/3, read_link_info/3, write_file_info/4, 
	 read_link/3, make_symlink/4, rename/4, delete/3, make_dir/3, 
	 del_dir/3]).

%% Callbacks
-export([init/3, handle_msg/2, reconnect/2, terminate/2, close/1]).

-define(DEFAULT_TIMEOUT, 10000).

-record(state, {ssh_ref, conn_type, target}).


%%%-----------------------------------------------------------------
%%%------------------------ SSH COMMANDS ---------------------------

%%%-----------------------------------------------------------------
%%% @spec connect(KeyOrName) -> {ok,Handle} | {error,Reason}
%%% @equiv connect(KeyOrName,host,[])
connect(KeyOrName) ->
    connect(KeyOrName, host).

%%%-----------------------------------------------------------------
%%% @spec connect(KeyOrName,ConnType) -> {ok,Handle} | {error,Reason}
%%% @equiv connect(KeyOrName,ConnType,[])
connect(KeyOrName, ConnType) when is_atom(ConnType) ->
    connect(KeyOrName, ConnType, []);

%%%-----------------------------------------------------------------
%%% @spec connect(KeyOrName,ExtraOpts) -> {ok,Handle} | {error,Reason}
%%% @equiv connect(KeyOrName,host,ExtraOpts)
connect(KeyOrName, ExtraOpts) when is_list(ExtraOpts) ->
    connect(KeyOrName, host, ExtraOpts).

%%%-----------------------------------------------------------------
%%% @spec connect(KeyOrName,ConnType,ExtraOpts) -> 
%%%          {ok,Handle} | {error,Reason}
%%%      KeyOrName = Key | Name
%%%      Key = atom()
%%%      Name = ct:target_name()
%%%      ConnType = ssh | sftp | host
%%%      ExtraOpts = ssh_connect_options()
%%%      Handle = handle()
%%%      Reason = term()
%%%
%%% @doc Open an SSH or SFTP connection using the information
%%%      associated with <code>KeyOrName</code>. 
%%%
%%%      <p>If <code>Name</code> (an alias name for <code>Key</code>), 
%%%      is used to identify the connection, this name may
%%%      be used as connection reference for subsequent calls.
%%%      It's only possible to have one open connection at a time 
%%%      associated with <code>Name</code>. If <code>Key</code> is
%%%      used, the returned handle must be used for subsequent calls
%%%      (multiple connections may be opened using the config
%%%      data specified by <code>Key</code>). See <c>ct:require/2</c>
%%%      for how to create a new <c>Name</c></p>
%%%
%%%      <p><code>ConnType</code> will always override the type
%%%      specified in the address tuple in the configuration data (and
%%%      in <code>ExtraOpts</code>). So it is possible to for example 
%%%      open an sftp connection directly using data originally
%%%      specifying an ssh connection. The value <code>host</code>
%%%      means the connection type specified by the host option
%%%      (either in the configuration data or in <code>ExtraOpts</code>)
%%%      will be used.</p>
%%%
%%%      <p><code>ExtraOpts</code> (optional) are extra SSH options 
%%%      to be added to the config data for <code>KeyOrName</code>. 
%%%      The extra options will override any existing options with the
%%%      same key in the config data. For details on valid SSH
%%%      options, see the documentation for the OTP ssh application.</p>
%%%
%%% @see ct:require/2
connect(KeyOrName, ConnType, ExtraOpts) ->
    case ct:get_config(KeyOrName) of
	undefined ->
	    log(heading(connect,KeyOrName), "Failed: ~p\n",
		[{not_available,KeyOrName}]),
	    {error,{not_available,KeyOrName}};
	SSHData ->
	    AllOpts = ExtraOpts++SSHData,
	    {ConnType1,Addr,AllOpts1} =
		case ConnType of
		    host ->
			case proplists:get_value(ssh, AllOpts) of
			    undefined ->
				case proplists:get_value(sftp, AllOpts) of
				    undefined ->
					log(heading(connect,KeyOrName), 
					    "No host information specified!\n",[]);
				    SFTPAddr ->
					{sftp,SFTPAddr,AllOpts}
				end;
			    SSHAddr ->
				{ssh,SSHAddr,AllOpts}
			end;
		    _ ->
			case proplists:get_value(ConnType, AllOpts) of
			    undefined when ConnType == ssh ->
				case proplists:get_value(sftp, AllOpts) of
				    undefined ->
					{ssh,undefined,AllOpts};
				    SFTPAddr ->
					try_log(heading(connect,KeyOrName), 
						"Note: Opening ssh connection "
						"to sftp host.\n",
					    []),
					{ssh,SFTPAddr,
					 [{ssh,SFTPAddr} |
					  proplists:delete(sftp, AllOpts)]}
				end;
			    undefined when ConnType == sftp ->
				case proplists:get_value(ssh, AllOpts) of
				    undefined ->
					{sftp,undefined,AllOpts};
				    SSHAddr ->
					try_log(heading(connect,KeyOrName), 
						"Note: Opening sftp connection "
						"to ssh host.\n",
					    []),
					{sftp,SSHAddr,
					 [{sftp,SSHAddr}|proplists:delete(ssh, AllOpts)]}
				end;
			    SSHorSFTPAddr ->
				{ConnType,SSHorSFTPAddr,AllOpts}
			end
		end,
	    case {Addr,proplists:get_value(port, AllOpts1)} of
		{undefined,_} ->
		    log(heading(connect,KeyOrName), "Failed: ~p\n",
			[{not_available,{KeyOrName,ConnType1}}]),
		    {error,{not_available,{KeyOrName,ConnType1}}};
		{_,undefined} ->
		    try_log(heading(connect,KeyOrName), 
			    "Opening ~w connection to ~p:22\n",
			    [ConnType1,Addr]),
		    ct_gen_conn:start(KeyOrName, {ConnType1,Addr,22}, 
				      AllOpts1, ?MODULE);		    
		{_,Port} ->
		    try_log(heading(connect,KeyOrName), 
			    "Opening ~w connection to ~p:~w\n",
			    [ConnType1,Addr,Port]),
		    ct_gen_conn:start(KeyOrName, {ConnType1,Addr,Port}, 
				      AllOpts1, ?MODULE)
	    end
    end.

%%%-----------------------------------------------------------------
%%% @spec disconnect(SSH) -> ok | {error,Reason}
%%%      SSH = connection()
%%%      Reason = term()
%%%
%%% @doc Close an SSH/SFTP connection.
disconnect(SSH) ->
    case get_handle(SSH) of
	{ok,Pid} ->
	    try_log(heading(disconnect,SSH), "Handle: ~p", [Pid], 5000),
	    case ct_gen_conn:stop(Pid) of
		{error,{process_down,Pid,noproc}} ->
		    {error,already_closed};
		Result ->
		    Result
	    end;
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% @spec session_open(SSH) -> {ok,ChannelId} | {error, Reason}
%%% @equiv session_open(SSH,DefaultTimeout) 
session_open(SSH) ->
    call(SSH, {session_open,?DEFAULT_TIMEOUT}).

%%%-----------------------------------------------------------------
%%% @spec session_open(SSH,Timeout) -> {ok,ChannelId} | {error, Reason}
%%%      SSH = connection()
%%%      Timeout = integer()
%%%      ChannelId = integer()
%%%      Reason = term()
%%%
%%% @doc Opens a channel for an SSH session.
session_open(SSH, Timeout) ->
    call(SSH, {session_open,Timeout}).

%%%-----------------------------------------------------------------
%%% @spec session_close(SSH,ChannelId) -> ok | {error, Reason}
%%%      SSH = connection()
%%%      ChannelId = integer()
%%%      Reason = term()
%%%
%%% @doc Closes an SSH session channel.
session_close(SSH, ChannelId) ->
    call(SSH, {session_close,ChannelId}).

%%%-----------------------------------------------------------------
%%% @spec exec(SSH,Command) -> {ok,Data} | {error,Reason}
%%% @equiv exec(SSH,Command,DefaultTimeout)
exec(SSH, Command) ->
    exec(SSH, undefined, Command, ?DEFAULT_TIMEOUT).

%%%-----------------------------------------------------------------
%%% @spec exec(SSH,Command,Timeout) -> {ok,Data} | {error,Reason}
%%%      SSH = connection()
%%%      Command = string()
%%%      Timeout = integer()
%%%      Data = list()
%%%      Reason = term()
%%% 
%%% @doc Requests server to perform <code>Command</code>. A session
%%%      channel is opened automatically for the request.
%%%      <code>Data</code> is received from the server as a result
%%%      of the command.
exec(SSH, Command, Timeout) when is_list(Command) ->
    exec(SSH, undefined, Command, Timeout);

%%%-----------------------------------------------------------------
%%% @spec exec(SSH,ChannelId,Command) -> {ok,Data} | {error,Reason}
%%% @equiv exec(SSH,ChannelId,Command,DefaultTimeout)
exec(SSH, ChannelId, Command) when is_integer(ChannelId) ->
    exec(SSH, ChannelId, Command, ?DEFAULT_TIMEOUT).

%%%-----------------------------------------------------------------
%%% @spec exec(SSH,ChannelId,Command,Timeout) -> {ok,Data} | {error,Reason}
%%%      SSH = connection()
%%%      ChannelId = integer()
%%%      Command = string()
%%%      Timeout = integer()
%%%      Data = list()
%%%      Reason = term()
%%% 
%%% @doc Requests server to perform <code>Command</code>. A previously
%%%      opened session channel is used for the request.
%%%      <code>Data</code> is received from the server as a result
%%%      of the command.
exec(SSH, ChannelId, Command, Timeout) ->
    call(SSH, {exec,ChannelId,Command,Timeout}).

%%%-----------------------------------------------------------------
%%% @spec receive_response(SSH,ChannelId) -> {ok,Data} | {error,Reason}
%%% @equiv receive_response(SSH,ChannelId,close)
receive_response(SSH, ChannelId) ->
    receive_response(SSH, ChannelId, close, ?DEFAULT_TIMEOUT).

%%%-----------------------------------------------------------------
%%% @spec receive_response(SSH,ChannelId,End) -> {ok,Data} | {error,Reason}
%%% @equiv receive_response(SSH,ChannelId,End,DefaultTimeout)
receive_response(SSH, ChannelId, End) when is_function(End) ->
    receive_response(SSH, ChannelId, End, ?DEFAULT_TIMEOUT);

%%%-----------------------------------------------------------------
%%% @spec receive_response(SSH,ChannelId,Timeout) -> {ok,Data} | {error,Reason}
%%% @equiv receive_response(SSH,ChannelId,close,Timeout)
receive_response(SSH, ChannelId, Timeout) when is_integer(Timeout) ->
    receive_response(SSH, ChannelId, close, Timeout).

%%%-----------------------------------------------------------------
%%% @spec receive_response(SSH,ChannelId,End,Timeout) -> 
%%%                    {ok,Data} | {timeout,Data} | {error,Reason}
%%%      SSH = connection()
%%%      ChannelId = integer()
%%%      End = Fun | close | timeout
%%%      Timeout = integer()
%%%      Data = list()
%%%      Reason = term()
%%%      
%%% @doc Receives expected data from server on the specified
%%%      session channel. 
%%%
%%%      <p>If <code>End == close</code>, data is returned
%%%      to the caller when the channel is closed by the
%%%      server. If a timeout occurs before this happens,
%%%      the function returns <code>{timeout,Data}</code>
%%%      (where <code>Data</code> is the data received so far).
%%%      If <code>End == timeout</code>, a timeout is expected
%%%      and <code>{ok,Data}</code> is returned both in the case
%%%      of a timeout and when the channel is closed. If 
%%%      <code>End</code> is a fun, this fun will be
%%%      called with one argument - the data value in a received
%%%      <code>ssh_cm</code> message (see ssh_connection(3)). The
%%%      fun should return <code>true</code> to end the receiving 
%%%      operation (and have the so far collected data returned), or
%%%      <code>false</code> to wait for more data from the server.
%%%      (Note that even if a fun is supplied, the function returns
%%%      immediately if the server closes the channel).</p>
receive_response(SSH, ChannelId, End, Timeout) ->
    call(SSH, {receive_response,ChannelId,End,Timeout}).

%%%-----------------------------------------------------------------
%%% @spec send(SSH,ChannelId,Data) -> ok | {error,Reason}
%%% @equiv send(SSH,ChannelId,0,Data,DefaultTimeout)
send(SSH, ChannelId, Data) ->
    send(SSH, ChannelId, 0, Data, ?DEFAULT_TIMEOUT).

%%%-----------------------------------------------------------------
%%% @spec send(SSH,ChannelId,Data,Timeout) -> ok | {error,Reason}
%%% @equiv send(SSH,ChannelId,0,Data,Timeout)
send(SSH, ChannelId, Data, Timeout) when is_integer(Timeout) ->
    send(SSH, ChannelId, 0, Data, Timeout);

%%%-----------------------------------------------------------------
%%% @spec send(SSH,ChannelId,Type,Data) -> ok | {error,Reason}
%%% @equiv send(SSH,ChannelId,Type,Data,DefaultTimeout)
send(SSH, ChannelId, Type, Data) when is_integer(Type) ->
    send(SSH, ChannelId, Type, Data, ?DEFAULT_TIMEOUT).

%%%-----------------------------------------------------------------
%%% @spec send(SSH,ChannelId,Type,Data,Timeout) -> ok | {error,Reason}
%%%      SSH = connection()
%%%      ChannelId = integer()
%%%      Type = integer()
%%%      Data = list()
%%%      Timeout = integer()
%%%      Reason = term()
%%% 
%%% @doc Send data to server on specified session channel.
send(SSH, ChannelId, Type, Data, Timeout) ->
    call(SSH, {send,ChannelId,Type,Data,Timeout}).

%%%-----------------------------------------------------------------
%%% @spec send_and_receive(SSH,ChannelId,Data) -> 
%%%                   {ok,Data} | {error,Reason}
%%% @equiv send_and_receive(SSH,ChannelId,Data,close)
send_and_receive(SSH, ChannelId, Data) ->
    send_and_receive(SSH, ChannelId, 0, Data, close, ?DEFAULT_TIMEOUT).

%%%-----------------------------------------------------------------
%%% @spec send_and_receive(SSH,ChannelId,Data,End) -> 
%%%                   {ok,Data} | {error,Reason}
%%% @equiv send_and_receive(SSH,ChannelId,0,Data,End,DefaultTimeout)
send_and_receive(SSH, ChannelId, Data, End) when is_function(End) ->
    send_and_receive(SSH, ChannelId, 0, Data, End, ?DEFAULT_TIMEOUT);

%%%-----------------------------------------------------------------
%%% @spec send_and_receive(SSH,ChannelId,Data,Timeout) -> 
%%%                   {ok,Data} | {error,Reason}
%%% @equiv send_and_receive(SSH,ChannelId,0,Data,close,Timeout)
send_and_receive(SSH, ChannelId, Data, Timeout) when is_integer(Timeout) ->
    send_and_receive(SSH, ChannelId, 0, Data, close, Timeout);

%%%-----------------------------------------------------------------
%%% @spec send_and_receive(SSH,ChannelId,Type,Data) -> 
%%%                   {ok,Data} | {error,Reason}
%%% @equiv send_and_receive(SSH,ChannelId,Type,Data,close,DefaultTimeout)
send_and_receive(SSH, ChannelId, Type, Data) when is_integer(Type) ->
    send_and_receive(SSH, ChannelId, Type, Data, close, ?DEFAULT_TIMEOUT).

%%%-----------------------------------------------------------------
%%% @spec send_and_receive(SSH,ChannelId,Data,End,Timeout) -> 
%%%                   {ok,Data} | {error,Reason}
%%% @equiv send_and_receive(SSH,ChannelId,0,Data,End,Timeout)
send_and_receive(SSH, ChannelId, Data, End, Timeout) when is_integer(Timeout) ->
    send_and_receive(SSH, ChannelId, 0, Data, End, Timeout);

%%%-----------------------------------------------------------------
%%% @spec send_and_receive(SSH,ChannelId,Type,Data,Timeout) -> 
%%%                   {ok,Data} | {error,Reason}
%%% @equiv send_and_receive(SSH,ChannelId,Type,Data,close,Timeout)
send_and_receive(SSH, ChannelId, Type, Data, Timeout) when is_integer(Type) ->
    send_and_receive(SSH, ChannelId, Type, Data, close, Timeout);

%%%-----------------------------------------------------------------
%%% @spec send_and_receive(SSH,ChannelId,Type,Data,End) -> 
%%%                   {ok,Data} | {error,Reason}
%%% @equiv send_and_receive(SSH,ChannelId,Type,Data,End,DefaultTimeout)
send_and_receive(SSH, ChannelId, Type, Data, End) when is_function(End) ->
    send_and_receive(SSH, ChannelId, Type, Data, End, ?DEFAULT_TIMEOUT).

%%%-----------------------------------------------------------------
%%% @spec send_and_receive(SSH,ChannelId,Type,Data,End,Timeout) -> 
%%%                   {ok,Data} | {error,Reason}
%%%      SSH = connection()
%%%      ChannelId = integer()
%%%      Type = integer()
%%%      Data = list()
%%%      End = Fun | close | timeout
%%%      Timeout = integer()
%%%      Reason = term()
%%%
%%% @doc Send data to server on specified session channel and wait
%%%      to receive the server response. 
%%%
%%%      <p>See <code>receive_response/4</code> for details on the 
%%%      <code>End</code> argument.</p>
send_and_receive(SSH, ChannelId, Type, Data, End, Timeout) ->
    call(SSH, {send_and_receive,ChannelId,Type,Data,End,Timeout}).

%%%-----------------------------------------------------------------
%%% @spec subsystem(SSH,ChannelId,Subsystem) -> Status | {error,Reason}
%%% @equiv subsystem(SSH,ChannelId,Subsystem,DefaultTimeout)
subsystem(SSH, ChannelId, Subsystem) ->
    subsystem(SSH, ChannelId, Subsystem, ?DEFAULT_TIMEOUT).

%%%-----------------------------------------------------------------
%%% @spec subsystem(SSH,ChannelId,Subsystem,Timeout) -> 
%%%             Status | {error,Reason}
%%%      SSH = connection()
%%%      ChannelId = integer()
%%%      Subsystem = string()
%%%      Timeout = integer()
%%%      Status = success | failure
%%%      Reason = term()
%%% 
%%% @doc Sends a request to execute a predefined subsystem.
subsystem(SSH, ChannelId, Subsystem, Timeout) ->
    call(SSH, {subsystem,ChannelId,Subsystem,Timeout}).


%%%-----------------------------------------------------------------
%%%------------------------ SFTP COMMANDS --------------------------

%%%-----------------------------------------------------------------
%%% @spec sftp_connect(SSH) -> {ok,Server} | {error,Reason}
%%%      SSH = connection()
%%%      Server = pid()
%%%      Reason = term()
%%% @doc Starts an SFTP session on an already existing SSH connection.
%%%      <code>Server</code> identifies the new session and must be
%%%      specified whenever SFTP requests are to be sent.
sftp_connect(SSH) ->
    call(SSH, sftp_connect).

%%%-----------------------------------------------------------------
%%% @spec read_file(SSH, File) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
read_file(SSH, File) ->
    call(SSH, {read_file,sftp,File}).
%%%-----------------------------------------------------------------
%%% @spec read_file(SSH, Server, File) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
read_file(SSH, Server, File) ->
    call(SSH, {read_file,Server,File}).

%%%-----------------------------------------------------------------
%%% @spec write_file(SSH, File, Iolist) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
write_file(SSH, File, Iolist) ->
    call(SSH, {write_file,sftp,File,Iolist}).
%%%-----------------------------------------------------------------
%%% @spec write_file(SSH, Server, File, Iolist) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
write_file(SSH, Server, File, Iolist) ->
    call(SSH, {write_file,Server,File,Iolist}).

%%%-----------------------------------------------------------------
%%% @spec list_dir(SSH, Path) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
list_dir(SSH, Path) ->
    call(SSH, {list_dir,sftp,Path}).
%%%-----------------------------------------------------------------
%%% @spec list_dir(SSH, Server, Path) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
list_dir(SSH, Server, Path) ->
    call(SSH, {list_dir,Server,Path}).

%%%-----------------------------------------------------------------
%%% @spec open(SSH, File, Mode) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
open(SSH, File, Mode) ->
    call(SSH, {open,sftp,File,Mode}).
%%%-----------------------------------------------------------------
%%% @spec open(SSH, Server, File, Mode) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
open(SSH, Server, File, Mode) ->
    call(SSH, {open,Server,File,Mode}).

%%%-----------------------------------------------------------------
%%% @spec opendir(SSH, Path) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
opendir(SSH, Path) ->
    call(SSH, {opendir,sftp,Path}).
%%%-----------------------------------------------------------------
%%% @spec opendir(SSH, Server, Path) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
opendir(SSH, Server, Path) ->
    call(SSH, {opendir,Server,Path}).

%%%-----------------------------------------------------------------
%%% @spec close(SSH, Handle) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
close(SSH, Handle) ->
    call(SSH, {close,sftp,Handle}).
%%%-----------------------------------------------------------------
%%% @spec close(SSH, Server, Handle) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
close(SSH, Server, Handle) ->
    call(SSH, {close,Server,Handle}).

%%%-----------------------------------------------------------------
%%% @spec read(SSH, Handle, Len) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
read(SSH, Handle, Len) ->
    call(SSH, {read,sftp,Handle,Len}).
%%%-----------------------------------------------------------------
%%% @spec read(SSH, Server, Handle, Len) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
read(SSH, Server, Handle, Len) ->
    call(SSH, {read,Server,Handle,Len}).

%%%-----------------------------------------------------------------
%%% @spec pread(SSH, Handle, Position, Length) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
pread(SSH, Handle, Position, Length) ->
    call(SSH, {pread,sftp,Handle,Position,Length}).
%%%-----------------------------------------------------------------
%%% @spec pread(SSH, Server, Handle, Position, Length) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
pread(SSH, Server, Handle, Position, Length) ->
    call(SSH, {pread,Server,Handle,Position,Length}).

%%%-----------------------------------------------------------------
%%% @spec aread(SSH, Handle, Len) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
aread(SSH, Handle, Len) ->
    call(SSH, {aread,sftp,Handle,Len}).
%%%-----------------------------------------------------------------
%%% @spec aread(SSH, Server, Handle, Len) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
aread(SSH, Server, Handle, Len) ->
    call(SSH, {aread,Server,Handle,Len}).

%%%-----------------------------------------------------------------
%%% @spec apread(SSH, Handle, Position, Length) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
apread(SSH, Handle, Position, Length) ->
    call(SSH, {apread,sftp,Handle,Position,Length}).
%%%-----------------------------------------------------------------
%%% @spec apread(SSH, Server, Handle, Position, Length) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
apread(SSH, Server, Handle, Position, Length) ->
    call(SSH, {apread,Server,Handle,Position,Length}).

%%%-----------------------------------------------------------------
%%% @spec write(SSH, Handle, Data) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
write(SSH, Handle, Data) ->
    call(SSH, {write,sftp,Handle,Data}).
%%%-----------------------------------------------------------------
%%% @spec write(SSH, Server, Handle, Data) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
write(SSH, Server, Handle, Data) ->
    call(SSH, {write,Server,Handle,Data}).

%%%-----------------------------------------------------------------
%%% @spec pwrite(SSH, Handle, Position, Data) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
pwrite(SSH, Handle, Position, Data) ->
    call(SSH, {pwrite,sftp,Handle,Position,Data}).
%%%-----------------------------------------------------------------
%%% @spec pwrite(SSH, Server, Handle, Position, Data) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
pwrite(SSH, Server, Handle, Position, Data) ->
    call(SSH, {pwrite,Server,Handle,Position,Data}).

%%%-----------------------------------------------------------------
%%% @spec awrite(SSH, Handle, Data) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
awrite(SSH, Handle, Data) ->
    call(SSH, {awrite,sftp,Handle, Data}).
%%%-----------------------------------------------------------------
%%% @spec awrite(SSH, Server, Handle, Data) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
awrite(SSH, Server, Handle, Data) ->
    call(SSH, {awrite,Server,Handle, Data}).

%%%-----------------------------------------------------------------
%%% @spec apwrite(SSH, Handle, Position, Data) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
apwrite(SSH, Handle, Position, Data) ->
    call(SSH, {apwrite,sftp,Handle,Position,Data}).
%%%-----------------------------------------------------------------
%%% @spec apwrite(SSH, Server, Handle, Position, Data) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
apwrite(SSH, Server, Handle, Position, Data) ->
    call(SSH, {apwrite,Server,Handle,Position,Data}).

%%%-----------------------------------------------------------------
%%% @spec position(SSH, Handle, Location) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
position(SSH, Handle, Location) ->
    call(SSH, {position,sftp,Handle,Location}).
%%%-----------------------------------------------------------------
%%% @spec position(SSH, Server, Handle, Location) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
position(SSH, Server, Handle, Location) ->
    call(SSH, {position,Server,Handle,Location}).

%%%-----------------------------------------------------------------
%%% @spec read_file_info(SSH, Name) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
read_file_info(SSH, Name) ->
    call(SSH, {read_file_info,sftp,Name}).
%%%-----------------------------------------------------------------
%%% @spec read_file_info(SSH, Server, Name) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
read_file_info(SSH, Server, Name) ->
    call(SSH, {read_file_info,Server,Name}).

%%%-----------------------------------------------------------------
%%% @spec get_file_info(SSH, Handle) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
get_file_info(SSH, Handle) ->
    call(SSH, {get_file_info,sftp,Handle}).
%%%-----------------------------------------------------------------
%%% @spec get_file_info(SSH, Server, Handle) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
get_file_info(SSH, Server, Handle) ->
    call(SSH, {get_file_info,Server,Handle}).

%%%-----------------------------------------------------------------
%%% @spec read_link_info(SSH, Name) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
read_link_info(SSH, Name) ->
    call(SSH, {read_link_info,sftp,Name}).
%%%-----------------------------------------------------------------
%%% @spec read_link_info(SSH, Server, Name) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
read_link_info(SSH, Server, Name) ->
    call(SSH, {read_link_info,Server,Name}).

%%%-----------------------------------------------------------------
%%% @spec write_file_info(SSH, Name, Info) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
write_file_info(SSH, Name, Info) ->
    call(SSH, {write_file_info,sftp,Name,Info}).
%%%-----------------------------------------------------------------
%%% @spec write_file_info(SSH, Server, Name, Info) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
write_file_info(SSH, Server, Name, Info) ->
    call(SSH, {write_file_info,Server,Name,Info}).

%%%-----------------------------------------------------------------
%%% @spec read_link(SSH, Name) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
read_link(SSH, Name) ->
    call(SSH, {read_link,sftp,Name}).
%%%-----------------------------------------------------------------
%%% @spec read_link(SSH, Server, Name) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
read_link(SSH, Server, Name) ->
    call(SSH, {read_link,Server,Name}).

%%%-----------------------------------------------------------------
%%% @spec make_symlink(SSH, Name, Target) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
make_symlink(SSH, Name, Target) ->
    call(SSH, {make_symlink,sftp,Name,Target}).
%%%-----------------------------------------------------------------
%%% @spec make_symlink(SSH, Server, Name, Target) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
make_symlink(SSH, Server, Name, Target) ->
    call(SSH, {make_symlink,Server,Name,Target}).

%%%-----------------------------------------------------------------
%%% @spec rename(SSH, OldName, NewName) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
rename(SSH, OldName, NewName) ->
    call(SSH, {rename,sftp,OldName,NewName}).
%%%-----------------------------------------------------------------
%%% @spec rename(SSH, Server, OldName, NewName) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
rename(SSH, Server, OldName, NewName) ->
    call(SSH, {rename,Server,OldName,NewName}).

%%%-----------------------------------------------------------------
%%% @spec delete(SSH, Name) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
delete(SSH, Name) ->
    call(SSH, {delete,sftp,Name}).
%%%-----------------------------------------------------------------
%%% @spec delete(SSH, Server, Name) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
delete(SSH, Server, Name) ->
    call(SSH, {delete,Server,Name}).

%%%-----------------------------------------------------------------
%%% @spec make_dir(SSH, Name) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
make_dir(SSH, Name) ->
    call(SSH, {make_dir,sftp,Name}).
%%%-----------------------------------------------------------------
%%% @spec make_dir(SSH, Server, Name) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
make_dir(SSH, Server, Name) ->
    call(SSH, {make_dir,Server,Name}).

%%%-----------------------------------------------------------------
%%% @spec del_dir(SSH, Name) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
del_dir(SSH, Name) ->
    call(SSH, {del_dir,sftp,Name}).
%%%-----------------------------------------------------------------
%%% @spec del_dir(SSH, Server, Name) -> Result
%%%      SSH = connection()
%%%      Result = ssh_sftp_return() | {error,Reason}
%%%      Reason = term()
%%% @doc For info and other types, see ssh_sftp(3).
del_dir(SSH, Server, Name) ->
    call(SSH, {del_dir,Server,Name}).


%%%=================================================================
%%% Callback functions
%%%=================================================================

%% @hidden
init(KeyOrName, {ConnType,Addr,Port}, AllOpts) ->
    User = proplists:get_value(user, AllOpts),
    Password = case proplists:get_value(password, AllOpts) of
		   undefined -> "";
		   Pwd -> Pwd
	       end,
    AllOpts1 = case proplists:get_value(connect_timeout, AllOpts) of
		   undefined ->
		       [{connect_timeout,?DEFAULT_TIMEOUT}|AllOpts];
		   _ ->
		       AllOpts		      
	       end,
    Options =
	lists:foldl(fun({ssh,_},Opts) -> Opts;
		       ({sftp,_},Opts) -> Opts;
		       ({port,_},Opts) -> Opts;
		       ({silently_accept_hosts,_},Opts) -> Opts;
		       ({user_interaction,_},Opts) -> Opts;
		       (Opt={Key,_},Opts) -> 
			    case lists:keymember(Key, 1, Opts) of
				true -> Opts;
				false -> [Opt|Opts]
			    end;
		       (_,Opts) -> Opts
		    end, [], AllOpts1),
    FinalOptions = [{silently_accept_hosts,true},
		    {user_interaction,false} | Options],
    _ = crypto:start(),
    _ = ssh:start(),
    Result = case ConnType of
		 ssh ->
		     ssh:connect(Addr, Port, FinalOptions);
		 sftp ->
		     ssh_sftp:start_channel(Addr, Port, FinalOptions)
	     end,
    case Result of
	Error = {error,_} ->
	    Error;
	Ok ->
	    SSHRef = element(2, Ok),
	    try_log(heading(init,KeyOrName), 
		    "Opened ~w connection:\n"
		    "Host: ~p (~p)\nUser: ~p\nPassword: ~p\n",
		[ConnType,Addr,Port,User,lists:duplicate(length(Password),$*)]),
	    {ok,SSHRef,#state{ssh_ref=SSHRef, conn_type=ConnType,
			      target=KeyOrName}}
    end.

%% @hidden
handle_msg(sftp_connect, State) ->
    #state{ssh_ref=SSHRef, target=Target} = State,
    try_log(heading(sftp_connect,Target), "SSH Ref: ~p", [SSHRef]),
    {ssh_sftp:start_channel(SSHRef),State};

handle_msg({session_open,TO}, State) ->
    #state{ssh_ref=SSHRef, target=Target} = State,
    try_log(heading(session_open,Target), "SSH Ref: ~p, Timeout: ~p",
	    [SSHRef,TO]),
    {ssh_connection:session_channel(SSHRef, TO),State};

handle_msg({session_close,Chn}, State) ->
    #state{ssh_ref=SSHRef, target=Target} = State,
    try_log(heading(session_close,Target), "SSH Ref: ~p, Chn: ~p", [SSHRef,Chn]),
    {ssh_connection:close(SSHRef, Chn),State};

handle_msg({exec,Chn,Command,TO}, State) ->
    #state{ssh_ref=SSHRef, target=Target} = State,
    Chn1 = 
	if Chn == undefined ->
		try_log(heading(exec,Target), 
			"Opening channel for exec, SSH Ref: ~p", [SSHRef]),
		case ssh_connection:session_channel(SSHRef, TO) of	
		    {ok,C} -> C;
		    CErr -> CErr
		end;
	   true ->
		Chn
	end,
    case Chn1 of
	{error,_} = ChnError ->
	    log(heading(exec,Target), "Opening channel failed: ~p", [ChnError]),
	    {ChnError,State};
	_ ->
	    try_log(heading(exec,Target), 
		    "SSH Ref: ~p, Chn: ~p, Command: ~p, Timeout: ~p", 
		    [SSHRef,Chn1,Command,TO]),
	    case ssh_connection:exec(SSHRef, Chn1, Command, TO) of
		success ->
		    Result = do_recv_response(SSHRef, Chn1, [], close, TO),
		    ssh_connection:close(SSHRef, Chn1),
		    {Result,State};
		Other ->
		    {{error,Other},State}
	    end
    end;

handle_msg({receive_response,Chn,End,TO}, State) ->
    #state{ssh_ref=SSHRef, target=Target} = State,
    try_log(heading(receive_response,Target), 
	    "SSH Ref: ~p, Chn: ~p, Timeout: ~p", [SSHRef,Chn,TO]),
    Result = do_recv_response(SSHRef, Chn, [], End, TO),
    {Result,State};

handle_msg({send,Chn,Type,Data,TO}, State) ->
    #state{ssh_ref=SSHRef, target=Target} = State,
    try_log(heading(send,Target), 
	    "SSH Ref: ~p, Chn: ~p, Type: ~p, Timeout: ~p~n"
	    "Data: ~p", [SSHRef,Chn,Type,TO,Data]),
    Result = ssh_connection:send(SSHRef, Chn, Type, Data, TO),
    {Result,State};

handle_msg({send_and_receive,Chn,Type,Data,End,TO}, State) -> 
    #state{ssh_ref=SSHRef, target=Target} = State,   
    try_log(heading(send_and_receive,Target), 
	    "SSH Ref: ~p, Chn: ~p, Type: ~p, Timeout: ~p~n"
	    "Data: ~p", [SSHRef,Chn,Type,TO,Data]),
    case ssh_connection:send(SSHRef, Chn, Type, Data, TO) of
	ok ->
	    Result = do_recv_response(SSHRef, Chn, [], End, TO),
	    {Result,State};
	Error ->
	    {Error,State}
    end;

handle_msg({subsystem,Chn,Subsystem,TO}, State) ->
    #state{ssh_ref=SSHRef, target=Target} = State,
    try_log(heading(subsystem,Target), 
	    "SSH Ref: ~p, Chn: ~p, Subsys: ~p, Timeout: ~p", 
	    [SSHRef,Chn,Subsystem,TO]),
    Result = ssh_connection:subsystem(SSHRef, Chn, Subsystem, TO),
    {Result,State};

%% --- SFTP Commands ---

handle_msg({read_file,Srv,File}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:read_file(ref(Srv,SSHRef), File),S};

handle_msg({write_file,Srv,File,Iolist}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:write_file(ref(Srv,SSHRef), File, Iolist),S};

handle_msg({list_dir,Srv,Path}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:list_dir(ref(Srv,SSHRef), Path),S};

handle_msg({open,Srv,File,Mode}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:open(ref(Srv,SSHRef), File, Mode),S};

handle_msg({opendir,Srv,Path}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:opendir(ref(Srv,SSHRef), Path),S};

handle_msg({close,Srv,Handle}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:close(ref(Srv,SSHRef), Handle),S};

handle_msg({read,Srv,Handle,Len}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:read(ref(Srv,SSHRef), Handle, Len),S};

handle_msg({pread,Srv,Handle,Position,Length}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:pread(ref(Srv,SSHRef),Handle,Position,Length),S};

handle_msg({aread,Srv,Handle,Len}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:aread(ref(Srv,SSHRef), Handle, Len),S};

handle_msg({apread,Srv,Handle,Position,Length}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:apread(ref(Srv,SSHRef), Handle, Position, Length),S};

handle_msg({write,Srv,Handle,Data}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:write(ref(Srv,SSHRef), Handle, Data),S};

handle_msg({pwrite,Srv,Handle,Position,Data}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:pwrite(ref(Srv,SSHRef), Handle, Position, Data),S};

handle_msg({awrite,Srv,Handle,Data}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:awrite(ref(Srv,SSHRef), Handle, Data),S};

handle_msg({apwrite,Srv,Handle,Position,Data}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:apwrite(ref(Srv,SSHRef), Handle, Position, Data),S};

handle_msg({position,Srv,Handle,Location}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:position(ref(Srv,SSHRef), Handle, Location),S};

handle_msg({read_file_info,Srv,Name}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:read_file_info(ref(Srv,SSHRef), Name),S};

handle_msg({get_file_info,Srv,Handle}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:get_file_info(ref(Srv,SSHRef), Handle),S};

handle_msg({read_link_info,Srv,Name}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:read_link_info(ref(Srv,SSHRef), Name),S};

handle_msg({write_file_info,Srv,Name,Info}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:write_file_info(ref(Srv,SSHRef), Name, Info),S};

handle_msg({read_link,Srv,Name}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:read_link(ref(Srv,SSHRef), Name),S};

handle_msg({make_symlink,Srv,Name,Target}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:make_symlink(ref(Srv,SSHRef), Name, Target),S};

handle_msg({rename,Srv,OldName,NewName}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:rename(ref(Srv,SSHRef), OldName, NewName),S};

handle_msg({delete,Srv,Name}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:delete(ref(Srv,SSHRef), Name),S};

handle_msg({make_dir,Srv,Name}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:make_dir(ref(Srv,SSHRef), Name),S};

handle_msg({del_dir,Srv,Name}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~p",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:del_dir(ref(Srv,SSHRef), Name),S}.

%% @hidden
reconnect(_Addr,_State) ->
    {error,no_reconnection_of_ssh}.

%% @hidden
close(SSHRef) ->
    disconnect(SSHRef).

%% @hidden
terminate(SSHRef, State) ->
    case State#state.conn_type of
	ssh ->
	    try_log(heading(disconnect_ssh,State#state.target),
		    "SSH Ref: ~p",[SSHRef], 5000),
	    ssh:close(SSHRef);
	sftp ->
	    try_log(heading(disconnect_sftp,State#state.target),
		    "SFTP Ref: ~p",[SSHRef], 5000),
	    ssh_sftp:stop_channel(SSHRef)
    end.


%%%=================================================================
%%% Internal functions

%%%-----------------------------------------------------------------
%%% 
do_recv_response(SSH, Chn, Data, End, Timeout) ->
    receive
	{ssh_cm, SSH, {open,Chn,RemoteChn,{session}}} ->
	    debug("RECVD open"),
	    {ok,{open,Chn,RemoteChn,{session}}};

	{ssh_cm, SSH, {closed,Chn}} ->
	    ssh_connection:close(SSH, Chn),
	    debug("CLSD~n~p ~p", [SSH,Chn]),
	    {ok,Data};

	{ssh_cm, SSH, {data,Chn,_,NewData}} ->
	    ssh_connection:adjust_window(SSH, Chn, size(NewData)),
	    debug("RECVD~n~p", [binary_to_list(NewData)]),
	    DataAcc = Data ++ binary_to_list(NewData),
	    if is_function(End) ->
		    case End(DataAcc) of
			true -> 
			    {ok,DataAcc};
			false ->
			    do_recv_response(SSH, Chn, DataAcc, End, Timeout)
		    end;
	       true ->
		    do_recv_response(SSH, Chn, DataAcc, End, Timeout)
	    end;

	{ssh_cm, SSH, {eof,Chn}} ->
	    debug("RECVD EOF~n~p ~p", [SSH,Chn]),
	    {ok,Data};

	{ssh_cm, SSH, {exit_signal,Chn,Signal,Err,_Lang}} ->
	    debug("RECVD exit_signal~n~p ~p~n~p ~p", [SSH,Chn,Signal,Err]),
	    do_recv_response(SSH, Chn, Data, End, Timeout);
	%%	    {ok,{exit_signal,Chn,Signal,Err,_Lang}};

	{ssh_cm, SSH, {exit_status,Chn,Status}} ->
	    debug("RECVD exit_status~n~p ~p~n~p", [SSH,Chn,Status]),
	    do_recv_response(SSH, Chn, Data, End, Timeout);
	%%	    {ok,{exit_status,Chn,_Status}};


	%%      --- INTERACTIVE MESSAGES - NOT HANDLED ---
	%%
	%% 	{ssh_cm, SSH, {subsystem,Chn,WantReply,Name}} ->
	%% 	    debug("RECVD SUBS WNTRPLY~n~p ~p~n~p~n~p",
	%% 		  [SSH,Chn,WantReply]),
	%% 	    ssh_connection:reply_request(SSH, WantReply, success, Chn),
	%% 	    do_recv_response(SSH, Chn, Data, End, Timeout);

	%% 	{ssh_cm, SSH, {shell,WantReply}} ->
	%% 	    debug("RECVD SHELL WNTRPLY~n~p ~p~n~p~n~p",
	%% 		  [SSH,Chn,WantReply]),
	%% 	    ssh_connection:reply_request(SSH, WantReply, success, Chn),
	%% 	    do_recv_response(SSH,Chn,Data,End,Timeout);

	%% 	{ssh_cm, SSH, {pty,Chn,WantReply,Pty}} ->
	%% 	    debug("RECVD PTY WNTRPLY~n~p ~p~n~p~n~p",
	%% 		  [SSH,Chn,WantReply,Pty]),
	%% 	    ssh_connection:reply_request(SSH, WantReply, success, Chn),
	%% 	    do_recv_response(SSH, Chn, Data, End, Timeout);

	%%	{ssh_cm, SSH, WCh={window_change,_Chn,_Width,_Height,_PixWidth,_PixHeight}} ->
	%%	    debug("RECVD WINCH"),
	%%	    {ok,WCh};

	Other ->
	    debug("UNEXPECTED MESSAGE~n~p ~p~n~p", [SSH,Chn,Other]),
	    do_recv_response(SSH, Chn, Data, End, Timeout)

    after Timeout ->
	    case End of
		timeout -> 
		    {ok,Data};
		_ -> 
		    {timeout,Data}
	    end
    end.

%%%-----------------------------------------------------------------
%%% 
get_handle(SSH) when is_pid(SSH) ->
    {ok,SSH};
get_handle(SSH) ->
    case ct_util:get_connection(SSH, ?MODULE) of
	{ok,{Pid,_}} ->
	    {ok,Pid};
	{error,no_registered_connection} ->
	    connect(SSH);
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% 
call(SSH, Msg) ->
    call(SSH, Msg, infinity).
	
call(SSH, Msg, Timeout) ->
    case get_handle(SSH) of
	{ok,Pid} ->
	    ct_gen_conn:call(Pid, Msg, Timeout);
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% 
ref(sftp, SSHRef) -> SSHRef;
ref(Server, _) -> Server.

%%%-----------------------------------------------------------------
%%% 
mod(Cmd) ->
    [Op,_Server|Args] = tuple_to_list(Cmd),
    list_to_tuple([Op|Args]).

%%%-----------------------------------------------------------------
%%% 	  
heading(Function, Ref) ->
    io_lib:format("ct_ssh:~w ~p",[Function,Ref]).

%%%-----------------------------------------------------------------
%%% 
log(Heading, Str, Args) ->
    ct_gen_conn:log(Heading, Str, Args).  

%%%-----------------------------------------------------------------
%%% 
try_log(Heading, Str, Args) ->
    try_log(Heading, Str, Args, infinity).

try_log(Heading, Str, Args, Timeout) ->
    case ct_util:is_silenced(ssh, Timeout) of
	true ->
	    ok;
	false ->
	    ct_gen_conn:log(Heading, Str, Args);
	_Error ->
	    ok
    end.

%%%-----------------------------------------------------------------
%%% 
debug(Str) ->
    debug(Str, []).

debug(_Str, _Args) ->
    %%    io:format("~n--- ct_ssh debug ---~n" ++ _Str ++ "~n", _Args),
    ok.
