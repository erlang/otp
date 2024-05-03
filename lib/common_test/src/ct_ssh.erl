%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2024. All Rights Reserved.
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

-module(ct_ssh).
-moduledoc """
SSH/SFTP client module.

SSH/SFTP client module.

This module uses application `SSH`, which provides detailed information about,
for example, functions, types, and options.

Argument `Server` in the SFTP functions is only to be used for SFTP sessions
that have been started on existing SSH connections (that is, when the original
connection type is `ssh`). Whenever the connection type is `sftp`, use the SSH
connection reference only.

The following options are valid for specifying an SSH/SFTP connection (that is,
can be used as configuration elements):

```erlang
 [{ConnType, Addr},
  {port, Port},
  {user, UserName}
  {password, Pwd}
  {user_dir, String}
  {public_key_alg, PubKeyAlg}
  {connect_timeout, Timeout}
  {key_cb, KeyCallbackMod}]
```

`ConnType = ssh | sftp`.

For other types, see `m:ssh`.

All time-out parameters in `ct_ssh` functions are values in milliseconds.
""".

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
         subsystem/3, subsystem/4,
         shell/2, shell/3]).

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

-doc "Handle for a specific SSH/SFTP connection, see module `m:ct`.".
-type handle() :: pid().
-doc "Return value from an `m:ssh_sftp` function.".
-type ssh_sftp_return() :: term().
-doc "For `target_name`, see module `m:ct`.".
-type connection() :: handle() | ct:target_name().

%%%-----------------------------------------------------------------
%%%------------------------ SSH COMMANDS ---------------------------

-doc """
connect(KeyOrName) -> {ok, Handle} | {error, Reason}

Equivalent to [`ct_ssh:connect(KeyOrName, host, [])`](`connect/3`).
""".
connect(KeyOrName) ->
    connect(KeyOrName, host).

-doc """
connect(KeyOrName, ConnType) -> {ok, Handle} | {error, Reason}

Equivalent to [`ct_ssh:connect(KeyOrName, ConnType, [])`](`connect/3`).
""".
connect(KeyOrName, ConnType) when is_atom(ConnType) ->
    connect(KeyOrName, ConnType, []);

connect(KeyOrName, ExtraOpts) when is_list(ExtraOpts) ->
    connect(KeyOrName, host, ExtraOpts).

-doc """
connect(KeyOrName, ConnType, ExtraOpts) -> {ok, Handle} | {error, Reason}

Opens an SSH or SFTP connection using the information associated with
`KeyOrName`.

If `Name` (an alias name for `Key`) is used to identify the connection, this
name can be used as connection reference for subsequent calls. Only one open
connection at a time associated with `Name` is possible. If `Key` is used, the
returned handle must be used for subsequent calls (multiple connections can be
opened using the configuration data specified by `Key`).

For information on how to create a new `Name`, see `ct:require/2`.

For `target_name`, see module `m:ct`.

`ConnType` always overrides the type specified in the address tuple in the
configuration data (and in `ExtraOpts`). So it is possible to, for example, open
an SFTP connection directly using data originally specifying an SSH connection.
Value `host` means that the connection type specified by the host option (either
in the configuration data or in `ExtraOpts`) is used.

`ExtraOpts` (optional) are extra SSH options to be added to the configuration
data for `KeyOrName`. The extra options override any existing options with the
same key in the configuration data. For details on valid SSH options, see
application [`SSH`](`e:ssh:index.html`).
""".
connect(KeyOrName, ConnType, ExtraOpts) ->
    case ct:get_config(KeyOrName) of
	undefined ->
	    log(heading(connect,KeyOrName), "Failed: ~tp\n",
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
		    log(heading(connect,KeyOrName), "Failed: ~tp\n",
			[{not_available,{KeyOrName,ConnType1}}]),
		    {error,{not_available,{KeyOrName,ConnType1}}};
		{_,undefined} ->
		    try_log(heading(connect,KeyOrName), 
			    "Opening ~w connection to ~tp:22\n",
			    [ConnType1,Addr]),
		    ct_gen_conn:start(KeyOrName, {ConnType1,Addr,22}, 
				      AllOpts1, ?MODULE);		    
		{_,Port} ->
		    try_log(heading(connect,KeyOrName), 
			    "Opening ~w connection to ~tp:~w\n",
			    [ConnType1,Addr,Port]),
		    ct_gen_conn:start(KeyOrName, {ConnType1,Addr,Port}, 
				      AllOpts1, ?MODULE)
	    end
    end.

-doc """
disconnect(SSH) -> ok | {error, Reason}

Closes an SSH/SFTP connection.
""".
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

-doc """
session_open(SSH) -> {ok, ChannelId} | {error, Reason}

Equivalent to [`ct_ssh:session_open(SSH, DefaultTimeout)`](`session_open/2`).
""".
session_open(SSH) ->
    call(SSH, {session_open,?DEFAULT_TIMEOUT}).

-doc """
session_open(SSH, Timeout) -> {ok, ChannelId} | {error, Reason}

Opens a channel for an SSH session.
""".
session_open(SSH, Timeout) ->
    call(SSH, {session_open,Timeout}).

-doc """
session_close(SSH, ChannelId) -> ok | {error, Reason}

Closes an SSH session channel.
""".
session_close(SSH, ChannelId) ->
    call(SSH, {session_close,ChannelId}).

-doc """
exec(SSH, Command) -> {ok, Data} | {error, Reason}

Equivalent to [`ct_ssh:exec(SSH, Command, DefaultTimeout)`](`exec/3`).
""".
exec(SSH, Command) ->
    exec(SSH, undefined, Command, ?DEFAULT_TIMEOUT).

-doc """
exec(SSH, Command, Timeout) -> {ok, Data} | {error, Reason}

Requests server to perform `Command`. A session channel is opened automatically
for the request. `Data` is received from the server as a result of the command.
""".
exec(SSH, Command, Timeout) when is_list(Command) ->
    exec(SSH, undefined, Command, Timeout);

exec(SSH, ChannelId, Command) when is_integer(ChannelId) ->
    exec(SSH, ChannelId, Command, ?DEFAULT_TIMEOUT).

-doc """
exec(SSH, ChannelId, Command, Timeout) -> {ok, Data} | {error, Reason}

Requests server to perform `Command`. A previously opened session channel is
used for the request. `Data` is received from the server as a result of the
command.
""".
exec(SSH, ChannelId, Command, Timeout) ->
    call(SSH, {exec,ChannelId,Command,Timeout}).

-doc """
receive_response(SSH, ChannelId) -> {ok, Data} | {error, Reason}

Equivalent to
[`ct_ssh:receive_response(SSH, ChannelId, close)`](`receive_response/3`).
""".
receive_response(SSH, ChannelId) ->
    receive_response(SSH, ChannelId, close, ?DEFAULT_TIMEOUT).

-doc """
receive_response(SSH, ChannelId, End) -> {ok, Data} | {error, Reason}

Equivalent to
[`ct_ssh:receive_response(SSH, ChannelId, End, DefaultTimeout)`](`receive_response/4`).
""".
receive_response(SSH, ChannelId, End) when is_function(End) ->
    receive_response(SSH, ChannelId, End, ?DEFAULT_TIMEOUT);

receive_response(SSH, ChannelId, Timeout) when is_integer(Timeout) ->
    receive_response(SSH, ChannelId, close, Timeout).

-doc """
receive_response(SSH, ChannelId, End, Timeout) -> {ok, Data} | {timeout, Data} |
{error, Reason}

Receives expected data from server on the specified session channel.

If `End == close`, data is returned to the caller when the channel is closed by
the server. If a time-out occurs before this happens, the function returns
`{timeout,Data}` (where `Data` is the data received so far).

If `End == timeout`, a time-out is expected and `{ok,Data}` is returned both in
the case of a time-out and when the channel is closed.

If `End` is a fun, this fun is called with one argument, the data value in a
received `ssh_cm` message (see `m:ssh_connection`. The fun is to return either
`true` to end the receiving operation (and have the so far collected data
returned) or `false` to wait for more data from the server. Even if a fun is
supplied, the function returns immediately if the server closes the channel).
""".
receive_response(SSH, ChannelId, End, Timeout) ->
    call(SSH, {receive_response,ChannelId,End,Timeout}).

-doc """
send(SSH, ChannelId, Data) -> ok | {error, Reason}

Equivalent to
[`ct_ssh:send(SSH, ChannelId, 0, Data, DefaultTimeout)`](`send/5`).
""".
send(SSH, ChannelId, Data) ->
    send(SSH, ChannelId, 0, Data, ?DEFAULT_TIMEOUT).

-doc """
send(SSH, ChannelId, Data, Timeout) -> ok | {error, Reason}

Equivalent to [`ct_ssh:send(SSH, ChannelId, 0, Data, Timeout)`](`send/5`).
""".
send(SSH, ChannelId, Data, Timeout) when is_integer(Timeout) ->
    send(SSH, ChannelId, 0, Data, Timeout);

send(SSH, ChannelId, Type, Data) when is_integer(Type) ->
    send(SSH, ChannelId, Type, Data, ?DEFAULT_TIMEOUT).

-doc """
send(SSH, ChannelId, Type, Data, Timeout) -> ok | {error, Reason}

Sends data to server on specified session channel.
""".
send(SSH, ChannelId, Type, Data, Timeout) ->
    call(SSH, {send,ChannelId,Type,Data,Timeout}).

-doc """
send_and_receive(SSH, ChannelId, Data) -> {ok, Data} | {error, Reason}

Equivalent to
[`ct_ssh:send_and_receive(SSH, ChannelId, Data, close)`](`send_and_receive/4`).
""".
send_and_receive(SSH, ChannelId, Data) ->
    send_and_receive(SSH, ChannelId, 0, Data, close, ?DEFAULT_TIMEOUT).

-doc """
send_and_receive(SSH, ChannelId, Data, End) -> {ok, Data} | {error, Reason}

Equivalent to
[`ct_ssh;send_and_receive(SSH, ChannelId, 0, Data, End, DefaultTimeout)`](`send_and_receive/6`).
""".
send_and_receive(SSH, ChannelId, Data, End) when is_function(End) ->
    send_and_receive(SSH, ChannelId, 0, Data, End, ?DEFAULT_TIMEOUT);

send_and_receive(SSH, ChannelId, Data, Timeout) when is_integer(Timeout) ->
    send_and_receive(SSH, ChannelId, 0, Data, close, Timeout);

send_and_receive(SSH, ChannelId, Type, Data) when is_integer(Type) ->
    send_and_receive(SSH, ChannelId, Type, Data, close, ?DEFAULT_TIMEOUT).

-doc """
send_and_receive(SSH, ChannelId, Data, End, Timeout) -> {ok, Data} | {error,
Reason}

Equivalent to
[`ct_ssh:send_and_receive(SSH, ChannelId, 0, Data, End, Timeout)`](`send_and_receive/6`).
""".
send_and_receive(SSH, ChannelId, Data, End, Timeout) when is_integer(Timeout) ->
    send_and_receive(SSH, ChannelId, 0, Data, End, Timeout);

send_and_receive(SSH, ChannelId, Type, Data, Timeout) when is_integer(Type) ->
    send_and_receive(SSH, ChannelId, Type, Data, close, Timeout);

send_and_receive(SSH, ChannelId, Type, Data, End) when is_function(End) ->
    send_and_receive(SSH, ChannelId, Type, Data, End, ?DEFAULT_TIMEOUT).

-doc """
send_and_receive(SSH, ChannelId, Type, Data, End, Timeout) -> {ok, Data} |
{error, Reason}

Sends data to server on specified session channel and waits to receive the
server response.

For details on argument `End`, see
[`ct_ssh:receive_response/4`](`receive_response/4`).
""".
send_and_receive(SSH, ChannelId, Type, Data, End, Timeout) ->
    call(SSH, {send_and_receive,ChannelId,Type,Data,End,Timeout}).

-doc """
subsystem(SSH, ChannelId, Subsystem) -> Status | {error, Reason}

Equivalent to
[`ct_ssh:subsystem(SSH, ChannelId, Subsystem, DefaultTimeout)`](`subsystem/4`).
""".
subsystem(SSH, ChannelId, Subsystem) ->
    subsystem(SSH, ChannelId, Subsystem, ?DEFAULT_TIMEOUT).

-doc """
subsystem(SSH, ChannelId, Subsystem, Timeout) -> Status | {error, Reason}

Sends a request to execute a predefined subsystem.
""".
subsystem(SSH, ChannelId, Subsystem, Timeout) ->
    call(SSH, {subsystem,ChannelId,Subsystem,Timeout}).


-doc """
shell(SSH, ChannelId) -> ok | {error, Reason}

Equivalent to [`ct_ssh:shell(SSH, ChannelId, DefaultTimeout)`](`shell/3`).
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec shell(SSH, ChannelId) -> Result when
      SSH :: handle() | ct:target_name(),
      ChannelId :: ssh:ssh_channel_id(),
      Result :: ok | {error,term()}.
shell(SSH, ChannelId) ->
    shell(SSH, ChannelId, ?DEFAULT_TIMEOUT).

-doc """
shell(SSH, ChannelId, Timeout) -> ok | {error, Reason}

Requests that the user default shell (typically defined in `/etc/passwd` in Unix
systems) is executed at the server end.
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec shell(SSH, ChannelId, Timeout) -> Result when
      SSH :: handle() | ct:target_name(),
      ChannelId :: ssh:ssh_channel_id(),
      Timeout :: timeout(),
      Result :: ok | {error,term()}.
shell(SSH, ChannelId, Timeout) ->
    call(SSH, {shell,ChannelId,Timeout}).


%%%-----------------------------------------------------------------
%%%------------------------ SFTP COMMANDS --------------------------

-doc """
sftp_connect(SSH) -> {ok, Server} | {error, Reason}

Starts an SFTP session on an already existing SSH connection. `Server`
identifies the new session and must be specified whenever SFTP requests are to
be sent.
""".
sftp_connect(SSH) ->
    call(SSH, sftp_connect).

-doc """
read_file(SSH, File) -> Result

For information and other types, see `m:ssh_sftp`.
""".
read_file(SSH, File) ->
    call(SSH, {read_file,sftp,File}).

-doc """
read_file(SSH, Server, File) -> Result

For information and other types, see `m:ssh_sftp`.
""".
read_file(SSH, Server, File) ->
    call(SSH, {read_file,Server,File}).

-doc """
write_file(SSH, File, Iolist) -> Result

For information and other types, see `m:ssh_sftp`.
""".
write_file(SSH, File, Iolist) ->
    call(SSH, {write_file,sftp,File,Iolist}).

-doc """
write_file(SSH, Server, File, Iolist) -> Result

For information and other types, see `m:ssh_sftp`.
""".
write_file(SSH, Server, File, Iolist) ->
    call(SSH, {write_file,Server,File,Iolist}).

-doc """
list_dir(SSH, Path) -> Result

For information and other types, see `m:ssh_sftp`.
""".
list_dir(SSH, Path) ->
    call(SSH, {list_dir,sftp,Path}).

-doc """
list_dir(SSH, Server, Path) -> Result

For information and other types, see `m:ssh_sftp`.
""".
list_dir(SSH, Server, Path) ->
    call(SSH, {list_dir,Server,Path}).

-doc """
open(SSH, File, Mode) -> Result

For information and other types, see `m:ssh_sftp`.
""".
open(SSH, File, Mode) ->
    call(SSH, {open,sftp,File,Mode}).

-doc """
open(SSH, Server, File, Mode) -> Result

For information and other types, see `m:ssh_sftp`.
""".
open(SSH, Server, File, Mode) ->
    call(SSH, {open,Server,File,Mode}).

-doc """
opendir(SSH, Path) -> Result

For information and other types, see `m:ssh_sftp`.
""".
opendir(SSH, Path) ->
    call(SSH, {opendir,sftp,Path}).

-doc """
opendir(SSH, Server, Path) -> Result

For information and other types, see `m:ssh_sftp`.
""".
opendir(SSH, Server, Path) ->
    call(SSH, {opendir,Server,Path}).

-doc """
close(SSH, Handle) -> Result

For information and other types, see `m:ssh_sftp`.
""".
close(SSH, Handle) ->
    call(SSH, {close,sftp,Handle}).

-doc """
close(SSH, Server, Handle) -> Result

For information and other types, see `m:ssh_sftp`.
""".
close(SSH, Server, Handle) ->
    call(SSH, {close,Server,Handle}).

-doc """
read(SSH, Handle, Len) -> Result

For information and other types, see `m:ssh_sftp`.
""".
read(SSH, Handle, Len) ->
    call(SSH, {read,sftp,Handle,Len}).

-doc """
read(SSH, Server, Handle, Len) -> Result

For information and other types, see `m:ssh_sftp`.
""".
read(SSH, Server, Handle, Len) ->
    call(SSH, {read,Server,Handle,Len}).

-doc """
pread(SSH, Handle, Position, Length) -> Result

For information and other types, see `m:ssh_sftp`.
""".
pread(SSH, Handle, Position, Length) ->
    call(SSH, {pread,sftp,Handle,Position,Length}).

-doc """
pread(SSH, Server, Handle, Position, Length) -> Result

For information and other types, see `m:ssh_sftp`.
""".
pread(SSH, Server, Handle, Position, Length) ->
    call(SSH, {pread,Server,Handle,Position,Length}).

-doc """
aread(SSH, Handle, Len) -> Result

For information and other types, see `m:ssh_sftp`.
""".
aread(SSH, Handle, Len) ->
    call(SSH, {aread,sftp,Handle,Len}).

-doc """
aread(SSH, Server, Handle, Len) -> Result

For information and other types, see `m:ssh_sftp`.
""".
aread(SSH, Server, Handle, Len) ->
    call(SSH, {aread,Server,Handle,Len}).

-doc """
apread(SSH, Handle, Position, Length) -> Result

For information and other types, see `m:ssh_sftp`.
""".
-spec apread(SSH, Handle, Position, Length) -> Result when
      SSH :: connection(),
      Handle :: term(),
      Position :: integer(),
      Length :: integer(),
      Result :: ssh_sftp_return().
apread(SSH, Handle, Position, Length) ->
    call(SSH, {apread,sftp,Handle,Position,Length}).

-doc """
apread(SSH, Server, Handle, Position, Length) -> Result

For information and other types, see `m:ssh_sftp`.
""".
apread(SSH, Server, Handle, Position, Length) ->
    call(SSH, {apread,Server,Handle,Position,Length}).

-doc """
write(SSH, Handle, Data) -> Result

For information and other types, see `m:ssh_sftp`.
""".
write(SSH, Handle, Data) ->
    call(SSH, {write,sftp,Handle,Data}).

-doc """
write(SSH, Server, Handle, Data) -> Result

For information and other types, see `m:ssh_sftp`.
""".
write(SSH, Server, Handle, Data) ->
    call(SSH, {write,Server,Handle,Data}).

-doc """
pwrite(SSH, Handle, Position, Data) -> Result

For information and other types, see `m:ssh_sftp`.
""".
pwrite(SSH, Handle, Position, Data) ->
    call(SSH, {pwrite,sftp,Handle,Position,Data}).

-doc """
pwrite(SSH, Server, Handle, Position, Data) -> Result

For information and other types, see `m:ssh_sftp`.
""".
pwrite(SSH, Server, Handle, Position, Data) ->
    call(SSH, {pwrite,Server,Handle,Position,Data}).

-doc """
awrite(SSH, Handle, Data) -> Result

For information and other types, see `m:ssh_sftp`.
""".
awrite(SSH, Handle, Data) ->
    call(SSH, {awrite,sftp,Handle, Data}).

-doc """
awrite(SSH, Server, Handle, Data) -> Result

For information and other types, see `m:ssh_sftp`.
""".
awrite(SSH, Server, Handle, Data) ->
    call(SSH, {awrite,Server,Handle, Data}).

-doc """
apwrite(SSH, Handle, Position, Data) -> Result

For information and other types, see `m:ssh_sftp`.
""".
apwrite(SSH, Handle, Position, Data) ->
    call(SSH, {apwrite,sftp,Handle,Position,Data}).

-doc """
apwrite(SSH, Server, Handle, Position, Data) -> Result

For information and other types, see `m:ssh_sftp`.
""".
apwrite(SSH, Server, Handle, Position, Data) ->
    call(SSH, {apwrite,Server,Handle,Position,Data}).

-doc """
position(SSH, Handle, Location) -> Result

For information and other types, see `m:ssh_sftp`.
""".
position(SSH, Handle, Location) ->
    call(SSH, {position,sftp,Handle,Location}).

-doc """
position(SSH, Server, Handle, Location) -> Result

For information and other types, see `m:ssh_sftp`.
""".
position(SSH, Server, Handle, Location) ->
    call(SSH, {position,Server,Handle,Location}).

-doc """
read_file_info(SSH, Name) -> Result

For information and other types, see `m:ssh_sftp`.
""".
read_file_info(SSH, Name) ->
    call(SSH, {read_file_info,sftp,Name}).

-doc """
read_file_info(SSH, Server, Name) -> Result

For information and other types, see `m:ssh_sftp`.
""".
read_file_info(SSH, Server, Name) ->
    call(SSH, {read_file_info,Server,Name}).

-doc """
get_file_info(SSH, Handle) -> Result

For information and other types, see `m:ssh_sftp`.
""".
get_file_info(SSH, Handle) ->
    call(SSH, {get_file_info,sftp,Handle}).

-doc """
get_file_info(SSH, Server, Handle) -> Result

For information and other types, see `m:ssh_sftp`.
""".
get_file_info(SSH, Server, Handle) ->
    call(SSH, {get_file_info,Server,Handle}).

-doc """
read_link_info(SSH, Name) -> Result

For information and other types, see `m:ssh_sftp`.
""".
read_link_info(SSH, Name) ->
    call(SSH, {read_link_info,sftp,Name}).

-doc """
read_link_info(SSH, Server, Name) -> Result

For information and other types, see `m:ssh_sftp`.
""".
read_link_info(SSH, Server, Name) ->
    call(SSH, {read_link_info,Server,Name}).

-doc """
write_file_info(SSH, Name, Info) -> Result

For information and other types, see `m:ssh_sftp`.
""".
write_file_info(SSH, Name, Info) ->
    call(SSH, {write_file_info,sftp,Name,Info}).

-doc """
write_file_info(SSH, Server, Name, Info) -> Result

For information and other types, see `m:ssh_sftp`.
""".
write_file_info(SSH, Server, Name, Info) ->
    call(SSH, {write_file_info,Server,Name,Info}).

-doc """
read_link(SSH, Name) -> Result

For information and other types, see `m:ssh_sftp`.
""".
read_link(SSH, Name) ->
    call(SSH, {read_link,sftp,Name}).

-doc """
read_link(SSH, Server, Name) -> Result

For information and other types, see `m:ssh_sftp`.
""".
read_link(SSH, Server, Name) ->
    call(SSH, {read_link,Server,Name}).

-doc """
make_symlink(SSH, Name, Target) -> Result

For information and other types, see `m:ssh_sftp`.
""".
make_symlink(SSH, Name, Target) ->
    call(SSH, {make_symlink,sftp,Name,Target}).

-doc """
make_symlink(SSH, Server, Name, Target) -> Result

For information and other types, see `m:ssh_sftp`.
""".
make_symlink(SSH, Server, Name, Target) ->
    call(SSH, {make_symlink,Server,Name,Target}).

-doc """
rename(SSH, OldName, NewName) -> Result

For information and other types, see `m:ssh_sftp`.
""".
rename(SSH, OldName, NewName) ->
    call(SSH, {rename,sftp,OldName,NewName}).

-doc """
rename(SSH, Server, OldName, NewName) -> Result

For information and other types, see `m:ssh_sftp`.
""".
rename(SSH, Server, OldName, NewName) ->
    call(SSH, {rename,Server,OldName,NewName}).


-doc """
delete(SSH, Name) -> Result

For information and other types, see `m:ssh_sftp`.
""".
delete(SSH, Name) ->
    call(SSH, {delete,sftp,Name}).

-doc """
delete(SSH, Server, Name) -> Result

For information and other types, see `m:ssh_sftp`.
""".
delete(SSH, Server, Name) ->
    call(SSH, {delete,Server,Name}).

-doc """
make_dir(SSH, Name) -> Result

For information and other types, see `m:ssh_sftp`.
""".
make_dir(SSH, Name) ->
    call(SSH, {make_dir,sftp,Name}).

-doc """
make_dir(SSH, Server, Name) -> Result

For information and other types, see `m:ssh_sftp`.
""".
make_dir(SSH, Server, Name) ->
    call(SSH, {make_dir,Server,Name}).

-doc """
del_dir(SSH, Name) -> Result

For information and other types, see `m:ssh_sftp`.
""".
del_dir(SSH, Name) ->
    call(SSH, {del_dir,sftp,Name}).

-doc """
del_dir(SSH, Server, Name) -> Result

For information and other types, see `m:ssh_sftp`.
""".
del_dir(SSH, Server, Name) ->
    call(SSH, {del_dir,Server,Name}).


%%%=================================================================
%%% Callback functions
%%%=================================================================

-doc false.
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
		    "Host: ~tp (~p)\nUser: ~tp\nPassword: ~p\n",
		[ConnType,Addr,Port,User,
                 lists:duplicate(string:length(Password),$*)]),
	    {ok,SSHRef,#state{ssh_ref=SSHRef, conn_type=ConnType,
			      target=KeyOrName}}
    end.

-doc false.
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
	    log(heading(exec,Target), "Opening channel failed: ~tp", [ChnError]),
	    {ChnError,State};
	_ ->
	    try_log(heading(exec,Target), 
		    "SSH Ref: ~p, Chn: ~p, Command: ~tp, Timeout: ~p",
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
	    "Data: ~tp", [SSHRef,Chn,Type,TO,Data]),
    Result = ssh_connection:send(SSHRef, Chn, Type, Data, TO),
    {Result,State};

handle_msg({send_and_receive,Chn,Type,Data,End,TO}, State) -> 
    #state{ssh_ref=SSHRef, target=Target} = State,   
    try_log(heading(send_and_receive,Target), 
	    "SSH Ref: ~p, Chn: ~p, Type: ~p, Timeout: ~p~n"
	    "Data: ~tp", [SSHRef,Chn,Type,TO,Data]),
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
	    "SSH Ref: ~p, Chn: ~p, Subsys: ~tp, Timeout: ~p",
	    [SSHRef,Chn,Subsystem,TO]),
    Result = ssh_connection:subsystem(SSHRef, Chn, Subsystem, TO),
    {Result,State};

handle_msg({shell,Chn,TO}, State) ->
    #state{ssh_ref=SSHRef, target=Target} = State,
    try_log(heading(shell,Target),
	    "SSH Ref: ~p, Chn: ~p, Timeout: ~p",
            [SSHRef,Chn,TO]),
    Result = ssh_connection:shell(SSHRef, Chn),
    {Result,State};

%% --- SFTP Commands ---

handle_msg({read_file,Srv,File}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:read_file(ref(Srv,SSHRef), File),S};

handle_msg({write_file,Srv,File,Iolist}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:write_file(ref(Srv,SSHRef), File, Iolist),S};

handle_msg({list_dir,Srv,Path}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:list_dir(ref(Srv,SSHRef), Path),S};

handle_msg({open,Srv,File,Mode}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:open(ref(Srv,SSHRef), File, Mode),S};

handle_msg({opendir,Srv,Path}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:opendir(ref(Srv,SSHRef), Path),S};

handle_msg({close,Srv,Handle}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:close(ref(Srv,SSHRef), Handle),S};

handle_msg({read,Srv,Handle,Len}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:read(ref(Srv,SSHRef), Handle, Len),S};

handle_msg({pread,Srv,Handle,Position,Length}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:pread(ref(Srv,SSHRef),Handle,Position,Length),S};

handle_msg({aread,Srv,Handle,Len}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:aread(ref(Srv,SSHRef), Handle, Len),S};

handle_msg({apread,Srv,Handle,Position,Length}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:apread(ref(Srv,SSHRef), Handle, Position, Length),S};

handle_msg({write,Srv,Handle,Data}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:write(ref(Srv,SSHRef), Handle, Data),S};

handle_msg({pwrite,Srv,Handle,Position,Data}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:pwrite(ref(Srv,SSHRef), Handle, Position, Data),S};

handle_msg({awrite,Srv,Handle,Data}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:awrite(ref(Srv,SSHRef), Handle, Data),S};

handle_msg({apwrite,Srv,Handle,Position,Data}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:apwrite(ref(Srv,SSHRef), Handle, Position, Data),S};

handle_msg({position,Srv,Handle,Location}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:position(ref(Srv,SSHRef), Handle, Location),S};

handle_msg({read_file_info,Srv,Name}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:read_file_info(ref(Srv,SSHRef), Name),S};

handle_msg({get_file_info,Srv,Handle}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:get_file_info(ref(Srv,SSHRef), Handle),S};

handle_msg({read_link_info,Srv,Name}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:read_link_info(ref(Srv,SSHRef), Name),S};

handle_msg({write_file_info,Srv,Name,Info}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:write_file_info(ref(Srv,SSHRef), Name, Info),S};

handle_msg({read_link,Srv,Name}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:read_link(ref(Srv,SSHRef), Name),S};

handle_msg({make_symlink,Srv,Name,Target}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:make_symlink(ref(Srv,SSHRef), Name, Target),S};

handle_msg({rename,Srv,OldName,NewName}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:rename(ref(Srv,SSHRef), OldName, NewName),S};

handle_msg({delete,Srv,Name}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:delete(ref(Srv,SSHRef), Name),S};

handle_msg({make_dir,Srv,Name}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:make_dir(ref(Srv,SSHRef), Name),S};

handle_msg({del_dir,Srv,Name}=Cmd, S=#state{ssh_ref=SSHRef}) ->
    try_log(heading(sftp,S#state.target), 
	    "SSH Ref: ~p, Server: ~p~nCmd: ~tp",
	    [SSHRef,ref(Srv,SSHRef),mod(Cmd)]),
    {ssh_sftp:del_dir(ref(Srv,SSHRef), Name),S}.

-doc false.
reconnect(_Addr,_State) ->
    {error,no_reconnection_of_ssh}.

-doc false.
close(SSHRef) ->
    disconnect(SSHRef).

-doc false.
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

do_recv_response(SSH, Chn, Data, End, Timeout) ->
    receive
	{ssh_cm, SSH, {open,Chn,RemoteChn,{session}}} ->
	    debug("RECVD open"),
	    {ok,{open,Chn,RemoteChn,{session}}};

	{ssh_cm, SSH, {closed,Chn}} ->
	    ssh_connection:close(SSH, Chn),
	    debug("CLSD~n~p ~p", [SSH,Chn]),
	    {ok,Data};

	{ssh_cm, SSH, {data,Chn,_,NewData}} when is_binary(NewData) ->
	    ssh_connection:adjust_window(SSH, Chn, byte_size(NewData)),
	    debug("RECVD~n~tp", [binary_to_list(NewData)]),
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
	    debug("UNEXPECTED MESSAGE~n~p ~p~n~tp", [SSH,Chn,Other]),
	    do_recv_response(SSH, Chn, Data, End, Timeout)

    after Timeout ->
	    case End of
		timeout -> 
		    {ok,Data};
		_ -> 
		    {timeout,Data}
	    end
    end.

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

call(SSH, Msg) ->
    call(SSH, Msg, infinity).
	
call(SSH, Msg, Timeout) ->
    case get_handle(SSH) of
	{ok,Pid} ->
	    ct_gen_conn:call(Pid, Msg, Timeout);
	Error ->
	    Error
    end.

ref(sftp, SSHRef) -> SSHRef;
ref(Server, _) -> Server.

mod(Cmd) ->
    [Op,_Server|Args] = tuple_to_list(Cmd),
    list_to_tuple([Op|Args]).

heading(Function, Ref) ->
    io_lib:format("ct_ssh:~tw ~tp",[Function,Ref]).

log(Heading, Str, Args) ->
    ct_gen_conn:log(Heading, Str, Args).  

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

debug(Str) ->
    debug(Str, []).

debug(_Str, _Args) ->
    %%    io:format("~n--- ct_ssh debug ---~n" ++ _Str ++ "~n", _Args),
    ok.
