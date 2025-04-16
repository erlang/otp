%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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
-doc "Reference to opened SSH/SFTP connection associated to either a `handle` or `target_name`.".
-type connection() :: handle() | ct:target_name().
-doc "Connection type used for connect.".
-type connection_type() :: 'host' | 'ssh' | 'sftp'.
-doc """
The valid values are `0` ("normal") and `1` ("stderr"), see
[RFC 4254, Section 5.2](https://tools.ietf.org/html/rfc4254#page-8).
""".
-type ssh_data_type_code() :: non_neg_integer().
-doc """
Data type representing a channel inside a connection.

"For `ssh_channel_id`, see module `m:ssh`.".
""".
-type ssh_channel_id() :: non_neg_integer().

%%%-----------------------------------------------------------------
%%%------------------------ SSH COMMANDS ---------------------------

-doc(#{equiv => connect(KeyOrName, host, [])}).
-spec connect(KeyOrName) -> {'ok', Handle} | {'error', Reason}
              when KeyOrName :: ct:key_or_name(),
                   Handle :: handle(),
                   Reason :: term().
connect(KeyOrName) ->
    connect(KeyOrName, host).

-doc """
Opens an SSH or SFTP connection using the information associated with `KeyOrName`
(see `connect/3`).

Equivalent to [`connect(KeyOrName, ConnType, [])`](`connect/3`) if
called with ConnType being atom.

Equivalent to [`connect(KeyOrName, host, ExtraOpts)`](`connect/3`) if
called with ExtraOpts being list.
""".
-spec connect(KeyOrName, ConnType) -> {'ok', Handle} | {'error', Reason}
              when KeyOrName :: ct:key_or_name(),
                   ConnType :: connection_type(),
                   Handle :: handle(),
                   Reason :: term();
             (KeyOrName, ExtraOpts) -> {'ok', Handle} | {'error', Reason}
              when KeyOrName :: ct:key_or_name(),
                   ExtraOpts :: [ExtraOption],
                   ExtraOption :: {'ssh', Address} | {'sftp', Address} | ssh:client_option()
                                | ssh_sftp:sftp_option(),
                   Address :: ssh:host(),
                   Handle :: handle(),
                   Reason :: term().
connect(KeyOrName, ConnType) when is_atom(ConnType) ->
    connect(KeyOrName, ConnType, []);

connect(KeyOrName, ExtraOpts) when is_list(ExtraOpts) ->
    connect(KeyOrName, host, ExtraOpts).

-doc """
Opens an SSH or SFTP connection using the information associated with
`KeyOrName`.

If `Name` (an alias name for `Key`) is used to identify the connection, this
name can be used as connection reference for subsequent calls. Only one open
connection at a time associated with `Name` is possible. If `Key` is used, the
returned handle must be used for subsequent calls (multiple connections can be
opened using the configuration data specified by `Key`).

For information on how to create a new `Name`, see `ct:require/2`.

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
-spec connect(KeyOrName, ConnType, ExtraOpts) -> {'ok', Handle} | {'error', Reason}
              when KeyOrName :: ct:key_or_name(),
                   ConnType :: connection_type(),
                   ExtraOpts :: [ExtraOption],
                   ExtraOption :: {'ssh', Address} | {'sftp', Address} | ssh:client_option()
                                | ssh_sftp:sftp_option(),
                   Address :: ssh:host(),
                   Handle :: handle(),
                   Reason :: term().
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
Closes an SSH/SFTP connection.
""".
-spec disconnect(SSH) -> 'ok' | {'error', Reason}
              when SSH :: connection(),
                   Reason :: term().
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

-doc(#{equiv => session_open(SSH, DefaultTimeout)}).
-spec session_open(SSH) -> {'ok', ChannelId} | {'error', Reason}
              when SSH :: connection(),
                   ChannelId :: ssh_channel_id(),
                   Reason :: term().
session_open(SSH) ->
    call(SSH, {session_open,?DEFAULT_TIMEOUT}).

-doc """
Opens a channel for an SSH session.
""".
-spec session_open(SSH, Timeout) -> {'ok', ChannelId} | {'error', Reason}
              when SSH :: connection(),
                   Timeout :: timeout(),
                   ChannelId :: ssh_channel_id(),
                   Reason :: term().
session_open(SSH, Timeout) ->
    call(SSH, {session_open,Timeout}).

-doc """
Closes an SSH session channel.
""".
-spec session_close(SSH, ChannelId) -> 'ok' | {'error', Reason}
              when SSH :: connection(),
                   ChannelId :: ssh_channel_id(),
                   Reason :: term().
session_close(SSH, ChannelId) ->
    call(SSH, {session_close,ChannelId}).

-doc(#{equiv => exec(SSH, Command, DefaultTimeout)}).
-spec exec(SSH, Command) -> {'ok', Data} | {'timeout', Data} | {'error', Reason}
              when SSH :: connection(),
                   Command :: string(),
                   Data :: string(),
                   Reason :: term().
exec(SSH, Command) ->
    exec(SSH, undefined, Command, ?DEFAULT_TIMEOUT).

-doc """
Requests server to perform `Command`, (see `exec/4`).

Equivalent to [`exec(SSH, undefined, Command, Timeout)`](`exec/4`) if
called with Command being string.

Equivalent to [`exec(SSH, ChannelId, Command, DefaultTimeout)`](`exec/4`) if
called with ChannelId being integer.
""".
-spec exec(SSH, Command, Timeout) -> {'ok', Data} | {'timeout', Data} | {'error', Reason}
              when SSH :: connection(),
                   Command :: string(),
                   Timeout :: timeout(),
                   Data :: string(),
                   Reason :: term();
          (SSH, ChannelId, Command) -> {'ok', Data} | {'timeout', Data} | {'error', Reason}
              when SSH :: connection(),
                   ChannelId :: ssh_channel_id(),
                   Command :: string(),
                   Data :: string(),
                   Reason :: term().
exec(SSH, Command, Timeout) when is_list(Command) ->
    exec(SSH, undefined, Command, Timeout);

exec(SSH, ChannelId, Command) when is_integer(ChannelId) ->
    exec(SSH, ChannelId, Command, ?DEFAULT_TIMEOUT).

-doc """
Requests server to perform `Command`. A previously opened session channel is
used for the request. `Data` is received from the server as a result of the
command.
""".
-spec exec(SSH, ChannelId, Command, Timeout) -> {'ok', Data} | {'timeout', Data} | {'error', Reason}
              when SSH :: connection(),
                   ChannelId :: ssh_channel_id() | 'undefined',
                   Command :: string(),
                   Timeout :: timeout(),
                   Data :: string(),
                   Reason :: term().
exec(SSH, ChannelId, Command, Timeout) ->
    call(SSH, {exec,ChannelId,Command,Timeout}).

-doc(#{equiv => receive_response(SSH, ChannelId, close, DefaultTimeout)}).
-spec receive_response(SSH, ChannelId) -> {'ok', Data} | {'timeout', Data} | {'error', Reason}
              when SSH :: connection(),
                   ChannelId :: ssh_channel_id(),
                   Data :: string(),
                   Reason :: term().
receive_response(SSH, ChannelId) ->
    receive_response(SSH, ChannelId, close, ?DEFAULT_TIMEOUT).

-doc """
Receives expected data from server on the specified session channel
(see `receive_response/4`).

Equivalent to [`receive_response(SSH, ChannelId, End, DefaultTimeout)`](`receive_response/4`) if
called with End being function.

Equivalent to [`receive_response(SSH, ChannelId, close, Timeout)`](`receive_response/4`) if
called with Timeout being integer.
""".
-spec receive_response(SSH, ChannelId, End) -> {'ok', Data} | {'timeout', Data} | {'error', Reason}
              when SSH :: connection(),
                   ChannelId :: ssh_channel_id(),
                   End :: fun((string()) -> boolean()),
                   Data :: string(),
                   Reason :: term();
                      (SSH, ChannelId, Timeout) -> {'ok', Data} | {'timeout', Data}
              | {'error', Reason}
              when SSH :: connection(),
                   ChannelId :: ssh_channel_id(),
                   Timeout :: timeout(),
                   Data :: string(),
                   Reason :: term().
receive_response(SSH, ChannelId, End) when is_function(End) ->
    receive_response(SSH, ChannelId, End, ?DEFAULT_TIMEOUT);

receive_response(SSH, ChannelId, Timeout) when is_integer(Timeout) ->
    receive_response(SSH, ChannelId, close, Timeout).

-doc """
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
-spec receive_response(SSH, ChannelId, End, Timeout) -> {'ok', Data} | {'timeout', Data}
              | {'error', Reason}
              when SSH :: connection(),
                   ChannelId :: ssh_channel_id(),
                   End :: 'close' | 'timeout' | fun((string()) -> boolean()),
                   Timeout :: timeout(),
                   Data :: string(),
                   Reason :: term().
receive_response(SSH, ChannelId, End, Timeout) ->
    call(SSH, {receive_response,ChannelId,End,Timeout}).

-doc(#{equiv => send(SSH, ChannelId, 0, Data, DefaultTimeout)}).
-spec send(SSH, ChannelId, Data) -> 'ok' | {'error', Reason}
              when SSH :: connection(),
                   ChannelId :: ssh_channel_id(),
                   Data :: iodata(),
                   Reason :: term().
send(SSH, ChannelId, Data) ->
    send(SSH, ChannelId, 0, Data, ?DEFAULT_TIMEOUT).

-doc """
Sends data to server on specified session channel (see `send/5`).

Equivalent to [`send(SSH, ChannelId, 0, Data, Timeout)`](`send/5`) if
called with Timeout being integer.

Equivalent to [`send(SSH, ChannelId, Type, Data, DefaultTimeout)`](`send/5`) if
called with Type being integer.
""".
-spec send(SSH, ChannelId, Data, Timeout) -> 'ok' | {'error', Reason}
              when SSH :: connection(),
                   ChannelId :: ssh_channel_id(),
                   Data :: iodata(),
                   Timeout :: timeout(),
                   Reason :: term();
          (SSH, ChannelId, Type, Data) -> 'ok' | {'error', Reason}
              when SSH :: connection(),
                   ChannelId :: ssh_channel_id(),
                   Type :: ssh_data_type_code(),
                   Data :: iodata(),
                   Reason :: term().
send(SSH, ChannelId, Data, Timeout) when is_integer(Timeout) ->
    send(SSH, ChannelId, 0, Data, Timeout);

send(SSH, ChannelId, Type, Data) when is_integer(Type) ->
    send(SSH, ChannelId, Type, Data, ?DEFAULT_TIMEOUT).

-doc """
Sends data to server on specified session channel.
""".
-spec send(SSH, ChannelId, Type, Data, Timeout) -> 'ok' | {'error', Reason}
              when SSH :: connection(),
                   ChannelId :: ssh_channel_id(),
                   Type :: ssh_data_type_code(),
                   Data :: iodata(),
                   Timeout :: timeout(),
                   Reason :: term().
send(SSH, ChannelId, Type, Data, Timeout) ->
    call(SSH, {send,ChannelId,Type,Data,Timeout}).

-doc(#{equiv => send_and_receive(SSH, ChannelId, 0, Data, close, DefaultTimeout)}).
-spec send_and_receive(SSH, ChannelId, Data) -> {'ok', ReceivedData}
              | {'timeout', ReceivedData} | {'error', Reason}
              when SSH :: connection(),
                   ChannelId :: ssh_channel_id(),
                   Data :: iodata(),
                   ReceivedData :: string(),
                   Reason :: term().
send_and_receive(SSH, ChannelId, Data) ->
    send_and_receive(SSH, ChannelId, 0, Data, close, ?DEFAULT_TIMEOUT).

-doc """
Sends data to server on specified session channel and waits to receive the
server response (see `send_and_receive/6`).

Equivalent to
[`send_and_receive(SSH, ChannelId, 0, Data, End, DefaultTimeout)`](`send_and_receive/6`)
if called with End being function.

Equivalent to
[`send_and_receive(SSH, ChannelId, 0, Data, close, Timeout)`](`send_and_receive/6`)
if called with Timeout being integer.

Equivalent to
[`send_and_receive(SSH, ChannelId, Type, Data, close, DefaultTimeout)`](`send_and_receive/6`)
if called with Type being integer.
""".
-spec send_and_receive(SSH, ChannelId, Data, End) -> {'ok', ReceivedData}
              | {'timeout', ReceivedData} | {'error', Reason}
              when SSH :: connection(),
                   ChannelId :: ssh_channel_id(),
                   Data :: iodata(),
                   End :: 'close' | 'timeout' | fun((string()) -> boolean()),
                   ReceivedData :: string(),
                   Reason :: term();
                      (SSH, ChannelId, Data, Timeout) -> {'ok', ReceivedData}
              | {'timeout', ReceivedData} | {'error', Reason}
              when SSH :: connection(),
                   ChannelId :: ssh_channel_id(),
                   Data :: iodata(),
                   Timeout :: timeout(),
                   ReceivedData :: string(),
                   Reason :: term();
                      (SSH, ChannelId, Type, Data) -> {'ok', ReceivedData}
              | {'timeout', ReceivedData} | {'error', Reason}
              when SSH :: connection(),
                   ChannelId :: ssh_channel_id(),
                   Type :: ssh_data_type_code(),
                   Data :: iodata(),
                   ReceivedData :: string(),
                   Reason :: term().
send_and_receive(SSH, ChannelId, Data, End) when is_function(End) ->
    send_and_receive(SSH, ChannelId, 0, Data, End, ?DEFAULT_TIMEOUT);

send_and_receive(SSH, ChannelId, Data, Timeout) when is_integer(Timeout) ->
    send_and_receive(SSH, ChannelId, 0, Data, close, Timeout);

send_and_receive(SSH, ChannelId, Type, Data) when is_integer(Type) ->
    send_and_receive(SSH, ChannelId, Type, Data, close, ?DEFAULT_TIMEOUT).

-doc """
Sends data to server on specified session channel and waits to receive the
server response (see `send_and_receive/6`).

Equivalent to
[`send_and_receive(SSH, ChannelId, 0, Data, End, Timeout)`](`send_and_receive/6`) if
called with Timeout being integer.

Equivalent to
[`send_and_receive(SSH, ChannelId, Type, Data, close, Timeout)`](`send_and_receive/6`) if
called with Type being integer.

Equivalent to
[`send_and_receive(SSH, ChannelId, Type, Data, End, DefaultTimeout)`](`send_and_receive/6`) if
called with End being function.
""".
-spec send_and_receive(SSH, ChannelId, Data, End, Timeout) -> {'ok', ReceivedData}
              | {'timeout', Data} | {'error', Reason}
              when SSH :: connection(),
                   ChannelId :: ssh_channel_id(),
                   Data :: iodata(),
                   End :: 'close' | 'timeout' | fun((string()) -> boolean()),
                   Timeout :: timeout(),
                   ReceivedData :: string(),
                   Reason :: term();
                      (SSH, ChannelId, Type, Data, Timeout) -> {'ok', ReceivedData}
              | {'timeout', ReceivedData} | {'error', Reason}
              when SSH :: connection(),
                   ChannelId :: ssh_channel_id(),
                   Type :: ssh_data_type_code(),
                   Data :: iodata(),
                   Timeout :: timeout(),
                   ReceivedData :: string(),
                   Reason :: term();
                      (SSH, ChannelId, Type, Data, End) -> {'ok', ReceivedData}
              | {'timeout', ReceivedData} | {'error', Reason}
              when SSH :: connection(),
                   ChannelId :: ssh_channel_id(),
                   Type :: ssh_data_type_code(),
                   Data :: iodata(),
                   End :: 'close' | 'timeout' | fun((string()) -> boolean()),
                   ReceivedData :: string(),
                   Reason :: term().
send_and_receive(SSH, ChannelId, Data, End, Timeout) when is_integer(Timeout) ->
    send_and_receive(SSH, ChannelId, 0, Data, End, Timeout);

send_and_receive(SSH, ChannelId, Type, Data, Timeout) when is_integer(Type) ->
    send_and_receive(SSH, ChannelId, Type, Data, close, Timeout);

send_and_receive(SSH, ChannelId, Type, Data, End) when is_function(End) ->
    send_and_receive(SSH, ChannelId, Type, Data, End, ?DEFAULT_TIMEOUT).

-doc """
Sends data to server on specified session channel and waits to receive the
server response.

For details on argument `End`, see
[`ct_ssh:receive_response/4`](`receive_response/4`).
""".
-spec send_and_receive(SSH, ChannelId, Type, Data, End, Timeout) -> {'ok', ReceivedData} |
          {'timeout', ReceivedData} | {'error', Reason}
              when  SSH :: connection(),
                    ChannelId :: ssh_channel_id(),
                    Type :: ssh_data_type_code(),
                    Data :: iodata(),
                    End :: 'close' | 'timeout' | fun((string()) -> boolean()),
                    Timeout :: timeout(),
                    ReceivedData :: string(),
                    Reason :: term().
send_and_receive(SSH, ChannelId, Type, Data, End, Timeout) ->
    call(SSH, {send_and_receive,ChannelId,Type,Data,End,Timeout}).

-doc(#{equiv => subsystem(SSH, Channel, Subsystem, DefaultTimeout)}).
-spec subsystem(SSH, ChannelId, Subsystem) -> Status | {'error', Reason} when
      SSH :: connection(),
      ChannelId :: ssh_channel_id(),
      Subsystem :: string(),
      Status :: 'success' | 'failure',
      Reason :: term().
subsystem(SSH, ChannelId, Subsystem) ->
    subsystem(SSH, ChannelId, Subsystem, ?DEFAULT_TIMEOUT).

-doc """
Sends a request to execute a predefined subsystem.
""".
-spec subsystem(SSH, ChannelId, Subsystem, Timeout) -> Status | {'error', Reason} when
      SSH :: connection(),
      ChannelId :: ssh_channel_id(),
      Subsystem :: string(),
      Timeout :: timeout(),
      Status :: 'success' | 'failure',
      Reason :: term().
subsystem(SSH, ChannelId, Subsystem, Timeout) ->
    call(SSH, {subsystem,ChannelId,Subsystem,Timeout}).


-doc(#{equiv => shell(SSH, ChannelId, DefaultTimeout), since => <<"OTP 20.0">>}).
-spec shell(SSH, ChannelId) -> Result when
      SSH :: connection(),
      ChannelId :: ssh:ssh_channel_id(),
      Result :: ok | {error, term()}.
shell(SSH, ChannelId) ->
    shell(SSH, ChannelId, ?DEFAULT_TIMEOUT).

-doc """
Requests that the user's default shell (typically defined in `/etc/passwd` in Unix
systems) is executed at the server end.
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec shell(SSH, ChannelId, Timeout) -> Result when
      SSH :: connection(),
      ChannelId :: ssh:ssh_channel_id(),
      Timeout :: timeout(),
      Result :: ok | {error, term()}.
shell(SSH, ChannelId, Timeout) ->
    call(SSH, {shell,ChannelId,Timeout}).


%%%-----------------------------------------------------------------
%%%------------------------ SFTP COMMANDS --------------------------

-doc """
Starts an SFTP session on an already existing SSH connection. `Server`
identifies the new session and must be specified whenever SFTP requests are to
be sent.
""".
-spec sftp_connect(SSH) -> {'ok', Server} | {'error', Reason} when
      SSH :: connection(),
      Server :: pid(),
      Reason :: term().
sftp_connect(SSH) ->
    call(SSH, sftp_connect).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec read_file(SSH, File) -> Result when
      SSH :: connection(),
      File :: file:filename(),
      Result :: {'ok', Data} | {'error', Reason},
      Data :: binary(),
      Reason :: term().
read_file(SSH, File) ->
    call(SSH, {read_file,sftp,File}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec read_file(SSH, Server, File) -> Result when
      SSH :: connection(),
      Server :: pid(),
      File :: file:filename(),
      Result :: {'ok', Data} | {'error', Reason},
      Data :: binary(),
      Reason :: term().
read_file(SSH, Server, File) ->
    call(SSH, {read_file,Server,File}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec write_file(SSH, File, Iolist) -> Result when
      SSH :: connection(),
      File :: file:filename(),
      Iolist :: iodata(),
      Result :: 'ok' | {'error', Reason},
      Reason :: term().
write_file(SSH, File, Iolist) ->
    call(SSH, {write_file,sftp,File,Iolist}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec write_file(SSH, Server, File, Iolist) -> Result when
      SSH :: connection(),
      Server :: pid(),
      File :: file:filename(),
      Iolist :: iodata(),
      Result :: 'ok' | {'error', Reason},
      Reason :: term().
write_file(SSH, Server, File, Iolist) ->
    call(SSH, {write_file,Server,File,Iolist}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec list_dir(SSH, Path) -> Result when
      SSH :: connection(),
      Path :: file:filename(),
      Result :: {'ok', FileNames} | {'error', Reason},
      FileNames :: [FileName],
      FileName :: file:filename(),
      Reason :: term().
list_dir(SSH, Path) ->
    call(SSH, {list_dir,sftp,Path}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec list_dir(SSH, Server, Path) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Path :: file:filename(),
      Result :: {'ok', FileNames} | {'error', Reason},
      FileNames :: [FileName],
      FileName :: file:filename(),
      Reason :: term().
list_dir(SSH, Server, Path) ->
    call(SSH, {list_dir,Server,Path}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec open(SSH, File, Mode) -> Result when
      SSH :: connection(),
      File :: file:filename(),
      Mode :: ['read' | 'write' | 'append' | 'binary' | 'raw'],
      Result :: {'ok', Handle} | {'error', Reason},
      Handle :: term(),
      Reason :: term().
open(SSH, File, Mode) ->
    call(SSH, {open,sftp,File,Mode}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec open(SSH, Server, File, Mode) -> Result when
      SSH :: connection(),
      Server :: pid(),
      File :: file:filename(),
      Mode :: ['read' | 'write' | 'append' | 'binary' | 'raw'],
      Result :: {'ok', Handle} | {'error', Reason},
      Handle :: term(),
      Reason :: term().
open(SSH, Server, File, Mode) ->
    call(SSH, {open,Server,File,Mode}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec opendir(SSH, Path) -> Result when
      SSH :: connection(),
      Path :: file:filename(),
      Result :: {'ok', Handle} | {'error', Reason},
      Handle :: term(),
      Reason :: term().
opendir(SSH, Path) ->
    call(SSH, {opendir,sftp,Path}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec opendir(SSH, Server, Path) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Path :: file:filename(),
      Result :: {'ok', Handle} | {'error', Reason},
      Handle :: term(),
      Reason :: term().
opendir(SSH, Server, Path) ->
    call(SSH, {opendir,Server,Path}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec close(SSH, Handle) -> Result when
      SSH :: connection(),
      Handle :: term(),
      Result :: 'ok' | {'error', Reason},
      Reason :: term().
close(SSH, Handle) ->
    call(SSH, {close,sftp,Handle}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec close(SSH, Server, Handle) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Handle :: term(),
      Result :: 'ok' | {'error', Reason},
      Reason :: term().
close(SSH, Server, Handle) ->
    call(SSH, {close,Server,Handle}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec read(SSH, Handle, Len) -> Result when
      SSH :: connection(),
      Handle :: term(),
      Len :: integer(),
      Result :: {'ok', Data} | 'eof' | {'error', Reason},
      Data :: string() | binary(),
      Reason :: term().
read(SSH, Handle, Len) ->
    call(SSH, {read,sftp,Handle,Len}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec read(SSH, Server, Handle, Len) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Handle :: term(),
      Len :: integer(),
      Result :: {'ok', Data} | 'eof' | {'error', Reason},
      Data :: string() | binary(),
      Reason :: term().
read(SSH, Server, Handle, Len) ->
    call(SSH, {read,Server,Handle,Len}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec pread(SSH, Handle, Position, Length) -> Result when
      SSH :: connection(),
      Handle :: term(),
      Position :: integer(),
      Length :: integer(),
      Result :: {'ok', Data} | 'eof' | {'error', Reason},
      Data :: string() | binary(),
      Reason :: term().
pread(SSH, Handle, Position, Length) ->
    call(SSH, {pread,sftp,Handle,Position,Length}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec pread(SSH, Server, Handle, Position, Length) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Handle :: term(),
      Position :: integer(),
      Length :: integer(),
      Result :: {'ok', Data} | 'eof' | {'error', Reason},
      Data :: string() | binary(),
      Reason :: term().
pread(SSH, Server, Handle, Position, Length) ->
    call(SSH, {pread,Server,Handle,Position,Length}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec aread(SSH, Handle, Len) -> Result when
      SSH :: connection(),
      Handle :: term(),
      Len :: integer(),
      Result :: {'async', N} | {'error', Reason},
      N :: term(),
      Reason :: term().
aread(SSH, Handle, Len) ->
    call(SSH, {aread,sftp,Handle,Len}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec aread(SSH, Server, Handle, Len) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Handle :: term(),
      Len :: integer(),
      Result :: {'async', N} | {'error', Reason},
      N :: term(),
      Reason :: term().
aread(SSH, Server, Handle, Len) ->
    call(SSH, {aread,Server,Handle,Len}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec apread(SSH, Handle, Position, Length) -> Result when
      SSH :: connection(),
      Handle :: term(),
      Position :: integer(),
      Length :: integer(),
      Result :: {'async', N} | {'error', Reason},
      N :: term(),
      Reason :: term().
apread(SSH, Handle, Position, Length) ->
    call(SSH, {apread,sftp,Handle,Position,Length}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec apread(SSH, Server, Handle, Position, Length) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Handle :: term(),
      Position :: integer(),
      Length :: integer(),
      Result :: {'async', N} | {'error', Reason},
      N :: term(),
      Reason :: term().
apread(SSH, Server, Handle, Position, Length) ->
    call(SSH, {apread,Server,Handle,Position,Length}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec write(SSH, Handle, Data) -> Result when
      SSH :: connection(),
      Handle :: term(),
      Data :: iodata(),
      Result :: 'ok' | {'error', Reason},
      Reason :: term().
write(SSH, Handle, Data) ->
    call(SSH, {write,sftp,Handle,Data}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec write(SSH, Server, Handle, Data) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Handle :: term(),
      Data :: iodata(),
      Result :: 'ok' | {'error', Reason},
      Reason :: term().
write(SSH, Server, Handle, Data) ->
    call(SSH, {write,Server,Handle,Data}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec pwrite(SSH, Handle, Position, Data) -> Result when
      SSH :: connection(),
      Handle :: term(),
      Position :: integer(),
      Data :: iolist(),
      Result :: 'ok' | {'error', Reason},
      Reason :: term().
pwrite(SSH, Handle, Position, Data) ->
    call(SSH, {pwrite,sftp,Handle,Position,Data}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec pwrite(SSH, Server, Handle, Position, Data) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Handle :: term(),
      Position :: integer(),
      Data :: iolist(),
      Result :: 'ok' | {'error', Reason},
      Reason :: term().
pwrite(SSH, Server, Handle, Position, Data) ->
    call(SSH, {pwrite,Server,Handle,Position,Data}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec awrite(SSH, Handle, Data) -> Result when
      SSH :: connection(),
      Handle :: term(),
      Data :: binary(),
      Result :: {'async', N} | {'error', Reason},
      N :: term(),
      Reason :: term().
awrite(SSH, Handle, Data) ->
    call(SSH, {awrite,sftp,Handle, Data}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec awrite(SSH, Server, Handle, Data) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Handle :: term(),
      Data :: binary(),
      Result :: {'async', N} | {'error', Reason},
      N :: term(),
      Reason :: term().
awrite(SSH, Server, Handle, Data) ->
    call(SSH, {awrite,Server,Handle, Data}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec apwrite(SSH, Handle, Position, Data) -> Result when
      SSH :: connection(),
      Handle :: term(),
      Position :: integer(),
      Data :: binary(),
      Result :: {'async', N} | {'error', Reason},
      N :: term(),
      Reason :: term().
apwrite(SSH, Handle, Position, Data) ->
    call(SSH, {apwrite,sftp,Handle,Position,Data}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec apwrite(SSH, Server, Handle, Position, Data) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Handle :: term(),
      Position :: integer(),
      Data :: binary(),
      Result :: {'async', N} | {'error', Reason},
      N :: term(),
      Reason :: term().
apwrite(SSH, Server, Handle, Position, Data) ->
    call(SSH, {apwrite,Server,Handle,Position,Data}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec position(SSH, Handle, Location) -> Result when
      SSH :: connection(),
      Handle :: term(),
      Location :: Offset | {'bof', Offset} | {'cur', Offset} | {'eof', Offset} | 'bof' | 'cur' |
                  'eof',
      Offset :: integer(),
      Result :: {'ok', NewPosition} | {'error', Reason},
      NewPosition :: integer(),
      Reason :: term().
position(SSH, Handle, Location) ->
    call(SSH, {position,sftp,Handle,Location}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec position(SSH, Server, Handle, Location) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Handle :: term(),
      Location :: Offset | {'bof', Offset} | {'cur', Offset} | {'eof', Offset} | 'bof' | 'cur' |
                  'eof',
      Offset :: integer(),
      Result :: {'ok', NewPosition} | {'error', Reason},
      NewPosition :: integer(),
      Reason :: term().
position(SSH, Server, Handle, Location) ->
    call(SSH, {position,Server,Handle,Location}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec read_file_info(SSH, Name) -> Result when
      SSH :: connection(),
      Name :: file:filename(),
      Result :: {'ok', FileInfo} | {'error', Reason},
      FileInfo :: file:file_info(),
      Reason :: term().
read_file_info(SSH, Name) ->
    call(SSH, {read_file_info,sftp,Name}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec read_file_info(SSH, Server, Name) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Name :: file:filename(),
      Result :: {'ok', FileInfo} | {'error', Reason},
      FileInfo :: file:file_info(),
      Reason :: term().
read_file_info(SSH, Server, Name) ->
    call(SSH, {read_file_info,Server,Name}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec get_file_info(SSH, Handle) -> Result when
      SSH :: connection(),
      Handle :: term(),
      Result :: {'ok', FileInfo} | {'error', Reason},
      FileInfo :: file:file_info(),
      Reason :: term().
get_file_info(SSH, Handle) ->
    call(SSH, {get_file_info,sftp,Handle}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec get_file_info(SSH, Server, Handle) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Handle :: term(),
      Result :: {'ok', FileInfo} | {'error', Reason},
      FileInfo :: file:file_info(),
      Reason :: term().
get_file_info(SSH, Server, Handle) ->
    call(SSH, {get_file_info,Server,Handle}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec read_link_info(SSH, Name) -> Result when
      SSH :: connection(),
      Name :: string(),
      Result :: {'ok', FileInfo} | {'error', Reason},
      FileInfo :: file:file_info(),
      Reason :: term().
read_link_info(SSH, Name) ->
    call(SSH, {read_link_info,sftp,Name}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec read_link_info(SSH, Server, Name) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Name :: file:filename(),
      Result :: {'ok', FileInfo} | {'error', Reason},
      FileInfo :: file:file_info(),
      Reason :: term().
read_link_info(SSH, Server, Name) ->
    call(SSH, {read_link_info,Server,Name}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec write_file_info(SSH, Name, Info) -> Result when
      SSH :: connection(),
      Name :: file:filename(),
      Info :: file:file_info(),
      Result :: 'ok' | {'error', Reason},
      Reason :: term().
write_file_info(SSH, Name, Info) ->
    call(SSH, {write_file_info,sftp,Name,Info}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec write_file_info(SSH, Server, Name, Info) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Name :: file:filename(),
      Info :: file:file_info(),
      Result :: 'ok' | {'error', Reason},
      Reason :: term().
write_file_info(SSH, Server, Name, Info) ->
    call(SSH, {write_file_info,Server,Name,Info}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec read_link(SSH, Name) -> Result when
      SSH :: connection(),
      Name :: file:filename(),
      Result :: {'ok', Target} | {'error', Reason},
      Target :: file:filename(),
      Reason :: term().
read_link(SSH, Name) ->
    call(SSH, {read_link,sftp,Name}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec read_link(SSH, Server, Name) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Name :: file:filename(),
      Result :: {'ok', Target} | {'error', Reason},
      Target :: file:filename(),
      Reason :: term().
read_link(SSH, Server, Name) ->
    call(SSH, {read_link,Server,Name}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec make_symlink(SSH, Name, Target) -> Result when
      SSH :: connection(),
      Name :: file:filename(),
      Target :: file:filename(),
      Result :: 'ok' | {'error', Reason},
      Reason :: term().
make_symlink(SSH, Name, Target) ->
    call(SSH, {make_symlink,sftp,Name,Target}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec make_symlink(SSH, Server, Name, Target) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Name :: file:filename(),
      Target :: file:filename(),
      Result :: 'ok' | {'error', Reason},
      Reason :: term().
make_symlink(SSH, Server, Name, Target) ->
    call(SSH, {make_symlink,Server,Name,Target}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec rename(SSH, OldName, NewName) -> Result when
      SSH :: connection(),
      OldName :: file:filename(),
      NewName :: file:filename(),
      Result :: 'ok' | {'error', Reason},
      Reason :: term().
rename(SSH, OldName, NewName) ->
    call(SSH, {rename,sftp,OldName,NewName}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec rename(SSH, Server, OldName, NewName) -> Result when
      SSH :: connection(),
      Server :: pid(),
      OldName :: file:filename(),
      NewName :: file:filename(),
      Result :: 'ok' | {'error', Reason},
      Reason :: term().
rename(SSH, Server, OldName, NewName) ->
    call(SSH, {rename,Server,OldName,NewName}).


-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec delete(SSH, Name) -> Result when
      SSH :: connection(),
      Name :: file:filename(),
      Result :: 'ok' | {'error', Reason},
      Reason :: term().
delete(SSH, Name) ->
    call(SSH, {delete,sftp,Name}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec delete(SSH, Server, Name) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Name :: file:filename(),
      Result :: 'ok' | {'error', Reason},
      Reason :: term().
delete(SSH, Server, Name) ->
    call(SSH, {delete,Server,Name}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec make_dir(SSH, Name) -> Result when
      SSH :: connection(),
      Name :: file:filename(),
      Result :: 'ok' | {'error', Reason},
      Reason :: term().
make_dir(SSH, Name) ->
    call(SSH, {make_dir,sftp,Name}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec make_dir(SSH, Server, Name) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Name :: file:filename(),
      Result :: 'ok' | {'error', Reason},
      Reason :: term().
make_dir(SSH, Server, Name) ->
    call(SSH, {make_dir,Server,Name}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec del_dir(SSH, Name) -> Result when
      SSH :: connection(),
      Name :: file:filename(),
      Result :: 'ok' | {'error', Reason},
      Reason :: term().
del_dir(SSH, Name) ->
    call(SSH, {del_dir,sftp,Name}).

-doc """
For information and other types, see `m:ssh_sftp`.
""".
-spec del_dir(SSH, Server, Name) -> Result when
      SSH :: connection(),
      Server :: pid(),
      Name :: file:filename(),
      Result :: 'ok' | {'error', Reason},
      Reason :: term().
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
    _ = application:start(crypto),
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

-dialyzer({no_opaque_union, [handle_msg/2]}).
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
