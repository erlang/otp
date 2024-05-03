%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2024. All Rights Reserved.
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

-module(ct_ftp).
-moduledoc """
FTP client module (based on the FTP application).

FTP client module (based on the `ftp` application).
""".

%% API
-export([get/3,put/3, open/1,close/1, send/2,send/3, 
	 recv/2,recv/3, cd/2, ls/2, type/2, delete/2]).

%% Callbacks
-export([init/3,handle_msg/2,reconnect/2,terminate/2]).

-include("ct_util.hrl").

-record(state,{ftp_pid,target_name}).

-define(DEFAULT_PORT,21).

-doc "For `target_name`, see module `m:ct`.".
-type connection() :: handle() | ct:target_name().
-doc "Handle for a specific FTP connection, see module `m:ct`.".
-type handle() :: ct:handle().
-export_type([connection/0, handle/0]).

%%%=================================================================
%%% API

-doc """
put(KeyOrName, LocalFile, RemoteFile) -> ok | {error, Reason}

Opens an FTP connection and sends a file to the remote host.

`LocalFile` and `RemoteFile` must be absolute paths.

For `target_name`, see module `m:ct`.

If the target host is a "special" node, the FTP address must be specified in the
configuration file as follows:

```erlang
 {node,[{ftp,IpAddr}]}.
```

If the target host is something else, for example, a UNIX host, the
configuration file must also include the username and password (both strings):

```erlang
 {unix,[{ftp,IpAddr},
        {username,Username},
        {password,Password}]}.
```

See also `ct:require/2`.
""".
put(KeyOrName,LocalFile,RemoteFile) ->
    Fun = fun(Ftp) -> send(Ftp,LocalFile,RemoteFile) end,
    open_and_do(KeyOrName,Fun).

-doc """
get(KeyOrName, RemoteFile, LocalFile) -> ok | {error, Reason}

Opens an FTP connection and fetches a file from the remote host.

`RemoteFile` and `LocalFile` must be absolute paths.

The configuration file must be as for [`ct_ftp:put/3`](`put/3`).

For `target_name`, see module `m:ct`.

See also `ct:require/2`.
""".
get(KeyOrName,RemoteFile,LocalFile) ->
    Fun = fun(Ftp) -> recv(Ftp,RemoteFile,LocalFile) end,
    open_and_do(KeyOrName,Fun).

-doc """
open(KeyOrName) -> {ok, Handle} | {error, Reason}

Opens an FTP connection to the specified node.

You can open a connection for a particular `Name` and use the same name as
reference for all following subsequent operations. If you want the connection to
be associated with `Handle` instead (if you, for example, need to open multiple
connections to a host), use `Key`, the configuration variable name, to specify
the target. A connection without an associated target name can only be closed
with the handle value.

For information on how to create a new `Name`, see `ct:require/2`.

For `target_name`, see module `m:ct`.
""".
open(KeyOrName) ->
    case ct_util:get_key_from_name(KeyOrName) of
	{ok,node} ->
	    open(KeyOrName,"erlang","x");
	_ ->
	    case ct:get_config(KeyOrName) of
		undefined ->
		    log(heading(open,KeyOrName),"Failed: ~tp\n",
			[{not_available,KeyOrName}]),
		    {error,{not_available,KeyOrName}};
		_ ->
		    case ct:get_config({KeyOrName,username}) of
			undefined ->
			    log(heading(open,KeyOrName),"Failed: ~tp\n",
				[{not_available,{KeyOrName,username}}]),
			    {error,{not_available,{KeyOrName,username}}};
			Username ->
			    case ct:get_config({KeyOrName,password}) of
				undefined ->
				    log(heading(open,KeyOrName),"Failed: ~tp\n",
					[{not_available,{KeyOrName,password}}]),
				    {error,{not_available,{KeyOrName,password}}};
				Password ->
				    open(KeyOrName,Username,Password)
			    end
		    end
	    end
    end.

open(KeyOrName,Username,Password) ->
    log(heading(open,KeyOrName),"",[]),
    case ct:get_config({KeyOrName,ftp}) of
	undefined ->
	    log(heading(open,KeyOrName),"Failed: ~tp\n",
		[{not_available,{KeyOrName,ftp}}]),
	    {error,{not_available,{KeyOrName,ftp}}};
	Addr ->
	    ct_gen_conn:start(KeyOrName,full_addr(Addr),{Username,Password},?MODULE)
    end.

-doc """
send(Connection, LocalFile) -> ok | {error, Reason}

Sends a file over FTP.

The file gets the same name on the remote host.

See also [`ct_ftp:send/3`](`send/3`).
""".
send(Connection,LocalFile) ->
    send(Connection,LocalFile,filename:basename(LocalFile)).

-doc """
send(Connection, LocalFile, RemoteFile) -> ok | {error, Reason}

Sends a file over FTP.

The file is named `RemoteFile` on the remote host.
""".
send(Connection,LocalFile,RemoteFile) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{send,LocalFile,RemoteFile});
	Error ->
	    Error
    end.

-doc """
recv(Connection, RemoteFile) -> ok | {error, Reason}

Fetches a file over FTP.

The file gets the same name on the local host.

See also [`ct_ftp:recv/3`](`recv/3`).
""".
recv(Connection,RemoteFile) ->
    recv(Connection,RemoteFile,filename:basename(RemoteFile)).

-doc """
recv(Connection, RemoteFile, LocalFile) -> ok | {error, Reason}

Fetches a file over FTP.

The file is named `LocalFile` on the local host.
""".
recv(Connection,RemoteFile,LocalFile) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{recv,RemoteFile,LocalFile});
	Error ->
	    Error
    end.

-doc """
cd(Connection, Dir) -> ok | {error, Reason}

Changes directory on remote host.
""".
cd(Connection,Dir) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{cd,Dir});
	Error ->
	    Error
    end.

-doc """
ls(Connection, Dir) -> {ok, Listing} | {error, Reason}

Lists directory `Dir`.
""".
ls(Connection,Dir) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{ls,Dir});
	Error ->
	    Error
    end.

-doc """
type(Connection, Type) -> ok | {error, Reason}

Changes the file transfer type.
""".
type(Connection,Type) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{type,Type});
	Error ->
	    Error
    end.
    
-doc """
delete(Connection, File) -> ok | {error, Reason}

Deletes a file on remote host.
""".
delete(Connection,File) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{delete,File});
	Error ->
	    Error
    end.

-doc """
close(Connection) -> ok | {error, Reason}

Closes the FTP connection.
""".
close(Connection) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    ct_gen_conn:stop(Pid);
	Error ->
	    Error
    end.


%%%=================================================================
%%% Callback functions

-doc false.
init(KeyOrName,{IP,Port},{Username,Password}) ->
    case ftp_connect(IP,Port,Username,Password) of
	{ok,FtpPid} ->
	    log(heading(init,KeyOrName), 
		"Opened ftp connection:\nIP: ~tp\nUsername: ~tp\nPassword: ~p\n",
		[IP,Username,lists:duplicate(string:length(Password),$*)]),
	    {ok,FtpPid,#state{ftp_pid=FtpPid,target_name=KeyOrName}};
	Error ->
	    Error
    end.
	    
ftp_connect(IP,Port,Username,Password) ->
    _ = ftp:start(),
    case ftp:open(IP,[{port,Port}]) of
	{ok,FtpPid} ->
	    case ftp:user(FtpPid,Username,Password) of
		ok ->
		    {ok,FtpPid};
		{error,Reason} ->
		    {error,{user,Reason}}
	    end;
	{error,Reason} ->
	    {error,{open,Reason}}
    end.

-doc false.
handle_msg({send,LocalFile,RemoteFile},State) ->
    log(heading(send,State#state.target_name),
	"LocalFile: ~tp\nRemoteFile: ~tp\n",[LocalFile,RemoteFile]),
    Result = ftp:send(State#state.ftp_pid,LocalFile,RemoteFile),
    {Result,State};
handle_msg({recv,RemoteFile,LocalFile},State) ->
    log(heading(recv,State#state.target_name),
	"RemoteFile: ~tp\nLocalFile: ~tp\n",[RemoteFile,LocalFile]),
    Result = ftp:recv(State#state.ftp_pid,RemoteFile,LocalFile),
    {Result,State};
handle_msg({cd,Dir},State) ->
    log(heading(cd,State#state.target_name),"Dir: ~tp\n",[Dir]),
    Result = ftp:cd(State#state.ftp_pid,Dir),
    {Result,State};
handle_msg({ls,Dir},State) ->
    log(heading(ls,State#state.target_name),"Dir: ~tp\n",[Dir]),
    Result = ftp:ls(State#state.ftp_pid,Dir),
    {Result,State};
handle_msg({type,Type},State) ->
    log(heading(type,State#state.target_name),"Type: ~tp\n",[Type]),
    Result = ftp:type(State#state.ftp_pid,Type),
    {Result,State};
handle_msg({delete,File},State) ->
    log(heading(delete,State#state.target_name),"Delete file: ~tp\n",[File]),
    Result = ftp:delete(State#state.ftp_pid,File),
    {Result,State}.

-doc false.
reconnect(_Addr,_State) ->
    {error,no_reconnection_of_ftp}.

-doc false.
terminate(FtpPid,State) ->
    log(heading(terminate,State#state.target_name),
	"Closing FTP connection.\nHandle: ~p\n",[FtpPid]),
    ftp:close(FtpPid).


%%%=================================================================
%%% Internal function
get_handle(Pid) when is_pid(Pid) ->
    {ok,Pid};
get_handle(Name) ->
    case ct_util:get_connection(Name,?MODULE) of
	{ok,{Pid,_}} ->
	    {ok,Pid};
	{error,no_registered_connection} ->
	    open(Name);
	Error ->
	    Error
    end.

full_addr({Ip,Port}) ->
    {Ip,Port};
full_addr(Ip) ->
    {Ip,?DEFAULT_PORT}.

call(Pid,Msg) ->
    ct_gen_conn:call(Pid,Msg).


heading(Function,Name) ->
    io_lib:format("ct_ftp:~tw ~tp",[Function,Name]).

log(Heading,Str,Args) ->
    ct_gen_conn:log(Heading,Str,Args).


open_and_do(Name,Fun) ->
    case open(Name) of
	{ok,Ftp} ->
	    R = Fun(Ftp),
	    close(Ftp),
	    R;
	Error ->
	    Error
    end.
    
    
