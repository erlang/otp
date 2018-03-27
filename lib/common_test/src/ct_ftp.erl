%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2017. All Rights Reserved.
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

%%% @doc FTP client module (based on the FTP application).
%%%
%%% @type connection() = handle() | ct:target_name()
%%% @type handle() = ct_gen_conn:handle(). Handle for a specific
%%% ftp connection.

-module(ct_ftp).

%% API
-export([get/3,put/3, open/1,close/1, send/2,send/3, 
	 recv/2,recv/3, cd/2, ls/2, type/2, delete/2]).

%% Callbacks
-export([init/3,handle_msg/2,reconnect/2,terminate/2]).

-include("ct_util.hrl").

-record(state,{ftp_pid,target_name}).

-define(DEFAULT_PORT,21).

%%%=================================================================
%%% API

%%%-----------------------------------------------------------------
%%% @spec put(KeyOrName,LocalFile,RemoteFile) -> ok | {error,Reason}
%%%      KeyOrName = Key | Name
%%%      Key = atom()
%%%      Name = ct:target_name()
%%%      LocalFile = string()
%%%      RemoteFile = string()
%%%
%%% @doc Open a ftp connection and send a file to the remote host.
%%%
%%% <p><code>LocalFile</code> and <code>RemoteFile</code> must be
%%% absolute paths.</p>
%%%
%%% <p>If the target host is a "special" node, the ftp address must be
%%% specified in the config file like this:</p>
%%% <pre>
%%% {node,[{ftp,IpAddr}]}.</pre>
%%%
%%% <p>If the target host is something else, e.g. a unix host, the
%%% config file must also include the username and password (both
%%% strings):</p>
%%% <pre>
%%% {unix,[{ftp,IpAddr},
%%%        {username,Username},
%%%        {password,Password}]}.</pre>
%%% @see ct:require/2
put(KeyOrName,LocalFile,RemoteFile) ->
    Fun = fun(Ftp) -> send(Ftp,LocalFile,RemoteFile) end,
    open_and_do(KeyOrName,Fun).

%%%-----------------------------------------------------------------
%%% @spec get(KeyOrName,RemoteFile,LocalFile) -> ok | {error,Reason}
%%%      KeyOrName = Key | Name
%%%      Key = atom()
%%%      Name = ct:target_name()
%%%      RemoteFile = string()
%%%      LocalFile = string()
%%%
%%% @doc Open a ftp connection and fetch a file from the remote host.
%%%
%%% <p><code>RemoteFile</code> and <code>LocalFile</code> must be
%%% absolute paths.</p>
%%%
%%% <p>The config file must be as for put/3.</p>
%%% @see put/3
%%% @see ct:require/2
get(KeyOrName,RemoteFile,LocalFile) ->
    Fun = fun(Ftp) -> recv(Ftp,RemoteFile,LocalFile) end,
    open_and_do(KeyOrName,Fun).


%%%-----------------------------------------------------------------
%%% @spec open(KeyOrName) -> {ok,Handle} | {error,Reason}
%%%      KeyOrName = Key | Name
%%%      Key = atom()
%%%      Name = ct:target_name()
%%%      Handle = handle()
%%% 
%%% @doc Open an FTP connection to the specified node.
%%% <p>You can open one connection for a particular <code>Name</code> and
%%% use the same name as reference for all subsequent operations. If you
%%% want the connection to be associated with <code>Handle</code> instead 
%%% (in case you need to open multiple connections to a host for example), 
%%% simply use <code>Key</code>, the configuration variable name, to 
%%% specify the target. Note that a connection that has no associated target 
%%% name can only be closed with the handle value.</p>
%%%
%%% <p>See <c>ct:require/2</c> for how to create a new <c>Name</c></p>
%%%
%%% @see ct:require/2
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


%%%-----------------------------------------------------------------
%%% @spec send(Connection,LocalFile) -> ok | {error,Reason}
%%%
%%% @doc Send a file over FTP.
%%% <p>The file will get the same name on the remote host.</p>
%%% @see send/3
send(Connection,LocalFile) ->
    send(Connection,LocalFile,filename:basename(LocalFile)).

%%%-----------------------------------------------------------------
%%% @spec send(Connection,LocalFile,RemoteFile) -> ok | {error,Reason}
%%%      Connection = connection()
%%%      LocalFile = string()
%%%      RemoteFile = string()
%%%
%%% @doc Send a file over FTP.
%%%
%%% <p>The file will be named <code>RemoteFile</code> on the remote host.</p>
send(Connection,LocalFile,RemoteFile) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{send,LocalFile,RemoteFile});
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% @spec recv(Connection,RemoteFile) -> ok | {error,Reason}
%%%
%%% @doc Fetch a file over FTP.
%%% <p>The file will get the same name on the local host.</p>
%%% @see recv/3
recv(Connection,RemoteFile) ->
    recv(Connection,RemoteFile,filename:basename(RemoteFile)).

%%%-----------------------------------------------------------------
%%% @spec recv(Connection,RemoteFile,LocalFile) -> ok | {error,Reason}
%%%      Connection = connection()
%%%      RemoteFile = string()
%%%      LocalFile = string()
%%%
%%% @doc Fetch a file over FTP.
%%%
%%% <p>The file will be named <code>LocalFile</code> on the local host.</p>
recv(Connection,RemoteFile,LocalFile) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{recv,RemoteFile,LocalFile});
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% @spec cd(Connection,Dir) -> ok | {error,Reason}
%%%      Connection = connection()
%%%      Dir = string()
%%%
%%% @doc Change directory on remote host.
cd(Connection,Dir) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{cd,Dir});
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% @spec ls(Connection,Dir) -> {ok,Listing} | {error,Reason}
%%%      Connection = connection()
%%%      Dir = string()
%%%      Listing = string()
%%%
%%% @doc List the directory Dir.
ls(Connection,Dir) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{ls,Dir});
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% @spec type(Connection,Type) -> ok | {error,Reason}
%%%      Connection = connection()
%%%      Type = ascii | binary
%%%
%%% @doc Change file transfer type
type(Connection,Type) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{type,Type});
	Error ->
	    Error
    end.
    
%%%-----------------------------------------------------------------
%%% @spec delete(Connection,File) -> ok | {error,Reason}
%%%      Connection = connection()
%%%      File = string()
%%%
%%% @doc Delete a file on remote host
delete(Connection,File) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    call(Pid,{delete,File});
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% @spec close(Connection) -> ok | {error,Reason}
%%%      Connection = connection()
%%%
%%% @doc Close the FTP connection.
close(Connection) ->
    case get_handle(Connection) of
	{ok,Pid} ->
	    ct_gen_conn:stop(Pid);
	Error ->
	    Error
    end.


%%%=================================================================
%%% Callback functions

%% @hidden
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
    case ftp:start_service([{host,IP},{port,Port}]) of
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

%% @hidden
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

%% @hidden
reconnect(_Addr,_State) ->
    {error,no_reconnection_of_ftp}.

%% @hidden
terminate(FtpPid,State) ->
    log(heading(terminate,State#state.target_name),
	"Closing FTP connection.\nHandle: ~p\n",[FtpPid]),
    ftp:stop_service(FtpPid).


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
    
    
