%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2012. All Rights Reserved.
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
%% Description: This module implements an ftp client, RFC 959. 
%% It also supports ipv6 RFC 2428.

-module(ftp).

-behaviour(gen_server).
-behaviour(inets_service).


%%  API - Client interface
-export([cd/2, close/1, delete/2, formaterror/1, 
	 lcd/2, lpwd/1, ls/1, ls/2, 
	 mkdir/2, nlist/1, nlist/2, 
	 open/1, open/2, 
	 pwd/1, quote/2,
	 recv/2, recv/3, recv_bin/2, 
	 recv_chunk_start/2, recv_chunk/1, 
	 rename/3, rmdir/2, 
	 send/2, send/3, send_bin/3, 
	 send_chunk_start/2, send_chunk/2, send_chunk_end/1, 
	 type/2, user/3, user/4, account/2,
	 append/3, append/2, append_bin/3,
	 append_chunk/2, append_chunk_end/1, append_chunk_start/2, info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

%% supervisor callbacks
-export([start_link/1, start_link/2]).

%% Behavior callbacks
-export([start_standalone/1, start_service/1, 
	 stop_service/1, services/0, service_info/1]).

-include("ftp_internal.hrl").

%% Constante used in internal state definition
-define(CONNECTION_TIMEOUT,  60*1000).
-define(DATA_ACCEPT_TIMEOUT, infinity).
-define(DEFAULT_MODE,        passive).
-define(PROGRESS_DEFAULT,    ignore).

%% Internal Constants
-define(FTP_PORT, 21).
-define(FILE_BUFSIZE, 4096).

%% Internal state
-record(state, {
	  csock   = undefined, % socket() - Control connection socket 
	  dsock   = undefined, % socket() - Data connection socket 
	  verbose = false,   % boolean() 
	  ldir    = undefined,  % string() - Current local directory
	  type    = ftp_server_default,  % atom() - binary | ascii 
	  chunk   = false,     % boolean() - Receiving data chunks 
	  mode    = ?DEFAULT_MODE,    % passive | active
	  timeout = ?CONNECTION_TIMEOUT, % integer()
	  %% Data received so far on the data connection
	  data    = <<>>,   % binary()
	  %% Data received so far on the control connection
	  %% {BinStream, AccLines}. If a binary sequence
	  %% ends with ?CR then keep it in the binary to
	  %% be able to detect if the next received byte is ?LF
	  %% and hence the end of the response is reached!
	  ctrl_data = {<<>>, [], start},  % {binary(), [bytes()], LineStatus}
	  %% pid() - Client pid (note not the same as "From")
	  owner = undefined,   
	  client = undefined,  % "From" to be used in gen_server:reply/2
	  %% Function that activated a connection and maybe some
	  %% data needed further on.
	  caller = undefined, % term()     
	  ipfamily,     % inet | inet6 | inet6fb4
	  progress = ignore,   % ignore | pid()	    
	  dtimeout = ?DATA_ACCEPT_TIMEOUT  % non_neg_integer() | infinity
	 }).


-type shortage_reason()  :: 'etnospc' | 'epnospc'.
-type restriction_reason() :: 'epath' | 'efnamena' | 'elogin' | 'enotbinary'.
-type common_reason() ::  'econn' | 'eclosed' | term().
-type file_write_error_reason() :: term(). % See file:write for more info


%%%=========================================================================
%%%  API - CLIENT FUNCTIONS
%%%=========================================================================

%%--------------------------------------------------------------------------
%% open(HostOrOtpList, <Port>, <Flags>) -> {ok, Pid} | {error, ehost}
%%	HostOrOtpList = string() | [{option_list, Options}] 
%%      Port = integer(), 
%%      Flags = [Flag], 
%%      Flag = verbose | debug | trace
%%
%% Description:  Start an ftp client and connect to a host.
%%--------------------------------------------------------------------------

-spec open(Host :: string() | inet:ip_address()) ->
    {'ok', Pid :: pid()} | {'error', Reason :: 'ehost' | term()}.

%% <BACKWARD-COMPATIBILLITY>
open({option_list, Options}) when is_list(Options) ->
    try
	{ok, StartOptions} = start_options(Options),
	{ok, OpenOptions}  = open_options(Options), 
	case ftp_sup:start_child([[[{client, self()} | StartOptions], []]]) of
	    {ok, Pid} ->
		call(Pid, {open, ip_comm, OpenOptions}, plain);
	    Error1 ->
		Error1
	end
    catch 
	throw:Error2 ->
	    Error2
    end;
%% </BACKWARD-COMPATIBILLITY>

open(Host) ->
    open(Host, []).

-spec open(Host :: string() | inet:ip_address(), Opts :: list()) ->
    {'ok', Pid :: pid()} | {'error', Reason :: 'ehost' | term()}.

%% <BACKWARD-COMPATIBILLITY>
open(Host, Port) when is_integer(Port) ->
    open(Host, [{port, Port}]);
%% </BACKWARD-COMPATIBILLITY>

open(Host, Opts) when is_list(Opts) ->
    ?fcrt("open", [{host, Host}, {opts, Opts}]), 
    try
	{ok, StartOptions} = start_options(Opts), 
	?fcrt("open", [{start_options, StartOptions}]), 
	{ok, OpenOptions}  = open_options([{host, Host}|Opts]), 
	?fcrt("open", [{open_options, OpenOptions}]), 
	case start_link(StartOptions, []) of
	    {ok, Pid} ->
		?fcrt("open - ok", [{pid, Pid}]), 
		call(Pid, {open, ip_comm, OpenOptions}, plain);
	    Error1 ->
		?fcrt("open - error", [{error1, Error1}]), 
		Error1
	end
    catch
	throw:Error2 ->
	    ?fcrt("open - error", [{error2, Error2}]), 
	    Error2
    end.


%%--------------------------------------------------------------------------
%% user(Pid, User, Pass, <Acc>) -> ok | {error, euser} | {error, econn} 
%%                                    | {error, eacct}
%%	Pid = pid(), 
%%      User = Pass =  Acc = string()
%%
%% Description:  Login with or without a supplied account name.
%%--------------------------------------------------------------------------
-spec user(Pid  :: pid(), 
	   User :: string(), 
	   Pass :: string()) ->
    'ok' | {'error', Reason :: 'euser' | common_reason()}.

user(Pid, User, Pass) ->
    call(Pid, {user, User, Pass}, atom).

-spec user(Pid  :: pid(), 
	   User :: string(), 
	   Pass :: string(), 
	   Acc  :: string()) ->
    'ok' | {'error', Reason :: 'euser' | common_reason()}.

user(Pid, User, Pass, Acc) ->
    call(Pid, {user, User, Pass, Acc}, atom).


%%--------------------------------------------------------------------------
%% account(Pid, Acc)  -> ok | {error, eacct}
%%	Pid = pid()
%%	Acc= string()
%%
%% Description:  Set a user Account.
%%--------------------------------------------------------------------------

-spec account(Pid :: pid(), Acc :: string()) ->
    'ok' | {'error', Reason :: 'eacct' | common_reason()}.

account(Pid, Acc) ->
    call(Pid, {account, Acc}, atom).


%%--------------------------------------------------------------------------
%% pwd(Pid) -> {ok, Dir} | {error, elogin} | {error, econn} 
%%	Pid = pid()
%%      Dir = string()
%%
%% Description:  Get the current working directory at remote server.
%%--------------------------------------------------------------------------

-spec pwd(Pid :: pid()) ->
    {'ok', Dir :: string()} | 
	{'error', Reason :: restriction_reason() | common_reason()}.

pwd(Pid) ->
    call(Pid, pwd, ctrl).


%%--------------------------------------------------------------------------
%% lpwd(Pid) ->  {ok, Dir} 
%%	Pid = pid()
%%      Dir = string()
%%
%% Description:  Get the current working directory at local server.
%%--------------------------------------------------------------------------

-spec lpwd(Pid :: pid()) ->
    {'ok', Dir :: string()}.

lpwd(Pid) ->
    call(Pid, lpwd, string).


%%--------------------------------------------------------------------------
%% cd(Pid, Dir) ->  ok | {error, epath} | {error, elogin} | {error, econn}
%%	Pid = pid()
%%	Dir = string()
%%
%% Description:  Change current working directory at remote server.
%%--------------------------------------------------------------------------

-spec cd(Pid :: pid(), Dir :: string()) ->
    'ok' | {'error', Reason :: restriction_reason() | common_reason()}.

cd(Pid, Dir) ->
    call(Pid, {cd, Dir}, atom).


%%--------------------------------------------------------------------------
%% lcd(Pid, Dir) ->  ok | {error, epath}
%%	Pid = pid()
%%	Dir = string()
%%
%% Description:  Change current working directory for the local client.
%%--------------------------------------------------------------------------

-spec lcd(Pid :: pid(), Dir :: string()) ->
    'ok' | {'error', Reason :: restriction_reason()}.

lcd(Pid, Dir) ->
    call(Pid, {lcd, Dir}, string).


%%--------------------------------------------------------------------------
%% ls(Pid) -> Result
%% ls(Pid, <Dir>) -> Result
%% 
%%	Pid = pid()
%%	Dir = string()
%%      Result = {ok, Listing} | {error, Reason}
%%      Listing = string()
%%      Reason = epath | elogin | econn
%%
%% Description: Returns a list of files in long format.
%%--------------------------------------------------------------------------

-spec ls(Pid :: pid()) ->
    {'ok', Listing :: string()} | 
	{'error', Reason :: restriction_reason() | common_reason()}.

ls(Pid) ->
  ls(Pid, "").

-spec ls(Pid :: pid(), Dir :: string()) ->
    {'ok', Listing :: string()} | 
	{'error', Reason ::  restriction_reason() | common_reason()}.

ls(Pid, Dir) ->
    call(Pid, {dir, long, Dir}, string).


%%--------------------------------------------------------------------------
%% nlist(Pid) -> Result
%% nlist(Pid, Pathname) -> Result
%% 
%%	Pid = pid()
%%	Pathname = string()
%%      Result = {ok, Listing} | {error, Reason}
%%      Listing = string()
%%      Reason = epath | elogin | econn
%%
%% Description:  Returns a list of files in short format
%%--------------------------------------------------------------------------

-spec nlist(Pid :: pid()) ->
    {'ok', Listing :: string()} | 
	{'error', Reason :: restriction_reason() | common_reason()}.

nlist(Pid) ->
  nlist(Pid, "").

-spec nlist(Pid :: pid(), Pathname :: string()) ->
    {'ok', Listing :: string()} | 
	{'error', Reason :: restriction_reason() | common_reason()}.

nlist(Pid, Dir) ->
    call(Pid, {dir, short, Dir}, string).


%%--------------------------------------------------------------------------
%% rename(Pid, Old, New) ->  ok | {error, epath} | {error, elogin} 
%%                              | {error, econn}
%%	Pid = pid()
%%	CurrFile = NewFile = string()
%%
%% Description:  Rename a file at remote server.
%%--------------------------------------------------------------------------

-spec rename(Pid :: pid(), Old :: string(), New :: string()) ->
    'ok' | {'error', Reason :: restriction_reason() | common_reason()}.

rename(Pid, Old, New) ->
    call(Pid, {rename, Old, New}, string).


%%--------------------------------------------------------------------------
%% delete(Pid, File) ->  ok | {error, epath} | {error, elogin} | 
%%                       {error, econn}
%%	Pid = pid()
%%	File = string()
%%
%% Description:  Remove file at remote server.
%%--------------------------------------------------------------------------

-spec delete(Pid :: pid(), File :: string()) ->
    'ok' | {'error', Reason :: restriction_reason() | common_reason()}.

delete(Pid, File) ->
    call(Pid, {delete, File}, string).


%%--------------------------------------------------------------------------
%% mkdir(Pid, Dir) -> ok | {error, epath} | {error, elogin} | {error, econn}
%%	Pid = pid(), 
%%	Dir = string()
%%
%% Description:  Make directory at remote server.
%%--------------------------------------------------------------------------

-spec mkdir(Pid :: pid(), Dir :: string()) ->
    'ok' | {'error', Reason :: restriction_reason() | common_reason()}.

mkdir(Pid, Dir) ->
    call(Pid, {mkdir, Dir}, atom).


%%--------------------------------------------------------------------------
%% rmdir(Pid, Dir) -> ok | {error, epath} | {error, elogin} | {error, econn}
%%	Pid = pid(), 
%%	Dir = string()
%%
%% Description:  Remove directory at remote server.
%%--------------------------------------------------------------------------

-spec rmdir(Pid :: pid(), Dir :: string()) ->
    'ok' | {'error', Reason :: restriction_reason() | common_reason()}.

rmdir(Pid, Dir) ->
    call(Pid, {rmdir, Dir}, atom).


%%--------------------------------------------------------------------------
%% type(Pid, Type) -> ok | {error, etype} | {error, elogin} | {error, econn}
%%	Pid = pid() 
%%	Type = ascii | binary
%%
%% Description:  Set transfer type.
%%--------------------------------------------------------------------------

-spec type(Pid :: pid(), Type :: ascii | binary) ->
    'ok' | 
	{'error', Reason :: 'etype' | restriction_reason() | common_reason()}.

type(Pid, Type) ->
    call(Pid, {type, Type}, atom).


%%--------------------------------------------------------------------------
%% recv(Pid, RemoteFileName [, LocalFileName]) -> ok | {error, epath} |
%%                                          {error, elogin} | {error, econn}
%%	Pid = pid()
%%	RemoteFileName = LocalFileName = string()
%%
%% Description:  Transfer file from remote server.
%%--------------------------------------------------------------------------

-spec recv(Pid :: pid(), RemoteFileName :: string()) ->
    'ok' | {'error', Reason :: restriction_reason() | 
                               common_reason() | 
                               file_write_error_reason()}.

recv(Pid, RemotFileName) ->
  recv(Pid, RemotFileName, RemotFileName).

-spec recv(Pid            :: pid(), 
	   RemoteFileName :: string(), 
	   LocalFileName  :: string()) ->
    'ok' | {'error', Reason :: term()}.

recv(Pid, RemotFileName, LocalFileName) ->
    call(Pid, {recv, RemotFileName, LocalFileName}, atom).


%%--------------------------------------------------------------------------
%% recv_bin(Pid, RemoteFile) -> {ok, Bin} | {error, epath} | {error, elogin} 
%%			   | {error, econn}
%%	Pid = pid()
%%	RemoteFile = string()
%%      Bin = binary()
%%
%% Description:  Transfer file from remote server into binary.
%%--------------------------------------------------------------------------

-spec recv_bin(Pid        :: pid(), 
	       RemoteFile :: string()) ->
    {'ok', Bin :: binary()} | 
	{'error', Reason :: restriction_reason() | common_reason()}.

recv_bin(Pid, RemoteFile) ->
    call(Pid, {recv_bin, RemoteFile}, bin).


%%--------------------------------------------------------------------------
%% recv_chunk_start(Pid, RemoteFile) -> ok | {error, elogin} | {error, epath} 
%%                                 | {error, econn}
%%	Pid = pid()
%%	RemoteFile = string()
%%
%% Description:  Start receive of chunks of remote file.
%%--------------------------------------------------------------------------

-spec recv_chunk_start(Pid        :: pid(), 
		       RemoteFile :: string()) ->
    'ok' | {'error', Reason :: restriction_reason() | common_reason()}.

recv_chunk_start(Pid, RemoteFile) ->
    call(Pid, {recv_chunk_start, RemoteFile}, atom).


%%--------------------------------------------------------------------------
%% recv_chunk(Pid, RemoteFile) ->  ok | {ok, Bin} | {error, Reason}
%%	Pid = pid()
%%	RemoteFile = string()
%%
%% Description:  Transfer file from remote server into binary in chunks
%%--------------------------------------------------------------------------

-spec recv_chunk(Pid :: pid()) ->
    'ok' | 
	{'ok', Bin :: binary()} | 
	{'error', Reason :: restriction_reason() | common_reason()}.

recv_chunk(Pid) ->
    call(Pid, recv_chunk, atom).


%%--------------------------------------------------------------------------
%% send(Pid, LocalFileName [, RemotFileName]) -> ok | {error, epath} 
%%                                                  | {error, elogin} 
%%                                                  | {error, econn}
%%	Pid = pid()
%%	LocalFileName = RemotFileName = string()
%%
%% Description:  Transfer file to remote server.
%%--------------------------------------------------------------------------

-spec send(Pid :: pid(), LocalFileName :: string()) ->
    'ok' | 
	{'error', Reason :: restriction_reason() | 
                            common_reason() | 
                            shortage_reason()}.

send(Pid, LocalFileName) ->
  send(Pid, LocalFileName, LocalFileName).

-spec send(Pid            :: pid(), 
	   LocalFileName  :: string(), 
	   RemoteFileName :: string()) ->
    'ok' | 
	{'error', Reason :: restriction_reason() | 
                            common_reason() | 
                            shortage_reason()}.

send(Pid, LocalFileName, RemotFileName) ->
    call(Pid, {send, LocalFileName, RemotFileName}, atom).


%%--------------------------------------------------------------------------
%% send_bin(Pid, Bin, RemoteFile) -> ok | {error, epath} | {error, elogin} 
%%                             | {error, enotbinary} | {error, econn}
%%	Pid = pid()
%%	Bin = binary()
%%	RemoteFile = string()
%%
%% Description:  Transfer a binary to a remote file.
%%--------------------------------------------------------------------------

-spec send_bin(Pid :: pid(), Bin :: binary(), RemoteFile :: string()) ->
    'ok' | 
	{'error', Reason :: restriction_reason() | 
                            common_reason() | 
                            shortage_reason()}.

send_bin(Pid, Bin, RemoteFile) when is_binary(Bin) ->
    call(Pid, {send_bin, Bin, RemoteFile}, atom);
send_bin(_Pid, _Bin, _RemoteFile) ->
  {error, enotbinary}.


%%--------------------------------------------------------------------------
%% send_chunk_start(Pid, RemoteFile) -> ok | {error, elogin} | {error, epath} 
%%                                 | {error, econn}
%%	Pid = pid()
%%	RemoteFile = string()
%%
%% Description:  Start transfer of chunks to remote file.
%%--------------------------------------------------------------------------

-spec send_chunk_start(Pid :: pid(), RemoteFile :: string()) ->
    'ok' | {'error', Reason :: restriction_reason() | common_reason()}.

send_chunk_start(Pid, RemoteFile) ->
    call(Pid, {send_chunk_start, RemoteFile}, atom).


%%--------------------------------------------------------------------------
%% append_chunk_start(Pid, RemoteFile) -> ok | {error, elogin} | 
%%                                        {error, epath} | {error, econn}
%%	Pid = pid()
%%	RemoteFile = string()
%%
%% Description:  Start append chunks of data to remote file.
%%--------------------------------------------------------------------------

-spec append_chunk_start(Pid :: pid(), RemoteFile :: string()) ->
    'ok' | {'error', Reason :: term()}.

append_chunk_start(Pid, RemoteFile) ->
    call(Pid, {append_chunk_start, RemoteFile}, atom).


%%--------------------------------------------------------------------------
%% send_chunk(Pid, Bin) -> ok | {error, elogin} | {error, enotbinary} 
%%                       | {error, echunk} | {error, econn}
%%      Pid = pid()
%%	Bin = binary().
%%
%% Purpose:  Send chunk to remote file.
%%--------------------------------------------------------------------------

-spec send_chunk(Pid :: pid(), Bin :: binary()) ->
    'ok' | 
	{'error', Reason :: 'echunk' | 
                            restriction_reason() | 
                            common_reason()}.

send_chunk(Pid, Bin) when is_binary(Bin) ->
    call(Pid, {transfer_chunk, Bin}, atom);
send_chunk(_Pid, _Bin) ->
  {error, enotbinary}.


%%--------------------------------------------------------------------------
%% append_chunk(Pid, Bin) -> ok | {error, elogin} | {error, enotbinary} 
%%			     | {error, echunk} | {error, econn}
%%	Pid = pid()
%%	Bin = binary()
%%
%% Description:  Append chunk to remote file.
%%--------------------------------------------------------------------------

-spec append_chunk(Pid :: pid(), Bin :: binary()) ->
    'ok' | 
	{'error', Reason :: 'echunk' | 
                            restriction_reason() | 
                            common_reason()}.

append_chunk(Pid, Bin) when is_binary(Bin) ->
    call(Pid, {transfer_chunk, Bin}, atom);
append_chunk(_Pid, _Bin) ->
  {error, enotbinary}.


%%--------------------------------------------------------------------------
%% send_chunk_end(Pid) -> ok | {error, elogin} | {error, echunk} 
%%			  | {error, econn}
%%	Pid = pid()
%%
%% Description:  End sending of chunks to remote file.
%%--------------------------------------------------------------------------

-spec send_chunk_end(Pid :: pid()) ->
    'ok' | 
	{'error', Reason :: restriction_reason() | 
                            common_reason() | 
                            shortage_reason()}.

send_chunk_end(Pid) ->
    call(Pid, chunk_end, atom).


%%--------------------------------------------------------------------------
%% append_chunk_end(Pid) ->  ok | {error, elogin} | {error, echunk} 
%%			     | {error, econn}
%%	Pid = pid()
%%
%% Description:  End appending of chunks to remote file.
%%--------------------------------------------------------------------------

-spec append_chunk_end(Pid :: pid()) ->
    'ok' | 
	{'error', Reason :: restriction_reason() | 
                            common_reason() | 
                            shortage_reason()}.

append_chunk_end(Pid) ->
    call(Pid, chunk_end, atom).


%%--------------------------------------------------------------------------
%% append(Pid, LocalFileName [, RemotFileName]) -> ok | {error, epath} 
%%                                                    | {error, elogin} 
%%                                                    | {error, econn}
%%	Pid = pid()
%%	LocalFileName = RemotFileName = string()
%%
%% Description:  Append the local file to the remote file
%%--------------------------------------------------------------------------

-spec append(Pid :: pid(), LocalFileName :: string()) ->
    'ok' | 
	{'error', Reason :: 'epath'    | 
                            'elogin'   | 
                            'etnospc'  | 
                            'epnospc'  | 
                            'efnamena' | common_reason()}.

append(Pid, LocalFileName) ->
    append(Pid, LocalFileName, LocalFileName).

-spec append(Pid            :: pid(), 
	     LocalFileName  :: string(), 
	     RemoteFileName :: string()) ->
    'ok' | {'error', Reason :: term()}.

append(Pid, LocalFileName, RemotFileName) ->
    call(Pid, {append, LocalFileName, RemotFileName}, atom).


%%--------------------------------------------------------------------------
%% append_bin(Pid, Bin, RemoteFile) -> ok | {error, epath} | {error, elogin} 
%%				  | {error, enotbinary} | {error, econn}
%%	Pid = pid()
%%	Bin = binary()
%%	RemoteFile = string()
%%
%% Purpose:  Append a binary to a remote file.
%%--------------------------------------------------------------------------

-spec append_bin(Pid        :: pid(), 
		 Bin        :: binary(), 
		 RemoteFile :: string()) ->
    'ok' | 
	{'error', Reason :: restriction_reason() | 
                            common_reason() | 
                            shortage_reason()}.

append_bin(Pid, Bin, RemoteFile) when is_binary(Bin) ->
    call(Pid, {append_bin, Bin, RemoteFile}, atom);
append_bin(_Pid, _Bin, _RemoteFile) ->
    {error, enotbinary}.


%%--------------------------------------------------------------------------
%% quote(Pid, Cmd) -> list()
%%	Pid = pid()
%%	Cmd = string()
%%
%% Description: Send arbitrary ftp command.
%%--------------------------------------------------------------------------

-spec quote(Pid :: pid(), Cmd :: string()) -> list().

quote(Pid, Cmd) when is_list(Cmd) ->
    call(Pid, {quote, Cmd}, atom).


%%--------------------------------------------------------------------------
%% close(Pid) -> ok
%%	Pid = pid()
%%
%% Description:  End the ftp session.
%%--------------------------------------------------------------------------

-spec close(Pid :: pid()) -> 'ok'.

close(Pid) ->
    cast(Pid, close),
    ok.


%%--------------------------------------------------------------------------
%% formaterror(Tag) -> string()
%%	Tag = atom() | {error, atom()}
%%
%% Description:  Return diagnostics.
%%--------------------------------------------------------------------------

-spec formaterror(Tag :: term()) -> string().

formaterror(Tag) ->
  ftp_response:error_string(Tag).


info(Pid) ->
    call(Pid, info, list).


%%%========================================================================
%%% Behavior callbacks
%%%========================================================================
start_standalone(Options) ->
    try
	{ok, StartOptions} = start_options(Options),
	{ok, OpenOptions}  = open_options(Options), 
	case start_link(StartOptions, []) of
	    {ok, Pid} ->
		call(Pid, {open, ip_comm, OpenOptions}, plain);
	    Error1 ->
		Error1
	end
    catch 
	throw:Error2 ->
	    Error2
    end.

start_service(Options) ->
    try
	{ok, StartOptions} = start_options(Options),
	{ok, OpenOptions}  = open_options(Options), 
	case ftp_sup:start_child([[[{client, self()} | StartOptions], []]]) of
	    {ok, Pid} ->
		call(Pid, {open, ip_comm, OpenOptions}, plain);
	    Error1 ->
		Error1
	end
    catch 
	throw:Error2 ->
	    Error2
    end.

stop_service(Pid) ->
    close(Pid).

services() ->
    [{ftpc, Pid} || {_, Pid, _, _} <- 
			supervisor:which_children(ftp_sup)].
service_info(Pid) ->
    {ok, Info} = call(Pid, info, list),
    {ok, [proplists:lookup(mode, Info), 
	  proplists:lookup(local_port, Info),
	  proplists:lookup(peer, Info),
	  proplists:lookup(peer_port, Info)]}.


%% This function extracts the start options from the 
%% Valid options: 
%%     debug, 
%%     verbose 
%%     ipfamily
%%     priority
%%     flags    (for backward compatibillity)
start_options(Options) ->
    ?fcrt("start_options", [{options, Options}]), 
    case lists:keysearch(flags, 1, Options) of
	{value, {flags, Flags}} ->
	    Verbose = lists:member(verbose, Flags),
	    IsTrace = lists:member(trace, Flags), 
	    IsDebug = lists:member(debug, Flags), 
	    DebugLevel = 
		if 
		    (IsTrace =:= true) ->
			trace;
		    IsDebug =:= true ->
			debug;
		    true ->
			disable
		end,
	    {ok, [{verbose,  Verbose}, 
		  {debug,    DebugLevel}, 
		  {priority, low}]};
	false ->
	    ValidateVerbose = 
		fun(true) -> true;
		   (false) -> true;
		   (_) -> false
		end,
	    ValidateDebug = 
		fun(trace) -> true;
		   (debug) -> true;
		   (disable) -> true;
		   (_) -> false
		end,
	    ValidatePriority = 
		fun(low) -> true;
		   (normal) -> true;
		   (high) -> true;
		   (_) -> false
		end,
	    ValidOptions = 
		[{verbose,  ValidateVerbose,  false, false}, 
		 {debug,    ValidateDebug,    false, disable}, 
		 {priority, ValidatePriority, false, low}], 
	    validate_options(Options, ValidOptions, [])
    end.


%% This function extracts and validates the open options from the 
%% Valid options: 
%%    mode
%%    host
%%    port
%%    timeout
%%    dtimeout
%%    progress
open_options(Options) ->
    ?fcrt("open_options", [{options, Options}]), 
    ValidateMode = 
	fun(active) -> true;
	   (passive) -> true;
	   (_) -> false
	end,
    ValidateHost = 
	fun(Host) when is_list(Host) ->
		true;
	   (Host) when is_tuple(Host) andalso 
		       ((size(Host) =:= 4) orelse (size(Host) =:= 8)) ->
		true;
	   (_) ->
		false
	end,
    ValidatePort = 
	fun(Port) when is_integer(Port) andalso (Port > 0) -> true;
	   (_) -> false
	end,
    ValidateIpFamily = 
	fun(inet) -> true;
	   (inet6) -> true;
	   (inet6fb4) -> true;
	   (_) -> false
	end,
    ValidateTimeout = 
	fun(Timeout) when is_integer(Timeout) andalso (Timeout >= 0) -> true;
	   (_) -> false
	end,
    ValidateDTimeout = 
	fun(DTimeout) when is_integer(DTimeout) andalso (DTimeout >= 0) -> true;
	   (infinity) -> true;
	   (_) -> false
	end,
    ValidateProgress = 
	fun(ignore) -> 
		true;
	   ({Mod, Func, _InitProgress}) when is_atom(Mod) andalso 
					     is_atom(Func) -> 
		true;
	   (_) ->
		false
	end,
    ValidOptions = 
	[{mode,     ValidateMode,     false, ?DEFAULT_MODE}, 
	 {host,     ValidateHost,     true,  ehost},
	 {port,     ValidatePort,     false, ?FTP_PORT},
	 {ipfamily, ValidateIpFamily, false, inet},
	 {timeout,  ValidateTimeout,  false, ?CONNECTION_TIMEOUT}, 
	 {dtimeout, ValidateDTimeout, false, ?DATA_ACCEPT_TIMEOUT}, 
	 {progress, ValidateProgress, false, ?PROGRESS_DEFAULT}], 
    validate_options(Options, ValidOptions, []).

validate_options([], [], Acc) ->
    ?fcrt("validate_options -> done", [{acc, Acc}]), 
    {ok, lists:reverse(Acc)};
validate_options([], ValidOptions, Acc) ->
    ?fcrt("validate_options -> done", 
	  [{valid_options, ValidOptions}, {acc, Acc}]), 
    %% Check if any mandatory options are missing!
    case [{Key, Reason} || {Key, _, true, Reason} <- ValidOptions] of
	[] ->
	    Defaults = 
		[{Key, Default} || {Key, _, _, Default} <- ValidOptions], 
	    {ok, lists:reverse(Defaults ++ Acc)};
	[{_, Reason}|_Missing] ->
	    throw({error, Reason})
    end;
validate_options([{Key, Value}|Options], ValidOptions, Acc) ->
    ?fcrt("validate_options -> check", 
	  [{key, Key}, {value, Value}, {acc, Acc}]), 
    case lists:keysearch(Key, 1, ValidOptions) of
	{value, {Key, Validate, _, Default}} ->
	    case (catch Validate(Value)) of
		true ->
		    ?fcrt("validate_options -> check - accept", []),
		    NewValidOptions = lists:keydelete(Key, 1, ValidOptions),
		    validate_options(Options, NewValidOptions, 
				     [{Key, Value} | Acc]);
		_ ->
		    ?fcrt("validate_options -> check - reject", 
			  [{default, Default}]),
		    NewValidOptions = lists:keydelete(Key, 1, ValidOptions),
		    validate_options(Options, NewValidOptions, 
				     [{Key, Default} | Acc])
	    end;
	false ->
	    validate_options(Options, ValidOptions, Acc)
    end;
validate_options([_|Options], ValidOptions, Acc) ->
    validate_options(Options, ValidOptions, Acc).



%%%========================================================================
%%% gen_server callback functions 
%%%========================================================================

%%-------------------------------------------------------------------------
%% init(Args) -> {ok, State} | {ok, State, Timeout} | {stop, Reason}
%% Description: Initiates the erlang process that manages a ftp connection.
%%-------------------------------------------------------------------------
init(Options) ->
    process_flag(trap_exit, true),

    %% Keep track of the client
    {value, {client, Client}} = lists:keysearch(client, 1, Options), 
    erlang:monitor(process, Client),

    %% Make sure inet is started
    inet_db:start(),
    
    %% Where are we
    {ok, Dir} = file:get_cwd(),

    %% Maybe activate dbg
    case key_search(debug, Options, disable) of
	trace ->
	    dbg:tracer(),
	    dbg:p(all, [call]),
	    dbg:tpl(ftp, [{'_', [], [{return_trace}]}]),
	    dbg:tpl(ftp_response, [{'_', [], [{return_trace}]}]),
	    dbg:tpl(ftp_progress, [{'_', [], [{return_trace}]}]);
	debug ->
	    dbg:tracer(),
	    dbg:p(all, [call]),
	    dbg:tp(ftp, [{'_', [], [{return_trace}]}]),
	    dbg:tp(ftp_response, [{'_', [], [{return_trace}]}]),
	    dbg:tp(ftp_progress, [{'_', [], [{return_trace}]}]); 
	_ ->
	    %% Keep silent
	    ok
    end,

    %% Verbose?
    Verbose  = key_search(verbose, Options, false),

    %% IpFamily?
    IpFamily = key_search(ipfamily, Options, inet),

    State    = #state{owner    = Client, 
		      verbose  = Verbose, 
		      ipfamily = IpFamily, 
		      ldir     = Dir},
    
    %% Set process prio
    Priority = key_search(priority, Options, low),
    process_flag(priority, Priority),

    %% And we are done
    {ok, State}.


%%--------------------------------------------------------------------------
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State}               |
%%                                      {noreply, State, Timeout}      |
%%                                      {stop, Reason, Reply, State}   |
%% Description: Handle incoming requests. 
%%-------------------------------------------------------------------------

%% Anyone can ask this question
handle_call({_, info}, _, #state{verbose  = Verbose,
				 mode     = Mode,
				 timeout  = Timeout,
				 ipfamily = IpFamily,
				 csock    = Socket,
				 progress = Progress} = State) ->
    {ok, {_, LocalPort}}  = inet:sockname(Socket),
    {ok, {Address, Port}} = inet:peername(Socket),
    Options = [{verbose,    Verbose}, 
	       {ipfamily,   IpFamily},
	       {mode,       Mode}, 
	       {peer,       Address}, 
	       {peer_port,  Port}, 
	       {local_port, LocalPort},
	       {timeout,    Timeout}, 
	       {progress,   Progress}],
    {reply, {ok, Options}, State};

%% But everything else must come from the owner
handle_call({Pid, _}, _, #state{owner = Owner} = State) when Owner =/= Pid ->
    {reply, {error, not_connection_owner}, State};

handle_call({_, {open, ip_comm, Opts}}, From, State) ->
    ?fcrd("handle_call(open)", [{opts, Opts}]), 
    case key_search(host, Opts, undefined) of
	undefined ->
	    {stop, normal, {error, ehost}, State};
	Host ->
	    Mode     = key_search(mode,     Opts, ?DEFAULT_MODE),
	    Port     = key_search(port,     Opts, ?FTP_PORT), 
	    Timeout  = key_search(timeout,  Opts, ?CONNECTION_TIMEOUT),
	    DTimeout = key_search(dtimeout, Opts, ?DATA_ACCEPT_TIMEOUT),
	    Progress = key_search(progress, Opts, ignore),
	    IpFamily = key_search(ipfamily, Opts, inet),

	    State2 = State#state{client   = From, 
				 mode     = Mode,
				 progress = progress(Progress),
				 ipfamily = IpFamily, 
				 dtimeout = DTimeout}, 

	    ?fcrd("handle_call(open) -> setup ctrl connection with", 
		  [{host, Host}, {port, Port}, {timeout, Timeout}]), 
	    case setup_ctrl_connection(Host, Port, Timeout, State2) of
		{ok, State3, WaitTimeout} ->
		    ?fcrd("handle_call(open) -> ctrl connection setup done", 
			  [{waittimeout, WaitTimeout}]), 
		    {noreply, State3, WaitTimeout};
		{error, Reason} ->
		    ?fcrd("handle_call(open) -> ctrl connection setup failed", 
			  [{reason, Reason}]), 
		    gen_server:reply(From, {error, ehost}),
		    {stop, normal, State2#state{client = undefined}}
	    end
    end;	

handle_call({_, {open, ip_comm, Host, Opts}}, From, State) ->
    Mode     = key_search(mode,     Opts, ?DEFAULT_MODE),
    Port     = key_search(port,     Opts, ?FTP_PORT), 
    Timeout  = key_search(timeout,  Opts, ?CONNECTION_TIMEOUT),
    DTimeout = key_search(dtimeout, Opts, ?DATA_ACCEPT_TIMEOUT),
    Progress = key_search(progress, Opts, ignore),
    
    State2 = State#state{client   = From, 
			 mode     = Mode,
			 progress = progress(Progress), 
			 dtimeout = DTimeout}, 

    case setup_ctrl_connection(Host, Port, Timeout, State2) of
	{ok, State3, WaitTimeout} ->
	    {noreply, State3, WaitTimeout};
	{error, _Reason} ->
	    gen_server:reply(From, {error, ehost}),
	    {stop, normal, State2#state{client = undefined}}
    end;	

handle_call({_, {user, User, Password}}, From, 
	    #state{csock = CSock} = State) when (CSock =/= undefined) ->
    handle_user(User, Password, "", State#state{client = From});

handle_call({_, {user, User, Password, Acc}}, From, 
	    #state{csock = CSock} = State) when (CSock =/= undefined) ->
    handle_user(User, Password, Acc, State#state{client = From});
   
handle_call({_, {account, Acc}}, From, State)->
    handle_user_account(Acc, State#state{client = From});

handle_call({_, pwd}, From, #state{chunk = false} = State) ->
    send_ctrl_message(State, mk_cmd("PWD", [])), 
    activate_ctrl_connection(State),
    {noreply, State#state{client = From, caller = pwd}};

handle_call({_, lpwd}, From,  #state{ldir = LDir} = State) ->
    {reply, {ok, LDir}, State#state{client = From}};

handle_call({_, {cd, Dir}}, From,  #state{chunk = false} = State) ->
    send_ctrl_message(State, mk_cmd("CWD ~s", [Dir])), 
    activate_ctrl_connection(State),
    {noreply, State#state{client = From, caller = cd}};

handle_call({_,{lcd, Dir}}, _From, #state{ldir = LDir0} = State) ->
    LDir = filename:absname(Dir, LDir0),
    case file:read_file_info(LDir) of %% FIX better check that LDir is a dir.
	{ok, _ } ->
	    {reply, ok, State#state{ldir = LDir}};
	_  ->
	    {reply, {error, epath}, State}
    end;

handle_call({_, {dir, Len, Dir}}, {_Pid, _} = From, 
	    #state{chunk = false} = State) ->
    setup_data_connection(State#state{caller = {dir, Dir, Len},
				      client = From});
handle_call({_, {rename, CurrFile, NewFile}}, From,
	    #state{chunk = false} = State) ->
    send_ctrl_message(State, mk_cmd("RNFR ~s", [CurrFile])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {rename, NewFile}, client = From}};

handle_call({_, {delete, File}}, {_Pid, _} = From, 
	    #state{chunk = false} = State) ->
    send_ctrl_message(State, mk_cmd("DELE ~s", [File])),
    activate_ctrl_connection(State),
    {noreply, State#state{client = From}};

handle_call({_, {mkdir, Dir}}, From,  #state{chunk = false} = State) ->
    send_ctrl_message(State, mk_cmd("MKD ~s", [Dir])),
    activate_ctrl_connection(State),
    {noreply, State#state{client = From}};

handle_call({_,{rmdir, Dir}}, From, #state{chunk = false} = State) ->
    send_ctrl_message(State, mk_cmd("RMD ~s", [Dir])),
    activate_ctrl_connection(State),
    {noreply, State#state{client = From}};

handle_call({_,{type, Type}}, From,  #state{chunk = false} 
	    = State) ->  
    case Type of
	ascii ->
	    send_ctrl_message(State, mk_cmd("TYPE A", [])),
	    activate_ctrl_connection(State),
	    {noreply, State#state{caller = type, type = ascii, 
				  client = From}};
	binary ->
	    send_ctrl_message(State, mk_cmd("TYPE I", [])),
	    activate_ctrl_connection(State),
	    {noreply, State#state{caller = type, type = binary, 
				  client = From}};
	_ ->
	    {reply, {error, etype}, State}
    end;

handle_call({_,{recv, RemoteFile, LocalFile}}, From, 
	    #state{chunk = false, ldir = LocalDir} = State) ->
    progress_report({remote_file, RemoteFile}, State),
    NewLocalFile = filename:absname(LocalFile, LocalDir),

    case file_open(NewLocalFile, write) of
	{ok, Fd} ->
	    setup_data_connection(State#state{client = From,
					      caller = 
					      {recv_file, 
					       RemoteFile, Fd}});
	{error, _What} ->
	    {reply, {error, epath}, State}
    end;

handle_call({_, {recv_bin, RemoteFile}}, From, #state{chunk = false} = 
	    State) ->
    setup_data_connection(State#state{caller = {recv_bin, RemoteFile},
				      client = From});

handle_call({_,{recv_chunk_start, RemoteFile}}, From, #state{chunk = false} 
	    = State) ->
    setup_data_connection(State#state{caller = {start_chunk_transfer,
						"RETR", RemoteFile},
				      client = From});

handle_call({_, recv_chunk}, _, #state{chunk = false} = State) ->
    {reply, {error, "ftp:recv_chunk_start/2 not called"}, State}; 

handle_call({_, recv_chunk}, From, #state{chunk = true} = State) ->
    activate_data_connection(State),
    {noreply, State#state{client = From, caller = recv_chunk}};
    
handle_call({_, {send, LocalFile, RemoteFile}}, From, 
	    #state{chunk = false, ldir = LocalDir} = State) ->
    progress_report({local_file, filename:absname(LocalFile, LocalDir)}, 
		    State),
    setup_data_connection(State#state{caller = {transfer_file,
						   {"STOR", 
						    LocalFile, RemoteFile}},
					 client = From});
handle_call({_, {append, LocalFile, RemoteFile}}, From, 
	    #state{chunk = false} = State) ->
    setup_data_connection(State#state{caller = {transfer_file,
						{"APPE", 
						 LocalFile, RemoteFile}},
				      client = From});
handle_call({_, {send_bin, Bin, RemoteFile}}, From, 
	    #state{chunk = false} = State) ->
    setup_data_connection(State#state{caller = {transfer_data,
					       {"STOR", Bin, RemoteFile}},
				      client = From});
handle_call({_,{append_bin, Bin, RemoteFile}}, From, 
	    #state{chunk = false} = State) ->
    setup_data_connection(State#state{caller = {transfer_data,
						{"APPE", Bin, RemoteFile}},
				      client = From});
handle_call({_, {send_chunk_start, RemoteFile}}, From, #state{chunk = false} 
	    = State) ->
    setup_data_connection(State#state{caller = {start_chunk_transfer,
						"STOR", RemoteFile},
				      client = From});
handle_call({_, {append_chunk_start, RemoteFile}}, From, #state{chunk = false} 
	    = State) ->
    setup_data_connection(State#state{caller = {start_chunk_transfer,
						"APPE", RemoteFile},
				      client = From});
handle_call({_, {transfer_chunk, Bin}}, _, #state{chunk = true} = State) ->
    send_data_message(State, Bin),
    {reply, ok, State};

handle_call({_, chunk_end}, From, #state{chunk = true} = State) ->
    close_data_connection(State),
    activate_ctrl_connection(State),
    {noreply, State#state{client = From, dsock = undefined, 
			  caller = end_chunk_transfer, chunk = false}};

handle_call({_, {quote, Cmd}}, From, #state{chunk = false} = State) ->
    send_ctrl_message(State, mk_cmd(Cmd, [])),
    activate_ctrl_connection(State),
    {noreply, State#state{client = From, caller = quote}};

handle_call({_, _Req}, _From, #state{csock = CSock} = State) 
  when (CSock =:= undefined) ->
    {reply, {error, not_connected}, State};

handle_call(_, _, #state{chunk = true} = State) ->
    {reply, {error, echunk}, State};

%% Catch all -  This can only happen if the application programmer writes 
%% really bad code that violates the API.
handle_call(Request, _Timeout, State) ->
    {stop, {'API_violation_connection_closed', Request},
     {error, {connection_terminated, 'API_violation'}}, State}.

%%--------------------------------------------------------------------------
%% handle_cast(Request, State) -> {noreply, State} | 
%%                                {noreply, State, Timeout} |
%%                                {stop, Reason, State} 
%% Description: Handles cast messages.         
%%-------------------------------------------------------------------------
handle_cast({Pid, close}, #state{owner = Pid} = State) ->
    send_ctrl_message(State, mk_cmd("QUIT", [])),
    close_ctrl_connection(State),
    close_data_connection(State),
    {stop, normal, State#state{csock = undefined, dsock = undefined}};

handle_cast({Pid, close}, State) ->
    Report = io_lib:format("A none owner process ~p tried to close an "
			     "ftp connection: ~n", [Pid]),
    error_logger:info_report(Report),
    {noreply, State};

%% Catch all -  This can oly happen if the application programmer writes 
%% really bad code that violates the API.
handle_cast(Msg, State) ->
  {stop, {'API_violation_connection_closed', Msg}, State}.

%%--------------------------------------------------------------------------
%% handle_info(Msg, State) -> {noreply, State} | {noreply, State, Timeout} |
%%			      {stop, Reason, State}
%% Description: Handles tcp messages from the ftp-server.
%% Note: The order of the function clauses is significant.
%%--------------------------------------------------------------------------

handle_info(timeout, #state{caller = open} = State) ->
    {stop, timeout, State};

handle_info(timeout, State) ->
    {noreply, State};

%%% Data socket messages %%%
handle_info({tcp, Socket, Data}, 
	    #state{dsock = Socket, 
		   caller = {recv_file, Fd}} = State) ->    
    file_write(binary_to_list(Data), Fd),
    progress_report({binary, Data}, State),
    activate_data_connection(State),
    {noreply, State};

handle_info({tcp, Socket, Data}, #state{dsock = Socket, client = From,	
					caller = recv_chunk} 
	    = State)  ->    
    gen_server:reply(From, {ok, Data}),
    {noreply, State#state{client = undefined, data = <<>>}};

handle_info({tcp, Socket, Data}, #state{dsock = Socket} = State) ->
    activate_data_connection(State),
    {noreply, State#state{data = <<(State#state.data)/binary,
				  Data/binary>>}};

handle_info({tcp_closed, Socket}, #state{dsock = Socket,
					 caller = {recv_file, Fd}} 
	    = State) ->
    file_close(Fd),
    progress_report({transfer_size, 0}, State),
    activate_ctrl_connection(State),
    {noreply, State#state{dsock = undefined, data = <<>>}};

handle_info({tcp_closed, Socket}, #state{dsock = Socket, client = From,
					 caller = recv_chunk} 
	    = State) ->
    gen_server:reply(From, ok),
    {noreply, State#state{dsock = undefined, client = undefined,
			  data = <<>>, caller = undefined,
			  chunk = false}};

handle_info({tcp_closed, Socket}, #state{dsock = Socket, caller = recv_bin, 
					 data = Data} = State) ->
    activate_ctrl_connection(State),
    {noreply, State#state{dsock = undefined, data = <<>>, 
			  caller = {recv_bin, Data}}};

handle_info({tcp_closed, Socket}, #state{dsock = Socket, data = Data,
					 caller = {handle_dir_result, Dir}} 
	    = State) ->
    activate_ctrl_connection(State),
    {noreply, State#state{dsock = undefined, 
			  caller = {handle_dir_result, Dir, Data},
%			  data = <<?CR,?LF>>}};
			  data = <<>>}};
	    
handle_info({tcp_error, Socket, Reason}, #state{dsock = Socket,
						client = From} = State) ->
    gen_server:reply(From, {error, Reason}),
    close_data_connection(State),
    {noreply, State#state{dsock = undefined, client = undefined,
			  data = <<>>, caller = undefined, chunk = false}};

%%% Ctrl socket messages %%%
handle_info({tcp, Socket, Data}, #state{csock = Socket, 
					verbose = Verbose,
					caller = Caller,
					client = From,
					ctrl_data = {CtrlData, AccLines, 
						     LineStatus}} 
	    = State) ->    
    case ftp_response:parse_lines(<<CtrlData/binary, Data/binary>>, 
				  AccLines, LineStatus) of
	{ok, Lines, NextMsgData} ->
	    verbose(Lines, Verbose, 'receive'),
	    CtrlResult = ftp_response:interpret(Lines), 
	    case Caller of
		quote ->
		    gen_server:reply(From, string:tokens(Lines, [?CR, ?LF])),
		    {noreply, State#state{client = undefined, 
					  caller = undefined,
					  ctrl_data = {NextMsgData, [], 
						       start}}};
		_ ->
		    handle_ctrl_result(CtrlResult,
				       State#state{ctrl_data = 
						   {NextMsgData, [], start}})
	    end;
	{continue, NewCtrlData} ->
	    activate_ctrl_connection(State),
	    {noreply, State#state{ctrl_data = NewCtrlData}}
    end;

handle_info({tcp_closed, Socket}, #state{csock = Socket}) ->  
    %% If the server closes the control channel it is 
    %% the expected behavior that connection process terminates.
    exit(normal); %% User will get error message from terminate/2

handle_info({tcp_error, Socket, Reason}, _) ->
    Report = 
	io_lib:format("tcp_error on socket: ~p  for reason: ~p~n",
		      [Socket, Reason]),
    error_logger:error_report(Report),
    %% If tcp does not work the only option is to terminate,
    %% this is the expected behavior under these circumstances.
    exit(normal); %% User will get error message from terminate/2

%% Monitor messages - if the process owning the ftp connection goes
%% down there is no point in continuing.
handle_info({'DOWN', _Ref, _Type, _Process, normal}, State) ->
    {stop, normal, State#state{client = undefined}};

handle_info({'DOWN', _Ref, _Type, _Process, shutdown}, State) ->
    {stop, normal, State#state{client = undefined}};
    
handle_info({'DOWN', _Ref, _Type, _Process, timeout}, State) ->
    {stop, normal, State#state{client = undefined}};
 
handle_info({'DOWN', _Ref, _Type, Process, Reason}, State) ->
    {stop, {stopped, {'EXIT', Process, Reason}},
     State#state{client = undefined}};

handle_info({'EXIT', Pid, Reason}, #state{progress = Pid} = State) ->
    Report = io_lib:format("Progress reporting stopped for reason ~p~n",
			   Reason),
    error_logger:info_report(Report),
    {noreply, State#state{progress = ignore}};
   
%% Catch all - throws away unknown messages (This could happen by "accident"
%% so we do not want to crash, but we make a log entry as it is an
%% unwanted behaviour.) 
handle_info(Info, State) ->
    Report = io_lib:format("ftp : ~p : Unexpected message: ~p\n", 
			   [self(), Info]),
    error_logger:info_report(Report),
    {noreply, State}.

%%--------------------------------------------------------------------------
%% terminate/2 and code_change/3
%%--------------------------------------------------------------------------
terminate(normal, State) ->
    %% If terminate reason =/= normal the progress reporting process will
    %% be killed by the exit signal.
    progress_report(stop, State), 
    do_termiante({error, econn}, State);
terminate(Reason, State) -> 
    Report = io_lib:format("Ftp connection closed due to: ~p~n", [Reason]),
    error_logger:error_report(Report),
    do_termiante({error, eclosed}, State).

do_termiante(ErrorMsg, State) ->
    close_data_connection(State),
    close_ctrl_connection(State),
    case State#state.client of
	undefined ->
	    ok;
	From ->
	    gen_server:reply(From, ErrorMsg)
    end,
    ok. 

code_change(_Vsn, State1, upgrade_from_pre_5_12) ->
    {state, CSock, DSock, Verbose, LDir, Type, Chunk, Mode, Timeout, 
     Data, CtrlData, Owner, Client, Caller, IPv6Disable, Progress} = State1, 
    IpFamily = 
	if
	    (IPv6Disable =:= true) ->
		inet;
	    true ->
		inet6fb4
	end,
    State2 = #state{csock     = CSock,
		    dsock     = DSock,
		    verbose   = Verbose, 
		    ldir      = LDir,
		    type      = Type, 
		    chunk     = Chunk, 
		    mode      = Mode, 
		    timeout   = Timeout, 
		    data      = Data, 
		    ctrl_data = CtrlData, 
		    owner     = Owner, 
		    client    = Client, 
		    caller    = Caller, 
		    ipfamily  = IpFamily,
		    progress  = Progress}, 
    {ok, State2};

code_change(_Vsn, State1, downgrade_to_pre_5_12) ->
    #state{csock     = CSock,
	   dsock     = DSock,
	   verbose   = Verbose, 
	   ldir      = LDir,
	   type      = Type, 
	   chunk     = Chunk, 
	   mode      = Mode, 
	   timeout   = Timeout, 
	   data      = Data, 
	   ctrl_data = CtrlData, 
	   owner     = Owner, 
	   client    = Client, 
	   caller    = Caller, 
	   ipfamily  = IpFamily,
	   progress  = Progress} = State1, 
    IPv6Disable = 
	if
	    (IpFamily =:= inet) ->
		true;
	    true ->
		false
	end,
    State2 = 
	{state, CSock, DSock, Verbose, LDir, Type, Chunk, Mode, Timeout, 
	 Data, CtrlData, Owner, Client, Caller, IPv6Disable, Progress},
    {ok, State2};

code_change(_Vsn, State, _Extra) ->
    {ok, State}.


%%%=========================================================================
%% Start/stop
%%%=========================================================================
%%--------------------------------------------------------------------------
%% start_link([Opts, GenServerOptions]) -> {ok, Pid} | {error, Reason} 
%%                                    
%% Description: Callback function for the ftp supervisor. It is called 
%%            : when start_service/1 calls ftp_sup:start_child/1 to start an 
%%            : instance of the ftp process. Also called by start_standalone/1
%%--------------------------------------------------------------------------
start_link([Opts, GenServerOptions]) ->
    start_link(Opts, GenServerOptions).

start_link(Opts, GenServerOptions) ->
    case lists:keysearch(client, 1, Opts) of
	{value, _} ->
	    %% Via the supervisor
	    gen_server:start_link(?MODULE, Opts, GenServerOptions);
	false ->
	    Opts2 = [{client, self()} | Opts], 
	    gen_server:start_link(?MODULE, Opts2, GenServerOptions)
    end.


%%% Stop functionality is handled by close/1

%%%========================================================================
%%% Internal functions
%%%========================================================================

%%--------------------------------------------------------------------------
%%% Help functions to handle_call and/or handle_ctrl_result
%%--------------------------------------------------------------------------
%% User handling 
handle_user(User, Password, Acc, State) ->
    send_ctrl_message(State, mk_cmd("USER ~s", [User])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {handle_user, Password, Acc}}}.

handle_user_passwd(Password, Acc, State) ->
    send_ctrl_message(State, mk_cmd("PASS ~s", [Password])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {handle_user_passwd, Acc}}}.

handle_user_account(Acc, State) ->
    send_ctrl_message(State, mk_cmd("ACCT ~s", [Acc])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = handle_user_account}}.


%%--------------------------------------------------------------------------
%% handle_ctrl_result 
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------------
%% Handling of control connection setup
handle_ctrl_result({pos_compl, _}, #state{caller = open, client = From} 
		   = State) ->
    gen_server:reply(From,  {ok, self()}),
    {noreply, State#state{client = undefined, 
			  caller = undefined }};
handle_ctrl_result({_, Lines}, #state{caller = open} = State) ->
    ctrl_result_response(econn, State, {error, Lines});

%%--------------------------------------------------------------------------
%% Data connection setup active mode 
handle_ctrl_result({pos_compl, _Lines}, 
		   #state{mode   = active,
			  caller = {setup_data_connection, 
				    {LSock, Caller}}} = State) ->
    handle_caller(State#state{caller = Caller, dsock = {lsock, LSock}});

handle_ctrl_result({Status, Lines}, 
		   #state{mode   = active, 
			  caller = {setup_data_connection, {LSock, _}}} 
		   = State) ->
    close_connection(LSock),
    ctrl_result_response(Status, State, {error, Lines});

%% Data connection setup passive mode 
handle_ctrl_result({pos_compl, Lines}, 
		   #state{mode     = passive,
			  ipfamily = inet6,
			  client   = From,
			  caller   = {setup_data_connection, Caller},
			  csock    = CSock,
			  timeout  = Timeout} 
		   = State) ->
    [_, PortStr | _] =  lists:reverse(string:tokens(Lines, "|")),
    {ok, {IP, _}} = inet:peername(CSock),
    case connect(IP, list_to_integer(PortStr), Timeout, State) of
	{ok, _, Socket} ->	       
	    handle_caller(State#state{caller = Caller, dsock = Socket});
	{error, _Reason} = Error ->
	    gen_server:reply(From, Error),
	    {noreply, State#state{client = undefined, caller = undefined}}
    end;

handle_ctrl_result({pos_compl, Lines}, 
		   #state{mode     = passive, 
			  ipfamily = inet,
			  client   = From,
			  caller   = {setup_data_connection, Caller}, 
			  timeout  = Timeout} = State) ->
    
    {_, [?LEFT_PAREN | Rest]} = 
	lists:splitwith(fun(?LEFT_PAREN) -> false; (_) -> true end, Lines),
    {NewPortAddr, _} =
	lists:splitwith(fun(?RIGHT_PAREN) -> false; (_) -> true end, Rest),
    [A1, A2, A3, A4, P1, P2] = 
	lists:map(fun(X) -> list_to_integer(X) end, 
		  string:tokens(NewPortAddr, [$,])),
    IP   = {A1, A2, A3, A4}, 
    Port = (P1 * 256) + P2, 
    case connect(IP, Port, Timeout, State) of
	{ok, _, Socket} ->
	    handle_caller(State#state{caller = Caller, dsock = Socket});
	{error, _Reason} = Error ->
	    gen_server:reply(From, Error),
	    {noreply,State#state{client = undefined, caller = undefined}}
    end;

%% FTP server does not support passive mode: try to fallback on active mode
handle_ctrl_result(_, 
		   #state{mode = passive, 
			  caller = {setup_data_connection, Caller}} = State) ->
    setup_data_connection(State#state{mode = active, caller = Caller});

    
%%--------------------------------------------------------------------------
%% User handling 
handle_ctrl_result({pos_interm, _}, 
		   #state{caller = {handle_user, PassWord, Acc}} = State) ->
    handle_user_passwd(PassWord, Acc, State);
handle_ctrl_result({Status, _}, 
		   #state{caller = {handle_user, _, _}} = State) ->
    ctrl_result_response(Status, State, {error, euser});

%% Accounts 
handle_ctrl_result({pos_interm_acct, _}, 
		   #state{caller = {handle_user_passwd, Acc}} = State) 
  when Acc =/= "" ->
    handle_user_account(Acc, State);
handle_ctrl_result({Status, _},
		   #state{caller = {handle_user_passwd, _}} = State) ->
    ctrl_result_response(Status, State, {error, euser});

%%--------------------------------------------------------------------------
%% Print current working directory
handle_ctrl_result({pos_compl, Lines}, 
		   #state{caller = pwd, client = From} = State) ->
    Dir = pwd_result(Lines),
    gen_server:reply(From, {ok, Dir}),
    {noreply, State#state{client = undefined, caller = undefined}};

%%--------------------------------------------------------------------------
%% Directory listing 
handle_ctrl_result({pos_prel, _}, #state{caller = {dir, Dir}} = State) ->
    case accept_data_connection(State) of
	{ok, NewState} ->
	    activate_data_connection(NewState),
	    {noreply, NewState#state{caller = {handle_dir_result, Dir}}};
	{error, _Reason} = ERROR ->
	    case State#state.client of
		undefined ->
		    {stop, ERROR, State};
		From ->
		    gen_server:reply(From, ERROR),
		    {stop, normal, State#state{client = undefined}}
	    end
    end;

handle_ctrl_result({pos_compl, _}, #state{caller = {handle_dir_result, Dir,
						    Data}, client = From} 
		   = State) ->
    case Dir of
	"" -> % Current directory
	    gen_server:reply(From, {ok, Data}),
	    {noreply, State#state{client = undefined, 
				  caller = undefined}};
	_ ->
	    %% <WTF>
	    %% Dir cannot be assumed to be a dir. It is a string that 
	    %% could be a dir, but could also be a file or even a string
	    %% containing wildcards (*).
	    %% 
	    %% %% If there is only one line it might be a directory with one
	    %% %% file but it might be an error message that the directory
	    %% %% was not found. So in this case we have to endure a little
	    %% %% overhead to be able to give a good return value. Alas not
	    %% %% all ftp implementations behave the same and returning
	    %% %% an error string is allowed by the FTP RFC. 
	    %% case lists:dropwhile(fun(?CR) -> false;(_) -> true end, 
	    %% 			 binary_to_list(Data)) of
	    %% 	L when (L =:= [?CR, ?LF]) orelse (L =:= []) ->	
	    %% 	    send_ctrl_message(State, mk_cmd("PWD", [])),
	    %% 	    activate_ctrl_connection(State),
	    %% 	    {noreply, 
	    %% 	     State#state{caller = {handle_dir_data, Dir, Data}}};
	    %% 	_ ->
	    %% 	    gen_server:reply(From, {ok, Data}),
	    %% 	    {noreply, State#state{client = undefined,
	    %% 				  caller = undefined}}
	    %% end
	    %% </WTF>
	    gen_server:reply(From, {ok, Data}),
	    {noreply, State#state{client = undefined,
				  caller = undefined}}
    end;

handle_ctrl_result({pos_compl, Lines}, 
		   #state{caller = {handle_dir_data, Dir, DirData}} = 
		   State) ->
    OldDir = pwd_result(Lines),    
    send_ctrl_message(State, mk_cmd("CWD ~s", [Dir])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {handle_dir_data_second_phase, OldDir,
				    DirData}}};
handle_ctrl_result({Status, _},
		   #state{caller = {handle_dir_data, _, _}} = State) ->
    ctrl_result_response(Status, State, {error, epath});

handle_ctrl_result(S={_Status, _},
		   #state{caller = {handle_dir_result, _, _}} = State) ->
    %% OTP-5731, macosx
    ctrl_result_response(S, State, {error, epath});

handle_ctrl_result({pos_compl, _},
		   #state{caller = {handle_dir_data_second_phase, OldDir, 
				    DirData}} = State) ->
    send_ctrl_message(State, mk_cmd("CWD ~s", [OldDir])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {handle_dir_data_third_phase, DirData}}};
handle_ctrl_result({Status, _}, 
		   #state{caller = {handle_dir_data_second_phase, _, _}} 
		   = State) ->
    ctrl_result_response(Status, State, {error, epath});
handle_ctrl_result(_, #state{caller = {handle_dir_data_third_phase, DirData},
			     client = From} = State) ->
    gen_server:reply(From, {ok, DirData}),
    {noreply, State#state{client = undefined, caller = undefined}};

handle_ctrl_result({Status, _}, #state{caller = cd} = State) ->
    ctrl_result_response(Status, State, {error, epath});

handle_ctrl_result(Status={epath, _}, #state{caller = {dir,_}} = State) ->
     ctrl_result_response(Status, State, {error, epath});

%%--------------------------------------------------------------------------
%% File renaming
handle_ctrl_result({pos_interm, _}, #state{caller = {rename, NewFile}} 
		   = State) ->
    send_ctrl_message(State, mk_cmd("RNTO ~s", [NewFile])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = rename_second_phase}}; 

handle_ctrl_result({Status, _}, 
		   #state{caller = {rename, _}} = State) ->
    ctrl_result_response(Status, State, {error, epath});

handle_ctrl_result({Status, _},
		   #state{caller = rename_second_phase} = State) ->
    ctrl_result_response(Status, State, {error, epath});

%%--------------------------------------------------------------------------
%% File handling - recv_bin
handle_ctrl_result({pos_prel, _}, #state{caller = recv_bin} = State) ->
    case accept_data_connection(State) of
	{ok, NewState} ->
	    activate_data_connection(NewState),
	    {noreply, NewState};
	{error, _Reason} = ERROR ->
	    case State#state.client of
		undefined ->
		    {stop, ERROR, State};
		From ->
		    gen_server:reply(From, ERROR),
		    {stop, normal, State#state{client = undefined}}
	    end
    end;

handle_ctrl_result({pos_compl, _}, #state{caller = {recv_bin, Data},
					  client = From} = State) ->
    gen_server:reply(From, {ok, Data}),
    close_data_connection(State),
    {noreply, State#state{client = undefined, caller = undefined}};

handle_ctrl_result({Status, _}, #state{caller = recv_bin} = State) ->
    close_data_connection(State),
    ctrl_result_response(Status, State#state{dsock = undefined}, 
			 {error, epath});

handle_ctrl_result({Status, _}, #state{caller = {recv_bin, _}} = State) ->
    close_data_connection(State),
    ctrl_result_response(Status, State#state{dsock = undefined}, 
			 {error, epath});
%%--------------------------------------------------------------------------
%% File handling - start_chunk_transfer
handle_ctrl_result({pos_prel, _}, #state{client = From,
					 caller = start_chunk_transfer}
		   = State) ->
    case accept_data_connection(State) of
	{ok, NewState} ->
	    gen_server:reply(From, ok),
	    {noreply, NewState#state{chunk = true, client = undefined,
				     caller = undefined}};
	{error, _Reason} = ERROR ->
	    case State#state.client of
		undefined ->
		    {stop, ERROR, State};
		From ->
		    gen_server:reply(From, ERROR),
		    {stop, normal, State#state{client = undefined}}
	    end
    end;

%%--------------------------------------------------------------------------
%% File handling - recv_file
handle_ctrl_result({pos_prel, _}, #state{caller = {recv_file, _}} = State) ->
    case accept_data_connection(State) of
	{ok, NewState} ->
	    activate_data_connection(NewState),
	    {noreply, NewState};
	{error, _Reason} = ERROR ->
	    case State#state.client of
		undefined ->
		    {stop, ERROR, State};
		From ->
		    gen_server:reply(From, ERROR),
		    {stop, normal, State#state{client = undefined}}
	    end
    end;

handle_ctrl_result({Status, _}, #state{caller = {recv_file, Fd}} = State) ->
    file_close(Fd),
    close_data_connection(State),
    ctrl_result_response(Status, State#state{dsock = undefined}, 
			 {error, epath});
%%--------------------------------------------------------------------------
%% File handling - transfer_*
handle_ctrl_result({pos_prel, _}, #state{caller = {transfer_file, Fd}} 
		   = State) ->
    case accept_data_connection(State) of
	{ok, NewState} ->
	    send_file(Fd, NewState); 
	{error, _Reason} = ERROR ->
	    case State#state.client of
		undefined ->
		    {stop, ERROR, State};
		From ->
		    gen_server:reply(From, ERROR),
		    {stop, normal, State#state{client = undefined}}
	    end
    end;

handle_ctrl_result({pos_prel, _}, #state{caller = {transfer_data, Bin}} 
		   = State) ->
    case accept_data_connection(State) of
	{ok, NewState} ->
	    send_data_message(NewState, Bin),
	    close_data_connection(NewState),
	    activate_ctrl_connection(NewState),
	    {noreply, NewState#state{caller = transfer_data_second_phase,
				     dsock = undefined}};
	{error, _Reason} = ERROR ->
	    case State#state.client of
		undefined ->
		    {stop, ERROR, State};
		From ->
		    gen_server:reply(From, ERROR),
		    {stop, normal, State#state{client = undefined}}
	    end
    end;

%%--------------------------------------------------------------------------
%% Default
handle_ctrl_result({Status, Lines}, #state{client = From} = State) 
  when From =/= undefined ->
    ctrl_result_response(Status, State, {error, Lines}).

%%--------------------------------------------------------------------------
%% Help functions to handle_ctrl_result
%%--------------------------------------------------------------------------
ctrl_result_response(pos_compl, #state{client = From} = State, _)  ->
    gen_server:reply(From, ok),
    {noreply, State#state{client = undefined, caller = undefined}};

ctrl_result_response(enofile, #state{client = From} = State, _) ->
    gen_server:reply(From, {error, enofile}),
    {noreply, State#state{client = undefined, caller = undefined}};

ctrl_result_response(Status, #state{client = From} = State, _) 
  when (Status =:= etnospc)  orelse 
       (Status =:= epnospc)  orelse 
       (Status =:= efnamena) orelse 
       (Status =:= econn) ->
%Status == etnospc; Status == epnospc; Status == econn ->
    gen_server:reply(From, {error, Status}),
%%    {stop, normal, {error, Status}, State#state{client = undefined}};
    {stop, normal, State#state{client = undefined}};

ctrl_result_response(_, #state{client = From} = State, ErrorMsg) ->
    gen_server:reply(From, ErrorMsg),
    {noreply, State#state{client = undefined, caller = undefined}}.

%%--------------------------------------------------------------------------
handle_caller(#state{caller = {dir, Dir, Len}} = State) ->
    Cmd = case Len of
	      short -> "NLST";
	      long -> "LIST"
	  end,
    case Dir of 
	"" ->
	    send_ctrl_message(State, mk_cmd(Cmd, ""));
	_ ->
	    send_ctrl_message(State, mk_cmd(Cmd ++ " ~s", [Dir]))
    end,
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {dir, Dir}}};
     
handle_caller(#state{caller = {recv_bin, RemoteFile}} = State) ->
    send_ctrl_message(State, mk_cmd("RETR ~s", [RemoteFile])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = recv_bin}};

handle_caller(#state{caller = {start_chunk_transfer, Cmd, RemoteFile}} = 
	      State) ->
    send_ctrl_message(State, mk_cmd("~s ~s", [Cmd, RemoteFile])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = start_chunk_transfer}};

handle_caller(#state{caller = {recv_file, RemoteFile, Fd}} = State) ->
    send_ctrl_message(State, mk_cmd("RETR ~s", [RemoteFile])), 
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {recv_file, Fd}}};

handle_caller(#state{caller = {transfer_file, {Cmd, LocalFile, RemoteFile}},
		     ldir = LocalDir, client = From} = State) ->
    case file_open(filename:absname(LocalFile, LocalDir), read) of
	{ok, Fd} ->
	    send_ctrl_message(State, mk_cmd("~s ~s", [Cmd, RemoteFile])),
	    activate_ctrl_connection(State),
	    {noreply, State#state{caller = {transfer_file, Fd}}};
	{error, _} ->
	    gen_server:reply(From, {error, epath}),
	    {noreply, State#state{client = undefined, caller = undefined,
				  dsock = undefined}} 
    end;

handle_caller(#state{caller = {transfer_data, {Cmd, Bin, RemoteFile}}} = 
	      State) ->
    send_ctrl_message(State, mk_cmd("~s ~s", [Cmd, RemoteFile])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {transfer_data, Bin}}}.

%%  ----------- FTP SERVER COMMUNICATION  ------------------------- 

%% Connect to FTP server at Host (default is TCP port 21) 
%% in order to establish a control connection.
setup_ctrl_connection(Host, Port, Timeout, State) ->
    MsTime = millisec_time(),
    case connect(Host, Port, Timeout, State) of
	{ok, IpFam, CSock} ->
	    NewState = State#state{csock = CSock, ipfamily = IpFam},
	    activate_ctrl_connection(NewState),
	    case Timeout - (millisec_time() - MsTime) of
		Timeout2 when (Timeout2 >= 0) ->
		    {ok, NewState#state{caller = open}, Timeout2};
		_ ->
		    %% Oups: Simulate timeout
		    {ok, NewState#state{caller = open}, 0}
	    end;
	Error ->
	    Error
    end.

setup_data_connection(#state{mode   = active, 
			     caller = Caller, 
			     csock  = CSock} = State) ->    
    case (catch inet:sockname(CSock)) of
	{ok, {{_, _, _, _, _, _, _, _} = IP, _}} ->
	    {ok, LSock} = 
		gen_tcp:listen(0, [{ip, IP}, {active, false},
				   inet6, binary, {packet, 0}]),
	    {ok, Port} = inet:port(LSock),
	    IpAddress = inet_parse:ntoa(IP),
	    Cmd = mk_cmd("EPRT |2|~s|~p|", [IpAddress, Port]),
	    send_ctrl_message(State, Cmd),
	    activate_ctrl_connection(State),  
	    {noreply, State#state{caller = {setup_data_connection, 
					    {LSock, Caller}}}};
	{ok, {{_,_,_,_} = IP, _}} ->	    
	    {ok, LSock} = gen_tcp:listen(0, [{ip, IP}, {active, false},
					     binary, {packet, 0}]),
	    {ok, Port} = inet:port(LSock),
	    {IP1, IP2, IP3, IP4} = IP,
	    {Port1, Port2} = {Port div 256, Port rem 256},
	    send_ctrl_message(State, 
			      mk_cmd("PORT ~w,~w,~w,~w,~w,~w",
				     [IP1, IP2, IP3, IP4, Port1, Port2])),
	    activate_ctrl_connection(State),
	    {noreply, State#state{caller = {setup_data_connection, 
					    {LSock, Caller}}}}
    end;

setup_data_connection(#state{mode = passive, ipfamily = inet6,
			     caller = Caller} = State) ->
    send_ctrl_message(State, mk_cmd("EPSV", [])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {setup_data_connection, Caller}}};

setup_data_connection(#state{mode = passive, ipfamily = inet,
			     caller = Caller} = State) ->
    send_ctrl_message(State, mk_cmd("PASV", [])),
    activate_ctrl_connection(State),
    {noreply, State#state{caller = {setup_data_connection, Caller}}}.


%% setup_data_connection(#state{mode = passive, ip_v6_disabled = false,
%% 			     caller = Caller} = State) ->
%%     send_ctrl_message(State, mk_cmd("EPSV", [])),
%%     activate_ctrl_connection(State),
%%     {noreply, State#state{caller = {setup_data_connection, Caller}}};

%% setup_data_connection(#state{mode = passive, ip_v6_disabled = true,
%% 			     caller = Caller} = State) ->
%%     send_ctrl_message(State, mk_cmd("PASV", [])),
%%     activate_ctrl_connection(State),
%%     {noreply, State#state{caller = {setup_data_connection, Caller}}}.


connect(Host, Port, Timeout, #state{ipfamily = inet = IpFam}) ->
    connect2(Host, Port, IpFam, Timeout);

connect(Host, Port, Timeout, #state{ipfamily = inet6 = IpFam}) ->
    connect2(Host, Port, IpFam, Timeout);

connect(Host, Port, Timeout, #state{ipfamily = inet6fb4}) ->
    case inet:getaddr(Host, inet6) of
	{ok, {0, 0, 0, 0, 0, 16#ffff, _, _} = IPv6} ->
	    case inet:getaddr(Host, inet) of
		{ok, IPv4} ->
		    IpFam = inet, 
		    connect2(IPv4, Port, IpFam, Timeout);
		
		_ ->
		    IpFam = inet6, 
		    connect2(IPv6, Port, IpFam, Timeout)
	    end;
	
	{ok, IPv6} ->
	    IpFam = inet6, 
	    connect2(IPv6, Port, IpFam, Timeout);
	
	_ ->
	    case inet:getaddr(Host, inet) of
		{ok, IPv4} ->
		    IpFam = inet,
		    connect2(IPv4, Port, IpFam, Timeout);
		Error ->
		    Error
	    end
    end.

connect2(Host, Port, IpFam, Timeout) ->
    Opts = [IpFam, binary, {packet, 0}, {active, false}], 
    case gen_tcp:connect(Host, Port, Opts, Timeout) of
	{ok, Sock} ->
	    {ok, IpFam, Sock};
	Error ->
	    Error
    end.
	    

accept_data_connection(#state{mode     = active,
			      dtimeout = DTimeout, 
			      dsock    = {lsock, LSock}} = State) ->
    case gen_tcp:accept(LSock, DTimeout) of
	{ok, Socket} ->
	    gen_tcp:close(LSock),
	    {ok, State#state{dsock = Socket}};
	{error, Reason} ->
	    {error, {data_connect_failed, Reason}}
    end;

accept_data_connection(#state{mode = passive} = State) ->
    {ok, State}.

send_ctrl_message(#state{csock = Socket, verbose = Verbose}, Message) ->
    %% io:format("send control message: ~n~p~n", [lists:flatten(Message)]),
    verbose(lists:flatten(Message),Verbose,send),
    send_message(Socket, Message).

send_data_message(#state{dsock = Socket}, Message) ->
    send_message(Socket, Message).

send_message(Socket, Message) ->
    case gen_tcp:send(Socket, Message) of
	ok ->
	    ok;
	{error, Reason} ->
	    Report = io_lib:format("gen_tcp:send/2 failed for "
				   "reason ~p~n", [Reason]),
	    error_logger:error_report(Report),
	    %% If tcp does not work the only option is to terminate,
	    %% this is the expected behavior under these circumstances.
	    exit(normal) %% User will get error message from terminate/2
    end.

activate_ctrl_connection(#state{csock = Socket, ctrl_data = {<<>>, _, _}}) ->
    activate_connection(Socket);
activate_ctrl_connection(#state{csock = Socket}) ->
    %% We have already received at least part of the next control message,
    %% that has been saved in ctrl_data, process this first.
    self() ! {tcp, Socket, <<>>}.

activate_data_connection(#state{dsock = Socket}) ->
    activate_connection(Socket).

activate_connection(Socket) ->
    inet:setopts(Socket, [{active, once}]).

close_ctrl_connection(#state{csock = undefined}) ->
    ok;
close_ctrl_connection(#state{csock = Socket}) ->
    close_connection(Socket).

close_data_connection(#state{dsock = undefined}) ->
    ok;
close_data_connection(#state{dsock = {lsock, Socket}}) ->
    close_connection(Socket);
close_data_connection(#state{dsock = Socket}) ->
    close_connection(Socket).

close_connection(Socket) ->
    gen_tcp:close(Socket).

%%  ------------ FILE HANDELING  ----------------------------------------   

send_file(Fd, State) ->
    case file_read(Fd) of
	{ok, N, Bin} when N > 0->
	    send_data_message(State, Bin),
	    progress_report({binary, Bin}, State),
	    send_file(Fd, State);
	{ok, _, _} ->
	    file_close(Fd),
	    close_data_connection(State),
	    progress_report({transfer_size, 0}, State),
	    activate_ctrl_connection(State),
	    {noreply, State#state{caller = transfer_file_second_phase,
				  dsock = undefined}};
        {error, Reason} ->
	    gen_server:reply(State#state.client, {error, Reason}),
	    {stop, normal, State#state{client = undefined}}
    end.

file_open(File, Option) ->
  file:open(File, [raw, binary, Option]).

file_close(Fd) ->
  file:close(Fd).

file_read(Fd) ->				
    case file:read(Fd, ?FILE_BUFSIZE) of
	{ok, Bytes} ->
	    {ok, size(Bytes), Bytes};
	eof ->
	    {ok, 0, []};
	Other ->
	    Other
    end.

file_write(Bytes, Fd) ->
    file:write(Fd, Bytes).

%% --------------  MISC ---------------------------------------------- 

call(GenServer, Msg, Format) ->
    call(GenServer, Msg, Format, infinity).
call(GenServer, Msg, Format, Timeout) ->   
    Req = {self(), Msg}, 
    case (catch gen_server:call(GenServer, Req, Timeout)) of
	{ok, Bin} when is_binary(Bin) andalso (Format =:= string) ->
	    {ok, binary_to_list(Bin)};
	{'EXIT', _} ->
	    {error, eclosed};
	Result ->
	    Result
    end.

cast(GenServer, Msg) ->
    gen_server:cast(GenServer, {self(), Msg}).

mk_cmd(Fmt, Args) ->
    [io_lib:format(Fmt, Args)| [?CR, ?LF]].		% Deep list ok.

pwd_result(Lines) ->
    {_, [?DOUBLE_QUOTE | Rest]} = 
	lists:splitwith(fun(?DOUBLE_QUOTE) -> false; (_) -> true end, Lines),
    {Dir, _} =
	lists:splitwith(fun(?DOUBLE_QUOTE) -> false; (_) -> true end, Rest),
    Dir.

%% is_verbose(Params) -> 
%%     check_param(verbose, Params).

%% is_debug(Flags) -> 
%%     check_param(debug, Flags).

%% is_trace(Flags) -> 
%%     check_param(trace, Flags).

%% is_ipv6_disabled(Flags) -> 
%%     check_param(ip_v6_disabled, Flags).

%% check_param(Param, Params) -> 
%%     lists:member(Param, Params).

key_search(Key, List, Default) ->	     
    case lists:keysearch(Key, 1, List) of
	{value, {_,Val}} ->
	    Val;
	false ->
	    Default
    end.

%% check_option(Pred, Value, Default) ->
%%     case Pred(Value) of
%% 	true ->
%% 	    Value;
%% 	false ->
%% 	    Default
%%     end.

verbose(Lines, true, Direction) ->
    DirStr =
	case Direction of
	    send ->
		"Sending: ";
	    _ ->
		"Receiving: "
	end,
    Str = string:strip(string:strip(Lines, right, ?LF), right, ?CR),
    erlang:display(DirStr++Str);
verbose(_, false,_) ->
    ok.

progress(Options) ->
    ftp_progress:start_link(Options).

progress_report(_, #state{progress = ignore}) ->
    ok;
progress_report(stop, #state{progress = ProgressPid}) ->
    ftp_progress:stop(ProgressPid);
progress_report({binary, Data}, #state{progress = ProgressPid}) ->
    ftp_progress:report(ProgressPid, {transfer_size, size(Data)});
progress_report(Report,  #state{progress = ProgressPid}) ->
    ftp_progress:report(ProgressPid, Report).


millisec_time() ->
    {A,B,C} = erlang:now(),
    A*1000000000+B*1000+(C div 1000).
