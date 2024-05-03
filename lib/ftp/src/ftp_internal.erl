%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2024. All Rights Reserved.
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

-module(ftp_internal).
-moduledoc false.

-behaviour(gen_server).

-export([start_service/1]).

-export([start_link/1, start_link/2]).

%%  API - Client interface
-export([cd/2, close/1, delete/2,
         lcd/2, lpwd/1, ls/2,
         mkdir/2, nlist/2,
         open/1, open/2,
         pwd/1, quote/2,
         recv/2, recv/3, recv_bin/2,
         recv_chunk_start/2, recv_chunk/1,
         rename/3, rmdir/2,
         send/2, send/3, send_bin/3,
         send_chunk_start/2, send_chunk/2, send_chunk_end/1,
         type/2, user/3, user/4, account/2,
         append/3, append_bin/3,
         append_chunk/2, append_chunk_end/1, append_chunk_start/2,
         info/1, latest_ctrl_response/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-include("ftp_internal.hrl").

-define(DBG(F,A), 'n/a').
%%-define(DBG(F,A), io:format(F,A)).
%%-define(DBG(F,A), ct:pal("~p:~p " ++ if is_list(F) -> F; is_atom(F) -> atom_to_list(F) end, [?MODULE,?LINE|A])).

%% Constants used in internal state definition
-define(CONNECTION_TIMEOUT,  60*1000).
-define(DATA_ACCEPT_TIMEOUT, infinity).
-define(DEFAULT_MODE,        passive).
-define(PROGRESS_DEFAULT,    ignore).
-define(FTP_EXT_DEFAULT,     false).

%% Internal Constants
-define(FTP_PORT, 21).
-define(FTPS_PORT, 990).
-define(FILE_BUFSIZE, 4096).


%%%=========================================================================
%%%  Data Types
%%%=========================================================================

%% Internal state
-record(state, {
          csock   = undefined :: undefined  % Control connection socket
                               | { tcp | ssl, inet:socket() | ssl:socket() | ssl:sslsocket()},
          dsock   = undefined :: undefined % Data connection socket
                               | { tcp | ssl | lsock, inet:socket() | ssl:socket() | ssl:sslsocket() | gen_tcp:socket()},
          tls_options = undefined :: undefined | [tuple()],
          verbose = false  :: boolean(),
          ldir    = undefined :: undefined | file:filename_all(), % Current local directory
          type    = ftp_server_default :: atom(), % binary | ascii
          chunk   = false :: boolean(), % Receiving data chunks
          mode    = ?DEFAULT_MODE :: active | passive,
          timeout = ?CONNECTION_TIMEOUT :: timeout(),
          %% Data received so far on the data connection
          data    = <<>> :: binary(),
          %% Data received so far on the control connection
          %% {BinStream, AccLines}. If a binary sequence
          %% ends with ?CR then keep it in the binary to
          %% be able to detect if the next received byte is ?LF
          %% and hence the end of the response is reached!
          ctrl_data = {<<>>, [], start} :: {binary(), [byte()], term()},
          %% pid() - Client pid (note not the same as "From")
          latest_ctrl_response = "" :: string(),
          owner = undefined :: undefined | term(),
          client = undefined :: undefined | gen_server:from(), % "From" to be used in gen_server:reply/2
          %% Function that activated a connection and maybe some
          %% data needed further on.
          caller = undefined :: term(),
          ipfamily :: inet:address_family() | inet6fb4,
          sockopts_ctrl = [] :: list(),
          sockopts_data_passive = [] :: list(),
          sockopts_data_active = []  :: list(),
          progress = ignore :: ignore | pid(),
          dtimeout = ?DATA_ACCEPT_TIMEOUT :: non_neg_integer() | infinity,
          tls_ctrl_session_reuse = false :: boolean(),
          tls_upgrading_data_connection = false :: boolean() | atom() | tuple(),
          ftp_extension = ?FTP_EXT_DEFAULT :: boolean()
         }).

-record(recv_chunk_closing, {
          dconn_closed = false,
          pos_compl_received = false,
          client_called_us = false
         }).


-type shortage_reason()  :: 'etnospc' | 'epnospc'.
-type restriction_reason() :: 'epath' | 'efnamena' | 'elogin' | 'enotbinary'.
-type common_reason() ::  'econn' | 'eclosed' | term().
-type file_write_error_reason() :: file:posix() | badarg | terminated. % See file:write for more info


%% This should be made an internal function when we remove the deprecation
%% ftp client processes should always be part of ftp supervisor tree.
%% We consider it a bug that the "standalone" concept of inets was
%% not removed when ftp was broken out, and it is now fixed.
-spec start_service(ServiceConfig) -> {ok, Pid} | {error, Reason} when
      ServiceConfig :: [{Property, Value}],
      Property :: proplists:property(),
      Value :: term(),
      Pid :: pid(),
      Reason :: term().
start_service(Options) ->
    try
        {ok, StartOptions} = start_options(Options),
        case ftp_sup:start_child([[[{client, self()} | StartOptions], []]]) of
            {ok, Pid} ->
                call(Pid, {open, ip_comm, Options}, plain);
            Error1 ->
                Error1
        end
    catch
        throw:Error2 ->
            Error2
    end.

-spec open(Host :: string() | inet:ip_address()) ->
    {'ok', Pid :: pid()} | {'error', Reason :: 'ehost' | term()}.

open(Host) ->
    open(Host, []).

-spec open(Host :: string() | inet:ip_address(), Opts) ->
    {'ok', Pid :: pid()} | {'error', Reason :: 'ehost' | term()} when
      Opts :: [Opt],
      Opt :: StartOption | OpenOption,
      StartOption :: {verbose, Verbose} | {debug, Debug},
      Verbose :: boolean(),
      Debug :: disable | debug | trace,
      OpenOption :: {ipfamily, IpFamily} | {port, Port :: port()} | {mode, Mode}
                    | {tls, TLSOptions :: [ssl:tls_option()]} | {tls_sec_method, TLSSecMethod :: ftps | ftpes}
                    | {tls_ctrl_session_reuse, TLSSessionReuse :: boolean() } | {timeout, Timeout :: timeout()}
                    | {dtimeout, DTimeout :: timeout()} | {progress, Progress} | {sock_ctrl, SocketCtrls}
                    | {sock_data_act, [SocketControl]} | {sock_data_pass, [SocketControl]},
      SocketCtrls :: [SocketControl],
      IpFamily :: inet | inet6 | inet6fb4,
      Mode :: active | passive,
      Module :: atom(),
      Function :: atom(),
      InitialData :: term(),
      Progress :: ignore | {Module, Function, InitialData},
      SocketControl :: gen_tcp:option().
%% <BACKWARD-COMPATIBILLITY>
open(Host, Port) when is_integer(Port) ->
    open(Host, [{port, Port}]);
%% </BACKWARD-COMPATIBILLITY>

open(Host, Options) when is_list(Options) ->
    start_service([{host,Host}|Options]).


%%--------------------------------------------------------------------------
%% Description:  Login with or without a supplied account name.
%%--------------------------------------------------------------------------
-spec user(Pid  :: pid(),
           User :: string(),
           Pass :: string()) ->
    'ok' | {'error', Reason :: 'euser' | common_reason()}.

user(Pid, User, Pass) ->
    case {is_name_sane(User), is_name_sane(Pass)} of
        {true, true} ->
            call(Pid, {user, User, Pass}, atom);
        _ ->
            {error, euser}
    end.

-spec user(Pid  :: pid(),
           User :: string(),
           Pass :: string(),
           Account  :: string()) ->
    'ok' | {'error', Reason :: 'euser' | common_reason()}.

user(Pid, User, Pass, Account) ->
    case {is_name_sane(User), is_name_sane(Pass), is_name_sane(Account)} of
        {true, true, true} ->
            call(Pid, {user, User, Pass, Account}, atom);
        _ ->
            {error, euser}
    end.


%%--------------------------------------------------------------------------
%% Description:  Set a user Account.
%%--------------------------------------------------------------------------

-spec account(Pid :: pid(), Acc :: string()) ->
    'ok' | {'error', Reason :: 'eacct' | common_reason()}.

account(Pid, Acc) ->
    case is_name_sane(Acc) of
        true ->
            call(Pid, {account, Acc}, atom);
        _ ->
            {error, eacct}
    end.


%%--------------------------------------------------------------------------
%% Description:  Get the current working directory at remote server.
%%--------------------------------------------------------------------------

-spec pwd(Pid :: pid()) ->
    {'ok', Dir :: string()} |
        {'error', Reason :: restriction_reason() | common_reason()}.

pwd(Pid) ->
    call(Pid, pwd, ctrl).


%%--------------------------------------------------------------------------
%% Description:  Get the current working directory at local server.
%%--------------------------------------------------------------------------

-spec lpwd(Pid :: pid()) ->
    {'ok', Dir :: string()}.

lpwd(Pid) ->
    call(Pid, lpwd, string).


%%--------------------------------------------------------------------------
%% Description:  Change current working directory at remote server.
%%--------------------------------------------------------------------------

-spec cd(Pid :: pid(), Dir :: string()) ->
    'ok' | {'error', Reason :: restriction_reason() | common_reason()}.

cd(Pid, Dir) ->
    case is_name_sane(Dir) of
        true ->
            call(Pid, {cd, Dir}, atom);
        _ ->
            {error, efnamena}
    end.


%%--------------------------------------------------------------------------
%% Description:  Change current working directory for the local client.
%%--------------------------------------------------------------------------

-spec lcd(Pid :: pid(), Dir :: string()) ->
    'ok' | {'error', Reason :: restriction_reason()}.

lcd(Pid, Dir) ->
    call(Pid, {lcd, Dir}, string).


%%--------------------------------------------------------------------------
%% Description: Returns a list of files in long format.
%%--------------------------------------------------------------------------

-spec ls(Pid :: pid(), Dir :: string()) ->
    {'ok', Listing :: string()} |
        {'error', Reason ::  restriction_reason() | common_reason()}.

ls(Pid, Dir) ->
    case is_name_sane(Dir) of
        true ->
            call(Pid, {dir, long, Dir}, string);
        _ ->
            {error, efnamena}
    end.


%%--------------------------------------------------------------------------
%% Description:  Returns a list of files in short format
%%--------------------------------------------------------------------------

-spec nlist(Pid :: pid(), Pathname :: string()) ->
    {'ok', Listing :: string()} |
        {'error', Reason :: restriction_reason() | common_reason()}.

nlist(Pid, Dir) ->
    case is_name_sane(Dir) of
        true ->
            call(Pid, {dir, short, Dir}, string);
        _ ->
            {error, efnamena}
    end.


%%--------------------------------------------------------------------------
%% Description:  Rename a file at remote server.
%%--------------------------------------------------------------------------

-spec rename(Pid :: pid(), Old :: string(), New :: string()) ->
    'ok' | {'error', Reason :: restriction_reason() | common_reason()}.

rename(Pid, Old, New) ->
    case {is_name_sane(Old), is_name_sane(New)} of
        {true, true} ->
            call(Pid, {rename, Old, New}, string);
        _ ->
            {error, efnamena}
    end.


%%--------------------------------------------------------------------------
%% Description:  Remove file at remote server.
%%--------------------------------------------------------------------------

-spec delete(Pid :: pid(), File :: string()) ->
    'ok' | {'error', Reason :: restriction_reason() | common_reason()}.

delete(Pid, File) ->
    case is_name_sane(File) of
        true ->
            call(Pid, {delete, File}, string);
        _ ->
            {error, efnamena}
    end.


%%--------------------------------------------------------------------------
%% Description:  Make directory at remote server.
%%--------------------------------------------------------------------------

-spec mkdir(Pid :: pid(), Dir :: string()) ->
    'ok' | {'error', Reason :: restriction_reason() | common_reason()}.

mkdir(Pid, Dir) ->
    case is_name_sane(Dir) of
        true ->
            call(Pid, {mkdir, Dir}, atom);
        _ ->
            {error, efnamena}
    end.


%%--------------------------------------------------------------------------
%% Description:  Remove directory at remote server.
%%--------------------------------------------------------------------------

-spec rmdir(Pid :: pid(), Dir :: string()) ->
    'ok' | {'error', Reason :: restriction_reason() | common_reason()}.

rmdir(Pid, Dir) ->
    case is_name_sane(Dir) of
        true ->
            call(Pid, {rmdir, Dir}, atom);
        _ ->
            {error, efnamena}
    end.


%%--------------------------------------------------------------------------
%% Description:  Set transfer type.
%%--------------------------------------------------------------------------

-spec type(Pid :: pid(), Type :: ascii | binary) ->
    'ok' |
        {'error', Reason :: 'etype' | restriction_reason() | common_reason()}.

type(Pid, Type) ->
    call(Pid, {type, Type}, atom).


%%--------------------------------------------------------------------------
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
    case is_name_sane(RemotFileName) of
        true ->
            call(Pid, {recv, RemotFileName, LocalFileName}, atom);
        _ ->
            {error, efnamena}
    end.


%%--------------------------------------------------------------------------
%% Description:  Transfer file from remote server into binary.
%%--------------------------------------------------------------------------

-spec recv_bin(Pid        :: pid(),
               RemoteFile :: string()) ->
    {'ok', Bin :: binary()} |
        {'error', Reason :: restriction_reason() | common_reason()}.

recv_bin(Pid, RemoteFile) ->
    case is_name_sane(RemoteFile) of
        true ->
            call(Pid, {recv_bin, RemoteFile}, bin);
        _ ->
            {error, efnamena}
    end.


%%--------------------------------------------------------------------------
%% Description:  Start receive of chunks of remote file.
%%--------------------------------------------------------------------------

-spec recv_chunk_start(Pid        :: pid(),
                       RemoteFile :: string()) ->
    'ok' | {'error', Reason :: restriction_reason() | common_reason()}.

recv_chunk_start(Pid, RemoteFile) ->
    case is_name_sane(RemoteFile) of
        true ->
            call(Pid, {recv_chunk_start, RemoteFile}, atom);
        _ ->
            {error, efnamena}
    end.


%%--------------------------------------------------------------------------
%% Description:  Transfer file from remote server into binary in chunks
%%--------------------------------------------------------------------------

-spec recv_chunk(Pid :: pid()) ->
    'ok' |
        {'ok', Bin :: binary()} |
        {'error', Reason :: restriction_reason() | common_reason()}.

recv_chunk(Pid) ->
    call(Pid, recv_chunk, atom).


%%--------------------------------------------------------------------------
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
    case is_name_sane(RemotFileName) of
        true ->
            call(Pid, {send, LocalFileName, RemotFileName}, atom);
        _ ->
            {error, efnamena}
    end.


%%--------------------------------------------------------------------------
%% Description:  Transfer a binary to a remote file.
%%--------------------------------------------------------------------------

-spec send_bin(Pid :: pid(), Bin :: binary(), RemoteFile :: string()) ->
    'ok' |
        {'error', Reason :: restriction_reason() |
                            common_reason() |
                            shortage_reason()}.

send_bin(Pid, Bin, RemoteFile) when is_binary(Bin) ->
    case is_name_sane(RemoteFile) of
        true ->
            call(Pid, {send_bin, Bin, RemoteFile}, atom);
        _ ->
            {error, efnamena}
    end;
send_bin(_Pid, _Bin, _RemoteFile) ->
  {error, enotbinary}.


%%--------------------------------------------------------------------------
%% Description:  Start transfer of chunks to remote file.
%%--------------------------------------------------------------------------

-spec send_chunk_start(Pid :: pid(), RemoteFile :: string()) ->
    'ok' | {'error', Reason :: restriction_reason() | common_reason()}.

send_chunk_start(Pid, RemoteFile) ->
    case is_name_sane(RemoteFile) of
        true ->
            call(Pid, {send_chunk_start, RemoteFile}, atom);
        _ ->
            {error, efnamena}
    end.


%%--------------------------------------------------------------------------
%% Description:  Start append chunks of data to remote file.
%%--------------------------------------------------------------------------

-spec append_chunk_start(Pid :: pid(), RemoteFile :: string()) ->
    'ok' | {'error', Reason :: restriction_reason() | common_reason()}.

append_chunk_start(Pid, RemoteFile) ->
    case is_name_sane(RemoteFile) of
        true ->
            call(Pid, {append_chunk_start, RemoteFile}, atom);
        _ ->
            {error, efnamena}
    end.


%%--------------------------------------------------------------------------
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
%% Description:  End appending of chunks to remote file.
%%--------------------------------------------------------------------------

-spec append_chunk_end(Pid :: pid()) ->
    'ok' |
        {'error', Reason :: echunk |
                            restriction_reason() |
                            common_reason() |
                            shortage_reason()}.

append_chunk_end(Pid) ->
    call(Pid, chunk_end, atom).


%%--------------------------------------------------------------------------
%% Description:  Append the local file to the remote file
%%--------------------------------------------------------------------------

-spec append(Pid            :: pid(),
             LocalFileName  :: string(),
             RemoteFileName :: string()) ->
    'ok' | {'error', Reason} when
      Reason :: epath | elogin | etnospc | epnospc | efnamena | common_reason().

append(Pid, LocalFileName, RemotFileName) ->
    case is_name_sane(RemotFileName) of
        true ->
            call(Pid, {append, LocalFileName, RemotFileName}, atom);
        _ ->
            {error, efnamena}
    end.


%%--------------------------------------------------------------------------
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
    case is_name_sane(RemoteFile) of
        true ->
            call(Pid, {append_bin, Bin, RemoteFile}, atom);
        _ ->
            {error, efnamena}
    end;
append_bin(_Pid, _Bin, _RemoteFile) ->
    {error, enotbinary}.


%%--------------------------------------------------------------------------
%% Description: Send arbitrary ftp command.
%%--------------------------------------------------------------------------

-spec quote(Pid :: pid(), Cmd :: string()) -> [FTPLine :: string()].

quote(Pid, Cmd) when is_list(Cmd) ->
    call(Pid, {quote, Cmd}, atom).


%%--------------------------------------------------------------------------
%% Description:  End the ftp session.
%%--------------------------------------------------------------------------

-spec close(Pid :: pid()) -> 'ok'.
close(Pid) ->
    cast(Pid, close),
    ok.


%%--------------------------------------------------------------------------
%% Description:  Return diagnostics.
%%--------------------------------------------------------------------------

info(Pid) ->
    call(Pid, info, list).


%%--------------------------------------------------------------------------
%% Description:  The latest received response from the server
%%--------------------------------------------------------------------------

-spec latest_ctrl_response(Pid :: pid()) -> string().

latest_ctrl_response(Pid) ->
    call(Pid, latest_ctrl_response, string).


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
    _ = inet_db:start(),

    %% Where are we
    {ok, Dir} = file:get_cwd(),

    %% Maybe activate dbg
    case key_search(debug, Options, disable) of
        trace ->
            _ = dbg:tracer(),
            _ = dbg:p(all, [call]),
            {ok, _} = dbg:tpl(ftp_internal, [{'_', [], [{return_trace}]}]),
            {ok, _} = dbg:tpl(ftp_response, [{'_', [], [{return_trace}]}]),
            {ok, _} = dbg:tpl(ftp_progress, [{'_', [], [{return_trace}]}]),
            ok;
        debug ->
            _ = dbg:tracer(),
            _ = dbg:p(all, [call]),
            {ok, _} = dbg:tp(ftp_internal, [{'_', [], [{return_trace}]}]),
            {ok, _} = dbg:tp(ftp_response, [{'_', [], [{return_trace}]}]),
            {ok, _} = dbg:tp(ftp_progress, [{'_', [], [{return_trace}]}]),
            ok;
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
    {ok, {_, LocalPort}}  = sockname(Socket),
    {ok, {Address, Port}} = peername(Socket),
    Options = [{verbose,    Verbose},
               {ipfamily,   IpFamily},
               {mode,       Mode},
               {peer,       Address},
               {peer_port,  Port},
               {local_port, LocalPort},
               {timeout,    Timeout},
               {progress,   Progress}],
    {reply, {ok, Options}, State};

handle_call({_,latest_ctrl_response}, _, #state{latest_ctrl_response=Resp} = State) ->
    {reply, {ok,Resp}, State};

%% But everything else must come from the owner
handle_call({Pid, _}, _, #state{owner = Owner} = State) when Owner =/= Pid ->
    {reply, {error, not_connection_owner}, State};

handle_call({_, {open, ip_comm, Options}}, From, State) ->
    {ok, Opts} = open_options(Options),

    case key_search(host, Opts, undefined) of
        undefined ->
            {stop, normal, {error, ehost}, State};
        Host ->
            TLSSecMethod = key_search(tls_sec_method, Opts, undefined),
            TLSOpts  = key_search(tls,      Opts, undefined),
            TLSReuse = key_search(tls_ctrl_session_reuse, Opts, false),
            Mode     = key_search(mode,     Opts, ?DEFAULT_MODE),
            Port0    = key_search(port,     Opts, 0),
            Port     = if Port0 == 0, TLSSecMethod == ftps -> ?FTPS_PORT; Port0 == 0 -> ?FTP_PORT; true -> Port0 end,
            Timeout  = key_search(timeout,  Opts, ?CONNECTION_TIMEOUT),
            DTimeout = key_search(dtimeout, Opts, ?DATA_ACCEPT_TIMEOUT),
            Progress = key_search(progress, Opts, ignore),
            IpFamily = key_search(ipfamily, Opts, inet),
            FtpExt   = key_search(ftp_extension, Opts, ?FTP_EXT_DEFAULT),

            {ok, {CtrlOpts, DataPassOpts, DataActOpts}} = socket_options(Options),

            State2 = State#state{client   = From,
                                 mode     = Mode,
                                 progress = progress(Progress),
                                 ipfamily = IpFamily,
                                 sockopts_ctrl = CtrlOpts,
                                 sockopts_data_passive =  DataPassOpts,
                                 sockopts_data_active = DataActOpts,
                                 timeout = Timeout,
                                 dtimeout = DTimeout,
                                 ftp_extension = FtpExt},

            case setup_ctrl_connection(Host, Port, Timeout, State2) of
                {ok, State3, WaitTimeout} when is_list(TLSOpts), TLSSecMethod == ftps ->
                    handle_ctrl_result({tls_upgrade, TLSSecMethod},
                                       State3#state{tls_options = TLSOpts,
						    tls_ctrl_session_reuse = TLSReuse,
						    timeout = WaitTimeout });
                {ok, State3, WaitTimeout} when is_list(TLSOpts) ->
                    {noreply, State3#state{tls_options = TLSOpts, tls_ctrl_session_reuse = TLSReuse }, WaitTimeout};
                {ok, State3, WaitTimeout} ->
                    {noreply, State3, WaitTimeout};
                {error, _Reason} ->
                    gen_server:reply(From, {error, ehost}),
                    {stop, normal, State2#state{client = undefined}}
            end
    end;

handle_call({_, {user, User, Password}}, From,
            #state{csock = CSock} = State) when (CSock =/= undefined) ->
    handle_user(User, Password, "", State#state{client = From});

handle_call({_, {user, User, Password, Acc}}, From,
            #state{csock = CSock} = State) when (CSock =/= undefined) ->
    handle_user(User, Password, Acc, State#state{client = From});

handle_call({_, {account, Acc}}, From, State)->
    handle_user_account(Acc, State#state{client = From});

handle_call({_, pwd}, From, #state{chunk = false} = State0) ->
    _ = send_ctrl_message(State0, mk_cmd("PWD", [])),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{client = From, caller = pwd}};

handle_call({_, lpwd}, From, #state{ldir = LDir} = State) ->
    {reply, {ok, LDir}, State#state{client = From}};

handle_call({_, {cd, Dir}}, From, #state{chunk = false} = State0) ->
    _ = send_ctrl_message(State0, mk_cmd("CWD ~s", [Dir])),
    State = activate_ctrl_connection(State0),
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
            #state{chunk = false} = State0) ->
    _ = send_ctrl_message(State0, mk_cmd("RNFR ~s", [CurrFile])),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{caller = {rename, NewFile}, client = From}};

handle_call({_, {delete, File}}, {_Pid, _} = From,
            #state{chunk = false} = State0) ->
    _ = send_ctrl_message(State0, mk_cmd("DELE ~s", [File])),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{client = From}};

handle_call({_, {mkdir, Dir}}, From, #state{chunk = false} = State0) ->
    _ = send_ctrl_message(State0, mk_cmd("MKD ~s", [Dir])),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{client = From}};

handle_call({_,{rmdir, Dir}}, From, #state{chunk = false} = State0) ->
    _ = send_ctrl_message(State0, mk_cmd("RMD ~s", [Dir])),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{client = From}};

handle_call({_,{type, Type}}, From, #state{chunk = false} = State0) ->
    case Type of
        ascii ->
            _ = send_ctrl_message(State0, mk_cmd("TYPE A", [])),
            State = activate_ctrl_connection(State0),
            {noreply, State#state{caller = type, type = ascii,
                                  client = From}};
        binary ->
            _ = send_ctrl_message(State0, mk_cmd("TYPE I", [])),
            State = activate_ctrl_connection(State0),
            {noreply, State#state{caller = type, type = binary,
                                  client = From}};
        _ ->
            {reply, {error, etype}, State0}
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
handle_call({_, recv_chunk}, _From, #state{chunk = true,
                                           data = Bin,
                                           caller = #recv_chunk_closing{dconn_closed       = true,
                                                                        pos_compl_received = true,
                                                                        client_called_us = true
                                                                       }
                                          } = State0) ->
    case Bin of
        <<>> ->
            {reply, ok, State0#state{caller = undefined,
                                     chunk = false,
                                     client = undefined}};
        Data ->
            {reply, Data, State0#state{caller = undefined,
                                       chunk = false,
                                       client = undefined}}
    end;
handle_call({_, recv_chunk}, _From, #state{chunk = true,
                                           caller = #recv_chunk_closing{dconn_closed       = true,
                                                                        pos_compl_received = true
                                                                       }
                                          } = State0) ->
    %% The ftp:recv_chunk call was the last event we waited for, finnish and clean up
    ?DBG("Data connection closed recv_chunk_closing ftp:recv_chunk, last event",[]),
    State = activate_ctrl_connection(State0),
    {reply, ok, State#state{caller = undefined,
                             chunk = false,
                             client = undefined}};
handle_call({_, recv_chunk}, From, #state{chunk = true,
                                          caller = #recv_chunk_closing{pos_compl_received = true
                                                                      } = R
                                         } = State0) ->
    State = activate_data_connection(State0),
    {noreply, State#state{client = From, caller = R#recv_chunk_closing{client_called_us=true}}};

handle_call({_, recv_chunk}, From, #state{chunk = true,
                                          caller = #recv_chunk_closing{} = R
                                         } = State) ->
    %% Waiting for more, don't care what
    ?DBG("recv_chunk_closing ftp:recv_chunk, get more",[]),
    {noreply, State#state{client = From, caller = R#recv_chunk_closing{client_called_us=true}}};

handle_call({_, recv_chunk}, From, #state{chunk = true} = State0) ->
    State = activate_data_connection(State0),
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

handle_call({_, {transfer_chunk, _}}, _, #state{chunk = false} = State) ->
    {reply, {error, echunk}, State};

handle_call({_, chunk_end}, From, #state{chunk = true} = State0) ->
    close_data_connection(State0),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{client = From, dsock = undefined,
                          caller = end_chunk_transfer, chunk = false}};

handle_call({_, chunk_end}, _, #state{chunk = false} = State) ->
    {reply, {error, echunk}, State};

handle_call({_, {quote, Cmd}}, From, #state{chunk = false} = State0) ->
    _ = send_ctrl_message(State0, mk_cmd(Cmd, [])),
    State = activate_ctrl_connection(State0),
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
    _ = send_ctrl_message(State, mk_cmd("QUIT", [])),
    close_ctrl_connection(State),
    close_data_connection(State),
    {stop, normal, State#state{csock = undefined, dsock = undefined}};

handle_cast({Pid, close}, State) ->
    Report = io_lib:format("A none owner process ~p tried to close an "
                             "ftp connection: ~n", [Pid]),
    error_logger:info_report(Report),
    {noreply, State};

%% Catch all -  This can only happen if the application programmer writes
%% really bad code that violates the API.
handle_cast(Msg, State) ->
  {stop, {'API_violation_connection_closed', Msg}, State}.

%%--------------------------------------------------------------------------
%% handle_info(Msg, State) -> {noreply, State} | {noreply, State, Timeout} |
%%                              {stop, Reason, State}
%% Description: Handles tcp messages from the ftp-server.
%% Note: The order of the function clauses is significant.
%%--------------------------------------------------------------------------

handle_info(timeout, #state{caller = open} = State) ->
    {stop, timeout, State};

handle_info(timeout, State) ->
    {noreply, State};

%%% Data socket messages %%%
handle_info({Trpt, Socket, Data},
            #state{dsock = {Trpt,Socket},
                   caller = {recv_file, Fd}} = State0) when Trpt==tcp;Trpt==ssl ->
    ?DBG('L~p --data ~p ----> ~s~p~n',[?LINE,Socket,Data,State0]),
    ok = file_write(binary_to_list(Data), Fd),
    progress_report({binary, Data}, State0),
    State = activate_data_connection(State0),
    {noreply, State};

handle_info({Trpt, Socket, Data}, #state{dsock = {Trpt,Socket}, client = From,
                                        caller = recv_chunk}
            = State) when Trpt==tcp;Trpt==ssl ->
    ?DBG('L~p --data ~p ----> ~s~p~n',[?LINE,Socket,Data,State]),
    gen_server:reply(From, {ok, Data}),
    {noreply, State#state{client = undefined, caller = undefined, data = <<>>}};

handle_info({Trpt, Socket, Data}, #state{dsock = {Trpt,Socket}} = State0) when Trpt==tcp;Trpt==ssl ->
    ?DBG('L~p --data ~p ----> ~s~p~n',[?LINE,Socket,Data,State0]),
    State = activate_data_connection(State0),
    {noreply, State#state{data = <<(State#state.data)/binary,
                                  Data/binary>>}};

handle_info({Cls, Socket}, #state{dsock = {Trpt,Socket},
                                  caller = {recv_file, Fd}} = State0)
  when {Cls,Trpt}=={tcp_closed,tcp} ; {Cls,Trpt}=={ssl_closed,ssl} ->
    file_close(Fd),
    progress_report({transfer_size, 0}, State0),
    State = activate_ctrl_connection(State0),
    ?DBG("Data channel close",[]),
    {noreply, State#state{dsock = undefined, data = <<>>}};

handle_info({Cls, Socket}, #state{dsock = {Trpt,Socket},
                                  client = Client,
                                  caller = recv_chunk} = State0)
  when {Cls,Trpt}=={tcp_closed,tcp} ; {Cls,Trpt}=={ssl_closed,ssl} ->
    ?DBG("Data channel close recv_chunk",[]),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{dsock = undefined,
                          caller = #recv_chunk_closing{dconn_closed     =  true,
                                                       client_called_us =  Client =/= undefined}
                         }};
handle_info({Cls, Socket}, #state{dsock = {Trpt,Socket},
                                  caller = #recv_chunk_closing{client_called_us = true,
                                                               pos_compl_received = true} = R} = State)
  when {Cls,Trpt}=={tcp_closed,tcp} ; {Cls,Trpt}=={ssl_closed,ssl} ->
    %% Maybe handle unprocessed chunk message before acking final chunk
    self() ! {Cls, Socket},
    {noreply, State#state{caller = R#recv_chunk_closing{dconn_closed = true}}};

handle_info({Cls, Socket}, #state{dsock = {Trpt,Socket}, caller = recv_bin,
                                         data = Data} = State0)
  when {Cls,Trpt}=={tcp_closed,tcp} ; {Cls,Trpt}=={ssl_closed,ssl} ->
    ?DBG("Data channel close",[]),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{dsock = undefined, data = <<>>,
                          caller = {recv_bin, Data}}};

handle_info({Cls, Socket}, #state{dsock = {Trpt,Socket}, data = Data,
                                  caller = {handle_dir_result, Dir}}
            = State0) when {Cls,Trpt}=={tcp_closed,tcp} ; {Cls,Trpt}=={ssl_closed,ssl} ->
    ?DBG("Data channel close",[]),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{dsock = undefined,
                          caller = {handle_dir_result, Dir, Data},
%                          data = <<?CR,?LF>>}};
                          data = <<>>}};

handle_info({Err, Socket, Reason}, #state{dsock = {Trpt,Socket},
                                          client = From} = State)
  when {Err,Trpt}=={tcp_error,tcp} ; {Err,Trpt}=={ssl_error,ssl} ->
    gen_server:reply(From, {error, Reason}),
    close_data_connection(State),
    {noreply, State#state{dsock = undefined, client = undefined,
                          data = <<>>, caller = undefined, chunk = false}};

%%% Ctrl socket messages %%%
handle_info({Transport, Socket, Data}, #state{csock = {Transport, Socket},
                                              verbose = Verbose,
                                              caller = Caller,
                                              client = From,
                                              ctrl_data = {BinCtrlData, AccLines,
                                                           LineStatus}}
            = State0) ->
    ?DBG('--ctrl ~p ----> ~s~p~n',[Socket,<<BinCtrlData/binary, Data/binary>>,State0]),
    case ftp_response:parse_lines(<<BinCtrlData/binary, Data/binary>>,
                                  AccLines, LineStatus) of
        {ok, Lines, NextMsgData} ->
            verbose(Lines, Verbose, 'receive'),
            CtrlResult = ftp_response:interpret(Lines),
            case Caller of
                quote ->
                    gen_server:reply(From, string:tokens(Lines, [?CR, ?LF])),
                    {noreply, State0#state{client = undefined,
                                           caller = undefined,
                                           latest_ctrl_response = Lines,
                                           ctrl_data = {NextMsgData, [],
                                                        start}}};
                _ ->
                    ?DBG('   ...handle_ctrl_result(~p,...) ctrl_data=~p~n',[CtrlResult,{NextMsgData, [], start}]),
                    handle_ctrl_result(CtrlResult,
                                       State0#state{latest_ctrl_response = Lines,
                                                    ctrl_data =
                                                        {NextMsgData, [], start}})
            end;
        {continue, CtrlData} when CtrlData =/= State0#state.ctrl_data ->
            ?DBG('   ...Continue... ctrl_data=~p~n',[CtrlData]),
            State1 = State0#state{ctrl_data = CtrlData},
            State = activate_ctrl_connection(State1),
            {noreply, State};
        {continue, _CtrlData} ->
            ?DBG('   ...Continue... ctrl_data=~p~n',[_CtrlData]),
            {noreply, State0}
    end;

%% If the server closes the control channel it is
%% the expected behavior that connection process terminates.
handle_info({Cls, Socket}, #state{csock = {Trpt, Socket}})
  when {Cls,Trpt}=={tcp_closed,tcp} ; {Cls,Trpt}=={ssl_closed,ssl} ->
    exit(normal); %% User will get error message from terminate/2

handle_info({Err, Socket, Reason}, _) when Err==tcp_error ; Err==ssl_error ->
    Report =
        io_lib:format("~p on socket: ~p  for reason: ~p~n",
                      [Err, Socket, Reason]),
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
                           [Reason]),
    error_logger:info_report(Report),
    {noreply, State#state{progress = ignore}};

%% Catch all - throws away unknown messages (This could happen by "accident"
%% so we do not want to crash, but we make a log entry as it is an
%% unwanted behaviour.)
handle_info(Info, State) ->
    Report = io_lib:format("ftp : ~p : Unexpected message: ~p~nState: ~p~n",
                           [self(), Info, State]),
    error_logger:info_report(Report),
    {noreply, State}.

%%--------------------------------------------------------------------------
%% terminate/2 and code_change/3
%%--------------------------------------------------------------------------
terminate(normal, State) ->
    %% If terminate reason =/= normal the progress reporting process will
    %% be killed by the exit signal.
    progress_report(stop, State),
    do_terminate({error, econn}, State);
terminate(Reason, State) ->
    Report = io_lib:format("Ftp connection closed due to: ~p~n", [Reason]),
    error_logger:error_report(Report),
    do_terminate({error, eclosed}, State).

do_terminate(ErrorMsg, State) ->
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
%%            : when open or legacy is called.
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
-spec handle_user(User, Password, Account, State) -> Result when
      User     :: io:format(),
      Password :: io:format(),
      Account  :: io:format(),
      State    :: #state{},
      Result   :: {noreply, #state{}}.
handle_user(User, Password, Acc, State0) ->
    _ = send_ctrl_message(State0, mk_cmd("USER ~s", [User])),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{caller = {handle_user, Password, Acc}}}.

handle_user_passwd(Password, Acc, State0) ->
    _ = send_ctrl_message(State0, mk_cmd("PASS ~s", [Password])),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{caller = {handle_user_passwd, Acc}}}.

handle_user_account(Acc, State0) ->
    _ = send_ctrl_message(State0, mk_cmd("ACCT ~s", [Acc])),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{caller = handle_user_account}}.


%%--------------------------------------------------------------------------
%% handle_ctrl_result
%%--------------------------------------------------------------------------
-type ctrl_status_operation() :: efnamena
                               | elogin
                               | enofile
                               | epath
                               | error
                               | etnospc
                               | epnospc
                               | efnamena
                               | econn
                               | perm_neg_compl
                               | pos_compl
                               | pos_interm
                               | pos_interm_acct
                               | pos_prel
                               | tls_upgrade
                               | trans_neg_compl.

-spec handle_ctrl_result(Operation, State) -> Result when
      Operation :: {ctrl_status_operation(), atom() | string()},
      State     :: #state{},
      Result    :: {noreply, #state{}, integer()}
                 | {noreply, #state{}}
                 | {stop, normal | {error, Reason}, #state{}}
                 | {error, term()},
      Reason    :: term().
handle_ctrl_result({pos_compl, _}, #state{csock = {tcp, _Socket},
                                          tls_options = TLSOptions,
                                          timeout = Timeout,
                                          caller = open}
                   = State0) when is_list(TLSOptions) ->
    _ = send_ctrl_message(State0, mk_cmd("AUTH TLS", [])),
    State = activate_ctrl_connection(State0),
    {noreply, State, Timeout};

handle_ctrl_result({tls_upgrade, S}, #state{csock = {tcp, Socket},
                                            tls_options = TLSOptions,
                                            timeout = Timeout,
                                            caller = open, client = From}
                   = State0) when is_list(TLSOptions) ->
    ?DBG('<--ctrl ssl:connect(~p, ~p)~n~p~n',[Socket,TLSOptions,State0]),
    catch ssl:start(),
    case ssl:connect(Socket, TLSOptions, Timeout) of
        {ok, TLSSocket} when S == ftps ->
            State1 = State0#state{csock = {ssl,TLSSocket}},
            State = activate_ctrl_connection(State1),
            {noreply, State#state{tls_upgrading_data_connection = pending}, Timeout};
        {ok, TLSSocket} ->
            State1 = State0#state{csock = {ssl,TLSSocket}},
            handle_ctrl_result({pos_compl, S}, State1#state{tls_upgrading_data_connection = pending});
        {error, _} = Error ->
            gen_server:reply(From, Error),
            {stop, normal, State0#state{client = undefined,
                                        caller = undefined,
                                        tls_upgrading_data_connection = false}}
    end;

handle_ctrl_result({pos_compl, _}, #state{tls_upgrading_data_connection = pending} = State0) ->
    _ = send_ctrl_message(State0, mk_cmd("PBSZ 0", [])),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{tls_upgrading_data_connection = {true, pbsz}}};

handle_ctrl_result({pos_compl, _}, #state{tls_upgrading_data_connection = {true, pbsz}} = State0) ->
    _ = send_ctrl_message(State0, mk_cmd("PROT P", [])),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{tls_upgrading_data_connection = {true, prot}}};

handle_ctrl_result({pos_compl, _}, #state{tls_upgrading_data_connection = {true, prot},
                                          client = From} = State) ->
    gen_server:reply(From, {ok, self()}),
    {noreply, State#state{client = undefined,
                          caller = undefined,
                          tls_upgrading_data_connection = false}};
handle_ctrl_result({pos_compl, _}, #state{caller = open, client = From}
                   = State) ->
    gen_server:reply(From, {ok, self()}),
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

handle_ctrl_result({Status, _Lines},
                   #state{mode   = active,
                          caller = {setup_data_connection, {LSock, _}}}
                   = State) ->
    close_connection({tcp,LSock}),
    ctrl_result_response(Status, State, {error, Status});

%% Data connection setup passive mode
handle_ctrl_result({pos_compl, Lines},
                   #state{mode     = passive,
                          ipfamily = inet6,
                          client   = From,
                          caller   = {setup_data_connection, Caller},
                          csock    = CSock,
                          sockopts_data_passive = SockOpts,
                          timeout  = Timeout}
                   = State) when is_list(Lines) ->
    [_, PortStr | _] =  lists:reverse(string:tokens(Lines, "|")),
    {ok, {IP, _}} = peername(CSock),
    case connect(IP, list_to_integer(PortStr), SockOpts, Timeout, State) of
        {ok, _, Socket} ->
            handle_caller(State#state{caller = Caller, dsock = {tcp, Socket}});
        {error, _Reason} = Error ->
            gen_server:reply(From, Error),
            {noreply, State#state{client = undefined, caller = undefined}}
    end;

handle_ctrl_result({pos_compl, Lines},
                   #state{mode     = passive,
                          ipfamily = inet,
                          client   = From,
                          caller   = {setup_data_connection, Caller},
                          timeout  = Timeout,
                          sockopts_data_passive = SockOpts,
                          ftp_extension = false} = State) when is_list(Lines) ->

    {_, [?LEFT_PAREN | Rest]} =
        lists:splitwith(fun(?LEFT_PAREN) -> false; (_) -> true end, Lines),
    {NewPortAddr, _} =
        lists:splitwith(fun(?RIGHT_PAREN) -> false; (_) -> true end, Rest),
    [A1, A2, A3, A4, P1, P2] =
        lists:map(fun(X) -> list_to_integer(X) end,
                  string:tokens(NewPortAddr, [$,])),
    IP   = {A1, A2, A3, A4},
    Port = (P1 * 256) + P2,

    ?DBG('<--data tcp connect to ~p:~p, Caller=~p~n',[IP,Port,Caller]),
    case connect(IP, Port, SockOpts, Timeout, State) of
        {ok, _, Socket}  ->
            handle_caller(State#state{caller = Caller, dsock = {tcp,Socket}});
        {error, _Reason} = Error ->
            gen_server:reply(From, Error),
            {noreply,State#state{client = undefined, caller = undefined}}
    end;

handle_ctrl_result({pos_compl, Lines},
                   #state{mode     = passive,
                          ipfamily = inet,
                          client   = From,
                          caller   = {setup_data_connection, Caller},
                          csock    = CSock,
                          timeout  = Timeout,
                          sockopts_data_passive = SockOpts,
                          ftp_extension = true} = State) when is_list(Lines) ->

    [_, PortStr | _] =  lists:reverse(string:tokens(Lines, "|")),
    {ok, {IP, _}} = peername(CSock),

    ?DBG('<--data tcp connect to ~p:~p, Caller=~p~n',[IP,PortStr,Caller]),
        case connect(IP, list_to_integer(PortStr), SockOpts, Timeout, State) of
                {ok, _, Socket} ->
                    handle_caller(State#state{caller = Caller, dsock = {tcp, Socket}});
                {error, _Reason} = Error ->
                    gen_server:reply(From, Error),
                    {noreply, State#state{client = undefined, caller = undefined}}
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
handle_ctrl_result({pos_prel, _}, #state{caller = {dir, Dir}} = State0) ->
    case accept_data_connection(State0) of
        {ok, State1} ->
            State = activate_data_connection(State1),
            {noreply, State#state{caller = {handle_dir_result, Dir}}};
        {error, _Reason} = Error ->
            ctrl_result_response(error, State0, Error)
    end;

handle_ctrl_result({pos_compl, _}, #state{caller = {handle_dir_result, ""=_CurrentDir,
                                                    Data}, client = From}= State) ->
    gen_server:reply(From, {ok, Data}),
    {noreply, State#state{client = undefined,
                          caller = undefined}};

handle_ctrl_result({pos_compl, _}, #state{caller = {handle_dir_result, _Dir,
                                                    Data}, client = From}= State) ->
    gen_server:reply(From, {ok, Data}),
    {noreply, State#state{client = undefined,
                          caller = undefined}};

handle_ctrl_result({pos_compl, _}=Operation, #state{caller = {handle_dir_result, Dir},
                                                    data   = Data}= State) ->
    handle_ctrl_result(Operation, State#state{caller = {handle_dir_result, Dir, Data}});

handle_ctrl_result(S={_Status, _},
                   #state{caller = {handle_dir_result, _, _}} = State) ->
    %% OTP-5731, macosx
    ctrl_result_response(S, State, {error, epath});

handle_ctrl_result({Status, _}, #state{caller = cd} = State) ->
    ctrl_result_response(Status, State, {error, Status});

handle_ctrl_result(Status={epath, _}, #state{caller = {dir,_}} = State) ->
     ctrl_result_response(Status, State, {error, epath});

%%--------------------------------------------------------------------------
%% File renaming
handle_ctrl_result({pos_interm, _}, #state{caller = {rename, NewFile}}
                   = State0) ->
    _ = send_ctrl_message(State0, mk_cmd("RNTO ~s", [NewFile])),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{caller = rename_second_phase}};

handle_ctrl_result({Status, _},
                   #state{caller = {rename, _}} = State) ->
    ctrl_result_response(Status, State, {error, Status});

handle_ctrl_result({Status, _},
                   #state{caller = rename_second_phase} = State) ->
    ctrl_result_response(Status, State, {error, Status});

%%--------------------------------------------------------------------------
%% File handling - recv_bin
handle_ctrl_result({pos_prel, _}, #state{caller = recv_bin} = State0) ->
    case accept_data_connection(State0) of
        {ok, State1} ->
            State = activate_data_connection(State1),
            {noreply, State};
        {error, _Reason} = Error ->
            ctrl_result_response(error, State0, Error)
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
handle_ctrl_result({pos_prel, _}, #state{caller = start_chunk_transfer}
                   = State0) ->
    case accept_data_connection(State0) of
        {ok, State1} ->
            State = start_chunk(State1),
            {noreply, State};
        {error, _Reason} = Error ->
            ctrl_result_response(error, State0, Error)
    end;

%%--------------------------------------------------------------------------
%% File handling - chunk_transfer complete

handle_ctrl_result({pos_compl, _}, #state{client = From,
                                          caller = #recv_chunk_closing{dconn_closed       = true,
                                                                       client_called_us   = true,
                                                                       pos_compl_received = false
                                                                      }}
                   = State0) when From =/= undefined ->
    %% The pos_compl was the last event we waited for, finnish and clean up
    ?DBG("recv_chunk_closing pos_compl, last event",[]),
    gen_server:reply(From, ok),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{caller = undefined,
                          chunk = false,
                          client = undefined}};

handle_ctrl_result({pos_compl, _}, #state{caller = #recv_chunk_closing{}=R}
                   = State0) ->
    %% Waiting for more, don't care what
    ?DBG("recv_chunk_closing pos_compl, wait more",[]),
    {noreply, State0#state{caller = R#recv_chunk_closing{pos_compl_received=true}}};

handle_ctrl_result({pos_compl, _}, #state{caller = undefined, chunk = true}
                   = State0) ->
    %% Waiting for user to call recv_chunk
    {noreply, State0#state{caller = #recv_chunk_closing{pos_compl_received=true}}};

%%--------------------------------------------------------------------------
%% File handling - recv_file
handle_ctrl_result({pos_prel, _}, #state{caller = {recv_file, _}} = State0) ->
    case accept_data_connection(State0) of
        {ok, State1} ->
            State = activate_data_connection(State1),
            {noreply, State};
        {error, _Reason} = Error ->
            ctrl_result_response(error, State0, Error)
    end;

handle_ctrl_result({Status, _}, #state{caller = {recv_file, Fd}} = State) ->
    file_close(Fd),
    close_data_connection(State),
    ctrl_result_response(Status, State#state{dsock = undefined},
                         {error, epath});
%%--------------------------------------------------------------------------
%% File handling - transfer_*
handle_ctrl_result({pos_prel, _}, #state{caller = {transfer_file, Fd}}
                   = State0) ->
    case accept_data_connection(State0) of
        {ok, State1} ->
            send_file(State1, Fd);
        {error, _Reason} = Error ->
            ctrl_result_response(error, State0, Error)
    end;

handle_ctrl_result({pos_prel, _}, #state{caller = {transfer_data, Bin}}
                   = State0) ->
    case accept_data_connection(State0) of
        {ok, State} ->
            send_bin(State, Bin);
        {error, _Reason} = Error ->
            ctrl_result_response(error, State0, Error)
    end;

%%--------------------------------------------------------------------------
%% Default
handle_ctrl_result({Status, _Lines}, #state{client = From} = State)
  when From =/= undefined ->
    ctrl_result_response(Status, State, {error, Status});
handle_ctrl_result(CtrlMsg, #state{caller = undefined} = State) ->
    logger:log(info, #{protocol => ftp, unexpected_msg => CtrlMsg}),
    {noreply, State}.

%%--------------------------------------------------------------------------
%% Help functions to handle_ctrl_result
%%--------------------------------------------------------------------------

-spec ctrl_result_response(Status, State, Error) -> Result when
      Status :: ctrl_status_operation() | {ctrl_status_operation(), _},
      State  :: #state{},
      Error  :: {error, string() | Status | atom() | Reason},
      Reason :: term(),
      Result    :: {noreply, #state{}}
                 | {stop, normal | {error, Reason}, #state{}}
                 | {error, term()}.

ctrl_result_response(pos_compl, #state{client = From} = State, _) ->
    gen_server:reply(From, ok),
    {noreply, State#state{client = undefined, caller = undefined}};

ctrl_result_response(enofile, #state{client = From} = State, _) ->
    gen_server:reply(From, {error, enofile}),
    {noreply, State#state{client = undefined, caller = undefined}};

ctrl_result_response(error, State0, {error, _Reason} = Error) ->
    case State0#state.client of
        undefined ->
            {stop, Error, State0};
        From ->
            gen_server:reply(From, Error),
            State = activate_ctrl_connection(State0),
            {noreply, State}
    end;

ctrl_result_response(Status, #state{client = From} = State, _)
  when (Status =:= etnospc)  orelse
        (Status =:= epnospc)  orelse
        (Status =:= efnamena) orelse
        (Status =:= econn) ->
    gen_server:reply(From, {error, Status}),
    {stop, normal, State#state{client = undefined}};

ctrl_result_response(_, #state{client = From} = State, ErrorMsg) ->
    gen_server:reply(From, ErrorMsg),
    {noreply, State#state{client = undefined, caller = undefined}}.

%%--------------------------------------------------------------------------
-spec handle_caller(State) -> Result when
      State  :: #state{},
      Result :: {noreply, #state{}}.
handle_caller(#state{caller = {dir, Dir, Len}} = State0) ->
    Cmd = case Len of
              short -> "NLST";
              long -> "LIST"
          end,
    _ = case Dir of
            "" ->
                send_ctrl_message(State0, mk_cmd(Cmd, ""));
            _ ->
                send_ctrl_message(State0, mk_cmd(Cmd ++ " ~s", [Dir]))
        end,
    State = activate_ctrl_connection(State0),
    {noreply, State#state{caller = {dir, Dir}}};

handle_caller(#state{caller = {recv_bin, RemoteFile}} = State0) ->
    _ = send_ctrl_message(State0, mk_cmd("RETR ~s", [RemoteFile])),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{caller = recv_bin}};

handle_caller(#state{caller = {start_chunk_transfer, Cmd, RemoteFile}} =
              State0) ->
    _ = send_ctrl_message(State0, mk_cmd("~s ~s", [Cmd, RemoteFile])),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{caller = start_chunk_transfer}};

handle_caller(#state{caller = {recv_file, RemoteFile, Fd}} = State0) ->
    _ = send_ctrl_message(State0, mk_cmd("RETR ~s", [RemoteFile])),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{caller = {recv_file, Fd}}};

handle_caller(#state{caller = {transfer_file, {Cmd, LocalFile, RemoteFile}},
                     ldir = LocalDir, client = From} = State0)
  when (is_binary(LocalFile) orelse is_list(LocalFile) orelse is_atom(LocalFile)) ->
    case file_open(filename:absname(LocalFile, LocalDir), read) of
        {ok, Fd} ->
            _ = send_ctrl_message(State0, mk_cmd("~s ~s", [Cmd, RemoteFile])),
            State = activate_ctrl_connection(State0),
            {noreply, State#state{caller = {transfer_file, Fd}}};
        {error, _} ->
            gen_server:reply(From, {error, epath}),
            {noreply, State0#state{client = undefined, caller = undefined,
                                   dsock = undefined}}
    end;

handle_caller(#state{caller = {transfer_data, {Cmd, Bin, RemoteFile}}} =
              State0) ->
    _ = send_ctrl_message(State0, mk_cmd("~s ~s", [Cmd, RemoteFile])),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{caller = {transfer_data, Bin}}}.

%%  ----------- FTP SERVER COMMUNICATION  -------------------------

%% Connect to FTP server at Host (default is TCP port 21)
%% in order to establish a control connection.
-spec setup_ctrl_connection(Host, Port, Timeout, State) -> Result when
      Host    :: inet:ip_address() | inet:hostname(),
      Port    :: inet:port_number(),
      Timeout :: non_neg_integer(),
      State   :: #state{},
      Reason  :: timeout | inet:posix(),
      Result  :: {ok, State, integer()} | {error, Reason}.
setup_ctrl_connection(Host, Port, Timeout, #state{sockopts_ctrl = SockOpts} = State0) ->
    MsTime = erlang:monotonic_time(),
    case connect(Host, Port, SockOpts, Timeout, State0) of
        {ok, IpFam, CSock} ->
            State1 = State0#state{csock = {tcp, CSock}, ipfamily = IpFam},
            State = activate_ctrl_connection(State1),
            case Timeout - millisec_passed(MsTime) of
                Timeout2 when (Timeout2 >= 0) ->
                    {ok, State#state{caller = open}, Timeout2};
                _ ->
                    %% Oups: Simulate timeout
                    {ok, State#state{caller = open}, 0}
            end;
        Error ->
            Error
    end.

-spec setup_data_connection(State) -> Result when
      State :: #state{},
      Result :: {noreply, State}.
setup_data_connection(#state{mode   = active,
                             caller = Caller,
                             csock  = CSock,
                             sockopts_data_active = SockOpts,
                             ftp_extension = FtpExt} = State0) ->
    case (catch sockname(CSock)) of
        {ok, {{_, _, _, _, _, _, _, _} = IP0, _}} ->
            IP = proplists:get_value(ip, SockOpts, IP0),
            {ok, LSock} =
                gen_tcp:listen(0, [{ip, IP}, {active, false},
                                   inet6, binary, {packet, 0} |
                                   lists:keydelete(ip,1,SockOpts)]),
            {ok, {_, Port}} = sockname({tcp,LSock}),
            IpAddress = inet_parse:ntoa(IP),
            Cmd = mk_cmd("EPRT |2|~s|~p|", [IpAddress, Port]),
            _ = send_ctrl_message(State0, Cmd),
            State = activate_ctrl_connection(State0),
            {noreply, State#state{caller = {setup_data_connection,
                                            {LSock, Caller}}}};
        {ok, {{_,_,_,_} = IP0, _}} ->
            IP = proplists:get_value(ip, SockOpts, IP0),
            {ok, LSock} = gen_tcp:listen(0, [{ip, IP}, {active, false},
                                             binary, {packet, 0} |
                                             lists:keydelete(ip,1,SockOpts)]),
            {ok, Port} = inet:port(LSock),
            _ = case FtpExt of
                    false ->
                        {IP1, IP2, IP3, IP4} = IP,
                        {Port1, Port2} = {Port div 256, Port rem 256},
                        send_ctrl_message(State0,
                                          mk_cmd("PORT ~w,~w,~w,~w,~w,~w",
                                                 [IP1, IP2, IP3, IP4, Port1, Port2]));
                    true ->
                        IpAddress = inet_parse:ntoa(IP),
                        Cmd = mk_cmd("EPRT |1|~s|~p|", [IpAddress, Port]),
                        send_ctrl_message(State0, Cmd)
                end,
            State = activate_ctrl_connection(State0),
            {noreply, State#state{caller = {setup_data_connection,
                                            {LSock, Caller}}}}
    end;

setup_data_connection(#state{mode = passive, ipfamily = inet6,
                             caller = Caller} = State0) ->
    _ = send_ctrl_message(State0, mk_cmd("EPSV", [])),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{caller = {setup_data_connection, Caller}}};

setup_data_connection(#state{mode = passive, ipfamily = inet,
                             caller = Caller,
                             ftp_extension = false} = State0) ->
    _ = send_ctrl_message(State0, mk_cmd("PASV", [])),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{caller = {setup_data_connection, Caller}}};

setup_data_connection(#state{mode = passive, ipfamily = inet,
                             caller = Caller,
                             ftp_extension = true} = State0) ->
    _ = send_ctrl_message(State0, mk_cmd("EPSV", [])),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{caller = {setup_data_connection, Caller}}}.

-spec connect(Host, Port, SockOpts, Timeout, State) -> Result when
      Host     :: inet:ip_address() | inet:hostname(),
      Port     :: inet:port_number(),
      SockOpts :: [inet:inet_backend() | gen_tcp:connect_option()],
      Timeout :: timeout(),
      State   :: #state{},
      Reason  :: timeout | inet:posix(),
      Result  :: {ok, inet:address_family(), gen_tcp:socket()} | {error, Reason}.
connect(Host, Port, SockOpts, Timeout, #state{ipfamily = inet = IpFam}) ->
    connect2(Host, Port, IpFam, SockOpts, Timeout);

connect(Host, Port, SockOpts, Timeout, #state{ipfamily = inet6 = IpFam}) ->
    connect2(Host, Port, IpFam, SockOpts, Timeout);

connect(Host, Port, SockOpts, Timeout, #state{ipfamily = inet6fb4}) ->
    case inet:getaddr(Host, inet6) of
        {ok, {0, 0, 0, 0, 0, 16#ffff, _, _} = IPv6} ->
            case inet:getaddr(Host, inet) of
                {ok, IPv4} ->
                    IpFam = inet,
                    connect2(IPv4, Port, IpFam, SockOpts, Timeout);

                _ ->
                    IpFam = inet6,
                    connect2(IPv6, Port, IpFam, SockOpts, Timeout)
            end;

        {ok, IPv6} ->
            IpFam = inet6,
            connect2(IPv6, Port, IpFam, SockOpts, Timeout);

        _ ->
            case inet:getaddr(Host, inet) of
                {ok, IPv4} ->
                    IpFam = inet,
                    connect2(IPv4, Port, IpFam, SockOpts, Timeout);
                Error ->
                    Error
            end
    end.

-spec connect2(Host, Port, IpFam, SockOpts, Timeout) -> Result when
      Host     :: inet:socket_address() | inet:hostname(),
      Port     :: inet:port_number(),
      SockOpts :: [inet:inet_backend() | gen_tcp:connect_option()],
      Timeout  :: timeout(),
      IpFam    :: inet:address_family(),
      Reason   :: timeout | inet:posix(),
      Result   :: {ok, inet:address_family(), gen_tcp:socket()} | {error, Reason}.
connect2(Host, Port, IpFam, SockOpts, Timeout) ->
    Opts = [IpFam, binary, {packet, 0}, {active, false} | SockOpts],
    case gen_tcp:connect(Host, Port, Opts, Timeout) of
        {ok, Sock} ->
            {ok, IpFam, Sock};
        Error ->
            Error
    end.

-spec accept_data_connection_tls_options(State) -> Result when
      State  :: #state{},
      Result :: [tuple()].
accept_data_connection_tls_options(#state{ csock = {ssl,Socket}, tls_options = TO0, tls_ctrl_session_reuse = true }) ->
	TO = lists:keydelete(reuse_sessions, 1, TO0),
	{ok, [{session_id,SSLSessionId},{session_data,SSLSessionData}]} = ssl:connection_information(Socket, [session_id, session_data]),
	lists:keystore(reuse_session, 1, TO, {reuse_session,{SSLSessionId,SSLSessionData}});
accept_data_connection_tls_options(#state{ tls_options = TO }) ->
	TO.

-spec accept_data_connection(State) -> Result when
      State  :: #state{},
      Result :: {ok, #state{}} | {error, Reason},
      Reason :: term().
accept_data_connection(#state{mode     = active,
                              dtimeout = DTimeout,
                              tls_options = TLSOptions0,
                              dsock    = {lsock, LSock}} = State0) ->
    case gen_tcp:accept(LSock, DTimeout) of
        {ok, Socket} when is_list(TLSOptions0) ->
            gen_tcp:close(LSock),
            TLSOptions = accept_data_connection_tls_options(State0),
            ?DBG('<--data ssl:connect(~p, ~p)~n~p~n',[Socket,TLSOptions,State0]),
            case ssl:connect(Socket, TLSOptions, DTimeout) of
                {ok, TLSSocket} ->
                    {ok, State0#state{dsock={ssl,TLSSocket}}};
                {error, Reason} ->
                    {error, {ssl_connect_failed, Reason}}
            end;
        {ok, Socket} ->
            gen_tcp:close(LSock),
            {ok, State0#state{dsock={tcp,Socket}}};
        {error, Reason} ->
            {error, {data_connect_failed, Reason}}
    end;

accept_data_connection(#state{mode = passive,
                              dtimeout = DTimeout,
                              dsock = {tcp,Socket},
                              tls_options = TLSOptions0} = State) when is_list(TLSOptions0) ->
    TLSOptions = accept_data_connection_tls_options(State),
    ?DBG('<--data ssl:connect(~p, ~p)~n~p~n',[Socket,TLSOptions,State]),
    case ssl:connect(Socket, TLSOptions, DTimeout) of
        {ok, TLSSocket} ->
            {ok, State#state{dsock={ssl,TLSSocket}}};
        {error, Reason} ->
            {error, {ssl_connect_failed, Reason}}
    end;
accept_data_connection(#state{mode = passive} = State) ->
    {ok,State}.

-spec send_ctrl_message(State, Message) -> _ when
      State   :: #state{},
      Message :: [term()].
send_ctrl_message(_S=#state{csock = Socket, verbose = Verbose}, Message) ->
    verbose(lists:flatten(Message),Verbose,send),
    ?DBG('<--ctrl ~p ---- ~s~p~n',[Socket,Message,_S]),
    _ = send_message(Socket, Message).

send_data_message(_S=#state{dsock = Socket}, Message) ->
    ?DBG('<==data ~p ==== ~s~n~p~n',[Socket,Message,_S]),
    case send_message(Socket, Message) of
        ok ->
            ok;
        {error, Reason} ->
            Report = io_lib:format("send/2 for socket ~p failed with "
                                   "reason ~p~n", [Socket, Reason]),
            error_logger:error_report(Report),
            %% If tcp/ssl does not work the only option is to terminate,
            %% this is the expected behavior under these circumstances.
            exit(normal) %% User will get error message from terminate/2
    end.

send_message({tcp, Socket}, Message) ->
    gen_tcp:send(Socket, Message);
send_message({ssl, Socket}, Message) ->
    ssl:send(Socket, Message).

activate_ctrl_connection(#state{csock = CSock, ctrl_data = {<<>>, _, _}} = State) ->
    _ = activate_connection(CSock),
    State;
activate_ctrl_connection(#state{csock = CSock} = State0) ->
    _ = activate_connection(CSock),
    %% We have already received at least part of the next control message,
    %% that has been saved in ctrl_data, process this first.
    {noreply, State} = handle_info({socket_type(CSock), unwrap_socket(CSock), <<>>}, State0),
    State.

activate_data_connection(#state{dsock = DSock} = State) ->
    _ = activate_connection(DSock),
    State.

activate_connection(Socket) ->
    case socket_type(Socket) of
        tcp ->
            _ = activate_connection(inet, tcp_closed, Socket);
        ssl ->
            _ = activate_connection(ssl, ssl_closed, Socket)
    end.

activate_connection(API, CloseTag, Socket0) ->
    Socket = unwrap_socket(Socket0),
    case API:setopts(Socket, [{active, once}]) of
        ok ->
            ok;
        {error, _} -> %% inet can return einval instead of closed
            self() ! {CloseTag, Socket}
    end.

ignore_return_value(_) -> ok.

unwrap_socket({tcp,Socket}) -> Socket;
unwrap_socket({ssl,Socket}) -> Socket.

socket_type({tcp,_Socket}) -> tcp;
socket_type({ssl,_Socket}) -> ssl.

close_ctrl_connection(#state{csock = undefined}) -> ok;
close_ctrl_connection(#state{csock = Socket}) -> close_connection(Socket).

close_data_connection(#state{dsock = undefined}) -> ok;
close_data_connection(#state{dsock = Socket}) -> close_connection(Socket).

close_connection({lsock,Socket}) -> ignore_return_value( gen_tcp:close(Socket) );
close_connection({tcp, Socket})  -> ignore_return_value( gen_tcp:close(Socket) );
close_connection({ssl, Socket})  -> ignore_return_value( ssl:close(Socket) ).

%%  ------------ FILE HANDLING  ----------------------------------------
send_file(#state{tls_upgrading_data_connection = {true, CTRL, _}} = State, Fd) ->
    {noreply, State#state{tls_upgrading_data_connection = {true, CTRL, ?MODULE, send_file, Fd}}};
send_file(#state{client = Client}=State0, Fd) ->
    case file_read(Fd) of
        {ok, N, Bin} when N > 0 ->
            send_data_message(State0, Bin),
            progress_report({binary, Bin}, State0),
            send_file(State0, Fd);
        {ok, _, _} ->
            file_close(Fd),
            close_data_connection(State0),
            progress_report({transfer_size, 0}, State0),
            State = activate_ctrl_connection(State0),
            {noreply, State#state{caller = transfer_file_second_phase,
                                  dsock = undefined}};
        {error, Reason} ->
            gen_server:reply(Client, {error, Reason}),
            {stop, normal, State0#state{client = undefined}}
    end.

file_open(File, Option) ->
  file:open(File, [raw, binary, Option]).

file_close(Fd) ->
    ignore_return_value( file:close(Fd) ).

file_read(Fd) ->
    case file:read(Fd, ?FILE_BUFSIZE) of
        {ok, Bytes} when is_binary(Bytes) ->
            {ok, byte_size(Bytes), Bytes};
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
        {'EXIT', _, _} ->
            {error, eclosed};
        {'EXIT', _} ->
            {error, eclosed};
        Result ->
            Result
    end.

cast(GenServer, Msg) ->
    gen_server:cast(GenServer, {self(), Msg}).

send_bin(#state{tls_upgrading_data_connection = {true, CTRL, _}} = State, Bin) ->
    State#state{tls_upgrading_data_connection = {true, CTRL, ?MODULE, send_bin, Bin}};
send_bin(State0, Bin) ->
    send_data_message(State0, Bin),
    close_data_connection(State0),
    State = activate_ctrl_connection(State0),
    {noreply, State#state{caller = transfer_data_second_phase,
                          dsock = undefined}}.

mk_cmd(Fmt, Args) ->
    [io_lib:format(Fmt, Args)| [?CR, ?LF]]. % Deep list ok.

is_name_sane([]) ->
    true;
is_name_sane([?CR| _]) ->
    false;
is_name_sane([?LF| _]) ->
    false;
is_name_sane([_| Rest]) ->
    is_name_sane(Rest).

pwd_result(Lines) ->
    {_, [?DOUBLE_QUOTE | Rest]} =
        lists:splitwith(fun(?DOUBLE_QUOTE) -> false; (_) -> true end, Lines),
    {Dir, _} =
        lists:splitwith(fun(?DOUBLE_QUOTE) -> false; (_) -> true end, Rest),
    Dir.


key_search(Key, List, Default) ->
    case lists:keysearch(Key, 1, List) of
        {value, {_,Val}} ->
            Val;
        false ->
            Default
    end.

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
progress_report(stop, #state{progress = ProgressPid}) when is_pid(ProgressPid) ->
    ftp_progress:stop(ProgressPid);
progress_report({binary, Data}, #state{progress = ProgressPid}) when is_binary(Data), is_pid(ProgressPid) ->
    ftp_progress:report(ProgressPid, {transfer_size, byte_size(Data)});
progress_report(Report, #state{progress = ProgressPid})  when is_pid(ProgressPid) ->
    ftp_progress:report(ProgressPid, Report).


peername({tcp, Socket}) -> inet:peername(Socket);
peername({ssl, Socket}) -> ssl:peername(Socket).

sockname({tcp, Socket}) -> inet:sockname(Socket);
sockname({ssl, Socket}) -> ssl:sockname(Socket).

start_chunk(#state{tls_upgrading_data_connection = {true, CTRL, _}} = State) ->
    State#state{tls_upgrading_data_connection = {true, CTRL, ?MODULE, start_chunk, undefined}};
start_chunk(#state{client = From} = State) ->
    gen_server:reply(From, ok),
    State#state{chunk = true,
                client = undefined,
                caller = undefined}.


%% This function extracts the start options from the
%% Valid options:
%%     debug,
%%     verbose
%%     ipfamily
%%     priority
%%     flags    (for backward compatibillity)
start_options(Options) ->
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
%%          ftp_extension

-spec open_options([tuple()]) -> {ok, [tuple()]} | no_return().
open_options(Options) ->
    ValidateMode =
        fun(active) -> true;
           (passive) -> true;
           (_) -> false
        end,
    ValidateHost =
        fun(Host) when is_list(Host) ->
                true;
           (Host) when tuple_size(Host) =:= 4; tuple_size(Host) =:= 8 ->
                true;
           (_) ->
                false
        end,
    ValidatePort =
        fun(Port) when is_integer(Port) andalso (Port >= 0) -> true;
           (_) -> false
        end,
    ValidateIpFamily =
        fun(inet) -> true;
           (inet6) -> true;
           (inet6fb4) -> true;
           (_) -> false
        end,
    ValidateTLS =
        fun(TLS) when is_list(TLS) -> true;
           (undefined) -> true;
           (_) -> false
        end,
    ValidateTLSSecMethod =
        fun(ftpes) -> true;
           (ftps) -> true;
           (_) -> false
        end,
    ValidateTLSCtrlSessionReuse =
        fun(Reuse) when is_boolean(Reuse) -> true;
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
        ValidateFtpExtension =
        fun(true) -> true;
                (false) -> true;
                (_) -> false
        end,
    ValidOptions =
        [{mode,     ValidateMode,     false, ?DEFAULT_MODE},
         {host,     ValidateHost,     true,  ehost},
         {port,     ValidatePort,     false, 0},
         {ipfamily, ValidateIpFamily, false, inet},
         {tls,      ValidateTLS,      false, undefined},
         {tls_sec_method, ValidateTLSSecMethod, false, ftpes},
         {tls_ctrl_session_reuse, ValidateTLSCtrlSessionReuse, false, false},
         {timeout,  ValidateTimeout,  false, ?CONNECTION_TIMEOUT},
         {dtimeout, ValidateDTimeout, false, ?DATA_ACCEPT_TIMEOUT},
         {progress, ValidateProgress, false, ?PROGRESS_DEFAULT},
         {ftp_extension, ValidateFtpExtension, false, ?FTP_EXT_DEFAULT}],
    validate_options(Options, ValidOptions, []).

%% validates socket options and set defaults
-spec socket_options(Options :: [{atom(), [term()]}]) -> {ok, tuple()} | no_return().
socket_options(Options) ->
    CtrlOpts = proplists:get_value(sock_ctrl, Options, []),
    DataActOpts = proplists:get_value(sock_data_act, Options, CtrlOpts),
    DataPassOpts = proplists:get_value(sock_data_pass, Options, CtrlOpts),
    case [O || O <- lists:usort(CtrlOpts++DataPassOpts++DataActOpts),
               not valid_socket_option(O)] of
        [] ->
            {ok, {CtrlOpts, DataPassOpts, DataActOpts}};
        Invalid ->
            throw({error,{sock_opts,Invalid}})
    end.


valid_socket_option(inet            ) -> false;
valid_socket_option(inet6           ) -> false;
valid_socket_option({ipv6_v6only, _}) -> false;
valid_socket_option({active,_}      ) -> false;
valid_socket_option({packet,_}      ) -> false;
valid_socket_option({mode,_}        ) -> false;
valid_socket_option(binary          ) -> false;
valid_socket_option(list            ) -> false;
valid_socket_option({header,_}      ) -> false;
valid_socket_option({packet_size,_} ) -> false;
valid_socket_option(_) -> true.


-spec validate_options(Options, ValidOptions, Acc) -> Result when
      Options      :: [tuple()],
      ValidOptions :: [tuple()],
      Acc          :: [tuple()],
      Result       :: {ok, [tuple()]} | no_return().
validate_options([], [], Acc) ->
    {ok, lists:reverse(Acc)};
validate_options([], ValidOptions, Acc) ->
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
    case lists:keysearch(Key, 1, ValidOptions) of
        {value, {Key, Validate, _, Default}} ->
            case (catch Validate(Value)) of
                true ->
                    NewValidOptions = lists:keydelete(Key, 1, ValidOptions),
                    validate_options(Options, NewValidOptions,
                                     [{Key, Value} | Acc]);
                _ ->
                    NewValidOptions = lists:keydelete(Key, 1, ValidOptions),
                    validate_options(Options, NewValidOptions,
                                     [{Key, Default} | Acc])
            end;
        false ->
            validate_options(Options, ValidOptions, Acc)
    end;
validate_options([_|Options], ValidOptions, Acc) ->
    validate_options(Options, ValidOptions, Acc).

%% Help function, elapsed milliseconds since T0
millisec_passed(T0) ->
    %% OTP 18
    erlang:convert_time_unit(erlang:monotonic_time() - T0,
                             native,
                             micro_seconds) div 1000.
