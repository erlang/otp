%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2023. All Rights Reserved.
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

-module(ftp).

-removed([{start_service, 1, "use ftp:open/2 instead"},
          {stop_service, 1,  "use ftp:close/1 instead"}]).

-export([start/0,
         stop/0
        ]).

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
         append_chunk/2, append_chunk_end/1, append_chunk_start/2,
         info/1, latest_ctrl_response/1]).

-include("ftp_internal.hrl").

%%%=========================================================================
%%%  API
%%%=========================================================================

start() ->
    application:start(ftp).

stop() ->
    application:stop(ftp).


%%%=========================================================================
%%%  API - CLIENT FUNCTIONS
%%%=========================================================================

%%--------------------------------------------------------------------------
%% Description:  Start an ftp client and connect to a host.
%%--------------------------------------------------------------------------

-spec open(Host :: string() | inet:ip_address()) ->
    {'ok', Pid :: pid()} | {'error', Reason :: term()}.

%% <BACKWARD-COMPATIBILLITY>
open({option_list, Options}) when is_list(Options) ->
    ftp_internal:start_service(Options);
%% </BACKWARD-COMPATIBILLITY>

open(Host) ->
  ftp_internal:open(Host).


-spec open(Host :: string() | inet:ip_address(), Opts) ->
    {'ok', Pid :: pid()} | {'error', Reason :: term()} when
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
open(Host, Port) ->
    ftp_internal:open(Host, Port).

%%--------------------------------------------------------------------------
%% Description:  Login with or without a supplied account name.
%%--------------------------------------------------------------------------
-spec user(Pid  :: pid(),
           User :: string(),
           Pass :: string()) ->
    'ok' | {'error', Reason :: term()}.

user(Pid, User, Pass) ->
    ftp_internal:user(Pid, User, Pass).

-spec user(Pid  :: pid(),
           User :: string(),
           Pass :: string(),
           Account  :: string()) ->
    'ok' | {'error', Reason :: term()}.

user(Pid, User, Pass, Account) ->
    ftp_internal:user(Pid, User, Pass, Account).

%%--------------------------------------------------------------------------
%% Description:  Set a user Account.
%%--------------------------------------------------------------------------

-spec account(Pid :: pid(), Acc :: string()) ->
    'ok' | {'error', Reason :: term()}.

account(Pid, Acc) ->
    ftp_internal:account(Pid, Acc).

%%--------------------------------------------------------------------------
%% Description:  Get the current working directory at remote server.
%%--------------------------------------------------------------------------

-spec pwd(Pid :: pid()) ->
    {'ok', Dir :: string()} |
        {'error', Reason :: term()}.

pwd(Pid) ->
    ftp_internal:pwd(Pid).

%%--------------------------------------------------------------------------
%% Description:  Get the current working directory at local server.
%%--------------------------------------------------------------------------

-spec lpwd(Pid :: pid()) ->
    {'ok', Dir :: string()}.

lpwd(Pid) ->
    ftp_internal:lpwd(Pid).


%%--------------------------------------------------------------------------
%% Description:  Change current working directory at remote server.
%%--------------------------------------------------------------------------

-spec cd(Pid :: pid(), Dir :: string()) ->
    'ok' | {'error', Reason :: term()}.

cd(Pid, Dir) ->
    ftp_internal:cd(Pid, Dir).

%%--------------------------------------------------------------------------
%% Description:  Change current working directory for the local client.
%%--------------------------------------------------------------------------

-spec lcd(Pid :: pid(), Dir :: string()) ->
    'ok' | {'error', Reason :: term()}.

lcd(Pid, Dir) ->
    ftp_internal:lcd(Pid, Dir).

%%--------------------------------------------------------------------------
%% Description: Returns a list of files in long format.
%%--------------------------------------------------------------------------

-spec ls(Pid :: pid()) ->
    {'ok', Listing :: string()} |
        {'error', Reason :: term()}.

ls(Pid) ->
  ls(Pid, "").

-spec ls(Pid :: pid(), Dir :: string()) ->
    {'ok', Listing :: string()} |
        {'error', Reason ::  term()}.

ls(Pid, Dir) ->
    ftp_internal:ls(Pid, Dir).


%%--------------------------------------------------------------------------
%% Description:  Returns a list of files in short format
%%--------------------------------------------------------------------------

-spec nlist(Pid :: pid()) ->
    {'ok', Listing :: string()} |
        {'error', Reason :: term()}.

nlist(Pid) ->
  nlist(Pid, "").

-spec nlist(Pid :: pid(), Pathname :: string()) ->
    {'ok', Listing :: string()} |
        {'error', Reason :: term()}.

nlist(Pid, Dir) ->
    ftp_internal:nlist(Pid, Dir).

%%--------------------------------------------------------------------------
%% Description:  Rename a file at remote server.
%%--------------------------------------------------------------------------

-spec rename(Pid :: pid(), Old :: string(), New :: string()) ->
    'ok' | {'error', Reason :: term()}.

rename(Pid, Old, New) ->
    ftp_internal:rename(Pid, Old, New).

%%--------------------------------------------------------------------------
%% Description:  Remove file at remote server.
%%--------------------------------------------------------------------------

-spec delete(Pid :: pid(), File :: string()) ->
    'ok' | {'error', Reason :: term()}.

delete(Pid, File) ->
    ftp_internal:delete(Pid, File).

%%--------------------------------------------------------------------------
%% Description:  Make directory at remote server.
%%--------------------------------------------------------------------------

-spec mkdir(Pid :: pid(), Dir :: string()) ->
    'ok' | {'error', Reason :: term()}.

mkdir(Pid, Dir) ->
    ftp_internal:mkdir(Pid, Dir).

%%--------------------------------------------------------------------------
%% Description:  Remove directory at remote server.
%%--------------------------------------------------------------------------

-spec rmdir(Pid :: pid(), Dir :: string()) ->
    'ok' | {'error', Reason :: term()}.

rmdir(Pid, Dir) ->
    ftp_internal:rmdir(Pid, Dir).

%%--------------------------------------------------------------------------
%% Description:  Set transfer type.
%%--------------------------------------------------------------------------

-spec type(Pid :: pid(), Type :: ascii | binary) ->
    'ok' | {'error', Reason :: term()}.

type(Pid, Type) ->
    ftp_internal:type(Pid, Type).


%%--------------------------------------------------------------------------
%% Description:  Transfer file from remote server.
%%--------------------------------------------------------------------------

-spec recv(Pid :: pid(), RemoteFileName :: string()) ->
    'ok' | {'error', Reason :: term()}.

recv(Pid, RemoteFileName) ->
  ftp_internal:recv(Pid, RemoteFileName).

-spec recv(Pid            :: pid(),
           RemoteFileName :: string(),
           LocalFileName  :: string()) ->
    'ok' | {'error', Reason :: term()}.

recv(Pid, RemoteFileName, LocalFileName) ->
    ftp_internal:recv(Pid, RemoteFileName, LocalFileName).


%%--------------------------------------------------------------------------
%% Description:  Transfer file from remote server into binary.
%%--------------------------------------------------------------------------

-spec recv_bin(Pid        :: pid(),
               RemoteFile :: string()) ->
    {'ok', Bin :: binary()} | {'error', Reason :: term()}.

recv_bin(Pid, RemoteFile) ->
    ftp_internal:recv_bin(Pid, RemoteFile).


%%--------------------------------------------------------------------------
%% Description:  Start receive of chunks of remote file.
%%--------------------------------------------------------------------------

-spec recv_chunk_start(Pid        :: pid(),
                       RemoteFile :: string()) ->
    'ok' | {'error', Reason :: term()}.

recv_chunk_start(Pid, RemoteFile) ->
    ftp_internal:recv_chunk_start(Pid, RemoteFile).


%%--------------------------------------------------------------------------
%% Description:  Transfer file from remote server into binary in chunks
%%--------------------------------------------------------------------------

-spec recv_chunk(Pid :: pid()) ->
    'ok' |
        {'ok', Bin :: binary()} |
        {'error', Reason :: term()}.

recv_chunk(Pid) ->
    ftp_internal:recv_chunk(Pid).


%%--------------------------------------------------------------------------
%% Description:  Transfer file to remote server.
%%--------------------------------------------------------------------------

-spec send(Pid :: pid(), LocalFileName :: string()) ->
    'ok' | {'error', Reason :: term()}.

send(Pid, LocalFileName) ->
  send(Pid, LocalFileName, LocalFileName).

-spec send(Pid            :: pid(),
           LocalFileName  :: string(),
           RemoteFileName :: string()) ->
    'ok' | {'error', Reason :: term()}.

send(Pid, LocalFileName, RemotFileName) ->
    ftp_internal:send(Pid, LocalFileName, RemotFileName).


%%--------------------------------------------------------------------------
%% Description:  Transfer a binary to a remote file.
%%--------------------------------------------------------------------------

-spec send_bin(Pid :: pid(), Bin :: binary(), RemoteFile :: string()) ->
    'ok' | {'error', Reason :: term()}.

send_bin(Pid, Bin, RemoteFile) ->
    ftp_internal:send_bin(Pid, Bin, RemoteFile).


%%--------------------------------------------------------------------------
%% Description:  Start transfer of chunks to remote file.
%%--------------------------------------------------------------------------

-spec send_chunk_start(Pid :: pid(), RemoteFile :: string()) ->
    'ok' | {'error', Reason :: term()}.

send_chunk_start(Pid, RemoteFile) ->
    ftp_internal:send_chunk_start(Pid, RemoteFile).

%%--------------------------------------------------------------------------
%% Description:  Start append chunks of data to remote file.
%%--------------------------------------------------------------------------

-spec append_chunk_start(Pid :: pid(), RemoteFile :: string()) ->
    'ok' | {'error', Reason :: term()}.

append_chunk_start(Pid, RemoteFile) ->
    ftp_internal:append_chunk_start(Pid, RemoteFile).


%%--------------------------------------------------------------------------
%% Purpose:  Send chunk to remote file.
%%--------------------------------------------------------------------------

-spec send_chunk(Pid :: pid(), Bin :: binary()) ->
    'ok' | {'error', Reason :: term()}.

send_chunk(Pid, Bin) ->
    ftp_internal:send_chunk(Pid, Bin).

%%--------------------------------------------------------------------------
%% Description:  Append chunk to remote file.
%%--------------------------------------------------------------------------

-spec append_chunk(Pid :: pid(), Bin :: binary()) ->
    'ok' | {'error', Reason :: term()}.

append_chunk(Pid, Bin) ->
    ftp_internal:append_chunk(Pid, Bin).


%%--------------------------------------------------------------------------
%% Description:  End sending of chunks to remote file.
%%--------------------------------------------------------------------------

-spec send_chunk_end(Pid :: pid()) ->
    'ok' | {'error', Reason :: term()}.

send_chunk_end(Pid) ->
    ftp_internal:send_chunk_end(Pid).


%%--------------------------------------------------------------------------
%% Description:  End appending of chunks to remote file.
%%--------------------------------------------------------------------------

-spec append_chunk_end(Pid :: pid()) ->
    'ok' | {'error', Reason :: term()}.

append_chunk_end(Pid) ->
    ftp_internal:append_chunk_end(Pid).


%%--------------------------------------------------------------------------
%% Description:  Append the local file to the remote file
%%--------------------------------------------------------------------------

-spec append(Pid :: pid(), LocalFileName :: string()) ->
    'ok' | {'error', Reason :: term()}.

append(Pid, LocalFileName) ->
    append(Pid, LocalFileName, LocalFileName).

-spec append(Pid            :: pid(),
             LocalFileName  :: string(),
             RemoteFileName :: string()) ->
    'ok' | {'error', Reason :: term()}.

append(Pid, LocalFileName, RemotFileName) ->
    ftp_internal:append(Pid, LocalFileName, RemotFileName).


%%--------------------------------------------------------------------------
%% Purpose:  Append a binary to a remote file.
%%--------------------------------------------------------------------------

-spec append_bin(Pid        :: pid(),
                 Bin        :: binary(),
                 RemoteFile :: string()) ->
    'ok' | {'error', Reason :: term()}.

append_bin(Pid, Bin, RemoteFile) ->
    ftp_internal:append_bin(Pid, Bin, RemoteFile).


%%--------------------------------------------------------------------------
%% Description: Send arbitrary ftp command.
%%--------------------------------------------------------------------------

-spec quote(Pid :: pid(), Cmd :: string()) -> [FTPLine :: string()].

quote(Pid, Cmd) when is_list(Cmd) ->
    ftp_internal:quote(Pid, Cmd).


%%--------------------------------------------------------------------------
%% Description:  End the ftp session.
%%--------------------------------------------------------------------------

-spec close(Pid :: pid()) -> 'ok'.
close(Pid) ->
    ftp_internal:close(Pid).


%%--------------------------------------------------------------------------
%% Description:  Return diagnostics.
%%--------------------------------------------------------------------------

-spec formaterror(Tag :: atom() | {error, atom()}) -> string().

formaterror(Tag) ->
  ftp_response:error_string(Tag).


info(Pid) ->
    ftp_internal:info(Pid).


%%--------------------------------------------------------------------------
%% Description:  The latest received response from the server
%%--------------------------------------------------------------------------

-spec latest_ctrl_response(Pid :: pid()) -> string().

latest_ctrl_response(Pid) ->
    ftp_internal:latest_ctrl_response(Pid).

