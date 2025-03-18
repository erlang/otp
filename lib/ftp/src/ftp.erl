%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2025. All Rights Reserved.
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
-moduledoc """
A File Transfer Protocol client.

This module implements a client for file transfer according to a subset of the
File Transfer Protocol (FTP), see [RFC 959](http://www.ietf.org/rfc/rfc959.txt).

The FTP client always tries to use passive FTP mode and only resort to active
FTP mode if this fails. This default behavior can be changed by start option
[mode](`m:ftp#mode`).

For a simple example of an FTP session, see [FTP User's Guide](ftp_client.md).

The return values of the following functions depend much on the implementation
of the FTP server at the remote host. In particular, the results from `ls` and
`nlist` varies. Often real errors are not reported as errors by `ls`, even if,
for example, a file or directory does not exist. `nlist` is usually more strict,
but some implementations have the peculiar behaviour of responding with an error
if the request is a listing of the contents of a directory that exists but is
empty.

## Errors
The possible error reasons and the corresponding diagnostic strings returned by
[`formaterror/1`](`formaterror/1`) are as follows:

- **`echunk`** - Synchronization error during chunk sending according to one of
  the following:

  - A call is made to [`send_chunk/2`](`send_chunk/2`) or
    [`send_chunk_end/1`](`send_chunk_end/1`) before a call to
    [`send_chunk_start/2`](`send_chunk_start/2`).
  - A call has been made to another transfer function during chunk sending, that
    is, before a call to [`send_chunk_end/1`](`send_chunk_end/1`).

- **`eclosed`** - The session is closed.

- **`econn`** - Connection to the remote server is prematurely closed.

- **`ehost`** - Host is not found, FTP server is not found, or connection is
  rejected by FTP server.

- **`elogin`** - User is not logged in.

- **`enotbinary`** - Term is not a binary.

- **`epath`** - No such file or directory, or directory already exists, or
  permission denied.

- **`etype`** - No such type.

- **`euser`** - Invalid username or password.

- **`etnospc`** - Insufficient storage space in system \[452].

- **`epnospc`** - Exceeded storage allocation (for current directory or dataset)
  \[552].

- **`efnamena`** - Filename not allowed \[553].
""".


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

-type client()                   :: pid(). 

-export_type([client/0]).

-include("ftp_internal.hrl").

%%%=========================================================================
%%%  API
%%%=========================================================================

-doc false.
start() ->
    application:start(ftp).

-doc false.
stop() ->
    application:stop(ftp).


%%%=========================================================================
%%%  API - CLIENT FUNCTIONS
%%%=========================================================================

%%--------------------------------------------------------------------------
%% Description:  Start an ftp client and connect to a host.
%%--------------------------------------------------------------------------

-doc(#{group => <<"Connection API">>}).
-doc(#{equiv => open/2}).
-spec open(Host :: inet:hostname() | inet:ip_address()) ->
    {'ok', Client :: client()} | {'error', Reason :: term()}.

%% <BACKWARD-COMPATIBILLITY>
open({option_list, Options}) when is_list(Options) ->
    ftp_internal:start_service(Options);
%% </BACKWARD-COMPATIBILLITY>

open(Host) ->
  ftp_internal:open(Host).

-doc(#{group => <<"Connection API">>}).
-doc """
Starts a FTP client process and opens a session with the FTP server at `Host`.

A session opened in this way is closed using function `close/1`.

The available configuration options are as follows:

- **\{host, Host\}** - [](){: #host } Host = `string() | ip_address()`

- **\{port, Port\}** - [](){: #port } Default is `0` which aliases to `21` or
  `990` when used with [`{tls_sec_method,ftps}`](`open/2`)).

- **\{mode, Mode\}** - [](){: #mode } Default is `passive`.

- **\{verbose, Verbose\}** - [](){: #verbose } Determines if the FTP
  communication is to be verbose or not.

  Default is `false`.

- **\{debug, Debug\}** - [](){: #debug } Debugging using the dbg toolkit.

  Default is `disable`.

- **\{ipfamily, IpFamily\}** - [](){: #ipfamily } With `inet6fb4` the client
  behaves as before, that is, tries to use IPv6, and only if that does not work
  it uses IPv4).

  Default is `inet` (IPv4).

- **\{timeout, Timeout\}** - [](){: #timeout } Connection time-out.

  Default is `60000` (milliseconds).

- **\{dtimeout, DTimeout\}** - [](){: #dtimeout } Data connect time-out. The
  time the client waits for the server to connect to the data socket.

  Default is `infinity`.

- **\{tls, TLSOptions\}** - [](){: #tls_options } The FTP session is transported
  over `tls` (`ftps`, see [RFC 4217](http://www.ietf.org/rfc/rfc4217.txt)). The
  list `TLSOptions` can be empty. The function `ssl:connect/3` is used for
  securing both the control connection and the data sessions.

- **\{tls_sec_method, TLSSecMethod\}** - [](){: #tls_sec_method } When set to
  `ftps` will connect immediately with SSL instead of upgrading with STARTTLS.
  This suboption is ignored unless the suboption `tls` is also set.

  Default is `ftpes`

- **\{tls_ctrl_session_reuse, boolean()\}** - [](){: #tls_ctrl_session_reuse }
  When set to `true` the client will re-use the TLS session from the control
  channel on the data channel as enforced by many FTP servers as
  ([proposed and implemented first by vsftpd](https://scarybeastsecurity.blogspot.com/2009/02/vsftpd-210-released.html)).

  Default is `false`.

- **\{sock_ctrl, SocketCtrls :: \[SocketControl :: gen_tcp:option()]\}** -
  Passes options from `SocketCtrls` down to the underlying transport layer
  (tcp).

  `t:gen_tcp:option/0` except for `ipv6_v6only`, `active`, `packet`, `mode`,
  `packet_size` and `header`.

  Default value is `SocketCtrls = []`.

- **\{sock_data_act, \[SocketControl]\}** - Passes options from
  `[SocketControl]` down to the underlying transport layer (tcp).

  `sock_data_act` uses the value of `sock_ctrl` as default value.

- **\{sock_data_pass, \[SocketControl]\}** - Passes options from
  `[SocketControl]` down to the underlying transport layer (tcp).

  `sock_data_pass` uses the value of `sock_ctrl` as default value.

- **\{progress, Progress\}** - [](){: #progress } Progress =
  `ignore | {Module, Function, InitialData}`

  `Module = atom()`, `Function = atom()`

  `InitialData = term()`

  Default is `ignore`.

  Option `progress` is intended to be used by applications that want to create
  some type of progress report, such as a progress bar in a GUI. Default for the
  progress option is `ignore`, that is, the option is not used. When the
  progress option is specified, the following happens when `ftp:send/[3,4]` or
  `ftp:recv/[3,4]` are called:

  - Before a file is transferred, the following call is made to indicate the
    start of the file transfer and how large the file is. The return value of
    the callback function is to be a new value for the `UserProgressTerm` that
    will be used as input the next time the callback function is called.

    `Module:Function(InitialData, File, {file_size, FileSize})`

  - Every time a chunk of bytes is transferred the following call is made:

    `Module:Function(UserProgressTerm, File, {transfer_size, TransferSize})`

  - At the end of the file the following call is made to indicate the end of the
    transfer:

    `Module:Function(UserProgressTerm, File, {transfer_size, 0})`

  The callback function is to be defined as follows:

  `Module:Function(UserProgressTerm, File, Size) -> UserProgressTerm`

  `UserProgressTerm = term()`

  `File = string()`

  `Size = {transfer_size, integer()} | {file_size, integer()} | {file_size, unknown}`

  For remote files, `ftp` cannot determine the file size in a platform
  independent way. In this case the size becomes `unknown` and it is left to the
  application to determine the size.

  > #### Note {: .info }
  >
  > The callback is made by a middleman process, hence the file transfer is not
  > affected by the code in the progress callback function. If the callback
  > crashes, this is detected by the FTP connection process, which then prints
  > an info-report and goes on as if the progress option was set to `ignore`.

  The file transfer type is set to the default of the FTP server when the
  session is opened. This is usually ASCII mode.

  The current local working directory (compare [`lpwd/1`](`lpwd/1`)) is set to
  the value reported by `file:get_cwd/1`, the wanted local directory.

  The return value `Pid` is used as a reference to the newly created FTP client
  in all other functions, and they are to be called by the process that created
  the connection. The FTP client process monitors the process that created it
  and terminates if that process terminates.
""".
-spec open(Host :: string() | inet:ip_address(), Opts) ->
    {'ok', Client :: client()} | {'error', Reason :: term()} when
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
-doc(#{group => <<"Connection API">>}).
-doc """
Performs login of `User` with `Pass`.
""".
-spec user(Pid  :: pid(),
           User :: string(),
           Pass :: string()) ->
    'ok' | {'error', Reason :: term()}.

user(Pid, User, Pass) ->
    ftp_internal:user(Pid, User, Pass).

-doc(#{group => <<"Connection API">>}).
-doc """
Performs login of `User` with `Pass` to the account specified by `Account`.
""".
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

-doc(#{group => <<"Connection API">>}).
-doc """
Sets the account for an operation, if needed.
""".
-spec account(Client ::client(), Acc :: string()) ->
    'ok' | {'error', Reason :: term()}.

account(Pid, Acc) ->
    ftp_internal:account(Pid, Acc).

%%--------------------------------------------------------------------------
%% Description:  Get the current working directory at remote server.
%%--------------------------------------------------------------------------
-doc(#{group => <<"Info API">>}).
-doc """
Returns the current working directory at the remote server.
""".
-spec pwd(Client ::client()) ->
    {'ok', Dir :: string()} |
        {'error', Reason :: term()}.

pwd(Pid) ->
    ftp_internal:pwd(Pid).

%%--------------------------------------------------------------------------
%% Description:  Get the current working directory at local server.
%%--------------------------------------------------------------------------
-doc(#{group => <<"Info API">>}).
-doc """
Returns the current working directory at the local client.
""".
-spec lpwd(Client ::client()) ->
    {'ok', Dir :: string()}.

lpwd(Pid) ->
    ftp_internal:lpwd(Pid).


%%--------------------------------------------------------------------------
%% Description:  Change current working directory at remote server.
%%--------------------------------------------------------------------------
-doc(#{group => <<"Update API">>}).
-doc """
Changes the working directory at the remote server to `Dir`.
""".
-spec cd(Client ::client(), Dir :: string()) ->
    'ok' | {'error', Reason :: term()}.

cd(Pid, Dir) ->
    ftp_internal:cd(Pid, Dir).

%%--------------------------------------------------------------------------
%% Description:  Change current working directory for the local client.
%%--------------------------------------------------------------------------
-doc(#{group => <<"Update API">>}).
-doc """
Changes the working directory to `Dir` for the local client.
""".
-spec lcd(Client ::client(), Dir :: string()) ->
    'ok' | {'error', Reason :: term()}.

lcd(Pid, Dir) ->
    ftp_internal:lcd(Pid, Dir).

%%--------------------------------------------------------------------------
%% Description: Returns a list of files in long format.
%%--------------------------------------------------------------------------
-doc(#{group => <<"Info API">>}).
-doc(#{equiv => ls/2}).
-spec ls(Client ::client()) ->
    {'ok', Listing :: string()} |
        {'error', Reason :: term()}.

ls(Pid) ->
  ls(Pid, "").

-doc(#{group => <<"Info API">>}).
-doc """
Returns a list of files in long format.

`Dir` can be a directory or a file. The `Dir` string can contain wildcards.

[`ls/1`](`ls/1`) implies the current remote directory of the user.

The format of `Listing` depends on the operating system. On UNIX, it is
typically produced from the output of the `ls -l` shell command.
""".
-spec ls(Client ::client(), Dir :: string()) ->
    {'ok', Listing :: string()} |
        {'error', Reason ::  term()}.

ls(Pid, Dir) ->
    ftp_internal:ls(Pid, Dir).


%%--------------------------------------------------------------------------
%% Description:  Returns a list of files in short format
%%--------------------------------------------------------------------------

-doc(#{group => <<"Info API">>}).
-doc(#{equiv => nlist/2}).
-spec nlist(Client ::client()) ->
    {'ok', Listing :: string()} |
        {'error', Reason :: term()}.

nlist(Pid) ->
  nlist(Pid, "").

-doc(#{group => <<"Info API">>}).
-doc """
Returns a list of files in short format.

`Pathname` can be a directory or a file. The `Pathname` string can contain
wildcards.

[`nlist/1`](`nlist/1`) implies the current remote directory of the user.

The format of `Listing` is a stream of filenames where each filename is
separated by <CRLF> or <NL>. Contrary to function `ls`, the purpose of `nlist`
is to enable a program to process filename information automatically.
""".
-spec nlist(Client ::client(), Pathname :: string()) ->
    {'ok', Listing :: string()} |
        {'error', Reason :: term()}.

nlist(Pid, Dir) ->
    ftp_internal:nlist(Pid, Dir).

%%--------------------------------------------------------------------------
%% Description:  Rename a file at remote server.
%%--------------------------------------------------------------------------
-doc(#{group => <<"Update API">>}).
-doc """
Renames `Old` to `New` at the remote server.
""".
-spec rename(Client ::client(), Old :: string(), New :: string()) ->
    'ok' | {'error', Reason :: term()}.

rename(Pid, Old, New) ->
    ftp_internal:rename(Pid, Old, New).

%%--------------------------------------------------------------------------
%% Description:  Remove file at remote server.
%%--------------------------------------------------------------------------

-doc(#{group => <<"Update API">>}).
-doc """
Deletes the file `File` at the remote server.
""".
-spec delete(Client ::client(), File :: string()) ->
    'ok' | {'error', Reason :: term()}.

delete(Pid, File) ->
    ftp_internal:delete(Pid, File).

%%--------------------------------------------------------------------------
%% Description:  Make directory at remote server.
%%--------------------------------------------------------------------------
-doc(#{group => <<"Update API">>}).
-doc """
Creates the directory `Dir` at the remote server.
""".
-spec mkdir(Client ::client(), Dir :: string()) ->
    'ok' | {'error', Reason :: term()}.

mkdir(Pid, Dir) ->
    ftp_internal:mkdir(Pid, Dir).

%%--------------------------------------------------------------------------
%% Description:  Remove directory at remote server.
%%--------------------------------------------------------------------------
-doc(#{group => <<"Update API">>}).
-doc """
Removes directory `Dir` at the remote server.
""".
-spec rmdir(Client ::client(), Dir :: string()) ->
    'ok' | {'error', Reason :: term()}.

rmdir(Pid, Dir) ->
    ftp_internal:rmdir(Pid, Dir).

%%--------------------------------------------------------------------------
%% Description:  Set transfer type.
%%--------------------------------------------------------------------------
-doc(#{group => <<"Update API">>}).
-doc """
Sets the file transfer type to `ascii` or `binary`. When an FTP session is
opened, the default transfer type of the server is used, most often `ascii`,
which is default according to [RFC 959](http://www.ietf.org/rfc/rfc959.txt).
""".
-spec type(Client ::client(), Type :: ascii | binary) ->
    'ok' | {'error', Reason :: term()}.

type(Pid, Type) ->
    ftp_internal:type(Pid, Type).


%%--------------------------------------------------------------------------
%% Description:  Transfer file from remote server.
%%--------------------------------------------------------------------------

-doc(#{group => <<"File Transfer API">>}).
-doc(#{equiv => recv/3}).
-spec recv(Client ::client(), RemoteFileName :: file:filename()) ->
    'ok' | {'error', Reason :: term()}.

recv(Pid, RemoteFileName) ->
  ftp_internal:recv(Pid, RemoteFileName).

-doc(#{group => <<"File Transfer API">>}).
-doc """
Transfers the file `RemoteFileName` from the remote server to the file system of
the local client. If `LocalFileName` is specified, the local file will be
`LocalFileName`, otherwise `RemoteFileName`.

If the file write fails, the command is aborted and `{error, term()}` is
returned. However, the file is _not_ removed.
""".
-spec recv(Pid            :: pid(),
           RemoteFileName :: file:filename(),
           LocalFileName  :: file:filename()) ->
    'ok' | {'error', Reason :: term()}.

recv(Pid, RemoteFileName, LocalFileName) ->
    ftp_internal:recv(Pid, RemoteFileName, LocalFileName).


%%--------------------------------------------------------------------------
%% Description:  Transfer file from remote server into binary.
%%--------------------------------------------------------------------------

-doc(#{group => <<"File Transfer API">>}).
-doc """
Transfers the file `RemoteFile` from the remote server and receives it as a
binary.
""".
-spec recv_bin(Pid        :: pid(),
               RemoteFile :: string()) ->
    {'ok', Bin :: binary()} | {'error', Reason :: term()}.

recv_bin(Pid, RemoteFile) ->
    ftp_internal:recv_bin(Pid, RemoteFile).


%%--------------------------------------------------------------------------
%% Description:  Start receive of chunks of remote file.
%%--------------------------------------------------------------------------
-doc(#{group => <<"Chunk File Transfer API">>}).
-doc """
Starts transfer of the file `RemoteFile` from the remote server.
""".
-spec recv_chunk_start(Pid        :: pid(),
                       RemoteFile :: string()) ->
    'ok' | {'error', Reason :: term()}.

recv_chunk_start(Pid, RemoteFile) ->
    ftp_internal:recv_chunk_start(Pid, RemoteFile).


%%--------------------------------------------------------------------------
%% Description:  Transfer file from remote server into binary in chunks
%%--------------------------------------------------------------------------
-doc(#{group => <<"Chunk File Transfer API">>}).
-doc """
Receives a chunk of the remote file (`RemoteFile` of `recv_chunk_start`). The
return values have the following meaning:

- `ok` = the transfer is complete.
- `{ok, Bin}` = just another chunk of the file.
- `{error, Reason}` = transfer failed.
""".
-spec recv_chunk(Client ::client()) ->
    'ok' |
        {'ok', Bin :: binary()} |
        {'error', Reason :: term()}.

recv_chunk(Pid) ->
    ftp_internal:recv_chunk(Pid).


%%--------------------------------------------------------------------------
%% Description:  Transfer file to remote server.
%%--------------------------------------------------------------------------

-doc(#{group => <<"File Transfer API">>}).
-doc(#{equiv => send/3}).
-spec send(Client ::client(), LocalFileName :: file:filename()) ->
    'ok' | {'error', Reason :: term()}.

send(Pid, LocalFileName) ->
  send(Pid, LocalFileName, LocalFileName).

-doc(#{group => <<"File Transfer API">>}).
-doc """
Transfers the file `LocalFileName` to the remote server. If `RemoteFileName` is
specified, the name of the remote file is set to `RemoteFileName`, otherwise to
`LocalFileName`.
""".
-spec send(Pid            :: pid(),
           LocalFileName  :: file:filename(),
           RemoteFileName :: file:filename()) ->
    'ok' | {'error', Reason :: term()}.

send(Pid, LocalFileName, RemotFileName) ->
    ftp_internal:send(Pid, LocalFileName, RemotFileName).


%%--------------------------------------------------------------------------
%% Description:  Transfer a binary to a remote file.
%%--------------------------------------------------------------------------
-doc(#{group => <<"File Transfer API">>}).
-doc """
Transfers the binary `Bin` into the file `RemoteFile` at the remote server.
""".
-spec send_bin(Client ::client(), Bin :: binary(), RemoteFile :: string()) ->
    'ok' | {'error', Reason :: term()}.

send_bin(Pid, Bin, RemoteFile) ->
    ftp_internal:send_bin(Pid, Bin, RemoteFile).


%%--------------------------------------------------------------------------
%% Description:  Start transfer of chunks to remote file.
%%--------------------------------------------------------------------------
-doc(#{group => <<"Chunk File Transfer API">>}).
-doc """
Starts transfer of chunks into the file `RemoteFile` at the remote server.
""".
-spec send_chunk_start(Client ::client(), RemoteFile :: string()) ->
    'ok' | {'error', Reason :: term()}.

send_chunk_start(Pid, RemoteFile) ->
    ftp_internal:send_chunk_start(Pid, RemoteFile).

%%--------------------------------------------------------------------------
%% Description:  Start append chunks of data to remote file.
%%--------------------------------------------------------------------------
-doc(#{group => <<"Chunk File Transfer API">>}).
-doc """
Starts the transfer of chunks for appending to the file `RemoteFile` at the
remote server. If the file does not exist, it is created.
""".
-spec append_chunk_start(Client ::client(), RemoteFile :: string()) ->
    'ok' | {'error', Reason :: term()}.

append_chunk_start(Pid, RemoteFile) ->
    ftp_internal:append_chunk_start(Pid, RemoteFile).


%%--------------------------------------------------------------------------
%% Purpose:  Send chunk to remote file.
%%--------------------------------------------------------------------------
-doc(#{group => <<"Chunk File Transfer API">>}).
-doc """
Transfers the chunk `Bin` to the remote server, which writes it into the file
specified in the call to [`send_chunk_start/2`](`send_chunk_start/2`).

For some errors, for example, file system full, it is necessary to to call
`send_chunk_end` to get the proper reason.
""".
-spec send_chunk(Client ::client(), Bin :: binary()) ->
    'ok' | {'error', Reason :: term()}.

send_chunk(Pid, Bin) ->
    ftp_internal:send_chunk(Pid, Bin).

%%--------------------------------------------------------------------------
%% Description:  Append chunk to remote file.
%%--------------------------------------------------------------------------
-doc(#{group => <<"Chunk File Transfer API">>}).
-doc """
Transfers the chunk `Bin` to the remote server, which appends it to the file
specified in the call to [`append_chunk_start/2`](`append_chunk_start/2`).

For some errors, for example, file system full, it is necessary to call
`append_chunk_end` to get the proper reason.
""".
-spec append_chunk(Client ::client(), Bin :: binary()) ->
    'ok' | {'error', Reason :: term()}.

append_chunk(Pid, Bin) ->
    ftp_internal:append_chunk(Pid, Bin).


%%--------------------------------------------------------------------------
%% Description:  End sending of chunks to remote file.
%%--------------------------------------------------------------------------

-doc(#{group => <<"Chunk File Transfer API">>}).
-doc """
Stops transfer of chunks to the remote server. The file at the remote server,
specified in the call to [`send_chunk_start/2`](`send_chunk_start/2`) is closed
by the server.
""".
-spec send_chunk_end(Client ::client()) ->
    'ok' | {'error', Reason :: term()}.

send_chunk_end(Pid) ->
    ftp_internal:send_chunk_end(Pid).


%%--------------------------------------------------------------------------
%% Description:  End appending of chunks to remote file.
%%--------------------------------------------------------------------------

-doc(#{group => <<"Chunk File Transfer API">>}).
-doc """
Stops transfer of chunks for appending to the remote server. The file at the
remote server, specified in the call to
[`append_chunk_start/2`](`append_chunk_start/2`), is closed by the server.
""".
-spec append_chunk_end(Client ::client()) ->
    'ok' | {'error', Reason :: term()}.

append_chunk_end(Pid) ->
    ftp_internal:append_chunk_end(Pid).


%%--------------------------------------------------------------------------
%% Description:  Append the local file to the remote file
%%--------------------------------------------------------------------------

-doc(#{group => <<"Update API">>}).
-doc(#{equiv => append/3}).
-spec append(Client ::client(), LocalFileName :: file:filename()) ->
    'ok' | {'error', Reason :: term()}.

append(Pid, LocalFileName) ->
    append(Pid, LocalFileName, LocalFileName).

-doc(#{group => <<"Update API">>}).
-doc """
Transfers the file `LocalFile` to the remote server. If `RemoteFile` is
specified, the name of the remote file that the file is appended to is set to
`RemoteFile`, otherwise to `LocalFile`. If the file does not exists, it is
created.
""".
-spec append(Pid            :: pid(),
             LocalFileName  :: file:filename(),
             RemoteFileName :: file:filename()) ->
    'ok' | {'error', Reason :: term()}.

append(Pid, LocalFileName, RemotFileName) ->
    ftp_internal:append(Pid, LocalFileName, RemotFileName).


%%--------------------------------------------------------------------------
%% Purpose:  Append a binary to a remote file.
%%--------------------------------------------------------------------------

-doc(#{group => <<"Update API">>}).
-doc """
Transfers the binary `Bin` to the remote server and appends it to the file
`RemoteFile`. If the file does not exist, it is created.
""".
-spec append_bin(Pid        :: pid(),
                 Bin        :: binary(),
                 RemoteFile :: string()) ->
    'ok' | {'error', Reason :: term()}.

append_bin(Pid, Bin, RemoteFile) ->
    ftp_internal:append_bin(Pid, Bin, RemoteFile).


%%--------------------------------------------------------------------------
%% Description: Send arbitrary ftp command.
%%--------------------------------------------------------------------------

-doc """
> #### Note {: .info }
>
> The telnet end of line characters, from the FTP protocol definition, CRLF, for
> example, "\\\\r\\\\n" has been removed.

Sends an arbitrary FTP command and returns verbatim a list of the lines sent
back by the FTP server. This function is intended to give application accesses
to FTP commands that are server-specific or that cannot be provided by this FTP
client.

> #### Note {: .info }
>
> FTP commands requiring a data connection cannot be successfully issued with
> this function.
""".
-spec quote(Client ::client(), Cmd :: string()) -> [FTPLine :: string()].

quote(Pid, Cmd) when is_list(Cmd) ->
    ftp_internal:quote(Pid, Cmd).


%%--------------------------------------------------------------------------
%% Description:  End the ftp session.
%%--------------------------------------------------------------------------

-doc(#{group => <<"Connection API">>}).
-doc """
Ends an FTP session, created using function [open](`open/2`).
""".
-spec close(Client ::client()) -> 'ok'.
close(Pid) ->
    ftp_internal:close(Pid).


%%--------------------------------------------------------------------------
%% Description:  Return diagnostics.
%%--------------------------------------------------------------------------

-doc(#{group => <<"Info API">>}).
-doc """
Given an error return value `{error, AtomReason}`, this function returns a
readable string describing the error.
""".
-spec formaterror(Tag :: atom() | {error, atom()}) -> string().

formaterror(Tag) ->
  ftp_response:error_string(Tag).


-doc false.
info(Pid) ->
    ftp_internal:info(Pid).


%%--------------------------------------------------------------------------
%% Description:  The latest received response from the server
%%--------------------------------------------------------------------------

-doc false.
-spec latest_ctrl_response(Client ::client()) -> string().

latest_ctrl_response(Pid) ->
    ftp_internal:latest_ctrl_response(Pid).

