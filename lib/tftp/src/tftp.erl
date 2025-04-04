%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 2005-2025. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File    : tftp.erl
%%% Author  : Hakan Mattsson <hakan@erix.ericsson.se>
%%% Description : Trivial FTP
%%% Created : 18 May 2004 by Hakan Mattsson <hakan@erix.ericsson.se>
%%%-------------------------------------------------------------------
-module(tftp).
-moduledoc """
Trivial FTP.

Interface module for the `tftp` application.

## Overwiew

This is a complete implementation of the following IETF standards:
    RFC 1350, The TFTP Protocol (revision 2).
    RFC 2347, TFTP Option Extension.
    RFC 2348, TFTP Blocksize Option.
    RFC 2349, TFTP Timeout Interval and Transfer Size Options.

The only feature that not is implemented in this release is
the "netascii" transfer mode.

The [start](`start/1`) function starts a daemon process which, listens
for UDP packets on a port. When it receives a request for read or
write it spawns a temporary server process which handles the actual
transfer of the file. On the client side the
[read_file/3](`read_file/3`) and [write_file/3](`write_file/3`)
functions spawns a temporary client process which establishes contact
with a TFTP daemon and performs the actual transfer of the file.

Most of the options are common for both the client and the server
side, but some of them differs a little.

## Callbacks

A `tftp` callback module is to be implemented as a `tftp` behavior and export
the functions listed in the following.

On the server side, the callback interaction starts with a call to `open/5` with
the registered initial callback state. `open/5` is expected to open the
(virtual) file. Then either function [`Module:read/1`](`c:read/1`) or
[`Module:write/2`](`c:write/2`) is invoked repeatedly, once per transferred block. At
each function call, the state returned from the previous call is obtained. When
the last block is encountered, function [`Module:read/1`](`c:read/1`) or
[`Module:write/2`](`c:write/2`) is expected to close the (virtual) file and return its
last state. Function [`Module:abort/3`](`c:abort/3`) is only used in error situations.
Function `prepare/5` is not used on the server side.

On the client side, the callback interaction is the same, but it starts and ends
a bit differently. It starts with a call to `prepare/5` with the same arguments
as `open/5` takes. `prepare/5` is expected to validate the TFTP options
suggested by the user and to return the subset of them that it accepts. Then the
options are sent to the server, which performs the same TFTP option negotiation
procedure. The options that are accepted by the server are forwarded to function
`open/5` on the client side. On the client side, function `open/5` must accept
all option as-is or reject the transfer. Then the callback interaction follows
the same pattern as described for the server side. When the last block is
encountered in [`Module:read/1`](`c:read/1`) or [`Module:write/2`](`c:write/2`), the returned
state is forwarded to the user and returned from `read_file`/3 or
[`write_file/3`](`write_file/3`).

If a callback (performing the file access in the TFTP server) takes too long
time (more than the double TFTP time-out), the server aborts the connection and
sends an error reply to the client. The server simply
assumes that the client has given up.

If the TFTP server receives yet another request from the same client (same host
and port) while it already has an active connection to the client, it ignores
the new request if the request is equal to the first one (same filename and
options). This implies that the (new) client will be served by the already
ongoing connection on the server side. By not setting up yet another connection,
in parallel with the ongoing one, the server consumes less resources.

""".

%%-------------------------------------------------------------------
%% Interface
%%-------------------------------------------------------------------

%% Public functions
-export([
	 read_file/3,
	 write_file/3,
	 start/1,
	 info/1,
	 change_config/2,
	 start/0,
         stop/0
	]).

-export_type([option/0]).

-include("tftp.hrl").

-doc """
Information about the peer provided for callback.
""".
-type peer() :: {PeerType :: inet | inet6,
		 PeerHost :: inet:ip_address(),
		 PeerPort :: port()}.
-doc """
Access mode.
""".
-type access() :: read | write.

-doc """
All options most of them common to the client and server.

- **`{debug, Level::none | error | warning | brief | normal | verbose | all}`**

  Controls the level of debug printouts. Default is `none`.

- **`{host, Host::inet:hostname()}`** -

  The name or IP address of the host where the TFTP daemon resides. This option
  is only used by the client.

- **`{port, Port::inet:port_number()}`**

  The TFTP port where the daemon listens. Defaults is the standardized
  number 69. On the server side, it can sometimes make sense to set it to 0,
  meaning that the daemon just picks a free port (which one is returned by
  function [`info/1`](`info/1`)).

  If a socket is connected already, option `{udp, [{fd, integer()}]}` can be
  used to pass the open file descriptor to `gen_udp`. This can be automated by
  using a command-line argument stating the prebound file descriptor number. For
  example, if the port is 69 and file descriptor 22 is opened by
  `setuid_socket_wrap`, the command-line argument "-tftpd_69 22" triggers the
  prebound file descriptor 22 to be used instead of opening port 69. The UDP
  option `{udp, [{fd, 22}]}` is automatically added. See `init:get_argument/`
  about command-line arguments and `gen_udp:open/2` about UDP options.

- **`{port_policy, random | inet:port_number() | {range, Min::inet:port_number(), Max::inet:port_nuber()}`**

  Policy for the selection of the temporary port that is used by the
  server/client during the file transfer. Default is `random`, which is the
  standardized policy. With this policy a randomized free port is used. A single
  port or a range of ports can be useful if the protocol passes through a
  firewall.

- **`{udp, Options::gen_udp:option}`**

- **`{use_tsize, boolean()}`**

  Flag for automated use of option `tsize`. With this set to `true`, the
  [`write_file/3`](`write_file/3`) client determines the filesize and sends it
  to the server as the standardized `tsize` option. A
  [`read_file/3`](`read_file/3`) client acquires only a filesize from the server
  by sending a zero `tsize`.

- **`{max_tsize, MaxTsize::pos_integer() | infinity}`**

  Threshold for the maximal filesize in bytes. The transfer is aborted if the
  limit is exceeded. Default is `infinity`.

- **`{max_conn, MaxConn::pos_integer() | infinity}`**

  Threshold for the maximal number of active connections. The daemon rejects the
  setup of new connections if the limit is exceeded. Default is `infinity`.

- **TftpOption::option()**

  Name and value of a TFTP option.

- **`{reject, Feature:: access() | TftpKey::string()}`**

  Controls which features to reject. This is mostly useful for the server as it
  can restrict the use of certain TFTP options or read/write access.

- **`{callback, {RegExp ::string(), Module::module(), State :: term()}}`**

  Registration of a callback module. When a file is to be transferred, its local
  filename is matched to the regular expressions of the registered callbacks.
  The first matching callback is used during the transfer. See `read_file/3` and
  `write_file/3`.

  The callback module must implement the `tftp` behavior, see
  [callbacks](`m:tftp#callbacks`).

- **`{logger, module()}`**

  Callback module for customized logging of errors, warnings, and info messages.
  The callback module must implement the `m:tftp_logger` behavior. The default
  module is `tftp_logger`.

- **`{max_retries, MaxRetries::non_neg_integer()}`**

  Threshold for the maximal number of retries. By default the server/client
  tries to resend a message up to five times when the time-out expires.
""".
-type connection_option() :: {atom(), term()} | option().

-doc """
Error reason codes.
""".
-type error_code() :: undef | enoent | eacces | enospc |
		      badop | eexist | baduser | badopt |
		      pos_integer().


-doc """
Specific TFTP protocol options
""".
-type option() :: {string(), Value :: string()}.


-doc """
Prepares to open a file on the client side.

No new options can be added, but those present in `SuggestedOptions` can be
omitted or replaced with new values in `AcceptedOptions`.

This is followed by a call to `open/4` before any read/write access is
performed. `AcceptedOptions` is sent to the server, which replies with the
options that it accepts. These are then forwarded to `open/4` as
`SuggestedOptions`.

""".
-doc(#{since => <<"OTP 18.1">>}).

-callback prepare(Peer :: peer(),
		  Access :: access(),
		  Filename :: file:name(),
		  Mode :: string(),
		  SuggestedOptions :: [option()],
		  InitialState :: [] | [{root_dir, string()}]) ->
    {ok, AcceptedOptions :: [option()], NewState :: term()} |
    {error, {Code :: error_code(), string()}}.

-doc """
Opens a file for read or write access.

On the client side, where the `open/5` call has been preceded by a call to
`prepare/5`, all options must be accepted or rejected.

On the server side, where there is no preceding `prepare/5` call, no new options
can be added, but those present in `SuggestedOptions` can be omitted or replaced
with new values in `AcceptedOptions`.

""".

-doc(#{group => <<"Client API">>,
       since => <<"OTP 18.1">>}).
-callback open(Peer :: peer(),
	       Access :: access(),
	       Filename :: file:name(),
	       Mode :: string(),
	       SuggestedOptions :: [option()],
	       State :: [] | [{root_dir, string()}] | term()) ->
    {ok, AcceptedOptions :: [option()], NewState :: term()} |
    {error, {Code :: error_code(), string()}}.

-doc """
Reads a chunk from the file.

The callback function is expected to close the file when the last file chunk is
encountered. When an error is encountered, the callback function is expected to
clean up after the aborted file transfer, such as closing open file descriptors,
and so on. In both cases there will be no more calls to any of the callback
functions.

""".
-doc(#{since => <<"OTP 18.1">>}).
-callback read(State :: term()) -> {more, binary(), NewState :: term()} |
				   {last, binary(), integer()} |
				   {error, {Code :: error_code(), string()}}.

-doc """
Writes a chunk to the file.

The callback function is expected to close the file when the last file chunk is
encountered. When an error is encountered, the callback function is expected to
clean up after the aborted file transfer, such as closing open file descriptors,
and so on. In both cases there will be no more calls to any of the callback
functions.

""".
-doc(#{since => <<"OTP 18.1">>}).
-callback write(binary(), State :: term()) ->
    {more, NewState :: term()} |
    {last, FileSize :: integer()} |
    {error, {Code :: error_code(), string()}}.

-doc """
Invoked when the file transfer is aborted.

The callback function is expected to clean up its used resources after the
aborted file transfer, such as closing open file descriptors and so on. The
function is not invoked if any of the other callback functions returns an error,
as it is expected that they already have cleaned up the necessary resources.
However, it is invoked if the functions fail (crash).
""".
-doc(#{since => <<"OTP 18.1">>}).
-callback abort(Code :: error_code(), string(), State :: term()) -> 'ok'.

-doc(#{group => <<"Client API">>}).
-doc """
Reads a (virtual) file `RemoteFilename` from a TFTP server.

If `LocalFilename` is the atom `binary`, `tftp_binary` is used as callback
module. It concatenates all transferred blocks and returns them as one single
binary in `LastCallbackState`.

If `LocalFilename` is a string and there are no registered callback modules,
`tftp_file` is used as callback module. It writes each transferred block to the
file named `LocalFilename` and returns the number of transferred bytes in
`LastCallbackState`.

If `LocalFilename` is a string and there are registered callback modules,
`LocalFilename` is tested against the regexps of these and the callback module
corresponding to the first match is used, or an error tuple is returned if no
matching regexp is found.
""".

-spec read_file(RemoteFilename, LocalFilename, Options) ->
        {ok, LastCallbackState} | {error, Reason} when
    RemoteFilename    :: file:filename(),
    LocalFilename     :: file:filename_all() | 'binary',
    Options           :: [connection_option()],
    LastCallbackState :: term(),
    Reason            :: term().

read_file(RemoteFilename, LocalFilename, Options) ->
    tftp_engine:client_start(read, RemoteFilename, LocalFilename, Options).
    

-doc(#{group => <<"Client API">>}).
-doc """
Writes a (virtual) file `RemoteFilename` to a TFTP server.

If `LocalFilename` is a binary, `tftp_binary` is used as callback module. The
binary is transferred block by block and the number of transferred bytes is
returned in `LastCallbackState`.

If `LocalFilename` is a string and there are no registered callback modules,
`tftp_file` is used as callback module. It reads the file named `LocalFilename`
block by block and returns the number of transferred bytes in
`LastCallbackState`.

If `LocalFilename` is a string and there are registered callback modules,
`LocalFilename` is tested against the regexps of these and the callback module
corresponding to the first match is used, or an error tuple is returned if no
matching regexp is found.
""".

-spec write_file(RemoteFilename, LocalFilename, Options) ->
        {ok, LastCallbackState} | {error, Reason} when
    RemoteFilename    :: file:filename(),
    LocalFilename     :: file:filename_all() | binary,
    Options           :: [option()],
    LastCallbackState :: term(),
    Reason            :: term().

write_file(RemoteFilename, LocalFilename, Options) ->
    tftp_engine:client_start(write, RemoteFilename, LocalFilename, Options).

-doc(#{group => <<"Server API">>}).
-doc """
Starts a daemon process listening for UDP packets on a port.

When it receives a request for read or write, it spawns a temporary
server process handling the actual transfer of the (virtual) file.
""".

-spec start(Options) -> {ok, Pid} | {error, Reason} when
    Options :: [connection_option()],
    Pid     :: pid(),
    Reason  :: term().

start(Options) ->
    tftp_engine:daemon_start(Options).


-doc(#{group => <<"Server API">>}).
-doc """
Returns information about all TFTP server.
""".
info(Pid) ->
    tftp_engine:info(Pid).


-doc(#{group => <<"Server API">>}).
-doc """
Changes configuration a TFTP Server
""".
change_config(Pid, Options) ->
    tftp_engine:change_config(Pid, Options).


-doc false.
start() ->
    application:start(tftp).

-doc false.
stop() ->
    application:stop(tftp).

