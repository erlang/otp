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

%%% Description: SFTP protocol front-end

-module(ssh_sftp).
-moduledoc """
SFTP client.

This module implements an SSH FTP (SFTP) client. SFTP is a secure, encrypted
file transfer service available for SSH.
""".

-behaviour(ssh_client_channel).

-include_lib("kernel/include/file.hrl").
-include("ssh.hrl").
-include("ssh_xfer.hrl").

%% API

-export([start_channel/1, start_channel/2, start_channel/3, stop_channel/1]).

-export([open/3, open_tar/3, opendir/2, close/2, readdir/2, pread/4, read/3,
         open/4, open_tar/4, opendir/3, close/3, readdir/3, pread/5, read/4,
	 apread/4, aread/3, pwrite/4, write/3, apwrite/4, awrite/3,
	 pwrite/5, write/4,
	 position/3, real_path/2, read_file_info/2, get_file_info/2,
	 position/4, real_path/3, read_file_info/3, get_file_info/3,
	 write_file_info/3, read_link_info/2, read_link/2, make_symlink/3,
	 write_file_info/4, read_link_info/3, read_link/3, make_symlink/4,
	 rename/3, delete/2, make_dir/2, del_dir/2, send_window/1,
	 rename/4, delete/3, make_dir/3, del_dir/3, send_window/2,
	 recv_window/1, list_dir/2, read_file/2, write_file/3,
	 recv_window/2, list_dir/3, read_file/3, write_file/4]).

%% ssh_client_channel callbacks
-export([init/1, handle_call/3, handle_cast/2, code_change/3, handle_msg/2, handle_ssh_msg/2, terminate/2]).
%% TODO: Should be placed elsewhere ssh_sftpd should not call functions in ssh_sftp!
-export([info_to_attr/1, attr_to_info/1]).

-behaviour(ssh_dbg).
-export([ssh_dbg_trace_points/0, ssh_dbg_flags/1, ssh_dbg_on/1, ssh_dbg_off/1, ssh_dbg_format/2]).

-record(state,
	{
	  xf,
	  rep_buf = <<>> :: binary(),
	  req_id,
	  req_list = [], %% {ReqId, Fun}
	  inf,   %% list of fileinf,
	  opts
	 }).

-record(fileinf,
	{
	  handle,
	  offset,
	  size,
	  mode
	 }).

-record(bufinf,
	{
	  mode                  :: read | write, % read | write  (=from or to buffer by user)
	  crypto_state          :: term() | undefined,
	  crypto_fun,            % For encode or decode depending on the mode field
	  size = 0              :: non_neg_integer() | undefined, % # bytes "before" the current buffer for the position call

	  chunksize             :: non_neg_integer() | undefined, % The size of the chunks to be sent or received
	  enc_text_buf = <<>>   :: binary() | undefined,          % Encrypted text
	  plain_text_buf = <<>> :: binary() | undefined           % Decrypted text
	}).

-define(FILEOP_TIMEOUT, infinity).

-define(NEXT_REQID(S),
	S#state { req_id = (S#state.req_id + 1) band 16#ffffffff}).

-define(XF(S), S#state.xf).
-define(REQID(S), S#state.req_id).

-doc """
Specifies available SFTP options.
""".
-type sftp_option() :: {timeout, timeout()}
                     | {sftp_vsn, pos_integer()}
                     | {window_size, pos_integer()}
                     | {packet_size, pos_integer()} .

%% For ct_ssh:connect
-export_type([sftp_option/0]).

-doc """
A description of the reason why an operation failed.

The `t:atom/0` value is formed from the sftp error codes in the protocol-level
responses as defined in
[draft-ietf-secsh-filexfer-13](https://tools.ietf.org/html/draft-ietf-secsh-filexfer-13#page-49)
section 9.1. The codes are named as `SSH_FX_*` which are transformed into
lowercase of the star-part. E.g. the error code `SSH_FX_NO_SUCH_FILE` will cause
the `t:reason/0` to be `no_such_file`.

The `t:string/0` reason is the error information from the server in case of an
exit-signal. If that information is empty, the reason is the exit signal name.

The `t:tuple/0` reason are other errors like for example `{exit_status,1}`.
""".
-type reason() :: atom() | string() | tuple() .

%%====================================================================
%% API
%%====================================================================
-doc(#{equiv => start_channel/2}).
-spec start_channel(ssh:open_socket() | ssh:connection_ref() | ssh:host()) ->
          {ok, pid()} | {ok, pid(), ssh:connection_ref()} | {error, reason()}.
start_channel(Dest) ->
    start_channel(Dest, []).

%%% -spec:s are as if Dialyzer handled signatures for separate
%%% function clauses.
-doc """
Starts new ssh channel for communicating with the SFTP server.

Starts an ssh channel when first argument is a connection reference.

Equivalent to [start_channel(Host, 22, UserOptions)](`start_channel/3`) when
first argument is recognized as network host.

Otherwise, first argument is treated as a network socket which will be used for
establishing new SSH connection. New connection reference will be used for
starting an SSH channel.

The returned `pid` for this process is to be used as input to all other API
functions in this module.

See also (`start_channel/3`).

""".
-spec start_channel(ssh:open_socket(), [ssh:client_option() | sftp_option()]) ->
          {ok, pid(), ssh:connection_ref()} | {error,reason()};
                   (ssh:connection_ref(), [ssh:client_option() | sftp_option()]) ->
          {ok, pid()}  | {ok, pid(), ssh:connection_ref()} | {error, reason()};
                   (ssh:host(), [ssh:client_option() | sftp_option()]) ->
          {ok, pid(), ssh:connection_ref()} | {error, reason()}.
start_channel(Cm, UserOptions0) when is_pid(Cm) ->
    UserOptions = legacy_timeout(UserOptions0),
    Timeout = proplists:get_value(timeout, UserOptions, infinity),
    {_SshOpts, ChanOpts, SftpOpts} = handle_options(UserOptions),
    WindowSize = proplists:get_value(window_size, ChanOpts, ?XFER_WINDOW_SIZE),
    PacketSize = proplists:get_value(packet_size, ChanOpts, ?XFER_PACKET_SIZE),
    case ssh_connection:session_channel(Cm, WindowSize, PacketSize, Timeout) of
	{ok, ChannelId} ->
            case ssh_connection_handler:start_channel(Cm, ?MODULE, ChannelId,
                                                      [Cm,ChannelId,SftpOpts], undefined) of
		{ok, Pid} ->
		    case wait_for_version_negotiation(Pid, Timeout) of
			ok ->
			    {ok, Pid};
			TimeOut ->
			    TimeOut
		    end;
		{error, Reason} ->
		    {error, format_channel_start_error(Reason)}
	    end;
	Error ->
	    Error
    end;
start_channel(Dest, UserOptions0) ->
    UserOptions = legacy_timeout(UserOptions0),
    {SshOpts, ChanOpts, SftpOpts} = handle_options(UserOptions),
    case ssh:is_host(Dest, SshOpts) of
        true ->
            %% Dest looks like is a Host
            start_channel(Dest, 22, UserOptions);
        false ->
            %% No, it is probably not a Host, must be a socket
            Socket = Dest,
            Timeout = proplists:get_value(connect_timeout, SshOpts, infinity),
            case ssh:connect(Socket, SshOpts, Timeout) of
                {ok,Cm} ->
                    case start_channel(Cm, ChanOpts ++ SftpOpts) of
                        {ok, Pid} ->
                            {ok, Pid, Cm};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end
    end.

-doc """
Starts new ssh connection and channel for communicating with the SFTP server.

The returned `pid` for this process is to be
used as input to all other API functions in this module.

Options:

- **`{timeout, timeout()}`** - There are two ways to set a timeout for the
  underlying ssh connection:

  - If the connection timeout option `connect_timeout` is set, that value is
    used also for the negotiation timeout and this option (`timeout`) is
    ignored.
  - Otherwise, this option (`timeout`) is used as the negotiation timeout only
    and there is no connection timeout set

  The value defaults to `infinity`.

- **`{sftp_vsn, integer()}`** - Desired SFTP protocol version. The actual
  version is the minimum of the desired version and the maximum supported
  versions by the SFTP server.

All other options are directly passed to [ssh:connect/3](`m:ssh`) or ignored if
a connection is already provided.
""".
-spec start_channel(ssh:host(),
                    inet:port_number(),
                    [ssh:client_option() | sftp_option()]
                   )
                   -> {ok,pid(),ssh:connection_ref()} | {error,reason()}.
start_channel(Host, Port, UserOptions0) ->
    UserOptions = legacy_timeout(UserOptions0),
    Timeout = proplists:get_value(connect_timeout, UserOptions, infinity),
    {SshOpts, _ChanOpts, _SftpOpts} = handle_options(UserOptions),
    case ssh:connect(Host, Port, SshOpts, Timeout) of
	{ok, Cm} ->
            case start_channel(Cm, UserOptions) of
                {ok, Pid} ->
                    {ok, Pid, Cm};
                Error ->
                    Error
            end;
	{error, Timeout} ->
            {error, timeout};
	Error ->
	    Error
    end.

%%% Helper for start_channel

wait_for_version_negotiation(Pid, Timeout) ->
    call(Pid, wait_for_version_negotiation, Timeout).

%%%----------------------------------------------------------------
-doc """
Stops an SFTP channel. Does not close the SSH connection. Use `ssh:close/1` to
close it.
""".
-spec stop_channel(ChannelPid) -> ok when
      ChannelPid :: pid().

stop_channel(Pid) ->
    case is_process_alive(Pid) of
	true ->
            MonRef = erlang:monitor(process, Pid),
            unlink(Pid),
            exit(Pid, ssh_sftp_stop_channel),
            receive {'DOWN',MonRef,_,_,_} -> ok
            after
                1000 ->
                    erlang:demonitor(MonRef, [flush]),
                    exit(Pid, kill),
                    ok
            end;
	false ->
	    ok
    end.

%%%----------------------------------------------------------------
-doc(#{equiv => open/4}).
-spec open(ChannelPid, Name, Mode) -> {ok, Handle} | Error when
      ChannelPid :: pid(),
      Name :: string(),
      Mode :: [read | write | append | binary | raw],
      Handle :: term(),
      Error :: {error, reason()} .
open(Pid, File, Mode) ->
    open(Pid, File, Mode, ?FILEOP_TIMEOUT).
-doc """
Opens a file on the server and returns a handle, which can be used for reading
or writing.
""".
-spec open(ChannelPid, Name, Mode, Timeout) -> {ok, Handle} | Error when
      ChannelPid :: pid(),
      Name :: string(),
      Mode :: [read | write | append | binary | raw],
      Timeout :: timeout(),
      Handle :: term(),
      Error :: {error, reason()} .
open(Pid, File, Mode, FileOpTimeout) ->
    call(Pid, {open, false, File, Mode}, FileOpTimeout).


-doc(#{group => <<"Crypto open_tar">>}).
-type tar_crypto_spec() :: encrypt_spec() | decrypt_spec() .

-doc(#{group => <<"Crypto open_tar">>}).
-type encrypt_spec() :: {init_fun(), crypto_fun(), final_fun()} .
-doc """
Specifies the encryption or decryption applied to tar files when using
`open_tar/3` or `open_tar/4`.

The encryption or decryption is applied to the generated stream of bytes prior
to sending the resulting stream to the SFTP server.

For code examples see Section
[Example with encryption](using_ssh.md#example-with-encryption) in the ssh Users
Guide.
""".
-doc(#{group => <<"Crypto open_tar">>}).
-type decrypt_spec() :: {init_fun(), crypto_fun()} .

-doc(#{group => <<"Crypto open_tar">>}).
-type init_fun() :: fun(() -> {ok,crypto_state()})
                  | fun(() -> {ok,crypto_state(),chunk_size()}) .

-doc(#{group => <<"Crypto open_tar">>}).
-type crypto_fun() :: fun((TextIn::binary(), crypto_state()) -> crypto_result()) .
-doc """
The initial `t:crypto_state/0` returned from the `t:init_fun/0` is folded into
repeated applications of the `t:crypto_fun/0` in the
[tar_crypto_spec](`t:tar_crypto_spec/0`). The binary returned from that fun is
sent to the remote SFTP server and the new `t:crypto_state/0` is used in the
next call of the `t:crypto_fun/0`.

If the `t:crypto_fun/0` reurns a `t:chunk_size/0`, that value is as block size
for further blocks in calls to `t:crypto_fun/0`.
""".
-doc(#{group => <<"Crypto open_tar">>}).
-type crypto_result() :: {ok,TextOut::binary(),crypto_state()}
                       | {ok,TextOut::binary(),crypto_state(),chunk_size()} .

-doc """
If doing encryption, the `t:final_fun/0` in the
[tar_crypto_spec](`t:tar_crypto_spec/0`) is applied to the last piece of data.
The `t:final_fun/0` is responsible for padding (if needed) and encryption of
that last piece.
""".
-doc(#{group => <<"Crypto open_tar">>}).
-type final_fun() :: fun((FinalTextIn::binary(),crypto_state()) -> {ok,FinalTextOut::binary()}) .

-doc(#{group => <<"Crypto open_tar">>}).
-type chunk_size() :: undefined | pos_integer().
-doc """
The `t:init_fun/0` in the [tar_crypto_spec](`t:tar_crypto_spec/0`) is applied
once prior to any other `crypto` operation. The intention is that this function
initiates the encryption or decryption for example by calling
`crypto:crypto_init/4` or similar. The `t:crypto_state/0` is the state such a
function may return.

If the selected cipher needs to have the input data partitioned into blocks of a
certain size, the `t:init_fun/0` should return the second form of return value
with the `t:chunk_size/0` set to the block size. If the `t:chunk_size/0` is
`undefined`, the size of the `PlainBin`s varies, because this is intended for
stream crypto, whereas a fixed `t:chunk_size/0` is intended for block crypto. A
`t:chunk_size/0` can be changed in the return from the `t:crypto_fun/0`. The
value can be changed between `t:pos_integer/0` and `undefined`.
""".
-doc(#{group => <<"Crypto open_tar">>}).
-type crypto_state() :: any() .


-doc(#{equiv => open_tar/4}).
-doc(#{since => <<"OTP 17.4">>}).
-spec open_tar(ChannelPid, Path, Mode) -> {ok, Handle} | Error when
      ChannelPid :: pid(),
      Path :: string(),
      Mode :: [read | write | {crypto, tar_crypto_spec()} ],
      Handle :: term(),
      Error :: {error, reason()} .
open_tar(Pid, File, Mode) ->
    open_tar(Pid, File, Mode, ?FILEOP_TIMEOUT).
-doc """
Opens a handle to a tar file on the server, associated with `ChannelPid`. The
handle can be used for remote tar creation and extraction. The actual writing
and reading is performed by calls to [erl_tar:add/3,4](`erl_tar:add/3`) and
`erl_tar:extract/2`. Note: The `erl_tar:init/3` function should not be called,
that one is called by this open_tar function.

For code examples see Section
[SFTP Client with TAR Compression](using_ssh.md#sftp-client-with-tar-compression)
in the ssh Users Guide.

The `crypto` mode option is explained in the data types section above, see
[Crypto operations for open_tar](`m:ssh_sftp#types-crypto-open_tar`).
Encryption is assumed if the `Mode` contains `write`, and decryption if the
`Mode` contains `read`.
""".
-doc(#{since => <<"OTP 17.4">>}).
-spec open_tar(ChannelPid, Path, Mode, Timeout) -> {ok, Handle} | Error when
      ChannelPid :: pid(),
      Path :: string(),
      Mode :: [read | write | {crypto, tar_crypto_spec()} ],
      Timeout :: timeout(),
      Handle :: term(),
      Error :: {error, reason()} .
open_tar(Pid, File, Mode, FileOpTimeout) ->
    case {lists:member(write,Mode),
	  lists:member(read,Mode),
	  Mode -- [write,read]} of
	{true,false,[]} ->
	    {ok,Handle} = open(Pid, File, [write], FileOpTimeout),
	    erl_tar:init(Pid, write,
			 fun(write, {_,Data}) ->
				 write_to_remote_tar(Pid, Handle, to_bin(Data), FileOpTimeout);
			    (position, {_,Pos}) ->
				 position(Pid, Handle, Pos, FileOpTimeout);
			    (close, _) ->
				 close(Pid, Handle, FileOpTimeout)
			 end);
	{true,false,[{crypto,{CryptoInitFun,CryptoEncryptFun,CryptoEndFun}}]} ->
	    {ok,SftpHandle} = open(Pid, File, [write], FileOpTimeout),
	    BI = #bufinf{mode = write,
			 crypto_fun = CryptoEncryptFun},
	    {ok,BufHandle} = open_buf(Pid, CryptoInitFun, BI, FileOpTimeout),
	    erl_tar:init(Pid, write,
			 fun(write, {_,Data}) ->
				 write_buf(Pid, SftpHandle, BufHandle,  to_bin(Data), FileOpTimeout);
			    (position, {_,Pos}) ->
				 position_buf(Pid, SftpHandle, BufHandle, Pos, FileOpTimeout);
			    (close, _) ->
				 {ok,#bufinf{
					plain_text_buf = PlainBuf0,
					enc_text_buf = EncBuf0,
					crypto_state = CState0
				       }}  = call(Pid, {get_bufinf,BufHandle}, FileOpTimeout),
				 {ok,EncTextTail} = CryptoEndFun(PlainBuf0, CState0),
				 EncTextBuf = <<EncBuf0/binary, EncTextTail/binary>>,
				 case write(Pid, SftpHandle, EncTextBuf, FileOpTimeout) of
				     ok ->
					 call(Pid, {erase_bufinf,BufHandle}, FileOpTimeout),
					 close(Pid, SftpHandle, FileOpTimeout);
				     Other ->
					 Other
				 end
			 end);
	{false,true,[]} ->
	    {ok,Handle} = open(Pid, File, [read,binary], FileOpTimeout),
	    erl_tar:init(Pid, read,
			 fun(read2, {_,Len}) ->
				 read_repeat(Pid, Handle, Len, FileOpTimeout);
			    (position, {_,Pos}) ->
				 position(Pid, Handle, Pos, FileOpTimeout);
			    (close, _) ->
				 close(Pid, Handle, FileOpTimeout)
			 end);
	{false,true,[{crypto,{CryptoInitFun,CryptoDecryptFun}}]} ->
	    {ok,SftpHandle} = open(Pid, File, [read,binary], FileOpTimeout),
	    BI = #bufinf{mode = read,
			 crypto_fun = CryptoDecryptFun},
	    {ok,BufHandle} = open_buf(Pid, CryptoInitFun, BI, FileOpTimeout),
	    erl_tar:init(Pid, read,
			 fun(read2, {_,Len}) ->
				 read_buf(Pid, SftpHandle, BufHandle, Len, FileOpTimeout);
			    (position, {_,Pos}) ->
				 position_buf(Pid, SftpHandle, BufHandle, Pos, FileOpTimeout);
			    (close, _) ->
				 call(Pid, {erase_bufinf,BufHandle}, FileOpTimeout),
				 close(Pid, SftpHandle, FileOpTimeout)
                         end);
	_ ->
	    {error,{illegal_mode,Mode}}
    end.


-doc(#{equiv => opendir/3}).
-spec opendir(ChannelPid, Path) -> {ok, Handle} | Error when
      ChannelPid :: pid(),
      Path :: string(),
      Handle :: term(),
      Error :: {error, reason()} .
opendir(Pid, Path) ->
    opendir(Pid, Path, ?FILEOP_TIMEOUT).
-doc """
Opens a handle to a directory on the server. The handle can be used for reading
directory contents.
""".
-spec opendir(ChannelPid, Path, Timeout) -> {ok, Handle} | Error when
      ChannelPid :: pid(),
      Path :: string(),
      Timeout :: timeout(),
      Handle :: term(),
      Error :: {error, reason()} .
opendir(Pid, Path, FileOpTimeout) ->
    call(Pid, {opendir, false, Path}, FileOpTimeout).

-doc(#{equiv => close/3}).
-spec close(ChannelPid, Handle) -> ok | Error when
      ChannelPid :: pid(),
      Handle :: term(),
      Error :: {error, reason()} .
close(Pid, Handle) ->
    close(Pid, Handle, ?FILEOP_TIMEOUT).
-doc "Closes a handle to an open file or directory on the server.".
-spec close(ChannelPid, Handle, Timeout) -> ok | Error when
      ChannelPid :: pid(),
      Handle :: term(),
      Timeout :: timeout(),
      Error :: {error, reason()} .
close(Pid, Handle, FileOpTimeout) ->
    call(Pid, {close,false,Handle}, FileOpTimeout).

-doc false.
readdir(Pid,Handle) ->
    readdir(Pid,Handle, ?FILEOP_TIMEOUT).
-doc false.
readdir(Pid,Handle, FileOpTimeout) ->
    call(Pid, {readdir,false,Handle}, FileOpTimeout).

-doc(#{equiv => pread/5}).
-spec pread(ChannelPid, Handle, Position, Len) -> {ok, Data} | eof | Error when
      ChannelPid :: pid(),
      Handle :: term(),
      Position :: integer(),
      Len :: integer(),
      Data :: string() | binary(),
      Error :: {error, reason()}.
pread(Pid, Handle, Offset, Len) ->
    pread(Pid, Handle, Offset, Len, ?FILEOP_TIMEOUT).

-doc """
The `pread/3,4` function reads from a specified position, combining the
`position/3` and [`read/3,4`](`read/3`) functions.
""".
-spec pread(ChannelPid, Handle, Position, Len, Timeout) -> {ok, Data} | eof | Error when
      ChannelPid :: pid(),
      Handle :: term(),
      Position :: integer(),
      Len :: integer(),
      Timeout :: timeout(),
      Data :: string() | binary(),
      Error :: {error, reason()}.
pread(Pid, Handle, Offset, Len, FileOpTimeout) ->
    call(Pid, {pread,false,Handle, Offset, Len}, FileOpTimeout).


-doc(#{equiv => read/4}).
-spec read(ChannelPid, Handle, Len) -> {ok, Data} | eof | Error when
      ChannelPid :: pid(),
      Handle :: term(),
      Len :: integer(),
      Data :: string() | binary(),
      Error :: {error, reason()}.
read(Pid, Handle, Len) ->
    read(Pid, Handle, Len, ?FILEOP_TIMEOUT).

-doc """
Reads `Len` bytes from the file referenced by `Handle`. Returns `{ok, Data}`,
`eof`, or `{error, reason()}`. If the file is opened with `binary`, `Data` is a
binary, otherwise it is a string.

If the file is read past `eof`, only the remaining bytes are read and returned.
If no bytes are read, `eof` is returned.
""".
-spec read(ChannelPid, Handle, Len, Timeout) -> {ok, Data} | eof | Error when
      ChannelPid :: pid(),
      Handle :: term(),
      Len :: integer(),
      Timeout :: timeout(),
      Data :: string() | binary(),
      Error :: {error, reason()}.
read(Pid, Handle, Len, FileOpTimeout) ->
    call(Pid, {read,false,Handle, Len}, FileOpTimeout).


%% TODO this ought to be a cast! Is so in all practical meaning
%% even if it is obscure!
-doc """
The [`apread/4`](`apread/4`) function reads from a specified position, combining
the `position/3` and `aread/3` functions.
""".
-spec apread(ChannelPid, Handle, Position, Len) -> {async, N} | Error when
      ChannelPid :: pid(),
      Handle :: term(),
      Position :: integer(),
      Len :: integer(),
      Error :: {error, reason()},
      N :: term() .
apread(Pid, Handle, Offset, Len) ->
    call(Pid, {pread,true,Handle, Offset, Len}, infinity).

%% TODO this ought to be a cast! 
-doc """
Reads from an open file, without waiting for the result. If the handle is valid,
the function returns `{async, N}`, where `N` is a term guaranteed to be unique
between calls of `aread`. The actual data is sent as a message to the calling
process. This message has the form `{async_reply, N, Result}`, where `Result` is
the result from the read, either `{ok, Data}`, `eof`, or `{error, reason()}`.
""".
-spec aread(ChannelPid, Handle, Len) -> {async, N} | Error when
      ChannelPid :: pid(),
      Handle :: term(),
      Len :: integer(),
      Error :: {error, reason()},
      N :: term() .
aread(Pid, Handle, Len) ->
    call(Pid, {read,true,Handle, Len}, infinity).


-doc(#{equiv => pwrite/5}).
-spec pwrite(ChannelPid, Handle, Position, Data) -> ok | Error when
      ChannelPid :: pid(),
      Handle :: term(),
      Position :: integer(),
      Data :: iolist(),
      Error :: {error, reason()}.
pwrite(Pid, Handle, Offset, Data) ->
    pwrite(Pid, Handle, Offset, Data, ?FILEOP_TIMEOUT).

-doc """
The `pwrite/3,4` function writes to a specified position, combining the
`position/3` and [`write/3,4`](`write/3`) functions.
""".
-spec pwrite(ChannelPid, Handle, Position, Data, Timeout) -> ok | Error when
      ChannelPid :: pid(),
      Handle :: term(),
      Position :: integer(),
      Data :: iolist(),
      Timeout :: timeout(),
      Error :: {error, reason()}.
pwrite(Pid, Handle, Offset, Data, FileOpTimeout) ->
    call(Pid, {pwrite,false,Handle,Offset,Data}, FileOpTimeout).


-doc(#{equiv => write/4}).
-spec write(ChannelPid, Handle, Data) -> ok | Error when
      ChannelPid :: pid(),
      Handle :: term(),
      Data :: iodata(),
      Error :: {error, reason()}.
write(Pid, Handle, Data) ->
    write(Pid, Handle, Data, ?FILEOP_TIMEOUT).

-doc """
Writes `data` to the file referenced by `Handle`. The file is to be opened with
`write` or `append` flag. Returns `ok` if successful or `{error, reason()}`
otherwise.
""".
-spec write(ChannelPid, Handle, Data, Timeout) -> ok | Error when
      ChannelPid :: pid(),
      Handle :: term(),
      Data :: iodata(),
      Timeout :: timeout(),
      Error :: {error, reason()}.
write(Pid, Handle, Data, FileOpTimeout) ->
    call(Pid, {write,false,Handle,Data}, FileOpTimeout).

%% TODO this ought to be a cast! Is so in all practical meaning
%% even if it is obscure!
-doc """
The [`apwrite/4`](`apwrite/4`) function writes to a specified position,
combining the `position/3` and `awrite/3` functions.
""".
-spec apwrite(ChannelPid, Handle, Position, Data) -> {async, N} | Error when
      ChannelPid :: pid(),
      Handle :: term(),
      Position :: integer(),
      Data :: binary(),
      Error :: {error, reason()},
      N :: term() .
apwrite(Pid, Handle, Offset, Data) ->
    call(Pid, {pwrite,true,Handle,Offset,Data}, infinity).

%% TODO this ought to be a cast!  Is so in all practical meaning
%% even if it is obscure!
-doc """
Writes to an open file, without waiting for the result. If the handle is valid,
the function returns `{async, N}`, where `N` is a term guaranteed to be unique
between calls of `awrite`. The result of the `write` operation is sent as a
message to the calling process. This message has the form
`{async_reply, N, Result}`, where `Result` is the result from the write, either
`ok`, or `{error, reason()}`.
""".
-spec awrite(ChannelPid, Handle, Data) -> {async, N} | Error when
      ChannelPid :: pid(),
      Handle :: term(),
      Data :: binary(),
      Error :: {error, reason()},
      N :: term() .
awrite(Pid, Handle, Data) ->
    call(Pid, {write,true,Handle,Data}, infinity).

-doc(#{equiv => position/4}).
-spec position(ChannelPid, Handle, Location) -> {ok, NewPosition} | Error when
      ChannelPid :: pid(),
      Handle :: term(),
      Location :: Offset | {bof, Offset} | {cur, Offset} | {eof, Offset} | bof | cur | eof,
      Offset :: integer(),
      NewPosition :: integer(),
      Error :: {error, reason()}.
position(Pid, Handle, Pos) ->
    position(Pid, Handle, Pos, ?FILEOP_TIMEOUT).

-doc """
Sets the file position of the file referenced by `Handle`. Returns
`{ok, NewPosition}` (as an absolute offset) if successful, otherwise
`{error, reason()}`. `Location` is one of the following:

- **`Offset`** - The same as `{bof, Offset}`.

- **`{bof, Offset}`** - Absolute offset.

- **`{cur, Offset}`** - Offset from the current position.

- **`{eof, Offset}`** - Offset from the end of file.

- **`bof | cur | eof`** - The same as eariler with `Offset` 0, that is, `{bof, 0} | {cur, 0} | {eof, 0}`.
""".
-spec position(ChannelPid, Handle, Location, Timeout) -> {ok, NewPosition} | Error when
      ChannelPid :: pid(),
      Handle :: term(),
      Location :: Offset | {bof, Offset} | {cur, Offset} | {eof, Offset} | bof | cur | eof,
      Timeout :: timeout(),
      Offset :: integer(),
      NewPosition :: integer(),
      Error :: {error, reason()}.
position(Pid, Handle, Pos, FileOpTimeout) ->
    call(Pid, {position, Handle, Pos}, FileOpTimeout).

-doc false.
real_path(Pid, Path) ->
    real_path(Pid, Path, ?FILEOP_TIMEOUT).
-doc false.
real_path(Pid, Path, FileOpTimeout) ->
    call(Pid, {real_path, false, Path}, FileOpTimeout).


-doc(#{equiv => read_file_info/3}).
-spec read_file_info(ChannelPid, Name) -> {ok, FileInfo} | Error when
      ChannelPid :: pid(),
      Name :: string(),
      FileInfo :: file:file_info(),
      Error :: {error, reason()}.
read_file_info(Pid, Name) ->
    read_file_info(Pid, Name, ?FILEOP_TIMEOUT).

-doc """
Returns a `file_info` record from the file system object specified by `Name` or
`Handle`. See `file:read_file_info/2` for information about the record.

Depending on the underlying OS:es links might be followed and info on the final
file, directory etc is returned. See `read_link_info/2` on how to get
information on links instead.
""".
-spec read_file_info(ChannelPid, Name, Timeout) -> {ok, FileInfo} | Error when
      ChannelPid :: pid(),
      Name :: string(),
      Timeout :: timeout(),
      FileInfo :: file:file_info(),
      Error :: {error, reason()}.
read_file_info(Pid, Name, FileOpTimeout) ->
    call(Pid, {read_file_info,false,Name}, FileOpTimeout).

-doc false.
get_file_info(Pid, Handle) ->
    get_file_info(Pid, Handle, ?FILEOP_TIMEOUT).
-doc false.
get_file_info(Pid, Handle, FileOpTimeout) ->
    call(Pid, {get_file_info,false,Handle}, FileOpTimeout).


-doc(#{equiv => write_file_info/4}).
-spec write_file_info(ChannelPid, Name, FileInfo) -> ok | Error when
      ChannelPid :: pid(),
      Name :: string(),
      FileInfo :: file:file_info(),
      Error :: {error, reason()}.
write_file_info(Pid, Name, Info) ->
    write_file_info(Pid, Name, Info, ?FILEOP_TIMEOUT).

-doc """
Writes file information from a `file_info` record to the file specified by
`Name`. See [file:write_file_info/2,3](`file:write_file_info/2`) for
information about the record.
""".
-spec write_file_info(ChannelPid, Name, FileInfo, Timeout) -> ok | Error when
      ChannelPid :: pid(),
      Name :: string(),
      FileInfo :: file:file_info(),
      Timeout :: timeout(),
      Error :: {error, reason()}.
write_file_info(Pid, Name, Info, FileOpTimeout) ->
    call(Pid, {write_file_info,false,Name, Info}, FileOpTimeout).


-doc(#{equiv => read_link_info/3}).
-spec read_link_info(ChannelPid, Name) -> {ok, FileInfo} | Error when
      ChannelPid :: pid(),
      Name :: string(),
      FileInfo :: file:file_info(),
      Error :: {error, reason()}.
read_link_info(Pid, Name) ->
    read_link_info(Pid, Name, ?FILEOP_TIMEOUT).

-doc """
Returns a `file_info` record from the symbolic link specified by `Name` or
`Handle`. See `file:read_link_info/2` for information about the record.
""".
-spec read_link_info(ChannelPid, Name, Timeout) -> {ok, FileInfo} | Error when
      ChannelPid :: pid(),
      Name :: string(),
      FileInfo :: file:file_info(),
      Timeout :: timeout(),
      Error :: {error, reason()}.
read_link_info(Pid, Name, FileOpTimeout) ->
    call(Pid, {read_link_info,false,Name}, FileOpTimeout).


-doc(#{equiv => read_link/3}).
-spec read_link(ChannelPid, Name) -> {ok, Target} | Error when
      ChannelPid :: pid(),
      Name :: string(),
      Target :: string(),
      Error :: {error, reason()}.
read_link(Pid, LinkName) ->
    read_link(Pid, LinkName, ?FILEOP_TIMEOUT).

-doc "Reads the link target from the symbolic link specified by `name`.".
-spec read_link(ChannelPid, Name, Timeout) -> {ok, Target} | Error when
      ChannelPid :: pid(),
      Name :: string(),
      Target :: string(),
      Timeout :: timeout(),
      Error :: {error, reason()}.
read_link(Pid, LinkName, FileOpTimeout) ->
    case call(Pid, {read_link,false,LinkName}, FileOpTimeout) of
	 {ok, [{Name, _Attrs}]} ->
	    {ok, Name};
	ErrMsg ->
	    ErrMsg
    end.

-doc(#{equiv => make_symlink/4}).
-spec make_symlink(ChannelPid, Name, Target) -> ok | Error when
      ChannelPid :: pid(),
      Name :: string(),
      Target :: string(),
      Error :: {error, reason()} .
make_symlink(Pid, Name, Target) ->
    make_symlink(Pid, Name, Target, ?FILEOP_TIMEOUT).
-doc "Creates a symbolic link pointing to `Target` with the name `Name`.".
-spec make_symlink(ChannelPid, Name, Target, Timeout) -> ok | Error when
      ChannelPid :: pid(),
      Name :: string(),
      Target :: string(),
      Timeout :: timeout(),
      Error :: {error, reason()} .
make_symlink(Pid, Name, Target, FileOpTimeout) ->
    call(Pid, {make_symlink,false, Name, Target}, FileOpTimeout).


-doc(#{equiv => rename/4}).
-spec rename(ChannelPid, OldName, NewName) -> ok | Error when
      ChannelPid :: pid(),
      OldName :: string(),
      NewName :: string(),
      Error :: {error, reason()}.
rename(Pid, FromFile, ToFile) ->
    rename(Pid, FromFile, ToFile, ?FILEOP_TIMEOUT).

-doc "Renames a file named `OldName` and gives it the name `NewName`.".
-spec rename(ChannelPid, OldName, NewName, Timeout) -> ok | Error when
      ChannelPid :: pid(),
      OldName :: string(),
      NewName :: string(),
      Timeout :: timeout(),
      Error :: {error, reason()}.
rename(Pid, FromFile, ToFile, FileOpTimeout) ->
    call(Pid, {rename,false,FromFile, ToFile}, FileOpTimeout).

-doc(#{equiv => delete/3}).
-spec delete(ChannelPid, Name) -> ok | Error when
      ChannelPid :: pid(),
      Name :: string(),
      Error :: {error, reason()} .
delete(Pid, Name) ->
    delete(Pid, Name, ?FILEOP_TIMEOUT).
-doc "Deletes the file specified by `Name`.".
-spec delete(ChannelPid, Name, Timeout) -> ok | Error when
      ChannelPid :: pid(),
      Name :: string(),
      Timeout :: timeout(),
      Error :: {error, reason()} .
delete(Pid, Name, FileOpTimeout) ->
    call(Pid, {delete,false,Name}, FileOpTimeout).

-doc(#{equiv => make_dir/3}).
-spec make_dir(ChannelPid, Name) -> ok | Error when
      ChannelPid :: pid(),
      Name :: string(),
      Error :: {error, reason()} .
make_dir(Pid, Name) ->
    make_dir(Pid, Name, ?FILEOP_TIMEOUT).
-doc """
Creates a directory specified by `Name`. `Name` must be a full path to a new
directory. The directory can only be created in an existing directory.
""".
-spec make_dir(ChannelPid, Name, Timeout) -> ok | Error when
      ChannelPid :: pid(),
      Name :: string(),
      Timeout :: timeout(),
      Error :: {error, reason()} .
make_dir(Pid, Name, FileOpTimeout) ->
    call(Pid, {make_dir,false,Name}, FileOpTimeout).

-doc(#{equiv => del_dir/3}).
-spec del_dir(ChannelPid, Name) -> ok | Error when
      ChannelPid :: pid(),
      Name :: string(),
      Error :: {error, reason()} .
del_dir(Pid, Name) ->
    del_dir(Pid, Name, ?FILEOP_TIMEOUT).
-doc """
Deletes a directory specified by `Name`. The directory must be empty before it
can be successfully deleted.
""".
-spec del_dir(ChannelPid, Name, Timeout) -> ok | Error when
      ChannelPid :: pid(),
      Name :: string(),
      Timeout :: timeout(),
      Error :: {error, reason()} .
del_dir(Pid, Name, FileOpTimeout) ->
    call(Pid, {del_dir,false,Name}, FileOpTimeout).

%% TODO : send_window and recv_window  - Really needed? Not documented!
%% internal use maybe should be handled in other way!
-doc false.
send_window(Pid) ->
    send_window(Pid, ?FILEOP_TIMEOUT).
-doc false.
send_window(Pid, FileOpTimeout) ->
    call(Pid, send_window, FileOpTimeout).

-doc false.
recv_window(Pid) ->
    recv_window(Pid, ?FILEOP_TIMEOUT).
-doc false.
recv_window(Pid, FileOpTimeout) ->
    call(Pid, recv_window, FileOpTimeout).


-doc(#{equiv => list_dir/3}).
-spec list_dir(ChannelPid, Path) -> {ok,FileNames} | Error when
      ChannelPid :: pid(),
      Path :: string(),
      FileNames :: [FileName],
      FileName :: string(),
      Error :: {error, reason()} .
list_dir(Pid, Name) ->
    list_dir(Pid, Name, ?FILEOP_TIMEOUT).
-doc """
Lists the given directory on the server, returning the filenames as a list of
strings.
""".
-spec list_dir(ChannelPid, Path, Timeout) -> {ok,FileNames} | Error when
      ChannelPid :: pid(),
      Path :: string(),
      Timeout :: timeout(),
      FileNames :: [FileName],
      FileName :: string(),
      Error :: {error, reason()} .
list_dir(Pid, Name, FileOpTimeout) ->
    case opendir(Pid, Name, FileOpTimeout) of
	{ok,Handle} ->
	    Res = do_list_dir(Pid, Handle, FileOpTimeout, []),
	    close(Pid, Handle, FileOpTimeout),
	    case Res of
		{ok, List} ->
		    NList = lists:foldl(fun({Nm, _Info},Acc) ->
					  [Nm|Acc] end,
				  [], List),
		    {ok,NList};
		Error -> Error
	    end;
	Error ->
	    Error
    end.

do_list_dir(Pid, Handle, FileOpTimeout, Acc) ->
    case readdir(Pid, Handle, FileOpTimeout) of
	{ok, []} ->
	    {ok, Acc};
	{ok, Names} ->
	    do_list_dir(Pid, Handle, FileOpTimeout, Acc ++ Names);
	eof ->
	    {ok, Acc};
	Error ->
	    Error
    end.


-doc(#{equiv => read_file/3}).
-spec read_file(ChannelPid, File) -> {ok, Data} | Error when
      ChannelPid :: pid(),
      File :: string(),
      Data :: binary(),
      Error :: {error, reason()}.
read_file(Pid, Name) ->
    read_file(Pid, Name, ?FILEOP_TIMEOUT).

-doc "Reads a file from the server, and returns the data in a binary.".
-spec read_file(ChannelPid, File, Timeout) -> {ok, Data} | Error when
      ChannelPid :: pid(),
      File :: string(),
      Data :: binary(),
      Timeout :: timeout(),
      Error :: {error, reason()}.
read_file(Pid, Name, FileOpTimeout) ->
    case open(Pid, Name, [read, binary], FileOpTimeout) of
	{ok, Handle} ->
	    {ok,{_WindowSz,PacketSz}} = recv_window(Pid, FileOpTimeout),
	    Res = read_file_loop(Pid, Handle, PacketSz, FileOpTimeout, []),
	    close(Pid, Handle),
	    Res;
	Error ->
	    Error
    end.

read_file_loop(Pid, Handle, PacketSz, FileOpTimeout, Acc) ->
    case read(Pid, Handle, PacketSz, FileOpTimeout) of
	{ok, Data}  ->
	    read_file_loop(Pid, Handle, PacketSz, FileOpTimeout, [Data|Acc]);
	eof ->
	    {ok, list_to_binary(lists:reverse(Acc))};
	Error ->
	    Error
    end.

-doc(#{equiv => write_file/4}).
-spec write_file(ChannelPid, File, Data) -> ok | Error when
      ChannelPid :: pid(),
      File :: string(),
      Data :: iodata(),
      Error :: {error, reason()}.
write_file(Pid, Name, List) ->
    write_file(Pid, Name, List, ?FILEOP_TIMEOUT).

-doc """
Writes a file to the server. The file is created if it does not exist but
overwritten if it exists.
""".
-spec write_file(ChannelPid, File, Data, Timeout) -> ok | Error when
      ChannelPid :: pid(),
      File :: string(),
      Data :: iodata(),
      Timeout :: timeout(),
      Error :: {error, reason()}.
write_file(Pid, Name, List, FileOpTimeout) when is_list(List) ->
    write_file(Pid, Name, to_bin(List), FileOpTimeout);
write_file(Pid, Name, Bin, FileOpTimeout) ->
    case open(Pid, Name, [write, binary], FileOpTimeout) of
	{ok, Handle} ->
	    {ok,{_Window,Packet}} = send_window(Pid, FileOpTimeout),
	    Res = write_file_loop(Pid, Handle, 0, Bin, byte_size(Bin), Packet,
				  FileOpTimeout),
	    close(Pid, Handle, FileOpTimeout),
	    Res;
	Error ->
	    Error
    end.

write_file_loop(_Pid, _Handle, _Pos, _Bin, 0, _PacketSz,_FileOpTimeout) ->
    ok;
write_file_loop(Pid, Handle, Pos, Bin, Remain, PacketSz, FileOpTimeout) ->
    if Remain >= PacketSz ->
	    <<_:Pos/binary, Data:PacketSz/binary, _/binary>> = Bin,
	    case write(Pid, Handle, Data, FileOpTimeout) of
		ok ->
		    write_file_loop(Pid, Handle,
				    Pos+PacketSz, Bin, Remain-PacketSz,
				    PacketSz, FileOpTimeout);
		Error ->
		    Error
	    end;
       true ->
	    <<_:Pos/binary, Data/binary>> = Bin,
	    write(Pid, Handle, Data, FileOpTimeout)
    end.

%%====================================================================
%% SSh channel callbacks 
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} 
%%                        
%% Description: 
%%--------------------------------------------------------------------
-doc false.
init([Cm, ChannelId, Options]) ->
    Timeout = proplists:get_value(timeout, Options, infinity),
    erlang:monitor(process, Cm),
    case ssh_connection:subsystem(Cm, ChannelId, "sftp", Timeout) of
	success ->
	    Xf = #ssh_xfer{cm = Cm,
			       channel = ChannelId},
	    {ok, #state{xf = Xf,
			req_id = 0,
			rep_buf = <<>>,
			inf = new_inf(),
			opts = Options}};
	failure ->
	    {stop, {shutdown, "server failed to start sftp subsystem"}};
	Error ->
	    {stop, {shutdown, Error}}
    end.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
-doc false.
handle_call({{timeout, infinity}, wait_for_version_negotiation}, From,
	    #state{xf = #ssh_xfer{vsn = undefined} = Xf} = State) ->
    {noreply, State#state{xf = Xf#ssh_xfer{vsn = {wait, From, undefined}}}};

handle_call({{timeout, Timeout}, wait_for_version_negotiation}, From,
	    #state{xf = #ssh_xfer{vsn = undefined} = Xf} = State) ->
    TRef = erlang:send_after(Timeout, self(), {timeout, undefined, From}),
    {noreply, State#state{xf = Xf#ssh_xfer{vsn = {wait, From, TRef}}}};

handle_call({_, wait_for_version_negotiation}, _, State) ->
    {reply, ok, State};

handle_call({{timeout, infinity}, Msg}, From, State) ->
    do_handle_call(Msg, From, State);
handle_call({{timeout, Timeout}, Msg}, From,  #state{req_id = Id} = State) ->
    timer:send_after(Timeout, {timeout, Id, From}),
    do_handle_call(Msg, From, State).

-doc false.
handle_cast(_,State) ->
    {noreply, State}.

-doc false.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_handle_call({get_bufinf,BufHandle}, _From, S=#state{inf=I0}) ->
    {reply, maps:find(BufHandle,I0), S};

do_handle_call({put_bufinf,BufHandle,B}, _From, S=#state{inf=I0}) ->
    {reply, ok, S#state{inf=maps:put(BufHandle,B,I0)}};

do_handle_call({erase_bufinf,BufHandle}, _From, S=#state{inf=I0}) ->
    {reply, ok, S#state{inf=maps:remove(BufHandle,I0)}};

do_handle_call({open, Async,FileName,Mode}, From, #state{xf = XF} = State) ->
    {Access,Flags,Attrs} = open_mode(XF#ssh_xfer.vsn, Mode),
    ReqID = State#state.req_id,
    ssh_xfer:open(XF, ReqID, FileName, Access, Flags, Attrs),
    case Async of
	true ->
	    {reply, {async,ReqID},
	     update_request_info(ReqID, State,
		      fun({ok,Handle},State1) ->
			      open2(ReqID,FileName,Handle,Mode,Async,
				    From,State1);
			 (Rep,State1) ->
			      async_reply(ReqID, Rep, From, State1)
		      end)};
	false ->
	    {noreply,
	     update_request_info(ReqID, State,
		      fun({ok,Handle},State1) ->
			      open2(ReqID,FileName,Handle,Mode,Async,
				    From,State1);
			 (Rep,State1) ->
			      sync_reply(Rep, From, State1)
		      end)}
    end;

do_handle_call({opendir,Async,Path}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:opendir(?XF(State), ReqID, Path),
    make_reply(ReqID, Async, From, State);

do_handle_call({readdir,Async,Handle}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:readdir(?XF(State), ReqID, Handle),
    make_reply(ReqID, Async, From, State);

do_handle_call({close,_Async,Handle}, From, State) ->
    %% wait until all operations on handle are done
    case get_size(Handle, State) of
	undefined ->
	    ReqID = State#state.req_id,
	    ssh_xfer:close(?XF(State), ReqID, Handle),
	    make_reply_post(ReqID, false, From, State,
			    fun(Rep, State1) ->
				    {Rep, erase_handle(Handle, State1)}
			    end);
	_ ->
	    case lseek_position(Handle, cur, State) of
		{ok,_} ->
		    ReqID = State#state.req_id,
		    ssh_xfer:close(?XF(State), ReqID, Handle),
		    make_reply_post(ReqID, false, From, State,
				    fun(Rep, State1) ->
					    {Rep, erase_handle(Handle, State1)}
				    end);
		Error ->
		    {reply, Error, State}
	    end
    end;

do_handle_call({pread,Async,Handle,At,Length}, From, State) ->
    case lseek_position(Handle, At, State) of
	{ok,Offset} ->
	    ReqID = State#state.req_id,
	    ssh_xfer:read(?XF(State),ReqID,Handle,Offset,Length),
	    %% To get multiple async read to work we must update the offset
	    %% before the operation begins
	    State1 = update_offset(Handle, Offset+Length, State),
	    make_reply_post(ReqID,Async,From,State1,
			    fun({ok,Data}, State2) ->
				    case get_mode(Handle, State2) of
					binary -> {{ok,Data}, State2};
					text -> {{ok,binary_to_list(Data)}, State2}
				    end;
			       (Rep, State2) ->
				    {Rep, State2}
			    end);
	Error ->
	    {reply, Error, State}
    end;

do_handle_call({read,Async,Handle,Length}, From, State) ->
    case lseek_position(Handle, cur, State) of
	{ok,Offset} ->
	    ReqID = State#state.req_id,
	    ssh_xfer:read(?XF(State),ReqID,Handle,Offset,Length),
	    %% To get multiple async read to work we must update the offset
	    %% before the operation begins
	    State1 = update_offset(Handle, Offset+Length, State),
	    make_reply_post(ReqID,Async,From,State1,
			    fun({ok,Data}, State2) ->
				    case get_mode(Handle, State2) of
					binary -> {{ok,Data}, State2};
					text -> {{ok,binary_to_list(Data)}, State2}
				    end;
			       (Rep, State2) -> {Rep, State2}
			    end);
	Error ->
	    {reply, Error, State}
    end;

do_handle_call({pwrite,Async,Handle,At,Data0}, From, State) ->
    case lseek_position(Handle, At, State) of
	{ok,Offset} ->
	    Data = to_bin(Data0),
	    ReqID = State#state.req_id,
	    Size = byte_size(Data),
	    ssh_xfer:write(?XF(State),ReqID,Handle,Offset,Data),
	    State1 = update_size(Handle, Offset+Size, State),
	    make_reply(ReqID, Async, From, State1);
	Error ->
	    {reply, Error, State}
    end;

do_handle_call({write,Async,Handle,Data0}, From, State) ->
    case lseek_position(Handle, cur, State) of
	{ok,Offset} ->
	    Data = to_bin(Data0),
	    ReqID = State#state.req_id,
	    Size = byte_size(Data),
	    ssh_xfer:write(?XF(State),ReqID,Handle,Offset,Data),
	    State1 = update_offset(Handle, Offset+Size, State),
	    make_reply(ReqID, Async, From, State1);
	Error ->
	    {reply, Error, State}
    end;

do_handle_call({position,Handle,At}, _From, State) ->
    %% We could make this auto sync when all request to Handle is done?
    case lseek_position(Handle, At, State) of
	{ok,Offset} ->
	    {reply, {ok, Offset}, update_offset(Handle, Offset, State)};
	Error ->
	    {reply, Error, State}
    end;

do_handle_call({rename,Async,FromFile,ToFile}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:rename(?XF(State),ReqID,FromFile,ToFile,[overwrite]),
    make_reply(ReqID, Async, From, State);

do_handle_call({delete,Async,Name}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:remove(?XF(State), ReqID, Name),
    make_reply(ReqID, Async, From, State);

do_handle_call({make_dir,Async,Name}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:mkdir(?XF(State), ReqID, Name,
		   #ssh_xfer_attr{ type = directory }),
    make_reply(ReqID, Async, From, State);

do_handle_call({del_dir,Async,Name}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:rmdir(?XF(State), ReqID, Name),
    make_reply(ReqID, Async, From, State);

do_handle_call({real_path,Async,Name}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:realpath(?XF(State), ReqID, Name),
    make_reply(ReqID, Async, From, State);

do_handle_call({read_file_info,Async,Name}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:stat(?XF(State), ReqID, Name, all),
    make_reply(ReqID, Async, From, State);

do_handle_call({get_file_info,Async,Name}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:fstat(?XF(State), ReqID, Name, all),
    make_reply(ReqID, Async, From, State);

do_handle_call({read_link_info,Async,Name}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:lstat(?XF(State), ReqID, Name, all),
    make_reply(ReqID, Async, From, State);

do_handle_call({read_link,Async,Name}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:readlink(?XF(State), ReqID, Name),
    make_reply(ReqID, Async, From, State);

do_handle_call({make_symlink, Async, Path, TargetPath}, From, State) ->
    ReqID = State#state.req_id,
    ssh_xfer:symlink(?XF(State), ReqID, Path, TargetPath),
    make_reply(ReqID, Async, From, State);

do_handle_call({write_file_info,Async,Name,Info}, From, State) ->
    ReqID = State#state.req_id,
    A = info_to_attr(Info),
    ssh_xfer:setstat(?XF(State), ReqID, Name, A),
    make_reply(ReqID, Async, From, State);

%% TODO: Do we really want this format? Function send_window
%% is not documented and seems to be used only inernaly! 
%% It is backwards compatible for now.
do_handle_call(send_window, _From, State) ->
    XF = State#state.xf,
     [{send_window,{{win_size, Size0},{packet_size, Size1}}}] =
	ssh:channel_info(XF#ssh_xfer.cm,  XF#ssh_xfer.channel, [send_window]),
    {reply, {ok, {Size0, Size1}}, State};

%% TODO: Do we really want this format? Function recv_window
%% is not documented and seems to be used only inernaly! 
%% It is backwards compatible for now.
do_handle_call(recv_window, _From, State) ->
    XF = State#state.xf,
    [{recv_window,{{win_size, Size0},{packet_size, Size1}}}] =
	ssh:channel_info(XF#ssh_xfer.cm,  XF#ssh_xfer.channel, [recv_window]),
    {reply, {ok, {Size0, Size1}}, State};

%% Backwards compatible
do_handle_call(stop, _From, State) ->
    {stop, shutdown, ok, State};

do_handle_call(Call, _From, State) ->
    {reply, {error, bad_call, Call, State}, State}.

%%--------------------------------------------------------------------
%% Function: handle_ssh_msg(Args) -> {ok, State} | {stop, ChannelId, State}
%%                        
%% Description: Handles channel messages
%%--------------------------------------------------------------------
-doc false.
handle_ssh_msg({ssh_cm, _ConnectionManager,
		{data, _ChannelId, 0, Data}}, #state{rep_buf = Data0} =
	       State0) ->
    State = handle_reply(State0, <<Data0/binary,Data/binary>>),
    {ok, State};

handle_ssh_msg({ssh_cm, _ConnectionManager,
		{data, _ChannelId, 1, Data}}, State) ->
    error_logger:format("ssh: STDERR: ~s\n", [binary_to_list(Data)]),
    {ok, State};

handle_ssh_msg({ssh_cm, _ConnectionManager, {eof, _ChannelId}}, State) ->
    {ok, State};

handle_ssh_msg({ssh_cm, _, {signal, _, _}}, State) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    {ok, State};

handle_ssh_msg({ssh_cm, _, {exit_signal, ChannelId, Signal, Error0, _}},
	       State0) ->
    Error =
        case Error0 of
            "" -> Signal;
            _ -> Error0
        end,
    State = reply_all(State0, {error, Error}),
    {stop, ChannelId,  State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, Status}}, State0) ->
    State = 
        case State0 of
            0 -> State0;
            _ -> reply_all(State0, {error, {exit_status, Status}})
        end,
    {stop, ChannelId, State}.

%%--------------------------------------------------------------------
%% Function: handle_msg(Args) -> {ok, State} | {stop, ChannelId, State}
%%                        
%% Description: Handles channel messages
%%--------------------------------------------------------------------
-doc false.
handle_msg({ssh_channel_up, _, _}, #state{opts = Options, xf = Xf} = State) ->
    Version = proplists:get_value(sftp_vsn, Options, ?SSH_SFTP_PROTOCOL_VERSION),
    ssh_xfer:protocol_version_request(Xf, Version),
    {ok, State};

%% Version negotiation timed out
handle_msg({timeout, undefined, From},
	   #state{xf = #ssh_xfer{channel = ChannelId}} = State) ->
    ssh_client_channel:reply(From, {error, timeout}),
    {stop, ChannelId, State};

handle_msg({timeout, Id, From}, #state{req_list = ReqList0} = State) ->
    case lists:keysearch(Id, 1, ReqList0) of
	false ->
	    {ok, State};
	_ ->
	    ReqList = lists:keydelete(Id, 1, ReqList0),
	    ssh_client_channel:reply(From, {error, timeout}),
	    {ok, State#state{req_list = ReqList}}
    end;

%% Connection manager goes down
handle_msg({'DOWN', _Ref, _Type, _Process, _},
	   #state{xf = #ssh_xfer{channel = ChannelId}} = State) ->
    {stop, ChannelId, State};

%% Stopped by user
handle_msg({'EXIT', _, ssh_sftp_stop_channel},
	   #state{xf = #ssh_xfer{channel = ChannelId}} = State) ->
    {stop, ChannelId, State};

handle_msg(_, State) ->
    {ok, State}.
%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: Called when the channel process is terminated
%%--------------------------------------------------------------------
%% Backwards compatible
-doc false.
terminate(shutdown, #state{xf = #ssh_xfer{cm = Cm}} = State) ->
    reply_all(State, {error, closed}),
    ssh:close(Cm);

terminate(_Reason, State) ->
    reply_all(State, {error, closed}).

%%====================================================================
%% Internal functions
%%====================================================================
legacy_timeout(UserOptions) ->
    %% Make both connect_timeout and timeout defined if exactly one of them is defined:
    case {proplists:get_value(connect_timeout, UserOptions),
          proplists:get_value(timeout, UserOptions)} of
        {undefined, undefined} ->
            UserOptions;
        {undefined, TO} ->
            [{connect_timeout,TO} | UserOptions];
        {TO, undefined} ->
            [{timeout,TO} | UserOptions];
        {_, _} ->
            UserOptions
    end.


handle_options(UserOptions) ->
    handle_options(UserOptions, [], [], []).

handle_options([], Sftp, Chan, Ssh) ->
    {Ssh, Chan, Sftp};
handle_options([{timeout, _} = Opt | Rest], Sftp, Chan, Ssh) ->
    handle_options(Rest, [Opt|Sftp], Chan, Ssh);
handle_options([{sftp_vsn, _} = Opt| Rest], Sftp, Chan, Ssh) ->
    handle_options(Rest, [Opt|Sftp], Chan, Ssh);
handle_options([{window_size, _} = Opt| Rest], Sftp, Chan, Ssh) ->
    handle_options(Rest, Sftp, [Opt|Chan], Ssh);
handle_options([{packet_size, _} = Opt| Rest], Sftp, Chan, Ssh) ->
    handle_options(Rest, Sftp, [Opt|Chan], Ssh);
handle_options([Opt|Rest], Sftp, Chan, Ssh) ->
    handle_options(Rest, Sftp, Chan, [Opt|Ssh]).

call(Pid, Msg, TimeOut) ->
    ssh_client_channel:call(Pid, {{timeout, TimeOut}, Msg}, infinity).

handle_reply(State, <<?UINT32(Len),Reply:Len/binary,Rest/binary>>) ->
    do_handle_reply(State, Reply, Rest);
handle_reply(State, Data) ->
     State#state{rep_buf = Data}.

do_handle_reply(#state{xf = Xf} = State,
		<<?SSH_FXP_VERSION, ?UINT32(Version), BinExt/binary>>, Rest) ->
    Ext = ssh_xfer:decode_ext(BinExt),
    case Xf#ssh_xfer.vsn of
	undefined ->
	    ok;
	{wait, From, TRef} ->
	    if is_reference(TRef) ->
		    erlang:cancel_timer(TRef);
	       true ->
		    ok
	    end,
	    ssh_client_channel:reply(From, ok)
    end,
    State#state{xf = Xf#ssh_xfer{vsn = Version, ext = Ext}, rep_buf = Rest};

do_handle_reply(State0, Data, Rest) ->
    case catch ssh_xfer:xf_reply(?XF(State0), Data) of
	{'EXIT', _Reason} ->
	    handle_reply(State0, Rest);
	XfReply ->
	    State = handle_req_reply(State0, XfReply),
	    handle_reply(State, Rest)
    end.

handle_req_reply(State0, {_, ReqID, _} = XfReply) ->
    case lists:keysearch(ReqID, 1, State0#state.req_list) of
	false ->
	    State0;
	{value,{_,Fun}} ->
	    List = lists:keydelete(ReqID, 1, State0#state.req_list),
	    State1 = State0#state { req_list = List },
	    case catch Fun(xreply(XfReply),State1) of
		{'EXIT', _} ->
		    State1;
		State ->
		    State
	    end
    end.

xreply({handle,_,H}) -> {ok, H};
xreply({data,_,Data}) -> {ok, Data};
xreply({name,_,Names}) -> {ok, Names};
xreply({attrs, _, A}) -> {ok, attr_to_info(A)};
xreply({extended_reply,_,X}) -> {ok, X};
xreply({status,_,{ok, _Err, _Lang, _Rep}}) -> ok;
xreply({status,_,{eof, _Err, _Lang, _Rep}}) -> eof;
xreply({status,_,{Stat, _Err, _Lang, _Rep}}) -> {error, Stat};
xreply({Code, _, Reply}) -> {Code, Reply}.

update_request_info(ReqID, State, Fun) ->
    List = [{ReqID,Fun} | State#state.req_list],
    ID = (State#state.req_id + 1) band 16#ffffffff,
    State#state { req_list = List, req_id = ID }.

async_reply(ReqID, Reply, _From={To,_}, State) ->
    To ! {async_reply, ReqID, Reply},
    State.

sync_reply(Reply, From, State) ->
    catch (ssh_client_channel:reply(From, Reply)),
    State.

open2(OrigReqID,FileName,Handle,Mode,Async,From,State) ->
    I0 = State#state.inf,
    FileMode = case lists:member(binary, Mode) orelse lists:member(raw, Mode) of
		   true -> binary;
		   false -> text
	       end,
    I1 = add_new_handle(Handle, FileMode, I0),
    State0 = State#state{inf = I1},
    ReqID = State0#state.req_id,
    ssh_xfer:stat(State0#state.xf, ReqID, FileName, [size]),
    case Async of
	true ->
	    update_request_info(ReqID, State0,
				fun({ok,FI},State1) ->
					Size = FI#file_info.size,
					State2 = if is_integer(Size) ->
							 put_size(Handle, Size, State1);
						    true ->
							 State1
						 end,
					async_reply(OrigReqID, {ok,Handle}, From, State2);
				   (_, State1) ->
					async_reply(OrigReqID, {ok,Handle}, From, State1)
				end);
	false ->
	    update_request_info(ReqID, State0,
				fun({ok,FI},State1) ->
					Size = FI#file_info.size,
					State2 = if is_integer(Size) ->
							 put_size(Handle, Size, State1);
						    true ->
							 State1
						 end,
					sync_reply({ok,Handle}, From, State2);
				   (_, State1) ->
					sync_reply({ok,Handle}, From, State1)
				end)
    end.

reply_all(State, Reply) ->
    List = State#state.req_list,
    lists:foreach(fun({_ReqID,Fun}) ->
		    catch Fun(Reply,State)
	    end, List),
    State#state {req_list = []}.

make_reply(ReqID, true, From, State) ->
    {reply, {async, ReqID},
     update_request_info(ReqID, State,
			 fun(Reply,State1) ->
				 async_reply(ReqID,Reply,From,State1)
			 end)};

make_reply(ReqID, false, From, State) ->
    {noreply, 
     update_request_info(ReqID, State,
			 fun(Reply,State1) ->
				 sync_reply(Reply, From, State1)
			 end)}.

make_reply_post(ReqID, true, From, State, PostFun) ->
    {reply, {async, ReqID},
     update_request_info(ReqID, State,
			 fun(Reply,State1) ->
				 case catch PostFun(Reply, State1) of
				     {'EXIT',_} ->
					 async_reply(ReqID,Reply, From, State1);
				     {Reply1, State2} ->
					 async_reply(ReqID,Reply1, From, State2)
				 end
			 end)};

make_reply_post(ReqID, false, From, State, PostFun) ->
    {noreply,
     update_request_info(ReqID, State,
			 fun(Reply,State1) ->
				 case catch PostFun(Reply, State1) of
				     {'EXIT',_} ->
					 sync_reply(Reply, From, State1);
				     {Reply1, State2} ->
					 sync_reply(Reply1, From, State2)
				 end
			 end)}.

%% convert: file_info -> ssh_xfer_attr
-doc false.
info_to_attr(I) when is_record(I, file_info) ->
    #ssh_xfer_attr { permissions = I#file_info.mode,
		     size = I#file_info.size,
		     type = I#file_info.type,
		     owner = I#file_info.uid,
		     group = I#file_info.gid,
		     atime = datetime_to_unix(I#file_info.atime),
		     mtime = datetime_to_unix(I#file_info.mtime),
		     createtime = datetime_to_unix(I#file_info.ctime)}.

%% convert: ssh_xfer_attr -> file_info
-doc false.
attr_to_info(A) when is_record(A, ssh_xfer_attr) ->
    #file_info{
      size   = A#ssh_xfer_attr.size,
      type   = A#ssh_xfer_attr.type,
      access = file_mode_to_owner_access(A#ssh_xfer_attr.permissions),
      atime  = unix_to_datetime(A#ssh_xfer_attr.atime),
      mtime  = unix_to_datetime(A#ssh_xfer_attr.mtime),
      ctime  = unix_to_datetime(A#ssh_xfer_attr.createtime),
      mode   = A#ssh_xfer_attr.permissions,
      links  = 1,
      major_device = 0,
      minor_device = 0,
      inode  = 0,
      uid    = A#ssh_xfer_attr.owner,
      gid    = A#ssh_xfer_attr.group}.

file_mode_to_owner_access(FileMode)
  when is_integer(FileMode) ->
    %% The file mode contains the access permissions.
    %% The read and write access permission of file owner
    %% are located in 8th and 7th bit of file mode respectively.

    ReadPermission = ((FileMode bsr 8) band 1),
    WritePermission =  ((FileMode bsr 7) band 1),
    case {ReadPermission, WritePermission} of
        {1, 1} ->
            read_write;
        {1, 0} ->
            read;
        {0, 1} ->
            write;
        {0, 0} ->
            none;
        _ ->
            undefined
    end;
file_mode_to_owner_access(_) ->
    undefined.

unix_to_datetime(undefined) ->
    undefined;
unix_to_datetime(UTCSecs) ->
    UTCDateTime =
	calendar:gregorian_seconds_to_datetime(UTCSecs + 62167219200),
    erlang:universaltime_to_localtime(UTCDateTime).

datetime_to_unix(undefined) ->
    undefined;
datetime_to_unix(LocalDateTime) ->
    UTCDateTime = erlang:localtime_to_universaltime(LocalDateTime),
    calendar:datetime_to_gregorian_seconds(UTCDateTime) - 62167219200.


open_mode(Vsn,Modes) when Vsn >= 5 ->
    open_mode5(Modes);
open_mode(_Vsn, Modes) ->
    open_mode3(Modes).

open_mode5(Modes) ->
    A = #ssh_xfer_attr{type = regular},
    {Fl, Ac} = case {lists:member(write, Modes),
		     lists:member(read, Modes),
		     lists:member(append, Modes)} of
		   {_, _, true} ->
		       {[append_data],
			[read_attributes,
			 append_data, write_attributes]};
		   {true, false, false} ->
		       {[create_truncate],
			[write_data, write_attributes]};
		   {true, true, _} ->
		       {[open_or_create],
			[read_data, read_attributes,
			 write_data, write_attributes]};
		   {false, true, _} ->
		       {[open_existing],
			[read_data, read_attributes]}
	       end,
    {Ac, Fl, A}.

open_mode3(Modes) ->
    A = #ssh_xfer_attr{type = regular},
    Fl = case {lists:member(write, Modes),
	       lists:member(read, Modes),
	       lists:member(append, Modes)} of
	     {_, _, true} ->
		 [append];
	     {true, false, false} ->
		 [write, creat, trunc];
	     {true, true, _} ->
		 [read, write];
	     {false, true, _} ->
		 [read]
	 end,
    {[], Fl, A}.

%% accessors for inf map
new_inf() -> #{}.

add_new_handle(Handle, FileMode, Inf) ->
    maps:put(Handle, #fileinf{offset=0, size=0, mode=FileMode}, Inf).

update_size(Handle, NewSize, State) ->
    OldSize = get_size(Handle, State),
    if NewSize > OldSize ->
	    put_size(Handle, NewSize, State);
       true ->
	    State
    end.

%% set_offset(Handle, NewOffset) ->
%%     put({offset,Handle}, NewOffset).

update_offset(Handle, NewOffset, State0) ->
    State1 = put_offset(Handle, NewOffset, State0),
    update_size(Handle, NewOffset, State1).

%% access size and offset for handle
put_size(Handle, Size, State) ->
    Inf0 = State#state.inf,
    case maps:find(Handle, Inf0) of
	{ok, FI} ->
	    State#state{inf=maps:put(Handle, FI#fileinf{size=Size}, Inf0)};
	_ ->
	    State#state{inf=maps:put(Handle, #fileinf{size=Size,offset=0}, Inf0)}
    end.

put_offset(Handle, Offset, State) ->
    Inf0 = State#state.inf,
    case maps:find(Handle, Inf0) of
	{ok, FI} ->
	    State#state{inf=maps:put(Handle, FI#fileinf{offset=Offset}, Inf0)};
	_ ->
	    State#state{inf=maps:put(Handle, #fileinf{size=Offset, offset=Offset}, Inf0)}
    end.

get_size(Handle, State) ->
    case maps:find(Handle, State#state.inf) of
	{ok, FI} ->
	    FI#fileinf.size;
	_ ->
	    undefined
    end.

%% get_offset(Handle, State) ->
%%     {ok, FI} = maps:find(Handle, State#state.inf),
%%     FI#fileinf.offset.

get_mode(Handle, State) ->
    case maps:find(Handle, State#state.inf) of
	{ok, FI} ->
	    FI#fileinf.mode;
	_ ->
	    undefined
    end.

erase_handle(Handle, State) ->
    FI = maps:remove(Handle, State#state.inf),
    State#state{inf = FI}.

%%
%% Calculate a integer offset
%%
lseek_position(Handle, Pos, State) ->
    case maps:find(Handle, State#state.inf) of
	{ok, #fileinf{offset=O, size=S}} ->
	    lseek_pos(Pos, O, S);
	_ ->
	    {error, einval}
    end.

lseek_pos(_Pos, undefined, _) ->
    {error, einval};
lseek_pos(Pos, _CurOffset, _CurSize)
  when is_integer(Pos) andalso 0 =< Pos andalso Pos < ?SSH_FILEXFER_LARGEFILESIZE ->
    {ok,Pos};
lseek_pos(bof, _CurOffset, _CurSize) ->
    {ok,0};
lseek_pos(cur, CurOffset, _CurSize) ->
    {ok,CurOffset};
lseek_pos(eof, _CurOffset, CurSize) ->
    {ok,CurSize};
lseek_pos({bof, Offset}, _CurOffset, _CurSize)
  when is_integer(Offset) andalso 0 =< Offset andalso Offset < ?SSH_FILEXFER_LARGEFILESIZE ->
    {ok, Offset};
lseek_pos({cur, Offset}, CurOffset, _CurSize)
  when is_integer(Offset) andalso -(?SSH_FILEXFER_LARGEFILESIZE) =< Offset andalso
       Offset < ?SSH_FILEXFER_LARGEFILESIZE ->
    NewOffset = CurOffset + Offset,
    if NewOffset < 0 ->
	    {ok, 0};
       true ->
	    {ok, NewOffset}
    end;
lseek_pos({eof, Offset}, _CurOffset, CurSize)
  when is_integer(Offset) andalso -(?SSH_FILEXFER_LARGEFILESIZE) =< Offset andalso
       Offset < ?SSH_FILEXFER_LARGEFILESIZE ->
    NewOffset = CurSize + Offset,
    if NewOffset < 0 ->
	    {ok, 0};
       true ->
	    {ok, NewOffset}
    end;
lseek_pos(_, _, _) ->
    {error, einval}.

%%%================================================================
%%%
to_bin(Data) when is_list(Data) -> ?to_binary(Data);
to_bin(Data) when is_binary(Data) -> Data.


read_repeat(Pid, Handle, Len, FileOpTimeout) ->
    {ok,{_WindowSz,PacketSz}} = recv_window(Pid, FileOpTimeout),
    read_rpt(Pid, Handle, Len, PacketSz, FileOpTimeout, <<>>).

read_rpt(Pid, Handle, WantedLen, PacketSz, FileOpTimeout, Acc) when WantedLen > 0 ->
    case read(Pid, Handle, min(WantedLen,PacketSz), FileOpTimeout) of
	{ok, Data}  ->
	    read_rpt(Pid, Handle, WantedLen-byte_size(Data), PacketSz, FileOpTimeout, <<Acc/binary, Data/binary>>);
	eof ->
	    {ok, Acc};
	Error ->
	    Error
    end;
read_rpt(_Pid, _Handle, WantedLen, _PacketSz, _FileOpTimeout, Acc) when WantedLen >= 0 ->
    {ok,Acc}.


write_to_remote_tar(_Pid, _SftpHandle, <<>>, _FileOpTimeout) ->
    ok;
write_to_remote_tar(Pid, SftpHandle, Bin, FileOpTimeout) ->
    {ok,{_Window,Packet}} = send_window(Pid, FileOpTimeout),
    write_file_loop(Pid, SftpHandle, 0, Bin, byte_size(Bin), Packet, FileOpTimeout).

position_buf(Pid, SftpHandle, BufHandle, Pos, FileOpTimeout) ->
    {ok,#bufinf{mode = Mode,
		plain_text_buf = Buf0,
		size = Size}} = call(Pid, {get_bufinf,BufHandle}, FileOpTimeout),
    case Pos of
	{cur,0} when Mode==write ->
	    {ok,Size+byte_size(Buf0)};

	{cur,0} when Mode==read ->
	    {ok,Size};

	_ when Mode==read, is_integer(Pos) ->
	    Skip = Pos-Size,
	    if
		Skip < 0 ->
		    {error, cannot_rewind};
		Skip == 0 ->
		    %% Optimization
		    {ok,Pos};
		Skip > 0 ->
		    case read_buf(Pid, SftpHandle, BufHandle, Skip, FileOpTimeout) of
			%% A bit innefficient to fetch the bufinf again, but there are lots of
			%% other more important optimizations waiting....
			{ok,_} ->
			    {ok,Pos};
			 Other ->
			    Other
		    end
	    end;

	_ ->
	    {error,{not_yet_implemented,{pos,Pos}}}
      end.

read_buf(Pid, SftpHandle, BufHandle, WantedLen, FileOpTimeout) ->
    {ok,{_Window,Packet}} = send_window(Pid, FileOpTimeout),
    {ok,B0}  = call(Pid, {get_bufinf,BufHandle}, FileOpTimeout),
    case do_the_read_buf(Pid, SftpHandle, WantedLen, Packet, FileOpTimeout, B0) of
	{ok,ResultBin,B} ->
	    call(Pid, {put_bufinf,BufHandle,B}, FileOpTimeout),
	    {ok,ResultBin};
	{error,Error} ->
	    {error,Error};
	{eof,B} ->
	    call(Pid, {put_bufinf,BufHandle,B}, FileOpTimeout),
	    eof
      end.

do_the_read_buf(_Pid, _SftpHandle, WantedLen, _Packet, _FileOpTimeout,
		B=#bufinf{plain_text_buf=PlainBuf0,
			  size = Size})
    when byte_size(PlainBuf0) >= WantedLen ->
    %% We already have the wanted number of bytes decoded and ready!
    <<ResultBin:WantedLen/binary, PlainBuf/binary>> = PlainBuf0,
    {ok,ResultBin,B#bufinf{plain_text_buf=PlainBuf,
			   size = Size + WantedLen}};

do_the_read_buf(Pid, SftpHandle, WantedLen, Packet, FileOpTimeout,
		B0=#bufinf{plain_text_buf = PlainBuf0,
			   enc_text_buf = EncBuf0,
			   chunksize = undefined
			  })
  when byte_size(EncBuf0) > 0 ->
    %% We have (at least) one decodable byte waiting for decoding.
    {ok,DecodedBin,B} = apply_crypto(EncBuf0, B0),
    do_the_read_buf(Pid, SftpHandle, WantedLen, Packet, FileOpTimeout,
		    B#bufinf{plain_text_buf = <<PlainBuf0/binary, DecodedBin/binary>>,
			     enc_text_buf = <<>>
			    });

do_the_read_buf(Pid, SftpHandle, WantedLen, Packet, FileOpTimeout,
		B0=#bufinf{plain_text_buf = PlainBuf0,
			   enc_text_buf = EncBuf0,
			   chunksize = ChunkSize0
			  })
  when byte_size(EncBuf0) >= ChunkSize0 ->
    %% We have (at least) one chunk of decodable bytes waiting for decoding.
    <<ToDecode:ChunkSize0/binary, EncBuf/binary>> = EncBuf0,
    {ok,DecodedBin,B} = apply_crypto(ToDecode, B0),
    do_the_read_buf(Pid, SftpHandle, WantedLen, Packet, FileOpTimeout,
		    B#bufinf{plain_text_buf = <<PlainBuf0/binary, DecodedBin/binary>>,
			     enc_text_buf = EncBuf
			    });

do_the_read_buf(Pid, SftpHandle, WantedLen, Packet, FileOpTimeout, B=#bufinf{enc_text_buf = EncBuf0}) ->
    %% We must read more bytes and append to the buffer of encoded bytes.
    case read(Pid, SftpHandle, Packet, FileOpTimeout) of
	{ok,EncryptedBin} ->
	    do_the_read_buf(Pid, SftpHandle, WantedLen, Packet, FileOpTimeout,
			    B#bufinf{enc_text_buf = <<EncBuf0/binary, EncryptedBin/binary>>});
	eof ->
	    {eof,B};
	Other ->
	    Other
    end.


write_buf(Pid, SftpHandle, BufHandle, PlainBin, FileOpTimeout) ->
    {ok,{_Window,Packet}} = send_window(Pid, FileOpTimeout),
    {ok,B0=#bufinf{plain_text_buf=PTB}}  = call(Pid, {get_bufinf,BufHandle}, FileOpTimeout),
    case do_the_write_buf(Pid, SftpHandle, Packet, FileOpTimeout,
			  B0#bufinf{plain_text_buf = <<PTB/binary,PlainBin/binary>>}) of
	{ok, B} ->
	    call(Pid, {put_bufinf,BufHandle,B}, FileOpTimeout),
	    ok;
	{error,Error} ->
	    {error,Error}
    end.

do_the_write_buf(Pid, SftpHandle, Packet, FileOpTimeout,
		 B=#bufinf{enc_text_buf = EncBuf0,
			   size = Size})
  when byte_size(EncBuf0) >= Packet ->
    <<BinToWrite:Packet/binary, EncBuf/binary>> = EncBuf0,
    case write(Pid, SftpHandle, BinToWrite, FileOpTimeout) of
	ok ->
	    do_the_write_buf(Pid, SftpHandle, Packet, FileOpTimeout,
			     B#bufinf{enc_text_buf = EncBuf,
				      size = Size + Packet});
	Other ->
	    Other
    end;

do_the_write_buf(Pid, SftpHandle, Packet, FileOpTimeout,
		 B0=#bufinf{plain_text_buf = PlainBuf0,
			    enc_text_buf = EncBuf0,
			    chunksize = undefined})
  when byte_size(PlainBuf0) > 0 ->
     {ok,EncodedBin,B} = apply_crypto(PlainBuf0, B0),
     do_the_write_buf(Pid, SftpHandle, Packet, FileOpTimeout,
		     B#bufinf{plain_text_buf = <<>>,
			      enc_text_buf = <<EncBuf0/binary, EncodedBin/binary>>});

do_the_write_buf(Pid, SftpHandle, Packet, FileOpTimeout,
		 B0=#bufinf{plain_text_buf = PlainBuf0,
			    enc_text_buf = EncBuf0,
			    chunksize = ChunkSize0
			   })
  when byte_size(PlainBuf0) >= ChunkSize0 ->
    <<ToEncode:ChunkSize0/binary, PlainBuf/binary>> = PlainBuf0,
    {ok,EncodedBin,B} = apply_crypto(ToEncode, B0),
    do_the_write_buf(Pid, SftpHandle, Packet, FileOpTimeout,
		     B#bufinf{plain_text_buf = PlainBuf,
			      enc_text_buf = <<EncBuf0/binary, EncodedBin/binary>>});

do_the_write_buf(_Pid, _SftpHandle, _Packet, _FileOpTimeout, B) ->
    {ok,B}.

apply_crypto(In, B=#bufinf{crypto_state = CState0,
			   crypto_fun = F}) ->
    case F(In,CState0) of
	{ok,EncodedBin,CState} ->
	    {ok, EncodedBin, B#bufinf{crypto_state=CState}};
	{ok,EncodedBin,CState,ChunkSize} ->
	    {ok, EncodedBin, B#bufinf{crypto_state=CState,
				      chunksize=ChunkSize}}
    end.

open_buf(Pid, CryptoInitFun, BufInfo0, FileOpTimeout) ->
    case CryptoInitFun() of
	{ok,CryptoState} ->
	    open_buf1(Pid, BufInfo0, FileOpTimeout, CryptoState, undefined);
	{ok,CryptoState,ChunkSize} ->
	    open_buf1(Pid, BufInfo0, FileOpTimeout, CryptoState, ChunkSize);
	Other ->
	    Other
    end.

open_buf1(Pid, BufInfo0, FileOpTimeout, CryptoState, ChunkSize) ->
    BufInfo = BufInfo0#bufinf{crypto_state = CryptoState,
			      chunksize = ChunkSize},
    BufHandle = make_ref(),
    call(Pid, {put_bufinf,BufHandle,BufInfo}, FileOpTimeout),
    {ok,BufHandle}.

format_channel_start_error({shutdown, Reason}) ->
    Reason;
format_channel_start_error(Reason) ->
    Reason.

%%%################################################################
%%%#
%%%# Tracing
%%%#

-doc false.
ssh_dbg_trace_points() -> [terminate].

-doc false.
ssh_dbg_flags(terminate) -> [c].

-doc false.
ssh_dbg_on(terminate) -> dbg:tp(?MODULE,  terminate, 2, x).

-doc false.
ssh_dbg_off(terminate) -> dbg:ctpg(?MODULE, terminate, 2).

-doc false.
ssh_dbg_format(terminate, {call, {?MODULE,terminate, [Reason, State]}}) ->
    ["Sftp Terminating:\n",
     io_lib:format("Reason: ~p,~nState:~n~s", [Reason, wr_record(State)])
    ];
ssh_dbg_format(terminate, {return_from, {?MODULE,terminate,2}, _Ret}) ->
    skip.

?wr_record(state).

