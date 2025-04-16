%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose: Details of connection protocol
%%----------------------------------------------------------------------

-module(ssh_connection).
-moduledoc """
This module provides API functions to send SSH Connection Protocol events to the
other side of an SSH channel.

The [SSH Connection Protocol (RFC 4254)](http://www.ietf.org/rfc/rfc4254.txt) is used by
clients and servers, that is, SSH channels, to communicate over the SSH
connection. The API functions in this module send SSH Connection Protocol
events, which are received as messages by the remote channel handling the remote
channel. The Erlang format of thoose messages is (see also
[below](`t:event/0`)):

`{ssh_cm,` `t:ssh:connection_ref/0` `,` `t:channel_msg/0` `}`

If the `m:ssh_client_channel` behavior is used to implement the channel process,
these messages are handled by
[handle_ssh_msg/2](`c:ssh_client_channel:handle_ssh_msg/2`).
""".

-include_lib("kernel/include/logger.hrl").

-include("ssh.hrl").
-include("ssh_connect.hrl").
-include("ssh_transport.hrl").

%% API
-export([session_channel/2, session_channel/4,
	 exec/4, shell/2, subsystem/4, send/3, send/4, send/5, 
	 send_eof/2, adjust_window/3, setenv/5, close/2, reply_request/4,
	 ptty_alloc/3, ptty_alloc/4]).

%% Potential API currently unsupported and not tested
-export([window_change/4, window_change/6,
	 signal/3, exit_status/3]).

%% Internal SSH application API
-export([channel_data/5,
         handle_msg/4,
         handle_stop/1,

         open_channel/4,

	 channel_adjust_window_msg/2,
	 channel_close_msg/1,
	 channel_open_failure_msg/4,
	 channel_open_msg/5,
	 channel_status_msg/1,
         channel_data_msg/3,
         channel_eof_msg/1,
         channel_failure_msg/1, 
         channel_open_confirmation_msg/4,
         channel_request_msg/4,
         channel_success_msg/1,

         request_global_msg/3,
	 request_failure_msg/0, 
	 request_success_msg/1,

         send_environment_vars/3,

	 encode_ip/1
        ]).

%% For testing only
-export([encode_pty_opts/1, decode_pty_opts/1]).

-type connection_ref() :: ssh:connection_ref().
-type channel_id()     :: ssh:channel_id().

-doc """
The status of a request. Corresponds to the `SSH_MSG_CHANNEL_SUCCESS` and
`SSH_MSG_CHANNEL_FAILURE` values in
[RFC 4254, Section 5.4](https://tools.ietf.org/html/rfc4254#section-5.4).
""".
-type req_status() :: success | failure .
-doc """
The result of a call.

If the request reached the peer, was handled and the response reached the
requesting node the `t:req_status/0` is the status reported from the peer.

If not, the `t:reason/0` indicates what went wrong:

- **`closed`** - indicates that the channel or connection was closed when trying
  to send the request

- **`timeout`** - indicates that the operation exceeded a time limit
""".
-type reason() :: closed | timeout .

-type result() :: req_status() | {error, reason()} .

-doc """
The valid values are `0` ("normal") and `1` ("stderr"), see
[RFC 4254, Section 5.2](https://tools.ietf.org/html/rfc4254#page-8).
""".
-type ssh_data_type_code() :: non_neg_integer(). % Only 0 and 1 are used


%%% The SSH Connection Protocol

-export_type([event/0,
              channel_msg/0,
              want_reply/0,
              data_ch_msg/0,
              eof_ch_msg/0,
              signal_ch_msg/0,
              exit_signal_ch_msg/0,
              exit_status_ch_msg/0,
              closed_ch_msg/0,
              env_ch_msg/0,
              pty_ch_msg/0,
              shell_ch_msg/0,
              window_change_ch_msg/0,
              exec_ch_msg/0
             ]).

-doc(#{group => <<"General">>}).
-type event() :: {ssh_cm, ssh:connection_ref(), channel_msg()}.
-doc """
As mentioned in the introduction, the
[SSH Connection Protocol](https://tools.ietf.org/html/rfc4254) events are
handled as messages. When writing a channel handling process without using the
support by the `m:ssh_client_channel` behavior the process must handle thoose
messages.
""".
-doc(#{group => <<"General">>}).
-type channel_msg() ::  data_ch_msg()
                      | eof_ch_msg()
                      | closed_ch_msg()
                      | pty_ch_msg()
                      | env_ch_msg()
                      | shell_ch_msg()
                      | exec_ch_msg()
                      | signal_ch_msg()
                      | window_change_ch_msg()
                      | exit_status_ch_msg()
                      | exit_signal_ch_msg()
                        .

-doc """
Messages that include a `WantReply` expect the channel handling process to call
[ssh_connection:reply_request/4](`reply_request/4`) with the boolean value of
`WantReply` as the second argument.
""".
-doc(#{group => <<"General">>}).
-type want_reply() :: boolean().

-doc """
Data has arrived on the channel. This event is sent as a result of calling
[ssh_connection:send/3,4,5](`send/3`).
""".
-doc(#{group => <<"Data Transfer">>,
       rfc => ~"RFC 4254, section 5.2"}).
-type data_ch_msg() :: {data,
                        ssh:channel_id(),
                        ssh_data_type_code(),
                        Data :: binary()
                       } .
-doc """
Indicates that the other side sends no more data. This event is sent as a result
of calling [ssh_connection:send_eof/2](`send_eof/2`).
""".
-doc(#{group => <<"Closing a Channel">>,
       rfc => ~"RFC 4254, section 5.3"}).
-type eof_ch_msg() :: {eof,
                       ssh:channel_id()
                      } .
-doc """
A signal can be delivered to the remote process/service using the following
message. Some systems do not support signals, in which case they are to ignore
this message. There is currently no function to generate this event as the
signals referred to are on OS-level and not something generated by an Erlang
program.
""".
-doc(#{group => <<"Signals">>,
       rfc => ~"RFC 4254, section 6.9"}).
-type signal_ch_msg() :: {signal,
                          ssh:channel_id(),
                          SignalName :: string()
                         } .
-doc """
A remote execution can terminate violently because of a signal. Then this
message can be received. For details on valid string values, see
[RFC 4254](https://tools.ietf.org/html/rfc4254#section-6.10) Section 6.10, which
shows a special case of these signals.
""".
-doc(#{group => <<"Exit Status">>,
       rfc => ~"RFC 4254, section 6.10"}).
-type exit_signal_ch_msg() :: {exit_signal, ssh:channel_id(),
                               ExitSignal :: string(),
                               ErrorMsg :: string(),
                               LanguageString :: string()} .
-doc """
When the command running at the other end terminates, the following message can
be sent to return the exit status of the command. A zero `exit_status` usually
means that the command terminated successfully. This event is sent as a result
of calling [ssh_connection:exit_status/3](`exit_status/3`).
""".
-doc(#{group => <<"Exit Status">>,
       rfc => ~"RFC 4254, section 6.10"}).
-type exit_status_ch_msg() :: {exit_status,
                               ssh:channel_id(),
                               ExitStatus :: non_neg_integer()
                              } .
-doc """
This event is sent as a result of calling [ssh_connection:close/2](`close/2`).
Both the handling of this event and sending it are taken care of by the
`m:ssh_client_channel` behavior.
""".
-doc(#{group => <<"Closing a Channel">>,
       rfc => ~"RFC 4254, section 5.3"}).
-type closed_ch_msg() :: {closed,
                          ssh:channel_id()
                         } .
-doc """
Environment variables can be passed to the shell/command to be started later.
This event is sent as a result of calling [ssh_connection:setenv/5](`setenv/5`).
""".
-doc(#{group => <<"Environment Variable">>,
       rfc => ~"RFC 4254, section 6.4"}).
-type env_ch_msg() :: {env,
                       ssh:channel_id(),
                       want_reply(),
                       Var :: string(),
                       Value :: string()
                      } .
-doc(#{group => <<"Pseudo-Terminal">>,
       rfc => ~"RFC 4254, section 6.2"}).
-type pty_ch_msg() :: {pty,
                       ssh:channel_id(),
                       want_reply(),
                       {Terminal :: string(),
                        CharWidth :: non_neg_integer(),
                        RowHeight :: non_neg_integer(),
                        PixelWidth :: non_neg_integer(),
                        PixelHeight :: non_neg_integer(),
                        TerminalModes :: [term_mode()]
                       }
                      } .

-doc """
A pseudo-terminal has been requested for the session. `Terminal` is the value of
the TERM environment variable value, that is, `vt100`. Zero dimension parameters
must be ignored. The character/row dimensions override the pixel dimensions
(when non-zero). Pixel dimensions refer to the drawable area of the window.
`Opcode` in the `TerminalModes` list is the mnemonic name, represented as a
lowercase Erlang atom, defined in
[RFC 4254](https://tools.ietf.org/html/rfc4254#section/8), Section 8. It can
also be an `Opcode` if the mnemonic name is not listed in the RFC. Example:
`OP code: 53, mnemonic name ECHO erlang atom: echo`. This event is sent as a
result of calling [ssh_connection:ptty_alloc/4](`ptty_alloc/4`).
""".
-doc(#{group => <<"Pseudo-Terminal">>,
       rfc => ~"RFC 4254, section 6.2"}).
-type term_mode() :: {Opcode :: atom() | byte(),
                      Value :: non_neg_integer()} .

-doc """
This message requests that the user default shell is started at the other end.
This event is sent as a result of calling [ssh_connection:shell/2](`shell/2`).
""".
-doc(#{group => <<"Shell or Command">>,
       rfc => ~"RFC 4254, section 6.5"}).
-type shell_ch_msg() :: {shell,
                         ssh:channel_id(),
                         want_reply()
                        } .
-doc """
When the window (terminal) size changes on the client side, it _can_ send a
message to the server side to inform it of the new dimensions. No API function
generates this event.
""".
-doc(#{group => <<"Window Change">>,
       rfc => ~"RFC 4254, section 6.7"}).
-type window_change_ch_msg() :: {window_change,
                                 ssh:channel_id(),
                                 CharWidth :: non_neg_integer(),
                                 RowHeight :: non_neg_integer(),
                                 PixelWidth :: non_neg_integer(),
                                 PixelHeight :: non_neg_integer()
                                } .
-doc """
This message requests that the server starts execution of the given command.
This event is sent as a result of calling [ssh_connection:exec/4 ](`exec/4`).
""".
-doc(#{group => <<"Shell or Command">>,
       rfc => ~"RFC 4254, section 6.5"}).
-type exec_ch_msg() :: {exec,
                        ssh:channel_id(),
                        want_reply(),
                        Command :: string()
                       } .

%%% This function is solely to convince all
%%% checks that the type event() exists...
-export([dummy/1]).
-doc false.
-spec dummy(event()) -> false.
dummy(_) -> false.

%%--------------------------------------------------------------------
%%% API
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Description: Opens a channel for a ssh session. A session is a
%% remote execution of a program. The program may be a shell, an
%% application, a system command, or some built-in subsystem.
%% --------------------------------------------------------------------

-doc(#{equiv => session_channel/4}).
-spec session_channel(ConnectionRef, Timeout) -> Result when
      ConnectionRef :: ssh:connection_ref(),
      Timeout :: timeout(),
      Result :: {ok, ssh:channel_id()} | {error, reason()} .

session_channel(ConnectionRef, Timeout) ->
    session_channel(ConnectionRef, undefined, undefined, Timeout).


-doc """
Opens a channel for an SSH session. The channel id returned from this function
is the id used as input to the other functions in this module.
""".
-spec session_channel(ConnectionRef, InitialWindowSize, MaxPacketSize, Timeout) -> Result when
      ConnectionRef :: ssh:connection_ref(),
      InitialWindowSize :: pos_integer() | undefined,
      MaxPacketSize :: pos_integer() | undefined,
      Timeout :: timeout(),
      Result :: {ok, ssh:channel_id()} | {error, reason()} .

session_channel(ConnectionRef, InitialWindowSize, MaxPacketSize, Timeout) ->
    open_channel(ConnectionRef, "session", <<>>,
                 InitialWindowSize,
                 MaxPacketSize,
                 Timeout).

%%--------------------------------------------------------------------
%% Description: Opens a channel for the given type.
%% --------------------------------------------------------------------
-doc false.
open_channel(ConnectionRef, Type, ChanData, Timeout) ->
    open_channel(ConnectionRef, Type, ChanData, undefined, undefined, Timeout).

open_channel(ConnectionRef, Type, ChanData, InitialWindowSize, MaxPacketSize, Timeout) ->
    case ssh_connection_handler:open_channel(ConnectionRef, Type, ChanData,
                                             InitialWindowSize, MaxPacketSize,
                                             Timeout) of
        {open, Channel} ->
	    {ok, Channel};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% Description: Will request that the server start the
%% execution of the given command. 
%%--------------------------------------------------------------------
-doc """
Is to be called by a client-channel process to request that the server starts
executing the given command. The result is several messages according to the
following pattern. The last message is a channel close message, as the `exec`
request is a one-time execution that closes the channel when it is done.

- **N x [data message(s)](`t:data_ch_msg/0`)** - The result of executing the
  command can be only one line or thousands of lines depending on the command.

- **0 or 1 x [eof message](`t:eof_ch_msg/0`)** - Indicates that no more data is
  to be sent.

- **0 or 1 x [exit signal message](`t:exit_signal_ch_msg/0`)** - Not all systems
  send signals. For details on valid string values, see RFC 4254, Section 6.10

- **0 or 1 x [exit status message](`t:exit_status_ch_msg/0`)** - It is
  recommended by the SSH Connection Protocol to send this message, but that is
  not always the case.

- **1 x [closed status message](`t:closed_ch_msg/0`)** - Indicates that the
  `ssh_client_channel` started for the execution of the command has now been
  shut down.

See the User's Guide section on
[One-Time Execution](using_ssh.md#one-time-execution) for examples.

> #### Note {: .info }
>
> In case when command generates large amount of output data, manual
> window adjustment might be necessary in order to receive it.
> see [`ssh_connectino:adjust_window/3`](`adjust_window/3`)
""".
-spec exec(ConnectionRef, ChannelId, Command, Timeout) -> result() when
      ConnectionRef :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id(),
      Command :: string(),
      Timeout :: timeout().

exec(ConnectionRef, ChannelId, Command, TimeOut) ->
    ssh_connection_handler:request(ConnectionRef, self(), ChannelId, "exec",
				   true, [?string(Command)], TimeOut).

%%--------------------------------------------------------------------
%% Description: Will request that the user's default shell (typically
%% defined in /etc/passwd in UNIX systems) be started at the other
%% end.
%%--------------------------------------------------------------------
-doc """
Is to be called by a client channel process to request that the user default
shell (typically defined in /etc/passwd in Unix systems) is executed at the
server end.

Note: the return value is `ok` instead of `success` unlike in other functions in
this module. This is a fault that was introduced so long ago that any change
would break a large number of existing software.
""".
-spec shell(ConnectionRef, ChannelId) -> Result when
      ConnectionRef :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id(),
      Result :: ok | success | failure | {error, timeout} .

shell(ConnectionRef, ChannelId) ->
    ssh_connection_handler:request(ConnectionRef, self(), ChannelId,
 				   "shell", false, <<>>, 0).
%%--------------------------------------------------------------------
%%
%% Description: Executes a predefined subsystem.
%%--------------------------------------------------------------------
-doc """
Is to be called by a client-channel process for requesting to execute a
predefined subsystem on the server.

The function [`subsystem/4`](`subsystem/4`) and subsequent calls of
[send/3,4,5](`send/3`) must be executed in the same process.
""".
-spec subsystem(ConnectionRef, ChannelId, Subsystem, Timeout) -> result() when
      ConnectionRef :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id(),
      Subsystem  :: string(),
      Timeout :: timeout().

subsystem(ConnectionRef, ChannelId, SubSystem, TimeOut) ->
     ssh_connection_handler:request(ConnectionRef, self(),
				    ChannelId, "subsystem", 
				    true, [?string(SubSystem)], TimeOut).
%%--------------------------------------------------------------------
%% Description: Sends channel data.
%%--------------------------------------------------------------------
-doc(#{equiv => send/5}).
-spec send(connection_ref(), channel_id(), iodata()) ->
		  ok | {error, reason()}.

send(ConnectionRef, ChannelId, Data) ->
    send(ConnectionRef, ChannelId, 0, Data, infinity).


-doc """
send(ConnectionRef, ChannelId, Type, Data)

Depending on input arguments equivalent to one of `send/5` calls specified below.

Equivalent to [send(ConnectionRef, ChannelId, 0, Data, TimeOut)](`send/5`) if
called with TimeOut being integer.

Equivalent to [send(ConnectionRef, ChannelId, 0, Data, infinity)](`send/5`) if
called with TimeOut being infinity atom.

Equivalent to [send(ConnectionRef, ChannelId, Type, Data, infinity)](`send/5`) if
called with last argument which is not integer or infinity atom.
""".

-spec send(connection_ref(), channel_id(), iodata(), timeout()) -> ok | {error, reason()};
          (connection_ref(), channel_id(), ssh_data_type_code(), iodata()) -> ok | {error, reason()}.

send(ConnectionRef, ChannelId, Data, TimeOut) when is_integer(TimeOut) ->
    send(ConnectionRef, ChannelId, 0, Data, TimeOut);

send(ConnectionRef, ChannelId, Data, infinity) ->
    send(ConnectionRef, ChannelId, 0, Data, infinity);

send(ConnectionRef, ChannelId, Type, Data) ->
    send(ConnectionRef, ChannelId, Type, Data, infinity).


-doc """
Is to be called by client- and server-channel processes to send data to each
other.

The function `subsystem/4` and subsequent calls of `send/3,4,5` must be executed
in the same process.
""".
-spec send(connection_ref(), channel_id(), ssh_data_type_code(), iodata(), timeout()) -> ok | {error, reason()}.

send(ConnectionRef, ChannelId, Type, Data, TimeOut) ->
    ssh_connection_handler:send(ConnectionRef, ChannelId,
				Type, Data, TimeOut).
%%--------------------------------------------------------------------
-doc "Sends EOF on channel `ChannelId`.".
-spec send_eof(ConnectionRef, ChannelId) -> ok  | {error, closed} when
      ConnectionRef :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id().
%%
%%
%% Description: Sends eof on the channel <ChannelId>.
%%--------------------------------------------------------------------
send_eof(ConnectionRef, Channel) ->
    ssh_connection_handler:send_eof(ConnectionRef, Channel).

%%--------------------------------------------------------------------
-doc """
Adjusts the SSH flow control window. This is to be done by both the client- and
server-side channel processes.

> #### Note {: .info }
>
> Channels implemented with the `m:ssh_client_channel` behavior do not normally
> need to call this function as flow control is handled by the behavior. The
> behavior adjusts the window every time the callback
> [handle_ssh_msg/2](`c:ssh_client_channel:handle_ssh_msg/2`) returns after
> processing channel data.
""".
-spec adjust_window(ConnectionRef, ChannelId, NumOfBytes) -> ok when
      ConnectionRef :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id(),
      NumOfBytes  :: integer().
%%
%%
%% Description: Adjusts the ssh flowcontrol window.
%%--------------------------------------------------------------------
adjust_window(ConnectionRef, Channel, Bytes) ->
    ssh_connection_handler:adjust_window(ConnectionRef, Channel, Bytes).

%%--------------------------------------------------------------------
-doc """
Environment variables can be passed before starting the shell/command. Is to be
called by a client channel processes.
""".
-spec setenv(ConnectionRef, ChannelId, Var, Value, Timeout) -> success when
      ConnectionRef :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id(),
      Var :: string(),
      Value :: string(),
      Timeout :: timeout().
%%
%%
%% Description: Environment variables may be passed to the shell/command to be
%% started later.
setenv(ConnectionRef, ChannelId, Var, Value, TimeOut) ->
    setenv(ConnectionRef, ChannelId, true, Var, Value, TimeOut).

setenv(ConnectionRef, ChannelId, WantReply, Var, Value, TimeOut) ->
    case ssh_connection_handler:request(ConnectionRef, ChannelId,
                                        "env", WantReply,
                                        [?string(Var), ?string(Value)], TimeOut) of
        ok when WantReply == false ->
            success;
        Reply ->
            Reply
    end.
%%--------------------------------------------------------------------
-doc """
A server- or client-channel process can choose to close their session by sending
a close event.

> #### Note {: .info }
>
> This function is called by the `ssh_client_channel` behavior when the channel
> is terminated, see `m:ssh_client_channel`. Thus, channels implemented with the
> behavior are not to call this function explicitly.
""".
-spec close(ConnectionRef, ChannelId) -> ok when
      ConnectionRef :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id().
%%
%%
%% Description: Sends a close message on the channel <ChannelId>.
%%--------------------------------------------------------------------
close(ConnectionRef, ChannelId) ->
    ssh_connection_handler:close(ConnectionRef, ChannelId).

%%--------------------------------------------------------------------
-doc """
Sends status replies to requests where the requester has stated that it wants a
status report, that is, `WantReply = true`. If `WantReply` is `false`, calling
this function becomes a "noop". Is to be called while handling an SSH Connection
Protocol message containing a `WantReply` boolean value.
""".
-spec reply_request(ConnectionRef, WantReply, Status, ChannelId) -> ok when
      ConnectionRef :: ssh:connection_ref(),
      WantReply :: boolean(),
      Status :: req_status(),
      ChannelId :: ssh:channel_id().
%%
%%
%% Description: Send status replies to requests that want such replies.
%%--------------------------------------------------------------------
reply_request(ConnectionRef, true, Status, ChannelId) ->
    ssh_connection_handler:reply_request(ConnectionRef, Status, ChannelId);
reply_request(_,false, _, _) ->
    ok.

%%--------------------------------------------------------------------
%% Description: Sends a ssh connection protocol pty_req.
%%--------------------------------------------------------------------
-doc(#{equiv => ptty_alloc/4}).
-doc(#{since => <<"OTP 17.5">>}).
-spec ptty_alloc(ConnectionRef, ChannelId, Options) -> result() when
      ConnectionRef :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id(),
      Options  :: proplists:proplist().

ptty_alloc(ConnectionRef, Channel, Options) ->
    ptty_alloc(ConnectionRef, Channel, Options, infinity).


-doc """
Sends an SSH Connection Protocol `pty_req`, to allocate a pseudo-terminal. Is to
be called by an SSH client process.

Options:

- **\{term, string()\}** - Defaults to _os:getenv("TERM")_ or _vt100_ if it is
  undefined.

- **\{width, integer()\}** - Defaults to 80 if `pixel_width` is not defined.

- **\{height, integer()\}** - Defaults to 24 if `pixel_height` is not defined.

- **\{pixel_width, integer()\}** - Is disregarded if `width` is defined.

- **\{pixel_height, integer()\}** - Is disregarded if `height` is defined.

- **\{pty_opts, \[\{posix_atom(), integer()\}]\}** - Option can be an empty
  list. Otherwise, see possible _POSIX_ names in Section 8 in
  [RFC 4254](http://www.ietf.org/rfc/rfc4254.txt).
""".
-doc(#{since => <<"OTP 17.4">>}).
-spec ptty_alloc(ConnectionRef, ChannelId, Options, Timeout) -> result() when
      ConnectionRef :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id(),
      Options  :: proplists:proplist(),
      Timeout :: timeout().

ptty_alloc(ConnectionRef, Channel, Options0, TimeOut) ->
    TermData = backwards_compatible(Options0, []), % FIXME
    {Width, PixWidth} = pty_default_dimensions(width, TermData),
    {Height, PixHeight} = pty_default_dimensions(height, TermData),
    pty_req(ConnectionRef, Channel,
	    proplists:get_value(term, TermData, os:getenv("TERM", ?DEFAULT_TERMINAL)),
	    proplists:get_value(width, TermData, Width),
	    proplists:get_value(height, TermData, Height),
	    proplists:get_value(pixel_widh, TermData, PixWidth),
	    proplists:get_value(pixel_height, TermData, PixHeight),
	    proplists:get_value(pty_opts, TermData, []), TimeOut
	   ).

%%--------------------------------------------------------------------
%% Not yet officially supported! The following functions are part of the
%% initial contributed ssh application. They are untested. Do we want them?
%% Should they be documented and tested?
%%--------------------------------------------------------------------
-doc false.
window_change(ConnectionRef, Channel, Width, Height) ->
    window_change(ConnectionRef, Channel, Width, Height, 0, 0).
-doc false.
window_change(ConnectionRef, Channel, Width, Height,
	      PixWidth, PixHeight) ->
    ssh_connection_handler:request(ConnectionRef, Channel,
				   "window-change", false, 
				   [?uint32(Width), ?uint32(Height),
				    ?uint32(PixWidth), ?uint32(PixHeight)], 0).

-doc false.
signal(ConnectionRef, Channel, Sig) ->
    ssh_connection_handler:request(ConnectionRef, Channel,
				   "signal", false, [?string(Sig)], 0).


-doc """
Is to be called by a server-channel process to send the exit status of a command
to the client.
""".
-spec exit_status(ConnectionRef, ChannelId, Status) -> ok when
      ConnectionRef :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id(),
      Status  :: integer().
exit_status(ConnectionRef, Channel, Status) ->
    ssh_connection_handler:request(ConnectionRef, Channel,
				   "exit-status", false, [?uint32(Status)], 0).

%%--------------------------------------------------------------------
%%% Internal, that is, ssh application internal API
%%--------------------------------------------------------------------

%%%----------------------------------------------------------------
%%% Send data on a channel/connection as result of for example
%%% ssh_connection:send (executed in the ssh_connection_state machine)
%%%

-doc false.
channel_data(ChannelId, DataType, Data0, 
	     #connection{channel_cache = Cache} = Connection,
	     From) ->
    case ssh_client_channel:cache_lookup(Cache, ChannelId) of
	#channel{remote_id = Id, sent_close = false} = Channel0 ->
            Data = ?to_binary(Data0),
	    {SendList, Channel} =
		update_send_window(Channel0#channel{flow_control = From}, DataType,
				   Data, Connection),
	    Replies = 
		lists:map(fun({SendDataType, SendData}) -> 
				  {connection_reply,
				   channel_data_msg(Id,
						    SendDataType,
						    SendData)}
			  end, SendList),
	    FlowCtrlMsgs = flow_control(Replies, Channel, Cache),
	    {Replies ++ FlowCtrlMsgs, Connection};
	_ ->
	    {[{channel_request_reply,From,{error,closed}}], Connection}
    end.

%%%----------------------------------------------------------------
%%% Handle the channel messages on behalf of the ssh_connection_handler
%%% state machine.
%%%
%%% Replies {Reply, UpdatedConnection}
%%%

-doc false.
handle_msg(#ssh_msg_disconnect{code = Code, description = Description}, Connection, _, _SSH) ->
    {disconnect, {Code, Description}, handle_stop(Connection)};

handle_msg(Msg, Connection, server, Ssh = #ssh{authenticated = false}) ->
    %% See RFC4252 6.
    %% Message numbers of 80 and higher are reserved for protocols running
    %% after this authentication protocol, so receiving one of them before
    %% authentication is complete is an error, to which the server MUST
    %% respond by disconnecting, preferably with a proper disconnect message
    %% sent to ease troubleshooting.
    MsgFun = fun(M) ->
                     MaxLogItemLen = ?GET_OPT(max_log_item_len, Ssh#ssh.opts),
                     io_lib:format("Connection terminated. Unexpected message for unauthenticated user."
                                   " Message:  ~w", [M],
                                   [{chars_limit, MaxLogItemLen}])
             end,
    ?LOG_DEBUG(MsgFun, [Msg]),
    {disconnect, {?SSH_DISCONNECT_PROTOCOL_ERROR, "Connection refused"}, handle_stop(Connection)};

handle_msg(#ssh_msg_channel_open_confirmation{recipient_channel = ChannelId, 
					      sender_channel = RemoteId,
					      initial_window_size = WindowSz,
					      maximum_packet_size = PacketSz}, 
	   #connection{channel_cache = Cache} = Connection0, _, _SSH) ->
    
    #channel{remote_id = undefined} = Channel =
	ssh_client_channel:cache_lookup(Cache, ChannelId), 
    
    ssh_client_channel:cache_update(Cache, Channel#channel{
				     remote_id = RemoteId,
				     recv_packet_size = max(32768, % rfc4254/5.2
							    min(PacketSz, Channel#channel.recv_packet_size)
							   ),
				     send_window_size = WindowSz,
				     send_packet_size = PacketSz}),
    reply_msg(Channel, Connection0, {open, ChannelId});
 
handle_msg(#ssh_msg_channel_open_failure{recipient_channel = ChannelId,
					 reason = Reason,
					 description = Descr,
					 lang = Lang},  
	   #connection{channel_cache = Cache} = Connection0, _, _SSH) ->
    Channel = ssh_client_channel:cache_lookup(Cache, ChannelId), 
    ssh_client_channel:cache_delete(Cache, ChannelId),
    reply_msg(Channel, Connection0, {open_error, Reason, Descr, Lang});

handle_msg(#ssh_msg_channel_success{recipient_channel = ChannelId}, Connection, _, _SSH) ->
    reply_msg(ChannelId, Connection, success);

handle_msg(#ssh_msg_channel_failure{recipient_channel = ChannelId}, Connection, _, _SSH) ->
    reply_msg(ChannelId, Connection, failure);

handle_msg(#ssh_msg_channel_eof{recipient_channel = ChannelId}, Connection, _, _SSH) ->
    reply_msg(ChannelId, Connection, {eof, ChannelId});
   
handle_msg(#ssh_msg_channel_close{recipient_channel = ChannelId},   
	   #connection{channel_cache = Cache} = Connection0, _, _SSH) ->

	case ssh_client_channel:cache_lookup(Cache, ChannelId) of
		#channel{sent_close = Closed, remote_id = RemoteId,
			 flow_control = FlowControl} = Channel ->
		ssh_client_channel:cache_delete(Cache, ChannelId),
		{CloseMsg, Connection} = 
		    reply_msg(Channel, Connection0, {closed, ChannelId}),
		ConnReplyMsgs =
		    case Closed of
			true -> [];
			false ->
			    RemoteCloseMsg = channel_close_msg(RemoteId),
			    [{connection_reply, RemoteCloseMsg}]
		    end,

		%% if there was a send() in progress, make it fail
		SendReplyMsgs =
		    case FlowControl of
			undefined -> [];
			From ->
			    [{flow_control, From, {error, closed}}]
		    end,

		Replies = ConnReplyMsgs ++ CloseMsg ++ SendReplyMsgs,
		{Replies, Connection};

	    undefined ->
		{[], Connection0}
	end;

handle_msg(#ssh_msg_channel_data{recipient_channel = ChannelId,
				 data = Data}, 
	   Connection, _, _SSH) ->
    channel_data_reply_msg(ChannelId, Connection, 0, Data);

handle_msg(#ssh_msg_channel_extended_data{recipient_channel = ChannelId,
					  data_type_code = DataType,
					  data = Data}, 
	   Connection, _, _SSH) ->
    channel_data_reply_msg(ChannelId, Connection, DataType, Data);

handle_msg(#ssh_msg_channel_window_adjust{recipient_channel = ChannelId,
					  bytes_to_add = Add},
	   #connection{channel_cache = Cache} = Connection, _, _SSH) ->
    case ssh_client_channel:cache_lookup(Cache, ChannelId) of
        Channel0 = #channel{send_window_size = Size,
                            remote_id = RemoteId} ->
            {SendList, Channel} =  %% TODO: Datatype 0 ?
                update_send_window(Channel0#channel{send_window_size = Size + Add},
                                   0, undefined, Connection),
            Replies = lists:map(fun({Type, Data}) ->
                                        {connection_reply,
                                         channel_data_msg(RemoteId, Type, Data)}
                                end, SendList),
            FlowCtrlMsgs = flow_control(Channel, Cache),
            {Replies ++ FlowCtrlMsgs, Connection};
        undefined ->
            {[], Connection}
    end;
handle_msg(#ssh_msg_channel_open{channel_type = "session" = Type,
				 sender_channel = RemoteId,
				 initial_window_size = WindowSz,
				 maximum_packet_size = PacketSz}, 
	   #connection{options = SSHopts} = Connection0,
	   server, _SSH) ->
    MinAcceptedPackSz =
        ?GET_OPT(minimal_remote_max_packet_size, SSHopts),
    
    if 
	MinAcceptedPackSz =< PacketSz ->
	    try setup_session(Connection0, RemoteId,
			      Type, WindowSz, PacketSz) of
		Result ->
		    Result
	    catch _:_ ->
		    FailMsg = channel_open_failure_msg(RemoteId, 
						       ?SSH_OPEN_CONNECT_FAILED,
						       "Connection refused", "en"),
		    {[{connection_reply, FailMsg}], Connection0}
	    end;

	MinAcceptedPackSz > PacketSz ->
	    FailMsg = channel_open_failure_msg(RemoteId, 
					       ?SSH_OPEN_ADMINISTRATIVELY_PROHIBITED,
					       lists:concat(["Maximum packet size below ",MinAcceptedPackSz,
							      " not supported"]), "en"),
	    {[{connection_reply, FailMsg}], Connection0}
    end;

handle_msg(#ssh_msg_channel_open{channel_type = "forwarded-tcpip",
				 sender_channel = RemoteId,
                                 initial_window_size = WindowSize,
                                 maximum_packet_size = PacketSize,
                                 data = <<?DEC_BIN(ConnectedHost,_L1), ?UINT32(ConnectedPort),
                                          ?DEC_BIN(_OriginHost,_L2), ?UINT32(_OriginPort)
                                        >>
                                },
           #connection{channel_cache = Cache,
                       channel_id_seed = ChId,
                       suggest_window_size = WinSz,
                       suggest_packet_size = PktSz,
                       options = Options,
                       connection_supervisor = ConnectionSup
                      } = C,
	   client, _SSH) ->
    {ReplyMsg, NextChId} =
        case ssh_connection_handler:retrieve(C, {tcpip_forward,ConnectedHost,ConnectedPort}) of
            {ok, {ConnectToHost,ConnectToPort}} ->
                case gen_tcp:connect(ConnectToHost, ConnectToPort, [{active,false}, binary]) of
                    {ok,Sock} ->
                        {ok,Pid} = ssh_connection_sup:start_channel(client, ConnectionSup, self(),
                                                                   ssh_tcpip_forward_client, ChId,
                                                                   [Sock], undefined, Options),
                        ssh_client_channel:cache_update(Cache,
                                                        #channel{type = "forwarded-tcpip",
                                                                 sys = "none",
                                                                 local_id = ChId,
                                                                 remote_id = RemoteId,
                                                                 user = Pid,
                                                                 recv_window_size = WinSz,
                                                                 recv_packet_size = PktSz,
                                                                 send_window_size = WindowSize,
                                                                 send_packet_size = PacketSize,
                                                                 send_buf = queue:new()
                                                                }),
                        gen_tcp:controlling_process(Sock, Pid),
                        inet:setopts(Sock, [{active,once}]),
                        {channel_open_confirmation_msg(RemoteId, ChId, WinSz, PktSz),
                         ChId + 1};

                    {error,Error} ->
                        {channel_open_failure_msg(RemoteId, 
                                                  ?SSH_OPEN_CONNECT_FAILED,
                                                  io_lib:format("Forwarded connection refused: ~p",[Error]),
                                                  "en"),
                         ChId}
                end;

            undefined ->
                {channel_open_failure_msg(RemoteId, 
                                          ?SSH_OPEN_CONNECT_FAILED,
                                          io_lib:format("No forwarding ordered",[]),
                                          "en"),
                 ChId}
        end,
    {[{connection_reply, ReplyMsg}], C#connection{channel_id_seed = NextChId}};

handle_msg(#ssh_msg_channel_open{channel_type = "direct-tcpip",
				 sender_channel = RemoteId,
                                 initial_window_size = WindowSize,
                                 maximum_packet_size = PacketSize,
                                 data = <<?DEC_BIN(HostToConnect,_L1),        ?UINT32(PortToConnect),
                                          ?DEC_BIN(_OriginatorIPaddress,_L2), ?UINT32(_OrignatorPort)
                                        >>
                                }, 
	   #connection{channel_cache = Cache,
                       channel_id_seed = ChId,
                       suggest_window_size = WinSz,
                       suggest_packet_size = PktSz,
                       options = Options,
                       connection_supervisor = ConnectionSup
                      } = C,
	   server, _SSH) ->
    Allowed = case ?GET_OPT(tcpip_tunnel_in, Options) of
                  T when is_boolean(T) -> T;
                  AllowedFun when is_function(AllowedFun, 2) ->
                      AllowedFun(binary_to_list(HostToConnect), PortToConnect)
              end,
    {ReplyMsg, NextChId} =
        case Allowed of
            denied ->
                {channel_open_failure_msg(RemoteId,
                                          ?SSH_OPEN_ADMINISTRATIVELY_PROHIBITED,
                                          "Not allowed", "en"),
                 ChId};

            false ->
                {channel_open_failure_msg(RemoteId, 
                                          ?SSH_OPEN_CONNECT_FAILED,
                                          "Forwarding disabled", "en"),
                 ChId};

            true ->
                case gen_tcp:connect(binary_to_list(HostToConnect), PortToConnect,
                                     [{active,false}, binary]) of
                    {ok,Sock} ->
                        {ok,Pid} = ssh_connection_sup:start_channel(server, ConnectionSup, self(),
                                                                   ssh_tcpip_forward_srv, ChId,
                                                                   [Sock], undefined, Options),
                        ssh_client_channel:cache_update(Cache,
                                                        #channel{type = "direct-tcpip",
                                                                 sys = "none",
                                                                 local_id = ChId,
                                                                 remote_id = RemoteId,
                                                                 user = Pid,
                                                                 recv_window_size = WinSz,
                                                                 recv_packet_size = PktSz,
                                                                 send_window_size = WindowSize,
                                                                 send_packet_size = PacketSize,
                                                                 send_buf = queue:new()
                                                                }),
                        gen_tcp:controlling_process(Sock, Pid),
                        inet:setopts(Sock, [{active,once}]),

                        {channel_open_confirmation_msg(RemoteId, ChId, WinSz, PktSz),
                         ChId + 1};

                    {error,Error} ->
                        {channel_open_failure_msg(RemoteId, 
                                                  ?SSH_OPEN_CONNECT_FAILED,
                                                  io_lib:format("Forwarded connection refused: ~p",[Error]),
                                                  "en"),
                         ChId}
                end
        end,
    {[{connection_reply, ReplyMsg}], C#connection{channel_id_seed = NextChId}};

handle_msg(#ssh_msg_channel_open{channel_type = "session",
				 sender_channel = RemoteId}, 
	   Connection,
	   client, _SSH) ->
    %% Client implementations SHOULD reject any session channel open
    %% requests to make it more difficult for a corrupt server to attack the
    %% client. See See RFC 4254 6.1.
    FailMsg = channel_open_failure_msg(RemoteId, 
				       ?SSH_OPEN_CONNECT_FAILED,
				       "Connection refused", "en"),
    {[{connection_reply, FailMsg}], Connection};

handle_msg(#ssh_msg_channel_open{sender_channel = RemoteId}, Connection, _, _SSH) ->
    FailMsg = channel_open_failure_msg(RemoteId, 
				       ?SSH_OPEN_ADMINISTRATIVELY_PROHIBITED,
				       "Not allowed", "en"),
    {[{connection_reply, FailMsg}], Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "exit-status",
				    data = Data},
           Connection, _, _SSH) ->
    <<?UINT32(Status)>> = Data,
    reply_msg(ChannelId, Connection, {exit_status, ChannelId, Status});

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "exit-signal",
				    want_reply = false,
				    data = Data},
           #connection{channel_cache = Cache} = Connection0, _, _SSH) ->
    <<?DEC_BIN(SigName, _SigLen),
      ?BOOLEAN(_Core),
      ?DEC_BIN(Err, _ErrLen),
      ?DEC_BIN(Lang, _LangLen)>> = Data,
    case ssh_client_channel:cache_lookup(Cache, ChannelId) of
        #channel{remote_id = RemoteId} = Channel ->
            {Reply, Connection} =  reply_msg(Channel, Connection0,
                                             {exit_signal, ChannelId,
                                              binary_to_list(SigName),
                                              binary_to_list(Err),
                                              binary_to_list(Lang)}),
            ChannelCloseMsg = channel_close_msg(RemoteId),
            {[{connection_reply, ChannelCloseMsg}|Reply], Connection};
        _ ->
            %% Channel already closed by peer
            {[], Connection0}
    end;

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "xon-xoff",
				    want_reply = false,
				    data = Data},
           Connection, _, _SSH) ->
    <<?BOOLEAN(CDo)>> = Data,
    reply_msg(ChannelId, Connection, {xon_xoff, ChannelId, CDo=/= 0});

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "window-change",
				    want_reply = false,
				    data = Data}, 
           Connection0, _, _SSH) ->
    <<?UINT32(Width),?UINT32(Height),
      ?UINT32(PixWidth), ?UINT32(PixHeight)>> = Data,
    reply_msg(ChannelId, Connection0, {window_change, ChannelId,
                                       Width, Height,
                                       PixWidth, PixHeight});

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "signal",
				    data = Data}, 
           Connection0, _, _SSH) ->
    <<?DEC_BIN(SigName, _SigLen)>> = Data,
    reply_msg(ChannelId, Connection0, {signal, ChannelId,
                                       binary_to_list(SigName)});

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "subsystem",
				    want_reply = WantReply,
				    data = Data},
	   #connection{channel_cache = Cache} = Connection, server, _SSH) ->
    <<?DEC_BIN(SsName,_SsLen)>> = Data,
    #channel{remote_id=RemoteId} = Channel = 
	ssh_client_channel:cache_lookup(Cache, ChannelId), 
    Reply =
        case start_subsystem(SsName, Connection, Channel,
                             {subsystem, ChannelId, WantReply, binary_to_list(SsName)}) of
            {ok, Pid} ->
                erlang:monitor(process, Pid),
                ssh_client_channel:cache_update(Cache, Channel#channel{user=Pid}),
                channel_success_msg(RemoteId);
            {error,_Error} ->
                channel_failure_msg(RemoteId)
        end,
    {[{connection_reply,Reply}], Connection};

handle_msg(#ssh_msg_channel_request{request_type = "subsystem"},
	   Connection, client, _SSH) ->
    %% The client SHOULD ignore subsystem requests. See RFC 4254 6.5.
    {[], Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "pty-req",
				    want_reply = WantReply,
				    data = Data},
	   Connection, server, _SSH) ->
    <<?DEC_BIN(BTermName,_TermLen),
      ?UINT32(Width),?UINT32(Height),
      ?UINT32(PixWidth), ?UINT32(PixHeight),
      Modes/binary>> = Data,
    TermName = binary_to_list(BTermName),
    PtyOpts0 = decode_pty_opts(Modes),
    PtyOpts = case proplists:get_value(onlcr, PtyOpts0, undefined) of
                  undefined ->
                      %% If - peer client asked for pty
                      %%    - did not tell if LF->CRLF expansion is wanted
                      %% then
                      %%    - do LF->CRLF expansion
                      [{onlcr,1} | PtyOpts0];
                  _ ->
                      PtyOpts0
              end,
    PtyRequest = {TermName, Width, Height,
		  PixWidth, PixHeight, PtyOpts},
    handle_cli_msg(Connection, ChannelId,
		   {pty, ChannelId, WantReply, PtyRequest});

handle_msg(#ssh_msg_channel_request{request_type = "pty-req"},
	   Connection, client, _SSH) ->
    %% The client SHOULD ignore pty requests. See RFC 4254 6.2.
    {[], Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "shell",
				    want_reply = WantReply},
	   Connection, server, _SSH) ->
    handle_cli_msg(Connection, ChannelId,
		   {shell, ChannelId, WantReply});
 
handle_msg(#ssh_msg_channel_request{request_type = "shell"},
	   Connection, client, _SSH) ->
    %% The client SHOULD ignore shell requests. See RFC 4254 6.5.
    {[], Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
				    request_type = "exec",
				    want_reply = WantReply,
				    data = Data},
	   Connection, server, _SSH) ->
    <<?DEC_BIN(Command, _Len)>> = Data,
    handle_cli_msg(Connection, ChannelId,
		   {exec, ChannelId, WantReply, binary_to_list(Command)});
	
handle_msg(#ssh_msg_channel_request{request_type = "exec"},
	   Connection, client, _SSH) ->
    %% The client SHOULD ignore exec requests. See RFC 4254 6.5.
    {[], Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
  				    request_type = "env",
  				    want_reply = WantReply,
  				    data = Data}, 
	   Connection, server, _SSH) ->
    <<?DEC_BIN(Var,_VarLen), ?DEC_BIN(Value,_ValLen)>> = Data,
    handle_cli_msg(Connection, ChannelId,
 		   {env, ChannelId, WantReply, Var, Value});

handle_msg(#ssh_msg_channel_request{request_type = "env"},
	   Connection, client, _SSH) ->
    %% The client SHOULD ignore env requests. 
    {[], Connection};

handle_msg(#ssh_msg_channel_request{recipient_channel = ChannelId,
                                    want_reply = WantReply},
	   #connection{channel_cache = Cache} = Connection, _, _SSH) ->
    %% Not a valid request_type. All valid types are handling the
    %% parameter checking in their own clauses above.
    %% 
    %% The special ReqType faulty_msg signals that something went
    %% wrong found during decoding.
    %%
    %% RFC4254 5.4 says:
    %% "If 'want reply' is FALSE, no response will be sent to the request.
    %%  Otherwise, the recipient responds with either
    %%  SSH_MSG_CHANNEL_SUCCESS, SSH_MSG_CHANNEL_FAILURE, or request-specific
    %%  continuation messages.  If the request is not recognized or is not
    %%  supported for the channel, SSH_MSG_CHANNEL_FAILURE is returned."
    %%
    case ssh_client_channel:cache_lookup(Cache, ChannelId) of
        #channel{remote_id = RemoteId} when WantReply==true -> 
            FailMsg = channel_failure_msg(RemoteId),
            {[{connection_reply, FailMsg}], Connection};
        _ -> %% Channel has been closed or no reply is wanted
            {[], Connection}
    end;

handle_msg(#ssh_msg_global_request{name = <<"tcpip-forward">>,
				   want_reply = WantReply,
				   data = <<?DEC_BIN(ListenAddrStr,_Len),?UINT32(ListenPort)>>},
           #connection{options = Opts} = Connection, server, _SSH) ->
    case ?GET_OPT(tcpip_tunnel_out, Opts) of
        false ->
            %% This daemon instance has not enabled tcpip_forwarding
            {[{connection_reply, request_failure_msg()}], Connection};

        true ->
            ConnectionSup = ?GET_INTERNAL_OPT(connection_sup, Opts),
            FwdSup = ssh_connection_sup:tcpip_fwd_supervisor(ConnectionSup),
            ConnPid = self(),
            case ssh_tcpip_forward_acceptor:supervised_start(FwdSup,
                                                             {ListenAddrStr, ListenPort},
                                                             undefined,
                                                             "forwarded-tcpip", ssh_tcpip_forward_srv,
                                                             ConnPid) of
                {ok,ListenPort} when WantReply==true ->
                    {[{connection_reply, request_success_msg(<<>>)}], Connection};

                {ok,LPort} when WantReply==true ->
                    {[{connection_reply, request_success_msg(<<?UINT32(LPort)>>)}], Connection};

                {error,_} when WantReply==true ->
                    {[{connection_reply, request_failure_msg()}], Connection};

                _ when WantReply==true ->
                    {[{connection_reply, request_failure_msg()}], Connection};

                _ ->
                    {[], Connection}
            end
    end;

handle_msg(#ssh_msg_global_request{name = _Type,
				   want_reply = WantReply,
				   data = _Data}, Connection, _Role, _SSH) ->
    if WantReply == true ->
	    FailMsg = request_failure_msg(),
	    {[{connection_reply, FailMsg}], Connection};
       true ->
	    {[], Connection}  
    end;

handle_msg(#ssh_msg_request_failure{},
	   #connection{requests = [{_, From} | Rest]} = Connection, _, _SSH) ->
    {[{channel_request_reply, From, {failure, <<>>}}],
     Connection#connection{requests = Rest}};

handle_msg(#ssh_msg_request_failure{},
	   #connection{requests = [{_, From,_} | Rest]} = Connection, _, _SSH) ->
    {[{channel_request_reply, From, {failure, <<>>}}],
     Connection#connection{requests = Rest}};

handle_msg(#ssh_msg_request_success{data = Data},
	   #connection{requests = [{_, From} | Rest]} = Connection, _, _SSH) ->
    {[{channel_request_reply, From, {success, Data}}],
     Connection#connection{requests = Rest}};

handle_msg(#ssh_msg_request_success{data = Data},
	   #connection{requests = [{_, From, Fun} | Rest]} = Connection0, _, _SSH) ->
    Connection = Fun({success,Data}, Connection0),
    {[{channel_request_reply, From, {success, Data}}],
     Connection#connection{requests = Rest}}.


%%%----------------------------------------------------------------
%%% Returns pending responses to be delivered to the peer when a
%%% Channel/Connection closes
%%%
-doc false.
handle_stop(#connection{channel_cache = Cache} = Connection0) ->
    {Connection, Replies} = 
	ssh_client_channel:cache_foldl(
          fun(Channel, {Connection1, Acc}) ->
                  {Reply, Connection2} =
                      reply_msg(Channel, Connection1,
                                {closed, Channel#channel.local_id}),
                  {Connection2, Reply ++ Acc}
          end, {Connection0, []}, Cache),
    ssh_client_channel:cache_delete(Cache),
    {Replies, Connection}.

%%%----------------------------------------------------------------
%%% channel_*_msg(...)
%%% Returns a #ssh_msg_....{} for channel operations.
%%%
-doc false.
channel_adjust_window_msg(ChannelId, Bytes) ->
    #ssh_msg_channel_window_adjust{recipient_channel = ChannelId,
				   bytes_to_add = Bytes}.

-doc false.
channel_close_msg(ChannelId) ->
    #ssh_msg_channel_close {recipient_channel = ChannelId}.

-doc false.
channel_data_msg(ChannelId, 0, Data) ->
    #ssh_msg_channel_data{recipient_channel = ChannelId,
			  data = Data};
channel_data_msg(ChannelId, Type, Data) ->
    #ssh_msg_channel_extended_data{recipient_channel = ChannelId,
				    data_type_code = Type,
				    data = Data}.

-doc false.
channel_eof_msg(ChannelId) ->
    #ssh_msg_channel_eof{recipient_channel = ChannelId}.

-doc false.
channel_failure_msg(ChannelId) ->
    #ssh_msg_channel_failure{recipient_channel = ChannelId}.

-doc false.
channel_open_msg(Type, ChannelId, WindowSize, MaxPacketSize, Data) ->
    #ssh_msg_channel_open{channel_type = Type,
			  sender_channel = ChannelId,
			  initial_window_size = WindowSize,
			  maximum_packet_size = MaxPacketSize,
			  data = Data
			 }.

-doc false.
channel_open_confirmation_msg(RemoteId, LID, WindowSize, PacketSize) ->
    #ssh_msg_channel_open_confirmation{recipient_channel = RemoteId,
				       sender_channel = LID,
				       initial_window_size = WindowSize,
				       maximum_packet_size = PacketSize}.

-doc false.
channel_open_failure_msg(RemoteId, Reason, Description, Lang) ->
    #ssh_msg_channel_open_failure{recipient_channel = RemoteId,
				  reason = Reason,
				  description = Description,
				  lang = Lang}.

-doc false.
channel_status_msg({success, ChannelId}) ->
    channel_success_msg(ChannelId);

channel_status_msg({failure, ChannelId}) ->
    channel_failure_msg(ChannelId).

-doc false.
channel_request_msg(ChannelId, Type, WantReply, Data) ->
    #ssh_msg_channel_request{recipient_channel = ChannelId,
			     request_type = Type,
			     want_reply = WantReply,
			     data = Data}.

-doc false.
channel_success_msg(ChannelId) ->
    #ssh_msg_channel_success{recipient_channel = ChannelId}.

%%%----------------------------------------------------------------
%%% request_*_msg(...)
%%% Returns a #ssh_msg_....{}
%%%
-doc false.
request_global_msg(Name, WantReply, Data) ->
    #ssh_msg_global_request{name = Name,
                            want_reply = WantReply,
                            data = Data}.

-doc false.
request_failure_msg() ->
    #ssh_msg_request_failure{}.

-doc false.
request_success_msg(Data) ->
    #ssh_msg_request_success{data = Data}.

%%%----------------------------------------------------------------
%%%
%%%
-doc false.
encode_ip(Addr) when is_tuple(Addr) ->
    case catch inet_parse:ntoa(Addr) of
	{'EXIT',_} -> false;
	A -> A
    end;
encode_ip(Addr) when is_list(Addr) ->
    case inet_parse:address(Addr) of
	{ok, _} -> Addr;
	Error ->
	    case inet:getaddr(Addr, inet) of
		{ok, A} ->
		    inet_parse:ntoa(A);
		Error -> false
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Internal functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------
%%% Create the channel data when an ssh_msg_open_channel message
%%% of "session" typ is handled
%%%
setup_session(#connection{channel_cache = Cache,
                          channel_id_seed = NewChannelID,
                          suggest_window_size = WinSz,
                          suggest_packet_size = PktSz
			 } = C,
	      RemoteId, Type, WindowSize, PacketSize) when is_integer(WinSz),
                                                           is_integer(PktSz) ->
    NextChannelID = NewChannelID + 1,
    Channel =
        #channel{type = Type,
                 sys = "ssh",
                 local_id = NewChannelID,
                 recv_window_size = WinSz,
                 recv_packet_size = PktSz,
                 send_window_size = WindowSize,
                 send_packet_size = PacketSize,
                 send_buf = queue:new(),
                 remote_id = RemoteId
                },
    ssh_client_channel:cache_update(Cache, Channel),
    OpenConfMsg = channel_open_confirmation_msg(RemoteId, NewChannelID,
						WinSz, 
						PktSz),
                Reply = {connection_reply, OpenConfMsg},
    {[Reply], C#connection{channel_id_seed = NextChannelID}}.


%%%----------------------------------------------------------------
%%% Start a cli or subsystem
%%%
start_cli(#connection{options = Options, 
		      cli_spec = CliSpec,
		      exec = Exec,
		      connection_supervisor = ConnectionSup}, ChannelId) ->
    case CliSpec of
        no_cli ->
            {error, cli_disabled};
        {CbModule, Args} ->
            ssh_connection_sup:start_channel(server, ConnectionSup, self(), CbModule, ChannelId, Args, Exec, Options)
    end.


start_subsystem(BinName, #connection{options = Options,
                                     connection_supervisor = ConnectionSup},
	       #channel{local_id = ChannelId}, _ReplyMsg) ->
    Name = binary_to_list(BinName),
    case check_subsystem(Name, Options) of
	{Callback, Opts} when is_atom(Callback), Callback =/= none ->
            ssh_connection_sup:start_channel(server, ConnectionSup, self(), Callback, ChannelId, Opts, undefined, Options);
        {none, _} ->
            {error, bad_subsystem};
	{_, _} ->
	    {error, legacy_option_not_supported}
    end.


%%% Helpers for starting cli/subsystems
check_subsystem("sftp"= SsName, Options) ->
    case ?GET_OPT(subsystems, Options) of
	no_subsys -> 	% FIXME: Can 'no_subsys' ever be matched?
	    {SsName, {Cb, Opts}} = ssh_sftpd:subsystem_spec([]),
	    {Cb, Opts};
	SubSystems ->
	    proplists:get_value(SsName, SubSystems, {none, []})
    end;

check_subsystem(SsName, Options) ->
    Subsystems = ?GET_OPT(subsystems, Options),
    case proplists:get_value(SsName, Subsystems, {none, []}) of
	Fun when is_function(Fun) ->
	    {Fun, []};
	{_, _} = Value ->
	    Value
    end.

%%%----------------------------------------------------------------
%%%
%%% Send-window handling
%%%

update_send_window(Channel, _, undefined,
		   #connection{channel_cache = Cache}) ->
    do_update_send_window(Channel, Cache);

update_send_window(#channel{send_buf = SendBuffer} = Channel, DataType, Data,
		   #connection{channel_cache = Cache}) ->
    do_update_send_window(Channel#channel{send_buf = queue:in({DataType, Data}, SendBuffer)},
			  Cache).

do_update_send_window(Channel0, Cache) ->
    {SendMsgs, Channel} = get_window(Channel0, []),
    ssh_client_channel:cache_update(Cache, Channel), 
    {SendMsgs, Channel}.

get_window(#channel{send_window_size = 0
		   } = Channel, Acc) ->
    {lists:reverse(Acc), Channel};
get_window(#channel{send_packet_size = 0
		   } = Channel, Acc) ->
    {lists:reverse(Acc), Channel};
get_window(#channel{send_buf = Buffer, 
		    send_packet_size = PacketSize,
		    send_window_size = WindowSize0
		   } = Channel, Acc0) ->
    case queue:out(Buffer) of
	{{value, {_, Data} = Msg}, NewBuffer} ->
	    case handle_send_window(Msg, byte_size(Data), PacketSize, WindowSize0, Acc0) of
		{WindowSize, Acc, {_, <<>>}} ->
		    {lists:reverse(Acc), Channel#channel{send_window_size = WindowSize,
							 send_buf = NewBuffer}};
		{WindowSize, Acc, Rest} ->
		    get_window(Channel#channel{send_window_size = WindowSize,
					       send_buf = queue:in_r(Rest, NewBuffer)}, Acc)
	    end;
	{empty, NewBuffer} ->
	    {[], Channel#channel{send_buf = NewBuffer}}
    end.

handle_send_window(Msg = {Type, Data}, Size, PacketSize, WindowSize, Acc) when Size =< WindowSize ->
    case Size =< PacketSize of
	true ->
	    {WindowSize - Size, [Msg | Acc], {Type, <<>>}};
	false ->
	    <<Msg1:PacketSize/binary, Msg2/binary>> = Data,
	    {WindowSize - PacketSize, [{Type, Msg1} | Acc], {Type, Msg2}}
    end;
handle_send_window({Type, Data}, _, PacketSize, WindowSize, Acc) when WindowSize =< PacketSize ->
    <<Msg1:WindowSize/binary, Msg2/binary>> = Data,
    {WindowSize - WindowSize, [{Type, Msg1} | Acc], {Type, Msg2}};
handle_send_window({Type, Data}, _, PacketSize, WindowSize, Acc) ->
    <<Msg1:PacketSize/binary, Msg2/binary>> = Data,
    {WindowSize - PacketSize, [{Type, Msg1} | Acc], {Type, Msg2}}.

%%%----------------------------------------------------------------
%%%
%%% Flow control
%%% 

flow_control(Channel, Cache) ->
    flow_control([window_adjusted], Channel, Cache).

flow_control([], Channel, Cache) ->
    ssh_client_channel:cache_update(Cache, Channel),
    [];
flow_control([_|_], #channel{flow_control = From,
			     send_buf = Buffer} = Channel, Cache) when From =/= undefined ->
    case queue:is_empty(Buffer) of
	true ->
	    ssh_client_channel:cache_update(Cache, Channel#channel{flow_control = undefined}),
	    [{flow_control, Cache, Channel, From, ok}];
	false ->
	    []
    end;
flow_control(_,_,_) ->
    [].

%%%----------------------------------------------------------------
%%%
%%% Pseudo terminal stuff
%%% 

pty_req(ConnectionRef, Channel, Term, Width, Height,
	 PixWidth, PixHeight, PtyOpts, TimeOut) ->
    ssh_connection_handler:request(ConnectionRef,
				   Channel, "pty-req", true,
				   [?string(Term),
				    ?uint32(Width), ?uint32(Height),
				    ?uint32(PixWidth),?uint32(PixHeight),
				    encode_pty_opts(PtyOpts)], TimeOut).

pty_default_dimensions(Dimension, TermData) ->
    case proplists:get_value(Dimension, TermData, 0) of
	N when is_integer(N), N > 0 ->
	    {N, 0};
	_ ->
            PixelDim = list_to_atom("pixel_" ++ atom_to_list(Dimension)),
	    case proplists:get_value(PixelDim, TermData, 0) of
		N when is_integer(N), N > 0 ->
		    {0, N};
		_ ->
		    {?TERMINAL_WIDTH, 0}
	    end
    end.

-doc false.
encode_pty_opts(Opts) ->
    Bin = list_to_binary(encode_pty_opts2(Opts)),
    <<?STRING(Bin)>>.

encode_pty_opts2([]) -> 
    [?TTY_OP_END];
encode_pty_opts2([{vintr,Value} | Opts]) ->
    [?VINTR, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vquit,Value} | Opts]) ->
    [?VQUIT, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{verase,Value} | Opts]) ->
    [?VERASE, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vkill,Value} | Opts]) ->
    [?VKILL, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{veof,Value} | Opts]) ->
    [?VEOF, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{veol,Value} | Opts]) ->
    [?VEOL, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{veol2,Value} | Opts]) ->
    [?VEOL2, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vstart,Value} | Opts]) ->
    [?VSTART, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vstop,Value} | Opts]) ->
    [?VSTOP, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vsusp,Value} | Opts]) ->
    [?VSUSP, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vdsusp,Value} | Opts]) ->
    [?VDSUSP, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vreprint,Value} | Opts]) ->
    [?VREPRINT, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vwerase,Value} | Opts]) ->
    [ ?VWERASE, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vlnext,Value} | Opts]) ->
    [?VLNEXT, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vflush,Value} | Opts]) ->
    [?VFLUSH, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vswtch,Value} | Opts]) ->
    [?VSWTCH, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vstatus,Value} | Opts]) ->
    [?VSTATUS, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{vdiscard,Value} | Opts]) ->
    [?VDISCARD, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{ignpar,Value} | Opts]) ->
    [?IGNPAR, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{parmrk,Value} | Opts]) ->
    [?PARMRK, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{inpck,Value} | Opts]) ->
    [?INPCK, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{istrip,Value} | Opts]) ->
    [?ISTRIP, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{inlcr,Value} | Opts]) ->
    [?INLCR, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{igncr,Value} | Opts]) ->
    [?IGNCR, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{icrnl,Value} | Opts]) ->
    [?ICRNL, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{iuclc,Value} | Opts]) ->
    [?IUCLC, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{ixon,Value} | Opts]) ->
    [?IXON, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{ixany,Value} | Opts]) ->
    [?IXANY, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{ixoff,Value} | Opts]) ->
    [?IXOFF, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{imaxbel,Value} | Opts]) ->
    [?IMAXBEL, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{iutf8,Value} | Opts]) ->
    [?IUTF8, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{isig,Value} | Opts]) ->
    [?ISIG, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{icanon,Value} | Opts]) ->
    [?ICANON, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{xcase,Value} | Opts]) ->
    [?XCASE, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{echo,Value} | Opts]) ->
    [?ECHO, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{echoe,Value} | Opts]) ->
    [?ECHOE, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{echok,Value} | Opts]) ->
    [?ECHOK, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{echonl,Value} | Opts]) ->
    [?ECHONL, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{noflsh,Value} | Opts]) ->
    [?NOFLSH, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{tostop,Value} | Opts]) ->
    [?TOSTOP, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{iexten,Value} | Opts]) ->
    [?IEXTEN, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{echoctl,Value} | Opts]) ->
    [?ECHOCTL, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{echoke,Value} | Opts]) ->
    [?ECHOKE, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{pendin,Value} | Opts]) ->
    [?PENDIN, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{opost,Value} | Opts]) ->
    [?OPOST, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{olcuc,Value} | Opts]) ->
    [?OLCUC, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{onlcr,Value} | Opts]) ->
    [?ONLCR, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{ocrnl,Value} | Opts]) ->
    [?OCRNL, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{onocr,Value} | Opts]) ->
    [?ONOCR, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{onlret,Value} | Opts]) ->
    [?ONLRET, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{cs7,Value} | Opts]) ->
    [?CS7, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{cs8,Value} | Opts]) ->
    [?CS8, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{parenb,Value} | Opts]) ->
    [?PARENB, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{parodd,Value} | Opts]) ->
    [?PARODD, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{tty_op_ispeed,Value} | Opts]) ->
    [?TTY_OP_ISPEED, ?uint32(Value) | encode_pty_opts2(Opts)];
encode_pty_opts2([{tty_op_ospeed,Value} | Opts]) ->
    [?TTY_OP_OSPEED, ?uint32(Value) | encode_pty_opts2(Opts)].

-doc false.
decode_pty_opts(<<>>) ->		     
    [];
decode_pty_opts(<<0, 0, 0, 0>>) ->
    [];
decode_pty_opts(<<?DEC_BIN(Modes,_Len)>>) ->
    decode_pty_opts2(Modes);
decode_pty_opts(Binary) ->
    decode_pty_opts2(Binary).

decode_pty_opts2(<<?TTY_OP_END>>) ->		     
    [];
decode_pty_opts2(<<Code, ?UINT32(Value), Tail/binary>>) ->
    Op = case Code of
	     ?VINTR -> vintr;
	     ?VQUIT -> vquit;
	     ?VERASE -> verase;
	     ?VKILL -> vkill;
	     ?VEOF -> veof;
	     ?VEOL -> veol;
	     ?VEOL2 -> veol2;
	     ?VSTART -> vstart;
	     ?VSTOP -> vstop;
	     ?VSUSP -> vsusp;
	     ?VDSUSP -> vdsusp;
	     ?VREPRINT -> vreprint;
	     ?VWERASE -> vwerase;
	     ?VLNEXT -> vlnext;
	     ?VFLUSH -> vflush;
	     ?VSWTCH -> vswtch;
	     ?VSTATUS -> vstatus;
	     ?VDISCARD -> vdiscard;
	     ?IGNPAR -> ignpar;
	     ?PARMRK -> parmrk;
	     ?INPCK -> inpck;
	     ?ISTRIP -> istrip;
	     ?INLCR -> inlcr;
	     ?IGNCR -> igncr;
	     ?ICRNL -> icrnl;
	     ?IUCLC -> iuclc;
	     ?IXON -> ixon;
	     ?IXANY -> ixany;
	     ?IXOFF -> ixoff;
	     ?IMAXBEL -> imaxbel;
             ?IUTF8 -> iutf8; % RFC 8160
	     ?ISIG -> isig;
	     ?ICANON -> icanon;
	     ?XCASE -> xcase;
	     ?ECHO -> echo;
	     ?ECHOE -> echoe;
	     ?ECHOK -> echok;
	     ?ECHONL -> echonl;
	     ?NOFLSH -> noflsh;
	     ?TOSTOP -> tostop;
	     ?IEXTEN -> iexten;
	     ?ECHOCTL -> echoctl;
	     ?ECHOKE -> echoke;
	     ?PENDIN -> pendin;
	     ?OPOST -> opost;
	     ?OLCUC -> olcuc;
	     ?ONLCR -> onlcr;
	     ?OCRNL -> ocrnl;
	     ?ONOCR -> onocr;
	     ?ONLRET -> onlret;
	     ?CS7 -> cs7;
	     ?CS8 -> cs8;
	     ?PARENB -> parenb;
	     ?PARODD -> parodd;
	     ?TTY_OP_ISPEED -> tty_op_ispeed;
	     ?TTY_OP_OSPEED -> tty_op_ospeed;
	     _ -> Code
	 end,    
    [{Op, Value} | decode_pty_opts2(Tail)].


backwards_compatible([], Acc) ->
    Acc;
backwards_compatible([{hight, Value} | Rest], Acc) ->
    backwards_compatible(Rest, [{height, Value} | Acc]);
backwards_compatible([{pixel_hight, Value} | Rest], Acc) ->
    backwards_compatible(Rest, [{height, Value} | Acc]);
backwards_compatible([Value| Rest], Acc) ->
    backwards_compatible(Rest, [ Value | Acc]).


%%%----------------------------------------------------------------
%%%
%%% Common part of handling channel messages meant for a cli (like "env", "exec" etc)
%%% Called at the finnish of handle_msg(#ssh_msg_channel_request,...)
%%%

handle_cli_msg(C0, ChId, Reply0) ->
    Cache = C0#connection.channel_cache,
    Ch0 = ssh_client_channel:cache_lookup(Cache, ChId),
    case Ch0#channel.user of
        undefined ->
            case start_cli(C0, ChId) of
                {ok, Pid} ->
                    erlang:monitor(process, Pid),
                    Ch = Ch0#channel{user = Pid},
                    ssh_client_channel:cache_update(Cache, Ch),
                    reply_msg(Ch, C0, Reply0);
                {error, _Error} ->
                    Reply = {connection_reply, channel_failure_msg(Ch0#channel.remote_id)},
                    {[Reply], C0}
            end;
        
        _ ->
            reply_msg(Ch0, C0, Reply0)
    end.

%%%----------------------------------------------------------------
%%%
%%% TCP/IP forwarding

%%%----------------------------------------------------------------
%%%
%%% Request response handling on return to the calling ssh_connection_handler
%%% state machine.
%%% 

channel_data_reply_msg(ChannelId, Connection, DataType, Data) ->
    case ssh_client_channel:cache_lookup(Connection#connection.channel_cache, ChannelId) of
	#channel{recv_window_size = Size} = Channel ->
	    WantedSize = Size - byte_size(Data),
	    ssh_client_channel:cache_update(Connection#connection.channel_cache, 
                                     Channel#channel{recv_window_size = WantedSize}),
            reply_msg(Channel, Connection, {data, ChannelId, DataType, Data});
	undefined ->
	    {[], Connection}
    end.


reply_msg(ChId, C, Reply) when is_integer(ChId) ->
    reply_msg(ssh_client_channel:cache_lookup(C#connection.channel_cache, ChId), C, Reply);

reply_msg(Channel, Connection, {open, _} = Reply) ->
    request_reply_or_data(Channel, Connection, Reply);
reply_msg(Channel, Connection, {open_error, _, _, _} = Reply) ->
    request_reply_or_data(Channel, Connection, Reply);
reply_msg(Channel, Connection, success = Reply) ->
    request_reply_or_data(Channel, Connection, Reply);
reply_msg(Channel, Connection, failure = Reply) ->
    request_reply_or_data(Channel, Connection, Reply);
reply_msg(Channel, Connection, {closed, _} = Reply) ->
    request_reply_or_data(Channel, Connection, Reply);
reply_msg(undefined, Connection, _Reply) ->
    {[], Connection};
reply_msg(#channel{user = ChannelPid}, Connection, Reply) ->
    {[{channel_data, ChannelPid, Reply}], Connection}.


request_reply_or_data(#channel{local_id = ChannelId, user = ChannelPid}, 
		      #connection{requests = Requests} = 
		      Connection, Reply) -> 
    case lists:keysearch(ChannelId, 1, Requests) of
	{value, {ChannelId, From}} ->
	    {[{channel_request_reply, From, Reply}],
	     Connection#connection{requests = 
				       lists:keydelete(ChannelId, 1, Requests)}};
	false when (Reply == success) or (Reply == failure) ->
	    {[], Connection};
	false ->
	    {[{channel_data, ChannelPid, Reply}], Connection}
    end.

%%%----------------------------------------------------------------
-doc false.
send_environment_vars(ConnectionRef, Channel, VarNames) ->
    lists:foldl(
      fun(Var, success) ->
              case os:getenv(Var) of
                  false ->
                      success;
                  Value ->
                      setenv(ConnectionRef, Channel, false,
                             Var, Value, infinity)
              end
      end, success, VarNames).
