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

%%
%%----------------------------------------------------------------------
%% Purpose: Megaco transport behaviour module
%%----------------------------------------------------------------------

-module(megaco_transport).
-moduledoc """
Megaco transport behaviour.

The following functions should be exported from a `megaco_transport` callback
module:

- [send_message/2](`m:megaco_transport#send_message`) [`mandatory`]
- [send_message/3](`m:megaco_transport#send_message`) [`optional`]
- [resend_message/2](`m:megaco_transport#resend_message`) [`optional`]
-

[](){: #send_message }
""".

-doc(#{equiv => {callback,send_message,3}}).
-callback send_message(Handle, Msg) -> ok | {cancel, Reason :: term()} | Error when
      Handle :: term(),
      Msg :: iodata(),
      Error :: term().

-doc """
Send a megaco message.

If the function returns `{cancel, Reason}`, this means the transport module
decided not to send the message. This is _not_ an error. No error messages will
be issued and no error counters incremented. What actions this will result in
depends on what kind of message was sent.

In the case of requests, megaco will cancel the message in much the same way as
if `megaco:cancel` had been called (after a successfull send). The information
will be propagated back to the user differently depending on how the request(s)
where issued: For requests issued using [megaco:call](`m:megaco#call`), the info
will be delivered in the return value. For requests issued using `megaco:cast`
the info will be delivered via a call to the callback function
[handle_trans_reply](`m:megaco_user#trans_reply`).

In the case of reply, megaco will cancel the reply and information of this will
be returned to the user via a call to the callback function
[handle_trans_ack](`m:megaco_user#trans_ack`).

The function [`send_message/3`](`c:send_message/3`) will only be called if the
[resend_indication](`m:megaco#ui_resend_indication`) config option has been set
to the value `flag`. The third argument, `Resend` then indicates if the message
send is a resend or not.

[](){: #resend_message }
""".
-callback send_message(Handle, Msg, Resend) -> ok | {cancel, Reason :: term()} | Error  when
      Handle :: term(),
      Msg :: iodata(),
      Resend :: boolean(),
      Error :: term().

-doc """
Re-send a megaco message.

Note that this function will only be called if the user has set the
[resend_indication](`m:megaco#ui_resend_indication`) config option to
`true`_and_ it is in fact a message resend. If not _both_ of these condition's
are meet, `send_message` will be called.

If the function returns `{cancel, Reason}`, this means the transport module
decided not to send the message. This is _not_ an error. No error messages will
be issued and no error counters incremented. What actions this will result in
depends on what kind of message was sent.

In the case of requests, megaco will cancel the message in much the same way as
if `megaco:cancel` had been called (after a successfull send). The information
will be propagated back to the user differently depending on how the request(s)
where issued: For requests issued using [megaco:call](`m:megaco#call`), the info
will be delivered in the return value. For requests issued using `megaco:cast`
the info will be delivered via a call to the callback function
[handle_trans_reply](`m:megaco_user#trans_reply`).

In the case of reply, megaco will cancel the reply and information of this will
be returned to the user via a call to the callback function
[handle_trans_ack](`m:megaco_user#trans_ack`).
""".
-callback resend_message(Handle, Msg) -> ok | {cancel, Reason :: term()} | Error when
      Handle :: term(),
      Msg :: iodata(),
      Error :: term().

-optional_callbacks([{send_message,3},{resend_message,2}]).
