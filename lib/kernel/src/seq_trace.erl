%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1998-2025. All Rights Reserved.
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

-module(seq_trace).
-moduledoc """
Sequential tracing of information transfers.

Sequential tracing makes it possible to trace information flows between
processes resulting from one initial transfer of information. Sequential tracing
is independent of the ordinary tracing in Erlang, which is controlled by the
`erlang:trace/3` BIF. For more information about what sequential tracing is and
how it can be used, see section [Sequential Tracing](`m:seq_trace#whatis`).

`seq_trace` provides functions that control all aspects of sequential tracing.
There are functions for activation, deactivation, inspection, and for collection
of the trace output.

## Trace Messages Sent to the System Tracer

The format of the messages is one of the following, depending on if flag
`timestamp` of the trace token is set to `true` or `false`:

```text
{seq_trace, Label, SeqTraceInfo, TimeStamp}
```

or

```text
{seq_trace, Label, SeqTraceInfo}
```

Where:

```erlang
Label = int()
TimeStamp = {Seconds, Milliseconds, Microseconds}
  Seconds = Milliseconds = Microseconds = int()
```

`SeqTraceInfo` can have the following formats:

- **`{send, Serial, From, To, Message}`** - Used when a process `From` with its
  trace token flag `send` set to `true` has sent information. `To` may be a
  process identifier, a registered name on a node represented as
  `{NameAtom, NodeAtom}`, or a node name represented as an atom. `From` may be a
  process identifier or a node name represented as an atom. `Message` contains
  the information passed along in this information transfer. If the transfer is
  done via message passing, it is the actual message.

- **`{'receive', Serial, From, To, Message}`** - Used when a process `To`
  receives information with a trace token that has flag `'receive'` set to
  `true`. `To` may be a process identifier, or a node name represented as an
  atom. `From` may be a process identifier or a node name represented as an
  atom. `Message` contains the information passed along in this information
  transfer. If the transfer is done via message passing, it is the actual
  message.

- **`{print, Serial, From, _, Info}`** - Used when a process `From` has called
  `seq_trace:print(Label, TraceInfo)` and has a trace token with flag `print`
  set to `true`, and `label` set to `Label`.

`Serial` is a tuple `{PreviousSerial, ThisSerial}`, where:

- Integer `PreviousSerial` denotes the serial counter passed in the last
  received information that carried a trace token. If the process is the first
  in a new sequential trace, `PreviousSerial` is set to the value of the process
  internal "trace clock".
- Integer `ThisSerial` is the serial counter that a process sets on outgoing
  messages. It is based on the process internal "trace clock", which is
  incremented by one before it is attached to the trace token in the message.

[](){: #whatis }

## Sequential Tracing

Sequential tracing is a way to trace a sequence of information transfers between
different local or remote processes, where the sequence is initiated by a single
transfer. The typical information transfer is an ordinary Erlang message passed
between two processes, but information is transferred also in other ways. In
short, it works as follows:

Each process has a _trace token_, which can be empty or not empty. When not
empty, the trace token can be seen as the tuple `{Label, Flags, Serial, From}`.
The trace token is passed invisibly when information is passed between
processes. In most cases the information is passed in ordinary messages between
processes, but information is also passed between processes by other means. For
example, by spawning a new process. An information transfer between two
processes is represented by a send event and a receive event regardless of how
it is passed.

To start a sequential trace, the user must explicitly set the trace token in the
process that will send the first information in a sequence.

The trace token of a process is set each time the process receives information.
This is typically when the process matches a message in a receive statement,
according to the trace token carried by the received message, empty or not.

On each Erlang node, a process can be set as the _system tracer_. This process
will receive trace messages each time information with a trace token is sent or
received (if the trace token flag `send` or `'receive'` is set). The system
tracer can then print each trace event, write it to a file, or whatever
suitable.

> #### Note {: .info }
>
> The system tracer only receives those trace events that occur locally within
> the Erlang node. To get the whole picture of a sequential trace, involving
> processes on many Erlang nodes, the output from the system tracer on each
> involved node must be merged (offline).

The following sections describe sequential tracing and its most fundamental
concepts.

## Different Information Transfers

Information flows between processes in a lot of different ways. Not all flows of
information will be covered by sequential tracing. One example is information
passed via ETS tables. Below is a list of information paths that are covered by
sequential tracing:

- **Message Passing** - All ordinary messages passed between Erlang processes.

- **Exit signals** - An exit signal is represented as an `{'EXIT', Pid, Reason}`
  tuple.

- **Process Spawn** - A process spawn is represented as multiple information
  transfers. At least one spawn request and one spawn reply. The actual amount
  of information transfers depends on what type of spawn it is and may also
  change in future implementations. Note that this is more or less an internal
  protocol that you are peeking at. The spawn request will be represented as a
  tuple with the first element containing the atom `spawn_request`, but this is
  more or less all that you can depend on.

> #### Note {: .info }
>
> If you do ordinary `send` or `receive` trace on the system, you will only see
> ordinary message passing, not the other information transfers listed above.

> #### Note {: .info }
>
> When a send event and corresponding receive event do not both correspond to
> ordinary Erlang messages, the `Message` part of the trace messages may not be
> identical. This since all information not necessarily are available when
> generating the trace messages.

## Trace Token

Each process has a current trace token which is "invisibly" passed from the
parent process on creation of the process.

The current token of a process is set in one of the following two ways:

- Explicitly by the process itself, through a call to `seq_trace:set_token/1,2`
- When information is received. This is typically when a received message is
  matched out in a receive expression, but also when information is received in
  other ways.

In both cases, the current token is set. In particular, if the token of a
received message is empty, the current token of the process is set to empty.

A trace token contains a label and a set of flags. Both the label and the flags
are set in both alternatives above.

## Serial

The trace token contains a component called `serial`. It consists of two
integers, `Previous` and `Current`. The purpose is to uniquely identify each
traced event within a trace sequence, as well as to order the messages
chronologically and in the different branches, if any.

The algorithm for updating `Serial` can be described as follows:

Let each process have two counters, `prev_cnt` and `curr_cnt`, both are set to
`0` when a process is created outside of a trace sequence. The counters are
updated at the following occasions:

- _When the process is about to pass along information to another process and
  the trace token is not empty._ This typically occurs when sending a message,
  but also, for example, when spawning another process.

  Let the serial of the trace token be `tprev` and `tcurr`.

  ```text
  curr_cnt := curr_cnt + 1
  tprev := prev_cnt
  tcurr := curr_cnt
  ```

  The trace token with `tprev` and `tcurr` is then passed along with the
  information passed to the other process.

- _When the process calls_ `seq_trace:print(Label, Info)`, `Label` _matches the
  label part of the trace token and the trace token print flag is `true`._

  The algorithm is the same as for send above.

- _When information is received that also contains a non-empty trace token. For
  example, when a message is matched out in a receive expression, or when a new
  process is spawned._

  The process trace token is set to the trace token from the message.

  Let the serial of the trace token be `tprev` and `tcurr`.

  ```text
  if (curr_cnt < tcurr )
     curr_cnt := tcurr
  prev_cnt := tcurr
  ```

`curr_cnt` of a process is incremented each time the process is involved in a
sequential trace. The counter can reach its limit (27 bits) if a process is very
long-lived and is involved in much sequential tracing. If the counter overflows,
the serial for ordering of the trace events cannot be used. To prevent the
counter from overflowing in the middle of a sequential trace, function
`seq_trace:reset_trace/0` can be called to reset `prev_cnt` and `curr_cnt` of
all processes in the Erlang node. This function also sets all trace tokens in
processes and their message queues to empty, and thus stops all ongoing
sequential tracing.

## Performance Considerations

The performance degradation for a system that is enabled for sequential tracing
is negligible as long as no tracing is activated. When tracing is activated,
there is an extra cost for each traced message, but all other messages are
unaffected.

## Ports

Sequential tracing is not performed across ports.

If the user for some reason wants to pass the trace token to a port, this must
be done manually in the code of the port controlling process. The port
controlling processes have to check the appropriate sequential trace settings
(as obtained from `seq_trace:get_token/1`) and include trace information in the
message data sent to their respective ports.

Similarly, for messages received from a port, a port controller has to retrieve
trace-specific information, and set appropriate sequential trace flags through
calls to `seq_trace:set_token/2`.

## Distribution

Sequential tracing between nodes is performed transparently. This applies to
C-nodes built with `Erl_Interface` too. A C-node built with `Erl_Interface` only
maintains one trace token, which means that the C-node appears as one process
from the sequential tracing point of view.

## Example of Use

This example gives a rough idea of how the new primitives can be used and what
kind of output it produces.

Assume that you have an initiating process with `Pid == <0.30.0>` like this:

```erlang
-module(seqex).
-compile(export_all).

loop(Port) ->
    receive
        {Port,Message} ->
            seq_trace:set_token(label,17),
            seq_trace:set_token('receive',true),
            seq_trace:set_token(print,true),
            seq_trace:print(17,"**** Trace Started ****"),
            call_server ! {self(),the_message};
        {ack,Ack} ->
            ok
    end,
    loop(Port).
```

And a registered process `call_server` with `Pid == <0.31.0>` like this:

```erlang
loop() ->
    receive
        {PortController,Message} ->
            Ack = {received, Message},
            seq_trace:print(17,"We are here now"),
            PortController ! {ack,Ack}
    end,
    loop().
```

A possible output from the system's `sequential_tracer` can be like this:

```erlang
17:<0.30.0> Info {0,1} WITH
"**** Trace Started ****"
17:<0.31.0> Received {0,2} FROM <0.30.0> WITH
{<0.30.0>,the_message}
17:<0.31.0> Info {2,3} WITH
"We are here now"
17:<0.30.0> Received {2,4} FROM <0.31.0> WITH
{ack,{received,the_message}}
```

The implementation of a system tracer process that produces this printout can
look like this:

```erlang
tracer() ->
    receive
        {seq_trace,Label,TraceInfo} ->
           print_trace(Label,TraceInfo,false);
        {seq_trace,Label,TraceInfo,Ts} ->
           print_trace(Label,TraceInfo,Ts);
        _Other -> ignore
    end,
    tracer().

print_trace(Label,TraceInfo,false) ->
    io:format("~p:",[Label]),
    print_trace(TraceInfo);
print_trace(Label,TraceInfo,Ts) ->
    io:format("~p ~p:",[Label,Ts]),
    print_trace(TraceInfo).

print_trace({print,Serial,From,_,Info}) ->
    io:format("~p Info ~p WITH~n~p~n", [From,Serial,Info]);
print_trace({'receive',Serial,From,To,Message}) ->
    io:format("~p Received ~p FROM ~p WITH~n~p~n",
              [To,Serial,From,Message]);
print_trace({send,Serial,From,To,Message}) ->
    io:format("~p Sent ~p TO ~p WITH~n~p~n",
              [From,Serial,To,Message]).
```

The code that creates a process that runs this tracer function and sets that
process as the system tracer can look like this:

```erlang
start() ->
    Pid = spawn(?MODULE,tracer,[]),
    seq_trace:set_system_tracer(Pid), % set Pid as the system tracer
    ok.
```

With a function like `test/0`, the whole example can be started:

```erlang
test() ->
    P = spawn(?MODULE, loop, [port]),
    register(call_server, spawn(?MODULE, loop, [])),
    start(),
    P ! {port,message}.
```
""".

%% Don't forget to update seq_trace_SUITE after changing these.
-define(SEQ_TRACE_SEND, 1).                     %(1 << 0)
-define(SEQ_TRACE_RECEIVE, 2).                  %(1 << 1)
-define(SEQ_TRACE_PRINT, 4).                    %(1 << 2)
-define(SEQ_TRACE_NOW_TIMESTAMP, 8).            %(1 << 3)
-define(SEQ_TRACE_STRICT_MON_TIMESTAMP, 16).    %(1 << 4)
-define(SEQ_TRACE_MON_TIMESTAMP, 32).           %(1 << 5)

-export([set_token/1,
	 set_token/2,
	 get_token/0,
	 get_token/1,
	 print/1,
	 print/2,
	 reset_trace/0,
	 set_system_tracer/1,
	 get_system_tracer/0]).

%%---------------------------------------------------------------------------

-type flag()       :: 'send' | 'receive' | 'print' | 'timestamp' |
                      'monotonic_timestamp' | 'strict_monotonic_timestamp'.
-type component()  :: 'label' | 'serial' | flag().
-type value()      :: (Label :: term())
                    | {Previous :: non_neg_integer(),
                       Current :: non_neg_integer()}
                    | (Bool :: boolean()).

%%---------------------------------------------------------------------------

-doc "An opaque term (a tuple) representing a trace token.".
-type token() :: {integer(), boolean(), _, _, _}.
-doc """
Sets the trace token for the calling process to `Token`. If `Token == []` then
tracing is disabled, otherwise `Token` should be an Erlang term returned from
`get_token/0` or [`set_token/1`](`set_token/1`). [`set_token/1`](`set_token/1`)
can be used to temporarily exclude message passing from the trace by setting the
trace token to empty like this:

```erlang
OldToken = seq_trace:set_token([]), % set to empty and save
                                    % old value
% do something that should not be part of the trace
io:format("Exclude the signalling caused by this~n"),
seq_trace:set_token(OldToken), % activate the trace token again
...
```

Returns the previous value of the trace token.
""".
-spec set_token(Token) -> PreviousToken | 'ok' when
      Token :: [] | token(),
      PreviousToken :: [] | token().

set_token([]) ->
    erlang:seq_trace(sequential_trace_token,[]);
set_token({Flags,Label,Serial,_From,Lastcnt}) ->
    F = decode_flags(Flags),
    set_token2([{label,Label},{serial,{Lastcnt, Serial}} | F]).

-doc """
Sets the individual `Component` of the trace token to `Val`. Returns the
previous value of the component.

- **[`set_token(label, Label)`](`set_token/2`)** - The `label` component is a
  term which identifies all events belonging to the same sequential trace. If
  several sequential traces can be active simultaneously, `label` is used to
  identify the separate traces. Default is 0.

  > #### Warning {: .warning }
  >
  > Labels were restricted to small signed integers (28 bits) prior to OTP 21.
  > The trace token will be silently dropped if it crosses over to a node that
  > does not support the label.

- **[`set_token(serial, SerialValue)`](`set_token/2`)** -
  `SerialValue = {Previous, Current}`. The `serial` component contains counters
  which enables the traced messages to be sorted, should never be set explicitly
  by the user as these counters are updated automatically. Default is `{0, 0}`.

- **[`set_token(send, Bool)`](`set_token/2`)** - A trace token flag
  (`true | false`) which enables/disables tracing on information sending.
  Default is `false`.

- **[`set_token('receive', Bool)`](`set_token/2`)** - A trace token flag
  (`true | false`) which enables/disables tracing on information reception.
  Default is `false`.

- **[`set_token(print, Bool)`](`set_token/2`)** - A trace token flag
  (`true | false`) which enables/disables tracing on explicit calls to
  `seq_trace:print/1`. Default is `false`.

- **[`set_token(timestamp, Bool)`](`set_token/2`)** - A trace token flag
  (`true | false`) which enables/disables a timestamp to be generated for each
  traced event. Default is `false`.

- **[`set_token(strict_monotonic_timestamp, Bool)`](`set_token/2`)** - A trace
  token flag (`true | false`) which enables/disables a strict monotonic
  timestamp to be generated for each traced event. Default is `false`.
  Timestamps will consist of
  [Erlang monotonic time](`e:erts:time_correction.md#erlang-monotonic-time`) and
  a monotonically increasing integer. The time-stamp has the same format and
  value as produced by
  `{erlang:monotonic_time(nanosecond), erlang:unique_integer([monotonic])}`.

- **[`set_token(monotonic_timestamp, Bool)`](`set_token/2`)** - A trace token
  flag (`true | false`) which enables/disables a strict monotonic timestamp to
  be generated for each traced event. Default is `false`. Timestamps will use
  [Erlang monotonic time](`e:erts:time_correction.md#erlang-monotonic-time`).
  The time-stamp has the same format and value as produced by
  `erlang:monotonic_time(nanosecond)`.

If multiple timestamp flags are passed, `timestamp` has precedence over
`strict_monotonic_timestamp` which in turn has precedence over
`monotonic_timestamp`. All timestamp flags are remembered, so if two are passed
and the one with highest precedence later is disabled the other one will become
active.
""".
-spec set_token(Component, Val) -> OldVal when
      Component :: component(),
      Val :: value(),
      OldVal :: value().

set_token(Type, Val) ->
    erlang:seq_trace(Type, Val).

-doc """
Returns the value of the trace token for the calling process. If `[]` is
returned, it means that tracing is not active. Any other value returned is the
value of an active trace token. The value returned can be used as input to the
[`set_token/1`](`set_token/1`) function.
""".
-spec get_token() -> [] | token().

get_token() ->
    element(2,process_info(self(),sequential_trace_token)).

-doc """
Returns the value of the trace token component `Component`. See `set_token/2`
for possible values of `Component` and `Val`.
""".
-spec get_token(Component) -> [] | {Component, Val} when
      Component :: component(),
      Val :: value().
get_token(Type) ->
    erlang:seq_trace_info(Type).

-doc """
Puts the Erlang term `TraceInfo` into the sequential trace output if the calling
process currently is executing within a sequential trace and the `print` flag of
the trace token is set.
""".
-spec print(TraceInfo) -> 'ok' when
      TraceInfo :: term().

print(Term) ->
    erlang:seq_trace_print(Term),
    ok.

-doc """
Same as [`print/1`](`print/1`) with the additional condition that `TraceInfo` is
output only if `Label` is equal to the label component of the trace token.
""".
-spec print(Label, TraceInfo) -> 'ok' when
      Label :: integer(),
      TraceInfo :: term().

print(Label, Term) when is_atom(Label) ->
    erlang:error(badarg, [Label, Term]);
print(Label, Term) ->
    erlang:seq_trace_print(Label, Term),
    ok.

-doc """
Sets the trace token to empty for all processes on the local node. The process
internal counters used to create the serial of the trace token is set to 0. The
trace token is set to empty for all messages in message queues. Together this
will effectively stop all ongoing sequential tracing in the local node.
""".
-spec reset_trace() -> 'true'.

reset_trace() ->
    erlang:system_flag(reset_seq_trace, true).

%% reset_trace(Pid) -> % this might be a useful function too

-type tracer() :: (Pid :: pid()) | port() |
                  (TracerModule :: {module(), term()}) |
                  'false'.

-doc """
Sets the system tracer. The system tracer can be either a process, port or
[tracer module](`m:erl_tracer`) denoted by `Tracer`. Returns the previous value
(which can be `false` if no system tracer is active).

Failure: `{badarg, Info}}` if `Pid` is not an existing local pid.
""".
-spec set_system_tracer(Tracer) -> OldTracer when
      Tracer :: tracer(),
      OldTracer :: tracer().

set_system_tracer({Module, State} = Tracer) ->
    case erlang:module_loaded(Module) of
        false ->
            Module:enabled(trace_status, erlang:self(), State);
        true ->
            ok
    end,
    erlang:system_flag(sequential_tracer, Tracer);
set_system_tracer(Tracer) ->
    erlang:system_flag(sequential_tracer, Tracer).

-doc """
Returns the pid, port identifier or tracer module of the current system tracer
or `false` if no system tracer is activated.
""".
-spec get_system_tracer() -> Tracer when
      Tracer :: tracer().

get_system_tracer() ->
    element(2, erlang:system_info(sequential_tracer)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal help functions 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_token2([{Type,Val}|T]) ->
    _ = erlang:seq_trace(Type, Val),
    set_token2(T);
set_token2([]) ->
    ok.

decode_flags(Flags) ->
    Print = (Flags band ?SEQ_TRACE_PRINT) > 0,
    Send = (Flags band ?SEQ_TRACE_SEND) > 0,
    Rec = (Flags band ?SEQ_TRACE_RECEIVE) > 0,
    NowTs = (Flags band ?SEQ_TRACE_NOW_TIMESTAMP) > 0,
    StrictMonTs = (Flags band ?SEQ_TRACE_STRICT_MON_TIMESTAMP) > 0,
    MonTs = (Flags band ?SEQ_TRACE_MON_TIMESTAMP) > 0,
    [{print,Print},{send,Send},{'receive',Rec},{timestamp,NowTs},
     {strict_monotonic_timestamp, StrictMonTs},
     {monotonic_timestamp, MonTs}].
