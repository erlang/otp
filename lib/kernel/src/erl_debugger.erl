%%
%% Copyright (c) Meta Platforms, Inc. and affiliates.
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
-module(erl_debugger).
-moduledoc """
Erlang debugger support (EXPERIMENTAL).

This module exposes low-level functionality for the implementation
of a debugger for Erlang.

Any local process can register itself as the debugger for a node, but
there can be at most one such process registered at any given time.
Using the BIFs in this module, a debugger can:

  - set breakpoints;
  - inspect internal process state, such registers, stack-frames;
  - get notified on debugger events such as a process hitting a breakpoint;
  - resume processes paused on breakpoints

At the moment, the API is highly experimental; so don't depend on it, or otherwise
expect frequent incompatible changes.
""".

%% Public API
-export([supported/0]).
-export([instrumentations/0, toggle_instrumentations/1]).
-export([register/1, unregister/2, whereis/0]).
-export([breakpoint/3]).
-export([stack_frames/2, peek_stack_frame_slot/4]).
-export([xregs_count/1, peek_xreg/3]).


%% Types

-export_type([session/0, event_message/0, event/0]).

-doc """
Debugger session identifier. It is attached to all debugger events
""".
-opaque session() :: integer().

-doc """
The debugger process will receive debugger-event messages, wrapped in
an envelope of this type.
""".
-type event_message() ::
    {debugger_event, session(), event()}.

-doc """
Debugger-events:

  * `{breakpoint, Pid, {M,F,A}, Line, Resume}`: process Pid hit a breakpoint
    on module `M`, at the given `Line`. The debugger can resume the process
    by executing `Resume()`.
""".
-type event() ::
    {breakpoint, pid(), mfa(), Line :: pos_integer(), Resume :: fun(() -> ok)} .


-export_type([stack_frame/0, stack_frame_fun/0, stack_frame_info/0, stack_frame_slot/0, reg_val/0]).

-doc """
A stack-frame, including the value of each slot.
""".
-type stack_frame() ::
    {FrameNo :: non_neg_integer(), stack_frame_fun(), stack_frame_info()}.

-doc """
What is running in each stack frame, including special VM frames.
""".
-type stack_frame_fun() ::
    #{function := mfa(), line := pos_integer() | undefined}
    | '<terminate process>'
    | '<continue terminate process>'
    | '<terminate process normally>'
    | '<breakpoint>'
    | 'unknown function'.

-doc """
Extra information about a stack-frame.

  - `slots`: Y-registers (in order `[Y0,...Yk])`, followed by exception-handlers.
  - `code`: Memory address of the next instruction to execute in this frame.
""".
-type stack_frame_info() :: #{
    slots := [stack_frame_slot()],
    code := pos_integer()
}.

-doc """
The contents of a stack frame slot can be a Y-register
or an exception handler.
""".
-type stack_frame_slot() ::
    reg_val() | {'catch', stack_frame_fun()}.

-doc """
The value of a X/Y register, if it fits within the requested
size. If it's too large, then size of the term.
""".
-type reg_val() ::
    {value, term()} | {too_large, Size :: pos_integer()}.

-export_type([instrumentation/0]).

-doc """
Debugging instrumentations that can be applied on module loading

  - `line_breakpoint`: Allows setting breakpoints at the beginning
     of executable lines
""".
-type instrumentation() :: line_breakpoint.

%% Capabilities

-doc """
Returns `true` if the emulator supports debugging.

The debugger can only be used if the `+D` argument was passed
to the emulator on start-up.
""".
-spec supported() -> boolean().
supported() ->
    erlang:nif_error(undef).

-doc """
Returns the instrumentations that will be applied on module loading.

Modules that are already loaded may have had a different set of
instrumentations applied.
""".
-spec instrumentations() -> #{instrumentation() => boolean()}.
instrumentations() ->
    erlang:nif_error(undef).

-doc """
Updates the instrumentations that will be applied on module loading.

Modules that are already loaded will keep the instrumentation they
had at their time of loading.
""".
-spec toggle_instrumentations(Toggle) -> ok when
    Toggle :: #{instrumentation() => boolean()}.
toggle_instrumentations(_) ->
    erlang:nif_error(undef).

%% Registration

-doc """
Register the given process as the debugger.

If the registration succeeds, it returns `{ok, Session}`, where the `Session`
is a token that will be included on every message sent to the process.

Returns `{error, already_exists}` some process is currently registered as debugger.
""".
-spec register(pid()) -> {ok, session()} | {error, already_exists}.
register(_) ->
    erlang:nif_error(undef).

-doc """
Unregisters the given process.

The session given on registration needs to be provided.
""".
-spec unregister(pid(), session()) -> ok.
unregister(_, _) ->
    erlang:nif_error(undef).

-doc """
Returns the pid of the registered debugger.
""".
-spec whereis() -> undefined | pid().
whereis() ->
    erlang:nif_error(undef).


%% Breakpoints

-doc """
Set or clear a breakpoints on the given Module/Line.

When a process hits a breakpoint, it will pause and a `breakpoint`
message is sent to the registered debugger.

Returns `ok` on success. It can fail with the following reasons:
  - `{badkey, Module}`: The given module is not loaded
  - `{unsupported, Module}`: the module was loaded without support
    for line breakpoints..
  - `{badkey, Line}`: The line is not relevant; it could refer to a comment,
     not exist in the module source, etc.
  - `{unsupported, Line}`: It is not possible to set a breakpoint in
    in the given line; e.g. if it refers to a function head.
""".
-spec breakpoint(Module, Line, Flag) -> ok | {error, Reason} when
    Module :: module(),
    Line :: pos_integer(),
    Flag :: boolean(),
    Reason :: {unsupported, Module | Line} | {badkey, Module | Line}.
breakpoint(_, _, _) ->
    erlang:nif_error(undef).


%% Stack frames

-doc """
Get the all the stack-frames for a suspended process.

If the given process is not in a suspended state, returns `running`.
Otherwise, list of [stack frames](stack_frame/1), which includes
the content of each slot. For slots containing terms, `MaxTermSize`
controls the maximum size of values that are allowed to be returned
(to avoid accidentally blowing the heap of the caller).
""".
-spec stack_frames(pid(), MaxTermSize :: non_neg_integer()) -> running | [stack_frame()].
stack_frames(_, _) ->
    erlang:nif_error(undef).

-doc """
Get the value of a slot in a suspended process stack-frame.

Returns `running` if the process is not suspended, and `undefined`
if the frame or the slot don't exist for that process.
Otherwise returns the slot, that can be a term, if its size is less
than `MaxTermSize`, or a an exeption handler.
""".
-spec peek_stack_frame_slot(pid(), FrameNo, Slot, MaxSize) ->
    running | undefined | stack_frame_slot() when
    FrameNo :: pos_integer(),
    Slot :: non_neg_integer(),
    MaxSize :: non_neg_integer().
peek_stack_frame_slot(_, _, _, _) ->
    erlang:nif_error(undef).

%% Process registers

-doc """
Get the number of X-registers currently in use by a suspended process.
""".
-spec xregs_count(pid()) -> running | non_neg_integer().
xregs_count(_) ->
    erlang:nif_error(undef).

-doc """
Get the value of an X-register for a suspended process.
""".
-spec peek_xreg(pid(), Reg, MaxSize) ->
    running | undefined | reg_val() when
    Reg :: non_neg_integer(),
    MaxSize :: non_neg_integer().
peek_xreg(_, _, _) ->
    erlang:nif_error(undef).
