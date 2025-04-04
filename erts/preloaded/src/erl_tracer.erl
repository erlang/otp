%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2015-2025. All Rights Reserved.
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

-module(erl_tracer).
-moduledoc """
Erlang tracer behavior.

This behavior module implements the back end of the Erlang tracing system. The
functions in this module are called whenever a trace probe is triggered. Both
the `enabled` and `trace` functions are called in the context of the entity that
triggered the trace probe. This means that the overhead by having the tracing
enabled is greatly effected by how much time is spent in these functions. So, do
as little work as possible in these functions.

> #### Note {: .info }
>
> All functions in this behavior must be implemented as NIFs. This limitation
> can be removed in a future releases. An
> [example tracer module NIF](`m:erl_tracer#example`) implementation is provided
> below.

> #### Warning {: .warning }
>
> Do not send messages or issue port commands to the `Tracee` in any of the
> callbacks. This is not allowed and can cause all sorts of strange behavior,
> including, but not limited to, infinite recursions.

[](){: #example }

## Erl Tracer Module Example

In this example, a tracer module with a NIF back end sends a message for each
`send` trace tag containing only the sender and receiver. Using this tracer
module, a much more lightweight message tracer is used, which only records who
sent messages to who.

The following is an example session using it on Linux:

```erlang
$ gcc -I erts-8.0/include/ -fPIC -shared -o erl_msg_tracer.so erl_msg_tracer.c
$ erl
Erlang/OTP 19 [DEVELOPMENT] [erts-8.0] [source-ed2b56b] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.0  (abort with ^G)
1> c(erl_msg_tracer), erl_msg_tracer:load().
ok
2> Tracer = spawn(fun F() -> receive M -> io:format("~p~n",[M]), F() end end).
<0.37.0>
3> erlang:trace(new, true, [send,{tracer, erl_msg_tracer, Tracer}]).
0
{trace,<0.39.0>,<0.27.0>}
4> {ok, D} = file:open("/tmp/tmp.data",[write]).
{trace,#Port<0.486>,<0.40.0>}
{trace,<0.40.0>,<0.21.0>}
{trace,#Port<0.487>,<0.4.0>}
{trace,#Port<0.488>,<0.4.0>}
{trace,#Port<0.489>,<0.4.0>}
{trace,#Port<0.490>,<0.4.0>}
{ok,<0.40.0>}
{trace,<0.41.0>,<0.27.0>}
5>
```

`erl_msg_tracer.erl`:

```erlang
-module(erl_msg_tracer).

-export([enabled/3, trace/5, load/0]).

load() ->
    erlang:load_nif("erl_msg_tracer", []).

enabled(_, _, _) ->
    error.

trace(_, _, _, _, _) ->
    error.
```

`erl_msg_tracer.c`:

```c
#include <erl_nif.h>

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

/* The NIFs: */
static ERL_NIF_TERM enabled(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"enabled", 3, enabled},
    {"trace", 5, trace}
};

ERL_NIF_INIT(erl_msg_tracer, nif_funcs, load, NULL, upgrade, unload)

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    *priv_data = NULL;
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{

}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
    if (*old_priv_data != NULL || *priv_data != NULL) {
	return -1; /* Don't know how to do that */
    }
    if (load(env, priv_data, load_info)) {
	return -1;
    }
    return 0;
}

/*
 * argv[0]: TraceTag
 * argv[1]: TracerState
 * argv[2]: Tracee
 */
static ERL_NIF_TERM enabled(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPid to_pid;
    if (enif_get_local_pid(env, argv[1], &to_pid))
        if (!enif_is_process_alive(env, &to_pid))
            if (enif_is_identical(enif_make_atom(env, "trace_status"), argv[0]))
                /* tracer is dead so we should remove this tracepoint */
                return enif_make_atom(env, "remove");
            else
                return enif_make_atom(env, "discard");

    /* Only generate trace for when tracer != tracee */
    if (enif_is_identical(argv[1], argv[2]))
        return enif_make_atom(env, "discard");

    /* Only trigger trace messages on 'send' */
    if (enif_is_identical(enif_make_atom(env, "send"), argv[0]))
        return enif_make_atom(env, "trace");

    /* Have to answer trace_status */
    if (enif_is_identical(enif_make_atom(env, "trace_status"), argv[0]))
        return enif_make_atom(env, "trace");

    return enif_make_atom(env, "discard");
}

/*
 * argv[0]: TraceTag, should only be 'send'
 * argv[1]: TracerState, process to send {Tracee, Recipient} to
 * argv[2]: Tracee
 * argv[3]: Message
 * argv[4]: Options, map containing Recipient
 */
static ERL_NIF_TERM trace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPid to_pid;
    ERL_NIF_TERM recipient, msg;

    if (enif_get_local_pid(env, argv[1], &to_pid)) {
      if (enif_get_map_value(env, argv[4], enif_make_atom(env, "extra"), &recipient)) {
        msg = enif_make_tuple3(env, enif_make_atom(env, "trace"), argv[2], recipient);
        enif_send(env, &to_pid, NULL, msg);
      }
    }

    return enif_make_atom(env, "ok");
}
```
""".
-moduledoc(#{since => "OTP 19.0"}).

-export([enabled/3, trace/5, on_load/0]).

-nifs([enabled/3, trace/5]).

-doc "The process or port that the trace belongs to.".
-type tracee() :: port() | pid() | undefined.

-type trace_tag_running_ports() :: in | out | in_exiting | out_exiting | out_exited.
-type trace_tag_running_procs() :: in | out | in_exiting | out_exiting | out_exited.
-type trace_tag_send() :: send | send_to_non_existing_process.
-type trace_tag_receive() :: 'receive'.
-type trace_tag_call() :: call | return_to | return_from | exception_from.
-type trace_tag_procs() :: spawn | spawned | exit | link | unlink
                         | getting_linked | getting_unlinked
                         | register | unregister.
-type trace_tag_ports() :: open | closed  | link | unlink
                         | getting_linked | getting_unlinked.
-type trace_tag_gc() :: gc_minor_start | gc_minor_end
                      | gc_major_start | gc_major_end.

-doc """
The different trace tags that the tracer is called with.

Each trace tag is described in detail in [`Module:trace/5`](`c:trace/5`).
""".
-type trace_tag() :: trace_tag_send()
                   | trace_tag_receive()
                   | trace_tag_call()
                   | trace_tag_procs()
                   | trace_tag_ports()
                   | trace_tag_running_procs()
                   | trace_tag_running_ports()
                   | trace_tag_gc().

-doc """
The options for the tracee:

- **`timestamp`** - If set the tracer has been requested to include a time
  stamp.

- **`extra`** - If set the tracepoint has included additional data about the
  trace event. What the additional data is depends on which `TraceTag` has been
  triggered. The `extra` trace data corresponds to the fifth element in the
  trace tuples described in [trace:process/4](`m:trace#process_trace_messages`).

- **`match_spec_result`** - If set the tracer has been requested to include the
  output of a match specification that was run.

- **`scheduler_id`** - If set the scheduler id is to be included by the tracer.
""".
-type trace_opts() :: #{ extra => term(), match_spec_result => term(),
                         scheduler_id => non_neg_integer(),
                         timestamp => timestamp | cpu_timestamp |
                                      monotonic | strict_monotonic }.
-doc """
The state specified when calling
[`erlang:trace(PidPortSpec,true,[{tracer,Module,TracerState}])`](`erlang:trace/3`).
The tracer state is an immutable value that is passed to `erl_tracer` callbacks
and is to contain all the data that is needed to generate the trace event.
""".
-type tracer_state() :: term().

-doc """
This callback is called whenever a tracepoint is triggered.

It allows the tracer to decide whether a trace is to be generated or not. This
check is made as early as possible to limit the amount of overhead associated
with tracing. If `trace` is returned, the necessary trace data is created and
the trace callback of the tracer is called. If `discard` is returned, this trace
call is discarded and no call to trace is done.

`trace_status` is a special type of `TraceTag`, which is used to check if the
tracer is still to be active. It is called in multiple scenarios, but most
significantly it is used when tracing is started using this tracer. If `remove`
is returned when the `trace_status` is checked, the tracer is removed from the
tracee.

This function can be called multiple times per tracepoint, so it is important
that it is both fast and without side effects.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback enabled(TraceTag, TracerState, Tracee) -> Result when
      TraceTag :: trace_tag() | trace_status,
      TracerState :: term(),
      Tracee :: tracee(),
      Result :: trace | discard | remove.
-doc """
This callback is called whenever a tracepoint with trace flag
[`call | return_to`](`erlang:trace/3`) is triggered.

If [`enabled_call/3`](`c:enabled_call/3`) is undefined,
[`Module:enabled/3`](`c:enabled/3`) is called instead.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback enabled_call(TraceTag, TracerState, Tracee) -> Result when
      TraceTag :: trace_tag_call(),
      TracerState :: term(),
      Tracee :: tracee(),
      Result :: trace | discard | remove.
-doc """
This callback is called whenever a tracepoint with trace flag
[`garbage_collection`](`erlang:trace/3`) is triggered.

If [`enabled_garbage_collection/3`](`c:enabled_garbage_collection/3`) is
undefined, [`Module:enabled/3`](`c:enabled/3`) is called instead.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback enabled_garbage_collection(TraceTag, TracerState, Tracee) -> Result when
      TraceTag :: trace_tag_gc(),
      TracerState :: term(),
      Tracee :: tracee(),
      Result :: trace | discard | remove.
-doc """
This callback is called whenever a tracepoint with trace flag
[`ports`](`erlang:trace/3`) is triggered.

If [`enabled_ports/3`](`c:enabled_ports/3`) is undefined,
[`Module:enabled/3`](`c:enabled/3`) is called instead.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback enabled_ports(TraceTag, TracerState, Tracee) -> Result when
      TraceTag :: trace_tag_ports(),
      TracerState :: term(),
      Tracee :: tracee(),
      Result :: trace | discard | remove.
-doc """
This callback is called whenever a tracepoint with trace flag
[`procs`](`erlang:trace/3`) is triggered.

If [`enabled_procs/3`](`c:enabled_procs/3`) is undefined,
[`Module:enabled/3`](`c:enabled/3`) is called instead.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback enabled_procs(TraceTag, TracerState, Tracee) -> Result when
      TraceTag :: trace_tag_procs(),
      TracerState :: term(),
      Tracee :: tracee(),
      Result :: trace | discard | remove.
-doc """
This callback is called whenever a tracepoint with trace flag
[`'receive'`](`erlang:trace/3`) is triggered.

If [`enabled_receive/3`](`c:enabled_receive/3`) is undefined,
[`Module:enabled/3`](`c:enabled/3`) is called instead.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback enabled_receive(TraceTag, TracerState, Tracee) -> Result when
      TraceTag :: trace_tag_receive(),
      TracerState :: term(),
      Tracee :: tracee(),
      Result :: trace | discard | remove.
-doc """
This callback is called whenever a tracepoint with trace flag
[`running_ports`](`erlang:trace/3`) is triggered.

If [`enabled_running_ports/3`](`c:enabled_running_ports/3`) is undefined,
[`Module:enabled/3`](`c:enabled/3`) is called instead.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback enabled_running_ports(TraceTag, TracerState, Tracee) -> Result when
      TraceTag :: trace_tag_running_ports(),
      TracerState :: term(),
      Tracee :: tracee(),
      Result :: trace | discard | remove.
-doc """
This callback is called whenever a tracepoint with trace flag
[`running_procs | running`](`erlang:trace/3`) is triggered.

If [`enabled_running_procs/3`](`c:enabled_running_procs/3`) is undefined,
[`Module:enabled/3`](`c:enabled/3`) is called instead.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback enabled_running_procs(TraceTag, TracerState, Tracee) -> Result when
      TraceTag :: trace_tag_running_procs(),
      TracerState :: term(),
      Tracee :: tracee(),
      Result :: trace | discard | remove.
-doc """
This callback is called whenever a tracepoint with trace flag
[`send`](`erlang:trace/3`) is triggered.

If [`enabled_send/3`](`c:enabled_send/3`) is undefined,
[`Module:enabled/3`](`c:enabled/3`) is called instead.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback enabled_send(TraceTag, TracerState, Tracee) -> Result when
      TraceTag :: trace_tag_send(),
      TracerState :: term(),
      Tracee :: tracee(),
      Result :: trace | discard | remove.

-doc """
This callback is called when a tracepoint is triggered and the
[`Module:enabled/3`](`c:enabled/3`) callback returned `trace`.

In it any side effects needed by the tracer are to be done. The tracepoint
payload is located in the `TraceTerm`. The content of the `TraceTerm` depends on
which `TraceTag` is triggered. `TraceTerm` corresponds to the fourth element in
the trace tuples described in [`trace:process/4`](`m:trace#process_trace_messages`).

If the trace tuple has five elements, the fifth element will be sent as the
`extra` value in the `Opts` maps.

The `TraceTag` `seq_trace` is handled slightly differently. There is no `Tracee`
for `seq_trace`, instead the `Label` associated with the `seq_trace` event is
specified.

For more information on what `Label` and `SeqTraceInfo` can be, see
`m:seq_trace`.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback trace(seq_trace, TracerState, Label, SeqTraceInfo, Opts) -> Result when
      TracerState :: term(),
      Label :: term(),
      SeqTraceInfo :: term(),
      Opts :: trace_opts(),
      Result :: ok;
                (TraceTag, TracerState, Tracee, TraceTerm, Opts) -> Result when
      TraceTag :: trace_tag(),
      TracerState :: term(),
      Tracee :: tracee(),
      TraceTerm :: term(),
      Opts :: trace_opts(),
      Result :: ok.
-doc """
This callback is called when a tracepoint is triggered and the
[`Module:enabled_call/3`](`c:enabled_call/3`) callback returned `trace`.

If [`trace_call/5`](`c:trace_call/5`) is undefined,
[`Module:trace/5`](`c:trace/5`) is called instead.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback trace_call(TraceTag, TracerState, Tracee, TraceTerm, Opts) -> Result when
      TraceTag :: trace_tag_call(),
      TracerState :: term(),
      Tracee :: tracee(),
      TraceTerm :: term(),
      Opts :: trace_opts(),
      Result :: ok.
-doc """
This callback is called when a tracepoint is triggered and the
[`Module:enabled_garbage_collection/3`](`c:enabled_garbage_collection/3`)
callback returned `trace`.

If [`trace_garbage_collection/5`](`c:trace_garbage_collection/5`) is undefined,
[`Module:trace/5`](`c:trace/5`) is called instead.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback trace_garbage_collection(TraceTag, TracerState, Tracee, TraceTerm, Opts) -> Result when
      TraceTag :: trace_tag_gc(),
      TracerState :: term(),
      Tracee :: tracee(),
      TraceTerm :: term(),
      Opts :: trace_opts(),
      Result :: ok.
-doc """
This callback is called when a tracepoint is triggered and the
[`Module:enabled_ports/3`](`c:enabled_ports/3`) callback returned `trace`.

If [`trace_ports/5`](`c:trace_ports/5`) is undefined,
[`Module:trace/5`](`c:trace/5`) is called instead.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback trace_ports(TraceTag, TracerState, Tracee, TraceTerm, Opts) -> Result when
      TraceTag :: trace_tag(),
      TracerState :: term(),
      Tracee :: tracee(),
      TraceTerm :: term(),
      Opts :: trace_opts(),
      Result :: ok.
-doc """
This callback is called when a tracepoint is triggered and the
[`Module:enabled_procs/3`](`c:enabled_procs/3`) callback returned `trace`.

If [`trace_procs/5`](`c:trace_procs/5`) is undefined,
[`Module:trace/5`](`c:trace/5`) is called instead.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback trace_procs(TraceTag, TracerState, Tracee, TraceTerm, Opts) -> Result when
      TraceTag :: trace_tag(),
      TracerState :: term(),
      Tracee :: tracee(),
      TraceTerm :: term(),
      Opts :: trace_opts(),
      Result :: ok.
-doc """
This callback is called when a tracepoint is triggered and the
[`Module:enabled_receive/3`](`c:enabled_receive/3`) callback returned `trace`.

If [`trace_receive/5`](`c:trace_receive/5`) is undefined,
[`Module:trace/5`](`c:trace/5`) is called instead.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback trace_receive(TraceTag, TracerState, Tracee, TraceTerm, Opts) -> Result when
      TraceTag :: trace_tag_receive(),
      TracerState :: term(),
      Tracee :: tracee(),
      TraceTerm :: term(),
      Opts :: trace_opts(),
      Result :: ok.
-doc """
This callback is called when a tracepoint is triggered and the
[`Module:enabled_running_ports/3`](`c:enabled_running_ports/3`) callback
returned `trace`.

If [`trace_running_ports/5`](`c:trace_running_ports/5`) is undefined,
[`Module:trace/5`](`c:trace/5`) is called instead.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback trace_running_ports(TraceTag, TracerState, Tracee, TraceTerm, Opts) -> Result when
      TraceTag :: trace_tag_running_ports(),
      TracerState :: term(),
      Tracee :: tracee(),
      TraceTerm :: term(),
      Opts :: trace_opts(),
      Result :: ok.
-doc """
This callback is called when a tracepoint is triggered and the
[`Module:enabled_running_procs/3`](`c:enabled_running_procs/3`) callback
returned `trace`.

If [`trace_running_procs/5`](`c:trace_running_procs/5`) is undefined,
[`Module:trace/5`](`c:trace/5`) is called instead.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback trace_running_procs(TraceTag, TracerState, Tracee, TraceTerm, Opts) -> Result when
      TraceTag :: trace_tag_running_procs(),
      TracerState :: term(),
      Tracee :: tracee(),
      TraceTerm :: term(),
      Opts :: trace_opts(),
      Result :: ok.
-doc """
This callback is called when a tracepoint is triggered and the
[`Module:enabled_send/3`](`c:enabled_send/3`) callback returned `trace`.

If [`trace_send/5`](`c:trace_send/5`) is undefined,
[`Module:trace/5`](`c:trace/5`) is called instead.
""".
-doc(#{since => <<"OTP 19.0">>}).
-callback trace_send(TraceTag, TracerState, Tracee, TraceTerm, Opts) -> Result when
      TraceTag :: trace_tag_send(),
      TracerState :: term(),
      Tracee :: tracee(),
      TraceTerm :: term(),
      Opts :: trace_opts(),
      Result :: ok.

-optional_callbacks(
   [enabled_call/3,
    enabled_garbage_collection/3,
    enabled_ports/3,
    enabled_procs/3,
    enabled_receive/3,
    enabled_running_ports/3,
    enabled_send/3,
    trace_call/5,
    trace_garbage_collection/5,
    trace_ports/5,
    trace_procs/5,
    trace_receive/5,
    trace_running_ports/5,
    trace_send/5
   ]).

-doc false.
on_load() ->
    case erlang:load_nif(atom_to_list(?MODULE), 0) of
        ok -> ok
    end.

%%%
%%% NIF placeholders
%%%

%% This suppression is needed as trace_tag gets collapsed to atom()
-dialyzer({no_contracts, enabled/3}).

-doc false.
-spec enabled(Tag :: trace_status,
              TracerState :: tracer_state(),
              Tracee :: tracee()) ->
                     trace | remove;
             (Tag :: trace_tag() | seq_trace,
              TracerState :: tracer_state(),
              Tracee :: tracee()) ->
   trace | discard.
enabled(_, _, _) ->
    erlang:nif_error(nif_not_loaded).

-doc false.
-spec trace(Tag :: trace_tag() | seq_trace,
            TracerState :: tracer_state(),
            Tracee :: tracee(),
            Msg :: term(),
            Opts :: trace_opts()) -> any().

trace(_, _, _, _, _) ->
    erlang:nif_error(nif_not_loaded).
