%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2024. All Rights Reserved.
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
-module(dyntrace).
-moduledoc """
Interface to dynamic tracing

This module implements interfaces to dynamic tracing, should such be compiled
into the virtual machine. For a standard and/or commercial build, no dynamic
tracing is available, in which case none of the functions in this module is
usable or give any effect.

Should dynamic tracing be enabled in the current build, either by configuring
with `./configure --with-dynamic-trace=dtrace` or with
`./configure --with-dynamic-trace=systemtap`, the module can be used for two
things:

- Trigger the user-probe `user_trace_i4s4` in the NIF library `dyntrace.so` by
  calling `dyntrace:p/{1,2,3,4,5,6,7,8}`.
- Set a user specified tag that will be present in the trace messages of both
  the `efile_drv` and the user-probe mentioned above.

Both building with dynamic trace probes and using them is experimental and
unsupported by Erlang/OTP. It is included as an option for the developer to
trace and debug performance issues in their systems.

The original implementation is mostly done by Scott Lystiger Fritchie as an Open
Source Contribution and it should be viewed as such even though the source for
dynamic tracing as well as this module is included in the main distribution.
However, the ability to use dynamic tracing of the virtual machine is a very
valuable contribution which OTP has every intention to maintain as a tool for
the developer.

How to write `d` programs or `systemtap` scripts can be learned from books and
from a lot of pages on the Internet. This manual page does not include any
documentation about using the dynamic trace tools of respective platform.
However, the `examples` directory of the `runtime_tools` application contains
comprehensive examples of both `d` and `systemtap` programs that will help you
get started. Another source of information is the [dtrace](dtrace.md) and
[systemtap](systemtap.md) chapters in the Runtime Tools Users' Guide.
""".
-moduledoc(#{since => "OTP R15B01"}).

%%% @doc The Dynamic tracing interface module
%%%
%%% This Dynamic tracing interface module, with the corresponding NIFs, should
%%% work on any operating system platform where user-space DTrace/Systemtap
%%% (and in the future LttNG UST) probes are supported.
%%%
%%% It is recommended that you use the `dyntrace:p()' function to add
%%% Dynamic trace probes to your Erlang code.  This function can accept up to
%%% four integer arguments and four string arguments; the integer
%%% argument(s) must come before any string argument.
%%%
%%% If using DTrace, enable the dynamic trace probe using the 'dtrace'
%%% command, for example:
%%%
%%%    dtrace -s /your/path/to/lib/runtime_tools-1.8.7/examples/user-probe.d
%%%
%%% Then, back at the Erlang shell, try this example:
%%% ```
%%% 1> dyntrace:put_tag("GGOOOAAALL!!!!!").
%%% true
%%%
%%% 2> dyntrace:p(7, 8, 9, "one", "four").
%%% true
%%% '''
%%%
%%% Output from the example D script `user-probe.d' looks like:
%%% ```
%%% <0.34.0> GGOOOAAALL!!!!! 7 8 9 0 'one' 'four' '' ''
%%% '''
%%%
%%% If the expected type of variable is not present, e.g. integer when
%%% integer() is expected, or an I/O list when iolist() is expected,
%%% then the driver will ignore the user's input and use a default
%%% value of 0 or NULL, respectively.

-export([available/0,
         user_trace_s1/1, % TODO: unify with pid & tag args like user_trace_i4s4
         p/0, p/1, p/2, p/3, p/4, p/5, p/6, p/7, p/8,
         pn/1, pn/2, pn/3, pn/4, pn/5, pn/6, pn/7, pn/8, pn/9]).
-export([put_tag/1, get_tag/0, get_tag_data/0, spread_tag/1, restore_tag/1]).

-export([trace/5,
         trace_procs/5,
         trace_ports/5,
         trace_running_procs/5,
         trace_running_ports/5,
         trace_call/5,
         trace_send/5,
         trace_receive/5,
         trace_garbage_collection/5]).

-export([enabled_procs/3,
         enabled_ports/3,
         enabled_running_procs/3,
         enabled_running_ports/3,
         enabled_call/3,
         enabled_send/3,
         enabled_receive/3,
         enabled_garbage_collection/3,
         enabled/3]).

-export([user_trace_i4s4/9]). % Know what you're doing!

-nifs([available/0, user_trace_s1/1, user_trace_i4s4/9, user_trace_n/10,

       trace_procs/5, trace_ports/5, trace_running_procs/5,
       trace_running_ports/5, trace_call/5, trace_send/5,
       trace_receive/5, trace_garbage_collection/5, enabled_procs/3,
       enabled_ports/3, enabled_running_procs/3, enabled_running_ports/3,
       enabled_call/3, enabled_send/3, enabled_receive/3,
       enabled_garbage_collection/3,

       enabled/3, trace/5]).

-compile(no_native).
-on_load(on_load/0).

-type probe_arg() :: integer() | iolist().
-type int_p_arg() :: integer() | iolist() | undef.
-type n_probe_label() :: 0..1023.

%% The *_maybe() types use atom() instead of a stricter 'undef'
%% because user_trace_i4s4/9 is exposed to the outside world, and
%% because the driver will allow any atom to be used as a "not
%% present" indication, we'll allow any atom in the types.

-type integer_maybe() :: integer() | atom().
-type iolist_maybe() :: iolist() | atom().

-spec on_load() -> term().
on_load() ->
    PrivDir = code:priv_dir(runtime_tools),
    LibName = "dyntrace",
    Lib = filename:join([PrivDir, "lib", LibName]),
    Status = case erlang:load_nif(Lib, 0) of
                 ok -> ok;
                 {error, {load_failed, _}}=Error1 ->
                     ArchLibDir = 
                         filename:join([PrivDir, "lib", 
                                        erlang:system_info(system_architecture)]),
                     Candidate =
                         filelib:wildcard(
                           filename:join([ArchLibDir,LibName ++ "*" ]),
                           erl_prim_loader),
                     case Candidate of
                         [] -> Error1;
                         _ ->
                             ArchLib = filename:join([ArchLibDir, LibName]),
                             erlang:load_nif(ArchLib, 0)
                     end;
                 Error1 -> Error1
             end,
    case Status of
        ok -> ok;
        {error, {E, Str}} ->
	    case erlang:system_info(dynamic_trace) of
		none ->
		    ok;
		_ ->
		    error_logger:error_msg("Unable to load dyntrace library. Failed with error:~n
\"~p, ~s\"~n"
					   "Dynamic tracing is enabled but the driver is not built correctly~n",[
														 E,Str]),
		    Status
	    end
    end.

%%%
%%% NIF placeholders
%%%

-doc """
This function uses the NIF library to determine if dynamic tracing is available.

This function will throw an exception if the `dyntrace` NIF library could not be
loaded by the `on_load` function in this module.

Use [`erlang:system_info(dynamic_trace)`](`e:erts:erlang.md#system_info_dynamic_trace`)
to determine whether the run-time system supports dynamic tracing.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec available() -> true | false.

available() ->
    erlang:nif_error(nif_not_loaded).

-doc false.
-spec user_trace_s1(iolist()) -> true | false | error | badarg.

user_trace_s1(_Message) ->
    erlang:nif_error(nif_not_loaded).

-doc false.
-spec user_trace_i4s4(binary() | undefined,
                      integer_maybe(), integer_maybe(),
                          integer_maybe(), integer_maybe(),
                      iolist_maybe(), iolist_maybe(),
                          iolist_maybe(), iolist_maybe()) ->
      true | false | error | badarg.

user_trace_i4s4(_, _, _, _, _, _, _, _, _) ->
    erlang:nif_error(nif_not_loaded).

-spec user_trace_n(n_probe_label(), binary() | undefined,
                   integer_maybe(), integer_maybe(),
                   integer_maybe(), integer_maybe(),
                   iolist_maybe(), iolist_maybe(),
                   iolist_maybe(), iolist_maybe()) ->
      true | false | error | badarg.

user_trace_n(_, _, _, _, _, _, _, _, _, _) ->
    erlang:nif_error(nif_not_loaded).

-doc false.
trace(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

-doc false.
trace_procs(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

-doc false.
trace_ports(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

-doc false.
trace_running_procs(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

-doc false.
trace_running_ports(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

-doc false.
trace_call(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

-doc false.
trace_send(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

-doc false.
trace_receive(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

-doc false.
trace_garbage_collection(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) ->
    erlang:nif_error(nif_not_loaded).

-doc false.
enabled(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

-doc false.
enabled_procs(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

-doc false.
enabled_ports(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

-doc false.
enabled_running_procs(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

-doc false.
enabled_running_ports(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

-doc false.
enabled_call(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

-doc false.
enabled_send(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

-doc false.
enabled_receive(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

-doc false.
enabled_garbage_collection(_TraceTag, _TracerState, _Tracee) ->
    erlang:nif_error(nif_not_loaded).

%%%
%%% Erlang support functions
%%%

-doc """
Calling this function triggers the "user" trace probe `user_trace_i4s4` in the
dyntrace NIF module, sending a trace message containing only the user tag and
zeroes/empty strings in all other fields.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec p() -> true | false | error | badarg.

p() ->
    user_trace_int(undef, undef, undef, undef, undef, undef, undef, undef).

-doc """
p(Arg)

Calling this function will trigger the "user" trace probe `user_trace_i4s4` in the
`dyntrace` NIF module, sending a trace message containing the user tag and the
integer or string parameter in the first integer/string field.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec p(probe_arg()) -> true | false | error | badarg.

p(I1) when is_integer(I1) ->
    user_trace_int(I1, undef, undef, undef, undef, undef, undef, undef);
p(S1) ->
    user_trace_int(undef, undef, undef, undef, S1, undef, undef, undef).

-doc """
p(Arg1, Arg2)

Calling this function will trigger the "user" trace probe `user_trace_i4s4` in the
dyntrace NIF module, sending a trace message containing the user tag and the
[`integer()`](`t:integer/0`) or [`string()`](`t:string/0`) parameters as the
first fields of their respective type.

[`integer()`](`t:integer/0`) parameters should be put before any
[`string()`](`t:string/0`) parameters.

That is, the following calls work:

- [`dyntrace:p(1, "Hello")`](`p/2`)
- [`dyntrace:p(1, 1)`](`p/2`)
- [`dyntrace:p("Hello", "Again")`](`p/2`)

The following call is invalid because the string argument comes before the
integer argument:

- [`dyntrace:p("Hello", 1)`](`p/2`)

""".
-doc(#{since => <<"OTP R15B01">>}).
-spec p(probe_arg(), probe_arg()) -> true | false | error | badarg.

p(I1, I2) when is_integer(I1), is_integer(I2) ->
    user_trace_int(I1, I2, undef, undef, undef, undef, undef, undef);
p(I1, S1) when is_integer(I1) ->
    user_trace_int(I1, undef, undef, undef, S1, undef, undef, undef);
p(S1, S2) ->
    user_trace_int(undef, undef, undef, undef, S1, S2, undef, undef).

-doc """
p(Arg1, Arg2, Arg3)

Calling this function will trigger the "user" trace probe `user_trace_i4s4` in the
dyntrace NIF module, sending a trace message containing the user tag and the
[`integer()`](`t:integer/0`) or [`string()`](`t:string/0`) parameters as the
first fields of their respective type.

[`integer()`](`t:integer/0`) parameters should be put before any
[`string()`](`t:string/0`) parameters.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec p(probe_arg(), probe_arg(), probe_arg()) -> true | false | error | badarg.

p(I1, I2, I3) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_int(I1, I2, I3, undef, undef, undef, undef, undef);
p(I1, I2, S1) when is_integer(I1), is_integer(I2) ->
    user_trace_int(I1, I2, undef, undef, S1, undef, undef, undef);
p(I1, S1, S2) when is_integer(I1) ->
    user_trace_int(I1, undef, undef, undef, S1, S2, undef, undef);
p(S1, S2, S3) ->
    user_trace_int(undef, undef, undef, undef, S1, S2, S3, undef).

-doc """
p(Arg1, Arg2, Arg3, Arg4)

Calling this function will trigger the "user" trace probe `user_trace_i4s4` in the
dyntrace NIF module, sending a trace message containing the user tag and the
[`integer()`](`t:integer/0`) or [`string()`](`t:string/0`) parameters as the
first fields of their respective type.

[`integer()`](`t:integer/0`) parameters should be put before any
[`string()`](`t:string/0`) parameters.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec p(probe_arg(), probe_arg(), probe_arg(), probe_arg()) ->
      true | false | error | badarg.

p(I1, I2, I3, I4) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_int(I1, I2, I3, I4, undef, undef, undef, undef);
p(I1, I2, I3, S1) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_int(I1, I2, I3, undef, S1, undef, undef, undef);
p(I1, I2, S1, S2) when is_integer(I1), is_integer(I2) ->
    user_trace_int(I1, I2, undef, undef, S1, S2, undef, undef);
p(I1, S1, S2, S3) when is_integer(I1) ->
    user_trace_int(I1, undef, undef, undef, S1, S2, S3, undef);
p(S1, S2, S3, S4) ->
    user_trace_int(undef, undef, undef, undef, S1, S2, S3, S4).

-doc """
p(Int, Arg1, Arg2, Arg3, String)

Calling this function will trigger the "user" trace probe `user_trace_i4s4` in the
dyntrace NIF module, sending a trace message containing the user tag and the
[`integer()`](`t:integer/0`) or [`string()`](`t:string/0`) parameters as the
first fields of their respective type.

[`integer()`](`t:integer/0`) parameters should be put before any
[`string()`](`t:string/0`) parameters.

There can be no more than four parameters of each type,
so the first parameter must be of type [`integer()`](`t:integer/0`) and
the last parameter of type [`string()`](`t:string/0`).
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec p(probe_arg(), probe_arg(), probe_arg(), probe_arg(),
        probe_arg()) ->
      true | false | error | badarg.

p(I1, I2, I3, I4, S1) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_int(I1, I2, I3, I4, S1, undef, undef, undef);
p(I1, I2, I3, S1, S2) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_int(I1, I2, I3, undef, S1, S2, undef, undef);
p(I1, I2, S1, S2, S3) when is_integer(I1), is_integer(I2) ->
    user_trace_int(I1, I2, undef, undef, S1, S2, S3, undef);
p(I1, S1, S2, S3, S4) when is_integer(I1) ->
    user_trace_int(I1, undef, undef, undef, S1, S2, S3, S4).

-doc """
p(Int1, Int2, Arg1, Arg2, String1, String2)

Calling this function will trigger the "user" trace probe `user_trace_i4s4` in the
dyntrace NIF module, sending a trace message containing the user tag and the
[`integer()`](`t:integer/0`) or [`string()`](`t:string/0`) parameters as the
first fields of their respective type.

[`integer()`](`t:integer/0`) parameters should be put before any
[`string()`](`t:string/0`) parameters.

There can be no more than four parameters of each type,
so the first two parameters must be of type [`integer()`](`t:integer/0`) and
the last two of type [`string()`](`t:string/0`).
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec p(probe_arg(), probe_arg(), probe_arg(), probe_arg(),
        probe_arg(), probe_arg()) ->
      true | false | error | badarg.

p(I1, I2, I3, I4, S1, S2) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_int(I1, I2, I3, I4, S1, S2, undef, undef);
p(I1, I2, I3, S1, S2, S3) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_int(I1, I2, I3, undef, S1, S2, S3, undef);
p(I1, I2, S1, S2, S3, S4) when is_integer(I1), is_integer(I2) ->
    user_trace_int(I1, I2, undef, undef, S1, S2, S3, S4).

-doc """
p(Int1, Int2, Int3, Arg, String1, String2, String4)

Calling this function will trigger the "user" trace probe `user_trace_i4s4` in the
dyntrace NIF module, sending a trace message containing the user tag and the
[`integer()`](`t:integer/0`) or [`string()`](`t:string/0`) parameters as the
first fields of their respective type.

[`integer()`](`t:integer/0`) parameters should be put before any
[`string()`](`t:string/0`) parameters.

There can be no more than four parameters of each type,
so the first three parameters must be of type [`integer()`](`t:integer/0`) and
the last three of type [`string()`](`t:string/0`).
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec p(probe_arg(), probe_arg(), probe_arg(), probe_arg(),
        probe_arg(), probe_arg(), probe_arg()) ->
      true | false | error | badarg.

p(I1, I2, I3, I4, S1, S2, S3) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_int(I1, I2, I3, I4, S1, S2, S3, undef);
p(I1, I2, I3, S1, S2, S3, S4) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_int(I1, I2, I3, undef, S1, S2, S3, S4).

-doc """
p(Int1, Int2, Int3, Int4, String1, String2, String3, String4)

Calling this function will trigger the "user" trace `probe user_trace_i4s4` in the
dyntrace NIF module, sending a trace message containing all the
[`integer()`](`t:integer/0`) and [`string()`](`t:string/0`) parameters
provided, as well as any user tag set in the current process.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec p(probe_arg(), probe_arg(), probe_arg(), probe_arg(),
        probe_arg(), probe_arg(), probe_arg(), probe_arg()) ->
      true | false | error | badarg.

p(I1, I2, I3, I4, S1, S2, S3, S4) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_int(I1, I2, I3, I4, S1, S2, S3, S4).

-spec user_trace_int(int_p_arg(), int_p_arg(), int_p_arg(), int_p_arg(),
                     int_p_arg(), int_p_arg(), int_p_arg(), int_p_arg()) ->
      true | false | error | badarg.

user_trace_int(I1, I2, I3, I4, S1, S2, S3, S4) ->
    UTag = get_tag(),
    try
        user_trace_i4s4(UTag, I1, I2, I3, I4, S1, S2, S3, S4)
    catch
        error:nif_not_loaded ->
            false
    end.

-doc false.
-spec pn(n_probe_label()) -> true | false | error | badarg.

pn(ProbeLabel) ->
    user_trace_n_int(ProbeLabel, undef, undef, undef, undef, undef, undef, undef, undef).

-doc false.
-spec pn(n_probe_label(), probe_arg()) -> true | false | error | badarg.

pn(ProbeLabel, I1) when is_integer(I1) ->
    user_trace_n_int(ProbeLabel, I1, undef, undef, undef, undef, undef, undef, undef);
pn(ProbeLabel, S1) ->
    user_trace_n_int(ProbeLabel, undef, undef, undef, undef, S1, undef, undef, undef).

-doc false.
-spec pn(n_probe_label(), probe_arg(), probe_arg()) -> true | false | error | badarg.

pn(ProbeLabel, I1, I2) when is_integer(I1), is_integer(I2) ->
    user_trace_n_int(ProbeLabel, I1, I2, undef, undef, undef, undef, undef, undef);
pn(ProbeLabel, I1, S1) when is_integer(I1) ->
    user_trace_n_int(ProbeLabel, I1, undef, undef, undef, S1, undef, undef, undef);
pn(ProbeLabel, S1, S2) ->
    user_trace_n_int(ProbeLabel, undef, undef, undef, undef, S1, S2, undef, undef).

-doc false.
-spec pn(n_probe_label(), probe_arg(), probe_arg(), probe_arg()) -> true | false | error | badarg.

pn(ProbeLabel, I1, I2, I3) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_n_int(ProbeLabel, I1, I2, I3, undef, undef, undef, undef, undef);
pn(ProbeLabel, I1, I2, S1) when is_integer(I1), is_integer(I2) ->
    user_trace_n_int(ProbeLabel, I1, I2, undef, undef, S1, undef, undef, undef);
pn(ProbeLabel, I1, S1, S2) when is_integer(I1) ->
    user_trace_n_int(ProbeLabel, I1, undef, undef, undef, S1, S2, undef, undef);
pn(ProbeLabel, S1, S2, S3) ->
    user_trace_n_int(ProbeLabel, undef, undef, undef, undef, S1, S2, S3, undef).

-doc false.
-spec pn(n_probe_label(), probe_arg(), probe_arg(), probe_arg(), probe_arg()) ->
      true | false | error | badarg.

pn(ProbeLabel, I1, I2, I3, I4) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_n_int(ProbeLabel, I1, I2, I3, I4, undef, undef, undef, undef);
pn(ProbeLabel, I1, I2, I3, S1) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_n_int(ProbeLabel, I1, I2, I3, undef, S1, undef, undef, undef);
pn(ProbeLabel, I1, I2, S1, S2) when is_integer(I1), is_integer(I2) ->
    user_trace_n_int(ProbeLabel, I1, I2, undef, undef, S1, S2, undef, undef);
pn(ProbeLabel, I1, S1, S2, S3) when is_integer(I1) ->
    user_trace_n_int(ProbeLabel, I1, undef, undef, undef, S1, S2, S3, undef);
pn(ProbeLabel, S1, S2, S3, S4) ->
    user_trace_n_int(ProbeLabel, undef, undef, undef, undef, S1, S2, S3, S4).

-doc false.
-spec pn(n_probe_label(), probe_arg(), probe_arg(), probe_arg(), probe_arg(),
        probe_arg()) ->
      true | false | error | badarg.

pn(ProbeLabel, I1, I2, I3, I4, S1) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_n_int(ProbeLabel, I1, I2, I3, I4, S1, undef, undef, undef);
pn(ProbeLabel, I1, I2, I3, S1, S2) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_n_int(ProbeLabel, I1, I2, I3, undef, S1, S2, undef, undef);
pn(ProbeLabel, I1, I2, S1, S2, S3) when is_integer(I1), is_integer(I2) ->
    user_trace_n_int(ProbeLabel, I1, I2, undef, undef, S1, S2, S3, undef);
pn(ProbeLabel, I1, S1, S2, S3, S4) when is_integer(I1) ->
    user_trace_n_int(ProbeLabel, I1, undef, undef, undef, S1, S2, S3, S4).

-doc false.
-spec pn(n_probe_label(), probe_arg(), probe_arg(), probe_arg(), probe_arg(),
        probe_arg(), probe_arg()) ->
      true | false | error | badarg.

pn(ProbeLabel, I1, I2, I3, I4, S1, S2) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_n_int(ProbeLabel, I1, I2, I3, I4, S1, S2, undef, undef);
pn(ProbeLabel, I1, I2, I3, S1, S2, S3) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_n_int(ProbeLabel, I1, I2, I3, undef, S1, S2, S3, undef);
pn(ProbeLabel, I1, I2, S1, S2, S3, S4) when is_integer(I1), is_integer(I2) ->
    user_trace_n_int(ProbeLabel, I1, I2, undef, undef, S1, S2, S3, S4).

-doc false.
-spec pn(n_probe_label(), probe_arg(), probe_arg(), probe_arg(), probe_arg(),
        probe_arg(), probe_arg(), probe_arg()) ->
      true | false | error | badarg.

pn(ProbeLabel, I1, I2, I3, I4, S1, S2, S3) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_n_int(ProbeLabel, I1, I2, I3, I4, S1, S2, S3, undef);
pn(ProbeLabel, I1, I2, I3, S1, S2, S3, S4) when is_integer(I1), is_integer(I2), is_integer(I3) ->
    user_trace_n_int(ProbeLabel, I1, I2, I3, undef, S1, S2, S3, S4).

-doc false.
-spec pn(n_probe_label(), probe_arg(), probe_arg(), probe_arg(), probe_arg(),
        probe_arg(), probe_arg(), probe_arg(), probe_arg()) ->
      true | false | error | badarg.

pn(ProbeLabel, I1, I2, I3, I4, S1, S2, S3, S4) when is_integer(I1), is_integer(I2), is_integer(I3), is_integer(I4) ->
    user_trace_n_int(ProbeLabel, I1, I2, I3, I4, S1, S2, S3, S4).

-spec user_trace_n_int(n_probe_label(),
                       int_p_arg(), int_p_arg(), int_p_arg(), int_p_arg(),
                       int_p_arg(), int_p_arg(), int_p_arg(), int_p_arg()) ->
      true | false | error | badarg.

user_trace_n_int(ProbeLabel, I1, I2, I3, I4, S1, S2, S3, S4) ->
    UTag = get_tag(),
    try
        user_trace_n(ProbeLabel, UTag, I1, I2, I3, I4, S1, S2, S3, S4)
    catch
        error:nif_not_loaded ->
            false
    end.

-doc """
put_tag(Item)

This function sets the user tag of the current process.

The user tag is a [`binary()`](`t:binary/0`), but can be specified as
any [`iodata()`](`t:iodata/0`), which is automatically converted to a
binary by this function.

The user tag is provided to the user probes triggered by calls top
`dyntrace:p/{1,2,3,4,5,6,7,8}` as well as probes in the `efile` driver. In the
future, user tags might be added to more probes.

The old user tag (if any) is returned, or `undefined` if no user tag was present,
or dynamic tracing is not enabled.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec put_tag(undefined | iodata()) -> binary() | undefined.
put_tag(Data) ->
    erlang:dt_put_tag(unicode:characters_to_binary(Data)).

-doc """
This function returns the user tag set in the current process. If no tag is set
or dynamic tracing is not available, it returns `undefined`.

This function returns the user tag set in the current process or, if no user tag
is present, the last user tag sent to the process together with a message (in
the same way as [sequential trace tokens](`m:seq_trace`) are spread to other
processes together with messages. For an explanation of how user tags can be
spread together with messages, see `spread_tag/1`. If no tag is found or dynamic
tracing is not available, it returns `undefined`
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec get_tag() -> binary() | undefined.
get_tag() ->
    erlang:dt_get_tag().

-doc false.
-spec get_tag_data() -> binary() | undefined.
%% Gets tag if set, otherwise the spread tag data from last incoming message
get_tag_data() ->
    erlang:dt_get_tag_data().

-doc """
spread_tag(boolean())

This function controls if user tags are to be spread to other processes with the
next message.

Spreading of user tags work like spreading of sequential trace
tokens, so that a received user tag will be active in the process until the next
message arrives (if that message does not also contain the user tag).

This functionality is used when a client process communicates with a file
i/o-server to spread the user tag to the I/O-server and then down to the
`efile` driver. By using [`spread_tag/1`](`spread_tag/1`) and
[`restore_tag/1`](`restore_tag/1`), one can enable or disable spreading of user
tags to other processes and then restore the previous state of the user tag. The
TagData returned from this call contains all previous information so the state
(including any previously spread user tags) will be completely restored by a
later call to [`restore_tag/1`](`restore_tag/1`).

The `m:file` module already spreads tags, so there is no need to manually call
this function to get user tags spread to the `efile` driver through that module.

The most use of this function would be if one, for example, uses the `m:io` module
to communicate with an I/O-server for a regular file, such as in the following
example:

```erlang
f() ->
   {ok, F} = file:open("test.tst", [write]),
   Saved = dyntrace:spread_tag(true),
   io:format(F, "Hello world!", []),
   dyntrace:restore_tag(Saved),
   file:close(F).
```

In this example, any user tag set in the calling process will be spread to the
I/O-server when the `io:format/3` call is done.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec spread_tag(boolean()) -> true | {non_neg_integer(), binary() | []}.
%% Makes the tag behave as a sequential trace token, will spread with
%% messages to be picked up by someone using get_tag_data
spread_tag(B) ->
    erlang:dt_spread_tag(B).

-doc """
restore_tag(TagData)

Restores the previous state of user tags and their spreading as it was before a
call to `spread_tag/1`.

Note that the restoring is not limited to the same process; one can
utilize this to turn off spreding in one process and restore it in a
newly created process that is is actually going to send messages:

```erlang
f() ->
    TagData = dyntrace:spread_tag(false),
    spawn(fun() ->
             dyntrace:restore_tag(TagData),
             do_something()
          end),
    do_something_else(),
    dyntrace:restore_tag(TagData).
```

Correctly handling user tags and their spreading might take some effort, as
Erlang programs tend to send and receive messages so that sometimes the user tag
gets lost due to various things, like double receives or communication with a
port (ports do not handle user tags, in the same way as they do not handle
regular sequential trace tokens).
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec restore_tag(true | {non_neg_integer(), binary() | []}) -> true.
restore_tag(T) ->
    erlang:dt_restore_tag(T).
