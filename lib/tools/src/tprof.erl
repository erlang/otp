%%
%%
%% Copyright WhatsApp Inc. and its affiliates. All rights reserved.
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
%%
%%-------------------------------------------------------------------
%%
%% @author Maxim Fedorov <maximfca@gmail.com>
%% Erlang Process Tracing profiler.
%%
-module(tprof).
-moduledoc """
Process Tracing Profiling Tool

`tprof` provides convenience helpers for Erlang process profiling. Underlying
mechanism is the Erlang trace BIFs.

> #### Warning {: .warning }
>
> This module aims to replace `eprof` and `cprof` into a unified API for
> measuring call count, time, and allocation. It is experimental in Erlang/OTP
> 27.0.

It is possible to analyze the number of calls, the time spent by function, and
heap allocations by function. Profiling can be done ad-hoc or run in a
server-aided mode for deeper introspection of the code running in production.

> #### Warning {: .warning }
>
> Avoid hot code reload for modules that are participating in the tracing.
> Reloading a module turns tracing off, discarding accumulated statistics.
> `tprof` may not report correct amounts when code reload happened during
> profiling session.

The `type` option controls which type of profiling to perform. You can choose
between `call_count`, `call_time`, and `call_memory`. The default is
`call_count`, it has the smallest footprint on the system but it does not
support per-process profiling. For this reason, all of the examples below use
`call_memory`, which measures heap allocation, and provide a more complex
feature set to demonstrate.

Heap allocations happen for all Erlang terms that do not fit a single machine
word. For example, a function returning tuple of 2 elements needs to allocate
this tuple on the process heap. Actual consumption is more than 2 machine words,
because Erlang runtime needs to store tuple size and other internal information.

> #### Note {: .info }
>
> When profiling is enabled, expect a slowdown in the program execution.
>
> For profiling convenience, measurements are accumulated for functions that are
> not enabled in trace pattern. Consider this call stack example:
>
> ```text
> top_traced_function(...)
> not_traced_function()
> bottom_traced_function()
> ```
>
> Allocations that happened within `not_traced_function` will be accounted into
> `top_traced_function`. However allocations happened within
> `bottom_traced_function` are not included in the `top_traced_function`. If you
> want to account only own allocations, you need to trace all functions.

## Ad-hoc profiling

Basic profiling providing accumulated memory allocation data. You can choose to
print per-process statistics, total statistics, or omit printing and extract
machine-readable data that you can later sort/print:

```text
1> tprof:profile(lists, seq, [1, 16], #{type => call_memory}).

****** Process <0.179.0>    -- 100.00 % of total allocations ***
FUNCTION          CALLS  WORDS  PER CALL  [     %]
lists:seq_loop/3      5     32         6  [100.00]
32            [ 100.0]
ok
```

By default tracing is enabled for all functions of all modules. When functions
are created in the interactive shell, parts of shell code are also traced. It is
however possible to limit the trace to specific functions or modules:

```text
1> tprof:profile(fun() -> lists:seq(1, 16) end, #{type => call_memory}).

****** Process <0.224.0>    -- 100.00 % of total allocations ***
FUNCTION                   CALLS  WORDS  PER CALL  [    %]
erl_eval:match_list/6          1      3         3  [ 3.19]
erl_eval:do_apply/7            1      3         3  [ 3.19]
lists:reverse/1                1      4         4  [ 4.26]
erl_eval:add_bindings/2        1      5         5  [ 5.32]
erl_eval:expr_list/7           3      7         2  [ 7.45]
erl_eval:ret_expr/3            4     16         4  [17.02]
erl_eval:merge_bindings/4      3     24         8  [25.53]
lists:seq_loop/3               5     32         6  [34.04]

2> tprof:profile(fun() -> lists:seq(1, 16) end,
                 #{type => call_memory, pattern => [{lists, seq_loop, '_'}]}).
****** Process <0.247.0>    -- 100.00 % of total allocations ***
FUNCTION          CALLS  WORDS  PER CALL  [     %]
lists:seq_loop/3      5     32         6  [100.00]
```

Ad-hoc profiling results may be printed in a few different ways. Following
examples are using `test` module defined like this:

```erlang
-module(test).
-export([test_spawn/0]).
test_spawn() ->
    {Pid, MRef} = spawn_monitor(fun () -> lists:seq(1, 32) end),
    receive
        {'DOWN', MRef, process, Pid, normal} ->
            done
    end.
```

Default format prints per-process statistics.

```text
2> tprof:profile(test, test_spawn, [], #{type => call_memory}).

****** Process <0.176.0>    -- 23.66 % of total allocations ***
FUNCTION                CALLS  WORDS  PER CALL  [    %]
erlang:spawn_monitor/1      1      2         2  [ 9.09]
erlang:spawn_opt/4          1      6         6  [27.27]
test:test_spawn/0           1     14        14  [63.64]
                                    22            [100.0]

****** Process <0.177.0>    -- 76.34 % of total allocations ***
FUNCTION           CALLS  WORDS  PER CALL  [    %]
erlang:apply/2         1      7         7  [ 9.86]
lists:seq_loop/3       9     64         7  [90.14]
                             71            [100.0]
```

This example prints the combined memory allocation of all processes, sorted by
total allocated words in the descending order

```text
5> tprof:profile(test, test_spawn, [],
                 #{type => call_memory, report => {total, {measurement, descending}}}).

FUNCTION                CALLS  WORDS  PER CALL  [    %]
lists:seq_loop/3            9     64         7  [68.82]
test:test_spawn/0           1     14        14  [15.05]
erlang:apply/2              1      7         7  [ 7.53]
erlang:spawn_opt/4          1      6         6  [ 6.45]
erlang:spawn_monitor/1      1      2         2  [ 2.15]
                                  93            [100.0]
```

You can also collect the profile for further inspection.

```text
6> {done, ProfileData} = tprof:profile(fun test:test_spawn/0,
                                       #{type => call_memory, report => return}).
<...>
7> tprof:format(tprof:inspect(ProfileData, process, {percent, descending})).

****** Process <0.223.0>    -- 23.66 % of total allocations ***
FUNCTION                CALLS  WORDS  PER CALL  [    %]
test:test_spawn/0           1     14        14  [63.64]
erlang:spawn_opt/4          1      6         6  [27.27]
erlang:spawn_monitor/1      1      2         2  [ 9.09]
22            [100.0]

****** Process <0.224.0>    -- 76.34 % of total allocations ***
FUNCTION           CALLS  WORDS  PER CALL  [    %]
lists:seq_loop/3       9     64         7  [90.14]
erlang:apply/2         1      7         7  [ 9.86]
71            [100.0]
```

The processes which are profiled depends on the profiling type. `call_count`,
the default, will count calls across all processes. The other types, `call_time`
and `call_memory`, take into account all processes spawned from the
user-provided function (using `set_on_spawn` argument for trace/3 BIF). You
cannot restrict the profiled processes for `call_count`, but you can limit the
trace to a single process for the other two:

```text
2> tprof:profile(test, test_spawn, [],
                 #{type => call_memory, set_on_spawn => false}).

****** Process <0.183.0>    -- 100.00 % of total allocations ***
FUNCTION                CALLS  WORDS  PER CALL  [    %]
erlang:spawn_monitor/1      1      2         2  [ 9.09]
erlang:spawn_opt/4          1      6         6  [27.27]
test:test_spawn/0           1     14        14  [63.64]
```

[](){: #pg_example }

Erlang programs may perform expensive operations in processes that are different
from the original one. You can include multiple, new or even all processes in
the trace when measuring time or memory:

```text
7> pg:start_link().
{ok,<0.252.0>}
8> tprof:profile(fun() -> pg:join(group, self()) end,
                 #{type => call_memory, rootset => [pg]}).
****** Process <0.252.0>    -- 52.86 % of total allocations ***
FUNCTION                      CALLS  WORDS  PER CALL  [    %]
pg:leave_local_update_ets/5       1      2         2  [ 1.80]
gen:reply/2                       1      3         3  [ 2.70]
erlang:monitor/2                  1      3         3  [ 2.70]
gen_server:try_handle_call/4      1      3         3  [ 2.70]
gen_server:try_dispatch/4         1      3         3  [ 2.70]
maps:iterator/1                   2      4         2  [ 3.60]
maps:take/2                       1      6         6  [ 5.41]
pg:join_local_update_ets/5        1      8         8  [ 7.21]
pg:handle_info/2                  1      8         8  [ 7.21]
pg:handle_call/3                  1      9         9  [ 8.11]
gen_server:loop/7                 2      9         4  [ 8.11]
ets:lookup/2                      2     10         5  [ 9.01]
pg:join_local/3                   1     11        11  [ 9.91]
pg:notify_group/5                 2     16         8  [14.41]
erlang:setelement/3               2     16         8  [14.41]
111            [100.0]

****** Process <0.255.0>    -- 47.14 % of total allocations ***
FUNCTION                   CALLS  WORDS  PER CALL  [    %]
erl_eval:match_list/6          1      3         3  [ 3.03]
erlang:monitor/2               1      3         3  [ 3.03]
lists:reverse/1                2      4         2  [ 4.04]
pg:join/3                      1      4         4  [ 4.04]
erl_eval:add_bindings/2        1      5         5  [ 5.05]
erl_eval:do_apply/7            2      6         3  [ 6.06]
gen:call/4                     1      8         8  [ 8.08]
erl_eval:expr_list/7           4     10         2  [10.10]
gen:do_call/4                  1     16        16  [16.16]
erl_eval:ret_expr/3            4     16         4  [16.16]
erl_eval:merge_bindings/4      3     24         8  [24.24]
99            [100.0]
```

There is no default limit on the profiling time. It is possible to define such
limit for ad-hoc profile. If function being profiled does not return in a
specified amount of time, process is terminated with `kill` reason. Any unlinked
children started by the user-supplied function are kept, it is developer's
responsibility to ensure cleanup.

```erlang
9> tprof:profile(timer, sleep, [100000], #{timeout => 1000}).
```

By default, only one ad-hoc or server-aided profiling session is allowed at any
point in time. It is possible to force multiple ad-hoc sessions concurrently,
but it is developer responsibility to ensure non-overlapping trace patterns.

```erlang
1> tprof:profile(fun() -> lists:seq(1, 32) end,
    #{registered => false, pattern => [{lists, '_', '_'}]}).
```

## Server-aided profiling

Memory profiling can be done when your system is up and running. You can start
the `tprof` server, add trace patterns and processes to trace while your system
handles actual traffic. You can extract the data any time, inspect, and print.
The example below traces activity of all processes supervised by kernel:

```text
1> tprof:start(#{type => call_memory}).
{ok,<0.200.0>}
2> tprof:enable_trace({all_children, kernel_sup}).
34
3> tprof:set_pattern('_', '_' , '_').
16728
4> Sample = tprof:collect().
[{gen_server,try_dispatch,4,[{<0.154.0>,2,6}]},
{erlang,iolist_to_iovec,1,[{<0.161.0>,1,8}]},
<...>
5 > tprof:format(tprof:inspect(Sample)).

****** Process <0.154.0>    -- 14.21 % of total allocations ***
FUNCTION                   CALLS  WORDS  PER CALL  [    %]
maps:iterator/1                2      4         2  [15.38]
gen_server:try_dispatch/4      2      6         3  [23.08]
net_kernel:handle_info/2       2     16         8  [61.54]
                                     26            [100.0]

****** Process <0.161.0>    -- 85.79 % of total allocations ***
FUNCTION                        CALLS  WORDS  PER CALL  [    %]
disk_log:handle/2                   2      2         1  [ 1.27]
disk_log_1:maybe_start_timer/1      1      3         3  [ 1.91]
disk_log_1:mf_write_cache/1         1      3         3  [ 1.91]
<...>
```

[](){: #inspect_example }

It is possible to profile the entire running system, and then examine individual
processes:

```text
1> tprof:start(#{type => call_memory}).
2> tprof:enable_trace(processes), tprof:set_pattern('_', '_' , '_').
9041
3> timer:sleep(10000), tprof:disable_trace(processes), Sample = tprof:collect().
[{user_drv,server,3,[{<0.64.0>,12,136}]},
{user_drv,contains_ctrl_g_or_ctrl_c,1,[{<0.64.0>,80,10}]},
<...>
4> Inspected = tprof:inspect(Sample, process, words), Shell = maps:get(self(), Inspected).
{2743,
[{shell,{enc,0},1,2,2,0.07291286912139992},
<...>
5> tprof:format(Shell).

FUNCTION                           CALLS  WORDS  PER CALL  [    %]
<...>
erl_lint:start/2                       2    300       150  [10.94]
shell:used_records/1                 114    342         3  [12.47]
```
""".
-moduledoc(#{since => "OTP @OTP-18756@"}).

%% API
-export([
    start/0,
    start/1,
    start_link/0,
    start_link/1,
    stop/0,
    set_pattern/3,
    clear_pattern/3,
    get_trace_map/0,
    enable_trace/1, enable_trace/2,
    disable_trace/1, disable_trace/2,
    pause/0,
    continue/0,
    restart/0,
    collect/0,
    %% ad-hoc profiling
    profile/1, profile/2, profile/3, profile/4,
    %% Analysis API
    inspect/1, inspect/3,
    format/1, format/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-compile([warn_missing_spec]).

%% typedefs for easier digestion

%% Configures the type of profiling.
-type start_options() :: #{type => trace_type()}.

%% Trace type
-type trace_type() :: call_count | call_time | call_memory.

%% Trace spec: module() or '_', function or '_', arity or '_'
-type trace_pattern() :: {module(), Fun :: atom(), arity() | '_'}.

%% Trace map: accumulated view of multiple trace patterns
-doc """
Traced functions (with their arities) grouped by module name,
or `all` if all code is traced.
""".
-type trace_map() :: #{module() => [{Fun :: atom(), arity()}]} | all.

%% Single trace_info call with associated module/function/arity
-doc "Raw data extracted from tracing BIFs.".
-type trace_info() :: {module(), Fun :: atom(), Arity :: non_neg_integer(),
    [{pid(), Count :: pos_integer(), Measurement :: pos_integer()}]}.

%% Combined report for a single function (one or all processes).
-doc "Inspected data for a single function of the specified `Module`.".
-type profile_line() :: {module(), Function :: {atom(), arity()},
    Count :: pos_integer(), Measurement :: pos_integer(), MeasurementPerCall :: non_neg_integer(), Percent :: float()}.

%% Single profiling attempt result.
-doc """
Profile of a single process, or combined profile of multiple processes, sorted
by a selected column.
""".
-type profile_result() :: {trace_type(), TotalMeasurement :: non_neg_integer(), [profile_line()]}.

%% Convenience type used to sort the profiling results.
-doc """
Column to sort by `inspect/3`, or [`profile`](`profile/2`).

- **`module`** - Module name.

- **`function`** - Function name.

- **`calls`** - Number of calls to the function.

- **`measurement`** - Total measurement (call count, time, or heap allocation)
  throughout all calls to the function.

- **`measurement_per_call`** - Measurement (call count, time, or heap
  allocation) on average per function call.

- **`percent`** - Percentage of measurement to total amount during the entire
  profile collection.
""".
-type column() :: module | function | calls | measurement | measurement_per_call | percent.

%% Sort by
-type sort_by() :: column() | {column(), ascending} | {column(), descending}.

%% Selected options allowed for enable/disable trace
-doc "Options for enabling profiling of the selected processes, see `enable_trace/2`.".
-type trace_options() :: #{
    set_on_spawn => boolean()
}.

%% Convenience type to define which processes to trace
-type rootset() :: [process()] |   %% list of pids/registered names
    processes |
    existing_processes |
    new_processes.

-doc "Ad-hoc profiler options, see [`profile`](`profile/2`).".
-type profile_options() :: #{
    type => trace_type(),                           %% the type of profiling
    timeout => timeout(),                           %% stop profiling after the timeout
    pattern => trace_pattern() | [trace_pattern()], %% list of patterns to trace
    set_on_spawn => boolean(),                      %% trace spawned processes or not (true by default)
    rootset => rootset(),                           %% extra processes to trace
    report => return | process | total | {process, sort_by()} | {total, sort_by()},   %% print or return results
    device => io:device(),                          %% device to report to
    registered => false | {local, atom()}           %% register the profiler process (to detect concurrent attempts)
}.

%%--------------------------------------------------------------------
%% Server-aided API
-doc """
Starts the server, not supervised. Profiling server stores current trace
patterns and ensures a single instance of profiler is running.
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec start() -> {'ok', Pid} | {'error', Reason} when Pid :: pid(), Reason :: {'already_started', Pid}.
start() ->
    start(#{}).

-doc false.
-spec start(start_options()) -> {'ok', Pid} | {'error', Reason}
        when Pid :: pid(), Reason :: {'already_started', Pid}.
start(Config) when is_map(Config) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Starts the process and links it to the caller.
-doc "Starts the server, supervised by the calling process.".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec start_link() -> {'ok', Pid} | {'error', Reason} when Pid :: pid(), Reason :: {'already_started', Pid}.
start_link() ->
    start_link(#{}).

-doc false.
-spec start_link(start_options()) -> {'ok', Pid} | {'error', Reason}
        when Pid :: pid(), Reason :: {'already_started', Pid}.
start_link(Config) when is_map(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

-doc "Stops the `tprof`, disabling tracing that has been enabled.".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

-doc """
Turns tracing on for the supplied pattern. Requires running `tprof`. Patterns
are additive, following the same rules as `erlang:trace_pattern/3`. Returns
number of functions matching the supplied pattern.

```erlang
1> tprof:set_pattern(lists, seq, '_').
2
2> tprof:set_pattern(lists, keyfind, 3).
1
3> tprof:get_trace_map().
#{lists => [{keyfind,3},{seq,2},{seq,3}]}
```

If there are no functions matching the pattern, error is returned

```erlang
> tprof:set_pattern(no_module, func, '_').
{error,{trace_pattern,no_module,func,'_'}}
```
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec set_pattern(module(), atom(), arity() | '_') -> ok | {error, {trace_pattern, trace_pattern()}}.
set_pattern(Mod, Fun, Arity) ->
    gen_server:call(?MODULE, {set_pattern, Mod, Fun, Arity}, infinity).

%% @doc Stops tracing all or specific function patterns.
-doc """
Turns tracing off for the supplied pattern.

```erlang
1> tprof:set_pattern(lists, seq, '_').
2
2> tprof:clear_pattern(lists, seq, 3).
1
3> tprof:get_trace_map().
#{lists => [{seq,2}]}
```
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec clear_pattern(module(), atom(), arity() | '_') -> ok.
clear_pattern(Mod, Fun, Arity) ->
    gen_server:call(?MODULE, {clear_pattern, Mod, Fun, Arity}, infinity).

%% @doc Returns current trace map.
-doc "Returns a map of module names to functions with their arities.".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec get_trace_map() -> trace_map().
get_trace_map() ->
    gen_server:call(?MODULE, get_trace_map).

%% @doc Returns statistics for current trace map.
-doc false.
-spec collect() -> {trace_type(), [trace_info()]}.
collect() ->
    gen_server:call(?MODULE, collect, infinity).

%% Process identified by a PID or a registered name.
-doc "Either process identified (pid), or a registered process name.".
-type process() :: pid() | atom().

%% @doc Shortcut for erlang:trace/3 BIF touching only memory tracing flags.
%%      Returns number of successful operations, and list of those unsuccessful
%%      if the list was supplied. By default applies set_on_spawn flag.
-doc """
The same as
[`enable_trace` ](`enable_trace/2`)`(Spec, #{set_on_spawn => true})`.
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec enable_trace(Spec) -> non_neg_integer()
    when Spec :: pid() |
        processes |
        new_processes |
        existing_processes |
        {children | all_children, process()};
    ([process()]) -> non_neg_integer() | {non_neg_integer(), [process()]}.
enable_trace(Rootset) ->
    enable_trace(Rootset, #{set_on_spawn => true}).

-doc """
Similar to `erlang:trace/3`, but supports a few more options for tracing
convenience. Tracing per process is not supported by `call_count` profilers.

`Spec` is either a process identifier (pid) for a local process, one of the
following atoms, or a list of local process identifiers or their registered
names:

- **`processes`** - All currently existing processes and all that will be
  created in the future.

- **`existing_processes`** - All currently existing processes.

- **`new_processes`** - All processes that will be created in the future.

- **`children`** - All currently running processes that were directly spawned by
  the specified process. This mode is helpful for tracing workers of a single
  supervisor.

- **`all_children`** - All currently running processes that were spawned by the
  specified process, or any recursive descendant of it. This mode is designed to
  facilitate tracing of supervision trees.

> #### Note {: .info }
>
> The profiling server does not keep track of processes that were added to the
> tracing set. It is permitted to stop the profiling server (wiping out any
> accumulated data), restart the server, set entirely different tracing pattern
> keeping the list of traced processes for future use. Use
> [`disable_trace`(processes)](`disable_trace/2`) to clear the list of traced
> processes.

Specify `Options` to modify tracing behaviour:

- **`set_on_spawn`** - Automatically start tracing for processes spawned by the
  traced process. On by default.
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec enable_trace(Spec, trace_options()) -> non_neg_integer()
    when Spec :: pid() |
        processes |
        new_processes |
        existing_processes |
        {children | all_children, process()};
    ([process()], trace_options()) -> non_neg_integer() | {non_neg_integer(), [process()]}.
enable_trace(Procs, Options) when Procs =:= processes; Procs =:= new_processes; Procs =:= existing_processes ->
    erlang:trace(Procs, true, trace_options(Options));
enable_trace({Children, PidOrName}, Options) when Children =:= children; Children =:= all_children ->
    Pids = children(Children, PidOrName),
    toggle_trace(Pids, true, trace_options(Options), 0, []);
enable_trace(Pid, Options) when is_pid(Pid); is_atom(Pid) ->
    toggle_process_trace(Pid, true, trace_options(Options));
enable_trace(List, Options) when is_list(List) ->
    toggle_trace(List, true, trace_options(Options), 0, []).

-doc """
The same as
[`disable_trace` ](`disable_trace/2`)`(Spec, #{set_on_spawn => true})`.
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec disable_trace(Spec) -> non_neg_integer()
    when Spec :: pid() |
        processes |
        new_processes |
        existing_processes |
        {children | all_children, process()};
    ([process()]) -> non_neg_integer() | {non_neg_integer(), [process()]}.
disable_trace(Rootset) ->
    disable_trace(Rootset, #{set_on_spawn => true}).

-doc """
Stops accumulating traces for specified processes. See `enable_trace/2` for
options description.

Profile accumulated before process is removed from the traced list is retained.
This allows to enable tracing for many or even all processes in the system,
sleep for a short period of time, then disable tracing for all processes,
avoiding system overload, but keeping profile data.
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec disable_trace(Spec, trace_options()) -> non_neg_integer()
    when Spec :: pid() |
        processes |
        new_processes |
        existing_processes |
        {children | all_children, process()};
    ([process()], trace_options()) -> non_neg_integer() | {non_neg_integer(), [process()]}.
disable_trace(Procs, Options) when Procs =:= processes; Procs =:= new_processes; Procs =:= existing_processes ->
    erlang:trace(Procs, false, trace_options(Options));
disable_trace({Children, PidOrName}, Options) when Children =:= children; Children =:= all_children ->
    Pids = children(Children, PidOrName),
    toggle_trace(Pids, false, trace_options(Options), 0, []);
disable_trace(Pid, Options) when is_pid(Pid); is_atom(Pid) ->
    toggle_process_trace(Pid, false, trace_options(Options));
disable_trace(List, Options) when is_list(List) ->
    toggle_trace(List, false, trace_options(Options), 0, []).

%% @doc Pauses tracing for the entire trace_map
-doc """
Pauses trace collection for all currently traced functions, keeping all traces
intact. Use `continue/0` to resume trace collection.
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec pause() -> ok | not_running.
pause() ->
    gen_server:call(?MODULE, pause, infinity).

%% @doc Continues paused tracing.
-doc "Resumes previously paused profiling.".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec continue() -> ok | not_paused.
continue() ->
    gen_server:call(?MODULE, continue, infinity).

%% @doc Restarts tracing, clearing current statistics. Profiling could be
%%      running or paused.
-doc """
Clears accumulated profiles. If profiling was paused prior to calling `restart`,
it gets continued.
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec restart() -> ok.
restart() ->
    gen_server:call(?MODULE, restart, infinity).

%%--------------------------------------------------------------------
%% Common API

%% @doc Shortcut to transform raw collected data by process, sorted by percent.
-doc """
The same as [`inspect` ](`inspect/3`)`(Profile, process, percent)`. Transforms
raw profile into a map of process identifiers to a tuple containing total count
of words allocated, and a list of all traced functions sorted in the ascending
order by the allocation percentage.
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec inspect({trace_type(), [trace_info()]}) -> #{pid() | all => profile_result()}.
inspect(Profile) ->
    inspect(Profile, process, percent).

%% @doc Transforms raw collected data into shape suitable for analysis and printing.
-doc """
Transforms raw data returned by tracing BIFs into a form convenient for
subsequent analysis and formatting.

When `process` is given as second argument, it returns a map of process
identifiers with corresponding profiling results sorted by the selected column.
When the second argument is `total` or when profiling by `call_count`, the
returned map has a single `all` key with profiling results from all processes.

Inspected profile can be leveraged to
[print profiling results](`m:tprof#inspect_example`).
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec inspect({trace_type(), [trace_info()]}, process | total, sort_by()) ->
    #{pid() | all => profile_result()}.
inspect({Type, Profile}, process, SortBy) ->
    maps:map(
        fun (_Pid, {Total, Stats}) ->
            {Type, Total, inspect_sort(Stats, SortBy)}
        end, inspect_processes(Profile, #{}));
inspect({Type, Profile}, total, SortBy) ->
    GrandTotal = lists:sum([Mem || {_M, _F, _A, Mems} <- Profile, {_P, _C, Mem} <- Mems]),
    TotalStats = [inspect_total(M, F, A, GrandTotal, Mems) || {M, F, A, Mems} <- Profile],
    #{all => {Type, GrandTotal, inspect_sort(TotalStats, SortBy)}}.

%% @doc Formats inspect()-ed totals and per-function data
-doc(#{equiv => format/2}).
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec format(#{pid() | all => profile_result()}) -> ok.
format(Inspected) ->
    format_impl([], Inspected).

-doc "Formats profile transformed with [`inspect` ](`inspect/3`)to a specified device.".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec format(io:device(), #{pid() | all => profile_result()}) -> ok.
format(IoDevice, Inspected) ->
    format_impl(IoDevice, Inspected).

%%--------------------------------------------------------------------
%% Ad-hoc API

%% @doc Runs the function/MFA with heap tracing enabled.
-doc(#{equiv => profile/4}).
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec profile(fun(() -> term())) -> ok | {term(), [trace_info()]}.
profile(Fun) when is_function(Fun) ->
    profile(Fun, #{}).

-doc(#{equiv => profile/4}).
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec profile(fun(() -> term()), profile_options()) -> ok | {term(), [trace_info()]}.
profile(Fun, Options) when is_function(Fun) ->
    do_profile(Fun, Options).

-doc(#{equiv => profile/4}).
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec profile(module(), Fun :: atom(), Args :: [term()]) -> ok | {term(), [trace_info()]}.
profile(Module, Function, Args) when is_atom(Module), is_atom(Function), is_list(Args) ->
    profile(Module, Function, Args, #{}).

-doc """
Produces ad-hoc profile for function `Fun` or `Module`:`Function` call. By
default, result is formatted to the output device, use `report` option to change
this behaviour.

Ad-hoc profiling starts a new instance of `tprof` server, runs the profiling
routine, extracts results and shuts the server down. If `tprof` is already
running (for server-aided profiling), default ad-hoc profiler options block this
call to avoid mixing results from several independent instances. Use
`registered => false` option to override this behaviour.

Ad-hoc profiler supports following`Options`:

- **`type`** - The type of profiling to perform.

- **`device`** - Specifies I/O devices to print the profile to. Useful to
  redirect text output to console or `standard_error`.

- **`pattern`** - Specifies trace pattern, or a list of trace patterns to
  enable. By default, all functions (`{'_', '_', '_'}`) are traced.

- **`registered`** - Specifies `tprof` registered process name. Use `false` to
  leave the process unregistered, or `{local, myname}` to register the process
  under a different name.

- **`report`** - Controls output format. The default is `process`, printing
  per-process profiling data sorted by percentage of the total allocation.
  Specify `report => return` to suppress printing and get the raw data for
  further evaluation with `inspect/3` and formatting with `format/2`.

- **`rootset`** - Includes extra processes in the trace list. Useful for
  profiling allocations for `m:gen_server`, calls, or other allocations caused
  by inter-process communications. See [example](`m:tprof#pg_example`).

- **`set_on_spawn`** - Automatically start tracing for processes spawned by the
  traced process. On by default.

- **`timeout`** - Terminate profiling after the specified amount of time
  (milliseconds).
""".
-doc(#{since => <<"OTP @OTP-18756@">>}).
-spec profile(module(), Fun :: atom(), Args :: [term()], profile_options()) -> ok | {term(), [trace_info()]}.
profile(Module, Function, Args, Options) when is_atom(Module), is_atom(Function), is_list(Args) ->
    do_profile({Module, Function, Args}, Options).

%%--------------------------------------------------------------------
%% gen_server implementation
-record(tprof_state, {
    type = call_count :: trace_type(),
    trace_map = #{} :: trace_map(),
    paused = false :: boolean(),
    ad_hoc = undefined :: undefined |
        {pid(), Timer :: reference() | false, Patterns :: [trace_pattern()],
            RootSet :: rootset(), ReplyTo :: gen_server:from()}
}).

-type state() :: #tprof_state{}.

-doc false.
-spec init(start_options()) -> {ok, state()}.
init(Config) ->
    Type = maps:get(type, Config, call_count),
    false = erlang:process_flag(trap_exit, true), %% need this for reliable terminate/2 call
    {ok, #tprof_state{type=Type}}.

-doc false.
-spec handle_call(term(), gen_server:from(), state()) -> {reply | noreply, term(), state()}.
handle_call({set_pattern, M, F, A}, _From, #tprof_state{trace_map = Map, type = Type} = State) ->
    {Reply, NewMap} = enable_pattern(M, F, A, Map, Type),
    {reply, Reply, State#tprof_state{trace_map = NewMap}};
handle_call({clear_pattern, M, F, A}, _From, #tprof_state{trace_map = Map, type = Type} = State) ->
    {Ret, NewMap} = disable_pattern(M, F, A, Map, Type),
    {reply, Ret, State#tprof_state{trace_map = NewMap}};
handle_call(get_trace_map, _From, #tprof_state{trace_map = Map} = State) ->
    {reply, Map, State};
handle_call(pause, _From, #tprof_state{paused = true} = State) ->
    {reply, not_running, State};
handle_call(pause, _From, #tprof_state{trace_map = Map, paused = false, type = Type} = State) ->
    foreach(Map, pause, Type),
    {reply, ok, State#tprof_state{paused = true}};
handle_call(continue, _From, #tprof_state{paused = false} = State) ->
    {reply, running, State};
handle_call(continue, _From, #tprof_state{trace_map = Map, type = Type} = State) ->
    foreach(Map, true, Type),
    {reply, ok, State#tprof_state{paused = false}};
handle_call(restart, _From, #tprof_state{trace_map = Map, type = Type} = State) ->
    foreach(Map, restart, Type),
    {reply, ok, State#tprof_state{paused = false}};
handle_call(collect, _From, #tprof_state{trace_map = Map, type = Type} = State) ->
    {reply, collect(Map, Type), State};
handle_call({profile, What, Options}, From, #tprof_state{ad_hoc = undefined, trace_map = Map, type = Type} = State) ->
    %% ad-hoc profile routed via gen_server to handle 'EXIT' signal
    {Pid, Timer, Patterns, RootSet, NewMap} = ad_hoc_run(What, Options, Map, Type),
    {noreply, State#tprof_state{ad_hoc = {Pid, Timer, Patterns, RootSet, From}, trace_map = NewMap}};
handle_call({profile, _What, _Options}, _From, State) ->
    {reply, {error, running}, State}.

-doc false.
-spec handle_cast(term(), state()) -> no_return().
handle_cast(_Req, _State) ->
    erlang:error(notsup).

-doc false.
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'EXIT', Pid, Reason}, #tprof_state{ad_hoc = {Pid, Timer, Patterns, RootSet, From},
    trace_map = Map, type = Type} = State) ->
    _ = disable_trace(RootSet),
    Profile = collect(Map, Type),
    gen:reply(From, {Reason, Profile}),
    Timer =/= false andalso erlang:cancel_timer(Timer),
    {noreply, State#tprof_state{ad_hoc = undefined, trace_map = disable_patterns(Patterns, Map, Type)}};

handle_info({cancel, Pid}, #tprof_state{ad_hoc = {Pid, _Timer, Patterns, RootSet, From},
    trace_map = Map, type = Type} = State) ->
    _ = disable_trace(RootSet),
    Profile = collect(Map, Type),
    gen:reply(From, {{'EXIT', timeout}, Profile}),
    {noreply, State#tprof_state{ad_hoc = undefined, trace_map = disable_patterns(Patterns, Map, Type)}}.

-doc false.
-spec terminate(term(), state()) -> ok.
terminate(_Reason, #tprof_state{trace_map = Map, type = Type}) ->
    clear_pattern(Map, Type),
    ok.

%%--------------------------------------------------------------------
%% Internal implementation

-include_lib("kernel/include/logger.hrl").

%% Add the trace of the specified module to the accumulator
collect_trace(Mod, FunList, Acc, Type) ->
    {Fail, Ret} = lists:foldl(
        fun ({Fun, Arity}, {Fail, Prev}) ->
            case combine_trace(erlang:trace_info({Mod, Fun, Arity}, Type)) of
                skip ->
                    {Fail, Prev};
                fail ->
                    {true, Prev};
                Tr ->
                    {Fail, [{Mod, Fun, Arity, Tr} | Prev]}
            end
        end, {false, Acc}, FunList),
    %% module may have been hot-code reloaded, or tracing was broken by something else
    Fail andalso begin
        ?LOG_WARNING(
            "tprof encountered an error tracing module ~s, was it reloaded or untraced?",
            [Mod])
        end,
    Ret.

%% It is possible that due to hot code reload event
%% some function is no longer traced, while it was supposed to.
%% Reinstating tracing automatically is wrong thing to do, because
%% statistics won't be correct anyway. Hence the warning in the user
%% guide, guarding against hot code reload while tracing.
combine_trace({_, false}) -> fail;
combine_trace({call_count, 0}) -> skip;
combine_trace({call_count, Num}) -> [{all, Num, Num}];
combine_trace({call_time, Times}) ->
    case [{Pid, Calls, S * 1000000 + Us} || {Pid, Calls, S, Us} <- Times] of
        [] ->
            skip;
        NonZero ->
            NonZero
    end;
combine_trace({call_memory, Mem}) ->
    case [{Pid, Calls, Words} || {Pid, Calls, Words} <- Mem, Words > 0] of
        [] ->
            skip;
        NonZero ->
            NonZero
    end.

%% Inspection: iterate over collected traces, return map of
%%  #{Pid => [{M, {F, A}, Calls, Measurement, PerCall, Percentage}], unsorted.
inspect_processes([], Acc) ->
    maps:map(
        fun (_Pid, {Total, Lines}) ->
            {Total, [{M, {F, A}, Calls, Measurement, PerCall, divide(Measurement * 100, Total)}
                || {M, F, A, Calls, Measurement, PerCall} <- Lines]}
        end, Acc);
inspect_processes([{_M, _F, _A, []} | Tail], Acc) ->
    inspect_processes(Tail, Acc);
inspect_processes([{M, F, A, [{Pid, Calls, Mem} | TripletTail]} | Tail], Acc) ->
    ProfLine = {M, F, A, Calls, Mem, divide(Mem, Calls)},
    inspect_processes([{M, F, A, TripletTail} | Tail],
        maps:update_with(Pid, fun ({Grand, L}) -> {Grand + Mem, [ProfLine | L]} end, {Mem, [ProfLine]}, Acc)).

%% Inspection: remove Pid information from the Profile, return list of
%%  [{M, F, A, TotalCalls, Measurement, PerCall, Percentage}]
inspect_total(M, F, A, GrandTotal, Mems) ->
    {TC, TM} = lists:foldl(
        fun ({_Pid, Calls, Mem}, {TotalCalls, TotalMem}) ->
            {TotalCalls + Calls, TotalMem + Mem}
        end, {0, 0}, Mems),
    {M, {F, A}, TC, TM, divide(TM, TC), divide(TM * 100, GrandTotal)}.

divide(_,0) -> 0.0;
divide(T,N) -> T/N.

%% Returns "sort by" column index
column(module) -> {1, ascending};
column(function) -> {2, ascending};
column(calls) -> {3, ascending};
column(measurement) -> {4, ascending};
column(measurement_per_call) -> {5, ascending};
column(percent) -> {6, ascending}.

%% Sorts by column name, ascending/descending
inspect_sort(Profile, undefined) ->
    Profile;
inspect_sort(Profile, {Column, ascending}) when is_integer(Column) ->
    lists:keysort(Column, Profile);
inspect_sort(Profile, {Column, descending}) when is_integer(Column) ->
    lists:reverse(lists:keysort(Column, Profile));
inspect_sort(Profile, {Column, Direction}) when is_atom(Column) ->
    {Col, _Skip} = column(Column),
    inspect_sort(Profile, {Col, Direction});
inspect_sort(Profile, Column) when is_atom(Column) ->
    inspect_sort(Profile, column(Column)).

%% Formats the inspected profile to the Device, which could be [] meaning
%%  default output.
format_impl(Device, Empty) when Empty =:= #{} ->
    format_out(Device, "Trace is empty~n", []);
format_impl(Device, #{all := {Type, Total, Inspected}}) ->
    format_each(Device, Type, Total, Inspected);
format_impl(Device, Inspected) when is_map(Inspected) ->
    %% grab the total-total words
    GrandTotal = maps:fold(fun (_Pid, {_Type, Total, _Profile}, Acc) -> Acc + Total end, 0, Inspected),
    %% per-process printout
    maps:foreach(
        fun(Pid, {Type, Total, Each}) ->
            format_out(Device, "~n****** Process ~w  --  ~.2f% of total *** ~n",
                [Pid, divide(100 * Total, GrandTotal)]),
            format_each(Device, Type, Total, Each)
        end, Inspected).

format_each(Device, call_count, _Total, Inspected) ->
    {Widths, Lines} = lists:foldl(
        fun ({Mod, {F, A}, Calls, _Value, _VPC, Percent}, {Widths, Ln}) ->
            Line = [lists:flatten(io_lib:format("~tw:~tw/~w", [Mod, F, A])),
                integer_to_list(Calls), float_to_list(Percent, [{decimals, 2}])],
            NewWidths = [erlang:max(Old, New) || {Old, New} <- lists:zip([string:length(L) || L <- Line], Widths)],
            {NewWidths, [Line | Ln]}
        end, {[0, 5, 5], []}, Inspected),

    %% figure out formatting line
    Fmt = lists:flatten(io_lib:format("~~.~wts  ~~~ws  [~~~ws]~~n", Widths)),
    %% print using this format
    format_out(Device, Fmt, ["FUNCTION", "CALLS", "%"]),
    [format_out(Device, Fmt, Line) || Line <- lists:reverse(Lines)],
    format_out(Device, Fmt, [" ", " ", "100.0"]);
format_each(Device, call_time, Total, Inspected) ->
    format_labelled(Device, "TIME (μs)", Total, Inspected);
format_each(Device, call_memory, Total, Inspected) ->
    format_labelled(Device, "WORDS", Total, Inspected).

format_labelled(Device, Label, Total, Inspected) ->
    %% The width of the value is either total or label
    ValueWidth = erlang:max(length(Label), length(integer_to_list(Total))),

    {Widths, Lines} = lists:foldl(
        fun ({Mod, {F, A}, Calls, Value, VPC, Percent}, {Widths, Ln}) ->
            Line = [lists:flatten(io_lib:format("~tw:~tw/~w", [Mod, F, A])),
                integer_to_list(Calls), integer_to_list(Value),
                float_to_list(VPC, [{decimals, 2}]),
                float_to_list(Percent, [{decimals, 2}])],
            NewWidths = [erlang:max(Old, New) || {Old, New} <- lists:zip([string:length(L) || L <- Line], Widths)],
            {NewWidths, [Line | Ln]}
        end, {[0, 5, ValueWidth, 8, 5], []}, Inspected),

    %% figure out formatting line
    Fmt = lists:flatten(io_lib:format("~~.~wts  ~~~ws  ~~~wts  ~~~ws  [~~~ws]~~n", Widths)),
    %% print using this format
    format_out(Device, Fmt, ["FUNCTION", "CALLS", Label, "PER CALL", "%"]),
    [format_out(Device, Fmt, Line) || Line <- lists:reverse(Lines)],
    format_out(Device, Fmt, [" ", " ", integer_to_list(Total), " ", "100.0"]).

%% format implementation that uses [] as a way to tell "default output"
format_out([], Fmt, Args) ->
    io:format(Fmt, Args);
format_out(Device, Fmt, Args) ->
    io:format(Device, Fmt, Args).

%% pattern collapse code
enable_pattern('_', '_', '_', _Acc, Type) ->
    %% need to re-trace everything, probably some new modules were loaded
    %% discard any existing trace pattern
    erlang:trace_pattern(on_load, true, [Type]),
    {erlang:trace_pattern({'_', '_', '_'}, true, [Type]), all};
enable_pattern(Mod, '_', '_', Acc, Type) ->
    %% code may have been hot-loaded, redo the trace
    case erlang:trace_pattern({Mod, '_', '_'}, true, [Type]) of
        0 ->
            {{error, {trace_pattern, Mod, '_', '_'}}, Acc};
        Traced ->
            {Traced, update_trace_map(Acc, fun() -> Acc#{Mod => Mod:module_info(functions)} end)}
    end;
enable_pattern(Mod, Fun, '_', Acc, Type) ->
    case erlang:trace_pattern({Mod, Fun, '_'}, true, [Type]) of
        0 ->
            {{error, {trace_pattern, Mod, Fun, '_'}}, Acc};
        Traced ->
            {Traced,
             update_trace_map(
               Acc,
               fun() ->
                       Added = [{F, A} || {F, A} <- Mod:module_info(functions), F =:= Fun],
                       maps:update_with(
                         Mod,
                         fun (FAs) ->
                                 Added ++ [{F, A} || {F, A} <- FAs, F =/= Fun]
                         end, Added, Acc)
               end)}
    end;
enable_pattern(Mod, Fun, Arity, Acc, Type) ->
    case erlang:trace_pattern({Mod, Fun, Arity}, true, [Type]) of
        0 ->
            {{error, {trace_pattern, Mod, Fun, Arity}}, Acc};
        1 ->
            {1, update_trace_map(
                  Acc,
                  fun() ->
                          maps:update_with(
                            Mod,
                            fun(FAs) ->
                                    [{Fun, Arity} | FAs -- [{Fun, Arity}]]
                            end, [{Fun, Arity}], Acc)
                  end)}
    end.

update_trace_map(all, _) ->
    all;
update_trace_map(_Map, Fun) ->
    Fun().

%% pattern collapse code for un-tracing
disable_pattern('_', '_', '_', _Acc, Type) ->
    erlang:trace_pattern(on_load, false, [Type]),
    Traced = erlang:trace_pattern({'_', '_', '_'}, false, [Type]),
    {Traced, #{}};
disable_pattern(Mod, '_', '_', Acc, Type) when is_map_key(Mod, Acc); Acc =:= all ->
    Traced = erlang:trace_pattern({Mod, '_', '_'}, false, [Type]),
    {Traced, update_trace_map(Acc, fun() -> maps:remove(Mod, Acc) end)};
disable_pattern(Mod, Fun, '_', Acc, Type) when is_map_key(Mod, Acc); Acc =:= all ->
    Traced = erlang:trace_pattern({Mod, Fun, '_'}, false, [Type]),
    {Traced, update_trace_map(
               Acc,
               fun() ->
                       maps:update_with(
                         Mod,
                         fun (FAs) -> [{F, A} || {F, A} <- FAs, F =/= Fun] end,
                         Acc)
               end)};
disable_pattern(Mod, Fun, Arity, Acc, Type) when is_map_key(Mod, Acc); Acc =:= all ->
    Traced = erlang:trace_pattern({Mod, Fun, Arity}, false, [Type]),
    {Traced, update_trace_map(
               Acc,
               fun() ->
                       maps:update_with(Mod, fun (FAs) -> FAs -- [{Fun, Arity}] end, Acc)
               end)};
disable_pattern(Mod, Fun, Arity, Acc, _Type) ->
    {{error, {not_traced, Mod, Fun, Arity}}, Acc}.

disable_patterns(Patterns, Map, Type) ->
    lists:foldl(
        fun ({M, F, A}, Acc) -> {_, New} = disable_pattern(M, F, A, Acc, Type), New end,
        Map,
        Patterns
    ).

%% ad-hoc profiler implementation
do_profile(What, Options) ->
    %% start a new tprof server, potentially registered to a new name
    Pid = start_result(start_internal(Options)),
    try
        {Ret, Profile} = gen_server:call(Pid, {profile, What, Options}, infinity),
        return_profile(maps:get(report, Options, process), Profile, Ret,
            maps:get(device, Options, []))
    after
        gen_server:stop(Pid)
    end.

start_internal(Options) ->
    case maps:get(registered, Options, {local, ?MODULE}) of
        false -> gen_server:start_link(?MODULE, Options, []);
        {local, Name} -> gen_server:start_link({local, Name}, ?MODULE, Options, [])
    end.

start_result({ok, Pid}) -> Pid;
start_result({error, Reason}) -> erlang:error(Reason).

return_profile(return, Profile, Ret, _Device) ->
    {Ret, Profile};
return_profile(process, Profile, Ret, Device) ->
    return_profile({process, percent}, Profile, Ret, Device);
return_profile(total, Profile, Ret, Device) ->
    return_profile({total, percent}, Profile, Ret, Device);
return_profile({Agg, Sort}, Profile, _Ret, Device) ->
    format_impl(Device, inspect(Profile, Agg, Sort)).

%% @doc clears tracing for the entire trace map passed
-spec clear_pattern(trace_map() | all, trace_type()) -> ok.
clear_pattern(all, Type) ->
    erlang:trace_pattern(on_load, false, [Type]),
    erlang:trace_pattern({'_', '_', '_'}, false, [Type]);
clear_pattern(Existing, Type) ->
    maps:foreach(
      fun(Mod, FunArity) ->
              [erlang:trace_pattern({Mod, F, A}, false, [Type]) || {F, A} <- FunArity]
      end, Existing).

trace_options(#{set_on_spawn := false}) ->
    [call, silent];
trace_options(_) ->
    [call, silent, set_on_spawn].

children(Children, PidOrName) when is_atom(PidOrName) ->
    case erlang:whereis(PidOrName) of
        undefined -> [];
        Pid -> children(Children, Pid)
    end;
children(children, Pid) when is_pid(Pid) ->
    [P || P <- erlang:processes(), erlang:process_info(P, parent) =:= {parent, Pid}];
children(all_children, Pid) when is_pid(Pid) ->
    %% build a process tree (could use a digraph too)
    Tree = maps:groups_from_list(
        fun (P) ->
            case erlang:process_info(P, parent) of
                undefined -> undefined;
                {parent, Parent} -> Parent
            end
        end, erlang:processes()),
    select_pids(Tree, Pid).

select_pids(Tree, Pid) ->
    case maps:find(Pid, Tree) of
        error -> [];
        {ok, Children} ->
            Children ++ lists:concat([select_pids(Tree, C) || C <- Children])
    end.

toggle_process_trace(Pid, On, Flags) when is_pid(Pid) ->
    try
        1 = erlang:trace(Pid, On, Flags)
    catch _:_ ->
        0
    end;
toggle_process_trace(Name, On, Flags) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            0;
        Pid ->
            toggle_process_trace(Pid, On, Flags)
    end.

toggle_trace([], _On, _Flags, Success, []) ->
    Success;
toggle_trace([], _On, _Flags, Success, Failure) ->
    {Success, lists:reverse(Failure)};
toggle_trace([Pid | Tail], On, Flags, Success, Failure) when is_pid(Pid) ->
    {NS, NF} =
        try
            1 = erlang:trace(Pid, On, Flags),
            {Success + 1, Failure}
        catch _:_ ->
            {Success, [Pid | Failure]}
        end,
    toggle_trace(Tail, On, Flags, NS, NF);
toggle_trace([Name | Tail], On, Flags, Success, Failure) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            toggle_trace(Tail, On, Flags, Success, [Name | Failure]);
        Pid ->
            {NS, NF} =
                try
                    1 = erlang:trace(Pid, On, Flags),
                    {Success + 1, Failure}
                catch _:_ ->
                    {Success, [Name | Failure]}
                end,
            toggle_trace(Tail, On, Flags, NS, NF)
    end.

%% @doc Collects memory tracing data (usable for inspect()) for
%%      all traced functions.
-spec collect(trace_map(), trace_type()) -> {trace_type(), [trace_info()]}.
collect(all, Type) ->
    collect(
      #{ Mod => Mod:module_info(functions) || {Mod, _} <- code:all_loaded() },
      Type);
collect(Pattern, Type) ->
    {Type, maps:fold(fun(K, V, Acc) -> collect_trace(K, V, Acc, Type) end, [], Pattern)}.

foreach(Map, Action, Type) ->
    maps:foreach(
        fun (Mod, Funs) ->
            [erlang:trace_pattern({Mod, F, A}, Action, [Type]) || {F, A} <- Funs]
        end, Map).

ad_hoc_run(What, Options, Map, Type) ->
    %% add missing patterns
    Patterns = make_list(maps:get(pattern, Options, {'_', '_', '_'})),
    NewMap = lists:foldl(
        fun({M, F, A}, Acc) ->
            {_, NewMap} = enable_pattern(M, F, A, Acc, Type),
            NewMap
        end, Map, Patterns),
    %% check whether spawned processes are also traced
    OnSpawn = maps:get(set_on_spawn, Options, true),
    %% enable tracing for items in the rootset
    RootSet = maps:get(rootset, Options, []),
    _ = enable_trace(RootSet), %% ignore errors when setting up rootset trace
    %% spawn a separate process to run the user-supplied MFA
    %% if RootSet is 'processes' or 'new_processes', skip the trace flags
    Flags = trace_flags(RootSet, OnSpawn),
    Pid = spawn_profiled(What, Flags),
    %% start timer to terminate the function being profiled if it takes too long
    %%  to complete
    Timer = is_map_key(timeout, Options) andalso
        erlang:send_after(maps:get(timeout, Options), self(), {cancel, Pid}),
    {Pid, Timer, Patterns, RootSet, NewMap}.

trace_flags(processes, _) -> [];
trace_flags(new_processes, _) -> [];
trace_flags(_, true) -> [call, silent, set_on_spawn];
trace_flags(_, false) -> [call, silent].

make_list({M, F, A}) -> [{M, F, A}];
make_list(List) -> List.

spawn_profiled(Fun, Flags) when is_function(Fun) ->
    spawn_link(
        fun() ->
            Flags =/= [] andalso begin 1 = erlang:trace(self(), true, Flags) end,
            Ret = catch Fun(),
            Flags =/= [] andalso begin 1 = erlang:trace(self(), false, Flags) end,
            exit(Ret)
        end);
spawn_profiled({M, F, A}, Flags) ->
    spawn_link(
        fun() ->
            Flags =/= [] andalso begin 1 = erlang:trace(self(), true, Flags) end,
            Ret = catch erlang:apply(M, F, A),
            Flags =/= [] andalso begin 1 = erlang:trace(self(), false, Flags) end,
            exit(Ret)
        end).
