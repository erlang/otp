%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright WhatsApp Inc. and its affiliates. All rights reserved.
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
%%-------------------------------------------------------------------
%%
%% @author Maxim Fedorov <maximfca@gmail.com>
%% Erlang Process Tracing profiler.
%%
-module(tprof).
-moduledoc """
Process Tracing Profiling Tool

`tprof` provides convenience helpers for Erlang process profiling using
the trace BIFs.

> #### Warning {: .warning }
>
> This module aims to replace `eprof` and `cprof` into a unified API for
> measuring call count, time, and allocation. It is experimental in Erlang/OTP
> 27.0.

It is possible to analyze the number of calls, the time spent by function, and
heap allocations by function. Profiling can be done [ad-hoc](#module-ad-hoc-profiling)
 or run in a [server-aided mode](#module-server-aided-profiling) for deeper
introspection of the code running in production. The server-aided mode can be
run using the default tprof server or an isolated `t:server/0` started through
[`start(#{ session => atom() })`](`start/1`).

There are [three kinds of profiling](`t:trace_type/0`) supported by this module:

- `call_count`
- `call_time`
- `call_memory`

The default is `call_count`, which has the smallest performance impact
and memory footprint, but it does not support per-process
profiling. For this reason, all of the examples below uses
`call_memory`, which measures heap allocation, and provide a more complex
feature set to demonstrate.

Erlang terms that do not fit in a single machine word are allocated on
the process heap. For example, a function returning a tuple of two
elements needs to allocate the tuple on the process heap. The actual
consumption is three words, because the runtime systems also need an
extra word to store the tuple size.

> #### Note {: .info }
>
> Expect a slowdown in the program execution when profiling is enabled.
>
> For profiling convenience, measurements are accumulated for functions that are
> not enabled in some trace pattern. Consider this call stack example:
>
> ```text
> top_traced_function(...)
> not_traced_function()
> bottom_traced_function()
> ```
>
> Allocations that happened within `not_traced_function` will be added to
> the allocations for `top_traced_function`. However, allocations that occurred
> within `bottom_traced_function` are not included in the `top_traced_function`.
> To only keep track of each function own allocations, it is necessary to
> trace all functions.

> #### Warning {: .warning }
>
> Avoid hot code reloading for modules participating in the tracing.
> Reloading a module disables tracing and discards the accumulated statistics.
> The `tprof` results will probably be incorrect when the profiled code was
> reloading during a profiling session.

## Ad-hoc profiling

Ad-hoc profiling is convenient for profiling a single function call.

For example:

```erlang
1> tprof:profile(lists, seq, [1, 16], #{type => call_memory}).

****** Process <0.92.0>  --  100.00% of total *** 
FUNCTION          CALLS  WORDS  PER CALL  [     %]
lists:seq_loop/3      5     32      6.40  [100.00]
                            32            [ 100.0]
ok
```

By default tracing is enabled for all functions in all modules. When funs
are created in the interactive shell, parts of shell code are also traced:

```erlang
1> tprof:profile(fun() -> lists:seq(1, 16) end, #{type => call_memory}).

****** Process <0.95.0>  --  100.00% of total *** 
FUNCTION                   CALLS  WORDS  PER CALL  [    %]
erl_eval:do_apply/7            1      3      3.00  [ 3.61]
erl_eval:match_list/6          1      3      3.00  [ 3.61]
lists:reverse/1                1      4      4.00  [ 4.82]
erl_eval:expr_list/7           3      7      2.33  [ 8.43]
erl_eval:ret_expr/3            4     16      4.00  [19.28]
erl_eval:merge_bindings/4      3     18      6.00  [21.69]
lists:seq_loop/3               5     32      6.40  [38.55]
                                     83            [100.0]
ok
```

However, it is possible to limit the trace to specific functions or modules:

```erlang
2> tprof:profile(fun() -> lists:seq(1, 16) end,
                 #{type => call_memory, pattern => [{lists, seq_loop, '_'}]}).
****** Process <0.98.0>  --  100.00% of total *** 
FUNCTION          CALLS  WORDS  PER CALL  [     %]
lists:seq_loop/3      5     32      6.40  [100.00]
                            32            [ 100.0]

ok
```

Ad-hoc profiling results can be printed in a few different ways. The following
examples use the `test` module defined like this:

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

By default per-process statistics is shown:

```erlang
1> tprof:profile(test, test_spawn, [], #{type => call_memory}).

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

The following example prints the combined memory allocation of all
processes, sorted by the total number of allocated words in descending
order:

```erlang
2> tprof:profile(test, test_spawn, [],
                 #{type => call_memory, report => {total, {measurement, descending}}}).

FUNCTION                CALLS  WORDS  PER CALL  [    %]
lists:seq_loop/3            9     64         7  [68.82]
test:test_spawn/0           1     14        14  [15.05]
erlang:apply/2              1      7         7  [ 7.53]
erlang:spawn_opt/4          1      6         6  [ 6.45]
erlang:spawn_monitor/1      1      2         2  [ 2.15]
                                  93            [100.0]
```

The profiling data can also be collected for further inspection:

```erlang
3> {done, ProfileData} = tprof:profile(fun test:test_spawn/0,
                                       #{type => call_memory, report => return}).
<...>
4> tprof:format(tprof:inspect(ProfileData, process, {percent, descending})).

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

Which processes that are profiled depends on the profiling type.

* `call_count` (default) counts calls in all processes.

* `call_time` and `call_memory` limits the profiling to the processes
  spawned from the user-provided function (using the `set_on_spawn`
  option for `trace:process/4`).

`call_time` and `call_memory` can be restricted to profile a single process:

```erlang
2> tprof:profile(test, test_spawn, [],
                 #{type => call_memory, set_on_spawn => false}).

****** Process <0.183.0>    -- 100.00 % of total allocations ***
FUNCTION                CALLS  WORDS  PER CALL  [    %]
erlang:spawn_monitor/1      1      2         2  [ 9.09]
erlang:spawn_opt/4          1      6         6  [27.27]
test:test_spawn/0           1     14        14  [63.64]
```

[](){: #pg_example }

Erlang programs can perform expensive operations in other processes
than the original one. You can include multiple, new, or even all
processes in the trace when measuring time or memory:

```erlang
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

By default, there is no limit for the profiling time. For ad-hoc
profiling, it is possible to configure a time limit. If the profiled
function does not return before that time expires, the process is
terminated with reason `kill`. Any unlinked children processes started
by the user-supplied function are kept; it is the responsibility of
the developer to take care of such processes.

```erlang
9> tprof:profile(timer, sleep, [100000], #{timeout => 1000}).
```

By default, only one ad-hoc or server-aided profiling session is
allowed at any point in time. It is possible to force multiple ad-hoc
sessions concurrently, but it is the responsibility of the developer
to ensure that trace patterns do not overlap:

```erlang
1> tprof:profile(fun() -> lists:seq(1, 32) end,
    #{registered => false, pattern => [{lists, '_', '_'}]}).
```

## Server-aided profiling

Server-aided profiling can be done on a system that is up and
running. To do that, start the `tprof` server, and then add trace
patterns and processes to trace while the system handles actual
traffic. Data can extracted, inspected, and printed at any time. The
following example traces activity of all processes supervised by
the Kernel supervisor:

```erlang
1> tprof:start(#{type => call_memory}).
{ok,<0.200.0>}
2> tprof:enable_trace({all_children, kernel_sup}).
34
3> tprof:set_pattern('_', '_' , '_').
16728
4> Sample = tprof:collect().
{call_memory,
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

```erlang
1> tprof:start(#{type => call_memory}).
2> tprof:enable_trace(all), tprof:set_pattern('_', '_' , '_').
9041
3> timer:sleep(10000), tprof:disable_trace(all), Sample = tprof:collect().
{call_memory,
    [{user_drv,server,3,[{<0.64.0>,12,136}]},
     {user_drv,contains_ctrl_g_or_ctrl_c,1,[{<0.64.0>,80,10}]},
<...>
4> Inspected = tprof:inspect(Sample, process, measurement), Shell = maps:get(self(), Inspected).
{call_memory, 2743,
    [{shell,{enc,0},1,2,2,0.07291286912139992},
<...>
5> tprof:format(Shell).

FUNCTION                           CALLS  WORDS  PER CALL  [    %]
<...>
erl_lint:start/2                       2    300       150  [10.94]
shell:used_records/1                 114    342         3  [12.47]
```
""".
-moduledoc(#{since => "OTP 27.0"}).

%% API
-export([
    start/0, start/1,
    start_link/0, start_link/1,
    stop/0, stop/1,
    set_pattern/3, set_pattern/4,
    clear_pattern/3, clear_pattern/4,
    get_trace_map/0, get_trace_map/1,
    enable_trace/1, enable_trace/2, enable_trace/3,
    disable_trace/1, disable_trace/2, disable_trace/3,
    pause/0,pause/1,
    continue/0,continue/1,
    restart/0,restart/1,
    collect/0,collect/1,
    %% ad-hoc profiling
    profile/1, profile/2, profile/3, profile/4,
    %% Analysis API
    inspect/1, inspect/3,
    format/1, format/2,
    %% Used for testing
    get_session/1
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
-type start_options() :: #{type => trace_type(), session => atom()}.

-doc """
A tprof server.

Each server uses a separate `t:trace:session/0` in order to
keep profiling isolated.
""".
-type server() :: pid() | 'tprof'.

%% Trace type
-doc """
The type of profiling that the tprof server will do.

- **call_count** - Counts the number of calls made to functions. This
  is a global profiling event that cannot be limited to specific processes.
  See [call_count](`trace#call_count`) in `trace:function/4` for more details.
- **call_time** - Counts the accumulated time spent in functions.
  See [call_time](`trace#call_time`) in `trace:function/4` for more details.
- **call_memory** - Counts the accumulated memory allocated in functions.
  See [call_memory](`trace#call_memory`) in `trace:function/4` for more details.
""".
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
Column to sort by `inspect/3` or [`profile/4`](`profile/4`).

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
-doc "Options for enabling profiling of the selected processes; see `enable_trace/2`.".
-type trace_options() :: #{
    set_on_spawn => boolean()
}.

%% Convenience type to define which processes to trace
-type rootset() :: [process()] |   %% list of pids/registered names
    all |
    existing |
    new.

-doc "Ad-hoc profiler options; see [`profile/4`](`profile/4`).".
-type profile_options() ::
        #{
          type => trace_type(),                           %% the type of profiling
          timeout => timeout(),                           %% stop profiling after the timeout
          pattern => trace_pattern() | [trace_pattern()], %% list of patterns to trace
          set_on_spawn => boolean(),                      %% trace spawned processes or not (true by default)
          rootset => rootset(),                           %% extra processes to trace
          report => return | process | total | {process, sort_by()} | {total, sort_by()},   %% print or return results
          device => io:device()                          %% device to report to
         }.

%%--------------------------------------------------------------------
%% Server-aided API
-doc(#{equiv => start(#{})}).
-spec start() -> {'ok', Server} | {'error', Reason} when Server :: server(), Reason :: {'already_started', pid()}.
start() ->
    start(#{}).

-doc """
Starts the server, not supervised.

Profiling server stores current trace patterns and owns the [trace session](`t:trace:session/0`)
used for profiling.

If no `session` is provided in `Config`, then a default session called `tprof` is
used and the profiling server is [registered](`register/2`) as `tprof`.

If `session` is provided in `Config`, then a session with that name is created
and all profiling is done within that session. The profiling server is not [registered](`register/2`)
in this case. When using `m:tprof` like this the `t:pid/0` returned from this
function needs to be provided to the functions in this module.
""".
-spec start(Config :: start_options()) -> {'ok', Server} | {'error', Reason}
        when Server :: server(), Reason :: {'already_started', pid()}.
start(#{ session := _ } = Config) when is_map(Config) ->
    gen_server:start(?MODULE, Config, []);
start(Config) when is_map(Config) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Config, []).

-doc(#{equiv => start_link(#{}) }).
-spec start_link() -> {'ok', Server} | {'error', Reason} when
      Server :: server(), Reason :: {'already_started', pid()}.
start_link() ->
    start_link(#{}).

-doc "Equivalent to `start/1` but also links the profiling server to the caller.".
-spec start_link(Config :: start_options()) -> {'ok', Server} | {'error', Reason}
        when Server :: server(), Reason :: {'already_started', pid()}.
start_link(#{ session := _ } = Config) when is_map(Config) ->
    gen_server:start_link(?MODULE, Config, []);
start_link(Config) when is_map(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

-doc "Stops the default `tprof` server and disable tracing enabled by the server.".
-spec stop() -> ok.
stop() ->
    stop(?MODULE).

-doc "Equivalent to `stop/0` but uses the provided `Server`.".
-spec stop(server()) -> ok.
stop(Server) ->
    gen_server:stop(Server).

-doc """
Enables tracing for all functions matching the supplied pattern.

Patterns are additive, following the same rules as `trace:function/4`.
Returns the number of functions matching the supplied pattern.

```erlang
1> tprof:set_pattern(lists, seq, '_').
2
2> tprof:set_pattern(lists, keyfind, 3).
1
3> tprof:get_trace_map().
#{lists => [{keyfind,3},{seq,2},{seq,3}]}
```

If no functions match the pattern, an `error` tuple is returned:

```erlang
> tprof:set_pattern(no_module, func, '_').
{error,{trace_pattern,no_module,func,'_'}}
```

Requires that the default `tprof` server has been [`started`](`start/1`).
""".
-spec set_pattern(module(), atom(), arity() | '_') -> ok | {error, {trace_pattern, trace_pattern()}}.
set_pattern(Mod, Fun, Arity) ->
    set_pattern(?MODULE, Mod, Fun, Arity).

-doc "Equivalent to `set_pattern/3` but uses the provided `Server`.".
-spec set_pattern(server(), module(), atom(), arity() | '_') -> ok | {error, {trace_pattern, trace_pattern()}}.
set_pattern(Server, Mod, Fun, Arity) ->
    gen_server:call(Server, {set_pattern, Mod, Fun, Arity}, infinity).

-doc """
Disables tracing functions matching the supplied pattern.

```erlang
1> tprof:set_pattern(lists, seq, '_').
2
2> tprof:clear_pattern(lists, seq, 3).
1
3> tprof:get_trace_map().
#{lists => [{seq,2}]}
```

Requires that the default `tprof` server has been [`started`](`start/1`).
""".
-spec clear_pattern(module(), atom(), arity() | '_') -> ok.
clear_pattern(Mod, Fun, Arity) ->
    clear_pattern(?MODULE, Mod, Fun, Arity).

-doc """
Equivalent to [`clear_pattern(Mod, Fun, Arity)`](`clear_pattern/3`) but uses the provided `Server`.
""".
-spec clear_pattern(server(), module(), atom(), arity() | '_') -> ok.
clear_pattern(Server, Mod, Fun, Arity) ->
    gen_server:call(Server, {clear_pattern, Mod, Fun, Arity}, infinity).

-doc "Returns a map of module names to functions with their arities.".
-spec get_trace_map() -> trace_map().
get_trace_map() ->
    get_trace_map(?MODULE).

-doc "Equivalent to `get_trace_map/0` but uses the provided `Server`.".
-spec get_trace_map(server()) -> trace_map().
get_trace_map(Server) ->
    gen_server:call(Server, get_trace_map).

-doc "Returns statistics for current trace map.".
-spec collect() -> {trace_type(), [trace_info()]}.
collect() ->
    collect(?MODULE).

-doc "Equivalent to `collect/0` but uses the provided `Server`.".
-spec collect(server()) -> {trace_type(), [trace_info()]}.
collect(Server) ->
    gen_server:call(Server, collect, infinity).

%% Only used for testing
-doc false.
-spec get_session(server()) -> trace:session().
get_session(Server) ->
    gen_server:call(Server, get_session).

-doc "A process identifier (pid) or a registered process name.".
-type process() :: pid() | atom().

-doc #{equiv => enable_trace(Spec, #{set_on_spawn => true})}.
-spec enable_trace(Spec) -> non_neg_integer() when
      Spec :: pid() | all | new | existing |
              {children | all_children, process()};
                  ([process()]) -> non_neg_integer() | {non_neg_integer(), [process()]}.
enable_trace(Rootset) ->
    enable_trace(Rootset, default_trace_options()).

-doc """
Similar to `trace:process/4`, but supports a few more options for tracing
convenience.

Tracing per process is not supported by `call_count` profilers.

`Spec` is either a process identifier (pid) for a local process, one of the
following atoms, or a list of local process identifiers or their registered
names:

- **`all`** - All currently existing processes and all that will be
  created in the future.

- **`existing`** - All currently existing processes.

- **`new`** - All processes that will be created in the future.

- **`children`** - All currently running processes that were directly spawned by
  the specified process. This mode is helpful for tracing workers of a single
  supervisor.

- **`all_children`** - All currently running processes that were spawned by the
  specified process, or any recursive descendant of it. This mode is designed to
  facilitate tracing of supervision trees.

Returns the number of processes for which tracing was enabled.

When a list of pids, `children` or `all_children` is used, the processes that
tracing failed to be enabled on will also be returned. Tracing can fail to be
enabled if the process has terminated before tracing could be enabled.

> #### Note {: .info }
>
> The profiling server does not keep track of processes that were added to the
> tracing set. It is permitted to stop the profiling server (wiping out any
> accumulated data), restart the server, set entirely different tracing pattern
> keeping the list of traced processes for future use. Use
> [`disable_trace(Processes)`](`disable_trace/2`) to clear the list of traced
> processes.

Specify `Options` to modify tracing behavior:

- **`set_on_spawn`** - Automatically start tracing for processes spawned by the
  traced process. On by default.
""".
-spec enable_trace(Spec, trace_options()) -> Traced :: non_neg_integer()
              when Spec :: pid() | all | new | existing;
                  (Spec, trace_options()) -> Traced :: non_neg_integer() | {Traced :: non_neg_integer(), Failed :: [process()]}
              when Spec :: [process()] | {children | all_children, process()}.
enable_trace(Spec, Options) ->
    enable_trace(?MODULE, Spec, Options).

-doc "Equivalent to `enable_trace/2` but uses the provided `Server`.".
-spec enable_trace(server(), Spec, trace_options()) -> non_neg_integer()
              when Spec :: pid() | all | new | existing |
                           {children | all_children, process()};
                  (server(), [process()], trace_options()) -> non_neg_integer() | {non_neg_integer(), [process()]}.
enable_trace(Server, Spec, Options) ->
    enable_session_trace(get_session(Server), Spec, Options).

enable_session_trace(Session, Spec) ->
    enable_session_trace(Session, Spec, default_trace_options()).
enable_session_trace(Session, Procs, Options) when Procs =:= all; Procs =:= new; Procs =:= existing ->
    trace:process(Session, Procs, true, trace_options(Options));
enable_session_trace(Session, {Children, PidOrName}, Options) when Children =:= children; Children =:= all_children ->
    Pids = children(Children, PidOrName),
    toggle_trace(Session, Pids, true, trace_options(Options), 0, []);
enable_session_trace(Session, Pid, Options) when is_pid(Pid); is_atom(Pid) ->
    toggle_process_trace(Session, Pid, true, trace_options(Options));
enable_session_trace(Session, List, Options) when is_list(List) ->
    toggle_trace(Session, List, true, trace_options(Options), 0, []).

-doc #{equiv => disable_trace(Spec, #{set_on_spawn => true})}.
-spec disable_trace(Spec) -> non_neg_integer()
              when Spec :: pid() | all | new | existing |
                           {children | all_children, process()};
                   ([process()]) -> non_neg_integer() | {non_neg_integer(), [process()]}.
disable_trace(Rootset) ->
    disable_trace(Rootset, default_trace_options()).

-doc """
Stops accumulating traces for specified processes.

See `enable_trace/2` for a description of the options.

The profile data accumulated before the process is removed from the
traced list is retained. This makes it possible to enable tracing for
many or all processes in the system, sleep for a short period of
time, then disable tracing for all processes (to avoid system
overload), but keeping profile data.
""".
-spec disable_trace(Spec, trace_options()) -> non_neg_integer() when
      Spec :: pid() | all | new | existing | {children | all_children, process()};
    ([process()], trace_options()) -> non_neg_integer() | {non_neg_integer(), [process()]}.
disable_trace(Spec, Options) ->
    disable_trace(?MODULE, Spec, Options).

-spec disable_trace(server(), Spec, trace_options()) -> non_neg_integer()
              when Spec :: pid() | all | new | existing |
                           {children | all_children, process()};
                   (server(),[process()], trace_options()) -> non_neg_integer() | {non_neg_integer(), [process()]}.
disable_trace(Server, Spec, Options) ->
    disable_session_trace(get_session(Server), Spec, Options).

disable_session_trace(Session, Procs) ->
    disable_session_trace(Session, Procs, default_trace_options()).
disable_session_trace(Session, Procs, Options) when Procs =:= all;
                                                    Procs =:= new_processes;
                                                    Procs =:= existing_processes ->
    trace:process(Session,  Procs, false, trace_options(Options));
disable_session_trace(Session, {Children, PidOrName}, Options) when Children =:= children;
                                                                    Children =:= all_children ->
    Pids = children(Children, PidOrName),
    toggle_trace(Session, Pids, false, trace_options(Options), 0, []);
disable_session_trace(Session, Pid, Options) when is_pid(Pid); is_atom(Pid) ->
    toggle_process_trace(Session, Pid, false, trace_options(Options));
disable_session_trace(Session, List, Options) when is_list(List) ->
    toggle_trace(Session, List, false, trace_options(Options), 0, []).


-doc """
Pauses trace collection for all currently traced functions, retaining existing traces.

Use `continue/0` to resume trace collection.
""".
-spec pause() -> ok | not_running.
pause() ->
    pause(?MODULE).

-doc "Equivalent to `pause/0` but uses the provided `Server`.".
-spec pause(server()) -> ok | not_running.
pause(Server) ->
    gen_server:call(Server, pause, infinity).

-doc "Resumes previously paused profiling.".
-spec continue() -> ok | not_paused.
continue() ->
    continue(?MODULE).

-doc "Equivalent to `continue/0` but uses the provided `Server`.".
-spec continue(server()) -> ok | not_paused.
continue(Server) ->
    gen_server:call(Server, continue, infinity).

-doc """
Clears accumulated profiles and starts profiling if it was paused.
""".
-spec restart() -> ok.
restart() ->
    restart(?MODULE).

-doc "Equivalent to `restart/0` but uses the provided `Server`.".
-spec restart(server()) -> ok.
restart(Server) ->
    gen_server:call(Server, restart, infinity).

%%--------------------------------------------------------------------
%% Common API

-doc """
Equivalent to [`inspect(Profile, process, percent)`](`inspect/3`).

Transforms raw profile into a map of process identifiers to a tuple containing total count
of words allocated, and a list of all traced functions sorted in the ascending
order by the allocation percentage.
""".
-spec inspect({trace_type(), [trace_info()]}) -> #{all => profile_result()}.
inspect(Profile) ->
    inspect(Profile, process, percent).

-doc """
Transforms raw data returned by tracing BIFs into a form convenient for
subsequent analysis and formatting.

* When the `Type` argument is `process`, this function returns a map of process
  identifiers with corresponding profiling results sorted by the selected column.

* When `Type` argument is `total` or when profiling by `call_count`, this function
  returns a map with a single `all` key with profiling results from all processes.

The inspected profile data can be leveraged to
[print profiling results](`m:tprof#inspect_example`).
""".
-spec inspect(Profile :: {trace_type(), [trace_info()]}, Type :: process | total,
              SortBy :: sort_by()) ->
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

-doc """
Formats profile data transformed with [`inspect/3`](`inspect/3`), outputting to
the default output device.

""".
-spec format(profile_result() | #{pid() | all => profile_result()}) -> ok.
format(Inspected) ->
    format_impl([], Inspected).

-doc """
Formats profile transformed with [`inspect/3`](`inspect/3`),
outputting to device `IoDevice`.
""".
-spec format(io:device(), profile_result() | #{pid() | all => profile_result()}) -> ok.
format(IoDevice, Inspected) ->
    format_impl(IoDevice, Inspected).

%%--------------------------------------------------------------------
%% Ad-hoc API

-doc #{equiv => profile(Fun, #{})}.
-spec profile(fun(() -> term())) -> ok | {term(), [trace_info()]}.
profile(Fun) when is_function(Fun) ->
    profile(Fun, #{}).

-doc """
Does ad-hoc profiling of the call `Fun()`.

By default, the result is formatted to the output device; use the `report`
option to change this behavior.

Ad-hoc profiling starts a new instance of `tprof` server, runs the
profiling routine, extracts results, and shuts down the server.

See `profile/4` for a list of the supported options.
""".
-spec profile(fun(() -> term()), profile_options()) -> ok | {term(), {trace_type(), [trace_info()]}}.
profile(Fun, Options) when is_function(Fun) ->
    do_profile(Fun, Options).

-doc #{equiv => profile(Module, Function, Args, #{})}.
-spec profile(module(), Fun :: atom(), Args :: [term()]) ->
          ok | {term(), {trace_type(), [trace_info()]}}.
profile(Module, Function, Args) when is_atom(Module), is_atom(Function), is_list(Args) ->
    profile(Module, Function, Args, #{}).

-doc """
Does ad-hoc profiling for the call `apply(Module, Function, Args)`.

By default, the result is formatted to the output device; use option `report`
to change this behavior.

Ad-hoc profiling starts a new instance of `tprof` server, runs the
profiling routine, extracts results, and shuts down the server.

The ad-hoc profiler supports the following `Options`:

- **`type`** - The type of profiling to perform.

- **`device`** - Specifies I/O devices to print the profile to. Useful to
  redirect text output to console or `standard_error`.

- **`pattern`** - Specifies a trace pattern, or a list of trace patterns to
  enable. By default, all functions (`{'_', '_', '_'}`) are traced.

- **`report`** - Controls output format. The default is `process`; printing
  per-process profiling data sorted by percentage of the total allocation.
  Specify `report => return` to suppress printing and get the raw data for
  further evaluation with `inspect/3` and formatting with `format/2`.

- **`rootset`** - Includes extra processes in the trace list. Useful for
  profiling allocations for `m:gen_server`, calls, or other allocations caused
  by inter-process communications. See [this example](`m:tprof#pg_example`).

- **`set_on_spawn`** - Automatically start tracing for processes spawned by the
  traced process. Enabled by default.

- **`timeout`** - Terminate profiling after the specified amount of time
  (milliseconds).
""".
-spec profile(module(), Fun :: atom(), Args :: [term()], profile_options()) ->
          ok | {term(), {trace_type(), [trace_info()]}}.
profile(Module, Function, Args, Options) when is_atom(Module), is_atom(Function), is_list(Args) ->
    do_profile({Module, Function, Args}, Options).

%%--------------------------------------------------------------------
%% gen_server implementation
-record(tprof_state, {
    session = undefined :: trace:session() | undefined,
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
    Session = trace:session_create(maps:get(session, Config, ?MODULE), self(), []),
    {ok, #tprof_state{session = Session, type = Type}}.

-doc false.
-spec handle_call(term(), gen_server:from(), state()) -> {reply | noreply, term(), state()}.
handle_call(get_session, _From, State) ->
    {reply, State#tprof_state.session, State};
handle_call({set_pattern, M, F, A}, _From, #tprof_state{session = Session, trace_map = Map, type = Type} = State) ->
    {Reply, NewMap} = enable_pattern(Session, M, F, A, Map, Type),
    {reply, Reply, State#tprof_state{trace_map = NewMap}};
handle_call({clear_pattern, M, F, A}, _From, #tprof_state{session = Session, trace_map = Map, type = Type} = State) ->
    {Ret, NewMap} = disable_pattern(Session, M, F, A, Map, Type),
    {reply, Ret, State#tprof_state{trace_map = NewMap}};
handle_call(get_trace_map, _From, #tprof_state{trace_map = Map} = State) ->
    {reply, Map, State};
handle_call(pause, _From, #tprof_state{paused = true} = State) ->
    {reply, not_running, State};
handle_call(pause, _From, #tprof_state{session = Session, trace_map = Map,
                                       paused = false, type = Type} = State) ->
    foreach(Session, Map, pause, Type),
    {reply, ok, State#tprof_state{paused = true}};
handle_call(continue, _From, #tprof_state{paused = false} = State) ->
    {reply, running, State};
handle_call(continue, _From, #tprof_state{session = Session,
                                          trace_map = Map, type = Type} = State) ->
    foreach(Session, Map, true, Type),
    {reply, ok, State#tprof_state{paused = false}};
handle_call(restart, _From, #tprof_state{session = Session,
                                         trace_map = Map, type = Type} = State) ->
    foreach(Session, Map, restart, Type),
    {reply, ok, State#tprof_state{paused = false}};
handle_call(collect, _From, #tprof_state{session = Session, trace_map = Map, type = Type} = State) ->
    {reply, collect(Session, Map, Type), State};
handle_call({profile, What, Options}, From, #tprof_state{session = Session, ad_hoc = undefined, trace_map = Map, type = Type} = State) ->
    %% ad-hoc profile routed via gen_server to handle 'EXIT' signal
    {Ref, Pid, Patterns, RootSet, NewMap} = ad_hoc_run(Session, What, Options, Map, Type),
    {noreply, State#tprof_state{ad_hoc = {Ref, Pid, Patterns, RootSet, From}, trace_map = NewMap}};
handle_call({profile, _What, _Options}, _From, State) ->
    {reply, {error, running}, State}.

-doc false.
-spec handle_cast(term(), state()) -> no_return().
handle_cast(_Req, _State) ->
    erlang:error(notsup).

-doc false.
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', Ref, process, Pid, Reason},
            #tprof_state{ad_hoc = {Pid, Ref, Patterns, RootSet, From},
    trace_map = Map, type = Type, session = Session} = State) ->
    _ = disable_session_trace(Session, RootSet),
    Profile = collect_ad_hoc(Session, Map, Type),
    gen_server:reply(From, {Reason, Profile}),
    {noreply, State#tprof_state{ad_hoc = undefined, trace_map = disable_patterns(Session, Patterns, Map, Type)}}.

-doc false.
-spec terminate(term(), state()) -> ok.
terminate(_Reason, #tprof_state{session = Session}) ->
    trace:session_destroy(Session),
    ok.

%%--------------------------------------------------------------------
%% Internal implementation

-include_lib("kernel/include/logger.hrl").

default_trace_options() ->
    #{set_on_spawn => true}.

%% Add the trace of the specified module to the accumulator
collect_trace(Session, Mod, FunList, Acc, Type) ->
    {Fail, Ret} = lists:foldl(
        fun ({Fun, Arity}, {Fail, Prev}) ->
            case combine_trace(trace:info(Session, {Mod, Fun, Arity}, Type)) of
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
combine_trace({_, undefined}) -> skip; %% module unloaded after code:all_loaded()
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
        end, Inspected);
format_impl(Device, {Type, Total, Inspected}) ->
    format_each(Device, Type, Total, Inspected).

format_each(Device, call_count, _Total, Inspected) ->
    {Widths, Lines} =
        lists:foldl(
          fun ({Mod, {F, A}, Calls, _Value, _VPC, Percent}, {Widths, Ln}) ->
                  Line = [lists:flatten(io_lib:format("~tw:~tw/~w", [Mod, F, A])),
                          integer_to_list(Calls), float_to_list(Percent, [{decimals, 2}])],
                  NewWidths = [max(Old, New) ||
                                  Old <- [string:length(L) || L <- Line] && New <- Widths],
                {NewWidths, [Line | Ln]}
        end, {[0, 5, 5], []}, Inspected),

    %% figure out formatting line
    Fmt = lists:flatten(io_lib:format("~~.~wts  ~~~ws  [~~~ws]~~n", Widths)),
    %% print using this format
    format_out(Device, Fmt, ["FUNCTION", "CALLS", "%"]),
    [format_out(Device, Fmt, Line) || Line <- lists:reverse(Lines)],
    format_out(Device, Fmt, [" ", " ", "100.0"]);
format_each(Device, call_time, Total, Inspected) ->
    format_labelled(Device, "TIME (us)", Total, Inspected);
format_each(Device, call_memory, Total, Inspected) ->
    format_labelled(Device, "WORDS", Total, Inspected).

format_labelled(Device, Label, Total, Inspected) ->
    %% The width of the value is either total or label
    ValueWidth = erlang:max(length(Label), length(integer_to_list(Total))),

    {Widths, Lines} =
        lists:foldl(
          fun ({Mod, {F, A}, Calls, Value, VPC, Percent}, {Widths, Ln}) ->
                  Line = [lists:flatten(io_lib:format("~tw:~tw/~w", [Mod, F, A])),
                          integer_to_list(Calls), integer_to_list(Value),
                          float_to_list(VPC, [{decimals, 2}]),
                          float_to_list(Percent, [{decimals, 2}])],
                  NewWidths = [max(Old, New) ||
                                  Old <- [string:length(L) || L <- Line] && New <- Widths],
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
enable_pattern(Session, '_', '_', '_', _Acc, Type) ->
    %% need to re-trace everything, probably some new modules were loaded
    %% discard any existing trace pattern
    trace:function(Session, on_load, true, [Type]),
    {trace:function(Session, {'_', '_', '_'}, true, [Type]), all};
enable_pattern(Session, Mod, '_', '_', Acc, Type) ->
    %% code may have been hot-loaded, redo the trace
    case trace:function(Session, {Mod, '_', '_'}, true, [Type]) of
        0 ->
            {{error, {trace_pattern, Mod, '_', '_'}}, Acc};
        Traced ->
            {Traced, update_trace_map(Acc, fun() -> Acc#{Mod => Mod:module_info(functions)} end)}
    end;
enable_pattern(Session, Mod, Fun, '_', Acc, Type) ->
    case trace:function(Session, {Mod, Fun, '_'}, true, [Type]) of
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
enable_pattern(Session, Mod, Fun, Arity, Acc, Type) ->
    case trace:function(Session, {Mod, Fun, Arity}, true, [Type]) of
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
disable_pattern(Session, '_', '_', '_', _Acc, Type) ->
    trace:function(Session, on_load, false, [Type]),
    Traced = trace:function(Session, {'_', '_', '_'}, false, [Type]),
    {Traced, #{}};
disable_pattern(Session, Mod, '_', '_', Acc, Type) when is_map_key(Mod, Acc); Acc =:= all ->
    Traced = trace:function(Session, {Mod, '_', '_'}, false, [Type]),
    {Traced, update_trace_map(Acc, fun() -> maps:remove(Mod, Acc) end)};
disable_pattern(Session, Mod, Fun, '_', Acc, Type) when is_map_key(Mod, Acc); Acc =:= all ->
    Traced = trace:function(Session, {Mod, Fun, '_'}, false, [Type]),
    {Traced, update_trace_map(
               Acc,
               fun() ->
                       maps:update_with(
                         Mod,
                         fun (FAs) -> [{F, A} || {F, A} <- FAs, F =/= Fun] end,
                         Acc)
               end)};
disable_pattern(Session, Mod, Fun, Arity, Acc, Type) when is_map_key(Mod, Acc); Acc =:= all ->
    Traced = trace:function(Session, {Mod, Fun, Arity}, false, [Type]),
    {Traced, update_trace_map(
               Acc,
               fun() ->
                       maps:update_with(Mod, fun (FAs) -> FAs -- [{Fun, Arity}] end, Acc)
               end)};
disable_pattern(_Session, Mod, Fun, Arity, Acc, _Type) ->
    {{error, {not_traced, Mod, Fun, Arity}}, Acc}.

disable_patterns(Session, Patterns, Map, Type) ->
    lists:foldl(
        fun ({M, F, A}, Acc) -> {_, New} = disable_pattern(Session, M, F, A, Acc, Type), New end,
        Map,
        Patterns
    ).

%% ad-hoc profiler implementation
do_profile(What, Options) ->
    %% start a new tprof server
    Pid = start_result(start_link(Options#{ session => tprof_profile })),
    try
        {Ret, Profile} = gen_server:call(Pid, {profile, What, Options}, infinity),
        return_profile(maps:get(report, Options, process), Profile, Ret,
            maps:get(device, Options, []))
    after
        gen_server:stop(Pid)
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

toggle_process_trace(Session, Pid, On, Flags) when is_pid(Pid) ->
    try
        1 = trace:process(Session, Pid, On, Flags)
    catch _:_ ->
        0
    end;
toggle_process_trace(Session, Name, On, Flags) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            0;
        Pid ->
            toggle_process_trace(Session, Pid, On, Flags)
    end.

toggle_trace(_Session, [], _On, _Flags, Success, []) ->
    Success;
toggle_trace(_Session, [], _On, _Flags, Success, Failure) ->
    {Success, lists:reverse(Failure)};
toggle_trace(Session, [Pid | Tail], On, Flags, Success, Failure) when is_pid(Pid) ->
    {NS, NF} =
        try
            1 = trace:process(Session, Pid, On, Flags),
            {Success + 1, Failure}
        catch _:_ ->
            {Success, [Pid | Failure]}
        end,
    toggle_trace(Session, Tail, On, Flags, NS, NF);
toggle_trace(Session, [Name | Tail], On, Flags, Success, Failure) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            toggle_trace(Session, Tail, On, Flags, Success, [Name | Failure]);
        Pid ->
            {NS, NF} =
                try
                    1 = trace:process(Session, Pid, On, Flags),
                    {Success + 1, Failure}
                catch _:_ ->
                    {Success, [Name | Failure]}
                end,
            toggle_trace(Session, Tail, On, Flags, NS, NF)
    end.

-spec collect(trace:session(),trace_map(), trace_type()) -> {trace_type(), [trace_info()]}.
collect(S, all, Type) ->
    collect(S,
      #{ Mod => Mod:module_info(functions) || {Mod, _} <- code:all_loaded() },
      Type);
collect(S, Pattern, Type) ->
    {Type, maps:fold(fun(K, V, Acc) -> collect_trace(S, K, V, Acc, Type) end, [], Pattern)}.

collect_ad_hoc(Session, Pattern, Type) ->
    {Type, TraceInfo} = collect(Session, Pattern, Type),
    {Type, [TI || TI = {Mod, Fun, Arity, _} <- TraceInfo,
                  Mod =/= trace,
                  Mod =/= tprof,
                  not (Mod =:= erts_internal andalso Fun =:= trace_pattern andalso Arity =:= 4),
                  not (Mod =:= erts_internal andalso Fun =:= trace andalso Arity =:= 4)
           ]}.

foreach(S, all, Action, Type) ->
    _ = trace:function(S, on_load, Action, [Type]),
    _ = trace:function(S, {'_', '_', '_'}, Action, [Type]),
    ok;
foreach(S, Map, Action, Type) ->
    maps:foreach(
        fun (Mod, Funs) ->
            [trace:function(S, {Mod, F, A}, Action, [Type]) || {F, A} <- Funs]
        end, Map).

ad_hoc_run(Session, What, Options, Map, Type) ->
    %% add missing patterns
    Patterns = make_list(maps:get(pattern, Options, {'_', '_', '_'})),
    NewMap = lists:foldl(
        fun({M, F, A}, Acc) ->
            {_, NewMap} = enable_pattern(Session, M, F, A, Acc, Type),
            NewMap
        end, Map, Patterns),

    %% check whether spawned processes are also traced
    OnSpawn = maps:get(set_on_spawn, Options, true),
    %% enable tracing for items in the rootset
    RootSet = maps:get(rootset, Options, []),
    %% ignore errors when setting up rootset trace
    _ = enable_session_trace(Session, RootSet),

    %% spawn a separate process to run the user-supplied MFA
    %% if RootSet is 'all' or 'new', skip the trace flags
    Flags = trace_flags(RootSet, OnSpawn),

    {Pid, Ref} = spawn_profiled(Session, What, Type, Flags, maps:get(timeout, Options, infinity)),

    {Pid, Ref, Patterns, RootSet, NewMap}.

trace_flags(all, _) -> [];
trace_flags(new, _) -> [];
trace_flags(_, true) -> [call, silent, set_on_spawn];
trace_flags(_, false) -> [call, silent].

make_list({M, F, A}) -> [{M, F, A}];
make_list(List) -> List.

spawn_profiled(Session, Fun, call_count, _Flags, Timeout) when is_function(Fun) ->
    spawn_monitor(
      fun() ->
              start_timer(Session, call_count, Timeout),
              _ = trace:function(Session, {'_','_','_'}, restart, [call_count]),
              Ret = catch Fun(),
              _ = trace:function(Session, {'_','_','_'}, pause, [call_count]),
              exit(Ret)
      end);
spawn_profiled(Session, {M, F, A}, call_count, _Flags, Timeout) ->
    spawn_monitor(
      fun() ->
              start_timer(Session, call_count, Timeout),
              _ = trace:function(Session, {'_','_','_'}, restart, [call_count]),
              Ret = catch erlang:apply(M, F, A),
              _ = trace:function(Session, {'_','_','_'}, pause, [call_count]),
              exit(Ret)
      end);
spawn_profiled(Session, Fun, Type, Flags, Timeout) when is_function(Fun) ->
    spawn_monitor(
      fun() ->
              start_timer(Session, Type, Timeout),
              Flags =/= [] andalso begin 1 = trace:process(Session, self(), true, Flags) end,
              Ret = catch Fun(),
              Flags =/= [] andalso begin 1 = trace:process(Session, self(), false, Flags) end,
              exit(Ret)
      end);
spawn_profiled(Session, {M, F, A}, Type, Flags, Timeout) ->
    spawn_monitor(
      fun() ->
              start_timer(Session, Type, Timeout),
              Flags =/= [] andalso begin 1 = trace:process(Session, self(), true, Flags) end,
              Ret = catch erlang:apply(M, F, A),
              Flags =/= [] andalso begin 1 = trace:process(Session, self(), false, Flags) end,
              exit(Ret)
      end).

%% In order for the timer to not trigger any profiling code, we
%% make sure to start it before profiling is started. 
start_timer(_Session, _Type, infinity) ->
    ok;
start_timer(Session, Type, Timeout) ->
    Ref = make_ref(),
    Pid = self(),
    spawn(fun() ->
                  MonRef = monitor(process, Pid),
                  Pid ! Ref,
                  receive
                      {'DOWN', MonRef, _, _, _} ->
                          ok
                  after Timeout ->
                          %% Pause call_count profiling before the
                          %% timer triggers so that the code in gen_server
                          %% does not become part of the profiling
                          Type =/= call_count orelse
                              trace:function(Session, {'_','_','_'}, pause, [Type]),
                          exit(Pid, {'EXIT',timeout})
                  end
          end),
    receive Ref -> ok end.
