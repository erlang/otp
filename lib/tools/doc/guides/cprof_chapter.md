<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# cprof - The Call Count Profiler

`cprof` is a profiling tool that can be used to get a picture of how often
different functions in the system are called.

`cprof` uses breakpoints similar to local call trace, but containing counters,
to collect profiling data. Therefore there is no need for special compilation of
any module to be profiled.

`cprof` presents all profiled modules in decreasing total call count order, and
for each module presents all profiled functions also in decreasing call count
order. A call count limit can be specified to filter out all functions below the
limit.

Profiling is done in the following steps:

- **`cprof:start/0..3`** - Starts profiling with zeroed call counters for
  specified functions by setting call count breakpoints on them.

- **`Mod:Fun()`** - Runs the code to be profiled.

- **`cprof:pause/0..3`** - Pauses the call counters for specified functions.
  This minimises the impact of code running in the background or in the shell
  that disturbs the profiling. Call counters are automatically paused when they
  "hit the ceiling" of the host machine word size. For a 32 bit host the maximum
  counter value is 2147483647.

- **`cprof:analyse/0..2`** - Collects call counters and computes the result.

- **`cprof:restart/0..3`** - Restarts the call counters from zero for specified
  functions. Can be used to collect a new set of counters without having to stop
  and start call count profiling.

- **`cprof:stop/0..3`** - Stops profiling by removing call count breakpoints
  from specified functions.

Functions can be specified as either all in the system, all in one module, all
arities of one function, one function, or all functions in all modules not yet
loaded. As for now, BIFs cannot be call count traced.

The analysis result can either be for all modules, or for one module. In either
case a call count limit can be given to filter out the functions with a call
count below the limit. The all modules analysis does _not_ contain the module
`cprof` itself, it can only be analysed by specifying it as a single module to
analyse.

Call count tracing is very lightweight compared to other forms of tracing since
no trace message has to be generated. Some measurements indicates performance
degradations in the vicinity of 10 percent.

The following sections show some examples of profiling with `cprof`. See also
`m:cprof`.

## Example: Background work

From the Erlang shell:

```erlang
1> cprof:start(), cprof:pause(). % Stop counters just after start
8492
2> cprof:analyse().
{539,
 [{shell,155,
         [{{shell,prep_check,1},55},
          {{shell,used_records,4},45},
          {{shell,used_records,1},45},
          {{shell,used_record_defs,2},1},
          {{shell,record_defs,2},1},
          {{shell,record_bindings,2},1},
          {{shell,exprs,7},1},
          {{shell,expr,4},1},
          {{shell,expand_records,2},1},
          {{shell,check_command,2},1},
          {{shell,apply_fun,3},1},
          {{shell,'-exprs/7-lc$^0/1-0-',1},1},
          {{shell,'-eval_loop/3-fun-0-',3},1}]},
  %% Information about many modules omitted.
                     .
                     .
                     .
  %% Here is the last part.
  {erts_internal,2,[{{erts_internal,trace_pattern,3},2}]},
  {otp_internal,1,[{{otp_internal,obsolete,3},1}]},
  {maps,1,[{{maps,from_list,1},1}]},
  {erl_internal,1,[{{erl_internal,bif,3},1}]}]}
3> cprof:analyse(cprof).
{cprof,3,[{{cprof,tr,2},2},{{cprof,pause,0},1}]}
4> cprof:stop().
8586
```

The example showed some of the background work that the shell performs just to
interpret the first command line.

What is captured in this example is the part of the work the shell does while
interpreting the command line that occurs between the actual calls to
`cprof:start()` and `cprof:analyse()`.

## Example: One module

From the Erlang shell:

```erlang
1> cprof:start(),R=calendar:day_of_the_week(1896,4,27),cprof:pause(),R.
1
2> cprof:analyse(calendar).
{calendar,9,
          [{{calendar,last_day_of_the_month1,2},1},
           {{calendar,last_day_of_the_month,2},1},
           {{calendar,is_leap_year1,1},1},
           {{calendar,is_leap_year,1},1},
           {{calendar,dy,1},1},
           {{calendar,dm,1},1},
           {{calendar,df,2},1},
           {{calendar,day_of_the_week,3},1},
           {{calendar,date_to_gregorian_days,3},1}]}
3> cprof:stop().
8648
```

The example tells us that "Aktiebolaget LM Ericsson & Co" was registered on a
Monday (since the return value of the first command is 1), and that the
`calendar` module needed 9 function calls to calculate that.

Using `cprof:analyse()` in this example also shows approximately the same
background work as in the first example.

## Example: In the code

Write a module:

```erlang
-module(sort).
-export([do/1]).

do(N) ->
    cprof:stop(),
    cprof:start(),
    do(N, []).

do(0, L) ->
    R = lists:sort(L),
    cprof:pause(),
    R;
do(N, L) ->
    do(N-1, [rand:uniform(256)-1 | L]).
```

From the Erlang shell:

```erlang
1> c(sort).
{ok,sort}
2> rand:seed(default, 42), ok.
ok.
3> sort:do(1000).
[0,0,0,1,1,1,1,2,2,3,3,4,4,4,4,5,5,5,6,6,6,6,7,7,7,7,7,8,8|...]
4> cprof:analyse().
{13180,
 [{lists,6173,
         [{{lists,rmerge3_1,6},1045},
          {{lists,rmerge3_2,6},977},
          {{lists,split_1,5},652},
          {{lists,merge3_1,6},579},
          {{lists,merge3_2,6},577},
          {{lists,rmerge3_12_3,6},511},
          {{lists,split_1_1,6},347},
          {{lists,merge3_12_3,6},310},
          {{lists,rmerge3_21_3,6},282},
          {{lists,merge3_21_3,6},221},
          {{lists,merge2_1,4},154},
          {{lists,merge2_2,5},138},
          {{lists,reverse,2},106},
          {{lists,rmerge2_2,5},87},
          {{lists,rmergel,2},81},
          {{lists,rmerge2_1,4},75},
          {{lists,mergel,2},28},
          {{lists,keyfind,3},2},
          {{lists,sort,1},1}]},
  {rand,5000,
        [{{rand,uniform_s,2},1000},
         {{rand,uniform,1},1000},
         {{rand,seed_put,1},1000},
         {{rand,seed_get,0},1000},
         {{rand,exsss_uniform,2},1000}]},
  {erlang,1004,
          [{{erlang,put,2},1000},
           {{erlang,trace_pattern,3},2},
           {{erlang,ensure_tracer_module_loaded,2},2}]},
  {sort,1001,[{{sort,do,2},1001}]},
  {erts_internal,2,[{{erts_internal,trace_pattern,3},2}]}]}
5> cprof:stop().
12625
```

The example shows some details of how `lists:sort/1` works. It used 6173
function calls in the module `lists` to complete the work.

This time, since the shell was not involved in starting and stopping `cprof`, no
other work was done in the system during the profiling.
