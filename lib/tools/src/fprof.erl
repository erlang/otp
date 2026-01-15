%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2001-2025. All Rights Reserved.
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

%%%----------------------------------------------------------------------
%%% File    : fprof.erl
%%% Author  : Raimo Niskanen <raimo@erix.ericsson.se>
%%% Purpose : File tracing profiling tool which accumulated times.
%%% Created : 18 Jun 2001 by Raimo Niskanen <raimo@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(fprof).
-moduledoc """
A Time Profiling Tool using trace to file for minimal runtime performance
impact.

This module is used to profile a program to find out how the execution time is
used. Tracing to file is used to minimize the runtime performance degradation.

The `fprof` module uses tracing to collect profiling data, hence there is no
need for special compilation of any module to be profiled. When it starts
tracing, `fprof` will erase all previous tracing in the node and set the
necessary trace flags on the profiling target processes as well as local call
trace on all functions in all loaded modules and all modules to be loaded.
`fprof` disable all tracing in the node when it stops tracing.

`fprof` presents both _own time_ that is, how much time a function has
used for its own execution, and _accumulated time_ that is, including
called functions. All presented times are collected using trace
timestamps. `fprof` tries to collect CPU time timestamps, if the host
machine OS supports it. Therefore, the times can be wallclock times and
OS scheduling will randomly strike all called functions in a
presumably fair way.

However, if the profiling time is short, and the host machine OS does
not support high resolution CPU time measurements, a few OS
schedulings can show up as ridiculously long execution times for
functions doing practically nothing. As an example, it has been
observed that a function that more or less just composing a tuple, was
running 100 times slower than normal. When tracing was repeated, the
execution time was normal.

Profiling is essentially done in 3 steps:

- Tracing: to a file. The trace data contains entries for function
  calls, returns to function, process scheduling, other process
  related events (for example `spawn`), and garbage collection. All trace
  entries are timestamped.

- Profiling: the trace file is read, the execution call stack is
  simulated, and raw profile data is calculated from the simulated call stack
  and the trace timestamps. The profile data is stored in the `fprof` server
  state. During this step the trace data may be dumped in text format to file or
  console.

- Analysing: the raw profile data is sorted, filtered and dumped in
  text format either to file or console. The text format intended to be both
  readable for a human reader, as well as parsable with the standard erlang
  parsing tools.

Since `fprof` sends trace data to afile, the runtime performance
degradation is minimized, but still far from negligible, especially
for programs that themselves use the filesystem heavily. Where the
trace file is placed is also important, for example, on Unix systems
`/tmp` is usually a good choice since it is essentially a RAM disk,
while any network-mounted disk is a bad idea.

`fprof` can also skip the file step and trace to a tracer process that does the
profiling in runtime.

[](){: #analysis }

## Analysis format

This section describes the output format of the `analyse/1` function.

The format is parsable with the standard Erlang parsing tools
`m:erl_scan` and `m:erl_parse`, `file:consult/1`, or `io:read/2`. The
parse format is not described here — it should be easy enough for the
interested reader to try it out. Note that some flags to
[`analyse/1`](`analyse/1`) will affect the format.

The following example was run on Erlang/OTP R8 on Solaris 8; all OTP
internals in this example are version dependent.

As an example, we will use the following function, which is a
slightly modified benchmark function from module `m:file`:

```erlang
-module(foo).
-export([create_file_slow/2]).

create_file_slow(Name, N) when is_integer(N), N >= 0 ->
    {ok, FD} =
        file:open(Name, [raw, write, delayed_write, binary]),
    if N > 256 ->
            ok = file:write(FD,
                            lists:map(fun (X) -> <<X:32/unsigned>> end,
                            lists:seq(0, 255))),
            ok = create_file_slow(FD, 256, N);
       true ->
            ok = create_file_slow(FD, 0, N)
    end,
    ok = file:close(FD).

create_file_slow(FD, M, M) ->
    ok;
create_file_slow(FD, M, N) ->
    ok = file:write(FD, <<M:32/unsigned>>),
    create_file_slow(FD, M+1, N).
```

Let us have a look at the printout after running:

```erlang
1> fprof:apply(foo, create_file_slow, [junk, 1024]).
2> fprof:profile().
3> fprof:analyse().
```

The printout starts with:

```erlang
%% Analysis results:
{  analysis_options,
 [{callers, true},
  {sort, acc},
  {totals, false},
  {details, true}]}.

%                                       CNT       ACC       OWN
[{ totals,                             9627, 1691.119, 1659.074}].  %%%
```

The `CNT` column shows the total number of function calls that was found in the
trace. In the `ACC` column is the total time of the trace from first timestamp to
last. And in the `OWN` column is the sum of the execution time in functions found
in the trace, not including called functions. In this case it is very close to
the `ACC` time since the emulator had practically nothing to do except
executing our test program.

All time values in the printout are in milliseconds.

The printout continues:

```erlang
%                                       CNT       ACC       OWN
[{ "<0.28.0>",                         9627,undefined, 1659.074}].   %%
```

This is the printout header of one process. The printout contains only this one
process since we called `fprof:apply/3` that traces only the current process.
Therefore the `CNT` and `OWN` columns perfectly matches the totals above. The `ACC`
column is undefined since summing the `ACC` times of all calls in the process
makes no sense — one would get something like the `ACC` value from totals above
multiplied by the average depth of the call stack.

All paragraphs up to the next process header only concerns function calls within
this process.

Now we come to something more interesting:

```erlang
{[{undefined,                             0, 1691.076,    0.030}],
 { {fprof,apply_start_stop,4},            0, 1691.076,    0.030},     %
 [{{foo,create_file_slow,2},              1, 1691.046,    0.103},
  {suspend,                               1,    0.000,    0.000}]}.

{[{{fprof,apply_start_stop,4},            1, 1691.046,    0.103}],
 { {foo,create_file_slow,2},              1, 1691.046,    0.103},     %
 [{{file,close,1},                        1, 1398.873,    0.019},
  {{foo,create_file_slow,3},              1,  249.678,    0.029},
  {{file,open,2},                         1,   20.778,    0.055},
  {{lists,map,2},                         1,   16.590,    0.043},
  {{lists,seq,2},                         1,    4.708,    0.017},
  {{file,write,2},                        1,    0.316,    0.021}]}.
```

The printout consists of one paragraph per called function. The function
_marked_ with `%` is the one the paragraph concerns — `foo:create_file_slow/2`.
Above the marked function are the _calling_ functions — those that has called
the marked, and below are those _called_ by the marked function.

The paragraphs are per default sorted in descending order of the `ACC` column for
the marked function. The calling list and called list within one paragraph are
also per default sorted in descending order of their `ACC` column.

The columns are:

* `CNT` - the number of times the function has been called
* `ACC` - the time spent in the function including called functions
* `OWN` - the time spent in the function not including called functions

The rows for the _calling_ functions contain statistics for the _marked_
function with the constraint that only the occasions when a call was made from
the _row's_ function to the _marked_ function are accounted for.

The row for the _marked_ function simply contains the sum of all _calling_ rows.

The rows for the _called_ functions contains statistics for the _row's_ function
with the constraint that only the occasions when a call was made from the
_marked_ to the _row's_ function are accounted for.

So, we see that `foo:create_file_slow/2` used very little time for its own
execution. It spent most of its time in `file:close/1`. The function
`foo:create_file_slow/3` that writes 3/4 of the file contents is the second
biggest time thief.

We also see that the call to `file:write/2` that writes 1/4 of the file contents
takes very little time in itself. What takes time is to build the data
(`lists:seq/2` and `lists:map/2`).

The function `undefined` that has called `fprof:apply_start_stop/4` is an
unknown function because that call was not recorded in the trace. It was only
recorded that the execution returned from `fprof:apply_start_stop/4` to some
other function above in the call stack, or that the process exited from there.

Let us continue down the printout to find:

```erlang
{[{{foo,create_file_slow,2},              1,  249.678,    0.029},
  {{foo,create_file_slow,3},            768,    0.000,   23.294}],
 { {foo,create_file_slow,3},            769,  249.678,   23.323},     %
 [{{file,write,2},                      768,  220.314,   14.539},
  {suspend,                              57,    6.041,    0.000},
  {{foo,create_file_slow,3},            768,    0.000,   23.294}]}.
```

If you compare with the code you will see there also that
`foo:create_file_slow/3` was called only from `foo:create_file_slow/2` and
itself, and called only `file:write/2`, note the number of calls to
`file:write/2`. But here we see that `suspend` was called a few times. This is a
pseudo function that indicates that the process was suspended while executing in
`foo:create_file_slow/3`, and since there is no `receive` or `erlang:yield/0` in
the code, it must be Erlang scheduling suspensions, or the trace file driver
compensating for large file write operations (these are regarded as a schedule
out followed by a schedule in to the same process).

Let us find the `suspend` entry:

```erlang
{[{{file,write,2},                       53,    6.281,    0.000},
  {{foo,create_file_slow,3},             57,    6.041,    0.000},
  {{prim_file,drv_command,4},            50,    4.582,    0.000},
  {{prim_file,drv_get_response,1},       34,    2.986,    0.000},
  {{lists,map,2},                        10,    2.104,    0.000},
  {{prim_file,write,2},                  17,    1.852,    0.000},
  {{erlang,port_command,2},              15,    1.713,    0.000},
  {{prim_file,drv_command,2},            22,    1.482,    0.000},
  {{prim_file,translate_response,2},     11,    1.441,    0.000},
  {{prim_file,'-drv_command/2-fun-0-',1},  15,    1.340,    0.000},
  {{lists,seq,4},                         3,    0.880,    0.000},
  {{foo,'-create_file_slow/2-fun-0-',1},   5,    0.523,    0.000},
  {{erlang,bump_reductions,1},            4,    0.503,    0.000},
  {{prim_file,open_int_setopts,3},        1,    0.165,    0.000},
  {{prim_file,i32,4},                     1,    0.109,    0.000},
  {{fprof,apply_start_stop,4},            1,    0.000,    0.000}],
 { suspend,                             299,   32.002,    0.000},     %
 [ ]}.
```

We find no particularly long suspend times, so no function seems to have waited
in a receive statement. Actually, `prim_file:drv_command/4` contains a receive
statement, but in this test program, the message lies in the process receive
buffer when the receive statement is entered. We also see that the total suspend
time for the test run is small.

The `suspend` pseudo function has an `OWN` time of zero. This is to prevent
the process total `OWN` time from including time in suspension. Whether suspend
time is really `ACC` or `OWN` time is more of a philosophical question.

Now we look at another interesting pseudo function, `garbage_collect`:

```erlang
{[{{prim_file,drv_command,4},            25,    0.873,    0.873},
  {{prim_file,write,2},                  16,    0.692,    0.692},
  {{lists,map,2},                         2,    0.195,    0.195}],
 { garbage_collect,                      43,    1.760,    1.760},     %
 [ ]}.
```

Here we see that no function stands out, which is very normal.

The `garbage_collect` pseudo function has not an `OWN` time of zero like
`suspend`, instead it is equal to the `ACC` time.

Garbage collection often occurs while a process is suspended, but `fprof` hides
this fact by pretending that the suspended function was first unsuspended and
then garbage collected. Otherwise the printout would show `garbage_collect`
being called from `suspend`, but not which function that might have caused the
garbage collection.

Let us now get back to the test code:

```erlang
{[{{foo,create_file_slow,3},            768,  220.314,   14.539},
  {{foo,create_file_slow,2},              1,    0.316,    0.021}],
 { {file,write,2},                      769,  220.630,   14.560},     %
 [{{prim_file,write,2},                 769,  199.789,   22.573},
  {suspend,                              53,    6.281,    0.000}]}.
```

Not unexpectedly, we see that `file:write/2` was called from
`foo:create_file_slow/3` and `foo:create_file_slow/2`. The number of calls in
each case as well as the used time are also confirms the previous results.

We see that `file:write/2` only calls `prim_file:write/2`, but let us refrain
from digging into the internals of the kernel application.

If we nevertheless _do_ dig down we find the call to the linked-in driver
that does the file operations towards the host operating system:

```erlang
{[{{prim_file,drv_command,4},           772, 1458.356, 1456.643}],
 { {erlang,port_command,2},             772, 1458.356, 1456.643},     %
 [{suspend,                              15,    1.713,    0.000}]}.
```

This is 86 % of the total run time, and as we saw before it is the close
operation the absolutely biggest contributor. We find a comparison ratio a
little bit up in the call stack:

```erlang
{[{{prim_file,close,1},                   1, 1398.748,    0.024},
  {{prim_file,write,2},                 769,  174.672,   12.810},
  {{prim_file,open_int,4},                1,   19.755,    0.017},
  {{prim_file,open_int_setopts,3},        1,    0.147,    0.016}],
 { {prim_file,drv_command,2},           772, 1593.322,   12.867},     %
 [{{prim_file,drv_command,4},           772, 1578.973,   27.265},
  {suspend,                              22,    1.482,    0.000}]}.
```

The time for file operations in the linked in driver distributes itself as 1 %
for open, 11 % for write, and 87 % for close. All data is probably buffered in
the operating system until the close.

The observant reader may notice that the ACC times for
`prim_file:drv_command/2` and `prim_file:drv_command/4` is not equal between the
paragraphs above, even though it is easy to believe that
`prim_file:drv_command/2` is just a passthrough function.

The missing time can be found in the paragraph for `prim_file:drv_command/4`
where it is evident that not only `prim_file:drv_command/2` is called but also a
fun:

```erlang
{[{{prim_file,drv_command,2},           772, 1578.973,   27.265}],
 { {prim_file,drv_command,4},           772, 1578.973,   27.265},     %
 [{{erlang,port_command,2},             772, 1458.356, 1456.643},
  {{prim_file,'-drv_command/2-fun-0-',1}, 772,   87.897,   12.736},
  {suspend,                              50,    4.582,    0.000},
  {garbage_collect,                      25,    0.873,    0.873}]}.
```

And some more missing time can be explained by the fact that
`prim_file:open_int/4` both calls `prim_file:drv_command/2` directly as well as
through `prim_file:open_int_setopts/3`, which complicates the picture.

```erlang
{[{{prim_file,open,2},                    1,   20.309,    0.029},
  {{prim_file,open_int,4},                1,    0.000,    0.057}],
 { {prim_file,open_int,4},                2,   20.309,    0.086},     %
 [{{prim_file,drv_command,2},             1,   19.755,    0.017},
  {{prim_file,open_int_setopts,3},        1,    0.360,    0.032},
  {{prim_file,drv_open,2},                1,    0.071,    0.030},
  {{erlang,list_to_binary,1},             1,    0.020,    0.020},
  {{prim_file,i32,1},                     1,    0.017,    0.017},
  {{prim_file,open_int,4},                1,    0.000,    0.057}]}.
.
.
.
{[{{prim_file,open_int,4},                1,    0.360,    0.032},
  {{prim_file,open_int_setopts,3},        1,    0.000,    0.016}],
 { {prim_file,open_int_setopts,3},        2,    0.360,    0.048},     %
 [{suspend,                               1,    0.165,    0.000},
  {{prim_file,drv_command,2},             1,    0.147,    0.016},
  {{prim_file,open_int_setopts,3},        1,    0.000,    0.016}]}.
```

## Notes

The actual supervision of execution times is in itself a CPU-intensive activity.
A message is written on the trace file for every function call that is made by
the profiled code.

The `ACC` time calculation is sometimes difficult to make correct, since it is
difficult to define. This happens especially when a function occurs in several
instances in the call stack, for example by calling itself perhaps through other
functions and perhaps even non-tail recursively.

To produce sensible results, `fprof` tries not to charge any function more than
once for `ACC` time. The instance highest up (with longest duration) in the call
stack is chosen.

Sometimes a function can unexpectedly waste a lot (some 10 ms or more depending
on host machine OS) of `OWN` (and `ACC`) time, even functions that do practically
nothing at all. The problem may be that the OS has chosen to schedule out the
Erlang runtime system process for a while, and if the OS does not support high
resolution CPU time measurements `fprof` will use wallclock time for its
calculations, and it will appear as if functions are randomly burning virtual
machine time.

## See Also

[fprof - The File Trace Profiler](fprof_chapter.md), `m:dbg`, `m:eprof`
""".
-author('raimo@erix.ericsson.se').

%% External exports
-export([
	 apply/2, apply/3, apply/4,
	 start/0, stop/0, stop/1,
	 trace/1, trace/2,
	 profile/0, profile/1, profile/2,
	 analyse/0, analyse/1, analyse/2]).
%% Debug functions
-export([get_state/0,
	 save_profile/0, save_profile/1, save_profile/2,
	 load_profile/0, load_profile/1, load_profile/2,
	 code_change/0]).

%% Debug exports
-export([call/1, just_call/1, reply/2]).
-export([trace_off/0, trace_on/3]).
-export([getopts/2, setopts/1]).
-export([println/5, print_callers/2, print_func/2, print_called/2]).
-export([trace_call_collapse/1]).
-export([parsify/1]).

%% Internal exports
-export(['$code_change'/1]).



-define(FNAME_WIDTH, 72).
-define(NR_WIDTH, 15).

-define(TRACE_FILE, "fprof.trace").
-define(DUMP_FILE, "fprof.dump").
-define(PROFILE_FILE, "fprof.profile").
-define(ANALYSIS_FILE, "fprof.analysis").

-define(FPROF_SERVER, fprof_server).
-define(FPROF_SERVER_TIMEOUT, infinity).



-define(debug, 9).
%-define(debug, 0).
-ifdef(debug).
dbg(Level, F, A) when Level >= ?debug ->
    io:format(F, A),
    ok;
dbg(_, _, _) ->
    ok.
-define(dbg(Level, F, A), dbg((Level), (F), (A))).
-else.
-define(dbg(Level, F, A), ok).
-endif.



%%%----------------------------------------------------------------------
%%% Higher order API functions
%%%----------------------------------------------------------------------


-doc #{equiv => apply(Func, Args, [])}.
-spec apply(Func, Args) -> term() when
      Func :: fun() | {Module :: module(), Function :: atom()},
      Args :: [term()].

apply({M, F}, Args) 
  when is_atom(M), is_atom(F), is_list(Args) ->
    apply_1(M, F, Args, []);
apply(Fun, Args) 
  when is_function(Fun), is_list(Args) ->
    apply_1(Fun, Args, []);
apply(A, B) ->
    erlang:error(badarg, [A, B]).

-type pid_spec() :: pid() | atom().
-type trace_option() :: 'cpu_time'
                      | {'cpu_time', boolean()}
                      | 'file'
                      | {'file', Filename :: file:filename()}
                      | {'procs', PidSpec :: pid_spec()}
                      | {'procs', [PidSpec :: pid_spec()]}
                      | 'start'
                      | 'stop'
                      | {'tracer', Tracer :: pid() | port()}
                      | 'verbose'
                      | {'verbose', boolean()}.

-type apply_option() :: 'continue'
                        | {'procs', PidList :: [pid()]}
                        | 'start'
                        | TraceStartOption :: trace_option().

-doc """
apply(Arg1, Arg2, Arg3)

Calls the given function surrounded by
[`trace([start, ...])`](`trace/1`) and
[`trace(stop)`](`trace/1`).

If the function arguments (`Arg1`, `Arg2`, and `Arg3`) are `Module`
(an atom), `Function` (an atom), and `Args` (a list), the function
will be called using
[`erlang:apply(Module, Function, Args)`](`erlang:apply/3`).

If the function arguments are `Func` (a fun), `Args` (a list), and
`OptionList` (a list of options), the fun will be called using
[`erlang:apply(Func, Args)`](`erlang:apply/2`).

Some effort is made to keep the trace clean from unnecessary trace messages;
tracing is started and stopped from a spawned process while `erlang:apply/2`
is called in the current process only surrounded by `receive` and `send`
statements towards the trace starting process. The trace starting process exits
when it is not needed any more.

The `TraceStartOption` is any option allowed for `trace/1`. The
options `[start, {procs, [self() | PidList]} | OptList]` are given to
[`trace/1`](`trace/1`), where `OptList` is `OptionList` with the
`continue`, `start` and `{procs, _}` options removed.

The `continue` option inhibits the call to [`trace(stop)`](`trace/1`) and leaves
it up to the caller to stop tracing at a suitable time.
""".
-spec apply(Module, Function, Args) -> term() when
                Module :: module(),
                Function :: atom(),
                Args :: [term()];
           (Func, Args, OptionList) -> term() when
                Func :: fun() | {Module :: module(), Function :: atom()},
                Args :: [term()],
                OptionList :: [Option],
                Option :: apply_option().

apply(M, F, Args) when is_atom(M), is_atom(F), is_list(Args) ->
    apply_1(M, F, Args, []);
apply({M, F}, Args, Options) 
  when is_atom(M), is_atom(F), is_list(Args), is_list(Options) ->
    apply_1(M, F, Args, Options);
apply(Fun, Args, Options) 
  when is_function(Fun), is_list(Args), is_list(Options) ->
    apply_1(Fun, Args, Options);
apply(A, B, C) ->
    erlang:error(badarg, [A, B, C]).

-doc #{equiv => apply({Module, Function}, Args, OptionList)}.
-spec apply(Module, Function, Args, OptionList) -> term() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      OptionList :: [Option],
      Option :: apply_option().

apply(M, F, Args, Options)
  when is_atom(M), is_atom(F), is_list(Args), is_list(Options) ->
    apply_1(M, F, Args, Options);
apply(A, B, C, D) ->
    erlang:error(badarg, [A, B, C, D]).

apply_1(M, F, Args, Options) ->
    Arity = length(Args),
    apply_1(fun M:F/Arity, Args, Options).

apply_1(Function, Args, Options) ->        
    {[_, Procs, Continue], Options_1} =
	getopts(Options, [start, procs, continue]),
    Procs_1 = case Procs of
		  [{procs, P}] when is_list(P) ->
		      P;
		  _ ->
		      []
	      end,
    case Continue of
	[] ->
	    apply_start_stop(Function, Args, Procs_1, Options_1);
	[continue] ->
	    apply_continue(Function, Args, Procs_1, Options_1);
	_ ->
	    erlang:error(badarg, [Function, Args, Options])
    end.



apply_start_stop(Function, Args, Procs, Options) ->
    Ref = make_ref(),
    Parent = self(),
    Child = 
	spawn(
	  fun() ->
		  MRef = erlang:monitor(process, Parent),
		  receive
		      {Parent, Ref, start_trace} ->
			  case trace([start, 
				      {procs, [Parent | Procs]} 
				      | Options]) of
			      ok ->
				  catch Parent ! {self(), Ref, trace_started},
				  receive
				      {Parent, Ref, stop_trace} ->
					  trace([stop]),
					  catch Parent 
					      ! {self(), Ref, trace_stopped},
					  done;
				      {'DOWN', MRef, _, _, _} ->
					  trace([stop])
				  end;
			      {error, Reason} ->
				  exit(Reason)
			  end;
		      {'DOWN', MRef, _, _, _} ->
			  done
		  end
	  end),
    MRef = erlang:monitor(process, Child),
    catch Child ! {self(), Ref, start_trace},
    receive
	{Child, Ref, trace_started} ->
	    try erlang:apply(Function, Args)
	    after
		catch Child ! {self(), Ref, stop_trace},
	        receive
		    {Child, Ref, trace_stopped} ->
			receive
			    {'DOWN', MRef, _, _, _} ->
				ok
			end;
		    {'DOWN', MRef, _, _, _} ->
			trace([stop])
		end
	    end;
	{'DOWN', MRef, _, _, Reason} ->
	    exit(Reason)
    end.

apply_continue(Function, Args, Procs, Options) ->
    Ref = make_ref(),
    Parent = self(),
    Child = 
	spawn(
	  fun() ->
		  MRef = erlang:monitor(process, Parent),
		  receive
		      {Parent, Ref, start_trace} ->
			  case trace([start, 
				      {procs, [Parent | Procs]} 
				      | Options]) of
			      ok ->
				  exit({Ref, trace_started});
			      {error, Reason} ->
				  exit(Reason)
			  end;
		      {'DOWN', MRef, _, _, _} ->
			  done
		  end
	  end),
    MRef = erlang:monitor(process, Child),
    catch Child ! {self(), Ref, start_trace},
    receive
	{'DOWN', MRef, _, _, {Ref, trace_started}} ->
	    erlang:apply(Function, Args);
	{'DOWN', MRef, _, _, Reason} ->
	    exit(Reason)
    end.



%%%----------------------------------------------------------------------
%%% Requests to ?FPROF_SERVER
%%%----------------------------------------------------------------------

-record(trace_start, {procs,  % List of processes
		      mode,   % normal | verbose
		      type,   % file | tracer
		      dest}). % Filename | Pid/Port

-record(trace_stop, {}).

% -record(open_out, {file}).

% -record(close_out, {}).

-record(profile, {src,          % Filename
		  group_leader, % IoPid
		  dump,         % Filename | IoPid
		  flags}).      % List

-record(profile_start, {group_leader, % IoPid
			dump,         % Filename | IoPid
			flags}).      % List

-record(profile_stop, {}).

-record(analyse, {group_leader, % IoPid
		  dest,         % Filename | IoPid
		  flags,        % List
		  cols,         % Integer
		  callers,      % Boolean
		  sort,         % acc_r | own_r
		  totals,       % Boolean
		  details}).    % Boolean

-record(stop, {
	 reason}).



%%---------------
%% Debug requests
%%---------------

-record(get_state, {}).

-record(save_profile, {file}).

-record(load_profile, {file}).



%%%----------------------------------------------------------------------
%%% Basic API functions
%%%----------------------------------------------------------------------


-dialyzer({no_contracts, trace/2}).
-doc """
trace(What, Value)

Starts or stop tracing.

If `What` is atom `start`, this call is equivalent to
[`trace([start, {file, Value}])`](`trace/1`).

If `What` is atom `verbose`, this call is equivalent to
[`trace([start, verbose, {file, Value}])`](`trace/1`).

If `What` is a tuple `{OptionName, OptionValue}`,
this call is equivalent to
[`trace([What])`](`trace/1`).
""".
-spec trace('start', Filename) -> 'ok' |
                                  {'error', Reason} |
                                  {'EXIT', ServerPid, Reason} when
                Filename :: file:filename(),
                ServerPid :: pid(),
                Reason :: term();
           ('verbose', Filename) -> 'ok' |
                                    {'error', Reason} |
                                    {'EXIT', ServerPid, Reason} when
                Filename :: file:filename(),
                ServerPid :: pid(),
                Reason :: term();
           (OptionName, OptionValue) -> 'ok' |
                                     {'error', Reason} |
                                     {'EXIT', ServerPid, Reason} when
                OptionName :: atom(),
                OptionValue :: term(),
                ServerPid :: pid(),
                Reason :: term().

trace(start, Filename) ->
    trace([start, {file, Filename}]);
trace(verbose, Filename) ->
    trace([start, verbose, {file, Filename}]);
trace(OptionName, Value) when is_atom(OptionName) ->
    trace([{OptionName, Value}]);
trace(OptionName, Value) ->
    erlang:error(badarg, [OptionName, Value]).

-dialyzer({no_contracts, trace/1}).
-doc """
trace(Arg)

Starts or stops tracing.

If `Arg` is atom `verbose`, this call is equivalent to
`trace([start, verbose])`.

If `Arg` is an atom, this call is equivalent to
`trace([Arg])`.

If `Arg` is a tuple `{OptionName, OptionValue}`, this call is equivalent to
`trace([Arg])`.

Otherwise, `Arg` has to be a list of [trace options](`t:trace_option/0`).

`PidSpec` and `Tracer` are used in calls to
[`erlang:trace(PidSpec, true, [{tracer, Tracer} | Flags])`](`erlang:trace/3`),
and `Filename` is used to call
[`dbg:trace_port(file, Filename)`](`dbg:trace_port/2`).

Option description:

- **`stop`** - Stops a running `fprof` trace and clears all tracing from the
  node. Either option `stop` or `start` must be specified, but not both.

- **`start`** - Clears all tracing from the node and starts a new `fprof` trace.
  Either option `start` or `stop` must be specified, but not both.

- **`verbose` | `{verbose, boolean()}`** - The `verbose` or
  `{verbose, true}` options add some trace flags that `fprof` does not need, but that
  can be interesting for general debugging purposes. These options are only allowed
  with the `start` option.

- **`cpu_time` | `{cpu_time, boolean()}`** - The `cpu_time` or
  `{cpu_time, true}` options make the timestamps in the trace be in CPU time instead of
  the default wallclock time. These options are only allowed with the
  `start` option.

  > #### Note {: .info }
  >
  > Getting correct values out of `cpu_time` can be difficult. The best way to get
  > correct values is to run using a single scheduler and bind that scheduler to
  > a specific CPU. For example:
  >
  > ```bash
  > erl +S 1 +sbt db`
  > ```

- **`{procs, PidSpec}` | `{procs, [PidSpec]}`** - Specifies which processes that
  should be traced. If this option is not given, the calling process is traced.
  All processes spawned by the traced processes are also traced. This option is
  only allowed with the `start` option.

- **`file` | `{file, Filename}`** - Specifies the filename of the trace. If the
  option `file` is given, or none of these options are given, the file
  `fprof.trace` is used. This option is only allowed with the `start` option,
  but not with the `{tracer, Tracer}` option.

- **`{tracer, Tracer}`** - Specifies that trace to process or port shall be done
  instead of trace to file. This option is only allowed with the `start` option,
  but not with the `{file, Filename}` option.
""".
-spec trace('verbose') -> 'ok' |
                        {'error', Reason} |
                        {'EXIT', ServerPid, Reason} when
                ServerPid :: pid(),
                Reason :: term();
           (OptionName) -> 'ok' |
                           {'error', Reason} |
                           {'EXIT', ServerPid, Reason} when
                OptionName :: atom(),
                ServerPid :: pid(),
                Reason :: term();
           ({OptionName, OptionValue}) -> 'ok' |
                                          {'error', Reason} |
                                          {'EXIT', ServerPid, Reason} when
                OptionName :: atom(),
                OptionValue :: term(),
                ServerPid :: pid(),
                Reason :: term();
           (OptionList) -> 'ok' |
                           {'error', Reason} |
                           {'EXIT', ServerPid, Reason} when
                OptionList :: [Option],
                Option :: trace_option(),
                ServerPid :: pid(),
                Reason :: term().

trace(stop) ->
    %% This shortcut is present to minimize the number of undesired
    %% function calls at the end of the trace.
    call(#trace_stop{});
trace(verbose) ->
    trace([start, verbose]);
trace([stop]) ->
    %% This shortcut is present to minimize the number of undesired
    %% function calls at the end of the trace.
    call(#trace_stop{});
trace({Opt, _Val} = Option) when is_atom(Opt) ->
    trace([Option]);
trace(Option) when is_atom(Option) ->
    trace([Option]);
trace(Options) when is_list(Options) ->
    case getopts(Options, 
		 [start, stop, procs, verbose, file, tracer, cpu_time]) of
	{[[], [stop], [], [], [], [], []], []} ->
	    call(#trace_stop{});
	{[[start], [], Procs, Verbose, File, Tracer, CpuTime], []} ->
	    {Type, Dest} = case {File, Tracer} of
			       {[], [{tracer, Pid} = T]} 
			       when is_pid(Pid); is_port(Pid) ->
				   T;
			       {[file], []} ->
				   {file, ?TRACE_FILE};
			       {[{file, []}], []} ->
				   {file, ?TRACE_FILE};
			       {[{file, _} = F], []} ->
				   F;
			       {[], []} ->
				   {file, ?TRACE_FILE};
			       _ ->
				   erlang:error(badarg, [Options])
			   end,
	    V = case Verbose of
		       [] -> normal;
		       [verbose] -> verbose;
		       [{verbose, true}] -> verbose;
		       [{verbose, false}] -> normal;
		       _ -> erlang:error(badarg, [Options])
		   end,
	    CT = case CpuTime of
		     [] -> wallclock;
		     [cpu_time] -> cpu_time;
		     [{cpu_time, true}] -> cpu_time;
		     [{cpu_time, false}] -> wallclock;
		     _ -> erlang:error(badarg, [Options])
		 end,
	    call(#trace_start{procs = case Procs of
					  [] ->
					      [self()];
					  [{procs, P}] when is_list(P) ->
					      P;
					  [{procs, P}] ->
					      [P];
					  _ ->
					      erlang:error(badarg, [Options])
				      end,
			      mode = {V, CT},
			      type = Type,
			      dest = Dest});
	_ ->
	    erlang:error(badarg, [Options])
    end;
trace(Options) ->
    erlang:error(badarg, [Options]).


-doc #{equiv => profile([])}.
-spec profile() -> 'ok' |
                   {'error', Reason} |
                   {'EXIT', ServerPid, Reason} when
                ServerPid :: pid(),
                Reason :: term().

profile() ->
    profile([]).

-type profile_option() :: 'append'
                        | 'dump'
                        | {'dump',
                           pid() | Dump :: (Dumpfile :: file:filename() | [])}
                        | 'file'
                        | {'file', Filename :: file:filename()}
                        | 'start'
                        | 'stop'.

-doc #{equiv => profile([{OptionName, OptionValue}])}.
-spec profile(OptionName, OptionValue) ->'ok' |
                                         {'ok', Tracer} |
                                         {'error', Reason} |
                                         {'EXIT', ServerPid, Reason} when
      OptionName :: atom(),
      OptionValue :: term(),
      Tracer :: pid(),
      ServerPid :: pid(),
      Reason :: term().

profile(Option, Value) when is_atom(Option) ->
    profile([{Option, Value}]);
profile(Option, Value) ->
    erlang:error(badarg, [Option, Value]).

-doc """
profile(Arg)

Compiles a trace into raw profile data held by the `fprof` server.

If `Arg` is an atom, this call is equivalent to `profile([Arg])`.

If `Arg` is a tuple `{OptionName, OptionValue}`,
this call is equivalent to `profile([Arg])`.

Otherwise, `Arg` must be a list of options.

`Dumpfile` is used to call `file:open/2`, and `Filename` is used to call
[`dbg:trace_port(file, Filename)`](`dbg:trace_port/2`).

The following options are supported:

- **`file` | `{file, Filename}`** - Reads the file `Filename` and creates raw
  profile data that is stored in RAM by the `fprof` server. If the option `file`
  is given, or none of these options are given, the file `fprof.trace` is
  read. The call will return when the whole trace has been read with the return
  value `ok` if successful. This option is not allowed with the `start` or
  `stop` options.

- **`dump` | `{dump, Dump}`** - Specifies the destination for the trace text
  dump. If this option is not given, no dump is generated, if it is `dump` the
  destination will be the caller's group leader, otherwise the destination
  `Dump` is either the pid of an I/O device or a filename. If the
  filename is `[]`, `"fprof.dump"` is used instead. This option cannot be
  combined with the `stop` option.

- **`append`** - Causes the trace text dump to be appended to the destination
  file. This option is only allowed with the `{dump, Dumpfile}` option.

- **`start`** - Starts a tracer process that profiles trace data in runtime. The
  call will return immediately with the return value `{ok, Tracer}` if
  successful. This option is not allowed with the `stop`, `file`, or
  `{file, Filename}` options.

- **`stop`** - Stops the tracer process that profiles trace data in runtime. The
  return value will be value `ok` if successful. This option cannot be combined
  with the `start`, `file`, or `{file, Filename}` options.
""".
-spec profile(OptionName) -> 'ok' |
                             {'ok', Tracer} |
                             {'error', Reason} |
                             {'EXIT', ServerPid, Reason} when
                  OptionName :: atom(),
                  Tracer :: pid(),
                  ServerPid :: pid(),
                  Reason :: term();
           ({OptionName, OptionValue}) -> 'ok' |
                                          {'ok', Tracer} |
                                          {'error', Reason} |
                                          {'EXIT', ServerPid, Reason} when
                  OptionName :: atom(),
                  OptionValue :: term(),
                  Tracer :: pid(),
                  ServerPid :: pid(),
                  Reason :: term();
           (OptionList) -> 'ok' |
                           {'ok', Tracer} |
                           {'error', Reason} |
                           {'EXIT', ServerPid, Reason} when
                  OptionList :: [Option],
                  Option :: profile_option(),
                  Tracer :: pid(),
                  ServerPid :: pid(),
                  Reason :: term().

profile(Option) when is_atom(Option) ->
    profile([Option]);
profile({Opt, _Val} = Option) when is_atom(Opt) ->
    profile([Option]);
profile(Options) when is_list(Options) ->
    case getopts(Options, [start, stop, file, dump, append]) of
	{[Start, [], File, Dump, Append], []} ->
	    {Target, Flags} = 
		case {Dump, Append} of
		    {[], []} ->
			{[], []};
		    {[dump], []} ->
			{group_leader(), []};
		    {[{dump, []}], []} ->
			{?DUMP_FILE, []};
		    {[{dump, []}], [append]} ->
			{?DUMP_FILE, [append]};
		    {[{dump, D}], [append]} when is_pid(D) ->
			erlang:error(badarg, [Options]);
		    {[{dump, D}], [append]} ->
			{D, [append]};
		    {[{dump, D}], []} ->
			{D, []};
		    _ ->
			erlang:error(badarg, [Options])
		end,
	    case {Start, File} of
		{[start], []} ->
		    call(#profile_start{group_leader = group_leader(),
					dump = Target,
					flags = Flags});
		{[], _} ->
		    Src = 
			case File of
			    [] ->
				?TRACE_FILE;
			    [file] ->
				?TRACE_FILE;
			    [{file, []}] ->
				?TRACE_FILE;
			    [{file, F}] ->
				F;
			    _ ->
				erlang:error(badarg, [Options])
			end,
		    call(#profile{src = Src,
				  group_leader = group_leader(),
				  dump = Target,
				  flags = Flags});
		_ ->
		    erlang:error(badarg, [Options])
	    end;
	{[[], [stop], [], [], []], []} ->
	    call(#profile_stop{});
	_ ->
	    erlang:error(badarg, [Options])
    end;
profile(Options) ->
    erlang:error(badarg, [Options]).


-doc #{equiv => analyse([])}.
-spec analyse() ->  'ok' |
                    {'error', Reason} |
                    {'EXIT', ServerPid, Reason} when
      ServerPid :: pid(),
      Reason :: term().

analyse() ->
    analyse([]).

-doc #{equiv => analyse([{OptionName, OptionValue}])}.
-spec analyse(OptionName, OptionValue) ->'ok' |
                                         {'error', Reason} |
                                         {'EXIT', ServerPid, Reason} when
      OptionName :: atom(),
      OptionValue :: term(),
      ServerPid :: pid(),
      Reason :: term().

analyse(Option, Value) when is_atom(Option) ->
    analyse([{Option, Value}]);
analyse(Option, Value) ->
    erlang:error(badarg, [Option, Value]).

-type analyse_option() :: 'append'
                        | 'callers'
                        | {'callers', boolean()}
                        | {'cols', Cols :: non_neg_integer()}
                        | 'dest'
                        | {'dest',
                           Dest :: (pid() | (Destfile :: file:filename()))}
                        | 'details'
                        | {'details', boolean()}
                        | 'no_callers'
                        | 'no_details'
                        | {'sort', SortSpec :: 'acc' | 'own'}
                        | 'totals'
                        | {'totals', boolean()}.

-doc """
analyse(Arg)

Analyses raw profile data in the `fprof` server.

If `Arg` is an atom, this call is equivalent to `analyse([Arg])`.

If `Arg` is a a tuple `{Option, _}`, this call is equivalent to
`analyse([Option])`.

Otherwise `Arg` must be a list of valid options.

If called when no raw profile data is available, `{error, no_profile}`
is returned.

`Destfile` is used to call `file:open/2`.

Option description:

- **`dest` | `{dest, Dest}`** - Specifies the destination for the analysis. If
  this option is not given or it is `dest`, the destination will be the caller's
  group leader, otherwise the destination `Dest` is either the `t:pid/0` of an
  I/O device or a filename. If the filename is `[]`, `"fprof.analysis"` is used
  instead.

- **`append`** - Causes the analysis to be appended to the destination file.
  This option is only allowed with the `{dest, Destfile}` option.

- **`{cols, Cols}`** - Specifies the number of columns in the analysis text. If
  this option is not given the number of columns is set to 80.

- **`callers` | `{callers, true}`** - Prints callers and called information in
  the analysis. This is the default.

- **`{callers, false}` | `no_callers`** - Suppresses the printing of callers and
  called information in the analysis.

- **`{sort, SortSpec}`** - Specifies if the analysis should be sorted according
  to the ACC column, which is the default, or the OWN column. See
  [Analysis Format](`m:fprof#analysis`) below.

- **`totals` | `{totals, true}`** - Includes a section containing call
  statistics for all calls regardless of process, in the analysis.

- **`{totals, false}`** - Suppresses the totals section in the analysis, which
  is the default.

- **`details` | `{details, true}`** - Prints call statistics for each process in
  the analysis. This is the default.

- **`{details, false}` | `no_details`** - Suppresses the call statistics for
  each process from the analysis.
""".
-spec analyse(OptionName) -> 'ok' |
                             {'error', Reason} |
                             {'EXIT', ServerPid, Reason} when
                  OptionName :: atom(),
                  ServerPid :: pid(),
                  Reason :: term();
           ({OptionName, OptionValue}) -> 'ok' |
                                          {'error', Reason} |
                                          {'EXIT', ServerPid, Reason} when
                  OptionName :: atom(),
                  OptionValue :: term(),
                  ServerPid :: pid(),
                  Reason :: term();
           (OptionList) -> 'ok' |
                           {'error', Reason} |
                           {'EXIT', ServerPid, Reason} when
                  OptionList :: [Option],
                  Option :: analyse_option(),
                  ServerPid :: pid(),
                  Reason :: term().

analyse(Option) when is_atom(Option) ->
    analyse([Option]);
analyse({Opt, _Val} = Option) when is_atom(Opt) ->
    analyse([Option]);
analyse(Options) when is_list(Options) ->
    case getopts(Options, 
		 [dest, append, cols, callers, no_callers, 
		  sort, totals, details, no_details]) of
	{[Dest, Append, Cols, Callers, NoCallers,
	  Sort, Totals, Details, NoDetails], []} ->
	    {Target, Flags} = 
		case {Dest, Append} of
		    {[], []} ->
			{group_leader(), []};
		    {[dest], []} ->
			{group_leader(), []};
		    {[{dest, []}], []} ->
			{?ANALYSIS_FILE, []};
		    {[{dest, []}], [append]} ->
			{?ANALYSIS_FILE, [append]};
		    {[{dest, F}], [append]} when is_pid(F) ->
			erlang:error(badarg, [Options]);
		    {[{dest, F}], [append]} ->
			{F, [append]};
		    {[{dest, F}], []} ->
			{F, []};
		    _ ->
			erlang:error(badarg, [Options])
		end,
	    call(#analyse{group_leader = group_leader(),
			  dest = Target,
			  flags = Flags,
			  cols = case Cols of
				     [] ->
					 80;
				     [{cols, C}] when is_integer(C), C > 0 ->
					 C;
				     _ ->
					 erlang:error(badarg, [Options])
				 end,
			  callers = case {Callers, NoCallers} of
					{[], []} -> 
					    true;
					{[callers], []} ->
					    true;
					{[{callers, true}], []} ->
					    true;
					{[{callers, false}], []} ->
					    false;
					{[], [no_callers]} ->
					    false;
					_ ->
					    erlang:error(badarg, [Options])
				    end,
			  sort = case Sort of
				     [] -> 
					 acc;
				     [{sort, acc}] ->
					 acc;
				     [{sort, own}] ->
					 own;
				     _ ->
					 erlang:error(badarg, [Options])
				 end,
			  totals = case Totals of
				       [] -> 
					   false;
				       [totals] ->
					   true;
				       [{totals, true}] ->
					   true;
				       [{totals, false}] ->
					   false;
				       _ ->
					   erlang:error(badarg, [Options])
				   end,
			  details = case {Details, NoDetails} of
					{[], []} ->
					    true;
					{[details], []} ->
					    true;
					{[{details, true}], []} ->
					    true;
					{[{details, false}], []} ->
					    false;
					{[], [no_details]} ->
					    false;
				       _ ->
					   erlang:error(badarg, [Options])
				    end});
  	_ ->
	    erlang:error(badarg, [Options])
    end;
analyse(Options) ->
    erlang:error(badarg, [Options]).



%%----------------
%% Debug functions
%%----------------



-doc false.
get_state() ->
    just_call(#get_state{}).



-doc false.
save_profile() ->
    save_profile([]).

-doc false.
save_profile(Option, Value) when is_atom(Option) ->
    save_profile([{Option, Value}]);
save_profile(Option, Value) ->
    erlang:error(badarg, [Option, Value]).

-doc false.
save_profile(Option) when is_atom(Option) ->
    save_profile([Option]);
save_profile(Options) when is_list(Options) ->
    case getopts(Options, [file]) of
	{[File], []} ->
	    call(#save_profile{file = case File of
					  [] -> 
					      ?PROFILE_FILE;
					  [{file, F}] ->
					      F;
					  _ ->
					      erlang:error(badarg, [Options])
				      end});
  	_ ->
	    erlang:error(badarg, [Options])
    end;
save_profile(Options) ->
    erlang:error(badarg, [Options]).



-doc false.
load_profile() ->
    load_profile([]).

-doc false.
load_profile(Option, Value) when is_atom(Option) ->
    load_profile([{Option, Value}]);
load_profile(Option, Value) ->
    erlang:error(badarg, [Option, Value]).

-doc false.
load_profile(Option) when is_atom(Option) ->
    load_profile([Option]);
load_profile(Options) when is_list(Options) ->
    case getopts(Options, [file]) of
	{[File], []} ->
	    call(#load_profile{file = case File of
					  [] -> 
					      ?PROFILE_FILE;
					  [{file, F}] ->
					      F;
					  _ ->
					      erlang:error(badarg, [Options])
				      end});
  	_ ->
	    erlang:error(badarg, [Options])
    end;
load_profile(Options) ->
    erlang:error(badarg, [Options]).



-doc false.
code_change() ->
    just_call('$code_change').



%%%----------------------------------------------------------------------
%%% ETS table record definitions
%%% The field 'id' must be first in these records;
%%% it is the common ets table index field.
%%%----------------------------------------------------------------------

-record(clocks, {
	  id,
	  cnt = 0,   % Number of calls
	  own = 0,   % Own time (wall clock)
	  acc = 0}). % Accumulated time : own + subfunctions (wall clock)

-record(proc, {
	  id,
	  parent,
	  spawned_as,     % Spawned MFArgs
	  init_log = [],  % List of first calls, head is newest
	  init_cnt = 2}). % First calls counter, counts down to 0

-record(misc, {id, 
	       data}).



%% Analysis summary record
-record(funcstat, {
	  callers_sum,   % #clocks{id = {Pid, Caller, Func}}
	  called_sum,    % #clocks{id = {Pid, Caller, Func}}
	  callers = [],  % [#clocks{}, ...]
	  called = []}). % [#clocks{}, ...]



%%%----------------------------------------------------------------------
%%% ?FPROF_SERVER
%%%----------------------------------------------------------------------

%%%-------------------
%%% Exported functions
%%%-------------------

-doc """
Starts the `fprof` server.

Note that there is seldom any need to call this function directly, since
the server will be automatically started by any function that will need it.
""".
-spec start() -> {'ok', Pid} | {'error', {'already_started', Pid}} when
      Pid :: pid().

%% Start server process
start() ->
    spawn_3step(
      fun () ->
	      try register(?FPROF_SERVER, self()) of
		  true ->
		      process_flag(trap_exit, true),
		      {{ok, self()}, loop}
	      catch
		  error:badarg ->
		      {{error, {already_started, whereis(?FPROF_SERVER)}},
		       already_started}
	      end
      end,
      fun (X) ->
	      X
      end,
      fun (loop) ->
	      put(trace_state, idle),
	      put(profile_state, {idle, undefined}),
	      put(pending_stop, []),
	      server_loop([]);
	  (already_started) ->
	      ok
      end).


-doc #{equiv => stop(normal)}.
-spec stop() -> 'ok'.

%% Stop server process
stop() ->
    stop(normal).

-doc """
Stops the `fprof` server.

The supplied `Reason` becomes the exit reason for the server process. By default,
any `Reason` other than `kill` sends a request to the server and waits for it to
clean up, reply, and exit. If `Reason` is `kill`, the server is bluntly killed.

If the `fprof` server is not running, this function returns immediately.

> #### Note {: .info }
>
> When the `fprof` server is stopped the collected raw profile data is lost.
""".
-spec stop(Reason) -> 'ok' when
      Reason :: term().

stop(kill) ->
    case whereis(?FPROF_SERVER) of
	undefined ->
	    ok;
	Pid ->
	    exit(Pid, kill),
	    ok
    end;
stop(Reason) ->
    just_call(#stop{reason = Reason}),
    ok.



%%%------------------------
%%% Client helper functions
%%%------------------------

%% Send request to server process and return the server's reply.
%% First start server if it ain't started.
-doc false.
call(Request) ->
    case whereis(?FPROF_SERVER) of
	undefined ->
	    _ = start(),
	    just_call(Request);
	Server ->
	    just_call(Server, Request)
    end.

%% Send request to server process, and return the server's reply.
%% Returns {'EXIT', Pid, Reason} if the server dies during the
%% call, or if it wasn't started.
-doc false.
just_call(Request) ->
    just_call(whereis(?FPROF_SERVER), Request).

just_call(undefined, _) ->
    {'EXIT', ?FPROF_SERVER, noproc};
just_call(Pid, Request) ->
    Mref = erlang:monitor(process, Pid),
    receive
	{'DOWN', Mref, _, _, Reason} ->
	    {'EXIT', Pid, Reason}
    after 0 ->
	    Tag = {Mref, self()},
	    {T, Demonitor} = case Request of
				 #stop{} ->
				     {?FPROF_SERVER_TIMEOUT, false};
				 _ ->
				     {0, true}
			     end,
	    %% io:format("~p request: ~p~n", [?MODULE, Request]),
	    catch Pid ! {?FPROF_SERVER, Tag, Request},
	    receive
		{?FPROF_SERVER, Mref, Reply} ->
		    case Demonitor of
			true -> erlang:demonitor(Mref);
			false -> ok
		    end,
		    receive {'DOWN', Mref, _, _, _} -> ok after T -> ok end,
		    Reply;
		{'DOWN', Mref, _, _, Reason} ->
		    receive {?FPROF_SERVER, Mref, _} -> ok after T -> ok end,
		    {'EXIT', Pid, Reason}
	    after ?FPROF_SERVER_TIMEOUT ->
		    timeout
	    end
    end.



%%%------------------------
%%% Server helper functions
%%%------------------------

%% Return the reply to the client's request.
-doc false.
reply({Mref, Pid}, Reply) when is_reference(Mref), is_pid(Pid) ->
    catch Pid ! {?FPROF_SERVER, Mref, Reply},
    ok.



server_loop(State) ->    
    receive 
	{?FPROF_SERVER, {Mref, Pid} = Tag, '$code_change'} 
	when is_reference(Mref), is_pid(Pid) ->
	    reply(Tag, ok),
	    ?MODULE:'$code_change'(State);
	{?FPROF_SERVER, {Mref, Pid} = Tag, Request} 
	when is_reference(Mref), is_pid(Pid) ->
	    server_loop(handle_req(Request, Tag, State));
	Other ->
	    server_loop(handle_other(Other, State))
    end.

%-export.
-doc false.
'$code_change'(State) ->
    case lists:keysearch(time, 1, module_info(compile)) of
	{value, {time, {Y, M, D, HH, MM, SS}}} ->
	    io:format("~n~w: code change to compile time "
		      ++"~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w~n",
		      [?MODULE, Y, M, D, HH, MM, SS]);
	false ->
	    ok
    end,
    server_loop(State).



%% Server help function that stops the server iff the
%% sub state machines are in proper states. Sends the reply
%% to all waiting clients.
try_pending_stop(State) ->
    case {get(trace_state), get(profile_state), get(pending_stop)} of
	{idle, {idle, _}, [_|_] = PendingStop} ->
	    Reason = get(stop_reason),
	    Reply = result(Reason),
	    lists:foreach(
	      fun (Tag) ->
		      reply(Tag, Reply)
	      end,
	      PendingStop),
	    exit(Reason);
	_ ->
	    State
    end.

%%------------------
%% Server handle_req			    
%%------------------

handle_req(#trace_start{procs = Procs,
			mode = Mode,
			type = file,
			dest = Filename}, Tag, State) ->
    case {get(trace_state), get(pending_stop)} of
	{idle, []} ->
	    trace_off(),
	    Port = open_dbg_trace_port(file, Filename),
	    case trace_on(Procs, Port, Mode) of
		ok ->
		    put(trace_state, running),
		    put(trace_type, file),
		    put(trace_pid, Port),
		    reply(Tag, ok),
		    State;
		Error ->
		    reply(Tag, Error),
		    State
	    end;
	_ ->
	    reply(Tag, {error, already_tracing}),
	    State
    end;
handle_req(#trace_start{procs = Procs,
			mode = Mode,
			type = tracer,
			dest = Tracer}, Tag, State) ->
    case {get(trace_state), get(pending_stop)} of
	{idle, []} ->
	    trace_off(),
	    case trace_on(Procs, Tracer, Mode) of
		ok ->
		    put(trace_state, running),
		    put(trace_type, tracer),
		    put(trace_pid, Tracer),
		    reply(Tag, ok),
		    State;
		Error ->
		    reply(Tag, Error),
		    State
	    end;
	_ ->
	    reply(Tag, {error, already_tracing}),
	    State
    end;

handle_req(#trace_stop{}, Tag, State) ->
    case get(trace_state) of
	running ->
	    TracePid = get(trace_pid),
	    trace_off(),
	    case erase(trace_type) of
		file ->
		    catch erlang:port_close(TracePid),
		    put(trace_state, stopping),
		    put(trace_tag, Tag),
		    State;
		tracer ->
		    erase(trace_pid),
		    put(trace_state, idle),
		    case {get(profile_state), get(profile_type), 
			  get(profile_pid)} of
			{running, tracer, TracePid} ->
			    exit(TracePid, normal),
			    put(profile_tag, Tag),
			    State;
			_ ->
			    reply(Tag, ok),
			    try_pending_stop(State)
		    end
	    end;
	_ ->
	    reply(Tag, {error, not_tracing}),
	    State
    end;

handle_req(#profile{src = Filename,
		    group_leader = GroupLeader,
		    dump = Dump,
		    flags = Flags}, Tag, State) ->
    case {get(profile_state), get(pending_stop)} of
	{{idle, _}, []} ->
	    case ensure_open(Dump, [write | Flags]) of
		{already_open, DumpPid} ->
		    put(profile_dump, DumpPid),
		    put(profile_close_dump, false);
		{ok, DumpPid} ->
		    put(profile_dump, DumpPid),
		    put(profile_close_dump, true);
		{error, _} = Error ->
		    reply(Tag, Error),
		    State
	    end,
	    Table = ets:new(?MODULE, [set, public, {keypos, #clocks.id}]),
	    Pid = spawn_link_dbg_trace_client(Filename, Table, 
					      GroupLeader, 
					      get(profile_dump)),
	    put(profile_state, running),
	    put(profile_type, file),
	    put(profile_pid, Pid),
	    put(profile_tag, Tag),
	    put(profile_table, Table),
	    State;
	_ ->
	    reply(Tag, {error, already_profiling}),
	    State
    end;
	    
handle_req(#profile_start{group_leader = GroupLeader,
			  dump = Dump,
			  flags = Flags}, Tag, State) ->
    case {get(profile_state), get(pending_stop)} of
	{{idle, _}, []} ->
	    case ensure_open(Dump, [write | Flags]) of
		{already_open, DumpPid} ->
		    put(profile_dump, DumpPid),
		    put(profile_close_dump, false);
		{ok, DumpPid} ->
		    put(profile_dump, DumpPid),
		    put(profile_close_dump, true);
		{error, _} = Error ->
		    reply(Tag, Error),
		    State
	    end,
	    Table = ets:new(?MODULE, [set, public, {keypos, #clocks.id}]),
	    Pid = spawn_link_trace_client(Table, GroupLeader, 
					  get(profile_dump)),
	    put(profile_state, running),
	    put(profile_type, tracer),
	    put(profile_pid, Pid),
	    put(profile_table, Table),
	    reply(Tag, {ok, Pid}),
	    State;
	_ ->
	    reply(Tag, {error, already_profiling}),
	    State
    end;

handle_req(#profile_stop{}, Tag, State) ->
    case {get(profile_state), get(profile_type)} of
	{running, tracer} ->
	    ProfilePid = get(profile_pid),
	    case {get(trace_state), get(trace_type), get(trace_pid)} of
		{running, tracer, ProfilePid} ->
		    trace_off(),
		    erase(trace_type),
		    erase(trace_pid),
		    put(trace_state, idle);
		_ ->
		    ok
	    end,
	    exit(ProfilePid, normal),
	    put(profile_tag, Tag),
	    State;
	{running, file} ->
	    reply(Tag, {error, profiling_file}),
	    State;
	{_, _} ->
	    reply(Tag, {error, not_profiling}),
	    State
    end;

handle_req(#analyse{dest = Dest,
		    flags = Flags} = Request, Tag, State) ->
    case get(profile_state) of
	{idle, undefined} ->
	    reply(Tag, {error, no_profile}),
	    State;
	{idle, _} ->
	    case ensure_open(Dest, [write | Flags]) of
		{error, _} = Error ->
		    reply(Tag, Error),
		    State;
		{DestState, DestPid} ->
		    ProfileTable = get(profile_table),
		    reply(Tag,
			  spawn_3step(
			    fun() ->
				    do_analyse(ProfileTable, 
					       Request#analyse{dest = DestPid})
			    end,
			    fun(Result) ->
				    {Result,finish}
			    end,
			    fun(finish) ->
				    ok
			    end)),
		    case DestState of
			already_open ->
			    ok;
			ok ->
			    ok = file:close(DestPid)
		    end,
		    State
	    end;
	_ ->
	    reply(Tag, {error, profiling}),
	    State
    end;

handle_req(#stop{reason = Reason}, Tag, State) ->
    PendingStop = get(pending_stop),
    case PendingStop of
	[] ->
	    put(stop_reason, Reason);
	_ ->
	    ok
    end,
    put(pending_stop, [Tag | PendingStop]),
    try_pending_stop(State);

%%----------------------
%% Server debug requests
%%----------------------

handle_req(#get_state{}, Tag, State) ->
    reply(Tag, {ok, get()}),
    State;

handle_req(#save_profile{file = File}, Tag, State) ->
    case get(profile_state) of
	{idle, undefined} ->
	    reply(Tag, {error, no_profile});
	{idle, _} ->
	    reply(Tag, ets:tab2file(get(profile_table), File)),
	    State;
	_ ->
	    reply(Tag, {error, profiling}),
	    State
    end;

handle_req(#load_profile{file = File}, Tag, State) ->
    case get(profile_state) of
	{idle, Result} ->
	    case ets:file2tab(File) of
		{ok, Table} ->
		    put(profile_state, {idle, ok}),
		    case Result of
			{error, no_profile} ->
			    ets:delete(put(profile_table, Table));
			_ ->
			    put(profile_table, Table)
		    end,
		    reply(Tag, ok),
		    State;
		Error ->
		    reply(Tag, Error),
		    State
	    end;
	_ ->
	    reply(Tag, {error, profiling}),
	    State
    end;

	    

handle_req(Request, Tag, State) ->
    io:format("~n~p:handle_req, unknown request - ~p~n", 
	      [?MODULE, Request]),
    reply(Tag, {error, unknown_request}), 
    State.

%%--------------------
%% Server handle_other
%%--------------------

handle_other({'EXIT', Pid, Reason} = Other, State) when is_pid(Pid); is_port(Pid) ->
    case {get(trace_state), get(trace_pid)} of
	{running, Pid} ->
	    trace_off(),
	    io:format("~n~p:handle_other, unexpected ~p (trace_pid)~n",
		      [?MODULE, Other]),
	    put(trace_state, idle),
	    erase(trace_type),
	    erase(trace_pid),
	    try_pending_stop(State);
	{stopping, Pid} ->
	    put(trace_state, idle),
	    erase(trace_pid),
	    reply(erase(trace_tag), result(Reason)),
	    try_pending_stop(State);
	_ ->
	    case {get(profile_state), get(profile_pid)} of
		{running, Pid} ->
		    Result = result(Reason),
		    put(profile_state, {idle, Result}),
		    erase(profile_type),
		    erase(profile_pid),
		    case erase(profile_close_dump) of
			true ->
			    file:close(erase(profile_dump));
			false ->
			    erase(profile_dump)
		    end,
		    reply(erase(profile_tag), Result),
		    try_pending_stop(State);
		_ ->
		    io:format("~n~p:handle_other, unexpected ~p~n",
			      [?MODULE, Other]),
		    State
	    end
    end;

handle_other(Other, State) ->
    io:format("~p:handle_other, unknown - ~p", 
			  [?MODULE, Other]),
    State.



%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

result(normal) ->
    ok;
result(Reason) ->
    {error, Reason}.

ensure_open(Pid, _Options) when is_pid(Pid) ->
    {already_open, Pid};
ensure_open([], _Options) ->
    {already_open, undefined};
ensure_open(Filename, Options) when is_atom(Filename); is_list(Filename) ->
    file:open(Filename, [{encoding, utf8} | Options]).

%%%---------------------------------
%%% Fairly generic utility functions
%%%---------------------------------



%% getopts(List, Options)) -> {DecodedOptions, RestOptions}
%%
%% List           = [Option]
%% Options        = [OptionTag]
%% Option         = OptionTag | OptionTuple
%% OptionTuple    = tuple(), element(1, OptionTuple) == OptionTag
%% OptionTag      = term()
%% OptionValue    = term()
%% DecodedOptions = [OptionList]
%% OptionList     = [Option]
%% RestOptions    = [Option]
%%
%% Searches List for options with tags defined in Options.
%% Returns DecodedOptions containing one OptionList per
%% OptionTag in Options, and RestOptions which contains
%% all terms from List not matching any OptionTag.
%%
%% All returned lists preserve the order from Options and List.
%%
%% An example:
%%     getopts([{f, 1}, e, {d, 2}, {c, 3, 4}, {b, 5}, a, b],
%%             [a, b, c, d]) ->
%%         {[[a], [{b, 5}, b],[{c, 3, 4}], [{d, 2}]], 
%%          [{f, 1}, e]}
%%
-doc false.
getopts(List, Options) when is_list(List), is_list(Options) ->
    getopts_1(Options, List, []).

getopts_1([], List, Result) ->
    {lists:reverse(Result), List};
getopts_1([Option | Options], List, Result) ->
    {Optvals, Remaining} = getopts_2(List, Option, [], []),
    getopts_1(Options, Remaining, [Optvals | Result]).

getopts_2([], _Option, Result, Remaining) ->
    {lists:reverse(Result), lists:reverse(Remaining)};
getopts_2([Option | Tail], Option, Result, Remaining) ->
    getopts_2(Tail, Option, [Option | Result], Remaining);
getopts_2([Optval | Tail], Option, Result, Remaining) 
  when element(1, Optval) =:= Option ->
    getopts_2(Tail, Option, [Optval | Result], Remaining);
getopts_2([Other | Tail], Option, Result, Remaining) ->
    getopts_2(Tail, Option, Result, [Other | Remaining]).

%% setopts(Options) -> List
%%
%% The reverse of getopts, almost.
%% Re-creates (approximately) List from DecodedOptions in 
%% getopts/2 above. The original order is not preserved, 
%% but rather the order from Options.
%% 
%% An example:
%%     setopts([[a], [{b,5}, b], [{c, 3, 4}], [{d,2}]]) ->
%%         [a, {b, 5}, b, {c, 3, 4}, {d, 2}]
%%
%% And a more generic example:
%%     {D, R} = getopts(L, O),
%%     L2 = setopts(D) ++ R
%% L2 will contain exactly the same terms as L, but not in the same order.
%%
-doc false.
setopts(Options) when is_list(Options) ->
    lists:append(Options).



spawn_3step(FunPrelude, FunAck, FunBody) ->
    spawn_3step(spawn, FunPrelude, FunAck, FunBody).

spawn_link_3step(FunPrelude, FunAck, FunBody) ->
    spawn_3step(spawn_link, FunPrelude, FunAck, FunBody).

spawn_3step(Spawn, FunPrelude, FunAck, FunBody) 
  when Spawn =:= spawn; Spawn =:= spawn_link ->
    Parent = self(),
    Ref = make_ref(),
    Child = 
	erlang:Spawn(
	  fun() ->
		  Ack = FunPrelude(),
		  catch Parent ! {self(), Ref, Ack},
		  MRef = erlang:monitor(process, Parent),
		  receive
		      {Parent, Ref, Go} ->
			  erlang:demonitor(MRef, [flush]),
			  FunBody(Go);
		      {'DOWN', MRef, _, _, _} ->
			  ok
		  end
	  end),
    MRef = erlang:monitor(process, Child),
    receive
	{Child, Ref, Ack} ->
	    erlang:demonitor(MRef, [flush]),
	    try FunAck(Ack) of
		{Result, Go} ->
		    catch Child ! {Parent, Ref, Go},
		    Result
	    catch
		Class:Reason:Stacktrace ->
		    catch exit(Child, kill),
		    erlang:raise(Class, Reason, Stacktrace)
	    end;
	{'DOWN', MRef, _, _, Reason} ->
	    receive {Child, Ref, _Ack} -> ok after 0 -> ok end,
	    case Spawn of 
		spawn_link ->
		    receive {'EXIT', Reason} -> ok after 0 -> ok end;
		spawn ->
		    ok
	    end,
	    exit(Reason)
    end.



%%%---------------------------------
%%% Trace message handling functions
%%%---------------------------------

-doc false.
trace_off() ->
    try erlang:trace_delivered(all) of
	Ref -> receive {trace_delivered, all, Ref} -> ok end
    catch
	error:undef -> ok
    end,
    try erlang:trace(all, false, [all, cpu_timestamp])
    catch
	error:badarg -> erlang:trace(all, false, [all])
    end,
    erlang:trace_pattern(on_load, false, [local]),
    erlang:trace_pattern({'_', '_', '_'}, false, [local]),
    ok.



-doc false.
trace_on(Procs, Tracer, {V, CT}) ->
    case case CT of
	     cpu_time ->
		 try erlang:trace(all, true, [cpu_timestamp]) of _ -> ok
		 catch
		     error:badarg -> {error, not_supported}
		 end;
	     wallclock -> ok
	 end
	of ok ->
	    MatchSpec = [{'_', [], [{message, {{cp, {caller}}}}]}],
	    erlang:trace_pattern(on_load, MatchSpec, [local]),
	    erlang:trace_pattern({'_', '_', '_'}, MatchSpec, [local]),
	    lists:foreach(
	      fun (P) ->
		      erlang:trace(P, true, [{tracer, Tracer} | trace_flags(V)])
	      end,
	      Procs),
	    ok;
	Error ->
	    Error
    end.



trace_flags(normal) ->
    [call, return_to, 
     running, procs, garbage_collection, 
     arity, timestamp, set_on_spawn];
trace_flags(verbose) ->
    [call, return_to, 
     send, 'receive',
     running, procs, garbage_collection, 
     timestamp, set_on_spawn].



%%%-------------------------------------
%%% Tracer process functions, for
%%% the 'dbg' tracer and for a lookalike 
%%%-------------------------------------

open_dbg_trace_port(Type, Spec) ->
    Fun = dbg:trace_port(Type, Spec),
    Fun().



spawn_link_dbg_trace_client(File, Table, GroupLeader, Dump) ->
    case dbg:trace_client(file, File, 
			  {fun handler/2, 
			   {init, GroupLeader, Table, Dump}}) of
	Pid when is_pid(Pid) ->
	    link(Pid),
	    Pid
    end.

spawn_link_trace_client(Table, GroupLeader, Dump) ->
    Parent = self(),
    spawn_link_3step(
      fun() ->
	      process_flag(trap_exit, true),
	      {self(),go}
      end,
      fun(Ack) ->
	      Ack
      end,
      fun(go) ->
	      Init = {init, GroupLeader, Table, Dump},
	      tracer_loop(Parent, fun handler/2, Init)
      end).

tracer_loop(Parent, Handler, State) ->
    receive
	Trace when element(1, Trace) =:= trace ->
	    tracer_loop(Parent, Handler, Handler(Trace, State));
	Trace when element(1, Trace) =:= trace_ts ->
	    tracer_loop(Parent, Handler, Handler(Trace, State));
	{'EXIT', Parent, Reason} ->
	    _ = handler(end_of_trace, State),
	    exit(Reason);
	_ ->
	    tracer_loop(Parent, Handler, State)
    end.



%%%---------------------------------
%%% Trace message handling functions
%%%---------------------------------

handler(end_of_trace, {init, GroupLeader, Table, Dump}) ->
    dump(Dump, start_of_trace),
    dump(Dump, end_of_trace),
    info(GroupLeader, Dump, "Empty trace!~n", []),
    end_of_trace(Table, undefined),
    done;
handler(end_of_trace, {error, Reason, _, GroupLeader, Dump}) ->
    info(GroupLeader, Dump, "~nEnd of erroneous trace!~n", []),
    exit(Reason);
handler(end_of_trace, {_, TS, GroupLeader, Table, Dump}) ->
    dump(Dump, end_of_trace),
    info(GroupLeader, Dump, "~nEnd of trace!~n", []),
    end_of_trace(Table, TS),
    done;
handler(Trace, {init, GroupLeader, Table, Dump}) ->
    dump(Dump, start_of_trace),
    info(GroupLeader, Dump, "Reading trace data...~n", []),
    try trace_handler(Trace, Table, GroupLeader, Dump) of
	TS ->
	    ets:insert(Table, #misc{id = first_ts, data = TS}),
	    ets:insert(Table, #misc{id = last_ts_n, data = {TS, 1}}),
	    {1, TS, GroupLeader, Table, Dump}
    catch
	Error ->
	    dump(Dump, {error, Error}),
	    end_of_trace(Table, undefined),
	    {error, Error, 1, GroupLeader, Dump}
    end;
%%     case catch trace_handler(Trace, Table, GroupLeader, Dump) of
%% 	{'EXIT', Reason} ->
%% 	    dump(Dump, {error, Reason}),
%% 	    end_of_trace(Table, undefined),
%% 	    {error, Reason, 1, GroupLeader, Dump};
%% 	TS ->
%% 	    ets:insert(Table, #misc{id = first_ts, data = TS}),
%% 	    ets:insert(Table, #misc{id = last_ts_n, data = {TS, 1}}),
%% 	    {1, TS, GroupLeader, Table, Dump}
%%     end;
handler(_, {error, Reason, M, GroupLeader, Dump}) ->
    N = M+1,
    info_dots(GroupLeader, Dump, N),
    {error, Reason, N, GroupLeader, Dump};
handler(Trace, {M, TS0, GroupLeader, Table, Dump}) ->
    N = M+1,
    info_dots(GroupLeader, Dump, N),
    try trace_handler(Trace, Table, GroupLeader, Dump) of
	TS ->
	    ets:insert(Table, #misc{id = last_ts_n, data = {TS, N}}),
	    {N, TS, GroupLeader, Table, Dump}
    catch
	Error ->
	    dump(Dump, {error, Error}),
	    end_of_trace(Table, TS0),
	    {error, Error, N, GroupLeader, Dump}
    end.
%%     case catch trace_handler(Trace, Table, GroupLeader, Dump) of
%% 	{'EXIT', Reason} ->
%% 	    dump(Dump, {error, Reason}),
%% 	    end_of_trace(Table, TS0),
%% 	    {error, Reason, N, GroupLeader, Dump};
%% 	TS ->
%% 	    ets:insert(Table, #misc{id = last_ts_n, data = {TS, N}}),
%% 	    {N, TS, GroupLeader, Table, Dump}
%%     end.



end_of_trace(Table, TS) ->
    %%
    %% Close all process stacks, as if the processes exited.
    %%
    Procs = get(),
    put(table, Table),
    ?dbg(2, "get() -> ~p~n", [Procs]),
    _ = lists:map(fun ({Pid, _}) when is_pid(Pid) ->
                          trace_exit(Table, Pid, TS)
                  end, Procs),
    _ = erase(),
    ok.



info_dots(GroupLeader, GroupLeader, _) ->
    ok;
info_dots(GroupLeader, _, N) ->
    if (N rem 100000) =:= 0 ->
	    io:format(GroupLeader, ",~n", []);
       (N rem 50000) =:= 0 ->
	    io:format(GroupLeader, ".~n", []);
       (N rem 1000) =:= 0 ->
	    io:put_chars(GroupLeader, ".");
       true ->
	    ok
    end.

info_suspect_call(GroupLeader, GroupLeader, _, _) ->
    ok;
info_suspect_call(GroupLeader, _, Func, Pid) ->
    io:format(GroupLeader,
	      "~nWarning: ~tp called in ~p - trace may become corrupt!~n",
	      parsify([Func, Pid])).

info(GroupLeader, GroupLeader, _, _) ->
    ok;
info(GroupLeader, _, Format, List) ->
    io:format(GroupLeader, Format, List).

dump_stack(undefined, _, _) ->
    false;
dump_stack(Dump, Stack, Term) ->
    {Depth, _D} = 
	case Stack of
	    undefined ->
		{0, 0};
	    _ ->
		case length(Stack) of
		    0 ->
			{0, 0};
		    N ->
			{N, length(hd(Stack))}
		end
	end,
     io:format(Dump, "~s~tp.~n", [lists:duplicate(Depth, "  "), parsify(Term)]),
    true.

dump(undefined, _) ->
    false;
dump(Dump, Term) ->
    io:format(Dump, "~tp.~n", [parsify(Term)]),
    true.



%%%----------------------------------
%%% Profiling state machine functions
%%%----------------------------------



trace_handler({trace_ts, Pid, call, _MFA, _TS} = Trace, 
	      _Table, _, Dump) ->
    Stack = get(Pid),
    dump_stack(Dump, Stack, Trace),
    throw({incorrect_trace_data, ?MODULE, ?LINE,
	  [Trace, Stack]});
trace_handler({trace_ts, Pid, call, {_M, _F, Arity} = Func, 
	       {cp, CP}, TS} = Trace,
	      Table, GroupLeader, Dump)
  when is_integer(Arity) ->
    dump_stack(Dump, get(Pid), Trace),
    case Func of
	{erlang, trace, 3} ->
	    info_suspect_call(GroupLeader, Dump, Func, Pid);
	{erlang, trace_pattern, 3} ->
	    info_suspect_call(GroupLeader, Dump, Func, Pid);
	_ ->
	    ok
    end,
    trace_call(Table, Pid, Func, TS, CP),
    TS;
trace_handler({trace_ts, Pid, call, {_M, _F, Args} = MFArgs, 
	       {cp, CP}, TS} = Trace,
	      Table, _, Dump)
  when is_list(Args) ->
    dump_stack(Dump, get(Pid), Trace),
    Func = mfarity(MFArgs),
    trace_call(Table, Pid, Func, TS, CP),
    TS;
%%
%% return_to
trace_handler({trace_ts, Pid, return_to, undefined, TS} = Trace,
	      Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_return_to(Table, Pid, undefined, TS),
    TS;
trace_handler({trace_ts, Pid, return_to, {_M, _F, Arity} = Func, TS} = Trace,
	      Table, _, Dump)
  when is_integer(Arity) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_return_to(Table, Pid, Func, TS),
    TS;
trace_handler({trace_ts, Pid, return_to, {_M, _F, Args} = MFArgs, TS} = Trace,
	      Table, _, Dump)
  when is_list(Args) ->
    dump_stack(Dump, get(Pid), Trace),
    Func = mfarity(MFArgs),
    trace_return_to(Table, Pid, Func, TS),
    TS;
%%
%% spawn, only needed (and reliable) prior to 19.0
trace_handler({trace_ts, Pid, spawn, Child, MFArgs, TS} = Trace,
	      Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_spawn(Table, Child, MFArgs, TS, Pid),
    TS;
%%
%% spawned, added in 19.0
trace_handler({trace_ts, Pid, spawned, Parent, MFArgs, TS} = Trace,
	      Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_spawn(Table, Pid, MFArgs, TS, Parent),
    TS;
%%
%% exit
trace_handler({trace_ts, Pid, exit, _Reason, TS} = Trace,
	      Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_exit(Table, Pid, TS),
    TS;
%%
%% out
trace_handler({trace_ts, Pid, out, 0, TS} = Trace,
	      Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_out(Table, Pid, undefined, TS),
    TS;
trace_handler({trace_ts, Pid, out, {_M, _F, Arity} = Func, TS} = Trace,
	      Table, _, Dump)
  when is_integer(Arity) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_out(Table, Pid, Func, TS),
    TS;
trace_handler({trace_ts, Pid, out, {_M, _F, Args} = MFArgs, TS} = Trace,
	      Table, _, Dump)
  when is_list(Args) ->
    dump_stack(Dump, get(Pid), Trace),
    Func = mfarity(MFArgs),
    trace_out(Table, Pid, Func, TS),
    TS;
%%
%% in
trace_handler({trace_ts, Pid, in, 0, TS} = Trace,
	      Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_in(Table, Pid, undefined, TS),
    TS;
trace_handler({trace_ts, Pid, in, {_M, _F, Arity} = Func, TS} = Trace,
	      Table, _, Dump)
  when is_integer(Arity) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_in(Table, Pid, Func, TS),
    TS;
trace_handler({trace_ts, Pid, in, {_M, _F, Args} = MFArgs, TS} = Trace,
	      Table, _, Dump)
  when is_list(Args) ->
    dump_stack(Dump, get(Pid), Trace),
    Func = mfarity(MFArgs),
    trace_in(Table, Pid, Func, TS),
    TS;
%%
%% gc_start
trace_handler({trace_ts, Pid, gc_minor_start, _Func, TS} = Trace, Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_gc_start(Table, Pid, TS),
    TS;

trace_handler({trace_ts, Pid, gc_major_start, _Func, TS} = Trace, Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_gc_start(Table, Pid, TS),
    TS;
    
trace_handler({trace_ts, Pid, gc_start, _Func, TS} = Trace, Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_gc_start(Table, Pid, TS),
    TS;

%%
%% gc_end
trace_handler({trace_ts, Pid, gc_minor_end, _Func, TS} = Trace, Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_gc_end(Table, Pid, TS),
    TS;

trace_handler({trace_ts, Pid, gc_major_end, _Func, TS} = Trace, Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_gc_end(Table, Pid, TS),
    TS;
    
trace_handler({trace_ts, Pid, gc_end, _Func, TS} = Trace, Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    trace_gc_end(Table, Pid, TS),
    TS;
    
%%
%% link
trace_handler({trace_ts, Pid, link, _OtherPid, TS} = Trace,
	      _Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    TS;
%%
%% unlink
trace_handler({trace_ts, Pid, unlink, _OtherPid, TS} = Trace,
	      _Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    TS;
%%
%% getting_linked
trace_handler({trace_ts, Pid, getting_linked, _OtherPid, TS} = Trace,
	      _Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    TS;
%%
%% getting_unlinked
trace_handler({trace_ts, Pid, getting_unlinked, _OtherPid, TS} = Trace,
	      _Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    TS;
%%
%% register
trace_handler({trace_ts, Pid, register, _Name, TS} = Trace,
	      _Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    TS;
%%
%% unregister
trace_handler({trace_ts, Pid, unregister, _Name, TS} = Trace,
	      _Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    TS;
%%
%% send
trace_handler({trace_ts, Pid, send, _OtherPid, _Msg, TS} = Trace,
	      _Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    TS;
%%
%% send_to_non_existing_process
trace_handler({trace_ts, Pid, send_to_non_existing_process, _OtherPid, _Msg, TS} = Trace,
	      _Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    TS;
%%
%% 'receive'
trace_handler({trace_ts, Pid, 'receive', _Msg, TS} = Trace,
	      _Table, _, Dump) ->
    dump_stack(Dump, get(Pid), Trace),
    TS;
%%
%% Others
trace_handler(Trace, _Table, _, Dump) ->
    dump(Dump, Trace),
    throw({incorrect_trace_data, ?MODULE, ?LINE, [Trace]}).



%% The call stack
%% --------------
%%
%% The call stack can be modeled as a tree, with each level in the tree
%% corresponding to a real (non-tail recursive) stack entry, 
%% and the nodes within a level corresponding to tail recursive
%% calls on that real stack depth.
%%
%% Example:
%% a() ->
%%     b().
%% b() ->
%%     c(),
%%     d().
%% c() -> ok.
%% d() ->
%%     e(),
%%     c().
%% e() ->
%%     f().
%% f() -> ok.
%%
%% During the execution the call tree would be, for each call and return_to:
%%
%% a()    b()    c()    ->b    d()    e()    f()    ->d    c()    ->a
%%
%%     a      a      a      a      a      a      a      a      a      a
%%            |      |      |      |\     |\     |\     |\    /|\
%%            b      b      b      b d    b d    b d    b d  b d c
%%                   |                      |     /|
%%                   c                      e    e f
%%
%% The call tree is in this code represented as a two level list, 
%% which for the biggest tree (5 nodes) in the example above would be:
%%     [[{f, _}, {e, _}], [{d, _}, {b, _}], [{a, _}]]
%% where the undefined fields are timestamps of the calls to the
%% functions, and the function name fields are really 
%% {Module, Function, Arity} tuples.
%%
%% Since tail recursive calls can form an infinite loop, cycles 
%% within a tail recursive level must be collapsed or else the
%% stack (tree) size may grow towards infinity.



trace_call(Table, Pid, Func, TS, CP) ->
    Stack = get_stack(Pid),
    ?dbg(0, "trace_call(~p, ~p, ~p, ~p)~n~p~n", 
	 [Pid, Func, TS, CP, Stack]),
    {Proc,InitCnt} = 
	case ets:lookup(Table, Pid) of
	    [#proc{init_cnt = N} = P] ->
		{P,N};
	    [] ->
		{undefined,0}
	end,
    case Stack of
	[] ->
	    init_log(Table, Proc, Func),
	    OldStack = 
		if CP =:= undefined ->
			Stack;
		   true ->
			[[{CP, TS}]]
		end,
	    put(Pid, trace_call_push(Table, Pid, Func, TS, OldStack));
	[[{Func, FirstInTS}]] when InitCnt=:=2 ->
	    %% First call on this process. Take the timestamp for first
	    %% time the process was scheduled in.
	    init_log(Table, Proc, Func),
	    OldStack = 
		if CP =:= undefined ->
			[];
		   true ->
			[[{CP, FirstInTS}]]
		end,
	    put(Pid, trace_call_push(Table, Pid, Func, FirstInTS, OldStack));
	[[{suspend, _} | _] | _] ->
	    throw({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Pid, Func, TS, CP, Stack]});
	[[{garbage_collect, _} | _] | _] ->
	    throw({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Pid, Func, TS, CP, Stack]});
	[[{CP, _} | _], [{CP, _} | _] | _] ->
	    %% This is a difficult case - current function becomes
	    %% new stack top but is already pushed. It might be that
	    %% this call is actually tail recursive, or maybe not.
	    %% Assume tail recursive to not build the stack infinitely
	    %% and fix the problem at the next call after a return to
	    %% this level.
	    %%
	    %% This can be viewed as collapsing a very short stack
	    %% recursive stack cykle.
	    init_log(Table, Proc, Func),
	    put(Pid, trace_call_shove(Table, Pid, Func, TS, Stack));
	[[{CP, _} | _] | _] ->
	    %% Current function becomes new stack top -> stack push
	    init_log(Table, Proc, Func),
	    put(Pid, trace_call_push(Table, Pid, Func, TS, Stack));
	[_, [{CP, _} | _] | _] ->
	    %% Stack top unchanged -> no push == tail recursive call
	    init_log(Table, Proc, Func),
	    put(Pid, trace_call_shove(Table, Pid, Func, TS, Stack));
	[[{Func0, _} | _], [{Func0, _} | _], [{CP, _} | _] | _] ->
	    %% Artificial case that only should happen when 
	    %% stack recursive short cycle collapsing has been done,
	    %% otherwise CP should not occur so far from the stack front.
	    %%
	    %% It is a tail recursive call but fix the stack first.
	    init_log(Table, Proc, Func),
	    put(Pid, 
		trace_call_shove(Table, Pid, Func, TS,
				 trace_return_to_int(Table, Pid, Func0, TS,
						     Stack)));
	[[{_, TS0} | _] = Level0] ->
	    %% Current function known, but not stack top
	    %% -> assume tail recursive call
	    init_log(Table, Proc, Func),
	    OldStack =
		if CP =:= undefined ->
			Stack;
		   true ->
			[Level0, [{CP, TS0}]]
		end,
	    put(Pid, trace_call_shove(Table, Pid, Func, TS, OldStack));
	[_ | _] ->
	    %% Weird case when the stack is seriously f***ed up.
	    %% CP is not at stack top nor at previous stack top, 
	    %% which is impossible, if we had a correct stack view.
	    OldStack = 
		if CP =:= undefined ->
			%% Assume that CP is unknown because it is
			%% the stack bottom for the process, and that 
			%% the whole call stack is invalid. Waste it.
			trace_return_to_int(Table, Pid, CP, TS, Stack);
		   true ->
			%% Assume that we have collapsed a tail recursive
			%% call stack cykle too many. Introduce CP in
			%% the current tail recursive level so it at least
			%% gets charged for something.
			init_log(Table, Proc, CP),
			trace_call_shove(Table, Pid, CP, TS, Stack)
		end,
	    %% Regard this call as a stack push.
	    init_log(Table, Pid, Func), % will lookup Pid in Table
	    put(Pid, trace_call_push(Table, Pid, Func, TS, OldStack))
    end,
    ok.

%% Normal stack push
trace_call_push(Table, Pid, Func, TS, Stack) ->
    case Stack of
	[] ->
	    ok;
	[_ | _] ->
	    trace_clock(Table, Pid, TS, Stack, #clocks.own)
    end,
    NewStack = [[{Func, TS}] | Stack],
    trace_clock(Table, Pid, 1, NewStack, #clocks.cnt),
    NewStack.

%% Tail recursive stack push
trace_call_shove(Table, Pid, Func, TS, Stack) ->
    trace_clock(Table, Pid, TS, Stack, #clocks.own),
    [[_ | NewLevel0] | NewStack1] = 
	case Stack of
	    [] ->
		[[{Func, TS}]];
	    [Level0 | Stack1] ->
		[trace_call_collapse([{Func, TS} | Level0]) | Stack1]
	end,
    NewStack = [[{Func, TS} | NewLevel0] | NewStack1],
    trace_clock(Table, Pid, 1, NewStack, #clocks.cnt),
    NewStack.

%% Collapse tail recursive call stack cycles to prevent them from
%% growing to infinite length.
-doc false.
trace_call_collapse([]) ->
    [];
trace_call_collapse([_] = Stack) ->
    Stack;
trace_call_collapse([_, _] = Stack) ->
    Stack;
trace_call_collapse([_ | Stack1] = Stack) ->
    trace_call_collapse_1(Stack, Stack1, 1).

%% Find some other instance of the current function in the call stack
%% and try if that instance may be used as stack top instead.
trace_call_collapse_1(Stack, [], _) ->
    Stack;
trace_call_collapse_1([{Func0, _} | _] = Stack, [{Func0, _} | S1] = S, N) ->
    case trace_call_collapse_2(Stack, S, N) of
	true ->
	    S;
	false ->
	    trace_call_collapse_1(Stack, S1, N+1)
    end;
trace_call_collapse_1(Stack, [_ | S1], N) ->
    trace_call_collapse_1(Stack, S1, N+1).

%% Check if all caller/called pairs in the perhaps to be collapsed
%% stack segment (at the front) are present in the rest of the stack, 
%% and also in the same order.
trace_call_collapse_2(_, _, 0) ->
    true;
trace_call_collapse_2([{Func1, _} | [{Func2, _} | _] = Stack2],
	   [{Func1, _} | [{Func2, _} | _] = S2],
	   N) ->
    trace_call_collapse_2(Stack2, S2, N-1);
trace_call_collapse_2([{Func1, _} | _], [{Func1, _} | _], _N) ->
    false;
trace_call_collapse_2(_Stack, [_], _N) ->
    false;
trace_call_collapse_2(Stack, [_ | S], N) ->
    trace_call_collapse_2(Stack, S, N);
trace_call_collapse_2(_Stack, [], _N) ->
    false.



trace_return_to(Table, Pid, Func, TS) ->
    Stack = get_stack(Pid),
    ?dbg(0, "trace_return_to(~p, ~p, ~p)~n~p~n", 
	 [Pid, Func, TS, Stack]),
    case Stack of
	[[{suspend, _} | _] | _] ->
	    throw({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Pid, Func, TS, Stack]});
	[[{garbage_collect, _} | _] | _] ->
	    throw({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Pid, Func, TS, Stack]});
	[_ | _] ->
	    put(Pid, trace_return_to_int(Table, Pid, Func, TS, Stack));
	[] ->
	    put(Pid, trace_return_to_int(Table, Pid, Func, TS, Stack))
    end,
    ok.

trace_return_to_int(Table, Pid, Func, TS, Stack) ->
    %% The old stack must be sent to trace_clock, so
    %% the function we just returned from is charged with
    %% own time.
    trace_clock(Table, Pid, TS, Stack, #clocks.own),
    case trace_return_to_2(Table, Pid, Func, TS, Stack) of
	{undefined, _} ->
	    [[{Func, TS}] | Stack];
	{[[{Func, _} | Level0] | Stack1], _} ->
	    [[{Func, TS} | Level0] | Stack1];
	{NewStack, _} ->
	    NewStack
    end.

%% A list of charged functions is passed around to assure that 
%% any function is charged with ACC time only once - the first time
%% it is encountered. The function trace_return_to_1 is called only
%% for the front of a tail recursive level, and if the front 
%% does not match the returned-to function, trace_return_to_2
%% is called for all functions within the tail recursive level.
%%
%% Charging is done in reverse order, that is, from stack rear to front.

%% Search the call stack until the returned-to function is found at
%% a tail recursive level's front, and charge it with ACC time.
trace_return_to_1(_, _, undefined, _, []) ->
    {[], []};
trace_return_to_1(_, _, _, _, []) ->
    {undefined, []};
trace_return_to_1(Table, Pid, Func, TS, 
		  [[{Func, _} | Level0] | Stack1] = Stack) ->
    %% Match at front of tail recursive level
    Charged = trace_return_to_3([Level0 | Stack1], []),
    case lists:member(Func, Charged) of
	false ->
	    trace_clock(Table, Pid, TS, Stack, #clocks.acc),
	    {Stack, [Func | Charged]};
	true ->
	    {Stack, Charged}
    end;
trace_return_to_1(Table, Pid, Func, TS, Stack) ->
    trace_return_to_2(Table, Pid, Func, TS, Stack).

%% Charge all functions within one tail recursive level, 
%% from rear to front, with ACC time.
trace_return_to_2(Table, Pid, Func, TS, [] = Stack) ->
    trace_return_to_1(Table, Pid, Func, TS, Stack);
trace_return_to_2(Table, Pid, Func, TS, [[] | Stack1]) ->
    trace_return_to_1(Table, Pid, Func, TS, Stack1);
trace_return_to_2(Table, Pid, Func, TS,
		  [[{Func0, _} | Level1] | Stack1] = Stack) ->
    case trace_return_to_2(Table, Pid, Func, TS, [Level1 | Stack1]) of
	{undefined, _} = R ->
	    R;
	{NewStack, Charged} = R ->
	    case lists:member(Func0, Charged) of
		false ->
		    trace_clock(Table, Pid, TS, Stack, #clocks.acc),
		    {NewStack, [Func0 | Charged]};
		true ->
		    R
	    end
    end.

%% Return a flat list of all function names in the given stack
trace_return_to_3([], R) ->
    R;
trace_return_to_3([[] | Stack1], R) ->
    trace_return_to_3(Stack1, R);
trace_return_to_3([[{Func0, _} | Level0] | Stack1], R) ->
    trace_return_to_3([Level0 | Stack1], [Func0 | R]).



trace_spawn(Table, Pid, MFArgs, TS, Parent) ->
    Stack = get(Pid),
    ?dbg(0, "trace_spawn(~p, ~p, ~p, ~p)~n~p~n", 
	 [Pid, MFArgs, TS, Parent, Stack]),
    case Stack of
	undefined ->
	    {M,F,Args} = MFArgs,
	    OldStack = [[{{M,F,length(Args)},TS}]],
	    put(Pid, trace_call_push(Table, Pid, suspend, TS, OldStack)),
	    ets:insert(Table, #proc{id = Pid, parent = Parent,
				    spawned_as = MFArgs});
	_ ->
            %% In 19.0 we get both a spawn and spawned event,
            %% however we do not know the order so we just ignore
            %% the second event that comes
	    ok
    end.



trace_exit(Table, Pid, TS) ->
    Stack = erase(Pid),
    ?dbg(0, "trace_exit(~p, ~p)~n~p~n", [Pid, TS, Stack]),
    case Stack of
	undefined ->
	    ok;
	[] ->
	    ok;
	[_ | _] = Stack ->
	    _ = trace_return_to_int(Table, Pid, undefined, TS, Stack),
	    ok
    end,
    ok.



trace_out(Table, Pid, Func, TS) ->    
    Stack = get_stack(Pid),
    ?dbg(0, "trace_out(~p, ~p, ~p)~n~p~n", [Pid, Func, TS, Stack]),
    case Stack of
	[] ->
	    put(Pid, trace_call_push(Table, Pid, suspend, TS, 
				     case Func of
					 undefined -> [];
					 _ ->
					     [[{Func,TS}]]
				     end));
	[[{suspend,_}] | _] ->
	    %% No stats update for a suspend on suspend
	    put(Pid, [[{suspend,TS}] | Stack]);
	[_ | _] ->
	    put(Pid, trace_call_push(Table, Pid, suspend, TS, Stack))
    end.



trace_in(Table, Pid, Func, TS) ->	    
    Stack = get(Pid),
    ?dbg(0, "trace_in(~p, ~p, ~p)~n~p~n", [Pid, Func, TS, Stack]),
    case Stack of
	undefined ->
	    %% First activity on a process which existed at the time
	    %% the fprof trace was started.
	    put(Pid, [[{Func,TS}]]);
	[] ->
	    put(Pid, [[{Func,TS}]]);
	[[{suspend, _}]] ->
	    put(Pid, trace_return_to_int(Table, Pid, undefined, TS, Stack));
	[[{suspend,_}] | [[{suspend,_}] | _]=NewStack] ->
	    %% No stats update for a suspend on suspend
	    put(Pid, NewStack);
	[[{suspend, _}] | [[{Func1, _} | _] | _]] ->
	    %% This is a new process (suspend and Func1 was inserted
	    %% by trace_spawn) or any process that has just been
	    %% scheduled out and now back in.
	    put(Pid, trace_return_to_int(Table, Pid, Func1, TS, Stack));
	_ ->
	    throw({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Pid, Func, TS, Stack]})
    end.



trace_gc_start(Table, Pid, TS) ->    
    Stack = get_stack(Pid),
    ?dbg(0, "trace_gc_start(~p, ~p)~n~p~n", [Pid, TS, Stack]),
    put(Pid, trace_call_push(Table, Pid, garbage_collect, TS, Stack)).



trace_gc_end(Table, Pid, TS) ->	    
    Stack = get(Pid),
    ?dbg(0, "trace_gc_end(~p, ~p)~n~p~n", [Pid, TS, Stack]),
    case Stack of
	undefined ->
	    put(Pid, []);
	[] ->
	    ok;
	[[{garbage_collect, _}]] ->
	    put(Pid, trace_return_to_int(Table, Pid, undefined, TS, Stack));
	[[{garbage_collect, _}], [{Func1, _} | _] | _] ->
	    put(Pid, trace_return_to_int(Table, Pid, Func1, TS, Stack));
	_ ->
	    throw({inconsistent_trace_data, ?MODULE, ?LINE,
		  [Pid, TS, Stack]})
    end.



%%%-----------------------------------------
%%% Statistics calculating support functions
%%%-----------------------------------------



get_stack(Id) ->
    case get(Id) of
	undefined ->
	    [];
	Stack ->
	    Stack
    end.



mfarity({M, F, Args}) when is_list(Args) ->
    {M, F, length(Args)};
mfarity(MFA) ->
    MFA.



init_log(_Table, _Proc, suspend) ->
    ok;
init_log(_Table, _Proc, void) ->
    ok;
init_log(_Table, undefined, _Entry) ->
    ok;
init_log(_Table, #proc{init_cnt = 0}, _Entry) ->
    ok;
init_log(Table, #proc{init_cnt = N, init_log = L} = Proc, Entry) ->
    ets:insert(Table, Proc#proc{init_cnt = N-1, init_log = [Entry | L]});
init_log(Table, Id, Entry) ->
    Proc = 
	case ets:lookup(Table, Id) of
	    [P] -> P;
	    [] -> undefined
	end,
    init_log(Table,Proc,Entry).


trace_clock(_Table, _Pid, _T, 
	    [[{suspend, _}], [{suspend, _}] | _]=_Stack, _Clock) ->
    ?dbg(9, "trace_clock(Table, ~w, ~w, ~w, ~w)~n",
	 [_Pid, _T, _Stack, _Clock]),
    ok;
trace_clock(Table, Pid, T, 
	    [[{garbage_collect, TS0}], [{suspend, _}]], Clock) ->
    trace_clock_1(Table, Pid, T, TS0, undefined, garbage_collect, Clock);
trace_clock(Table, Pid, T, 
	    [[{garbage_collect, TS0}], [{suspend, _}], [{Func2, _} | _] | _],
	    Clock) ->
    trace_clock_1(Table, Pid, T, TS0, Func2, garbage_collect, Clock);
trace_clock(Table, Pid, T, [[{Func0, TS0}, {Func1, _} | _] | _], Clock) ->
    trace_clock_1(Table, Pid, T, TS0, Func1, Func0, Clock);
trace_clock(Table, Pid, T, [[{Func0, TS0}], [{Func1, _} | _] | _], Clock) ->
    trace_clock_1(Table, Pid, T, TS0, Func1, Func0, Clock);
trace_clock(Table, Pid, T, [[{Func0, TS0}]], Clock) ->
    trace_clock_1(Table, Pid, T, TS0, undefined, Func0, Clock);
trace_clock(_, _, _, [], _) ->
    ok.

trace_clock_1(Table, Pid, _, _, Caller, suspend, #clocks.own) ->
    clock_add(Table, {Pid, Caller, suspend}, #clocks.own, 0);
trace_clock_1(Table, Pid, T, TS, Caller, Func, Clock) ->
    clock_add(Table, {Pid, Caller, Func}, Clock,
	      if is_integer(T) ->
		      T;
		 true ->
		      ts_sub(T, TS)
	      end).

clock_add(Table, Id, Clock, T) ->
    ?dbg(1, "clock_add(Table, ~w, ~w, ~w)~n", [Id, Clock, T]),
    try _ = ets:update_counter(Table, Id, {Clock, T}), ok
    catch
	error:badarg ->
	    ets:insert(Table, #clocks{id = Id}),
	    X = ets:update_counter(Table, Id, {Clock, T}),
	    if X >= 0 -> ok;
	       true -> ?dbg(0, "Negative counter value ~p ~p ~p ~p~n",
			  [X, Id, Clock, T])
	    end,
	    ok
    end.

clocks_add(Table, #clocks{id = Id} = Clocks) ->
    ?dbg(1, "clocks_add(Table, ~w)~n", [Clocks]),
    case ets:lookup(Table, Id) of
	[Clocks0] ->
	    ets:insert(Table, clocks_sum(Clocks, Clocks0, Id));
	[] ->
	    ets:insert(Table, Clocks)
    end.



clocks_sum(#clocks{id = _Id1, 
		   cnt = Cnt1, 
		   own = Own1, 
		   acc = Acc1}, 
	   #clocks{id = _Id2, 
		   cnt = Cnt2, 
		   own = Own2, 
		   acc = Acc2}, 
	   Id) ->
    #clocks{id = Id,
	    cnt = Cnt1 + Cnt2,
	    own = Own1 + Own2,
	    acc = Acc1 + Acc2}.



ts_sub({A, B, C} = _T, {A0, B0, C0} = _T0) ->
    X = ((((A-A0)*1000000) + (B-B0))*1000000) + C - C0,
    if X >= 0 -> ok;
       true -> ?dbg(9, "Negative counter value ~p ~p ~p~n",
		    [X, _T, _T0])
    end,
    X;
ts_sub(_, _) ->
    undefined.



%%%--------------------------------
%%% Profile data analysis functions
%%%--------------------------------



do_analyse(Table, Analyse) ->
    ?dbg(5, "do_analyse_1(~p, ~p)~n", [Table, Analyse]),
    Result = 
	try do_analyse_1(Table, Analyse)
	catch
	    Error -> Error
	end,
    ?dbg(5, "do_analyse_1(_, _) ->~p~n", [Result]),
    Result.

-dialyzer({no_improper_lists, do_analyse_1/2}).

do_analyse_1(Table, 
	   #analyse{group_leader = GroupLeader,
		    dest = Io,
		    cols = Cols0,
		    callers = PrintCallers,
		    sort = Sort,
		    totals = PrintTotals,
		    details = PrintDetails} = _Analyse) ->
    Waste = 11,
    MinCols = Waste + 12, %% We need Width >= 1
    Cols = if Cols0 < MinCols -> MinCols; true -> Cols0 end,
    Width = (Cols-Waste) div 12,
    FnameWidth = Cols - Waste - 5*Width,
    Dest = {Io, [FnameWidth, Width, 2*Width, 2*Width]},
    SortElement = case Sort of
		      own ->
			  #clocks.own;
		      acc ->
			  #clocks.acc
		  end,
    %%
    %% Clean out the process dictionary before the next step
    %%
    _Erase = erase(),
    ?dbg(2, "erase() -> ~p~n", [_Erase]),
    %%
    %% Process the collected data and spread it to 3 places:
    %% * Per {process, caller, func}. Stored in the process dictionary.
    %% * Sum per process. Stored in an ets table.
    %% * Extra info per process. Stored in another ets table.
    %%
    io:format(GroupLeader, "Processing data...~n", []),
    PidTable = ets:new(?MODULE, [set, private, {keypos, #clocks.id}]),
    ProcTable = ets:new(?MODULE, [set, private, {keypos, #proc.id}]),
    ets_select_foreach(
      Table, [{'_', [], ['$_']}], 100,
      fun (#clocks{id = {Pid, Caller, Func}} = Clocks) ->
	      case PrintDetails of
		  true ->
		      funcstat_pd(Pid, Caller, Func, Clocks),
		      clocks_add(PidTable, Clocks#clocks{id = Pid});
		  false ->
		      ok
	      end,
	      clocks_add(PidTable, Clocks#clocks{id = totals}),
	      case PrintTotals of
		  true ->
		      funcstat_pd(totals, Caller, Func, Clocks);
		  false ->
		      ok
	      end;
	  (#proc{} = Proc) ->
	      ets:insert(ProcTable, Proc);
	  (#misc{} = Misc) ->
	      ets:insert(ProcTable, Misc)
      end),
    ?dbg(3, "get() -> ~p~n", [get()]),
    {FirstTS, LastTS, _TraceCnt} = 
	case {ets:lookup(ProcTable, first_ts), 
	      ets:lookup(ProcTable, last_ts_n)} of
	    {[#misc{data = FTS}], [#misc{data = {LTS, TC}}]} 
	    when FTS =/= undefined, LTS =/= undefined ->
		{FTS, LTS, TC};
	    _ ->
		throw({error,empty_trace})
	end,
    Totals0 = 
	case ets:lookup(PidTable, totals) of
	    [T0] ->
		ets:delete(PidTable, totals),
		T0;
	    _ ->
		throw({error,empty_trace})
	end,
    Totals = Totals0#clocks{acc = ts_sub(LastTS, FirstTS)},
    ?dbg(3, "Totals0 =  ~p~n", [Totals0]),
    ?dbg(3, "PidTable =  ~p~n", [ets:tab2list(PidTable)]),
    ?dbg(3, "ProcTable =  ~p~n", [ets:tab2list(ProcTable)]),
    ?dbg(4, "Totals = ~p~n", [Totals]),
    %%
    %% Reorganize the process dictionary by Pid.
    %%
    lists:foreach(
      fun ({{Pid, _Func}, Funcstat}) ->
	      put(Pid, [Funcstat | case get(Pid) of
				       undefined -> [];
				       Other -> Other
				   end])
      end,
      erase()),
    ?dbg(4, "get() -> ~p~n", [get()]),
    %%
    %% Sort the processes
    %%
    PidSorted = 
	postsort_r(
	  lists:sort(
	    ets:select(PidTable, 
		       [{'_', [], [[{element, #clocks.own, '$_'} | '$_']]}]))),
    ?dbg(4, "PidSorted = ~p~n", [PidSorted]),
    %%
    %% Print the functions per process
    %%
    io:format(GroupLeader, "Creating output...~n", []),
    println(Dest, "%% ", [], "Analysis results:", ""),
    println(Dest, "{  ", analysis_options, ",", ""),
    println(Dest, " [{", {callers, PrintCallers}, "},", ""),
    println(Dest, "  {", {sort, Sort}, "},", ""),
    println(Dest, "  {", {totals, PrintTotals}, "},", ""),
    println(Dest, "  {", {details, PrintDetails}, "}]}.", ""),
    println(Dest),
    lists:foreach(
      fun ({#clocks{} = Clocks, ProcOrPid, FuncstatList}) ->
	      println(Dest, "%  ", head, "", ""),
	      case ProcOrPid of
		  #proc{} ->
		      println(Dest, "[{ ", Clocks, "},", "%%"),
		      print_proc(Dest, ProcOrPid);
		  totals ->
		      println(Dest, "[{ ", Clocks, "}].", "%%%");
		  _ when is_pid(ProcOrPid) ->
		      println(Dest, "[{ ", Clocks, "}].", "%%")
	      end,
	      println(Dest),
	      lists:foreach(
		fun (#funcstat{callers_sum = CallersSum, 
%			       called_sum = CalledSum, 
			       callers = Callers, 
			       called = Called}) ->
			case {PrintCallers, Callers} of
%			    {true, []} ->
%				ok;
			    {true, _} ->
				print_callers(Dest, Callers),
				println(Dest, " { ", CallersSum, "},", "%"),
				print_called(Dest, Called),
				println(Dest);
			    {false, _} ->
				println(Dest, "{  ", CallersSum, "}.", "")
			end,
			ok
		end,
		%% Sort the functions within the process, 
		%% and the callers and called within the function.
		funcstat_sort_r(FuncstatList, SortElement)),
	      println(Dest)
      end,
      %% Look up the processes in sorted order
      lists:map(
	fun (#clocks{id = Pid} = Clocks) -> 
		Proc = case ets:lookup(ProcTable, Pid) of
			   [] -> Pid;
			   [ProcX] -> ProcX
		       end,
		FuncstatList = 
		    case get(Pid) of
			undefined ->
			    [];
			FL ->
			    FL
		    end,
		{Clocks, Proc, FuncstatList}
	end,
	case PrintDetails of
	    true ->
		[Totals | PidSorted];
	    false ->
		[Totals]
	end)),
    %%
    %% Cleanup
    %%
    ets:delete(PidTable),
    ets:delete(ProcTable),
    io:format(GroupLeader, "Done!~n", []),
    ok.



%%----------------------------
%% Analysis printout functions
%%----------------------------



print_proc({undefined, _}, _) ->
    ok;
print_proc(Dest, 
	   #proc{id = _Pid, 
		 parent = Parent, 
		 spawned_as = SpawnedAs,
		 init_log = InitLog}) ->
    case {Parent, SpawnedAs, InitLog} of
	{undefined, undefined, []} ->
	    println(Dest, "   ", [], "].", "");
	{_, undefined, []} ->
	    println(Dest, " { ", {spawned_by, parsify(Parent)}, "}].", "");
	_ ->
	    println(Dest, " { ", {spawned_by, parsify(Parent)}, "},", ""),
	    case {SpawnedAs, InitLog} of
		{_, []} ->
		    println(Dest, " { ",
			    {spawned_as, SpawnedAs},
			    "}].", "");
		{undefined, _} ->
		    println(Dest, " { ", 
			    {initial_calls, lists:reverse(InitLog)},
			    "}].", "");
		_ ->
		    println(Dest, " { ",
			    {spawned_as, SpawnedAs},
			    "},", ""),
		    println(Dest, " { ",
			    {initial_calls, lists:reverse(InitLog)},
			    "}].", "")
	    end
    end.



-doc false.
print_callers(Dest, []) ->
    println(Dest, "{[", [], "],", "");
print_callers(Dest, [Clocks]) ->
    println(Dest, "{[{", Clocks, "}],", "");
print_callers(Dest, [Clocks | Tail]) ->
    println(Dest, "{[{", Clocks, "},", ""),
    print_callers_1(Dest, Tail).

print_callers_1(Dest, [Clocks]) ->
    println(Dest, "  {", Clocks, "}],", "");
print_callers_1(Dest, [Clocks | Tail]) ->
    println(Dest, "  {", Clocks, "},", ""),
    print_callers_1(Dest, Tail).



-doc false.
print_func(Dest, Clocks) ->
    println(Dest, " { ", Clocks, "},", "%").



-doc false.
print_called(Dest, []) ->
    println(Dest, " [", [], "]}.", "");
print_called(Dest, [Clocks]) ->
    println(Dest, " [{", Clocks, "}]}.", "");
print_called(Dest, [Clocks | Tail]) ->
    println(Dest, " [{", Clocks, "},", ""),
    print_called_1(Dest, Tail).

print_called_1(Dest, [Clocks]) ->
    println(Dest, "  {", Clocks, "}]}.", "");
print_called_1(Dest, [Clocks | Tail]) ->
    println(Dest, "  {", Clocks, "},", ""),
    print_called_1(Dest, Tail).



println({undefined, _}) ->
    ok;
println({Io, _}) ->
    io:nl(Io).

-doc false.
println({undefined, _}, _Head,
	_, 
	_Tail, _Comment) ->
    ok;
println({Io, [W1, W2, W3, W4]}, Head,
	#clocks{id = Pid, cnt = Cnt, acc = _, own = Own},
	Tail, Comment) when is_pid(Pid) ->
    io:put_chars(Io,
		 [pad(Head, $ , 3),
		  flat_format(parsify(Pid), $,, W1),
		  flat_format(Cnt, $,, W2, right),
		  flat_format(undefined, $,, W3, right),
		  flat_format(Own*0.001, [], W4-1, right),
		  pad(Tail, $ , 4),
		  pad($ , Comment, 4),
		  io_lib:nl()]);
println({Io, [W1, W2, W3, W4]}, Head,
	#clocks{id = {_M, _F, _A} = Func, cnt = Cnt, acc = Acc, own = Own},
	Tail, Comment) ->
    io:put_chars(Io,
		 [pad(Head, $ , 3),
		  flat_format(Func, $,, W1),
		  flat_format(Cnt, $,, W2, right),
		  flat_format(Acc*0.001, $,, W3, right),
		  flat_format(Own*0.001, [], W4-1, right),
		  pad(Tail, $ , 4),
		  pad($ , Comment, 4),
		  io_lib:nl()]);
println({Io, [W1, W2, W3, W4]}, Head,
	#clocks{id = Id, cnt = Cnt, acc = Acc, own = Own},
	Tail, Comment) ->
    io:put_chars(Io,
		 [pad(Head, $ , 3),
		  flat_format(parsify(Id), $,, W1),
		  flat_format(Cnt, $,, W2, right),
		  flat_format(Acc*0.001, $,, W3, right),
		  flat_format(Own*0.001, [], W4-1, right),
		  pad(Tail, $ , 4),
		  pad($ , Comment, 4),
		  io_lib:nl()]);
println({Io, [W1, W2, W3, W4]}, Head,
	head,
	Tail, Comment) ->
    io:put_chars(Io,
		 [pad(Head, $ , 3),
		  pad(" ", $ , W1),
		  pad($ , " CNT ", W2),
		  pad($ , " ACC ", W3),
		  pad($ , " OWN", W4-1),
		  pad(Tail, $ , 4),
		  pad($ , Comment, 4),
		  io_lib:nl()]);
println({Io, _}, Head,
	[],
	Tail, Comment) ->
    io:format(Io, "~s~ts~ts~n",
	      [pad(Head, $ , 3), Tail, Comment]);
println({Io, _}, Head,
	{Tag, Term},
	Tail, Comment) ->
    io:format(Io, "~s~tp, ~tp~ts~ts~n",
	      [pad(Head, $ , 3), parsify(Tag), parsify(Term), Tail, Comment]);
println({Io, _}, Head,
	Term,
	Tail, Comment) ->
    io:format(Io, "~s~tp~ts~ts~n",
	      [pad(Head, $ , 3), parsify(Term), Tail, Comment]).



%%%--------------------------
%%% Sorting support functions
%%%--------------------------


%% Add a Clocks record to the callers and called funcstat records
%% in the process dictionary.
%% 
funcstat_pd(Pid, Func1, Func0, Clocks) ->
    put({Pid, Func0},
	case get({Pid, Func0}) of
	    undefined ->
		#funcstat{callers_sum = Clocks#clocks{id = Func0}, 
			  called_sum = #clocks{id = Func0},
			  callers = [Clocks#clocks{id = Func1}]};
	    #funcstat{callers_sum = CallersSum,
		      callers = Callers} = FuncstatCallers ->
		FuncstatCallers#funcstat{
                  callers_sum = clocks_sum(CallersSum, Clocks, Func0),
                  callers = insert_call(Clocks, Func1, Callers)}
        end),
    put({Pid, Func1},
        case get({Pid, Func1}) of
            undefined ->
                #funcstat{callers_sum = #clocks{id = Func1},
                          called_sum = Clocks#clocks{id = Func1},
                          called = [Clocks#clocks{id = Func0}]};
            #funcstat{called_sum = CalledSum,
                      called = Called} = FuncstatCalled ->
                FuncstatCalled#funcstat{
                  called_sum = clocks_sum(CalledSum, Clocks, Func1),
                  called = insert_call(Clocks, Func0, Called)}
        end).

insert_call(Clocks, Func, ClocksList) ->
    insert_call(Clocks, Func, ClocksList, []).

insert_call(Clocks, Func, [#clocks{id = Func} = C | T], Acc) ->
    [clocks_sum(C, Clocks, Func) | T ++ Acc];
insert_call(Clocks, Func, [H | T], Acc) ->
    insert_call(Clocks, Func, T, [H | Acc]);
insert_call(Clocks, Func, [], Acc) ->
    [Clocks#clocks{id = Func} | Acc].



%% Sort a list of funcstat records,
%% and sort the callers and called lists within the funcstat record.
funcstat_sort_r(FuncstatList, Element) ->
    funcstat_sort_r_1(FuncstatList, Element, []).

-dialyzer({no_improper_lists, funcstat_sort_r_1/3}).

funcstat_sort_r_1([], _, R) ->
    postsort_r(lists:sort(R));
funcstat_sort_r_1([#funcstat{callers_sum = #clocks{} = Clocks,
			     callers = Callers,
			     called = Called} = Funcstat
		   | L], 
		  Element,
		  R) ->
    funcstat_sort_r_1(L, 
		      Element, 
		      [[element(Element, Clocks)
			|Funcstat#funcstat{
			   callers = clocks_sort_r(Callers, Element),
			   called = clocks_sort_r(Called, Element)}]
		       | R]).



%% Sort a list of clocks records.
clocks_sort_r(L, E) ->
    clocks_sort_r_1(L, E, []).

-dialyzer({no_improper_lists, clocks_sort_r_1/3}).

clocks_sort_r_1([], _, R) ->
    postsort_r(lists:sort(R));
clocks_sort_r_1([#clocks{} = C | L], E, R) ->
    clocks_sort_r_1(L, E, [[element(E, C)|C] | R]).


%% Take a list of terms with sort headers and strip the headers.
postsort_r(L) ->
    postsort_r(L, []).

postsort_r([], R) ->
    R;
postsort_r([[_|C] | L], R) ->
    postsort_r(L, [C | R]).



%%%----------------------------------------------------------------------
%%% Fairly generic support functions
%%%

%% Standard format and flatten.
flat_format(F, Trailer) when is_float(F) ->
    lists:flatten([io_lib:format("~.3f", [F]), Trailer]);
flat_format(W, Trailer) ->
    lists:flatten([io_lib:format("~tp", [W]), Trailer]).

%% Format, flatten, and pad.
flat_format(Term, Trailer, Width) ->
    flat_format(Term, Trailer, Width, left).

flat_format(Term, Trailer, Width, left) ->
    flat_format(Term, Trailer, Width, {left, $ });
flat_format(Term, Trailer, Width, {left, Filler}) ->
    pad(flat_format(Term, Trailer), Filler, Width);
flat_format(Term, Trailer, Width, right) ->
    flat_format(Term, Trailer, Width, {right, $ });
flat_format(Term, Trailer, Width, {right, Filler}) ->
    pad(Filler, flat_format(Term, Trailer), Width).



%% Left pad a string using a given char.
pad(Char, L, Size) when is_integer(Char), is_list(L), is_integer(Size) ->
    List = lists:flatten(L),
    Length = length(List),
    if Length >= Size ->
	    List;
       true ->
	    lists:append(lists:duplicate(Size - Length, Char), List)
    end;
%% Right pad a string using a given char.
pad(L, Char, Size) when is_list(L), is_integer(Char), is_integer(Size) ->
    List = lists:flatten(L),
    Length = length(List),
    if Length >= Size ->
	    List;
       true ->
	    lists:append(List, lists:duplicate(Size - Length, Char))
    end.



ets_select_foreach(Table, MatchSpec, Limit, Fun) ->
    ets:safe_fixtable(Table, true),
    ets_select_foreach_1(ets:select(Table, MatchSpec, Limit), Fun).

ets_select_foreach_1('$end_of_table', _) ->
    ok;
ets_select_foreach_1({Matches, Continuation}, Fun) ->
    ?dbg(2, "Matches = ~p~n", [Matches]),
    lists:foreach(Fun, Matches),
    ets_select_foreach_1(ets:select(Continuation), Fun).



%% Converts the parts of a deep term that are not parseable when printed
%% with io:format() into their string representation.
-doc false.
parsify([]) ->
    [];
parsify([Hd | Tl]) ->
    [parsify(Hd) | parsify(Tl)];
parsify({A, B}) ->
    {parsify(A), parsify(B)};
parsify({A, B, C}) ->
    {parsify(A), parsify(B), parsify(C)};
parsify(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(parsify(tuple_to_list(Tuple)));
parsify(Map) when is_map(Map) ->
    maps:from_list(parsify(maps:to_list(Map)));
parsify(Pid) when is_pid(Pid) ->
    erlang:pid_to_list(Pid);
parsify(Port) when is_port(Port) ->
    erlang:port_to_list(Port);
parsify(Ref) when is_reference(Ref) -> 
    erlang:ref_to_list(Ref);
parsify(Fun) when is_function(Fun) ->
    erlang:fun_to_list(Fun);
parsify(Term) ->
    Term.
