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
# Profiling

## Do Not Guess About Performance - Profile

Even experienced software developers often guess wrong about where the
performance bottlenecks are in their programs. Therefore, profile your program
to see where the performance bottlenecks are and concentrate on optimizing them.

Erlang/OTP contains several tools to help finding bottlenecks:

- `m:tprof` is a tracing profiler that can measure call count, call time, or
  heap allocations per function call.
- `m:fprof` provides the most detailed information about where the program time
  is spent, but it significantly slows down the program it profiles.
- `m:dbg` is the generic erlang tracing frontend. By using the `timestamp` or
  `cpu_timestamp` options it can be used to time how long function calls in a
  live system take.
- `m:lcnt` is used to find contention points in the Erlang Run-Time System's
  internal locking mechanisms. It is useful when looking for bottlenecks in
  interaction between process, port, ets tables and other entities that can be
  run in parallel.

The tools are further described in [Tools](profiling.md#profiling_tools).

There are also several open source tools outside of Erlang/OTP that can be used
to help profiling. Some of them are:

- [erlgrind](https://github.com/isacssouza/erlgrind) can be used to visualize
  fprof data in kcachegrind.
- [eflame](https://github.com/proger/eflame) is an alternative to fprof that
  displays the profiling output as a flamegraph.
- [recon](https://ferd.github.io/recon/index.html) is a collection of Erlang
  profiling and debugging tools. This tool comes with an accompanying E-book
  called [Erlang in Anger](https://www.erlang-in-anger.com/).
- [perf](https://perf.wiki.kernel.org/index.php/Main_Page) is a sampling
  profiler for Linux that provides functionality similar to `fprof` but with
  much lower overhead. Profiling Erlang code is possible when the emulator has
  been started with the `+JPperf true` emulator flag, and is only available when
  the JIT is enabled.

  For more details about how to run `perf` see the
  [perf support](`e:erts:beamasm.md#linux-perf-support`) section in the BeamAsm
  internal documentation.

## Memory profiling

```text
eheap_alloc: Cannot allocate 1234567890 bytes of memory (of type "heap").
```

The above slogan is one of the more common reasons for Erlang to terminate. For
unknown reasons the Erlang Run-Time System failed to allocate memory to use.
When this happens a crash dump is generated that contains information about the
state of the system as it ran out of memory. Use the
[`crashdump_viewer`](`e:observer:cdv_cmd.md`) to get a view of the memory being
used. Look for processes with large heaps or many messages, large ets tables,
etc.

When looking at memory usage in a running system the most basic function to get
information from is [`erlang:memory()`](`erlang:memory/0`). It returns the
current memory usage of the system. `m:instrument` can be used to get a more
detailed breakdown of where memory is used.

Processes, ports and ets tables can then be inspected using their respective
info functions, i.e. [`erlang:process_info/2 `](`m:erlang#process_info_memory`),
[`erlang:port_info/2 `](`m:erlang#port_info_memory`)and `ets:info/1`.

Sometimes the system can enter a state where the reported memory from
`erlang:memory(total)` is very different from the memory reported by the OS.
This can be because of internal fragmentation within the Erlang Run-Time System.
Data about how memory is allocated can be retrieved using
[`erlang:system_info(allocator)`](`m:erlang#system_info_allocator`). The data
you get from that function is very raw and not very pleasant to read.
[recon_alloc](http://ferd.github.io/recon/recon_alloc.html) can be used to
extract useful information from system_info statistics counters.

## Large Systems

For a large system, it can be interesting to run profiling on a simulated and
limited scenario to start with. But bottlenecks have a tendency to appear or
cause problems only when many things are going on at the same time, and when
many nodes are involved. Therefore, it is also desirable to run profiling in a
system test plant on a real target system.

For a large system, you do not want to run the profiling tools on the whole
system. Instead you want to concentrate on central processes and modules, which
account for a big part of the execution.

There are also some tools that can be used to get a view of the whole system
with more or less overhead.

- `m:observer` is a GUI tool that can connect to remote nodes and display a
  variety of information about the running system.
- `m:etop` is a command line tool that can connect to remote nodes and display
  information similar to what the UNIX tool top shows.
- `m:msacc` allows the user to get a view of what the Erlang Run-Time system is
  spending its time doing. Has a very low overhead, which makes it useful to run
  in heavily loaded systems to get some idea of where to start doing more
  granular profiling.

## What to Look For

When analyzing the result file from the profiling activity, look for functions
that are called many times and have a long "own" execution time (time excluding
calls to other functions). Functions that are called a lot of times can also be
interesting, as even small things can add up to quite a bit if repeated often.
Also ask yourself what you can do to reduce this time. The following are
appropriate types of questions to ask yourself:

- Is it possible to reduce the number of times the function is called?
- Can any test be run less often if the order of tests is changed?
- Can any redundant tests be removed?
- Does any calculated expression give the same result each time?
- Are there other ways to do this that are equivalent and more efficient?
- Can another internal data representation be used to make things more
  efficient?

These questions are not always trivial to answer. Some benchmarks might be
needed to back up your theory and to avoid making things slower if your theory
is wrong. For details, see [Benchmarking](profiling.md#benchmark).

## Tools

[](){: #profiling_tools }

### fprof

`fprof` measures the execution time for each function, both own time, that is,
how much time a function has used for its own execution, and accumulated time,
that is, including called functions. The values are displayed per process. You
also get to know how many times each function has been called.

`fprof` is based on trace to file to minimize runtime performance impact. Using
`fprof` is just a matter of calling a few library functions, see the `m:fprof`
manual page in Tools.

### eprof

`eprof` is based on the Erlang `trace_info` BIFs. `eprof` shows how much time
has been used by each process, and in which function calls this time has been
spent. Time is shown as a percentage of total time and absolute time. For more
information, see the `m:eprof` manual page in Tools.

### cprof

`cprof` is something in between `fprof` and `cover` regarding features. It
counts how many times each function is called when the program is run, on a per
module basis. `cprof` has a low performance degradation effect (compared with
`fprof`) and does not need to recompile any modules to profile (compared with
`cover`). For more information, see the `m:cprof` manual page in Tools.

### Tool Summary

| _Tool_  | _Results_                           | _Size of Result_ | _Effects on Program Execution Time_ | _Records Number of Calls_ | _Records Execution Time_ | _Records Called by_ | _Records Garbage Collection_ |
| ------- | ----------------------------------- | ---------------- | ----------------------------------- | ------------------------- | ------------------------ | ------------------- | ---------------------------- |
| `fprof` | Per process to screen/file          | Large            | Significant slowdown                | Yes                       | Total and own            | Yes                 | Yes                          |
| `eprof` | Per process/function to screen/file | Medium           | Small slowdown                      | Yes                       | Only total               | No                  | No                           |
| `cprof` | Per module to caller                | Small            | Small slowdown                      | Yes                       | No                       | No                  | No                           |

_Table: Tool Summary_

### dbg

`dbg` is a generic Erlang trace tool. By using the `timestamp` or
`cpu_timestamp` options it can be used as a precision instrument to profile how
long time a function call takes for a specific process. This can be very useful
when trying to understand where time is spent in a heavily loaded system as it
is possible to limit the scope of what is profiled to be very small. For more
information, see the `m:dbg` manual page in Runtime Tools.

### lcnt

`lcnt` is used to profile interactions in between entities that run in parallel.
For example if you have a process that all other processes in the system needs
to interact with (maybe it has some global configuration), then `lcnt` can be
used to figure out if the interaction with that process is a problem.

In the Erlang Run-time System entities are only run in parallel when there are
multiple schedulers. Therefore `lcnt` will show more contention points (and thus
be more useful) on systems using many schedulers on many cores.

For more information, see the `m:lcnt` manual page in Tools.

[](){: #benchmark }

## Benchmarking

The main purpose of benchmarking is to find out which implementation of a given
algorithm or function is the fastest. Benchmarking is far from an exact science.
Today's operating systems generally run background tasks that are difficult to
turn off. Caches and multiple CPU cores do not facilitate benchmarking. It would
be best to run UNIX computers in single-user mode when benchmarking, but that is
inconvenient to say the least for casual testing.

Benchmarks can measure wall-clock time or CPU time.

- `timer:tc/3` measures wall-clock time. The advantage with wall-clock time is
  that I/O, swapping, and other activities in the operating system kernel are
  included in the measurements. The disadvantage is that the measurements vary a
  lot. Usually it is best to run the benchmark several times and note the
  shortest time, which is to be the minimum time that is possible to achieve
  under the best of circumstances.
- [statistics/1](`erlang:statistics/1`) with argument `runtime` measures CPU
  time spent in the Erlang virtual machine. The advantage with CPU time is that
  the results are more consistent from run to run. The disadvantage is that the
  time spent in the operating system kernel (such as swapping and I/O) is not
  included. Therefore, measuring CPU time is misleading if any I/O (file or
  socket) is involved.

It is probably a good idea to do both wall-clock measurements and CPU time
measurements.

Some final advice:

- The granularity of both measurement types can be high. Therefore, ensure that
  each individual measurement lasts for at least several seconds.
- To make the test fair, each new test run is to run in its own, newly created
  Erlang process. Otherwise, if all tests run in the same process, the later
  tests start out with larger heap sizes and therefore probably do fewer garbage
  collections. Also consider restarting the Erlang emulator between each test.
- Do not assume that the fastest implementation of a given algorithm on computer
  architecture X is also the fastest on computer architecture Y.
