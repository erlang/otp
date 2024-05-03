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
# cover - The Coverage Analysis Tool

## Introduction

The module `cover` provides a set of functions for coverage analysis of Erlang
programs, counting how many times each [executable line](cover_chapter.md#lines)
is executed.

Coverage analysis can be used to verify test cases, making sure all relevant
code is covered, and can be helpful when looking for bottlenecks in the code.

## Getting Started With Cover

### Example

Assume that a test case for the following program should be verified:

```erlang
-module(channel).
-behaviour(gen_server).

-export([start_link/0,stop/0]).
-export([alloc/0,free/1]). % client interface
-export([init/1,handle_call/3,terminate/2]). % callback functions

start_link() ->
    gen_server:start_link({local,channel}, channel, [], []).

stop() ->
    gen_server:call(channel, stop).

%%%-Client interface functions-------------------------------------------

alloc() ->
    gen_server:call(channel, alloc).

free(Channel) ->
    gen_server:call(channel, {free,Channel}).

%%%-gen_server callback functions----------------------------------------

init(_Arg) ->
    {ok,channels()}.

handle_call(stop, _Client, Channels) ->
    {stop,normal,ok,Channels};

handle_call(alloc, _Client, Channels) ->
    {Ch,Channels2} = alloc(Channels),
    {reply,{ok,Ch},Channels2};

handle_call({free,Channel}, _Client, Channels) ->
    Channels2 = free(Channel, Channels),
    {reply,ok,Channels2}.

terminate(_Reason, _Channels) ->
    ok.

%%%-Internal functions---------------------------------------------------

channels() ->
    [ch1,ch2,ch3].

alloc([Channel|Channels]) ->
    {Channel,Channels};
alloc([]) ->
    false.

free(Channel, Channels) ->
    [Channel|Channels].
```

The test case is implemented as follows:

```erlang
-module(test).
-export([s/0]).

s() ->
    {ok,Pid} = channel:start_link(),
    {ok,Ch1} = channel:alloc(),
    ok = channel:free(Ch1),
    ok = channel:stop().
```

### Preparation

First of all, Cover must be started. This spawns a process which owns the Cover
database where all coverage data will be stored.

```erlang
1> cover:start().
{ok,<0.90.0>}
```

To include other nodes in the coverage analysis, use
`cover:start/1`. All cover-compiled modules will then be loaded on all
nodes, and data from all nodes will be summed up when analysing. For
simplicity this example only involves the current node.

Before any analysis can take place, the involved modules must be
_cover-compiled_. This means that some extra information is added to
the module before beging compiled into a binary and
[loaded](cover_chapter.md#loading).  The source file of the module is
not affected and no `.beam` file is created.

```erlang
2> cover:compile_module(channel).
{ok,channel}
```

Each time a function in the cover-compiled module `channel` is called,
information about the call will be added to the Cover database. Run the test
case:

```text
3> test:s().
ok
```

Cover analysis is performed by examining the contents of the Cover database. The
output is determined by two parameters, `Level` and `Analysis`. `Analysis` is
either `coverage` or `calls` and determines the type of the analysis. `Level` is
either `module`, `function`, `clause`, or `line` and determines the level of the
analysis.

### Coverage Analysis

Analysis of type `coverage` is used to find out how much of the code has been
executed and how much has not been executed. Coverage is represented by a tuple
`{Cov,NotCov}`, where `Cov` is the number of executable lines that have been
executed at least once and `NotCov` is the number of executable lines that have
not been executed.

If the analysis is made on module level, the result is given for the entire
module as a tuple `{Module,{Cov,NotCov}}`:

```erlang
4> cover:analyse(channel, coverage, module).
{ok,{channel,{14,1}}}
```

For `channel`, the result shows that 14 lines in the module are covered but one
line is not covered.

If the analysis is made on function level, the result is given as a list of
tuples `{Function,{Cov,NotCov}}`, one for each function in the module. A
function is specified by its module name, function name and arity:

```erlang
5> cover:analyse(channel, coverage, function).
{ok,[{{channel,start_link,0},{1,0}},
     {{channel,stop,0},{1,0}},
     {{channel,alloc,0},{1,0}},
     {{channel,free,1},{1,0}},
     {{channel,init,1},{1,0}},
     {{channel,handle_call,3},{5,0}},
     {{channel,terminate,2},{1,0}},
     {{channel,channels,0},{1,0}},
     {{channel,alloc,1},{1,1}},
     {{channel,free,2},{1,0}}]}
```

For `channel`, the result shows that the uncovered line is in the function
`channel:alloc/1`.

If the analysis is made on clause level, the result is given as a list of tuples
`{Clause,{Cov,NotCov}}`, one for each function clause in the module. A clause is
specified by its module name, function name, arity and position within the
function definition:

```erlang
6> cover:analyse(channel, coverage, clause).
{ok,[{{channel,start_link,0,1},{1,0}},
     {{channel,stop,0,1},{1,0}},
     {{channel,alloc,0,1},{1,0}},
     {{channel,free,1,1},{1,0}},
     {{channel,init,1,1},{1,0}},
     {{channel,handle_call,3,1},{1,0}},
     {{channel,handle_call,3,2},{2,0}},
     {{channel,handle_call,3,3},{2,0}},
     {{channel,terminate,2,1},{1,0}},
     {{channel,channels,0,1},{1,0}},
     {{channel,alloc,1,1},{1,0}},
     {{channel,alloc,1,2},{0,1}},
     {{channel,free,2,1},{1,0}}]}
```

For `channel`, the result shows that the uncovered line is in the second clause
of `channel:alloc/1`.

Finally, if the analysis is made on line level, the result is given as a list of
tuples `{Line,{Cov,NotCov}}`, one for each executable line in the source code. A
line is specified by its module name and line number.

```erlang
7> cover:analyse(channel, coverage, line).
{ok,[{{channel,9},{1,0}},
     {{channel,12},{1,0}},
     {{channel,17},{1,0}},
     {{channel,20},{1,0}},
     {{channel,25},{1,0}},
     {{channel,28},{1,0}},
     {{channel,31},{1,0}},
     {{channel,32},{1,0}},
     {{channel,35},{1,0}},
     {{channel,36},{1,0}},
     {{channel,39},{1,0}},
     {{channel,44},{1,0}},
     {{channel,47},{1,0}},
     {{channel,49},{0,1}},
     {{channel,52},{1,0}}]}
```

For `channel`, the result shows that the uncovered line is line number 49.

### Call Statistics

Analysis of type `calls` is used to find out how many times something has been
called and is represented by an integer `Calls`.

If the analysis is made on module level, the result is given as a tuple
`{Module,Calls}`. Here `Calls` is the total number of calls to functions in the
module:

```erlang
8> cover:analyse(channel, calls, module).
{ok,{channel,12}}
```

For `channel`, the result shows that a total of twelve calls have been made to
functions in the module.

If the analysis is made on function level, the result is given as a list of
tuples `{Function,Calls}`. Here `Calls` is the number of calls to each function:

```erlang
9> cover:analyse(channel, calls, function).
{ok,[{{channel,start_link,0},1},
     {{channel,stop,0},1},
     {{channel,alloc,0},1},
     {{channel,free,1},1},
     {{channel,init,1},1},
     {{channel,handle_call,3},3},
     {{channel,terminate,2},1},
     {{channel,channels,0},1},
     {{channel,alloc,1},1},
     {{channel,free,2},1}]}
```

For `channel`, the result shows that `handle_call/3` is the most called function
in the module (three calls). All other functions have been called once.

If the analysis is made on clause level, the result is given as a list of tuples
`{Clause,Calls}`. Here `Calls` is the number of calls to each function clause:

```erlang
10> cover:analyse(channel, calls, clause).
{ok,[{{channel,start_link,0,1},1},
     {{channel,stop,0,1},1},
     {{channel,alloc,0,1},1},
     {{channel,free,1,1},1},
     {{channel,init,1,1},1},
     {{channel,handle_call,3,1},1},
     {{channel,handle_call,3,2},1},
     {{channel,handle_call,3,3},1},
     {{channel,terminate,2,1},1},
     {{channel,channels,0,1},1},
     {{channel,alloc,1,1},1},
     {{channel,alloc,1,2},0},
     {{channel,free,2,1},1}]}
```

For `channel`, the result shows that all clauses have been called once, except
the second clause of `channel:alloc/1` which has not been called at all.

Finally, if the analysis is made on line level, the result is given as a list of
tuples `{Line,Calls}`. Here `Calls` is the number of times each line has been
executed:

```erlang
11> cover:analyse(channel, calls, line).
{ok,[{{channel,9},1},
     {{channel,12},1},
     {{channel,17},1},
     {{channel,20},1},
     {{channel,25},1},
     {{channel,28},1},
     {{channel,31},1},
     {{channel,32},1},
     {{channel,35},1},
     {{channel,36},1},
     {{channel,39},1},
     {{channel,44},1},
     {{channel,47},1},
     {{channel,49},0},
     {{channel,52},1}]}
```

For `channel`, the result shows that all lines have been executed once, except
line number 49 which has not been executed at all.

### Analysis to File

A line level calls analysis of `channel` can be written to a file using
`cover:analyse_to_file/1`:

```erlang
12> cover:analyse_to_file(channel).
{ok,"channel.COVER.out"}
```

The function creates a copy of `channel.erl` where it for each executable line
is specified how many times that line has been executed. The output file is
called `channel.COVER.out`.

```erlang
File generated from /Users/bjorng/git/otp/channel.erl by COVER 2024-03-20 at 13:25:04

****************************************************************************

        |  -module(channel).
        |  -behaviour(gen_server).
        |
        |  -export([start_link/0,stop/0]).
        |  -export([alloc/0,free/1]). % client interface
        |  -export([init/1,handle_call/3,terminate/2]). % callback functions
        |
        |  start_link() ->
     1..|      gen_server:start_link({local,channel}, channel, [], []).
        |
        |  stop() ->
     1..|      gen_server:call(channel, stop).
        |
        |  %%%-Client interface functions-------------------------------------------
        |
        |  alloc() ->
     1..|      gen_server:call(channel, alloc).
        |
        |  free(Channel) ->
     1..|      gen_server:call(channel, {free,Channel}).
        |
        |  %%%-gen_server callback functions----------------------------------------
        |
        |  init(_Arg) ->
     1..|      {ok,channels()}.
        |
        |  handle_call(stop, _Client, Channels) ->
     1..|      {stop,normal,ok,Channels};
        |
        |  handle_call(alloc, _Client, Channels) ->
     1..|      {Ch,Channels2} = alloc(Channels),
     1..|      {reply,{ok,Ch},Channels2};
        |
        |  handle_call({free,Channel}, _Client, Channels) ->
     1..|      Channels2 = free(Channel, Channels),
     1..|      {reply,ok,Channels2}.
        |
        |  terminate(_Reason, _Channels) ->
     1..|      ok.
        |
        |  %%%-Internal functions---------------------------------------------------
        |
        |  channels() ->
     1..|      [ch1,ch2,ch3].
        |
        |  alloc([Channel|Channels]) ->
     1..|      {Channel,Channels};
        |  alloc([]) ->
     0..|      false.
        |
        |  free(Channel, Channels) ->
     1..|      [Channel|Channels].
```

### Conclusion

By looking at the results from the analyses, it can be deduced that
the test case does not cover the case when all channels are allocated
and `test.erl` should be extended accordingly. Incidentally, when the
test case is corrected a bug in `channel` will be discovered.

When the Cover analysis is ready, Cover is stopped and all cover-compiled
modules are [unloaded](cover_chapter.md#loading). The code for `channel` is now
loaded as usual from a `.beam` file in the current path.

```erlang
13> code:which(channel).
cover_compiled
14> cover:stop().
ok
15> code:which(channel).
"./channel.beam"
```

## Miscellaneous

### Performance

Execution of code in cover-compiled modules is slower and more memory consuming
than for regularly compiled modules. As the Cover database contains information
about each executable line in each cover-compiled module, performance decreases
proportionally to the size and number of the cover-compiled modules.

To improve performance when analysing cover results it is possible to do
multiple calls to [analyse](`cover:analyse/1`) and
[analyse_to_file](`cover:analyse_to_file/1`) at once. You can also use the
[async_analyse_to_file](`cover:async_analyse_to_file/1`) convenience function.

[](){: #lines }

### Executable Lines

Cover uses the concept of _executable lines_, which is code lines containing
an executable expression such as a matching or a function call. A blank line or
a line containing a comment, function head or pattern in a `case` or `receive`
statement is not executable.

In the example below, lines number 2, 4, 6, 8, and 11 are executable lines:

```erlang
1: is_loaded(Module, Compiled) ->
2:   case get_file(Module, Compiled) of
3:     {ok,File} ->
4:       case code:which(Module) of
5:         ?TAG ->
6:           {loaded,File};
7:         _ ->
8:           unloaded
9:       end;
10:    false ->
11:      false
12:  end.
```

[](){: #loading }

### Code Loading Mechanism

When a module is cover-compiled, it is also loaded using the normal code loading
mechanism of Erlang. This means that if a cover-compiled module is re-loaded
during a Cover session, for example using `c(Module)`, it will no longer be
cover-compiled.

Use `cover:is_compiled/1` or `code:which/1` to see whether or not a
module is cover-compiled (and still loaded).

When Cover is stopped, all cover-compiled modules are unloaded.
