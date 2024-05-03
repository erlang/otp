<!--
%CopyrightBegin%

Copyright Ericsson AB 2023. All Rights Reserved.

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
# Common Test's Property Testing Support: ct_property_test

## General

The _Common Test Property Testing Support (ct_property_test)_ is an aid to run
property based testing tools in Common Test test suites.

Basic knowledge of property based testing is assumed in the following. It is
also assumed that at least one of the following property based testing tools is
installed and available in the library path:

- [QuickCheck](http://www.quviq.com),
- [PropEr](https://proper-testing.github.io) or
- [Triq](https://github.com/krestenkrab/triq)

[](){: #supported }

## What Is Supported?

The [ct_property_test](`m:ct_property_test#`) module does the following:

- Compiles the files with property tests in the subdirectory `property_test`
- Tests properties in those files using the first found Property Testing Tool.
- Saves the results - that is the printouts - in the usual Common Test Log

## Introductory Example

Assume that we want to test the lists:sort/1 function.

We need a property to test the function. In normal way, we create
`property_test/ct_prop.erl` module in the `test` directory in our application:

```erlang
-module(ct_prop).
-export([prop_sort/0]).

%%% This will include the .hrl file for the installed testing tool:
-include_lib("common_test/include/ct_property_test.hrl").

%%% The property we want to check:
%%%   For all possibly unsorted lists,
%%%   the result of lists:sort/1 is sorted.
prop_sort() ->
    ?FORALL(UnSorted, list(),
            is_sorted(lists:sort(UnSorted))
           ).

%%% Function to check that a list is sorted:
is_sorted([]) ->
    true;
is_sorted([_]) ->
    true;
is_sorted([H1,H2|SortedTail]) when H1 =< H2 ->
    is_sorted([H2|SortedTail]);
is_sorted(_) ->
    false.
```

We also need a CommonTest test suite:

```erlang
-module(ct_property_test_SUITE).
-compile(export_all). % Only in tests!

-include_lib("common_test/include/ct.hrl").

all() -> [prop_sort
         ].

%%% First prepare Config and compile the property tests for the found tool:
init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

%%%================================================================
%%% Test suites
%%%
prop_sort(Config) ->
    ct_property_test:quickcheck(
      ct_prop:prop_sort(),
      Config
     ).
```

We run it as usual, for example with ct_run in the OS shell:

```text
..../test$ ct_run -suite ct_property_test_SUITE
.....
Common Test: Running make in test directories...

TEST INFO: 1 test(s), 1 case(s) in 1 suite(s)

Testing lib.common_test.ct_property_test_SUITE: Starting test, 1 test cases

----------------------------------------------------
2019-12-18 10:44:46.293
Found property tester proper
at "/home/X/lib/proper/ebin/proper.beam"


----------------------------------------------------
2019-12-18 10:44:46.294
Compiling in "/home/..../test/property_test"
  Deleted:   ["ct_prop.beam"]
  ErlFiles:  ["ct_prop.erl"]
  MacroDefs: [{d,'PROPER'}]

Testing lib.common_test.ct_property_test_SUITE: TEST COMPLETE, 1 ok, 0 failed of 1 test cases

....
```

[](){: #stateful1 }

## A stateful testing example

Assume a test that generates some parallel stateful commands, and runs 300
tests:

```erlang
prop_parallel(Config) ->
    numtests(300,
             ?FORALL(Cmds, parallel_commands(?MODULE),
                     begin
                         RunResult = run_parallel_commands(?MODULE, Cmds),
                         ct_property_test:present_result(?MODULE, Cmds, RunResult, Config)
                     end)).
```

The `ct_property_test:present_result/4` is a help function for printing some
statistics in the CommonTest log file.

Our example test could for example be a simple test of an ftp server, where we
perform get, put and delete requests, some of them in parallel. Per default, the
result has three sections:

```text
*** User 2019-12-11 13:28:17.504 ***

Distribution sequential/parallel

 57.7% sequential
 28.0% parallel_2
 14.3% parallel_1



*** User 2019-12-11 13:28:17.505 ***

Function calls

 44.4% get
 39.3% put
 16.3% delete



*** User 2019-12-11 13:28:17.505 ***

Length of command sequences

Range  : Number in range
-------:----------------
 0 -  4:    8    2.7%  <-- min=3
 5 -  9:   44   14.7%
10 - 14:   74   24.7%
15 - 19:   60   20.0%  <-- mean=18.7 <-- median=16.0
20 - 24:   38   12.7%
25 - 29:   26    8.7%
30 - 34:   19    6.3%
35 - 39:   19    6.3%
40 - 44:    8    2.7%
45 - 49:    4    1.3%  <-- max=47
        ------
          300
```

The first part - _Distribution sequential/parallel_ \- shows the distribution in
the sequential and parallel part of the result of parallel_commands/1. See any
property testing tool for an explanation of this function. The table shows that
of all commands (get and put in our case), 57.7% are executed in the sequential
part prior to the parallel part, 28.0% are executed in the first parallel list
and the rest in the second parallel list.

The second part - _Function calls_ \- shows the distribution of the three calls
in the generated command lists. We see that all of the three calls are executed.
If it was so that we thought that we also generated a fourth call, a table like
this shows that we failed with that.

The third and final part - _Length of command sequences_ \- show statistics of
the generated command sequences. We see that the shortest list has three
elementes while the longest has 47 elements. The mean and median values are also
shown. Further we could for example see that only 2.7% of the lists (that is
eight lists) only has three or four elements.
