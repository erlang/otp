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
# Benchmarking

The main purpose of benchmarking is to find out which implementation of a given
algorithm or function is the fastest. Benchmarking is far from an exact science.
Today's operating systems generally run background tasks that are difficult to
turn off. Caches and multiple CPU cores do not facilitate benchmarking. It would
be best to run UNIX computers in single-user mode when benchmarking, but that is
inconvenient to say the least for casual testing.

## Using erlperf

A useful tool for benchmarking is [erlperf](https://github.com/max-au/erlperf)
([documentation](https://hexdocs.pm/erlperf/erlperf.html)).
It makes it simple to find out which code is faster. For example, here is how
two methods of generating random bytes can be compared:

```text
% erlperf 'rand:bytes(2).' 'crypto:strong_rand_bytes(2).'
Code                                 ||        QPS       Time   Rel
rand:bytes(2).                        1    7784 Ki     128 ns  100%
crypto:strong_rand_bytes(2).          1    2286 Ki     437 ns   29%
```

From the **Time** column we can read out that on average a call to
[`rand:bytes(2)`](`rand:bytes/1`) executes in 128 nano seconds, while
a call to
[`crypto:strong_rand_bytes(2)`](`crypto:strong_rand_bytes/1`) executes
in 437 nano seconds.

From the **QPS** column we can read out how many calls that can be
made in a second. For `rand:bytes(2)`, it is 7,784,000 calls per second.

The **Rel** column shows the relative differences, with `100%` indicating
the fastest code.

When generating two random bytes at the time, `rand:bytes/1` is more
than three times faster than `crypto:strong_rand_bytes/1`. Assuming
that we really need strong random numbers and we need to get them as
fast as possible, what can we do? One way could be to generate more
than two bytes at the time.

```text
% erlperf 'rand:bytes(100).' 'crypto:strong_rand_bytes(100).'
Code                                   ||        QPS       Time   Rel
rand:bytes(100).                        1    2124 Ki     470 ns  100%
crypto:strong_rand_bytes(100).          1    1915 Ki     522 ns   90%
```

`rand:bytes/1` is still faster when we generate 100 bytes at the time,
but the relative difference is smaller.

```
% erlperf 'rand:bytes(1000).' 'crypto:strong_rand_bytes(1000).'
Code                                    ||        QPS       Time   Rel
crypto:strong_rand_bytes(1000).          1    1518 Ki     658 ns  100%
rand:bytes(1000).                        1     284 Ki    3521 ns   19%
```

When we generate 1000 bytes at the time, `crypto:strong_rand_bytes/1` is
now the fastest.

## Benchmarking using Erlang/OTP functionality

Benchmarks can measure wall-clock time or CPU time.

- `timer:tc/3` measures wall-clock time. The advantage with wall-clock time is
  that I/O, swapping, and other activities in the operating system kernel are
  included in the measurements. The disadvantage is that the measurements often
  vary a lot. Usually it is best to run the benchmark several times and note
  the shortest time, which is the minimum time that is possible to achieve
  under the best of circumstances.

- [`statistics(runtime)`](`erlang:statistics/1`) measures CPU time spent
  in the Erlang virtual machine. The advantage with CPU time is that
  the results are more consistent from run to run. The disadvantage is
  that the time spent in the operating system kernel (such as swapping
  and I/O) is not included. Therefore, measuring CPU time is
  misleading if any I/O (file or socket) is involved.

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
