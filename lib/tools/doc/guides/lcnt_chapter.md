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
# lcnt - The Lock Profiler

Internally in the Erlang runtime system locks are used to protect resources from
being updated from multiple threads in a fatal way. Locks are necessary to
ensure that the runtime system works properly, but it also introduces
limitations, namely _lock contention_ and _locking overhead_.

With lock contention we mean when one thread locks a resource, and another
thread (or threads) tries to acquire the same resource at the same time. The
lock will deny the other thread access to the resource and the thread will be
blocked from continuing its execution. The second thread has to wait until the
first thread has completed its access to the resource and unlocked it. The
`lcnt` tool measures these lock conflicts.

Locks have an inherent cost in execution time and memory space. It takes time to
initialize, destroy, acquire, or release locks. To decrease lock contention
it is sometimes necessary to use finer-grained locking strategies. This
usually also increases the locking overhead. Hence there is a tradeoff between
lock contention and overhead. In general, lock contention increases with the
number of threads running concurrently.

The `lcnt` tool does not measure locking overhead.

## Enabling lock-counting

For investigation of locks in the emulator we use an internal tool called `lcnt`
(short for lock-count). The VM needs to be compiled with this option enabled. To
compile a lock-counting VM along with a normal VM, use:

```text
cd $ERL_TOP
./configure --enable-lock-counter
make
```

Start the lock-counting VM like this:

```text
$ERL_TOP/bin/erl -emu_type lcnt
```

To verify that lock counting is enabled check that `[lock-counting]` appears in
the status text when the VM is started.

```text
Erlang/OTP 27 [erts-15.0] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit] [lock-counting]
```

## Getting started

Once you have a lock counting enabled VM the module `lcnt` can be used. The
module is intended to be used from the current running nodes shell. To access
remote nodes use [`lcnt:clear(Node)`](`lcnt:clear/1`) and
[`lcnt:collect(Node)`](`lcnt:collect/1`).

All locks are continuously monitored and its statistics updated. Use
[`lcnt:clear/0`](`lcnt:clear/1`) to initially clear all counters
before running any specific tests. This command will also reset the
internal duration timer.

To retrieve lock statistics information, use
[`lcnt:collect/0,1`](`lcnt:collect/1`). The collect operation will
start a `lcnt` server if it not already started. All collected data
will be stored in an Erlang term and uploaded to the server along with
the duration time. The duration time is the time between
[`lcnt:clear/0,1`](`lcnt:clear/1`) and
[`lcnt:collect/0,1`](`lcnt:collect/1`).

Once the data is collected to the server it can be filtered, sorted, and printed
in multiple ways.

## Example of usage

Here is an example of running the [Big Bang Benchmark](#the-big-bang-benchmark):

```text
Erlang/OTP 27 [erts-15.0] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit] [lock-counting]

Eshell V15.0 (press Ctrl+G to abort, type help(). for help)
1> lcnt:rt_opt({copy_save, true}).
false
2> lcnt:clear(), big:bang(1000), lcnt:collect().
ok
3> lcnt:conflicts().
      lock   id  #tries  #collisions  collisions [%]  time [us]  duration [%]
     -----  --- ------- ------------ --------------- ---------- -------------
 run_queue   10  590799         8875          1.5022      37906        2.2167
 proc_msgq 1048 2515180         4667          0.1856      20962        1.2258
 proc_main 1048 2195317        23775          1.0830       1664        0.0973
ok
```

Another way to to profile a specific function is to use `lcnt:apply/3` or
`lcnt:apply/1`, which calls `lcnt:clear/0` before calling the function and
`lcnt:collect/0` after its invocation. This method should only be used in
micro-benchmarks since it sets `copy_save` to `true` for the duration of the
function call, which may cause the emulator to run out of memory if attempted
under load.

```text
1> lcnt:apply(fun() -> big:bang(1000) end).
1845411
2> lcnt:conflicts().
      lock   id  #tries  #collisions  collisions [%]  time [us]  duration [%]
     -----  --- ------- ------------ --------------- ---------- -------------
 run_queue   10  582789         9237          1.5850      41929        2.2633
 proc_msgq 1047 2494483         4731          0.1897      11173        0.6031
 proc_main 1047 2192806        23283          1.0618       1500        0.0810
ok
```

The process locks are sorted after its class like all other locks. It is
convenient to look at specific processes and ports as classes. We can do this by
swapping class and class identifiers with `lcnt:swap_pid_keys/0`.

```text
3> lcnt:swap_pid_keys().
ok
4> lcnt:conflicts([{print, [name, tries, ratio, time]}]).
                   lock  #tries  collisions [%]  time [us]
                  ----- ------- --------------- ----------
              run_queue  582789          1.5850      41929
 <nonode@nohost.1042.0>    5692          0.5095        484
  <nonode@nohost.465.0>    4989          0.4410        393
  <nonode@nohost.347.0>    6319          2.1839        284
  <nonode@nohost.436.0>    6077          1.9747        198
  <nonode@nohost.307.0>    5071          1.3015        192
  <nonode@nohost.455.0>    5846          1.7106        186
  <nonode@nohost.565.0>    6305          1.2054        179
  <nonode@nohost.461.0>    5820          1.2715        176
  <nonode@nohost.173.0>    6329          1.4852        168
  <nonode@nohost.453.0>    5172          0.8701        167
  <nonode@nohost.741.0>    5306          0.4146        166
  <nonode@nohost.403.0>    5838          1.9870        160
  <nonode@nohost.463.0>    6346          1.5443        143
  <nonode@nohost.184.0>    5542          0.4331        141
  <nonode@nohost.289.0>    5260          0.2662        137
  <nonode@nohost.166.0>    5610          0.9447        127
  <nonode@nohost.189.0>    5354          0.5230        118
  <nonode@nohost.121.0>    5845          0.9239        115
  <nonode@nohost.104.0>    5140          0.7782        108
ok
```

## Example with Mnesia Transaction Benchmark

From the Erlang shell:

```erlang
Erlang/OTP 27 [erts-15.0] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit] [lock-counting]

Eshell V15.0 (press Ctrl+G to abort, type help(). for help)
1> Conf = [{db_nodes, [node()]}, {driver_nodes, [node()]}, {replica_nodes, [node()]},
    {n_drivers_per_node, 10}, {n_branches, 1000}, {n_accounts_per_branch, 10},
    {replica_type, ram_copies}, {stop_after, 60000}, {reuse_history_id, true}], ok.
ok
2> mnesia_tpcb:init([{use_running_mnesia, false}|Conf]).
    .
    .
    .
ignore
```

Initial configuring of the benchmark is done. It is time to profile the actual
Mnesia benchmark:

```erlang
3> lcnt:apply(fun() -> {ok,{time, Tps,_,_,_,_}} = mnesia_tpcb:run([{use_running_mnesia,
    true}|Conf]), Tps/60 end).
      .
      .
      .
50204.666666666664
```

The benchmark runs for 60 seconds (followed by verification and
analysis), and then returns the number of transactions per seconds.

```text
4> lcnt:swap_pid_keys().
ok
5> lcnt:conflicts().
                   lock      id    #tries  #collisions  collisions [%]  time [us]  duration [%]
                  -----     ---   ------- ------------ --------------- ---------- -------------
              run_queue      10  89329288      3227515          3.6131    5018119        8.3606
          mnesia_locker       5  64793236      8231226         12.7038      98654        0.1644
                 db_tab 3012324 416847817       140631          0.0337      75308        0.1255
 <nonode@nohost.1158.0>       5  14499900        36934          0.2547       4878        0.0081
 <nonode@nohost.1157.0>       5  14157504        35797          0.2528       4727        0.0079
 <nonode@nohost.1163.0>       5  14194934        34510          0.2431       4403        0.0073
 <nonode@nohost.1164.0>       5  14149447        35326          0.2497       4150        0.0069
 <nonode@nohost.1166.0>       5  14316525        35675          0.2492       4116        0.0069
 <nonode@nohost.1159.0>       5  14241146        35358          0.2483       4101        0.0068
 <nonode@nohost.1162.0>       5  14224491        35182          0.2473       4094        0.0068
 <nonode@nohost.1160.0>       5  14190075        35328          0.2490       4075        0.0068
 <nonode@nohost.1165.0>       5  14308906        35031          0.2448       3896        0.0065
 <nonode@nohost.1161.0>       5  14457330        36182          0.2503       3856        0.0064
              mnesia_tm       5  28149333       179294          0.6369       1057        0.0018
               pix_lock    1024       132            1          0.7576        549        0.0009
   <nonode@nohost.85.0>       5        17            2         11.7647         87        0.0001
 <nonode@nohost.1156.0>       5      1335            6          0.4494          1        0.0000
ok
```

The `id` header represents the number of unique identifiers under a
class when the option `{combine, true}` is used (which is enabled by
default). It will otherwise show the specific identifier. The `db_tab`
listing shows 3,012,324 unique locks, which is one for each ETS table
created. Mnesia creates one ETS table for each transaction.

The listing shows also shows that the `mnesia_locker` process has highly contended locks.
Using `lcnt:inspect/1` more information can be displayed for that process:

```text
6> lcnt:inspect(mnesia_locker).
          lock          id   #tries  #collisions  collisions [%]  time [us]  duration [%] histogram [log2(us)]
         -----         ---  ------- ------------ --------------- ---------- ------------- ---------------------
 mnesia_locker   proc_main 19853372      7591248         38.2366      80550        0.1342 |.    ...X........             |
 mnesia_locker   proc_msgq 30917225       639627          2.0688      17126        0.0285 |.    .X.........              |
 mnesia_locker proc_status  9348426          351          0.0038        978        0.0016 |        .xxX. .               |
 mnesia_locker    proc_btm        0            0          0.0000          0        0.0000 |                              |
 mnesia_locker  proc_trace  4674213            0          0.0000          0        0.0000 |                              |
ok
```

Listing the conflicts without class combiner:

```text
7> lcnt:conflicts([{combine, false}, {print, [name, id, tries, ratio, time]}]).
                   lock                        id   #tries  collisions [%]  time [us]
                  -----                       ---  ------- --------------- ----------
              run_queue                         2 31075249          3.5676    1728233
              run_queue                         1 29738521          3.6348    1683219
              run_queue                         3 27912150          3.6429    1573593
          mnesia_locker                 proc_main 19853372         38.2366      80550
                 db_tab mnesia_transient_decision  3012281          2.5675      55104
              run_queue                         4   512077          3.7041      29486
          mnesia_locker                 proc_msgq 30917225          2.0688      17126
                 db_tab                   account  6044562          0.3599       7909
                 db_tab                    branch  6026659          0.3132       5654
                 db_tab                    teller  6044659          0.2684       4727
 <nonode@nohost.1158.0>                 proc_main  3207155          0.7178       3726
 <nonode@nohost.1163.0>                 proc_main  3138532          0.7485       3593
 <nonode@nohost.1157.0>                 proc_main  3133180          0.7156       3547
 <nonode@nohost.1166.0>                 proc_main  3165128          0.7609       3517
 <nonode@nohost.1164.0>                 proc_main  3128838          0.7525       3477
 <nonode@nohost.1160.0>                 proc_main  3137627          0.7559       3433
 <nonode@nohost.1162.0>                 proc_main  3144886          0.7509       3425
 <nonode@nohost.1159.0>                 proc_main  3149315          0.7487       3372
 <nonode@nohost.1161.0>                 proc_main  3196546          0.7591       3310
 <nonode@nohost.1165.0>                 proc_main  3164333          0.7483       3309
ok
```

In this scenario the locks for the scheduler's run queues dominate the time waiting
for locks. The most contended lock for ETS tables is for the `mnesia_transient_decision`
ETS table.

Here is how to show the information for the ETS tables.

```text
8> lcnt:inspect(db_tab, [{print, [name, id, tries, colls, ratio, duration]}]).
   lock                        id    #tries  #collisions  collisions [%]  duration [%]
  -----                       ---   ------- ------------ --------------- -------------
 db_tab mnesia_transient_decision   3012281        77341          2.5675        0.0918
 db_tab                   account   6044562        21753          0.3599        0.0132
 db_tab                    branch   6026659        18873          0.3132        0.0094
 db_tab                    teller   6044659        16221          0.2684        0.0079
 db_tab                   history   3012281         4005          0.1330        0.0022
 db_tab              mnesia_stats   3071064         2437          0.0794        0.0010
 db_tab        mnesia_trans_store        15            0          0.0000        0.0000
 db_tab           mnesia_decision   3012281            0          0.0000        0.0000
 db_tab                    schema         0            0          0.0000        0.0000
 db_tab                      dets         0            0          0.0000        0.0000
 db_tab               dets_owners         0            0          0.0000        0.0000
 db_tab             dets_registry         0            0          0.0000        0.0000
 db_tab         mnesia_lock_queue  36154974            0          0.0000        0.0000
 db_tab       mnesia_sticky_locks  12108098            0          0.0000        0.0000
 db_tab          mnesia_tid_locks  27176721            0          0.0000        0.0000
 db_tab         mnesia_held_locks  48321870            0          0.0000        0.0000
 db_tab             mnesia_subscr         0            0          0.0000        0.0000
 db_tab               mnesia_gvar 102680683            1          0.0000        0.0000
 db_tab            user_functions         0            0          0.0000        0.0000
 db_tab             shell_records         0            0          0.0000        0.0000
ok
```

## Deciphering the output

Typically high `time` values are bad and this is often the thing to look for.
However, one should also look for high lock acquisition frequencies (`#tries`)
since locks generate overhead and because high frequency could become
problematic if they begin to have conflicts even if it is not shown in a
particular test.

## The Big Bang Benchmark

```erlang
-module(big).
-export([bang/1]).

pinger([], [], true) ->
    receive
	{procs, Procs, ReportTo} ->
	    pinger(Procs, [], ReportTo)
    end;
pinger([], [], false) ->
    receive {ping, From} -> From ! {pong, self()} end,
    pinger([],[],false);
pinger([], [], ReportTo) ->
    ReportTo ! {done, self()},
    pinger([],[],false);
pinger([], [Po|Pos] = Pongers, ReportTo) ->
    receive
	{ping, From} ->
	    From ! {pong, self()},
	    pinger([], Pongers, ReportTo);
	{pong, Po} ->
	    pinger([], Pos, ReportTo)
    end;
pinger([Pi|Pis], Pongers, ReportTo) ->
    receive {ping, From} -> From ! {pong, self()}
    after 0 -> ok
    end,
    Pi ! {ping, self()},
    pinger(Pis, [Pi|Pongers], ReportTo).

spawn_procs(N) when N =< 0 ->
    [];
spawn_procs(N) ->
    [spawn_link(fun () -> pinger([],[],true) end) | spawn_procs(N-1)].

send_procs([], Msg) ->
    Msg;
send_procs([P|Ps], Msg) ->
    P ! Msg,
    send_procs(Ps, Msg).

receive_msgs([]) ->
    ok;
receive_msgs([M|Ms]) ->
    receive
	M ->
	    receive_msgs(Ms)
    end.

bang(N) when integer(N) ->
    Procs = spawn_procs(N),
    RMsgs = lists:map(fun (P) -> {done, P} end, Procs),
    Start = now(),
    send_procs(Procs, {procs, Procs, self()}),
    receive_msgs(RMsgs),
    Stop = now(),
    lists:foreach(fun (P) -> exit(P, normal) end, Procs),
    timer:now_diff(Stop, Start).
```

## See Also

[LCNT Reference Manual](`m:lcnt`)
