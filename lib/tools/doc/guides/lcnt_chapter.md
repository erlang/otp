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
ensure that the runtime system works properly but it also introduces a couple of
limitations. Lock contention and locking overhead.

With lock contention we mean when one thread locks a resource and another
thread, or threads, tries to acquire the same resource at the same time. The
lock will deny the other thread access to the resource and the thread will be
blocked from continuing its execution. The second thread has to wait until the
first thread has completed its access to the resource and unlocked it. The
`lcnt` tool measures these lock conflicts.

Locks have an inherent cost in execution time and memory space. It takes time
initialize, destroy, acquiring or releasing locks. To decrease lock contention
it some times necessary to use finer grained locking strategies. This will
usually also increase the locking overhead and hence there is a tradeoff between
lock contention and overhead. In general, lock contention increases with the
number of threads running concurrently. The `lcnt` tool does not measure locking
overhead.

## Enabling lock-counting

For investigation of locks in the emulator we use an internal tool called `lcnt`
(short for lock-count). The VM needs to be compiled with this option enabled. To
compile a lock-counting VM along with a normal VM, use:

```text
cd $ERL_TOP
./configure --enable-lock-counter
```

Start the lock-counting VM like this:

```text
$ERL_TOP/bin/erl -emu_type lcnt
```

To verify that lock counting is enabled check that `[lock-counting]` appears in
the status text when the VM is started.

```erlang
Erlang/OTP 20 [erts-9.0] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe]
 [kernel-poll:false] [lock-counting]
```

## Getting started

Once you have a lock counting enabled VM the module `lcnt` can be used. The
module is intended to be used from the current running nodes shell. To access
remote nodes use `lcnt:clear(Node)` and `lcnt:collect(Node)`.

All locks are continuously monitored and its statistics updated. Use
`lcnt:clear/0` to initially clear all counters before running any specific
tests. This command will also reset the duration timer internally.

To retrieve lock statistics information, use `lcnt:collect/0,1`. The collect
operation will start a `lcnt` server if it not already started. All collected
data will be built into an Erlang term and uploaded to the server and a duration
time will also be uploaded. This duration is the time between `lcnt:clear/0,1`
and `lcnt:collect/0,1`.

Once the data is collected to the server it can be filtered, sorted and printed
in many different ways.

See the [reference manual](`m:lcnt`) for a description of each function.

## Example of usage

From the Erlang shell:

```text
Erlang R13B03 (erts-5.7.4) [source] [smp:8:8] [rq:8] [async-threads:0] [hipe]
 [kernel-poll:false] [lock-counting]
1> lcnt:rt_opt({copy_save, true}).
false
2> lcnt:clear(), big:bang(1000), lcnt:collect().
ok
3> lcnt:conflicts().
                   lock   id  #tries  #collisions  collisions [%]  time [us]  duration [%]
                  -----  --- ------- ------------ --------------- ---------- -------------
         alcu_allocator   50 4113692       158921          3.8632     215464        4.4962
               pix_lock  256 4007140         4882          0.1218      12221        0.2550
              run_queue    8 2287246         6949          0.3038       9825        0.2050
              proc_main 1029 3115778        25755          0.8266       1199        0.0250
              proc_msgq 1029 2467022         1910          0.0774       1048        0.0219
            proc_status 1029 5708439         2435          0.0427        706        0.0147
 message_pre_alloc_lock    8 2008569          134          0.0067         90        0.0019
              timeofday    1   54065            8          0.0148         22        0.0005
                gc_info    1    7071            7          0.0990          5        0.0001
ok
```

Another way to to profile a specific function is to use `lcnt:apply/3` or
`lcnt:apply/1` which does `lcnt:clear/0` before the function and
`lcnt:collect/0` after its invocation. This method should only be used in
micro-benchmarks since it sets `copy_save` to `true` for the duration of the
function call, which may cause the emulator to run out of memory if attempted
under load.

```text
Erlang R13B03 (erts-5.7.4) [source] [smp:8:8] [rq:8] [async-threads:0] [hipe]
 [kernel-poll:false] [lock-counting]
1> lcnt:apply(fun() -> big:bang(1000) end).
4384.338
2> lcnt:conflicts().
                   lock   id  #tries  #collisions  collisions [%]  time [us]  duration [%]
                  -----  --- ------- ------------ --------------- ---------- -------------
         alcu_allocator   50 4117913       183091          4.4462     234232        5.1490
              run_queue    8 2050398         3801          0.1854       6700        0.1473
               pix_lock  256 4007080         4943          0.1234       2847        0.0626
              proc_main 1028 3000178        28247          0.9415       1022        0.0225
              proc_msgq 1028 2293677         1352          0.0589        545        0.0120
            proc_status 1028 5258029         1744          0.0332        442        0.0097
 message_pre_alloc_lock    8 2009322          147          0.0073         82        0.0018
              timeofday    1   48616            9          0.0185         13        0.0003
                gc_info    1    7455           12          0.1610          9        0.0002
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
         alcu_allocator 4117913          4.4462     234232
              run_queue 2050398          0.1854       6700
               pix_lock 4007080          0.1234       2847
 message_pre_alloc_lock 2009322          0.0073         82
  <nonode@nohost.660.0>   13493          1.4452         41
  <nonode@nohost.724.0>   13504          1.1404         36
  <nonode@nohost.803.0>   13181          1.6235         35
  <nonode@nohost.791.0>   13534          0.8202         22
   <nonode@nohost.37.0>    8744          5.8326         22
  <nonode@nohost.876.0>   13335          1.1174         19
  <nonode@nohost.637.0>   13452          1.3678         19
  <nonode@nohost.799.0>   13497          1.8745         18
  <nonode@nohost.469.0>   11009          2.5343         18
  <nonode@nohost.862.0>   13131          1.2566         16
  <nonode@nohost.642.0>   13216          1.7327         15
  <nonode@nohost.582.0>   13156          1.1098         15
  <nonode@nohost.622.0>   13420          0.7303         14
  <nonode@nohost.596.0>   13141          1.6437         14
  <nonode@nohost.592.0>   13346          1.2064         13
  <nonode@nohost.526.0>   13076          1.1701         13
ok
```

## Example with Mnesia Transaction Benchmark

From the Erlang shell:

```erlang
Erlang R13B03 (erts-5.7.4) [source] [smp:8:8] [rq:8] [async-threads:0] [hipe]
 [kernel-poll:false] [lock-counting]

Eshell V5.7.4  (abort with ^G)
1> Conf=[{db_nodes, [node()]}, {driver_nodes, [node()]}, {replica_nodes, [node()]},
 {n_drivers_per_node, 10}, {n_branches, 1000}, {n_accounts_per_branch, 10},
 {replica_type, ram_copies}, {stop_after, 60000}, {reuse_history_id, true}].
[{db_nodes,[nonode@nohost]},
 {driver_nodes,[nonode@nohost]},
 {replica_nodes,[nonode@nohost]},
 {n_drivers_per_node,10},
 {n_branches,1000},
 {n_accounts_per_branch,10},
 {replica_type,ram_copies},
 {stop_after,60000},
 {reuse_history_id,true}]
2> mnesia_tpcb:init([{use_running_mnesia, false}|Conf]).
ignore
```

Initial configuring of the benchmark is done. It is time to profile the actual
benchmark and Mnesia

```erlang
3> lcnt:apply(fun() -> {ok,{time, Tps,_,_,_,_}} = mnesia_tpcb:run([{use_running_mnesia,
 true}|Conf]), Tps/60 end).
12037.483333333334
ok
4> lcnt:swap_pid_keys().
ok
```

The `id` header represents the number of unique identifiers under a class when
the option `{combine, true}` is used (which is on by default). It will otherwise
show the specific identifier. The `db_tab` listing shows 722287 unique locks, it
is one for each ets-table created and Mnesia creates one for each transaction.

```text
5> lcnt:conflicts().
                   lock     id   #tries  #collisions  collisions [%]  time [us]  duration [%]
                  -----    ---  ------- ------------ --------------- ---------- -------------
         alcu_allocator     50 56355118       732662          1.3001    2934747        4.8862
                 db_tab 722287 94513441        63203          0.0669    1958797        3.2613
              timeofday      1  2701048       175854          6.5106    1746079        2.9071
               pix_lock    256 24306168       163214          0.6715     918309        1.5289
              run_queue      8 11813811       152637          1.2920     357040        0.5945
 message_pre_alloc_lock      8 17671449        57203          0.3237     263043        0.4380
          mnesia_locker      4 17477633      1618548          9.2607      97092        0.1617
              mnesia_tm      4  9891408       463788          4.6888      86353        0.1438
                gc_info      1   823460          628          0.0763      24826        0.0413
     meta_main_tab_slot     16 41393400         7193          0.0174      11393        0.0190
 <nonode@nohost.1108.0>      4  4331412          333          0.0077       7148        0.0119
            timer_wheel      1   203185           30          0.0148       3108        0.0052
 <nonode@nohost.1110.0>      4  4291098          210          0.0049        885        0.0015
 <nonode@nohost.1114.0>      4  4294702          288          0.0067        442        0.0007
 <nonode@nohost.1113.0>      4  4346066          235          0.0054        390        0.0006
 <nonode@nohost.1106.0>      4  4348159          287          0.0066        379        0.0006
 <nonode@nohost.1111.0>      4  4279309          290          0.0068        325        0.0005
 <nonode@nohost.1107.0>      4  4292190          302          0.0070        315        0.0005
 <nonode@nohost.1112.0>      4  4208858          265          0.0063        276        0.0005
 <nonode@nohost.1109.0>      4  4377502          267          0.0061        276        0.0005
ok
```

The listing shows `mnesia_locker`, a process, has highly contended locks.

```text
6> lcnt:inspect(mnesia_locker).
          lock          id  #tries  #collisions  collisions [%]  time [us]  duration [%]
         -----         --- ------- ------------ --------------- ---------- -------------
 mnesia_locker   proc_msgq 5449930        59374          1.0894      69781        0.1162
 mnesia_locker   proc_main 4462782      1487374         33.3284      14398        0.0240
 mnesia_locker proc_status 7564921        71800          0.9491      12913        0.0215
 mnesia_locker   proc_link       0            0          0.0000          0        0.0000
ok
```

Listing without class combiner.

```erlang
7> lcnt:conflicts([{combine, false}, {print, [name, id, tries, ratio, time]}]).
                   lock                        id   #tries  collisions [%]  time [us]
                  -----                       ---  ------- --------------- ----------
                 db_tab mnesia_transient_decision   722250          3.9463    1856852
              timeofday                 undefined  2701048          6.5106    1746079
         alcu_allocator                 ets_alloc  7490696          2.2737     692655
         alcu_allocator                 ets_alloc  7081771          2.3294     664522
         alcu_allocator                 ets_alloc  7047750          2.2520     658495
         alcu_allocator                 ets_alloc  5883537          2.3177     610869
               pix_lock                        58 11011355          1.1924     564808
               pix_lock                        60  4426484          0.7120     262490
         alcu_allocator                 ets_alloc  1897004          2.4248     219543
 message_pre_alloc_lock                 undefined  4211267          0.3242     128299
              run_queue                         3  2801555          1.3003     116792
              run_queue                         2  2799988          1.2700     100091
              run_queue                         1  2966183          1.2712      78834
          mnesia_locker                 proc_msgq  5449930          1.0894      69781
 message_pre_alloc_lock                 undefined  3495672          0.3262      65773
 message_pre_alloc_lock                 undefined  4189752          0.3174      58607
              mnesia_tm                 proc_msgq  2094144          1.7184      56361
              run_queue                         4  2343585          1.3115      44300
                 db_tab                    branch  1446529          0.5229      38244
                gc_info                 undefined   823460          0.0763      24826
ok
```

In this scenario the lock that protects ets-table `mnesia_transient_decision`
has spent most of its waiting for. That is 1.8 seconds in a test that run for 60
seconds. The time is also spread on eight different scheduler threads.

```text
8> lcnt:inspect(db_tab, [{print, [name, id, tries, colls, ratio, duration]}]).
   lock                        id  #tries  #collisions  collisions [%]  duration [%]
  -----                       --- ------- ------------ --------------- -------------
 db_tab mnesia_transient_decision  722250        28502          3.9463        3.0916
 db_tab                    branch 1446529         7564          0.5229        0.0637
 db_tab                   account 1464500         8203          0.5601        0.0357
 db_tab                    teller 1464529         8110          0.5538        0.0291
 db_tab                   history  722250         3767          0.5216        0.0232
 db_tab              mnesia_stats  750332         7057          0.9405        0.0180
 db_tab        mnesia_trans_store      61            0          0.0000        0.0000
 db_tab        mnesia_trans_store      61            0          0.0000        0.0000
 db_tab        mnesia_trans_store      53            0          0.0000        0.0000
 db_tab        mnesia_trans_store      53            0          0.0000        0.0000
 db_tab        mnesia_trans_store      53            0          0.0000        0.0000
 db_tab        mnesia_trans_store      53            0          0.0000        0.0000
 db_tab        mnesia_trans_store      53            0          0.0000        0.0000
 db_tab        mnesia_trans_store      53            0          0.0000        0.0000
 db_tab        mnesia_trans_store      53            0          0.0000        0.0000
 db_tab        mnesia_trans_store      53            0          0.0000        0.0000
 db_tab        mnesia_trans_store      53            0          0.0000        0.0000
 db_tab        mnesia_trans_store      53            0          0.0000        0.0000
 db_tab        mnesia_trans_store      53            0          0.0000        0.0000
 db_tab        mnesia_trans_store      53            0          0.0000        0.0000
ok
```

## Deciphering the output

Typically high `time` values are bad and this is often the thing to look for.
However, one should also look for high lock acquisition frequencies (#tries)
since locks generate overhead and because high frequency could become
problematic if they begin to have conflicts even if it is not shown in a
particular test.

## See Also

[LCNT Reference Manual](`m:lcnt`)
