Returns information about the current system.

The documentation of this function is broken into the following sections in
order to make it easier to navigate.

- [`Memory Allocation`](`m:erlang#system_info/1-memory-allocation`) -
  [`allocated_areas`](`m:erlang#system_info_allocated_areas`),
  [`allocator`](`m:erlang#system_info_allocator`),
  [`alloc_util_allocators`](`m:erlang#system_info_alloc_util_allocators`),
  [`allocator_sizes`](`m:erlang#system_info_allocator_sizes`)

- [`CPU Topology`](`m:erlang#system_info/1-cpu-topology`) -
  [`cpu_topology`](`m:erlang#system_info_cpu_topology`),
  [`logical_processors`](`m:erlang#system_info_logical_processors`),
  [`update_cpu_info`](`m:erlang#system_info_update_cpu_info`)

- [`Process Information`](`m:erlang#system_info/1-process-information`) -
  [`fullsweep_after`](`m:erlang#system_info_fullsweep_after`),
  [`garbage_collection`](`m:erlang#system_info_garbage_collection`),
  [`heap_sizes`](`m:erlang#system_info_heap_sizes`),
  [`heap_type`](`m:erlang#system_info_heap_type`),
  [`max_heap_size`](`m:erlang#system_info_max_heap_size`),
  [`message_queue_data`](`m:erlang#system_info_message_queue_data`),
  [`min_heap_size`](`m:erlang#system_info_min_heap_size`),
  [`min_bin_vheap_size`](`m:erlang#system_info_min_bin_vheap_size`),
  [`procs`](`m:erlang#system_info_procs`)

- [`System Limits`](`m:erlang#system_info/1-system-limits`) -
  [`atom_count`](`m:erlang#system_info_atom_count`),
  [`atom_limit`](`m:erlang#system_info_atom_limit`),
  [`ets_count`](`m:erlang#system_info_ets_count`),
  [`ets_limit`](`m:erlang#system_info_ets_limit`),
  [`port_count`](`m:erlang#system_info_port_count`),
  [`port_limit`](`m:erlang#system_info_port_limit`),
  [`process_count`](`m:erlang#system_info_process_count`),
  [`process_limit`](`m:erlang#system_info_process_limit`)

- [`System Time`](`m:erlang#system_info/1-system-time`) -
  [`end_time`](`m:erlang#system_info_end_time`),
  [`os_monotonic_time_source`](`m:erlang#system_info_os_monotonic_time_source`),
  [`os_system_time_source`](`m:erlang#system_info_os_system_time_source`),
  [`start_time`](`m:erlang#system_info_start_time`),
  [`time_correction`](`m:erlang#system_info_time_correction`),
  [`time_offset`](`m:erlang#system_info_time_offset`),
  [`time_warp_mode`](`m:erlang#system_info_time_warp_mode`),
  [`tolerant_timeofday`](`m:erlang#system_info_tolerant_timeofday`)

- [`Scheduler Information`](`m:erlang#system_info/1-scheduler-information`) -
  [`dirty_cpu_schedulers`](`m:erlang#system_info_dirty_cpu_schedulers`),
  [`dirty_cpu_schedulers_online`](`m:erlang#system_info_dirty_cpu_schedulers_online`),
  [`dirty_io_schedulers`](`m:erlang#system_info_dirty_io_schedulers`),
  [`multi_scheduling`](`m:erlang#system_info_multi_scheduling`),
  [`multi_scheduling_blockers`](`m:erlang#system_info_multi_scheduling_blockers`),
  [`normal_multi_scheduling_blockers`](`m:erlang#system_info_normal_multi_scheduling_blockers`),
  [`scheduler_bind_type`](`m:erlang#system_info_scheduler_bind_type`),
  [`scheduler_bindings`](`m:erlang#system_info_scheduler_bindings`),
  [`scheduler_id`](`m:erlang#system_info_scheduler_id`),
  [`schedulers`](`m:erlang#system_info_schedulers`),
  [`smp_support`](`m:erlang#system_info_smp_support`),
  [`threads`](`m:erlang#system_info_threads`),
  [`thread_pool_size`](`m:erlang#system_info_thread_pool_size`)

- [`Distribution Information`](`m:erlang#system_info/1-distribution-information`) -
  [`creation`](`m:erlang#system_info_creation`),
  [`delayed_node_table_gc`](`m:erlang#system_info_delayed_node_table_gc`),
  [`dist`](`m:erlang#system_info_dist`),
  [`dist_buf_busy_limit`](`m:erlang#system_info_dist_buf_busy_limit`),
  [`dist_ctrl`](`m:erlang#system_info_dist_ctrl`)

- [`System Information`](`m:erlang#system_info/1-system-information`) -
  [`c_compiler_used`](`m:erlang#system_info_c_compiler_used`),
  [`check_io`](`m:erlang#system_info_check_io`),
  [`debug_compiled`](`m:erlang#system_info_debug_compiled`),
  [`driver_version`](`m:erlang#system_info_driver_version`),
  [`dynamic_trace`](`m:erlang#system_info_dynamic_trace`),
  [`dynamic_trace_probes`](`m:erlang#system_info_dynamic_trace_probes`),
  [`emu_flavor`](`m:erlang#system_info_emu_flavor`),
  [`emu_type`](`m:erlang#system_info_emu_type`),
  [`info`](`m:erlang#system_info_info`),
  [`kernel_poll`](`m:erlang#system_info_kernel_poll`),
  [`loaded`](`m:erlang#system_info_loaded`),
  [`machine`](`m:erlang#system_info_machine`),
  [`modified_timing_level`](`m:erlang#system_info_modified_timing_level`),
  [`nif_version`](`m:erlang#system_info_nif_version`),
  [`otp_release`](`m:erlang#system_info_otp_release`),
  [`outstanding_system_requests_limit`](`m:erlang#system_info_outstanding_system_requests_limit`),
  [`port_parallelism`](`m:erlang#system_info_port_parallelism`),
  [`system_architecture`](`m:erlang#system_info_system_architecture`),
  [`system_logger`](`m:erlang#system_info_system_logger`),
  [`system_version`](`m:erlang#system_info_system_version`),
  [`trace_control_word`](`m:erlang#system_info_trace_control_word`),
  [`version`](`m:erlang#system_info_version`),
  [`wordsize`](`m:erlang#system_info_wordsize`)

## Memory Allocation

Returns various information about the memory allocators of the current system (emulator) as specified by `Item`:

* __`allocated_areas`__{: #system_info_allocated_areas } - Returns `[tuple()]` with
  information about miscellaneous allocated memory areas.

  Each tuple contains an atom describing the type of memory as first element and
  the amount of allocated memory in bytes as second element. When information
  about allocated and used memory is present, also a third element is present,
  containing the amount of used memory in bytes.

  `erlang:system_info(allocated_areas)` is intended for debugging, and the content
  is highly implementation-dependent. The content of the results therefore
  changes when needed without prior notice.

  Notice that the sum of these values is _not_ the total amount of memory
  allocated by the emulator. Some values are part of other values, and some
  memory areas are not part of the result. For information about the total amount
  of memory allocated by the emulator, see
  [`erlang:memory/0,1`](`erlang:memory/0`).

- `allocator`{: #system_info_allocator } - Returns
  ```
  {Allocator :: undefined | glibc,
   Version :: [non_neg_integer()],
   Features :: [atom()],
   Settings :: [{Subsystem :: atom(),
                 [{Parameter :: atom(),
                   Value :: term()}]
                 }]
  }
  ```

  where

  - `Allocator` corresponds to the `malloc()` implementation used. If
    `Allocator` equals `undefined`, the `malloc()` implementation used cannot be
    identified. `glibc` can be identified.
  - `Version` is a list of integers (but not a string) representing the
    version of the `malloc()` implementation used.
  - `Features` is a list of atoms representing the allocation features used.
  - `Settings` is a list of subsystems, their configurable parameters, and used
    values. Settings can differ between different combinations of platforms,
    allocators, and allocation features. Memory sizes are given in bytes.

  See also "System Flags Effecting erts_alloc" in
  [`erts_alloc(3)`](erts_alloc.md#flags).

- `{allocator, Alloc}`{: #system_info_allocator_tuple } - Returns
  information about the specified allocator. As from ERTS 5.6.1, the return
  value is a list of `{instance, InstanceNo, InstanceInfo}` tuples, where
  `InstanceInfo` contains information about a specific instance of the
  allocator. If `Alloc` is not a recognized allocator, `undefined` is
  returned. If `Alloc` is disabled, `false` is returned.

  Notice that the information returned is highly implementation-dependent and
  can be changed or removed at any time without prior notice. It was initially
  intended as a tool when developing new allocators, but as it can be of
  interest for others it has been briefly documented.

  The recognized allocators are listed in [`erts_alloc(3)`](erts_alloc.md).
  Information about super carriers can be obtained from ERTS 8.0 with
  `{allocator, erts_mmap}` or from ERTS 5.10.4; the returned list when calling
  with `{allocator, mseg_alloc}` also includes an `{erts_mmap, _}` tuple as one
  element in the list.

  After reading the `erts_alloc(3)` documentation, the returned information more
  or less speaks for itself, but it can be worth explaining some things. Call
  counts are presented by two values, the first value is giga calls, and the
  second value is calls. `mbcs` and `sbcs` denote multi-block carriers, and
  single-block carriers, respectively. Sizes are presented in bytes. When a
  size is not presented, it is the amount of something. Sizes and amounts are
  often presented by three values:

  - The first is the current value.
  - The second is the maximum value since the last call to
    `erlang:system_info({allocator, Alloc})`.
  - The third is the maximum value since the emulator was started.

  If only one value is present, it is the current value. `fix_alloc` memory
  block types are presented by two values. The first value is the memory pool
  size and the second value is the used memory size.

- `alloc_util_allocators`{: #system_info_alloc_util_allocators } - Returns a
  list of the names of all allocators using the ERTS internal `alloc_util`
  framework as atoms. For more information, see section
  [The alloc_util framework](erts_alloc.md#alloc_util) in `erts_alloc(3)`.

- `{allocator_sizes, Alloc}`{: #system_info_allocator_sizes } - Returns
  various size information for the specified allocator. The information
  returned is a subset of the information returned by
  [`erlang:system_info({allocator, Alloc})`](`m:erlang#system_info_allocator_tuple`).

## CPU Topology

Returns various information about the CPU topology of the current system (emulator) as specified by `Item`:

- `cpu_topology`{: #system_info_cpu_topology } - Returns the `t:cpu_topology()`
  currently used by the emulator. The CPU topology is used when binding
  schedulers to logical processors. The CPU topology used is the
  [user-defined CPU topology](`m:erlang#system_info_cpu_topology_defined`), if
  such exists, otherwise the
  [automatically detected CPU topology](`m:erlang#system_info_cpu_topology_detected`),
  if such exists. If no CPU topology exists, `undefined` is returned.

- `{cpu_topology, defined}`{: #system_info_cpu_topology_defined } - Returns
  the user-defined `t:cpu_topology()`. For more information, see command-line flag
  [`+sct`](erl_cmd.md#+sct) in `erl(1)` and argument
  [`cpu_topology`](`m:erlang#system_info_cpu_topology`).

- `{cpu_topology, detected}`{: #system_info_cpu_topology_detected } -
  Returns the automatically detected `t:cpu_topology()`. The emulator detects the
  CPU topology on some newer Linux, Solaris, FreeBSD, and Windows systems. On
  Windows system with more than 32 logical processors, the CPU topology is not
  detected.

  For more information, see argument
  [`cpu_topology`](`m:erlang#system_info_cpu_topology`).

- `{cpu_topology, used}` - Returns `CpuTopology` used by the emulator. For
  more information, see argument
  [`cpu_topology`](`m:erlang#system_info_cpu_topology`).

- `logical_processors`{: #system_info_logical_processors } - Returns the
  detected number of logical processors configured in the system. The return
  value is either an integer, or the atom `unknown` if the emulator cannot
  detect the configured logical processors.

- `logical_processors_available`{: #system_info_logical_processors_available
  } - Returns the detected number of logical processors available to the Erlang
  runtime system. The return value is either an integer, or the atom `unknown`
  if the emulator cannot detect the available logical processors. The number of
  available logical processors is less than or equal to the number of
  [logical processors online](`m:erlang#system_info_logical_processors_online`).

- `logical_processors_online`{: #system_info_logical_processors_online } -
  Returns the detected number of logical processors online on the system. The
  return value is either an integer, or the atom `unknown` if the emulator
  cannot detect logical processors online. The number of logical processors
  online is less than or equal to the number of
  [logical processors configured](`m:erlang#system_info_logical_processors`).

- `cpu_quota`{: #system_info_cpu_quota } - Returns the detected CPU quota
  the emulator is limited by. The return value is an integer saying how many
  processors' worth of runtime we get (between 1 and the number of logical
  processors), or the atom `unknown` if the emulator cannot detect a quota.

- `update_cpu_info`{: #system_info_update_cpu_info } - The runtime system
  rereads the CPU information available and updates its internally stored
  information about the
  [detected CPU topology](`m:erlang#system_info_cpu_topology_detected`) and the
  number of logical processors
  [configured](`m:erlang#system_info_logical_processors`),
  [online](`m:erlang#system_info_logical_processors_online`),
  [available](`m:erlang#system_info_logical_processors_available`), and
  [cpu quota](`m:erlang#system_info_cpu_quota`).

  If the CPU information has changed since the last time it was read, the atom
  `changed` is returned, otherwise the atom `unchanged`. If the CPU information
  has changed, you probably want to
  [adjust the number of schedulers online](`m:erlang#system_flag_schedulers_online`).
  You typically want to have as many schedulers online as
  [logical processors available](`m:erlang#system_info_logical_processors_available`).
  
  Since: OTP R14B

## Process Information

Returns information about the default process heap settings:

- `fullsweep_after`{: #system_info_fullsweep_after } - Returns
  `{fullsweep_after, integer() >= 0}`, which is the `fullsweep_after` garbage
  collection setting used by default. For more information, see
  `garbage_collection` described below.

- `garbage_collection`{: #system_info_garbage_collection } - Returns
  `t:garbage_collection_defaults/0` describing the default garbage collection settings.
  A process spawned on the local node by a `spawn` or `spawn_link` uses these
  garbage collection settings. The default settings can be changed by using
  [`erlang:system_flag/2`](`erlang:system_flag/2`).
  [`spawn_opt/2,3,4`](`erlang:spawn_opt/4`) can spawn a process that does not
  use the default settings.

- `heap_sizes`{: #system_info_heap_sizes } - Returns a list of integers
  representing valid heap sizes in words. All Erlang heaps are sized from sizes
  in this list.

- `heap_type`{: #system_info_heap_type } - Returns the heap type used by the
  current emulator. One heap type exists:

  - `private` - Each process has a heap reserved for its use and no
    references between heaps of different processes are allowed. Messages
    passed between processes are copied between heaps.

- `max_heap_size`{: #system_info_max_heap_size } - Returns
  `{max_heap_size, MaxHeapSize}`, where `MaxHeapSize` is the current
  system-wide maximum heap size settings for spawned processes. This setting
  can be set using the command-line flags [`+hmax`](erl_cmd.md#+hmax),
  [`+hmaxk`](erl_cmd.md#+hmaxk), [`+hmaxel`](erl_cmd.md#+hmaxel) and
  [`+hmaxibl`](erl_cmd.md#+hmaxib) in `erl(1)`. It can also be changed at runtime
  using
  [`erlang:system_flag(max_heap_size, MaxHeapSize)`](`m:erlang#system_flag_max_heap_size`).
  For more details about the `max_heap_size` process flag, see
  [`process_flag(max_heap_size, MaxHeapSize)`](`m:erlang#process_flag_max_heap_size`).
  
  Since: OTP 19.0

- `message_queue_data`{: #system_info_message_queue_data } - Returns the
  default value of the `message_queue_data` process flag, which can be either
  `off_heap` or `on_heap`. The default value is set by the command-line
  argument [`+hmqd`](erl_cmd.md#+hmqd) in `erl(1)`. For more information, see the
  documentation of
  [`process_flag(message_queue_data, MQD)`](`m:erlang#process_flag_message_queue_data`).
  
  Since: OTP 19.0

- `min_heap_size`{: #system_info_min_heap_size } - Returns
  `{min_heap_size, MinHeapSize}`, where `MinHeapSize` is the current
  system-wide minimum heap size for spawned processes.
  
  Since: OTP R13B04

- `min_bin_vheap_size`{: #system_info_min_bin_vheap_size } - Returns
  `{min_bin_vheap_size, MinBinVHeapSize}`, where `MinBinVHeapSize` is the
  current system-wide minimum binary virtual heap size for spawned processes.
  
  Since: OTP R13B04

- `procs`{: #system_info_procs } - Returns a binary containing a string of
  process and port information formatted as in Erlang crash dumps. For more
  information, see section
  [How to interpret the Erlang crash dumps](crash_dump.md) in the User's Guide.

## System Limits

Returns information about the current system (emulator) limits as specified by `Item`:

- `atom_count`{: #system_info_atom_count } - Returns the number of atoms
  currently existing at the local node. The value is given as an integer.
  
  Since: OTP 20.0

- `atom_limit`{: #system_info_atom_limit } - Returns the maximum number of
  atoms allowed. This limit can be increased at startup by passing
  command-line flag [`+t`](erl_cmd.md#+t_size) to `erl(1)`.
  
  Since: OTP 20.0

- `ets_count`{: #system_info_ets_count } - Returns the number of ETS tables
  currently existing at the local node.
  
  Since: OTP 21.1

- `ets_limit`{: #system_info_ets_limit } - Returns the limit for number of
  ETS tables. This limit is [partially obsolete](`m:ets#max_ets_tables`) and
  number of tables are only limited by available memory.
  
  Since: OTP R16B03

- `port_count`{: #system_info_port_count } - Returns the number of ports
  currently existing at the local node. The value is given as an integer. This
  is the same value as returned by `length(erlang:ports())`, but more
  efficient.
  
  Since: OTP R16B

- `port_limit`{: #system_info_port_limit } - Returns the maximum number of
  simultaneously existing ports at the local node as an integer. This limit can
  be configured at startup by using command-line flag [`+Q`](erl_cmd.md#+Q) in
  `erl(1)`.
  
  Since OTP R16B

- `process_count`{: #system_info_process_count } - Returns the number of
  processes currently existing at the local node. The value is given as an
  integer. This is the same value as returned by `length(processes())`, but
  more efficient.

- `process_limit`{: #system_info_process_limit } - Returns the maximum
  number of simultaneously existing processes at the local node. The value is
  given as an integer. This limit can be configured at startup by using
  command-line flag [`+P`](erl_cmd.md#+P) in `erl(1)`.

## System Time

Returns information about the current system (emulator) time as specified by `Item`:

- `end_time`{: #system_info_end_time } - The last
  [Erlang monotonic time](`erlang:monotonic_time/0`) in `native`
  [time unit](`t:time_unit/0`) that can be represented internally in
  the current Erlang runtime system instance. The time between the
  [start time](`m:erlang#system_info_start_time`) and the end time is at least a
  quarter of a millennium.
  
  Since: OTP 18.0

- `os_monotonic_time_source`{: #system_info_os_monotonic_time_source } -
  Returns a list containing information about the source of
  [OS monotonic time](time_correction.md#os-monotonic-time) that is used by the
  runtime system.

  If `[]` is returned, no OS monotonic time is available. The list contains
  two-tuples with `Key`s as first element, and `Value`s as second element. The
  order of these tuples is undefined. The following tuples can be part of the
  list, but more tuples can be introduced in the future:

  - `{function, Function}` - `Function` is the name of the function used.
    This tuple always exists if OS monotonic time is available to the runtime
    system.

  - `{clock_id, ClockId}` - This tuple only exists if `Function` can be used
    with different clocks. `ClockId` corresponds to the clock identifier used
    when calling `Function`.

  - `{resolution, OsMonotonicTimeResolution}` - Highest possible
    [resolution](time_correction.md#time-resolution) of current OS monotonic
    time source as parts per second. If no resolution information can be
    retrieved from the OS, `OsMonotonicTimeResolution` is set to the resolution
    of the time unit of `Function`s return value. That is, the actual
    resolution can be lower than `OsMonotonicTimeResolution`. Notice that the
    resolution does not say anything about the
    [accuracy](time_correction.md#time-accuracy) or whether the
    [precision](time_correction.md#time-precision) aligns with the resolution.
    You do, however, know that the precision is not better than
    `OsMonotonicTimeResolution`.

  - `{used_resolution, UsedOsMonotonicTimeResolution}` - The OS monotonic time
    resolution used by the runtime system. This is very often the same as
    `OsMonotonicTimeResolution`. However, on some systems the resolution has to
    be reduced in order to reliably produce monotonic timestamps. An example of
    this is when `QueryPerformanceCounter()` is used as OS monotonic time
    source on Windows. If such a reduction of the resolution has been done,
    `UsedOsMonotonicTimeResolution` will be smaller than
    `OsMonotonicTimeResolution`.

  - `{extended, Extended}` - `Extended` equals `yes` if the range of time
    values has been extended; otherwise `Extended` equals `no`. The range must
    be extended if `Function` returns values that wrap fast. This typically is
    the case when the return value is a 32-bit value.

  - `{parallel, Parallel}` - `Parallel` equals `yes` if `Function` is called
    in parallel from multiple threads. If it is not called in parallel, because
    calls must be serialized, `Parallel` equals `no`.

  - `{time, OsMonotonicTime}` - `OsMonotonicTime` equals current OS
    monotonic time in `native` [time unit](`t:time_unit/0`).
  
  Since: OTP 18.0

- `os_system_time_source`{: #system_info_os_system_time_source } - Returns a
  list containing information about the source of
  [OS system time](time_correction.md#os-system-time) that is used by the
  runtime system.

  The list contains two-tuples with `Key`s as first element, and `Value`s as
  second element. The order of these tuples is undefined. The following tuples
  can be part of the list, but more tuples can be introduced in the future:

  - `{function, Function}` - `Function` is the name of the function used.

  - `{clock_id, ClockId}` - Exists only if `Function` can be used with
    different clocks. `ClockId` corresponds to the clock identifier used when
    calling `Function`.

  - `{resolution, OsSystemTimeResolution}` - Highest possible
    [resolution](time_correction.md#time-resolution) of current OS system time
    source as parts per second. If no resolution information can be retrieved
    from the OS, `OsSystemTimeResolution` is set to the resolution of the time
    unit of `Function`s return value. That is, the actual resolution can be
    lower than `OsSystemTimeResolution`. Notice that the resolution does not
    say anything about the [accuracy](time_correction.md#time-accuracy) or
    whether the [precision](time_correction.md#time-precision) do align with the
    resolution. You do, however, know that the precision is not better than
    `OsSystemTimeResolution`.

  - `{parallel, Parallel}` - `Parallel` equals `yes` if `Function` is called
    in parallel from multiple threads. If it is not called in parallel, because
    calls needs to be serialized, `Parallel` equals `no`.

  - `{time, OsSystemTime}` - `OsSystemTime` equals current OS system time in
    `native` [time unit](`t:time_unit/0`).
  
  Since: OTP 18.0

- `start_time`{: #system_info_start_time } - The
  [Erlang monotonic time](`erlang:monotonic_time/0`) in `native`
  [time unit](`t:time_unit/0`) at the time when current Erlang runtime
  system instance started.

  See also [`erlang:system_info(end_time)`](`m:erlang#system_info_end_time`).
  
  Since: OTP 18.0

- `time_correction`{: #system_info_time_correction } - Returns a `t:boolean()`
  value indicating whether [time correction](time_correction.md#time-correction)
  is enabled or not.
  
  Since: OTP 18.0

- `time_offset`{: #system_info_time_offset } - Returns the state of the time
  offset:

  - `preliminary` - The time offset is preliminary, and will be changed and
    finalized later. The preliminary time offset is used during the preliminary
    phase of the
    [single time warp mode](time_correction.md#single-time-warp-mode).

  - `final` - The time offset is final. This either because
    [no time warp mode](time_correction.md#no-time-warp-mode) is used, or
    because the time offset have been finalized when
    [single time warp mode](time_correction.md#single-time-warp-mode) is used.

  - `volatile` - The time offset is volatile. That is, it can change at any
    time. This is because
    [multi-time warp mode](time_correction.md#multi-time-warp-mode) is used.
  
  Since: OTP 18.0

- `time_warp_mode`{: #system_info_time_warp_mode } - Returns a value
  identifying the [time warp mode](time_correction.md#time-warp-modes) that is
  used:

  - `no_time_warp` - The
    [no time warp mode](time_correction.md#no-time-warp-mode) is used.

  - `single_time_warp` - The
    [single time warp mode](time_correction.md#single-time-warp-mode) is used.

  - `multi_time_warp` - The
    [multi-time warp mode](time_correction.md#multi-time-warp-mode) is used.
  
  Since: OTP 18.0

- `tolerant_timeofday`{: #system_info_tolerant_timeofday } - Returns whether
  a pre ERTS 7.0 backwards compatible compensation for sudden changes of system
  time is `enabled` or `disabled`. Such compensation is `enabled` when the
  [time offset](`m:erlang#system_info_time_offset`) is `final`, and
  [time correction](`m:erlang#system_info_time_correction`) is enabled.
  
  Since: OTP 17.1

## Scheduler Information

Returns information about schedulers, scheduling and threads in the current system as specified by `Item`:

- `dirty_cpu_schedulers`{: #system_info_dirty_cpu_schedulers } - Returns the
  number of dirty CPU scheduler threads used by the emulator. Dirty CPU
  schedulers execute CPU-bound native functions, such as NIFs, linked-in
  driver code, and BIFs that cannot be managed cleanly by the normal emulator
  schedulers.

  The number of dirty CPU scheduler threads is determined at emulator boot time
  and cannot be changed after that. However, the number of dirty CPU scheduler
  threads online can be changed at any time. The number of dirty CPU schedulers
  can be set at startup by passing command-line flag [`+SDcpu`](erl_cmd.md#+SDcpu)
  or [`+SDPcpu`](erl_cmd.md#+SDPcpu) in `erl(1)`.

  See also
  [`erlang:system_flag(dirty_cpu_schedulers_online, DirtyCPUSchedulersOnline)`](`m:erlang#system_flag_dirty_cpu_schedulers_online`),
  [`erlang:system_info(dirty_cpu_schedulers_online)`](`m:erlang#system_info_dirty_cpu_schedulers_online`),
  [`erlang:system_info(dirty_io_schedulers)`](`m:erlang#system_info_dirty_io_schedulers`),
  [`erlang:system_info(schedulers)`](`m:erlang#system_info_schedulers`),
  [`erlang:system_info(schedulers_online)`](`m:erlang#system_info_schedulers_online`),
  and
  [`erlang:system_flag(schedulers_online, SchedulersOnline)`](`m:erlang#system_flag_schedulers_online`).
  
  Since: OTP 17.0

- `dirty_cpu_schedulers_online`{: #system_info_dirty_cpu_schedulers_online
  } - Returns the number of dirty CPU schedulers online. The return value
  satisfies `1 <= DirtyCPUSchedulersOnline <= N`, where `N` is the smallest of
  the return values of `erlang:system_info(dirty_cpu_schedulers)` and
  `erlang:system_info(schedulers_online)`.

  The number of dirty CPU schedulers online can be set at startup by passing
  command-line flag [`+SDcpu`](erl_cmd.md#+SDcpu) in `erl(1)`.

  For more information, see
  [`erlang:system_info(dirty_cpu_schedulers)`](`m:erlang#system_info_dirty_cpu_schedulers`),
  [`erlang:system_info(dirty_io_schedulers)`](`m:erlang#system_info_dirty_io_schedulers`),
  [`erlang:system_info(schedulers_online)`](`m:erlang#system_info_schedulers_online`),
  and
  [`erlang:system_flag(dirty_cpu_schedulers_online, DirtyCPUSchedulersOnline)`](`m:erlang#system_flag_dirty_cpu_schedulers_online`).
  
  Since: OTP 17.0

- `dirty_io_schedulers`{: #system_info_dirty_io_schedulers } - Returns the
  number of dirty I/O schedulers as an integer. Dirty I/O schedulers execute
  I/O-bound native functions, such as NIFs and linked-in driver code, which
  cannot be managed cleanly by the normal emulator schedulers.

  This value can be set at startup by passing command-line argument
  [`+SDio`](erl_cmd.md#+SDio) in `erl(1)`.

  For more information, see
  [`erlang:system_info(dirty_cpu_schedulers)`](`m:erlang#system_info_dirty_cpu_schedulers`),
  [`erlang:system_info(dirty_cpu_schedulers_online)`](`m:erlang#system_info_dirty_cpu_schedulers_online`),
  and
  [`erlang:system_flag(dirty_cpu_schedulers_online, DirtyCPUSchedulersOnline)`](`m:erlang#system_flag_dirty_cpu_schedulers_online`).
  
  Since: OTP 17.0

- `multi_scheduling`{: #system_info_multi_scheduling } - Returns one of the
  following:

  - `disabled` - The emulator has been started with only one scheduler
    thread.

  - `blocked` - The emulator has more than one scheduler thread, but all
    scheduler threads except one are blocked. That is, only one scheduler
    thread schedules Erlang processes and executes Erlang code.

  - `blocked_normal` - The emulator has more than one scheduler thread, but
    all normal scheduler threads except one are blocked. Notice that dirty
    schedulers are not blocked, and can schedule Erlang processes and execute
    native code.

  - `enabled` - The emulator has more than one scheduler thread, and no
    scheduler threads are blocked. That is, all available scheduler threads
    schedule Erlang processes and execute Erlang code.

  See also
  [`erlang:system_flag(multi_scheduling, BlockState)`](`m:erlang#system_flag_multi_scheduling`),
  [`erlang:system_info(multi_scheduling_blockers)`](`m:erlang#system_info_multi_scheduling_blockers`),
  [`erlang:system_info(normal_multi_scheduling_blockers)`](`m:erlang#system_info_normal_multi_scheduling_blockers`),
  and [`erlang:system_info(schedulers)`](`m:erlang#system_info_schedulers`).

- `multi_scheduling_blockers`{: #system_info_multi_scheduling_blockers } -
  Returns a list of `Pid`s when multi-scheduling is blocked, otherwise the
  empty list is returned. The `Pid`s in the list represent all the processes
  currently blocking multi-scheduling. A `Pid` occurs only once in the list,
  even if the corresponding process has blocked multiple times.

  See also
  [`erlang:system_flag(multi_scheduling, BlockState)`](`m:erlang#system_flag_multi_scheduling`),
  [`erlang:system_info(multi_scheduling)`](`m:erlang#system_info_multi_scheduling`),
  [`erlang:system_info(normal_multi_scheduling_blockers)`](`m:erlang#system_info_normal_multi_scheduling_blockers`),
  and [`erlang:system_info(schedulers)`](`m:erlang#system_info_schedulers`).

- `normal_multi_scheduling_blockers`{:
  #system_info_normal_multi_scheduling_blockers } - Returns a list of `Pid`s
  when normal multi-scheduling is blocked (that is, all normal schedulers but
  one is blocked), otherwise the empty list is returned. The `Pid`s in the
  list represent all the processes currently blocking normal multi-scheduling.
  A `Pid` occurs only once in the list, even if the corresponding process has
  blocked multiple times.

  See also
  [`erlang:system_flag(multi_scheduling, BlockState)`](`m:erlang#system_flag_multi_scheduling`),
  [`erlang:system_info(multi_scheduling)`](`m:erlang#system_info_multi_scheduling`),
  [`erlang:system_info(multi_scheduling_blockers)`](`m:erlang#system_info_multi_scheduling_blockers`),
  and [`erlang:system_info(schedulers)`](`m:erlang#system_info_schedulers`).
  
  Since: OTP 19.0

- `scheduler_bind_type`{: #system_info_scheduler_bind_type } - Returns
  `t:scheduler_bind_type()`, information about how the user has requested
  schedulers to be bound or not bound.

  Notice that although a user has requested schedulers to be bound, they can
  silently have failed to bind. To inspect the scheduler bindings, call
  [`erlang:system_info(scheduler_bindings)`](`m:erlang#system_info_scheduler_bindings`).

  For more information, see command-line argument [`+sbt`](erl_cmd.md#+sbt) in
  `erl(1)` and
  [`erlang:system_info(scheduler_bindings)`](`m:erlang#system_info_scheduler_bindings`).

- `scheduler_bindings`{: #system_info_scheduler_bindings } - Returns
  information about the currently used scheduler bindings.

  A tuple of a size equal to
  [`erlang:system_info(schedulers)`](`m:erlang#system_info_schedulers`) is
  returned. The tuple elements are integers or the atom `unbound`. Logical
  processor identifiers are represented as integers. The `N`th element of the
  tuple equals the current binding for the scheduler with the scheduler
  identifier equal to `N`. For example, if the schedulers are bound,
  `element(erlang:system_info(scheduler_id), erlang:system_info(scheduler_bindings))`
  returns the identifier of the logical processor that the calling process is
  executing on.

  Notice that only schedulers online can be bound to logical processors.

  For more information, see command-line argument [`+sbt`](erl_cmd.md#+sbt) in
  `erl(1)` and
  [`erlang:system_info(schedulers_online)`](`m:erlang#system_info_schedulers_online`).

- `scheduler_id`{: #system_info_scheduler_id } - Returns the scheduler ID
  (`SchedulerId`) of the scheduler thread that the calling process is
  executing on. `SchedulerId` is a positive integer, where
  `1 <= SchedulerId <= erlang:system_info(schedulers)`.

  See also
  [`erlang:system_info(schedulers)`](`m:erlang#system_info_schedulers`).

- `schedulers`{: #system_info_schedulers } - Returns the number of scheduler
  threads used by the emulator. Scheduler threads online schedules Erlang
  processes and Erlang ports, and execute Erlang code and Erlang linked-in
  driver code.

  The number of scheduler threads is determined at emulator boot time and cannot
  be changed later. However, the number of schedulers online can be changed at
  any time.

  See also
  [`erlang:system_flag(schedulers_online, SchedulersOnline)`](`m:erlang#system_flag_schedulers_online`),
  [`erlang:system_info(schedulers_online)`](`m:erlang#system_info_schedulers_online`),
  [`erlang:system_info(scheduler_id)`](`m:erlang#system_info_scheduler_id`),
  [`erlang:system_flag(multi_scheduling, BlockState)`](`m:erlang#system_flag_multi_scheduling`),
  [`erlang:system_info(multi_scheduling)`](`m:erlang#system_info_multi_scheduling`),
  [`erlang:system_info(normal_multi_scheduling_blockers)`](`m:erlang#system_info_normal_multi_scheduling_blockers`)
  and
  [`erlang:system_info(multi_scheduling_blockers)`](`m:erlang#system_info_multi_scheduling_blockers`).

- `schedulers_online`{: #system_info_schedulers_online } - Returns the
  number of schedulers online. The scheduler identifiers of schedulers online
  satisfy the relationship
  `1 <= SchedulerId <= erlang:system_info(schedulers_online)`.

  For more information, see
  [`erlang:system_info(schedulers)`](`m:erlang#system_info_schedulers`) and
  [`erlang:system_flag(schedulers_online, SchedulersOnline)`](`m:erlang#system_flag_schedulers_online`).

- `smp_support`{: #system_info_smp_support } - Returns `true`.

- `threads`{: #system_info_threads } - Returns `true`.

- `thread_pool_size`{: #system_info_thread_pool_size } - Returns the number of async threads in the
  async thread pool used for asynchronous driver calls
  ([ `erl_driver:driver_async()`](erl_driver.md#driver_async)). The value is
  given as an integer.

## Distribution Information

Returns information about Erlang Distribution in the current system as specified by `Item`:

- `async_dist`{: #system_info_async_dist } - Returns the value of the command line argument
  [+pad <boolean>](erl_cmd.md#+pad) which the runtime system use. This value
  determines the default [`async_dist`](`m:erlang#process_flag_async_dist`)
  value for newly spawned processes.
  
  Since: OTP 25.3

- `creation`{: #system_info_creation } - Returns the "creation" value of the
  local node as an integer. The creation is changed when a node is restarted.
  The creation of a node is stored in process identifiers, port identifiers, and
  references. This makes it possible to distinguish between identifiers from
  different incarnations of a node. Creation values are currently 32-bit
  positive integers, but this may change in future releases. If the node is not
  alive, `0` is returned.

- `delayed_node_table_gc`{: #system_info_delayed_node_table_gc } - Returns
  the amount of time in seconds garbage collection of an entry in a node table
  is delayed. This limit can be set on startup by passing command-line flag
  [`+zdntgc`](erl_cmd.md#+zdntgc) to `erl(1)`. For more information, see the
  documentation of the command-line flag.
  
  Since: OTP 18.0

- `dist`{: #system_info_dist } - Returns a binary containing a string of
  distribution information formatted as in Erlang crash dumps. For more
  information, see section
  [How to interpret the Erlang crash dumps](crash_dump.md) in the User's Guide.

- `dist_buf_busy_limit`{: #system_info_dist_buf_busy_limit } - Returns the
  value of the distribution buffer busy limit in bytes. This limit can be set
  at startup by passing command-line flag [`+zdbbl`](erl_cmd.md#+zdbbl) to
  `erl(1)`.
  
  Since: OTP R14B01

- `dist_ctrl`{: #system_info_dist_ctrl } - Returns a list of tuples
  `{Node :: node(), ControllingEntity :: port() | pid()}`, one entry for each
  connected remote node. `Node` is the node name and `ControllingEntity` is the
  port or process identifier responsible for the communication to that node.
  More specifically, `ControllingEntity` for nodes connected through TCP/IP (the
  normal case) is the socket used in communication with the specific node.

## System Information

Returns various information about the current system (emulator) as specified by `Item`:

- `c_compiler_used`{: #system_info_c_compiler_used } - Returns a two-tuple
  describing the C compiler used when compiling the runtime system. The first
  element is an atom describing the name of the compiler, or `undefined` if
  unknown. The second element is a term describing the version of the compiler,
  or `undefined` if unknown.

- `check_io`{: #system_info_check_io } - Returns a list containing
  miscellaneous information about the emulators internal I/O checking. Notice
  that the content of the returned list can vary between platforms and over
  time. It is only guaranteed that a list is returned.

- `debug_compiled`{: #system_info_debug_compiled } - Returns `true` if the
  emulator has been debug-compiled, otherwise `false`.

- `driver_version`{: #system_info_driver_version } - Returns a string
  containing the Erlang driver version used by the runtime system. It has the
  form ["<major ver>.<minor ver>"](erl_driver.md#version_management).

- `dynamic_trace`{: #system_info_dynamic_trace } - Returns an atom
  describing the dynamic trace framework compiled into the virtual machine. It
  can be `dtrace`, `systemtap`, or `none`. For a commercial or standard build,
  it is always `none`. The other return values indicate a custom configuration
  (for example, `./configure --with-dynamic-trace=dtrace`). For more
  information about dynamic tracing, see [`dyntrace(3)`](`m:dyntrace`) manual
  page and the `README.dtrace`/`README.systemtap` files in the Erlang source
  code top directory.
  
  Since: OTP R15B01

- `dynamic_trace_probes`{: #system_info_dynamic_trace_probes } - Returns a
  `t:boolean()` indicating if dynamic trace probes (`dtrace` or `systemtap`) are
  built into the emulator. This can only be `true` if the virtual machine was
  built for dynamic tracing (that is, `system_info(dynamic_trace)` returns
  `dtrace` or `systemtap`).
  
  Since: OTP R15B01

- `emu_flavor`{: #system_info_emu_flavor } - Returns an atom describing the
  flavor of the runtime system. This will be either `emu` or `jit`. Possible
  return values can be added or removed at any time without prior notice.
  
  Since: OTP 24.0

- `emu_type`{: #system_info_emu_type } - Returns an atom describing the
  build type of the runtime system. This is normally the atom `opt` for
  optimized. Other possible return values are `debug`, `gcov`, `valgrind`,
  `gprof`, and `lcnt`. Possible return values can be added or removed at any
  time without prior notice.
  
  Since: OTP 24.0

- `halt_flush_timeout`{: #system_info_halt_flush_timeout } - Returns the
  default *halt flush timeout* set by the `erl`
  [`+zhft <Timeout>`](erl_cmd.md#+zhft) command line flag.

  Since: OTP 27.0

- `info`{: #system_info_info } - Returns a binary containing a string of
  miscellaneous system information formatted as in Erlang crash dumps. For more
  information, see section
  [How to interpret the Erlang crash dumps](crash_dump.md) in the User's Guide.

- `kernel_poll`{: #system_info_kernel_poll } - Returns `true` if the
  emulator uses some kind of kernel-poll implementation, otherwise `false`.

- `loaded`{: #system_info_loaded } - Returns a binary containing a string of
  loaded module information formatted as in Erlang crash dumps. For more
  information, see section
  [How to interpret the Erlang crash dumps](crash_dump.md) in the User's Guide.

- `machine`{: #system_info_machine } - Returns a string containing the
  Erlang machine name.

- `modified_timing_level`{: #system_info_modified_timing_level } - Returns
  the modified timing-level (an `t:integer()`) if modified timing is enabled,
  otherwise `undefined`. For more information about modified timing, see
  command-line flag [`+T`](erl_cmd.md#+T_level) in `erl(1)`

- `nif_version`{: #system_info_nif_version } - Returns a string containing
  the version of the Erlang NIF interface used by the runtime system. It is on
  the form "<major ver>.<minor ver>".
  
  Since: OTP 17.4

- `otp_release`{: #system_info_otp_release } -
  Returns a string containing the OTP release number of the OTP release that the
  currently executing ERTS application is part of.

  As from Erlang/OTP 17, the OTP release number corresponds to the major OTP
  version number. No `erlang:system_info()` argument gives the exact OTP
  version. This is because the exact OTP version in the general case is
  difficult to determine. For more information, see the description of versions
  in [System principles](`e:system:versions.md`) in System Documentation.

- `outstanding_system_requests_limit`{:
  #system_info_outstanding_system_requests_limit } - Returns the limit on the
  amount of outstanding requests made by a system process orchestrating system
  wide changes. See
  [`erlang:system_flag(outstanding_system_requests_limit, Limit)`](`m:erlang#system_flag_outstanding_system_requests_limit`)
  for more information.
  
  Since: OTP 24.2

- `port_parallelism`{: #system_info_port_parallelism } - Returns the default
  port parallelism scheduling hint used. For more information, see
  command-line argument [`+spp`](erl_cmd.md#+spp) in `erl(1)`.
  
  Since: OTP R16B

- `system_architecture`{: #system_info_system_architecture } - Returns a
  string containing the processor and OS architecture the emulator is built
  for.

- `system_logger`{: #system_info_system_logger } - Returns the current
  `system_logger` as set by [`erlang:system_flag(system_logger,
  *)`](`erlang:system_flag/2`).
  
  Since: OTP 21.3

- `system_version`{: #system_info_system_version } - Returns a string
  containing version number and some important properties, such as the number of
  schedulers.

- `trace_control_word`{: #system_info_trace_control_word } - Returns the
  value of the node trace control word. For more information, see function
  `get_tcw` in section [Match Specifications in Erlang](match_spec.md#get_tcw)
  in the User's Guide.

- `version`{: #system_info_version } - Returns a string containing the
  version number of the emulator.

- `wordsize`{: #system_info_wordsize } - Same as `{wordsize, internal}`.

- `{wordsize, internal}` - Returns the size of Erlang term words in bytes as
  an integer, that is, 4 is returned on a 32-bit architecture, and 8 is
  returned on a 64-bit architecture.

- `{wordsize, external}` - Returns the true word size of the emulator, that
  is, the size of a pointer. The value is given in bytes as an integer. On a
  pure 32-bit architecture, 4 is returned. On a 64-bit architecture, 8 is
  returned.
