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
# ERTS Release Notes

This document describes the changes made to the ERTS application.

## Erts 15.1.1

### Fixed Bugs and Malfunctions

- On Windows, successive failed socket calls caused socket to become "uninitialized".

  Own Id: OTP-19251 Aux Id: [#8853]

- The socket framework fails to start on a IPv6-only Windows machine.

  Own Id: OTP-19254 Aux Id: [#8848]

[#8853]: https://github.com/erlang/otp/issues/8853
[#8848]: https://github.com/erlang/otp/issues/8848

## Erts 15.1

### Fixed Bugs and Malfunctions

- The `erl -man example` has been corrected to not consider values set in `ERL_ZFLAGS` and stop parsing arguments when a `--` is encountered.

  Own Id: OTP-19098 Aux Id: [PR-8478] [GH-8477]

- Compiler warnings for  Windows I/O back-end have been silenced.

  Own Id: OTP-19113

- Bugs related to `return_to` trace have been fixed. It did not work for more than once trace session and it did sometimes not trigger for exceptions.

  Own Id: OTP-19122

- Potential deadlocks while writing a crash dump have been eliminated.

  Own Id: OTP-19133 Aux Id: [PR-8521], [GH-8498]

- When loading a damaged or too old BEAM file, the runtime system could crash.

  Own Id: OTP-19153 Aux Id: [PR-8623]

- A scheduler thread could get stuck when deleting a memory allocator carrier when adjacent carriers were deleted and/or inserted simultaneously by other schedulers. This in turn could cause the other schedulers to get stuck as well.

  Own Id: OTP-19154 Aux Id: [GH-8613], [PR-8627]

- Statistics for number of carriers in a shared pool after calling `instrument:allocations` or `instrument:carriers` are now correct. Also, a potential bug in carrier block scanning was eliminated.

  Own Id: OTP-19166 Aux Id: [PR-8636]

- A race in the kTLS flavour of SSL distribution has been fixed so that `inet_drv.c` doesn't read ahead too much data, which could cause the kTLS encryption to be activated too late when some encrypted data had already been read into the `inet_drv.c` buffer as unencrypted.

  Own Id: OTP-19175 Aux Id: [GH-8561], [PR-8690]

- Fixed an emulator crash relating to compressed ETS tables.

  Own Id: OTP-19176 Aux Id: [PR-8683]

- A function (encode_sockaddr) was called with superfluous argument, on Windows, in the net nif.

  Own Id: OTP-19181

- Fixed a crash that could happen on reallocation failure.

  Own Id: OTP-19192

- Man pages are now available for `erl`, `erlc`, `dialyzer`, and all other programs that are included in Erlang/OTP.

  Own Id: OTP-19201 Aux Id: [PR-8740]

- A previous correction in the Erlang/OTP 27.0.1 emergency patch had the unfortunate side effect of sometimes causing an unnecessary fullsweep (major) garbage collection instead of a  generation (minor) garbage collection. This has been corrected.

  Own Id: OTP-19209 Aux Id: [PR-8751], [PR-8539]

- Fixed trace matchspec functions `trace` and `enable_trace` to use the session tracer when enabling trace flags on untraced processes.

  Own Id: OTP-19211 Aux Id: [GH-8657]

- Fixed a typo in the type spec for `t:erlang:garbage_collection_defaults/0`.

  Own Id: OTP-19215 Aux Id: [PR-8757]

- Corrected socket:ioctl for genaddr (SIOCGENADDR).

  Own Id: OTP-19216

- The support for Transparent Huge Pages has been disabled on non-amd64 Linux systems.

  Own Id: OTP-19219 Aux Id: [PR-8702]

- Fixed a race condition on Windows when upgrading from `-noshell` to a shell that would cause Erlang to crash with the error:
  
  ```
  {'GetOverlappedResult',
    'The I/O operation has been aborted because of either a thread exit or an application request.'}.
  ```

  Own Id: OTP-19220 Aux Id: [PR-8774] [GH-7621]

[PR-8478]: https://github.com/erlang/otp/pull/8478
[GH-8477]: https://github.com/erlang/otp/issues/8477
[PR-8521]: https://github.com/erlang/otp/pull/8521
[GH-8498]: https://github.com/erlang/otp/issues/8498
[PR-8623]: https://github.com/erlang/otp/pull/8623
[GH-8613]: https://github.com/erlang/otp/issues/8613
[PR-8627]: https://github.com/erlang/otp/pull/8627
[PR-8636]: https://github.com/erlang/otp/pull/8636
[GH-8561]: https://github.com/erlang/otp/issues/8561
[PR-8690]: https://github.com/erlang/otp/pull/8690
[PR-8683]: https://github.com/erlang/otp/pull/8683
[PR-8740]: https://github.com/erlang/otp/pull/8740
[PR-8751]: https://github.com/erlang/otp/pull/8751
[PR-8539]: https://github.com/erlang/otp/pull/8539
[GH-8657]: https://github.com/erlang/otp/issues/8657
[PR-8757]: https://github.com/erlang/otp/pull/8757
[PR-8702]: https://github.com/erlang/otp/pull/8702
[PR-8774]: https://github.com/erlang/otp/pull/8774
[GH-7621]: https://github.com/erlang/otp/issues/7621

### Improvements and New Features

- Added functions `getservbyname` and `getservbyport` to the `net` module.

  Own Id: OTP-19101 Aux Id: OTP-18835

- Introduced enet | esock variants of `m:inet` functions, either when called with sockets,
  with explicit inet_backend config or with the e inet_backend kernel config option.

  Own Id: OTP-19132 Aux Id: OTP-19101

- Optimize process and port creation when such tracing is not enabled by any trace session.

  Own Id: OTP-19167 Aux Id: [PR-8655]

- Compiler warnings for some removed functions have been corrected to point out the correct replacement functions.

  Own Id: OTP-19186 Aux Id: [PR-8709]

- A boolean option `read_ahead` has been implemented for `gen_tcp`, default `true`, to facilitate not reading past (caching data) the end of a packet.  In particular, for kTLS, caching data could read in data that was supposed to be decrypted by the platform's network stack, before crypto parameters could be activated.

  Own Id: OTP-19199 Aux Id: OTP-19175, [GH-8561], [GH-8690], [GH-8785]

- The `m:zip` module has been updated with support for:
  
  * zip64 archives - Archives larger than 4GB or with more than 2^32 entries.
  * extended timestamps - Higher resolution and in UTC.
  * UID/GID - Save and extract the original UID/GID.
  * Fixes so that permission mode attributes are correctly read and set for files in archives.
  * `zip:list_dir/2` now also returns directories, not only files. (You can disable this behaviour by using the option `skip_directories`).
  
  Various bugs in the original implementation have also been fixed, such as:
  
  * Correctly encode and decode the DOS timestamps for entries within an archive (that is the non-extended timestamp).
  * Fix DOS timestamps to be set to localtime instead of UTC (use extended timestamps for UTC timestamps).
  * Use the unix file attributes read from disk when creating archives instead of setting everything to 644.

  Own Id: OTP-19214 Aux Id: [PR-8765]

[PR-8655]: https://github.com/erlang/otp/pull/8655
[PR-8709]: https://github.com/erlang/otp/pull/8709
[GH-8561]: https://github.com/erlang/otp/issues/8561
[GH-8690]: https://github.com/erlang/otp/issues/8690
[GH-8785]: https://github.com/erlang/otp/issues/8785
[PR-8765]: https://github.com/erlang/otp/pull/8765

## Erts 15.0.1

### Fixed Bugs and Malfunctions

- In rare circumstances the JIT could do an unsafe in-place update of a tuple.

  Own Id: OTP-19108 Aux Id: [PR-8539]

- When a port command crashed in the inet driver during `gen_tcp:send/2`, a monitor `'DOWN'` message could be left lingering in the caller's mailbox. This has now been fixed.

  Own Id: OTP-19121 Aux Id: [GH-8484]

- `'DOWN'` messages originating from a monitored port, contained the atom `process` instead of the atom `port` as the third element when the exit reason was not an immediate term.

  Own Id: OTP-19123 Aux Id: [GH-8484], [PR-8546]

- Fix so that the options to enable Transparent Huge Page alignment of the Erlang VM executable are only applied to the Erlang VM and not other native programs such as `erlc` and `dialyzer`. This bug was introduced in Erlang/OTP 27.0.

  Own Id: OTP-19137 Aux Id: [GH-8574]

- When [*no time warp mode*](time_correction.md#no-time-warp-mode) was enabled, a smaller Erlang monotonic time could be read than a previously read time, i.e., breaking the monotonic property. The runtime system will abort when detecting an issue like this since OTP 24.3.4.17 and OTP 25.0.
  
  Up until OTP 25 *no time warp mode* is the default. As of OTP 26 [*multi time warp mode*](time_correction.md#multi-time-warp-mode) is the default.

  Own Id: OTP-19147 Aux Id: ERIERL-1043, ERIERL-1106, [PR-8619]

- When calling `trace:function(Session, _, true, [meta])` the meta tracer was incorrectly set to be the calling process. Now it's set to the session tracer as expected.

  Own Id: OTP-19151 Aux Id: [PR-8616], [GH-8614]

[PR-8539]: https://github.com/erlang/otp/pull/8539
[GH-8484]: https://github.com/erlang/otp/issues/8484
[GH-8484]: https://github.com/erlang/otp/issues/8484
[PR-8546]: https://github.com/erlang/otp/pull/8546
[GH-8574]: https://github.com/erlang/otp/issues/8574
[PR-8619]: https://github.com/erlang/otp/pull/8619
[PR-8616]: https://github.com/erlang/otp/pull/8616
[GH-8614]: https://github.com/erlang/otp/issues/8614

## Erts 15.0

### Fixed Bugs and Malfunctions

- Bugs in how `erl -extra` interacts with passing flags via ERL_*FLAGS or `-args_file` have been fixed.

  Own Id: OTP-18766 Aux Id: [PR-7639]

- Fixed a bug that prevented the emulator from building on recent versions of Yocto Linux.

  Own Id: OTP-18918 Aux Id: [PR-7952]

- Fixed spectre mitigation configure test to work with GCC patches to always add `-fcf-protection=full`.

  Own Id: OTP-18928 Aux Id: [PR-8006]

- A  call to `socket:[recv|recvfrom|recvmsg]/*` with Timeout = 0 on Windows could cause a (case clause) crash if data is immediately available.

  Own Id: OTP-19063 Aux Id: OTP-18835

- Fix bug on Windows where `exit_status` would not be sent when a port exits after the stdin/stdout handles have been closed.

  Own Id: OTP-19077 Aux Id: [PR-8324]

[PR-7639]: https://github.com/erlang/otp/pull/7639
[PR-7952]: https://github.com/erlang/otp/pull/7952
[PR-8006]: https://github.com/erlang/otp/pull/8006
[PR-8324]: https://github.com/erlang/otp/pull/8324

### Improvements and New Features

- Refactored how the JIT handles POSIX signals and how they affect thread stacks, allowing us to use the native stack register for Erlang stacks on more platforms.
  
  Notably, containers built on 64-bit x86 Alpine Linux images will now perform much better in sequential code. As an example, running `dialyzer` over the OTP code base finishes about 15% quicker.

  Own Id: OTP-18568 Aux Id: [PR-7174]

- The `m:instrument` module can now track allocations on a per-process or per-port basis.

  Own Id: OTP-18577 Aux Id: [PR-7236]

- The `pid` field returned from [`erlang:fun_info/1,2`](`erlang:fun_info/2`) is now always the pid for the `init` process of the local node, not the pid for the actual process that created the fun.

  *** POTENTIAL INCOMPATIBILITY ***

  Own Id: OTP-18594 Aux Id: [PR-7274]

- By default, escripts will now be compiled instead of interpreted. That means that the `compiler` application must be installed.

  *** POTENTIAL INCOMPATIBILITY ***

  Own Id: OTP-18639 Aux Id: [PR-7348]

- A binary returned from the `m:socket` receive functions is no longer created as a sub binary of an often large receive buffer binary (socket option `{otp,rcvbuf}`).  This avoids space waste, trusting the allocators to implement reallocation efficiently.

  Own Id: OTP-18642 Aux Id: [GH-6152], [PR-7465]

- The default process limit has been raised to `1048576` processes.

  Own Id: OTP-18699 Aux Id: [PR-7388]

- The `erlang:system_monitor/2` functionality is now able to monitor long message queues in the system.

  Own Id: OTP-18709 Aux Id: [PR-7651]

- The `erl` command now supports the `-S` flag, which is similar to the `-run` flag, except that it will pass all arguments up to end of the command line to the called function. (The `-run` flag will not pass arguments beginning with a hyphen.) Another difference is that `-S` will always call a function with one argument, passing an empty list if no arguments were given.

  Own Id: OTP-18744 Aux Id: [PR-7470]

- When implementing an alternative carrier for the Erlang distribution, a separate input handler process may now be registered, using `erlang:dist_ctrl_input_handler/2`, also in the case when the distribution controller is a port.

  Own Id: OTP-18774 Aux Id: [PR-7110]

- The call stack trace has now been added to the error reported by `erlang:process_flag/2` when `max_heap_size` limit has been exceeded.

  Own Id: OTP-18779 Aux Id: [PR-7592]

- `-callback` attributes have been added to `m:erl_tracer`.

  Own Id: OTP-18794 Aux Id: [PR-7703]

- For `inet_backend = socket`, setting the `active` socket option alone, to `once`, `true` or `N` has been optimized, as well as the corresponding data delivery.

  Own Id: OTP-18835

- New functions `socket:sendv/*` for sending I/O vectors have been added.

  Own Id: OTP-18845

- Socket options that take string now also accept binaries.

  Own Id: OTP-18849 Aux Id: [PR-6510]

- Native coverage support has been implemented in the JIT. It will  automatically be used by the `m:cover` tool to reduce the execution overhead when running cover-compiled code.
  
  There are also new APIs to support native coverage without using the `cover` tool.
  
  To instrument code for native coverage it must be compiled with the [`line_coverage`](`m:compile#line_coverage`) option.
  
  To enable native coverage in the runtime system, start it like so:
  
  ```text
  $ erl +JPcover true
  ```
  
  There are also the following new functions for supporting native coverage:
  
  * `code:coverage_support/0`
  * `code:get_coverage/2`
  * `code:reset_coverage/1`
  * `code:get_coverage_mode/0`
  * `code:get_coverage_mode/1`
  * `code:set_coverage_mode/1`

  Own Id: OTP-18856 Aux Id: [PR-7856]

- Changed the default value of the command line flag `-code_path_choice` to `strict`. 
  
  Note that for application systems using archives, it is necessary to add the `code_path_choice relaxed` to the command line that invokes `erl`.

  Own Id: OTP-18894 Aux Id: [PR-7243]

- Added module loading to `erl -init_debug` printouts.

  Own Id: OTP-18929 Aux Id: [PR-8004]

- When the runtime system halts, it performs various flush operations before terminating. By default there is no limit on how much time the flush operations are allowed to take. A new *halt flush timeout* functionality has been introduced which can be used for limiting the amount of time that the flushing operations are allowed to take. For more information see the documentation of the [`flush_timeout`](`m:erlang#halt_flush_timeout`) option of the [`erlang:halt/2`](`erlang:halt/2`) BIF and the documentation of the `erl` [`+zhft <Timeout>`](erl_cmd.md#+zhft) command line flag.

  Own Id: OTP-18938 Aux Id: [PR-8035], [GH-7438]

- Optimized code loading by moving certain operations from the code server to the caller.

  Own Id: OTP-18941 Aux Id: [PR-7981]

- Updated asmjit to version a465fe71ab3d0e224b2b4bd0fac69ae68ab9239d

  Own Id: OTP-18942

- The deprecated functions in `m:zlib` have been removed. That includes `inflateChunk/{1,2}`, `getBufSize/1`, `setBufSize/2`, the CRC32 functions, and the Adler checksum functions.

  Own Id: OTP-18950

- The documentation has been migrated to use Markdown and ExDoc.

  Own Id: OTP-18955 Aux Id: [PR-8026]

- Safe destructive update of tuples has been implemented in the compiler and runtime system. This allows the VM to update tuples in-place when it is safe to do so, thus improving performance by doing less copying but also by producing less garbage.
  
  Example:
  
  ```erlang
  -record(rec, {a,b,c}).
  
  update(#rec{a=needs_update,b=N}=R0) ->
      R = R0#rec{a=up_to_date},
      if
          N < 0 ->
              R#rec{c=negative};
          N == 0 ->
              R#rec{c=zero};
          N > 0 ->
              R#rec{c=positive}
      end.
  ```
  
  The record updates in each of the three clauses of the `if` can safely be done in-place, because variable `R` is not used again.

  Own Id: OTP-18972 Aux Id: [PR-8090]

- The obsolete and undocumented support for opening a port to an external
  resource by passing an atom (or a string) as first argument to
  [`open_port()`](`erlang:open_port/2`), implemented by the vanilla driver,
  has been removed. This feature has been scheduled for removal in OTP 27
  since the release of OTP 26.

  Own Id: OTP-18976 Aux Id: [PR-7125]

- An optional NIF callback `ERL_NIF_OPT_ON_UNLOAD_THREAD` to be called by all scheduler threads when a NIF library is unloaded. Used for releasing thread specific data. Can be set with function `enif_set_option`.

  Own Id: OTP-18977 Aux Id: [PR-7809]

- There is a new module `m:trace` in Kernel providing the same trace functionality as `erlang:trace/3` and `erlang:trace_pattern/3`, but with the addition of **dynamic isolated trace sessions**.

  Own Id: OTP-18980

- Added the `+MMlp on|off` emulator option to let the `mseg` allocator use "large pages" (sometimes known as "huge pages" or "super pages"). This currently only affects super-carrier allocations, but may be extended in the future.

  Own Id: OTP-18984 Aux Id: [PR-7977]

- `inet_backend = socket` has been optimized and reworked to be more compatible with the original `inet_backend = inet`.

  Own Id: OTP-19004 Aux Id: OTP-18835

- The `socket` documentation has been reworked, and due to
  that a few details were fixed:
  * `socket:is_supported/1` now returns `true` for example for `protocols`
    that is a "category", not an item.
  * `socket:cancel_monitor/1` no longer badargs for a monitor that was set by
    another process, instead it returns `false` as for other unknown
    `reference()`s.

  Own Id: OTP-19054

[PR-7174]: https://github.com/erlang/otp/pull/7174
[PR-7236]: https://github.com/erlang/otp/pull/7236
[PR-7274]: https://github.com/erlang/otp/pull/7274
[PR-7348]: https://github.com/erlang/otp/pull/7348
[GH-6152]: https://github.com/erlang/otp/issues/6152
[PR-7465]: https://github.com/erlang/otp/pull/7465
[PR-7388]: https://github.com/erlang/otp/pull/7388
[PR-7651]: https://github.com/erlang/otp/pull/7651
[PR-7470]: https://github.com/erlang/otp/pull/7470
[PR-7110]: https://github.com/erlang/otp/pull/7110
[PR-7592]: https://github.com/erlang/otp/pull/7592
[PR-7703]: https://github.com/erlang/otp/pull/7703
[PR-6510]: https://github.com/erlang/otp/pull/6510
[PR-7856]: https://github.com/erlang/otp/pull/7856
[PR-7243]: https://github.com/erlang/otp/pull/7243
[PR-8004]: https://github.com/erlang/otp/pull/8004
[PR-8035]: https://github.com/erlang/otp/pull/8035
[GH-7438]: https://github.com/erlang/otp/issues/7438
[PR-7981]: https://github.com/erlang/otp/pull/7981
[PR-8026]: https://github.com/erlang/otp/pull/8026
[PR-8090]: https://github.com/erlang/otp/pull/8090
[PR-7125]: https://github.com/erlang/otp/pull/7125
[PR-7809]: https://github.com/erlang/otp/pull/7809
[PR-7977]: https://github.com/erlang/otp/pull/7977

## Erts 14.2.5.3

### Fixed Bugs and Malfunctions

* A race in the kTLS flavour of SSL distribution has been fixed so inet_drv.c doesn't read ahead too much data which could cause the kTLS encryption to be activated too late when some encrypted data had already been read into the inet_drv.c buffer as unencrypted.

  Own Id: OTP-19175 Aux Id: GH-8561, PR-8690

## Erts 14.2.5.2

### Fixed Bugs and Malfunctions

* When [*no time warp mode*](time_correction.md#no-time-warp-mode) was enabled, a smaller Erlang monotonic time could be read than a previously read time, i.e., breaking the monotonic property. The runtime system will abort when detecting an issue like this since OTP 24.3.4.17 and OTP 25.0.

  Up until OTP 25 *no time warp mode* is the default. As of OTP 26 [*multi time warp mode*](time_correction.md#multi-time-warp-mode) is the default.

  Own Id: OTP-19147 Aux Id: ERIERL-1043, ERIERL-1106, PR-8619
* A scheduler thread could get stuck when deleting a memory allocator carrier when adjacent carriers were deleted and/or inserted simultaneously by other schedulers. This in turn could cause the other schedulers to get stuck as well.

  Own Id: OTP-19154 Aux Id: GH-8613, PR-8627

## Erts 14.2.5.1

### Fixed Bugs and Malfunctions

* A call to socket:\[recv|recvfrom|recvmsg]/* with Timeout = 0 on Windows could cause a (case clause) crash if data is immediately available.

  Own Id: OTP-19063 Aux Id: OTP-18835
* When a port command crashed in the inet driver during `gen_tcp:send/2`, a monitor `'DOWN'` message could be left lingering in the caller's mailbox. This has now been fixed.

  Own Id: OTP-19121 Aux Id: GH-8484
* `'DOWN'` messages originating from a monitored port, contained the atom `process` instead of the atom `port` as the third element when the exit reason was not an immediate term.

  Own Id: OTP-19123 Aux Id: GH-8484, PR-8546

## Erts 14.2.5

### Fixed Bugs and Malfunctions

* [`gen_sctp:send/3,4`](`gen_sctp:send/4`) now waits for the send to complete instead of returning an OS result such as `{error, ewouldblock}`.

  Own Id: OTP-19061
* ETS functions did not properly handle keys containing maps, sometimes matching too many or too few objects.

  Own Id: OTP-19070 Aux Id: GH-8385
* Fix CPU quota determination for cgroups.

  The bug was introduced through OTP-18999.

  Own Id: OTP-19071 Aux Id: OTP-18999 PR-8380

### Improvements and New Features

* Added a warning to open_port/2 regarding the BadBatBut attack affecting Windows.

  Own Id: OTP-19069

## Erts 14.2.4

### Fixed Bugs and Malfunctions

* Fixed CPU quota determination for cgroup version 2

  Own Id: OTP-18999 Aux Id: GH-7928
* Fix faulty reduction counting in exiting process which could cause it to do unnecessary yielding.

  Own Id: OTP-19014
* Fix bug in `re:run/3` where if an invalid UTF-8 subject was given, re:run could get stuck in an infinite loop. Bug was introduced in Erlang/OTP 22.1.

  Own Id: OTP-19015 Aux Id: ERIERL-682
* On AArch64 (ARM64), Erlang code using bit syntax construction compiled using Erlang/OTP 24 could crash the runtime system when run in Erlang/OTP 26.2.3.

  Own Id: OTP-19021 Aux Id: GH-8238, PR-8248
* Calling `erlang:trace/3` with first argument one of `ports`, `processes`, `existing_ports`, `existing_processes`, `existing` or `all`, could cause emulator crash if a dirty scheduler was executing a simultaneous trace action.

  Own Id: OTP-19034
* Fixed an integer overflow when the monotonic time unit reported by the operating system was greater than 10 and lower than 100 microseconds.

  Own Id: OTP-19036 Aux Id: GH-8186
* Fix option reuseaddr for FreeBSD 14

  Own Id: OTP-19041
* When a traced process executing on a dirty scheduler received an exit signal, the dirty scheduler could use the wrong thread specific data which could lead to a crash.

  Own Id: OTP-19043 Aux Id: PR-8342
* Fixed a more or less harmless bug that caused time correction of Erlang monotonic time to become slightly off on Windows platforms when `QueryPerformanceCounter()` was used as OS monotonic time source.

  `erlang:system_info(os_monotonic_time_source)` now also returns information about *used resolution* which not always corresponds to the resolution of the OS monotonic time source.

  Own Id: OTP-19048 Aux Id: PR-8343
* When using IPv6, classic gen_udp failed to add (group) membership (drop was used instead).

  Own Id: OTP-19049 Aux Id: #8176
* Fix bug on Windows where "Unknown event: 2" would be printed to the console.

  Own Id: OTP-19060

### Improvements and New Features

* Checks for monotonicity of monotonic time have been improved so that Erlang and OS monotonic time are checked separately.

  A new `configure` argument `--enable-ensure-os-monotonic-time` has also been added. It enables functionality ensuring the monotonicity of monotonic timestamps delivered by the OS. When a non-monotonic timestamp is detected, it will be replaced by the last delivered monotonic timestamp before being used by Erlang's time functionality. Note that you do *not* want to enable this unless the OS monotonic time source on the system fails to produce monotonic timestamps. This since ensuring the monotonicity of OS monotonic timestamps will hurt scalability and performance of the system.

  Own Id: OTP-19044 Aux Id: ERIERL-1043, PR-8342
* For severe errors, when the \`socket\` module terminates the Erlang VM, now an erl_crash.dump is produced, to facilitate post mortem debugging.

  Own Id: OTP-19058

## Erts 14.2.3

### Fixed Bugs and Malfunctions

* Fixed compile warning in erl_nif.c for gcc-13.

  Own Id: OTP-18906 Aux Id: PR-7930
* Fix C++ compile error for macros `enif_select_read` and friends.

  Own Id: OTP-18907 Aux Id: PR-7932
* Fixed a name clash on Solaris that prevented the JIT from being built.

  Own Id: OTP-18940 Aux Id: GH-8024
* Fix termcap detection on solaris.

  Own Id: OTP-18952 Aux Id: PR-8025
* Fix heap corruption bug that could cause runaway memory consumption due to circular offheap list at process exit. Other symptoms may also be possible. Bug exists since OTP 25.0.

  Own Id: OTP-18971 Aux Id: GH-8044
* Do not clear tracing in old module instance if load fails with 'not_purged'.

  Own Id: OTP-18979
* When exceeding the \`max_heap_size\` limit in a garbage collection initiated by some bit syntax operations, the process would not always terminate immediately.

  Own Id: OTP-18982
* The code server could be hanging if a module with `on_load` function was loaded at the same time as another module was purged using `erlang:purge_module` directly.

  Own Id: OTP-19006
* A process optimized for parallel signal delivery could under some circumstances lose wakeup information. That is, the processes was not woken up to take care of the signal, so the signal would not be taken care of until the process was woken by another signal. Only processes configured with [`message_queue_data`](`m:erlang#process_flag_message_queue_data`) set to `off_heap` utilize this optimization.

  Own Id: OTP-19008 Aux Id: GH-8119, PR-8201
* Fix segfault when generating crashdump containing a fun places in persistent_term storage.

  Own Id: OTP-19009 Aux Id: PR-8181
* By default the JIT is disabled on Intel Macs, because of annoying poups on macOS Sonoma. It is now possible to explicitly enable the JIT on Intel Macs. Here is how: `./configure --enable-jit`

  Own Id: OTP-19011

## Erts 14.2.2

### Fixed Bugs and Malfunctions

* 32-bit runtime systems on most Unix like platforms could crash if a BIF timer was set with a huge timeout of more than 68 years into the future. In order for the crash to occur, the huge timer (at a later time than when it was set) had to become the nearest active timer set on the specific scheduler on which it was set. This could not happen on a system with only one scheduler since there would always be shorter timers in the system.

  Setting a timer larger than 49 days on Windows could under rare circumstances cause the timeout to be delayed.

  Own Id: OTP-18911 Aux Id: ERIERL-1023, PR-7983
* Fix bug where the system installed openssl/md5.h would be confused with the vendored md5.h.

  Own Id: OTP-18931 Aux Id: GH-7987 PR-7989
* The JIT has now been disabled on x86 Macs to prevent annoying the "verifying shm-xyz" popups introduced in MacOS Sonoma.

  ARM Macs are unaffected.

  Own Id: OTP-18949
* Garbage collection of a process on a dirty scheduler could collide with signal handling for that process causing a crash of the runtime system. This bug was introduced in OTP 25.3.2.8 and OTP 26.2.

  Own Id: OTP-18957 Aux Id: GH-8051, PR-8088, OTP-18841

## Erts 14.2.1

### Fixed Bugs and Malfunctions

- Removed unnecessary PCRE source tar-ball.

  Own Id: OTP-18902

## Erts 14.2

### Fixed Bugs and Malfunctions

- Fix erl.exe to to restore the console to its original state when exiting. This
  bug was introduced in OTP 26.0 and only happens when erl.exe is run in
  cmd.exe.

  Own Id: OTP-18751 Aux Id: GH-7621 GH-7548

- Fix faulty debug assert when page size is larger than 16kb, like on PowerPC.
  Did crash debug VM directly at start.

  Own Id: OTP-18802

- `zlib` will no longer randomly return garbage (negative) Adler32 checksums.

  Own Id: OTP-18811 Aux Id: ERIERL-994

- Replaced unintentional Erlang Public License 1.1 headers in some files with
  the intended Apache License 2.0 header.

  Own Id: OTP-18815 Aux Id: PR-7780

- A process with `message_queue_data` configured as `off_heap` could end up in
  an inconsistent state when being `receive` traced, inspected using
  [`process_info/2`](`process_info/2`) with the `message_queue_len` item, or
  inspected using the break menu (CTRL-C). When it ended up in this inconsistent
  state, it was not enqueued into a run queue even though it was set in a
  runnable state.This also effected signals being sent to the process after it
  had gotten into this inconsistent state, in such a way that it was from this
  point not possible to communicate with it.

  Own Id: OTP-18838 Aux Id: PR-7822, GH-7801

- A race occurring when a process was selected for dirty execution
  simultaneously as it was scheduled for handling a signal could cause the
  process to end up in an inconsistent state. When it ended up in this
  inconsistent state, it was not enqueued into a run queue even though it was
  set in a runnable state. This also effected signals being sent to the process
  after it had gotten into this inconsistent state, in such a way that it was
  from this point not possible to communicate with it.

  Own Id: OTP-18839 Aux Id: PR-7822, GH-7801

- When a process had to to wait in the run queue for a long time before being
  selected for dirty execution, it could not receive signals. This caused
  inspection of such a process, for example using
  [`process_info/2`](`process_info/2`), to take a long time.

  This issue was introduced in OTP 25.3.2.6 and 26.1 when fixing an issue where
  a constant flow of signals prevented a process from being able to execute
  dirty.

  Own Id: OTP-18841 Aux Id: PR-7822, GH-7801, OTP-18737

- Fixed a bug in the JIT that miscompiled large `select_val` instructions.

  Own Id: OTP-18842

- Fix bug on Windows where large writes to `standard_io` could cause duplicate
  data to be written.

  Own Id: OTP-18871 Aux Id: GH-7838

- The `struct ip_mreqn` field `imr_ifindex` had got an incorrect byte order
  conversion that has been corrected.

  Own Id: OTP-18880 Aux Id: GH-7736, PR-7761

- On OTP 24 and OTP 25, incoming distributed messages larger than 64 KiB sent
  using an alias leaked memory if the alias had been removed prior to entering
  the node. This issue was not present on OTP 26.

  Incoming distributed messages larger than 64 KiB sent using an alias which had
  been removed on the receiving node could crash the node. This crash was quite
  unlikely on OTP 24 and OTP 25, but very likely on OTP 26.

  `'DOWN'` signals with exit reason larger than 64 KiB directed towards a
  process on a node with a not matching creation leaked memory on the receiving
  node. Such signals should however be very rare.

  Own Id: OTP-18885 Aux Id: GH-7834, GH-7890, PR-7915

### Improvements and New Features

- Add Windows support for DGRAM socket connect.

  Own Id: OTP-18762

- [`process_info/2`](`erlang:process_info/2`) now supports lookup of values for
  specific keys in the process dictionary. For example,
  `{{dictionary, Key}, Value} = process_info(Pid, {dictionary, Key})`.

  Own Id: OTP-18765 Aux Id: PR-7707

- Removed unnecessary regexp library used when generating yielding BIFs.

  Own Id: OTP-18830 Aux Id: PR-7823

- Fix tty restore when `+Bc` is used.

  Own Id: OTP-18872 Aux Id: GH-7832

- Replaced old md5 implementation with an implementation from OpenSSL.

  Own Id: OTP-18877

- Removed unused `makewhatis` script.

  Own Id: OTP-18899

## Erts 14.1.1

### Fixed Bugs and Malfunctions

- If the external term format encoding of an argument list part of a distributed
  spawn operation was faulty, the newly spawned remote process could misbehave.
  The misbehavior included hanging or interpret an incoming message as an
  argument list to use. This was very unlikely to happen unless using an
  alternate implementation of the distribution protocol which made a faulty
  encoding of the argument list. The child process will now detect this error
  and terminate before executing the user specified code.

  Own Id: OTP-18790 Aux Id: PR-7742

- Fix bugs where if the body of a matchspec would return a map with a variable
  ('$1', '$\_' etc) as one of the keys or values and the variable was not an
  immidiate, the term would not be copied to the receiving processes heap. This
  would later corrupt the term in the table as the GC could place move markers
  in it, which in turn would cause the VM to crash.

  Bug has been present for since OTP 17.0.

  Own Id: OTP-18797 Aux Id: PR-7712 GH-7683

## Erts 14.1

### Fixed Bugs and Malfunctions

- maps:put with existing key and identical value was not optimized as a no-op
  correctly if having the same 32-bit hash as another key in the map. In
  practice very rare and harmless.

  Own Id: OTP-18592

- Fixed an issue with truncated crash slogans on failed emulator start.

  Own Id: OTP-18623 Aux Id: GH-7344

- Fixed a bug where the emulator was unable to determine the current `cgroup`
  CPU quota.

  Own Id: OTP-18645 Aux Id: GH-7401

- A process optimized for parallel signal delivery could under some
  circumstances lose wakeup information. That is, the processes was not woken up
  to take care of the signal, so the signal would not be taken care of until the
  process was woken by another signal. Only processes configured with
  [`message_queue_data`](`m:erlang#process_flag_message_queue_data`) set to
  `off_heap` utilize this optimization.

  Own Id: OTP-18647 Aux Id: PR-7595

- Function socket:close/1 could cause a VM crash on Windows.

  Own Id: OTP-18669 Aux Id: OTP-18029

- Fixed a bug in the ARM JIT where it could accidentally add garbage trailing
  bits when creating bitstrings whose size wasn't an even multiple of 8 bits.

  Own Id: OTP-18672 Aux Id: GH-7469

- Fix `erlang:system_info/1` documentation to show correct types.

  Own Id: OTP-18674 Aux Id: PR-7472

- Expanded the documentation about how to use the `standard_io`,
  `standard_error` and `user` I/O devices.

  Added the types [`io:standard_io/0`](`t:io:standard_io/0`),
  `io:standard:error/0` and [`io:user/0`](`t:io:user/0`).

  Own Id: OTP-18676 Aux Id: PR-7473 GH-7459

- Fix compilation with GNU termcap.

  Own Id: OTP-18702 Aux Id: GH-7381

- Delivery time of message signals to a process not executing any `receive`
  expressions could become very long, potentially infinite. For example, a
  process checking for messages using
  [`process_info(self(), message_queue_len)`](`process_info/2`) or
  [`process_info(self(), messages)`](`process_info/2`) and avoiding to execute a
  `receive` expression matching on messages could be very slow in detecting new
  messages. Note that you are still discouraged from using `process_info()` this
  way. A process that wants to check if there are messages available to handle
  should execute a `receive` expression matching on messages.

  Own Id: OTP-18706 Aux Id: GH-7413, PR-7595, ERIERL-979

- On AArch64 (ARM64), when calculating both the quotient and remainder with a
  divisor begin a power two, the remainder could be incorrectly calculated.

  Own Id: OTP-18724 Aux Id: GH-7566, PR-7567

- Fix bug causing "magic" references in a `compressed` ETS table to not keep the
  referred object alive. The symptom would be the referred object being garbage
  collected prematurely and the reference appearing stale, not referring to
  anything. Examples of such magically referred objects are `atomics` and NIF
  resources.

  Own Id: OTP-18732 Aux Id: GH-7444, PR-7458

- Matching out short bitstrings with a fixed size not divisible by 8 could could
  lead to the runtime system terminating with an "Overrun heap and stack" error.

  Own Id: OTP-18733 Aux Id: GH-7292

- A constant flow of incoming non-message signals could prevent a process
  needing to execute dirty from doing so.

  Own Id: OTP-18737 Aux Id: PR-7595

- A BEAM file usually contains a chunk with the tag "Type" containing type
  information that can be used by the JIT. The `beam_lib:strip/1` takes care to
  preserve that chunk, but a build/release tool that does customized stripping
  could accidentally remove the chunk. Loading a BEAM file without the "Type"
  chunk could cause incorrect behavior of the loaded code.

  Own Id: OTP-18745 Aux Id: GH-7492, PR-7616

- `gen_udp:recv/*` for Unix Domain Socket in binary mode and passive mode has
  been fixed to not crash.

  Own Id: OTP-18747 Aux Id: GH-7605

- The cleanup operation of not yet delivered signals to a terminated process
  yielded excessively.

  Own Id: OTP-18752 Aux Id: PR-7633

- Fixed minor hashing issue with the `local` option of
  `term_to_binary()`/`term_to_iovec()`.

  Own Id: OTP-18753 Aux Id: PR-7634

### Improvements and New Features

- Update gen_tcp_socket and gen_udp_socket to handle 'completion' (socket on
  Windows).

  Own Id: OTP-18586 Aux Id: OTP-18029

- Add support for Unix Domain Sockets (only for STREAM sockets) on Windows for
  'socket'.

  Own Id: OTP-18611 Aux Id: OTP-18029, #5024

- In Erlang/OTP 27, by default escripts will be compiled before being executed.
  That means that the `compiler` application must be installed. It is possible
  to force the escript to be interpreted by adding the directive
  `-mode(interpret).` to the escript file.

  In Erlang/OTP 28, support for interpreting an escript will be removed.

  Own Id: OTP-18638

- Add basic support for socket ioctl on Windows.

  Own Id: OTP-18660

- Removed erts/etc/darwin/Info.plist, as it is no longer necessary after macos
  10.12

  Own Id: OTP-18661 Aux Id: PR-6112

- Add support for (Windows) socket option exclusiveaddruse.

  Own Id: OTP-18686

- \[socket] Add support for the 'nopush' option.

  Own Id: OTP-18687

- Add support for socket option 'BSP STATE'.

  Own Id: OTP-18693

- Add tcp socket options 'keepcnt', 'keepidle' and 'keepintvl'.

  Own Id: OTP-18698

- Add support for misc (Windows) socket options ('max_msg_size' and 'maxdg').

  Own Id: OTP-18710

## Erts 14.0.2

### Fixed Bugs and Malfunctions

- Fix using the IME (Input Method Editor) to enter text in cmd.exe and
  powershell.exe on Windows.

  Own Id: OTP-18630 Aux Id: PR-7275 GH-7029

- Multiple socket:accept calls issue. When making multiple accept calls, only
  the last call is active.

  Own Id: OTP-18635 Aux Id: #7328

- Fix the shell to ignore terminal delay when the terminal capabilities report
  that they should be used.

  Own Id: OTP-18636 Aux Id: PR-7352 GH-7308

- Fix "oldshell" to echo characters while typing on Windows.

  Own Id: OTP-18637 Aux Id: PR-7359 GH-7324

- On Windows, a call to the function socket:close, when there are waiting active
  calls to read, write or accept functions, could hang.

  Own Id: OTP-18646

- Fix issues when reading or configuring `standard_io` on Windows when erl.exe
  is started using `-noshell` flag.

  Own Id: OTP-18649 Aux Id: GH-7261 PR-7400

- The following functions are now much faster when given a long list or binary:

  - erlang:list_to_integer/1
  - erlang:binary_to_integer/1
  - erlang:binary_to_integer/2
  - erlang:list_to_integer/2
  - string:to_integer/1

  Own Id: OTP-18659 Aux Id: PR-7426

- On AArch64 (ARM64), equality and non-equality tests with literal bitstrings
  could succeed when they should fail and vice versa.

  Own Id: OTP-18663 Aux Id: GH-7433, PR-7437

## Erts 14.0.1

### Fixed Bugs and Malfunctions

- Build of the socket nif failed on Solaris 11.

  Own Id: OTP-18585 Aux Id: OTP-18029

- Fixed two reduction-counting bugs relating to binaries.

  Own Id: OTP-18587

- Constructing a binary segment not aligned with a byte boundary, with a size
  not fitting in 31 bits, and with a value not fitting in a 64-bit word could
  crash the runtime system.

  Own Id: OTP-18597

- When a binary construction failed because of bad size for a segment, the error
  information was not always correct.

  Own Id: OTP-18602 Aux Id: GH-7282

- Fixed a crash when calling a fun that was defined in a module that had been
  upgraded.

  Own Id: OTP-18621 Aux Id: GH-7288

## Erts 14.0

### Fixed Bugs and Malfunctions

- If a local fun was called while reloading the _exact same_ module that defined
  said fun, there was a small window in which the call would land in code that
  was yet to be fully loaded.

  Own Id: OTP-18016

- Fix the TLS distribution to work when starting Erlang in embedded mode and a
  connection is done before kernel is fully started.

  Own Id: OTP-18248 Aux Id: PR-6227 GH-6085

- erl `-remsh` has been improved to provide better error reasons and work when
  using a shell without terminal support (that is an "oldshell").

  Own Id: OTP-18271 Aux Id: PR-6279

- Fix so that `-fno-omit-frame-pointer` is applied to all of the Erlang VM when
  using the JIT so that tools, such as perf, can crawl the process stacks.

  Own Id: OTP-18274 Aux Id: PR-6048

- Compilation server now support unicode paths in compilation server for
  filesystems that are encoded with unicode.

  Own Id: OTP-18277 Aux Id: PR-6306

- Reintroduced the optimization that turned anonymous functions without free
  variables into literals (OTP-15195). This optimization was lost during
  refactoring in OTP 24.

  Alongside this fix, we plan to remove the "fun creator pid" feature in OTP 27.
  See
  [Upcoming Potential Incompatibilities](`e:general_info:upcoming_incompatibilities.md#fun_creator_pid`)
  for more details.

  Own Id: OTP-18498

- Fixed a crash during tracing on certain platforms that cannot use the machine
  stack for Erlang code (mainly OpenBSD and Linux with musl).

  Own Id: OTP-18561

### Improvements and New Features

- The [`enif_set_option()`](erl_nif.md#enif_set_option) function has been
  introduced into the NIF API. It can be used in order to set the
  [`ERL_NIF_OPT_DELAY_HALT`](erl_nif.md#delay_halt) and/or
  [`ERL_NIF_OPT_ON_HALT`](erl_nif.md#on_halt) options with which one can
  synchronize halt of the runtime system with flushing enabled and execution of
  NIFs.
  [Halt of the runtime system _without_ flushing enabled](`erlang:halt/2`), now
  terminates the runtime system without execution of `atexit`/`on_exit` handlers
  that may have been installed into the runtime system which might be considered
  a potential incompatibility.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17771 Aux Id: GH-5325, PR-6370

- The TTY/terminal subsystem has been rewritten by moving more code to Erlang
  from the old linked-in driver and implementing all the I/O primitives needed
  in a NIF instead.

  On Unix platforms the user should not notice a lot of difference, besides
  better handling of unicode characters and fixing of some long standing bugs.

  Windows users will notice that erl.exe has the same functionality as a normal
  Unix shell and that werl.exe has been removed and replaced with a symlink to
  erl.exe. This makes the Windows Erlang terminal experience identical to that
  of Unix.

  The re-write brings with it a number of bug fixes and feature additions:

  - The TTY is now reset when Erlang exits, fixing zsh to not break when
    terminating an Erlang session.
  - `standard_error` now uses the same unicode mode as `standard_io`.
  - Hitting backspace when searching the shell history with an empty search
    string no longer breaks the shell.
  - Tab expansion now works on remote nodes started using the JCL interface.
  - It is now possible to configure the shell slogan and the session slogans
    (that is the texts that appear when you start an Erlang shell). See the
    kernel documentation for more details.
  - Added shell:start_interactive for starting the interactive shell from a
    non-interactive Erlang session (for example an escript).
  - On Windows, when starting in detached mode the standard handler are now set
    to `nul` devices instead of being unset.
  - Standard I/O now always defaults to `unicode` mode if supported. Previously
    the default was `latin1` if the runtime system had been started with
    `-oldshell` or `-noshell` (for example in an `escript`). To send raw bytes
    over standard out, one now explicitly has to specify
    `io:setopts(standard_io, [{encoding, latin1}]).`

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17932 Aux Id: PR-6144 GH-3150 GH-3390 GH-4343 GH-4225

- Add support for socket on Windows.

  - Pre release status.
  - Error codes not finalized.
  - No explicit support for Windows specific options (socket options, flags for
    read and write).
  - New async api for Windows (completion). See the `Asynchronous calls` chapter
    in the (Socket Usage) Users Guide.
  - To ensure platform independence, gen_tcp and gen_udp is _intended_ to be
    used (not yet updated).

  Own Id: OTP-18029

- Updated configure cache for Windows. This makes configure run faster on
  Windows as many more checks are cached.

  Own Id: OTP-18053 Aux Id: PR-6101

- Optimized record updates.

  Own Id: OTP-18126 Aux Id: PR-6033

- Optimized internal hash routines.

  Own Id: OTP-18131

- As announced when OTP 25 was released,
  [multi time warp mode](time_correction.md#multi-time-warp-mode) is now enabled
  by default. This assumes that all code executing on the system is
  [time warp safe](time_correction.md#time-warp-safe-code).

  If you have old code in the system that is not time warp safe, you now
  explicitly need to start the system in
  [no time warp mode](time_correction.md#no-time-warp-mode) (or
  [singe time warp mode](time_correction.md#single-time-warp-mode) if it is
  partially time warp safe) in order to avoid problems. When starting the system
  in no time warp mode, the system behaves as it did prior to the introduction
  of the extended time functionality introduced in OTP 18.

  If you have code that is not time warp safe, you are strongly encouraged to
  change this so that you can use multi time warp mode. Compared to no time warp
  mode, multi time warp mode improves scalability and performance as well as
  accuracy and precision of time measurements.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18135 Aux Id: GH-4965, PR-6046

- There are several new optimization for binary syntax in the JIT:

  - Creation and matching of binaries with segments of fixed sizes have been
    optimized.
  - Creation and matching of UTF-8 segments have been optimized.
  - Appending to binaries has been optimized.

  Own Id: OTP-18137 Aux Id: PR-6259, PR-6404, PR-6576, PR-6804

- As announced since the release of OTP 24, support for:

  - version 4 node container types in the external term format are now
    mandatory. That is, references supporting up to 5 32-bit integer
    identifiers, and process and port identifiers with support for 64-bit data
    storage. The distribution flag
    [`DFLAG_V4_NC`](erl_dist_protocol.md#DFLAG_V4_NC) is therefor now also
    mandatory. OTP has since OTP 24 supported this. Also note that the external
    format produced by `term_to_binary()` and `term_to_iovec()` will
    unconditionally produce pids, ports, and references supporting this larger
    format.
  - the [new link protocol](erl_dist_protocol.md#new_link_protocol) introduced
    in OTP 23.3 is now mandatory. The distribution flag
    [`DFLAG_UNLINK_ID`](erl_dist_protocol.md#DFLAG_UNLINK_ID) is therefor now
    also mandatory.

  Due to the above, OTP 26 nodes will refuse to connect to OTP nodes from
  releases prior to OTP 24.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18140 Aux Id: PR-6072

- Optimization of
  [_process aliases_](`e:system:ref_man_processes.md#process-aliases`) made
  possible now that support for
  [version 4 node container types](erl_dist_protocol.md#DFLAG_V4_NC) in the
  external term format is mandatory.

  Own Id: OTP-18141 Aux Id: OTP-18140, PR-6073

- The compiler and JIT now generate better code for creation of small maps where
  all keys are literals known at compile time.

  Own Id: OTP-18185 Aux Id: GH-6139

- When `erl -eval` fails to execute a command, an error description is printed
  to standard_error.

  Own Id: OTP-18227 Aux Id: PR-6254

- Support for UTF-8 atoms and strings in the NIF interface including new
  functions `enif_make_new_atom`, `enif_make_new_atom_len` and
  `enif_get_string_length`.

  Own Id: OTP-18334 Aux Id: PR-6434

- The following `inet:setopts/2` options have been introduced:

  - **[`reuseport`](`m:inet#option-reuseport`)** - Reuse of local port. Load
    balancing may or may not be provided depending on underlying OS.

  - **[`reuseport_lb`](`m:inet#option-reuseport_lb`)** - Reuse of local port.
    Load balancing provided.

  - **[`exclusiveaddruse`](`m:inet#option-exclusiveaddruse`)** - Exclusive
    address/port usage on Windows. This socket option is Windows specific and
    will silently be ignored on other systems.

  The behavior of setting [`reuseaddr`](`m:inet#option-reuseaddr`) on Windows
  have changed in a _backwards incompatible_ way. The underlying `SO_REUSEADDR`
  socket option is now only set if both the `reusaddr` and the `reuseport`
  `inet` options have been set. This since the underlying `SO_REUSEADDR` socket
  option on Windows behaves similar to how BSD behaves if both the underlying
  socket options `SO_REUSEADDR` and `SO_REUSEPORT` have been set. See the
  documentation of the `reuseaddr` option for more information.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18344 Aux Id: PR-6522, PR-6944, OTP-18324, PR-6481, GH-6461

- `erlang:display/1` will now print large maps in a more readable way (similar
  to how small maps are printed).

  Own Id: OTP-18360 Aux Id: PR-6497

- The BIFs [`min/2`](`min/2`) and [`max/2`](`max/2`) are now allowed to be used
  in guards and match specs.

  Own Id: OTP-18367 Aux Id: GH-6544

- Fail `enif_init_resource_type` and friends by returning NULL if not called
  during load/upgrade. Old behavior was undefined.

  Own Id: OTP-18369

- New option `include_shared_binaries` for the `max_heap_size` process limit. If
  set to `true`, large binaries (> 64 bytes), which may be referred by several
  processes, are included in the memory sum compared against the `max_heap_size`
  limit.

  Own Id: OTP-18410 Aux Id: GH-5889, PR-6345

- Map comprehensions as suggested in EEP 58 has now been implemented.

  Own Id: OTP-18413 Aux Id: EEP-58, PR-6727

- Some map operations have been optimized by changing the internal sort order of
  atom keys. This changes the (undocumented) order of how atom keys in small
  maps are printed and returned by `maps:to_list/1` and `maps:next/1`. The new
  order is unpredictable and may change between different invocations of the
  Erlang VM.

  For applications where order is important, there is a new function
  `maps:iterator/2` for creating iterators that return the map elements in a
  deterministic order. There are also new modifiers `k` and `K` for the format
  string for [`io:format()`](`t:io:format/0`) to support printing map elements
  ordered.

  Own Id: OTP-18414 Aux Id: PR-6151

- Reduced memory usage of `file:read_file_info/1,2`

  Own Id: OTP-18424 Aux Id: PR-6716

- Add new function `current_stacktrace` for trace match specifications used by
  `erlang:trace_pattern/3`.

  This new option puts the current stacktrace of the caller into the trace
  message sent to the trace receiver.

  Own Id: OTP-18425 Aux Id: PR-6628, GH-5333

- The amount of significant bits in node local process identifiers and port
  identifiers has been extended from 28 bits to 60 bits on 64-bit runtime
  systems. This makes these identifiers large enough to in practice never having
  to be reused during the life time of a node.

  Own Id: OTP-18435 Aux Id: PR-6827

- New trace feature `call_memory`. Similar to `call_time` tracing, but instead
  of measure accumulated time in traced functions it measures accumulated heap
  space consumed by traced functions. It can be used to compare how much
  different functions are contributing to garbage collection being triggered.

  Own Id: OTP-18440 Aux Id: PR-6351

- It is no longer necessary to enable a feature in the runtime system in order
  to load modules that are using it. It is sufficient to enable the feature in
  the compiler when compiling it.

  That means that to use feature `maybe_expr` in Erlang/OTP 26, it is sufficient
  to enable it during compilation.

  In Erlang/OTP 27, feature `maybe_expr` will be enabled by default, but it will
  be possible to disable it.

  Own Id: OTP-18445

- Handling of `on_load` modules during boot has been improved by adding an extra
  step in the boot order for embedded mode that runs all `on_load` handlers,
  instead of relying on explicit invocation of them, later, when the kernel
  supervision tree starts.

  This is mostly a code improvement and OTP internal simplification to avoid
  future bugs and to simplify code maintenance.

  Own Id: OTP-18447

- Introduced the [`local`](`m:erlang#term_to_binary_local`) option of
  [`term_to_binary/2`](`erlang:term_to_binary/2`) and
  [`term_to_iovec/2`](`erlang:term_to_iovec/2`).

  Own Id: OTP-18477 Aux Id: PR-7006

- Document the commonly used practice to create and store static atoms at NIF
  load time in callbacks `load` or `upgrade`.

  Own Id: OTP-18483 Aux Id: PR-6888

- Optimize `ets:lookup_element` for uncompressed tables by using a more
  efficient method to copy the term from ETS to the heap of the calling process.

  Own Id: OTP-18493 Aux Id: PR-6272

- The default encoding of atoms by `term_to_binary` and `term_to_iovec` have
  changed from Latin1 to UTF-8. The old encoding can still be obtained with
  options `{minor_version, 1}`.

  Apart from encoding code points between 128 and 255 with two bytes (UTF-8)
  instead of one, most atoms will occupy one less byte as the length field use
  only one byte instead of two if possible.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18505 Aux Id: PR-6991

- The version of zlib included in the Erlang/OTP source code is now 1.2.13.

  Own Id: OTP-18517

- `gen_tcp:send/*`, `gen_udp:send/*` and `gen_sctp:send/*` have been optimized
  to use the infamous receive reference optimization, so now sending should not
  have bad performance when the calling process has a large message queue.

  Own Id: OTP-18520 Aux Id: GH-6455

- Added the new built-in type `t:dynamic/0` introduced in EEP-61, improving
  support for gradual type checkers.

  Own Id: OTP-18522

- Optimize `maps:merge/2` memory consumption for small maps (<33 keys) by
  reusing key tuples or entire maps if the result map has the same number of
  keys as any of the argument maps.

  Own Id: OTP-18523 Aux Id: PR-7004

- Optimize `maps:merge/2` memory consumption further for small maps by mutating
  2nd map to use literal key tuple of 1st map if both have the same keys.

  Own Id: OTP-18524 Aux Id: PR-7004, OTP-18523

- [`ceil/1`](`ceil/1`), [`floor/1`](`floor/1`),
  [`is_bitstring/1`](`is_bitstring/1`), [`is_boolean/1`](`is_boolean/1`),
  [`is_function/2`](`is_function/2`), and [`tuple_size/1`](`tuple_size/1`) can
  now be used in match specifications.

  Own Id: OTP-18526 Aux Id: GH-7045

- Allow IPv6 addresses as host in `http` packets decoded by
  `erlang:decode_packet/3` and `gen_tcp` packet option. The IPv6 address should
  be enclosed within `[]` according to RFC2732.

  Own Id: OTP-18540 Aux Id: PR-6900

- Removed the experimental `erts_alloc_config` module. It no longer produced
  good configurations and cannot be fixed in a reasonably backwards compatible
  manner. It has since OTP 25 been deprecated and scheduled for removal in
  OTP 26.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18549 Aux Id: PR-7105

- ERTS internal thread names have been changed. All threads created by ERTS now
  have a prefix `erts_` followed by a type name potentially followed by an
  integer index. For example, normal schedulers are now named `erts_sched_<IX>`,
  dirty CPU schedulers `erts_dcpus_<IX>`, and dirty IO schedulers
  `erts_dios_<IX>`. NIF and driver thread names are truncated at 15 characters
  regardless of whether the underlying platform support more characters or not.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18552 Aux Id: PR-6973

- Further robustify implementation of large maps (> 32 keys). Keys that happen
  to have same internal 32-bit hash values are now put in collision nodes which
  are traversed with linear search. This removes the demand for the internal
  hash function when salted to eventually produce different hashes for all
  possible pairs of unequal terms.

  Own Id: OTP-18569

- In Erlang/OTP 27, `0.0` will no longer be considered to be exactly equal to
  `-0.0`. See
  [Upcoming Potential Incompatibilities](`e:general_info:upcoming_incompatibilities.md#float_matching`).

  Own Id: OTP-18574

## Erts 13.2.2.10

### Fixed Bugs and Malfunctions

* `'DOWN'` messages originating from a monitored port, contained the atom `process` instead of the atom `port` as the third element when the exit reason was not an immediate term.

  Own Id: OTP-19123 Aux Id: GH-8484, PR-8546
* When [*no time warp mode*](time_correction.md#no-time-warp-mode) was enabled, a smaller Erlang monotonic time could be read than a previously read time, i.e., breaking the monotonic property. The runtime system will abort when detecting an issue like this since OTP 24.3.4.17 and OTP 25.0.

  Up until OTP 25 *no time warp mode* is the default. As of OTP 26 [*multi time warp mode*](time_correction.md#multi-time-warp-mode) is the default.

  Own Id: OTP-19147 Aux Id: ERIERL-1043, ERIERL-1106, PR-8619
* A scheduler thread could get stuck when deleting a memory allocator carrier when adjacent carriers were deleted and/or inserted simultaneously by other schedulers. This in turn could cause the other schedulers to get stuck as well.

  Own Id: OTP-19154 Aux Id: GH-8613, PR-8627

## Erts 13.2.2.9

### Fixed Bugs and Malfunctions

* ETS functions did not properly handle keys containing maps, sometimes matching too many or too few objects.

  Own Id: OTP-19070 Aux Id: GH-8385
* Fix CPU quota determination for cgroups.

  The bug was introduced through OTP-18999.

  Own Id: OTP-19071 Aux Id: OTP-18999 PR-8380

## Erts 13.2.2.8

### Fixed Bugs and Malfunctions

* Fixed CPU quota determination for cgroup version 2

  Own Id: OTP-18999 Aux Id: GH-7928
* Fix faulty reduction counting in exiting process which could cause it to do unnecessary yielding.

  Own Id: OTP-19014
* Calling `erlang:trace/3` with first argument one of `ports`, `processes`, `existing_ports`, `existing_processes`, `existing` or `all`, could cause emulator crash if a dirty scheduler was executing a simultaneous trace action.

  Own Id: OTP-19034
* Fixed an integer overflow when the monotonic time unit reported by the operating system was greater than 10 and lower than 100 microseconds.

  Own Id: OTP-19036 Aux Id: GH-8186
* When a traced process executing on a dirty scheduler received an exit signal, the dirty scheduler could use the wrong thread specific data which could lead to a crash.

  Own Id: OTP-19043 Aux Id: PR-8342
* Fixed a more or less harmless bug that caused time correction of Erlang monotonic time to become slightly off on Windows platforms when `QueryPerformanceCounter()` was used as OS monotonic time source.

  `erlang:system_info(os_monotonic_time_source)` now also returns information about *used resolution* which not always corresponds to the resolution of the OS monotonic time source.

  Own Id: OTP-19048 Aux Id: PR-8343

### Improvements and New Features

* Checks for monotonicity of monotonic time have been improved so that Erlang and OS monotonic time are checked separately.

  A new `configure` argument `--enable-ensure-os-monotonic-time` has also been added. It enables functionality ensuring the monotonicity of monotonic timestamps delivered by the OS. When a non-monotonic timestamp is detected, it will be replaced by the last delivered monotonic timestamp before being used by Erlang's time functionality. Note that you do *not* want to enable this unless the OS monotonic time source on the system fails to produce monotonic timestamps. This since ensuring the monotonicity of OS monotonic timestamps will hurt scalability and performance of the system.

  Own Id: OTP-19044 Aux Id: ERIERL-1043, PR-8342
* For severe errors, when the \`socket\` module terminates the Erlang VM, now an erl_crash.dump is produced, to facilitate post mortem debugging.

  Own Id: OTP-19058

## Erts 13.2.2.7

### Fixed Bugs and Malfunctions

* Fix heap corruption bug that could cause runaway memory consumption due to circular offheap list at process exit. Other symptoms may also be possible. Bug exists since OTP 25.0.

  Own Id: OTP-18971 Aux Id: GH-8044
* The code server could be hanging if a module with `on_load` function was loaded at the same time as another module was purged using `erlang:purge_module` directly.

  Own Id: OTP-19006
* A process optimized for parallel signal delivery could under some circumstances lose wakeup information. That is, the processes was not woken up to take care of the signal, so the signal would not be taken care of until the process was woken by another signal. Only processes configured with [`message_queue_data`](`m:erlang#process_flag_message_queue_data`) set to `off_heap` utilize this optimization.

  Own Id: OTP-19008 Aux Id: GH-8119, PR-8201
* Fix bug in `re:run/3` where if an invalid UTF-8 subject was given, re:run could get stuck in an infinite loop. Bug was introduced in Erlang/OTP 22.1.

  Own Id: OTP-19015 Aux Id: ERIERL-682

## Erts 13.2.2.6

### Fixed Bugs and Malfunctions

* 32-bit runtime systems on most Unix like platforms could crash if a BIF timer was set with a huge timeout of more than 68 years into the future. In order for the crash to occur, the huge timer (at a later time than when it was set) had to become the nearest active timer set on the specific scheduler on which it was set. This could not happen on a system with only one scheduler since there would always be shorter timers in the system.

  Setting a timer larger than 49 days on Windows could under rare circumstances cause the timeout to be delayed.

  Own Id: OTP-18911 Aux Id: ERIERL-1023, PR-7983
* Garbage collection of a process on a dirty scheduler could collide with signal handling for that process causing a crash of the runtime system. This bug was introduced in OTP 25.3.2.8 and OTP 26.2.

  Own Id: OTP-18957 Aux Id: GH-8051, PR-8088, OTP-18841

## Erts 13.2.2.5

### Fixed Bugs and Malfunctions

- Fix faulty debug assert when page size is larger than 16kb, like on PowerPC.
  Did crash debug VM directly at start.

  Own Id: OTP-18802

- A process with `message_queue_data` configured as `off_heap` could end up in
  an inconsistent state when being `receive` traced, inspected using
  [`process_info/2`](`process_info/2`) with the `message_queue_len` item, or
  inspected using the break menu (CTRL-C). When it ended up in this inconsistent
  state, it was not enqueued into a run queue even though it was set in a
  runnable state.This also effected signals being sent to the process after it
  had gotten into this inconsistent state, in such a way that it was from this
  point not possible to communicate with it.

  Own Id: OTP-18838 Aux Id: PR-7822, GH-7801

- A race occurring when a process was selected for dirty execution
  simultaneously as it was scheduled for handling a signal could cause the
  process to end up in an inconsistent state. When it ended up in this
  inconsistent state, it was not enqueued into a run queue even though it was
  set in a runnable state. This also effected signals being sent to the process
  after it had gotten into this inconsistent state, in such a way that it was
  from this point not possible to communicate with it.

  Own Id: OTP-18839 Aux Id: PR-7822, GH-7801

- When a process had to to wait in the run queue for a long time before being
  selected for dirty execution, it could not receive signals. This caused
  inspection of such a process, for example using
  [`process_info/2`](`process_info/2`), to take a long time.

  This issue was introduced in OTP 25.3.2.6 and 26.1 when fixing an issue where
  a constant flow of signals prevented a process from being able to execute
  dirty.

  Own Id: OTP-18841 Aux Id: PR-7822, GH-7801, OTP-18737

- Fixed a bug in the JIT that miscompiled large `select_val` instructions.

  Own Id: OTP-18842

- On OTP 24 and OTP 25, incoming distributed messages larger than 64 KiB sent
  using an alias leaked memory if the alias had been removed prior to entering
  the node. This issue was not present on OTP 26.

  Incoming distributed messages larger than 64 KiB sent using an alias which had
  been removed on the receiving node could crash the node. This crash was quite
  unlikely on OTP 24 and OTP 25, but very likely on OTP 26.

  `'DOWN'` signals with exit reason larger than 64 KiB directed towards a
  process on a node with a not matching creation leaked memory on the receiving
  node. Such signals should however be very rare.

  Own Id: OTP-18885 Aux Id: GH-7834, GH-7890, PR-7915

- Removed unnecessary PCRE source tar-ball.

  Own Id: OTP-18902

### Improvements and New Features

- Removed unnecessary regexp library used when generating yielding BIFs.

  Own Id: OTP-18830 Aux Id: PR-7823

- Replaced old md5 implementation with an implementation from OpenSSL.

  Own Id: OTP-18877

- Removed unused `makewhatis` script.

  Own Id: OTP-18899

## Erts 13.2.2.4

### Fixed Bugs and Malfunctions

- If the external term format encoding of an argument list part of a distributed
  spawn operation was faulty, the newly spawned remote process could misbehave.
  The misbehavior included hanging or interpret an incoming message as an
  argument list to use. This was very unlikely to happen unless using an
  alternate implementation of the distribution protocol which made a faulty
  encoding of the argument list. The child process will now detect this error
  and terminate before executing the user specified code.

  Own Id: OTP-18790 Aux Id: PR-7742

- On Apple Silicon Macs running macOS Sonoma, the runtime system with the JIT
  enabled would crash. Therefore, the `configure` script will by default now
  disable the JIT on Macs with Apple Silicon. When building for earlier versions
  of macOS, the JIT can be explicitly enabled by passing `--enable-jit` to the
  `configure` script.

  Own Id: OTP-18792 Aux Id: GH-7687

- Fix bugs where if the body of a matchspec would return a map with a variable
  ('$1', '$\_' etc) as one of the keys or values and the variable was not an
  immidiate, the term would not be copied to the receiving processes heap. This
  would later corrupt the term in the table as the GC could place move markers
  in it, which in turn would cause the VM to crash.

  Bug has been present for since OTP 17.0.

  Own Id: OTP-18797 Aux Id: PR-7712 GH-7683

## Erts 13.2.2.3

### Fixed Bugs and Malfunctions

- A process optimized for parallel signal delivery could under some
  circumstances lose wakeup information. That is, the processes was not woken up
  to take care of the signal, so the signal would not be taken care of until the
  process was woken by another signal. Only processes configured with
  [`message_queue_data`](`m:erlang#process_flag_message_queue_data`) set to
  `off_heap` utilize this optimization.

  Own Id: OTP-18647 Aux Id: PR-7595

- Delivery time of message signals to a process not executing any `receive`
  expressions could become very long, potentially infinite. For example, a
  process checking for messages using
  [`process_info(self(), message_queue_len)`](`process_info/2`) or
  [`process_info(self(), messages)`](`process_info/2`) and avoiding to execute a
  `receive` expression matching on messages could be very slow in detecting new
  messages. Note that you are still discouraged from using `process_info()` this
  way. A process that wants to check if there are messages available to handle
  should execute a `receive` expression matching on messages.

  Own Id: OTP-18706 Aux Id: GH-7413, PR-7595, ERIERL-979

- Fix bug causing "magic" references in a `compressed` ETS table to not keep the
  referred object alive. The symptom would be the referred object being garbage
  collected prematurely and the reference appearing stale, not referring to
  anything. Examples of such magically referred objects are `atomics` and NIF
  resources.

  Own Id: OTP-18732 Aux Id: GH-7444, PR-7458

- A constant flow of incoming non-message signals could prevent a process
  needing to execute dirty from doing so.

  Own Id: OTP-18737 Aux Id: PR-7595

- The cleanup operation of not yet delivered signals to a terminated process
  yielded excessively.

  Own Id: OTP-18752 Aux Id: PR-7633

## Erts 13.2.2.2

### Fixed Bugs and Malfunctions

- Multiple socket:accept calls issue. When making multiple accept calls, only
  the last call is active.

  Own Id: OTP-18635 Aux Id: #7328

- The following functions are now much faster when given a long list or binary:

  - erlang:list_to_integer/1
  - erlang:binary_to_integer/1
  - erlang:binary_to_integer/2
  - erlang:list_to_integer/2
  - string:to_integer/1

  Own Id: OTP-18659 Aux Id: PR-7426

## Erts 13.2.2.1

### Fixed Bugs and Malfunctions

- Fixed a crash during tracing on certain platforms that cannot use the machine
  stack for Erlang code (mainly OpenBSD and Linux with musl).

  Own Id: OTP-18561

- Constructing a binary segment not aligned with a byte boundary, with a size
  not fitting in 31 bits, and with a value not fitting in a 64-bit word could
  crash the runtime system.

  Own Id: OTP-18597

### Improvements and New Features

- Further robustify implementation of large maps (> 32 keys). Keys that happen
  to have same internal 32-bit hash values are now put in collision nodes which
  are traversed with linear search. This removes the demand for the internal
  hash function when salted to eventually produce different hashes for all
  possible pairs of unequal terms.

  Own Id: OTP-18569

## Erts 13.2.2

### Fixed Bugs and Malfunctions

- If a runtime system which was starting the distribution already had existing
  pids, ports, or references referring to a node with the same nodename/creation
  pair that the runtime system was about to use, these already existing pids,
  ports, or references would not work as expected in various situations after
  the node had gone alive. This could only occur if the runtime system was
  communicated such pids, ports, or references prior to the distribution was
  started. That is, it was extremely unlikely to happen unless the distribution
  was started dynamically and was even then very unlikely to happen. The runtime
  system now checks for already existing pids, ports, and references with the
  same nodename/creation pair that it is about to use. If such are found another
  creation will be chosen in order to avoid these issues.

  Own Id: OTP-18570 Aux Id: PR-7190

## Erts 13.2.1

### Fixed Bugs and Malfunctions

- Fixed a bug in the loader that prevented certain modules compiled with
  `no_ssa_opt` from being loaded.

  Own Id: OTP-18519 Aux Id: GH-7024

- Implementations of the [`call()`](driver_entry.md#call) driver callback that
  returned a faulty encoded result could cause a memory leak and could cause
  invalid data on the heap of the processes calling `erlang:port_call/3`.

  Own Id: OTP-18525 Aux Id: PR-7049

- Fixed a memory corruption issue when upgrading code. The bug was introduced in
  `OTP 25.3`

  Own Id: OTP-18553

- Fixed configure tests for a few ARM-specific instructions, which prevented the
  emulator from being built on some platforms.

  Own Id: OTP-18554

- Aliases created in combination with a monitor using the
  `{alias, explicit_unalias}` option stopped working from remote nodes when a
  `'DOWN'` signal had been received due to the monitor or if the monitor was
  removed using the `erlang:demonitor()` BIF.

  This bug was introduced in OTP 24.3.4.10 and OTP 25.3.

  Own Id: OTP-18557 Aux Id: PR-7131, OTP-18496

- In rare circumstances, bit syntax matching of an invalid code point for a
  `utf32` would crash the runtime system.

  Own Id: OTP-18560

- Building the runtime system failed when native atomic support was missing.
  Note that execution on such systems have only been rudimentary tested.

  Own Id: OTP-18563 Aux Id: GH-7114, PR-7159

## Erts 13.2

### Fixed Bugs and Malfunctions

- Fixed a bug on Windows where `file:read_file_info/1` would fail for files with
  corrupt metadata.

  Own Id: OTP-18348 Aux Id: GH-6356

- Fix [`process_info(_, binary)`](`process_info/2`) to again include "writable
  binaries" which were lost in OTP-25.0. Writable binaries are an optimization
  used when binaries are appended upon in a loop.

  Own Id: OTP-18373 Aux Id: PR-6574, GH-6573

- Fix rare race when receiving fragmented messages on a terminating connection.
  Could potentially cause memory leaks as well as double free crashes. Bug
  exists since OTP 22.0.

  Own Id: OTP-18382 Aux Id: PR-6585

- Fixed bug that could maybe cause problems when a file descriptor number is
  closed by a linked in driver and then opened (reused) and passed to
  `enif_select` by a NIF. No actual symptoms seen, only failed internal
  assertions in debug build.

  Own Id: OTP-18391

- The runtime system could crash when tracing a process executing on a dirty
  scheduler.

  Own Id: OTP-18398 Aux Id: PR-6495, GH-6448, GH-5984

- In the binary syntax, attempting to match out integers with size exceeding 2
  GiB could crash the runtime system.

  Own Id: OTP-18406 Aux Id: GH-6701

- Fixed edge case in floating-point negation where `A = 0.0, B = -A` did not
  produce `B = -0.0` on x86_64 JIT.

  Own Id: OTP-18411 Aux Id: GH-6717

- Fixed an issue in the JIT that could crash the emulator on some platforms.

  Own Id: OTP-18418

- Added meta data to the windows installer.

  Own Id: OTP-18429 Aux Id: PR-6587 GH-4232 GH-6537

- Fixed ETS insertion order into `bag` and `duplicate_bag` of tuples with
  identical keys when passed in a list to `ets:insert/2`. The insert order has
  been head-to-tail but was accidentally changed in OTP 23.0. For `bag` it was
  reverted (tail-to-head), while for `duplicate_bag` it was sometimes reverted
  depending on the length of the list and number of "reductions" left for the
  calling process.

  This fix changes the insert order of `ets:insert/2` back to always be
  head-to-tail of the list argument.

  Own Id: OTP-18434 Aux Id: PR-6752

- With the JIT for AArch64 (AMD64), calling `bxor` in with non-integer arguments
  in a guard would crash the runtime system.

  Own Id: OTP-18454 Aux Id: PR-6839

- Fix bug regarding process flag `max_heap_size`. Could cause strange behavior
  when a process was killed due to exceeding the limit.

  Own Id: OTP-18457 Aux Id: PR-6816

- Fixed binary comprehensions to be similar to other creation of binary data
  with respect to its contribution of triggering garbage collection.

  Own Id: OTP-18458

- In rare circumstances, when a process exceeded its allowed heap size set by
  option `max_heap_size`, it would not be killed as it should be, but instead
  enter a kind of zombie state it would never get out of.

  Own Id: OTP-18463 Aux Id: PR-6858

- Instead of crashing, the [`list_to_integer/1`](`list_to_integer/1`) and
  [`list_to_integer/2`](`list_to_integer/2`) BIFs now raise the `system_limit`
  exception for overlong lists that can't be converted to integers. Similarly,
  the `string:to_integer/1` BIF now returns `{error,system_limit}` for overlong
  lists.

  Own Id: OTP-18475 Aux Id: PR-6897

- Active process aliases of a process at its termination leaked memory.

  Own Id: OTP-18496 Aux Id: GH-6947, PR-6953

### Improvements and New Features

- Support for fully asynchronous distributed signaling where send operations
  _never_ block. This functionality is by default disabled and can be enabled
  per process. For more information see the documentation of
  [`process_flag(async_dist, Bool)`](`m:erlang#process_flag_async_dist`).

  Own Id: OTP-18374 Aux Id: PR-6632

- Added the `+JPperf no_fp` option to explicitly disable Erlang frame pointers
  otherwise added when using the `+JPperf map` option.

  Own Id: OTP-18426

## Erts 13.1.5

### Fixed Bugs and Malfunctions

- Comparisons between small numbers and pids or ports would in some edge cases
  say that the number was greater than the pid or port, violating the term
  order.

  Own Id: OTP-18415

- [`process_info(Pid, status)`](`process_info/2`) when `Pid /= self()` could
  return an erroneous result.

  Own Id: OTP-18421 Aux Id: PR-6806

## Erts 13.1.4

### Fixed Bugs and Malfunctions

- Fixed a bug in selective receive optimization that could crash 32-bit
  emulators.

  Own Id: OTP-18383 Aux Id: ERIERL-905

- A race condition which was very rarely triggered could cause the signal queue
  of a process to become inconsistent causing the runtime system to crash.

  Own Id: OTP-18388 Aux Id: OTP-17462, PR-6662

## Erts 13.1.3

### Fixed Bugs and Malfunctions

- Fix perf/gdb JIT symbols to not contain `CodeInfoPrologue` for the JIT
  internal module erts_beamasm.

  Own Id: OTP-18256 Aux Id: PR-6316

- Fixed minor memory leaks.

  Own Id: OTP-18281 Aux Id: PR-4840

- Fix bugs in `ets:insert` and `ets:insert_new` when called with a list of
  tuples to insert while a concurrent process either deletes or renames the
  table. The table deletion could be done with `ets:delete/1` or be caused by
  termination of the table owning process.

  Symptoms are either VM crash or strange incorrect behavior from the insert
  operation. The risk of triggering the bugs increases with the length of the
  list of tuple to insert. Bugs exist since OTP 23.0.

  Own Id: OTP-18284 Aux Id: PR-6305

- Boost execution of scheduled thread progress jobs. This to prevent memory
  exhaustion in extremely rapid allocation/deallocation scenarios, such as
  repeated ETS table creations/deletions.

  Own Id: OTP-18294 Aux Id: PR-6390

- Fix segv crash during crash dumping an ETS table doing
  `ets:delete_all_objects`.

  Own Id: OTP-18295

- Spec for function net:if_names/0 incorrect

  Own Id: OTP-18296 Aux Id: OTP-16464

- Fix bug in `binary_to_term` decoding a binary term 2Gbyte or larger.

  Own Id: OTP-18306 Aux Id: GH-6393, PR-6401

- Documentation of `erlang:module_loaded/1` has been adjusted:

  - It did not previously say that the BIF only returns `true` for modules
    loaded as _current code_.
  - The warning claiming that the BIF should only be used by the code server has
    been removed.

  Own Id: OTP-18313 Aux Id: PR-6456

- Fix [`list_to_atom/1`](`list_to_atom/1`) for negative code points. Could
  either return with a positive code point or fail with an incorrect exception.

  Own Id: OTP-18321

- Fix rare bug causing VM crash when sending to a pid of a spawning process
  returned from `erlang:processes/0`.

  Only seen when provoked by system process literal_area_collector, triggered by
  a module purge operation, on a VM started with +Meamin (no customized
  allocators).

  Own Id: OTP-18322 Aux Id: PR-6479

- gen_udp:open/2 with option(s) add_membership or drop_membership would drop
  earlier options.

  Own Id: OTP-18323 Aux Id: #6476

- The `inet:setopts/2` `{reuseaddr, true}` option will now be ignored on Windows
  unless the socket is an UDP socket. For more information see the documentation
  of the `reuseaddr` option part of the documentation of `inet:setopts/2`.

  Prior to OTP 25 the `{reuseaddr, true}` option was ignored for all sockets on
  Windows, but as of OTP 25.0 this was changed so that it was not ignored for
  any sockets.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18324 Aux Id: GH-6461, PR-6481

- Fix bug in `binary_to_term` decoding a list of length 1G or longer.

  Own Id: OTP-18328 Aux Id: GH-6439, PR-6440

- Fix bug in `binary_to_term` (and distributed receive) when decoding a large
  map (>32 keys) with unsorted small maps (<= 32) as keys of the large map.

  This was only a problem if the term was encoded by `erl_interface`,
  `jinterface` or otherwise, as the VM itself always encodes small maps with
  sorted keys.

  The "binary_to_term" would appear as successful but the created large map was
  internally inconsistent. The smaller key-maps could not be found with maps:get
  and friends. Other operations such as map compare and merge could probably
  also give incorrect results.

  Own Id: OTP-18343 Aux Id: GH-6496

- Fix Windows bug in `open_port({spawn, Command}, ..)` when `Command` is found
  via the OS search PATH and that directory path contains white spaces. The port
  program would start but the command line arguments to it could be incorrect.

  Own Id: OTP-18345 Aux Id: GH-6387, PR-6396

## Erts 13.1.2.1

### Fixed Bugs and Malfunctions

- Fixed a bug in selective receive optimization that could crash 32-bit
  emulators.

  Own Id: OTP-18383 Aux Id: ERIERL-905

- A race condition which was very rarely triggered could cause the signal queue
  of a process to become inconsistent causing the runtime system to crash.

  Own Id: OTP-18388 Aux Id: OTP-17462, PR-6662

## Erts 13.1.2

### Fixed Bugs and Malfunctions

- Add abandon carrier free utilization limit (`+Muacful`) option to erts_alloc.
  This option allows the user to mark unused segments in a memory carrier as
  re-useable by the OS if needed.

  This functionality was a non-configurable default before Erlang/OTP 25, but
  removed due to performance issues.

  Own Id: OTP-18290 Aux Id: ERIERL-866 PR-6378

## Erts 13.1.1

### Fixed Bugs and Malfunctions

- Listen sockets created with the socket module, leaked (erlang-) monitors.

  Own Id: OTP-18240 Aux Id: #6285

- [Notifications about available distribution data](`erlang:dist_ctrl_get_data_notification/1`)
  sent to distribution controller processes could be lost. Distribution
  controller processes can be used when implementing an alternative distribution
  carrier. The default distribution over tcp was not effected and the bug was
  also not present on x86/x86_64 platforms.

  Own Id: OTP-18258 Aux Id: GH-6309, PR-6324

## Erts 13.1

### Fixed Bugs and Malfunctions

- Fixed inconsistency bugs in `m:global` due to `nodeup`/`nodedown` messages not
  being delivered before/after traffic over connections. Also fixed various
  other inconsistency bugs and deadlocks in both `m:global_group` and `global`.

  As building blocks for these fixes, a new BIF `erlang:nodes/2` has been
  introduced and `net_kernel:monitor_nodes/2` has been extended.

  The [`-hidden`](erl_cmd.md#hidden) and
  [`-connect_all`](erl_cmd.md#connect_all) command line arguments did not work
  if multiple instances were present on the command line which has been fixed.
  The new kernel parameter [`connect_all`](`e:kernel:kernel_app.md#connect_all`)
  has also been introduced in order to replace the `-connect_all` command line
  argument.

  Own Id: OTP-17934 Aux Id: PR-6007

- Fixed IPv6 multicast_if and membership socket options.

  Own Id: OTP-18091 Aux Id: #5789

- Accept funs (NEW_FUN_EXT) with incorrectly encoded size field. This is a
  workaround for a bug (OTP-18104) existing in OTP 23 and 24 that could cause
  incorrect size fields in certain cases. The emulator does not use the decoded
  size field, but `erl_interface` still does and is not helped by this
  workaround.

  Own Id: OTP-18093 Aux Id: OTP-18104, PR-5987

- Fixed issue with inet:getifaddrs hanging on pure IPv6 Windows

  Own Id: OTP-18102 Aux Id: #5904

- Fix faulty distribution encoding of terms with either

  - a fun with bit-string or export-fun in its environment when encoded toward a
    not yet established (pending) connection
  - or a fun with a binary/bitstring, in its environment, referring to an
    off-heap binary (larger than 64 bytes).

  The symptom could be failed decoding on the receiving side leading to aborted
  connection. Fix OTP-18093 is a workaround for theses bugs that makes the VM
  accepts such faulty encoded funs.

  The first encoding bug toward pending connection exists only in OTP 23 and 24,
  but the second one exists also on OTP 25.

  Own Id: OTP-18104 Aux Id: OTP-18093

- Fixed emulator crash that could happen during crashdump generation of ETS
  tables with options `ordered_set` and `{write_concurrency,true}`.

  Own Id: OTP-18144 Aux Id: GH-5981

- Retrieval of monotonic and system clock resolution on MacOS could cause a
  crash and/or erroneous results.

  Own Id: OTP-18160 Aux Id: PR-6103

- Fix bug where the max allowed size of `erl +hmax` was lower than what was
  allowed by `process_flag`.

  Own Id: OTP-18161 Aux Id: PR-6081

- On computers with ARM64 (AArch64) processors, the JIT could generate incorrect
  code when more than 4095 bits were skipped at the tail end of a binary match.

  Own Id: OTP-18201

- In rare circumstances, an [`is_binary/1`](`is_binary/1`) guard test could
  succeed when given a large integer.

  Own Id: OTP-18216 Aux Id: GH-6239, PR-6240

- Fix bug causing `ets:info` (and sometimes `ets:whereis`) to return 'undefined'
  for an existing table if a concurrent process were doing `ets:insert` with a
  long list on the same table.

  Own Id: OTP-18218 Aux Id: ERIERL-855

- Fix writing and reading of more than 2 GB in a single read/write operation on
  macOS. Before this fix attempting to read/write more than 2GB would result in
  `{error,einval}`.

  Own Id: OTP-18222 Aux Id: PR-6248 GH-6242

- Fix bug sometimes causing emulator crash at node shutdown when there are
  pending connections. Only seen when running duel distribution protocols,
  inet_drv and inet_tls_dist.

  Own Id: OTP-18243 Aux Id: GH-6247, PR-6258

### Improvements and New Features

- Yield when adjusting large process message queues due to

  - copying terms from a literal area prior to removing the literal area.
  - changing the `message_queue_data` state from `on_heap` to `off_heap`.

  The message queue adjustment work will now be interleaved with all other types
  of work that processes have to do, even other message queue adjustment work.

  Own Id: OTP-17340 Aux Id: PR-6133

- Add rudimentary debug feature (option) for the inet-driver based sockets, such
  as gen_tcp and gen_udp.

  Own Id: OTP-18032

- Introduced the `hidden` and `dist_listen` options to `net_kernel:start/2`.

  Also documented the [`-dist_listen`](erl_cmd.md#dist_listen) command line
  argument which was erroneously documented as a `kernel` parameter and not as a
  command line argument.

  Own Id: OTP-18107 Aux Id: PR-6009

- New documentation chapter "Debugging NIFs and Port Drivers" under
  Interoperability Tutorial.

  Own Id: OTP-18109

- Add new API function erl_features:configurable/0

  Own Id: OTP-18199 Aux Id: PR-5790

## Erts 13.0.4

### Fixed Bugs and Malfunctions

- The [`monitor/3`](`erlang:monitor/3`) BIF did not apply options to the created
  monitor if the target process or port did not exist. That is, the
  corresponding down message would get a `` `DOWN` `` tag even if a custom tag
  had been set, and the returned reference was not an alias even if the `alias`
  option had been passed.

  Own Id: OTP-18190 Aux Id: GH-6185, PR-6209

- The `erlang:monotonic_time/1`, `erlang:system_time/1`, `erlang:time_offset/1`,
  and `os:system_time/1` BIFs erroneously failed when passed the argument
  `native`.

  Own Id: OTP-18197 Aux Id: GH-6165, PR-6213

## Erts 13.0.3

### Fixed Bugs and Malfunctions

- Distributed exit signals could be lost under the following conditions:

  - An exit signal from a parent process to a child process was lost if:

    - the parent process terminated before the spawn request that created the
      child had completed,
    - the spawn request set up a link between parent and child
    - the spawn request was distributed, and
    - the exit reason was larger than one machine word.

  - Loss of a connection over which a not yet completed spawn request was
    ongoing could cause loss of exit signals. Such loss of exit signals was very
    rare. Besides the above described connection loss also the following
    conditions had to be satisfied:

    - The spawn request that was interrupted by the connection loss also had to
      set up a link between the parent process and the child process.
    - The parent process that issued the spawn request also had to be
      terminating while the spawn request was interrupted by the connection
      loss.
    - The same parent process also had to have made other spawn requests to
      other nodes than to the node to which the connection was lost.
    - These spawn requests to the other nodes also had to set up links.
    - These spawn requests to the other nodes also had to be not yet completed
      at the time of the connection loss. That is, the spawn reply from the
      child process had not yet reached the parent process.

    If all the conditions above were met, exit signals to the children spawned
    due to the above described spawn requests to other nodes _could_ be lost.

    The above bug also caused a significant memory leak when it was triggered
    since the destruction of the parent process never completed.

  Own Id: OTP-18164 Aux Id: PR-6114

- A race could cause [`process_info(Pid, message_queue_len)`](`process_info/2`)
  on other processes to return invalid results.

  Own Id: OTP-18169 Aux Id: PR-6134

- Fixed reduction counting for handling process system tasks.

  Own Id: OTP-18170 Aux Id: PR-6135

- Priority elevation of terminating processes did not work which could cause
  execution of such processes to be delayed.

  Own Id: OTP-18175 Aux Id: PR-6142

- An unlink operation made by a process that terminated before the unlink
  operation completed, i.e., before it had received an unlink-ack signal from
  the linked process, caused an exit signal to erroneously be sent from the
  terminating process to the process being unlinked. This exit signal would most
  often be ignored by the receiver, but if the receiver of the exit signal
  concurrently set up a new link, it could receive the exit signal with the
  actual exit reason of the terminating process instead of a `noproc` exit
  reason. It is however very hard to detect that this has happened and has no
  obvious negative consequences, so it should be considered harmless.

  A distributed unlink-ack signal received by a terminating process was also not
  properly removed which could cause a minor memory leak.

  Own Id: OTP-18177 Aux Id: PR-6150

## Erts 13.0.2

### Fixed Bugs and Malfunctions

- On computers with the ARM64 (AArch64) architecture (such as Apple Silicon
  Macs) a `rem` expression followed by a `div` expression with the same operands
  could evaluate to the wrong result if the result of the `rem` expression was
  unused.

  Own Id: OTP-18143

## Erts 13.0.1

### Fixed Bugs and Malfunctions

- A `spawn_reply` signal from a remote node could be delayed and be delivered
  after other signals from the newly spawned process.

  When this bug triggered, the connection to the node where the process was
  spawned sometimes could be taken down due to the bug. The following error
  message would then be logged if this happened:
  `Missing 'spawn_reply' signal from the node <RemoteNode> detected by <Pid> on the node <LocalNode>. The node <RemoteNode> probably suffers from the bug with ticket id OTP-17737.`

  This bug only affected processes which had enabled
  [`off_heap` `message_queue_data`](`m:erlang#process_flag_message_queue_data`)
  and parallel reception of signals had been automatically enabled.

  This bug was introduced in OTP 25.0, ERTS version 13.0.

  Own Id: OTP-18105 Aux Id: OTP-16982, PR-6003

- Fixed type spec of `erlang:system_info(dist_ctrl)`.

  Own Id: OTP-18106 Aux Id: PR-5992

- The zlib built in to the runtime system has been updated to version 1.2.12.
  (Note that on most platforms, the platform's own zlib is used.)

  Own Id: OTP-18123 Aux Id: GH-5994

- The `erlang:is_alive()` BIF could return `true` before configured distribution
  service was available. This bug was introduced in OTP 25.0 ERTS version 13.0.

  The `erlang:monitor_node()` and `erlang:monitor()` BIFs could erroneously fail
  even though configured distribution service was available. This occurred if
  these BIFs were called after the distribution had been started using dynamic
  node name assignment but before the name had been assigned.

  Own Id: OTP-18124 Aux Id: OTP-17558, PR-6032

## Erts 13.0

### Fixed Bugs and Malfunctions

- The socket option 'reuseaddr' is _no longer_ ignored on Windows.

  Own Id: OTP-17447 Aux Id: GH-4819

- The growth rate of writable binaries has been adjusted to only increase by 20%
  after 16MB in size. Before this change the size would always double.

  This change may degrade write performance of large binaries.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17569 Aux Id: PR-4793

- Fix reduction counting bug in `re:run` that caused the function to yield too
  frequently when doing `global` matches.

  Own Id: OTP-17661 Aux Id: PR-5165

- Fix spelling mistakes in epmd error messages.

  Own Id: OTP-17758 Aux Id: PR-5391

- Fix bug where the "newshell" would trigger a newline at the column width of
  the terminal, even if the next character to be printed was a newline. This
  would cause the terminal to render two newlines instead of one.

  Own Id: OTP-17779 Aux Id: GH-5403 PR-5599

- Fix the memory value returned from `ets:info(Tid,memory)` when the
  `read_concurrency` option is used.

  Before this fix the memory used by the scheduler specific lock cache lines was
  not counted towards the total. This caused the returned memory usage to be
  very incorrect on systems with many schedulers for tables with man locks.

  Own Id: OTP-17832 Aux Id: PR-5494

- Fix the undocumented `--profile_boot` option to work again.

  Own Id: OTP-17836 Aux Id: PR-5546

- \[socket] Encode of sockaddr has been improved.

  Own Id: OTP-18020

- Fix `erl_child_setup` (the program used by `open_port({spawn,...})` and
  `os:cmd/1`) to better handle partial reads from the Erlang VM.

  Own Id: OTP-18047 Aux Id: PR-5861

- The runtime system would crash when attempting to create more than 33554431
  atoms.

  Own Id: OTP-18068 Aux Id: GH-5903

### Improvements and New Features

- Users can now configure ETS tables with the `{write_concurrency, auto}`
  option. This option forces tables to automatically change the number of locks
  that are used at run-time depending on how much concurrency is detected. The
  `{decentralized_counters, true}` option is enabled by default when
  `{write_concurrency, auto}` is active.

  Benchmark results comparing this option with the other ETS optimization
  options are available here:

  https://erlang.org/bench/ets_bench_result_lock_config.html

  Own Id: OTP-15991 Aux Id: PR-5208

- The net module now works on Windows.

  Own Id: OTP-16464

- To enable more optimizations, BEAM files compiled with OTP 21 and earlier
  cannot be loaded in OTP 25.

  Own Id: OTP-16702

- Optimize minor garbage collection for processes with large number of binaries,
  funs and/or external pids/ports/refs. This is a continuation of the
  optimization (OTP-17602) released in OTP-24.1.

  Own Id: OTP-16852 Aux Id: ERL-1347, PR-5195

- The signal queue of a process with message_queue_data=off_heap\* has been
  optimized to allow parallel reception of signals from multiple processes.

  This is possible to do as Erlang only guarantees that signals (i.e., message
  signals and non-message signals) sent from a single process to another process
  are ordered in send order. However, there are no ordering guarantees for
  signals sent from different processes to a particular process. Therefore,
  several processes can send signals in parallel to a specific process without
  synchronizing with each other. However, such signal sending was previously
  always serialized as the senders had to acquire the lock for the outer signal
  queue of the receiving process. This parallel signal sending optimization
  yields much better scalability for signal sending than what was previously
  possible, see https://erlang.org/bench/sigq_bench_result.html for benchmark
  results.

  \* Information about how to enable the message_queue_data=off_heap setting can
  be found in the documentation of the function erlang:process_flag/2.

  Own Id: OTP-16982 Aux Id: PR-5020

- The JIT now works for 64-bit ARM processors.

  Own Id: OTP-17119 Aux Id: PR-4869

- Added support for the compile attribute `-nifs()` to empower compiler and
  loader with information about which functions may be overridden as NIFs by
  `erlang:load_nif/2`. It is recommended to use this attribute in all modules
  that load NIF libraries.

  Own Id: OTP-17151 Aux Id: ERIERL-590, PR-5479

- A test case has been added to the otp_SUITE that test that the dependency
  versions for OTP's applications are correct. The test case uses xref to check
  if the used functions are available in the specified dependency versions. The
  test case depends on the Erlang/OTP team's testing infrastructure and will be
  skipped if its dependencies are not met.

  Own Id: OTP-17224

- An Erlang installation directory is now relocatable on the file system given
  that the paths in the installation's `RELEASES` file are paths that are
  relative to the installations root directory. The
  `` `release_handler:create_RELEASES/4 `` function can generate a `RELEASES`
  file with relative paths if its `RootDir` parameter is set to the empty
  string.

  Own Id: OTP-17304

- The following distribution flags are now mandatory: `DFLAG_BIT_BINARIES`,
  `DFLAG_EXPORT_PTR_TAG`, `DFLAG_MAP_TAGS`, `DFLAG_NEW_FLOATS`, and
  `DFLAG_FUN_TAGS`. This mainly concerns libraries or application that implement
  the distribution protocol themselves.

  Own Id: OTP-17318 Aux Id: PR-4972

- Input for `configure` scripts adapted to `autoconf` 2\.71.

  Own Id: OTP-17414 Aux Id: PR-4967

- When binary construction using the binary syntax fails, the error message
  printed in the shell and by `erl_error:format_exception/3,4` will contain more
  detailed information about what went wrong.

  Own Id: OTP-17504 Aux Id: GH-4971, PR-5281, PR-5752

- The configuration files [`.erlang`](erl_cmd.md),
  [`.erlang.cookie`](`e:system:distributed.md`) and
  [`.erlang.crypt`](`m:beam_lib#module-erlang-crypt`) can now be located in the XDG
  Config Home directory.

  See the documentation for each file and `filename:basedir/2` for more details.

  Own Id: OTP-17554 Aux Id: GH-5016 PR-5408 OTP-17821

- Make [`byte_size/1`](`byte_size/1`) and `binary_part/2/3` callable from match
  specs (in ETS and tracing).

  Own Id: OTP-17555 Aux Id: PR-5027

- Dynamic node name improvements: `erlang:is_alive/0` changed to return true for
  pending dynamic node name and new function `net_kernel:get_state/0`.

  Own Id: OTP-17558 Aux Id: OTP-17538, PR-5111, GH-5402

- A new option called `short` has been added to the functions
  `erlang:float_to_list` and `erlang:float_to_binary`. This option creates the
  shortest correctly rounded string representation of the given float that can
  be converted back to the same float again.

  Own Id: OTP-17562 Aux Id: GH-4492

- The tagged tuple tests and fun-calls have been optimized and are now a little
  bit cheaper than previously.

  These optimizations become possible after making sure that all boxed terms
  have at least one word allocated after the arity word. This has been
  accomplished by letting all empty tuples refer to the same empty tuple literal
  which also reduces memory usage for empty tuples.

  Own Id: OTP-17608

- The signal queue benchmark in parallel_messages_SUITE and the ETS benchmark in
  ets_SUITE have benchmark result visualization HTML pages with "fill-screen"
  buttons to make the graphs bigger. This button did not work as intended
  before. When pressing the button for a graph, the last graph got replaced with
  a bigger version and not the one over the button. This is now fixed.

  Own Id: OTP-17630

- The test case num_bif_SUITE:t_float_to_string previously failed sometimes as
  it assumed a certain rounding of floats printed with sprintf but the rounding
  type is platform specific.

  Own Id: OTP-17636

- Optimize interpreter to create heap binaries of small match contexts if
  possible.

  This optimization was already done in the JIT.

  Own Id: OTP-17660 Aux Id: PR-5164

- Optimize integer multiplication for x86 JIT

  Own Id: OTP-17667 Aux Id: PR-5237

- Removed use of node creation value zero as a wildcard. Also prevent zero from
  being used as creation by `erl_interface` and `jinterface` nodes.

  Own Id: OTP-17682 Aux Id: PR-5347

- Distributed spawn operations now require
  [distributed `spawn_request()`](erl_dist_protocol.md#DFLAG_SPAWN) support.
  Distributed `spawn_request()` was introduced in OTP 23. That is, distributed
  spawn operations against Erlang nodes of releases prior to OTP 23 will fail.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17683 Aux Id: PR-5306

- The Erlang compiler now includes type information in BEAM files, and the JIT
  can now use that type information to do optimizations such as eliminating or
  simplifying type tests.

  Own Id: OTP-17684 Aux Id: PR-5316, PR-5664

- Improved the JIT's support for external tools like `perf` and `gdb`, allowing
  them to show line numbers and even the original Erlang source code when that
  can be found.

  To aid them in finding the source code, the `absolute_path` compiler option
  has been added to embed the absolute file path of a module.

  Own Id: OTP-17685

- Add \[32-bit] to the Erlang shell title row for 32-bit VMs.

  Own Id: OTP-17717 Aux Id: PR-5290

- Instructions for how to build the runtime system for iOS/iPadOS can now be
  found in `HOWTO/INSTALL.md`.

  Own Id: OTP-17728 Aux Id: PR-5284

- Add support for static Elixir NIF modules with non-alphanumeric characters by
  using new macro `STATIC_ERLANG_NIF_LIBNAME`.

  Own Id: OTP-17729 Aux Id: PR-5477

- Add new function `caller_line` to for trace match specifications used by
  erlang:trace_pattern/3.

  This new option puts the line number of the caller into the trace message sent
  to the trace receiver.

  Own Id: OTP-17753 Aux Id: PR-5305 GH-5297

- A new `erl` command line argument `+ssrct` has been introduced which will
  cause the runtime system to skip reading CPU topology information. This reduce
  start up time especially when the CPU topology is large. Reading of CPU
  topology information is now also skipped if a user defined CPU topology is set
  using the `+sct` command line argument.

  Own Id: OTP-17762 Aux Id: GH-5204, PR-5219

- The default time warp mode will change in Erlang/OTP 26. Added a warning about
  this upcoming potential incompatibility to the documentation.

  Own Id: OTP-17772 Aux Id: GH-4965 PR-5644

- The emulator will no longer mark unused memory as discardable (e.g. through
  `madvise(2)`), as it caused more problems than it solved.

  Own Id: OTP-17824

- When a record matching or record update fails, a
  `{badrecord,ExpectedRecordTag}` exception used to be raised. In this release,
  the exception has been changed to `{badrecord,ActualValue}`, where
  `ActualValue` is the actual that was found instead of the expected record.

  Own Id: OTP-17841 Aux Id: PR-5694

- Removed the previously undocumented and unsupported `emem` tool.

  Own Id: OTP-17892 Aux Id: PR-5591

- Remove version number from the default install path on Windows.

  Own Id: OTP-17899 Aux Id: PR-5524

- On Windows apply the limit flag JOB_OBJECT_LIMIT_BREAKAWAY_OK in the Erlang
  service to be able to start a OS child process with a different session
  number.

  Own Id: OTP-17927 Aux Id: PR-5283

- New erl command line option `+IOs`. It can be used to disable scheduler thread
  poll optimization, which has been seen to cause degraded event latency in some
  use cases.

  Own Id: OTP-17945 Aux Id: GH-4759, PR-5809

- An API for multihomed SCTP connect has been added in the guise of
  `gen_sctp:connectx_init/*`

  Own Id: OTP-17951 Aux Id: PR-5656

- \[socket] Add encoding of the field hatype of the type sockaddr_ll (family
  'packet').

  Own Id: OTP-17968 Aux Id: OTP-16464

- A cross compilation issue has been fixed about finding libdlpi during the
  configure phase.

  Own Id: OTP-17985 Aux Id: GH-5728

- [`process_info/2`](`erlang:process_info/2`) now also accepts `parent` as
  argument. When passed, the process identifier of the parent process will be
  returned.

  Own Id: OTP-17999 Aux Id: PR-5768

- Add function attributes to `erl_nif` and `erl_driver` APis to improve compiler
  detection of interface misuse.

  Own Id: OTP-18006 Aux Id: PR-5932

- The exported type `t:erlang:send_destination/0` has been introduced.

  Own Id: OTP-18033 Aux Id: PR-2926, GH-5376

- Building of the C/C++ make dependencies on Windows has been optimized to be a
  lot faster.

  Own Id: OTP-18036 Aux Id: PR-5846

- `file:sync/1` will now use the `F_BARRIERFSYNC` flag when available on Mac OS.

  Own Id: OTP-18038

## Erts 12.3.2.17

### Fixed Bugs and Malfunctions

* The code server could be hanging if a module with `on_load` function was loaded at the same time as another module was purged using `erlang:purge_module` directly.

  Own Id: OTP-19006
* Fix bug in `re:run/3` where if an invalid UTF-8 subject was given, re:run could get stuck in an infinite loop. Bug was introduced in Erlang/OTP 22.1.

  Own Id: OTP-19015 Aux Id: ERIERL-682
* Calling `erlang:trace/3` with first argument one of `ports`, `processes`, `existing_ports`, `existing_processes`, `existing` or `all`, could cause emulator crash if a dirty scheduler was executing a simultaneous trace action.

  Own Id: OTP-19034
* Fixed an integer overflow when the monotonic time unit reported by the operating system was greater than 10 and lower than 100 microseconds.

  Own Id: OTP-19036 Aux Id: GH-8186
* When a traced process executing on a dirty scheduler received an exit signal, the dirty scheduler could use the wrong thread specific data which could lead to a crash.

  Own Id: OTP-19043 Aux Id: PR-8342
* Fixed a more or less harmless bug that caused time correction of Erlang monotonic time to become slightly off on Windows platforms when `QueryPerformanceCounter()` was used as OS monotonic time source.

  `erlang:system_info(os_monotonic_time_source)` now also returns information about *used resolution* which not always corresponds to the resolution of the OS monotonic time source.

  Own Id: OTP-19048 Aux Id: PR-8343

### Improvements and New Features

* Checks for monotonicity of monotonic time have been improved so that Erlang and OS monotonic time are checked separately.

  A new `configure` argument `--enable-ensure-os-monotonic-time` has also been added. It enables functionality ensuring the monotonicity of monotonic timestamps delivered by the OS. When a non-monotonic timestamp is detected, it will be replaced by the last delivered monotonic timestamp before being used by Erlang's time functionality. Note that you do *not* want to enable this unless the OS monotonic time source on the system fails to produce monotonic timestamps. This since ensuring the monotonicity of OS monotonic timestamps will hurt scalability and performance of the system.

  Own Id: OTP-19044 Aux Id: ERIERL-1043, PR-8342

## Erts 12.3.2.16

### Fixed Bugs and Malfunctions

* 32-bit runtime systems on most Unix like platforms could crash if a BIF timer was set with a huge timeout of more than 68 years into the future. In order for the crash to occur, the huge timer (at a later time than when it was set) had to become the nearest active timer set on the specific scheduler on which it was set. This could not happen on a system with only one scheduler since there would always be shorter timers in the system.

  Setting a timer larger than 49 days on Windows could under rare circumstances cause the timeout to be delayed.

  Own Id: OTP-18911 Aux Id: ERIERL-1023, PR-7983

## Erts 12.3.2.15

### Fixed Bugs and Malfunctions

- Fix faulty debug assert when page size is larger than 16kb, like on PowerPC.
  Did crash debug VM directly at start.

  Own Id: OTP-18802

- On OTP 24 and OTP 25, incoming distributed messages larger than 64 KiB sent
  using an alias leaked memory if the alias had been removed prior to entering
  the node. This issue was not present on OTP 26.

  Incoming distributed messages larger than 64 KiB sent using an alias which had
  been removed on the receiving node could crash the node. This crash was quite
  unlikely on OTP 24 and OTP 25, but very likely on OTP 26.

  `'DOWN'` signals with exit reason larger than 64 KiB directed towards a
  process on a node with a not matching creation leaked memory on the receiving
  node. Such signals should however be very rare.

  Own Id: OTP-18885 Aux Id: GH-7834, GH-7890, PR-7915

- Removed unnecessary PCRE source tar-ball.

  Own Id: OTP-18902

### Improvements and New Features

- Removed unnecessary regexp library used when generating yielding BIFs.

  Own Id: OTP-18830 Aux Id: PR-7823

- Replaced old md5 implementation with an implementation from OpenSSL.

  Own Id: OTP-18877

- Removed unused `makewhatis` script.

  Own Id: OTP-18899

## Erts 12.3.2.14

### Fixed Bugs and Malfunctions

- Fix bug causing "magic" references in a `compressed` ETS table to not keep the
  referred object alive. The symptom would be the referred object being garbage
  collected prematurely and the reference appearing stale, not referring to
  anything. Examples of such magically referred objects are `atomics` and NIF
  resources.

  Own Id: OTP-18732 Aux Id: GH-7444, PR-7458

- The cleanup operation of not yet delivered signals to a terminated process
  yielded excessively.

  Own Id: OTP-18752 Aux Id: PR-7633

- If the external term format encoding of an argument list part of a distributed
  spawn operation was faulty, the newly spawned remote process could misbehave.
  The misbehavior included hanging or interpret an incoming message as an
  argument list to use. This was very unlikely to happen unless using an
  alternate implementation of the distribution protocol which made a faulty
  encoding of the argument list. The child process will now detect this error
  and terminate before executing the user specified code.

  Own Id: OTP-18790 Aux Id: PR-7742

- Fix bugs where if the body of a matchspec would return a map with a variable
  ('$1', '$\_' etc) as one of the keys or values and the variable was not an
  immidiate, the term would not be copied to the receiving processes heap. This
  would later corrupt the term in the table as the GC could place move markers
  in it, which in turn would cause the VM to crash.

  Bug has been present for since OTP 17.0.

  Own Id: OTP-18797 Aux Id: PR-7712 GH-7683

## Erts 12.3.2.13

### Fixed Bugs and Malfunctions

- The following functions are now much faster when given a long list or binary:

  - erlang:list_to_integer/1
  - erlang:binary_to_integer/1
  - erlang:binary_to_integer/2
  - erlang:list_to_integer/2
  - string:to_integer/1

  Own Id: OTP-18659 Aux Id: PR-7426

## Erts 12.3.2.12

### Fixed Bugs and Malfunctions

- In rare circumstances, bit syntax matching of an invalid code point for a
  `utf32` would crash the runtime system.

  Own Id: OTP-18560

- If a runtime system which was starting the distribution already had existing
  pids, ports, or references referring to a node with the same nodename/creation
  pair that the runtime system was about to use, these already existing pids,
  ports, or references would not work as expected in various situations after
  the node had gone alive. This could only occur if the runtime system was
  communicated such pids, ports, or references prior to the distribution was
  started. That is, it was extremely unlikely to happen unless the distribution
  was started dynamically and was even then very unlikely to happen. The runtime
  system now checks for already existing pids, ports, and references with the
  same nodename/creation pair that it is about to use. If such are found another
  creation will be chosen in order to avoid these issues.

  Own Id: OTP-18570 Aux Id: PR-7190

- Constructing a binary segment not aligned with a byte boundary, with a size
  not fitting in 31 bits, and with a value not fitting in a 64-bit word could
  crash the runtime system.

  Own Id: OTP-18597

### Improvements and New Features

- Further robustify implementation of large maps (> 32 keys). Keys that happen
  to have same internal 32-bit hash values are now put in collision nodes which
  are traversed with linear search. This removes the demand for the internal
  hash function when salted to eventually produce different hashes for all
  possible pairs of unequal terms.

  Own Id: OTP-18569

## Erts 12.3.2.11

### Fixed Bugs and Malfunctions

- Fix bug sometimes causing emulator crash at node shutdown when there are
  pending connections. Only seen when running duel distribution protocols,
  inet_drv and inet_tls_dist.

  Own Id: OTP-18243 Aux Id: GH-6247, PR-6258

- Fix bug in `binary_to_term` (and distributed receive) when decoding a large
  map (>32 keys) with unsorted small maps (<= 32) as keys of the large map.

  This was only a problem if the term was encoded by `erl_interface`,
  `jinterface` or otherwise, as the VM itself always encodes small maps with
  sorted keys.

  The "binary_to_term" would appear as successful but the created large map was
  internally inconsistent. The smaller key-maps could not be found with maps:get
  and friends. Other operations such as map compare and merge could probably
  also give incorrect results.

  Own Id: OTP-18343 Aux Id: GH-6496

- Implementations of the [`call()`](driver_entry.md#call) driver callback that
  returned a faulty encoded result could cause a memory leak and could cause
  invalid data on the heap of the processes calling `erlang:port_call/3`.

  Own Id: OTP-18525 Aux Id: PR-7049

- Aliases created in combination with a monitor using the
  `{alias, explicit_unalias}` option stopped working from remote nodes when a
  `'DOWN'` signal had been received due to the monitor or if the monitor was
  removed using the `erlang:demonitor()` BIF.

  This bug was introduced in OTP 24.3.4.10 and OTP 25.3.

  Own Id: OTP-18557 Aux Id: PR-7131, OTP-18496

## Erts 12.3.2.10

### Fixed Bugs and Malfunctions

- Active process aliases of a process at its termination leaked memory.

  Own Id: OTP-18496 Aux Id: GH-6947, PR-6953

## Erts 12.3.2.9

### Fixed Bugs and Malfunctions

- [`process_info(Pid, status)`](`process_info/2`) when `Pid /= self()` could
  return an erroneous result.

  Own Id: OTP-18421 Aux Id: PR-6806

- In rare circumstances, when a process exceeded its allowed heap size set by
  option `max_heap_size`, it would not be killed as it should be, but instead
  enter a kind of zombie state it would never get out of.

  Own Id: OTP-18463 Aux Id: PR-6858

## Erts 12.3.2.8

### Fixed Bugs and Malfunctions

- Fixed a bug in selective receive optimization that could crash 32-bit
  emulators.

  Own Id: OTP-18383 Aux Id: ERIERL-905

- A race condition which was very rarely triggered could cause the signal queue
  of a process to become inconsistent causing the runtime system to crash.

  Own Id: OTP-18388 Aux Id: OTP-17462, PR-6662

## Erts 12.3.2.7

### Fixed Bugs and Malfunctions

- Spec for function net:if_names/0 incorrect

  Own Id: OTP-18296 Aux Id: OTP-16464

- Fix bug in `binary_to_term` decoding a binary term 2Gbyte or larger.

  Own Id: OTP-18306 Aux Id: GH-6393, PR-6401

- Fix [`list_to_atom/1`](`list_to_atom/1`) for negative code points. Could
  either return with a positive code point or fail with an incorrect exception.

  Own Id: OTP-18321

- Fix bug in `binary_to_term` decoding a list of length 1G or longer.

  Own Id: OTP-18328 Aux Id: GH-6439, PR-6440

## Erts 12.3.2.6

### Fixed Bugs and Malfunctions

- [Notifications about available distribution data](`erlang:dist_ctrl_get_data_notification/1`)
  sent to distribution controller processes could be lost. Distribution
  controller processes can be used when implementing an alternative distribution
  carrier. The default distribution over tcp was not effected and the bug was
  also not present on x86/x86_64 platforms.

  Own Id: OTP-18258 Aux Id: GH-6309, PR-6324

## Erts 12.3.2.5

### Fixed Bugs and Malfunctions

- Fix writing and reading of more than 2 GB in a single read/write operation on
  macOS. Before this fix attempting to read/write more than 2GB would result in
  `{error,einval}`.

  Own Id: OTP-18222 Aux Id: PR-6248 GH-6242

## Erts 12.3.2.4

### Fixed Bugs and Malfunctions

- Fix bug causing `ets:info` (and sometimes `ets:whereis`) to return 'undefined'
  for an existing table if a concurrent process were doing `ets:insert` with a
  long list on the same table.

  Own Id: OTP-18218 Aux Id: ERIERL-855

## Erts 12.3.2.3

### Fixed Bugs and Malfunctions

- Fix faulty distribution encoding of terms with either

  - a fun with bit-string or export-fun in its environment when encoded toward a
    not yet established (pending) connection
  - or a fun with a binary/bitstring, in its environment, referring to an
    off-heap binary (larger than 64 bytes).

  The symptom could be failed decoding on the receiving side leading to aborted
  connection. Fix OTP-18093 is a workaround for theses bugs that makes the VM
  accepts such faulty encoded funs.

  The first encoding bug toward pending connection exists only in OTP 23 and 24,
  but the second one exists also on OTP 25.

  Own Id: OTP-18104 Aux Id: OTP-18093

- Distributed exit signals could be lost under the following conditions:

  - An exit signal from a parent process to a child process was lost if:

    - the parent process terminated before the spawn request that created the
      child had completed,
    - the spawn request set up a link between parent and child
    - the spawn request was distributed, and
    - the exit reason was larger than one machine word.

  - Loss of a connection over which a not yet completed spawn request was
    ongoing could cause loss of exit signals. Such loss of exit signals was very
    rare. Besides the above described connection loss also the following
    conditions had to be satisfied:

    - The spawn request that was interrupted by the connection loss also had to
      set up a link between the parent process and the child process.
    - The parent process that issued the spawn request also had to be
      terminating while the spawn request was interrupted by the connection
      loss.
    - The same parent process also had to have made other spawn requests to
      other nodes than to the node to which the connection was lost.
    - These spawn requests to the other nodes also had to set up links.
    - These spawn requests to the other nodes also had to be not yet completed
      at the time of the connection loss. That is, the spawn reply from the
      child process had not yet reached the parent process.

    If all the conditions above were met, exit signals to the children spawned
    due to the above described spawn requests to other nodes _could_ be lost.

    The above bug also caused a significant memory leak when it was triggered
    since the destruction of the parent process never completed.

  Own Id: OTP-18164 Aux Id: PR-6114

- A race could cause [`process_info(Pid, message_queue_len)`](`process_info/2`)
  on other processes to return invalid results.

  Own Id: OTP-18169 Aux Id: PR-6134

- Fixed reduction counting for handling process system tasks.

  Own Id: OTP-18170 Aux Id: PR-6135

- Priority elevation of terminating processes did not work which could cause
  execution of such processes to be delayed.

  Own Id: OTP-18175 Aux Id: PR-6142

- An unlink operation made by a process that terminated before the unlink
  operation completed, i.e., before it had received an unlink-ack signal from
  the linked process, caused an exit signal to erroneously be sent from the
  terminating process to the process being unlinked. This exit signal would most
  often be ignored by the receiver, but if the receiver of the exit signal
  concurrently set up a new link, it could receive the exit signal with the
  actual exit reason of the terminating process instead of a `noproc` exit
  reason. It is however very hard to detect that this has happened and has no
  obvious negative consequences, so it should be considered harmless.

  A distributed unlink-ack signal received by a terminating process was also not
  properly removed which could cause a minor memory leak.

  Own Id: OTP-18177 Aux Id: PR-6150

- The [`monitor/3`](`erlang:monitor/3`) BIF did not apply options to the created
  monitor if the target process or port did not exist. That is, the
  corresponding down message would get a `` `DOWN` `` tag even if a custom tag
  had been set, and the returned reference was not an alias even if the `alias`
  option had been passed.

  Own Id: OTP-18190 Aux Id: GH-6185, PR-6209

- The `erlang:monotonic_time/1`, `erlang:system_time/1`, `erlang:time_offset/1`,
  and `os:system_time/1` BIFs erroneously failed when passed the argument
  `native`.

  Own Id: OTP-18197 Aux Id: GH-6165, PR-6213

## Erts 12.3.2.2

### Fixed Bugs and Malfunctions

- Fixed emulator crash that could happen during crashdump generation of ETS
  tables with options `ordered_set` and `{write_concurrency,true}`.

  Own Id: OTP-18144 Aux Id: GH-5981

## Erts 12.3.2.1

### Fixed Bugs and Malfunctions

- Accept funs (NEW_FUN_EXT) with incorrectly encoded size field. This is a
  workaround for a bug (OTP-18104) existing in OTP 23 and 24 that could cause
  incorrect size fields in certain cases. The emulator does not use the decoded
  size field, but `erl_interface` still does and is not helped by this
  workaround.

  Own Id: OTP-18093 Aux Id: OTP-18104, PR-5987

- The zlib built in to the runtime system has been updated to version 1.2.12.
  (Note that on most platforms, the platform's own zlib is used.)

  Own Id: OTP-18123 Aux Id: GH-5994

## Erts 12.3.2

### Fixed Bugs and Malfunctions

- Let EPMD tolerate failure when binding to IPv4/IPv6 loopback intefaces in
  addition to user-supplied addresses via `ERL_EPMD_ADDRESS` or the `-address`
  option. This can happen, for example, if the host system has ipv6 disabled via
  the disable_ipv6 sysctl.

  Own Id: OTP-17970 Aux Id: PR-5762

- Fixed a rare bug in [`binary_to_term/1`](`binary_to_term/1`),
  `enif_make_map_from_arrays`, `erl_drv_send_term`, and Erlang distribution that
  could crash the emulator.

  Own Id: OTP-18027

- Fixed a rare race in `persistent_term:get/1,2` that could cause it to return
  the value of another key.

  Own Id: OTP-18065 Aux Id: GH-5908

- Fix bug where the process message queue was left in an inconsistent state when
  changing from on_heap to off_heap message queue data causing the GC to
  segfault.

  Own Id: OTP-18075 Aux Id: PR-5927

- Fix functions that convert universal to localtime (such as erlang:localtime/0
  and erlang:universaltime_to_localtime/1) to fetch the correct localtime if it
  is changed after the start of the VM.

  Own Id: OTP-18076 Aux Id: ERIERL-802 PR-5905

- Fix memory leak when a process doing a distributed fragmented send is sent an
  exit signal. Before this fix the receiving node would be left with an
  incomplete message that would remain until the nodes were disconnected. The
  bug has existed since Erlang/OTP 21.

  Own Id: OTP-18077 Aux Id: GH-5876 PR-5892

- Corrected the behaviour of the shutdown function when using with the
  inet_backend = socket. It was not sufficiently compatible with the "old"
  gen_tcp.

  Own Id: OTP-18080 Aux Id: GH-5930

## Erts 12.3.1

### Fixed Bugs and Malfunctions

- `erlang:open_port({spawn, _},_)` has been fixed on Windows to handle
  whitespace characters in the path correctly.

  This could, for example, cause execution of the resolver helper program
  `inet_gethost` to fail and instead possibly execute a different program.

  Own Id: OTP-17978 Aux Id: OTP-17958

- Fix race condition when creating crash dump that could cause multiple threads
  to race when writing the initial information in a crash dump.

  The race condition was introduced in erts-12.2 (Erlang/OTP 24.2).

  Own Id: OTP-17993 Aux Id: PR-5806

- Fix Erlang monotonic time on MacOS. Previously used OS monotonic time
  primitive on MacOS is buggy and will not be used anymore. It has been replaced
  with usage of another OS monotonic time primitive that does not appear to be
  buggy.

  Own Id: OTP-17998 Aux Id: PR-5825, GH-5554

## Erts 12.3

### Fixed Bugs and Malfunctions

- Fixed a bug in the x86 JIT that might cause floating point instructions to
  wrongly throw an exception.

  Own Id: OTP-17822

- Preserve correct `nodedown_reason` if supervised distribution controller
  processes exit with `{shutdown, Reason}`.

  Own Id: OTP-17838 Aux Id: PR-5748

- Handling of `send_timeout` for `gen_tcp` has been corrected so that the
  timeout is honored also when sending 0 bytes.

  Own Id: OTP-17840

- By default `global` does _not_ take any actions to restore a fully connected
  network when connections are lost due to network issues. This is problematic
  for all applications expecting a fully connected network to be provided, such
  as for example `mnesia`, but also for `global` itself. A network of
  overlapping partitions might cause the internal state of `global` to become
  inconsistent. Such an inconsistency can remain even after such partitions have
  been brought together to form a fully connected network again. The effect on
  other applications that expects that a fully connected network is maintained
  may vary, but they might misbehave in very subtle hard to detect ways during
  such a partitioning.

  In order to prevent such issues, we have introduced a _prevent overlapping
  partitions_ fix which can be enabled using the
  [`prevent_overlapping_partitions`](`e:kernel:kernel_app.md#prevent_overlapping_partitions`)
  `kernel(6)` parameter. When this fix has been enabled, `global` will actively
  disconnect from nodes that reports that they have lost connections to other
  nodes. This will cause fully connected partitions to form instead of leaving
  the network in a state with overlapping partitions. Note that this fix _has_
  to be enabled on _all_ nodes in the network in order to work properly. Since
  this quite substantially changes the behavior, this fix is currently disabled
  by default. Since you might get hard to detect issues without this fix you
  are, however, _strongly_ advised to enable this fix in order to avoid issues
  such as the ones described above. As of OTP 25 this fix will become enabled by
  default.

  Own Id: OTP-17843 Aux Id: ERIERL-732, PR-5611

- Corrected the type specification of `erlang:seq_trace/2`.

  Own Id: OTP-17900 Aux Id: GH-5667

- Fix memory leak when tracing on running on a process that only handle system
  tasks or non-message signals (for example process_info requests).

  Own Id: OTP-17904 Aux Id: ERIERL-757

### Improvements and New Features

- Add support for using socket:sockaddr_in() and socket:sockaddr_in6() when
  using gen_sctp, gen_tcp and gen_udp. This will make it possible to use Link
  Local IPv6 addresses.

  Own Id: OTP-17455 Aux Id: GH-4852

- Show `on_load` failure reasons in embedded mode.

  Own Id: OTP-17718 Aux Id: PR-5199

- Compile date saved in the Erlang VM executable has been removed.

  Own Id: OTP-17891 Aux Id: PR-5589

- Improve documentation for the dynamic node name feature.

  Own Id: OTP-17918

## Erts 12.2.1

### Fixed Bugs and Malfunctions

- Fixed a memory leak in `file:read_file_info/2` and `file:read_file/1` on
  Windows.

  Own Id: OTP-17827 Aux Id: GH-5527

- Fix GC emulator crash when `spawn_request` was used when message tracing was
  enabled.

  Own Id: OTP-17871 Aux Id: PR-5612

## Erts 12.2

### Fixed Bugs and Malfunctions

- When matching and constructing `utf16` segments in the binary syntax, the
  `native` flag would be ignored. That is, the endian would always be big endian
  even on a little-endian computer (almost all modern computers).

  Own Id: OTP-17713

- Fix the help printout of `+JPperf`.

  Own Id: OTP-17749 Aux Id: PR-5378 GH-5361

- Fix bug that could cause Erlang to deadlock during creation of an Erlang crash
  dump.

  Own Id: OTP-17751 Aux Id: PR-5315

- Fixed C++ build errors on some aarch64 platforms.

  Own Id: OTP-17763 Aux Id: GH-5351

- For macOS, the `Info.plist` file embedded in the runtime system now only
  contains the absolute minimum amount of information needed for the web view in
  `wx` to work towards `localhost`. The other fields have been removed, allowing
  an application packaged in a bundle to specify the application name and other
  parameter in its own `Info.plist` file.

  Own Id: OTP-17785 Aux Id: PR-5393

- Fix bug in internal stacks (WSTACK and ESTACK) used by
  [`term_to_binary/2`](`term_to_binary/2`) to encode terms. The bug could cause
  a segfault if a very very large map was to be encoded with the `deterministic`
  option given.

  Own Id: OTP-17804 Aux Id: PR-5372

- Improve the error printout when [`open_port/2`](`open_port/2`) fails because
  of invalid arguments.

  Own Id: OTP-17805 Aux Id: PR-5406

- Fix bug in crash dumps where the stackframe of a process would be printed
  using an incorrect format.

  Crash dump viewer has also been fixed to be able read the broken stack format.

  The bug has existed since Erlang/OTP 23.0.

  Own Id: OTP-17814 Aux Id: PR-5462

### Improvements and New Features

- An option for enabling dirty scheduler specific allocator instances has been
  introduced. By default such allocator instances are disabled. For more
  information see the documentation of the [`+Mdai`](erts_alloc.md#Mdai) `erl`
  command line argument.

  Own Id: OTP-17363 Aux Id: GH-4728, PR-5187

- Minor optimization of receive markers in message queues.

  Own Id: OTP-17673 Aux Id: OTP-16226

- All predefined types have been added to the `erlang` module together with
  documentation.

  Any reference to a predefined type now links to that documentation so that the
  user can view it.

  Own Id: OTP-17689 Aux Id: PR-5292

- Suppress a code checker warning caused by debug builds of YCF. YCF tries to
  get a conservative estimate of the bottom of the stack by reading and
  returning a call stack allocated variable.

  Own Id: OTP-17719

- Add file and product properties to erl.exe and werl.exe.

  Own Id: OTP-17724 Aux Id: ERL-1224

- Micro optimization in bitstring append operations.

  Own Id: OTP-17760 Aux Id: ERIERL-725, PR-5414

- Responsiveness of processes executing on `normal` or `low` priority could
  suffer due to code purging or literal area removal on systems with a huge
  amount of processes. This since during these operations all processes on the
  system were scheduled for execution at once.

  This problem has been fixed by introducing a limit on outstanding purge and
  copy literal requests in the system. By default this limit is set to twice the
  amount of schedulers on the system. This will ensure that schedulers will have
  enough work scheduled to perform these operations as quickly as possible at
  the same time as other work will be interleaved to a much higher degree.
  Performance of these operations will however be somewhat degraded due to the
  overhead of enforcing this limit compared to when using a very large limit.

  This limit can be set by passing the `+zosrl` command line argument to `erl`,
  or by calling
  `erlang:system_flag(outstanding_system_requests_limit, NewLimit)`.

  Own Id: OTP-17796 Aux Id: ERIERL-729, PR-5473

## Erts 12.1.5

### Fixed Bugs and Malfunctions

- The runtime system could call `select()` with a too large timeout value when
  executing on MacOS. This could in turn cause the runtime system to crash.

  Own Id: OTP-17735 Aux Id: GH-5339

- The fix for Linux's behaviour when reconnecting an UDP socket in PR-5120
  released in OTP-24.1.2 has been refined to only dissolve the socket's
  connection before a connect if the socket is already connected, that is: only
  for a reconnect.

  This allows code to open a socket with an ephemeral port, get the port number
  and connect; without the port number changing (on Linux). This turned out to
  have at least one valid use case (besides test cases).

  Should one reconnect the socket then the port number may change, on Linux; it
  is a known quirk, which can be worked around by binding to a specific port
  number when opening the socket. If you can do without an ephemeral port, that
  is...

  Own Id: OTP-17736 Aux Id: GH-5279, PR-5120, OTP-17559

- Certain distributed signals that for various reasons must to be forced into
  the distribution buffer even when it is full would instead be lost if the
  distribution buffer was full when sent. The effected signals:

  - `EXIT` signals with exit reasons of one word size.
  - `DOWN` signals with exit reasons of one word size.
  - `demonitor` signals from a terminating process.
  - `unlink_ack` signals on OTP 23 and 24.
  - `spawn_reply` signals on OTP 23 and 24.

  Own Id: OTP-17737 Aux Id: GH-5346, GH-4989

## Erts 12.1.4

### Fixed Bugs and Malfunctions

- Fix bug where a gen_tcp write error that happened during a delayed_send would
  cause a use after free segfault.

  Own Id: OTP-17731 Aux Id: PR-5285

- Fix x86 JIT bug where a `rem` instruction could cause a segfault if given
  values that would cause an badarith exception.

  Own Id: OTP-17732 Aux Id: PR-5331 ERIERL-664

## Erts 12.1.3

### Fixed Bugs and Malfunctions

- Reduction counter was not updated before and after doing `apply` operations on
  the runtime system with the jit enabled. This caused reduction counting to get
  out of sync if a garbage collection was made as part of the `apply` operation.

  Own Id: OTP-17675

- This fixes a bug in `erts_factory_undo` that caused the heap to not be reset
  correctly. The `erts_factory_undo` function is, for example, called when a
  [`binary_to_term/1`](`binary_to_term/1`) call fails to reset the heap to its
  state before the [`binary_to_term/1`](`binary_to_term/1`) call. This can cause
  the heap to contain invalid terms which potentially can cause issues (e.g.,
  crashes) when the whole heap is scanned.

  Own Id: OTP-17677

- When attempting to construct a binary with an segment having an illegal type
  for the size (e.g. an atom), there could be an unnecessary memory allocation
  (and subsequent deallocation) before the operation failed. Amended to fail
  before allocating any memory for the binary.

  Own Id: OTP-17686

- Fix bug in `persistent_term` when a key-value pair contains a magic reference
  that is referred more than once. Magic references are NIF resources or
  returned from BIFs like `ets:new`, `atomics:new`. The bug could cause the
  memory of the referred resource to be prematurely deallocated.

  The bug also apply to magic references in message passing on a runtime built
  with configure option `--enable-sharing-preserving`.

  Bug exist for 64-bit since OTP-24.0 and for 32-bit since OTP-20.0.

  Own Id: OTP-17700 Aux Id: GH-5271, PR-5273

- Fixed a crash when inspecting the stack trace of an exception raised at a very
  high line number.

  This bug was introduced in OTP 24.

  Own Id: OTP-17712

- The following two bugs that caused `erlang:demonitor()` to behave erroneously
  have been fixed. The bugs were only triggered if the monitor that was removed
  by `demonitor()` had previously been created simultaneously as a monitor and
  as an alias.

  - A demonitor operation on a monitor created using the
    `{alias, reply_demonitor}` option erroneously behaved as if the
    `{alias, explicit_unalias}` option had been used.
  - A demonitor operation did not prevent a corresponding `'DOWN'` message from
    being delivered if the monitor reference was kept as an active alias after
    the operation. This could only occur if the monitored process simultaneously
    terminated before the demonitor signal reached it, and the exit reason was
    not an immediate term. That is, a term larger than one machine word.

  Own Id: OTP-17722 Aux Id: GH-5310, PR-5313

## Erts 12.1.2

### Improvements and New Features

- The python scripts that existed in
  erts/lib_src/yielding_c_fun/lib/tiny_regex_c/scripts had a license that was
  incompatible with Erlang/OTP's license. This ticket removes these scripts that
  were not used by us.

  Own Id: OTP-17658

## Erts 12.1.1

### Fixed Bugs and Malfunctions

- A race between an exiting port and handling of simultaneously received signals
  to that port could cause a runtime system crash. The effected signals are
  `link`, `monitor` and `demonitor`. On OTP 22 a similar race could also cause a
  memory leak when receiving an `unlink` signal.

  Own Id: OTP-17642 Aux Id: PR-5248

- [A user defined tag on a monitor message](`erlang:monitor/3`) could cause the
  runtime system to crash when the monitor message had been received.

  Own Id: OTP-17646 Aux Id: GH-5221, PR-5232

- A call to [`erlang:demonitor(Ref)`](`erlang:demonitor/1`) where the reference
  `Ref` referred to an active alias, but not an active monitor, caused the
  runtime system to crash.

  Own Id: OTP-17647 Aux Id: GH-5225, PR-5230

- The message queue of a process entered an inconsistent state after a `receive`
  expression with an invalid timeout value was executed. If the exception raised
  due to the invalid timeout value was caught, the following `receive`
  expression executed by the process could fail to match messages already
  present in the message queue.

  On OTP 24 this could also cause the whole runtime system to crash.

  Own Id: OTP-17651 Aux Id: GH-5235, PR-5244

- Sending a `Port ! {PortOwner, close}` signal from a process other than the
  port owner could erroneously trigger a `badsig` exit signal being sent to the
  port owner process even though the correct `PortOwner` had been passed in the
  signal.

  Own Id: OTP-17665 Aux Id: PR-5248

## Erts 12.1

### Fixed Bugs and Malfunctions

- Atoms with Unicode code points greater than 255 (for example Greek or Cyrillic
  characters) would not be displayed correctly by `crashdump_viewer`.

  Own Id: OTP-17377

- Fix rare minor memory leak related to jit code loading.

  Own Id: OTP-17445 Aux Id: PR-4843

- The extended error information has been corrected and improved for the
  following BIFs: [`binary_to_existing_atom/2`](`binary_to_existing_atom/2`),
  [`list_to_existing_atom/1`](`list_to_existing_atom/1`),
  `erlang:send_after/{3,4}`, and `erlang:start_timer/{3,4}`.

  Own Id: OTP-17449 Aux Id: GH-4900

- Fix bug provoked when building with gcc 10 and link time optimization (-flto),
  causing Erlang compiler to crash. Bug exists since OTP-24.0.

  Own Id: OTP-17477 Aux Id: GH-4846, PR-4894

- Corrected bugs where builds were not reducible even when the `deterministic`
  option was given. In particular, modules with map literals with more than 32
  elements could cause this problem.

  As part of this fix, the `term_to_binary` BIF now accepts the option
  `deterministic`.

  Own Id: OTP-17495 Aux Id: PR-5153

- After an exception has been caught in a process, the stack trace would be kept
  in the process longer than necessary.

  Own Id: OTP-17512

- Fix rare race bug in memory management of distribution entries. Have been seen
  to cause VM crash when massive number of repeated concurrent failing
  connection attempts.

  Own Id: OTP-17513 Aux Id: GH-4964, PR-5015

- The configure test for `--disable-esock-socket-registry` has been corrected so
  disabling now works.

  Own Id: OTP-17539

- `init:stop()` no longer unloads loaded code before terminating the runtime
  system. On systems with slow CPUs (such as Raspberry PI Zero), that can
  significantly speed up termination of the runtime system.

  Own Id: OTP-17542 Aux Id: GH-5031, PR-5032

- Fixed match specifications that use maps in either the guard or the body to
  work properly.

  With this fix both keys and values in maps can be expressions.

  Various other crashes and bugs when using maps in match specifications have
  also been fixed.

  Own Id: OTP-17567 Aux Id: PR-4915 PR-5115

- Parsing of the result value in the native DNS resolver has been made more
  defensive against incorrect results.

  Own Id: OTP-17578 Aux Id: ERIERL-683

- When [`binary_to_term/2`](`binary_to_term/2`) failed, the extended error
  information would always blame the second argument even if the actual error
  was in the first argument.

  Own Id: OTP-17591 Aux Id: GH-5171

- On 32-bit computers, `binary_to_term/1,2` is now more resilient against
  corrupted binaries containing maps in the external format.

  Own Id: OTP-17604

- A call to [`process_info(Pid, status)`](`process_info/2`) could erroneously
  report the status `running` when it should have reported `waiting`. This
  occurred when the calling process was executing on a higher priority than the
  process being inspected. This bug has been present since OTP 21.0 (erts
  version 10.0).

  Own Id: OTP-17628

### Improvements and New Features

- Optimize memory usage of erts internal processes used during code
  loading/purging by hibernating them after a long time of inactivity.

  Own Id: OTP-17426 Aux Id: PR-4785

- Add the type `t:erlang:stacktrace/0`.

  Own Id: OTP-17453 Aux Id: PR-4764

- The arity argument of [`error/2,3`](`erlang:error/2`) can now be `none` to
  indicate that the calling functions arity should be used.

  Own Id: OTP-17456 Aux Id: PR-4764

- Optimize match spec compiler for immediate (single word) constant terms.

  Own Id: OTP-17469

- Functions `erlang:set_cookie(Cookie)` and `erlang:get_cookie(Node)` have been
  added for completeness and to facilitate configuring distributed nodes with
  different cookies.

  The documentation regarding distribution cookies has been improved to be less
  vague.

  Own Id: OTP-17538 Aux Id: GH-5063, PR-5111

- A workaround has been implemented for Linux's quirky behaviour to not adjust
  the source IP address when connecting a connected (reconnecing) UDP socket.

  The workaround is to, on Linux, always dissolve any connection before
  connecting an UDP socket.

  Own Id: OTP-17559 Aux Id: GH-5092, PR-5120

- The internal documentation for how to use Yielding C Fun (YCF) has been
  updated to contain text about best practices for using YCF for ERTS.

  Own Id: OTP-17596

- Optimize garbage collection for processes with large number of binaries, funs
  and/or external pids/ports/refs.

  Own Id: OTP-17602 Aux Id: PR-5149

## Erts 12.0.4

### Fixed Bugs and Malfunctions

- A call to the `process_info()` BIF could end up hanging for ever due to a bug
  introduced when the new selective receive optimization was introduced in OTP
  24.0. Note that this bug only effects `process_info()`.

  Own Id: OTP-17548 Aux Id: PR-5078, OTP-10391

- Fix buffer overrun problem in the tty driver. The problem happens on some
  platforms when using the CTRL+R functionality of newshell with very long
  strings in the history.

  Own Id: OTP-17560 Aux Id: GH-5116

- Fix race-condition that could cause a crash when tracing scheduling or garbage
  collections on a process that was running on a dirty scheduler.

  Own Id: OTP-17568 Aux Id: PR-4940

- Fix rare bug where re:run would crash/return invalid results when given a
  subbinary as subject.

  This bug has existed since Erlang/OTP 20.0.

  Own Id: OTP-17585 Aux Id: GH-5150

- `binary_to_term/1,2` is now more resilient against corrupted binaries
  containing maps in the external format.

  Own Id: OTP-17594

## Erts 12.0.3

### Fixed Bugs and Malfunctions

- A call to `erlang:cancel_timer(_, [{info, false}])` could cause the calling
  process to block forever in the call. Note that only the synchronous version
  of the call (that is, the `async` option is `false`) in combination with the
  `info` option set to `false` was effected by this bug.

  Own Id: OTP-17472 Aux Id: PR-4932

- Microstate accounting (msacc) and `os:perf_counter()` unintentionally used
  system time instead of monotonic time for time measurements on a lot of
  systems. These systems were all non x86/x86_64 systems or x86/x86_64 systems
  without a reliable and constant rdtsc instruction.

  The lock counting (lcnt) built runtime system also unintentionally used system
  time instead of monotonic time for time measurements on all systems.

  Own Id: OTP-17493

- Simultaneous calls to `erlang:system_flag(schedulers_online, _)` could cause
  callers to end up in a suspended state forever.

  Own Id: OTP-17500 Aux Id: GH-4809

## Erts 12.0.2

### Fixed Bugs and Malfunctions

- Not yet handled alias-message signals in the signal queue at the time when a
  garbage collection was performed could cause a memory corruption which in turn
  could result in a crash of the runtime system. This bug was introduced in OTP
  24.0.

  Own Id: OTP-17431 Aux Id: GH-4858, PR-4870, OTP-16718

- Fixed bug when using external pids/ports in keys of big maps (> 32). Could
  cause runtime crash. Bug exists since OTP 24.0.

  Own Id: OTP-17436 Aux Id: PR-4875

- After a node restart with `init:restart/0,1`, the module `socket` was not
  usable because supporting tables had been cleared and not re-initialized. This
  has now been fixed.

  Handling of the "." domain as a search domain was incorrect and caused a crash
  in the DNS resolver `inet_res`, which has now been fixed.

  Own Id: OTP-17439 Aux Id: GH-4827, PR-4888, GH-4838

- A call to `port_command()` could cause a scheduler to end up in an eternal
  loop if the port was busy and the calling process had incoming signals at the
  time of the call. This bug was introduced in OTP 23.3.2 (ERTS version 11.2.1),
  OTP 22.3.4.18 (ERTS version 10.7.2.10), and OTP 21.3.8.23 (ERTS version
  10.3.5.18).

  Own Id: OTP-17448 Aux Id: GH-4898, PR-4903, OTP-17291

- Bug fixes and code cleanup for the new `socket` implementation, such as:

  Assertions on the result of demonitoring has been added in the NIF code, where
  appropriate.

  Internal state handling for socket close in the NIF code has been reviewed.

  Looping over `close()` for `EINTR` in the NIF code has been removed, since it
  is strongly discouraged on Linux and Posix is not clear about if it is
  allowed.

  The `inet_backend` temporary socket option for legacy `gen_tcp` sockets has
  been documented.

  The return value from `net:getaddrinfo/2` has been corrected: the `protocol`
  field is now an `t:atom/0`, instead of, incorrectly,
  [`list(atom())`](`t:list/1`). The documentation has also been corrected about
  this return type.

  Deferred close of a `socket:sendfile/*` file was broken and has been
  corrected.

  Some debug code, not enabled by default, in the socket NIF has been corrected
  to not accidentally core dump for debug printouts of more or less innocent
  events.

  Own Id: OTP-17452

- Dirty execution of a process in combination with an unlink signal from a port
  to the process could cause the signal queue of the process to enter into an
  inconsistent state. The result of the inconsistency typically caused a crash
  of the runtime system. This bug was introduced in OTP 23.3 (ERTS version
  11.2).

  Own Id: OTP-17462 Aux Id: GH-4885, PR-4914, OTP-17127

## Erts 12.0.1

### Fixed Bugs and Malfunctions

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## Erts 12.0

### Fixed Bugs and Malfunctions

- `file:open/2` now throws an badarg error when opened with both the `ram` and
  `raw` options.

  Own Id: OTP-16822 Aux Id: PR-2723

- The estone benchmark has been updated to better reflect changes in the
  compiler and run-time system.

  Own Id: OTP-16879

- Fix profile guided optimization of run-time system when using GCC 7 or later.

  Own Id: OTP-16880

- Fix double close of fd when creating crash dump.

  Own Id: OTP-16884

- Improve `erl` error message when unable to open included `args_file`.

  Own Id: OTP-17014

- Remove warning text about the `--` operation from documentation

  The `--` operation was optimized in Erlang/OTP 22 so that its worst case
  complexity is O(N*log(N)), where N is the total size of the input lists.
  Therefore, the warning in the documentation saying that the time complexity is
  proportional to length(A)*length(B) is incorrect and is no longer needed.
  Notice that Erlang/OTP 21 will no longer be supported when Erlang/OTP 24 gets
  released.

  Own Id: OTP-17020

- A floating point zero (0.0) can be both positive (+0.0) and negative (-0.0).
  Multiple bugs in the compiler, runtime system, and STDLIB have been fixed to
  ensure that the minus sign on 0.0 is not lost.

  Own Id: OTP-17077 Aux Id: ERL-1431, PR-2903, PR-2905, PR-2906

- Fix compiler warnings produced by the clang compiler.

  Own Id: OTP-17105 Aux Id: PR-2872

- Windows process `erl.exe` killed if its service process `erlsrv.exe`
  terminates.

  Own Id: OTP-17131 Aux Id: PR-3031, GH-4360

- The `configure` scripts in `crypto` and `erts` now fail if a requested feature
  cannot be enabled.

  Large parts of the `configure` script of `crypto` have been rewritten with
  various improvements and bug fixes. It is now better at finding usable OpenSSL
  libraries, but will in the following cases fail to detect OpenSSL libraries
  where it previously sometimes detected the libraries by chance:

  - OpenSSL installations with `include` directory and `lib` directory parts
    installed in different base directories. In order to detect such
    installations after this change, the user must explicitly specify the
    locations using the
    [`--with-ssl=<path>`](`e:system:install.md#advanced-configuration-and-build-of-erlang-otp_configuring`)
    and the
    [`--with-ssl-incl=<path>`](`e:system:install.md#advanced-configuration-and-build-of-erlang-otp_configuring`)
    `configure` command line arguments.
  - When building with old `gcc` compilers or other compilers on Debian
    derivatives with multiarch directories under the `lib` directory. In order
    to detect such installations after this change, the user must explicitly
    specify the multiarch directory name using the
    [`--with-ssl-lib-subdir=lib/<multiarch-dir>`](`e:system:install.md#advanced-configuration-and-build-of-erlang-otp_configuring`)
    `configure` command line argument.

  Own Id: OTP-17254 Aux Id: ERIERL-618, GH-4230

- The `erl` command line arguments `+Bi`, `+Bd`, and `+B` erroneously caused
  reception of the `USR1` signal to terminate the runtime system without
  creating a crash dump. Reception of the `USR1` signal now always cause
  termination _with_ creation of a crash dump, regardless of command line
  arguments passed. This bug has existed at least since OTP R5B.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17275 Aux Id: PR-4553

- Add check to make sure that when passing an option to `erl` that requires an
  argument, but none is given, we report an error. This fixes a bug introduced
  in OTP-22.1 via OTP-15926.

  Own Id: OTP-17314 Aux Id: OTP-15926, GH-4624, PR-

- The "Last calls" section in crash dumps have been updated to print newlines
  after each non-function `save_calls` state (send, receive, timeout).

  Own Id: OTP-17329 Aux Id: PR-4730

- Sockets created with socket:accept not counted (socket:info/0).

  Own Id: OTP-17372

### Improvements and New Features

- The experimental `socket` module can now use any protocol (by name) the OS
  supports. Suggested in PR-2641, implemented in PR-2670.

  Own Id: OTP-14601 Aux Id: PR-2641, PR-2670, OTP-16749

- New functions `enif_dynamic_resource_call` enables NIFs to call native code in
  another NIF module. The call is done via a resource callback function
  `dyncall` supplied by the user with the new `enif_init_resource_type`.

  Own Id: OTP-14753

- Runtime support for new improved selective receive optimization.

  Own Id: OTP-16226 Aux Id: OTP-10391

- The deprecated function `erlang:get_stacktrace/0` has been removed. Use the
  new syntax in try/catch to retrieve the stack backtrace.

  Own Id: OTP-16653

- Support for handling abstract code created before OTP R15 has been dropped.

  Own Id: OTP-16678 Aux Id: PR-2627

- Extended error information for failing BIF calls as proposed in
  [EEP 54](https://github.com/erlang/eep/blob/master/eeps/eep-0054.md) has been
  implemented.

  When a BIF call from the Erlang shell fails, more information about which
  argument or arguments that were in error will be printed. The same extended
  error information will by `proc_lib`, `common_test`, and `qlc` when BIF calls
  fail.

  For applications that wish to provide the same extended error information,
  there are new functions `erl_error:format_exception/3` and
  `erl_error:format_exception/4`.

  There is a new [`error/3`](`error/3`) BIF that allows applications or
  libraries to provide extended error information in the same way for their own
  exceptions.

  Own Id: OTP-16686

- The `m:erlang` module documentation has been updated to improve clarity and
  description of edge cases.

  Own Id: OTP-16687 Aux Id: PR-2996 PR-2762

- An example implementation of Erlang distribution over UDS using distribution
  processes has been introduced.

  Thanks to Jérôme de Bretagne

  Own Id: OTP-16703 Aux Id: PR-2620

- Improve code generation when creating funs by adding a new beam instruction
  make_fun3 that does not do GC and allows for better register allocation.

  Own Id: OTP-16712

- The [_process alias_](`e:system:ref_man_processes.md#process-aliases`) feature
  as outlined by
  [EEP 53](https://github.com/erlang/eep/blob/master/eeps/eep-0053.md) has been
  introduced. It is introduced in order to provide a lightweight mechanism that
  can prevent late replies after timeout or connection loss. For more
  information, see EEP 53 and the documentation of the new
  [`alias/1`](`erlang:alias/1`) BIF and the new options to the
  [`monitor/3`](`erlang:monitor/3`) BIF.

  The `call` operation in the framework used by `gen_server`, `gen_statem`, and
  `gen_event` has been updated to utilize alias in order to prevent late
  responses. The `gen_statem` behavior still use a proxy process in the
  distributed case, since it has always prevented late replies and aliases won't
  work against pre OTP 24 nodes. The proxy process can be removed in OTP 26.

  The alias feature also made it possible to introduce new functions similar to
  the [`erpc:receive_response()`](`erpc:receive_response/2`) function in the gen
  behaviors, so the new functions
  [`gen_server:receive_response()`](`gen_server:receive_response/2`),
  [`gen_statem:receive_response()`](`gen_statem:receive_response/2`),
  [`gen_event:receive_response()`](`gen_event:receive_response/2`) have also
  been introduced.

  Own Id: OTP-16718 Aux Id: PR-2735

- Accept 64-bit process identifiers from external nodes. This is the first step
  in an upgrade path toward using 64-bit pids in a future OTP release.

  Own Id: OTP-16720 Aux Id: PR-2680

- The experimental new socket API has been further developed. Some backwards
  incompatible changes with respect to OTP 23 have been made.

  The control message format has been changed so a decoded value is now in the
  'value' field instead of in the 'data' field. The 'data' field now always
  contains binary data.

  Some type names have been changed regarding message headers and control
  message headers.

  `socket:bind/2` now returns plain `ok` instead of `{ok, Port}` which was only
  relevant for the `inet` and `inet6` address families and often not
  interesting. To find out which port was chosen use `socket:sockname/1`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16749 Aux Id: OTP-14601

- Remove old unused `+MYm` and `ERL_MALLOC_LIB` options.

  Own Id: OTP-16788

- Increase timer resolution on windows.

  Own Id: OTP-16814 Aux Id: PR-2704

- The code loader has been rewritten in order to be able to load JIT:ed code. As
  a consequence of this, it is no longer possible to load HiPE code.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16878

- Add support in the Erlang/OTP build system to generate a compilation database
  that can be used by third-party tools (such as irony in Emacs) to compile the
  erts C and C++ source code. Create the database using `make compdb`.

  Own Id: OTP-16881

- The BeamAsm JIT-compiler has been added to Erlang/OTP. The JIT-compiler is
  enabled by default on most x86 64-bit platforms that have a C++ compiler that
  can compile C++17. To verify that a JIT enabled emulator is running you can
  use [`erlang:system_info(emu_flavor)`](`m:erlang#system_info_emu_flavor`).

  For more information see the
  [internal documentation of BeamAsm in erts](BeamAsm.md).

  Own Id: OTP-16885 Aux Id: PR-2745

- By default all ERTS internal memory allocators based on `alloc_util` will now
  use their own separate carrier pool for migration of carriers instead of using
  a node global carrier pool. This was the default behavior between OTP 17 and
  OTP 21, but changed to use a node global carrier pool as of OTP 22.0. Usage of
  the node global carrier pool proved troublesome since it had a tendency to
  spread long lived blocks into allocators with normally short lived blocks
  causing increased memory fragmentation. The node global carrier pool behavior
  as well as other behaviors can be configured using the
  [`+M<S>cp`](erts_alloc.md#M_cp) command line argument.

  Own Id: OTP-16898 Aux Id: OTP-16856

- New functions have been added to the `maps` module: `merge_with/3`,
  `intersect/2`, `intersect_with/3`, `filtermap/2`, `from_keys/2`, and
  `maps:foreach/2`.

  `maps:merge_with/3` is the same as `merge/2` but takes an extra fun that is
  used to combine items with the same key.

  `maps:intersect/2` computes the intersection of two maps.

  `maps:intersect_with/3` is the same as `intersect/2` but takes an extra fun
  that is used to combine intersecting items.

  `maps:filtermap/2` allows filtering and mapping of a map in a single pass.

  `maps:from_keys/2` constructs a map from a list of keys and a single value and
  can be used to optimize sets operations such as from_list/1, filter/2,
  intersection/2, and subtract/2.

  `maps:foreach/2` allows iteration over a map without returning any value.

  Own Id: OTP-16936 Aux Id: ERL-1367

- Change `escript`s to output any errors or warnings to standard error instead
  of standard out.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16945

- A new erl parameter for specifying a file descriptor with configuration data
  has been added. This makes it possible to pass the parameter "-configfd FD"
  when executing the erl command. When this option is given, the system will try
  to read and parse configuration parameters from the file descriptor.

  Own Id: OTP-16952

- The experimental HiPE application has been removed, together with all related
  functionality in other applications.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16963

- The pretty printer for floating point number have been changed to make it
  easier to see if the integer part of the number has been rounded. After the
  change the digit that may have been rounded always appears last or just before
  the exponent character (e or E). This is accomplished by always printing the
  number using scientific notation if it is so large that the integer part could
  be rounded.

  Own Id: OTP-16980 Aux Id: ERL-1308

- The `erlang:monitor_node/2` BIF will now fail with a `notalive` exception if
  distribution has not been started on the current node; it used to fail with a
  `badarg` exception.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16987

- Accept references up to a size of 160-bits from remote nodes. This is the
  first step in an upgrade path toward using references up to 160-bits in a
  future OTP release.

  Own Id: OTP-17005 Aux Id: OTP-16718

- Accept 64-bit port identifiers from external nodes. This is the first step in
  an upgrade path toward using 64-bit port identifiers in a future OTP release.

  Own Id: OTP-17007

- One can now pass the ERL_ROOTDIR environment variable to the erl and start
  scrips. This makes it easier to use Erlang for Android apps. On Android, apps
  don't control where they will be installed.

  Own Id: OTP-17028

- All long running functions in the maps API are now yielding. In previous
  releases the functions `maps:from_list/1`, `maps:keys/1` and `maps:values/1`
  did not yield. This could cause unfair scheduling of processes.

  Own Id: OTP-17057

- `socket:sendfile/2,3,4,5` has been implemented, for platforms that support the
  underlying socket library call.

  Own Id: OTP-17154 Aux Id: OTP-16749

- The bit matching and construction syntax now supports 16-bit floats (IEEE
  754-2008).

  Own Id: OTP-17207

- Add [`process_flag(fullsweep_after, N)`](`process_flag/2`) to change
  `fullsweep_after` value in an already spawned process.

  Own Id: OTP-17285 Aux Id: PR-4651

- The `max_heap_size` error report has been updated to include the message queue
  size.

  Own Id: OTP-17293 Aux Id: PR-4672

- Introduce new types `t:nonempty_binary/0` and `t:nonempty_bitstring/0`.

  Own Id: OTP-17301 Aux Id: GH-4636

- Optimize updates of large maps with identical keys and values. E.g. in the
  example below the original `Map` will be reused as the return of the second
  update.

  `1> Map = LargeMap#{ a => b }.`

  `2> Map#{ a := b }.`

  The same optimization was done for small maps (< 33 keys) in erts-10.4 (OTP
  22.0).

  Own Id: OTP-17310 Aux Id: PR-4656

- The previously undocumented internal `-no_epmd` option has been made
  documented and public.

  Own Id: OTP-17341 Aux Id: PR-2945

- Reduce memory carrier super alignment on 64-bit architectures. In practice
  allows more fine grained control over configuration of memory carrier sizes,
  from increments of 256kb to 16kb.

  Own Id: OTP-17368

## Erts 11.2.2.18

### Fixed Bugs and Malfunctions

- Fix [`list_to_atom/1`](`list_to_atom/1`) for negative code points. Could
  either return with a positive code point or fail with an incorrect exception.

  Own Id: OTP-18321

- A race condition which was very rarely triggered could cause the signal queue
  of a process to become inconsistent causing the runtime system to crash.

  Own Id: OTP-18388 Aux Id: OTP-17462, PR-6662

- [`process_info(Pid, status)`](`process_info/2`) when `Pid /= self()` could
  return an erroneous result.

  Own Id: OTP-18421 Aux Id: PR-6806

- In rare circumstances, when a process exceeded its allowed heap size set by
  option `max_heap_size`, it would not be killed as it should be, but instead
  enter a kind of zombie state it would never get out of.

  Own Id: OTP-18463 Aux Id: PR-6858

- Implementations of the [`call()`](driver_entry.md#call) driver callback that
  returned a faulty encoded result could cause a memory leak and could cause
  invalid data on the heap of the processes calling `erlang:port_call/3`.

  Own Id: OTP-18525 Aux Id: PR-7049

- If a runtime system which was starting the distribution already had existing
  pids, ports, or references referring to a node with the same nodename/creation
  pair that the runtime system was about to use, these already existing pids,
  ports, or references would not work as expected in various situations after
  the node had gone alive. This could only occur if the runtime system was
  communicated such pids, ports, or references prior to the distribution was
  started. That is, it was extremely unlikely to happen unless the distribution
  was started dynamically and was even then very unlikely to happen. The runtime
  system now checks for already existing pids, ports, and references with the
  same nodename/creation pair that it is about to use. If such are found another
  creation will be chosen in order to avoid these issues.

  Own Id: OTP-18570 Aux Id: PR-7190

## Erts 11.2.2.17

### Fixed Bugs and Malfunctions

- [Notifications about available distribution data](`erlang:dist_ctrl_get_data_notification/1`)
  sent to distribution controller processes could be lost. Distribution
  controller processes can be used when implementing an alternative distribution
  carrier. The default distribution over tcp was not effected and the bug was
  also not present on x86/x86_64 platforms.

  Own Id: OTP-18258 Aux Id: GH-6309, PR-6324

## Erts 11.2.2.16

### Fixed Bugs and Malfunctions

- Distributed exit signals could be lost under the following conditions:

  - An exit signal from a parent process to a child process was lost if:

    - the parent process terminated before the spawn request that created the
      child had completed,
    - the spawn request set up a link between parent and child
    - the spawn request was distributed, and
    - the exit reason was larger than one machine word.

  - Loss of a connection over which a not yet completed spawn request was
    ongoing could cause loss of exit signals. Such loss of exit signals was very
    rare. Besides the above described connection loss also the following
    conditions had to be satisfied:

    - The spawn request that was interrupted by the connection loss also had to
      set up a link between the parent process and the child process.
    - The parent process that issued the spawn request also had to be
      terminating while the spawn request was interrupted by the connection
      loss.
    - The same parent process also had to have made other spawn requests to
      other nodes than to the node to which the connection was lost.
    - These spawn requests to the other nodes also had to set up links.
    - These spawn requests to the other nodes also had to be not yet completed
      at the time of the connection loss. That is, the spawn reply from the
      child process had not yet reached the parent process.

    If all the conditions above were met, exit signals to the children spawned
    due to the above described spawn requests to other nodes _could_ be lost.

    The above bug also caused a significant memory leak when it was triggered
    since the destruction of the parent process never completed.

  Own Id: OTP-18164 Aux Id: PR-6114

- A race could cause [`process_info(Pid, message_queue_len)`](`process_info/2`)
  on other processes to return invalid results.

  Own Id: OTP-18169 Aux Id: PR-6134

- Fixed reduction counting for handling process system tasks.

  Own Id: OTP-18170 Aux Id: PR-6135

- Priority elevation of terminating processes did not work which could cause
  execution of such processes to be delayed.

  Own Id: OTP-18175 Aux Id: PR-6142

- An unlink operation made by a process that terminated before the unlink
  operation completed, i.e., before it had received an unlink-ack signal from
  the linked process, caused an exit signal to erroneously be sent from the
  terminating process to the process being unlinked. This exit signal would most
  often be ignored by the receiver, but if the receiver of the exit signal
  concurrently set up a new link, it could receive the exit signal with the
  actual exit reason of the terminating process instead of a `noproc` exit
  reason. It is however very hard to detect that this has happened and has no
  obvious negative consequences, so it should be considered harmless.

  A distributed unlink-ack signal received by a terminating process was also not
  properly removed which could cause a minor memory leak.

  Own Id: OTP-18177 Aux Id: PR-6150

- The `erlang:monotonic_time/1`, `erlang:system_time/1`, `erlang:time_offset/1`,
  and `os:system_time/1` BIFs erroneously failed when passed the argument
  `native`.

  Own Id: OTP-18197 Aux Id: GH-6165, PR-6213

- Fix writing and reading of more than 2 GB in a single read/write operation on
  macOS. Before this fix attempting to read/write more than 2GB would result in
  `{error,einval}`.

  Own Id: OTP-18222 Aux Id: PR-6248 GH-6242

## Erts 11.2.2.15

### Fixed Bugs and Malfunctions

- Fix faulty distribution encoding of terms with either

  - a fun with bit-string or export-fun in its environment when encoded toward a
    not yet established (pending) connection
  - or a fun with a binary/bitstring, in its environment, referring to an
    off-heap binary (larger than 64 bytes).

  The symptom could be failed decoding on the receiving side leading to aborted
  connection. Fix OTP-18093 is a workaround for theses bugs that makes the VM
  accepts such faulty encoded funs.

  The first encoding bug toward pending connection exists only in OTP 23 and 24,
  but the second one exists also on OTP 25.

  Own Id: OTP-18104 Aux Id: OTP-18093

## Erts 11.2.2.14

### Fixed Bugs and Malfunctions

- Accept funs (NEW_FUN_EXT) with incorrectly encoded size field. This is a
  workaround for a bug (OTP-18104) existing in OTP 23 and 24 that could cause
  incorrect size fields in certain cases. The emulator does not use the decoded
  size field, but `erl_interface` still does and is not helped by this
  workaround.

  Own Id: OTP-18093 Aux Id: OTP-18104, PR-5987

- The zlib built in to the runtime system has been updated to version 1.2.12.
  (Note that on most platforms, the platform's own zlib is used.)

  Own Id: OTP-18123 Aux Id: GH-5994

- Fixed emulator crash that could happen during crashdump generation of ETS
  tables with options `ordered_set` and `{write_concurrency,true}`.

  Own Id: OTP-18144 Aux Id: GH-5981

## Erts 11.2.2.13

### Fixed Bugs and Malfunctions

- Let EPMD tolerate failure when binding to IPv4/IPv6 loopback intefaces in
  addition to user-supplied addresses via `ERL_EPMD_ADDRESS` or the `-address`
  option. This can happen, for example, if the host system has ipv6 disabled via
  the disable_ipv6 sysctl.

  Own Id: OTP-17970 Aux Id: PR-5762

- Fixed a rare bug in [`binary_to_term/1`](`binary_to_term/1`),
  `enif_make_map_from_arrays`, `erl_drv_send_term`, and Erlang distribution that
  could crash the emulator.

  Own Id: OTP-18027

- Fixed a rare race in `persistent_term:get/1,2` that could cause it to return
  the value of another key.

  Own Id: OTP-18065 Aux Id: GH-5908

- Fix bug where the process message queue was left in an inconsistent state when
  changing from on_heap to off_heap message queue data causing the GC to
  segfault.

  Own Id: OTP-18075 Aux Id: PR-5927

- Fix functions that convert universal to localtime (such as erlang:localtime/0
  and erlang:universaltime_to_localtime/1) to fetch the correct localtime if it
  is changed after the start of the VM.

  Own Id: OTP-18076 Aux Id: ERIERL-802 PR-5905

- Fix memory leak when a process doing a distributed fragmented send is sent an
  exit signal. Before this fix the receiving node would be left with an
  incomplete message that would remain until the nodes were disconnected. The
  bug has existed since Erlang/OTP 21.

  Own Id: OTP-18077 Aux Id: GH-5876 PR-5892

## Erts 11.2.2.12

### Fixed Bugs and Malfunctions

- Fix Erlang monotonic time on MacOS. Previously used OS monotonic time
  primitive on MacOS is buggy and will not be used anymore. It has been replaced
  with usage of another OS monotonic time primitive that does not appear to be
  buggy.

  Own Id: OTP-17998 Aux Id: PR-5825, GH-5554

## Erts 11.2.2.11

### Fixed Bugs and Malfunctions

- By default `global` does _not_ take any actions to restore a fully connected
  network when connections are lost due to network issues. This is problematic
  for all applications expecting a fully connected network to be provided, such
  as for example `mnesia`, but also for `global` itself. A network of
  overlapping partitions might cause the internal state of `global` to become
  inconsistent. Such an inconsistency can remain even after such partitions have
  been brought together to form a fully connected network again. The effect on
  other applications that expects that a fully connected network is maintained
  may vary, but they might misbehave in very subtle hard to detect ways during
  such a partitioning.

  In order to prevent such issues, we have introduced a _prevent overlapping
  partitions_ fix which can be enabled using the
  [`prevent_overlapping_partitions`](`e:kernel:kernel_app.md#prevent_overlapping_partitions`)
  `kernel(6)` parameter. When this fix has been enabled, `global` will actively
  disconnect from nodes that reports that they have lost connections to other
  nodes. This will cause fully connected partitions to form instead of leaving
  the network in a state with overlapping partitions. Note that this fix _has_
  to be enabled on _all_ nodes in the network in order to work properly. Since
  this quite substantially changes the behavior, this fix is currently disabled
  by default. Since you might get hard to detect issues without this fix you
  are, however, _strongly_ advised to enable this fix in order to avoid issues
  such as the ones described above. As of OTP 25 this fix will become enabled by
  default.

  Own Id: OTP-17843 Aux Id: ERIERL-732, PR-5611

- Fix memory leak when tracing on running on a process that only handle system
  tasks or non-message signals (for example process_info requests).

  Own Id: OTP-17904 Aux Id: ERIERL-757

## Erts 11.2.2.10

### Fixed Bugs and Malfunctions

- Fix GC emulator crash when `spawn_request` was used when message tracing was
  enabled.

  Own Id: OTP-17871 Aux Id: PR-5612

## Erts 11.2.2.9

### Fixed Bugs and Malfunctions

- Fixed a memory leak in `file:read_file_info/2` and `file:read_file/1` on
  Windows.

  Own Id: OTP-17827 Aux Id: GH-5527

### Improvements and New Features

- Responsiveness of processes executing on `normal` or `low` priority could
  suffer due to code purging or literal area removal on systems with a huge
  amount of processes. This since during these operations all processes on the
  system were scheduled for execution at once.

  This problem has been fixed by introducing a limit on outstanding purge and
  copy literal requests in the system. By default this limit is set to twice the
  amount of schedulers on the system. This will ensure that schedulers will have
  enough work scheduled to perform these operations as quickly as possible at
  the same time as other work will be interleaved to a much higher degree.
  Performance of these operations will however be somewhat degraded due to the
  overhead of enforcing this limit compared to when using a very large limit.

  This limit can be set by passing the `+zosrl` command line argument to `erl`,
  or by calling
  `erlang:system_flag(outstanding_system_requests_limit, NewLimit)`.

  Own Id: OTP-17796 Aux Id: ERIERL-729, PR-5473

## Erts 11.2.2.8

### Fixed Bugs and Malfunctions

- The runtime system could call `select()` with a too large timeout value when
  executing on MacOS. This could in turn cause the runtime system to crash.

  Own Id: OTP-17735 Aux Id: GH-5339

- Certain distributed signals that for various reasons must to be forced into
  the distribution buffer even when it is full would instead be lost if the
  distribution buffer was full when sent. The effected signals:

  - `EXIT` signals with exit reasons of one word size.
  - `DOWN` signals with exit reasons of one word size.
  - `demonitor` signals from a terminating process.
  - `unlink_ack` signals on OTP 23 and 24.
  - `spawn_reply` signals on OTP 23 and 24.

  Own Id: OTP-17737 Aux Id: GH-5346, GH-4989

## Erts 11.2.2.7

### Fixed Bugs and Malfunctions

- A call to [`process_info(Pid, status)`](`process_info/2`) could erroneously
  report the status `running` when it should have reported `waiting`. This
  occurred when the calling process was executing on a higher priority than the
  process being inspected. This bug has been present since OTP 21.0 (erts
  version 10.0).

  Own Id: OTP-17628

- A race between an exiting port and handling of simultaneously received signals
  to that port could cause a runtime system crash. The effected signals are
  `link`, `monitor` and `demonitor`. On OTP 22 a similar race could also cause a
  memory leak when receiving an `unlink` signal.

  Own Id: OTP-17642 Aux Id: PR-5248

- The message queue of a process entered an inconsistent state after a `receive`
  expression with an invalid timeout value was executed. If the exception raised
  due to the invalid timeout value was caught, the following `receive`
  expression executed by the process could fail to match messages already
  present in the message queue.

  On OTP 24 this could also cause the whole runtime system to crash.

  Own Id: OTP-17651 Aux Id: GH-5235, PR-5244

- Sending a `Port ! {PortOwner, close}` signal from a process other than the
  port owner could erroneously trigger a `badsig` exit signal being sent to the
  port owner process even though the correct `PortOwner` had been passed in the
  signal.

  Own Id: OTP-17665 Aux Id: PR-5248

- This fixes a bug in `erts_factory_undo` that caused the heap to not be reset
  correctly. The `erts_factory_undo` function is, for example, called when a
  [`binary_to_term/1`](`binary_to_term/1`) call fails to reset the heap to its
  state before the [`binary_to_term/1`](`binary_to_term/1`) call. This can cause
  the heap to contain invalid terms which potentially can cause issues (e.g.,
  crashes) when the whole heap is scanned.

  Own Id: OTP-17677

- Fix bug in `persistent_term` when a key-value pair contains a magic reference
  that is referred more than once. Magic references are NIF resources or
  returned from BIFs like `ets:new`, `atomics:new`. The bug could cause the
  memory of the referred resource to be prematurely deallocated.

  The bug also apply to magic references in message passing on a runtime built
  with configure option `--enable-sharing-preserving`.

  Bug exist for 64-bit since OTP-24.0 and for 32-bit since OTP-20.0.

  Own Id: OTP-17700 Aux Id: GH-5271, PR-5273

### Improvements and New Features

- The python scripts that existed in
  erts/lib_src/yielding_c_fun/lib/tiny_regex_c/scripts had a license that was
  incompatible with Erlang/OTP's license. This ticket removes these scripts that
  were not used by us.

  Own Id: OTP-17658

## Erts 11.2.2.6

### Fixed Bugs and Malfunctions

- Parsing of the result value in the native DNS resolver has been made more
  defensive against incorrect results.

  Own Id: OTP-17578 Aux Id: ERIERL-683

- On 32-bit computers, `binary_to_term/1,2` is now more resilient against
  corrupted binaries containing maps in the external format.

  Own Id: OTP-17604

## Erts 11.2.2.5

### Fixed Bugs and Malfunctions

- Fix buffer overrun problem in the tty driver. The problem happens on some
  platforms when using the CTRL+R functionality of newshell with very long
  strings in the history.

  Own Id: OTP-17560 Aux Id: GH-5116

- Fix race-condition that could cause a crash when tracing scheduling or garbage
  collections on a process that was running on a dirty scheduler.

  Own Id: OTP-17568 Aux Id: PR-4940

- Fix rare bug where re:run would crash/return invalid results when given a
  subbinary as subject.

  This bug has existed since Erlang/OTP 20.0.

  Own Id: OTP-17585 Aux Id: GH-5150

- `binary_to_term/1,2` is now more resilient against corrupted binaries
  containing maps in the external format.

  Own Id: OTP-17594

## Erts 11.2.2.4

### Fixed Bugs and Malfunctions

- Microstate accounting (msacc) and `os:perf_counter()` unintentionally used
  system time instead of monotonic time for time measurements on a lot of
  systems. These systems were all non x86/x86_64 systems or x86/x86_64 systems
  without a reliable and constant rdtsc instruction.

  The lock counting (lcnt) built runtime system also unintentionally used system
  time instead of monotonic time for time measurements on all systems.

  Own Id: OTP-17493

- Simultaneous calls to `erlang:system_flag(schedulers_online, _)` could cause
  callers to end up in a suspended state forever.

  Own Id: OTP-17500 Aux Id: GH-4809

- Fix rare race bug in memory management of distribution entries. Have been seen
  to cause VM crash when massive number of repeated concurrent failing
  connection attempts.

  Own Id: OTP-17513 Aux Id: GH-4964, PR-5015

## Erts 11.2.2.3

### Fixed Bugs and Malfunctions

- A call to `erlang:cancel_timer(_, [{info, false}])` could cause the calling
  process to block forever in the call. Note that only the synchronous version
  of the call (that is, the `async` option is `false`) in combination with the
  `info` option set to `false` was effected by this bug.

  Own Id: OTP-17472 Aux Id: PR-4932

## Erts 11.2.2.2

### Fixed Bugs and Malfunctions

- A call to `port_command()` could cause a scheduler to end up in an eternal
  loop if the port was busy and the calling process had incoming signals at the
  time of the call. This bug was introduced in OTP 23.3.2 (ERTS version 11.2.1),
  OTP 22.3.4.18 (ERTS version 10.7.2.10), and OTP 21.3.8.23 (ERTS version
  10.3.5.18).

  Own Id: OTP-17448 Aux Id: GH-4898, PR-4903, OTP-17291

- Dirty execution of a process in combination with an unlink signal from a port
  to the process could cause the signal queue of the process to enter into an
  inconsistent state. The result of the inconsistency typically caused a crash
  of the runtime system. This bug was introduced in OTP 23.3 (ERTS version
  11.2).

  Own Id: OTP-17462 Aux Id: GH-4885, PR-4914, OTP-17127

## Erts 11.2.2.1

### Fixed Bugs and Malfunctions

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## Erts 11.2.2

### Fixed Bugs and Malfunctions

- Fix bug in match spec compilator seen to cause a stack overflow crash on debug
  VM for certain match specs. Could potentially cause problems for standard VM,
  but has not been verified. Match specs are used by `ets:match/select`
  functions and `erlang:trace_pattern`.

  Own Id: OTP-17379 Aux Id: PR-4804

## Erts 11.2.1

### Fixed Bugs and Malfunctions

- The following signals could pass before other signals from the same sender to
  the same receiver. That is, these signals could arrive too early.

  - `garbage-collect` request. Sent from one process to another using one of the
    `garbage_collect/0` BIFs.
  - `check-process-code` request. Sent from one process to another using one of
    the `check_process_code()` BIFs.
  - `is-process-alive` reply. Sent as a response to a process calling the
    `is_process_alive()` BIF.
  - `process-info` reply. Sent as a response to a process calling one of the
    `process_info()` BIFs.
  - `port-command` reply. Sent as a response to a process calling one of the
    `port_command()` BIFs.
  - `port-connect` reply. Sent as a response to a process calling the
    `port_connect()` BIF.
  - `port-close` reply. Sent as a response to a process calling the
    `port_close()` BIF.
  - `port-control` reply. Sent as a response to a process calling the
    `port_control()` BIF.
  - `port-call` reply. Sent as a response to a process calling the `port_call()`
    BIF.
  - `port-info` reply. Sent as a response to a process calling one of the
    `port_info()` BIFs.

  Own Id: OTP-17291

- Fix bug in `persistent_term:get/0` and `info/0` that could cause VM crash in
  rare cases. Bug exists since OTP 23.0.

  Own Id: OTP-17298

- Fixed a bug in `zlib` where decompression would crash with `data_error` on
  some valid inputs.

  Own Id: OTP-17299 Aux Id: GH-4710

- A garbage collection of a literal area missed messages that entirely consisted
  of a term in a literal area. This could in turn lead to a crash of the runtime
  system.

  Own Id: OTP-17307

- A call to [`process_flag(message_queue_data, off_heap)`](`process_flag/2`)
  could cause a crash of the runtime system when sequential tracing was enabled.

  Own Id: OTP-17349

## Erts 11.2

### Fixed Bugs and Malfunctions

- Fix memory leak of about 6 words when `erlang:process_flag/3` is called with a
  pid of an already dead process. Bug exists since OTP-21.0.

  Own Id: OTP-17081 Aux Id: PR-2930

- Fixed small memory leak in `erl_drv_send_term` and `erl_drv_output_term` when
  failing due to the term being invalid.

  Own Id: OTP-17089 Aux Id: PR-2934

- The DTrace/SystemTap `process_heap_grow` probe is now called with valid the
  heap and stack pointers for the process in question.

  Own Id: OTP-17096 Aux Id: PR-2932

- Fixed a performance issue in memory allocation for Linux kernels that didn't
  support `MADV_FREE`.

  Own Id: OTP-17124

- A [new link protocol](erl_dist_protocol.md#new_link_protocol) has been
  introduced which prevents links from ending up in an inconsistent state where
  one participant considers itself linked while the other doesn't. This bug has
  always existed in the distributed case, but has since OTP 21 also existed in
  the node local case since the distributed link protocol then was adopted also
  for node local links. The bug could, however, only trigger if both
  participants operated on the link simultaneously.

  Own Id: OTP-17127

- Fix memory leak when receiving sigchld from port program to already dead port.

  Own Id: OTP-17163

- Fix bug where complex seq_trace tokens (that is lists, tuples, maps etc) could
  becomes corrupted by the GC. The bug was introduced in OTP-21.

  Own Id: OTP-17209 Aux Id: PR-3039

- Fixed WSLPATH environment variable addition to PATH on windows, the last
  character was lost.

  Own Id: OTP-17229

- Fixed a bug in the timer implementation which could cause timers that were set
  to more than 37.25 hours in the future to be delayed. This could occur if
  there were multiple timers scheduled to be triggered very close in time, but
  still at different times, and the scheduler thread handling the timers was not
  able to handle them quickly enough. Delayed timers were in this case triggered
  when another unrelated timer was triggered.

  Own Id: OTP-17253

- Fixed small memory leak in `erlang:trace/3` if option `{tracer,_}` is included
  and the option list is invalid or the call races with a concurrent trace or
  code change operation.

  Own Id: OTP-17265 Aux Id: PR-4596

- Fix configure check for `inet_pton` on 32-bit windows. The failure of this
  check would cause epmd to be built without ipv6 support.

  Own Id: OTP-17283

### Improvements and New Features

- Various address sanitizer support.

  Own Id: OTP-16959 Aux Id: PR-2965

- The emulator will now honor `cgroup2` CPU quotas.

  Own Id: OTP-17002

- Improved memory barrier usage on ARMv8 hardware, and specifically on Apple
  silicon.

  Own Id: OTP-17195 Aux Id: PR-4505, PR-4538

- Improved memory barrier usage on 64-bit POWER hardware.

  Own Id: OTP-17200 Aux Id: PR-4510

- Fix a file descriptor leak when using sendfile and the remote side closes the
  connection. This bug has been present since OTP-21.0.

  Own Id: OTP-17244

- Refinement of the documentation of the `message_queue_data` process flag.

  Own Id: OTP-17252 Aux Id: PR-4568

## Erts 11.1.8

### Fixed Bugs and Malfunctions

- Fixed a bug that could cause some work scheduled for execution on scheduler
  threads to be delayed until other similar work appeared. Beside delaying
  various cleanup of internal data structures also the following could be
  delayed:

  - Termination of a distribution controller process
  - Disabling of the distribution on a node
  - Gathering of memory allocator information using the `instrument` module
  - Enabling, disabling, and gathering of `msacc` information
  - Delivery of `'CHANGE'` messages when time offset is monitored
  - A call to `erlang:cancel_timer()`
  - A call to `erlang:read_timer()`
  - A call to `erlang:statistics(io | garbage_collection | scheduler_wall_time)`
  - A call to `ets:all()`
  - A call to `erlang:memory()`
  - A call to `erlang:system_info({allocator | allocator_sizes, _})`
  - A call to `erlang:trace_delivered()`

  The bug existed on runtime systems running on all types of hardware except for
  x86/x86_64.

  Own Id: OTP-17185

## Erts 11.1.7

### Improvements and New Features

- Make windows installer remove write access rights for non admin users when
  installing to a non default directory. Reduces the risk for DLL sideloading,
  but the user should always be aware of the access rights for the installation.

  Own Id: OTP-17097

## Erts 11.1.6

### Fixed Bugs and Malfunctions

- The `suspend_process()` and `resume_process()` BIFs did not check their
  arguments properly which could cause an emulator crash.

  Own Id: OTP-17080

- The runtime system would get into an infinite loop if the runtime system was
  started with more than 1023 file descriptors already open.

  Own Id: OTP-17088 Aux Id: ERIERL-580

## Erts 11.1.5

### Fixed Bugs and Malfunctions

- Fix emulator crash when sending small bit-strings over Erlang distribution
  while the connection is being setup.

  The fault was introduced in OTP-23.0

  Own Id: OTP-17083 Aux Id: ERIERL-572

## Erts 11.1.4

### Fixed Bugs and Malfunctions

- Fixed bug which could cause VM crash when a NIF is loaded at the same time as
  the Erlang implementation of the NIF is called. Bug exists since OTP 23.0.

  Own Id: OTP-16859

- Fixed `enif_make_map_*` functions in debug build when given environment from
  `enif_alloc_env`.

  Own Id: OTP-16863 Aux Id: ERL-1352

- Fixed broken configuration option `--disable-pie`.

  Own Id: OTP-16864

- Fixed rare distribution bug in race between received signal
  (link/monitor/spawn_request/spawn_reply) and disconnection. Symptom: VM crash.
  Since: OTP 21.0.

  Own Id: OTP-16869 Aux Id: ERL-1337

- Fixed a performance issue when extremely many items were stored in the process
  dictionary. (Fixing this bug also eliminates a compiler warning emitted by the
  latest version of Clang.)

  Own Id: OTP-16888

- Remove `-ftree-copyrename` from flags passed to compiler when building erts.
  The flag is not used by modern gcc's and is not supported by clang.

  Own Id: OTP-16894

- Modules using complicated nested binary comprehensions could fail to load.

  Own Id: OTP-16899

- Fixed a race in `file:read_file/1` were an incomplete file could be returned
  if another OS process swapped the file out while reading.

  Own Id: OTP-16948 Aux Id: PR-2792

- The call [`list_to_integer("10", true)`](`list_to_integer/2`) would return `4`
  instead of raising an exception. Certain other atoms would also be interpreted
  as a number base.

  Own Id: OTP-17030

- On macOS 11 (Big Sur), erl would not start if the maximum number of file
  descriptors were unlimited (`ulimit -n unlimited`).

  Own Id: OTP-17055 Aux Id: ERL-1417

### Improvements and New Features

- Add manifest to all executables and dynamic libraries.

  Own Id: OTP-17067 Aux Id: PR-2907

## Erts 11.1.3

### Fixed Bugs and Malfunctions

- Fixed a crash when exceptions were thrown during call time tracing.

  Own Id: OTP-16994

## Erts 11.1.2

### Fixed Bugs and Malfunctions

- Fixed bugs causing issues when enabling the ERTS internal allocators on a
  system built with the undocumented and unsupported `SMALL_MEMORY` feature.

  Own Id: OTP-16939

- The inet driver used to use 16 as maximum elements in an I/O vector passed to
  `writev()` (and `WSASend()` on Windows). When the data to send contained lots
  of elements, this caused a performance degradation since repeated calls to
  `writev()` had to be made to a much larger extent. The inet driver now looks
  up actual maximum amount of elements that can be used on the system, instead
  of just assuming 16. On most systems this will result in a maximum amount of
  I/O vector elements of 1024.

  As of OTP 23.0 the term encoding of signals to send over the distribution are
  encoded into I/O vectors of buffers instead of into a single buffer. Reference
  counted binaries are referred to directly from the I/O vector instead of being
  copied into the single buffer. That is, Erlang signals containing huge amounts
  of reference counted binaries was effected by this performance degradation.

  Own Id: OTP-16955 Aux Id: ERL-1343, OTP-15618

- In the distributed case, a faulty `reply` option in a call to the
  `spawn_request()` BIF erroneously caused a `badarg` exception instead of a
  `badopt` error message reply.

  Own Id: OTP-16991 Aux Id: OTP-15251

## Erts 11.1.1

### Fixed Bugs and Malfunctions

- inet:setopts(\[\{active,once\}]) wakes up IO polling thread unnecessarily,
  leading to lock contention and visibly higher CPU utilization.

  Own Id: OTP-16847 Aux Id: ERL-1301

- Two bugs in the ERTS internal thread wakeup functionality have been fixed.
  These bugs mainly hit when all threads in the system tried to go to sleep.
  When the bugs were triggered, certain operations were delayed until a thread
  woke up due to some other reason. Most important operations effected were code
  loading, persistent term updates, and memory deallocation.

  Own Id: OTP-16870

- Fixed bug in `ets:select_replace/2` on `compressed` tables that could produce
  faulty results or VM crash. Bug exists since OTP 20.

  Own Id: OTP-16874 Aux Id: ERL-1356, PR-2763

- When compiling Erlang/OTP on macOS using Xcode 12, the performance of the BEAM
  interpreter would be degraded.

  Own Id: OTP-16892

### Improvements and New Features

- As of OTP 22, the allocator specific memory carrier pools were replaced by a
  node global carrier pool. This unfortunately caused substantial memory
  fragmentation in some cases due to long lived data being spread into carriers
  used by allocators mainly handling short lived data.

  A new command line argument `+M<S>cp` has been introduced with which one can
  enable the old behavior as well as configuring other behaviors for the carrier
  pools. In order to configure the old behavior, with allocator specific carrier
  pools for all allocators, pass `+Mucp :` (including the colon character) as a
  command line argument to `erl` when starting the Erlang system.

  The default configuration for carrier pools will be changed to `+Mucp :` some
  time in the future, but not in this patch.

  Own Id: OTP-16856

## Erts 11.1

### Fixed Bugs and Malfunctions

- Update the documentation of the abstract format to use `ANNO` instead of
  `LINE`.

  Own Id: OTP-16625 Aux Id: PR-2609

- The emulator will no longer revert to the default number of schedulers when
  running under a CPU quota lower than 1 CPU.

  Own Id: OTP-16710 Aux Id: ERL-1280

- Fixed a problem with crash dumps. When a process that contained reference to
  literals internally created by the runtime system (such as the tuple returned
  by `os:type/0`), the literal would not be included in the crash dump and the
  crashdump viewer would complain about the heap being incomplete.

  Own Id: OTP-16713

- Fix configure detection of PGO for clang.

  Own Id: OTP-16738

- The to_erl program has been fixed to correctly interpret newline as only
  newline and not newline+return.

  This bug would cause the terminal to behave strangely when using lines longer
  than the terminal size.

  Own Id: OTP-16741

- A race condition when changing process priority by calling
  [`process_flag(priority, Prio)`](`process_flag/2`) could cause elevation of
  priority for a system task to be ignored. This bug hit if the system task was
  scheduled on the process calling `process_flag()` at the same time as the
  priority was changed. The bug is quite harmless and should hit very seldom if
  ever.

  Own Id: OTP-16770

- Adjust `/bin/sh` to `/system/bin/sh` in scripts when installing on Android.

  Own Id: OTP-16833 Aux Id: PR-2729

- In rare circumstances, when loading a BEAM file generated by an alternative
  code generator (not the Erlang compiler in OTP) or from handwritten or patched
  BEAM code, the loader could do an unsafe optimization.

  Own Id: OTP-16850 Aux Id: ERL-1344

- A memory and file descriptor leak in socket has been fixed. (When a newly
  opened socket that had not entered the fd into the VM's poll set (neither
  received, sent, accepted nor connected) was abandoned without closing (process
  died), after assigning a different controlling process, then a memory block
  and the file descriptor could be leaked.)

  Own Id: OTP-16857

- The documentation of [`statistics(run_queue)`](`statistics/1`) erroneously
  stated that it returns the total length of all normal run queues when it is
  the total length of all normal and dirty CPU run queues that is returned. The
  documentation has been updated to reflect the actual behavior.

  Own Id: OTP-16866 Aux Id: ERL-1355

### Improvements and New Features

- Changes in order to build on the Haiku operating system.

  Thanks to Calvin Buckley

  Own Id: OTP-16707 Aux Id: PR-2638

- When building the inet driver on Windows, there where many compiler warnings
  regarding type casting (used when calling the debug macro). This has now been
  resolved.

  Own Id: OTP-16715

- Make (use of) the socket registry optional (still enabled by default). Its now
  possible to build OTP with the socket registry turned off, turn it off by
  setting an environment variable and controlling in runtime (via function calls
  and arguments when creating sockets).

  Own Id: OTP-16763

- Change default filename encoding on android to UTF-8.

  Own Id: OTP-16821 Aux Id: PR-2733

- Clarification of the format of the atom cache header used by the distribution.

  Own Id: OTP-16848 Aux Id: PR-2737

## Erts 11.0.4

### Fixed Bugs and Malfunctions

- The ERTS internal I/O poll implementation could get into an inconsistent state
  causing input events to be ignored.

  Own Id: OTP-16780 Aux Id: PR-2701

## Erts 11.0.3

### Fixed Bugs and Malfunctions

- Fixed bug in `erlang:load_nif/2` that could cause it to throw `badarg`
  exception if a concurrent code change operation was in progress. Bug existed
  since OTP 23.0.

  Own Id: OTP-16704 Aux Id: ERL-1273, PR-16704

- Minor fix of debug compiled VM.

  Own Id: OTP-16717

- An unintentional reuse of an already used emulator internal event object could
  cause a wakeup signal to a thread to be lost. In worst case this could cause
  the runtime system to hang. This hang was however quite rare.

  Own Id: OTP-16766 Aux Id: ERL-1304

- NIF threads and driver threads on non-Linux systems leaked internal resources
  when terminating. On Windows these resources were one event per thread. On
  most other systems one mutex and one condition variable per thread. On these
  other systems that also lacked `pthread_cond_timedwait()` also a pipe with its
  file descriptors was leaked.

  Own Id: OTP-16772 Aux Id: ERL-1304

## Erts 11.0.2

### Fixed Bugs and Malfunctions

- Fixed bug when sending an export fun (eg `lists:reverse/1`) on a not yet
  established connection. It could cause VM crash. Bug exists since OTP 23.0.

  Own Id: OTP-16693 Aux Id: ERL-1254, PR-2640

## Erts 11.0.1

### Fixed Bugs and Malfunctions

- The functionality utilized by BIFs for temporary disabling of garbage
  collection while yielding could cause system task queues to become
  inconsistent on a process executing such a BIF. Process system tasks are for
  example utilized when purging code, garbage collecting literal data, and when
  issuing an ordinary garbage collection from another process.

  The bug does not trigger frequently. Multiple code purges in direct sequence
  makes it more likely that this bug is triggered. In the cases observed, this
  has resulted in a hanging code purge operation.

  Own Id: OTP-16639 Aux Id: ERL-1236

- SCTP and UDP recv/2,3 hangs indefinitely if socket is closed while recv is
  called (socket in passive mode).

  Own Id: OTP-16654 Aux Id: ERL-1242

## Erts 11.0

### Fixed Bugs and Malfunctions

- BIFs now behave like ordinary functions with regard to tracing, allowing
  `call_count` tracing and fixing a few bugs where return trace messages were
  lost when BIFs tail-called themselves or other functions ("trapping").

  Own Id: OTP-14734 Aux Id: ERL-496

- Fix various compiler warnings on 64-bit Windows.

  Own Id: OTP-15800

- `erlang:fun_info(fun foo/1, name/1)` used to return a function name based on
  the name of the function that `fun foo/1` was used in. The name returned is
  now `-fun.foo/1-`.

  Own Id: OTP-15837

- `file:allocate/3` will now update the file size on all platforms.

  Own Id: OTP-16155 Aux Id: PR-2408

- `erlang:decode_packet` with type set to `httph` no longer accepts http headers
  that have whitespaces in between the header name and the colon. That is:

  `Content-Type : text/html`

  is no longer allowed. This has been changed to conform with RFC 7230 and thus
  protect against http desync attacks.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16168 Aux Id: ERL-1053

- Fix the quoting rules in `erl -args_file`, `ERL_FLAGS`, `ERL_AFLAGS` and
  `ERL_ZFLAGS` to work as unix sh quoting.

  This bug fix can make previous configuration options to `erl` passed through
  `ERL_FLAGS`, `ERL_AFLAGS`, `ERL_ZFLAGS` or `-args_file` not be interpreted in
  the same way as before the fix.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16244 Aux Id: ERL-1051

- Fix the Erlang distribution to handle the scenario when a node connects that
  can handle message fragmentation but can not handle the atom cache. This bug
  only affects users that have implemented a custom distribution carrier. It has
  been present since OTP-21.

  The `DFLAG_FRAGMENT` distribution flag was added to the set of flags that can
  be rejected by a distribution implementation.

  Own Id: OTP-16284

- Compiling a match specification with excessive nesting caused the runtime
  system to crash due to scheduler stack exhaustion. Instead of crashing the
  runtime system, effected functions will now raise a `system_limit` error
  exception in this situation.

  Own Id: OTP-16431 Aux Id: ERL-592

- Fixed a bug that prevented Erlang from being started on Windows if it were
  installed on certain paths.

  Own Id: OTP-16478 Aux Id: ERL-1115

- Fix bug on Windows causing bad performance when standard input is closed,
  especially if the VM is only assigned one CPU core. Could be provoked for
  example by starting erl or escript via function `os:cmd/1`. Could be avoided
  with command line option `-noinput`.

  The bad performance was caused by an io thread spinning indefinitely.

  Own Id: OTP-16521 Aux Id: ERL-716

- Fixed a bug on Unix platforms that would cause `file:read_file_info/1` to
  return incorrect results if the emulator's effective user or group id differed
  from its actual ones.

  Own Id: OTP-16571

- socket: Compile problems on Android when PACKET_FASTROUTE and PACKET_USER are
  both defined and has the same value. Use of PACKET_FASTROUTE has been removed
  as it may be unused and also only for none user-land.

  Own Id: OTP-16576 Aux Id: ERL-1208

- Fixed bug in erl_crash.dump generation that could cause a SEGV core dump if a
  recently cancelled timer was found.

  Own Id: OTP-16596 Aux Id: ERL-1105, PR-2606

### Improvements and New Features

- Improved concurrency of `erlang:load_nif/2` as it does no longer block other
  schedulers from executing during initial load of a NIF library.

  Own Id: OTP-10278

- EEP-52 has been implemented.

  In binary matching, the size of the segment to be matched is now allowed to be
  a guard expression, and similarly in map matching the keys can now be guard
  expressions. See the Erlang Reference Manual and Programming Examples for more
  details.

  Language compilers or code generators that generate Core Erlang code may need
  to be updated to be compatible with the compiler in OTP 23. For more details,
  see the section Backwards Compatibility in
  [EEP 52](http://erlang.org/eeps/eep-0052.html).

  Own Id: OTP-14708

- Internally in BEAM, handling of continuation pointers has been simplified.
  This change is not user-visible, except when examining a process stack in the
  crashdump viewer. The continuation pointer for a function will now be stored
  below the y(0) for that function.

  Own Id: OTP-15077

- `seq_trace` tokens are now propagated to spawned processes.

  Own Id: OTP-15232 Aux Id: ERL-700

- Improvements of distributed spawn operations. These include both scalability
  and performance improvements as well as new functionality.

  New functionality:

  - A distributed [`spawn_monitor()`](`erlang:spawn_monitor/4`) BIF.
  - Support for `monitor` option in the distributed
    [`spawn_opt()`](`erlang:spawn_opt/5`) BIF.
  - New [`spawn_request()`](`erlang:spawn_request/5`) BIFs for asynchronous
    spawn of processes. `spawn_request()` supports all options that
    `spawn_opt()` support plus a few more.

  Own Id: OTP-15251

- Make `ets:insert/2` and `ets:insert_new/2` yield scheduler execution on long
  lists of records to insert.

  Own Id: OTP-15517 Aux Id: ERL-560

- Increased size of node incarnation numbers (aka "creation"), from 2 bits to 32
  bits. This will reduce the risk of pids/ports/refs, from different node
  incarnation with the same name, being mixed up.

  Own Id: OTP-15603

- The runtime system can now encode Erlang terms to the Erlang external term
  format as I/O vectors. The main benefit of this is that reference counted
  binaries can be referred to directly instead of copied into a new binary.

  The default Erlang distribution over TCP will always utilize this. Alternate
  distribution implementations utilizing a port as distribution controller will
  utilize this if the driver implements the [`outputv`](driver_entry.md#outputv)
  callback. Alternate Erlang distribution implementations utilizing a process as
  distribution controller will utilize this if I/O vectors are utilized by the
  functionality that processes the data returned from
  [`erlang:dist_ctrl_get_data()`](`erlang:dist_ctrl_get_data/1`).

  The return type for data returned by
  [`erlang:dist_ctrl_get_data()`](`erlang:dist_ctrl_get_data/1`) has been
  changed from `t:iodata/0` to `iovec()`. Note that `iovec()` data is valid
  `t:iodata/0` so old implementations using `erlang:dist_ctrl_get_data()` do not
  need to be changed, but may benefit from being changed depending on usage
  scenario.

  The new BIFs [`term_to_iovec/1`](`erlang:term_to_iovec/1`) and
  [`term_to_iovec/2`](`erlang:term_to_iovec/2`) have been introduced. These work
  exactly as `term_to_binary()` with the corresponding arity except the return
  type.

  Own Id: OTP-15618

- Improved ETS scalability of concurrent calls that change the size of a table,
  like `ets:insert/2` and `ets:delete/2`.

  This performance feature was implemented for `ordered_set` in OTP 22.0 and
  does now apply for all ETS table types.

  The improved scalability may come at the cost of longer latency of
  `ets:info(T,size)` and `ets:info(T,memory)`. A new table option
  `decentralized_counters` has therefore been added. It is default `true` for
  `ordered_set` with `write_concurrency` enabled and default `false` for all
  other table types.

  Own Id: OTP-15744 Aux Id: OTP-15623, PR-2229

- Directories can now be opened by `file:open/2` when passing the `directory`
  option.

  Own Id: OTP-15835 Aux Id: PR-2212

- Add Hygon Dhyana as known processor to enable support for atomic operations.

  Own Id: OTP-15840

- Make `erlang:phash2` functions consume reductions proportional to the size of
  the input term and yield scheduler when reductions are depleted.

  Own Id: OTP-15842 Aux Id: PR-2182

- Fix various build issues when compiling Erlang/OTP to the IBM AIX platform.

  Own Id: OTP-15866 Aux Id: PR-2110

- Add configure options `--enable-pie` and `--disable-pie` to control the build
  of position independent executables.

  Own Id: OTP-15868

- `file:read_file_info/2` can now be used on opened files and directories.

  Own Id: OTP-15956 Aux Id: PR-2231

- Add arity-1 versions of `atom_to_binary`, `binary_to_atom` and
  `binary_to_existing_atom`, all with `utf8` as default encoding.

  Own Id: OTP-15995 Aux Id: PR-2358

- Optimized the erts internal hash table implementation for faster lookups. The
  internal hash is used for things like; the process registry, executing
  erlang:apply/2, executing M:func(test), and more.

  Own Id: OTP-16014 Aux Id: PR-2345

- CPU quotas are now taken into account when deciding the default number of
  online schedulers, improving performance in container environments where
  quotas are applied, such as `docker` with the `--cpus` flag.

  Own Id: OTP-16105 Aux Id: ERL-927

- The `-config` option to `erl` now can take multiple config files without
  repeating the `-config` option. Example:

  erl -config sys local

  Own Id: OTP-16148 Aux Id: PR-2373

- Removed the `scheduler_poll` and `async I/O` dtrace and LTTng trace probes.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16215

- Optimized `persistent_term:put/2` and [`erase/1`](`erase/1`) to consume less
  CPU in many cases.

  Own Id: OTP-16237 Aux Id: PR-2389

- The possibility to run Erlang distribution without relying on EPMD has been
  extended. To achieve this a couple of new options to the inet distribution has
  been added.

  - **\-dist_listen false** - Setup the distribution channel, but do not listen
    for incoming connection. This is useful when you want to use the current
    node to interact with another node on the same machine without it joining
    the entire cluster.

  - **\-erl_epmd_port Port** - Configure a default port that the built-in EPMD
    client should return. This allows the local node to know the port to connect
    to for any other node in the cluster.

  The `erl_epmd` callback API has also been extended to allow returning `-1` as
  the creation which means that a random creation will be created by the node.

  In addition a new callback function called `listen_port_please` has been added
  that allows the callback to return which listen port the distribution should
  use. This can be used instead of `inet_dist_listen_min/max` if the listen port
  is to be fetched from an external service.

  Own Id: OTP-16250

- On systems without `closefrom()`, such as Linux, iterating over all possible
  file descriptors and calling `close()` for each is inefficient. This is
  markedly so when the maximum number of file descriptors has been tuned to a
  large number.

  Instead, in erl_child_setup, walk the open descriptors under `/dev/fd` and
  close only those which are open.

  This optimization affects the CPU usage of starting a new Erlang instance.

  Own Id: OTP-16270

- Optimized `maps:merge/2` for trivial cases of an empty map(s) or same map.

  Own Id: OTP-16283 Aux Id: PR-2441

- The new experimental `socket` module has been moved to the Kernel application.

  Own Id: OTP-16312

- Improved the presentation of allocations and carriers in the `instrument`
  module.

  Own Id: OTP-16327

- As announced in OTP 22.0, the previously existing limited support for VxWorks
  has now been removed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16329 Aux Id: OTP-15621

- The return value when using the `httph` and `httph_bin` option to
  `erlang:decode_packet/3` and `inet:setopts/2` has been changed to also include
  the original header unmodified. See `erlang:decode_packet/3`. Example:

  ```erlang
   >
  	    erlang:decode_packet(httph_bin,<<"HELLO:
  	    hi\r\n\r\n">>,[]).
  	    {ok,{http_header,0,<<"Hello">>,<<"HELLO">>,<<"hi">>},<<"\r\n">>}
  ```

  Own Id: OTP-16347 Aux Id: PR-2466

- Ensure `net_kernel:monitor_nodes/1` sends `nodedown` messages of a failed
  connection before `nodeup` messages of a reestablished connection toward the
  same node.

  Own Id: OTP-16362

- Update of [sequential tracing](`m:seq_trace#whatis`) to also support other
  information transfers than message passing.

  Own Id: OTP-16370 Aux Id: OTP-15251, OTP-15232

- socket: It is now possible to create a socket from an already existing file
  descriptor.

  Own Id: OTP-16398 Aux Id: ERL-1154

- socket: The socket:supports/1 function now also report if netns is supported
  or not.

  Own Id: OTP-16432

- `=:=` has been optimized to return `false` immediately when comparing two maps
  of different sizes.

  Own Id: OTP-16454

- Changed the behaviour of passing the `erl` command line argument
  [`+A 0`](erl_cmd.md#async_thread_pool_size) to silently imply `+A 1`. That is,
  it will no longer be possible to completely disable the async thread pool.
  Disabling of the async thread pool has since OTP 21 had no benefits; only lots
  of drawbacks.

  Own Id: OTP-16482

- The deprecated `erlang:get_stacktrace/0` BIF now returns an empty list instead
  of a stacktrace. To retrieve the stacktrace, use the extended try/catch syntax
  that was introduced in OTP 21. `erlang:get_stacktrace/0` is scheduled for
  removal in OTP 24.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16484

- `init:restart/1` has been introduced. `init:restart/1` can be utilized for
  changing the code loading mode during a restart.

  Own Id: OTP-16492 Aux Id: PR-2461

- Improve configure for the net nif, which should increase portability.

  Own Id: OTP-16530 Aux Id: OTP-16464

- socket: Socket counters and socket global counters are now represented as maps
  (instead of property lists).

  Own Id: OTP-16535

- Reduced the resource usage of `erlc` in parallel builds (e.g. `make -j128`).

  Own Id: OTP-16543 Aux Id: ERL-1186

- The experimental socket module has gotten restrictions removed so now the
  'seqpacket' socket type should work for any communication domain (protocol
  family) where the OS supports it, typically the Unix Domain.

  Own Id: OTP-16550 Aux Id: ERIERL-476

- Updated the internal `pcre` library to `8.44`.

  Own Id: OTP-16557

- There is now cost in terms of reductions when copying binary data using the
  binary syntax.

  Own Id: OTP-16601 Aux Id: OTP-16577

- The executable `erl_call` is now part of the `erts` distribution in addition
  to `erl_interface`.

  Own Id: OTP-16602

- Fix a buffer overflow bug that caused EPMD to consume 100% CPU after many
  nodes had been connected on the same time on NetBSD.

  Own Id: OTP-16615

- `erl -remsh` now uses the dynamic node names feature by default. See the
  [erl](erl_cmd.md) documentation for details.

  Own Id: OTP-16616

- socket: By default the socket options rcvtimeo and sndtimeo are now disabled.
  To enable these, OTP now has to be built with the configure option
  --enable-esock-rcvsndtimeo

  Own Id: OTP-16620

- The environment variable $HOME does no longer have to be set before Erlang can
  be started.

  Own Id: OTP-16635 Aux Id: ERL-476 PR-2390

## Erts 10.7.2.19

### Fixed Bugs and Malfunctions

* A race could cause [`process_info(Pid, message_queue_len)`](`process_info/2`) on other processes to return invalid results.

  Own Id: OTP-18169 Aux Id: PR-6134
* Fixed reduction counting for handling process system tasks.

  Own Id: OTP-18170 Aux Id: PR-6135
* Priority elevation of terminating processes did not work which could cause execution of such processes to be delayed.

  Own Id: OTP-18175 Aux Id: PR-6142
* The `erlang:monotonic_time/1`, `erlang:system_time/1`, `erlang:time_offset/1`, and `os:system_time/1` BIFs erroneously failed when passed the argument `native`.

  Own Id: OTP-18197 Aux Id: GH-6165, PR-6213
* Notifications about available distribution data sent to distribution controller processes could be lost. Distribution controller processes can be used when implementing an alternative distribution carrier. The default distribution over tcp was not effected and the bug was also not present on x86/x86_64 platforms.

  Own Id: OTP-18258 Aux Id: GH-6309, PR-6324

## Erts 10.7.2.18

### Fixed Bugs and Malfunctions

- Fix bug where the process message queue was left in an inconsistent state when
  changing from on_heap to off_heap message queue data causing the GC to
  segfault.

  Own Id: OTP-18075 Aux Id: PR-5927

- Fix functions that convert universal to localtime (such as erlang:localtime/0
  and erlang:universaltime_to_localtime/1) to fetch the correct localtime if it
  is changed after the start of the VM.

  Own Id: OTP-18076 Aux Id: ERIERL-802 PR-5905

- Fix memory leak when a process doing a distributed fragmented send is sent an
  exit signal. Before this fix the receiving node would be left with an
  incomplete message that would remain until the nodes were disconnected. The
  bug has existed since Erlang/OTP 21.

  Own Id: OTP-18077 Aux Id: GH-5876 PR-5892

## Erts 10.7.2.17

### Fixed Bugs and Malfunctions

- By default `global` does _not_ take any actions to restore a fully connected
  network when connections are lost due to network issues. This is problematic
  for all applications expecting a fully connected network to be provided, such
  as for example `mnesia`, but also for `global` itself. A network of
  overlapping partitions might cause the internal state of `global` to become
  inconsistent. Such an inconsistency can remain even after such partitions have
  been brought together to form a fully connected network again. The effect on
  other applications that expects that a fully connected network is maintained
  may vary, but they might misbehave in very subtle hard to detect ways during
  such a partitioning.

  In order to prevent such issues, we have introduced a _prevent overlapping
  partitions_ fix which can be enabled using the
  `prevent_overlapping_partitions` `kernel(6)` parameter. When this fix has been
  enabled, `global` will actively disconnect from nodes that reports that they
  have lost connections to other nodes. This will cause fully connected
  partitions to form instead of leaving the network in a state with overlapping
  partitions. Note that this fix _has_ to be enabled on _all_ nodes in the
  network in order to work properly. Since this quite substantially changes the
  behavior, this fix is currently disabled by default. Since you might get hard
  to detect issues without this fix you are, however, _strongly_ advised to
  enable this fix in order to avoid issues such as the ones described above. As
  of OTP 25 this fix will become enabled by default.

  Own Id: OTP-17843 Aux Id: ERIERL-732, PR-5611

- Fix memory leak when tracing on running on a process that only handle system
  tasks or non-message signals (for example process_info requests).

  Own Id: OTP-17904 Aux Id: ERIERL-757

- Fix Erlang monotonic time on MacOS. Previously used OS monotonic time
  primitive on MacOS is buggy and will not be used anymore. It has been replaced
  with usage of another OS monotonic time primitive that does not appear to be
  buggy.

  Own Id: OTP-17998 Aux Id: PR-5825, GH-5554

## Erts 10.7.2.16

### Improvements and New Features

- Responsiveness of processes executing on `normal` or `low` priority could
  suffer due to code purging or literal area removal on systems with a huge
  amount of processes. This since during these operations all processes on the
  system were scheduled for execution at once.

  This problem has been fixed by introducing a limit on outstanding purge and
  copy literal requests in the system. By default this limit is set to twice the
  amount of schedulers on the system. This will ensure that schedulers will have
  enough work scheduled to perform these operations as quickly as possible at
  the same time as other work will be interleaved to a much higher degree.
  Performance of these operations will however be somewhat degraded due to the
  overhead of enforcing this limit compared to when using a very large limit.

  This limit can be set by passing the `+zosrl` command line argument to `erl`,
  or by calling
  `erlang:system_flag(outstanding_system_requests_limit, NewLimit)`.

  Own Id: OTP-17796 Aux Id: ERIERL-729, PR-5473

## Erts 10.7.2.15

### Fixed Bugs and Malfunctions

- The runtime system could call `select()` with a too large timeout value when
  executing on MacOS. This could in turn cause the runtime system to crash.

  Own Id: OTP-17735 Aux Id: GH-5339

- Certain distributed signals that for various reasons must to be forced into
  the distribution buffer even when it is full would instead be lost if the
  distribution buffer was full when sent. The effected signals:

  - `EXIT` signals with exit reasons of one word size.
  - `DOWN` signals with exit reasons of one word size.
  - `demonitor` signals from a terminating process.
  - `unlink_ack` signals on OTP 23 and 24.
  - `spawn_reply` signals on OTP 23 and 24.

  Own Id: OTP-17737 Aux Id: GH-5346, GH-4989

## Erts 10.7.2.14

### Fixed Bugs and Malfunctions

- On 32-bit computers, `binary_to_term/1,2` is now more resilient against
  corrupted binaries containing maps in the external format.

  Own Id: OTP-17604

- A call to [`process_info(Pid, status)`](`process_info/2`) could erroneously
  report the status `running` when it should have reported `waiting`. This
  occurred when the calling process was executing on a higher priority than the
  process being inspected. This bug has been present since OTP 21.0 (erts
  version 10.0).

  Own Id: OTP-17628

- A race between an exiting port and handling of simultaneously received signals
  to that port could cause a runtime system crash. The effected signals are
  `link`, `monitor` and `demonitor`. On OTP 22 a similar race could also cause a
  memory leak when receiving an `unlink` signal.

  Own Id: OTP-17642 Aux Id: PR-5248

- The message queue of a process entered an inconsistent state after a `receive`
  expression with an invalid timeout value was executed. If the exception raised
  due to the invalid timeout value was caught, the following `receive`
  expression executed by the process could fail to match messages already
  present in the message queue.

  On OTP 24 this could also cause the whole runtime system to crash.

  Own Id: OTP-17651 Aux Id: GH-5235, PR-5244

- Sending a `Port ! {PortOwner, close}` signal from a process other than the
  port owner could erroneously trigger a `badsig` exit signal being sent to the
  port owner process even though the correct `PortOwner` had been passed in the
  signal.

  Own Id: OTP-17665 Aux Id: PR-5248

- This fixes a bug in `erts_factory_undo` that caused the heap to not be reset
  correctly. The `erts_factory_undo` function is, for example, called when a
  [`binary_to_term/1`](`binary_to_term/1`) call fails to reset the heap to its
  state before the [`binary_to_term/1`](`binary_to_term/1`) call. This can cause
  the heap to contain invalid terms which potentially can cause issues (e.g.,
  crashes) when the whole heap is scanned.

  Own Id: OTP-17677

- Fix bug in `persistent_term` when a key-value pair contains a magic reference
  that is referred more than once. Magic references are NIF resources or
  returned from BIFs like `ets:new`, `atomics:new`. The bug could cause the
  memory of the referred resource to be prematurely deallocated.

  The bug also apply to magic references in message passing on a runtime built
  with configure option `--enable-sharing-preserving`.

  Bug exist for 64-bit since OTP-24.0 and for 32-bit since OTP-20.0.

  Own Id: OTP-17700 Aux Id: GH-5271, PR-5273

## Erts 10.7.2.13

### Fixed Bugs and Malfunctions

- Fixed a bug in `zlib` where decompression would crash with `data_error` on
  some valid inputs. The bug could also cause extraction from a zip archive
  using `zip:extract()` to fail.

  Own Id: OTP-17470 Aux Id: ERIERL-657

- A call to `erlang:cancel_timer(_, [{info, false}])` could cause the calling
  process to block forever in the call. Note that only the synchronous version
  of the call (that is, the `async` option is `false`) in combination with the
  `info` option set to `false` was effected by this bug.

  Own Id: OTP-17472 Aux Id: PR-4932

- Microstate accounting (msacc) and `os:perf_counter()` unintentionally used
  system time instead of monotonic time for time measurements on a lot of
  systems. These systems were all non x86/x86_64 systems or x86/x86_64 systems
  without a reliable and constant rdtsc instruction.

  The lock counting (lcnt) built runtime system also unintentionally used system
  time instead of monotonic time for time measurements on all systems.

  Own Id: OTP-17493

- Simultaneous calls to `erlang:system_flag(schedulers_online, _)` could cause
  callers to end up in a suspended state forever.

  Own Id: OTP-17500 Aux Id: GH-4809

- Fix rare race bug in memory management of distribution entries. Have been seen
  to cause VM crash when massive number of repeated concurrent failing
  connection attempts.

  Own Id: OTP-17513 Aux Id: GH-4964, PR-5015

- Fix buffer overrun problem in the tty driver. The problem happens on some
  platforms when using the CTRL+R functionality of newshell with very long
  strings in the history.

  Own Id: OTP-17560 Aux Id: GH-5116

- Fix race-condition that could cause a crash when tracing scheduling or garbage
  collections on a process that was running on a dirty scheduler.

  Own Id: OTP-17568 Aux Id: PR-4940

- Fix rare bug where re:run would crash/return invalid results when given a
  subbinary as subject.

  This bug has existed since Erlang/OTP 20.0.

  Own Id: OTP-17585 Aux Id: GH-5150

- `binary_to_term/1,2` is now more resilient against corrupted binaries
  containing maps in the external format.

  Own Id: OTP-17594

## Erts 10.7.2.12

### Fixed Bugs and Malfunctions

- A call to `port_command()` could cause a scheduler to end up in an eternal
  loop if the port was busy and the calling process had incoming signals at the
  time of the call. This bug was introduced in OTP 23.3.2 (ERTS version 11.2.1),
  OTP 22.3.4.18 (ERTS version 10.7.2.10), and OTP 21.3.8.23 (ERTS version
  10.3.5.18).

  Own Id: OTP-17448 Aux Id: GH-4898, PR-4903, OTP-17291

## Erts 10.7.2.11

### Fixed Bugs and Malfunctions

- Fix bug in match spec compilator seen to cause a stack overflow crash on debug
  VM for certain match specs. Could potentially cause problems for standard VM,
  but has not been verified. Match specs are used by `ets:match/select`
  functions and `erlang:trace_pattern`.

  Own Id: OTP-17379 Aux Id: PR-4804

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## Erts 10.7.2.10

### Fixed Bugs and Malfunctions

- The following signals could pass before other signals from the same sender to
  the same receiver. That is, these signals could arrive too early.

  - `garbage-collect` request. Sent from one process to another using one of the
    `garbage_collect/0` BIFs.
  - `check-process-code` request. Sent from one process to another using one of
    the `check_process_code()` BIFs.
  - `is-process-alive` reply. Sent as a response to a process calling the
    `is_process_alive()` BIF.
  - `process-info` reply. Sent as a response to a process calling one of the
    `process_info()` BIFs.
  - `port-command` reply. Sent as a response to a process calling one of the
    `port_command()` BIFs.
  - `port-connect` reply. Sent as a response to a process calling the
    `port_connect()` BIF.
  - `port-close` reply. Sent as a response to a process calling the
    `port_close()` BIF.
  - `port-control` reply. Sent as a response to a process calling the
    `port_control()` BIF.
  - `port-call` reply. Sent as a response to a process calling the `port_call()`
    BIF.
  - `port-info` reply. Sent as a response to a process calling one of the
    `port_info()` BIFs.

  Own Id: OTP-17291

- A garbage collection of a literal area missed messages that entirely consisted
  of a term in a literal area. This could in turn lead to a crash of the runtime
  system.

  Own Id: OTP-17307

- A call to [`process_flag(message_queue_data, off_heap)`](`process_flag/2`)
  could cause a crash of the runtime system when sequential tracing was enabled.

  Own Id: OTP-17349

## Erts 10.7.2.9

### Fixed Bugs and Malfunctions

- Fixed a bug in the timer implementation which could cause timers that were set
  to more than 37.25 hours in the future to be delayed. This could occur if
  there were multiple timers scheduled to be triggered very close in time, but
  still at different times, and the scheduler thread handling the timers was not
  able to handle them quickly enough. Delayed timers were in this case triggered
  when another unrelated timer was triggered.

  Own Id: OTP-17253

- Fix bug in call_time tracing (used by eprof) that could cause VM crash. Bug
  exists since OTP-22.2 (but not in OTP-23).

  Own Id: OTP-17290 Aux Id: GH-4635

### Improvements and New Features

- Fix a file descriptor leak when using sendfile and the remote side closes the
  connection. This bug has been present since OTP-21.0.

  Own Id: OTP-17244

## Erts 10.7.2.8

### Fixed Bugs and Malfunctions

- Fixed a bug that could cause some work scheduled for execution on scheduler
  threads to be delayed until other similar work appeared. Beside delaying
  various cleanup of internal data structures also the following could be
  delayed:

  - Termination of a distribution controller process
  - Disabling of the distribution on a node
  - Gathering of memory allocator information using the `instrument` module
  - Enabling, disabling, and gathering of `msacc` information
  - Delivery of `'CHANGE'` messages when time offset is monitored
  - A call to `erlang:cancel_timer()`
  - A call to `erlang:read_timer()`
  - A call to `erlang:statistics(io | garbage_collection | scheduler_wall_time)`
  - A call to `ets:all()`
  - A call to `erlang:memory()`
  - A call to `erlang:system_info({allocator | allocator_sizes, _})`
  - A call to `erlang:trace_delivered()`

  The bug existed on runtime systems running on all types of hardware except for
  x86/x86_64.

  Own Id: OTP-17185

## Erts 10.7.2.7

### Fixed Bugs and Malfunctions

- The `suspend_process()` and `resume_process()` BIFs did not check their
  arguments properly which could cause an emulator crash.

  Own Id: OTP-17080

- The runtime system would get into an infinite loop if the runtime system was
  started with more than 1023 file descriptors already open.

  Own Id: OTP-17088 Aux Id: ERIERL-580

## Erts 10.7.2.6

### Fixed Bugs and Malfunctions

- Fixed rare distribution bug in race between received signal
  (link/monitor/spawn_request/spawn_reply) and disconnection. Symptom: VM crash.
  Since: OTP 21.0.

  Own Id: OTP-16869 Aux Id: ERL-1337

## Erts 10.7.2.5

### Fixed Bugs and Malfunctions

- Fixed bugs causing issues when enabling the ERTS internal allocators on a
  system built with the undocumented and unsupported `SMALL_MEMORY` feature.

  Own Id: OTP-16939

## Erts 10.7.2.4

### Fixed Bugs and Malfunctions

- inet:setopts(\[\{active,once\}]) wakes up IO polling thread unnecessarily,
  leading to lock contention and visibly higher CPU utilization.

  Own Id: OTP-16847 Aux Id: ERL-1301

- The documentation of [`statistics(run_queue)`](`statistics/1`) erroneously
  stated that it returns the total length of all normal run queues when it is
  the total length of all normal and dirty CPU run queues that is returned. The
  documentation has been updated to reflect the actual behavior.

  Own Id: OTP-16866 Aux Id: ERL-1355

- Two bugs in the ERTS internal thread wakeup functionality have been fixed.
  These bugs mainly hit when all threads in the system tried to go to sleep.
  When the bugs were triggered, certain operations were delayed until a thread
  woke up due to some other reason. Most important operations effected were code
  loading, persistent term updates, and memory deallocation.

  Own Id: OTP-16870

- Fixed bug in `ets:select_replace/2` on `compressed` tables that could produce
  faulty results or VM crash. Bug exists since OTP 20.

  Own Id: OTP-16874 Aux Id: ERL-1356, PR-2763

### Improvements and New Features

- As of OTP 22, the allocator specific memory carrier pools were replaced by a
  node global carrier pool. This unfortunately caused substantial memory
  fragmentation in some cases due to long lived data being spread into carriers
  used by allocators mainly handling short lived data.

  A new command line argument `+M<S>cp` has been introduced with which one can
  enable the old behavior as well as configuring other behaviors for the carrier
  pools. In order to configure the old behavior, with allocator specific carrier
  pools for all allocators, pass `+Mucp :` (including the colon character) as a
  command line argument to `erl` when starting the Erlang system.

  The default configuration for carrier pools will be changed to `+Mucp :` some
  time in the future, but not in this patch.

  Own Id: OTP-16856

## Erts 10.7.2.3

### Fixed Bugs and Malfunctions

- The ERTS internal I/O poll implementation could get into an inconsistent state
  causing input events to be ignored.

  Own Id: OTP-16780 Aux Id: PR-2701

## Erts 10.7.2.2

### Fixed Bugs and Malfunctions

- An unintentional reuse of an already used emulator internal event object could
  cause a wakeup signal to a thread to be lost. In worst case this could cause
  the runtime system to hang. This hang was however quite rare.

  Own Id: OTP-16766 Aux Id: ERL-1304

- NIF threads and driver threads on non-Linux systems leaked internal resources
  when terminating. On Windows these resources were one event per thread. On
  most other systems one mutex and one condition variable per thread. On these
  other systems that also lacked `pthread_cond_timedwait()` also a pipe with its
  file descriptors was leaked.

  Own Id: OTP-16772 Aux Id: ERL-1304

## Erts 10.7.2.1

### Fixed Bugs and Malfunctions

- Fixed bug in erl_crash.dump generation that could cause a SEGV core dump if a
  recently cancelled timer was found.

  Own Id: OTP-16596 Aux Id: ERL-1105, PR-2606

- The functionality utilized by BIFs for temporary disabling of garbage
  collection while yielding could cause system task queues to become
  inconsistent on a process executing such a BIF. Process system tasks are for
  example utilized when purging code, garbage collecting literal data, and when
  issuing an ordinary garbage collection from another process.

  The bug does not trigger frequently. Multiple code purges in direct sequence
  makes it more likely that this bug is triggered. In the cases observed, this
  has resulted in a hanging code purge operation.

  Own Id: OTP-16639 Aux Id: ERL-1236

- SCTP and UDP recv/2,3 hangs indefinitely if socket is closed while recv is
  called (socket in passive mode).

  Own Id: OTP-16654 Aux Id: ERL-1242

## Erts 10.7.2

### Fixed Bugs and Malfunctions

- A literal area could prematurely be released before all uses of it had been
  removed. This occurred either when a terminating process had a complex exit
  reason referring to a literal that concurrently was removed, or when a
  terminating process continued executing a dirty NIF accessing a literal (via
  the heap) that concurrently was removed.

  Own Id: OTP-16640 Aux Id: OTP-16193

- The VM could potentially crash when checking process code of a process that
  terminated while executing a dirty NIF. The checking of process code is part
  of a code purge operation.

  Own Id: OTP-16641

- System tasks of `low` priority were not interleaved with `normal` priority
  system tasks as they should. This could potentially delay garbage collection
  of another process longer than intended if the garbage collection was
  requested from a `low` priority process.

  Own Id: OTP-16642

## Erts 10.7.1

### Fixed Bugs and Malfunctions

- [re:run(Subject, RE, \[unicode])](`re:run/3`) returned `nomatch` instead of
  failing with a `badarg` error exception when `Subject` contained illegal utf8
  and `RE` was passed as a binary. This has been corrected along with
  corrections of reduction counting in `re:run()` error cases.

  Own Id: OTP-16553

- Fixed a bug that could cause the emulator to crash when purging modules or
  persistent terms.

  Own Id: OTP-16555 Aux Id: ERL-1188

- Fixed a bug in a receive optimization. This could cause a `receive` not to
  match even though a matching message was present in the message queue. This
  bug was introduced in ERTS version 10.6 (OTP 22.2).

  Own Id: OTP-16572 Aux Id: ERL-1199, OTP-16269

## Erts 10.7

### Fixed Bugs and Malfunctions

- `gen_udp:recv(S, N, 0)` corrupted the internal state for the socket so after
  receive with `{active, once}` it always returned `{error, timeout}`, and
  wasted the received UDP packets. This bug has now been fixed.

  Reported and pinpointed by Alexander Petrovsky.

  Own Id: OTP-16059

- The atom esock_atom_user and esock_atom_kernel was never actually created.
  This has now been corrected.

  Own Id: OTP-16381

- Fixed bug in `socket` module that could potentially lead to scheduler
  deadlocks.

  Own Id: OTP-16384

- Fix bug causing VM crash if `down` or `stop` callback of a NIF resource is
  called after the NIF module has been purged. The fix will postpone unloading
  until all such resource objects have been garbage collected.

  Own Id: OTP-16399

- Fixed bug in `ets:update_counter/4`, when called with an invalid `UpdateOp`
  and a `Key` that does not exist, causing `ets:info(T,size)` to return
  incorrect values. Bug exists since OTP-19.0.2.

  Own Id: OTP-16404 Aux Id: ERL-1127

- Fix potential heap corrupting bugs when a process calls a BIF that blocks
  other normal schedulers and then writes to its own heap without the main
  process lock. A NIF running on a dirty scheduler trying to interact with such
  a process could corrupt its heap. The fixed BIFs were related to code loading
  and tracing.

  Own Id: OTP-16417

- Fixed bug in `erlang:list_to_ref/1` when called with a reference created by a
  remote note. Function [`list_to_ref/1`](`list_to_ref/1`) is intended for
  debugging and not to be used in application programs. Bug exist since OTP
  20.0.

  Own Id: OTP-16438

- The prim_net nif (net/kernel) made use of an undefined atom, notsup. This has
  now been corrected.

  Own Id: OTP-16440

- Corrected the valid range of the `erl` command line argument
  [`+SDio <NumberOfDirtyIoSchedulers>`](erl_cmd.md#%2BSDio) from `0..1024` to
  `1..1024`. `+SDio 0` was erroneously allowed which just caused the VM to crash
  on the first dirty I/O job scheduled.

  Own Id: OTP-16481

- Fix a crash when attempting to log faults when loading files during early
  boot.

  Own Id: OTP-16491

### Improvements and New Features

- For [socket](`m:socket#`), not all send and receive flags are supported on all
  platforms. In order to (at least) simplify testing, the
  socket:supports/0,1,2,3 functions has been extended with send_flags and
  recv_flags items, which indicates what the current platform can manage.

  Own Id: OTP-16153

- Add a "full featured" version of getifaddrs in the net module.

  Own Id: OTP-16212 Aux Id: ERL-1070

- The options `busy_limits_port` and `busy_limits_msgq` have been added to the
  BIF `erlang:open_port/2`. The `busy_limits_port` option can be used for
  controlling the busy state of a port executing the `spawn_driver` or the
  `fd_driver`. The `busy_limits_msgq` option can be used for controlling the
  busy state of the port message queue.

  Own Id: OTP-16306 Aux Id: ERIERL-439

- A socket "registry" has been added making it possible to list current open
  sockets.

  Own Id: OTP-16309

- The counters managed by the socket nif has been extended. Their "size" has
  been increased from 32 bit to 64. Two max package size (for read and write)
  has been added. And four accept counters has been added.

  Own Id: OTP-16387

- Add gcc option `-fno-common` to detect accidental name clashes of global
  variables.

  Own Id: OTP-16420 Aux Id: PR-2513

- New dynamic lock checker that verifies locking orders and detects potential
  deadlock bugs in drivers and NIFs. Enabled together with the old static lock
  checker (for ERTS internal locks) in emulator started with `-emu_type debug`
  or built with configuration option `--enable-lock-checking`.

  Own Id: OTP-16427

## Erts 10.6.4

### Fixed Bugs and Malfunctions

- A process could get into an inconsistent state where it was runnable, but
  never scheduled for execution. This could occur when a mix of `normal` and
  `low` priority processes where scheduled on the same type of dirty scheduler
  simultaneously.

  Own Id: OTP-16446 Aux Id: ERL-1157

## Erts 10.6.3

### Fixed Bugs and Malfunctions

- A process could end up in a state where it got endlessly rescheduled without
  making any progress. This occurred when a system task, such as check of
  process code (part of a code purge), was scheduled on a high priority process
  trying to execute on a dirty scheduler.

  Own Id: OTP-16436 Aux Id: ERL-1152

### Improvements and New Features

- Improved signal handling for processes executing dirty. For example, avoid
  busy wait in dirty signal handler process when process is doing garbage
  collection on dirty scheduler.

  Own Id: OTP-16358

## Erts 10.6.2

### Fixed Bugs and Malfunctions

- Taking a scheduler offline could cause timers set while executing on that
  scheduler to be delayed until the scheduler was put online again. This bug was
  introduced in ERTS version 10.0 (OTP 21.0).

  Own Id: OTP-16371

- The `ets:update_counter/4` core dumped when given an ordered_set with
  write_concurrency enabled and an invalid position. This bug has been fixed.

  Own Id: OTP-16378 Aux Id: ERL-1125

- A process calling
  [`erlang:system_flag(multi_scheduling, block)`](`m:erlang#system_flag_multi_scheduling`)
  could end up blocked waiting for the operation to complete indefinitely.

  Own Id: OTP-16379

### Improvements and New Features

- Duplicate entries for \[socket:]getopt and \[socket:]setopt in man page.

  Own Id: OTP-16333 Aux Id: ERL-1104

## Erts 10.6.1

### Fixed Bugs and Malfunctions

- Corrected an issue with the new socket api which could cause a core dump. A
  race during socket close could cause a core dump (an invalid nif environment
  free).

  Own Id: OTP-16314 Aux Id: ERL-1098

- Corrected an issue with the new socket api which could cause a core dump. When
  multiple accept processes waiting for a connect a connect could cause a core
  dump.

  Own Id: OTP-16359

## Erts 10.6

### Fixed Bugs and Malfunctions

- The functions [`unicode:characters_to_list()`](`unicode:characters_to_list/2`)
  and [`unicode:characters_to_binary()`](`unicode:characters_to_binary/3`)
  raised a `badarg` exception instead of returning an error tuple when passed
  very large invalid code points as input.

  Own Id: OTP-16052

- `file:allocate/3` will now work properly on Mac OS.

  Own Id: OTP-16074 Aux Id: ERL-1042, PR-2386

- For [socket](`m:socket#`), invalid encoding of send and receive flags caused
  badarg and send failure.

  Own Id: OTP-16149

- Fix a bug in `re` on platforms where the native stack grows upwards, for
  instance on HP PA-RISC.

  Own Id: OTP-16150 Aux Id: ERL-1043

- For [socket](`m:socket#`), unable to properly decode the timestamp control
  message header on FreeBSD. We incorrectly used the SO_TIMESTAMP flag for the
  timestamp control message header type. It should have been SCM_TIMESTAMP. This
  caused the timestamp control message header to not be fully decoded.

  Own Id: OTP-16151 Aux Id: #2400

- For [socket](`m:socket#`), when setting the ip option 'recvtos' to true,
  thereby indicating that we want to receive the TOS control message header, we
  don't actually get TOS but RECVTOS on FreeBSD (and maybe others)\! This was
  previously not handled.

  Own Id: OTP-16152 Aux Id: OTP-16114

- Fixed a bug where the emulator would print out error messages forever if it
  crashed during startup.

  Own Id: OTP-16159 Aux Id: ERL-1060

- For [socket](`m:socket#`), wrong type for protocol caused segmentation fault
  if protocol was provided as \{raw, integer()\}.

  Own Id: OTP-16163 Aux Id: ERL-1061

- For [socket](`m:socket#`), when setting the ip option 'recvttl' to true,
  thereby indicating that we want to receive the TTL control message header, we
  don't actually get TTL but RECVTTL on Solaris (and maybe others)\! This was
  previously not handled.

  Own Id: OTP-16172 Aux Id: OTP-16114

- For [socket](`m:socket#`), IPv6 control message headers was incorrectly
  decoded with level ip instead of ipv6.

  Own Id: OTP-16173

- Fix configure bug where `clock_getres` would not correctly be detected causing
  `erlang:system_info(os_system_time_source)` to return an incorrect resolution.

  This bug effects all OSs that use `clock_gettime` to get time (basically all
  OSs except windows) and has been present since OTP-19.2.

  Own Id: OTP-16191 Aux Id: ERL-1067

- Fix bugs when using `ERLC_USE_SERVER=true` on Windows when building
  Erlang/OTP.

  Change the cookie used by the compile server to be hard coded instead of using
  the users cookie.

  Own Id: OTP-16192

- Fix so that `persistent_term` is listed in the erts application file.

  Own Id: OTP-16194

- Fix bug in call_time tracing which could sometimes cause call time
  measurements being attributed to the wrong function.

  Own Id: OTP-16195 Aux Id: ERL-1027

- The [socket](`m:socket#`) socket option 'peek_off' has been disabled. If
  peek_off was set and then socket:recv/3 was called with the peek flag, the
  call could hang.

  Own Id: OTP-16196

- Handle possibly uninitiated (control message header) data.

  Own Id: OTP-16197

- For [net](`m:net#`), a couple of NI macros was deprecated in new versions of
  glibc, so these are no longer used (IDN_ALLOW_UNASSIGNED and
  IDN_USE_STD3_ASCII_RULES).

  Own Id: OTP-16203

- The string value from a get operation needed to be truncated.

  Own Id: OTP-16204

- Fix misleading seq_trace message for remote send operation toward named
  process. Changed receiver to be `{Name,Node}` instead of just `Name`.

  Own Id: OTP-16206 Aux Id: PR-2423

- Fixed a bug causing actual nodedown reason reported by
  [`net_kernel:monitor_nodes(true, [nodedown_reason])`](`net_kernel:monitor_nodes/2`)
  to be lost and replaced by the reason `killed`.

  Own Id: OTP-16216

- For [socket](`m:socket#`), the timestamp creation used when printing warning
  messages and debug printouts did not work. The used buffer was too small.

  Own Id: OTP-16223

- Fix compiler warnings generated by GCC 8

  Own Id: OTP-16235

- Fix `erl -emu_type` to accept the option `opt` which means the normal
  optimized emulator. This can be used to override the `-emu_type` option from
  environment variables.

  Own Id: OTP-16297

- Corrected the function spec to reflect the actual code of the function
  socket:bind.

  Own Id: OTP-16310 Aux Id: ERL-1082

- Fixed a performance issue in ETS lookup when using the `compressed` option and
  the term contained atoms. Before this fix the decompress algorithm for atoms
  would unnecessarily take a global lock to validate the atom.

  Own Id: OTP-16316

### Improvements and New Features

- For [socket](`m:socket#`), not all send and receive flags are supported on all
  platforms. In order to (at least) simplify testing, the
  socket:supports/0,1,2,3 functions has been extended with send_flags and
  recv_flags items, which indicates what the current platform can manage.

  Own Id: OTP-16153

- For [socket](`m:socket#`), add support for IPv6 socket options tclass and
  recvtclass. Both has been added, but the use of them are platform dependent.
  Call socket:supports(options, ipv6, Opt) to be sure what option to use in
  order to request a TCLASS control message header.

  Own Id: OTP-16180

- For [socket](`m:socket#`), the TCP socket option cork was not supported even
  though the supports function reported it as such.

  Own Id: OTP-16205

- The Kernel application's User's Guide now contain a Logger Cookbook with
  common usage patterns.

  Own Id: OTP-16208

- Add a "full featured" version of getifaddrs in the net module.

  Own Id: OTP-16212 Aux Id: ERL-1070

- Fix an inefficiency introduced by the bug fix in OTP-16241 which fixed a
  problem with the receive optimization. That bug fix introduced an inefficiency
  when using the receive optimization and a burst of messages arrive at once
  when the receiving process' mailbox was empty.

  Own Id: OTP-16269 Aux Id: OTP-16241

- For [socket](`m:socket#`), added support for the socket option extended_err.
  Andreas Schultz.

  Own Id: OTP-16302 Aux Id: #2449

- ETS tables have been optimized to not use any locks when running in a system
  with only one scheduler enabled. This can provide significant performance
  gains for applications that use ETS tables heavily.

  Own Id: OTP-16315

## Erts 10.5.6

### Fixed Bugs and Malfunctions

- Large amounts of quickly executed dirty work could cause heavy contention on
  an internal spin lock. The spin lock was replaced by a mutex which behaves
  much better under these conditions.

  Own Id: OTP-16301 Aux Id: ERL-1079

## Erts 10.5.5

### Fixed Bugs and Malfunctions

- A literal area could prematurely be released before all uses of it had been
  removed. This occurred either when a terminating process had a complex exit
  reason referring to a literal that concurrently was removed, or when a
  terminating process continued executing a dirty NIF accessing a literal (via
  the heap) that concurrently was removed.

  Own Id: OTP-16193

- Fix bug causing VM crash due to memory corruption of distribution entry.
  Probability of crash increases if Erlang distribution is frequently
  disconnected and reestablished towards same node names. Bug exists since
  OTP-21.0.

  Own Id: OTP-16224 Aux Id: ERL-1044

- Fixed bug causing crash of VM built with configuration
  `--enable--sharing-preserving`. Provoked when a sent message contains both a
  bit string and the heap binary (< 65 bytes) which the bit string was matched
  from. Bug exists since OTP-19.0 but has seen to be easier to provoke since
  OTP-22.1.

  Own Id: OTP-16265 Aux Id: ERL-1064

## Erts 10.5.4

### Fixed Bugs and Malfunctions

- The compiler could do an unsafe optimization of receives, which would cause a
  receive to only scan part of the message queue.

  This bug fix in the compiler fixes a bug in the socket module.

  Own Id: OTP-16219 Aux Id: ERL-1076

- Fix bug where the receive marker used by the runtime to do the receive queue
  optimization could be incorrectly set. The symptom of this would be that a
  message that should match in a receive never matches.

  The bug requires the OTP-22 compiler and multiple selective receives to
  trigger. See OTP-16219 for details about the bug fix in the compiler.

  Own Id: OTP-16241 Aux Id: ERL-1076 OTP-16219

## Erts 10.5.3

### Fixed Bugs and Malfunctions

- Erlang/OTP can now be built on macOS Catalina (10.15).

  Own Id: OTP-16177 Aux Id: ERL-1063

## Erts 10.5.2

### Improvements and New Features

- Added the environment variable `ERLC_SERVER_ID`, which allows multiple compile
  servers to run separately under the same user.

  Own Id: OTP-16125 Aux Id: ERIERL-412

## Erts 10.5.1

### Fixed Bugs and Malfunctions

- A terminating process sending distributed `'EXIT'` or `'DOWN'` signals while
  terminating could end up in a state where no progress at all was made. This
  was triggered by a distribution channel that the terminating process was
  sending on got busy. This bug has existed since ERTS version 10.4 (OTP 22.0).

  Own Id: OTP-16069

- When communicating with a simultaneously exiting port via the
  `erlang:port_*()` BIFs one could sometimes get stray `{Ref, What}` messages.
  Where `Ref` was a reference and `What` usually were the atom `badarg`.

  Own Id: OTP-16107 Aux Id: ERL-1049

## Erts 10.5

### Fixed Bugs and Malfunctions

- If you set `{linger,{true,0}}` on a `gen_tcp` listen socket, accept a
  connection on that socket, and then close the accepted socket, now the linger
  zero setting is transferred to the accepted socket. Before this correction
  that information was lost and the close behaviour on the accepted socket
  incorrect.

  Own Id: OTP-15370 Aux Id: ERIERL-353

- Sending ancillary data implemented in OTP-15747 accidentally left behind test
  code that caused all UDP sends to fail on Windows. This has now been fixed.

  Own Id: OTP-15422 Aux Id: OTP-15747

- In the socket nif, used invalid flags when if-def'ing for supported TCP flags:
  TCP_MAXSEG and TCP_NODELAY (the support function).

  Own Id: OTP-15827

- Fixed memory leaks in experimental socket module.

  Own Id: OTP-15830

- `re:run()` now yields when validating utf8 in a large subject.

  Own Id: OTP-15836 Aux Id: ERL-876

- Fixed bug in `seq_trace:set_token(label,Term)` which could cause VM crash if
  `Term` was heap allocated (not an atom, small integer, local pid or port). Bug
  exists since OTP 21.0 when terms other than small integers were first allowed
  as labels.

  Own Id: OTP-15849 Aux Id: ERL-700

- Extra `-mode` flags given to `erl` are ignored with a warning.

  Own Id: OTP-15852

- Don't loop indefinitely when `--enable-pgo` is given to configure, but
  compiler does not support pgo.

  Own Id: OTP-15853 Aux Id: PR-2254

- Fix `seq_trace:print/2` not to raise `badarg` exception if label is not a
  small integer. Bug exists since OTP 21.0.

  Own Id: OTP-15859 Aux Id: ERL-700

- Fixed hipe_flush_icache_range for non-Linux OS on ARM.

  Own Id: OTP-15874 Aux Id: ERL-958, PR-2266

- The fix in OTP-15871 was too conservative and disabled the offending load-time
  optimization in some cases where it was safe.

  Own Id: OTP-15881

- Upgraded the ERTS internal PCRE library from version 8.42 to version 8.43. See
  [http://pcre.org/original/changelog.txt](http://pcre.org/original/changelog.txt)
  for information about changes made to PCRE. This library implements major
  parts of the `m:re` regular expressions module.

  Own Id: OTP-15889

- Fix race condition when closing a socket while using `{active,N}` on Windows.

  Own Id: OTP-15901 Aux Id: ERL-960 PR-2272

- Allow more than one `-config` command line option to `erl` on Windows to
  conform with other OS.

  Own Id: OTP-15918 Aux Id: ERL-912

- Fix so that ERL_FLAGS environment variable does not interfere with command
  line arguments. Before this fix you could write:

  `ERL_FLAGS="10" erl +S`

  and erlang would start as if `+S` had been given the argument `10`.

  Own Id: OTP-15931

- The bug with ID ERL-717 has been fixed. The functions `io:columns()` and
  `io:rows()` only worked correctly inside interactive erlang shells before this
  fix. These functions returned `{error,enotsup}` before this fix even if stdout
  and stdin were connected to a terminal when they were invoked from an escript
  or a program started with e.g., `erl -noshell`.

  Own Id: OTP-15959 Aux Id: ERL-717

- Do not use named label in `ethread.c` inline assemble. This allows erts to be
  compiled using gcc 9.1.0 with LTO enabled.

  Own Id: OTP-15971 Aux Id: PR-2333

- `erlang:fun_to_list/1` will now escape the module and function name when
  necessary.

  Own Id: OTP-15975 Aux Id: ERL-1009

- [`process_info(P,binary)`](`process_info/2`) would neglect to look through
  heap fragments, potentially missing a few binaries associated with the
  process.

  Own Id: OTP-15978 Aux Id: ERIERL-366

- HiPE is now automatically disabled on systems with non-glibc implementation
  (for instance musl). This is because musl does not provide the API's for
  guaranteeing that signals are delivered on the correct native stack.

  Own Id: OTP-16037

- Fixed bug triggered if a process is killed during call to
  `persistent_term:put` or `persistent_term:erase`.

  Own Id: OTP-16041

- Add units to all memory slogans in the crash dump documentation.

  Own Id: OTP-16042

- Fix a bug in `binary_to_term` that would crash the emulator if a term larger
  than 16GB was to be decoded.

  Own Id: OTP-16058 Aux Id: PR-2382

- Fixed bug related to an exiting process sending EXIT and DOWN signals to
  remote linked/monitored processes. Bugs exists since OTP 22.0.

  Own Id: OTP-16060

### Improvements and New Features

- `erlc` can now automatically use a compile server to avoid starting an Erlang
  system for each file to be compiled in a multi-file project. See the
  documentation for how to enable it.

  Own Id: OTP-15738 Aux Id: PR-2361

- The possibility to send ancillary data, in particular the TOS field, has been
  added to `gen_udp:send/4,5`.

  Own Id: OTP-15747 Aux Id: ERIERL-294

- The net module has been split into 'net' (kernel) and prim_net (preloaded).

  Own Id: OTP-15765

- Socket counters now works as expected and can also be extracted with the (new)
  info function.

  Own Id: OTP-15818

- `re:run()` now avoids validating utf8 in the subject more than once in the
  same call. This validation could previously be performed multiple times when
  the `global` option was passed.

  Own Id: OTP-15831 Aux Id: ERL-876

- The un-documented function `erlang:dist_get_stat/1` now returns the real value
  of what the distribution queue contains instead of a boolean.

  Own Id: OTP-15905 Aux Id: PR-2270

- ETS `ordered_set` tables with `write_concurrency` enabled has got a
  performance issue fixed. There were no limits for the values of internal
  statistics counters before this fix. This could result in that the data
  structure sometimes reacted slowly to a change in how many parallel processes
  were using it.

  Own Id: OTP-15906

- Optimize the reception of large distribution messages.

  Own Id: OTP-15926 Aux Id: PR-2291

- Binary matching and functions like [`split_binary/2`](`split_binary/2`) will
  now create heap binaries when the results are small enough, reducing the
  chances of small sub-binaries keeping large binaries alive.

  Own Id: OTP-15977 Aux Id: ERIERL-366

- Fixed rare emulator crash in `instrument:allocations/0-1`.

  Own Id: OTP-15983

- Ports could pass very small binaries as reference counted off heap binaries to
  processes. This could cause an unnecessary large memory usage and an
  unnecessary load on the binary allocator. Small binaries are now always passed
  as heap binaries to processes.

  Own Id: OTP-16001 Aux Id: ERIERL-366

- `unicode:characters_to_binary()` could return very small binaries as reference
  counted off heap binaries. This could cause an unnecessary large memory usage
  and an unnecessary load on the binary allocator. Small binaries are now always
  returned as heap binaries.

  Own Id: OTP-16002 Aux Id: ERIERL-366

- Improved `erl_nif` documentation regarding `on_load` and Erlang stub/fallback
  functions.

  Own Id: OTP-16028 Aux Id: PR-2362

- New feature `ets:info(_, binary)` to get information about all reference
  counted binaries kept by a table. This is the same kind of debug information
  that [`process_info(_, binary)`](`process_info/2`) returns for a process.

  Own Id: OTP-16035 Aux Id: ERIERL-366

## Erts 10.4.4

### Fixed Bugs and Malfunctions

- An invalid value test caused the socket:setopt(Socket, ip, add_membership,
  ip_mreq()) to fail with badarg. The same for drop_membership.

  Own Id: OTP-15908 Aux Id: ERL-980

- Fixed bug causing VM crash when doing textual dump of a process containing an
  unhandled monitor down signal. Textual process dumps can be done with
  `erlang:system_info(procs)`, trace feature `process_dump`, Erlang shell break
  menu and a crashdump. Bug exist since OTP 21.0.

  Own Id: OTP-15909 Aux Id: ERL-979

- `lists:subtract/2` would produce incorrect results for some inputs on 64-bit
  platforms.

  Own Id: OTP-15938 Aux Id: ERL-986

- Fixed a bug in the loader that was similar to `OTP-15938`, yielding incorrect
  code for some inputs on 64-bit platforms.

  Own Id: OTP-15939

- Fixed bug causing scheduler threads in rare cases to block spinnning
  indefinitely. Bug exists since OTP 21.0.

  Own Id: OTP-15941 Aux Id: PR-2313

## Erts 10.4.3

### Fixed Bugs and Malfunctions

- Fixed a buffer overflow when
  [`binary_to_existing_atom/2`](`binary_to_existing_atom/2`) and
  `list_to_existing_atom/2` was used with the `latin1` encoding.

  Own Id: OTP-15819 Aux Id: ERL-944

- The runtime system disconnected a connection if it received an
  [`exit/2`](`exit/2`) signal where the recipient was a process on an old
  incarnation of the current node. That is, the receiving node had the same node
  name, but another "creation" number. The signal will now just be dropped since
  the receiving process no longer exists.

  Own Id: OTP-15867 Aux Id: ERIERL-373

## Erts 10.4.2

### Fixed Bugs and Malfunctions

- Fixed [`process_info(Pid,reductions)`](`process_info/2`) to not categorically
  increase reduction count of the measured process `Pid`. Repeated reduction
  measure of an idle process will most often (but not guaranteed) return the
  same value, like it behaved before OTP 21.3.8.

  Own Id: OTP-15865 Aux Id: ERL-964

- Fixed an incorrect load-time optimization that could cause a crash when
  extracting deeply nested tuple elements.

  Own Id: OTP-15871 Aux Id: ERIERL-374

- Fix bug causing VM crash when pressing P for "proc info" in Erlang shell break
  menu. Bug exists since OTP 22.0.

  Own Id: OTP-15873 Aux Id: ERL-965

## Erts 10.4.1

### Fixed Bugs and Malfunctions

- In nested use of `try`/`catch`, rethrowing an exception using `erlang:raise/3`
  with a different class would not always be able to change the class of the
  exception.

  Own Id: OTP-15834 Aux Id: ERIERL-367

## Erts 10.4

### Fixed Bugs and Malfunctions

- Do not allocate new bitstring/binary when an empty binary is appended.

  Own Id: OTP-15535 Aux Id: PR-2055

- Document that [`process_info(_, current_function)`](`process_info/2`) can
  return `{current_function, undefined}` in case of execution of native code.

  Own Id: OTP-15543 Aux Id: PR-2089

- Fixed bug in `ets:select`, `ets:match` and friends which could cause the table
  to remain fixated (as if `ets:safe_fixtable` had been called) after the call
  returned. This could happen for `protected` tables if another concurrent
  running process transferred table ownership to the process during its
  ets:select/match call. Ownership can be transferred using either
  `ets:give_away` or the `heir` table option.

  Own Id: OTP-15672

- Fixed a Windows-specific bug in `file:list_dir/1` that caused it to misbehave
  on network shares.

  Own Id: OTP-15693

- Fixed bug when calling `enif_whereis_*` from NIF resource destructor. Symptoms
  could be emulator crash or hanging scheduler threads.

  Own Id: OTP-15694 Aux Id: ERL-863

- Fixed a bug in the error case of [`apply/3`](`apply/3`), where the exception
  would erroneously have an empty argument list in some cases.

  Own Id: OTP-15698

- A bug has been fixed in the `maps` implementation that could cause a crash or
  memory usage to grow until the machine ran out of memory. This could happen
  when inserting a new key-value pair with a key `K1` containing a binary `B1`
  into a map `M` having a key `K2` with a binary `B2` if the following
  conditions were met:

  - `B1 =/= B2`
  - `size(B1) >= 4294967296`
  - `size(B2) >= 4294967296`
  - `size(M) >= 32`
  - `(size(B1) rem 4294967296) == (size(B2) rem 4294967296)`
  - the first `(size(B1) rem 4294967296)` bytes are the same both in `B1` and
    `B2`
  - substituting `B1` in `K1` with `B2` would create a term with the same value
    as `K2`

  The root cause of the problem is that the `maps` implementation only hashed
  the first `(X rem 4294967296)` bytes of binaries so that different binaries
  could get the same hash value independently of the hash seed.

  Own Id: OTP-15707

- `term_to_binary()` and distributed sends will now throw a `system_limit`
  exception instead of producing erroneous results when trying to encode a
  binary larger than 4 GB.

  Own Id: OTP-15708

- The vxworks configure has been updated to respect the environment CFLAGS.

  Own Id: OTP-15773

- Fix configure to not enable PGO (profile guided optimizations) when linking of
  the PGO binary fails. For instance this happens when there is no gcov lib
  installed.

  Own Id: OTP-15788

- Fix bug on OpenBSD where sockets using `active, true` or `active, N` could
  cause the system to live lock. The bug was introduced in erts-10.2 (OTP-21.2).

  Own Id: OTP-15791

### Improvements and New Features

- Add support for Erlang Distribution protocol to split the payload of large
  signals into several fragments. This allows other processes to communicate
  uninterrupted during the transmission of these signals.

  Own Id: OTP-13397

- A simple socket API is provided through the socket module. This is a low level
  API that does _not_ replace gen\_\[tcp|udp|sctp]. It is intended to
  _eventually_ replace the inet driver, but not the high level gen-modules
  (gen_tcp, gen_udp and gen_sctp). It also provides a basic API that facilitates
  the implementation of other protocols, that is TCP, UDP and SCTP.

  Known issues are; No support for the Windows OS (currently).

  Own Id: OTP-14831

- Added NIF functions
  [`enif_set_pid_undefined`](erl_nif.md#enif_set_pid_undefined),
  [`enif_is_pid_undefined`](erl_nif.md#enif_is_pid_undefined) and
  [`enif_compare_pids`](erl_nif.md#enif_compare_pids).

  Own Id: OTP-15011 Aux Id: PR-2147

- Underutilized memory segments (carriers) can now move between all allocator
  instances, rather than just between instances of the same type, which greatly
  reduces memory usage in some scenarios.

  Own Id: OTP-15063

- The emulator will now mark free blocks in pooled carriers with
  `madvise(2) + MADV_FREE` (or similar), letting the OS reclaim the associated
  physical memory if necessary.

  Own Id: OTP-15075

- New `ERL_NIF_SELECT_CANCEL` feature added to `enif_select` in order to cancel
  (or "deselect") a read or write event on a previously selected file
  descriptor.

  Own Id: OTP-15095

- ETS option `write_concurrency` now also affects and improves the scalability
  of `ordered_set` tables. The implementation is based on a data structure
  called contention adapting search tree, where the lock granularity adapts to
  the actual amount of concurrency exploited by the applications in runtime.

  Own Id: OTP-15128

- Build configuration of the `crypto` application has been moved from the `erts`
  application into the `crypto` application.

  Own Id: OTP-15129

- Anonymous functions that don't capture environment are now created at
  load-time instead of in run-time.

  Own Id: OTP-15195 Aux Id: PR-1812

- Optimize updates of maps with identical keys and values. E.g. in the example
  below the original Map will be reused as the return of the update.

  1> Map = #\{ a => b \}. #\{ a => b \} 2> Map#\{ a := b \}.

  Own Id: OTP-15211 Aux Id: PR-1889

- Optimize `binary:match/2` and `binary:matches/2` to use memchr internally.

  Own Id: OTP-15238 Aux Id: PR-1803

- The runtime system used to terminate when a message larger than 2 Gb was
  passed over the distribution. The send operation will now instead throw a
  `system_limit` exception.

  Own Id: OTP-15261

- Change the first module called by erts to be named erl_init instead of
  otp_ring0. systools in sasl have been updated to reflect this change.

  Own Id: OTP-15336 Aux Id: PR-1825

- Minor adjustments made to build system for parallel configure.

  Own Id: OTP-15340 Aux Id: OTP-14625

- Two new NIF interface functions `enif_select_read` and `enif_select_write`.
  They are similar to existing `enif_select` but allow a custom event message as
  an argument.

  Own Id: OTP-15349 Aux Id: PR-2084

- The embedded copy of `zlib` has been updated from `1.2.8` to `1.2.11`.

  Note that this copy is only used as a fallback when the target platform
  doesn't provide any `zlib` development libraries. If your system provides
  `zlib` then it will be used even if it is older than `1.2.11`.

  Own Id: OTP-15351 Aux Id: ERL-749

- New NIF function `enif_make_monitor_term`.

  Own Id: OTP-15362 Aux Id: PR-2127

- Appending lists (The ++ operator) will now yield properly on large inputs.

  Own Id: OTP-15427

- The [`length/1`](`length/1`) BIF used to calculate the length of the list in
  one go without yielding, even if the list was very long. In OTP 22,
  [`length/1`](`length/1`) will yield when called with long lists.

  Own Id: OTP-15439

- Processes sending messages are now punished with a reduction cost based on
  message size. That is, a process sending a large message will yield earlier
  than before.

  Own Id: OTP-15513 Aux Id: ERL-773

- The transitory emulator option `+ztma true` (introduced in OTP 21.3) has been
  removed.

  Own Id: OTP-15581 Aux Id: OTP-15580

- In OTP 22, HiPE (the native code compiler) is not fully functional. The
  reasons for this are:

  There are new BEAM instructions for binary matching that the HiPE native code
  compiler does not support.

  The new optimizations in the Erlang compiler create new combination of
  instructions that HiPE currently does not handle correctly.

  If erlc is invoked with the `+native` option, and if any of the new binary
  matching instructions are used, the compiler will issue a warning and produce
  a BEAM file without native code.

  Own Id: OTP-15596

- The termination behaviour of processes has changed to allow processes to yield
  while sending link exit/monitor down signals.

  The erl crash dump has been expanded to now also include processes that are
  termeinating but have not yet terminated.

  Own Id: OTP-15610

- The dist messages EXIT, EXIT2 and MONITOR_DOWN have been updated with new
  versions that send the reason term as part of the payload of the message
  instead of as part of the control message.

  The old versions are still present and can be used when communicating with
  nodes that don't support the new versions.

  Own Id: OTP-15611

- When sending messages, exit, exit2 and monitor down distributed signals, the
  process sending will now yield appropriately.

  This means that a terminating process will yield and possibly be suspended on
  busy distribution entries while they are terminating. This means that any
  memory held by such processes will not be released until after all
  exit/monitor down signals have been sent.

  Own Id: OTP-15612

- All external pids/ports/refs created by erlang:list_to_pid/port/ref debug
  functions now compare equal to any other pid/port/ref with the same number
  from that node.

  Before this change they compared differently because the node creation of the
  pid/port/ref did not compare equal to any real pid/port/ref creation.

  This will mostly effect pids/ports/refs typed in the shell.

  Own Id: OTP-15613

- The `persistent_term` functions [`put/2`](`put/2`) and [`erase/1`](`erase/1`)
  are now yielding.

  Own Id: OTP-15615

- A new
  [`erlang:dist_ctrl_set_opt(DHandle, get_size, Value)`](`erlang:dist_ctrl_set_opt/3`)
  option has been added. This option makes it possible to configure the
  distribution channel identified by `DHandle` so that
  [`erlang:dist_ctrl_get_data(DHandle)`](`erlang:dist_ctrl_get_data/1`) also
  returns the size of the data to pass over the channel.

  Own Id: OTP-15617

- Previously, all ETS tables used centralized counter variables to keep track of
  the number of items stored and the amount of memory consumed. These counters
  can cause scalability problems (especially on big NUMA systems). This change
  adds an implementation of a decentralized counter and modifies the
  implementation of ETS so that ETS tables of type `ordered_set` with
  `write_concurrency` enabled use the decentralized counter. Experiments
  indicate that this change substantially improves the scalability of ETS
  `ordered_set` tables with `write_concurrency` enabled in scenarios with
  frequent `ets:insert/2` and `ets:delete/2` calls.

  Own Id: OTP-15623 Aux Id: PR-2190

- The [`iolist_size/1`](`iolist_size/1`) function is now yielding which means
  that an Erlang/OTP system will be responsive even if the applications running
  on the system frequently call [`iolist_size/1`](`iolist_size/1`) with large
  iolists.

  Own Id: OTP-15631

- A simple test suite for the net module has been added.

  Own Id: OTP-15635

- Added the NIF function `enif_term_type`, which helps avoid long sequences of
  `enif_is_xyz` by returning the type of the given term. This is especially
  helpful for NIFs that serialize terms, such as JSON encoders, where it can
  improve both performance and readability.

  Own Id: OTP-15640

- The last call optimization is now applied to BIFs. When calling a BIF in the
  tail position of a function, the return address and stack frame will now be
  discarded before calling the BIF. As a consequence of this change, the
  immediate caller of a tail-called BIF will no longer be available in stack
  backtraces.

  Own Id: OTP-15674 Aux Id: PR-2177

- Fix GC bug where distributed messages in a processes mailbox would cause extra
  GCs. This can be very expensive if there many messages in the mailbox.

  Own Id: OTP-15703

- Internal documentation has now been added to the _Erts_ and _Compiler_
  applications.

  The internal documents for _Erts_ describe miscellaneous interesting
  implementation details. Those details can change at any time.

  The internal documentation for _Compiler_ documents the API for the Core
  Erlang modules. While we will not change those APIs without good reason, we
  don't give the same guarantees about backward compatibility as for the rest of
  the APIs in OTP.

  Own Id: OTP-15715

- The performance of non-bignum integer arithmetic has been improved.

  Own Id: OTP-15740

- The `-remsh` option to `erl` now automatically adds the local systems hostname
  to the target node if no nodename is given. e.g.

  `erl -name foo -remsh bar`  
  `erl -sname foo -remsh bar`

  Own Id: OTP-15794 Aux Id: PR-2219

## Erts 10.3.5.19

### Fixed Bugs and Malfunctions

- Fix bug in match spec compilator seen to cause a stack overflow crash on debug
  VM for certain match specs. Could potentially cause problems for standard VM,
  but has not been verified. Match specs are used by `ets:match/select`
  functions and `erlang:trace_pattern`.

  Own Id: OTP-17379 Aux Id: PR-4804

- A call to `port_command()` could cause a scheduler to end up in an eternal
  loop if the port was busy and the calling process had incoming signals at the
  time of the call. This bug was introduced in OTP 23.3.2 (ERTS version 11.2.1),
  OTP 22.3.4.18 (ERTS version 10.7.2.10), and OTP 21.3.8.23 (ERTS version
  10.3.5.18).

  Own Id: OTP-17448 Aux Id: GH-4898, PR-4903, OTP-17291

## Erts 10.3.5.18

### Fixed Bugs and Malfunctions

- The following signals could pass before other signals from the same sender to
  the same receiver. That is, these signals could arrive too early.

  - `garbage-collect` request. Sent from one process to another using one of the
    `garbage_collect/0` BIFs.
  - `check-process-code` request. Sent from one process to another using one of
    the `check_process_code()` BIFs.
  - `is-process-alive` reply. Sent as a response to a process calling the
    `is_process_alive()` BIF.
  - `process-info` reply. Sent as a response to a process calling one of the
    `process_info()` BIFs.
  - `port-command` reply. Sent as a response to a process calling one of the
    `port_command()` BIFs.
  - `port-connect` reply. Sent as a response to a process calling the
    `port_connect()` BIF.
  - `port-close` reply. Sent as a response to a process calling the
    `port_close()` BIF.
  - `port-control` reply. Sent as a response to a process calling the
    `port_control()` BIF.
  - `port-call` reply. Sent as a response to a process calling the `port_call()`
    BIF.
  - `port-info` reply. Sent as a response to a process calling one of the
    `port_info()` BIFs.

  Own Id: OTP-17291

- A garbage collection of a literal area missed messages that entirely consisted
  of a term in a literal area. This could in turn lead to a crash of the runtime
  system.

  Own Id: OTP-17307

- A call to [`process_flag(message_queue_data, off_heap)`](`process_flag/2`)
  could cause a crash of the runtime system when sequential tracing was enabled.

  Own Id: OTP-17349

## Erts 10.3.5.17

### Fixed Bugs and Malfunctions

- Fixed a bug in the timer implementation which could cause timers that were set
  to more than 37.25 hours in the future to be delayed. This could occur if
  there were multiple timers scheduled to be triggered very close in time, but
  still at different times, and the scheduler thread handling the timers was not
  able to handle them quickly enough. Delayed timers were in this case triggered
  when another unrelated timer was triggered.

  Own Id: OTP-17253

### Improvements and New Features

- Fix a file descriptor leak when using sendfile and the remote side closes the
  connection. This bug has been present since OTP-21.0.

  Own Id: OTP-17244

## Erts 10.3.5.16

### Fixed Bugs and Malfunctions

- Fixed a bug that could cause some work scheduled for execution on scheduler
  threads to be delayed until other similar work appeared. Beside delaying
  various cleanup of internal data structures also the following could be
  delayed:

  - Termination of a distribution controller process
  - Disabling of the distribution on a node
  - Gathering of memory allocator information using the `instrument` module
  - Enabling, disabling, and gathering of `msacc` information
  - Delivery of `'CHANGE'` messages when time offset is monitored
  - A call to `erlang:cancel_timer()`
  - A call to `erlang:read_timer()`
  - A call to `erlang:statistics(io | garbage_collection | scheduler_wall_time)`
  - A call to `ets:all()`
  - A call to `erlang:memory()`
  - A call to `erlang:system_info({allocator | allocator_sizes, _})`
  - A call to `erlang:trace_delivered()`

  The bug existed on runtime systems running on all types of hardware except for
  x86/x86_64.

  Own Id: OTP-17185

## Erts 10.3.5.15

### Fixed Bugs and Malfunctions

- Fixed rare distribution bug in race between received signal
  (link/monitor/spawn_request/spawn_reply) and disconnection. Symptom: VM crash.
  Since: OTP 21.0.

  Own Id: OTP-16869 Aux Id: ERL-1337

- The `suspend_process()` and `resume_process()` BIFs did not check their
  arguments properly which could cause an emulator crash.

  Own Id: OTP-17080

- The runtime system would get into an infinite loop if the runtime system was
  started with more than 1023 file descriptors already open.

  Own Id: OTP-17088 Aux Id: ERIERL-580

## Erts 10.3.5.14

### Fixed Bugs and Malfunctions

- The ERTS internal I/O poll implementation could get into an inconsistent state
  causing input events to be ignored.

  Own Id: OTP-16780 Aux Id: PR-2701

- The documentation of [`statistics(run_queue)`](`statistics/1`) erroneously
  stated that it returns the total length of all normal run queues when it is
  the total length of all normal and dirty CPU run queues that is returned. The
  documentation has been updated to reflect the actual behavior.

  Own Id: OTP-16866 Aux Id: ERL-1355

- Two bugs in the ERTS internal thread wakeup functionality have been fixed.
  These bugs mainly hit when all threads in the system tried to go to sleep.
  When the bugs were triggered, certain operations were delayed until a thread
  woke up due to some other reason. Most important operations effected were code
  loading, persistent term updates, and memory deallocation.

  Own Id: OTP-16870

- Fixed bug in `ets:select_replace/2` on `compressed` tables that could produce
  faulty results or VM crash. Bug exists since OTP 20.

  Own Id: OTP-16874 Aux Id: ERL-1356, PR-2763

## Erts 10.3.5.13

### Fixed Bugs and Malfunctions

- An unintentional reuse of an already used emulator internal event object could
  cause a wakeup signal to a thread to be lost. In worst case this could cause
  the runtime system to hang. This hang was however quite rare.

  Own Id: OTP-16766 Aux Id: ERL-1304

- NIF threads and driver threads on non-Linux systems leaked internal resources
  when terminating. On Windows these resources were one event per thread. On
  most other systems one mutex and one condition variable per thread. On these
  other systems that also lacked `pthread_cond_timedwait()` also a pipe with its
  file descriptors was leaked.

  Own Id: OTP-16772 Aux Id: ERL-1304

## Erts 10.3.5.12

### Fixed Bugs and Malfunctions

- The functionality utilized by BIFs for temporary disabling of garbage
  collection while yielding could cause system task queues to become
  inconsistent on a process executing such a BIF. Process system tasks are for
  example utilized when purging code, garbage collecting literal data, and when
  issuing an ordinary garbage collection from another process.

  The bug does not trigger frequently. Multiple code purges in direct sequence
  makes it more likely that this bug is triggered. In the cases observed, this
  has resulted in a hanging code purge operation.

  Own Id: OTP-16639 Aux Id: ERL-1236

- A literal area could prematurely be released before all uses of it had been
  removed. This occurred either when a terminating process had a complex exit
  reason referring to a literal that concurrently was removed, or when a
  terminating process continued executing a dirty NIF accessing a literal (via
  the heap) that concurrently was removed.

  Own Id: OTP-16640 Aux Id: OTP-16193

- The VM could potentially crash when checking process code of a process that
  terminated while executing a dirty NIF. The checking of process code is part
  of a code purge operation.

  Own Id: OTP-16641

- System tasks of `low` priority were not interleaved with `normal` priority
  system tasks as they should. This could potentially delay garbage collection
  of another process longer than intended if the garbage collection was
  requested from a `low` priority process.

  Own Id: OTP-16642

## Erts 10.3.5.11

### Fixed Bugs and Malfunctions

- [re:run(Subject, RE, \[unicode])](`re:run/3`) returned `nomatch` instead of
  failing with a `badarg` error exception when `Subject` contained illegal utf8
  and `RE` was passed as a binary. This has been corrected along with
  corrections of reduction counting in `re:run()` error cases.

  Own Id: OTP-16553

- Fixed a bug that could cause the emulator to crash when purging modules or
  persistent terms.

  Own Id: OTP-16555 Aux Id: ERL-1188

## Erts 10.3.5.10

### Fixed Bugs and Malfunctions

- Fixed bug in `ets:update_counter/4`, when called with an invalid `UpdateOp`
  and a `Key` that does not exist, causing `ets:info(T,size)` to return
  incorrect values. Bug exists since OTP-19.0.2.

  Own Id: OTP-16404 Aux Id: ERL-1127

- A process could get into an inconsistent state where it was runnable, but
  never scheduled for execution. This could occur when a mix of `normal` and
  `low` priority processes where scheduled on the same type of dirty scheduler
  simultaneously.

  Own Id: OTP-16446 Aux Id: ERL-1157

- Corrected the valid range of the `erl` command line argument
  [`+SDio <NumberOfDirtyIoSchedulers>`](erl_cmd.md#%2BSDio) from `0..1024` to
  `1..1024`. `+SDio 0` was erroneously allowed which just caused the VM to crash
  on the first dirty I/O job scheduled.

  Own Id: OTP-16481

## Erts 10.3.5.9

### Fixed Bugs and Malfunctions

- A process could end up in a state where it got endlessly rescheduled without
  making any progress. This occurred when a system task, such as check of
  process code (part of a code purge), was scheduled on a high priority process
  trying to execute on a dirty scheduler.

  Own Id: OTP-16436 Aux Id: ERL-1152

- Fixed bug in `erlang:list_to_ref/1` when called with a reference created by a
  remote note. Function [`list_to_ref/1`](`list_to_ref/1`) is intended for
  debugging and not to be used in application programs. Bug exist since OTP
  20.0.

  Own Id: OTP-16438

## Erts 10.3.5.8

### Fixed Bugs and Malfunctions

- Taking a scheduler offline could cause timers set while executing on that
  scheduler to be delayed until the scheduler was put online again. This bug was
  introduced in ERTS version 10.0 (OTP 21.0).

  Own Id: OTP-16371

- A process calling
  [`erlang:system_flag(multi_scheduling, block)`](`m:erlang#system_flag_multi_scheduling`)
  could end up blocked waiting for the operation to complete indefinitely.

  Own Id: OTP-16379

## Erts 10.3.5.7

### Fixed Bugs and Malfunctions

- A literal area could prematurely be released before all uses of it had been
  removed. This occurred either when a terminating process had a complex exit
  reason referring to a literal that concurrently was removed, or when a
  terminating process continued executing a dirty NIF accessing a literal (via
  the heap) that concurrently was removed.

  Own Id: OTP-16193

- Fix bug causing VM crash due to memory corruption of distribution entry.
  Probability of crash increases if Erlang distribution is frequently
  disconnected and reestablished towards same node names. Bug exists since
  OTP-21.0.

  Own Id: OTP-16224 Aux Id: ERL-1044

- Fix bug where the receive marker used by the runtime to do the receive queue
  optimization could be incorrectly set. The symptom of this would be that a
  message that should match in a receive never matches.

  The bug requires the OTP-22 compiler and multiple selective receives to
  trigger. See OTP-16219 for details about the bug fix in the compiler.

  Own Id: OTP-16241 Aux Id: ERL-1076 OTP-16219

- Fixed bug causing crash of VM built with configuration
  `--enable--sharing-preserving`. Provoked when a sent message contains both a
  bit string and the heap binary (< 65 bytes) which the bit string was matched
  from. Bug exists since OTP-19.0 but has seen to be easier to provoke since
  OTP-22.1.

  Own Id: OTP-16265 Aux Id: ERL-1064

## Erts 10.3.5.6

### Fixed Bugs and Malfunctions

- Fix a bug in `binary_to_term` that would crash the emulator if a term larger
  than 16GB was to be decoded.

  Own Id: OTP-16058 Aux Id: PR-2382

- When communicating with a simultaneously exiting port via the
  `erlang:port_*()` BIFs one could sometimes get stray `{Ref, What}` messages.
  Where `Ref` was a reference and `What` usually were the atom `badarg`.

  Own Id: OTP-16107 Aux Id: ERL-1049

## Erts 10.3.5.5

### Fixed Bugs and Malfunctions

- [`process_info(P,binary)`](`process_info/2`) would neglect to look through
  heap fragments, potentially missing a few binaries associated with the
  process.

  Own Id: OTP-15978 Aux Id: ERIERL-366

- Fixed bug triggered if a process is killed during call to
  `persistent_term:put` or `persistent_term:erase`.

  Own Id: OTP-16041

### Improvements and New Features

- Fixed rare emulator crash in `instrument:allocations/0-1`.

  Own Id: OTP-15983

## Erts 10.3.5.4

### Fixed Bugs and Malfunctions

- Fixed bug causing VM crash when doing textual dump of a process containing an
  unhandled monitor down signal. Textual process dumps can be done with
  `erlang:system_info(procs)`, trace feature `process_dump`, Erlang shell break
  menu and a crashdump. Bug exist since OTP 21.0.

  Own Id: OTP-15909 Aux Id: ERL-979

- `lists:subtract/2` would produce incorrect results for some inputs on 64-bit
  platforms.

  Own Id: OTP-15938 Aux Id: ERL-986

- Fixed a bug in the loader that was similar to `OTP-15938`, yielding incorrect
  code for some inputs on 64-bit platforms.

  Own Id: OTP-15939

- Fixed bug causing scheduler threads in rare cases to block spinnning
  indefinitely. Bug exists since OTP 21.0.

  Own Id: OTP-15941 Aux Id: PR-2313

## Erts 10.3.5.3

### Fixed Bugs and Malfunctions

- If you set `{linger,{true,0}}` on a `gen_tcp` listen socket, accept a
  connection on that socket, and then close the accepted socket, now the linger
  zero setting is transferred to the accepted socket. Before this correction
  that information was lost and the close behaviour on the accepted socket
  incorrect.

  Own Id: OTP-15370 Aux Id: ERIERL-353

- Fixed [`process_info(Pid,reductions)`](`process_info/2`) to not categorically
  increase reduction count of the measured process `Pid`. Repeated reduction
  measure of an idle process will most often (but not guaranteed) return the
  same value, like it behaved before OTP 21.3.8.

  Own Id: OTP-15865 Aux Id: ERL-964

- The runtime system disconnected a connection if it received an
  [`exit/2`](`exit/2`) signal where the recipient was a process on an old
  incarnation of the current node. That is, the receiving node had the same node
  name, but another "creation" number. The signal will now just be dropped since
  the receiving process no longer exists.

  Own Id: OTP-15867 Aux Id: ERIERL-373

### Improvements and New Features

- The possibility to send ancillary data, in particular the TOS field, has been
  added to `gen_udp:send/4,5`.

  Own Id: OTP-15747 Aux Id: ERIERL-294

## Erts 10.3.5.2

### Fixed Bugs and Malfunctions

- In nested use of `try`/`catch`, rethrowing an exception using `erlang:raise/3`
  with a different class would not always be able to change the class of the
  exception.

  Own Id: OTP-15834 Aux Id: ERIERL-367

- Fixed bug in `seq_trace:set_token(label,Term)` which could cause VM crash if
  `Term` was heap allocated (not an atom, small integer, local pid or port). Bug
  exists since OTP 21.0 when terms other than small integers were first allowed
  as labels.

  Own Id: OTP-15849 Aux Id: ERL-700

- Fix `seq_trace:print/2` not to raise `badarg` exception if label is not a
  small integer. Bug exists since OTP 21.0.

  Own Id: OTP-15859 Aux Id: ERL-700

## Erts 10.3.5.1

### Fixed Bugs and Malfunctions

- Fixed a buffer overflow when
  [`binary_to_existing_atom/2`](`binary_to_existing_atom/2`) and
  `list_to_existing_atom/2` was used with the `latin1` encoding.

  Own Id: OTP-15819 Aux Id: ERL-944

## Erts 10.3.5

### Fixed Bugs and Malfunctions

- Fixed more bugs in [`process_info(reductions)`](`process_info/1`) causing it
  to sometimes behave non-monotonic. That is, a subsequent call toward the same
  process could return a lower reduction value.

  Own Id: OTP-15793 Aux Id: ERIERL-337, OTP-15709

## Erts 10.3.4

### Fixed Bugs and Malfunctions

- Add missing documentation of new external tags `NEW_PID`, `NEW_PORT` and
  `NEWER_REFERENCE` introduced in OTP 19.

  These new tags are planned to be "activated" in OTP 23 when distribution
  capability flag `DFLAG_BIG_CREATION` becomes mandatory. Older nodes (>= 19)
  are able to decode these new tags and send them back to the new node. Nodes
  older than OTP 23 will however never encode their own local pids, ports and
  references using the new tags.

  Own Id: OTP-15766

## Erts 10.3.3

### Fixed Bugs and Malfunctions

- Fix `erlang:open_port/2` with the `fd` option to correctly cleanup the pollset
  when the port is closed. Before this fix there would be error printouts sent
  to logger when the same fd was reused in a new port.

  Own Id: OTP-15753 Aux Id: ERL-900

## Erts 10.3.2

### Fixed Bugs and Malfunctions

- Fixed a bug in `seq_trace:reset_trace/0` that could crash the emulator.

  Own Id: OTP-15704

- Fixed bug in [`process_info(reductions)`](`process_info/1`) causing it to
  sometimes return invalid results.

  Own Id: OTP-15709 Aux Id: ERIERL-337

## Erts 10.3.1

### Fixed Bugs and Malfunctions

- If a suspend/resume signal pair was sent to a process while it was executing
  dirty, the receiving process could later end up in a suspended state
  indefinitely. This bug was introduced in ERTS version 10.0 (OTP 21.0).

  Suspend/resume signals are sent from
  [`erlang:suspend_process()`](`erlang:suspend_process/1`)/[`erlang:resume_process()`](`erlang:resume_process/1`).
  The `m:dbg` trace tool utilize this functionality and could thus trigger this
  bug.

  Own Id: OTP-15688

- Fix a possible deadlock when terminating the ERTS caused by a dirty scheduler
  not releasing it's run-queue lock when terminating.

  Own Id: OTP-15690 Aux Id: PR-2172

## Erts 10.3

### Fixed Bugs and Malfunctions

- When multiplying a number by itself, a word beyond the number on the heap
  could be read (and ignored). This bug was extremely unlikely to actually cause
  a real problem.

  Own Id: OTP-15484

- Fix bug where doing `seq_trace:reset_trace()` while another process was doing
  a garbage collection could cause the run-time system to segfault.

  Own Id: OTP-15490

- Fix reading of ancillary data from packet oriented sockets on old Linux kernel
  versions. Without this fix, getting the data would cause the port to enter an
  infinite loop.

  Own Id: OTP-15494

- Fix bug where crash dumping or doing `erlang:system_info(procs)` while another
  process was doing a garbage collection could cause the run-time system to
  segfault.

  Own Id: OTP-15527

- Fix `erlang:system_info(kernel_poll)` to return correct value. Before this
  fix, the call always returned `false`.

  Own Id: OTP-15556

- Fix bug in `enif_make_map_from_arrays` that would produce broken maps when
  number of keys were 32. Bug exists since OTP 21.0.

  Own Id: OTP-15567

- Fix a bug in `binary:encode_unsigned` that may cause a read of uninitialized
  memory.

  The bug existed since the function was added (OTP R16B02).

  Own Id: OTP-15583 Aux Id: PR-2118

- Fixed a bug that could cause `heart` to kill an exiting node before it had
  time to flush all buffered writes. If environment variable
  `HEART_KILL_SIGNAL=SIGABRT` was set a superfluous core dump could also be
  generated.

  Own Id: OTP-15599 Aux Id: ERIERL-298

- Fix `enif_consume_timeslice` to be a no-op on dirty scheduler and not crash
  debug compiled emulator.

  Own Id: OTP-15604

- Fixed macro redefinition warnings.

  Own Id: OTP-15629

- `to_erl` fixed to not garble terminal input beyond 7-bit ASCII.

  Own Id: OTP-15650 Aux Id: ERL-854, PR-2161

- Minor fixes for `make clean`.

  Own Id: OTP-15657

- Fixed a bug in all `ets:select*` and `ets:match*` functions that could in some
  rare cases lead to very poor performance.

  Own Id: OTP-15660 Aux Id: ERL-869

### Improvements and New Features

- Add `erlang:system_flag(system_logger, Pid)` and
  `erlang:system_info(system_logger)`. This system_flag can be used to set the
  process that will receive the logging messages generated by ERTS.

  Own Id: OTP-15375

- [`integer_to_list/2`](`integer_to_list/2`) and
  [`integer_to_binary/2`](`integer_to_binary/2`) are now implemented in C,
  improving their performance.

  Own Id: OTP-15503 Aux Id: PR-2052

- Improved `term_to_binary` to do more fair reduction count and yielding when
  encoding large byte lists (strings).

  Own Id: OTP-15514 Aux Id: ERL-774

- Made internal port drivers more robust against `erlang:port_control` with
  invalid arguments and added documentation warnings about such abuse.

  Own Id: OTP-15555 Aux Id: ERIERL-231

- Fix bug on NetBSD where the `exit_status` from a port program would never be
  sent.

  Own Id: OTP-15558 Aux Id: ERL-725

- There is a new function `persistent:term(Key, Default)` to allow specifying a
  default when looking up a persistent term.

  Own Id: OTP-15576 Aux Id: ERL-843

- A transitory emulator option '`+ztma true`' has been added to allow running
  existing BEAM code that relies on "tuple calls" (dispatch on parameterized
  modules) which has been compiled under OTP 20 or earlier. This option will be
  removed in OTP 22, so such modules should eventually be recompiled with the
  `+tuple_calls` option.

  Own Id: OTP-15580 Aux Id: PR-2113

## Erts 10.2.5

### Fixed Bugs and Malfunctions

- Fixes of install/release phase in build system.

  - The source tree was modified when installing/releasing and/or applying a
    patch.
  - Some files were installed with wrong access rights.
  - If applying a patch (using `otp_patch_apply`) as another user (except root)
    than the user that built the source, the documentation was not properly
    updated.

  Own Id: OTP-15551

- Setting the `recbuf` size of an inet socket the `buffer` is also automatically
  increased. Fix a bug where the auto adjustment of inet buffer size would be
  triggered even if an explicit inet buffer size had already been set.

  Own Id: OTP-15651 Aux Id: ERIERL-304

- Reading from UDP using active `true` or active `N` mode has been optimized
  when more packets than specified by `read_packets` are available on the
  socket.

  Own Id: OTP-15652 Aux Id: ERIERL-304

## Erts 10.2.4

### Fixed Bugs and Malfunctions

- When using the `{linger,{true,T}}` option; `gen_tcp:listen/2` used the full
  linger time before returning for example `eaddrinuse`. This bug has now been
  corrected.

  Own Id: OTP-14728 Aux Id: ERIERL-303

## Erts 10.2.3

### Fixed Bugs and Malfunctions

- Fix bug where doing a `gen_tcp:send` on a socket with `delay_send` set to true
  could cause a segfault if the other side closes the connection.

  Bug was introduced in erts-10.2 (OTP-21.2).

  Own Id: OTP-15536 Aux Id: ERL-827

- Fix a race condition when a port program closes that could result in the next
  started port to hang during startup.

  When this fault happens the following error is normally (but not always)
  logged:

  `=ERROR REPORT==== 14-Jan-2019::10:45:52.868246 ===`  
  `Bad input fd in erts_poll()! fd=11, port=#Port<0.505>, driver=spawn, name=/bin/sh -s unix:cmd`

  Bug was introduced in erts-10.0 (OTP-21.0).

  Own Id: OTP-15537

- Fix a bug where polling for external events could be delayed for a very long
  time if all active schedulers were 100% loaded.

  Bug was introduced in erts-10.2 (OTP-21.2).

  Own Id: OTP-15538 Aux Id: ERIERL-229

## Erts 10.2.2

### Fixed Bugs and Malfunctions

- Fixed a crash when dangling files were closed after `init:restart/0`.

  Own Id: OTP-15495 Aux Id: ERL-821

- A bug that could cause dirty schedulers to become unresponsive has been fixed.

  Own Id: OTP-15509 Aux Id: PR-2027, PR-2093

## Erts 10.2.1

### Fixed Bugs and Malfunctions

- Fixed bug on big endian architectures when changing file permissions or
  ownership with `file:change_mode`, `change_owner`, `change_group` or
  `write_file_info`. Bug exists since OTP-21.0.

  Own Id: OTP-15485

- Fixed bug in `atomics` with option `{signed,false}` when returned values are
  `(1 bsl 63)` or larger. Could cause heap corruption leading to VM crash or
  other unpleasant symptoms. Bug exists since OTP-21.2 when module `atomics` was
  introduced.

  Own Id: OTP-15486 Aux Id: PR-2061

- Fixed bug in operator `band` of two negative operands causing erroneous result
  if the absolute value of one of the operands have the lowest `N*W` bits as
  zero and the other absolute value is not larger than `N*W` bits. `N` is an
  integer of 1 or larger and `W` is 32 or 64 depending on word size.

  Own Id: OTP-15487 Aux Id: ERL-804

## Erts 10.2

### Fixed Bugs and Malfunctions

- When a process was waiting for a TCP socket send operation to complete, and
  another process closed the socket during that send, the sending process could
  hang. This bug has now been corrected.

  Own Id: OTP-12242 Aux Id: ERL-561

- Document `bit_size` in match specifications and allow it in `ets:fun2ms`.

  Own Id: OTP-15343 Aux Id: PR-1962

- Fixed bug in `ets:select_replace` when called with a fully bound key could
  cause a following call to `ets:next` or `ets:prev` to crash the emulator or
  return invalid result.

  Own Id: OTP-15346

- When a module has been purged from memory, any literals belonging to that
  module will be copied to all processes that hold references to them. The max
  heap size limit would be ignored in the garbage collection initiated when
  copying literals to a process. If the max heap size was exceeded, the process
  would typically be terminated in the following garbage collection. Corrected
  to terminate the process directly if copying a literal would exceed the max
  heap size.

  Own Id: OTP-15360

- Fix compilation of run_erl on Solaris 11.4 and later.

  Own Id: OTP-15389

- Fixed a bug where `lists:reverse/1-2` could use far too many reductions. This
  bug was introduced in `OTP 21.1`.

  Own Id: OTP-15436

- Fixed a bug where a dirty scheduler could stay awake forever if a distribution
  entry was removed as part of a dirty GC.

  Own Id: OTP-15446 Aux Id: PR-2024

- Fix microstate accounting handing in various places. Most importantly the GC
  states when the GC is run on a dirty scheduler are now managed correctly.

  Own Id: OTP-15450 Aux Id: ERIERL-229

- Fixed bug in `file:sendfile` when the send operation failed. For sockets in
  `active` modes it could cause emulator crash or a hanging call. For sockets
  with `{active,false}` an unexpected `{inet_reply, _, _}` message could be sent
  to the calling process. The bug exists since OTP-21.0.

  Own Id: OTP-15461 Aux Id: ERL-784

- The erts configure script has been updated to reject any CFLAGS that does not
  have `-O`. This in order to prevent the common mistake of forgetting to add
  `-O2` to custom CFLAGS.

  Own Id: OTP-15465

- Fix reduction count in lists:member/2

  Own Id: OTP-15474 Aux Id: ERIERL-229

### Improvements and New Features

- New `counters` and `atomics` modules supplies access to highly efficient
  operations on mutable fixed word sized variables.

  Own Id: OTP-13468

- There is a new module `persistent_term` that implements a term storage
  suitable for terms that are frequently used but never or infrequently updated.
  Lookups are done in constant time without copying the terms.

  Own Id: OTP-14669 Aux Id: PR-1989

- A function `inet:getifaddrs/1` that takes a list with a namespace option has
  been added, for platforms that support that feature, for example Linux
  (only?).

  Own Id: OTP-15121 Aux Id: ERIERL-189, PR-1974

- Added the `nopush` option for TCP sockets, which corresponds to `TCP_NOPUSH`
  on \*BSD and `TCP_CORK` on Linux.

  This is also used internally in `file:sendfile` to reduce latency on
  subsequent send operations.

  Own Id: OTP-15357 Aux Id: ERL-698

- List subtraction (The `--` operator) will now yield properly on large inputs.

  Own Id: OTP-15371

- Optimize handling of send_delay for tcp sockes to better work with the new
  pollthread implementation introduced in OTP-21.

  Own Id: OTP-15471 Aux Id: ERIERL-229

- Optimize driver_set_timer with a zero timeout to short-circuit and not create
  any timer structure, but instead schedule the timer immediately.

  Own Id: OTP-15472 Aux Id: ERIERL-229

- Add `erl_xcomp_code_model_small` as a cross configure variable in order to let
  the emulator be build with the assumption that a small code model will be used
  on the target machine.

  Own Id: OTP-15473 Aux Id: ERIERL-229

- Add a new pollset that is made to handle sockets that use `{active, true}` or
  `{active, N}`. The new pollset will not be polled by a pollthread, but instead
  polled by a normal scheduler.

  This change was made because of the overhead associated with constantly having
  to re-apply the ONESHOT mechanism on fds that all input events were
  interesting.

  The new pollset is only active on platforms that support concurrent kernel
  poll updates, i.e. Linux and BSD.

  Own Id: OTP-15475 Aux Id: ERIERL-229

- Fix bug where emulator would segfault if a literal message was sent when
  sequence tracing was enabled.

  Own Id: OTP-15478 Aux Id: ERL-741

## Erts 10.1.3

### Improvements and New Features

- Added an optional `./configure` flag to compile the emulator with spectre
  mitigation: `--with-spectre-mitigation`

  Note that this requires a recent version of GCC with support for spectre
  mitigation and the `--mindirect-branch=thunk` flag, such as `8.1`.

  Own Id: OTP-15430 Aux Id: ERIERL-237

## Erts 10.1.2

### Fixed Bugs and Malfunctions

- Fixed a rare bug where files could be closed on a normal instead of an IO
  scheduler, resulting in system instability if the operation blocked.

  Own Id: OTP-15421

## Erts 10.1.1

### Fixed Bugs and Malfunctions

- A bug where the socket option 'pktoptions' caused a read of uninitialized
  memory has been fixed. Would cause malfunction on FreeBSD.

  Own Id: OTP-14297 Aux Id: OTP-15141

- Fixed a memory leak on errors when reading files.

  Own Id: OTP-15318

- File access through UNC paths works again on Windows. This regression was
  introduced in OTP 21.

  Own Id: OTP-15333 Aux Id: ERL-737

## Erts 10.1

### Fixed Bugs and Malfunctions

- Fix the seq_trace token to not be cleared when a process receives messages
  sent by erts. Some examples of when this could happen is all port BIFs, i.e.
  `open_port`, `port_command` etc etc.

  Fix so that messages sent by nifs can be traced using normal and `seq_trace`
  tracing.

  Own Id: OTP-15038 Aux Id: ERL-602

- Fixed specs and documentation for `process_info` item `monitored_by` to
  include port identifiers and nif resources as possible types.

  Own Id: OTP-15180 Aux Id: ERL-648

- Fix bug in generation of erl_crash.dump, which could cause VM to crash.

  Bug exist since erts-9.2 (OTP-20.2).

  Own Id: OTP-15181

- Fix bug where ctrl-break or ctrl-c would not trigger the break mode properly
  on Windows. This bug was introduced in erts-10.0 (OTP-21).

  Own Id: OTP-15205

- Fix a performance bug for reception of UDP packages, where a memory buffer
  would be reallocated when it should not have been.

  Introduce a limit on the maximum automatic increase of the UDP user-space
  buffer to the theoretical max of the network PATH, i.e. 65535.

  Own Id: OTP-15206

- Fix alignment of erts allocator state internally in erts. With the improper
  alignment the emulator would refuse to start when compiled with clang on
  32-bit systems.

  Own Id: OTP-15208 Aux Id: PR-1897 ERL-677

- Fix bug where too many concurrent calls to
  `erlang:open_port({spawn,"cmd"},...)` would result in the emulator terminating
  with the reason "Failed to write to erl_child_setup: ". After this fix the
  `open_port` call will throw an `emfile` exception instead.

  Own Id: OTP-15210

- Upgraded the ERTS internal PCRE library from version 8.41 to version 8.42. See
  [http://pcre.org/original/changelog.txt](http://pcre.org/original/changelog.txt)
  for information about changes made to PCRE. This library implements major
  parts of the `m:re` regular expressions module.

  Own Id: OTP-15217

- Fix `open_port({fd,X,Y}, ...)` to release the file descriptors from the
  pollset when closing the port. Without this fix the same file descriptor
  number could not be reused when doing multiple open_port and port_close
  sequences.

  Own Id: OTP-15236 Aux Id: ERL-692

- Fixed bug in [`float_to_list/2`](`float_to_list/2`) and
  [`float_to_binary/2`](`float_to_binary/2`) with options
  `[{decimals,0},compact]` causing totally wrong results. Bug exists since
  OTP-21.0.

  Own Id: OTP-15276 Aux Id: PR-1920

- Fixed bug in `erlang:memory` causing `ets` to report too much. This small
  false memory leak (16 bytes each time) can only happen when a specific race
  condition occurs between scheduler threads on a table with option
  `write_concurrency`.

  Own Id: OTP-15278

- Minor `configure` test fixes

  Own Id: OTP-15282

- Improved robustness of distribution connection setup. In OTP-21.0 a truly
  asynchronous connection setup was introduced. This is further improvement on
  that work to make the emulator more robust and also be able to recover in
  cases when involved Erlang processes misbehave.

  Own Id: OTP-15297 Aux Id: OTP-15279, OTP-15280

### Improvements and New Features

- The socket options `recvtos`, `recvttl`, `recvtclass` and `pktoptions` have
  been implemented in the socket modules. See the documentation for the
  `gen_tcp`, `gen_udp` and `inet` modules. Note that support for these in the
  runtime system is platform dependent. Especially for `pktoptions` which is
  very Linux specific and obsoleted by the RFCs that defined it.

  Own Id: OTP-15145 Aux Id: ERIERL-187

## Erts 10.0.8

### Fixed Bugs and Malfunctions

- As of ERTS version 10.0 (OTP 21.0) the `erl_child_setup` program, which
  creates port programs, ignores `TERM` signals. This setting was
  unintentionally inherited by port programs. Handling of `TERM` signals in port
  programs has now been restored to the default behavior. That is, terminate the
  process.

  Own Id: OTP-15289 Aux Id: ERIERL-235, OTP-14943, ERL-576

- The fix made for OTP-15279 in erts-10.07 (OTP-21.0.8) was not complete. It
  could cause a new connection attempt to be incorrectly aborted in certain
  cases. This fix will amend that flaw.

  Own Id: OTP-15296 Aux Id: OTP-15279, ERIERL-226

## Erts 10.0.7

### Fixed Bugs and Malfunctions

- A process could get stuck in an infinite rescheduling loop between normal and
  dirty schedulers. This bug was introduced in ERTS version 10.0.

  Thanks to Maxim Fedorov for finding and fixing this issue.

  Own Id: OTP-15275 Aux Id: PR-1943

- Garbage collection of a distribution entry could cause an emulator crash if
  `net_kernel` had not brought previous connection attempts on it down properly.

  Own Id: OTP-15279 Aux Id: ERIERL-226

## Erts 10.0.6

### Fixed Bugs and Malfunctions

- A race between termination of a process and resume of the same process via
  `erlang:resume_process/1` could cause the VM to crash. This bug was introduced
  in erts version 10.0 (OTP 21.0).

  Own Id: OTP-15237

- When tracing on `running`, `in` trace events could be lost when a process was
  rescheduled between a dirty and a normal scheduler.

  Own Id: OTP-15269 Aux Id: ERL-713

## Erts 10.0.5

### Fixed Bugs and Malfunctions

- Fixed a bug which caused an emulator crash when `enif_send()` was called by a
  NIF that executed on a dirty scheduler. The bug was either triggered when the
  NIF called `enif_send()` without a message environment, or when the process
  executing the NIF was `send` traced.

  Own Id: OTP-15223

- Fixed a bug causing some Erlang references to be inconsistently ordered. This
  could for example cause failure to look up certain elements with references as
  keys in search data structures. This bug was introduced in R13B02.

  Thanks to Simon Cornish for finding the bug and supplying a fix.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15225

## Erts 10.0.4

### Fixed Bugs and Malfunctions

- Fixed a bug that prevented the `noshell` option from working correctly on Mac
  OS X and BSD.

  Own Id: OTP-15169

- Fixed a crash when matching directly against a literal map using a single key
  that had been saved on the stack.

  Own Id: OTP-15184

- Fix node crash when passing a bad time option to `file:read_file_info/2`.

  Own Id: OTP-15196

## Erts 10.0.3

### Fixed Bugs and Malfunctions

- Fixed a scheduler bug that caused normal schedulers to run dirty code.

  Own Id: OTP-15154

- Fixed a bug in `erlang:trace_info/2` which caused the emulator to crash when a
  bad argument was passed. The bug was introduced in ERTS version 10.0.

  Own Id: OTP-15183 Aux Id: ERL-670

## Erts 10.0.2

### Fixed Bugs and Malfunctions

- Fixed a rare bug that could cause processes to be scheduled after they had
  been freed.

  Own Id: OTP-15067 Aux Id: ERL-573

- Fixed a race condition in the inet driver that could cause receive to hang
  when the emulator was compiled with gcc 8.

  Own Id: OTP-15158 Aux Id: ERL-654

## Erts 10.0.1

### Fixed Bugs and Malfunctions

- The keys used in `os:getenv` and `os:putenv` are case-insensitive again on
  Windows.

  Own Id: OTP-15147 Aux Id: ERL-644

## Erts 10.0

### Fixed Bugs and Malfunctions

- The type specifications for [`file:posix/0`](`t:file:posix/0`) and
  [`inet:posix/0`](`t:inet:posix/0`) have been updated according to which errors
  file and socket operations should be able to return.

  Own Id: OTP-14019 Aux Id: ERL-550

- Fix error printout from run_erl and a bug that could cause unintended fds to
  be leaked into the started program.

  Own Id: OTP-14537 Aux Id: PR1529

- File operations used to accept [filenames](`t:file:name_all/0`) containing
  null characters (integer value zero). This caused the name to be truncated and
  in some cases arguments to primitive operations to be mixed up. Filenames
  containing null characters inside the filename are now _rejected_ and will
  cause primitive file operations to fail.

  Also environment variable operations used to accept
  [names](`t:os:env_var_name/0`) and [values](`t:os:env_var_value/0`) of
  environment variables containing null characters (integer value zero). This
  caused operations to silently produce erroneous results. Environment variable
  names and values containing null characters inside the name or value are now
  _rejected_ and will cause environment variable operations to fail.

  Primitive environment variable operations also used to accept the `$=`
  character in environment variable names causing various problems. `$=`
  characters in environment variable names are now also _rejected_.

  Also `os:cmd/1` now reject null characters inside its
  [command](`t:os:os_command/0`).

  `erlang:open_port/2` will also reject null characters inside the port name
  from now on.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14543 Aux Id: ERL-370

- Fix bugs related to the bookkeeping of microstate accounting states.

  Own Id: OTP-14652

- `os:putenv` and `os:getenv` no longer access the process environment directly
  and instead work on a thread-safe emulation. The only observable difference is
  that it's _not_ kept in sync with libc `getenv(3)` / `putenv(3)`, so those who
  relied on that behavior in drivers or NIFs will need to add manual
  synchronization.

  On Windows this means that you can no longer resolve DLL dependencies by
  modifying the `PATH` just before loading the driver/NIF. To make this less of
  a problem, the emulator now adds the target DLL's folder to the DLL search
  path.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14666

- Corrected `erlang:is_builtin(erlang, M, F)` to return `true` for
  [`apply/2`](`apply/2`) and `yield/0`.

  Own Id: OTP-14713 Aux Id: ERL-500

- Fixed a bug where the PATH environment variable wasn't updated correctly on a
  release downgrade, effectively keeping the PATH of the new release.

  Own Id: OTP-14719

- A receive optimization that avoids scanning the entire message queue when
  receiving a message containing a freshly created reference could in rare
  circumstances (involving recursive calls to the functions that does the
  receive) cause the receive to hang. This has been corrected.

  Own Id: OTP-14782 Aux Id: ERL-511

- Fix building of Erlang/OTP on platforms which have small data area with short
  addressing. For example the PowerPC/RTEMS platform.

  Own Id: OTP-14909 Aux Id: PR-1692

- Fixed a crash when `enif_make_binary` is called with a binary produced by
  `enif_inspect_binary` in a different environment.

  Own Id: OTP-14931

- Fixed a crash when `enif_make_binary` is called more than once with a binary
  that had previously been added to an `enif_ioq`.

  Own Id: OTP-14932

- The erl_child_setup program now ignores SIGTERM signals.

  Own Id: OTP-14943 Aux Id: ERL-576

- Force 64-bit alignment on pre-allocators on architectures which needs it.

  Own Id: OTP-14977

- Fixed a bug where dirty scheduler picked up non-dirty work.

  Own Id: OTP-14978

- Calls to `gen_tcp:send/2` on closed sockets now returns `{error, closed}`
  instead of `{error,enotconn}`.

  Own Id: OTP-15001

- `erlang:monotonic_time/1` failed with `badarg` when passing the `perf_counter`
  time unit as argument.

  Own Id: OTP-15008

- Fix bug where rapid `init:restart()` calls would sometimes crash because a
  code load request leaked in between the restarts.

  Own Id: OTP-15013

- Improve [`float_to_list(F, [{decimals,D}])`](`float_to_list/2`) to closer
  conform with `io_lib:format("~.*f", [D,F])`.

  There are however, still cases when `float_to_list` does not produce the exact
  same result as `io_lib:format`, especially for large values `F` and/or many
  decimals `D`.

  Own Id: OTP-15015 Aux Id: OTP-14890

- Fixed a deadlock that would occur on certain allocators when a reallocation
  failed with `+ramv` enabled.

  Own Id: OTP-15024

- Fix bug that made it impossible to use an erl_tracer as the seq_trace trace
  receiver.

  Own Id: OTP-15029

- Fix bug where a large (> 1 GB) emulator generated error logger message would
  cause the emulator to crash.

  Own Id: OTP-15032

- The emulator will no longer crash when reading the file information of an
  ordinary file that has an NTFS reparse point, such as files stored in a
  OneDrive-mapped folder.

  Own Id: OTP-15062 Aux Id: ERL-615

- Fixed bug in `enif_binary_to_term` which could cause memory corruption for
  immediate terms (atoms, small integers, pids, ports, empty lists).

  Own Id: OTP-15080

- Fixed bug in `erlang:system_profile/2` that could cause superfluous
  `{profile,_,active,_,_}` messages for terminating processes.

  Own Id: OTP-15085

- On OSs with per thread CPU time support, change `cpu_timestamp` in
  `erlang:trace/3` to use it instead of per process CPU time. This makes this
  option usable on such OSs when running multiple schedulers.

  Own Id: OTP-15090

- Fix segfault in abort_signal_task which could happen if a port terminated
  while there were outstanding port tasks that were not signals, for example a
  ready_input/ready_output event.

  Own Id: OTP-15108 Aux Id: ERL-621

- Fixed bug in `ets` that could cause VM crash if process A terminates after
  fixating a table and process B deletes the table at "the same time". The table
  fixation could be done with `ets:safe_fixtable` or if process A terminates in
  the middle of a long running `select` or `match` call.

  Own Id: OTP-15109

- Owner and group changes through `file:write_file_info`, `file:change_owner`,
  and `file:change_group` will no longer report success on permission errors.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15118

- Fix a bug error reporting from escripts on windows where the error message
  would get garbled.

  Own Id: OTP-15119 Aux Id: PR-1826

- Fix segfault when a process is internally re-scheduled while being traced for
  in out events. This bug was introduced in erts-8.0 (OTP-19.0).

  Own Id: OTP-15125

### Improvements and New Features

- It is now possible to open device files and FIFOs with `file:open/2`.

  Own Id: OTP-11462

- The `erlang:system_flag(scheduler_wall_time,Bool)` call is now reference
  counted and will be turned off if the (last) process that started the
  performance statistics dies. Thus it is no longer possible to start the
  statistics with
  `rpc:call(Node, erlang, system_flag, [scheduler_wall_time, true])` since it
  will be turned off directly afterwards when the rpc process dies.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11694

- A new logging API is added to Erlang/OTP, see the `m:logger` manual page, and
  section [Logging](`e:kernel:logger_chapter.md`) in the Kernel User's Guide.

  Calls to `error_logger` are automatically redirected to the new API, and
  legacy error logger event handlers can still be used. It is, however,
  recommended to use the Logger API directly when writing new code.

  Notice the following potential incompatibilities:

  - Kernel configuration parameters `error_logger` still works, but is overruled
    if the default handler's output destination is configured with Kernel
    configuration parameter `logger`.

    In general, parameters for configuring error logger are overwritten by new
    parameters for configuring Logger.

  - The concept of SASL error logging is deprecated, meaning that by default the
    SASL application does not affect which log events are logged.

    By default, supervisor reports and crash reports are logged by the default
    Logger handler started by Kernel, and end up at the same destination
    (terminal or file) as other standard log event from Erlang/OTP.

    Progress reports are not logged by default, but can be enabled by setting
    the primary log level to info, for example with the Kernel configuration
    parameter `logger_level`.

    To obtain backwards compatibility with the SASL error logging functionality
    from earlier releases, set Kernel configuration parameter
    `logger_sasl_compatible` to `true`. This prevents the default Logger handler
    from logging any supervisor-, crash-, or progress reports. Instead, SASL
    adds a separate Logger handler during application start, which takes care of
    these log events. The SASL configuration parameters `sasl_error_logger` and
    `sasl_errlog_type` specify the destination (terminal or file) and severity
    level to log for these events.

  Since Logger is new in Erlang/OTP 21.0, we do reserve the right to introduce
  changes to the Logger API and functionality in patches following this release.
  These changes might or might not be backwards compatible with the initial
  version.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13295

- `gen_sctp:connect_init/4` or rather connect in `inet_drv.c` for SCTP has been
  fixed to not check the write file descriptor for writeability after a connect,
  since for SCTP (SOCK_SEQPACKET) that property does not seem to be any kind of
  indicator for when a connect has finished. This fixes connects that the OS
  returned as "in progress" that was misinterpreted by `gen_sctp:connect_init`
  as failed.

  Own Id: OTP-13760 Aux Id: PR-1592

- The file driver has been rewritten as a NIF, decreasing the latency of file
  operations. Notable incompatibilities are:

  - The `use_threads` option for `file:sendfile/5` no longer has any effect; we
    either use non-blocking `sendfile(2)` or fall back to `file:read` \+
    `gen_tcp:send`.
  - The file-specific DTrace probes have been removed. The same effect can be
    achieved with normal tracing together with the `nif__entry`/`nif__return`
    probes to track scheduling.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14256

- The I/O polling functionality of erts has been re-written to better make use
  of the OSs polling mechanisms. This change means that erts will now always
  prefer to use a kernel-polling mechanism if possible. Also all of the I/O
  polling has been moved to dedicated threads instead of being placed in the
  scheduler loops.

  As a result of this, the `erl` options `+K` and `+secio` have been removed. It
  is still possible to disable kernel-poll, but it has to be done at compile
  time through the configure option `--disable-kernel-poll`.

  The new `erl` options [`+IOt`](erl_cmd.md#%2BIOt) and
  [`+IOp`](erl_cmd.md#%2BIOp) can be used to change how many IO poll threads and
  poll sets that erts should use. See their respective documentation for more
  details.

  Own Id: OTP-14346

- Truly asynchronous auto-connect. Earlier, when `erlang:send` was aimed toward
  an unconnected node, the function would not return until the connection setup
  had completed (or failed). Now the function returns directly after the message
  has been enqueued and the connection setup started.

  The same applies to all distributed operations that may trigger auto-connect,
  i.e. `'!'`, `send`, `link`, `monitor`, `monitor_node`, [`exit/2`](`exit/2`)
  and `group_leader`.

  The interface for all these functions are unchanged as they do not return
  connection failures. The only exception is `erlang:monitor` where a _possible
  incompatibility_ is introduced: An attempt to monitor a process on a primitive
  node (such as erl_interface or jinterface), where remote process monitoring is
  not implemented, will no longer fail with `badarg` exception. Instead a
  monitor will be created, but it will only supervise the connection to the
  node.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14370

- Changed the default behaviour of `.erlang` loading: `.erlang` is no longer
  loaded from the current directory. `c:erlangrc(PathList)` can be used to
  search and load an `.erlang` file from user specified directories.

  `escript`, `erlc`, `dialyzer` and `typer` no longer load an `.erlang` at all.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14439

- New functionality for implementation of alternative carriers for the Erlang
  distribution has been introduced. This mainly consists of support for usage of
  distribution controller processes (previously only ports could be used as
  distribution controllers). For more information see
  [ERTS User's Guide ➜ How to implement an Alternative Carrier for the Erlang Distribution ➜ Distribution Module](alt_dist.md#distribution-module).

  Own Id: OTP-14459

- Add support for the lcc compiler and in extension the Elbrus 2000 platform.

  Own Id: OTP-14492

- Support for "tuple calls" have been removed from the run-time system. Tuple
  calls was an undocumented and unsupported feature which allowed the module
  argument for an apply operation to be a tuple: `Var = dict:new(), Var:size()`.
  This "feature" frequently caused confusion, especially when such call failed.
  The stacktrace would point out functions that don't exist in the source code.

  For legacy code that need to use parameterized modules or tuple calls for some
  other reason, there is a new compiler option called `tuple_calls`. When this
  option is given, the compiler will generate extra code that emulates the old
  behavior for calls where the module is a variable.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14497

- Creation of small maps with literal keys has been optimized to be faster and
  potentially use less memory. The keys are combined into a literal key tuple
  which is put into the literal pool. The key tuple can be shared between many
  instances of maps having the same keys.

  Own Id: OTP-14502

- When an exception is thrown, include the arguments of the call in the
  stacktrace for BIFs `band`, `bor`, `bsl`, `bsr`, `bxor`, `div`, `rem` and the
  operators `+`, `-`, `*` and `/`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14508

- The non-smp emulators have been removed. This means that the configure options
  `--disable-threads` and `--enable-plain-emulator` have been removed and
  configure will now refuse to build Erlang/OTP on platforms without thread
  support.

  In order to achieve a similar setup as the non-smp emulator, it is possible to
  start Erlang/OTP with the `+S 1` option.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14518

- Modules that use floating point constants compiled with R15 or earlier will
  need to be re-compiled before they can be loaded.

  Own Id: OTP-14575

- Implementation of true asynchronous signaling between processes in order to
  improve scalability. Signals affected include exit, monitor, demonitor,
  monitor triggered, link, unlink, and group leader.

  Own Id: OTP-14589

- Added a PGO (profile guided optimization) pass to the build step of erts. This
  can be disabled by passing --disable-pgo to configure.

  Own Id: OTP-14604

- Improved the performance of `binary:split` and `binary:match`.

  Own Id: OTP-14610 Aux Id: PR-1480

- It is not longer possible to disable dirty schedulers when building erlang.

  Own Id: OTP-14613

- Loaded BEAM code in a 64-bit system requires less memory because of better
  packing of operands for instructions.

  These memory savings were achieved by major improvements to the `beam_makeops`
  scripts used when building the run time system and BEAM compiler. There is
  also new for documentation for `beam_makeops` that describes how new BEAM
  instructions and loader transformations can be implemented. The documentation
  is found in here in a source directory or git repository:
  erts/emulator/internal_doc/beam_makeops.md. An online version can be found
  here:
  https://github.com/erlang/otp/blob/master/erts/emulator/internal_doc/beam_makeops.md

  Own Id: OTP-14626

- `file:read_file` has been changed to read the content of files that report a
  size of 0 even when data can be read from them. An example of such a file is
  `/proc/cpuinfo` on Linux.

  Own Id: OTP-14637 Aux Id: ERL-327 PR-1524

- It is no longer possible to disable the `temp_alloc` allocator. Disabling it
  caused serious performance degradations and was never what was wanted.

  Own Id: OTP-14651

- The reduction cost of sending messages is now constant. It will no longer
  scale according to the length of the receiving process' message queue.

  Own Id: OTP-14667

- Improved loading of modules with `-on_load` directive, to no longer block all
  schedulers when the load operation is completed.

  Own Id: OTP-14680

- On platforms with real-time signals available, SIGRTMIN+1 is now used as the
  internal scheduler suspend signal instead of SIGUSR2.

  Own Id: OTP-14682

- When the value returned from a '`catch`' expression is ignored, no stacktrace
  will be built if an exception is caught. That will save time and produce less
  garbage. There are also some minor optimizations of '`try`/`catch`' both in
  the compiler and run-time system.

  Own Id: OTP-14683

- The guarantees and non-guarantees of `erlang:get_stacktrace/0` are now
  documented.

  Own Id: OTP-14687

- There is a new syntax in '`try/catch`' for retrieving the stacktrace without
  calling '`erlang:get_stacktrace/0`'. See the reference manual for a
  description of the new syntax. The '`erlang:get_stacktrace/0`' BIF is now
  deprecated.

  Own Id: OTP-14692

- New 'used' option for [`binary_to_term/2`](`binary_to_term/2`) that will also
  return number of bytes actually read from the binary. This enables easy access
  to any extra data in the binary located directly after the returned term.

  Own Id: OTP-14780

- Added more statistics for `erlang:system_info({allocator,A})` in the
  `mbcs_pool` section.

  Own Id: OTP-14795 Aux Id: ERL-88

- Added `enif_ioq_peek_head` to retrieve Erlang terms from NIF IO queues without
  having to resort to copying.

  Own Id: OTP-14797

- There is a new option '`makedep_side_effect`' for the compiler and `-MMD` for
  '`erlc`' that generates dependencies and continues to compile as normal.

  Own Id: OTP-14830

- Added `ets:whereis/1` for retrieving the table identifier of a named table.

  Own Id: OTP-14884

- `seq_trace` labels may now be any erlang term.

  Own Id: OTP-14899

- Optimized the common case of `monitor` followed by `send` to the same local
  process. The monitor signal is now delayed in order to be piggybacked with the
  sent message and thereby only get one lock operation on the message queue of
  the receiver. A delayed monitor signal is flushed if no `send` has been done
  at the latest when the process is scheduled out.

  Own Id: OTP-14901

- Make hipe compiled code work on x86_64 (amd64) with OS security feature PIE,
  where executable code can be loaded into a random location. Old behavior, if
  hipe was enabled, was to disable PIE build options for the VM.

  Own Id: OTP-14903

- The number of driver async threads will now default to 1 as the standard
  drivers do not use them anymore. Users that changed this value to tweak the
  file driver should replace `+A` with `+SDio` since it now uses dirty IO
  schedulers instead of async threads.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14928

- Optimize `==` and `/=` for binaries with different sizes to be constant in
  time instead of proportional to the size of their common prefix.

  Own Id: OTP-14934 Aux Id: PR-1708

- Refactorings making some internal process flags available for other usage.

  Own Id: OTP-14948

- Removed need for HiPE to allocate native executable memory in low 2GB address
  space on x86_64. Command line option `+MXscs` is thereby obsolete and ignored.

  Own Id: OTP-14951

- Added `enif_make_map_from_arrays` for creating a populated map, analogous to
  `enif_make_list_from_array`.

  Own Id: OTP-14954

- Added configuration switches for busy-wait and wake up thresholds for dirty
  schedulers, and changing these settings for normal schedulers will no longer
  affect dirty schedulers.

  Refer to the documentation for details. The new switches are
  [\+sbwtdcpu](erl_cmd.md#%2Bsbwtdcpu), [\+sbwtdio](erl_cmd.md#%2Bsbwtdio),
  [\+swtdcpu](erl_cmd.md#%2Bswtdcpu), and [\+swtdio](erl_cmd.md#%2Bswtdio).

  The default busy wait threshold for dirty scheduler threads has also been
  lowered to `short`.

  Own Id: OTP-14959

- The list of "taints" now also includes dynamic loaded drivers in addition to
  NIF libraries. Statically linked drivers and NIF libraries that are part of
  erts are not included. The "taints" are returned by `system_info(taints)` and
  printed in the header of `erl_crash.dump` files.

  Own Id: OTP-14960

- Added `instrument:allocations` and `instrument:carriers` for retrieving
  information about memory utilization and fragmentation.

  The old `instrument` interface has been removed, as have the related options
  `+Mim` and `+Mis`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14961

- The process suspend functionality used by the `erlang:suspend_process/2` BIF
  has been reimplemented using the newly introduced true asynchronous signaling
  between processes. This mainly to reduce memory usage in the process control
  block of all processes, but also in order to simplify the implementation.

  > #### Warning {: .warning }
  >
  > You can easily create deadlocks if processes suspends each other (directly
  > or in circles). In ERTS versions prior to ERTS version 10.0, the runtime
  > system prevented such deadlocks, but this prevention has now been removed
  > due to performance reasons.

  Other ERTS internal functionality that used the previous process suspend
  functionality have also been reimplemented to use asynchronous signaling
  instead.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14964 Aux Id: OTP-14589

- Added the `nifs` option to `?MODULE:module_info/1` for listing a module's
  installed NIF functions.

  Own Id: OTP-14965

- New implementation of `erlang:process_info/[1,2]`.

  In the general case when inspecting another process, the new implementation
  sends an asynchronous process-info request signal to the other process and
  waits for the result instead of locking the other process and reading the
  result directly. In some special cases where no conflicts occur, signal order
  won't be violated, and the amount of data requested is guaranteed to be small,
  the inspected process may be inspected directly.

  Appropriate amount of reductions are now also bumped when inspecting a
  process.

  Own Id: OTP-14966

- Removed process start time from crash dump in order to save memory in process
  control block.

  Own Id: OTP-14975 Aux Id: PR-1597

- Optimize `erlang:put/2` when updating existing key with a new immediate value
  (atom, small integer, pid, port).

  Own Id: OTP-14976

- `erlang:process_info/1` has been changed to no longer include `messages` by
  default. Instead `erlang:process_info/2` should be used.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14986 Aux Id: PR-1745

- New `erlang:system_info(ets_count)` to get total number of ets tables existing
  at the local node.

  Own Id: OTP-14987

- New NIF functions: `enif_mutex_name`, `enif_cond_name`, `enif_rwlock_name`,
  `enif_thread_name`, `enif_vfprintf`, `enif_vsnprintf`.

  Own Id: OTP-14994

- When `erlang:system_flag(backtrace_depth, 0)` has been called, all exceptions
  will now contain the entry for _one_ function (despite the zero). It used to
  be that a hand-made stack backtrace passed to `erlang:raise/3` would be
  truncated to an empty list.

  Own Id: OTP-15026

- Fixed bug for named `ets` tables which could cause unexpected results from
  matchspec iteration functions (`ets:select*` and `ets:match*`) if the table
  was deleted and recreated with the same name during the iteration. The
  iteration could incorrectly continue through the recreated table. The expected
  correct behavior is now for the iteration call to fail with a `badarg`
  exception if the table is deleted before the iteration has completed.

  Own Id: OTP-15031

- Two new guards BIFs operating on maps have been added:
  [`map_get/2`](`map_get/2`) and [`is_map_key/2`](`is_map_key/2`). They do the
  same as `maps:get/2` and `maps:is_key/2`, respectively, except that they are
  allowed to be used in guards.

  Own Id: OTP-15037 Aux Id: PR-1784, PR-1802

- Release run-queue lock while cleaning up terminated dirty process.

  Own Id: OTP-15081

- The callback module passed as `-epmd_module` to erl has been expanded to be
  able to do name and port resolving.

  Documentation has also been added in the `m:erl_epmd` reference manual and
  ERTS User's Guide
  [How to Implement an Alternative Node Discovery for Erlang Distribution](alt_disco.md).

  Own Id: OTP-15086 Aux Id: PR-1694

## Erts 9.3.3.15

### Fixed Bugs and Malfunctions

- A process could end up in a state where it got endlessly rescheduled without
  making any progress. This occurred when a system task, such as check of
  process code (part of a code purge), was scheduled on a high priority process
  trying to execute on a dirty scheduler.

  Own Id: OTP-16436 Aux Id: ERL-1152

- Fixed bug in `erlang:list_to_ref/1` when called with a reference created by a
  remote note. Function [`list_to_ref/1`](`list_to_ref/1`) is intended for
  debugging and not to be used in application programs. Bug exist since OTP
  20.0.

  Own Id: OTP-16438

- A process could get into an inconsistent state where it was runnable, but
  never scheduled for execution. This could occur when a mix of `normal` and
  `low` priority processes where scheduled on the same type of dirty scheduler
  simultaneously.

  Own Id: OTP-16446 Aux Id: ERL-1157

- Fixed erroneous mapping of exit reason from `kill` to `killed` on reception of
  some exit signals due to a broken link. This bug has existed since ERTS
  version 5.5.2 (OTP R11).

  This bug was also unknowingly fixed in ERTS version 10.0 (OTP 21.0) due to a
  new ERTS internal implementation of signaling between processes.

  Own Id: OTP-16465 Aux Id: ERL-1165, OTP-6160, OTP-14589

- Corrected the valid range of the `erl` command line argument
  [`+SDio <NumberOfDirtyIoSchedulers>`](erl_cmd.md#%2BSDio) from `0..1024` to
  `1..1024`. `+SDio 0` was erroneously allowed which just caused the VM to crash
  on the first dirty I/O job scheduled.

  Own Id: OTP-16481

## Erts 9.3.3.14

### Fixed Bugs and Malfunctions

- A process calling
  [`erlang:system_flag(multi_scheduling, block)`](`m:erlang#system_flag_multi_scheduling`)
  could end up blocked waiting for the operation to complete indefinitely.

  Own Id: OTP-16379

## Erts 9.3.3.13

### Fixed Bugs and Malfunctions

- A literal area could prematurely be released before all uses of it had been
  removed. This occurred either when a terminating process had a complex exit
  reason referring to a literal that concurrently was removed, or when a
  terminating process continued executing a dirty NIF accessing a literal (via
  the heap) that concurrently was removed.

  Own Id: OTP-16193

## Erts 9.3.3.12

### Fixed Bugs and Malfunctions

- The runtime system disconnected a connection if it received an
  [`exit/2`](`exit/2`) signal where the recipient was a process on an old
  incarnation of the current node. That is, the receiving node had the same node
  name, but another "creation" number. The signal will now just be dropped since
  the receiving process no longer exists.

  Own Id: OTP-15867 Aux Id: ERIERL-373

- Fix a bug in `binary_to_term` that would crash the emulator if a term larger
  than 16GB was to be decoded.

  Own Id: OTP-16058 Aux Id: PR-2382

- When communicating with a simultaneously exiting port via the
  `erlang:port_*()` BIFs one could sometimes get stray `{Ref, What}` messages.
  Where `Ref` was a reference and `What` usually were the atom `badarg`.

  Own Id: OTP-16107 Aux Id: ERL-1049

## Erts 9.3.3.11

### Fixed Bugs and Malfunctions

- Fixed a buffer overflow when
  [`binary_to_existing_atom/2`](`binary_to_existing_atom/2`) and
  `list_to_existing_atom/2` was used with the `latin1` encoding.

  Own Id: OTP-15819 Aux Id: ERL-944

## Erts 9.3.3.10

### Fixed Bugs and Malfunctions

- Fixes of install/release phase in build system.

  - The source tree was modified when installing/releasing and/or applying a
    patch.
  - Some files were installed with wrong access rights.
  - If applying a patch (using `otp_patch_apply`) as another user (except root)
    than the user that built the source, the documentation was not properly
    updated.

  Own Id: OTP-15551

- Minor fixes for `make clean`.

  Own Id: OTP-15657

- Fixed a bug in all `ets:select*` and `ets:match*` functions that could in some
  rare cases lead to very poor performance.

  Own Id: OTP-15660 Aux Id: ERL-869

- Fix a possible deadlock when terminating the ERTS caused by a dirty scheduler
  not releasing it's run-queue lock when terminating.

  Own Id: OTP-15690 Aux Id: PR-2172

- Add missing documentation of new external tags `NEW_PID`, `NEW_PORT` and
  `NEWER_REFERENCE` introduced in OTP 19.

  These new tags are planned to be "activated" in OTP 23 when distribution
  capability flag `DFLAG_BIG_CREATION` becomes mandatory. Older nodes (>= 19)
  are able to decode these new tags and send them back to the new node. Nodes
  older than OTP 23 will however never encode their own local pids, ports and
  references using the new tags.

  Own Id: OTP-15766

## Erts 9.3.3.9

### Improvements and New Features

- Added an optional `./configure` flag to compile the emulator with spectre
  mitigation: `--with-spectre-mitigation`

  Note that this requires a recent version of GCC with support for spectre
  mitigation and the `--mindirect-branch=thunk` flag, such as `8.1`.

  Own Id: OTP-15430 Aux Id: ERIERL-237

## Erts 9.3.3.8

### Fixed Bugs and Malfunctions

- A bug that could cause dirty schedulers to become unresponsive has been fixed.

  Own Id: OTP-15509 Aux Id: PR-2027, PR-2093

## Erts 9.3.3.7

### Fixed Bugs and Malfunctions

- Fixed bug in operator `band` of two negative operands causing erroneous result
  if the absolute value of one of the operands have the lowest `N*W` bits as
  zero and the other absolute value is not larger than `N*W` bits. `N` is an
  integer of 1 or larger and `W` is 32 or 64 depending on word size.

  Own Id: OTP-15487 Aux Id: ERL-804

## Erts 9.3.3.6

### Improvements and New Features

- List subtraction (The `--` operator) will now yield properly on large inputs.

  Own Id: OTP-15371

## Erts 9.3.3.5

### Fixed Bugs and Malfunctions

- ERTS internal trees of monitor structures could get into an inconsistent
  state. This could cause `'DOWN'` messages not to be delivered when they
  should, as well as delivery of `'DOWN'` messages that should not be delivered.

  This bug was introduced in ERTS version 9.0 (OTP 20.0) and was fixed in ERTS
  version 10.0 (OTP 21.0) due to a rewrite of the monitor code. That is, this
  bug only exist in the OTP 20 release.

  Own Id: OTP-15399 Aux Id: ERL-751, ERIERL-262, OTP-14205

## Erts 9.3.3.4

### Fixed Bugs and Malfunctions

- Fixed bug in `ets:select_replace` when called with a fully bound key could
  cause a following call to `ets:next` or `ets:prev` to crash the emulator or
  return invalid result.

  Own Id: OTP-15346

## Erts 9.3.3.3

### Fixed Bugs and Malfunctions

- Fixed a bug which caused an emulator crash when `enif_send()` was called by a
  NIF that executed on a dirty scheduler. The bug was either triggered when the
  NIF called `enif_send()` without a message environment, or when the process
  executing the NIF was `send` traced.

  Own Id: OTP-15223

- Fixed a bug causing some Erlang references to be inconsistently ordered. This
  could for example cause failure to look up certain elements with references as
  keys in search data structures. This bug was introduced in R13B02.

  Thanks to Simon Cornish for finding the bug and supplying a fix.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15225

## Erts 9.3.3.2

### Fixed Bugs and Malfunctions

- Fixed a race condition in the inet driver that could cause receive to hang
  when the emulator was compiled with gcc 8.

  Own Id: OTP-15158 Aux Id: ERL-654

- Fix bug in generation of erl_crash.dump, which could cause VM to crash.

  Bug exist since erts-9.2 (OTP-20.2).

  Own Id: OTP-15181

## Erts 9.3.3.1

### Fixed Bugs and Malfunctions

- Fixed a rare bug that could cause processes to be scheduled after they had
  been freed.

  Own Id: OTP-15067 Aux Id: ERL-573

## Erts 9.3.3

### Fixed Bugs and Malfunctions

- Fixed bug in `ets` that could cause VM crash if process A terminates after
  fixating a table and process B deletes the table at "the same time". The table
  fixation could be done with `ets:safe_fixtable` or if process A terminates in
  the middle of a long running `select` or `match` call.

  Own Id: OTP-15109

## Erts 9.3.2

### Fixed Bugs and Malfunctions

- Fixed bug in `enif_binary_to_term` which could cause memory corruption for
  immediate terms (atoms, small integers, pids, ports, empty lists).

  Own Id: OTP-15080

- Fixed bug in `erlang:system_profile/2` that could cause superfluous
  `{profile,_,active,_,_}` messages for terminating processes.

  Own Id: OTP-15085

## Erts 9.3.1

### Fixed Bugs and Malfunctions

- Fixed a crash in `heart:get_cmd/0` when the stored command was too long.

  Own Id: OTP-15034

## Erts 9.3

### Fixed Bugs and Malfunctions

- Fixed a `configure` test for `libz` internals that unintentionally caused
  various native code in OTP to link against `libz`. Under certain circumstances
  this caused the build of OTP to fail.

  Own Id: OTP-14840 Aux Id: ERL-529

- File names containing unicode codepoints larger than 255 were not correctly
  encoded in stack traces.

  Own Id: OTP-14847 Aux Id: ERL-544

- Fix HiPE bug for binary constructs like `<<X/utf8>>` which could in rare cases
  cause faulty results or VM crash.

  This fix affects both the `hipe` compiler and `erts` runtime in an
  _incompatible_ way. Old hipe compiled files need to be recompiled to load and
  run properly as native.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14850 Aux Id: PR-1664

- Fix [`term_to_binary/2`](`term_to_binary/2`) spec for `minor_version`.

  Own Id: OTP-14876 Aux Id: ERL-548

- Fix bug in erlang:binary_to_integer/2 where invalid characters were not
  detected for bases larger then 10. e.g.
  [`binary_to_integer(<<":">>, 16)`](`binary_to_integer/2`) would return 3 and
  not badarg as it should.

  Own Id: OTP-14879

- Fixed bug in [`float_to_list/2`](`float_to_list/2`) and
  [`float_to_binary/2`](`float_to_binary/2`) with option `decimals` that caused
  a faulty rounding up of the last decimal digit for about 6% of floats with a
  fraction part.

  For example, [`float_to_list(0.145, [{decimals,1}])`](`float_to_list/2`)
  returned `"0.2"` instead of `"0.1"`.

  Own Id: OTP-14890

- Fix bug causing slow hipe execution in modules loaded early during boot or
  loaded by `code:atomic_load` or `code:finish_loading`.

  Own Id: OTP-14891

- Fixed a buffer overflow in an internal string formatting function that could
  be hit if specifying a long floating-point format specifier to `erts_sprintf`
  or similar.

  Own Id: OTP-14920

- `erlang:iolist_to_iovec/1` and `enif_inspect_iovec` will no longer fail when
  provided with binaries that have been matched-out on a non-byte boundary.

  Own Id: OTP-14921

- [`iolist_to_binary/1`](`iolist_to_binary/1`) and `erlang:iolist_to_iovec/1`
  will now badarg if supplied with a bitstring (without a list).

  Own Id: OTP-14926

- Reject loading modules with names containing directory separators ('/' or '\\'
  on Windows).

  Own Id: OTP-14933 Aux Id: ERL-564, PR-1716

- Fix potential dead-lock when the tracer process dies while a the traced
  process is running on a dirty scheduler.

  Own Id: OTP-14938

### Improvements and New Features

- More crash dump info such as: process binary virtual heap stats, full info for
  process causing out-of-mem during GC, more port related info, and dirty
  scheduler info.

  Own Id: OTP-14820

## Erts 9.2.1

### Improvements and New Features

- Improve search algorithm of abandoned memory carriers. Instead of limited
  linear search, each allocator instance maintain a balanced search tree of all
  its abandoned carriers for faster and more exhaustive search.

  Own Id: OTP-14915 Aux Id: ERIERL-88

- New erts_alloc command line options `+M_acnl` and `+M_acfml` to limit carrier
  abandonment.

  Own Id: OTP-14916 Aux Id: ERIERL-88

- New family of `erts_alloc` strategies: Age Order First Fit. Similar to
  "address order", but instead the oldest possible carrier is always chosen for
  allocation.

  Own Id: OTP-14917 Aux Id: ERIERL-88

- Add possibility to change allocator options at runtime with
  `system_info(erts_alloc, ...)`. Only option `sbct` (single block carrier
  threshold) is currently supported via this interface.

  Own Id: OTP-14918 Aux Id: ERIERL-88

## Erts 9.2.0.1

### Improvements and New Features

- Extra internal consistency checks wrt communication with erl_child_setup
  process.

  Own Id: OTP-15488 Aux Id: ERIERL-231

## Erts 9.2

### Fixed Bugs and Malfunctions

- Fix a bug in tracing where the \{caller\} match spec function would be set to
  undefined incorrectly when used in conjunction with return_to or return_trace
  on some functions.

  The functions effected are: erlang:put/2, erlang:erase/1,
  erlang:process_info/1,2, erlang:nif_load/2, erts_internal:garbage_collection/1
  and erts_internal:check_process_code/1.

  Because of this bug, the analysis done by fprof could become incorrect when
  the functions above are the tail-call in a function.

  Own Id: OTP-14677

- Fix emulator deadlock that would happen if `trap_exit` was set to true and a
  process sends an exit signal to itself using
  [`exit(self(), Reason)`](`exit/2`) while receive tracing was enabled for that
  process.

  Own Id: OTP-14678 Aux Id: ERL-495

- Writing of crash dumps is significantly faster.

  Maps are now included in crash dumps.

  Constants terms would only be shown in one process, while other processes
  referencing the same constant term would show a marker for incomplete heap.

  Own Id: OTP-14685 Aux Id: OTP-14611, OTP-14603, OTP-14595

- The fallback home directory for windows has been changed to be the PROFILE
  directory instead of the WINDOWS directory. The fallback is used when the
  environment variables HOMEDRIVE and HOMEPATH have not been set.

  Own Id: OTP-14691

- Fix bug for hipe compiled code using `<<X/utf32>>` binary construction that
  could cause faulty result or even VM crash.

  On architectures other than x86_64, code need to be recompiled to benefit from
  this fix.

  Own Id: OTP-14740

- Fixed bug in `erlang:garbage_collect/2` and `erlang:check_process_code/3`,
  when called with option `{async,ReqestId}`. Could cause VM crash or heap
  corruption if `RequestId` was an immediate term (like a pid, atom or small
  integer). Bug exists since OTP-17.0.

  Own Id: OTP-14752

- ERL*NIF_MINOR_VERSION wasn't bumped with the addition of `enif_ioq*\*`.

  Own Id: OTP-14779

- Purging of loaded code that contained "fake literals" (for example the magic
  reference obtained from '`ets:new/2`') would crash the runtime system.
  Corrected.

  Own Id: OTP-14791

- Setting the size of the atom table to a number near 2147483647 (using the
  '`+t`' option) would cause the emulator to exit with a failure to allocate a
  huge amount of memory. This has been corrected. Also the usage message for the
  '`+t`' option has been corrected to show the correct upper limit 2147483647
  instead of 0.

  Own Id: OTP-14796

- Fixed a bug that prevented registered process names from being resolved in
  lcnt results.

  Own Id: OTP-14803

- Formatting bugs were fixed in several HiPE debug BIFs.

  Own Id: OTP-14804

### Improvements and New Features

- Binaries and some other data in crash dumps are now encoded in base64 (instead
  of in hex), which will reduce the size of crash dumps.

  A few bugs in the handling of sub binaries in `crashdump_viewer` have been
  fixed.

  Own Id: OTP-14686

- Micro optimization for send operations of messages to other nodes. The local
  ack-message, which is otherwise sent back from TPC/IP port driver to sending
  client process, is now ignored earlier for distributed send operations.

  Own Id: OTP-14689

## Erts 9.1.5

### Fixed Bugs and Malfunctions

- Fixed a bug in file closure on Unix; close(2) was retried on EINTR which could
  cause a different (recently opened) file to be closed as well.

  Own Id: OTP-14775

- A race-condition when tearing down a connection with active node monitors
  could cause the runtime system to crash.

  This bug was introduced in ERTS version 8.0 (OTP 19.0).

  Own Id: OTP-14781 Aux Id: OTP-13047

## Erts 9.1.4

### Fixed Bugs and Malfunctions

- Microstate accounting sometimes produced incorrect results for dirty
  schedulers.

  Own Id: OTP-14707

- Fixed a regression in `zlib:gunzip/1` that prevented it from working when the
  decompressed size was a perfect multiple of 16384. This regression was
  introduced in 20.1.1

  Own Id: OTP-14730 Aux Id: ERL-507

- Fixed a memory corruption bug in `enif_inspect_iovec`; writable binaries
  stayed writable after entering the iovec.

  Own Id: OTP-14745

- Fixed a crash in `enif_inspect_iovec` on encountering empty binaries.

  Own Id: OTP-14750

- `zlib:deflateParams/3` will no longer return `buf_error` when called after
  `zlib:deflate/2` with zlib `1.2.11`.

  Own Id: OTP-14751

## Erts 9.1.3

### Fixed Bugs and Malfunctions

- Added zlib:set_controlling_process/2 to move a zstream() between processes.

  Own Id: OTP-14672 Aux Id: ERL-494

- Fix so that schedulers are bound correctly when the first available cpu is not
  the first detected cpu. e.g. when using "taskset -c X..Y" when X is not equal
  to 0.

  Own Id: OTP-14694

## Erts 9.1.2

### Fixed Bugs and Malfunctions

- Fixed bug that could cause a VM crash when a corrupt message is received on
  distribution channel from other node.

  Own Id: OTP-14661 Aux Id: ERIERL-80

## Erts 9.1.1

### Fixed Bugs and Malfunctions

- The new zlib module returned a data_error when inflating concatenated streams,
  which was incompatible with the old module's behavior of returning the
  uncompressed data up to the end of the first stream.

  Own Id: OTP-14648

- zlib:gunzip/1 will no longer stop at the end of the first stream when
  decompressing concatenated gzip files.

  Own Id: OTP-14649

## Erts 9.1

### Fixed Bugs and Malfunctions

- Changed `erlang:apply/2` to raise a `badarg` exception if the second argument
  is not a proper list. Previous behavior was a misleading `undef` exception.

  Own Id: OTP-14490 Aux Id: ERL-432

- On macOS, `crypto` would crash if `observer` had been started before `crypto`.
  On the beta for macOS 10.13 (High Sierra), `crypto` would crash. Both of those
  bugs have been fixed.

  Own Id: OTP-14499 Aux Id: ERL-251 ERL-439

- Fixed bug in enif_whereis_pid/port that could cause heap corruption in rare
  cases.

  Own Id: OTP-14523

- Fix so that trace messages generated when in a dirty nif are flushed correctly
  when the dirty nif is done executing.

  Own Id: OTP-14538

- Fix escape code handling when using ANSI color codes in the shell.

  Own Id: OTP-14549 Aux Id: PR1536

- Upgraded the ERTS internal PCRE library from version 8.40 to version 8.41. See
  [http://pcre.org/original/changelog.txt](http://pcre.org/original/changelog.txt)
  for information about changes made to PCRE. This library implements major
  parts of the `m:re` regular expressions module.

  Own Id: OTP-14574

- Fixed a bug causing [`statistics(runtime)`](`statistics/1`) to produce
  negative values and a bug in [`statistics(wall_clock)`](`statistics/1`)
  causing it to produce values one second too long.

  [`statistics(runtime)`](`statistics/1`) now also use `getrusage()` as source
  when available preventing the returned value from wrapping as frequent as
  before.

  Own Id: OTP-14597 Aux Id: ERL-465

- Fixed small memory leak that could occur when sending to a terminating port.

  Own Id: OTP-14609

- Fix bug causing VM crash when a module with `-on_load` directive is loaded
  while `erlang:trace(on_load, ...)` is enabled.

  Own Id: OTP-14612

- A warning that the compiler may optimize away atoms have been added to the
  documentation of [`list_to_existing_atom/1`](`list_to_existing_atom/1`) and
  [`binary_to_existing_atom/2`](`binary_to_existing_atom/2`).

  Own Id: OTP-14614 Aux Id: ERL-453

### Improvements and New Features

- Lock counting can now be fully toggled at runtime in the lock counting
  emulator (`-emu_type lcnt`). Everything is enabled by default to match the old
  behavior, but specific categories can be toggled at will with minimal runtime
  overhead when disabled. Refer to the documentation on `lcnt:rt_mask/1` for
  details.

  Own Id: OTP-13170

- The `zlib` module has been refactored and all its operations will now yield
  appropriately, allowing them to be used freely in concurrent applications.

  The following functions have been deprecated, but will not produce compiler
  warnings until OTP 21: `zlib:adler32`, `zlib:crc32`, `zlib:inflateChunk`,
  `zlib:getBufSize`, `zlib:setBufSize`.

  The behavior of throwing an error when a dictionary is required for
  decompression has also been deprecated. Refer to the documentation on
  `inflateSetDictionary/2` for details.

  Own Id: OTP-14185

- `lcnt:collect` and `lcnt:clear` will no longer block all other threads in the
  runtime system.

  Own Id: OTP-14412

- Add `erlang:iolist_to_iovec/1`, which converts an iolist() to an
  erlang:iovec(), which suitable for use with `enif_inspect_iovec`.

  Own Id: OTP-14520

- When provided with bad arguments, the `zlib` module will now raise named
  exceptions instead of just `badarg`. For example, `not_initialized` when using
  `zlib:inflate/2` with an uninitialized stream.

  Own Id: OTP-14527

- `erlang:halt/2` allows any Unicode string as slogan for the crash dump.

  Own Id: OTP-14553

- Add new nif API functions for managing an I/O Queue. The added functions are:

  - [`enif_ioq_create()`](erl_nif.md#enif_ioq_create)
  - [`enif_ioq_destroy()`](erl_nif.md#enif_ioq_destroy)
  - [`enif_ioq_enq_binary()`](erl_nif.md#enif_ioq_enq_binary)
  - [`enif_ioq_enqv()`](erl_nif.md#enif_ioq_enqv)
  - [`enif_ioq_deq()`](erl_nif.md#enif_ioq_deq)
  - [`enif_ioq_peek()`](erl_nif.md#enif_ioq_peek)
  - [`enif_inspect_iovec()`](erl_nif.md#enif_inspect_iovec)
  - [`enif_free_iovec()`](erl_nif.md#enif_free_iovec)

  Own Id: OTP-14598

## Erts 9.0.5

### Fixed Bugs and Malfunctions

- Fixed bug in `binary_to_term` and `binary_to_atom` that could cause VM crash.
  Typically happens when the last character of an UTF8 string is in the range
  128 to 255, but truncated to only one byte. Bug exists in `binary_to_term`
  since ERTS version 5.10.2 (OTP_R16B01) and `binary_to_atom` since ERTS version
  9.0 (OTP-20.0).

  Own Id: OTP-14590 Aux Id: ERL-474

## Erts 9.0.4

### Fixed Bugs and Malfunctions

- A timer internal bit-field used for storing scheduler id was too small. As a
  result, VM internal timer data structures could become inconsistent when using
  1024 schedulers on the system. Note that systems with less than 1024
  schedulers are not effected by this bug.

  This bug was introduced in ERTS version 7.0 (OTP 18.0).

  Own Id: OTP-14548 Aux Id: OTP-11997, ERL-468

- Automatic cleanup of a BIF timer, when the owner process terminated, could
  race with the timeout of the timer. This could cause the VM internal data
  structures to become inconsistent which very likely caused a VM crash.

  This bug was introduced in ERTS version 9.0 (OTP 20.0).

  Own Id: OTP-14554 Aux Id: OTP-14356, ERL-468

## Erts 9.0.3

### Fixed Bugs and Malfunctions

- Binary append operations did not check for overflow, resulting in nonsensical
  results when huge binaries were appended.

  Own Id: OTP-14524

## Erts 9.0.2

### Fixed Bugs and Malfunctions

- Added missing release notes for OTP-14491 ("performance bug in
  pre-allocators") which was included in erts-9.0.1 (OTP-20.0.1).

  Own Id: OTP-14494

- Fixed a bug that prevented TCP sockets from being closed properly on send
  timeouts.

  Own Id: OTP-14509

- Fixed bug in operator `bxor` causing erroneuos result when one operand is a
  big _negative_ integer with the lowest `N*W` bits as zero and the other
  operand not larger than `N*W` bits. `N` is an integer of 1 or larger and `W`
  is 32 or 64 depending on word size.

  Own Id: OTP-14514

## Erts 9.0.1

### Fixed Bugs and Malfunctions

- Fixed a bug in gen_tcp:send where it never returned when repeatedly called on
  a remotely closed TCP socket.

  Own Id: OTP-13939 Aux Id: ERL-193

- Fixed segfault that could happen during cleanup of aborted
  erlang:port_command/3 calls. A port_command is aborted if the port is closed
  at the same time as the port_command was issued. This bug was introduced in
  erts-8.0.

  Own Id: OTP-14481

- Fixed implementation of [`statistics(wall_clock)`](`statistics/1`) and
  [`statistics(runtime)`](`statistics/1`) so that values do not unnecessarily
  wrap due to the emulator. Note that the values returned by
  [`statistics(runtime)`](`statistics/1`) may still wrap due to limitations in
  the underlying functionality provided by the operating system.

  Own Id: OTP-14484

- Fix performance bug in pre-allocators that could cause them to permanently
  fall back on normal more expensive memory allocation. Pre-allocators are used
  for quick allocation of short lived meta data used by messages and other
  scheduled tasks. Bug exists since OTP_R15B02. \[this release note was missing
  in erts-9.0.1]

  Own Id: OTP-14491

## Erts 9.0

### Fixed Bugs and Malfunctions

- Fix various bugs regarding loading, upgrade and purge of HiPE compiled code:

  - The native code memory for a purged module was never deallocated.
  - Wrong functions could in some cases be called after a module upgrade.
  - `erlang:check_process_code` did not check for recursive calls made from
    native code.

  Own Id: OTP-13968

- Hipe optional LLVM backend does require LLVM version 3.9 or later as older
  versions forced strong dependencies on erts internals structures.

  Own Id: OTP-14238

- When an exception such as '[`throw(HugeTerm)`](`throw/1`)' was caught,
  `HugeTerm` term would be kept in the process until the next exception
  occurred, potentially increasing the heap size for the process. That has been
  corrected.

  Own Id: OTP-14255 Aux Id: OTP-14400, OTP-14401

- Slogans in crash dumps have been extended to print more complex terms.

  Own Id: OTP-14303

- Fixed bug when using `enif_inspect_binary` in combination with `enif_copy`. In
  some circumstances the inspected binary could be reallocated by the
  `enif_copy` call when it shouldn't have been.

  Own Id: OTP-14304

- The address family `local` (AF_UNIX / AF_LOCAL) now does not ensure zero
  termination of Linux Abstract Addresses so they can use all bytes.

  Own Id: OTP-14305

- Use `-fno-PIE` for Gentoo Hardened and others that don't accept linker flag
  `-no-pie`.

  Own Id: OTP-14307 Aux Id: PR-1379

- Disable hipe for `ppc64le` architecture (little endian) as it is not, and has
  never been, supported. It was earlier equated with `ppc64` (big endian) which
  lead to broken build without `--disable-hipe`.

  Own Id: OTP-14314 Aux Id: ERL-369, PR-1394

- Fix 'epmd -kill' to return a failure exit status code if epmd was not killed
  because of some error.

  Own Id: OTP-14324

- Fixed the following dirty scheduler related bugs:

  - the `+SDPcpu` command line argument could cause the amount of dirty CPU
    schedulers to be set to zero
  - `erlang:system_flag(multi_scheduling, _)` failed when only one normal
    scheduler was used together with dirty scheduler support

  Own Id: OTP-14335

- Fix erlexec to handle mismatch in sysconf and proc fs when figuring out the
  cpu topology. This behaviour has been seen when using docker together with
  `--cpuset-cpus`.

  Own Id: OTP-14352

- Fixed memory segment cache used for multiblock carriers. Huge (> 2GB) memory
  segments could cause a VM crash. Creation of such huge memory segments used
  for multiblock carriers is however very uncommon.

  Own Id: OTP-14360 Aux Id: ERL-401, PR-1417

- Fix bug causing `code:is_module_native` to falsely return true when `local`
  call trace is enabled for the module.

  Own Id: OTP-14390

- Fix emulator crash when receive tracing on a `trace_delivered` message.

  Own Id: OTP-14411

- Fix file:sendfile error handling on SunOS when a connection is closed during
  transmission.

  Own Id: OTP-14424

- `escript` did not handle paths with spaces correct.

  Own Id: OTP-14433

- Fix erroneous lock check assertion when `wx` is run on MacOS X.

  Own Id: OTP-14437 Aux Id: ERL-360

- Active-mode TCP sockets are now cleaned up properly on send/shutdown errors.

  Own Id: OTP-14441 Aux Id: ERL-430

- Fix compilation of hipe_mkliterals when the LIBS configure variable had to be
  set.

  Own Id: OTP-14447

### Improvements and New Features

- Added `erlang:garbage_collect/2` that takes an option list as the last
  argument that can be used to control whether a minor or a major garbage
  collection is to be done. Doing a minor collection only collects terms that
  have recently died, but is cheaper than a major collection.

  Own Id: OTP-11695

- Optimized test for tuples with an atom as first element.

  Own Id: OTP-12148

- Erlang literals are no longer copied during process to process messaging.

  Own Id: OTP-13529

- Add support in the `erl_nif` API for asynchronous message notifications when
  sockets or other file descriptors are ready to accept read or write
  operations. The following functions have been added:

  - enif_select
  - enif_monitor_process
  - enif_demonitor_process
  - enif_compare_monitors
  - enif_open_resource_type_x

  Own Id: OTP-13684

- There are two new guard BIFs '[`floor/1`](`floor/1`)' and
  '[`ceil/1`](`ceil/1`)'. They both return integers. In the '`math`' module,
  there are two new BIFs with the same names that return floating point values.

  Own Id: OTP-13692

- Remove deprecated `erlang:hash/2`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13827

- Replaced usage of deprecated symbolic [`time unit`](`t:erlang:time_unit/0`)
  representations.

  Own Id: OTP-13831 Aux Id: OTP-13735

- Added support in zlib for extraction of the inflation dictionary.

  Own Id: OTP-13842

- The previously used purge strategy has been removed. The optional purge
  strategy introduced in ERTS version 8.1 is now the only strategy available.

  The new purge strategy is slightly incompatible with the old strategy.
  Previously processes holding `fun`s that referred to the module being purged
  either failed a soft purge, or was killed during a hard purge. The new
  strategy completely ignores `fun`s. If `fun`s referring to the code being
  purged exist, and are used after a purge, an exception will be raised upon
  usage. That is, the behavior will be exactly the same as the case when a `fun`
  is received by a process after the purge.

  For more information see the documentation of `erlang:check_process_code/3`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13844 Aux Id: OTP-13833

- Dirty schedulers are now enabled by default when the runtime system is built
  with SMP support.

  Own Id: OTP-13860

- Improved ETS lookup/insert/delete speed for large `set`, `bag` and
  `duplicate_bag` by a significant reduction of the hash load factor. This speed
  improvement comes at the expense of less than one word per table entry. Tables
  with less than 256 entries are not affected at all.

  Own Id: OTP-13903

- The NIF library `reload` feature is not supported anymore. It has been marked
  as deprecated since OTP R15B. This means that you are only allowed to do one
  successful call to `erlang:load_nif/2` for each module instance. A second call
  to `erlang:load_nif/2` will return `{error, {reload, _}}` even if the NIF
  library implements the `reload` callback.

  Runtime upgrade of a NIF library is still supported by using the Erlang module
  upgrade mechanics with a current and an old module instance existing at the
  same time with their corresponding NIF libraries.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13908

- Add `erlang:system_info(atom_count)` and `erlang:system_info(atom_limit)` to
  provide a way to retrieve the current and maximum number of atoms.

  Own Id: OTP-13976

- The function `fmod/2` has been added to the `math` module.

  Own Id: OTP-14000

- `erlang:load_nif/2` returns new error type `notsup` when called for a HiPE
  compiled module, which is not supported.

  Own Id: OTP-14002

- Add driver and nif lock instrumentation to lcnt

  Own Id: OTP-14069

- Reduce memory pressure by converting sub-binaries to heap-binaries when
  possible. This is done during garbage collection.

  Own Id: OTP-14149

- Dirty schedulers are now enabled and supported on Erlang runtime systems with
  SMP support.

  Besides support for dirty NIFs also support for dirty BIFs and dirty garbage
  collection have been introduced. All garbage collections that potentially will
  take a long time to complete are now performed on dirty schedulers if enabled.

  `erlang:statistics/1` with arguments inspecting scheduler and run queue states
  have been changed due to the dirty scheduler support. Code using this
  functionality may have to be rewritten taking these incompatibilities into
  consideration. Examples of such uses are calls to
  [`erlang:statistics(scheduler_wall_time)`](`m:erlang#statistics_scheduler_wall_time`),
  [`statistics(total_run_queue_lengths)`](`m:erlang#statistics_total_run_queue_lengths`),
  [`statistics(total_active_tasks)`](`m:erlang#statistics_total_active_tasks`),
  etc.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14152

- Atoms may now contain arbitrary Unicode characters.

  Own Id: OTP-14178

- Introduce an event manager in Erlang to handle OS signals. A subset of OS
  signals may be subscribed to and those are described in the Kernel
  application.

  Own Id: OTP-14186

- The `escript` program now handles symbolic links to escripts.

  This is useful for standalone systems with `escript`s residing on a bin
  directory not included in the execution path (as it may cause their `erl`
  program(s) to override the desired one). Instead the `escript`s can be
  referred to via symbolic links from a bin directory in the path.

  Own Id: OTP-14201 Aux Id: PR-1293

- All uses of the magic binary kludge has been replaced by uses of erlang
  references.

  A magic binary was presented as an empty binary, but actually referred other
  data internally in the Erlang VM. Since they were presented as empty binaries,
  different magic binaries compared as equal, and also lost their internal data
  when passed out of an erlang node.

  The new usage of references has not got any of these strange semantic issues,
  and the usage of these references has been optimized to give the same
  performance benefits as well as memory usage benefits as magic binaries had.

  A couple of examples of previous uses of magic binaries are match
  specifications and NIF resources.

  Own Id: OTP-14205

- The non-smp emulators have been deprecated and are scheduled for removal in
  OTP-21.

  In preparation for this, the threaded non-smp emulator is no longer built by
  default and has to be enabled using the --enable-plain-emulator to configure.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14272

- Allow HiPE to run on VM built with `--enable-m32-build`.

  Own Id: OTP-14330 Aux Id: PR-1397

- Upgraded the OTP internal PCRE library from version 8.33 to version 8.40. This
  library is used for implementation of the `m:re` regular expressions module.

  Besides various bug fixes, the new version allows for better stack protection.
  In order to utilize this feature, the stack size of normal scheduler threads
  is now by default set to 128 kilo words on all platforms. The stack size of
  normal scheduler threads can be set upon system start by passing the
  [`+sss`](erl_cmd.md#sched_thread_stack_size) command line argument to the
  [`erl`](erl_cmd.md) command.

  See
  [http://pcre.org/original/changelog.txt](http://pcre.org/original/changelog.txt)
  for information about changes made to PCRE between the versions 8.33 and 8.40.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14331 Aux Id: ERL-208

- Remove generation of atoms in old latin1 external format in the distribution
  between erlang nodes, `erl_interface`, and `jinterface`. The new utf8 format
  for atoms was introduced in OTP R16. An OTP 20 node can therefore not connect
  to nodes older than R16.

  Atoms that can be encoded using latin1 are still encoded by `term_to_binary()`
  using latin1 encoding. Note that all atoms will by default be encoded using
  utf8 in a future Erlang/OTP release. For more information see the
  documentation of `erlang:term_to_binary/2`.

  Own Id: OTP-14337

- Added function `re:version/0` which returns information about the OTP internal
  PCRE version used for implementation of the `re` module.

  Own Id: OTP-14347 Aux Id: PR-1412

- Added new debug bif `erlang:list_to_port/1`.

  Own Id: OTP-14348

- Various improvements of timer management internally in the VM. These
  improvements both reduced memory consumption of timer wheels as well as reduce
  the amount of work that has to be performed in order to handle timers.

  Own Id: OTP-14356

- Sockets can now be bound to device (SO_BINDTODEVICE) on platforms where it is
  supported.

  This has been implemented e.g to support VRF-Lite under Linux; see
  [VRF ](https://www.kernel.org/doc/Documentation/networking/vrf.txt), and
  GitHub pull request [\#1326](https://github.com/erlang/otp/pull/1326).

  Own Id: OTP-14357 Aux Id: PR-1326

- Added the following [`erl`](erl_cmd.md) command line arguments with which you
  can set suggested stack for dirty schedulers:

  - **[`+sssdcpu`](erl_cmd.md#dcpu_sched_thread_stack_size)** - for dirty CPU
    schedulers

  - **[`+sssdio`](erl_cmd.md#dio_sched_thread_stack_size)** - for dirty IO
    schedulers

  The default suggested stack size for dirty schedulers is 40 kilo words.

  Own Id: OTP-14380

- Changed erts startup program name, argv 0, to use the environment variable
  `ESCRIPT_NAME` so that `erlc`, `dialyzer`, `typer`, `ct_run`, or the escript
  name can be seen with external programs, such as ps and htop (depending on
  options), on unix.

  Own Id: OTP-14381

- Improvements of `escript` documentation.

  Own Id: OTP-14384 Aux Id: OTP-14201

- Add function `enif_hash` for NIFs to calculate hash values of arbitrary terms.

  Own Id: OTP-14385 Aux Id: PR-1413

- '`./configure --enable-lock-counter`' will enabling building of an additional
  emulator that has support for lock counting. (The option previously existed,
  but would turn on lock counting in the default emulator being built.) To start
  the lock-counting emulator, use '`erl -emu_type lcnt`'.

  On Windows, `erl` recognized the undocumented option `-debug` for starting a
  debug-compiled emulator. That option has been removed. Use
  '`erl -emu_type debug`' instead.

  Own Id: OTP-14407

- Warnings have been added to the relevant documentation about not using
  un-secure distributed nodes in exposed environments.

  Own Id: OTP-14425

- Improvement of the documentation of the environment variable
  `ERL_CRASH_DUMP_SECONDS` as well as the default behavior when it is not set.

  Own Id: OTP-14434

- Enabled off-heap message queue for some system processes that might receive
  large amounts of messages.

  Own Id: OTP-14438

- ETS lock indexes have been replaced with the table name in LCNT results.

  Own Id: OTP-14442 Aux Id: ERIERL-22

- Introduced the new functions
  [`enif_whereis_pid()`](erl_nif.md#enif_whereis_pid) and
  [`enif_whereis_port()`](erl_nif.md#enif_whereis_port).

  Own Id: OTP-14453 Aux Id: PR-1400

## Erts 8.3.5.7

### Fixed Bugs and Malfunctions

- Fixed bug in operator `band` of two negative operands causing erroneous result
  if the absolute value of one of the operands have the lowest `N*W` bits as
  zero and the other absolute value is not larger than `N*W` bits. `N` is an
  integer of 1 or larger and `W` is 32 or 64 depending on word size.

  Own Id: OTP-15487 Aux Id: ERL-804

### Improvements and New Features

- Added an optional `./configure` flag to compile the emulator with spectre
  mitigation: `--with-spectre-mitigation`

  Note that this requires a recent version of GCC with support for spectre
  mitigation and the `--mindirect-branch=thunk` flag, such as `8.1`.

  Own Id: OTP-15430 Aux Id: ERIERL-237

## Erts 8.3.5.6

### Fixed Bugs and Malfunctions

- Fixed small memory leak that could occur when sending to a terminating port.

  Own Id: OTP-14609 Aux Id: ERIERL-238

## Erts 8.3.5.5

### Fixed Bugs and Malfunctions

- Fixed a race condition in the inet driver that could cause receive to hang
  when the emulator was compiled with gcc 8.

  Own Id: OTP-15158 Aux Id: ERL-654

- Fixed a bug causing some Erlang references to be inconsistently ordered. This
  could for example cause failure to look up certain elements with references as
  keys in search data structures. This bug was introduced in R13B02.

  Thanks to Simon Cornish for finding the bug and supplying a fix.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15225

## Erts 8.3.5.4

### Fixed Bugs and Malfunctions

- Fixed a bug in file closure on Unix; close(2) was retried on EINTR which could
  cause a different (recently opened) file to be closed as well.

  Own Id: OTP-14775

- A race-condition when tearing down a connection with active node monitors
  could cause the runtime system to crash.

  This bug was introduced in ERTS version 8.0 (OTP 19.0).

  Own Id: OTP-14781 Aux Id: OTP-13047

## Erts 8.3.5.3

### Fixed Bugs and Malfunctions

- A timer internal bit-field used for storing scheduler id was too small. As a
  result, VM internal timer data structures could become inconsistent when using
  1024 schedulers on the system. Note that systems with less than 1024
  schedulers are not effected by this bug.

  This bug was introduced in ERTS version 7.0 (OTP 18.0).

  Own Id: OTP-14548 Aux Id: OTP-11997, ERL-468

- Fixed bug in `binary_to_term` and `binary_to_atom` that could cause VM crash.
  Typically happens when the last character of an UTF8 string is in the range
  128 to 255, but truncated to only one byte. Bug exists in `binary_to_term`
  since ERTS version 5.10.2 (OTP_R16B01) and `binary_to_atom` since ERTS version
  9.0 (OTP-20.0).

  Own Id: OTP-14590 Aux Id: ERL-474

- Fix bug causing VM crash when a module with `-on_load` directive is loaded
  while `erlang:trace(on_load, ...)` is enabled.

  Own Id: OTP-14612

- Fixed bug that could cause a VM crash when a corrupt message is received on
  distribution channel from other node.

  Own Id: OTP-14661 Aux Id: ERIERL-80

## Erts 8.3.5.2

### Fixed Bugs and Malfunctions

- Fix performance bug in pre-allocators that could cause them to permanently
  fall back on normal more expensive memory allocation. Pre-allocators are used
  for quick allocation of short lived meta data used by messages and other
  scheduled tasks. Bug exists since OTP_R15B02.

  Own Id: OTP-14491

- Fixed a bug that prevented TCP sockets from being closed properly on send
  timeouts.

  Own Id: OTP-14509

- Fixed bug in operator `bxor` causing erroneuos result when one operand is a
  big _negative_ integer with the lowest `N*W` bits as zero and the other
  operand not larger than `N*W` bits. `N` is an integer of 1 or larger and `W`
  is 32 or 64 depending on word size.

  Own Id: OTP-14514

## Erts 8.3.5.1

### Fixed Bugs and Malfunctions

- Fixed a bug in gen_tcp:send where it never returned when repeatedly called on
  a remotely closed TCP socket.

  Own Id: OTP-13939 Aux Id: ERL-193

- Fixed segfault that could happen during cleanup of aborted
  erlang:port_command/3 calls. A port_command is aborted if the port is closed
  at the same time as the port_command was issued. This bug was introduced in
  erts-8.0.

  Own Id: OTP-14481

- Fixed implementation of [`statistics(wall_clock)`](`statistics/1`) and
  [`statistics(runtime)`](`statistics/1`) so that values do not unnecessarily
  wrap due to the emulator. Note that the values returned by
  [`statistics(runtime)`](`statistics/1`) may still wrap due to limitations in
  the underlying functionality provided by the operating system.

  Own Id: OTP-14484

## Erts 8.3.5

### Fixed Bugs and Malfunctions

- Active-mode TCP sockets are now cleaned up properly on send/shutdown errors.

  Own Id: OTP-14441 Aux Id: ERL-430

- A code purge operation could under certain circumstances expand the size of
  hibernated processes.

  Own Id: OTP-14444 Aux Id: ERIERL-24

- Fix so that the ERL_ZZ_SIGTERM_KILL introduced in erts-8.3.4 works.

  Own Id: OTP-14451

## Erts 8.3.4

### Fixed Bugs and Malfunctions

- Add option to make SIGTERM trigger the OS default behaviour instead of doing a
  graceful shutdown. To activate this bahviour the environment variable
  ERL_ZZ_SIGTERM_KILL should be set to "true". This option only works in OTP 19
  as OTP 20 will have a different way to deal with SIGTERM.

  Own Id: OTP-14418 Aux Id: ERIERL-15

## Erts 8.3.3

### Fixed Bugs and Malfunctions

- Fixed memory segment cache used for multiblock carriers. Huge (> 2GB) memory
  segments could cause a VM crash. Creation of such huge memory segments used
  for multiblock carriers is however very uncommon.

  Own Id: OTP-14360 Aux Id: ERL-401, PR-1417

- Fix release note for OTP-14290 in ERTS version 8.3.1. It was erroneously
  placed under "Known Bugs and Problems".

  Own Id: OTP-14363 Aux Id: OTP-14290

## Erts 8.3.2

### Fixed Bugs and Malfunctions

- The `+Bi` command line argument of `erl` erroneously caused `SIGTERM` to be
  ignored by the VM as well as of all its child processes. This bug was
  introduced in erts version 8.3.

  Own Id: OTP-14358 Aux Id: OTP-14085

## Erts 8.3.1

### Fixed Bugs and Malfunctions

- Invoking `init:stop/0` via the SIGTERM signal, in a non-SMP BEAM, could cause
  BEAM to terminate with fatal error. This has now been fixed and the BEAM will
  terminate normally when SIGTERM is received.

  Own Id: OTP-14290

- Trying to open a directory with file:read_file/1 on Unix leaked a file
  descriptor. This bug has now been fixed.

  Own Id: OTP-14308 Aux Id: ERL-383

## Erts 8.3

### Fixed Bugs and Malfunctions

- Fixed a number of bugs that caused faulty stack-traces to be generated. The
  faulty stack traces were generated either when applying the following
  functions or tracing the following functions:

  - `erlang:error/1`
  - `erlang:error/2`
  - `erlang:exit/1`
  - `erlang:throw/1`

  Own Id: OTP-14055

- Corrected documentation about memory footprint for maps.

  Own Id: OTP-14118

- Fix [`process_info(Pid, current_stacktrace)`](`process_info/2`) to use stack
  depth limit set by `system_flag(backtrace_depth)`. The old behavior was a hard
  coded depth limit of 8.

  Own Id: OTP-14119 Aux Id: PR-1263

- A process calling
  [`erlang:system_flag(multi_scheduling, block)`](`m:erlang#system_flag_multi_scheduling`)
  could end up hanging forever in the call.

  Own Id: OTP-14121

- Dirty scheduler bug fixes:

  - Fixed call time tracing of process being scheduled on dirty scheduler.
  - GC info from dirty schedulers.
  - Multi scheduling block with dirty schedulers could crash the runtime system.
  - Process structures could be removed prematurely.
  - GC on dirty scheduler could crash the runtime system.
  - Termination of a process executing on a dirty scheduler could cause a
    runtime system crash.

  Own Id: OTP-14122

- Fixed crash that occurred when writing timer data to a crash dump.

  Own Id: OTP-14133

- A literal area could be removed while still referred from processes.

  Own Id: OTP-14134

- Fixed a bug in the garbage collector that could crash the runtime system.

  Own Id: OTP-14135

- Fixed a bug in call-time trace for NIFs which caused tracing to erroneously be
  started multiple times for one call.

  Own Id: OTP-14136

- Remove a debug printout and an unnecessary garbage collection when handling
  exceptions in hipe compiled code.

  Own Id: OTP-14153

- Fix bug in tracing of garbage collection that could cause VM crash. Bug exists
  since OTP 19.0.

  Own Id: OTP-14154

- Fix bug in `binary_to_term` for binaries created by `term_to_binary `with
  option `compressed`. The bug can cause `badarg` exception for a valid binary
  when Erlang VM is linked against a `zlib` library of version 1.2.9 or newer.
  Bug exists since OTP 17.0.

  Own Id: OTP-14159 Aux Id: ERL-340

- Fix suspension of schedulers when generating a crashdump.

  Own Id: OTP-14164

- NIF resources was not handled in a thread-safe manner in the runtime system
  without SMP support.

  As a consequence of this fix, the following driver functions are now
  thread-safe also in the runtime system without SMP support:

  - `driver_free_binary()`
  - `driver_realloc_binary()`
  - `driver_binary_get_refc()`
  - `driver_binary_inc_refc()`
  - `driver_binary_dec_refc()`

  Own Id: OTP-14202

- Fix `erlang:round/1` for large floating point numbers with an odd absolute
  value between `(1 bsl 52)` and `(1 bsl 53)`. The result was falsely calculated
  as the next higher even number even though all integer values up to
  `(1 bsl 53)` can be represented as floats with full precision.

  Own Id: OTP-14227

- Add size of literals to module code size in crash dump and `(l)oaded` command
  in break menu like it used to be before OTP-19.0.

  Own Id: OTP-14228

- Fix potential bug in `enif_send` when called without a process context and
  with argument `msg_env` as `NULL`.

  Own Id: OTP-14229

- Fix bug where passing an appendable binary to `erlang:port_control()` could
  crash the emulator.

  Own Id: OTP-14231

- Receive expressions with timeout in the Erlang shell could cause a VM crash.

  Own Id: OTP-14241 Aux Id: ERL-365

### Improvements and New Features

- A received SIGTERM signal to beam will generate a `'stop'` message to the
  `init` process and terminate the Erlang VM nicely. This is equivalent to
  calling `init:stop/0`.

  Own Id: OTP-14085

- Workaround for buggy Android implementation of `PTHREAD_STACK_MIN` causing
  build of runtime system to crash on undeclared `PAGE_SIZE`.

  Own Id: OTP-14165 Aux Id: ERL-319

- Add configure option --without-thread-names that removes the naming of
  individual emulator threads.

  Own Id: OTP-14234

- Add warning in documentation of `zlib:deflateInit/6` about option
  `WindowsBits` values 8 and -8.

  Own Id: OTP-14254 Aux Id: ERL-362

## Erts 8.2.2

### Fixed Bugs and Malfunctions

- Fix bug in `binary_to_term` for binaries created by `term_to_binary `with
  option `compressed`. The bug can cause `badarg` exception for a valid binary
  when Erlang VM is linked against a `zlib` library of version 1.2.9 or newer.
  Bug exists since OTP 17.0.

  Own Id: OTP-14159 Aux Id: ERL-340

- The driver efile_drv when opening a file now use fstat() on the open file
  instead of stat() before opening, if fstat() exists. This avoids a race when
  the file happens to change between stat() and open().

  Own Id: OTP-14184 Aux Id: seq-13266

## Erts 8.2.1

### Fixed Bugs and Malfunctions

- Fix a quite rare bug causing VM crash during code loading and the use of
  export funs (fun M:F/A) of not yet loaded modules. Requires a very specific
  timing of concurrent scheduler threads. Has been seen on ARM but can probably
  also occurred on other architectures. Bug has existed since OTP R16.

  Own Id: OTP-14144 Aux Id: seq13242

## Erts 8.2

### Fixed Bugs and Malfunctions

- Fixed `configure` failures on MacOSX. Most important `clock_gettime()` was
  detected when building for MacOSX - El Capitan using XCode 8 despite it is not
  available until MacOSX - Sierra.

  Own Id: OTP-13904 Aux Id: ERL-256

- `code:add_pathsa/1` and command line option `-pa` both revert the given list
  of directories when adding it at the beginning of the code path. This is now
  documented.

  Own Id: OTP-13920 Aux Id: ERL-267

- Fix a compilation error of erts in OpenBSD related to the usage of the
  \_\_errno variable.

  Own Id: OTP-13927

- Fixed so that when enabling tracing on a process that had an invalid tracer
  associated with it, the new tracer overwrites the old tracer. Before this fix,
  calling erlang:trace/3 would behave as if the tracer was still alive and not
  apply the new trace.

  This fault was introduced in ERTS 8.0.

  Own Id: OTP-13928

- Fix parsing of `-profile_boot 'true' | 'false'`

  Own Id: OTP-13955 Aux Id: ERL-280

- A slight improvement of `erlang:get_stacktrace/0` for exceptions raised in
  hipe compiled code. Beam compiled functions in such stack trace was earlier
  replaced by some unrelated function. They are now instead omitted. This is an
  attempt to reduce the confusion in the absence of a complete and correct stack
  trace for mixed beam and hipe functions.

  Own Id: OTP-13992

- Correct type declaration of match specification head.

  Own Id: OTP-13996

- HiPE code loading failed for x86_64 if gcc was configured with
  `--enable-default-pie`. Fixed by disabling PIE, if needed for HiPE, when
  building the VM.

  Own Id: OTP-14031 Aux Id: ERL-294, PR-1239

- Faulty arguments could be presented on exception from a NIF that had
  rescheduled itself using `enif_schedule_nif()`.

  Own Id: OTP-14048

- The runtime system could crash if a garbage collection on a process was
  performed immediately after a NIF had been rescheduled using
  `enif_schedule_nif()`.

  Own Id: OTP-14049

- A reference to purged code could be left undetected by the purge operation if
  a process just had rescheduled a NIF call using `enif_schedule_nif()` when the
  process was checked. This could cause a runtime system crash.

  Own Id: OTP-14050

- Fixed a number of dirty scheduler related bugs:

  - Process priority was not handled correct when scheduling on a dirty
    scheduler.
  - The runtime system could crash when an exit signal with a compound exit
    reason was sent to a process executing on a dirty scheduler.
  - The runtime system crashed when call tracing a process executing on a dirty
    scheduler.
  - A code purge operation could end up hanging forever when a process executed
    on a dirty scheduler

  Own Id: OTP-14051

### Improvements and New Features

- Fix minor soft purge race bug that could incorrectly trigger code_server to
  load new code for the module if the soft purge failed and no current version
  of the module existed.

  Own Id: OTP-13925

- To ease troubleshooting, `erlang:load_nif/2` now includes the return value
  from a failed call to load/reload/upgrade in the text part of the error tuple.
  The `crypto` NIF makes use of this feature by returning the source line
  where/if the initialization fails.

  Own Id: OTP-13951

- New environment variable `ERL_CRASH_DUMP_BYTES` can be used to limit the size
  of crash dumps. If the limit is reached, crash dump generation is aborted and
  the generated file will be truncated.

  Own Id: OTP-14046

## Erts 8.1.1.1

### Fixed Bugs and Malfunctions

- A code purge operation could under certain circumstances expand the size of
  hibernated processes.

  Own Id: OTP-14444 Aux Id: ERIERL-24

## Erts 8.1.1

### Fixed Bugs and Malfunctions

- The emulator got a dynamic library dependency towards libsctp, which on Linux
  was not intended since the emulator there loads and resolves the needed sctp
  functions in runtime. This has been fixed and a configure switch
  --enable-sctp=lib has been added for those who want such a library dependency.

  Own Id: OTP-13956 Aux Id: ERL-262, ERL-133

- Fix SIGUSR1 crashdump generation

  Do not generate a core when a crashdump is asked for.

  Own Id: OTP-13997

- The new functions in `code` that allows loading of many modules at once had a
  performance problem. While executing a helper function in the
  `erl_prim_loader` process, garbage messages were produced. The garbages
  messages were ignored and ultimately discarded, but there would be a negative
  impact on performance and memory usage. The number of garbage message depended
  on both the number of modules to be loaded and the length of the code path.

  The functions affected of this problem were: `atomic_load/1`,
  `ensure_modules_loaded/1`, and `prepare_loading/1`.

  Own Id: OTP-14009

## Erts 8.1

### Fixed Bugs and Malfunctions

- Fix bug for calls from hipe code to BIFs that disable GC while yielding. Has
  been causing Dialyzer crashes on ARM (and presumably all other non-intel
  platforms).

  Own Id: OTP-13724 Aux Id: PR-1116

- Fix a bug where changing the current working directory of the VM would not
  change the current working directory of programs spawned using
  `erlang:open_port({spawn,""}, ...)`.

  Own Id: OTP-13733 Aux Id: ERL-175

- Fix a bug where disabling tracing from a process that had return_to tracing
  enabled and was tracing on `erlang:trace/3` would cause a segmentation fault.

  Own Id: OTP-13734

- Update all erts documentation to use simpler English, use consistent
  terminology and be easier to navigate.

  Own Id: OTP-13740

- Add dirty schedulers to the microstate accounting statistics.

  Own Id: OTP-13744

- Fixed dirty scheduler build support on 32-bit windows.

  Own Id: OTP-13759

- inet:getstat(Socket) on an SCTP socket returned 0 for send stats. This bug has
  now been corrected. Reported by systra as issue ERL-102 on bugs.erlang.org.

  Own Id: OTP-13773 Aux Id: ERL-102

- AF_UNSPEC and unknown address families were misread by UDP receive in
  prim_inet resulting in an exception. This bug has now been corrected.

  Own Id: OTP-13775

- Sweep HiPE stack for literals during code purge.

  Own Id: OTP-13777 Aux Id: PR-1122

- Fix bug in run_erl for OpenBSD that could cause it on rare occations to exit
  without starting the program (erl) at all.

  Own Id: OTP-13795

- Update build scripts to not make assumptions about where env, cp and perl are
  located.

  Own Id: OTP-13800

- Fixed a bug where init:stop could deadlock if a process with infinite shutdown
  timeout (e.g. a supervisor) attempted to load code while terminating.

  Own Id: OTP-13802

- Fixed a segmentation fault on sparc CPUs when free'ing a tracer module's
  state.

  Own Id: OTP-13803

- `fun`s was not properly handled during purge of a module. This could cause a
  crash of the VM after a purge of a module.

  Own Id: OTP-13809

- Fixed a memory leak when the process monitoring a port crashed.

  Own Id: OTP-13818

- Fixed multiple dirty scheduler related tracing bugs.

  Own Id: OTP-13822

- Fix error handling in beam code runtime loader for a number of cases when
  index and size fields got corrupted (negative) values.

  Own Id: OTP-13848 Aux Id: ERL-216

- Minor fix of dirty scheduler implementation.

  Own Id: OTP-13852

- Calls to `erl_drv_send_term()` or `erl_drv_output_term()` from a non-scheduler
  thread while the corresponding port was invalid caused the emulator to enter
  an inconsistent state which eventually caused an emulator crash.

  Own Id: OTP-13866

- Fix a rare race condition in `erlang:open_port({spawn, ""}, ...)` that would
  result in the erl_child_setup program aborting and cause the emulator to exit.

  Own Id: OTP-13868

- Driver and NIF operations accessing processes or ports could cause an emulator
  crash when used from non-scheduler threads. Those operations are:

  - `erl_drv_send_term()`
  - `driver_send_term()`
  - `erl_drv_output_term()`
  - `driver_output_term()`
  - `enif_send()`
  - `enif_port_command()`

  Own Id: OTP-13869

- Fix start scripts generation dependency in Makefile

  Own Id: OTP-13871 Aux Id: ERL-241

- The VM could crash if `erlang:get_stacktrace()` was called after a rescheduled
  NIF had thrown an exception.

  Own Id: OTP-13877

- Calling `code:delete/1` before a loading a module with an on_load function,
  the old code would be overwritten, causing a memory or a crash if NIFs were
  involved. (Thanks to vans163 for reporting this bug.)

  Own Id: OTP-13893 Aux Id: ERL-240

### Improvements and New Features

- Improved accuracy of timeouts on MacOS X. This by setting premature timeouts
  followed by a short actual timeout during scheduler wait.

  Own Id: OTP-13698

- Added the following symbolic time unit representations to the
  `t:erlang:time_unit/0` type:

  - `second`
  - `millisecond`
  - `microsecond`
  - `nanosecond`

  The following symbolic time unit representations are now _deprecated_, but
  still part of the [`erlang:time_unit()`](`t:erlang:time_unit/0`) type:

  - `seconds`
  - `milli_seconds`
  - `micro_seconds`
  - `nano_seconds`

  Own Id: OTP-13735

- Fix maps hashing entropy of maps with maps keys.

  Own Id: OTP-13763 Aux Id: ERL-199

- Improved dirty scheduler support. A purge of a module can now be performed
  without having to wait for completion of all ongoing dirty NIF calls.

  Note that when enabling support for dirty schedulers, a new purge strategy
  will as of ERTS version 8.1 be enabled. This new strategy is not fully
  backwards compatible with the strategy used by default. For more information
  see the documentation of `erlang:check_process_code/3`.

  Own Id: OTP-13808 Aux Id: OTP-13833

- A new purge strategy has been introduced. The new strategy will by default be
  disabled during the OTP 19 release, but will be the only strategy available as
  of the OTP 20 release.

  The new strategy is slightly incompatible with the strategy being used by
  default in OTP 19. Using the default strategy, processes holding `fun`s that
  refer to the module being purged either fail a soft purge, or will be killed
  during a hard purge. The new strategy completely ignores `fun`s. If `fun`s
  referring to the code being purged exist, and are used after a purge, an
  exception will be raised upon usage. That is, the behavior will be exactly the
  same as the case when a `fun` is received by a process after the purge.

  The new strategy can optionally be enabled when building OTP during OTP 19,
  and will automatically be enabled if the runtime system is built with support
  for dirty schedulers.

  For more information see the documentation of `erlang:check_process_code/3`.

  Own Id: OTP-13833

- Fixed unnecessary overestimation of heap size need during garbage collection.

  Own Id: OTP-13851

## Erts 8.0.5

### Fixed Bugs and Malfunctions

- Fixed a VM crash that occurred in a garbage collection of a process when it
  had received binaries. This bug was introduced in ERTS version 8.0 (OTP 19.0).

  Own Id: OTP-13890

## Erts 8.0.4

### Fixed Bugs and Malfunctions

- Fixed a VM crash that occurred in garbage collection of a process when it had
  received maps over the distribution. This bug was introduced in ERTS version
  8.0 (OTP 19.0).

  Own Id: OTP-13889

## Erts 8.0.3

### Fixed Bugs and Malfunctions

- Fixed a race that could cause a lost wakeup of a process that timed out in a
  `receive ... after`. This bug was introduced in ERTS version 7.0.

  Own Id: OTP-13798 Aux Id: OTP-11997

- Fixed segfault after writing an erl crash dump.

  Own Id: OTP-13799

## Erts 8.0.2

### Fixed Bugs and Malfunctions

- Fix scheduler deadlock bug in `ets:update_counter/4` when key is not found and
  inserting the default object causes the table to grow.

  Own Id: OTP-13731 Aux Id: ERL-188

- Fix VM abort "Overrun stack and heap" in garbage collection triggered by a
  `bsl` operation and some very specific heap conditions.

  Own Id: OTP-13732 Aux Id: seq13142

## Erts 8.0.1

### Fixed Bugs and Malfunctions

- A memory allocation bug in [`group_leader/2`](`group_leader/2`) could cause an
  emulator crash when garbage collecting a process that had been assigned a
  remote group leader. This bug was introduced in ERTS version 8.0.

  Own Id: OTP-13716

## Erts 8.0

### Fixed Bugs and Malfunctions

- The handling of `on_load` functions has been improved. The major improvement
  is that if a code upgrade fails because the `on_load` function fails, the
  previous version of the module will now be retained.

  Own Id: OTP-12593

- `is_builtin(erlang, apply, 3)` will now return `true`.

  Own Id: OTP-13034

- Fix `enif_get_list_length` to return false if list is improper or have length
  larger than `UINT_MAX` (did return true and an incorrect length value).

  Own Id: OTP-13288 Aux Id: PR913

- Cleanup hipe signal handling code for x86 and make it more portable.

  Own Id: OTP-13341 Aux Id: PR951

- Make file:datasync use fsync instead of fdatasync on Mac OSX.

  Own Id: OTP-13411

- Make sure to create a crash dump when running out of memory. This was
  accidentally removed in the erts-7.3 release.

  Own Id: OTP-13419

- A bug has been fixed where if erlang was started +B on a unix platform it
  would be killed by a SIGUSR2 signal when creating a crash dump.

  Own Id: OTP-13425

- Fix race between [`process_flag(trap_exit,true)`](`process_flag/2`) and a
  received exit signal.

  A process could terminate due to exit signal even though
  [`process_flag(trap_exit,true)`](`process_flag/2`) had returned. A very
  specific timing between call to [`process_flag/2`](`process_flag/2`) and exit
  signal from another scheduler was required for this to happen.

  Own Id: OTP-13452

- Don't search for non-existing Map keys twice

  For `maps:get/2,3` and `maps:find/2`, searching for an immediate key, e.g. an
  atom, in a small map, the search was performed twice if the key did not exist.

  Own Id: OTP-13459

- When an abnormally large distribution message is about to be sent, the VM has
  been changed to create a crash dump instead of a core dump.

  Own Id: OTP-13474

- Fix `erlang:process_info/2` type specification

  Own Id: OTP-13485 Aux Id: ERL-123

- Fix bug in [`open_port/2`](`open_port/2`) with option `{args, List}`. A vm
  crash could be caused by an improper `List`.

  Own Id: OTP-13489 Aux Id: ERL-127

- Fixed a race-condition bug where the emulator could crash when
  `erlang:system_profile/1,2` was enabled and a process had to be re-scheduled
  during termination.

  Own Id: OTP-13494 Aux Id: ERL-126

- Fixed bugs where the reduction counter was not handled correct.

  Own Id: OTP-13512

- Fixed typo in description of the `EPMD_DUMP_REQ` response.

  Own Id: OTP-13517

- Fixed a bug where a process flagged as sensitive would sometimes record its
  save_calls when it shouldn't.

  Own Id: OTP-13540

- Update configure scripts to not use hard-coded path for /bin/pwd and /bin/rm.

  Own Id: OTP-13562

- When passing a larger binary than the outputv callback of a linked-in driver
  can handle in one io vector slot, the binary is now split into multiple slots
  in the io vector. This change only effects system where the max size of an io
  vector slot is smaller then the word size of the system (e.g. Windows).

  This change means that it is now possible on Windows to send binaries that are
  larger than 4GB to port_command, which is what is used for file:write,
  gen_tcp:send etc.

  Own Id: OTP-13628

- Workaround of Maps output in crashdumps. Currently the atom 'undefined' is
  generated instead of Map data if a Map type is encountered during crash.

  Own Id: OTP-13657

### Improvements and New Features

- The tracing support has been extended to allow a
  [tracer module](`m:erl_tracer`) to be the trace event handler instead of a
  process or port. The [tracer module](`m:erl_tracer`) makes it possible for
  trace tools to filter or manipulate trace event data without the trace event
  first having to be copied from the traced process or port.

  With the introduction of this feature, `erlang:trace(all|existing, _, _)` now
  also returns the tracer process as part of the number of processes on which
  tracing is enabled. The is incompatible with the previous releases.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-10267

- Introduce LTTng tracing of Erlang Runtime System

  For LTTng to be enabled OTP needs to be built with configure option
  `--with-dynamic-trace=lttng`.

  This feature introduces tracepoints for schedulers, drivers, memory carriers,
  memory and async thread pool.

  For a list of all tracepoints, see
  [Runtime Tools User's Guide](`e:runtime_tools:lttng.md`) .

  Own Id: OTP-10282

- Make it possible to monitor/demonitor ports using the `erlang:monitor/2` API.
  The process and port information functions have also been updated to include
  information about monitors from processes to ports.

  Own Id: OTP-11384

- Add microstate accounting

  Microstate accounting is a way to track which state the different threads
  within ERTS are in. The main usage area is to pin point performance
  bottlenecks by checking which states the threads are in and then from there
  figuring out why and where to optimize.

  Since checking whether microstate accounting is on or off is relatively
  expensive only a few of the states are enabled by default and more states can
  be enabled through configure.

  There is a convenience module called msacc that has been added to
  runtime_tools that can assist in gathering and interpreting the data from
  Microstate accounting.

  For more information see
  [erlang:statistics(microstate*accounting, *)](`m:erlang#statistics_microstate_accounting`)
  and the `m:msacc` module in runtime_tools.

  Own Id: OTP-12345

- The port of Erlang/OTP to the real-time operating system OSE has been removed.

  Own Id: OTP-12573

- Sharing preserved copy for messages and exit signals

  Enable sharing preserved copy with configure option
  `--enable-sharing-preserving`. This will preserve sharing, within the process,
  when communication with other processes in the Erlang node. There is a
  trade-off, the copy is more costly but this cost can be reclaimed if there is
  a lot of sharing in the message. In addition literals will not be copied in a
  send except during a purge phase of the module where the literals are located.
  This feature is considered experimental in 19.0.

  Own Id: OTP-12590 Aux Id: OTP-10251

- Halfword BEAM has been removed.

  Own Id: OTP-12883

- Added `os:perf_counter/1`.

  The perf_counter is a very very cheap and high resolution timer that can be
  used to timestamp system events. It does not have monoticity guarantees, but
  should on most OS's expose a monotonous time.

  Own Id: OTP-12908

- Support for a fragmented young heap generation. That is, the young heap
  generation can consist of multiple non continuous memory areas. The main
  reason for this change is to avoid extra copying of messages that could not be
  allocated directly on the receivers heap.

  Own Id: OTP-13047

- Erlang linked-in driver can now force the call to open_port to block until a
  call to erl_drv_init_ack is made inside the driver. This is useful when you
  want to do some asynchronous initialization, for example getting configuration
  from a pipe, and you want the initial open_port call to fail if the
  configuration is incomplete or wrong. See the erl_driver documentation for
  more details on the API.

  Own Id: OTP-13086

- Erlang linked-in drivers can now set their own pids as seen in
  `erlang:port_info/1` by using the `erl_drv_set_pid` function. For more details
  see the erl_driver documentation.

  Own Id: OTP-13087

- The functionality behind `erlang:open_port/2` when called with spawn or
  spawn_executable has been redone so that the forking of the new program is
  done in a separate process called erl_child_setup. This allows for a much more
  robust implementation that uses less memory and does not block the entire
  emulator if the program to be started is on an un-accessible NFS. Benchmarks
  have shown this approach to be about 3-5 times as fast as the old approach
  where the fork/vfork was done by erts. This is a pure stability and
  performance fix, however some error messages may have changed, which is why it
  is marked as a backwards incompatible change.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13088

- Improved yielding strategy in the implementation of the following native
  functions:

  - `erlang:binary_to_list/1`
  - `erlang:binary_to_list/3`
  - `erlang:bitstring_to_list/1`
  - `erlang:list_to_binary/1`
  - `erlang:iolist_to_binary/1`
  - `erlang:list_to_bitstring/1`
  - `binary:list_to_bin/1`

  This in order to improve performance of these functions.

  Own Id: OTP-13096

- All garbage collections of processes now bump reductions. Also the amount of
  reductions bumped when garbage collecting has been adjusted. It now better
  corresponds to the amount of work performed. This in order to improve the real
  time characteristics of the system.

  Own Id: OTP-13097

- New functions that can load multiple modules at once have been added to the
  '`code`' module. The functions are `code:atomic_load/1`,
  `code:prepare_loading/1`, `code:finish_loading/1`, and
  `code:ensure_modules_loaded/1`.

  Own Id: OTP-13111

- The `-boot_var` option for `erl` now only supports a single key and single
  value (as documented). The option used to allow multiple key/value pairs, but
  that behavior was undocumented.

  The function `erl_prim_loader:start/3` has been removed. Its documentation has
  also been removed.

  The undocumented and unsupported function `erl_prim_loader:get_files/2` has
  been removed.

  Own Id: OTP-13112

- Low level BIF `erlang:purge_module/1` is made more robust against incorrect
  use. Lingering processes that still refer the old code are now killed before
  the module is purged to prevent fatal VM behavior.

  Own Id: OTP-13122

- Improved dirty scheduler implementation. For more information see the
  [NIF documentation](erl_nif.md#dirty_nifs).

  > #### Note {: .info }
  >
  > - The dirty scheduler support is still _experimental_.
  > - The support for determining whether dirty NIF support exist or not at
  >   compile time using the C preprocessor macro
  >   `ERL_NIF_DIRTY_SCHEDULER_SUPPORT` has been removed.
  > - The `enif_is_on_dirty_scheduler()` function has been removed. Use
  >   [`enif_thread_type()`](erl_nif.md#enif_thread_type) instead.

  Own Id: OTP-13123

- Various optimizations done to process dictionary access.

  Own Id: OTP-13167

- Added max_heap_size process flag. max_heap_size allows the user to limit the
  maximum heap used by a process. See
  [erlang:process_flag](`erlang:process_flag/2`) for more details.

  Own Id: OTP-13174

- Allow dynamic drivers and NIF libraries to be built with gcc option
  `-fvisibility=hidden` for faster loading and more optimized code.

  Own Id: OTP-13227

- Add `erlang:process_info(Pid, garbage_collection_info)` which returns extended
  garbage_collection information. For more details see the documentation.

  Own Id: OTP-13265

- The functions `erlang:list_to_integer/1` and `string:to_integer/1` have been
  optimized for large inputs.

  Own Id: OTP-13293

- Improved memory allocation strategy for hipe native code on x86_64 (amd64)
  architectures by reserving enough low virtual address space needed for the
  HiPE/AMD64 small code model. The default virtual address area for hipe code is
  set to 512Mb, but can be changed with emulator flag `+MXscs`.

  Own Id: OTP-13359

- Introduction of configurable management of data referred to by the message
  queue of a process. Each process can be configured individually.

  It is now possible to configure the message queue of a process, so that all
  data referred by it will be kept outside of the heap, and by this prevent this
  data from being part of garbage collections.

  For more information see the documentation of
  [`process_flag(message_queue_data, MQD)`](`m:erlang#process_flag_message_queue_data`).

  Own Id: OTP-13366 Aux Id: OTP-13047

- Processes now yield when scanning large message queues and not finding a
  matching message. This in order to improve real time characteristics.

  Own Id: OTP-13401

- Optimized an erts internal function that is used to traverse erlang terms. The
  internal function was mainly used by term_to_binary and comparison of terms.
  Benchmarks have shown up to a 10% performance increase in those functions
  after the optimization.

  Own Id: OTP-13440

- Add the following NIF API functions:

  - [`enif_cpu_time`](erl_nif.md#enif_cpu_time)
  - [`enif_now_time`](erl_nif.md#enif_now_time)
  - [`enif_make_unique_integer`](erl_nif.md#enif_make_unique_integer)
  - [`enif_is_process_alive`](erl_nif.md#enif_is_process_alive)
  - [`enif_is_port_alive`](erl_nif.md#enif_is_port_alive)
  - [`enif_term_to_binary`](erl_nif.md#enif_term_to_binary)
  - [`enif_binary_to_term`](erl_nif.md#enif_binary_to_term)
  - [`enif_port_command`](erl_nif.md#enif_port_command)

  For details of what each function does, see the erl_nif documentation.

  Own Id: OTP-13442

- Optimize `'++'` operator and `lists:append/2` by using a single pass to build
  a new list while checking for properness.

  Own Id: OTP-13487

- Handle terms (pids,ports and refs) from nodes with a 'creation' value larger
  than 3. This is a preparation of the distribution protocol to allow OTP 19
  nodes to correctly communicate with future nodes (20 or higher). The
  'creation' value differentiates different incarnations of the same node
  (name).

  Own Id: OTP-13488

- Don't send unasked for systemd notifications in epmd

  Own Id: OTP-13493 Aux Id: PR-999

- The enif_send API has been extended to allow NULL to be used as the message
  environment. When used this way, a message environment is implicitly created
  and the given term is copied into that environment before sending. This can be
  an optimization if many small messages are being sent by the nif.

  Own Id: OTP-13495

- The tracing support has been extended to allow tracing on ports. Ports can be
  traced on using the 'ports', 'send' and 'receive' trace flags.

  The first argument of `erlang:trace/3` has been extended so that `'all'`,
  `'existing'` and `'new'` now include both processes and ports. New `Tracee`
  variants, `'all_processes'`, `'all_ports'`, `'existing_processes'` etc have
  been added to specify only processes or ports.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13496

- When the `'procs'` trace flag is enabled, a `'spawned'` trace event is now
  also generated by a newly created process. The previous event `'spawn'`
  remains, but as it is generated by the process that did the spawn, it is not
  guaranteed that it is ordered with other trace events from the newly spawned
  process. So when tracking the lifetime of a process this new event should be
  used as the creation event.

  This new trace event is marked as an incompatibility because tools that expect
  certain trace events when enabling 'procs' will have to updated.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13497

- Add the `erlang:match_spec_test/3` function. The functions allows the testing
  of match specifications for both tracing and ets tables. It can be used to
  test that a match specification does the expected filtering on specific data.
  It also returns more verbose error reasons for incorrectly constructed match
  specifications.

  Own Id: OTP-13501

- The erts internal tracing support has been changed to have much less overhead
  and be more scalable.

  This rewrite does not break any backwards incompatibilities, but it does
  change the ordering of some trace messages when compared to previous releases.
  It should be noted that this only applies to trace messages sent to processes
  or ports, it does not apply to the new tracer module. However in future
  releases they may also be effected by this.

  Trace messages are only guaranteed to be ordered from one traced process or
  port. In previous releases this was not visible as a `'send'` trace message
  would always arrive before the corresponding `'receive'` trace message that is
  no longer always the case. This also means that timestamped trace messages may
  seem to arrive out of order as the timestamp is taken when the event is
  triggered and not when it is put in the queue of the tracer.

  Own Id: OTP-13503

- Add possibility to filter `send` and `receive` trace with match
  specifications.

  Own Id: OTP-13507

- Add `maps:update_with/3,4` and `maps:take/2`

  Own Id: OTP-13522 Aux Id: PR-1025

- Introduce LTTng tracing via Erlang tracing.

  For LTTng to be enabled OTP needs to be built with configure option
  `--with-dynamic-trace=lttng`.

  The dynamic trace module `dyntrace` is now capable to be used as a LTTng sink
  for Erlang tracing. For a list of all tracepoints, see
  [Runtime Tools User's Guide](`e:runtime_tools:lttng.md`) .

  This feature also introduces an incompatible change in trace tags. The trace
  tags `gc_start` and `gc_end` has been split into `gc_minor_start`,
  `gc_minor_end` and `gc_major_start`, `gc_major_end`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13532

- Print heap pointers for garbing processes during crashdump

  Own Id: OTP-13541 Aux Id: PR-1026

- Changed and improved low level memory statistics returned by
  `erlang:system_info/1`. The info for `erts_mmap` has been moved from
  `mseg_alloc` to its own section returned by `{allocator, erts_mmap}`.

  Own Id: OTP-13560

- Add enif_snprintf to the NIF API

  The function `enif_snprintf` is similar to `snprintf` call but can handle
  formatting of Erlang terms via `%T` format specifier.

  Own Id: OTP-13580

- The warning in the documentation for `erlang:raise/3` has been removed. It is
  now officially perfectly fine to use raise/3 in production code.

  Own Id: OTP-13599

- Fix bugs caused by the VM sometimes truncating object sizes or offsets to 32
  bits on 64-bit hosts. These bugs were mainly found when working with large
  unicode strings and nifs environments.

  Own Id: OTP-13606

- Add `-start_epmd` command line option, this lets you disable automatic
  starting of epmd when starting a distributed node.

  Add `-epmd_module` command line option, this lets you specify a module to
  register and look-up node names in. The default module is `erl_epmd`.

  Own Id: OTP-13627

- `erlang:halt` now truncates strings longer than 200 characters instead of
  failing with `badarg`.

  Own Id: OTP-13630

- Fix possible race in poller wake up on windows

  Own Id: OTP-13634

## Erts 7.3.1.6

### Improvements and New Features

- List subtraction (The `--` operator) will now yield properly on large inputs.

  Own Id: OTP-15371

## Erts 7.3.1.5

### Fixed Bugs and Malfunctions

- Fixed small memory leak that could occur when sending to a terminating port.

  Own Id: OTP-14609 Aux Id: ERIERL-238

## Erts 7.3.1.4

### Fixed Bugs and Malfunctions

- Fix performance bug in pre-allocators that could cause them to permanently
  fall back on normal more expensive memory allocation. Pre-allocators are used
  for quick allocation of short lived meta data used by messages and other
  scheduled tasks. Bug exists since OTP_R15B02.

  Own Id: OTP-14491

- Fixed bug in operator `bxor` causing erroneuos result when one operand is a
  big _negative_ integer with the lowest `N*W` bits as zero and the other
  operand not larger than `N*W` bits. `N` is an integer of 1 or larger and `W`
  is 32 or 64 depending on word size.

  Own Id: OTP-14514

- A timer internal bit-field used for storing scheduler id was too small. As a
  result, VM internal timer data structures could become inconsistent when using
  1024 schedulers on the system. Note that systems with less than 1024
  schedulers are not effected by this bug.

  This bug was introduced in ERTS version 7.0 (OTP 18.0).

  Own Id: OTP-14548 Aux Id: OTP-11997, ERL-468

- Fixed bug in `binary_to_term` and `binary_to_atom` that could cause VM crash.
  Typically happens when the last character of an UTF8 string is in the range
  128 to 255, but truncated to only one byte. Bug exists in `binary_to_term`
  since ERTS version 5.10.2 (OTP_R16B01) and `binary_to_atom` since ERTS version
  9.0 (OTP-20.0).

  Own Id: OTP-14590 Aux Id: ERL-474

## Erts 7.3.1.3

### Fixed Bugs and Malfunctions

- A bug has been fixed where if erlang was started +B on a unix platform it
  would be killed by a SIGUSR2 signal when creating a crash dump.

  Own Id: OTP-13425 Aux Id: ERL-94

- Calls to `erl_drv_send_term()` or `erl_drv_output_term()` from a non-scheduler
  thread while the corresponding port was invalid caused the emulator to enter
  an inconsistent state which eventually caused an emulator crash.

  Own Id: OTP-13866

- Driver and NIF operations accessing processes or ports could cause an emulator
  crash when used from non-scheduler threads. Those operations are:

  - `erl_drv_send_term()`
  - `driver_send_term()`
  - `erl_drv_output_term()`
  - `driver_output_term()`
  - `enif_send()`
  - `enif_port_command()`

  Own Id: OTP-13869

- Fix bug in `binary_to_term` for binaries created by `term_to_binary `with
  option `compressed`. The bug can cause `badarg` exception for a valid binary
  when Erlang VM is linked against a `zlib` library of version 1.2.9 or newer.
  Bug exists since OTP 17.0.

  Own Id: OTP-14159 Aux Id: ERL-340

## Erts 7.3.1.2

### Fixed Bugs and Malfunctions

- Fixed a race that could cause a lost wakeup of a process that timed out in a
  `receive ... after`. This bug was introduced in ERTS version 7.0.

  Own Id: OTP-13798 Aux Id: OTP-11997

- Fixed segfault after writing an erl crash dump.

  Own Id: OTP-13799

## Erts 7.3.1.1

### Fixed Bugs and Malfunctions

- Fix scheduler deadlock bug in `ets:update_counter/4` when key is not found and
  inserting the default object causes the table to grow.

  Own Id: OTP-13731 Aux Id: ERL-188

## Erts 7.3.1

### Fixed Bugs and Malfunctions

- [`process_info(Pid, last_calls)`](`process_info/2`) did not work for
  `Pid /= self()`.

  Own Id: OTP-13418

- Make sure to create a crash dump when running out of memory. This was
  accidentally removed in the erts-7.3 release.

  Own Id: OTP-13419

- Schedulers could be woken by a premature timeout on Linux. This premature
  wakeup was however harmless.

  Own Id: OTP-13420

- A process communicating with a port via one of the `erlang:port_*` BIFs could
  potentially end up in an inconsistent state if the port terminated during the
  communication. When this occurred the process could later block in a `receive`
  even though it had messages matching in its message queue.

  This bug was introduced in erts version 5.10 (OTP R16A).

  Own Id: OTP-13424 Aux Id: OTP-10336

- The reference count of a process structure could under rare circumstances be
  erroneously managed. When this happened invalid memory accesses occurred.

  Own Id: OTP-13446

- Fix race between [`process_flag(trap_exit,true)`](`process_flag/2`) and a
  received exit signal.

  A process could terminate due to exit signal even though
  [`process_flag(trap_exit,true)`](`process_flag/2`) had returned. A very
  specific timing between call to [`process_flag/2`](`process_flag/2`) and exit
  signal from another scheduler was required for this to happen.

  Own Id: OTP-13452

## Erts 7.3

### Fixed Bugs and Malfunctions

- The '-path' flag to 'erl' has been documented. This flag replaces the path
  specified in the boot script. It has always existed, but was earlier only
  documented in SASL (script).

  Own Id: OTP-13060

- The `call_time` tracing functionality internally used a time based on OS
  system time in order to measure call time which could cause erroneous results
  if OS system time was changed during tracing.

  This functionality now use Erlang monotonic time in order to measure time.
  Besides fixing the erroneous results due to OS system time being used, the
  results are often also better since Erlang monotonic time often has better
  accuracy and precision.

  Own Id: OTP-13216

- Fix behaviour of -delay_write command line switch of epmd, which is used for
  debugging - in some cases epmd was sleeping twice the requested amount of
  time.

  Own Id: OTP-13220

- Fix race between timeout and exit signal that could cause a process to ignore
  the exit signal and continue execution. Bug exist since OTP 18.0.

  Own Id: OTP-13245

- Fix bug in `erlang:halt/1,2` for large exit status values, causing either
  `badarg` (on 32-bit) or exit with a crash dump and/or core dump (on 64-bit).
  Make `erlang:halt/1,2` tolerate any non negative integer as exit status and
  truncate high order bits if the OS does not support it.

  Own Id: OTP-13251 Aux Id: ERL-49

- `gen_tcp:accept/2` was not
  [time warp safe](time_correction.md#time-warp-safe-code). This since it used
  the same time as returned by `erlang:now/0` when calculating timeout. This has
  now been fixed.

  Own Id: OTP-13254 Aux Id: OTP-11997, OTP-13222

- Fix faulty error handling when writing to a compressed file.

  Own Id: OTP-13270

- Fix sendfile usage for large files on FreeBSD

  Own Id: OTP-13271

- Fix bug that could cause
  [`process_info(P,current_location)`](`process_info/2`) to crash emulator for
  hipe compiled modules.

  Own Id: OTP-13282 Aux Id: ERL-79

- Out of memory errors have been changed to cause an exit instead of abort.

  Own Id: OTP-13292

- When calling `garbage_collect/[1,2]` or `check_process_code/[2,3]` from a
  process with a higher priority than the priority of the process operated on,
  the run queues could end up in an inconsistent state. This bug has now been
  fixed.

  Own Id: OTP-13298 Aux Id: OTP-11388

- A workaround for an issue with older gcc versions (less than 5) and inline
  assembly on 32-bit x86 caused an emulator crash when it had been compiled with
  a newer gcc version. An improved `configure` test, run when building OTP, now
  detects whether the workaround should be used or not.

  Own Id: OTP-13326 Aux Id: ERL-80

### Improvements and New Features

- Introduced new statistics functionality in order to more efficiently retrieve
  information about run able and active processes and ports. For more
  information see:

  - [`statistics(total_run_queue_lengths)`](`m:erlang#statistics_total_run_queue_lengths`)
  - [`statistics(run_queue_lengths)`](`m:erlang#statistics_run_queue_lengths`)
  - [`statistics(total_active_tasks)`](`m:erlang#statistics_total_active_tasks`)
  - [`statistics(active_tasks)`](`m:erlang#statistics_active_tasks`)

  Own Id: OTP-13201

- Time warp safety improvements.

  Introduced the options `monotonic_timestamp`, and `strict_monotonic_timestamp`
  to the trace, sequential trace, and system profile functionality. This since
  the already existing `timestamp` option is not time warp safe.

  Introduced the option `safe_fixed_monotonic_time` to `ets:info/2` and
  `dets:info/2`. This since the already existing `safe_fixed` option is not time
  warp safe.

  Own Id: OTP-13222 Aux Id: OTP-11997

- Fix a register race where down nodes goes undetected in epmd

  Own Id: OTP-13301

- Improved the gcc inline assembly implementing double word atomic compare and
  exchange on x86/x86_64 so that it also can be used when compiling with clang.

  Own Id: OTP-13336

- An optimization preventing a long wait for a scheduler thread looking up
  information about a process executing on another scheduler thread had
  unintentionally been lost in erts-5.10 (OTP R16A). This optimization has now
  been reintroduced.

  Own Id: OTP-13365 Aux Id: OTP-9892

## Erts 7.2.1.1

### Improvements and New Features

- Introduced new statistics functionality in order to more efficiently retrieve
  information about run able and active processes and ports. For more
  information see:

  - [`statistics(total_run_queue_lengths)`](`m:erlang#statistics_total_run_queue_lengths`)
  - [`statistics(run_queue_lengths)`](`m:erlang#statistics_run_queue_lengths`)
  - [`statistics(total_active_tasks)`](`m:erlang#statistics_total_active_tasks`)
  - [`statistics(active_tasks)`](`m:erlang#statistics_active_tasks`)

  Own Id: OTP-13201

## Erts 7.2.1

### Fixed Bugs and Malfunctions

- Revert "Fix erroneous splitting of emulator path"

  Own Id: OTP-13202

- Fix HiPE enabled emulator for FreeBSD.

  Own Id: OTP-13204 Aux Id: pr926

## Erts 7.2

### Fixed Bugs and Malfunctions

- Small documentation fixes

  Own Id: OTP-13017

- Fix memory corruption bug caused by disabling distribution and then re-enable
  distribution with a node name that has previously been used by a remote node.

  Own Id: OTP-13076 Aux Id: seq12959

- Renamed variables with name bool as Visual Studio 2015 now treats this is a
  keyword.

  Own Id: OTP-13079

- `erl_prim_loader` has not supported custom loaders for several releases. In
  the documentation for `erl_prim_loader`, all references to custom loaders have
  now been removed.

  Own Id: OTP-13102

- Fixed compilation of erts together with libc versions that do not define
  \_\_uint32_t.

  Own Id: OTP-13105

- erl -make now returns non-zero exit codes on failure

  Own Id: OTP-13107

- Fix crash on init:restart in embedded mode caused by on_load handler process
  not being relaunched leading to load failure for modules such as crypto and
  asn1rt_nif that need it to be present for correct NIF loading.

  Own Id: OTP-13115

- Fix maps decode in erlang:binary_to_term/1

  Decoding a term with a large (HAMT) map in an small (FLAT) map could cause a
  critical error if the external format was not produced by beam.

  Own Id: OTP-13125

- Fix very rare bug in GC when big maps with a lot of hash collisions from a
  remote node are waiting in inner message queue.

  Own Id: OTP-13146

- Fixed a bug that could cause a crash dump to become almost empty.

  Own Id: OTP-13150

### Improvements and New Features

- Updated the xmllint target to just check the xml files with real documentation
  content.  
  Corrected some errors and added some missing target in the DTD's.

  Own Id: OTP-13026

- Add function enif_getenv to read OS environment variables in a portable way
  from NIFs.

  Own Id: OTP-13147

## Erts 7.1

### Fixed Bugs and Malfunctions

- Fix bug in ETS that could cause stray objects marked for deletion to
  occasionally be missed by the cleanup done by `safe_fixtable(_,false)`.

  Own Id: OTP-12870

- Fixed VM crash that could occur if a trace port was linked to a process, and
  the trace port terminated abnormally while handling a trace message. This bug
  has always existed in the runtime system with SMP support.

  Own Id: OTP-12901

- Instead of aborting, the vm now creates a crash dump when a system process is
  terminated.

  Own Id: OTP-12934

- Fixed a rare emulator dead lock that occurred when
  erlang:process_flag(priority,...) was called by a process that was also
  scheduled for an internal system activity.

  Own Id: OTP-12943

- The runtime system on various posix platforms (except for Linux and Solaris)
  could crash when large amounts of file-descriptors were in use.

  Own Id: OTP-12954

- A beam file compiled by hipe for an incompatible runtime system was sometimes
  not rejected by the loader, which could lead to vm crash. This fix will also
  allow the same hipe compiler to be used by both normal and debug-built vm.

  Own Id: OTP-12962

- Fix bug in `maps:merge/2` when called by hipe compiled code that could cause
  vm crash. Bug exists since erts-7.0 (OTP 18.0).

  Own Id: OTP-12965

- When tracing with `process_dump` option, the VM could abort if there was an
  ongoing binary match somewhere in the call stack of the traced process.

  Own Id: OTP-12968

- Fixed possible output deadlock in tty driver when hitting "CTRL-C" in a
  non-smp emulator shell on unix.

  Own Id: OTP-12987 Aux Id: Seq12947

- Fix `binary_to_integer` to throw badarg for "+" and "-" similar to
  `list_to_integer`.

  Own Id: OTP-12988

- Suppress warning of unused argument when using macro enif_make_pid.

  Own Id: OTP-12989

### Improvements and New Features

- Changed default clock source used for OS system time on MacOS X to
  `gettimeofday()` in order to improve performance. The system can be configured
  during build to use the previously used higher resolution clock source by
  passing the switch
  [`--with-clock-resolution=high`](`e:system:install.md#advanced-configuration-and-build-of-erlang-otp_configuring`)
  when configuring the build.

  Own Id: OTP-12945 Aux Id: OTP-12892

- Added the `configure` option
  [`--disable-saved-compile-time`](`e:system:install.md#advanced-configuration-and-build-of-erlang-otp_configuring`)
  which disables saving of compile date and time in the emulator binary.

  Own Id: OTP-12971

## Erts 7.0.3

### Fixed Bugs and Malfunctions

- Fixed a binary memory leak when printing to shell using the tty driver (i.e.
  not -oldshell).

  Own Id: OTP-12941

- Fix a bug where the standard error port sometimes crashes with eagain as the
  reason.

  Own Id: OTP-12942

- When tracing with `process_dump` option, the VM could abort if there was an
  ongoing binary match somewhere in the call stack of the traced process./

  Own Id: OTP-12968

## Erts 7.0.2

### Fixed Bugs and Malfunctions

- A process could end up in an inconsistent half exited state in the runtime
  system without SMP support. This could occur if the processes was traced by a
  port that it also was linked to, and the port terminated abnormally while
  handling a trace message for the process.

  This bug has always existed in the runtime system without SMP support, but
  never in the runtime system with SMP support.

  Own Id: OTP-12889 Aux Id: seq12885

- Removed unnecessary copying of data when retrieving corrected Erlang monotonic
  time.

  Own Id: OTP-12894

- Changed default OS monotonic clock source chosen at build time. This in order
  to improve performance. The behavior will now on most systems be that (both OS
  and Erlang) monotonic time stops when the system is suspended.

  If you prefer that monotonic time elapse during suspend of the machine, you
  can pass the command line argument
  `--enable-prefer-elapsed-monotonic-time-during-suspend` to `configure` when
  building Erlang/OTP. The configuration stage will try to find such a clock
  source, but might not be able to find it. Note that there might be a
  performance penalty associated with such a clock source.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12895

- `erlang:system_info(end_time)` returned a faulty value on 32-bit
  architectures.

  Own Id: OTP-12896

### Improvements and New Features

- The `configure` command line argument
  `--enable-gettimeofday-as-os-system-time` has been added which force usage of
  `gettimeofday()` for OS system time. This will improve performance of
  `os:system_time()` and `os:timestamp()` on MacOS X, at the expense of worse
  accuracy, resolution and precision of Erlang monotonic time, Erlang system
  time, and OS system time.

  Own Id: OTP-12892

## Erts 7.0.1

### Fixed Bugs and Malfunctions

- Fix a rare hanging of the VM seen to happen just after emulator start. Bug
  exists since R14.

  Own Id: OTP-12859 Aux Id: seq12882

## Erts 7.0

### Fixed Bugs and Malfunctions

- Fix issuing with spaces and quoting in the arguments when using
  erlang:open_port spawn_executable on windows. The behavior now mimics how unix
  works. This change implies a backwards incompatibility for how
  spawn_executable works on windows.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11905

- Fix global call trace when hipe compiled code call beam compiled functions.
  Tracing of beam functions should now always work regardless who the caller is.

  Own Id: OTP-11939

- Correct cache alignment for ETS `write_concurrency` locks to improve
  performance by reduced false sharing. May increase memory footprint for tables
  with `write_concurrency`.

  Own Id: OTP-11974

- All possibly blocking operations in the fd/spawn and terminal driver have been
  converted to non-blocking operations. Before this fix it was possible for the
  VM to be blocked for a long time if the entity consuming stdout/stderr did not
  consume it fast enough.

  Own Id: OTP-12239

- Add missing overhead for offheap binaries created from external format. This
  fix can improve the garbage collection of large binaries originating from
  `binary_to_term` or messages from remote nodes.

  Own Id: OTP-12554

- Ensure hashing of zero is consistent

  Erlang treats positive and negative zero as equal:

  `true = 0.0 =:= 0.0/-1`

  However, Erlangs hash functions: hash, phash and phash2 did not reflect this
  behaviour. The hash values produced by the different hash functions would not
  be identical for positive and negative zero.

  This change ensures that hash value of positive zero is always produced
  regardless of the signedness of the zero float, i.e.,

  `true = erlang:phash2(0.0) =:= erlang:phash2(0.0/-1)`

  Own Id: OTP-12641

- Ensure NIF term creation disallows illegal floating point values and too long
  atoms. Such values will cause a NIF to throw badarg exception when it returns.

  Own Id: OTP-12655

- Fixed building of Map results from match_specs

  A faulty "box-value" entered into the heap which could cause a segmentation
  fault in the garbage collector if it was written on a heap fragment.

  Own Id: OTP-12656

- Fix hipe bug when matching a "writable" binary. The bug has been seen to
  sometimes cause a failed binary matching of a correct utf8 character, but
  other symptoms are also possible.

  Own Id: OTP-12667

- Keep dirty schedulers from waking other schedulers.

  Own Id: OTP-12685

- Disable floating point exceptions if the VM is compiled by clang/llvm. This is
  a known long-standing problem in clang/llvm.

  Own Id: OTP-12717

- Fix bug in `file:sendfile` for FreeBSD causing not the entire file to be sent.

  Own Id: OTP-12720

- Fix the broken Android support in erl_child_setup.c

  Own Id: OTP-12751

- Faulty statistics reported by the `fix_alloc` allocator.

  Own Id: OTP-12766

- Fix two erts_snprintf() calls to correct sizes.

  \- run_erl.c (ose): Use the size of the signal type, not its pointer. -
  erl_node_tables.c: Use the size of the \_BUFFER in erts_snprintf() to make
  sure we can use the full space.

  Own Id: OTP-12771

- Delayed memory allocations could be delayed an unnecessarily long time.

  Own Id: OTP-12812

- Make sure that timeouts on a pool of acceptors are released in the correct
  order.

  Own Id: OTP-12817

- Fix segmentation fault in module_info for deleted modules

  Own Id: OTP-12820

- Fix garbage collection of literals in code purge

  During code purging and check_process_code, the checking of the binary
  reference embedded in the match binary state was omitted for the tracing
  tests. This would cause the binary match state to reference deallocated
  memory.

  Own Id: OTP-12821

- A bug has been corrected for gen_tcp:close so when \{linger,\{true,0\}\} is in
  effect it does not wait for data in the driver queue to transfer out before
  closing the port. Bug fix by Rory Byrne.

  Own Id: OTP-12840

- The documentation of the driver callback [`start()`](driver_entry.md#start)
  erroneously stated that a return value of `ERL_DRV_ERROR_ERRNO` caused the
  error value to be passed via `erl_errno` when it should have been `errno`.

  Own Id: OTP-12855

### Improvements and New Features

- Add `md5` and `module` entries to `?MODULE:module_info/0/1` and remove
  obsolete entry 'import'.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11940

- Debug function `erlang:display/1` shows content of binaries and bitstrings,
  not only the length.

  Own Id: OTP-11941

- The time functionality of Erlang has been extended. This both includes a
  [new API](time_correction.md#new-time-api) for time, as well as
  [time warp modes](time_correction.md#time-warp-modes) which alters the
  behavior of the system when system time changes. _You are strongly encouraged
  to use the new API_ instead of the old API based on `erlang:now/0`.
  `erlang:now/0` has been deprecated since it is and forever will be a
  scalability bottleneck. For more information see the
  [Time and Time Correction](time_correction.md) chapter of the ERTS User's
  Guide.

  Besides the API changes and time warp modes a lot of scalability and
  performance improvements regarding time management has been made internally in
  the runtime system. Examples of such improvements are scheduler specific timer
  wheels, scheduler specific BIF timer management, parallel retrieval of
  monotonic time and system time on systems with primitives that are not buggy.

  Own Id: OTP-11997

- `erlang:function_exported(M, F, A)` will now return `true` if `M:F/A` refers
  to a BIF.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12099

- New BIF: `erlang:get_keys/0`, lists all keys associated with the process
  dictionary. Note: `erlang:get_keys/0` is auto-imported.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12151 Aux Id: seq12521

- Make distributed send of large messages yield to improve real-time
  characteristics.

  Own Id: OTP-12232

- Use high accuracy poll timeouts

  Where available, use poll/select API's that can handle time resolutions less
  than 1ms. In the cases where such API's are not available the timeout is
  rounded up to the nearest ms.

  Own Id: OTP-12236

- The internal group to user_drv protocol has been changed to be synchronous in
  order to guarantee that output sent to a process implementing the user_drv
  protocol is printed before replying. This protocol is used by the
  standard_output device and the ssh application when acting as a client.

  This change changes the previous unlimited buffer when printing to standard_io
  and other devices that end up in user_drv to 1KB.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12240

- The previously introduced "eager check I/O" feature is now enabled by default.

  Eager check I/O can be disabled using the `erl` command line argument:
  `+secio false`

  Characteristics impact compared to previous default:

  - Lower latency and smoother management of externally triggered I/O
    operations.
  - A slightly reduced priority of externally triggered I/O operations.

  Own Id: OTP-12254 Aux Id: OTP-12117

- Properly support maps in match_specs

  Own Id: OTP-12270

- The notice that a crashdump has been written has been moved to be printed
  before the crashdump is generated instead of afterwords. The wording of the
  notice has also been changed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12292

- New function `ets:take/2`. Works the same as `ets:delete/2` but also returns
  the deleted object(s).

  Own Id: OTP-12309

- Tracing with cpu_timestamp option has been enabled on Linux.

  Own Id: OTP-12366

- ets:info/1,2 now contains information about whether write_concurrency or
  read_concurrency is enabled.

  Own Id: OTP-12376

- Improved usage of `gcc`'s builtins for atomic memory access. These are used
  when no other implementation of atomic memory operations is available. For
  example, when compiling for ARM when `libatomic_ops` is not available.

  The largest improvement will be seen when compiling with a `gcc` with support
  for the `__atomic_*` builtins (using a `gcc` of at least version 4.7), but
  also when only the legacy `__sync_*` builtins are available (using a `gcc` of
  at least version 4.1) an improvement can be seen.

  For more information see the
  "[Atomic Memory Operations and the VM](`e:system:install.md#advanced-configuration-and-build-of-erlang-otp_configuring_atomic-memory-operations-and-the-vm`)"
  section of `$ERL_TOP/HOWTO/INSTALL.md`.

  Own Id: OTP-12383

- Introduce `math:log2/1` function to math module.

  Own Id: OTP-12411

- The documentation of the Abstract Format (in the ERTS User's Guide) has been
  updated with types and specification. (Thanks to Anthony Ramine.)

  The explicit representation of parentheses used in types of the abstract
  format has been removed. Instead the new functions
  `erl_parse:type_inop_prec()` and `erl_parse:type_preop_prec()` can be used for
  inserting parentheses where needed.

  Own Id: OTP-12492

- Remove perfctr support

  Development of perfctr in the linux kernel ceased in 2010. The perfctr support
  code in the Erlang VM is thus effectively dead code and therefore removed.

  Own Id: OTP-12508

- `zlib:inflateChunk/2` has been added. It works like `zlib:inflate/2`, but
  decompresses no more data than will fit in the buffer configured by
  `zlib:setBufSize/2`.

  Own Id: OTP-12548

- Use linear search for small select_val arrays

  Own Id: OTP-12555

- New BIF ets:update_counter/4 with a default object as argument, which will be
  inserted in the table if the key was not found.

  Own Id: OTP-12563

- Export missing types from zlib module

  Own Id: OTP-12584

- Use persistent hashmaps for large Maps

  Maps will use a persistent hashmap implementation when the number of pairs in
  a Map becomes sufficiently large. The change will occur when a Map reaches 33
  pairs in size but this limit might change in the future.

  The most significant impact for the user by this change is speed, and to a
  lesser degree memory consumption and introspection of Maps. Memory consumption
  size is probalistic but lesser than `gb_trees` or `dict` for instance. Any
  other impacts will be transparent for the user except for the following
  changes.

  Semantics of Maps have changed in two incompatible ways compared to the
  experimental implementation in OTP 17:

  - Hashing of maps is done different by `erlang:phash2/1,2`, `erlang:phash/1`
    and `erlang:hash/2`.
  - Comparing two maps with ==, /=, =<, <, >= and >, is done different if the
    keys contain floating point numbers.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12585

- Scalability improvement for `erlang:make_ref/0`, and other functionality that
  create references. Each scheduler now manage its own set of references. By
  this no communication at all is needed when creating references.

  Previous implementation generated a strictly monotonically increasing sequence
  of references corresponding to creation time on the runtime system instance.
  This is _not_ the case with current implementation. You can only expect
  reference to be unique. The Erlang/OTP documentation has never mentioned
  anything else but the uniqueness property, so this change _is_ fully
  compatible. The only reason we've marked this as a potential incompatibility
  is since an early draft for an Erlang specification mentions strict
  monotonicity as a property.

  If you need to create data with a strict monotonicity property use
  [erlang:unique_integer(\[monotonic])](`erlang:unique_integer/1`). Do _not_ use
  the deprecated [erlang:now()](`erlang:now/0`).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12610

- Enable different abort signal from heart

  By using environment variable HEART_KILL_SIGNAL, heart can now use a different
  signal to kill the old running Erlang.

  By default the signal is SIGKILL but SIGABRT may also be used by setting
  environment variable: HEART_KILL_SIGNAL=SIGABRT

  Own Id: OTP-12613 Aux Id: seq12826

- Update autconf to latest version 2015-03-04

  Own Id: OTP-12646

- Optimization of timers internally in the VM. This include process timers
  (`receive ... after`), port timers (`driver_set_timer()`) as well as BIF
  timers (`erlang:send_after()`/`erlang:start_timer()`).

  Each scheduler thread now has its own lock-free timer service instead of one
  locked central service. This dramatically improves performance of timer
  management on systems with a large amount of schedulers and timers.

  The timer service internal data structure has also been optimized to be able
  to handle more timers than before. That is, each timer service is by its self
  able to handle more timers without dramatic performance loss than the old
  centralized timer service.

  The API of BIF timers has also been extended. Timeout values are for example
  no longer limited to 32-bit integers. For more information see the
  documentation of `erlang:start_timer/4`, `erlang:send_after/4`,
  `erlang:cancel_timer/2`, and `erlang:read_timer/2`.

  Characteristics impact: Calls to the synchronous versions of
  `erlang:cancel_timer()`, and `erlang:read_timer()` may take substantially
  longer time to complete than before. This occur when the timer that is
  accessed is managed by a remote scheduler. You typically want to use the new
  asynchronous option in order to avoid blocking the calling process.

  Own Id: OTP-12650 Aux Id: OTP-11997

- Specialize instructions from common assembler patterns

  Specialize common instructions of `rem`, `band`, `minus` and `plus` in the
  beam loader. This will reduce the number of fetches and thus lessen the
  instruction dispatch pressure during runtime and speed up those operations in
  some common cases.

  Specialize move patterns from x-registers to the stack with a new
  `move_window` instruction. This change will reduce instruction dispatch
  pressure.

  Own Id: OTP-12690

- Fix cross compilation for Android.

  Own Id: OTP-12693

- Fix incorrect use of autoconf macro AC_EGREP_CPP, which could cause faulty
  configuration if run from a path containing the string 'yes'.

  Own Id: OTP-12706

- Minimal Java version is now 1.6

  Own Id: OTP-12715

- Send format and args on process exit to error_logger

  Previously, the emulator would generate a whole string with values and call
  the error_logger passing `"~s~n"`. This changes it to a format string
  containing `~p` with the respective values as arguments.

  Own Id: OTP-12735

- Map error logger warnings to warning messages by default.

  Own Id: OTP-12755

- Configure architecture ppc64le architecture as a ppc64

  Own Id: OTP-12761

- Add function `enif_raise_exception` to allow a NIF to raise an error exception
  with any type of reason.

  Own Id: OTP-12770

- Optimized node table statistics retrieval.

  Own Id: OTP-12777

- Map beam error logger warnings to warning messages by default. Previously
  these messages were mapped to the error channel by default.

  Own Id: OTP-12781

- gen_tcp:shutdown/2 is now asynchronous

  This solves the following problems with the old implementation:

  It doesn't block when the TCP peer is idle or slow. This is the expected
  behaviour when shutdown() is called: the caller needs to be able to continue
  reading from the socket, not be prevented from doing so.

  It doesn't truncate the output. The current version of gen_tcp:shutdown/2 will
  truncate any outbound data in the driver queue after about 10 seconds if the
  TCP peer is idle of slow. Worse yet, it doesn't even inform anyone that the
  data has been truncated: 'ok' is returned to the caller; and a FIN rather than
  an RST is sent to the TCP peer.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12797

- Introduced delayed node table GC. This in order to avoid oscillation of
  entries in and out of the tables. The oscillation caused unnecessary lock
  contention on the table locks. The delay length can be set by passing the
  [`+zdntgc`](erl_cmd.md#%2Bzdntgc) command line argument.

  Characteristics impact: The tables can grow to very large sizes with unused
  entries if the node is get huge amounts of short lived connections from other
  nodes. This problem can be alleviated by shortening the length of the delay
  using the `+zdntgc` command line argument.

  Own Id: OTP-12802

- Improved implementation of [`erlang:statistics`](`erlang:statistics/1`)`(io)`
  in order to reduce contention between schedulers.

  Characteristics impact: The actual call to `erlang:statistics(io)` takes
  longer time to complete, but the overall impact on the system is improved.

  Own Id: OTP-12842

- There are many cases where user code needs to be able to distinguish between a
  socket that was closed normally and one that was aborted. Setting the option
  \{show_econnreset, true\} enables the user to receive ECONNRESET errors on
  both active and passive sockets.

  Own Id: OTP-12843

- Do not preallocate too large event pool

  A default pool size of 4000 is too excessive for the common case. This
  corresponds directly to the number of threads in the system. Change
  ERTS_TS_EV_ALLOC_DEFAULT_POOL_SIZE to 2048. Change ERTS_TS_EV_ALLOC_POOL_SIZE
  to 32.

  Own Id: OTP-12849

## Erts 6.4.1.7

### Fixed Bugs and Malfunctions

- A process communicating with a port via one of the `erlang:port_*` BIFs could
  potentially end up in an inconsistent state if the port terminated during the
  communication. When this occurred the process could later block in a `receive`
  even though it had messages matching in its message queue.

  This bug was introduced in erts version 5.10 (OTP R16A).

  Own Id: OTP-13424 Aux Id: OTP-10336

- Calls to `erl_drv_send_term()` or `erl_drv_output_term()` from a non-scheduler
  thread while the corresponding port was invalid caused the emulator to enter
  an inconsistent state which eventually caused an emulator crash.

  Own Id: OTP-13866

- Driver and NIF operations accessing processes or ports could cause an emulator
  crash when used from non-scheduler threads. Those operations are:

  - `erl_drv_send_term()`
  - `driver_send_term()`
  - `erl_drv_output_term()`
  - `driver_output_term()`
  - `enif_send()`
  - `enif_port_command()`

  Own Id: OTP-13869

- Fix bug in `binary_to_term` for binaries created by `term_to_binary `with
  option `compressed`. The bug can cause `badarg` exception for a valid binary
  when Erlang VM is linked against a `zlib` library of version 1.2.9 or newer.
  Bug exists since OTP 17.0.

  Own Id: OTP-14159 Aux Id: ERL-340

- Fixed bug in operator `bxor` causing erroneuos result when one operand is a
  big _negative_ integer with the lowest `N*W` bits as zero and the other
  operand not larger than `N*W` bits. `N` is an integer of 1 or larger and `W`
  is 32 or 64 depending on word size.

  Own Id: OTP-14514

- Fixed bug in `binary_to_term` and `binary_to_atom` that could cause VM crash.
  Typically happens when the last character of an UTF8 string is in the range
  128 to 255, but truncated to only one byte. Bug exists in `binary_to_term`
  since ERTS version 5.10.2 (OTP_R16B01) and `binary_to_atom` since ERTS version
  9.0 (OTP-20.0).

  Own Id: OTP-14590 Aux Id: ERL-474

## Erts 6.4.1.6

### Fixed Bugs and Malfunctions

- When calling `garbage_collect/[1,2]` or `check_process_code/[2,3]` from a
  process with a higher priority than the priority of the process operated on,
  the run queues could end up in an inconsistent state. This bug has now been
  fixed.

  Own Id: OTP-13298 Aux Id: OTP-11388

## Erts 6.4.1.5

### Fixed Bugs and Malfunctions

- Fixed a bug that could cause a crash dump to become almost empty.

  Own Id: OTP-13150

## Erts 6.4.1.4

### Fixed Bugs and Malfunctions

- The 'raw' socket option could not be used multiple times in one call to any
  e.g gen_tcp function because only one of the occurrences were used. This bug
  has been fixed, and also a small bug concerning propagating error codes from
  within inet:setopts/2.

  Own Id: OTP-11482 Aux Id: seq12872

## Erts 6.4.1

### Fixed Bugs and Malfunctions

- The VTS mode in Common Test has been modified to use a private version of the
  Webtool application (ct_webtool).

  Own Id: OTP-12704 Aux Id: OTP-10922

## Erts 6.4

### Fixed Bugs and Malfunctions

- Fix missing quotation in the `LM_FIND_EMU_CC` `autoconf` macro which could
  cause build failures.

  Own Id: OTP-12388

- Fix erroneous printout of monitors in crashdump file.

  Own Id: OTP-12537

- The runtime system without SMP support could crash in the BIF
  [`port_control/3`](`port_control/3`) if the port that was being accessed died
  during the call to the BIF.

  Own Id: OTP-12544 Aux Id: Seq12777

- Avoid corrupt oversized integer to be created from binary matching. Instead
  throw system_limit exception which is the correct behavior. A peculiar symptom
  of this bug was that bitwise operations (band, bor, bxor) on such oversized
  integers could return the empty list []. Credit: Mikael Pettersson, Nico
  Kruber

  Own Id: OTP-12556

- A race condition when calling `port_info/1` could cause a memory fault has
  been fixed.

  Own Id: OTP-12587

- Fix comparison of exact terms. An overflow that could cause faulty comparisons
  has been fixed. Comparison of exact terms is exclusively used within Maps.

  Own Id: OTP-12623

- Fix bug in [`list_to_integer/1`](`list_to_integer/1`) for very long lists that
  could cause VM crash.

  Own Id: OTP-12624

### Improvements and New Features

- Introduced a runtime system internal 64-bit API for atomic memory operations.

  Own Id: OTP-12351

- Add command line argument option for the initial size of process dictionaries.

  Use '+hpds <size>' to set initial process dictionary size for spawned
  processes.

  Own Id: OTP-12535 Aux Id: seq12809

- Fix documentation on $char for Unicode

  Own Id: OTP-12545

## Erts 6.3.1

### Fixed Bugs and Malfunctions

- Fix getifaddrs realloc pointer error

  When a buffer was exhausted and subsequently reallocated, we could get an
  unsafe pointer pointing to faulty memory.

  For this to occur we would need to have a large number of interfaces and a
  reallocation of memory to a lower addresses.

  The symptom would be garbage returned from erlang:port_control(Port, 25, [])
  (prim_inet:getifaddrs(Port) resulting in a badarg) or a segmentation fault.

  Own Id: OTP-12445

- Don't close all file descriptors twice in child_setup

  The commit c2b4eab25c907f453a394d382c04cd04e6c06b49 introduced an error in
  which child_setup erroneously tried to close all file descriptors twice.

  Use closefrom() if available when closing all file descriptors.

  The function closefrom() was only used in the vfork() case before but is now
  also used in the fork() case if available.

  Own Id: OTP-12446

- During a crashdump all file descriptors are closed to ensure the closing of
  the epmd port and to reserve a file descriptor for the crashdump file.

  If a driver (third party library) cannot handle closing of sockets this could
  result in a segmentation fault in which case a crashdump would not be
  produced. This is now fixed by only closing inets sockets via an emergency
  close callback to the driver and thus closing the epmd socket.

  Own Id: OTP-12447

## Erts 6.3

### Fixed Bugs and Malfunctions

- Fix HiPE debug lock checking on OS X 64bit

  Position-independent code is mandatory on OS X. We use r11 as an intermediate
  register to fill BIF_P->hipe.bif_callee. This fixes the following error when
  doing \`make debug FLAVOR=smp\`:

  clang -cc1as: fatal error: error in backend: 32-bit absolute addressing is not
  supported in 64-bit mode

  Own Id: OTP-12188

- Fix race bug that could cause VM crash in `erlang:port_get_data/1` if the port
  was closed by a concurrent process. Also fix fatal bug if `port_set_data/2` is
  called with a non-immediate data term. Both bugs exist since R16B01.

  Own Id: OTP-12208

- Correct make variable SSL_DED_LD_RUNTIME_LIBRARY_PATH when erl_xcomp_sysroot
  ends with a slash.

  Own Id: OTP-12216 Aux Id: seq12700

- Fix two cases of unreachable code caused by false use of assignment operators.

  Own Id: OTP-12222

- Fix bug when hipe compiled code makes tail call to a BIF that disables GC
  while trapping (sush as binary_to_list, list_to_binary, binary_to_term,
  term_to_binary).

  Own Id: OTP-12231

- Fix bug when a migrated empty memory carrier is reused just before it should
  be destroyed by the thread that created it.

  Own Id: OTP-12249

- Prevents compile-time errors in NIFs, when the compiler is instructed to treat
  missing field initializers as errors, by adding an initializer for the new
  options field which was added to ErlNifEntry for 17.3.

  Own Id: OTP-12266

- Fixed CPU topology detection on FreeBSD systems where Erlang/OTP is compiled
  by new C compilers (including, but possibly not limited to, gcc 4.9 and
  clang).

  Own Id: OTP-12267

- Use C99 function isfinite() instead of finite() when available on non GCC
  compilers.

  Own Id: OTP-12268

- Fix bug on windows where an incorrect number of links could be returned when
  doing file:read_file_info on a directory.

  Own Id: OTP-12269

- Fix rare bug when purging module on VM started with +Meamin.

  Own Id: OTP-12273

- Repair run_erl terminal window size adjustment sent from to_erl. This was
  broken in OTP 17.0 which could lead to strange cursor behaviour in the to_erl
  shell.

  Own Id: OTP-12275 Aux Id: seq12739

- Fixed bug on windows causing gen_tcp/udp to return an error when given an fd
  to work with.

  Own Id: OTP-12289

- Fix various internal erts issues where negating a signed integer in C would
  trigger undefined behavior. This fixes issues when dividing with bignums and
  list_to_integer.

  Own Id: OTP-12290

- When flushing output to stdout on windows, the emulator could sometimes hang
  indefinitely waiting for the flush to complete. This has been fixed.

  Own Id: OTP-12291

- Fix so that non-smp emulators with dirty scheduler support shows the correct
  number of dirty schedulers when calling erlang:system_info(system_version).

  Own Id: OTP-12295

- Add `nif_version` to `erlang:system_info/1` in order to get the NIF API
  version of the runtime system in a way similar to `driver_version`.

  Own Id: OTP-12298

- Fix bug that could cause the return value from dirty NIF with zero arity to be
  treated as garbage, leading to VM crash.

  Own Id: OTP-12300

- Improve allocation carrier migration search logic. This will reduce the risk
  of failed migrations that could lead to excess memory consumption. It will
  also improve smp performance due to reduced memory contention on the migration
  pool.

  Own Id: OTP-12323

### Improvements and New Features

- Introduced support for eager check I/O.

  By default eager check I/O will be disabled, but this will most likely be
  changed in OTP 18. When eager check I/O is enabled, schedulers will more
  frequently check for I/O work. Outstanding I/O operations will however not be
  prioritized to the same extent as when eager check I/O is disabled.

  Eager check I/O can be enabled using the `erl` command line argument:
  `+secio true`

  Characteristics impact when enabled:

  - Lower latency and smoother management of externally triggered I/O
    operations.
  - A slightly reduced priority of externally triggered I/O operations.

  Own Id: OTP-12117

- Fix erts .app-file

  Own Id: OTP-12189

- Add configure option --with-ssl-incl=PATH to support OpenSSL installations
  with headers and libraries at different places.

  Own Id: OTP-12215 Aux Id: seq12700

- Optimization of atomic memory operations with release barrier semantics on
  32-bit PowerPC when using the implementation included in OTP.

  Own Id: OTP-12250

- Minor adjustment of scheduler activation code making sure that an activation
  of a scheduler is not prevented by its run-queue being non-empty. (Thanks to
  Songlu Cai)

  Own Id: OTP-12287

- Improved support for atomic memory operations provided by the
  [libatomic_ops](https://github.com/ivmai/libatomic_ops/) library. Most
  importantly support for use of native double word atomics when implemented by
  `libatomic_ops` (for example, implemented for ARM).

  The
  [`$ERL_TOP/HOWTO/INSTALL.md`](`e:system:install.md#advanced-configuration-and-build-of-erlang-otp_configuring_atomic-memory-operations-and-the-vm`)
  document now also more clearly describes when you want to build together with
  a `libatomic_ops` installation.

  Own Id: OTP-12302

- Add configure option --with-ssl-rpath to control which runtime library path to
  use for dynamic linkage toward OpenSSL.

  Own Id: OTP-12316 Aux Id: seq12753

- Added systemd notify support to epmd

  Own Id: OTP-12321

## Erts 6.2.1

### Fixed Bugs and Malfunctions

- Fix bug when an migrated empty memory carrier is reused just before it should
  be destroyed by the thread that created it.

  Own Id: OTP-12249

- Repair run_erl terminal window size adjustment sent from to_erl. This was
  broken in OTP 17.0 which could lead to strange cursor behaviour in the to_erl
  shell.

  Own Id: OTP-12275 Aux Id: seq12739

## Erts 6.2

### Fixed Bugs and Malfunctions

- General documentation updates.

  Own Id: OTP-12052

- A bug in the VM code implementing sending of signals to ports could cause the
  receiving port queue to remain in a busy state forever. When this state had
  been reached, processes sending command signals to the port either got
  suspended forever, or, if the `nosuspend` feature was used, always failed to
  send to the port. This bug was introduced in ERTS version 5.10.

  In order for this bug to be triggered on a port, one had to at least once
  utilize the `nosuspend` functionality when passing a signal to the port. This
  by either calling

  - [`port_command(Port, Data, [nosuspend | Options])`](`erlang:port_command/3`),
  - [`erlang:send(Port, {PortOwner, {command, Data}}, [nosuspend | Options])`](`erlang:send/3`),
  - [`erlang:send_nosuspend(Port, {PortOwner, {command, Data}})`](`erlang:send_nosuspend/2`),
    or
  - [`erlang:send_nosuspend(Port, {PortOwner, {command, Data}}, Options)`](`erlang:send_nosuspend/3`).

  Thanks Vasily Demidenok for reporting the issue, and Sergey Kudryashov for
  providing a testcase.

  Own Id: OTP-12082 Aux Id: OTP-10336

- Fix size overflow bug at memory allocation. A memory allocation call, with an
  insane size close to the entire address space, could return successfully as if
  it had allocated just a few bytes. (Thanks to Don A. Bailey for reporting)

  Own Id: OTP-12091

- Fix various issues where negating a signed integer would trigger undefined
  behaviour. This fixes issues in the enif_make_int64 interface and some edge
  cases inside the erlang runtime system.

  Own Id: OTP-12097

- The documentation erroneously listed the [`+swct`](erl_cmd.md#%2Bswct) command
  line argument under `+sws`.

  Own Id: OTP-12102 Aux Id: OTP-10994

- Profiling messages could be delivered out of order when profiling on
  `runnable_procs` and/or `runnable_ports` using `erlang:system_profile/2`. This
  bug was introduced in ERTS version 5.10.

  Own Id: OTP-12105 Aux Id: OTP-10336

- Various logging fixes, including: Add run queue index to the process dump in
  crash dumps.  
  Add thread index to enomem slogan when crashing.  
  Remove error logger message for sending messages to old instances of the same
  node.

  Own Id: OTP-12115

- Fix compiler warnings reported by LLVM

  Own Id: OTP-12138

- Correct conversion of `MIN_SMALL` by
  [`list_to_integer/1`](`list_to_integer/1`) and
  [`binary_to_integer/1`](`binary_to_integer/1`). The bug produced an
  unnormalized bignum which can cause strange behavior such as comparing
  different to a correct `MIN_SMALL` integer. The value `MIN_SMALL` is
  `-(1 bsl 27) = -134217728` on a 32-bit VM and
  `-(1 bsl 59) = -576460752303423488` on a 64-bit VM. (Thanks to Jesper Louis
  Andersen, Mikael Pettersson and Anthony Ramine for report, patch and
  optimization suggestion)

  Own Id: OTP-12140

- Fix bug in `term_to_binary` that reallocates binary with inconsistent size
  information. Bug has never been confirmed to be the cause of any faulty
  behavior.

  Own Id: OTP-12141

- Real_path method used while prim loading archive files was not taking into
  account the fact that windows directory symlinks can be across different
  drives.

  Own Id: OTP-12155

### Improvements and New Features

- Add log2 histogram to lcnt for lock wait time

  Own Id: OTP-12059

- Introduced [`enif_schedule_nif()`](erl_nif.md#enif_schedule_nif) to the NIF
  API.

  The `enif_schedule_nif()` function allows a long-running NIF to be broken into
  separate NIF invocations without the help of a wrapper function written in
  Erlang. The NIF first executes part of the long-running task, then calls
  `enif_schedule_nif()` to schedule a NIF for later execution to continue the
  task. Any number of NIFs can be scheduled in this manner, one after another.
  Since the emulator regains control between invocations, this helps avoid
  problems caused by native code tying up scheduler threads for too long.

  The `enif_schedule_nif()` function also replaces the
  `enif_schedule_dirty_nif()` in the experimental dirty NIF API. Note that the
  only incompatible changes made are in the experimental dirty NIF API.

  See the [NIF documentation](erl_nif.md) for more information.

  Thanks to Steve Vinoski.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12128

## Erts 6.1.2

### Fixed Bugs and Malfunctions

- OTP-11850 fixed filelib:wildcard/1 to work with broken symlinks. This
  correction, however, introduced problems since symlinks were no longer
  followed for functions like filelib:ensure_dir/1, filelib:is_dir/1,
  filelib:file_size/1, etc. This is now corrected.

  Own Id: OTP-12054 Aux Id: seq12660

## Erts 6.1.1

### Fixed Bugs and Malfunctions

- Fixed ETHR_FORCE_INLINE which caused the build to break on some platforms
  without adequate thread support (VxWorks).

  Own Id: OTP-12010

## Erts 6.1

### Fixed Bugs and Malfunctions

- The documentation for [`spawn_opt/5`](`spawn_opt/5`) now has a note mentioning
  that the `monitor` option is not supported.

  Own Id: OTP-11849

- Fix broken system monitoring of `large_heap` for non-smp VM. No message for
  `large_heap` was ever sent on non-smp VM. Bug exist since R16B.

  Own Id: OTP-11852

- The emulator without SMP support crashed when passing a message to a process
  without enough heap space for the message. This bug was introduced in
  `erts-6.0`.

  Own Id: OTP-11887 Aux Id: OTP-11388

- Fix race between ETS table deletion and unfixation that could cause VM crash.
  The race could happen between a terminating process that does not own the
  table but has a fixation on it and another process that deletes the table
  (maybe the owner terminating) at the same time. Bug existed since R15B02.

  Own Id: OTP-11892

- The string following the `-eval` option when invoking `erl` would not be
  properly translated from UTF-8 to a list of Unicode characters (as would the
  arguments for `-run`).

  That bug would cause the build of Erlang/OTP to fail when building in a
  directory whose pathname contained non-US ASCII characters encoded in UTF-8.
  (Thanks to Eric Pailleau for reporting this bug.)

  Own Id: OTP-11916

- Fix erts_debug:size/1 to handle Map sizes

  Own Id: OTP-11923

- Removed `erlang:bitstr_to_list/1` and `erlang:list_to_bitstr/1`. They were
  added by mistake, and have always raised an `undefined` exception when called.

  Own Id: OTP-11942

- Fixed compilation using mingw-w64 on Windows.

  Thanks to Jani Hakala.

  Own Id: OTP-11945

- The git sha is no longer printed in the shell start header when erlang is
  built from a tagged git release.

  Own Id: OTP-11961

- Fixed a bug where `send` trace events were erroneously dropped when the send
  was done to a registered process. This bug was introduced in R16B.

  Own Id: OTP-11968

### Improvements and New Features

- The following native functions now bump an appropriate amount of reductions
  and yield when out of reductions:

  - `erlang:binary_to_list/1`
  - `erlang:binary_to_list/3`
  - `erlang:bitstring_to_list/1`
  - `erlang:list_to_binary/1`
  - `erlang:iolist_to_binary/1`
  - `erlang:list_to_bitstring/1`
  - `binary:list_to_bin/1`

  Characteristics impact:

  - **Performance** - The functions converting from lists got a performance loss
    for very small lists, and a performance gain for very large lists.

  - **Priority** - Previously a process executing one of these functions
    effectively got an unfair priority boost. This priority boost depended on
    the input size. The larger the input was, the larger the priority boost got.
    This unfair priority boost is now lost.

  Own Id: OTP-11888

- The systemd features of epmd have been removed from epmd by default. To enable
  them you have to build erlang with the configure option --enable-systemd.

  Own Id: OTP-11921

- Removed Erlang wrapper code used when calling
  [`binary_to_term/1`](`binary_to_term/1`), and
  [`binary_to_term/2`](`binary_to_term/2`). This improves the performance of
  these BIFs especially when they are called with small binaries as input.

  Own Id: OTP-11931

- Add erlang:system_info(tolerant_timeofday), an API to check whether
  compensation for sudden changes of system time is enabled or not.

  Own Id: OTP-11970

## Erts 6.0.1

### Fixed Bugs and Malfunctions

- Fix broken system monitoring of `large_heap` for non-smp VM. No message for
  `large_heap` was ever sent on non-smp VM. Bug exist since R16B.

  Own Id: OTP-11852

- Fixed type spec of `erlang:system_info/1`.

  Own Id: OTP-11859 Aux Id: OTP-11615

## Erts 6.0

### Fixed Bugs and Malfunctions

- The option dupnames did not work as intended in re. When looking for names
  with \{capture, \[Name, ...]\}, re:run returned a random instance of the match
  for that name, instead of the leftmost matching instance, which was what the
  documentation stated. This is now corrected to adhere to the documentation.
  The option \{capture,all_names\} along with a re:inspect/2 function is also
  added to further help in using named subpatterns.

  Own Id: OTP-11205

- Allow loading of NIF library with unicode path name

  Own Id: OTP-11408

- Allow loading of driver with unicode path name

  Own Id: OTP-11549

- Fixed a bug where starting Erlang without having an open stdin on fd 0 would
  sometimes deadlock the emulator when terminating.

  Own Id: OTP-11558

- The option '-names' in epmd now works on Windows (Thanks to Johannes Weißl)

  Own Id: OTP-11565

- Correction of the examples in escript documentation. (Thanks to Pierre
  Fenoll).

  Own Id: OTP-11577

- Fix bs_get_integer instruction

  The instruction bs_get_integer could unnecessarily trigger a garbage
  collection in failure cases which is unwanted or outright dangerous.

  Ex:

  <<X:Sz,\_/bits>> = <<"some binary">>

  Previously, if Sz induced X to a bignum it would reserved memory size this on
  the heap via a garbage collection before checking if the size could actually
  match.

  It will now check the binary size before triggering a collection.

  Own Id: OTP-11581

- Remove heap space overestimation in `binary_to_term` (and remote message
  reception) for integers in the intervals \[-2147483648,-1] and
  \[256,2147483647] on 64-bit emulators.

  Own Id: OTP-11585

- Add support for detecting the separate tinfo library from ncurses (Thanks to
  Dirkjan Ochtman)

  Own Id: OTP-11590

- Deprecation warning for system_flag(cpu_topology) has been extended for
  removal in OTP 18 (Thanks to Steve Vinoski for the update)

  Own Id: OTP-11602

- Documentation improvement regarding some awkward wording around the +spp flag.
  (Thanks to Brian L. Troutwine )

  Own Id: OTP-11607

- Fixed bug where sendfile would return the wrong error code for a remotely
  closed socket if the socket was in passive mode. (Thanks to Vincent Siliakus
  for reporting the bug.)

  Own Id: OTP-11614

- Increase garbage collection tenure rate

  The garbage collector tries to maintain the previous heap block size during a
  minor gc, i.e. 'need' is not utilized in determining the size of the new heap,
  instead it relies on tenure and garbage to be sufficiently large.

  In instances during intense growing with exclusively live data on the heap
  coupled with delayed tenure, fullsweeps would be triggered directly after a
  minor gc to make room for 'need' since the new heap would be full.

  To remedy this, the tenure of terms on the minor heap will always happen (if
  it is below the high watermark) instead of every other minor gc.

  Characteristics Impact: Reduced CPU-time spent in garbage collection but may
  infer delays in collecting garbage from the heap. Tweak 'fullsweep_after'
  options to increase gc pressure if needed.

  Own Id: OTP-11617

- Fix bug when comparing integers with floats larger than 2^992. The bug could
  potentially cause memory corruption on 32-bit emulators.

  Own Id: OTP-11618

- Cross-compilation fixes for TileraMDE-3.0.1.125620

  Own Id: OTP-11635

- sendfile no longer uses async threads by default

  This has been done because a slow client attack is possible if the async
  thread pool is used. The scenario is:

  Client does a request for a file and then slowly receives the file one byte at
  a time. This will eventually fill the async thread pool with blocking sendfile
  operations and thus starving the vm of all file operations.

  If you still want to use the async threads pool for sendfile an option to
  enable it has been introduced.

  Thanks to Christopher Faulet for identifying this vulnerability.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11639

- Do proper rollback of calls to `enif_open_resource_type` when load/upgrade
  callbacks of NIF library return failure.

  Own Id: OTP-11722

- Changed the default configuration when configuring with `$ERL_TOP/configure`
  to be the same as when configuring with `$ERL_TOP/otp_build configure`.

  Previously floating point exceptions got enabled by default on Linux when HiPE
  was enabled when configuring with `$ERL_TOP/configure`, but not when
  configuring with `$ERL_TOP/otp_build configure`. The default is now in both
  cases not to use floating point exceptions since there still exist unresolved
  issues with floating point exceptions on Linux.

  For more information see [`$ERL_TOP/HOWTO/INSTALL.md`](`e:system:install.md`).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11723

- A comment in erl_db_tree.c no longer differ from the code. (Thanks to Cobus
  Carstens)

  Own Id: OTP-11793

- Fix epmd debug functionality for VxWorks (Thanks to Jay True)

  Own Id: OTP-11808

- Use closefrom/2 when available in child_setup (Thanks to Rick Reed and Anthony
  Ramine)

  Own Id: OTP-11809

- Fix dtrace/systemtap bug where the probe arguments would be concatenated due
  to faulty length calculation.

  Thanks to Michal Ptaszek and Scott Lystig Fritchie

  Own Id: OTP-11816

- It is now better documented that the `+fn*` flags to `erl` also affect how
  command line parameters and environment variables are read. (Thanks to Vlad
  Dumitrescu)

  Own Id: OTP-11818

### Improvements and New Features

- Options to set match_limit and match_limit_recursion are added to re:run. The
  option report_errors is also added to get more information when re:run fails
  due to limits or compilation errors.

  Own Id: OTP-10285

- Dialyzer's `unmatched_return` warnings have been corrected.

  Own Id: OTP-10908

- A common case is to wrap an argument to
  [`list_to_binary/1`](`list_to_binary/1`) in a list to ensure conversion can
  happen even though the argument may already be a binary. Take special care of
  this case and do not copy binary.

  Impact: May cause incompatibility since a single binary is no longer copied.
  Use `binary:copy/1,2` instead.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11082

- Make erlang:open_port/2 spawn and spawn_executable handle unicode.

  Own Id: OTP-11105

- Handle unicode (widestring) in erl, erlc, heart, etc on windows.

  Own Id: OTP-11135

- The version of the PCRE library Used by Erlang's re module is raised to 8.33
  from 7.6. This means, among other things, better Unicode and Unicode Character
  Properties support. New options connected to PCRE 8.33 are also added to the
  re module (ucd, notempty_atstart, no_start_optimize). PCRE has extended the
  regular expression syntax between 7.6 and 8.33, why this imposes a potential
  incompatibility. Only very complicated regular expressions may be affected,
  but if you know you are using obscure features, please test run your regular
  expressions and verify that their behavior has not changed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11204

- Filenames containing UTF-8 encoded characters can now be handled by erlc.

  If you have set the `ERLC_EMULATOR` environment variable, note that `erlc` in
  OTP 17 will only work with `erl` in OTP 17 since the protocol between the
  `erlc` program and the `erl_compile` module has changed.

  Own Id: OTP-11248

- By giving --enable-static-\{nifs,drivers\} to configure it is now possible to
  statically linking of nifs and drivers to the main Erlang VM binary. At the
  moment only the asn1 and crypto nifs of the Erlang/OTP nifs and drivers have
  been prepared to be statically linked. For more details see the Installation
  Guide in the System documentation.

  Own Id: OTP-11258

- Erlang/OTP has been ported to the realtime operating system OSE. The port
  supports both smp and non-smp emulator. For details around the port and how to
  started see the User's Guide in the _ose_ application.

  Note that not all parts of Erlang/OTP has been ported.

  Notable things that work are: non-smp and smp emulators, OSE signal
  interaction, crypto, asn1, run_erl/to_erl, tcp, epmd, distribution and most if
  not all non-os specific functionality of Erlang.

  Notable things that does not work are: udp/sctp, os_mon, erl_interface,
  binding of schedulers.

  Own Id: OTP-11334

- Add the \{active,N\} socket option for TCP, UDP, and SCTP, where N is an
  integer in the range -32768..32767, to allow a caller to specify the number of
  data messages to be delivered to the controlling process. Once the socket's
  delivered message count either reaches 0 or is explicitly set to 0 with
  inet:setopts/2 or by including \{active,0\} as an option when the socket is
  created, the socket transitions to passive (\{active, false\}) mode and the
  socket's controlling process receives a message to inform it of the
  transition. TCP sockets receive \{tcp_passive,Socket\}, UDP sockets receive
  \{udp_passive,Socket\} and SCTP sockets receive \{sctp_passive,Socket\}.

  The socket's delivered message counter defaults to 0, but it can be set using
  \{active,N\} via any gen_tcp, gen_udp, or gen_sctp function that takes socket
  options as arguments, or via inet:setopts/2. New N values are added to the
  socket's current counter value, and negative numbers can be used to reduce the
  counter value. Specifying a number that would cause the socket's counter value
  to go above 32767 causes an einval error. If a negative number is specified
  such that the counter value would become negative, the socket's counter value
  is set to 0 and the socket transitions to passive mode. If the counter value
  is already 0 and inet:setopts(Socket, \[\{active,0\}]) is specified, the
  counter value remains at 0 but the appropriate passive mode transition message
  is generated for the socket.

  Thanks to Steve Vinoski

  Own Id: OTP-11368

- A new optional scheduler utilization balancing mechanism has been introduced.
  For more information see the [`+sub`](erl_cmd.md#%2Bsub) command line
  argument.

  Characteristics impact: None, when not enabled. When enabled, changed timing
  in the system, normally a small overhead due to measuring of utilization and
  calculating balancing information. On some systems, such as old Windows
  systems, the overhead can be quite substantial. This time measurement overhead
  highly depend on the underlying primitives provided by the OS.

  Own Id: OTP-11385

- A call to either the [`garbage_collect/1`](`garbage_collect/1`) BIF or the
  [`check_process_code/2`](`check_process_code/2`) BIF may trigger garbage
  collection of another processes than the process calling the BIF. The previous
  implementations performed these kinds of garbage collections without
  considering the internal state of the process being garbage collected. In
  order to be able to more easily and more efficiently implement yielding native
  code, these types of garbage collections have been rewritten. A garbage
  collection like this is now triggered by an asynchronous request signal, the
  actual garbage collection is performed by the process being garbage collected
  itself, and finalized by a reply signal to the process issuing the request.
  Using this approach processes can disable garbage collection and yield without
  having to set up the heap in a state that can be garbage collected.

  The [`garbage_collect/2`](`erlang:garbage_collect/2`), and
  [`check_process_code/3`](`erlang:check_process_code/3`) BIFs have been
  introduced. Both taking an option list as last argument. Using these, one can
  issue asynchronous requests.

  `code:purge/1` and `code:soft_purge/1` have been rewritten to utilize
  asynchronous `check_process_code` requests in order to parallelize work.

  Characteristics impact: A call to the
  [`garbage_collect/1`](`garbage_collect/1`) BIF or the
  [`check_process_code/2`](`check_process_code/2`) BIF will normally take longer
  time to complete while the system as a whole won't be as much negatively
  effected by the operation as before. A call to `code:purge/1` and
  `code:soft_purge/1` may complete faster or slower depending on the state of
  the system while the system as a whole won't be as much negatively effected by
  the operation as before.

  Own Id: OTP-11388 Aux Id: OTP-11535, OTP-11648

- Cleanup 'Buckets' and 'Time left' fields in crashdump to ease parsing.

  Own Id: OTP-11419

- Add sync option to file:open/2.

  The sync option adds the POSIX O_SYNC flag to the open system call on
  platforms that support the flag or its equivalent, e.g.,
  FILE_FLAG_WRITE_THROUGH on Windows. For platforms that don't support it,
  file:open/2 returns \{error, enotsup\} if the sync option is passed in. Thank
  to Steve Vinoski and Joseph Blomstedt

  Own Id: OTP-11498

- erlang:binary_to_term will now cost an appropriate amount of reductions and
  will interrupt (yield) for reschedule if the term is big. This avoids too long
  schedules when binary_to_term is used. (Thanks to Svante Karlsson for the
  original patch)

  Impact: Programs running binary_to_term on large binaries will run more
  smoothly, but rescheduling will impact the single process performance of the
  BIF. Single threaded benchmarks might show degraded performance of the BIF,
  while general system behaviour will be improved.

  Own Id: OTP-11535 Aux Id: OTP-11388

- Added high resolution icon for windows. (Thanks to Daniel Goertz for the
  inspiration.)

  Own Id: OTP-11560

- Migration of memory carriers has been enabled by default on all ERTS internal
  memory allocators based on the [`alloc_util`](erts_alloc.md#alloc_util)
  framework except for `temp_alloc`. That is,
  [`+M<S>acul de`](erts_alloc.md#M_acul) is default for these allocators. Note
  that this also implies changed allocation strategies for all of these
  allocators. They will all now use the "address order first fit carrier best
  fit" strategy.

  By passing `+Muacul 0` on the command line, all configuration changes made by
  this change will be reverted.

  Characteristics impact: Improved memory characteristics with a smaller memory
  footprint at the expense of a quite small performance cost.

  Own Id: OTP-11604 Aux Id: OTP-10279

- A clarification has been added to the documentation of `-on_load()` in the
  Reference Manual that it is only recommended for loading NIF libraries.

  Own Id: OTP-11611

- `+fnaw` is now default when starting the emulator; it used to be `+fnl`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11612

- EEP43: New data type - Maps

  With Maps you may for instance:

  - \_\_\_\_ - `M0 = #{ a => 1, b => 2}, % create associations`

  - \_\_\_\_ - `M1 = M0#{ a := 10 }, % update values`

  - \_\_\_\_ - `M2 = M1#{ "hi" => "hello"}, % add new associations`

  - \_\_\_\_ - `#{ "hi" := V1, a := V2, b := V3} = M2. % match keys with values`

  For information on how to use Maps please see Map Expressions in the
  [Reference Manual](`e:system:expressions.md#map-expressions`).

  The current implementation is without the following features:

  - \_\_\_\_ - No variable keys

  - \_\_\_\_ - No single value access

  - \_\_\_\_ - No map comprehensions

  Note that Maps is _experimental_ during OTP 17.0.

  Own Id: OTP-11616

- The previously deprecated driver API function `driver_async_cancel()` has been
  removed. Due to this, the driver API version has been bumped to 3.0.

  Thanks to Steve Vinoski.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11628

- Experimental "dirty scheduler" functionality has been introduced. In order to
  try the functionality out, you need to pass the command line argument
  `--enable-dirty-schedulers` to `configure` when building the system.

  Dirty schedulers can currently only be used by NIFs on a system with SMP
  support. More information can be found in the
  [`erl_nif(3)`](erl_nif.md#dirty_nifs) documentation, the
  [`erl(1)`](erl_cmd.md) documentation, and in the git commit comment of commit
  'c1c03ae4ee50e58b7669ea88ec4d29c6b2b67c7b'.

  Note that the functionality is _experimental_, and _not supported_. This
  functionality _will_ be subject to backward incompatible changes. You should
  _not_ enable the dirty scheduler functionality on production systems. It is
  only provided for testing.

  Thanks to Steve Vinoski.

  Own Id: OTP-11629

- Improve reduction cost and yielding of `term_to_binary`. The reduction cost is
  increased and garbage collection is disabled during yield.

  Impact: Improves system responsiveness when `term_to_binary` is called with
  large terms without significant degradation of single threaded performance.

  Own Id: OTP-11648 Aux Id: OTP-11388

- By default, the system's version of zlib will be used, provided its version is
  1.2.4 or higher; otherwise the built-in zlib will be used. The built-in
  version of zlib has been bumped to 1.2.8. (Use the `--enable-builtin-zlib`
  option to `configure` to force the use of the built-in zlib.)

  Own Id: OTP-11669

- The default float encoding in binary_to_term and external_size has been
  changed to use minor_mode 1 instead of 0.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11738

- Introduced the `configure` option `--with-assumed-cache-line-size=SIZE`. For
  more information see [`$ERL_TOP/HOWTO/INSTALL.md`](`e:system:install.md`).

  Own Id: OTP-11742

- Halfword emulator is marked as deprecated. It still works as before but is
  planned to be removed in a future major release.

  Own Id: OTP-11777

- The external format for Maps has changed in a way that is not compatible with
  the format used in OTP 17.0-rc1 and OTP 17.0-rc2.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11782

- Fixed faulty make dependency that would make some make versions fail while
  building gen_git_version.mk.

  Own Id: OTP-11784

- Introduced functionality for allowing old drivers and NIF libraries to be
  loaded during a transition period. For more information see
  [the version management section in the `erl_driver(3)` documentation](erl_driver.md#version_management)
  and
  [the version management section in the `erl_nif(3)` documentation](erl_nif.md#version_management).

  Own Id: OTP-11799

- Support file paths longer than 259 characters on Windows. Long absolute paths
  are automatically converted to UNC format with a `\\?\` prefix which is the
  only way to represent long paths. The 259 character limit still applies for
  individual file names, relative paths and the current working directory.

  Own Id: OTP-11813

- Document that escript:create/2 also accepts a 3-elements tuple containing
  files and zip:create/3 options to build a zip file.

  Thanks to Pierre Fenoll

  Own Id: OTP-11827

- Add systemd socket activation for epmd.

  Thanks to Matwey V. Kornilov

  Own Id: OTP-11829

## Erts 5.10.4.1

### Known Bugs and Problems

- When using gen_tcp:connect and the `fd` option with `port` and/or `ip`, the
  `port` and `ip` options were ignored. This has been fixed so that if `port`
  and/or `ip` is specified together with `fd` a bind is requested for that `fd`.
  If `port` and/or `ip` is not specified bind will not be called.

  Own Id: OTP-12061

## Erts 5.10.4

### Fixed Bugs and Malfunctions

- When normalizing paths, erl_prim_loader would always convert backslash to
  forward slash. This is correct on Windows, but not on other operating systems.
  erl_prim_loader now checks which OS is running before performing this
  conversion.

  Own Id: OTP-11170

- Fixed syslog defines and defined LOG_ERR for systems without syslog.h. Thanks
  to Matt Lewandowsky.

  Own Id: OTP-11349

- Check all pattern arguments passed to binary:matches/2. Thanks to Mike Sassak.

  Own Id: OTP-11350

- Fix two small silent rules omissions. Thanks to Anthony Ramine.

  Own Id: OTP-11351

- Teach configure to detect if posix_memalign cannot align to more than the
  system page size.

  For cross-compiled systems a new environment variable called
  erl_xcomp_posix_memalign has been introduced to indicate whether
  posix_memalign should be used.

  Own Id: OTP-11371

- Fix bsr bug occurring when shifting a huge number a huge number of bits to the
  right. Thanks to Lars Hesel Christensen.

  Own Id: OTP-11381

- Fix memory leak for distributed monitors

  Own Id: OTP-11410

- Fix various typos in erts, kernel and ssh. Thanks to Martin Hässler.

  Own Id: OTP-11414

- Crashdumps initiated by out-of-memory on process spawn could cause the beam to
  segfault during crashdump writing due to invalid pointers.

  The pointers are invalid since the process creation never finished. This fix
  removes these processes from the printouts. Reported by Richard Carlsson.

  Own Id: OTP-11420

- Crash dumps from 64-bit Erlang machines would have all memory addresses
  truncated to 32 bits, which could cause trouble inspecting processes message
  queues and stacks in the crashdump viewer.

  Own Id: OTP-11450

- Threads other than schedulers threads could make thread unsafe accesses when
  support for migration of memory carriers had been enabled, i.e., when the
  [`+M<S>acul`](erts_alloc.md#M_acul) command line flag had been passed to
  [`erl`](erl_cmd.md). This could cause corruption of the VMs internal state.

  This bug was introduced in erts-5.10.2 when the support for migration of
  memory carriers was introduced.

  Own Id: OTP-11456 Aux Id: OTP-10279

- Fix bug in `binary_to_term` for invalid bitstrings and very large binaries
  (>2Gb).

  Own Id: OTP-11479

- Under rare circumstances a process calling `inet:close/1`, `gen_tcp:close/1`,
  `gen_udp:close/1`, or `gen_sctp:close/1` could hang in the call indefinitely.

  Own Id: OTP-11491

- Fix bug that could cause a 32-bit emulator to always crash at start (since
  R16B01) depending on the alignment of static data in the beam executable.

  Own Id: OTP-11496

- Fix benign bugs regarding bitstring compare. Only a nuisance for debug and
  valgrind VM.

  Own Id: OTP-11501

- Silence warnings (Thanks to Anthony Ramine)

  Own Id: OTP-11517

- The default wordsize of the emulator (beam) is now determined by compiler
  default on Mac OSX (Darwin). This was previously forced to 32bits by the
  configure script unless otherwise specified.

  Own Id: OTP-11521

### Improvements and New Features

- A new memory allocation feature called "super carrier" has been introduced.
  The super carrier feature can be used in different ways. It can for example be
  used for pre-allocation of all memory that the runtime system should be able
  to use.

  By default the super carrier is disabled. It is enabled by passing the
  [`+MMscs <size in MB>`](erts_alloc.md#MMscs) command line argument. For more
  information see the documentation of the [`+MMsco`](erts_alloc.md#MMsco),
  [`+MMscrfsd`](erts_alloc.md#MMscrfsd), [`+MMscrpm`](erts_alloc.md#MMscrpm),
  [`+MMscs`](erts_alloc.md#MMscs), [`+MMusac`](erts_alloc.md#Musac), and,
  [`+Mlpm`](erts_alloc.md#Mlpm) command line arguments in the
  [`erts_alloc(3)`](erts_alloc.md) documentation.

  Since it is disabled by default there should be no impact on system
  characteristics if not used.

  This change has been marked as a potential incompatibility since the returned
  list when calling
  [`erlang:system_info({allocator, mseg_alloc})`](`m:erlang#system_info_allocator_tuple`)
  now also include an `{erts_mmap, _}` tuple as one element in the list.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11149

- Added erlang:system_info(ets_limit) to provide a way to retrieve the runtime's
  maximum number of ETS tables. Thanks to Steve Vinoski

  Own Id: OTP-11362

- Add new BIF os:unsetenv/1 which deletes an environment variable. Thanks to
  Martin Hässler.

  Own Id: OTP-11446

- Introduced a new guarantee regarding exit signals from ports:

  If the process calling one of the synchronous port BIFs listed below is linked
  to the port identified by the first argument, and the port exits before
  sending the result of the port operation, the exit signal issued due to this
  link will be received by the processes before the BIF returns, or fail with an
  exception due to the port not being open.

  The synchronous port BIFs are:

  - [`port_close/1`](`erlang:port_close/1`)
  - [`port_command/2`](`erlang:port_command/2`)
  - [`port_command/3`](`erlang:port_command/3`)
  - [`port_connect/2`](`erlang:port_connect/2`)
  - [`port_control/3`](`erlang:port_control/3`)
  - `erlang:port_call/3`
  - `erlang:port_info/1`
  - `erlang:port_info/2`

  Note that some ports under certain circumstances unlink themselves from the
  calling process before exiting, i.e. even though the process linked itself to
  the port there might be no link triggering an exit signal.

  Characteristics impact: The return or exception from the synchronous port BIF
  will be delayed if the port simultaneously exit due to some issue unrelated to
  the outstanding synchronous port BIF call. In all other cases characteristics
  are unchanged.

  Own Id: OTP-11489

## Erts 5.10.3.1

### Improvements and New Features

- Memory allocators will be able to create `sys_alloc` carriers as fallback, if
  `mseg_alloc` cannot create more carriers, on systems with `posix_memalign()`
  support. This is similar to how it worked in pre-R16 releases.

  Windows systems will create carriers using `_aligned_malloc()` and can by this
  use the new optimized allocator header scheme introduced in R16 on other
  platforms.

  Own Id: OTP-11318

## Erts 5.10.3

### Fixed Bugs and Malfunctions

- The documentation of predefined types and built-in types has been corrected.

  Own Id: OTP-11090

- Fix changing terminal parameters in to_erl

  Change the behaviour of to_erl to use TCSADRAIN instead of TCSANOW when
  changing terminal parameters. This makes the serial driver wait for the output
  queues to be empty before applying the terminal parameter change. Thanks to
  Stefan Zegenhagen.

  Own Id: OTP-11206

- The default value of \{flush, boolean()\} in erlang:halt/2 is documented to be
  'true' if the status is an integer. The implementation behaviour was reversed.
  The Implementation is now corrected to adhere to the documentation. Thanks to
  Jose Valim for reporting the error.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11218

- Fix serious race bug in R16B01 that could cause PID mix-ups when a lot of
  processes were spawned and terminated in a very rapid pace on an SMP emulator
  with at least two scheduler threads.

  Own Id: OTP-11225

- Validating a trace pattern with the option silent no longer incorrectly
  enables/disables the silent option of the calling process.

  Own Id: OTP-11232

- Fixed a bug where GCC 4.8 and later use a more aggressive loop optimization
  algorithm that broke some previously working code in the efile driver. Thanks
  to Tomas Abrahamsson for reporting this issue.

  Own Id: OTP-11246

- Fixed bug when printing memory allocator acul option in crash dump.

  Own Id: OTP-11264

- Opening a new compressed file on Windows could in rare (random) cases result
  in \{error,eisdir\} or other error codes although it should have succeeded.
  This is now corrected.

  Own Id: OTP-11265

- Fixed a race condition when closing a trace port that would cause the emulator
  to crash.

  Own Id: OTP-11290

### Improvements and New Features

- There is a new somewhat experimental socket option 'netns' that can set the
  network namespace for a socket on Linux:es where it is supported. See the
  documentation.

  Own Id: OTP-11157

- New allocator strategy `aoffcbf` (address order first fit carrier best fit).
  Supports carrier migration but with better CPU performance than `aoffcaobf`.

  Own Id: OTP-11174

- Introduced functionality for inspection of system and build configuration.

  Own Id: OTP-11196

- Fix matching of floating point middle-endian machines. Thanks to Johannes
  Weissl.

  Own Id: OTP-11201

- Fix compile error on ARM and GCC versions greater than 4.1.0. Thanks to
  Johannes Weissl.

  Own Id: OTP-11214

- run_erl: Redirect standard streams to /dev/null. Thanks to Johannes Weissl.

  Own Id: OTP-11215

- Misc. corrections in documentation for erl_driver. Thanks to Giacomo Olgeni.

  Own Id: OTP-11227

- Fix documentation regarding binary_part.

  Own Id: OTP-11239

- Make edlin understand a few important control keys. Thanks to Stefan
  Zegenhagen.

  Own Id: OTP-11251

- Export type zlib:zstream/0. Thanks to Loic Hoguin.

  Own Id: OTP-11278

- Add erl option to set schedulers by percentages.

  For applications where measurements show enhanced performance from the use of
  a non-default number of emulator scheduler threads, having to accurately set
  the right number of scheduler threads across multiple hosts each with
  different numbers of logical processors is difficult because the erl +S option
  requires absolute numbers of scheduler threads and scheduler threads online to
  be specified.

  To address this issue, add a +SP option to erl, similar to the existing +S
  option but allowing the number of scheduler threads and scheduler threads
  online to be set as percentages of logical processors configured and logical
  processors available, respectively. For example, "+SP 50:25" sets the number
  of scheduler threads to 50% of the logical processors configured, and the
  number of scheduler threads online to 25% of the logical processors available.
  The +SP option also interacts with any settings specified with the +S option,
  such that the combination of options "+S 4:4 +SP 50:50" (in either order)
  results in 2 scheduler threads and 2 scheduler threads online.

  Thanks to Steve Vinoski

  Own Id: OTP-11282

- Extend erl_driver interface with lock names

  Lock and thread names are already a feature in the driver interface. This
  extension will let developers read these names which eases debugging.

  Own Id: OTP-11303

- Fix incorrect values returned by integer_to_binary/2. Thanks to Juan Jose
  Comellas.

  Own Id: OTP-11311

- Fix system_flag scheduling_statistics - disable . Thanks to Steve Vinoski.

  Own Id: OTP-11317

- The documentation of predefined types has been corrected Thanks to Kostis
  Sagonas.

  Own Id: OTP-11321

## Erts 5.10.2

### Fixed Bugs and Malfunctions

- A bug in prim_inet has been corrected. If the port owner was killed at a bad
  time while closing the socket port the port could become orphaned hence
  causing port and socket leaking. Reported by Fred Herbert, Dmitry Belyaev and
  others.

  Own Id: OTP-10497 Aux Id: OTP-10562

- Compilation fixes for NetBSD. Thanks to YAMAMOTO Takashi.

  Own Id: OTP-10941

- Fixed a race condition when using delayed_write when writing to a file which
  would cause the same data to be written multiple times.

  Own Id: OTP-10984

- Fix small memory leak from tracing with option `meta`.

  Own Id: OTP-10997

- Correct typo in erlsrv usage. Thanks to Bryan Hunter

  Own Id: OTP-11002

- ct_run: delete unused function. Thanks to Tuncer Ayaz.

  Own Id: OTP-11003

- Corrections to run_erl/to_erl handshake behaviour.

  Own Id: OTP-11012

- Fix typo in type: erlang:process_info_item(). Thanks to Andrew Tunnell-Jones.

  Own Id: OTP-11024

- Fix src/dest overlap issue in ttsl driver. Thanks to Steve Vinoski.

  Own Id: OTP-11064

- When sending to a port using `erlang:send(Port, Msg, [nosuspend])`, the send
  operation was performed synchronously. This bug has now been fixed.

  Own Id: OTP-11076 Aux Id: OTP-10336

- When converting a faulty binary to a list with unicode:characters_to_list, the
  error return value could contain a faulty "rest", i.e. the io_list of
  characters that could not be converted was wrong. This happened only if input
  was a sub binary and conversion was from utf8. This is now corrected.

  Own Id: OTP-11080

- Runtime system could crash when reporting stale `driver_select()`.

  Own Id: OTP-11084

- Fix lock order violation for memory instrumentation (+Mim, +Mis, +Mit).

  Own Id: OTP-11085

- Fixed some compilation warnings on miscellaneous platforms. Thanks to Anthony
  Ramine.

  Own Id: OTP-11086

- Fixed issue when flushing i/o during shutdown on windows where the Erlang VM
  would sometime hang due to a race condition.

  Own Id: OTP-11096

- Fixed issue where repeated calls to erlang:nodes() could cause unnecessary
  contention in the dist_table lock.

  Own Id: OTP-11097

- Properly guard WIDE_TAG use with HAVE_WCWIDTH in ttsl_drv. Thanks to Anthony
  Ramine

  Own Id: OTP-11106

- Fix some Makefile rules that didn't support silent rules. Thanks to Anthony
  Ramine.

  Own Id: OTP-11111

- Fix receive support in erl_eval with a BEAM module. Thanks to Anthony Ramine.

  Own Id: OTP-11137

- erlang:now() could suddenly jump ~24 days into the future on Windows. This is
  now corrected. Thanks to Garret Smith for reporting and testing fixes.

  Own Id: OTP-11146

- erlang:term_to_binary will now cost an appropriate amount of reductions and
  will interrupt (yield) for reschedule if the term is big. This avoids too long
  schedules when term_to_binary is used.

  Impact: Programs running term_to_binary on large terms will run more smothly,
  but rescheduling will impact the single process performance of the BIF. Single
  threaded benchmarks will show degraded performance of the BIF when called with
  very large terms, while general system behaviour will be improved. The
  overhead for allowing restart and reduction counting also degrades local
  performance of the BIF with between 5% and 10% even for small terms.

  Own Id: OTP-11163

### Improvements and New Features

- Replaced the lock protecting gathering of garbage collection statistics with a
  lock-free solution.

  Own Id: OTP-10271 Aux Id: kunagi-108 \[04c5410f-9cc4-4696-8639-36bf98686c7a-7]

- Support for migration of memory carriers between memory allocator instances
  has been introduced.

  By default this feature is not enabled and do not effect the characteristics
  of the system. When enabled it has the following impact on the characteristics
  of the system:

  - Reduced memory footprint when the memory load is unevenly distributed
    between scheduler specific allocator instances.
  - Depending on the default allocation strategy used on a specific allocator
    there might or might not be a slight performance loss.
  - When enabled on the `fix_alloc` allocator, a different strategy for
    management of fix blocks will be used.
  - The information returned from
    [`erlang:system_info({allocator, A})`](`m:erlang#system_info_allocator_tuple`),
    and
    [`erlang:system_info({allocator_sizes, A})`](`m:erlang#system_info_allocator_sizes`)
    will be slightly different when this feature has been enabled. An
    `mbcs_pool` tuple will be present giving information about abandoned
    carriers, and in the `fix_alloc` case no `fix_types` tuple will be present.

  For more information, see the documentation of the
  [`+M<S>acul`](erts_alloc.md#M_acul) command line argument.

  Own Id: OTP-10279

- Change specs for spawn_opt to use the process_level() type declaration instead
  of re-defining it in various places. Thanks to Kostis Sagonas.

  Own Id: OTP-11008

- Postscript files no longer needed for the generation of PDF files have been
  removed.

  Own Id: OTP-11016

- Erlang source files with non-ASCII characters are now encoded in UTF-8
  (instead of latin1).

  Own Id: OTP-11041 Aux Id: OTP-10907

- Optimization of simultaneous `inet_db` operations on the same socket by using
  a lock free implementation.

  Impact on the characteristics of the system: Improved performance.

  Own Id: OTP-11074

- The `high_msgq_watermark` and `low_msgq_watermark` `inet` socket options
  introduced in OTP-R16A could only be set on TCP sockets. These options are now
  generic and can be set on all types of sockets.

  Own Id: OTP-11075 Aux Id: OTP-10336

- A new better algorithm for management of the process, and port tables has been
  introduced.

  Impact on the characteristics of the system:

  - The new algorithm ensures that both insert and delete operations can be made
    in O(1) time complexity. Previously used algorithm either caused insert or
    delete to be O(N).
  - The new algorithm will also ensure that reuse of identifiers will be less
    frequent than when the old algorithm was used.
  - Previously used algorithm ensured that the latest created identifier
    compared as the largest when comparing two identifiers of the same type that
    had been created on the same node as long as no identifiers had been reused.
    Since identifiers can be reused quite fast, one has never been able to rely
    on this property. Due to the introduction of this new algorithm this
    property will not hold even if no identifiers has been reused yet. This
    could be considered as an incompatibility.

  Due to the above mensioned potential incompatibility, it will still be
  possible to enable the old algorithm for some time. The command line argument
  [`+P legacy`](erl_cmd.md#%2BP) will enable the old algorithm on the process
  table, and [`+Q legacy`](erl_cmd.md#%2BQ) will do the same for the port table.
  These command line arguments are however deprecated as of their introduction
  and have been scheduled for removal in OTP-R18.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11077

- Support wide characters in the shell through wcwidth(). Thanks to Anthony
  Ramine. Reported by Loïc Hoguin.

  Own Id: OTP-11088

- Added total used memory for each process in erlang crash dumps.

  Own Id: OTP-11098

- Added support for hipe on Raspberry Pi (armv6l). Thanks to Klaus Alfert.

  Own Id: OTP-11125

- Remove 'query' from the list of reserved words in docs. Thanks to Matthias
  Endler and Loïc Hoguin.

  Own Id: OTP-11158

- Lift static limitation (FD_SETSIZE) for file descriptors on Mac OS X. (Thanks
  to Anthony Ramine)

  Own Id: OTP-11159

### Known Bugs and Problems

- Miscellaneous native code in OTP misbehave either due to lengthy execution, or
  due to not bumping reductions properly. Problems typically occur when passing
  huge sets of data to a misbehaving BIF. Fixing this has high priority and is
  being worked on, but there will remain issues like this for some time.

  In order to alleviate problems with scheduling which might occur when
  executing misbehaving native code, the command line argument
  [\+sfwi](erl_cmd.md#%2Bsfwi) has been introduced.

  By default this feature is disabled and you are advised not to enable it if
  you do not encounter problems with misbehaving native code.

  When enabled it has the following impact on the characteristics of the system:

  - Work will always be distributed between schedulers even when executing
    misbehaving native code.
  - It may cause an increased amount of processes and/or ports bouncing between
    schedulers which in turn will cause a performance loss.
  - It may cause reduced performance due to reduced or lost work compaction when
    all schedulers do not execute under full load.
  - An increased contention on run queue locks.

  Own Id: OTP-11164

## Erts 5.10.1.2

### Fixed Bugs and Malfunctions

- A bug in the implementation of offline schedulers has been fixed. The bug was
  introduced in OTP-R16A/ERTS-5.10, and caused work-stealing between schedulers
  to fail. This in turn, caused work to accumulate in some run-queues. The bug
  was only triggered when there were offline schedulers in the system, i.e.,
  when the amount of online schedulers was less than the total amount of
  schedulers. The effect of the bug got more severe the larger amount of offline
  schedulers the system had.

  Own Id: OTP-11022 Aux Id: OTP-9892

## Erts 5.10.1.1

### Fixed Bugs and Malfunctions

- The BIF [`is_process_alive/1`](`erlang:is_process_alive/1`) could prematurely
  return `false` while the process being inspected was terminating. This bug was
  introduced in ERTS-5.10.

  Own Id: OTP-10926

- Fix a problem in `erlang:delete_element/2` where the call could corrupt one
  word of stack if the heap and stack met during call.

  Own Id: OTP-10932

- The [`+sws<value>`](erl_cmd.md#%2Bsws) and [`+swt<value>`](erl_cmd.md#%2Bswt)
  system flags failed if no white space were passed between the parameter and
  value parts of the flags. Upon failure, the runtime system refused to start.

  Own Id: OTP-11000

### Improvements and New Features

- Scheduler threads will now by default be less eager requesting wakeup due to
  certain cleanup operations. This can also be controlled using the
  [`+swct`](erl_cmd.md#%2Bswct) command line argument of [`erl(1)`](erl_cmd.md).

  Own Id: OTP-10994

## Erts 5.10.1

### Fixed Bugs and Malfunctions

- Threads created internally in the runtime system by vanilla, fd, and spawn
  drivers on Windows systems could make thread unsafe calls to
  `driver_select()`.

  Own Id: OTP-10802

- Threads created internally in the runtime system by the vanilla, fd, and spawn
  drivers on Windows systems could make unsafe memory accesses to driver data
  after port had terminated.

  Own Id: OTP-10803

- The runtime system could crash when flushing data to standard out or standard
  error on Windows.

  Own Id: OTP-10807

- Bugs due to the port optimizations introduced in erts-5.10/OTP-R16A have been
  fixed:

  - Memory leak when terminating ports
  - Memory leak when reaching the system limit of maximum amount of concurrently
    existing ports
  - Crashes due to missing, or late test of bad port handle
  - The newly introduced driver API function `erl_drv_busy_msgq_limits()` could
    not be used by dynamically linked in drivers on Windows

  Own Id: OTP-10809 Aux Id: OTP-10336

- Fix `{packet,httph}` header capitalization for unrecognized header fields
  longer than 20 characters such as `Sec-Websocket-Version`. The limit is simply
  raised from 20 to 50 characters with the hope that valid headers longer than
  50 are not used.

  Own Id: OTP-10824

- Fix rounding issues in float_to_list/1,2. Thanks to Serge Aleynikov

  Own Id: OTP-10837

- Fix memory leak in file driver introduced in R16A.

  Own Id: OTP-10841

- A bug in an ERTS internal queue implementation could cause the loss of a wake
  up signal to a consumer thread. This has now been fixed.

  The effect of this bug, when triggered, was often only a small or even no
  delay of certain operations. This since, threads often are woken due to other
  unrelated reasons. However, if the consumer thread was not woken due to other
  reasons when the bug was triggered, these operations could be left hanging,
  potentially for ever. Such effects seems to have been very rare, but we have
  on at least one occasion gotten a report about such an issue.

  Operations potentially effected by this bug:

  - **Inspection of memory allocation status** - The Erlang process calling
    `erlang:memory/[0,1]`, or
    `erlang:system_info({allocator|allocator_sizes, _})` could potentially hang
    waiting for responses from involved threads.

  - **Async thread pool jobs** - An async thread pool job request and/or reply
    could potentially be left hanging. In OTP this only effected file
    operations, but user implemented drivers using the async thread pool were
    also effected. In the file operation case, this would typically translate
    into an Erlang process potentially hanging on the file operation.

  - **Shutting down the runtime system** - Due to the issue with the async
    thread pool mentioned above, flushing of I/O while terminating the runtime
    system could also potentially hang.

  - **ETS memory deallocation** - Scheduled jobs handling deallocation of the
    main structure of an ETS table could potentially hang. This more or less
    only translates into minor memory leaks.

  - **Shutting down distribution** - The distribution shutdown phase used when
    manually shutting down the distribution, i.e., when calling
    `net_kernel:stop()`, could potentially hang.

  Own Id: OTP-10854

- OS X Snow Leopard now only uses write, as writev does not work properly on
  very large files.

  Own Id: OTP-10858

- Fixed a bug where line oriented file I/O using read_ahead was very slow for
  files with very large difference in line length.

  Own Id: OTP-10859

- In erts-5.10 (R16A) faulty hashvalues were produced for non-ASCII atoms
  (characters in byte-range 128..255). This has now been fixed and hashvalues
  conforms to previous OTP releases.

  Own Id: OTP-10860

- Fixes of memory accesses that might be thread unsafe when the runtime system
  has been linked against third-party libraries for atomic memory operations
  during the build. Most builds are uneffected by this bug. If triggered, the
  runtime system will most likely crash more or less immediately.

  Own Id: OTP-10875 Aux Id: OTP-10854

- Fixed a bug where it was longer possible to give the +sws proposal flag to
  non-smp emulators.

  Own Id: OTP-10881 Aux Id: seq12258

- Faulty type to bytes read for ReadFile on Windows. This could cause windows
  systems to misbehave. The correct type is now used.

  Own Id: OTP-10890

- Change default max ports for Windows to 8192. Having a too large value caused
  Windows to not be able to recover properly. If you want to use another value,
  pass `+Q Value` to erl or werl.

  Own Id: OTP-10892

- Fix rare crash on halfword vm during code loading.

  Own Id: OTP-10896

### Improvements and New Features

- Tuple funs (deprecated in R15B) are no longer supported.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-10170

- Added four new bifs, `erlang:binary_to_integer/1,2`,
  `erlang:integer_to_binary/1`, `erlang:binary_to_float/1` and
  `erlang:float_to_binary/1,2`. These bifs work similarly to how their list
  counterparts work, except they operate on binaries. In most cases converting
  from and to binaries is faster than converting from and to lists.

  These bifs are auto-imported into erlang source files and can therefore be
  used without the `erlang` prefix.

  Own Id: OTP-10300 Aux Id: kunagi-74 \[74]

- The experimental support for packages has been removed.

  Own Id: OTP-10348 Aux Id: kunagi-316 \[227]

- The driver API function
  [`erl_drv_consume_timeslice()`](erl_driver.md#erl_drv_consume_timeslice), and
  the NIF API function
  [`enif_consume_timeslice()`](erl_nif.md#enif_consume_timeslice) have been
  introduced.

  These functions are provided in order to better support co-operative
  scheduling, improve system responsiveness, and to make it easier to prevent
  misbehaviors of the VM due to a process or port monopolizing a scheduler
  thread. They can be used when dividing lengthy work into a number of repeated
  calls without the need to use threads.

  Own Id: OTP-10810

- The list_to_integer/2 bif has been optimized when used with bases other
  than 10.

  Own Id: OTP-10834 Aux Id: kunagi-74 \[74]

- The git commit sha of the HEAD commit is now added to the Erlang shell when
  compiling a non-released Erlang version.

  Own Id: OTP-10838

- Change caching policy for memory segment allocator. For instance, prefer sbc
  segments over mbc segments, caching policy is time-arrow aware, evicting older
  cached segments to store newer segments.

  The default number of cacheable segment has been increased from five to ten
  segments. This can be modified, same as before, with the command line option
  `+MMmcs 5`

  Impact: Increased speed for processing on larger objects, e.g. binaries.
  Slight increase of mapped and resident memory. Tune your system with memory
  options to `erl` for best performance.

  Own Id: OTP-10840

- Updated config.sub and config.guess to latest version from gnu.org

  Own Id: OTP-10848

- Add an xcomp file for Blue Gene/Q. Thanks to Kostis Sagonas.

  Own Id: OTP-10849

- Cleanup of documentation of the type language. Thanks to Kostis Sagonas.

  Own Id: OTP-10850

- Change the return value of hipe_bifs:remove_refs_from/1. Thanks to Kostis
  Sagonas.

  Own Id: OTP-10851

- As of ERTS-5.10/OTP-R16A node names passed in the EPMD protocol are required
  to be encoded in UTF-8. Since EPMD previously accepted latin1 encoded node
  names this is an incompatibility. However, since Erlang nodes always have
  required characters in node names to be 7-bit ASCII characters (and still do
  require this), this incompatibility should not effect anyone using EPMD as an
  Erlang Port Mapper Daemon.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-10872 Aux Id: OTP-10753

- The +pc flag to erl can be used to set the range of characters considered
  printable. This affects how the shell and io:format("~tp",...) functionality
  does heuristic string detection. More can be read in STDLIB users guide.

  Own Id: OTP-10884

- Fix a number of type cast errors related to formatted printing on Win64 that
  can potentially cause problem when the Erlang VM exceeds 4 GB of ram. (Thanks
  to Blaine Whittle for the original patch)

  Own Id: OTP-10887

- The effect of the deprecated environment variable ERL_MAX_PORTS had been
  removed prematurely. It has now been re-added. Note that this is still
  scheduled to be released in R17B.

  Own Id: OTP-10895

## Erts 5.10

### Fixed Bugs and Malfunctions

- Set new peeled off SCTP socket to nonblocking socket (Thanks to Jonas
  Falkevik)

  Own Id: OTP-10491

- Fix various typos (thanks to Tuncer Ayaz)

  Own Id: OTP-10611

- Fix fd leak when using async thread pool

  When using the async thread pool, if an erlang process asks to open a file and
  it gets shutdown/killed while the file:open/2 call hasn't returned, it's
  possible to leak a file descriptor against the target file. This has now been
  fixed. (Thanks to Filipe David Manana)

  Own Id: OTP-10677

- Use sys/types.h instead of string.h to pull ssize_t definition to
  erl_driver.h. This fixes build issue on NetBSD. (Thanks to Yamamoto Takashi).

  Own Id: OTP-10699

- Arguments given with the -run or -s flags to erl are now translated according
  to the file name encoding mode of the runtime system.

  Own Id: OTP-10702

- The octet counters in the gen_tcp/inet interface could behave in unexpected
  ways on 64bit platforms. The behaviour is now as expected.

  Own Id: OTP-10746

- Certain linux kernels, most notably in redhat and CentOS distribution, had a
  bug in writev which generated an infinite loop in the tcp code of the VM. The
  bug is now worked around.

  Own Id: OTP-10747

- A process that got killed (got an exit signal) while operating on a
  compresseed file, could cause a segmentation fault in the VM. This is now
  corrected. Thanks to Filipe David Manana for identifying the problem and
  submitting a solution.

  Own Id: OTP-10748

- Windows previously used three digit exponent in formatting which caused
  difference between platforms, as can be seen by float_to_list/1. This has now
  been fixed.

  Own Id: OTP-10751

### Improvements and New Features

- A boolean socket option 'ipv6_v6only' for IPv6 sockets has been added. The
  default value of the option is OS dependent, so applications aiming to be
  portable should consider using `{ipv6_v6only,true}` when creating an `inet6`
  listening/destination socket, and if necessary also create an `inet` socket on
  the same port for IPv4 traffic. See the documentation.

  Own Id: OTP-8928 Aux Id: kunagi-193 \[104]

- It is now allowed to define stubs for BIFs, to allow type specs to be written
  for BIFs. For example, if there is BIF called `lists:member/2`, a dummy
  definition of `lists:member/2` is now allowed.

  Own Id: OTP-9861

- Process optimizations. The most notable:

  - New internal process table implementation allowing for both parallel reads
    as well as writes. Especially read operations have become really cheap. This
    reduce contention in various situations. For example when, spawning
    processes, terminating processes, sending messages, etc.
  - Optimizations of run queue management reducing contention.
  - Optimizations of process internal state changes reducing contention.

  These changes imply changes of the characteristics the system. Most notable:
  changed timing in the system.

  Own Id: OTP-9892 Aux Id: OTP-10167

- Non-blocking code loading. Earlier when an Erlang module was loaded, all other
  execution in the VM were halted while the load operation was carried out in
  single threaded mode. Now modules are loaded without blocking the VM.
  Processes may continue executing undisturbed in parallel during the entire
  load operation. The load operation is completed by making the loaded code
  visible to all processes in a consistent way with one single atomic
  instruction. Non-blocking code loading will improve realtime characteristics
  when modules are loaded/upgraded on a running SMP system.

  Own Id: OTP-9974

- In the SMP emulator, turning on and off tracing will no longer take down the
  system to single-scheduling.

  Own Id: OTP-10122

- Remove VxWorks support

  Own Id: OTP-10146

- Added a general framework for executing benchmarks of Erlang/OTP. Benchmarks
  for the Erlang VM and mnesia have been incorporated in the framework.

  For details about how to add more benchmarks see $ERL_TOP/HOWTO/BENCHMARKS.md
  in the source distribution.

  Own Id: OTP-10156

- Optimized deletion of ETS-tables which significantly improves performance when
  large amounts of temporary tables are used.

  This change imply changes of the characteristics the system. Most notable:
  changed timing in the system.

  Own Id: OTP-10167 Aux Id: OTP-9892

- Tuple funs (deprecated in R15B) are no longer supported.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-10170

- New internal header scheme for allocators

  Impact: Reduces size on object allocated in multiblock carriers by one word

  Own Id: OTP-10273 Aux Id: kunagi-20 \[20]

- Major port improvements. The most notable:

  - New internal port table implementation allowing for both parallel reads as
    well as writes. Especially read operations have become really cheap.This
    reduce contention in various situations. For example when, creating ports,
    terminating ports, etc.
  - Dynamic allocation of port structures. This allow for a much larger maximum
    amount of ports allowed as a default. The previous default of 1024 has been
    raised to 65536. Maximum amount of ports can be set using the
    [\+Q](erl_cmd.md#%2BQ) command line flag of [erl(1)](erl_cmd.md). The
    previously used environment variable `ERL_MAX_PORTS` has been deprecated and
    scheduled for removal in OTP-R17.
  - Major rewrite of scheduling of port tasks. Major benefits of the rewrite are
    reduced contention on run queue locks, and reduced amount of memory
    allocation operations needed. The rewrite was also necessary in order to
    make it possible to schedule signals from processes to ports.
  - Improved internal thread progress functionality for easy management of
    unmanaged threads. This improvement was necessary for the rewrite of the
    port task scheduling.
  - Rewrite of all process to port signal implementations in order to make it
    possible to schedule those operations. All port operations can now be
    scheduled which allows for reduced lock contention on the port lock as well
    as truly asynchronous communication with ports.
  - Optimized lookup of port handles from drivers.
  - Optimized driver lookup when creating ports.
  - Preemptible `erlang:ports/0` BIF.
  - Improving responsiveness by bumping reductions for a process calling a
    driver callback directly.

  These changes imply changes of the characteristics of the system. The most
  notable:

  - **Order of signal delivery** - The previous implementation of the VM has
    delivered signals from processes to ports in a synchronous stricter fashion
    than required by the language. As of ERTS version 5.10, signals are truly
    asynchronously delivered. The order of signal delivery still adheres to the
    requirements of the language, but only to the requirements. That is, some
    signal sequences that previously always were delivered in one specific order
    may now from time to time be delivered in different orders. This may cause
    Erlang programs that have made _false assumptions_ about signal delivery
    order to fail even though they previously succeeded. For more information
    about signal ordering guarantees, see the chapter on
    [communication](communication.md) in the ERTS user's guide. The `+n` command
    line flag of [erl(1)](erl_cmd.md) can be helpful when trying to find
    signaling order bugs in Erlang code that have been exposed by these changes.

  - **Latency of signals sent from processes to ports** - Signals from processes
    to ports where previously always delivered immediately. This kept latency
    for such communication to a minimum, but it could cause lock contention
    which was very expensive for the system as a whole. In order to keep this
    latency low also in the future, most signals from processes to ports are by
    default still delivered immediately as long as no conflicts occur. Such
    conflicts include not being able to acquire the port lock, but also include
    other conflicts. When a conflict occur, the signal will be scheduled for
    delivery at a later time. A scheduled signal delivery may cause a higher
    latency for this specific communication, but improves the overall
    performance of the system since it reduce lock contention between
    schedulers. The default behavior of only scheduling delivery of these
    signals on conflict can be changed by passing the [\+spp](erl_cmd.md#%2Bspp)
    command line flag to [erl(1)](erl_cmd.md). The behavior can also be changed
    on port basis using the [parallelism](`m:erlang#open_port_parallelism`)
    option of the [open_port/2](`erlang:open_port/2`) BIF.

  - **Execution time of the `erlang:ports/0` BIF** - Since `erlang:ports/0` now
    can be preempted, the responsiveness of the system as a whole has been
    improved. A call to `erlang:ports/0` may, however, take a much longer time
    to complete than before. How much longer time heavily depends on the system
    load.

  - **Reduction cost of calling driver callbacks** - Calling a driver callback
    is quite costly. This was previously not reflected in reduction cost at all.
    Since the reduction cost now has increased, a process performing lots of
    direct driver calls will be scheduled out more frequently than before.

  _Potential incompatibilities_:

  - `driver_send_term()` has been deprecated and has been scheduled for removal
    in OTP-R17. Replace usage of `driver_send_term()` with usage of
    [erl_drv_send_term()](erl_driver.md#erl_drv_send_term).
  - `driver_output_term()` has been deprecated and has been scheduled for
    removal in OTP-R17. Replace usage of `driver_output_term()` with usage of
    [erl_drv_output_term()](erl_driver.md#erl_drv_output_term).
  - The new function
    [erl_drv_busy_msgq_limits()](erl_driver.md#erl_drv_busy_msgq_limits) has
    been added in order to able to control management of port queues.

  The [driver API version](erl_driver.md#version_management) has been bumped to
  2.1 from 2.0 due to the above changes in the driver API.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-10336 Aux Id: OTP-9892

- The experimental support for packages has been removed.

  Own Id: OTP-10348 Aux Id: kunagi-316 \[227]

- Wrong parameters when setting seq_trace-tokens from within a trace-pattern
  could crash the VM. This is now corrected.

  Own Id: OTP-10522

- Erlang specification 4.7.3 defines max tuple size to 65535 elements It is now
  enforced to no more than 16777215 elements (arity 24 bits)

  Previous edge cases (28 bits) were not validated and could cause undefined
  behaviour.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-10633

- Add insert_element/3 and delete_element/2

  Own Id: OTP-10643

- The previous default of a maximum of 32768 simultaneous processes has been
  raised to 262144. This value can be changed using the [\+P](erl_cmd.md#%2BP)
  command line flag of [erl(1)](erl_cmd.md). Note that the value passed now is
  considered as a hint, and that actual value chosen in most cases will be a
  power of two.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-10647 Aux Id: OTP-9892, OTP-10336

- The previously (in R15) proposed scheduler wakeup strategy is now used by
  default. This strategy is not as quick to forget about previous overload as
  the previous strategy.

  This change imply changes of the characteristics the system. Most notable:
  When a small overload comes and then disappears repeatedly, the system will
  for a bit longer time be willing to wake up schedulers than before. Timing in
  the system will due to this also change.

  The previous strategy can still be enabled by passing the
  [\+sws legacy](erl_cmd.md#%2Bsws) command line flag to `erl`.

  Own Id: OTP-10661 Aux Id: OTP-10033

- The [\+stbt](erl_cmd.md#%2Bstbt) command line argument of `erl` was added.
  This argument can be used for trying to set scheduler bind type. Upon failure
  unbound schedulers will be used.

  Own Id: OTP-10668

- Support ANSI in console

  Unix platforms will no longer filter control sequences to the ttsl driver thus
  enabling ANSI and colors in console. (Thanks to Pedram Nimreezi)

  Own Id: OTP-10678

- Add file:allocate/3 operation

  This operation allows pre-allocation of space for files. It succeeds only on
  systems that support such operation. (Thanks to Filipe David Manana)

  Own Id: OTP-10680

- Treat `-Wreturn-type` warnings as error when using GCC (Thanks to Tuncer Ayaz)

  Own Id: OTP-10683

- Implement ./otp_build configure --enable-silent-rules

  With silent rules, the output of make is less verbose and compilation warnings
  are easier to spot. Silent rules are disabled by default and can be disabled
  or enabled at will by make V=0 and make V=1. (Thanks to Anthony Ramine)

  Own Id: OTP-10726

- Use share flags for all file operations on Windows. Thanks to Filipe David
  Borba Manana.

  Own Id: OTP-10727

- Make/fakefop adjustments. Thanks to Tuncer Ayaz and Sebastian Rasmussen.

  Own Id: OTP-10733

- The runtime system will now by default use 10 async threads if thread support
  has been enabled when building the runtime system.

  This will prevent long blocking file-operations from blocking scheduler
  threads for long periods of time, which can be harmful. Apart from
  file-operations, it also effects other operations scheduled on the async
  thread pool by user implemented drivers.

  The amount of async threads can be controlled by using the
  [`+A`](erl_cmd.md#async_thread_pool_size) command line argument of
  [erl](erl_cmd.md). When running some offline tools you _might_ want to disable
  async threads, but you are advised _not_ to in the general case. Instead, you
  might want to increase the amount of async threads used.

  This change imply changes of the characteristics the system compared to the
  previous default. The responsiveness of the system as a whole will be
  improved. Operations scheduled on the async thread pool will get an increased
  latency. The throughput of these operations may increase, or decrease
  depending on the type of the operations and how they get scheduled. In the
  case of file operations, the throughput very much depends on how the Erlang
  application access files. Multiple concurrent accesses to different files have
  the potential of an increased throughput.

  Own Id: OTP-10736

- The default reader group limit has been increased to 64 from 8. This limit can
  be set using the `+rg` command line argument of [erl](erl_cmd.md).

  This change of default value will reduce lock contention on ETS tables using
  the `read_concurrency` option at the expense of memory consumption when the
  amount of schedulers and logical processors are between 8 and 64. For more
  information, see documentation of the `+rg` command line argument of
  [erl](erl_cmd.md).

  Own Id: OTP-10737

- New BIF float_to_list/2 which solves a problem of float_to_list/1 that doesn't
  allow specifying the number of digits after the decimal point when formatting
  floats (Thanks to Serge Aleynikov).

  Own Id: OTP-10752

- Limited support for unicode atoms in the external format and in the internal
  representation of the vm. This is a preparative feature in order to support
  communication with future releases of Erlang/OTP that may create unicode
  atoms.

  Own Id: OTP-10753

- Increased potential concurrency in ETS for `write_concurrency` option. The
  number of internal table locks has increased from 16 to 64. This makes it four
  times less likely that two concurrent processes writing to the same table
  would collide and thereby serialized. The cost is an increased constant memory
  footprint for tables using write_concurrency. The memory consumption per
  inserted record is not affected. The increased footprint can be particularly
  large if `write_concurrency` is combined with `read_concurrency`.

  Own Id: OTP-10787

## Erts 5.9.3.1

### Known Bugs and Problems

- Create an erl_crash.dump if no heart exists and no ERL_CRASH_DUMP_SECONDS is
  set (behaviour changed).

  Don't create an erl_crash.dump if heart do exists and no
  ERL_CRASH_DUMP_SECONDS is set (behaviour not changed).

  This changes the behaviour back to the R15B02 default considering if a beam
  was running with no heart.

  Own Id: OTP-10602

## Erts 5.9.3

### Fixed Bugs and Malfunctions

- Fix linking in OpenBSD. (Thanks to Matthew Dempsky)

  Own Id: OTP-10395

- Fix bug causing fallback atomics to be used even though healthy gcc atomics or
  libatomic_ops was detected.

  Own Id: OTP-10418

- Ensure 'erl_crash.dump' when asked for it. This will change erl_crash.dump
  behaviour.

  \* Not setting ERL_CRASH_DUMP_SECONDS will now terminate beam immediately on a
  crash without writing a crash dump file.

  \* Setting ERL_CRASH_DUMP_SECONDS to 0 will also terminate beam immediately on
  a crash without writing a crash dump file, i.e. same as not setting
  ERL_CRASH_DUMP_SECONDS environment variable.

  \* Setting ERL_CRASH_DUMP_SECONDS to a negative value will let the beam wait
  indefinitely on the crash dump file being written.

  \* Setting ERL_CRASH_DUMP_SECONDS to a positive value will let the beam wait
  that many seconds on the crash dump file being written.

  A positive value will set an alarm/timeout for restart both in beam and in
  heart if heart is running.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-10422 Aux Id: kunagi-250 \[161]

- Fix bug where MSVRT100.dll was not included in the windows installer.

  Own Id: OTP-10481

- In the expression `<<Bin/binary,...>>`, if `Bin` was a bitstring with a size
  not a multiple of 8, either no exception was generated or an incorrect
  exception was generated. (Thanks to Adam Rutkowski for reporting this bug.)

  Own Id: OTP-10524

- The runtime system could crash while scheduling a port task. The port task was
  scheduled either due to an external I/O event being triggered, a driver
  timeout being triggered, or data being sent over a distribution channel.

  Own Id: OTP-10556

- `erlang:memory(ets)` erroneously included the size of each ETS-table main
  structure twice.

  Own Id: OTP-10558

- Fix compile error in generated file hipe_amd64_bifs.S for Solaris.

  Own Id: OTP-10577

- A faulty spec for process_info/2 could cause false dialyzer warnings. The spec
  is corrected.

  Own Id: OTP-10584

- In very rare cases, the VM could crash if a garbage collector was called while
  executing an appending bit syntax instruction. The symptom was a core when
  reallocating memory in the function erts_bs_append. The garbage collector bug
  is now corrected.

  Own Id: OTP-10590

### Improvements and New Features

- Improve support for building and testing in embedded ppc environments.

  Own Id: OTP-10265 Aux Id: kunagi-159 \[daf97f67-5724-4812-a5b6-7e86990133d2-1]

- Due to a race condition on Windows, sometimes when printing to standard output
  and then immediately terminating erlang all data would not be printed. The
  emulator now waits for all data to be printed before exiting.

  Own Id: OTP-10325 Aux Id: kunagi-166 \[dd72d0e2-3e76-4a51-8b56-7564e24eecae]

- The frequency with which sleeping schedulers are woken due to outstanding
  memory deallocation jobs has been reduced.

  Own Id: OTP-10476 Aux Id: OTP-10162

- Clearer warnings about the dangers of misuse of
  [native functions](erl_nif.md#WARNING) and [drivers](erl_driver.md#WARNING)
  have been added to the documentation.

  Own Id: OTP-10557

## Erts 5.9.2

### Fixed Bugs and Malfunctions

- Fix erl_prim_loader errors in handling of primary archive. The following
  errors have been corrected:

  - If primary archive was named "xxx", then a file in the same directory named
    "xxxyyy" would be interpreted as a file named "yyy" inside the archive.
  - erl_prim_loader did not correctly create and normalize absolute paths for
    primary archive and files inside it, so unless given with exact same path
    files inside the archive would not be found. E.g. if escript was started as
    /full/path/to/xxx then "./xxx/file" would not be found since erl_prim_loader
    would try to match /full/path/to/xxx with /full/path/to/./xxx. Same problem
    with ../.
  - Depending on how the primary archive was built, erl_prim_loader:list_dir/1
    would sometimes return an empty string inside the file list. This was a
    virtual element representing the top directory of the archive. This has been
    removed.

  Thanks to Tuncer Ayaz and Shunichi Shinohara for reporting and co-authoring
  corrections.

  Own Id: OTP-10071

- Fix: Add port-I/O statistics for active once and true and not only active
  false.

  Own Id: OTP-10073

- The 64-bit windows installer did not look in the right directories for 64-bit
  version of Microsoft Visual C++ 2010 Redistibutable Package and hence took the
  wrong decision about having to install the redistributable package if the
  32-bit version was installed but not the 64-bit and vice versa. This bug has
  now been fixed Furthermore the sub-installer for the redistributable package
  is now run in silent mode if the erlang installer is.

  Own Id: OTP-10096

- epmd would fail to start automatically when starting a distributed erlang node
  installed in a location with a whitespace in the path.

  Own Id: OTP-10106

- A more or less harmless bug that sometimes caused memory deallocations to be
  delayed longer than intended has been fixed.

  Own Id: OTP-10116

- Fix bug causing emulator crash when running HiPE on ARM. Bug has existed since
  R15B.

  Own Id: OTP-10137

- A bug regarding spaces in C function prototypes has been fixed. (Thanks to
  Richard O'Keefe.)

  Own Id: OTP-10138

- Corrected dtrace pid length in message related probes. (Thanks to Zheng Siyao)

  Own Id: OTP-10142

- Correct formatting in exit error messages

  Ensure displayed sizes are not negative. (Thanks to Michael Santos)

  Own Id: OTP-10148

- fix escript/primary archive reloading

  If the mtime of an escript/primary archive file changes after being added to
  the code path, correctly reload the archive and update the cache. (Thanks to
  Tuncer Ayaz)

  Own Id: OTP-10151

- Doc fix: link from erlang:now/0 to os:timestamp/0

  Sometimes os:timestamp/0 is more appropriate than erlang:now/0. The
  documentation for the former has a link to the latter; this patch adds a link
  in the other direction to make os:timestamp/0 more visible. Thanks to Magnus
  Henoch

  Own Id: OTP-10180

- The caret in the werl window (on Windows) could appear at the wrong place
  after regaining focus. This is now corrected.

  Own Id: OTP-10181

- Fix bug that in some cases could cause corrupted binaries in ETS tables with
  `compressed` option.

  Own Id: OTP-10182

- Fix use of "clever" mktime

  Commit 1eef765 introduced regression (conditional \_always\_ evaluates to
  true) in which erlang:localtime_to_universaltime/2 stopped working on systems
  configured with timezone without DST (i.e. UTC) on \*BSD platforms: 1>
  erlang:localtime_to_universaltime(\{\{2012,1,1\},\{0,0,0\}\}, true). \*\*
  exception error: bad argument Thanks to Piotr Sikora

  Own Id: OTP-10187

- Relocate bodies of DTrace probes to the statically-linked VM.

  Due to various operating systems (in both the DTrace and SystemTap worlds) not
  fully supporting DTrace probes (or SystemTap-compatibility mode probes) in
  shared libraries, we relocate those probes to the statically-linked virtual
  machine. This could be seen as pollution of the pristine VM by a (yet)
  experimental feature. However:

  1\. This code can be eliminated completely by the C preprocessor. 2. Leaving
  the probes in the dyntrace NIF shared library simply does not work correctly
  on too many platforms. _Many_ thanks to Macneil Shonle at Basho for assisting
  when my RSI-injured fingers gave out. (note: Solaris 10 and FreeBSD
  9.0-RELEASE can take a long time to compile)

  Own Id: OTP-10189

- Fix bug in `ets:test_ms/2` that could cause emulator crash when using `'$_'`
  in match spec.

  Own Id: OTP-10190

- Supplying a filename longer than the operating system MAX_PATH to
  file:read_link/1 would cause a crash (Segemntation fault/Critical Error) on
  all platforms. This is now corrected.

  Own Id: OTP-10200

- If Perl was configured to interpret files as being encoded in UTF-8, the build
  would crash in `make_preload`. (Thanks to Aaron Harnly for noticing this
  issue.)

  Own Id: OTP-10201

- Fix the erlc -MP flag

  Because of a copy-and-paste error in erlc.c, the -MP flag had the same effect
  as -MG. As a workaround, you had to pass +makedep_phony to enable the MP
  option. This patch makes -MP work as intended.

  Own Id: OTP-10211

- Allow mixed IPv4 and IPv6 addresses to sctp_bindx

  Also allow mixed address families to bind, since the first address on a
  multihomed sctp socket must be bound with bind, while the rest are to be bound
  using sctp_bindx. At least Linux supports adding address of mixing families.
  Make inet_set_faddress function available also when HAVE_SCTP is not defined,
  since we use it to find an address for bind to be able to mix ipv4 and ipv6
  addresses. Thanks to Tomas Abrahamsson

  Own Id: OTP-10217

- Fix support for leap seconds-aware timezones

  erlang:universaltime_to_localtime is leap seconds-aware (since 2008), however
  erlang:localtime_to_universaltime is not, which gives surprising results on
  systems configured with leap seconds-aware timezones: 1>
  erlang:universaltime_to_localtime(\{\{2012,1,1\},\{0,0,0\}\}).
  \{\{2012,1,1\},\{0,0,0\}\} 2>
  erlang:localtime_to_universaltime(\{\{2012,1,1\},\{0,0,0\}\}).
  \{\{2012,1,1\},\{0,0,24\}\} and completely breaks
  calendar:local_time_to_universal_time_dst: 3>
  calendar:local_time_to_universal_time_dst(\{\{2011,1,1\},\{0,0,0\}\}). []
  Thanks to Piotr Sikora

  Own Id: OTP-10227

- erlsrv: gracefully stop emulator on Windows shutdown

  Windows will send the SERVICE_CONTROL_SHUTDOWN event to the service control
  handler when shutting down the system. Instead of ignoring the event, erlsrv
  will now invoke the stop action. Likewise, the Erlang emulator (and it's po
  drivers) must not quit upon reception of the CTRL_SHUTDOWN_EVENT event in th
  console control handler. Thanks to Jan Kloetzke

  Own Id: OTP-10228

- Fix dtrace bug in file rename operation.

  Own Id: OTP-10234

- Fix bug in memory management of driver port data locks (PDL). In some cases
  PDLs could be deallocated before `ready_async` or `async_free` callback was
  called.

  Own Id: OTP-10249

### Improvements and New Features

- Add port and suspend options to lock-counter profiling. (Thanks to Rick Reed)

  Own Id: OTP-10051

- Latency when using the active_once option in gen_tcp communication is reduced.

  Own Id: OTP-10055 Aux Id: sto139

- Remove bit8 option support from inet

  Own Id: OTP-10056

- The OS Pid of a port program is now available by calling
  erlang:port_info(Port,os_pid), Thanks to Matthias Lang for the original patch.

  Own Id: OTP-10057

- Fix openpty usage in run_erl.

  Reopening a slave file descriptor which was closed earlier could lead to a
  misbehaving connection. This has now been remedied.

  Own Id: OTP-10076

- Remove all code, documentation, options and diagnostic functions which were
  related to the experimental hybrid heap implementation.

  Own Id: OTP-10105

- Optimizations of memory deallocations.

  Own Id: OTP-10162 Aux Id: OTP-7775

- Optimization of process locking.

  Own Id: OTP-10163

- Added a xcomp example file for powerpc-dso-linux-gnu

  Own Id: OTP-10198

- Detect when middle endian doubles are used by a platform and account for it
  when decoding floats. (Thanks to Mike Sperber)

  Own Id: OTP-10209

## Erts 5.9.1.2

### Fixed Bugs and Malfunctions

- On Linux systems using heart (erl -heart) and a HEAR_BEAT_TIMEOUT less than
  default, heart could fire even though Erlang was running fine after approx 298
  to 497 days (depending on kernel config). This was due to the behaviour of the
  times(2) system call. Usage of times(2) is now replaced with clock_gettime(2)
  and the CLOCK_MONOTONIC clock, resulting in a more stable solution. The Erlang
  VM itself has used clock_gettime(2) on linux since before R12B, so this only
  affects the heart program.

  Own Id: OTP-10111 Aux Id: seq12075

## Erts 5.9.1.1

### Fixed Bugs and Malfunctions

- If threads support for the runtime system had been disabled at compile time
  (`--disable-threads` had been passed to `configure`), and the `+A` command
  line argument of `erl` was passed when starting the runtime system,
  [driver_system_info()](erl_driver.md#driver_system_info) erroneously claimed
  that the runtime system had async threads even though it had not.

  Due to this bug the file driver did not split tasks into smaller chunks, but
  instead completed the whole task at once, i.e., the scheduler got occupied
  with I/O for a longer time than intended.

  Own Id: OTP-10059

### Improvements and New Features

- A proposal for a new scheduler wakeup strategy has been implemented. For more
  information see the documentation of the [\+sws](erl_cmd.md#%2Bsws) command
  line argument of `erl`.

  Own Id: OTP-10033 Aux Id: Seq12025

- A switch for configuration of busy wait length for scheduler threads has been
  added. For more information see the documentation of the
  [\+sbwt](erl_cmd.md#%2Bsbwt) command line argument of `erl`.

  Own Id: OTP-10044 Aux Id: Seq11976

- The extra memory barriers introduced by bug-fix OTP-9281 were unnecessarily
  used also on tables without the `write_concurrency` option enabled. This could
  unnecessarily degrade performance of ETS tables without `write_concurrency` on
  some hardware (e.g. PowerPC) while not effecting performance at all on other
  hardware (e.g. x86/x86_64).

  OTP-9281 (R14B03): ETS tables using the `write_concurrency` option could
  potentially get into an internally inconsistent state.

  Own Id: OTP-10048 Aux Id: OTP-9281

## Erts 5.9.1

### Fixed Bugs and Malfunctions

- `erlang:system_profile` erroneous profiled the profiler process when observing
  runnable processes. This has been corrected.

  Own Id: OTP-9849

- Calling trace_info/2 asking for information about a function that had native
  could crash the run-time system.

  Own Id: OTP-9886

- reduce smp locking time range in erts_garbage_collect (thanks to Jovi Zhang)

  Own Id: OTP-9912

- Fix typo in supervisor behaviour doc (Thanks to Ricardo Catalinas Jiménez)

  Own Id: OTP-9924

- Correct spelling of registered (Thanks to Richard Carlsson)

  Own Id: OTP-9925

- erts: Remove unused variable (Thanks to Jovi Zhang)

  Own Id: OTP-9926

- Fix bug in ETS with `compressed` option and insertion of term containing large
  integers (>2G) on 64-bit machines. Seen to cause emulator crash. (Thanks to
  Diego Llarrull for excellent bug report)

  Own Id: OTP-9932

- Handle Linux OS where /sys/devices/system/node is only readable by root.
  Fallback to /sys/devices/system/cpu for topology info.

  Own Id: OTP-9978

- When an escript ends now all printout to standard output and standard error
  gets out on the terminal. This bug has been corrected by changing the
  behaviour of erlang:halt/0,1, which should fix the same problem for other
  escript-like applications, i.e that data stored in the output port driver
  buffers got lost when printing on a TTY and exiting through erlang:halt/0,1.

  The BIF:s erlang:halt/0,1 has gotten improved semantics and there is a new BIF
  erlang:halt/2 to accomplish something like the old semantics. See the
  documentation.

  Now erlang:halt/0 and erlang:halt/1 with an integer argument will close all
  ports and allow all pending async threads operations to finish before exiting
  the emulator. Previously erlang:halt/0 and erlang:halt(0) would just wait for
  pending async threads operations but not close ports. And erlang:halt/1 with a
  non-zero integer argument would not even wait for pending async threads
  operations.

  To roughly the old behaviour, to not wait for ports and async threads
  operations when you exit the emulator, you use erlang:halt/2 with an integer
  first argument and an option list containing \{flush,false\} as the second
  argument. Note that now is flushing not dependent of the exit code, and you
  cannot only flush async threads operations which we deemed as a strange
  behaviour anyway.

  Also, erlang:halt/1,2 has gotten a new feature: If the first argument is the
  atom 'abort' the emulator is aborted producing a core dump, if the operating
  system so allows.

  Own Id: OTP-9985

- Added check to inet driver to avoid building on operating systems that do not
  yet have IPv6 compatible socket API. (Thanks to Peer Stritzinger)

  Own Id: OTP-9996

- Fix bug when the number of CPUs actually found is lower than the configured
  value. (Thanks to Benjamin Herrenschmidt)

  Own Id: OTP-10004

- The runtime system without SMP support and without thread support erroneously
  busy waited when no work was present. This bug first appeared in `erts-5.9`.

  Own Id: OTP-10019

- Various typographical errors corrected in documentation for common_test,
  driver, erl_driver and windows installation instructions. (Thanks to Tuncer
  Ayaz)

  Own Id: OTP-10037

- Fix memory leak caused by race on exiting process

  Own Id: OTP-10041

### Improvements and New Features

- Add `erlang:statistics(scheduler_wall_time)` to ensure correct determination
  of scheduler utilization. Measuring scheduler utilization is strongly
  preferred over CPU utilization, since CPU utilization gives very poor
  indications of actual scheduler/vm usage.

  Own Id: OTP-9858

- ERTS internal API improvements. In some cases the amount of atomic read
  operations needed have been reduced due to this.

  Own Id: OTP-9922

- The DTrace source patch from Scott Lystig Fritchie is integrated in the source
  tree. Using an emulator with dtrace probe is still not supported for
  production use, but may be a valuable debugging tool. Configure with
  --with-dynamic-trace=dtrace (or --with-dynamic-trace=systemtap) to create a
  build with dtrace probes enabled. See runtime_tools for documentation and
  examples.

  Own Id: OTP-10017

### Known Bugs and Problems

- enif_make_copy may invalidate enif_inspect_binary.

  Own Id: OTP-9828

## Erts 5.9.0.1

### Fixed Bugs and Malfunctions

- A feature test for the `lwsync` instruction performed on PowerPC hardware at
  runtime system startup got into an eternal loop if the instruction was not
  supported. This bug was introduced in erts-5.9/OTP-R15B.

  Own Id: OTP-9843

- I/O events could potentially be delayed for ever when enabling kernel-poll on
  a non-SMP runtime system executing on Solaris. When also combined with
  async-threads the runtime system hung before completing the boot phase. This
  bug was introduced in erts-5.9/OTP-R15B.

  Own Id: OTP-9844

## Erts 5.9

### Fixed Bugs and Malfunctions

- Honor option `packet_size` for http packet parsing by both TCP socket and
  `erlang:decode_packet`. This gives the ability to accept HTTP headers larger
  than the default setting, but also avoid DoS attacks by accepting lines only
  up to whatever length you wish to allow. For consistency, packet type `line`
  also honor option `packet_size`. (Thanks to Steve Vinoski)

  Own Id: OTP-9389

- A few contracts in the `lists` module have been corrected.

  Own Id: OTP-9616

- The Unicode noncharacter code points 16#FFFE and 16#FFFE were not allowed to
  be encoded or decoded using the `unicode` module or bit syntax. That was
  inconsistent with the other noncharacters 16#FDD0 to 16#FDEF that could be
  encoded/decoded. To resolve the inconsistency, 16#FFFE and 16#FFFE can now be
  encoded and decoded. (Thanks to Alisdair Sullivan.)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9624

- Make epp search directory of current file first when including another file
  This completes a partial fix in R11 that only worked for include_lib().
  (Thanks to Richard Carlsson)

  Own Id: OTP-9645

- Fixed memory leak in `enif_inspect_io_list_as_binary` when applied on a
  process independent environment.

  Own Id: OTP-9668

- The number of beam catches allowed in code are no longer statically defined
  and will grow according to its need.

  Own Id: OTP-9692

- Add missing parenthesis in heart doc.

  Add missing spaces in the Reference Manual distributed section.

  In the HTML version of the doc those spaces are necessary to separate those
  words.

  Own Id: OTP-9693

- Fixes module erlang doc style: option description (Thanks to Ricardo Catalinas
  Jiménez)

  Own Id: OTP-9697

- Specifying a scope to binary:match/3 when using multiple searchstrings
  resulted in faulty return values. This is now corrected.

  Own Id: OTP-9701

- The runtime system crashed if more than one thread tried to exit the runtime
  system at the same time.

  Own Id: OTP-9705

- Fix documentation for erlang:process_flag/2

  For the subsection about process_flag(save_calls, N) there's an unrelated
  paragraph about process priorities which was copied from the preceding
  subsection regarding process_flag(priority, Level). (Thanks to Filipe David
  Manana)

  Own Id: OTP-9714

- Calls to `erlang:system_flag(schedulers_online, N)` and/or
  `erlang:system_flag(multi_scheduling, block|unblock)` could cause internal
  data used by this functionality to get into an inconsistent state. When this
  happened various problems occurred. This bug was quite hard to trigger, so
  hopefully no-one has been effected by it.

  A spinlock used by the run-queue management sometimes got heavily contended.
  This code has now been rewritten, and the spinlock has been removed.

  Own Id: OTP-9727

- Use libdlpi to get physical address (Thanks to Trond Norbye)

  Own Id: OTP-9818

### Improvements and New Features

- An option list argument can now be passed to
  `file:read_file_info/2, file:read_link_info/2` and `file:write_file_info/3`
  and set time type information in the call. Valid options are
  `{time, local}, {time, universal}` and `{time, posix}`. In the case of `posix`
  time no conversions are made which makes the operation a bit faster.

  Own Id: OTP-7687

- A number of memory allocation optimizations have been implemented. Most
  optimizations reduce contention caused by synchronization between threads
  during allocation and deallocation of memory. Most notably:

  - Synchronization of memory management in scheduler specific allocator
    instances has been rewritten to use lock-free synchronization.
  - Synchronization of memory management in scheduler specific pre-allocators
    has been rewritten to use lock-free synchronization.
  - The 'mseg_alloc' memory segment allocator now use scheduler specific
    instances instead of one instance. Apart from reducing contention this also
    ensures that memory allocators always create memory segments on the local
    NUMA node on a NUMA system.

  Own Id: OTP-7775

- The ethread atomic memory operations API used by the runtime system has been
  extended and improved.

  The ethread library now also performs runtime tests for presence of hardware
  features, such as for example SSE2 instructions, instead of requiring this to
  be determined at compile time.

  All uses of the old deprecated atomic API in the runtime system have been
  replaced with the use of the new atomic API. In a lot of places this change
  imply a relaxation of memory barriers used.

  Own Id: OTP-9014

- gen_sctp:open/0-2 may now return \{error,eprotonosupport\} if SCTP is not
  supported

  gen_sctp:peeloff/1 has been implemented and creates a one-to-one socket which
  also are supported now

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9239

- Sendfile has been added to the file module's API. sendfile/2 is used to read
  data from a file and send it to a tcp socket using a zero copying mechanism if
  available on that OS.

  Thanks to Tuncer Ayaz and Steve Vinovski for original implementation

  Own Id: OTP-9240

- enif_get_reverse_list function added to nif API. This function should be used
  to reverse small lists which are deep within other structures making it
  impractical to do the reverse in Erlang.

  Own Id: OTP-9392

- The deprecated concat_binary/1 BIF has been removed. Use `list_to_binary` or
  [`iolist_to_binary/1`](`iolist_to_binary/1`) instead.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9421

- Erlang/OTP can now be built using parallel make if you limit the number of
  jobs, for instance using '`make -j6`' or '`make -j10`'. '`make -j`' does not
  work at the moment because of some missing dependencies.

  Own Id: OTP-9451

- Line number and filename information are now included in exception backtraces
  as a fourth element in the MFA tuple. The information will be pretty-printed
  by the shell and used by `common_test` to provide better indication of where a
  test case.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9468

- All binary constants used to be handled as heap binaries (i.e. the entire
  binary would be copied when sent to another process). Binary constants larger
  than 64 bytes are now refc binaries (i.e. the actual data in the binary will
  not be copied when sent to another process).

  Own Id: OTP-9486

- If a float and an integer is compared, the integer is only converted to a
  float if the float datatype can contain it. Otherwise the float is converted
  to an integer.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9497

- Add NIF function enif_is_number

  This function allows for easily determining if a term represents or not a
  number (integer, float, small or big).(Thanks to Filipe David Manana)

  Own Id: OTP-9629

- The ERTS internal system block functionality has been replaced by new
  functionality for blocking the system. The old system block functionality had
  contention issues and complexity issues. The new functionality piggy-backs on
  thread progress tracking functionality needed by newly introduced lock-free
  synchronization in the runtime system. When the functionality for blocking the
  system isn't used, there is more or less no overhead at all. This since the
  functionality for tracking thread progress is there and needed anyway.

  Own Id: OTP-9631

- An ERTS internal, generic, many to one, lock-free queue for communication
  between threads has been introduced. The many to one scenario is very common
  in ERTS, so it can be used in a lot of places in the future. Currently it is
  used by scheduling of certain jobs, and the async thread pool, but more uses
  are planned for the future.

  Drivers using the driver_async functionality are not automatically locked to
  the system anymore, and can be unloaded as any dynamically linked in driver.

  Scheduling of ready async jobs is now also interleaved in between other jobs.
  Previously all ready async jobs were performed at once.

  Own Id: OTP-9632

- Tuple funs (a two-element tuple with a module name and a function) are now
  officially deprecated and will be removed in R16. Use '`fun M:F/A`' instead.
  To make you aware that your system uses tuple funs, the very first time a
  tuple fun is applied, a warning will be sent to the error logger.

  Own Id: OTP-9649

- Changed the internal BIF calling convention. Will make simpler faster code and
  allow BIFs with an arbitrary arity.

  Own Id: OTP-9662

- Windows native critical sections are now used internally in the runtime system
  on Windows as mutex implementation. This since they perform better under
  extreme contention than our own implementation.

  Own Id: OTP-9671

- Convert some erl_nif macros into inline functions. Allow for better compile
  time type checking. (Thanks to Tuncer Ayaz)

  Own Id: OTP-9675

- The `+scl` command line flag has been added. It can be used for disabling
  compaction of scheduler load. For more information see the [erl](erl_cmd.md)
  documentation.

  Own Id: OTP-9695

- The build system has been updated so that Erlang/OTP can be built on Mac OS X
  Lion systems without a GCC compiler. The INSTALL guide has been updated with
  instructions on how to install a GCC compiler and build Erlang/OTP with it, in
  order to get a run-time system with better performance.

  Own Id: OTP-9712

- When loading a module, the system use to run on a single scheduler during the
  entire loading process. This has been changed to only take down the system
  just before inserting the loaded code into the system tables, resulting in a
  much shorter disruption if a module is loaded in a busy system. (Suggested by
  Bob Ippolito.)

  Own Id: OTP-9720

- Possible to run HiPE without floating point exceptions (FPE). Useful on
  platforms that lack reliable FPE. Slower float operations compared to HiPE
  with FPE.

  Own Id: OTP-9724

- As of ERTS version 5.9 (OTP-R15B) the runtime system will by default _not_
  bind schedulers to logical processors.

  If the Erlang runtime system is the only operating system process that binds
  threads to logical processors, this improves the performance of the runtime
  system. However, if other operating system processes (as for example another
  Erlang runtime system) also bind threads to logical processors, there might be
  a performance penalty instead. In some cases this performance penalty might be
  severe. Due to this, we change the default so that the user must make an
  active decision in order to bind schedulers.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9726

- The use of `erlang:system_flag(scheduler_bind_type, _)` and
  `erlang:system_flag(cpu_topology, _)` have been deprecated and scheduled for
  removal in erts-5.10/OTP-R16. For more information see the documentation of
  `erlang:system_flag/2`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9749

- An ancient workaround for a Windows bug was removed from the open_port code,
  open_port(\{spawn,...\}...) is now faster. Thanks to Daniel Goertzen.

  Own Id: OTP-9766

- The use of deprecated 32bit time_t on 32bit Windows is removed.

  Own Id: OTP-9767

- The NIF `reload` mechanism is deprecated. Do not use it as an upgrade method
  for live production systems. It might be removed in future releases. It can
  still serve as a development feature but a warning message will be logged each
  time it is used.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9771

- The driver interface has been changed to enable 64-bit aware drivers. Most
  importantly the return types for ErlDrvEntry callbacks 'call' and 'control'
  has ben enlarged which require drivers to be changed or they will cause
  emulator crashes. See
  [Rewrites for 64-bit driver interface ](erl_driver.md#rewrites-for-64-bit-driver-interface)in
  the driver manual.

  Due to this driver [version management](erl_driver.md#version_management) is
  now mandatory. A driver that is not written with version management or a
  driver that was compiled with the wrong major version will be not be loaded by
  the emulator.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9795

- Eliminate use of deprecated regexp module

  Own Id: OTP-9810

## Erts 5.8.5

### Fixed Bugs and Malfunctions

- Several bugs concerning constant binary constructions such as <<0:4294967295>>
  have been corrected. Depending on the actual size of the binary and the type
  of run-time system (32-bit, halfword, 64-bit), such expression could either
  crash the run-time system or make the loader refuse loading of the module.

  Own Id: OTP-9284

- The Erlsrv utility failed to stop the erlang machine if no StopAction was
  defined when the service was stopped. This is now corrected.

  Own Id: OTP-9344

- Due to a bug in glibc the runtime system could abort while trying to destroy a
  mutex. A fix for this was introduced in R14B02. This fix did, however, not
  solve the problem. The runtime system will now issue a warning instead of
  aborting.

  Own Id: OTP-9373 Aux Id: OTP-9009

- Replace atom in DRV macro in prim_file with string

  An experimental version of Dialyzer discovered that the atom that replaced the
  DRV macro in prim_file ends up in calls to erlang:open_port(\{spawn, Driver\},
  Portopts) as the Driver argument. The documentation states that this call
  requires a string there.

  This change is also consistent with the one introduced in commit
  0f03b1e9d2bef3bc830c31a369261af4c5234727 by Kostis Sagonas.

  Own Id: OTP-9377

- Fix typos in the epmd documentation (Thanks to Holger Weiß )

  Own Id: OTP-9387

- Fix faulty integer terms created by NIF API from 64-bit integers on halfword
  emulator. (Thanks to Paolo Negri and Paul Davis)

  Own Id: OTP-9394

- Fix `epmd` crash on vxworks caused by faulty argument to select() system call.

  Own Id: OTP-9427 Aux Id: seq11855

- The ets:test_ms function could in rare cases truncate the error messages. This
  is now corrected.

  Own Id: OTP-9435

- Fix bug related to hibernate and HiPE (clear F_HIBERNATE_SCHED flag)

  F_HIBERNATE_SCHED flag that was introduced in b7ecdcd1ae9e11b8f75e must be
  cleared in hipe_mode_switch as well. Otherwise, processes running HiPE code
  that hibernate, wake up and then trap into a BIF will not be
  rescheduled.(Thanks to Paul Guyot)

  Own Id: OTP-9452

- Fix bug in FreeBSD topology detection code (Thanks to Paul Guyot)

  Own Id: OTP-9453

- Fix use of logical operator && with constant operand instead of bitwise &
  (Thanks to Cristian Greco)

  Own Id: OTP-9454

- inet: error if fd does not match socket domain

  If an IPv4 fd is opened as an IPv6 socket, unexpected behaviour can occur. For
  example, if an IPv4 UDP socket is opened and passed into Erlang as an IPv6
  socket, the first 3 bytes (corresponding to 1 byte representing the protocol
  family, 2 bytes set to the port) are stripped from the payload. The cause of
  the UDP payload truncation happens in inet_drv.c:packet_inet_input when a call
  to inet_get_address fails silently because the family is set to PF_INET6 but
  the buffer len is the size of an IPv4 struct sockaddr_in.

  (Thanks to Andrew Tunnell-Jones for finding the bug and the test case\!)

  Own Id: OTP-9455

- erts: use a union to avoid strict aliasing issues

  Use a union for pointer type conversion to avoid compiler warnings about
  strict-aliasing violations with gcc-4.1. gcc >= 4.2 does not emit the warning.
  erts: adapt matrix_nif to R14 erl_nif API changes (Thanks To Tuncer Ayaz)

  Own Id: OTP-9487

- fix 64-bit issues in the garbage collection (Thanks to Richard Carlsson)

  Own Id: OTP-9488

- epmd: fix compiler warnings

  Suppress compiler warnings about ignored return values. (Thanks to Michael
  Santos )

  Own Id: OTP-9500

- Fix non-existing function (erlang:disconnect/1) in distributed reference
  manual (Thanks to Fabian Król)

  Own Id: OTP-9504

- Document fdatasync -lrt requirement (SunOS <= 5.10) (Thanks to Tuncer Ayaz)

  Own Id: OTP-9512

- Let epmd ignore empty ERL_EPMD_ADDRESS

  If the environment variable ERL_EPMD_ADDRESS is set to the empty string, empd
  now behaves like it does by default when ERL_EPMD_ADDRESS is unset. That is,
  in this case, epmd now listens on all available interfaces instead of using
  only the loopback interface, which happened because epmd added the loopback
  address to the (in this case empty) list of addresses specified via
  ERL_EPMD_ADDRESS.

  Also, epmd now ignores ERL_EPMD_ADDRESS if it contains only separator
  characters (comma and space).

  The same applies to epmd's -address option.(Thanks to Holger Weiß)

  Own Id: OTP-9525

- Remove dead code in erl_compile (Thanks to Tuncer Ayaz)

  Own Id: OTP-9527

- Add erlang:external_size/2 BIF

  This BIF's second parameter is a list of options. Currently the only allowed
  option is \{minor_version, Version\} where version is either 0 (default) or 1.
  (Thanks to Filipe David Manana )

  Own Id: OTP-9528

- Fix enif_compare on 64bits machines

  In 64bits machines the Sint type has a size of 8 bytes, while on 32bits
  machines it has a 4 bytes size. enif_compare was ignoring this and therefore
  returning incorrect values when the result of the CMP function (which returns
  a Sint value) doesn't fit in 4 bytes. (Thanks to Filipe David Manana)

  Own Id: OTP-9533

- Implement or fix -Werror option

  If -Werror is enabled and there are warnings no output file is written. Also
  make sure that error/warning reporting is consistent. (Thanks to Tuncer Ayaz)

  Own Id: OTP-9536

- In some rare cases we did not have a run queue when scheduling misc ops. This
  is now fixed.

  Own Id: OTP-9537

- Remove misc. compiler warnings

  Own Id: OTP-9542

- Two bugs in gen_sctp has been corrected: getopts/setopts hence also send could
  only be called from socket owner, and options 'linger', 'rcvbuf' and 'sndbuf'
  was read from wrong protocol layer hence read wrong values by getopts.

  Own Id: OTP-9544

- Erlang/OTP can now be built on MacOS X Lion.

  Own Id: OTP-9547

- XML files have been corrected.

  Own Id: OTP-9550 Aux Id: OTP-9541

- Fix potential errors inspired by running cppcheck(1) (Thanks to Christian von
  Roques)

  Own Id: OTP-9557

- When auxiliary work was enqueued on a scheduler, the wakeup of the scheduler
  in order to handle this work could be lost. Wakeups in order to handle
  ordinary work were not effected by this bug. The bug only effected runtime
  systems with SMP support as follows:

  - Deallocation of some ETS data structures could be delayed.
  - On Linux systems not using the NPTL thread library (typically ancient
    systems with kernel versions prior to 2.6) and Windows systems, the
    `{Port, {exit_status, Status}}` message from a terminating port program
    could be delayed. That is, it only effected port programs which had been
    started by passing `exit_status` as an option to
    [`open_port/2`](`open_port/2`).

  Own Id: OTP-9567

- Handle rare race in the crypto key server functionality

  Own Id: OTP-9586

### Improvements and New Features

- Types and specifications have been added.

  Own Id: OTP-9356

- New allocator strategy "address order first fit". May ease the emptying of
  memory carriers and thereby real release of memory back to the OS.

  Own Id: OTP-9424

- The new `erlang:check_old_code/1` BIF checks whether a module has old code.

  Own Id: OTP-9495

- Update documentation and specifications of some of the zlib functions.

  Own Id: OTP-9506

- Detect the available CPUs on IRIX

  Add support for querying the number of configured and online processors on SGI
  systems running IRIX.(Thanks to Holger Weiß)

  Own Id: OTP-9531

## Erts 5.8.4

### Fixed Bugs and Malfunctions

- Fix binary and iolist overflow problems. Typically problems arose in length
  calculation where the result would exceed (1 bsl 32 - 1).

  Own Id: OTP-9118

- Using the old erlang shell (i.e. erl instead on werl) on windows and doing
  several init:restart's would eventually hang the VM. That is no longer the
  case.

  Own Id: OTP-9139

- Removed recursive C code when printing Erlang terms to buffers, avoiding stack
  overflows that could cause VM to crash.

  Own Id: OTP-9140

- The send_timeout option in gen_tcp did not work properly in active mode or
  with \{active,once\} options. This is now corrected.

  Own Id: OTP-9145

- Fixed various typos across the documentation (Thanks to Tuncer Ayaz)

  Own Id: OTP-9154

- Remove duplicate stack entries which could occur after calling certain BIFs.

  Own Id: OTP-9163

- A race when starting two nodes simultaneously using run_erl has been removed.

  Own Id: OTP-9164

- Add documentation on .erlang processing back again (Thanks to Gabor Liptak)

  Own Id: OTP-9189

- Remove gratuitous paren in driver_entry(Thanks to Tuncer Ayaz)

  Own Id: OTP-9192

- Fix some wrong pointer dereferences (Thanks to Cristian Greco)

  Own Id: OTP-9194

- erts: Remove unused variables (Thanks to Tuncer Ayaz)

  Own Id: OTP-9205

- The documentation for `init:get_args/0` has been removed. `init:get_args/0`
  itself was deprecated in R9C and removed in R12B. (Thanks to Eric Pailleau.)

  Own Id: OTP-9209

- Allow user to specify the IP address epmd binds to

  The IP address(es) epmd binds to can now be specified by the user, either via
  epmd's new "-address" option or (if that's not used) by setting the
  environment variable ERL_EPMD_ADDRESS. Multiple addresses may be specified
  using a comma-separated list. If the loopback address is not in this list, it
  will be added implicitly, so that the daemon can be queried by an interactive
  epmd process.(Thanks to Holger Weiß)

  Own Id: OTP-9213

- epmd: include host address in local access check

  In FreeBSD jails, the source and destination address of connections to
  localhost are changed to be the IP address of the jail. Consider connections
  from the host's IP address to itself (e.g., the source and destination address
  match) to be local for the access control checks. (Thanks to Michal Santos and
  Tom at diogunix.com)

  Own Id: OTP-9214

- Fix list returned by net_kernel:epmd_module

  Function epmd_module of net_kernel returns a list instead of an atom, when the
  epmd_module-flag is used. (Thanks to Markus Knofe)

  Own Id: OTP-9215

- Fix epmd's dbg_perror() output

  The dbg_perror() function now hands the current errno value over to
  dbg_gen_printf(). This fixes the problem that errno had been reset to zero by
  the time it was used (to print the corresponding error message) in the
  dbg_gen_printf() function. (Thanks to Holger Weiß)

  Own Id: OTP-9223

- heart: remove garbage appended to heart command

  heart:get_cmd/0 is documented to return an empty string if the command is
  cleared. get_cmd/0 returns 2 extra bytes: 1 byte for the trailing null, 1 byte
  from the op (the op is an unsigned char and 2 bytes are allocated for it in
  the returned buffer). (Thanks to Michael Santos)

  Own Id: OTP-9224

- file: fix hang reading compressed files

  The gzio driver goes into an infinite loop when reading past the end of a
  compressed file. Reported-By: Alex Morarash (Thanks to Michael Santos)

  Own Id: OTP-9245

- Eliminate alias warnings from gcc 4.5.2

  Own Id: OTP-9250

- Unsigned integer may overflow in error message (Thanks to Michael Santos)

  Own Id: OTP-9251

- Driver names should be strings, not atoms

  Own Id: OTP-9253

- driver_entry: Remove gratuitous paren and fix typo (Thanks to Tuncer Ayaz)

  Own Id: OTP-9254

- Fix format specifiers in erl_exit messages

  Fix an error message by using an unsigned integer specifier as seen in a tweet
  by @metabrew: #erlang VM crashed with "no next heap size found: -2090496108,
  offset 0", suddenly allocated all available RAM

  Also correct mis-typed string formats in bif.c.(Thanks to Michael Santos)

  Own Id: OTP-9262

- net_drv: remove unused tcp request id inet_drv: remove gratuitous assignment
  (Thanks to Tuncer Ayaz)

  Own Id: OTP-9263

- Teach run_erl RUN_ERL_DISABLE_FLOWCNTRL for disabling flow control

  Flow control can cause unwanted behaviour of the beam process, if accidentally
  hit Ctrl-S (instead of Ctrl-D to detach) the entire beam may be blocked.

  Fix this problem by making it possible to turn off flow control by setting the
  environment variable RUN_ERL_DISABLE_FLOWCNTRL. (Thanks to Jonas Faklkevik)

  Own Id: OTP-9270

- The following bugs due to missing memory barriers have been fixed:

  - ETS tables using the `write_concurrency` option could potentially get into
    an internally inconsistent state.
  - ETS tables using the `ordered_set` option could potentially get into an
    internally inconsistent state.
  - A number of memory barriers have been added when building with the
    `libatomic_ops` API (i.e. when passing `--with-libatomic_ops=PATH` to
    `configure`) and the tilera atomics API (i.e. when building for the tilera
    chip). Note that these bugs were due to erroneous usage of the APIs, and not
    in the implementations of the APIs. When using these APIs the following bugs
    where present:

    - The BIF `erlang:ports/0` could return an erroneous result.
    - A thread blocking other threads during code loading, or setup of tracing
      could potentially read invalid data.
    - Fixation of ETS tables could potentially get into an internally
      inconsistent state.

  Own Id: OTP-9281

- Fix halfword bug for ETS ordered_set when doing `select/match` with partly
  bound key.

  Own Id: OTP-9292

- Fix bug in `code:is_module_native` that caused crash for deleted modules.

  Own Id: OTP-9298

- Calling `driver_async_cancel()` could cause a scheduler thread to enter an
  eternal loop doing no useful work. (Thanks to Anders Ramsell)

  Own Id: OTP-9302

### Improvements and New Features

- New `enif_is_exception function` to allow NIFs to determine whether an
  `ERL_NIF_TERM` represents an exception. (Thanks to Steve Vinoski)

  Own Id: OTP-9150

- A process being garbage collected by another process could be scheduled on
  another scheduler. This prevented this scheduler from doing any useful work
  until the garbage collection was done. This either occurred due to a explicit
  call to the [`garbage_collect/1`](`garbage_collect/1`) BIF, or due to a
  garbage collection part of code loading. A process being garbage collected
  like this will now not be scheduled until the garbage collection has
  completed.

  Own Id: OTP-9211

- Remove unnecessary validation copy in prim_file:drv_command/3 (Thanks to Tony
  Rogvall)

  Own Id: OTP-9276

- Symbolic link handling on windows have been slightly updated to map error
  conditions more consequently and correctly read directory links created
  outside of the Erlang environment.

  Own Id: OTP-9279

- Due to standard library DLL mismatches between versions of OpenSSL and
  Erlang/OTP, OpenSSL is now linked statically to the crypto driver on Windows.
  This fixes problems starting crypto when running Erlang as a service on all
  Windows versions.

  Own Id: OTP-9280

- Halfword emulator memory handling improvements:

  - Much more of internal memory structures have been made able to use "high"
    memory and are no longer restricted to the 4Gb limit that still applies for
    all process heap data.
  - Fixed faulty values from `erlang:memory()` caused by 32-bit counter
    overflow.
  - New counter `low` in `erlang:memory()` that sums up all memory restricted by
    4Gb limit.

  Own Id: OTP-9291 Aux Id: seq11841

- The value set in the undocumented and unsupported ERL_version_FLAGS (e.g.
  ERL_R14B03_FLAGS) environment variable can now be overridden by the command
  line (similar to ERL_AFLAGS).

  Own Id: OTP-9297

## Erts 5.8.3.2

### Known Bugs and Problems

- Fix halfword emulator bug in `ets:select_delete` for `ordered_set` that caused
  emulator to crash.

  Own Id: OTP-9258 Aux Id: seq11836

## Erts 5.8.3.1

### Fixed Bugs and Malfunctions

- Ets table type ordered_set could order large integer keys wrongly on pure
  64bit platforms. This is now corrected.

  Own Id: OTP-9181

- The status of a process was unnecessarily set to waiting before a process was
  enqueued in a run queue. This bug was harmless up until OTP-R14B01. In
  OTP-R14B02 `erlang:hibernate/3` was fixed (OTP-9125). After the introduction
  of OTP-9125, the previously harmless process status bug sometimes caused
  erroneous badarg exceptions from `process_info()`.

  OTP-9125 also introduced a thread unsafe access to the status field of a
  process which now also have been fixed.

  \*** INCOMPATIBILITY with noxs \***

  Own Id: OTP-9197

## Erts 5.8.3

### Fixed Bugs and Malfunctions

- The scroll wheel now scrolls the werl window on Windows.

  Own Id: OTP-8985

- Some malformed distribution messages could cause VM to crash, this is now
  corrected.

  Own Id: OTP-8993

- The OS function getifaddrs() can return NULL in some address fields for e.g
  PPP and tunnel devices which caused the emulator to segfault. This bug has now
  been corrected.

  Own Id: OTP-8996

- The expression <<A:0>> would always produce an empty binary, even if `A` was
  not an integer. Corrected to cause a `badarg` exception if the type of `A` is
  invalid. (Thanks to Zvi.)

  Own Id: OTP-8997

- A bug that potentially could cause an emulator crash when deleting an
  ETS-table has been fixed. A resource leak when hitting the maximum amount of
  ETS-tables allowed has also been fixed.

  Own Id: OTP-8999

- A bug in the [`exit/2`](`exit/2`) BIF could potentially cause an emulator
  crash.

  Own Id: OTP-9005

- Due to a bug in glibc the runtime system could abort while trying to destroy a
  mutex. The runtime system will now issue a warning instead of aborting.

  Own Id: OTP-9009

- A bug in epmd could create strange behaviour when listen() calls failed. This
  is now corrected thanks to Steve Vinoski.

  Own Id: OTP-9024

- When setting file_info the win32_driver will now correctly set access and
  modified time. Previously these entities were swapped.

  Own Id: OTP-9046

- Setting scheduler bind type to `unbound` failed if binding of schedulers
  wasn't supported, or if CPU topology wasn't present. This even though the
  documentation stated that it is possible to set the bind type to `unbound`.

  Own Id: OTP-9056 Aux Id: Seq11779

- Two problems were fixed in crash dump: The time left for timers are now shown
  as unsigned integers and the contents of ordered_set ETS tables is no longer
  included.

  Own Id: OTP-9057

- The VM could fail to set IP_TOS and SO_PRIORITY in certain situations, either
  because sockets were supplied as open file descriptors, or because SO_PRIORITY
  by default was set higher than the user can explicitly set it to. Those
  situations are now handled.

  Own Id: OTP-9069

- Wx on MacOS X generated complains on stderr about certain cocoa functions not
  being called from the "Main thread". This is now corrected.

  Own Id: OTP-9081

- Fix a couple typos in driver_entry(3) (thanks to Tuncer Ayaz).

  Own Id: OTP-9085

- Mention that "-detached" implies "-noinput"

  Clarify that specifying "-noinput" is unnecessary if the "-detached" flag is
  given. (thanks to Holger Weiß)

  Own Id: OTP-9086

- A potential problem (found by code inspection) when calling a fun whose code
  was not loaded has been fixed.

  Own Id: OTP-9095

- The emulator could get into a state where it didn't check for I/O.

  Own Id: OTP-9105 Aux Id: Seq11798

- Attempting to create binaries exceeding 2Gb (using for example
  [`term_to_binary/1`](`term_to_binary/1`)) would crash the emulator with an
  attempt to allocate huge amounts of memory. (Thanks to Jon Meredith.)

  Own Id: OTP-9117

- Fix erlang:hibernate/3 on HiPE enabled emulator (Thanks to Paul Guyot)

  Own Id: OTP-9125

### Improvements and New Features

- From this release, the previously experimental halfword emulator is now
  official. It can be enabled by giving the `--enable-halfword-emulator` option
  to the `configure` script.

  The halfword emulator is a 64-bit application, but uses halfwords (32-bit
  words) for all data in Erlang processes, therefore using less memory and being
  faster than the standard 64-bit emulator. The total size of all BEAM code and
  all process data for all processes is limited to 4Gb, but ETS tables and
  off-heap binaries are only limited by the amount of available memory.

  Own Id: OTP-8941

- 32-bit atomic memory operations have been introduced internally in the run
  time system, and are now used where appropriate. There were previously only
  atomic memory operations of word size available. The 32-bit atomic memory
  operations slightly reduce memory consumption, and slightly improve
  performance on 64-bit runtime systems.

  Own Id: OTP-8974

- Performance enhancements for looking up timer-entries and removing timers from
  the wheel.

  Own Id: OTP-8990

- Write accesses to ETS tables have been optimized by reducing the amount of
  atomic memory operations needed during a write access.

  Own Id: OTP-9000

- Strange C coding in the VM made the -D_FORTIFY_SOURCE option to gcc-4.5 react
  badly. The code is now cleaned up so that it's accepted by gcc-4.5.

  Own Id: OTP-9025

- The memory footprint for loaded code has been somewhat reduced (especially in
  the 64-bit BEAM machine).

  Own Id: OTP-9030

- The maximum number of allowed arguments for an Erlang function has been
  lowered from 256 to 255, so that the number of arguments can now fit in a
  byte.

  Own Id: OTP-9049

- Dependency generation for Makefiles has been added to the compiler and erlc.
  See the manual pages for `compile` and `erlc`. (Thanks to Jean-Sebastien
  Pedron.)

  Own Id: OTP-9065

## Erts 5.8.2

### Fixed Bugs and Malfunctions

- Fix format_man_pages so it handles all man sections and remove warnings/errors
  in various man pages.

  Own Id: OTP-8600

- The `configure` command line argument
  [\--enable-ethread-pre-pentium4-compatibility](`e:system:install.md#advanced-configuration-and-build-of-erlang-otp`)
  had no effect. This option is now also automatically enabled if required on
  the build machine.

  Own Id: OTP-8847

- Windows 2003 and Windows XP pre SP3 would sometimes not start the Erlang R14B
  VM at all due to a bug in the cpu topology detection. The bug affects Windows
  only, no other platform is even remotely affected. The bug is now corrected.

  Own Id: OTP-8876

- The HiPE run-time in the 64-bit emulator could do a 64-bit write to a 32-bit
  struct field. It happened to be harmless on Intel/AMD processors. Corrected.
  (Thanks to Mikael Pettersson.)

  Own Id: OTP-8877

- A bug in [erl_drv_tsd_get()](erl_driver.md#erl_drv_tsd_get) and
  [enif_tsd_get()](erl_nif.md#enif_tsd_get) could cause an emulator crash. These
  functions are currently not used in OTP. That is, the crash only occur on
  systems with user implemented NIF libraries, or drivers that use one of these
  functions.

  Own Id: OTP-8889

- Calling `erlang:system_info({cpu_topology, CpuTopologyType})` with another
  `CpuTopologyType` element than one of the documented atoms `defined`,
  `detected`, or `used` caused an emulator crash. (Thanks to Paul Guyot)

  Own Id: OTP-8914

- The ERTS internal rwlock implementation could get into an inconsistent state.
  This bug was very seldom triggered, but could be during heavy contention. The
  bug was introduced in R14B (erts-5.8.1).

  The bug was most likely to be triggered when using the `read_concurrency`
  option on an ETS table that was frequently accessed from multiple processes
  doing lots of writes and reads. That is, in a situation where you typically
  don't want to use the `read_concurrency` option in the first place.

  Own Id: OTP-8925 Aux Id: OTP-8544

- Tracing to port could cause an emulator crash when unloading the trace driver.

  Own Id: OTP-8932

- Removed use of CancelIoEx on Windows that had been shown to cause problems
  with some drivers.

  Own Id: OTP-8937

- The fallback implementation used when no native atomic implementation was
  found did not compile. (Thanks to Patrick Baggett, and Tuncer Ayaz)

  Own Id: OTP-8944

- Some integer values used during load balancing could under rare circumstances
  wrap causing a load unbalance between schedulers.

  Own Id: OTP-8950

- The windows VM now correctly handles appending to large files (> 4GB).

  Own Id: OTP-8958

- Name resolving of IPv6 addresses has been implemented for Windows versions
  that support it. The use of ancient resolver flags (AI_V4MAPPED |
  AI_ADDRCONFIG) to the getaddrinfo() function has been removed since e.g
  FreeBSD regard mapped IPv4 addresses to be a security problem and the
  semantics of the address configured flag is uncertain.

  Own Id: OTP-8969

### Improvements and New Features

- The help texts produced by the `configure` scripts in the top directory and in
  the erts directory have been aligned and cleaned up.

  Own Id: OTP-8859

- When the runtime system had fewer schedulers than logical processors, the
  system could get an unnecessarily large amount reader groups.

  Own Id: OTP-8861

- `run_rel` has been updated to support Solaris's /dev/ptmx device and to load
  the necessary STREAMS modules so that `to_erl` can provide terminal echo of
  keyboard input. (Thanks to Ryan Tilder.)

  Own Id: OTP-8878

- The Erlang VM now supports Unicode filenames. The feature is turned on by
  default on systems where Unicode filenames are mandatory (Windows and MacOSX),
  but can be enabled on other systems with the '+fnu' emulator option. Enabling
  the Unicode filename feature on systems where it is not default is however
  considered experimental and not to be used for production. Together with the
  Unicode file name support, the concept of "raw filenames" is introduced, which
  means filenames provided without implicit unicode encoding translation. Raw
  filenames are provided as binaries, not lists. For further information, see
  stdlib users guide and the chapter about using Unicode in Erlang. Also see the
  file module manual page.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8887

- Buffer overflows have been prevented in `erlc`, `dialyzer`, `typer`,
  `run_test`, `heart`, `escript`, and `erlexec`.

  (Thanks to Michael Santos.)

  Own Id: OTP-8892

- The runtime system is now less eager to suspend processes sending messages
  over the distribution. The default value of the distribution buffer busy limit
  has also been increased from 128 KB to 1 MB. This in order to improve
  throughput.

  Own Id: OTP-8901

- The distribution buffer busy limit can now be configured at system startup.
  For more information see the documentation of the `erl`
  [\+zdbbl](erl_cmd.md#%2Bzdbbl) command line flag. (Thanks to Scott Lystig
  Fritchie)

  Own Id: OTP-8912

- The inet driver internal buffer stack implementation has been rewritten in
  order to reduce lock contention.

  Own Id: OTP-8916

- New ETS option `compressed`, to enable a more compact storage format at the
  expence of heavier table operations. For test and evaluation, `erl +ec` can be
  used to force compression on all ETS tables.

  Own Id: OTP-8922 Aux Id: seq11658

- There is now a new function inet:getifaddrs/0 modeled after C library function
  getifaddrs() on BSD and LInux that reports existing interfaces and their
  addresses on the host. This replaces the undocumented and unsupported
  inet:getiflist/0 and inet:ifget/2.

  Own Id: OTP-8926

- Support for detection of CPU topology and binding of schedulers on FreeBSD 8
  have been added. (Thanks to Paul Guyot)

  Own Id: OTP-8939

- Several bugs related to hibernate/3 and HiPE have been corrected. (Thanks to
  Paul Guyot.)

  Own Id: OTP-8952

- Support for soft and hard links on Windows versions and filesystems that
  support them is added.

  Own Id: OTP-8955

- The win32 virtual machine is now linked large address aware. his allows the
  Erlang VM to use up to 3 gigs of address space on Windows instead of the
  default of 2 gigs.

  Own Id: OTP-8956

## Erts 5.8.1.2

### Fixed Bugs and Malfunctions

- Fix that the documentation top index generator can handle an Ericsson internal
  application group.

  Own Id: OTP-8875

- In embedded mode, on_load handlers that called `code:priv_dir/1` or other
  functions in `code` would hang the system. Since the `crypto` application now
  contains an on_loader handler that calls `code:priv_dir/1`, including the
  `crypto` application in the boot file would prevent the system from starting.

  Also extended the `-init_debug` option to print information about on_load
  handlers being run to facilitate debugging.

  Own Id: OTP-8902 Aux Id: seq11703

## Erts 5.8.1.1

### Fixed Bugs and Malfunctions

- Windows 2003 and Windows XP pre SP3 would sometimes not start the Erlang R14B
  VM at all due to a bug in the cpu topology detection. The bug affects Windows
  only, no other platform is even remotely affected. The bug is now corrected.

  Own Id: OTP-8876

## Erts 5.8.1

### Fixed Bugs and Malfunctions

- Very small floating point numbers generated errors when converting from list
  to float in some versions of the VM, this is now corrected so that i.e.
  list_to_float("1.0e-324"). returns 0.0 in all versions of Erlang.

  Own Id: OTP-7178

- Windows Vista and Windows 7 file system virtualization, which makes "old
  style" windows programs execute in a file system sandbox, was previously
  unintentionally turned on for the Erlang VM. This is now corrected so that
  i.e. writes to C:\\Program Files\\. without administrator privileges will
  fail.

  Own Id: OTP-7405

- Fix faulty 64-bit integer term output from drivers. Large 64-bits integers did
  not generate correct bignums and could even cause emulator crash. Only affects
  drivers using ERL_DRV_INT64 or ERL_DRV_UINT64, introduced in R13B03.

  Own Id: OTP-8716

- Fixed: inet:setopts(S, \[\{linger,\{true,2\}\}]) returned \{error,einval\} for
  SCTP sockets. The inet_drv had a bug when checking the option size.

  Own Id: OTP-8726 Aux Id: seq11617

- Fix libm linking with --as-needed flag

  When building with "--as-needed" linker flags on Linux the build will fail.
  This has now been fixed.

  (Thanks to Christian Faulhammer)

  Own Id: OTP-8728

- gen_udp:connect/3 was broken for SCTP enabled builds. It did not detect remote
  end errors as it should.

  Own Id: OTP-8729

- Reduce the risk of integer wrapping in bin vheap size counting.

  The vheap size series will now use the golden ratio instead of doubling and
  fibonacci sequences.

  Own Id: OTP-8730

- ETS ordered_set containing `[]` as key could cause strange thing to happen,
  like an infinite hanging `ets:select`.

  Own Id: OTP-8732

- reference() has been substituted for ref() in the documentation.

  Own Id: OTP-8733

- When a native compiled module called a not loaded non-native compiled module
  that had an on_load function, the export entries were trashed after code
  loading so on the next call from the native compiled module to the non-native
  compiled the emulator crashed. This bug has now been fixed.

  Own Id: OTP-8736

- HiPE-enabled Erlang VMs running on BSD systems sometimes generated messages
  like "Yikes\! erts_alloc() returned misaligned address 0x8016a512c". Fixed.
  (Thanks to Mikael Pettersson.)

  Own Id: OTP-8769

- A race condition in `erts_poll()` could cause delay of poll for I/O.

  Own Id: OTP-8773

- Removed some potential vulnerabilities from the Erlang Port Mapper Daemon
  (epmd) and straightened up access control. Also removed hazardous interfaces
  allowing anyone on a machine to forcefully unregister other nodes. This means
  that the ei_unregister/erl_unregister interfaces in erl_interface is rendered
  not only error prone and mystifying as before, but totally ineffective. The
  old behaviour of unchecked node unregistering can be restored if needed, see
  epmd documentation for details.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8780

- Building in a source tree without prebuilt platform independent build results
  failed on the SSL examples when:

  - cross building. This has been solved by not building the SSL examples during
    a cross build.
  - building on Windows.

  Own Id: OTP-8791

- inet:getsockopt for SCTP sctp_default_send_param had a bug to not initialize
  required feilds causing random answers. It is now corrected.

  Own Id: OTP-8795 Aux Id: seq11655

- The hipe_bifs:get_hrvtime/0 BIF now always returns a real value even if the
  "perfctr" Linux kernel extension is not available. It used to return a dummy
  value. (Thanks to Mikael Pettersson.)

  Own Id: OTP-8798

- Calling a native-code compiled module with an `on_load` function could cause a
  crash. (Thanks to Mikael Pettersson.)

  Own Id: OTP-8799

- The emulator could crash while writing a crash dump if native-compiled modules
  had been loaded. (Thanks to Paul Guyot.)

  Own Id: OTP-8801

- The garbage collector could crash if invoked from native-compiled code after a
  call to a BIF. (Thanks to Paul Guyot.)

  Own Id: OTP-8821

- A rare memory leak in binary:matches is removed

  Own Id: OTP-8823

- For a socket in the HTTP packet mode, the return value from `gen_tcp:recv/2,3`
  if there is an error in the header will be `{ok,{http_error,String}}` instead
  of `{error,{http_error,String}}` to be consistent with `ssl:recv/2,3`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8831

### Improvements and New Features

- ets:select_reverse/\{1,2,3\} are now documented.

  Own Id: OTP-7863

- External format of integers changed to make full use of all 32 bits of
  INTEGER_EXT. This is a compatible change as old code can read full 32-bit
  integers but only produce 28-bit integers as INTEGER_EXT.

  Own Id: OTP-8540 Aux Id: seq11534

- Large parts of the `ethread` library have been rewritten. The `ethread`
  library is an Erlang runtime system internal, portable thread library used by
  the runtime system itself.

  Most notable improvement is a reader optimized rwlock implementation which
  dramatically improve the performance of read-lock/read-unlock operations on
  multi processor systems by avoiding ping-ponging of the rwlock cache lines.
  The reader optimized rwlock implementation is used by miscellaneous rwlocks in
  the runtime system that are known to be read-locked frequently, and can be
  enabled on ETS tables by passing the
  [\{read_concurrency, true\}](`m:ets#new_2_read_concurrency`) option upon table
  creation. See the documentation of `ets:new/2` for more information. The
  reader optimized rwlock implementation can be fine tuned when starting the
  runtime system. For more information, see the documentation of the
  [\+rg](erl_cmd.md#%2Brg) command line argument of `erl`.

  There is also a new implementation of rwlocks that is not optimized for
  readers. Both implementations interleaves readers and writers during
  contention as opposed to, e.g., the NPTL (Linux) pthread rwlock implementation
  which use either a reader or writer preferred strategy. The reader/writer
  preferred strategy is problematic since it starves threads doing the
  non-preferred operation.

  The new rwlock implementations in general performs better in ERTS than common
  pthread implementations. However, in some extremely heavily contended cases
  this is not the case. Such heavy contention can more or less only appear on
  ETS tables. This when multiple processes do very large amounts of write locked
  operations simultaneously on the same table. Such use of ETS is bad regardless
  of rwlock implementation, will never scale, and is something we strongly
  advise against.

  The new rwlock implementations depend on atomic operations. If no native
  atomic implementation is found, a fallback solution will be used. Using the
  fallback implies a performance degradation. That is, it is more important now
  than before to build OTP with a native atomic implementation.

  The `ethread` library contains native atomic implementations for, x86 (32 and
  64 bit), powerpc (32 bit), sparc V9 (32 and 64 bit), and tilera (32 bit). On
  other hardware gcc's builtin support for atomic memory access will be used if
  such exists. If no such support is found, `configure` will warn about no
  atomic implementation available.

  The `ethread` library can now also use the `libatomic_ops` library for atomic
  memory accesses. This makes it possible for the Erlang runtime system to
  utilize optimized native atomic operations on more platforms than before. If
  `configure` warns about no atomic implementation available, try using the
  `libatomic_ops` library. Use the
  [\--with-libatomic_ops=PATH](`e:system:install.md#advanced-configuration-and-build-of-erlang-otp`)
  `configure` command line argument when specifying where the `libatomic_ops`
  installation is located. The `libatomic_ops` library can be downloaded from:
  [http://www.hpl.hp.com/research/linux/atomic_ops/](http://www.hpl.hp.com/research/linux/atomic_ops/)

  The changed API of the `ethread` library has also caused modifications in the
  Erlang runtime system. Preparations for the to come "delayed deallocation"
  feature has also been done since it depends on the `ethread` library.

  _Note_: When building for x86, the `ethread` library will now use instructions
  that first appeared on the pentium 4 processor. If you want the runtime system
  to be compatible with older processors (back to 486) you need to pass the
  [\--enable-ethread-pre-pentium4-compatibility](`e:system:install.md#advanced-configuration-and-build-of-erlang-otp`)
  `configure` command line argument when configuring the system.

  Own Id: OTP-8544

- erlang:localtime_to_universaltime(\{\{2008, 8, 1\}, \{0, 0, 0\}\},true) when
  TZ=UTC now behaves consistently on all Unix platforms.

  The problem fixed was originally reported by Paul Guyot on erlang-bugs mailing
  list:

  http://www.erlang.org/pipermail/erlang-bugs/2008-November/001077.html

  Own Id: OTP-8580

- Optimization reducing memory consumption by two words per ETS object.

  Own Id: OTP-8737

- Fixes for unsupported halfword-emulator

  Own Id: OTP-8745

- NIF 64-bit integer support; `enif_get_int64`, `enif_get_uint64`,
  `enif_make_int64`, `enif_make_uint64`.

  Own Id: OTP-8746

- Alignment of trailing data in messages has been adjusted. This in order to be
  able to pass data of any type as trailing data in the future.

  Own Id: OTP-8754

- The obsolete/driver.h header file has been removed. It has been obsolete and
  deprecated since R8B. Drivers that still include obsolete/driver.h must be
  updated to include erl_driver.h.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8758

- Added erlang:system_info(build_type) which makes it easier to chose drivers,
  NIF libraries, etc based on build type of the runtime system.

  The NIF library for crypto can now be built for valgrind and/or debug as
  separate NIF libraries that will be automatically loaded if the runtime system
  has been built with a matching build type.

  Own Id: OTP-8760

- Further lessened the memory requirements of ETS objects.

  Own Id: OTP-8762

- The broken elib_malloc alternate memory allocator has been removed.
  `erlang:system_info(elib_malloc)` will always return `false`, and in R15,
  `erlang:system_info(elib_malloc)` will fail with a `badarg` exception.

  Own Id: OTP-8764

- Calling `erlang:system_info/1` with the new argument `update_cpu_info` will
  make the runtime system reread and update the internally stored CPU
  information. For more information see the documentation of
  [erlang:system_info(update_cpu_info)](`m:erlang#system_info_update_cpu_info`).

  The CPU topology is now automatically detected on Windows systems with less
  than 33 logical processors. The runtime system will now, also on Windows, by
  default bind schedulers to logical processors using the `default_bind` bind
  type if the amount of schedulers is at least equal to the amount of logical
  processors configured, binding of schedulers is supported, and a CPU topology
  is available at startup.

  Own Id: OTP-8765

- The SMP ERTS internal child waiter thread used on Linux system with NPTL was
  unintentionally disabled during cross compilation rewrites (OTP-8323 in
  R13B03). It has now been re-enabled. Enabling it again gives a slight
  performance improvement.

  Own Id: OTP-8774

- `epmd` used to generate a message to the syslog when it started up, which
  could be annoying. This has been changed to only generate the message if the
  debug switch is given. (Thanks to Michael Santos.)

  Own Id: OTP-8775

- The scheduler wakeup threshold is now possible to adjust at system boot. For
  more information see the [\+swt](erl_cmd.md#%2Bswt) command line argument of
  `erl`.

  Own Id: OTP-8811

- The undocumented function inet:ifget/2 has been improved to return interface
  hardware address (MAC) on platforms supporting getaddrinfo() (such as BSD
  unixes). Note it still does not work on all platforms for example not Windows
  nor Solaris, so the function is still undocumented.

  Buffer overflow and field init bugs for inet:ifget/2 and inet:getservbyname/2
  has also been fixed.

  Thanks to Michael Santos.

  Own Id: OTP-8816

- Optimizations for MIPS when using gcc atomics. (Thanks to Steve Vinoski)

  Own Id: OTP-8834

- Lock optimization in timer functionality.

  Own Id: OTP-8835

### Known Bugs and Problems

- Fix epmd and build environment to build on VxWorks

  Own Id: OTP-8838

## Erts 5.8

### Fixed Bugs and Malfunctions

- Driver threads, such as async threads, using
  [port data locks](erl_driver.md#ErlDrvPDL) peeked at the port status field
  without proper locking when looking up the driver queue.

  Own Id: OTP-8475

- The use of `mmap()` was unnecessarily disabled when cross compiling.

  The `configure` arguments `--with-ssl`, and `--with-odbc` refused to accept
  libraries outside of `$erl_xcomp_sysroot` when cross compiling for no good
  reason.

  The `configure` argument `--with-odbc` didn't handle the value `yes` correct.

  The `configure` arguments `--with-odbc`, and `--without-odbc` have also been
  added to the configure help.

  (Thanks to Steve Vinoski for reporting these issues)

  Own Id: OTP-8484

- A call to the BIF [`unregister(RegName)`](`unregister/1`) when a port had the
  name `RegName` registered in the runtime system without SMP support caused a
  runtime system crash. (Thanks to Per Hedeland for the bugfix and test case.)

  Own Id: OTP-8487

- The runtime system crashed if fewer logical processors were found than
  reported by `sysconf( SC_NPROCESSORS_CONF)`.

  Own Id: OTP-8549

- Fix memory management bug causing crash of non-SMP emulator with async threads
  enabled. The bug did first appear in R13B03.

  Own Id: OTP-8591 Aux Id: seq11554

- Port locks could be prematurely destroyed.

  Own Id: OTP-8612

- The `empd` program could loop and consume 100% CPU time if an unexpected error
  occurred in `listen()` or `accept()`. Now `epmd` will terminate if a
  non-recoverable error occurs. (Thanks to Michael Santos.)

  Own Id: OTP-8618

- When kernel poll has been enabled, a livelock could in rare circumstances
  occur. Problem reported by Chetan Ahuja, fix by Mikael Pettersson.

  Own Id: OTP-8632

- Windows: Closing port of program that stalled without reading all data could
  deadlock scheduler thread.

  Own Id: OTP-8641

- On some combination of Montavista Linux on Cavium Octeon processors, some
  socket-related system calls returned other numbers than -1 for errors. This
  caused a core dump in inet_drv.c. Now the code works around this problem.

  Own Id: OTP-8654

- Missing memory barriers in `erts_poll()` could cause the runtime system to
  hang indefinitely.

  Own Id: OTP-8658

- `ethr_rwmutex_tryrlock()` acquired and refused to acquire a lock with inverted
  logic. The lock was however never acquired in a thread unsafe manner. (Thanks
  to JR Zhang for noting this issue)

  Own Id: OTP-8662

- Extreme combinations of register/unregister in a highly parallel SMP
  application could crash the VM. The error is corrected.

  Own Id: OTP-8663

- On Windows, files are now opened with FILE_SHARE_DELETE to get closer to Unix
  semantics.

  Own Id: OTP-8667

- `erlang:system_info(multi_scheduling)` sometimes erroneously returned
  `enabled` when it should have returned `blocked`.

  Own Id: OTP-8675

- Fix bug causing `erlang:decode_packet` and `enif_make_string` to generate
  faulty strings with negative character values for ascii values larger
  than 127. (Thanks to Paul Guyot)

  Own Id: OTP-8685

- [`open_port/2`](`open_port/2`) with the `spawn` and `spawn_executable` options
  can include an `{env,Env}` option. In some cases unsetting variables would not
  work on Unix (typically if more variables were unset than were actually
  present in the environment).

  Own Id: OTP-8701

- A user defined CPU topology set via a call to
  [erlang:system_flag(cpu_topology, CPUTopology)](`m:erlang#system_flag_cpu_topology`)
  was not properly verified, and could in worst case cause an emulator crash.
  The emulator crash could only occur when a user defined CPU topology already
  existed and was redefined.

  Own Id: OTP-8710

### Improvements and New Features

- The grammar for match specifications in ERTS users guide only described the
  tracing dialect of match specifications. An informal grammar for the ETS
  dialect is added.

  Own Id: OTP-8086 Aux Id: seq11333

- The module binary from EEP31 (and EEP9) is implemented.

  Own Id: OTP-8217

- New NIF API function `enif_make_new_binary`

  Own Id: OTP-8474

- The guard BIF [`is_boolean/1`](`is_boolean/1`) (introduced in R10B) has now
  been included in the lists of BIFs allowed in guards in the Reference Manual.

  Own Id: OTP-8477

- Added function `zip:foldl/3` to iterate over zip archives.

  Added functions to create and extract escripts. See `escript:create/2` and
  `escript:extract/2`.

  The undocumented function `escript:foldl/3` has been removed. The same
  functionality can be achieved with the more flexible functions
  `escript:extract/2` and `zip:foldl/3`.

  Record fields has been annotated with type info. Source files as been adapted
  to fit within 80 chars and trailing whitespace has been removed.

  Own Id: OTP-8521

- A regular expression with many levels of parenthesis could cause a buffer
  overflow. That has been corrected. (Thanks to Michael Santos.)

  Own Id: OTP-8539

- `erlang:decode_packet(httph_bin,..)` could return corrupt header strings or
  even crash the VM. This has been fixed. It only happened on 32-bit VM if the
  header name was unknown and between 16 and 20 characters long. Sockets with
  simular `packet` option did not suffer from this bug.

  Own Id: OTP-8548

- New NIF features:

  - Send messages from a NIF, or from thread created by NIF, to any local
    process (`enif_send`)
  - Store terms between NIF calls (`enif_alloc_env`, `enif_make_copy`)
  - Create binary terms with user defined memory management
    (`enif_make_resource_binary`)

  And some incompatible changes made to the API. For more information see the
  warning text in [erl_nif(3)](erl_nif.md).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8555

- If the '`fop`' program (needed for building PDF files) cannot not be found, it
  is now possible to build the HTML and man pages anyway (there will also be
  dummy PDF files with no real content created). (Thanks to Tuncer Ayaz.)

  Own Id: OTP-8559

- When defining macros the closing right parenthesis before the dot is now
  mandatory.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8562

- Local and imported functions now override the auto-imported BIFs when the
  names clash. The pre R14 behaviour was that auto-imported BIFs would override
  local functions. To avoid that old programs change behaviour, the following
  will generate an error:

  - Doing a call without explicit module name to a local function having a name
    clashing with the name of an auto-imported BIF that was present (and
    auto-imported) before OTP R14A
  - Explicitly importing a function having a name clashing with the name of an
    autoimported BIF that was present (and autoimported) before OTP R14A
  - Using any form of the old compiler directive `nowarn_bif_clash`

  If the BIF was added or auto-imported in OTP R14A or later, overriding it with
  an import or a local function will only result in a warning,

  To resolve clashes, you can either use the explicit module name `erlang` to
  call the BIF, or you can remove the auto-import of that specific BIF by using
  the new compiler directive `-compile({no_auto_import,[F/A]}).`, which makes
  all calls to the local or imported function without explicit module name pass
  without warnings or errors.

  The change makes it possible to add auto-imported BIFs without breaking or
  silently changing old code in the future. However some current code
  ingeniously utilizing the old behaviour or the `nowarn_bif_clash` compiler
  directive, might need changing to be accepted by the compiler.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8579

- A bug in re that could cause certain regular expression matches never to
  terminate is corrected. (Thanks to Michael Santos and Gordon Guthrie.)

  Own Id: OTP-8589

- The erlang:open_port spawn and spawn_executable directives can include an
  `{env,Env}` directive to set up environment variables for the spawned process.
  A bug prevented applications from using `{env,Env}` to set an environment
  variable whose value ended with a '`=`' (equal sign) character; the trailing
  equal sign was mistaken as an indication that an environment variable was to
  be cleared from the environment of the spawned process. (Thanks to Steve
  Vinoski.)

  Own Id: OTP-8614

- `receive` statements that can only read out a newly created reference are now
  specially optimized so that it will execute in constant time regardless of the
  number of messages in the receive queue for the process. That optimization
  will benefit calls to `gen_server:call()`. (See `gen:do_call/4` for an example
  of a receive statement that will be optimized.)

  Own Id: OTP-8623

- The functions file:advise/4 and file:datasync/1 have been added. (Thanks to
  Filipe David Manana.)

  Own Id: OTP-8637

- New NIF API functions: `enif_make_atom_len`, `enif_make_existing_atom_len`,
  `enif_make_string_len`, `enif_get_atom_length`, `enif_get_list_length`,
  `enif_is_list`, `enif_is_tuple` (by Tuncer Ayaz)

  Own Id: OTP-8640

- Support for using gcc's built-in functions for atomic memory access has been
  added. This functionality will be used if available and no other native atomic
  implementation in ERTS is available.

  Own Id: OTP-8659

- The number of spinlocks used when implementing atomic fall-backs when no
  native atomic implementation is available has been increased from 16 to 1024.

  Own Id: OTP-8660

- Writer preferred pthread read/write locks has been enabled on Linux.

  Own Id: OTP-8661

- The runtime system will by default bind schedulers to logical processors using
  the `default_bind` bind type if the amount of schedulers are at least equal to
  the amount of logical processors configured, binding of schedulers is
  supported, and a CPU topology is available at startup.

  _NOTE:_ If the Erlang runtime system is the only operating system process that
  binds threads to logical processors, this improves the performance of the
  runtime system. However, if other operating system processes (as for example
  another Erlang runtime system) also bind threads to logical processors, there
  might be a performance penalty instead. If this is the case you, are are
  advised to unbind the schedulers using the [\+sbtu](erl_cmd.md#%2Bsbt) command
  line argument, or by invoking
  [erlang:system_flag(scheduler_bind_type, unbound)](`m:erlang#system_flag_scheduler_bind_type`).

  Own Id: OTP-8666

- The recently added BIFs erlang:min/2, erlang:max/2 and erlang:port_command/3
  are now auto-imported (as they were originally intended to be). Due to the
  recent compiler change (OTP-8579), the only impact on old code defining it's
  own min/2, max/2 or port_command/3 functions will be a warning, the local
  functions will still be used. The warning can be removed by using
  -compile(\{no_auto_import,\[min/2,max/2,port_command/3]\}). in the source
  file.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8669 Aux Id: OTP-8579

- There is a new option 'exclusive' to file:open/2 that uses the OS O_EXCL flag
  where supported to open the file in exclusive mode.

  Own Id: OTP-8670

- Now, binary_to_term/2 is auto-imported. This will cause a compile warning if
  and only if a module has got a local function with that name.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8671

- Alignment of scheduler data and run queues were adjusted.

  Own Id: OTP-8673

- Call time breakpoint tracing

  - Introduce a `call_time` option to `erlang:trace_pattern/3`.This option
    enables call time breakpoint tracing on code that is executed by processes
    with call tracing enabled. Call time tracing stores the number of calls and
    the time spent of each function with this trace pattern enabled. The
    information can be retrieved with `erlang:trace_info/2`
  - Add a scheduler array for BpData. To solve the issue of multiple schedulers
    constantly updating the head pointer to the bp data wheel, each scheduler
    now has its own entrypoint to the wheel. This head pointer can be updated
    without a locking being taken.

  Teach call count tracing to use atomics

  - Call count previously used a global lock for accessing and writing its
    counter in the breakpoint. This is now changed to atomics instead.
  - The change will let call count tracing and cprof to scale better when
    increasing the number of schedulers.

  Own Id: OTP-8677

- `eprof` has been reimplemented with support in the Erlang virtual machine and
  is now both faster (i.e. slows down the code being measured less) and scales
  much better. In measurements we saw speed-ups compared to the old eprof
  ranging from 6 times (for sequential code that only uses one scheduler/core)
  up to 84 times (for parallel code that uses 8 cores).

  Note: The API for the `eprof` has been cleaned up and extended. See the
  documentation.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8706

## Erts 5.7.5.2

### Known Bugs and Problems

- Two bugs in gen_sctp has been corrected: getopts/setopts hence also send could
  only be called from socket owner, and options 'linger', 'rcvbuf' and 'sndbuf'
  was read from wrong protocol layer hence read wrong values by getopts.

  Own Id: OTP-9544

## Erts 5.7.5.1

### Fixed Bugs and Malfunctions

- Driver threads, such as async threads, using
  [port data locks](erl_driver.md#ErlDrvPDL) peeked at the port status field
  without proper locking when looking up the driver queue.

  Own Id: OTP-8475

- A call to the BIF [`unregister(RegName)`](`unregister/1`) when a port had the
  name `RegName` registered in the runtime system without SMP support caused a
  runtime system crash. (Thanks to Per Hedeland for the bugfix and test case.)

  Own Id: OTP-8487

- Fix memory management bug causing crash of non-SMP emulator with async threads
  enabled. The bug did first appear in R13B03.

  Own Id: OTP-8591 Aux Id: seq11554

## Erts 5.7.5

### Fixed Bugs and Malfunctions

- Fix `binary_to_term` crash on compressed term with corrupt size field.

  Own Id: OTP-8336

- Processes and/or ports could get stuck on a blocked scheduler when
  `erlang:system_flag(multi_scheduling, block)` was used.

  Processes and/or ports could get stuck on an offline scheduler when schedulers
  online were reduced using
  `erlang:system_flag(schedulers_online, SchedulersOnline)`.

  Own Id: OTP-8342

- Building on Windows will now work if the paths to `mc.exe` and `rc.exe`
  contain spaces. The README.win32 file has been updated with some information
  about building using Visual Studio 2008. (Thanks to Andrew Thompson.)

  Own Id: OTP-8345

- EPMD now correctly handles the extra data field which can be given in the
  ALIVE2_REQ request and retrieved in the PORT2_RESP response. (Thanks to Klas
  Johansson.)

  Own Id: OTP-8361

- The configure test for reliable floating point exceptions has been update to
  work on modern versions of Mac OS X. (Thanks to Trannie Carter.)

  Own Id: OTP-8368

- ERTS makefiles used to detect the use of a gcc C compiler by checking if CC
  equaled gcc. That is, the makefiles failed to detect gcc C compilers with
  other command line names than gcc. \`configure' now substitute GCC into the
  makefiles. If CC is a gcc C compiler, GCC will have the value yes. (Thanks to
  Jean-Sébastien Pédron)

  Own Id: OTP-8373

- ETS bug causing the `memory` counter from `ets:info` for ordered_set's to
  sometimes get out of synch and absurdly high.

  Own Id: OTP-8377 Aux Id: seq11442

- Immediately repeated multi-scheduling block/unblock cycles using
  `erlang:system_flag(multi_scheduling, block | unblock)` could deadlock the
  runtime system.

  Own Id: OTP-8386

- A number of bugs concerning re and unicode are corrected:

  re:compile no longer loses unicode option, which also fixes bug in re:split.

  re:replace now handles unicode charlist replacement argument

  re:replace now handles unicode RE charlist argument correctly

  re:replace now handles binary unicode output correctly when nothing is
  replaced.

  Most code, testcases and error isolation done by Rory Byrne.

  Own Id: OTP-8394

- The loading of native code was not properly atomic in the SMP emulator, which
  could cause crashes. Also a per-MFA information table for the native code has
  now been protected with a lock since it turns that it could be accessed
  concurrently in the SMP emulator. (Thanks to Mikael Pettersson.)

  Own Id: OTP-8397

- Fix processes in exiting status that are about to be scheduled, to not be
  allowed to garbage collect.

  Own Id: OTP-8420

- Removed bogus "\\011" character sequence in documentation.

  Own Id: OTP-8422

- The resolver routines failed to look up the own node name as hostname, if the
  OS native resolver was erroneously configured, bug reported by Yogish Baliga,
  now fixed.

  The resolver routines now tries to parse the hostname as an IP string as most
  OS resolvers do, unless the native resolver is used.

  The DNS resolver inet_res and file resolver inet_hosts now do not read OS
  configuration files until they are needed. Since the native resolver is
  default, in most cases they are never needed.

  The DNS resolver's automatic updating of OS configuration file data
  (/etc/resolv.conf) now uses the 'domain' keyword as default search domain if
  there is no 'search' keyword.

  Own Id: OTP-8426 Aux Id: OTP-8381

- The re module: A regular expression with an option change at the start of a
  pattern that had top-level alternatives could cause overwriting and/or a
  crash. (Thanks to Michael Santos.)

  Own Id: OTP-8438

- Harmless buffer overflow by one byte in asn1 and ram_file_drv.

  Own Id: OTP-8451

### Improvements and New Features

- Improved GC performance after BIF/NIF call when a lot of heap fragments was
  created. This will mainly benefit NIFs that return large compound terms.

  Own Id: OTP-8240

- Incompatible changes in the experimental NIF feature.

  - Changed the NIF function prototypes in order to allow more than 3 function
    arguments.
  - `enif_get_data` renamed as `enif_priv_data`.
  - `enif_make_string` got a third argument for character encoding.
  - The return value of `erlang:load_nif/2` on error changed.

  Read more in the documentation of `erl_nif` and `erlang:load_nif/2`

  .

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8304

- Cross compilation improvements and other build system improvements.

  Most notable:

  - Lots of cross compilation improvements. The old cross compilation support
    was more or less non-existing as well as broken. Please, note that the cross
    compilation support should still be considered as experimental. Also note
    that old cross compilation configurations cannot be used without
    modifications. For more information on cross compiling Erlang/OTP see the
    `$ERL_TOP/INSTALL-CROSS.md` file.
  - Support for staged install using
    [DESTDIR](http://www.gnu.org/prep/standards/html_node/DESTDIR.html). The old
    broken `INSTALL_PREFIX` has also been fixed. For more information see the
    `$ERL_TOP/INSTALL.md` file.
  - Documentation of the `release` target of the top `Makefile`. For more
    information see the `$ERL_TOP/INSTALL.md` file.
  - `make install` now by default creates relative symbolic links instead of
    absolute ones. For more information see the `$ERL_TOP/INSTALL.md` file.
  - `$ERL_TOP/configure --help=recursive` now works and prints help for all
    applications with `configure` scripts.
  - Doing `make install`, or `make release` directly after `make all` no longer
    triggers miscellaneous rebuilds.
  - Existing bootstrap system is now used when doing `make install`, or
    `make release` without a preceding `make all`.
  - The `crypto` and `ssl` applications use the same runtime library path when
    dynamically linking against `libssl.so` and `libcrypto.so`. The runtime
    library search path has also been extended.
  - The `configure` scripts of Erl_interface and ODBC now search for thread
    libraries and thread library quirks the same way as ERTS do.
  - The `configure` script of the `odbc` application now also looks for odbc
    libraries in `lib64` and `lib/64` directories when building on a 64-bit
    system.
  - The `config.h.in` file in the `erl_interface` application is now
    automatically generated in instead of statically updated which reduces the
    risk of `configure` tests without any effect.

  (Thanks to Henrik Riomar for suggestions and testing)

  (Thanks to Winston Smith for the AVR32-Linux cross configuration and testing)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8323

- NIF improvements:

  - Driver API for multi-threading made available for NIFs.
  - Support for mempory managed (garbage collected) resource objects. A way to
    pass "pointers" to native data structures between C and Erlang in a safe
    way.
  - Support for references, floats and term comparison.
  - Various new functions, like `enif_inspect_iolist_as_binary`,
    `enif_make_sub_binary`, `enif_get_string`, `enif_get_atom`,
    `enif_make_tuple_from_array`, `enif_make_list_from_array`,
    `enif_make_existing_atom`.

  Own Id: OTP-8335

- Minor alignment adjustments of scheduler specific data.

  Own Id: OTP-8341

- The documentation is now possible to build in an open source environment after
  a number of bugs are fixed and some features are added in the documentation
  build process.

  \- The arity calculation is updated.

  \- The module prefix used in the function names for bif's are removed in the
  generated links so the links will look like
  "http://www.erlang.org/doc/man/erlang.html#append_element-2" instead of
  "http://www.erlang.org/doc/man/erlang.html#erlang:append_element-2".

  \- Enhanced the menu positioning in the html documentation when a new page is
  loaded.

  \- A number of corrections in the generation of man pages (thanks to Sergei
  Golovan)

  \- The legal notice is taken from the xml book file so OTP's build process can
  be used for non OTP applications.

  Own Id: OTP-8343

- There is new `erlang:binary_to_term/2` BIF that takes an option list. The
  option `safe` can be used to prevent creation of resources that are not
  garbage collected (such as atoms). (Thanks to Jayson Vantuyl.)

  Own Id: OTP-8367

- The default settings for garbage collection of binaries has been adjusted to
  be less aggressive than in R13B03. It is now also possible configure the
  settings for binary GC. See the documentation for `spawn_opt/2-5`,
  `erlang:system_info/1`, `erlang:system_flag/2`, `process_flag/2-3`,
  `erlang:trace/3`, and the documentation for `erl` for the new command line
  options `+hms` and `+hmbs`.

  Own Id: OTP-8370

- A bug causing memory corruption in re:run() has been corrected. (Thanks to
  Yamashina Hio.)

  Own Id: OTP-8375

- The `-Werror` option for `erlc` and the compiler option `warnings_as_errors`
  will cause warnings to be treated as errors. (Thanks to Christopher Faulet.)

  Own Id: OTP-8382

- Explicit top directories in archive files are now optional.

  For example, if an archive (app-vsn.ez) just contains an app-vsn/ebin/mod.beam
  file, the file info for the app-vsn and app-vsn/ebin directories are faked
  using the file info from the archive file as origin. The virtual directories
  can also be listed. For short, the top directories are virtual if they does
  not exist.

  Own Id: OTP-8387

- An unnecessary lock operation in os:timestamp/0 has been eliminated, making it
  slightly more efficient. (Thanks to Jonas Falkevik and Tuncer Ayaz.)

  Own Id: OTP-8390

- There is a new `+t` emulator option for changing the maximum number of atoms.
  (Thanks to Julien Barbot.)

  Own Id: OTP-8405

- Fixed numerous compiler warnings generated by gcc 4.4.1 and tile-cc
  2.0.1.78377 when compiling the runtime system.

  Own Id: OTP-8412

- `configure` learned the option `--enable-m32-build` to force the building of a
  32-bit run-time on systems where the default C compiler generates 64-bit
  executables by default.

  Own Id: OTP-8415

- HiPE now works in the 64-bit emulator on Mac OS X. (Thanks to Geoff Cant.)

  Own Id: OTP-8416

- Improved handling of symbolic links to escripts

  Own Id: OTP-8417

- Add lock profiling tool.

  The Lock profiling tool, lcnt, can make use of the internal lock statistics
  when the runtime system is built with this feature enabled.

  This provides a mechanism to examine potential lock bottlenecks within the
  runtime itself.

  \- Add erts_debug:lock_counters(\{copy_save, bool()\}). This option enables or
  disables statistics saving for destroyed processes and ets-tables. Enabling
  this might consume a lot of memory.

  \- Add id-numbering for lock classes which is otherwise undefined.

  Own Id: OTP-8424

- Removed spurious options to the emulator from escript.

  Own Id: OTP-8427

- Minor documentation fixes. Mainly anchor adjustments.

  Own Id: OTP-8457

## Erts 5.7.4

### Fixed Bugs and Malfunctions

- An insufficient stack allocation was made when reading CPU information on BSD
  operating systems. (Thanks Michael Turner and Akira Kitada)

  Own Id: OTP-8207

- A bug when supplying an argument without a dash directly after the program
  name when starting erlang could prevent distribution to start. This is now
  corrected.

  Own Id: OTP-8209

- A ticker process could potentially be blocked indefinitely trying to send a
  tick to a node not responding. If this happened, the connection would not be
  brought down as it should.

  Own Id: OTP-8218

- Using certain firewalls (i.e. MS IAS Client and certain versions of COMODO)
  could expose an undocumented behaviour in the Win32 socket interface causing
  the name resolution calls to hang infinitely. This is now worked around by
  adding possibilities for port programs under Windows to use overlapped I/O on
  their standard input/output file handles.

  Own Id: OTP-8230

- Fixed bug on ETS tables with `write_concurrency`. The emulator could crash
  when doing a `select` or `match` with a bound key without finding any object.

  Own Id: OTP-8242

- The `information-request` / `information-response`, and
  `group-leader-change-request` / `group-leader-changed-response` signal pairs
  described below did not always adhere to the signal order guarantees of
  Erlang's signal model in the runtime system with SMP support. These signals
  could for example sometimes pass exit signals.

  The following BIFs behaviors can be modeled as if an asynchronous
  `information-request` signal is sent to `Pid`. If `Pid` is alive, it responds
  with an asynchronous `information-response` signal; otherwise, the runtime
  system responds with a `no-such-process` signal. When the response is
  received, the caller transforms it into the result of the BIF.

  - [`is_process_alive(Pid)`](`is_process_alive/1`)
  - `erlang:process_display(Pid, Type)`
  - [`process_info(Pid)`](`process_info/1`)
  - [`process_info(Pid, ItemSpec)`](`process_info/2`)

  When `Pid` resides on the same node as the caller of
  [`group_leader(GroupLeader, Pid)`](`group_leader/2`), the
  [`group_leader/2`](`group_leader/2`) BIFs behavior can be modeled as if an
  asynchronous `group-leader-change-request` signal is sent to `Pid`. If `Pid`
  is alive, it responds with an asynchronous `group-leader-changed-response`
  signal; otherwise, the runtime system responds with a `no-such-process`
  signal. When the response is received, the caller transforms it into the
  result of the BIF. The distributed case which only consists of an asynchronous
  `group-leader-change-request` signal and no response is not effected.

  Own Id: OTP-8245

- Errors in the `system_profile` documentation has been corrected.

  Own Id: OTP-8257

- Low watermark socket option modified high watermark instead of low watermark
  in the inet_driver. (Thanks to Feng Yu and Tuncer Ayaz)

  Own Id: OTP-8279

- A race condition could cause the runtime system with SMP support to end up in
  a completely unresponsive state.

  Own Id: OTP-8297

### Improvements and New Features

- The use of `pthread_cond_timedwait()` have been completely removed from the
  runtime system. This since its behavior is unpredictable when the system clock
  is suddenly changed. The previous use of it was harmless.

  Own Id: OTP-8193

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8201

- A new garbage collecting strategy for binaries which is more aggressive than
  the previous implementation. Binaries now has a virtual binary heap tied to
  each process. When binaries are created or received to a process it will check
  if the heap limit has been reached and if a reclaim should be done. This
  imitates the behavior of ordinary Erlang terms. The virtual heaps are grown
  and shrunk like ordinary heaps. This will lessen the memory footprint of
  binaries in a system.

  Own Id: OTP-8202

- The `ErlDrvTermData` term types used by `driver_output_term()` and
  `driver_send_term()` have been extended with the term types `ERL_DRV_INT64`,
  and `ERL_DRV_UINT64` for passing 64-bit integers. Also the 64-bit integer data
  types `ErlDrvSInt64` and `ErlDrvUInt64` have been introduced.

  For more information see the [erl_driver(3)](erl_driver.md) documentation.

  Own Id: OTP-8205

- \[escript] The restriction that the first line in escripts must begin with
  `#!` has been removed.

  \[escript] Some command line options to the escript executable has now been
  documented. For example you can run an escript in the debugger by just adding
  a command line option.

  \[escript] The documentation of the escript header syntax has been clarified.
  For example the header is optional. This means that it is possible to directly
  "execute" `.erl`, `.beam` and`.zip` files.

  Own Id: OTP-8215

- The instruction for building OTP on Windows was outdated and incomplete, the
  document is updated.

  Also the otp_build script required windows drives to show up in Cygwin using
  the /cygdrive prefix. That requirement is now removed.

  Own Id: OTP-8219

- A module can have native implemented functions (NIFs) that are dynamically
  loaded by calling `erlang:load_nif/2`. This is an experimental feature that is
  not yet intended for production systems. It is released with intention to get
  some early feedback on the interfaces before they are carved in stone.

  Own Id: OTP-8220

- The [`float/1`](`float/1`) BIF would always force a garbage collection. The
  BIFs [`size/1`](`size/1`), [`byte_size/1`](`byte_size/1`),
  [`bit_size/1`](`bit_size/1`), [`abs/1`](`abs/1`), and [`round/1`](`round/1`)
  would force a garbage-collection if the result was not a sufficiently small
  integer.

  Own Id: OTP-8221

- The `erlang:port_command/3` BIF has been added. `erlang:port_command/3` is
  currently not auto imported, but it is planned to be auto imported in OTP R14.
  For more information see the `m:erlang` documentation.

  Own Id: OTP-8225

- '`configure --enable-darwin-64bit`' would fail if Snow Leopard had been booted
  with the 64-bit kernel. (Thanks to Ryan M. Graham.)

  Own Id: OTP-8236

## Erts 5.7.3

### Fixed Bugs and Malfunctions

- On Windows, open_port(\{spawn,Command\},Opts) could not run executables with
  spaces in the path or filename, regardless of quoting efforts. While
  open_port(\{spawn_executable,Exec\},Opts) can run any executable, it was still
  impossible to use 'spawn' to do the same thing. This is now corrected.

  Own Id: OTP-8055

- The scheduler bind type `processor_spread` spread schedulers too much on large
  NUMA systems.

  The new scheduler bind type `spread` spreads schedulers as much as possible,
  and behaves as `processor_spread` previously did. For more information see the
  documentation of the `+sbt` command line argument in the [erl](erl_cmd.md)
  documentation, and the documentation of
  `erlang:system_flag(scheduler_bind_type, SchedulerBindType)`.

  Own Id: OTP-8063

- Automatically detected CPU topology on Linux system could erroneously contain
  logical processors with `-1` as identifiers. This happened when
  `sysconf(_SC_NPROCESSORS_CONF)` returned a value larger than the amount of
  logical processors found.

  Own Id: OTP-8064

- When the minimal term [] (end of list) was sent as the complete message to a
  process on another node, and received there, it could not be decoded. This bug
  is now corrected. Fortunately [] is uncommon as the complete message in real
  applications but it is a serious bug anyway.

  Own Id: OTP-8092

- A bug when the floating point exception pointer was not initialized has been
  corrected. It manifested itself on CentOS 5.1 sometimes when a floating point
  value was sent to a remote node. Bug reported and patch suggested by David
  Reiss, confirmed by Mikael Pettersson.

  Some build problems on IRIX was also corrected. Problem reported by Patrick
  Baggett, patch by Mikael Pettersson.

  Own Id: OTP-8095

- A terminating process could erroneously unregister a name for another process.
  This could occur under the following conditions: The name of the terminating
  process was unregistered and then registered for another process
  simultaneously as the process that first had the name was terminating.

  Own Id: OTP-8099 Aux Id: seq11344

- Running erlc in a very deep directory (with a path length of more 256 or more
  characters) would cause the emulator to crash in a call to
  [`list_to_atom/1`](`list_to_atom/1`). (Thanks to Chris Newcombe.)

  Own Id: OTP-8124

- A deadlock of the runtime system could occur when unregistering the name of a
  port.

  Own Id: OTP-8145

- `Makefile.in` has been updated to use the LDFLAGS environment variable (if
  set). (Thanks to Davide Pesavento.)

  Own Id: OTP-8157

- The pthread rwlock implementation on Linux could cause starvation of writers.
  We, therefore, now use our own rwlock implementation on Linux.

  Own Id: OTP-8158

- Open source Erlang builds are updated to work well on Snow Leopard (MacOS X
  10.6)

  Own Id: OTP-8168

- A call to `erlang:system_info(schedulers_online)` could end up in an infinite
  loop. This happened if the amount of schedulers was larger than one, the
  amount of schedulers online was one, and someone was blocking
  multi-scheduling.

  Own Id: OTP-8169

- An error in erlang:system_profile/2 could cause timestamped messages to arrive
  out of order in the SMP case. This has now been fixed.

  Own Id: OTP-8171

- [`binary_to_atom/2`](`binary_to_atom/2`) and
  [`binary_to_existing_atom/2`](`binary_to_existing_atom/2`) would leak memory
  if the binary contained unaligned data.

  Own Id: OTP-8192

- The async thread pool in the runtime system without SMP support used a memory
  allocator that was not thread safe for async jobs.

  Own Id: OTP-8194

### Improvements and New Features

- Processor internal NUMA nodes are now supported in the ERTS internal CPU
  topology representation. For more information see the documentation of the
  `+sct` command line argument in the [erl](erl_cmd.md) documentation, and the
  documentation of `erlang:system_info(cpu_topology)`.

  Own Id: OTP-8041

- Documentation for ets improved about concurrency.

  Own Id: OTP-8050

- Emulator flags in an escript were earlier inherited to emulators started from
  the emulator running the escript. For example when an escript invoked
  `os:cmd("erl")`, the new emulator were given erroneous emulator flags. This
  bug has now been fixed

  Escript filenames may now contain dots.

  Own Id: OTP-8060

- Made some BIFs non-recursive (relational operators,hash and phash) to limit
  internal stack usage.

  Own Id: OTP-8065

- Fixed Windows specific bug in erl_prim_loader. Now it handles the root
  directory (e.g. c:/) better. This bug affected the directory listing in the
  debugger.

  Own Id: OTP-8080

- A TCP socket with option `{packet,4}` could crash the emulator if it received
  a packet header with a very large size value (>2Gb). The same bug caused
  `erlang:decode_packet/3` to return faulty values. (Thanks to Georgos Seganos.)

  Own Id: OTP-8102

- The maximum size of the export table has been raised from 65536 to 524288
  entries.

  Own Id: OTP-8104 Aux Id: seq11345

- The file module has now a read_line/1 function similar to the io:get_line/2,
  but with byte oriented semantics. The function file:read_line/1 works for raw
  files as well, but for good performance it is recommended to use it together
  with the 'read_ahead' option for raw file access.

  Own Id: OTP-8108

- Fixed bug causing emulator crash when reading a term in external format
  containing a corrupt list with a negative length.

  Own Id: OTP-8117

- New emulator flag `+sss`, to set stack size of scheduler threads.

  Own Id: OTP-8119

- The Windows utility Erlsrv, run in interactive mode now accepts options for
  registering internal service name and description field of Windows registry
  database.

  Own Id: OTP-8132

- `erlang:demonitor(Mon, [flush])` has been optimized. Previously it always
  searched the message queue of the caller for a `'DOWN'` message. Current
  implementation only search the message queue when necessary. It is quite
  common that the search is not necessary.

  A new option `info` has been added to `erlang:demonitor/2`. For more
  information see the `m:erlang` documentation.

  Own Id: OTP-8143

- I/O tasks could unnecessarily be rescheduled. This was harmless, but not
  useful work.

  Own Id: OTP-8148

- Minor improvements of `erlang:memory/[1,2]`.

  Own Id: OTP-8152

- New configuration option to enable use of shared zlib.

  Own Id: OTP-8155

- Fixed smp bug in ETS that could cause emulator crash when table with more than
  1000 objects accessed by several processes, including calls to variants of
  `select` or `match` combined with concurrent object deletion.

  Own Id: OTP-8166 Aux Id: seq11392

- The code path interpretation is now more relaxed. The flag -code_path_choice
  now defaults to relaxed instead of strict. See the documentation of code and
  init for more info.

  Own Id: OTP-8170

- Load balancing of run queues and check for I/O are triggered more often than
  before in situations where processes are scheduled often but are doing very
  little work each time they execute.

  Own Id: OTP-8172

- Call tracing binary comprehensions would cause an emulator crash. (Thanks to
  Paul Mineiro.)

  Own Id: OTP-8179

- [`binary_to_term/1`](`binary_to_term/1`) would crash the emulator instead of
  generating a `badarg` exception when given certain invalid terms. (Thanks to
  Scott Lystig Fritchie.)

  Own Id: OTP-8180

## Erts 5.7.2

### Fixed Bugs and Malfunctions

- Crash dumps should now cause less problems for the crashdump_viewer
  application. (For processes where arity was non-zero, the arguments are now
  longer printed - they used to be printed in a format that was not parseable.)

  Own Id: OTP-7472 Aux Id: seq11019, 11292

- Processes could potentially get stuck on an offline scheduler.

  Own Id: OTP-7990

- [`binary_to_atom/2`](`binary_to_atom/2`) and
  [`binary_to_existing_atom/2`](`binary_to_existing_atom/2`) could leak memory
  if they caused a `badarg` exception.

  Own Id: OTP-7997

- A process could under very rare circumstances erroneously be resumed.

  Own Id: OTP-8000

- Load balancing between schedulers could under rare circumstances cause an
  emulator crash.

  Own Id: OTP-8008

- `erlang:memory(processes_used)` always returned `0` instead of the correct
  value. (Thanks to Geoff Cant)

  Own Id: OTP-8022

### Improvements and New Features

- Major improvements of the Erlang distribution for Erlang runtime systems with
  SMP support. Previously distribution port locks were heavily contended, and
  all encoding and decoding for a specific distribution channel had to be done
  in sequence. Lock contention due to the distribution is now negligible and
  both encoding and decoding of Erlang messages can be done in parallel.

  The old atom cache protocol used by the Erlang distribution has been dropped
  since it effectively prevented all parallel encoding and decoding of messages
  passed over the same distribution channel.

  A new atom cache protocol has been introduced which isolates atom cache
  accesses and makes parallel encoding and decoding of messages passed over the
  same distribution channel possible. The new atom cache protocol also use an
  atom cache size 8 times larger than before. The new atom cache protocol is
  documented in the ERTS users guide.

  Erlang messages received via the distribution are now decoded by the receiving
  Erlang processes without holding any distribution channel specific locks.
  Erlang messages and signals sent over the distribution are as before encoded
  by the sending Erlang process, but now without holding any distribution
  channel specific locks during the encoding. That is, both encoding and
  decoding can be and are done in parallel regardless of distribution channel
  used.

  The part that cannot be parallelized is the atom cache updates. Atom cache
  updates are therefore now scheduled on the distribution port. Since it is only
  one entity per distribution channel doing this work there is no lock
  contention due to the atom cache updates.

  The new runtime system does not understand the old atom cache protocol. New
  and old runtime systems can however still communicate, but no atom cache will
  be used.

  Own Id: OTP-7774

- Fixed a bug that caused error logging from `driver_select` sometimes with
  additional symptoms such as failing IP communications or even an emulator
  crash.

  Own Id: OTP-7898 Aux Id: seq11304

- Improved SMP concurrency for ETS tables. Several mutating operations can now
  be performed truly concurrent on different records of the same table. To
  support this, the table has to be created with option `write_concurrency`, as
  it is achieved at the expense of some execution and memory overhead.
  `ets:select` and `select_count` has also been improved for all tables to not
  acquire exclusive table lock during the iteration.

  Own Id: OTP-7922

- erl (that is erl.exe and dyn_erl) and erlexec has been made more dynamic so no
  hard coded paths needs to added at installation time to erl (that is erl.ini
  and erl). Reltool will make use of this in a future release.

  Own Id: OTP-7952

- Added functionality to get higher resolution timestamp from system. The
  erlang:now function returns a timestamp that's not always consistent with the
  actual operating system time (due to resilience against large time changes in
  the operating system). The function os:timestamp/0 is added to get a similar
  timestamp as the one being returned by erlang:now, but untouched by Erlangs
  time correcting and smoothing algorithms. The timestamp returned by
  os:timestamp is always consistent with the operating systems view of time,
  like the calendar functions for getting wall clock time, but with higher
  resolution. Example of usage can be found in the os manual page.

  Own Id: OTP-7971

- Two new options are added to open_port - spawn_executable which runs external
  executables in a controlled way, and spawn_driver which only opens port to
  loaded Erlang drivers. See the erlang manual page for details.

  Own Id: OTP-7995

- New functionality in ETS to transfer the ownership of a table. A table can
  either change owner be declaring an "heir", another process that will inherit
  the table if the owner terminates. A table can also change owner by calling a
  new function `ets:give_away`.

  Own Id: OTP-8006

- Updates to Tilera build environment.

  Own Id: OTP-8009

- A stack trace was unnecessarily saved during process termination.

  Own Id: OTP-8014

- User defined CPU topology and scheduler bind type can now be set from the
  command line when starting an emulator. For more information see the
  documentation of the `+sct`, and the `+sbt` emulator flags in the
  [erl](erl_cmd.md) documentation.

  The CPU topologies returned from `erlang:system_info/1` and
  `erlang:system_flag/2` now always contain the `processor` level, also when not
  strictly necessary.

  Own Id: OTP-8030

- Various fixes in ETS: `ets:first` could return a deleted key in a fixated
  table. `ets:lookup` could return objects out of order if a deleted object was
  re-inserted into a fixed bag. `ets:delete_object` could fail to delete
  duplicate objects in a duplicate_bag.

  Own Id: OTP-8040

## Erts 5.7.1

### Fixed Bugs and Malfunctions

- Fixed a bug on Windows that could make `gen_tcp:send` hang trying to send an
  iolist of more than 16 binaries.

  Own Id: OTP-7816

- The runtime system could under rare circumstances crash during load balancing.

  Own Id: OTP-7908 Aux Id: otp-7500

- `run_erl` uses fallback if Unix98 pseudo-terminal is not present on host.

  Own Id: OTP-7916 Aux Id: seq11249

- A message buffer memory leak in the runtime system without smp support has
  been fixed.

  Own Id: OTP-7941

- Attempting to append a binary of 16Mb or greater to another binary using the
  bit syntax would cause a `system_limit` exception. There was also several
  cases when constructing binaries when a `badarg` exception was generated when
  it should have been `system_limit`.

  Own Id: OTP-7942

- The runtime system with SMP support failed to terminate the caller of
  [`link(RemotePid)`](`link/1`) properly, if `RemotePid` was the pid of a
  process on an unreachable node. The calling process was in this case marked as
  exiting, but never terminated.

  Own Id: OTP-7946

### Improvements and New Features

- Rudimentary support for cross compiling is added to the source release. The
  support is still in its infancy and has only been used to cross compile on
  Linux for a different cpu architecture and a different Linux version, but
  should be extendible to support other platforms as well. The cross
  configuration files with examples are placed in $ERL_TOP/xcomp/. View
  README.xcomp and run $ERL_TOP/otp_build -help for further information.

  Own Id: OTP-7854

- The escape sequence `\{` which was given a new interpretation in R13A has
  retained its old meaning (the ASCII code for `{`), which means that codes
  greater than 255 have to be stated using hexadecimal characters (for example,
  `\x{AAA}`). The escape sequence `\xH` where H is a hexadecimal character
  followed by something else but a hexadecimal character is no longer valid
  (incompatibility with R13A). Character codes less than 256 can be stated using
  two hexadecimal characters (for example, `\x0D`).

  Own Id: OTP-7891 Aux Id: OTP-7855

- The [`term_to_binary/1`](`term_to_binary/1`) BIF used to be implemented with
  recursive C code, which could cause the Erlang emulator to terminate because
  of a stack overflow.

  Also fixed some minor issues in [`term_to_binary/1`](`term_to_binary/1`) and
  [`binary_to_term/1`](`binary_to_term/1`) pointed out by Matthew Dempsky.

  Own Id: OTP-7894

- Several glitches and performance issues in the Unicode and I/O-system
  implementation of R13A have been corrected.

  Own Id: OTP-7896 Aux Id: OTP-7648 OTP-7887

- Minor documentation improvements of the `scheduler_bind_type` argument of
  `erlang:system_flag/2`, and the `scheduler_bind_type`, and the
  `scheduler_bindings` arguments of `erlang:system_info/1`.

  Own Id: OTP-7901 Aux Id: OTP-7777

- There is a new BIF `erlang:make_tuple/3`.

  Own Id: OTP-7913

## Erts 5.7

### Fixed Bugs and Malfunctions

- _OpenSource:_

  FreeBSD leap-seconds are handled according to patch submitted by OpenSource
  user Kenji Rikitake. No test case covers this functionality (unsupported
  platform).

  Own Id: OTP-7609

- A corrected bug in ETS for `bag` and `duplicate_bag`. A `delete/2` or
  `lookup_element/3` could miss objects in a fixed table if one or more objects
  with the same key had already been deleted.

  Own Id: OTP-7665

- A new driver call-back `stop_select` is introduced to allow drivers to
  de-select and then close a file descriptor in a safe way in a SMP emulator.
  The old way was not strictly according to posix standard and could in some
  rare cases lead to unexpected behavior. A new flag `ERL_DRV_USE` can be passed
  to `driver_select()` to tell it that the descriptor should be closed.
  `stop_select` is then called when it is safe to do so. Old drivers will
  however still work as before.

  Own Id: OTP-7670

- A bug fixed for TCP sockets with option `{packet,http}`. An HTTP request with
  an absolute URI was returned with a corrupt path string. This bug did only
  exist in R12B-4 and R12B-5.

  Own Id: OTP-7682 Aux Id: OTP-7647

- run_erl did in some cases fail to extract control sequences from to_erl (like:
  winsize=X,Y) and did instead send them to be interpreted by the erlang shell.

  Own Id: OTP-7688

- A bug in the installer on Windows not updating file associations properly is
  now corrected.

  Own Id: OTP-7746

- More space than necessary could be allocated in binaries when appending to a
  binary (also in a binary comprehension) and the data appended did not consist
  of wholes bytes (e.g. 13 bits).

  Own Id: OTP-7747

- The gen_sctp option sctp_peer_addr_params,
  #sctp_paddrparams\{address=\{IP,Port\} was erroneously decoded in the inet
  driver. This bug has now been corrected.

  Own Id: OTP-7755

- Outstanding async driver jobs leaked memory if the issuing port died before
  the async jobs completed.

  Own Id: OTP-7784

- A bug in the dynamic library loading affecting, among others, OpenSolaris is
  now corrected. (Thanks to Paul Fisher.)

  Own Id: OTP-7796

- run_erl compile errors fixed for FreeBSD

  Own Id: OTP-7817

- A bug in the inet driver for SCTP on Solaris showing for e.g gen_sctp:abort/1
  and gen_sctp:eof/1 has been corrected. Patch suggestion by Simon Cornish.

  Own Id: OTP-7866

### Improvements and New Features

- The order of objects visited in select for ordered_set is now documented.

  Own Id: OTP-7339

- The runtime system with SMP support now uses multiple, scheduler specific run
  queues, instead of one globally shared run queue.

  The lock protecting the shared run queue was heavily contended, and the shared
  run queue also caused Erlang processes to randomly migrate between schedulers
  with negative cache effects as a result.

  With the current scheduler specific run queue solution, lock contention due to
  run queue protection has been reduced, and Erlang processes are only migrated
  when needed to balance the load between the schedulers. The reduced amount of
  migration also reduce lock contention on locks protecting the scheduler
  specific instances of the erts internal memory allocators.

  The scheduler specific run queues are also a necessity for a lot of future
  planned NUMA (Non-Uniform Memory Access) specific optimizations.

  Own Id: OTP-7500

- Support for Unicode is implemented as described in EEP10. Formatting and
  reading of unicode data both from terminals and files is supported by the io
  and io_lib modules. Files can be opened in modes with automatic translation to
  and from different unicode formats. The module 'unicode' contains functions
  for conversion between external and internal unicode formats and the re module
  has support for unicode data. There is also language syntax for specifying
  string and character data beyond the ISO-latin-1 range.

  The interactive shell will support input and output of unicode characters when
  the terminal and operating system supports it.

  Please see the EEP and the io/io_lib manual pages as well as the stdlib users
  guide for details.

  _I/O-protocol incompatibilities:_

  The io_protocol between io_Server and client is updated to handle protocol
  data in unicode formats. The updated protocol is now documented. The
  specification resides in the stdlib _users manual_, which is a new part of the
  manual.

  _io module incompatibilities:_

  The io:put_chars, io:get_chars and io:get_line all handle and return unicode
  data. In the case where binaries can be provided (as to io:put_chars), they
  shall be encoded in UTF-8. When binaries are returned (as by
  io:get_line/get_chars when the io_server is set in _binary mode_) the returned
  data is also _always_ encoded as UTF-8. The file module however still returns
  byte-oriented data, why file:read can be used instead of io:get_chars to read
  binary data in ISO-latin-1.

  _io_lib module incompatibilities:_

  io_lib:format can, given new format directives (i.e "~ts" and "~tc"), return
  lists containing integers larger than 255.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7648 Aux Id: OTP-7580 OTP-7514 OTP-7494 OTP-7443 OTP-7181 EEP10
  EEP11

- The format of the string returned by `erlang:system_info(system_version)` (as
  well as the first message when Erlang is started) has changed. The string now
  contains the both the OTP version number as well as the erts version number.

  Own Id: OTP-7649

- Message passing has been further optimized for parallel execution. Serial
  message passing is slightly more expensive than before, but parallel send to a
  common receiver is much cheaper.

  Own Id: OTP-7659

- Lock contention on the atom table lock when decoding Erlang terms on the
  external format has been drastically reduced.

  Own Id: OTP-7660

- The undocumented, unsupported, and deprecated guard BIF `is_constant/1` has
  been removed.

  \*** INCOMPATIBILITY with R12B \***

  Own Id: OTP-7673

- The Erlang process lock implementation has been improved by Mat Hostetter at
  Tilera Corporation.

  Own Id: OTP-7692

- A `{nodedown, Node}` message passed by the `net_kernel:monitor_nodes/X`
  functionality is now guaranteed to be sent after `Node` has been removed from
  the result returned by `erlang:nodes/Y`.

  Own Id: OTP-7725

- The short-circuit operators `andalso` and `orelse` no longer guarantees that
  their second argument is either `true` or `false`. As a consequence,
  `andalso`/`orelse` are now tail-recursive.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7748

- A new BIF, `lists:keyfind/3`, has been added. It works like
  `lists:keysearch/3` except that it does not wrap the returned tuple in a
  `value` tuple in case of success. (Thanks to James Hague for suggesting this
  function.)

  Own Id: OTP-7752

- Optimization for drivers by creating small binaries direct on process heap.

  Own Id: OTP-7762

- `I bsl N` could cause the Erlang virtual machine to run of memory instead
  generating a `system_limit` if N was absurdly huge. (Thanks to Daniel
  Hedlund.)

  There would always be a garbage collection when evaluating `I bsl N` or
  `I bsr N` if `I` was a bignum.

  If `I` is an integer and `N` a bignum, `I bsl N` will now cause the correct
  `system_limit` exception instead of `bad_arith` as in earlier releases.

  If `I` is an integer and `N` a bignum, `I bsr N` will return either 0 or -1
  depending on the sign of `I` instead of causing a `bad_arith` exception as in
  earlier releases.

  Own Id: OTP-7764

- Scheduler threads can now be bound to logical processors on newer Linux and
  Solaris systems. More systems will be supported in the future.

  In some cases performance has increased drastically when binding schedulers.
  Schedulers are not bound by default, though. This since it might cause a
  performance degradation if multiple programs have bound to processors, e.g.
  multiple Erlang runtime systems. For more information see the documentation of
  `erlang:system_flag/2`.

  In order to bind scheduler threads the CPU topology need to be known. On some
  newer Linux and Solaris systems the runtime system automatically detects the
  CPU topology. If the emulator isn't able to automatically detect the CPU
  topology, the CPU topology can be defined. For more information see the
  documentation of `erlang:system_flag/2`.

  Own Id: OTP-7777

- The compiler will refuse to a compile file where the module name in the file
  differs from the output file name.

  When compiling using `erlc`, the current working directory will no be included
  in the code path (unless explicitly added using "-pa .").

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7793

- The BIFs [`atom_to_binary/2`](`atom_to_binary/2`),
  [`binary_to_atom/2`](`binary_to_atom/2`), and
  [`binary_to_existing_atom/2`](`binary_to_existing_atom/2`) have been added.

  Own Id: OTP-7804

- The amount of schedulers online can now be changed during operation. The
  amount of schedulers online defaults to the same amount as available logical
  processors. For more information see the documentation of
  `erlang:system_flag/2` and `erl`.

  Own Id: OTP-7811

- The deprecated functions `erlang:fault/1`, `erlang:fault/2`, and
  `file:rawopen/2` have been removed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7812

- Erts internal dynamically allocated process and port specific data for rarely
  used data. This is used to reduce memory usage of processes and ports that do
  not use specific functionality. More functionality will be moved to process
  and port specific data in future releases.

  Own Id: OTP-7818

- New packet type `http_bin` for gen_tcp sockets and `erlang:decode_packet`. It
  works like `http` except that strings are returned as binaries instead of
  lists.

  Own Id: OTP-7821

- The obsolete wd_keeper program for embedded Solaris systems has been removed.

  Own Id: OTP-7822

- Nodes belonging to different independent clusters can now co-exist on the same
  host with the help of a new environment variable setting ERL_EPMD_PORT.

  Own Id: OTP-7826

- There are new functions `erlang:min/2` and `erlang:max/2` to calculate the
  minimum and maximum of two terms, respectively. Note that the functions are
  not auto-imported, so they need to be imported explicitly or the `erlang`
  prefix must be used when calling them.

  Own Id: OTP-7841

- The copyright notices have been updated.

  Own Id: OTP-7851

- Enhanced build environment for cross compilation to Tilera Tile architecture.

  Support for native ethread atomics on Tilera Tile64/TilePro (Thanks to Tilera
  Corporation).

  Own Id: OTP-7852

- The escape sequences `\x` and `\{` have been assigned new interpretations
  (they used to return the ASCII code for `x` and `{` respectively). One or more
  octal characters inside curly brackets after a leading backslash is from now
  on an alternative to the existing syntax `\NNN`, but can also be used for
  codes greater than 255. In a similar fashion, one or more hexadecimal
  characters can be put inside curly brackets after a leading `\x`. Furthermore,
  the escape sequences `\xH` and `\xHH`, where N is a hexadecimal character, can
  be used for codes less than 256.

  NOTE: These new escape sequences are still considered experimental and may be
  changed in the R13B release.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7855

- The PCRE library's exported function names are now prefixed with erts\_ in the
  erlang emulator to avoid clashes with dynamically loaded drivers.

  Own Id: OTP-7861

- A runtime system with SMP support will now be built by default on most
  platforms if a usable posix thread library or native windows threads are
  found.

  For more information see the top README file.

  Own Id: OTP-7872

## Erts 5.6.5.1

### Fixed Bugs and Malfunctions

- A corrected bug in ETS for `bag` and `duplicate_bag`. A `delete/2` or
  `lookup_element/3` could miss objects in a fixed table if one or more objects
  with the same key had already been deleted.

  Own Id: OTP-7665

- A bug fixed for TCP sockets with option `{packet,http}`. An HTTP request with
  an absolute URI was returned with a corrupt path string. This bug did only
  exist in R12B-4 and R12B-5.

  Own Id: OTP-7682 Aux Id: OTP-7647

- Calling `gen_tcp:send()` from several processes on socket with option
  `send_timeout` could lead to much longer timeout than specified. The solution
  is a new socket option `{send_timeout_close,true}` that will do automatic
  close on timeout. Subsequent calls to send will then immediately fail due to
  the closed connection.

  Own Id: OTP-7731 Aux Id: seq11161

- A process being garbage collected via the
  [`garbage_collect/1`](`garbage_collect/1`) BIF or the
  [`check_process_code/2`](`check_process_code/2`) BIF didn't handle message
  receive and resume correctly during the garbage collect. When this occurred,
  the process returned to the state it had before the garbage collect instead of
  entering the new state.

  Own Id: OTP-7738

## Erts 5.6.5

### Fixed Bugs and Malfunctions

- A bug in inet_drv concerning gen_tcp:connect has been corrected. A connect
  towards a non-open port through open firewalls could sometimes erroneously be
  successful. Any subsequent operation would fail, though.

  Own Id: OTP-6542

- Floating point arithmetic in drivers could cause a runtime system crash and/or
  unexpected results on runtime systems with floating point exceptions enabled.
  Floating point exceptions are disabled unless explicitly enabled or if hipe is
  enabled.

  Own Id: OTP-7237

- A bug when many sockets got signalled simultaneously causing the emulator to
  panic with the message "Inconsistent, why isn't io reported?" is now
  corrected.

  Own Id: OTP-7420

- Starting erl with option "-detached" now disconnects correctly from terminal
  session on Unix.

  Own Id: OTP-7461

- Mended gdb etp-commands for ETS access.

  Own Id: OTP-7538

- `erlang:decode_packet/3` allows white space between HTTP header tag and colon
  according to RFC2616.

  Own Id: OTP-7543

- An emulator compiled for SCTP now starts even if the dynamic libraries are not
  present. The SCTP driver is then of course not loaded.

  Own Id: OTP-7551

- To build on Mac OS X, 10.3.0 or later is now required because of fixes for two
  problems:

  There would be a resource leak when `erl_ddl` attempted to unload a driver.
  This problem has been corrected by using `dlopen()` (which works on all modern
  Unix-like platforms) instead of the Mac OS X specific API calls.

  Signal handling in the run-time system for HiPE has been updated to work on
  later versions of Mac OS X than 10.2.x. Therefore, `--enable-hipe` now works
  on Mac OS X with Intel CPUs.

  Thanks to Geoff Cant for the patches.

  Own Id: OTP-7562

- Corrected some information about the protocol between EPMD and Erlang nodes.
  (Thanks to Michael Regen.)

  Own Id: OTP-7594

- When using `erlang:system_monitor(Pid,{long_gc,Time})`, and the GC time
  exceeded 1 second, it sometimes erroneously showed up as about 4300 seconds.
  (This bug was corrected in R9C, but re-introduced in R12B.) (Thanks to Chris
  Newcombe.)

  Own Id: OTP-7622 Aux Id: OTP-4903, seq8379

### Improvements and New Features

- The driver entry of a dynamically loaded driver is now copied when loaded
  which enables some internal optimizations. Note that drivers that modify the
  driver entry during execution will not work anymore. Such a miss-use of the
  driver interface is however not supported.

  Own Id: OTP-6900

- The split function is now added to the re library. Exceptions and errors from
  both run, replace and split are made more consistent.

  Own Id: OTP-7514 Aux Id: OTP-7494

- Fixed harmless compiler warnings when building the emulator and minor build
  changes in order to avoid unnecessary rebuilds.

  Own Id: OTP-7530

- There is now experimental support for loading of code from archive files. See
  the documentation of `code`, `init`, `erl_prim_loader `and `escript` for more
  info.

  The error handling of `escripts` has been improved.

  An `escript` may now set explicit arguments to the emulator, such as
  `-smp enabled`.

  An `escript` may now contain a precompiled beam file.

  An `escript` may now contain an archive file containing one or more
  applications (experimental).

  The internal module `code_aux` has been removed.

  Own Id: OTP-7548 Aux Id: otp-6622

- The reallocation functionality part of the ERTS internal memory allocators,
  now consider current block in combination with surrounding free blocks as an
  alternative location for a reallocation.

  Own Id: OTP-7555

- There could remain false references from a process to a module that has been
  called earlier, so that the process would be killed if the module was
  reloaded. (Thanks to Richard Carlsson.)

  Also, the fix for this bug also made it possible to make stack backtraces (as
  returned from `erlang:get_stacktrace/0` and other functions) more correct in
  that the immediate caller is always included in the stack backtrace (it could
  sometimes be missing).

  Own Id: OTP-7559

- Improved locking in IO-handling for better smp performance.

  Own Id: OTP-7560

- Improved BIF rescheduling functionality.

  Own Id: OTP-7587

- Loading a module compiled with Erlang/OTP R9C and calling `module_info/0` in
  the module would crash the emulator. The emulator now refuses to load any
  module compiled with R9C or earlier. (Note: only trivial modules compiled with
  R10B or earlier could be loaded anyway.) (Thanks to Martin Kjellin.)

  Own Id: OTP-7590

## Erts 5.6.4.2

### Fixed Bugs and Malfunctions

- A process calling one of the following BIFs could under very rare conditions
  deadlock in the runtime system with SMP support:
  [`check_process_code/2`](`check_process_code/2`),
  [`garbage_collect/1`](`garbage_collect/1`), `process_info/[1,2]`,
  `system_flag/2`, and `erlang:suspend_process/[1,2]`.

  Own Id: OTP-7582

- A couple of statistics variables were not managed in a thread safe manner in
  the runtime system with SMP support.

  Own Id: OTP-7583

- An extremely rare race condition when terminating a process could potentially
  cause a runtime system crash.

  Own Id: OTP-7584

- Under certain conditions and when using run_erl/to_erl, the terminal Erlang
  driver (ttsl_drv) could crash the emulator by doing a division by zero due to
  incorrect handling of terminals reporting a zero width. For terminals
  reporting zero width, the driver now fallbacks to a default width of 80 and a
  default height of 24 (vt100), as a fallback behaviour. This fixes the crashes
  and also makes output on "dumb" terminals much more readable.

  Own Id: OTP-7592 Aux Id: seq11073

## Erts 5.6.4.1

### Improvements and New Features

- A new `erts_alloc` parameter `+M<S>rmbcmt` (relative multiblock carrier move
  threshold) has been added. It determines when to force a moving realloc in a
  multiblock carrier when a block is shrunk. For more information see the
  `erts_alloc(3)` documentation.

  Own Id: OTP-7540

- The new option `+d` can be given to `erl` to suppress the crash dump
  generation if an internal error is detected. As a result, a more useful core
  dump is produced.

  Own Id: OTP-7578 Aux Id: seq11052

## Erts 5.6.4

### Fixed Bugs and Malfunctions

- Double backslashes in format string passed to the erts internal printf
  implementation produced erroneous results. No such format strings were passed
  to the erts internal printf implementation, i.e., the bug was therefore
  completely harmless. (Thanks to Perry Smith.)

  Own Id: OTP-7408

- Large files are now handled on Windows, where the filesystem supports it.

  Own Id: OTP-7410

- Bug fixed for `{packet,http}` when space follows http headers.

  Own Id: OTP-7458

- The trace option `running` could cause an emulator crash if the current
  function couldn't be determined.

  Own Id: OTP-7484

- Using 16#ffffFFFF as a timeout value in receive...after would often cause a
  timeout almost at once due to an 32-bit integer overflow. (Thanks to Serge
  Aleynikov and Matthias Lang.)

  Own Id: OTP-7493

- For the process that an escript runs in, the `trap_exit` process flag is now
  `false` instead of `true` (as in previous releases). Scripts that depend on
  the previous (counter-intuitive) behaviour might not work. (Thanks to Bengt
  Kleberg.)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7517

- A bug in the `string:to_integer/1` builtin made the emulator unstable. This is
  now corrected. (Thanks to Lev Walkin.)

  Own Id: OTP-7526

### Improvements and New Features

- Performance for ETS intensive applications should now be better in the SMP
  emulator. Also, ETS table identifiers (as returned by `ets:new/2`) are now
  spread out in wider range than before (using 28 bits in a 32-bit emulator) to
  make sure that the table identifier for a deleted table will not be quickly
  re-used.

  NOTE: Table identifiers can now be negative integers. Programs that
  (incorrectly) assume that table identifiers can only be positive integers
  might stop to work. (The type of a table identifier is intentionally not
  documented, and may change in a future release.)

  Own Id: OTP-7348

- New BIF `erlang:decode_packet/3` that extracts a protocol packet from a
  binary. Similar to the socket option `{packet, Type}`. Also documented the
  socket packet type `http` and made it official. _NOTE_: The tuple format for
  `http` packets sent from an active socket has been changed in an incompatible
  way.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7404

- The source code for the documentation for some applications (erts, kernel,
  stdlib, and several others) are now included in the source tar ball. There is
  currently no Makefile support for building HTML files from the source (such
  support will be included in a future release).

  Own Id: OTP-7406

- A lot of frequently accessed memory counters (erts internal) have been
  removed. This since they hurt performance on the runtime system with SMP
  support. As a result `erlang:memory/[0,1]` will only deliver a result if all
  `erts_alloc(3)` allocators are enabled (default). The result delivered when
  all `erts_alloc(3)` allocators are enabled are both more accurate and less
  accurate than before. More memory than before are included in the result, but
  the different parts that are summed are not gathered atomically. A call to
  `erlang:memory/[0,1]` is much cheaper for the system than before. This since
  the information isn't gathered atomically anymore which was very expensive.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7468

- Pre-allocators used for, for example, timers, and messages have been rewritten
  to be scheduler specific. That is, different schedulers will now allocate from
  different pools which reduces lock contention.

  Own Id: OTP-7470

- On Mac OS X, file:sync/1 now guarantees that all filesystem buffers are
  written to the disk by using the fcntl() with F_FULLFSYNC option. Previously,
  file:sync/1 called fsync(), which only guaranteed that the data had been
  transferred to the disk drive. (Thanks to Jan Lehnardt.)

  Own Id: OTP-7471

- Termination of a process that takes a long time can now be preempted, i.e.,
  the terminating process will be rescheduled for later continuation of
  termination so that other processes can execute. Termination of a process that
  owns many and/or large ets tables typically takes a long time.

  Own Id: OTP-7477

- A new trace option `exiting` has been added. The `exiting` trace is similar to
  the `running` trace, but for exiting processes. For more information see the
  erlang(3) documentation.

  The `erlang:trace/3` bif now doesn't block other scheduler threads if only one
  tracee is specified in the call to `erlang:trace/3`.

  Own Id: OTP-7481

- The re module is extended with repetitive matches (global option) and
  replacement function.

  Own Id: OTP-7494 Aux Id: OTP-7181

- In the section about binary construction, the reference manual now mentions
  what happens when an integer value does not fit into an integer segment of
  size N (namely, that the N least significant bits will be put into the binary
  and that the most significant bits will be silently discarded). (Thanks to
  Edwin Fine.)

  Own Id: OTP-7506

- Setting the `{active,once}` for a socket (using inets:setopts/2) is now
  specially optimized (because the `{active,once}` option is typically used much
  more frequently than other options).

  Own Id: OTP-7520

### Known Bugs and Problems

- Floating point arithmetic in drivers can cause a runtime system crash and/or
  unexpected results on runtime systems with floating point exceptions enabled.
  Floating point exceptions are disabled unless explicitly enabled or if hipe is
  enabled.

  Own Id: OTP-7237

## Erts 5.6.3.3

### Fixed Bugs and Malfunctions

- Binary construction with an integer field of size 0 at the end of the
  constructed binary (and the size given in a variable), would cause a write of
  one byte outside the memory reserved for the binary, which in turn could cause
  an emulator crash.

  Own Id: OTP-7422

- A race condition in the dynamic driver implementation could cause an emulator
  crash. (Thanks to Paul Fisher)

  Own Id: OTP-7464

- Calls to `erlang:system_info(allocated_areas)` could cause the runtime system
  with SMP support to crash.

  Own Id: OTP-7474

- The `env` option to `open_port()` could cause the runtime system with SMP
  support to crash.

  Own Id: OTP-7475

### Improvements and New Features

- Operations that needed to block other threads in the runtime system with SMP
  support unnecessarily waited for async threads to block. Most important the
  `erlang:memory/[0,1]` bif, code loading, and the `erlang:trace/3` bif.

  Own Id: OTP-7480

## Erts 5.6.3.2

### Fixed Bugs and Malfunctions

- Calls to `erlang:memory/[0,1]` could cause the runtime system with SMP support
  to crash.

  Own Id: OTP-7415

## Erts 5.6.3.1

### Fixed Bugs and Malfunctions

- Doing local call trace on bit syntax matching code that has been optimized
  with delayed sub-binary creation could crash the emulator.

  Own Id: OTP-7399 Aux Id: seq10978

## Erts 5.6.3

### Fixed Bugs and Malfunctions

- Only one to_erl process at a time is allowed to connect to the same run_erl
  pipe. Prevents buggy behaviour when IO from several to_erl's get interleaved.

  Own Id: OTP-5107

- IPv6 name resolving has now been fixed to use getaddrinfo() patch (thoroughly
  reworked) courtesy of Love Hörnquist-Åstrand submitted by Fredrik Thulin. It
  also can use gethostname2() patch (also reworked) courtesy of Mikael Magnusson
  for debian submitted by Sergei Golovan.

  Own Id: OTP-5382

- Improved error handling in run_erl

  Own Id: OTP-7252

- A permanent fix for the deadlock issue temporarily fixed by OTP-7260.

  - **OTP-7260** - The runtime system with SMP support could under rare
    circumstances deadlock when a distribution channel was taken down while
    multiple simultaneous operations were performed on it.

  Own Id: OTP-7267 Aux Id: OTP-7260

- ./configure has been improved to find 64-bit OpenSSL libraries.

  Own Id: OTP-7270

- A terminating process could under very rare circumstances trigger a bug which
  could crash the runtime system with SMP support.

  Own Id: OTP-7272

- SCTP_ADDR_CONFIRMED events are now handled by gen_sctp.

  Own Id: OTP-7276

- binary_to_term/1 would crash the emulator if the binary data contained an
  external fun with non-atom module and/or function. Corrected to generate a
  badarg exception.

  Own Id: OTP-7281

- On Mac OS 10.5 (Leopard), sending to socket which the other end closes could
  cause the emulator to consume 100% CPU time. (Thanks to Matthias Radestock.)

  Own Id: OTP-7289

- The vanilla driver used on Windows could crash the emulator and sometimes
  produced corrupt files. The vanilla driver is the driver that is used when one
  only pass a filename as first argument to [`open_port/2`](`open_port/2`).
  _NOTE_: This use of [`open_port/2`](`open_port/2`) is _obsolete_, and the
  documentation of this use has previously been removed. The functionality is
  only present for backward compatibility reasons and _will_ eventually be
  removed.

  Own Id: OTP-7301

- Faulty matching in binaries larger than 512Mb on 64bit machines fixed.(On
  32bit, the size limit for binaries is still 512Mb). Thanks to Edwin Fine and
  Per Gustafsson for finding fault and fix.

  Own Id: OTP-7309

- crypto:start() on Windows caused emulator to hang on error popup window if
  openssl DLL was not found. Windows error popups now suppressed.

  Own Id: OTP-7325

- Configuration option `without-termcap` can be used to disable the use of
  termcap libraries for terminal cursor control in the shell.

  Own Id: OTP-7338

- to_erl reports its terminal window size to run_erl in order to get output
  formatted accordingly

  Own Id: OTP-7342

- On Solaris, the `compressed` option for file operations did not work if the
  file descriptor happened to be greater than 255 (a problem with fopen() and
  friends in Solaris itself).

  Own Id: OTP-7343 Aux Id: seq10949

- A race condition in the runtime system with SMP support causing an erroneous
  removal of a newly created ets table has been fixed.

  The race occurred when a process removed a table during termination
  simultaneously as another process removed the same table via `ets:delete/1`
  and a third process created a table that accidentally got the same internal
  table index as the table being removed.

  Own Id: OTP-7349

- `zlib:inflate` failed when the size of the inflated data was an exact multiple
  of the internal buffer size (4000 bytes by default).

  Own Id: OTP-7359

- If the total number of allowed atoms is exceeded, there will now be a
  controlled termination of the emulator with a crash dump file. The emulator
  used to simply crash. (Thanks Howard Yeh and Thomas Lindgren.)

  Own Id: OTP-7372

- The break handler in werl on Windows could cause the emulator to hang or
  crash, that is now corrected.

  Own Id: OTP-7394 Aux Id: seq10969

- The configure script now tests for an serious optimization bug in gcc-4.3.0.
  If the bug is present, the configure script will abort (if this happens, the
  only way to build Erlang/OTP is to change to another version of gcc). (Thanks
  to Mikael Pettersson.)

  Own Id: OTP-7397

### Improvements and New Features

- On Unix, the emulator now notices when the width of the terminal has changed.
  (Thanks to Matthew Dempsky and Patrick Mahoney.)

  Own Id: OTP-7290

- There is a new function `init:stop/1` which can be used to shutdown the system
  cleanly AND generate a non-zero exit status or crash dump. (Thanks to Magnus
  Froberg.)

  Own Id: OTP-7308

- process_info(Pid, garbage_collection) now returns more information

  Own Id: OTP-7311

- The `hide` option for [`open_port/2`](`open_port/2`) is now documented.
  (Thanks to Richard Carlsson.)

  Own Id: OTP-7358

### Known Bugs and Problems

- Floating point arithmetic in drivers can cause a runtime system crash on
  runtime systems with floating point exceptions enabled. Floating point
  exceptions are disabled unless explicitly enabled or if hipe is enabled.

  Own Id: OTP-7237

## Erts 5.6.2

### Fixed Bugs and Malfunctions

- The maximum length of an atom of 255 characters is now strictly enforced.
  [`binary_to_term/1`](`binary_to_term/1`) will now fail with a badarg if an
  encoded term contains an atom longer than 255 characters. Atoms created by
  drivers will now be truncated to 255 characters if necessary. (Thanks to
  Matthew Dempsky.)

  Own Id: OTP-7147

- A bug in "bignum handling" on some 64bit architectures could cause rem and div
  operations on large numbers to hang indefinitely. Rem operations involving the
  smallest negative number representable in 28 bits or 60 bits could also cause
  access violation and emulator crash. Both errors are corrected.

  Own Id: OTP-7177

- When doing the initial garbage collection after waking a hibernated process, a
  fullsweep garbage collection was unnecessarily triggered.

  Own Id: OTP-7212

- The beta testing module `gen_sctp` now supports active mode as stated in the
  documentation. Active mode is still rather untested, and there are some issues
  about what should be the right semantics for `gen_sctp:connect/5`. In
  particular: should it be blocking or non-blocking or choosable. There is a
  high probability it will change semantics in a (near) future patch.

  Try it, give comments and send in bug reports\!

  Own Id: OTP-7225

- Invalid arguments to `ets:update_counter/3` were not handled correctly. A
  tuple position (`Pos`) less than 1 caused the element directly following the
  key to be updated (as if no position at all had been specified). All invalid
  values for `Pos` will now fail with `badarg`.

  Own Id: OTP-7226

- The runtime system with SMP support could under rare circumstances deadlock
  when a distribution channel was taken down while multiple simultaneous
  operations were performed on it.

  Own Id: OTP-7260

### Improvements and New Features

- More checksum/hash algorithms from the zlib library are now available as built
  in functions (like md5 hashes has been for a long time).

  Own Id: OTP-7128

- Minor improvements in the garbage collector.

  Own Id: OTP-7139 Aux Id: OTP-7132

- The switch "-detached" to the windows werl program now can create an erlang
  virtual machine without any main window and without a temporary console
  showing.

  Own Id: OTP-7142

- `erlang:system_info/1` now accepts the `logical_processors`, and
  `debug_compiled` arguments. For more info see the, `m:erlang` documentation.

  The scale factor returned by `test_server:timetrap_scale_factor/0` is now also
  effected if the emulator uses a larger amount of scheduler threads than the
  amount of logical processors on the system.

  Own Id: OTP-7175

- A new BIF ets:update_element/3. To update individual elements within an
  ets-tuple, without having to read, update and write back the entire tuple.

  Own Id: OTP-7200

- A process executing the `processes/0` BIF can now be preempted by other
  processes during its execution. This in order to disturb the rest of the
  system as little as possible. The returned result is, of course, still a
  consistent snapshot of existing processes at a time during the call to
  `processes/0`.

  The documentation of the `processes/0` BIF and the
  [`is_process_alive/1`](`is_process_alive/1`) BIF have been updated in order to
  clarify the difference between an existing process and a process that is
  alive.

  Own Id: OTP-7213

## Erts 5.6.1.1

### Fixed Bugs and Malfunctions

- Not enough parameters were passed when sending an error report in
  erl_check_io.c (Thanks to Matthew Dempsky).

  Own Id: OTP-7176

- In rare circumstances, complex binary matching code could cause the emulator
  to crash or not match when it should. (Thanks to Rory Byrne.)

  Own Id: OTP-7198

### Improvements and New Features

- The `{allocator_sizes, Alloc}` and `alloc_util_allocators` arguments are now
  accepted by `erlang:system_info/1`. For more information see the `m:erlang`
  documentation.

  Own Id: OTP-7167

- The finishing reallocation of the heap block when hibernating a process is now
  always moving the heap block since it drastically reduces memory fragmentation
  when hibernating large amounts of processes.

  Own Id: OTP-7187

## Erts 5.6.1

### Fixed Bugs and Malfunctions

- The SMP emulator on sparc64 erroneously used the sparc32 atomic and the
  sparc32 spinlock implementations which caused it to crash.

  Own Id: OTP-7006

- Call tracing the new guard BIFs `byte_size`, `bit_size`, or `tuple_size` and
  the loading a module that uses one of those functions, could cause the
  emulator to terminate.

  Own Id: OTP-7008

- configuring --enable-darwin-universal or --enable-darwin-64bit on MacOSX could
  result in a non optimized emulator. Top level configure script now corrected.

  Own Id: OTP-7014

- configuring --with-gd did not produce correct include flags for percept.

  Own Id: OTP-7015

- Environment variables weren't handled in thread safe manner in the runtime
  system with SMP support on Windows.

  `erl_drv_putenv()`, and `erl_drv_getenv()` has been introduced for use in
  drivers. Do _not_ use putenv(), or getenv() directly in drivers. For more
  information see the `erl_driver` documentation.

  Own Id: OTP-7035

- HIPE: Corrected the choice of interface to the send/3 and setnode/3 BIFs for
  native-compiled code. Using the incorrect interface could, in unusual
  circumstances, lead to random runtime errors.

  Own Id: OTP-7067

- Garbage collections could become extremely slow when there were many keys in
  the process dictionary. (Thanks to Fredrik Svahn.)

  Own Id: OTP-7068

- The duplicate documentation directory in the windows installation is removed.

  Own Id: OTP-7070

- Documentation bugfixes and clarifications.

  (Thanks to Joern (opendev@gmail.com), Matthias Lang, and Richard Carlsson.)

  Own Id: OTP-7079

- The runtime system with SMP support _not_ using the native atomic integer
  implementation part of OTP could deadlock when run on a system with more than
  one logical processor. That is, only the runtime system with SMP support on
  _other_ hardware platforms than x86, x86_64, sparc32, and powerpc32 were
  effected by this bug.

  Own Id: OTP-7080

- The break handling code (run when Ctrl-C is hit) could could potentially
  deadlock the runtime system with SMP support.

  Own Id: OTP-7104

- The sctp driver has been updated to work against newer lksctp packages e.g
  1.0.7 that uses the API spelling change adaption -> adaptation. Older lksctp
  (1.0.6) still work. The erlang API in gen_sctp.erl and inet_sctp.hrl now
  spells 'adaptation' regardless of the underlying C API.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7120

- A bug in `erlang:phash2/1` on 64-bit platforms has been fixed. (Thanks to
  Scott Lystig Fritchie.)

  Own Id: OTP-7127

- The emulator could under rare circumstances crash while garbage collecting.

  Own Id: OTP-7132

### Improvements and New Features

- The documentation has been updated so as to reflect the last updates of the
  Erlang shell as well as the minor modifications of the control sequence `p` of
  the `io_lib` module.

  Superfluous empty lines have been removed from code examples and from Erlang
  shell examples.

  Own Id: OTP-6944 Aux Id: OTP-6554, OTP-6911

- Bit syntax construction with a small integer in a non-byte aligned field wider
  than the CPU's word size could cause garbage bits in the beginning of the
  field.

  Own Id: OTP-7085

- All Windows versions older than Windows 2000 are now _not supported_ by the
  Erlang runtime system. This since there was a need for usage of features
  introduced in Windows 2000.

  Own Id: OTP-7086

- Memory management improvements especially for the runtime system with SMP
  support:

  - The runtime system with SMP support can now use multiple, thread specific
    instances of most memory allocators. This improves performance since it
    reduces lock contention in the memory allocators. It may however increase
    memory usage for some applications. The runtime system with SMP support will
    by default enable this feature on most allocators. The amount of instances
    used can be configured.
  - `driver_alloc()`, `driver_realloc()`, and `driver_free()` now use their own
    erts specific memory allocator instead of the default `malloc()`
    implementation on the system.
  - The default configuration of some allocators have been changed to fit
    applications that use much memory better.
  - Some new `erts_alloc` configuration parameters have been added.
  - `erts_alloc_config` has been modified to be able to create configurations
    suitable for multiple instances of allocators.
  - The returned value from `erlang:system_info({allocator, Alloc})` has been
    changed. This since an allocator may now run in multiple instances.

  If you for some reason want the memory allocators to be configured as before,
  you can pass the `+Mea r11b` command-line argument to `erl`.

  For more information see the `erts_alloc(3)`, the `m:erts_alloc_config`, and
  the `m:erlang` documentation.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7100

- On Unix, denormalized floating point numbers could not be created using
  [`list_to_float/1`](`list_to_float/1`) or
  [`binary_to_term/1`](`binary_to_term/1`). (Thanks to Matthew Dempsky.)

  Own Id: OTP-7122

- Native atomic integers and spin-locks are now also available for the runtime
  system with SMP support on sparc64.

  Own Id: OTP-7130

- FP exceptions support for sparc64 userspace on Linux has been added. Note that
  FP exception support is now turned off by default, so to actually enable it
  you need to do '`./configure --enable-fp-exceptions`'.

  Own Id: OTP-7131

## Erts 5.6

### Fixed Bugs and Malfunctions

- A bug for raw files when reading 0 bytes returning 'eof' instead of empty data
  has been corrected.

  Own Id: OTP-6291 Aux Id: OTP-6967

- All exported functions in gzio.c have now been renamed to avoid conflict with
  drivers that are indirectly linked with an external zlib library.

  Own Id: OTP-6816 Aux Id: OTP-6591

- On the 64-bit Erlang emulator, bit syntax construction with integers
  containing more than 60 bits ("big numbers") into fields with more than 60
  bits could produce incorrect results.

  Own Id: OTP-6833

- When the runtime system failed to allocate memory for binaries, it could dead
  lock while writing the `erl_crash.dump`.

  Own Id: OTP-6848

- The runtime system with SMP support could deadlock if a process called the
  `erlang:suspend_process(Pid)` BIF or the `erlang:garbage_collect(Pid)` BIF
  while the process identified by `Pid` was currently running and the process
  calling the BIFs was terminated during the call to the BIFs.

  Processes suspending each other via the `erlang:suspend_process/1` BIF or
  garbage collecting each other via the `erlang:garbage_collect/1` BIF could
  deadlock each other when the runtime system with SMP support was used.

  Own Id: OTP-6920

- `dbg` could leave traced processes in a suspended state if the tracer process
  was killed with exit reason `kill`.

  `erlang:suspend_process/2` has been introduced which accepts an option list as
  second argument. For more information see the `m:erlang` documentation.

  Processes suspended via `erlang:suspend_process/[1,2]` will now be
  automatically resumed if the process that called
  `erlang:suspend_process/[1,2]` terminates.

  Processes could previously be suspended by one process and resumed by another
  unless someone was tracing the suspendee. This is _not_ possible anymore. The
  process resuming a process _has_ to be the one that suspended it.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6946

- file:write_file/3, file:write/2 and file:read/2 could crash (contrary to
  documentation) for odd enough file system problems, e.g write to full file
  system. This bug has now been corrected.

  In this process the file module has been rewritten to produce better error
  codes. Posix error codes now originate from the OS file system calls or are
  generated only for very similar causes (for example 'enomem' is generated if a
  memory allocation fails, and 'einval' is generated if the file handle in
  Erlang is a file handle but currently invalid).

  More Erlang-ish error codes are now generated. For example `{error,badarg}` is
  now returned from `file:close/1` if the argument is not of a file handle type.
  See file(3).

  The possibility to write a single byte using `file:write/2` instead of a list
  or binary of one byte, contradictory to the documentation, has been removed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6967 Aux Id: OTP-6597 OTP-6291

- Monitor messages produced by the system monitor functionality, and garbage
  collect trace messages could contain erroneous heap and/or stack sizes when
  the actual heaps and/or stacks were huge.

  As of erts version 5.6 the `large_heap` option to
  `erlang:system_monitor/[1,2]` has been modified. The monitor message is sent
  if the sum of the sizes of all memory blocks allocated for all heap
  generations is equal to or larger than the specified size. Previously the
  monitor message was sent if the memory block allocated for the youngest
  generation was equal to or larger than the specified size.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6974 Aux Id: seq10796

- `inet:getopts/2` returned random values on Windows Vista.

  Own Id: OTP-7003

### Improvements and New Features

- The emulator internal process lock implementation has been rewritten and
  optimized. A slight risk of starvation existed in the previous implementation.
  This risk has also been eliminated in the new implementation.

  Own Id: OTP-6500

- Bitstrings (bit-level) binaries and binary comprehensions are now part of the
  language. See the Reference Manual.

  Own Id: OTP-6558

- The windows version of erlang now has SMP support. The SMP emulator is run by
  default on machines which shows more than one virtual or physical processor.

  Own Id: OTP-6560 Aux Id: OTP-6925

- The details of the compressed term format has been documented in
  erl_ext_dist.txt. (Thanks to Daniel Goertzen.)

  Own Id: OTP-6755

- The runtime system with SMP support is now started by default if more than one
  logical processor are detected. For more information, see the `erl(3)`
  documentation.

  Own Id: OTP-6756

- The external format for Erlang terms and the distribution protocol are now
  documented in ERTS User's Guide.

  Own Id: OTP-6779

- New BIF's erlang:system_profile/1 and erlang:system_profile/2. These BIF's
  controls concurrency profiling options for processes, ports and schedulers.

  Own Id: OTP-6783 Aux Id: OTP-6285

- The `ErlDrvTermData` term types used by `driver_output_term()` and
  `driver_send_term()` have been extended with the term types
  `ERL_DRV_BUF2BINARY`, `ERL_DRV_EXT2TERM`, and `ERL_DRV_UINT`.
  `ERL_DRV_BUF2BINARY` is used for passing and creating a binary,
  `ERL_DRV_EXT2TERM` is used for passing terms encoded with the external term
  format, and `ERL_DRV_UINT` is used for passing unsigned integers.

  Also the data types `ErlDrvUInt` and `ErlDrvSInt` have been added which makes
  it more obvious how arguments to term types are interpreted with regards to
  width and signedness.

  The incorrect data types `ErlDriverTerm`, `ErlDriverBinary`, and
  `ErlDriverPort` in the `erl_driver(3)` documentation have been replaced with
  the correct data types `ErlDrvTermData`, `ErlDrvBinary`, and `ErlDrvPort`.

  For more information see the `erl_driver(3)` documentation.

  Own Id: OTP-6823

- Miscellaneous improvements of the erts internal thread library.

  It now support optimized atomic operations and spin-locks on windows.

  Fall-backs based on mutexes and/or spin-locks for missing optimized atomic
  operations, spin-locks, or rwlocks has been implemented. This makes it
  possible to compile the runtime system with SMP support on a lot more
  platforms.

  Default stack size on OpenBSD has been increased to 256 kilo-words.

  Own Id: OTP-6831 Aux Id: OTP-6560

- Many bit syntax operations, both construction and matching, are faster. For
  further information, see the Efficiency Guide.

  Own Id: OTP-6838

- Literal lists, tuples, and binaries are no longer constructed at run-time as
  they used to be, but are stored in a per-module constant pool. Literals that
  are used more than once are stored only once.

  This is not a change to the language, only in the details of its
  implementation. Therefore, the implications of this change is described in the
  Efficiency Guide.

  Example 1: In the expression
  [`element(BitNum-1, {1,2,4,8,16,32,64,128})`](`element/2`), the tuple used to
  be constructed every time the expression was executed, which could be
  detrimental to performance in two ways if the expression was executed in a
  loop: the time to build the tuple itself and the time spent in garbage
  collections because the heap filled up with garbage faster.

  Example 2: Literal strings, such as `"abc"`, used to be stored in the compiled
  code compactly as a byte string and expanded to a list at run-time. Now all
  strings will be stored expanded to lists (such as `[$a,$b,$c]`) in the
  constant pool. That means that the string will be faster to use at run-time,
  but that it will require more space even when not used. If space is an issue,
  you might want to use binary literals (that is, `<<"abc">>`) instead of string
  literals for infrequently used long strings (such as error messages).

  Own Id: OTP-6850

- The Erlang driver API has been extended with a portable POSIX thread like API
  for multi-threading. The Erlang driver thread API provides:

  - Threads
  - Mutexes
  - Condition variables
  - Read/Write locks
  - Thread specific data

  For more information see the `erl_driver(3)` documentation.

  Own Id: OTP-6857

- Recursive calls now usually consume less stack than in R11B. See the
  Efficiency Guide.

  Own Id: OTP-6862 Aux Id: seq10746

- The deprecated BIFs `erlang:old_binary_to_term/1` and `erlang:info/1` have
  been removed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6876

- Calls to driver call-backs triggered by external events are now scheduled and
  interleaved with execution of Erlang processes also on the runtime system
  without SMP support.

  Own Id: OTP-6878

- Faster arithmetic of integers of more than 27 bits signed (or 60 bits signed
  on an 64-bit CPU), and also faster integer multiplication. (Thanks to Tony
  Rogvall.)

  Own Id: OTP-6891

- Significant improvements of the `process_info` BIFs:

  - [`process_info/2`](`process_info/2`) can now be called with a list of items
    as second argument in order to atomically retrieve information about
    multiple items.
  - `process_info/[1,2]` has been optimized in the runtime system with SMP
    support. The whole scheduler could previously be blocked for a significant
    period of time in `process_info/[1,2]` waiting for a lock on the process
    being inspected. The Erlang process calling `process_info/[1,2]` can still
    be blocked for a significant period of time waiting for the lock, but the
    scheduler will now be able to run other processes while the process calling
    `process_info/[1,2]` waits for the lock.
  - [`process_info/2`](`process_info/2`) now accept a few more items than
    before.
  - The documentation of `process_info/[1,2]` has been improved.

  For more information see the `m:erlang` documentation.

  Own Id: OTP-6899

- [`open_port({}, [])`](`open_port/2`) could crash the emulator. (Thanks to
  Matthew Dempsky.)

  Own Id: OTP-6901

- Two new guard BIFs have been introduced as a recommended replacement for
  [`size/1`](`size/1`). (The [`size/1`](`size/1`) BIF will be removed no earlier
  than in R14B.) The BIFs are [`tuple_size/1`](`tuple_size/1`) to calculate the
  size of a tuple and [`byte_size/1`](`byte_size/1`) to calculate the number of
  bytes needed for the contents of the binary or bitstring (rounded up to the
  nearest number of bytes if necessary).

  There is also a new [`bit_size/1`](`bit_size/1`) BIF that returns the exact
  number of bits that a binary or bitstring contains.

  Own Id: OTP-6902

- The `ets:fixtable/2` function, which has been deprecated for several releases,
  has been removed.

  The `ets:info/1` function has been reimplemented as a BIF, which guarantees
  that information returned is consistent.

  The `ets:info/2` function now fails with reason `badarg` if the second
  argument is invalid. (Dialyzer can be used to find buggy code where the second
  argument is misspelled.)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6906

- As the linux kernel may generate a minor fault when tracing with CPU
  timestamps, and there exists no patch to the Linux kernel that fixes the
  problem, cpu timestamps are disabled on Linux for now.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6922

- The functions io:columns/0, io:columns/1, io:rows/0 and io:rows/1 are added to
  allow the user to get information about the terminal geometry. The shell takes
  some advantage of this when formatting output. For regular files and other
  io-devices where height and width are not applicable, the functions return
  \{error,enotsup\}.

  Potential incompatibility: If one has written a custom io-handler, the handler
  has to either return an error or take care of io-requests regarding terminal
  height and width. Usually that is no problem as io-handlers, as a rule of
  thumb, should give an error reply when receiving unknown io-requests, instead
  of crashing.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6933

- `driver_caller()` can now also be used from the `start` callback of a driver.

  Own Id: OTP-6951

- The emulator can now be compiled for 64bit intel, as well as a 32bit universal
  binary on darwin/MacOSX 10.4 and 10.5.

  Own Id: OTP-6961

- If `open_port` fails because all available ports are already in use, it will
  now throw a `system_limit` exception instead of an `enfile` exception.
  (`enfile` might still be thrown if the operating system would return ENFILE.)

  Own Id: OTP-6968

- The [`spawn_monitor/1`](`spawn_monitor/1`) and
  [`spawn_monitor/3`](`spawn_monitor/3`) BIFs are now auto-imported (i.e. they
  no longer need an `erlang:` prefix).

  Own Id: OTP-6975

- On Windows, the werl window now handles resize, so that the whole window can
  be utilized. Text selection is also updated to be line oriented instead of
  rectangle oriented as in earlier versions.

  Own Id: OTP-6994 Aux Id: OTP-6933

- Kqueue support (kernel-poll) has been enabled on FreeBSD. The problem with
  kqueue not detecting writes on a pipe on FreeBSD was actually not a kqueue
  issue, but a writev on pipes issue. Neither poll(), nor select() detected the
  write when the bug hit. NetBSD and DragonFlyBSD probably have or have had the
  same bug. This bug should have been fixed in FreeBSD 6.3 and FreeBSD 7.0
  thanks to Jean-Sebastien Pedron.

  Own Id: OTP-7001

## Erts 5.5.5.5

### Fixed Bugs and Malfunctions

- Hanging writes on temporarily unavailable NFS filesystems could cause the
  execution of (not file related) erlang code to get blocked even though I/O
  threads were used. This is now corrected.

  Own Id: OTP-6907 Aux Id: seq10771

## Erts 5.5.5.4

### Fixed Bugs and Malfunctions

- Data passed to a driver via `erlang:port_call` could be corrupted when the
  runtime system with SMP support was used. (Thanks to YAMASHINA Hio.)

  Own Id: OTP-6879

- In the SMP emulator, if several processes called ets:update_counter/3 (even
  for different tables) when the counter values exceeded 27 bits, the counter
  values could be corrupted or the emulator could crash.

  Own Id: OTP-6880 Aux Id: seq10760

## Erts 5.5.5.3

### Fixed Bugs and Malfunctions

- Creating a named table using `ets:new/2` or renaming a named table using
  `ets:rename/2` could in rare circumstances succeed, meaning that there would
  be two or more tables with the same name. Now such call will fail with a
  `badarg` exception as it is supposed to do.

  `ets:delete/1` used on a named table now removes the name immediately so that
  a new table with the same name can be created.

  Turning on call trace on the internal BIF that implements `ets:delete/1` would
  crash the emulator.

  SMP emulator only: Using `ets:rename/2` on a table that `ets:safe_fixtable/2`
  has been used on could cause an emulator crash or undefined behaviour because
  of a missing lock.

  Own Id: OTP-6872 Aux Id: seq10756, seq10757

## Erts 5.5.5.2

### Known Bugs and Problems

- ets:select/3 on ordered_set and with a chunksize a multiple of 1000 gave all
  elements instead of just 1000. Also ets:slot/2 on ordered set could give
  unexpected results on SMP emulator. Both problems are corrected.

  Own Id: OTP-6842

## Erts 5.5.5.1

### Fixed Bugs and Malfunctions

- All exported functions in gzio.c have now been renamed to avoid conflict with
  drivers that are indirectly linked with an external zlib library.

  Own Id: OTP-6816 Aux Id: OTP-6591

- Calling binary_to_term/1 with certain invalid binaries would crash the
  emulator.

  Own Id: OTP-6817

### Improvements and New Features

- Restored speed of bit-syntax matching of 32 bits integers.

  Own Id: OTP-6789 Aux Id: seq10688

## Erts 5.5.5

### Fixed Bugs and Malfunctions

- The functions in gzio.c have been renamed to avoid conflict with drivers that
  indirectly linked with an external zlib library.

  Own Id: OTP-6591

- The emulator without SMP support dumped core if an async-thread finished a job
  after the emulator had begun writing an `erl_crash.dump`.

  Own Id: OTP-6685

- In bit syntax matching, integer fields with greater size than 16Mb would fail
  to match. (Thanks to Bertil Karlsson and Francesco Pierfederici.)

  Matching out a 32-bit integer not aligned on a byte boundary from a binary
  could cause an heap overflow (and subsequent termination of the emulator).

  A module that contained bit syntax construction with a literal field size
  greater than 16Mb would fail to load.

  Several other similar bugs having to do with huge field sizes were eliminated.

  Attempting to construct a binary longer than 536870911 bytes will now fail
  with a `system_limit` exception (rather than fail in mysterious ways or
  construct an binary with incorrect contents). Similarly, attempting to match a
  binary longer than 536870911 bytes will now fail (instead of producing an
  incorrect result). This limitation has been documented in the Efficiency
  Guide. (The limit is in the 32-bit emulator; use the 64-bit emulator if you
  need to handle larger binaries than 536870911.)

  Own Id: OTP-6686

- Bugs in rem and div of very large numbers are corrected.

  Own Id: OTP-6692

- `erlang:system_info({allocator, Alloc})` didn't allocate enough heap when a
  bignum was part of the result which could cause an emulator crash.

  Own Id: OTP-6693

- It was previously not possible to pass `erts_alloc` the same configuration via
  the command-line, as used by default.

  A `+M*` command-line argument that configure a size of some sort can now be
  passed a value that equals the size of the address space. The value used, in
  this case, will be `"the size of the address space" - 1`.

  Own Id: OTP-6699

- `SysIOVec* driver_peekq(ErlDrvPort port, int *vlen)` did not update `*vlen` if
  `port` was invalid. `*vlen` is now set to `-1` if the `port` is invalid.

  The `efile` driver expects `*vlen` to be updated also when the `port` is
  invalid. This situation occurs seldom, but when the runtime system has
  async-threads enabled and ports are killed it can. When it occurred the
  runtime system crashed.

  Own Id: OTP-6729

### Improvements and New Features

- For scripts written using `escript`, there is a new function
  `escript:script_name/0`, which can be used to retrieve the pathame of the
  script. The documentation has been clarified regarding pre-defined macros such
  as ?MODULE and the module name.

  Own Id: OTP-6593

- The section Guards in the chapter The Abstract Format of the ERTS User's Guide
  has been updated.

  Own Id: OTP-6600

- Corrected protocol layer flue for socket options SO_LINGER, SO_SNDBUF and
  SO_RCVBUF, for SCTP.

  Own Id: OTP-6625 Aux Id: OTP-6336

- The behaviour of the inet option \{active,once\} on peer close is improved and
  documented.

  Own Id: OTP-6681

- The inet option send_timeout for connection oriented sockets is added to allow
  for timeouts in communicating send requests to the underlying TCP stack.

  Own Id: OTP-6684 Aux Id: seq10637 OTP-6681

- The command line flag `-args_file FileName`, and the environment variables
  `ERL_AFLAGS`, and `ERL_ZFLAGS` for the `erl` command have been added. For more
  information see the [erl](erl_cmd.md) documentation.

  Own Id: OTP-6697

- The `is_constant/1` type test has been deprecated. `is_constant/1` is
  improperly named and almost entirely undocumented.

  Own Id: OTP-6731

## Erts 5.5.4.3

### Fixed Bugs and Malfunctions

- [`process_flag(trap_exit, Bad)`](`process_flag/2`) where `Bad` was a term not
  equal to `true` or `false`, didn't fail with `badarg` as it should; instead,
  the failure was silently ignored. This bug was introduced in `erts-5.5.2`.

  Own Id: OTP-6627 Aux Id: OTP-6160

- The minimum and default stack size for async-threads has been increased to 16
  kilowords. This since the previous minimum and default stack size of 8
  kilowords proved to be too small (introduced in `erts-5.5.4.2`).

  Own Id: OTP-6628 Aux Id: OTP-6580, Seq10633

### Improvements and New Features

- process_flag/2 accepts the new flag `sensitive`.

  Own Id: OTP-6592 Aux Id: seq10555

## Erts 5.5.4.2

### Fixed Bugs and Malfunctions

- When a port steals control over a file descriptor from another port, the
  stealing port tests if the other port is alive. This in order to be able to
  give an accurate error message. In the runtime system with SMP support, this
  test was done without appropriate locks held. This could in worst case lead to
  an erroneous error message; therefore, this bug is to be considered harmless.

  Own Id: OTP-6602

### Improvements and New Features

- The default stack size for threads in the async-thread pool has been shrunk to
  8 kilowords, i.e., 32 KB on 32-bit architectures. This small default size has
  been chosen since the amount of async-threads might be quite large. The
  default stack size is enough for drivers delivered with Erlang/OTP, but might
  not be sufficiently large for other dynamically linked in drivers that use the
  `driver_async()` functionality. A suggested stack size for threads in the
  async-thread pool can be configured via the `+a` command line argument of
  [erl](erl_cmd.md).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6580

## Erts 5.5.4.1

### Fixed Bugs and Malfunctions

- Setting the time on the system while using heart on a linux machine where the
  emulator was built with clock_gettime support (default from Linux
  2.6/erts-5.5.4 and upwards), could make the heart command fire. This was due
  to bug in the heart executable which is now corrected.

  Own Id: OTP-6598 Aux Id: seq10614

## Erts 5.5.4

### Fixed Bugs and Malfunctions

- Corrected misspelling of '`-pz`' in the help text for `erlc`. (Thanks to Ulf
  Wiger.)

  Own Id: OTP-6433

- The MD5 calculation of a BEAM file done by `code:module_md5/1`,
  `beam_lib:md5/1`, and by the compiler for the default value of the `vsn`
  attribute have all been changed so that its result will be the same on all
  platforms; modules containing funs could get different MD5s on different
  platforms.

  Own Id: OTP-6459

- The emulator could dump core while writing an `erl_crash.dump` file if there
  were funs with a large terms in its environment. Since there is no way to
  inspect a fun's environment in the crashdump_viewer application anyway, a
  variables in the environment are now set to [] before dumping the fun. (Thanks
  to Jean-Sebastien Pedron.)

  Own Id: OTP-6504

- `{Port, {exit_status, S}}` messages from ports opened with the `exit_status`
  option could under rare circumstances be delayed. This bug was present on
  Erlang runtime systems without SMP support on all unix operating systems other
  than SunOS.

  Own Id: OTP-6528

- A bug in linuxthreads could cause the emulator to dump core when dlerror() was
  called before the first call to dlopen(). As a workaround the emulator always
  makes a call to dlopen() on initialization when linuxthreads is used as thread
  library.

  Own Id: OTP-6530

- file:sync/1 did not do anything on Windows. Now it calls the system function
  for flushing buffers (FlushFileBuffers()). (Thanks to Matthew Sackman.)

  Own Id: OTP-6531

- [`open_port/2`](`open_port/2`) could on the runtime system with SMP support
  fail with the wrong exit reason when a port couldn't be created. When this
  happened the exit reason was typically `eintr`, or `ebusy` instead of
  `eagain`.

  Own Id: OTP-6536

- The file driver (efile_drv) did not flush data written using the option
  'delayed_write' after the set timeout time, rather at the next file operation.
  This bug has now been corrected.

  Own Id: OTP-6538

### Improvements and New Features

- An interface towards the SCTP Socket API Extensions has been implemented.It is
  an Open Source patch courtesy of Serge Aleynikov and Leonid Timochouk. The
  Erlang code parts has been adapted by the OTP team, changing the Erlang API
  somewhat.

  The Erlang interface consists of the module `gen_sctp` and an include file
  `-include_lib("kernel/include/inet_sctp.hrl").` for option record definitions.
  The `gen_sctp` module is documented.

  The delivered Open Source patch, before the OTP team rewrites, was written
  according to http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13 and was
  claimed to work fine, tested on Linux Fedora Core 5.0 (kernel 2.6.15-2054 or
  later) and on Solaris 10 and 11. The OTP team rewrites used the same standard
  document but might have accidentally broken some functionality. If so it will
  soon be patched to working state. The tricky parts in C and the general design
  has essentially not changed. During the rewrites the code was hand tested on
  SuSE Linux Enterprise Server 10, and briefly on Solaris 10. Feedbach on code
  and docs is very much appreciated.

  The SCTP interface is in beta state. It has only been hand tested and has no
  automatic test suites in OPT meaning everything is most certainly not tested.
  Socket active mode is broken. IPv6 is not tested. The documentation has been
  reworked due to the API changes, but has not been proofread after this.

  Thank you from the OTP team to Serge Aleynikov and Leonid Timochouk for a
  valuable contribution. We hope we have not messed it up too much.

  Own Id: OTP-6336

- A `{minor_version,Version}` option is now recognized by
  [`term_to_binary/2`](`term_to_binary/2`). \{minor_version,1\} will cause
  floats to be encoded in an exact and more space-efficient way compared to the
  previous encoding.

  Own Id: OTP-6434

- There is a new `escript` program that can be used for writing scripts in
  Erlang. Erlang scripts don't need to be compiled and any arguments can be
  passed to them without risk that they are interpreted by the Erlang system.

  Own Id: OTP-6505

- Setting and getting socket options in a "raw" fashion is now allowed. Using
  this feature will inevitably produce non portable code, but will allow setting
  ang getting arbitrary uncommon options on TCP stacks that do have them.

  Own Id: OTP-6519

- Miscellaneous signal handling improvements on the Erlang runtime system with
  SMP support.

  The fallback implementation of spin locks and atomic operations are now
  implemented using pthread spin locks when pthread spin locks are found on the
  system.

  The Erlang runtime system with SMP support can now run on Linux systems that
  has Linuxthreads instead of NPTL (Native POSIX Thread Library). Note that the
  SMP support hasn't been as thoroughly tested with Linuxthreads as with NPTL. A
  runtime system with SMP support will therefore not be built by default on
  Linux when NPTL isn't found. In order to force a build of the runtime system
  with SMP support, pass `--enable-smp-support` to `configure` when building
  OTP.

  Own Id: OTP-6525

## Erts 5.5.3.1

### Fixed Bugs and Malfunctions

- `erlang:system_flag(multi_scheduling, block)` could cause the emulator with
  SMP support to deadlock.

  Own Id: OTP-6431 Aux Id: OTP-6403

- The runtime system with SMP support failed to call the driver timeout callback
  of ports in state closing. This could cause ports to fail flushing their I/O
  queues.

  Own Id: OTP-6432

- The `{Port, closed}` message from a closed port could arrive at the port owner
  before `Port` had been removed from the result of `erlang:ports/0` in the
  runtime system with SMP support.

  Own Id: OTP-6437

- The async id of async jobs created via `driver_async` wasn't created in a
  thread safe manner in the runtime system with SMP support. This could in worst
  case cause `driver_async_cancel()` to cancel another async job than intended.

  Own Id: OTP-6438

- Under rare circumstances a terminating connection between two nodes could
  cause an instantaneous reconnect between the two nodes to fail on the runtime
  system with SMP support.

  Own Id: OTP-6447

- In the documentation of the driver entry field `extended_marker` of erts
  version 5.5.3 (`driver_entry(3)`), the following is stated: "The following
  fields are ignored if this field is equal to `0`". This is a documentation bug
  and has been changed to: "If this field is equal to `0`, all the fields
  following this field also _have_ to be `0`, or `NULL` in case it is a pointer
  field".

  The runtime check for detection of old incompatible drivers made when loading
  drivers has been improved. The emulator can, however, not make sure that a
  driver that doesn't use the extended driver interface isn't incompatible.
  Therefore, when loading a driver that doesn't use the extended driver
  interface, there is a risk that it will be loaded also when the driver is
  incompatible. When the driver use the extended driver interface, the emulator
  can verify that it isn't of an incompatible driver version. You are therefore
  advised to use the extended driver interface.

  For more information see the `erl_driver(3)` and `driver_entry(3)`
  documentation.

  Own Id: OTP-6452 Aux Id: OTP-6330

- When terminating ports on the runtime system with SMP support, removal of
  links to processes was done without locking the link lock on processes. This
  could cause an emulator crash.

  Own Id: OTP-6475

- The emulator with SMP support could crash when a port flushed its I/O queue by
  calling `driver_async()` from the timeout driver callback.

  Own Id: OTP-6479

- Large exit reasons could under rare circumstances cause the runtime system
  with SMP support to crash.

  Own Id: OTP-6521

### Improvements and New Features

- Faster system calls for keeping the time accurate are used on newer Linux
  kernels, which can result in a significant speed-up of the emulator on those
  systems.

  Own Id: OTP-6430

- Added number of async threads and number of scheduler threads to the system
  information that can be retrieved via `driver_system_info()`. For more
  information see the `erl_driver(3)` documentation.

  Own Id: OTP-6440

- When `SIGUSR1` is received by the runtime system with SMP support, the
  `erl_crash.dump` is now written by a special thread, instead of as before
  directly from the signal handler.

  Own Id: OTP-6465

- term_to_binary/2 with compression is now faster.

  term_to_binary/2 now accepts the option '`{compressed,Level}`' for specifying
  the compression level. `Level` must be in the range 0 (no compression) through
  9 (highest compression level). Default is 6.

  Future compatibility bugfix: binary_to_term/1 did not handle the `Uniq` and
  `Index` fields correctly.

  Own Id: OTP-6494

- Removed unnecessary reallocation when initializing kernel-poll set.

  Own Id: OTP-6516

## Erts 5.5.3

### Fixed Bugs and Malfunctions

- Node and fun table entries could under rare circumstances be deallocated
  multiple times on the emulator with SMP support.

  Own Id: OTP-6369

- epoll_wait() can repeat entries for the same file descriptor in the result
  array. This could cause the ready_input, ready_output, or event callbacks of a
  driver to unintentionally be called multiple times. We have only noted
  repeated entries when an error condition has occurred on the file descriptor.
  In this case, the repeated entries should normally not be a problem for the
  driver since it should detect the error and de-select the file descriptor.
  Therefore this bug should be considered harmless. The emulator now coalesce
  repeated entries into one.

  You are only affected by this bug if you are using erts-5.5.2.\* and the
  kernel-poll feature on linux.

  Own Id: OTP-6376 Aux Id: OTP-6222

- If a process that was waiting in gen_tcp:accept/1 was killed, calling
  gen_tcp:accept/1 again on the same listen socket would fail with
  '`{error,einval}`'.

  Own Id: OTP-6381 Aux Id: seq10535

- The emulator failed to start on Solaris 10 when kernel-poll support was
  enabled and the maximum number of filedescriptors configured was less than or
  equal to 256.

  Own Id: OTP-6384 Aux Id: OTP-6222

- The R10B compiler could generate unsafe `bs_save/bs_restore` instructions that
  could cause memory corruption. (The R11B compiler does not have that problem.)
  The erlang emulator will now refuse to load R10B-compiled modules that contain
  such unsafe `bs_save/bs_restore` instructions. In addition, the beam_validator
  module in the compiler will also reject such instructions (in case it is used
  to validate R10B code). (Thanks to Matthew Reilly.)

  Own Id: OTP-6386

- Process and port timers could fail to work properly on the runtime system with
  SMP support. Many thanks to Dmitriy Kargapolov and Serge Aleynikov who tracked
  down this bug.

  Own Id: OTP-6387

- Bit syntax code compiled by an R10B compiler that matched out a floating point
  number would not properly check that the floating point number was valid; if
  the float was, for instance, NaN the emulator could crash.

  Own Id: OTP-6395

- statistics(runtime) on Windows used to return the elapsed system time for the
  process, instead of the user time. Corrected. (Thanks to Robert Virding.)

  Own Id: OTP-6407

- A loadable driver (loaded by erl_ddll) which used driver_async() would cause
  the emulator to crash. (Thanks to Dmitriy Kargapolov.)

  Own Id: OTP-6410

- Under rare circumstances the emulator on unix platforms could fail to
  terminate the Erlang port corresponding to a port program opened with the
  `exit_status` option.

  Own Id: OTP-6411

- A link removed via [`unlink/1`](`unlink/1`) could under rare circumstances
  transfer exit signals for a short period of time after the call to
  [`unlink/1`](`unlink/1`) had returned when the runtime system with SMP support
  was used.

  Own Id: OTP-6425 Aux Id: OTP-6160

### Improvements and New Features

- In the runtime system with SMP support, ports are now being scheduled on the
  scheduler threads interleaved with processes instead of being run in a
  separate I/O thread.

  Own Id: OTP-6262

- More interfaces are added in erl_ddll, to support different usage scenarios.

  Own Id: OTP-6307 Aux Id: OTP-6234

- In the runtime system with SMP support, the global I/O lock has been replaced
  with a more fine grained port locking scheme. Port locking is either done on
  driver level, i.e., all ports executing the same driver share a lock, or on
  port level, i.e., each port has its own lock. Currently the inet driver, the
  efile driver, and the spawn driver use port level locking and all other
  statically linked in drivers use driver level locking. By default dynamically
  linked in drivers will use locking on driver level. For more information on
  how to enable port level locking see the `erl_driver(3)` and the
  `driver_entry(3)` man pages.

  As of erts version 5.5.3 the driver interface has been extended. The extended
  interface introduce version management, the possibility to pass capability
  flags to the runtime system at driver initialization, and some new driver API
  functions. For example, the `driver_system_info()` function which can be used
  to determine if the driver is run in a runtime system with SMP support or not.
  The extended interface doesn't have to be used, _but_ dynamically linked in
  driver _have_ to be recompiled. For information see the `erl_driver(3)` and
  the `driver_entry(3)` man pages.

  _NOTE:_ Dynamically linked in drivers _have_ to be recompiled.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6330 Aux Id: OTP-6262

- A test and debug feature which modifies the timing of the runtime system has
  been added. For more information, see documentation of the `+T<Level>` command
  line argument of [erl](erl_cmd.md).

  Own Id: OTP-6382

- The version of zlib (http://zlib.net) linked into run-time system has been
  updated to version 1.2.3.

  Own Id: OTP-6394

- The `erlc` program now passes on the `-smp` and `-hybrid` options to the
  Erlang emulator it starts. This is mainly useful for compiling native code,
  because native code must be compiled with same type of run-time system as in
  which it will be run.

  If the `-s` option is given, `erlc` now prints a warning that it is deprecated
  and that it will be removed in R12B.

  Own Id: OTP-6398

- The `schedulers` option of `erlang:system_flag/2` has been removed, i.e., the
  number of scheduler threads cannot be changed after emulator boot time any
  more.

  A `multi_scheduling` option has been added to `erlang:system_flag/2`. This
  option can be used for blocking and unblocking multi-scheduling. For more
  information see the `m:erlang` documentation.

  Own Id: OTP-6403

- A port program that had been started with the `exit_status` option and closed
  one of the pipes used for communication with the emulator caused the emulator
  to continuously poll for termination of the port program. This only became a
  problem when the emulator didn't have other things to do and the port program
  closed a pipe much earlier than it terminated. When the emulator had other
  things to do, such as running Erlang processes, the emulator polled for
  termination in between scheduling of processes.

  Now the emulator doesn't poll for termination of the port program at all;
  instead, it waits for the child signal from the terminated port program to
  arrive and then schedules the Erlang port for termination.

  The termination of any port programs have also been optimized. Previously the
  termination of any port program did always cause a scan of a table of the size
  equal to maximum number of file descriptors. If the maximum number of file
  descriptors was large, this scan could be quite expensive. Now the search have
  been reduced to the set of ports started with the `exit_status` option.

  Note, all of the above only applies to Erlang emulators on Unix platforms.

  Own Id: OTP-6412 Aux Id: seq10194

- \* BEAM: added support for floating-point exceptions on FreeBSD (32-bit x86)

  \* SMP: made locking procedures work even when native lock operations aren't
  implemented

  \* SMP: improved timing accuracy in the timer thread (if enabled)

  Own Id: OTP-6424

## Erts 5.5.2.2

### Fixed Bugs and Malfunctions

- A bug in the kernel poll implementation could cause the emulator to omit
  polling for events on file descriptors. The bug was only present when using
  the kernel poll implementation based on epoll or kqueue. This bug was
  introduced in erts-5.5.2.

  Own Id: OTP-6344 Aux Id: OTP-6222

## Erts 5.5.2.1

### Fixed Bugs and Malfunctions

- The run_erl program now acquires its pseudo-ttys using openpty(3), so that it
  will work on newer Linux distributions that don't have the traditional
  pseudo-tty devices in the file system. On platforms that don't have
  openpty(3), run_erl will still search for pseudo-tty devices in the file
  system.

  The run_erl program will now wait using waitpid(3) to prevent the program it
  spawned to become defunct. run_erl will also terminate after a delay of 5
  seconds (to allow any pending output to be written to the log file) if the
  spawned program terminates even if some child of it still holds stdin and/or
  stdout open.

  Own Id: OTP-6225 Aux Id: seq10500

- A bug in ordered_set ETS datatyp caused ets:select (and match) to return
  faulty results when the table contained process id's from another node.

  Own Id: OTP-6338

## Erts 5.5.2

### Fixed Bugs and Malfunctions

- erlc: A typo in the help text for '-pa path' was corrected.

  Own Id: OTP-6218

- Failure in port command/control driver callbacks could crash the non-SMP
  emulator. This bug was introduced in the 5.5 version of erts.

  Own Id: OTP-6224

- Erroneous "`Input driver gone away without deselecting!`" error reports could
  sometimes occur when a driver instance terminated in the ready_output()
  callback of the driver. This bug was only present in emulators that used
  poll(). Note, that this bug was harmless, the only effect it had was the
  erroneous error report.

  Own Id: OTP-6229 Aux Id: OTP-3993, Seq5266, Seq7247, OTP-4307

- The emulator could cause a type assertion failure while writing an
  erl_crash.dump, causing the erl_crash.dump to be terminated and a core dump
  generated.

  Own Id: OTP-6235 Aux Id: seq10444

- The registered name of a process is now the last observable resource removed
  before links and monitors are triggered when a process terminates.

  Previously ets tables were removed after the registered name. This could cause
  problems on the runtime system with SMP support for code that expected that
  ets tables owned by a specific process had been removed if the name of the
  process had been removed.

  Own Id: OTP-6237

- Failure to fork() a new (os) process could cause the emulator to deadlock.
  This bug affect all emulators with SMP support, and emulators with async
  thread support on SunOS.

  Own Id: OTP-6241 Aux Id: OTP-3906

- Fprof traces could become truncated for the SMP emulator. This bug has now
  been corrected.

  Own Id: OTP-6246

- The undocumented functions inet:getiflist/0,1 inet:ifget/2,3 and inet:getif/1
  were completely broken on Windows. That has been fixed.

  Own Id: OTP-6255

- Behavior in case of disappeared nodes when using the dist_auto_connect once
  got changed in R11B-1. The timeouts regarding normal distributed operations is
  now reverted to the old (pre R11B-1).

  Own Id: OTP-6258 Aux Id: OTP-6200, seq10449

- The bsl and bsr operators could cause the emulator to crash if given invalid
  arguments. (Thanks to datacompboy and Per Gustafsson.)

  Own Id: OTP-6259

- driver_cancel_timer() could under certain circumstances fail to cancel the
  timer on the runtime system with SMP support.

  Own Id: OTP-6261

- A call to erlang:system_info(info) could deadlock the runtime system with SMP
  support.

  Own Id: OTP-6268

### Improvements and New Features

- Exit signals are now truly asynchronous in the runtime system with SMP
  support. This simplifies locking in bifs a lot, and makes process termination
  cheaper.

  Own Id: OTP-6160

- When tracing on the runtime system with SMP support it can be difficult to
  know when a trace has been delivered to the tracer. A new built in function
  `erlang:trace_delivered/1` has been introduced in order to make it easier to
  know when the trace has been delivered. See the erlang(3) man page for more
  information.

  Own Id: OTP-6205 Aux Id: OTP-6269

- Kernel poll support can now be combined with SMP support. Currently the
  following kernel poll versions exist: `/dev/poll`, `epoll`, and `kqueue`.
  Linux `kpoll` has been replaced with `epoll`. Some time in the future there
  will also be a kernel poll version using Solaris event ports.

  The "check io" implementation for unix has been completely rewritten. The
  current kernel poll implementation reduce the amount of system calls needed
  compared to the old kernel poll implementation.

  When `epoll` or `kqueue` is used either `poll` or `select` is used as
  fallback. Previously only `poll` could be used as fallback. Since `select` now
  can be used as fallback, kernel poll support is now also available on newer
  MacOSX. Note however, when `select` is used as fallback, the maximum number of
  file descriptors is limited to `FD_SETSIZE`.

  Kernel poll support is now enabled by default if `/dev/poll`, `epoll`, or
  `kqueue` is found when building OTP, i.e. you do not have to pass the
  `--enable-kernel-poll` argument to `configure`. As before, kernel poll is
  disabled by default in the runtime system. In order to enable it, pass the
  `+Ktrue` command line argument to `erl`.

  Note: `configure` will refuse to enable kernel poll support on FreeBSD since
  `kqueue` have problems with (at least) pipes on all version of FreeBSD that we
  have tested.

  Own Id: OTP-6222 Aux Id: seq10380

- The `erl_ddll` module and the code in the emulator have been completely
  rewritten; several bugs were fixed.

  Own Id: OTP-6234

- The SMP emulator now avoids locking for the following operations (thus making
  them as fast as in the UP emulator): [`atom_to_list/1`](`atom_to_list/1`),
  atom comparison, atom hashing, `erlang:apply/3`.

  Own Id: OTP-6252

- There are new BIFs `erlang:spawn_monitor/1,3`, and the new option `monitor`
  for `spawn_opt/2,3,4,5`.

  The `observer_backend` module has been updated to handle the new BIFs.

  Own Id: OTP-6281

## Erts 5.5.1.1

### Improvements and New Features

- There is now an option read_packets for UDP sockets that sets the maximum
  number of UDP packets that will be read for each invocation of the socket
  driver.

  Own Id: OTP-6249 Aux Id: seq10452

## Erts 5.5.1

### Fixed Bugs and Malfunctions

- Complex pattern matching of strings would fail in the 64 bits emulator because
  of a bug in the loader. (Thanks to Igor Goryachev.)

  Own Id: OTP-6142

- `-134217728 div 134217728` and `-134217728 rem 134217728` would be calculated
  incorrectly. [`abs(-2147483648)`](`abs/1`) could in unlucky circumstances
  cause a heap overflow, as could size(Binary) when size of the binary was
  larger than 128Mb.

  Own Id: OTP-6154

- erlang:display/1 displayed erroneous values for negative integers.

  Big integers (both positive and negative) were previously displayed in
  hexadecimal form while small integers were displayed in decimal form. All
  integers are now displayed in decimal form.

  NOTE: erlang:display/1 should only be used for debugging.

  Own Id: OTP-6156

- A call to erlang:trace/3 with erroneous flags caused the SMP emulator to
  deadlock instead of exiting the calling process with badarg.

  Own Id: OTP-6175

- A bug causing the emulator to hang when exiting a process that is exception
  traced has been fixed.

  Own Id: OTP-6180

- ets:rename/1 could deadlock, or crash the SMP emulator when the table wasn't a
  named table.

  ets:next/2, and ets:prev/2 could return erroneous results on the SMP emulator.

  Own Id: OTP-6198 Aux Id: seq10392, seq10415

- A memory allocation bug could cause the SMP emulator to crash when a process
  had executed a `receive after` with a larger timeout than 10 minutes.

  Own Id: OTP-6199

- The runtime system with SMP support did not slowly adjust its view of time
  when the system time suddenly changed.

  Timeouts could sometimes timeout too early on the runtime system with SMP
  support.

  Own Id: OTP-6202

### Improvements and New Features

- The smp runtime system now automatically detects the number of logical
  processors on MacOSX (darwin) and OpenBSD.

  The smp runtime system is now built by default on MacOSX (darwin) on x86.

  Own Id: OTP-6119

- The `-smp` command line argument now take the following options: `enable`,
  `auto`, or `disable`.

  Especially the `-smpauto` argument is useful since it starts the Erlang
  runtime system with SMP support if it is available and more than one logical
  processor are detected; otherwise, it starts the Erlang runtime system without
  SMP support. For more information see the [erl](erl_cmd.md) man page.

  Own Id: OTP-6126

- Increased the reduction cost for sending messages in the SMP emulator so it
  behaves more like the non-SMP emulator.

  Own Id: OTP-6196

- A port running a dynamically linked-in driver that exits due to the driver
  being unloaded now exits with exit reason `driver_unloaded`. Previously the
  port exited with exit reason `-1`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6204

- Changed name of the `erlang:system_info/1` argument `scheduler` to
  `scheduler_id`. This since the `scheduler` argument so easily could be mixed
  up with the `schedulers` argument (both returning integers).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6208

- The changes below were made by Mikael Pettersson, HiPE.

  HiPE runtime system:

  Reduce overheads in the HiPE runtime system's BIF glue code.

  Fix bug when exceptions are thrown from BEAM to HiPE.

  Support SPARC on Linux.

  Support x86 on FreeBSD.

  Floating-point exceptions:

  Reduce overheads in checking results of floating-point operations.

  Minor bug fix in SSE2 floating-point exception handling.

  Support SSE2 floating-point exceptions on 32-bit x86 machines.

  Make FP exceptions work in the SMP runtime system on FreeBSD/x86.

  Support floating-point exceptions on SPARCs running Linux.

  Runtime system:

  Minor scheduler optimisation in the non-SMP runtime system.

  Substantial reduction of I/O thread overheads in the SMP runtime system if the
  separate timer thread is used. (In R11B-1, the separate timer thread is not
  used.)

  Own Id: OTP-6211

## ERTS 5.5

### Fixed Bugs and Malfunctions

- Previously [`unlink/1`](`unlink/1`) and `erlang:demonitor/2` behaved
  completely asynchronous. This had one undesirable effect, though. You could
  never know when you were guaranteed _not_ to be affected by a link that you
  had unlinked or a monitor that you had demonitored.

  The new behavior of [`unlink/1`](`unlink/1`) and `erlang:demonitor/2` can be
  viewed as two operations performed atomically. Asynchronously send an unlink
  signal or a demonitor signal, and ignore any future results of the link or
  monitor.

  _NOTE_: This change can cause some obscure code to fail which previously did
  not. For example, the following code might hang:

  ```erlang
              Mon = erlang:monitor(process, Pid),
              %% ...
              exit(Pid, bang),
              erlang:demonitor(Mon),
              receive
                  {'DOWN', Mon , process, Pid, _} -> ok
              %% We were previously guaranteed to get a down message
              %% (since we exited the process ourself), so we could
              %% in this case leave out:
              %% after 0 -> ok
              end,
  ```

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-5772

- Two bugs fixed: If the environment variable `ERL_FLAGS` was set, its contents
  would be appended to the end of the command line even if the command line had
  an `-extra` options. Changed to place the options from `ERL_FLAGS` just before
  `-extra`. Also, the `-smp` and `-hybrid` flags no longer have any effect if
  placed after `-extra`.

  Own Id: OTP-6054

### Improvements and New Features

- The documentation for writing drivers in the ERTS User's Guide has been
  expanded and updated.

  Own Id: OTP-5192

- The `andalso` and `orelse` operators are now allowed to be used in guards.
  That also applies to match specifications.

  Own Id: OTP-5894 Aux Id: OTP-5149

- There is a new trace match spec function `{exception_trace}` and a
  corresponding trace message `exception_from` that can be used to trace on any
  exit from a function, both normal function return and exception return. See
  the documentation for details.

  The `return_to` trace message is now also generated when the execution returns
  to a function due to catching an exception.

  Own Id: OTP-5956

- Erlang runtime system with SMP (symmetric multi processing) support.

  The runtime system with SMP support is in this release focused on stability
  and there are a number of steps with optimizations to follow before it will
  take full advantage of multi processor systems. The released system is however
  truly multi threaded and you will notice increased performance for many
  applications already. We recommend that you evaluate your application on the
  SMP version of the runtime system and wait for some more optimizations before
  you use it in a real product. You will then discover if there are any problems
  in your application that needs to be fixed in order for it to work properly in
  a multi threaded environment. More optimized versions of the runtime system
  with SMP support will be included in the R11B maintenance releases.

  The SMP enabled runtime system will be started if the `-smp` command line
  argument is passed to the `erl` command. In order to make use of more than one
  processor core, multiple scheduler threads are used. By default, the number of
  scheduler threads will equal the number of processor cores. The number of
  scheduler threads can be set with the `+S` command line argument. For more
  information see the [erl](erl_cmd.md) man page.

  A runtime system with SMP support is by default built on the following
  platforms if posix threads, and a gcc compiler of at least version 2.95 is
  found:

  - Linux with at least kernel version 2.6 and the Native POSIX Thread Library
    on x86, x86_64, and 32-bits PowerPC.
  - Solaris of at least version 8 on 32-bits SPARC-V9.
  - MacOSX of at least version 10.4 (Darwin 8.0) on 32-bits PowerPC.

  The runtime system with SMP support is known _not_ to build on:

  - Windows.
  - Linux with kernel versions less than 2.4, or without the Native POSIX Thread
    Library.
  - Other hardware platforms than x86, x86_64, 32-bits SPARC-V9 and 32-bits
    PowerPC.

  Windows will be supported in a future release.

  The runtime system with SMP support might build on other operating systems in
  combination with supported hardware. In order to force a build of a runtime
  system with SMP support, pass the `--enable-smp-support` command line argument
  to configure. Note, however, that it is not enough that it builds. The
  underlying thread library and operating system has to provide SMP support as
  well. If the thread library does not distribute scheduler threads over
  multiple processor cores then the runtime system will only seemingly provide
  SMP support. If the runtime system is not built by default on a specific
  platform, we have _not_ tested it on that platform.

  _NOTE_: The design of SMP support for drivers is ongoing. There will probably
  be incompatible driver changes (only affecting drivers run on the runtime
  system with SMP support) released as patches for R11B.

  _Potential incompatibility_: Previously, specific driver call-backs were
  always called from the same thread. This is _not_ true in the runtime system
  with SMP support. Calls to call-backs will be made from different threads,
  e.g., two consecutive calls to exactly the same call-back can be made from two
  different threads. This will in _most_ cases not be a problem. All calls to
  call-backs are synchronized, i.e., only one call-back will be called at a
  time.

  In the future the default behavior will _probably_ be the following: Calls to
  call-backs will, as now, be made from different threads. Calls to call-backs
  in the same driver _instance_ will be synchronized. It will probably be
  possible to configure so that all calls to call-backs in all driver instances
  of a specific driver type will be synchronized. It may be possible to
  configure so that all calls to call-backs of a driver instance or a of a
  specific driver type will be made from the same thread.

  Parallelism in the Erlang code executed is a necessity for the Erlang runtime
  system to be able to take advantage of multi-core or multi-processor hardware.
  There need to be at least as many Erlang processes runnable as processor cores
  for the Erlang runtime system to be able to take advantage of all processor
  cores.

  An Erlang runtime system with SMP support with only one Erlang process
  runnable all the time will almost always be slower than the same Erlang
  runtime system without SMP support. This is due to thread synchronization
  overhead.

  Known major bottleneck in the Erlang runtime system:

  - Currently the I/O system uses one "big lock", i.e. only one thread can do
    I/O at a time (with the exception of async threads and threads created by
    users own linked-in drivers). This is high on the list of things to
    optimize. Note, code that does not do I/O can be executed at the same time
    as one thread does I/O.

  Some pitfalls which might cause Erlang programs that work on the non-SMP
  runtime system to fail on the SMP runtime system:

  - A newly spawned process will often begin executing immediately. Code that
    expects that the parent process will be able to execute for a while before
    the child process begins executing is likely to fail.
  - High priority processes could previously provide mutual exclusion (bad
    programming style) by preventing normal and low priority processes from
    being run. High priority processes cannot be used this way to provide mutual
    exclusion.
  - `erlang:yield()` could be used to provide some kind of temporary mutual
    exclusion (also bad programming style). `erlang:yield()` cannot be used to
    provide any kind of mutual exclusion.
  - Obscure pitfall, only if a process being traced also sends normal messages
    to the tracer:  
      The order between trace messages and normal messages is undefined. I.e.
    the order between normal messages sent from a tracee to a tracer and the
    trace messages generated from the same tracee to the same tracer is
    undefined. The internal order of normal messages and the internal order of
    trace messages will, of course, be preserved as before.

  The kernel poll feature is currently not supported by the runtime system with
  SMP support. It will probably be supported in a future release.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6006 Aux Id: OTP-6095

- Linked-in driver modifications.

  - Linked-in drivers must be recompiled.
  - The `refc` field in the `ErlDrvBinary` type has been removed. The reference
    count can be accessed via API functions. For more information see the
    `erl_driver(3)` man page.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6095 Aux Id: OTP-6006

## ERTS 5.4.13

### Fixed Bugs and Malfunctions

- Large files (more than 2 GBytes) are now handled on Solaris 8.

  Own Id: OTP-5849 Aux Id: seq10157

- A failing bit syntax construction could fail with the PREVIOUS exception
  reason that had occurred in the process (instead of with `badarg`).

  Own Id: OTP-5911

- When building OTP, the Kernel application was built in both the primary and
  secondary bootstrap steps, which would cause problems if OTP including its
  bootstrap is checked into a version control system (such as CVS). (Thanks to
  Sebastian Strollo.)

  Own Id: OTP-5921

- `binary_to_term(<<131,109,255,255,255,255)` and similar expressions used to
  crash the emulator instead of causing a `badarg` exception. (Thanks to
  Matthias Lang.)

  Own Id: OTP-5933

- `erlang:hibernate/3` could sometimes crash the emulator when no heap was
  needed.

  Own Id: OTP-5940

- Execution of match specs could under rare circumstances cause the emulator to
  dump core.

  Execution of match specs could cause memory leaks in the hybrid emulator.

  Own Id: OTP-5955

- A bug in `erlang:trace_info/2` when getting info for a function in a deleted
  module resulting in an emulator crash, has been corrected.

  Own Id: OTP-5957

- Different (and old) `config.guess` files in the ERTS and Erl_Interface
  applications would cause build problems on the new Intel-based iMacs. (Thanks
  to Sebastion Strollo.)

  Own Id: OTP-5967

- pthread header and library mismatch on Linux systems (at least some SuSE and
  Debian) with both NPTL and Linuxthreads libraries installed.

  Own Id: OTP-5981

### Improvements and New Features

- The driver_set_timer did not change the previous timeout if called a second
  time. Now it works as specified, changing the timeout.

  Own Id: OTP-5942

- The undocumented `{packet,http}` option (for the `gen_tcp` module) did not
  work correctly when there were multiple continuation lines. (Thanks to Per
  Hedeland.)

  Own Id: OTP-5945

- The setuid_socket_wrap program was corrected to work for C compilers that
  treat the `char` type as unsigned. (Thanks to Magnus Henoch.)

  Own Id: OTP-5946

## ERTS 5.4.12

### Fixed Bugs and Malfunctions

- Fixed a minor build problem on Windows.

  Own Id: OTP-5819 Aux Id: OTP-5382 OTP-5540 OTP-5577

- The `delay_send` option for `gen_tcp` was broken on Windows.

  Own Id: OTP-5822

- If there were user-defined variables in the boot script, and their values were
  not provided using the `-boot_var` option, the emulator would refuse to start
  with a confusing error message. Corrected to show a clear, understandable
  message.

  The `prim_file` module was modified to not depend on the `lists` module, to
  make it possible to start the emulator using a user-defined loader. (Thanks to
  Martin Bjorklund.)

  Own Id: OTP-5828 Aux Id: seq10151

### Improvements and New Features

- The HiPE compiler identifies more leaf functions, giving slightly faster code.

  Corrected problems in HiPE's coalescing register allocating that would cause
  it to fail when compiling very large functions (e.g. some of parse modules in
  the Megaco application).

  Own Id: OTP-5853

## ERTS 5.4.11

### Fixed Bugs and Malfunctions

- Timers could sometimes timeout too early. This bug has now been fixed.

  Automatic cancellation of timers created by
  `erlang:send_after(Time,pid(),Msg)` and `erlang:start_timer(Time,pid(),Msg)`
  has been introduced. Timers created with the receiver specified by a pid, will
  automatically be cancelled when the receiver exits. For more information see
  the `m:erlang` man page.

  In order to be able to maintain a larger amount of timers without increasing
  the maintenance cost, the internal timer wheel and bif timer table have been
  enlarged.

  Also a number of minor bif timer optimizations have been implemented.

  Own Id: OTP-5795 Aux Id: OTP-5090, seq8913, seq10139, OTP-5782

- `erlang:monitor(process,Pid)` hanged if `Pid` referred to a process on a
  non-existing node with the same nodename as the nodename of node on which the
  call was made. This bug has now been fixed.

  Own Id: OTP-5827

### Improvements and New Features

- By setting Unix environment variables, the priority for the emulator can be
  lowered when it is writing crash dumps and the time allowed for finishing
  writing a crash dump can be set to a certain number of seconds. See the
  documentation for `erl` in the ERTS application. (Also, a few other previously
  undocumented environment variables are now documented.)

  Own Id: OTP-5818

- Documentation improvements:

  \- documentation for `erlang:link/1` corrected

  \- command line flag `-code_path_cache` added

  \- `erl` command line flags clarifications

  \- `m:net_kernel` clarifications

  Own Id: OTP-5847

## ERTS 5.4.10

### Fixed Bugs and Malfunctions

- \-D_GNU_SOURCE is now always passed on the compile command line on linux. This
  in order to ensure that all included system headers see \_GNU_SOURCE defined.

  \_GNU_SOURCE is now also defined on linux in configure when looking for
  features.

  Some minor (harmless) configure bugs were also fixed.

  Own Id: OTP-5749

- Some compiler warnings and Dialyzer warnings were eliminated in the Tools
  application.

  When tracing to a port (which `fprof` does), there could be fake schedule
  out/schedule in messages sent for a process that had exited.

  Own Id: OTP-5757

### Improvements and New Features

- The BIFs [`iolist_size/1`](`iolist_size/1`) and
  [`iolist_to_binary/1`](`iolist_to_binary/1`) has been added.

  The BIF [`list_to_existing_atom/1`](`list_to_existing_atom/1`) has been added.

  Minor bug fix: The exception reason could be changed to `error` inside nested
  try/catch constructs if the `erlang:raise/3` BIF was called with an empty
  stacktrace. (Calling `erlang:raise/3` with an empty stacktrace is NOT
  recommended.)

  Minor bugfix: On Windows, `file:open/2` will now return the documented error
  reason `{error,eisdir}` if the filename refers to a directory (it used to
  return `{error,eacces}`).

  The message in the documentation for `erlang:system_monitor/2`, description of
  `busy_dist_port`, was corrected.

  Own Id: OTP-5709 Aux Id: seq10100

- The previously undocumented and UNSUPPORTED `zlib` module has been updated in
  an incompatible way and many bugs have been corrected. It is now also
  documented.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-5715

- New socket options `priority` and `tos` for platforms that support them
  (currently only Linux).

  Own Id: OTP-5756

- Only the emulator is now linked with termcap library in order to decrease
  library dependencies for other otp programs.

  Own Id: OTP-5758

## ERTS 5.4.9.2

### Fixed Bugs and Malfunctions

- The native resolver has gotten an control API for extended debugging and soft
  restart. It is: `inet_gethost_native:control(Control)`  
  `Control = {debug_level,Level} | soft_restart`  
  `Level = integer() in the range 0-4`.

  Own Id: OTP-5751 Aux Id: EABln25013

## ERTS 5.4.9.1

### Improvements and New Features

- On VxWorks, epmd did not handle file descriptors with higher numbers than 63.
  Also, if epmd should get a file descriptor with a number >= FD_SETSIZE, it
  will close a the file descriptor and write a message to the log (instead of
  mysteriously fail); the Erlang node that tried to register will fail with a
  duplicate_name error (unfortunately, epmd has no way to indicate to the Erlang
  node why the register attempt failed).

  Own Id: OTP-5716 Aux Id: seq10070

## ERTS 5.4.9

### Fixed Bugs and Malfunctions

- Timezone data is now initialized better. (was a problem at least on NetBSD
  2.0.2) Thanks to Rich Neswold.

  Own Id: OTP-5621

- The hybrid-heap emulator ('erl -hybrid') is much more stable. We have
  corrected all known bugs that caused it to dump core while running our test
  suites.

  Own Id: OTP-5634

- Fixed rare memory leaks in `erlang:demonitor/1` when distributed monitors were
  removed.

  Own Id: OTP-5692

- Processes were sometimes unnecessarily garbage collected when terminating.
  These unnecessary garbage collections have now been eliminated.

  Own Id: OTP-5693

### Improvements and New Features

- The `c:i/0` function will now run in a paged mode if there are more than 100
  processes in the system. (Thanks to Ulf Wiger.)

  `erlang:system_info(process_count)` has been optimized and does now return
  exactly the same value as [`length(processes())`](`length/1`). Previously
  `erlang:system_info(process_count)` did not include exiting processes which
  are included in [`length(processes())`](`length/1`).

  The `+P` flag for `erl`, which sets the maximum number of processes allowed to
  exist at the same, no longer accepts values higher than 134217727. (You will
  still probably run out of memory before you'll be able to reach that limit.)

  Own Id: OTP-5645 Aux Id: seq9984

- The term-building driver functions `driver_output_term()` and
  `driver_send_term()` have been updated:

  The ERL_DRV_FLOAT type has been added.

  For the ERL_DRV_BINARY type, the length and offset are now validated against
  the length of the driver binary.

  The ERL_DRV_PID type is now implemented (it was documented, but not
  implemented).

  Own Id: OTP-5674

## ERTS 5.4.8

### Fixed Bugs and Malfunctions

- `ets:delete/1` now allows other Erlang process to run when a large table is
  being deleted.

  Own Id: OTP-5572

- A bug regarding tcp sockets which results in hanging `gen_tcp:send/2` has been
  corrected. To encounter this bug you needed one process that read from a
  socket, one that wrote more date than the reader read out so the sender got
  suspended, and then the reader closed the socket. (Reported and diagnosed by
  Alexey Shchepin.)

  Corrected a bug in the (undocumented and unsupported) option `{packet,http}`
  for `gen_tcp.` (Thanks to Claes Wikstrom and Luke Gorrie.)

  Updated the documentation regarding the second argument to `gen_tcp:recv/2`,
  the `Length` to receive.

  Own Id: OTP-5582 Aux Id: seq9839

- Erlang/OTP will now build on Mac OS X 10.4 "Tiger" (the problem was that 10.4
  has a partially implemented poll() function that can't handle devices). Also,
  on Mac OS X 10.3 "Panther", Erlang/OTP will now use select() instead of poll()
  (because poll() on Mac OS X 10.3 is implemented using select()).

  Own Id: OTP-5595

- A bug in the file driver when opening a file in compressed mode, and the
  returned allocated pointer from the compressing library was in very high
  memory (>= 2GB), causing e.g. `file:read/2` to return `{error,ebadf}`, has
  been corrected.

  Own Id: OTP-5618

### Improvements and New Features

- The new fun `M:F/A` construct creates a fun that refers to the latest version
  of `M:F/A.` This syntax is meant to replace tuple funs `{M,F}` which have many
  problems.

  The new type test [`is_function(Fun,A)`](`is_function/2`) (which may be used
  in guards) test whether `Fun` is a fun that can be applied with `A` arguments.
  (Currently, `Fun` can also be a tuple fun.)

  Own Id: OTP-5584

- In the HiPE application, there's a new experimental register allocator
  (optimistic coalescing), and the linear scan register allocator is now also
  available on ppc. Plus lots of cleanups.

  Minor hybrid heap corrections.

  The maximum size of a heap used to be artificially limited so that the size of
  a heap would fit in 28 bits; that limitation could cause the emulator to
  terminate in a garbage collection even if there still was available memory.
  Now the largest heap size for a 32 bit CPU is 1,699,221,830 bytes. (Thanks to
  Jesper Wilhelmsson.)

  Also removed the undocumented `+H` emulator option.

  Own Id: OTP-5596

## ERTS 5.4.7

### Fixed Bugs and Malfunctions

- `to_erl` could close unexpectedly on Linux systems. Also, output from the
  emulator could be lost. Corrected.

  Own Id: OTP-5561

### Improvements and New Features

- The `cpu_timestamp` option for `erlang:trace/3` is now also supported on
  Linux.

  Own Id: OTP-5532 Aux Id: seq9813

- The last known `werl` window size/position is now saved correctly when `werl`
  is stopped with the window minimized. A problem with the placement not being
  saved if the emulator is halted or stopped from the JCL menu has also been
  fixed.

  Own Id: OTP-5544 Aux Id: OTP-5522

## ERTS 5.4.6

### Fixed Bugs and Malfunctions

- Some math libraries do not always throw floating-point exceptions on errors.
  In order to be able to use these libraries, floating-point errors are now more
  thoroughly checked.

  Misc floating-point fixes for Linux and MacOSX.

  Own Id: OTP-5467

- An internal buffer was sometimes not cleared which caused garbage to appear in
  error messages sent to the error logger.

  `errno` was sometimes clobbered which caused erroneous error reports about
  `poll()` errors.

  Only emulators on unix platforms were affected by these bugs.

  Own Id: OTP-5492

- The ethread library now works on OpenBSD.

  Own Id: OTP-5515

- Corrected a bug in the (undocumented and unsupported) option `{packet,http}`
  for `gen_tcp`. (Thanks to Claes Wikstrom and Luke Gorrie.)

  Own Id: OTP-5519

### Improvements and New Features

- [`binary_to_term/1`](`binary_to_term/1`) could cause the emulator to crash
  when given invalid pids or funs.

  Own Id: OTP-5484 Aux Id: seq9801

- Some more stability problems were fixed in the hybrid-heap emulator.

  Own Id: OTP-5489

- After `werl` was closed with the window minimized, it was not possible to
  restart `werl` with an open window. A temporary solution has so far been
  implemented that restores the initial window settings every time `werl` is
  started.

  Own Id: OTP-5522

## ERTS 5.4.5

### Fixed Bugs and Malfunctions

- If a process had node links (created by [`monitor_node/2`](`monitor_node/2`)),
  executing [`process_info(Pid,memory)`](`process_info/2`) for that process
  would crash the emulator.

  Own Id: OTP-5420

- Minor corrections to the help text printed by `erlc -help`. The documentation
  for `erlc` was also slightly updated.

  Own Id: OTP-5428

- 32-bit words were used for offsets in the garbage collector. This caused the
  emulator to crash on 64-bit machines when heaps were moved more than 4 GB
  during garbage collection.

  Own Id: OTP-5430

- [`is_boolean(42.5)`](`is_boolean/1`) failed to load if optimization was
  explicitly turned off.

  Own Id: OTP-5448

- If there was a call to `Module:foo/X` from any loaded module, the returned by
  `M:module_info(exports)` would always include `{foo,X}` (even though
  `Module:foo/X` if was not defined).

  Own Id: OTP-5450 Aux Id: seq9722

### Improvements and New Features

- The `c` option for the `+B` flag has been introduced which makes it possible
  to use Ctrl-C (Ctrl-Break on Windows) to interrupt the shell process rather
  than to invoke the emulator break handler. All new `+B` options are also
  supported on Windows (`werl`) as of now. Furthermore, Ctrl-C on Windows has
  now been reserved for copying text (what Ctrl-Ins was used for previously).
  Ctrl-Break should be used for break handling. Lastly, the documentation of the
  system flags has been updated.

  Own Id: OTP-5388

## ERTS 5.4.4

### Fixed Bugs and Malfunctions

- The function `erlang:float/1` can now be used in match specifications and is
  recognized by `dbg:fun2ms/1` and `ets:fun2ms/1`. This addition is part of the
  work to "harmonize" match specification guards with Erlang guards.

  Own Id: OTP-5297 Aux Id: OTP-4927

- The [`register/2`](`register/2`) BIF would return `true` even if the second
  argument was not a pid for living process. Corrected to cause an exception.

  Own Id: OTP-5324 Aux Id: seq9698

- In the 'bag' type of ets tables, elements having the same key were supposed to
  be order in insertion order. The would be wrong if a rehash occurred.

  Own Id: OTP-5340 Aux Id: seq9704

- Linked in drivers in the Crypto and Asn1 applications are now compiled with
  the `-D_THREAD_SAFE` and `-D_REENTRANT` switches on unix when the emulator has
  thread support enabled.

  Linked in drivers on MacOSX are not compiled with the undocumented
  `-lbundle1.o` switch anymore. Thanks to Sean Hinde who sent us a patch.

  Linked in driver in Crypto, and port programs in SSL, now compiles on OSF1.

  Minor makefile improvements in Runtime_Tools.

  Own Id: OTP-5346

- Fixed a bug in the hybrid heap in distributed send operations.

  Own Id: OTP-5361

- A BIF `erlang:raise/3` has been added. See the manual for details. It is
  intended for internal system programming only, advanced error handling.

  Own Id: OTP-5376 Aux Id: OTP-5257

- Mikael Pettersson (HiPE) corrected a few bugs in the emulator that caused
  problems when compiled with the experimental gcc-4.0.0.

  Own Id: OTP-5386

### Improvements and New Features

- Minor update of the internal documentation of the `epmd` protocol.

  The listen port of `epmd` has now been registered at IANA:
  [http://www.iana.org/assignments/port-numbers](http://www.iana.org/assignments/port-numbers).

  Own Id: OTP-5234

- `run_erl.c` now works on Mac OS X and FreeBSD.

  Own Id: OTP-5384

- A few bugs were corrected in the HiPE application.

  Own Id: OTP-5385

## ERTS 5.4.3

### Fixed Bugs and Malfunctions

- Font and color preferences for `werl.exe` now can be saved even after the
  first time you run `werl.exe`. The window position and size is also saved.
  Patch from James Hague who did all the hard work.

  Own Id: OTP-5250

- OTP archive libraries, e.g. the `erl_interface` library, on MacOSX could not
  be used without first rerunning `ranlib` on them. This bug has now been fixed.

  Own Id: OTP-5274

- Bugs in `erlang:hash/2` and `erlang:phash/2` on 64-bit platforms have been
  fixed.

  Own Id: OTP-5292

### Improvements and New Features

- Corrected a crash in the 64-bits emulator.

  Corrected a problem in the hybrid heap emulator.

  In the chapter about the abstract format in the ERTS User's Guide, updated the
  last section about how the abstract format is stored in BEAM files.

  Own Id: OTP-5262

- The maximum number of concurrent threads in the internal ethread thread
  package has been limited to 2048.

  Own Id: OTP-5280

## ERTS 5.4.2.1

### Improvements and New Features

- If Erlang/OTP was installed in a short directory name, such as
  `C:\Program\erl5.4.2`, the emulator would not start.

  Own Id: OTP-5254

## ERTS 5.4.2

### Fixed Bugs and Malfunctions

- If one used `select/3` and `select/1` on a non-fixed ETS table and deleted
  objects simultaneously, the emulator could crash. Note that the result of such
  simultaneous operations on tables that are not in a fixed state is still
  undefined, but the emulator crash is, needless to say, fixed.

  Own Id: OTP-5209 Aux Id: seq9198

- Arithmetic with big numbers could crash the emulator.

  The HiPE native code compiler and run-time code in the emulator has been
  updated. (Note: Native code is still not supported.)

  Eliminated a few bugs that could crash the hybrid emulator (which is not
  supported).

  Own Id: OTP-5233 Aux Id: seq9587

## ERTS 5.4.1

### Fixed Bugs and Malfunctions

- The ethread library was unnecessarily rebuilt multiple times during the build
  process, also a debug version of the library was build during the install
  phase. These unnecessary builds have now been removed. Note, the content of
  the installed Erlang/OTP system is not effected at all by this change.

  Own Id: OTP-5203

- The emulator could fail to clear the memory segment cache. This could
  potentially cause memory allocation to unnecessarily fail when memory usage
  was close to its maximum. This bug has now been fixed.

  Own Id: OTP-5211

- std_alloc (std short for standard) was sometimes called def_alloc (def short
  for default). std_alloc is now everywhere referred to as std_alloc.

  Own Id: OTP-5216

- A documentation bug has been corrected in the `erts_alloc(3)` documentation.
  It was stated that some of the memory allocators present were by default
  disabled. This is true for Erlang/OTP R9C, but is not true for Erlang/OTP
  R10B. In R10B all memory allocators present are enabled by default.

  Own Id: OTP-5217

### Improvements and New Features

- The emulator now close all open files and sockets immediately after receiving
  an USR1 signal. This causes the emulator to unregister at `epmd` as early as
  possible.

  Own Id: OTP-5221 Aux Id: OTP-4985, seq9514

- Try/catch support in the emulator slightly updated.

  Own Id: OTP-5229
