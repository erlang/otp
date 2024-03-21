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
# OS_Mon Release Notes

This document describes the changes made to the OS_Mon application.

## Os_Mon 2.9.1

### Fixed Bugs and Malfunctions

- Fixed some benign compile warnings on Windows.

  Own Id: OTP-18895

## Os_Mon 2.9

### Fixed Bugs and Malfunctions

- Fix internal os_mon_sysinfo:get_disk_info/1 function to not crash when run on
  Windows with multiple drives.

  Own Id: OTP-18246 Aux Id: PR-6284 GH-6156

- Fixed a memory leak when calling `cpu_sup:util/0,1` on FreeBSD.

  Own Id: OTP-18546 Aux Id: GH-7070, PR-7071

### Improvements and New Features

- The `disksup:get_disk_info/0` and `disksup:get_disk_info/1` functions have
  been introduced. These can be used in order to immediately fetch information
  about current disk usage.

  Own Id: OTP-18303 Aux Id: PR-6384

- Runtime dependencies have been updated.

  Own Id: OTP-18350

- Support for `cpu_sup:util()` on OpenBSD.

  Own Id: OTP-18566 Aux Id: PR-7080

## Os_Mon 2.8.2

### Fixed Bugs and Malfunctions

- Avoid error report from failing `erlang:port_close` at shutdown of `cpu_sup`
  and `memsup`. Bug exists since OTP 25.3 (os_mon-2.8.1).

  Own Id: OTP-18559 Aux Id: ERIERL-942

## Os_Mon 2.8.1

### Fixed Bugs and Malfunctions

- The port programs used by `cpu_sup` and `memsup` are now gracefully shut down
  when `cpu_sup` and `memsup` are shut down.

  Own Id: OTP-18469 Aux Id: PR-6689

## Os_Mon 2.8

### Improvements and New Features

- The [`disk_space_check_interval`](`m:disksup#config`) configuration parameter
  of `disksup` can now be set to values smaller than a minute.

  Own Id: OTP-18304 Aux Id: PR-6385

## Os_Mon 2.7.1

### Fixed Bugs and Malfunctions

- On a Mac with Apple Silicon, `memsup:get_os_wordsize/0` would return 32
  instead of 64.

  Own Id: OTP-17441

## Os_Mon 2.7

### Improvements and New Features

- The temporarily introduced configuration parameter
  `memsup_improved_system_memory_data` has been removed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16943 Aux Id: OTP-16906

- Fix `disk_sup` to also search the system PATH on linux when looking for the
  `df` program.

  Own Id: OTP-16944 Aux Id: PR-2787

## Os_Mon 2.6.1

### Improvements and New Features

- The configuration parameter `memsup_improved_system_memory_data` has been
  introduced. It can be used to modify the result returned by
  `memsup:get_system_memory_data()`. For more information see the `memsup`
  documentation.

  Note that the configuration parameter is intended to be removed in OTP 24 and
  the modified result is intended to be used as of OTP 24.

  Own Id: OTP-16906 Aux Id: ERIERL-532

## Os_Mon 2.6

### Fixed Bugs and Malfunctions

- `memsup` now returns the correct amount of system memory on macOS.

  Own Id: OTP-16798 Aux Id: ERL-1327

### Improvements and New Features

- Fix memsup:get_os_wordsize/0 to return the current size on aarch64.

  Own Id: OTP-16742

## Os_Mon 2.5.2

### Fixed Bugs and Malfunctions

- Fix various compiler warnings on 64-bit Windows.

  Own Id: OTP-15800

### Improvements and New Features

- Refactored the internal handling of deprecated and removed functions.

  Own Id: OTP-16469

## Os_Mon 2.5.1.1

### Improvements and New Features

- The configuration parameter `memsup_improved_system_memory_data` has been
  introduced. It can be used to modify the result returned by
  `memsup:get_system_memory_data()`. For more information see the `memsup`
  documentation.

  Note that the configuration parameter is intended to be removed in OTP 24 and
  the modified result is intended to be used as of OTP 24.

  Own Id: OTP-16906 Aux Id: ERIERL-532

## Os_Mon 2.5.1

### Fixed Bugs and Malfunctions

- Fix `disk_sup` to ignore squashfs on Linux when determining if a mounted
  filesystem is full or not.

  Own Id: OTP-15778

- Fix bug where `cpu_sup:util()` always returned 100% on systems not using gnu
  libc, for example Alpine OS.

  Own Id: OTP-15974 Aux Id: ERL-1012

## Os_Mon 2.5

### Fixed Bugs and Malfunctions

- Fix typespec of `cpu_sup:util()`.

  Own Id: OTP-15770 Aux Id: PR-2208

### Improvements and New Features

- The application otp_mibs has been removed from OTP. Some of its components
  (mibs) have been moved to other apps (snmp), or removed completely (os_mon).

  Own Id: OTP-14984 Aux Id: OTP-15329

## Os_Mon 2.4.7

### Fixed Bugs and Malfunctions

- Due to `/proc` restrictions in newer Android releases enforced by SELinux,
  cpu_sup is fixed so that it gets some basic CPU stats using the `sysinfo`
  syscall rather than reading `/proc/loadavg`.

  Own Id: OTP-15387 Aux Id: PR-1966

## Os_Mon 2.4.6

### Fixed Bugs and Malfunctions

- Improved documentation.

  Own Id: OTP-15190

## Os_Mon 2.4.5

### Fixed Bugs and Malfunctions

- Fix `disksup` to handle mount paths with spaces in them.

  Own Id: OTP-14513

## Os_Mon 2.4.4

### Fixed Bugs and Malfunctions

- Removed all old unused files in the documentation.

  Own Id: OTP-14475 Aux Id: ERL-409, PR-1493

## Os_Mon 2.4.3

### Fixed Bugs and Malfunctions

- On macOS 10.13 (High Sierra), disksup could not grab information for any disks
  that used the new APFS file system. That has been corrected.

  Own Id: OTP-14560 Aux Id: ERL-461

## Os_Mon 2.4.2

### Improvements and New Features

- Support s390x in os_mon.

  Own Id: OTP-14161 Aux Id: PR-1309

## Os_Mon 2.4.1

### Fixed Bugs and Malfunctions

- Fix type specification for cpu_sup:util/1

  Own Id: OTP-13526 Aux Id: PR-1029

- Fix strict compilation on SUN/SPARC

  Own Id: OTP-13548 Aux Id: PR-1046

- Implement cpu_sup:util/0,1 on Mac OSX

  Own Id: OTP-13597 Aux Id: PR-1049

- Fix memsup:get_os_wordsize() on 64-bit FreeBSD and 64-bit Linux PPC

  Own Id: OTP-13601 Aux Id: PR-1039

## Os_Mon 2.4

### Improvements and New Features

- cpu_sup should use native sysctl/libkvm calls on BSD

  This avoids forking off with os:cmd every time we just want to collect the
  load averages. riak does this every second, which results in a lot of
  unnecessary load.

  Own Id: OTP-12730

- Implement native cpu_sup:util/0,1 for FreeBSD

  Own Id: OTP-12796

## Os_Mon 2.3.1

### Fixed Bugs and Malfunctions

- Do not crash with badmatch when integer part of loadavg has more than 2
  digits.

  Own Id: OTP-12581

### Improvements and New Features

- Fix compilation of memsup on OpenBSD.

  Own Id: OTP-12404

## Os_Mon 2.3

### Improvements and New Features

- Adds a new application parameter 'disksup_posix_only', to make diskup use only
  options defined in the POSIX standard.

  Own Id: OTP-12053

## Os_Mon 2.2.15

### Fixed Bugs and Malfunctions

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

### Improvements and New Features

- Calls to erlang:open_port/2 with 'spawn' are updated to handle space in the
  command path.

  Own Id: OTP-10842

## Os_Mon 2.2.14

### Fixed Bugs and Malfunctions

- Fix incorrect reporting of memory on OS X via memsup. Thanks to Christopher
  Meiklejohn.

  Own Id: OTP-11454

## Os_Mon 2.2.13

### Fixed Bugs and Malfunctions

- Use 'df -k -l' to query FreeBSD and OpenBSD about diskspace on local disks.
  Previously 'df' -k -t ufs' was used but this will not handle zfs or other
  disks. Just use '-l' instead of listing potential filesystems.

  Own Id: OTP-11207

### Improvements and New Features

- Fix compilation on Solaris. Thanks to Maciej Malecki.

  Own Id: OTP-11213

- Fix broken cpu_sup:nprocs and others on Solaris 64-bit. Thanks to Simon
  Cornish.

  Own Id: OTP-11298

## Os_Mon 2.2.12

### Fixed Bugs and Malfunctions

- Compilation fixes for NetBSD. Thanks to YAMAMOTO Takashi.

  Own Id: OTP-10941

### Improvements and New Features

- Fixed disksup:get_disk_data for SUSv3, specifically OS X ML. Thanks to Sriram
  Melkote.

  Own Id: OTP-10945

## Os_Mon 2.2.11

### Fixed Bugs and Malfunctions

- Removed deprecated function calls to snmp

  Own Id: OTP-10448

## Os_Mon 2.2.10

### Fixed Bugs and Malfunctions

- Infinity timeout added to internal calls in disksup to allow it to work
  properly under very heavy load.

  Own Id: OTP-10100

- Clarify error messages from os_mon port programs

  Own Id: OTP-10161

## Os_Mon 2.2.9

### Fixed Bugs and Malfunctions

- Fix segmentation fault in memsup

  when /proc/meminfo does not include information about buffers/cache (for
  instance inside OpenVZ container) (Thanks to Anton Vorobev)

  Own Id: OTP-9913

## Os_Mon 2.2.8

### Improvements and New Features

- Erlang/OTP can now be built using parallel make if you limit the number of
  jobs, for instance using '`make -j6`' or '`make -j10`'. '`make -j`' does not
  work at the moment because of some missing dependencies.

  Own Id: OTP-9451

- Tuple funs (a two-element tuple with a module name and a function) are now
  officially deprecated and will be removed in R16. Use '`fun M:F/A`' instead.
  To make you aware that your system uses tuple funs, the very first time a
  tuple fun is applied, a warning will be sent to the error logger.

  Own Id: OTP-9649

## Os_Mon 2.2.7

### Fixed Bugs and Malfunctions

- Remove misc. compiler warnings

  Own Id: OTP-9542

## Os_Mon 2.2.6

### Fixed Bugs and Malfunctions

- Add NetBSD support to memsup and disksup (Thanks to Andrew Thompson)

  Own Id: OTP-9216

- Add support for DragonFlyBSD to memsup

  DragonFly was partially supported by os_mon already but when trying to start
  the os_mon application it'd crash with an error about an unknown operating
  system in memsup. This patch changes memsup to use the FreeBSD sysctl method
  to get memory information when on DragonFly. (Thanks to Andrew Thompson )

  Own Id: OTP-9217

## Os_Mon 2.2.5

### Improvements and New Features

- Cleanups suggested by tidier and modernization of types and specs.

  Own Id: OTP-8455

## Os_Mon 2.2.4

### Fixed Bugs and Malfunctions

- Memsup did not read memory correctly on macOS X Snowleopard. This has now been
  corrected. (Thanks to Joel Reymont)

  Own Id: OTP-8211

- Removed unused code in `cpu_sup.erl`.

  Own Id: OTP-8226

### Improvements and New Features

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8201

## Os_Mon 2.2.3

### Fixed Bugs and Malfunctions

- A missing define in `memsup.c` caused a build error on IRIX machines. This has
  now been fixed.

  Own Id: OTP-8094

## Os_Mon 2.2.2

### Fixed Bugs and Malfunctions

- disksup:get_disk_data/0 returned disk volume in bytes instead of kbytes as
  stated in the documentation. The problem occurred on Windows only and is now
  corrected.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7741

## Os_Mon 2.2.1

### Fixed Bugs and Malfunctions

- An error in `memsup` could cause `os_mon` to report erroneous memory values on
  windows for ranges of memory between 2GB and 4GB. This have now been fixed.

  Own Id: OTP-7944

### Improvements and New Features

- Added support for dragonfly OS.

  Own Id: OTP-7938

## Os_Mon 2.2

### Improvements and New Features

- The copyright notices have been updated.

  Own Id: OTP-7851

## Os_Mon 2.1.8

### Fixed Bugs and Malfunctions

- A problem with `OTP-OS-MON-MIB.mib` for 64-bit environments has now been
  fixed. The mib has been extended with 64-bit memory retrieval counterparts.

  In addition, a new function `get_os_wordsize/0` has been added in the `memsup`
  module

  Own Id: OTP-7441

- An error in `memsup.c` caused the compilation to crash on bsd environments.
  This has now been fixed.

  Own Id: OTP-7558

## Os_Mon 2.1.7

### Fixed Bugs and Malfunctions

- Fixed a build error that occurred on NetBSD (Thanks to Per Hedeland and
  Raphael Langerhorst)

  Own Id: OTP-7505

### Improvements and New Features

- Memory information in `memsup:get_system_memory_data/0` now has additional
  entries in its property list for Linux.

  Own Id: OTP-7409 Aux Id: seq10984

## Os_Mon 2.1.6

### Fixed Bugs and Malfunctions

- System information retrieval on darwin platforms with environments locales not
  conforming to the C locale caused an error in `cpu_sup` resulting in process
  termination.

  Own Id: OTP-7320

## Os_Mon 2.1.5

### Improvements and New Features

- CPU utilization, on linux, is now measured via a port program instead of
  os:cmd in erlang. This should enhance performance.

  Own Id: OTP-7108 Aux Id: OTP-6935

## Os_Mon 2.1.3

### Improvements and New Features

- Extended memsup memory probing on Linux to use a port program to probe memory
  usage. This is faster than the previous implementation.

  Own Id: OTP-6860 Aux Id: seq10616

## Os_Mon 2.1.2.1

### Improvements and New Features

- Minor Makefile changes.

  Own Id: OTP-6689

## OS_Mon 2.1.2

### Fixed Bugs and Malfunctions

- When the `memsup_system_only` flag was set to `true`, a `badmatch` exception
  occurred in the function `os_mon_mib:get_load/1`.

  Own Id: OTP-6351 Aux Id: seq10517

## OS_Mon 2.1.1

### Fixed Bugs and Malfunctions

- Did not build on Mac OS X.

  Added support for IRIX. (Thanks to Michel Urvoy and Daniel Solaz.)

  Own Id: OTP-6136

### Improvements and New Features

- `disksup`: Now using [`round(T*100)`](`round/1`) instead of
  [`trunc(T*100)`](`trunc/1`) when setting the threshold value given a float
  `T`.

  Own Id: OTP-6153

## Os_Mon 2.1

### Fixed Bugs and Malfunctions

- In 2.0, a call to `alarm_handler:get_alarms/0` was introduced in `memsup` and
  `disksup`. This will lead to problems if the default `alarm_handler` event
  handler is not used, however, and the call has now been removed. (Thanks to
  Serge Aleynikov for pointing this out.)

  Own Id: OTP-6029

- A bug that in rare cases caused `cpu_sup` to crash has been corrected.

  Own Id: OTP-6102 Aux Id: seq10312

## OS_Mon 2.0

A note on backwards compatibility: The behaviour of OS_Mon 2.0 is backwards
compatible under normal operation, but has changed somewhat in error situations:
The services do not terminate and the API functions do not raise exceptions in
all cases where they did before. Also, in the case where a service does
terminate, the exit reason may be different. See below for details.

### Fixed Bugs and Malfunctions

- A call to a OS_Mon service (cpu_sup, disksup, ...) when OS_Mon is not running,
  or when the service is not available for the OS, or when the service is not
  started, will no longer result in an exception. Instead a warning is issued
  via error_logger and a dummy value is returned, which one is specified in the
  man pages for the respective service.

  The reason is that it should not be necessary for a service to be started on
  each and every node of a distributed Erlang system for the OS-MON-MIB and
  other OS_Mon users to work properly.

  Own Id: OTP-4332 Aux Id: seq7358

- References to the obsolete EVA application in OTP-OS-MON-MIB has been removed.

  Own Id: OTP-5699

- Setting the option `memsup_system_only` to `true` did not work, but would
  crash the `memsup` process.

  Own Id: OTP-5890 Aux Id: seq10185

- `cpu_sup:nprocs/0` returned 0 on FreeBsd.

  Own Id: OTP-5901

- If the OS_Mon service `disksup` or `memsup` was restarted, the same alarm
  could be set twice. Also, set alarms were not cleared when the application was
  stopped.

  Own Id: OTP-5910

### Improvements and New Features

- Thresholds and time intervals in `disksup` and `memsup` are now configurable
  in run-time.

  Own Id: OTP-4246 Aux Id: Seq7230

- `memsup` can now handle systems with more than 4GB of RAM.

  Own Id: OTP-5800 Aux Id: seq10130

- The entire OS_Mon application (code and documentation) has been reviewed and
  consequently updated with the goal to make the application more robust,
  homogeneous and easier to configure.

  The behaviour under normal operation is backwards compatible. However,
  recoverable errors now no longer terminate the affected service (and thus
  possible the entire application), instead `error_logger` is used to warn the
  user if/when such errors occurs. Also, in the case of unrecoverable errors,
  the services have been made more homogeneous with respect to behavior and exit
  reasons. See below for more information and refer to the man pages for
  details.

  Port handling has been unified, meaning that if a port program sends garbage
  or unexpectedly dies, this is now handled the same way by all OS_Mon services,
  namely immediate process termination with the exit reason
  `{port_error,Garbage}` or `{port_died,Reason}`, respectively.

  Application configuration parameter handling has been unified. Bad parameter
  values are no longer silently ignored (`disksup`) or cause application
  termination (`memsup`, `os_sup`). Instead a warning is issued and the default
  value for the parameter is used. Also, some cases where a bad parameter value
  accidentally could be accepted have been corrected.

  Message handling has been unified. Unknown (`gen_server-`) calls cause process
  termination, whereas unknown casts and messages are now ignored by all OS_Mon
  services.

  Own Id: OTP-5897

- The following changes have been made to the `os_sup` service:

  It is now available on Windows, using `nteventlog` as backend.

  On Solaris, enabling the service (that is, installing a new configuration file
  for `syslogd` etc.) can now be done outside the `os_sup` process. The reason
  for this is that the Erlang emulator should normally not be run with `root`
  privileges, as is otherwise required. The new application configuration
  parameter `os_sup_config` must be set to `false`.

  Also, `os_sup` can now be configured using a new configuration parameter
  `os_sup_mfa` to call an arbitrary Erlang function when a message is received
  from the OS.

  Own Id: OTP-5925

- The `memsup` service has been rewritten, replacing the supervised
  `memsup_helper` with a linked help process. This gives the `memsup` process
  more control and prevents a situation where it gets out of synch with the
  received memory data and thus possibly returns erroneous results.

  Own Id: OTP-5927

## OS_Mon 1.8.1

### Fixed Bugs and Malfunctions

- `cpu_sup:util/0` failed with error reason `negative_diff` when called the
  first time on a machine (hw) that had been up for a very long time.

  Own Id: OTP-5869 Aux Id: seq10166

## OS_Mon 1.8

### Improvements and New Features

- The memsup part of the OS_Mon application has been made more stable. If there
  are (possibly temporary) problems collecting memory data, the interface
  functions (`get_memory_data/0`, `get_system_memory_data/0`) now do not fail,
  but return the previously collected value, if any, or a dummy value otherwise.
  Also, a warning message is printed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-5798

## OS_Mon 1.7.4

### Fixed Bugs and Malfunctions

- Corrected several problems in the error handling/error recovery (especially
  when OS_Mon is starting up).

  Own Id: OTP-5559

## OS_Mon 1.7.3

### Improvements and New Features

- `memsup.c` will now compile on OpenBSD. (Thanks to Geoff White and Jay
  Nelson.)

  The `disksup` and `cpu_sup` modules now work on Mac OS X (tested on Mac OS
  10.3.8).

  The `memsup` module should now work on Linux 2.6.\* as well as on older
  Linuxes. (`/proc/meminfo` has slightly different formats in different releases
  of Linux.)

  Own Id: OTP-5421  
  Aux Id: OTP-5194, OTP-5228, OTP-5291

## OS_Mon 1.7.2

This version is identical with 1.7.
