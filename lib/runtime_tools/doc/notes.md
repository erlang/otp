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
# Runtime_Tools Release Notes

This document describes the changes made to the Runtime_Tools application.

## Runtime_Tools 2.0.1

### Fixed Bugs and Malfunctions

- Fixed issue with fetching port information for observer could crash if port
  had died.

  Own Id: OTP-18868 Aux Id: GH-7735

- Fixed some benign compile warnings on Windows.

  Own Id: OTP-18895

## Runtime_Tools 2.0

### Fixed Bugs and Malfunctions

- Fixed the type specification for `instrument:carriers/0,1`

  Own Id: OTP-18499 Aux Id: PR-6946

### Improvements and New Features

- Add `dbg:tracer(file, Filename)` as a convenient way to trace to a file in
  clean text.

  Own Id: OTP-18211 Aux Id: PR-6143

- Handling of `on_load` modules during boot has been improved by adding an extra
  step in the boot order for embedded mode that runs all `on_load` handlers,
  instead of relying on explicit invocation of them, later, when the kernel
  supervision tree starts.

  This is mostly a code improvement and OTP internal simplification to avoid
  future bugs and to simplify code maintenance.

  Own Id: OTP-18447

- Deprecates `dbg:stop_clear/0` because it is simply a function alias to
  `dbg:stop/0`

  Own Id: OTP-18478 Aux Id: GH-6903

- The `instrument` module has been moved from `tools` to `runtime_tools`.

  Own Id: OTP-18487 Aux Id: PR-6829

- Removed the experimental `erts_alloc_config` module. It no longer produced
  good configurations and cannot be fixed in a reasonably backwards compatible
  manner. It has since OTP 25 been deprecated and scheduled for removal in
  OTP 26.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18549 Aux Id: PR-7105

## Runtime_Tools 1.19

### Fixed Bugs and Malfunctions

- Reading port socket options on macOS and Windows "skips" invalid options.

  Own Id: OTP-18012 Aux Id: #5798

### Improvements and New Features

- `dbg:stop/0` now behaves like `dbg:stop_clear/0`, clearing all global trace
  patterns for all functions.

  Own Id: OTP-17909 Aux Id: ERIERL-760

- `erts_alloc_config` has been scheduled for removal in OTP 26. It has not
  produced good configurations for a very long time, and unfortunately it cannot
  be fixed in a backwards compatible manner.

  Own Id: OTP-17939

## Runtime_Tools 1.18

### Fixed Bugs and Malfunctions

- Fixed bug in `scheduler:utilization(Seconds)` that would leave the
  `scheduler_wall_time` system flag incorrectly enabled.

  Own Id: OTP-17800 Aux Id: PR-5425

### Improvements and New Features

- Add `scheduler:get_sample/0` and `get_sample_all/0`. Also clarify `scheduler`
  module documentation about how it depends on system flag
  `scheduler_wall_time`.

  Own Id: OTP-17830 Aux Id: GH-5425, PR-5444

## Runtime_Tools 1.17

### Improvements and New Features

- Observer now has a sectiion for new socket.

  Own Id: OTP-17346

- The dbg docs have been expanded to include the meaning of all the function
  name acronyms.

  Own Id: OTP-17572 Aux Id: PR-5117

## Runtime_Tools 1.16.2

### Improvements and New Features

- The experimental HiPE application has been removed, together with all related
  functionality in other applications.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16963

## Runtime_Tools 1.16.1

### Fixed Bugs and Malfunctions

- The function `dbg:n/1` used a local fun to set up a tracer on a remote node.
  This works fine as long as the remote node is running exactly the same version
  of Erlang/OTP but does not work at all otherwise. This is fixed by exporting
  the relevant function and by calling this function on the remote node to set
  up remote tracing.

  Own Id: OTP-16930 Aux Id: ERL-1371, GH-4396

## Runtime_Tools 1.16

### Improvements and New Features

- Clarify documentation of module 'scheduler'.

  Own Id: OTP-17208 Aux Id: GH-4502, PR-4532

## Runtime_Tools 1.15.1

### Fixed Bugs and Malfunctions

- Fixed a crash in `appmon_info` triggered by trying to read port info from a
  port that was in the process of terminating.

  `appmon_info` is used by `observer` to get information from the observed node.

  Own Id: OTP-16787 Aux Id: PR-2673

## Runtime_Tools 1.15

### Improvements and New Features

- Improved the presentation of allocations and carriers in the `instrument`
  module.

  Own Id: OTP-16327

- Minor updates due to the new spawn improvements made.

  Own Id: OTP-16368 Aux Id: OTP-15251

## Runtime_Tools 1.14.0.1

### Fixed Bugs and Malfunctions

- The function `dbg:n/1` used a local fun to set up a tracer on a remote node.
  This works fine as long as the remote node is running exactly the same version
  of Erlang/OTP but does not work at all otherwise. This is fixed by exporting
  the relevant function and by calling this function on the remote node to set
  up remote tracing.

  Own Id: OTP-16930 Aux Id: ERL-1371, GH-4396

## Runtime_Tools 1.14

### Improvements and New Features

- Fix `dbg:stop_clear/0` to also clear trace events (`send` and `'receive'`).

  Own Id: OTP-16044

## Runtime_Tools 1.13.3

### Improvements and New Features

- Minor updates to build system necessary due to move of configuration of
  `crypto` application.

  Own Id: OTP-15262 Aux Id: OTP-15129

## Runtime_Tools 1.13.2.1

### Fixed Bugs and Malfunctions

- The function `dbg:n/1` used a local fun to set up a tracer on a remote node.
  This works fine as long as the remote node is running exactly the same version
  of Erlang/OTP but does not work at all otherwise. This is fixed by exporting
  the relevant function and by calling this function on the remote node to set
  up remote tracing.

  Own Id: OTP-16930 Aux Id: ERL-1371, GH-4396

## Runtime_Tools 1.13.2

### Improvements and New Features

- Update of systemtap trace example scripts.

  Own Id: OTP-15670

## Runtime_Tools 1.13.1

### Improvements and New Features

- Optimize `observer` by using new `system_info(ets_count)` instead of more
  expensive [`length(ets:all())`](`length/1`).

  Own Id: OTP-15163 Aux Id: PR-1844

## Runtime_Tools 1.13

### Improvements and New Features

- New utility module `scheduler` which makes it easier to measure scheduler
  utilization.

  Own Id: OTP-14904

## Runtime_Tools 1.12.5

### Fixed Bugs and Malfunctions

- `system_information:to_file/1` will now use slightly less memory.

  Own Id: OTP-14816

## Runtime_Tools 1.12.4

### Improvements and New Features

- New family of `erts_alloc` strategies: Age Order First Fit. Similar to
  "address order", but instead the oldest possible carrier is always chosen for
  allocation.

  Own Id: OTP-14917 Aux Id: ERIERL-88

## Runtime_Tools 1.12.3

### Fixed Bugs and Malfunctions

- Removed all old unused files in the documentation.

  Own Id: OTP-14475 Aux Id: ERL-409, PR-1493

## Runtime_Tools 1.12.2

### Improvements and New Features

- General Unicode improvements.

  Own Id: OTP-14462

## Runtime_Tools 1.12.1

### Fixed Bugs and Malfunctions

- A faulty encoding comment was added when saving trace patterns to file. This
  is now corrected.

  Own Id: OTP-14479

## Runtime_Tools 1.12

### Fixed Bugs and Malfunctions

- Add compile option `-compile(no_native)` in modules with `on_load` directive
  which is not yet supported by HiPE.

  Own Id: OTP-14316 Aux Id: PR-1390

### Improvements and New Features

- Miscellaneous updates due to atoms containing arbitrary Unicode characters.

  Own Id: OTP-14285

- Sockets can now be bound to device (SO_BINDTODEVICE) on platforms where it is
  supported.

  This has been implemented e.g to support VRF-Lite under Linux; see
  [VRF ](https://www.kernel.org/doc/Documentation/networking/vrf.txt), and
  GitHub pull request [\#1326](https://github.com/erlang/otp/pull/1326).

  Own Id: OTP-14357 Aux Id: PR-1326

## Runtime_Tools 1.11.1

### Fixed Bugs and Malfunctions

- etop erroneously reported the average scheduler utilization since the tool was
  first started instead of the scheduler utilization since last update. This is
  now corrected.

  Own Id: OTP-14090 Aux Id: seq13232

## Runtime_Tools 1.11

### Improvements and New Features

- Add option `queue_size` to ttb:tracer/2. This sets the maximum queue size for
  the IP trace driver which is used when tracing to shell and/or `{local,File}`.

  The default value for `queue_size` is specified by `dbg`, and it is now
  changed from 50 to 200.

  Own Id: OTP-13829 Aux Id: seq13171

- The port information page is updated to show more information per port.

  Own Id: OTP-13948 Aux Id: ERL-272

## Runtime_Tools 1.10.1

### Improvements and New Features

- Correct some minor documentation issues.

  Own Id: OTP-13891

## Runtime_Tools 1.10

### Fixed Bugs and Malfunctions

- Fix bug in dbg:trace_port/2 that could cause the trace ip driver to produce
  faulty error reports "...(re)selected before stop_select was called for driver
  trace_ip_drv".

  Own Id: OTP-13576 Aux Id: ERL-119

### Improvements and New Features

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

- Update observer GUI to support tracing on ports, and to set matchspecs for
  send/receive. This required some minor bugfixes in runtime_tools/dbg.

  Own Id: OTP-13481

- Update dbg and ttb to work with a tracer module as tracer and tracing on
  ports.

  Own Id: OTP-13500

- Updated dbg to accept the new trace options `monotonic_timestamp` and
  `strict_monotonic_timestamp`.

  Own Id: OTP-13502

- Introduce LTTng tracing via Erlang tracing.

  For LTTng to be enabled OTP needs to be built with configure option
  `--with-dynamic-trace=lttng`.

  The dynamic trace module `dyntrace` is now capable to be used as a LTTng sink
  for Erlang tracing. For a list of all tracepoints, see
  [Runtime Tools User's Guide](lttng.md) .

  This feature also introduces an incompatible change in trace tags. The trace
  tags `gc_start` and `gc_end` has been split into `gc_minor_start`,
  `gc_minor_end` and `gc_major_start`, `gc_major_end`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13532

## Runtime_Tools 1.9.3

### Improvements and New Features

- `dbg:trace_client()` now uses a read buffer to speed up reading of trace
  files.

  Own Id: OTP-13279

## Runtime_Tools 1.9.2

### Improvements and New Features

- Clarified dbg:stop documentation

  Own Id: OTP-13078

## Runtime_Tools 1.9.1

### Fixed Bugs and Malfunctions

- The `trace_file_drv` did not handle `EINTR` correct which caused it to fail
  when the runtime system received a signal.

  Own Id: OTP-12890 Aux Id: seq12885

## Runtime_Tools 1.9

### Improvements and New Features

- Change license text from Erlang Public License to Apache Public License v2

  Own Id: OTP-12845

## Runtime_Tools 1.8.16

### Fixed Bugs and Malfunctions

- The trace process started by `dbg` would not always terminate when
  `dbg:stop/0` was called.

  Own Id: OTP-12517

## Runtime_Tools 1.8.15

### Fixed Bugs and Malfunctions

- Add `nif_version` to `erlang:system_info/1` in order to get the NIF API
  version of the runtime system in a way similar to `driver_version`.

  Own Id: OTP-12298

## Runtime_Tools 1.8.14

### Fixed Bugs and Malfunctions

- The documentation for the return value of dbg:\{stop,stop_clear\} functions
  are now correct (Thanks to Luca Favatella)

  Own Id: OTP-11603

- Fix DTrace build on Illumos. (Thanks to Ryan Zezeski.)

  Own Id: OTP-11622

- Do not turn off scheduler_wall_time, as it can interfere with other
  applications usage.

  Own Id: OTP-11693 Aux Id: seq12528

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

### Improvements and New Features

- Allow install path to have unicode characters.

  Own Id: OTP-10877

- The `erts_alloc_config` tool has been updated to produce configurations that
  better fit todays SMP support in the VM.

  Own Id: OTP-11662

- The [`app`](`e:kernel:app.md`)\-file key
  [`runtime_dependencies`](`e:kernel:app.md#runtime_dependencies`) has been
  introduced.

  Runtime dependencies have been added to all app-files in OTP. Note that these
  may not be completely correct during OTP 17, but this is actively being worked
  on.

  The function `system_information:sanity_check/0` will verify all declared
  runtime dependencies in the system when called.

  Own Id: OTP-11773

## Runtime_Tools 1.8.13

### Fixed Bugs and Malfunctions

- Observer did not produce correct result when ERTS internal memory allocators
  had been disabled.

  Own Id: OTP-11520

## Runtime_Tools 1.8.12

### Fixed Bugs and Malfunctions

- The process trace flag 'silent' is now allowed in call to dbg:p/2.

  Own Id: OTP-11222

### Improvements and New Features

- Introduced functionality for inspection of system and build configuration.

  Own Id: OTP-11196

## Runtime_Tools 1.8.11

### Fixed Bugs and Malfunctions

- Some bugs related to calculation of CPU/scheduler utilization in observer are
  corrected.

  Current function for a process is accepted to be 'undefined' when running
  hipe.

  Own Id: OTP-10894

### Improvements and New Features

- Erlang source files with non-ASCII characters are now encoded in UTF-8
  (instead of latin1).

  Own Id: OTP-11041 Aux Id: OTP-10907

## Runtime_Tools 1.8.10

### Fixed Bugs and Malfunctions

- Fix Table Viewer refresh crash on no more existing ets tables (Thanks to Peti
  Gömori)

  Own Id: OTP-10635

### Improvements and New Features

- User Guides for the dynamic tracing tools dtrace and systemtap have been added
  to the documentation.

  Own Id: OTP-10155

- Where necessary a comment stating encoding has been added to Erlang files. The
  comment is meant to be removed in Erlang/OTP R17B when UTF-8 becomes the
  default encoding.

  Own Id: OTP-10630

- Some examples overflowing the width of PDF pages have been corrected.

  Own Id: OTP-10665

- The backend module appmon_info.erl is moved from appmon application to
  runtime_tools. This allows appmon to be run from a remote erlang node towards
  a target node which does not have appmon (and its dependencies) installed, as
  long as runtime_tools is installed there.

  Own Id: OTP-10786

## Runtime_Tools 1.8.9

### Fixed Bugs and Malfunctions

- Change the module-level docs to give complete step-by-step instructions for
  using the \`dyntrace:p()\` trace function. (Thanks to Scott Lystig Fritchie)

  Own Id: OTP-10141

- Add 1024 separate USDT probes to dyntrace.erl and dyntrace.c (Thanks to Scott
  Lystig Fritchie)

  Own Id: OTP-10143

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

## Runtime_Tools 1.8.8

### Improvements and New Features

- The DTrace source patch from Scott Lystig Fritchie is integrated in the source
  tree. Using an emulator with dtrace probe is still not supported for
  production use, but may be a valuable debugging tool. Configure with
  --with-dynamic-trace=dtrace (or --with-dynamic-trace=systemtap) to create a
  build with dtrace probes enabled. See runtime_tools for documentation and
  examples.

  Own Id: OTP-10017

## Runtime_Tools 1.8.7

### Fixed Bugs and Malfunctions

- Earlier dbg:stop only did erlang:trace_delivered and did not flush the trace
  file driver. Therefore there could still be trace messages that were delivered
  to the driver (guaranteed by erlang:trace_delivered) but not yet written to
  the file when dbg:stop returned. Flushing is now added on each node before the
  dbg process terminates.

  Own Id: OTP-9651

- File handles created by the trace_file_drv driver was inherited to child
  processes. This is now corrected.

  Own Id: OTP-9658

### Improvements and New Features

- Erlang/OTP can now be built using parallel make if you limit the number of
  jobs, for instance using '`make -j6`' or '`make -j10`'. '`make -j`' does not
  work at the moment because of some missing dependencies.

  Own Id: OTP-9451

- Two new built-in trace pattern aliases have been added: caller_trace (c) and
  caller_exception_trace (cx). See the dbg:ltp/0 documentation for more info.

  Own Id: OTP-9458

## Runtime_Tools 1.8.6

### Improvements and New Features

- Two new built-in trace pattern aliases have been added: caller_trace (c) and
  caller_exception_trace (cx). See the dbg:ltp/0 documentation for more info.

  Own Id: OTP-9458

## Runtime_Tools 1.8.5

### Improvements and New Features

- When a big number of trace patterns are set by inviso the Erlang VM could get
  unresponsive for several seconds. This is now corrected.

  Own Id: OTP-9048 Aux Id: seq11480

## Runtime_Tools 1.8.4.1

### Fixed Bugs and Malfunctions

- Minor corrections and removal of a temporary workaround.

  Own Id: OTP-8755 Aux Id: seq-11628, seq-11639

- Small fix in inviso_autostart_server.

  Own Id: OTP-8783 Aux Id: seq11628

## Runtime_Tools 1.8.4

### Improvements and New Features

- Miscellaneous updates.

  Own Id: OTP-8705

## Runtime_Tools 1.8.3

### Improvements and New Features

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
  - The `configure` scripts of `erl_interface` and `odbc` now search for thread
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

- Cleanups suggested by tidier and modernization of types and specs.

  Own Id: OTP-8455

## Runtime_Tools 1.8.2

### Improvements and New Features

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8201

## Runtime_Tools 1.8.1

### Fixed Bugs and Malfunctions

- `Makefile.in` has been updated to use the LDFLAGS environment variable (if
  set). (Thanks to Davide Pesavento.)

  Own Id: OTP-8157

## Runtime_Tools 1.8

### Improvements and New Features

- `etop` would crash if the emulator's custom allocators had been turned off
  (e.g. using the `+Meamin` option).

  Own Id: OTP-7519

- The copyright notices have been updated.

  Own Id: OTP-7851

- Now, dbg:p/2 accepts \{X,Y,Z\} process specification as stated in the
  documentation. It also now accepts "<X.Y.Z>" like from erlang:pid_to_list/1.

  There is now a pre-saved match spec in dbg that saves a lot of typing. Use
  dbg:ltp/0 to find out more...

  Own Id: OTP-7867

## Runtime_Tools 1.7.3

### Fixed Bugs and Malfunctions

- Fixed a timestamp problem where some events could be sent out of order. Minor
  fixes to presentation of data.

  Own Id: OTP-7544 Aux Id: otp-7442

## Runtime_Tools 1.7.2

### Fixed Bugs and Malfunctions

- `etop` now collects process information faster and more reliably than before
  (a race condition reported by Igor Goryachev has been eliminated).

  Trace messages could be lost when `ttb:stop/0` was called.

  Own Id: OTP-7164

## Runtime_Tools 1.7.1

### Improvements and New Features

- The documentation has been updated so as to reflect the last updates of the
  Erlang shell as well as the minor modifications of the control sequence `p` of
  the `io_lib` module.

  Superfluous empty lines have been removed from code examples and from Erlang
  shell examples.

  Own Id: OTP-6944 Aux Id: OTP-6554, OTP-6911

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

## Runtime_Tools 1.7

### Fixed Bugs and Malfunctions

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

### Improvements and New Features

- The undocumented and unsupported function `dbg:tracer/1` has been removed. The
  undocumented, unsupported, and broken function `dbg:i/1` has been removed.

  Own Id: OTP-6939

## Runtime_Tools 1.6.8

### Fixed Bugs and Malfunctions

- In this release the following has been fixed and enhanced: Autostart: It is
  now possible to configure modules that shall be loaded by the autostart
  mechanism. This because it is not certain that all application systems make
  use of the OTP boot script to set up paths to all Erlang modules.
  Runtime_tools/Inviso: A bug in the fetch_log functionality has been fixed.
  Further a bug that was (supposedly) fixed in a previous patch concerning
  meta-tracer write_ti has been fixed (again) in this patch. A bug in
  inviso_as_lib making remote autostart config file updates fail has been fixed.
  Inviso: inviso_tool has been given a flush API.

  Own Id: OTP-6918

## Runtime_Tools 1.6.7

### Improvements and New Features

- The following bugs/improvements have been done: Internal interworking between
  inviso_rt and inviso_rt_meta. The call function used by inviso_rt to call
  inviso_rt_meta is now protected with a monitor. Inviso_rt_meta now includes
  the timestamp of the incoming meta trace message when calling the
  call-callback. (Makes it possible to add a "better" timestamp to the ti-file.)
  Bug in inviso_tool making it not remove trace patterns when terminating. Bug
  in internal function h_start_session making inviso_tool crash if there were no
  active nodes to start the session on. The user-inviso_tool and inviso
  API-inviso control component request/response gen_server protocols had default
  time-out. Since many trace operations can be time consuming, a longer time-out
  is necessary. Improved overload protection. It is now possible to let the
  overload protection renew itself (e.g after an exit from an external overload
  protector). Inviso_rt_meta now fully uses the exception_trace match spec
  action term. Run Trace Case API (as in contrast to activate and deactivate
  trace case APIs) in inviso_tool. Flush trace-port API added to inviso.
  Get_session_data API added to inviso_tool. Improved inviso_tool:stop making it
  possible to name nodes which shall not have their trace patterns removed when
  inviso_tool terminates. Bug in handling of writing multiple ti-entries if
  returned from a call/return_from call-back in inviso_rt_meta Process trace
  flags are no longer explicitly removed by the inviso_tool when it terminates.
  Not necessary. Inviso_tool get_autostart_data adopted to standard autostarter.

  \*** INCOMPATIBILITY with Meta trace call-backs are called with different
  arguments now. \***

  Own Id: OTP-6881

## Runtime_Tools 1.6.6

### Fixed Bugs and Malfunctions

- A bug in inviso_rt_meta caused an ETS table containing information on
  initiated (init_tpm) functions to be lost when suspending tracing. Further an
  enhancement to inviso_rt has been introduced making it possible to activate
  process trace flags based on globally registered names. It is then not an
  error to activate a global name on a node where the name does not reside. The
  process count in the return value will simply be set to zero (hence exactly
  one node in the NodeResult part of the return value will indicate one matching
  process found). A bug was found in fetch_log API. At the same time the
  fetch_log functionality was enhanced to also offer flow control making fetcher
  processes send chunks of transferred file data at a slower pace.

  Own Id: OTP-6703

### Improvements and New Features

- Minor Makefile changes.

  Own Id: OTP-6689 Aux Id: OTP-6742

- An experimental tool called `erts_alloc_config` has been added.
  `erts_alloc_config` is intended to be used to aid creation of an `erts_alloc`
  configuration that is suitable for a limited number of runtime scenarios. For
  more information see the `m:erts_alloc_config` documentation.

  Own Id: OTP-6700

## Runtime_Tools 1.6.5

### Improvements and New Features

- Misc improvements.

  Own Id: OTP-6576

## Runtime_Tools 1.6.4

### Improvements and New Features

- This application has been updated to eliminate warnings by Dialyzer.

  Own Id: OTP-6551

## Runtime_Tools 1.6.3

### Fixed Bugs and Malfunctions

- This ticket includes several improvements and bugfixes to both runtime_tools
  and inviso. The overload mechanism can now also react to incoming messages.
  This is useful if an external overload watch-dog is used. Some improvements of
  variable bindings has been done to the default autostart mechanism -
  inviso_autostart_server. Autostart "jobs" can now be done in parallel,
  allowing for some jobs to hang waiting for some parts of the traced system to
  become ready before proceeding. Previously when using named meta-match-specs
  (tpm_ms) ending up with zero match-specs still kept the meta trace pattern
  active. This caused zero match-specs to be equal to unlimited meta tracing on
  that particular function. If the internal database becomes empty of meta match
  specs, meta trace pattern is removed for that function. Standard public loop
  data in the inviso runtime meta tracer process is now extended to a 2-tuple.
  The functions ctp/1 and ctpl/1 are added making it possible to remove trace
  patterns for a list of functions rather than one by one. Inviso_rt_meta will
  now accept a list of binaries to be output into the trace information file, in
  additions to a single binary. Further it is also possible to make own output
  to the trace information file using the write_ti/1 function. An error was
  discovered in inviso_rt making the inviso_rt_meta remain rather than terminate
  if the inviso_rt terminated due to "running alone" (not allowed to run without
  a control component). A new tool, inviso_tool, has been added to the inviso
  application.

  Own Id: OTP-6426

## Runtime_Tools 1.6.2

### Fixed Bugs and Malfunctions

- Several minor bugs and race conditions eliminated in the runtime_tools and
  observer applications.

  Own Id: OTP-6265

## Runtime_Tools 1.6.1

### Improvements and New Features

- There are new BIFs `erlang:spawn_monitor/1,3`, and the new option `monitor`
  for `spawn_opt/2,3,4,5`.

  The `observer_backend` module has been updated to handle the new BIFs.

  Own Id: OTP-6281

## Runtime_Tools 1.6

Added the runtime part of the Inviso tracer, see the new Inviso application for
more information. This also meant adding an application callback module and an
application supervisor tree for Runtime_Tools.

## Runtime_Tools 1.5.1.1

### Improvements and New Features

- The `dbg` manual page has been updated with information about how to avoid
  deadlock when tracing.

  Own Id: OTP-5373 Aux Id: seq9729

## Runtime_Tools 1.5.1

### Fixed Bugs and Malfunctions

- Linked in drivers in the Crypto, and Asn1 applications are now compiled with
  the `-D_THREAD_SAFE` and `-D_REENTRANT` switches on unix when the emulator has
  thread support enabled.

  Linked in drivers on MacOSX are not compiled with the undocumented
  `-lbundle1.o` switch anymore. Thanks to Sean Hinde who sent us a patch.

  Linked in driver in Crypto, and port programs in SSL, now compiles on OSF1.

  Minor `Makefile` improvements in Runtime_Tools.

  Own Id: OTP-5346
