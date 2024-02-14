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
# Kernel Release Notes

This document describes the changes made to the Kernel application.

## Kernel 9.2.1

### Fixed Bugs and Malfunctions

* Fix `group` (that is the shell) to properly handle when an `get_until` callback function returned `{done, eof, []}` when an `eof` was detected.

  Own Id: OTP-18901

## Kernel 9.2

### Fixed Bugs and Malfunctions

- For `inet_backend = socket`, an unexpected receive error such as `etimedout`
  caused the receiving state machine server to crash. This bug has now been
  fixed.

  Own Id: OTP-18749 Aux Id: GH-7608

- Fix bug where reading using `file` from a unicode enabled `standard_io`,
  `standard_error` or any other `group` backed device would result in incorrect
  values being returned or a crash.

  Now instead a no_translation error is returned to the caller when unicode data
  is read using `file`. See
  [Using Unicode](`e:stdlib:unicode_usage.md#escripts-and-non-interactive-i-o`)
  in the STDLIB User's Guide for more details on how to correctly read from
  `standard_io`.

  Own Id: OTP-18800 Aux Id: PR-7714 GH-7591

- The native resolver interface module has gotten a rewrite of its ETS table
  handling to minimize term copying, and also to move the handling of client
  time-outs to the clients, which helps the native resolver name server from
  digging itself into a tar pit when heavily loaded.

  Own Id: OTP-18812 Aux Id: ERIERL-997

- Replaced unintentional Erlang Public License 1.1 headers in some files with
  the intended Apache License 2.0 header.

  Own Id: OTP-18815 Aux Id: PR-7780

- Fix bug in `pg` if a client process both monitored a group/scope and joined a
  group. The termination of such process resulted in crash of the `pg` server
  process.

  Own Id: OTP-18833 Aux Id: GH-7625, PR-7659

- Fix crash when using `file:consult` and the underlying file read returns an
  error while reading.

  Own Id: OTP-18873 Aux Id: PR-7831

- Corrected gen_tcp_socket listen option handling.

  Own Id: OTP-18883 Aux Id: #7764

### Improvements and New Features

- Add Windows support for DGRAM socket connect.

  Own Id: OTP-18762

- Document the, previously opaque, types select_tag() and completion_tag().

  Own Id: OTP-18818 Aux Id: #7337

## Kernel 9.1

### Fixed Bugs and Malfunctions

- Fixed an issue with truncated crash slogans on failed emulator start.

  Own Id: OTP-18623 Aux Id: GH-7344

- Fix shell:start_interactive function specification.

  Own Id: OTP-18628 Aux Id: GH-7280

- Fix code:get_doc/1 to return missing, when it can't find erts instead of
  crashing.

  Own Id: OTP-18654 Aux Id: PR-7404

- Function socket:close/1 could cause a VM crash on Windows.

  Own Id: OTP-18669 Aux Id: OTP-18029

- Fix deadlock when `erl.exe` is used as part of a pipe on Windows and trying to
  set the encoding of the `standard_io` device.

  Own Id: OTP-18675 Aux Id: PR-7473 GH-7459

- Expanded the documentation about how to use the `standard_io`,
  `standard_error` and `user` I/O devices.

  Added the types [`io:standard_io/0`](`t:io:standard_io/0`),
  `io:standard:error/0` and [`io:user/0`](`t:io:user/0`).

  Own Id: OTP-18676 Aux Id: PR-7473 GH-7459

- Fix logger's overload protection mechanism to only fetch memory used by
  messages when needed.

  Own Id: OTP-18677 Aux Id: PR-7418 GH-7417

- Fixed a number of socket-related issues causing incompatibilities with gen_tcp
  and gen_udp respectively.

  Own Id: OTP-18685

- gen_tcp:connect with socket address and socket (inet-) backend fails because
  of missing callback function.

  Own Id: OTP-18707 Aux Id: #7530

- The DNS RR cache used by \`inet_res\` has been fixed to preserve insert order,
  which is beneficial when the DNS server returns RRs in some specific order for
  e.g load balancing purposes.

  Own Id: OTP-18731 Aux Id: GH-7577, PR-7578

- The options \`reuseport\`, \`reuseport_lb\` and \`exclusiveaddruse\` were
  accidentally not allowed for e.g \`gen_udp:open/1,2\`, which has now been
  corrected.

  Own Id: OTP-18734 Aux Id: OTP-18344, PR-6522, GH-6461, GH-7569

- `gen_udp:recv/*` for Unix Domain Socket in binary mode and passive mode has
  been fixed to not crash.

  Own Id: OTP-18747 Aux Id: GH-7605

- Fixed issue where cursor would not be placed at the end of the expression when
  navigating shell history.

  Own Id: OTP-18757 Aux Id: PR-7631

### Improvements and New Features

- Update gen_tcp_socket and gen_udp_socket to handle 'completion' (socket on
  Windows).

  Own Id: OTP-18586 Aux Id: OTP-18029

- Add support for Unix Domain Sockets (only for STREAM sockets) on Windows for
  'socket'.

  Own Id: OTP-18611 Aux Id: OTP-18029, #5024

- Add basic support for socket ioctl on Windows.

  Own Id: OTP-18660

- The [`file:location/0`](`t:file:location/0`) type is now exported.

  Own Id: OTP-18681

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

- The keyboard shortcuts for the shell are now configurable.

  Own Id: OTP-18754 Aux Id: PR-7604 PR-7647

- Optimized code_server to reduce repeated work when loading the same module
  concurrently.

  Own Id: OTP-18755 Aux Id: PR-7503

## Kernel 9.0.2

### Fixed Bugs and Malfunctions

- Fix bug where when you entered Alt+Enter in the terminal, the cursor would
  move to the last line, instead of moving to the next line.

  Own Id: OTP-18580 Aux Id: PR-7242

- Fix so that the shell does not crash on startup when termcap is not available.

  Own Id: OTP-18624 Aux Id: GH-7296

- Multiple socket:accept calls issue. When making multiple accept calls, only
  the last call is active.

  Own Id: OTP-18635 Aux Id: #7328

- Fix the shell to ignore terminal delay when the terminal capabilities report
  that they should be used.

  Own Id: OTP-18636 Aux Id: PR-7352 GH-7308

- Fix "oldshell" to echo characters while typing on Windows.

  Own Id: OTP-18637 Aux Id: PR-7359 GH-7324

- Fix eof handling when reading from stdin when erlang is started using
  `-noshell`.

  Own Id: OTP-18640 Aux Id: PR-7384 GH-7368 GH-7286 GH-6881

- On Windows, a call to the function socket:close, when there are waiting active
  calls to read, write or accept functions, could hang.

  Own Id: OTP-18646

- Fix issues when reading or configuring `standard_io` on Windows when erl.exe
  is started using `-noshell` flag.

  Own Id: OTP-18649 Aux Id: GH-7261 PR-7400

- gen_udp:connect with inet_backend = socket fails when the Address is a
  hostname (string or atom).

  Own Id: OTP-18650

- Fixed problem which would cause shell to crash if particular escape sequence
  was written to stdout.

  Own Id: OTP-18651 Aux Id: PR-7242

- Fixed problem where output would disappear if it was received after a prompt
  was written in the shell.

  Own Id: OTP-18652 Aux Id: PR-7242

- Fix a crash where the location of erts could not be found in rebar3 dev
  builds.

  Own Id: OTP-18656 Aux Id: PR-7404 GH-7390

- Introduce the KERNEL application parameter `standard_io_encoding` that can be
  used to set the default encoding for standard_io. This option needs to be set
  to `latin1` if the application wants to treat all input data as bytes rather
  than utf-8 encoded characters.

  Own Id: OTP-18657 Aux Id: GH-7230 PR-7384

## Kernel 9.0.1

### Fixed Bugs and Malfunctions

- The POSIX error `exdev` was sometimes incorrectly described as "cross domain
  link" in some error messages.

  Own Id: OTP-18578 Aux Id: GH-7213

- Corrected the socket send function description (send with Timeout = nowait).
  The send function(s) could not return \{ok, \{RestData, SelectInfo\}\}

  Own Id: OTP-18584 Aux Id: #7238

## Kernel 9.0

### Fixed Bugs and Malfunctions

- Fixed a bug where duplicate keys were allowed in the .app file of an
  application. Duplicate keys are now rejected and the application will not
  start if they exist.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18210 Aux Id: GH-5877 PR-5878

- Fix inconsistent handling in logger_formatter of the branched values in
  conditional branches. For example using `msg` in a conditional branch would
  not be formatted as it should before this fix.

  Own Id: OTP-18225 Aux Id: PR-6036

- Fix the logger_std_h handler to log to standard_error if logging to
  standard_io fails for any reason.

  Own Id: OTP-18226 Aux Id: PR-6253

- Fix the TLS distribution to work when starting Erlang in embedded mode and a
  connection is done before kernel is fully started.

  Own Id: OTP-18248 Aux Id: PR-6227 GH-6085

- erl `-remsh` has been improved to provide better error reasons and work when
  using a shell without terminal support (that is an "oldshell").

  Own Id: OTP-18271 Aux Id: PR-6279

- Fix logging of log events generated before kernel is started to not fail if
  the code for formatting those log messaged have not yet been loaded.

  Own Id: OTP-18286 Aux Id: PR-5955

- `proc_lib:start*/*` has become synchronous when the started process fails.
  This requires that a failing process use a new function
  `proc_lib:init_fail/2,3`, or exits, to indicate failure. All OTP behaviours
  have been fixed to do this.

  All these start functions now consume the `'EXIT'` message from a process link
  for all error returns. Previously it was only the `start_link/*` functions
  that did this, and only when the started function exited, not when it used
  `init_ack/1,2` or `init_fail/2,3` to create the return value.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18471 Aux Id: GH-6339, PR-6843

- Fixed a bug where `file:read(standard_io, ...)` unexpectedly returned `eof` in
  binary mode.

  Own Id: OTP-18486 Aux Id: PR-6881

- Return type for `seq_trace:get_token/1` fixed.

  Own Id: OTP-18528 Aux Id: PR-7009

- Looking up, connecting to and sending to a host with an empty name is now
  handled by trying to look up the address of the root domain, which fails.
  Previously some of these operations caused an internal exception, which
  contradicted type specifications.

  Own Id: OTP-18543 Aux Id: GH-6353

- Replaced a regex with a special case handling of ANSI Select Graphic Rendition
  escape characters, to speed up io output that make use of these escape
  sequences.

  Own Id: OTP-18547 Aux Id: PR-7092

### Improvements and New Features

- The Erlang shell has been improved to support the following features:

  - Auto-complete variables, record names, record field names, map keys,
    function parameter types and filenames.
  - Open external editor in the shell (with C-o) to edit the current expression
    in an editor.
  - Support defining records (with types), functions and function typespecs, and
    custom types in the shell.
  - Do not save pager commands, and input to io:getline in history.

  Own Id: OTP-14835 Aux Id: PR-5924

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

- As announced since the release of OTP 24, support for:

  - version 4 node container types in the external term format are now
    mandatory. That is, references supporting up to 5 32-bit integer
    identifiers, and process and port identifiers with support for 64-bit data
    storage. The distribution flag
    [`DFLAG_V4_NC`](`e:erts:erl_dist_protocol.md#DFLAG_V4_NC`) is therefor now
    also mandatory. OTP has since OTP 24 supported this. Also note that the
    external format produced by `term_to_binary()` and `term_to_iovec()` will
    unconditionally produce pids, ports, and references supporting this larger
    format.
  - the [new link protocol](`e:erts:erl_dist_protocol.md#new_link_protocol`)
    introduced in OTP 23.3 is now mandatory. The distribution flag
    [`DFLAG_UNLINK_ID`](`e:erts:erl_dist_protocol.md#DFLAG_UNLINK_ID`) is
    therefor now also mandatory.

  Due to the above, OTP 26 nodes will refuse to connect to OTP nodes from
  releases prior to OTP 24.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18140 Aux Id: PR-6072

- Support for Kernel TLS (kTLS), has been added to the SSL application, for TLS
  distribution (`-proto_dist inet_tls`), the SSL option `{ktls, true}`. Using
  this for general SSL sockets is uncomfortable, undocumented and not
  recommended since it requires very platform dependent raw options.

  This, for now, only works for some not too old Linux distributions. Roughly, a
  kernel 5.2.0 or later with support for UserLand Protocols and the kernel
  module `tls` is required.

  Own Id: OTP-18235 Aux Id: PR-6104, PR-5840

- Add code:get_doc/2 which adds support to fetch documentation skeletons of
  functions using debug_info chunks instead of eep48 doc chunks.

  Own Id: OTP-18261 Aux Id: PR-5924

- The Erlang shell's auto-completion when typing `tab` has been changed to
  happen after the editing current line instead of before it.

  This behaviour can be configured using a the `shell_expand_location` STDLIB
  configuration parameter.

  Own Id: OTP-18278 Aux Id: PR-6260

- Typing `Ctrl+L` in a shell now clears the screen and redraws the current line
  instead of only redrawing the current line. To only redraw the current line,
  you must now type `Alt+L`. This brings the behaviour of `Ctrl+L` closer to how
  bash and other shells work.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18285 Aux Id: PR-6262

- gen_server optimized by caching callback functions

  Own Id: OTP-18305 Aux Id: PR-5831

- Prepare the `pg` communication protocol for upgrade. The plan is for OTP-28
  nodes to be able to use an upgraded `pg` protocol while still being able to
  talk with OTP 26 nodes.

  Own Id: OTP-18327 Aux Id: PR-6433

- New `disk_log` log type `rotate`, where the log files are compressed upon
  rotation.

  Own Id: OTP-18331 Aux Id: ERIERL-870

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

- Replace size/1 with either tuple_size/1 or byte_size/1

  The [`size/1`](`size/1`) BIF is not optimized by the JIT, and its use can
  result in worse types for Dialyzer.

  When one knows that the value being tested must be a tuple,
  [`tuple_size/1`](`tuple_size/1`) should always be preferred.

  When one knows that the value being tested must be a binary,
  [`byte_size/1`](`byte_size/1`) should be preferred. However,
  [`byte_size/1`](`byte_size/1`) also accepts a bitstring (rounding up size to a
  whole number of bytes), so one must make sure that the call to `byte_size/` is
  preceded by a call to [`is_binary/1`](`is_binary/1`) to ensure that bitstrings
  are rejected. Note that the compiler removes redundant calls to
  [`is_binary/1`](`is_binary/1`), so if one is not sure whether previous code
  had made sure that the argument is a binary, it does not harm to add an
  [`is_binary/1`](`is_binary/1`) test immediately before the call to
  [`byte_size/1`](`byte_size/1`).

  Own Id: OTP-18405 Aux Id:
  GH-6672,PR-6702,PR-6768,PR-6700,PR-6769,PR-6812,PR-6814

- The function `file:pid2name/1` is deprecated and will be removed in
  Erlang/OTP 27.

  Own Id: OTP-18419

- The modules Erlang DNS resolver `inet_res` and helper modules have been
  updated for RFC6891; to handle OPT RR with DNSSEC OK (DO) bit.

  Own Id: OTP-18442 Aux Id: PR-6786, GH-6606

- Introduced `application:get_supervisor/1`.

  Own Id: OTP-18444 Aux Id: PR-6035

- Handling of `on_load` modules during boot has been improved by adding an extra
  step in the boot order for embedded mode that runs all `on_load` handlers,
  instead of relying on explicit invocation of them, later, when the kernel
  supervision tree starts.

  This is mostly a code improvement and OTP internal simplification to avoid
  future bugs and to simplify code maintenance.

  Own Id: OTP-18447

- Reduce contention on the code_server by doing the code preparation on the
  client.

  Own Id: OTP-18448 Aux Id: PR-6736

- Added a mode to ensure_all_loaded, to start children application and their
  dependencies concurrently.

  Own Id: OTP-18451 Aux Id: PR-6737

- Cache OTP boot code paths, to limit how many folders that are being accessed
  during a module lookup. Can be disabled with -cache_boot_path false. OTP boot
  code paths consists of ERL_LIB environment variables. The various otp/\*/ebin
  folders. And the \{path, ...\} clauses in the init script.

  Own Id: OTP-18452 Aux Id: PR-6729

- Erlang distribution code in Kernel and SSL has been refactored a bit to
  facilitate debugging and re-usability, which shouldn't have any noticeable
  effects on behaviour or performance.

  Own Id: OTP-18456

- Add cache attribute to code path apis.

  Added an optional cache/nocache argument to all code:add_path*,
  code:set_path*, and code:replace_path\* functions. These functions will then
  avoid doing file-accesses if they are cached. Cache can be cleared with
  code:clear_cache/0. Added code:del_paths/1 to make it easier to clear multiple
  paths.

  Own Id: OTP-18466 Aux Id: PR-6832

- Deprecates `dbg:stop_clear/0` because it is simply a function alias to
  `dbg:stop/0`

  Own Id: OTP-18478 Aux Id: GH-6903

- Improvements to code:ensure_modules_loaded/1: Previously it would prepare
  modules and then abandon references to said modules if they had on_load
  callbacks. This pull request makes it so they keep the references around and
  then serially load them without having to fetch the object code and prepare
  them again.

  Own Id: OTP-18484 Aux Id: PR-6844

- The internal DNS resolver has been updated to handle DNS LOC RR:s (RFC 1876).
  This is an undocumented module, although still used by power users. See the
  source code.

  Own Id: OTP-18510 Aux Id: GH-6098, PR-6982

- Reduced memory consumption in `global` when informing other nodes about lost
  connections.

  Own Id: OTP-18521 Aux Id: PR-7025

- The `net_kernel`, `global`, and `global_group` servers now have
  [_fully asynchronous distributed signaling_](`m:erlang#process_flag_async_dist`)
  enabled all the time which prevents them from ever getting blocked on send of
  distributed signals.

  Documentation about blocking distributed signals has also been improved.

  Own Id: OTP-18533 Aux Id: PR-7061

- Allow IPv6 addresses as host in `http` packets decoded by
  `erlang:decode_packet/3` and `gen_tcp` packet option. The IPv6 address should
  be enclosed within `[]` according to RFC2732.

  Own Id: OTP-18540 Aux Id: PR-6900

- Remove deprecated functions in OTP-26

  Own Id: OTP-18542

- Removed `code:is_module_native/1` since HiPE has been removed. It has since
  OTP 24 been deprecated and scheduled for removal in OTP 26.

  Removed `code:rehash/0` since the code path feature no longer is present. It
  has since OTP 19 been deprecated and has since OTP 24 been scheduled for
  removal in OTP 26.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18551 Aux Id: PR-7106

- Added support for multiple line expressions and navigation in the shell. Added
  new keybindings:

  - navigate up (ctrl+up)/(alt+up)
  - navigate down (ctrl+down)/(alt+down)
  - insert newline in middle of line (alt+enter)
  - navigate top (alt+<)/(alt+shift+up)
  - navigate bottom (alt+>)/(alt+shift+down)
  - clear current expression (alt+c)
  - cancel search (alt+c)
  - opening editor on mac (option+o)/(alt+o)

  Modifies the prompt for new lines to make it clearer that the prompt has
  entered multi-line mode. Supports terminal with small window size, recommend
  not go lower than 7 rows and 40 columns. Modifies the search prompt to support
  multi-line statements. Redraw the prompt after continuing from JCL menu.

  Own Id: OTP-18575 Aux Id: PR-7169

## Kernel 8.5.4.2

### Fixed Bugs and Malfunctions

- gen_tcp:connect with socket address and socket (inet-) backend fails because
  of missing callback function.

  Own Id: OTP-18707 Aux Id: #7530

## Kernel 8.5.4.1

### Fixed Bugs and Malfunctions

- Multiple socket:accept calls issue. When making multiple accept calls, only
  the last call is active.

  Own Id: OTP-18635 Aux Id: #7328

- gen_udp:connect with inet_backend = socket fails when the Address is a
  hostname (string or atom).

  Own Id: OTP-18650

## Kernel 8.5.4

### Fixed Bugs and Malfunctions

- Fixed a bug on Windows where `file:read_file_info/1` would fail for files with
  corrupt metadata.

  Own Id: OTP-18348 Aux Id: GH-6356

- Accept connection setup from OTP 23 and 24 nodes that are not using epmd.

  Own Id: OTP-18404 Aux Id: GH-6595, PR-6625

## Kernel 8.5.3

### Fixed Bugs and Malfunctions

- The tcp connect option 'bind_to_device' could not be used with inet_backend =
  'socket'. 'inet' requires value type binarry() and 'socket' requires value
  type 'string()'.

  Own Id: OTP-18357 Aux Id: #6509

- Minor issue processing options when calling gen_tcp:connect with a sockaddr()
  and inet_backend = socket.

  Own Id: OTP-18358 Aux Id: #6528

## Kernel 8.5.2

### Fixed Bugs and Malfunctions

- Fixed shutdown crash in gen_tcp socket backend, when the other end closed the
  socket.

  Own Id: OTP-18270 Aux Id: #6331

- `erl_tar` can now read gzip-compressed tar files that are padded. There is a
  new option `compressed_one` for `file:open/2` that will read a single member
  from a gzip file,

  Own Id: OTP-18289 Aux Id: PR-6343

- Fix `os:cmd` to not translate all exceptions thrown to `badarg`. For example
  `emfile` from `erlang:open_port` was translated to `badarg`.

  This bug has existed since Erlang/OTP 24.

  Own Id: OTP-18291 Aux Id: PR-6382

- Spec for function net:if_names/0 incorrect

  Own Id: OTP-18296 Aux Id: OTP-16464

- Missing ctrl option name transation for TOS and TTL (on FreeBSD) when using
  gen_udp with the 'socket' inet_backend.

  Own Id: OTP-18315

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

### Improvements and New Features

- The distribution socket option handling in `inet_tcp_dist` has been cleaned up
  to clarify which were mandatory and which just had default values.

  Own Id: OTP-18293

- Improve warning message format for gen_tcp_socket.

  Own Id: OTP-18317

## Kernel 8.5.1

### Fixed Bugs and Malfunctions

- Listen sockets created with the socket module, leaked (erlang-) monitors.

  Own Id: OTP-18240 Aux Id: #6285

- `m:peer` nodes failed to halt when the process supervising the control
  connection crashed. When an alternative control connection was used, this
  supervision process also quite frequently crashed when the `peer` node was
  stopped by the node that started it which caused the `peer` node to linger
  without ever halting.

  Own Id: OTP-18249 Aux Id: PR-6301

## Kernel 8.5

### Fixed Bugs and Malfunctions

- Fixed inconsistency bugs in `m:global` due to `nodeup`/`nodedown` messages not
  being delivered before/after traffic over connections. Also fixed various
  other inconsistency bugs and deadlocks in both `m:global_group` and `global`.

  As building blocks for these fixes, a new BIF `erlang:nodes/2` has been
  introduced and `net_kernel:monitor_nodes/2` has been extended.

  The [`-hidden`](`e:erts:erl_cmd.md#hidden`) and
  [`-connect_all`](`e:erts:erl_cmd.md#connect_all`) command line arguments did
  not work if multiple instances were present on the command line which has been
  fixed. The new kernel parameter [`connect_all`](kernel_app.md#connect_all) has
  also been introduced in order to replace the `-connect_all` command line
  argument.

  Own Id: OTP-17934 Aux Id: PR-6007

- Fixed IPv6 multicast_if and membership socket options.

  Own Id: OTP-18091 Aux Id: #5789

- Fixed issue with inet:getifaddrs hanging on pure IPv6 Windows

  Own Id: OTP-18102 Aux Id: #5904

- The type specifications for `inet:getopts/2` and `inet:setopts/2` have been
  corrected regarding SCTP options.

  Own Id: OTP-18115 Aux Id: PR-5939

- The type specifications for `inet:parse_*` have been tightened.

  Own Id: OTP-18121 Aux Id: PR-5972

- Fix gen_tcp:connect/3 spec to include the inet_backend option.

  Own Id: OTP-18171 Aux Id: PR-6131

- Fix bug where using a binary as the format when calling
  `logger:log(Level, Format, Args)` (or any other logging function) would cause
  a crash or incorrect logging.

  Own Id: OTP-18229 Aux Id: PR-6212

### Improvements and New Features

- Add rudimentary debug feature (option) for the inet-driver based sockets, such
  as gen_tcp and gen_udp.

  Own Id: OTP-18032

- Introduced the `hidden` and `dist_listen` options to `net_kernel:start/2`.

  Also documented the [`-dist_listen`](`e:erts:erl_cmd.md#dist_listen`) command
  line argument which was erroneously documented as a `kernel` parameter and not
  as a command line argument.

  Own Id: OTP-18107 Aux Id: PR-6009

- Scope and group monitoring have been introduced in `m:pg`. For more
  information see the documentation of
  [`pg:monitor_scope()`](`pg:monitor_scope/0`),
  [`pg:monitor()`](`pg:monitor/1`), and [`pg:demonitor()`](`pg:demonitor/1`).

  Own Id: OTP-18163 Aux Id: PR-6058, PR-6275

- A new function `global:disconnect/0` has been introduced with which one can
  cleanly disconnect a node from all other nodes in a cluster of `global` nodes.

  Own Id: OTP-18232 Aux Id: OTP-17843, PR-6264

## Kernel 8.4.2

### Fixed Bugs and Malfunctions

- A call to [`net_kernel:setopts(new, Opts)`](`net_kernel:setopts/2`) at the
  same time as a connection was being set up could cause a deadlock between the
  `net_kernel` process and the process setting up the connection.

  Own Id: OTP-18198 Aux Id: GH-6129, PR-6216

## Kernel 8.4.1

### Fixed Bugs and Malfunctions

- The DNS resolver `inet_res` has been fixed to ignore trailing dot difference
  in the request domain between the sent request and the received response, when
  validating a response.

  Own Id: OTP-18112 Aux Id: ERIERL-811

- A bug in `inet_res` has been fixed where a missing internal `{ok,_}` wrapper
  caused `inet_res:resolve/*` to return a calculated host name instead of an
  `{ok,Msg}` tuple, when resolving an IP address or a host name that is an IP
  address string.

  Own Id: OTP-18122 Aux Id: GH-6015, PR-6020

- The `erlang:is_alive()` BIF could return `true` before configured distribution
  service was available. This bug was introduced in OTP 25.0 ERTS version 13.0.

  The `erlang:monitor_node()` and `erlang:monitor()` BIFs could erroneously fail
  even though configured distribution service was available. This occurred if
  these BIFs were called after the distribution had been started using dynamic
  node name assignment but before the name had been assigned.

  Own Id: OTP-18124 Aux Id: OTP-17558, PR-6032

- Added the missing mandatory `address/0` callback in the `gen_tcp_dist`
  example.

  Own Id: OTP-18136

## Kernel 8.4

### Fixed Bugs and Malfunctions

- The DNS resolver implementation has been rewritten to validate replies more
  thoroughly, and a bit optimized to create less garbage.

  Own Id: OTP-17323

- The socket option 'reuseaddr' is _no longer_ ignored on Windows.

  Own Id: OTP-17447 Aux Id: GH-4819

- Fix bug where using the atoms `string` or `report` as the format when calling
  `logger:log(Level, Format, Args)` (or any other logging function) would cause
  a crash or incorrect logging.

  Own Id: OTP-17551 Aux Id: GH-5071 PR-5075

- As of OTP 25, `global` will by default prevent overlapping partitions due to
  network issues by actively disconnecting from nodes that reports that they
  have lost connections to other nodes. This will cause fully connected
  partitions to form instead of leaving the network in a state with overlapping
  partitions.

  Prevention of overlapping partitions can be disabled using the
  [`prevent_overlapping_partitions`](kernel_app.md#prevent_overlapping_partitions)
  `kernel(6)` parameter, making `global` behave like it used to do. This is,
  however, problematic for all applications expecting a fully connected network
  to be provided, such as for example `mnesia`, but also for `global` itself. A
  network of overlapping partitions might cause the internal state of `global`
  to become inconsistent. Such an inconsistency can remain even after such
  partitions have been brought together to form a fully connected network again.
  The effect on other applications that expects that a fully connected network
  is maintained may vary, but they might misbehave in very subtle hard to detect
  ways during such a partitioning. Since you might get hard to detect issues
  without this fix, you are _strongly_ advised _not_ to disable this fix. Also
  note that this fix _has_ to be enabled on _all_ nodes in the network in order
  to work properly.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17911 Aux Id: PR-5687, PR-5611, OTP-17843

- Starting the helper program for name resolving; `inet_gethost`, has been
  improved to use an absolute file system path to ensure that the right program
  is started.

  If the helper program can not be started - the system now halts, to avoid
  running with a silently broken name resolver.

  Own Id: OTP-17958 Aux Id: OTP-17978

- The type specification for `inet_res:getbyname/2,3` has been corrected to
  reflect that it can return peculiar `#hostent{}` records.

  Own Id: OTP-17986 Aux Id: PR-5412, PR-5803

- `code:module_status/1` would always report BEAM files loaded from an archive
  as `modified`, and `code:modified_modules/0` would always return the name of
  all modules loaded from archives.

  Own Id: OTP-17990 Aux Id: GH-5801

- In logger fix file handler shutdown delay by using erlang timers instead of
  the timer module's timers.

  Own Id: OTP-18001 Aux Id: GH-5780 PR-5829

- Fix the meta data in log events generated by logger on failure to not contain
  the original log event's meta data.

  Own Id: OTP-18003 Aux Id: PR-5771

- Fix logger file backend to re-create the log folder if it has been deleted.

  Own Id: OTP-18015 Aux Id: GH-5828 PR-5845

- \[socket] Encode of sockaddr has been improved.

  Own Id: OTP-18020

- Fix `put_chars` requests to the io server with incomplete unicode data to exit
  with `no_translation` error.

  Own Id: OTP-18070 Aux Id: PR-5885

### Improvements and New Features

- The net module now works on Windows.

  Own Id: OTP-16464

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

- Fix `os:cmd` to work on Android OS.

  Own Id: OTP-17479 Aux Id: PR-4917

- The configuration files [`.erlang`](`e:erts:erl_cmd.md`),
  [`.erlang.cookie`](`e:system:distributed.md`) and
  [`.erlang.crypt`](`m:beam_lib#module-erlang-crypt`) can now be located in the XDG
  Config Home directory.

  See the documentation for each file and `filename:basedir/2` for more details.

  Own Id: OTP-17554 Aux Id: GH-5016 PR-5408 OTP-17821

- Dynamic node name improvements: `erlang:is_alive/0` changed to return true for
  pending dynamic node name and new function `net_kernel:get_state/0`.

  Own Id: OTP-17558 Aux Id: OTP-17538, PR-5111, GH-5402

- The types for callback result types in `gen_statem` has bee augmented with
  arity 2 types where it is possible for a callback module to specify the type
  of the callback data, so the callback module can get type validation of it.

  Own Id: OTP-17589 Aux Id: PR-4926

- The tagged tuple tests and fun-calls have been optimized and are now a little
  bit cheaper than previously.

  These optimizations become possible after making sure that all boxed terms
  have at least one word allocated after the arity word. This has been
  accomplished by letting all empty tuples refer to the same empty tuple literal
  which also reduces memory usage for empty tuples.

  Own Id: OTP-17608

- A [`net_ticker_spawn_options`](kernel_app.md#net_ticker_spawn_options)
  `kernel` configuration parameter with which one can set spawn options for the
  distribution channel ticker processes has been introduced.

  Own Id: OTP-17617 Aux Id: PR-5069

- The most, or at least the most used, `m:rpc` operations now require `m:erpc`
  support in order to communicate with other Erlang nodes. `erpc` was introduced
  in OTP 23. That is, `rpc` operations against Erlang nodes of releases prior to
  OTP 23 will fail.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17681 Aux Id: PR-5307

- The new module `peer` supersedes the `slave` module. The `slave` module is now
  deprecated and will be removed in OTP 27.

  `peer` contains an extended and more robust API for starting erlang nodes.

  Own Id: OTP-17720 Aux Id: PR-5162

- In order to make it easier for the user to manage multiple outstanding
  asynchronous `call` requests, new functionality utilizing request identifier
  collections have been introduced in
  [`erpc`](`t:erpc:request_id_collection/0`),
  [`gen_server`](`t:gen_server:request_id_collection/0`),
  [`gen_statem`](`t:gen_statem:request_id_collection/0`), and
  [`gen_event`](`t:gen_event:request_id_collection/0`).

  Own Id: OTP-17784 Aux Id: PR-5792

- Type specifications have been added to the `gen_server`, and the documentation
  has been updated to utilize this.

  This surfaced a few type violations that has been corrected in `global`,
  `logger_olp` and `rpc`.

  Own Id: OTP-17915 Aux Id: PR-5751, GH-2375, GH-2690

- IP address validation functions `is_ipv4_address/1`, `is_ipv6_address/1` and
  `is_ip_address/1` have been added to the module `inet` in Kernel.

  Own Id: OTP-17923 Aux Id: PR-5646

- An API for multihomed SCTP connect has been added in the guise of
  `gen_sctp:connectx_init/*`

  Own Id: OTP-17951 Aux Id: PR-5656

- \[socket] Add encoding of the field hatype of the type sockaddr_ll (family
  'packet').

  Own Id: OTP-17968 Aux Id: OTP-16464

- Added support for configurable features as described in EEP-60. Features can
  be enabled/disabled during compilation with options
  (`-enable-feature Feature`, `-disable-feature Feature` and
  `+{feature, Feature, enable|disable}`) to `erlc` as well as with directives
  (`-feature(Feature, enable|disable).`) in the file. Similar options can be
  used to `erl` for enabling/disabling features allowed at runtime. The new
  `maybe` expression (EEP-49) is fully supported as the feature `maybe_expr`.
  The features support is documented in the reference manual.

  Own Id: OTP-17988

## Kernel 8.3.2.4

### Fixed Bugs and Malfunctions

- gen_tcp:connect with socket address and socket (inet-) backend fails because
  of missing callback function.

  Own Id: OTP-18707 Aux Id: #7530

## Kernel 8.3.2.3

### Fixed Bugs and Malfunctions

- Spec for function net:if_names/0 incorrect

  Own Id: OTP-18296 Aux Id: OTP-16464

- Missing ctrl option name transation for TOS and TTL (on FreeBSD) when using
  gen_udp with the 'socket' inet_backend.

  Own Id: OTP-18315

- The tcp connect option 'bind_to_device' could not be used with inet_backend =
  'socket'. 'inet' requires value type binarry() and 'socket' requires value
  type 'string()'.

  Own Id: OTP-18357 Aux Id: #6509

- Minor issue processing options when calling gen_tcp:connect with a sockaddr()
  and inet_backend = socket.

  Own Id: OTP-18358 Aux Id: #6528

### Improvements and New Features

- Improve warning message format for gen_tcp_socket.

  Own Id: OTP-18317

## Kernel 8.3.2.2

### Improvements and New Features

- A new function `global:disconnect/0` has been introduced with which one can
  cleanly disconnect a node from all other nodes in a cluster of `global` nodes.

  Own Id: OTP-18232 Aux Id: OTP-17843, PR-6264

## Kernel 8.3.2.1

### Fixed Bugs and Malfunctions

- A call to [`net_kernel:setopts(new, Opts)`](`net_kernel:setopts/2`) at the
  same time as a connection was being set up could cause a deadlock between the
  `net_kernel` process and the process setting up the connection.

  Own Id: OTP-18198 Aux Id: GH-6129, PR-6216

## Kernel 8.3.2

### Fixed Bugs and Malfunctions

- inet:getopts/2 for the 'raw' option for a socket created with inet-backend
  'socket' failed.

  Own Id: OTP-18078 Aux Id: GH-5930

- Corrected the behaviour of the shutdown function when using with the
  inet_backend = socket. It was not sufficiently compatible with the "old"
  gen_tcp.

  Own Id: OTP-18080 Aux Id: GH-5930

## Kernel 8.3.1

### Fixed Bugs and Malfunctions

- Fix failed accepted connection setup after previous established connection
  from same node closed down silently.

  Own Id: OTP-17979 Aux Id: ERIERL-780

- Fixed a problem where typing Ctrl-R in the shell could hang if there were some
  problem with the history log file.

  Own Id: OTP-17981 Aux Id: PR-5791

## Kernel 8.3

### Fixed Bugs and Malfunctions

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
  [`prevent_overlapping_partitions`](kernel_app.md#prevent_overlapping_partitions)
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

- Fix bug where `logger` would crash when logging a report including improper
  lists.

  Own Id: OTP-17851

- Make `erlang:set_cookie` work for dynamic node names.

  Own Id: OTP-17902 Aux Id: GH-5402, PR-5670

### Improvements and New Features

- Add support for using socket:sockaddr_in() and socket:sockaddr_in6() when
  using gen_sctp, gen_tcp and gen_udp. This will make it possible to use Link
  Local IPv6 addresses.

  Own Id: OTP-17455 Aux Id: GH-4852

- A [`net_tickintensity`](kernel_app.md#net_tickintensity) `kernel` parameter
  has been introduced. It can be used to control the amount of ticks during a
  [`net_ticktime`](kernel_app.md#net_ticktime) period.

  A new `net_kernel:start/2` function has also been introduced in order to make
  it easier to add new options. The use of `net_kernel:start/1` has been
  deprecated.

  Own Id: OTP-17905 Aux Id: ERIERL-732, PR-5740

- Improve documentation for the dynamic node name feature.

  Own Id: OTP-17918

## Kernel 8.2

### Fixed Bugs and Malfunctions

- socket:which_sockets( pid() ) uses wrong keyword when looking up socket owner
  ('ctrl' instead of 'owner').

  Own Id: OTP-17716

- In epmd_ntop, the #if defined(EPMD6) conditional was inverted and it was only
  including the IPv6-specific code when EPMD6 was undefined. This was causing
  IPv6 addrs to be interpreted as IPv4 addrs and generating nonsense IPv4
  addresses as output.

  Several places were incorrectly using 'num_sockets' instead of 'i' to index
  into the iserv_addr array during error logging. This would result in a read
  into uninitialized data in the iserv_addr array.

  Thanks to John Eckersberg for providing this fix.

  Own Id: OTP-17730

- Minor fix of the `erl_uds_dist` distribution module example.

  Own Id: OTP-17765 Aux Id: PR-5289

- A bug has been fixed for the legacy TCP socket adaption module
  `gen_tcp_socket` where it did bind to a socket address when given a file
  descriptor, but should not.

  Own Id: OTP-17793 Aux Id: PR-5348, OTP-17451, PR-4787, GH-4680, PR-2989,
  OTP-17216

- Improve the error printout when [`open_port/2`](`open_port/2`) fails because
  of invalid arguments.

  Own Id: OTP-17805 Aux Id: PR-5406

- Calling socket:monitor/1 on an already closed socket should succeed and result
  in an immediate DOWN message. This has now been fixed.

  Own Id: OTP-17806

- Fix the configuration option `logger_metadata` to work.

  Own Id: OTP-17807 Aux Id: PR-5418

- Fix tls and non-tls distribution to use erl_epmd:address_please to figure out
  if IPv4 or IPv6 addresses should be used when connecting to the remote node.

  Before this fix, a dns lookup of the remote node hostname determined which IP
  version was to be used which meant that the hostname had to resolve to a valid
  ip address.

  Own Id: OTP-17809 Aux Id: PR-5337 GH-5334

### Improvements and New Features

- Add `logger:reconfigure/0`.

  Own Id: OTP-17375 Aux Id: PR-4663 PR-5186

- Add socket function ioctl/2,3,4 for socket device control.

  Own Id: OTP-17528

- Add simple support for socknames/1 for gen_tcp_socket and gen_udp_socket.

  Own Id: OTP-17531

- The types for callback result types in `gen_statem` has bee augmented with
  arity 2 types where it is possible for a callback module to specify the type
  of the callback data, so the callback module can get type validation of it.

  Own Id: OTP-17738 Aux Id: PR-4926, OTP-17589

## Kernel 8.1.3

### Fixed Bugs and Malfunctions

- The internal, undocumented, but used, module `inet_dns` has been fixed to
  handle mDNS high bit usage of the Class field.

  Code that uses the previously obsolete, undocumented and unused record field
  `#dns_rr.func` will need to be updated since that field is now used as a
  boolean flag for the mDNS high Class bit. Code that uses the also undocumented
  record `#dns_query` will need to be recompiled since a boolean field
  `#dns_query.unicast_response` has been added for the mDNS high Class bit.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17734 Aux Id: GH-5327, OTP-17659

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

## Kernel 8.1.2

### Fixed Bugs and Malfunctions

- The undocumented DNS encode/decode module `inet_dns` has been cleaned up to
  handle the difference between "symbolic" and "raw" records in a more
  consistent manner.

  PR-5145/OTP-17584 introduced a change that contributed to an already existing
  confusion, which this correction should remedy.

  Own Id: OTP-17659 Aux Id: ERIERL-702

## Kernel 8.1.1

### Fixed Bugs and Malfunctions

- Add more info about the socket 'type' ('socket' or 'port') for the DOWN
  message when monitoring sockets.

  Own Id: OTP-17640

## Kernel 8.1

### Fixed Bugs and Malfunctions

- The extended error information has been corrected and improved for the
  following BIFs: [`binary_to_existing_atom/2`](`binary_to_existing_atom/2`),
  [`list_to_existing_atom/1`](`list_to_existing_atom/1`),
  `erlang:send_after/{3,4}`, and `erlang:start_timer/{3,4}`.

  Own Id: OTP-17449 Aux Id: GH-4900

- Fixed rare bug that could cause net_kernel process to hang for ever. Have seen
  to happen with massive number of TLS connections while remote nodes are
  restarting. Bug exists since OTP-22.0.

  Own Id: OTP-17476 Aux Id: GH-4931, PR-4934

- Improve handling of closed sockets for inet:info/1.

  Own Id: OTP-17492

- This change fixes a performance problem introduced in pull-request #2675.
  Pull-request #2675 made so the system tried to start children of already
  started applications which is unnecessary. This change fixes this performance
  problem.

  Own Id: OTP-17519

- Fix code:get_doc/1 to not crash when module is located in an escript.

  Own Id: OTP-17570 Aux Id: PR-5139 GH-4256 ERL-1261

- Parsing of the result value in the native DNS resolver has been made more
  defensive against incorrect results.

  Own Id: OTP-17578 Aux Id: ERIERL-683

- A bug in the option handling for the legacy socket adaptor, that is; when
  using `inet_backend = socket`, has been fixed. Now socket options are set
  before the bind() call so options regarding, for example address reuse have
  the desired effect.

  Own Id: OTP-17580 Aux Id: GH-5122

- `inet:ntoa/1` has been fixed to not accept invalid numerical addresses.

  Own Id: OTP-17583 Aux Id: GH-5136

- Parsing of DNS records has been improved for records of known types to not
  accept and present malformed ones in raw format.

  Own Id: OTP-17584 Aux Id: PR-5145

- The `ip_mreq()` type for the `{ip,add_membership}` and `{ip,drop_membership}`
  socket options has been corrected to have an `interface` field instead of,
  incorrectly, an `address` field.

  Own Id: OTP-17590 Aux Id: PR-5170

### Improvements and New Features

- Add simple utility function to display existing sockets i the erlang shell
  (socket:i/0).

  Own Id: OTP-17376 Aux Id: OTP-17157

- gen_udp can now be configured to use the socket inet-backend (in the same way
  as gen_tcp).

  Own Id: OTP-17410

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

- Documented our recommendation against opening NFS-mounted files, FIFOs,
  devices, and similar using `file:open/2`.

  Own Id: OTP-17576 Aux Id: ERIERL-685

## Kernel 8.0.2

### Fixed Bugs and Malfunctions

- For `gen_tcp:connect/3,4` it is possible to specify a specific source port,
  which should be enough to bind the socket to an address with that port before
  connecting.

  Unfortunately that feature was lost in OTP-17216 that made it mandatory to
  specify the source address to get an address binding, and ignored a specified
  source port if no source address was specified.

  That bug has now been corrected.

  Own Id: OTP-17536 Aux Id: OTP-17216, ERIERL-677

## Kernel 8.0.1

### Fixed Bugs and Malfunctions

- Fix a race condition in Global.

  Own Id: OTP-16033 Aux Id: ERIERL-329, ERL-1414, GH-4448, ERL-885, GH-3923

- After a node restart with `init:restart/0,1`, the module `socket` was not
  usable because supporting tables had been cleared and not re-initialized. This
  has now been fixed.

  Handling of the "." domain as a search domain was incorrect and caused a crash
  in the DNS resolver `inet_res`, which has now been fixed.

  Own Id: OTP-17439 Aux Id: GH-4827, PR-4888, GH-4838

- Handling of combinations of the `fd` option and binding to an address has been
  corrected, especially for the `local` address family.

  Own Id: OTP-17451 Aux Id: OTP-17374

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

## Kernel 8.0

### Fixed Bugs and Malfunctions

- A bug has been fixed for the internal `inet_res` resolver cache that handled a
  resolver configuration file status timer incorrectly and caused performance
  problems due to many unnecessary file system accesses.

  Own Id: OTP-14700 Aux Id: PR-2848

- Change the value of the tag `head` returned by `disk_log:info/1` from
  `{ok, Head}` to just `Head`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16809 Aux Id: ERL-1313

- Two options have been added to `erl_call`. The `-fetch_stdout` option fetches
  stdout data resulting from the code invoked by `erl_call`. The `-fetch_stdout`
  option disables printing of the result term. In order to implement the first
  of these two options a new function called `ei_xrpc_from` has been added to
  erl_interface. For details see the `erl_call` documentation and
  `erl_interface` documentation.

  Own Id: OTP-17132

- Missing runtime dependencies has been added to this application.

  Own Id: OTP-17243 Aux Id: PR-4557

- `inet:get_rc/0` has been corrected to return host entries as separate entries
  instead of (incorrectly) in a list within the list. This bug was introduced by
  OTP-16487 in OTP-23.0-rc1.

  Own Id: OTP-17262 Aux Id: GH-4588, PR-4604, OTP-16487

- The type gen_tcp:option_name() had a duplicate pktoptions value.

  Own Id: OTP-17277

- Fixed removal of empty groups from internal state in `pg`.

  Own Id: OTP-17286 Aux Id: PR-4619

- `erl -remsh` now prints an error message when it fails to connect to the
  remote node.

  Own Id: OTP-17287 Aux Id: PR-4581

- Fix bugs related to corrupt shell history files.

  Error messages printed by shell history are now logged as logger error reports
  instead of written to standard error.

  Own Id: OTP-17288 Aux Id: PR-4581

- A logger warning is now issues when too many arguments are given to `-name` or
  `-sname`. Example: `erl -name a b`.

  Own Id: OTP-17315 Aux Id: GH-4626

- The cache used by `inet_res` now, again, can handle multiple IP addresses per
  domain name, and thus fixes a bug introduced in PR-3041 (OTP-13126) and
  PR-2891 (OTP-14485).

  Own Id: OTP-17344 Aux Id: PR-4633, GH-4631, OTP-14485, OTP-12136

- Sockets created with socket:accept not counted (socket:info/0).

  Own Id: OTP-17372

- The `{fd, Fd}` option to `gen_tcp:listen/2` did not work for `inet_backend`
  `socket`, which has been fixed.

  Own Id: OTP-17374 Aux Id: PR-4787, GH-4680, PR-2989, OTP-17216

### Improvements and New Features

- The cache used by the DNS resolver `inet_res` has been improved to use ETS
  lookups instead of server calls. This is a considerable speed improvement for
  cache hits.

  Own Id: OTP-13126 Aux Id: PR-3041

- The cache ETS table type for the internal DNS resolver `inet_res` has changed
  type (internally) to get better speed and atomicity.

  Own Id: OTP-14485 Aux Id: PR-2891

- The experimental `socket` module can now use any protocol (by name) the OS
  supports. Suggested in PR-2641, implemented in PR-2670.

  Own Id: OTP-14601 Aux Id: PR-2641, PR-2670, OTP-16749

- The DNS resolver `inet_res` has been updated to support CAA (RFC 6844) and URI
  (RFC 7553) records.

  Own Id: OTP-16517 Aux Id: PR-2827

- A compatibility adaptor for `gen_tcp` to use the new `socket` API has been
  implemented (`gen_tcp_socket`). Used when setting the kernel application
  variable `inet_backend = socket`.

  Own Id: OTP-16611 Aux Id: OTP-16749

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

- The file server can now be bypassed in `file:delete/1,2` with the `raw`
  option.

  Own Id: OTP-16698 Aux Id: PR-2634

- An example implementation of Erlang distribution over UDS using distribution
  processes has been introduced.

  Thanks to Jérôme de Bretagne

  Own Id: OTP-16703 Aux Id: PR-2620

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

- New function `os:env/0` returns all OS environment variables as a list of
  2-tuples.

  Own Id: OTP-16793 Aux Id: ERL-1332, PR-2740

- Remove the support for distributed disk logs. The new function
  `disk_log:all/0` is to be used instead of `disk_log:accessible_logs/0`. The
  function `disk_log:close/1` is to be used instead of `disk_log:lclose/1,2`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16811

- Expand the spec for `erl_epmd:listen_port_please/2` to mirror
  `erl_epmd:port_please/2`.

  Own Id: OTP-16947 Aux Id: PR-2781

- A new erl parameter for specifying a file descriptor with configuration data
  has been added. This makes it possible to pass the parameter "-configfd FD"
  when executing the erl command. When this option is given, the system will try
  to read and parse configuration parameters from the file descriptor.

  Own Id: OTP-16952

- The experimental HiPE application has been removed, together with all related
  functionality in other applications.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16963

- The `pg2` module has been removed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16968

- Accept references up to a size of 160-bits from remote nodes. This is the
  first step in an upgrade path toward using references up to 160-bits in a
  future OTP release.

  Own Id: OTP-17005 Aux Id: OTP-16718

- Allow utf-8 binaries as parts of logger_formatter template.

  Own Id: OTP-17015

- Let `disk_log:open/1` change the size if a wrap log is opened for the first
  time, that is, the disk log process does not exist, and the value of option
  `size` does not match the current size of the disk log.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17062 Aux Id: ERL-1418, GH-4469, ERIERL-537

- Allow the shell history of an erlang node to be fetched and stores using a
  custom callback module. See `shell_history` configuration parameter in the
  [kernel documentation](kernel_app.md) for more details.

  Own Id: OTP-17103 Aux Id: PR-2949

- The simple logger (used to log events that happen before kernel has been
  started) has been improved to print prettier error messages.

  Own Id: OTP-17106 Aux Id: PR-2885

- `socket:sendfile/2,3,4,5` has been implemented, for platforms that support the
  underlying socket library call.

  Own Id: OTP-17154 Aux Id: OTP-16749

- Add socket monitor(s) for all types sockets.

  Own Id: OTP-17155

- Fix various issues with the gen_tcp_socket. Including documenting some
  incompatibilities.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17156

- inet:i/0 now also shows existing gen_tcp compatibility sockets (based on
  'socket').

  Own Id: OTP-17157

- Added support in logger for setting primary metadata. The primary metadata is
  passed as a base metadata to all log events in the system. See
  [Metadata](logger_chapter.md#metadata) in the Logger chapter of the Kernel
  User's Guide for more details.

  Own Id: OTP-17181 Aux Id: PR-2457

- Recognize new key 'optional_applications' in application resource files.

  Own Id: OTP-17189 Aux Id: PR-2675

- The `Fun`'s passed to logger:log/2,3,4 can now return metadata that will only
  be fetched when needed. See [`logger:log/2,3,4`](`logger:log/2`) for more
  details.

  Own Id: OTP-17198 Aux Id: PR-2721

- `erpc:multicall()` has been rewritten to be able to utilize the newly
  introduced and improved selective receive optimization.

  Own Id: OTP-17201 Aux Id: PR-4534

- Add utility fiunction inet:info/1 to provide miscellaneous info about a
  socket.

  Own Id: OTP-17203 Aux Id: OTP-17156

- The behaviour for `gen_tcp:connect/3,4` has been changed to not per default
  bind to an address, which allows the network stack to delay the address and
  port selection to when the remote address is known. This allows better port
  re-use, and thus enables far more outgoing connections, since the ephemeral
  port range no longer has to be a hard limit.

  There is a theoretical possibility that this behaviour change can affect the
  set of possible error values, or have other small implications on some
  platforms.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17216 Aux Id: PR-2989

- An option `{nxdomain_reply, boolean()}` has been implemented in the DNS
  resolver `inet_res`. It is useful since an `nxdomain` error from a name server
  does contain the SOA record if the domain exists at all. This record is useful
  to determine a TTL for negative caching of the failed entry.

  Own Id: OTP-17266 Aux Id: PR-4564

- Optimized lookup of local processes part of groups in `pg`.

  Own Id: OTP-17284 Aux Id: PR-4615

- The return values from module `socket` functions `send()`, `sendto()`,
  `sendmsg()`, `sendfile()` and `recv()` has been changed to return a tuple
  tagged with `select` when a `SelectInfo` was returned, and not sometimes
  tagged with `ok`.

  This is a backwards incompatible change that improves usability for code using
  asynchronous operations.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17355 Aux Id: OTP-17154

- Fixed warnings in code matching on underscore prefixed variables.

  Own Id: OTP-17385 Aux Id: OTP-17123

## Kernel 7.3.1.7

### Improvements and New Features

- A new function `global:disconnect/0` has been introduced with which one can
  cleanly disconnect a node from all other nodes in a cluster of `global` nodes.

  Own Id: OTP-18232 Aux Id: OTP-17843, PR-6264

## Kernel 7.3.1.6

### Fixed Bugs and Malfunctions

- A call to [`net_kernel:setopts(new, Opts)`](`net_kernel:setopts/2`) at the
  same time as a connection was being set up could cause a deadlock between the
  `net_kernel` process and the process setting up the connection.

  Own Id: OTP-18198 Aux Id: GH-6129, PR-6216

## Kernel 7.3.1.5

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
  [`prevent_overlapping_partitions`](kernel_app.md#prevent_overlapping_partitions)
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

- Fix failed accepted connection setup after previous established connection
  from same node closed down silently.

  Own Id: OTP-17979 Aux Id: ERIERL-780

### Improvements and New Features

- A [`net_tickintensity`](kernel_app.md#net_tickintensity) `kernel` parameter
  has been introduced. It can be used to control the amount of ticks during a
  [`net_ticktime`](kernel_app.md#net_ticktime) period.

  A new `net_kernel:start/2` function has also been introduced in order to make
  it easier to add new options. The use of `net_kernel:start/1` has been
  deprecated.

  Own Id: OTP-17905 Aux Id: ERIERL-732, PR-5740

## Kernel 7.3.1.4

### Fixed Bugs and Malfunctions

- Parsing of the result value in the native DNS resolver has been made more
  defensive against incorrect results.

  Own Id: OTP-17578 Aux Id: ERIERL-683

## Kernel 7.3.1.3

### Fixed Bugs and Malfunctions

- Fix code:get_doc/1 to not crash when module is located in an escript.

  Own Id: OTP-17570 Aux Id: PR-5139 GH-4256 ERL-1261

## Kernel 7.3.1.2

### Fixed Bugs and Malfunctions

- Handling of the "." domain as a search domain was incorrect and caused a crash
  in the DNS resolver `inet_res`, which has now been fixed.

  Own Id: OTP-17473 Aux Id: GH-4838, OTP-17439

- Fixed rare bug that could cause net_kernel process to hang for ever. Have seen
  to happen with massive number of TLS connections while remote nodes are
  restarting. Bug exists since OTP-22.0.

  Own Id: OTP-17476 Aux Id: GH-4931, PR-4934

## Kernel 7.3.1.1

### Fixed Bugs and Malfunctions

- Fix a race condition in Global.

  Own Id: OTP-16033 Aux Id: ERIERL-329, ERL-1414, GH-4448, ERL-885, GH-3923

## Kernel 7.3.1

### Fixed Bugs and Malfunctions

- A bug in the Erlang DNS resolver has been fixed, where it could be made to
  bring down the kernel supervisor and thereby the whole node, when getting an
  incorrect (IN A reply to an IN CNAME query) reply from the DNS server and used
  the reply record's value without verifying its type.

  Own Id: OTP-17361

## Kernel 7.3

### Fixed Bugs and Malfunctions

- The range check for compression pointers in DNS encoding was faulty, which
  caused incorrect label compression encoding for very large DNS messages;
  larger than about 16 kBytes, such as AXFR responses. This more than 11 year
  old bug has now been corrected.

  Own Id: OTP-13641 Aux Id: PR-2959

- Fix of internal links in the `erpc` documentation.

  Own Id: OTP-17202 Aux Id: PR-4516

- Fix bug where complex seq_trace tokens (that is lists, tuples, maps etc) could
  becomes corrupted by the GC. The bug was introduced in OTP-21.

  Own Id: OTP-17209 Aux Id: PR-3039

- When running Xref in the `modules` mode, the Debugger application would show
  up as a dependency for the Kernel applications.

  Own Id: OTP-17223 Aux Id: GH-4546, PR-4554

### Improvements and New Features

- `m:erl_epmd` (the epmd client) will now try to reconnect to the local EPMD if
  the connection is broken.

  Own Id: OTP-17178 Aux Id: PR-3003

## Kernel 7.2.1

### Fixed Bugs and Malfunctions

- When using the DNS resolver option `servfail_retry_timeout` it did not honour
  the overall call time-out in e.g `inet_res:getbyname/3`. This misbehaviour has
  now been fixed. Also, the `servfail_retry_timeout` behaviour has been improved
  to only be enforced for servers that gives a servfail answer.

  Own Id: OTP-12960 Aux Id: ERIERL-598, PR-4509

## Kernel 7.2

### Fixed Bugs and Malfunctions

- The `apply` call's in `logger.hrl` are now called with `erlang` prefix to
  avoid clashed with local [`apply/3`](`apply/3`) functions.

  Own Id: OTP-16976 Aux Id: PR-2807

- Fix memory leak in `pg`.

  Own Id: OTP-17034 Aux Id: PR-2866

- Fix crash in `logger_proxy` due to stray `gen_server:call` replies not being
  handled. The stray replies come when logger is under heavy load and the flow
  control mechanism is reaching its limit.

  Own Id: OTP-17038

- Fixed a bug in `erl_epmd:names()` that caused it to return the illegal return
  value `noport` instead of `{error, Reason}` where `Reason` is the actual error
  reason. This bug also propagated to `net_adm:names()`.

  This bug was introduced in `kernel` version 7.1 (OTP 23.1).

  Own Id: OTP-17054 Aux Id: ERL-1424

### Improvements and New Features

- Add export of some resolver documented types.

  Own Id: OTP-16954 Aux Id: ERIERL-544

- Add configurable retry timeout for resolver lookups.

  Own Id: OTP-16956 Aux Id: ERIERL-547

- `gen_server:multi_call()` has been optimized in the special case of only
  calling the local node with timeout set to `infinity`.

  Own Id: OTP-17058 Aux Id: PR-2887

## Kernel 7.1

### Fixed Bugs and Malfunctions

- A fallback has been implemented for file:sendfile when using inet_backend
  socket

  Own Id: OTP-15187 Aux Id: ERL-1293

- Make default TCP distribution honour option `backlog` in
  `inet_dist_listen_options`.

  Own Id: OTP-16694 Aux Id: PR-2625

- Raw option handling for the experimental `gen_tcp_socket` backend was broken
  so that all raw options were ignored by for example `gen_tcp:listen/2`, a bug
  that now has been fixed. Reported by Jan Uhlig.

  Own Id: OTP-16743 Aux Id: ERL-1287

- Accept fails with inet-backend socket.

  Own Id: OTP-16748 Aux Id: ERL-1284

- Fixed various minor errors in the socket backend of gen_tcp.

  Own Id: OTP-16754

- Correct `disk_log:truncate/1` to count the header. Also correct the
  documentation to state that `disk_log:truncate/1` can be used with external
  disk logs.

  Own Id: OTP-16768 Aux Id: ERL-1312

- Fix erl_epmd:port_please/2,3 type specs to include all possible error values.

  Own Id: OTP-16783

- Fix `erl -erl_epmd_port` to work properly. Before this fix it did not work at
  all.

  Own Id: OTP-16785

- Fix typespec for internal function `erlang:seq_trace_info/1` to allow
  `t:term/0` as returned label. This in turn fixes so that calls to
  `seq_trace:get_token/1` can be correctly analyzer by dialyzer.

  Own Id: OTP-16823 Aux Id: PR-2722

- Fix erroneous double registration of processes in `pg` when distribution is
  dynamically started.

  Own Id: OTP-16832 Aux Id: PR-2738

### Improvements and New Features

- Make (use of) the socket registry optional (still enabled by default). Its now
  possible to build OTP with the socket registry turned off, turn it off by
  setting an environment variable and controlling in runtime (via function calls
  and arguments when creating sockets).

  Own Id: OTP-16763

- `erl -remsh nodename` no longer requires the hostname to be given when used
  together with dynamic nodenames.

  Own Id: OTP-16784

## Kernel 7.0

### Fixed Bugs and Malfunctions

- Fix race condition during shutdown when `shell_history` is enabled. The race
  condition would trigger crashes in `disk_log`.

  Own Id: OTP-16008 Aux Id: PR-2302

- Fix the Erlang distribution to handle the scenario when a node connects that
  can handle message fragmentation but can not handle the atom cache. This bug
  only affects users that have implemented a custom distribution carrier. It has
  been present since OTP-21.

  The `DFLAG_FRAGMENT` distribution flag was added to the set of flags that can
  be rejected by a distribution implementation.

  Own Id: OTP-16284

- Fix bug where a binary was not allowed to be the format string in calls to
  `logger:log`.

  Own Id: OTP-16395 Aux Id: PR-2444

- Fix bug where `logger` would end up in an infinite loop when trying to log the
  crash of a handler or formatter.

  Own Id: OTP-16489 Aux Id: ERL-1134

- `code:lib_dir/1` has been fixed to also return the lib dir for `erts`.

  This is been marked as an incompatibility for any application that depended on
  `{error,bad_name}` to be returned for `erts`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16502

- The application `stop/1` callback was not called if the application master of
  the application terminated.

  Own Id: OTP-16504 Aux Id: PR-2328

- Fix bug in `application:loaded_applications/0` that could cause it to fail
  with `badarg` if for example a concurrent upgrade/downgrade is running.

  Own Id: OTP-16627 Aux Id: PR-2601

### Improvements and New Features

- A new module `m:erpc` has been introduced in the `kernel` application. The
  `erpc` module implements an enhanced subset of the operations provided by the
  `m:rpc` module. Enhanced in the sense that it makes it possible to distinguish
  between returned value, raised exceptions, and other errors. `erpc` also has
  better performance and scalability than the original `rpc` implementation.
  This by utilizing the newly introduced
  [`spawn_request()`](`erlang:spawn_request/5`) BIF. Also the `rpc` module
  benefits from these improvements by utilizing `erpc` when it is possible.

  This change has been marked as a potential incompatibility since
  [`rpc:block_call()`](`rpc:block_call/5`) now only is guaranteed to block other
  `block_call()` operations. The documentation previously claimed that it would
  block all `rpc` operations. This has however never been the case. It
  previously did not block node-local `block_call()` operations.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13450 Aux Id: OTP-15251

- A client node can receive its node name dynamically from the node that it
  first connects to. This featured can by used by

  - starting with `erl -sname undefined`
  - erl_interface functions `ei_connect_init` and friends
  - `erl_call -R`

  Own Id: OTP-13812

- Improved the printout of single line logger events for most of the OTP
  behaviours in STDLIB and Kernel. This includes `proc_lib`, `gen_server`,
  `gen_event`, `gen_statem`, `gen_fsm`, `supervisor`, `supervisor_bridge` and
  `application`.

  Improved the [`chars_limit`](`m:logger_formatter#chars_limit`) and
  [`depth`](`m:logger_formatter#depth`) handling in `proc_lib` and when
  formatting of exceptions.

  Own Id: OTP-15299

- Remove usage and documentation of old requests of the I/O-protocol.

  Own Id: OTP-15695

- Directories can now be opened by `file:open/2` when passing the `directory`
  option.

  Own Id: OTP-15835 Aux Id: PR-2212

- The check of whether to log or not based on the log level in `logger` has been
  optimized by using `persistent_term` to store the log level.

  Own Id: OTP-15948 Aux Id: PR-2356

- `file:read_file_info/2` can now be used on opened files and directories.

  Own Id: OTP-15956 Aux Id: PR-2231

- The `-config` option to `erl` now can take multiple config files without
  repeating the `-config` option. Example:

  erl -config sys local

  Own Id: OTP-16148 Aux Id: PR-2373

- Improved node connection setup handshake protocol. Made possible to agree on
  protocol version without dependence on `epmd` or other prior knowledge of peer
  node version. Also added exchange of node incarnation ("creation") values and
  expanded the distribution capability flag field from 32 to 64 bits.

  Own Id: OTP-16229

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

- A first EXPERIMENTAL module that is a `socket` backend to `gen_tcp` and `inet`
  has been implemented. Others will follow. Feedback will be appreciated.

  Own Id: OTP-16260 Aux Id: OTP-15403

- The new experimental `socket` module has been moved to the Kernel application.

  Own Id: OTP-16312

- Replace usage of deprecated function in the `group` module.

  Own Id: OTP-16345

- Minor updates due to the new spawn improvements made.

  Own Id: OTP-16368 Aux Id: OTP-15251

- Update of [sequential tracing](`m:seq_trace#whatis`) to also support other
  information transfers than message passing.

  Own Id: OTP-16370 Aux Id: OTP-15251, OTP-15232

- `code:module_status/1` now accepts a list of modules. `code:module_status/0`,
  which returns the statuses for all loaded modules, has been added.

  Own Id: OTP-16402

- `filelib:wildcard/1,2` is now twice as fast when a double star (`**`) is part
  of the pattern.

  Own Id: OTP-16419

- A new implementation of distributed named process groups has been introduced.
  It is available in the `m:pg` module.

  Note that this `pg` module only has the name in common with the experimental
  `pg` module that was present in `stdlib` up until OTP 17.

  Thanks to Maxim Fedorov for the implementation.

  Own Id: OTP-16453 Aux Id: PR-2524

- The `pg2` module has been deprecated. It has also been scheduled for removal
  in OTP 24.

  You are advised to replace the usage of `pg2` with the newly introduced `m:pg`
  module. `pg` has a similar API, but with a more scalable implementation.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16455

- Refactored the internal handling of deprecated and removed functions.

  Own Id: OTP-16469

- The internal hosts file resolver cache `inet_hosts` has been rewritten to
  behave better when the hosts file changes. For example the cache is updated
  per entry instead of cleared and reloaded so lookups do not temporarily fail
  during reloading, and; when multiple processes simultaneously request reload
  these are now folded into one instead of all done in sequence. Reported and
  first solution suggestion by Maxim Fedorov.

  Own Id: OTP-16487 Aux Id: PR-2516

- Add `code:all_available/0` that can be used to get all available modules.

  Own Id: OTP-16494

- As of OTP 23, the distributed `m:disk_log` feature has been deprecated. It has
  also been scheduled for removal in OTP 24.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16495

- Add the function `code:fetch_docs/1` for fetching embedded documentation for
  aa Erlang module.

  Own Id: OTP-16499

- Improve configure for the net nif, which should increase portability.

  Own Id: OTP-16530 Aux Id: OTP-16464

- socket: Socket counters and socket global counters are now represented as maps
  (instead of property lists).

  Own Id: OTP-16535

- The experimental socket module has gotten restrictions removed so now the
  'seqpacket' socket type should work for any communication domain (protocol
  family) where the OS supports it, typically the Unix Domain.

  Own Id: OTP-16550 Aux Id: ERIERL-476

- Allow using custom IO devices in `logger_std_h`.

  Own Id: OTP-16563 Aux Id: PR-2523

- Added `file:del_dir_r/1` which deletes a directory together with all of its
  contents, similar to `rm -rf` on Unix systems.

  Own Id: OTP-16570 Aux Id: PR-2565

- socket: By default the socket options rcvtimeo and sndtimeo are now disabled.
  To enable these, OTP now has to be built with the configure option
  --enable-esock-rcvsndtimeo

  Own Id: OTP-16620

- The experimental gen_tcp compatibility code utilizing the socket module could
  loose buffered data when receiving a specified number of bytes. This bug has
  been fixed. Reported by Maksim Lapshin on bugs.erlang.org ERL-1234

  Own Id: OTP-16632 Aux Id: ERL-1234

## Kernel 6.5.2.5

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

### Improvements and New Features

- A `net_tickintensity` `kernel` parameter has been introduced. It can be used
  to control the amount of ticks during a `net_ticktime` period.

  A new `net_kernel:start/2` function has also been introduced in order to make
  it easier to add new options. The use of `net_kernel:start/1` has been
  deprecated.

  Own Id: OTP-17905 Aux Id: ERIERL-732, PR-5740

## Kernel 6.5.2.4

### Fixed Bugs and Malfunctions

- Fixed rare bug that could cause net_kernel process to hang for ever. Have seen
  to happen with massive number of TLS connections while remote nodes are
  restarting. Bug exists since OTP-22.0.

  Own Id: OTP-17476 Aux Id: GH-4931, PR-4934

## Kernel 6.5.2.3

### Fixed Bugs and Malfunctions

- Fix a race condition in Global.

  Own Id: OTP-16033 Aux Id: ERIERL-329, ERL-1414, GH-4448, ERL-885, GH-3923

## Kernel 6.5.2.2

### Fixed Bugs and Malfunctions

- When running Xref in the `modules` mode, the Debugger application would show
  up as a dependency for the Kernel applications.

  Own Id: OTP-17223 Aux Id: GH-4546, PR-4554

## Kernel 6.5.2.1

### Fixed Bugs and Malfunctions

- Fix bug in `application:loaded_applications/0` that could cause it to fail
  with `badarg` if for example a concurrent upgrade/downgrade is running.

  Own Id: OTP-16627 Aux Id: PR-2601

## Kernel 6.5.2

### Fixed Bugs and Malfunctions

- The DNS resolver \`inet_res\` has been fixed to return the last intermediate
  error when subsequent requests times out.

  Own Id: OTP-16414 Aux Id: ERIERL-452

- The prim_net nif (net/kernel) made use of an undefined atom, notsup. This has
  now been corrected.

  Own Id: OTP-16440

- Fix a crash when attempting to log faults when loading files during early
  boot.

  Own Id: OTP-16491

- Fix crash in logger when logging to a remote node during boot.

  Own Id: OTP-16493 Aux Id: ERIERL-459

### Improvements and New Features

- Improved `net_kernel` debug functionality.

  Own Id: OTP-16458 Aux Id: PR-2525

## Kernel 6.5.1

### Fixed Bugs and Malfunctions

- The 'socket state' info provided by the inet info function has been improved

  Own Id: OTP-16043 Aux Id: ERL-1036

- Fix bug where `logger` would crash when starting when a very large log file
  needed to be rotated and compressed.

  Own Id: OTP-16145 Aux Id: ERL-1034

- Fixed a bug causing actual nodedown reason reported by
  [`net_kernel:monitor_nodes(true, [nodedown_reason])`](`net_kernel:monitor_nodes/2`)
  to be lost and replaced by the reason `killed`.

  Own Id: OTP-16216

- The documentation for `rpc:call/4,5/` has been updated to describe what
  happens when the called function throws or return an `'EXIT'` tuple.

  Own Id: OTP-16279 Aux Id: ERL-1066

## Kernel 6.5

### Fixed Bugs and Malfunctions

- The type specification for `gen_sctp:connect/4,5` has been corrected.

  Own Id: OTP-15344 Aux Id: ERL-947

- Extra `-mode` flags given to `erl` are ignored with a warning.

  Own Id: OTP-15852

- Fix type spec for `seq_trace:set_token/2`.

  Own Id: OTP-15858 Aux Id: ERL-700

- `logger:compare_levels/2` would fail with a `badarg` exception if given the
  values `all` or `none` as any of the parameters. This is now corrected.

  Own Id: OTP-15942 Aux Id: PR-2301

- Fix bug where the log file in `logger_std_h` would not be closed when the
  inode of the file changed. This would in turn cause a file descriptor leak
  when tools like logrotate are used.

  Own Id: OTP-15997 Aux Id: PR-2331

- Fix a race condition in the debugging function `net_kernel:nodes_info/0`.

  Own Id: OTP-16022

- Fix race condition when closing a file opened in `compressed` or
  `delayed_write` mode.

  Own Id: OTP-16023

### Improvements and New Features

- The possibility to send ancillary data, in particular the TOS field, has been
  added to `gen_udp:send/4,5`.

  Own Id: OTP-15747 Aux Id: ERIERL-294

- If the log file was given with relative path, the standard logger handler
  (`logger_std_h`) would store the file name with relative path. If the current
  directory of the node was later changed, a new file would be created relative
  the new current directory, potentially failing with an `enoent` if the new
  directory did not exist. This is now corrected and `logger_std_h` always
  stores the log file name as an absolute path, calculated from the current
  directory at the time of the handler startup.

  Own Id: OTP-15850

- Support local sockets with inet:i/0.

  Own Id: OTP-15935 Aux Id: PR-2299

## Kernel 6.4.1

### Fixed Bugs and Malfunctions

- `user`/`user_drv` could respond to io requests before they had been processed,
  which could cause data to be dropped if the emulator was halted soon after a
  call to `io:format/2`, such as in an escript.

  Own Id: OTP-15805

## Kernel 6.4

### Fixed Bugs and Malfunctions

- Fix so that when multiple `-sname` or `-name` are given to `erl` the first one
  is chosen. Before this fix distribution was not started at all when multiple
  name options were given.

  Own Id: OTP-15786 Aux Id: ERL-918

- Fix `inet_res` configuration pointing to non-existing files to work again.
  This was broken in KERNEL-6.3 (OTP-21.3).

  Own Id: OTP-15806

### Improvements and New Features

- A simple socket API is provided through the socket module. This is a low level
  API that does _not_ replace gen\_\[tcp|udp|sctp]. It is intended to
  _eventually_ replace the inet driver, but not the high level gen-modules
  (gen_tcp, gen_udp and gen_sctp). It also provides a basic API that facilitates
  the implementation of other protocols, that is TCP, UDP and SCTP.

  Known issues are; No support for the Windows OS (currently).

  Own Id: OTP-14831

- Improved the documentation for the linger option.

  Own Id: OTP-15491 Aux Id: PR-2019

- Global no longer tries more than once when connecting to other nodes.

  Own Id: OTP-15607 Aux Id: ERIERL-280

- The dist messages EXIT, EXIT2 and MONITOR_DOWN have been updated with new
  versions that send the reason term as part of the payload of the message
  instead of as part of the control message.

  The old versions are still present and can be used when communicating with
  nodes that don't support the new versions.

  Own Id: OTP-15611

- Kernel configuration parameter `start_distribution = boolean()` is added. If
  set to `false`, the system is started with all distribution functionality
  disabled. Defaults to `true`.

  Own Id: OTP-15668 Aux Id: PR-2088

- In OTP-21.3, a warning was introduced for duplicated applications/keys in
  configuration. This warning would be displayed both when the configuration was
  given as a file on system start, and during runtime via
  `application:set_env/1,2`.

  The warning is now changed to a `badarg` exception in
  `application:set_env/1,2`. If the faulty configuration is given in a
  configuration file on system start, the startup will fail.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15692 Aux Id: PR-2170

## Kernel 6.3.1.3

### Fixed Bugs and Malfunctions

- Fix bug where the log file in `logger_std_h` would not be closed when the
  inode of the file changed. This would in turn cause a file descriptor leak
  when tools like logrotate are used.

  Own Id: OTP-15997 Aux Id: PR-2331

## Kernel 6.3.1.2

### Improvements and New Features

- The possibility to send ancillary data, in particular the TOS field, has been
  added to `gen_udp:send/4,5`.

  Own Id: OTP-15747 Aux Id: ERIERL-294

## Kernel 6.3.1.1

### Fixed Bugs and Malfunctions

- Fix type spec for `seq_trace:set_token/2`.

  Own Id: OTP-15858 Aux Id: ERL-700

## Kernel 6.3.1

### Fixed Bugs and Malfunctions

- Fixed a performance regression when reading files opened with the `compressed`
  flag.

  Own Id: OTP-15706 Aux Id: ERIERL-336

## Kernel 6.3

### Fixed Bugs and Malfunctions

- If for example the `/etc/hosts` did not come into existence until after the
  kernel application had started, its content was never read. This bug has now
  been corrected.

  Own Id: OTP-14702 Aux Id: PR-2066

- Fix bug where doing `seq_trace:reset_trace()` while another process was doing
  a garbage collection could cause the run-time system to segfault.

  Own Id: OTP-15490

- Fix `erl_epmd:port_please` spec to include `t:atom/0` and `t:string/0`.

  Own Id: OTP-15557 Aux Id: PR-2117

- The Logger handler logger_std_h now keeps track of the inode of its log file
  in order to re-open the file if the inode changes. This may happen, for
  instance, if the log file is opened and saved by an editor.

  Own Id: OTP-15578 Aux Id: ERL-850

- When user specific file modes are given to the logger handler `logger_std_h`,
  they were earlier accepted without any control. This is now changes, so Logger
  will adjust the file modes as follows:

  \- If `raw` is not found in the list, it is added.  
  \- If none of `write`, `append` or `exclusive` are found in the list, `append`
  is added.  
  \- If none of `delayed_write` or `{delayed_write,Size,Delay}` are found in the
  list, `delayed_write` is added.

  Own Id: OTP-15602

### Improvements and New Features

- The standard logger handler, `logger_std_h`, now has a new internal feature
  for log rotation. The rotation scheme is as follows:

  The log file to which the handler currently writes always has the same name,
  i.e. the name which is configured for the handler. The archived files have the
  same name, but with extension ".N", where N is an integer. The newest archive
  has extension ".0", and the oldest archive has the highest number.

  The size at which the log file is rotated, and the number of archive files
  that are kept, is specified with the handler specific configuration parameters
  `max_no_bytes` and `max_no_files` respectively.

  Archives can be compressed, in which case they get a ".gz" file extension
  after the integer. Compression is specified with the handler specific
  configuration parameter `compress_on_rotate`.

  Own Id: OTP-15479

- The new functions `logger:i/0` and `logger:i/1` are added. These provide the
  same information as `logger:get_config/0` and other `logger:get_*_config`
  functions, but the information is more logically sorted and more readable.

  Own Id: OTP-15600

- Logger is now protected against overload due to massive amounts of log events
  from the emulator or from remote nodes.

  Own Id: OTP-15601

- Logger now uses os:system_time/1 instead of erlang:system_time/1 to generate
  log event timestamps.

  Own Id: OTP-15625

- Add functions `application:set_env/1,2` and `application:set_env/2`. These
  take a list of application configuration parameters, and the behaviour is
  equivalent to calling `application:set_env/4` individually for each
  application/key combination, except it is more efficient.

  `set_env/1,2` warns about duplicated applications or keys. The warning is also
  emitted during boot, if applications or keys are duplicated within one
  configuration file, e.g. sys.config.

  Own Id: OTP-15642 Aux Id: PR-2164

- Handler specific configuration parameters for the standard handler
  `logger_std_h` are changed to be more intuitive and more similar to the
  disk_log handler.

  Earlier there was only one parameter, `type`, which could have the values
  `standard_io`, `standard_error`, `{file,FileName}` or `{file,FileName,Modes}`.

  This is now changed, so the following parameters are allowed:

  `type = standard_io | standard_error | file`  
  `file = file:filename()`  
  `modes = [file:mode()]`

  All parameters are optional. `type` defaults to `standard_io`, unless a file
  name is given, in which case it defaults to `file`. If `type` is set to
  `file`, the file name defaults to the same as the handler id.

  The potential incompatibility is that `logger:get_config/0` and
  `logger:get_handler_config/1` now returns the new parameters, even if the
  configuration was set with the old variant, e.g. `#{type=>{file,FileName}}`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15662

- The new configuration parameter `file_check` is added to the Logger handler
  `logger_std_h`. This parameter specifies how long (in milliseconds) the
  handler may wait before checking if the log file still exists and the inode is
  the same as when it was opened.

  The default value is 0, which means that this check is done prior to each
  write operation. Setting a higher number may improve performance, but adds the
  risk of losing log events.

  Own Id: OTP-15663

## Kernel 6.2.1

### Fixed Bugs and Malfunctions

- Setting the `recbuf` size of an inet socket the `buffer` is also automatically
  increased. Fix a bug where the auto adjustment of inet buffer size would be
  triggered even if an explicit inet buffer size had already been set.

  Own Id: OTP-15651 Aux Id: ERIERL-304

## Kernel 6.2

### Fixed Bugs and Malfunctions

- A new function, `logger:update_handler_config/3` is added, and the handler
  callback `changing_config` now has a new argument, `SetOrUpdate`, which
  indicates if the configuration change comes from `set_handler_config/2,3` or
  `update_handler_config/2,3`.

  This allows the handler to consistently merge the new configuration with the
  old (if the change comes from `update_handler_config/2,3`) or with the default
  (if the change comes from `set_handler_config/2,3`).

  The built-in handlers `logger_std_h` and `logger_disk_log_h` are updated
  accordingly. A bug which could cause inconsistency between the handlers'
  internal state and the stored configuration is also corrected.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15364

- Fix fallback when custom erl_epmd client does not implement address_please.

  Own Id: OTP-15388 Aux Id: PR-1983

- The logger ets table did not have the `read_concurrency` option. This is now
  added.

  Own Id: OTP-15453 Aux Id: ERL-782

- During system start, logger has a simple handler which prints to stdout. After
  the kernel supervision is started, this handler is removed and replaced by the
  default handler. Due to a bug, logger earlier issued a debug printout saying
  it received an unexpected message, which was the EXIT message from the simple
  handler's process. This is now corrected. The simple handler's process now
  unlinks from the logger process before terminating.

  Own Id: OTP-15466 Aux Id: ERL-788

- The logger handler `logger_std_h` would not re-create it's log file if it was
  removed. Due to this it could not be used with tools like 'logrotate'. This is
  now corrected.

  Own Id: OTP-15469

### Improvements and New Features

- A function `inet:getifaddrs/1` that takes a list with a namespace option has
  been added, for platforms that support that feature, for example Linux
  (only?).

  Own Id: OTP-15121 Aux Id: ERIERL-189, PR-1974

- Added the `nopush` option for TCP sockets, which corresponds to `TCP_NOPUSH`
  on \*BSD and `TCP_CORK` on Linux.

  This is also used internally in `file:sendfile` to reduce latency on
  subsequent send operations.

  Own Id: OTP-15357 Aux Id: ERL-698

- Optimize handling of send_delay for tcp sockes to better work with the new
  pollthread implementation introduced in OTP-21.

  Own Id: OTP-15471 Aux Id: ERIERL-229

## Kernel 6.1.1

### Fixed Bugs and Malfunctions

- Fix bug causing net_kernel process crash on connection attempt from node with
  name identical to local node.

  Own Id: OTP-15438 Aux Id: ERL-781

## Kernel 6.1

### Fixed Bugs and Malfunctions

- The values `all` and `none` are documented as valid value for the Kernel
  configuration parameter `logger_level`, but would cause a crash during node
  start. This is now corrected.

  Own Id: OTP-15143

- Fix some potential buggy behavior in how ticks are sent on inter node
  distribution connections. Tick is now sent to c-node even if there are unsent
  buffered data, as c-nodes need ticks in order to send reply ticks. The amount
  of sent data was also calculated wrongly when ticks were suppressed due to
  unsent buffered data.

  Own Id: OTP-15162 Aux Id: ERIERL-191

- Non semantic change in dist_util.erl to silence dialyzer warning.

  Own Id: OTP-15170

- Fixed `net_kernel:connect_node(node())` to return `true` (and do nothing) as
  it always has before OTP-21.0. Also documented this successful "self connect"
  as the expected behavior.

  Own Id: OTP-15182 Aux Id: ERL-643

- The single_line option on logger_formatter would in some cases add an unwanted
  comma after the association arrows in a map. This is now corrected.

  Own Id: OTP-15228

- Improved robustness of distribution connection setup. In OTP-21.0 a truly
  asynchronous connection setup was introduced. This is further improvement on
  that work to make the emulator more robust and also be able to recover in
  cases when involved Erlang processes misbehave.

  Own Id: OTP-15297 Aux Id: OTP-15279, OTP-15280

### Improvements and New Features

- A new macro, `?LOG(Level,...)`, is added. This is equivalent to the existing
  `?LOG_<LEVEL>(...)` macros.

  A new variant of Logger report callback is added, which takes an extra
  argument containing options for size limiting and line breaks. Module
  `proc_lib` in `STDLIB` uses this for crash reports.

  Logger configuration is now checked a bit more for errors.

  Own Id: OTP-15132

- The socket options `recvtos`, `recvttl`, `recvtclass` and `pktoptions` have
  been implemented in the socket modules. See the documentation for the
  `gen_tcp`, `gen_udp` and `inet` modules. Note that support for these in the
  runtime system is platform dependent. Especially for `pktoptions` which is
  very Linux specific and obsoleted by the RFCs that defined it.

  Own Id: OTP-15145 Aux Id: ERIERL-187

- Add `logger:set_application_level/2` for setting the logger level of all
  modules in one application.

  Own Id: OTP-15146

## Kernel 6.0.1

### Fixed Bugs and Malfunctions

- Fixed bug in `net_kernel` that could cause an emulator crash if certain
  connection attempts failed. Bug exists since kernel-6.0 (OTP-21.0).

  Own Id: OTP-15280 Aux Id: ERIERL-226, OTP-15279

## Kernel 6.0

### Fixed Bugs and Malfunctions

- Clarify the documentation of `rpc:multicall/5`.

  Own Id: OTP-10551

- The DNS resolver when getting econnrefused from a server retained an invalid
  socket so look up towards the next server(s) also failed.

  Own Id: OTP-13133 Aux Id: PR-1557

- No resolver backend returns V4Mapped IPv6 addresses any more. This was
  inconsistent before, some did, some did not. To facilitate working with such
  addresses a new function `inet:ipv4_mapped_ipv6_address/1` has been added.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13761 Aux Id: ERL-503

- The type specifications for [`file:posix/0`](`t:file:posix/0`) and
  [`inet:posix/0`](`t:inet:posix/0`) have been updated according to which errors
  file and socket operations should be able to return.

  Own Id: OTP-14019 Aux Id: ERL-550

- Fix name resolving in IPv6 only environments when doing the initial
  distributed connection.

  Own Id: OTP-14501

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

- Fixed connection tick toward primitive hidden nodes (erl_interface) that could
  cause faulty tick timeout in rare cases when payload data is sent to hidden
  node but not received.

  Own Id: OTP-14681

- Make group react immediately on an EXIT-signal from shell in e.g ssh.

  Own Id: OTP-14991 Aux Id: PR1705

- Calls to `gen_tcp:send/2` on closed sockets now returns `{error, closed}`
  instead of `{error,enotconn}`.

  Own Id: OTP-15001

- The `included_applications` key are no longer duplicated as application
  environment variable. Earlier, the included applications could be read both
  with `application:get[_all]_env(...)` and `application:get[_all]_key(...)`
  functions. Now, it can only be read with `application:get[_all]_key(...)`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15071

- Owner and group changes through `file:write_file_info`, `file:change_owner`,
  and `file:change_group` will no longer report success on permission errors.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15118

### Improvements and New Features

- A new logging API is added to Erlang/OTP, see the `m:logger` manual page, and
  section [Logging](logger_chapter.md) in the Kernel User's Guide.

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

- The function `inet:i/0` has been documented.

  Own Id: OTP-13713 Aux Id: PR-1645

- Typespecs for `netns` and `bind_to_device` options have been added to
  `gen_tcp`, `gen_udp` and `gen_sctp` functions.

  Own Id: OTP-14359 Aux Id: PR-1816

- New functionality for implementation of alternative carriers for the Erlang
  distribution has been introduced. This mainly consists of support for usage of
  distribution controller processes (previously only ports could be used as
  distribution controllers). For more information see
  [ERTS User's Guide ➜ How to implement an Alternative Carrier for the Erlang Distribution ➜ Distribution Module](`e:erts:alt_dist.md#distribution-module`).

  Own Id: OTP-14459

- `seq_trace` labels may now be any erlang term.

  Own Id: OTP-14899

- The SSL distribution protocol `-proto inet_tls` has stopped setting the SSL
  option `server_name_indication`. New verify funs for client and server in
  `inet_tls_dist` has been added, not documented yet, that checks node name if
  present in peer certificate. Usage is still also yet to be documented.

  Own Id: OTP-14969 Aux Id: OTP-14465, ERL-598

- Changed timeout of `gen_server` calls to `auth` server from default 5 seconds
  to `infinity`.

  Own Id: OTP-15009 Aux Id: ERL-601

- The callback module passed as `-epmd_module` to erl has been expanded to be
  able to do name and port resolving.

  Documentation has also been added in the `m:erl_epmd` reference manual and
  ERTS User's Guide
  [How to Implement an Alternative Node Discovery for Erlang Distribution](`e:erts:alt_disco.md`).

  Own Id: OTP-15086 Aux Id: PR-1694

- Included config file specified with relative path in sys.config are now first
  searched for relative to the directory of sys.config itself. If not found, it
  is also searched for relative to the current working directory. The latter is
  for backwards compatibility.

  Own Id: OTP-15137 Aux Id: PR-1838

## Kernel 5.4.3.2

### Fixed Bugs and Malfunctions

- Non semantic change in dist_util.erl to silence dialyzer warning.

  Own Id: OTP-15170

## Kernel 5.4.3.1

### Fixed Bugs and Malfunctions

- Fix some potential buggy behavior in how ticks are sent on inter node
  distribution connections. Tick is now sent to c-node even if there are unsent
  buffered data, as c-nodes need ticks in order to send reply ticks. The amount
  of sent data was calculated wrongly when ticks where suppressed due to unsent
  buffered data.

  Own Id: OTP-15162 Aux Id: ERIERL-191

## Kernel 5.4.3

### Fixed Bugs and Malfunctions

- Correct a few contracts.

  Own Id: OTP-14889

- Reject loading modules with names containing directory separators ('/' or '\\'
  on Windows).

  Own Id: OTP-14933 Aux Id: ERL-564, PR-1716

- Fix bug in handling of os:cmd/2 option max_size on windows.

  Own Id: OTP-14940

## Kernel 5.4.2

### Fixed Bugs and Malfunctions

- Add `os:cmd/2` that takes an options map as the second argument.

  Add `max_size` as an option to `os:cmd/2` that control the maximum size of the
  result that `os:cmd/2` will return.

  Own Id: OTP-14823

## Kernel 5.4.1

### Fixed Bugs and Malfunctions

- Refactored an internal API.

  Own Id: OTP-14784

## Kernel 5.4

### Fixed Bugs and Malfunctions

- Processes which did output after switching jobs (Ctrl+G) could be left forever
  stuck in the io request.

  Own Id: OTP-14571 Aux Id: ERL-472

### Improvements and New Features

- Lock counting can now be fully toggled at runtime in the lock counting
  emulator (`-emu_type lcnt`). Everything is enabled by default to match the old
  behavior, but specific categories can be toggled at will with minimal runtime
  overhead when disabled. Refer to the documentation on `lcnt:rt_mask/1` for
  details.

  Own Id: OTP-13170

- `lcnt:collect` and `lcnt:clear` will no longer block all other threads in the
  runtime system.

  Own Id: OTP-14412

- General Unicode improvements.

  Own Id: OTP-14462

## Kernel 5.3.1

### Fixed Bugs and Malfunctions

- The documentation for the 'quiet' option in disk_log:open/1 had an incorrect
  default value.

  Own Id: OTP-14498

## Kernel 5.3

### Fixed Bugs and Malfunctions

- Function `inet:ntoa/1` has been fixed to return lowercase letters according to
  RFC 5935 that has been approved after this function was written. Previously
  uppercase letters were returned so this may be a backwards incompatible change
  depending on how the returned address string is used.

  Function `inet:parse_address/1` has been fixed to accept %-suffixes on scoped
  addresses. The addresses does not work yet, but gives no parse errors.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13006 Aux Id: ERIERL-20, ERL-429

- Fix bug where gethostname would incorrectly fail with enametoolong on Linux.

  Own Id: OTP-14310

- Fix bug causing `code:is_module_native` to falsely return true when `local`
  call trace is enabled for the module.

  Own Id: OTP-14390

- Add early reject of invalid node names from distributed nodes.

  Own Id: OTP-14426

### Improvements and New Features

- Since Unicode is now allowed in atoms an extra check is needed for node names,
  which are restricted to Latin-1.

  Own Id: OTP-13805

- Replaced usage of deprecated symbolic [`time unit`](`t:erlang:time_unit/0`)
  representations.

  Own Id: OTP-13831 Aux Id: OTP-13735

- `file:write_file(Name, Data, [raw])` would turn `Data` into a single binary
  before writing. This meant it could not take advantage of the `writev()`
  system call if it was given a list of binaries and told to write with `raw`
  mode.

  Own Id: OTP-13909

- The performance of the `disk_log` has been somewhat improved in some corner
  cases (big items), and the documentation has been clarified.

  Own Id: OTP-14057 Aux Id: PR-1245

- Functions for detecting changed code has been added. `code:modified_modules/0`
  returns all currently loaded modules that have changed on disk.
  `code:module_status/1` returns the status for a module. In the shell and in
  `c` module, `mm/0` is short for `code:modified_modules/0`, and `lm/0` reloads
  all currently loaded modules that have changed on disk.

  Own Id: OTP-14059

- Introduce an event manager in Erlang to handle OS signals. A subset of OS
  signals may be subscribed to and those are described in the Kernel
  application.

  Own Id: OTP-14186

- Sockets can now be bound to device (SO_BINDTODEVICE) on platforms where it is
  supported.

  This has been implemented e.g to support VRF-Lite under Linux; see
  [VRF ](https://www.kernel.org/doc/Documentation/networking/vrf.txt), and
  GitHub pull request [\#1326](https://github.com/erlang/otp/pull/1326).

  Own Id: OTP-14357 Aux Id: PR-1326

- Added option to store shell_history on disk so that the history can be reused
  between sessions.

  Own Id: OTP-14409 Aux Id: PR-1420

- The size of crash reports created by `gen_server`, `gen_statem` and `proc_lib`
  is limited with aid of the Kernel application variable
  `error_logger_format_depth`. The purpose is to limit the size of the messages
  sent to the `error_logger` process when processes with huge message queues or
  states crash.

  The crash report generated by `proc_lib` includes the new tag
  `message_queue_len`. The neighbour report also includes the new tag
  `current_stacktrace`. Finally, the neighbour report no longer includes the
  tags `messages` and `dictionary`.

  The new function `error_logger:get_format_depth/0` can be used to retrieve the
  value of the Kernel application variable `error_logger_format_depth`.

  Own Id: OTP-14417

- One of the ETS tables used by the `global` module is created with
  `{read_concurrency, true}` in order to reduce contention.

  Own Id: OTP-14419

- Warnings have been added to the relevant documentation about not using
  un-secure distributed nodes in exposed environments.

  Own Id: OTP-14425

## Kernel 5.2

### Fixed Bugs and Malfunctions

- Fix a race during cleanup of os:cmd that would cause os:cmd to hang
  indefinitely.

  Own Id: OTP-14232 Aux Id: seq13275

### Improvements and New Features

- The functions in the '`file`' module that take a list of paths (e.g.
  `file:path_consult/2`) will now continue to search in the path if the path
  contains something that is not a directory.

  Own Id: OTP-14191

- Two OTP processes that are known to receive many messages are 'rex' (used by
  'rpc') and 'error_logger'. Those processes will now store unprocessed messages
  outside the process heap, which will potentially decrease the cost of garbage
  collections.

  Own Id: OTP-14192

## Kernel 5.1.1

### Fixed Bugs and Malfunctions

- `code:add_pathsa/1` and command line option `-pa` both revert the given list
  of directories when adding it at the beginning of the code path. This is now
  documented.

  Own Id: OTP-13920 Aux Id: ERL-267

- Add lost runtime dependency to erts-8.1. This should have been done in
  kernel-5.1 (OTP-19.1) as it cannot run without at least erts-8.1 (OTP-19.1).

  Own Id: OTP-14003

- Type and doc for gen\_\{tcp,udp,sctp\}:controlling_process/2 has been
  improved.

  Own Id: OTP-14022 Aux Id: PR-1208

## Kernel 5.1

### Fixed Bugs and Malfunctions

- Fix a memory leak when calling seq_trace:get_system_tracer().

  Own Id: OTP-13742

- Fix for the problem that when adding the ebin directory of an application to
  the code path, the `code:priv_dir/1` function returns an incorrect path to the
  priv directory of the same application.

  Own Id: OTP-13758 Aux Id: ERL-195

- Fix code_server crash when adding code paths of two levels.

  Own Id: OTP-13765 Aux Id: ERL-194

- Respect -proto_dist switch while connection to EPMD

  Own Id: OTP-13770 Aux Id: PR-1129

- Fixed a bug where init:stop could deadlock if a process with infinite shutdown
  timeout (e.g. a supervisor) attempted to load code while terminating.

  Own Id: OTP-13802

- Close stdin of commands run in os:cmd. This is a backwards compatibility fix
  that restores the behaviour of pre 19.0 os:cmd.

  Own Id: OTP-13867 Aux Id: seq13178

### Improvements and New Features

- Add `net_kernel:setopts/2` and `net_kernel:getopts/2` to control options for
  distribution sockets in runtime.

  Own Id: OTP-13564

- Rudimentary support for DSCP has been implemented in the guise of a `tclass`
  socket option for IPv6 sockets.

  Own Id: OTP-13582

## Kernel 5.0.2

### Fixed Bugs and Malfunctions

- When calling os:cmd from a process that has set trap_exit to true an 'EXIT'
  message would be left in the message queue. This bug was introduced in kernel
  vsn 5.0.1.

  Own Id: OTP-13813

## Kernel 5.0.1

### Fixed Bugs and Malfunctions

- Fix a os:cmd bug where creating a background job using & would cause os:cmd to
  hang until the background job terminated or closed its stdout and stderr file
  descriptors. This bug has existed from kernel 5.0.

  Own Id: OTP-13741

## Kernel 5.0

### Fixed Bugs and Malfunctions

- The handling of `on_load` functions has been improved. The major improvement
  is that if a code upgrade fails because the `on_load` function fails, the
  previous version of the module will now be retained.

  Own Id: OTP-12593

- `rpc:call()` and `rpc:block_call()` would sometimes cause an exception (which
  was not mentioned in the documentation). This has been corrected so that
  `{badrpc,Reason}` will be returned instead.

  Own Id: OTP-13409

- On Windows, for modules that were loaded early (such as the `lists` module),
  `code:which/1` would return the path with mixed slashes and backslashes, for
  example: `"C:\\Program Files\\erl8.0/lib/stdlib-2.7/ebin/lists.beam"`. This
  has been corrected.

  Own Id: OTP-13410

- Make file:datasync use fsync instead of fdatasync on Mac OSX.

  Own Id: OTP-13411

- The default chunk size for the fallback sendfile implementation, used on
  platforms that do not have a native sendfile, has been decreased in order to
  reduce connectivity issues.

  Own Id: OTP-13444

- Large file writes (2Gb or more) could fail on some Unix platforms (for
  example, OS X and FreeBSD).

  Own Id: OTP-13461

- A bug has been fixed where the DNS resolver inet_res did not refresh its view
  of the contents of for example resolv.conf immediately after start and hence
  then failed name resolution. Reported and fix suggested by Michal Ptaszek in
  GitHUB pull req #949.

  Own Id: OTP-13470 Aux Id: Pull #969

- Fix process leak from global_group.

  Own Id: OTP-13516 Aux Id: PR-1008

- The function `inet:gethostbyname/1` now honors the resolver option `inet6`
  instead of always looking up IPv4 addresses.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13622 Aux Id: PR-1065

- The `Status` argument to `init:stop/1` is now sanity checked to make sure
  `erlang:halt` does not fail.

  Own Id: OTP-13631 Aux Id: PR-911

### Improvements and New Features

- Add \{line_delim, byte()\} option to inet:setopts/2 and decode_packet/3

  Own Id: OTP-12837

- Added `os:perf_counter/1`.

  The perf_counter is a very very cheap and high resolution timer that can be
  used to timestamp system events. It does not have monoticity guarantees, but
  should on most OS's expose a monotonous time.

  Own Id: OTP-12908

- The os:cmd call has been optimized on unix platforms to be scale better with
  the number of schedulers.

  Own Id: OTP-13089

- New functions that can load multiple modules at once have been added to the
  '`code`' module. The functions are `code:atomic_load/1`,
  `code:prepare_loading/1`, `code:finish_loading/1`, and
  `code:ensure_modules_loaded/1`.

  Own Id: OTP-13111

- The code path cache feature turned out not to be very useful in practice and
  has been removed. If an attempt is made to enable the code path cache, there
  will be a warning report informing the user that the feature has been removed.

  Own Id: OTP-13191

- When an attempt is made to start a distributed Erlang node with the same name
  as an existing node, the error message will be much shorter and easier to read
  than before. Example:

  `Protocol 'inet_tcp': the name somename@somehost seems to be in use by another Erlang node`

  Own Id: OTP-13294

- The output of the default error logger is somewhat prettier and easier to
  read. The default error logger is used during start-up of the OTP system. If
  the start-up fails, the output will be easier to read.

  Own Id: OTP-13325

- The functions `rpc:safe_multi_server_call/2,3` that were deprecated in R12B
  have been removed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13449

- Update the error reasons in dist_util, and show them in the logs if
  net_kernel:verbose(1) has been called.

  Own Id: OTP-13458

- Experimental support for Unix Domain Sockets has been implemented. Read the
  sources if you want to try it out. Example:
  `gen_udp:open(0, [{ifaddr,{local,"/tmp/socket"}}])`. Documentation will be
  written after user feedback on the experimental API.

  Own Id: OTP-13572 Aux Id: PR-612

- Allow heart to be configured to not kill the previous emulator before calling
  the HEART_COMMAND. This is done by setting the environment variable
  HEART_NO_KILL to TRUE.

  Own Id: OTP-13650

## Kernel 4.2

### Fixed Bugs and Malfunctions

- `code:load_abs([10100])` would bring down the entire runtime system and create
  a crash dump. Corrected to generate an error exception in the calling process.

  Also corrected specs for code loading functions and added more information in
  the documentation about the error reasons returned by code-loading functions.

  Own Id: OTP-9375

- `gen_tcp:accept/2` was not
  [time warp safe](`e:erts:time_correction.md#time-warp-safe-code`). This since
  it used the same time as returned by `erlang:now/0` when calculating timeout.
  This has now been fixed.

  Own Id: OTP-13254 Aux Id: OTP-11997, OTP-13222

- Correct the contract for `inet:getifaddrs/1`.

  Own Id: OTP-13335 Aux Id: ERL-95

### Improvements and New Features

- Time warp safety improvements.

  Introduced the options `monotonic_timestamp`, and `strict_monotonic_timestamp`
  to the trace, sequential trace, and system profile functionality. This since
  the already existing `timestamp` option is not time warp safe.

  Introduced the option `safe_fixed_monotonic_time` to `ets:info/2` and
  `dets:info/2`. This since the already existing `safe_fixed` option is not time
  warp safe.

  Own Id: OTP-13222 Aux Id: OTP-11997

- Add validation callback for heart

  The erlang heart process may now have a validation callback installed. The
  validation callback will be executed, if present, before any heartbeat to
  heart port program. If the validation fails, or stalls, no heartbeat will be
  sent and the node will go down.

  With the option `'check_schedulers'` heart executes a responsiveness check of
  the schedulers before a heartbeat is sent to the port program. If the
  responsiveness check fails, the heartbeat will not be performed (as intended).

  Own Id: OTP-13250

- Clarify documentation of `net_kernel:allow/1`

  Own Id: OTP-13299

- EPMD supports both IPv4 and IPv6

  Also affects oldest supported windows version.

  Own Id: OTP-13364

## Kernel 4.1.1

### Fixed Bugs and Malfunctions

- Host name lookups though inet_res, the Erlang DNS resolver, are now done case
  insensitively according to RFC 4343. Patch by Holger Weiß.

  Own Id: OTP-12836

- IPv6 distribution handler has been updated to share code with IPv4 so that all
  features are supported in IPv6 as well. A bug when using an IPv4 address as
  hostname has been fixed.

  Own Id: OTP-13040

- Caching of host names in the internal DNS resolver inet_res has been made
  character case insensitive for host names according to RFC 4343.

  Own Id: OTP-13083

- Cooked file mode buffering has been fixed so file:position/2 now works
  according to Posix on Posix systems i.e. when file:position/2 returns an error
  the file pointer is unaffected.

  The Windows system documentation, however, is unclear on this point so the
  documentation of file:position/2 still does not promise anything.

  Cooked file mode file:pread/2,3 and file:pwrite/2,3 have been corrected to
  honor character encoding like the combination of file:position/2 and
  file:read/2 or file:write/2 already does. This is probably not very useful
  since the character representation on the caller's side is latin1, period.

  Own Id: OTP-13155 Aux Id: PR#646

### Improvements and New Features

- Add \{line_delim, byte()\} option to inet:setopts/2 and decode_packet/3

  Own Id: OTP-12837

## Kernel 4.1

### Improvements and New Features

- A mechanism for limiting the amount of text that the built-in error logger
  events will produce has been introduced. It is useful for limiting both the
  size of log files and the CPU time used to produce them.

  This mechanism is experimental in the sense that it may be changed if it turns
  out that it does not solve the problem it is supposed to solve. In that case,
  there may be backward incompatible improvements to this mechanism.

  See the documentation for the config parameter `error_logger_format_depth` in
  the Kernel application for information about how to turn on this feature.

  Own Id: OTP-12864

## Kernel 4.0

### Fixed Bugs and Malfunctions

- Fix error handling in `file:read_line/1` for Unicode contents.

  Own Id: OTP-12144

- Introduce `os:getenv/2` which is similar to `os:getenv/1` but returns the
  passed default value if the required environment variable is undefined.

  Own Id: OTP-12342

- It is now possible to paste text in JCL mode (using Ctrl-Y) that has been
  copied in the previous shell session. Also a bug that caused the JCL mode to
  crash when pasting text has been fixed.

  Own Id: OTP-12673

- Ensure that each segment of an IPv6 address when parsed from a string has a
  maximum of 4 hex digits

  Own Id: OTP-12773

### Improvements and New Features

- New BIF: `erlang:get_keys/0`, lists all keys associated with the process
  dictionary. Note: `erlang:get_keys/0` is auto-imported.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12151 Aux Id: seq12521

- The internal group to user_drv protocol has been changed to be synchronous in
  order to guarantee that output sent to a process implementing the user_drv
  protocol is printed before replying. This protocol is used by the
  standard_output device and the ssh application when acting as a client.

  This change changes the previous unlimited buffer when printing to standard_io
  and other devices that end up in user_drv to 1KB.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12240

- The `inflateInit/2` and `deflateInit/6` functions now accepts a WindowBits
  argument equal to 8 and -8.

  Own Id: OTP-12564

- Map error logger warnings to warning messages by default.

  Own Id: OTP-12755

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

- There are many cases where user code needs to be able to distinguish between a
  socket that was closed normally and one that was aborted. Setting the option
  \{show_econnreset, true\} enables the user to receive ECONNRESET errors on
  both active and passive sockets.

  Own Id: OTP-12843

## Kernel 3.2.0.1

### Fixed Bugs and Malfunctions

- The 'raw' socket option could not be used multiple times in one call to any
  e.g gen_tcp function because only one of the occurrences were used. This bug
  has been fixed, and also a small bug concerning propagating error codes from
  within inet:setopts/2.

  Own Id: OTP-11482 Aux Id: seq12872

## Kernel 3.2

### Fixed Bugs and Malfunctions

- A bug causing an infinite loop in hostname resolving has been corrected. To
  trigger this bug you would have to enter an bogus search method from a
  configuration file e.g .inetrc.

  Bug pinpointed by Emil Holmström

  Own Id: OTP-12133

- The standard_error process now handles the getopts I/O protocol request
  correctly and stores its encoding in the same way as standard_io.

  Also, io:put_chars(standard_error, \[oops]) could previously crash the
  standard_error process. This is now corrected.

  Own Id: OTP-12424

### Improvements and New Features

- Configuration parameters for the Kernel application that allows setting socket
  options for the distribution sockets have been added. See the application
  Kernel documentation; parameters 'inet_dist_listen_options' and
  'inet_dist_connect_options'.

  Own Id: OTP-12476 Aux Id: OTP-12476

## Kernel 3.1

### Fixed Bugs and Malfunctions

- Make sure to install .hrl files when needed

  Own Id: OTP-12197

- Removed the undocumented application environment variable 'raw_files' from the
  kernel application. This variable was checked (by call to
  application:get_env/2) each time a raw file was to be opened in the file
  module.

  Own Id: OTP-12276

- A bug has been fixed when using the netns option to gen_udp, which
  accidentally only worked if it was the last option.

  Own Id: OTP-12314

### Improvements and New Features

- Updated documentation for inet buffer size options.

  Own Id: OTP-12296

- Introduce new option 'raw' in file_info and link_info functions. This option
  allows the caller not to go through the file server for information about
  files guaranteed to be local.

  Own Id: OTP-12325

## Kernel 3.0.3

### Fixed Bugs and Malfunctions

- Accept inet:ip_address() in net_adm:names/1

  Own Id: OTP-12154

## Kernel 3.0.2

### Fixed Bugs and Malfunctions

- OTP-11850 fixed filelib:wildcard/1 to work with broken symlinks. This
  correction, however, introduced problems since symlinks were no longer
  followed for functions like filelib:ensure_dir/1, filelib:is_dir/1,
  filelib:file_size/1, etc. This is now corrected.

  Own Id: OTP-12054 Aux Id: seq12660

## Kernel 3.0.1

### Fixed Bugs and Malfunctions

- If the Config given to application_controller:change_application_data included
  other config files, it was only expanded for already existing (loaded)
  applications. If an upgrade added a new application which had config data in
  an included config file, the new application did not get correct config data.

  This is now changed so config data will be expanded for all applications.

  Own Id: OTP-11864

- It was allowed to re-load pre-loaded modules such as `erlang`, but that could
  cause strange and unwanted things to happen, such as call
  [`apply/3`](`apply/3`) to loop. Pre-loaded modules are now sticky by default.
  (Thanks to Loïc Hoguin for reporting this bug.)

  `code:add_path("/ending/in/slash/")` removes the trailing slash, adding
  `/ending/in/slash` to the code path. However,
  `code:del_path("/ending/in/slash/")` would fail to remove the path since it
  did not remove the trailing slash. This has been fixed.

  Own Id: OTP-11913

- Fix erts_debug:size/1 to handle Map sizes

  Own Id: OTP-11923

- The documentation for `file:file_info/1` has been removed. The function itself
  was removed a long time ago.

  Own Id: OTP-11982

## Kernel 3.0

### Fixed Bugs and Malfunctions

- Fixed a deadlock possibility in terminate application

  Own Id: OTP-11171

- Fixed bug where sendfile would return the wrong error code for a remotely
  closed socket if the socket was in passive mode. (Thanks to Vincent Siliakus
  for reporting the bug.)

  Own Id: OTP-11614

- The new option `persistent` is added to `application:set_env/4` and
  `application:unset_env/3`. An environment key set with the `persistent` option
  will not be overridden by the ones configured in the application resource file
  on load. This means persistent values will stick after the application is
  loaded and also on application reload. (Thanks to José Valim)

  Own Id: OTP-11708

- The spec for file:set_cwd/1 is modified to also accept binaries as arguments.
  This has always been allowed in the code, but it was not reflected in the spec
  since binaries are mostly used for raw file names. Raw file names are names
  that are not encoded according to file:native_name_encoding(), and these are
  not allowed in file:set_cwd/1. The spec is now, however, more allowing in
  order to avoid unnecessary dialyzer warnings. Raw file names will still fail
  in runtime with reason 'no_translation'. (Thanks to José Valim)

  Own Id: OTP-11787

### Improvements and New Features

- heart:set_cmd/1 is updated to allow unicode code points > 255 in the given
  heart command

  Own Id: OTP-10843

- Dialyzer's `unmatched_return` warnings have been corrected.

  Own Id: OTP-10908

- Make erlang:open_port/2 spawn and spawn_executable handle unicode.

  Own Id: OTP-11105

- Erlang/OTP has been ported to the realtime operating system OSE. The port
  supports both smp and non-smp emulator. For details around the port and how to
  started see the User's Guide in the ose application.

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

- Add sync option to file:open/2.

  The sync option adds the POSIX O_SYNC flag to the open system call on
  platforms that support the flag or its equivalent, e.g.,
  FILE_FLAG_WRITE_THROUGH on Windows. For platforms that don't support it,
  file:open/2 returns \{error, enotsup\} if the sync option is passed in. Thank
  to Steve Vinoski and Joseph Blomstedt

  Own Id: OTP-11498

- The contract of `inet:ntoa/1` has been corrected.

  Thanks to Max Treskin.

  Own Id: OTP-11730

## Kernel 2.16.4.1

### Known Bugs and Problems

- When using gen_tcp:connect and the `fd` option with `port` and/or `ip`, the
  `port` and `ip` options were ignored. This has been fixed so that if `port`
  and/or `ip` is specified together with `fd` a bind is requested for that `fd`.
  If `port` and/or `ip` is not specified bind will not be called.

  Own Id: OTP-12061

## Kernel 2.16.4

### Fixed Bugs and Malfunctions

- Fix the typespec for the inet:ifget/2 and inet:ifget/3 return value. Thanks to
  Ali Sabil.

  Own Id: OTP-11377

- Fix various typos in erts, kernel and ssh. Thanks to Martin Hässler.

  Own Id: OTP-11414

- Fix rpc multicall sample code. Thanks to Edwin Fine.

  Own Id: OTP-11471

- Under rare circumstances a process calling `inet:close/1`, `gen_tcp:close/1`,
  `gen_udp:close/1`, or `gen_sctp:close/1` could hang in the call indefinitely.

  Own Id: OTP-11491

### Improvements and New Features

- Add more SCTP errors as described in RFC 4960. Thanks to Artem Teslenko.

  Own Id: OTP-11379

- Add new BIF os:unsetenv/1 which deletes an environment variable. Thanks to
  Martin Hässler.

  Own Id: OTP-11446

## Kernel 2.16.3

### Fixed Bugs and Malfunctions

- Fix indentation of User switch command help in Erlang shell. Thanks to Sylvain
  Benner.

  Own Id: OTP-11209

### Improvements and New Features

- The previous undocumented function ntoa/1 has been added to inet docs and
  exported in the inet module.

  Own Id: OTP-10676 Aux Id: OTP-10314

- Fix typo in abcast() function comment. Thanks to Johannes Weissl.

  Own Id: OTP-11219

- Add application:ensure_all_started/1-2. Thanks to Fred Hebert.

  Own Id: OTP-11250

- Make edlin understand a few important control keys. Thanks to Stefan
  Zegenhagen.

  Own Id: OTP-11251

- Cleanup of hipe_unified_loader, eliminating uses of is_subtype/2 in specs,
  change module-local void functions to return 'ok' instead of [] and made sure
  there are no dialyzer warnings with --Wunmatched_returns. Thanks to Kostis
  Sagonas.

  Own Id: OTP-11301

## Kernel 2.16.2

### Fixed Bugs and Malfunctions

- A bug in prim_inet has been corrected. If the port owner was killed at a bad
  time while closing the socket port the port could become orphaned hence
  causing port and socket leaking. Reported by Fred Herbert, Dmitry Belyaev and
  others.

  Own Id: OTP-10497 Aux Id: OTP-10562

- A few bugs regarding case sensitivity for hostname resolution while using e.g
  the internal lookup types 'file' and 'dns' has been corrected. When looking up
  hostnames ASCII letters a-z are to be regarded as the same as A-Z according to
  RFC 4343 "Domain Name System (DNS) Case Insensitivity Clarification", and this
  was not always the case.

  Own Id: OTP-10689 Aux Id: seq12227

### Improvements and New Features

- Add `application:ensure_started/1,2`. It is equivavlent to
  `application:start/1,2` except it returns `ok` for already started
  applications.

  Own Id: OTP-10910

- Optimize communication with file io server. Thanks to Anthony Ramine.

  Own Id: OTP-11040

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

- Fix deep list argument error under Windows in os:cmd/1. Thanks to Aleksandr
  Vinokurov .

  Own Id: OTP-11104

## Kernel 2.16.1

### Fixed Bugs and Malfunctions

- A bug that could cause a crash with wrong reason has been corrected in the
  `application_controller` module.

  Own Id: OTP-10754

- Fix `code:is_module_native/1` that sometimes in R16A returned false for hipe
  compiled modules containing BIFs such as `lists`.

  Own Id: OTP-10870

- Respect `{exit_on_close,false}` option on tcp socket in non-passive mode when
  receiving fails (due to an ill-formed packet for example) by only doing a half
  close and still allow sending on the socket. (Thanks to Anthony Molinaro and
  Steve Vinoski for reporting the problem)

  Own Id: OTP-10879

### Improvements and New Features

- Slightly nicer error message when node start fails due to duplicate name.
  Thanks to Magnus Henoch.

  Own Id: OTP-10797

- Miscellaneous updates due to Unicode support.

  Own Id: OTP-10820

- Add a new function code:get_mode() can be used to detect how the code servers
  behaves. Thanks to Vlad Dumitrescu

  Own Id: OTP-10823

- Fix type of error Reason on gen_tcp:send/2. Thanks to Sean Cribbs.

  Own Id: OTP-10839

- `file:list_dir_all/1` and `file:read_link_all/1` that can handle raw file
  names have been added. See the User Guide for STDLIB for information about raw
  file names.

  Own Id: OTP-10852

## Kernel 2.16

### Fixed Bugs and Malfunctions

- It is no longer possible to have `{Mod,Vsn}` in the 'modules' list in a .app
  file.

  This was earlier possible, although never documented in the .app file
  reference manual. It was however visible in the documentation of
  `application:load/[1,2]`, where the same term as in a .app file can be used as
  the first argument.

  The possibility has been removed since the `Vsn` part was never used.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-10417

- The contract of `erl_ddll:format_error/1` has been corrected. (Thanks to
  Joseph Wayne Norton.)

  Own Id: OTP-10473

- Change printout of application crash message on startup to formatted strings
  (Thanks to Serge Aleynikov)

  Own Id: OTP-10620

- The type `ascii_string()` in the `base64` module has been corrected. The type
  [`file:file_info()`](`t:file:file_info/0`) has been cleaned up. The type
  [`file:fd()`](`t:file:fd/0`) has been made opaque in the documentation.

  Own Id: OTP-10624 Aux Id: kunagi-352 \[263]

### Improvements and New Features

- Inet exported functionality

  inet:parse_ipv4_address/1, inet:parse_ipv4strict_address/1,
  inet:parse_ipv6_address/1, inet:parse_ipv6strict_address/1,
  inet:parse_address/1 and inet:parse_strict_address is now exported from the
  inet module.

  Own Id: OTP-8067 Aux Id: kunagi-274 \[185]

- A boolean socket option 'ipv6_v6only' for IPv6 sockets has been added. The
  default value of the option is OS dependent, so applications aiming to be
  portable should consider using `{ipv6_v6only,true}` when creating an `inet6`
  listening/destination socket, and if necessary also create an `inet` socket on
  the same port for IPv4 traffic. See the documentation.

  Own Id: OTP-8928 Aux Id: kunagi-193 \[104]

- Support for Unicode has been implemented.

  Own Id: OTP-10302

- The documentation for `global:register_name/3` has been updated to mention
  that the use of `{Module,Function}` as the method argument (resolve function)
  is deprecated.

  Own Id: OTP-10419

- Fixed bug where sendfile on oracle solaris would return an error when a
  partial send was done.

  Own Id: OTP-10549

- The `error_handler` module will now call `'$handle_undefined_function'/2` if
  an attempt is made to call a non-existing function in a module that exists.
  See the documentation for `error_handler` module for details.

  Own Id: OTP-10617 Aux Id: kunagi-340 \[251]

- Where necessary a comment stating encoding has been added to Erlang files. The
  comment is meant to be removed in Erlang/OTP R17B when UTF-8 becomes the
  default encoding.

  Own Id: OTP-10630

- Do not return wrong terms unnecessarily. (Thanks to Kostis Sagonas.)

  Own Id: OTP-10662

- Some examples overflowing the width of PDF pages have been corrected.

  Own Id: OTP-10665

- Add file:allocate/3 operation

  This operation allows pre-allocation of space for files. It succeeds only on
  systems that support such operation. (Thanks to Filipe David Manana)

  Own Id: OTP-10680

- Add application:get_key/3. The new function provides a default value for a
  configuration parameter. Thanks to Serge Aleynikov.

  Own Id: OTP-10694

- Add search to Erlang shell's history. Thanks to Fred Herbert.

  Own Id: OTP-10739

## Kernel 2.15.3

### Fixed Bugs and Malfunctions

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

## Kernel 2.15.2

### Fixed Bugs and Malfunctions

- Fixed issue where using controlling_process/2 with self() as the second
  argument caused the port to leak if self() crashes. (Thanks to Ricardo
  Catalinas Jiménez)

  Own Id: OTP-10094

- When sending large files using the file:sendfile fallback file:sendfile would
  crash. This is now fixed.

  Own Id: OTP-10098

- Fix rpc:call/5 for local calls with a finite Timeout (Thanks to Tomer
  Chachamu)

  Own Id: OTP-10149

- fix escript/primary archive reloading

  If the mtime of an escript/primary archive file changes after being added to
  the code path, correctly reload the archive and update the cache. (Thanks to
  Tuncer Ayaz)

  Own Id: OTP-10151

- Support added for home directories named with non-ASCII characters (codepoints
  above 127) on a system running in Unicode file mode (e.g. on MacOSX or Linux
  with startup arguments +fnu or +fna with the right LOCALE). Also environment
  variables with Unicode content are supported in applicable environments.

  Own Id: OTP-10160

- Allow mixed IPv4 and IPv6 addresses to sctp_bindx

  Also allow mixed address families to bind, since the first address on a
  multihomed sctp socket must be bound with bind, while the rest are to be bound
  using sctp_bindx. At least Linux supports adding address of mixing families.
  Make inet_set_faddress function available also when HAVE_SCTP is not defined,
  since we use it to find an address for bind to be able to mix ipv4 and ipv6
  addresses. Thanks to Tomas Abrahamsson

  Own Id: OTP-10217

### Improvements and New Features

- Document inet options: high_watermark, priority, linger and a some other
  options that previously was undocumented.

  Own Id: OTP-10053

- Remove bit8 option support from inet

  Own Id: OTP-10056

- The type of the disk log header has been corrected. (Thanks to Niclas Eklund.)

  Own Id: OTP-10131

## Kernel 2.15.1

### Fixed Bugs and Malfunctions

- Driver output has been corrected so output of large binaries (> 4 GiB) now
  does not silently fail or crash the emulator, but either outputs the binary or
  fails the call. This means that writing a binary > 4 Gib to file now works but
  on e.g 64-bit Windows (that has scatter/gather I/O buffer segment lengths of
  32 bits) fails. The behaviour may change in the future to always write the
  binary, in parts if necessary.

  Own Id: OTP-9820 Aux Id: OTP-9795

- erts: minor fix for unnecessary condition erts: change SENDFILE_CHUNK_SIZE
  from signed to unsigned (Thanks to jovi zhang)

  Own Id: OTP-9872

- Two contracts in `gen_sctp` have been corrected.

  Own Id: OTP-9874

- If a process calls a module with an running on_load handler, the process is
  supposed to be suspended. But if the module with the on_load handler was
  loading used `code:load_binary/3`, the call would instead fail with an `undef`
  exception.

  Own Id: OTP-9875

- File name and error reason is now returned if creation of a cookie fails.
  (Thanks to Magnus Henoch)

  Own Id: OTP-9954

- Fix port leak in `zlib` when passing invalid data to
  `compress,uncompress,zip,unzip,gzip,gunzip`.

  Own Id: OTP-9981

- Various typographical errors corrected in documentation for the global,
  error_logger, etop, lists, ets and supervisor modules and in the c_portdriver
  and kernel_app documentation. (Thanks to Ricardo Catalinas Jiménez)

  Own Id: OTP-9987

- Fix returned error from gen_tcp:accept/1,2 when running out of ports.

  The \{error, enfile\} return value is badly misleading and confusing for this
  case, since the Posix ENFILE errno value has a well-defined meaning that has
  nothing to do with Erlang ports. The fix changes the return value to \{error,
  system_limit\}, which is consistent with e.g. various file(3) functions.
  inet:format_error/1 has also been updated to support system_limit in the same
  manner as file:format_error/1. (Thanks to Per Hedeland)

  Own Id: OTP-9990

- `erts_debug:size/1` has been corrected to take sharing in the environment of
  funs into account. For funs it used to always give the same result as
  `erts_debug:flat_size/1`.

  Own Id: OTP-9991

- In some cases when the process doing file:sendfile crashes while sending the
  file the efile_drv code would not clean up after itself correctly. This has
  now been fixed.

  Own Id: OTP-9993

- On BSD based platforms file:sendfile would sometime go into an infinite loop
  when sending big files. This has now been fixed.

  Own Id: OTP-9994

- While `disk_log` eagerly collects logged terms for better performance,
  collecting too much data may choke the system and cause huge binaries to be
  written.

  The problem was addressed in OTP-9764, but the situation was not improved in
  all cases.

  (Thanks to Richard Carlsson.)

  Own Id: OTP-9999 Aux Id: OTP-9764

- The documentation of .app files incorrectly said that the default value for
  the `mod` parameter is `undefined`. This is now corrected to `[]`.

  Own Id: OTP-10002

## Kernel 2.15

### Fixed Bugs and Malfunctions

- Calls to `global:whereis_name/1` have been substituted for calls to
  `global:safe_whereis_name/1` since the latter is not safe at all.

  The reason for not doing this earlier is that setting a global lock masked out
  a bug concerning the restart of supervised children. The bug has now been
  fixed by a modification of `global:whereis_name/1`. (Thanks to Ulf Wiger for
  code contribution.)

  A minor race conditions in `gen_fsm:start*` has been fixed: if one of these
  functions returned `{error, Reason}` or ignore, the name could still be
  registered (either locally or in `global`. (This is the same modification as
  was done for gen_server in OTP-7669.)

  The undocumented function `global:safe_whereis_name/1` has been removed.

  Own Id: OTP-9212 Aux Id: seq7117, OTP-4174

- Honor option `packet_size` for http packet parsing by both TCP socket and
  `erlang:decode_packet`. This gives the ability to accept HTTP headers larger
  than the default setting, but also avoid DoS attacks by accepting lines only
  up to whatever length you wish to allow. For consistency, packet type `line`
  also honor option `packet_size`. (Thanks to Steve Vinoski)

  Own Id: OTP-9389

- `disk_log:reopen/2,3` and `disk_log:breopen/3` could return the error reason
  from `file:rename/2` rather than the reason `{file_error, Filename, Reason}`.
  This bug has been fixed.

  The message `{disk_log, Node, {error, disk_log_stopped}}` which according the
  documentation is sent upon failure to truncate or reopen a disk log was
  sometimes turned into a reply. This bug has been fixed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9508

- Environment variable 'shutdown_timeout' is added to kernel application.
  Earlier, application_controller would hang forever if an application top
  supervisor did not terminate upon a shutdown request. If this new environment
  variable is set to a positive integer T, then application controller will now
  give up after T milliseconds and instead brutally kill the application. For
  backwards compatibility, the default value for shutdown_timeout is 'infinity'.

  Own Id: OTP-9540

- Add '-callback' attributes in stdlib's behaviours

  Replace the behaviour_info(callbacks) export in stdlib's behaviours with
  -callback' attributes for all the callbacks. Update the documentation with
  information on the callback attribute Automatically generate 'behaviour_info'
  function from '-callback' attributes

  'behaviour_info(callbacks)' is a special function that is defined in a module
  which describes a behaviour and returns a list of its callbacks.

  This function is now automatically generated using the '-callback' specs. An
  error is returned by lint if user defines both '-callback' attributes and the
  behaviour_info/1 function. If no type info is needed for a callback use a
  generic spec for it. Add '-callback' attribute to language syntax

  Behaviours may define specs for their callbacks using the familiar spec
  syntax, replacing the '-spec' keyword with '-callback'. Simple lint checks are
  performed to ensure that no callbacks are defined twice and all types referred
  are declared.

  These attributes can be then used by tools to provide documentation to the
  behaviour or find discrepancies in the callback definitions in the callback
  module.

  Add callback specs into 'application' module in kernel Add callback specs to
  tftp module following internet documentation Add callback specs to
  inets_service module following possibly deprecated comments

  Own Id: OTP-9621

- make tab completion work in remote shells (Thanks to Mats Cronqvist)

  Own Id: OTP-9673

- Add missing parenthesis in heart doc.

  Add missing spaces in the Reference Manual distributed section.

  In the HTML version of the doc those spaces are necessary to separate those
  words.

  Own Id: OTP-9693

- Fixes net_kernel:get_net_ticktime() doc

  Adds missing description when \`ignored' is returned. (Thanks to Ricardo
  Catalinas Jiménez )

  Own Id: OTP-9713

- While `disk_log` eagerly collects logged terms for better performance,
  collecting too much data may choke the system and cause huge binaries to be
  written. In order to remedy the situation a (small) limit on the amount of
  data that is collected before writing to disk has been introduced.

  Own Id: OTP-9764

- - Correct callback spec in application module
  - Refine warning about callback specs with extra ranges
  - Cleanup autoimport compiler directives
  - Fix Dialyzer's warnings in typer
  - Fix Dialyzer's warning for its own code
  - Fix bug in Dialyzer's behaviours analysis
  - Fix crash in Dialyzer
  - Variable substitution was not generalizing any unknown variables.

  Own Id: OTP-9776

- Fix a crash when file:change_time/2,3 are called with invalid dates

  Calling file:change_time/2,3 with an invalid date tuple (e.g
  file:change_time("file.txt", \{undefined, undefined\})) will cause
  file_server_2 to crash. error_logger will shutdown and the whole VM will stop.
  Change behavior to validate given dates on system boundaries. (i.e before
  issuing a server call).(Thanks to Ahmed Omar)

  Own Id: OTP-9785

### Improvements and New Features

- An option list argument can now be passed to
  `file:read_file_info/2, file:read_link_info/2` and `file:write_file_info/3`
  and set time type information in the call. Valid options are
  `{time, local}, {time, universal}` and `{time, posix}`. In the case of `posix`
  time no conversions are made which makes the operation a bit faster.

  Own Id: OTP-7687

- `file:list_dir/1,2` will now fill an buffer entire with filenames from the
  efile driver before sending it to an erlang process. This will speed up this
  file operation in most cases.

  Own Id: OTP-9023

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

- Tuple funs (a two-element tuple with a module name and a function) are now
  officially deprecated and will be removed in R16. Use '`fun M:F/A`' instead.
  To make you aware that your system uses tuple funs, the very first time a
  tuple fun is applied, a warning will be sent to the error logger.

  Own Id: OTP-9649

## Kernel 2.14.5

### Fixed Bugs and Malfunctions

- Fix type of Packet arg of gen_tcp:send/2 and gen_udp:send/4

  The type is marked as a binary() or a string() but in practice it can be an
  iodata(). The test suite was updated to confirm the gen_tcp/2 and
  gen_udp:send/4 functions accept iodata() (iolists) packets. (Thanks to Filipe
  David Manana)

  Own Id: OTP-9514

- XML files have been corrected.

  Own Id: OTP-9550 Aux Id: OTP-9541

### Improvements and New Features

- The types and specifications of the inet modules have been improved.

  Own Id: OTP-9260

- Types and specifications have been added.

  Own Id: OTP-9356

- Contracts in STDLIB and Kernel have been improved and type errors have been
  corrected.

  Own Id: OTP-9485

- Update documentation and specifications of some of the zlib functions.

  Own Id: OTP-9506

## Kernel 2.14.4

### Fixed Bugs and Malfunctions

- The send_timeout option in gen_tcp did not work properly in active mode or
  with \{active,once\} options. This is now corrected.

  Own Id: OTP-9145

- Fixed various typos across the documentation (Thanks to Tuncer Ayaz)

  Own Id: OTP-9154

- Fix typo in doc of rpc:pmap/3 (Thanks to Ricardo Catalinas Jiménez)

  Own Id: OTP-9168

- A bug in inet_res, the specialized DNS resolver, has been corrected. A late
  answer with unfortunate timing could cause a runtime exception. Some code
  cleanup and improvements also tagged along. Thanks to Evegeniy Khramtsov for a
  pinpointing bug report and bug fix testing.

  Own Id: OTP-9221 Aux Id: OTP-8712

### Improvements and New Features

- Types and specifications have been added.

  Own Id: OTP-9268

- Erlang types and specifications are used for documentation.

  Own Id: OTP-9272

- Two opaque types that could cause warnings when running Dialyzer have been
  modified.

  Own Id: OTP-9337

## Kernel 2.14.3

### Fixed Bugs and Malfunctions

- `os:find_executable/{1,2}` will no longer return the path of a directory that
  happens to be in the PATH.

  Own Id: OTP-8983 Aux Id: seq11749

- Fix -spec for file:write_file/3

  Change type for second parameter from binary() to iodata(), since the function
  explicitly takes steps to accept lists as well as binaries. (thanks to Magnus
  Henoch).

  Own Id: OTP-9067

- Sanitize the specs of the code module

  After the addition of unicode_binary() to the file:filename() type, dialyzer
  started complaining about erroneous or incomplete specs in some functions of
  the 'code' module. The culprit was hard-coded information in erl_bif_types for
  functions of this module, which were not updated. Since these functions have
  proper specs these days and code duplication (pun intended) is never a good
  idea, their type information was removed from erl_bif_types.

  While doing this, some erroneous comments were fixed in the code module and
  also made sure that the code now runs without dialyzer warnings even when the
  -Wunmatched_returns option is used.

  Some cleanups were applied to erl_bif_types too.

  Own Id: OTP-9100

- \- Add spec for function that does not return - Strengthen spec - Introduce
  types to avoid duplication in specs - Add specs for functions that do not
  return - Add specs for behaviour callbacks - Simplify two specs

  Own Id: OTP-9127

## Kernel 2.14.2

### Improvements and New Features

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

- There is now a new function inet:getifaddrs/0 modeled after C library function
  getifaddrs() on BSD and LInux that reports existing interfaces and their
  addresses on the host. This replaces the undocumented and unsupported
  inet:getiflist/0 and inet:ifget/2.

  Own Id: OTP-8926

## Kernel 2.14.1.1

### Fixed Bugs and Malfunctions

- In embedded mode, on_load handlers that called `code:priv_dir/1` or other
  functions in `code` would hang the system. Since the `crypto` application now
  contains an on_loader handler that calls `code:priv_dir/1`, including the
  `crypto` application in the boot file would prevent the system from starting.

  Also extended the `-init_debug` option to print information about on_load
  handlers being run to facilitate debugging.

  Own Id: OTP-8902 Aux Id: seq11703

## Kernel 2.14.1

### Fixed Bugs and Malfunctions

- Fixed: inet:setopts(S, \[\{linger,\{true,2\}\}]) returned \{error,einval\} for
  SCTP sockets. The inet_drv had a bug when checking the option size.

  Own Id: OTP-8726 Aux Id: seq11617

- gen_udp:connect/3 was broken for SCTP enabled builds. It did not detect remote
  end errors as it should.

  Own Id: OTP-8729

- reference() has been substituted for ref() in the documentation.

  Own Id: OTP-8733

- A bug introduced in kernel-2.13.5.3 has been fixed. If running
  `net_kernel:set_net_ticktime/1` twice within the `TransitionPerod` the second
  call caused the net_kernel process to crash with a `badmatch`.

  Own Id: OTP-8787 Aux Id: seq11657, OTP-8643

- inet:getsockopt for SCTP sctp_default_send_param had a bug to not initialize
  required feilds causing random answers. It is now corrected.

  Own Id: OTP-8795 Aux Id: seq11655

- For a socket in the HTTP packet mode, the return value from `gen_tcp:recv/2,3`
  if there is an error in the header will be `{ok,{http_error,String}}` instead
  of `{error,{http_error,String}}` to be consistent with `ssl:recv/2,3`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8831

### Improvements and New Features

- Even when configuring erlang with --enable-native-libs, the native code for
  modules loaded very early (such as lists) would not get loaded. This has been
  corrected. (Thanks to Paul Guyot.)

  Own Id: OTP-8750

- The undocumented function inet:ifget/2 has been improved to return interface
  hardware address (MAC) on platforms supporting getaddrinfo() (such as BSD
  unixes). Note it still does not work on all platforms for example not Windows
  nor Solaris, so the function is still undocumented.

  Buffer overflow and field init bugs for inet:ifget/2 and inet:getservbyname/2
  has also been fixed.

  Thanks to Michael Santos.

  Own Id: OTP-8816

- As a usability improvement the 'inet6' option to functions gen_tcp:listen/2,
  gen_tcp:connect/3-4, gen_udp:open/2 and gen_sctp:open/1-2 is now implicit if
  the address argument or the 'ip' option contain an IPv6 address (8-tuple).

  Own Id: OTP-8822

## Kernel 2.14

### Fixed Bugs and Malfunctions

- os:find_executable can now be fed with the complete name of the executable on
  Windows and still find it. I.e os:find_executable("werl.exe") will work as
  os:find_executable("werl").

  Own Id: OTP-3626

- The shell's line editing has been improved to more resemble the behaviour of
  readline and other shells. (Thanks to Dave Peticolas)

  Own Id: OTP-8635

- Under certain circumstances the net kernel could hang. (Thanks to Scott Lystig
  Fritchie.)

  Own Id: OTP-8643 Aux Id: seq11584

- The kernel DNS resolver was leaking one or two ports if the DNS reply could
  not be parsed or if the resolver(s) caused noconnection type errors. Bug now
  fixed. A DNS specification borderline truncated reply triggering the port
  leakage bug has also been fixed.

  Own Id: OTP-8652

### Improvements and New Features

- As of this version, the global name server no longer supports nodes running
  Erlang/OTP R11B.

  Own Id: OTP-8527

- The file module's functions write,read and read_line now handles named
  io_servers like 'standard_io' and 'standard_error' correctly.

  Own Id: OTP-8611

- The functions file:advise/4 and file:datasync/1 have been added. (Thanks to
  Filipe David Manana.)

  Own Id: OTP-8637

- When exchanging groups between nodes `pg2` did not remove duplicated members.
  This bug was introduced in R13B03 (kernel-2.13.4).

  Own Id: OTP-8653

- There is a new option 'exclusive' to file:open/2 that uses the OS O_EXCL flag
  where supported to open the file in exclusive mode.

  Own Id: OTP-8670

## Kernel 2.13.5.3

### Fixed Bugs and Malfunctions

- A bug introduced in Kernel 2.13.5.2 has been fixed.

  Own Id: OTP-8686 Aux Id: OTP-8643

## Kernel 2.13.5.2

### Fixed Bugs and Malfunctions

- Under certain circumstances the net kernel could hang. (Thanks to Scott Lystig
  Fritchie.)

  Own Id: OTP-8643 Aux Id: seq11584

## Kernel 2.13.5.1

### Fixed Bugs and Malfunctions

- A race condition in `os:cmd/1` could cause the caller to get stuck in
  `os:cmd/1` forever.

  Own Id: OTP-8502

## Kernel 2.13.5

### Fixed Bugs and Malfunctions

- A race bug affecting `pg2:get_local_members/1` has been fixed. The bug was
  introduced in R13B03.

  Own Id: OTP-8358

- The loading of native code was not properly atomic in the SMP emulator, which
  could cause crashes. Also a per-MFA information table for the native code has
  now been protected with a lock since it turns that it could be accessed
  concurrently in the SMP emulator. (Thanks to Mikael Pettersson.)

  Own Id: OTP-8397

- user.erl (used in oldshell) is updated to handle unicode in prompt strings
  (io:get_line/\{1,2\}). io_lib is also updated to format prompts with the 't'
  modifier (i.e. ~ts instead of ~s).

  Own Id: OTP-8418 Aux Id: OTP-8393

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

### Improvements and New Features

- The expected return value for an on_load function has been changed. (See the
  section about code loading in the Reference manual.)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8339

- Explicit top directories in archive files are now optional.

  For example, if an archive (app-vsn.ez) just contains an app-vsn/ebin/mod.beam
  file, the file info for the app-vsn and app-vsn/ebin directories are faked
  using the file info from the archive file as origin. The virtual directories
  can also be listed. For short, the top directories are virtual if they does
  not exist.

  Own Id: OTP-8387

- `code:clash/0` now looks inside archives (.ez files). (Thanks to Tuncer Ayaz.)

  Own Id: OTP-8413

- There are new `gen_sctp:connect_init/*` functions that initiate an SCTP
  connection without blocking for the result. The result is delivered
  asynchronously as an sctp_assoc_change event. (Thanks to Simon Cornish.)

  Own Id: OTP-8414

## Kernel 2.13.4

### Fixed Bugs and Malfunctions

- A link in `m:pg2` has been fixed. (Thanks to Christophe Romain.)

  Own Id: OTP-8198

- A ticker process could potentially be blocked indefinitely trying to send a
  tick to a node not responding. If this happened, the connection would not be
  brought down as it should.

  Own Id: OTP-8218

- A bug in `pg2` when members who died did not leave process groups has been
  fixed. (Thanks to Matthew Dempsky.)

  Own Id: OTP-8259

### Improvements and New Features

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8201

- The top directory in archive files does not need to have a `-vsn` suffix
  anymore. For example if the archive file has the name like `mnesia-4.4.7.ez`
  the top directory in the archive can either be named `mnesia` or
  `mnesia-4.4.7`. If the archive file has a name like `mnesia.ez` the top
  directory in the archive must be named `mnesia` as earlier.

  Own Id: OTP-8266

- The -on_load() directive can be used to run a function when a module is
  loaded. It is documented in the section about code loading in the Reference
  Manual.

  Own Id: OTP-8295

## Kernel 2.13.3

### Improvements and New Features

- The DNS resolver client inet_res has been rewritten, documented and released.
  See inet_res(3) and Erts User's Guide: Inet configuration.

  It can formally not be incompatible with respect to earlier versions since
  there was no earlier official version. However it was used before and some
  details have changed.

  Configuration now initializes from /etc/resolv.conf and /etc/hosts on all unix
  platforms regardless of which distribution mode the node is started in. The
  directory (/etc) these files are supposed to reside in can be changed via an
  environment variable. These configuration file locations can also be changed
  in the inet configuration. The files are monitored for change and re-read,
  which makes a few resolver configuration variables out of application control.
  The /etc/hosts entries have now their own cache table that is shadowed (with
  lookup method 'file' is used) by the application configured host entries. This
  problem (that inet_res configuration only worked for distribution mode long
  names) was among other reported by Matthew O'Gorman many moons ago.

  The lookup methods are still 'native' only per default. Resolver configuration
  is done on all Unix platforms just to get a usable configuration for direct
  calls to inet_res.

  The functions `inet_res:nslookup/3..5` and `inet_res:nnslookup/4..4` are no
  longer recommended to use, instead use `inet_res:lookup/3..5` and
  `inet_res:resolve/3..5` which provide clearer argument types and the
  possibility to override options in the call.

  Users of previous unsupported versions of inet_res have included internal
  header files to get to the internal record definitions in order to examine DNS
  replies. This is still unsupported and there are access functions in inet_dns
  to use instead. These are documented in inet_res(3).

  Bug fix: a compression reference loop would make DNS message decoding loop
  forever. Problem reported by Florian Weimer.

  Bug fix and patch suggestion by Sergei Golovan: configuring IPv6 nameservers
  did not work. His patch (as he warned) created many UDP sockets; one per
  nameserver. This has been fixed in the released version.

  Improvement: `inet_res` is now EDNS0 capable. The current implementation is
  simple and does not probe and cache EDNS info for nameservers, which a fully
  capable implementation probably should do. EDNS has to be enabled via resolver
  configuration, and if a nameserver replies that it does not support EDNS,
  `inet_res` falls back to a regular DNS query.

  Improvement: now `inet_res` automatically falls back to TCP if it gets a
  truncated answer from a nameserver.

  Warning: some of the ancient and exotic record types handled by `inet_res` and
  `inet_dns` are not supported by current versions of BIND, so they could not be
  tested after the rewrite, with reasonable effort, e.g MD, MF, NULL, and SPF.
  The risk for bugs in these particular records is still low since their code is
  mostly shared with other tested record types.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7955 Aux Id: OTP-7107 OTP-6852

- A TCP socket with option `{packet,4}` could crash the emulator if it received
  a packet header with a very large size value (>2Gb). The same bug caused
  `erlang:decode_packet/3` to return faulty values. (Thanks to Georgos Seganos.)

  Own Id: OTP-8102

- The file module has now a read_line/1 function similar to the io:get_line/2,
  but with byte oriented semantics. The function file:read_line/1 works for raw
  files as well, but for good performance it is recommended to use it together
  with the 'read_ahead' option for raw file access.

  Own Id: OTP-8108

## Kernel 2.13.2

### Fixed Bugs and Malfunctions

- A bug when doing io:get_line (among other calls) from a file opened with
  encoding other than latin1, causing false unicode errors to occur, is now
  corrected.

  Own Id: OTP-7974

### Improvements and New Features

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

## Kernel 2.13.1

### Fixed Bugs and Malfunctions

- Many concurrent calls to `os:cmd/1` will only block one scheduler thread at a
  time, making an smp emulator more responsive if the OS is slow forking
  processes.

  Own Id: OTP-7890 Aux Id: seq11219

- Fixed hanging early RPC that did IO operation during node start.

  Own Id: OTP-7903 Aux Id: seq11224

- The error behavior of gen_tcp and gen_udp has been corrected.
  gen_tcp:connect/3,4 and gen_udp:send/4 now returns \{error,eafnosupport\} for
  conflicting destination address versus socket address family. Other corner
  cases for IP address string host names combined with not using the native (OS)
  resolver (which is not default) has also been changed to return
  \{error,nxdomain\} instead of \{error,einval\}. Those changes just may
  surprise old existing code. gen_tcp:listen/2 and gen_udp:open/2 now fails for
  conflicting local address versus socket address family instead of trying to
  use an erroneous address. Problem reported by Per Hedeland.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7929

### Improvements and New Features

- Several glitches and performance issues in the Unicode and I/O-system
  implementation of R13A have been corrected.

  Own Id: OTP-7896 Aux Id: OTP-7648 OTP-7887

- The unsupported DNS resolver client inet_res has now been improved to handle
  NAPTR queries.

  Own Id: OTP-7925 Aux Id: seq11231

## Kernel 2.13

### Fixed Bugs and Malfunctions

- The old Erlang DNS resolver inet_res has been corrected to handle TXT records
  with more than one character string. Patch courtesy of Geoff Cant.

  Own Id: OTP-7588

- When chunk reading a disk log opened in read_only mode, bad terms could crash
  the disk log process.

  Own Id: OTP-7641 Aux Id: seq11090

- `gen_tcp:send()` did sometimes (only observed on Solaris) return
  `{error,enotconn}` instead of the expected `{error,closed}` as the peer socket
  had been explicitly closed.

  Own Id: OTP-7647

- The gen_sctp option sctp_peer_addr_params,
  #sctp_paddrparams\{address=\{IP,Port\} was erroneously decoded in the inet
  driver. This bug has now been corrected.

  Own Id: OTP-7755

### Improvements and New Features

- Erlang programs can now access STDERR on platforms where such a file
  descriptor is available by using the io_server 'standard_error', i.e.
  io:format(standard_error,"~s~n",\[ErrorMessage]),

  Own Id: OTP-6688

- The format of the string returned by `erlang:system_info(system_version)` (as
  well as the first message when Erlang is started) has changed. The string now
  contains the both the OTP version number as well as the erts version number.

  Own Id: OTP-7649

- As of this version, the global name server no longer supports nodes running
  Erlang/OTP R10B.

  Own Id: OTP-7661

- A `{nodedown, Node}` message passed by the `net_kernel:monitor_nodes/X`
  functionality is now guaranteed to be sent after `Node` has been removed from
  the result returned by `erlang:nodes/Y`.

  Own Id: OTP-7725

- The deprecated functions `erlang:fault/1`, `erlang:fault/2`, and
  `file:rawopen/2` have been removed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7812

- Nodes belonging to different independent clusters can now co-exist on the same
  host with the help of a new environment variable setting ERL_EPMD_PORT.

  Own Id: OTP-7826

- The copyright notices have been updated.

  Own Id: OTP-7851

## Kernel 2.12.5.1

### Fixed Bugs and Malfunctions

- When chunk reading a disk log opened in read_only mode, bad terms could crash
  the disk log process.

  Own Id: OTP-7641 Aux Id: seq11090

- Calling `gen_tcp:send()` from several processes on socket with option
  `send_timeout` could lead to much longer timeout than specified. The solution
  is a new socket option `{send_timeout_close,true}` that will do automatic
  close on timeout. Subsequent calls to send will then immediately fail due to
  the closed connection.

  Own Id: OTP-7731 Aux Id: seq11161

## Kernel 2.12.5

### Fixed Bugs and Malfunctions

- The documentation of `rpc:pmap/3` has been corrected. (Thanks to Kirill
  Zaborski.)

  Own Id: OTP-7537

- The listen socket used for the distributed Erlang protocol now uses the socket
  option 'reuseaddr', which is useful when you force the listen port number
  using kernel options 'inet_dist_listen_min' and 'inet_dist_listen_max' and
  restarts a node with open connections.

  Own Id: OTP-7563

- Fixed memory leak of unclosed TCP-ports. A gen_tcp:send() followed by a
  failing gen_tcp:recv() could in some cases cause the port to linger after
  being closed.

  Own Id: OTP-7615

### Improvements and New Features

- Processes spawned using `proc_lib` (including `gen_server` and other library
  modules that use `proc_lib`) no longer keep the entire argument list for the
  initial call, but only the arity.

  Also, if `proc_lib:spawn/1` is used to spawn a fun, the actual fun is not
  kept, but only module, function name, and arity of the function that
  implements the fun.

  The reason for the change is that keeping the initial fun (or a fun in an
  argument list), would prevent upgrading the code for the module. A secondary
  reason is that keeping the fun and function arguments could waste a
  significant amount of memory.

  The drawback with the change is that the crash reports will provide less
  precise information about the initial call (only `Module:Function/Arity`
  instead of `Module:Function(Arguments)`). The function
  `proc_lib:initial_call/1` still returns a list, but each argument has been
  replaced with a dummy atom.

  Own Id: OTP-7531 Aux Id: seq11036

- `io:get_line/1` when reading from standard input is now substantially faster.
  There are also some minor performance improvements in `io:get_line/1` when
  reading from any file opened in binary mode. (Thanks to Fredrik Svahn.)

  Own Id: OTP-7542

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

- `code:is_sticky/1` is now documented. (Thanks to Vlad Dumitrescu.)

  Own Id: OTP-7561

- In the job control mode, the "s" and "r" commands now take an optional
  argument to specify which shell to start. (Thanks to Robert Virding.)

  Own Id: OTP-7617

- `net_adm:world/0,1` could crash if called in an emulator that has not been
  started with either the `-sname` or `-name` option; now it will return an
  empty list. (Thanks to Edwin Fine.)

  Own Id: OTP-7618

## Kernel 2.12.4

### Fixed Bugs and Malfunctions

- Large files are now handled on Windows, where the filesystem supports it.

  Own Id: OTP-7410

### Improvements and New Features

- New BIF `erlang:decode_packet/3` that extracts a protocol packet from a
  binary. Similar to the socket option `{packet, Type}`. Also documented the
  socket packet type `http` and made it official. _NOTE_: The tuple format for
  `http` packets sent from an active socket has been changed in an incompatible
  way.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7404

- Setting the `{active,once}` for a socket (using inets:setopts/2) is now
  specially optimized (because the `{active,once}` option is typically used much
  more frequently than other options).

  Own Id: OTP-7520

## Kernel 2.12.3

### Fixed Bugs and Malfunctions

- SCTP_ADDR_CONFIRMED events are now handled by gen_sctp.

  Own Id: OTP-7276

- When leaving a process group with `pg2:leave/2` the process was falsely
  assumed to be a member of the group. This bug has been fixed.

  Own Id: OTP-7277

- In the Erlang shell, using up and down arrow keys, the wrong previous command
  could sometimes be retrieved.

  Own Id: OTP-7278

- The documentation for `erlang:trace/3` has been corrected.

  Own Id: OTP-7279 Aux Id: seq10927

- In the SMP emulator, there was small risk that `code:purge(Mod)` would kill a
  process that was running code in `Mod` and unload the module `Mod` before the
  process had terminated. `code:purge(Mod)` now waits for confirmation (using
  `erlang:monitor/2`) that the process has been killed before proceeding.

  Own Id: OTP-7282

- `zlib:inflate` failed when the size of the inflated data was an exact multiple
  of the internal buffer size (4000 bytes by default).

  Own Id: OTP-7359

### Improvements and New Features

- Additional library directories can now be specified in the environment
  variable ERL_LIBS. See the manual page for the `code` module. (Thanks to Serge
  Aleynikov.)

  Own Id: OTP-6940

- crypto and zlib drivers improved to allow concurrent smp access.

  Own Id: OTP-7262

- There is a new function `init:stop/1` which can be used to shutdown the system
  cleanly AND generate a non-zero exit status or crash dump. (Thanks to Magnus
  Froberg.)

  Own Id: OTP-7308

- The `hide` option for [`open_port/2`](`open_port/2`) is now documented.
  (Thanks to Richard Carlsson.)

  Own Id: OTP-7358

## Kernel 2.12.2.1

### Improvements and New Features

- `os:cmd/1` on unix platforms now use `/bin/sh` as shell instead of looking for
  `sh` in the `PATH` environment.

  Own Id: OTP-7283

## Kernel 2.12.2

### Fixed Bugs and Malfunctions

- A bug caused by a race condition involving `disk_log` and `pg2` has been
  fixed.

  Own Id: OTP-7209 Aux Id: seq10890

- The beta testing module `gen_sctp` now supports active mode as stated in the
  documentation. Active mode is still rather untested, and there are some issues
  about what should be the right semantics for `gen_sctp:connect/5`. In
  particular: should it be blocking or non-blocking or choosable. There is a
  high probability it will change semantics in a (near) future patch.

  Try it, give comments and send in bug reports\!

  Own Id: OTP-7225

### Improvements and New Features

- `erlang:system_info/1` now accepts the `logical_processors`, and
  `debug_compiled` arguments. For more info see the, `m:erlang` documentation.

  The scale factor returned by `test_server:timetrap_scale_factor/0` is now also
  effected if the emulator uses a larger amount of scheduler threads than the
  amount of logical processors on the system.

  Own Id: OTP-7175

- Updated the documentation for `erlang:function_exported/3` and `io:format/2`
  functions to no longer state that those functions are kept mainly for
  backwards compatibility.

  Own Id: OTP-7186

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

- [`tuple_size/1`](`tuple_size/1`) and [`byte_size/1`](`byte_size/1`) have been
  substituted for [`size/1`](`size/1`) in the documentation.

  Own Id: OTP-7244

## Kernel 2.12.1.2

### Improvements and New Features

- The `{allocator_sizes, Alloc}` and `alloc_util_allocators` arguments are now
  accepted by `erlang:system_info/1`. For more information see the `m:erlang`
  documentation.

  Own Id: OTP-7167

## Kernel 2.12.1.1

### Fixed Bugs and Malfunctions

- Fixed a problem in group that could cause the ssh server to lose answers or
  hang.

  Own Id: OTP-7185 Aux Id: seq10871

## Kernel 2.12.1

### Fixed Bugs and Malfunctions

- file:read/2 and file:consult_stream/1,3 did not use an empty prompt on I/O
  devices. This bug has now been corrected.

  Own Id: OTP-7013

- The sctp driver has been updated to work against newer lksctp packages e.g
  1.0.7 that uses the API spelling change adaption -> adaptation. Older lksctp
  (1.0.6) still work. The erlang API in gen_sctp.erl and inet_sctp.hrl now
  spells 'adaptation' regardless of the underlying C API.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7120

### Improvements and New Features

- The documentation has been updated so as to reflect the last updates of the
  Erlang shell as well as the minor modifications of the control sequence `p` of
  the `io_lib` module.

  Superfluous empty lines have been removed from code examples and from Erlang
  shell examples.

  Own Id: OTP-6944 Aux Id: OTP-6554, OTP-6911

- [`tuple_size/1`](`tuple_size/1`) and [`byte_size/1`](`byte_size/1`) have been
  substituted for [`size/1`](`size/1`).

  Own Id: OTP-7009

## Kernel 2.12

### Fixed Bugs and Malfunctions

- A bug for raw files when reading 0 bytes returning 'eof' instead of empty data
  has been corrected.

  Own Id: OTP-6291 Aux Id: OTP-6967

- A bug in gen_udp:fdopen reported by David Baird and also found by Dialyzer has
  been fixed.

  Own Id: OTP-6836 Aux Id: OTP-6594

- Calling `error_logger:tty(true)` multiple times does not give multiple error
  log printouts.

  Own Id: OTP-6884 Aux Id: seq10767

- The global name server now ignores `nodeup` messages when the command line
  flag `-connect_all false` has been used. (Thanks to Trevor Woollacott.)

  Own Id: OTP-6931

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

- Minor documentation corrections for file:pread/2 and file:pread/3.

  Own Id: OTP-6853

- The deprecated functions `file:file_info/1`, `init:get_flag/1`,
  `init:get_flags/0`, and `init:get_args/0` have been removed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6886

- Contract directives for modules in Kernel and STDLIB.

  Own Id: OTP-6895

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

- The undocumented and unsupported functions `inet:ip_to_bytes/1`,
  `inet:ip4_to_bytes/1`, `inet:ip6_to_bytes/1`, and `inet:bytes_to_ip6/16` have
  been removed.

  Own Id: OTP-6938

- Added new checksum combine functions to `zlib`. And fixed a bug in
  `zlib:deflate`. Thanks Matthew Dempsky.

  Own Id: OTP-6970

- The [`spawn_monitor/1`](`spawn_monitor/1`) and
  [`spawn_monitor/3`](`spawn_monitor/3`) BIFs are now auto-imported (i.e. they
  no longer need an `erlang:` prefix).

  Own Id: OTP-6975

- All functions in the `code` module now fail with an exception if they are
  called with obviously bad arguments, such as a tuple when an atom was
  expected. Some functions now also fail for undocumented argument types (for
  instance, `ensure_loaded/1` now only accepts an atom as documented; it used to
  accept a string too).

  Dialyzer will generally emit warnings for any calls that use undocumented
  argument types. Even if the call happens to still work in R12B, you should
  correct your code. A future release will adhere to the documentation.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6983

## Kernel 2.11.5.2

### Fixed Bugs and Malfunctions

- The kernel parameter dist_auto_connect once could fail to block a node if
  massive parallel sends were issued during a transient failure of network
  communication

  Own Id: OTP-6893 Aux Id: seq10753

## Kernel 2.11.5.1

### Fixed Bugs and Malfunctions

- The internal (rarely used) DNS resolver has been modified to not use the
  domain search list when asked to resolve an absolute name; a name with a
  terminating dot. There was also a bug causing it to create malformed DNS
  queries for absolute names that has been corrected, correction suggested by
  Scott Lystig Fritchie. The code has also been corrected to look up cached RRs
  in the same search order as non-cached, now allows having the root domain
  among the search domains, and can now actually do a zone transfer request.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6806 Aux Id: seq10714 EABln35459

- zlib:close/1 would leave an EXIT message in the message queue if the calling
  process had the trap_exit flag enabled.

  Own Id: OTP-6811

### Improvements and New Features

- The documentation of [`process_flag(priority, Level)`](`process_flag/2`) has
  been updated, see the `m:erlang` documentation.

  Own Id: OTP-6745 Aux Id: OTP-6715

## Kernel 2.11.5

### Fixed Bugs and Malfunctions

- The shell has been updated to fix the following flaws: Shell process exit left
  you with an unresponsive initial shell if not using oldshell. Starting a
  restricted shell with a nonexisting callback module resulted in a shell where
  no commands could be used, not even init:stop/0. Fun's could not be used as
  parameters to local shell functions (in shell_default or user_default) when
  restricted_shell was active.

  Own Id: OTP-6537

- The undocumented feature gen_tcp:fdopen/2 was broken in R11B-4. It is now
  fixed again.

  Own Id: OTP-6615

- Corrected cancellation of timers in three places in the inet_res module.
  (Problem found by Dialyzer.)

  Own Id: OTP-6676

### Improvements and New Features

- Corrected protocol layer flue for socket options SO_LINGER, SO_SNDBUF and
  SO_RCVBUF, for SCTP.

  Own Id: OTP-6625 Aux Id: OTP-6336

- The behaviour of the inet option \{active,once\} on peer close is improved and
  documented.

  Own Id: OTP-6681

- The inet option send_timeout for connection oriented sockets is added to allow
  for timeouts in communicating send requests to the underlying TCP stack.

  Own Id: OTP-6684 Aux Id: seq10637 OTP-6681

- Minor Makefile changes.

  Own Id: OTP-6689 Aux Id: OTP-6742

- The documentation of [`process_flag(priority, Level)`](`process_flag/2`) has
  been updated, see the `m:erlang` documentation.

  Own Id: OTP-6715

## Kernel 2.11.4.2

### Improvements and New Features

- process_flag/2 accepts the new flag `sensitive`.

  Own Id: OTP-6592 Aux Id: seq10555

## Kernel 2.11.4.1

### Fixed Bugs and Malfunctions

- A bug in gen_udp:open that broke the 'fd' option has been fixed.

  Own Id: OTP-6594 Aux Id: seq10619

## Kernel 2.11.4

### Fixed Bugs and Malfunctions

- Added a warning to the documentation for the `error_logger` functions
  `error_msg/1,2`, `warning_msg/1,2` and `info_msg/1,2` that calling these
  function with bad arguments can crash the standard event handler.

  Own Id: OTP-4575 Aux Id: seq7693

- A bug in `inet_db` concerning getting the resolver option `retry` has been
  corrected.

  Own Id: OTP-6380 Aux Id: seq10534

- Names registered by calling `global:register_name()` or
  `global:re_register_name()` were not always unregistered when the registering
  or registered process died. This bug has been fixed.

  Own Id: OTP-6428

- When setting the kernel configuration parameter `error_logger` to `false`, the
  documentation stated that "No error logger handler is installed". This is
  true, but error logging is not turned off, as the initial, primitive error
  logger event handler is kept, printing raw event messages to tty.

  Changing this behavior can be viewed as a backward incompatible change.
  Instead a new value `silent` for the configuration parameter has been added,
  which ensures that error logging is completely turned off.

  Own Id: OTP-6445

- Clarified the documentation for `code:lib_dir/1` and `code:priv_dir/1`. The
  functions traverse the names of the code path, they do not search the actual
  directories.

  Own Id: OTP-6466

- `io:setopts` returned `{error,badarg}`, when called with only an `expand_fun`
  argument. (Thanks to igwan.)

  Own Id: OTP-6508

### Improvements and New Features

- An interface towards the SCTP Socket API Extensions has been implemented.It is
  an Open Source patch courtesy of Serge Aleynikov and Leonid Timochouk. The
  Erlang code parts has been adapted by the OTP team, changing the Erlang API
  somewhat.

  The Erlang interface consists of the module `gen_sctp` and an include file
  `-include_lib("kernel/include/inet_sctp.hrl").` for option record definitions.
  The `gen_sctp` module is documented.

  The delivered Open Source patch, before the OTP team rewrites, was written
  according to
  [http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13](http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13)
  and was claimed to work fine, tested on Linux Fedora Core 5.0 (kernel
  2.6.15-2054 or later) and on Solaris 10 and 11. The OTP team rewrites used the
  same standard document but might have accidentally broken some functionality.
  If so, it will soon be patched to working state. The tricky parts in C and the
  general design has essentially not changed. During the rewrites the code was
  hand tested on SuSE Linux Enterprise Server 10, and briefly on Solaris 10.
  Feedbach on code and docs is very much appreciated.

  The SCTP interface is in beta state. It has only been hand tested and has no
  automatic test suites in OTP meaning everything is most certainly not tested.
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

- Monitoring of nodes has been improved. Now the following properties apply to
  `net_kernel:monitor_nodes/[1,2]`:

  - `nodeup` messages will be delivered before delivery of any message from the
    remote node passed through the newly established connection.
  - `nodedown` messages will not be delivered until all messages from the remote
    node that have been passed through the connection have been delivered.
  - Subscriptions can also be made before the `net_kernel` server has been
    started.

  Own Id: OTP-6481

- Setting and getting socket options in a "raw" fashion is now allowed. Using
  this feature will inevitably produce non portable code, but will allow setting
  ang getting arbitrary uncommon options on TCP stacks that do have them.

  Own Id: OTP-6519

- Dialyzer warnings have been eliminated.

  Own Id: OTP-6523

- The documentation for `file:delete/1` and `file:set_cwd/1` has been updated to
  clarify what happens if the input arguments are of an incorrect type.

  Own Id: OTP-6535

## Kernel 2.11.3.1

### Fixed Bugs and Malfunctions

- An erroneous packet size could be used for the first messages passed through a
  newly established connection between two Erlang nodes. This could cause
  messages to be discarded, or termination of the connection.

  Own Id: OTP-6473

## Kernel 2.11.3

### Fixed Bugs and Malfunctions

- On Unix, the `unix:cmd/1` function could leave an 'EXIT' message in the
  message queue for the calling process That problem was more likely to happen
  in an SMP emulator.

  Own Id: OTP-6368

### Improvements and New Features

- More interfaces are added in erl_ddll, to support different usage scenarios.

  Own Id: OTP-6307 Aux Id: OTP-6234

- Locks set by calling `global:set_lock()` were not always deleted when the
  locking process died. This bug has been fixed.

  Own Id: OTP-6341 Aux Id: seq10445

## Kernel 2.11.2

### Fixed Bugs and Malfunctions

- Behavior in case of disappeared nodes when using he dist_auto_connect once got
  changed in R11B-1. The timeouts regarding normal distributed operations is now
  reverted to the old (pre R11B-1).

  Own Id: OTP-6258 Aux Id: OTP-6200, seq10449

- Start-up problems for the internal process used by the `inet:gethostbyname()`
  functions were eliminated. If the internal process (`inet_gethost_native`) had
  not previously been started, and if several processes at the same time called
  one of the `inet:gethostbyname()` functions, the calls could fail.

  Own Id: OTP-6286

### Improvements and New Features

- Code cleanup: the old internal obsolete file_server has been removed. It was
  only used when communicating with R7 and older nodes.

  Own Id: OTP-6245

- Trying to open a non-existent or badly formed disk log no longer results in a
  crash report. In particular, `ets:file2tab/1` reports no error when the
  argument is not a well-formed disk log file. (The return value has not been
  changed, it is still an error tuple.)

  Own Id: OTP-6278 Aux Id: seq10421

- There are new BIFs `erlang:spawn_monitor/1,3`, and the new option `monitor`
  for `spawn_opt/2,3,4,5`.

  The `observer_backend` module has been updated to handle the new BIFs.

  Own Id: OTP-6281

- To help Dialyzer find more bugs, many functions in the Kernel and STDLIB
  applications now only accept arguments of the type that is documented.

  For instance, the functions `lists:prefix/2` and `lists:suffix/2` are
  documented to only accept lists as their arguments, but they actually accepted
  anything and returned `false`. That has been changed so that the functions
  cause an exception if one or both arguments are not lists.

  Also, the `string:strip/3` function is documented to take a character argument
  that is a character to strip from one or both ends of the string. Given a list
  instead of a character, it used to do nothing, but will now cause an
  exception.

  Dialyzer will find most cases where those functions are passed arguments of
  the wrong type.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6295

## Kernel 2.11.1.1

### Improvements and New Features

- There is now an option read_packets for UDP sockets that sets the maximum
  number of UDP packets that will be read for each invocation of the socket
  driver.

  Own Id: OTP-6249 Aux Id: seq10452

## Kernel 2.11.1

### Fixed Bugs and Malfunctions

- In R11B-0, the erl_ddll server process is always started. Despite that, the
  configuration parameter `start_ddll` for the Kernel application was still
  obeyed, which would cause the erl_ddll server to be started TWICE (and the
  system shutting down as a result). In this release, `start_ddll` is no longer
  used and its documentation has been removed.

  Own Id: OTP-6163

- The kernel option \{dist_auto_connect,once\} could block out nodes that had
  never been connected, causing persistent partitioning of networks.
  Furthermore, partial restarts of networks could cause inconsistent global name
  databases. Both problems are now solved.

  Own Id: OTP-6200 Aux Id: seq10377

### Improvements and New Features

- Late arriving tcp_closed and udp_closed messages are now removed from the
  message queue of a process calling gen_tcp:close/1, gen_udp:close/1, and
  inet:close/1.

  Own Id: OTP-6197

## Kernel 2.11

### Fixed Bugs and Malfunctions

- When repairing a disk log with a corrupt index file (caused by for instance a
  hard disk failure) the old contents of the index file is kept unmodified. This
  will make repeated attempts to open the disk log fail every time.

  Own Id: OTP-5558 Aux Id: seq9823

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
                  {'DOWN', Mon, process, Pid, _} -> ok
              %% We were previously guaranteed to get a down message
              %% (since we exited the process ourself), so we could
              %% in this case leave out:
              %% after 0 -> ok
              end,
  ```

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-5772

- The behavior when an application fails to start and possibly causes the
  runtime system to halt has been cleaned up, including fixing some minor bugs.

  `application_controller` should now always terminate with a non-nested string,
  meaning the slogan in an `erl_crash.dump` should always be easy to read.

  `init` now makes sure that the slogan passed to `erlang:halt/1` does not
  exceed the maximum allowed length.

  Redundant calls to [`list_to_atom/1`](`list_to_atom/1`) has been removed from
  the primitive `error_logger` event handler. (Thanks Serge Aleynikov for
  pointing this out).

  The changes only affects the contents of the error messages and crashdump file
  slogan.

  Own Id: OTP-5964

- The `erl_ddll` server is now started when OTP is started and placed under the
  Kernel supervisor. This fixes several minor issues. It used to be started on
  demand.

  The documentation for the `start` and `stop` functions in the `erl_ddll`
  module has been removed, as those functions are not meant to be used by other
  applications.

  Furthermore, the `erl_ddll:stop/1` function no longer terminates the
  `erl_ddll` server, as that would terminate the entire runtime system.

  Own Id: OTP-6033

### Improvements and New Features

- Removed some unused functions from `application_master`.

  Own Id: OTP-3889

- Global no longer allows the registration of a process under more than one
  name. If the old (buggy) behavior is desired the Kernel application variable
  `global_multi_name_action` can be given the value `allow`.

  Own Id: OTP-5640 Aux Id: OTP-5603

- The (slightly misleading) warnings that was shown when the `erlang.erl` file
  was compiled has been eliminated.

  Own Id: OTP-5947

- The `auth` module API is deprecated.

  Own Id: OTP-6037

- Added `erlang:demonitor/2`, making it possible to at the same time flush a
  received `'DOWN'` message, if there is one. See `m:erlang`.

  Own Id: OTP-6100 Aux Id: OTP-5772

## Kernel 2.10.13

### Fixed Bugs and Malfunctions

- Large files (more than 2 GBytes) are now handled on Solaris 8.

  Own Id: OTP-5849 Aux Id: seq10157

- During startup, a garbage `{'DOWN', ...}` message was left by
  `inet_gethost_native`, that caused problems for the starting code server.

  Own Id: OTP-5978 Aux Id: OTP-5974

### Improvements and New Features

- `global` now makes several attempts to connect nodes when maintaining the
  fully connected network. More than one attempt is sometimes needed under very
  heavy load.

  Own Id: OTP-5889

- `erl_epmd` now explicitly sets the timeout to `infinity` when calling
  `gen_server:call`. The old timeout of 15 seconds could time out under very
  heavy load.

  Own Id: OTP-5959

- Corrected the start of code server to use reference-tagged tuples to ensure
  that an unexpected message sent to the parent process does not cause a halt of
  the system. Also removed the useless `start/*` functions in both `code.erl`
  and `code_server.erl` and no longer exports the `init` function from
  `code_server.erl`.

  Own Id: OTP-5974 Aux Id: seq10243, OTP-5978

## Kernel 2.10.12

### Fixed Bugs and Malfunctions

- A bug in `global` has been fixed: the locker process added `nonode@nohost` to
  the list of nodes to lock. This could happen before any nodes got known to the
  global name server. Depending on net configuration the symptom was a delay.

  Own Id: OTP-5792 Aux Id: OTP-5563

- If an `.app` file is missing, the error reason returned by
  `application:load/1` has been corrected to
  `{"no such file or directory", "FILE.app"}`, instead of the less informative
  `{"unknown POSIX error","FILE.app"}`.

  Own Id: OTP-5809

- Bug fixes: `disk_log:accessible_logs/0` no longer reports all `pg2` process
  groups as distributed disk logs; `disk_log:pid2name/1` did not recognize
  processes of distributed disk logs.

  Own Id: OTP-5810

- The functions `file:consult/1`, `file:path_consult/2`, `file:eval/1,2`,
  `file:path_eval/2,3`, `file:script/1,2`, `file:path_script/2,3` now return
  correct line numbers in error tuples.

  Own Id: OTP-5814

- If there were user-defined variables in the boot script, and their values were
  not provided using the `-boot_var` option, the emulator would refuse to start
  with a confusing error message. Corrected to show a clear, understandable
  message.

  The `prim_file` module was modified to not depend on the `lists` module, to
  make it possible to start the emulator using a user-defined loader. (Thanks to
  Martin Bjorklund.)

  Own Id: OTP-5828 Aux Id: seq10151

- Minor corrections in the description of open modes. (Thanks to Richard
  Carlsson.)

  Own Id: OTP-5856

### Improvements and New Features

- `application_controller` now terminates with the actual error reason, instead
  of `shutdown`. This means that the crash dump now should be somewhat more
  informative, in the case where the runtime system is terminated due to an
  error in an application.

  Example: If the (permanent) application `app1` fails to start, the slogan now
  will be:
  "`Kernel pid terminated (application_controller) ({application_start_failure,app1,{shutdown, {app1,start,[normal,[]]}}})`"

  rather than the previous
  "`Kernel pid terminated (application_controller) (shutdown)`".

  Own Id: OTP-5811

## Kernel 2.10.11.1

### Fixed Bugs and Malfunctions

- Timers could sometimes timeout too early. This bug has now been fixed.

  Automatic cancellation of timers created by `erlang:send_after(Time,` pid(),
  Msg), and `erlang:start_timer(Time,` pid(), Msg) has been introduced. Timers
  created with the receiver specified by a pid, will automatically be cancelled
  when the receiver exits. For more information see the `m:erlang` man page.

  In order to be able to maintain a larger amount of timers without increasing
  the maintenance cost, the internal timer wheel and bif timer table have been
  enlarged.

  Also a number of minor bif timer optimizations have been implemented.

  Own Id: OTP-5795 Aux Id: OTP-5090, seq8913, seq10139, OTP-5782

### Improvements and New Features

- Documentation improvements:

  \- documentation for `erlang:link/1` corrected

  \- command line flag `-code_path_cache` added

  \- `erl` command line flags clarifications

  \- `m:net_kernel` clarifications

  Own Id: OTP-5847

## Kernel 2.10.11

### Fixed Bugs and Malfunctions

- Several bug fixes and improvements in the global name registration facility
  (see `m:global`):

  - the name resolving procedure did not always unlink no longer registered
    processes;
  - the global name could sometimes hang when a `nodedown` was immediately
    followed by a `nodeup`;
  - global names were not always unregistered when a node went down;
  - it is now possible to set and delete locks at the same time as the global
    name server is resolving names--the handling of global locks has been
    separated from registration of global names;

  As of this version, `global` no longer supports nodes running Erlang/OTP R7B
  or earlier.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-5563

- The functions `global:set_lock/3` and `global:trans/4` now accept the value
  `0` (zero) of the `Retries` argument.

  Own Id: OTP-5737

- The `inet:getaddr(Addr, Family)` no longer validates the `Addr` argument if it
  is a 4 or 8 tuple containing the IP address, except for the size of the tuple
  and that it contains integers in the correct range.

  The reason for the change is that validation could cause the following
  sequence of calls to fail:

  `{ok,Addr} = inet:getaddr(localhost, inet6), gen_tcp:connect(Addr, 7, [inet6])`

  Own Id: OTP-5743

### Improvements and New Features

- The previously undocumented and UNSUPPORTED `zlib` module has been updated in
  an incompatible way and many bugs have been corrected. It is now also
  documented.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-5715

- Added `application` interface functions `which_applications/1`, `set_env/4`
  and `unset_env/3`, which take an additional `Timeout` argument. To be used in
  situations where the standard gen_server timeout (5000ms) is not adequate.

  Own Id: OTP-5724 Aux Id: seq10083

- Improved documentation regarding synchronized start of applications with
  included applications (using start phases and `application_starter`).

  Own Id: OTP-5754

- New socket options `priority` and `tos` for platforms that support them
  (currently only Linux).

  Own Id: OTP-5756

- The global name server has been optimized when it comes to maintaining a fully
  connected network.

  Own Id: OTP-5770

## Kernel 2.10.10.1

### Fixed Bugs and Malfunctions

- The native resolver has gotten an control API for extended debugging and soft
  restart. It is: `inet_gethost_native:control(Control)`  
  `Control = {debug_level,Level} | soft_restart`  
  `Level = integer() in the range 0-4`.

  Own Id: OTP-5751 Aux Id: EABln25013

## Kernel 2.10.10

### Fixed Bugs and Malfunctions

- If several processes (at the same node) simultaneously tried to start the same
  distributed application, this could lead to `application:start` returning an
  erroneous value, or even hang.

  Own Id: OTP-5606 Aux Id: seq9838

### Improvements and New Features

- The manual pages for most of the Kernel and some of the STDLIB modules have
  been updated, in particular regarding type definitions.

  The documentation of the return value for `erts:info/1` has been corrected.

  The documentation for `erlang:statistics/1` now lists all possible arguments.

  Own Id: OTP-5360

- When the native resolver fails a `gethostbyaddr` lookup, `nxdomain` should be
  returned. There should be no attempt to fallback on a routine that succeeds if
  only the syntax of the IP address is valid. This has been fixed.

  Own Id: OTP-5598 Aux Id: OTP-5576

- Replaced some tuple funs with the new `fun M:F/A` construct.

  The high-order functions in the `lists` module no longer accept bad funs under
  any circumstances. '`lists:map(bad_fun, [])`' used to return '`[]`' but now
  causes an exception.

  Unused, broken compatibility code in the `ets` module was removed. (Thanks to
  Dialyzer.)

  Eliminated 5 discrepancies found by Dialyzer in the Appmon application.

  Own Id: OTP-5633

- The possibility to have comments following the list of tuples in a config file
  (file specified with the `-config` flag) has been added.

  Own Id: OTP-5661 Aux Id: seq10003

## Kernel 2.10.9

### Fixed Bugs and Malfunctions

- '`erl -config sys.config`' would fail to start if the `sys.config` file did
  not contain any whitespace at all after the dot. (Thanks to Anders Nygren.)

  Own Id: OTP-5543

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

### Improvements and New Features

- At startup, the Erlang resolver hosts table was used to look up the name of
  the local (and possibly stand alone) host. This was incorrect. The configured
  resolver method is now used for this purpose.

  Own Id: OTP-5393

- The `erlang:port_info/1` BIF is now documented. Minor corrections of the
  documentation for `erlang:port_info/2`.

  Added a note to the documentation of the `math` module that all functions are
  not available on all platforms.

  Added more information about the `+c` option in the `erl` man page in the ERTS
  documentation.

  Own Id: OTP-5555

- The new `fun M:F/A` construct creates a fun that refers to the latest version
  of `M:F/A.` This syntax is meant to replace tuple funs `{M,F}` which have many
  problems.

  The new type test [`is_function(Fun,A)`](`is_function/2`) (which may be used
  in guards) test whether `Fun` is a fun that can be applied with `A` arguments.
  (Currently, `Fun` can also be a tuple fun.)

  Own Id: OTP-5584

- According to the documentation `global` implements the equivalent of
  [`register/2`](`register/2`), which returns `badarg` if a process is already
  registered. As it turns out there is no check in `global` if a process is
  registered under more than one name. If some process is accidentally or by
  design given several names, it is possible that the name registry becomes
  inconsistent due to the way the resolve function is called when name clashes
  are discovered (see `register_name/3` in `m:global`).

  In OTP R11B `global` will not allow the registration of a process under more
  than one name. To help finding code where `no` will be returned, a Kernel
  application variable, `global_multi_name_action`, is hereby introduced.
  Depending on its value (`info`, `warning`, or `error`), messages are sent to
  the error logger when `global` discovers that some process is given more than
  one name. The variable only affects the node where it is defined.

  Own Id: OTP-5603

## Kernel 2.10.8

### Improvements and New Features

- In case of a DNS lookup loop, `inet_db:getbyname` ends up building an infinite
  list. This has been fixed.

  Own Id: OTP-5449

- When doing an `inet6` name lookup on an IPv4 address it was possible to get an
  address on IPv4 format back. This has been corrected. Some other minor
  inconsistencies regarding IPv6 name lookup have also been corrected.

  Own Id: OTP-5576

## Kernel 2.10.7

### Fixed Bugs and Malfunctions

- Under certain circumstances the `net_kernel` could emit spurious nodedown
  messages. This bug has been fixed.

  Own Id: OTP-5396

- Removed description of the `keep_zombies` configuration parameter in the
  `kernel` man page.

  Own Id: OTP-5497

### Improvements and New Features

- Eliminated Dialyzer warnings (caused by dead code) in the `init` and
  `prim_file` modules.

  Own Id: OTP-5496

- `inet_config` now also checks the environment variable `ERL_INETRC` for a
  possible user configuration file. See the ERTS User's Guide for details.

  Own Id: OTP-5512

## Kernel 2.10.6

### Improvements and New Features

- The `c` option for the `+B` flag has been introduced which makes it possible
  to use Ctrl-C (Ctrl-Break on Windows) to interrupt the shell process rather
  than to invoke the emulator break handler. All new `+B` options are also
  supported on Windows (werl) as of now. Furthermore, Ctrl-C on Windows has now
  been reserved for copying text (what Ctrl-Ins was used for previously).
  Ctrl-Break should be used for break handling. Lastly, the documentation of the
  system flags has been updated.

  Own Id: OTP-5388

- The possibility to start the Erlang shell in parallel with the rest of the
  system was reintroduced for backwards compatibility in STDLIB 1.13.1. The flag
  to be used for this is now called `async_shell_start` and has been documented.
  New shells started from the JCL menu are not synchronized with `init` anymore.
  This makes it possible to start a new shell (e.g. for debugging purposes) even
  if the initial shell has not come up.

  Own Id: OTP-5406 Aux Id: OTP-5218

## Kernel 2.10.5

### Fixed Bugs and Malfunctions

- Documentation for `erlang:binary_to_float/1` deleted. The BIF itself was
  removed several releases ago.

  Updated documentation for [`apply/2`](`apply/2`) and [`apply/3`](`apply/3`).

  Own Id: OTP-5391

### Improvements and New Features

- `net_kernel:monitor_nodes/2` which takes a flag and an option list has been
  added. By use of `net_kernel:monitor_nodes/2` one can subscribe for
  `nodeup/nodedown` messages with extra information. It is now possible to
  monitor hidden nodes, and get `nodedown` reason. See the `m:net_kernel`
  documentation for more information.

  Own Id: OTP-5374

## Kernel 2.10.4

### Fixed Bugs and Malfunctions

- The application master for an application now terminates the application
  faster, which reduces the risk for timeouts in other parts of the system.

  Own Id: OTP-5363 Aux Id: EABln19084

- A BIF `erlang:raise/3` has been added. See the manual for details. It is
  intended for internal system programming only, advanced error handling.

  Own Id: OTP-5376 Aux Id: OTP-5257

## Kernel 2.10.3

### Improvements and New Features

- With the `-eval` flag (`erl -eval Expr`), an arbitrary expression can be
  evaluated during system initialization. This is documented in `m:init`.

  Own Id: OTP-5260

- The unsupported and undocumented modules `socks5`, `socks5_auth`,
  `socks5_tcp`, and `socks5_udp` have been removed.

  Own Id: OTP-5266

## Kernel 2.10.1

### Fixed Bugs and Malfunctions

- The Pman 'trace shell' functionality was broken and has now been fixed.
  Furthermore, Pman could not correctly find the pid of the active shell if more
  than one shell process was running on the node. This has also been corrected.

  Own Id: OTP-5191

- The documentation for the `auth:open/1` function which no longer exists has
  been removed. (Thanks to Miguel Barreiro.)

  Own Id: OTP-5208

- Corrected the `crc32/3` function in the undocumented and unsupported `zlib`
  module.

  Own Id: OTP-5227

### Improvements and New Features

- You can now start Erlang with the `-rsh` flag which gives you a remote initial
  shell instead of a local one. Example:

  ```text
              erl -sname this_node -rsh other_node@other_host
  ```

  Own Id: OTP-5210

- If `/etc/hosts` specified two hosts with the same IP address (on separate
  lines), only the last host would be registered by inet_db during inet
  configuration. This has been corrected now so that both aliases are registered
  with the same IP address.

  Own Id: OTP-5212 Aux Id: seq7128

- The documentation for BIFs that take I/O lists have been clarified. Those are
  [`list_to_binary/1`](`list_to_binary/1`),
  [`port_command/2`](`port_command/2`), [`port_control/3`](`port_control/3`).

  Documentation for all `is_*` BIFs (such as [`is_atom/1`](`is_atom/1`)) has
  been added.

  Removed the documentation for `erlang:float_to_binary/2` which was removed
  from the run-time system several releases ago.

  Own Id: OTP-5222
