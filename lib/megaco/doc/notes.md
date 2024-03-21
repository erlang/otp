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
# Megaco Release Notes

This document describes the changes made to the Megaco system from version to
version. The intention of this document is to list all incompatibilities as well
as all enhancements and bugfixes for every release of Megaco. Each release of
Megaco thus constitutes one section in this document. The title of each section
is the version number of Megaco.

## Megaco 4.5

### Improvements and New Features

- Make megaco transports handle gen_tcp | gen_udp with socket backend on Windows
  (completion).

  Own Id: OTP-18599 Aux Id: OTP-18029

## Megaco 4.4.4

### Fixed Bugs and Malfunctions

- Removed configure option `--enable-sanitizers`. It was untested and broken.
  Address sanitizer for the emulator has better support by the `asan` build
  target.

  Own Id: OTP-18538 Aux Id: GH-7031, PR-7078

## Megaco 4.4.3

### Improvements and New Features

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

  Own Id: OTP-18432 Aux Id:
  GH-6672,PR-6793,PR-6784,PR-6787,PR-6785,PR-6682,PR-6800,PR-6797,PR-6798,PR-6799,PR-6796,PR-6813,PR-6671,PR-6673,PR-6684,PR-6694,GH-6677,PR-6696,PR-6670,PR-6674

## Megaco 4.4.2

### Improvements and New Features

- A very minor improvement to the measurement tool.

  Own Id: OTP-18298

## Megaco 4.4.1

### Fixed Bugs and Malfunctions

- Fixed various dialyzer related issues in the examples and the application
  proper.

  Own Id: OTP-18179 Aux Id: ERIERL-836

### Improvements and New Features

- There is a new configure option, `--enable-deterministic-build`, which will
  apply the `deterministic` compiler option when building Erlang/OTP. The
  `deterministic` option has been improved to eliminate more sources of
  non-determinism in several applications.

  Own Id: OTP-18165 Aux Id: PR-5965

## Megaco 4.4

### Improvements and New Features

- Input for `configure` scripts adapted to `autoconf` 2\.71.

  Own Id: OTP-17414 Aux Id: PR-4967

- Megaco test suite(s) use the new peer module for node starts.

  Own Id: OTP-17910

## Megaco 4.3

### Fixed Bugs and Malfunctions

- The compilation time is no longer recorded in BEAM files. There remained
  several undocumented functions that attempted to retrieve compilation times.
  Those have now been removed.

  Own Id: OTP-17962

### Improvements and New Features

- Update the performance and debug chapters of the megaco user's guide. Also
  some updates to the meas tools.

  Own Id: OTP-17839

## Megaco 4.2

### Improvements and New Features

- \[megaco_tcp] When connect fails, include more info in the error reason.

  Own Id: OTP-17817

## Megaco 4.1

### Improvements and New Features

- It is now possible to configure the built-in transports (megaco_tcp and megaco
  udp) to use the new (gen_udp- and gen_tcp-) option 'inet_backend'.

  Own Id: OTP-17533

## Megaco 4.0.1

### Fixed Bugs and Malfunctions

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## Megaco 4.0

### Improvements and New Features

- All the pre-v3 codec(s) (prev3a, prev3b and prev3c) was deprecated in
  OTP-23.0. They have now been removed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16560

- Removed deprecated functions marked for removal.

  Own Id: OTP-17049

## Megaco 3.19.5.1

### Fixed Bugs and Malfunctions

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## Megaco 3.19.5

### Fixed Bugs and Malfunctions

- Fixed usage of `AC_CONFIG_AUX_DIRS()` macros in configure script sources.

  Own Id: OTP-17093 Aux Id: ERL-1447, PR-2948

## Megaco 3.19.4

### Fixed Bugs and Malfunctions

- Empty statistics descriptor (now) allowed in both encode and decode for
  version 3.

  Own Id: OTP-17012 Aux Id: ERL-1405

## Megaco 3.19.3

### Fixed Bugs and Malfunctions

- The expected number of warnings when (yecc) generating v2 and v3 (text)
  parser's was incorrect.

  Own Id: OTP-16836

## Megaco 3.19.2

### Fixed Bugs and Malfunctions

- The v2 and v3 parsers could not properly decode some IPv6 addresses.

  Own Id: OTP-16818 Aux Id: ERIERL-526

## Megaco 3.19.1

### Fixed Bugs and Malfunctions

- The mini parser could not properly decode some IPv6 addresses.

  Own Id: OTP-16631 Aux Id: ERIERL-491

## Megaco 3.19

### Improvements and New Features

- Refactored the internal handling of deprecated and removed functions.

  Own Id: OTP-16469

- The preliminary version 3 codec(s) prev3a, prev3b and prev3c has been
  deprecated and will be _removed_ in OTP 24. The encoding config option
  'version3' will continue to work until OTP 24.

  Own Id: OTP-16531

## Megaco 3.18.8.4

### Fixed Bugs and Malfunctions

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## Megaco 3.18.8.3

### Fixed Bugs and Malfunctions

- Empty statistics descriptor (now) allowed in both encode and decode for
  version 3.

  Own Id: OTP-17012 Aux Id: ERL-1405

## Megaco 3.18.8.2

### Fixed Bugs and Malfunctions

- The v2 and v3 parsers could not properly decode some IPv6 addresses.

  Own Id: OTP-16818 Aux Id: ERIERL-526

## Megaco 3.18.8.1

### Fixed Bugs and Malfunctions

- The mini parser could not properly decode some IPv6 addresses.

  Own Id: OTP-16631 Aux Id: ERIERL-491

## Megaco 3.18.8

### Fixed Bugs and Malfunctions

- The documented function megaco:get_sdp_record_from_PropertGroup/2 was a
  wrapper for megaco_sdp:get_sdp_record_from_PropertGroup/2 but did not actually
  exist. This has now been fixed.

  Own Id: OTP-16449

### Improvements and New Features

- Test suite completely reworked. Add (timestamp) utility functions for
  debugging and testing.

  Own Id: OTP-16158

## Megaco 3.18.7

### Fixed Bugs and Malfunctions

- The meas example had not been updated for a long time, which caused it to not
  work. Also, it made use of deprecated functions (now()). This has now been
  fixed.

  Own Id: OTP-16061

## Megaco 3.18.6

### Fixed Bugs and Malfunctions

- Fix various minor issues related to Dialyzer. Mostly these are dialyzer
  warnings, but there was also some minor bugs detected by Dialyzer.

  Own Id: OTP-15882

## Megaco 3.18.5

### Improvements and New Features

- Minor updates to build system necessary due to move of configuration of
  `crypto` application.

  Own Id: OTP-15262 Aux Id: OTP-15129

- Minor adjustments made to build system for parallel configure.

  Own Id: OTP-15340 Aux Id: OTP-14625

## Megaco 3.18.4

### Fixed Bugs and Malfunctions

- Improved documentation.

  Own Id: OTP-15190

## Megaco 3.18.3

### Fixed Bugs and Malfunctions

- Removed all old unused files in the documentation.

  Own Id: OTP-14475 Aux Id: ERL-409, PR-1493

## Megaco 3.18.2

### Improvements and New Features

- Typos have been fixed.

  Own Id: OTP-14387

## Megaco 3.18.1

### Improvements and New Features

- Internal changes

  Own Id: OTP-13551

## Megaco 3.18

### Improvements and New Features

- The runtime dependencies in the application resource file have been updated.

  Own Id: OTP-12762

## Megaco 3.17.3

### Improvements and New Features

- Distribute `autoconf` helpers to applications at build time instead of having
  multiple identical copies committed in the repository.

  Own Id: OTP-12348

## Megaco 3.17.2

### Fixed Bugs and Malfunctions

- Implement --enable-sanitizers\[=sanitizers]. Similar to debugging with
  Valgrind, it's very useful to enable -fsanitize= switches to catch bugs at
  runtime.

  Own Id: OTP-12153

## Megaco 3.17.1

### Improvements and New Features

- The default encoding of Erlang files has been changed from ISO-8859-1 to
  UTF-8.

  The encoding of XML files has also been changed to UTF-8.

  Own Id: OTP-10907

## Megaco 3.17.0.3

### Improvements and New Features

- Updated doc files to utf8.

  Own Id: OTP-10907

## Megaco 3.17.0.2

### Improvements and New Features

- Introduced functionality for inspection of system and build configuration.

  Own Id: OTP-11196

## Megaco 3.17.0.1

### Improvements and New Features

- Postscript files no longer needed for the generation of PDF files have been
  removed.

  Own Id: OTP-11016

## Megaco 3.17

Version 3.17 supports code replacement in runtime from/to version 3.16.0.3 and
and 3.16.0.2.

### Improvements and new features

-

### Fixed bugs and malfunctions

- Buffer overrun error while flex scanner processing property parm groups.

  This error occured only for large messages if a buffer realloc was needed
  while processing the property parm groups.

  Own Id: OTP-10998

  Aux Id: Seq 12263

### Incompatibilities

-

## Megaco 3.16.0.3

Version 3.16.0.2 supports code replacement in runtime from/to version 3.16.0.1,
3.16, 3.15.1.1, 3.15.1 and 3.15.

### Improvements and new features

- Where necessary, a comment stating encoding has been added to Erlang files.
  The comment is meant to be removed in Erlang/OTP R17B when UTF-8 becomes the
  default encoding.

  Own Id: OTP-10630

### Fixed bugs and malfunctions

-

### Incompatibilities

- A number of binary encoding alternatives has been removed. The binary encoding
  option `driver` has been removed since this (the use of the asn1 linked in
  driver) is now default and there is now way to _not_ use it. See
  [configuration of binary encoding](megaco_encode.md#binary_config) for more
  info.

## Megaco 3.16.0.2

Version 3.16.0.2 supports code replacement in runtime from/to version 3.16.0.1,
3.16, 3.15.1.1, 3.15.1 and 3.15.

### Improvements and new features

- Allow whitespaces in installation path.

  It is now possible to give configure and make an installation/release path
  with whitespaces in it.

  Own Id: OTP-10107

- Fix parallel make for behaviours.
- Removed use of deprecated system flag, `global_haeps_size`, in the measurement
  tool `mstone1`.

### Fixed bugs and malfunctions

-

### Incompatibilities

-

## Megaco 3.16.0.1

Version 3.16.0.1 supports code replacement in runtime from/to version 3.16,
3.15.1.1, 3.15.1 and 3.15.

### Improvements and new features

- Fixed some faulty test cases.
- Removed use of deprecated system flag, `scheduler_bind_type`, in the
  measurement tool `mstone1`.

  Own Id: OTP-9949

### Fixed bugs and malfunctions

-

### Incompatibilities

-

## Megaco 3.16

Version 3.16 supports code replacement in runtime from/to version 3.15.1.1,
3.15.1 and 3.15.

### Improvements and new features

- Minor improvements to the measurement tool `mstone1`.

  Own Id: OTP-9604

- ASN.1 no longer makes use of a driver to accelerate encode/decode, instead it
  uses NIFs. The encoding config option is _still_ the same, i.e. `driver`.

  Own Id: OTP-9672

- The profiling test tool has been rewritten.

  Håkan Mattsson

  Own Id: OTP-9679

- The flex driver has been updated to support the new driver format (changed to
  enable 64-bit aware drivers).

  Own Id: OTP-9795

### Fixed bugs and malfunctions

-

### Incompatibilities

-

## Megaco 3.15.1.1

Version 3.15.1.1 supports code replacement in runtime from/to version 3.15.1 and
3.15.

### Improvements and new features

- Correct various XML errors.

  Own Id: OTP-9550

### Fixed bugs and malfunctions

-

## Megaco 3.15.1

Version 3.15.1 supports code replacement in runtime from/to version 3.15,
3.14.1.1, 3.14.1 and 3.14.

### Improvements and new features

-

### Fixed bugs and malfunctions

- Fixing miscellaneous things detected by dialyzer.

  Own Id: OTP-9075

## Megaco 3.15

### Improvements and New Features

- Fixing auto-import issues.

  Own Id: OTP-8842

### Fixed bugs and malfunctions

-

## Megaco 3.14.1.1

Version 3.14.1.1 supports code replacement in runtime from/to version 3.14.1,
3.14, 3.13, 3.12 and 3.11.3.

### Improvements and new features

- Updated the [performance](megaco_performance.md) chapter.

  Own Id: OTP-8696

### Fixed bugs and malfunctions

-

## Megaco 3.14.1

Version 3.14.1 supports code replacement in runtime from/to version 3.14, 3.13,
3.12 and 3.11.3.

### Improvements and new features

- A minor compiler related performance improvement.

  Own Id: OTP-8561

### Fixed bugs and malfunctions

- A race condition when, during high load, processing both the original and a
  resent message and delivering this as two separate messages to the user.

  Note that this solution only protects against multiple reply deliveries\!

  Own Id: OTP-8529

  Aux Id: Seq 10915

- Fix shared libraries installation.

  The flex shared lib(s) were incorrectly installed as data files.

  Peter Lemenkov

  Own Id: OTP-8627

- Eliminated a possible race condition while creating pending counters.

  Own Id: OTP-8634

  Aux Id: Seq 11579

## Megaco 3.14

Version 3.14 supports code replacement in runtime from/to version 3.13, 3.12 and
3.11.3.

### Improvements and new features

- Various changes to configure and makefile(s) to facilitate cross compilation
  (and other build system improvements).

  Own Id: OTP-8323

- Added a help target in the test Makefile to explain the most useful make
  targets, used when testing the application using the test-server provided with
  megaco.

  Own Id: OTP-8362

- Adapted megaco_filter to the new internal format.

  Own Id: OTP-8403

### Fixed bugs and malfunctions

- Callbacks, when the callback module is unknown (undefined), results in warning
  messages.

  A race condition scenario. As part of a cancelation operation, replies with
  waiting acknowledgements is cancelled. This includes informing the user (via a
  call to the handle_trans_ack callback function). It is possible that at this
  point the connection data has been removed, which makes it impossible for
  megaco to perform this operation, resulting in the warning message. The
  solution is to also store the callback module with the other reply
  information, to be used when cleaning up after a cancelation.

  Own Id: OTP-8328

  Aux Id: Seq 11384

## Megaco 3.13

Version 3.13 supports code replacement in runtime from/to version 3.12 and
3.11.3.

### Improvements and new features

- A minor optimization by using ets:update_element instead of ets:insert for
  some table updates.

  Own Id: OTP-8239

- The documentation is now built with open source tools (_xsltproc_ and _fop_)
  that exists on most platforms. One visible change is that the frames are
  removed.

  Own Id: OTP-8249

### Fixed bugs and malfunctions

-

### Incompatibilities

-
