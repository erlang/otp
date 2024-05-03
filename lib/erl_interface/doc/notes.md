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
# Erl_interface Release Notes

This document describes the changes made to the Erl_interface application.

## Erl_Interface 5.5.1

### Fixed Bugs and Malfunctions

* Fix bug where the system installed openssl/md5.h would be confused with the vendored md5.h.

  Own Id: OTP-18931 Aux Id: GH-7987 PR-7989

## Erl_Interface 5.5

### Fixed Bugs and Malfunctions

- Fixed some benign compile warnings on Windows.

  Own Id: OTP-18895

### Improvements and New Features

- Add support to encode maps with `ei_x_format`.

  Own Id: OTP-18764 Aux Id: PR-7602

- Replaced old md5 implementation with an implementation from OpenSSL.

  Own Id: OTP-18877

### Known Bugs and Problems

- The `ei` API for decoding/encoding terms is not fully 64-bit compatible since
  terms that have a representation on the external term format larger than 2 GB
  cannot be handled.

  Own Id: OTP-16607 Aux Id: OTP-16608

## Erl_Interface 5.4

### Improvements and New Features

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

### Known Bugs and Problems

- The `ei` API for decoding/encoding terms is not fully 64-bit compatible since
  terms that have a representation on the external term format larger than 2 GB
  cannot be handled.

  Own Id: OTP-16607 Aux Id: OTP-16608

## Erl_Interface 5.3.2.1

### Improvements and New Features

- Replaced old md5 implementation with an implementation from OpenSSL.

  Own Id: OTP-18877

## Erl_Interface 5.3.2

### Fixed Bugs and Malfunctions

- Fixed configure tests for a few ARM-specific instructions, which prevented the
  emulator from being built on some platforms.

  Own Id: OTP-18554

## Erl_Interface 5.3.1

### Fixed Bugs and Malfunctions

- Accept connection setup from OTP 23 and 24 nodes that are not using epmd.

  Own Id: OTP-18404 Aux Id: GH-6595, PR-6625

### Known Bugs and Problems

- The `ei` API for decoding/encoding terms is not fully 64-bit compatible since
  terms that have a representation on the external term format larger than 2 GB
  cannot be handled.

  Own Id: OTP-16607 Aux Id: OTP-16608

## Erl_Interface 5.3

### Fixed Bugs and Malfunctions

- `erl_call` no longer links against `nsl` on platforms where `gethostbyname` is
  provided by libc.

  Own Id: OTP-17846 Aux Id: PR-5558

### Improvements and New Features

- The following distribution flags are now mandatory: `DFLAG_BIT_BINARIES`,
  `DFLAG_EXPORT_PTR_TAG`, `DFLAG_MAP_TAGS`, `DFLAG_NEW_FLOATS`, and
  `DFLAG_FUN_TAGS`. This mainly concerns libraries or application that implement
  the distribution protocol themselves.

  Own Id: OTP-17318 Aux Id: PR-4972

- Input for `configure` scripts adapted to `autoconf` 2\.71.

  Own Id: OTP-17414 Aux Id: PR-4967

- Removed use of node creation value zero as a wildcard. Also prevent zero from
  being used as creation by `erl_interface` and `jinterface` nodes.

  Own Id: OTP-17682 Aux Id: PR-5347

- Changed `creation` arguments, of function `ei_connect_init` and friends, from
  type `short` to `unsigned int` for full 32-bit range.

  Own Id: OTP-17802 Aux Id: PR-5347

### Known Bugs and Problems

- The `ei` API for decoding/encoding terms is not fully 64-bit compatible since
  terms that have a representation on the external term format larger than 2 GB
  cannot be handled.

  Own Id: OTP-16607 Aux Id: OTP-16608

## Erl_Interface 5.2.2.1

### Improvements and New Features

- Replaced old md5 implementation with an implementation from OpenSSL.

  Own Id: OTP-18877

## Erl_Interface 5.2.2

### Fixed Bugs and Malfunctions

- Avoid attempt build dynamic libs if config option `--enable-ei-dynamic-lib` is
  not given.

  Own Id: OTP-17987 Aux Id: GH-5781, PR-5787

## Erl_Interface 5.2.1

### Fixed Bugs and Malfunctions

- Fix compile error regarding `gethostbyaddr_r` on Android. Error introduced in
  OTP 24.3.

  Own Id: OTP-17975 Aux Id: PR-5763

## Erl_Interface 5.2

### Improvements and New Features

- Add `--enable-ei-dynamic-lib` configure option that will make erl_interface
  also release a dynamic library version of libei.

  Own Id: OTP-17883 Aux Id: PR-5601 ERIERL-724

### Known Bugs and Problems

- The `ei` API for decoding/encoding terms is not fully 64-bit compatible since
  terms that have a representation on the external term format larger than 2 GB
  cannot be handled.

  Own Id: OTP-16607 Aux Id: OTP-16608

## Erl_Interface 5.1

### Improvements and New Features

- erl_call now prints an error when the arguments cannot be parsed.

  Own Id: OTP-17626

### Known Bugs and Problems

- The `ei` API for decoding/encoding terms is not fully 64-bit compatible since
  terms that have a representation on the external term format larger than 2 GB
  cannot be handled.

  Own Id: OTP-16607 Aux Id: OTP-16608

## Erl_Interface 5.0.1

### Fixed Bugs and Malfunctions

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## Erl_Interface 5.0

### Fixed Bugs and Malfunctions

- Two options have been added to `erl_call`. The `-fetch_stdout` option fetches
  stdout data resulting from the code invoked by `erl_call`. The `-fetch_stdout`
  option disables printing of the result term. In order to implement the first
  of these two options a new function called `ei_xrpc_from` has been added to
  erl_interface. For details see the `erl_call` documentation and
  `erl_interface` documentation.

  Own Id: OTP-17132

### Improvements and New Features

- Accept 64-bit process identifiers from external nodes. This is the first step
  in an upgrade path toward using 64-bit pids in a future OTP release.

  Own Id: OTP-16720 Aux Id: PR-2680

- The experimental HiPE application has been removed, together with all related
  functionality in other applications.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16963

- The `registry` functionality part of `erl_interface` has been removed. It was
  as of OTP 23 deprecated and scheduled for removal in OTP 24.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16970

- Accept references up to a size of 160-bits from remote nodes. This is the
  first step in an upgrade path toward using references up to 160-bits in a
  future OTP release.

  Own Id: OTP-17005 Aux Id: OTP-16718

- Accept 64-bit port identifiers from external nodes. This is the first step in
  an upgrade path toward using 64-bit port identifiers in a future OTP release.

  Own Id: OTP-17007

- Support the new link protocol in order to be able to phase out the old link
  protocol in the future. `erl_interface` does not support setting up or
  removing links from the `erl_interface` side, so the bug present with the old
  protocol did not effect `erl_interface`. This since both participants of a
  link simultaneously needed to operate on the link in order to trigger the bug.

  Own Id: OTP-17270 Aux Id: OTP-17127

### Known Bugs and Problems

- The `ei` API for decoding/encoding terms is not fully 64-bit compatible since
  terms that have a representation on the external term format larger than 2 GB
  cannot be handled.

  Own Id: OTP-16607 Aux Id: OTP-16608

## Erl_Interface 4.0.3.1

### Fixed Bugs and Malfunctions

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

### Known Bugs and Problems

- The `ei` API for decoding/encoding terms is not fully 64-bit compatible since
  terms that have a representation on the external term format larger than 2 GB
  cannot be handled.

  Own Id: OTP-16607 Aux Id: OTP-16608

## Erl_Interface 4.0.3

### Fixed Bugs and Malfunctions

- Fix bug where sending of large data with ei*send*\*/ei_rpc with infinite
  timeout could fail when the tcp buffer becomes full.

  Fault has existed since OTP-21.

  Own Id: OTP-17358 Aux Id: ERLERL-610

### Known Bugs and Problems

- The `ei` API for decoding/encoding terms is not fully 64-bit compatible since
  terms that have a representation on the external term format larger than 2 GB
  cannot be handled.

  Own Id: OTP-16607 Aux Id: OTP-16608

## Erl_Interface 4.0.2.1

### Fixed Bugs and Malfunctions

- Fix bug where sending of large data with ei*send*\*/ei_rpc with infinite
  timeout could fail when the tcp buffer becomes full.

  Fault has existed since OTP-21.

  Own Id: OTP-17358 Aux Id: ERLERL-610

## Erl_Interface 4.0.2

### Fixed Bugs and Malfunctions

- Integers outside of the range \[-(1 bsl 32) - 1, (1 bsl 32) -1] were
  previously intended to be printed in an internal bignum format by
  `ei_print_term()` and `ei_s_print_term()`. Unfortunately the implementation
  has been buggy since OTP R13B02 and since then produced results with random
  content which also could crash the calling program.

  This fix replaces the printing of the internal format with printing in
  hexadecimal form and extend the range for printing in decimal form. Currently
  integers in the range \[-(1 bsl 64), (1 bsl 64)] are printed in decimal form
  and integers outside of this range in Erlang hexadecimal form.

  Own Id: OTP-17099 Aux Id: ERIERL-585

### Known Bugs and Problems

- The `ei` API for decoding/encoding terms is not fully 64-bit compatible since
  terms that have a representation on the external term format larger than 2 GB
  cannot be handled.

  Own Id: OTP-16607 Aux Id: OTP-16608

## Erl_Interface 4.0.1

### Fixed Bugs and Malfunctions

- Fix erl_interface on windows to be compiled with correct flags to make
  internal primitives reentrant.

  Own Id: OTP-16740

- Fixed `ei_get_type` to set `*size` to zero for floats, pids, port and refs
  according to documentation.

  Own Id: OTP-16753 Aux Id: ERL-1288, PR-2678

- Fix ei_connect when using a dynamic node name to force usage of distribution
  version 6.

  This bug caused `erl_call -R -address` to not work properly.

  Own Id: OTP-16786

### Improvements and New Features

- Changes in order to build on the Haiku operating system.

  Thanks to Calvin Buckley

  Own Id: OTP-16707 Aux Id: PR-2638

### Known Bugs and Problems

- The `ei` API for decoding/encoding terms is not fully 64-bit compatible since
  terms that have a representation on the external term format larger than 2 GB
  cannot be handled.

  Own Id: OTP-16607 Aux Id: OTP-16608

## Erl_Interface 4.0

### Fixed Bugs and Malfunctions

- Fix various compiler warnings on 64-bit Windows.

  Own Id: OTP-15800

- `erl_call` will now work properly on systems that cannot resolve their own
  hostname.

  Own Id: OTP-16604

- Various bug fixes:

  - Internal error checking in various functions.
  - [`ei_rpc()`](ei_connect.md#ei_rpc) accepted any 2-tuple message as an rpc
    response.
  - [`ei_decode_ref()`](ei.md#ei_decode_ref) now refuse to write outside of
    allocated memory in case a huge reference is decoded.
  - [`ei_decode_ei_term()`](ei.md#ei_decode_ei_term) now reports the same term
    types as [`ei_get_type()`](ei.md#ei_get_type).

  Own Id: OTP-16623

### Improvements and New Features

- A client node can receive its node name dynamically from the node that it
  first connects to. This featured can by used by

  - starting with `erl -sname undefined`
  - erl_interface functions `ei_connect_init` and friends
  - `erl_call -R`

  Own Id: OTP-13812

- Increased size of node incarnation numbers (aka "creation"), from 2 bits to 32
  bits. This will reduce the risk of pids/ports/refs, from different node
  incarnation with the same name, being mixed up.

  Own Id: OTP-15603

- Fix various build issues when compiling Erlang/OTP to the IBM AIX platform.

  Own Id: OTP-15866 Aux Id: PR-2110

- Improved node connection setup handshake protocol. Made possible to agree on
  protocol version without dependence on `epmd` or other prior knowledge of peer
  node version. Also added exchange of node incarnation ("creation") values and
  expanded the distribution capability flag field from 32 to 64 bits.

  Own Id: OTP-16229

- New `erl_call` option `-address [Host]:Port` to connect directly to a node
  without being dependent on `epmd` to resolve the node name.

  Own Id: OTP-16251

- As announced in OTP 22.0, the deprecated parts of `erl_interface` have now
  been removed (essentially all C functions with prefix `erl_`).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16328

- As announced in OTP 22.0, the previously existing limited support for VxWorks
  has now been removed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16329 Aux Id: OTP-15621

- New function `ei_connect_host_port` and friends to allow node connection
  without being dependent on `epmd` for node name resolution.

  Own Id: OTP-16496 Aux Id: OTP-16251

- A number of new functions have been added to the `erl_interface` API:

  - [`ei_cmp_pids()`](ei.md#ei_cmp_pids)
  - [`ei_cmp_ports()`](ei.md#ei_cmp_ports)
  - [`ei_cmp_refs()`](ei.md#ei_cmp_refs)
  - [`ei_decode_iodata()`](ei.md#ei_decode_iodata)
  - [`ei_make_pid()`](ei_connect.md#ei_make_pid)
  - [`ei_make_ref()`](ei_connect.md#ei_make_ref)

  Own Id: OTP-16594

- Added a `-timeout` option to `erl_call`.

  Own Id: OTP-16624

- The `erl_interface` `registry` functionality is deprecated as of OTP 23, and
  will be removed in OTP 24. Reasonably new `gcc` compilers will issue
  deprecation warnings when using this functionality. In order to disable these
  warnings, define the macro `EI_NO_DEPR_WARN`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16630

- Documentation improvements.

  Own Id: OTP-16633

### Known Bugs and Problems

- The `ei` API for decoding/encoding terms is not fully 64-bit compatible since
  terms that have a representation on the external term format larger than 2 GB
  cannot be handled.

  Own Id: OTP-16607 Aux Id: OTP-16608

## Erl_Interface 3.13.2.2

### Fixed Bugs and Malfunctions

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

### Known Bugs and Problems

- The `ei` API for decoding/encoding terms is not fully 64-bit compatible since
  terms that have a representation on the external term format larger than 2 GB
  cannot be handled.

  Own Id: OTP-16607 Aux Id: OTP-16608

## Erl_Interface 3.13.2.1

### Fixed Bugs and Malfunctions

- Fix bug where sending of large data with ei*send*\*/ei_rpc with infinite
  timeout could fail when the tcp buffer becomes full.

  Fault has existed since OTP-21.

  Own Id: OTP-17358 Aux Id: ERLERL-610

### Known Bugs and Problems

- The `ei` API for decoding/encoding terms is not fully 64-bit compatible since
  terms that have a representation on the external term format larger than 2 GB
  cannot be handled.

  Own Id: OTP-16607 Aux Id: OTP-16608

## Erl_Interface 3.13.2.0.1

### Fixed Bugs and Malfunctions

- Fix bug where sending of large data with ei*send*\*/ei_rpc with infinite
  timeout could fail when the tcp buffer becomes full.

  Fault has existed since OTP-21.

  Own Id: OTP-17358 Aux Id: ERIERL-610

## Erl_Interface 3.13.2

### Fixed Bugs and Malfunctions

- Fix link error "multiple definition of \`ei_default_socket_callbacks'" for gcc
  version 10 or when built with gcc option -fno-common. Error exists since
  OTP-21.3.

  Own Id: OTP-16412 Aux Id: PR-2503

## Erl_Interface 3.13.1

### Fixed Bugs and Malfunctions

- Fix user supplied socket implementation for Windows and other platforms
  without gcc atomics.

  Own Id: OTP-16308

## Erl_Interface 3.13

### Fixed Bugs and Malfunctions

- Fix bugs in `ei_print_term` for binaries and bit strings causing incorrect
  output.

  Own Id: OTP-15917

- Fixed bug in `ei_decode_fun` for very old fun encoding format. Bug exist since
  OTP 22.0.

  Own Id: OTP-15996

### Improvements and New Features

- `ei_print_term()` now supports printing of maps and funs.

  Own Id: OTP-15814

## Erl_Interface 3.12

### Fixed Bugs and Malfunctions

- The vxworks configure has been updated to respect the environment CFLAGS.

  Own Id: OTP-15773

### Improvements and New Features

- Minor adjustments made to build system for parallel configure.

  Own Id: OTP-15340 Aux Id: OTP-14625

- The limited support for VxWorks is deprecated as of OTP 22, and will be
  removed in OTP 23.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15621

- The old legacy `erl_interface` library (functions with prefix `erl_`) is
  deprecated as of OTP 22, and will be removed in OTP 23. This does not apply to
  the `ei` library. Reasonably new `gcc` compilers will issue deprecation
  warnings. In order to disable these warnings, define the macro
  `EI_NO_DEPR_WARN`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15622

- Added support to receive, decode, encode and send both bit strings and export
  funs (`fun M:F/A`).

  New functions `ei_decode_bitstring` and `ei_encode_bitstring` have been added
  in order to decode and encode bit strings where the number of bits is not
  necessary divisible by 8 (a whole number of bytes). The existing functions
  `ei_decode_fun` and `ei_encode_fun` can now also handle export funs.

  Before this change, bit strings and export funs sent to an erl_interface
  c-node were encoded using an undocumented fallback tuple format. For bit
  strings `{Binary,BitsInLastByte}` and for export funs `{M,F}`. Existing c-node
  implementations expecting these tuples must be changed to instead use
  `ei_decode_bitstring` and `ei_decode_fun`. As a temporary solution you can
  also build erl_interface with macro `EI_COMPAT=21` or call
  `ei_set_compat_rel(21)` to receive the old fallback tuples.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15712 Aux Id: OTP-15774

## Erl_Interface 3.11.3.2

### Fixed Bugs and Malfunctions

- Fix bug where sending of large data with ei*send*\*/ei_rpc with infinite
  timeout could fail when the tcp buffer becomes full.

  Fault has existed since OTP-21.

  Own Id: OTP-17358 Aux Id: ERLERL-610

### Known Bugs and Problems

- The `ei` API for decoding/encoding terms is not fully 64-bit compatible since
  terms that have a representation on the external term format larger than 2 GB
  cannot be handled.

  Own Id: OTP-16607 Aux Id: OTP-16608

## Erl_Interface 3.11.3.1

### Fixed Bugs and Malfunctions

- Fix link error "multiple definition of \`ei_default_socket_callbacks'" for gcc
  version 10 or when built with gcc option -fno-common. Error exists since
  OTP-21.3.

  Own Id: OTP-16412 Aux Id: PR-2503

### Known Bugs and Problems

- The `ei` API for decoding/encoding terms is not fully 64-bit compatible since
  terms that have a representation on the external term format larger than 2 GB
  cannot be handled.

  Own Id: OTP-16607 Aux Id: OTP-16608

## Erl_Interface 3.11.3

### Fixed Bugs and Malfunctions

- `erl_interface`/`ei` refused to use node names with an alive name (the part of
  the node name preceding the @ sign) longer than 63 characters and a host name
  longer than 64 characters. The total amount of characters allowed in a node
  name (alivename@hostname) was thus limited to 128 characters. These limits
  applied both to the own node name as well as node names of other nodes.
  Ordinary Erlang nodes limit the node name length to 256 characters, which
  meant that you could not communicate with certain Erlang nodes due to their
  node name used.

  `erl_interface`/`ei` now allow the total amount of characters in a node name
  to be up to 256 characters. These characters may be distributed between alive
  name and host name in whatever way needed. That is, the maximum amount of
  characters in the alive name may be 254 and the maximum amount of characters
  in the host name may be 254, but in total the node name must not exceed 256
  characters.

  Own Id: OTP-15781 Aux Id: ERIERL-356

## Erl_Interface 3.11.2.1

### Fixed Bugs and Malfunctions

- `erl_interface`/`ei` refused to use node names with an alive name (the part of
  the node name preceding the @ sign) longer than 63 characters and a host name
  longer than 64 characters. The total amount of characters allowed in a node
  name (alivename@hostname) was thus limited to 128 characters. These limits
  applied both to the own node name as well as node names of other nodes.
  Ordinary Erlang nodes limit the node name length to 256 characters, which
  meant that you could not communicate with certain Erlang nodes due to their
  node name used.

  `erl_interface`/`ei` now allow the total amount of characters in a node name
  to be up to 256 characters. These characters may be distributed between alive
  name and host name in whatever way needed. That is, the maximum amount of
  characters in the alive name may be 254 and the maximum amount of characters
  in the host name may be 254, but in total the node name must not exceed 256
  characters.

  Own Id: OTP-15781 Aux Id: ERIERL-356

## Erl_Interface 3.11.2

### Fixed Bugs and Malfunctions

- Fix handling of Makefile dependencies so that parallel make works properly.

  Own Id: OTP-15757

## Erl_Interface 3.11.1

### Fixed Bugs and Malfunctions

- Fixed two bugs in the `erl_call` program. A missing initialization (introduced
  in `erl_interface-3.11`) which either caused a crash or failure to connect to
  or start a node, and an incorrectly calculated timeout which could cause
  failure to start an erlang node. These bugs only caused failures on some
  platforms.

  Own Id: OTP-15676 Aux Id: OTP-15442, ERL-881

## Erl_Interface 3.11

### Improvements and New Features

- Support for plugin of a
  [user supplied socket implementation](ei_connect.md#ussi) has been added.

  Own Id: OTP-15442 Aux Id: ERIERL-258

## Erl_Interface 3.10.4

### Fixed Bugs and Malfunctions

- Make `ei_connect` and friends also accept state `ok_simultaneous` during
  handshake, which means the other node has initiated a connection setup that
  will be cancelled in favor of this connection.

  Own Id: OTP-15161 Aux Id: ERIERL-191

- Fixed bug in `ei_receive_msg`, `ei_xreceive_msg`, `ei_receive_msg_tmo` and
  `ei_xreceive_msg_tmo`. The `x->index` was set to entire buffer size instead of
  the number of bytes actually received.

  Own Id: OTP-15171

- Fixed bug in `ei_connect_init` which could be provoked if called by concurrent
  threads. `ei_connect_init` called posix interface `gethostbyname` which is
  documented as not thread safe.

  Own Id: OTP-15191

- Fixed bug in erl_compare_ext() ignoring the tail of lists of otherwise equal
  content. Example: `[a | b]` and `[a | c]` compared equal and `{[a], b}` and
  `{[a], c}` compared equal.

  Own Id: OTP-15277 Aux Id: PR-1929

## Erl_Interface 3.10.3

### Fixed Bugs and Malfunctions

- Fix bug where calling erl_init on certain platforms could result in a buffer
  overflow bug.

  Own Id: OTP-15033

- Fixed `erl_call -m` to not deallocate module source binary before it has been
  read.

  Own Id: OTP-15105 Aux Id: ERL-629

### Improvements and New Features

- The program `erl_call` calls `erl_eval:eval_str/1` when it used to call
  `lib:eval_str/1`. This means that `erl_call` will fail when trying interact
  with an Erlang node running Erlang/OTP 20 or earlier.

  Own Id: OTP-15114 Aux Id: OTP-15072, ERL-634

## Erl_Interface 3.10.2.2

### Fixed Bugs and Malfunctions

- Fix handling of Makefile dependencies so that parallel make works properly.

  Own Id: OTP-15757

## Erl_Interface 3.10.2.1

### Fixed Bugs and Malfunctions

- Make `ei_connect` and friends also accept state `ok_simultaneous` during
  handshake, which means the other node has initiated a connection setup that
  will be cancelled in favor of this connection.

  Own Id: OTP-15161 Aux Id: ERIERL-191

## Erl_Interface 3.10.2

### Fixed Bugs and Malfunctions

- Fix bug in `ei_connect` functions that may cause failure due to insufficient
  buffer space for gethostbyname_r.

  Own Id: OTP-15022 Aux Id: ERIERL-163

- Optimize encoding/decoding for pure 7-bit ascii atoms.

  Own Id: OTP-15023 Aux Id: ERIERL-150

## Erl_Interface 3.10.1

### Fixed Bugs and Malfunctions

- Removed all old unused files in the documentation.

  Own Id: OTP-14475 Aux Id: ERL-409, PR-1493

## Erl_Interface 3.10

### Fixed Bugs and Malfunctions

- Fix bug where gethostname would incorrectly fail with enametoolong on Linux.

  Own Id: OTP-14310

### Improvements and New Features

- Remove generation of atoms in old latin1 external format in the distribution
  between erlang nodes, `erl_interface`, and `jinterface`. The new utf8 format
  for atoms was introduced in OTP R16. An OTP 20 node can therefore not connect
  to nodes older than R16.

  Atoms that can be encoded using latin1 are still encoded by `term_to_binary()`
  using latin1 encoding. Note that all atoms will by default be encoded using
  utf8 in a future Erlang/OTP release. For more information see the
  documentation of `erlang:term_to_binary/2`.

  Own Id: OTP-14337

## Erl_Interface 3.9.3

### Improvements and New Features

- Minor documentation update

  Own Id: OTP-14233 Aux Id: PR-1343

## Erl_Interface 3.9.2

### Fixed Bugs and Malfunctions

- Fix `ei_connect_init` and `ei_connect_xinit` to adjust the `creation` argument
  to be compatible with nodes older than OTP-19.

  Own Id: OTP-13981

### Improvements and New Features

- Editorial documentation changes

  Own Id: OTP-13980

## Erl_Interface 3.9.1

### Fixed Bugs and Malfunctions

- Look for .erlang.cookie in windows system directory if HOMEDRIVE and HOMEPATH
  is not set. The same behaviour as the VM.

  Own Id: OTP-13849

## Erl_Interface 3.9

### Fixed Bugs and Malfunctions

- Fix decoding of LLONG_MIN in erl_decode

  Own Id: OTP-13666 Aux Id: ERL-158

- On windows `ei_decode_ulong` and `ei_decode_long` now correctly returns an
  error when trying to decode a number that does not fit in a long. Fixed a bug
  on windows where enabling ei tracing would cause a segmentation fault.

  Own Id: OTP-13673

### Improvements and New Features

- Handle terms (pids,ports and refs) from nodes with a 'creation' value larger
  than 3. This is a preparation of the distribution protocol to allow OTP 19
  nodes to correctly communicate with future nodes (20 or higher). The
  'creation' value differentiates different incarnations of the same node
  (name).

  Own Id: OTP-13488

## Erl_Interface 3.8.2

### Fixed Bugs and Malfunctions

- Fix Erl_Interface build error on Debian/Hurd and Debian/kFreeBSD. (Thanks to
  Sergei Golovan)

  Own Id: OTP-13328

### Improvements and New Features

- EPMD supports both IPv4 and IPv6

  Also affects oldest supported windows version.

  Own Id: OTP-13364

## Erl_Interface 3.8.1

### Improvements and New Features

- Fix the conditional selection of gethostbyname_r and gethostbyaddr_r.

  Own Id: OTP-13188

## Erl_Interface 3.8

### Improvements and New Features

- Do not accept Nan and Infinity values

  Erlang does not accept these values, so we return an error in the C interface
  rather than letting them through to the Erlang VM, which rejects the message
  with a somewhat cryptic "bad external term".

  Own Id: OTP-12801

## Erl_Interface 3.7.20

### Fixed Bugs and Malfunctions

- Use C99 function isfinite() instead of finite() when available on non GCC
  compilers.

  Own Id: OTP-12268

### Improvements and New Features

- Distribute `autoconf` helpers to applications at build time instead of having
  multiple identical copies committed in the repository.

  Own Id: OTP-12348

- Added an .appup file for the application.

  Own Id: OTP-12358 Aux Id: OTP-12178

## Erl_Interface 3.7.19

### Fixed Bugs and Malfunctions

- Added a `.app` file for the application.

  Own Id: OTP-12178

## Erl_Interface 3.7.18

### Fixed Bugs and Malfunctions

- Implement --enable-sanitizers\[=sanitizers]. Similar to debugging with
  Valgrind, it's very useful to enable -fsanitize= switches to catch bugs at
  runtime.

  Own Id: OTP-12153

## Erl_Interface 3.7.17

### Fixed Bugs and Malfunctions

- Now works with Visual Studio.

  Own Id: OTP-11984

## Erl_Interface 3.7.16

### Fixed Bugs and Malfunctions

- Fix memcheck warning in gen_challange (Thanks to Olivier Girondel)

  Own Id: OTP-11608

## Erl_Interface 3.7.15

### Fixed Bugs and Malfunctions

- Silence warnings (Thanks to Anthony Ramine)

  Own Id: OTP-11517

## Erl_Interface 3.7.14

### Improvements and New Features

- Introduced functionality for inspection of system and build configuration.

  Own Id: OTP-11196

- Header and library files from ic and erl_interface are now installed into
  usr/\{include,lib\}. Note that these directories are unversioned, so the
  latest installed version will be the one in the directory.

  Own Id: OTP-11284

- Fix location of true binary under Mac OSX. Thanks to Simon Cornish.

  Own Id: OTP-11289

## Erl_Interface 3.7.13

### Improvements and New Features

- A guard was added to check if file descriptor is valid before closing it.

  Own Id: OTP-11167

## Erl_Interface 3.7.12

### Fixed Bugs and Malfunctions

- Superfluous trailing comma in enum erlang_char_encoding causing compile error
  for g++ with --pedantic option.

  Own Id: OTP-10913 Aux Id: seq12264

## Erl_Interface 3.7.11

### Fixed Bugs and Malfunctions

- Revert the structs `erlang_pid`, `erlang_port` and `erlang_ref` as they were
  before R16A (without `node_org_enc`) in order to be backward compatible with
  user code that accesses the fields of these structs.

  Own Id: OTP-10885 Aux Id: seq12256

## Erl_Interface 3.7.10

### Improvements and New Features

- Where necessary a comment stating encoding has been added to Erlang files. The
  comment is meant to be removed in Erlang/OTP R17B when UTF-8 becomes the
  default encoding.

  Own Id: OTP-10630

- Limited support for unicode atoms in the external format and in the internal
  representation of the vm. This is a preparative feature in order to support
  communication with future releases of Erlang/OTP that may create unicode
  atoms.

  Own Id: OTP-10753

## Erl_Interface 3.7.9

### Improvements and New Features

- Teach lib/erl_interface/configure.in to look for pthreads support in libc
  (where it can be found on QNX)

  A minor tweak such that this configure _fails_ if you pass --enable-threads
  and no pthreads support can be found.

  (Thanks to Per Hedeland)

  Own Id: OTP-10581

## Erl_Interface 3.7.8

### Improvements and New Features

- Detect when middle endian doubles are used by a platform and account for it
  when decoding floats. (Thanks to Mike Sperber)

  Own Id: OTP-10209

## Erl_Interface 3.7.7

### Fixed Bugs and Malfunctions

- Minor suppressions and fixes of compilation warnings

  Own Id: OTP-10016

## Erl_Interface 3.7.6

### Fixed Bugs and Malfunctions

- An error when getting global names on OS X Lion has been fixed. The error
  caused truncated strings to be returned from the function.

  Own Id: OTP-9799

### Improvements and New Features

- Erlang/OTP can now be built using parallel make if you limit the number of
  jobs, for instance using '`make -j6`' or '`make -j10`'. '`make -j`' does not
  work at the moment because of some missing dependencies.

  Own Id: OTP-9451

- Eliminate use of deprecated regexp module

  Own Id: OTP-9810

## Erl_Interface 3.7.5

### Fixed Bugs and Malfunctions

- Align ei buffer according to size of pointers

  Own Id: OTP-9390

- XML files have been corrected.

  Own Id: OTP-9550 Aux Id: OTP-9541

- Make comment and documentation reflect code in
  erl_interface/src/misc/ei_decode_term.c (Thanks to Anneli Cuss)

  Own Id: OTP-9559

### Improvements and New Features

- ei: integer overflow in string/atom encoding

  ei_encode_atom() and ei_encode_string() use strlen() to get the length of the
  buffer. As strlen() returns an unsigned long long and both ei functions take a
  signed integer, the length fields may overflow.

  Check so that the results of strlen can be held in a signed integer. (Thanks
  to Michael Santos)

  Own Id: OTP-9530

## Erl_Interface 3.7.4

### Fixed Bugs and Malfunctions

- Fix using sizeof() for array given as function argument

  When using the sizeof() operator for an array given as function argument it
  returns the size of the pointer. In this case, the affected function hex(char
  digest\[16], char buff\[33]) will just print 4 or 8 byte instead of the full
  length of 16 bytes, on 32bit and 64bit systems respectively. (Thanks to
  Cristian greco)

  Own Id: OTP-9151

- Initialize `to` and `to_name` in `erl_receive_msg`. (Thanks to Göran Larsson)

  Own Id: OTP-9241

- erl_interface: fix compile error(Thanks to Michael Santos)

  Own Id: OTP-9252

## Erl_Interface 3.7.3

### Fixed Bugs and Malfunctions

- Some malformed distribution messages could cause VM to crash, this is now
  corrected.

  Own Id: OTP-8993

- Strengthen string copy check (Thanks to Michael Santos).

  Own Id: OTP-9071

- Strengthen atom length check when decoding atoms (Thanks to Michael Santos).

  Own Id: OTP-9072

### Improvements and New Features

- Fix global registration. C node needed DFLAG_DIST_MONITOR_FLAT set when
  connecting. Fix list compare in erl_compare_ext to return correct result.
  (Thanks to Vitaliy Batichko and Evgeny Khirin)

  Own Id: OTP-9015

## Erl_Interface 3.7.2

### Fixed Bugs and Malfunctions

- erl_call: remove get_hostent

  get_hostent does not properly handle IPv4 addresses on little endian platforms
  and fails with hostnames beginning with a number. Remove get_hostent and use
  ei_gethostbyname directly since gethostbyname supports IPv4 addresses.

  (Thanks to Michael Santos)

  Own Id: OTP-8890

- teach ei_x_format to handle unary - and + (Thanks to Steve Vinoski)

  Own Id: OTP-8891

- Fix zero byte allocation in registry. (Thanks to Michael Santos)

  Own Id: OTP-8893

- Check the length of the node name to prevent an overflow. Memory error control
  of ei_alloc_big. (Thanks to Michael Santos)

  Own Id: OTP-8943

- erl_term_len() in erl_interface could returned too large values for integers
  (since R14B) and too small values for refs (since R9B).

  Own Id: OTP-8945

## Erl_Interface 3.7.1.1

### Fixed Bugs and Malfunctions

- The `erl_interface` tracelevel for erlang messages was incorrect. This has now
  been fixed.

  Own Id: OTP-8874

## Erl_Interface 3.7.1

### Fixed Bugs and Malfunctions

- Removed unused variable in `ei_decode_term.c`.

  Fixed faulty deallocation in `erl_call`.

  Own Id: OTP-8748

- ei_connect: correct man page examples (Thanks to Michael Santos)

  Own Id: OTP-8813

- ei: prevent overflow in `ei_connect_init` and `ei_xconnect`

  Add length check of the buffer before copying. (Thanks to Michael Santos)

  Own Id: OTP-8814

- Remove DECLSPEC feature which fails on Windows Vista and use the fallback
  implementation instead.

  Own Id: OTP-8826

- erl_call: fix multiple buffer overflows (Thanks to Michael Santos)

  Own Id: OTP-8827

### Improvements and New Features

- Fix incorrect writev iovec buffer handling in `erl_interface` (Thanks to Steve
  Vinoski)

  Own Id: OTP-8837

## Erl_Interface 3.7

### Improvements and New Features

- compact IEEE 754 double encoding in external binary format for ei

  - Implement the compact IEEE 754 double encoding in external binary format for
    ei. Encoding for ei now always produces the NEW_FLOAT_EXT format. Decoding
    and term printing handle both the old ERL_FLOAT_EXT encoding and the new
    NEW_FLOAT_EXT encoding.
  - Legacy erl_interface code also handles the new encoding, but still produces
    the ERL_FLOAT_EXT encoding by default.
  - Also enable the DFLAG_NEW_FLOATS distribution flag.
  - ei_get_type() will return ERL_FLOAT_EXT regardless if the external format is
    encoded with ERL_FLOAT_EXT or NEW_FLOAT_EXT for doubles.
  - Reduce the number of copies of the code for encoding and decoding doubles
    throughout ei and erl_interface by instead calling the ei encoding and
    decoding functions wherever possible.
  - Restore commented-out float tests in ei_decode_SUITE and ei_encode_SUITE in
    lib/erl_interface/test. Modify them to make them match the style of other
    tests in the same suites.

  These changes are based on an ei float patch from Serge Aleynikov originally
  submitted against R12B-2 in July 2008 and reworked by Steve Vinoski May 2010.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8684

## Erl_Interface 3.6.5

### Improvements and New Features

- Document debug support.

  Debug trace output for connection activity could be enabled setting the trace
  level as an integer to the EI_TRACELEVEL environment variable. This option
  could also be read and set from a running program using
  ei_get_tracelevel(void) and ei_set_tracelevel(int).

  Own Id: OTP-5037

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
    libraries and thread library quirks the same way as `erts` do.
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

- Change erroneous "\\\\0" in documentation files `erl_notes.xml` and
  `erl_eterm.xml`.

  Own Id: OTP-8326

- Allow `erl_match()` to match `ERL_LONGLONG` and `ERL_U_LONGLONG` terms (Thanks
  to Scott Lystig Fritchie).

  Own Id: OTP-8400

## Erl_Interface 3.6.4

### Improvements and New Features

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8201

## Erl_Interface 3.6.3

### Fixed Bugs and Malfunctions

- The manual states that erl_receive() return the reason in the `ErlMessage`
  struct. This was not the case and the function is now corrected.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-4969

- In `send_exit.c` an erroneous size of memory allocation could occur when
  reallocating a buffer.

  In `ei_decode_trace.c` the index could be updated when the decoding failed.

  In `ei_printterm.c` the index could be updated when the decoding failed in
  lists and tuples.

  In `ei_decode_term.c` when decoding a double (ERL_FLOAT_EXT) no check was done
  to ensure that the last of the 31 bytes was null terminated.

  In `ei_decode_term.c` when decoding references, only the first 3 bytes are
  read, but the index did not increment by the total size.

  In `ei_decode_fun.c` no check of correct buffer allocation or data length was
  done.

  In `ei_decode_string.c` the integer list string case did not decode the NIL
  tail correctly.

  These errors has now been fixed. (Thanks to Romain Lenglet, Paul Mineiro and
  Paul Guyot).

  Own Id: OTP-6117

- `ei_decode_big` could be decoded with a garbage byte.

  `ei_encode_big` and `ei_x_encode_big` is now available.

  Own Id: OTP-7554

- The function `erl_init_resolve()` did not conform to C99 standard which caused
  a build error on some platforms. This has now been fixed.

  Own Id: OTP-8093

- `Makefile.in` has been updated to use the LDFLAGS environment variable (if
  set). (Thanks to Davide Pesavento.)

  Own Id: OTP-8157

### Improvements and New Features

- Added support for 64-bit integers in encoding/decoding.

  Added support for better printouts of binaries.

  Own Id: OTP-6091

## Erl_Interface 3.6.2

### Fixed Bugs and Malfunctions

- A problem with `gethostbyname` in `erl_start.c` could cause a buffer overflow.
  This has now been fixed.

  Clean up of code and removed compiler warnings.

  Own Id: OTP-7978

## Erl_Interface 3.6.1

### Fixed Bugs and Malfunctions

- A faulty validation in `ei_reg_getpval` caused it to never return the
  key-value. This has now been fixed. (Thanks to Matt Stancliff)

  Own Id: OTP-7960

### Improvements and New Features

- Minor update to the `configure` script.

  Own Id: OTP-7959

## Erl_Interface 3.6.1

### Improvements and New Features

- Minor update to the `configure` script.

  Own Id: OTP-7959

## Erl_Interface 3.6

### Improvements and New Features

- Nodes belonging to different independent clusters can now co-exist on the same
  host with the help of a new environment variable setting ERL_EPMD_PORT.

  Own Id: OTP-7826

## Erl_Interface 3.5.9

### Fixed Bugs and Malfunctions

- A type-casting bug in ei_skip_term and ei_printterm on 64bit platforms
  rendering undefined results is now corrected.

  Own Id: OTP-7577

- A bug in the hostent copying code of erl_interface on MacOS X/Darwin is now
  corrected.

  Own Id: OTP-7593

- A problem with building `erl_interface` on FreeBSD has been fixed (Thanks to
  Akira Kitada).

  Own Id: OTP-7611

## Erl_Interface 3.5.8

### Fixed Bugs and Malfunctions

- Fixed bug in erl_interface when decoding broken data

  Own Id: OTP-7448

## Erl_Interface 3.5.7

### Fixed Bugs and Malfunctions

- An erroneous freeing of memory could occur when using `ei_x_format_wo_ver` in
  erl_interface, resulting in a segmentation fault.

  Own Id: OTP-6795

- A faulty compare in `erl_marshal` has now been fixed. (Thanks to Simon Cornish
  and Paul Mineiro)

  Own Id: OTP-7368

## Erl_Interface 3.5.6

### Fixed Bugs and Malfunctions

- Minor documentation fixes.

  Own Id: OTP-7183 Aux Id: OTP-7118

## Erl_Interface 3.5.5.4

### Fixed Bugs and Malfunctions

- The symbol \_\_erl_errno was undefined in the single thread version of the ei
  library, but is now defined.

  Own Id: OTP-6887

- Corrected FreeBSD build error.

  Own Id: OTP-7093

## Erl_Interface 3.5.5.3

### Improvements and New Features

- Calls to alloca in erl_marshal.c have been removed. A static buffer is now
  used instead to store node names temporarily.

  Own Id: OTP-6331 Aux Id: seq10468

- ei_print_term interprets a list of integers with values from 0 to 255 as a
  string. If the original list contains the integer 0, this is considered
  terminator of the string. This is incorrect. The function has now been
  modified to not look for '\\0' in a string, but always print all characters.

  Own Id: OTP-6339 Aux Id: seq10492

## Erl_Interface 3.5.5.2

### Fixed Bugs and Malfunctions

- The combination of xeon processors with 64bit x86 extensions and a 32bit linux
  could cause ei_decode_long and ei_decode_longlong to fail for the value
  LONG_MIN and LONGLONG_MIN. The conversion is now made more portable.

  Own Id: OTP-6216

## Erl_Interface 3.5.5.1

### Improvements and New Features

- Portability enhancements.

  Own Id: OTP-6132

## Erl_Interface 3.5.5

### Fixed Bugs and Malfunctions

- Different (and old) `config.guess` files in the `erts` and `erl_interface`
  applications would cause build problems on the new Intel-based iMacs. (Thanks
  to Sebastion Strollo.)

  Own Id: OTP-5967

- pthread header and library mismatch on linux systems (at least some SuSE and
  Debian) with both NPTL and Linuxthreads libraries installed.

  Own Id: OTP-5981

### Improvements and New Features

- Support for a C node to connect to an Erlang node on a standalone host has
  been added.

  Own Id: OTP-5883 Aux Id: seq10170

## Erl_interface 3.5.2

### Improvements and New Features

- A configuration test error caused erl_interface to be built without support
  for threads. This has been corrected.

  Own Id: OTP-5456

## Erl_interface 3.5.1

### Improvements and New Features

- Changes and improvements have been made to the build and test environment to
  solve problems with failing erl_interface test cases.

  Own Id: OTP-5295 Aux Id: OTP-5387

## Erl_interface 3.5

### Improvements and New Features

- Process identifiers and port identifiers have been made more unique.
  Previously 18 bits were used as id in the internal representation of process
  and port identifiers. Now 28 bits are used.

  The maximum limit on the number of concurrently existing processes due to the
  representation of pids has been increased to 268435456 processes. The same is
  true for ports. This limit will at least on a 32-bit architecture be
  impossible to reach due to memory shortage.

  _NOTE:_ By default, the `ERTS`, and the `erl_interface`, `ei`, and
  `jinterface` libraries are now only guaranteed to be compatible with other
  Erlang/OTP components from the same release. It is possible to set each
  component in compatibility mode of an earlier release, though. See the
  documentation for respective component on how to set it in compatibility mode.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-4968 Aux Id: OTP-4196

## Erl_interface 3.4.5

### Fixed Bugs and Malfunctions

- Corrections for mistakes done for patch erl_605/OTP-4874.

  Own Id: OTP-4995 Aux Id: OTP-4874

## Erl_interface 3.4.4

### Fixed Bugs and Malfunctions

- A small optimization in ei_rpc\*() was added and a bug in ei_decode_longlong()
  was corrected.

  Own Id: OTP-4784

## Erl_interface 3.4.2

### Fixed Bugs and Malfunctions

- Strings longer than 65535 bytes were encoded wrong in ei/erl_interface.

  Own Id: OTP-4865 Aux Id: EABln07451

## Erl_interface 3.4.1

### Fixed Bugs and Malfunctions

- erl_call -a parsed erlang terms incorrectly due to a bug in ei_format, which
  is now corrected.

  Own Id: OTP-4777 Aux Id: seq8099
