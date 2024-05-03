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
# Jinterface Release Notes

This document describes the changes made to the Jinterface application.

## Jinterface 1.14

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

## Jinterface 1.13.2

### Fixed Bugs and Malfunctions

- Accept connection setup from OTP 23 and 24 nodes that are not using epmd.

  Own Id: OTP-18404 Aux Id: GH-6595, PR-6625

## Jinterface 1.13.1

### Fixed Bugs and Malfunctions

- Fix javadoc build error by adding option `-encoding UTF-8`.

  Own Id: OTP-18215 Aux Id: PR-6154

## Jinterface 1.13

### Improvements and New Features

- The following distribution flags are now mandatory: `DFLAG_BIT_BINARIES`,
  `DFLAG_EXPORT_PTR_TAG`, `DFLAG_MAP_TAGS`, `DFLAG_NEW_FLOATS`, and
  `DFLAG_FUN_TAGS`. This mainly concerns libraries or application that implement
  the distribution protocol themselves.

  Own Id: OTP-17318 Aux Id: PR-4972

- Removed use of node creation value zero as a wildcard. Also prevent zero from
  being used as creation by `erl_interface` and `jinterface` nodes.

  Own Id: OTP-17682 Aux Id: PR-5347

- Add new abstract class `OtpGenericTransportFactory` to allow implementation of
  any transport protocol without dependency on epmd.

  Own Id: OTP-17961 Aux Id: PR-4839

## Jinterface 1.12.2

### Fixed Bugs and Malfunctions

- Fix bug in `OtpOutputStream.write_pid/4` and `write_ref/3` causing faulty
  encodig. Bug exists since OTP 23.0.

  Own Id: OTP-17887 Aux Id: ERIERL-750, PR-5640

## Jinterface 1.12.1

### Fixed Bugs and Malfunctions

- Fixed rare race bug that could cause NullPointerException on local close of
  connection.

  Own Id: OTP-17478 Aux Id: PR-4837

## Jinterface 1.12

### Improvements and New Features

- Accept 64-bit process identifiers from external nodes. This is the first step
  in an upgrade path toward using 64-bit pids in a future OTP release.

  Own Id: OTP-16720 Aux Id: PR-2680

- Accept references up to a size of 160-bits from remote nodes. This is the
  first step in an upgrade path toward using references up to 160-bits in a
  future OTP release.

  Own Id: OTP-17005 Aux Id: OTP-16718

- Accept 64-bit port identifiers from external nodes. This is the first step in
  an upgrade path toward using 64-bit port identifiers in a future OTP release.

  Own Id: OTP-17007

- Make `OtpErlangExternalFun`'s fields `module`, `function` and `arity` public.

  Own Id: OTP-17170 Aux Id: PR-3005

## Jinterface 1.11.1.1

### Fixed Bugs and Malfunctions

- Fix bug in `OtpOutputStream.write_pid/4` and `write_ref/3` causing faulty
  encodig. Bug exists since OTP 23.0.

  Own Id: OTP-17887 Aux Id: ERIERL-750, PR-5640

## Jinterface 1.11.1

### Fixed Bugs and Malfunctions

- A [new link protocol](`e:erts:erl_dist_protocol.md#new_link_protocol`) has
  been introduced which prevents links from ending up in an inconsistent state
  where one participant considers itself linked while the other doesn't. This
  bug has always existed in the distributed case, but has since OTP 21 also
  existed in the node local case since the distributed link protocol then was
  adopted also for node local links. The bug could, however, only trigger if
  both participants operated on the link simultaneously.

  Own Id: OTP-17127

## Jinterface 1.11

### Improvements and New Features

- Increased size of node incarnation numbers (aka "creation"), from 2 bits to 32
  bits. This will reduce the risk of pids/ports/refs, from different node
  incarnation with the same name, being mixed up.

  Own Id: OTP-15603

- Improved node connection setup handshake protocol. Made possible to agree on
  protocol version without dependence on `epmd` or other prior knowledge of peer
  node version. Also added exchange of node incarnation ("creation") values and
  expanded the distribution capability flag field from 32 to 64 bits.

  Own Id: OTP-16229

## Jinterface 1.10.1

### Fixed Bugs and Malfunctions

- Replaced deprecated <tt> with <code> in documentation.

  Own Id: OTP-16050

## Jinterface 1.10

### Fixed Bugs and Malfunctions

- All incorrect (that is, all) uses of "can not" has been corrected to "cannot"
  in source code comments, documentation, examples, and so on.

  Own Id: OTP-14282 Aux Id: PR-1891

### Improvements and New Features

- Added support to receive export funs (`fun M:F/A`).

  Before this change, export funs sent to a jinterface node were encoded using
  an undocumented fallback tuple format `{M,F}`. Existing jinterface
  implementations expecting these tuples must be changed to instead use the
  existing `OtpErlangExternalFun` class.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15774 Aux Id: OTP-15712

## Jinterface 1.9.1

### Fixed Bugs and Malfunctions

- Improved documentation.

  Own Id: OTP-15190

## Jinterface 1.9

### Improvements and New Features

- Add module package name for Java 9

  Own Id: OTP-14844

## Jinterface 1.8.1

### Fixed Bugs and Malfunctions

- Removed all old unused files in the documentation.

  Own Id: OTP-14475 Aux Id: ERL-409, PR-1493

## Jinterface 1.8

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

## Jinterface 1.7.1

### Fixed Bugs and Malfunctions

- Update build scripts to not make assumptions about where env, cp and perl are
  located.

  Own Id: OTP-13800

## Jinterface 1.7

### Fixed Bugs and Malfunctions

- Fix Jinterface build on Maven

  Own Id: OTP-13482

### Improvements and New Features

- Handle terms (pids,ports and refs) from nodes with a 'creation' value larger
  than 3. This is a preparation of the distribution protocol to allow OTP 19
  nodes to correctly communicate with future nodes (20 or higher). The
  'creation' value differentiates different incarnations of the same node
  (name).

  Own Id: OTP-13488

## Jinterface 1.6.1

### Fixed Bugs and Malfunctions

- Add missing Term tag matching switch statement that was missing an external
  fun tag.

  Own Id: OTP-13106

- fixed writing small compressed values.

  Own Id: OTP-13165

## Jinterface 1.6

### Fixed Bugs and Malfunctions

- Reformat the sources for JInterface uniformly and according to the standard
  Java style guidelines. Provide description of the rules applied in Eclipse
  format (for other editors one can check the settings against these).

  In short, the formatting style is: _ indentation uses only spaces; each level
  is 4 positions _ no trailing whitespace _ mostly default Java style formatting
  (any difference is minor) _ always use \{\} blocks \* use 'final' as much as
  possible

  Own Id: OTP-12333

- Remove extra @param in javadoc as this gives an error when building the docs
  in java 1.8

  Own Id: OTP-12746

### Improvements and New Features

- Add basic transport factory implementation. This makes possible creating
  connections between nodes using ssh channels for example.

  Own Id: OTP-12686

- Add Jinterface generic match and bind methods to provide low level interface
  base methods sufficient for variety of higher level pattern matching/variable
  binding implementations.

  Own Id: OTP-12691

- Minimal Java version is now 1.6

  Own Id: OTP-12715

## Jinterface 1.5.12

### Fixed Bugs and Malfunctions

- handle empty .erlang.cookie without crashing and OtpErlangList.clone must not
  return null

  Own Id: OTP-12210

- This fixes all the compilation warnings in the Java code

  Own Id: OTP-12211

### Improvements and New Features

- Added an .appup file for the application.

  Own Id: OTP-12358 Aux Id: OTP-12178

## Jinterface 1.5.11

### Fixed Bugs and Malfunctions

- Added a `.app` file for the application.

  Own Id: OTP-12178

## Jinterface 1.5.10

### Fixed Bugs and Malfunctions

- Array now show meaningful values in exceptions.

  Own Id: OTP-12049

- Documentation improvements.

  Own Id: OTP-12050

- Include the cause when raising a new IOException, which should make the reason
  for the exception clearer.

  Own Id: OTP-12075

- Arrays (here: md5 and freeVars) must not be compared with equals, which is
  broken (compares identity).

  Own Id: OTP-12121

## Jinterface 1.5.9

### Improvements and New Features

- Implement support for Maps

  The API and implementation are simplistic, like for lists and tuples, using
  arrays and without any connection to java.util.Map. (Thanks to Vlad
  Dumitrescu)

  Own Id: OTP-11703

## Jinterface 1.5.8

### Fixed Bugs and Malfunctions

- Fixed a bug in OtpErlangTuple constructor. Thanks to Vlad Dumitrescu.

  Own Id: OTP-10819

- Fixed finding cookie file on windows. Thanks to Vlad Dumitrescu

  Own Id: OTP-10821

### Improvements and New Features

- Don't compress external binary format if this increases the size. Thanks to
  Nico Kruber.

  Own Id: OTP-10822

## Jinterface 1.5.7

### Fixed Bugs and Malfunctions

- fix reading compressed binary terms from Java (Thanks to Nico Kruber)

  Own Id: OTP-10505

- OtpEpmd.lokupNames() no longer hangs when badly configured (Thanks to Vlad
  Dumitrescu)

  Own Id: OTP-10579

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

## Jinterface 1.5.6

### Fixed Bugs and Malfunctions

- Correct spelling of registered (Thanks to Richard Carlsson)

  Own Id: OTP-9925

- Java 1.5 has a bug where detecting codepoint offsets in strings that are
  created by String.substring() gives wrong results. The new implementation uses
  a different method, avoiding the issue. (Thanks to Vlad Dumitrescu)

  Own Id: OTP-9927

- Improve error message when creating a too long OtpErlangAtom. Also print the
  value that we tried to use for the atom. (Thanks to Vlad Dumitrescu)

  Own Id: OTP-9928

## Jinterface 1.5.5

### Fixed Bugs and Malfunctions

- JInterface: improve OtpOutputStream buffer allocation

  Previously, the buffer was increased linearly by 2048 bytes. I now propose to
  use an exponential increase function (similar to Javas ArrayList, e.g. always
  at least +50%). This significantly increases performance of e.g. doRPC for
  large parameters. (Thanks to Nico Kruber)

  Own Id: OTP-9806

## Jinterface 1.5.4

### Fixed Bugs and Malfunctions

- Some malformed distribution messages could cause VM to crash, this is now
  corrected.

  Own Id: OTP-8993

## Jinterface 1.5.3.2

### Improvements and New Features

- The OtpMbox class did not have a hash() method, which it should have because
  it overrides equals().

  Own Id: OTP-8854

## Jinterface 1.5.3.1

### Improvements and New Features

- An pom.xml file is now generated. (Thanks to Gabor Liptak.)

  Own Id: OTP-8841

## Jinterface 1.5.3

### Improvements and New Features

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

## Jinterface 1.5.2

### Improvements and New Features

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8201

## Jinterface 1.5.1

### Fixed Bugs and Malfunctions

- Many Erlang classes, e.g. OtpErlangRef, was missing an implementation of the
  hashCode() method, making it futile to put them in hash structures such as
  HashMap. Bug and patch provided by Paul Guyot. We extended the patch to all
  classes and improved (?) on the hash algorithm.

  Own Id: OTP-7899

### Improvements and New Features

- jinterface uses the new environment variable ERL_EPMD_PORT the same way that
  erl, epmd and erl_interface do since R13A.

  Own Id: OTP-7885

## Jinterface 1.5

### Improvements and New Features

- A number of fixes and improvements from the ErlIDE group; Vlad Dumitrescu and
  Jakob Cederlund: JDK 1.5 is now a minimal requirement for building Jinterface.
  New method: OtpEpmd.lookupNames. OtpErlangList is now iterable. Non-proper
  lists are now allowed - you have to test if a list is proper or not.
  Non-proper lists can also be created. New methods: isProper, getHead, getTail
  and getNthTail. The get tail methods creates a sublist object that re-uses the
  original list. OtpErlangPid is now Comparable. Empty atoms can now be
  constructed, a missing feature pointed out by Sebastien Boisgerault on
  erlang-questions.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7832

## Jinterface 1.4.2

### Fixed Bugs and Malfunctions

- A bug when Jinterface did not detect remote node disconnects has been
  corrected.

  Own Id: OTP-7624

## Jinterface 1.4.1

### Fixed Bugs and Malfunctions

- Jinterface has been fixed to use the loopback interface when connecting to the
  Erlang Port Mapper Daemon - epmd. This facilitates firewall configuration and
  saves resources.

  Own Id: OTP-7103

- Jinterface now refuses to connect to another node that does not agree on the
  other node's name. This has been a problem in that a connection was set up,
  but not used on a following send, which caused a second erroneous connection
  attempt to fail since the other (Erlang) node rejects a connection due to name
  disagreement.

  Problem reported and dissected by Alexander Lamb

  Own Id: OTP-7330

## Jinterface 1.4

### Improvements and New Features

- Jinterface has been updated to handle compressed terms, bitstring, new (IEEE)
  floats and bignums. This has caused a few incompatibilities.

  com.ericsson.otp.erlang.OtpOutputStream now extends
  java.io.ByteArrayOutputStream, previously java.lang.Object, and hence the
  method size() now return the number of bytes in the stream as dictated by the
  new parent class and not the buffer length as before. The new method length()
  returns what the old size() did return. The method count() is deprecated as it
  returns the same as the new size().

  com.ericsson.otp.erlang.OtpErlangLong now can handle larger values than
  64-bit. Previously when larger than 64-bit integers were sent to a Jinterface
  node, it caused an com.ericsson.otp.erlang.OtpDecodeException. Now the integer
  is accepted, but the return value of longValue() is silently truncated, as
  opposed to the return value of intValue() that now and previously raises an
  com.ericsson.otp.erlang.OtpRangeException for unrepresentable values. The new
  methods isLong() and isULong() can be used to determine if the value fits in a
  long.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6809

## Jinterface 1.3

### Fixed Bugs and Malfunctions

- `OtpMbox.receive()` and `OtpMbox.receive(long timeout)` can now throw
  `OtpErlangDecodeException` if the received message cannot be decoded. `null`
  is now only returned from `OtpMbox.receive(long timeout)` if a timeout occurs.
  `OtpMbox.receive()` will never return `null`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-3932 Aux Id: seq5196

- Strings longer than 65535 bytes were encoded wrong by jinterface.

  Own Id: OTP-4883 Aux Id: OTP-4865

- Misc bugfixes:

  - A node pinging itself does no longer cause the java program to hang.
  - `OtpMbox.close()` uses exit reason `'normal'` (atom) instead of `"normal"`
    (string).
  - Added `OtpNode.closeMbox(OtpMbox mbox, OtpErlangObject reason)`.
  - Added `OtpMbox.exit(OtpErlangObject reason)` and
    `OtpMbox.exit(String reason)`.
  - Added `OtpMbox.exit(OtpErlangPid to, OtpErlangObject reason)`.
  - `OtpMbox.exit(OtpErlangPid to, String reason)` is now equivalent to
    `OtpMbox.exit(to, new OtpErlangAtom(reason))`.
  - Exit signals from an Erlang node can now contain any erlang term as reason,
    not only atoms.

  Own Id: OTP-5088

- For java versions 1.3.1 or later, `localStatus` was not sent to registered
  status handler when `OtpNode.close()` was called. This is now corrected.

  Own Id: OTP-5169

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

## Jinterface 1.2.1.2

### Fixed Bugs and Malfunctions

- Output of Java long was truncated to 32 bits. IDL long long and unsigned long
  long could not be used in a struct for the Java backend. All unsigned integer
  types for the Java backend had broken marshalling for large values. These bugs
  has now been fixed.

  Own Id: OTP-4763 Aux Id: seq8024
