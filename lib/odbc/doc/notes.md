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
# ODBC Release Notes

This document describes the changes made to the odbc application.

## ODBC 2.14.1

### Improvements and New Features

- Allow larger column sizes than 8001 in case DB supports it.

  Own Id: OTP-18539

## ODBC 2.14

### Improvements and New Features

- Input for `configure` scripts adapted to `autoconf` 2\.71.

  Own Id: OTP-17414 Aux Id: PR-4967

## ODBC 2.13.5

### Fixed Bugs and Malfunctions

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## ODBC 2.13.4

### Fixed Bugs and Malfunctions

- Fix compiler warnings produced by the clang compiler.

  Own Id: OTP-17105 Aux Id: PR-2872

## ODBC 2.13.3.1

### Fixed Bugs and Malfunctions

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## ODBC 2.13.3

### Fixed Bugs and Malfunctions

- Make sure odbc c-process exits when erlang process orders it to shutdown.

  Own Id: OTP-17188 Aux Id: ERL-1448

## ODBC 2.13.2

### Fixed Bugs and Malfunctions

- Fixed usage of `AC_CONFIG_AUX_DIRS()` macros in configure script sources.

  Own Id: OTP-17093 Aux Id: ERL-1447, PR-2948

## ODBC 2.13.1

### Improvements and New Features

- Changes in order to build on the Haiku operating system.

  Thanks to Calvin Buckley

  Own Id: OTP-16707 Aux Id: PR-2638

## ODBC 2.13

### Fixed Bugs and Malfunctions

- Fix various compiler warnings on 64-bit Windows.

  Own Id: OTP-15800

### Improvements and New Features

- Rewrite due to the removal of `erl_interface` legacy functions.

  Own Id: OTP-16544 Aux Id: OTP-16328

## ODBC 2.12.4.1

### Fixed Bugs and Malfunctions

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## ODBC 2.12.4

### Improvements and New Features

- Minor adjustments made to build system for parallel configure.

  Own Id: OTP-15340 Aux Id: OTP-14625

## ODBC 2.12.3

### Fixed Bugs and Malfunctions

- Enhance error handling to avoid stack corruption

  Own Id: OTP-15667 Aux Id: ERL-808, PR-2065

## ODBC 2.12.2

### Fixed Bugs and Malfunctions

- Improved documentation.

  Own Id: OTP-15190

## ODBC 2.12.1

### Fixed Bugs and Malfunctions

- Removed all old unused files in the documentation.

  Own Id: OTP-14475 Aux Id: ERL-409, PR-1493

## ODBC 2.12

### Improvements and New Features

- Change configure to skip odbc for old MACs, the change in PR-1227 is not
  backwards compatible with old MACs, and we do not see a need to continue
  support for such old versions. However it is still possible to make it work on
  such machines using the --with-odbc configure option.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14083

## ODBC 2.11.3

### Fixed Bugs and Malfunctions

- ODBC build configure has been updated to accept Mac OS X El Capitan. Fixed by
  Lee Bannard.

  Own Id: OTP-13781

## ODBC 2.11.2

### Improvements and New Features

- Configure enhancement for better handling program paths used in the build
  process

  Own Id: OTP-13559

## ODBC 2.11.1

### Improvements and New Features

- New application variable to set timeout of internal communication setup
  between the erlang code and the c-port program that interfaces the odbc
  driver. This can be useful if you have an underlying system that is slow due
  to heavy load at startup.

  With this environment variable you can easily bypass and tailor odbc to the
  needs of the underlying actual system without changing the configuration.
  Which is a good thing because this value is very system specific.

  Own Id: OTP-12935

## ODBC 2.11

### Improvements and New Features

- Change license text from Erlang Public License to Apache Public License v2

  Own Id: OTP-12845

## ODBC 2.10.22

### Fixed Bugs and Malfunctions

- OS X Mavericks is based on Darwin version 13.x, and Yosemite on 14.x. Change
  the ODBC configure.in script to recognize these versions.

  Own Id: OTP-12260

### Improvements and New Features

- The commands longer than 127 chars sent to odbc server crashed it, e.g. a
  connection string with driver path and some additional parameters.

  Own Id: OTP-12346

- Distribute `autoconf` helpers to applications at build time instead of having
  multiple identical copies committed in the repository.

  Own Id: OTP-12348

## ODBC 2.10.21

### Fixed Bugs and Malfunctions

- Fix compiler warnings reported by LLVM

  Own Id: OTP-12138

- Implement --enable-sanitizers\[=sanitizers]. Similar to debugging with
  Valgrind, it's very useful to enable -fsanitize= switches to catch bugs at
  runtime.

  Own Id: OTP-12153

## ODBC 2.10.20

### Fixed Bugs and Malfunctions

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

### Improvements and New Features

- Removed warnings at compile time by adding missing include file (Thanks to
  Anthony Ramine)

  Own Id: OTP-11569

- Apple has removed iODBC in OS X 10.9 Mavericks, but forgot to remove all
  binaries, adopt configure so that will be possible to build odbc with own
  installation.

  Own Id: OTP-11630

## ODBC 2.10.19

### Fixed Bugs and Malfunctions

- Updated configure test for header files sql.h and sqlext.h to function
  correctly on windows.

  Own Id: OTP-11574

## ODBC 2.10.18

### Improvements and New Features

- Configure now also checks for the existence of the sql.h header file

  Own Id: OTP-11483

## ODBC 2.10.17

### Fixed Bugs and Malfunctions

- The format of the xml source for documentation is corrected in order to
  conform to the DTDs and to pass xmllint without errors.

  Own Id: OTP-11193

### Improvements and New Features

- Introduced functionality for inspection of system and build configuration.

  Own Id: OTP-11196

- Prevent odbcserver crash if it's executed and supplied incorrect data to
  stdin. Thanks to Sergei Golovan.

  Own Id: OTP-11233

## ODBC 2.10.16

### Improvements and New Features

- Fix a 64bit related bug in odbcserver. Thanks to Satoshi Kinoshita.

  Own Id: OTP-10993

- Postscript files no longer needed for the generation of PDF files have been
  removed.

  Own Id: OTP-11016

- Fix checking for odbc in standard locations when "with-odbc" flag present.
  Thanks to Alexey Saltanov.

  Own Id: OTP-11126

## ODBC 2.10.15

### Improvements and New Features

- Fixed calling odbc:param_query/3 and odbc:param_query/4 with unparameterized
  query string and empty parameters list. Thanks to Danil Onishchenko.

  Own Id: OTP-10798

## ODBC 2.10.14

### Improvements and New Features

- Under Unix enable TCP_NODELAY to disable Nagel's socket algorithm. Thanks to
  Andy Richards

  Impact: Performance gain on Unix systems

  Own Id: OTP-10506

- Added extended_errors option to ODBC

  When enabled, this option alters the return code of ODBC operations that
  produce errors to include the ODBC error code as well as the native error
  code, in addition to the ODBC reason field which is returned by default.
  Thanks to Bernard Duggan.

  Own Id: OTP-10603

- Where necessary a comment stating encoding has been added to Erlang files. The
  comment is meant to be removed in Erlang/OTP R17B when UTF-8 becomes the
  default encoding.

  Own Id: OTP-10630

- Some examples overflowing the width of PDF pages have been corrected.

  Own Id: OTP-10665

- Fix aotocommit for Oracle ODBC driver in Linux. Thanks to Danil Onishchenko.

  Own Id: OTP-10735

## ODBC 2.10.13

### Fixed Bugs and Malfunctions

- Add support for NULL value in odbc:param_query

  Support atom 'null' in odbc:param_query as database NULL value Fix "ODBC:
  received unexpected info:\{tcp_closed, ...\}" when connection is terminated.
  Fix possible access violation with 64bit ODBC. Thanks to Maxim Zrazhevskiy

  Own Id: OTP-10206

## ODBC 2.10.12

### Fixed Bugs and Malfunctions

- An ODBC process should exit normally if its client exits with 'shutdown'

  There is nothing strange about the client shutting down, so the ODBC process
  should exit normally to avoid generating a crash report for a non-problem.
  (Thanks to Magnus Henoch)

  Own Id: OTP-9716

### Improvements and New Features

- Erlang/OTP can now be built using parallel make if you limit the number of
  jobs, for instance using '`make -j6`' or '`make -j10`'. '`make -j`' does not
  work at the moment because of some missing dependencies.

  Own Id: OTP-9451

## ODBC 2.10.11

### Fixed Bugs and Malfunctions

- When using output parameters the internal odbc state was not correctly cleaned
  causing the next call to param_query to misbehave.

  Own Id: OTP-9444

- XML files have been corrected.

  Own Id: OTP-9550 Aux Id: OTP-9541

### Improvements and New Features

- Add code to handle old ODBC drivers on solaris. Also adds tests with MySQL.

  Own Id: OTP-8407

- Odbc now supports SQL_WLONGVARCHAR, thanks to Hanfei Shen for the patch.

  Own Id: OTP-8493

## ODBC 2.10.10

### Fixed Bugs and Malfunctions

- Better error messages for connection issues.

  Own Id: OTP-9111

## ODBC 2.10.9

### Improvements and New Features

- Ipv6 is now supported on Windows as well as on UNIX for internal socket
  communication. (ODBC uses sockets instead of the "Erlang port pipes" as some
  ODBC-drivers are known to mess with stdin/stdout.)

  Loopback address constants are used when connecting the c-side to the
  erlang-side over local socket API avoiding getaddrinfo problems, and the \{ip,
  loopback\} option is added as a listen option on the erlang-side. Also cleaned
  up the TIME_STAMP contribution.

  Own Id: OTP-8917

## ODBC 2.10.8

### Improvements and New Features

- ODBC now handles the types SQL_WCHAR and SQL_WVARCHAR. Thanks to Juhani
  Ränkimies. ODBC also has a new connection option to return all strings as
  binaries and also expect strings to be binaries in the param_query function.
  These changes provides some unicode support.

  Own Id: OTP-7452

- Now supports SQL_TYPE_TIMESTAMP on the format \{\{YY, MM, DD\}, \{HH, MM,
  SS\}\}. Thanks to Juhani Ränkimies.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8511

## ODBC 2.10.7

### Fixed Bugs and Malfunctions

- The odbc application can now be compiled on FreeBSD. (Thanks to Kenji
  Rikitake.)

  Own Id: OTP-8444

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

- odbc:param_query() now properly indicates if nothing was updated. (Thanks to
  Paul Oliver.)

  Own Id: OTP-8347

### Known Bugs and Problems

- The ODBC test cases are failing for linux and MacOSX There is problems with
  setting of options on odbc-connections, and the odbcserver just exits with an
  exit code.

  Own Id: OTP-8407

## ODBC 2.10.6

### Fixed Bugs and Malfunctions

- Applied a patch from Andrew Thompson, which fixes some error cases.

  Own Id: OTP-8291

### Improvements and New Features

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8250

## ODBC 2.10.5

### Fixed Bugs and Malfunctions

- A missing return statement in a non void function has been fixed in `odbc`.
  (Thanks to Nico Kruber)

  Own Id: OTP-7978

## ODBC 2.10.4

### Improvements and New Features

- param_query now handles the in_or_out parameter correctly.

  Own Id: OTP-7720

- Changed the internal socket use so that it will become more robust to
  non-functional ipv6 and fallback on ipv4.

  Own Id: OTP-7721

## ODBC 2.10.3

### Improvements and New Features

- Configure update for mac.

  Own Id: OTP-7418

### Known Bugs and Problems

- describe_table/\[2,3] on mac gives an empty result

  Own Id: OTP-7478

## ODBC 2.10.2

### Fixed Bugs and Malfunctions

- SQLINTEGERs where not retrieved correctly on 64 bit platforms as an SQLINTEGER
  is defined to be a 32 bit integer and not a true long.

  Own Id: OTP-7297

## ODBC 2.10.1

### Improvements and New Features

- Now supports out and input parameters for stored procedures.

  Own Id: OTP-7019

- ODBC is now prebuilt for SLES10 in the commercial build and parameters to
  error_logger:error_report/1 has been corrected.

  Own Id: OTP-7294

- Parametrized queries will now work correctly when using Erlang R12B-2 on Linux
  (SuSE 10.3), MySQL 5.0.45, myodbc 3.51 and unixODBC 2.2.12. Earlier it could
  happen that an error was returned even though data was correctly inserted into
  the database.

  Own Id: OTP-7307

### Known Bugs and Problems

- SQLINTEGERs are not retrieved correctly on 64 bit platforms as an SQLINTEGER
  seems to be defined to be a 32 bit integer and not a true long.

  Own Id: OTP-7297

## ODBC 2.10

### Improvements and New Features

- Enhanced configure to among other things work better when there is a library
  found but it is not usable e.i. 32 bit library in 64 bit build.

  Own Id: OTP-7062

## ODBC 2.0.9

### Improvements and New Features

- The odbc application now has to be explicitly started and stopped e.i. it will
  not automatically be started as a temporary application as it did before.
  Although a practical feature when testing things in the shell, it is not
  desirable that people take advantage of this and not start the odbc
  application in a correct way in their products. Added functions to the odbc
  API that calls application:start/stop.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6984

- Changed Makefile.in so that odbc is not disabled on 64-bits architectures. It
  was earlier disabled due to that it had never been tested in that environment.

  Own Id: OTP-6987

## ODBC 2.0.8

### Improvements and New Features

- Minor Makefile changes.

  Own Id: OTP-6689

## ODBC 2.0.7

### Fixed Bugs and Malfunctions

- When using a parameterized query on a windows platform the data was inserted
  in the table on the sql-server but the connection was lost, this seems to be
  due to a compiler error that has now been worked around, but further
  investigation is ongoing to verify that that really was the problem.

  Own Id: OTP-5504

- param_query/\[3,4] could return an unexpected row count for some drivers, in
  this case a postgresdriver.

  Own Id: OTP-6363

## ODBC 2.0.6

### Fixed Bugs and Malfunctions

- pthread header and library mismatch on linux systems (at least some SuSE and
  Debian) with both NPTL and Linuxthreads libraries installed.

  Own Id: OTP-5981

### Improvements and New Features

- Changed configure to find odbc in /usr/local too

  Own Id: OTP-5966

### Known Bugs and Problems

- When using a parameterized query on a windows platform the data is inserted in
  the table on the sql-server but for some reason the connection is lost.

  Own Id: OTP-5504

## ODBC 2.0.5

### Fixed Bugs and Malfunctions

- Fixed bug, reported error when deleting nonexisting rows, thanks to Laura M.
  Castro for reporting this.

  Own Id: OTP-5759

### Known Bugs and Problems

- When using a parameterized query on a windows platform the data is inserted in
  the table on the sql-server but for some reason the connection is lost.

  Own Id: OTP-5504

## Odbc 2.0.4

### Improvements and New Features

- /usr was added as a default place for configure to look for the odbc library
  on unix/linux platforms.

  Own Id: OTP-5501

- A legacy timer in the c port program was set to infinity. All timeout handling
  is handled by the erlang code and a extra timeout in the c code will just lead
  to confusion if it is released.

  Own Id: OTP-5502

### Known Bugs and Problems

- When using a parameterized query on a windows platform the data is inserted in
  the table on the sql-server but for some reason the connection is lost.

  Own Id: OTP-5504

## Odbc 2.0.3

### Improvements and New Features

- odbc now uses configure as all "normal" applications instead of providing
  special Makefiles for each commercial supported platform. This also makes it
  easier to build odbc on non supported platforms.

  Own Id: OTP-5437

## odbc 2.0.2

### Fixed Bugs and Malfunctions

- When issuing a batch of queries and one of the queries fail the odbc port
  program crashed. This is no longer the case.

  Own Id: OTP-5176

## odbc 2.0.1

### Improvements and New Features

- Added use of the socket option TCP_NODELAY, as in the case of Erlang odbc the
  Nagel algorithm will never help, but always cause an unnecessary delay.

  Own Id: OTP-5100

## odbc 2.0

### Improvements and New Features

- Erlang ODBC now handles batches of queries and can return multiple result
  sets.

  Own Id: OTP-4642 Aux Id: seq7766

- The old interface that became deprecated in odbc 1.0 has now been removed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-4794

- The port program now sends different exit codes to Erlang when it exits due to
  failure. This instead of sending the same exit code and then trying to write
  to stderr. Erlang encodes the exit code to a descriptive atom.

  Own Id: OTP-4813

- Erlang ODBC now supports parameterized queries for the most common ODBC data
  types.

  Own Id: OTP-4821

- SQL_NUMERIC and SQL_DECIMAL columns are converted to integer and float values
  if possible.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-4826

- Result sets are now by default returned as a list of tuples which is the most
  intuitive and useful mapping. To keep some degree of backwards compatibility
  you may turn this off to get the old behavior that result sets are returned as
  lists of lists. However do not use this in new code as it is considered a
  deprecated feature that eventually will disappear.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-4850

- The odbc implementation now mostly uses sockets to communicate between the c
  and the erlang process, this is to avoid a lot of problems arising from
  different odbc-drivers doing strange things that disturbed the port-program
  communication mechanism.

  Own Id: OTP-4875
