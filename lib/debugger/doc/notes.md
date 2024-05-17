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
# Debugger Release Notes

This document describes the changes made to the Debugger application.

## Debugger 5.4

### Fixed Bugs and Malfunctions

- The dependencies for this application are now listed in the app file.

  Own Id: OTP-18831 Aux Id: [PR-7441]

[PR-7441]: https://github.com/erlang/otp/pull/7441

### Improvements and New Features

- Type specs have been added to all API functions.

  Own Id: OTP-18819 Aux Id: [PR-7781]

- The documentation has been migrated to use Markdown and ExDoc.

  Own Id: OTP-18955 Aux Id: [PR-8026]

- The Debugger now use a trace session for its internal use of tracing to avoid interfering with the user's use of tracing.

  Own Id: OTP-19074 Aux Id: [PR-8389]

[PR-7781]: https://github.com/erlang/otp/pull/7781
[PR-8026]: https://github.com/erlang/otp/pull/8026
[PR-8389]: https://github.com/erlang/otp/pull/8389

## Debugger 5.3.4

### Fixed Bugs and Malfunctions

* Guards with nested record expression could wrongly evaluate to false.

  Own Id: OTP-18958 Aux Id: GH-8120, PR-8275

## Debugger 5.3.3

### Fixed Bugs and Malfunctions

- Map comprehensions now work in the Debugger.

  Own Id: OTP-18888 Aux Id: GH-7914

## Debugger 5.3.2

### Fixed Bugs and Malfunctions

- The call `int:no_break(Module)` did not remove any breakpoints.

  Own Id: OTP-18644 Aux Id: GH-7336

- The `maybe` expression is now supported in the Debugger.

  Own Id: OTP-18740 Aux Id: GH-7410, PR-7599

## Debugger 5.3.1.3

### Fixed Bugs and Malfunctions

* Guards with nested record expression could wrongly evaluate to false.

  Own Id: OTP-18958 Aux Id: GH-8120, PR-8275

## Debugger 5.3.1.2

### Fixed Bugs and Malfunctions

- The `maybe` expression is now supported in the Debugger.

  Own Id: OTP-18740 Aux Id: GH-7410, PR-7599

## Debugger 5.3.1.1

### Fixed Bugs and Malfunctions

- The call `int:no_break(Module)` did not remove any breakpoints.

  Own Id: OTP-18644 Aux Id: GH-7336

## Debugger 5.3.1

### Fixed Bugs and Malfunctions

- Fixed a bug that would cause analysis to crash.

  Own Id: OTP-18372 Aux Id: GH-6580

## Debugger 5.3

### Improvements and New Features

- The configuration files [`.erlang`](`e:erts:erl_cmd.md`),
  [`.erlang.cookie`](`e:system:distributed.md`) and
  [`.erlang.crypt`](`m:beam_lib#module-erlang-crypt`) can now be located in the XDG
  Config Home directory.

  See the documentation for each file and `filename:basedir/2` for more details.

  Own Id: OTP-17554 Aux Id: GH-5016 PR-5408 OTP-17821

## Debugger 5.2.1.1

### Fixed Bugs and Malfunctions

- The call `int:no_break(Module)` did not remove any breakpoints.

  Own Id: OTP-18644 Aux Id: GH-7336

## Debugger 5.2.1

### Fixed Bugs and Malfunctions

- Fix record index matching, it was broken and could never match.

  Own Id: OTP-17865 Aux Id: GH-5571

## Debugger 5.2

### Improvements and New Features

- Improve record handling, print known records with record syntax.

  Own Id: OTP-17574

## Debugger 5.1

### Improvements and New Features

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

## Debugger 5.0

### Improvements and New Features

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

- The deprecated `erlang:get_stacktrace/0` BIF now returns an empty list instead
  of a stacktrace. To retrieve the stacktrace, use the extended try/catch syntax
  that was introduced in OTP 21. `erlang:get_stacktrace/0` is scheduled for
  removal in OTP 24.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16484

## Debugger 4.2.8

### Fixed Bugs and Malfunctions

- Fix a bug where Unicode atoms are printed differently depending on if integer
  lists are printed as strings or not.

  Own Id: OTP-16186

## Debugger 4.2.7

### Fixed Bugs and Malfunctions

- All incorrect (that is, all) uses of "can not" has been corrected to "cannot"
  in source code comments, documentation, examples, and so on.

  Own Id: OTP-14282 Aux Id: PR-1891

## Debugger 4.2.6

### Fixed Bugs and Malfunctions

- Improved documentation.

  Own Id: OTP-15190

## Debugger 4.2.5

### Fixed Bugs and Malfunctions

- Fix a bug where calling a fun inside a binary would crash the Debugger.

  Own Id: OTP-14957 Aux Id: PR-1741

## Debugger 4.2.4

### Fixed Bugs and Malfunctions

- Do not quote variables and button names in Debugger windows. The bug was
  introduced in Erlang/OTP 20.1.

  Own Id: OTP-14802

## Debugger 4.2.3

### Improvements and New Features

- Tools are updated to show Unicode atoms correctly.

  Own Id: OTP-14464

## Debugger 4.2.2

### Fixed Bugs and Malfunctions

- The Erlang shell, `qlc:string_to_handle()`, and the Debugger (the Evaluator
  area and Edit variable window of the Bindings area) can parse pids, ports,
  references, and external funs, as long as they can be created in the running
  system.

  Own Id: OTP-14296

- Fix editing of simple binary values in the Bindings area of the Debugger's
  Attach Process Window.

  Own Id: OTP-14318

### Improvements and New Features

- Miscellaneous updates due to atoms containing arbitrary Unicode characters.

  Own Id: OTP-14285

## Debugger 4.2.1

### Fixed Bugs and Malfunctions

- Update build scripts to not make assumptions about where env, cp and perl are
  located.

  Own Id: OTP-13800

- A bug causing non-interpreted code to crash the debugger has been fixed.

  Own Id: OTP-13756

## Debugger 4.2

### Improvements and New Features

- When the debugger searches for source files, it will also use the location of
  the source in the compilation information part of the BEAM file.

  Own Id: OTP-13375

## Debugger 4.1.2

### Improvements and New Features

- Documentation corrections.

  Own Id: OTP-12994

## Debugger 4.1.1

### Fixed Bugs and Malfunctions

- Fix crash when starting a quick debugging session. Thanks Alan Duffield.

  Own Id: OTP-12911 Aux Id: seq12906

## Debugger 4.1

### Improvements and New Features

- Support variables as Map keys in expressions and patterns

  Erlang will accept any expression as keys in Map expressions and it will
  accept literals or bound variables as keys in Map patterns.

  Own Id: OTP-12218

## Debugger 4.0.3

### Fixed Bugs and Malfunctions

- Fix save state which did not work on Mac.

  Own Id: OTP-12378

## Debugger 4.0.2

### Fixed Bugs and Malfunctions

- Make sure to install .hrl files when needed

  Own Id: OTP-12197

- Invoking debugger functions `ia/1` and `iaa/1` crashed, when it tried to
  invoke the old and removed gs based gui functions.

  Own Id: OTP-12357

## Debugger 4.0.1

### Fixed Bugs and Malfunctions

- Fix evaluation of map updates in the debugger and erl_eval

  Reported-by: José Valim

  Own Id: OTP-11922

## Debugger 4.0

### Fixed Bugs and Malfunctions

- The debugger now correctly evaluates code such as '`X = true andalso X`'.
  (Thanks to Anthony Ramine.)

  Own Id: OTP-11553

- A few subtle bugs in the evaluation of code in the debugger has been
  corrected. (Thanks to Anthony Ramine.)

  Own Id: OTP-11676

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

### Improvements and New Features

- Removed gs based applications and gs based backends. The `observer`
  application replaces the removed applications.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-10915

- Support Maps syntax in debugger (Thanks to Anthony Ramine).

  Own Id: OTP-11673

## Debugger 3.2.12

### Improvements and New Features

- Fix matching of floating point middle-endian machines. Thanks to Johannes
  Weissl.

  Own Id: OTP-11201

## Debugger 3.2.11

### Improvements and New Features

- A new checkbox has been added. When it is checked, the range set by the `erl`
  flag `+pc` is used for determining when to print lists of integers as strings.
  When it is unchecked, integer lists are never printed as strings.

  A minor incompatibility: settings saved by Erlang R16B01 or later cannot be
  read by Erlang R16B or earlier.

  Own Id: OTP-10899

- Erlang source files with non-ASCII characters are now encoded in UTF-8
  (instead of latin1).

  Own Id: OTP-11041 Aux Id: OTP-10907

## Debugger 3.2.10

### Improvements and New Features

- The +pc flag to erl can be used to set the range of characters considered
  printable. This affects how the shell and io:format("~tp",...) functionality
  does heuristic string detection. More can be read in STDLIB users guide.

  Own Id: OTP-10884

## Debugger 3.2.9

### Fixed Bugs and Malfunctions

- Fix Debugger settings dialog due to changed behavior in wxFileDialog (Thanks
  to Håkan Mattsson)

  Own Id: OTP-10621

### Improvements and New Features

- Support for Unicode has been implemented.

  Own Id: OTP-10302

- Where necessary a comment stating encoding has been added to Erlang files. The
  comment is meant to be removed in Erlang/OTP R17B when UTF-8 becomes the
  default encoding.

  Own Id: OTP-10630

- Integer lists and utf-8 binaries in variables are now displayed as strings.

  Own Id: OTP-10679

## Debugger 3.2.8

### Fixed Bugs and Malfunctions

- Fixed disappearing breakpoints bug, reported by Ricardo Catalinas Jiménez.

  Own Id: OTP-9950

## Debugger 3.2.7

### Fixed Bugs and Malfunctions

- Fix "OK" spelling in debugger messages and variables

  Simple code refactor in the debugger: renames all the occurrences of "Ok" to
  "OK" in the code, variable names and strings. This improves the consistency of
  the code and follows the GTK UI where "OK" is always used.(Thanks to Ricardo
  Catalinas Jiménez)

  Own Id: OTP-9699

### Improvements and New Features

- Variables are now now allowed in '`fun M:F/A`' as suggested by Richard O'Keefe
  in EEP-23.

  The representation of '`fun M:F/A`' in the abstract format has been changed in
  an incompatible way. Tools that directly read or manipulate the abstract
  format (such as parse transforms) may need to be updated. The compiler can
  handle both the new and the old format (i.e. extracting the abstract format
  from a pre-R15 BEAM file and compiling it using compile:forms/1,2 will work).
  The `syntax_tools` application can also handle both formats.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9643

## Debugger 3.2.6

### Improvements and New Features

- Fix issues reported by dialyzer.

  Own Id: OTP-9107

## Debugger 3.2.5

### Improvements and New Features

- Miscellaneous updates

  Own Id: OTP-8976

## Debugger 3.2.4

### Improvements and New Features

- Type specs have been added/cleaned up. (Thanks to Kostis Sagonas.)

  Own Id: OTP-8757

## Debugger 3.2.3

### Improvements and New Features

- Warnings due to new autoimported BIFs removed

  Own Id: OTP-8674 Aux Id: OTP-8579

- The predefined builtin type tid() has been removed. Instead, ets:tid() should
  be used.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8687

## Debugger 3.2.2

### Fixed Bugs and Malfunctions

- Bugs have been fixed in the evaluation of comprehensions and short-circuit
  expressions in guards.

  Own Id: OTP-8310

### Improvements and New Features

- Miscellaneous corrections of the WX version of the debugger.

  Own Id: OTP-8346

## Debugger 3.2.1

### Improvements and New Features

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8201

## Debugger 3.2

### Improvements and New Features

- Added a new gui, start with debugger:start(gs) for old gui.

  Own Id: OTP-7366

- The undocumented, unsupported, and deprecated guard BIF `is_constant/1` has
  been removed.

  \*** INCOMPATIBILITY with R12B \***

  Own Id: OTP-7673

## Debugger 3.1.1.4

### Improvements and New Features

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

## Debugger 3.1.1.3

### Improvements and New Features

- Minor changes.

  Own Id: OTP-7388

## Debugger 3.1.1.2

### Improvements and New Features

- The documentation has been updated so as to reflect the last updates of the
  Erlang shell as well as the minor modifications of the control sequence `p` of
  the `io_lib` module.

  Superfluous empty lines have been removed from code examples and from Erlang
  shell examples.

  Own Id: OTP-6944 Aux Id: OTP-6554, OTP-6911

## Debugger 3.1.1.1

### Improvements and New Features

- Minor Makefile changes.

  Own Id: OTP-6689

- Obsolete guard tests (such as list()) have been replaced with the modern guard
  tests (such as is_list()).

  Own Id: OTP-6725

## Debugger 3.1.1

### Fixed Bugs and Malfunctions

- Removed some dead code from `dbg_ieval`.

  Own Id: OTP-6552

## Debugger 3.1

### Fixed Bugs and Malfunctions

- The interpreter could not handle the case where an expression that should
  evaluate to a fun referred to uninterpreted code.

  Own Id: OTP-6061 Aux Id: seq10295

- Timeout for `gen_server` calls within Debugger is now set to `infinity`.

  Own Id: OTP-6103

### Improvements and New Features

- Notification windows are now centered over a parent window.

  Own Id: OTP-6011 Aux Id: OTP-5987

- When interpreting a module, it is now checked that the module does not belong
  to the Kernel, STDLIB, GS or Debugger application, as interpreting a module
  used by the debugger/interpreter itself will lead to a deadlock or emulator
  crash.

  Also `int:interpretable(Mod)` has been extended to return `{error,{app,App}}`
  if `Mod` belongs to one of the above applications.

  Own Id: OTP-6020

- `andalso`/`orelse` is now supported.

  Own Id: OTP-6021 Aux Id: OTP-5894

## Debugger 3.0.1

### Fixed Bugs and Malfunctions

- When evaluating a guard expression for a fun function clause, the environment
  variable bindings were not taken into account.

  Own Id: OTP-5837

### Improvements and New Features

- A number of smaller improvements to the GUI:

  - Multiple choices now possible in Function Break Dialog Window.
  - Right-clicking the Module entry in a Break Dialog Window will open a popup
    menu from which the appropriate (interpreted) module can be selected.
  - Auto Attach options can now be changed using the buttons in the left part of
    the Monitor Window, not only by the corresponding menu buttons.
  - Buttons for enabling and disabling all breakpoints have been added.
  - View Module Window keyboard shortcuts corrected.

  Own Id: OTP-4284

## Debugger 3.0

### Improvements and New Features

- Debugger can now handle `try-catch`. This meant large parts of the interpreter
  had to be rewritten. Also, some small changes to the GUI have been made.

  Own Id: OTP-5730

## Debugger 2.3.3

### Improvements and New Features

- It is now possible to encrypt the debug information in Beam files, to help
  keep the source code secret. See the documentation for compile on how to
  provide the key for encrypting, and the documentation for beam_lib on how to
  provide the key for decryption so that tools such as the Debugger, Xref, or
  Cover can be used.

  The `beam_lib:chunks/2` functions now accepts an additional chunk type
  '`compile_info`' to retrieve the compilation information directly as a term.
  (Thanks to Tobias Lindahl.)

  Own Id: OTP-5460 Aux Id: seq9787

## Debugger 2.3.2

### Fixed Bugs and Malfunctions

- The graphic applications now search for HTML documentation in the correct
  place.

  Own Id: OTP-5381

## Debugger 2.3.1

### Fixed Bugs and Malfunctions

- Package support has been added to the debugger. Thanks to Richard Carlsson in
  the HiPE project.

  Own Id: OTP-5255

### Improvements and New Features

- Updated to handle the latest version of the compiler.

  Own Id: OTP-5265
