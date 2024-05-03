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
# Parsetools Release Notes

This document describes the changes made to the Parsetools application.

## Parsetools 2.5

### Improvements and New Features

- Leex has been extended with optional column number support.

  Own Id: OTP-18491 Aux Id: PR-6882

## Parsetools 2.4.1

### Improvements and New Features

- There is a new configure option, `--enable-deterministic-build`, which will
  apply the `deterministic` compiler option when building Erlang/OTP. The
  `deterministic` option has been improved to eliminate more sources of
  non-determinism in several applications.

  Own Id: OTP-18165 Aux Id: PR-5965

## Parsetools 2.4

### Improvements and New Features

- In the generated code, `yecc` will now quote all atoms coming from terminals
  in the grammar, in order to avoid conflicts with future reserved words.

  Own Id: OTP-17755

## Parsetools 2.3.2

### Fixed Bugs and Malfunctions

- The default parser include file for yecc (`yeccpre`) will no longer crash when
  attempting to print tokens when reporting an error.

  Own Id: OTP-17721

## Parsetools 2.3.1

### Fixed Bugs and Malfunctions

- Fix a bug in Leex which caused the Erlang Compiler to generate warnings.

  Own Id: OTP-17499 Aux Id: GH-4918

- Fix a bug in Yecc which caused the Erlang Compiler to generate warnings.

  Own Id: OTP-17535 Aux Id: GH-5067

## Parsetools 2.3

### Improvements and New Features

- Add types and specifications for documentation.

  Own Id: OTP-16957

- Let Leex and Yecc recognize the environment variable ERL_COMPILER_OPTIONS. Add
  Yecc option `{error_location, column | line}`.

  Own Id: OTP-17023

## Parsetools 2.2

### Improvements and New Features

- Remove usage and documentation of old requests of the I/O-protocol.

  Own Id: OTP-15695

## Parsetools 2.1.8

### Fixed Bugs and Malfunctions

- Improved documentation.

  Own Id: OTP-15190

## Parsetools 2.1.7

### Improvements and New Features

- Calls to `erlang:get_stacktrace()` are removed.

  Own Id: OTP-14861

## Parsetools 2.1.6

### Fixed Bugs and Malfunctions

- Warnings about unused functions in `leexinc.hrl` are suppressed.

  Own Id: OTP-14697

## Parsetools 2.1.5

### Fixed Bugs and Malfunctions

- Minor documentation fixes

  Own Id: OTP-14276 Aux Id: PR-1357

### Improvements and New Features

- Miscellaneous updates due to atoms containing arbitrary Unicode characters.

  Own Id: OTP-14285

## Parsetools 2.1.4

### Fixed Bugs and Malfunctions

- Correct counting of newlines when rules with newlines are used in Leex.

  Own Id: OTP-13916 Aux Id: ERL-263

- Correct handling of Unicode in Leex.

  Own Id: OTP-13919

## Parsetools 2.1.3

### Fixed Bugs and Malfunctions

- Yecc generates Dialyzer suppressions to avoid warnings when operator
  precedence declarations are used.

  Own Id: OTP-13681

## Parsetools 2.1.2

### Improvements and New Features

- Internal changes

  Own Id: OTP-13551

## Parsetools 2.1.1

### Fixed Bugs and Malfunctions

- Correct the documentation of the error tuple returned by Yecc and Leex.

  Own Id: OTP-13031

## Parsetools 2.1

### Improvements and New Features

- The new `-dialyzer()` attribute is used for suppressing Dialyzer warnings in
  generated code.

  Own Id: OTP-12271

## Parsetools 2.0.12

### Fixed Bugs and Malfunctions

- The line counter becomes invalid when rules with linewrap are used. This issue
  appears because the parsing FSM does not roll back the line counter after
  attempting such a rule.

  Own Id: OTP-12238

## Parsetools 2.0.11

### Fixed Bugs and Malfunctions

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

- A Yecc example has been updated in the documentation (Thanks to Pierre
  Fenoll.)

  Own Id: OTP-11749

## Parsetools 2.0.10

### Fixed Bugs and Malfunctions

- A bug causing Yecc to generate badly formed parsers when encountering very
  simple recursive rules has been fixed. (Thanks to Eric Pailleau.)

  Own Id: OTP-11269

- A bug where Unicode filenames combined with Latin-1 encoding could crash Yecc
  and Leex has been fixed.

  Own Id: OTP-11286

### Improvements and New Features

- Fix leex module\`s inability to build unicode-aware lexers. Thanks to Pierre
  Fenoll.

  Own Id: OTP-11313

## Parsetools 2.0.9

### Improvements and New Features

- Miscellaneous updates due to Unicode support.

  Own Id: OTP-10820

## Parsetools 2.0.8

### Improvements and New Features

- Support for Unicode has been implemented.

  Own Id: OTP-10302

- The file `esyntax.yrl` has been removed.

  Own Id: OTP-10660

## Parsetools 2.0.7

### Improvements and New Features

- Tuple funs (a two-element tuple with a module name and a function) are now
  officially deprecated and will be removed in R16. Use '`fun M:F/A`' instead.
  To make you aware that your system uses tuple funs, the very first time a
  tuple fun is applied, a warning will be sent to the error logger.

  Own Id: OTP-9649

## Parsetools 2.0.6

### Fixed Bugs and Malfunctions

- Dialyzer warnings have been removed.

  Own Id: OTP-8318

- yecc: add warnings_as_errors option(Thanks to Tuncer ayaz)

  Own Id: OTP-9376

- Fix incorrect order of pseudo variables in yecc example

  The example is for converting from infix to prefix. This change uses to
  correct ordering of the triplet. (Thanks to Garret Smith)

  Own Id: OTP-9484

- Implement or fix -Werror option

  If -Werror is enabled and there are warnings no output file is written. Also
  make sure that error/warning reporting is consistent. (Thanks to Tuncer Ayaz)

  Own Id: OTP-9536

- XML files have been corrected.

  Own Id: OTP-9550 Aux Id: OTP-9541

## Parsetools 2.0.5

### Improvements and New Features

- The formatting of Yecc's error messages has been improved. (Thanks to Joe
  Armstrong.)

  Own Id: OTP-8919

## Parsetools 2.0.4

### Fixed Bugs and Malfunctions

- Running HiPE-compiled Yecc parsers no longer results in a `function_clause`
  error.

  Own Id: OTP-8771

## Parsetools 2.0.3

### Fixed Bugs and Malfunctions

- Yecc failed to report reduce/reduce conflicts where one of the reductions
  involved the root symbol. This bug has been fixed. (Thanks to Manolis
  Papadakis.)

  Own Id: OTP-8483

- A bug introduced in Parsetools 1.4.4 (R12B-2) has been fixed. (Thanks to
  Manolis Papadakis.)

  Own Id: OTP-8486

## Parsetools 2.0.2

### Improvements and New Features

- Cleanups suggested by tidier and modernization of types and specs.

  Own Id: OTP-8455

## Parsetools 2.0.1

### Improvements and New Features

- Leex no longer uses the deprecated `regexp` module. (Thanks to Robert
  Virding.).

  Own Id: OTP-8231

### Fixed Bugs and Malfunctions

- A minor bug in `m:leex` has been fixed.

  Own Id: OTP-8197

## Parsetools 2.0

### Improvements and New Features

- Leex, a lexical analyzer generator for Erlang, has been added to Parsetools.
  This initial version should be considered experimental; it is known that there
  will be changes and additions. (Thanks to Robert Virding.).

  Own Id: OTP-8013

### Fixed Bugs and Malfunctions

- The parsers generated by Yecc now report correct error lines when possible.

  Own Id: OTP-7969

## Parsetools 1.4.7

### Fixed Bugs and Malfunctions

- A bug in yeccpre.hrl introduced in R13A has been fixed.

  Own Id: OTP-7945

## Parsetools 1.4.6

### Improvements and New Features

- Updated file headers.

  Own Id: OTP-7798

## Parsetools 1.4.5

### Improvements and New Features

- The `yecc` grammar has been augmented with an optional header section. (Thanks
  to Richard Carlsson.)

  Own Id: OTP-7292

## Parsetools 1.4.4

### Improvements and New Features

- The size of the code generated by Yecc has been reduced. The code is also
  faster.

  Macros can now be used in actions.

  Own Id: OTP-7224

## Parsetools 1.4.3

### Improvements and New Features

- [`tuple_size/1`](`tuple_size/1`) and [`byte_size/1`](`byte_size/1`) have been
  substituted for [`size/1`](`size/1`).

  Own Id: OTP-7009

## Parsetools 1.4.2

### Improvements and New Features

- The size of the code generated by yecc has been reduced.

  A note regarding the `includefile` option: although yecc can cope with
  includefiles based on some earlier `yeccpre.hrl` it is recommended for
  efficiency reasons to update includefiles as to follow the pattern in the
  latest `yeccpre.hrl`.

  Own Id: OTP-6851

## Parsetools 1.4.1.1

### Improvements and New Features

- Minor Makefile changes.

  Own Id: OTP-6689

## Parsetools 1.4.1

### Fixed Bugs and Malfunctions

- A bug concerning precedence declarations of non-terminals "one level up" has
  been fixed in yecc.

  Own Id: OTP-6362

## Parsetools 1.4

### Improvements and Fixed Bugs

Several modifications of Yecc have been made:

- The new functions `file/1,2` take the role of the old functions `yecc/2,3,4`.
  The latter functions are no longer documented but are kept for backward
  compatibility.
- More checks of the grammar file have been implemented. Examples are warnings
  for unused non-terminals and duplicated declarations.
- Invalid pseudo variables are no longer replaced by `'$undefined'` but cause a
  failure.
- Reserved words no longer need to be quoted when used as terminals or
  non-terminals.
- When compiling the generated parser file errors and warnings concerning user
  code refer to the grammar file, not the parser file.
- Yecc emits a warning if there are conflicts in the grammar. The new
  declaration `Expect` can be used to suppress this warning.
- The new operator precedence declaration `Nonassoc` can be used to declare
  operators with no associativity.
- Precedence can be given to more than one operator with one single operator
  precedence declaration.
- The function `parse_and_scan/1` in the default includefile accepts
  `{Function, A}` as well as \{\{M,F\}, A\} as tokenizer function. Exceptions in
  the tokenizer are never caught.
- The functions `yecc:file/1,2` can be accessed from the Erlang shell via the
  new functions `c:y/1,2` in STDLIB.

See yecc(3) for further details.

Own Id: OTP-5366

## Parsetools 1.3.2

### Fixed Bugs and Malfunctions

- A bug in `Yecc` that was introduced in R9B has been removed. Another bug
  concerning precedence declaration "one level up" has been fixed.

  Own Id: OTP-5461

## Parsetools 1.3.1

### Fixed Bugs and Malfunctions

- A bug in the file `parsetools/include/yeccpre.hrl` caused
  `yecc:parse_and_scan/1` to always report a parse failure when the lexer
  reported end-of-file. This problem has been fixed.

  Own Id: OTP-5369
