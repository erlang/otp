<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2023-2025. All Rights Reserved.

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
# Syntax_Tools Release Notes

This document describes the changes made to the Syntax_Tools application.

## Syntax_Tools 4.0.1

### Fixed Bugs and Malfunctions

- Fixed zip generator crash in `annotate_bindings/1`

  Own Id: OTP-19731 Aux Id: [GH-10102], [PR-10104]

[GH-10102]: https://github.com/erlang/otp/issues/10102
[PR-10104]: https://github.com/erlang/otp/pull/10104

## Syntax_Tools 4.0

### Fixed Bugs and Malfunctions

- A few minor issues were corrected in `m:syntax_tools`, as well in the `m:erl_anno` module.

  Own Id: OTP-19422 Aux Id: [PR-9253]

[PR-9253]: https://github.com/erlang/otp/pull/9253

### Improvements and New Features

- Comprehensions have been extended with zip generators  according to [EEP 73](https://www.erlang.org/eeps/eep-0073). 
  
  Example:
  
  ```
  1> [A+B || A <- [1,2,3] && B <- [4,5,6]].
  [5,7,9]
  ```

  Own Id: OTP-19184 Aux Id: [PR-8926]

- New strict generators have been added for comprehensions.
  
  The currently existing generators are "relaxed": they ignore terms in the
  right-hand side expression that do not match the left-hand side pattern.
  
  The new strict generators fail with exception `badmatch` if a pattern doesn't match.
  
  Examples:
  
  Using the current relaxed generator operator `<-`, any element not matching
  the pattern `{_,_}` will be silently discarded:
  
  ```
  1> [T || {_,_}=T <- [{ok,1},ok,{error,2}]].
  [{ok,1},{error,2}]
  ```
  If the intention is that all lists processed by a list comprehension must only
  contain tuples of size two, using the new strict version of the operator ensures
  that term not matching will cause a crash:
  
  ```
  2> [T || {_,_}=T <:- [{ok,1},ok,{error,2}]].
  ** exception error: no match of right hand side value ok
  ```
  Using the strict generator operator to mark the intention that all list elements must match the pattern could help finding mistakes quicker if something unpexected is added to the list processed by the generator.
  
  The strict version for bitstring generators is `<:=`.

  Own Id: OTP-19317 Aux Id: [PR-8625]

- Fixed licenses in files and added ORT curations to the following apps: otp, eldap, erl_interface, eunit, parsetools, stdlib, syntax_tools, and ERTS.

  Own Id: OTP-19478 Aux Id: [PR-9376], [PR-9402], [PR-9819]

- The license and copyright header has changed format to include an `SPDX-License-Identifier`. At the same time, most files have been updated to follow a uniform standard for license headers.

  Own Id: OTP-19575 Aux Id: [PR-9670]

[PR-8926]: https://github.com/erlang/otp/pull/8926
[PR-8625]: https://github.com/erlang/otp/pull/8625
[PR-9376]: https://github.com/erlang/otp/pull/9376
[PR-9402]: https://github.com/erlang/otp/pull/9402
[PR-9819]: https://github.com/erlang/otp/pull/9819
[PR-9670]: https://github.com/erlang/otp/pull/9670

## Syntax_Tools 3.2.2.2

### Fixed Bugs and Malfunctions

- Annotate map comprehensions and generators

  Own Id: OTP-19817 Aux Id: [GH-10119]

[GH-10119]: https://github.com/erlang/otp/issues/10119

## Syntax_Tools 3.2.2.1

### Fixed Bugs and Malfunctions

- Backport fix for annotating maybe to OTP-27

  Own Id: OTP-19740 Aux Id: [GH-10103], [PR-10118]

[GH-10103]: https://github.com/erlang/otp/issues/10103
[PR-10118]: https://github.com/erlang/otp/pull/10118

## Syntax_Tools 3.2.2

### Fixed Bugs and Malfunctions

- Annotation of `maybe` expressions has been corrected.

  Own Id: OTP-19405 Aux Id: [PR-8811]

[PR-8811]: https://github.com/erlang/otp/pull/8811

## Syntax_Tools 3.2.1

### Fixed Bugs and Malfunctions

- The documentation for `syntax_tools` has been polished after the migration to the new documentation system.

  Own Id: OTP-19102 Aux Id: [PR-8515]

[PR-8515]: https://github.com/erlang/otp/pull/8515

## Syntax_Tools 3.2

### Fixed Bugs and Malfunctions

- The `m:epp_dodger` module can now handle the `maybe` and `else` keywords.

  Own Id: OTP-18608 Aux Id: [GH-7266], [PR-7267]

- Reverting a `#wrapper` will no longer throw away changes made to positions/annotations.

  Own Id: OTP-18805 Aux Id: [PR-7398]

[GH-7266]: https://github.com/erlang/otp/issues/7266
[PR-7267]: https://github.com/erlang/otp/pull/7267
[PR-7398]: https://github.com/erlang/otp/pull/7398

### Improvements and New Features

- The type `t:erl_syntax:annotation_or_location/0` is now exported.

  Own Id: OTP-18715 Aux Id: [PR-7535]

- The documentation has been migrated to use Markdown and ExDoc.

  Own Id: OTP-18955 Aux Id: [PR-8026]

[PR-7535]: https://github.com/erlang/otp/pull/7535
[PR-8026]: https://github.com/erlang/otp/pull/8026

## Syntax_Tools 3.1

### Improvements and New Features

- Map comprehensions as suggested in EEP 58 has now been implemented.

  Own Id: OTP-18413 Aux Id: EEP-58, PR-6727

## Syntax_Tools 3.0.1

### Fixed Bugs and Malfunctions

- `erl_syntax_lib:annotate_bindings/1,2` will now properly annotate named
  functions and their arguments.

  Own Id: OTP-18380 Aux Id: PR-6523, GH-4733

## Syntax_Tools 3.0

### Fixed Bugs and Malfunctions

- The `erl_syntax_lib:analyze_attribute/1` function would return
  `{Name, {Name, Value}}` instead of `{Name, Value}` (which is the documented
  return value).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17894 Aux Id: PR-5509

### Improvements and New Features

- Added support for configurable features as described in EEP-60. Features can
  be enabled/disabled during compilation with options
  (`-enable-feature Feature`, `-disable-feature Feature` and
  `+{feature, Feature, enable|disable}`) to `erlc` as well as with directives
  (`-feature(Feature, enable|disable).`) in the file. Similar options can be
  used to `erl` for enabling/disabling features allowed at runtime. The new
  `maybe` expression (EEP-49) is fully supported as the feature `maybe_expr`.
  The features support is documented in the reference manual.

  Own Id: OTP-17988

## Syntax_Tools 2.6

### Fixed Bugs and Malfunctions

- The type spec of `erl_syntax:function_type/2` has been fixed.

  Own Id: OTP-17302 Aux Id: PR-4705

- Output parentheses around `BitStringExpr` when pretty-printing binary
  comprehensions.

  Own Id: OTP-17321

### Improvements and New Features

- Support for handling abstract code created before OTP R15 has been dropped.

  Own Id: OTP-16678 Aux Id: PR-2627

- The `igor` and `erl_tidy` modules have been the removed and are now maintained
  by their original author Richard Carlsson. They can be found at
  [github.com/richcarl/igor](https://github.com/richcarl/igor) and
  [github.com/richcarl/erl_tidy](https://github.com/richcarl/erl_tidy),
  respectively.

  Own Id: OTP-17180

## Syntax_Tools 2.5

### Fixed Bugs and Malfunctions

- `epp_dodger` was unable to handle a parameterized macro in a function head.

  Own Id: OTP-17064 Aux Id: GH-4445, PR-2964

### Improvements and New Features

- All functions in `erl_tidy` in syntax_tools have now been deprecated and are
  scheduled for removal in OTP 24. Users who still need it can find it at
  [https://github.com/richcarl/erl_tidy](https://github.com/richcarl/erl_tidy).

  Own Id: OTP-17167 Aux Id: OTP-17046

## Syntax_Tools 2.4

### Improvements and New Features

- In the `syntax_tools` application, the `igor` module and all functions in
  `erl_tidy` except `file/2` have been deprecated.

  Own Id: OTP-17046

## Syntax_Tools 2.3.1

### Fixed Bugs and Malfunctions

- Minor documentation fix of `erl_syntax:operator/1`.

  Own Id: OTP-16732 Aux Id: PR-2659

## Syntax_Tools 2.3

### Improvements and New Features

- Remove incomplete support for `cond` expressions.

  Own Id: OTP-15925 Aux Id: PR-2304

- Improved indentation for code generated with `erl_prettypr` and `tidier`.

  Own Id: OTP-16386 Aux Id: PR-2451

## Syntax_Tools 2.2.1

### Fixed Bugs and Malfunctions

- Add missing calls to `erl_syntax:unwrap/1`. The nodes concerned represent
  names and values of maps and map types.

  Own Id: OTP-16012 Aux Id: PR-2348

## Syntax_Tools 2.2

### Fixed Bugs and Malfunctions

- All incorrect (that is, all) uses of "can not" has been corrected to "cannot"
  in source code comments, documentation, examples, and so on.

  Own Id: OTP-14282 Aux Id: PR-1891

### Improvements and New Features

- Correct links in the documentation.

  Own Id: OTP-15761

## Syntax_Tools 2.1.7.1

### Fixed Bugs and Malfunctions

- Add missing calls to `erl_syntax:unwrap/1`. The nodes concerned represent
  names and values of maps and map types.

  Own Id: OTP-16012 Aux Id: PR-2348

## Syntax_Tools 2.1.7

### Fixed Bugs and Malfunctions

- Fix pretty-printing of type funs.

  Own Id: OTP-15519 Aux Id: ERL-815

## Syntax_Tools 2.1.6

### Fixed Bugs and Malfunctions

- Fix the `TypeName` type in erl_syntax_lib.

  Own Id: OTP-15207 Aux Id: PR-1888

- Correct unfolding of the stacktrace variable.

  Own Id: OTP-15291 Aux Id: ERL-719

- Correct `erl_syntax:revert/1` bug regarding the types `t:map/0` and
  `t:tuple/0`.

  Own Id: OTP-15294

### Improvements and New Features

- Support bitstrings as literals in module `erl_syntax`.

  Own Id: OTP-15165 Aux Id: PR-1842

## Syntax_Tools 2.1.5

### Improvements and New Features

- Update to use the new string api instead of the old.

  Own Id: OTP-15036

## Syntax_Tools 2.1.4.2

### Fixed Bugs and Malfunctions

- Add missing calls to `erl_syntax:unwrap/1`. The nodes concerned represent
  names and values of maps and map types.

  Own Id: OTP-16012 Aux Id: PR-2348

## Syntax_Tools 2.1.4.1

### Fixed Bugs and Malfunctions

- Fix a bug regarding reverting map types.

  Own Id: OTP-15098 Aux Id: ERIERL-177

## Syntax_Tools 2.1.4

### Fixed Bugs and Malfunctions

- Removed all old unused files in the documentation.

  Own Id: OTP-14475 Aux Id: ERL-409, PR-1493

## Syntax_Tools 2.1.3

### Improvements and New Features

- General Unicode improvements.

  Own Id: OTP-14462

- A process trapping exits and calling `erl_tidy` no longer hangs if an error
  occurs.

  Own Id: OTP-14471 Aux Id: ERL-413

## Syntax_Tools 2.1.2

### Improvements and New Features

- Miscellaneous updates due to atoms containing arbitrary Unicode characters.

  Own Id: OTP-14285

## Syntax_Tools 2.1.1.1

### Fixed Bugs and Malfunctions

- Fix a bug regarding reverting map types.

  Own Id: OTP-15098 Aux Id: ERIERL-177

## Syntax_Tools 2.1.1

### Fixed Bugs and Malfunctions

- The address to the FSF in the license header has been updated.

  Own Id: OTP-14084

## Syntax_Tools 2.1

### Fixed Bugs and Malfunctions

- Fix a bug where `erl_tidy` crashed on the tilde character when printing to
  standard output.

  Own Id: OTP-13725 Aux Id: ERL-151, PR-1071

- `merl_transform` could get into an infinite loop when syntactically incorrect
  text was passed to a `merl:qquote/2,3` call.

  Own Id: OTP-13755

### Improvements and New Features

- Improve types and specs in OTP documentation generated from Erlang source
  files.

  Own Id: OTP-13720 Aux Id: ERL-120

## Syntax_Tools 2.0

### Improvements and New Features

- The abstract data type in `erl_syntax` is augmented with types and function
  specifications.

  The module `erl_prettypr` pretty prints types and function specification, and
  the output can be parsed.

  The types of record fields are no longer ignored. As a consequence
  `erl_syntax_lib:analyze_record_field/1` returns `{Default, Type}` instead of
  `Default`. The functions `analyze_record_attribute`, `analyze_attribute`,
  `analyze_form`, and `analyze_forms` in the `erl_syntax_lib` module are also
  affected by this incompatible change.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12863

## Syntax_Tools 1.7

### Improvements and New Features

- Use the new `erl_anno` module.

  Own Id: OTP-12732

- The `merl` module has been added to the `syntax_tools` application. The Merl
  library is a simpler way to work with erl_syntax parse trees.

  Own Id: OTP-12769

## Syntax_Tools 1.6.18

### Fixed Bugs and Malfunctions

- Fix bad format of error in epp_dodger:parse_file/3

  Own Id: OTP-12406

## Syntax_Tools 1.6.17

### Fixed Bugs and Malfunctions

- Teach Maps to erl_syntax

  Affected functions:

  - erl_syntax:abstract/1
  - erl_syntax:concrete/1
  - erl_syntax:is_leaf/1
  - erl_syntax:is_literal/1

  Own Id: OTP-12265

## Syntax_Tools 1.6.16

### Fixed Bugs and Malfunctions

- The default encoding for Erlang source files is now UTF-8. As a temporary
  measure to ease the transition from the old default of Latin-1, if EDoc
  encounters byte sequences that are not valid UTF-8 sequences, EDoc will re-try
  in Latin-1 mode. This workaround will be removed in a future release.

  Own Id: OTP-12008

## Syntax_Tools 1.6.15

### Fixed Bugs and Malfunctions

- Fix reverting map in syntax_tools

  There was a bug in erl_syntax when running e.g. erl_syntax:revert_forms,
  affecting maps. Instead of getting Key/Value you got Key/Key in the resulting
  abstract form.

  Own Id: OTP-11930

## Syntax_Tools 1.6.14

### Fixed Bugs and Malfunctions

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

### Improvements and New Features

- Add implementation of having erl_tidy print to screen instead of writing to
  the file provided. (Thanks to Aaron France)

  Own Id: OTP-11632

- Support Maps syntax in syntax_tools (Thanks to Anthony Ramine).

  Own Id: OTP-11663

## Syntax_Tools 1.6.13

### Fixed Bugs and Malfunctions

- In syntax_tools-1.6.12 (OTP R16B03) a bug was introduced which broke reverting
  of local implicit funs. Implicit funs were mistakenly thought to be using
  abstract terms for their name and arity. This has now been corrected. (Thanks
  to Anthony Ramine)

  Own Id: OTP-11576

### Improvements and New Features

- The default encoding of Erlang files has been changed from ISO-8859-1 to
  UTF-8.

  The encoding of XML files has also been changed to UTF-8.

  Own Id: OTP-10907

## Syntax_Tools 1.6.12

### Fixed Bugs and Malfunctions

- Fix transformation of implicit funs in igor (Thanks to Anthony Ramine)

  Own Id: OTP-11506

## Syntax_Tools 1.6.11

### Improvements and New Features

- Miscellaneous updates due to Unicode support.

  Own Id: OTP-10820

## Syntax_Tools 1.6.10

### Improvements and New Features

- Support for Unicode has been implemented.

  Own Id: OTP-10302

## Syntax_Tools 1.6.9

### Fixed Bugs and Malfunctions

- Syntax Tools 1.6.9

  Minor bugfixes, spec annotations and documentation cleanup. Thanks to Richard
  Carlsson

  Own Id: OTP-10208

## Syntax_Tools 1.6.8

### Fixed Bugs and Malfunctions

- Minor suppressions and fixes of compilation warnings

  Own Id: OTP-10016

## Syntax_Tools 1.6.7.2

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

- Eliminate use of deprecated regexp module

  Own Id: OTP-9810

## Syntax_Tools 1.6.7.1

### Fixed Bugs and Malfunctions

- In a file containing declarations and comments without any empty lines between
  them, the `recomment_forms()` function would associate a multi-line comment
  with the declaration above it rather than the one following it. (Thanks to
  Richard Carlsson and Kostis Sagonas.)

  Own Id: OTP-9180

## Syntax_Tools 1.6.7

### Improvements and New Features

- Miscellaneous updates

  Own Id: OTP-8976

## Syntax_Tools 1.6.6

### Improvements and New Features

- Minor changes and clean-ups.

  Own Id: OTP-8709

## Syntax_Tools 1.6.5

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

## Syntax_Tools 1.6.4

### Improvements and New Features

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8201

## Syntax_Tools 1.6.3

### Improvements and New Features

- Miscellaneous updates.

  Own Id: OTP-8190

## Syntax_Tools 1.6.2

### Improvements and New Features

- Miscellaneous updates.

  Own Id: OTP-8038

## Syntax_Tools 1.6

### Improvements and New Features

- Miscellaneous updates.

  Own Id: OTP-7877

## Syntax_Tools 1.5.6

### Improvements and New Features

- Minor updates.

  Own Id: OTP-7642

## Syntax_Tools 1.5.5

### Improvements and New Features

- Minor changes.

  Own Id: OTP-7388

## Syntax_Tools 1.5.4

### Improvements and New Features

- Minor updates, mostly cosmetic.

  Own Id: OTP-7243

## Syntax_Tools 1.5.3

### Fixed Bugs and Malfunctions

- A missing directory (`examples`) has been added and another broken link in the
  documentation has been fixed.

  Own Id: OTP-6468

## Syntax_Tools 1.5.2

### Fixed Bugs and Malfunctions

- Fixed some broken links in the documentation.

  Own Id: OTP-6420

## Syntax_Tools 1.5.1

Miscellaneous changes.
