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
# EDoc Release Notes

This document describes the changes made to the EDoc application.

## Edoc 1.2.1

### Fixed Bugs and Malfunctions

- Emit <code> instead of <tt>.

  Own Id: OTP-18782 Aux Id: PR-7643

## Edoc 1.2

### Fixed Bugs and Malfunctions

- Fix unused types warnings in internal edoc module.

  Own Id: OTP-17550 Aux Id: GH-5094 PR-5106

### Improvements and New Features

- Add source file to the warning on skipped tags when generating EEP-48 style
  docs.

  Own Id: OTP-17556 Aux Id: PR-5023

- Fix the doc chunks generators to emit documentation even if there is not
  module level documentation.

  Fix the doc chunks generators to respect the `@hidden` and `@private` tags
  properly for both modules and functions.

  Own Id: OTP-17733 Aux Id: PR-5205

## Edoc 1.1

### Improvements and New Features

- Add option `link_predefined_types` that is used to create links to erlang
  predefined types. This is mainly to be used by erl_docgen when creating the
  Erlang/OTP documentation.

  Own Id: OTP-17743 Aux Id: PR-5292

## Edoc 1.0.1

### Fixed Bugs and Malfunctions

- Fix broken documentation link in edoc_extract:file/4.

  Own Id: OTP-17552 Aux Id: GH-5058

## Edoc 1.0

### Fixed Bugs and Malfunctions

- Fix so that the edoc_doclet option `file_suffix` also effects the links
  emitted into the module index.

  Own Id: OTP-17092

### Improvements and New Features

- EDoc, the Erlang documentation engine, hits version 1.0 with this release,
  which means a few changes.

  EDoc is now capable of emitting EEP-48 doc chunks. This means that, with some
  configuration, community projects can now provide documentation for
  `m:shell_docs` the same way that OTP libraries did since OTP 23.0.

  The `@spec` and `@type` EDoc tags have been deprecated. These are not
  supported with the new chunk-generating doclet and layout. Moreover,
  previously when there was a redundant `@spec` tag and `-spec` attribute
  defined for the same function, the `@spec` tag would take precedence. Now, the
  `-spec` attribute takes precedence and is more important. The same is true for
  redundant `@type` tags and `-type` attributes. Warnings are now emitted when
  such redundant entries are found.

  The `?NO_APP` macro in `edoc_doclet.hrl` has been deprecated. Use the atom
  `no_app` instead.

  See the [Doc chunks chapter in the Edoc User's Guide](chapter.md) for more
  details.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16949 Aux Id: PR-2803 OTP-17192

- Edoc has been updated to use `-spec` to document its own interface instead of
  `@doc@` tags.

  Together with this change the inter-application linking for `-spec` style
  documentation has been improved.

  Own Id: OTP-17095 Aux Id: PR-2914

- Allow user defined edoc macros to be functions.

  Own Id: OTP-17153 Aux Id: PR-2674

## Edoc 0.12

### Improvements and New Features

- Remove Inets dependency from EDoc.

  Own Id: OTP-15999 Aux Id: PR-2317

- Add support for overloaded Erlang specifications.

  Own Id: OTP-16407 Aux Id: PR-2430

- Refactored the internal handling of deprecated and removed functions.

  Own Id: OTP-16469

## Edoc 0.11

### Improvements and New Features

- Correct links in the documentation.

  Own Id: OTP-15761

## Edoc 0.10

### Improvements and New Features

- Print a helpful message explaining that adding `{preprocess, true}` can help
  if reading a source file fails.

  Own Id: OTP-15605 Aux Id: ERL-841

## Edoc 0.9.4

### Fixed Bugs and Malfunctions

- Improved documentation.

  Own Id: OTP-15190

## Edoc 0.9.3

### Fixed Bugs and Malfunctions

- Types and function specifications including the `t:map/0` type are
  pretty-printed correctly.

  Own Id: OTP-15117

### Improvements and New Features

- Update to use the new string api instead of the old.

  Own Id: OTP-15036

## Edoc 0.9.2

### Fixed Bugs and Malfunctions

- The map type is correctly denoted as `t:map/0` in function specifications and
  types.

  Own Id: OTP-14777

## Edoc 0.9.1

### Improvements and New Features

- Tools are updated to show Unicode atoms correctly.

  Own Id: OTP-14464

## Edoc 0.9

### Improvements and New Features

- To support stable builds, `edoc` no longer includes time stamps in the footer
  for generated files.

  Own Id: OTP-14277

- Miscellaneous updates due to atoms containing arbitrary Unicode characters.

  Own Id: OTP-14285

## Edoc 0.8.1

### Improvements and New Features

- Document the function tags `@param` and `@returns`.

  Own Id: OTP-13930 Aux Id: PR-1175

## Edoc 0.8

### Improvements and New Features

- Improve types and specs in OTP documentation generated from Erlang source
  files.

  Own Id: OTP-13720 Aux Id: ERL-120

## Edoc 0.7.19

### Improvements and New Features

- Handle typed record fields.

  Own Id: OTP-13558

## Edoc 0.7.18

### Fixed Bugs and Malfunctions

- Assign correct names to list arguments.

  Own Id: OTP-13234 Aux Id: ERL-63

### Improvements and New Features

- Unless the `sort_functions` option is `true`, `edoc_layout` does not sort
  functions.

  Own Id: OTP-13302

## Edoc 0.7.17

### Improvements and New Features

- Remove functionality related to packages

  Own Id: OTP-12431

## Edoc 0.7.16

### Fixed Bugs and Malfunctions

- Maps: Properly align union typed assoc values in documentation

  Own Id: OTP-12190

## Edoc 0.7.15

### Fixed Bugs and Malfunctions

- Fix spec to doc generation from erl_docgen and edoc for maps

  Own Id: OTP-12058

## Edoc 0.7.14

### Fixed Bugs and Malfunctions

- The default encoding for Erlang source files is now UTF-8. As a temporary
  measure to ease the transition from the old default of Latin-1, if EDoc
  encounters byte sequences that are not valid UTF-8 sequences, EDoc will re-try
  in Latin-1 mode. This workaround will be removed in a future release.

  Own Id: OTP-12008

## Edoc 0.7.13

### Fixed Bugs and Malfunctions

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

## Edoc 0.7.12.1

### Improvements and New Features

- The encoding of the `notes.xml` file has been changed from latin1 to utf-8 to
  avoid future merge problems.

  Own Id: OTP-11310

## Edoc 0.7.12

### Fixed Bugs and Malfunctions

- EDoc sometimes failed to associate a comment with the preceding type
  declaration. This bug has been fixed. (Thanks to Serge Aleynikov for reporting
  the bug.)

  Own Id: OTP-10866

### Improvements and New Features

- Miscellaneous updates due to Unicode support.

  Own Id: OTP-10820

## Edoc 0.7.11

### Improvements and New Features

- Since EDoc 0.7.7 (R14B02) separate values of union types can be annotated.
  However, the parser has hitherto chosen not to add the necessary parentheses
  due to backwards compatibility.

  From this release on code traversing the output of `edoc_parser` needs to take
  care of parentheses around separate values of union types. Examples of such
  code are layout modules and doclet modules.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-10195

- Support for Unicode has been implemented.

  Own Id: OTP-10302

- Where necessary a comment stating encoding has been added to Erlang files. The
  comment is meant to be removed in Erlang/OTP R17B when UTF-8 becomes the
  default encoding.

  Own Id: OTP-10630

## Edoc 0.7.10

### Fixed Bugs and Malfunctions

- List behaviour callbacks in Edoc when using -callback attribute. (Thanks to
  Magnus Henoch.)

  Added special case for file names under Windows. (Thanks to Beads
  Land-Trujillo.)

  Own Id: OTP-10174

## Edoc 0.7.9.1

### Improvements and New Features

- Miscellaneous documentation build updates

  Own Id: OTP-9813

## Edoc 0.7.9

### Fixed Bugs and Malfunctions

- `no_return` is a new built-in type.

  Own Id: OTP-9350

- synchronized with edoc development version

  forgot to ensure that xmerl is found in path for include_lib to work

  fix -spec declaration that doesn't work in R13B04

  eliminate warnings about unused imports

  removed CVS-keywords from source files (Thanks to Richard Carlsson )

  Own Id: OTP-9463

- Add a proplist() type

  Recently I was adding specs to an API and found that there is no canonical
  proplist() type defined. (Thanks to Ryan Zezeski)

  Own Id: OTP-9499

- Removed some never-matching clauses reported by dialyzer Fix macro expansion
  in comments following Erlang types URI-escape bytes as two hex digits always
  (reported by Alfonso De Gregorio) Updated author e-mail Recognize some more
  URI schemas in wiki text, in particular https (Thanks to Richard Carlsson)

  Own Id: OTP-9590

## Edoc 0.7.8

### Fixed Bugs and Malfunctions

- Fix infinite loop for malformed edoc input

  When processing an edoc comment with \`\`\` in it, if the comment ends without
  a matching ''' then an infinite loop occurs in the function
  edoc_wiki:strip_empty_lines/2. This change fixes that by adding a clause to
  return from the function upon the end of the comment input. This allows an
  error to be thrown to indicate the problem, which is the same behaviour as
  leaving either \`\` or \` unmatched. (Thanks to Taylor Venable)

  Own Id: OTP-9165

- Bugs concerning the option `report_missing_types` that was added in EDoc-0.7.7
  have been corrected: the option was misspelled in the source, and local
  definitions as well as the function tags `@private` and `@hidden` were not
  handled correctly. (Thanks to Manolis Papadakis.)

  Own Id: OTP-9301

## Edoc 0.7.7

### Fixed Bugs and Malfunctions

- Add encoding when parsing Wiki text. EDoc used to fail on strings such as
  "äåö". (Thanks to Richard Carlsson.)

  Own Id: OTP-9109

### Improvements and New Features

- It is now possible to use Erlang specifications and types in EDoc
  documentation. Erlang specifications and types will be used unless there is
  also a function specification (`@spec`) or a type alias (`@type`) with the
  same name. In the current implementation the placement of `-spec` matters: it
  should be placed where the `@spec` would otherwise have been placed.

  Not all Erlang types are included in the documentation, but only those
  exported by some `export_type` declaration or used by some documented Erlang
  specification (`-spec`).

  There is currently no support for overloaded Erlang specifications.

  The syntax definitions of EDoc have been augmented to cope with most of the
  Erlang types. (But we recommend that Erlang types should be used instead.)

  `edoc:read_source()` takes one new option, `report_missing_types`.
  `edoc_layout:module()` takes one new option, `pretty_printer`.

  Own Id: OTP-8525

- The `edoc_lib` module is meant to be private, but since it is referred to from
  other man pages it has been included in the OTP documentation. The
  modifications introduced in this ticket make all functions private except
  those referred to from other pages.

  Own Id: OTP-9110

## Edoc 0.7.6.8

### Improvements and New Features

- Compiler warnings were eliminated.

  Own Id: OTP-8855

## Edoc 0.7.6.7

### Fixed Bugs and Malfunctions

- Edoc now uses the new API functions to `inets` instead of the deprecated ones.

  Own Id: OTP-8749

## Edoc 0.7.6.6

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

## Edoc 0.7.6.5

### Improvements and New Features

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8201

## Edoc 0.7.6.4

### Improvements and New Features

- Miscellaneous updates.

  Own Id: OTP-8190

## Edoc 0.7.6.3

### Improvements and New Features

- The copyright notices have been updated.

  Own Id: OTP-7851

## Edoc 0.7.6.2

### Improvements and New Features

- Minor updates.

  Own Id: OTP-7642

## Edoc 0.7.6.1

### Fixed Bugs and Malfunctions

- Correction to work with new versions of STDLIB that no longer has the
  `erl_internal:obsolete/3` function.

  Own Id: OTP-7539

## Edoc 0.7.6

### Improvements and New Features

- Minor changes.

  Own Id: OTP-7388

## Edoc 0.7.5

### Improvements and New Features

- Minor updates, mostly cosmetic.

  Own Id: OTP-7243

## Edoc 0.7.3

### Improvements and New Features

- Minor Makefile changes.

  Own Id: OTP-6689

- Dialyzer warnings were eliminated.

  Own Id: OTP-6737

## EDoc 0.7.2

### Fixed Bugs and Malfunctions

- Some missing files have been added: `~/include/edoc_doclet.hrl`,
  `~/priv/edoc.dtd`, `~/priv/erlang.png`

  Own Id: OTP-6457

### Improvements and New Features

- - Undefined macros only cause warnings, not errors.
  - New, built-in `@version` macro.
  - Documented the `@docfile` and `@headerfile` generic tags.
  - Added recognition of `"TODO:"` as a wiki equivalent for `@todo` tags.
  - Added documentation about overview pages.
  - `'where'` and `','` are allowed as separators in specs.
  - Corrected ambiguity in spec grammar (possible incompatibility issue -
    parentheses may need to be added in some cases, in existing code).
  - Experimental (and undocumented) support for `@param` and `@return` tags and
    corresponding `"..."` annotations on `@spec` parameters.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6568

## EDoc 0.7.1

### Fixed Bugs and Malfunctions

- Fixed some broken links in the documentation.

  Own Id: OTP-6419

## EDoc 0.7.0

Miscellaneous changes.
