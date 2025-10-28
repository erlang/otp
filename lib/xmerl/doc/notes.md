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
# Xmerl Release Notes

This document describes the changes made to the Xmerl application.

## Xmerl 2.1.7

### Fixed Bugs and Malfunctions

- The XSD validation failed due to not handling the optional text blocks correctly in an XSD complex type with attribute `mixed=true`.

  Own Id: OTP-19792 Aux Id: ERERL-1261,[PR-10249]

[PR-10249]: https://github.com/erlang/otp/pull/10249

## Xmerl 2.1.6

### Fixed Bugs and Malfunctions

- Corrected the bug that comments couldn't be exported.
  
  `#xmlComment` elements is now exported when calling `export/3` or `export_simple/3` and similar functions. Top level comments will only be exported if one creates and export elements as a document.

  Own Id: OTP-19757 Aux Id: [PR-9796], [GH-5697]

[PR-9796]: https://github.com/erlang/otp/pull/9796
[GH-5697]: https://github.com/erlang/otp/issues/5697

## Xmerl 2.1.5

### Fixed Bugs and Malfunctions

- The type specs of `xmerl_scan:file/2` and `xmerl_scan:string/2`
  has been updated to return `t:dynamic/0`. Due to hook functions
  they can return any user defined term.

  Own Id: OTP-19662 Aux Id: [PR-9905], ERIERL-1225

[PR-9905]: https://github.com/erlang/otp/pull/9905

## Xmerl 2.1.4

### Fixed Bugs and Malfunctions

- With this change all public functions in xmerl have specs.

  Own Id: OTP-19534 Aux Id: [PR-9327]

[PR-9327]: https://github.com/erlang/otp/pull/9327

### Improvements and New Features

- The license and copyright header has changed format to include an `SPDX-License-Identifier`. At the same time, most files have been updated to follow a uniform standard for license headers.

  Own Id: OTP-19575 Aux Id: [PR-9670]

[PR-9670]: https://github.com/erlang/otp/pull/9670

## Xmerl 2.1.3.2

### Fixed Bugs and Malfunctions

- The XSD validation failed due to not handling the optional text blocks correctly in an XSD complex type with attribute `mixed=true`.

  Own Id: OTP-19792 Aux Id: ERERL-1261,[PR-10249]

[PR-10249]: https://github.com/erlang/otp/pull/10249

## Xmerl 2.1.3.1

### Fixed Bugs and Malfunctions

- The type specs of `xmerl_scan:file/2` and `xmerl_scan:string/2`
  has been updated to return `t:dynamic/0`. Due to hook functions
  they can return any user defined term.

  Own Id: OTP-19662 Aux Id: [PR-9905], ERIERL-1225

[PR-9905]: https://github.com/erlang/otp/pull/9905

## Xmerl 2.1.3

### Improvements and New Features

- A new option to discard whitespace before the `xml` tag when reading from a stream has been added to the Xmerl SAX parser.
  
  * __`{discard_ws_before_xml_document, Boolean}`__ - Discard whitespace before `xml` tag instead of returning a fatal error if set to `true` (`false` is default)

  Own Id: OTP-19602 Aux Id: [PR-9753]

[PR-9753]: https://github.com/erlang/otp/pull/9753

## Xmerl 2.1.2

### Fixed Bugs and Malfunctions

- Corrected faulty type specification
  
  Corrected type specification for the input parameter of `xmerl_xs:value_of/1`.
  
  Also replaced underscore in the return type specifications of `xmerl_xs:select/2`,
  `xmerl_xpath:string/2` and `xmerl_xpath:string/3` with specified return values to improve documentation.

  Own Id: OTP-19571 Aux Id: ERIERL-1212, [PR-9676]

[PR-9676]: https://github.com/erlang/otp/pull/9676

## Xmerl 2.1.1

### Fixed Bugs and Malfunctions

- Some old-style `catch` expressions in the xmerl_sax_parser when the continuation fun was called caused the stack to grow until all free memory was exhausted. These parts have been rewritten so that the parser now runs correctly without growing the stack. At the same time all old-style `catch` expressions in xmerl were replaced with `try`/`catch`.

  Own Id: OTP-19496 Aux Id: [GH-9190], [PR-9463]

[GH-9190]: https://github.com/erlang/otp/issues/9190
[PR-9463]: https://github.com/erlang/otp/pull/9463

## Xmerl 2.1

### Fixed Bugs and Malfunctions

- Corrected export functions from internal structure to XML so xmlText items of type
  cdata are handled correctly. They were just exported as normal text instead of
  output in a CDATA section.

  Own Id: OTP-19217 Aux Id: ERIERL-1104

### Improvements and New Features

- The type spec for `xmerl_sax_parser:file/2` has been corrected to indicate that it can return an `{error, _}` tuple.

  Own Id: OTP-19129 Aux Id: [PR-8583]

[PR-8583]: https://github.com/erlang/otp/pull/8583

## Xmerl 2.0

### Fixed Bugs and Malfunctions

- Some default values in Xmerl has been changed to avoid XML External Entity (XXE) vulnerabilities if you're parsing untrusted XML.
  
  xmerl_scan: the default value for allow_entities has changed to false.
  xmerl_sax_parser: the default value for external_entities has changed to none.

  *** POTENTIAL INCOMPATIBILITY ***

  Own Id: OTP-19079 Aux Id: [GH-7539]

- An event state in xmerl_sax_parser which  was updated in the event callback for a startEntity was not saved.

  Own Id: OTP-19086 Aux Id: [GH-8452]

[GH-7539]: https://github.com/erlang/otp/issues/7539
[GH-8452]: https://github.com/erlang/otp/issues/8452

### Improvements and New Features

- Function specifications and types have been added to all public API functions.

  Own Id: OTP-18913

- As an alternative to `xmerl_xml`, a new export module `xmerl_xml_indent` that provides out-of-the box indented output has been added.

  Own Id: OTP-18922 Aux Id: [PR-7942]

- The documentation has been migrated to use Markdown and ExDoc.

  Own Id: OTP-18955 Aux Id: [PR-8026]

[PR-7942]: https://github.com/erlang/otp/pull/7942
[PR-8026]: https://github.com/erlang/otp/pull/8026

## Xmerl 1.3.34.3

### Improvements and New Features

* A new option to discard whitespace before the `xml` tag when reading from a stream has been added to the Xmerl SAX parser.

  * __`{discard_ws_before_xml_document, Boolean}`__ - Discard whitespace before `xml` tag instead of returning a fatal error if set to `true` (`false` is default)

  Own Id: OTP-19602 Aux Id: PR-9753

## Xmerl 1.3.34.2

### Fixed Bugs and Malfunctions

* Some old-style `catch` expressions in the xmerl_sax_parser when the continuation fun was called caused the stack to grow until all free memory was exhausted. These parts have been rewritten so that the parser now runs correctly without growing the stack. At the same time all old-style `catch` expressions in xmerl were replaced with `try`/`catch`.

  Own Id: OTP-19496 Aux Id: GH-9190, PR-9463

## Xmerl 1.3.34.1

### Fixed Bugs and Malfunctions

* Corrected export functions from internal structure to XML so xmlText items of type cdata are handled correctly. They were just exported as normal text instead of output in a CDATA section.

  Own Id: OTP-19217 Aux Id: ERIERL-1104

## Xmerl 1.3.34

### Fixed Bugs and Malfunctions

- Update all <tt> html tags to be <code> instead.

  Own Id: OTP-18799 Aux Id: PR-7695

## Xmerl 1.3.33

### Fixed Bugs and Malfunctions

- The xmerl version 1.3.32 was released in OTP 26.0.1, but the incorrect version
  number of 1.3.31.1 was used for it. This incorrect version number continued to
  appear in OTP 26.0.2, OTP 26.1, and OTP 26.1.1. The actual xmerl code in these
  OTP versions however corresponds to xmerl version 1.3.32.

  Own Id: OTP-18803

## Xmerl 1.3.32

### Fixed Bugs and Malfunctions

- New options to `xmerl_scan` and `xmerl_sax_parser` so one can limit the
  behaviour of the parsers to avoid some XML security issues.

  `xmerl_scan` gets one new option:

  - **`{allow_entities, Boolean}`** - Gives the possibility to disallow entities
    by setting this option to false (`true` is default)

  `xmerl_sax_parser` gets the following options:

  - **`disallow_entities`** - Don't allow entities in document

  - **`{entity_recurse_limit, N}`** - Set a limit on entity recursion depth
    (default is 3)

  - **`{external_entities, AllowedType}`** - Specify which types of external
    entities that are allowed, this also affect external DTD's. The types are
    `all`(default), `file` and `none`

  - **`{fail_undeclared_ref, Boolean}`** - Sets the behavior for undeclared
    references due to an external file is not parsed (`true` is default)

  The old option `skip_external_dtd` is still valid and the same as
  `{external_entities, none}` and `{fail_undeclared_ref, false}` but just
  affects DTD's and not other external references.

  Own Id: OTP-18595 Aux Id: ERIERL-944

## Xmerl 1.3.31.2

### Fixed Bugs and Malfunctions

* Corrected export functions from internal structure to XML so xmlText items of type cdata are handled correctly. They were just exported as normal text instead of output in a CDATA section.

  Own Id: OTP-19217 Aux Id: ERIERL-1104

## Xmerl 1.3.31.1

### Fixed Bugs and Malfunctions

- New options to `xmerl_scan` and `xmerl_sax_parser` so one can limit the
  behaviour of the parsers to avoid some XML security issues.

  `xmerl_scan` gets one new option:

  - **\{allow_entities, Boolean\}** - Gives the possibility to disallow entities
    by setting this option to false (`true` is default)

  `xmerl_sax_parser` gets the following options:

  - **`disallow_entities`** - Don't allow entities in document

  - **`{entity_recurse_limit, N}`** - Set a limit on entity recursion depth
    (default is 3)

  - **`{external_entities, AllowedType}`** - Specify which types of external
    entities that are allowed, this also affect external DTD's. The types are
    `all`(default), `file` and `none`

  - **`{fail_undeclared_ref, Boolean}`** - Sets the behavior for undeclared
    references due to an external file is not parsed (`true` is default)

  The old option `skip_external_dtd` is still valid and the same as
  `{external_entities, none}` and `{fail_undeclared_ref, false}` but just
  affects DTD's and not other external references.

  Own Id: OTP-18595 Aux Id: ERIERL-944

## Xmerl 1.3.31

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

## Xmerl 1.3.30

### Improvements and New Features

- There is a new configure option, `--enable-deterministic-build`, which will
  apply the `deterministic` compiler option when building Erlang/OTP. The
  `deterministic` option has been improved to eliminate more sources of
  non-determinism in several applications.

  Own Id: OTP-18165 Aux Id: PR-5965

## Xmerl 1.3.29

### Improvements and New Features

- Fixed misspellings in both documentation, comments and code (internal data
  structures).

  Own Id: OTP-17935 Aux Id: PR-5590

## Xmerl 1.3.28.1

### Fixed Bugs and Malfunctions

- New options to `xmerl_scan` and `xmerl_sax_parser` so one can limit the
  behaviour of the parsers to avoid some XML security issues.

  `xmerl_scan` gets one new option:

  - **\{allow_entities, Boolean\}** - Gives the possibility to disallow entities
    by setting this option to false (`true` is default)

  `xmerl_sax_parser` gets the following options:

  - **`disallow_entities`** - Don't allow entities in document

  - **`{entity_recurse_limit, N}`** - Set a limit on entity recursion depth
    (default is 3)

  - **`{external_entities, AllowedType}`** - Specify which types of external
    entities that are allowed, this also affect external DTD's. The types are
    `all`(default), `file` and `none`

  - **`{fail_undeclared_ref, Boolean}`** - Sets the behavior for undeclared
    references due to an external file is not parsed (`true` is default)

  The old option `skip_external_dtd` is still valid and the same as
  `{external_entities, none}` and `{fail_undeclared_ref, false}` but just
  affects DTD's and not other external references.

  Own Id: OTP-18595 Aux Id: ERIERL-944

## Xmerl 1.3.28

### Fixed Bugs and Malfunctions

- Fixed a bug that could cause parsing to crash.

  Own Id: OTP-17388 Aux Id: OTP-17123

## Xmerl 1.3.27.1

### Fixed Bugs and Malfunctions

- New options to `xmerl_scan` and `xmerl_sax_parser` so one can limit the
  behaviour of the parsers to avoid some XML security issues.

  `xmerl_scan` gets one new option:

  - **`{allow_entities, Boolean}`** - Gives the possibility to disallow entities
    by setting this option to false (`true` is default)

  `xmerl_sax_parser` gets the following options:

  - **`disallow_entities`** - Don't allow entities in document

  - **`{entity_recurse_limit, N}`** - Set a limit on entity recursion depth
    (default is 3)

  - **`{external_entities, AllowedType}`** - Specify which types of external
    entities that are allowed, this also affect external DTD's. The types are
    `all`(default), `file` and `none`

  - **`{fail_undeclared_ref, Boolean}`** - Sets the behavior for undeclared
    references due to an external file is not parsed (`true` is default)

  The old option `skip_external_dtd` is still valid and the same as
  `{external_entities, none}` and `{fail_undeclared_ref, false}` but just
  affects DTD's and not other external references.

  Own Id: OTP-18595 Aux Id: ERIERL-944

## Xmerl 1.3.27

### Fixed Bugs and Malfunctions

- Fixed the bug that Xmerl SAX parser couldn't handle files ending with a ^M.

  Own Id: OTP-17308

## Xmerl 1.3.26

### Fixed Bugs and Malfunctions

- Corrected namespace and expanded name in the old dom backend example module.

  Own Id: OTP-17060

- Corrected a bug that in some cases didn't allow unresolved references when
  skip_external_dtd option used.

  Own Id: OTP-17061

## Xmerl 1.3.25

### Fixed Bugs and Malfunctions

- Fix a bug that the function name didn't get normalized in some case which left
  white spaces in links.

  Own Id: OTP-16617

## Xmerl 1.3.24

### Fixed Bugs and Malfunctions

- Fix a performance problem when using internal general references in XML
  content.

  Own Id: OTP-16578 Aux Id: ERIERL-482

## Xmerl 1.3.23

### Fixed Bugs and Malfunctions

- Replace a local floor/1 function with erlang:floor/1.

  Own Id: OTP-16324

### Improvements and New Features

- Implement handling of parsed entity content and some other improvements.

  - Updated the SAX test-suite (xmerl_sax_std_SUITE) to more recent version
  - Test now uses its own data directory since the non-SAX xmerl test suite
    (xmerl_std_SUITE) uses a different version of the other suite
  - Canonical XML checks have been added to all tests that apply. Canonical, as
    it applies here, is regular canonical XML with the addition of notation
    declarations if any. Output must match byte-for-byte.
  - External entities implemented, both as markup and replacement text
  - Cyclic references are checked at the end of the DTD parse; also when a
    reference is found
  - Default attributes are reported in events
  - Attributes are whitespace normalized if their type demands it
  - External entities can have a different encoding than the base document and
    will be transcoded to the document encoding
  - Conditional sections (IGNORE/INCLUDE) handling has been added to DTD parsing
  - Element content-model is now checked for well-formed-ness
  - A few missing function clauses have been added

  Own Id: OTP-16339

## Xmerl 1.3.22

### Fixed Bugs and Malfunctions

- `xmerl_sax_parser` crashed during charset detection when the xml declarations
  attribute values was missing the closing quotation (' or ").

  Own Id: OTP-15826

## Xmerl 1.3.21

### Fixed Bugs and Malfunctions

- A typo in an error printout has been fixed.

  Own Id: OTP-14703 Aux Id: PR-1964

## Xmerl 1.3.20.1

### Fixed Bugs and Malfunctions

- `xmerl_sax_parser` crashed during charset detection when the xml declarations
  attribute values was missing the closing quotation (' or ").

  Own Id: OTP-15826

## Xmerl 1.3.20

### Fixed Bugs and Malfunctions

- Handling of character references in attributes are fixed.

  Own Id: OTP-15684 Aux Id: ERL-837

- Normalization of whitespace characters in attributes are fixed so it works
  when character references are used.

  Own Id: OTP-15685 Aux Id: ERL-475

## Xmerl 1.3.19

### Fixed Bugs and Malfunctions

- The charset detection parsing crash in some cases when the XML directive is
  not syntactic correct.

  Own Id: OTP-15492 Aux Id: ERIERL-283

## Xmerl 1.3.18

### Fixed Bugs and Malfunctions

- Improved documentation.

  Own Id: OTP-15190

## Xmerl 1.3.17

### Fixed Bugs and Malfunctions

- Fix typos in documentation.

  Own Id: OTP-15039

## Xmerl 1.3.16.1

### Fixed Bugs and Malfunctions

- The charset detection parsing crash in some cases when the XML directive is
  not syntactic correct.

  Own Id: OTP-15492 Aux Id: ERIERL-283

## Xmerl 1.3.16

### Fixed Bugs and Malfunctions

- Removed all old unused files in the documentation.

  Own Id: OTP-14475 Aux Id: ERL-409, PR-1493

## Xmerl 1.3.15

### Fixed Bugs and Malfunctions

- Improves accumulator fun in xmerl_scan so that only one #xmlText record is
  returned for strings which have character references.

  (Thanks to Jimmy Zöger)

  Own Id: OTP-14377 Aux Id: PR-1369

## Xmerl 1.3.14

### Fixed Bugs and Malfunctions

- A couple of bugs are fixed in the sax parser (xmerl_sax_parser).

  - The continuation function was not called correctly when the XML directive
    was fragmented.
  - When the event callback modules (xmerl_sax_old_dom and xmerl_sax_simple) got
    an endDocument event at certain conditions the parser crashed.
  - Replaced internal ets table with map to avoid table leakage.

  Own Id: OTP-14430

## Xmerl 1.3.13

### Fixed Bugs and Malfunctions

- The namespace_conformant option in xmerl_scan did not work when parsing
  documents without explicit XML namespace declaration.

  Own Id: OTP-14139

- Fix a "well-formedness" bug in the XML Sax parser so it returns an error if
  there are something more in the file after the matching document. If one using
  the xmerl_sax_parser:stream() a rest is allowed which then can be sent to a
  new call of xmerl_sax_parser:stream() to parse next document.

  This is done to be compliant with XML conformance tests.

  Own Id: OTP-14211

- Fixed compiler and dialyzer warnings in the XML SAX parser.

  Own Id: OTP-14212

- Change how to interpret end of document in the XML SAX parser to comply with
  Tim Brays comment on the standard. This makes it possible to handle more than
  one doc on a stream, the standard makes it impossible to know when the
  document is ended without waiting for the next document (and not always even
  that).

  Tim Brays comment:

  Trailing "Misc"  
  The fact that you're allowed some trailing junk after the root element, I
  decided (but unfortunately too late) is a real design error in XML. If I'm
  writing a network client, I'm probably going to close the link as soon as a I
  see the root element end-tag, and not depend on the other end closing it down
  properly.  
  Furthermore, if I want to send a succession of XML documents over a network
  link, if I find a processing instruction after a root element, is it a trailer
  on the previous document, or part of the prolog of the next?

  Own Id: OTP-14213

## Xmerl 1.3.12

### Fixed Bugs and Malfunctions

- Fix a number of broken links in the xmerl documentation.

  Own Id: OTP-13880

## Xmerl 1.3.11

### Improvements and New Features

- Internal changes

  Own Id: OTP-13551

## Xmerl 1.3.10

### Improvements and New Features

- Suppress Dialyzer warnings.

  Own Id: OTP-12862

## Xmerl 1.3.9

### Fixed Bugs and Malfunctions

- Removed the built-in definitions of xml.xsd from the xmerl_xsd module.

  Own Id: OTP-13070

## Xmerl 1.3.8

### Fixed Bugs and Malfunctions

- Remove compiler warnings in xmerl.

  Own Id: OTP-12689

## Xmerl 1.3.7

### Fixed Bugs and Malfunctions

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

## Xmerl 1.3.6

### Fixed Bugs and Malfunctions

- Fixed a problem in the SAX parser when the header of the next document was
  appearing in the buffer when using xmerl_sax_parser:stream/2 function.

  Own Id: OTP-11551 Aux Id: seq12505

### Improvements and New Features

- The default encoding of Erlang files has been changed from ISO-8859-1 to
  UTF-8.

  The encoding of XML files has also been changed to UTF-8.

  Own Id: OTP-10907

## Xmerl 1.3.5

### Improvements and New Features

- Teach xmerl_xpath to resolve context namespaces in more cases. Thanks to
  Daniel White.

  Own Id: OTP-11461

- Avoid serialization on code_server in xmerl:export(). Thanks to Richard
  Carlsson.

  Own Id: OTP-11463

## Xmerl 1.3.4

### Improvements and New Features

- Fixed various typos in xmerl documentation. Thanks to David Welton.

  Own Id: OTP-11224

## Xmerl 1.3.3

### Improvements and New Features

- Where necessary a comment stating encoding has been added to Erlang files. The
  comment is meant to be removed in Erlang/OTP R17B when UTF-8 becomes the
  default encoding.

  Own Id: OTP-10630

- Some examples overflowing the width of PDF pages have been corrected.

  Own Id: OTP-10665

## Xmerl 1.3.2

### Fixed Bugs and Malfunctions

- Fix a continuation bug when a new block of bytes is to be read during parsing
  of a default declaration.

  Own Id: OTP-10063 Aux Id: seq12049

## Xmerl 1.3.1

### Fixed Bugs and Malfunctions

- Add missing spaces in xmerl doc (Thanks to Ricardo Catalinas Jiménez)

  Own Id: OTP-9873

- Fixed a continuation error in the sax parser and added latin1 as recognized
  encoding (not only the iso-8859 variants).

  Own Id: OTP-9961

- Removed the unused file xmerl_xlink.hrl. Thanks to Vlad Dumitrescu for
  informing us about it.

  Own Id: OTP-9965

- xmerl couldn't handle comments inside a type specification.

  Own Id: OTP-10023

- Fix some small errors in the sax parser: error message bug, removal of
  trailing blanks in DTD element definitions and an documentation error of the
  startDTD event in xmerl_sax_parser module.

  Own Id: OTP-10026

## Xmerl 1.3

### Fixed Bugs and Malfunctions

- Fix character check of non-characters due to change in unicode module.

  Own Id: OTP-9670

- Treat , as special in xmerl_xpath_scan. (Thanks to Anneli Cuss)

  Own Id: OTP-9753

- Fix bug in namespace handling for attributes when the `namespace_conformant`
  flag is set to true.

  Own Id: OTP-9821

### Improvements and New Features

- Updates to the xml scanner

  - xmerl_scan is now returning xmlComment records in the output.  
    Functions `xmerl_scan:file/2` and `xmerl_scan:string/2` now accepts a new
    option `{comments, Flag}` for filtering of comments.  
    Default (`true`) is that `#xmlComment` records are returned from the scanner
    and this flag should be set to false if one don't want comments in the
    output.
  - Add _default_attrs_ option  
    When _default_attrs_ is `true` any attribute with a default value defined in
    the doctype but not in the attribute axis of the currently scanned element
    is added to it.
  - Allow whole documents to be returned  
    Functions `xmerl_scan:file/2` and `xmerl_scan:string/2` now accepts a new
    option `{document, true}` to produce a whole document as a `xmlDocument`
    record instead of just the root element node.  
    This option is the only way to get to the top-level comments and processing
    instructions without hooking through the customization functions. Those
    nodes are needed to implement \[Canonical XML]\[c14n-xml] support.  
    \[c14n-xml]: http://www.w3.org/TR/2008/PR-xml-c14n11-20080129/ _Canonical
    XML_
  - Parents and namespace are tracked in `#xmlAttribute` nodes
  - Parents are tracked in `#xmlPI` nodes
  - Set `vsn` field in `#xmlDecl` record
  - Fix namespace-conformance constraints  
    See \[Namespaces in XML 1.0 (Third Edition)]\[1]: The prefix xml is by
    definition bound to the namespace name http://www.w3.org/XML/1998/namespace.
    It MAY, but need not, be declared, and MUST NOT be bound to any other
    namespace name. Other prefixes MUST NOT be bound to this namespace name, and
    it MUST NOT be declared as the default namespace.  
    The prefix xmlns is used only to declare namespace bindings and is by
    definition bound to the namespace name http://www.w3.org/2000/xmlns/. It
    MUST NOT be declared . Other prefixes MUST NOT be bound to this namespace
    name, and it MUST NOT be declared as the default namespace. Element names
    MUST NOT have the prefix xmlns.  
    In XML documents conforming to this specification, no tag may contain two
    attributes which have identical names, or have qualified names with the same
    local part and with prefixes which have been bound to namespace names that
    are identical.  
    \[1] http://www.w3.org/TR/REC-xml-names/

  Updates of xmerl's Xpath functionality.

  - Add `#xmlPI` support to xmerl_xpath:write_node/1
  - Fix processing-instruction(name?)
  - Fix path filters, support more top-level primary expressions
  - Accumulate comments in element nodes
  - Implement namespace axis  
    Namespace nodes are represented as `#xmlNsNode` records. Now that the
    namespace axis is correctly implemented, attributes nodes corresponding to
    attributes that declare namespaces are ignored.  
    See \[5.3 Attribute Nodes]\[xpath-5.3]:  
    There are no attribute nodes corresponding to attributes that declare
    namespaces.  
    \[xpath-5.3]: http://www.w3.org/TR/xpath/#attribute-nodes

  (Thanks to Anthony Ramine)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9664

- Eliminate use of deprecated regexp module

  Own Id: OTP-9810

## Xmerl 1.2.10

### Fixed Bugs and Malfunctions

- Fixed a schema search bug in xmerl_xsd.

  A new flag was needed in the xsd_state record so if the state is saved there
  is an incompatibility and a state conversion is needed.

  \*** INCOMPATIBILITY with R14B03 \***

  Own Id: OTP-9410

- Fixed xmerl_scan problems with entities in attribute values.

  Own Id: OTP-9411

- Streaming bug in xmerl_scan.

  If the continuation_fun runs out of input at the end of an attribute value
  then it crashed. (Thanks to Simon Cornish)

  Own Id: OTP-9457

- Fixed xmerl_ucs UCS2 little endian en/decoding

  Corrected number of shift bytes in xmerl_ucs:char_to_ucs2le and recursive call
  from from_ucs2le to from_ucs4le. (Thanks to Michal Ptaszek)

  Own Id: OTP-9548

- Add latin9 (iso-8859-15) support in xmerl_ucs (Thanks to David Julien)

  Own Id: OTP-9552

- Improve spelling throughout documentation, code comments and error messages

  Own Id: OTP-9555

## Xmerl 1.2.9

### Fixed Bugs and Malfunctions

- Fix minor typos and improve punctuation in the xmerl_xpath @doc comment
  (Thanks to Marcus Marinelli)

  Own Id: OTP-9187

- Prevent xmerl from over-normalizing character references in attributes

  Section 3.3.3 of the XML Recommendation gives the rules for attribute-value
  normalization. One of those rules requires that character references not be
  re-normalized after being replaced with the referenced characters. (Thanks to
  Tom Moertel)

  Own Id: OTP-9274

- Fixed the default encoding option in SAX parser.

  Own Id: OTP-9288

### Improvements and New Features

- Added the xmerl test suites and examples to the open source distribution.

  Own Id: OTP-9228

## Xmerl 1.2.8

### Fixed Bugs and Malfunctions

- The function xmerl_lib:expand_content/1 is mainly for expanding Simple XML,
  but can also handle xmerl records. This patch fixes an omission that caused
  expand_content/1 to not maintain the parents list when expanding
  #xmlElement\{\} records. (Thanks to Ulf Wiger)

  Own Id: OTP-9034

### Improvements and New Features

- Removed some dialyzer warnings.

  Own Id: OTP-9074

## Xmerl 1.2.7

### Fixed Bugs and Malfunctions

- An empty element declared as simpleContent was not properly validated.

  Own Id: OTP-8599

- Fix format_man_pages so it handles all man sections and remove warnings/errors
  in various man pages.

  Own Id: OTP-8600

### Improvements and New Features

- Fix entity checking so there are no fatal errors for undefined entities when
  option skip_external_dtd is used.

  Own Id: OTP-8947

## Xmerl 1.2.6

### Fixed Bugs and Malfunctions

- Fixed problem with hex entities in UTF-8 documents: When a document was in
  UTF-8 encoding, xmerl_scan improperly replaced hex entities by the UTF-8 bytes
  instead of returning the character, as it does with inline UTF-8 text and
  decimal entities. (Thanks to Paul Guyot.)

  Own Id: OTP-8697

## Xmerl 1.2.5

### Improvements and New Features

- All Erlang files are now built by the test server instead of the test
  directory Makefile.

  Erlang files in data directories are now built by the test suites instead of
  using prebuilt versions under version control.

  Removed a number of obsolete guards.

  Own Id: OTP-8537

- An empty element declared as a simpleContent was not properly validated.

  Own Id: OTP-8599

## Xmerl 1.2.4

### Improvements and New Features

- Updated the documentation Makefile to work with the new documentation build
  process.

  Own Id: OTP-8343

## Xmerl 1.2.3

### Fixed Bugs and Malfunctions

- A continuation clause of `parse_reference/3` had its parameters in wrong
  order.

  Own Id: OTP-8251 Aux Id: seq11429

### Improvements and New Features

- A new option to turn off the parsing of an external DTD is added to
  `xmerl_sax_parser:file/2` and `xmerl_sax_parser:stream/2`
  (`skip_external_dtd`).

  Own Id: OTP-8252 Aux Id: seq11432

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8253

## Xmerl 1.2.2

### Fixed Bugs and Malfunctions

- `xmerl_sax_parse:stream/2` failed with
  `{fatal_error,_, "Continuation function undefined, and more data needed",_,_}`
  when no continuation function was defined even though it was a complete
  document as input.

  Own Id: OTP-8213

- The namespace URI supplied on unprefixed attributes in startElement tuples is
  the same as the URI for the default namespace. According to the standard the
  namespace for an unprefixed attribute should always has no value.

  Own Id: OTP-8214

## Xmerl 1.2.1

### Fixed Bugs and Malfunctions

- `xmerl/include/xmerl.hrl` contained internal debug macros (`dbg/2` and
  `DBG/0`) which now is moved to `xmerl_internal.hrl`.

  Own Id: OTP-8084

- The function `xmerl_uri:parse/1` couldn't handle FTP URIs containing username
  and password. The default FTP port constant was also wrong. (Thanks to Steve
  Vinoski)

  Own Id: OTP-8156

### Improvements and New Features

- The SAX parser couldn't handle consecutive documents on the same stream. The
  return values are now changed so they return a rest value instead of giving an
  error about "erranous information after the end tag".

  This means that the functions `file/2` and `stream/2` now returns
  `{ok, EventState, Rest}` when the parsing is correct. The rest can then be
  used as input to a new call to `xmerl_sax_parse:stream/2`. If one know that
  it's just one document the rest value in the result tuple can be matched
  against `<<>>` or `[]` depending on if the input is in binary form or not.

  Own Id: OTP-8153 Aux Id: seq11388

## Xmerl 1.2

### Improvements and New Features

In xmerl-1.2 we have added the first Beta version of the new SAX parser (module:
xmerl_sax_parser), it supports XML 1.0. We call it Beta due to that the
validation part is not ready yet and that the parser still has some known
limitations (mostly in the DTD area).

Known limitations:

- the external DTD in the DOCTYPE declaration is handled but other external
  entities are not supported.
- the general entity values are just checked in the structure after replacement.
- parsed entities are supported on markup declaration level (e.g. partly
  replacement of markup declaration with PEReference is not supported).
- conditionalSect in external DTD's are not supported.
- recursive loops in entity declarations are not detected.

The version is increased from 1.1.12 to 1.2 is due to that the new parser is
dependent on the Unicode support that was added in OTP R13B. The old xmerl
functionality is not changed.

Own Id: OTP-6635

## Xmerl 1.1.12

### Improvements and New Features

- Updated copyright notice in source files

  Own Id: OTP-7847

## Xmerl 1.1.11

### Fixed Bugs and Malfunctions

- An empty element with a complexType and simpleContent was not properly
  validated. This error is now corrected.

  Own Id: OTP-7736

## Xmerl 1.1.10

### Fixed Bugs and Malfunctions

- Changed the examples in Customization Functions Tutorial to correct Erlang
  code.

  Own Id: OTP-6053

- Some XPath errors solved, typo in compare function '\!=', error in `id()`
  function.

  Own Id: OTP-6792 Aux Id: seq10570

- The XPath function `contains()` now implemented. See XPath 1.0 section 4.2.

  Own Id: OTP-6873

- Fixed that xmerl_xsd:process_schema/2 with \{xsdbase, Dirname\} failed with
  enoent and a number of inor documentation bugs in xmerl_xsd reference manual.

  Own Id: OTP-7165

- Fixed xmerl_scan's problem with numeric character references followed by UTF-8
  characters in the contents.

  Own Id: OTP-7430

- Fixed an incorrect guard for xmerl_scan:to_ucs/2.

  Own Id: OTP-7473

- Some bug corrections of xmerl XPath implementation, most provided by Matthew
  Dempsky.

  Own Id: OTP-7496

- Now with `t:string/0` and `name()` all XPath functions are implemented. The
  string representation of QName by `name()` is "\{Namespace URI\}local-name".

  Own Id: OTP-7510

## Xmerl 1.1.9

### Fixed Bugs and Malfunctions

- A number of minor scanner faults have got more clear error messages.

  Own Id: OTP-5998, Aux Id: seq9803

- An example error in the Xmerl Users Guide is corrected.

  Own Id: OTP-6947

- When xmerl_xsd:validate was executed the schema table in the state was deleted
  and next execution would fail. This is now corrected.

  Own Id: OTP-7288

## Xmerl 1.1.8

### Fixed Bugs and Malfunctions

- A Kleene Closure child in a sequence consumed all following children. This
  problem has been fixed.

  Own Id: OTP-7211

- Now validating xhtml1-transitional.dtd. A certain contentspec with a
  succeeding choice, that didn't match all content, followed by other child
  elements caused a failure. This is now corrected.

  Own Id: OTP-7214

## Xmerl 1.1.7

### Improvements and New Features

- xmerl's schema validation now takes default facets into account

  Own Id: OTP-7190

## Xmerl 1.1.6

### Fixed Bugs and Malfunctions

- Parsing XML with option `{validation,schema}` is now corrected.

  Own Id: OTP-6773

- union type is now supported

  Own Id: OTP-6877 Aux Id: seq10755

- Now xmerl validates as expected when a sequence has a present group member and
  a following element.

  Own Id: OTP-6910

## Xmerl 1.1.5

### Fixed Bugs and Malfunctions

- The head of a substitutionGroup may have type anyType and thus allow members
  of any type. This was an oversight, but is now corrected.

  Own Id: OTP-6720

- A recursive group reference in a redefine refers to the definition in the
  redefined schema. See 4.2.2 in XMLSchema part1 "Schema Representation
  Constraint: Individual Component Redefinition" bullet 2.

  Own Id: OTP-6739

- Solved some content model problems, for instance in some cases failed when
  more than one choice.

  Own Id: OTP-6752

## Xmerl 1.1.4

### Improvements and New Features

- An additional format is possible for the simple syntax: `{Fun, State}`. The
  fun should retrieve the replacement in simple syntax format. The semantics of
  fun:
  `fun(State) -> code that creates replacement, then returns {SimpleSyntax,NewState} | done`

  Own Id: OTP-6679

## Xmerl 1.1.3

### Improvements and New Features

- Memory consumption decreased: moved transforming from utf-8 to unicode from an
  extra pass of the document to the occasion when a character is parsed. Removed
  use of lists:subtract. Those changes also speeds up parsing in some scenarios.

  Own Id: OTP-6599 Aux Id: seq10552

## Xmerl 1.1.2

### Fixed Bugs and Malfunctions

- Schema processor reprocessed schemas that already were processed, using
  process_schemas on a system of schemas with circular dependencies.

  Own Id: OTP-6460 Aux Id: seq10564

### Improvements and New Features

- Dialyzer warnings now removed, i.e. dead code have been removed.

  Own Id: OTP-6507

## Xmerl 1.1.1

### Fixed Bugs and Malfunctions

- Bug in xmerl removed so that simple syntax element content is exported
  correctly.

  Own Id: OTP-6402 Aux Id: OTP-6099

## Xmerl 1.1

### Fixed Bugs and Malfunctions

- Xmerl failed to parse and export with the sax_file front-end. Therefore hook
  function calls were added in the parser and handling of text content were
  changed.

  Own Id: OTP-6043

- Bug in xmerl removed so that simple syntax element content is exported
  correctly.

  Own Id: OTP-6099

### Improvements and New Features

- xmerl now supports XMLSchema validation. Documentation in reference manual for
  xmerl. The release of XMLSchema validation should be considered as a beta
  release. The user interface may still be adjusted in a coming release.
  Opinions and evaluations are welcome.

  Own Id: OTP-6401

## xmerl 1.0.5

### Fixed Bugs and Malfunctions

- Code that caused compiler warnings has been reviewed.

## xmerl 1.0.4

### Fixed Bugs and Malfunctions

- xmerl behaved strange parsing a XML-document with a copyright sign in a
  comment.

  Own Id: OTP-5599

- Line count for error messages in DTD improved, still problem because of ENTITY
  expansions. Didn't delete digraphs after recursion test. Now correctly parsing
  of declaration separators \[28a-b].

  Own Id: OTP-5718

- Failed to validate a XML file with a content spec that had a choice of which
  one element was a sequence with optional elements, and all elements of that
  sequence were missing

  Own Id: OTP-5734

- Location paths for document root and attributes is now working as expected.

  Own Id: OTP-5895

- Now has the last() predicate in the XPATH modules the properties specified in
  ch 2.4 in the XPATH spec, i.e. if last() evaluates to a number other than the
  context position it is false, otherwise true.

  Own Id: OTP-5902

- The location path of a single wildcard now only selects element nodes.

  Own Id: OTP-5905

## Xmerl 1.0.3

### Fixed Bugs and Malfunctions

- Removed call of undefined function in xmerl_lib.

  Own Id: OTP-5587

## Xmerl 1.0.2

### Fixed Bugs and Malfunctions

- Better identification of errors in xml code.

  Own Id: OTP-5498 Aux Id: seq9803

- Some minor bugs fixed.

  Own Id: OTP-5500

- Parser failed on PE reference as EnumeratedType AttType, now corrected.

  Own Id: OTP-5531

## Xmerl 1.0.1

### Fixed Bugs and Malfunctions

- Fixed bug in xmerl_xpath. Xpath expressions that select nodes of type text()
  didn't work, like "context/text()", "child::text()", "descendant::text()".

  Own Id: OTP-5268 Aux Id: seq9656

- Minor bugs fixed.

  Own Id: OTP-5301

## Xmerl 1.0

### Improvements and New Features

- The OTP release of xmerl 1.0 is mainly the same as xmerl-0.20 of
  http://sowap.sourceforge.net/. It is capable of parsing XML 1.0. There have
  only been minor improvements: Some bugs that caused an unexpected crash when
  parsing bad XML. Failure report that also tells which file that caused an
  error.

  Own Id: OTP-5174
