<!--
%CopyrightBegin%

Copyright Ericsson AB 2023. All Rights Reserved.

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
# Erl_Docgen Release Notes

This document describes the changes made to the _erl_docgen_ application.

## Erl_Docgen 1.5.2

### Fixed Bugs and Malfunctions

- Fix erl_docgen dtd to only allow a single `datatype_title` within a
  `datatypes` block.

  Own Id: OTP-18775 Aux Id: PR-7663

- Fix so that EEP-48 doc chunks include the module summary and generates equiv
  tags in the correct order.

  The function/type group title is now also included in the entry metadata.

  Own Id: OTP-18776 Aux Id: PR-7663

- Update all <tt> html tags to be <code> instead.

  Own Id: OTP-18799 Aux Id: PR-7695

## Erl_Docgen 1.5.1

### Fixed Bugs and Malfunctions

- Expanded the documentation about how to use the `standard_io`,
  `standard_error` and `user` I/O devices.

  Added the types [`io:standard_io/0`](`t:io:standard_io/0`),
  `io:standard:error/0` and [`io:user/0`](`t:io:user/0`).

  Own Id: OTP-18676 Aux Id: PR-7473 GH-7459

## Erl_Docgen 1.5

### Improvements and New Features

- New XML tag <change>. Like a <note> box, but with a different (steel blue)
  color and label. Intended to contain talk about semantic differences between
  OTP releases.

  Own Id: OTP-18338 Aux Id: PR-6408

- Add "since" attribute to XML taglist/tag elements to document OTP version when
  a particular option/feature was introduced. The version is shown out in the
  right margin, similar to "since" versions for functions.

  Own Id: OTP-18501 Aux Id: PR-6987

## Erl_Docgen 1.4

### Improvements and New Features

- Update DTD to allow XML tag `em` under `pre`.

  Own Id: OTP-18244

## Erl_Docgen 1.3

### Fixed Bugs and Malfunctions

- Fix types that are not derived from code to display the correct information in
  EEP-48 style documentation chunks.

  Own Id: OTP-18084 Aux Id: PR-5948

### Improvements and New Features

- Any exported, but private function or module in Erlang/OTP now generate an
  EEP-48 style documentation entry with the content set to hidden.

  Before this change, exported but private functions and modules did not have
  any entry at all.

  Own Id: OTP-17669 Aux Id: PR-5226

## Erl_Docgen 1.2.1

### Fixed Bugs and Malfunctions

- Fix css for large tables and images on small screens

  Own Id: OTP-17852

- Fix bug with codeinclude tag that caused it to not include the code if used in
  a correct, but unexpected way.

  Own Id: OTP-17907 Aux Id: GH-5662 PR-5666

## Erl_Docgen 1.2

### Fixed Bugs and Malfunctions

- Fix `codeinclude` tag to correctly respect the `type` attribute.

  Own Id: OTP-17754 Aux Id: PR-5365

### Improvements and New Features

- The HTML documentation has been updated to collapse better on small screens.

  Own Id: OTP-17687 Aux Id: PR-5197

- All predefined types have been added to the `erlang` module together with
  documentation.

  Any reference to a predefined type now links to that documentation so that the
  user can view it.

  Own Id: OTP-17689 Aux Id: PR-5292

## Erl_Docgen 1.1.2

### Fixed Bugs and Malfunctions

- Fix codeinclude tags to work as part of reference manual documentation.

  Own Id: OTP-17458 Aux Id: PR-4764

- Fix creation of link elements with anchors in EEP-48 style documentation.

  Own Id: OTP-17575 Aux Id: PR-5132

## Erl_Docgen 1.1.1

### Fixed Bugs and Malfunctions

- Fix links generated in specs to types in other applications to point to the
  correct place. This bug was introduced in Erlang/OTP 24.0.

  Own Id: OTP-17433 Aux Id: GH-4849 PR-4857

## Erl_Docgen 1.1

### Fixed Bugs and Malfunctions

- Fixed bug in `m:shell_docs` and `erl_docgen` that interpreted `em` tags as
  `strong`.

  Own Id: OTP-17122

- Missing runtime dependencies has been added to this application.

  Own Id: OTP-17243 Aux Id: PR-4557

- Fix bug where `see*` elements within `type`/`name` were removed when
  generating html. Bug has been present since OTP-21.

  Own Id: OTP-17257

### Improvements and New Features

- Add support for displaying `.svg` images.

  Own Id: OTP-16877

- Updated the way specs are generated after changes in edoc.

  Own Id: OTP-17192 Aux Id: PR-2803

## Erl_Docgen 1.0.2

### Fixed Bugs and Malfunctions

- Fix links in titles to github and anchors to work.

  Own Id: OTP-17013

- Fix some typing errors on variable names in documentation examples.

  Own Id: OTP-17065 Aux Id: ERL-1386

## Erl_Docgen 1.0.1

### Fixed Bugs and Malfunctions

- Repaired lost function "since" versions in the right margin of the module
  reference HTML documentation.

  Own Id: OTP-16661 Aux Id: ERL-1259

- Remove erlang compilation warnings and trailing whitespaces.

  Own Id: OTP-16675

## Erl_Docgen 1.0

### Improvements and New Features

- Embedded documentation (also known as Documentation Chunks) is now also
  available in the form of files according to
  [EEP-48](https://www.erlang.org/erlang-enhancement-proposals/eep-0048.html).
  The Documentation Chunks are produced by default when building the other
  Erlang/OTP documentation. If you want to only build the embedded documentation
  you can pass the `DOC_TARGETS=chunks` environment variable to make.

  Own Id: OTP-16406

- Minor DTD additions.

  Own Id: OTP-16497

- The `seealso` tag has been replaced with type aware tags instead. The new tags
  are: `seemfa|seeerl|seetype|seeapp|seecom|seecref|seefile|seeguide`.

  `fsdescription` has been added for adding a title to groups of functions, for
  instance Module Callbacks.

  The `dtd`s of all documentation files have been trimmed from all unused or
  rarely-used tags.

  Unused `dtd`s have been removed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16503

## Erl_Docgen 0.11

### Fixed Bugs and Malfunctions

- Fixed a cosmetic formatting bug in the data types section of generated HTML
  documentation.

  Own Id: OTP-15940

### Improvements and New Features

- Added support for specifying what language a code snippet should be
  highlighted as.

  Updated the code examples syntax highlighting library.

  Own Id: OTP-16209

## Erl_Docgen 0.10

### Improvements and New Features

- Update the documentation build support to handle FOP 2.1 .

  Own Id: OTP-16051

## Erl_Docgen 0.9.1

### Fixed Bugs and Malfunctions

- All incorrect (that is, all) uses of "can not" has been corrected to "cannot"
  in source code comments, documentation, examples, and so on.

  Own Id: OTP-14282 Aux Id: PR-1891

## Erl_Docgen 0.9

### Improvements and New Features

- The HTML reference documentation shows the OTP version where modules and
  functions were first introduced. Versions older than R13B04 are not shown.

  Own Id: OTP-15460

- Make html documentation of C functions with many arguments more readable.

  Own Id: OTP-15637 Aux Id: PR-2160

## Erl_Docgen 0.8.1

### Fixed Bugs and Malfunctions

- Indexing for the online search function has been corrected for CREF documents.

  Own Id: OTP-14406

## Erl_Docgen 0.8

### Improvements and New Features

- Add a hoverable element to the titles in the html documentation with a link to
  github where the documentation can be edited.

  Make the anchors in the html User's Guide and system documentation use the
  title of the sections instead of a generated id.

  Own Id: OTP-14979

## Erl_Docgen 0.7.3

### Fixed Bugs and Malfunctions

- Update makefile so db_funcs.xsl is a part of the installed application.

  Own Id: OTP-15091

## Erl_Docgen 0.7.2

### Fixed Bugs and Malfunctions

- The style for code, warning and note tags in the pdf have been changed so they
  look like the html version.  
  The spacing around code blocks have been changed for both html and pdf so it's
  the same regardless if the user have a newline after the start tag (or before
  the end tag) or not.

  Own Id: OTP-14674

## Erl_Docgen 0.7.1

### Improvements and New Features

- General Unicode improvements.

  Own Id: OTP-14462

## Erl_Docgen 0.7

### Fixed Bugs and Malfunctions

- Sort index of C functions alphabetically in the sidebar.

  Own Id: OTP-14333 Aux Id: ERL-393

- The right side index of functions now handle functions with same name but
  different arity.

  Own Id: OTP-14431

### Improvements and New Features

- Improvements in the OTP documentation style.

  (Thanks to Mariano Guerra)

  Own Id: OTP-14371 Aux Id: PR-1215

## Erl_Docgen 0.6.1

### Fixed Bugs and Malfunctions

- Docgen would previously emit "utf8" as the default encoding in xml. This has
  now been remedied by emitting the correct string "UTF-8" instead.

  Own Id: OTP-13971

## Erl_Docgen 0.6

### Improvements and New Features

- Improve types and specs in OTP documentation generated from Erlang source
  files.

  Own Id: OTP-13720 Aux Id: ERL-120

## Erl_Docgen 0.5

### Fixed Bugs and Malfunctions

- Generate HTML anchors for data types without `name` attribute.

  Own Id: OTP-13600 Aux Id: Jira: ERL-141

- Updated make rules so it's possible to use the xmllint target for checking the
  system documentation.  
  Removed usage of non defined DTD tag (output) from the system documentation
  and corrected a number of xml faults.

  Added support for quote tag and a new level of header formatting in
  erl_docgen.

  A fault when generating html for manual set markers for section headings is
  corrected so now is the title visible after hyperlink jump.

  Own Id: OTP-13638

- Corrected the space handling for the seealso tag.

  Own Id: OTP-13639

### Improvements and New Features

- Sort the modules function index alphabetically.

  Own Id: OTP-13668 Aux Id: PR-543

## Erl_Docgen 0.4.2

### Fixed Bugs and Malfunctions

- Correctly generate anno tags for maps keys

  Own Id: OTP-12955

## Erl_Docgen 0.4.1

### Improvements and New Features

- Updated the xmllint target to just check the xml files with real documentation
  content.  
  Corrected some errors and added some missing target in the DTD's.

  Own Id: OTP-13026

## Erl_Docgen 0.4

### Improvements and New Features

- Add possibility to add extra information on documentation front pages.

  Own Id: OTP-12722

## Erl_Docgen 0.3.7

### Fixed Bugs and Malfunctions

- Maps: Properly align union typed assoc values in documentation

  Own Id: OTP-12190

## Erl_Docgen 0.3.6

### Fixed Bugs and Malfunctions

- Fix spec to doc generation from erl_docgen and edoc for maps

  Own Id: OTP-12058

## Erl_Docgen 0.3.5

### Fixed Bugs and Malfunctions

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

### Improvements and New Features

- Handle map types in docgen_edoc_xml_cb

  Own Id: OTP-11776

## Erl_Docgen 0.3.4.1

### Improvements and New Features

- The encoding of the `notes.xml` file has been changed from latin1 to utf-8 to
  avoid future merge problems.

  Own Id: OTP-11310

## Erl_Docgen 0.3.4

### Fixed Bugs and Malfunctions

- Correct a PDF indentation bug for tagged lists in tagged lists and added some
  missing tags to the DTD.

  Own Id: OTP-10725

### Improvements and New Features

- Support for Unicode has been implemented.

  Own Id: OTP-10302

- Where necessary a comment stating encoding has been added to Erlang files. The
  comment is meant to be removed in Erlang/OTP R17B when UTF-8 becomes the
  default encoding.

  Own Id: OTP-10630

- Some examples overflowing the width of PDF pages have been corrected.

  Own Id: OTP-10665

## Erl_Docgen 0.3.3

### Improvements and New Features

- A possibility to configure erl_docgen so it can generate documentation for
  other products than Erlang/OTP.

  Own Id: OTP-9040

## Erl_Docgen 0.3.2

### Fixed Bugs and Malfunctions

- A bug regarding spaces in C function prototypes has been fixed. (Thanks to
  Richard O'Keefe.)

  Own Id: OTP-10138

## Erl_Docgen 0.3.1

### Fixed Bugs and Malfunctions

- Some links in C libraries were not generated correctly. This bug has been
  fixed.

  Own Id: OTP-9832

- Set \`font-family: Courier, monospace' in OTP doc CSS

  left Courier as the primary original font and also added monospace as
  secondary for people which in Linux does not have it installed.

  Also adds minor cosmetic changes to the CSS. (Thanks to Ricardo Catalinas
  Jiménez)

  Own Id: OTP-9918

- When generating from edoc it is now possible to use ranges in specs and <img>
  tags in the description.

  Own Id: OTP-9970

### Improvements and New Features

- Optimize and improve handling of multiple func:s.

  Own Id: OTP-9877

- The generation of the libraries' HTML documentation has been optimized.

  Own Id: OTP-9893

## Erl_Docgen 0.3

### Fixed Bugs and Malfunctions

- Bug fixes concerning the generation of manpages.

  Own Id: OTP-9614

- Fix syntax bug in eix files.

  Own Id: OTP-9617

- Bug fix concerning the generation of manpages.

  Own Id: OTP-9759

- Fixed an arity calculation bug for erlang functions in the documentation index
  for html and pdf.

  Own Id: OTP-9772

### Improvements and New Features

- The docbuilder application is removed in R15 and parts still used in the OTP
  documentation build process and the DTD documentation is moved to erl_docgen.

  Own Id: OTP-9721

## Erl_Docgen 0.2.6

### Fixed Bugs and Malfunctions

- Bug fixes.

  Own Id: OTP-9360

- The manpage generation has been improved.

  Own Id: OTP-9541 Aux Id: OTP-9550

- Fix eix file generation for new function spec references.

  Own Id: OTP-9562

- The function signatures in the pdf files was not in a fixed font.

  Own Id: OTP-9563

- The parts level in the system documentation was missing in the bookmarks menu
  for the pdf and the copyright year generation for PDF was not correct.

  Own Id: OTP-9576

- The indentation after `Warning:` and `Note:` in manpages has been improved.

  Own Id: OTP-9588

## Erl_Docgen 0.2.5

### Improvements and New Features

- The support for using Erlang specifications and types has been improved.

  Own Id: OTP-9261

## erl_docgen 0.2.4

### Fixed Bugs and Malfunctions

- Subsections below level 2 where not handled correct when generating html and
  pdf.

  Own Id: OTP-90730

## erl_docgen 0.2.3

### Fixed Bugs and Malfunctions

- Fix format_man_pages so it handles all man sections and remove warnings/errors
  in various man pages.

  Own Id: OTP-8600

### Improvements and New Features

- Support for using Dialyzer specifications and types has been added. This is an
  experimental release; changes are expected before the new functionality is
  used when building the OTP documentation.

  Own Id: OTP-8720

## erl_docgen 0.2.2

### Fixed Bugs and Malfunctions

- Fixed the transformation from xml to html of the funcs block in comref pages.

  Own Id: OTP-8792

## erl_docgen 0.2.1

### Fixed Bugs and Malfunctions

- The text within code examples (CODE and PRE tags) will not be space normalized
  for man pages.

  Own Id: OTP-8476

## erl_docgen 0.2

### Improvements and New Features

- Added an xsl transform from OTP xml documentation to a file (.eix) of erlang
  terms that can be read by the erldoc application. Erldoc handles the
  documentation search mechanism at erlang.org. Added generation of eix files to
  otp_release_targets.mk. Fixed a copyright date error in db_html.xsl .

  Own Id: OTP-8308

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

## erl_docgen 0.1

### Improvements and new features

This is the first release of erl_docgen for internal use.
