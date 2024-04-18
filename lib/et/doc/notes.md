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
# Event Tracer (ET) Release Notes

This document describes the changes made to the `Event Tracer (ET)` system from
version to version. The intention of this document is to list all
incompatibilities as well as all enhancements and bugfixes for every release of
`Event Tracer (ET)`. Each release of `Event Tracer (ET) `thus constitutes one
section in this document. The title of each section is the version number of
`Event Tracer (ET)`.

## ET 1.7

### Improvements and New Features

- Deprecates `dbg:stop_clear/0` because it is simply a function alias to
  `dbg:stop/0`

  Own Id: OTP-18478 Aux Id: GH-6903

## ET 1.6.5

### Improvements and New Features

- Fixed warnings in code matching on underscore prefixed variables.

  Own Id: OTP-17385 Aux Id: OTP-17123

## ET 1.6.4

### Fixed Bugs and Malfunctions

- The scroll bar of the et_viewer window could not be dragged all the way to the
  top of the window. It would always stop at the second event. This is now
  corrected.

  Own Id: OTP-15463 Aux Id: ERL-780

## ET 1.6.3

### Fixed Bugs and Malfunctions

- Improved documentation.

  Own Id: OTP-15190

## ET 1.6.2

### Improvements and New Features

- Calls to `erlang:get_stacktrace()` are removed.

  Own Id: OTP-14861

## ET 1.6.1

### Improvements and New Features

- Tools are updated to show Unicode atoms correctly.

  Own Id: OTP-14464

## ET 1.6

### Improvements and New Features

- Update selector to utilize new garbage collection trace tags.

  Own Id: OTP-13545

## ET 1.5.1

### Fixed Bugs and Malfunctions

- The `et` application must continue to use `erlang:now/0` in order to obtain
  timestamps that are consistent with timestamps obtained from tracing. The
  application has been updated to suppress the warning for `erlang:now/0`.

  Own Id: OTP-12780

## ET 1.5

### Fixed Bugs and Malfunctions

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

## ET 1.4.4.5

### Improvements and New Features

- The encoding of the `notes.xml` file has been changed from latin1 to utf-8 to
  avoid future merge problems.

  Own Id: OTP-11310

## ET 1.4.4.4

### Improvements and New Features

- Use erlang:demonitor(Ref, \[flush]) where applicable. Thanks to Loïc Hoguin.

  Own Id: OTP-11039

- Rename and document lists:zf/2 as lists:filtermap/2. Thanks to Anthony Ramine.

  Own Id: OTP-11078

## ET 1.4.4.3

### Improvements and New Features

- Where necessary a comment stating encoding has been added to Erlang files. The
  comment is meant to be removed in Erlang/OTP R17B when UTF-8 becomes the
  default encoding.

  Own Id: OTP-10630

## ET 1.4.4.2

### Fixed Bugs and Malfunctions

- Fix typo in ET doc (Thanks to Ricardo Catalinas Jiménez)

  Own Id: OTP-10119

## ET 1.4.4.1

### Improvements and New Features

- The GS applications is now deprecated and will be removed in the R16 release.
  The following GS-based applications have been superseded by the Observer
  application and will removed in R16: Appmon, Pman, Tv.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9907

## ET 1.4.4

### Fixed Bugs and Malfunctions

- Dialyzer warnings have been fixed.

  Own Id: OTP-9470

## ET 1.4.3

### Fixed Bugs and Malfunctions

- The popup window 'contents viewer' did not display properly on Windows.

  Own Id: OTP-9238

## ET 1.4.2

### Fixed Bugs and Malfunctions

- Fix error when module et was used in et_selector trace patterns.

  Own Id: OTP-8904

## ET 1.4.1

### Fixed Bugs and Malfunctions

- Fixed broken links in the documentation.

  Own Id: OTP-8796

### Improvements and New Features

- Fixed gui crash on windows.

  Own Id: OTP-8830

## ET 1.4

### Improvements and New Features

- Thanks to Olle Mattsson the GUI parts are rewritten to use `wxWidgets`. For
  the time being it is still possible to use the old `GS` based version of the
  tool, but it is deprecated. The `wxWidgets` based version is started by
  default.

  A print function has been added, in order to enable printing of sequence
  charts.

  A new tutorial has been added to the documentation. It is based on
  `Jayson Vantuyl's` article
  `http://souja.net/2009/04/making-sense-of-erlangs-event-tracer.html`.

  The functions `et:trace_me/4` and `et:trace_me/5` has been introduced in order
  to replace the deprecated functions `et:report_event/4` and
  `et:report_event/5`. Hopefully the new names makes it a little more obvious
  what the intended usage of the functions are.

  The `max_events` configuration parameter to `et_viewer` is not used any more.
  Now the event cache in the `Viewer` only contains those events that actually
  are displayed in the GUI.

  Own Id: OTP-8058

## ET 1.3.3

### Improvements and New Features

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the HTML frames are
  removed.

  Own Id: OTP-8201

## ET 1.3.2

### Improvements and New Features

- The start module in the app file has been corrected.

  Own Id: OTP-8078

## ET 1.3.1

### Improvements and New Features

- Changed copyright

  Own Id: OTP-7830

## ET 1.3

### Improvements and New Features

- Adjusted copyright

  Own Id: OTP-6985

## ET 1.0.1

### Improvements and New Features

- Document source changed from SGML to XML.

  Own Id: OTP-6774

## ET 1.0.0.1

### Improvements and New Features

- Minor Makefile changes.

  Own Id: OTP-6689

## Event Tracer (ET) 1.0

### Improvements and new features

This is the first release of the `Event Tracer (ET)` as a stand-alone
application separated from the Megaco application.
