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
# wxErlang Release Notes

This document describes the changes made to the wxErlang application.

## Wx 2.4

### Improvements and New Features

- Guards have been added to `gen_*:start*` API functions to catch bad arguments
  earlier. Before this change, in some cases, a bad argument could tag along and
  cause the server to fail later, right after start.

  Own Id: OTP-18857 Aux Id: GH-7685

## Wx 2.3.1

### Fixed Bugs and Malfunctions

- The `wx` application would fail to build on macOS with Xcode 15.

  Own Id: OTP-18768 Aux Id: PR-7670

## Wx 2.3

### Improvements and New Features

- Runtime dependencies have been updated.

  Own Id: OTP-18350

- The implementation has been fixed to use `proc_lib:init_fail/2,3` where
  appropriate, instead of `proc_lib:init_ack/1,2`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18490 Aux Id: OTP-18471, GH-6339, PR-6843

## Wx 2.2.2.1

### Fixed Bugs and Malfunctions

- The `wx` application would fail to build on macOS with Xcode 15.

  Own Id: OTP-18768 Aux Id: PR-7670

## Wx 2.2.2

### Fixed Bugs and Malfunctions

- Improve debug prints from the nifs. Some minor fixes for wxWidgets-3.2. Fixed
  OpenGL debug functions.

  Own Id: OTP-18512

## Wx 2.2.1

### Fixed Bugs and Malfunctions

- Added environment variable `WX_MACOS_NON_GUI_APP` to allow user to override
  `OSXIsGUIApplication` behavior.

  Own Id: OTP-18213 Aux Id: PR-6113

## Wx 2.2

### Improvements and New Features

- Input for `configure` scripts adapted to `autoconf` 2\.71.

  Own Id: OTP-17414 Aux Id: PR-4967

- Added `aux1Down` and `aux2Down` fields to the `wxMouseState` record. Since one
  record have been changed a recompilation of user code might be required.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17950

- Add mac specific menubar functions.

  Own Id: OTP-18008 Aux Id: PR-5816

## Wx 2.1.4

### Fixed Bugs and Malfunctions

- Fix build failure with wxWidgets-3.1.6.

  Own Id: OTP-18064 Aux Id: GH-5893

### Improvements and New Features

- Enable the possibility to build wx on windows with wxWidgets-3.1.6.

  Own Id: OTP-18061 Aux Id: GH-5883

## Wx 2.1.3

### Fixed Bugs and Malfunctions

- Fixed a bug in callback handling which could lead to a unresponsive gui.

  Own Id: OTP-17982 Aux Id: GH-5758

## Wx 2.1.2

### Fixed Bugs and Malfunctions

- Removed the `static_data` option from `wxImage` creation functions, as it was
  broken and could lead to crashes. Now image data is always copied to wxWidgets
  as was the default behavior.

  Removed some non working `wxGridEvent` event types, which have there own
  events in newer `wxWidgets` versions, and added a couple of event types that
  where missing in `wx`.

  Own Id: OTP-17947

## Wx 2.1.1

### Fixed Bugs and Malfunctions

- Fix crash in cleanup code when a gui application is exiting.

  Fix errors in the OpenGL wrapper that could cause crashes and improve the
  documentation.

  Own Id: OTP-17745

## Wx 2.1

### Fixed Bugs and Malfunctions

- Fix crash when closing an application.

  Own Id: OTP-17507

- Some functions with overloaded color arguments could not be used. For example
  the copy constructor `wxTextAttr:new(TextAttr)` did not work.

  Own Id: OTP-17577 Aux Id: GH-4999

### Improvements and New Features

- Added the Microsoft Edge WebView loader dll to the installer on windows.

  Own Id: OTP-17325

- Handle specific Mac gui application events.

  Own Id: OTP-17438 Aux Id: PR-4780

## Wx 2.0.1

### Fixed Bugs and Malfunctions

- Fix build problems when wxWidgets are built with -enable-std.

  Own Id: OTP-17407 Aux Id: GH-4834

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## Wx 2.0

### Fixed Bugs and Malfunctions

- Fix compiler warnings produced by the clang compiler.

  Own Id: OTP-17105 Aux Id: PR-2872

### Improvements and New Features

- The application has been completely rewritten in order to use wxWidgets
  version 3 as its base.

  Add basic documentation generated from the wxWidgets project.

  Own Id: OTP-16800

- The experimental HiPE application has been removed, together with all related
  functionality in other applications.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16963

- Added support for wxWebView.

  Own Id: OTP-17213 Aux Id: PR-3027

- Due to the support of the new backend versions some API incompatibilities have
  been introduced. Examples of changes are:  
  wxWindowDC default creators have been removed  
  wxClientDC default creators have been removed  
  wxPaintDC default creators have been removed  
  wxWindow:setVirtualSizeHints() has been deprecated in wxWidgets and removed  
  wxWindow:makeModal() has been deprecated in wxWidgets and removed  
  wxToolBar:add/insertTool without label have been deprecated in wxWidgets and
  removed  
  wxStyledTextCtrl some functions have changed arguments from boolean to int  
  wxSizerItem:new() Some arguments have become options  
  Removed deprecated wxSizerItem:setWindow() use assignWindow()  
  Removed deprecated wxSizerItem:setSpacer() use assignSpacer()  
  Removed deprecated wxSizerItem:setSpacer() use assignSpacer()  
  Removed deprecated wxSizerItem:setSizer() use assignSizer()  
  wxMenu append/insert/prepend have changed return value and lost IsCheckable
  argument  
  wxListCtrl:setItem/4 changed return value  
  wxImage:convertToGreyscale() options have changed  
  wxGridSizer:wxGridSizer() options have changed  
  wxGrid API have many changes  
  wxGraphicsRenderer:create*GradientBrush() uses GradientStops now  
  wxGraphicsRenderer:createPen() have been removed  
  wxGraphicsRenderer:create*GradientBrush() uses GradientStops now  
  wxGLCanvas API is incompatible  
  wxFlexGridSizer:wxFlexGridSizer() options have changed  
  wxDisplay:new() options have changed  
  wxCalendarDateAttr:new(ColText \[,OptList]) have been removed  
  wxBitmapButton:set/getBitmapSelected() have been removed

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17219 Aux Id: OTP-16800

## Wx 1.9.3.1

### Fixed Bugs and Malfunctions

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## Wx 1.9.3

### Fixed Bugs and Malfunctions

- Fixed wx initialization on mac, top level menus did not always work on newer
  MacOS versions. The menus will not work until wxWidgets-3.1.5 is released and
  used on these MacOS versions.

  Own Id: OTP-17187

## Wx 1.9.2

### Fixed Bugs and Malfunctions

- Add popup menu callback to `wxTaskBarIcon:new/1`.

  Own Id: OTP-16983 Aux Id: PR-2743

## Wx 1.9.1

### Fixed Bugs and Malfunctions

- Fix various compiler warnings on 64-bit Windows.

  Own Id: OTP-15800

### Improvements and New Features

- Refactored the internal handling of deprecated and removed functions.

  Own Id: OTP-16469

## Wx 1.9.0.1

### Fixed Bugs and Malfunctions

- Commit of generated `configure` script.

  Own Id: OTP-17420 Aux Id: OTP-17398, GH-4821

## Wx 1.9

### Improvements and New Features

- Added `wxWindow:isShownOnScreen/1`, `wxMouseEvent:getWheelAxis` and mac
  specific menubar functions. Fixed defines that have changed in newer wxWidgets
  versions, that caused some literals to become run-time dependent on wxWidgets
  version.

  Own Id: OTP-16285

## Wx 1.8.9

### Fixed Bugs and Malfunctions

- Fix a driver bug that could crashes when allocating memory.

  Own Id: OTP-15883 Aux Id: PR-2261

## Wx 1.8.8

### Fixed Bugs and Malfunctions

- All incorrect (that is, all) uses of "can not" has been corrected to "cannot"
  in source code comments, documentation, examples, and so on.

  Own Id: OTP-14282 Aux Id: PR-1891

## Wx 1.8.7

### Fixed Bugs and Malfunctions

- Improved support for wxWidgets 3.1.3 which have changed `wxFONTWEIGTH`, also
  added `wxGCDC` and `wxDisplay` modules.

  Fixed a crash on Mojave and check for events more often.

  Own Id: OTP-15587

## Wx 1.8.6

### Fixed Bugs and Malfunctions

- Fixed delayed delete bug which caused wx applications to crash on Mojave.

  Own Id: OTP-15426 Aux Id: ERL-755

## Wx 1.8.5

### Fixed Bugs and Malfunctions

- Fixed compilation warning on Darwin.

  Own Id: OTP-15230 Aux Id: PR-1860

## Wx 1.8.4

### Improvements and New Features

- Changed implementation so wx can now be built towards wxWidgets-3.1.1.

  Own Id: OTP-15027

## Wx 1.8.3

### Fixed Bugs and Malfunctions

- wx crashes in otp 20.1 if empty binaries was sent down as arguments.

  Own Id: OTP-14688

## Wx 1.8.2

### Fixed Bugs and Malfunctions

- Do not deprecate `wxGraphicsContext:createLinearGradientBrush/7` and
  `wxGraphicsContext:createRadialGradientBrush/8` which are still available in
  wxWidgets-3.0.

  Own Id: OTP-14539

### Improvements and New Features

- General Unicode improvements.

  Own Id: OTP-14462

## Wx 1.8.1

### Fixed Bugs and Malfunctions

- Fix a livelock that could be caused by `wx:batch/1`.

  Own Id: OTP-14289

## Wx 1.8

### Fixed Bugs and Malfunctions

- Allow string arguments to be binaries as specified, i.e. unicode:chardata().

  Own Id: OTP-13934 Aux Id: ERL-270

### Improvements and New Features

- Add wxWindow:dragAcceptFiles/2 and wxDropFilesEvent to support simple drag and
  drop from file browser.

  Own Id: OTP-13933

## Wx 1.7.1

### Fixed Bugs and Malfunctions

- Increased the stacksize for the wx thread. The default stacksize on Windows is
  1MB which is not enough if the user created many nested dialogs.

  Own Id: OTP-13816

## Wx 1.7

### Fixed Bugs and Malfunctions

- Fixed bugs which could cause called functions to be invoked twice or not at
  all when callbacks where invoked at the same time.

  Own Id: OTP-13491

### Improvements and New Features

- Changed atom 'boolean' fields in #wxMouseState\{\} to 'boolean()'.

  Moved out arguments in wxListCtrl:hitTest to result.

  Removed no-op functions in wxGauge that have been removed from wxWidgets-3.1.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13553

## Wx 1.6.1

### Fixed Bugs and Malfunctions

- Fixed commands with multiple binaries, such as `wxImage:new/4`. Added
  `wxWindow:SetDoubleBuffered/1`, `wxWindow:isDoubleBuffered/1`,
  `wxWindow:setTransparent/2` and `wxWindow:canSetTransparent/1`. Fixed timing
  issues.

  Own Id: OTP-13404

## Wx 1.6

### Improvements and New Features

- Add wxOverlay and make wxPostScripDC optional to make it easier to build on
  windows.

  Correct some function specifications.

  The driver implementation have been optimized and now invokes commands after
  events have been sent to erlang.

  Own Id: OTP-13160

## Wx 1.5

### Improvements and New Features

- Extend AUI functionality.

  Own Id: OTP-12961

## Wx 1.4

### Fixed Bugs and Malfunctions

- The undocumented option `generic_debug` for `gen_server` has been removed.

  Own Id: OTP-12183

- Remove raise condition where `wx` could crash during emulator stoppage.

  Own Id: OTP-12734

### Improvements and New Features

- Use wxWidgets-3.0, if found, as default backend on windows.

  Own Id: OTP-12632

- Add missing fields in some events records. May require a recompilation of user
  applications.

  Own Id: OTP-12660

## Wx 1.3.3

### Fixed Bugs and Malfunctions

- Fix timing related crash during wx application stop.

  Own Id: OTP-12374

## Wx 1.3.2

### Fixed Bugs and Malfunctions

- Fixed a minor typo in the graphicsContext example.

  Own Id: OTP-12259

### Improvements and New Features

- Distribute `autoconf` helpers to applications at build time instead of having
  multiple identical copies committed in the repository.

  Own Id: OTP-12348

## Wx 1.3.1

### Fixed Bugs and Malfunctions

- Implement --enable-sanitizers\[=sanitizers]. Similar to debugging with
  Valgrind, it's very useful to enable -fsanitize= switches to catch bugs at
  runtime.

  Own Id: OTP-12153

## Wx 1.3

### Fixed Bugs and Malfunctions

- Fix delayed destroy for wxPaintDC objects which could cause an eternal loop
  for modal dialogs.

  Fix wxSL_LABELS compatibility between wxWidgets-2.8 and wxWidgets-3.0 versions

  Own Id: OTP-11985

### Improvements and New Features

- Add missing classes wxPopup\[Transient]Window, wxActivateEvent and
  wxTextCtrl:cahngeValue/2 function.

  Own Id: OTP-11986

## Wx 1.2

### Fixed Bugs and Malfunctions

- Refactored C++ code, fixed crashes and a deadlock on linux.

  Own Id: OTP-11586

- Some local implementations of removing the last element from a list are
  replaced by `lists:droplast/1`. Note that this requires at least `stdlib-2.0`,
  which is the stdlib version delivered in OTP 17.0. (Thanks to Hans Svensson)

  Own Id: OTP-11678

- Reworked the internal event handling to avoid crashes in destroy objects.
  Thanks Tom for the bug report.

  Own Id: OTP-11699

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

## Wx 1.1.2

### Fixed Bugs and Malfunctions

- Fixed a problem which caused the debugger to crash when closing a window.
  Fixed static linking on mac.

  Own Id: OTP-11444

## Wx 1.1.1

### Fixed Bugs and Malfunctions

- wx initialization hanged with wxWidgets-3.0 on mac. Fixed a crash with
  wxListBox on wxWidgets-3.0 (thanks Sergei Golovan) Fixed documentation links.
  Fixed event callbacks cleanup.

  Own Id: OTP-11393

- Improve documentation (Thanks to Boris Mühmer)

  Own Id: OTP-11505

### Improvements and New Features

- Fix silent make rules (Thanks to Anthony Ramine)

  Own Id: OTP-11515

## Wx 1.0

### Fixed Bugs and Malfunctions

- Add \{silent_start, boolean()\} option to wx:new/1 in order to be able to
  suppress error messages during startup of wx. (Thanks to Håkan Mattsson)

  Own Id: OTP-10585

- Fix wxTreeCtrl:getBoundingRect/2 and wxTreeCtrl:hitTest/1. wxTreeCtrl:hitTest
  now returns a tuple not bug compatible with previous releases but needed.

  Own Id: OTP-10743

### Improvements and New Features

- The wx application now compiles and is usable with the unstable development
  branch of wxWidgets-2.9. Some functions are currently not available in
  wxWidgets-2.9 and their erlang counterparts are marked as deprecated. They
  will generate an error if called when linked against wxWidgets-2.9 libraries.
  This means that wx can now be built on 64bit MacOsX, but keep in mind that
  wxWidgets-2.9 is still a development branch and needs (a lot) more work before
  it becomes stable.

  Own Id: OTP-10407 Aux Id: kunagi-262 \[173]

## Wx 0.99.2

### Improvements and New Features

- Fix errors in wxDC and wxGraphicsContext api.

  Add wxTaskBarIcon.

  Add wxStyledTextControl:setEdgeMode/2.

  Add type and specs for all functions and records.

  Own Id: OTP-9947

## Wx 0.99.1

### Fixed Bugs and Malfunctions

- Fixed a deadlock in the driver, which could happen if a callback caused
  another callback to be invoked.

  Own Id: OTP-9725

### Improvements and New Features

- Implemented wxSystemOptions.

  Load Opengl from libGL.so.1 instead libGL.so to work around linux problems.

  Own Id: OTP-9702

## Wx 0.99

### Fixed Bugs and Malfunctions

- wx: fix obsolete guard warning (list/1) (Thanks to Tuncer Ayaz)

  Own Id: OTP-9513

- XML files have been corrected.

  Own Id: OTP-9550 Aux Id: OTP-9541

### Improvements and New Features

- Support virtual tables in wxListCtrl.

  Own Id: OTP-9415

## Wx 0.98.10

### Fixed Bugs and Malfunctions

- Fixed wx app files on mac and solaris. Thanks Jachym Holecek and Joe Williams.

  Own Id: OTP-9324

## Wx 0.98.9

### Fixed Bugs and Malfunctions

- Wx crashed if graphics could not be initiated, for instance if DISPLAY was not
  available.

  Wx could crash during startup, thanks Boris Muhmer for extra ordinary testing.

  Own Id: OTP-9080

- Wx on MacOS X generated complains on stderr about certain cocoa functions not
  being called from the "Main thread". This is now corrected.

  Own Id: OTP-9081

## Wx 0.98.8

### Improvements and New Features

- Add wxSystemSettings which was missing in the previous release, despite
  previous comments.

  Fix an external loop when stopping erlang nicely.

  Separate OpenGL to it's own dynamic loaded library, so other graphic libraries
  can reuse the gl module and it will not waste memory if not used.

  Own Id: OTP-8951

## Wx 0.98.7

### Fixed Bugs and Malfunctions

- Fix crash (segmentation fault) in callback handling.

  Own Id: OTP-8766

### Improvements and New Features

- Add wxSystemSettings module.

  Add wxTreeCtrl:editLabel/2.

  Own Id: OTP-8767

## Wx 0.98.6

### Improvements and New Features

- Calling `sys:get_status()` for processes that have globally registered names
  that were not atoms would cause a crash. Corrected. (Thanks to Steve Vinoski.)

  Own Id: OTP-8656

## Wx 0.98.5

### Fixed Bugs and Malfunctions

- Corrected incorrectly generated wxFileDialog:getPaths/1. Reported by
  Jason/hornja.

  Own Id: OTP-8330

- Fixed a memory reference bug which caused unexplained \{badarg, Int\} exits
  when running multiple wx applications.

  Own Id: OTP-8461

### Improvements and New Features

- Added `wxListCtrl:getEditCtrl/1` (not available on Mac).

  Own Id: OTP-8408

- Cleanups suggested by tidier and modernization of types and specs.

  Own Id: OTP-8455

- Changed representation of wxTreeItem to be an integer. This saves memory,
  where the driver do not need to keep a object reference to each tree item.

  Added getFirstChild and getNextChild to wxTreeCtrl.

  Own Id: OTP-8462

## Wx 0.98.4

### Improvements and New Features

- Added wx_object improvements from Mazen.

  Fixed pid issues, reported by Mazen.

  Added wxLogNull class, reported by Amit Murthy.

  Various configure fixes.

  Own Id: OTP-8243 Aux Id: seq11418

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8250

- wx now builds with wxWidgets 2.8.4 or a later 2.8 release, thanks Nico Kruber.

  Own Id: OTP-8292

## Wx 0.98.3

### Fixed Bugs and Malfunctions

- Added wxListCtrl sorting and build fixes supplied by Paul Hampson. Thanks.

  Own Id: OTP-8126

### Improvements and New Features

- wxHtmlWindow class implemented.

  All exceptions from callbacks are now caught and written to the log.

  Some defines where wrong in 'wx.hrl'.

  `wx:batch/1` and friends could hang forever if for instance a breakpoint was
  set inside the fun. That caused all wx applications to hang.

  Added missing wxAuiPaneInfo constructor and destructor.

  Added wxAuiNotebookEvent and wxAuiManagerEvent.

  Calling non supported wxWidgets functions hanged instead of crashed.

  Update OpenGL to version 3.1 and added some of the missing glu functions.

  Fixed wxRadioBox which inherited the wrong class, thanks Atilla Erdodi.

  Own Id: OTP-8083

- Removed some of the automatic garbage collecting after application exit, user
  will get a warning instead so he can correct the code.

  Own Id: OTP-8138

## Wx 0.98.2

### Improvements and New Features

- Olle Mattson have made a large demo, see `examples/demo/`, that triggered the
  following bugs and new features:

  New book controls.

  Added wxToolbar:addTool/6.

  Empty binaries will be used to indicate NULL where applicable.

  Own Id: OTP-7943

- Applied patch from Nico Kruber, which fixes building on some wxwidgets
  installations.

  Open source

## Wx 0.98.1

### Improvements and New Features

- Added `xrcctrl/3` to wxXmlResource and added a resource example.

  Added several event types and events records and fixed a couple of event
  related bugs.

  Event callbacks can now use `wxEvtHandler:connect/2`.

  Error handling and debugging aid have been improved.

  Added wxSplitterWindow and wxGauge:pulse and a couple of missing macros in
  `wx.hrl`.

  Thanks to Steve Davis for feedback and bug reports.

  Own Id: OTP-7875

## Wx 0.98

### Improvements and New Features

- A first beta release of wxErlang.

  Own Id: OTP-7859
