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
# Observer Release Notes

This document describes the changes made to the Observer application.

## Observer 2.16

### Fixed Bugs and Malfunctions

- The dependencies for this application are now listed in the app file.

  Own Id: OTP-18831 Aux Id: [PR-7441]

[PR-7441]: https://github.com/erlang/otp/pull/7441

### Improvements and New Features

- The new function `proc_lib:set_label/1` can be used to add a descriptive term to any process that does not have a registered name. The name will be shown by tools such as `\c:i/0`, `m:observer`, and it will be included in crash reports produced by processes using `m:gen_server`, `m:gen_statem`, `m:gen_event`, and `m:gen_fsm`.
  
  The label for a process can be retrieved by calling `proc_lib:get_label/1`.
  
  Note that those functions work on any process, not only processes that use `m:proc_lib`.
  
  Example:
  
  ```text
  1> self().
  <0.90.0>
  2> proc_lib:set_label(my_label).
  ok
  3> i().
      .
      .
      .
  <0.90.0>              erlang:apply/2                        2586    75011    0
  my_label              c:pinfo/2                               51
  4> proc_lib:get_label(self()).
  my_label
  ```

  Own Id: OTP-18789 Aux Id: [PR-7720], [PR-8003]

- The documentation has been migrated to use Markdown and ExDoc.

  Own Id: OTP-18955 Aux Id: [PR-8026]

- `m:etop` has been updated to use `dbg:session/2` in order to not interfere with any other tracing.

  Own Id: OTP-19082 Aux Id: [PR-8363]

[PR-7720]: https://github.com/erlang/otp/pull/7720
[PR-8003]: https://github.com/erlang/otp/pull/8003
[PR-8026]: https://github.com/erlang/otp/pull/8026
[PR-8363]: https://github.com/erlang/otp/pull/8363

## Observer 2.15.1

### Fixed Bugs and Malfunctions

- Closing the trace log window via the menu did not work.

  Own Id: OTP-18722 Aux Id: PR-7462

## Observer 2.15

### Improvements and New Features

- Runtime dependencies have been updated.

  Own Id: OTP-18350

- Added `start/1`, `start_and_wait functions/1|2` functions.

  Own Id: OTP-18430 Aux Id: PR-6397

- Deprecates `dbg:stop_clear/0` because it is simply a function alias to
  `dbg:stop/0`

  Own Id: OTP-18478 Aux Id: GH-6903

## Observer 2.14.0.1

### Fixed Bugs and Malfunctions

* Fixed runtime dependencies.

  Own Id: OTP-19064

## Observer 2.14

### Fixed Bugs and Malfunctions

- A WX event race could causes a crash in when handling socket or port info.

  Own Id: OTP-18339

### Improvements and New Features

- Improve the nodes menu to include more nodes.

  Own Id: OTP-18269 Aux Id: PR-6030

## Observer 2.13

### Improvements and New Features

- Fixed units in gui.

  Own Id: OTP-18151 Aux Id: PR-6063

## Observer 2.12

### Fixed Bugs and Malfunctions

- Fixed default handling of Mac specific menus.

  Own Id: OTP-17996 Aux Id: PR-5795

- Reading port socket options on macOS and Windows "skips" invalid options.

  Own Id: OTP-18012 Aux Id: #5798

### Improvements and New Features

- The configuration files [`.erlang`](`e:erts:erl_cmd.md`),
  [`.erlang.cookie`](`e:system:distributed.md`) and
  [`.erlang.crypt`](`m:beam_lib#module-erlang-crypt`) can now be located in the XDG
  Config Home directory.

  See the documentation for each file and `filename:basedir/2` for more details.

  Own Id: OTP-17554 Aux Id: GH-5016 PR-5408 OTP-17821

## Observer 2.11.1

### Fixed Bugs and Malfunctions

- Fixed node information lookup for remote process identifiers in
  `crashdump_viewer`.

  Own Id: OTP-17995 Aux Id: PR-5804

## Observer 2.11

### Improvements and New Features

- Calculate the display width in `etop`, instead of hard-coding it to 89
  characters.

  Own Id: OTP-17880 Aux Id: PR-5557

## Observer 2.10.1

### Fixed Bugs and Malfunctions

- Fix bug in crash dumps where the stackframe of a process would be printed
  using an incorrect format.

  Crash dump viewer has also been fixed to be able read the broken stack format.

  The bug has existed since Erlang/OTP 23.0.

  Own Id: OTP-17814 Aux Id: PR-5462

## Observer 2.10

### Fixed Bugs and Malfunctions

- Atoms with Unicode code points greater than 255 (for example Greek or Cyrillic
  characters) would not be displayed correctly by `crashdump_viewer`.

  Own Id: OTP-17377

- Fix the crashdump_viewer to be able to parse
  [`monitor_node/2`](`erlang:monitor_node/2`) monitors correctly.

  Own Id: OTP-17425 Aux Id: PR-4799

### Improvements and New Features

- Observer now has a sectiion for new socket.

  Own Id: OTP-17346

- Added a trace all button to the trace window.

  Own Id: OTP-17520 Aux Id: PR-4962

## Observer 2.9.6

### Improvements and New Features

- Updated gui parts to work with the new wx version.

  Own Id: OTP-17214

## Observer 2.9.5

### Fixed Bugs and Malfunctions

- Fix graph windows flickering on windows.

  Own Id: OTP-16778

## Observer 2.9.4

### Improvements and New Features

- Minor updates due to the new spawn improvements made.

  Own Id: OTP-16368 Aux Id: OTP-15251

## Observer 2.9.3

### Fixed Bugs and Malfunctions

- Crashdump Viewer how handles crash dumps where the Old Binary VHeap has
  overflowed.

  Own Id: OTP-16296

## Observer 2.9.2

### Fixed Bugs and Malfunctions

- Fix bug after a user followed link on a pid from an expanded term window.

  Own Id: OTP-15980 Aux Id: PR-2201

### Improvements and New Features

- Improved dark mode colors on Linux.

  Own Id: OTP-15916 Aux Id: ERL-921

## Observer 2.9.1

### Fixed Bugs and Malfunctions

- All incorrect (that is, all) uses of "can not" has been corrected to "cannot"
  in source code comments, documentation, examples, and so on.

  Own Id: OTP-14282 Aux Id: PR-1891

## Observer 2.9

### Fixed Bugs and Malfunctions

- Since Logger was introduced in OTP-21.0, menu choice _Log > Toggle Log View_
  in observer would cause a crash unless an `error_logger` event handler was
  explicitly installed. This is now corrected.

  Own Id: OTP-15553 Aux Id: ERL-848

### Improvements and New Features

- Since persistent_term was introduced, observer would sometimes crash when
  expanding a term from a process state. This is now corrected.

  Own Id: OTP-15493 Aux Id: ERL-810

- Add `OBSERVER_SCALE` environment variable for HiDPI support.

  Own Id: OTP-15586 Aux Id: PR-2105

## Observer 2.8.2

### Fixed Bugs and Malfunctions

- Literals such as `#{"one"=>1}` dumped to a crash dump would cause
  `crashdump_viewer` to crash.

  Own Id: OTP-15365 Aux Id: ERL-722

- `crashdump_viewer` would sometimes crash when processing a dump which was
  truncated in the `literals` area. This is now corrected.

  Own Id: OTP-15377

- Since OTP-20.2, `crashdump_viewer` was very slow when opening a crash dump
  with many processes. An ets:select per process could be removed, which
  improved the performance a lot.

  A bug when parsing heap data in a crashdump caused `crashdump_viewer` to crash
  when multiple `Yc` lines referenced the same reference counted binary. This is
  now corrected.

  Own Id: OTP-15391

## Observer 2.8.1

### Fixed Bugs and Malfunctions

- Improved documentation.

  Own Id: OTP-15190

## Observer 2.8

### Fixed Bugs and Malfunctions

- Added possibility to garbage collect selected processes and fixed a crash when
  the saved config file contained bad data.

  Own Id: OTP-14993 Aux Id: PR-1666

### Improvements and New Features

- Use uri_string module instead of http_uri.

  Own Id: OTP-14902

## Observer 2.7

### Fixed Bugs and Malfunctions

- etop.hrl used a relative path to include observer_backend.hrl, this is now
  changed to use include_lib instead. runtime_tools/include is added to the
  tertiary bootstrap.

  Own Id: OTP-14842 Aux Id: ERL-534

- If a crashdump was truncated in the attributes section for a module,
  crashdump_viewer would crash when a module view was opened from the GUI. This
  bug was introduced in OTP-20.2 and is now corrected.

  Own Id: OTP-14846 Aux Id: ERL-537

- Optimized ets and mnesia table view tab in observer gui, listing 10000 tables
  was previously very slow.

  Own Id: OTP-14856 Aux Id: ERIERL-117

### Improvements and New Features

- When a process has many links and/or monitors, it could earlier take very long
  time to display the process information window. This is now improved by only
  showing a few links and monitors, and then an link named "more..." to expand
  the rest.

  Own Id: OTP-14725

- More crash dump info such as: process binary virtual heap stats, full info for
  process causing out-of-mem during GC, more port related info, and dirty
  scheduler info.

  Own Id: OTP-14820

## Observer 2.6

### Fixed Bugs and Malfunctions

- A bug introduced in OTP-20 would make Crashdump Viewer crash when trying to
  expand an empty binary. This is now corrected.

  Own Id: OTP-14642

- If a match spec in the config file contained more than one clause, observer
  would earlier crash when trying to display it in the GUI. This is now
  corrected.

  Own Id: OTP-14643 Aux Id: ERL-489

- Writing of crash dumps is significantly faster.

  Maps are now included in crash dumps.

  Constants terms would only be shown in one process, while other processes
  referencing the same constant term would show a marker for incomplete heap.

  Own Id: OTP-14685 Aux Id: OTP-14611, OTP-14603, OTP-14595

### Improvements and New Features

- Binaries and some other data in crash dumps are now encoded in base64 (instead
  of in hex), which will reduce the size of crash dumps.

  A few bugs in the handling of sub binaries in `crashdump_viewer` have been
  fixed.

  Own Id: OTP-14686

- In order to allow future improvements, Crashdump Viewer now checks the version
  tag of the crashdump to see that it is a known format. If the crashdump
  version is newer than Crashdump Viewer is prepared to read, then an
  information dialog is displayed before Crashdump Viewer terminates.

  If an incomplete process heap is discovered in a crashdump, Crashdump Viewer
  will now display a warning for this, similar to the warning displayed when a
  crashdump is truncated. Incomplete heaps can occur if for instance the
  literals are not included, which is the case for all dumps prior to OTP-20.2.

  Own Id: OTP-14755

## Observer 2.5

### Improvements and New Features

- The following improvements are done to Crashdump Viewer:

  - Reading of crash dumps with many binaries is optimized.
  - A progress bar is shown when the detail view for a process is opened.
  - The `cdv` script now sets `ERL_CRASH_DUMP_SECONDS=0` to avoid generating a
    new crash dump from the node running the Crashdump Viewer.
  - A warning dialog is shown if the node running the Crashdump Viewer could
    potentially overwrite the crash dump under inspection.
  - Bugfix: In some situations, Crashdump Viewer could not find the end of the
    'Last calls' section in a crash dump, and would erroneously mark the crash
    dump as truncated. This is now corrected.
  - Bugfix: In some situations, process info for a specific process would be
    marked as truncated by Crashdump Viewer, even if the crash dump was
    truncated in the binary section - and not related to the process in
    question. This is now corrected.

  Own Id: OTP-14386

- General Unicode improvements.

  Own Id: OTP-14462

- Tools are updated to show Unicode atoms correctly.

  Own Id: OTP-14464

- Add system statistics and limits to frontpage in observer.

  Own Id: OTP-14536

## Observer 2.4

### Fixed Bugs and Malfunctions

- `etop` had a hardcoded timeout value of 1 second when waiting for data from a
  remote node. When this expired, which could happen for instance if there were
  very many processes on the remote node, etop would exit with reason
  `connection_lost`. To overcome this problem, the timeout is now changed to be
  the same as the update interval, which is configurable.

  Own Id: OTP-14393

### Improvements and New Features

- Show dirty-scheduler threads in performance monitor graph and add a column
  with maximum allocated memory in the Memory Allocators table.

  Own Id: OTP-14137

- Keep table and port selection after refresh of tables. Store settings before
  shutdown and restore when starting application.

  Own Id: OTP-14270

- Miscellaneous updates due to atoms containing arbitrary Unicode characters.

  Own Id: OTP-14285

- When observing a node older than OTP-19.0, a pop-up will be displayed when
  trying to access port information. Earlier, observer would crash in this
  situation.

  Own Id: OTP-14345 Aux Id: ERL-399

## Observer 2.3.1

### Fixed Bugs and Malfunctions

- etop erroneously reported the average scheduler utilization since the tool was
  first started instead of the scheduler utilization since last update. This is
  now corrected.

  Own Id: OTP-14090 Aux Id: seq13232

- crashdump_viewer crashed when the 'Slogan' had more than one line. This is now
  corrected.

  Own Id: OTP-14093 Aux Id: ERL-318

- When clicking an HTML-link to a port before the port tab has been opened for
  the first time, observer would crash since port info is not initiated. This is
  now corrected.

  Own Id: OTP-14151 Aux Id: PR-1296

- The dialyzer and observer applications will now use a portable way to find the
  home directory. That means that there is no longer any need to manually set
  the HOME environment variable on Windows.

  Own Id: OTP-14249 Aux Id: ERL-161

## Observer 2.3

### Fixed Bugs and Malfunctions

- The shell script (priv/bin/cdv) and bat file (priv/bin/cdv.bat) which can be
  used for starting crashdump_viewer both started a distributed erlang node.
  This would cause any attempt at starting a second instance of the
  crashdump_viewer to fail. To solve this problem, cdv and cdv.bat now use
  non-distributed nodes when starting the crashdump_viewer.

  Own Id: OTP-14010

- A bug caused the number of buckets to be shown in the 'Objects' column, and
  the number of objects to be shown in the 'Memory' column for ets table in
  crashdump_viewer. This is now corrected.

  Own Id: OTP-14064

### Improvements and New Features

- Add option `queue_size` to ttb:tracer/2. This sets the maximum queue size for
  the IP trace driver which is used when tracing to shell and/or `{local,File}`.

  The default value for `queue_size` is specified by `dbg`, and it is now
  changed from 50 to 200.

  Own Id: OTP-13829 Aux Id: seq13171

- The port information page is updated to show more information per port.

  Own Id: OTP-13948 Aux Id: ERL-272

## Observer 2.2.2

### Fixed Bugs and Malfunctions

- Fixed error handling in observer when mnesia tables was requested and not
  available.

  Own Id: OTP-13845 Aux Id: ERL-237

## Observer 2.2.1

### Fixed Bugs and Malfunctions

- Fixed a crash happening when observing another node, who have a different
  number of schedulers than the current one.

  Own Id: OTP-13702 Aux Id: ERL-171

## Observer 2.2

### Improvements and New Features

- Update observer GUI to support tracing on ports, and to set matchspecs for
  send/receive. This required some minor bugfixes in runtime_tools/dbg.

  Own Id: OTP-13481

- Update dbg and ttb to work with a tracer module as tracer and tracing on
  ports.

  Own Id: OTP-13500

- Added possibility to change update frequency and length of the graph windows.

  Own Id: OTP-13555

- Improved background coloring to work with dark themes and other visual
  improvements.

  Own Id: OTP-13556

- Crashdump viewer now allows port info "Port controls forker process..."

  Own Id: OTP-13647

## Observer 2.1.2

### Improvements and New Features

- Documentation corrections.

  Own Id: OTP-12994

## Observer 2.1.1

### Fixed Bugs and Malfunctions

- Show ets owner pid in crashdump viewers popup window, thanks Leo Liu.

  Own Id: OTP-13030

- Several initialisms (eg, ERTS, ETS, SMP) are used as headings. They were being
  capitalized incorrectly.

  Own Id: OTP-13044

- Fixed a crash in crashdump viewer when dump contained a truncated binary.

  Own Id: OTP-13163

## Observer 2.1

### Fixed Bugs and Malfunctions

- Show run queue status in crashdump viewer.

  Own Id: OTP-12401

- Don't refresh observer table view if there was no change

  Own Id: OTP-12819

### Improvements and New Features

- Added display of new crashdump information available in crashdumps from
  Erlang/OTP 18.

  Own Id: OTP-12363

- Added the possibility to view sasl log entries for processes.

  Own Id: OTP-12504

- Add memory allocator usage and utilization graphs.

  Own Id: OTP-12631

## Observer 2.0.4

### Fixed Bugs and Malfunctions

- Fix crash when opening a process information window.

  Own Id: OTP-12634

## Observer 2.0.3

### Fixed Bugs and Malfunctions

- A note saying only R15B nodes can be observed is removed from the user guide.

  Own Id: OTP-12078

## Observer 2.0.2

### Fixed Bugs and Malfunctions

- Fixed statusbar on Windows

  Own Id: OTP-12162

## Observer 2.0.1

### Fixed Bugs and Malfunctions

- crashdump_viewer would crash if the owner of a timer was specified as the
  process' registered name. This has been corrected.

  Own Id: OTP-11919

- Fix crash and minor updates.

  Own Id: OTP-11949

## Observer 2.0

### Fixed Bugs and Malfunctions

- etop trace handler now works in smp environment (Thanks to Péter Gömöri)

  Own Id: OTP-11633

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

- The `crashdump_viewer` is re-written using `wx`. The old `webtool` interface
  for `crashdump_viewer` does no longer exist.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11179

## Observer 1.3.1.2

### Fixed Bugs and Malfunctions

- The documentation for `ttb:tracer/2` incorrectly stated that there was an
  option named '`overload`', while the correct name used in the implementation
  is '`overload_check`'.

  Own Id: OTP-11335 Aux Id: seq12385

- Fixed typo in observer documentation. Thanks to Dave Parfitt.

  Own Id: OTP-11475

## Observer 1.3.1.1

### Improvements and New Features

- The encoding of the `notes.xml` file has been changed from latin1 to utf-8 to
  avoid future merge problems.

  Own Id: OTP-11310

## Observer 1.3.1

### Fixed Bugs and Malfunctions

- Some bugs related to calculation of CPU/scheduler utilization in observer are
  corrected.

  Current function for a process is accepted to be 'undefined' when running
  hipe.

  Own Id: OTP-10894

### Improvements and New Features

- - The new Memory field from a crash dump is now presented by crashdump viewer,
    both in the process overview and in the process detail page.
  - A summary of blocks- and carriers sizes is added to the allocator
    information page in the crashdump viewer.

  Own Id: OTP-10604 Aux Id: kunagi-336 \[247]

- Use "open" as default browser for crashdump viewer on Mac OS X. Thanks to
  Magnus Henoch.

  Own Id: OTP-10929

- Fix observer table viewer crash on formatting improper lists. Thanks to Andrey
  Tsirulev

  Own Id: OTP-10931

- Postscript files no longer needed for the generation of PDF files have been
  removed.

  Own Id: OTP-11016

- Add processes state view in observer. Thanks to Eric Pailleau.

  Own Id: OTP-11136

## Observer 1.3

### Improvements and New Features

- Where necessary a comment stating encoding has been added to Erlang files. The
  comment is meant to be removed in Erlang/OTP R17B when UTF-8 becomes the
  default encoding.

  Own Id: OTP-10630

## Observer 1.2

### Fixed Bugs and Malfunctions

- The module name in the link to the detail page for each loaded module was
  earlier not URL encoded. If the module name contained e.g. a # this could
  cause the crashdump viewer to crash when opening the link. This has been
  corrected.

  Own Id: OTP-10090 Aux Id: seq12068

- Escape control characters in Table Viewer

  Similar behaviour to old tv. Objects in tables supposed to be printed in a
  single line and it looks ugly when a \[...,10,...] integer list creates a
  new-line. Fix Table Viewer search crash on new|changed|deleted rows.

  Fix Table Viewer crash after a 'Found' -> 'Not found' search sequence

  Start position was lost after a 'Found' -> 'Not found' search sequence leading
  an undefined position in the next search. Thanks to Peti Gömori

  Own Id: OTP-10218

- observer: fix app file (Noticed-by: Motiejus Jakstys)

  Add missing observer modules to observer.app.src. Thanks to Tuncer Ayaz.

  Own Id: OTP-10221

- Make Table Viewer search a bit faster

  Edit table row in a multiline text dialog. Thanks to Peti Gomori.

  Own Id: OTP-10225

### Improvements and New Features

- Allow tracing on bifs.

  Ask epmd for local nodes, and remember users last input in connect.

  Fix crashes when a table or process information could not be retrieved.

  Own Id: OTP-10075

## Observer 1.0

### Fixed Bugs and Malfunctions

- The following bugs in `ttb` have been corrected:

  - `ttb:tracer/2` would earlier crash when trying to set up tracing for a
    diskless node to wrap files, i.e. when option
    `{file,{local,{wrap,Filename,Size,Count}}}` was used.
  - `ttb:stop([fetch])` would sometimes silently fail if multiple nodes with
    different current working directories were traced.
  - `ttb:stop([fetch])` would crash if the tracer was started with option
    `{file,{local,Filename}}`
  - A deadlock would sometimes occur due to an information printout from the
    `ttb_control` process when `ttb` was stopped.

  Own Id: OTP-9431

- The file trace port to which the IP trace client relays all traces from
  diskless nodes was not flushed and closed properly on ttb:stop. This has been
  corrected.

  Own Id: OTP-9665

### Improvements and New Features

- A new GUI for Observer. Integrating pman, etop, appmon and tv into observer
  with tracing facilities.

  Own Id: OTP-4779

- The following new features are added to `ttb`:

  - A one-command trace setup is added, `ttb:start_trace/4`.
  - The following new options are added to `ttb:tracer/2`:
    - _`shell`_ \- Show trace messages on the console in real time
    - _`timer`_ \- Time constrained tracing
    - _`overload`_ \- Overload protection
    - _`flush`_ \- Flush file trace port buffers with given frequency
    - _`resume`_ \- Automatically resume tracing after node restart
  - A new shortcut is added for common tracer settings similar to using the
    `dbg` module directly, `ttb:tracer(shell | dbg)`.
  - Some shortcuts are added for commonly used match specifications in `ttb:tp`
    and `ttb:tpl`.
  - The `Options` argument to functions `ttb:tracer`, `ttb:write_config`,
    `ttb:stop` and `ttb:format` may now be one single option instead of a list.
  - The history buffer of the last trace is now always automatically dumped to
    the file `ttb_last_config` when `ttb:stop` is called.
  - The following new options are added to `ttb:stop/1`:
    - _`fetch_dir`_ \- Specify where to store fetched logs
    - _`{format,FormatOpts}`_ \- Specify options to use when formatting the
      fetched logs
    - _`return_fetch_dir`_ \- Indicate that the return value from `ttb:stop/1`
      should include the name of the directory where the fetched logs are stored
  - The option `disable_sort` is added to `ttb:format/2`. When this option is
    used, trace messages from different logs are not merged according to
    timestamps, but just appended one log after the other.

  Own Id: OTP-9403

- The following non backwards compatible changes are done in `ttb`:

  - When setting up trace with ttb, the 'timestamp' trace flag will now always
    be set.
  - The 'fetch' option to ttb:stop/1 is removed since it is now default behavior
    that trace logs are fetched when stopping ttb. Fetching can be disabled with
    the 'nofetch' option to ttb:stop/1.
  - The name of the upload directory is changed from ttb_upload-Timestamp to
    ttb_upload_FileName-Timestamp.
  - To format the output using 'et', you now need to provide the option
    \{handler,ttb:get_et_handler()\} instead of \{handler,et\}.
  - When formatting a trace log, the handler state was earlier reset after each
    trace file, this is now changed so the handler state is passed not only from
    one trace message to the next in the same file, but also from one file to
    the next.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9430

## Observer 0.9.10

### Fixed Bugs and Malfunctions

- Do not install \*.bat files on non-win32 machines (Thanks to Hans Ulrich
  Niedermann)

  Own Id: OTP-9515

## Observer 0.9.9

### Improvements and New Features

- The time needed for loading a crashump into the crashdump viewer would earlier
  grow exponentially with the size of the crashdump file. Reading a file of 20M
  would take a couple of minutes, and for a dump of 250M it would take between 1
  and 2 hours. This has been solved.

  Earlier, all processes, timers, funs or ets-tables would be loaded into the
  memory of the crashdump viewer node before sending it on to the web server.
  This has been changed and the pages are now sent to the web server in chunks.

  A security function in newer web browsers prevents a full file path to be sent
  from an HTML file input field, i.e. the field needed to implement the "Browse"
  button when loading a file into the crashdump viewer. To overcome this, the
  file input field is no longer used. Instead a normal text input field is used,
  and the user needs to manually insert the complete file path. For convenience,
  a shell script and a batch file are added to the observer application. These
  can be used to start the crashdump_viewer and a browser and load a file - with
  the file name given from the command line. The shell script and batch file are
  called cdv and cdv.bat respectively, and can be found in the priv dir of the
  observer application.

  Own Id: OTP-9051 Aux Id: seq11789

## Observer 0.9.8.4

### Improvements and New Features

- The multitrace.erl installation example file is now installed in the examples
  directory. (Thanks to Peter Lemenkov.)

  Own Id: OTP-8857

## Observer 0.9.8.3

### Improvements and New Features

- The test suite has been updated for R14A.

  Own Id: OTP-8708

## Observer 0.9.8.2

### Improvements and New Features

- Misc updates

  Own Id: OTP-8456

## Observer 0.9.8.1

### Improvements and New Features

- Major improvements of the Erlang distribution for Erlang runtime systems with
  SMP support. Previously distribution port locks were heavily contended, and
  all encoding and decoding for a specific distribution channel had to be done
  in sequence. Lock contention due to the distribution is now negligible and
  both encoding and decoding of Erlang messages can be done in parallel.

  The old atom cache protocol used by the Erlang distribution has been dropped
  since it effectively prevented all parallel encoding and decoding of messages
  passed over the same distribution channel.

  A new atom cache protocol has been introduced which isolates atom cache
  accesses and makes parallel encoding and decoding of messages passed over the
  same distribution channel possible. The new atom cache protocol also use an
  atom cache size 8 times larger than before. The new atom cache protocol is
  documented in the ERTS users guide.

  Erlang messages received via the distribution are now decoded by the receiving
  Erlang processes without holding any distribution channel specific locks.
  Erlang messages and signals sent over the distribution are as before encoded
  by the sending Erlang process, but now without holding any distribution
  channel specific locks during the encoding. That is, both encoding and
  decoding can be and are done in parallel regardless of distribution channel
  used.

  The part that cannot be parallelized is the atom cache updates. Atom cache
  updates are therefore now scheduled on the distribution port. Since it is only
  one entity per distribution channel doing this work there is no lock
  contention due to the atom cache updates.

  The new runtime system does not understand the old atom cache protocol. New
  and old runtime systems can however still communicate, but no atom cache will
  be used.

  Own Id: OTP-7774

## Observer 0.9.8

### Improvements and New Features

- `etop` would crash if the emulator's custom allocators had been turned off
  (e.g. using the `+Meamin` option).

  Own Id: OTP-7519

- The copyright notices have been updated.

  Own Id: OTP-7851

## Observer 0.9.7.4

### Improvements and New Features

- Minor Makefile changes.

  Own Id: OTP-6689

- Obsolete guard tests (such as list()) have been replaced with the modern guard
  tests (such as is_list()).

  Own Id: OTP-6725

## Observer 0.9.7.3

### Improvements and New Features

- This application has been updated to eliminate warnings by Dialyzer.

  Own Id: OTP-6551

## Observer 0.9.7.2

### Fixed Bugs and Malfunctions

- Several minor bugs and race conditions eliminated in the runtime_tools and
  observer applications.

  Own Id: OTP-6265

## Observer 0.9.7.1

### Fixed Bugs and Malfunctions

- Crash dump with large integers could crash the `crashdump_viewer`.

  Own Id: OTP-6301

## Observer 0.9.7 (R11B)

### Fixed Bugs and Malfunctions

- Fixed a bug in `etop` which made the Load and Memory information in the header
  incorrect -- for example the value shown for `binary` (memory allocated for
  binaries) was actually the number of currently running processes. (Thanks to
  Rikard Johansson.)

  Own Id: OTP-6075

## Observer 0.9.6.2

### Fixed Bugs and Malfunctions

- The Observer application has been recompiled because of a compiler bug.

  Own Id: OTP-5700

## Observer 0.9.6.1

### Fixed Bugs and Malfunctions

- `crashdump_viewer` is faster when showing message, dictionary, and stack dump
  for large processes.

  Own Id: OTP-5408
