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
# Tools Release Notes

This document describes the changes made to the Tools application.

## Tools 3.6

### Improvements and New Features

- Map comprehensions as suggested in EEP 58 has now been implemented.

  Own Id: OTP-18413 Aux Id: EEP-58, PR-6727

- The `instrument` module has been moved from `tools` to `runtime_tools`.

  Own Id: OTP-18487 Aux Id: PR-6829

## Tools 3.5.3

### Improvements and New Features

- Removed the previously undocumented and unsupported `emem` tool.

  Own Id: OTP-17892 Aux Id: PR-5591

## Tools 3.5.2

### Fixed Bugs and Malfunctions

- Erlang-mode fixed for newer versions of xref using CL-Lib structures instead
  of EIEIO classes.

  Own Id: OTP-17746 Aux Id: GH-5314, PR-5324

## Tools 3.5.1

### Fixed Bugs and Malfunctions

- The `cover` tool would not work on modules compiled with the `tuple_calls`
  option.

  Own Id: OTP-17440 Aux Id: GH-4796

## Tools 3.5

### Fixed Bugs and Malfunctions

- For cover-compiled code, the error behaviour of list and binary comprehensions
  that used `andalso`/`orelse` in guards could be changed so that a filter that
  was supposed be evaluated in guard context was evaluated in body context. That
  is, there was a possibility that comprehensions that did not raise exceptions
  could raise exceptions when being run using `cover`.

  Own Id: OTP-17221 Aux Id: PR-4547

### Improvements and New Features

- Support for handling abstract code created before OTP R15 has been dropped.

  Own Id: OTP-16678 Aux Id: PR-2627

- Add types and specifications for documentation.

  Own Id: OTP-16957

- The experimental HiPE application has been removed, together with all related
  functionality in other applications.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16963

## Tools 3.4.4

### Fixed Bugs and Malfunctions

- `cover` would crash when compiling a module having an exported function named
  `clauses`.

  Own Id: OTP-17162 Aux Id: GH-4549, PR-2997, PR-4555, elixir-lang/elixir#10666

- If `beam_lib` is asked to return abstract code for a BEAM file produced by
  Elixir and Elixir is not installed on the computer, `beam_lib` will no longer
  crash, but will return an error tuple. The `cover:compile_beam()` and
  `cover:compile_beam_directory()` functions have been updated to also return an
  error tuple in that situation.

  Own Id: OTP-17194 Aux Id: GH-4353

- Make emacs mode work on emacs-27.

  Own Id: OTP-17225 Aux Id: PR-4542, GH-4451

## Tools 3.4.3

### Fixed Bugs and Malfunctions

- Correct the Xref analysis `undefined_functions` to not report internally
  generated behaviour_info/1.

  Own Id: OTP-17191 Aux Id: OTP-16922, ERL-1476, GH-4192

## Tools 3.4.2

### Fixed Bugs and Malfunctions

- Correct the Xref analysis `exports_not_used` to not report internally
  generated `behaviour_info/1`.

  Own Id: OTP-16922 Aux Id: PR-2752

## Tools 3.4.1

### Fixed Bugs and Malfunctions

- Correct the Xref analysis `locals_not_used` to find functions called
  exclusively from `on_load` functions.

  Own Id: OTP-16854 Aux Id: PR-2750

## Tools 3.4

### Improvements and New Features

- Updates for new `erlang:term_to_iovec()` BIF.

  Own Id: OTP-16128 Aux Id: OTP-15618

- Improved the presentation of allocations and carriers in the `instrument`
  module.

  Own Id: OTP-16327

- Minor updates due to the new spawn improvements made.

  Own Id: OTP-16368 Aux Id: OTP-15251

## Tools 3.3.1.1

### Fixed Bugs and Malfunctions

- `cover` would crash when compiling a module having an exported function named
  `clauses`.

  Own Id: OTP-17162 Aux Id: GH-4549, PR-2997, PR-4555, elixir-lang/elixir#10666

## Tools 3.3.1

### Fixed Bugs and Malfunctions

- An Emacs warning due to lacking type in defcustom declaration has been fixed.

  Own Id: OTP-16356

- Improve emacs indentation.

  Own Id: OTP-16472 Aux Id: ERL-1140

- The cover tool could generate instrumented code for a module that would cause
  warnings to be issued.

  Own Id: OTP-16476 Aux Id: ERL-1147

- Fixed generated [fprof analysis format](`m:fprof#analysis`) to also handle
  data in maps.

  Own Id: OTP-16498 Aux Id: ERL-814

## Tools 3.3

### Fixed Bugs and Malfunctions

- Improve `-spec` indentation in emacs mode.

  Own Id: OTP-16164

### Improvements and New Features

- The Emacs erlang-mode function that lets the user open the documentation for
  an Erlang/OTP function in an Emacs buffer has been improved. Bugs in this
  function has been fixed and and the user will now be asked if the man pages
  should be downloaded automatically by Emacs when they can't be found on the
  system. To test this functionality, put the cursor over the function name in a
  call to an Erlang/OTP function (e.g., "io:format("arg")") and type C-c C-d
  (i.e., Ctrl-key and c-key and then Ctrl-key and d-key). There is also a new
  menu item under the Erlang menu (labeled "Man - Function Under Cursor").

  Own Id: OTP-16174

## Tools 3.2.1

### Fixed Bugs and Malfunctions

- `cover` would fail to start if two processes tried to start it at the exact
  same time.

  Own Id: OTP-15813 Aux Id: ERL-943

## Tools 3.2

### Fixed Bugs and Malfunctions

- Add `cprof` and `tags` modules to .app file so that they are included in
  releases.

  Own Id: OTP-15534 Aux Id: PR-2078

- Improved documentation parsing in emacs erldoc functionality.

  Own Id: OTP-15699 Aux Id: PR-2184

### Improvements and New Features

- The `cover` tool now uses the `counters` module instead of `ets` for updating
  the counters for how many times a line has been executed. By default, Cover
  will work with distributed nodes, but a new function `cover:local_only/0`
  allows running the Cover in a restricted but faster local-only mode.

  The increase in speed will vary depending on the type of code being
  cover-compiled, but as an example, the compiler test suite runs more than
  twice as fast with the new Cover.

  Own Id: OTP-15575

## Tools 3.1.0.1

### Fixed Bugs and Malfunctions

- `cover` would fail to start if two processes tried to start it at the exact
  same time.

  Own Id: OTP-15813 Aux Id: ERL-943

## Tools 3.1

### Fixed Bugs and Malfunctions

- Minor fixes for `make clean`.

  Own Id: OTP-15657

### Improvements and New Features

- In the HTML file generated by `cover:analyse_to_file/1,2`, a link is now added
  to the line number. This makes it easier to share pointers to specific lines.

  Own Id: OTP-15541

- Uncovered lines are now marked with a sad face, `:-(`, in the HTML output from
  `cover:analyse_to_file/1,2`. This is to make these lines easier to find by
  search.

  Own Id: OTP-15542

## Tools 3.0.2

### Improvements and New Features

- Remove emacs warnings and added more tests.

  Own Id: OTP-15476

## Tools 3.0.1

### Improvements and New Features

- The HTML pages generated by cover:analyse_to_file/1 and related functions is
  improved for readability.

  Own Id: OTP-15213 Aux Id: PR-1807

- Add alignment functionality in emacs.

  Own Id: OTP-15239 Aux Id: PR-1728

## Tools 3.0

### Improvements and New Features

- Added `instrument:allocations` and `instrument:carriers` for retrieving
  information about memory utilization and fragmentation.

  The old `instrument` interface has been removed, as have the related options
  `+Mim` and `+Mis`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14961

## Tools 2.11.2.2

### Fixed Bugs and Malfunctions

- `cover` would fail to start if two processes tried to start it at the exact
  same time.

  Own Id: OTP-15813 Aux Id: ERL-943

## Tools 2.11.2.1

### Fixed Bugs and Malfunctions

- Minor fixes for `make clean`.

  Own Id: OTP-15657

## Tools 2.11.2

### Fixed Bugs and Malfunctions

- A counting bug is corrected in `Cover`. The bug was introduced in Erlang/OTP
  18.0.

  Own Id: OTP-14817 Aux Id: PR 1641

- The `lcnt` server will no longer crash if `lcnt:information/0` is called
  before `lcnt:collect/0`.

  Own Id: OTP-14912

- `lcnt:collect` will now implicitly start the `lcnt` server, as per the
  documentation.

  Own Id: OTP-14913

### Improvements and New Features

- Improved indentation in emacs and various other updates.

  Own Id: OTP-14944

## Tools 2.11.1

### Fixed Bugs and Malfunctions

- Removed all old unused files in the documentation.

  Own Id: OTP-14475 Aux Id: ERL-409, PR-1493

## Tools 2.11

### Fixed Bugs and Malfunctions

- The predefined Xref analysis `locals_not_used` no longer reports unused
  functions with the `-on_load()` attribute.

  The new predefined Xref variable `OL` holds all functions with the
  `-on_load()` attribute.

  Own Id: OTP-14344

- In fprof when sampling multiple processes and analyzing with totals set to
  true, the output now sums together all caller and callee entries which
  concerns the same function. Previous behaviour was to report each contributing
  entry separately.

  Own Id: OTP-14500

### Improvements and New Features

- Lock counting can now be fully toggled at runtime in the lock counting
  emulator (`-emu_type lcnt`). Everything is enabled by default to match the old
  behavior, but specific categories can be toggled at will with minimal runtime
  overhead when disabled. Refer to the documentation on `lcnt:rt_mask/1` for
  details.

  Own Id: OTP-13170

- `lcnt:collect` and `lcnt:clear` will no longer block all other threads in the
  runtime system.

  Own Id: OTP-14412

- General Unicode improvements.

  Own Id: OTP-14462

- Tools are updated to show Unicode atoms correctly.

  Own Id: OTP-14464

- Add `erlang:iolist_to_iovec/1`, which converts an iolist() to an
  erlang:iovec(), which suitable for use with `enif_inspect_iovec`.

  Own Id: OTP-14520

## Tools 2.10.1

### Fixed Bugs and Malfunctions

- In OTP-20.0, the behavior of c, make, and ct_make was changed so that in some
  cases the beam files by default would be written to the directory where the
  source files were found. This is now changed back to the old behavior so beam
  files are by default written to current directory.

  Own Id: OTP-14489 Aux Id: ERL-438

## Tools 2.10

### Fixed Bugs and Malfunctions

- In some situations, `make:all()` and friends did not detect changes in include
  files located in the current directory. This is now corrected.

  Own Id: OTP-14339 Aux Id: ERL-395

### Improvements and New Features

- The `make` module now accepts the `{emake,Emake}` option.

  Own Id: OTP-14253

- Miscellaneous updates due to atoms containing arbitrary Unicode characters.

  Own Id: OTP-14285

## Tools 2.9.1

### Improvements and New Features

- Improved edoc support in emacs mode.

  Own Id: OTP-14217 Aux Id: PR-1282

## Tools 2.9

### Fixed Bugs and Malfunctions

- Fix unhandled trace event send_to_non_existing_process in fprof.

  Own Id: OTP-13998

### Improvements and New Features

- Improved edoc support in emacs erlang-mode.

  Own Id: OTP-13945 Aux Id: PR-1157

- Added erldoc to emacs mode which opens html documentation in browser from
  emacs. For example `M-x erldoc-browse RET lists:foreach/2`.

  Own Id: OTP-14018 Aux Id: PR-1197

## Tools 2.8.6

### Fixed Bugs and Malfunctions

- Errors in type specification and Emacs template generation for
  `gen_statem:code_change/4` has been fixed from bugs.erlang.org's Jira cases
  ERL-172 and ERL-187.

  Own Id: OTP-13746 Aux Id: ERL-172, ERL-187

- Fix gc_start/gc_end in fprof tags when parsing old trace logs.

  Own Id: OTP-13778 Aux Id: PR-1136

- `make` (tools) and `ct_make` (common_test) would crash if an Erlang source
  file contained a `-warning()` directive.

  Own Id: OTP-13855

## Tools 2.8.5

### Fixed Bugs and Malfunctions

- Correct a bug when adding multiple modules to an Xref server. The bug was
  introduced in OTP-19.0.

  Own Id: OTP-13708 Aux Id: ERL-173

## Tools 2.8.4

### Fixed Bugs and Malfunctions

- Update fprof to use the new 'spawned' trace event to determine when a process
  has been created.

  Own Id: OTP-13499

### Improvements and New Features

- Optimize adding multiple modules to an Xref server.

  Own Id: OTP-13593

- Various emacs mode improvements, such as better tags support.

  Own Id: OTP-13610

## Tools 2.8.3

### Fixed Bugs and Malfunctions

- `cover:compile_beam/1` and `cover:compile_beam_directory/1,2` crashed when
  trying to compile a beam file without a `'file'` attribute. This has been
  corrected and an error is returned instead.

  Thanks to Louis-Philippe Gauthier for reporting this bug.

  Own Id: OTP-13200

- Fix a bit string comprehension bug in Cover.

  Own Id: OTP-13277 Aux Id: PR 856

## Tools 2.8.2

### Fixed Bugs and Malfunctions

- The emacs mode does not add a newline after the arrow on -callback lines
  anymore.

  Own Id: OTP-13042

## Tools 2.8.1

### Fixed Bugs and Malfunctions

- If a module includes eunit.hrl, a parse transform adds the function test/0 on
  line 0 in the module. A bug in OTP-18.0 caused cover:analyse_to_file/1 to fail
  to insert cover data in the output file when line 0 existed in the cover data
  table. This is now corrected.

  Own Id: OTP-12981

## Tools 2.8

### Fixed Bugs and Malfunctions

- In order to improve performance of the cover tool, new functions are added for
  cover compilation and analysis on multiple files. This allows for more
  parallelisation.

  Some improvements of the data base access is also done in order to improve the
  performance when analysing and resetting cover data.

  Minor incompatibility: An error reason from analyse_to_file is changed from
  no_source_code_found to \{no_source_code_found,Module\}.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12330 Aux Id: seq12757

- Attempting to do a `cover` analysis when neither source code nor beam file
  could be found would hang the `cover` server. Corrected to return a proper
  error.

  Own Id: OTP-12806

### Improvements and New Features

- Allow maps for supervisor flags and child specs

  Earlier, supervisor flags and child specs were given as tuples. While this is
  kept for backwards compatibility, it is now also allowed to give these
  parameters as maps, see [sup_flags](`m:supervisor#sup_flags`) and
  [child_spec](`m:supervisor#child_spec`).

  Own Id: OTP-11043

- Remove Mnemosyne rules support.

  Own Id: OTP-12511

- Add printout of total number of calls and time in eprof

  Own Id: OTP-12681

## Tools 2.7.2

### Fixed Bugs and Malfunctions

- Fix lcnt sorting and printout of histograms.

  Own Id: OTP-12364

- Fix a Unicode bug in the `tags` module.

  Own Id: OTP-12567

- Fix tags completion in erlang.el for GNU Emacs 23+

  Own Id: OTP-12583

## Tools 2.7.1

### Fixed Bugs and Malfunctions

- Fixed a typo in erlang-mode comment.

  Own Id: OTP-12214

- Add a skeleton for -spec in Erlang mode for Emacs

  Own Id: OTP-12283

### Improvements and New Features

- Cover no longer crashes when compiling `receive` and the like with just an
  `after` clause. Thanks to José Valim for providing a fix.

  Own Id: OTP-12328

## Tools 2.7

### Improvements and New Features

- Add log2 histogram to lcnt for lock wait time

  Own Id: OTP-12059

## Tools 2.6.15

### Fixed Bugs and Malfunctions

- Removed `erlang:bitstr_to_list/1` and `erlang:list_to_bitstr/1`. They were
  added by mistake, and have always raised an `undefined` exception when called.

  Own Id: OTP-11942

## Tools 2.6.14

### Fixed Bugs and Malfunctions

- Removed the support for the query keyword from emacs mode (Thanks to Paul
  Oliver)

  Own Id: OTP-11568

- Emacs mode improvements (Thanks to Steve Vinoski)

  Own Id: OTP-11601

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

- The emacs erlang mode now match erlang keywords more carefully (Thanks to
  Steve Vinoski)

  Own Id: OTP-11786

- The emacs erlang-mode now auto loads for more file types (Thanks to Phil
  Hagelberg)

  Own Id: OTP-11788

### Improvements and New Features

- `cover` can run on itself. Also, support for reading BEAM files produced by
  ancient OTP versions before R9C has been removed.

  Own Id: OTP-11692

- Support maps in cover

  Own Id: OTP-11764

## Tools 2.6.13

### Fixed Bugs and Malfunctions

- Erlang-specific compilation error regexp is added in erlang-eunit.el. This
  defvar was earlier in erlang.el, but was erroneously removed in R15B02, while
  still used by erlang-eunit.el.

  Own Id: OTP-11417 Aux Id: seq12447

- Take compiler options from beam in cover:compile_beam. Thanks to Péter Gömöri.

  Own Id: OTP-11439

- Silence warnings (Thanks to Anthony Ramine)

  Own Id: OTP-11517

### Improvements and New Features

- Add iodata, nonempty_string to built-in type highlighting for emacs. Thanks to
  Paul Oliver.

  Own Id: OTP-11394

## Tools 2.6.12

### Improvements and New Features

- Remove trailing spaces in Emacs templates. Thanks to Roberto Aloi.

  Own Id: OTP-11198

- Fixed the Emacs erlang-mode to accommodate the coding style where lists
  written across several lines have each line starting with a comma. Thanks to
  Magnus Henoch.

  Own Id: OTP-11242

- Make the Emacs Erlang mode TRAMP-aware when compiling. Thanks to Tomas
  Abrahamsson.

  Own Id: OTP-11270

## Tools 2.6.11

### Fixed Bugs and Malfunctions

- When cover:stop(Node) was called on a non-existing node, a process waiting for
  cover data from the node would hang forever. This has been corrected.

  Own Id: OTP-10979

### Improvements and New Features

- Make cover smarter about finding source from beam.

  In particular, search using the source path in module_info if the current
  heuristic fails.

  Own Id: OTP-10902

- Remove Flymake dependency in erlang-pkg.el. Thanks to Magnus Henoch.

  Own Id: OTP-10930

- Erlang-mode: Add autoload cookies for file extension associations. Thanks to
  Magnus Henoch.

  Own Id: OTP-10999

- Postscript files no longer needed for the generation of PDF files have been
  removed.

  Own Id: OTP-11016

- Fix a race condition when there're several applications in apps directory.
  Thanks to Manuel Rubio.

  Own Id: OTP-11028

- New option for eprof, 'set_on_spawn'. This option was previously always on and
  is also the default.

  Own Id: OTP-11144

## Tools 2.6.10

### Improvements and New Features

- Fix a bug in cover when used with no_auto_import. Thanks to José Valim.

  Own Id: OTP-10778

## Tools 2.6.9

### Fixed Bugs and Malfunctions

- Add missing modules in app-file

  Own Id: OTP-10439

- Make erlang-mode more compatible with package.el (Thanks to Gleb Peregud)

  Own Id: OTP-10465

- Fix various typos (thanks to Tuncer Ayaz)

  Own Id: OTP-10611

- Add separate face for exported functions (Thanks to Thomas Järvstrand)

  Own Id: OTP-10637

- The BIF highlighting in the emacs mode has been updated to correspond with the
  correct BIFs.

  Own Id: OTP-10774

### Improvements and New Features

- Support for Unicode has been implemented.

  Own Id: OTP-10302

- A new function, cover:flush(Nodes), is added which will fetch data from remote
  nodes without stopping cover on those nodes. This is used by test_server and
  common_test when it is safe to assume that the node will be terminated after
  the test anyway. The purpose is to avoid processes crashing when re-loading
  the original beam if the processes is still running old code.

  Remote nodes will now continue to count code coverage if the connection to the
  main node is broken. Earlier, a broken connection would cause the cover_server
  on the remote node to die and thus any still cover compiled modules would
  cause process crash when trying to insert cover data in ets tables that used
  to exist on the cover_server. The new functionality also involves
  synchronization with the main node if the nodes are reconnected.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-10523 Aux Id: OTP-10427

- Where necessary a comment stating encoding has been added to Erlang files. The
  comment is meant to be removed in Erlang/OTP R17B when UTF-8 becomes the
  default encoding.

  Own Id: OTP-10630

- Fix syntax highlighting of $\\' in Emacs mode. Thanks to Magnus Henoch.

  Own Id: OTP-10766

## Tools 2.6.8

### Fixed Bugs and Malfunctions

- The last tuple fun call has been removed from fprof.

  Own Id: OTP-10091 Aux Id: seq12067

- Fix indentation of record fields in Emacs (Thanks to Tomas Abrahamsson)

  Own Id: OTP-10120

- Documentation fixes (Thanks to Ricardo Catalinas Jiménez )

  Own Id: OTP-10121

- Remove Erlang-specific compilation error regexp in erlang.el

  Own Id: OTP-10168

- Fix highlighting of atoms ending with a dollar sign

  Like this: 'atom$'. In that example, the last single quote should be
  recognised as ending the atom. This needs a font-lock workaround similar to
  the one for strings. Thanks to Magnus Henoch

  Own Id: OTP-10178

- Xref now accepts filenames with character codes greater than 126. (Thanks to
  Emile Joubert for reporting the issue.)

  Own Id: OTP-10192

- Add test_indentation target to lib/tools/emacs/Makefile

  Automatically indent test.erl.orig, save to test.erl, and compare to
  test.erl.intended. Thanks to Magnus Henoch.

  Own Id: OTP-10226

## Tools 2.6.7

### Fixed Bugs and Malfunctions

- Makefiles in erts, hipe and tools have been corrected to enable parallel make,
  i.e MAKEFLAGS=-jX where X is the parallelity number. As a result of this
  dependencies were corrected since that is what is needed for parallel make to
  work.

  Own Id: OTP-9857 Aux Id: OTP-9451

- Minor suppressions and fixes of compilation warnings

  Own Id: OTP-10016

## Tools 2.6.6.6

### Fixed Bugs and Malfunctions

- Update system profiling principles to reflect eprof performance improvements.

  Own Id: OTP-9656

- \[cover] fix leftover \{'DOWN', ..\} msg in callers queue

  After stopping cover with cover:stop() there could still be a \{'DOWN',...\}
  leftover message in the calling process's message queue. This unexpected
  leftover could be eliminated if erlang:demonitor/2 with option flush would be
  used in certain points

  Own Id: OTP-9694

- Add deps as erlang-flymake include directory.

  Update erlang-flymake to recognize the "deps" folder as an include directory.
  This makes erlang-flymake compatible with the rebar dependency management
  tool's default folder structure, which puts included dependencies in
  "deps".(Thanks to Kevin Albrecht)

  Own Id: OTP-9791

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

- Tuple funs (a two-element tuple with a module name and a function) are now
  officially deprecated and will be removed in R16. Use '`fun M:F/A`' instead.
  To make you aware that your system uses tuple funs, the very first time a
  tuple fun is applied, a warning will be sent to the error logger.

  Own Id: OTP-9649

- Eliminate use of deprecated regexp module

  Own Id: OTP-9810

## Tools 2.6.6.5

### Fixed Bugs and Malfunctions

- Teach the emacs mode to compile yecc and leex files

  If visiting a .yrl or .xrl file in emacs with erlang-mode, then the
  \`erlang-compile' function (normally bound to C-c C-k), now knows how to
  compile yecc and leex files, and then, if that compilation succeeds, also
  compiles the resulting .erl files.

  Also introduce a \`erlang-compile-command-function-alist' to make it possible
  to hook in other functions for computing compilation commands/expressions,
  depending on file name. (Thanks to Tomas Abrahamsson )

  Own Id: OTP-9503

### Improvements and New Features

- Bugs in xref(3) have been fixed. (Thanks to Matthias Lang.)

  Own Id: OTP-9416

## Tools 2.6.6.4

### Fixed Bugs and Malfunctions

- Change make:files to behave more like erlc

  This change removes the unnecessary checks on the files when make:files is
  called and allows the error checking to be done in compile:file, where the
  error messages are produced. It does not affect the return value.

  (Thanks to Sam bobroff)

  Own Id: OTP-9179

- add user specified compiler options on form reloading

  In order to be able to test non-exported functions from another (test) module
  it is necessary to compile the specific module (at least during the test
  phase) with the export*all compiler option. This allows complete separation of
  testing and productive code. At the moment it is not possible to combine this
  with a test code coverage using the cover module. The problem is that when
  cover compiling a module using cover:compile*\* the code is reloaded into the
  emulator omitting/filtering the passed user options. In my example above the
  export_all option would be removed and the non-exported functions cannot be
  called any more. (Thanks to Tobias Schlager)

  Own Id: OTP-9204

- Inhibit electric newline after "->" when inside a type spec

  The Erlang mode for Emacs inserts a newline after every "->", which saves you
  one keystroke when writing a function, but that is inappropriate when writing
  a type spec, as you'd normally keep the spec on one line. This change inhibits
  the automatic insertion when the current line starts with "-spec" or
  "-type".(Thanks to Magnus Henoch)

  Own Id: OTP-9255

- Add a check logic to prevent file descriptor leak

  cover module handle files as raw in export and import. Assert counts of ports
  are the same at the beginning and at the end of the test case.(Thanks to
  Shunichi Shinohara)

  Own Id: OTP-9300

## Tools 2.6.6.3

### Fixed Bugs and Malfunctions

- Declare indentation options as "safe" in erlang-mode for Emacs

  Emacs has a facility for setting options on a per-file basis based on comments
  in the source file. By default, all options are considered "unsafe", and the
  user is queried before the variable is set. This patch declares the variables
  erlang-indent-level, erlang-indent-guard and erlang-argument-indent to be
  safe, if the value specified in the source file is valid.

  Such declarations usually look like this:

  %% -_- erlang-indent-level: 2 -_-

  and appear on the first line of the file. (thanks to Magnus Henoch)

  Own Id: OTP-9122

### Improvements and New Features

- Cover has been improved to take less memory and allow parallel analysis of
  cover data. Data collection from nodes is now done in parallel and it is now
  possible to issue multiple analyse and analyse_to_file requests at the same
  time. A new function call async_analyse_to_file has also been introduced, see
  the documentation for more details.

  Own Id: OTP-9043 Aux Id: seq11771

## Tools 2.6.6.2

### Fixed Bugs and Malfunctions

- eprof: API sort mismatch has now been fixed.

  Own Id: OTP-8853

- eprof: fix division by zero in statistics

  Own Id: OTP-8963

## Tools 2.6.6.1

### Fixed Bugs and Malfunctions

- `cover` will now show ampersand characters in the source code correctly.
  (Thanks to Tom Moertel.)

  Own Id: OTP-8776

## Tools 2.6.6

### Fixed Bugs and Malfunctions

- A race condition affecting Cover has been removed.

  Own Id: OTP-8469

- Emacs improvements:

  Fixed emacs-mode installation problems.

  Fixed a couple of -spec and -type indentation and font-lock problems.

  Fixed error messages on emacs-21.

  Magnus Henoch fixed several issues.

  Ralf Doering, Klas Johansson and Chris Bernard contributed various emacs-eunit
  improvements.

  Klas Johansson and Dave Peticolas added emacs-flymake support.

  Own Id: OTP-8530

### Improvements and New Features

- Xref has been updated to use the `re` module instead of the deprecated
  `regexp` module.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8472

- When given the option `{builtins,true}` Xref now adds calls to operators.

  Own Id: OTP-8647

- `eprof` has been reimplemented with support in the Erlang virtual machine and
  is now both faster (i.e. slows down the code being measured less) and scales
  much better. In measurements we saw speed-ups compared to the old eprof
  ranging from 6 times (for sequential code that only uses one scheduler/core)
  up to 84 times (for parallel code that uses 8 cores).

  Note: The API for the `eprof` has been cleaned up and extended. See the
  documentation.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8706

## Tools 2.6.5.1

### Fixed Bugs and Malfunctions

- A bug concerning bit comprehensions has been fixed in Cover. The bug was
  introduced in R13B03. (Thanks to Matthew Sackman.)

  Own Id: OTP-8340

### Improvements and New Features

- Add lock profiling tool.

  The Lock profiling tool, lcnt, can make use of the internal lock statistics
  when the runtime system is built with this feature enabled.

  This provides a mechanism to examine potential lock bottlenecks within the
  runtime itself.

  \- Add erts_debug:lock_counters(\{copy_save, bool()\}). This option enables or
  disables statistics saving for destroyed processes and ets-tables. Enabling
  this might consume a lot of memory.

  \- Add id-numbering for lock classes which is otherwise undefined.

  Own Id: OTP-8424

- emacs: Moved code skeletons to a separate file and and added a configurable
  variable to choose skeleton. Thanks Dave Peticolas.

  Own Id: OTP-8446

## Tools 2.6.5

### Fixed Bugs and Malfunctions

- The coverage analysis tool `cover` has been improved when it comes to handling
  list and bit string comprehensions (a counter for each qualifier), bit syntax
  expressions (the Value and Size expressions), and try expressions (the body
  called `Exprs` in the Reference Manual). A few (not all) situations where
  several expressions are put on the same line are also handled better than
  before.

  Own Id: OTP-8188 Aux Id: seq11397

- When loading Cover compiled code on remote nodes running code in the loaded
  module, a `badarg` failure was sometimes the result. This bug has been fixed.

  Own Id: OTP-8270 Aux Id: seq11423

- The short-circuit operators `andalso` and `orelse` are now handled correctly
  by the coverage analysis tool `cover` (it is no longer checked that the second
  argument returns a Boolean value.)

  Own Id: OTP-8273

## Tools 2.6.4

### Fixed Bugs and Malfunctions

- `cover` now properly escapes greater-than and less-than characters in comments
  in HTML reports. (Thanks to Magnus Henoch.)

  Own Id: OTP-7939

## Tools 2.6.3

### Improvements and New Features

- xref:start/1 does now allow anonymous XREF processes to be started

  Own Id: OTP-7831

## Tools 2.6.2

### Fixed Bugs and Malfunctions

- A bug in the Xref scanner has been fixed.

  Own Id: OTP-7423

- A bug in Fprof where the function 'undefined' appeared to call 'undefined' has
  been corrected.

  Own Id: OTP-7509

## Tools 2.6.1

### Improvements and New Features

- The documentation has been updated so as to reflect the last updates of the
  Erlang shell as well as the minor modifications of the control sequence `p` of
  the `io_lib` module.

  Superfluous empty lines have been removed from code examples and from Erlang
  shell examples.

  Own Id: OTP-6944 Aux Id: OTP-6554, OTP-6911

- [`tuple_size/1`](`tuple_size/1`) and [`byte_size/1`](`byte_size/1`) have been
  substituted for [`size/1`](`size/1`).

  Own Id: OTP-7009

- The coverage analysis tool `cover` now handles the short-circuit Boolean
  expressions `andalso/2` and `orelse/2` properly.

  Own Id: OTP-7095

## Tools 2.6

### Fixed Bugs and Malfunctions

- The `cover` tool could use huge amounts of memory when used in a distributed
  system.

  Own Id: OTP-6758

## Tools 2.5.5

### Fixed Bugs and Malfunctions

- Missing buffer-local declaration in erlang.el has been added. Before this fix
  there could arise problems in other emacs modes after visiting a buffer using
  the erlang mode.

  Own Id: OTP-6721

- Key-map for 'backward-delete-char-untabif updated to work properly with
  Xemacs.

  Own Id: OTP-6723

### Improvements and New Features

- Minor updates of Xref.

  Own Id: OTP-6586

- Minor Makefile changes.

  Own Id: OTP-6689 Aux Id: OTP-6742

- "C-u C-c C-k" now does a compile with both "debug_info" and "export_all".

  Own Id: OTP-6741

## Tools 2.5.4.1

### Improvements and New Features

- Changes due to internal interface changes in the erts application which are
  needed at compile-time. No functionality has been changed.

  Own Id: OTP-6611 Aux Id: OTP-6580

## Tools 2.5.4

### Fixed Bugs and Malfunctions

- Made change to support the function erlang-find-tag for xemacs and emacs-21.

  Own Id: OTP-6512

### Improvements and New Features

- Minor updates of xref for future compatibility.

  Own Id: OTP-6513

## Tools 2.5.3

### Fixed Bugs and Malfunctions

- `eprof` did not work reliably in the SMP emulator, because the trace receiver
  process could not process the trace messages fast enough. Therefore, `eprof`
  now blocks the other schedulers while profiling.

  Own Id: OTP-6373

## Tools 2.5.2

### Fixed Bugs and Malfunctions

- Fprof traces could become truncated for the SMP emulator. This bug has now
  been corrected.

  Own Id: OTP-6246

## Tools 2.5.1

### Fixed Bugs and Malfunctions

- eprof now works somewhat better in the SMP emulator.

  Own Id: OTP-6152

## Tools 2.5

### Fixed Bugs and Malfunctions

- Fixed some bugs in `make`:

  `make:files/1,2` can now handle a file in another directory as argument,
  similar to `make:all/0,1`.

  When specifying a file name including the `.erl` extension in `Emakefile`,
  `make:all/0,1` looked for the object code in the wrong place.

  When specifying a file name including the `.erl` extension in `Emakefile` and
  some compile options for the file, `make:files/0,1` did not use the options as
  it should do.

  Own Id: OTP-6057 Aux Id: seq10299

- `cover`: When `cover:stop()` was called, the cover compiled code was not
  unloaded (as stated in the documentation) but simply marked as 'old'. This
  meant that processes lingering in (or with funs referencing to) the cover
  compiled code would survive even when the cover server and its ETS tables was
  terminated.

  Now the cover compiled code is unloaded, meaning that processes lingering
  in/with references to it will be killed when `cover:stop` is called, instead
  of later crashing with `badarg` when trying to bump counters in ETS tables no
  longer existing.

### Improvements and New Features

- Replaced call to deprecated function `file:file_info/1` with call to
  `filelib:is_dir/1` and `filelib:is_regular/1` in `tags.erl`.

  Own Id: OTP-6079

## Tools 2.4.7

### Fixed Bugs and Malfunctions

- A bug in `fprof` profiling causing erroneous inconsistent trace failure has
  been corrected.

  Own Id: OTP-5922 Aux Id: seq10203

## Tools 2.4.6

### Fixed Bugs and Malfunctions

- Emacs: `erlang-man-function` and `erlang-man-module` used a pattern matching
  to find the requested module that sometimes yielded unexpected results. For
  example, `erlang-man-module file` would display the man page for
  `CosFileTransfer_File`.

  Own Id: OTP-5746 Aux Id: seq10096

- Some compiler warnings and Dialyzer warnings were eliminated in the Tools
  application.

  When tracing to a port (which `fprof` does), there could be fake schedule
  out/schedule in messages sent for a process that had exited.

  Own Id: OTP-5757

## Tools 2.4.5

### Fixed Bugs and Malfunctions

- The cross reference tool `xref` did not handle the new `fun M:F/A` construct
  properly. This problem has been fixed.

  Own Id: OTP-5653

## Tools 2.4.4

### Fixed Bugs and Malfunctions

- The `cover` tool did not escape '<' and '>' not being part of HTML tags in
  HTML log files.

  Own Id: OTP-5588

## Tools 2.4.3

### Improvements and New Features

- It is now possible to encrypt the debug information in beam files, to help
  keep the source code secret. See `m:compile` for how to provide the key for
  encrypting, and `m:beam_lib` for how to provide the key for decryption so that
  tools such as Debugger, `xref`, or `cover` can be used.

  The `beam_lib:chunks/2` functions now accepts an additional chunk type
  '`compile_info`' to retrieve the compilation information directly as a term.
  (Thanks to Tobias Lindahl.)

  Own Id: OTP-5460 Aux Id: seq9787

## Tools 2.4.2

### Fixed Bugs and Malfunctions

- The `cover` tool could not analyze empty modules on module level.

  Own Id: OTP-5418

## Tools 2.4.1

### Fixed Bugs and Malfunctions

- The `xref` analysis `locals_not_used` could return too many functions. This
  problem has been fixed.

  Own Id: OTP-5071

- The `cover` tool could not always compile parse transformed modules. This
  problem has been fixed.

  Own Id: OTP-5305
