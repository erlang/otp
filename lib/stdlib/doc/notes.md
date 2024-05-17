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
# STDLIB Release Notes

This document describes the changes made to the STDLIB application.

## STDLIB 6.0

### Fixed Bugs and Malfunctions

- The specs in module `m:binary` has been updated to reflect what is allowed by the documentation.

  Own Id: OTP-18684 Aux Id: [PR-7481]

- Several functions in the `m:binary` module would accept arguments of the wrong type under certain circumstances. In this release, they now raise an exception when incorrect types are given.
  
  The following functions would accept an invalid pattern if the subject binary was empty or if the `{scope,{0,0}}` option was given:
  [`binary:match/2,3`](`binary:match/3`),
  [`binary:matches/2,3`](`binary:matches/2`),
  [`binary:replace/3,4`](`binary:replace/3`), and
  [`binary:split/2,3`](`binary:split/2`)
  
  The call `binary:copy(<<1:1>>, 0)` would return an empty binary instead of raising an exception. Similarly, calls to [`binary:part/2,3`](`binary:part/2`) attempting to extract 0 bytes at position 0 of a bitstring would return an empty binary instead of raising an exception.

  Own Id: OTP-18743 Aux Id: [PR-7607], [PR-7628]

- The documentation for the preprocessor now mentions that `defined(Name)` can be called in the condition for an `-if` or `-elif` directive to test whether `Name` is the name of a defined macro. (This feature was implemented in OTP 21.)
  
  If a function call in an `-if` or `-elif` with a name that is not the name of a guard BIF, there would not be a compilation error, but would instead cause the lines following the directive to be skipped. This has now been changed to be a compilation error.

  *** POTENTIAL INCOMPATIBILITY ***

  Own Id: OTP-18784 Aux Id: [GH-7706], [PR-7726]

- `get_until` requests using the I/O protocol now correctly return a binary or list when `eof` is the last item returned by the callback.

  Own Id: OTP-18930 Aux Id: [PR-7993], [GH-4992]

- The error handling the `simple_one_for_one` supervisor has been enhanced. A transient child returning `ignore` will no longer cause a crash.
  
  Also, automatic shutdown has been disabled because it does not make sense for this supervisor type. That is was allowed is considered a bug. Therefore, we don't consider this an incompatible change.

  *** POTENTIAL INCOMPATIBILITY ***

  Own Id: OTP-19029 Aux Id: [PR-8230]

- Fix shell expansion to not crash when expanding a map with non-atom keys and to not list zero arity functions when an argument has been given.

  Own Id: OTP-19073 Aux Id: [PR-8375] [GH-8366] [GH-8365] [GH-8364]

[PR-7481]: https://github.com/erlang/otp/pull/7481
[PR-7607]: https://github.com/erlang/otp/pull/7607
[PR-7628]: https://github.com/erlang/otp/pull/7628
[GH-7706]: https://github.com/erlang/otp/issues/7706
[PR-7726]: https://github.com/erlang/otp/pull/7726
[PR-7993]: https://github.com/erlang/otp/pull/7993
[GH-4992]: https://github.com/erlang/otp/issues/4992
[PR-8230]: https://github.com/erlang/otp/pull/8230
[PR-8375]: https://github.com/erlang/otp/pull/8375
[GH-8366]: https://github.com/erlang/otp/issues/8366
[GH-8365]: https://github.com/erlang/otp/issues/8365
[GH-8364]: https://github.com/erlang/otp/issues/8364

### Improvements and New Features

- The functions [`is_equal/2`](`sets:is_equal/2`), [`map/2`](`sets:map/2`), and [`filtermap/2`](`sets:filtermap/2`) have been added to the modules `m:sets`, `m:ordsets`, and `m:gb_sets`.

  Own Id: OTP-18622 Aux Id: [PR-7183], [PR-7232]

- The compiler now emits nicer error message for function head mismatches.
  For example, given:
  
  ```erlang
  a() -> ok;
  a(_) -> error.
  ```
  
  Erlang/OTP 26 and earlier would emit a diagnostic similar to:
  
  ```text
  t.erl:6:1: head mismatch
  %    6| a(_) -> error.
  %     | ^
  ```
  
  while in Erlang/OTP 27 the diagnostic is similar to:
  
  ```text
  t.erl:6:1: head mismatch: function a with arities 0 and 1 is regarded as two distinct functions. Is the number of arguments incorrect or is the semicolon in a/0 unwanted?
  %    6| a(_) -> error.
  %     | ^
  ```

  Own Id: OTP-18648 Aux Id: [PR-7383]

- [`zip:create/2,3`](`zip:create/2`) will now tolerate POSIX timestamps in the provided `file_info` records.

  Own Id: OTP-18668

- The callback function `c:gen_statem:handle_event/4` has been cached in the `gen_statem` engine to optimize callback call speed.

  Own Id: OTP-18671 Aux Id: [PR-7419]

- The type `beam_lib:beam/0` is now exported.

  Own Id: OTP-18716 Aux Id: [PR-7534]

- The documentation for the `m:binary` module has been improved.

  Own Id: OTP-18741 Aux Id: [PR-7585]

- [`binary:replace/3,4`](`binary:replace/3`) now supports using a fun for supplying the replacement binary.

  Own Id: OTP-18742 Aux Id: [PR-7590]

- Triple-Quoted Strings has been implemented as per [EEP 64](https://www.erlang.org/eeps/eep-0064). See [String](`e:system:data_types.md#string`) in the Reference Manual.
  
  Example:
  
  ```erlang
  1> """
     a
     b
     c
     """.
  "a\nb\nc"
  ```
  
  Adjacent string literals without intervening white space is now a syntax error, to avoid possible confusion with triple-quoted strings. For example:
  
  ```erlang
  1> "abc""xyz".
  "xyz".
  * 1:6: adjacent string literals without intervening white space
  ```

  *** POTENTIAL INCOMPATIBILITY ***

  Own Id: OTP-18750 Aux Id: OTP-18746, [PR-7313], [PR-7451]

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

- `-callback` attributes has been added to modules `m:sys` and `m:erl_error`.

  Own Id: OTP-18793 Aux Id: [PR-7703]

- Several new functions that accept funs have been added to module `m:timer`.
  
  Functions [`apply_after/2`](`timer:apply_after/2`), [`apply_interval/2`](`timer:apply_interval/2`), and [`apply_repeatedly/2`](`apply_repeatedly/2`) accept a nullary fun as the second argument, while  functions [`apply_after/3`](`timer:apply_after/3`), [`apply_interval/3`](`timer:apply_interval/3`), and [`apply_repeatedly/3`](`apply_repeatedly/3`) accept an n-ary fun as the second and a list of n arguments for the fun as the third argument.

  Own Id: OTP-18808 Aux Id: [PR-7649]

- Sigils on string literals have been implemented as per [EEP 66](https://www.erlang.org/eeps/eep-0066), that is: binary and string sigils in verbatim and escape characters variants, as well as a default (vanilla) Sigil.  All for ordinary strings and for triple-quoted strings (EEP 64). See [Sigils in the Reference Manual](`e:system:data_types.md#sigil`).
  
  Examples:
  
  ```erlang
  1> ~"Björn".
  <<"Björn"/utf8>>
  2> ~b"Björn".
  <<"Björn"/utf8>>
  3> ~S"\s*(\w+)".
  "\\s*(\\w+)"
  4> ~B"\s*(\w+)".
  <<"\\s*(\\w+)">>
  ```

  Own Id: OTP-18825 Aux Id: OTP-18750, [PR-7684]

- Functions `shell:default_multiline_prompt/1`, `shell:inverted_space_prompt/1`, and 
  `shell:prompt_width/1` have been exported to help with custom prompt implementations.

  Own Id: OTP-18834 Aux Id: [PR-7675] [PR-7816]

- The shell now pages long output from the documentation help command ([`h(Module)`](`c:h/1`)), auto completions and the search command.

  Own Id: OTP-18846 Aux Id: [PR-7845]

- The `M-h` hotkey (Alt/Option-h) now outputs help for the module or function directly before the cursor.

  Own Id: OTP-18847 Aux Id: [PR-7846]

- Added support for adding a custom code formatter that formats your multi-line shell commands in your preferred formatting on submission. See `shell:format_shell_func/` and `shell:erl_pp_format_func/1`.

  Own Id: OTP-18848 Aux Id: [PR-7847]

- Added shell functions for viewing, forgetting and saving locally defined functions, types and records.

  Own Id: OTP-18852 Aux Id: [PR-7844]

- Added `string:jaro_similarity/2`, which can be used to calculate the similarity between two strings.

  Own Id: OTP-18865 Aux Id: [PR-7879]

- The new function `ets:update_element/4` is similar to `ets:update_element/3`, but takes a default tuple as the fourth argument, which will be inserted if no previous record with that key exists.

  Own Id: OTP-18870 Aux Id: [PR-7857]

- Added functions to retrieve the next higher or lower key/element from `m:gb_trees` and `m:gb_sets`, as well as returning iterators that start at given keys/elements.

  Own Id: OTP-18874 Aux Id: [PR-7745]

- When the shell built-in function [`c/1,2`][c12] is used to re-compile a module, the current working directory of the original compilation is now added to the include path.
  
  [c12]: `\c:c/1`

  Own Id: OTP-18908 Aux Id: [PR-7957]

- The `timer` module now uses a private table for its internal state, slightly improving its performance.

  Own Id: OTP-18914 Aux Id: [PR-7973]

- [EEP-59 - Documentation Attributes](https://www.erlang.org/eeps/eep-0059) has been implemented.
  
  Documentation attributes can be used to document functions, types, callbacks, and modules.
  The keyword `-moduledoc "Documentation here".` is used to document modules, while `-doc "Documentation here".` can be used on top of functions, types, and callbacks to document them, respectively.
  
  * Types, callbacks, and function documentation can be set to `hidden` either via `-doc false` or `-doc hidden`. When documentation attributes mark a type as hidden, they will not be part of the documentation.
  
  * The documentation from `moduledoc` and `doc` gets added by default to the binary beam file, following the format of [EEP-48](https://www.erlang.org/eeps/eep-0048).
  
  * Using the compiler flag `warn_missing_doc` will raise a warning when
  `-doc` attributes are missing in exported functions, types, and callbacks.
  
  * Using the compiler flag `warn_missing_spec_documented` will raise a warning when
  spec attributes are missing in documented functions, types, and callbacks.
  
  * `moduledoc`s and `doc`s may refer to external files to be embedded, such as `-doc {file, "README.md"}.`, which refers to the file `README.md` found in the current working directory.
  
  * The compiler warns about exported functions whose specs refer to hidden types. Thus, there will be warnings when a hidden type (meaning, the type is not part of the documentation) gets used in an exported function.

  Own Id: OTP-18916 Aux Id: [PR-7936]

- New `m:ets` functions `ets:first_lookup/1`, `ets:next_lookup/2`, `ets:prev_lookup/2` and `ets:last_lookup/1`. Example: `ets:next_lookup/1` is equivalent to `ets:next/2` followed by `ets:lookup/2` with the next key. The new combined functions are more efficient and with guaranteed atomicity.

  Own Id: OTP-18923 Aux Id: [PR-6791]

- The `maybe` expression is now enabled by default.
  
  To use `maybe` as an atom, it needs to be single-quoted. Alternatively, the `maybe` expression can be disabled by disabling the `maybe_expr` feature. That can be done by placing the following the line at the beginning of an Erlang source file:
  
  ```erlang
  -feature(maybe_expr, disable).
  ```
  
  Another way to disable the `maybe_expr` feature is by passing the `-disable-feature` option to `erlc`:
  
  ```text
  erlc -disable-feature maybe_expr some_file.erl
  ```

  Own Id: OTP-18944 Aux Id: [PR-8067]

- The compiler will now raise a warning when updating record/map literals. As an example, consider this module:
  
  ```erlang
  -module(t).
  -export([f/0]).
  -record(r, {a,b,c}).
  
  f() ->
      #r{a=1}#r{b=2}.
  ```
  
  The compiler raises the following warning:
  
  ```text
  1> c(t).
  t.erl:6:12: Warning: expression updates a literal
  %    6|     #r{a=1}#r{b=2}.
  %     |            ^
  ```

  Own Id: OTP-18951 Aux Id: [PR-8069]

- The documentation has been migrated to use Markdown and ExDoc.

  Own Id: OTP-18955 Aux Id: [PR-8026]

- Optimized `ets:foldl` and `ets:foldr` to use new `ets:next_lookup`. Also made them immune against table renaming.

  Own Id: OTP-18993 Aux Id: [PR-8048]

- Windows now supports all functions in `m:math`.

  Own Id: OTP-19001 Aux Id: [PR-8164]

- `m:erl_lint` (and by extension the [`compiler`](`m:compile`)) will now warn for code using deprecated callbacks.
  
  The only callback currenly deprecated is `format_status/2` in [`gen_server`](`c:gen_server:format_status/2`), [`gen_event`](`c:gen_event:format_status/2`) and [`gen_statem`](`c:gen_server:format_status/2`).
  
  You can use `nowarn_deprecated_callback` to silence the warning.

  Own Id: OTP-19010 Aux Id: [PR-8205]

- There is a new module [`json`](`m:json`) for encoding and decoding [JSON](https://en.wikipedia.org/wiki/JSON).
  
  Both encoding and decoding can be customized. Decoding can be done in a SAX-like fashion and handle multiple documents and streams of data.

  Own Id: OTP-19020 Aux Id: [PR-8111]

[PR-7183]: https://github.com/erlang/otp/pull/7183
[PR-7232]: https://github.com/erlang/otp/pull/7232
[PR-7383]: https://github.com/erlang/otp/pull/7383
[PR-7419]: https://github.com/erlang/otp/pull/7419
[PR-7534]: https://github.com/erlang/otp/pull/7534
[PR-7585]: https://github.com/erlang/otp/pull/7585
[PR-7590]: https://github.com/erlang/otp/pull/7590
[PR-7313]: https://github.com/erlang/otp/pull/7313
[PR-7451]: https://github.com/erlang/otp/pull/7451
[PR-7720]: https://github.com/erlang/otp/pull/7720
[PR-8003]: https://github.com/erlang/otp/pull/8003
[PR-7703]: https://github.com/erlang/otp/pull/7703
[PR-7649]: https://github.com/erlang/otp/pull/7649
[PR-7684]: https://github.com/erlang/otp/pull/7684
[PR-7675]: https://github.com/erlang/otp/pull/7675
[PR-7816]: https://github.com/erlang/otp/pull/7816
[PR-7845]: https://github.com/erlang/otp/pull/7845
[PR-7846]: https://github.com/erlang/otp/pull/7846
[PR-7847]: https://github.com/erlang/otp/pull/7847
[PR-7844]: https://github.com/erlang/otp/pull/7844
[PR-7879]: https://github.com/erlang/otp/pull/7879
[PR-7857]: https://github.com/erlang/otp/pull/7857
[PR-7745]: https://github.com/erlang/otp/pull/7745
[PR-7957]: https://github.com/erlang/otp/pull/7957
[PR-7973]: https://github.com/erlang/otp/pull/7973
[PR-7936]: https://github.com/erlang/otp/pull/7936
[PR-6791]: https://github.com/erlang/otp/pull/6791
[PR-8067]: https://github.com/erlang/otp/pull/8067
[PR-8069]: https://github.com/erlang/otp/pull/8069
[PR-8026]: https://github.com/erlang/otp/pull/8026
[PR-8048]: https://github.com/erlang/otp/pull/8048
[PR-8164]: https://github.com/erlang/otp/pull/8164
[PR-8205]: https://github.com/erlang/otp/pull/8205
[PR-8111]: https://github.com/erlang/otp/pull/8111

## STDLIB 5.2.3

### Fixed Bugs and Malfunctions

* Fix shell expansion of `-type a() :: $a.` in the erlang shell.

  Own Id: OTP-19062
* Fix the shell Job Control Mode to not crash when typing `TAB` or `CTRL+R`.

  Own Id: OTP-19072 Aux Id: PR-8391

## STDLIB 5.2.2

### Fixed Bugs and Malfunctions

* Attempting to use the `maybe` construct in a macro argument could crash the compiler.

  Own Id: OTP-19031 Aux Id: GH-8268

## STDLIB 5.2.1

### Fixed Bugs and Malfunctions

* The help texts shown by `argparse` will now display sub-command arguments in the correct order.

  Own Id: OTP-18900 Aux Id: PR-7945, GH-7934
* Clarified the argparse documentation regarding the user-defined help template.

  Own Id: OTP-18937
* Fix shell expansion to not crash when expanding invalid using invalid atoms.

  Own Id: OTP-18953 Aux Id: GH-8016 PR-8075

## STDLIB 5.2

### Fixed Bugs and Malfunctions

- Make `shell_docs` correctly trim the newline at the end of code blocks.

  Own Id: OTP-18777 Aux Id: PR-7663

- Replaced unintentional Erlang Public License 1.1 headers in some files with
  the intended Apache License 2.0 header.

  Own Id: OTP-18815 Aux Id: PR-7780

- Fixed a bug where autocompletion could crash the shell when trying to expand a
  nested tuple.

  Own Id: OTP-18822 Aux Id: PR-7796

- Removed auto closing feature, in autocompletion, for function arguments,
  tuples, records and maps, since this could interfere with autocompletion of
  atoms.

  Own Id: OTP-18823

- Fixed a bug where autocompletion string formatting would remove suggestions
  that had the same name but different case.

  Own Id: OTP-18824

- Fix so that ctrl+h, ctrl+backspace in the shell only removes one character
  instead of a whole word.

  Own Id: OTP-18826 Aux Id: PR-7797

- Fix so that its possible to override the default keyboard shortcuts for the
  shell.

  Own Id: OTP-18827 Aux Id: PR-7797

- Allow shell local func v(), in a restricted shell

  Own Id: OTP-18828 Aux Id: PR-7799

- Report syntax error when writing an invalid attribute like '1> -hej.'

  Own Id: OTP-18829 Aux Id: PR-7799

- When attempting to match part of a record in the key of a map generator, the
  entire record would be matched.

  Own Id: OTP-18866 Aux Id: GH-7875, PR-7878

### Improvements and New Features

- The warning for accidental use of a future triple-quoted string delimiter has
  been upgraded to instead warn for adjacent strings without intervening white
  space, which effectively is the same at a string start, but also covers the
  same situation at a string end.

  Own Id: OTP-18821 Aux Id: OTP-18746

- The removal of the deprecated `slave` module, originally planned for OTP 27,
  has been postponed to OTP 29.

  Own Id: OTP-18840 Aux Id: PR-7629

- Guards have been added to `gen_*:start*` API functions to catch bad arguments
  earlier. Before this change, in some cases, a bad argument could tag along and
  cause the server to fail later, right after start.

  Own Id: OTP-18857 Aux Id: GH-7685

## STDLIB 5.1.1

### Improvements and New Features

- Garbage collect the shell process when reducing the amount of saved history
  and results.

  Own Id: OTP-18773 Aux Id: PR-7691

## STDLIB 5.1

### Fixed Bugs and Malfunctions

- The compiler could run forever when compiling a call to
  [`is_record/3`](`is_record/3`) with a huge positive tuple size. The call
  [`is_record(A, a, 0)`](`is_record/3`) would crash the compiler when used in a
  function body. When used in a guard the compiler would emit incorrect code
  that would accept `{a>` as a record.

  Own Id: OTP-18605 Aux Id: GH-7298, GH-7317

- Fix bug in `ets:tab2file` that could make it fail if another Erlang process
  created the same file at the same time.

  Own Id: OTP-18614 Aux Id: GH-7162, PR-7237

- An `{else_clause,Value}` exception will now be reported nicely in the shell.

  Own Id: OTP-18616 Aux Id: GH-7258

- Correct return value for error case, so that it matches the documented and
  intended return value \{error, \{already_started, pid()\} when local
  registered names are used.

  Own Id: OTP-18627 Aux Id: PR-7072

- `sys:get_state/1,2` and `sys:replace_state/2,3` has been corrected to handle a
  state named `error` as a state name, not as a failed system callback.

  For the standard server behaviours this was an issue only for `gen_statem`
  (and `gen_fsm`) when the state name was `error`, and for `gen_server` if the
  complete state was `{error,_}`.

  Own Id: OTP-18633

- Multiple problems were fixed in `filelib:safe_relative_path/2`. If its second
  argument was a path that contained symbolic links, an incorrect result patch
  could be returned. Also, paths were sometimes falsely considered unsafe.

  Own Id: OTP-18655 Aux Id: GH-6460, PR-7208

- Fix deadlock when `erl.exe` is used as part of a pipe on Windows and trying to
  set the encoding of the `standard_io` device.

  Own Id: OTP-18675 Aux Id: PR-7473 GH-7459

- Expanded the documentation about how to use the `standard_io`,
  `standard_error` and `user` I/O devices.

  Added the types [`io:standard_io/0`](`t:io:standard_io/0`),
  `io:standard:error/0` and [`io:user/0`](`t:io:user/0`).

  Own Id: OTP-18676 Aux Id: PR-7473 GH-7459

- Fix `h/2,3` to properly render multi-clause documentation.

  Own Id: OTP-18683 Aux Id: PR-7502

- Timers created by `timer:apply_after/4`, `apply_interval/4`, and
  `apply_repeatedly/4` would silently fail to do the apply if it was not
  possible to spawn a process when the timer expired. This has now been
  corrected, and if the spawn fails, the system will be taken down producing a
  crash dump.

  Own Id: OTP-18759 Aux Id: GH-7606

- When an Erlang source file lacked a module definition, there would be a
  spurious "module name must not be empty" diagnostic for each spec in the file.

  Own Id: OTP-18763 Aux Id: GH-7655

### Improvements and New Features

- The argument descriptions for option types in `argparse` have been made less
  ambiguous.

  Own Id: OTP-18679 Aux Id: ERIERL-965

- Clarified the documentation of normal shutdown reason on `gen_server:call/2,3`

  Own Id: OTP-18690 Aux Id: PR-7511, GH-7510

- Pattern matching and equivalence (`=:=`, `=/=`) comparisons on `0.0` will now
  raise a warning, as it will no longer be considered equivalent to `-0.0` in
  OTP 27.

  If a match on `0.0` specifically is desired (distinct from `-0.0`), the
  warning can be suppressed by writing `+0.0` instead.

  The arithmetic comparison operators are unaffected, including arithmetic
  equality (`==`).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18696

- The semantics of the `gen_{server,statem,event}` behaviour's synchronous start
  behaviour introduced in OTP-26.0 with OTP-18471, has been clarified in the
  documentation.

  Own Id: OTP-18705 Aux Id: GH-7524, OTP-18471, GH-6339, PR-6843

- Added functionality to set a custom multiline prompt.

  Own Id: OTP-18736 Aux Id: PR-7564

- A warning for (accidental use of) Triple-Quoted Strings has been implemented
  as per
  [EEP 64](https://github.com/erlang/eep/blob/master/eeps/eep-0064.md#backwards-incompatibility).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18746 Aux Id: PR-7313, PR-7456

- The keyboard shortcuts for the shell are now configurable.

  Own Id: OTP-18754 Aux Id: PR-7604 PR-7647

## STDLIB 5.0.2

### Fixed Bugs and Malfunctions

- Fix bug where when you entered Alt+Enter in the terminal, the cursor would
  move to the last line, instead of moving to the next line.

  Own Id: OTP-18580 Aux Id: PR-7242

- Fix eof handling when reading from stdin when erlang is started using
  `-noshell`.

  Own Id: OTP-18640 Aux Id: PR-7384 GH-7368 GH-7286 GH-6881

- Fixed problem where output would disappear if it was received after a prompt
  was written in the shell.

  Own Id: OTP-18652 Aux Id: PR-7242

- The following functions are now much faster when given a long list or binary:

  - erlang:list_to_integer/1
  - erlang:binary_to_integer/1
  - erlang:binary_to_integer/2
  - erlang:list_to_integer/2
  - string:to_integer/1

  Own Id: OTP-18659 Aux Id: PR-7426

## STDLIB 5.0.1

### Fixed Bugs and Malfunctions

- The POSIX error `exdev` was sometimes incorrectly described as "cross domain
  link" in some error messages.

  Own Id: OTP-18578 Aux Id: GH-7213

## STDLIB 5.0

### Fixed Bugs and Malfunctions

- All process calls in `dets` have been updated to use the receive queue
  optimizations.

  Own Id: OTP-18275 Aux Id: PR-6045

- `proc_lib:start*/*` has become synchronous when the started process fails.
  This requires that a failing process use a new function
  `proc_lib:init_fail/2,3`, or exits, to indicate failure. All OTP behaviours
  have been fixed to do this.

  All these start functions now consume the `'EXIT'` message from a process link
  for all error returns. Previously it was only the `start_link/*` functions
  that did this, and only when the started function exited, not when it used
  `init_ack/1,2` or `init_fail/2,3` to create the return value.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18471 Aux Id: GH-6339, PR-6843

- Fixed a bug where `file:read(standard_io, ...)` unexpectedly returned `eof` in
  binary mode.

  Own Id: OTP-18486 Aux Id: PR-6881

- In the shell, `v(N)` would fail to retrieve the command if the command's
  return value was `undefined`.

  Own Id: OTP-18548 Aux Id: PR-6967

### Improvements and New Features

- The Erlang shell has been improved to support the following features:

  - Auto-complete variables, record names, record field names, map keys,
    function parameter types and filenames.
  - Open external editor in the shell (with C-o) to edit the current expression
    in an editor.
  - Support defining records (with types), functions and function typespecs, and
    custom types in the shell.
  - Do not save pager commands, and input to io:getline in history.

  Own Id: OTP-14835 Aux Id: PR-5924

- Gen_server now caches external functions for use in handle_call, handle_cast
  and handle_info.

  Own Id: OTP-15597 Aux Id: PR-5831

- The TTY/terminal subsystem has been rewritten by moving more code to Erlang
  from the old linked-in driver and implementing all the I/O primitives needed
  in a NIF instead.

  On Unix platforms the user should not notice a lot of difference, besides
  better handling of unicode characters and fixing of some long standing bugs.

  Windows users will notice that erl.exe has the same functionality as a normal
  Unix shell and that werl.exe has been removed and replaced with a symlink to
  erl.exe. This makes the Windows Erlang terminal experience identical to that
  of Unix.

  The re-write brings with it a number of bug fixes and feature additions:

  - The TTY is now reset when Erlang exits, fixing zsh to not break when
    terminating an Erlang session.
  - `standard_error` now uses the same unicode mode as `standard_io`.
  - Hitting backspace when searching the shell history with an empty search
    string no longer breaks the shell.
  - Tab expansion now works on remote nodes started using the JCL interface.
  - It is now possible to configure the shell slogan and the session slogans
    (that is the texts that appear when you start an Erlang shell). See the
    kernel documentation for more details.
  - Added shell:start_interactive for starting the interactive shell from a
    non-interactive Erlang session (for example an escript).
  - On Windows, when starting in detached mode the standard handler are now set
    to `nul` devices instead of being unset.
  - Standard I/O now always defaults to `unicode` mode if supported. Previously
    the default was `latin1` if the runtime system had been started with
    `-oldshell` or `-noshell` (for example in an `escript`). To send raw bytes
    over standard out, one now explicitly has to specify
    `io:setopts(standard_io, [{encoding, latin1}]).`

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17932 Aux Id: PR-6144 GH-3150 GH-3390 GH-4343 GH-4225

- Added the `zip:zip_get_crc32/2` function to retrieve the CRC32 checksum from
  an opened ZIP archive.

  Own Id: OTP-18159 Aux Id: PR-6904

- Added the options `post_process_args` and `detached` to the `peer:start`
  function.

  Own Id: OTP-18176 Aux Id: PR-6118

- The `re:replace/3,4` functions now accept a fun as the replacement argument.

  Own Id: OTP-18221 Aux Id: PR-6197

- The performance of the `base64` module has been significantly improved. For
  example, on an x86_64 system with the JIT both encode and decode are more than
  three times faster than in Erlang/OTP 25.

  Own Id: OTP-18228 Aux Id: GH-5639

- Improved implementation of `timer:apply_interval/4` reducing load on the timer
  server, and introduction of the new function `timer:apply_repeatedly/4`.
  `timer:apply_repeatedly/4` is similar to `timer:apply_interval/4`, but
  `timer:apply_repeatedly/4` prevents parallel execution of triggered `apply`
  operations which `timer:apply_interval/4` does not.

  Own Id: OTP-18236 Aux Id: PR-6256

- The `base64` module now supports encoding and decoding with an alternate URL
  safe alphabet, and an option for accepting or adding missing `=` padding
  characters.

  Own Id: OTP-18247 Aux Id: PR-6280, PR-6711

- Add `shell:whereis/0` which can be used to locate the current shell process.

  Own Id: OTP-18272 Aux Id: PR-6279

- The Erlang shell's auto-completion when typing `tab` has been changed to
  happen after the editing current line instead of before it.

  This behaviour can be configured using a the `shell_expand_location` STDLIB
  configuration parameter.

  Own Id: OTP-18278 Aux Id: PR-6260

- New function `ets:lookup_element/4` with a `Default` argument returned if the
  key did not exist in the table. The old `ets:lookup_element/3` raises a
  `badarg` exception which can be both inconvenient and slower.

  Own Id: OTP-18279 Aux Id: PR-6234

- Typing `Ctrl+L` in a shell now clears the screen and redraws the current line
  instead of only redrawing the current line. To only redraw the current line,
  you must now type `Alt+L`. This brings the behaviour of `Ctrl+L` closer to how
  bash and other shells work.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18285 Aux Id: PR-6262

- `peer` nodes using `standard_io` connections now include standard error from
  the node in the io stream from the started node.

  Own Id: OTP-18287 Aux Id: PR-5955

- A limitation in the binary syntax has been removed. It is now possible to
  match binary patterns in parallel. Example: `<<A:8>> = <<B:4,C:4>> = Bin`

  Own Id: OTP-18297 Aux Id: GH-6348

- Improve type specification of `unicode:characters_to_list()`.

  Own Id: OTP-18301 Aux Id: PR-6350

- In the `lists` module, the `zip` family of functions now takes options to
  allow handling lists of different lengths.

  Own Id: OTP-18318 Aux Id: PR-6347

- It is documented that `$\^X` is the ASCII code for Control X, where X is an
  uppercase or lowercase letter. However, this notation would work for any
  character X, even then it didn't make sense.

  In Erlang/OTP 26, it is now documented that the following characters are also
  allowed to follow the `\^` characters: `@`, `[`, `\`, `]`, `^`, `_`, and `?`.
  Attempt to use other characters will be rejected with a compiler error.

  The value for `$\^?` is now 127 (instead of 31 as in earlier releases).

  Own Id: OTP-18337 Aux Id: GH-6477, PR-6503

- The `binary:encode_hex/2` function has been added to allow the encoded
  hexadecimal digits to be in either lower or upper case.

  Own Id: OTP-18354 Aux Id: PR-6297

- Variants of `timer:tc()` with user specified time unit have been introduced.

  Own Id: OTP-18355 Aux Id: PR-6507

- New function `math:tau/0`. Returns `2*math:pi()`.

  Own Id: OTP-18361 Aux Id: PR-6536

- The BIFs [`min/2`](`min/2`) and [`max/2`](`max/2`) are now allowed to be used
  in guards and match specs.

  Own Id: OTP-18367 Aux Id: GH-6544

- Optimized `gen_server:multi_call()`.

  Own Id: OTP-18385 Aux Id: PR-6698

- Map comprehensions as suggested in EEP 58 has now been implemented.

  Own Id: OTP-18413 Aux Id: EEP-58, PR-6727

- Some map operations have been optimized by changing the internal sort order of
  atom keys. This changes the (undocumented) order of how atom keys in small
  maps are printed and returned by `maps:to_list/1` and `maps:next/1`. The new
  order is unpredictable and may change between different invocations of the
  Erlang VM.

  For applications where order is important, there is a new function
  `maps:iterator/2` for creating iterators that return the map elements in a
  deterministic order. There are also new modifiers `k` and `K` for the format
  string for [`io:format()`](`t:io:format/0`) to support printing map elements
  ordered.

  Own Id: OTP-18414 Aux Id: PR-6151

- Make gen_server fail "silently" with a new return value for init/1.

  Own Id: OTP-18423 Aux Id: https://github.com/erlang/backlog/issues/142

- Improved the selective receive optimization, which can now be enabled for
  references returned from other functions.

  This greatly improves the performance of `gen_server:send_request/3`,
  `gen_server:wait_response/2`, and similar functions.

  Own Id: OTP-18431 Aux Id: PR-6739

- It is no longer necessary to enable a feature in the runtime system in order
  to load modules that are using it. It is sufficient to enable the feature in
  the compiler when compiling it.

  That means that to use feature `maybe_expr` in Erlang/OTP 26, it is sufficient
  to enable it during compilation.

  In Erlang/OTP 27, feature `maybe_expr` will be enabled by default, but it will
  be possible to disable it.

  Own Id: OTP-18445

- Static supervisors are very idle processes after they have started so they
  will now be hibernated after start to improve resource management.

  Own Id: OTP-18474 Aux Id: PR-6895

- Deprecates `dbg:stop_clear/0` because it is simply a function alias to
  `dbg:stop/0`

  Own Id: OTP-18478 Aux Id: GH-6903

- Support has been added in `ms_transform` for the actions `caller_line/0`,
  `current_stacktrace/0`, and `current_stacktrace/1`.

  Own Id: OTP-18494 Aux Id: PR-6924

- The family of enumeration functions in module `lists` has been extended with
  `enumerate/3` that allows a step value to be supplied.

  Own Id: OTP-18495 Aux Id: PR-6943

- Update Unicode to version 15.0.0.

  Own Id: OTP-18500

- The regular expression library powering the `re` module is likely to be
  changed in Erlang/OTP 27. See
  [Upcoming Potential Incompatibilities](`e:general_info:upcoming_incompatibilities.md#new_re_engine`).

  Own Id: OTP-18511 Aux Id: PR-7017

- Improved the performance of `sets:subtract/2` when subtracting a small number
  of elements.

  Own Id: OTP-18515 Aux Id: GH-6990

- The linter will no longer raise warnings for underspecified opaque types.

  Own Id: OTP-18518 Aux Id: GH-7015

- Added the new built-in type `t:dynamic/0` introduced in EEP-61, improving
  support for gradual type checkers.

  Own Id: OTP-18522

- The by `gen_statem` previously used call proxy process that was used for
  preventing late replies from reaching the client at timeout or connection loss
  has been removed. It is no longer needed since _process aliases_ take care of
  this, are used, and supported by all Erlang nodes that an OTP 26 Erlang node
  can communicate with.

  Own Id: OTP-18537 Aux Id: PR-7081

- Added the `argparse` module for simplified argument handling in escripts and
  similar.

  Own Id: OTP-18558 Aux Id: PR-6852

- Added support for multiple line expressions and navigation in the shell. Added
  new keybindings:

  - navigate up (ctrl+up)/(alt+up)
  - navigate down (ctrl+down)/(alt+down)
  - insert newline in middle of line (alt+enter)
  - navigate top (alt+<)/(alt+shift+up)
  - navigate bottom (alt+>)/(alt+shift+down)
  - clear current expression (alt+c)
  - cancel search (alt+c)
  - opening editor on mac (option+o)/(alt+o)

  Modifies the prompt for new lines to make it clearer that the prompt has
  entered multi-line mode. Supports terminal with small window size, recommend
  not go lower than 7 rows and 40 columns. Modifies the search prompt to support
  multi-line statements. Redraw the prompt after continuing from JCL menu.

  Own Id: OTP-18575 Aux Id: PR-7169

## STDLIB 4.3.1.4

### Fixed Bugs and Malfunctions

* Attempting to use the `maybe` construct in a macro argument could crash the compiler.

  Own Id: OTP-19031 Aux Id: GH-8268

## STDLIB 4.3.1.3

### Improvements and New Features

- Garbage collect the shell process when reducing the amount of saved history
  and results.

  Own Id: OTP-18773 Aux Id: PR-7691

## STDLIB 4.3.1.2

### Fixed Bugs and Malfunctions

- The following functions are now much faster when given a long list or binary:

  - erlang:list_to_integer/1
  - erlang:binary_to_integer/1
  - erlang:binary_to_integer/2
  - erlang:list_to_integer/2
  - string:to_integer/1

  Own Id: OTP-18659 Aux Id: PR-7426

## STDLIB 4.3.1.1

### Improvements and New Features

- Static supervisors are very idle processes after they have started so they
  will now be hibernated after start to improve resource management.

  Own Id: OTP-18556

## STDLIB 4.3.1

### Fixed Bugs and Malfunctions

- The type specs in the `erl_parse` module has been updated to include the
  `maybe` construct and the `!` operator.

  Own Id: OTP-18506 Aux Id: GH-6956

## STDLIB 4.3

### Fixed Bugs and Malfunctions

- Fixed a bug that would cause analysis to crash.

  Own Id: OTP-18372 Aux Id: GH-6580

- Fixed a crash when formatting stack traces for error reports.

  Own Id: OTP-18375 Aux Id: GH-6591

- Instead of crashing, the [`list_to_integer/1`](`list_to_integer/1`) and
  [`list_to_integer/2`](`list_to_integer/2`) BIFs now raise the `system_limit`
  exception for overlong lists that can't be converted to integers. Similarly,
  the `string:to_integer/1` BIF now returns `{error,system_limit}` for overlong
  lists.

  Own Id: OTP-18475 Aux Id: PR-6897

### Improvements and New Features

- Removal of non-necessary `undefined` types added to the state's `supervisor`
  record.

  Own Id: OTP-18393 Aux Id: PR-6666

## STDLIB 4.2

### Fixed Bugs and Malfunctions

- `erl_tar` can now read gzip-compressed tar files that are padded. There is a
  new option `compressed_one` for `file:open/2` that will read a single member
  from a gzip file,

  Own Id: OTP-18289 Aux Id: PR-6343

- A concurrent call to `ets:rename` could cause `ets:delete_all_objects` to fail
  halfway through with badarg.

  Own Id: OTP-18292 Aux Id: PR-6366

- It is not allowed to call functions from guards. The compiler failed to reject
  a call in a guard when done by constructing a record with a default
  initialization expression that called a function.

  Own Id: OTP-18325 Aux Id: GH-6465, GH-6466

- The compiler could crash when using a record with complex field initialization
  expression as a filter in a list comprehension.

  Own Id: OTP-18336 Aux Id: GH-6501, PR-6502

- `unicode:characters_to_binary()` could build unnecessarily large call stack.

  Own Id: OTP-18351 Aux Id: ERIERL-885, PR-6529

### Improvements and New Features

- Improve error message for ets:new/2 name clash. Say "name already exists"
  instead of less specific "invalid options".

  Own Id: OTP-18283 Aux Id: PR-6338

## STDLIB 4.1.1

### Fixed Bugs and Malfunctions

- `m:peer` nodes failed to halt when the process supervising the control
  connection crashed. When an alternative control connection was used, this
  supervision process also quite frequently crashed when the `peer` node was
  stopped by the node that started it which caused the `peer` node to linger
  without ever halting.

  Own Id: OTP-18249 Aux Id: PR-6301

## STDLIB 4.1

### Fixed Bugs and Malfunctions

- Fixed inconsistency bugs in `m:global` due to `nodeup`/`nodedown` messages not
  being delivered before/after traffic over connections. Also fixed various
  other inconsistency bugs and deadlocks in both `m:global_group` and `global`.

  As building blocks for these fixes, a new BIF `erlang:nodes/2` has been
  introduced and `net_kernel:monitor_nodes/2` has been extended.

  The [`-hidden`](`e:erts:erl_cmd.md#hidden`) and
  [`-connect_all`](`e:erts:erl_cmd.md#connect_all`) command line arguments did
  not work if multiple instances were present on the command line which has been
  fixed. The new kernel parameter
  [`connect_all`](`e:kernel:kernel_app.md#connect_all`) has also been introduced
  in order to replace the `-connect_all` command line argument.

  Own Id: OTP-17934 Aux Id: PR-6007

- Fix the `public_key:ssh*` functions to be listed under the correct release in
  the Removed Functionality User's Guide.

  Own Id: OTP-18139 Aux Id: PR-6060

- The type spec for `format_status/1` in `gen_statem`, `gen_server` and
  `gen_event` has been corrected to state that the return value is of the same
  type as the argument (instead of the same value as the argument).

  Own Id: OTP-18142 Aux Id: PR-6078

- If the `timer` server child spec was already present in `kernel_sup` but it
  was not started, the `timer` server would fail to start with an
  `{error, already_present}` error instead of restarting the server.

  Own Id: OTP-18146 Aux Id: PR-5983

- When changing callback module in `gen_statem` the state_enter calls flag from
  the old module was used in for the first event in the new module, which could
  confuse the new module and cause malfunction. This bug has been corrected.

  With this change some `sys` debug message formats have been modified, which
  can be a problem for debug code relying on the format.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18239

### Improvements and New Features

- There is a new configure option, `--enable-deterministic-build`, which will
  apply the `deterministic` compiler option when building Erlang/OTP. The
  `deterministic` option has been improved to eliminate more sources of
  non-determinism in several applications.

  Own Id: OTP-18165 Aux Id: PR-5965

- The `rfc339_to_system_time/1,2` functions now allows the minutes part to be
  omitted from the time zone.

  Own Id: OTP-18166 Aux Id: PR-6108

- The `receive` statement in `gen_event` has been optimized to not use selective
  receive (which was never needed, and could cause severe performance
  degradation under heavy load).

  Own Id: OTP-18194 Aux Id: PR-6199

- Add new API function erl_features:configurable/0

  Own Id: OTP-18199 Aux Id: PR-5790

## STDLIB 4.0.1

### Fixed Bugs and Malfunctions

- In the initial release of Erlang/OTP 25, the expression bound to the `_`
  pseudo-field in a record initialization would always be evaluated once, even
  if all other fields in the record were explicitly initialized. That would
  break the use case of binding the expression `error(...)` to `_` in order to
  get an exception if not all fields were initialized.

  The behavior of binding to `_` has been reverted to the pre-OTP 25 behavior,
  that is, to not evaluate the expression if all fields have been bound to
  explicit values.

  Own Id: OTP-18110 Aux Id: GH-6000

## STDLIB 4.0

### Fixed Bugs and Malfunctions

- Improve the Erlang code linter's check of unused types.

  Own Id: OTP-17370 Aux Id: GH-4784

- Fix race condition in `proc_lib:stop/3` where the process is not stopped when
  the timeout given is very short.

  Own Id: OTP-17480 Aux Id: GH-4853 PR-4872

- Maps are now fully supported in by `ms_transform`.

  Own Id: OTP-17518 Aux Id: GH-4915

- Fix gen_server:call with the first argument as self() to throw an error
  instead of failing with a timeout.

  The same fix has also been done for gen_statem:call/3, gen_event:sync_notify/2
  and any other functionality relying on the internal gen:call/3 function.

  A similar fix was also done when using io:format/2 and the current
  group_leader was set to the current process.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17544 Aux Id: PR-5008

- erl_pp printed unary - and + operators with a space between the operator and
  the operand. This is fixed by not having any space in between.

  Own Id: OTP-17566 Aux Id: PR-5095, GH-5093

- Adjust uri_string:normalize behavior for URIs with undefined port (URI string
  with a port colon but no port value or URI map with port => undefined).

  Remove redundant normalization from http_request module.

  Before this change, normalize would not remove port subcomponent in such cases
  and could for example return "http://localhost:" URI.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17627

- Fix reduction counting bug in `re:run` that caused the function to yield too
  frequently when doing `global` matches.

  Own Id: OTP-17661 Aux Id: PR-5165

- Fix the memory value returned from `ets:info(Tid,memory)` when the
  `read_concurrency` option is used.

  Before this fix the memory used by the scheduler specific lock cache lines was
  not counted towards the total. This caused the returned memory usage to be
  very incorrect on systems with many schedulers for tables with man locks.

  Own Id: OTP-17832 Aux Id: PR-5494

- Avoid confusion by correcting the argument order in the gen_event crash log
  printout.

  Own Id: OTP-17878

- Fixed `string:next_grapheme/1` to return an empty binary in the tail for
  binary input for the last grapheme cluster.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18009 Aux Id: PR-5785

- Fixed type specifications of the `supervisor:sup_name/0` and
  [`supervisor:sup_ref/0`](`t:supervisor:sup_ref/0`) types.

  Own Id: OTP-18034 Aux Id: PR-4661, GH-4622

- If a default record field initialization (`_ = Expr`) was used even though all
  records fields were explicitly initialized, `Expr` would not be evaluated.
  That would not be a problem, except when `Expr` would bind a variable
  subsequently used, in which case the compiler would crash.

  As an example, if record `#r{}` is defined to have only one field `a`, the
  following code would crash the compiler:

  `#r{a=[],_=V=42}, V`

  To fix that problem, the compiler will make sure that `Expr` is always
  evaluated at least once. The compiler will now rewrite the example to
  essentially:

  `V=42, #r{a=[]}, V`

  Own Id: OTP-18083

### Improvements and New Features

- Users can now configure ETS tables with the `{write_concurrency, auto}`
  option. This option forces tables to automatically change the number of locks
  that are used at run-time depending on how much concurrency is detected. The
  `{decentralized_counters, true}` option is enabled by default when
  `{write_concurrency, auto}` is active.

  Benchmark results comparing this option with the other ETS optimization
  options are available here:

  https://erlang.org/bench/ets_bench_result_lock_config.html

  Own Id: OTP-15991 Aux Id: PR-5208

- The `format_status/2` callback for `gen_server`, `gen_statem` and `gen_event`
  has been deprecated in favor of the new `format_status/1` callback.

  The new callback adds the possibility to limit and change many more things
  than the just the state, such as the last received message, the reason for
  terminating and more events specific to each type of behavior. See the
  respective modules documentation for more details.

  Own Id: OTP-17351 Aux Id: GH-4673 PR-4952

- The `timer` module has been modernized and made more efficient, which makes
  the timer server less susceptible to being overloaded. The `timer:sleep/1`
  function now accepts an arbitrarily large integer.

  Own Id: OTP-17481 Aux Id: PR-4811

- Add `lists:enumerate/[1,2]`.

  Own Id: OTP-17523 Aux Id: PR-4928

- The configuration files [`.erlang`](`e:erts:erl_cmd.md`),
  [`.erlang.cookie`](`e:system:distributed.md`) and
  [`.erlang.crypt`](`m:beam_lib#module-erlang-crypt`) can now be located in the XDG
  Config Home directory.

  See the documentation for each file and `filename:basedir/2` for more details.

  Own Id: OTP-17554 Aux Id: GH-5016 PR-5408 OTP-17821

- Support `native` time unit in `calendar` functions `system_time_to_rfc3339/2`
  and `rfc3339_to_system_time`.

  Own Id: OTP-17592 Aux Id: ERIERL-663, PR-5243

- The tagged tuple tests and fun-calls have been optimized and are now a little
  bit cheaper than previously.

  These optimizations become possible after making sure that all boxed terms
  have at least one word allocated after the arity word. This has been
  accomplished by letting all empty tuples refer to the same empty tuple literal
  which also reduces memory usage for empty tuples.

  Own Id: OTP-17608

- The signal queue benchmark in parallel_messages_SUITE and the ETS benchmark in
  ets_SUITE have benchmark result visualization HTML pages with "fill-screen"
  buttons to make the graphs bigger. This button did not work as intended
  before. When pressing the button for a graph, the last graph got replaced with
  a bigger version and not the one over the button. This is now fixed.

  Own Id: OTP-17630

- The new module `peer` supersedes the `slave` module. The `slave` module is now
  deprecated and will be removed in OTP 27.

  `peer` contains an extended and more robust API for starting erlang nodes.

  Own Id: OTP-17720 Aux Id: PR-5162

- This change introduces quote and unquote functions in uri_string module - a
  replacement for deprecated encode and decode functions from http_uri.

  Own Id: OTP-17778 Aux Id: GH-5368

- In order to make it easier for the user to manage multiple outstanding
  asynchronous `call` requests, new functionality utilizing request identifier
  collections have been introduced in
  [`erpc`](`t:erpc:request_id_collection/0`),
  [`gen_server`](`t:gen_server:request_id_collection/0`),
  [`gen_statem`](`t:gen_statem:request_id_collection/0`), and
  [`gen_event`](`t:gen_event:request_id_collection/0`).

  Own Id: OTP-17784 Aux Id: PR-5792

- Update to the Unicode 14.0 specification.

  Own Id: OTP-17869 Aux Id: PR-5595

- The following ets types have been renamed to a clearer name: `tab/0` to
  `table/0` and `comp_match_spec/0` to `compiled_match_spec/0`.

  The types `table_access/0` and `table_type/0` have been exported.

  Own Id: OTP-17901 Aux Id: GH-4968 PR-5649

- Add support for locating `.asn1` files to the default search rules of
  `filelib:find_file/1` and `filelib:find_source/1`.

  Own Id: OTP-17908 Aux Id: GH-5655 PR-5669

- Type specifications have been added to the `gen_server`, and the documentation
  has been updated to utilize this.

  This surfaced a few type violations that has been corrected in `global`,
  `logger_olp` and `rpc`.

  Own Id: OTP-17915 Aux Id: PR-5751, GH-2375, GH-2690

- The non-local function handler for the `erl_eval` can now be called with
  either two or three arguments. When called with three arguments, the first
  argument is the annotation for the node in the abstract format.

  All errors during evaluation will now be passed through `erlang:raise/3`. If
  the restricted shell is active and it does not let `erlang:raise/3` through,
  evaluation errors will be printed in less clear way. See the documentation for
  restricted shell in `shell`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17925 Aux Id: PR-5631

- Added `filelib:ensure_path/1` that ensures that all directories for the given
  path exists (unlike `filelib:ensure_dir/1`, which will not create the last
  segment of the path).

  Own Id: OTP-17953 Aux Id: PR-5621

- The functions `groups_from_list/2` and `groups_from_list/3` have been added to
  the `maps` module.

  Own Id: OTP-17969 Aux Id: PR-5588

- `gen_server` has been refactored to throw more readable exceptions when a
  callback returns bad values in the `Timeout` field
  (`timeout() | 'hibernate' | {'continue,_}`), and also to verify that argument
  in the `gen_server:enter_loop/3,4,5` API function.

  Own Id: OTP-17974 Aux Id: GH-5683

- The functions `uniq/1` and `uniq/2` for removing duplicates have been added to
  the `lists` module.

  Own Id: OTP-17977 Aux Id: GH-5606, PR-5766

- Added support for configurable features as described in EEP-60. Features can
  be enabled/disabled during compilation with options
  (`-enable-feature Feature`, `-disable-feature Feature` and
  `+{feature, Feature, enable|disable}`) to `erlc` as well as with directives
  (`-feature(Feature, enable|disable).`) in the file. Similar options can be
  used to `erl` for enabling/disabling features allowed at runtime. The new
  `maybe` expression (EEP-49) is fully supported as the feature `maybe_expr`.
  The features support is documented in the reference manual.

  Own Id: OTP-17988

- The function `filename:safe_relative_path/1`, which has been deprecated since
  OTP 25, has been removed. Use `filelib:safe_relative_path/2` instead.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17991

- A new PRNG have been added to the `rand` module: `mwc59` which has been
  developed in collaboration with Sebastiano Vigna. It is intended for
  applications that need really fast pseudo-random numbers, and it comes with
  two output value scramblers, one fast and one thorough.

  Two internal functions for the `exsp` generator have also been exported so
  they can be used outside the `rand` plug-in framework to shave off some
  overhead.

  The internal `splitmix64` generator has also been exported which can be useful
  for seeding other kinds of PRNG:s than its own.

  Own Id: OTP-18011

## STDLIB 3.17.2.4

### Fixed Bugs and Malfunctions

- The following functions are now much faster when given a long list or binary:

  - erlang:list_to_integer/1
  - erlang:binary_to_integer/1
  - erlang:binary_to_integer/2
  - erlang:list_to_integer/2
  - string:to_integer/1

  Own Id: OTP-18659 Aux Id: PR-7426

## STDLIB 3.17.2.3

### Improvements and New Features

- Static supervisors are very idle processes after they have started so they
  will now be hibernated after start to improve resource management.

  Own Id: OTP-18556

## STDLIB 3.17.2.2

### Fixed Bugs and Malfunctions

- It is not allowed to call functions from guards. The compiler failed to reject
  a call in a guard when done by constructing a record with a default
  initialization expression that called a function.

  Own Id: OTP-18325 Aux Id: GH-6465, GH-6466

## STDLIB 3.17.2.1

### Fixed Bugs and Malfunctions

- When changing callback module in `gen_statem` the state_enter calls flag from
  the old module was used in for the first event in the new module, which could
  confuse the new module and cause malfunction. This bug has been corrected.

  With this change some `sys` debug message formats have been modified, which
  can be a problem for debug code relying on the format.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18239

## STDLIB 3.17.2

### Fixed Bugs and Malfunctions

- The type specifications for `shell_docs:get_doc/3`,
  `shell_docs:get_callback_doc/3`, and `shell_docs:get_type_doc/3` incorrectly
  stated that the returned `Metadata` was an empty map.

  Own Id: OTP-18081

## STDLIB 3.17.1

### Fixed Bugs and Malfunctions

- The compilation time is no longer recorded in BEAM files. There remained
  several undocumented functions that attempted to retrieve compilation times.
  Those have now been removed.

  Own Id: OTP-17962

## STDLIB 3.17

### Fixed Bugs and Malfunctions

- Fix rendering of nbsp on terminals that do not support unicode.

  Own Id: OTP-17662 Aux Id: PR-5206

- Improved the `m:erl_error` printout for when `m:re` fails to compile a regular
  expression to also print hints about why the compilation failed.

  Own Id: OTP-17750 Aux Id: PR-5366

- Fixed spec for `supervisor_bridge:start_link()`.

  Own Id: OTP-17766 Aux Id: PR-5362

- Added missing shutdown clauses in `supervisor` which could cause erroneous
  error reports.

  Own Id: OTP-17767 Aux Id: PR-5344

### Improvements and New Features

- Add the `no_auto_import_types` to erl_lint to allow a module to define types
  of the same name as a predefined type.

  Own Id: OTP-17744 Aux Id: PR-5292

## STDLIB 3.16.1

### Fixed Bugs and Malfunctions

- Fixed a bug that could cause a child to become orphaned when a supervisor died
  between unlinking and sending the shutdown signal to this child.

  There was also a possibility for erratic supervisor reports caused by a race
  between a supervisor shutting down a child and that child exiting by itself at
  the same time.

  Own Id: OTP-17649 Aux Id: GH-5193, PR-5201

## STDLIB 3.16

### Fixed Bugs and Malfunctions

- Fix `io:format` with `~p` to no longer interpret floats as printable
  characters.

  Own Id: OTP-17424 Aux Id: GH-4801 PR-4803

- Fix specs for base64 encode/decode functions to also include 0.

  Own Id: OTP-17429 Aux Id: GH-4761

- The failing call `io:format("~p\n")` would result in a warning for line number
  0 instead of the correct line and column numbers. This has been corrected, and
  all warnings for failing calls to [`io:format()`](`t:io:format/0`) has been
  rephrased to make it clearer exactly what the problem is.

  Own Id: OTP-17430

- When the options `warn_missing_spec` and `export_all` were given, there would
  only be warnings for missing specs for functions that had been explicitly
  exported using an `-export` attribute.

  Own Id: OTP-17434 Aux Id: GH-4772

- Calling `c:ls/1` with an atom whose contents is the the name of a file (as
  opposed to a directory) would crash.

  Own Id: OTP-17463 Aux Id: GH-4916

- The `MODULE` and `MODULE_STRING` macros would always appear to be defined
  (when tested by `-ifdef`), even though no `-module()` declaration had been
  seen yet. Changed so that `-ifdef ?MODULE.` will not consider ?MODULE defined
  if `-module()` has not been previously seen.

  Own Id: OTP-17505 Aux Id: GH-4995

- Fix bug with rendering of missing types and callbacks in shell_docs.

  Own Id: OTP-17573 Aux Id: ERL-1264 GH-4270

- When the `deterministic` option was given to the compiler, the `?FILE` macro
  would be expanded to full path of the source file before the first `include`
  directive and to base part of the filename after `include` directive.

  Own Id: OTP-17581 Aux Id: PR-5141

- Fixed broken `win32reg:delete_key` and fixed `win32reg:value` for `default`
  value.

  Own Id: OTP-17622 Aux Id: PR-5038

- Fixed error information for the call `maps:get(some_key, #{})`.

  Own Id: OTP-17634 Aux Id: GH-5196

### Improvements and New Features

- Most output functions in the `io` module now print extra error information
  when provided with invalid arguments. The functions are: `io:format`,
  `io:fwrite`, `io:put_chars`, `io:nl` and `io:write`.

  Own Id: OTP-17317 Aux Id: PR-4757

- EEP-54 (Provide more information about errors) now includes two new return
  values for the `format_error` callback, `general` and `reason`.

  Multi-line error descriptions returned from a `format_error` callback are now
  correctly indented.

  The documentation for `m:erl_error`, [`error/3`](`erlang:error/3`) and
  [Errors and Error Handling](`e:system:errors.md`) in the Erlang Reference
  Manual have been extended.

  Own Id: OTP-17454 Aux Id: PR-4764

- In the documentation for the `lists` module, it has been clarified that
  predicate funs must return a boolean.

  Own Id: OTP-17503 Aux Id: GH-4985

- The documentation for `c:c/1`, `c:c/2`, and `c:c/3` has been clarified.

  Own Id: OTP-17571 Aux Id: GH-5103

## STDLIB 3.15.2

### Fixed Bugs and Malfunctions

- Fix a bug that could cause a crash when formatting tuples using the control
  sequences `p` or `P` and limiting the output with the option `chars_limit`.

  Own Id: OTP-17525 Aux Id: GH-5053

## STDLIB 3.15.1

### Fixed Bugs and Malfunctions

- Fix a bug that could cause a loop when formatting terms using the control
  sequences p or P and limiting the output with the option `chars_limit`.

  Own Id: OTP-17459 Aux Id: GH-4824, GH-4842

## STDLIB 3.15

### Fixed Bugs and Malfunctions

- Time-outs in `gen_statem` with relative time `0` did not behave quite
  according to the intended model. This has now been corrected.

  The correction introduces a small potential incompatibility e.g when combining
  a state time-out with inserted events, and the inserted event does a state
  change in the state with the time-out. Before this correction the state
  time-out could be delivered even after the second state change, but now it is
  guaranteed that a state time-out is only delivered in the state it was started
  for, even in this corner case.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15107 Aux Id: ERL-1381, PR-2813

- Fix bugs in `erl_eval` concerning bitstring comprehensions.

  Own Id: OTP-16865

- File names that start with a dot (such as "`.gitignore`" are now treated as
  file names and not extensions by `filename:extension/1` and
  `filename:rootname/1`.

  Own Id: OTP-16905

- Fixed a bug where `beam_lib:chunks/3` with the `allow_missing_chunks` option
  would crash if a named chunk was missing.

  Own Id: OTP-16950 Aux Id: ERL-1378

- A floating point zero (0.0) can be both positive (+0.0) and negative (-0.0).
  Multiple bugs in the compiler, runtime system, and STDLIB have been fixed to
  ensure that the minus sign on 0.0 is not lost.

  Own Id: OTP-17077 Aux Id: ERL-1431, PR-2903, PR-2905, PR-2906

- Eliminated a Dialyzer crashed when the `-MMD` option is used to generate a
  dependency file and a BEAM file a the same time.

  Own Id: OTP-17118 Aux Id: PR-2825

- Fixed bug in `m:shell_docs` and `erl_docgen` that interpreted `em` tags as
  `strong`.

  Own Id: OTP-17122

- On Solaris, the `math:acos/1` and `math:asin/1` functions would not fail for
  arguments outside the valid domain.

  Own Id: OTP-17133

- Silence `unused_record` warnings when using `ms_transform`. The parse
  transform `ms_transform` replaces records with tuples, which can cause the
  Erlang code linter to emit warnings about unused records.

  Own Id: OTP-17186

- Documented a deficiency in the `re` module regarding the `[:ascii:]` character
  class matching Latin-1 characters.

  Own Id: OTP-17222 Aux Id: GH-4544

- Fixed `spec` of start functions in generic behaviors.

  Own Id: OTP-17342 Aux Id: GH-4725 PR-4726

- Supervisors rejected child specs with a shutdown value of 0.

  Own Id: OTP-17364 Aux Id: PR-4747

### Improvements and New Features

- In the `rand` module it is now possible to seed the default algorithm using an
  algorithm alias: `default`.

  Generating pseudo random binaries has been implemented with `rand:bytes/1` and
  `rand:bytes_s/2`.

  Own Id: OTP-14646 Aux Id: PR-2920

- New functions have been added to the `proplists` module: `to_map/1,2` and
  `from_map/1`.

  Own Id: OTP-14647 Aux Id: PR-2910

- New functions have been added to the `queue` module: `all/2`, `any/2`,
  `delete/2`, `delete_r/2`, `delete_with/2`, and `delete_with_r/2`.

  Own Id: OTP-14650 Aux Id: PR-2850

- New function have been added to the `queue` module: `fold/2` and
  `filtermap/2`.

  Own Id: OTP-14793 Aux Id: PR-2791

- Support for handling abstract code created before OTP R15 has been dropped.

  Own Id: OTP-16678 Aux Id: PR-2627

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

- The [_process alias_](`e:system:ref_man_processes.md#process-aliases`) feature
  as outlined by
  [EEP 53](https://github.com/erlang/eep/blob/master/eeps/eep-0053.md) has been
  introduced. It is introduced in order to provide a lightweight mechanism that
  can prevent late replies after timeout or connection loss. For more
  information, see EEP 53 and the documentation of the new
  [`alias/1`](`erlang:alias/1`) BIF and the new options to the
  [`monitor/3`](`erlang:monitor/3`) BIF.

  The `call` operation in the framework used by `gen_server`, `gen_statem`, and
  `gen_event` has been updated to utilize alias in order to prevent late
  responses. The `gen_statem` behavior still use a proxy process in the
  distributed case, since it has always prevented late replies and aliases wont
  work against pre OTP 24 nodes. The proxy process can be removed in OTP 26.

  The alias feature also made it possible to introduce new functions similar to
  the [`erpc:receive_response()`](`erpc:receive_response/2`) function in the gen
  behaviors, so the new functions
  [`gen_server:receive_response()`](`gen_server:receive_response/2`),
  [`gen_statem:receive_response()`](`gen_statem:receive_response/2`),
  [`gen_event:receive_response()`](`gen_event:receive_response/2`) have also
  been introduced.

  Own Id: OTP-16718 Aux Id: PR-2735

- Improved documentation about exit signals emitted when a `gen_server`
  terminates.

  Own Id: OTP-16910 Aux Id: PR-2771

- New functions have been added to the `maps` module: `merge_with/3`,
  `intersect/2`, `intersect_with/3`, `filtermap/2`, `from_keys/2`, and
  `maps:foreach/2`.

  `maps:merge_with/3` is the same as `merge/2` but takes an extra fun that is
  used to combine items with the same key.

  `maps:intersect/2` computes the intersection of two maps.

  `maps:intersect_with/3` is the same as `intersect/2` but takes an extra fun
  that is used to combine intersecting items.

  `maps:filtermap/2` allows filtering and mapping of a map in a single pass.

  `maps:from_keys/2` constructs a map from a list of keys and a single value and
  can be used to to optimize sets operations such as from_list/1, filter/2,
  intersection/2, and subtract/2.

  `maps:foreach/2` allows iteration over a map without returning any value.

  Own Id: OTP-16936 Aux Id: ERL-1367

- The experimental HiPE application has been removed, together with all related
  functionality in other applications.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16963

- The `filename:src/1` function which was deprecated in OTP 20 has been removed.
  Use `filelib:find_source/1,3` instead.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16971

- The pretty printer for floating point number have been changed to make it
  easier to see if the integer part of the number has been rounded. After the
  change the digit that may have been rounded always appears last or just before
  the exponent character (e or E). This is accomplished by always printing the
  number using scientific notation if it is so large that the integer part could
  be rounded.

  Own Id: OTP-16980 Aux Id: ERL-1308

- Accept references up to a size of 160-bits from remote nodes. This is the
  first step in an upgrade path toward using references up to 160-bits in a
  future OTP release.

  Own Id: OTP-17005 Aux Id: OTP-16718

- Add option `location` to `erl_parse:abstract/2`.

  Own Id: OTP-17024

- All long running functions in the maps API are now yielding. In previous
  releases the functions `maps:from_list/1`, `maps:keys/1` and `maps:values/1`
  did not yield. This could cause unfair scheduling of processes.

  Own Id: OTP-17057

- The `sets` module now has an optional map-based implementation, as described
  in `EEP 50`.

  To use this implementation, pass the `{version,2}` option to `sets:new/1` or
  `sets:from_list/2`.

  Own Id: OTP-17059 Aux Id: PR-2864

- Added `shell_docs:supported_tags/0`. This function can be used to retrieve the
  tags currently supported by `shell_docs`.

  Own Id: OTP-17120

- The `application/erlang+html` documentation storage format used by
  `m:shell_docs` has been updated to include the tags `b`, `strong`, `h4`, `h5`
  and `h6`.

  Own Id: OTP-17121

- Do not pretty-print `catch` expressions with unnecessary parentheses. The
  re-write of the Erlang parser grammar in PR-2584 implies that parentheses
  around `catch` expressions are in many cases no longer required.

  Own Id: OTP-17169 Aux Id: PR-2584

- Improved explanation of `{continue,Continue}` in `Module:init/1` of the
  `gen_server` documentation.

  Own Id: OTP-17171 Aux Id: PR-3011

- The `erl_eval` module now accepts a map for keeping track of bindings. Using
  an `orddict` for bindings will still work.

  Own Id: OTP-17175

- Documented `epp:scan_erl_form/1` and added `epp:scan_file/2`.

  Own Id: OTP-17199 Aux Id: PR-2658

- The standard floating point printing algorithm used by the `io` and `io_lib`
  modules has been changed from the algorithm described in \[1] to the Ryu
  algorithm \[2]. This gives a significant speed improvement for the printing of
  most floating point numbers and a small memory consumption improvement.

  \[1]: Robert G. Burger and R. Kent Dybvig. 1996. Printing floating-point
  numbers quickly and accurately. In Proceedings of the ACM SIGPLAN 1996
  conference on Programming language design and implementation (PLDI '96).
  Association for Computing Machinery, New York, NY, USA, 108–116.
  DOI:https://doi.org/10.1145/231379.231397

  \[2]: Ulf Adams. 2018. Ryū: fast float-to-string conversion. In Proceedings of
  the 39th ACM SIGPLAN Conference on Programming Language Design and
  Implementation (PLDI 2018). Association for Computing Machinery, New York, NY,
  USA, 270–282. DOI:https://doi.org/10.1145/3192366.3192369

  Thanks to Thomas Depierre

  Own Id: OTP-17210

- Add hex encoding and decoding functions in the binary module.

  Own Id: OTP-17236 Aux Id: PR-3014

- The undocumented and partially broken `ets:filter/3` function has been
  removed.

  Own Id: OTP-17263

- Add support in `m:shell_docs` to display any `"text"` documentation format.
  This means that `h(Module)` in the shell now can display the `"text/markdown"`
  of Elixir documentation.

  Own Id: OTP-17267

- The internal hashing of keys within ETS tables of types `set`, `bag`,
  `duplicate_bag` has been salted to diverge from `erlang:phash2`. This to avoid
  bad hashing if `phash2` is used to distribute the keys over separate
  tables/nodes.

  Own Id: OTP-17276 Aux Id: PR-2979

- Updated to the Unicode 13.0 specification.

  Own Id: OTP-17327 Aux Id: PR-4707

- Add compiler option `{nowarn_unused_record, RecordNames}`. Document compiler
  option `nowarn_unused_type`.

  Own Id: OTP-17330

- Implementation of
  [EEP 56](https://github.com/erlang/eep/blob/master/eeps/eep-0056.md) in
  supervisor. It adds the concept of `significant` children as well as the
  `auto_shutdown` supervisor flag.

  See the [supervisor manual page](`m:supervisor`) for more information.

  Own Id: OTP-17334 Aux Id: PR-4638, EEP-56

- Fixed warnings in code matching on underscore prefixed variables.

  Own Id: OTP-17385 Aux Id: OTP-17123

## STDLIB 3.14.2.3

### Fixed Bugs and Malfunctions

- It is not allowed to call functions from guards. The compiler failed to reject
  a call in a guard when done by constructing a record with a default
  initialization expression that called a function.

  Own Id: OTP-18325 Aux Id: GH-6465, GH-6466

## STDLIB 3.14.2.2

### Fixed Bugs and Malfunctions

- Fix a bug that could cause a crash when formatting tuples using the control
  sequences `p` or `P` and limiting the output with the option `chars_limit`.

  Own Id: OTP-17525 Aux Id: GH-5053

## STDLIB 3.14.2.1

### Fixed Bugs and Malfunctions

- Fix a bug that could cause a loop when formatting terms using the control
  sequences p or P and limiting the output with the option `chars_limit`.

  Own Id: OTP-17459 Aux Id: GH-4824, GH-4842

## STDLIB 3.14.2

### Fixed Bugs and Malfunctions

- Dictionaries that have become zipped by the zip module did not get executable
  permission (for the file owner) which makes the files inside the dictionary
  inaccessible. This is fixed by giving dictionaries inside a zip archive XRW
  permission.

  Own Id: OTP-17295 Aux Id: GH-4687

## STDLIB 3.14.1

### Fixed Bugs and Malfunctions

- Handle maps in `erl_parse:tokens()`.

  Own Id: OTP-16978

- The erlang shell function `rr` has been fixed to be able to read records from
  files within a code archive.

  Own Id: OTP-17182 Aux Id: PR-3002

- If `beam_lib` is asked to return abstract code for a BEAM file produced by
  Elixir and Elixir is not installed on the computer, `beam_lib` will no longer
  crash, but will return an error tuple. The `cover:compile_beam()` and
  `cover:compile_beam_directory()` functions have been updated to also return an
  error tuple in that situation.

  Own Id: OTP-17194 Aux Id: GH-4353

- Correct example module `erl_id_trans` regarding the `{char, C}` type.

  Own Id: OTP-17273

## STDLIB 3.14

### Fixed Bugs and Malfunctions

- This change fixes the handling of deep lists in the path component when using
  uri_string:recompose/1.

  Own Id: OTP-16941

- Fix `m:shell_docs` to clear shell decorations (bold/underline) when paginating
  output.

  Fix various small renderings issues when integrating `m:shell_docs` with edoc.

  Own Id: OTP-17047

### Improvements and New Features

- Improved the API and documentation of the uri_string module.

  Added a new chapter to the Users Guide about Uniform Resource Identifiers and
  their handling with the new API.

  Added two new API functions: uri_string:allowed_characters/0 and
  uri_string:percent_decode/1.

  This change has been marked as potentially incompatible as
  uri*string:normalize/2 used to decode percent-encoded character triplets that
  corresponded to characters not in the reserved set. After this change,
  uri_string:normalize/2 will only decode those percent-encoded triplets that
  correspond to characters in the unreserved set (ALPHA / DIGIT / "-" / "." /
  "*" / "~").

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16460

- The `shell_docs` module has been expanded with the possibility to configure
  unicode, ansi and column size for the rendered text.

  Own Id: OTP-16990

## STDLIB 3.13.2

### Fixed Bugs and Malfunctions

- The functions `digraph:in_edges/2` and `digraph:out_edges/2` would return
  false edges if called for a vertex that had a '\_' atom in its name term.

  Own Id: OTP-16655

- `filelib:wildcard("not-a-directory/..")` should return an empty list. On
  Windows it returned `"not-a-directory/.."`.

  Own Id: OTP-16700

- Fix the typespec of shell_docs:render to use the correct type for an MFA.

  Own Id: OTP-16739

- Fix uri_string:recompose/1 when host is present but input path is not
  absolute.

  This change prevents the recompose operation to change the top level domain of
  the host when the path does not start with a slash.

  Own Id: OTP-16751 Aux Id: ERL-1283

- The `epp` module would return a badly formed error term when an '`if`'
  preprocessor directive referenced an undefined symbol. `epp:format_error/1`
  would crash when called with the bad error term.

  Own Id: OTP-16816 Aux Id: ERL-1310

- `lists:sublist(List, Start, Len)` failed with an exception if
  `Start > length(List) + 1` even though it is explicitly documented that "It is
  not an error for `Start+Len` to exceed the length of the list".

  Own Id: OTP-16830 Aux Id: ERL-1334, PR-2718

## STDLIB 3.13.1

### Fixed Bugs and Malfunctions

- When a temporary child of a `simple_one_for_one supervisor` died, the internal
  state of the supervisor would be corrupted in a way that would cause the
  supervisor to retain the start arguments for subsequent children started by
  the supervisor, causing unnecessary growth of the supervisor's heap. There
  state corruption could potentially cause other problems as well.

  Own Id: OTP-16804

## STDLIB 3.13

### Fixed Bugs and Malfunctions

- Compiling a match specification with excessive nesting caused the runtime
  system to crash due to scheduler stack exhaustion. Instead of crashing the
  runtime system, effected functions will now raise a `system_limit` error
  exception in this situation.

  Own Id: OTP-16431 Aux Id: ERL-592

- Initialization of record fields using `_` is no longer allowed if the number
  of affected fields is zero.

  Own Id: OTP-16516

- Fix bugs in `eval_bits`.

  Own Id: OTP-16545

### Improvements and New Features

- Improved the printout of single line logger events for most of the OTP
  behaviours in STDLIB and Kernel. This includes `proc_lib`, `gen_server`,
  `gen_event`, `gen_statem`, `gen_fsm`, `supervisor`, `supervisor_bridge` and
  `application`.

  Improved the [`chars_limit`](`m:logger_formatter#chars_limit`) and
  [`depth`](`m:logger_formatter#depth`) handling in `proc_lib` and when
  formatting of exceptions.

  Own Id: OTP-15299

- Remove usage and documentation of old requests of the I/O-protocol.

  Own Id: OTP-15695

- Improved ETS scalability of concurrent calls that change the size of a table,
  like `ets:insert/2` and `ets:delete/2`.

  This performance feature was implemented for `ordered_set` in OTP 22.0 and
  does now apply for all ETS table types.

  The improved scalability may come at the cost of longer latency of
  `ets:info(T,size)` and `ets:info(T,memory)`. A new table option
  `decentralized_counters` has therefore been added. It is default `true` for
  `ordered_set` with `write_concurrency` enabled and default `false` for all
  other table types.

  Own Id: OTP-15744 Aux Id: OTP-15623, PR-2229

- Handle Unicode filenames in the `zip` module.

  Own Id: OTP-16005 Aux Id: ERL-1003, ERL-1150

- Unicode support was updated to the Unicode 12.1 standard.

  Own Id: OTP-16073 Aux Id: PR-2339

- All of the modules [`proc_lib`](`proc_lib:start_monitor/3`),
  [`gen_server`](`gen_server:start_monitor/3`),
  [`gen_statem`](`gen_statem:start_monitor/3`), and
  [`gen_event`](`gen_event:start_monitor/0`) have been extended with a
  `start_monitor()` function. For more information, see the documentation of
  `start_monitor()` for these modules.

  Own Id: OTP-16120 Aux Id: ERIERL-402, PR-2427

- Updates for new `erlang:term_to_iovec()` BIF.

  Own Id: OTP-16128 Aux Id: OTP-15618

- Documented a quirk regarding extraction from file descriptors in `erl_tar`.

  Own Id: OTP-16171 Aux Id: ERL-1057

- Added `ok` as return value to `gen_server:reply/2`

  Own Id: OTP-16210 Aux Id: PR-2411

- New functions have been added to `m:c` for printing embedded documentation for
  Erlang modules. The functions are:

  - **h/1,2,3** - Print the documentation for a Module:Function/Arity.

  - **ht/1,2,3** - Print the type documentation for a Module:Type/Arity.

  The embedded documentation is created when building the Erlang/OTP
  documentation.

  Own Id: OTP-16222

- Add `indent` and `linewidth` to the options of the `erl_pp` module's
  functions.

  Own Id: OTP-16276 Aux Id: PR-2443

- Minor updates due to the new spawn improvements made.

  Own Id: OTP-16368 Aux Id: OTP-15251

- The compiler will now raise a warning when inlining is used in modules that
  load NIFs.

  Own Id: OTP-16429 Aux Id: ERL-303

- Refactored the internal handling of deprecated and removed functions.

  Own Id: OTP-16469

- Extend `erl_parse:abstract/1,2` to handle external fun expressions
  (`fun M:F/A`).

  Own Id: OTP-16480

- Added `filelib:safe_relative_path/2` to replace
  `filename:safe_relative_path/1`, which did not safely handle symbolic links.

  `filename:safe_relative_path/1` has been deprecated.

  Own Id: OTP-16483 Aux Id: PR-2542

- The module `shell_docs` has been added. The module contains functions for
  rendering, validating and normalizing embedded documentation.

  Own Id: OTP-16500

- Module and function auto-completion in the shell now looks at all available
  modules instead of only those loaded. A module is considered available if it
  either is loaded already or would be loaded if called.

  The auto-completion has also been expanded to work in the new `h/1,2,3`
  function in `m:c`.

  Own Id: OTP-16501 Aux Id: OTP-16494, OTP-16222, OTP-16406, OTP-16499,
  OTP-16500, PR-2545, ERL-708

- Updated the internal `pcre` library to `8.44`.

  Own Id: OTP-16557

## STDLIB 3.12.1.2

### Fixed Bugs and Malfunctions

- Fix a bug that could cause a crash when formatting tuples using the control
  sequences `p` or `P` and limiting the output with the option `chars_limit`.

  Own Id: OTP-17525 Aux Id: GH-5053

## STDLIB 3.12.1.1

### Fixed Bugs and Malfunctions

- Fix a bug that could cause a loop when formatting terms using the control
  sequences p or P and limiting the output with the option `chars_limit`.

  Own Id: OTP-17459 Aux Id: GH-4824, GH-4842

## STDLIB 3.12.1

### Fixed Bugs and Malfunctions

- [re:run(Subject, RE, \[unicode])](`re:run/3`) returned `nomatch` instead of
  failing with a `badarg` error exception when `Subject` contained illegal utf8
  and `RE` was passed as a binary. This has been corrected along with
  corrections of reduction counting in `re:run()` error cases.

  Own Id: OTP-16553

## STDLIB 3.12

### Fixed Bugs and Malfunctions

- Fix type specification for uri_string:normalize/2 that may also return
  error().

  Own Id: OTP-16322

- Improve error handling in uri_string:normalize/2. This change fixes a crash
  when the input URI has faulty percent-encoding.

  Own Id: OTP-16351

- Fix minor bugs in the Erlang pretty printer (`erl_pp`).

  Own Id: OTP-16435

- Fix the Erlang parser regarding consecutive unary operators.

  Own Id: OTP-16439

- Let `calendar:rfc3339_to_system_time()` crash when the time offset is missing.

  Own Id: OTP-16514 Aux Id: ERL-1182

### Improvements and New Features

- Implement uri_string:resolve/\{2,3\} that can be used to resolve a URI
  reference against a base URI.

  Own Id: OTP-16321

- In `gen_statem` it is now possible to change the callback module for a running
  server. See `gen_statem`'s documentation for `change_callback_module`,
  `push_callback_module`, and `pop_callback_module`.

  Own Id: OTP-16477 Aux Id: PR-2531

## STDLIB 3.11.2

### Fixed Bugs and Malfunctions

- A directory traversal vulnerability has been eliminated in erl_tar. erl_tar
  will now refuse to extract symlinks that points outside the targeted
  extraction directory and will return `{error,{Path,unsafe_symlink}}`. (Thanks
  to Eric Meadows-Jönsson for the bug report and for suggesting a fix.)

  Own Id: OTP-16441

## STDLIB 3.11.1

### Fixed Bugs and Malfunctions

- The `ets:update_counter/4` core dumped when given an ordered_set with
  write_concurrency enabled and an invalid position. This bug has been fixed.

  Own Id: OTP-16378 Aux Id: ERL-1125

## STDLIB 3.11

### Fixed Bugs and Malfunctions

- The functions [`unicode:characters_to_list()`](`unicode:characters_to_list/2`)
  and [`unicode:characters_to_binary()`](`unicode:characters_to_binary/3`)
  raised a `badarg` exception instead of returning an error tuple when passed
  very large invalid code points as input.

  Own Id: OTP-16052

- Fixed a bug in the linter where list and binary comprehensions could suppress
  unsafe variable errors.

  Own Id: OTP-16053 Aux Id: ERL-1039

- Fixed incorrect type specifications for `erl_tar:open/2`, `create/2,3`, and
  `add/4`.

  Own Id: OTP-16085 Aux Id: PR-2379

- Fixed erroneous type spec for `binary:list_to_bin/1`. Argument type was
  changed from `t:iodata/0` to `t:iolist/0`.

  Own Id: OTP-16132 Aux Id: ERL-1041

- Fix a race in `pool:pspawn_link` that caused a `noproc` error to be thrown
  when using it to spawn a very short lived process.

  Own Id: OTP-16211

- Fixed a performance issue in ETS lookup when using the `compressed` option and
  the term contained atoms. Before this fix the decompress algorithm for atoms
  would unnecessarily take a global lock to validate the atom.

  Own Id: OTP-16316

### Improvements and New Features

- Added a new compiler/linter option to disable warnings for unused types
  (`nowarn_unused_type`).

  Own Id: OTP-16262 Aux Id: ERIERL-435

- ETS tables have been optimized to not use any locks when running in a system
  with only one scheduler enabled. This can provide significant performance
  gains for applications that use ETS tables heavily.

  Own Id: OTP-16315

## STDLIB 3.10

### Fixed Bugs and Malfunctions

- `re:run()` now yields when validating utf8 in a large subject.

  Own Id: OTP-15836 Aux Id: ERL-876

- Upgraded the ERTS internal PCRE library from version 8.42 to version 8.43. See
  [http://pcre.org/original/changelog.txt](http://pcre.org/original/changelog.txt)
  for information about changes made to PCRE. This library implements major
  parts of the `m:re` regular expressions module.

  Own Id: OTP-15889

- The bug with ID ERL-717 has been fixed. The functions `io:columns()` and
  `io:rows()` only worked correctly inside interactive erlang shells before this
  fix. These functions returned `{error,enotsup}` before this fix even if stdout
  and stdin were connected to a terminal when they were invoked from an escript
  or a program started with e.g., `erl -noshell`.

  Own Id: OTP-15959 Aux Id: ERL-717

- Fixed handling of ".." and "@" in wildcards. ".." would only work when
  preceded by a literal pattern such as in "a/..", not when preceded by wildcard
  characters such as in "\*/..". The combination "@/.." was also broken, and in
  addition "@" in a pattern could degrade performance of the wildcard matching.

  Own Id: OTP-15987 Aux Id: ERL-1029

- Make sure `ets:fun2ms()` can handle `++/2` in the head of functions when
  called from the shell.

  Own Id: OTP-15992 Aux Id: PR-2322

### Improvements and New Features

- Debugging of time-outs in `gen_statem` has been improved. Starting a time-out
  is now logged in `sys:log` and `sys:trace`. Running time-outs are visible in
  server crash logs, and with `sys:get_status`. Due to this system events
  `{start_timer, Action, State}` and `{insert_timout, Event, State}` have been
  added, which may surprise tools that rely on the format of these events.

  New features: The `EventContent` of a running time-out can be updated with
  `{TimeoutType, update, NewEventContent}`. Running time-outs can be cancelled
  with `{TimeoutType, cancel}` which is more readable than using
  `Time = infinity`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15510

- `re:run()` now avoids validating utf8 in the subject more than once in the
  same call. This validation could previously be performed multiple times when
  the `global` option was passed.

  Own Id: OTP-15831 Aux Id: ERL-876

- ETS `ordered_set` tables with `write_concurrency` enabled has got a
  performance issue fixed. There were no limits for the values of internal
  statistics counters before this fix. This could result in that the data
  structure sometimes reacted slowly to a change in how many parallel processes
  were using it.

  Own Id: OTP-15906

- The `ordsets:union/1` is now faster when passed a long list of ordsets.

  Own Id: OTP-15927

- `unicode:characters_to_binary()` could return very small binaries as reference
  counted off heap binaries. This could cause an unnecessary large memory usage
  and an unnecessary load on the binary allocator. Small binaries are now always
  returned as heap binaries.

  Own Id: OTP-16002 Aux Id: ERIERL-366

- Display a more meaningful error message when a bad I/O server is used in a
  script written in Erlang (`escript`).

  Own Id: OTP-16006 Aux Id: ERL-992

- New feature `ets:info(_, binary)` to get information about all reference
  counted binaries kept by a table. This is the same kind of debug information
  that [`process_info(_, binary)`](`process_info/2`) returns for a process.

  Own Id: OTP-16035 Aux Id: ERIERL-366

- Corrected ETS documentation about the behavior of compiled match
  specifications when serialized through external format.

  Own Id: OTP-16038 Aux Id: PR-2366

## STDLIB 3.9.2

### Fixed Bugs and Malfunctions

- Fix a bug that could cause a loop when formatting terms using the control
  sequences `p` or `P` and limiting the output with the option `chars_limit`.

  Own Id: OTP-15875 Aux Id: ERL-967

## STDLIB 3.9.1

### Fixed Bugs and Malfunctions

- Fix a bug that could cause a failure when formatting binaries using the
  control sequences `p` or `P` and limiting the output with the option
  `chars_limit`.

  Own Id: OTP-15847 Aux Id: ERL-957

## STDLIB 3.9

### Fixed Bugs and Malfunctions

- Fix a bug in `string:lexemes/2`.

  The bug was found when optimizing the handling of deep lists of Unicode
  characters in the `string` module.

  Own Id: OTP-15649

- A bug has been fixed in the `maps` implementation that could cause a crash or
  memory usage to grow until the machine ran out of memory. This could happen
  when inserting a new key-value pair with a key `K1` containing a binary `B1`
  into a map `M` having a key `K2` with a binary `B2` if the following
  conditions were met:

  - `B1 =/= B2`
  - `size(B1) >= 4294967296`
  - `size(B2) >= 4294967296`
  - `size(M) >= 32`
  - `(size(B1) rem 4294967296) == (size(B2) rem 4294967296)`
  - the first `(size(B1) rem 4294967296)` bytes are the same both in `B1` and
    `B2`
  - substituting `B1` in `K1` with `B2` would create a term with the same value
    as `K2`

  The root cause of the problem is that the `maps` implementation only hashed
  the first `(X rem 4294967296)` bytes of binaries so that different binaries
  could get the same hash value independently of the hash seed.

  Own Id: OTP-15707

- Since the introduction of the stack trace variable, the Erlang Pretty Printer
  has left out the exception class `throw` even when the stack trace variable
  cannot be left out, which is not correct Erlang code. The fix is to always
  include the exception class `throw`.

  Own Id: OTP-15751

- `record_info/2` is a pseudo-function that requires literal arguments known at
  compile time. Therefore, the following usage is illegal: `fun record/info/2`.
  The compiler would crash when during compilation of that kind of code.
  Corrected to issue a compilation error.

  Own Id: OTP-15760 Aux Id: ERL-907

### Improvements and New Features

- A new `rand` module algorithm, `exro928ss` (Xoroshiro928\*\*), has been
  implemented. It has got a really long period and good statistical quality for
  all output bits, while still being only about 50% slower than the default
  algorithm.

  The same generator is also used as a long period counter in a new `crypto`
  plugin for the `rand` module, algorithm `crypto_aes`. This plugin uses AES-256
  to scramble the counter which buries any detectable statistical artifacts.
  Scrambling is done in chunks which are cached to get good amortized speed
  (about half of the default algorithm).

  Own Id: OTP-14461 Aux Id: PR-1857

- Types related to server naming and starting have been exported from
  `gen_statem`. These are: `server_name/0`, `server_ref/0`, `start_opt/0`,
  `start_ret/0` and `enter_loop_opt/0`.

  Own Id: OTP-14724 Aux Id: PR-2056

- The default algorithm for the `rand` module has been changed to `exsss`
  (Xorshift116\*\*) which is a combination of the Xorshift116 (`exsp`) state
  update and a new scrambler "StarStar" from the 2018 paper "Scrambled Linear
  Pseudorandom Number Generators" by David Blackman and Sebastiano Vigna. This
  combination should not have the caveat of weak low bits that the previous
  default algorithm(s) have had, with the cost of about 10% lower speed. See
  GitHub pull request #1969.

  Own Id: OTP-14731 Aux Id: PR-1969

- The generic state machine behaviour `gen_statem` has gotten code cleanup and
  documentation improvements from GitHub Pull Request #1855, even though the PR
  itself was rejected.

  Own Id: OTP-14737 Aux Id: PR-1855

- Update Unicode specification to version 11.0.

  Own Id: OTP-15111

- ETS option `write_concurrency` now also affects and improves the scalability
  of `ordered_set` tables. The implementation is based on a data structure
  called contention adapting search tree, where the lock granularity adapts to
  the actual amount of concurrency exploited by the applications in runtime.

  Own Id: OTP-15128

- Optimized `maps:new/0` with trivial Erlang implementation, making use of
  literal terms (the empty map) not needing dynamic heap allocation.

  Own Id: OTP-15200 Aux Id: PR-1878

- The `gen_*` behaviours have been changed so that if logging of the last N
  messages through `sys:log/2,3` is active for the server, this log is included
  in the terminate report.

  To accomplish this the format of "System Events" as defined in the man page
  for `sys` has been clarified and cleaned up, a new function `sys:get_log/1`
  has been added, and `sys:get_debug/3` has been deprecated. Due to these
  changes, code that relies on the internal badly documented format of "System
  Events", need to be corrected.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15381

- The `gen_statem` behaviour engine loop has been optimized for better
  performance in particular when the callback module returns some actions, that
  is better performance for more realistic applications than the Echo Benchmark.

  Own Id: OTP-15452

- Do not allow function specifications for functions residing in other modules.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15563 Aux Id: ERL-845, OTP-15562

- The `persistent_term` functions [`put/2`](`put/2`) and [`erase/1`](`erase/1`)
  are now yielding.

  Own Id: OTP-15615

- Previously, all ETS tables used centralized counter variables to keep track of
  the number of items stored and the amount of memory consumed. These counters
  can cause scalability problems (especially on big NUMA systems). This change
  adds an implementation of a decentralized counter and modifies the
  implementation of ETS so that ETS tables of type `ordered_set` with
  `write_concurrency` enabled use the decentralized counter. Experiments
  indicate that this change substantially improves the scalability of ETS
  `ordered_set` tables with `write_concurrency` enabled in scenarios with
  frequent `ets:insert/2` and `ets:delete/2` calls.

  Own Id: OTP-15623 Aux Id: PR-2190

- Use `ssh` instead of `rsh` as the default remote shell.

  Own Id: OTP-15633 Aux Id: PR-1787

- Added `beam_lib:strip/2` and friends, which accept a list of chunks that
  should be preserved when stripping.

  Own Id: OTP-15680 Aux Id: PR-2114

- Optimize printing of maps with `io_lib:write()`. Also optimize pretty printing
  of strings (`~s` and `~ts`) when limiting the output with the `chars_limit`
  option.

  Own Id: OTP-15705

- There are new compiler options `nowarn_removed` and `{nowarn_removed,Items}`
  to suppress warnings for functions and modules that have been removed from
  OTP.

  Own Id: OTP-15749 Aux Id: ERL-904

- Let the Erlang Pretty Printer put atomic parts on the same line.

  Own Id: OTP-15755

- Add option `quote_singleton_atom_types` to the Erlang Pretty Printer's
  functions. Setting the option to `true` adds quotes to all singleton atom
  types.

  Own Id: OTP-15756

## STDLIB 3.8.2.4

### Fixed Bugs and Malfunctions

- [re:run(Subject, RE, \[unicode])](`re:run/3`) returned `nomatch` instead of
  failing with a `badarg` error exception when `Subject` contained illegal utf8
  and `RE` was passed as a binary. This has been corrected along with
  corrections of reduction counting in `re:run()` error cases.

  Own Id: OTP-16553

## STDLIB 3.8.2.3

### Fixed Bugs and Malfunctions

- A directory traversal vulnerability has been eliminated in erl_tar. erl_tar
  will now refuse to extract symlinks that points outside the targeted
  extraction directory and will return `{error,{Path,unsafe_symlink}}`. (Thanks
  to Eric Meadows-Jönsson for the bug report and for suggesting a fix.)

  Own Id: OTP-16441

## STDLIB 3.8.2.2

### Fixed Bugs and Malfunctions

- Fix a bug that could cause a loop when formatting terms using the control
  sequences `p` or `P` and limiting the output with the option `chars_limit`.

  Own Id: OTP-15875 Aux Id: ERL-967

## STDLIB 3.8.2.1

### Fixed Bugs and Malfunctions

- Fix a bug that could cause a failure when formatting binaries using the
  control sequences `p` or `P` and limiting the output with the option
  `chars_limit`.

  Own Id: OTP-15847 Aux Id: ERL-957

## STDLIB 3.8.2

### Fixed Bugs and Malfunctions

- A bug in gen_statem has been fixed where the internal timeout message could
  arrive as an info to the callback module during high load due to incorrect use
  of asynchronous timer cancel.

  Own Id: OTP-15295

## STDLIB 3.8.1

### Fixed Bugs and Malfunctions

- Fixed a performance regression when reading files opened with the `compressed`
  flag.

  Own Id: OTP-15706 Aux Id: ERIERL-336

## STDLIB 3.8

### Fixed Bugs and Malfunctions

- Fix a bug in the Erlang Pretty Printer: long atom names in combination with
  `<<>>` could cause a crash.

  Own Id: OTP-15592 Aux Id: ERL-818

- Fix bugs that could cause wrong results or bad performance when formatting
  lists of characters using the control sequences `p` or `P` and limiting the
  output with the option `chars_limit`.

  Own Id: OTP-15639

### Improvements and New Features

- Improved ETS documentation about safe table traversal and the partially bound
  key optimization for `ordered_set`.

  Own Id: OTP-15545 Aux Id: PR-2103, PR-2139

- Optimize `calendar:gregorian_days_to_date/1`.

  Own Id: OTP-15572 Aux Id: PR-2121

- Optimize functions `calendar:rfc3339_to_system_time()` and
  `calendar:system_time_to_rfc3339()`.

  Own Id: OTP-15630

## STDLIB 3.7.1

### Fixed Bugs and Malfunctions

- Optimize pretty printing of terms. The slower behaviour was introduced in
  Erlang/OTP 20.

  Own Id: OTP-15573 Aux Id: ERIERL-306

## STDLIB 3.7

### Fixed Bugs and Malfunctions

- Document `bit_size` in match specifications and allow it in `ets:fun2ms`.

  Own Id: OTP-15343 Aux Id: PR-1962

- The `beam()` type in `beam_lib` is defined as
  `module() | file:filename() | binary()`. The `t:module/0` is misleading.
  Giving the module name as an atom will only work if the BEAM file is in a
  current directory.

  To avoid confusion, `t:module/0` has been removed from the type. That means
  that there will be a Dialyzer warning for code that call `beam_lib` with an
  atom as filename, but the calls will still work.

  Own Id: OTP-15378 Aux Id: ERL-696

- `unicode_util` crashed on certain emoji grapheme clusters in binary strings.

  Own Id: OTP-15428 Aux Id: ERL-777

- When an external fun was used, warnings for unused variables could be
  suppressed.

  Own Id: OTP-15437 Aux Id: ERL-762

- Fix reduction count in lists:member/2

  Own Id: OTP-15474 Aux Id: ERIERL-229

### Improvements and New Features

- When specified, the `+{source,Name}` option will now override the actual file
  name in stack traces, instead of only affecting the return value of
  `Mod:module_info()`.

  The `+deterministic` flag will also affect stack traces now, omitting all path
  information except the file name, fixing a long-standing issue where
  deterministic builds required deterministic paths.

  Own Id: OTP-15245 Aux Id: ERL-706

- List subtraction (The `--` operator) will now yield properly on large inputs.

  Own Id: OTP-15371

- `calendar:system_time_to_rfc3339/1,2` no longer remove trailing zeros from
  fractions.

  Own Id: OTP-15464

## STDLIB 3.6

### Fixed Bugs and Malfunctions

- The specs of `filename:basedir/2,3` are corrected.

  Own Id: OTP-15252 Aux Id: ERL-667

### Improvements and New Features

- Let `dets:open_file()` exit with a `badarg` message if given a raw file name
  (a binary).

  Own Id: OTP-15253 Aux Id: OTP-13229, ERL-55

- The `Format` argument of the formatting functions in modules `io` and `io_lib`
  is accepted even if it is, for example, a list of binaries. This is how it
  used to be before Erlang/OTP 21.0.

  Own Id: OTP-15304

## STDLIB 3.5.1

### Fixed Bugs and Malfunctions

- Fix a bug that could cause a crash when formatting a list of non-characters
  using the control sequences `p` or `P` and limiting the output with the option
  `chars_limit`.

  Own Id: OTP-15159

## STDLIB 3.5

### Fixed Bugs and Malfunctions

- `gen_statem` improvements.

  When using an exception that is valid but not allowed in a state enter call,
  the reason has been changed from `{bad_action_from_state_function,Action}` to
  `{bad_state_enter_action_from_state_function,Action}`.

  Timer parsing has been improved. Many erroneous timeout tuples was not handled
  correctly.

  The documentation has been updated, in particular the User's Guide and the
  pointer to it from the Reference Manual is much more obvious.

  Own Id: OTP-14015

- The type specifications for [`file:posix/0`](`t:file:posix/0`) and
  [`inet:posix/0`](`t:inet:posix/0`) have been updated according to which errors
  file and socket operations should be able to return.

  Own Id: OTP-14019 Aux Id: ERL-550

- File operations used to accept [filenames](`t:file:name_all/0`) containing
  null characters (integer value zero). This caused the name to be truncated and
  in some cases arguments to primitive operations to be mixed up. Filenames
  containing null characters inside the filename are now _rejected_ and will
  cause primitive file operations to fail.

  Also environment variable operations used to accept
  [names](`t:os:env_var_name/0`) and [values](`t:os:env_var_value/0`) of
  environment variables containing null characters (integer value zero). This
  caused operations to silently produce erroneous results. Environment variable
  names and values containing null characters inside the name or value are now
  _rejected_ and will cause environment variable operations to fail.

  Primitive environment variable operations also used to accept the `$=`
  character in environment variable names causing various problems. `$=`
  characters in environment variable names are now also _rejected_.

  Also `os:cmd/1` now reject null characters inside its
  [command](`t:os:os_command/0`).

  `erlang:open_port/2` will also reject null characters inside the port name
  from now on.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14543 Aux Id: ERL-370

- Make `io_lib:unscan_format/1` work with pad char and default precision.

  Own Id: OTP-14958 Aux Id: PR-1735

- The control sequence modifiers `t` and `l` can be used together in the same
  control sequence which makes it possible to have Unicode atoms and no
  detection of printable character lists at the same time.

  Own Id: OTP-14971 Aux Id: PR-1743

- Fix a bug in the Erlang code linter: the check of guard expressions no longer
  returns `false` if the map syntax is used. The bug affected the Erlang shell,
  the Debugger, and other modules evaluating abstract code.

  Own Id: OTP-15035 Aux Id: ERL-613

- A sys debug fun of type \{Fun,State\} should not be possible to install twice.
  This was, however, possible if the current State was 'undefined', which was
  mistaken for non-existing fun. This has been corrected.

  Own Id: OTP-15049

- Fix `io:putchars/2` stacktrace rewriting at errors to point to a valid
  function.

  Own Id: OTP-15101

### Improvements and New Features

- The `gen_server` has gotten a new callback `handle_continue/2` for check
  pointing the state. This is useful at least when implementing behaviours on
  top of `gen_server` and for some start up scenarios.

  Own Id: OTP-13019 Aux Id: PR-1490

- The semantics of timeout parameter `{clean_timeout,infinity}` to
  `gen_statem:call/3` has been changed to use a proxy process for the call. With
  this change `clean_timeout` implicates a proxy process with no exceptions.
  This may be a hard to observe incompatibility: in the presence of network
  problems a late reply could arrive in the caller's message queue when catching
  errors. That will not happen after this correction.

  The semantics of timeout parameter `infinity` has not been changed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13073 Aux Id: PR-1595

- A new logging API is added to Erlang/OTP, see the `m:logger` manual page, and
  section [Logging](`e:kernel:logger_chapter.md`) in the Kernel User's Guide.

  Calls to `error_logger` are automatically redirected to the new API, and
  legacy error logger event handlers can still be used. It is, however,
  recommended to use the Logger API directly when writing new code.

  Notice the following potential incompatibilities:

  - Kernel configuration parameters `error_logger` still works, but is overruled
    if the default handler's output destination is configured with Kernel
    configuration parameter `logger`.

    In general, parameters for configuring error logger are overwritten by new
    parameters for configuring Logger.

  - The concept of SASL error logging is deprecated, meaning that by default the
    SASL application does not affect which log events are logged.

    By default, supervisor reports and crash reports are logged by the default
    Logger handler started by Kernel, and end up at the same destination
    (terminal or file) as other standard log event from Erlang/OTP.

    Progress reports are not logged by default, but can be enabled by setting
    the primary log level to info, for example with the Kernel configuration
    parameter `logger_level`.

    To obtain backwards compatibility with the SASL error logging functionality
    from earlier releases, set Kernel configuration parameter
    `logger_sasl_compatible` to `true`. This prevents the default Logger handler
    from logging any supervisor-, crash-, or progress reports. Instead, SASL
    adds a separate Logger handler during application start, which takes care of
    these log events. The SASL configuration parameters `sasl_error_logger` and
    `sasl_errlog_type` specify the destination (terminal or file) and severity
    level to log for these events.

  Since Logger is new in Erlang/OTP 21.0, we do reserve the right to introduce
  changes to the Logger API and functionality in patches following this release.
  These changes might or might not be backwards compatible with the initial
  version.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13295

- Add functions `calendar:system_time_to_local_time/2` and
  `calendar:system_time_to_universal_time/2`.

  Own Id: OTP-13413

- Functions `rand:uniform_real/0` and `rand:uniform_real_s/1` have been added.
  They produce uniformly distributed numbers in the range `0.0 =< X < 1.0` that
  are as close to random real numbers as Normalized IEEE 754 Double Precision
  allows. Because the random real number exactly `0.0` is infinitely improbable
  they will never return exactly `0.0`.

  These properties are useful when you need to call for example `math:log(X)` or
  `1 / X` on a random value `X`, since that will never fail with a number from
  these new functions.

  Own Id: OTP-13764 Aux Id: PR-1574

- Added maps:iterator/0 and maps:next/1 to be used for iterating over the
  key-value associations in a map.

  Own Id: OTP-14012

- Changed the default behaviour of `.erlang` loading: `.erlang` is no longer
  loaded from the current directory. `c:erlangrc(PathList)` can be used to
  search and load an `.erlang` file from user specified directories.

  `escript`, `erlc`, `dialyzer` and `typer` no longer load an `.erlang` at all.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14439

- Added new uri_string module to stdlib for handling URIs (RFC 3986).

  Own Id: OTP-14496

- Update Unicode specification to version 10.0.

  Own Id: OTP-14503

- `filelib:wildcard()` now allows characters with a special meaning to be
  escaped using backslashes.

  This is an incompatible change, but note that the use of backslashes in
  wildcards would already work differently on Windows and Unix. Existing calls
  to `filelib:wildcard()` needs to be updated. On Windows, directory separators
  must always be written as a slash.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14577

- The supervisor now stores its child specifications in a map instead of a list.
  This causes a significant improvement when starting many children under a
  non-simple_one_for_one supervisor.

  Own Id: OTP-14586

- The `base64` module is optimized.

  Note that the functions `encode/1`, `decode/1`, and `mime_decode/1` fail
  unless called with an argument of the documented type. They used to accept any
  `t:iodata/0`.

  Own Id: OTP-14624 Aux Id: PR-1565

- Add function `lists:search/2`.

  Own Id: OTP-14675 Aux Id: PR-102

- uri_string module extended with functions for handling
  application/x-www-form-urlencoded query strings based on the HTML5
  specification.

  Own Id: OTP-14747

- Add functions `calendar:rfc3339_to_system_time/1,2` and
  `calendar:system_time_to_rfc3339/1,2`.

  Own Id: OTP-14764

- The stack traces returned by the functions of the `erl_eval` module more
  accurately reflect where the exception occurred.

  Own Id: OTP-14826 Aux Id: PR 1540

- Add options `atime`, `mtime`, `ctime`, `uid`, and `gid` to the
  `erl_tar:add/3,4` functions.

  Own Id: OTP-14834 Aux Id: PR 1608

- Added `ets:whereis/1` for retrieving the table identifier of a named table.

  Own Id: OTP-14884

- Improved URI normalization functions in the uri_string module.

  Own Id: OTP-14910

- The new functions `io_lib:fwrite/3` and `io_lib:format/3` take a third
  argument, an option list. The only option is `chars_limit`, which is used for
  limiting the number of returned characters. The limit is soft, which means
  that the number of returned characters exceeds the limit with at most a
  smallish amount. If the limit is set, the functions `format/3` and `fwrite/3`
  try to distribute the number of characters evenly over the control sequences
  `pPswW`. Furthermore, the control sequences `pPwP` try to distribute the
  number of characters evenly over substructures.

  A modification of the control sequences `pPwW` is that even if there is no
  limit on the number of returned characters, all associations of a map are
  printed to the same depth. The aim is to give a more consistent output as the
  order of map keys is not defined. As before, if the depth is less than the
  number of associations of a map, the selection of associations to print is
  arbitrary.

  Own Id: OTP-14983

- Add functions `ordsets:is_empty/1` and `sets:is_empty/1`.

  Own Id: OTP-14996 Aux Id: ERL-557, PR-1703

- Improve performance of `string:uppercase/1`, `string:lowercase/1` and
  `string:casefold/1` when handling ASCII characters.

  Own Id: OTP-14998

- External funs with literal values for module, name, and arity (e.g.
  `erlang:abs/1`) are now treated as literals. That means more efficient code
  that produces less garbage on the heap.

  Own Id: OTP-15003

- sys:statistics(Pid,get) did not report 'out' messages from gen_server. This is
  now corrected.

  Own Id: OTP-15047

- A sys debug function can now have the format \{Id,Fun,State\} in addition to
  the old \{Fun,State\}. This allows installing multiple instances of a debug
  fun.

  Own Id: OTP-15048

- The `lib` module is removed:

  - `lib:error_message/2` is removed.
  - `lib:flush_receive/0` is removed.
  - `lib:nonl/1` is removed.
  - `lib:progname/0` is replaced by `ct:get_progname/0`.
  - `lib:send/2` is removed.
  - `lib:sendw/2` is removed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15072 Aux Id: PR 1786, OTP-15114

- Function `ets:delete_all_objects/1` now yields the scheduler thread for large
  tables that take significant time to clear. This to improve real time
  characteristics of other runnable processes.

  Own Id: OTP-15078

- In control sequences of the functions `io:fwrite/2,3` and `io_lib:fwrite/2,3`
  containing `p` or `P`, a field width of value `0` means that no line breaks
  are inserted. This is in contrast to the old behaviour, where `0` used to
  insert line breaks after every subterm. To insert line breaks after every
  subterm, a field width of value `1` can be used.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15103 Aux Id: ERL-607

## STDLIB 3.4.5.1

### Improvements and New Features

- List subtraction (The `--` operator) will now yield properly on large inputs.

  Own Id: OTP-15371

## STDLIB 3.4.5

### Fixed Bugs and Malfunctions

- The `Module:init/1` function in `gen_statem` may return an actions list
  containing any action, but an erroneous check only allowed state enter actions
  so e.g `{next_event,internal,event}` caused a server crash. This bug has been
  fixed.

  Own Id: OTP-13995

## STDLIB 3.4.4

### Fixed Bugs and Malfunctions

- Correct `filelib:find_source()` and `filelib:find_file()` to by default also
  search one level below `src`. This is in accordance with the Design Principles
  which states that an application can have Erlang source files one level below
  the `src` directory.

  Own Id: OTP-14832 Aux Id: ERL-527

- The contract of `erl_tar:table/2` is corrected.

  Own Id: OTP-14860 Aux Id: PR 1670

- Correct a few contracts.

  Own Id: OTP-14889

- Fix string:prefix/2 to handle an empty string as second argument.

  Own Id: OTP-14942 Aux Id: PR-1702

## STDLIB 3.4.3

### Fixed Bugs and Malfunctions

- Make `ets:i/1` exit cleaner when ^D is input while browsing a table. Only the
  old Erlang shell is affected ([erl](`e:erts:erl_cmd.md`) flag `-oldshell`).

  Own Id: OTP-14663

- Fixed handling of windows UNC paths in module `filename`.

  Own Id: OTP-14693

### Improvements and New Features

- Improve performance of the new string functionality when handling ASCII
  characters.

  Own Id: OTP-14670

- Added a clarification to the documentation of `unicode:characters_to_list/2`.

  Own Id: OTP-14798

## STDLIB 3.4.2

### Fixed Bugs and Malfunctions

- Fix a bug in the Erlang shell where recursively defined records with typed
  fields could cause a loop.

  Own Id: OTP-14488 Aux Id: PR-1489

- Make edlin handle grapheme clusters instead of codepoints to improve the
  handling multi-codepoints characters.

  Own Id: OTP-14542

- There could be false warnings for `erlang:get_stacktrace/0` being used outside
  of a `try` block when using multiple `catch` clauses.

  Own Id: OTP-14600 Aux Id: ERL-478

### Improvements and New Features

- The Erlang code linter no longer checks that the functions mentioned in
  `nowarn_deprecated_function` options are declared in the module.

  Own Id: OTP-14378

- General Unicode improvements.

  Own Id: OTP-14462

## STDLIB 3.4.1

### Fixed Bugs and Malfunctions

- A bug in `proc_lib:format()` introduced in Erlang/OTP 20.0 is corrected.

  Own Id: OTP-14482 Aux Id: PR-1488

- Fix string:len/1 to be compatible with previous versions.

  Own Id: OTP-14487 Aux Id: ERIERL-40

- In OTP-20.0, the behavior of c, make, and ct_make was changed so that in some
  cases the beam files by default would be written to the directory where the
  source files were found. This is now changed back to the old behavior so beam
  files are by default written to current directory.

  Own Id: OTP-14489 Aux Id: ERL-438

## STDLIB 3.4

### Fixed Bugs and Malfunctions

- For many releases, it has been legal to override a BIF with a local function
  having the same name. However, calling a local function with the same name as
  guard BIF as filter in a list comprehension was not allowed.

  Own Id: OTP-13690

- A new (default) pseudo-random number generator algorithm Xoroshiro116+ has
  been implemented in the `rand` module.

  The old algorithm implementations had a number of flaws so they are all
  deprecated, but corrected versions of two of them have been added. See the
  documentation.

  Own Id: OTP-14295 Aux Id: PR-1372

- The Erlang shell, `qlc:string_to_handle()`, and the Debugger (the Evaluator
  area and Edit variable window of the Bindings area) can parse pids, ports,
  references, and external funs, as long as they can be created in the running
  system.

  Own Id: OTP-14296

- Internal code change: Calls to `catch` followed by a call to
  `erlang:get_stacktrace/0` has been rewritten to use `try` instead of `catch`
  to make the code future-proof.

  Own Id: OTP-14400

- The `ms_transform` module, used by `ets:fun2ms/1` and `dbg:fun2ms/1`,
  evaluates constant arithmetic expressions. This is necessary since the Erlang
  compiler, which normally evaluates constant expressions, does not recognize
  the format generated by `ms_transform`.

  Own Id: OTP-14454 Aux Id: ERIERL-29

- The state machine engine `gen_statem` can now handle generic time-outs
  (multiple named) as well as absolute time-out time. See the documentation.

  The `gen_statem` callback `Module:init/1` has become mandatory to harmonize
  with other `gen_*` modules. This may be an incompatibility for `gen_statem`
  callback modules that use `gen_statem:enter_loop/4-6`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14531

### Improvements and New Features

- Improved unicode support for strings. Added normalization functions in the
  `unicode` module. Extended the `string` module API with new functions with
  improved unicode handling and that works on grapheme clusters. The new
  functions operates on the [`unicode:chardata()`](`t:unicode:chardata/0`) type,
  thus they also accept `UTF-8 binaries` as input.

  The old string API have been marked as obsolete. The return values have been
  changed for some error cases.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-10289 Aux Id: OTP-10309

- There are two new guard BIFs '[`floor/1`](`floor/1`)' and
  '[`ceil/1`](`ceil/1`)'. They both return integers. In the '`math`' module,
  there are two new BIFs with the same names that return floating point values.

  Own Id: OTP-13692

- Making code_change, terminate and handle_info callbacks optional in the OTP
  behaviours.

  Own Id: OTP-13801

- The support for Dets files created with Erlang/OTP R7 and earlier is removed.

  Own Id: OTP-13830

- Replaced usage of deprecated symbolic [`time unit`](`t:erlang:time_unit/0`)
  representations.

  Own Id: OTP-13831 Aux Id: OTP-13735

- The function `fmod/2` has been added to the `math` module.

  Own Id: OTP-14000

- The EXIT signals received from processes using `proc_lib` now looks like EXIT
  signals from processes that were spawned using `spawn_link`. In particular,
  that means that the stack trace is now included in the EXIT signal so that it
  can see where the process crashed.

  Own Id: OTP-14001

- `sets:add_element/2` is faster when adding an element that is already present,
  and `sets:del_element/2` is faster when the element to be deleted is not
  present. This optimization can make certain operations, such as sets:union/2
  with many overlapping elements, up to two orders of magnitude faster.

  Own Id: OTP-14035

- Add information in doc about supervisor shutdown reason when maximum restart
  frequency is reached.

  Own Id: OTP-14037 Aux Id: PR-1233

- Added `rand:jump/[0|1]` functions.

  Own Id: OTP-14038 Aux Id: PR-1235

- Functions for detecting changed code has been added. `code:modified_modules/0`
  returns all currently loaded modules that have changed on disk.
  `code:module_status/1` returns the status for a module. In the shell and in
  `c` module, `mm/0` is short for `code:modified_modules/0`, and `lm/0` reloads
  all currently loaded modules that have changed on disk.

  Own Id: OTP-14059

- Each assert macro in `assert.hrl` now has a corresponding version with an
  extra argument, for adding comments to assertions. These can for example be
  printed as part of error reports, to clarify the meaning of the check that
  failed.

  Own Id: OTP-14066

- `error_logger_tty_h` and `error_logger_file_h` now inserts the node
  information for nonlocal messages before the message itself instead of after,
  both for readability and so as not to change the line termination property at
  the end of the message.

  Own Id: OTP-14068

- The Erlang code linter checks for badly formed type constraints.

  Own Id: OTP-14070 Aux Id: PR-1214

- By default, there will now be a warning when `export_all` is used. The warning
  can be disabled using `nowarn_export_all`.

  Own Id: OTP-14071

- When a `gen_server` process crashes, the stacktrace for the client will be
  printed to facilitate debugging.

  Own Id: OTP-14089

- Optimized ETS operations by changing table identifier type from integer to
  reference. The reference enables a more direct mapping to the table with less
  potential lock contention and makes especially creation and deletion of tables
  scale much better.

  The change of the opaque type for the ETS table identifiers may cause failure
  in code that make faulty assumptions about this opaque type.

  > #### Note {: .info }
  >
  > The number of tables stored at one Erlang node _used_ to be limited. This is
  > no longer the case (except by memory usage). The previous default limit was
  > about 1400 tables and could be increased by setting the environment variable
  > `ERL_MAX_ETS_TABLES` before starting the Erlang runtime system. This hard
  > limit has been removed, but it is currently useful to set the
  > `ERL_MAX_ETS_TABLES` anyway. It should be set to an approximate of the
  > maximum amount of tables used. This since an internal table for named tables
  > is sized using this value. If large amounts of named tables are used and
  > `ERL_MAX_ETS_TABLES` hasn't been increased, the performance of named table
  > lookup will degrade.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14094

- `take/2` has been added to `dict`, `orddict`, and `gb_trees`. `take_any/2` has
  been added to `gb_trees`.

  Own Id: OTP-14102

- Extend gen_event API to handle options as well.

  Own Id: OTP-14123

- Advice on how to tune the supervisor restart frequency (intensity and period)
  is added to System Documentation - Design Principles - Supervisor Behaviour.

  Own Id: OTP-14168 Aux Id: PR-1289

- gen_fsm is deprecated and is replaced by gen_statem, however for backwards
  compatibility reasons gen_fsm may continue to exist as an undocumented feature
  for quite some time.

  Own Id: OTP-14183

- The shell functions `c/1` and `c/2` have been extended so that if the argument
  is a module name instead of a file name, it automatically locates the .beam
  file and the corresponding source file, and then recompiles the module using
  the same compiler options (plus any options passed to c/2). If compilation
  fails, the old beam file is preserved. Also adds `c(Mod, Opts, Filter)`, where
  the Filter argument allows you to remove old compiler options before the new
  options are added.

  New utility functions `file_find/2/3` and `find_source/1/2/3` have been added
  to `filelib`.

  Own Id: OTP-14190

- `erl_tar` in previous versions of OTP only supports the USTAR format. That
  limited path names to at most 255 bytes, and did not support Unicode
  characters in names in a portable way.

  `erl_tar` now has support for reading tar archives in the formats currently in
  common use, such as v7, STAR, USTAR, PAX, and GNU tar's extensions to the
  STAR/USTAR format. When writing tar archives, `erl_tar` can now write them in
  the `PAX` format if necessary (for example, to support very long filenames or
  filenames with Unicode characters). If possible, `erl_tar` will still write
  tar archives in the USTAR for maximum portability.

  Own Id: OTP-14226

- `base64:mime_decode/1` has been optimized so that it is now almost as fast
  as`base64:decode/1`; it used be noticeably slower.

  Own Id: OTP-14245

- `erl_tar` will now strip any leading '`/`' from pathnames when extracting
  files from a tar archive and write a message to the error logger. There is
  also new check for directory traversal attacks; if a relative path points
  above the current working directory the extraction will be aborted.

  Own Id: OTP-14278

- Miscellaneous updates due to atoms containing arbitrary Unicode characters.

  Own Id: OTP-14285

- The Crypto application now supports generation of cryptographically strong
  random numbers (floats < 1.0 and integer arbitrary ranges) as a plugin to the
  'rand' module.

  Own Id: OTP-14317 Aux Id: PR-1372

- Add new function `ets:select_replace/2` which performs atomic
  "compare-and-swap" operations for ETS objects using match specifications.

  Own Id: OTP-14319 Aux Id: PR-1076

- The Erlang code linter checks for bad `dialyzer` attributes. It also checks
  for bad type variables in type declarations.

  Own Id: OTP-14323

- Two new functions has been implemented in the `rand` module; `normal/2` and
  `normal_s/3`, that both produce normal distribution (pseudo) random numbers
  with mean value and variance according to arguments.

  Own Id: OTP-14328 Aux Id: PR-1382

- Upgraded the OTP internal PCRE library from version 8.33 to version 8.40. This
  library is used for implementation of the `m:re` regular expressions module.

  Besides various bug fixes, the new version allows for better stack protection.
  In order to utilize this feature, the stack size of normal scheduler threads
  is now by default set to 128 kilo words on all platforms. The stack size of
  normal scheduler threads can be set upon system start by passing the
  [`+sss`](`e:erts:erl_cmd.md#sched_thread_stack_size`) command line argument to
  the [`erl`](`e:erts:erl_cmd.md`) command.

  See
  [http://pcre.org/original/changelog.txt](http://pcre.org/original/changelog.txt)
  for information about changes made to PCRE between the versions 8.33 and 8.40.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14331 Aux Id: ERL-208

- Added function `re:version/0` which returns information about the OTP internal
  PCRE version used for implementation of the `re` module.

  Own Id: OTP-14347 Aux Id: PR-1412

- The format of debug information that is stored in BEAM files (when
  `debug_info` is used) has been changed. The purpose of the change is to better
  support other BEAM-based languages such as Elixir or LFE.

  All tools included in OTP (dialyzer, debugger, cover, and so on) will handle
  both the new format and the previous format. Tools that retrieve the debug
  information using `beam_lib:chunk(Beam, [abstract_code])` will continue to
  work with both the new and old format. Tools that call
  `beam_lib:chunk(Beam, ["Abst"])` will not work with the new format.

  For more information, see the description of `debug_info` in the documentation
  for `beam_lib` and the description of the `{debug_info,{Backend,Data}}` option
  in the documentation for `compile`.

  Own Id: OTP-14369 Aux Id: PR-1367

- Add option hibernate_after to gen_server, gen_statem and gen_event. Also added
  to the deprecated gen_fsm behaviour.

  Own Id: OTP-14405

- The size of crash reports created by `gen_server`, `gen_statem` and `proc_lib`
  is limited with aid of the Kernel application variable
  `error_logger_format_depth`. The purpose is to limit the size of the messages
  sent to the `error_logger` process when processes with huge message queues or
  states crash.

  The crash report generated by `proc_lib` includes the new tag
  `message_queue_len`. The neighbour report also includes the new tag
  `current_stacktrace`. Finally, the neighbour report no longer includes the
  tags `messages` and `dictionary`.

  The new function `error_logger:get_format_depth/0` can be used to retrieve the
  value of the Kernel application variable `error_logger_format_depth`.

  Own Id: OTP-14417

## STDLIB 3.3

### Fixed Bugs and Malfunctions

- An escript with only two lines would not work.

  Own Id: OTP-14098

- Characters (`$char`) can be used in constant pattern expressions. They can
  also be used in types and contracts.

  Own Id: OTP-14103 Aux Id: ERL-313

- The signatures of `erl_parse:anno_to_term/1` and `erl_parse:anno_from_term/1`
  are corrected. Using these functions no longer results in false Dialyzer
  warnings.

  Own Id: OTP-14131

- Pretty-printing of maps is improved.

  Own Id: OTP-14175 Aux Id: seq13277

- If any of the following functions in the `zip` module crashed, a file would be
  left open: `extract()`, `unzip()`, `create()`, or `zip()`. This has been
  corrected.

  A `zip` file having a "Unix header" could not be unpacked.

  Own Id: OTP-14189 Aux Id: ERL-348, ERL-349

- Improve the Erlang shell's tab-completion of long names.

  Own Id: OTP-14200 Aux Id: ERL-352

- The reference manual for `sys` had some faulty information about the
  'get_modules' message used by processes where modules change dynamically
  during runtime. The documentation is now corrected.

  Own Id: OTP-14248 Aux Id: ERL-367

### Improvements and New Features

- Bug fixes, new features and improvements to gen_statem:

  A new type init_result/1 has replaced the old init_result/0, so if you used
  that old type (that was never documented) you have to change your code, which
  may be regarded as a potential incompatibility.

  Changing callback modes after code change did not work since the new callback
  mode was not recorded. This bug has been fixed.

  The event types state_timeout and \{call,From\} could not be generated with a
  \{next_event,EventType,EventContent\} action since they did not pass the
  runtime type check. This bug has now been corrected.

  State entry calls can now be repeated using (new) state callback returns
  \{repeat*state,...\}, \{repeat_state_and_data,*\} and repeat_state_and_data.

  There have been lots of code cleanup in particular regarding timer handling.
  For example is async cancel_timer now used. Error handling has also been
  cleaned up.

  To align with probable future changes to the rest of gen\_\*, terminate/3 has
  now got a fallback and code_change/4 is not mandatory.

  Own Id: OTP-14114

- `filename:safe_relative_path/1` to sanitize a relative path has been added.

  Own Id: OTP-14215

## STDLIB 3.2

### Fixed Bugs and Malfunctions

- When a simple_one_for_one supervisor is shutting down, and a child exits with
  an exit reason of the form \{shutdown, Term\}, an error report was earlier
  printed. This is now corrected.

  Own Id: OTP-13907 Aux Id: PR-1158, ERL-163

- Allow empty list as parameter of the fun used with `dbg:fun2ms/1`.

  Own Id: OTP-13974

### Improvements and New Features

- The new behaviour gen_statem has been improved with 3 new features: the
  possibility to use old style non-proxy timeouts for gen_statem:call/2,3, state
  entry code, and state timeouts. These are backwards compatible. Minor code and
  documentation improvements has been performed including a borderline semantics
  correction of timeout zero handling.

  Own Id: OTP-13929 Aux Id: PR-1170, ERL-284

## STDLIB 3.1

### Fixed Bugs and Malfunctions

- The `zip:unzip/1,2` and `zip:extract/1,2` functions have been updated to
  handle directory traversal exploits. Any element in the zip file that contains
  a path that points to a directory above the top level working directory,
  `cwd`, will instead be extracted in `cwd`. An error message is printed for any
  such element in the zip file during the unzip operation. The `keep_old_files`
  option determines if a file will overwrite a previous file with the same name
  within the zip file.

  Own Id: OTP-13633

- Correct the contracts for `ets:match_object/1,3`.

  Own Id: OTP-13721 Aux Id: PR-1113

- Errors in type specification and Emacs template generation for
  `gen_statem:code_change/4` has been fixed from bugs.erlang.org's Jira cases
  ERL-172 and ERL-187.

  Own Id: OTP-13746 Aux Id: ERL-172, ERL-187

### Improvements and New Features

- gen_statem has been changed to set the callback mode for a server to what
  Module:callback_mode/0 returns. This facilitates e.g code downgrade since the
  callback mode now becomes a property of the currently active code, not of the
  server process.

  Exception handling from Module:init/1 has also been improved.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13752

## STDLIB 3.0.1

### Fixed Bugs and Malfunctions

- Correct a bug regarding typed records in the Erlang shell. The bug was
  introduced in OTP-19.0.

  Own Id: OTP-13719 Aux Id: ERL-182

## STDLIB 3.0

### Fixed Bugs and Malfunctions

- Fix a race bug affecting `dets:open_file/2`.

  Own Id: OTP-13260 Aux Id: seq13002

- Don't search for non-existing Map keys twice

  For `maps:get/2,3` and `maps:find/2`, searching for an immediate key, e.g. an
  atom, in a small map, the search was performed twice if the key did not exist.

  Own Id: OTP-13459

- Avoid stray corner-case math errors on Solaris, e.g. an error is thrown on
  underflows in exp() and pow() when it shouldn't be.

  Own Id: OTP-13531

- Fix linting of map key variables

  Map keys cannot be unbound and then used in parallel matching.

  Example: `#{ K := V } = #{ k := K } = M.` This is illegal if `'K'` is not
  bound.

  Own Id: OTP-13534 Aux Id: ERL-135

- Fixed a bug in re on openbsd where sometimes re:run would return an incorrect
  result.

  Own Id: OTP-13602

- To avoid potential timer bottleneck on supervisor restart, timer server is no
  longer used when the supervisor is unable to restart a child.

  Own Id: OTP-13618 Aux Id: PR-1001

- The Erlang code preprocessor (`epp`) can handle file names spanning over many
  tokens. Example: `-include("a" "file" "name").`.

  Own Id: OTP-13662 Aux Id: seq13136

### Improvements and New Features

- The types of The Abstract Format in the `erl_parse` module have been refined.

  Own Id: OTP-10292

- Undocumented syntax for function specifications,
  `-spec F/A :: Domain -> Range`, has been removed (without deprecation).

  Using the `is_subtype(V, T)` syntax for constraints (in function
  specifications) is no longer documented, and the newer syntax `V :: T` should
  be used instead. The Erlang Parser still recognizes the `is_subtype` syntax,
  and will continue to do so for some time.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11879

- The '`random`' module has been deprecated. Use the '`rand`' module instead.

  Own Id: OTP-12502 Aux Id: OTP-12501

- Background: In record fields with a type declaration but without an
  initializer, the Erlang parser inserted automatically the singleton type
  `'undefined'` to the list of declared types, if that value was not present
  there. That is, the record declaration:

  \-record(rec, \{f1 :: float(), f2 = 42 :: integer(), f3 ::
  some_mod:some_typ()\}).

  was translated by the parser to:

  \-record(rec, \{f1 :: float() | 'undefined', f2 = 42 :: integer(), f3 ::
  some_mod:some_typ() | 'undefined'\}).

  The rationale for this was that creation of a "dummy" `#rec{}` record should
  not result in a warning from dialyzer that, for example, the implicit
  initialization of the `#rec.f1` field violates its type declaration.

  Problems: This seemingly innocent action has some unforeseen consequences.

  For starters, there is no way for programmers to declare that e.g. only floats
  make sense for the `f1` field of `#rec{}` records when there is no "obvious"
  default initializer for this field. (This also affects tools like PropEr that
  use these declarations produced by the Erlang parser to generate random
  instances of records for testing purposes.)

  It also means that dialyzer does not warn if e.g. an
  [`is_atom/1`](`is_atom/1`) test or something more exotic like an
  [`atom_to_list/1`](`atom_to_list/1`) call is performed on the value of the
  `f1` field.

  Similarly, there is no way to extend dialyzer to warn if it finds record
  constructions where `f1` is not initialized to some float.

  Last but not least, it is semantically problematic when the type of the field
  is an opaque type: creating a union of an opaque and a structured type is very
  problematic for analysis because it fundamentally breaks the opacity of the
  term at that point.

  Change: To solve these problems the parser will not automatically insert the
  `'undefined'` value anymore; instead the user has the option to choose the
  places where this value makes sense (for the field) and where it does not and
  insert the `| 'undefined'` there manually.

  Consequences of this change: This change means that dialyzer will issue a
  warning for all places where records with uninitialized fields are created and
  those fields have a declared type that is incompatible with `'undefined'`
  (e.g. `t:float/0`). This warning can be suppressed easily by adding
  `| 'undefined'` to the type of this field. This also adds documentation that
  the user really intends to create records where this field is uninitialized.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12719

- Remove deprecated functions in the modules `erl_scan` and `erl_parse`.

  Own Id: OTP-12861

- The pre-processor can now expand the ?FUNCTION_NAME and ?FUNCTION_ARITY
  macros.

  Own Id: OTP-13059

- A new behaviour `gen_statem` has been implemented. It has been thoroughly
  reviewed, is stable enough to be used by at least two heavy OTP applications,
  and is here to stay. But depending on user feedback, we do not expect but
  might find it necessary to make minor not backwards compatible changes into
  OTP-20.0, so its state can be designated as "not quite experimental"...

  The `gen_statem` behaviour is intended to replace `gen_fsm` for new code. It
  has the same features and add some really useful:

  - State code is gathered
  - The state can be any term
  - Events can be postponed
  - Events can be self generated
  - A reply can be sent from a later state
  - There can be multiple sys traceable replies

  The callback model(s) for `gen_statem` differs from the one for `gen_fsm`, but
  it is still fairly easy to rewrite from `gen_fsm` to `gen_statem`.

  Own Id: OTP-13065 Aux Id: PR-960

- Optimize binary:split/2 and binary:split/3 with native BIF implementation.

  Own Id: OTP-13082

- Background: The types of record fields have since R12B been put in a separate
  form by `epp:parse_file()`, leaving the record declaration form untyped. The
  separate form, however, does not follow the syntax of type declarations, and
  parse transforms inspecting `-type()` attributes need to know about the
  special syntax. Since the compiler stores the return value of
  `epp:parse_file()` as debug information in the abstract code chunk (`"Abst"`
  or `abstract_code`), tools too need to know about the special syntax, if they
  inspect `-type()` attributes in abstract code.

  Change: No separate type form is created by `epp:parse_file()`, but the type
  information is kept in the record fields. This means that all parse transforms
  and all tools inspecting `-record()` declarations need to recognize
  `{typed_record_field, Field, Type}`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13148

- Unsized fields of the type `bytes` in binary generators are now forbidden.
  (The other ways of writing unsized fields, such as `binary`, are already
  forbidden.)

  Own Id: OTP-13152

- The type `t:map/0` is built-in, and cannot be redefined.

  Own Id: OTP-13153

- Let `dets:open_file()` exit with a `badarg` message if given a raw file name
  (a binary).

  Own Id: OTP-13229 Aux Id: ERL-55

- Add `filename:basedir/2,3`

  basedir returns suitable path(s) for 'user_cache', 'user_config', 'user_data',
  'user_log', 'site_config' and 'site_data'. On linux and linux like systems the
  paths will respect the XDG environment variables.

  Own Id: OTP-13392

- There are new preprocessor directives `-error(Term)` and `-warning(Term)` to
  cause a compilation error or a compilation warning, respectively.

  Own Id: OTP-13476

- Optimize `'++'` operator and `lists:append/2` by using a single pass to build
  a new list while checking for properness.

  Own Id: OTP-13487

- Add `maps:update_with/3,4` and `maps:take/2`

  Own Id: OTP-13522 Aux Id: PR-1025

- `lists:join/2` has been added. Similar to `string:join/2` but works with
  arbitrary lists.

  Own Id: OTP-13523

- Obfuscate asserts to make Dialyzer shut up.

  Own Id: OTP-13524 Aux Id: PR-1002

- Supervisors now explicitly add their callback module in the return from
  sys:get_status/1,2. This is to simplify custom supervisor implementations. The
  Misc part of the return value from sys:get_status/1,2 for a supervisor is now:

  \[\{data, [\{"State", State\}]\},\{supervisor,\[\{"Callback",Module\}]\}]

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13619 Aux Id: PR-1000

- Relax translation of initial calls in `proc_lib`, i.e. remove the restriction
  to only do the translation for `gen_server` and `gen_fsm`. This enables user
  defined `gen` based generic callback modules to be displayed nicely in `c:i()`
  and observer.

  Own Id: OTP-13623

- The function `queue:lait/1` (misspelling of `liat/1`) is now deprecated.

  Own Id: OTP-13658

## STDLIB 2.8.0.1

### Improvements and New Features

- List subtraction (The `--` operator) will now yield properly on large inputs.

  Own Id: OTP-15371

## STDLIB 2.8

### Fixed Bugs and Malfunctions

- Fix evaluation in matching of bound map key variables in the interpreter.

  Prior to this patch, the following code would not evaluate:
  `X = key,(fun(#{X := value}) -> true end)(#{X => value})`

  Own Id: OTP-13218

- Fix `erl_eval` not using non-local function handler.

  Own Id: OTP-13228 Aux Id: ERL-32

- The Erlang Code Linter no longer crashes if there is a `-deprecated()`
  attribute but no `-module()` declaration.

  Own Id: OTP-13230 Aux Id: ERL-62

- The timestamp in the result returned by `dets:info(Tab, safe_fixed)` was
  unintentionally broken as a result of the time API rewrites in OTP 18.0. This
  has now been fixed.

  Own Id: OTP-13239 Aux Id: OTP-11997

- A rare race condition in `beam_lib` when using encrypted abstract format has
  been eliminated.

  Own Id: OTP-13278

- Improved maps:with/2 and maps:without/2 algorithms

  The new implementation speeds up the execution significantly for all sizes of
  input.

  Own Id: OTP-13376

### Improvements and New Features

- Time warp safety improvements.

  Introduced the options `monotonic_timestamp`, and `strict_monotonic_timestamp`
  to the trace, sequential trace, and system profile functionality. This since
  the already existing `timestamp` option is not time warp safe.

  Introduced the option `safe_fixed_monotonic_time` to `ets:info/2` and
  `dets:info/2`. This since the already existing `safe_fixed` option is not time
  warp safe.

  Own Id: OTP-13222 Aux Id: OTP-11997

- In the shell Ctrl+W (delete word) will no longer consider "." as being part of
  a word.

  Own Id: OTP-13281

## STDLIB 2.7

### Fixed Bugs and Malfunctions

- The Erlang Pretty Printer uses `::` for function type constraints.

  A bug concerning pretty printing of annotated type union elements in map pair
  types has been fixed.

  Some minor issues regarding the documentation of types and specs have been
  corrected.

  Own Id: OTP-13084

- The shell command `rp` prints strings as lists of integers if pretty printing
  of lists is set to `false`.

  Own Id: OTP-13145

- The shell would crash if a bit syntax expression with conflicting types were
  given (e.g. if a field type was given as '`integer-binary`'). (Thanks to
  Aleksei Magusev for reporting this bug.)

  Own Id: OTP-13157

- The `rand:export_seed/0` would never return '`undefined`' even if no seed has
  previously been created. Fixed to return '`undefined`' if there is no seed in
  the process dictionary.

  Own Id: OTP-13162

### Improvements and New Features

- Add support for the Delete, Home and End keys in the Erlang shell.

  Own Id: OTP-13032

- `beam_lib:all_chunks/1` and `beam_lib:build_module/1` have been documented.

  Own Id: OTP-13063

## STDLIB 2.6

### Fixed Bugs and Malfunctions

- In OTP 18.0, `qlc` does not handle syntax errors well. This bug has been
  fixed.

  Own Id: OTP-12946

- Optimize zip:unzip/2 when uncompressing to memory.

  Own Id: OTP-12950

- The STDLIB reference manual is updated to show correct information about the
  return value of `gen_fsm:reply/2`.

  Own Id: OTP-12973

- re:split2,3 and re:replace/3,4 now correctly handles pre-compiled patterns
  that have been compiled using the '`unicode`' option.

  Own Id: OTP-12977

- Export `shell:catch_exception/1` as documented.

  Own Id: OTP-12990

### Improvements and New Features

- A mechanism for limiting the amount of text that the built-in error logger
  events will produce has been introduced. It is useful for limiting both the
  size of log files and the CPU time used to produce them.

  This mechanism is experimental in the sense that it may be changed if it turns
  out that it does not solve the problem it is supposed to solve. In that case,
  there may be backward incompatible improvements to this mechanism.

  See the documentation for the config parameter `error_logger_format_depth` in
  the Kernel application for information about how to turn on this feature.

  Own Id: OTP-12864

## STDLIB 2.5

### Fixed Bugs and Malfunctions

- Fix handling of single dot in filename:join/2

  The reference manual says that filename:join(A,B) is equivalent to
  filename:join(\[A,B]). In some rare cases this turns out not to be true. For
  example:

  `filename:join("/a/.","b") -> "/a/./b"` vs
  `filename:join(["/a/.","b"]) -> "/a/b"`.

  This has been corrected. A single dot is now only kept if it occurs at the
  very beginning or the very end of the resulting path.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12158

- The undocumented option `generic_debug` for `gen_server` has been removed.

  Own Id: OTP-12183

- erl_lint:icrt_export/4 has been rewritten to make the code really follow the
  scoping rules of Erlang, and not just in most situations by accident.

  Own Id: OTP-12186

- Add 'trim_all' option to binary:split/3

  This option can be set to remove \_ALL\_ empty parts of the result of a call
  to binary:split/3.

  Own Id: OTP-12301

- Correct orddict(3) regarding evaluation order of `fold()` and `t:map/0`.

  Own Id: OTP-12651 Aux Id: seq12832

- Correct `maps` module error exceptions

  Bad input to maps module function will now yield the following exceptions:

  - \{badmap, NotMap\}, or
  - badarg.

  Own Id: OTP-12657

- It is now possible to paste text in JCL mode (using Ctrl-Y) that has been
  copied in the previous shell session. Also a bug that caused the JCL mode to
  crash when pasting text has been fixed.

  Own Id: OTP-12673

- Add `uptime()` shell command.

  Own Id: OTP-12752

- Cache nowarn_bif_clash functions in erl_lint.

  This patch stores nowarn_bif_clash in the lint record. By using erlc
  +'\{eprof,lint_module\}' when compiling the erlang parser, we noticed the time
  spent on nowarn_function/2 reduced from 30% to 0.01%.

  Own Id: OTP-12754

- Optimize the Erlang Code Linter by using the cached filename information.

  Own Id: OTP-12772

- If a child of a simple_one_for_one returns ignore from its start function no
  longer store the child for any restart type. It is not possible to restart or
  delete the child because the supervisor is a simple_one_for_one.

  Own Id: OTP-12793

- Make `ets:file2tab` preserve enabled `read_concurrency` and
  `write_concurrency` options for tables.

  Own Id: OTP-12814

- There are many cases where user code needs to be able to distinguish between a
  socket that was closed normally and one that was aborted. Setting the option
  \{show_econnreset, true\} enables the user to receive ECONNRESET errors on
  both active and passive sockets.

  Own Id: OTP-12841

### Improvements and New Features

- Allow maps for supervisor flags and child specs

  Earlier, supervisor flags and child specs were given as tuples. While this is
  kept for backwards compatibility, it is now also allowed to give these
  parameters as maps, see [sup_flags](`m:supervisor#sup_flags`) and
  [child_spec](`m:supervisor#child_spec`).

  Own Id: OTP-11043

- A new system message, `terminate`, is added. This can be sent with
  `sys:terminate/2,3`. If the receiving process handles system messages properly
  it will terminate shortly after receiving this message.

  The new function `proc_lib:stop/1,3` utilizes this new system message and
  monitors the receiving process in order to facilitate a synchronous stop
  mechanism for 'special processes'.

  `proc_lib:stop/1,3` is used by the following functions:

  - `gen_server:stop/1,3` (new)
  - `gen_fsm:stop/1,3` (new)
  - `gen_event:stop/1,3` (modified to be synchronous)
  - `wx_object:stop/1,3` (new)

  Own Id: OTP-11173 Aux Id: seq12353

- Remove the `pg` module, which has been deprecated through OTP-17, is now
  removed from the STDLIB application. This module has been marked experimental
  for more than 15 years, and has largely been superseded by the `pg2` module
  from the Kernel application.

  Own Id: OTP-11907

- New BIF: `erlang:get_keys/0`, lists all keys associated with the process
  dictionary. Note: `erlang:get_keys/0` is auto-imported.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12151 Aux Id: seq12521

- Add three new functions to `io_lib`\-- `scan_format/2`, `unscan_format/1`, and
  `build_text/1`\-- which expose the parsed form of the format control sequences
  to make it possible to easily modify or filter the input to `io_lib:format/2`.
  This can e.g. be used in order to replace unbounded-size control sequences
  like `~w` or `~p` with corresponding depth-limited `~W` and `~P` before doing
  the actual formatting.

  Own Id: OTP-12167

- Introduce the `erl_anno` module, an abstraction of the second element of
  tokens and tuples in the abstract format.

  Own Id: OTP-12195

- Support variables as Map keys in expressions and patterns

  Erlang will accept any expression as keys in Map expressions and it will
  accept literals or bound variables as keys in Map patterns.

  Own Id: OTP-12218

- The last traces of Mnemosyne Rules have been removed.

  Own Id: OTP-12257

- Properly support maps in match_specs

  Own Id: OTP-12270

- New function `ets:take/2`. Works the same as `ets:delete/2` but also returns
  the deleted object(s).

  Own Id: OTP-12309

- `string:tokens/2` is somewhat faster, especially if the list of separators
  only contains one separator character.

  Own Id: OTP-12422 Aux Id: seq12774

- The documentation of the Abstract Format (in the ERTS User's Guide) has been
  updated with types and specification. (Thanks to Anthony Ramine.)

  The explicit representation of parentheses used in types of the abstract
  format has been removed. Instead the new functions
  `erl_parse:type_inop_prec()` and `erl_parse:type_preop_prec()` can be used for
  inserting parentheses where needed.

  Own Id: OTP-12492

- Prevent zip:zip_open/\[12] from leaking file descriptors if parent process
  dies.

  Own Id: OTP-12566

- Add a new random number generator, see `rand` module. It have better
  characteristics and an improved interface.

  Own Id: OTP-12586 Aux Id: OTP-12501, OTP-12502

- `filename:split/1` when given an empty binary will now return an empty list,
  to make it consistent with return value when given an empty list.

  Own Id: OTP-12716

- Add `sync` option to `ets:tab2file/3`.

  Own Id: OTP-12737 Aux Id: seq12805

- Add functions `gb_sets:iterator_from()` and `gb_trees:iterator_from()`.
  (Thanks to Kirill Kinduk.)

  Own Id: OTP-12742

- Add `maps:filter/2` to maps module.

  Own Id: OTP-12745

- Change some internal data structures to Maps in order to speed up compilation
  time. Measured speed up is around 10%-15%.

  Own Id: OTP-12774

- Update `orddict` to use parameterized types and specs. (Thanks to UENISHI
  Kota.)

  Own Id: OTP-12785

- The assert macros in `eunit` has been moved out to
  `stdlib/include/assert.hrl`. This files get included by `eunit.hrl`. Thus,
  nothing changes for eunit users, but the asserts can now also be included
  separately.

  Own Id: OTP-12808

## STDLIB 2.4

### Fixed Bugs and Malfunctions

- Behaviour of character types \\d, \\w and \\s has always been to not match
  characters with value above 255, not 128, i.e. they are limited to ISO-Latin-1
  and not ASCII

  Own Id: OTP-12521

### Improvements and New Features

- c:m/1 now displays the module's MD5 sum.

  Own Id: OTP-12500

- Make ets:i/1 handle binary input from IO server.

  Own Id: OTP-12550

## STDLIB 2.3

### Fixed Bugs and Malfunctions

- The documentation of string:tokens/2 now explicitly specifies that adjacent
  separator characters do not give any empty strings in the resulting list of
  tokens.

  Own Id: OTP-12036

- Fix broken deprecation warnings in ssh application

  Own Id: OTP-12187

- Maps: Properly align union typed assoc values in documentation

  Own Id: OTP-12190

- Fix filelib:wildcard/2 when 'Cwd' ends with a dot

  Own Id: OTP-12212

- Allow `Name/Arity` syntax in maps values inside attributes.

  Own Id: OTP-12213

- Fix edlin to correctly save text killed with ctrl-u. Prior to this fix,
  entering text into the Erlang shell and then killing it with ctrl-u followed
  by yanking it back with ctrl-y would result in the yanked text being the
  reverse of the original killed text.

  Own Id: OTP-12224

- If a callback function was terminated with exit/1, there would be no stack
  trace in the ERROR REPORT produced by gen_server. This has been corrected.

  To keep the backwards compatibility, the actual exit reason for the process is
  not changed.

  Own Id: OTP-12263 Aux Id: seq12733

- Warnings produced by `ms_transform` could point out the wrong line number.

  Own Id: OTP-12264

### Improvements and New Features

- Supports tar file creation on other media than file systems mounted on the
  local machine.

  The `erl_tar` api is extended with `erl_tar:init/3` that enables usage of user
  provided media storage routines. A ssh-specific set of such routines is hidden
  in the new function `ssh_sftp:open_tar/3` to simplify creating a tar archive
  on a remote ssh server.

  A chunked file reading option is added to `erl_tar:add/3,4` to save memory on
  e.g small embedded systems. The size of the slices read from a file in that
  case can be specified.

  Own Id: OTP-12180 Aux Id: seq12715

- I/O requests are optimized for long message queues in the calling process.

  Own Id: OTP-12315

## STDLIB 2.2

### Fixed Bugs and Malfunctions

- The type spec of the FormFunc argument to sys:handle_debug/4 was erroneously
  pointing to dbg_fun(). This is now corrected and the new type is format_fun().

  Own Id: OTP-11800

- Behaviors such as gen_fsm and gen_server should always invoke format_status/2
  before printing the state to the logs.

  Own Id: OTP-11967

- The documentation of `dets:insert_new/2` has been corrected. (Thanks to Alexei
  Sholik for reporting the bug.)

  Own Id: OTP-12024

- Printing a term with io_lib:format and control sequence w, precision P and
  field width F, where F< P would fail in one of the two following ways:

  1. If P < printed length of the term, an infinite loop would be entered,
     consuming all available memory.

  2. If P >= printed length of the term, an exception would be raised.

  These two problems are now corrected.

  Own Id: OTP-12041

- The documentation of `maps:values/1` has been corrected.

  Own Id: OTP-12055

- Expand shell functions in map expressions.

  Own Id: OTP-12063

### Improvements and New Features

- Add maps:with/2

  Own Id: OTP-12137

## STDLIB 2.1.1

### Fixed Bugs and Malfunctions

- OTP-11850 fixed filelib:wildcard/1 to work with broken symlinks. This
  correction, however, introduced problems since symlinks were no longer
  followed for functions like filelib:ensure_dir/1, filelib:is_dir/1,
  filelib:file_size/1, etc. This is now corrected.

  Own Id: OTP-12054 Aux Id: seq12660

## STDLIB 2.1

### Fixed Bugs and Malfunctions

- `filelib:wildcard("broken_symlink")` would return an empty list if
  "broken_symlink" was a symlink that did not point to an existing file.

  Own Id: OTP-11850 Aux Id: seq12571

- `erl_tar` can now handle files names that contain Unicode characters. See
  "UNICODE SUPPORT" in the documentation for `erl_tar`.

  When creating a tar file, `erl_tar` would sometime write a too short end of
  tape marker. GNU tar would correctly extract files from such tar file, but
  would complain about "A lone zero block at...".

  Own Id: OTP-11854

- When redefining and exporting the type `t:map/0` the Erlang Code Linter
  (`erl_lint`) erroneously emitted an error. This bug has been fixed.

  Own Id: OTP-11872

- Fix evaluation of map updates in the debugger and erl_eval

  Reported-by: José Valim

  Own Id: OTP-11922

### Improvements and New Features

- The following native functions now bump an appropriate amount of reductions
  and yield when out of reductions:

  - `erlang:binary_to_list/1`
  - `erlang:binary_to_list/3`
  - `erlang:bitstring_to_list/1`
  - `erlang:list_to_binary/1`
  - `erlang:iolist_to_binary/1`
  - `erlang:list_to_bitstring/1`
  - `binary:list_to_bin/1`

  Characteristics impact:

  - **Performance** - The functions converting from lists got a performance loss
    for very small lists, and a performance gain for very large lists.

  - **Priority** - Previously a process executing one of these functions
    effectively got an unfair priority boost. This priority boost depended on
    the input size. The larger the input was, the larger the priority boost got.
    This unfair priority boost is now lost.

  Own Id: OTP-11888

- Add `maps:get/3` to maps module. The function will return the supplied default
  value if the key does not exist in the map.

  Own Id: OTP-11951

## STDLIB 2.0

### Fixed Bugs and Malfunctions

- The option dupnames did not work as intended in re. When looking for names
  with \{capture, \[Name, ...]\}, re:run returned a random instance of the match
  for that name, instead of the leftmost matching instance, which was what the
  documentation stated. This is now corrected to adhere to the documentation.
  The option \{capture,all_names\} along with a re:inspect/2 function is also
  added to further help in using named subpatterns.

  Own Id: OTP-11205

- If option 'binary' was set for standard_input, then c:i() would hang if the
  output was more than one page long - i.e. then input after "(c)ontinue (q)uit
  -->" could not be read. This has been corrected. (Thanks to José Valim)

  Own Id: OTP-11589

- stdlib/lists: Add function droplast/1 This functions drops the last element of
  a non-empty list. lists:last/1 and lists:droplast/1 are the dual of hd/1 and
  tl/1 but for the end of a list. (Thanks to Hans Svensson)

  Own Id: OTP-11677

- Allow all auto imports to be suppressed at once. Introducing the
  no_auto_import attribute: -compile(no_auto_import). Useful for code generation
  tools that always use the qualified function names and want to avoid the auto
  imported functions clashing with local ones. (Thanks to José Valim.)

  Own Id: OTP-11682

- supervisor_bridge does no longer report normal termination of children. The
  reason is that in some cases, for instance when the restart strategy is
  simple_one_for_one, the log could be completely overloaded with reports about
  normally terminating processes. (Thanks to Artem Ocheredko)

  Own Id: OTP-11685

- The type annotations for alternative registries using the \{via, Module,
  Name\} syntax for sup_name() and sup_ref() in the supervisor module are now
  consistent with the documentation. Dialyzer should no longer complain about
  valid supervisor:start_link() and supervisor:start_child() calls. (Thanks to
  Caleb Helbling.)

  Own Id: OTP-11707

- Two Dets bugs have been fixed. When trying to open a short file that is not a
  Dets file, the file was deleted even with just read access. Calling
  `dets:is_dets_file/1` with a file that is not a Dets file, a file descriptor
  was left open. (Thanks to Håkan Mattsson for reporting the bugs.)

  Own Id: OTP-11709

- Fix race bug in `ets:all`. Concurrent creation of tables could cause other
  tables to not be included in the result. (Thanks to Florian Schintke for bug
  report)

  Own Id: OTP-11726

- erl_eval now properly evaluates '=='/2 when it is used in guards. (Thanks to
  José Valim)

  Own Id: OTP-11747

- Calls to proplists:get_value/3 are replaced by the faster lists:keyfind/3 in
  io_lib_pretty. Elements in the list are always 2-tuples. (Thanks to Andrew
  Thompson)

  Own Id: OTP-11752

- A qlc bug where filters were erroneously optimized away has been fixed. Thanks
  to Sam Bobroff for reporting the bug.

  Own Id: OTP-11758

- A number of compiler errors where unusual or nonsensical code would crash the
  compiler have been reported by Ulf Norell and corrected by Anthony Ramine.

  Own Id: OTP-11770

- Since Erlang/OTP R16B the Erlang Core Linter (`erl_lint`) has not emitted
  errors when built-in types were re-defined. This bug has been fixed. (Thanks
  to Roberto Aloi.)

  Own Id: OTP-11772

- The functions `sys:get_state/1,2` and `sys:replace_state/2,3` are fixed so
  they can now be run while the process is sys suspended. To accomplish this,
  the new callbacks `Mod:system_get_state/1` and `Mod:system_replace_state/2`
  are added, which are also implemented by the generic behaviours `gen_server`,
  `gen_event` and `gen_fsm`.

  The potential incompatibility refers to:

  - The previous behaviour of intercepting the system message and passing a
    tuple of size 2 as the last argument to `sys:handle_system_msg/6` is no
    longer supported.
  - The error handling when `StateFun` in `sys:replace_state/2,3` fails is
    changed from being totally silent to possibly (if the callback module does
    not catch) throw an exception in the client process.

  (Thanks to James Fish and Steve Vinoski)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11817

### Improvements and New Features

- Options to set match_limit and match_limit_recursion are added to re:run. The
  option report_errors is also added to get more information when re:run fails
  due to limits or compilation errors.

  Own Id: OTP-10285

- The pre-defined types `array/0`, `dict/0`, `digraph/0`, `gb_set/0`,
  `gb_tree/0`, `queue/0`, `set/0`, and `tid/0` have been deprecated. They will
  be removed in Erlang/OTP 18.0.

  Instead the types [`array:array/0`](`t:array:array/0`),
  [`dict:dict/0`](`t:dict:dict/0`), [`digraph:graph/0`](`t:digraph:graph/0`),
  `gb_set:set/0`, `gb_tree:tree/0`, [`queue:queue/0`](`t:queue:queue/0`),
  [`sets:set/0`](`t:sets:set/0`), and [`ets:tid/0`](`t:ets:tid/0`) can be used.
  (Note: it has always been necessary to use [`ets:tid/0`](`t:ets:tid/0`).)

  It is allowed in Erlang/OTP 17.0 to locally re-define the types `array/0`,
  `dict/0`, and so on.

  New types [`array:array/1`](`t:array:array/1`),
  [`dict:dict/2`](`t:dict:dict/2`), [`gb_sets:set/1`](`t:gb_sets:set/1`),
  [`gb_trees:tree/2`](`t:gb_trees:tree/2`),
  [`queue:queue/1`](`t:queue:queue/1`), and [`sets:set/1`](`t:sets:set/1`) have
  been added.

  A compiler option, `nowarn_deprecated_type`, has been introduced. By including
  the attribute

  `-compile(nowarn_deprecated_type).`

  in an Erlang source file, warnings about deprecated types can be avoided in
  Erlang/OTP 17.0.

  The option can also be given as a compiler flag:

  `erlc +nowarn_deprecated_type file.erl`

  Own Id: OTP-10342

- Calls to erlang:open_port/2 with 'spawn' are updated to handle space in the
  command path.

  Own Id: OTP-10842

- Dialyzer's `unmatched_return` warnings have been corrected.

  Own Id: OTP-10908

- Forbid unsized fields in patterns of binary generators and simplified
  v3_core's translation of bit string generators. (Thanks to Anthony Ramine.)

  Own Id: OTP-11186

- The version of the PCRE library Used by Erlang's re module is raised to 8.33
  from 7.6. This means, among other things, better Unicode and Unicode Character
  Properties support. New options connected to PCRE 8.33 are also added to the
  re module (ucd, notempty_atstart, no_start_optimize). PCRE has extended the
  regular expression syntax between 7.6 and 8.33, why this imposes a potential
  incompatibility. Only very complicated regular expressions may be affected,
  but if you know you are using obscure features, please test run your regular
  expressions and verify that their behavior has not changed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11204

- Added dict:is_empty/1 and orddict:is_empty/1. (Thanks to Magnus Henoch.)

  Own Id: OTP-11353

- A call to either the [`garbage_collect/1`](`garbage_collect/1`) BIF or the
  [`check_process_code/2`](`check_process_code/2`) BIF may trigger garbage
  collection of another processes than the process calling the BIF. The previous
  implementations performed these kinds of garbage collections without
  considering the internal state of the process being garbage collected. In
  order to be able to more easily and more efficiently implement yielding native
  code, these types of garbage collections have been rewritten. A garbage
  collection like this is now triggered by an asynchronous request signal, the
  actual garbage collection is performed by the process being garbage collected
  itself, and finalized by a reply signal to the process issuing the request.
  Using this approach processes can disable garbage collection and yield without
  having to set up the heap in a state that can be garbage collected.

  The [`garbage_collect/2`](`erlang:garbage_collect/2`), and
  [`check_process_code/3`](`erlang:check_process_code/3`) BIFs have been
  introduced. Both taking an option list as last argument. Using these, one can
  issue asynchronous requests.

  `code:purge/1` and `code:soft_purge/1` have been rewritten to utilize
  asynchronous `check_process_code` requests in order to parallelize work.

  Characteristics impact: A call to the
  [`garbage_collect/1`](`garbage_collect/1`) BIF or the
  [`check_process_code/2`](`check_process_code/2`) BIF will normally take longer
  time to complete while the system as a whole wont be as much negatively
  effected by the operation as before. A call to `code:purge/1` and
  `code:soft_purge/1` may complete faster or slower depending on the state of
  the system while the system as a whole wont be as much negatively effected by
  the operation as before.

  Own Id: OTP-11388 Aux Id: OTP-11535, OTP-11648

- Improve the documentation of the supervisor's `via` reference. (Thanks to
  MaximMinin.)

  Own Id: OTP-11399

- `orddict:from_list/1` now uses the optimized sort routines in the `lists`
  module instead of (essentially) an insertion sort. Depending on the input
  data, the speed of the new `from_list/1` is anything from slightly faster up
  to several orders of magnitude faster than the old `from_list/1`.

  (Thanks to Steve Vinoski.)

  Own Id: OTP-11552

- EEP43: New data type - Maps

  With Maps you may for instance:

  - \_\_\_\_ - `M0 = #{ a => 1, b => 2}, % create associations`

  - \_\_\_\_ - `M1 = M0#{ a := 10 }, % update values`

  - \_\_\_\_ - `M2 = M1#{ "hi" => "hello"}, % add new associations`

  - \_\_\_\_ - `#{ "hi" := V1, a := V2, b := V3} = M2. % match keys with values`

  For information on how to use Maps please see Map Expressions in the
  [Reference Manual](`e:system:expressions.md#map-expressions`).

  The current implementation is without the following features:

  - \_\_\_\_ - No variable keys

  - \_\_\_\_ - No single value access

  - \_\_\_\_ - No map comprehensions

  Note that Maps is _experimental_ during OTP 17.0.

  Own Id: OTP-11616

- When tab completing the erlang shell now expands zero-arity functions all the
  way to closing parenthesis, unless there is another function with the same
  name and a different arity. (Thanks to Pierre Fenoll.)

  Own Id: OTP-11684

- The Erlang Code Preprocessor (`epp`) could loop when encountering a circular
  macro definition in an included file. This bug has been fixed.

  Thanks to Maruthavanan Subbarayan for reporting the bug, and to Richard
  Carlsson for providing a bug fix.

  Own Id: OTP-11728

- The Erlang Code Linter (`erl_lint`) has since Erlang/OTP R13B emitted warnings
  whenever any of the types `t:arity/0`, `t:bitstring/0`, `t:iodata/0`, or
  `t:boolean/0` were re-defined. Now errors are emitted instead.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11771

- The `encoding` option of `erl_parse:abstract/2` has been extended to include
  `none` and a callback function (a predicate).

  Own Id: OTP-11807

- Export zip option types to allow referal from other modules.

  Thanks to Pierre Fenoll and Håkan Mattson

  Own Id: OTP-11828

- The module `pg` has been deprecated and will be removed in Erlang/OTP 18.

  Own Id: OTP-11840

## STDLIB 1.19.4

### Fixed Bugs and Malfunctions

- Fix typo in gen_server.erl. Thanks to Brian L. Troutwine.

  Own Id: OTP-11398

- Spec for atan2 should be atan2(Y, X), not atan2(X, Y). Thanks to Ary
  Borenszweig.

  Own Id: OTP-11465

### Improvements and New Features

- Add XML marker for regexp syntax. Thanks to Håkan Mattson.

  Own Id: OTP-11442

## STDLIB 1.19.3

### Fixed Bugs and Malfunctions

- The functions `dets:foldl/3`, `dets:foldr/3`, and `dets:traverse/2` did not
  release the table after having traversed the table to the end. The bug was
  introduced in R16B. (Thanks to Manuel Duran Aguete.)

  Own Id: OTP-11245

- If the `fun M:F/A` construct was used erroneously the linter could crash.
  (Thanks to Mikhail Sobolev.)

  Own Id: OTP-11254

- The specifications of `io_lib:fread/2,3` have been corrected. (Thanks to Chris
  King and Kostis Sagonas for pinpointing the bug.)

  Own Id: OTP-11261

### Improvements and New Features

- Fixed type typo in gen_server.

  Own Id: OTP-11200

- Update type specs in filelib and io_prompt. Thanks to Jose Valim.

  Own Id: OTP-11208

- Fix typo in abcast() function comment. Thanks to Johannes Weissl.

  Own Id: OTP-11219

- Make edlin understand a few important control keys. Thanks to Stefan
  Zegenhagen.

  Own Id: OTP-11251

- Export the edge/0 type from the digraph module. Thanks to Alex Ronne Petersen.

  Own Id: OTP-11266

- Fix variable usage tracking in erl_lint and fixed unsafe variable tracking in
  try expressions. Thanks to Anthony Ramine.

  Own Id: OTP-11268

## STDLIB 1.19.2

### Fixed Bugs and Malfunctions

- The Erlang scanner no longer accepts floating point numbers in the input
  string.

  Own Id: OTP-10990

- When converting a faulty binary to a list with unicode:characters_to_list, the
  error return value could contain a faulty "rest", i.e. the io_list of
  characters that could not be converted was wrong. This happened only if input
  was a sub binary and conversion was from utf8. This is now corrected.

  Own Id: OTP-11080

- The type `hook_function()` has been corrected in `erl_pp`, the Erlang Pretty
  Printer.

  The printing of invalid forms, e.g. record field types, has also been fixed.
  It has been broken since R16B.

  (Thanks to Tomáš Janoušek.)

  Own Id: OTP-11100

- Fix receive support in erl_eval with a BEAM module. Thanks to Anthony Ramine.

  Own Id: OTP-11137

### Improvements and New Features

- Delete obsolete note about simple-one-for-one supervisor. Thanks to Magnus
  Henoch.

  Own Id: OTP-10938

- When selecting encoding of a script written in Erlang (`escript`) the optional
  directive on the second line is now recognized.

  Own Id: OTP-10951

- The function `erl_parse:abstract/2` has been documented.

  Own Id: OTP-10992

- Integrate elliptic curve contribution from Andreas Schultz

  In order to be able to support elliptic curve cipher suites in SSL/TLS,
  additions to handle elliptic curve infrastructure has been added to public_key
  and crypto.

  This also has resulted in a rewrite of the crypto API to gain consistency and
  remove unnecessary overhead. All OTP applications using crypto has been
  updated to use the new API.

  Impact: Elliptic curve cryptography (ECC) offers equivalent security with
  smaller key sizes than other public key algorithms. Smaller key sizes result
  in savings for power, memory, bandwidth, and computational cost that make ECC
  especially attractive for constrained environments.

  Own Id: OTP-11009

- Added sys:get_state/1,2 and sys:replace_state/2,3. Thanks to Steve Vinoski.

  Own Id: OTP-11013

- Optimizations to gen mechanism. Thanks to Loïc Hoguin.

  Own Id: OTP-11025

- Optimizations to gen.erl. Thanks to Loïc Hoguin.

  Own Id: OTP-11035

- Use erlang:demonitor(Ref, \[flush]) where applicable. Thanks to Loïc Hoguin.

  Own Id: OTP-11039

- Erlang source files with non-ASCII characters are now encoded in UTF-8
  (instead of latin1).

  Own Id: OTP-11041 Aux Id: OTP-10907

- Fix rest_for_one and one_for_all restarting a child not terminated. Thanks to
  James Fish.

  Own Id: OTP-11042

- Fix excessive CPU consumption of timer_server. Thanks to Aliaksey
  Kandratsenka.

  Own Id: OTP-11053

- Rename and document lists:zf/2 as lists:filtermap/2. Thanks to Anthony Ramine.

  Own Id: OTP-11078

- Fixed an inconsistent state in epp. Thanks to Anthony Ramine

  Own Id: OTP-11079

- c:ls(File) will now print File, similar to ls(1) in Unix. The error messages
  have also been improved. (Thanks to Bengt Kleberg.)

  Own Id: OTP-11108

- Support callback attributes in erl_pp. Thanks to Anthony Ramine.

  Own Id: OTP-11140

- Improve erl_lint performance. Thanks to José Valim.

  Own Id: OTP-11143

## STDLIB 1.19.1

### Fixed Bugs and Malfunctions

- Bugs related to Unicode have been fixed in the `erl_eval` module.

  Own Id: OTP-10622 Aux Id: kunagi-351 \[262]

- `filelib:wildcard("some/relative/path/*.beam", Path)` would fail to match any
  file. That is, filelib:wildcard/2 would not work if the first component of the
  pattern did not contain any wildcard characters. (A previous attempt to fix
  the problem in R15B02 seems to have made matters worse.)

  (Thanks to Samuel Rivas and Tuncer Ayaz.)

  There is also an incompatible change to the `Path` argument. It is no longer
  allowed to be a binary.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-10812

### Improvements and New Features

- The new STDLIB application variable `shell_strings` can be used for
  determining how the Erlang shell outputs lists of integers. The new function
  `shell:strings/1` toggles the value of the variable.

  The control sequence modifier `l` can be used for turning off the string
  recognition of `~p` and `~P`.

  Own Id: OTP-10755

- Miscellaneous updates due to Unicode support.

  Own Id: OTP-10820

- Extend `~ts` to handle binaries with characters coded in ISO-latin-1

  Own Id: OTP-10836

- The +pc flag to erl can be used to set the range of characters considered
  printable. This affects how the shell and io:format("~tp",...) functionality
  does heuristic string detection. More can be read in STDLIB users guide.

  Own Id: OTP-10884

## STDLIB 1.19

### Fixed Bugs and Malfunctions

- Wildcards such as "some/path/\*" passed to `filelib:wildcard/2` would fail to
  match any file. (Thanks to Samuel Rivas for reporting this bug.)

  Own Id: OTP-6874 Aux Id: kunagi-190 \[101]

- Fixed error handling in proc_lib:start which could hang if the spawned process
  died in init.

  Own Id: OTP-9803 Aux Id: kunagi-209 \[120]

- Allow \*\* in filelib:wildcard

  Two adjacent \* used as a single pattern will match all files and zero or more
  directories and subdirectories. (Thanks to José Valim)

  Own Id: OTP-10431

- Add the \\gN and \\g\{N\} syntax for back references in re:replace/3,4 to
  allow use with numeric replacement strings. (Thanks to Vance Shipley)

  Own Id: OTP-10455

- Export ets:match_pattern/0 type (Thanks to Joseph Wayne Norton)

  Own Id: OTP-10472

- Fix printing the empty binary at depth 1 with ~W (Thanks to Andrew Thompson)

  Own Id: OTP-10504

- The type `ascii_string()` in the `base64` module has been corrected. The type
  [`file:file_info()`](`t:file:file_info/0`) has been cleaned up. The type
  [`file:fd()`](`t:file:fd/0`) has been made opaque in the documentation.

  Own Id: OTP-10624 Aux Id: kunagi-352 \[263]

### Improvements and New Features

- Dets tables are no longer fixed while traversing with a bound key (when only
  the objects with the right key are matched). This optimization affects the
  functions `match/2`, `match_object/2`, `select/2`, `match_delete/2`, and
  `select_delete/2`.

  Own Id: OTP-10097

- Support for Unicode has been implemented.

  Own Id: OTP-10302

- The linter now warns for opaque types that are not exported, as well as for
  under-specified opaque types.

  Own Id: OTP-10436

- The type [`file:name()`](`t:file:name/0`) has been substituted for the type
  [`file:filename()`](`t:file:filename/0`) in the following functions in the
  `filename` module: `absname/2`, `absname_join/2`, `join/1,2`, and `split/1`.

  Own Id: OTP-10474

- If a child process fails in its start function, then the error reason was
  earlier only reported as an error report from the error_handler, and
  supervisor:start_link would only return `{error,shutdown}`. This has been
  changed so the supervisor will now return `{error,{shutdown,Reason}}`, where
  `Reason` identifies the failing child and its error reason. (Thanks to Tomas
  Pihl)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-10490

- Where necessary a comment stating encoding has been added to Erlang files. The
  comment is meant to be removed in Erlang/OTP R17B when UTF-8 becomes the
  default encoding.

  Own Id: OTP-10630

- The contracts and types of the modules `erl_scan` and `sys` have been
  corrected and improved. (Thanks to Kostis Sagonas.)

  Own Id: OTP-10658

- The Erlang shell now skips the rest of the line when it encounters an Erlang
  scanner error.

  Own Id: OTP-10659

- Clean up some specs in the proplists module. (Thanks to Kostis Sagonas.)

  Own Id: OTP-10663

- Some examples overflowing the width of PDF pages have been corrected.

  Own Id: OTP-10665

- Enable escript to accept emulator arguments when script file has no shebang.
  Thanks to Magnus Henoch

  Own Id: OTP-10691

- Fix bug in queue:out/1, queue:out_r/1 that makes it O(N^2) in worst case.
  Thanks to Aleksandr Erofeev.

  Own Id: OTP-10722

- There are new functions in the `epp` module which read the character encoding
  from files. See `m:epp` for more information.

  Own Id: OTP-10742 Aux Id: OTP-10302

- The functions in `io_lib` have been adjusted for Unicode. The existing
  functions `write_string()` and so on now take Unicode strings, while the old
  behavior has been taken over by new functions `write_latin1_string()` and so
  on. There are also new functions to write Unicode strings as Latin-1 strings,
  mainly targetted towards the Erlang pretty printer (`erl_pp`).

  Own Id: OTP-10745 Aux Id: OTP-10302

- The new functions `proc_lib:format/2` and `erl_parse:abstract/2` accept an
  encoding as second argument.

  Own Id: OTP-10749 Aux Id: OTP-10302

- Increased potential concurrency in ETS for `write_concurrency` option. The
  number of internal table locks has increased from 16 to 64. This makes it four
  times less likely that two concurrent processes writing to the same table
  would collide and thereby serialized. The cost is an increased constant memory
  footprint for tables using write_concurrency. The memory consumption per
  inserted record is not affected. The increased footprint can be particularly
  large if `write_concurrency` is combined with `read_concurrency`.

  Own Id: OTP-10787

## STDLIB 1.18.3

### Fixed Bugs and Malfunctions

- Minor test updates

  Own Id: OTP-10591

## STDLIB 1.18.2

### Fixed Bugs and Malfunctions

- Fixed bug where if given an invalid drive letter on windows ensure dir would
  go into an infinite loop.

  Own Id: OTP-10104

- Calls to gen_server:enter_loop/4 where ServerName has a global scope and no
  timeout is given now works correctly.

  Thanks to Sam Bobroff for reporting the issue.

  Own Id: OTP-10130

- fix escript/primary archive reloading

  If the mtime of an escript/primary archive file changes after being added to
  the code path, correctly reload the archive and update the cache. (Thanks to
  Tuncer Ayaz)

  Own Id: OTP-10151

- Fix bug that in some cases could cause corrupted binaries in ETS tables with
  `compressed` option.

  Own Id: OTP-10182

- Fix filename:nativename/1 on Win32

  Don't choke on paths given as binary argument on Win32. Thanks to Jan Klötzke

  Own Id: OTP-10188

- Fix bug in `ets:test_ms/2` that could cause emulator crash when using `'$_'`
  in match spec.

  Own Id: OTP-10190

- Fix bug where zip archives wrongly have a first disk number set to 1

  Own Id: OTP-10223

### Improvements and New Features

- The message printed by the Erlang shell as an explanation of the `badarith`
  error has been corrected. (Thanks to Matthias Lang.)

  Own Id: OTP-10054

## STDLIB 1.18.1

### Fixed Bugs and Malfunctions

- References to `is_constant/1` (which was removed in the R12 release) has been
  removed from documentation and code.

  Own Id: OTP-6454 Aux Id: seq10407

- Leave control back to gen_server during supervisor's restart loop

  When an attempt to restart a child failed, supervisor would earlier keep the
  execution flow and try to restart the child over and over again until it
  either succeeded or the restart frequency limit was reached. If none of these
  happened, supervisor would hang forever in this loop.

  This commit adds a timer of 0 ms where the control is left back to the
  gen_server which implements the supervisor. This way any incoming request to
  the supervisor will be handled - which could help breaking the infinite loop -
  e.g. shutdown request for the supervisor or for the problematic child.

  This introduces some incompatibilities in stdlib due to new return values from
  supervisor:

  - restart_child/2 can now return \{error,restarting\}
  - delete_child/2 can now return \{error,restarting\}
  - which_children/1 returns a list of \{Id,Child,Type,Mods\}, where Child, in
    addition to the old pid() or 'undefined', now also can be 'restarting'.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9549

- If a temporary child's start function returned 'ignore', then the supervisor
  would keep it's child specification. This has been corrected. Child
  specifications for non-existing temporary children shall never be kept.

  Own Id: OTP-9782 Aux Id: seq11964

- Use universal time as base in error logger

  Previous conversion used the deprecated
  calendar:local_time_to_universal_time/1

  Own Id: OTP-9854

- Calling a guard test (such as is_list/1) from the top-level in a guard, would
  cause a compiler crash if there was a local definition with the same name.
  Corrected to reject the program with an error message.

  Own Id: OTP-9866

- Fix the type spec from the doc of binary:part/3 (Thanks to Ricardo Catalinas
  Jiménez)

  Own Id: OTP-9920

- Correct spelling of registered (Thanks to Richard Carlsson)

  Own Id: OTP-9925

- Put gb_trees documentation into alphabetical order (Thanks to Aidan Hobson
  Sayers)

  Own Id: OTP-9929

- Fix bug in ETS with `compressed` option and insertion of term containing large
  integers (>2G) on 64-bit machines. Seen to cause emulator crash. (Thanks to
  Diego Llarrull for excellent bug report)

  Own Id: OTP-9932

- Add plugin support for alternative name lookup This patch introduces a new way
  of locating a behaviour instance: \{via, Module, Name\}. (Thanks to Ulf Wiger)

  Own Id: OTP-9945

- The function `digraph_utils:condensation/1` used to create a digraph
  containing loops contradicting the documentation which states that the created
  digraph is free of cycles. This bug has been fixed. (Thanks to Kostis Sagonas
  for finding the bug.)

  Own Id: OTP-9953

- When an escript ends now all printout to standard output and standard error
  gets out on the terminal. This bug has been corrected by changing the
  behaviour of erlang:halt/0,1, which should fix the same problem for other
  escript-like applications, i.e that data stored in the output port driver
  buffers got lost when printing on a TTY and exiting through erlang:halt/0,1.

  The BIF:s erlang:halt/0,1 has gotten improved semantics and there is a new BIF
  erlang:halt/2 to accomplish something like the old semantics. See the
  documentation.

  Now erlang:halt/0 and erlang:halt/1 with an integer argument will close all
  ports and allow all pending async threads operations to finish before exiting
  the emulator. Previously erlang:halt/0 and erlang:halt(0) would just wait for
  pending async threads operations but not close ports. And erlang:halt/1 with a
  non-zero integer argument would not even wait for pending async threads
  operations.

  To roughly the old behaviour, to not wait for ports and async threads
  operations when you exit the emulator, you use erlang:halt/2 with an integer
  first argument and an option list containing \{flush,false\} as the second
  argument. Note that now is flushing not dependant of the exit code, and you
  cannot only flush async threads operations which we deemed as a strange
  behaviour anyway.

  Also, erlang:halt/1,2 has gotten a new feature: If the first argument is the
  atom 'abort' the emulator is aborted producing a core dump, if the operating
  system so allows.

  Own Id: OTP-9985

- Add escript win32 alternative invocation. escript can now be started as both
  "escript.exe" and "escript" (Thanks to Pierre Rouleau)

  Own Id: OTP-9997

## STDLIB 1.18

### Fixed Bugs and Malfunctions

- Improved algorithm in module `random`. Avoid seed values that are even
  divisors of the primes and by that prevent getting sub-seeds that are stuck on
  zero. Worst case was random:seed(0,0,0) that produced a series of only zeros.
  This is an incompatible change in the sense that applications that relies on
  reproducing a specific series for a given seed will fail. The pseudo random
  output is still deterministic but different compared to earlier versions.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8713

- Calls to `global:whereis_name/1` have been substituted for calls to
  `global:safe_whereis_name/1` since the latter is not safe at all.

  The reason for not doing this earlier is that setting a global lock masked out
  a bug concerning the restart of supervised children. The bug has now been
  fixed by a modification of `global:whereis_name/1`. (Thanks to Ulf Wiger for
  code contribution.)

  A minor race conditions in `gen_fsm:start*` has been fixed: if one of these
  functions returned `{error, Reason}` or ignore, the name could still be
  registered (either locally or in `global`. (This is the same modification as
  was done for gen_server in OTP-7669.)

  The undocumented function `global:safe_whereis_name/1` has been removed.

  Own Id: OTP-9212 Aux Id: seq7117, OTP-4174

- If a child of a supervisor terminates with reason \{shutdown,Term\} it is now
  handled by the supervisor as if the reason was 'shutdown'.

  For children with restart type 'permanent', this implies no change. For
  children with restart type 'transient', the child will no longer be restarted
  and no supervisor report will be written. For children with restart type
  'temporary', no supervisor report will be written.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9222

- Minor improvement of documentation regarding supervisor restart strategy for
  temporary and transient child processes.

  Own Id: OTP-9381

- A Dets table with sufficiently large buckets could not always be repaired.
  This bug has been fixed.

  The format of Dets files has been modified. When downgrading tables created
  with the new system will be repaired. Otherwise the modification should not be
  noticeable.

  Own Id: OTP-9607

- A few contracts in the `lists` module have been corrected.

  Own Id: OTP-9616

- Add '-callback' attributes in stdlib's behaviours

  Replace the behaviour_info(callbacks) export in stdlib's behaviours with
  -callback' attributes for all the callbacks. Update the documentation with
  information on the callback attribute Automatically generate 'behaviour_info'
  function from '-callback' attributes

  'behaviour_info(callbacks)' is a special function that is defined in a module
  which describes a behaviour and returns a list of its callbacks.

  This function is now automatically generated using the '-callback' specs. An
  error is returned by lint if user defines both '-callback' attributes and the
  behaviour_info/1 function. If no type info is needed for a callback use a
  generic spec for it. Add '-callback' attribute to language syntax

  Behaviours may define specs for their callbacks using the familiar spec
  syntax, replacing the '-spec' keyword with '-callback'. Simple lint checks are
  performed to ensure that no callbacks are defined twice and all types referred
  are declared.

  These attributes can be then used by tools to provide documentation to the
  behaviour or find discrepancies in the callback definitions in the callback
  module.

  Add callback specs into 'application' module in kernel Add callback specs to
  tftp module following internet documentation Add callback specs to
  inets_service module following possibly deprecated comments

  Own Id: OTP-9621

- If a Dets table had been properly closed but the space management data could
  not been read, it was not possible to repair the file. This bug has been
  fixed.

  Own Id: OTP-9622

- The Unicode noncharacter code points 16#FFFE and 16#FFFE were not allowed to
  be encoded or decoded using the `unicode` module or bit syntax. That was
  inconsistent with the other noncharacters 16#FDD0 to 16#FDEF that could be
  encoded/decoded. To resolve the inconsistency, 16#FFFE and 16#FFFE can now be
  encoded and decoded. (Thanks to Alisdair Sullivan.)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9624

- Make epp search directory of current file first when including another file
  This completes a partial fix in R11 that only worked for include_lib().
  (Thanks to Richard Carlsson)

  Own Id: OTP-9645

- ms_transform: Fix incorrect \`variable shadowed' warnings

  This patch removes incorrect passing of variable bindings from one function
  clause to another. (Thanks to Haitao Li)

  Own Id: OTP-9646

- Explicitly kill dynamic children in supervisors

  According to the supervisor's documentation: "Important note on
  simple-one-for-one supervisors: The dynamically created child processes of a
  simple-one-for-one supervisor are not explicitly killed, regardless of
  shutdown strategy, but are expected to terminate when the supervisor does
  (that is, when an exit signal from the parent process is received)."

  All is fine as long as we stop simple_one_for_one supervisor manually. Dynamic
  children catch the exit signal from the supervisor and leave. But, if this
  happens when we stop an application, after the top supervisor has stopped, the
  application master kills all remaining processes associated to this
  application. So, dynamic children that trap exit signals can be killed during
  their cleanup (here we mean inside terminate/2). This is unpredictable and
  highly time-dependent.

  In this commit, supervisor module is patched to explicitly terminate dynamic
  children accordingly to the shutdown strategy.

  NOTE: Order in which dynamic children are stopped is not defined. In fact,
  this is "almost" done at the same time.

  Stack errors when dynamic children are stopped

  Because a simple_one_for_one supervisor can have many workers, we stack errors
  during its shutdown to report only one message for each encountered error
  type. Instead of reporting the child's pid, we use the number of concerned
  children. (Thanks to Christopher Faulet)

  Own Id: OTP-9647

- Allow an infinite timeout to shutdown worker processes

  Now, in child specification, the shutdown value can also be set to infinity
  for worker children. This restriction was removed because this is not always
  possible to predict the shutdown time for a worker. This is highly
  application-dependent. Add a warning to docs about workers' shutdown strategy
  (Thanks to Christopher Faulet)

  Own Id: OTP-9648

- A badarg would sometimes occur in supervisor when printing error reports and
  the child pid was undefined. This has been corrected.

  Own Id: OTP-9669

- Fix re:split spec not to accept option 'global'(Thanks to Shunichi Shinohara)

  Own Id: OTP-9691

### Improvements and New Features

- Fix a few tests that used to fail on the HiPE platform.

  Own Id: OTP-9637

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

- The deprecated '`regexp`' module has been removed. Use the '`re`' module
  instead.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9737

- `filename:find_src/1,2` will now work on stripped BEAM files (reported by Per
  Hedeland). The HiPE compiler will also work on stripped BEAM files. The BEAM
  compiler will no longer include compilation options given in the source code
  itself in `M:module_info(compile)` (because those options will be applied
  anyway if the module is re-compiled).

  Own Id: OTP-9752

## STDLIB 1.17.5

### Fixed Bugs and Malfunctions

- erl_tar:extract failed when executed inside a directory with some parent
  directory to which the user has no read access. This has been corrected.

  Own Id: OTP-9368

- A bug in `erl_scan:set_attribute/3` has been fixed.

  Own Id: OTP-9412

- The contract of `io_lib:fread()` has been corrected.

  Own Id: OTP-9413 Aux Id: seq11873

- A crash in io*lib:fread/2 when end of input data was encountered while trying
  to match literal characters, which should return \{more,*,_,_\} but instead
  crashed, has been corrected. Reported by Klas Johansson.

  A similar peculiarity for io:fread when encountering end of file before any
  field data has also been corrected.

  Own Id: OTP-9439

- The contract of `timer:now_diff()` has been corrected. (Thanks to Alex
  Morarash).

  Own Id: OTP-9450

- Fix minor typo in gen_fsm documentation (Thanks to Haitao Li)

  Own Id: OTP-9456

- The contracts of `zip:zip_list_dir/1` and `zip:zip_get/2` have been corrected.

  Own Id: OTP-9471 Aux Id: seq11887, OTP-9472

- A bug in `zip:zip_open()` has been fixed.

  Own Id: OTP-9472 Aux Id: seq11887, OTP-9471

- Fix trivial documentation errors(Thanks to Matthias Lang)

  Own Id: OTP-9498

- Add a proplist() type

  Recently I was adding specs to an API and found that there is no canonical
  proplist() type defined. (Thanks to Ryan Zezeski)

  Own Id: OTP-9499

- fix supervisors restarting temporary children

  In the current implementation of supervisors, temporary children should never
  be restarted. However, when a temporary child is restarted as part of a
  one_for_all or rest_for_one strategy where the failing process is not the
  temporary child, the supervisor still tries to restart it.

  Because the supervisor doesn't keep some of the MFA information of temporary
  children, this causes the supervisor to hit its restart limit and crash.

  This patch fixes the behaviour by inserting a clause in terminate_children/2-3
  (private function) that will omit temporary children when building a list of
  killed processes, to avoid having the supervisor trying to restart them again.

  Only supervisors in need of restarting children used the list, so the change
  should be of no impact for the functions that called terminate_children/2-3
  only to kill all children.

  The documentation has been modified to make this behaviour more explicit.
  (Thanks to Fred Hebert)

  Own Id: OTP-9502

- fix broken edoc annotations (Thanks to Richard Carlsson)

  Own Id: OTP-9516

- XML files have been corrected.

  Own Id: OTP-9550 Aux Id: OTP-9541

- Handle rare race in the crypto key server functionality

  Own Id: OTP-9586

### Improvements and New Features

- Types and specifications have been added.

  Own Id: OTP-9356

- The contracts of the `queue` module have been modified.

  Own Id: OTP-9418

- Contracts in STDLIB and Kernel have been improved and type errors have been
  corrected.

  Own Id: OTP-9485

- Types for several BIFs have been extended/corrected. Also the types for types
  for `lists:keyfind/3`, `lists:keysearch/3`, and `lists:keyemember/3` have been
  corrected. The incorrect/incomplete types could cause false dialyzer warnings.

  Own Id: OTP-9496

## STDLIB 1.17.4

### Fixed Bugs and Malfunctions

- The default value `undefined` was added to records field types in such a way
  that the result was not always a well-formed type. This bug has been fixed.

  Own Id: OTP-9147

- Update index file atomically

  Since the log_mf_h index file might be read by other processes than the error
  handler (e.g. by the rb tool), this file should be updated atomically. This
  will avoid hitting the time gap between opening the file in write mode (and
  thus emptying the file) and the actual update with the new contents. To do
  this, a temporary file is written, and the file:rename/1 used to replace the
  real index file.

  Own Id: OTP-9148

- Fixed various typos across the documentation (Thanks to Tuncer Ayaz)

  Own Id: OTP-9154

- Supervisors should not save child-specs for temporary processes when they
  terminate as they should not be restarted. Saving the temporary child spec
  will result in that you cannot start a new temporary process with the same
  child spec as an already terminated temporary process. Since R14B02 you cannot
  restart a temporary temporary process as arguments are no longer saved, it has
  however always been semantically incorrect to restart a temporary process.
  Thanks to Filipe David Manana for reporting this and suggesting a solution.

  Own Id: OTP-9167 Aux Id: OTP-9064

- Various small documentation fixes (Thanks to Bernard Duggan)

  Own Id: OTP-9172

- Fix format_status bug for unregistered gen_event processes

  Port the gen_fsm code for format_status to gen_event in order to prevent a
  lists:concat(\[...,pid()]) crash when calling sys:get_status/1 on an
  unregistered gen_event process.

  Refactor format*status header code from gen*\* behaviours to module gen.

  Extend the format_status tests in gen_event_SUITE to cover format_status bugs
  with anonymous gen_event processes. (Thanks To Geoff Cant)

  Own Id: OTP-9218

- List of pids changed to 'set' in supervisor for dynamic temporary children.
  Accessing the list would not scale well when adding/deleting many children.
  (Thanks to Evgeniy Khramtsov)

  Own Id: OTP-9242

- Change pool module to attempt to attach to nodes that are already running

  The pool module prints out an error message and takes no further action for
  nodes that are already running. This patch changes that behavior so that if
  the return from slave:start/3 is \{already_running, Node\} then an attempt to
  attach to the node is still made. This makes sense because the node has been
  specified by the user in the .hosts.erlang file indicating a wish for the node
  to be part of the pool and a manual attach can be successfully made after the
  pool is started.(Thanks to Kelly McLaughlin)

  Own Id: OTP-9244

- unicode: document 16#FFFE and 16#FFFF (non chars)(Thanks to Tuncer Ayaz)

  Own Id: OTP-9256

- re: remove gratuitous "it " in manpage (Thanks to Tuncer Ayaz)

  Own Id: OTP-9307

- A bug in erl_eval(3) has been fixed.

  Own Id: OTP-9322

### Improvements and New Features

- Add `timer:tc/1` and remove the catch in `tc/2` and `tc/3`. The time measuring
  functions will thus no longer trap exits, errors or throws caused by the
  measured function.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-9169

- Allow supervisor:terminate_child(SupRef,Pid) for simple_one_for_one
  supervisors

  supervisor:terminate_child/2 was earlier not allowed if the supervisor used
  restart strategy simple_one_for_one. This is now changed so that children of
  this type of supervisors can be terminated by specifying the child's Pid.

  (Thanks to Vance Shipley.)

  Own Id: OTP-9201

- Types and specifications have been added.

  Own Id: OTP-9267

- Erlang types and specifications are used for documentation.

  Own Id: OTP-9271

- Allow Dets tablenames to be arbitrary terms.

  Own Id: OTP-9282

- A specification that could cause problems for Dialyzer has been fixed. An
  opaque type in erl_eval has been turned in to a ordinary type. This is a
  temporary fix.

  Own Id: OTP-9333

## STDLIB 1.17.3

### Fixed Bugs and Malfunctions

- Two bugs in io:format for ~F.~Ps has been corrected. When length(S) >=
  abs(F) > P, the precision P was incorrectly ignored. When F == P > length(S)
  the result was incorrectly left adjusted. Bug found by Ali Yakout who also
  provided a fix.

  Own Id: OTP-8989 Aux Id: seq11741

- Fix exception generation in the io module

  Some functions did not generate correct badarg exception on a badarg
  exception.

  Own Id: OTP-9045

- Fixes to the dict and orddict module documentation

  Fixed grammar and one inconsistency (Key - Value instead of key/value, since
  everywhere else the former is used). (thanks to Filipe David Manana)

  Own Id: OTP-9083

- Add ISO week number calculation functions to the calendar module in stdlib

  This new feature adds the missing week number function to the calendar module
  of the stdlib application. The implementation conforms to the ISO 8601
  standard. The new feature has been implemented tested and documented (thanks
  to Imre Horvath).

  Own Id: OTP-9087

### Improvements and New Features

- Implement the 'MAY' clauses from RFC4648 regarding the pad character to make
  mime_decode() and mime_decode_to_string() functions more tolerant of badly
  padded base64. The RFC is quoted below for easy reference.

  "RFC4648 Section 3.3 with reference to MIME decoding: Furthermore, such
  specifications MAY ignore the pad character, "=", treating it as non-alphabet
  data, if it is present before the end of the encoded data. If more than the
  allowed number of pad characters is found at the end of the string (e.g., a
  base 64 string terminated with "==="), the excess pad characters MAY also be
  ignored."

  Own Id: OTP-9020

- Supervisors will no longer save start parameters for temporary processes as
  they will not be restarted. In the case of simple_one_for_one workers such as
  ssl-connection processes this will substantial reduce the memory footprint of
  the supervisor.

  Own Id: OTP-9064

- When running escript it is now possible to add the -n flag and the escript
  will be compiled using +native.

  Own Id: OTP-9076

## STDLIB 1.17.2.1

### Fixed Bugs and Malfunctions

- Several type specifications for standard libraries were wrong in the R14B01
  release. This is now corrected. The corrections concern types in
  re,io,filename and the module erlang itself.

  Own Id: OTP-9008

## STDLIB 1.17.2

### Fixed Bugs and Malfunctions

- When several clients accessed a Dets table simultaneously, one of them calling
  `dets:insert_new/2`, the Dets server could crash. Alternatively, under the
  same conditions, `ok` was sometimes returned instead of `true`. (Thanks to
  John Hughes.)

  Own Id: OTP-8856

- When several clients accessed a Dets table simultaneously, inserted or updated
  objects were sometimes lost due to the Dets file being truncated. (Thanks to
  John Hughes.)

  Own Id: OTP-8898

- When several clients accessed a Dets table simultaneously, modifications of
  the Dets server's internal state were sometimes thrown away. The symptoms are
  diverse: error with reason `bad_object`; inserted objects not returned by
  `lookup()`; et cetera. (Thanks to John Hughes.)

  Own Id: OTP-8899

- If a Dets table was closed after calling `bchunk/2`, `match/1,3`,
  `match_object/1,3`, or `select/1,3` and then opened again, a subsequent call
  using the returned continuation would normally return a reply. This bug has
  fixed; now the call fails with reason `badarg`.

  Own Id: OTP-8903

- Cover did not collect coverage data for files such as Yecc parses containing
  include directives. The bug has been fixed by modifying `epp`, the Erlang Code
  Preprocessor.

  Own Id: OTP-8911

- If a Dets table with fewer slots than keys was opened and then closed after
  just a lookup, the contents were no longer well-formed. This bug has been
  fixed. (Thanks to Matthew Evans.)

  Own Id: OTP-8923

- In a supervisor, when it terminates a child, if that child happens to have
  exited fractionally early, with normal, the supervisor reports this as an
  error. This should not be reported as an error.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8938 Aux Id: seq11615

### Improvements and New Features

- The documentation filelib:wildcard/1,2 now describes the character set syntax
  for wildcards.

  Own Id: OTP-8879 Aux Id: seq11683

- Buffer overflows have been prevented in `erlc`, `dialyzer`, `typer`,
  `run_test`, `heart`, `escript`, and `erlexec`.

  (Thanks to Michael Santos.)

  Own Id: OTP-8892

- Using a float for the number of copies for `string:copies/2` resulted in an
  infinite loop. Now it will fail with an exception instead. (Thanks to Michael
  Santos.)

  Own Id: OTP-8915

- New ETS option `compressed`, to enable a more compact storage format at the
  expence of heavier table operations. For test and evaluation, `erl +ec` can be
  used to force compression on all ETS tables.

  Own Id: OTP-8922 Aux Id: seq11658

- The default maximum number of slots of a Dets table has been changed as to be
  equal to the maximum number of slots. (Thanks to Richard Carlsson.)

  Own Id: OTP-8959

## STDLIB 1.17.1

### Fixed Bugs and Malfunctions

- reference() has been substituted for ref() in the documentation.

  Own Id: OTP-8733

### Improvements and New Features

- The ms_transform now warns if the fun head shadows surrounding variables (just
  like the warnings you would get for an ordinary fun in the same context).

  Own Id: OTP-6759

- ets:select_reverse/\{1,2,3\} are now documented.

  Own Id: OTP-7863

- Large parts of the `ethread` library have been rewritten. The `ethread`
  library is an Erlang runtime system internal, portable thread library used by
  the runtime system itself.

  Most notable improvement is a reader optimized rwlock implementation which
  dramatically improve the performance of read-lock/read-unlock operations on
  multi processor systems by avoiding ping-ponging of the rwlock cache lines.
  The reader optimized rwlock implementation is used by miscellaneous rwlocks in
  the runtime system that are known to be read-locked frequently, and can be
  enabled on ETS tables by passing the
  [\{read_concurrency, true\}](`m:ets#new_2_read_concurrency`) option upon table
  creation. See the documentation of `ets:new/2` for more information. The
  reader optimized rwlock implementation can be fine tuned when starting the
  runtime system. For more information, see the documentation of the
  [\+rg](`e:erts:erl_cmd.md#%2Brg`) command line argument of `erl`.

  There is also a new implementation of rwlocks that is not optimized for
  readers. Both implementations interleaves readers and writers during
  contention as opposed to, e.g., the NPTL (Linux) pthread rwlock implementation
  which use either a reader or writer preferred strategy. The reader/writer
  preferred strategy is problematic since it starves threads doing the
  non-preferred operation.

  The new rwlock implementations in general performs better in ERTS than common
  pthread implementations. However, in some extremely heavily contended cases
  this is not the case. Such heavy contention can more or less only appear on
  ETS tables. This when multiple processes do very large amounts of write locked
  operations simultaneously on the same table. Such use of ETS is bad regardless
  of rwlock implementation, will never scale, and is something we strongly
  advise against.

  The new rwlock implementations depend on atomic operations. If no native
  atomic implementation is found, a fallback solution will be used. Using the
  fallback implies a performance degradation. That is, it is more important now
  than before to build OTP with a native atomic implementation.

  The `ethread` library contains native atomic implementations for, x86 (32 and
  64 bit), powerpc (32 bit), sparc V9 (32 and 64 bit), and tilera (32 bit). On
  other hardware gcc's builtin support for atomic memory access will be used if
  such exists. If no such support is found, `configure` will warn about no
  atomic implementation available.

  The `ethread` library can now also use the `libatomic_ops` library for atomic
  memory accesses. This makes it possible for the Erlang runtime system to
  utilize optimized native atomic operations on more platforms than before. If
  `configure` warns about no atomic implementation available, try using the
  `libatomic_ops` library. Use the
  [\--with-libatomic_ops=PATH](`e:system:install.md#advanced-configuration-and-build-of-erlang-otp`)
  `configure` command line argument when specifying where the `libatomic_ops`
  installation is located. The `libatomic_ops` library can be downloaded from:
  [http://www.hpl.hp.com/research/linux/atomic_ops/](http://www.hpl.hp.com/research/linux/atomic_ops/)

  The changed API of the `ethread` library has also caused modifications in the
  Erlang runtime system. Preparations for the to come "delayed deallocation"
  feature has also been done since it depends on the `ethread` library.

  _Note_: When building for x86, the `ethread` library will now use instructions
  that first appeared on the pentium 4 processor. If you want the runtime system
  to be compatible with older processors (back to 486) you need to pass the
  [\--enable-ethread-pre-pentium4-compatibility](`e:system:install.md#advanced-configuration-and-build-of-erlang-otp`)
  `configure` command line argument when configuring the system.

  Own Id: OTP-8544

- Some Built In Functions (BIFs) from the module erlang was never made
  autoimported for backward compatibility reasons. As local functions now
  override autoimports, new autoimports is no longer a problem, why the
  following BIFs are finally made autoimported: monitor/2, monitor/3,
  demonitor/2, demonitor/3, error/1, error/2, integer_to_list/2,
  list_to_integer/2.

  Own Id: OTP-8763

## STDLIB 1.17

### Fixed Bugs and Malfunctions

- The Erlang code preprocessor (`epp`) sent extra messages on the form
  `{eof,Location}` to the client when parsing the `file` attribute. This bug,
  introduced in R11B, has been fixed.

  Own Id: OTP-8470

- The abstract type 'fun' could not be printed by the Erlang pretty printer
  (`erl_pp`). This bug has been fixed.

  Own Id: OTP-8473

- The function `erl_scan:reserved_word/1` no longer returns `true` when given
  the word `spec`. This bug was introduced in STDLIB-1.15.3 (R12B-3).

  Own Id: OTP-8567

- The documentation of `lists:keysort/2` states that the sort is stable.

  Own Id: OTP-8628 Aux Id: seq11576

- The shell's line editing has been improved to more resemble the behaviour of
  readline and other shells. (Thanks to Dave Peticolas)

  Own Id: OTP-8635

- The Erlang code preprocessor (`epp`) did not correctly handle premature
  end-of-input when defining macros. This bug, introduced in STDLIB 1.16, has
  been fixed.

  Own Id: OTP-8665 Aux Id: OTP-7810

### Improvements and New Features

- The module binary from EEP31 (and EEP9) is implemented.

  Own Id: OTP-8217

- The erlang pretty printer (`erl_pp`) no longer quotes atoms in types.

  Own Id: OTP-8501

- The Erlang code preprocessor (`epp`) now considers records with no fields as
  typed.

  Own Id: OTP-8503

- Added function `zip:foldl/3` to iterate over zip archives.

  Added functions to create and extract escripts. See `escript:create/2` and
  `escript:extract/2`.

  The undocumented function `escript:foldl/3` has been removed. The same
  functionality can be achieved with the more flexible functions
  `escript:extract/2` and `zip:foldl/3`.

  Record fields has been annotated with type info. Source files as been adapted
  to fit within 80 chars and trailing whitespace has been removed.

  Own Id: OTP-8521

- The Erlang parser no longer duplicates the singleton type `undefined` in the
  type of record fields without initial value.

  Own Id: OTP-8522

- A regular expression with many levels of parenthesis could cause a buffer
  overflow. That has been corrected. (Thanks to Michael Santos.)

  Own Id: OTP-8539

- When defining macros the closing right parenthesis before the dot is now
  mandatory.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8562

- Some properties of a compiled re pattern are defined to allow for guard tests.

  Own Id: OTP-8577

- Local and imported functions now override the auto-imported BIFs when the
  names clash. The pre R14 behaviour was that auto-imported BIFs would override
  local functions. To avoid that old programs change behaviour, the following
  will generate an error:

  - Doing a call without explicit module name to a local function having a name
    clashing with the name of an auto-imported BIF that was present (and
    auto-imported) before OTP R14A
  - Explicitly importing a function having a name clashing with the name of an
    autoimported BIF that was present (and autoimported) before OTP R14A
  - Using any form of the old compiler directive `nowarn_bif_clash`

  If the BIF was added or auto-imported in OTP R14A or later, overriding it with
  an import or a local function will only result in a warning,

  To resolve clashes, you can either use the explicit module name `erlang` to
  call the BIF, or you can remove the auto-import of that specific BIF by using
  the new compiler directive `-compile({no_auto_import,[F/A]}).`, which makes
  all calls to the local or imported function without explicit module name pass
  without warnings or errors.

  The change makes it possible to add auto-imported BIFs without breaking or
  silently changing old code in the future. However some current code
  ingeniously utilizing the old behaviour or the `nowarn_bif_clash` compiler
  directive, might need changing to be accepted by the compiler.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8579

- The undocumented, unsupport, and deprecated function `lists:flat_length/1` has
  been removed.

  Own Id: OTP-8584

- A bug in re that could cause certain regular expression matches never to
  terminate is corrected. (Thanks to Michael Santos and Gordon Guthrie.)

  Own Id: OTP-8589

- Nested records can now be accessed without parenthesis. See the Reference
  Manual for examples. (Thanks to YAMASHINA Hio and Tuncer Ayaz.)

  Own Id: OTP-8597

- `receive` statements that can only read out a newly created reference are now
  specially optimized so that it will execute in constant time regardless of the
  number of messages in the receive queue for the process. That optimization
  will benefit calls to `gen_server:call()`. (See `gen:do_call/4` for an example
  of a receive statement that will be optimized.)

  Own Id: OTP-8623

- The beam_lib:cmp/2 function now compares BEAM files in stricter way. The BEAM
  files will be considered different if there are any changes except in the
  compilation information ("CInf") chunk. beam_lib:cmp/2 used to ignore
  differences in the debug information (significant for Dialyzer) and other
  chunks that did not directly change the run-time behavior.

  Own Id: OTP-8625

- When a gen_server, gen_fsm process, or gen_event terminates abnormally,
  sometimes the text representation of the process state can occupy many lines
  of the error log, depending on the definition of the state term. A mechanism
  to trim out parts of the state from the log has been added (using a
  format_status/2 callback). See the documentation.

  Own Id: OTP-8630

- Calling `sys:get_status()` for processes that have globally registered names
  that were not atoms would cause a crash. Corrected. (Thanks to Steve Vinoski.)

  Own Id: OTP-8656

- The Erlang scanner has been augmented with two new tokens: `..` and `...`.

  Own Id: OTP-8657

- Expressions evaluating to integers can now be used in types and function
  specifications where hitherto only integers were allowed ("Erlang_Integer").

  Own Id: OTP-8664

- The compiler optimizes record operations better.

  Own Id: OTP-8668

- The recently added BIFs erlang:min/2, erlang:max/2 and erlang:port_command/3
  are now auto-imported (as they were originally intended to be). Due to the
  recent compiler change (OTP-8579), the only impact on old code defining it's
  own min/2, max/2 or port_command/3 functions will be a warning, the local
  functions will still be used. The warning can be removed by using
  -compile(\{no_auto_import,\[min/2,max/2,port_command/3]\}). in the source
  file.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8669 Aux Id: OTP-8579

- Now, binary_to_term/2 is auto-imported. This will cause a compile warning if
  and only if a module has got a local function with that name.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8671

- The predefined builtin type tid() has been removed. Instead, ets:tid() should
  be used.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8687

## STDLIB 1.16.5

### Fixed Bugs and Malfunctions

- Because of a race condition, using filelib:ensure_dir/1 from multiple
  processes to create the same path or parts of the same directory structure,
  filelib:ensure_dir/1 could return a meaningless \{error,eexist\}. That race
  condition has been eliminated, and \{error,eexist\} will now be returned only
  if there exists a regular file, device file, or some other non-directory file
  with the same name. (Thanks to Tuncer Ayaz.)

  Own Id: OTP-8389

- A number of bugs concerning re and unicode are corrected:

  re:compile no longer loses unicode option, which also fixes bug in re:split.

  re:replace now handles unicode charlist replacement argument

  re:replace now handles unicode RE charlist argument correctly

  re:replace now handles binary unicode output correctly when nothing is
  replaced.

  Most code, testcases and error isolation done by Rory Byrne.

  Own Id: OTP-8394

- The loading of native code was not properly atomic in the SMP emulator, which
  could cause crashes. Also a per-MFA information table for the native code has
  now been protected with a lock since it turns that it could be accessed
  concurrently in the SMP emulator. (Thanks to Mikael Pettersson.)

  Own Id: OTP-8397

- user.erl (used in oldshell) is updated to handle unicode in prompt strings
  (io:get_line/\{1,2\}). io_lib is also updated to format prompts with the 't'
  modifier (i.e. ~ts instead of ~s).

  Own Id: OTP-8418 Aux Id: OTP-8393

- The re module: A regular expression with an option change at the start of a
  pattern that had top-level alternatives could cause overwriting and/or a
  crash. (Thanks to Michael Santos.)

  Own Id: OTP-8438

### Improvements and New Features

- The ability for the gen_server and gen_fsm callback modules to format their
  own state for display under the sys:get_status/1,2 calls has been restored and
  documented. (Thanks to Steve Vinoski.)

  Own Id: OTP-8324

- c:nc/\{1,2\} used to assume that the beam file was created in the same
  directory as the source code and failed to load the code if it was not.
  Corrected to look for the beam file in the current directory or in the
  directory specified by the `{outdir,Dir}` option. (Thanks to Alex Suraci.)

  Own Id: OTP-8337

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

- Shell tab completion now works for quoted module and function names. (Thanks
  to Ulf Wiger.)

  Own Id: OTP-8383

- Explicit top directories in archive files are now optional.

  For example, if an archive (app-vsn.ez) just contains an app-vsn/ebin/mod.beam
  file, the file info for the app-vsn and app-vsn/ebin directories are faked
  using the file info from the archive file as origin. The virtual direcories
  can also be listed. For short, the top directories are virtual if they does
  not exist.

  Own Id: OTP-8387

- Macros overloading has been implemented. (Thanks to Christopher Faulet.)

  Own Id: OTP-8388

- The new function `shell:prompt_func/1` and the new application configuration
  parameter `shell_prompt_func` can be used for customizing the Erlang shell
  prompt.

  Own Id: OTP-8393

- Improved handling of typed records in escripts

  Own Id: OTP-8434

- Added supervisor:count_children/1 to count the number of children being
  managed without the memory impact of which_children/1. (Thanks to Jay Nelson.)

  Own Id: OTP-8436

## STDLIB 1.16.4

### Improvements and New Features

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8201

- \[escript] The restriction that the first line in escripts must begin with
  `#!` has been removed.

  \[escript] Some command line options to the escript executable has now been
  documented. For example you can run an escript in the debugger by just adding
  a command line option.

  \[escript] The documentation of the escript header syntax has been clarified.
  For example the header is optional. This means that it is possible to directly
  "execute" `.erl`, `.beam` and`.zip` files.

  Own Id: OTP-8215

- Optimized array:from_orddict/1, it is now faster and uses less memory if the
  orddict was sparse.

  Changed array:reset/2, it will now never expand the array which it could
  before for non fixed arrays. See the documentation.

  Own Id: OTP-8216

- The Erlang Pretty Printer (`erl_pp`) now puts the leading `[` of list
  comprehensions as well as the leading `<<` of bit string comprehensions on a
  separate line in order to expose the Cover counter of the template.

  Own Id: OTP-8227

- The extension ".xrl" used for Leex input files is now recognized by the
  compiler.

  Own Id: OTP-8232

- Some clarifications have been made in the documentation regarding
  `gen_server`, `gen_fsm`, and `gen_event` behavior when handling `'EXIT'`
  messages from the parent process. For more information see the `m:gen_server`,
  `m:gen_fsm`, and `m:gen_event` documentation.

  Own Id: OTP-8255 Aux Id: seq11419

- The -on_load() directive can be used to run a function when a module is
  loaded. It is documented in the section about code loading in the Reference
  Manual.

  Own Id: OTP-8295

## STDLIB 1.16.3.1

### Fixed Bugs and Malfunctions

- An erroneous type spec for `gen:start/6` caused dialyzer to erroneously issue
  warnings when `{spawn_opt, SpawnOptionList}` was passed in the option list to
  the `gen_server` and `gen_fsm` start functions.

  Own Id: OTP-8068 Aux Id: seq11323, seq11314

## STDLIB 1.16.3

### Fixed Bugs and Malfunctions

- The linter used to crash on invalid `-opaque` declarations.

  Own Id: OTP-8051

- Bugs in `digraph:add_edge/5` and `digraph:del_path/3` have been fixed. (Thanks
  to Crystal Din.)

  Own Id: OTP-8066

- When trying to insert objects with `dets:insert_new()` into a Dets table of
  type `duplicate_bag`, already existing objects would sometimes be duplicated.
  This bug has been fixed. (Thanks to Crystal Din.)

  Own Id: OTP-8070

- Running erlc in a very deep directory (with a path length of more 256 or more
  characters) would cause the emulator to crash in a call to
  [`list_to_atom/1`](`list_to_atom/1`). (Thanks to Chris Newcombe.)

  Own Id: OTP-8124

- A few minor bugs have been fixed in the Erlang Code Preprocessor (`epp`).

  Own Id: OTP-8130

- A bug in The Erlang Meta Interpreter (`erl_eval`) has been fixed: exceptions
  generated in the template of bit string comprehensions were not handled
  properly. (Thanks to Ulf Wiger.)

  Own Id: OTP-8133

### Improvements and New Features

- Option `{capture,none}` was missing in documentation for `re:run/3`.

  Own Id: OTP-8113

- When [`erl_scan:tokens()`](`t:erl_scan:tokens/0`) returns an error tuple
  `{error, ErrorInfo, EndLocation`\}, the list `LeftOverChars` is the remaining
  characters of the input data, starting from `EndLocation`. It used to be the
  empty list.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8129

- The Erlang Meta Interpreter (`erl_eval`) has been somewhat optimized when it
  comes to interpreting `receive`\-expressions. (Thanks to Richard Carlsson.)

  Own Id: OTP-8139

- The Erlang Pretty Printer (`erl_pp`) has been modified as to handle types.

  Own Id: OTP-8150

## STDLIB 1.16.2

### Fixed Bugs and Malfunctions

- The text of tokens returned by the Erlang scanner (`erl_scan`) was sometimes
  empty when the `text` option was given and `StartLocation` was a line. This
  bug has been fixed.

  Own Id: OTP-7965

- The documentation for `base64:decode/1` has been updated to point out that it
  strips whitespace.

  `base64:decode/1` and `base64:mime_decode/1` would sometimes fail instead of
  stripping away non-base64 characters.

  Own Id: OTP-7984

- Two types in the `gen` module were corrected.

  Own Id: OTP-8029 Aux Id: seq11296

- `array:from_orddict([])` and `array:from_list([])` would construct fixed
  arrays instead of extendible arrays.

  Own Id: OTP-8033

### Improvements and New Features

- Interpreted escripts are now tail recursive.

  The function erl_eval:expr/5 has been introduced.

  Own Id: OTP-7933

- `gen_server:call/2,3` will be somewhat faster if the calling process has a
  many messages in its message queue.

  Own Id: OTP-7979

- Random now supports seed with arity one, `random:seed/1`, which takes a
  three-tuple.

  Own Id: OTP-8019

- The `regexp` module now recognizes the escape sequences `\xXY` and `\x{X...}`.

  Own Id: OTP-8024

## STDLIB 1.16.1

### Fixed Bugs and Malfunctions

- The documentation of `dets:open_file/1` now states that the file is repaired
  if it has not been properly closed. (Thanks to Ulf Wiger.)

  Own Id: OTP-7895

### Improvements and New Features

- The Erlang scanner no longer returns the text of tokens when the start
  location is a pair of a line and column unless the new option `text` is
  supplied (incompatibility with R13A).

  There are new functions to access the attributes of tokens:
  `attributes_info/1,2` and `set_attribute/3`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7892 Aux Id: OTP-7810

- Several glitches and performance issues in the Unicode and I/O-system
  implementation of R13A have been corrected.

  Own Id: OTP-7896 Aux Id: OTP-7648 OTP-7887

- The type spec of filelib:wildcard/2 has been corrected.

  Own Id: OTP-7915

- New functions: `gb_sets:is_disjoint/2`, `ordsets:is_disjoint/2`, and
  `gb_sets:is_disjoint/2`.

  Own Id: OTP-7947

- The function `gb_trees:map/2` which was added in R13A is now documented.

  Own Id: OTP-7948

## STDLIB 1.16

### Fixed Bugs and Malfunctions

- Fixed a minor race conditions in `gen_server:start*`: if one of these
  functions returned `{error,Reason}` or `ignore`, the name could still be
  registered (either locally or in `global`).

  A process started by `proc_lib` in some cases depended on its process
  dictionary not to be erased, and would crash when terminating abnormally and
  not generate a proper crash report. This has been corrected (but the initial
  call will not be shown in the error report if the process dictionary has been
  erased). NOTE: There is no longer any need to erase the process dictionary for
  memory conservation reasons, since the actual call arguments are no longer
  saved in the process dictionary.

  Own Id: OTP-7669

- The Erlang preprocessor used wrong line number when stringifying macro
  arguments. (Thanks to John Hughes.)

  Own Id: OTP-7702

- A bug in the `qlc` module has been fixed: merge join sometimes failed to
  return all answers. (Thanks to Bernard Duggan.)

  Own Id: OTP-7714

### Improvements and New Features

- A new option, `key_equality`, has been added to `qlc:table/2`. This option
  makes it possible for `qlc` to better handle tables that use `==/2` when
  comparing keys for equality (examples of such tables are ordered ETS tables
  and gb_table in qlc(3)).

  Own Id: OTP-6674

- The functions `lists:seq/1,2` return the empty list in a few cases when they
  used to generate an exception, for example `lists:seq(1, 0)`. See lists(3) for
  details. (Thanks to Richard O'Keefe.)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7230

- The order of objects visited in select for ordered_set is now documented.

  Own Id: OTP-7339

- It is now possible to debug code in escripts and archives.

  Own Id: OTP-7626

- Support for Unicode is implemented as described in EEP10. Formatting and
  reading of unicode data both from terminals and files is supported by the io
  and io_lib modules. Files can be opened in modes with automatic translation to
  and from different unicode formats. The module 'unicode' contains functions
  for conversion between external and internal unicode formats and the re module
  has support for unicode data. There is also language syntax for specifying
  string and character data beyond the ISO-latin-1 range.

  The interactive shell will support input and output of unicode characters when
  the terminal and operating system supports it.

  Please see the EEP and the io/io_lib manual pages as well as the stdlib users
  guide for details.

  _I/O-protocol incompatibilities:_

  The io_protocol between io_Server and client is updated to handle protocol
  data in unicode formats. The updated protocol is now documented. The
  specification resides in the stdlib _users manual_, which is a new part of the
  manual.

  _io module incompatibilities:_

  The io:put_chars, io:get_chars and io:get_line all handle and return unicode
  data. In the case where binaries can be provided (as to io:put_chars), they
  shall be encoded in UTF-8. When binaries are returned (as by
  io:get_line/get_chars when the io_server is set in _binary mode_) the returned
  data is also _always_ encoded as UTF-8. The file module however still returns
  byte-oriented data, why file:read can be used instead of io:get_chars to read
  binary data in ISO-latin-1.

  _io_lib module incompatibilities:_

  io_lib:format can, given new format directives (i.e "~ts" and "~tc"), return
  lists containing integers larger than 255.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7648 Aux Id: OTP-7580 OTP-7514 OTP-7494 OTP-7443 OTP-7181 EEP10
  EEP11

- The function `pool:attach/1` now returns `already_attached` if the node is
  already attached, rather than `allready_attached` (sic\!). (Thanks to Edwin
  Fine.)

  Own Id: OTP-7653 Aux Id: OTP-7603

- Preprocessor directives are now allowed in escripts. This means that for
  example macros may be used in escripts.

  Own Id: OTP-7662

- When a process started with `proc_lib`, `gen_server`, or `gen_fsm` exits with
  reason `{shutdown,Term}`, a crash report will no longer be generated (to allow
  a clean shutdown, but still provide additional information to process that are
  linked to the terminating process).

  Own Id: OTP-7740 Aux Id: seq10847

- A new BIF, `lists:keyfind/3`, has been added. It works like
  `lists:keysearch/3` except that it does not wrap the returned tuple in a
  `value` tuple in case of success. (Thanks to James Hague for suggesting this
  function.)

  Own Id: OTP-7752

- `lists:suffix(Suffix, List)` used to have a a complexity of
  `length(Suffix)*length(List)` (which could become quite slow for some inputs).
  It has now been re-implemented so that its complexity is
  `length(Suffix)+length(List)`. (Thanks to Richard O'Keefe for the new
  implementation.)

  Own Id: OTP-7797

- The Erlang scanner has been augmented as to return white spaces, comments, and
  exact location of tokens. The functions `string/3`, `tokens/4`, and
  `token_info/1,2` are new. See erl_scan(3) for details.

  `tokens/3,4` have been modified as to return a list of tokens instead of an
  error when `eof` is encountered before the dot.

  Own Id: OTP-7810

- `filelib:fold_files/5` now uses the `re` module instead of the `regexp` module
  for regular expression matching. In practice, this change will not be a
  problem for most regular expressions used for `filelib:fold_files/5`. (The
  major difference in regular expression is that parenthesis and curly brackets
  is treated as literal characters by `regexp` but as special characters by
  `re`; fortunately, those characters are rarely used in filenames.)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7819

- `digraph:new(Type)` will now cause a `badarg` exception if `Type` is not a
  valid type. Similarly, `digraph_utils:subgraph/2,3` will now cause a `badarg`
  if the arguments are invalid. (Those functions used to return error tuples if
  something was wrong.)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7824

- The argument passed to `random:uniform/1` must now be an integer (as stated in
  the documentation). In previous releases, a floating point number was also
  allowed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7827

- The copyright notices have been updated.

  Own Id: OTP-7851

- A few missing match spec functions was added to dbg:fun2ms; exception_trace/0
  and trace/2,3.

  There is a new function queue:member/2.

  A bug in io_lib:fread that made it accidentally concatenate fields separated
  by newline has been corrected. Reported and analyzed by Matthew Palmer to
  erlang-patches.

  Own Id: OTP-7865

## STDLIB 1.15.5

### Fixed Bugs and Malfunctions

- A bug in the `qlc` module has been fixed: when merge joining two query handles
  the temporary file used for equivalence classes was not truncated properly
  which could result in poor performance.

  Own Id: OTP-7552

- The characters 16#C0 and 16#E0 ("A" and "a" with grave accent), were not
  properly converted by the `string:to_lower/1` and `string:to_upper/1`
  functions. (Thanks to Richard O'Keefe.)

  Own Id: OTP-7589

- The function `pool:attach/1` now returns `already_attached` if the node is
  already attached, rather than `allready_attached` (sic\!). (Thanks to Edwin
  Fine.)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7603

- The documentation for `io:get_line/1,2` now mentions that the return value can
  also be `{error,Reason}`.

  Own Id: OTP-7604 Aux Id: seq11063

### Improvements and New Features

- The split function is now added to the re library. Exceptions and errors from
  both run, replace and split are made more consistent.

  Own Id: OTP-7514 Aux Id: OTP-7494

- Processes spawned using `proc_lib` (including `gen_server` and other library
  modules that use `proc_lib`) no longer keep the entire argument list for the
  initial call, but only the arity.

  Also, if `proc_lib:spawn/1` is used to spawn a fun, the actual fun is not
  kept, but only module, function name, and arity of the function that
  implements the fun.

  The reason for the change is that keeping the initial fun (or a fun in an
  argument list), would prevent upgrading the code for the module. A secondary
  reason is that keeping the fun and function arguments could waste a
  significant amount of memory.

  The drawback with the change is that the crash reports will provide less
  precise information about the initial call (only `Module:Function/Arity`
  instead of `Module:Function(Arguments)`). The function
  `proc_lib:initial_call/1` still returns a list, but each argument has been
  replaced with a dummy atom.

  Own Id: OTP-7531 Aux Id: seq11036

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

- Enabled explicit control of which types of files that should be compressed in
  a ZIP archive.

  Own Id: OTP-7549 Aux Id: otp-6622

- In the job control mode, the "s" and "r" commands now take an optional
  argument to specify which shell to start. (Thanks to Robert Virding.)

  Own Id: OTP-7617

## STDLIB 1.15.4

### Fixed Bugs and Malfunctions

- A bug in the calendar module could cause
  calendar:local_time_to_universal_time_dst/1 to return duplicate identical
  values for local times in timezones without DST. Multiple values should only
  be returned when a local time is within the hour occurring twice due to shift
  from DST to non-DST, and certainly only in timezones with DST. The correct
  behaviour is now implemented.

  Own Id: OTP-7344 Aux Id: seq10960

- The documentation of `(d)ets:init_table()` has been corrected. (Thanks to Paul
  Mineiro.)

  Own Id: OTP-7413

- The soft upper limit of 60 on the number of non-white characters on a line,
  which was introduced in R12B-0 for the control sequences `p` and `P` of the
  functions `io:fwrite/2,3` and `io_lib:fwrite/2`, has been removed. This means
  that terms whose printed representation fits on a line will have no NEWLINEs.
  The Erlang shell still uses the 60 character limit, though.

  Own Id: OTP-7421 Aux Id: OTP-6708

- Some debug code has been removed from Dets.

  Own Id: OTP-7424

- The documentation of `dets:match_delete/2` has been corrected. (Thanks to Paul
  Mineiro.)

  Own Id: OTP-7445

- Corrections of digraph(3). (Thanks to Vlad Dumitrescu.)

  Own Id: OTP-7492

- For the process that an escript runs in, the `trap_exit` process flag is now
  `false` instead of `true` (as in previous releases). Scripts that depend on
  the previous (counter-intuitive) behaviour might not work. (Thanks to Bengt
  Kleberg.)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7517

### Improvements and New Features

- The documentation of `lists:(u)sort/2` now states what is expected of an
  ordering function.

  Own Id: OTP-7489

- The re module is extended with repetitive matches (global option) and
  replacement function.

  Own Id: OTP-7494 Aux Id: OTP-7181

- The Erlang shell now displays a nicer error message when evaluating an
  undefined command. (Thanks to Richard Carlsson.)

  Own Id: OTP-7495

## STDLIB 1.15.3

### Fixed Bugs and Malfunctions

- zip:unzip to/from binary with empty directories did not work. (Thanks to
  Martin Dvorak.)

  Own Id: OTP-7248

- The documentation of the control sequence `w` of the `io_lib` module now
  states that floating point numbers are printed accurately.

  Own Id: OTP-7324 Aux Id: OTP-7084

- zip:unzip was not supporting a flavour of the zip format found in jar-files.

  Own Id: OTP-7382 Aux Id: seq10970

### Improvements and New Features

- An experimental module "re" is added to the emulator which interfaces a
  publicly available regular expression library for Perl-like regular
  expressions (PCRE). The interface is purely experimental and _will_ be subject
  to change.

  The implementation is for reference and testing in connection to the relevant
  EEP.

  Own Id: OTP-7181

## STDLIB 1.15.2

### Fixed Bugs and Malfunctions

- When inserting many small objects, Dets sometimes crashed when reaching the
  maximum number of slots. (Thanks to Daniel Goertzen.)

  Own Id: OTP-7146

- Processes linked to the Erlang shell did not get an exit signal when the
  evaluator process was killed. This bug, introduced in R12B-0, has been fixed.

  Own Id: OTP-7184 Aux Id: OTP-6554

- Invalid arguments to `ets:update_counter/3` were not handled correctly. A
  tuple position (`Pos`) less than 1 caused the element directly following the
  key to be updated (as if no position at all had been specified). All invalid
  values for `Pos` will now fail with `badarg`.

  Own Id: OTP-7226

- For certain terminals, io:columns/0 could return 0 instead of enotsup. That is
  now corrected.

  Own Id: OTP-7229 Aux Id: seq10886

- `qlc:info()` can now handle port identifiers, pids, references, and funs.
  (Thanks to Wojciech Kaczmare for reporting this bug.)

  When evaluating the `parent_fun` messages sent to the process calling
  `qlc:cursor()` were sometimes erroneously consumed. This bug has been fixed.

  Own Id: OTP-7232

- `erl_parse:abstract()` can now handle bit strings.

  Own Id: OTP-7234

### Improvements and New Features

- The `queue` module has been rewritten to make it easier to use. Suggestions
  and discussion from and with among others Lev Walkin, Anders Ramsell and Rober
  Virding in december 2007 on erlang-questions@erlang.org. It was also discussed
  to change the internal representation to contain length information which
  would speed up `len/1` but that change has been postponed. Anyone interested
  may write an EEP and try to reach an acceptable compromise for queue overhead
  and thereby the speed of all other operations than `len/1`. The `queue` module
  is now optimized for fast and minimal garbage `in/2` and `out/1` and such. See
  the documentation.

  New functions: `is_queue/1`, [`get/1`](`get/1`), `get_r/1`, `peek/1`,
  `peek_r/1`, `drop/1`, `drop_r/1` and `liat/1`. `is_queue/1` is a new
  predicate, `liat/1` is a correction of an old misspelling, and the others
  (`get`\*, `peek`\* and `drop`\*) are new interface functions.

  Own Id: OTP-7064

- The functions `io_lib:write/1,2` and `io_lib:print/1,4` have been changed when
  it comes to writing floating point numbers. This change affects the control
  sequences `p`, `P`, `w`, and `W` of the `io_lib` module. (Thanks to Bob
  Ippolito for code contribution.)

  Own Id: OTP-7084

- Updated the documentation for `erlang:function_exported/3` and `io:format/2`
  functions to no longer state that those functions are kept mainly for
  backwards compatibility.

  Own Id: OTP-7186

- A new BIF ets:update_element/3. To update individual elements within an
  ets-tuple, without having to read, update and write back the entire tuple.

  Own Id: OTP-7200

- `string:join/2` now accepts an empty list as first argument.

  Own Id: OTP-7231 Aux Id: OTP-6671

- `qlc:info/1,2` accepts a new option, `depth`. The type `SelectedObjects` used
  in the description of `qlc:table/2` has been augmented.

  Own Id: OTP-7238

- [`tuple_size/1`](`tuple_size/1`) and [`byte_size/1`](`byte_size/1`) have been
  substituted for [`size/1`](`size/1`) in the documentation.

  Own Id: OTP-7244

## STDLIB 1.15.1

### Fixed Bugs and Malfunctions

- Ets:select/3 in combination with ets:repair_continuation/2 and ordered_set
  data tables could result in function_clause although used as intended. This is
  now corrected. Thanks to Paul Mineiro for finding and isolating the bug\!

  Own Id: OTP-7025

- The compiler warning for the deprecated function `ftp:close/1` now mentions
  the correct replacement function.

  The warning for the removed functions in the `httpd_util` module have been
  changed to say they have been removed, not merely deprecated. (Thanks to
  Fredrik Thulin.)

  Own Id: OTP-7034 Aux Id: seq10825

- In `(Expr)#r{}` (no fields are updated), `Expr` is no longer evaluated more
  than once. There is also a test that `Expr` is of the correct record type.
  (Thanks to Dominic Williams.)

  Own Id: OTP-7078 Aux Id: OTP-4962

- Documentation bugfixes and clarifications.

  (Thanks to Joern (opendev@gmail.com), Matthias Lang, and Richard Carlsson.)

  Own Id: OTP-7079

- Duplicated objects were sometimes not deleted from the list of answers when a
  QLC table was traversed using a match specification. (Thanks to Dmitri
  Girenko.)

  Own Id: OTP-7114

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

- It is now possible to hibernate a gen_server/gen_event/gen_fsm. In gen_server
  and gen_fsm, hibernation is triggered by returning the atom
  'hibernate' instead of a timeout value. In the gen_event case hibernation is
  triggered by a event handler returning a tuple with an extra element
  containing the atom 'hibernate'.

  Own Id: OTP-7026 Aux Id: seq10817

- Some undocumented debug functionality has been added to Dets.

  Own Id: OTP-7066

- The functions `digraph_utils:is_tree/1`, `digraph_utils:is_arborescence/1`,
  and `digraph_utils:arborescence_root/1` are new.

  Own Id: OTP-7081

- The compiler could generate suboptimal code for record updates if the record
  update code consisted of multiple source code lines.

  Own Id: OTP-7101

## STDLIB 1.15

### Fixed Bugs and Malfunctions

- Bugs have been fixed in `qlc`:

  - Setting the `lookup_fun` option of `qlc:table/2` to `undefined` could cause
    a crash.
  - If a QLC restricted some column of a table in such a way that a traversal
    using a match specification was possible and the QLC also compared the key
    column or some indexed column of the the table with a column of some other
    table, `qlc` always chose to traverse the table first, never considering
    lookup join. This has been changed so that lookup join is always preferred;
    if an initial traversal using the match specification is desired, the query
    needs to be rewritten introducing an extra QLC with the filter(s)
    restricting the column.
  - When trying to find candidates for match specifications and lookup, filters
    using variables from one generator only are ignored unless they are placed
    immediately after the generator and possibly other filters using variables
    from the same generator. In particular, filters joining two tables should
    not be placed between the generator and the filters using the generator
    only.
  - The call-back function `TraverseFun` used for implementing QLC tables is
    allowed to return a term other than a list since STDLIB 1.14 (OTP-5195).
    However, when the returned term was a fun `qlc` often tried to call the fun
    instead of returning it.

  A few minor optimizations have been implemented as well.

  Own Id: OTP-6673

- A bug concerning the use of parameterized modules from the shell has been
  fixed.

  Own Id: OTP-6785

- A bug regarding the size expression of the bit syntax has been fixed in the
  `erl_eval` module.

  Own Id: OTP-6787

- The log_mf_h event handler didn't close the index file when it was done
  reading it causing a file descriptor leak.

  Own Id: OTP-6800

- Definitions for the `filename()` and `dirname()` types have been added to the
  documentation for the `filelib` module.

  Own Id: OTP-6870

- file:write_file/3, file:write/2 and file:read/2 could crash (contrary to
  documentation) for odd enough file system problems, e.g write to full file
  system. This bug has now been corrected.

  In this process the file module has been rewritten to produce better error
  codes. Posix error codes now originate from the OS file system calls or are
  generated only for very similar causes (for example 'enomem' is generated if a
  memory allocation fails, and 'einval' is generated if the file handle in
  Erlang is a file handle but currently invalid).

  More Erlang-ish error codes are now generated. For example `{error,badarg}` is
  now returned from `file:close/1` if the argument is not of a file handle type.
  See file(3).

  The possibility to write a single byte using `file:write/2` instead of a list
  or binary of one byte, contradictory to the documentation, has been removed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6967 Aux Id: OTP-6597 OTP-6291

- A bug concerning the evaluation of the `++/2` operator has been fixed in
  `erl_eval`. (Thanks to Matthew Dempsky.)

  Own Id: OTP-6977

### Improvements and New Features

- The behaviour of the internal functions gen:call/3,4 has been changed slightly
  in the rare case that when the caller was linked to the called server, and the
  server crashed during the call; its exit signal was consumed by the
  gen:call/3,4 code and converted to an exit exception. This exit signal is no
  longer consumed.

  To even notice this change, 1) the calling process has to be linked to the
  called server.

  2. the call must not be remote by name that is it must be local or remote by
     pid, local by name or global by name.

  3. the calling process has to have set
     [`process_flag(trap_exit, true)`](`process_flag/2`).

  4. the server has to crash during the call.

  5. the calling process has to be sensitive to getting previously consumed
     `{'EXIT',Pid,Reason}` messages in its message queue.

  The old behaviour was once the only way for a client to notice if the server
  died, but has since `erlang:monitor(process, {Name,Node})` was introduced and
  used in gen:call been regarded as an undesired behaviour if not a bug.

  The affected user APIs are: `gen_server:call/2,3`,
  `gen_fsm:sync_send_event/2,3`, `gen_fsm:sync_send_all_state_event/2,3`,
  `gen_event:_`, `sys:_` and maybe a few others that hardly will be noticed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-3954 Aux Id: Seq 4538

- When an exception occurs the Erlang shell now displays the class, the reason,
  and the stacktrace in a clearer way (rather than dumping the raw EXIT tuples
  as before). `proc_lib:format/1` displays the exception of crash reports in the
  same clearer way.

  The new shell command `catch_exception` and the new application configuration
  parameter `shell_catch_exception` can be used for catching exceptions that
  would normally exit the Erlang shell.

  Own Id: OTP-6554 Aux Id: OTP-6289

- The function `string:join/2` joins strings in a list with a separator.
  Example: '`string:join(["a", "b", "c"], ", ") gives "a, b, c"`'

  Own Id: OTP-6671

- The control sequence `P` of the `Format` argument of the functions
  `io:fwrite/2,3` and `io_lib:fwrite/2` now inserts fewer line breaks when
  printing tuples and lists. A soft upper limit of 60 on the number of non-white
  characters on a line has been introduced.

  Own Id: OTP-6708

- The new module `array` provides a fast functional array implementation.

  Own Id: OTP-6733

- Functions that have long been deprecated have now been removed from the
  following modules: `dict`, `erl_eval`, `erl_pp`, `io`, `io_lib`, `lists`,
  `orddict`, `ordsets`, `sets`, and `string`.

  The undocumented function `lists:zf/3` has also been removed (use a list
  comprehension or `lists:zf/2` instead).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6845

- Minor documentation corrections for file:pread/2 and file:pread/3.

  Own Id: OTP-6853

- Contract directives for modules in Kernel and STDLIB.

  Own Id: OTP-6895

- The `ets:fixtable/2` function, which has been deprecated for several releases,
  has been removed.

  The `ets:info/1` function has been reimplemented as a BIF, which guarantees
  that information returned is consistent.

  The `ets:info/2` function now fails with reason `badarg` if the second
  argument is invalid. (Dialyzer can be used to find buggy code where the second
  argument is misspelled.)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6906

- The Erlang pretty printer `erl_pp` now inserts more newlines in order to
  facilitate line coverage analysis by `Cover`. (Thanks to Thomas Arts.)

  Own Id: OTP-6911

- The documentation for ets:safe_fixtable/2, ets:foldl/3, and ets:foldr/3 is now
  clearer about what will happen if objects are inserted during table
  traversals.

  Own Id: OTP-6928 Aux Id: seq10779

- It is now possible to extract files in tar files directly into binaries. It is
  also possible to add files to tar files directly from binaries.

  Own Id: OTP-6943

- The functions `keystore/4` and `keytake/3` are new in the `lists` module.

  Own Id: OTP-6953

- The new `qlc` option `tmpdir_usage` can be used for outputting messages onto
  the error logger when a temporary file is about to be created, or to prohibit
  the usage of temporary files altogether.

  Own Id: OTP-6964

## STDLIB 1.14.5.3

### Improvements and New Features

- The allowed syntax for -type() and -spec() was updated.

  Own Id: OTP-6861 Aux Id: OTP-6834

## STDLIB 1.14.5.2

### Improvements and New Features

- The compiler will for forward compatibility ignore the -type() and -spec()
  attributes that will be introduced in the R12B release.

  Own Id: OTP-6834

## STDLIB 1.14.5.1

### Fixed Bugs and Malfunctions

- The log_mf_h event handler didn't close the index file when it was done
  reading it causing a file descriptor leak.

  Own Id: OTP-6800

### Improvements and New Features

- The dict:size/1 and orddict:size/1 functions have been documented.

  Own Id: OTP-6818

## STDLIB 1.14.5

### Fixed Bugs and Malfunctions

- Bugs have been fixed in Dets concerning comparison (==) and matching (=:=).

  The STDLIB manual pages have been updated as to more carefully state when
  terms are matched and when they are compared.

  Own Id: OTP-4738 Aux Id: OTP-4685

- The shell has been updated to fix the following flaws: Shell process exit left
  you with an unresponsive initial shell if not using oldshell. Starting a
  restricted shell with a nonexisting callback module resulted in a shell where
  no commands could be used, not even init:stop/0. Fun's could not be used as
  parameters to local shell functions (in shell_default or user_default) when
  restricted_shell was active.

  Own Id: OTP-6537

- A bug in QLC's parse transform has been fixed.

  Own Id: OTP-6590

- A bug concerning `lists:sort/1` and `lists:keysort/2` and a mix of floating
  point numbers and integers has been fixed.

  Own Id: OTP-6606

- When calling `erlang:garbage_collect/0` in the Erlang shell not only the
  evaluator process (the one returned by calling `self/0` in the Erlang shell)
  is garbage collected, but also the process holding the history list.

  Own Id: OTP-6659

- Functions of the `beam_lib` module that used to catch exceptions and return a
  tuple `{'EXIT',Reason}` now exit with the reason `Reason`.

  Own Id: OTP-6711

- The `erl_eval` module now calls the non-local function handler whenever an
  operator is evaluated (exceptions are `andalso`, `orelse`, and `catch`). The
  non-local function handler is now also called when the function or operator
  occurs in a guard test (such calls used to be ignored).

  These changes affect the Erlang shell when running in restricted mode: the
  callback function `non_local_allowed/3` is now called for operators such as
  `'!'/2`. This means that `non_local_allowed/3` may need to
  be changed as to let operators through. Note that `erlang:'!'/2` as well as
  `erlang:send/2,3` have to be restricted in order to stop message passing in
  the shell.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6714 Aux Id: seq10374

### Improvements and New Features

- The new compiler option `warn_obsolete_guard` can be used for turning on
  warnings for calls to old type testing BIFs.

  Own Id: OTP-6585

- For scripts written using `escript`, there is a new function
  `escript:script_name/0`, which can be used to retrieve the pathame of the
  script. The documentation has been clarified regarding pre-defined macros such
  as ?MODULE and the module name.

  Own Id: OTP-6593

- Minor Makefile changes.

  Own Id: OTP-6689 Aux Id: OTP-6742

## STDLIB 1.14.4

### Fixed Bugs and Malfunctions

- The MD5 calculation of a BEAM file done by `code:module_md5/1`,
  `beam_lib:md5/1`, and by the compiler for the default value of the `vsn`
  attribute have all been changed so that its result will be the same on all
  platforms; modules containing funs could get different MD5s on different
  platforms.

  Own Id: OTP-6459

- When sorting terms using the `file_sorter` module (the option `Format` set to
  `term`), file errors were not always properly handled. This bug has been
  fixed.

  The directory supplied with the `tmpdir` option is no longer checked unless it
  is actually used. The error reason `not_a_directory` can no longer be
  returned; instead a `file_error` tuple is returned

  Own Id: OTP-6526

- Bugs regarding `try`/`catch` have been fixed in the `erl_eval` module.

  Own Id: OTP-6539

- When sorting the operands of a join operation, QLC called `file:open/3` with
  bad arguments. This bug has been fixed.

  Own Id: OTP-6562 Aux Id: seq10606

### Improvements and New Features

- The functions `beam_lib:cmp/1` and `beam_lib:strip/1` (and similar functions)
  have been updated to handle optional chunks (such as "FunT") in more general
  way in order to be future compatible.

  The function `beam_lib:chunks/3` has been added.

  The function `beam_lib:md5/1` has been added.

  Own Id: OTP-6443

- Added base64 as a module to stdlib, encoding and decoding

  Own Id: OTP-6470

- Added the functions to_upper/1 and to_lower/1 to the string module. These
  provide case conversion for ISO/IEC 8859-1 characters (Latin1) and strings.

  Own Id: OTP-6472

- The callback function `non_local_allowed/3` used by the restricted shell can
  now return the value `{{restricted,NewFuncSpec,NewArgList},NewState}` which
  can be used for letting the shell call some other function than the one
  specified.

  Own Id: OTP-6497 Aux Id: seq10555

- There is a new `escript` program that can be used for writing scripts in
  Erlang. Erlang scripts don't need to be compiled and any arguments can be
  passed to them without risk that they are interpreted by the Erlang system.

  Own Id: OTP-6505

- The `Format` argument of the functions `io:fwrite/2,3` and `io_lib:fwrite/2`
  is now allowed to be a binary.

  Own Id: OTP-6517

## STDLIB 1.14.3.1

### Fixed Bugs and Malfunctions

- The control sequences `p` and `P` of the `Format` argument of the functions
  `io:fwrite/2,3` and `io_lib:fwrite/2` could cause a `badarg` failure when
  applied to binaries. This bug was introduced in STDLIB 1.14.3. (Thanks to
  Denis Bilenko.)

  Own Id: OTP-6495

### Improvements and New Features

- Added the option \{cwd, Dir\} to make zip-archives with relative pathnames
  without having to do (a global) file:set_cwd.

  Own Id: OTP-6491 Aux Id: seq10551

## STDLIB 1.14.3

### Fixed Bugs and Malfunctions

- The `spawn_opt/2,3,4,5` option `monitor` \-- introduced in Kernel 2.11.2 -- is
  currently not possible to use when starting a process using `proc_lib`, that
  is, also when starting a gen_server, gen_fsm etc.

  This limitation has now been properly documented and the behavior of the
  `gen_fsm`, `gen_server`, and `proc_lib` `start` and `start_link` functions
  when providing this option has been changed from hanging indefinitely to
  failing with reason `badarg`.

  (Thanks to Fredrik Linder)

  Own Id: OTP-6345

### Improvements and New Features

- The control sequence `P` of the `Format` argument of the functions
  `io:fwrite/2,3` and `io_lib:fwrite/2` now replaces the tail of binary strings
  with `...` when the maximum depth has been reached. For instance,
  `io:fwrite("~P", [<<"a binary string">>, 3]).` prints `<<"a binary"...>>`.

  The indentation takes more care not to exceed the right margin, if possible.

  If the maximum depth is reached while printing a tuple, `,...` is printed
  instead of `|...` (this change applies to the control sequence `W` as well).

  Own Id: OTP-6354

- The Erlang shell command `h/0` that prints the history list now avoids
  printing (huge) terms referred to by `v/1` but instead just prints the call to
  `v/1`.

  Own Id: OTP-6390

## STDLIB 1.14.2.2

### Fixed Bugs and Malfunctions

- The functions `dets:select/1,3`, `dets:match/1,3`, and `dets:match_object/1,3`
  have been changed as to never return `{[],Continuation}`. This change affects
  the corresponding functions in Mnesia.

  Bugs have been fixed in QLC: `qlc:info()` could crash if the `tmpdir` option
  did not designate a valid directory; the results of looking up keys are kept
  in RAM, which should improve performance.

  Own Id: OTP-6359

## STDLIB 1.14.2.1

### Fixed Bugs and Malfunctions

- A bug in `erl_pp:exprs()` has been fixed.

  Own Id: OTP-6321 Aux Id: seq10497

## STDLIB 1.14.2

### Fixed Bugs and Malfunctions

- The control sequences `p` and `P` of the `Format` argument of the functions
  `io:format/2,3` and `io_lib:format/2` did not handle binaries very well. This
  bug, introduced in stdlib-1.14, has been fixed.

  Own Id: OTP-6230

- `filelib:wildcard(Wc, PathWithRedundantSlashes)`, where
  `PathWithRedundantSlashes` is a directory path containing redundant slashes,
  such as `/tmp/` or `//tmp`, could return incorrect results. (Thanks to Martin
  Bjorklund.)

  Own Id: OTP-6271

- The Erlang code preprocessor crashed if the predefined macros ?MODULE or
  ?MODULE_STRING were used before the module declaration. This bug has been
  fixed.

  Own Id: OTP-6277

### Improvements and New Features

- Support for faster join of two tables has been added to the `qlc` module.
  There are two kinds of fast joins: lookup join that uses existing indices, and
  merge join that takes two sorted inputs. There is a new `join` option that can
  be used to force QLC to use a particular kind of join in some QLC expression.

  Several other changes have also been included:

  - The new `tmpdir` option of `cursor/2`, `eval/2`, `fold/4`, and `info/2` can
    be used to set the directory that join uses for temporary files. The option
    also overrides the `tmpdir` option of `keysort/3` and `sort/2`.
  - The new `lookup` option can be used to assert that constants are looked up
    when evaluating some QLC expression.
  - The `cache` and `cache_all` options accept new tags: `ets`, `list`, and
    `no`. The tag `list` caches answers in a list using a temporary file if the
    answers cannot be held in RAM. Combining `{cache,list}` and `{unique, true}`
    is equivalent to calling `sort/2` with the option `unique` set to `true`.
    The old tags `true` (equivalent to `ets`) and `false` (equivalent to `no`)
    are recognized for backward compatibility.
  - The new option `max_list_size` can be used to set the limit where merge join
    starts to use temporary files for large equivalence classes and when answers
    cached in lists are put on temporary files.
  - There is a new callback `is_sorted_key` to be supplied as an option to
    `table/2`.
  - QLC analyzes each and every QLC expression when trying to find constants for
    the lookup function. Hitherto only QLC expressions with exactly one
    generator were analyzed.

    Note that only filters with guard syntax placed immediately after the
    generator are analyzed. The restriction to guard filters is an incompatible
    change. See `m:qlc` for further details.

  - In a similar way several match specifications for traversal of QLC tables
    can be utilized for different generators of one single QLC expression.
  - A bug has been fixed: when caching answers to a sufficiently complex query
    it could happen that some answers were not returned.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6038

- The Erlang pretty printer (`erl_pp`) is now much faster when the code is
  deeply nested. A few minor bugs have been fixed as well.

  Own Id: OTP-6227 Aux Id: OTP-5924

- The Erlang shell now tries to garbage collect large binaries. Under certain
  circumstances such binaries could otherwise linger on for an indefinite amount
  of time.

  Own Id: OTP-6239

- To help Dialyzer find more bugs, many functions in the Kernel and STDLIB
  applications now only accept arguments of the type that is documented.

  For instance, the functions `lists:prefix/2` and `lists:suffix/2` are
  documented to only accept lists as their arguments, but they actually accepted
  anything and returned `false`. That has been changed so that the functions
  cause an exception if one or both arguments are not lists.

  Also, the `string:strip/3` function is documented to take a character argument
  that is a character to strip from one or both ends of the string. Given a list
  instead of a character, it used to do nothing, but will now cause an
  exception.

  Dialyzer will find most cases where those functions are passed arguments of
  the wrong type.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6295

## STDLIB 1.14.1

### Fixed Bugs and Malfunctions

- The functions `c:y/1,2` which call `yecc:file/1,2` are now listed by
  `c:help/0`.

  Documentation of `c:y/1,2` has been added to `m:c`.

  The fact that the control sequence character `s` recognizes binaries and deep
  character lists has been documented in `m:io`. This feature was added in
  R11B-0 (OTP-5403).

  Own Id: OTP-6140

- The shell command rr() sometimes failed to read record definitions from
  file(s). This problem has been fixed.

  Own Id: OTP-6166 Aux Id: OTP-5878

- The nonlocal function handler in `erl_eval`, which is used for implementing
  the restricted mode of the Erlang shell, did not handle calls to
  `erlang:apply/3` correctly. This bug has been fixed.

  Own Id: OTP-6169 Aux Id: seq10374

- ets:rename/1 could deadlock, or crash the SMP emulator when the table wasn't a
  named table.

  ets:next/2, and ets:prev/2 could return erroneous results on the SMP emulator.

  Own Id: OTP-6198 Aux Id: seq10392, seq10415

- When closing a Dets table the space management data was sometimes saved in
  such a way that opening the table could not be done without repairing the
  file. This bug has been fixed.

  Own Id: OTP-6206

## STDLIB 1.14

### Fixed Bugs and Malfunctions

- A bugfix in QLC: two of the call-back functions used for implementing QLC
  tables, `TraverseFun` and `LookupFun`, are now allowed to return a term other
  than a list. Such a term is immediately returned as the results of the current
  query, and is useful mostly for returning error tuples.

  Several other minor bugs have been also been fixed.

  Own Id: OTP-5195

- The STDLIB modules `error_logger_file_h` and `error_logger_tty_h` now read the
  environment variable `utc_log` from the SASL application.

  Own Id: OTP-5535

- `ets:info/1` has been corrected to behave according to the documentation and
  return a list of tuples, not a tuple with tuples.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-5639

- Referencing a so far undeclared record from the default value of some record
  declaration is from now on considered an error by the linter. It is also an
  error if the default value of a record declaration uses or binds a variable.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-5878

- When a file `.hrl` file is included using `-include_lib`, the include path is
  temporarily updated to include the directory the `.hrl` file was found in,
  which will allow that `.hrl` file to itself include files from the same
  directory as itself using `-include`. (Thanks to Richard Carlsson.)

  Own Id: OTP-5944

- Corrected `filelib:ensure_dir/1` which sometimes returned `true` and sometimes
  `ok` to always return `ok` when successful. This goes against the
  documentation which said `true`, but `ok` was judged to be a more logical
  return value.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-5960 Aux Id: seq10240

- The shell now handles records better when used in calls on the form
  `{Module, Function}(ArgList)`.

  Own Id: OTP-5990 Aux Id: OTP-5876

- The functions `lists:ukeysort/2` and `lists:ukeymerge/3` have been changed in
  such a way that two tuples are considered equal if their keys match.

  For the sake of consistency, `lists:usort/2` and `lists:umerge/3` have been
  modified too: two elements are considered equal if they compare equal.

  The `file_sorter` module has been modified in a similar way: the `unique`
  option now applies to the key (`keysort()` and `keymerge()`) and the ordering
  function (the option `{order, Order} `).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6019

- Correction in documentation for `ets:update_counter/3`; failure with `badarg`
  also if the counter to be updated is the key.

  Own Id: OTP-6072

- When sorting terms using the `file_sorter` module and an ordering fun, the
  sort was not always stable. This bug has been fixed.

  Own Id: OTP-6088

### Improvements and New Features

- Improvements of the linter:

  - The `compile` attribute is recognized after function definitions.
  - The new compiler option `nowarn_deprecated_function` can be used for turning
    off warnings for calls to deprecated functions.
  - The new compiler option `{nowarn_unused_function,[{Name,Arity}]}` turns off
    warnings for unused local functions for the mentioned functions. The new
    options `{nowarn_deprecated_function,[{Module,Name,Arity}]}` and
    `{nowarn_bif_clash,[{Name,Arity}]}` work similarly.

  The Erlang code preprocessor `epp` now recognizes the `file` attribute. This
  attribute is meant to be used by tools such as Yecc that generate source code
  files.

  Own Id: OTP-5362

- The formatting option `~s` of `io:fwrite` and `io_lib:fwrite` has been
  extended to handle arguments that are binaries or I/O lists.

  Own Id: OTP-5403

- The control sequences `p` and `P` of the `Format` argument of the functions
  `io:format/2,3` and `io_lib:format/2` have been changed as to display the
  contents of binaries containing printable characters as strings.

  Own Id: OTP-5485

- The linter emits warnings for functions exported more than once in `export`
  attributes.

  Own Id: OTP-5494

- A manual for STDLIB has been added, `stdlib(6)`. It mentions the configuration
  parameters for the Erlang shell.

  Own Id: OTP-5530

- Added the `zip` module with functions for reading and creating zip archives.
  See `m:zip`.

  Own Id: OTP-5786

- Simple-one-for-one supervisors now store the pids of child processes using
  `dict` instead of a list. This significantly improves performance when there
  are many dynamic supervised child processes. (Thanks to Mickaël Rémond et al.)

  Own Id: OTP-5898

- When given the new option '`strict_record_tests`', the compiler will generate
  code that verifies the record type for '`R#record.field`' operations in
  guards. Code that verifies record types in bodies has already been generated
  since R10B, but in this release there will be a '`{badrecord,RecordTag}`'
  instead of a '`badmatch`' if the record verification test fails. See the
  documentation for the `compile` module for more information.

  The Erlang shell always applies strict record tests.

  Own Id: OTP-5915 Aux Id: OTP-5714

- The Erlang pretty printer (`erl_pp`) now tries to insert line breaks at
  appropriate places.

  Own Id: OTP-5924

- The `public` option has been removed from `digraph:new/1`. The reason is that
  several functions in the `digraph` module are implemented using multiple ETS
  accesses, which is not thread safe. (Thanks to Ulf Wiger.)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-5985

- The function `lists:keyreplace/4` checks that the fourth argument (`NewTuple`)
  is a tuple.

  Own Id: OTP-6023

- Added an example of how to reconstruct source code from debug info (abstract
  code) to `m:beam_lib`. (Thanks to Mats Cronqvist who wrote the example.)

  Own Id: OTP-6073

- The new compiler option `warn_unused_record` is used for finding unused
  locally defined record types.

  Own Id: OTP-6105

## STDLIB 1.13.12

### Fixed Bugs and Malfunctions

- `shell_default:xm/1` has been added. It calls `xref:m/1`.

  Own Id: OTP-5405 Aux Id: OTP-4101

- Warnings are output whenever so far undeclared records are referenced from
  some default value of a record declaration. In STDLIB 1.14 (R11B) such forward
  references will cause a compilation error.

  Own Id: OTP-5878

- The linter's check of the `deprecated` attribute did not take the compile
  option `export_all` into account. This bug has been fixed.

  Own Id: OTP-5917

- The Erlang pretty printer did not handle `try/catch` correctly. This bug has
  been fixed.

  Own Id: OTP-5926

- Corrected documentation for `lists:nthtail/3`.

  Added documentation for `lists:keymap/3`.

  Tried to clarify some other type declarations and function descriptions in
  `m:lists`.

  Corrected documentation for `timer:now_diff/2`.

  Fixed broken links in `m:gen_fsm`, `m:gen_server`, `m:io_lib` and `lib(3)`.

  Own Id: OTP-5931

- Type checks have been added to functions in `lists.erl`.

  Own Id: OTP-5939

### Improvements and New Features

- The new STDLIB module `erl_expand_records` expands records in abstract code.
  It is used by the Erlang shell, which means that Compiler is no longer used by
  the shell.

  Own Id: OTP-5876 Aux Id: OTP-5435

- The compiler will now warn that the `megaco:format_versions/1` function is
  deprecated.

  Own Id: OTP-5976

## STDLIB 1.13.11

### Fixed Bugs and Malfunctions

- When calling `gen_server:enter_loop` with a registered server name, it was
  only checked that the registered name existed, not that it actually was the
  name of the calling process.

  Own Id: OTP-5854

### Improvements and New Features

- More detail on `beam_lib:version/1` in documentation.

  Own Id: OTP-5789

- The new function `io:read/3` works like `io:read/1,2` but takes a third
  argument, `StartLine`.

  Own Id: OTP-5813

- The new function `gen_fsm:enter_loop/4,5,6`, similar to
  `gen_server:enter_loop/3,4,5`, has been added.

  Own Id: OTP-5846 Aux Id: seq10163

- The function `c:i/1` is now exported.

  Own Id: OTP-5848 Aux Id: seq10164

## STDLIB 1.13.10

### Fixed Bugs and Malfunctions

- A couple of type errors have been fixed in `sofs`.

  Own Id: OTP-5739

- The pre-processor used to complain that the macro definition
  '`-define(S(S), ??S).`' was circular, which it isn't. (Thanks to Richard
  Carlsson.)

  Own Id: OTP-5777

## STDLIB 1.13.9

### Fixed Bugs and Malfunctions

- The linter, QLC and the module `erl_pp` did not handle the new '`fun M:F/A`'
  construct in all situations. This problem has been fixed.

  Own Id: OTP-5644

### Improvements and New Features

- The manual pages for most of the Kernel and some of the STDLIB modules have
  been updated, in particular regarding type definitions.

  The documentation of the return value for `erts:info/1` has been corrected.

  The documentation for `erlang:statistics/1` now lists all possible arguments.

  Own Id: OTP-5360

- Replaced some tuple funs with the new `fun M:F/A` construct.

  The high-order functions in the lists module no longer accept bad funs under
  any circumstances. '`lists:map(bad_fun, [])`' used to return '`[]`' but now
  causes an exception.

  Unused, broken compatibility code in the `ets` module was removed. (Thanks to
  Dialyzer.)

  Eliminated 5 discrepancies found by Dialyzer in the Appmon application.

  Own Id: OTP-5633

- The `c:i/0` function will now run in a paged mode if there are more than 100
  processes in the system. (Thanks to Ulf Wiger.)

  `erlang:system_info(process_count)` has been optimized and does now return
  exactly the same value as [`length(processes())`](`length/1`). Previously
  `erlang:system_info(process_count)` did not include exiting processes which
  are included in [`length(processes())`](`length/1`).

  The `+P` flag for `erl`, which sets the maximum number of processes allowed to
  exist at the same, no longer accepts values higher than 134217727. (You will
  still probably run out of memory before you'll be able to reach that limit.)

  Own Id: OTP-5645 Aux Id: seq9984

## STDLIB 1.13.8

### Fixed Bugs and Malfunctions

- Very minor corrections in `beam_lib` and its documentation.

  Own Id: OTP-5589

### Improvements and New Features

- The `erlang:port_info/1` BIF is now documented. Minor corrections of the
  documentation for `erlang:port_info/2`.

  Added a note to the documentation of the `math` module that all functions are
  not available on all platforms.

  Added more information about the '`+c`' option in the `erl` man page in the
  ERTS documentation.

  Own Id: OTP-5555

- The new `fun M:F/A` construct creates a fun that refers to the latest version
  of `M:F/A`. This syntax is meant to replace tuple funs `{M,F}` which have many
  problems.

  The new type test [`is_function(Fun,A)`](`is_function/2`) (which may be used
  in guards) test whether `Fun` is a fun that can be applied with `A` arguments.
  (Currently, `Fun` can also be a tuple fun.)

  Own Id: OTP-5584

## STDLIB 1.13.7

### Fixed Bugs and Malfunctions

- `filelib:wildcard/2` was broken (it ignored its second argument).

  Also, `filelib:wildcard("Filename")` (where the argument does not contain any
  meta-characters) would always return `["Filename"]`. Corrected so that an
  empty list will be returned if `"Filename"` does not actually exist. (Same
  correction in `filelib:wildcard/2`.) (This change is a slight
  incompatibility.)

  `filelib:wildcard/1,2` will generate a different exception when given bad
  patterns such as `"{a,"`. The exception used to be caused by
  '[`exit(missing_delimiter)`](`exit/1`)' but is now
  '`erlang:error({badpattern,missing_delimiter})`'.

  Own Id: OTP-5523 Aux Id: seq9824

### Improvements and New Features

- Further improvements of encrypted debug info: New option `encrypt_debug_info`
  for compiler.

  Own Id: OTP-5541 Aux Id: seq9837

## STDLIB 1.13.6

### Fixed Bugs and Malfunctions

- When opening a Dets table read only an attempt was sometimes made to re-hash
  the table resulting in an error message. This problem has been fixed.

  Own Id: OTP-5487 Aux Id: OTP-4989

### Improvements and New Features

- It is now possible to encrypt the debug information in Beam files, to help
  keep the source code secret. See the documentation for `compile` on how to
  provide the key for encrypting, and the documentation for `beam_lib` on how to
  provide the key for decryption so that tools such as the Debugger, Xref, or
  Cover can be used.

  The `beam_lib:chunks/2` functions now accepts an additional chunk type
  `compile_info` to retrieve the compilation information directly as a term.
  (Thanks to Tobias Lindahl.)

  Own Id: OTP-5460 Aux Id: seq9787

## STDLIB 1.13.5

### Fixed Bugs and Malfunctions

- Closing a Dets table kept in RAM would cause a crash if the file could not be
  written. This problem has been fixed by returning an error tuple.

  Own Id: OTP-5402

- `erl_pp` now correctly pretty-prints `fun F/A`.

  Own Id: OTP-5412

- The Erlang shell failed if the compiler was not in the code path. This problem
  has been fixed, but in order to evaluate records the compiler is still needed.

  Own Id: OTP-5435

- Corrected the example in the documentation for `ets:match/2`. Also clarified
  that `ets:update_counter/3` updates the counter atomically. (Thanks to Anders
  Svensson.)

  Own Id: OTP-5452 Aux Id: seq9770, seq9789

### Improvements and New Features

- The possibility to start the Erlang shell in parallel with the rest of the
  system was reintroduced for backwards compatibility in STDLIB 1.13.1. The flag
  to be used for this is now called `async_shell_start` and has been documented.
  New shells started from the JCL menu are not synchronized with `init` anymore.
  This makes it possible to start a new shell (e.g. for debugging purposes) even
  if the initial shell has not come up.

  Own Id: OTP-5406 Aux Id: OTP-5218

- The compiler will now produce warnings when using the deprecated functions in
  the `snmp` module.

  Own Id: OTP-5425

- The function `c:zi/0` has been removed. Use `c:i/0` instead.

  Own Id: OTP-5432

- Corrected two minor bugs found by the Dialyzer: Calling a parameterized module
  from a restricted shell (i.e. if `shell:start_restricted/1` has been used)
  would crash the shell evaluator. A debug printout in `gen_fsm` had a clause
  that would never match; causing less information to be printed.

  And a somewhat more serious one also found by Dialyzer: `rpc:yield/1` would
  crash unless the call started by `rpc:async_call/4` had already finished;
  `rpc:nb_yield(Key,infinity)` would also crash.

  Cleaned up and removed redundant code found by Dialyzer in
  `erlang:dmonitor_p/2`.

  Own Id: OTP-5462

## STDLIB 1.13.4

### Fixed Bugs and Malfunctions

- Bugs in the Erlang shell have been fixed.

  Own Id: OTP-5327

- Some dead code reported by Dialyzer was eliminated.

  A bug in `dbg` when tracing to wrap trace files has been corrected. It failed
  to delete any already existing wrap trace files with the same names when
  starting a new wrap trace.

  Own Id: OTP-5329

- The linter could output invalid warnings about bit patterns in record
  initializations. This problem has been fixed.

  Own Id: OTP-5338

- `ordsets:is_set(NoList)`, where `NoList` is any term except a list, would
  crash. For consistency with `sets:is_set/1` and `gb_sets:is_set/1`, it now
  returns `false`.

  Own Id: OTP-5341

- A BIF `erlang:raise/3` has been added. See the manual for details. It is
  intended for internal system programming only, advanced error handling.

  Own Id: OTP-5376 Aux Id: OTP-5257

### Improvements and New Features

- The `deprecated` attribute is now checked by the linter. See `m:xref` for a
  description of the `deprecated` attribute.

  Own Id: OTP-5276

- The restricted shell will now indicate if the return value from a user
  predicate is on an incorrect form.

  Own Id: OTP-5335

## STDLIB 1.13.3

### Fixed Bugs and Malfunctions

- Bugs concerning unused and shadowed variables have been fixed in the linter.

  Own Id: OTP-5091

- A bug in the evaluator that caused the shell to choke on bit syntax
  expressions has been fixed.

  Own Id: OTP-5237

- `io:format/2` et.al no longer crashes for some combinations of precision and
  value for format character "g". Previously it crashed if the precision P was 4
  or lower and the absolute value of the float to print was lower than 10^4 but
  10^(P-1) or higher. Now it will not crash depending on the value of the float.

  Own Id: OTP-5263

- Bugs in the handling of the bit syntax have been fixed in the Erlang shell.

  Own Id: OTP-5269

- `gb_sets:del_element/2` was changed to do the same as `gb_sets:delete_any/2`
  which was the original intention, not as `gb_sets:delete/2`. Code that relies
  on `gb_sets:del_element/2` causing an error if the element does not exist must
  be changed to call `gb_sets:delete/2` instead.

  The documentation was also updated to explicitly document functions that were
  only referred to as 'aliases' of a documented function. Also, a list of all
  functions common to the `gb_sets`, `sets`, and `ordsets` was added.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-5277

- Debug messages have been removed from the QLC module.

  Own Id: OTP-5283

### Improvements and New Features

- The size of continuations returned from `dets:match/1,3`,
  `dets:match_object/1,3`, and `dets:select/1,3` has been reduced. This affects
  the amount of data Mnesia sends between nodes while evaluating QLC queries.

  Own Id: OTP-5232

## STDLIB 1.13.2

### Improvements and New Features

- The `-rsh` switch for starting a remote shell (introduced with OTP-5210)
  clashed with an already existing switch used by `slave`. Therefore the switch
  for the remote shell is now instead named `-remsh`.

  Own Id: OTP-5248 Aux Id: OTP-5210

## STDLIB 1.13.1

### Fixed Bugs and Malfunctions

- The Pman 'trace shell' functionality was broken as has now been fixed.
  Furthermore, Pman could not correctly find the pid of the active shell if more
  than one shell process was running on the node. This has also been corrected.

  Own Id: OTP-5191

- When the undocumented feature "parameterized modules" was used, the ?MODULE
  macro did not work correctly.

  Own Id: OTP-5224

### Improvements and New Features

- You can now start Erlang with the `-rsh` flag which gives you a remote initial
  shell instead of a local one. Example:

  ```text
              erl -sname this_node -rsh other_node@other_host
  ```

  Own Id: OTP-5210

- The man page for the `lists` module has been updated with description of the
  new `zip`, `unzip`, and `partition/2` functions.

  Own Id: OTP-5213

- The top level group leader used to be listed as job #1 in the job list in JCL
  mode. Since there is no shell associated with this process that can be
  connected to, it will no longer be listed.

  Own Id: OTP-5214

- The possibility to start the Erlang shell in parallel with the rest of the
  system has been reintroduced for backwards compatibility. Note that this old
  behaviour is error prone and should not be used unless for some reason
  necessary.

  Own Id: OTP-5218 Aux Id: seq9534

- The `shell` commands `rr/1,2,3` now accepts wildcards when reading record
  definitions from BEAM files.

  Own Id: OTP-5226
