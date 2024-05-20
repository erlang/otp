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
# Dialyzer Release Notes

This document describes the changes made to the Dialyzer application.

## Dialyzer 5.2

### Improvements and New Features

- The `--gui` option for Dialyzer has been removed.

  Own Id: OTP-18667 Aux Id: [PR-7443]

- The documentation has been migrated to use Markdown and ExDoc.

  Own Id: OTP-18955 Aux Id: [PR-8026]

[PR-7443]: https://github.com/erlang/otp/pull/7443
[PR-8026]: https://github.com/erlang/otp/pull/8026

## Dialyzer 5.1.3

### Fixed Bugs and Malfunctions

* Fixed an issue with bitstring type inference on segments following UTF-8/16/32 segments.

  Own Id: OTP-19068 Aux Id: GH-8383

## Dialyzer 5.1.2

### Fixed Bugs and Malfunctions

- Fix `dialyzer --output` flag to work. This option was accidentally removed in
  OTP 26.0.

  Own Id: OTP-18767 Aux Id: PR-7657

- Fixed a crash in contract checking relating to opaque types.

  Own Id: OTP-18772 Aux Id: GH-7676

## Dialyzer 5.1.1

### Fixed Bugs and Malfunctions

- Fixed a bug that caused dialyzer to crash when analyzing bogus code that
  contained the literal atom `undefined` in segment sizes.

  Own Id: OTP-18629 Aux Id: GH-7325

- Dialyzer could crash when attempting to analyze a module that defined a type
  called `product/`.

  Own Id: OTP-18738 Aux Id: GH-7584

## Dialyzer 5.1

### Fixed Bugs and Malfunctions

- When checking behaviors, Dialyzer could generate false warning that a callback
  function did not have the correct type according to the spec in the behavior
  definition.

  Own Id: OTP-18237 Aux Id: GH-6221, PR-6243

- In a spec, [`list(none())`](`t:list/1`) used to mean `t:none/0`. It has now
  been corrected to mean the empty list.

  Own Id: OTP-18276 Aux Id: GH-6333

- The compiler would silently accept singleton (unbound) type variables in a
  union type. Starting from Erlang/OTP 26, the compiler will generate a warning
  for this example. The warning can be disabled using the
  `nowarn_singleton_typevar` option. In Erlang/OTP 27, the warning will become
  an error.

  Own Id: OTP-18389 Aux Id: GH-6508, PR-6864, GH-7116

- Fixed a bug that prevented the `--plts` option from being used together with
  `--add-to-plt`.

  Own Id: OTP-18485 Aux Id: GH-6850, PR-6854

- Fixed a crash when analyzing code that contained illegal bitstring segment
  sizes.

  Own Id: OTP-18562

- Fixed a crash when formatting certain warnings that contained multi-byte
  unicode characters.

  Own Id: OTP-18564 Aux Id: GH-7153

### Improvements and New Features

- Dialyzer has a new incremental mode that be invoked by giving the
  `--incremental` option when running Dialyzer. This new incremental mode is
  likely to become the default in a future release.

  Incremental mode primarily differs from the previous, "classic", ways of
  running Dialyzer, in that its model is optimised around the common use case of
  regularly analysing a single codebase, tweaking the code, analysing it again,
  and so on, without explicit reference to the building and checking of a PLT.

  In this mode the PLT file acts much more like a true cache, where users
  provide a codebase and a set of files they care about, and Dialyzer does the
  legwork in terms of deciding how to most efficiently report all of the
  relevant warnings given the cached results it may already have in the PLT (and
  if a PLT doesn't exist, incremental mode will create one).

  Own Id: OTP-18188 Aux Id: PR-5997

- Dialyzer now produces clearer error messages for contract violations.

  Own Id: OTP-18238 Aux Id: PR-6271

- The name of a built-in type can now be reused as the name of type locally.
  That is useful when an OTP release introduces a new built-in type; having the
  possibility to redefine built-in types locally can make it easier to maintain
  code that works in multiple OTP releases.

  Own Id: OTP-18282 Aux Id: GH-6132, PR-6335

- There is new option `-no_spec` to ignore all specs. It is useful for debugging
  when one suspects that some specs could be incorrect.

  Own Id: OTP-18310

- Dialyzer's overloaded domain warning is now disabled by default, and can be
  enabled with the flag `-Woverlapping_contract`.

  Dialyzer used to issue a warning for overloaded domains stating
  `"such contracts are currently unsupported and are simply ignored"`.

  These contracts are not "ignored" but rather, Dialyzer takes the union of the
  overloaded domains. This means that we lose the dependency from each
  corresponding input to output type. Because of this, the warning is really
  about not being able to establish a dependency between the input and output
  types of each respective overloaded function specification.

  Own Id: OTP-18342 Aux Id: GH-6117, PR-6654

- Dialyzer has enabled (by default) warnings about unknown types and functions.

  Prior to this change, Dialyzer had warnings about unknown types and functions
  disabled (by default).

  This default value has been overwritten; Dialyzer now warns about unknown
  types and functions (as requested by the community in GH-5695). Thus, the
  following two examples are equivalent, i.e., passing the `-Wunknown` function
  is enabled by default:

  `dialyzer moduler.erl -Wunknown -Wmissing_return`

  `dialyzer moduler.erl -Wmissing_return`

  Dialyzer has a new flag, `-Wno_unknown`. Its purpose is to suppress warnings
  about unknown functions and types.

  Users who wish to suppress these warnings can invoke Dialyzer using this flag.
  Example: `dialyzer module.erl -Wno_unknown`

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18439 Aux Id: GH-5695,PR-6822, GH-6942

- Deprecates `dbg:stop_clear/0` because it is simply a function alias to
  `dbg:stop/0`

  Own Id: OTP-18478 Aux Id: GH-6903

- Added the new built-in type `t:dynamic/0` introduced in EEP-61, improving
  support for gradual type checkers.

  Own Id: OTP-18522

- Added the `argparse` module for simplified argument handling in escripts and
  similar.

  Own Id: OTP-18558 Aux Id: PR-6852

## Dialyzer 5.0.5

### Fixed Bugs and Malfunctions

- Fixed a bug that would cause analysis to crash.

  Own Id: OTP-18372 Aux Id: GH-6580

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

## Dialyzer 5.0.4

### Fixed Bugs and Malfunctions

- Dialyzer would crash when attempting to analyze a bit syntax segment size
  having an literal non-integer size such as `[]`.

  Own Id: OTP-18307 Aux Id: GH-6419, GH-6473

- Dialyzer could crash when trying to analyze a convoluted nested expression
  involving funs,

  Own Id: OTP-18347 Aux Id: GH-6518, PR-6525

## Dialyzer 5.0.3

### Fixed Bugs and Malfunctions

- Dialyzer could crash when analyzing Elixir code that used intricate macros.

  Own Id: OTP-18262 Aux Id: GH-6323

### Improvements and New Features

- The `--input_list_file` option has been added.

  Own Id: OTP-18263 Aux Id: ERIERL-821

## Dialyzer 5.0.2

### Fixed Bugs and Malfunctions

- Two bugs have been fixed in Dialyzer's checking of behaviors:

  When a _mandatory_ callback function is present but not exported, Dialyzer
  would not complain about a missing callback.

  When an _optional_ callback function was not exported and had incompatible
  arguments and/or the return values were incompatible, Dialyzer would complain.
  This has been changed to suppress the warning, because the function might not
  be intended to be a callback function, for instance if a release added a new
  optional callback function (such as `format_status/1` for the gen_server
  behaviour added in OTP 25).

  Own Id: OTP-18127 Aux Id: ERIERL-817

- The `no_extra_return` and `no_missing_return` warnings can now be suppressed
  through `-dialyzer` directives in source code.

  Own Id: OTP-18148 Aux Id: PR-6068

## Dialyzer 5.0.1

### Fixed Bugs and Malfunctions

- Fixed the documentation for the `missing_return` and `extra_return` options.

  Own Id: OTP-18120

## Dialyzer 5.0

### Fixed Bugs and Malfunctions

- Fixed a bug that could cause the type analyzer to enter an infinite loop.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17644 Aux Id: PR-5223

### Improvements and New Features

- Optimize operations in the `erl_types` module. Parallelize the Dialyzer pass
  `remote`.

  Own Id: OTP-17524

- Added the `missing_return` and `extra_return` options to raise warnings when
  specifications differ from inferred types. These are similar to, but not quite
  as verbose as `overspecs` and `underspecs`.

  Own Id: OTP-17654 Aux Id: GH-5214

- The `race_conditions` option has been removed.

  Own Id: OTP-17819

- The default location of the plt has been changed from `$HOME` to
  `filename:basedir(user_cache,"erlang")`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17821 Aux Id: GH-5016 PR-5408 OTP-17554

- `dialyzer` will now honor dependencies inside type declarations. That is, if
  the declaration of an exported type changes, all modules using said type will
  be revisited.

  Own Id: OTP-17826 Aux Id: PR-5498

- Dialyzer now better understands the types for [`min/2`](`min/2`),
  [`max/2`](`max/2`), and `erlang:raise/3`. Because of that, Dialyzer can
  potentially generate new warnings. In particular, functions that use
  `erlang:raise/3` could now need a spec with a `t:no_return/0` return type to
  avoid an unwanted warning.

  Own Id: OTP-17897 Aux Id: PR-5651

- The `typer_core` module has been added to provide an Erlang API for running
  `typer`.

  Own Id: OTP-17964 Aux Id: PR-5660

- Added the `--annotate-in-place` option to `typer`, which can be used to
  annotate the specs that the tool inferred directly into the source code files.

  Own Id: OTP-18035 Aux Id: PR-5802

## Dialyzer 4.4.4.1

### Improvements and New Features

- The `--input_list_file` option has been added.

  Own Id: OTP-18263 Aux Id: ERIERL-821

## Dialyzer 4.4.4

### Fixed Bugs and Malfunctions

- There could be spurious warnings for unknown types when a type was a subtype
  of an existing type that was a subtype of an unknown type.

  Own Id: OTP-17963 Aux Id: GH-5764

## Dialyzer 4.4.3

### Fixed Bugs and Malfunctions

- Fixed a crash when opaque types contained certain unicode characters.

  Own Id: OTP-17643 Aux Id: GH-5210

- When the compiler is invoked by Dialyzer, it will no longer apply an
  optimization of binary patterns that would turn the pattern `<<"bar">>` into
  `<<6447474:24>>`, which would be very confusing when printed out by Dialyzer.

  Own Id: OTP-17768 Aux Id: GH-5429

## Dialyzer 4.4.2

### Fixed Bugs and Malfunctions

- Do not crash if a PLT file no longer exists.

  Own Id: OTP-17511 Aux Id: GH-4501

- Fix bug in `erl_types` related to maps.

  Own Id: OTP-17537

- Fix bugs in `erl_types` regarding improper lists.

  Own Id: OTP-17541

- The `underspecs` and `overspecs` options will now generate correct warnings
  for misused opaque types.

  Own Id: OTP-17616 Aux Id: GH-5118

## Dialyzer 4.4.1

### Fixed Bugs and Malfunctions

- Do not expose line number `0` in messages if there are other locations to use.

  Own Id: OTP-17443 Aux Id: GH-4890

- In rare circumstances, Dialyzer could crash analyzing code with a list
  comprehension whose value was ignored. (Thanks to Ulf Wiger for reporting this
  bug.)

  Own Id: OTP-17482

## Dialyzer 4.4

### Improvements and New Features

- Some internal HiPE modules have been moved into the dialyzer application so
  that dialyzer works when HiPE is disabled.

  Own Id: OTP-16883

- The experimental HiPE application has been removed, together with all related
  functionality in other applications.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16963

- Add warning option `no_underspecs`.

  Own Id: OTP-16986 Aux Id: ERL-1379, ERL-1480, GH-4033

- Report filename and location for warnings returned due to the `-Wunknown`
  option. When used from the command-line, one location per file is printed.

  Own Id: OTP-16995 Aux Id: ERL-1348

- Add types and specifications for documentation.

  Own Id: OTP-17084

- Add option `error_location`. The option is recognized if included in the
  environment variable ERL_COMPILER_OPTIONS.

  Own Id: OTP-17177 Aux Id: OTP-16824

- Clarify how to declare records used in match patterns.

  Own Id: OTP-17183 Aux Id: ERL-892, GH-4493

## Dialyzer 4.3.1.2

### Improvements and New Features

- The `--input_list_file` option has been added.

  Own Id: OTP-18263 Aux Id: ERIERL-821

## Dialyzer 4.3.1.1

### Fixed Bugs and Malfunctions

- In rare circumstances, Dialyzer could crash analyzing code with a list
  comprehension whose value was ignored. (Thanks to Ulf Wiger for reporting this
  bug.)

  Own Id: OTP-17482

## Dialyzer 4.3.1

### Fixed Bugs and Malfunctions

- Correct handling of PLTs in the GUI.

  Own Id: OTP-17091

## Dialyzer 4.3

### Improvements and New Features

- Clarify warning option `-Wunmatched_returns` in `m:dialyzer`.

  Own Id: OTP-17068 Aux Id: ERL-1223

## Dialyzer 4.2.1

### Fixed Bugs and Malfunctions

- In rare circumstance, dialyzer wold crash when analyzing a list comprehension.

  Own Id: OTP-16813 Aux Id: ERL-1307

## Dialyzer 4.2

### Improvements and New Features

- Improve handling of `maps:remove/2`.

  Own Id: OTP-16055 Aux Id: ERL-1002

## Dialyzer 4.1.1

### Fixed Bugs and Malfunctions

- Fix a bug where warnings about overspecified functions were erroneously
  emitted. Only overloaded functions were affected by the bug.

  Own Id: OTP-16292

### Improvements and New Features

- Remove test data with GNU license.

  Own Id: OTP-16146

## Dialyzer 4.1

### Improvements and New Features

- Allow native compilation when using Dialyzer from Erlang. The options `native`
  (defaults to `false`) and `native_cache` have been added.

  Own Id: OTP-15880 Aux Id: PR-2283

## Dialyzer 4.0.3

### Fixed Bugs and Malfunctions

- The HiPE compiler would badly miscompile certain try/catch expressions, so it
  will now refuse to compile modules containing try or catch.

  As a consequence of this, `dialyzer` will no longer compile key modules to
  native code.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15949

## Dialyzer 4.0.2

### Fixed Bugs and Malfunctions

- Make sure Dialyzer does not crash if the formatting of results fails. Instead
  of crashing, an unformatted version of the results is returned.

  Own Id: OTP-15922 Aux Id: PR-2240, ERL-949

## Dialyzer 4.0.1

### Fixed Bugs and Malfunctions

- Fix a bug that caused a crash when indenting a Dialyzer warning mentioning
  more than one record field.

  Own Id: OTP-15861 Aux Id: ERL-953

## Dialyzer 4.0

### Fixed Bugs and Malfunctions

- All incorrect (that is, all) uses of "can not" has been corrected to "cannot"
  in source code comments, documentation, examples, and so on.

  Own Id: OTP-14282 Aux Id: PR-1891

### Improvements and New Features

- By default Dialyzer inserts line breaks in types, contracts, and Erlang Code
  when formatting results. Use the new `--no_indentation` option to get the old
  behavior of not inserting line breaks.

  Own Id: OTP-15135

- Use bit syntax in warnings instead of Core Erlang syntax, for readability.

  Own Id: OTP-15752

- The format of the raw analysis result tagged with `fun_app_args` is changed to
  `{fun_app_args, [ArgNs, Args, Type]}`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15779

## Dialyzer 3.3.2

### Fixed Bugs and Malfunctions

- Fix a bug that caused Dialyzer to crash when analyzing a contract with a
  module name differing from the analyzed module's name. The bug was introduced
  in Erlang/OTP 18.

  Own Id: OTP-15562 Aux Id: ERL-845

- Fix a bug in the handling of the `Key` argument of
  `lists:{keysearch, keyfind, keymember}`.

  Own Id: OTP-15570

- Optimize (again) Dialyzer's handling of left-associative use of `andalso` and
  `orelse` in guards.

  Own Id: OTP-15577 Aux Id: ERL-851, PR-2141, PR-1944

## Dialyzer 3.3.1

### Improvements and New Features

- Optimize Dialyzer's handling of left-associative use of `andalso` and `orelse`
  in guards.

  Own Id: OTP-15268 Aux Id: ERL-680

## Dialyzer 3.3

### Improvements and New Features

- Changed the default behaviour of `.erlang` loading: `.erlang` is no longer
  loaded from the current directory. `c:erlangrc(PathList)` can be used to
  search and load an `.erlang` file from user specified directories.

  `escript`, `erlc`, `dialyzer` and `typer` no longer load an `.erlang` at all.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14439

- Dialyzer can no longer read BEAM files created with OTP 19 or earlier.

  Own Id: OTP-14493 Aux Id: PR-1434

- Speed up the computation of MD5 sums.

  Own Id: OTP-14937 Aux Id: PR-1719

- Fix a situation where Dialyzer unnecessarily discarded contract information,
  resulting in missed warnings.

  Own Id: OTP-14970 Aux Id: PR-1722

- The (not recommended) option `-Woverspecs` is somewhat refined, and generates
  warnings in a few more cases.

  Own Id: OTP-14982 Aux Id: OTP-14970, PR-1722

- Do not emit warnings for fun expressions residing in code that cannot be run.
  This is consistent with how Dialyzer treats other code that cannot be run.

  Own Id: OTP-15079 Aux Id: ERL-593

## Dialyzer 3.2.4

### Fixed Bugs and Malfunctions

- Fix bugs concerning `erlang:abs/1` and `erlang:bsl/2`.

  Own Id: OTP-14858 Aux Id: ERL-551

- Fix a bug that caused Dialyzer to crash instead of emitting a warning.

  Own Id: OTP-14911

- Fix a bug concerning parameterized opaque types.

  Own Id: OTP-14925 Aux Id: ERL-565

## Dialyzer 3.2.3

### Fixed Bugs and Malfunctions

- The error message returned from Dialyzer when, for example, a modified record
  field type is not a subtype of the declared type, no longer includes a call
  stack. The bug was introduced in Erlang/OTP 19.3.

  Own Id: OTP-14742

- A bug relating to maps and never returning functions has been fixed.

  Own Id: OTP-14743

## Dialyzer 3.2.2

### Fixed Bugs and Malfunctions

- Fix a bug regarding map types that caused Dialyzer to go into an infinite
  loop. A consequence of the fix is that compound map keys such as maps and
  tuples sometimes are handled with less precision than before.

  Own Id: OTP-14572 Aux Id: seq13319

### Improvements and New Features

- General Unicode improvements.

  Own Id: OTP-14462

- The check for unknown remote types is improved.

  Own Id: OTP-14606 Aux Id: OTP-14218

## Dialyzer 3.2.1

### Fixed Bugs and Malfunctions

- Fix a bug where merging PLT:s could lose info. The bug was introduced in
  Erlang/OTP 20.0.

  Own Id: OTP-14558 Aux Id: ERIERL-53

## Dialyzer 3.2

### Fixed Bugs and Malfunctions

- The check of bad type variables in type declarations was mistakingly removed
  in Erlang/OTP 18, and is now re-introduced.

  Own Id: OTP-14423 Aux Id: OTP-14323

### Improvements and New Features

- Analyzing modules with binary construction with huge strings is now much
  faster. The compiler also compiles such modules slightly faster.

  Own Id: OTP-14125 Aux Id: ERL-308

- The peak memory consumption is reduced.

  Own Id: OTP-14127

- Warnings about unknown types are now also generated for types not used by any
  function specification.

  Own Id: OTP-14218 Aux Id: OTP-14127

- TypEr has been removed as separate application and is now a part of the
  Dialyzer application. Documentation for TypEr has been added in the Dialyzer
  application.

  Own Id: OTP-14336

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

## Dialyzer 3.1.1

### Fixed Bugs and Malfunctions

- Report unknown types properly. A bug was introduced in Erlang/OTP 19.3, where
  warnings about unknown types were simply discarded.

  Own Id: OTP-14368

## Dialyzer 3.1

### Fixed Bugs and Malfunctions

- Fix a bug concerning parameterized opaque types.

  Own Id: OTP-14130

- Improve a few warnings. One of them could cause a crash.

  Own Id: OTP-14177

- The dialyzer and observer applications will now use a portable way to find the
  home directory. That means that there is no longer any need to manually set
  the HOME environment variable on Windows.

  Own Id: OTP-14249 Aux Id: ERL-161

### Improvements and New Features

- The peak memory consumption is reduced.

  The evaluation of huge SCCs in `dialyzer_typesig` is optimized.

  Analyzing modules with binary construction with huge strings is now much
  faster.

  Own Id: OTP-14126 Aux Id: ERL-308

## Dialyzer 3.0.3

### Fixed Bugs and Malfunctions

- Fix bugs regarding opaque types.

  Own Id: OTP-13693

- Fix error handling of bad `-dialyzer()` attributes.

  Own Id: OTP-13979 Aux Id: ERL-283

### Improvements and New Features

- A few warning messages have been improved.

  Own Id: OTP-11403

## Dialyzer 3.0.2

### Improvements and New Features

- The translation of forms to types is improved for opaque types in a few cases.

  Own Id: OTP-13682

- Add warning suppression to compiler-generated case statements. Warnings about
  clauses that cannot match and are also compiler generated are suppressed
  unless none of the clauses return.

  Own Id: OTP-13723 Aux Id: ERL-159, PR-1121

## Dialyzer 3.0.1

### Fixed Bugs and Malfunctions

- Fix a map related bug.

  Own Id: OTP-13709 Aux Id: ERL-177, PR-1115

## Dialyzer 3.0

### Fixed Bugs and Malfunctions

- Fix a bug in the translation of forms to types.

  Own Id: OTP-13520

- Correct misspelling in Dialyzer's acronym definition.

  Own Id: OTP-13544 Aux Id: PR-1007

- Dialyzer no longer crashes when there is an invalid function call such as
  `42(7)` in a module being analyzed. The compiler will now warn for invalid
  function calls such as `X = 42, x(7)`.

  Own Id: OTP-13552 Aux Id: ERL-138

- Fix a bug that caused Dialyzer to go into an infinite loop.

  Own Id: OTP-13653 Aux Id: ERL-157

- Fix a bug in Dialyzer related to call-site analysis.

  Own Id: OTP-13655 Aux Id: PR-1092

### Improvements and New Features

- The evaluation of SCCs in `dialyzer_typesig` is optimized.

  Maps are used instead of Dicts to further optimize the evaluation.

  Own Id: OTP-10349

- Since Erlang/OTP R14A, when support for parameterized modules was added,
  `t:module/0` has included `t:tuple/0`, but that part is removed; the type
  `t:module/0` is now the same as `t:atom/0`, as documented in the Reference
  Manual.

  Own Id: OTP-13244

- The type specification syntax for Maps is improved:

  - The association type `KeyType := ValueType` denotes an association that must
    be present.
  - The shorthand `...` stands for the association type `any() => any()`.

  An incompatible change is that `#{}` stands for the empty map. The type
  `t:map/0` (a map of any size) can be written as `#{...}`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13542 Aux Id: PR-1014

- The translation of forms to types is improved.

  Own Id: OTP-13547

## Dialyzer 2.9

### Fixed Bugs and Malfunctions

- Dialyzer no longer asserts that files and directories to be removed from a PLT
  exist.

  Own Id: OTP-13103 Aux Id: ERL-40

- Fix a bug concerning parameterized opaque types.

  Own Id: OTP-13237

- Fix pretty printing of Core Maps

  Literal maps could cause Dialyzer to crash when pretty printing the results.

  Own Id: OTP-13238

- If a behavior module contains an non-exported function with the same name as
  one of the behavior's callbacks, the callback info was inadvertently deleted
  from the PLT as the `dialyzer_plt:delete_list/2` function was cleaning up the
  callback table.

  Own Id: OTP-13287

- Correct the contract for `erlang:byte_size/1`

  Correct the handling of comparison operators for map and bit string operands.

  Own Id: OTP-13312

### Improvements and New Features

- Dialyzer recognizes calls to `M:F/A` where `M`, `F`, and `A` are all literals.

  Own Id: OTP-13217

## Dialyzer 2.8.2

### Fixed Bugs and Malfunctions

- Reintroduce the `erlang:make_fun/3` BIF in erl_bif_types.

  Own Id: OTP-13068

## Dialyzer 2.8.1

### Fixed Bugs and Malfunctions

- Improve the translation of forms to types.

  Own Id: OTP-12865

- Fix a bug concerning parameterized opaque types.

  Own Id: OTP-12866

- Fix a bug concerning parameterized opaque types.

  Own Id: OTP-12940

- Fix bugs concerning `erlang:abs/1`.

  Own Id: OTP-12948

- Fix a bug concerning `lists:keydelete/3` with union and opaque types.

  Own Id: OTP-12949

- Use new function `hipe:erts_checksum` to get correct runtime checksum for
  cached beam files.

  Own Id: OTP-12964 Aux Id: OTP-12963, OTP-12962

## Dialyzer 2.8

### Fixed Bugs and Malfunctions

- The translation of Erlang forms to the type representation used by Dialyzer
  has been improved in several ways. The most important change is that deeply
  nested records can be handled.

  Own Id: OTP-12350

- Fix a bug that could cause bogus warnings for opaque types.

  In Erlang/OTP 18 two parameterized types declared in the same module always
  result in a contradiction (`t:none/0`) when combined outside of the module
  where they are declared, unless they have the same number of parameters.

  The behaviour is different from Erlang/OTP 17 where, for instance,
  [`dict:dict()`](`t:dict:dict/0`) and [`dict:dict(_, _)`](`t:dict:dict/2`),
  which are both opaque, can be combined outside of the `dict` module.

  In Erlang/OTP 18, [`dict:dict()`](`t:dict:dict/0`) and
  [`dict:dict(_, _)`](`t:dict:dict/2`) can still be combined outside of the
  `dict` module. That has been made possible by not declaring
  [`dict:dict()`](`t:dict:dict/0`) as opaque.

  Own Id: OTP-12493

- Update the PLT properly when a module is changed. (Thanks to James Fish for
  the bug report, and to Stavros Aronis for fixing the bug.)

  Own Id: OTP-12637

- An argument of '\*'/2 is not constraind if the other operand can be zero.

  Own Id: OTP-12725

- Mention the option `check_plt` among the `dialyzer:gui()` options. (Thanks to
  James Fish.)

  Own Id: OTP-12750

- Fix a bug which could cause an infinite loop in Dialyzer.

  Own Id: OTP-12826

### Improvements and New Features

- The `-dialyzer()` attribute can be used for suppressing warnings in a module
  by specifying functions or warning options. It can also be used for requesting
  warnings in a module.

  Own Id: OTP-10280

- The pre-defined types `array()`, `dict()`, `digraph()`, `gb_set()`,
  `gb_tree()`, `queue()`, `set()`, and `tid()` have been removed.

  Own Id: OTP-11445 Aux Id: OTP-10342, OTP-9352

- A few type names that have been used for representing certain predefined types
  can now be used for user-defined types. This affects the types `product/_`,
  `union/_`, and `range/2` as well as `tuple/N` (N > 0), `map/N` (N > 0),
  `atom/1`, `integer/1`, `binary/2`, `record/_,` and `'fun'/_`. A consequence is
  that, for example, it is no longer possible to refer to a record type with
  `record(r)`; instead the usual record notation, `#r{}`, is to be used.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11851

- When implementing user-defined behaviours it is now possible to specify
  optional callback functions. See OTP Design Principles User's Guide, Sys and
  Proc_Lib, User-Defined Behaviours, for details.

  Own Id: OTP-11861

- Add two options to the Dialyzer: `no_missing_calls` suppresses warnings about
  calls to missing or unexported functions; `unknown` lets warnings about
  unknown functions or types affect the exit status. See also dialyzer(3).

  Own Id: OTP-12682

- By default, dialyzer will now cache native versions of dialyzer modules to
  avoid having to re-compile them each time dialyzer is started. Caching can be
  disabled using the option `--no_native_cache`.

  Own Id: OTP-12779

## Dialyzer 2.7.4

### Fixed Bugs and Malfunctions

- A bug concerning `t:map/0` types has been fixed.

  Own Id: OTP-12472

## Dialyzer 2.7.3

### Fixed Bugs and Malfunctions

- When compiling Erlang source, Dialyzer now ignores the environment variable
  ERL_COMPILER_OPTIONS as well as skips the Erlang Compiler option
  `warnings_as_errors`.

  Own Id: OTP-12225

- Dialyzer did not check the type of record elements when updating them. The
  bug, introduced in Erlang/OTP 17.1, has been corrected. (Thanks to Nicolas
  Dudebout for pointing it out.)

  Own Id: OTP-12319

- Coalesce map keys in dialyzer mode

  This fixes a regression introduced in commit
  805f9c89fc01220bc1bb0f27e1b68fd4eca688ba The problem occurred with compounded
  map keys compiled with dialyzer option turned on, '+dialyzer'.

  Reported by: Ivan Uemlianin

  Own Id: OTP-12347

## Dialyzer 2.7.2

### Fixed Bugs and Malfunctions

- A bug concerning `is_record/2,3` has been fixed, as well as some cases where
  Dialyzer could crash due to reaching system limits.

  Own Id: OTP-12018

- When given the `-Wunderspecs` flag Dialyzer sometimes output bogus warnings
  for parametrized types. This bug has been fixed.

  Own Id: OTP-12111

- Dialyzer now fetch the compile options from beam files, and use them when
  creating core from the abstract code. Previously the options were ignored.

  Own Id: OTP-12150

## Dialyzer 2.7.1

### Fixed Bugs and Malfunctions

- Fix a bug concerning opaque types. Thanks to Shayan Pooya for pointing out the
  bug.

  Own Id: OTP-11869

- A bug where Dialyzer failed to handle typed records with fields containing
  remote types has been fixed. Thanks to Erik Søe Sørensen for reporting the
  bug.

  Own Id: OTP-11918

- Make sure that only literal records are checked against the types of record
  definitions. Until now the elements of tuples have been checked against record
  field types if the tag och size of the tuple matches the record definition,
  often with surprising results.

  Own Id: OTP-11935 Aux Id: seq12590

- A Dialyzer crash involving analysis of Map types has now been fixed.

  Own Id: OTP-11947

## Dialyzer 2.7

### Fixed Bugs and Malfunctions

- Dialyzer will no longer emit warnings when inspecting or modifying opaque
  types within the scope of a module.

  Hitherto the shape of terms (tuple, list, etc.) has been used to determine the
  opaque terms, but now the contracts are used for decorating types with
  opacity.

  Own Id: OTP-10397

- With `--Wunmatched_returns`, dialyzer will no longer warn when the value of a
  list comprehension is ignored, provided that the each value in the list would
  be an atomic value (such as integer or atoms, as opposed to tuples and lists).
  Example: ignoring '`[io:format(...) || ...]`' will not cause a warning, while
  ignoring '`[file:close(Fd) || ...]`' will.

  Own Id: OTP-11626

- The man page for dialyzer now contains correct information regarding
  -Wno_behaviours. (Thanks to Steve Vinosky.)

  Own Id: OTP-11706

- Fix handling of 'on_load' attribute. (Thanks to Kostis Sagonas.)

  Own Id: OTP-11743

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

- The generalization of guard constraints has been modified.

  Own Id: OTP-11798 Aux Id: seq12547

- Dialyzer now plays nicely with funs that come as "external" arguments. (Thanks
  to Stavros Aronis for fixing the bug.)

  Own Id: OTP-11826

### Improvements and New Features

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

- Removed gs based applications and gs based backends. The `observer`
  application replaces the removed applications.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-10915

- Forbid unsized fields in patterns of binary generators and simplified
  v3_core's translation of bit string generators. (Thanks to Anthony Ramine.)

  Own Id: OTP-11186

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

- Parameterized opaque types have been introduced.

  Own Id: OTP-11625

- Some function specs are corrected or moved and some edoc comments are
  corrected in order to allow use of edoc. (Thanks to Pierre Fenoll)

  Own Id: OTP-11702

## Dialyzer 2.6.1

### Fixed Bugs and Malfunctions

- A bug that made it impossible to do any analyses from the GUI has been fixed.

  Own Id: OTP-11057 Aux Id: seq12313

### Improvements and New Features

- Include module, function and arity in Dialyzer's "overlapping domain"
  warnings. Thanks to Magnus Henoch.

  Own Id: OTP-10918

- Improve Dialyzer output for scan errors. Thanks to Magnus Henoch.

  Own Id: OTP-10996

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

- Bitstring type inference and duplicate module error message fixes. Thanks to
  Stavros Aronis.

  Own Id: OTP-11027

- Erlang source files with non-ASCII characters are now encoded in UTF-8
  (instead of latin1).

  Own Id: OTP-11041 Aux Id: OTP-10907

## Dialyzer 2.6

### Improvements and New Features

- Miscellaneous updates due to Unicode support.

  Own Id: OTP-10820

- User defined types with same name and different arity and documentation
  inconsistencies. Thanks Stavros Aronis.

  Own Id: OTP-10861

- Native code compilation changes. Thanks to Kostis Sagonas.

  Own Id: OTP-10865

## Dialyzer 2.5.4

### Improvements and New Features

- Support for Unicode has been implemented.

  Own Id: OTP-10302

- Dialyzer no longer outputs warnings for unused anonymous functions ("funs").
  Warnings are still output for unused functions.

  Own Id: OTP-10433

- Where necessary a comment stating encoding has been added to Erlang files. The
  comment is meant to be removed in Erlang/OTP R17B when UTF-8 becomes the
  default encoding.

  Own Id: OTP-10630

- Some examples overflowing the width of PDF pages have been corrected.

  Own Id: OTP-10665

- Fix precision of record creation violation warnings. Thanks to Stavros Aronis

  Own Id: OTP-10681

- Report spec discrepancy on mismatching lists. Thanks to Stavros Aronis.

  Own Id: OTP-10740

- Properly support functions with arbitrary arity in type specs. Thanks to
  Stavros Aronis.

  Own Id: OTP-10772

## Dialyzer 2.5.3

### Fixed Bugs and Malfunctions

- Fix a crash in race condition detection

  Remove old untested experimental extension

  Respect \{plt_check,false\} option when using dialyzer:run/1

  Fix handling of tuple set remote types appearing in tuple sets

  Own Id: OTP-10464

## Dialyzer 2.5.2

### Fixed Bugs and Malfunctions

- Correct handling of type names in contracts. Fix crash related to contract
  checking. Do not rewrite unchanged PLT.

  Own Id: OTP-10083

- Stop a forgotten server process

  Dialyzer forgot to stop a server process before finishing its analysis. This
  is a concurrency error detected by Concuerror. Changes to fix warnings
  identified by running dialyzer -Wunmatched_returns. Thanks to Kostis Sagonas.

  Own Id: OTP-10231

### Improvements and New Features

- Bug fixes and improvements of `dialyzer_typesig`.

  Own Id: OTP-10082

- Add parallel dialyzer support

  Own Id: OTP-10103

- An alternative implementation of the solver in `dialyzer_typesig` has been
  introduced. It is faster than the original implementation.

  Own Id: OTP-10110

- Bugs in `erl_types:t_inf()` (HiPE) and in `dialyzer_dataflow` (Dialyzer) have
  been fixed.

  Own Id: OTP-10191

## Dialyzer 2.5.1

### Improvements and New Features

- Handle `nowarn_unused_function` the same way as the compiler does.

  Own Id: OTP-9833

## Dialyzer 2.5

### Fixed Bugs and Malfunctions

- Fix false warning about closure application

  Whenever a variable that could hold one of two or more possible closures was
  used in a particular application, the application was assumed to fail if ONE
  of the closures would fail in this application. This has been corrected to
  infer failing application if ALL possible closures would fail in the
  particular application.

  Change category of 'might also return' warnings

  Dialyzer emits warnings like the following "The specification for \_ states
  that the function might also return _ but the inferred return is _", which are
  actually underspecifications and not wrong type specifications. This patch
  makes sure that they are filed under the appropriate category.

  Own Id: OTP-9707

- Wrap up behaviours patch for Dialyzer

  - Enable warnings by default, add two options for suppressing them
  - Fix warning formatting and update testsuites.
  - Detection of callback-spec discrepancies
  - Allow none() as return value in callbacks
  - Behaviour callback discrepancy detection for Dialyzer
  - Add lookup function for callbacks
  - Store callbacks in codeserver and PLT
  - Collect callback definitions during compilation
  - Update inets results

  Own Id: OTP-9731

- - No warnings for underspecs with remote types
  - Fix crash in Typer
  - Fix Dialyzer's warning for its own code
  - Fix Dialyzer's warnings in HiPE
  - Add file/line info in a particular Dialyzer crash
  - Update inets test results

  Own Id: OTP-9758

- - Correct callback spec in application module
  - Refine warning about callback specs with extra ranges
  - Cleanup autoimport compiler directives
  - Fix Dialyzer's warnings in typer
  - Fix Dialyzer's warning for its own code
  - Fix bug in Dialyzer's behaviours analysis
  - Fix crash in Dialyzer
  - Variable substitution was not generalizing any unknown variables.

  Own Id: OTP-9776

### Improvements and New Features

- Optimize the joining of maps in `dialyzer_dataflow`.

  Own Id: OTP-9761

## Dialyzer 2.4.4

### Fixed Bugs and Malfunctions

- Update results of race_SUITE/extract_translations Update results of
  small_SUITE/flatten Add codec_can and list_to_bitstring tests Fix bug when
  reporting unused functions Update Dialyzer r9c_suite results Fix dialyzer
  warning on default clause for binary comprehension (Thanks to Ivan Dubrov)

  Own Id: OTP-9483

- Fix server loop detection

  Dialyzer does not normally emit warnings for functions that implement
  non-terminating server loops. This detection failed when some of the elements
  in an SCC terminated normally (being for example list comprehensions or other
  generic anonymous functions that were included in the SCC). This patch fixes
  that.

  Own Id: OTP-9489

- Add a proplist() type

  Recently I was adding specs to an API and found that there is no canonical
  proplist() type defined. (Thanks to Ryan Zezeski)

  Own Id: OTP-9499

- Suppress some warnings about generation of non-returning funs

  No warnings are emitted for funs that are non-returning when the function that
  generates them has a contract that specifies that it will return such a
  non-returning fun.

  Enhance Dialyzer's inference on comparisons

  This patch makes Dialyzer aware of Erlang's total ordering of terms, enabling
  discrepancy detection in cases where e.g. integer() < tuple() is treated as a
  comparison that might also return false (when it is certain to always return
  true).

  Minor fix in dead code

  Fix infinite loop in dataflow

  Update r9c/\{inets,mnesia\} results in dialyzer's test suite

  Add origin information to #fun_var closures

  (Thanks to Tuncer Ayaz and Maria Christakis)

  Own Id: OTP-9529

- Quote atoms if necessary in types

  Atoms in some occurrences were not correctly quoted when formatted to strings,
  for instance by the typer program (Thanks to Tomas Abrahamsson)

  Update Dialyzer's reference results

  Own Id: OTP-9560

- Fix typer's crash for nonexisting files Remove unused macro Fix bug in
  dataflow Decrease tuple arity limit This fixes a memory related crash.

  Own Id: OTP-9597

### Improvements and New Features

- Types for several BIFs have been extended/corrected. Also the types for types
  for `lists:keyfind/3`, `lists:keysearch/3`, and `lists:keyemember/3` have been
  corrected. The incorrect/incomplete types could cause false dialyzer warnings.

  Own Id: OTP-9496

## Dialyzer 2.4.3

### Fixed Bugs and Malfunctions

- Fix the name of an error function(Thanks to Maria christakis)

  Own Id: OTP-9175

- Fix crash related with the contract blame assignment patch

  Own Id: OTP-9219

- dialyzer/doc: synchronize manual.txt and dialyzer.xml (Thanks to Tuncer Ayaz)

  Own Id: OTP-9226

- Simplify Dialyzer's test suite structure

  \*\_SUITE.erl files are now automatically generated by the respective data
  directories by the Makefile.

  Own Id: OTP-9278

## Dialyzer 2.4.2

### Fixed Bugs and Malfunctions

- Add a --fullpath option to Dialyzer

  This change adds a --fullpath option to Dialyzer, which makes the warning
  messages contain the full path of the corresponding file.

  Original patch submitted by Magnus Henoch (legoscia) on 15/9/2010 and cooked
  to death in the 'pu' branch all this time.

  The patch was essentially correct and most of it has been used as is, but
  there have been some changes to make the code slightly prettier, avoid some
  code duplication, and add documentation to dialyzer's doc files and to its
  help message.

  Own Id: OTP-9098

- Fix warnings about guards containing not

  The wording of warnings about unsatisfiable guards that used 'not' was
  incorrect (the 'not' was not mentioned and it appeared as "Guard test
  is_atom(atom()) can never succeed") (thanks to Stavros Aronis).

  Own Id: OTP-9099

- Version 2.4.2 (in Erlang/OTP R14B02) ------------------------------------ -
  Added --fullpath option to display files with warnings with their full file
  names (thanks to Magnus Henoch for the original patch). - Better handling of
  'and'/'or'/'not' guards that generate warnings (thanks to Stavros Aronis). -
  Better blame assignment for cases when a function's spec is erroneous (thanks
  to Stavros Aronis). - More descriptive warnings when a tuple/record pattern
  contains subterms that violate the declared types of record fields (thanks to
  Matthias Lang for the test case and for Stavros Aronis for the actual fix).

  Own Id: OTP-9126

- Add spec to dialyzer_cl_parse:get_lib_dir/1

  Own Id: OTP-9129

### Improvements and New Features

- Test suites for Dialyzer

  This is a transcription of most of the cvs.srv.it.uu.se:/hipe repository
  dialyzer_tests into test suites that use the test server framework.

  See README for information on how to use the included scripts for
  modifications and updates.

  When testing Dialyzer it's important that several OTP modules are included in
  the plt. The suites takes care of that too.

  Own Id: OTP-9116

## Dialyzer 2.4.0

### Fixed Bugs and Malfunctions

- \- Fixed pretty rare infinite loop when refining the types of an SCC whose
  functions all returned none() (thanks to Stavros Aronis).

  \- Fixed pretty rare crash when taking the infimum of two tuple_sets.

  Own Id: OTP-8979

### Improvements and New Features

- \- Added ability to supply multiple PLTs for the analysis (option --plts).
  Currently these PLTs must be independent (i.e., no module appears in more than
  one PLT) and there must not include files with module name clashes.

  \- Strengthened and streamlined hard-coded type information for some BIFs and
  key library functions.

  Own Id: OTP-8962

## Dialyzer 2.3.1

### Improvements and New Features

- Eliminated warnings for auto-imported BIF clashes.

  Own Id: OTP-8840

## Dialyzer 2.3.0

### Improvements and New Features

- Various changes to dialyzer-related files for R14.

  \- Dialyzer properly supports the new attribute -export_type and checks that
  remote types only refer to exported types. A warning is produced if some
  files/applications refer to types defined in modules which are neither in the
  PLT nor in the analyzed applications.

  \- Support for detecting data races involving whereis/1 and unregister/1.

  \- More precise identification of the reason(s) why a record construction
  violates the types declared for its fields.

  \- Fixed bug in the handling of the 'or' guard.

  \- Better handling of the erlang:element/2 BIF.

  \- Complete handling of Erlang BIFs.

  Own Id: OTP-8699

## Dialyzer 2.2.0

### Improvements and New Features

- Much better support for opaque types (thanks to Manouk Manoukian).

  Added support for recursive types (experimental).

  Added support for parameterized modules.

  Dialyzer now warns when -specs state that a function returns some type when in
  fact it does not.

  Added `--no_native` (`-nn`) option so that the user can bypass the native code
  compilation that dialyzer heuristically performs when dialyzing many files.

  Fixed minor bug in the dialyzer script allowing the --wx option to bring up
  the wx-based GUI regardless of its placement in the options list.

  Options --apps and -Wrace_conditions, which were added in the previous
  version, are now properly documented in the manual.

  Own Id: OTP-8464

## Dialyzer 2.1.0

### Improvements and New Features

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8201

- Dialyzer can statically detect some kinds of data races in Erlang programs.
  Use the new option -Wrace_conditions to enable the race analysis. The
  technique is described in a paper which is available at:
  http://www.it.uu.se/research/group/hipe/dialyzer/publications/races.pdf

  Added support for packages (thanks to Maria Christakis).

  There has been a major change in the default mode of Dialyzer. Previously, the
  default mode was the GUI, while now it is the command line. As a result of
  this change, it is now possible to analyze a set of files/dirs with the
  command:

  - dialyzer file1 ... fileN

  In other words, the -c (--command-line) option is no longer necessary, though
  it will be retained for some time for backwards compatibility. To start
  dialyzer's GUI use either of the following commands:

  - dialyzer --gui %% for the old gs-based GUI
  - dialyzer --wx %% for the new wx-based GUI (where available)

  There is a new option --apps which allows the user to easily refer to
  Erlang/OTP applications and include them in the analysis or in the building of
  the PLT. For example, we recommend building the PLT with:

  - dialyzer --build_plt --apps erts kernel stdlib mnesia ...

  The new option can also take absolute file names as well as applications. Note
  that the application versions that will be included in the PLT are those that
  correspond to the Erlang/OTP system which is used.

  Dialyzer has a new wxWidgets based GUI (thanks to Elli Frangaki) for platforms
  where the wx application is available.

  Own Id: OTP-8300

## Dialyzer 2.0.0

### Improvements and New Features

- There is a major addition to the capabilities of dialyzer, worthy of bumping
  the version number. Starting with this version, dialyzer not only accepts but
  also properly processes remote types (i.e., types of the form
  ModuleName:TypeName()). Previous dialyzer versions only accepted this notation
  in -type and -spec declarations, but effectively ignored its information by
  mapping remote types to the type any(). In contrast, starting with this
  version, remote types are used in the analysis and are also stored in the
  PLTs. (This addition changes the format of PLTs and requires rebuilding any
  PLTs created by an older dialyzer version.) Note that dialyzer will complain
  and abort the analysis of a set of modules if it needs to process a remote
  type without a definition (either because the module does not include a
  definition of the type or the module is not included in the analysis). We may
  relax this restriction in a future version.

  Fixed minor issue with dialyzer:run/1 crashing (upon its return) when used for
  adding type information to an existing PLT.

  Fixed minor but quite annoying issues in dialyzer's GUI.

  Own Id: OTP-8187

## Dialyzer 1.9.2

### Improvements and New Features

- Fixed problem with type inference going into an infinite loop when analyzing a
  strongly connected component of functions that do not return but also contain
  an erroneous call which makes them fail and be assigned the type none()
  instead of the type unit().

  More accurate type information for some BIFs and library files.

  Introduced boolean() as the \`official' name for the type was so far known as
  bool(). The latter is still accepted as well as boolean().

  Own Id: OTP-8037

## Dialyzer 1.9.1

### Improvements and New Features

- Has better handling of opaque types.

  The handling of UTF segments of bitstreams has been significantly strengthened
  and revised. In all probability, now it is correct.

  Own Id: OTP-7958

## Dialyzer 1.9.0

### Improvements and New Features

- The analysis accepts opaque type declarations and detects violations of
  opacity of terms of such types. Starting with R13, many Erlang/OTP standard
  libraries (array, dict, digraph, ets, gb_sets, gb_trees, queue, and sets)
  contain opaque type declarations of their main data types. Dialyzer will spit
  out warnings in code that explicitly depends on the structure of these terms.

  Added support for handling UTF segments in bitstreams and for detecting
  obvious type errors in these segments. Warning: This code is not terribly
  tested though since there are very few Erlang programs which use Unicode-based
  binaries - not surprising since this is a new language feature of R13.

  Strengthened the discrepancy identification when testing for equality and
  matching between terms of different types. This detects more bugs in code.

  Added warning for M:F(...) calls where M is not a module term and F is not an
  atom. Previously, such calls where found to fail but the reason for the
  failure was not reported.

  Added a convenient shorthand for the --no_check_plt option (-n).

  Added the --dump_callgraph option for dumping the callgraph of all files that
  are analyzed into a specified file. The callgraph either be dumped in raw
  format, in .dot format, or converted to a .ps (postscript) file. Note that in
  large callgraphs the generated postscript file might not be interpretable by
  Ghostview. (Thanks to Ilya Khlopotov for the initial version of this
  functionality.)

  Own Id: OTP-7864

## Dialyzer 1.8.3

### Improvements and New Features

- Added the `--no_check_plt` option that makes the startup time faster when
  working with stable PLTs that do not change.

  Changed the phrasing of some warnings so that they do not cause confusion to
  some users and correspond better to reality.

  Own Id: OTP-7632

## Dialyzer 1.8.2

### Improvements and New Features

- Minor updates.

  Own Id: OTP-7522

## Dialyzer 1.8.1

### Improvements and New Features

- There is new `--raw` option for Dialyzer to output the result of the analysis
  in Erlang term, to facilitate later filtering and/or formatting.

  Own Id: OTP-7386

- The return type of the Erlang interface dialyzer:run/1 has changed to only
  return a list of warnings. If something goes wrong dialyzer dies with an
  exception.

  The handling of the PLT is now more flexible. There is no longer any default
  PLT included with OTP. Please consult the manual for the changes.

  Own Id: OTP-7389

## Dialyzer 1.8.0

### Improvements and New Features

- Dialyzer's analysis is from now on exclusively based on success typings. In
  particular, support for options `--old_style` and `--dataflow` has been
  discontinued.

  Better and more aggressive handling of type information in records.

  Dialyzer has a new warning option `-Wunmatched_returns` which warns for
  function calls that ignore the return value. This catches many common
  programming errors (e.g. calling `file:close/1` and not checking for the
  absence of errors), interface discrepancies (e.g. a function returning
  multiple values when in reality the function is void and only called for its
  side-effects), calling the wrong function (e.g. `io_lib:format/1` instead of
  `io:format/1`), and even possible performance defects (e.g. unnecessarily
  constructing a list using a list comprehension instead of using
  `lists:foreach/2`). Whenever a function returns a single atomic value (e.g.
  'ok' or pid()), the warning is suppressed. This allows for "void" functions
  (returning a single atom like 'ok') or for calls to certain builtins like
  `spawn`. Because not all calls which ignore the return value are
  discrepancies, the option is off by default and has to be explicitly requested
  by the user. But we recommend it nevertheless.

  Some warning options (`-Wno_comp`, `-Wno_guards`, `-Wno_unsafe_beam`, etc.)
  which could be used when analyzing bytecode produced by an old BEAM compiler
  have been removed.

  Own Id: OTP-7241

## Dialyzer 1.7.2

### Improvements and New Features

- The warnings returned by the Erlang interface now contains a tag describing
  the type of warning.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7134

## Dialyzer 1.7.1

### Improvements and New Features

- Use of success typings is now default, is robust and detects significantly
  more errors than in previous versions.

  Dialyzer now accepts and takes into account type information in record
  declarations and in contracts -- see related paper in Erlang'07 workshop.
  Various OTP applications (e.g. stdlib and kernel) are partially annotated with
  appropriate contracts specifying types information for their functions.

  The type previously known as unit() has been renamed to no_return(). Its use
  in a contract as the return type of a function now silences off the "function
  has no local return" dialyzer warning without use of the corresponding option.

  Own Id: OTP-6997

## Dialyzer 1.7.0

### Improvements and New Features

- Minor Makefile changes.

  Own Id: OTP-6689

- Dialyzer can now use success typings to find discrepancies. As a consequence
  significantly more discrepancies are detected. The downside is that the
  analysis takes about 2 to 2.5 times longer than in previous versions, and may
  also result in some warnings that might be harder to interpret even though
  they are correct. This has been a major change, worth of increasing the
  version number.

  New command-line options:

  \--succ_typings Use the success typings analysis.

  \--dataflow (default) Use the previous analysis.

  The new type unit() has been introduced to handle the return type of
  non-terminating functions such as servers.

  Dialyzer's code server uses a compressed representation and the analysis
  requires significantly less memory than in previous versions.

  Own Id: OTP-6736

## Dialyzer 1.6.0

### Improvements and New Features

- Dialyzer (in the modes where either source or BEAM code which contains
  debug_info is analyzed) spits out line information for all discrepancies. This
  has been a major change, worth of increasing the version number.

  We warn users that _considerably_ more discrepancies are identified by this
  version of Dialyzer compared with previous ones and applications. If, for some
  reason, warnings generated by previous versions of Dialyzer are preferable,
  the command line option --old_style can be employed.

  Own Id: OTP-6546

- Dialyzer handles term comparison operators more precisely and is able to
  identify more discrepancies in 'if' or 'case' statements with comparisons.

  Dialyzer has more precise type information for many standard OTP functions.

  Own Id: OTP-6547

## Dialyzer 1.5.1

### Improvements and New Features

- Updated the chapter "More on the Persistent Lookup Table (PLT)" in Dialyzer
  User's Guide and added information on how to use Dialyzer from Erlang to
  `m:dialyzer`. Also, the Dialyzer text files used by its GUI are now included
  in the Erlang/OTP release.

  Own Id: OTP-6361

- New options `--check_init_plt` and `--verbose`.

  Improvements in the analysis (meaning that this version can find more
  discrepancies than the previous version).

  Own Id: OTP-6421

## Dialyzer 1.5.0

### Improvements and New Features

- Dialyzer's building of PLT is based on a different type inference algorithm.
  More specifically, Dialyzer uses inference of refined success typings to infer
  function prototypes. As a result, Dialyzer bases its analysis on a
  significantly more powerful basis and thus is able to detect more
  discrepancies. The downside is that building the PLT is a considerably slower
  process. We will work on improving that.

  Dialyzer takes into account the BEAM compiler directive
  `-compile({nowarn_unused_function, {F,A}}).` and then suppresses the warning
  that function F/A will never be called.

  Dialyzer's default initial PLT now also includes "mnesia".

  Own Id: OTP-6304

## Dialyzer 1.4.2

### Improvements and New Features

- Improvements in PLT management.

  Own Id: OTP-6128

## Dialyzer 1.4.1

### Fixed Bugs and Malfunctions

- Some minor changes.

### Improvements and New Features

- Some minor changes.

## Dialyzer 1.4.0

### Fixed Bugs and Malfunctions

- Changes for Dialyzer to work with Erlang/OTP R10B-10.

### Improvements and New Features

- Dialyzer's analysis is significantly faster as it uses a global function
  call-graph for obtaining type signatures for all analyzed functions.

## Dialyzer 1.3.1

### Fixed Bugs and Malfunctions

- Small changes for Dialyzer to work with Erlang/OTP R10B-5.
- Fixed a small buglet in the analysis; this affected one of HiPE's files.

### Improvements and New Features

- Modified setup script for execution under Cygwin (patch by Vlad Dumitrescu).
- Added command line option --no_warn_on_inline.
- Dialyzer now explicitly warns when modules with the same name but from
  different dirs are being analyzed (contribution by Ulf Wiger).

## Dialyzer 1.3.0

### Fixed Bugs and Malfunctions

- Fixed a number of false positives that Dialyzer 1.2.0 was spitting out.

### Improvements and New Features

- Requires the presence of an Erlang/OTP R10B-4 system.
- Dialyzer is significantly (approx 40%) faster since it now uses 'ets' rather
  than 'dets' for its PLT.
- Slightly improved the precision of the analysis.
- In the GUI version, Dialyzer now reports the list of modules that should be
  included in the modules to analyze in order to possibly improve the accuracy
  of the reported results.
- Some more information is displayed when calling a function or closure with
  arguments of the wrong type.
- The record guard now allows discrepancies involving tuples that are known to
  be records to be displayed as #rec\{\} rather than \{'rec',_,...,_\}
- Added -q option which makes the command-line version of Dialyzer a bit more
  silent.

## Dialyzer 1.2.0

### Improvements and New Features

- Dialyzer works with the open source and commercial versions of Erlang/OTP
  R10B-2 on all platforms (i.e., HiPE support is not a prerequisite anymore).
- Whenever a .beam file contains abstract code (i.e., has been compiled with the
  +debug_info option), the analysis starts from this code rather than from BEAM
  bytecode -- this makes the results identical to those obtained by analysis
  starting from source code. (This is a contribution from Bjorn Gustavsson -
  Thanks\!)
- Added -pa command line option for easier handling of -include_lib() directives
  in source code.
- Includes all changes added in v1.1.1; see below. The "Add Recursively"
  functionality is also available in the command-line mode (-r option).

## Dialyzer 1.1.1

### Fixed Bugs and Malfunctions

- Fixed problems using the shell script on Solaris machines.
- Fixed small inconsistencies in Dialyzer's documentation and help.

### Improvements and New Features

- The command-line mode of Dialyzer spits discrepancies in stdout by default and
  returns a numeric value indicating its exit status.
- Added "Add Recursively" button contributed by Martin Bjorklund (thanks\!).

## Dialyzer 1.1.0

### Improvements and New Features

- Dialyzer works with the open source version of Erlang/OTP R10B-1 (on platforms
  where HiPE support is available) and it does not require installation of a
  customized Erlang/OTP system anymore.
- Dialyzer comes with an extensive command-line interface, in addition to an
  enhanced GUI.
- Analysis can start from source code (through Core Erlang) as well as from BEAM
  bytecode.
- Dialyzer finds considerably more discrepancies in Erlang code than previous
  versions.
- Added ability to selectively turn on/off different warning categories.

## Dialyzer 1.0.1

### Fixed Bugs and Malfunctions

- Fixed major bug of v1.0.0 which caused the analysis to stop prematurely (i.e.,
  without properly reaching fixpoint) when analyzing more than one module. This
  did not affect correctness of the results, but lost many opportunities to
  detect discrepancies.

### Improvements and New Features

- Performance improvements in the analysis.

## Dialyzer 1.0.0

### Improvements and New Features

- Initial Dialyzer version: Analysis starts from BEAM bytecode only and only a
  GUI version is available. Dialyzer reports discrepancies in the use of the
  "sequential" part of Erlang. To be used, Dialyzer requires the presence of a
  specific snapshot of Erlang/OTP which can be obtained from Dialyzer's
  homepage.
