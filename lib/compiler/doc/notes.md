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
# Compiler Release Notes

This document describes the changes made to the Compiler application.

## Compiler 8.5

### Fixed Bugs and Malfunctions

- Generators for binary comprehensions could be evaluated before it was known that they would be needed. That could result in a binary comprehensions failing if a generator that should not be evaluated until later failed.
  
  As an example, consider this module:
  
  ```erlang
  -module(t).
  -export([f/0]).
  
  f() ->
      <<0 || _ <- [], _ <- ok, false>>.
  ```
  
  In Erlang/OTP 26 it would fail like so:
  
  ```erlang
  1> t:f().
  ** exception error: bad generator ok
       in function  t:f/0 (t.erl, line 6)
  ```
  
  In Erlang/OTP 27 it returns an empty binary:
  
  ```erlang
  1> t:f().
  <<>>
  ```

  Own Id: OTP-18703 Aux Id: [GH-7494], [PR-7538]

- The documentation for the preprocessor now mentions that `defined(Name)` can be called in the condition for an `-if` or `-elif` directive to test whether `Name` is the name of a defined macro. (This feature was implemented in OTP 21.)
  
  If a function call in an `-if` or `-elif` with a name that is not the name of a guard BIF, there would not be a compilation error, but would instead cause the lines following the directive to be skipped. This has now been changed to be a compilation error.

  *** POTENTIAL INCOMPATIBILITY ***

  Own Id: OTP-18784 Aux Id: [GH-7706], [PR-7726]

[GH-7494]: https://github.com/erlang/otp/issues/7494
[PR-7538]: https://github.com/erlang/otp/pull/7538
[GH-7706]: https://github.com/erlang/otp/issues/7706
[PR-7726]: https://github.com/erlang/otp/pull/7726

### Improvements and New Features

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

- The compiler now optimizes creation of binaries that are known to be constant.
  
  Consider this example:
  
  ```erlang
  bin() ->
      C = char(),
      <<C>>.
  
  char() -> $*.
  ```
  
  Essentially, the compiler rewrites the example to the slightly more efficient:
  
  ```erlang
  bin() ->
      _ = char(),
      <<$*>>.
  
  char() -> $*.
  ```

  Own Id: OTP-18673 Aux Id: [PR-7474], ERIERL-964

- The compiler will now merge consecutive updates of the same record.
  
  As an example, the body of the following function will be combined into a single tuple creation instruction:
  
  ```erlang
  -record(r, {a,b,c,d}).
  
  update(Value) ->
      R0 = #r{},
      R1 = R0#r{a=Value},
      R2 = R1#r{b=2},
      R2#r{c=3}.
  ```

  Own Id: OTP-18680 Aux Id: [PR-7491], [PR-8086], ERIERL-967

- Improved the performance of the alias analysis pass.

  Own Id: OTP-18714 Aux Id: [PR-7528], [GH-7432]

- `-spec` attributes are now used for documentation.

  Own Id: OTP-18801 Aux Id: [PR-7739]

- Native coverage support has been implemented in the JIT. It will  automatically be used by the `m:cover` tool to reduce the execution overhead when running cover-compiled code.
  
  There are also new APIs to support native coverage without using the `cover` tool.
  
  To instrument code for native coverage it must be compiled with the [`line_coverage`](`m:compile#line_coverage`) option.
  
  To enable native coverage in the runtime system, start it like so:
  
  ```text
  $ erl +JPcover true
  ```
  
  There are also the following new functions for supporting native coverage:
  
  * `code:coverage_support/0`
  * `code:get_coverage/2`
  * `code:reset_coverage/1`
  * `code:get_coverage_mode/0`
  * `code:get_coverage_mode/1`
  * `code:set_coverage_mode/1`

  Own Id: OTP-18856 Aux Id: [PR-7856]

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

- The documentation has been migrated to use Markdown and ExDoc.

  Own Id: OTP-18955 Aux Id: [PR-8026]

- The order in which the compiler looks up options has changed.
  
  When there is a conflict in the compiler options given in the `-compile()` attribute and options given to the compiler, the options given in the `-compile()` attribute overrides the option given to the compiler, which in turn overrides options given in the `ERL_COMPILER_OPTIONS` environment variable.
  
  Example:
  
  If  `some_module.erl` has the following attribute:
  
  ```erlang
  -compile([nowarn_missing_spec]).
  ```
  
  and the compiler is invoked like so:
  
  ```text
  % erlc +warn_missing_spec some_module.erl
  ```
  
  no warnings will be issued for functions that do not have any specs.

  *** POTENTIAL INCOMPATIBILITY ***

  Own Id: OTP-18968 Aux Id: [GH-6979], [PR-8093]

- Safe destructive update of tuples has been implemented in the compiler and runtime system. This allows the VM to update tuples in-place when it is safe to do so, thus improving performance by doing less copying but also by producing less garbage.
  
  Example:
  
  ```erlang
  -record(rec, {a,b,c}).
  
  update(#rec{a=needs_update,b=N}=R0) ->
      R = R0#rec{a=up_to_date},
      if
          N < 0 ->
              R#rec{c=negative};
          N == 0 ->
              R#rec{c=zero};
          N > 0 ->
              R#rec{c=positive}
      end.
  ```
  
  The record updates in each of the three clauses of the `if` can safely be done in-place, because variable `R` is not used again.

  Own Id: OTP-18972 Aux Id: [PR-8090]

- Improved the match context reuse optimization slightly, allowing match contexts to be passed as-is to `bit_size/1` and `byte_size/1`.

  Own Id: OTP-18987

- `m:erl_lint` (and by extension the [`compiler`](`m:compile`)) will now warn for code using deprecated callbacks.
  
  The only callback currenly deprecated is `format_status/2` in [`gen_server`](`c:gen_server:format_status/2`), [`gen_event`](`c:gen_event:format_status/2`) and [`gen_statem`](`c:gen_server:format_status/2`).
  
  You can use `nowarn_deprecated_callback` to silence the warning.

  Own Id: OTP-19010 Aux Id: [PR-8205]

[PR-7383]: https://github.com/erlang/otp/pull/7383
[PR-7474]: https://github.com/erlang/otp/pull/7474
[PR-7491]: https://github.com/erlang/otp/pull/7491
[PR-8086]: https://github.com/erlang/otp/pull/8086
[PR-7528]: https://github.com/erlang/otp/pull/7528
[GH-7432]: https://github.com/erlang/otp/issues/7432
[PR-7739]: https://github.com/erlang/otp/pull/7739
[PR-7856]: https://github.com/erlang/otp/pull/7856
[PR-7936]: https://github.com/erlang/otp/pull/7936
[PR-8026]: https://github.com/erlang/otp/pull/8026
[GH-6979]: https://github.com/erlang/otp/issues/6979
[PR-8093]: https://github.com/erlang/otp/pull/8093
[PR-8090]: https://github.com/erlang/otp/pull/8090
[PR-8205]: https://github.com/erlang/otp/pull/8205

## Compiler 8.4.3

### Fixed Bugs and Malfunctions

* In rare circumstances, the compiler code generate unsafe code for a bit syntax match.

  Own Id: OTP-19019
* In rare circumstances, binary matches that were supposed to succeed failed.

  Own Id: OTP-19035 Aux Id: GH-8280, PR-8284
* Fixed a bug where a fun's environment could be overridden by an argument if all of the following conditions were met:

  * The fun was declared in the module that called it.
  * The fun's target was statically known.
  * The fun was called with a number of extra arguments equal to the number of environment variables.

  Own Id: OTP-19045 Aux Id: GH-8316

## Compiler 8.4.2

### Fixed Bugs and Malfunctions

* In rare circumstances, an unsafe optimization could cause the compiler to generate incorrect code for list matching.

  Own Id: OTP-19003 Aux Id: GH-8187, PR-8189

### Improvements and New Features

* Fix the compilation server to restart if the applications in its lib dir changes inbetween `erlc` invokations.

  Own Id: OTP-18936

## Compiler 8.4.1

### Fixed Bugs and Malfunctions

- The compiler could become extremely slow for modules containing huge
  functions.

  Own Id: OTP-18770 Aux Id: GH-7667, PR-7672

## Compiler 8.4

### Fixed Bugs and Malfunctions

- The compiler could run forever when compiling a call to
  [`is_record/3`](`is_record/3`) with a huge positive tuple size. The call
  [`is_record(A, a, 0)`](`is_record/3`) would crash the compiler when used in a
  function body. When used in a guard the compiler would emit incorrect code
  that would accept `{a>` as a record.

  Own Id: OTP-18605 Aux Id: GH-7298, GH-7317

- Fixed a bug that caused dialyzer to crash when analyzing bogus code that
  contained the literal atom `undefined` in segment sizes.

  Own Id: OTP-18629 Aux Id: GH-7325

- The compiler would crash when compiling some modules that contained a call to
  `erlang:load_nif/2`.

  Own Id: OTP-18662 Aux Id: GH-7409, PR-7416

- Fixed a bug that caused the compiler to crash on legal code.

  Own Id: OTP-18678 Aux Id: GH-7488

- The compiler could crash when attempting to compile a call to
  [`is_list/1`](`is_list/1`) in a complex expression.

  Own Id: OTP-18689 Aux Id: GH-7504, PR-7518

- A complex guard expression using the `or` operator could succeed when it was
  supposed to fail.

  Own Id: OTP-18692 Aux Id: GH-7517, PR-7519

- Compiling nested `try`/`catch` and `catch` expression could result in an
  internal compiler error.

  Own Id: OTP-18701 Aux Id: GH-7477, PR-7532

- Using the `bnot` operator in a complex expression could cause the compiler to
  terminate with an internal consistency failure diagnostic.

  Own Id: OTP-18719 Aux Id: GH-7468, PR-7562

- Fixed a bug that caused the compiler to crash in a binary optimization pass.

  Own Id: OTP-18721 Aux Id: PR-7527

- The compiler could terminate with an internal error when attempting to compile
  a binary pattern that could not possibly match.

  Own Id: OTP-18725 Aux Id: GH-7467

### Improvements and New Features

- Fixed various performance issues related to the alias optimization pass.

  Own Id: OTP-18691 Aux Id: PR-7448

## Compiler 8.3.2

### Fixed Bugs and Malfunctions

- Fixed a type handling bug that would cause an internal consistence failure for
  correct code.

  Own Id: OTP-18625 Aux Id: GH-7354

- Fixed a bug that could cause the stack trace of `throw` exceptions to be
  erroneously optimized out.

  Own Id: OTP-18626 Aux Id: GH-7356

- Complex guard expression using '`or`' were not always fully evaluated, making
  guards that were supposed to fail succeed.

  Own Id: OTP-18634 Aux Id: GH-7370

## Compiler 8.3.1

### Fixed Bugs and Malfunctions

- Fixed a bug where a failing `bsl` expression in a guard threw an exception
  instead of causing the guard to fail.

  Own Id: OTP-18576

- Fixed a bug that would case the validator to reject legal code.

  Own Id: OTP-18581 Aux Id: GH-7251

- The compiler could re-order clauses matching binaries so that the incorrect
  clause would match. That could only happen for code that used the option
  `{error_location,line}` or for code without line or column number information
  (e.g. generated by a parse transform).

  Own Id: OTP-18583 Aux Id: GH-7259

- Complex guard expression using the `or` operator and guard BIFs that can fail
  could sometimes be miscompiled so that the guard would succeed even if a call
  to a guard BIF failed.

  Own Id: OTP-18593 Aux Id: GH-7252

- With optimizations disabled, a `try`/`catch` construct could return an
  incorrect value.

  Own Id: OTP-18600 Aux Id: GH-7248

- In rare circumstance, a combination of binary construction and
  [`binary_part/3`](`binary_part/3`) would cause the compiler to generate unsafe
  code that would crash the runtime system.

  Own Id: OTP-18601

- The compiler could be very slow when compiling guards with multiple guard
  tests separated with '`or`' or '`;`'.

  Own Id: OTP-18617 Aux Id: GH-7338

- Complex guard expressions using '`or`' and map updates could succeed even if
  the map update failed.

  Own Id: OTP-18619 Aux Id: GH-7339

## Compiler 8.3

### Fixed Bugs and Malfunctions

- The compiler would silently accept singleton (unbound) type variables in a
  union type. Starting from Erlang/OTP 26, the compiler will generate a warning
  for this example. The warning can be disabled using the
  `nowarn_singleton_typevar` option. In Erlang/OTP 27, the warning will become
  an error.

  Own Id: OTP-18389 Aux Id: GH-6508, PR-6864, GH-7116

### Improvements and New Features

- Optimized record updates.

  Own Id: OTP-18126 Aux Id: PR-6033

- There are several new optimization for binary syntax in the JIT:

  - Creation and matching of binaries with segments of fixed sizes have been
    optimized.
  - Creation and matching of UTF-8 segments have been optimized.
  - Appending to binaries has been optimized.

  Own Id: OTP-18137 Aux Id: PR-6259, PR-6404, PR-6576, PR-6804

- The compiler and JIT now generate better code for creation of small maps where
  all keys are literals known at compile time.

  Own Id: OTP-18185 Aux Id: GH-6139

- A limitation in the binary syntax has been removed. It is now possible to
  match binary patterns in parallel. Example: `<<A:8>> = <<B:4,C:4>> = Bin`

  Own Id: OTP-18297 Aux Id: GH-6348

- It is documented that `$\^X` is the ASCII code for Control X, where X is an
  uppercase or lowercase letter. However, this notation would work for any
  character X, even then it didn't make sense.

  In Erlang/OTP 26, it is now documented that the following characters are also
  allowed to follow the `\^` characters: `@`, `[`, `\`, `]`, `^`, `_`, and `?`.
  Attempt to use other characters will be rejected with a compiler error.

  The value for `$\^?` is now 127 (instead of 31 as in earlier releases).

  Own Id: OTP-18337 Aux Id: GH-6477, PR-6503

- The BIFs [`min/2`](`min/2`) and [`max/2`](`max/2`) are now allowed to be used
  in guards and match specs.

  Own Id: OTP-18367 Aux Id: GH-6544

- Map comprehensions as suggested in EEP 58 has now been implemented.

  Own Id: OTP-18413 Aux Id: EEP-58, PR-6727

- Improved the selective receive optimization, which can now be enabled for
  references returned from other functions.

  This greatly improves the performance of `gen_server:send_request/3`,
  `gen_server:wait_response/2`, and similar functions.

  Own Id: OTP-18431 Aux Id: PR-6739

- Deprecates `dbg:stop_clear/0` because it is simply a function alias to
  `dbg:stop/0`

  Own Id: OTP-18478 Aux Id: GH-6903

- The compiler will now inline calls to `maps:get/3`.

  Own Id: OTP-18502

- In Erlang/OTP 27, `0.0` will no longer be considered to be exactly equal to
  `-0.0`. See
  [Upcoming Potential Incompatibilities](`e:general_info:upcoming_incompatibilities.md#float_matching`).

  Own Id: OTP-18574

## Compiler 8.2.6.4

### Fixed Bugs and Malfunctions

* In rare circumstances, an unsafe optimization could cause the compiler to generate incorrect code for list matching.

  Own Id: OTP-19003 Aux Id: GH-8187, PR-8189
* In rare circumstances, the compiler code generate unsafe code for a bit syntax match.

  Own Id: OTP-19019

## Compiler 8.2.6.3

### Fixed Bugs and Malfunctions

- Fixed a bug that could cause the stack trace of `throw` exceptions to be
  erroneously optimized out.

  Own Id: OTP-18626 Aux Id: GH-7356

## Compiler 8.2.6.2

### Fixed Bugs and Malfunctions

- The compiler could be very slow when compiling guards with multiple guard
  tests separated with '`or`' or '`;`'.

  Own Id: OTP-18617 Aux Id: GH-7338

## Compiler 8.2.6.1

### Fixed Bugs and Malfunctions

- Fixed a bug where a failing `bsl` expression in a guard threw an exception
  instead of causing the guard to fail.

  Own Id: OTP-18576

- Complex guard expression using the `or` operator and guard BIFs that can fail
  could sometimes be miscompiled so that the guard would succeed even if a call
  to a guard BIF failed.

  Own Id: OTP-18593 Aux Id: GH-7252

## Compiler 8.2.6

### Fixed Bugs and Malfunctions

- Fixed type handling bugs that could cause an internal error in the compiler
  for correct code.

  Own Id: OTP-18565 Aux Id: GH-7147

## Compiler 8.2.5

### Fixed Bugs and Malfunctions

- When a map update such as `#{}#{key:=value}` that should fail with an
  exception was unused, the exception would be lost.

  Own Id: OTP-18497 Aux Id: GH-6960, PR-6965

- Fixed bug in the validator that made it reject valid code.

  Own Id: OTP-18516 Aux Id: GH-6969

## Compiler 8.2.4

### Fixed Bugs and Malfunctions

- Fixed a bug that would cause the compiler to hang.

  Own Id: OTP-18378 Aux Id: GH-6604

- Fixed a crash when compiling code that contained `maybe` expressions.

  Own Id: OTP-18381 Aux Id: GH-6601

- Constructing a binary with an explicit size of `all` for a binary segment
  would crash the compiler.

  Own Id: OTP-18407 Aux Id: GH-6707

- The compiler would generate incorrect code for the following type of
  expression:

  `Pattern = BoundVar1 = . . . = BoundVarN = Expression`

  An exception should be raised if any of the bound variables have different
  values than `Expression`. The compiler would generate code that would cause
  the bound variables to be bound to the value of `Expression`whether the value
  matched or not.

  Own Id: OTP-18470 Aux Id: GH-6873, PR-6877

## Compiler 8.2.3

### Fixed Bugs and Malfunctions

- Fixed a bug that could cause legal code to fail validation.

  Own Id: OTP-18365

- Eliminated a rare crash in the `beam_types` module.

  Own Id: OTP-18368

## Compiler 8.2.2

### Fixed Bugs and Malfunctions

- Line number in compiler messages would be truncated to 4 digits for line
  numbers greater than 9999.

  Own Id: OTP-18268 Aux Id: GH-6332

- In rare circumstance, matching a binary as part of a `receive` clause could
  cause the compiler to terminate because of an internal consistency check
  failure.

  Own Id: OTP-18273 Aux Id: GH-6341

- Compiling a function with complex bit syntax matching such as
  `f(<<X:0, _:X>>, <<Y:0, _:Y>>) -> ok.` could crash the compiler.

  Own Id: OTP-18308 Aux Id: GH-6426

- It is not allowed to call functions from guards. The compiler failed to reject
  a call in a guard when done by constructing a record with a default
  initialization expression that called a function.

  Own Id: OTP-18325 Aux Id: GH-6465, GH-6466

- The compiler could crash when using a record with complex field initialization
  expression as a filter in a list comprehension.

  Own Id: OTP-18336 Aux Id: GH-6501, PR-6502

## Compiler 8.2.1

### Fixed Bugs and Malfunctions

- The compiler will now forbid using the empty atom `''` as module name. Also
  forbidden are modules names containing control characters, and module names
  containing only spaces and soft hyphens.

  Own Id: OTP-18125 Aux Id: GH-6026

- The `bin_opt_info` and `recv_opt_info` options would cause the compiler to
  crash when attempting to compile generated code without location information.

  Own Id: OTP-18162 Aux Id: PR-6102

- In rare circumstances involving floating point operations, the compiler could
  terminate with an internal consistency check failure.

  Own Id: OTP-18182 Aux Id: GH-6163

- In rare circumstances when doing arithmetic instructions on non-numbers, the
  compiler could crash.

  Own Id: OTP-18183 Aux Id: GH-6169

- In rare circumstances, complex boolean expressions in nested cases could cause
  the compiler to crash.

  Own Id: OTP-18184 Aux Id: GH-6164

- Expression similar to `#{assoc:=V} = #key=>self()}, V` would return the empty
  map instead of raising an exception.

  Own Id: OTP-18186

- Eliminated a crash in the `beam_ssa_bool` pass of the compiler when compiling
  a complex guard expression.

  Own Id: OTP-18187 Aux Id: GH-6184

- In rare circumstances, the compiler could crash with an internal consistency
  check failure.

  Own Id: OTP-18202 Aux Id: GH-6222

- When compiling with the option `inline_list_funcs`, the compiler could produce
  a nonsensical warning.

  Own Id: OTP-18214 Aux Id: GH-6158

- When given the `no_ssa_opt` option, the compiler could terminate with an
  internal consistency failure diagnostic when compiling map matching.

  Own Id: OTP-18234 Aux Id: GH-6277

### Improvements and New Features

- Made warnings for existing atoms being keywords in experimental features more
  precise, by not warning about quoted atoms.

  Own Id: OTP-18050

- There is a new configure option, `--enable-deterministic-build`, which will
  apply the `deterministic` compiler option when building Erlang/OTP. The
  `deterministic` option has been improved to eliminate more sources of
  non-determinism in several applications.

  Own Id: OTP-18165 Aux Id: PR-5965

## Compiler 8.2

### Fixed Bugs and Malfunctions

- A subtle bug regarding variable scoping has been corrected. Consider this
  example:

  `(A=1) + fun() -> A = 2() end`

  In the shell, the expression correctly evaluates to `3`. In compiled code, it
  raised a `{badmatch, 2}` exception.

  Own Id: OTP-17810 Aux Id: GH-5379

- Fixed a rare bug that would crash the compiler during type optimization.

  Own Id: OTP-17820

- Starting in OTP 24, when a fun was created and immediately used, it would be
  inlined. An unintended consequence of the inlining was that what would be a
  `function_clause` exception without the inlining would now be a rather
  confusing `case_clause` exception. This has been corrected, so that
  `function_clause` exceptions remain `function_clause` exceptions in inlined
  code.

  Own Id: OTP-17860 Aux Id: GH-5513, OTP-17226

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

- To enable more optimizations, BEAM files compiled with OTP 21 and earlier
  cannot be loaded in OTP 25.

  Own Id: OTP-16702

- Added support for the compile attribute `-nifs()` to empower compiler and
  loader with information about which functions may be overridden as NIFs by
  `erlang:load_nif/2`. It is recommended to use this attribute in all modules
  that load NIF libraries.

  Own Id: OTP-17151 Aux Id: ERIERL-590, PR-5479

- When binary construction using the binary syntax fails, the error message
  printed in the shell and by `erl_error:format_exception/3,4` will contain more
  detailed information about what went wrong.

  Own Id: OTP-17504 Aux Id: GH-4971, PR-5281, PR-5752

- The Erlang compiler now includes type information in BEAM files, and the JIT
  can now use that type information to do optimizations such as eliminating or
  simplifying type tests.

  Own Id: OTP-17684 Aux Id: PR-5316, PR-5664

- Improved the JIT's support for external tools like `perf` and `gdb`, allowing
  them to show line numbers and even the original Erlang source code when that
  can be found.

  To aid them in finding the source code, the `absolute_path` compiler option
  has been added to embed the absolute file path of a module.

  Own Id: OTP-17685

- The `maybe` ... `end` construction proposed in EEP-49 has been implemented. It
  can simplify complex code where otherwise deeply nested cases would have to be
  used.

  To enable `maybe`, give the option `-enable-feature maybe_expr` to `erlc` or
  add `-feature(maybe_expr, enable).` inside the module.

  Own Id: OTP-17705 Aux Id: PR-5411

- When a record matching or record update fails, a
  `{badrecord,ExpectedRecordTag}` exception used to be raised. In this release,
  the exception has been changed to `{badrecord,ActualValue}`, where
  `ActualValue` is the actual that was found instead of the expected record.

  Own Id: OTP-17841 Aux Id: PR-5694

- Improved optimization of try/catch expressions.

  Own Id: OTP-17842

- The `beam_trim` pass of the compiler could be extremely slow for huge
  straight-line functions. It will now compile such functions much faster (down
  to seconds from minutes for some huge functions).

  Own Id: OTP-17885 Aux Id: GH-5140

- Added support for configurable features as described in EEP-60. Features can
  be enabled/disabled during compilation with options
  (`-enable-feature Feature`, `-disable-feature Feature` and
  `+{feature, Feature, enable|disable}`) to `erlc` as well as with directives
  (`-feature(Feature, enable|disable).`) in the file. Similar options can be
  used to `erl` for enabling/disabling features allowed at runtime. The new
  `maybe` expression (EEP-49) is fully supported as the feature `maybe_expr`.
  The features support is documented in the reference manual.

  Own Id: OTP-17988

## Compiler 8.1.1.6

### Fixed Bugs and Malfunctions

* In rare circumstances, an unsafe optimization could cause the compiler to generate incorrect code for list matching.

  Own Id: OTP-19003 Aux Id: GH-8187, PR-8189
* In rare circumstances, the compiler code generate unsafe code for a bit syntax match.

  Own Id: OTP-19019

## Compiler 8.1.1.5

### Fixed Bugs and Malfunctions

- The compiler could be very slow when compiling guards with multiple guard
  tests separated with '`or`' or '`;`'.

  Own Id: OTP-18617 Aux Id: GH-7338

## Compiler 8.1.1.4

### Fixed Bugs and Malfunctions

- Complex guard expression using the `or` operator and guard BIFs that can fail
  could sometimes be miscompiled so that the guard would succeed even if a call
  to a guard BIF failed.

  Own Id: OTP-18593 Aux Id: GH-7252

## Compiler 8.1.1.3

### Fixed Bugs and Malfunctions

- The compiler would generate incorrect code for the following type of
  expression:

  `Pattern = BoundVar1 = . . . = BoundVarN = Expression`

  An exception should be raised if any of the bound variables have different
  values than `Expression`. The compiler would generate code that would cause
  the bound variables to be bound to the value of `Expression`whether the value
  matched or not.

  Own Id: OTP-18470 Aux Id: GH-6873, PR-6877

## Compiler 8.1.1.2

### Fixed Bugs and Malfunctions

- It is not allowed to call functions from guards. The compiler failed to reject
  a call in a guard when done by constructing a record with a default
  initialization expression that called a function.

  Own Id: OTP-18325 Aux Id: GH-6465, GH-6466

- Fixed a bug that could cause legal code to fail validation.

  Own Id: OTP-18365

## Compiler 8.1.1.1

### Fixed Bugs and Malfunctions

- The `bin_opt_info` and `recv_opt_info` options would cause the compiler to
  crash when attempting to compile generated code without location information.

  Own Id: OTP-18162 Aux Id: PR-6102

- In rare circumstances involving floating point operations, the compiler could
  terminate with an internal consistency check failure.

  Own Id: OTP-18182 Aux Id: GH-6163

## Compiler 8.1.1

### Fixed Bugs and Malfunctions

- Fixed a performance bug in the validator that made certain files take a very
  long time to compile.

  Own Id: OTP-18066 Aux Id: GH-5915

- In rare circumstances, the compiler would mistakenly assume that a call to
  [`setelement/3`](`setelement/3`) would always fail and remove all code
  following the call.

  Own Id: OTP-18082

## Compiler 8.1

### Fixed Bugs and Malfunctions

- The expression `<<0/native-float>>=Bin` would always fail to match, while
  `<<0/float-native>>=Bin` would match (provided that `Bin` contained the binary
  representation of `0.0`)

  Own Id: OTP-17895

### Improvements and New Features

- The compiler will now compile huge functions with straight-line code faster.

  Own Id: OTP-17886 Aux Id: GH-5140, GH-5686

## Compiler 8.0.4

### Fixed Bugs and Malfunctions

- When the compiler is invoked by Dialyzer, it will no longer apply an
  optimization of binary patterns that would turn the pattern `<<"bar">>` into
  `<<6447474:24>>`, which would be very confusing when printed out by Dialyzer.

  Own Id: OTP-17768 Aux Id: GH-5429

- The compiler would replace known failing calls (such as
  [`atom_to_list(42)`](`atom_to_list/1`)) with a call to
  [`error(badarg)`](`error/1`). With the extended error information introduced
  in OTP 24 (EEP 54), those "optimized" calls would not have extended error
  information. To ensure that as much extended error information as possible is
  available, the compiler now keeps the original call even when it is known to
  fail.

  Own Id: OTP-17786 Aux Id: GH-5440

## Compiler 8.0.3

### Fixed Bugs and Malfunctions

- If a parse transform raised an exception using [`throw/1`](`throw/1`) or
  [`exit/1`](`exit/1`), the compiler would report that as an internal compiler
  error, which would be confusing. Amended to report that the parse transform
  failed.

  Own Id: OTP-17421

- The failing call `io:format("~p\n")` would result in a warning for line number
  0 instead of the correct line and column numbers. This has been corrected, and
  all warnings for failing calls to [`io:format()`](`t:io:format/0`) has been
  rephrased to make it clearer exactly what the problem is.

  Own Id: OTP-17430

- When the options `warn_missing_spec` and `export_all` were given, there would
  only be warnings for missing specs for functions that had been explicitly
  exported using an `-export` attribute.

  Own Id: OTP-17434 Aux Id: GH-4772

- In rare circumstances, the compiler could emit an incorrect warning for a term
  that was constructed but never used.

  Own Id: OTP-17446 Aux Id: PR-4899

- Corrected bugs where builds were not reducible even when the `deterministic`
  option was given. In particular, modules with map literals with more than 32
  elements could cause this problem.

  As part of this fix, the `term_to_binary` BIF now accepts the option
  `deterministic`.

  Own Id: OTP-17495 Aux Id: PR-5153

- The `MODULE` and `MODULE_STRING` macros would always appear to be defined
  (when tested by `-ifdef`), even though no `-module()` declaration had been
  seen yet. Changed so that `-ifdef ?MODULE.` will not consider ?MODULE defined
  if `-module()` has not been previously seen.

  Own Id: OTP-17505 Aux Id: GH-4995

- In a guard, `not (X =:= true)` would incorrectly evaluate to `false` for
  non-boolean values of `X`.

  Own Id: OTP-17510 Aux Id: GH-5007

- When the `deterministic` option was given to the compiler, the `?FILE` macro
  would be expanded to full path of the source file before the first `include`
  directive and to base part of the filename after `include` directive.

  Own Id: OTP-17581 Aux Id: PR-5141

## Compiler 8.0.2

### Fixed Bugs and Malfunctions

- A compiler optimization pass could crash when given odd but legal code using
  [`throw/1`](`throw/1`).

  Own Id: OTP-17489 Aux Id: GH-4953

## Compiler 8.0.1

### Fixed Bugs and Malfunctions

- Fixed a bug that could cause `after` blocks to be ignored when
  `erlang:raise/3` was used in a catch block.

  Own Id: OTP-17428 Aux Id: GH-4859

- Fixed a bug in the validation pass that could cause it to reject valid code.

  Own Id: OTP-17437 Aux Id: OTP-17357, GH-4774

## Compiler 8.0

### Fixed Bugs and Malfunctions

- A floating point zero (0.0) can be both positive (+0.0) and negative (-0.0).
  Multiple bugs in the compiler, runtime system, and STDLIB have been fixed to
  ensure that the minus sign on 0.0 is not lost.

  Own Id: OTP-17077 Aux Id: ERL-1431, PR-2903, PR-2905, PR-2906

- A repeated stack trace variable in a try/catch was not rejected. The following
  example will now cause a compilation error:

  ```text
  try E catch _:A:A -> A
  	    end.
  ```

  Own Id: OTP-17104 Aux Id: ERL-1380

- Eliminated a Dialyzer crashed when the `-MMD` option is used to generate a
  dependency file and a BEAM file a the same time.

  Own Id: OTP-17118 Aux Id: PR-2825

- When the `makedep` option was given, the compiler would crash if the
  dependency output contained non-latin1 characters. The compiler will now
  output the dependency information encoded in UTF-8 to avoid crashing.

  Own Id: OTP-17206

### Improvements and New Features

- Selective receive optimization will now be applied much more often.

  The new [`recv_opt_info`](`e:system:eff_guide_processes.md#recv_opt_info`)
  compile flag can be used to print diagnostics relating to this optimization.

  You can read more about the
  [selective receive optimization](`e:system:eff_guide_processes.md#receiving-messages`)
  in the Efficiency Guide.

  Own Id: OTP-10391 Aux Id: OTP-16226

- `erlang:throw/1` will no longer build stack traces when we can prove that they
  will never be inspected.

  Own Id: OTP-16334

- Variables bound between the keywords 'try' and 'of' can now be used in the
  clauses following the 'of' keyword (that is, in the success case when no
  exception was raised).

  Own Id: OTP-16706 Aux Id: ERL-1281

- Compiler warnings and errors now include column numbers in addition to line
  numbers.

  When a compiler message is emitted, the source line is printed along with a
  marker (a `^` character) that indicates the column position of the issue. The
  option '`brief`' removes the printout of the source line.

  The compiler option `{error_location, line | column}` has been added. The
  default value is `column`. Besides adding column numbers to compilation
  warnings and errors, the option also determines whether column numbers are
  included in abstract code. If tools stop working, setting the environment
  variable `ERL_COMPILER_OPTIONS` can help (include `{error_location, line}`).

  The compiler will now call the function `PT`:`parse_transform_info/0` in parse
  transforms (if it exists). It can be used by parse transforms to signal that
  they can only handle line numbers in abstract code.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16824 Aux Id: PR-2664, PR-3006

- Fixed a performance bug that made functions with lots of `try/after` blocks
  slow to compile.

  Own Id: OTP-16867 Aux Id: ERL-1354

- The experimental HiPE application has been removed, together with all related
  functionality in other applications.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16963

- Generators in list and binary comprehensions will now raise a
  `{bad_generator,Generator}` exception if the generator has an incorrect type
  (instead of raising an ad-hoc `badarg` or `badarih` exception). Similarly,
  when a filter does not evaluate to a boolean, a `{bad_filter,Filter}`
  exception will be raised. Some minor bugs in the compilation of binary
  comprehensions have also been fixed.

  Own Id: OTP-16964

- Some compiler warnings, such as the warning for an expression whose result is
  ignored, could not be suppressed by assigning to a variable beginning with
  '`_`', but only by assigning to the anonymous variable ('`_`'). This has now
  been changed so that any warning that can be suppressed by assigning to the
  anonymous variable can also be suppressed by assigning to a variable beginning
  with '`_`'.

  Own Id: OTP-16981 Aux Id: ERL-1113

- The previously undocumented compiler options `warn_missing_spec` and
  `warn_missing_spec_all` are now documented.

  Own Id: OTP-17078 Aux Id: ERL-1430, PR-2918

- The compiler will now emit warnings when (previously bound)
  underscore-prefixed variables are matched.

  Own Id: OTP-17123

- Erlang source files not encoded in utf-8 will no longer be accepted by the
  compiler unless it contains a "coding: latin-1" comment.

  Own Id: OTP-17168

- New compiler options `from_abstr` and `no_lint` have been added. They are
  useful when implementing other languages running on the BEAM.

  Own Id: OTP-17172

- The bit matching and construction syntax now supports 16-bit floats (IEEE
  754-2008).

  Own Id: OTP-17207

- The compiler will now inline funs that are used only once immediately after
  their definition.

  Own Id: OTP-17226 Aux Id: GH-4019, PR-4545

- It is now possible to disable warnings emitted from the compiler's
  optimization passes with the new options `nowarn_opportunistic`,
  `nowarn_nomatch`, `nowarn_ignored`, and `nowarn_failed`.

  Own Id: OTP-17260

- Introduce new types `t:nonempty_binary/0` and `t:nonempty_bitstring/0`.

  Own Id: OTP-17301 Aux Id: GH-4636

- Add compiler option `{nowarn_unused_record, RecordNames}`. Document compiler
  option `nowarn_unused_type`.

  Own Id: OTP-17330

## Compiler 7.6.9.3

### Fixed Bugs and Malfunctions

- It is not allowed to call functions from guards. The compiler failed to reject
  a call in a guard when done by constructing a record with a default
  initialization expression that called a function.

  Own Id: OTP-18325 Aux Id: GH-6465, GH-6466

- Fixed a bug that could cause legal code to fail validation.

  Own Id: OTP-18365

- The compiler would generate incorrect code for the following type of
  expression:

  `Pattern = BoundVar1 = . . . = BoundVarN = Expression`

  An exception should be raised if any of the bound variables have different
  values than `Expression`. The compiler would generate code that would cause
  the bound variables to be bound to the value of `Expression`whether the value
  matched or not.

  Own Id: OTP-18470 Aux Id: GH-6873, PR-6877

## Compiler 7.6.9.2

### Fixed Bugs and Malfunctions

- In rare circumstances, the compiler would mistakenly assume that a call to
  [`setelement/3`](`setelement/3`) would always fail and remove all code
  following the call.

  Own Id: OTP-18082

## Compiler 7.6.9.1

### Fixed Bugs and Malfunctions

- Fixed a bug in the validation pass that could cause it to reject valid code.

  Own Id: OTP-17437 Aux Id: OTP-17357, GH-4774

## Compiler 7.6.9

### Fixed Bugs and Malfunctions

- Reverted the fix for `OTP-17357` as it turned out to be incomplete and made
  the validator reject much more legal code than before.

  It will be fixed more thoroughly in a later patch.

  Own Id: OTP-17386 Aux Id: ERIERL-650, OTP-17357

## Compiler 7.6.8

### Fixed Bugs and Malfunctions

- Fixed a bug in the validator that could cause it to reject valid code.

  Own Id: OTP-17357 Aux Id: GH-4774

## Compiler 7.6.7

### Fixed Bugs and Malfunctions

- Fixed a bug in the type optimization pass that could yield incorrect values or
  cause the wrong clauses to be executed.

  Own Id: OTP-17073

- Fixed a bug in the validator that could cause it to reject valid code.

  Own Id: OTP-17126 Aux Id: ERL-1471

## Compiler 7.6.6

### Fixed Bugs and Malfunctions

- Several minor compiler bugs have been fixed:

  Constructing a binary with a list as a size of a binary segment could generate
  a BEAM file that could not be loaded.

  When matching a binary segment of type `float` and ignoring the matched out
  value, the match would always succeed, even if the size was invalid or the
  value of the float was NaN or some other non-numeric float value.

  Attempting to construct an invalid external fun (e.g. `fun m:f:bad`) is
  supposed to raise a '`badarg`' exception, but if the value was never used, no
  exception would be raised.

  Own Id: OTP-16932

- Fixed multiple bugs in the validator that could cause it to reject valid code.

  Own Id: OTP-17039 Aux Id: ERL-1426

- The compiler could crash when a binary comprehension had a generator that
  depended on another generator.

  Own Id: OTP-17045 Aux Id: ERL-1427

- Fixed a bug in the type optimization pass that could yield incorrect values or
  cause the wrong clauses to be executed.

  Own Id: OTP-17072 Aux Id: ERL-1440

## Compiler 7.6.5

### Fixed Bugs and Malfunctions

- Fixed a bug in the boolean optimization pass that caused the compiler to
  confuse different clauses.

  Own Id: OTP-16951 Aux Id: ERL-1384

## Compiler 7.6.4

### Fixed Bugs and Malfunctions

- Fixed a performance bug that could be triggered by tuple matching in very
  large functions.

  Own Id: OTP-16895 Aux Id: ERL-1359

## Compiler 7.6.3

### Fixed Bugs and Malfunctions

- If the update of a map with the '`Map#{Key := Value}`' syntax failed, the line
  number in the stack backtrace could be incorrect.

  Own Id: OTP-16701 Aux Id: ERL-1271

- Fixed a performance bug that slowed down compilation of modules with deeply
  nested terms.

  Own Id: OTP-16755 Aux Id: ERL-1297

- The compiler could in rare circumstances do an an unsafe optimization that
  would result in a matching of a nested map pattern would fail to match.

  Own Id: OTP-16820

- Fixed a bug in the validator that caused it to reject valid code.

  Own Id: OTP-16838 Aux Id: ERL-1340

## Compiler 7.6.2

### Fixed Bugs and Malfunctions

- When calls to `is_map_key` were repeated, the compiler could terminate with an
  internal consistency failure.

  Own Id: OTP-16708 Aux Id: ERL-1276

- Fixed a bug in the type inference pass that could cause the compiler to hang.

  Own Id: OTP-16745 Aux Id: ERL-1289

## Compiler 7.6.1

### Fixed Bugs and Malfunctions

- In rare circumstances, a guard using 'not' could evaluate to the wrong boolean
  value.

  Own Id: OTP-16652 Aux Id: ERL-1246

- A guard expression that referenced a variable bound to a boolean expression
  could evaluate to the wrong value.

  Own Id: OTP-16657 Aux Id: ERL-1253

## Compiler 7.6

### Fixed Bugs and Malfunctions

- `erlang:fun_info(fun foo/1, name/1)` used to return a function name based on
  the name of the function that `fun foo/1` was used in. The name returned is
  now `-fun.foo/1-`.

  Own Id: OTP-15837

- Initialization of record fields using `_` is no longer allowed if the number
  of affected fields is zero.

  Own Id: OTP-16516

### Improvements and New Features

- EEP-52 has been implemented.

  In binary matching, the size of the segment to be matched is now allowed to be
  a guard expression, and similarly in map matching the keys can now be guard
  expressions. See the Erlang Reference Manual and Programming Examples for more
  details.

  Language compilers or code generators that generate Core Erlang code may need
  to be updated to be compatible with the compiler in OTP 23. For more details,
  see the section Backwards Compatibility in
  [EEP 52](http://erlang.org/eeps/eep-0052.html).

  Own Id: OTP-14708

- Allow underscores in numeric literals to improve readability. Examples:
  `123_456_789`, `16#1234_ABCD`.

  Own Id: OTP-16007 Aux Id: PR-2324

- Improved the type optimization pass' inference of types that depend on
  themselves, giving us more accurate types and letting us track the content
  types of lists.

  Own Id: OTP-16214 Aux Id: PR-2460

- Support message queue optimization also for references returned from the new
  [`spawn_request()`](`erlang:spawn_request/5`) BIFs.

  Own Id: OTP-16367 Aux Id: OTP-15251

- The compiler will now raise a warning when inlining is used in modules that
  load NIFs.

  Own Id: OTP-16429 Aux Id: ERL-303

- Refactored the internal handling of deprecated and removed functions.

  Own Id: OTP-16469

- Line information was sometimes incorrect for floating-point math exceptions.

  Own Id: OTP-16505 Aux Id: ERL-1178

- The `debug_info` option can now be specified in `-compile()` attributes.

  Own Id: OTP-16523 Aux Id: ERL-1058

- Reduced the resource usage of `erlc` in parallel builds (e.g. `make -j128`).

  Own Id: OTP-16543 Aux Id: ERL-1186

## Compiler 7.5.4.3

### Improvements and New Features

- Fixed a bug in the type optimization pass that could yield incorrect values or
  cause the wrong clauses to be executed.

  Own Id: OTP-17073

## Compiler 7.5.4.2

### Fixed Bugs and Malfunctions

- Fixed a bug in the validator that could cause it to reject valid code

  Own Id: OTP-17039 Aux Id: ERL-1426

## Compiler 7.5.4.1

### Fixed Bugs and Malfunctions

- Fixed a bug that could cause the compiler to crash on code that constructed
  binaries.

  Own Id: OTP-16747 Aux Id: ERL-1290

## Compiler 7.5.4

### Fixed Bugs and Malfunctions

- Fixed a bug in the validator that could cause it to reject valid code.

  Own Id: OTP-16580 Aux Id: ERL-1212

## Compiler 7.5.3

### Fixed Bugs and Malfunctions

- A '`receive`' with an '`after 0`' clause would prevent the optimization that
  can avoid scanning the entire receive queue when matching on a newly created
  reference.

  Own Id: OTP-16350

- HiPE can again handle modules with `catch` and `try` constructs.

  Own Id: OTP-16418

- Fixed a bug in bit-syntax optimization that could crash the compiler.

  Own Id: OTP-16515

## Compiler 7.5.2

### Fixed Bugs and Malfunctions

- Fixed a bug that could cause the compiler to reject valid code that used the
  [`is_map_key/2`](`is_map_key/2`) BIF.

  Own Id: OTP-16452 Aux Id: ERL-1161

- Fixed a bug that could cause the compiler to reject valid code that matched
  the same map key several times.

  Own Id: OTP-16456 Aux Id: ERL-1163

- The compiler could crash when compiling a convoluted `receive` statement.

  Own Id: OTP-16466 Aux Id: ERL-1170

- The compiler could crash when a fun was created but never used.

  The compiler could crash when compiling the expression `true = 0 / X`.

  Own Id: OTP-16467 Aux Id: ERL-1166, ERL-1167

## Compiler 7.5.1

### Fixed Bugs and Malfunctions

- Fixed a bug in the compiler that could cause it to reject valid code.

  Own Id: OTP-16385 Aux Id: ERL-1128

## Compiler 7.5

### Fixed Bugs and Malfunctions

- Fixed a bug in the linter where list and binary comprehensions could suppress
  unsafe variable errors.

  Own Id: OTP-16053 Aux Id: ERL-1039

- When a compilation starts from Core Erlang code, the `core_lint` pass will
  always be run and the compilation will be aborted if any errors are found.

  Own Id: OTP-16181 Aux Id: ERL-1065

### Improvements and New Features

- The warning message that appears when the compiler detects a non-utf-8 encoded
  source file without an encoding string in the beginning of the file has been
  changed to contain information about that support for latin1 encoded source
  files without an encoding string will be removed in Erlang/OTP 24.

  Own Id: OTP-16054 Aux Id: OTP-11791

## Compiler 7.4.9

### Fixed Bugs and Malfunctions

- Fixed a performance bug that caused repeated matches of large records to take
  a very long time to compile.

  Own Id: OTP-16259 Aux Id: ERIERL-436

## Compiler 7.4.8

### Fixed Bugs and Malfunctions

- The compiler could do an unsafe optimization of receives, which would cause a
  receive to only scan part of the message queue.

  This bug fix in the compiler fixes a bug in the socket module.

  Own Id: OTP-16219 Aux Id: ERL-1076

## Compiler 7.4.7

### Fixed Bugs and Malfunctions

- Fixed a bug where the compiler could generate incorrect code for a '`receive`'
  statement inside a '`try`'.

  Own Id: OTP-16199

## Compiler 7.4.6

### Fixed Bugs and Malfunctions

- Fixed a bug in the bit-syntax optimization pass that could crash the compiler.

  Own Id: OTP-16103 Aux Id: ERL-1050

## Compiler 7.4.5

### Fixed Bugs and Malfunctions

- Code such as the following would crash the compiler in OTP 22:
  `[some_atom = fun some_function/1]`

  Own Id: OTP-15833

- Compilation could get really slow (in the order of minutes instead of seconds)
  when compiling huge functions. (Thanks to Kostis Sagonas for reporting this
  bug.)

  Own Id: OTP-15923

- Fixed a bug in the validator that could reject valid code.

  Own Id: OTP-15954 Aux Id: ERL-995

- In rare circumstances, when two clauses had identical bodies and guard tests
  that tested a single boolean variable, the guard test for the second clause
  could be discarded, executing the second clause unconditionally if the first
  clause was not executed.

  Own Id: OTP-15963

- Fixed extremely slow compilation for huge functions doing predominantly
  pattern matching.

  Own Id: OTP-15966 Aux Id: ERL-1014

- The compiler could generate unsafe code (that would crash the runtime system)
  for map pattern matching. The code could be unsafe if the matched key was not
  present in the map at runtime.

  Own Id: OTP-15968 Aux Id: ERL-1017

- Correct code using try/after could fail to compile when using the option
  '`no_type_opt`'.

  Own Id: OTP-15969 Aux Id: ERL-997

- The compiler could crash when compiling code that called
  '[`length/1`](`length/1`)' on a binary extracted using the binary syntax.

  Own Id: OTP-15970 Aux Id: ERL-1013

- Fixed a bug where the compiler could fail with an internal consistency failure
  error when compiling receive statements.

  Own Id: OTP-15982 Aux Id: ERL-1022

- Fixed a problem where the compiler would crash when compiling binary matching
  in a function head.

  Own Id: OTP-15985 Aux Id: ERL-1026

## Compiler 7.4.4

### Fixed Bugs and Malfunctions

- Fixed a compiler crash introduced in `22.0.6` (OTP-15952).

  Own Id: OTP-15953 Aux Id: ERL-999

## Compiler 7.4.3

### Fixed Bugs and Malfunctions

- Fixed an unsafe optimization when matching [`tuple_size/1`](`tuple_size/1`)
  outside of guards, which could crash the emulator if the argument was not a
  tuple.

  Own Id: OTP-15945

- Fixed a rare bug that could cause the wrong kind of exception to be thrown
  when a BIF failed in a function that matched bitstrings.

  Own Id: OTP-15946

- Fixed a bug where receive statements inside try/catch blocks could return
  incorrect results.

  Own Id: OTP-15952

## Compiler 7.4.2

### Fixed Bugs and Malfunctions

- Fixed an incorrect type determination for constructed binaries, which could
  cause `is_binary` checks to succeed when they shouldn't have.

  Own Id: OTP-15872

## Compiler 7.4.1

### Fixed Bugs and Malfunctions

- The type optimization pass of the compiler could hang or loop for a long time
  when analyzing a [`setelement/3`](`setelement/3`) call with a variable
  position.

  Own Id: OTP-15828 Aux Id: ERL-948

- Certain complex receive statements would result in an internal compiler
  failure.

  Own Id: OTP-15832 Aux Id: ERL-950

- Fixed an unsafe type optimization.

  Own Id: OTP-15838

- Fixed a crash when optimizing compiler-generated exceptions (like badmatch)
  whose offending term was a constructed binary.

  Own Id: OTP-15839 Aux Id: ERL-954

- Fixed a bad optimization related to the `++/2` operator, where the compiler
  assumed that it always produced a list (`[] ++ RHS` returns `RHS` verbatim,
  even if it's not a list).

  Own Id: OTP-15841

- An [`is_binary/1`](`is_binary/1`) test followed by
  [`is_bitstring/1`](`is_bitstring/1`) (or vice versa) could fail because of an
  usafe optimization.

  Own Id: OTP-15845

- A Core Erlang module where the last clause in a `case` matched a map would
  fail to load.

  Own Id: OTP-15846 Aux Id: ERL-955

- Fixed a bug that could cause the compiler to crash when compiling complex
  nested case expressions.

  Own Id: OTP-15848 Aux Id: ERL-956

## Compiler 7.4

### Fixed Bugs and Malfunctions

- `record_info/2` is a pseudo-function that requires literal arguments known at
  compile time. Therefore, the following usage is illegal: `fun record/info/2`.
  The compiler would crash when during compilation of that kind of code.
  Corrected to issue a compilation error.

  Own Id: OTP-15760 Aux Id: ERL-907

### Improvements and New Features

- The compiler has been rewritten to internally use an intermediate
  representation based on Static Single Assignment (SSA). The new intermediate
  representation makes more optimizations possible.

  Most noticeable is that the binary matching optimizations are now applicable
  in many more circumstances than before.

  Another noticeable change is that type optimizations are now applied across
  local function calls, and will remove a lot more redundant type tests than
  before.

  Own Id: OTP-14894 Aux Id: ERL-714

- Funs are no longer created when they are only used locally, greatly improving
  the performance of named funs and "fun-wrapped" macros.

  Own Id: OTP-15273 Aux Id: ERL-639

- All compiler options that can be given in the source file can now also be
  given in the option list or from the command line for `erlc`.

  Specifically, the option `{nowarn_deprecated_function,MFAs}` was only
  recognized when given in the file with the attribute `-compile()`. The option
  `{nowarn_unused_function,FAs}` was incorrectly documented to only work in a
  file, but it also worked when given in the option list.

  Own Id: OTP-15456

- Do not allow function specifications for functions residing in other modules.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15563 Aux Id: ERL-845, OTP-15562

- Internal documentation has now been added to the _Erts_ and _Compiler_
  applications.

  The internal documents for _Erts_ describe miscellaneous interesting
  implementation details. Those details can change at any time.

  The internal documentation for _Compiler_ documents the API for the Core
  Erlang modules. While we will not change those APIs without good reason, we
  don't give the same guarantees about backward compatibility as for the rest of
  the APIs in OTP.

  Own Id: OTP-15715

- There are new compiler options `nowarn_removed` and `{nowarn_removed,Items}`
  to suppress warnings for functions and modules that have been removed from
  OTP.

  Own Id: OTP-15749 Aux Id: ERL-904

## Compiler 7.3.2

### Fixed Bugs and Malfunctions

- An expression such as `(A / B) band 16#ff` would crash the compiler.

  Own Id: OTP-15518 Aux Id: ERL-829

- There could be an incorrect warning when the `tuple_calls` option was given.
  The generated code would be correct. Here is an example of code that would
  trigger the warning:

  `(list_to_atom("prefix_" ++ atom_to_list(suffix))):doit(X)`.

  Own Id: OTP-15552 Aux Id: ERL-838

- Optimize (again) Dialyzer's handling of left-associative use of `andalso` and
  `orelse` in guards.

  Own Id: OTP-15577 Aux Id: ERL-851, PR-2141, PR-1944

## Compiler 7.3.1

### Fixed Bugs and Malfunctions

- An optimization that avoided allocation of a stack frame for some `case`
  expressions was introduced in OTP 21. (ERL-504/OTP-14808) It turns out that in
  rare circumstances, this optimization is not safe. Therefore, this
  optimization has been disabled.

  A similar optimization will be included in OTP 22 in a safe way.

  Own Id: OTP-15501 Aux Id: ERL-807, ERL-514, OTP-14808

## Compiler 7.3

### Fixed Bugs and Malfunctions

- Fixed a rare internal consistency failure caused by a bug in the `beam_jump`
  pass. (Thanks to Simon Cornish for reporting this bug.)

  Own Id: OTP-15400 Aux Id: ERL-759

- The compiler could fail with an internal consistency check failure when
  compiling code that used the [`is_function/2`](`is_function/2`) BIF.

  Own Id: OTP-15435 Aux Id: ERL-778

- When an external fun was used, warnings for unused variables could be
  suppressed.

  Own Id: OTP-15437 Aux Id: ERL-762

- The compiler would crash when compiling an `after` block that called
  `erlang:raise/3` like this: `erlang:raise(Class, Stacktrace, Stacktrace)`

  Own Id: OTP-15481

### Improvements and New Features

- When specified, the `+{source,Name}` option will now override the actual file
  name in stack traces, instead of only affecting the return value of
  `Mod:module_info()`.

  The `+deterministic` flag will also affect stack traces now, omitting all path
  information except the file name, fixing a long-standing issue where
  deterministic builds required deterministic paths.

  Own Id: OTP-15245 Aux Id: ERL-706

## Compiler 7.2.7

### Fixed Bugs and Malfunctions

- Fixed a bug where incorrect code was generated following a binary match guard.

  Own Id: OTP-15353 Aux Id: ERL-753

## Compiler 7.2.6

### Fixed Bugs and Malfunctions

- In rare circumstances, the matched out tail of a binary could be the entire
  original binary. (There was partial correction to this problem in version
  7.2.5 of the compiler application.)

  Own Id: OTP-15335 Aux Id: ERL-689, OTP-15219

## Compiler 7.2.5

### Fixed Bugs and Malfunctions

- Fixed a bug that prevented certain variable-sized binary comprehensions from
  compiling.

  Own Id: OTP-15186 Aux Id: ERL-665

- When compiling from Core Erlang, funs created in certain expressions that were
  only used for their side-effects were subtly broken.

  Own Id: OTP-15188 Aux Id: ERL-658

- There could be an internal consistency failure when a `receive` was nested in
  a `try`/`catch`.

  Own Id: OTP-15218 Aux Id: ERL-684

- In rare circumstances, the matched out tail of a binary could be the entire
  original binary.

  Own Id: OTP-15219 Aux Id: ERL-689

- When [`is_map_key/2`](`is_map_key/2`) was used in a guard together with the
  `not/1` or `or/2` operators, the error behavior could be wrong when
  [`is_map_key/2`](`is_map_key/2`) was passed a non-map as the second argument.

  In rare circumstances, compiling code that uses
  [`is_map_key/2`](`is_map_key/2`) could cause an internal consistency check
  failure.

  Own Id: OTP-15227 Aux Id: ERL-699

- The compiler could crash when compiling a function with multiple receives in
  multiple clauses.

  Own Id: OTP-15235 Aux Id: ERL-703

## Compiler 7.2.4

### Fixed Bugs and Malfunctions

- Fix a regression in OTP-15204 that removed `.beam` file metadata that some
  external build tools relied on.

  Own Id: OTP-15292

## Compiler 7.2.3

### Fixed Bugs and Malfunctions

- Fixed an issue where files compiled with the `+deterministic` option differed
  if they were compiled in a different directory but were otherwise identical.

  Own Id: OTP-15204 Aux Id: ERL-679

## Compiler 7.2.2

### Fixed Bugs and Malfunctions

- In rare cases involving matching of binary literal strings, the compiler could
  optimize away code that should be executed.

  Own Id: OTP-15156 Aux Id: ERL-655

- There could be an internal consistency check failure when compiling code that
  called [`map_get(Key, Map)`](`map_get/2`) and then updated the same map.

  Own Id: OTP-15157

- In rare circumstances, the compiler could crash in `beam_jump` when compiling
  a floating point operation.

  Own Id: OTP-15166 Aux Id: ERL-660

## Compiler 7.2.1

### Fixed Bugs and Malfunctions

- The could could crash when compiling a complicated function that used the
  binary syntax.

  Own Id: OTP-15150 Aux Id: ERL-650

## Compiler 7.2

### Fixed Bugs and Malfunctions

- Fixed an error in an optimization pass that caused impossible tuple matching.

  Own Id: OTP-14855 Aux Id: ERL-549

- The exception thrown when a list comprehension was given a non-list term was
  not always correct.

  Own Id: OTP-14992 Aux Id: ERL-572

- The compiler could produce incorrect code in rare circumstances when the
  `[{inline,F/A}]` option was used.

  Own Id: OTP-15115 Aux Id: PR-1831

### Improvements and New Features

- Changed the default behaviour of `.erlang` loading: `.erlang` is no longer
  loaded from the current directory. `c:erlangrc(PathList)` can be used to
  search and load an `.erlang` file from user specified directories.

  `escript`, `erlc`, `dialyzer` and `typer` no longer load an `.erlang` at all.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14439

- Support for "tuple calls" have been removed from the run-time system. Tuple
  calls was an undocumented and unsupported feature which allowed the module
  argument for an apply operation to be a tuple: `Var = dict:new(), Var:size()`.
  This "feature" frequently caused confusion, especially when such call failed.
  The stacktrace would point out functions that don't exist in the source code.

  For legacy code that need to use parameterized modules or tuple calls for some
  other reason, there is a new compiler option called `tuple_calls`. When this
  option is given, the compiler will generate extra code that emulates the old
  behavior for calls where the module is a variable.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14497

- In code such as `example({ok, Val}) -> {ok, Val}.` a tuple would be built. The
  compiler will now automatically rewrite the code to
  `example({ok,Val}=Tuple) -> Tuple.` which will reduce code size, execution
  time, and remove GC pressure.

  Own Id: OTP-14505

- The optimization of `case` expression where only one of the case arms can
  execute successfully has been improved.

  Own Id: OTP-14525

- Some uses of binary matching has been slightly improved, eliminating
  unnecessary register shuffling.

  Own Id: OTP-14594 Aux Id: ERL-444

- There is a new `{compile_info,Info}` option for the compiler that allows
  BEAM-based languages such as Elixir and LFE to add their own compiler
  versions.

  Own Id: OTP-14615 Aux Id: PR-1558

- Loaded BEAM code in a 64-bit system requires less memory because of better
  packing of operands for instructions.

  These memory savings were achieved by major improvements to the `beam_makeops`
  scripts used when building the run time system and BEAM compiler. There is
  also new for documentation for `beam_makeops` that describes how new BEAM
  instructions and loader transformations can be implemented. The documentation
  is found in here in a source directory or git repository:
  erts/emulator/internal_doc/beam_makeops.md. An online version can be found
  here:
  https://github.com/erlang/otp/blob/master/erts/emulator/internal_doc/beam_makeops.md

  Own Id: OTP-14626

- Size calculations for binary constructions has been somewhat optimized,
  producing smaller code.

  Own Id: OTP-14654

- When the value returned from a '`catch`' expression is ignored, no stacktrace
  will be built if an exception is caught. That will save time and produce less
  garbage. There are also some minor optimizations of '`try`/`catch`' both in
  the compiler and run-time system.

  Own Id: OTP-14683

- There is a new syntax in '`try/catch`' for retrieving the stacktrace without
  calling '`erlang:get_stacktrace/0`'. See the reference manual for a
  description of the new syntax. The '`erlang:get_stacktrace/0`' BIF is now
  deprecated.

  Own Id: OTP-14692

- The following is an internal change in the compiler, that is not noticeable
  for normal use of the compiler: The module `v3_life` has been removed. Its
  functionality has been simplified and integrated into `v3_codegen`.

  Own Id: OTP-14712

- The optimization of binary matching that delays creation of sub binaries (see
  the Efficiency Guide) could be thwarted by the argument order and could be
  necessary to change the argument order. The compiler has now become smarter
  and can handle any argument order.

  Own Id: OTP-14774

- When the compiler was faced with complex case expressions it would
  unnecessarily allocate stack elements and shuffle data between x and y
  registers. Improved code generation to only allocate a stack frame when
  strictly necessary.

  Own Id: OTP-14808 Aux Id: ERL-514

- There is a new option '`makedep_side_effect`' for the compiler and `-MMD` for
  '`erlc`' that generates dependencies and continues to compile as normal.

  Own Id: OTP-14830

- When compiling modules with huge functions, the compiler would generate a lot
  of atoms for its internal, sometimes so many that the atom table would
  overflow. The compiler has been rewritten to generate far less internal atoms
  to avoid filling the atom table.

  Own Id: OTP-14968 Aux Id: ERL-563

- External funs with literal values for module, name, and arity (e.g.
  `erlang:abs/1`) are now treated as literals. That means more efficient code
  that produces less garbage on the heap.

  Own Id: OTP-15003

- Two new guards BIFs operating on maps have been added:
  [`map_get/2`](`map_get/2`) and [`is_map_key/2`](`is_map_key/2`). They do the
  same as `maps:get/2` and `maps:is_key/2`, respectively, except that they are
  allowed to be used in guards.

  Own Id: OTP-15037 Aux Id: PR-1784, PR-1802

- A call or apply of a literal external fun will be replaced with a direct call.

  Own Id: OTP-15044 Aux Id: ERL-614

- Part of EEP-44 has been implemented.

  There is a new predefined macro called `OTP_RELEASE` which is an integer
  indicating the OTP release number (its value is `21` in this release).

  There are new preprocessor directives `-if(Condition).` and
  `-elif(Condition).`. The `if/elif` supports the builtin function
  `defined(Symbol)`.

  Own Id: OTP-15087 Aux Id: PR-1810

## Compiler 7.1.5.2

### Fixed Bugs and Malfunctions

- Fix a regression in OTP-15204 that removed `.beam` file metadata that some
  external build tools relied on.

  Own Id: OTP-15292

## Compiler 7.1.5.1

### Fixed Bugs and Malfunctions

- Fixed an issue where files compiled with the `+deterministic` option differed
  if they were compiled in a different directory but were otherwise identical.

  Own Id: OTP-15204 Aux Id: ERL-679

## Compiler 7.1.5

### Fixed Bugs and Malfunctions

- The internal compiler pass (`beam_validator`) that validates the generated
  code has been strengthened.

  When compiling from BEAM assembly code, the `beam_type` optimizer pass could
  make the code unsafe. Corrected.

  Own Id: OTP-14863

- Corrected optimizations of integers matched out from binaries and used in bit
  operations.

  Own Id: OTP-14898

## Compiler 7.1.4

### Fixed Bugs and Malfunctions

- The '`deterministic`' option was not recognized when given in a `-compile()`
  attribute in the source code.

  Own Id: OTP-14773 Aux Id: ERL-498

## Compiler 7.1.3

### Fixed Bugs and Malfunctions

- The compiler could issue an incorrect internal consistency failure diagnostic
  for some complicated bit syntax matches.

  Own Id: OTP-14640 Aux Id: ERL-490

## Compiler 7.1.2

### Fixed Bugs and Malfunctions

- Fail labels on guard BIFs weren't taken into account during an optimization
  pass, and a bug in the validation pass sometimes prevented this from being
  noticed when a fault occurred.

  Own Id: OTP-14522 Aux Id: ERIERL-48

- When compiling from Core Erlang, an 'apply' with a nested apply in the
  function position would be treated as an invalid call. Corrected. (Thanks to
  Mikael Pettersson for reporting this bug.)

  Own Id: OTP-14526

- Fixed checking of binary matching in the `beam_validator` module to ensure
  that potential compiler bugs are found at compile-time instead as emulator
  crash at run-time.

  Own Id: OTP-14591

- There could be false warnings for `erlang:get_stacktrace/0` being used outside
  of a `try` block when using multiple `catch` clauses.

  Own Id: OTP-14600 Aux Id: ERL-478

### Improvements and New Features

- The Erlang code linter no longer checks that the functions mentioned in
  `nowarn_deprecated_function` options are declared in the module.

  Own Id: OTP-14378

## Compiler 7.1.1

### Fixed Bugs and Malfunctions

- Fail labels on guard BIFs weren't taken into account during an optimization
  pass, and a bug in the validation pass sometimes prevented this from being
  noticed when a fault occurred.

  Own Id: OTP-14522 Aux Id: ERIERL-48

## Compiler 7.1

### Fixed Bugs and Malfunctions

- For many releases, it has been legal to override a BIF with a local function
  having the same name. However, calling a local function with the same name as
  guard BIF as filter in a list comprehension was not allowed.

  Own Id: OTP-13690

- compile:forms/2 would not return the module name as documented when one of the
  options '`from_core`', '`from_asm`', or '`from_beam`' was given. Also, the
  compiler would crash if one of those options was combined with '`native`'.

  Own Id: OTP-14408 Aux Id: ERL-417

### Improvements and New Features

- Optimized test for tuples with an atom as first element.

  Own Id: OTP-12148

- Compilation of modules with huge literal binary strings is now much faster.

  Own Id: OTP-13794

- Replaced usage of deprecated symbolic [`time unit`](`t:erlang:time_unit/0`)
  representations.

  Own Id: OTP-13831 Aux Id: OTP-13735

- The undocumented and unsupported module `sys_pre_expand` has been removed. As
  a partial replacement for the functionality, there is a new function
  `erl_internal:add_predefined_functions/1` and `erl_expand_records` will now
  add a module prefix to calls to BIFs and imported functions.

  Own Id: OTP-13856

- The internal compiler passes now start all generated variables with "@" to
  avoid any conflicts with variables in languages such as Elixir or LFE.

  Own Id: OTP-13924

- The function `fmod/2` has been added to the `math` module.

  Own Id: OTP-14000

- Code generation for complicated guards have been improved.

  Own Id: OTP-14042

- The compiler has new warnings for repeated identical map keys.

  A map expression such as,

  `#{'a' => 1, 'b' => 2, 'a' => 3}.`

  will produce a warning for the repeated key 'a'.

  Own Id: OTP-14058

- By default, there will now be a warning when `export_all` is used. The warning
  can be disabled using `nowarn_export_all`.

  Own Id: OTP-14071

- Optimize maps pattern matching by only examining the common keys in each
  clause first instead of all keys. This will reduce the number of lookups of
  each key in maps pattern matching.

  Own Id: OTP-14072

- There is a new '`deterministic`' option to omit '`source`' and '`options`'
  tuples in the BEAM file.

  Own Id: OTP-14087

- Analyzing modules with binary construction with huge strings is now much
  faster. The compiler also compiles such modules slightly faster.

  Own Id: OTP-14125 Aux Id: ERL-308

- Atoms may now contain arbitrary Unicode characters.

  Own Id: OTP-14178

- `compile:file/2` now accepts the option `extra_chunks` to include extra chunks
  in the BEAM file.

  Own Id: OTP-14221

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

- In a future release, `erlang:get_stacktrace/0` will probably only work when
  called from within a '`try`' expression (otherwise it will return `[]`.

  To help prepare for that change, the compiler will now by default warn if
  '`get_stacktrace/0`' is used in a way that will not work in the future. Note
  that the warning will not be issued if '`get_stacktrace/0`' is used in a
  function that uses neither '`catch`' nor '`try`' (because that could be a
  legal use if the function is called from within a '`try`'.

  Own Id: OTP-14401

## Compiler 7.0.4.1

### Fixed Bugs and Malfunctions

- Fail labels on guard BIFs weren't taken into account during an optimization
  pass, and a bug in the validation pass sometimes prevented this from being
  noticed when a fault occurred.

  Own Id: OTP-14522 Aux Id: ERIERL-48

## Compiler 7.0.4

### Fixed Bugs and Malfunctions

- Minor internal changes. A typo in the documentation was also fixed.

  Own Id: OTP-14240

## Compiler 7.0.3

### Fixed Bugs and Malfunctions

- Fixed a compiler crash when maps were matched.

  Own Id: OTP-13931 Aux Id: ERL-266

- Fixed a compiler crash having to with the delayed sub-creation optimization.
  (Thanks to Jose Valim for reporting this bug.)

  Own Id: OTP-13947 Aux Id: ERL-268

- The compiler option `inline_list_funcs` accidentally turned off some other
  optimizations.

  Own Id: OTP-13985

- The compiler could sometimes generate spurious warnings when inlining was
  enabled.

  Own Id: OTP-14040 Aux Id: ERL-301

## Compiler 7.0.2

### Fixed Bugs and Malfunctions

- If the compiler fails to write the BEAM file, it will now report the reason of
  the error for the write operation.

  Own Id: OTP-13701

- Fixed an internal compiler error. (Thanks to Svilen Ivanov for reporting this
  bug.)

  Own Id: OTP-13780 Aux Id: ERL-202

- The compiler could crash when trying to compile a complicated expression with
  multiple catches all on one line . (Thanks to Thomas Arts for reporting this
  bug.)

  Own Id: OTP-13804 Aux Id: ERL-209

- Eliminated a few internal compiler failures.

  Own Id: OTP-13863

## Compiler 7.0.1

### Fixed Bugs and Malfunctions

- A literal binary matching regression was introduced in 19.0 where a match
  could fail to resolve to the right clause. This has now been fixed.

  Own Id: OTP-13738

## Compiler 7.0

### Fixed Bugs and Malfunctions

- `compile:forms/1,2` would crash when used in a working directory that had been
  deleted by another process.

  Own Id: OTP-13430 Aux Id: ERL-113

- Dialyzer no longer crashes when there is an invalid function call such as
  `42(7)` in a module being analyzed. The compiler will now warn for invalid
  function calls such as `X = 42, x(7)`.

  Own Id: OTP-13552 Aux Id: ERL-138

### Improvements and New Features

- Optimization of tuple matching has been slightly improved.

  Own Id: OTP-12951

- Five deprecated and undocumented functions in the module `core_lib` have been
  removed. The functions are: `get_anno/{1,2}`, `is_literal/1`,
  `is_literal_list/1`, and `literal_value`. Use the appropriate functions in the
  `cerl` module instead.

  Own Id: OTP-12979

- The pre-processor can now expand the ?FUNCTION_NAME and ?FUNCTION_ARITY
  macros.

  Own Id: OTP-13059

- The function mapfold/4 has been added to the `cerl_trees` module.

  Own Id: OTP-13280

- Bitstring comprehensions have been generalized to allow arbitrary expressions
  in the construction part.

  Own Id: OTP-13289

- The compiler will now produce warnings for binary patterns that will never
  match (example: `<<-1/unsigned>> = Bin`).

  Own Id: OTP-13374 Aux Id: ERL-44

- The compiler will no longer put the compilation date and time into BEAM files.
  That means that two BEAM files compiled on the same computer from the same
  source code and compilation options will be identical.

  Note: If you want to find out whether a BEAM file on disk is different from
  the loaded code, compared the MD5 value obtained from `Mod:module_info(md5)`
  with the MD5 value obtained from `beam_lib:md5(BeamFileForMod)`

  .

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13504

- The function `compile:env_compiler_options/0` has been added to allow tools to
  pick up the same default compiler options as the compiler itself.

  Own Id: OTP-13654

## Compiler 6.0.3.1

### Fixed Bugs and Malfunctions

- Fail labels on guard BIFs weren't taken into account during an optimization
  pass, and a bug in the validation pass sometimes prevented this from being
  noticed when a fault occurred.

  Own Id: OTP-14522 Aux Id: ERIERL-48

## Compiler 6.0.3

### Fixed Bugs and Malfunctions

- An complicated guard expression in a function call could crash the compiler.
  (Thanks to Thomas Arts for reporting this bug.)

  Own Id: OTP-13208

- Constructing a map in a guard in a catch could crash the compiler. (Thanks to
  Thomas Arts for reporting this bug.)

  Own Id: OTP-13223

- Updating a fun as if it were a map would cause the compiler to crash. (Thanks
  to Thomas Arts for reporting this bug.)

  Own Id: OTP-13231

- Fix pretty printing of Core Maps

  Literal maps could cause Dialyzer to crash when pretty printing the results.

  Own Id: OTP-13238

- A complex combination of bit syntax matching operations would cause an
  internal consistency check failure during compilation. (Thanks to Jose Valim
  for reporting this bug.)

  Own Id: OTP-13309

## Compiler 6.0.2

### Fixed Bugs and Malfunctions

- Fix cerl_trees:label/2 bug with map K/V swap

  Own Id: OTP-13091

- Warnings produced when the '`bin_opt_info`' option was given could sometimes
  lack filenames and line numbers. (Thanks to José Valim for reporting this
  bug.)

  Own Id: OTP-13113

## Compiler 6.0.1

### Fixed Bugs and Malfunctions

- Fix `get_map_elements` register corruption

  Instruction `get_map_elements` might destroy target registers when the
  fail-label is taken. Only seen for patterns with two, and only two, target
  registers. Specifically if we copy one register and then jump.

  Own Id: OTP-12967

## Compiler 6.0

### Fixed Bugs and Malfunctions

- The compiler optimizes away building of terms that are never actually used. As
  a result, the compiler in OTP 18 may produce more warnings for terms that are
  built but not used than the compiler in OTP 17.

  Own Id: OTP-12453

- Using a map could incorrectly suppress warnings for unused variables.

  Own Id: OTP-12515

- The compiler now properly reports unknown parse transforms. That is, `undef`
  exceptions coming from the parse transform itself is reported differently from
  the absence of the parse transform.

  Own Id: OTP-12723

- Allow for 'creation of sub binary delayed' optimization if maps instructions
  are in a clause.

  Own Id: OTP-12758

### Improvements and New Features

- The `cerl` and `cerl_trees` modules in the Compiler application are now
  documented.

  Own Id: OTP-11978

- The deprecated '`asm`' option has been removed.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12100

- Support variables as Map keys in expressions and patterns

  Erlang will accept any expression as keys in Map expressions and it will
  accept literals or bound variables as keys in Map patterns.

  Own Id: OTP-12218

- Infer Map type information in beam_type compiler optimization pass.

  Own Id: OTP-12253

- Compiler optimizations have been improved.

  Own Id: OTP-12393

- Five undocumented functions in the module `core_lib` have been deprecated and
  will be removed in the next major release. The functions are:
  `get_anno/{1,2}`, `is_literal/1`, `is_literal_list/1`, and `literal_value`.
  Use the appropriate functions in the `cerl` module instead.

  Own Id: OTP-12497

- Change some internal data structures to Maps in order to speed up compilation
  time. Measured speed up is around 10%-15%.

  Own Id: OTP-12774

- Fix beam_bool pass for Maps instruction get_map_elements

  Before beam_split the get_map_elements instruction is still in blocks and the
  helper function in beam_jump did not reflect this.

  Own Id: OTP-12844 Aux Id: 17

## Compiler 5.0.4

### Fixed Bugs and Malfunctions

- Matching out a map from a record and then updating the record could cause a
  'badarg' exception at run-time. (Thanks to Dmitry Aleksandrov for reporting
  this bug.)

  Own Id: OTP-12402

- The compiler would crash when compiling some complex, nonsensical guards such
  as:

  ... `when {{X}}, -X`...

  Own Id: OTP-12410

- In rare circumstances, using binary pattern in the value part of a map pattern
  would cause the compiler to crash.

  Own Id: OTP-12414

- Case expressions where a map was wrapped in a tuple or list such as:

  `case {a,Map} of`  
  `{a,#{k:=_}}=Tuple -> Tuple`  
  `end.`

  would be unsafely "optimized" to either cause an exception at run-time or
  would return an empty map.

  Own Id: OTP-12451

- When a variable was compared to a literal map using the '`==`' operator, the
  compiler would change the operator to '`=:=`' since it is more efficient.
  However, this optimization is not safe if the map literal has numeric keys or
  values. The compiler will now only do the optimization if all keys and values
  are non-numeric.

  Own Id: OTP-12456

## Compiler 5.0.3

### Fixed Bugs and Malfunctions

- Named funs with the same name and arity could get mixed up with each other.

  Own Id: OTP-12262

- Coalesce map keys in dialyzer mode

  This fixes a regression introduced in commit
  805f9c89fc01220bc1bb0f27e1b68fd4eca688ba The problem occurred with compounded
  map keys compiled with dialyzer option turned on, '+dialyzer'.

  Reported by: Ivan Uemlianin

  Own Id: OTP-12347

## Compiler 5.0.2

### Fixed Bugs and Malfunctions

- Corrected a bug with incorrect code generation when inlining was turned on.

  Own Id: OTP-12132

## Compiler 5.0.1

### Fixed Bugs and Malfunctions

- A Dialyzer crash involving analysis of Map types has now been fixed.

  Own Id: OTP-11947

- The compiler would fail to compile a file with a latin-1 character in the
  false branch of an `-ifdef` or `-indef`.

  Own Id: OTP-11987

## Compiler 5.0

### Fixed Bugs and Malfunctions

- Line numbers would not be correct when a binary construction such as
  '`<<Bin/binary,...>>`' fails. (Thanks to Stanislav Seletskiy for reporting
  this bug.)

  Own Id: OTP-11572

- The compiler now properly annotates the code in value in the '`after`' clause
  for a '`try`' so that Dialyzer no longer generates a false warning for an
  unmatched return.

  Own Id: OTP-11580

- Some case statements where no clause would match could cause an internal error
  in the compiler. (Thanks to Erik Soe Sorensen for reporting this bug.)

  Own Id: OTP-11610

- With `--Wunmatched_returns`, dialyzer will no longer warn when the value of a
  list comprehension is ignored, provided that the each value in the list would
  be an atomic value (such as integer or atoms, as opposed to tuples and lists).
  Example: ignoring '`[io:format(...) || ...]`' will not cause a warning, while
  ignoring '`[file:close(Fd) || ...]`' will.

  Own Id: OTP-11626

- Matching out a binary and applying the binary as if it were a fun would crash
  the run-time system. (Thanks to Loïc Hoguin.)

  Own Id: OTP-11672

- Some local implementations of removing the last element from a list are
  replaced by `lists:droplast/1`. Note that this requires at least `stdlib-2.0`,
  which is the stdlib version delivered in OTP 17.0. (Thanks to Hans Svensson)

  Own Id: OTP-11678

- Allow all auto imports to be suppressed at once. Introducing the
  no_auto_import attribute: -compile(no_auto_import). Useful for code generation
  tools that always use the qualified function names and want to avoid the auto
  imported functions clashing with local ones. (Thanks to José Valim.)

  Own Id: OTP-11682

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

- Adapt 'asm' deprecation message to new version scheme. (Thanks to Tuncer Ayaz)

  Own Id: OTP-11751

- A number of compiler errors where unusual or nonsensical code would crash the
  compiler have been reported by Ulf Norell and corrected by Anthony Ramine.

  Own Id: OTP-11770

### Improvements and New Features

- Compilation times for modules with a huge number for record accesses using the
  dot operator has been improved.

  Own Id: OTP-10652

- The compiler can generate somewhat better code by moving let expressions into
  sequences. (Thanks to Anthony Ramine.)

  Own Id: OTP-11056

- Forbid unsized fields in patterns of binary generators and simplified
  v3_core's translation of bit string generators. (Thanks to Anthony Ramine.)

  Own Id: OTP-11186

- Funs can now be a given a name. Thanks to to Richard O'Keefe for the idea
  (EEP37) and to Anthony Ramine for the implementation.

  Own Id: OTP-11537

- Using the `from_asm` option to produce a BEAM file starting from BEAM assembly
  code would often fail because early optimization passes would not understand
  instructions that later optimization passes would introduce. (Thanks to
  Anthony Ramine.)

  Own Id: OTP-11544

- The `.core` and `.S` extensions are now documented in the `erlc`
  documentation, and the '`from_core`' and '`from_asm`' options are now
  documented in the compiler documentation. (Thanks to Tuncer Ayaz.)

  Own Id: OTP-11547

- Optimization of case expressions that build tuples or lists have been
  improved.

  Own Id: OTP-11584

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

- Some function specs are corrected or moved and some edoc comments are
  corrected in order to allow use of edoc. (Thanks to Pierre Fenoll)

  Own Id: OTP-11702

- Thanks to Anthony Ramine for several improvements to the optimizations in the
  BEAM compiler and for cleaning up the code the code that transforms list and
  binary comprehensions to Core Erlang.

  Own Id: OTP-11720

- The default encoding for Erlang source files is now UTF-8. As a temporary
  measure to ease the transition from the old default of latin-1, if the
  compiler encounters byte sequences that are not valid UTF-8 sequences, the
  compiler will re-try the compilation in latin-1 mode. This workaround will be
  removed in a future release.

  Own Id: OTP-11791

## Compiler 4.9.4

### Fixed Bugs and Malfunctions

- Typo fix ambigous -> ambiguous. Thanks to Leo Correa.

  Own Id: OTP-11455

### Improvements and New Features

- Lift 'after' blocks to zeroary functions. Thanks to Anthony Ramine.

  Own Id: OTP-11267

## Compiler 4.9.3

### Fixed Bugs and Malfunctions

- Expressions such as `'B = is_integer(V), if B and B -> ok end'` would crash
  the compiler.

  Own Id: OTP-11240

- `compile:file2/2` with the option `report_errors` could return ErrorInfo
  tuples with only two elements, while the documentation says that the ErrorInfo
  tuple always has three elements. Also updated the documentation to add that
  the first element may be '`none`' if no line number is applicable.

  Own Id: OTP-11304 Aux Id: seq12412

### Improvements and New Features

- Fix matching of floating point middle-endian machines. Thanks to Johannes
  Weissl.

  Own Id: OTP-11201

- Restrict inlining of local fun references. Thanks to Anthony Ramine.

  Own Id: OTP-11211

- Silence a misleading warning with some comprehensions. Thanks to Anthony
  Ramine.

  Own Id: OTP-11212

- Forbid returning a match context in beam_validator. Thanks to Anthony Ramine.

  Own Id: OTP-11247

## Compiler 4.9.2

### Fixed Bugs and Malfunctions

- Compiling functions with complex boolean operations in guards could be very
  slow. (Thanks to Magnus Muller for reporting this issue.)

  Own Id: OTP-10939

- Certain guard expressions used in a receive statement could cause the compiler
  to crash.

  Own Id: OTP-11119 Aux Id: seq12342

### Improvements and New Features

- Fix optimization of some binary comprehensions. Thanks to Anthony Ramine.

  Own Id: OTP-11005

- Use a set to store ref registers in beam_receive. Thanks to Anthony Ramine.

  Own Id: OTP-11069

- Fix renaming of bs_put_string instructions. Thanks to Anthony Ramine.

  Own Id: OTP-11129

## Compiler 4.9.1

### Fixed Bugs and Malfunctions

- The compiler would crash attempting to compile expressions such as "element(2,
  not_tuple)".

  Own Id: OTP-10794

- Forbid multiple values in Core Erlang sequence arguments. Thanks to José Valim
  and Anthony Ramine.

  Own Id: OTP-10818

- An unsafe optimization would cause the compiler to crash with an internal
  error for certain complex code sequences.

  Own Id: OTP-10825 Aux Id: seq12247

### Improvements and New Features

- Integers in expression that will give a floating point result (such as
  "`X / 2`" will now be converted to floating point at compile-time. (Suggested
  by Richard O'Keefe.)

  Identical floating points constans in a module will now be coalesced to one
  entry in the constant pool.

  Own Id: OTP-10788

## Compiler 4.9

### Improvements and New Features

- The compiler optimizations have been polished, so that the code quality will
  be slightly better in some cases.

  Own Id: OTP-10193

- Support for Unicode has been implemented.

  Own Id: OTP-10302

- Where necessary a comment stating encoding has been added to Erlang files. The
  comment is meant to be removed in Erlang/OTP R17B when UTF-8 becomes the
  default encoding.

  Own Id: OTP-10630

- Fix some wrong warnings triggered by the option inline_list_funcs. Thanks to
  Anthony Ramine.

  Own Id: OTP-10690

- Forbid local fun variables in Core Erlang guards. Thanks to Anthony Ramine.

  Own Id: OTP-10706

- Binary syntax matches could cause an internal consistency error in in the
  compiler. (Thanks to Viktor Sovietov for reporting this bug.)

  Own Id: OTP-10724

## Compiler 4.8.2

### Fixed Bugs and Malfunctions

- Modules with very many functions would compile very slowly.

  Own Id: OTP-10123

- `compile:forms/2` will now use a \{source,SourceFilePath\} to set the source
  returned by `module_info(compile)` (Thanks to José Valim)

  Own Id: OTP-10150

- A process which had enabled trap_exit would receive EXIT messages after
  calling the compiler. (Thanks to Jeremy Heater.)

  Own Id: OTP-10171

- Fix messages ordering with column numbers

  Own Id: OTP-10183

- sys_pre_expand: Fix BASE never being set

  Commit a612e99fb5aaa934fe5a8591db0f083d7fa0b20a turned module attributes from
  2-tuples to 3-tuples but forgot to update get_base/1, breaking BASE for
  parametric modules.

  Own Id: OTP-10184

- The compiler will now issue a warning if literal tuple funs are used. For
  example, \{erlang,is_tuple\}(X) will now generate a warning.

  Own Id: OTP-10185

- The compiler will now warn for illegal sizes for segments in binary
  construction. For example, <<X:(2.5)>> will now cause the compiler to issue a
  warning.

  Own Id: OTP-10197

- Fix the erlc -MP flag

  Because of a copy-and-paste error in erlc.c, the -MP flag had the same effect
  as -MG. As a workaround, you had to pass +makedep_phony to enable the MP
  option. This patch makes -MP work as intended.

  Own Id: OTP-10211

## Compiler 4.8.1

### Fixed Bugs and Malfunctions

- In rare circumstance, the compiler could crash when compiling a case
  statement. (Thanks to Hakan Mattsson.)

  Own Id: OTP-9842

- Calling a guard test (such as is_list/1) from the top-level in a guard, would
  cause a compiler crash if there was a local definition with the same name.
  Corrected to reject the program with an error message.

  Own Id: OTP-9866

- Using [`get/1`](`get/1`) in a `try` block could in some cases cause an
  internal compiler error. (Thanks to Eric Merritt.)

  Own Id: OTP-9867

- An unexported on_load function would not get run if the module was compiled
  with the `inline` option. (Thanks to Yiannis Tsiouris.)

  Own Id: OTP-9910

- Fixed a discrepancy in compile_info

  The BEAM disassembler used the atom 'none' to signify the absence of a
  compile_info chunk in a .beam file. This clashed with the type declaration of
  the compile_info field of a #beam_file\{\} record as containing a list. Now []
  signifies the absence of this chunk. This simplifies the code and avoids a
  dialyzer warning.

  Own Id: OTP-9917

- Fix typo in \`compile' doc: unmatched parenthesis (Thanks to Ricardo Catalinas
  Jiménez)

  Own Id: OTP-9919

- In a `try`...`catch` statement that always returned `false`, the compiler
  would remove calls to BIFs that could not cause an exception (such as
  [`put/2`](`put/2`)). Example of such code:
  `try put(K, V), false catch _:_ -> false end.`

  Own Id: OTP-9982

## Compiler 4.8

### Fixed Bugs and Malfunctions

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

- The calculation of the 'uniq' value for a fun (see `erlang:fun_info/1`) was
  too weak and has been strengthened. It used to be based on the only the code
  for the fun body, but it is now based on the MD5 of the BEAM code for the
  module.

  Own Id: OTP-9667

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

- `filename:find_src/1,2` will now work on stripped BEAM files (reported by Per
  Hedeland). The HiPE compiler will also work on stripped BEAM files. The BEAM
  compiler will no longer include compilation options given in the source code
  itself in `M:module_info(compile)` (because those options will be applied
  anyway if the module is re-compiled).

  Own Id: OTP-9752

- Inlining binary matching could cause an internal compiler error. (Thanks to
  Rene Kijewski for reporting this bug.)

  Own Id: OTP-9770

## Compiler 4.7.5

### Fixed Bugs and Malfunctions

- Compiler options given in the source code using a `-compile()` attribute used
  to be included twice in `Mod:module_info(compile)`. They are now only included
  once at the beginning of the list of options.

  Own Id: OTP-9534

- beam_disasm: Handle stripped BEAM files

  beam_disasm:file/1 would crash if asked to disassemble a stripped BEAM file
  without an "Attr" chunk. (Thanks to Haitao Li)

  Own Id: OTP-9571

## Compiler 4.7.4

### Fixed Bugs and Malfunctions

- If a variable is matched out in binary matching and used as the size for a
  binary element, it would seem to be unbound if used in a subsequent match
  operation. (Thanks to Bernard Duggan.)

  Own Id: OTP-9134

- Eliminate incorrect warning in `sys_core_fold`

  Own Id: OTP-9152

## Compiler 4.7.3

### Fixed Bugs and Malfunctions

- The `-export_type()` directive is no longer included among the attributes.

  Own Id: OTP-8998

### Improvements and New Features

- The maximum number of allowed arguments for an Erlang function has been
  lowered from 256 to 255, so that the number of arguments can now fit in a
  byte.

  Own Id: OTP-9049

- Dependency generation for Makefiles has been added to the compiler and erlc.
  See the manual pages for `compile` and `erlc`. (Thanks to Jean-Sebastien
  Pedron.)

  Own Id: OTP-9065

## Compiler 4.7.2

### Fixed Bugs and Malfunctions

- Two compiler bugs (that would cause the compiler to terminate) reported by
  Christopher Williams have been fixed.

  Own Id: OTP-8949

### Improvements and New Features

- The compiler would translate binary comprehensions containing tail segments in
  a way that would would confuse Dialyzer. For instance:

  `[42 || <<_:8/integer, _/bits>> <= Bits]`

  would produce a Dialyzer warning.

  Own Id: OTP-8864

- Code such as `foo(A) -> <<A:0>>` would crash the compiler.

  Own Id: OTP-8865

- The compiler could fail with an internal error when variables were exported
  from a receive block but the return value of the receive block were not used.
  (Thanks to Jim Engquist for reporting this error.)

  Own Id: OTP-8888

## Compiler 4.7.1

### Improvements and New Features

- Eliminated warnings for auto-imported BIF clashes.

  Own Id: OTP-8840

## Compiler 4.7

### Fixed Bugs and Malfunctions

- Several problems in the inliner have been fixed.

  Own Id: OTP-8552

### Improvements and New Features

- The module binary from EEP31 (and EEP9) is implemented.

  Own Id: OTP-8217

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

- The undocumented, unsupported, and deprecated function `lists:flat_length/1`
  has been removed.

  Own Id: OTP-8584

- Nested records can now be accessed without parenthesis. See the Reference
  Manual for examples. (Thanks to YAMASHINA Hio and Tuncer Ayaz.)

  Own Id: OTP-8597

- It is now possible to suppress the warning in code such as
  "`list_to_integer(S), ok`" by assigning the ignored value "_" like this: "`_ =
  list_to_integer(S), ok`".

  Own Id: OTP-8602

- `receive` statements that can only read out a newly created reference are now
  specially optimized so that it will execute in constant time regardless of the
  number of messages in the receive queue for the process. That optimization
  will benefit calls to `gen_server:call()`. (See `gen:do_call/4` for an example
  of a receive statement that will be optimized.)

  Own Id: OTP-8623

- The compiler optimizes record operations better.

  Own Id: OTP-8668

## Compiler 4.6.5

### Fixed Bugs and Malfunctions

- Using complex boolean expressions in ifs could cause the compiler to either
  crash or terminate with an internal error. (Thanks to Simon Cornish.)

  Own Id: OTP-8338

- Bit string comprehensions can now be used in parameterized modules. (Thanks to
  Jebu Ittiachen.)

  Own Id: OTP-8447

### Improvements and New Features

- The expected return value for an on_load function has been changed. (See the
  section about code loading in the Reference manual.)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8339

- In rare circumstances when using garbaging collecting guard BIFs, the
  validation pass (beam_validator) would signal that the code was unsafe, when
  it in fact was correct. (Thanks to Kiran Khaladkar.)

  Own Id: OTP-8378

- The `-Werror` option for `erlc` and the compiler option `warnings_as_errors`
  will cause warnings to be treated as errors. (Thanks to Christopher Faulet.)

  Own Id: OTP-8382

- Macros overloading has been implemented. (Thanks to Christopher Faulet.)

  Own Id: OTP-8388

## Compiler 4.6.4

### Fixed Bugs and Malfunctions

- The compiler's 'E' option now works with modules with types and
  specifications.

  Own Id: OTP-8238 Aux Id: OTP-8150

- Certain uses of binary matching in a `begin`-`end` in a list comprehension
  could cause the compiler to crash or generate incorrect code.

  Own Id: OTP-8271

### Improvements and New Features

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8201

- The compiler could crash if invalid calls to is_record/2 was used in (for
  example) a list comprehension. (Thanks to Tobias Lindahl.)

  Own Id: OTP-8269

- The -on_load() directive can be used to run a function when a module is
  loaded. It is documented in the section about code loading in the Reference
  Manual.

  Own Id: OTP-8295

## Compiler 4.6.3

### Improvements and New Features

- Corrected liveness optimization to eliminate a compiler crash that could occur
  when compiling bit syntax construction code. (Thanks to Mikage Sawatari.)

  Calling BIFs such as [`length/1`](`length/1`) in guard context in a try/catch
  block could cause a compiler crash. (Thanks to Paul Fisher.)

  Using filter expressions containing `andalso` or `orelse` in a list
  comprehension could cause a compiler crash. (Thanks to Martin Engström.)

  Own Id: OTP-8054

- A guard with nested 'not' operators could cause the compiler to crash. (Thanks
  to Tuncer Ayaz.)

  Own Id: OTP-8131

## Compiler 4.6.2

### Fixed Bugs and Malfunctions

- The compiler would crash while compiling certain complex function bodies
  containing `receive after` due to a bug in the jump optimizer (a label that
  had only had backward references could still be removed). (Thanks to Vincent
  de Phily.)

  Own Id: OTP-7980

## Compiler 4.6.1

### Fixed Bugs and Malfunctions

- Miscellaneous minor bugs fixed.

  Own Id: OTP-7937

### Improvements and New Features

- There will be more efficient code if there is a clause that matches the empty
  binary and no other clauses that matches non-empty binaries.

  Own Id: OTP-7924

- There is new option to allow a module to have a module name other than the
  filename. Do not use it unless you know what you are doing.

  Own Id: OTP-7927

## Compiler 4.6.0.1

### Fixed Bugs and Malfunctions

- Using `andalso`/`orelse` or record access in a `try`...`catch` could cause a
  compiler crash.

  Some large and complex functions could require extremely long compilation
  times (hours or days).

  Own Id: OTP-7905

## Compiler 4.6

### Fixed Bugs and Malfunctions

- For some complex guards which used `andalso`/`orelse`, the compiler would
  crash. (Thanks to Hunter Morris.)

  Own Id: OTP-7679

- Code that (incorrectly) used the the value of nested applications of
  [`setelement/3`](`setelement/3`) in bit syntax construction could crash the
  compiler.

  Own Id: OTP-7690

- Modules containing huge integers (consisting of several hundreds of thousands
  of digits or more) could be slow to compile. This problem has been corrected.

  Own Id: OTP-7707 Aux Id: seq11129

- If the generator in a list comprehension is given a non-list term, there will
  now be `function_clause` exception instead of a `case_clause` exception (as it
  was in all releases before R12B).

  Own Id: OTP-7844

### Improvements and New Features

- The compiler could crash if the size for a binary segment in matching was a
  complex literal such as binary or tuple.

  Own Id: OTP-7650

- The compiler generates more compact and faster code for matching of complex
  constants (such as constant lists and tuples).

  Own Id: OTP-7655

- The undocumented, unsupported, and deprecated guard BIF `is_constant/1` has
  been removed.

  \*** INCOMPATIBILITY with R12B \***

  Own Id: OTP-7673

- The compiler generates better code for many guard expressions, and especially
  for guards that use `andalso`/`orelse` or record fields.

  (In technical terms, `andalso`/`orelse` in a guard would case the creation of
  a stack frame and saving of all x registers that could potentially be alive
  after the guard and restoring all x registers before leaving the guard. For
  certain guards, far too many x registers were saved and subsequently restored.
  In this version of the compiler, no stack frame is created and no x registers
  are saved and restored.)

  Own Id: OTP-7718

- The default size for the resulting binary created by a binary comprehension
  was 64Kb in R12B (it would grow if needed). This was often far too much. In
  this release, the default is changed to 256 bytes. Furthermore, for most
  binary comprehensions without filters, the exact size of the resulting binary
  can be calculated beforehand and the compiler now generates code that does
  that calculation.

  Own Id: OTP-7737

- The short-circuit operators `andalso` and `orelse` no longer guarantees that
  their second argument is either `true` or `false`. As a consequence,
  `andalso`/`orelse` are now tail-recursive.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7748

- The compiler will refuse to a compile file where the module name in the file
  differs from the output file name.

  When compiling using `erlc`, the current working directory will no be included
  in the code path (unless explicitly added using "-pa .").

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7793

- There will no longer be any warnings for list comprehensions without
  generators, as such list comprehension have turned out to be useful.

  Own Id: OTP-7846

- Warnings for obsolete guard tests are now turned on. (That is, writing
  [`list(L)`](`t:list/1`) in a guard instead of [`is_list(L)`](`is_list/1`) will
  generate a warning.)

  The warnings can be turned off using the `nowarn_obsolete_guard` option.

  Own Id: OTP-7850

- The copyright notices have been updated.

  Own Id: OTP-7851

- If a module contains an exported function with the same name as an
  auto-imported BIF (such as [`length/1`](`length/1`)), any calls to the BIF
  must have an explicit `erlang:` prefix, or there will be a compilation error
  (such calls would only generate a warning in previous releases).

  (The reason for the change is to avoid breaking code in a future major
  release, R14 or R15, in which we plan to make calls without a module prefix
  always call the local function in the same module even if there is an
  auto-imported BIF with the same name.)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7873

## Compiler 4.5.5

### Fixed Bugs and Malfunctions

- Matching on a zero-width segment in the bit syntax would crash the compiler.
  (Thanks to Will.)

  Own Id: OTP-7591

### Improvements and New Features

- In bit syntax expressions which started with a binary segment, and was
  followed by at least two segments of variable size, too little space could be
  allocated for the binary, leading to memory corruption.

  Own Id: OTP-7556

- In user-defined attributes, `Name/Arity` is now allowed and will be translated
  to `{Name,Arity}`. (An implementation of EEP-24 by Richard O'Keefe.)

  The `module_info/{0,1}` functions automatically inserted into each compiled
  modules are now documented in the Modules section in the Reference Manual.

  Own Id: OTP-7586

## Compiler 4.5.4

### Improvements and New Features

- Certain complex bit syntax matching operations matching out binaries and
  having several clauses could give incorrect results (the matched out binaries
  were too short). (Thanks to Christian von Roques for bug report and
  correction.)

  Own Id: OTP-7498

## Compiler 4.5.3

### Improvements and New Features

- New option `warn_export_all` to warn for a module using `export_all`. (Thanks
  to Richard Carlsson.)

  Own Id: OTP-7392

## Compiler 4.5.2.1

### Fixed Bugs and Malfunctions

- In rare circumstances, the length/1 BIF (and a few other guard BIFs) would
  seem to return an incorrect value (of any type).

  Own Id: OTP-7345 Aux Id: seq10962

## Compiler 4.5.2

### Fixed Bugs and Malfunctions

- A bug in the old inliner has been fixed. Some undocumented functionality has
  been removed.

  Own Id: OTP-7223

- Matching several binary patterns in parallel using the '=' operator is not
  allowed (an implementation limitation), but the compiler did not reject all
  such attempts (depending on the patterns, the generated code might or might
  not work correctly). Now the compiler rejects all binary patterns joined by
  '='.

  Own Id: OTP-7227

- Complex combinations of record operations and binary matching could cause the
  compiler to crash. (Thanks to Vladimir Klebansky.)

  Own Id: OTP-7233

- In rare circumstances, mixing binary matching clauses with clauses matching
  other data types, the compiler could crash.

  Own Id: OTP-7240 Aux Id: seq10916

## Compiler 4.5.1.1

### Fixed Bugs and Malfunctions

- Corrected a compiler bug that could cause a complex binary matching operation
  to fail when it shouldn't. (Thanks to Tomas Stejskal.)

  Own Id: OTP-7188

- In unusual circumstances, the environment for a fun could bind wrong values.

  Own Id: OTP-7202 Aux Id: seq10887

- Long sequences of list comprehensions without generators joined by the '++'
  operator would cause a code expansion explosion, which could cause the
  compiler to run out of memory. To resolve this problem, in
  '`[...||...]++Expr`', `Expr` is now evaluated before the list comprehension.
  This change _is_ backwards compatible (see the following note about evaluation
  order if you have doubts).

  Note about evaluation order: The Reference manual says that subexpressions are
  evaluated _in any order_ before the expression itself. Therefore, in an
  expression such as '`LeftExpr++RightExpr`', you should not depend on
  `LeftExpr` being evaluated before `RightExpr` or vice versa. The evaluation
  order is only important if the expressions contains and/or depends on
  operations with side-effects, such as message passing or ETS operations.

  Own Id: OTP-7206

## Compiler 4.5.1

### Fixed Bugs and Malfunctions

- A match expression inside a function call could cause a false "a term is
  constructed but never used" warning.

  Own Id: OTP-7018 Aux Id: seq10824

- The compiler could crash if a binary tail was matched out, and then used in a
  binary append operation. (Thanks to Oleg Avdeev.)

  Similarly, the compiler could crash if a binary tail was matched out, and then
  used (incorrectly) in binary construction in an integer field. (Thanks to
  Fredrik Svahn.) Or was incorrectly used in a float field. Or was used in a
  binary field with a given length. (Thanks to Chih - Wei Yu.)

  Own Id: OTP-7022

- Matching an empty binary in a record and then using the same record again
  could cause a compiler crash. (Thanks to Fredrik Thulin.)

  Own Id: OTP-7029

- In rare circumstances, constants containing floating points and integers could
  be confused. Example:

  `f(a) -> [1]; f(b) -> [1.0].`

  Both `f(a)` and `f(b)` would return `[1]`.

  Own Id: OTP-7073

- Some bit syntax code such as

  `matching d(_,<$lt;$gt;$gt;) -> one; d(0,<$lt;D$gt;$gt;) ->two.`

  could crash the compiler. (Thanks to Simon Cornish.)

  Own Id: OTP-7094

- In unusual circumstances, a call to a fun could fail due to an unsafe
  optimization. (Thanks to Simon Cornish.)

  Own Id: OTP-7102

- Bit syntax matching with a guard containing two or more uses of andalso/orelse
  could cause the compiler to crash. (Thanks to Mateusz Berezecki.)

  Own Id: OTP-7113

- This was only a problem if you generated or wrote your own Core Erlang code:
  The Core Erlang optimizer code could move nested calls such as
  `erlang:'$lt;'(erlang:length(L), 2)` as case expression into a guard, which
  would change the semantics. (Thanks to Robert Virding.)

  Own Id: OTP-7117

### Improvements and New Features

- The compiler could generate suboptimal code for record updates if the record
  update code consisted of multiple source code lines.

  Own Id: OTP-7101

## Compiler 4.5

### Fixed Bugs and Malfunctions

- The compiler used to allow that a binary field without size could be used in
  other positions than at the end in bit syntax pattern. For instance,
  `<<B/binary,EmptyBinary/binary>> = Bin` used to compile, but now the
  compilation will fail with an an error message.

  Also, it is now longer permitted to give a literal string in a binary pattern
  a type or a size; for instance, `<<"abc"/binary>> = Bin` will no longer
  compile. (In previous releases, there would always be a `badmatch` exception
  at run-time.)

  Own Id: OTP-6885

### Improvements and New Features

- Bitstrings (bit-level) binaries and binary comprehensions are now part of the
  language. See the Reference Manual.

  Own Id: OTP-6558

- The '`compressed`' option for the compiler has been documented.

  Own Id: OTP-6801

- If the value of a list comprehension is not used, such as in
  '`[do_something(X) || X <- List], ok`', a result list will no longer be built.
  For more details, see the Efficiency Guide.

  If the value of an expression is not used, and the expression has no side
  effects except for possibly throwing an exception, a warning will be
  generated. Examples: '`self(),ok`' and '`{error,Reason},ok`'.

  Own Id: OTP-6824

- Three new functions have been added to the `compile` module: `noenv_file/2`,
  `noenv_forms/2`, and `noenv_output_generated/1`.

  Own Id: OTP-6829

- Many bit syntax operations, both construction and matching, are faster. For
  further information, see the Efficiency Guide.

  Own Id: OTP-6838

- Literal lists, tuples, and binaries are no longer constructed at run-time as
  they used to be, but are stored in a per-module constant pool. Literals that
  are used more than once are stored only once.

  This is not a change to the language, only in the details of its
  implementation. Therefore, the implications of this change is described in the
  Efficiency Guide.

  Example 1: In the expression
  [`element(BitNum-1, {1,2,4,8,16,32,64,128})`](`element/2`), the tuple used to
  be constructed every time the expression was executed, which could be
  detrimental to performance in two ways if the expression was executed in a
  loop: the time to build the tuple itself and the time spent in garbage
  collections because the heap filled up with garbage faster.

  Example 2: Literal strings, such as `"abc"`, used to be stored in the compiled
  code compactly as a byte string and expanded to a list at run-time. Now all
  strings will be stored expanded to lists (such as `[$a,$b,$c]`) in the
  constant pool. That means that the string will be faster to use at run-time,
  but that it will require more space even when not used. If space is an issue,
  you might want to use binary literals (that is, `<<"abc"<<`) instead of string
  literals for infrequently used long strings (such as error messages).

  Own Id: OTP-6850

- Recursive calls now usually consume less stack than in R11B. See the
  Efficiency Guide.

  Own Id: OTP-6862 Aux Id: seq10746

- Two new guard BIFs have been introduced as a recommended replacement for
  [`size/1`](`size/1`). (The [`size/1`](`size/1`) BIF will be removed no earlier
  than in R14B.) The BIFs are [`tuple_size/1`](`tuple_size/1`) to calculate the
  size of a tuple and [`byte_size/1`](`byte_size/1`) to calculate the number of
  bytes needed for the contents of the binary or bitstring (rounded up to the
  nearest number of bytes if necessary).

  There is also a new [`bit_size/1`](`bit_size/1`) BIF that returns the exact
  number of bits that a binary or bitstring contains.

  Own Id: OTP-6902

- The two internal functions `erl_bifs:is_bif/3` and `erl_bifs:is_guard/3` have
  been removed. They were unsupported, undocumented, and unmaintained.

  Own Id: OTP-6966

## Compiler 4.4.5

### Fixed Bugs and Malfunctions

- The compiler would crash if you tried to combine to non-list literals with
  '`++`' (for instance, `an_atom++"string"`).

  Own Id: OTP-6630 Aux Id: seq10635

### Improvements and New Features

- Minor Makefile changes.

  Own Id: OTP-6689

## Compiler 4.4.4

### Fixed Bugs and Malfunctions

- Incorrect code could be generated for bit syntax matching if the old inliner
  was used with aggressive settings.

  Own Id: OTP-6461

## Compiler 4.4.3

### Fixed Bugs and Malfunctions

- The R10B compiler could generate unsafe `bs_save/bs_restore` instructions that
  could cause memory corruption. (The R11B compiler does not have that problem.)
  The erlang emulator will now refuse to load R10B-compiled modules that contain
  such unsafe `bs_save/bs_restore` instructions. In addition, the beam_validator
  module in the compiler will also reject such instructions (in case it is used
  to validate R10B code). (Thanks to Matthew Reilly.)

  Own Id: OTP-6386

### Improvements and New Features

- Directives for parse transforms that have been run are now removed from the
  abstract code stored when the debug_info option is given, to prevent the parse
  transforms to be run again.

  Own Id: OTP-5344

- Minor improvements in code generation for some guards expression involving
  boolean expressions.

  Own Id: OTP-6347

## Compiler 4.4.2.1

### Fixed Bugs and Malfunctions

- The compiler could generate incorrect code for bit syntax matching consisting
  of several clauses.

  Own Id: OTP-6392 Aux Id: seq10539

## Compiler 4.4.2

### Fixed Bugs and Malfunctions

- Defining a fun itself containing a fun in an `after` block of a `try` would
  cause the compiler to crash or generate incorrect code. (Thanks to Tim Rath.)

  Shorter compilation times for modules containing with an extreme number of
  functions (10000 functions or more).

  (The compiled could generate deprecated instructions for certain bit syntax
  matching operations.)

  Own Id: OTP-6212 Aux Id: seq10446

- Fixed several bugs that would cause warnings to be shown without file name and
  line number.

  Own Id: OTP-6260 Aux Id: seq10461

### Improvements and New Features

- The `strict_record_tests` option is now default; that is, reading a field from
  a record using the `Record#record_tag.field` syntax will fail if `Record` is
  not a record of the correct type.

  If necessary, the record tests can be turned off by giving the
  `no_strict_record_tests` option. To avoid editing Makefiles, the environment
  variable `ERL_COMPILER_OPTIONS` can be set to "`no_strict_record_tests`".

  The `no_strict_record_tests` option will probably be removed in the R12B
  release.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6294

## Compiler 4.4.1

### Fixed Bugs and Malfunctions

- The compiler used to crash if a module contained code similar to
  '`fun(1=0) -> ok end`'. (Thanks to Richard Carlsson.)

  The compiler would spend really long time compiling bit syntax expressions
  such as '`<<1:(50*1024*1024)>>`' and produce a huge .beam file. Corrected.

  The compiler would compile list comprehensions with many generators really,
  really slow. (Thanks to Thomas Raes.)

  Module attributes would be stored in reverse order compared to the order in
  the source code. (Thus, `M:module_info(attributes)` would also return the
  attributes in reversed order.)

  Defining a fun in an `after` block of a `try` would cause the compiler to
  crash or generate incorrect code. (Thanks to Martin Bjorklund.)

  The combination of binary pattern and a guard with andalso/orelse could cause
  the compiler to crash.

  Own Id: OTP-6121 Aux Id: seq10400

## Compiler 4.4

### Fixed Bugs and Malfunctions

- When a `.hrl` file is included using `-include_lib`, the include path is
  temporarily updated to include the directory the `.hrl` file was found in,
  which will allow that `.hrl` file to itself include files from the same
  directory using `-include`. (Thanks to Richard Carlsson.)

  Own Id: OTP-5944

### Improvements and New Features

- The `andalso` and `orelse` operators are now allowed to be used in guards.
  That also applies to match specifications.

  Own Id: OTP-5894 Aux Id: OTP-5149

- When given the new option `strict_record_tests`, the compiler will generate
  code that verifies the record type for `R#record.field` operations in guards.
  Code that verifies record types in bodies has already been generated since
  R10B, but in this release there will be a `{badrecord,RecordTag}` instead of a
  `badmatch` if the record verification test fails. See `m:compile` for more
  information.

  The Erlang shell always applies strict record tests.

  Own Id: OTP-5915 Aux Id: OTP-5714

- The BIF [`is_record/3`](`is_record/3`) can now be used in guards. Also,
  [`is_record/3`](`is_record/3`) can now be called without an `erlang:` module
  prefix for consistency with the other `is_*` functions.

  Own Id: OTP-5916

- The compiler options `ignore_try` and `ignore_cond`, which allowed code that
  used unquoted `try` or `cond` as atoms or record tags, has been removed. Old
  code that depended on the options need to be revised to have occurrences of
  `try` or `cond` as atom or record tags single-quoted. (Note: Although `cond`
  is a reserved keyword, there is no `cond` statement. It might be introduced in
  a future release.)

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6058

## Compiler 4.3.12

### Improvements and New Features

- The following code would crash the compiler:
  `case T of #r{s = ""} -> T #r{s = "x"} end`. (Thanks to Richard Carlsson.)

  The compiler could crash if binaries were constructed in certain guards
  involving boolean operators (including semicolon). (Thanks to Torbjorn
  Tornkvist.)

  Own Id: OTP-5872

- The compiler will now warn that the `megaco:format_versions/1` function is
  deprecated.

  Own Id: OTP-5976

## Compiler 4.3.11

### Improvements and New Features

- The compiler would assume that some patterns with aliases ('=') would not
  match if they were split into several lines. (Thanks to Peter Nagy/Mats
  Cronqvist.)

  Minor cleanups to eliminate Dialyzer warnings.

  Own Id: OTP-5791 Aux Id: seq10141

## Compiler 4.3.10

### Fixed Bugs and Malfunctions

- When given the new option `strict_record_tests`, the compiler will generate
  code that verifies the record type for `R#record.field` operations (in body
  context only, not in guards). See the documentation for the `compile` module
  for more information.

  The beam validator pass of the compiler could crash given in rare
  circumstances when given certain combinations of catches and record
  operations. (Thanks to Mats Cronqvist.)

  Attributes containing binaries (such as -a(<<1,2,3>>)) would crash the
  compiler. (Thanks to Roger Price.)

  Multiple behaviours in the same module will no longer generate a warning,
  unless one or more callbacks for the behaviours overlap. For instance, using
  both the `application` and `supervisor` behaviours in the same module will NOT
  generate any warning, but using `gen_server` and `gen_fsm` will.

  Own Id: OTP-5714 Aux Id: seq10073

- The pre-processor used to complain that the macro definition
  `-define(S(S), ??S).` was circular, which it isn't. (Thanks to Richard
  Carlsson.)

  Own Id: OTP-5777

## Compiler 4.3.9

### Fixed Bugs and Malfunctions

- Updating at least two fields of a record with a literal string could cause the
  compiler to generate dangerous code that could cause a crash at run-time (e.g.
  `R#r{a="abc",b=1}`). (Thanks to Mikael Karlsson.)

  Unnecessary tests (such as a 'case' with two case branches that were
  identical) could cause the compiler to crash. (Thanks to Fredrik Thulin.)

  The validation pass of the compiler could generate an error for correct code
  when floating point operations were used in try/catch statements.

  In bit syntax construction, any field following a binary field would always be
  marked as "aligned" (which may or may not be correct). That would cause the
  hipe native compiler to generate incorrect code if the field was in fact
  unaligned. (Thanks to Per Gustafsson.)

  Some complex guard expressions (such as `A#a.b==""; A#a.b==undefined`) would
  crash the compiler. (Thanks to Sean Hinde.)

  Compilation speed has been increased for modules with many functions and/or
  atoms (such as modules generated by the Asn1 application or other code
  generators).

  Own Id: OTP-5632 Aux Id: seq10057

## Compiler 4.3.8

### Fixed Bugs and Malfunctions

- In some circumstances, having two try/catch constructs following each in a
  function body, would cause an internal error to be generated (when in fact the
  generated code was correct). (Thanks to Fredrik Thulin.)

  Incorrect calls such as `M:42()` would crash the compiler. The compiler now
  generates a warning. (Thanks to Ulf Wiger.)

  Own Id: OTP-5553

### Improvements and New Features

- The new `fun M:F/A` construct creates a fun that refers to the latest version
  of `M:F/A`. This syntax is meant to replace tuple funs `{M,F}` which have many
  problems.

  The new type test [`is_function(Fun, A)`](`is_function/2`) (which may be used
  in guards) test whether `Fun` is a fun that can be applied with `A` arguments.
  (Currently, `Fun` can also be a tuple fun.)

  Own Id: OTP-5584

## Compiler 4.3.7

### Improvements and New Features

- Further improvements of encrypted debug info: New option `encrypt_debug_info`
  for compiler.

  Own Id: OTP-5541 Aux Id: seq9837

## Compiler 4.3.6

### Fixed Bugs and Malfunctions

- Fixed a bug in the validator of the generated code (beam_validator) which
  caused an internal compiler error even though the generated code was indeed
  correct.

  Own Id: OTP-5481 Aux Id: seq9798

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

## Compiler 4.3.5

### Fixed Bugs and Malfunctions

- Complex functions could cause the internal validator in the compiler to
  generate an internal error even though the generated code was correct.

  Own Id: OTP-5436 Aux Id: seq9781

## Compiler 4.3.4

### Fixed Bugs and Malfunctions

- In rare circumstances, incorrect code for record or tuple access could be
  generated. The incorrect code would either trigger an internal error in the
  compiler or cause an exception at run time. (Thanks to Martin Bjorklund.)

  Corrected a bug in in bit syntax matching where clauses could match in the
  wrong order. (Thanks to Ulf Wiger.)

  Own Id: OTP-5404 Aux Id: seq9767

## Compiler 4.3.3

### Improvements and New Features

- Given bit syntax construction in certain complex contexts involving a catch,
  the compiler would either crash or terminate due to failure in an internal
  consistency check. (Thanks to Fredrik Thulin.)

  Matches such as `<<103133:64/float>> = <<103133:64/float>>` used to fail. Now
  they succeed.

  Shadowing of variables in bit syntax matches in fun heads such as in
  `L = 8, F = fun(<<L:L,B:L>>) -> B end` was handled incorrectly by the
  compiler. The fun used to be compiled as if it was written
  '`>fun(<<8:8,B:8>>)`, while it should be compiled in the same way as
  `fun(<<L:8,B:L>>)`.

  A bug in the validation pass has been corrected. It sometimes occurred when
  the compiler optimized by reusing code for causing an exception when the
  reused code was called from within catch or try-catch statements. Then the
  validator refused to approve the code and complained about
  `fun(<<L:L,B:L>>) -> B end` was handled incorrectly by the in the same way as
  `fun(<<L:8,B:L>>)`.

  A bug in the unknown_catch_try_state.

  Corrected a bug in the optimizer that would cause the compiler to crash.
  (Thanks to Peter-Henry Mander.)

  There are now warnings generated if a bit syntax construction will fail at
  run-time because of a type mismatch (e.g. `<<an_atom:8>>`).

  Own Id: OTP-5342 Aux Id: OTP-5118, OTP-5270, OTP-5323

- Binary pattern matching such as `t(<<A:8>> = <<A:8>)` used to silently fail at
  runtime (i.e. never match). The compiler now generates an error for any such
  patterns.

  Own Id: OTP-5371

## Compiler 4.3.2

### Fixed Bugs and Malfunctions

- In rare cases, the code compiler code generate code for a tuple match that
  could crash the emulator if passed a term that was not a tuple.

  If a bit syntax construction failed within a catch, previously assigned
  variables could get the wrong value.

  The compiler now runs a validation pass on the generated code and aborts
  before writing a Beam file if any suspect code is found. In particular, the
  validation pass checks for incorrect code that may cause emulator crashes or
  other strange symptoms in the emulator.

  Some corrections to the unsupported feature parameterized modules by Richard
  Carlsson (HiPE).

  Own Id: OTP-5247 Aux Id: OTP-5235

## Compiler 4.3.1

### Fixed Bugs and Malfunctions

- Corrected the release note regarding `try/catch` below. `try/catch` DOES work
  in the initial R10B release.

  A few minor issues code generation issues were corrected. Although the
  generated code was correct, it was slightly slower and larger than it needed
  to be.

  A debug printout (that could be seen in rare circumstances) has been removed.

  `not record_test(not_a_tuple, RecordTag)` and similar expressions in a guard
  would fail.

  New options `basic_validation` and `strong_validation` to do a quick check of
  the code of a module.

  The `inline` option was not recognized if it appeared in a `-compile()`
  directive inside the module.

  Corrected some bugs in the undocumented feature "parameterized modules".

  Own Id: OTP-5198

- When the undocumented feature "parameterized modules" was used, the `?MODULE`
  macro did not work correctly.

  Own Id: OTP-5224
