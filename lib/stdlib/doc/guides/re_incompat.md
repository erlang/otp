<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-->
# PCRE2 Migration

Starting with Erlang/OTP 28, the underlying library for handling regular expressions via the `re`
module changes from PCRE to PCRE2. This upgrade brings Erlang's regular expression capabilities
more in line with modern standards, particularly Perl, but also introduces several breaking
changes and differences in behavior compared to PCRE.

A key philosophical difference is that **PCRE2 is much stricter about pattern syntax**. Invalid
constructs that PCRE might have ignored or treated as literal characters will now typically raise
compilation errors, similar to using Perl in strict mode.

Below is a summary of notable incompatibilities and behavioral changes:

## Stricter Error Handling & Syntax Validation

  * **Invalid Escapes:** Undocumented escape sequences (for example, `\M`, `\i`) are now treated
  as errors. PCRE often treated these as literal characters (for example, `\M` became `M`).
  * **Invalid Escapes in Character Classes:** Using sequences that are not valid *within* a
  character class (for example, anchors like `\B`, sequences like `\R` or `\X`) is now an error
  (for example, `[\B]`). PCRE might have treated these literally.
  * **Invalid Character Ranges:** Ranges where the start point is logically after the end point,
  or involving incompatible types (for example, `[\d-a]`), are now errors. PCRE might have
  interpreted this as matching a digit, a hyphen, or 'a' literally.
  * **Invalid Backreferences:** Using a backreference to a non-existent capturing group (for
  example, `\8` when only 7 groups exist) is now treated as an error.
  * **Invalid Control Characters (`\cx`):** The `\c` escape must be followed by a character that
  maps to a valid control character (typically ASCII characters `@` through `_`, corresponding to
  `\x00` through `\x1F`, and `?` for `\x7F`). Using characters in the range 127-255 will result
  in an error.

## Syntax Changes & Requirements

  * **`\x` Requires Hex Digits:** The `\x` escape now *must* be followed by hexadecimal digits.
  Use `\xNN` (one or two digits) or `\x{HHHH}` (variable number of digits in braces). Using `\x`
  alone is an error.
  * **`\N` (Match Non-Newline) in Character Classes:** The shorthand `\N` is not allowed directly
  within a character class (for example, `[\N]` is invalid). However, the named Unicode sequence
  `\N{U+...}` *is* allowed (for example, `[\N{U+0041}]` to match 'A').
  * **Empty Group Names:** Defining capturing groups with empty names using `(?''...)` syntax is
  no longer supported and will cause an error.

## Option Handling

  * **Compile-Time vs. Run-Time Options:** Options affecting newline conventions (`{newline, _}`)
  or backslash R behavior (`bsr_anycrlf`, `bsr_unicode`) only control pattern *compilation*.
  If a pattern is pre-compiled using `re:compile/2`, passing incompatible options for these
  settings later to `re:run/3`, `re:replace/4`, or `re:split/3` will result in an error.
  If present, the options must match those used at compile time.

## Behavioral Changes & Feature Restrictions

  * **`\K` in Lookarounds:** The `\K` escape (reset match start) cannot be used inside lookahead
  `(?=...)`, `(?!...)` or lookbehind `(?<=...)`, `(?<!...)` assertions.
  * **`\K` in global match:** does no longer produce a duplicate match.
  * **`(*COMMIT)` with `no_start_optimize`:** The interaction between the `(*COMMIT)` verb and
  the `no_start_optimize` option may lead to different behavior than might be expected based on
  previous versions or other regex engines. Careful testing is advised if using this combination.
  * **`re:split/3` with Branch Reset Groups (`(?|...)`):** The behavior of `re:split/3` when using
  branch reset groups combined with backreferences within the pattern may differ from PCRE and
  potentially current Perl versions.
      * *Example:* For `re:split("abcabc", "(?|(abc)|(xyz))\\1", [{return, list}])`:
          * PCRE2 (OTP 28) returns: `["", "abc", ""]`
          * Some interpretations might expect `["", "abc", "", ""]` (empty binary instead of
          `undef` for the non-participating capture group within the split context). Reference:
          [Perl Issue \#22912](https://github.com/Perl/perl5/issues/22912)
  * **Default Handling of Bytes 128-255 Changed:** In OTP 28, when the
  `unicode` option is not used, character properties `\w`, `\s`, and so on, and option `caseless`
  now operate strictly under ASCII rules (bytes 0-127 only). That means that `\w` will not match
  latin-1 locale characters such as `åäö`.

## Unicode Property Updates (`\p{...}`, `\P{...}`)

PCRE2 uses updated Unicode character property data. This means the set of characters matched by
properties like `\p{L}` (Letter), `\p{N}` (Number), script names (`\p{Arabic}`), etc., may have
changed slightly. Specific examples noted:

  * `\p{Lo}` (Letter, Other) now includes characters like `U+4DB6`.
  * `\p{Arabic}` script now includes `U+061C` (Arabic Letter Mark) and formatting characters
  `U+0650` - `U+0655`.
  * `\p{Common}` script (characters used across multiple scripts) no longer includes `U+0589`
  (Armenian Full Stop). *(Verify character if critical)*.

## Erlang-Specific Changes

  * **Compiled Patterns Should Not be Shared:** Sharing the result of re:compile/2 across node
  boundaries or persisting it has never been officially supported, although it might have
  accidentally worked between certain previous versions. The internal format produced by
  `re:compile/2` has changed, and will not work on older OTP versions or across nodes. Relying
  on this undocumented behavior must be changed to compile the regular expression on the node
  instance where it is intended to be used.