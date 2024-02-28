%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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
%%
-module(re).
-moduledoc({file, "../doc/src/re.md"}).
-export([grun/3,urun/3,ucompile/2,replace/3,replace/4,split/2,split/3]).

-export_type([mp/0, compile_options/0, options/0]).

-doc """
Opaque data type containing a compiled regular expression.

`t:mp/0` is guaranteed to be a tuple() having the atom `re_pattern` as its first element, to
allow for matching in guards. The arity of the tuple or the content of the other
fields can change in future Erlang/OTP releases.
""".
-type mp() :: {re_pattern, _, _, _, _}.

-type nl_spec() :: cr | crlf | lf | anycrlf | any.

-type compile_options() :: [compile_option()].
-type compile_option() :: unicode | anchored | caseless | dollar_endonly
                        | dotall | extended | firstline | multiline
                        | no_auto_capture | dupnames | ungreedy
                        | {newline, nl_spec()}
                        | bsr_anycrlf | bsr_unicode
                        | no_start_optimize | ucp | never_utf.

-type options() :: [option()].
-type option() :: anchored | global | notbol | noteol | notempty |
                  notempty_atstart | report_errors |
                  {offset, non_neg_integer()} |
                  {match_limit, non_neg_integer()} |
                  {match_limit_recursion, non_neg_integer()} |
                  {newline, NLSpec :: nl_spec()} |
                  bsr_anycrlf | bsr_unicode | {capture, ValueSpec :: capture()} |
                  {capture, ValueSpec :: capture(), Type :: index | list | binary} |
                  compile_option().
-type capture() :: all | all_but_first | all_names | first | none |
                   ValueList :: [integer() | string() | atom()].

-type replace_fun() :: fun((binary(), [binary()]) -> iodata() | unicode:charlist()).

%%% BIFs

-export([internal_run/4]).

-export([version/0, compile/1, compile/2, run/2, run/3, inspect/2]).

-doc """
The return of this function is a string with the PCRE version of the system that
was used in the Erlang/OTP compilation.
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec version() -> binary().

%% We must inline these functions so that the stacktrace points to
%% the correct function.
-compile({inline, [badarg_with_cause/2, badarg_with_info/1]}).

version() ->
    erlang:nif_error(undef).

-doc "The same as [`compile(Regexp,[])`](`compile/2`)".
-spec compile(Regexp) -> {ok, MP} | {error, ErrSpec} when
      Regexp :: iodata(),
      MP :: mp(),
      ErrSpec :: {ErrString :: string(), Position :: non_neg_integer()}.

compile(_) ->
    erlang:nif_error(undef).

-doc """
Compiles a regular expression, with the syntax described below, into an internal
format to be used later as a parameter to `run/2` and `run/3`.

Compiling the regular expression before matching is useful if the same
expression is to be used in matching against multiple subjects during the
lifetime of the program. Compiling once and executing many times is far more
efficient than compiling each time one wants to match.

When option `unicode` is specified, the regular expression is to be specified as
a valid Unicode `charlist()`, otherwise as any valid `t:iodata/0`.

[](){: #compile_options }

Options:

- **`unicode`** - The regular expression is specified as a Unicode `charlist()`
  and the resulting regular expression code is to be run against a valid Unicode
  `charlist()` subject. Also consider option `ucp` when using Unicode
  characters.

- **`anchored`** - The pattern is forced to be "anchored", that is, it is
  constrained to match only at the first matching point in the string that is
  searched (the "subject string"). This effect can also be achieved by
  appropriate constructs in the pattern itself.

- **`caseless`** - Letters in the pattern match both uppercase and lowercase
  letters. It is equivalent to Perl option `/i` and can be changed within a
  pattern by a `(?i)` option setting. Uppercase and lowercase letters are
  defined as in the ISO 8859-1 character set.

- **`dollar_endonly`** - A dollar metacharacter in the pattern matches only at
  the end of the subject string. Without this option, a dollar also matches
  immediately before a newline at the end of the string (but not before any
  other newlines). This option is ignored if option `multiline` is specified.
  There is no equivalent option in Perl, and it cannot be set within a pattern.

- **`dotall`** - A dot in the pattern matches all characters, including those
  indicating newline. Without it, a dot does not match when the current position
  is at a newline. This option is equivalent to Perl option `/s` and it can be
  changed within a pattern by a `(?s)` option setting. A negative class, such as
  `[^a]`, always matches newline characters, independent of the setting of this
  option.

- **`extended`** - If this option is set, most white space characters in the
  pattern are totally ignored except when escaped or inside a character class.
  However, white space is not allowed within sequences such as `(?>` that
  introduce various parenthesized subpatterns, nor within a numerical quantifier
  such as `{1,3}`. However, ignorable white space is permitted between an item
  and a following quantifier and between a quantifier and a following + that
  indicates possessiveness.

  White space did not used to include the VT character (code 11), because Perl
  did not treat this character as white space. However, Perl changed at release
  5.18, so PCRE followed at release 8.34, and VT is now treated as white space.

  This also causes characters between an unescaped # outside a character class
  and the next newline, inclusive, to be ignored. This is equivalent to Perl's
  `/x` option, and it can be changed within a pattern by a `(?x)` option
  setting.

  With this option, comments inside complicated patterns can be included.
  However, notice that this applies only to data characters. Whitespace
  characters can never appear within special character sequences in a pattern,
  for example within sequence `(?(` that introduces a conditional subpattern.

- **`firstline`** - An unanchored pattern is required to match before or at the
  first newline in the subject string, although the matched text can continue
  over the newline.

- **`multiline`** - By default, PCRE treats the subject string as consisting of
  a single line of characters (even if it contains newlines). The "start of
  line" metacharacter (`^`) matches only at the start of the string, while the
  "end of line" metacharacter (`$`) matches only at the end of the string, or
  before a terminating newline (unless option `dollar_endonly` is specified).
  This is the same as in Perl.

  When this option is specified, the "start of line" and "end of line"
  constructs match immediately following or immediately before internal newlines
  in the subject string, respectively, as well as at the very start and end.
  This is equivalent to Perl option `/m` and can be changed within a pattern by
  a `(?m)` option setting. If there are no newlines in a subject string, or no
  occurrences of `^` or `$` in a pattern, setting `multiline` has no effect.

- **`no_auto_capture`** - Disables the use of numbered capturing parentheses in
  the pattern. Any opening parenthesis that is not followed by `?` behaves as if
  it is followed by `?:`. Named parentheses can still be used for capturing (and
  they acquire numbers in the usual way). There is no equivalent option in Perl.

- **`dupnames`** - Names used to identify capturing subpatterns need not be
  unique. This can be helpful for certain types of pattern when it is known that
  only one instance of the named subpattern can ever be matched. More details of
  named subpatterns are provided below.

- **`ungreedy`** - Inverts the "greediness" of the quantifiers so that they are
  not greedy by default, but become greedy if followed by "?". It is not
  compatible with Perl. It can also be set by a `(?U)` option setting within the
  pattern.

- **`{newline, NLSpec}`** - Overrides the default definition of a newline in the
  subject string, which is LF (ASCII 10) in Erlang.

  - **`cr`** - Newline is indicated by a single character `cr` (ASCII 13).

  - **`lf`** - Newline is indicated by a single character LF (ASCII 10), the
    default.

  - **`crlf`** - Newline is indicated by the two-character CRLF (ASCII 13
    followed by ASCII 10) sequence.

  - **`anycrlf`** - Any of the three preceding sequences is to be recognized.

  - **`any`** - Any of the newline sequences above, and the Unicode sequences VT
    (vertical tab, U+000B), FF (formfeed, U+000C), NEL (next line, U+0085), LS
    (line separator, U+2028), and PS (paragraph separator, U+2029).

- **`bsr_anycrlf`** - Specifies specifically that \\R is to match only the CR,
  LF, or CRLF sequences, not the Unicode-specific newline characters.

- **`bsr_unicode`** - Specifies specifically that \\R is to match all the
  Unicode newline characters (including CRLF, and so on, the default).

- **`no_start_optimize`** - Disables optimization that can malfunction if
  "Special start-of-pattern items" are present in the regular expression. A
  typical example would be when matching "DEFABC" against "(*COMMIT)ABC", where
  the start optimization of PCRE would skip the subject up to "A" and never
  realize that the (*COMMIT) instruction is to have made the matching fail. This
  option is only relevant if you use "start-of-pattern items", as discussed in
  section [PCRE Regular Expression Details](`m:re#module-pcre-regular-expression-details`).

- **`ucp`** - Specifies that Unicode character properties are to be used when
  resolving \\B, \\b, \\D, \\d, \\S, \\s, \\W and \\w. Without this flag, only
  ISO Latin-1 properties are used. Using Unicode properties hurts performance,
  but is semantically correct when working with Unicode characters beyond the
  ISO Latin-1 range.

- **`never_utf`** - Specifies that the (*UTF) and/or (*UTF8) "start-of-pattern
  items" are forbidden. This flag cannot be combined with option `unicode`.
  Useful if ISO Latin-1 patterns from an external source are to be compiled.
""".
-spec compile(Regexp, Options) -> {ok, MP} | {error, ErrSpec} when
      Regexp :: iodata() | unicode:charlist(),
      Options :: [Option],
      Option :: compile_option(),
      MP :: mp(),
      ErrSpec :: {ErrString :: string(), Position :: non_neg_integer()}.

compile(_, _) ->
    erlang:nif_error(undef).

-doc #{ equiv => run(Subject,RE,[]) }.
-spec run(Subject, RE) -> {match, Captured} | nomatch when
      Subject :: iodata() | unicode:charlist(),
      RE :: mp() | iodata(),
      Captured :: [CaptureData],
      CaptureData :: {integer(), integer()}.

run(_, _) ->
    erlang:nif_error(undef).

-doc """
Executes a regular expression matching, and returns `match/{match, Captured}` or
`nomatch`.

The regular expression can be specified either as `t:iodata/0` in
which case it is automatically compiled (as by [`compile/2`](`compile/2`)) and
executed, or as a precompiled `t:mp/0` in which case it is executed against the
subject directly.

When compilation is involved, exception `badarg` is thrown if a compilation
error occurs. Call [`compile/2`](`compile/2`) to get information about the
location of the error in the regular expression.

If the regular expression is previously compiled, the option list can only
contain the following options:

- `anchored`
- `{capture, ValueSpec}/{capture, ValueSpec, Type}`
- `global`
- `{match_limit, integer() >= 0}`
- `{match_limit_recursion, integer() >= 0}`
- `{newline, NLSpec}`
- `notbol`
- `notempty`
- `notempty_atstart`
- `noteol`
- `{offset, integer() >= 0}`
- `report_errors`

Otherwise all options valid for function [`compile/2`](`compile/2`) are also
allowed. Options allowed both for compilation and execution of a match, namely
`anchored` and `{newline, NLSpec}`, affect both the compilation and execution if
present together with a non-precompiled regular expression.

If the regular expression was previously compiled with option `unicode`,
`Subject` is to be provided as a valid Unicode `charlist()`, otherwise any
`t:iodata/0` will do. If compilation is involved and option `unicode` is
specified, both `Subject` and the regular expression are to be specified as
valid Unicode `charlists()`.

`{capture, ValueSpec}/{capture, ValueSpec, Type}` defines what to return from
the function upon successful matching. The `capture` tuple can contain both a
value specification, telling which of the captured substrings are to be
returned, and a type specification, telling how captured substrings are to be
returned (as index tuples, lists, or binaries). The options are described in
detail below.

If the capture options describe that no substring capturing is to be done
(`{capture, none}`), the function returns the single atom `match` upon
successful matching, otherwise the tuple `{match, ValueList}`. Disabling
capturing can be done either by specifying `none` or an empty list as
`ValueSpec`.

Option `report_errors` adds the possibility that an error tuple is returned. The
tuple either indicates a matching error (`match_limit` or
`match_limit_recursion`), or a compilation error, where the error tuple has the
format `{error, {compile, CompileErr}}`. Notice that if option `report_errors`
is not specified, the function never returns error tuples, but reports
compilation errors as a `badarg` exception and failed matches because of
exceeded match limits simply as `nomatch`.

The following options are relevant for execution:

- **`anchored`** - Limits [`run/3`](`run/3`) to matching at the first matching
  position. If a pattern was compiled with `anchored`, or turned out to be
  anchored by virtue of its contents, it cannot be made unanchored at matching
  time, hence there is no `unanchored` option.

- **`global`** - Implements global (repetitive) search (flag `g` in Perl). Each
  match is returned as a separate `t:list/0` containing the specific match and
  any matching subexpressions (or as specified by option `capture`. The
  `Captured` part of the return value is hence a `t:list/0` of `t:list/0`s when
  this option is specified.

  The interaction of option `global` with a regular expression that matches an
  empty string surprises some users. When option `global` is specified,
  [`run/3`](`run/3`) handles empty matches in the same way as Perl: a
  zero-length match at any point is also retried with options
  `[anchored, notempty_atstart]`. If that search gives a result of length > 0,
  the result is included. Example:

  ```erlang
  re:run("cat","(|at)",[global]).
  ```

  The following matchings are performed:

  - **At offset `0`** - The regular expression `(|at)` first match at the
    initial position of string `cat`, giving the result set `[{0,0},{0,0}]` (the
    second `{0,0}` is because of the subexpression marked by the parentheses).
    As the length of the match is 0, we do not advance to the next position yet.

  - **At offset `0` with `[anchored, notempty_atstart]`** - The search is
    retried with options `[anchored, notempty_atstart]` at the same position,
    which does not give any interesting result of longer length, so the search
    position is advanced to the next character (`a`).

  - **At offset `1`** - The search results in `[{1,0},{1,0}]`, so this search is
    also repeated with the extra options.

  - **At offset `1` with `[anchored, notempty_atstart]`** - Alternative `ab` is
    found and the result is \[\{1,2\},\{1,2\}]. The result is added to the list
    of results and the position in the search string is advanced two steps.

  - **At offset `3`** - The search once again matches the empty string, giving
    `[{3,0},{3,0}]`.

  - **At offset `1` with `[anchored, notempty_atstart]`** - This gives no result
    of length > 0 and we are at the last position, so the global search is
    complete.

  The result of the call is:

  ```erlang
  {match,[[{0,0},{0,0}],[{1,0},{1,0}],[{1,2},{1,2}],[{3,0},{3,0}]]}
  ```

- **`notempty`** - An empty string is not considered to be a valid match if this
  option is specified. If alternatives in the pattern exist, they are tried. If
  all the alternatives match the empty string, the entire match fails.

  _Example:_

  If the following pattern is applied to a string not beginning with "a" or "b",
  it would normally match the empty string at the start of the subject:

  ```text
  a?b?
  ```

  With option `notempty`, this match is invalid, so [`run/3`](`run/3`) searches
  further into the string for occurrences of "a" or "b".

- **`notempty_atstart`** - Like `notempty`, except that an empty string match
  that is not at the start of the subject is permitted. If the pattern is
  anchored, such a match can occur only if the pattern contains \\K.

  Perl has no direct equivalent of `notempty` or `notempty_atstart`, but it does
  make a special case of a pattern match of the empty string within its split()
  function, and when using modifier `/g`. The Perl behavior can be emulated
  after matching a null string by first trying the match again at the same
  offset with `notempty_atstart` and `anchored`, and then, if that fails, by
  advancing the starting offset (see below) and trying an ordinary match again.

- **`notbol`** - Specifies that the first character of the subject string is not
  the beginning of a line, so the circumflex metacharacter is not to match
  before it. Setting this without `multiline` (at compile time) causes
  circumflex never to match. This option only affects the behavior of the
  circumflex metacharacter. It does not affect \\A.

- **`noteol`** - Specifies that the end of the subject string is not the end of
  a line, so the dollar metacharacter is not to match it nor (except in
  multiline mode) a newline immediately before it. Setting this without
  `multiline` (at compile time) causes dollar never to match. This option
  affects only the behavior of the dollar metacharacter. It does not affect \\Z
  or \\z.

- **`report_errors`** - Gives better control of the error handling in
  [`run/3`](`run/3`). When specified, compilation errors (if the regular
  expression is not already compiled) and runtime errors are explicitly returned
  as an error tuple.

  The following are the possible runtime errors:

  - **`match_limit`** - The PCRE library sets a limit on how many times the
    internal match function can be called. Defaults to 10,000,000 in the library
    compiled for Erlang. If `{error, match_limit}` is returned, the execution of
    the regular expression has reached this limit. This is normally to be
    regarded as a `nomatch`, which is the default return value when this occurs,
    but by specifying `report_errors`, you are informed when the match fails
    because of too many internal calls.

  - **`match_limit_recursion`** - This error is very similar to `match_limit`,
    but occurs when the internal match function of PCRE is "recursively" called
    more times than the `match_limit_recursion` limit, which defaults to
    10,000,000 as well. Notice that as long as the `match_limit` and
    `match_limit_default` values are kept at the default values, the
    `match_limit_recursion` error cannot occur, as the `match_limit` error
    occurs before that (each recursive call is also a call, but not conversely).
    Both limits can however be changed, either by setting limits directly in the
    regular expression string (see section
    [PCRE Regular Eexpression Details](`m:re#module-pcre-regular-expression-details`)) or by
    specifying options to [`run/3`](`run/3`).

  It is important to understand that what is referred to as "recursion" when
  limiting matches is not recursion on the C stack of the Erlang machine or on
  the Erlang process stack. The PCRE version compiled into the Erlang VM uses
  machine "heap" memory to store values that must be kept over recursion in
  regular expression matches.

- **`{match_limit, integer() >= 0}`** - Limits the execution time of a match in
  an implementation-specific way. It is described as follows by the PCRE
  documentation:

  > The match_limit field provides a means of preventing PCRE from using
  > up a vast amount of resources when running patterns that are not going
  > to match, but which have a very large number of possibilities in their
  > search trees. The classic example is a pattern that uses nested
  > unlimited repeats.
  >
  > Internally, pcre_exec() uses a function called match(), which it calls
  > repeatedly (sometimes recursively). The limit set by match_limit is
  > imposed on the number of times this function is called during a match,
  > which has the effect of limiting the amount of backtracking that can
  > take place. For patterns that are not anchored, the count restarts
  > from zero for each position in the subject string.

  This means that runaway regular expression matches can fail faster if the
  limit is lowered using this option. The default value 10,000,000 is compiled
  into the Erlang VM.

  > #### Note {: .info }
  >
  > This option does in no way affect the execution of the Erlang VM in terms of
  > "long running BIFs". [`run/3`](`run/3`) always gives control back to the
  > scheduler of Erlang processes at intervals that ensures the real-time
  > properties of the Erlang system.

- **`{match_limit_recursion, integer() >= 0}`** - Limits the execution time and
  memory consumption of a match in an implementation-specific way, very similar
  to `match_limit`. It is described as follows by the PCRE documentation:

  > The match_limit_recursion field is similar to match_limit, but instead
  > of limiting the total number of times that match() is called, it
  > limits the depth of recursion. The recursion depth is a smaller number
  > than the total number of calls, because not all calls to match() are
  > recursive. This limit is of use only if it is set smaller than
  > match_limit.
  >
  > Limiting the recursion depth limits the amount of machine stack that
  > can be used, or, when PCRE has been compiled to use memory on the heap
  > instead of the stack, the amount of heap memory that can be used.

  The Erlang VM uses a PCRE library where heap memory is used when regular
  expression match recursion occurs. This therefore limits the use of machine
  heap, not C stack.

  Specifying a lower value can result in matches with deep recursion failing,
  when they should have matched:

  ```erlang
  1> re:run("aaaaaaaaaaaaaz","(a+)*z").
  {match,[{0,14},{0,13}]}
  2> re:run("aaaaaaaaaaaaaz","(a+)*z",[{match_limit_recursion,5}]).
  nomatch
  3> re:run("aaaaaaaaaaaaaz","(a+)*z",[{match_limit_recursion,5},report_errors]).
  {error,match_limit_recursion}
  ```

  This option and option `match_limit` are only to be used in rare cases.
  Understanding of the PCRE library internals is recommended before tampering
  with these limits.

- **`{offset, integer() >= 0}`** - Start matching at the offset (position)
  specified in the subject string. The offset is zero-based, so that the default
  is `{offset,0}` (all of the subject string).

- **`{newline, NLSpec}`** - Overrides the default definition of a newline in the
  subject string, which is LF (ASCII 10) in Erlang.

  - **`cr`** - Newline is indicated by a single character CR (ASCII 13).

  - **`lf`** - Newline is indicated by a single character LF (ASCII 10), the
    default.

  - **`crlf`** - Newline is indicated by the two-character CRLF (ASCII 13
    followed by ASCII 10) sequence.

  - **`anycrlf`** - Any of the three preceding sequences is be recognized.

  - **`any`** - Any of the newline sequences above, and the Unicode sequences VT
    (vertical tab, U+000B), FF (formfeed, U+000C), NEL (next line, U+0085), LS
    (line separator, U+2028), and PS (paragraph separator, U+2029).

- **`bsr_anycrlf`** - Specifies specifically that \\R is to match only the CR
  LF, or CRLF sequences, not the Unicode-specific newline characters. (Overrides
  the compilation option.)

- **`bsr_unicode`** - Specifies specifically that \\R is to match all the
  Unicode newline characters (including CRLF, and so on, the default).
  (Overrides the compilation option.)

- **`{capture, ValueSpec}`/`{capture, ValueSpec, Type}`** - Specifies which
  captured substrings are returned and in what format. By default,
  [`run/3`](`run/3`) captures all of the matching part of the substring and all
  capturing subpatterns (all of the pattern is automatically captured). The
  default return type is (zero-based) indexes of the captured parts of the
  string, specified as `{Offset,Length}` pairs (the `index` `Type` of
  capturing).

  As an example of the default behavior, the following call returns, as first
  and only captured string, the matching part of the subject ("abcd" in the
  middle) as an index pair `{3,4}`, where character positions are zero-based,
  just as in offsets:

  ```erlang
  re:run("ABCabcdABC","abcd",[]).
  ```

  The return value of this call is:

  ```erlang
  {match,[{3,4}]}
  ```

  Another (and quite common) case is where the regular expression matches all of
  the subject:

  ```erlang
  re:run("ABCabcdABC",".*abcd.*",[]).
  ```

  Here the return value correspondingly points out all of the string, beginning
  at index 0, and it is 10 characters long:

  ```erlang
  {match,[{0,10}]}
  ```

  If the regular expression contains capturing subpatterns, like in:

  ```erlang
  re:run("ABCabcdABC",".*(abcd).*",[]).
  ```

  all of the matched subject is captured, as well as the captured substrings:

  ```erlang
  {match,[{0,10},{3,4}]}
  ```

  The complete matching pattern always gives the first return value in the list
  and the remaining subpatterns are added in the order they occurred in the
  regular expression.

  The capture tuple is built up as follows:

  - **`ValueSpec`** - Specifies which captured (sub)patterns are to be returned.
    `ValueSpec` can either be an atom describing a predefined set of return
    values, or a list containing the indexes or the names of specific
    subpatterns to return.

    The following are the predefined sets of subpatterns:

    - **`all`** - All captured subpatterns including the complete matching
      string. This is the default.

    - **`all_names`** - All _named_ subpatterns in the regular expression, as if
      a `t:list/0` of all the names _in alphabetical order_ was specified. The
      list of all names can also be retrieved with `inspect/2`.

    - **`first`** - Only the first captured subpattern, which is always the
      complete matching part of the subject. All explicitly captured subpatterns
      are discarded.

    - **`all_but_first`** - All but the first matching subpattern, that is, all
      explicitly captured subpatterns, but not the complete matching part of the
      subject string. This is useful if the regular expression as a whole
      matches a large part of the subject, but the part you are interested in is
      in an explicitly captured subpattern. If the return type is `list` or
      `binary`, not returning subpatterns you are not interested in is a good
      way to optimize.

    - **`none`** - Returns no matching subpatterns, gives the single atom
      `match` as the return value of the function when matching successfully
      instead of the `{match, list()}` return. Specifying an empty list gives
      the same behavior.

    The value list is a list of indexes for the subpatterns to return, where
    index 0 is for all of the pattern, and 1 is for the first explicit capturing
    subpattern in the regular expression, and so on. When using named captured
    subpatterns (see below) in the regular expression, one can use `t:atom/0`s
    or `t:string/0`s to specify the subpatterns to be returned. For example,
    consider the regular expression:

    ```text
    ".*(abcd).*"
    ```

    matched against string "ABCabcdABC", capturing only the "abcd" part (the
    first explicit subpattern):

    ```erlang
    re:run("ABCabcdABC",".*(abcd).*",[{capture,[1]}]).
    ```

    The call gives the following result, as the first explicitly captured
    subpattern is "(abcd)", matching "abcd" in the subject, at (zero-based)
    position 3, of length 4:

    ```erlang
    {match,[{3,4}]}
    ```

    Consider the same regular expression, but with the subpattern explicitly
    named 'FOO':

    ```text
    ".*(?<FOO>abcd).*"
    ```

    With this expression, we could still give the index of the subpattern with
    the following call:

    ```erlang
    re:run("ABCabcdABC",".*(?<FOO>abcd).*",[{capture,[1]}]).
    ```

    giving the same result as before. But, as the subpattern is named, we can
    also specify its name in the value list:

    ```erlang
    re:run("ABCabcdABC",".*(?<FOO>abcd).*",[{capture,['FOO']}]).
    ```

    This would give the same result as the earlier examples, namely:

    ```erlang
    {match,[{3,4}]}
    ```

    The values list can specify indexes or names not present in the regular
    expression, in which case the return values vary depending on the type. If
    the type is `index`, the tuple `{-1,0}` is returned for values with no
    corresponding subpattern in the regular expression, but for the other types
    (`binary` and `list`), the values are the empty binary or list,
    respectively.

  - **`Type`** - Optionally specifies how captured substrings are to be
    returned. If omitted, the default of `index` is used.

    `Type` can be one of the following:

    - **`index`** - Returns captured substrings as pairs of byte indexes into
      the subject string and length of the matching string in the subject (as if
      the subject string was flattened with `erlang:iolist_to_binary/1` or
      `unicode:characters_to_binary/2` before matching). Notice that option
      `unicode` results in _byte-oriented_ indexes in a (possibly virtual)
      _UTF-8 encoded_ binary. A byte index tuple `{0,2}` can therefore represent
      one or two characters when `unicode` is in effect. This can seem
      counter-intuitive, but has been deemed the most effective and useful way
      to do it. To return lists instead can result in simpler code if that is
      desired. This return type is the default.

    - **`list`** - Returns matching substrings as lists of characters (Erlang
      `t:string/0`s). It option `unicode` is used in combination with the \\C
      sequence in the regular expression, a captured subpattern can contain
      bytes that are not valid UTF-8 (\\C matches bytes regardless of character
      encoding). In that case the `list` capturing can result in the same types
      of tuples that `unicode:characters_to_list/2` can return, namely
      three-tuples with tag `incomplete` or `error`, the successfully converted
      characters and the invalid UTF-8 tail of the conversion as a binary. The
      best strategy is to avoid using the \\C sequence when capturing lists.

    - **`binary`** - Returns matching substrings as binaries. If option
      `unicode` is used, these binaries are in UTF-8. If the \\C sequence is
      used together with `unicode`, the binaries can be invalid UTF-8.

  In general, subpatterns that were not assigned a value in the match are
  returned as the tuple `{-1,0}` when `type` is `index`. Unassigned subpatterns
  are returned as the empty binary or list, respectively, for other return
  types. Consider the following regular expression:

  ```text
  ".*((?<FOO>abdd)|a(..d)).*"
  ```

  There are three explicitly capturing subpatterns, where the opening
  parenthesis position determines the order in the result, hence
  `((?<FOO>abdd)|a(..d))` is subpattern index 1, `(?<FOO>abdd)` is subpattern
  index 2, and `(..d)` is subpattern index 3. When matched against the following
  string:

  ```text
  "ABCabcdABC"
  ```

  the subpattern at index 2 does not match, as "abdd" is not present in the
  string, but the complete pattern matches (because of the alternative
  `a(..d)`). The subpattern at index 2 is therefore unassigned and the default
  return value is:

  ```erlang
  {match,[{0,10},{3,4},{-1,0},{4,3}]}
  ```

  Setting the capture `Type` to `binary` gives:

  ```erlang
  {match,[<<"ABCabcdABC">>,<<"abcd">>,<<>>,<<"bcd">>]}
  ```

  Here the empty binary (`<<>>`) represents the unassigned subpattern. In the
  `binary` case, some information about the matching is therefore lost, as
  `<<>>` can also be an empty string captured.

  If differentiation between empty matches and non-existing subpatterns is
  necessary, use the `type` `index` and do the conversion to the final type in
  Erlang code.

  When option `global` is speciified, the `capture` specification affects each
  match separately, so that:

  ```erlang
  re:run("cacb","c(a|b)",[global,{capture,[1],list}]).
  ```

  gives

  ```erlang
  {match,[["a"],["b"]]}
  ```

For a descriptions of options only affecting the compilation step, see
`compile/2`.
""".
-spec run(Subject, RE, Options) -> {match, Captured} |
                                   match |
                                   nomatch |
				   {error, ErrType} when
      Subject :: iodata() | unicode:charlist(),
      RE :: mp() | iodata() | unicode:charlist(),
      Options :: options(),
      Captured :: [CaptureData] | [[CaptureData]],
      CaptureData :: {integer(), integer()}
                   | ListConversionData
                   | binary(),
      ListConversionData :: string()
                          | {error, string(), binary()}
                          | {incomplete, string(), binary()},
      ErrType :: match_limit | match_limit_recursion | {compile,  CompileErr}, 
      CompileErr :: {ErrString :: string(), Position :: non_neg_integer()}.

run(_, _, _) ->
    erlang:nif_error(undef).

-doc false.
-spec internal_run(Subject, RE, Options, FirstCall) -> {match, Captured} |
                                                       match |
                                                       nomatch |
                                                       {error, ErrType} when
      Subject :: iodata() | unicode:charlist(),
      RE :: mp() | iodata() | unicode:charlist(),
      Options :: [Option],
      Option :: anchored | global | notbol | noteol | notempty 
	      | notempty_atstart | report_errors
              | {offset, non_neg_integer()} |
		{match_limit, non_neg_integer()} |
		{match_limit_recursion, non_neg_integer()} |
                {newline, NLSpec :: nl_spec()} |
                bsr_anycrlf | bsr_unicode | {capture, ValueSpec} |
                {capture, ValueSpec, Type} | CompileOpt,
      Type :: index | list | binary,
      ValueSpec :: all | all_but_first | all_names | first | none | ValueList,
      ValueList :: [ValueID],
      ValueID :: integer() | string() | atom(),
      CompileOpt :: compile_option(),
      Captured :: [CaptureData] | [[CaptureData]],
      CaptureData :: {integer(), integer()}
                   | ListConversionData
                   | binary(),
      ListConversionData :: string()
                          | {error, string(), binary()}
                          | {incomplete, string(), binary()},
      ErrType :: match_limit | match_limit_recursion | {compile,  CompileErr}, 
      CompileErr :: {ErrString :: string(), Position :: non_neg_integer()},
      FirstCall :: boolean().

internal_run(_, _, _, _) ->
    erlang:nif_error(undef).

-doc """
Takes a compiled regular expression and an item, and returns the relevant data
from the regular expression.

The only supported item is `namelist`, which returns the tuple `{namelist, [binary()]}`,
containing the names of all (unique) named subpatterns in the regular expression.

For example:

```erlang
1> {ok,MP} = re:compile("(?<A>A)|(?<B>B)|(?<C>C)").
{ok,{re_pattern,3,0,0,
                <<69,82,67,80,119,0,0,0,0,0,0,0,1,0,0,0,255,255,255,255,
                  255,255,...>>}}
2> re:inspect(MP,namelist).
{namelist,[<<"A">>,<<"B">>,<<"C">>]}
3> {ok,MPD} = re:compile("(?<C>A)|(?<B>B)|(?<C>C)",[dupnames]).
{ok,{re_pattern,3,0,0,
                <<69,82,67,80,119,0,0,0,0,0,8,0,1,0,0,0,255,255,255,255,
                  255,255,...>>}}
4> re:inspect(MPD,namelist).
{namelist,[<<"B">>,<<"C">>]}
```

Notice in the second example that the duplicate name only occurs once in the
returned list, and that the list is in alphabetical order regardless of where
the names are positioned in the regular expression. The order of the names is
the same as the order of captured subexpressions if `{capture, all_names}` is
specified as an option to `run/3`. You can therefore create a name-to-value
mapping from the result of [`run/3`](`run/3`) like this:

```erlang
1> {ok,MP} = re:compile("(?<A>A)|(?<B>B)|(?<C>C)").
{ok,{re_pattern,3,0,0,
                <<69,82,67,80,119,0,0,0,0,0,0,0,1,0,0,0,255,255,255,255,
                  255,255,...>>}}
2> {namelist, N} = re:inspect(MP,namelist).
{namelist,[<<"A">>,<<"B">>,<<"C">>]}
3> {match,L} = re:run("AA",MP,[{capture,all_names,binary}]).
{match,[<<"A">>,<<>>,<<>>]}
4> NameMap = lists:zip(N,L).
[{<<"A">>,<<"A">>},{<<"B">>,<<>>},{<<"C">>,<<>>}]
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec inspect(MP,Item) -> {namelist, [ binary() ]} when
      MP :: mp(),
      Item :: namelist.

inspect(_,_) ->
    erlang:nif_error(undef).
    

%%% End of BIFs

-doc #{ equiv => split(Subject, RE, []) }.
-spec split(Subject, RE) -> SplitList when
      Subject :: iodata() | unicode:charlist(),
      RE :: mp() | iodata(),
      SplitList :: [iodata() | unicode:charlist()].

split(Subject,RE) ->
    try
        split(Subject,RE,[])
    catch
        error:_ ->
            badarg_with_info([Subject,RE])
    end.

-doc """
Splits the input into parts by finding tokens according to the regular
expression supplied.

The splitting is basically done by running a global regular
expression match and dividing the initial string wherever a match occurs. The
matching part of the string is removed from the output.

As in `run/3`, an `t:mp/0` compiled with option `unicode` requires `Subject` to
be a Unicode `charlist()`. If compilation is done implicitly and the `unicode`
compilation option is specified to this function, both the regular expression
and `Subject` are to be specified as valid Unicode `charlist()`s.

The result is given as a list of "strings", the preferred data type specified in
option `return` (default `iodata`).

If subexpressions are specified in the regular expression, the matching
subexpressions are returned in the resulting list as well. For example:

```erlang
re:split("Erlang","[ln]",[{return,list}]).
```

gives

```erlang
["Er","a","g"]
```

while

```erlang
re:split("Erlang","([ln])",[{return,list}]).
```

gives

```erlang
["Er","l","a","n","g"]
```

The text matching the subexpression (marked by the parentheses in the regular
expression) is inserted in the result list where it was found. This means that
concatenating the result of a split where the whole regular expression is a
single subexpression (as in the last example) always results in the original
string.

As there is no matching subexpression for the last part in the example (the
"g"), nothing is inserted after that. To make the group of strings and the parts
matching the subexpressions more obvious, one can use option `group`, which
groups together the part of the subject string with the parts matching the
subexpressions when the string was split:

```erlang
re:split("Erlang","([ln])",[{return,list},group]).
```

gives

```erlang
[["Er","l"],["a","n"],["g"]]
```

Here the regular expression first matched the "l", causing "Er" to be the first
part in the result. When the regular expression matched, the (only)
subexpression was bound to the "l", so the "l" is inserted in the group together
with "Er". The next match is of the "n", making "a" the next part to be
returned. As the subexpression is bound to substring "n" in this case, the "n"
is inserted into this group. The last group consists of the remaining string, as
no more matches are found.

By default, all parts of the string, including the empty strings, are returned
from the function, for example:

```erlang
re:split("Erlang","[lg]",[{return,list}]).
```

gives

```erlang
["Er","an",[]]
```

as the matching of the "g" in the end of the string leaves an empty rest, which
is also returned. This behavior differs from the default behavior of the split
function in Perl, where empty strings at the end are by default removed. To get
the "trimming" default behavior of Perl, specify `trim` as an option:

```erlang
re:split("Erlang","[lg]",[{return,list},trim]).
```

gives

```erlang
["Er","an"]
```

The "trim" option says; "give me as many parts as possible except the empty
ones", which sometimes can be useful. You can also specify how many parts you
want, by specifying `{parts,`N`}`:

```erlang
re:split("Erlang","[lg]",[{return,list},{parts,2}]).
```

gives

```erlang
["Er","ang"]
```

Notice that the last part is "ang", not "an", as splitting was specified into
two parts, and the splitting stops when enough parts are given, which is why the
result differs from that of `trim`.

More than three parts are not possible with this indata, so

```erlang
re:split("Erlang","[lg]",[{return,list},{parts,4}]).
```

gives the same result as the default, which is to be viewed as "an infinite
number of parts".

Specifying `0` as the number of parts gives the same effect as option `trim`. If
subexpressions are captured, empty subexpressions matched at the end are also
stripped from the result if `trim` or `{parts,0}` is specified.

The `trim` behavior corresponds exactly to the Perl default. `{parts,N}`, where
N is a positive integer, corresponds exactly to the Perl behavior with a
positive numerical third parameter. The default behavior of
[`split/3`](`split/3`) corresponds to the Perl behavior when a negative integer
is specified as the third parameter for the Perl routine.

Summary of options not previously described for function [`run/3`](`run/3`):

- **`{return,ReturnType}`** - Specifies how the parts of the original string are
  presented in the result list. Valid types:

  - **`iodata`** - The variant of `t:iodata/0` that gives the least copying of
    data with the current implementation (often a binary, but do not depend on
    it).

  - **`binary`** - All parts returned as binaries.

  - **`list`** - All parts returned as lists of characters ("strings").

- **`group`** - Groups together the part of the string with the parts of the
  string matching the subexpressions of the regular expression.

  The return value from the function is in this case a `t:list/0` of
  `t:list/0`s. Each sublist begins with the string picked out of the subject
  string, followed by the parts matching each of the subexpressions in order of
  occurrence in the regular expression.

- **`{parts,N}`** - Specifies the number of parts the subject string is to be
  split into.

  The number of parts is to be a positive integer for a specific maximum number
  of parts, and `infinity` for the maximum number of parts possible (the
  default). Specifying `{parts,0}` gives as many parts as possible disregarding
  empty parts at the end, the same as specifying `trim`.

- **`trim`** - Specifies that empty parts at the end of the result list are to
  be disregarded. The same as specifying `{parts,0}`. This corresponds to the
  default behavior of the `split` built-in function in Perl.
""".
-spec split(Subject, RE, Options) -> SplitList when
      Subject :: iodata() | unicode:charlist(),
      RE :: mp() | iodata() | unicode:charlist(),
      Options :: [ Option ],
      Option :: anchored | notbol | noteol | notempty | notempty_atstart
              | {offset, non_neg_integer()} | {newline, nl_spec()}
              | {match_limit, non_neg_integer()} 
              | {match_limit_recursion, non_neg_integer()}
              | bsr_anycrlf | bsr_unicode | {return, ReturnType}
              | {parts, NumParts} | group | trim | CompileOpt,
      NumParts :: non_neg_integer() | infinity,
      ReturnType :: iodata | list | binary,
      CompileOpt :: compile_option(),
      SplitList :: [RetData] | [GroupedRetData],
      GroupedRetData :: [RetData],
      RetData :: iodata() | unicode:charlist() | binary() | list().

split(Subject,RE,Options) ->
    try
    {NewOpt,Convert,Limit,Strip,Group} =
	process_split_params(Options,iodata,-1,false,false),
    Unicode = check_for_unicode(RE, Options),
    FlatSubject = to_binary(Subject, Unicode),
    case compile_split(RE,NewOpt) of
	{error,_Err} ->
	    throw(badre);
	{PreCompiled, NumSub, RunOpt} ->
	    %% OK, lets run
	    case re:run(FlatSubject,PreCompiled,RunOpt ++ [global]) of
		nomatch ->
		    case Group of
			true ->
			    convert_any_split_result([[FlatSubject]], 
						     Convert, Unicode, true);
			false ->
			    convert_any_split_result([FlatSubject], 
						     Convert, Unicode, false)
		    end;
		{match, Matches} ->
		    Res = do_split(FlatSubject, 0, Matches, NumSub, 
				   Limit, Group),
		    Stripped = case Strip of
				   true ->
				       backstrip_empty(Res,Group);
				   false ->
				       Res
			       end,
		    convert_any_split_result(Stripped, Convert, Unicode, Group)
	    end
    end
    catch
	throw:badopt ->
	    badarg_with_cause([Subject,RE,Options], badopt);
	throw:badre ->
	    badarg_with_info([Subject,RE,Options]);
	error:badarg ->
	    badarg_with_info([Subject,RE,Options])
    end.

backstrip_empty(List, false) ->
    do_backstrip_empty(List);
backstrip_empty(List, true) ->
    do_backstrip_empty_g(List).

do_backstrip_empty_g([]) ->
    [];
do_backstrip_empty_g([H]) ->
    case do_backstrip_empty(H) of
	[] ->
	    [];
	_ ->
	    [H]
    end;
do_backstrip_empty_g([H|T]) ->
    case do_backstrip_empty_g(T) of
	[] ->
	    case do_backstrip_empty(H) of
		[] ->
		    [];
		_ ->
		    [H]
	    end;
	Other ->
	    [H|Other]
    end.

do_backstrip_empty([]) ->
    [];
do_backstrip_empty([<<>>]) ->
    [];
do_backstrip_empty([<<>>|T]) ->
    case do_backstrip_empty(T) of
	[] ->
	    [];
	Other ->
	    [<<>>|Other]
    end;
do_backstrip_empty([H|T]) ->
    [H|do_backstrip_empty(T)].

convert_any_split_result(List,Type,Uni,true) ->
    [ convert_split_result(Part,Type,Uni) || Part <- List ];
convert_any_split_result(List,Type,Uni, false) ->
    convert_split_result(List,Type,Uni).

convert_split_result(List, iodata, _Unicode) ->
    List;
convert_split_result(List, binary, _Unicode) ->
    %% As it happens, the iodata is actually binaries
    List;
convert_split_result(List, list, true) ->
    [unicode:characters_to_list(Element,unicode) || Element <- List];
convert_split_result(List, list, false) ->
    [binary_to_list(Element) || Element <- List].

do_split(Subj, Off,  _, _, 0, false) ->
    <<_:Off/binary,Rest/binary>> = Subj,
    [Rest];
do_split(Subj, Off, [], _, _, false) ->
    <<_:Off/binary,Rest/binary>> = Subj,
    [Rest];
do_split(Subj, Off, _, _, _,false) when byte_size(Subj) =< Off ->
    [<<>>];
do_split(Subj, Off,  _, _, 0, true) ->
    <<_:Off/binary,Rest/binary>> = Subj,
    [[Rest]];
do_split(Subj, Off, [], _, _, true) ->
    <<_:Off/binary,Rest/binary>> = Subj,
    [[Rest]];
do_split(Subj, Off, _, _, _,true) when byte_size(Subj) =< Off ->
    [[<<>>]];
do_split(Subj, Offset, [[{MainI,MainL}|Sub]|T], NumSub, Limit, Group) ->
    NewOffset = MainI+MainL,
    KeptLen =  MainI - Offset,
    case {KeptLen,empty_sub(Sub),MainL} of
	{0,true,0} ->
	    do_split(Subj,NewOffset,T,NumSub,Limit,Group);
	_ ->
	    <<_:Offset/binary,Keep:KeptLen/binary,_/binary>> = Subj,
	    ESub = extend_subpatterns(Sub,NumSub),
	    Tail = do_split(Subj, NewOffset, T, NumSub, Limit - 1,Group),
	    case Group of
		false ->
		    [Keep | dig_subpatterns(Subj,lists:reverse(ESub),Tail)];
		true ->
		    [[Keep | dig_subpatterns(Subj,lists:reverse(ESub),[])]|
		     Tail]
	    end
    end.
empty_sub([]) ->
    true;
empty_sub([{_,0}|T]) ->
    empty_sub(T);
empty_sub(_) ->
    false.

dig_subpatterns(_,[],Acc) ->
    Acc;
dig_subpatterns(Subj,[{-1,0}|T],Acc) ->
    dig_subpatterns(Subj,T,[<<>>|Acc]);
dig_subpatterns(Subj,[{I,L}|T],Acc) ->
    <<_:I/binary,Part:L/binary,_/binary>> = Subj,
    dig_subpatterns(Subj,T,[Part|Acc]).

extend_subpatterns(_,0) ->
    [];
extend_subpatterns([],N) ->
    [{0,0} | extend_subpatterns([],N-1)];
extend_subpatterns([H|T],N) ->
    [H | extend_subpatterns(T,N-1)].

compile_split({re_pattern,N,_,_,_} = Comp, Options) ->
    {Comp,N,Options};
compile_split(Pat,Options0) when not is_tuple(Pat) ->
    Options = lists:filter(fun(O) ->
				   (not runopt(O))
			   end, Options0),
    case re:compile(Pat,Options) of
	{error,Err} ->
	    {error,Err};
	{ok, {re_pattern,N,_,_,_} = Comp} ->
	    NewOpt = lists:filter(fun(OO) -> (not copt(OO)) end, Options0),
	    {Comp,N,NewOpt}
    end;
compile_split(_,_) ->
    throw(badre).
    
-doc #{ equiv => replace(Subject, RE, Replacement, []) }.
-spec replace(Subject, RE, Replacement) -> iodata() | unicode:charlist() when
      Subject :: iodata() | unicode:charlist(),
      RE :: mp() | iodata(),
      Replacement :: iodata() | unicode:charlist() | replace_fun().

replace(Subject,RE,Replacement) ->
    try
        replace(Subject,RE,Replacement,[])
    catch
        error:_ ->
            badarg_with_info([Subject,RE,Replacement])
    end.

-doc """
Replaces the matched part of the `Subject` string with `Replacement`.

The permissible options are the same as for `run/3`, except that
option` capture` is not allowed. Instead a `{return, ReturnType}` is present.
The default return type is `iodata`, constructed in a way to minimize copying.
The `iodata` result can be used directly in many I/O operations. If a flat
`t:list/0` is desired, specify `{return, list}`. If a binary is desired, specify
`{return, binary}`.

As in function [`run/3`](`run/3`), an `t:mp/0` compiled with option `unicode`
requires `Subject` to be a Unicode `charlist()`. If compilation is done
implicitly and the `unicode` compilation option is specified to this function,
both the regular expression and `Subject` are to specified as valid Unicode
`charlist()`s.

If the replacement is given as a string, it can contain the special character
`&`, which inserts the whole matching expression in the result, and the special
sequence `\`N (where N is an integer > 0), `\g`N, or `\g{`N`}`, resulting in the
subexpression number N, is inserted in the result. If no subexpression with that
number is generated by the regular expression, nothing is inserted.

To insert an & or a \\ in the result, precede it with a \\. Notice that Erlang
already gives a special meaning to \\ in literal strings, so a single \\ must be
written as `"\\"` and therefore a double \\ as `"\\\\"`.

_Example:_

```erlang
1> re:replace("abcd","c","[&]",[{return,list}]).
"ab[c]d"
```

while

```erlang
2> re:replace("abcd","c","[\\&]",[{return,list}]).
"ab[&]d"
```

If the replacement is given as a fun, it will be called with the whole matching
expression as the first argument and a list of subexpression matches in the
order in which they appear in the regular expression. The returned value will be
inserted in the result.

_Example:_

```erlang
3> re:replace("abcd", ".(.)",
    fun(Whole, [<<C>>]) ->
         <<$#, Whole/binary, $-, (C - $a + $A), $#>>
    end,
    [{return, list}]).
"#ab-B#cd"
```

> #### Note {: .info }
>
> Non-matching optional subexpressions will not be included in the list of
> subexpression matches if they are the last subexpressions in the regular
> expression.
>
> _Example:_
>
> The regular expression `"(a)(b)?(c)?"` ("a", optionally followed by "b",
> optionally followed by "c") will create the following subexpression lists:
>
> - `[<<"a">>, <<"b">>, <<"c">>]` when applied to the string `"abc"`
> - `[<<"a">>, <<>>, <<"c">>]` when applied to the string `"acx"`
> - `[<<"a">>, <<"b">>]` when applied to the string `"abx"`
> - `[<<"a">>]` when applied to the string `"axx"`

As with [`run/3`](`run/3`), compilation errors raise the `badarg` exception.
`compile/2` can be used to get more information about the error.
""".
-spec replace(Subject, RE, Replacement, Options) -> iodata() | unicode:charlist() when
      Subject :: iodata() | unicode:charlist(),
      RE :: mp() | iodata() | unicode:charlist(),
      Replacement :: iodata() | unicode:charlist() | replace_fun(),
      Options :: [Option],
      Option :: anchored | global | notbol | noteol | notempty 
	      | notempty_atstart
              | {offset, non_neg_integer()} | {newline, NLSpec} | bsr_anycrlf
              | {match_limit, non_neg_integer()} 
              | {match_limit_recursion, non_neg_integer()}
              | bsr_unicode | {return, ReturnType} | CompileOpt,
      ReturnType :: iodata | list | binary,
      CompileOpt :: compile_option(),
      NLSpec :: cr | crlf | lf | anycrlf | any.

replace(Subject,RE,Replacement,Options) ->
    try
	{NewOpt,Convert} = process_repl_params(Options,iodata),
	Unicode = check_for_unicode(RE, Options),
	FlatSubject = to_binary(Subject, Unicode),
	Replacement1 = normalize_replacement(Replacement, Unicode),
	IoList = do_replace(FlatSubject,Subject,RE,Replacement1,NewOpt),
	case Convert of
	    iodata ->
		IoList;
	    binary ->
		case Unicode of
		    false ->
			iolist_to_binary(IoList);
		    true ->
			unicode:characters_to_binary(IoList,unicode)
		end;
	    list ->
		case Unicode of
		    false ->
			binary_to_list(iolist_to_binary(IoList));
		    true ->
			unicode:characters_to_list(IoList,unicode)
		end
	end
    catch
	throw:badopt ->
	    badarg_with_cause([Subject,RE,Replacement,Options], badopt);
	throw:badre ->
	    badarg_with_info([Subject,RE,Replacement,Options]);
	error:badarg ->
	    badarg_with_info([Subject,RE,Replacement,Options])
    end.

normalize_replacement(Replacement, _Unicode) when is_function(Replacement, 2) ->
    Replacement;
normalize_replacement(Replacement, Unicode) ->
    to_binary(Replacement, Unicode).

do_replace(FlatSubject,Subject,RE,Replacement,Options) ->
    case re:run(FlatSubject,RE,Options) of
	nomatch ->
	    Subject;
	{match,[Mlist|T]} when is_list(Mlist) ->
	    apply_mlist(FlatSubject,Replacement,[Mlist|T]);
	{match,Slist} ->
	    apply_mlist(FlatSubject,Replacement,[Slist])
    end.

process_repl_params([],Convert) ->
    {[],Convert};
process_repl_params([report_errors|_],_) ->
    throw(badopt);
process_repl_params([{capture,_,_}|_],_) ->
    throw(badopt);
process_repl_params([{capture,_}|_],_) ->
    throw(badopt);
process_repl_params([{return,iodata}|T],_C) ->
    process_repl_params(T,iodata);
process_repl_params([{return,list}|T],_C) ->
    process_repl_params(T,list);
process_repl_params([{return,binary}|T],_C) ->
    process_repl_params(T,binary);
process_repl_params([{return,_}|_],_) ->
    throw(badopt);
process_repl_params([H|T],C) ->
    {NT,NC} = process_repl_params(T,C),
    {[H|NT],NC};
process_repl_params(_,_) ->
    throw(badopt).

process_split_params([],Convert,Limit,Strip,Group) ->
    {[],Convert,Limit,Strip,Group};
process_split_params([trim|T],C,_L,_S,G) ->
    process_split_params(T,C,-1,true,G); 
process_split_params([{parts,0}|T],C,_L,_S,G) ->
    process_split_params(T,C,-1,true,G); 
process_split_params([{parts,N}|T],C,_L,_S,G) when is_integer(N), N >= 1 ->
    process_split_params(T,C,N-1,false,G); 
process_split_params([{parts,infinity}|T],C,_L,_S,G) ->
    process_split_params(T,C,-1,false,G); 
process_split_params([{parts,_}|_],_,_,_,_) ->
    throw(badopt); 
process_split_params([group|T],C,L,S,_G) ->
    process_split_params(T,C,L,S,true); 
process_split_params([global|_],_,_,_,_) ->
    throw(badopt);
process_split_params([report_errors|_],_,_,_,_) ->
    throw(badopt);
process_split_params([{capture,_,_}|_],_,_,_,_) ->
    throw(badopt);
process_split_params([{capture,_}|_],_,_,_,_) ->
    throw(badopt);
process_split_params([{return,iodata}|T],_C,L,S,G) ->
    process_split_params(T,iodata,L,S,G);
process_split_params([{return,list}|T],_C,L,S,G) ->
    process_split_params(T,list,L,S,G);
process_split_params([{return,binary}|T],_C,L,S,G) ->
    process_split_params(T,binary,L,S,G);
process_split_params([{return,_}|_],_,_,_,_) ->
    throw(badopt);
process_split_params([H|T],C,L,S,G) ->
    {NT,NC,NL,NS,NG} = process_split_params(T,C,L,S,G),
    {[H|NT],NC,NL,NS,NG};
process_split_params(_,_,_,_,_) ->
    throw(badopt).

apply_mlist(Subject,Replacement,Mlist) ->
    do_mlist(Subject,Subject,0,precomp_repl(Replacement), Mlist).


precomp_repl(<<>>) ->
    [];
precomp_repl(<<$\\,$g,${,Rest/binary>>) when byte_size(Rest) > 0 ->
    {NS, <<$},NRest/binary>>} = pick_int(Rest),
    [list_to_integer(NS) | precomp_repl(NRest)];
precomp_repl(<<$\\,$g,Rest/binary>>) when byte_size(Rest) > 0 ->
    {NS,NRest} = pick_int(Rest),
    [list_to_integer(NS) | precomp_repl(NRest)];
precomp_repl(<<$\\,X,Rest/binary>>) when X < $1 ; X > $9 ->
    %% Escaped character
    case precomp_repl(Rest) of
	[BHead | T0] when is_binary(BHead) ->
	    [<<X,BHead/binary>> | T0];
	Other ->
	    [<<X>> | Other]
    end;
precomp_repl(<<$\\,Rest/binary>>) when byte_size(Rest) > 0->
    {NS,NRest} = pick_int(Rest),
    [list_to_integer(NS) | precomp_repl(NRest)];
precomp_repl(<<$&,Rest/binary>>) ->
    [0 | precomp_repl(Rest)];
precomp_repl(<<X,Rest/binary>>) ->
    case precomp_repl(Rest) of
	[BHead | T0] when is_binary(BHead) ->
	    [<<X,BHead/binary>> | T0];
	Other ->
	    [<<X>> | Other]
    end;
precomp_repl(Repl) when is_function(Repl) ->
    Repl.
    


pick_int(<<X,R/binary>>) when X >= $0, X =< $9 ->
    {Found,Rest} = pick_int(R),
    {[X|Found],Rest};
pick_int(Bin) ->
    {[],Bin}.

do_mlist(_,<<>>,_,_,[]) ->
    []; %Avoid empty binary tail
do_mlist(_,Subject,_,_,[]) ->
    Subject;
do_mlist(Whole,Subject,Pos,Repl,[[{MPos,Count} | Sub] | Tail]) 
  when MPos > Pos ->
    EatLength = MPos - Pos,
    <<Untouched:EatLength/binary, Rest/binary>> = Subject,
    [Untouched | do_mlist(Whole,Rest, MPos, Repl, 
			  [[{MPos,Count} | Sub] | Tail])];
do_mlist(Whole,Subject,Pos,Repl,[[{MPos,Count} | Sub] | Tail]) 
  when MPos =:= Pos ->
    EatLength = Count,
    <<_:EatLength/binary,Rest/binary>> = Subject,
    NewData = do_replace(Whole,Repl,[{MPos,Count} | Sub]),
    [NewData | do_mlist(Whole,Rest,Pos+EatLength,Repl,Tail)].


do_replace(Subject, Repl, SubExprs0) when is_function(Repl) ->
    All = binary:part(Subject, hd(SubExprs0)),
    SubExprs1 =
        [ if
              Pos >= 0, Len > 0 ->
                  binary:part(Subject, Pos, Len);
              true ->
                  <<>>
          end || {Pos, Len} <- tl(SubExprs0) ],
    Repl(All, SubExprs1);
do_replace(_,[Bin],_) when is_binary(Bin) ->
    Bin;
do_replace(Subject,Repl,SubExprs0) ->
    SubExprs = list_to_tuple(SubExprs0),
    [ case Part of
	  N when is_integer(N) ->
	      if
		  tuple_size(SubExprs) =< N ->
		      <<>>;
		  true ->
		      {SPos,SLen} = element(N+1,SubExprs),
		      if 
			  SPos < 0 ->
			      <<>>;
			  true ->
			      <<_:SPos/binary,Res:SLen/binary,_/binary>> = 
				  Subject,
			      Res
		      end
	      end;
	  Other ->
	      Other
      end || Part <- Repl ].


check_for_unicode({re_pattern,_,1,_,_},_) ->
    true;
check_for_unicode({re_pattern,_,0,_,_},_) ->
    false;
check_for_unicode(_,L) ->
    lists:member(unicode,L).

check_for_crlf({re_pattern,_,_,1,_},_) ->
    true;
check_for_crlf({re_pattern,_,_,0,_},_) ->
    false;
check_for_crlf(_,L) ->
    case lists:keysearch(newline,1,L) of
	{value,{newline,any}} -> true;
	{value,{newline,crlf}} -> true;
	{value,{newline,anycrlf}} -> true;
	_ -> false
    end.
    
% SelectReturn = false | all | stirpfirst | none 
% ConvertReturn = index | list | binary
% {capture, all} -> all (untouchded)
% {capture, all_names} -> if names are present: treated as a name {capture, [...]} 
%                                      else:    same as {capture, []}
% {capture, first} -> kept in argument list and Select all
% {capture, all_but_first} -> removed from argument list and selects stripfirst
% {capture, none} ->  removed from argument list and selects none
% {capture, []} -> removed from argument list and selects none
% {capture,[...]} -> 0 added to selection list and selects stripfirst
% SelectReturn false is same as all in the end.

% Call as process_parameters([],0,false,index,NeedClean)

process_parameters([],InitialOffset, SelectReturn, ConvertReturn,_,_) ->
    {[], InitialOffset, SelectReturn, ConvertReturn};
process_parameters([{offset, N} | T],_Init0,Select0,Return0,CC,RE) ->
    process_parameters(T,N,Select0,Return0,CC,RE);
process_parameters([global | T],Init0,Select0,Return0,CC,RE) ->
    process_parameters(T,Init0,Select0,Return0,CC,RE);
process_parameters([{capture,Values,Type}|T],Init0,Select0,_Return0,CC,RE) ->
    process_parameters([{capture,Values}|T],Init0,Select0,Type,CC,RE);
process_parameters([{capture,Values}|T],Init0,Select0,Return0,CC,RE) ->
    % First process the rest to see if capture was already present
    {NewTail, Init1, Select1, Return1} = 
	process_parameters(T,Init0,Select0,Return0,CC,RE),
    case Select1 of
	false ->
	    case Values of
		all ->
		    {[{capture,all} | NewTail], Init1, all, Return0}; 
		all_names ->
		    case re:inspect(RE,namelist) of
			{namelist, []} ->
			    {[{capture,first} | NewTail], Init1, none, Return0};
			{namelist, List} ->
			    {[{capture,[0|List]} | NewTail], Init1, stripfirst, Return0}
		    end; 
		first ->
		    {[{capture,first} | NewTail], Init1, all, Return0};
		all_but_first ->
		    {[{capture,all} | NewTail], Init1, stripfirst, Return0};
		none ->
		    {[{capture,first} | NewTail], Init1, none, Return0};
		[] ->
		    {[{capture,first} | NewTail], Init1, none, Return0};
		List when is_list(List) ->
		    {[{capture,[0|List]} | NewTail], 
		     Init1, stripfirst, Return0};
		_ ->
		    throw(badlist)
	    end;
	_ ->
	    % Found overriding further down list, ignore this one
	    {NewTail, Init1, Select1, Return1}
    end;
process_parameters([H|T],Init0,Select0,Return0,true,RE) ->
    case copt(H) of
	true ->
	    process_parameters(T,Init0,Select0,Return0,true,RE);
	false ->
	    {NewT,Init,Select,Return} =
		process_parameters(T,Init0,Select0,Return0,true,RE),	
	    {[H|NewT],Init,Select,Return}
    end;
process_parameters([H|T],Init0,Select0,Return0,false,RE) ->
    {NewT,Init,Select,Return} =
		process_parameters(T,Init0,Select0,Return0,false,RE),
    {[H|NewT],Init,Select,Return};
process_parameters(_,_,_,_,_,_) ->
    throw(badlist).

postprocess({match,[]},_,_,_,_) ->
    nomatch;
postprocess({match,_},none,_,_,_) ->
    match;
postprocess({match,M},Any,binary,Flat,Uni) ->
    binarify(postprocess({match,M},Any,index,Flat,Uni),Flat);
postprocess({match,M},Any,list,Flat,Uni) ->
    listify(postprocess({match,M},Any,index,Flat,Uni),Flat,Uni);
postprocess({match,M},all,index,_,_) ->
    {match,M};
postprocess({match,M},false,index,_,_) ->
    {match,M};
postprocess({match,M},stripfirst,index,_,_) ->
    {match, [ T || [_|T] <- M ]}.

binarify({match,M},Flat) ->
    {match, [ [ case {I,L} of
		    {-1,0} ->
			<<>>;
		    {SPos,SLen} ->
			<<_:SPos/binary,Res:SLen/binary,_/binary>> = Flat,
			Res
		end || {I,L} <- One ] || One <- M ]}.
listify({match,M},Flat,Uni) ->
    {match, [ [ case {I,L} of
	    {_,0} ->
		[];
	    {SPos,SLen} ->
		case Uni of
		    true ->
			<<_:SPos/binary,Res:SLen/binary,_/binary>> = Flat,
			unicode:characters_to_list(Res,unicode);
		    false ->
			Start = SPos + 1,
			End = SPos + SLen,
			binary_to_list(Flat,Start,End)
		end
	end || {I,L} <- One ] || One <- M ]}.

ubinarify({match,M},Flat) ->
    {match, [ case {I,L} of
		  {-1,0} ->
		      <<>>;
		  {SPos,SLen} ->
		      <<_:SPos/binary,Res:SLen/binary,_/binary>> = Flat,
		      Res
		end || {I,L} <- M ]};
ubinarify(Else,_) ->
    Else.
ulistify({match,M},Flat) ->
    {match, [ case {I,L} of
	    {_,0} ->
		[];
	    {SPos,SLen} ->
		      <<_:SPos/binary,Res:SLen/binary,_/binary>> = Flat,
		      unicode:characters_to_list(Res,unicode)
	      end || {I,L} <- M ]};
ulistify(Else,_) ->
    Else.

process_uparams([global|_T],_RetType) ->
    throw(false);
process_uparams([{capture,Values,Type}|T],_OldType) ->
    process_uparams([{capture,Values}|T],Type);
process_uparams([H|T],Type) ->
    {NL,NType} = process_uparams(T,Type),
    {[H|NL],NType};
process_uparams([],Type) ->
    {[],Type}.


-doc false.
ucompile(RE,Options) ->
    try
	re:compile(unicode:characters_to_binary(RE,unicode),Options)
    catch
	error:AnyError ->
	    {'EXIT',{new_stacktrace,[{Mod,_,L,Loc}|Rest]}} =
		(catch erlang:error(new_stacktrace,
				    [RE,Options])),
	    erlang:raise(error,AnyError,[{Mod,compile,L,Loc}|Rest])
    end.
	

-doc false.
urun(Subject,RE,Options) ->
    try
	urun2(Subject,RE,Options)
    catch
	error:AnyError ->
	    {'EXIT',{new_stacktrace,[{Mod,_,L,Loc}|Rest]}} =
		(catch erlang:error(new_stacktrace,
				    [Subject,RE,Options])),
	    erlang:raise(error,AnyError,[{Mod,run,L,Loc}|Rest])
    end.

urun2(Subject0,RE0,Options0) ->
    {Options,RetType} = case (catch process_uparams(Options0,index)) of
			    {A,B} ->
				{A,B};
			    _ ->
				{Options0,false}
			end,
    Subject = unicode:characters_to_binary(Subject0,unicode),
    RE = case RE0 of
	     BinRE when is_binary(BinRE) ->
		 BinRE;
	     {re_pattern,_,_,_,_} = ReCompiled ->
		 ReCompiled;
	     ListRE ->
		 unicode:characters_to_binary(ListRE,unicode)
	 end,
    Ret = re:run(Subject,RE,Options),
    case RetType of
	binary ->
	    ubinarify(Ret,Subject);
	list ->
	    ulistify(Ret,Subject);
	_ ->
	    Ret
    end.
	

%% Might be called either with two-tuple (if regexp was already compiled)
%% or with 3-tuple (saving original RE for exceptions
-doc false.
grun(Subject,RE,{Options,NeedClean}) ->
    try
	grun2(Subject,RE,{Options,NeedClean})
    catch
	error:AnyError ->
	    {'EXIT',{new_stacktrace,[{Mod,_,L,Loc}|Rest]}} =
		(catch erlang:error(new_stacktrace,
				    [Subject,RE,Options])),
	    erlang:raise(error,AnyError,[{Mod,run,L,Loc}|Rest])
    end;
grun(Subject,RE,{Options,NeedClean,OrigRE}) ->
    try
	grun2(Subject,RE,{Options,NeedClean})
    catch
	error:AnyError ->
	    {'EXIT',{new_stacktrace,[{Mod,_,L,Loc}|Rest]}} =
		(catch erlang:error(new_stacktrace,
				    [Subject,OrigRE,Options])),
	    erlang:raise(error,AnyError,[{Mod,run,L,Loc}|Rest])
    end.

grun2(Subject,RE,{Options,NeedClean}) ->
    Unicode = check_for_unicode(RE,Options),
    CRLF = check_for_crlf(RE,Options),
    FlatSubject = to_binary(Subject, Unicode),
    do_grun(FlatSubject,Subject,Unicode,CRLF,RE,{Options,NeedClean}).

do_grun(FlatSubject,Subject,Unicode,CRLF,RE,{Options0,NeedClean}) ->
    {StrippedOptions, InitialOffset,
     SelectReturn, ConvertReturn} = 
	case (catch 
		  process_parameters(Options0, 0, false, index, NeedClean,RE)) of
	    badlist ->
		erlang:error(badarg,[Subject,RE,Options0]);
	    CorrectReturn ->
		CorrectReturn
	end,
    try
	postprocess(loopexec(FlatSubject,RE,InitialOffset,
			     byte_size(FlatSubject),
			     Unicode,CRLF,StrippedOptions,true),
		    SelectReturn,ConvertReturn,FlatSubject,Unicode)
    catch
	throw:ErrTuple ->
	    ErrTuple
    end.

loopexec(_,_,X,Y,_,_,_,_) when X > Y ->
    {match,[]};
loopexec(Subject,RE,X,Y,Unicode,CRLF,Options, First) ->
    case re:internal_run(Subject,RE,[{offset,X}]++Options,First) of
	{error, Err} ->
	    throw({error,Err});
	nomatch ->
	    {match,[]};
	{match,[{A,B}|More]} ->
	    {match,Rest} = 
		case B>0 of
		    true ->
			loopexec(Subject,RE,A+B,Y,Unicode,CRLF,Options,false);
		    false ->
			{match,M} = 
			    case re:internal_run(Subject,RE,[{offset,X},notempty_atstart,
                                                             anchored]++Options,false) of
				nomatch ->
				    {match,[]};
				{match,Other} ->
				    {match,Other}
			    end,
			NewA = case M of 
				   [{_,NStep}|_] when NStep > 0 ->
				       A+NStep;
				   _ ->
				       forward(Subject,A,1,Unicode,CRLF)
			       end,
			{match,MM} = loopexec(Subject,RE,NewA,Y,
					      Unicode,CRLF,Options,false),
			case M of 
			    [] ->
				{match,MM};
			    _ ->
				{match,[M | MM]}
			end
		end,
	    {match,[[{A,B}|More] | Rest]}
    end.
    
forward(_Chal,A,0,_,_) ->
    A;
forward(Chal,A,N,U,true) ->
    <<_:A/binary,Tl/binary>> = Chal,
    case Tl of
	<<$\r,$\n,_/binary>> ->
	    forward(Chal,A+2,N-1,U,true);
	_ -> 
	    forward2(Chal,A,N,U,true)
    end;
forward(Chal,A,N,U,false) ->
    forward2(Chal,A,N,U,false).

forward2(Chal,A,N,false,CRLF) ->
    forward(Chal,A+1,N-1,false,CRLF);
forward2(Chal,A,N,true,CRLF) ->
    <<_:A/binary,Tl/binary>> = Chal,
    Forw = case Tl of
	       <<1:1,1:1,0:1,_:5,_/binary>>  ->
		   2;
	       <<1:1,1:1,1:1,0:1,_:4,_/binary>>  ->
		   3;
	       <<1:1,1:1,1:1,1:1,0:1,_:3,_/binary>>  ->
		   4;
	       _ ->
		   1
	   end,
    forward(Chal,A+Forw,N-1,true,CRLF).

copt(caseless) ->
    true;
copt(no_start_optimize) ->
    true;
copt(never_utf) ->
    true;
copt(ucp) ->
    true;
copt(dollar_endonly) ->
    true;
copt(dotall) ->
    true;
copt(extended) ->
    true;
copt(firstline) ->
    true;
copt(multiline) ->
    true;
copt(no_auto_capture) ->
    true;
copt(dupnames) ->
    true;
copt(ungreedy) ->
    true;
copt(unicode) ->
    true;
copt(_) ->
    false.

%bothopt({newline,_}) ->
%    true;
%bothopt(anchored) ->
%    true;
%bothopt(_) ->
%    false.

runopt(notempty) ->
    true;
runopt(notempty_atstart) ->
    true;
runopt(notbol) ->
    true;
runopt(noteol) ->
    true;
runopt({offset,_}) ->
    true;
runopt({capture,_,_}) ->
    true;
runopt({capture,_}) ->
    true;
runopt(global) ->
    true;
runopt({match_limit,_}) ->
    true;
runopt({match_limit_recursion,_}) ->
    true;
runopt(_) ->
    false.

to_binary(Bin, _IsUnicode) when is_binary(Bin) ->
    Bin;
to_binary(Data, true) ->
    unicode:characters_to_binary(Data,unicode);
to_binary(Data, false) ->
    iolist_to_binary(Data).

badarg_with_cause(Args, Cause) ->
    erlang:error(badarg, Args, [{error_info, #{module => erl_stdlib_errors,
                                               cause => Cause}}]).

badarg_with_info(Args) ->
    erlang:error(badarg, Args, [{error_info, #{module => erl_stdlib_errors}}]).
