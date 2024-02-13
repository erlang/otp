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
# Upcoming Potential Incompatibilities

## Introduction

This document lists planned upcoming potential incompatibilities in Erlang/OTP.

## OTP 27

### Fun creator pid will always be local init process

As of OTP 27, the functions [`erlang:fun_info/1,2`](`erlang:fun_info/1`) will
always say that the local `init` process created all funs, regardless of which
process or node the fun was originally created on.

In OTP 28, the `{pid,_}`element will be removed altogether.

[](){: #maybe_expr }

### Feature maybe_expr will be enabled by default

As of OTP 27, the `maybe_expr` feature will be approved and enabled by default.
That means that code that uses the unquoted atom `maybe` will fail to compile.
All uses of `maybe` as an atom will need to be quoted. Alternatively, as a
short-term solution, the `maybe_expr` feature can be disabled.

It is recommend to quote all uses of the atom `maybe` as soon as possible. The
compiler option `warn_keywords` can be used to emit warnings about all
occurrences of `maybe` without quotes.

[](){: #new_re_engine }

### The re module will use a different regular expression engine

The functionality of module `m:re` is currently provided by the PCRE library,
which is no longer actively maintained. Therefore, in OTP 27, we will switch to
a different regular expression library.

The source code for PCRE used by the `re` module has been modified by the OTP
team to ensure that a regular expression match would yield when matching huge
input binaries and/or when using demanding (back-tracking) regular expressions.
Because of the those modifications, moving to a new version of PCRE has always
been a time-consuming process because all of the modifications had to be applied
by hand again to the updated PCRE source code.

Most likely, the new regular expression library will be
[RE2](https://github.com/google/re2). RE2 guarantees that the match time is
linear in the length of input string, and it also eschews recursion to avoid
stack overflow. That should make it possible to use RE2 without modifying its
source code. For more information about why RE2 is a good choice, see
[WhyRE2](https://github.com/google/re2/wiki/WhyRE2).

Some of implications of this change are:

- We expect that the functions in the `re` module will continue to be supported,
  although some of the options are likely to be dis-continued.
- It is likely that only pattern matching of UTF8-encoded binaries will be
  supported (not Latin1-encoded binaries).
- In order to guarantee the linear-time performance, RE2 does not support all
  the constructs in regular expression patterns that PCRE do. For example,
  backreferences and look-around assertions are not supported. See
  [Syntax](https://github.com/google/re2/wiki/Syntax) for a description of what
  RE2 supports.
- Compiling a regular expression is likely to be slower, and thus more can be
  gained by explicitly compiling the regular expression before matching with it.

[](){: #float_matching }

### 0\.0 and -0.0 will no longer be exactly equal

Currently, the floating point numbers `0.0` and `-0.0` have distinct internal
representations. That can be seen if they are converted to binaries:

```erlang
1> <<0.0/float>>.
<<0,0,0,0,0,0,0,0>>
2> <<-0.0/float>>.
<<128,0,0,0,0,0,0,0>>
```

However, when they are matched against each other or compared using the `=:=`
operator, they are considered to be equal. Thus, `0.0 =:= -0.0` currently
returns `true`.

In Erlang/OTP 27, `0.0 =:= -0.0` will return `false`, and matching `0.0` against
`-0.0` will fail. When used as map keys, `0.0` and `-0.0` will be considered to
be distinct.

The `==` operator will continue to return `true` for `0.0 == -0.0`.

To help to find code that might need to be revised, in OTP 27 there will be a
new compiler warning when matching against `0.0` or comparing to that value
using the `=:=` operator. The warning can be suppressed by matching against
`+0.0` instead of `0.0`.

We plan to introduce the same warning in OTP 26.1, but by default it will be
disabled.

[](){: #singleton_typevars }

### Singleton type variables will become a compile-time error

Before Erlang/OTP 26, the compiler would silenty accept the following spec:

```erlang
-spec f(Opts) -> term() when
    Opts :: {ok, Unknown} | {error, Unknown}.
f(_) -> error.
```

In OTP 26, the compiler emits a warning pointing out that the type variable
`Unknown` is unbound:

```erlang
t.erl:6:18: Warning: type variable 'Unknown' is only used once (is unbound)
%    6|     Opts :: {ok, Unknown} | {error, Unknown}.
%     |                  ^
```

In OTP 27, that warning will become an error.

[](){: #escripts_will_be_compiled }

### Escripts will be compiled by default

Escripts will be compiled by default instead of interpreted. That means that the
`compiler` application must be available.

The old behavior of interpreting escripts can be restored by adding the
following line to the script file:

```erlang
-mode(interpret).
```

In OTP 28, support for interpreting an escript will be removed.

### \-code_path_choice will default to strict

This command line option controls if paths given in the command line, boot
scripts, and the code server should be interpreted as is strict or relaxed.

OTP 26 and earlier defaults to `relaxed`, which means `-pa myapp/ebin` would
attempt to load `-pa myapp/ebin` and `-pa myapp/myapp/ebin`. The option will
default to strict in OTP 27.

### Archive fallbacks will be removed

OTP 26 and earlier allows an application to have part of its directories as
regular folders and others as archives. This functionality was previously used
by reltool but it is no longer the case from OTP 26. Support for archive
fallbacks will be removed from the code server in OTP 27.

[](){: #triple_quoted_strings }

### Triple-Quoted Strings

Before Erlang/OTP 27 a sequence of 3 or more double-quote characters was grouped
in pairs each meaning the empty string and if there was an odd number the last
character was the start of a string. The empty strings were then concatenated
and effectively disappeared.

In Erlang/OTP 27; 3 or more double-quote characters are interpreted as the start
of a "Triple-Quoted String". See [EEP 64](https://www.erlang.org/eeps/eep-0064).

Here follows some examples of code that would change meaning. Note that all
these examples before Erlang/OTP 27.0 was strange since there was no sensible
reason to write like that.

```erlang
"""String Content"""
%% Was interpreted as
"" "String Content" ""
%% Which becomes
"String Content"
%%
%% In OTP 27 it is instead a syntax error since no text is allowed
%% on the line after an opening triple-quote
```

```text
"""
String Content
"""
%% Was interpreted as
"" "
String Content
" ""
%% Which becomes
"
String Content
"
%%
%% In OTP 27 it is instead interpreted as a
%% Triple-Quoted String equivalent to
"String Content"
```

```erlang
""""
++ foo() ++
""""
%% Became
"" ++ foo() ++ ""
%%
%% In OTP 27 it is instead interpreted as a
%% Triple-Quoted String (triple-or-more) equivalent to
"++ foo() ++"
```

From Erlang/OTP 26.1 up to 27.0 the compiler issues a warning for a sequence of
3 or more double-quote characters since that is almost certainly a mistake or
something like a result of bad automatic code generation. If a users gets that
warning, the code should be corrected for example by inserting appropriate
spaces between the empty strings, or removing the redundant ones alltogether,
which will have the same meaning before and after Erlang/OTP 27.

## OTP 28

[](){: #fun_creator_pid }

### Fun creator pid will be removed

As of OTP 28, the function `erlang:fun_info/1` will not include the `{pid,_}`
element and the function `erlang:fun_info/2` will no longer accept `pid` as the
second argument.

[](){: #escript_interpret_mode_removed }

### Support for interpreting escripts will be removed

Escripts will be compiled, and it will no longer be possible to force an escript
to be interpreted by using the directive `-mode(interpret)`.

## OTP 29

### It will no longer be possible to disable feature maybe_expr

As of OTP 29, the `maybe_expr` feature will become permanent and no longer
possible to disable. All uses of `maybe` as an atom will need to be quoted.

It is recommend to quote all uses of the atom `maybe` as soon as possible. The
compiler option `warn_keywords` can be used to emit warnings about all
occurrences of `maybe` without quotes.

### cprof and eprof will be replaced by tprof

As of OTP 29, the `cprof` and `eprof` will be removed in favor of `m:tprof`
added in OTP 27.
