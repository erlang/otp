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
# Preprocessor

## File Inclusion

A file can be included as follows:

```erlang
-include(File).
-include_lib(File).
```

`File`, a string, is to point out a file. The contents of this file are included
as is, at the position of the directive.

Include files are typically used for record and macro definitions that are
shared by several modules. It is recommended to use the file name extension
`.hrl` for include files.

`File` can start with a path component `$VAR`, for some string `VAR`. If that is
the case, the value of the environment variable `VAR` as returned by
`os:getenv(VAR)` is substituted for `$VAR`. If `os:getenv(VAR)` returns `false`,
`$VAR` is left as is.

If the filename `File` is absolute (possibly after variable substitution), the
include file with that name is included. Otherwise, the specified file is
searched for in the following directories, and in this order:

1. The current working directory
1. The directory where the module is being compiled
1. The directories given by the `include` option

For details, see the [erlc(1)](`e:erts:erlc_cmd.md`) manual page in ERTS and
`m:compile` manual page in Compiler.

_Examples:_

```erlang
-include("my_records.hrl").
-include("incdir/my_records.hrl").
-include("/home/user/proj/my_records.hrl").
-include("$PROJ_ROOT/my_records.hrl").
```

`include_lib` is similar to `include`, but is not to point out an absolute file.
Instead, the first path component (possibly after variable substitution) is
assumed to be the name of an application.

_Example:_

```erlang
-include_lib("kernel/include/file.hrl").
```

The code server uses `code:lib_dir(kernel)` to find the directory of the current
(latest) version of Kernel, and then the subdirectory `include` is searched for
the file `file.hrl`.

## Defining and Using Macros

A macro is defined as follows:

```erlang
-define(Const, Replacement).
-define(Func(Var1,...,VarN), Replacement).
```

A macro definition can be placed anywhere among the attributes and function
declarations of a module, but the definition must come before any usage of the
macro.

If a macro is used in several modules, it is recommended that the macro
definition is placed in an include file.

A macro is used as follows:

```text
?Const
?Func(Arg1,...,ArgN)
```

Macros are expanded during compilation. A simple macro `?Const` is replaced with
`Replacement`.

_Example:_

```erlang
-define(TIMEOUT, 200).
...
call(Request) ->
    server:call(refserver, Request, ?TIMEOUT).
```

This is expanded to:

```erlang
call(Request) ->
    server:call(refserver, Request, 200).
```

A macro `?Func(Arg1,...,ArgN)` is replaced with `Replacement`, where all
occurrences of a variable `Var` from the macro definition are replaced with the
corresponding argument `Arg`.

_Example:_

```erlang
-define(MACRO1(X, Y), {a, X, b, Y}).
...
bar(X) ->
    ?MACRO1(a, b),
    ?MACRO1(X, 123)
```

This is expanded to:

```erlang
bar(X) ->
    {a,a,b,b},
    {a,X,b,123}.
```

It is good programming practice, but not mandatory, to ensure that a macro
definition is a valid Erlang syntactic form.

To view the result of macro expansion, a module can be compiled with the `'P'`
option. `compile:file(File, ['P'])`. This produces a listing of the parsed code
after preprocessing and parse transforms, in the file `File.P`.

## Predefined Macros

The following macros are predefined:

- **`?MODULE`** - The name of the current module.

- **`?MODULE_STRING`.** - The name of the current module, as a string.

- **`?FILE`.** - The file name of the current module.

- **`?LINE`.** - The current line number.

- **`?MACHINE`.** - The machine name, `'BEAM'`.

- **`?FUNCTION_NAME`** - The name of the current function.

- **`?FUNCTION_ARITY`** - The arity (number of arguments) for the current
  function.

- **`?OTP_RELEASE`** - The OTP release that the currently executing ERTS
  application is part of, as an integer. For details, see
  [`erlang:system_info(otp_release)`](`erlang:system_info/1`).

  > #### Change {: .info }
  >
  > The `?OTP_RELEASE` macro was introduced in Erlang/OTP 21.

- **`?FEATURE_AVAILABLE(Feature)`** - Expands to `true` if the
  [feature](`e:system:features.md#features`) `Feature` is available. The feature
  might or might not be enabled.

  > #### Change {: .info }
  >
  > The `?FEATURE_AVAILABLE()` macro was introduced in Erlang/OTP 25.

- **`?FEATURE_ENABLED(Feature)`** - Expands to `true` if the
  [feature](`e:system:features.md#features`) `Feature` is enabled.
  > #### Change {: .info }
  >
  > The `?FEATURE_ENABLED()` macro was introduced in Erlang/OTP 25.

## Macros Overloading

It is possible to overload macros, except for predefined macros. An overloaded
macro has more than one definition, each with a different number of arguments.

> #### Change {: .info }
>
> Support for overloading of macros was added in Erlang 5.7.5/OTP R13B04.

A macro `?Func(Arg1,...,ArgN)` with a (possibly empty) list of arguments results
in an error message if there is at least one definition of `Func` with
arguments, but none with N arguments.

Assuming these definitions:

```erlang
-define(F0(), c).
-define(F1(A), A).
-define(C, m:f).
```

the following does not work:

```erlang
f0() ->
    ?F0. % No, an empty list of arguments expected.

f1(A) ->
    ?F1(A, A). % No, exactly one argument expected.
```

On the other hand,

```text
f() ->
    ?C().
```

is expanded to

```erlang
f() ->
    m:f().
```

## Removing a macro definition

A definition of macro can be removed as follows:

```erlang
-undef(Macro).
```

## Conditional Compilation

The following macro directives support conditional compilation:

- **`-ifdef(Macro).`** - Evaluate the following lines only if `Macro` is
  defined.

- **`-ifndef(Macro).`** - Evaluate the following lines only if `Macro` is not
  defined.

- **`-else.`** - Only allowed after the `ifdef`, `ifndef`, `if`, and `elif`
  directives. The lines following `else` are evaluated if the preceding
  directive evaluated to false.

- **`-if(Condition).`** - Evaluates the following lines only if `Condition`
  evaluates to true.

- **`-elif(Condition).`** - Only allowed after an `if` or another `elif`
  directive. If the preceding `if` or `elif` directive does not evaluate to
  true, and the `Condition` evaluates to true, the lines following the `elif`
  are evaluated instead.

- **`-endif.`** - Specifies the end of a series of control flow directives.

> #### Note {: .info }
>
> Macro directives cannot be used inside functions.

Syntactically, the `Condition` in `if` and `elif` must be a
[guard expression](expressions.md#guard-expressions). Other constructs (such as
a `case` expression) result in a compilation error.

As opposed to the standard guard expressions, an expression in an `if` and
`elif` also supports calling the psuedo-function `defined(Name)`, which tests
whether the `Name` argument is the name of a previously defined macro.
`defined(Name)` evaluates to `true` if the macro is defined and `false`
otherwise. An attempt to call other functions results in a compilation error.

_Example:_

```erlang
-module(m).
...

-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-else.
-define(LOG(X), true).
-endif.

...
```

When trace output is desired, `debug` is to be defined when the module `m` is
compiled:

```erlang
% erlc -Ddebug m.erl

or

1> c(m, {d, debug}).
{ok,m}
```

`?LOG(Arg)` is then expanded to a call to `io:format/2` and provide the user
with some simple trace output.

_Example:_

```erlang
-module(m)
...
-if(?OTP_RELEASE >= 25).
%% Code that will work in OTP 25 or higher
-elif(?OTP_RELEASE >= 26).
%% Code that will work in OTP 26 or higher
-else.
%% Code that will work in OTP 24 or lower.
-endif.
...
```

This code uses the `OTP_RELEASE` macro to conditionally select code depending on
release.

_Example:_

```erlang
-module(m)
...
-if(?OTP_RELEASE >= 26 andalso defined(debug)).
%% Debugging code that requires OTP 26 or later.
-else.
%% Non-debug code that works in any release.
-endif.
...
```

This code uses the `OTP_RELEASE` macro and `defined(debug)` to compile debug
code only for OTP 26 or later.

## The -feature() directive

[](){: #feature-directive }

The directive `-feature(FeatureName, enable | disable)` can be used to enable or
disable the [feature](`e:system:features.md#features`) `FeatureName`. This is
the preferred way of enabling (disabling) features, although it is possible to
do it with options to the compiler as well.

Note that the `-feature(..)` directive may only appear before any syntax is
used. In practice this means it should appear before any `-export(..)` or record
definitions.

## \-error() and -warning() directives

The directive `-error(Term)` causes a compilation error.

_Example:_

```erlang
-module(t).
-export([version/0]).

-ifdef(VERSION).
version() -> ?VERSION.
-else.
-error("Macro VERSION must be defined.").
version() -> "".
-endif.
```

The error message will look like this:

```text
% erlc t.erl
t.erl:7: -error("Macro VERSION must be defined.").
```

The directive `-warning(Term)` causes a compilation warning.

_Example:_

```erlang
-module(t).
-export([version/0]).

-ifndef(VERSION).
-warning("Macro VERSION not defined -- using default version.").
-define(VERSION, "0").
-endif.
version() -> ?VERSION.
```

The warning message will look like this:

```text
% erlc t.erl
t.erl:5: Warning: -warning("Macro VERSION not defined -- using default version.").
```

> #### Change {: .info }
>
> The `-error()` and `-warning()` directives were added in Erlang/OTP 19.

## Stringifying Macro Arguments

The construction `??Arg`, where `Arg` is a macro argument, is expanded to a
string containing the tokens of the argument. This is similar to the `#arg`
stringifying construction in C.

_Example:_

```erlang
-define(TESTCALL(Call), io:format("Call ~s: ~w~n", [??Call, Call])).

?TESTCALL(myfunction(1,2)),
?TESTCALL(you:function(2,1)).
```

results in

```erlang
io:format("Call ~s: ~w~n",["myfunction ( 1 , 2 )",myfunction(1,2)]),
io:format("Call ~s: ~w~n",["you : function ( 2 , 1 )",you:function(2,1)]).
```

That is, a trace output, with both the function called and the resulting value.
