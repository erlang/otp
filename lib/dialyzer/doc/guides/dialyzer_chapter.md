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
# Dialyzer

## Introduction

### Scope

Dialyzer is a static analysis tool that identifies software discrepancies, such
as definite type errors, code that has become dead or unreachable because of
programming error, and unnecessary tests, in single Erlang modules or entire
(sets of) applications.

Dialyzer can be called from the command line and from Erlang.

### Prerequisites

It is assumed that the reader is familiar with the Erlang programming language.

[](){: #plt }

## The Persistent Lookup Table

Dialyzer stores the result of an analysis in a Persistent Lookup Table (PLT).
The PLT can then be used as a starting point for later analyses. It is
recommended to build a PLT with the Erlang/OTP applications that you are using,
but also to include your own applications that you are using frequently.

The PLT is built using option `--build_plt` to Dialyzer. The following command
builds the recommended minimal PLT for Erlang/OTP:

```text
dialyzer --build_plt --apps erts kernel stdlib mnesia
```

Dialyzer looks if there is an environment variable called `DIALYZER_PLT` and
places the PLT at this location. If no such variable is set, Dialyzer places the
PLT in a file called .dialyzer_plt in the
[`filename:basedir(user_cache, "erlang")`](`m:filename#user_cache`) folder. The
placement can also be specified using the options `--plt` or `--output_plt`.

Information can be added to an existing PLT using option `--add_to_plt`. If you
also want to include the Erlang compiler in the PLT and place it in a new PLT,
then use the following command:

```text
dialyzer --add_to_plt --apps compiler --output_plt my.plt
```

Then you can add your favorite application my_app to the new PLT:

```text
dialyzer --add_to_plt --plt my.plt -r my_app/ebin
```

But you realize that it is unnecessary to have the Erlang compiler in this one:

```text
dialyzer --remove_from_plt --plt my.plt --apps compiler
```

Later, when you have fixed a bug in your application my_app, you want to update
the PLT so that it becomes fresh the next time you run Dialyzer. In this case,
run the following command:

```text
dialyzer --check_plt --plt my.plt
```

Dialyzer then reanalyzes the changed files and the files that depend on these
files. Notice that this consistency check is performed automatically the next
time you run Dialyzer with this PLT. Option `--check_plt` is only for doing so
without doing any other analysis.

To get information about a PLT, use the following option:

```text
dialyzer --plt_info
```

To specify which PLT, use option `--plt`.

To get the output printed to a file, use option `--output_file`.

Notice that when manipulating the PLT, no warnings are emitted. To turn on
warnings during (re)analysis of the PLT, use option `--get_warnings`.

## Using Dialyzer from the Command Line

Dialyzer has a command-line version for automated use. See `m:dialyzer`.

## Using Dialyzer from Erlang

Dialyzer can also be used directly from Erlang. See `m:dialyzer`.

## Dialyzer's Model of Analysis

Dialyzer operates somewhere between a classical type checker and a more general
static-analysis tool: It checks and consumes function specs, yet doesn't require
them, and it can find bugs across modules which consider the dataflow of the
programs under analysis. This means Dialyzer can find genuine bugs in complex
code, and is pragmatic in the face of missing specs or limited information about
the codebase, only reporting issues which it can prove have the potential to
cause a genuine issue at runtime. This means Dialyzer will sometimes not report
every bug, since it cannot always find this proof.

### How Dialyzer Utilises Function Specifications

Dialyzer infers types for all top-level functions in a module. If the module
also has a spec given in the source-code, Dialyzer will compare the inferred
type to the spec. The comparison checks, for each argument and the return, that
the inferred and specified types overlap - which is to say, the types have at
least one possible runtime value in common. Notice that Dialyzer does not check
that one type contains a subset of values of the other, or that they're
precisely equal: This allows Dialyzer to make simplifying assumptions to
preserve performance and avoid reporting program flows which could potentially
succeed at runtime.

If the inferred and specified types do not overlap, Dialyzer will warn that the
spec is invalid with respect to the implementation. If they do overlap, however,
Dialyzer will proceed under the assumption that the correct type for the given
function is the intersection of the inferred type and the specified type (the
rationale being that the user may know something that Dialyzer itself cannot
deduce). One implication of this is that if the user gives a spec for a function
which overlaps with Dialyzer's inferred type, but is more restrictive, Dialyzer
will trust those restrictions. This may then generate an error elsewhere which
follows from the erroneously restricted spec.

_Examples:_

Non-overlapping argument:

```erlang
-spec foo(boolean()) -> string().
%% Dialyzer will infer: foo(integer()) -> string().
foo(N) ->
    integer_to_list(N).
```

Since the type of the argument in the spec is different from the type that
Dialyzer inferred, Dialyzer will generate the following warning:

```erlang
some_module.erl:7:2: Invalid type specification for function some_module:foo/1.
 The success typing is t:foo
          (integer()) -> string()
 But the spec is t:foo
          (boolean()) -> string()
 They do not overlap in the 1st argument
```

Non-overlapping return:

```erlang
-spec bar(a | b) -> atom().
%% Dialyzer will infer: bar(a | b) -> binary().
bar(a) -> <<"a">>;
bar(b) -> <<"b">>.
```

Since the return value in the spec and the return value inferred by Dialyzer are
different, Dialyzer will generate the following warning:

```erlang
some_module.erl:11:2: Invalid type specification for function some_module:bar/1.
 The success typing is t:bar
          ('a' | 'b') -> <<_:8>>
 But the spec is t:bar
          ('a' | 'b') -> atom()
 The return types do not overlap
```

Overlapping spec and inferred type:

```erlang
-spec baz(a | b) -> non_neg_integer().
%% Dialyzer will infer: baz(b | c | d) -> -1 | 0 | 1.
baz(b) -> -1;
baz(c) -> 0;
baz(d) -> 1.
```

Dialyzer will "trust" the spec and using the intersection of the spec and
inferred type:

```text
baz(b) -> 0 | 1.
```

Notice how the `c` and `d` from the argument to `baz/1` and the `-1` in the
return from the inferred type were dropped once the spec and inferred type were
intersected. This could result in warnings being emitted for later functions.

For example, if `baz/1` is called like this:

```text
call_baz1(A) ->
    case baz(A) of
        -1 -> negative;
        0 -> zero;
        1 -> positive
    end.
```

Dialyzer will generate the following warning:

```text
some_module.erl:25:9: The pattern
          -1 can never match the type
          0 | 1
```

If `baz/1` is called like this:

```text
call_baz2() ->
    baz(a).
```

Dialyzer will generate the following warnings:

```text
some_module.erl:30:1: Function call_baz2/0 has no local return
some_module.erl:31:9: The call t:baz
         ('a') will never return since it differs in the 1st argument
               from the success typing arguments:
         ('b' | 'c' | 'd')
```

## Feedback and Bug Reports

We very much welcome user feedback - even wishlists\! If you notice anything
weird, especially if Dialyzer reports any discrepancy that is a false positive,
please send an error report describing the symptoms and how to reproduce them.
