%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025-2026. All Rights Reserved.
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
-module(ct_doctest).
-moduledoc """"
`ct_doctest` runs doctests on documentation examples. Using `ct_doctest` ensures that the examples
in the documentation are correct, up to date, and stylistically consistent.

The tested examples can be either in a module (normally written using
[documentation attributes](`e:system:documentation.md`)) or in files.
By default `ct_doctest` looks for markdown code blocks and runs any Erlang
code block found that looks like a shell session.

The doctest parser looks for examples that are formatted as if they were run in the
Erlang shell, using prompts of the form `N>`, where `N` starts at `1` for each block.
The expected output is written on the lines following the prompt. For example:


    -doc """
    This is an example of a doctest:
    
    ```
    1> 1+2.
    3
    ```
    """.

`ct_doctest` can be used in Common Test suites to validate documentation examples as part of your
test runs. Normal usage is to call `module/1` with a module name. For example:

```
all() ->
    [doctests].
doctests(_Config) ->
    ct_doctest:module(my_module).
```

## Prompt format rules

For a code block to run as a doctest:

- prompts must start at `1>` for each block
- each subsequent prompt must increment (`2>`, `3>`, ...)
- continuation lines must be indented
- `%` style comment lines are allowed in prompt blocks
- mismatched prompt numbering causes a doctest parse error

## Troubleshooting

If a doctest fails unexpectedly:

- use `verbose` to print per-block execution details
- verify that expected output matches the shell output exactly
- verify prompt numbering and continuation-line indentation

## Examples

Below are examples of supported formats for the code blocks in the documentation. The parser
is quite flexible and supports various styles, including multi-line expressions, comments,
and even prebound variables.

### Basic example

```
1> 1+2.
3
```

### Basic example using Erlang code

This example uses an explicit Erlang code block. That is,

    ```erlang
    1> 1+2.
    3
    ```

instead of the previous one which is a generic code block. Both formats are supported.

```
1> 1+2.
3
```

### Multi-line prompt

Use multiline prompts for expressions that span multiple lines by starting the prompt with `>` and indenting the continuation lines. For example:

```
1> 1
  +
  2
  .
3
```

### Multi-line with comma

It is possible to have multiple expressions in the same prompt, separated by commas. For example:

```
1> A = 1,
  A + 2.
3
```

### Multi-line match

The expected output can span multiple lines. For example:

```
1> [1, 2].
[
 1
 ,
 2
 ]
```

### Multiple prompts

Examples can have multiple prompts. For example:

```
1> 1 + 2.
3
2> 3 + 4.
7
```

### Defining variables

Any variable defined in the examples will be available in the following prompts. For example:

```
1> A = 1 + 2.
3
2> A + 3.
6
```

### Prebound variables

If the documentation examples rely on certain variables being prebound, you can provide these
bindings when calling `module/3`. For example, if you have a module
doc that uses a variable `Prebound`, you can set it up like this:

```
1> Prebound.
hello
```

and then in your test suite:

```
binding_test(_Config) ->
    Bindings = [{moduledoc, #{'Prebound' => hello}}],
    ct_doctest:module(my_module, Bindings, []).
```

### Ignore result

To ignore the results of a prompt, just skip writing the expected output. For example:

```
1> 1 + 2.
2> 3 + 4.
7
```

### Matching exceptions

Examples of failures can be tested by writing the expected exception after the prompt. For example:

```
1> hello + 1.
** exception error: an error occurred when evaluating an arithmetic expression
     in operator  +/2
        called as hello + 1
2> lists:last([]).
** exception error: no function clause matching lists:last([])
```

The simplest way to know what output to write is to run the example in the shell and copy the output,
including the `** exception` line.

If you don't want to include the entire exception message, use only the start of the message.

```
1> hello + 1.
** exception error
```

### Comments

Comments can be inserted anywhere in the code block. For example:

```
%% A comment before the first prompt
1> [1,
%% A comment between prompts
  2].
[1,
%% A comment in a match
 2]
2> [1,
  %% Indented comment between prompts
  2].
[1,
 %% Indented comment in a match
 2]
```

### Matching of maps

When matching on maps, it is possible to use shell syntax, that is, `=>` and not `:=`, as in
normal Erlang code. For example:

```
1> #{ a => b }.
#{ a => b }
```

### Matching of ...

It is possible to use `...` in the expected output to indicate that the rest of the output
should be ignored. This is useful for outputs that are large or contain non-deterministic elements.

```
1> lists:seq(1,100).
[1, 2, 3, ...]
2> #{ a => b }.
#{ a => ... }
3> <<1, 0:1024>>.
<<1, 0, 0, 0, ...>>
```

### Compiling modules

ct_doctest can also compile full module code examples. It then looks for a
`-module` declaration to determine the module name and compiles the code as
if it were in a file. For example:

```
-module(my_module).
-export([foo/0]).
foo() ->
    ok.
```

The module is then available for use in following prompts. For example:

```
1> my_module:foo().
```

### Edge cases

The following are examples that are not supported by the parser and will be ignored.

```
a> should not be tested
```

```
 1> should not be tested
```

```
> should not be tested
```

```
should not be tested
1> 
```
"""".
-moduledoc(#{since => ~"OTP @OTP-20034@"}).

-include_lib("kernel/include/eep48.hrl").

-export([module/1, module/2, module/3, file/1, file/2, file/3]).

-doc """
Options for doctest execution.

* `parser` - Use this option to plug in an external documentation parser. The
  parser callback must be a `fun/1` and return a list of Erlang code block binaries.
  The code blocks are then checked to determine whether they should be run as doctests.
  If no parser is provided, a built-in markdown parser will be used.
* `skipped_blocks` - Sets the exact number of Erlang code blocks that are allowed
  to be skipped because no runnable shell prompts were found. It defaults to `false`.
* `verbose` - Print detailed information while running doctests, including each
  block run and skipped block details.
""".
-type options() :: [{parser, fun((unicode:unicode_binary()) -> [unicode:unicode_binary()] | {error, term()}) } |
                    {skipped_blocks, non_neg_integer() | false} |
                    {verbose, boolean()}].

-doc #{equiv => module(Module, [])}.
-spec module(module()) ->
          ok | {comment, string()} | {error, term()} | no_return().
module(Module) ->
    module(Module, []).

-doc #{equiv => module(Module, [], Options)}.
-spec module(module(), options()) ->
          ok | {comment, string()} | {error, term()} | no_return().
module(Module, Options) ->
    module(Module, [], Options).

-doc """
Run tests for the documentation in a module with EEP-48 docs.

When calling `module/3`, `ct_doctest` looks for documentation in the specified module and
runs any examples found there. The module, function, type, and callback documentation
are all checked for examples.

The function returns `ok` if all tests pass, or `{comment, Comment}` if all tests pass but one or more
functions lack tests. If any test fails, an exception in the form of `error({N, errors})` is raised,
where `N` is the number of failed tests. The details of each failure are printed to the console.

Use `Bindings` to provide prebound variables for a specific doc entry. Use
`moduledoc` for module docs and `{function, Name, Arity}` (or corresponding
`type`/`callback` keys) for entry-specific bindings.

See `t:options/0` for available options.
""".
-spec module(module(), Bindings, options()) ->
          ok | {comment, string()} | {error, term()} | no_return()
          when 
            KFA :: {Kind :: function | type | callback, atom(), arity()},
            Bindings :: [{KFA | moduledoc, erl_eval:binding_struct()}].
module(Module, Bindings, Options) ->
    HasParserKey = proplists:is_defined(parser, Options),
    ParserFun = options_parser(Options),
    ExpectedSkipped = options_skipped_blocks(Options),
    Verbose = options_verbose(Options),
    case code:get_doc(Module) of
        {ok, #docs_v1{ format = ~"text/markdown" } = Docs} when not HasParserKey ->
            run_module_docs(Docs, Bindings, ParserFun, ExpectedSkipped, Verbose);
        {ok, #docs_v1{} = Docs} when HasParserKey ->
            run_module_docs(Docs, Bindings, ParserFun, ExpectedSkipped, Verbose);
        {ok, _} ->
            {error, unsupported_format};
        Else ->
            Else
    end.

-doc #{equiv => file(File, [], [])}.
-spec file(file:filename()) ->
          ok | {error, term()} | no_return().
file(File) ->
    file(File, []).

-doc #{equiv => file(File, [], Options)}.
-spec file(file:filename(), options()) ->
          ok | {comment, string()} | {error, term()} | no_return().
file(File, Options) ->
    file(File, [], Options).

-doc """
Run doctests for a markdown file.

The function returns `ok` if all tests pass. If any test fails, an exception in the form of
`error({N, errors})` is raised, where `N` is the number of failed tests. The details of each
failure are printed to the console.

Use `Bindings` to provide prebound variables. Bindings are global for all files, so take
care to avoid any naming conflicts.

You can run doctests on non-markdown files by providing a custom parser that extracts the
code blocks to be tested.

See `t:options/0` for available options.
""".
-spec file(file:filename(), Bindings :: [{atom(), term()}], options()) ->
          ok | {comment, string()} | {error, term()} | no_return().
file(File, Bindings, Options) ->
    ParserFun = options_parser(Options),
    ExpectedSkipped = options_skipped_blocks(Options),
    Verbose = options_verbose(Options),
    case file:read_file(File) of
        {ok, Content} ->
            try
                Blocks = inspect(parse(Content, ParserFun)),
                {_RunResult, Skipped} = run_blocks(Blocks, Bindings,
                                {file, File}, Verbose),
                ensure_skipped_blocks(ExpectedSkipped, Skipped),
                ok
            catch
                throw:{error, Error} ->
                    format_error({file, File, Error}),
                    error({1, errors});
                C:R:ST ->
                    io:format("Uncaught exception in file ~ts~n", [File]),
                    erlang:raise(C, R, ST)
            end;
        {error, _} = Error ->
            Error
    end.

run_module_docs(#docs_v1{ docs = Docs, module_doc = MD },
                Bindings, ParserFun, ExpectedSkipped, Verbose) ->
    MDRes = parse_and_run(moduledoc, MD, Bindings, ParserFun, Verbose),
    Res =
        lists:append(
          [parse_and_run(KFA, EntryDocs, Bindings, ParserFun, Verbose) ||
              {KFA, _Anno, _Sig, EntryDocs, _Meta} <- Docs,
              is_map(EntryDocs)]),
    Errors =
        [{{T,F,A},E} || {{T,F,A},[{error,E}],_} <- Res] ++
        [{moduledoc,E} || {moduledoc,[{error,E}],_} <- MDRes],
    _ = [io:put_chars(format_error(E)) || E <- Errors],
    case length(Errors) of
        0 ->
            Skipped = lists:sum([Count || {_, _, Count} <- MDRes ++ Res]),
            verbose_log(Verbose,
                        "module complete; total skipped blocks: ~p (expected ~p)",
                        [Skipped, ExpectedSkipped]),
            ensure_skipped_blocks(ExpectedSkipped, Skipped),
            NoTests = lists:sort([io_lib:format("  ~p/~p\n", [F,A]) ||
                                     {{function,F,A},[],_} <- Res]),
            case length(NoTests) of
                0 ->
                    ok;
                N ->
                    io:format("The following functions have no tests:~n~n~ts~n",
                              [NoTests]),
                    {comment,
                     lists:flatten(io_lib:format("~p functions lack tests", [N]))}
            end;
        N ->
            error({N,errors})
    end.

format_error({moduledoc, Context}) ->
    [a_test_failed("moduledoc", Context), format_error_context(Context)];
format_error({{Type, Name, Arity}, Context}) ->
    [a_test_failed(io_lib:format("~p ~p/~p", [Type, Name, Arity]), Context), format_error_context(Context)];
format_error({file, Path, Context}) ->
    [a_test_failed(io_lib:format("file ~ts", [Path]), Context), format_error_context(Context)].

a_test_failed(Where, Context) ->
    LineNo =
        if is_map_key(line, Context) ->
            [" on line ", integer_to_list(maps:get(line, Context))];
           true ->
            ""
        end,
    io_lib:format("A test failed in ~ts~ts:~n~n", [Where, LineNo]).

format_error_context(#{ test := {test, Index, Test, Match}, message := Message }) ->
    io_lib:format("~ts> ~ts~n~ts~n~n~ts~n", [Index, Test, string:trim(Match), string:trim(Message)]);
format_error_context(#{ message := Message, context := Context }) ->
    io_lib:format("~ts~n~n~ts~n", [string:trim(Context), string:trim(Message)]);
format_error_context(#{ message := Message }) ->
    io_lib:format("~ts~n", [string:trim(Message)]).

parse_and_run(_, hidden, _, _, _) -> [];
parse_and_run(_, none, _, _, _) -> [];
parse_and_run(KFA, #{} = Ds, Bindings, ParserFun, Verbose) ->
    [do_parse_and_run(KFA, D, Bindings, ParserFun, Verbose) || _ := D <- Ds].

do_parse_and_run(KFA, Docs, Bindings, ParserFun, Verbose) ->
    try
        InitialBindings = proplists:get_value(KFA, Bindings, []),
        Blocks = inspect(parse(Docs, ParserFun)),
        {RunResult, Skipped} = run_blocks(Blocks, InitialBindings,
                                          {module, KFA}, Verbose),
        {KFA, RunResult, Skipped}
    catch
        throw:{error,_}=Error ->
            {KFA, [Error], 0};
        C:R:ST ->
            io:format("Uncaught exception in ~p~n", [KFA]),
            erlang:raise(C, R, ST)
    end.

run_blocks(Blocks, Bindings, Context, Verbose) ->
    {_Index, Result} =
        lists:foldl(fun(Test, {Index, {Acc, Skipped}}) ->
                            {Result0, NewSkipped} = test_block(Test, Bindings,
                                                               Context, Index, Skipped, Verbose),
                            {Index + 1, {Acc ++ Result0, NewSkipped}}
                    end, {1, {[], 0}}, Blocks),
    Result.

test_block(Code, Bindings, Context, Index, Skipped, Verbose) when is_binary(Code) ->
    ContextLabel = context_label(Context),
    FirstLines = first_lines(Code),
    verbose_log(Verbose, "running block ~p in ~ts:~n~ts",
                [Index, ContextLabel, Code]),
    try run_test(Code, Bindings, Verbose) of
        [] ->
            verbose_log(Verbose, "skipped block ~p in ~ts (no runnable prompt, ~p skipped): ~ts",
                        [Index, ContextLabel, Skipped + 1, FirstLines]),
            {[], Skipped + 1};
        Result ->
            verbose_log(Verbose, "passed block ~p in ~ts", [Index, ContextLabel]),
            {Result, Skipped}
    catch
        throw:{error, ErrorContext} = Error ->
            verbose_log(Verbose,
                        "failed block ~p in ~ts:~n~ts~n",
                        [Index, ContextLabel,
                         format_error_context(ErrorContext)]),
            throw(Error);
        C:R:ST ->
            verbose_log(Verbose,
                        "failed block ~p in ~ts with ~p:~tp~nblock snippet:~n~ts",
                        [Index, ContextLabel, C, R, Code]),
            erlang:raise(C, R, ST)
    end;
test_block(Other, _Bindings, _Context, _Index, _Skipped, _Verbose) ->
    throw({error, {invalid_code_block, Other}}).

context_label({module, moduledoc}) ->
    ~"moduledoc";
context_label({module, {Kind, Name, Arity}}) ->
    lists:flatten(io_lib:format("~p ~p/~p", [Kind, Name, Arity]));
context_label({file, Path}) ->
    unicode:characters_to_binary(Path);
context_label(Other) ->
    lists:flatten(io_lib:format("~tp", [Other])).

first_lines(Code) ->
    Lines = string:split(Code, "\n", all),
    lists:join($\n, lists:sublist(Lines, 5)).

verbose_log(false, _Fmt, _Args) ->
    ok;
verbose_log(true, Fmt, Args) ->
    Str = io_lib:format("ct_doctest(verbose): " ++ Fmt, Args),
    [First | Rest] = string:split(string:trim(Str), "\n", all),
    io:put_chars([First, [["\n    ", Line] || Line <- Rest], "\n"]).

parse(Content, ParserFun) ->
    validate_code_blocks(run_parser(ParserFun, Content)).

run_parser(ParserFun, Content) when is_function(ParserFun, 1) ->
    ParserFun(Content);
run_parser(Parser, _Content) ->
    Msg = io_lib:format("Invalid parser provided: ~p. Parser must be a fun/1.", [Parser]),
    throw({error, #{ message => Msg }}).

validate_code_blocks({error, Reason}) ->
    Msg = io_lib:format("Parser returned an error: ~p.", [Reason]),
    throw({error, #{ message => Msg }});
validate_code_blocks(Blocks) when is_list(Blocks) ->
    [validate_code_block(Block) || Block <- Blocks];
validate_code_blocks(Other) ->
    Msg = io_lib:format("Parser returned invalid result: ~p.", [Other]),
    throw({error, #{ message => Msg }}).

validate_code_block(Block) when is_binary(Block) ->
    Block;
validate_code_block(Other) ->
    throw({error, #{ message => io_lib:format("Invalid code block: ~p.", [Other]) }}).

options_parser(Options) ->
    proplists:get_value(parser, Options,
                        fun parse_markdown_builtin/1).

parse_markdown_builtin(Markdown) ->
    extract_erlang_code_blocks(inspect(shell_docs_markdown:parse_md(Markdown))).

extract_erlang_code_blocks(Ast) when is_list(Ast) ->
    lists:append([extract_erlang_code_blocks(Item) || Item <- Ast]);
extract_erlang_code_blocks({pre, [], [{code, Attrs, [Code]}]}) when is_binary(Code) ->
    Class = proplists:get_value(class, Attrs, ~"language-erlang"),
    Tokens = string:split(unicode:characters_to_binary(Class), ~" ", all),
    case lists:member(~"language-erlang", Tokens) of
        true ->
            [Code];
        false ->
            []
    end;
extract_erlang_code_blocks({_Tag, _Attrs, Content}) ->
    extract_erlang_code_blocks(Content);
extract_erlang_code_blocks(_Other) ->
    [].

options_skipped_blocks(Options) ->
    proplists:get_value(skipped_blocks, Options, false).

options_verbose(Options) ->
    proplists:get_value(verbose, Options, false) =:= true.

ensure_skipped_blocks(false, _Actual) ->
    ok;
ensure_skipped_blocks(Expected, Actual) when is_integer(Expected), Expected >= 0 ->
    case Actual of
        Expected ->
            ok;
        _ ->
            error({unexpected_skipped_blocks, Expected, Actual})
    end.

-define(RE_CAPTURE, ~B"(?:(?'line_number'[0-9]+)(?'prefix'>\s)|(?'prefix'\-module\())?(?'content'.*)").
-define(RE_OPTIONS, [{capture, [line_number, prefix, content], binary}, dupnames, unicode]).

run_test(Code, InitialBindings, Verbose) ->
    Lines = string:split(Code, "\n", all),
    CollapsedComments = [re:replace(Line, ~B"^\s*%.*$", <<"">>, [global, unicode]) || Line <- Lines],
    case lists:search(fun(Line) ->
                           re:run(Line, ~B"^\s*$", [unicode]) =:= nomatch
                       end, CollapsedComments) of
        false ->
            [];
        {value, FirstLine} ->
            case re:run(FirstLine, ?RE_CAPTURE, ?RE_OPTIONS) of
                {match, [_Line_Number, _Prefix = <<"> ">>, _Code]} ->
                    ReLines = [re:run(Line, ?RE_CAPTURE, ?RE_OPTIONS) || Line <- CollapsedComments],
                    Tests = inspect(parse_tests(ReLines, [], 1)),
                    check_prompt_numbers(Tests),
                    _ = lists:foldl(fun(Test, Bindings) ->
                                            try run_tests(Test, Bindings, Verbose)
                                            catch throw:{error, Error} ->
                                                    throw({error, Error#{ test => Test}})
                                            end
                                    end, InitialBindings, Tests),
                    [ok];
                {match, [_Line_Number, _Prefix = <<"-module(">>, _Code]} ->
                    [compile_string(Code)];
                _ ->
                    []
            end
    end.

compile_string(Code) ->

    Toks =
        case erl_scan:string(unicode:characters_to_list(Code),
                             0,
                             [text]) of
            {ok, T, _} ->
                T;
            {error, {Line,Mod,Reason}, _} ->
                Message = io_lib:format("unknown:~p: ~ts",[Line, Mod:format_error(Reason)]),
                throw({error,#{ message => Message, context => Code}})
        end,

    Forms = parse_tokens(Code, Toks),

    {attribute,_,module,ModuleName} = lists:keyfind(module, 3, Forms),

    case compile:forms(Forms, [binary, return_errors, {source, atom_to_list(ModuleName) ++ ".erl"}]) of
        {ok, Module, Binary} ->
            {module, Module} = code:load_binary(Module, "nofile", Binary),
            ok;
        {error, Errors, Warnings} ->
            Messages = [begin [{_, M}] = sys_messages:format_messages(File, "", Msgs, []), M end || {File, Msgs} <- Errors ++ Warnings],
            throw({error,#{ message => Messages, context => Code}})
    end.

parse_tokens(_Code, []) -> [];
parse_tokens(Code, Toks) ->
    case lists:splitwith(fun(T) ->
            element(1, T) =/= dot
        end, Toks) of
        {Ts, [{dot, _} = Dot | Rest]} ->
            case erl_parse:parse_form(Ts ++ [Dot]) of
                {ok, Forms} ->
                    [Forms | parse_tokens(Code, Rest)];
                {error, {Line, Mod, Reason}} ->
                    Message = io_lib:format("unknown:~p: ~ts",[Line, Mod:format_error(Reason)]),
                    throw({error,#{ message => Message, context => Code}})
            end
    end.

check_prompt_numbers(Tests) ->
    check_prompt_numbers(Tests, 1).

check_prompt_numbers([], _Expected) ->
    ok;
check_prompt_numbers([{test, LineNumber, _, _} = Test | T], Expected) ->
    case binary_to_integer(LineNumber) of
        Expected ->
            check_prompt_numbers(T, Expected + 1);
        Actual ->
            Message = io_lib:format("Bad prompt number ~p; expected ~p",
                                    [Actual,Expected]),
            throw({error,#{ message => Message, test => Test }})
    end.

parse_tests([], [], _) ->
    [];
parse_tests([], Cmd, No) ->
    [{test, No, lists:join($\n, lists:reverse(Cmd)), "_"}];
parse_tests([{match, [<<>>, <<>>, <<>>]} | T], Cmd, No) ->
    parse_tests(T, Cmd, No);
parse_tests([{match, [PromptNo, <<"> ">>, NewCmd]} | T], [], _) ->
    parse_tests(T, [NewCmd], PromptNo);
parse_tests([{match, [PromptNo, <<"> ">>, NewCmd]} | T], Cmd, No) ->
    [{test, No, lists:join($\n, lists:reverse(Cmd)), "_"} | parse_tests(T, [NewCmd], PromptNo)];
parse_tests([{match, [<<>>, <<>>, <<" ", _/binary>> = More]} | T], Acc, No) ->
    parse_tests(T, [More | Acc], No);
parse_tests([{match, [<<>>, <<>>, NewMatch]} | T], Cmd, No) ->
    {Match, Rest} = parse_match(T, [NewMatch]),
    [{test, No, lists:join($\n, lists:reverse(Cmd)),
      lists:join($\n, lists:reverse(Match))} | parse_tests(Rest, [], No)].

parse_match([{match, [<<>>, <<>>, <<>>]} | T], Acc) ->
    parse_match(T, Acc);
parse_match([{match, [<<>>, <<>>, <<" ", _/binary>> = More]} | T], Acc) ->
    parse_match(T, [More | Acc]);
parse_match(Rest, Acc) ->
    {Acc, Rest}.

run_tests({test, _Index, Test0, Match0}, Bindings, Verbose) ->
    Test1 = unicode:characters_to_list(Test0),
    Test = string:trim(string:trim(Test1), trailing, "."),
    case Match0 of
        [<<"** ", _/binary>> | _] ->
            Match = unicode:characters_to_list(Match0),
            run_failing(Test, Match, Bindings, Verbose);
        _ ->
            run_successful(Test, Match0, Bindings, Verbose)
    end.

run_successful(Test, Match, Bindings, Verbose) ->
    verbose_log(Verbose, "Running: ~ts = ~ts", [Match, Test]),
    Ast = parse_exprs(Test, Match),
    try
        {value, _Res, NewBindings} = inspect(erl_eval:exprs(Ast, Bindings)),
        NewBindings
    catch C:R:ST ->
            throw({error,#{ message => format_exception(C, R, ST) }})
    end.

run_failing(Test, Match, Bindings, Verbose) ->
    verbose_log(Verbose, "Running: ~ts", [Test]),
    Ast = parse_exprs(Test, "_"),
    try inspect(erl_eval:exprs(Ast, Bindings)) of
        {value, Res, _} ->
            Message = io_lib:format("Expected failure got ~ts",
                                    [Res]),
            throw({error,#{ message => Message, context => Match}})
    catch C:R:ST ->
            Actual = format_exception(C, R, ST),
            case string:prefix(Actual,  Match) of
                nomatch ->
                    Message = io_lib:format("Failure did not match:~n~ts~n",
                                            [Actual]),
                    throw({error,#{ message => Message, context => Match}});
                _ ->
                    Bindings
            end
    end.

%% As we allow <0.1.0> style matches we first need to check
%% if the match actually is a valid pattern. So we parse and
%% evaluate it first, and if that fails we convert it to a literal
%% and try again.
parse_exprs(Test, Match0) ->
    try {parse_exprs(Match0 ++ " = 1."),
         parse_exprs("1 = begin " ++ Test ++ " end.")} of
        {MatchAst, _TestAst} ->
            Match =
                try erl_eval:exprs(MatchAst, #{}) of
                    {value, _Res, _} ->
                        Match0
                catch
                    error:_ ->
                        maybe_convert_to_literal(Match0)
                end,
            parse_exprs(Match ++ " = begin " ++ Test ++ " end.")
    catch throw:_ ->
            parse_exprs(Match0 ++ " = " ++ Test ++ ".")
    end.

parse_exprs(Str) ->
    Cmd = lists:flatten(unicode:characters_to_list(Str)),
    maybe
        {ok, T, _} ?= erl_scan:string(Cmd, 0, [text]),
        RewrittenToks = rewrite_tokens(T),
        {ok, Ast} ?= inspect(erl_eval:extended_parse_exprs(RewrittenToks)),
        rewrite_match_ast(Ast)
    else
        {error, {Line,Mod,Reason}, _} ->
            Message = Mod:format_error(Reason),
            throw({error,#{ message => Message, line => Line}});
        {error, {Line,Mod,Reason}} ->
            Message = Mod:format_error(Reason),
            throw({error,#{ message => Message, line => Line}})
    end.

%% We rewrite ...>> to _/binary>> to match shell syntax better
rewrite_tokens([{'...', L1}, {'>>', L2} | T]) ->
    rewrite_tokens([{var, L1, '_'}, {'/', L1}, {atom, L1, binary}, {'>>', L2} | T]);
%% We rewrite , ... ] to | _ ] to match shell syntax better
rewrite_tokens([{',', L1}, {'...', L2}, {']', L3} | T]) ->
    rewrite_tokens([{'|', L1}, {var, L2, '_'}, {']', L3} | T]);
%% We rewrite ... to _ to match shell syntax better
rewrite_tokens([{'...', L} | T]) ->
    rewrite_tokens([{var, L, '_'} | T]);
rewrite_tokens([H | T]) ->
    [H | rewrite_tokens(T)];
rewrite_tokens([]) ->
    [].

%% Rewrite the AST to convert map field associations to exact matches on the LHS
rewrite_match_ast([{match, Ann, LHS, RHS}]) ->

    RewrittenLHS =
      erl_syntax_lib:map(
        fun(Tree) ->
                case erl_syntax:type(Tree) of
                    map_field_assoc ->
                        Name = erl_syntax:map_field_assoc_name(Tree),
                        Value = erl_syntax:map_field_assoc_value(Tree),
                        erl_syntax:map_field_exact(Name, Value);
                    _Else ->
                        Tree
                end
        end, LHS),
    [{match, Ann, erl_syntax:revert(RewrittenLHS), RHS}];
rewrite_match_ast([{match, _, _, _} = Match | Rest]) ->
    [Match | rewrite_match_ast(Rest)].

%% We do a little dance here in order to allow refs, pids and ports
%% to be matched as literals, since the shell prints them as literals
%% but erl_eval doesn't accept them as such.
%%
%% We traverse the AST and hoist only the literal-producing calls
%% (list_to_pid, list_to_port, list_to_ref) into variable bindings,
%% leaving the rest of the match pattern intact.
%%
%% For example, {ok, <0.1.0>} becomes: _L1 = <0.1.0>, {ok, _L1}
maybe_convert_to_literal(Match0) ->
    Match = unicode:characters_to_list(Match0),
    maybe
        {ok, Toks, _} ?= erl_scan:string(Match ++ ".", 0, [text]),
        RewrittenToks = rewrite_tokens(Toks),
        {ok, [Expr0]} ?= erl_eval:extended_parse_exprs(RewrittenToks),
        {Expr1, Acc} = hoist_literal_calls(Expr0),
        true ?= Acc =/= [],
        Expr = erl_syntax:revert(Expr1),
        Prefix = lists:join(", ",
            [lists:flatten(io_lib:format("~s = ~p", [V, L]))
             || {V, L} <- lists:reverse(Acc)]),
        ExprStr = lists:flatten(erl_pp:expr(Expr)),
        lists:flatten([Prefix, ", ", ExprStr])
    else
        _ ->
            Match
    end.

hoist_literal_calls(Expr) ->
    erl_syntax_lib:mapfold(
      fun(Tree, Acc) ->
              case is_literal_producing_call(Tree) of
                  {yes, Value} ->
                      VarName = "_L" ++ integer_to_list(
                                          erlang:unique_integer([positive])),
                      Var = erl_syntax:variable(list_to_atom(VarName)),
                      {Var, [{VarName, Value} | Acc]};
                  no ->
                      {Tree, Acc}
              end
      end, [], Expr).

is_literal_producing_call(Tree) ->
    case erl_syntax:type(Tree) of
        application ->
            Op = erl_syntax:application_operator(Tree),
            case erl_syntax:type(Op) of
                module_qualifier ->
                    Mod = erl_syntax:atom_value(
                            erl_syntax:module_qualifier_argument(Op)),
                    Fun = erl_syntax:atom_value(
                            erl_syntax:module_qualifier_body(Op)),
                    case {Mod, Fun} of
                        {erlang, F} when F =:= list_to_pid;
                                         F =:= list_to_port;
                                         F =:= list_to_ref ->
                            [Arg] = erl_syntax:application_arguments(Tree),
                            S = erl_syntax:string_value(Arg),
                            try {yes, erlang:F(S)} catch _:_ -> no end;
                        _ ->
                            no
                    end;
                _ ->
                    no
            end;
        implicit_fun ->
            %% Handles fun M:F/A references from shell syntax like M.F/A
            Reverted = erl_syntax:revert(Tree),
            {value, Val, _} = erl_eval:exprs([Reverted], #{}),
            {yes, Val};
        _ ->
            no
    end.

format_exception(Class, Reason, [Top,Next|_]) ->
    Stacktrace = [clean_stacktrace_item(Item) || Item <- [Top,Next]],
    Tag = "** ",
    I = iolist_size(Tag) + 1,
    PF = fun pp/2,
    SF = fun(Mod, _, _) -> Mod =:= erl_eval end,
    Enc = unicode,
    Str = erl_error:format_exception(I, Class, Reason, Stacktrace, SF, PF, Enc),
    Tag ++ string:trim(lists:flatten(Str), trailing).

clean_stacktrace_item({M,F,A,Info0}) ->
    Info = lists:keydelete(line, 1, Info0),
    {M,F,A,Info}.

pp(V, I) ->
    D = 30,
    io_lib_pretty:print(V, [{column, I}, {line_length, 120},
                            {depth, D}, {line_max_chars, 100},
                            {strings, true},
                            {encoding, unicode}]).

inspect(Term) ->
    %% Uncomment for debugging
    %% io:format("~tp~n",[Term]),
    Term.
