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
[1, 2, 3 | ...]
2> #{ a => b }.
#{ a => ... }
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
  to be skipped because no runnable shell prompts were found. It defaults to `0`.
  Set it to `false` to disable this check.
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
                    print_error({file, File, Error}),
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
        [{{F,A},E} || {{_,F,A},[{error,E}],_} <- Res] ++
        [{moduledoc,E} || {moduledoc,[{error,E}],_} <- MDRes],
    _ = [print_error(E) || E <- Errors],
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

print_error({moduledoc,{Message,Line,Context}}) ->
    io:format("Module Doc:~p: ~ts~n~ts~n", [Line,Context,Message]);
print_error({moduledoc,{Message,Context}}) ->
    io:format("Module Doc: ~ts~n~ts~n", [Context,Message]);
print_error({file, Path, {Message,Line,Context}}) ->
    io:format("File ~ts:~p: ~ts~n~ts~n", [Path,Line,Context,Message]);
print_error({file, Path, {Message,Context}}) ->
    io:format("File ~ts: ~ts~n~ts~n", [Path,Context,Message]);
print_error({{Name,Arity},{Message,Line,Context}}) ->
    io:format("~p/~p:~p: ~ts~n~ts~n", [Name,Arity,Line,Context,Message]);
print_error({{Name,Arity},{Message,Context}}) ->
    io:format("~p/~p: ~ts~n~ts~n", [Name,Arity,Context,Message]).

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
    verbose_log(Verbose, "running block ~p in ~ts: ~ts",
                [Index, ContextLabel, Code]),
    try run_test(Code, Bindings) of
        [] ->
            verbose_log(Verbose, "skipped block ~p in ~ts (no runnable prompt, ~p skipped): ~ts",
                        [Index, ContextLabel, Skipped + 1, FirstLines]),
            {[], Skipped + 1};
        Result ->
            verbose_log(Verbose, "passed block ~p in ~ts", [Index, ContextLabel]),
            {Result, Skipped}
    catch
        throw:{error, _} = Error ->
            verbose_log(Verbose,
                        "failed block ~p in ~ts:~n~ts~nblock snippet:~n~ts",
                        [Index, ContextLabel,
                         format_error_for_verbose(Error), Code]),
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

format_error_for_verbose({error, {Message, Line, Source}}) ->
    iolist_to_binary(io_lib:format("~ts (line ~p): ~ts",
                                   [Message, Line, Source]));
format_error_for_verbose({error, {Message, Source}}) ->
    iolist_to_binary(io_lib:format("~ts: ~ts", [Message, Source]));
format_error_for_verbose(Other) ->
    iolist_to_binary(io_lib:format("~tp", [Other])).

verbose_log(false, _Fmt, _Args) ->
    ok;
verbose_log(true, Fmt, Args) ->
    io:format("ct_doctest(verbose): " ++ Fmt ++ "~n", Args).

parse(Content, ParserFun) ->
    validate_code_blocks(run_parser(ParserFun, Content)).

run_parser(ParserFun, Content) when is_function(ParserFun, 1) ->
    ParserFun(Content);
run_parser(Parser, _Content) ->
    throw({error, {unsupported_parser, Parser}}).

validate_code_blocks({error, Reason}) ->
    throw({error, {parser_error, Reason}});
validate_code_blocks(Blocks) when is_list(Blocks) ->
    [validate_code_block(Block) || Block <- Blocks];
validate_code_blocks(Other) ->
    throw({error, {invalid_parser_result, Other}}).

validate_code_block(Block) when is_binary(Block) ->
    Block;
validate_code_block(Other) ->
    throw({error, {invalid_code_block, Other}}).

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
    proplists:get_value(skipped_blocks, Options, 0).

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

-define(RE_CAPTURE, ~"(?:(?'line_number'[0-9]+)(?'prefix'>\s))?(?'content'.*)").
-define(RE_OPTIONS, [{capture, [line_number, prefix, content] ,binary}, unicode]).

run_test(Code, InitialBindings) ->
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
                    check_prompt_numbers(ReLines, 1),
                    Tests = inspect(parse_tests(ReLines, [])),
                    _ = lists:foldl(fun(Test, Bindings) ->
                                            run_tests(Test, Bindings)
                                    end, InitialBindings, Tests),
                    [ok];
                _ ->
                    []
            end
    end.

check_prompt_numbers([], _Expected) ->
    ok;
check_prompt_numbers([{match, [<<>>|_]} | T], Expected) ->
    check_prompt_numbers(T, Expected);
check_prompt_numbers([{match, [LineNumber, _, Code]} | T], Expected) ->
    case binary_to_integer(LineNumber) of
        Expected ->
            check_prompt_numbers(T, Expected + 1);
        Actual ->
            Message = io_lib:format("Bad prompt number ~p; expected ~p",
                                    [Actual,Expected]),
            throw({error,{Message,Code}})
    end.

parse_tests([], []) ->
    [];
parse_tests([], Cmd) ->
    [{test, lists:join($\n, lists:reverse(Cmd)), "_"}];
parse_tests([{match, [<<>>, <<>>, <<>>]} | T], Cmd) ->
    parse_tests(T, Cmd);
parse_tests([{match, [_, <<"> ">>, NewCmd]} | T], []) ->
    parse_tests(T, [NewCmd]);
parse_tests([{match, [_, <<"> ">>, NewCmd]} | T], Cmd) ->
    [{test, lists:join($\n, lists:reverse(Cmd)), "_"} | parse_tests(T, [NewCmd])];
parse_tests([{match, [<<>>, <<>>, <<" ", _/binary>> = More]} | T], Acc) ->
    parse_tests(T, [More | Acc]);
parse_tests([{match, [<<>>, <<>>, NewMatch]} | T], Cmd) ->
    {Match, Rest} = parse_match(T, [NewMatch]),
    [{test, lists:join($\n, lists:reverse(Cmd)),
      lists:join($\n, lists:reverse(Match))} | parse_tests(Rest, [])].

parse_match([{match, [<<>>, <<>>, <<>>]} | T], Acc) ->
    parse_match(T, Acc);
parse_match([{match, [<<>>, <<>>, <<" ", _/binary>> = More]} | T], Acc) ->
    parse_match(T, [More | Acc]);
parse_match(Rest, Acc) ->
    {Acc, Rest}.

run_tests({test, Test0, Match0}, Bindings) ->
    Test1 = unicode:characters_to_list(Test0),
    Test = string:trim(string:trim(Test1), trailing, "."),
    case Match0 of
        [<<"** ", _/binary>> | _] ->
            Match = unicode:characters_to_list(Match0),
            Cmd = Test ++ ".",
            run_failing(Cmd, Test, Match, Bindings);
        _ ->
            Cmd = [unicode:characters_to_list(Match0),
                   " = begin ",
                   Test,
                   " end."],
            run_successful(Cmd, Test, Match0, Bindings)
    end.

run_successful(Cmd, Test, Match, Bindings) ->
    Ast = rewrite(parse(Cmd, Test, Match)),
    try
        {value, _Res, NewBindings} = inspect(erl_eval:exprs(Ast, Bindings)),
        NewBindings
    catch C:R:ST ->
            Actual = format_exception(C, R, ST),
            Message = io_lib:format("Expected value:~n~ts~n"
                                    "Got failure:~n~ts~n",
                                    [Match,Actual]),
            throw({error,{Message,Match}})
    end.

run_failing(Cmd, Test, Match, Bindings) ->
    Ast = parse(Cmd, Test, Match),
    try inspect(erl_eval:exprs(Ast, Bindings)) of
        {value, Res, _} ->
            Message = io_lib:format("Expected failure ~ts; got ~ts",
                                    [Match,Res]),
            throw({error,{Message,Match}})
    catch C:R:ST ->
            case format_exception(C, R, ST) of
                Match ->
                    Bindings;
                Actual ->
                    Message = io_lib:format("Expected failure:~n~ts~n"
                                            "Got failure:~n~ts~n",
                                            [Match,Actual]),
                    throw({error,{Message,Match}})
            end
    end.

parse(Cmd0, _Test, _Match) ->
    Cmd = lists:flatten(Cmd0),
    maybe
        {ok, T, _} ?= erl_scan:string(Cmd),
        RewrittenToks = rewrite_tokens(T),
        {ok, Ast} ?= inspect(erl_parse:parse_exprs(RewrittenToks)),
        Ast
    else
        {error, {Line,Mod,Reason}, _} ->
            Message = Mod:format_error(Reason),
            throw({error,{Message,Line,Cmd}});
        {error, {Line,Mod,Reason}} ->
            Message = Mod:format_error(Reason),
            throw({error,{Message,Line,Cmd}})
    end.

%% We rewrite ... to _ to match shell syntax better
rewrite_tokens([{'...', L} | T]) ->
    rewrite_tokens([{var, L, '_'} | T]);
rewrite_tokens([H | T]) ->
    [H | rewrite_tokens(T)];
rewrite_tokens([]) ->
    [].

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

rewrite([{match, Ann, LHS, RHS} | Rest]) ->
    [{match, Ann, rewrite_map_match(LHS), RHS} | Rest].

rewrite_map_match(AST) ->
    erl_syntax:revert(
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
        end, AST)).

inspect(Term) ->
    %% Uncomment for debugging
    %% io:format("~tp~n",[Term]),
    Term.
