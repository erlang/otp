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
`ct_doctest` runs doctests embedded in EEP-48 markdown documentation, normally
written using [documentation attributes](`e:system:documentation.md`).

Using `ct_doctest` ensures that the examples in your documentation are correct, up to date,
and stylistically consistent.

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
bindings when calling `test/2`. For example, if you have a module doc that uses a variable `Prebound`,
you can set it up like this:

```
1> Prebound.
hello
```

and then in your test suite:

```
binding_test(_Config) ->
    Prebound = erl_eval:add_binding('Prebound', hello, erl_eval:new_bindings()),
    ct_doctest:module(my_module, [{bindings, [{module_doc, Prebound}]}]).
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

-export([test/2]).

-doc "Variable bindings passed as option to `module/2` or `file/2`.".
-type doc_binding() :: {{function | type | callback, atom(), non_neg_integer()}
                         | module_doc, erl_eval:binding_struct()}.

-doc """
Run doctests for a module with markdown EEP-48 docs.

When calling `module/2`, `ct_doctest` looks for documentation in the specified module and
runs any examples found there. The module, function, type, and callback documentation
are all checked for examples.

The function returns `ok` if all tests pass, or `{comment, Comment}` if all tests pass but one or more
functions lack tests. If any test fails, an exception in the form of `error({N, errors})` is raised,
where `N` is the number of failed tests. The details of each failure are printed to the console.

`Bindings` can provide prebound variables for a specific doc entry. Use
`module_doc` for module docs and `{function, Name, Arity}` (or corresponding
`type`/`callback` keys) for entry-specific bindings.
""".
-spec test(module(), [doc_binding()]) -> ok | {comment, string()} | {error, term()} | no_return().
test(Module, Bindings) ->
    case code:get_doc(Module) of
        {ok, #docs_v1{ format = ~"text/markdown" } = Docs} ->
            module(Docs, Bindings);
        {ok, _} ->
            {error, unsupported_format};
        Else ->
            Else
    end.

module(#docs_v1{ docs = Docs, module_doc = MD }, Bindings) ->
    MDRes = lists:append([parse_and_run(module_doc, MD, Bindings)]),
    Res =
        lists:append(
          [parse_and_run(KFA, EntryDocs, Bindings) ||
              {KFA, _Anno, _Sig, EntryDocs, _Meta} <- Docs,
              is_map(EntryDocs)]),
    Errors =
        [{{F,A},E} || {{function,F,A},[{error,E}]} <- Res]
        ++ [{module_doc,E} || {module_doc,[{error,E}]} <- MDRes],
    _ = [print_error(E) || E <- Errors],
    case length(Errors) of
        0 ->
            NoTests = lists:sort([io_lib:format("  ~p/~p\n", [F,A]) ||
                                     {{function,F,A},[]} <- Res]),
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

print_error({module_doc,{Message,Line,Context}}) ->
    io:format("Module Doc:~p: ~ts~n~ts~n", [Line,Context,Message]);
print_error({module_doc,{Message,Context}}) ->
    io:format("Module Doc: ~ts~n~ts~n", [Context,Message]);
print_error({{Name,Arity},{Message,Line,Context}}) ->
    io:format("~p/~p:~p: ~ts~n~ts~n", [Name,Arity,Line,Context,Message]);
print_error({{Name,Arity},{Message,Context}}) ->
    io:format("~p/~p: ~ts~n~ts~n", [Name,Arity,Context,Message]).

parse_and_run(_, hidden, _) -> [];
parse_and_run(_, none, _) -> [];
parse_and_run(KFA, #{} = Ds, Bindings) ->
    [do_parse_and_run(KFA, D, Bindings) || _ := D <- Ds].

do_parse_and_run(KFA, Docs, Bindings) ->
    try
        InitialBindings = proplists:get_value(KFA, Bindings, erl_eval:new_bindings()),
        Items = inspect(shell_docs_markdown:parse_md(Docs)),
        {KFA, run_items(Items, InitialBindings)}
    catch
        throw:{error,_}=Error ->
            {KFA, [Error]};
        C:R:ST ->
            io:format("Uncaught exception in ~p~n", [KFA]),
            erlang:raise(C, R, ST)
    end.

run_items(Tests, Bindings) ->
    lists:flatmap(fun(Test) -> test_item(Test, Bindings) end, Tests).

test_item({pre,[],[{code,Attrs,[Code]}]}, Bindings) when is_binary(Code) ->
    case proplists:get_value(class, Attrs, ~"language-erlang") of
        ~"language-erlang" ->
            run_test(Code, Bindings);
        _ ->
            []
    end;
test_item({_Tag,_Attr, Content}, Bindings) ->
    run_items(Content, Bindings);
test_item(Header, _Bindings) when is_binary(Header) ->
    [].

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
    erl_syntax_lib:map(fun(Tree) ->
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
%    io:format("~tp~n",[Term]),
    Term.
