%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% SPDX-FileCopyrightText: Copyright Ericsson AB 1996-2025. All Rights Reserved.
%%
%% %CopyrightEnd%
%%
-module(shell_docs_test).
-moduledoc false.

-include_lib("kernel/include/eep48.hrl").

-export([module/2]).

-doc """
Here are some examples of what should work:

## Basic example:

```
1> 1+2.
3
```

## Basic example using erlang code:

```erlang
1> 1+2.
3
```

## Multi-line prompt example:

```erlang
1> 1
  +
  2
  .
3
```

## Multi-line with comma example:

```erlang
1> A = 1,
  A + 2.
3
```

## Multi-match example:

```erlang
1> [1, 2].
[
 1
 ,
 2
 ]
```

## Multiple prompts:

```
1> 1 + 2.
3
2> 3 + 4.
7
```

## Ignore result:

```
1> 1 + 2.
```

## Defining variables:

```
1> A = 1+2.
2> A + 3.
6
```

## Matching exceptions.

```
1> hello + 1.
** exception error: an error occurred when evaluating an arithmetic expression
     in operator  +/2
        called as hello + 1
2> lists:last([]).
** exception error: no function clause matching lists:last([])
```

## Comments:

```
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

## Prebound variables:

```
1> Prebound.
hello
```

## Matching of maps:

```
1> #{ a => b }.
#{ a => b }
```

## Edge cases:

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

```
%% should probably be tested?
1> ok.
```

```
%% should probably be tested?

1> ok.
```
""".
-spec module(#docs_v1{}, erl_eval:binding_struct()) -> _.
module(#docs_v1{ docs = Docs, module_doc = MD }, Bindings) ->
    MDRes = [parse_and_run(module_doc, MD, Bindings)],
    Res0 = [parse_and_run(KFA, EntryDocs, Bindings) ||
               {KFA, _Anno, _Sig, EntryDocs, _Meta} <- Docs,
               is_map(EntryDocs)] ++ MDRes,
    Res = lists:append(Res0),
    Errors = [{{F,A},E} || {{function,F,A},[{error,E}]} <- Res],
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
        {KFA, test(Items, InitialBindings)}
    catch
        throw:{error,_}=Error ->
            {KFA, [Error]};
        C:R:ST ->
            io:format("Uncaught exception in ~p~n", [KFA]),
            erlang:raise(C, R, ST)
    end.

test(Tests, Bindings) ->
    lists:flatmap(fun(Test) -> test_item(Test, Bindings) end, Tests).

test_item({pre,[],[{code,Attrs,[Code]}]}, Bindings) when is_binary(Code) ->
    case proplists:get_value(class, Attrs, ~"language-erlang") of
        ~"language-erlang" ->
            run_test(Code, Bindings);
        _ ->
            []
    end;
test_item({_Tag,_Attr, Content}, Bindings) ->
    test(Content, Bindings);
test_item(Header, _Bindings) when is_binary(Header) ->
    [].

-define(RE_CAPTURE, ~"(?:(?'line_number'[0-9]+)(?'prefix'>\s)|(?'prefix'%))?(?'content'.*)").
-define(RE_OPTIONS, [{capture, [line_number, prefix, content] ,binary}, dupnames]).

run_test(Code, InitialBindings) ->
    Lines = string:split(Code, "\n", all),
    ReLines = [re:run(Line, ?RE_CAPTURE, ?RE_OPTIONS) || Line <- Lines],
    case ReLines of
        [{match, [_Line_Number, _Prefix = <<"> ">>, _Code]} | _] ->
            check_line_numbers(ReLines, 1),
            Tests = inspect(parse_tests(ReLines, [])),
            _ = lists:foldl(fun(Test, Bindings) ->
                                    run_tests(Test, Bindings)
                            end, InitialBindings, Tests),
            [ok];
        [{match, [_Line_Number, _Prefix = <<"%">>, _Skip]} | _] ->
            Tests = inspect(parse_tests(ReLines, [])),
            _ = lists:foldl(fun(Test, Bindings) ->
                                    run_tests(Test, Bindings)
                            end, InitialBindings, Tests),
            [ok];
        _ ->
            []
    end.

check_line_numbers([], _Expected) ->
    ok;
check_line_numbers([{match, [<<>>|_]} | T], Expected) ->
    check_line_numbers(T, Expected);
check_line_numbers([{match, [LineNumber, _, Code]} | T], Expected) ->
    case binary_to_integer(LineNumber) of
        Expected ->
            check_line_numbers(T, Expected + 1);
        Actual ->
            Message = io_lib:format("Bad line number ~p; expected ~p",
                                    [Actual,Expected]),
            throw({error,{Message,Code}})
    end.

parse_tests([], []) ->
    [];
parse_tests([], Cmd) ->
    [{test, lists:join($\n, lists:reverse(Cmd)), "_"}];
parse_tests([{match, [<<>>, <<>>, <<>>]} | T], Cmd) ->
    parse_tests(T, Cmd);
parse_tests([{match, [<<>>, <<"%">>, _Skip]} | T], Cmd) ->
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

parse_match([{match, [<<>>, <<"%">>, _Skip]} | T], Acc) ->
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
    Ast0 = parse(Cmd, Test, Match),
    Ast = rewrite(Ast0),
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
        {ok, Ast} ?= inspect(erl_parse:parse_exprs(T)),
        Ast
    else
        {error, {_Line,Mod,Reason}, _} ->
            Message = Mod:format_error(Reason),
            throw({error,{Message,Cmd}});
        {error, {_Line,Mod,Reason}} ->
            Message = Mod:format_error(Reason),
            throw({error,{Message,Cmd}})
    end.

format_exception(Class, Reason, [{M,F,A,Info0},Item|_]) ->
    Info = lists:keydelete(line, 1, Info0),
    Stacktrace = [{M,F,A,Info},Item],
    Tag = "** ",
    I = iolist_size(Tag) + 1,
    PF = fun pp/2,
    SF = fun(Mod, _, _) -> Mod =:= erl_eval end,
    Enc = unicode,
    Str = erl_error:format_exception(I, Class, Reason, Stacktrace, SF, PF, Enc),
    Tag ++ string:trim(lists:flatten(Str), trailing).

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
