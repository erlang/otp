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
> 1+2.
3
```

## Basic example using erlang code:

```erlang
> 1+2.
3
```

## Multi-line prompt example:

```erlang
> 1
  +
  2
  .
3
```

## Multi-line with comma example:

```erlang
> A = 1,
  A + 2.
3
```

## Multi-match example:

```erlang
> [1, 2].
[
 1
 ,
 2
 ]
```

## Multiple prompts:

```
> 1 + 2.
3
> 3 + 4.
7
```

## Ignore result:

```
> 1 + 2.
```

## Defining variables:

```
> A = 1+2.
> A + 3.
6
```

## Comments:

```
> [1, 
% A comment in between prompts
  2].
[1,
% A comment in a match
 2]
> [1, 
  % Indented comment in between prompts
  2].
[1,
 % Indented comment in a match
 2]
```

## Prebound variables:

```
> Prebound.
hello
```

## Matching of maps:

```
> #{ a => b }.
#{ a => b }
```

""".
-spec module(#docs_v1{}, erl_eval:binding_struct()) -> _.
module(#docs_v1{ docs = Docs, module_doc = MD }, Bindings) ->
    MDRes = parse_and_run(module_doc, MD, Bindings),
    case lists:flatten(
           [parse_and_run(KFA, EntryDocs, Bindings)
            || {KFA, _Anno, _Sig, EntryDocs, _Meta} <- Docs, is_map(EntryDocs)] ++ MDRes) of
        [] ->
            ok;
        Else ->
            Else
    end.

parse_and_run(_, hidden, _) -> [];
parse_and_run(_, none, _) -> [];
parse_and_run(KFA, #{} = Ds, Bindings) ->
    [parse_and_run(KFA, D, Bindings) || _ := D <- Ds];
parse_and_run(KFA, Docs, Bindings) ->
    InitialBindings = proplists:get_value(KFA, Bindings, erl_eval:new_bindings()),
    io:format("Testing: ~p~n",[KFA]),
    case test(inspect(shell_docs_markdown:parse_md(Docs)), InitialBindings) of
        [] -> [];
        Else ->
            {KFA, lists:flatten(Else)}
    end.

test({pre,[],[{code,Attrs,[<<">",_/binary>> = Code]}]}, Bindings) ->
    case proplists:get_value(class, Attrs, ~"language-erlang") of
        ~"language-erlang" ->
            run_test(Code, Bindings);
        _ ->
            test(Code, Bindings)
    end;
test({_Tag,_Attr, Content}, Bindings) ->
    test(Content, Bindings);
test([H | T], Bindings) ->
    [test(H, Bindings) | test(T, Bindings)];
test(Text, _Bindings) when is_binary(Text); Text =:= [] ->
    [].

run_test(Code, InitialBindings) ->
    Lines = string:split(Code, "\n", all),
    Tests = inspect(parse_tests(Lines, [])),
    lists:foldl(fun(Test, Bindings) ->
                        run_tests(Test, Bindings)
                end, InitialBindings, Tests).

parse_tests([], []) ->
    [];
parse_tests([], Cmd) ->
    [{test, lists:join($\n, lists:reverse(Cmd)), "_"}];
parse_tests([<<>>|T], Cmd) ->
    parse_tests(T, Cmd);
parse_tests([<<"%", _Skip/binary>> | T], Cmd) ->
    parse_tests(T, Cmd);
parse_tests([<<"> ", NewCmd/binary>> | T], []) ->
    parse_tests(T, [NewCmd]);
parse_tests([<<"> ", NewCmd/binary>> | T], Cmd) ->
    [{test, lists:join($\n, lists:reverse(Cmd)), "_"} | parse_tests(T, [NewCmd])];
parse_tests([<<" ", More/binary>> | T], Acc) ->
    parse_tests(T, [More | Acc]);
parse_tests([NewMatch | T], Cmd) ->
    {Match, Rest} = parse_match(T, [NewMatch]),
    [{test, lists:join($\n, lists:reverse(Cmd)),
      lists:join($\n, lists:reverse(Match))} | parse_tests(Rest, [])].

parse_match([<<"%", _Skip/binary>> | T], Acc) ->
    parse_match(T, Acc);
parse_match([<<" ", More/binary>> | T], Acc) ->
    parse_match(T, [More | Acc]);
parse_match(Rest, Acc) ->
    {Acc, Rest}.

run_tests({test, Test, Match}, Bindings) ->
    maybe
        Cmd = [unicode:characters_to_list(Match), " = begin ",
                string:trim(string:trim(unicode:characters_to_list(Test)), trailing, "."), " end."],
        {ok, T, _} ?= erl_scan:string(lists:flatten(Cmd)),
        {ok, Ast0} ?= inspect(erl_parse:parse_exprs(T)),
        Ast = rewrite(Ast0),
        try
            {value, _Res, NewBindings} = inspect(erl_eval:exprs(Ast, Bindings)),
            NewBindings
        catch E:R:ST ->
                io:format("~p~n", [Ast]),
                erlang:raise(E,R,ST)
        end
    else
        Else -> throw({iolist_to_binary(Test), iolist_to_binary(Match), Else})
    end.

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
