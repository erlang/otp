%% ---------------------------------------------------------------------
%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
%%
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2012-2015 Richard Carlsson
%% @doc Unit tests for merl.
%% @private

-module(merl_tests).

%-define(MERL_NO_TRANSFORM, true).
-include("merl.hrl").

-include_lib("eunit/include/eunit.hrl").


%% utilities

f(Ts) when is_list(Ts) ->
    lists:flatmap(fun erl_prettypr:format/1, Ts);
f(T) ->
    erl_prettypr:format(T).

fe(Env) -> [{Key, f(T)} || {Key, T} <- Env].

g_exported_() ->
    %% for testing the parse transform, autoexported to avoid complaints
    {ok, merl:quote(?LINE, "42")}.


ok({ok, X}) -> X.


%%
%% tests
%%

parse_error_test_() ->
    [?_assertThrow({error, "1: syntax error before: '{'" ++ _},
                   f(merl:quote("{")))
    ].

transform_parse_error_test_() ->
    [?_assertEqual("merl:quote(\"{\")",
                   f(merl_transform:parse_transform(
                       [?Q("merl:quote(\"{\")")], []))),
     ?_assertEqual("merl:quote(2, \"{\")",
                   f(merl_transform:parse_transform(
                       [?Q("merl:quote(2, \"{\")")], []))),
     ?_assertEqual("merl:qquote(\"{\", [{var, V}])",
                   f(merl_transform:parse_transform(
                       [?Q("merl:qquote(\"{\", [{var, V}])")], []))),
     ?_assertEqual("merl:qquote(2, \"{\", [{var, V}])",
                   f(merl_transform:parse_transform(
                       [?Q("merl:qquote(2, \"{\", [{var, V}])")], [])))
    ].

term_test_() ->
    [?_assertEqual(tuple, erl_syntax:type(merl:term({}))),
     ?_assertEqual("{foo, 42}", f(merl:term({foo, 42})))
    ].

quote_form_test_() ->
    [?_assertEqual("f(X) -> {ok, X}.",
                   f(?Q("f(X) -> {ok, X}."))),
     ?_assertEqual("-module(foo).",
                   f(?Q("-module(foo)."))),
     ?_assertEqual("-import(bar, [f/1, g/2]).",
                   f(?Q("-import(bar, [f/1, g/2])."))),
     ?_assertEqual(("-module(foo)."
                    "-export([f/1])."
                    "f(X) -> {ok, X}."),
                   f(?Q(["-module(foo).",
                         "-export([f/1]).",
                         "f(X) -> {ok, X}."])))
    ].

quote_term_test_() ->
    [?_assertEqual("foo",
                   f(?Q("foo"))),
     ?_assertEqual("42",
                   f(?Q("42"))),
     ?_assertEqual("{foo, 42}",
                   f(?Q("{foo, 42}"))),
     ?_assertEqual(("1" ++ "2" ++ "3"),
                   f(?Q("1, 2, 3"))),
     ?_assertEqual(("foo" "42" "{}" "true"),
                   f(?Q("foo, 42, {}, (true)")))
    ].

quote_expr_test_() ->
    [?_assertEqual("2 + 2",
                   f(?Q("2 + 2"))),
     ?_assertEqual("f(foo, 42)",
                   f(?Q("f(foo, 42)"))),
     ?_assertEqual("case X of\n  a -> 1;\n  b -> 2\nend",
                   f(?Q("case X of a -> 1; b -> 2 end"))),
     ?_assertEqual(("2 + 2" ++ "f(42)" ++ "catch 22"),
                   f(?Q("2 + 2, f(42), catch 22")))
    ].

quote_try_clause_test_() ->
    [?_assertEqual("(error:R) when R =/= foo -> ok",
                   f(?Q("error:R when R =/= foo -> ok"))),
     %% note that without any context, clauses are printed as fun-clauses
     ?_assertEqual(("(error:badarg) -> badarg"
                    "(exit:normal) -> normal"
                    "(_) -> other"),
                   f(?Q(["error:badarg -> badarg;",
                         "exit:normal -> normal;"
                         "_ -> other"])))
    ].

quote_fun_clause_test_() ->
    [?_assertEqual("(X, Y) when X < Y -> {ok, X}",
                   f(?Q("(X, Y) when X < Y -> {ok, X}"))),
     ?_assertEqual(("(X, Y) when X < Y -> less"
                    "(X, Y) when X > Y -> greater"
                    "(_, _) -> equal"),
                   f(?Q(["(X, Y) when X < Y -> less;",
                         "(X, Y) when X > Y -> greater;"
                         "(_, _) -> equal"])))].

quote_case_clause_test_() ->
    [?_assertEqual("({X, Y}) when X < Y -> X",
                   f(?Q("{X, Y} when X < Y -> X"))),
     ?_assertEqual(("({X, Y}) when X < Y -> -1"
                    "({X, Y}) when X > Y -> 1"
                    "(_) -> 0"),
                   f(?Q(["{X, Y} when X < Y -> -1;",
                         "{X, Y} when X > Y -> 1;"
                         "_ -> 0"])))].

quote_comment_test_() ->
    [?_assertEqual("%% comment preserved\n"
                   "{foo, 42}",
                  f(?Q(["%% comment preserved",
                        "{foo, 42}"]))),
     ?_assertEqual("{foo, 42}"
                   "%% comment preserved\n",
                   f(?Q(["{foo, 42}",
                         "%% comment preserved"]))),
     ?_assertEqual("  % just a comment (with indent)\n",
                  f(?Q("  % just a comment (with indent)")))
    ].

metavar_test_() ->
    [?_assertEqual("'@foo'", f(merl:tree(merl:template(?Q("'@foo'"))))),
     ?_assertEqual("'@foo'", f(merl:tree(merl:template(?Q("_@foo"))))),
     ?_assertEqual("'@foo'", f(merl:tree(merl:template(?Q("\"'@foo\""))))),
     ?_assertEqual("{'@foo'}", f(merl:tree(merl:template(?Q("{_@foo}"))))),
     ?_assertEqual("'@foo'", f(merl:tree(merl:template(?Q("{_@_foo}"))))),
     ?_assertEqual("909123", f(merl:tree(merl:template(?Q("{9090123}"))))),
     ?_assertEqual("{'@foo'}",
                   f(merl:tree(merl:template(?Q("{{{_@__foo}}}"))))),
     ?_assertEqual("{909123}",
                   f(merl:tree(merl:template(?Q("{{{90900123}}}"))))),
     ?_assertEqual("{'@@foo'}",
                   f(merl:tree(merl:template(?Q("{{{_@__@foo}}}"))))),
     ?_assertEqual("{9099123}",
                   f(merl:tree(merl:template(?Q("{{{909009123}}}")))))
    ].

subst_test_() ->
    [?_assertEqual("42",
                   f(merl:subst(?Q("_@foo"), [{foo, merl:term(42)}]))),
     ?_assertEqual("'@foo'",
                   f(merl:subst(?Q("_@foo"), []))),
     ?_assertEqual("{42}",
                   f(merl:subst(?Q("{_@foo}"),
                                [{foo, merl:term(42)}]))),
     ?_assertEqual("{'@foo'}",
                   f(merl:subst(?Q("{_@foo}"), []))),
     ?_assertEqual("fun bar/0",
                   f(merl:subst(merl:template(?Q("fun '@foo'/0")),
                                [{foo, merl:term(bar)}]))),
     ?_assertEqual("fun foo/3",
                   f(merl:subst(merl:template(?Q("fun foo/9091")),
                                [{1, merl:term(3)}]))),
     ?_assertEqual("[42]",
                   f(merl:subst(merl:template(?Q("[_@foo]")),
                                [{foo, merl:term(42)}]))),
     ?_assertEqual("[foo, bar]",
                   f(merl:subst(merl:template(?Q("[_@foo]")),
                                [{foo, [merl:term(foo),merl:term(bar)]}]))),
     ?_assertEqual("{fee, fie, foe, fum}",
                   f(merl:subst(merl:template(?Q("{fee, _@foo, fum}")),
                                [{foo, [merl:term(fie),merl:term(foe)]}]))),
     ?_assertEqual("[foo, bar]",
                   f(merl:subst(merl:template(?Q("[_@@foo]")),
                                [{foo, [merl:term(foo),merl:term(bar)]}]))),
     ?_assertEqual("{fee, fie, foe, fum}",
                   f(merl:subst(merl:template(?Q("{fee, _@@foo, fum}")),
                                [{foo, [merl:term(fie),merl:term(foe)]}]))),
     ?_assertEqual("['@@foo']",
                   f(merl:subst(merl:template(?Q("[_@@foo]")), []))),
     ?_assertEqual("foo",
                   f(merl:subst(merl:template(?Q("[_@_foo]")),
                                [{foo, merl:term(foo)}]))),
     ?_assertEqual("{'@foo'}",
                   f(merl:subst(merl:template(?Q("{[_@_foo]}")), []))),
     ?_assertEqual("{'@@foo'}",
                   f(merl:subst(merl:template(?Q("{[_@_@foo]}")), []))),
     ?_assertEqual("-export([foo/1, bar/2]).",
                   f(merl:subst(merl:template(?Q("-export(['@_@foo'/0]).")),
                                [{foo, [erl_syntax:arity_qualifier(
                                          merl:term(foo),
                                          merl:term(1)),
                                        erl_syntax:arity_qualifier(
                                          merl:term(bar),
                                          merl:term(2))
                                       ]}
                                ])))
    ].

match_test_() ->
    [?_assertEqual({ok, []}, merl:match(?Q("foo"), ?Q("foo"))),
     ?_assertEqual(error,    merl:match(?Q("foo"), ?Q("bar"))),
     ?_assertEqual({ok,[]},  merl:match(?Q("{foo,42}"), ?Q("{foo,42}"))),
     ?_assertEqual(error,    merl:match(?Q("{foo,42}"), ?Q("{foo,bar}"))),
     ?_assertEqual({ok,[]},  merl:match(?Q("[foo,[42]]"), ?Q("[foo,[42]]"))),
     ?_assertEqual(error,    merl:match(?Q("[foo,[42]]"), ?Q("[foo,{42}]"))),
     ?_assertEqual({ok,[]},  merl:match(?Q("[foo,[_@_]]"),
                                        ?Q("[foo,[42]]"))),
     ?_assertEqual({ok,[]},  merl:match(?Q("[foo,[9090]]"),
                                        ?Q("[foo,[42]]"))),
     ?_assertEqual({ok,[]},  merl:match(?Q("{_@_,[_@_,2]}"),
                                        ?Q("{foo,[1,2]}"))),
     ?_assertEqual(error,    merl:match(?Q("{_@_,[_@_,2]}"),
                                        ?Q("{foo,[1,3]}"))),
     ?_assertEqual({ok,[]},  merl:match(?Q("[foo,[9090,9090]]"),
                                        ?Q("[foo,[1,2]]"))),
     ?_assertEqual(error,    merl:match(?Q("[foo,[9090,9090]]"),
                                        ?Q("[foo,[1,2,3]]"))),
     ?_assertEqual([{foo,"42"}],
                   fe(ok(merl:match(?Q("_@foo"), ?Q("42"))))),
     ?_assertEqual([{foo,"42"}],
                   fe(ok(merl:match(?Q("{_@foo}"), ?Q("{42}"))))),
     ?_assertEqual([{1,"0"},{foo,"bar"}],
                   fe(ok(merl:match(?Q("fun '@foo'/9091"),
                                    ?Q("fun bar/0"))))),
     ?_assertEqual([{line,"17"},{text,"\"hello\""}],
                   fe(ok(merl:match(?Q("{_@line, _@text}"),
                                    ?Q("{17, \"hello\"}"))))),
     ?_assertEqual([{line,"17"},{text,"\"hello\""}],
                   fe(ok(merl:match(?Q("foo(_@line, _@text)"),
                                    ?Q("foo(17, \"hello\")"))))),
     ?_assertEqual([{foo,""}],
                   fe(ok(merl:match(?Q("f(_@@foo)"),
                                    ?Q("f()"))))),
     ?_assertEqual([{foo,"fee"}],
                   fe(ok(merl:match(?Q("f(_@@foo)"),
                                    ?Q("f(fee)"))))),
     ?_assertEqual([{foo,"feefiefum"}],
                   fe(ok(merl:match(?Q("f(_@@foo)"),
                                    ?Q("f(fee, fie, fum)"))))),
     ?_assertEqual([{foo,""}],
                   fe(ok(merl:match(?Q("[_@@foo]"),
                                    ?Q("[]"))))),
     ?_assertEqual([{foo,"fee"}],
                   fe(ok(merl:match(?Q("[_@@foo]"),
                                    ?Q("[fee]"))))),
     ?_assertEqual([{foo,"feefiefoefum"}],
                   fe(ok(merl:match(?Q("[_@@foo]"),
                                    ?Q("[fee, fie, foe, fum]"))))),
     ?_assertEqual([{foo,""}],
                   fe(ok(merl:match(?Q("{_@@foo}"),
                                    ?Q("{}"))))),
     ?_assertEqual([{foo,"fee"}],
                   fe(ok(merl:match(?Q("{_@@foo}"),
                                    ?Q("{fee}"))))),
     ?_assertEqual([{foo,"feefiefoefum"}],
                   fe(ok(merl:match(?Q("{_@@foo}"),
                                    ?Q("{fee, fie, foe, fum}"))))),
     ?_assertEqual([{foo,"fie"}],
                   fe(ok(merl:match(?Q("{fee, _@@foo}"),
                                    ?Q("{fee, fie}"))))),
     ?_assertEqual([{foo,"fiefoefum"}],
                   fe(ok(merl:match(?Q("{fee, _@@foo}"),
                                    ?Q("{fee, fie, foe, fum}"))))),
     ?_assertEqual([{foo,"fie"}],
                   fe(ok(merl:match(?Q("{_@@foo, foe, fum}"),
                                    ?Q("{fie, foe, fum}"))))),
     ?_assertEqual([{foo,"feefie"}],
                   fe(ok(merl:match(?Q("{_@@foo, foe, fum}"),
                                    ?Q("{fee, fie, foe, fum}"))))),
     ?_assertEqual([{foo,"fie"}],
                   fe(ok(merl:match(?Q("{fee, _@@foo, fum}"),
                                    ?Q("{fee, fie, fum}"))))),
     ?_assertEqual([{foo,"fiefoe"}],
                   fe(ok(merl:match(?Q("{fee, _@@foo, fum}"),
                                    ?Q("{fee, fie, foe, fum}"))))),
     ?_assertEqual([{foo,"fiefoe"},{post,"fum"},{pre,"fee"}],
                   fe(ok(merl:match(?Q("{_@pre, _@@foo, _@post}"),
                                    ?Q("{fee, fie, foe, fum}"))))),
     ?_assertThrow({error, "multiple glob variables"++_},
                   fe(ok(merl:match(?Q("{_@@foo, _@@bar}"),
                                    ?Q("{fee, fie, foe, fum}"))))),
     ?_assertEqual([],
                   fe(ok(merl:match(?Q("{fee, _@@_}"),
                                    ?Q("{fee, fie, foe, fum}"))))),
     ?_assertEqual([],
                   fe(ok(merl:match(?Q("{_@@_, foe, fum}"),
                                    ?Q("{fee, fie, foe, fum}"))))),
     ?_assertEqual([{post,"fum"},{pre,"fee"}],
                   fe(ok(merl:match(?Q("{_@pre, _@@_, _@post}"),
                                    ?Q("{fee, fie, foe, fum}")))))
    ].

switch_test_() ->
    [?_assertEqual(42, merl:switch(?Q("foo"), [fun () -> 42 end])),
     ?_assertEqual(17, merl:switch(?Q("foo"), [fun () -> 17 end,
                                               fun () -> 42 end])),
     ?_assertEqual(17, merl:switch(?Q("foo"), [{?Q("foo"),
                                                fun ([]) -> 17 end},
                                               fun () -> 42 end])),
     ?_assertEqual(17,
                   merl:switch(?Q("foo"), [{?Q("bar"), fun ([]) -> 0 end},
                                           {?Q("foo"), fun ([]) -> 17 end},
                                           fun () -> 42 end])),
     ?_assertEqual([{foo,"17"}],
                   merl:switch(?Q("{foo,17}"),
                               [{?Q("{bar, _@foo}"), fun (_) -> 0 end},
                                {?Q("{foo, _@foo}"), fun fe/1},
                                fun () -> 42 end])),
     ?_assertEqual(17,
                   merl:switch(?Q("{foo, 17}"),
                               [{?Q("{foo, _@foo}"),
                                 fun ([{foo, X}]) -> f(X) =:= "17" end,
                                 fun (_) -> 17 end},
                                fun () -> 42 end])),
     ?_assertEqual([{foo,"17"}],
                   merl:switch(?Q("{foo, 17}"),
                               [{?Q("{foo, _@foo}"),
                                 fun ([{foo, X}]) -> f(X) =:= "42" end,
                                 fun (_) -> 0 end},
                                {?Q("{foo, _@foo}"), fun fe/1},
                                fun () -> 42 end])),
     ?_assertEqual(17,
                   merl:switch(?Q("{foo, 17}"),
                               [{?Q("{foo, _@foo}"),
                                 [{fun ([{foo, X}]) -> f(X) =:= "17" end,
                                   fun (_) -> 17 end},
                                  fun (_) -> 0 end]},
                                fun () -> 42 end])),
     ?_assertEqual([{foo,"17"}],
                   merl:switch(?Q("{foo, 17}"),
                               [{?Q("{foo, _@foo}"),
                                 [{fun ([{foo, X}]) -> f(X) =:= "42" end,
                                   fun (_) -> 0 end},
                                  fun fe/1]},
                                fun () -> 42 end]))
    ].

-ifndef(MERL_NO_TRANSFORM).

inline_meta_test_() ->
    [?_assertEqual("{foo}",
                   f(begin
                         Foo = ?Q("foo"),
                         ?Q("{_@Foo}")
                     end)),
     ?_assertEqual("{foo, '@bar'}",
                   f(begin
                         Foo = ?Q("foo"),
                         ?Q("{_@Foo,_@bar}")
                     end)),
     ?_assertEqual("{foo, '@bar'}",
                   f(begin
                         Q1 = ?Q("foo"),
                         ?Q("{90919,_@bar}")
                     end))
    ].

inline_meta_autoabstract_test_() ->
    [?_assertEqual("{foo}",
                   f(begin
                         Foo = foo,
                         ?Q("{_@Foo@}")
                     end)),
     ?_assertEqual("{foo, '@bar@'}",
                   f(begin
                         Foo = foo,
                         ?Q("{_@Foo@,_@bar@}")
                     end)),
     ?_assertEqual("{foo, '@bar@'}",
                   f(begin
                         Q1 = foo,
                         ?Q("{909199,_@bar@}")
                     end))
    ].

meta_match_test_() ->
    [?_assertEqual("{[bar], baz()}",
                   f(begin
                         Tree = ?Q("{foo, [bar], baz()}"),
                         ?Q("{foo, _@Bar, '@Baz'}") = Tree,
                         ?Q("{_@Bar, _@Baz}")
                     end)),
     ?_assertEqual("{[bar], baz()}",
                   f(begin
                         Tree = ?Q("{foo, [bar], baz()}"),
                         ?Q("{foo, 90919, 90929}") = Tree,
                         ?Q("{_@Q1, _@Q2}")
                     end)),
     ?_assertError({badmatch,error},
                   f(begin
                         Tree = ?Q("{foo, [bar], baz()}"),
                         ?Q("{fie, _@Bar, '@Baz'}") = Tree,
                         ?Q("{_@Bar, _@Baz}")
                     end))
    ].

meta_case_test_() ->
    [?_assertEqual("{[bar], baz()}",
                   f(begin
                         Tree = ?Q("{foo, [bar], baz()}"),
                         case Tree of
                             ?Q("{foo, _@Bar, '@Baz'}") -> ?Q("{_@Bar, _@Baz}")
                         end
                     end)),
     ?_assertEqual("{foo, [bar], baz()}",
                   f(begin
                         Tree = ?Q("{foo, [bar], baz()}"),
                         case Tree of
                             ?Q("{fie, _@Bar, '@Baz'}") -> ?Q("{_@Bar, _@Baz}");
                             _ -> Tree
                         end
                     end)),
     ?_assertError(merl_switch_clause,
                   f(begin
                         Tree = ?Q("{foo, [bar], baz()}"),
                         case Tree of
                             ?Q("{fie, _@Bar, '@Baz'}") -> ?Q("{_@Bar, _@Baz}")
                         end
                     end)),
     ?_assertEqual("{foo, 4}",
                   f(begin
                         Tree = ?Q("{foo, 3}"),
                         case Tree of
                             ?Q("{foo, _@N}") ->
                                 N1 = erl_syntax:concrete(N) + 1,
                                 ?Q("{foo, _@N1@}");
                             _ -> Tree
                         end
                     end)),
     ?_assertEqual("-export([f/4]).",
                   f(begin
                         Tree = ?Q("-export([f/3])."),
                         case Tree of
                             ?Q("-export([f/90919]).") ->
                                 Q2 = erl_syntax:concrete(Q1) + 1,
                                 ?Q("-export([f/909299]).");
                             _ -> Tree
                         end
                     end)),
     ?_assertEqual("{1, [bar], baz()}",
                   f(begin
                         Tree = ?Q("{foo, [bar], baz()}"),
                         case Tree of
                             ?Q("{foo, _@Bar, '@Baz'}") ->
                                 ?Q("{1, _@Bar, _@Baz}");
                             ?Q("{fie, _@Bar, '@Baz'}") ->
                                 ?Q("{2, _@Bar, _@Baz}");
                             _ -> Tree
                         end
                     end)),
     ?_assertEqual("{2, [bar], baz()}",
                   f(begin
                         Tree = ?Q("{fie, [bar], baz()}"),
                         case Tree of
                             ?Q("{foo, _@Bar, '@Baz'}") ->
                                 ?Q("{1, _@Bar, _@Baz}");
                             ?Q("{fie, _@Bar, '@Baz'}") ->
                                 ?Q("{2, _@Bar, _@Baz}");
                             _ -> Tree
                         end
                     end)),
     ?_assertEqual("{2, baz()}",
                   f(begin
                         Tree = ?Q("{foo, [bar], baz()}"),
                         case Tree of
                             ?Q("{foo, [_@Bar], '@Baz'}")
                               when erl_syntax:is_atom(Bar, foo) ->
                                 ?Q("{1, _@Baz}");
                             ?Q("{foo, [_@Bar], '@Baz'}")
                               when erl_syntax:is_atom(Bar, bar) ->
                                 ?Q("{2, _@Baz}");
                             ?Q("{foo, [_@Bar], '@Baz'}") ->
                                 ?Q("{3, _@Baz}");
                             _ -> Tree
                         end
                     end)),
     ?_assertEqual("{2, 42}",
                   f(begin
                         Tree = ?Q("{foo, [bar], 42}"),
                         case Tree of
                             ?Q("{foo, [_@Bar], '@Baz'}")
                               when erl_syntax:is_atom(Bar, bar),
                                    erl_syntax:is_integer(Baz, 17) ->
                                 ?Q("{1, _@Bar}");
                             ?Q("{foo, [_@Bar], '@Baz'}")
                               when erl_syntax:is_atom(Bar, bar),
                                    erl_syntax:is_integer(Baz, 42) ->
                                 ?Q("{2, _@Baz}");
                             ?Q("{foo, [_@Bar], '@Baz'}") ->
                                 ?Q("{3, _@Baz}");
                             _ -> Tree
                         end
                     end)),
     ?_assertEqual("{2, 42}",
                   f(begin
                         Tree = ?Q("{foo, [baz], 42}"),
                         case Tree of
                             ?Q("{foo, [_@Bar], '@Baz'}")
                               when erl_syntax:is_atom(Bar, bar),
                                    erl_syntax:is_integer(Baz, 17)
                                    ; erl_syntax:is_atom(Bar, baz),
                                    erl_syntax:is_integer(Baz, 17) ->
                                 ?Q("{1, _@Bar}");
                             ?Q("{foo, [_@Bar], '@Baz'}")
                               when erl_syntax:is_atom(Bar, bar),
                                    erl_syntax:is_integer(Baz, 42)
                                    ; erl_syntax:is_atom(Bar, baz),
                                    erl_syntax:is_integer(Baz, 42) ->
                                 ?Q("{2, _@Baz}");
                             ?Q("{foo, [_@Bar], '@Baz'}") ->
                                 ?Q("{3, _@Baz}");
                             _ -> Tree
                         end
                    end)),
     ?_assertEqual("{2, foo, Bar, Baz, Bar(), Baz()}",
                   f(begin
                         Tree = ?Q("foo(Bar, Baz) -> Bar(), Baz()."),
                         case Tree of
                             ?Q("'@Func'(_@Args) -> _@Body.") ->
                                 ?Q("{1, _@Func, _@Args, _@Body}");
                             ?Q("'@Func'(_@@Args) -> _@@Body.") ->
                                 ?Q("{2, _@Func, _@Args, _@Body}");
                             ?Q("'@Func'(_@Args, Baz) -> _@Body1, _@Body2.") ->
                                 ?Q("{3, _@Func, _@Args, _@Body1, _@Body2}")
                         end
                     end))
    ].

-endif.
