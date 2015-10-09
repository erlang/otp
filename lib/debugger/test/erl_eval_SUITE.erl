%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2013. All Rights Reserved.
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

-module(erl_eval_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([guard_1/1, guard_2/1,
	 match_pattern/1,
	 match_bin/1,
	 string_plusplus/1,
	 pattern_expr/1,
         guard_3/1, guard_4/1,
         lc/1,
         simple_cases/1,
         unary_plus/1,
         apply_atom/1,
         otp_5269/1,
         otp_6539/1,
         otp_6543/1,
         otp_6787/1,
         otp_6977/1,
	 otp_7550/1,
         otp_8133/1,
         funs/1,
	 try_catch/1,
	 eval_expr_5/1,
         eep37/1]).

%%
%% Define to run outside of test server
%%
%%-define(STANDALONE,1).

-import(lists,[concat/1, sort/1]).

-export([count_down/2, count_down_fun/0, do_apply/2,
         local_func/3, local_func_value/2]).

-ifdef(STANDALONE).
-define(config(A,B),config(A,B)).
-export([config/2]).
-define(line, noop, ).
config(priv_dir,_) ->
    ".".
-else.
-include_lib("test_server/include/test_server.hrl").
-export([init_per_testcase/2, end_per_testcase/2]).
% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).
init_per_testcase(_Case, Config) ->
    test_lib:interpret(?MODULE),
    ?line Dog = ?t:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].
end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.
-endif.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [guard_1, guard_2, match_pattern, string_plusplus,
     pattern_expr, match_bin, guard_3, guard_4, lc,
     simple_cases, unary_plus, apply_atom, otp_5269,
     otp_6539, otp_6543, otp_6787, otp_6977, otp_7550,
     otp_8133, funs, try_catch, eval_expr_5, eep37].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


guard_1(doc) ->
    ["(OTP-2405)"];
guard_1(suite) ->
    [];
guard_1(Config) when is_list(Config) ->
    ?line {ok,Tokens ,_} =
	erl_scan:string("if a+4 == 4 -> yes; true -> no end. "),
    ?line {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
    ?line no = guard_1_compiled(),
    ?line {value, no, []} = erl_eval:expr(Expr, []),
    ok.

guard_1_compiled() ->
    if a+4 == 4 -> yes; true -> no end.

guard_2(doc) ->
    ["Similar to guard_1, but type-correct"];
guard_2(suite) ->
    [];
guard_2(Config) when is_list(Config) ->
    ?line {ok,Tokens ,_} =
	erl_scan:string("if 6+4 == 4 -> yes; true -> no end. "),
    ?line {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
    ?line no = guard_2_compiled(),
    ?line {value, no, []} = erl_eval:expr(Expr, []),
    ok.

guard_2_compiled() ->
    if 6+4 == 4 -> yes; true -> no end.

string_plusplus(doc) ->
    ["OTP-3069: syntactic sugar string ++ ..."];
string_plusplus(suite) ->
    [];
string_plusplus(Config) when is_list(Config) ->
    ?line check(fun() -> case "abc" of "ab" ++ L -> L end end,
		"case \"abc\" of \"ab\" ++ L -> L end. ",
		"c"),
    ?line check(fun() -> case "abcde" of "ab" ++ "cd" ++ L -> L end end,
		"case \"abcde\" of \"ab\" ++ \"cd\" ++ L -> L end. ",
		"e"),
    ?line check(fun() -> case "abc" of [97, 98] ++ L -> L end end,
		"case \"abc\" of [97, 98] ++ L -> L end. ",
		"c"),
    ok.

match_pattern(doc) ->
    ["OTP-2983: match operator in pattern"];
match_pattern(suite) ->
    [];
match_pattern(Config) when is_list(Config) ->
    ?line check(fun() -> case {a, b} of {a, _X}=Y -> {x,Y} end end,
		"case {a, b} of {a, X}=Y -> {x,Y} end. ",
		{x, {a, b}}),
    ?line check(fun() -> case {a, b} of Y={a, _X} -> {x,Y} end end,
		"case {a, b} of Y={a, X} -> {x,Y} end. ",
		{x, {a, b}}),
    ?line check(fun() -> case {a, b} of Y={a, _X}=Z -> {Z,Y} end end,
		"case {a, b} of Y={a, X}=Z -> {Z,Y} end. ",
		{{a, b}, {a, b}}),
    ?line check(fun() -> A = 4, B = 28, <<13:(A+(X=B))>>, X end,
                "begin A = 4, B = 28, <<13:(A+(X=B))>>, X end.",
                28),
    ok.

match_bin(doc) ->
    ["binary match problems"];
match_bin(suite) ->
    [];
match_bin(Config) when is_list(Config) ->
    ?line check(fun() -> <<"abc">> = <<"abc">> end,
		"<<\"abc\">> = <<\"abc\">>. ",
		<<"abc">>),
    ?line check(fun() ->
			<<Size,B:Size/binary,Rest/binary>> = <<2,"AB","CD">>,
			{Size,B,Rest}
		end,
		"begin <<Size,B:Size/binary,Rest/binary>> = <<2,\"AB\",\"CD\">>, "
		"{Size,B,Rest} end. ",
		{2,<<"AB">>,<<"CD">>}),
    ok.

pattern_expr(doc) ->
    ["OTP-3144: compile-time expressions in pattern"];
pattern_expr(suite) ->
    [];
pattern_expr(Config) when is_list(Config) ->
    ?line check(fun() -> case 4 of 2+2 -> ok end end,
		"case 4 of 2+2 -> ok end. ",
		ok),
    ?line check(fun() -> case 2 of +2 -> ok end end,
		"case 2 of +2 -> ok end. ",
		ok),
    ok.

guard_3(doc) ->
    ["OTP-4518."];
guard_3(suite) ->
    [];
guard_3(Config) when is_list(Config) ->
    ?line check(fun() -> if false -> false; true -> true end end,
                "if false -> false; true -> true end.",
                true),
    ?line check(fun() -> if <<"hej">> == <<"hopp">> -> true;
                            true -> false end end,
                "begin if <<\"hej\">> == <<\"hopp\">> -> true;
                          true -> false end end.",
                false),
    ?line check(fun() -> if <<"hej">> == <<"hej">> -> true;
                            true -> false end end,
                "begin if <<\"hej\">> == <<\"hej\">> -> true;
                          true -> false end end.",
                true),
    ok.

guard_4(doc) ->
    ["OTP-4885."];
guard_4(suite) ->
    [];
guard_4(Config) when is_list(Config) ->
    check(fun() -> if erlang:'+'(3,a) -> true ; true -> false end end,
	  "if erlang:'+'(3,a) -> true ; true -> false end.",
	  false),
    check(fun() -> if erlang:is_integer(3) -> true ; true -> false end
	  end,
	  "if erlang:is_integer(3) -> true ; true -> false end.",
	  true),
    ?line check(fun() -> [X || X <- [1,2,3], erlang:is_integer(X)] end,
                "[X || X <- [1,2,3], erlang:is_integer(X)].",
                [1,2,3]),
    ?line check(fun() -> if is_atom(is_integer(a)) -> true ; true -> false end
                end,
                "if is_atom(is_integer(a)) -> true ; true -> false end.",
                true),
    check(fun() -> if erlang:is_atom(erlang:is_integer(a)) -> true;
		      true -> false end end,
	  "if erlang:is_atom(erlang:is_integer(a)) -> true; "
	  "true -> false end.",
	  true),
    ?line check(fun() -> if is_atom(3+a) -> true ; true -> false end end,
                "if is_atom(3+a) -> true ; true -> false end.",
                false),
    ?line check(fun() -> if erlang:is_atom(3+a) -> true ; true -> false end
                end,
                "if erlang:is_atom(3+a) -> true ; true -> false end.",
                false),
    ok.


lc(doc) ->
    ["OTP-4518."];
lc(suite) ->
    [];
lc(Config) when is_list(Config) ->
    ?line check(fun() -> X = 32, [X || X <- [1,2,3]] end,
                "begin X = 32, [X || X <- [1,2,3]] end.",
                [1,2,3]),
    ?line check(fun() -> X = 32,
                         [X || <<X:X>> <- [<<1:32>>,<<2:32>>,<<3:8>>]] end,
    %% "binsize variable"          ^
                "begin X = 32,
                 [X || <<X:X>> <- [<<1:32>>,<<2:32>>,<<3:8>>]] end.",
                [1,2]),
    ?line check(fun() -> Y = 13,[X || {X,Y} <- [{1,2}]] end,
                "begin Y = 13,[X || {X,Y} <- [{1,2}]] end.",
                [1]),
    ?line error_check("begin [A || X <- [{1,2}], 1 == A] end.",
                      {unbound_var,'A'}),
    ?line error_check("begin X = 32,
                        [{Y,W} || X <- [1,2,32,Y=4], Z <- [1,2,W=3]] end.",
                      {unbound_var,'Y'}),
    ?line error_check("begin X = 32,<<A:B>> = <<100:X>> end.",
                      {unbound_var,'B'}),
    ?line check(fun() -> [X || X <- [1,2,3,4], not (X < 2)] end,
                "begin [X || X <- [1,2,3,4], not (X < 2)] end.",
                [2,3,4]),
    ?line check(fun() -> [X || X <- [true,false], X] end,
                "[X || X <- [true,false], X].", [true]),
    ok.

simple_cases(doc) ->
    ["Simple cases, just to cover some code."];
simple_cases(suite) ->
    [];
simple_cases(Config) when is_list(Config) ->
    ?line check(fun() -> A = $C end, "A = $C.", $C),
    %% ?line check(fun() -> A = 3.14 end, "A = 3.14.", 3.14),
    ?line check(fun() -> self() ! a, A = receive a -> true end end,
                "begin self() ! a, A = receive a -> true end end.",
                true),
    ?line check(fun() -> c:flush(), self() ! a, self() ! b, self() ! c,
                         receive b -> b end,
                         {messages, [a,c]} =
                             erlang:process_info(self(), messages),
                         c:flush() end,
                "begin c:flush(), self() ! a, self() ! b, self() ! c,"
                "receive b -> b end,"
                "{messages, [a,c]} ="
                "     erlang:process_info(self(), messages), c:flush() end.",
                ok),
    ?line check(fun() -> self() ! a, A = receive a -> true
                                         after 0 -> false end end,
                "begin self() ! a, A = receive a -> true"
                "                      after 0 -> false end end.",
                true),
    ?line check(fun() -> c:flush(), self() ! a, self() ! b, self() ! c,
                         receive b -> b after 0 -> true end,
                         {messages, [a,c]} =
                             erlang:process_info(self(), messages),
                         c:flush() end,
                "begin c:flush(), self() ! a, self() ! b, self() ! c,"
                "receive b -> b after 0 -> true end,"
                "{messages, [a,c]} ="
                "     erlang:process_info(self(), messages), c:flush() end.",
                ok),
    ?line check(fun() -> receive _ -> true after 10 -> false end end,
                "receive _ -> true after 10 -> false end.",
                false),
    ?line check(fun() -> F = fun(A) -> A end, true = 3 == F(3) end,
                "begin F = fun(A) -> A end, true = 3 == F(3) end.",
                true),
    ?line check(fun() -> F = fun(A) -> A end, true = 3 == apply(F, [3]) end,
                "begin F = fun(A) -> A end, true = 3 == apply(F,[3]) end.",
                true),
    ?line check(fun() -> catch throw(a) end, "catch throw(a).", a),
    ?line check(fun() -> catch a end, "catch a.", a),
    ?line check(fun() -> 4 == 3 end, "4 == 3.", false),
    ?line check(fun() -> not true end, "not true.", false),
    ?line check(fun() -> -3 end, "-3.", -3),

    ?line error_check("3.0 = 4.0.", {badmatch,4.0}),
    ?line check(fun() -> <<(3.0+2.0):32/float>> = <<5.0:32/float>> end,
                "<<(3.0+2.0):32/float>> = <<5.0:32/float>>.",
                <<5.0:32/float>>),

    ?line check(fun() -> false andalso kludd end, "false andalso kludd.",
                false),
    ?line check(fun() -> true andalso true end, "true andalso true.",
                true),
    ?line check(fun() -> true andalso false end, "true andalso false.",
                false),
    ?line check(fun() -> true andalso kludd end, "true andalso kludd.",
                kludd),
    ?line error_check("kladd andalso kludd.", {badarg,kladd}),

    ?line check(fun() -> if false andalso kludd -> a; true -> b end end,
                "if false andalso kludd -> a; true -> b end.",
                b),
    ?line check(fun() -> if true andalso true -> a; true -> b end end,
                "if true andalso true -> a; true -> b end.",
                a),
    ?line check(fun() -> if true andalso false -> a; true -> b end end,
                "if true andalso false -> a; true -> b end.",
                b),

    ?line check(fun() -> true orelse kludd end,
                "true orelse kludd.", true),
    ?line check(fun() -> false orelse false end,
                "false orelse false.", false),
    ?line check(fun() -> false orelse true end,
                "false orelse true.", true),
    ?line check(fun() -> false orelse kludd end,
                "false orelse kludd.", kludd),
    ?line error_check("kladd orelse kludd.", {badarg,kladd}),
    ?line error_check("[X || X <- [1,2,3], begin 1 end].",{bad_filter,1}),
    ?line error_check("[X || X <- a].",{bad_generator,a}),

    ?line check(fun() -> if true orelse kludd -> a; true -> b end end,
                "if true orelse kludd -> a; true -> b end.", a),
    ?line check(fun() -> if false orelse false -> a; true -> b end end,
                "if false orelse false -> a; true -> b end.", b),
    ?line check(fun() -> if false orelse true -> a; true -> b end end,
                "if false orelse true -> a; true -> b end.", a),

    ?line check(fun() -> [X || X <- [1,2,3], X+2] end,
                "[X || X <- [1,2,3], X+2].", []),

    ?line check(fun() -> [X || X <- [1,2,3], [X] == [X || X <- [2]]] end,
                "[X || X <- [1,2,3], [X] == [X || X <- [2]]].",
                [2]),
    ?line check(fun() -> F = fun(1) -> ett; (2) -> zwei end,
                         ett = F(1), zwei = F(2) end,
                "begin F = fun(1) -> ett; (2) -> zwei end,
                         ett = F(1), zwei = F(2) end.",
                zwei),
    ?line check(fun() -> F = fun(X) when X == 1 -> ett;
                                (X) when X == 2 -> zwei end,
                         ett = F(1), zwei = F(2) end,
                "begin F = fun(X) when X == 1 -> ett;
                              (X) when X == 2 -> zwei end,
                         ett = F(1), zwei = F(2) end.",
                zwei),
    ?line error_check("begin F = fun(1) -> ett end, zwei = F(2) end.",
                      function_clause),
    ?line check(fun() -> if length([1]) == 1 -> yes;
                            true -> no end end,
                "if length([1]) == 1 -> yes;
                            true -> no end.",
                yes),
    ?line check(fun() -> if is_integer(3) -> true; true -> false end end,
                "if is_integer(3) -> true; true -> false end.", true),
    ?line check(fun() -> if integer(3) -> true; true -> false end end,
                "if integer(3) -> true; true -> false end.", true),
    ?line check(fun() -> if is_float(3) -> true; true -> false end end,
                "if is_float(3) -> true; true -> false end.", false),
    ?line check(fun() -> if float(3) -> true; true -> false end end,
                "if float(3) -> true; true -> false end.", false),
    ?line check(fun() -> if is_number(3) -> true; true -> false end end,
                "if is_number(3) -> true; true -> false end.", true),
    ?line check(fun() -> if number(3) -> true; true -> false end end,
                "if number(3) -> true; true -> false end.", true),
    ?line check(fun() -> if is_atom(a) -> true; true -> false end end,
                "if is_atom(a) -> true; true -> false end.", true),
    ?line check(fun() -> if atom(a) -> true; true -> false end end,
                "if atom(a) -> true; true -> false end.", true),
    ?line check(fun() -> if is_list([]) -> true; true -> false end end,
                "if is_list([]) -> true; true -> false end.", true),
    ?line check(fun() -> if list([]) -> true; true -> false end end,
                "if list([]) -> true; true -> false end.", true),
    ?line check(fun() -> if is_tuple({}) -> true; true -> false end end,
                "if is_tuple({}) -> true; true -> false end.", true),
    ?line check(fun() -> if tuple({}) -> true; true -> false end end,
                "if tuple({}) -> true; true -> false end.", true),
    ?line check(fun() -> if is_pid(self()) -> true; true -> false end end,
                "if is_pid(self()) -> true; true -> false end.", true),
    ?line check(fun() -> if pid(self()) -> true; true -> false end end,
                "if pid(self()) -> true; true -> false end.", true),
    ?line check(fun() -> R = make_ref(), if is_reference(R) -> true;
                                            true -> false end end,
                "begin R = make_ref(), if is_reference(R) -> true;"
                "true -> false end end.", true),
    ?line check(fun() -> R = make_ref(), if reference(R) -> true;
                                            true -> false end end,
                "begin R = make_ref(), if reference(R) -> true;"
                "true -> false end end.", true),
    ?line check(fun() -> if is_port(a) -> true; true -> false end end,
                "if is_port(a) -> true; true -> false end.", false),
    ?line check(fun() -> if port(a) -> true; true -> false end end,
                "if port(a) -> true; true -> false end.", false),
    ?line check(fun() -> if is_function(a) -> true; true -> false end end,
                "if is_function(a) -> true; true -> false end.", false),
    ?line check(fun() -> if function(a) -> true; true -> false end end,
                "if function(a) -> true; true -> false end.", false),
    ?line check(fun() -> if is_binary(<<>>) -> true; true -> false end end,
                "if is_binary(<<>>) -> true; true -> false end.", true),
    ?line check(fun() -> if binary(<<>>) -> true; true -> false end end,
                "if binary(<<>>) -> true; true -> false end.", true),
    ?line check(fun() -> if is_integer(a) == true -> yes;
                            true -> no end end,
                "if is_integer(a) == true -> yes;
                            true -> no end.",
                no),
    ?line check(fun() -> if [] -> true; true -> false end end,
                "if [] -> true; true -> false end.", false),
    ?line error_check("if lists:member(1,[1]) -> true; true -> false end.",
                      illegal_guard_expr),
    ?line error_check("if false -> true end.", if_clause),
    ?line check(fun() -> if a+b -> true; true -> false end end,
                "if a + b -> true; true -> false end.", false),
    ?line check(fun() -> if + b -> true; true -> false end end,
                "if + b -> true; true -> false end.", false),
    ?line error_check("case foo of bar -> true end.", {case_clause,foo}),
    ?line error_check("case 4 of 2+a -> true; _ -> false end.",
                      illegal_pattern),
    ?line error_check("case 4 of +a -> true; _ -> false end.",
                      illegal_pattern),
    ?line check(fun() -> case a of
                             X when X == b -> one;
                             X when X == a -> two
                         end end,
                "begin case a of
                             X when X == b -> one;
                             X when X == a -> two
                         end end.", two),
    ?line error_check("3 = 4.", {badmatch,4}),
    ?line error_check("a = 3.", {badmatch,3}),
    %% ?line error_check("3.1 = 2.7.",{badmatch,2.7}),
    ?line error_check("$c = 4.", {badmatch,4}),
    ?line check(fun() -> $c = $c end, "$c = $c.", $c),
    ?line check(fun() -> _ = bar end, "_ = bar.", bar),
    ?line check(fun() -> A = 14, A = 14 end,
                "begin A = 14, A = 14 end.", 14),
    ?line error_check("begin A = 14, A = 16 end.", {badmatch,16}),
    ?line error_check("\"hej\" = \"san\".", {badmatch,"san"}),
    ?line check(fun() -> "hej" = "hej" end,
                "\"hej\" = \"hej\".", "hej"),
    ?line error_check("[] = [a].", {badmatch,[a]}),
    ?line check(fun() -> [] = [] end, "[] = [].", []),
    ?line error_check("[a] = [].", {badmatch,[]}),
    ?line error_check("{a,b} = 34.", {badmatch,34}),
    ?line check(fun() -> <<X:7>> = <<8:7>>, X end,
		"begin <<X:7>> = <<8:7>>, X end.", 8),
    ?line error_check("<<34:32>> = \"hej\".", {badmatch,"hej"}),
    ?line check(fun() -> trunc((1 * 3 div 3 + 4 - 3) / 1) rem 2 end,
                "begin trunc((1 * 3 div 3 + 4 - 3) / 1) rem 2 end.", 0),
    ?line check(fun() -> (2#101 band 2#10101) bor (2#110 bxor 2#010) end,
                "(2#101 band 2#10101) bor (2#110 bxor 2#010).", 5),
    ?line check(fun() -> (2#1 bsl 4) + (2#10000 bsr 3) end,
                "(2#1 bsl 4) + (2#10000 bsr 3).", 18),
    ?line check(fun() -> ((1<3) and ((1 =:= 2) or (1 =/= 2))) xor (1=<2) end,
                "((1<3) and ((1 =:= 2) or (1 =/= 2))) xor (1=<2).", false),
    ?line check(fun() -> (a /= b) or (2 > 4) or (3 >= 3) end,
                "(a /= b) or (2 > 4) or (3 >= 3).", true),
    ?line check(fun() -> "hej" ++ "san" =/= "hejsan" -- "san" end,
                "\"hej\" ++ \"san\" =/= \"hejsan\" -- \"san\".", true),
    ?line check(fun() -> (bnot 1) < -0 end, "(bnot (+1)) < -0.", true),
    ok.

unary_plus(doc) ->
    ["OTP-4929. Unary plus rejects non-numbers."];
unary_plus(suite) ->
    [];
unary_plus(Config) when is_list(Config) ->
    ?line check(fun() -> F = fun(X) -> + X end,
                         true = -1 == F(-1) end,
                "begin F = fun(X) -> + X end,"
                "      true = -1 == F(-1) end.", true, ['F'], none, none),
    ?line error_check("+a.", badarith),
    ok.

apply_atom(doc) ->
    ["OTP-5064. Can no longer apply atoms."];
apply_atom(suite) ->
    [];
apply_atom(Config) when is_list(Config) ->
    ?line error_check("[X || X <- [[1],[2]],
                             begin L = length, L(X) =:= 1 end].",
                      {badfun,length}),
    ok.

otp_5269(doc) ->
    ["OTP-5269. Bugs in the bit syntax."];
otp_5269(suite) ->
    [];
otp_5269(Config) when is_list(Config) ->
    ?line check(fun() -> L = 8,
                         F = fun(<<A:L,B:A>>) -> B end,
                         F(<<16:8, 7:16>>)
                end,
                "begin
                   L = 8, F = fun(<<A:L,B:A>>) -> B end, F(<<16:8, 7:16>>)
                 end.",
                7),
    ?line check(fun() -> L = 8,
                         F = fun(<<L:L,B:L>>) -> B end,
                         F(<<16:8, 7:16>>)
                end,
                "begin
                   L = 8, F = fun(<<L:L,B:L>>) -> B end, F(<<16:8, 7:16>>)
                 end.",
                7),
    ?line check(fun() -> L = 8, <<A:L,B:A>> = <<16:8, 7:16>>, B end,
                "begin L = 8, <<A:L,B:A>> = <<16:8, 7:16>>, B end.",
                7),
    ?line error_check("begin L = 8, <<L:L,B:L>> = <<16:8, 7:16>> end.",
                      {badmatch,<<16:8,7:16>>}),

    ?line error_check("begin <<L:16,L:L>> = <<16:16,8:16>>, L end.",
                      {badmatch, <<16:16,8:16>>}),
    ?line check(fun() -> U = 8, (fun(<<U:U>>) -> U end)(<<32:8>>) end,
                "begin U = 8, (fun(<<U:U>>) -> U end)(<<32:8>>) end.",
                32),
    ?line check(fun() -> U = 8, [U || <<U:U>> <- [<<32:8>>]] end,
                "begin U = 8, [U || <<U:U>> <- [<<32:8>>]] end.",
                [32]),
    ?line error_check("(fun({3,<<A:32,A:32>>}) -> a end)
                          ({3,<<17:32,19:32>>}).",
                      function_clause),
    ?line check(fun() -> [X || <<A:8,
                                 B:A>> <- [<<16:8,19:16>>],
                               <<X:8>> <- [<<B:8>>]] end,
                "[X || <<A:8,
                                 B:A>> <- [<<16:8,19:16>>],
                               <<X:8>> <- [<<B:8>>]].",
                [19]),
    ok.

otp_6539(doc) ->
    ["OTP-6539. try/catch bugs."];
otp_6539(suite) ->
    [];
otp_6539(Config) when is_list(Config) ->
    ?line check(fun() ->
                        F = fun(A,B) ->
                                    try A+B
                                    catch _:_ -> dontthinkso
                                    end
                            end,
                        lists:zipwith(F, [1,2], [2,3])
                end,
                "begin
                     F = fun(A,B) ->
                                 try A+B
                                 catch _:_ -> dontthinkso
                                 end
                         end,
                     lists:zipwith(F, [1,2], [2,3])
                 end.",
                [3, 5]),
    ok.

otp_6543(doc) ->
    ["OTP-6543. bitlevel binaries."];
otp_6543(suite) ->
    [];
otp_6543(Config) when is_list(Config) ->
    ?line check(fun() ->
                        << <<X>> || <<X>> <- [1,2,3] >>
                end,
                "<< <<X>> || <<X>> <- [1,2,3] >>.",
                <<>>),
    ?line check(fun() ->
                        << <<X>> || X <- [1,2,3] >>
                end,
                "<< <<X>> || X <- [1,2,3] >>.",
                <<1,2,3>>),
    ?line check(fun() ->
                        << <<X:8>> || <<X:2>> <= <<"hej">> >>
                end,
                "<< <<X:8>> || <<X:2>> <= <<\"hej\">> >>.",
                <<1,2,2,0,1,2,1,1,1,2,2,2>>),
    ?line check(fun() ->
                        << <<X:8>> ||
                            <<65,X:4>> <= <<65,7:4,65,3:4,66,8:4>> >>
                end,
                "<< <<X:8>> ||
                            <<65,X:4>> <= <<65,7:4,65,3:4,66,8:4>> >>.",
                <<7,3>>),
    ?line check(fun() -> <<34:18/big>> end,
                "<<34:18/big>>.",
                <<0,8,2:2>>),
    ?line check(fun() -> <<34:18/big-unit:2>> end,
                "<<34:18/big-unit:2>>.",
                <<0,0,0,2,2:4>>),
    ?line check(fun() -> <<34:18/little>> end,
                "<<34:18/little>>.",
                <<34,0,0:2>>),
    ?line case eval_string("<<34:18/native>>.") of
              <<0,8,2:2>> -> ok;
              <<34,0,0:2>> -> ok
          end,
    ?line check(fun() -> <<34:18/big-signed>> end,
                "<<34:18/big-signed>>.",
                <<0,8,2:2>>),
    ?line check(fun() -> <<34:18/little-signed>> end,
                "<<34:18/little-signed>>.",
                <<34,0,0:2>>),
    ?line case eval_string("<<34:18/native-signed>>.") of
              <<0,8,2:2>> -> ok;
              <<34,0,0:2>> -> ok
          end,
    ?line check(fun() -> <<34:18/big-unsigned>> end,
                "<<34:18/big-unsigned>>.",
                <<0,8,2:2>>),
    ?line check(fun() -> <<34:18/little-unsigned>> end,
                "<<34:18/little-unsigned>>.",
                <<34,0,0:2>>),
    ?line case eval_string("<<34:18/native-unsigned>>.") of
              <<0,8,2:2>> -> ok;
              <<34,0,0:2>> -> ok
          end,
    ?line check(fun() -> <<3.14:32/float-big>> end,
                "<<3.14:32/float-big>>.",
                <<64,72,245,195>>),
    ?line check(fun() -> <<3.14:32/float-little>> end,
                "<<3.14:32/float-little>>.",
                <<195,245,72,64>>),
    ?line case eval_string("<<3.14:32/float-native>>.") of
              <<64,72,245,195>> -> ok;
              <<195,245,72,64>> -> ok
          end,
    ?line error_check("<<(<<17,3:2>>)/binary>>.", badarg),
    ?line check(fun() -> <<(<<17,3:2>>)/bitstring>> end,
                "<<(<<17,3:2>>)/bitstring>>.",
                <<17,3:2>>),
    ?line check(fun() -> <<(<<17,3:2>>):10/bitstring>> end,
                "<<(<<17,3:2>>):10/bitstring>>.",
                <<17,3:2>>),
    ?line check(fun() -> <<<<344:17>>/binary-unit:17>> end,
		"<<<<344:17>>/binary-unit:17>>.",
		<<344:17>>),

    ?line check(fun() -> <<X:18/big>> = <<34:18/big>>, X end,
                "begin <<X:18/big>> = <<34:18/big>>, X end.",
                34),
    ?line check(fun() -> <<X:18/big-unit:2>> = <<34:18/big-unit:2>>, X end,
                "begin <<X:18/big-unit:2>> = <<34:18/big-unit:2>>, X end.",
                34),
    ?line check(fun() -> <<X:18/little>> = <<34:18/little>>, X end,
                "begin <<X:18/little>> = <<34:18/little>>, X end.",
                34),
    ?line check(fun() -> <<X:18/native>> = <<34:18/native>>, X end,
                "begin <<X:18/native>> = <<34:18/native>>, X end.",
                34),
    ?line check(fun() -> <<X:18/big-signed>> = <<34:18/big-signed>>, X end,
                "begin <<X:18/big-signed>> = <<34:18/big-signed>>, X end.",
                34),
    ?line check(fun() -> <<X:18/little-signed>> = <<34:18/little-signed>>,
                         X end,
                "begin <<X:18/little-signed>> = <<34:18/little-signed>>,
                       X end.",
                34),
    ?line check(fun() -> <<X:18/native-signed>> = <<34:18/native-signed>>,
                         X end,
                "begin <<X:18/native-signed>> = <<34:18/native-signed>>,
                       X end.",
                34),
    ?line check(fun() -> <<X:18/big-unsigned>> = <<34:18/big-unsigned>>,
                         X end,
                "begin <<X:18/big-unsigned>> = <<34:18/big-unsigned>>,
                       X end.",
                34),
    ?line check(fun() ->
                        <<X:18/little-unsigned>> = <<34:18/little-unsigned>>,
                        X end,
                "begin <<X:18/little-unsigned>> = <<34:18/little-unsigned>>,
                       X end.",
                34),
    ?line check(fun() ->
                        <<X:18/native-unsigned>> = <<34:18/native-unsigned>>,
                        X end,
                "begin <<X:18/native-unsigned>> = <<34:18/native-unsigned>>,
                       X end.",
                34),
    ?line check(fun() -> <<X:32/float-big>> = <<2.0:32/float-big>>, X end,
                "begin <<X:32/float-big>> = <<2.0:32/float-big>>,
                        X end.",
                2.0),
    ?line check(fun() -> <<X:32/float-little>> = <<2.0:32/float-little>>,
                         X end,
                "begin <<X:32/float-little>> = <<2.0:32/float-little>>,
                        X end.",
                2.0),
    ?line check(fun() -> <<X:32/float-native>> = <<2.0:32/float-native>>,
                         X end,
                "begin <<X:32/float-native>> = <<2.0:32/float-native>>,
                        X end.",
                2.0),

    ?line check(
            fun() ->
                    [X || <<"hej",X:8>> <= <<"hej",8,"san",9,"hej",17,"hej">>]
            end,
            "[X || <<\"hej\",X:8>> <=
                        <<\"hej\",8,\"san\",9,\"hej\",17,\"hej\">>].",
            [8,17]),
    ?line check(
            fun() ->
                    L = 8, << <<B:32>> || <<L:L,B:L>> <= <<16:8, 7:16>> >>
            end,
            "begin L = 8, << <<B:32>> || <<L:L,B:L>> <= <<16:8, 7:16>> >>
             end.",
            <<0,0,0,7>>),
    %% Test the Value part of a binary segment.
    %% "Old" bugs have been fixed (partial_eval is called on Value).
    ?line check(fun() -> [ 3 || <<17/float>> <= <<17.0/float>>] end,
                "[ 3 || <<17/float>> <= <<17.0/float>>].",
                [3]),
    ?line check(fun() -> [ 3 || <<17/float>> <- [<<17.0/float>>]] end,
                "[ 3 || <<17/float>> <- [<<17.0/float>>]].",
                [3]),
    ?line check(fun() -> [ X || <<17/float,X:3>> <= <<17.0/float,2:3>>] end,
                "[ X || <<17/float,X:3>> <= <<17.0/float,2:3>>].",
                [2]),
    ?line check(fun() ->
                 [ foo || <<(1 bsl 1023)/float>> <= <<(1 bsl 1023)/float>>]
                end,
                "[ foo || <<(1 bsl 1023)/float>> <= <<(1 bsl 1023)/float>>].",
                [foo]),
    ?line check(fun() ->
                 [ foo || <<(1 bsl 1023)/float>> <- [<<(1 bsl 1023)/float>>]]
                end,
               "[ foo || <<(1 bsl 1023)/float>> <- [<<(1 bsl 1023)/float>>]].",
                [foo]),
    ?line error_check("[ foo || <<(1 bsl 1024)/float>> <-
                            [<<(1 bsl 1024)/float>>]].",
                      badarg),
    ?line check(fun() ->
                 [ foo || <<(1 bsl 1024)/float>> <- [<<(1 bsl 1023)/float>>]]
                end,
                "[ foo || <<(1 bsl 1024)/float>> <-
                            [<<(1 bsl 1023)/float>>]].",
                []),
    ?line check(fun() ->
                 [ foo || <<(1 bsl 1024)/float>> <= <<(1 bsl 1023)/float>>]
                end,
                "[ foo || <<(1 bsl 1024)/float>> <=
                            <<(1 bsl 1023)/float>>].",
                []),
    ?line check(fun() ->
                        L = 8,
                        [{L,B} || <<L:L,B:L/float>> <= <<32:8,7:32/float>>]
                end,
                "begin L = 8,
                       [{L,B} || <<L:L,B:L/float>> <= <<32:8,7:32/float>>]
                 end.",
                [{32,7.0}]),
    ?line check(fun() ->
                        L = 8,
                        [{L,B} || <<L:L,B:L/float>> <- [<<32:8,7:32/float>>]]
                end,
                "begin L = 8,
                       [{L,B} || <<L:L,B:L/float>> <- [<<32:8,7:32/float>>]]
                 end.",
                [{32,7.0}]),
    ?line check(fun() ->
                        [foo || <<"s">> <= <<"st">>]
                end,
                "[foo || <<\"s\">> <= <<\"st\">>].",
                [foo]),
    ?line check(fun() -> <<_:32>> = <<17:32>> end,
                "<<_:32>> = <<17:32>>.",
                <<17:32>>),
    ?line check(fun() -> [foo || <<_:32>> <= <<17:32,20:32>>] end,
                "[foo || <<_:32>> <= <<17:32,20:32>>].",
                [foo,foo]),

    ?line check(fun() -> << <<X:32>> || X <- [1,2,3], X > 1 >> end,
                "<< <<X:32>> || X <- [1,2,3], X > 1 >>.",
                <<0,0,0,2,0,0,0,3>>),
    ?line error_check("[X || <<X>> <= [a,b]].",{bad_generator,[a,b]}),
    ok.

otp_6787(doc) ->
    ["OTP-6787. bitlevel binaries."];
otp_6787(suite) ->
    [];
otp_6787(Config) when is_list(Config) ->
    ?line check(
            fun() -> <<16:(1024*1024)>> = <<16:(1024*1024)>> end,
            "<<16:(1024*1024)>> = <<16:(1024*1024)>>.",
            <<16:1048576>>),
    ok.

otp_6977(doc) ->
    ["OTP-6977. ++ bug."];
otp_6977(suite) ->
    [];
otp_6977(Config) when is_list(Config) ->
    ?line check(
            fun() -> (fun([$X] ++ _) -> ok end)("X") end,
            "(fun([$X] ++ _) -> ok end)(\"X\").",
            ok),
    ok.

otp_7550(doc) ->
    ["OTP-7550. Support for UTF-8, UTF-16, UTF-32."];
otp_7550(Config) when is_list(Config) ->

    %% UTF-8.
    ?line check(
	    fun() -> <<65>> = <<65/utf8>> end,
	    "<<65>> = <<65/utf8>>.",
	    <<65>>),
    ?line check(
	    fun() -> <<350/utf8>> = <<197,158>> end,
	    "<<350/utf8>> = <<197,158>>.",
	    <<197,158>>),
    ?line check(
	    fun() -> <<$b,$j,$\303,$\266,$r,$n>> = <<"bj\366rn"/utf8>> end,
	    "<<$b,$j,$\303,$\266,$r,$n>> = <<\"bj\366rn\"/utf8>>.",
	    <<$b,$j,$\303,$\266,$r,$n>>),

    %% UTF-16.
    ?line check(
	    fun() -> <<0,65>> = <<65/utf16>> end,
	    "<<0,65>> = <<65/utf16>>.",
	    <<0,65>>),
    ?line check(
	    fun() -> <<16#D8,16#08,16#DF,16#45>> = <<16#12345/utf16>> end,
	    "<<16#D8,16#08,16#DF,16#45>> = <<16#12345/utf16>>.",
	    <<16#D8,16#08,16#DF,16#45>>),
    ?line check(
	    fun() -> <<16#08,16#D8,16#45,16#DF>> = <<16#12345/little-utf16>> end,
	    "<<16#08,16#D8,16#45,16#DF>> = <<16#12345/little-utf16>>.",
	    <<16#08,16#D8,16#45,16#DF>>),

    ?line check(
	    fun() -> <<350/utf16>> = <<1,94>> end,
	    "<<350/utf16>> = <<1,94>>.",
	    <<1,94>>),
    ?line check(
	    fun() -> <<350/little-utf16>> = <<94,1>> end,
	    "<<350/little-utf16>> = <<94,1>>.",
	    <<94,1>>),
    ?line check(
	    fun() -> <<16#12345/utf16>> = <<16#D8,16#08,16#DF,16#45>> end,
	    "<<16#12345/utf16>> = <<16#D8,16#08,16#DF,16#45>>.",
	    <<16#D8,16#08,16#DF,16#45>>),
    ?line check(
	    fun() -> <<16#12345/little-utf16>> = <<16#08,16#D8,16#45,16#DF>> end,
	    "<<16#12345/little-utf16>> = <<16#08,16#D8,16#45,16#DF>>.",
	    <<16#08,16#D8,16#45,16#DF>>),

    %% UTF-32.
    ?line check(
	    fun() -> <<16#12345/utf32>> = <<16#0,16#01,16#23,16#45>> end,
	    "<<16#12345/utf32>> = <<16#0,16#01,16#23,16#45>>.",
	    <<16#0,16#01,16#23,16#45>>),
    ?line check(
	    fun() -> <<16#0,16#01,16#23,16#45>> = <<16#12345/utf32>> end,
	    "<<16#0,16#01,16#23,16#45>> = <<16#12345/utf32>>.",
	    <<16#0,16#01,16#23,16#45>>),
    ?line check(
	    fun() -> <<16#12345/little-utf32>> = <<16#45,16#23,16#01,16#00>> end,
	    "<<16#12345/little-utf32>> = <<16#45,16#23,16#01,16#00>>.",
	    <<16#45,16#23,16#01,16#00>>),
    ?line check(
	    fun() -> <<16#12345/little-utf32>> end,
	    "<<16#12345/little-utf32>>.",
	    <<16#45,16#23,16#01,16#00>>),

    %% Mixed.
    ?line check(
	    fun() -> <<16#41,16#12345/utf32,16#0391:16,16#2E:8>> end,
	    "<<16#41,16#12345/utf32,16#0391:16,16#2E:8>>.",
	    <<16#41,16#00,16#01,16#23,16#45,16#03,16#91,16#2E>>),
    ok.


otp_8133(doc) ->
    ["OTP-8133. Bit comprehension bug."];
otp_8133(suite) ->
    [];
otp_8133(Config) when is_list(Config) ->
    ?line check(
            fun() ->
                  E = fun(N) ->
                              if
                                  is_integer(N) -> <<N/integer>>;
                                  true -> throw(foo)
                              end
                      end,
                  try << << (E(V))/binary >> || V <- [1,2,3,a] >>
                  catch foo -> ok
                  end
            end,
            "begin
                 E = fun(N) ->
                            if is_integer(N) -> <<N/integer>>;
                               true -> throw(foo)
                            end
                     end,
                 try << << (E(V))/binary >> || V <- [1,2,3,a] >>
                 catch foo -> ok
                 end
             end.",
            ok),
    ?line check(
            fun() ->
                  E = fun(N) ->
                              if
                                  is_integer(N) -> <<N/integer>>;
                                  true -> erlang:error(foo)
                              end
                      end,
                  try << << (E(V))/binary >> || V <- [1,2,3,a] >>
                  catch error:foo -> ok
                  end
            end,
            "begin
                 E = fun(N) ->
                            if is_integer(N) -> <<N/integer>>;
                               true -> erlang:error(foo)
                            end
                     end,
                 try << << (E(V))/binary >> || V <- [1,2,3,a] >>
                 catch error:foo -> ok
                 end
             end.",
            ok),
    ok.

funs(doc) ->
    ["Simple cases, just to cover some code."];
funs(suite) ->
    [];
funs(Config) when is_list(Config) ->
    do_funs(none, none),
    do_funs(lfh(), none),
    do_funs(lfh(), efh()),

    ?line error_check("nix:foo().", {access_not_allowed,nix}, lfh(), efh()),
    ?line error_check("bar().", undef, none, none),

    ?line check(fun() -> F1 = fun(F,N) -> ?MODULE:count_down(F, N) end,
                         F1(F1, 1000) end,
                "begin F1 = fun(F,N) -> count_down(F, N) end,"
                "F1(F1,1000) end.",
		0, ['F1'], lfh(), none),

    ?line check(fun() -> F1 = fun(F,N) -> ?MODULE:count_down(F, N) end,
                         F1(F1, 1000) end,
                "begin F1 = fun(F,N) -> count_down(F, N) end,"
                "F1(F1,1000) end.",
		0, ['F1'], lfh_value(), none),

    ?line check(fun() -> F1 = fun(F,N) -> ?MODULE:count_down(F, N) end,
                         F1(F1, 1000) end,
                "begin F1 = fun(F,N) -> count_down(F, N) end,"
                "F1(F1,1000) end.",
		0, ['F1'], lfh_value_extra(), none),

    ?line check(fun() -> F1 = fun(F,N) -> ?MODULE:count_down(F, N) end,
                         F1(F1, 1000) end,
                "begin F1 = fun(F,N) -> count_down(F, N) end,"
                "F1(F1,1000) end.",
		0, ['F1'], {?MODULE,local_func_value}, none),
    %% This is not documented, and only for backward compatibility (good!).
    B0 = erl_eval:new_bindings(),
    ?line check(fun() -> is_function(?MODULE:count_down_fun()) end,
                "begin is_function(count_down_fun()) end.",
                true, [], {?MODULE,local_func,[B0]},none),

    EF = fun({timer,sleep}, As) when length(As) == 1 -> exit({got_it,sleep});
            ({M,F}, As) -> apply(M, F, As)
         end,
    EFH = {value, EF},
    ?line error_check("apply(timer, sleep, [1]).", got_it, none, EFH),
    ?line error_check("begin F = fun(T) -> timer:sleep(T) end,F(1) end.",
                      got_it, none, EFH),
    ?line error_check("fun c/1.", undef),
    ?line error_check("fun a:b/0().", undef),

    MaxArgs = 20,
    ?line [true] =
        lists:usort([run_many_args(SAs) || SAs <- many_args(MaxArgs)]),
    ?line {'EXIT',{{argument_limit,_},_}} =
        (catch run_many_args(many_args1(MaxArgs+1))),
    ok.

run_many_args({S, As}) ->
    apply(eval_string(S), As) =:= As.

many_args(N) ->
    [many_args1(I) || I <- lists:seq(1, N)].

many_args1(N) ->
    F = fun(L, P) ->
                tl(lists:flatten([","++P++integer_to_list(E) || E <- L]))
        end,
    L = lists:seq(1, N),
    T = F(L, "V"),
    S = lists:flatten(io_lib:format("fun(~s) -> [~s] end.", [T, T])),
    {S, L}.

do_funs(LFH, EFH) ->
    %% LFH is not really used by these examples...

    %% These tests do not prove that tail recursive functions really
    %% work (that the process does not grow); one should also run them
    %% manually with 1000 replaced by 1000000.

    M = atom_to_list(?MODULE),
    ?line check(fun() -> F1 = fun(F,N) -> ?MODULE:count_down(F, N) end,
                         F1(F1, 1000) end,
                concat(["begin F1 = fun(F,N) -> ", M,
                        ":count_down(F, N) end, F1(F1,1000) end."]),
		0, ['F1'], LFH, EFH),
    ?line check(fun() -> F1 = fun(F,N) -> apply(?MODULE,count_down,[F,N])
                              end, F1(F1, 1000) end,
                concat(["begin F1 = fun(F,N) -> apply(", M,
                        ",count_down,[F, N]) end, F1(F1,1000) end."]),
		0, ['F1'], LFH, EFH),
    ?line check(fun() -> F = fun(F,N) when N > 0 -> apply(F,[F,N-1]);
                                (_F,0) -> ok end,
                         F(F, 1000)
                end,
                "begin F = fun(F,N) when N > 0 -> apply(F,[F,N-1]);"
                             "(_F,0) -> ok end,"
                       "F(F, 1000) end.",
                ok, ['F'], LFH, EFH),
    ?line check(fun() -> F = fun(F,N) when N > 0 ->
                                     apply(erlang,apply,[F,[F,N-1]]);
                                (_F,0) -> ok end,
                         F(F, 1000)
                end,
                "begin F = fun(F,N) when N > 0 ->"
                                   "apply(erlang,apply,[F,[F,N-1]]);"
                             "(_F,0) -> ok end,"
                       "F(F, 1000) end.",
                ok, ['F'], LFH, EFH),
    ?line check(fun() -> F = count_down_fun(),
                         SF = fun(SF, F1, N) -> F(SF, F1, N) end,
                         SF(SF, F, 1000) end,
                concat(["begin F = ", M, ":count_down_fun(),"
                        "SF = fun(SF, F1, N) -> F(SF, F1, N) end,"
                        "SF(SF, F, 1000) end."]),
                ok, ['F','SF'], LFH, EFH),


    ?line check(fun() -> F = fun(X) -> A = 1+X, {X,A} end,
                         true = {2,3} == F(2) end,
                "begin F = fun(X) -> A = 1+X, {X,A} end,
                       true = {2,3} == F(2) end.", true, ['F'], LFH, EFH),
    ?line check(fun() -> F = fun(X) -> byte_size(X) end,
                         ?MODULE:do_apply(F,<<"hej">>) end,
                concat(["begin F = fun(X) -> size(X) end,",
                        M,":do_apply(F,<<\"hej\">>) end."]),
                3, ['F'], LFH, EFH),

    ?line check(fun() -> F1 = fun(X, Z) -> {X,Z} end,
                         Z = 5,
                         F2 = fun(X, Y) -> F1(Z,{X,Y}) end,
                         F3 = fun(X, Y) -> {a,F1(Z,{X,Y})} end,
                         {5,{x,y}} = F2(x,y),
                         {a,{5,{y,x}}} = F3(y,x),
                         {5,{5,y}} = F2(Z,y),
                         true = {5,{x,5}} == F2(x,Z) end,
                "begin F1 = fun(X, Z) -> {X,Z} end,
                       Z = 5,
                       F2 = fun(X, Y) -> F1(Z,{X,Y}) end,
                       F3 = fun(X, Y) -> {a,F1(Z,{X,Y})} end,
                       {5,{x,y}} = F2(x,y),
                       {a,{5,{y,x}}} = F3(y,x),
                       {5,{5,y}} = F2(Z,y),
                       true = {5,{x,5}} == F2(x,Z) end.",
                true, ['F1','Z','F2','F3'], LFH, EFH),
    ?line check(fun() -> F = fun(X) -> byte_size(X) end,
                         F2 = fun(Y) -> F(Y) end,
                         ?MODULE:do_apply(F2,<<"hej">>) end,
                concat(["begin F = fun(X) -> size(X) end,",
                        "F2 = fun(Y) -> F(Y) end,",
                        M,":do_apply(F2,<<\"hej\">>) end."]),
                3, ['F','F2'], LFH, EFH),
    ?line check(fun() -> Z = 5, F = fun(X) -> {Z,X} end,
                         F2 = fun(Z) -> F(Z) end, F2(3) end,
                "begin Z = 5, F = fun(X) -> {Z,X} end,
                       F2 = fun(Z) -> F(Z) end, F2(3) end.",
                {5,3},['F','F2','Z'], LFH, EFH),
    ?line check(fun() -> F = fun(Z) -> Z end,
                         F2 = fun(X) -> F(X), Z = {X,X}, Z end,
                         {1,1} = F2(1), Z = 7, Z end,
                "begin F = fun(Z) -> Z end,
                       F2 = fun(X) -> F(X), Z = {X,X}, Z end,
                       {1,1} = F2(1), Z = 7, Z end.", 7, ['F','F2','Z'],
                LFH, EFH),
    ?line check(fun() -> F = fun(F, N) -> [?MODULE:count_down(F,N) || X <-[1]]
                             end, F(F,2) end,
                concat(["begin F = fun(F, N) -> [", M,
                       ":count_down(F,N) || X <-[1]] end, F(F,2) end."]),
                [[[0]]], ['F'], LFH, EFH),
    ok.

count_down(F, N) when N > 0 ->
    F(F, N-1);
count_down(_F, N) ->
    N.

count_down_fun() ->
    fun(SF,F,N) when N > 0 -> SF(SF,F,N-1);
       (_SF,_F,_N) -> ok
    end.

do_apply(F, V) ->
    F(V).

lfh() ->
    {eval, fun(F, As, Bs) -> local_func(F, As, Bs) end}.

local_func(F, As0, Bs0) when is_atom(F) ->
    {As,Bs} = erl_eval:expr_list(As0, Bs0, {eval,lfh()}),
    case erlang:function_exported(?MODULE, F, length(As)) of
	true ->
	    {value,apply(?MODULE, F, As),Bs};
	false ->
	    {value,apply(shell_default, F, As),Bs}
    end.

lfh_value_extra() ->
    %% Not documented.
    {value, fun(F, As) -> local_func_value(F, As) end, []}.

lfh_value() ->
    {value, fun(F, As) -> local_func_value(F, As) end}.

local_func_value(F, As) when is_atom(F) ->
    case erlang:function_exported(?MODULE, F, length(As)) of
	true ->
	    apply(?MODULE, F, As);
	false ->
	    apply(shell_default, F, As)
    end.

efh() ->
    {value, fun(F, As) -> external_func(F, As) end}.

external_func({M,_}, _As) when M == nix ->
    exit({{access_not_allowed,M},[mfa]});
external_func(F, As) when is_function(F) ->
    apply(F, As);
external_func({M,F}, As) ->
    apply(M, F, As).



try_catch(doc) ->
    ["Test try-of-catch-after-end statement"];
try_catch(suite) ->
    [];
try_catch(Config) when is_list(Config) ->
    %% Match in of with catch
    ?line check(fun() -> try 1 of 1 -> 2 catch _:_ -> 3 end end,
		"try 1 of 1 -> 2 catch _:_ -> 3 end.", 2),
    ?line check(fun() -> try 1 of 1 -> 2; 3 -> 4 catch _:_ -> 5 end end,
		"try 1 of 1 -> 2; 3 -> 4 catch _:_ -> 5 end.", 2),
    ?line check(fun() -> try 3 of 1 -> 2; 3 -> 4 catch _:_ -> 5 end end,
		"try 3 of 1 -> 2; 3 -> 4 catch _:_ -> 5 end.", 4),
    %% Just after
    ?line check(fun () -> X = try 1 after put(try_catch, 2) end,
			  {X,get(try_catch)} end,
		"begin X = try 1 after put(try_catch, 2) end, "
		"{X,get(try_catch)} end.", {1,2}),
    %% Match in of with after
    ?line check(fun() -> X = try 1 of 1 -> 2 after put(try_catch, 3) end,
			 {X,get(try_catch)} end,
		"begin X = try 1 of 1 -> 2 after put(try_catch, 3) end, "
		"{X,get(try_catch)} end.", {2,3}),
    ?line check(fun() -> X = try 1 of 1 -> 2; 3 -> 4
			     after put(try_catch, 5) end,
			 {X,get(try_catch)} end,
		"begin X = try 1 of 1 -> 2; 3 -> 4 "
		"          after put(try_catch, 5) end, "
		"      {X,get(try_catch)} end.", {2,5}),
    ?line check(fun() -> X = try 3 of 1 -> 2; 3 -> 4
			     after put(try_catch, 5) end,
			 {X,get(try_catch)} end,
		"begin X = try 3 of 1 -> 2; 3 -> 4 "
		"          after put(try_catch, 5) end, "
		"      {X,get(try_catch)} end.", {4,5}),
    %% Nomatch in of
    ?line error_check("try 1 of 2 -> 3 catch _:_ -> 4 end.",
		      {try_clause,1}),
    %% Nomatch in of with after
    ?line check(fun () -> {'EXIT',{{try_clause,1},_}} =
			      begin catch try 1 of 2 -> 3
				          after put(try_catch, 4) end end,
			  get(try_catch) end,
		"begin {'EXIT',{{try_clause,1},_}} = "
		"          begin catch try 1 of 2 -> 3 "
		"                      after put(try_catch, 4) end end, "
		"      get(try_catch) end. ", 4),
    %% Exception in try
    ?line check(fun () -> try 1=2 catch error:{badmatch,2} -> 3 end end,
		"try 1=2 catch error:{badmatch,2} -> 3 end.", 3),
    ?line check(fun () -> try 1=2 of 3 -> 4
			  catch error:{badmatch,2} -> 5 end end,
		"try 1=2 of 3 -> 4 "
		"catch error:{badmatch,2} -> 5 end.", 5),
    %% Exception in try with after
    ?line check(fun () -> X = try 1=2
			      catch error:{badmatch,2} -> 3
			      after put(try_catch, 4) end,
			  {X,get(try_catch)} end,
		"begin X = try 1=2 "
		"          catch error:{badmatch,2} -> 3 "
		"          after put(try_catch, 4) end, "
		"      {X,get(try_catch)} end. ", {3,4}),
    ?line check(fun () -> X = try 1=2 of 3 -> 4
			      catch error:{badmatch,2} -> 5
			      after put(try_catch, 6) end,
			  {X,get(try_catch)} end,
		"begin X = try 1=2 of 3 -> 4"
		"          catch error:{badmatch,2} -> 5 "
		"          after put(try_catch, 6) end, "
		"      {X,get(try_catch)} end. ", {5,6}),
    %% Uncaught exception
    ?line error_check("try 1=2 catch error:undefined -> 3 end. ",
		      {badmatch,2}),
    ?line error_check("try 1=2 of 3 -> 4 catch error:undefined -> 5 end. ",
		      {badmatch,2}),
    %% Uncaught exception with after
    ?line check(fun () -> {'EXIT',{{badmatch,2},_}} =
			      begin catch try 1=2
					  after put(try_catch, 3) end end,
			  get(try_catch) end,
		"begin {'EXIT',{{badmatch,2},_}} = "
		"          begin catch try 1=2 "
		"                      after put(try_catch, 3) end end, "
		"      get(try_catch) end. ", 3),
    ?line check(fun () -> {'EXIT',{{badmatch,2},_}} =
			      begin catch try 1=2 of 3 -> 4
					  after put(try_catch, 5) end end,
			  get(try_catch) end,
		"begin {'EXIT',{{badmatch,2},_}} = "
		"          begin catch try 1=2 of 3 -> 4"
		"                      after put(try_catch, 5) end end, "
		"      get(try_catch) end. ", 5),
    ?line check(fun () -> {'EXIT',{{badmatch,2},_}} =
			      begin catch try 1=2 catch error:undefined -> 3
					  after put(try_catch, 4) end end,
			  get(try_catch) end,
		"begin {'EXIT',{{badmatch,2},_}} = "
		"          begin catch try 1=2 catch error:undefined -> 3 "
		"                      after put(try_catch, 4) end end, "
		"      get(try_catch) end. ", 4),
    ?line check(fun () -> {'EXIT',{{badmatch,2},_}} =
			      begin catch try 1=2 of 3 -> 4
					  catch error:undefined -> 5
					  after put(try_catch, 6) end end,
			  get(try_catch) end,
		"begin {'EXIT',{{badmatch,2},_}} = "
		"          begin catch try 1=2 of 3 -> 4 "
		"                      catch error:undefined -> 5 "
		"                      after put(try_catch, 6) end end, "
		"      get(try_catch) end. ", 6),
    ok.


eval_expr_5(doc) ->
    ["(OTP-7933)"];
eval_expr_5(suite) ->
    [];
eval_expr_5(Config) when is_list(Config) ->
    ?line {ok,Tokens ,_} =
	erl_scan:string("if a+4 == 4 -> yes; true -> no end. "),
    ?line {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
    ?line {value, no, []} = erl_eval:expr(Expr, [], none, none, none),
    ?line no = erl_eval:expr(Expr, [], none, none, value),
    try
	erl_eval:expr(Expr, [], none, none, 4711),
	?line function_clause = should_never_reach_here
    catch
	error:function_clause ->
	    ok
    end.

eep37(Config) when is_list(Config) ->
    check(fun () -> (fun _(X) -> X end)(42) end,
          "(fun _(X) -> X end)(42).",
          42),
    check(fun () -> (fun _Id(X) -> X end)(42) end,
          "(fun _Id(X) -> X end)(42).", 42),
    check(fun () -> is_function((fun Self() -> Self end)(), 0) end,
          "is_function((fun Self() -> Self end)(), 0).",
          true),
    check(fun () ->
                  F = fun Fact(N) when N > 0 ->
                              N * Fact(N - 1);
                          Fact(0) ->
                              1
                       end,
                  F(6)
          end,
          "(fun Fact(N) when N > 0 -> N * Fact(N - 1); Fact(0) -> 1 end)(6).",
          720),
    ok.

%% Check the string in different contexts: as is; in fun; from compiled code.
check(F, String, Result) ->
    check1(F, String, Result),
    FunString = concat(["fun() -> ", no_final_dot(String), " end(). "]),
    check1(F, FunString, Result),
    CompileString = concat(["hd(lists:map(fun(_) -> ", no_final_dot(String),
                            " end, [foo])). "]),
    check1(F, CompileString, Result).

check1(F, String, Result) ->
    Result = F(),
    case catch parse_and_run(String) of
        {value, Result, _} ->
            ok;
        Other ->
            test_server:fail({eval, Other, Result})
    end.

check(F, String, Result, BoundVars, LFH, EFH) ->
    Result = F(),
    case catch parse_and_run(String, LFH, EFH) of
        {value, Result, Bs} ->
            %% We just assume that Bs is an orddict...
            Keys = orddict:fetch_keys(Bs),
            case sort(BoundVars) == Keys of
                true ->
                    ok;
                false ->
                    test_server:fail({check, BoundVars, Keys})
            end,
            ok;
        Other ->
            test_server:fail({check, Other, Result})
    end.

error_check(String, Result) ->
    case catch parse_and_run(String) of
        {'EXIT', {Result,_}} ->
            ok;
        Other ->
            test_server:fail({eval, Other, Result})
    end.

error_check(String, Result, LFH, EFH) ->
    case catch parse_and_run(String, LFH, EFH) of
        {'EXIT', {Result,_}} ->
            ok;
        Other ->
            test_server:fail({eval, Other, Result})
    end.

eval_string(String) ->
    {value, Result, _} = parse_and_run(String),
    Result.

parse_and_run(String) ->
    {ok,Tokens,_} = erl_scan:string(String),
    {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
    erl_eval:expr(Expr, []).

parse_and_run(String, LFH, EFH) ->
    {ok,Tokens,_} = erl_scan:string(String),
    {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
    erl_eval:expr(Expr, [], LFH, EFH).

no_final_dot(S) ->
    case lists:reverse(S) of
        " ." ++ R -> lists:reverse(R);
        "." ++ R -> lists:reverse(R);
        _ -> S
    end.
