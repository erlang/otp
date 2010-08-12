%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(bs_construct_SUITE).

-export([all/1,init_per_testcase/2,fin_per_testcase/2,init_all/1,finish_all/1,
	 test1/1, test2/1, test3/1, test4/1, test5/1, testf/1, not_used/1, in_guard/1,
	 coerce_to_float/1]).

-include("test_server.hrl").

all(suite) ->
    [{conf,init_all,cases(),finish_all}].

cases() ->
    [test1, test2, test3, test4, test5, testf,
     not_used, in_guard, coerce_to_float].

init_per_testcase(_Case, Config) ->
    test_lib:interpret(?MODULE),
    Dog = test_server:timetrap(?t:minutes(1)),
    [{watchdog,Dog}|Config].

fin_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

init_all(Config) when is_list(Config) ->
    ?line test_lib:interpret(?MODULE),
    ?line true = lists:member(?MODULE, int:interpreted()),
    ok.

finish_all(Config) when is_list(Config) ->
    ok.

big(1) ->
    57285702734876389752897683.

i(X) -> X.

r(L) ->
    lists:reverse(L).

-define(T(B, L), {B, ??B, L}).
-define(N(B), {B, ??B, unknown}).

-define(FAIL(Expr), ?line {'EXIT',{badarg,_}} = (catch Expr)).

l(I_13, I_big1) ->
    [
     ?T(<<-43>>,
	[256-43]),
     ?T(<<56>>,
	[56]),
     ?T(<<1,2>>,
	[1, 2]),
     ?T(<<4:4, 7:4>>,
	[4*16+7]),
     ?T(<<777:16/big>>,
	[3, 9]),
     ?T(<<777:16/little>>,
	[9, 3]),
     ?T(<<0.0:32/float>>,
	[0,0,0,0]),
     ?T(<<0.125:32/float>>,
	[62,0,0,0]),
     ?T(<<0.125:32/little-float>>,
	[0,0,0,62]),
     ?T(<<I_big1:32>>,
	[138, 99, 0, 147]),
     ?T(<<57285702734876389752897684:32>>,
	[138, 99, 0, 148]),
     ?T(<<I_big1:32/little>>,
	r([138, 99, 0, 147])),
     ?T(<<-1:17/unit:8>>,
	lists:duplicate(17, 255)),

     ?T(<<I_13>>,
	[13]),

     ?T(<<4:8/unit:2,5:2/unit:8>>,
	[0, 4, 0, 5]),

     ?T(<<1:1, 0:6, 1:1>>,
	[129]),
     ?T(<<1:1/little, 0:6/little, 1:1/little>>,
	[129]),

     ?T(<<<<1,2>>/binary>>,
	[1, 2]),
     ?T(<<<<1,2>>:1/binary>>,
	[1]),
     ?T(<<4,3,<<1,2>>:1/binary>>,
	[4,3,1]),

     ?T(<<(256*45+47)>>,
	[47]),

     ?T(<<57:0>>,
	[]),

     ?T(<<"apa">>,
	"apa"),

     ?T(<<1:3,"string",9:5>>,
	[46,110,142,77,45,204,233]),

     ?T(<<>>,
	[]),

     ?T(<<37.98:64/native-float>>,
	native_3798()),

     ?T(<<32978297842987249827298387697777669766334937:128/native-integer>>,
	native_bignum())

     ].

native_3798() ->
    case <<1:16/native>> of
	<<0,1>> -> [64,66,253,112,163,215,10,61];
	<<1,0>> -> [61,10,215,163,112,253,66,64]
    end.

native_bignum() ->
    case <<1:16/native>> of
	<<0,1>> -> [129,205,18,177,1,213,170,101,39,231,109,128,176,11,73,217];
	<<1,0>> -> [217,73,11,176,128,109,231,39,101,170,213,1,177,18,205,129]
    end.

evaluate(Str, Vars) ->
    {ok,Tokens,_} =
	erl_scan:string(Str ++ " . "),
    {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
    case erl_eval:expr(Expr, Vars) of
	{value, Result, _} ->
	    Result
    end.

eval_list([], _Vars) ->
    [];
eval_list([{C_bin, Str, Bytes} | Rest], Vars) ->
    case catch evaluate(Str, Vars) of
	{'EXIT', Error} ->
	    io:format("Evaluation error: ~p, ~p, ~p~n", [Str, Vars, Error]),
	    exit(Error);
	E_bin ->
	    [{C_bin, E_bin, Str, Bytes} | eval_list(Rest, Vars)]
    end.

one_test({C_bin, E_bin, Str, Bytes}) when list(Bytes) ->
    io:format("  ~s, ~p~n", [Str, Bytes]),
    Bin = list_to_binary(Bytes),
    if
	C_bin == Bin ->
	    ok;
	true ->
	    io:format("ERROR: Compiled: ~p. Expected ~p. Got ~p.~n",
		      [Str, Bytes, binary_to_list(C_bin)]),
	    test_server:fail(comp)
    end,
    if
	E_bin == Bin ->
	    ok;
	true ->
	    io:format("ERROR: Interpreted: ~p. Expected ~p. Got ~p.~n",
		      [Str, Bytes, binary_to_list(E_bin)]),
	    test_server:fail(comp)
    end;
one_test({C_bin, E_bin, Str, Result}) ->
    io:format("  ~s ~p~n", [Str, C_bin]),
    if
	C_bin == E_bin ->
	    ok;
	true ->
	    Arbitrary = case Result of
			    unknown ->
				size(C_bin);
			    _ ->
				Result
			end,
	    case equal_lists(binary_to_list(C_bin),
			     binary_to_list(E_bin),
			     Arbitrary) of
		false ->
		    io:format("ERROR: Compiled not equal to interpreted:"
			      "~n ~p, ~p.~n",
			      [binary_to_list(C_bin), binary_to_list(E_bin)]),
		    test_server:fail(comp);
		0 ->
		    ok;
		%% For situations where the final bits may not matter, like
		%% for floats:
		N when integer(N) ->
		    io:format("Info: compiled and interpreted differ in the"
			      " last bytes:~n ~p, ~p.~n",
			      [binary_to_list(C_bin), binary_to_list(E_bin)]),
		    ok
	    end
    end.

equal_lists([], [], _) ->
    0;
equal_lists([], _, _) ->
    false;
equal_lists(_, [], _) ->
    false;
equal_lists([A|AR], [A|BR], R) ->
    equal_lists(AR, BR, R);
equal_lists(A, B, R) ->
    if
	length(A) /= length(B) ->
	    false;
	length(A) =< R ->
	    R;
	true ->
	    false
    end.

%%% Simple working cases
test1(suite) -> [];
test1(Config) when list(Config) ->
    ?line I_13 = i(13),
    ?line I_big1 = big(1),
    ?line Vars = [{'I_13', I_13},
		  {'I_big1', I_big1}],
    ?line lists:foreach(fun one_test/1, eval_list(l(I_13, I_big1), Vars)).

%%% Misc

%%% <<A:S, A:(N-S)>>
comp(N, A, S) ->
    M1 = (1 bsl S) - 1,
    M2 = (1 bsl (N-S)) - 1,
    [((A band M1) bsl (N-S)) bor (A band M2)].

gen(N, S, A) ->
    [?T(<<A:S, A:(N-S)>>, comp(N, A, S))].

gen_l(N, S, A) ->
    [?T(<<A:S/little, A:(N-S)/little>>, comp(N, A, S))].

test2(suite) -> [];
test2(Config) when list(Config) ->
    ?line test2(0, 8, 2#10101010101010101),
    ?line test2(0, 8, 2#1111111111).

test2(End, End, _) ->
    ok;
test2(I, End, A) ->
    test2(I, A),
    test2(I+1, End, A).

test2(S, A) ->
    N = 8,
    Vars = [{'A',A}, {'N',N}, {'S',S}],
    io:format("Vars: ~p\n", [Vars]),
    lists:foreach(fun one_test/1, eval_list(gen(N, S, A), Vars)),
    lists:foreach(fun one_test/1, eval_list(gen_l(N, S, A), Vars)).

%%% Tests without facit

t3() ->
    [?N(<<4711:13, 9876:13, 3:6>>),
     ?N(<<4.57:64/float>>),
     ?N(<<4.57:32/float>>),

     ?N(<<>>)
    ].

test3(suite) -> [];
test3(Config) when list(Config) ->
    ?line Vars = [],
    ?line lists:foreach(fun one_test/1, eval_list(t3(), Vars)).

gen_u(N, S, A) ->
    [?N(<<A:S, A:(N-S)>>)].

gen_u_l(N, S, A) ->
    [?N(<<A:S/little, A:(N-S)/little>>)].

test4(suite) -> [];
test4(Config) when list(Config) ->
    ?line test4(0, 16, 2#10101010101010101),
    ?line test4(0, 16, 2#1111111111).

test4(End, End, _) ->
    ok;
test4(I, End, A) ->
    test4(I, A),
    test4(I+1, End, A).

test4(S, A) ->
    N = 16,
    Vars = [{'A', A}, {'N', 16}, {'S', S}],
    lists:foreach(fun one_test/1, eval_list(gen_u(N, S, A), Vars)),
    lists:foreach(fun one_test/1, eval_list(gen_u_l(N, S, A), Vars)).

gen_b(N, S, A) ->
    [?T(<<A:S/binary-unit:1, A:(N-S)/binary-unit:1>>,
	binary_to_list(<<A:S/binary-unit:1, A:(N-S)/binary-unit:1>>))].

test5(suite) -> [];
test5(doc) -> ["OTP-3995"];
test5(Config) when list(Config) ->
    ?line test5(0, 8, <<73>>),
    ?line test5(0, 8, <<68>>).

test5(End, End, _) ->
    ok;
test5(I, End, A) ->
    test5(I, A),
    test5(I+1, End, A).

test5(S, A) ->
    N = 8,
    Vars = [{'A', A}, {'N', 8}, {'S', S}],
    lists:foreach(fun one_test/1, eval_list(gen_b(N, S, A), Vars)).

%%% Failure cases
testf(suite) -> [];
testf(Config) when list(Config) ->
    ?FAIL(<<3.14>>),
    ?FAIL(<<<<1,2>>>>),

    ?FAIL(<<2.71/binary>>),
    ?FAIL(<<24334/binary>>),
    ?FAIL(<<24334344294788947129487129487219847/binary>>),

    ?FAIL(<<<<1,2,3>>/float>>),

    %% Negative field widths.
    testf_1(-8, <<1,2,3,4,5>>),

    ?FAIL(<<42:(-16)>>),
    ?FAIL(<<3.14:(-8)/float>>),
    ?FAIL(<<<<23,56,0,2>>:(-16)/binary>>),
    ?FAIL(<<<<23,56,0,2>>:(2.5)/binary>>),
    ?FAIL(<<<<23,56,0,2>>:(anka)>>),

    ok.

testf_1(W, B) ->
    ?FAIL(<<42:W>>),
    ?FAIL(<<3.14:W/float>>),
    ?FAIL(<<B:W/binary>>).

not_used(doc) ->
    "Test that constructed binaries that are not used will still give an exception.";
not_used(Config) when is_list(Config) ->
    ?line ok = not_used1(3, <<"dum">>),
    ?line ?FAIL(not_used1(3, "dum")),
    ?line ?FAIL(not_used2(444, -2)),
    ?line ?FAIL(not_used2(444, anka)),
    ?line ?FAIL(not_used3(444)),
    ok.

not_used1(I, BinString) ->
    <<I:32,BinString/binary>>,
    ok.

not_used2(I, Sz) ->
    <<I:Sz>>,
    ok.

not_used3(I) ->
    <<I:(-8)>>,
    ok.

in_guard(Config) when list(Config) ->
    ?line 1 = in_guard(<<16#74ad:16>>, 16#e95, 5),
    ?line 2 = in_guard(<<16#3A,16#F7,"hello">>, 16#3AF7, <<"hello">>),
    ?line 3 = in_guard(<<16#FBCD:14,3.1415/float,3:2>>, 16#FBCD, 3.1415),
    nope = in_guard(<<1>>, 42, b),
    nope = in_guard(<<1>>, a, b),
    nope = in_guard(<<1,2>>, 1, 1),
    nope = in_guard(<<4,5>>, 1, 2.71),
    nope = in_guard(<<4,5>>, 1, <<12,13>>),
    ok.

in_guard(Bin, A, B) when <<A:13,B:3>> == Bin -> 1;
in_guard(Bin, A, B) when <<A:16,B/binary>> == Bin -> 2;
in_guard(Bin, A, B) when <<A:14,B/float,3:2>> == Bin -> 3;
in_guard(Bin, A, B) when {a,b,<<A:14,B/float,3:2>>} == Bin -> cant_happen;
in_guard(_, _, _) -> nope.

-define(COF(Int0),
	?line (fun(Int) ->
		       true = <<Int:32/float>> =:= <<(float(Int)):32/float>>,
		       true = <<Int:64/float>> =:= <<(float(Int)):64/float>>
			   end)(nonliteral(Int0)),
	?line true = <<Int0:32/float>> =:= <<(float(Int0)):32/float>>,
	?line true = <<Int0:64/float>> =:= <<(float(Int0)):64/float>>).

-define(COF64(Int0),
	?line (fun(Int) ->
		       true = <<Int:64/float>> =:= <<(float(Int)):64/float>>
			   end)(nonliteral(Int0)),
	?line true = <<Int0:64/float>> =:= <<(float(Int0)):64/float>>).

nonliteral(X) -> X.

coerce_to_float(Config) when list(Config) ->
    ?COF(0),
    ?COF(-1),
    ?COF(1),
    ?COF(42),
    ?COF(255),
    ?COF(-255),
    ?COF(38474),
    ?COF(387498738948729893849444444443),
    ?COF(-37489378937773899999999999999993),
    ?COF64(298748888888888888888888888883478264866528467367364766666666666666663),
    ?COF64(-367546729879999999999947826486652846736736476555566666663),
    ok.
