%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2012. All Rights Reserved.
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

%% Copied from bs_construct_SUITE in the emulator test suite.
%% The following test cases have been omitted since they don't
%% make much sense for the debugger:
%%  bs_add
%%  kostis

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 init_per_suite/1,end_per_suite/1,
	 test1/1, test2/1, test3/1, test4/1, test5/1, testf/1,
	 not_used/1, in_guard/1,
	 mem_leak/1, coerce_to_float/1, bjorn/1,
	 huge_float_field/1, huge_binary/1, system_limit/1, badarg/1,
	 copy_writable_binary/1, dynamic/1,
	 otp_7422/1, zero_width/1]).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [test1, test2, test3, test4, test5, testf, not_used,
     in_guard, mem_leak, coerce_to_float, bjorn,
     huge_float_field, huge_binary, system_limit, badarg,
     copy_writable_binary, dynamic, otp_7422, zero_width].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    test_lib:interpret(?MODULE),
    Dog = test_server:timetrap(?t:minutes(15)),
    [{watchdog,Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

init_per_suite(Config) when is_list(Config) ->
    ?line test_lib:interpret(?MODULE),
    ?line true = lists:member(?MODULE, int:interpreted()),
    Config.

end_per_suite(Config) when is_list(Config) ->
    ok.

big(1) ->
    57285702734876389752897683.

i(X) -> X.

r(L) ->
    lists:reverse(L).

-define(T(B, L), {B, ??B, L}).
-define(N(B), {B, ??B, unknown}).

-define(FAIL(Expr), ?line fail_check(catch Expr, ??Expr, [])).

-define(FAIL_VARS(Expr, Vars), ?line fail_check(catch Expr, ??Expr, Vars)).

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
	native_bignum()),

     %% Unit tests.
     ?T(<<<<5:3>>/bitstring>>, <<5:3>>),
     ?T(<<42,<<7:4>>/binary-unit:4>>, <<42,7:4>>),
     ?T(<<<<344:17>>/binary-unit:17>>, <<344:17>>),
     ?T(<<<<42,3,7656:16>>/binary-unit:16>>, <<42,3,7656:16>>)

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

one_test({C_bin, E_bin, Str, Bytes}) when is_list(Bytes) ->
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
		N when is_integer(N) ->
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

fail_check({'EXIT',{badarg,_}}, Str, Vars) ->
    try	evaluate(Str, Vars) of
	Res ->
	    io:format("Interpreted result: ~p", [Res]),
	    ?t:fail(did_not_fail_in_intepreted_code)
    catch
	error:badarg ->
	    ok
    end;
fail_check(Res, _, _) ->
    io:format("Compiled result: ~p", [Res]),
    ?t:fail(did_not_fail_in_compiled_code).

%%% Simple working cases
test1(suite) -> [];
test1(Config) when is_list(Config) ->
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
test2(Config) when is_list(Config) ->
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
test3(Config) when is_list(Config) ->
    ?line Vars = [],
    ?line lists:foreach(fun one_test/1, eval_list(t3(), Vars)).

gen_u(N, S, A) ->
    [?N(<<A:S, A:(N-S)>>)].

gen_u_l(N, S, A) ->
    [?N(<<A:S/little, A:(N-S)/little>>)].

test4(suite) -> [];
test4(Config) when is_list(Config) ->
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
test5(Config) when is_list(Config) ->
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
testf(Config) when is_list(Config) ->
    ?line ?FAIL(<<3.14>>),
    ?line ?FAIL(<<<<1,2>>>>),

    ?line ?FAIL(<<2.71/binary>>),
    ?line ?FAIL(<<24334/binary>>),
    ?line ?FAIL(<<24334344294788947129487129487219847/binary>>),
    BigInt = id(24334344294788947129487129487219847),
    ?line ?FAIL_VARS(<<BigInt/binary>>, [{'BigInt',BigInt}]),
    ?line ?FAIL_VARS(<<42,BigInt/binary>>, [{'BigInt',BigInt}]),
    ?line ?FAIL_VARS(<<BigInt:2/binary>>, [{'BigInt',BigInt}]),

    %% One negative field size, but the sum of field sizes will be 1 byte.
    %% Make sure that we reject that properly.
    I_minus_777 = id(-777),
    I_minus_2047 = id(-2047),
    ?line ?FAIL_VARS(<<I_minus_777:2048/unit:8,57:I_minus_2047/unit:8>>,
		     ordsets:from_list([{'I_minus_777',I_minus_777},
					{'I_minus_2047',I_minus_2047}])),
    ?line ?FAIL(<<<<1,2,3>>/float>>),

    %% Negative field widths.
    ?line testf_1(-8, <<1,2,3,4,5>>),
    ?line ?FAIL(<<0:(-(1 bsl 100))>>),

    ?line ?FAIL(<<42:(-16)>>),
    ?line ?FAIL(<<3.14:(-8)/float>>),
    ?line ?FAIL(<<<<23,56,0,2>>:(-16)/binary>>),
    ?line ?FAIL(<<<<23,56,0,2>>:(2.5)/binary>>),
    ?line ?FAIL(<<<<23,56,0,2>>:(anka)>>),
    ?line ?FAIL(<<<<23,56,0,2>>:(anka)>>),

    %% Unit failures.
    ?line ?FAIL(<<<<1:1>>/binary>>),
    Sz = id(1),
    ?line ?FAIL_VARS(<<<<1:Sz>>/binary>>, [{'Sz',Sz}]),
    ?line {'EXIT',{badarg,_}} = (catch <<<<1:(id(1))>>/binary>>),
    ?line ?FAIL(<<<<7,8,9>>/binary-unit:16>>),
    ?line ?FAIL(<<<<7,8,9,3:7>>/binary-unit:16>>),
    ?line ?FAIL(<<<<7,8,9,3:7>>/binary-unit:17>>),

    ok.

testf_1(W, B) ->
    Vars = [{'W',W}],
    ?FAIL_VARS(<<42:W>>, Vars),
    ?FAIL_VARS(<<3.14:W/float>>, Vars),
    ?FAIL_VARS(<<B:W/binary>>, [{'B',B}|Vars]).

not_used(doc) ->
    "Test that constructed binaries that are not used will still give an exception.";
not_used(Config) when is_list(Config) ->
    ?line ok = not_used1(3, <<"dum">>),
    ?line {'EXIT',{badarg,_}} = (catch not_used1(3, "dum")),
    ?line {'EXIT',{badarg,_}} = (catch not_used2(444, -2)),
    ?line {'EXIT',{badarg,_}} = (catch not_used2(444, anka)),
    ?line {'EXIT',{badarg,_}} = (catch not_used3(444)),
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

in_guard(Config) when is_list(Config) ->
    ?line 1 = in_guard(<<16#74ad:16>>, 16#e95, 5),
    ?line 2 = in_guard(<<16#3A,16#F7,"hello">>, 16#3AF7, <<"hello">>),
    ?line 3 = in_guard(<<16#FBCD:14,3.1415/float,3:2>>, 16#FBCD, 3.1415),
    ?line 3 = in_guard(<<16#FBCD:14,3/float,3:2>>, 16#FBCD, 3),
    ?line 3 = in_guard(<<16#FBCD:14,(2 bsl 226)/float,3:2>>, 16#FBCD, 2 bsl 226),
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

mem_leak(doc) -> "Make sure that construction has no memory leak";
mem_leak(Config) when is_list(Config) ->
    ?line B = make_bin(16, <<0>>),
    ?line mem_leak(1024, B),
    ok.

mem_leak(0, _) -> ok;
mem_leak(N, B) ->
    ?line big_bin(B, <<23>>),
    ?line {'EXIT',{badarg,_}} = (catch big_bin(B, bad)),
    mem_leak(N-1, B).

big_bin(B1, B2) ->
    <<B1/binary,B1/binary,B1/binary,B1/binary,
      B1/binary,B1/binary,B1/binary,B1/binary,
      B1/binary,B1/binary,B1/binary,B1/binary,
      B1/binary,B1/binary,B1/binary,B1/binary,
      B2/binary>>.

make_bin(0, Acc) -> Acc;
make_bin(N, Acc) -> make_bin(N-1, <<Acc/binary,Acc/binary>>).

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

coerce_to_float(Config) when is_list(Config) ->
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

bjorn(Config) when is_list(Config) ->
    ?line error = bjorn_1(),
    ok.

bjorn_1() ->
    Bitstr = <<7:13>>,
    try
	do_something()
    catch
	throw:blurf ->
	    ignore
    end,
    do_more(Bitstr, 13).

do_more(Bin, Sz) ->
    %% Previous bug in the bs_bits_to_bytes instruction: The exeption code
    %% was not set - the previous exception (throw:blurf) would be used,
    %% causing the catch to slip.
    try <<Bin:Sz/binary>> of
	_V -> ok
    catch
	error:_ ->
	    error
    end.

do_something() ->
    throw(blurf).

huge_float_field(Config) when is_list(Config) ->
    ?line {'EXIT',{badarg,_}} = (catch <<0.0:9/float-unit:8>>),
    ?line huge_float_check(catch <<0.0:67108865/float-unit:64>>),
    ?line huge_float_check(catch <<0.0:((1 bsl 26)+1)/float-unit:64>>),
    ?line huge_float_check(catch <<0.0:(id(67108865))/float-unit:64>>),
%%  ?line huge_float_check(catch <<0.0:((1 bsl 60)+1)/float-unit:64>>),
    ?line huge_float_check(catch <<3839739387439387383739387987347983:((1 bsl 26)+1)/float-unit:64>>),
%%  ?line huge_float_check(catch <<3839739387439387383739387987347983:((1 bsl 60)+1)/float-unit:64>>),
    ok.

huge_float_check({'EXIT',{system_limit,_}}) -> ok;
huge_float_check({'EXIT',{badarg,_}}) -> ok.

huge_binary(Config) when is_list(Config) ->
    ?line 16777216 = size(<<0:(id(1 bsl 26)),(-1):(id(1 bsl 26))>>),
    ok.

system_limit(Config) when is_list(Config) ->
    WordSize = erlang:system_info(wordsize),
    BitsPerWord = WordSize * 8,
    ?line {'EXIT',{system_limit,_}} =
	(catch <<0:(id(0)),42:(id(1 bsl BitsPerWord))>>),
    ?line {'EXIT',{system_limit,_}} =
	(catch <<42:(id(1 bsl BitsPerWord)),0:(id(0))>>),
    ?line {'EXIT',{system_limit,_}} =
	(catch <<(id(<<>>))/binary,0:(id(1 bsl 100))>>),

    case WordSize of
	4 ->
	    system_limit_32();
	8 ->
	    ok
    end.

system_limit_32() ->
    ?line {'EXIT',{badarg,_}} = (catch <<42:(-1)>>),
    ?line {'EXIT',{badarg,_}} = (catch <<42:(id(-1))>>),
    ?line {'EXIT',{badarg,_}} = (catch <<42:(id(-389739873536870912))/unit:8>>),
    ?line {'EXIT',{system_limit,_}} = (catch <<42:536870912/unit:8>>),
    ?line {'EXIT',{system_limit,_}} = (catch <<42:(id(536870912))/unit:8>>),
    ?line {'EXIT',{system_limit,_}} = (catch <<0:(id(8)),42:536870912/unit:8>>),
    ?line {'EXIT',{system_limit,_}} =
	(catch <<0:(id(8)),42:(id(536870912))/unit:8>>),
    ok.

badarg(Config) when is_list(Config) ->
    %% BEAM will generate a badarg exception for:
    %%   <<0:(id(1 bsl 100)),0:(id(-1))>>
    %% but the debugger will generate a system_limit exception.
    %% It does not seems worthwhile to fix the debugger.

    ?line {'EXIT',{badarg,_}} =
	(catch <<(id(<<>>))/binary,0:(id(-(1 bsl 100)))>>),

    ok.

copy_writable_binary(Config) when is_list(Config) ->
    ?line [copy_writable_binary_1(I) || I <- lists:seq(0, 256)],
    ok.

copy_writable_binary_1(_) ->
    ?line Bin0 = <<(id(<<>>))/binary,0,1,2,3,4,5,6,7>>,
    ?line SubBin = make_sub_bin(Bin0),
    ?line id(<<42,34,55,Bin0/binary>>),		%Make reallocation likelier.
    ?line Pid = spawn(fun() ->
			      copy_writable_binary_holder(Bin0, SubBin)
		      end),
    ?line Tab = ets:new(holder, []),
    ?line ets:insert(Tab, {17,Bin0}),
    ?line ets:insert(Tab, {42,SubBin}),
    ?line id(<<Bin0/binary,0:(64*1024*8)>>),
    ?line Pid ! self(),
    ?line [{17,Bin0}] = ets:lookup(Tab, 17),
    ?line [{42,Bin0}] = ets:lookup(Tab, 42),
    receive
	{Pid,Bin0,Bin0} -> ok;
	Other ->
	    io:format("Unexpected message: ~p", [Other]),
	    ?line ?t:fail()
    end,
    ok.

copy_writable_binary_holder(Bin, SubBin) ->
    receive
	Pid ->
	    Pid ! {self(),Bin,SubBin}
    end.

make_sub_bin(Bin0) ->
    N = bit_size(Bin0),
    <<_:17,Bin:N/bitstring,_:5>> = <<(-1):17,Bin0/bitstring,(-1):5>>,
    Bin = Bin0,					%Assertion.
    Bin.

%% Test that different ways of using bit syntax instructions
%% give the same result.

dynamic(Config) when is_list(Config) ->
    ?line dynamic_1(fun dynamic_big/5),
    ?line dynamic_1(fun dynamic_little/5),
    ok.

dynamic_1(Dynamic) ->
    <<Lpad:128>> = erlang:md5([0]),
    <<Rpad:128>> = erlang:md5([1]),
    <<Int:128>> = erlang:md5([2]),
    8385 = dynamic_2(0, {Int,Lpad,Rpad,Dynamic}, 0).

dynamic_2(129, _, Count) -> Count;
dynamic_2(Bef, Data, Count0) ->
    Count = dynamic_3(Bef, 128-Bef, Data, Count0),
    dynamic_2(Bef+1, Data, Count).

dynamic_3(_, -1, _, Count) -> Count;
dynamic_3(Bef, N, {Int0,Lpad,Rpad,Dynamic}=Data, Count) ->
    Int1 = Int0 band ((1 bsl (N+3))-1),
    Dynamic(Bef, N, Int1, Lpad, Rpad),
    Dynamic(Bef, N, -Int1, Lpad, Rpad),

    %% OTP-7085: Test a small number in a wide field.
    Int2 = Int0 band 16#FFFFFF,
    Dynamic(Bef, N, Int2, Lpad, Rpad),
    Dynamic(Bef, N, -Int2, Lpad, Rpad),
    dynamic_3(Bef, N-1, Data, Count+1).

dynamic_big(Bef, N, Int, Lpad, Rpad) ->
    NumBin = id(<<Int:N>>),
    MaskedInt = Int band ((1 bsl N) - 1),
    <<MaskedInt:N>> = NumBin,

    %% Construct the binary in two different ways.
    Bin = id(<<Lpad:Bef,NumBin/bitstring,Rpad:(128-Bef-N)>>),
    Bin = <<Lpad:Bef,Int:N,Rpad:(128-Bef-N)>>,

    %% Further verify the result by matching.
    LpadMasked = Lpad band ((1 bsl Bef) - 1),
    RpadMasked = Rpad band ((1 bsl (128-Bef-N)) - 1),
    Rbits = (128-Bef-N),
    <<LpadMasked:Bef,MaskedInt:N,RpadMasked:Rbits>> = id(Bin),
    ok.

dynamic_little(Bef, N, Int, Lpad, Rpad) ->
    NumBin = id(<<Int:N/little>>),
    MaskedInt = Int band ((1 bsl N) - 1),
    <<MaskedInt:N/little>> = NumBin,

    %% Construct the binary in two different ways.
    Bin = id(<<Lpad:Bef/little,NumBin/bitstring,Rpad:(128-Bef-N)/little>>),
    Bin = <<Lpad:Bef/little,Int:N/little,Rpad:(128-Bef-N)/little>>,

    %% Further verify the result by matching.
    LpadMasked = Lpad band ((1 bsl Bef) - 1),
    RpadMasked = Rpad band ((1 bsl (128-Bef-N)) - 1),
    Rbits = (128-Bef-N),
    <<LpadMasked:Bef/little,MaskedInt:N/little,RpadMasked:Rbits/little>> = id(Bin),
    ok.

otp_7422(Config) when is_list(Config) ->
    otp_7422_int(0),
    otp_7422_bin(0).

otp_7422_int(N) when N < 512 ->
    T = erlang:make_tuple(N, []),
    spawn_link(fun() ->
		       id(T),
		       %% A size of field 0 would write one byte beyond
		       %% the current position in the binary. It could
		       %% overwrite the continuation pointer stored on
		       %% the stack if HTOP was equal to E (the stack pointer).
		       id(<<0:(id(0))>>)
	       end),
    otp_7422_int(N+1);
otp_7422_int(_) -> ok.

otp_7422_bin(N) when N < 512 ->
    T = erlang:make_tuple(N, []),
    Z = id(<<>>),
    spawn_link(fun() ->
		       id(T),
		       id(<<Z:(id(0))/bits>>)
	       end),
    otp_7422_bin(N+1);
otp_7422_bin(_) -> ok.

zero_width(Config) when is_list(Config) ->
    ?line Z = id(0),
    Small = id(42),
    Big = id(1 bsl 128),
    ?line <<>> = <<Small:Z>>,
    ?line <<>> = <<Small:0>>,
    ?line <<>> = <<Big:Z>>,
    ?line <<>> = <<Big:0>>,

    ?line {'EXIT',{badarg,_}} = (catch <<not_a_number:0>>),
    ?line {'EXIT',{badarg,_}} = (catch <<(id(not_a_number)):Z>>),
    ?line {'EXIT',{badarg,_}} = (catch <<(id(not_a_number)):0>>),

    ok.

id(I) -> I.
