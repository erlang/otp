%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2024. All Rights Reserved.
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

-module(bs_construct_SUITE).

-export([all/0, suite/0,
         init_per_suite/1, end_per_suite/1,
	 test1/1, test2/1, test3/1, test4/1, test5/1, testf/1,
	 not_used/1, in_guard/1,
	 mem_leak/1, coerce_to_float/1, bjorn/1, append_empty_is_same/1,
	 huge_float_field/1, system_limit/1, badarg/1,
	 copy_writable_binary/1, kostis/1, dynamic/1, bs_add/1,
	 otp_7422/1, zero_width/1, bad_append/1, bs_append_overflow/1,
         bs_append_offheap/1,
         reductions/1, fp16/1, zero_init/1, error_info/1, little/1,
         heap_binary_unit/1,
         otp_24_code_gh_8238/1
        ]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() -> 
    [test1, test2, test3, test4, test5, testf, not_used,
     in_guard, mem_leak, coerce_to_float, bjorn, append_empty_is_same,
     huge_float_field, system_limit, badarg,
     copy_writable_binary, kostis, dynamic, bs_add, otp_7422, zero_width,
     bad_append, bs_append_overflow, bs_append_offheap,
     reductions, fp16, zero_init,
     error_info, little, heap_binary_unit,
     otp_24_code_gh_8238].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    application:stop(os_mon).

-define(T(B, L), {B, ??B, L}).
-define(N(B), {B, ??B, unknown}).

-define(FAIL(Expr), fail_check(catch Expr, ??Expr, [])).

-define(FAIL_VARS(Expr, Vars), fail_check(catch Expr, ??Expr, Vars)).

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
	lists:reverse([138, 99, 0, 147])),
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

     ?T(<< <<153,27:5>>:I_13/bits, 1:3 >>,
        [153,217]),

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

     ?T(<<$э/native-utf16,$т/native-utf16,$о/native-utf16," спутник"/native-utf16>>,
        native_utf16()),

     ?T(<<$в/native-utf32,"ода"/native-utf32>>,
	native_utf32()),

     %% Unit tests.
     ?T(<<<<5:3>>/bitstring>>, <<5:3>>),
     ?T(<<42,<<7:4>>/binary-unit:4>>, <<42,7:4>>),
     ?T(<<<<344:17>>/binary-unit:17>>, <<344:17>>),
     ?T(<<<<42,3,7656:16>>/binary-unit:16>>, <<42,3,7656:16>>),

     %% Different sizes and types. First without types.
     ?T(<<I_big1:8>>, [147]),
     ?T(<<I_big1:16>>, [0, 147]),
     ?T(<<I_big1:24>>, [99, 0, 147]),
     ?T(<<I_big1:32>>, [138, 99, 0, 147]),
     ?T(<<I_big1:40>>, [5, 138, 99, 0, 147]),
     ?T(<<I_big1:48>>, [229, 5, 138, 99, 0, 147]),
     ?T(<<I_big1:56>>, [249, 229, 5, 138, 99, 0, 147]),
     ?T(<<I_big1:64>>, [42, 249, 229, 5, 138, 99, 0, 147]),

     %% Known integer with range.
     ?T(<<(I_big1 band ((1 bsl 56) - 1)):8>>, [147]),
     ?T(<<(I_big1 band ((1 bsl 56) - 1)):16>>, [0, 147]),
     ?T(<<(I_big1 band ((1 bsl 56) - 1)):24>>, [99, 0, 147]),
     ?T(<<(I_big1 band ((1 bsl 56) - 1)):32>>, [138, 99, 0, 147]),
     ?T(<<(I_big1 band ((1 bsl 56) - 1)):40>>, [5, 138, 99, 0, 147]),
     ?T(<<(I_big1 band ((1 bsl 56) - 1)):48>>, [229, 5, 138, 99, 0, 147]),
     ?T(<<(I_big1 band ((1 bsl 56) - 1)):56>>, [249, 229, 5, 138, 99, 0, 147]),
     ?T(<<(I_big1 band ((1 bsl 64) - 1)):64>>, [42, 249, 229, 5, 138, 99, 0, 147]),

     %% Known integer with exact range.
     ?T(<<(I_big1 band ((1 bsl  8) - 1)):8>>, [147]),
     ?T(<<(I_big1 band ((1 bsl 16) - 1)):16>>, [0, 147]),
     ?T(<<(I_big1 band ((1 bsl 24) - 1)):24>>, [99, 0, 147]),
     ?T(<<(I_big1 band ((1 bsl 32) - 1)):32>>, [138, 99, 0, 147]),
     ?T(<<(I_big1 band ((1 bsl 40) - 1)):40>>, [5, 138, 99, 0, 147]),
     ?T(<<(I_big1 band ((1 bsl 48) - 1)):48>>, [229, 5, 138, 99, 0, 147]),

     %% Known integer without range.
     ?T(<<(I_big1 + 0):8>>, [147]),
     ?T(<<(I_big1 + 0):16>>, [0, 147]),
     ?T(<<(I_big1 + 0):24>>, [99, 0, 147]),
     ?T(<<(I_big1 + 0):32>>, [138, 99, 0, 147]),
     ?T(<<(I_big1 + 0):40>>, [5, 138, 99, 0, 147]),
     ?T(<<(I_big1 + 0):48>>, [229, 5, 138, 99, 0, 147]),
     ?T(<<(I_big1 + 0):56>>, [249, 229, 5, 138, 99, 0, 147]),
     ?T(<<(I_big1 + 0):64>>, [42, 249, 229, 5, 138, 99, 0, 147]),

     %% Known integer. Verify that the value does not bleed into the
     %% previous segment.
     ?T(<<1, (I_big1 + 0):8>>,  [1, 147]),
     ?T(<<2, (I_big1 + 0):16>>, [2, 0, 147]),
     ?T(<<3, (I_big1 + 0):24>>, [3, 99, 0, 147]),
     ?T(<<4, (I_big1 + 0):32>>, [4, 138, 99, 0, 147]),
     ?T(<<5, (I_big1 + 0):40>>, [5, 5, 138, 99, 0, 147]),
     ?T(<<6, (I_big1 + 0):48>>, [6, 229, 5, 138, 99, 0, 147]),
     ?T(<<7, (I_big1 + 0):56>>, [7, 249, 229, 5, 138, 99, 0, 147]),
     ?T(<<8, (I_big1 + 0):64>>, [8, 42, 249, 229, 5, 138, 99, 0, 147]),

     %% Test non-byte sizes.
     ?T(<<I_big1:33>>, <<197,49,128,73,1:1>>),
     ?T(<<I_big1:39>>, <<11,20,198,1,19:7>>),

     ?T(<<I_big1:57>>, <<124,242,130,197,49,128,73,1:1>>),
     ?T(<<I_big1:58>>, <<190,121,65,98,152,192,36,3:2>>),
     ?T(<<I_big1:59>>, <<95,60,160,177,76,96,18,3:3>>),
     ?T(<<I_big1:60>>, <<175,158,80,88,166,48,9,3:4>>),
     ?T(<<I_big1:61>>, <<87,207,40,44,83,24,4,19:5>>),
     ?T(<<I_big1:62>>, <<171,231,148,22,41,140,2,19:6>>),
     ?T(<<I_big1:63>>, <<85,243,202,11,20,198,1,19:7>>),

     %% Test non-byte sizes and also that the value does not bleed
     %% into the previous segment.
     ?T(<<17, I_big1:33>>, <<17, 197,49,128,73,1:1>>),
     ?T(<<19, I_big1:39>>, <<19, 11,20,198,1,19:7>>)
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

native_utf16() ->
    case <<1:16/native>> of
	<<0,1>> -> [4,77,4,66,4,62,0,32,4,65,4,63,4,67,4,66,4,61,4,56,4,58];
        <<1,0>> -> [77,4,66,4,62,4,32,0,65,4,63,4,67,4,66,4,61,4,56,4,58,4]
    end.

native_utf32() ->
    case <<1:16/native>> of
	<<0,1>> -> [0,0,4,50,0,0,4,62,0,0,4,52,0,0,4,48];
        <<1,0>> -> [50,4,0,0,62,4,0,0,52,4,0,0,48,4,0,0]
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
    io:format("  ~ts, ~p~n", [Str, Bytes]),
    Bin = list_to_binary(Bytes),
    if
	C_bin == Bin ->
	    ok;
	true ->
	    io:format("ERROR: Compiled: ~p.~nExpected ~p.~nGot ~p.~n",
		      [Str, Bytes, binary_to_list(C_bin)]),
	    ct:fail(comp)
    end,
    if
	E_bin == Bin ->
	    ok;
	true ->
	    io:format("ERROR: Interpreted: ~p.~nExpected ~p.~nGot ~p.~n",
		      [Str, Bytes, binary_to_list(E_bin)]),
	    ct:fail(comp)
    end;
one_test({C_bin, E_bin, Str, Result}) ->
    io:format("  ~ts ~p~n", [Str, C_bin]),
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
		    ct:fail(comp);
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
	    ct:fail(did_not_fail_in_intepreted_code)
    catch
	error:badarg ->
	    ok
    end;
fail_check(Res, _, _) ->
    io:format("Compiled result: ~p", [Res]),
    ct:fail(did_not_fail_in_compiled_code).

%%% Simple working cases
test1(Config) when is_list(Config) ->
    I_13 = id(13),
    I_big1 = id(57285702734876389752897683),
    Vars = [{'I_13', I_13},
            {'I_big1', I_big1}],
    lists:foreach(fun one_test/1, eval_list(l(I_13, I_big1), Vars)).

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

test2(Config) when is_list(Config) ->
    test2(0, 8, 2#10101010101010101),
    test2(0, 8, 2#1111111111).

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

test3(Config) when is_list(Config) ->
    Vars = [],
    lists:foreach(fun one_test/1, eval_list(t3(), Vars)).

gen_u(N, S, A) ->
    [?N(<<A:S, A:(N-S)>>)].

gen_u_l(N, S, A) ->
    [?N(<<A:S/little, A:(N-S)/little>>)].

test4(Config) when is_list(Config) ->
    test4(0, 16, 2#10101010101010101),
    test4(0, 16, 2#1111111111).

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

%% OTP-3995
test5(Config) when is_list(Config) ->
    test5(0, 8, <<73>>),
    test5(0, 8, <<68>>).

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
testf(Config) when is_list(Config) ->
    ?FAIL(<<3.14>>),
    ?FAIL(<<<<1,2>>>>),

    ?FAIL(<<2.71/binary>>),
    ?FAIL(<<24334/binary>>),
    ?FAIL(<<24334344294788947129487129487219847/binary>>),
    BigInt = id(24334344294788947129487129487219847),
    ?FAIL_VARS(<<BigInt/binary>>, [{'BigInt',BigInt}]),
    ?FAIL_VARS(<<42,BigInt/binary>>, [{'BigInt',BigInt}]),
    ?FAIL_VARS(<<BigInt:2/binary>>, [{'BigInt',BigInt}]),

    %% One negative field size, but the sum of field sizes will be 1 byte.
    %% Make sure that we reject that properly.
    I_minus_777 = id(-777),
    I_minus_2047 = id(-2047),
    ?FAIL_VARS(<<I_minus_777:2048/unit:8,57:I_minus_2047/unit:8>>,
		     ordsets:from_list([{'I_minus_777',I_minus_777},
					{'I_minus_2047',I_minus_2047}])),
    ?FAIL(<<<<1,2,3>>/float>>),

    %% Negative field widths.
    testf_1(-8, <<1,2,3,4,5>>),
    ?FAIL(<<0:(-(1 bsl 100))>>),

    ?FAIL(<<42:(-16)>>),
    ?FAIL(<<3.14:(-8)/float>>),
    ?FAIL(<<<<23,56,0,2>>:(-16)/binary>>),
    ?FAIL(<<<<23,56,0,2>>:(2.5)/binary>>),
    ?FAIL(<<<<23,56,0,2>>:(anka)>>),
    ?FAIL(<<<<23,56,0,2>>:(anka)>>),

    %% Unit failures.
    ?FAIL(<<<<1:1>>/binary>>),
    Sz = id(1),
    ?FAIL_VARS(<<<<1:Sz>>/binary>>, [{'Sz',Sz}]),
    {'EXIT',{badarg,_}} = (catch <<<<1:(id(1))>>/binary>>),
    ?FAIL(<<<<7,8,9>>/binary-unit:16>>),
    ?FAIL(<<<<7,8,9,3:7>>/binary-unit:16>>),
    ?FAIL(<<<<7,8,9,3:7>>/binary-unit:17>>),

    %% Failures not deteced by v3_core. Those must be detected at
    %% runtime.
    Atom = id(ok),
    Float = id(2.71),
    List = id([-1,0,1]),
    NonBinaries = [{'Atom',Atom}, {'Float',Float}, {'List',List}],
    ?FAIL_VARS(<<Atom/bits>>, NonBinaries),
    ?FAIL_VARS(<<Atom/bits,0>>, NonBinaries),
    ?FAIL_VARS(<<0,Atom/bits>>, NonBinaries),
    ?FAIL_VARS(<<Float/bits>>, NonBinaries),
    ?FAIL_VARS(<<Float/bits,0>>, NonBinaries),
    ?FAIL_VARS(<<List/bits>>, NonBinaries),
    ?FAIL_VARS(<<List/bits,0>>, NonBinaries),

    ok.

testf_1(W, B) ->
    Vars = [{'W',W}],
    ?FAIL_VARS(<<42:W>>, Vars),
    ?FAIL_VARS(<<3.14:W/float>>, Vars),
    ?FAIL_VARS(<<B:W/binary>>, [{'B',B}|Vars]).

%% Test that constructed binaries that are not used will still give an exception.
not_used(Config) when is_list(Config) ->
    ok = not_used1(3, <<"dum">>),
    {'EXIT',{badarg,_}} = (catch not_used1(3, "dum")),
    {'EXIT',{badarg,_}} = (catch not_used2(444, -2)),
    {'EXIT',{badarg,_}} = (catch not_used2(444, anka)),
    {'EXIT',{badarg,_}} = (catch not_used3(444)),
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
    1 = in_guard(<<16#74ad:16>>, 16#e95, 5),
    2 = in_guard(<<16#3A,16#F7,"hello">>, 16#3AF7, <<"hello">>),
    3 = in_guard(<<16#FBCD:14,3.1415/float,3:2>>, 16#FBCD, 3.1415),
    3 = in_guard(<<16#FBCD:14,3/float,3:2>>, 16#FBCD, 3),
    3 = in_guard(<<16#FBCD:14,(2 bsl 226)/float,3:2>>, 16#FBCD, 2 bsl 226),
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

%% Make sure that construction has no memory leak
mem_leak(Config) when is_list(Config) ->
    B = make_bin(16, <<0>>),
    mem_leak(1024, B),
    ok.

mem_leak(0, _) -> ok;
mem_leak(N, B) ->
    big_bin(B, <<23>>),
    {'EXIT',{badarg,_}} = (catch big_bin(B, bad)),
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
	(fun(Int) ->
		 true = <<Int:16/float>> =:= <<(float(Int)):16/float>>,
		 true = <<Int:32/float>> =:= <<(float(Int)):32/float>>,
		 true = <<Int:64/float>> =:= <<(float(Int)):64/float>>
	 end)(nonliteral(Int0)),
	true = <<Int0:16/float>> =:= <<(float(Int0)):16/float>>,
	true = <<Int0:32/float>> =:= <<(float(Int0)):32/float>>,
	true = <<Int0:64/float>> =:= <<(float(Int0)):64/float>>).

-define(COF32(Int0),
	(fun(Int) ->
		 true = <<Int:32/float>> =:= <<(float(Int)):32/float>>,
		 true = <<Int:64/float>> =:= <<(float(Int)):64/float>>
	 end)(nonliteral(Int0)),
	true = <<Int0:32/float>> =:= <<(float(Int0)):32/float>>,
	true = <<Int0:64/float>> =:= <<(float(Int0)):64/float>>).

-define(COF64(Int0),
	(fun(Int) ->
		 true = <<Int:64/float>> =:= <<(float(Int)):64/float>>
	 end)(nonliteral(Int0)),
	true = <<Int0:64/float>> =:= <<(float(Int0)):64/float>>).

nonliteral(X) -> X.
    
coerce_to_float(Config) when is_list(Config) ->
    ?COF(0),
    ?COF(-1),
    ?COF(1),
    ?COF(42),
    ?COF(255),
    ?COF(-255),
    ?COF(38474),
    ?COF(65504),
    ?COF(-65504),
    ?COF32(387498738948729893849444444443),
    ?COF32(-37489378937773899999999999999993),
    ?COF64(298748888888888888888888888883478264866528467367364766666666666666663),
    ?COF64(-367546729879999999999947826486652846736736476555566666663),
    ok.

bjorn(Config) when is_list(Config) ->
    error = bjorn_1(),
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

append_empty_is_same(Config) when is_list(Config) ->
    NonWritableBin = <<"123">>,
    true = erts_debug:same(NonWritableBin, append(NonWritableBin, <<>>)),
    WritableBin = <<(id(<<>>))/binary,0,1,2,3,4,5,6,7>>,
    true = erts_debug:same(WritableBin, append(WritableBin, <<>>)),
    ok.

append(A, B) ->
    <<A/binary, B/binary>>.

huge_float_field(Config) when is_list(Config) ->
    {'EXIT',{badarg,_}} = (catch <<0.0:9/float-unit:8>>),
    huge_float_check(catch <<0.0:67108865/float-unit:64>>),
    huge_float_check(catch <<0.0:((1 bsl 26)+1)/float-unit:64>>),
    huge_float_check(catch <<0.0:(id(67108865))/float-unit:64>>),
%%  huge_float_check(catch <<0.0:((1 bsl 60)+1)/float-unit:64>>),
    huge_float_check(catch <<3839739387439387383739387987347983:((1 bsl 26)+1)/float-unit:64>>),
%%  huge_float_check(catch <<3839739387439387383739387987347983:((1 bsl 60)+1)/float-unit:64>>),
    ok.

huge_float_check({'EXIT',{system_limit,_}}) -> ok;
huge_float_check({'EXIT',{badarg,_}}) -> ok.

system_limit(Config) when is_list(Config) ->
    WordSize = erlang:system_info(wordsize),
    BitsPerWord = WordSize * 8,
    {'EXIT',{system_limit,_}} =
	(catch <<0:(id(0)),42:(id(1 bsl BitsPerWord))>>),
    {'EXIT',{system_limit,_}} =
	(catch <<42:(id(1 bsl BitsPerWord)),0:(id(0))>>),
    {'EXIT',{system_limit,_}} =
	(catch <<(id(<<>>))/binary,0:(id(1 bsl 100))>>),

    %% Would fail to load.
    {'EXIT',{system_limit,_}} = (catch <<0:(1 bsl 67)>>),
    {'EXIT',{system_limit,_}} = (catch <<0:((1 bsl 64)+1)>>),

    case WordSize of
	4 ->
	    system_limit_32();
	8 ->
	    ok
    end.

system_limit_32() ->
    {'EXIT',{badarg,_}} = (catch <<42:(-1)>>),
    {'EXIT',{badarg,_}} = (catch <<42:(id(-1))>>),
    {'EXIT',{badarg,_}} = (catch <<42:(id(-389739873536870912))/unit:8>>),
    {'EXIT',{system_limit,_}} = (catch <<42:536870912/unit:8>>),
    {'EXIT',{system_limit,_}} = (catch <<42:(id(536870912))/unit:8>>),
    {'EXIT',{system_limit,_}} = (catch <<0:(id(8)),42:536870912/unit:8>>),
    {'EXIT',{system_limit,_}} = (catch <<0:(id(8)),42:(id(536870912))/unit:8>>),

    %% The size would be silently truncated, resulting in a crash.
    {'EXIT',{system_limit,_}} = (catch <<0:(1 bsl 35)>>),
    {'EXIT',{system_limit,_}} = (catch <<0:((1 bsl 32)+1)>>),

    %% Would fail to load.
    {'EXIT',{system_limit,_}} = (catch <<0:(1 bsl 43)>>),
    {'EXIT',{system_limit,_}} = (catch <<0:((1 bsl 40)+1)>>),
    ok.

badarg(Config) when is_list(Config) ->
    <<3:2>> = <<1:(id(1)),1:(id(1))>>,
    {'EXIT',{badarg,_}} = (catch <<0:(id(1)),0:(id(-1))>>),
    {'EXIT',{badarg,_}} = (catch <<0:(id(1)),0:(id(-(1 bsl 70)))>>),
    {'EXIT',{badarg,_}} = (catch <<0:(id(-(1 bsl 70))),0:(id(1))>>),
    {'EXIT',{badarg,_}} = (catch <<(id(<<>>))/binary,0:(id(-(1)))>>),
    ok.

copy_writable_binary(Config) when is_list(Config) ->
    [copy_writable_binary_1(I) || I <- lists:seq(0, 256)],
    ok.

copy_writable_binary_1(_) ->
    Bin0 = <<(id(<<>>))/binary,0,1,2,3,4,5,6,7>>,
    SubBin = make_sub_bin(Bin0),
    id(<<42,34,55,Bin0/binary>>),		%Make reallocation likelier.
    Pid = spawn(fun() ->
			      copy_writable_binary_holder(Bin0, SubBin)
		      end),
    Tab = ets:new(holder, []),
    ets:insert(Tab, {17,Bin0}),
    ets:insert(Tab, {42,SubBin}),
    id(<<Bin0/binary,0:(64*1024*8)>>),
    Pid ! self(),
    [{17,Bin0}] = ets:lookup(Tab, 17),
    [{42,Bin0}] = ets:lookup(Tab, 42),
    receive
	{Pid,Bin0,Bin0} -> ok;
	Other ->
	    ct:fail("Unexpected message: ~p", [Other])
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

%% Make sure that bit syntax expression with huge field size are
%% not constructed at compile time.

kostis(Config) when is_list(Config) ->
    case have_250_terabytes_of_ram() of
	true ->
	    Bin = <<0:800000000000>>,
	    EmbeddedBin = <<0,(<<0:99999999999>>)/bitstring,1>>,
	    Bin0 = list_to_binary([Bin,Bin,Bin,Bin,Bin]),
	    Bin1 = list_to_binary([Bin0,Bin0,Bin0,Bin0,Bin0,Bin0]),
	    Bin2 = list_to_binary([Bin1,Bin1]),
	    id({EmbeddedBin,Bin0,Bin1,Bin2});
	false ->
	    ok
    end.

%% I'm not even certain how much 250 TB really is...
%% but I'm sure I don't have it :-)

have_250_terabytes_of_ram() -> false.

%% Test that different ways of using bit syntax instructions
%% give the same result.

dynamic(Config) when is_list(Config) ->
    dynamic_1(fun dynamic_big/5),
    dynamic_1(fun dynamic_little/5),
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

    %% OTP-7085: Test a small number in a wide segment.
    Int2 = Int0 band 16#FFFFFF,
    Dynamic(Bef, N, Int2, Lpad, Rpad),
    Dynamic(Bef, N, -Int2, Lpad, Rpad),

    %% Test a bignum in a short segment.
    Int4 = ((Lpad bxor Rpad) bsl N) bor Int0,
    Dynamic(Bef, N, Int4, Lpad, Rpad),
    Dynamic(Bef, N, -Int4, Lpad, Rpad),

    dynamic_3(Bef, N-1, Data, Count+1).

dynamic_big(Bef, N, Int, Lpad, Rpad) ->
    NumBin = id(<<Int:N>>),
    MaskedInt = Int band ((1 bsl N) - 1),
    <<MaskedInt:N>> = NumBin,

    %% Construct the binary in two different ways.
    Bin = id(<<Lpad:Bef,NumBin/bitstring,Rpad:(128-Bef-N)>>),
    Bin = <<Lpad:Bef,Int:N,Rpad:(128-Bef-N)>>,

    %% Units are seldom used with integer segments even in our test
    %% suites, and I have never seen non-power-of-two units in
    %% production code.
    if
        Bef rem 8 =:= 0 ->
            Bin = <<Lpad:(Bef div 8)/unit:8,Int:N,Rpad:(128-Bef-N)>>;
        Bef rem 7 =:= 0 ->
            Bin = <<Lpad:(Bef div 7)/unit:7,Int:N,Rpad:(128-Bef-N)>>;
        (128-Bef-N) rem 5 =:= 0 ->
            Aft = (128 - Bef - N) div 5,
            Bin = <<Lpad:Bef,Int:N,Rpad:Aft/unit:5>>;
        true ->
            ok
    end,

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

    if
        Bef rem 8 =:= 0 ->
            Bin = <<Lpad:(Bef div 8)/little-unit:8,
                    Int:N/little,Rpad:(128-Bef-N)/little>>;
        Bef rem 9 =:= 0 ->
            Bin = <<Lpad:(Bef div 9)/little-unit:9,
                    Int:N/little,Rpad:(128-Bef-N)/little>>;
        (128-Bef-N) rem 17 =:= 0 ->
            Aft = (128 - Bef - N) div 17,
            Bin = <<Lpad:Bef/little,Int:N/little,Rpad:Aft/little-unit:17>>;
        true ->
            ok
    end,

    %% Further verify the result by matching.
    LpadMasked = Lpad band ((1 bsl Bef) - 1),
    RpadMasked = Rpad band ((1 bsl (128-Bef-N)) - 1),
    Rbits = (128-Bef-N),
    <<LpadMasked:Bef/little,MaskedInt:N/little,RpadMasked:Rbits/little>> = id(Bin),
    ok.

%% Test that the bs_add/5 instruction handles big numbers correctly.
bs_add(Config) when is_list(Config) ->
    Mod = list_to_atom(atom_to_list(?MODULE) ++ "_" ++
                           atom_to_list(?FUNCTION_NAME)),
    N = 2000,
    Code = [{module, Mod},
	    {exports, [{bs_add,2}]},
	    {labels, 2},

	    %% bs_add(Number, -SmallestBig) -> Number + N
	    {function, bs_add, 2, 2},
	    {label,1},
	    {func_info,{atom,Mod},{atom,bs_add},2},

	    {label,2},
	    {move,{x,0},{x,2}}] ++
	lists:duplicate(N-1, {bs_add,{f,0},[{x,2},{integer,1},1],{x,2}}) ++
	[{gc_bif,abs,{f,0},3,[{x,1}],{x,4}},	%Force GC, ignore result.
	 {gc_bif,'+',{f,0},3,[{x,2},{integer,1}],{x,0}}, %Safe result in {x,0}
	 return],

    %% Write assembly file and assemble it.
    PrivDir = proplists:get_value(priv_dir, Config),
    RootName = filename:join(PrivDir, atom_to_list(Mod)),
    AsmFile = RootName ++ ".S",
    {ok,Fd} = file:open(AsmFile, [write]),
    [io:format(Fd, "~p. \n", [T]) || T <- Code],
    ok = file:close(Fd),
    {ok,Mod} = compile:file(AsmFile, [from_asm,report,{outdir,PrivDir}]),
    LoadRc = code:load_abs(RootName),
    {module,_Module} = LoadRc,

    %% Find smallest positive bignum.
    SmallestBig = smallest_big(),
    io:format("~p\n", [SmallestBig]),
    DoTest = fun() ->
		     exit(Mod:bs_add(SmallestBig, -SmallestBig))
	     end,
    {Pid,Mref} = spawn_monitor(DoTest),
    receive
	{'DOWN',Mref,process,Pid,Res} -> ok
    end,

    case erlang:system_info(wordsize) of
        8 ->
            %% bignum-sized binaries must system_limit on 64-bit platforms
            {system_limit, _} = Res;
        4 ->
            Res = SmallestBig + N
    end,

    %% Clean up.
    ok = file:delete(AsmFile),
    ok = file:delete(code:which(Mod)),
    _ = code:delete(Mod),
    _ = code:purge(Mod),

    ok.


smallest_big() ->
    smallest_big_1(1 bsl 24).

smallest_big_1(N) ->
    case erts_debug:flat_size(N) of
	0 -> smallest_big_1(N+N);
	_  -> N
    end.

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
    Z = id(0),
    Small = id(42),
    Big = id(1 bsl 128),
    <<>> = <<Small:Z>>,
    <<>> = <<Small:0>>,
    <<>> = <<Big:Z>>,
    <<>> = <<Big:0>>,
    
    {'EXIT',{badarg,_}} = (catch <<not_a_number:0>>),
    {'EXIT',{badarg,_}} = (catch <<(id(not_a_number)):Z>>),
    {'EXIT',{badarg,_}} = (catch <<(id(not_a_number)):0>>),

    ok.

bad_append(_) ->
    do_bad_append(<<127:1>>, fun append_unit_3/1),
    do_bad_append(<<127:2>>, fun append_unit_3/1),
    do_bad_append(<<127:17>>, fun append_unit_3/1),

    do_bad_append(<<127:3>>, fun append_unit_4/1),
    do_bad_append(<<127:5>>, fun append_unit_4/1),
    do_bad_append(<<127:7>>, fun append_unit_4/1),
    do_bad_append(<<127:199>>, fun append_unit_4/1),

    do_bad_append(<<127:7>>, fun append_unit_8/1),
    do_bad_append(<<127:9>>, fun append_unit_8/1),

    do_bad_append(<<0:8>>, fun append_unit_16/1),
    do_bad_append(<<0:15>>, fun append_unit_16/1),
    do_bad_append(<<0:17>>, fun append_unit_16/1),
    ok.

do_bad_append(Bin0, Appender) ->
    {'EXIT',{badarg,_}} = (catch Appender(Bin0)),

    Bin1 = id(<<0:3,Bin0/bitstring>>),
    <<_:3,Bin2/bitstring>> = Bin1,
    {'EXIT',{badarg,_}} = (catch Appender(Bin2)),

    %% Create a writable binary.
    Empty = id(<<>>),
    Bin3 = <<Empty/bitstring,Bin0/bitstring>>,
    {'EXIT',{badarg,_}} = (catch Appender(Bin3)),
    ok.

append_unit_3(Bin) ->
    <<Bin/binary-unit:3,0:1>>.

append_unit_4(Bin) ->
    <<Bin/binary-unit:4,0:1>>.

append_unit_8(Bin) ->
    <<Bin/binary,0:1>>.

append_unit_16(Bin) ->
    <<Bin/binary-unit:16,0:1>>.

%% Test that the bs_append instruction will correctly check for
%% overflow by producing a binary whose total size would exceed the
%% maximum allowed size for a binary on a 32-bit computer.

bs_append_overflow(_Config) ->
    Memsize = memsize(),
    io:format("Memsize = ~w Bytes~n", [Memsize]),
    case erlang:system_info(wordsize) of
	8 ->
            %% Not possible to test on a 64-bit computer.
	    {skip, "64-bit architecture"};
        _ when Memsize < (2 bsl 30) ->
	    {skip, "Less than 2 GB of memory"};
	4 ->
            {'EXIT', {system_limit, _}} = (catch bs_append_overflow_signed()),
            erlang:garbage_collect(),
            {'EXIT', {system_limit, _}} = (catch bs_append_overflow_unsigned()),
            erlang:garbage_collect(),
	    ok
    end.

bs_append_overflow_signed() ->
    %% Produce a large binary that, if cast to signed int, would
    %% overflow into a negative number that fits a smallnum.
    Large = <<0:((1 bsl 30)-1)>>,
    <<Large/bits, Large/bits, Large/bits, Large/bits,
      Large/bits, Large/bits, Large/bits, Large/bits,
      Large/bits>>.

bs_append_overflow_unsigned() ->
    %% The following would succeed but would produce an incorrect result
    %% where B =:= C!
    A = <<0:((1 bsl 32)-8)>>,
    B = <<2, 3>>,
    C = <<A/binary,1,B/binary>>,
    true = byte_size(B) < byte_size(C).

bs_append_offheap(Config) when is_list(Config) ->
    %% test that erts_bs_private_append is reflected correctly in
    %%  process_info(Pid, binary()) report for off-heap binaries.
    {Pid, MRef} = erlang:spawn_monitor(fun bs_append_offheap_proc/0),
    receive
        {'DOWN', MRef, process, Pid, normal} ->
            ok;
        {'DOWN', MRef, process, Pid, {{badmatch,[{binary,[]}]}, _}} ->
            {fail, "missing binary in erts_bs_private_append"}
    end.

bs_append_offheap_proc() ->
    Self = self(),
    Len = 128,
    OffHeapBin = list_to_binary(lists:duplicate(Len, $b)),
    erlang:garbage_collect(Self),
    [{binary, [{_, Len, 1}]}] = erlang:process_info(Self, [binary]),
    Bin = <<OffHeapBin/binary, "a">>,
    erlang:garbage_collect(Self),
    %% expect a single binary (2 bytes longer than the original)
    [{binary, [{_, _, 1}]}] = erlang:process_info(Self, [binary]),
    Bin.

reductions(_Config) ->
    TwoMeg = <<0:(2_000*1024)/unit:8>>,
    reds_at_least(2000, fun() -> <<0:8,TwoMeg/binary>> end),
    reds_at_least(4000, fun() -> <<0:8,TwoMeg/binary,TwoMeg/binary>> end),
    reds_at_least(1000, fun() -> <<0:8,TwoMeg:(1000*1024)/binary>> end),

    %% Here we expect about 500 reductions in the bs_append
    %% instruction for setting up a writable binary and about 2000
    %% reductions in the bs_put_binary instruction for copying the
    %% binary data.
    reds_at_least(2500, fun() -> <<TwoMeg/binary,TwoMeg:(2000*1024)/binary>> end),
    ok.

reds_at_least(N, Fun) ->
    receive after 1 -> ok end,
    {reductions,Red0} = process_info(self(), reductions),
    _ = Fun(),
    {reductions,Red1} = process_info(self(), reductions),
    Diff = Red1 - Red0,
    io:format("Expected at least ~p; got ~p\n", [N,Diff]),
    if
        Diff >= N ->
            ok;
        Diff ->
            ct:fail({expected,N,got,Diff})
    end.
memsize() ->
    application:ensure_all_started(os_mon),
    case proplists:get_value(available_memory, memsup:get_system_memory_data()) of
        undefined ->
            {Tot,_Used,_}  = memsup:get_memory_data(),
            Tot;
        Available ->
            Available
    end.

-define(FP16(EncodedInt, Float),
        (fun(NlInt, NlFloat) ->
                 {0, true} = {0, <<NlInt:16>> =:= <<NlFloat:16/float>>},
                 {1, true} = {1, <<(NlInt+16#8000):16>> =:= <<-NlFloat:16/float>>},
                 {2, true} = {2, <<NlInt:16/little>> =:= <<NlFloat:16/float-little>>},
                 {3, true} = {3, <<(NlInt+16#8000):16/little>> =:= <<-NlFloat:16/float-little>>},
                 {4, true} = {4, <<NlInt:16/native>> =:= <<NlFloat:16/float-native>>},
                 {5, true} = {5, <<(NlInt+16#8000):16/native>> =:= <<-NlFloat:16/float-native>>}
         end)(nonliteral(EncodedInt), nonliteral(Float)),
        {a, true} = {a, <<EncodedInt:16>> =:= <<Float:16/float>>},
        {b, true} = {b, <<(EncodedInt+16#8000):16>> =:= <<-Float:16/float>>},
        {c, true} = {c, <<EncodedInt:16/little>> =:= <<Float:16/float-little>>},
        {d, true} = {d, <<(EncodedInt+16#8000):16/little>> =:= <<-Float:16/float-little>>},
        {e, true} = {e, <<EncodedInt:16/native>> =:= <<Float:16/float-native>>},
        {f, true} = {f, <<(EncodedInt+16#8000):16/native>> =:= <<-Float:16/float-native>>}).

fp16(_Config) ->
    %% smallest positive subnormal number
    ?FP16(16#0001, 0.000000059604645),
    %% largest positive subnormal number
    ?FP16(16#03ff, 0.000060975552),
    %% smallest positive normal number
    ?FP16(16#0400, 0.00006103515625),
    %% largest normal number
    ?FP16(16#7bff, 65504),
    ?FP16(16#7bff, 65504.0),
    %% largest number less than one
    ?FP16(16#3bff, 0.99951172),
    %% zero
    ?FP16(16#0000, 0.0),
    %% one
    ?FP16(16#3c00, 1),
    ?FP16(16#3c00, 1.0),
    %% smallest number larger than one
    ?FP16(16#3c01, 1.00097656),
    %% rounding of 1/3 to nearest
    ?FP16(16#3555, 0.33325195),
    %% others
    ?FP16(16#4000, 2),
    ?FP16(16#4000, 2.0),
    ok.

zero_init(_Config) ->
    <<LPad:64/bits,RPad:64/bits>> = id(erlang:md5([42])),
    Sizes = [511,512,513, 767,768,769, 1023,1024,1025,
             16#7fff,16#ffff,16#10000] ++ lists:seq(0, 257),
    _ = [do_zero_init(Size, LPad, RPad) || Size <- Sizes],
    ok.

do_zero_init(Size, LPad, RPad) ->
    try
        do_zero_init_1(Size, LPad, RPad)
    catch
        C:R:Stk ->
            io:format("Size = ~p, LPad = ~p, RPad = ~p\n", [Size, LPad, RPad]),
            erlang:raise(C, R, Stk)
    end.

do_zero_init_1(Size, LPad, RPad) ->
    Zeroes = id(<<0:Size>>),
    <<0:Size>> = Zeroes,
    Bin = id(<<LPad:64/bits, Zeroes/bits, RPad:64/bits>>),
    Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>),
    Bin = id(<<LPad:(id(64))/bits, 0:Size, RPad:64/bits>>),

    if
        Size rem 11 =:= 0 ->
            Bin = id(<<LPad:64/bits, 0:(Size div 11)/unit:11, RPad:64/bits>>);
        true ->
            ok
    end,

    case Size of
        0 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        1 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        2 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        3 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        4 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        5 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        6 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        7 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        8 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        9 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        10 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        11 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        12 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        13 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        14 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        15 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        16 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);

        31 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        32 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        33 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);

        47 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        48 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        49 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);

        63 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        64 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        65 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);

        79 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        80 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        81 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);

        90 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        91 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        92 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        93 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        94 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        95 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        96 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        97 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        98 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        99 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        100 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        101 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        102 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        103 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        104 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        105 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        106 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        107 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        108 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        109 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);

        127 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        128 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        129 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        130 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        131 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        132 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        133 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        134 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        135 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        136 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        137 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        138 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        139 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        140 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        141 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        142 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        143 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        144 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        145 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);

        159 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        160 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        161 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);

        191 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        192 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        193 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);

        255 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        256 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        257 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);

        511 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        512 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        513 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);

        767 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        768 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        769 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);

        1023 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        1024 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        1025 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);

        16#7fff ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        16#ffff ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        16#10000 ->
            Bin = id(<<LPad:64/bits, 0:Size, RPad:64/bits>>);
        _ ->
            ok
    end.


-define(ERROR_INFO(Expr),
        fun() ->
                try Expr of
                    _ ->
                        error(should_fail)
                catch
                    error:Reason:Stk ->
                        error_info_verify(Reason, Stk, ??Expr, #{})
                end
        end()).

-define(ERROR_INFO(Expr, Overrides),
        fun() ->
                try Expr of
                    _ ->
                        error(should_fail)
                catch
                    error:Reason:Stk ->
                        error_info_verify(Reason, Stk, ??Expr, Overrides)
                end
        end()).

error_info(_Config) ->
    case ?MODULE of
        bs_construct_r24_SUITE ->
            %% Error information is not implemented for old bit syntax
            %% instructions.
            ok;
        _ ->
            error_info()
    end.

error_info() ->
    Atom = id(some_atom),
    NegSize = id(-1),
    HugeNegSize = id(-1 bsl 64),
    Binary = id(<<"abc">>),
    HugeBig = id(1 bsl 1500),
    LongList = lists:seq(1, 100),
    BadBinary = id(ok),
    BadSize = case Atom of
                  a -> 0;
                  _ -> bad_size
              end,

    {badarg, {1,binary,type,Atom}, _} = ?ERROR_INFO(<<Atom/binary, Binary/binary>>),
    {badarg, {2,binary,type,Atom}, _} = ?ERROR_INFO(<<Binary/binary, Atom/binary>>),
    {badarg, {3,binary,type,Atom}, _} = ?ERROR_INFO(<<1:32, Binary/binary, Atom/binary>>),
    {badarg, {4,binary,type,Atom}, _} = ?ERROR_INFO(<<1:32, "xyz", Binary/binary, Atom/binary>>),

    {badarg, {1,integer,type,Atom}, _} = ?ERROR_INFO(<<Atom:32>>),
    {badarg, {1,integer,type,Atom}, _} = ?ERROR_INFO(<<Atom:(id(32))>>),
    {badarg, {1,integer,type,LongList}, _} = ?ERROR_INFO(<<LongList:32>>),
    {badarg, {1,integer,size,Atom}, _} = ?ERROR_INFO(<<42:Atom>>),
    {badarg, {1,integer,type,Atom}, _} = ?ERROR_INFO(<<Atom:32>>),
    {badarg, {1,integer,size,NegSize}, _} = ?ERROR_INFO(<<42:NegSize>>),
    {badarg, {1,integer,size,HugeNegSize}, _} = ?ERROR_INFO(<<42:HugeNegSize>>),
    {system_limit, {1,integer,size,1 bsl 58}, _} = ?ERROR_INFO(<<42:(1 bsl 58)/unit:255>>),
    {system_limit, {1,integer,size,1 bsl 60}, _} = ?ERROR_INFO(<<42:(1 bsl 60)/unit:8>>),
    {system_limit, {1,integer,size,1 bsl 64}, _} = ?ERROR_INFO(<<42:(1 bsl 64)>>),

    {badarg, {1,binary,type,Atom}, _} = ?ERROR_INFO(<<Atom:10/binary>>),
    {badarg, {1,binary,type,Atom}, _} = ?ERROR_INFO(<<Atom:(id(10))/binary>>),
    {badarg, {1,binary,size,Atom}, _} = ?ERROR_INFO(<<Binary:Atom/binary>>),
    {badarg, {1,binary,size,NegSize}, _} = ?ERROR_INFO(<<Binary:NegSize/binary>>),
    {badarg, {1,binary,size,HugeNegSize}, _} = ?ERROR_INFO(<<Binary:HugeNegSize/binary>>),
    {badarg, {1,binary,size,BadSize}, _} = ?ERROR_INFO(<<Binary:BadSize/binary>>),
    {badarg, {1,binary,size,BadSize}, _} = ?ERROR_INFO(<<BadBinary:BadSize/binary>>),
    {badarg, {1,binary,short,Binary}, _} = ?ERROR_INFO(<<Binary:10/binary>>),
    {badarg, {1,binary,short,Binary}, _} = ?ERROR_INFO(<<Binary:(id(10))/binary>>),
    {badarg, {1,binary,type,Atom}, _} = ?ERROR_INFO(<<Atom/binary>>),
    {badarg, {1,binary,unit,<<1:1>>}, _} = ?ERROR_INFO(<<(id(<<1:1>>))/binary>>),
    {badarg, {1,binary,unit,<<0:1111>>}, _} = ?ERROR_INFO(<<(id(<<0:1111>>))/binary>>),
    {badarg, {2,binary,unit,<<1:1>>}, _} = ?ERROR_INFO(<<0, (id(<<1:1>>))/binary>>),
    {badarg, {2,binary,unit,<<0:1111>>}, _} = ?ERROR_INFO(<<0, (id(<<0:1111>>))/binary>>),
    {system_limit, {1,binary,size,1 bsl 64}, _} = ?ERROR_INFO(<<Binary:(1 bsl 64)/binary>>),
    {system_limit, {1,binary,size,1 bsl 64}, _} = ?ERROR_INFO(<<Binary:(id(1 bsl 64))/binary>>),

    {badarg, {1,float,type,Atom}, _} = ?ERROR_INFO(<<Atom:64/float>>),
    {badarg, {1,float,size,Atom}, _} = ?ERROR_INFO(<<Atom:Atom/float>>),
    {badarg, {1,float,size,NegSize}, _} = ?ERROR_INFO(<<42.0:NegSize/float>>),
    {badarg, {1,float,size,HugeNegSize}, _} = ?ERROR_INFO(<<42.0:HugeNegSize/float>>),
    {badarg, {1,float,invalid,1}, _} = ?ERROR_INFO(<<42.0:(id(1))/float>>),
    {badarg, {1,float,no_float,HugeBig}, _} = ?ERROR_INFO(<<HugeBig:(id(64))/float>>),
    {badarg, {1,float,no_float,HugeBig}, _} = ?ERROR_INFO(<<HugeBig:64/float>>),
    {system_limit, {1,float,size,1 bsl 64}, _} = ?ERROR_INFO(<<42.0:(id(1 bsl 64))/float>>),

    {badarg, {1,utf8,type,Atom}, _} = ?ERROR_INFO(<<Atom/utf8>>),
    {badarg, {1,utf16,type,Atom}, _} = ?ERROR_INFO(<<Atom/utf16>>),
    {badarg, {1,utf32,type,Atom}, _} = ?ERROR_INFO(<<Atom/utf32>>),

    Bin = id(<<>>),
    Float = id(42.0),
    MaxSmall = (1 bsl 59) - 1,                  %Max small for 64-bit architectures.

    %% Attempt constructing a binary with total size 1^64 + 32.

    {system_limit, {1,integer,size,MaxSmall}, _} = ?ERROR_INFO(<<0:(MaxSmall)/unit:32,0:64>>),
    {system_limit, {1,integer,size,MaxSmall}, _} = ?ERROR_INFO(<<0:(MaxSmall)/unit:32,(id(0)):64>>),
    {system_limit, {1,integer,size,MaxSmall}, _} = ?ERROR_INFO(<<0:(id(MaxSmall))/unit:32,0:64>>),
    {system_limit, {1,integer,size,MaxSmall}, _} = ?ERROR_INFO(<<0:(id(MaxSmall))/unit:32,(id(0)):64>>),

    {system_limit, {1,binary,size,MaxSmall}, _} = ?ERROR_INFO(<<Bin:(MaxSmall)/binary-unit:32,0:64>>),
    {system_limit, {1,binary,size,MaxSmall}, _} = ?ERROR_INFO(<<Bin:(MaxSmall)/binary-unit:32,(id(0)):64>>),
    {system_limit, {1,binary,size,MaxSmall}, _} = ?ERROR_INFO(<<Bin:(id(MaxSmall))/binary-unit:32,0:64>>),
    {system_limit, {1,binary,size,MaxSmall}, _} = ?ERROR_INFO(<<Bin:(id(MaxSmall))/binary-unit:32,(id(0)):64>>),

    {system_limit, {1,float,size,MaxSmall}, _} = ?ERROR_INFO(<<Float:(MaxSmall)/float-unit:32,0:64>>),
    {system_limit, {1,float,size,MaxSmall}, _} = ?ERROR_INFO(<<Float:(MaxSmall)/float-unit:32,(id(0)):64>>),
    {system_limit, {1,float,size,MaxSmall}, _} = ?ERROR_INFO(<<Float:(id(MaxSmall))/float-unit:32,0:64>>),
    {system_limit, {1,float,size,MaxSmall}, _} = ?ERROR_INFO(<<Float:(id(MaxSmall))/float-unit:32,(id(0)):64>>),

    %% Test a size exceeding 1^64, where the sign bit (bit 63) is not set.
    0 = (((MaxSmall) * 33) bsr 63) band 1, %Assertion: The sign bit is not set.

    {system_limit, {1,integer,size,MaxSmall}, _} = ?ERROR_INFO(<<0:(MaxSmall)/unit:33>>),
    {system_limit, {1,integer,size,MaxSmall}, _} = ?ERROR_INFO(<<0:(MaxSmall)/unit:33>>),
    {system_limit, {1,integer,size,MaxSmall}, _} = ?ERROR_INFO(<<0:(id(MaxSmall))/unit:33>>),
    {system_limit, {1,integer,size,MaxSmall}, _} = ?ERROR_INFO(<<0:(id(MaxSmall))/unit:33>>),

    {system_limit, {1,binary,size,MaxSmall}, _} = ?ERROR_INFO(<<Bin:(MaxSmall)/binary-unit:33>>),
    {system_limit, {1,binary,size,MaxSmall}, _} = ?ERROR_INFO(<<Bin:(MaxSmall)/binary-unit:33>>),
    {system_limit, {1,binary,size,MaxSmall}, _} = ?ERROR_INFO(<<Bin:(id(MaxSmall))/binary-unit:33>>),
    {system_limit, {1,binary,size,MaxSmall}, _} = ?ERROR_INFO(<<Bin:(id(MaxSmall))/binary-unit:33>>),

    {system_limit, {1,float,size,MaxSmall}, _} = ?ERROR_INFO(<<Float:(MaxSmall)/float-unit:33>>),
    {system_limit, {1,float,size,MaxSmall}, _} = ?ERROR_INFO(<<Float:(MaxSmall)/float-unit:33>>),
    {system_limit, {1,float,size,MaxSmall}, _} = ?ERROR_INFO(<<Float:(id(MaxSmall))/float-unit:33>>),
    {system_limit, {1,float,size,MaxSmall}, _} = ?ERROR_INFO(<<Float:(id(MaxSmall))/float-unit:33>>),

    %% error messages with options
    PP = fun(Term) -> <<"'", (erlang:atom_to_binary(Term))/binary, "'">> end,

    {_, _, <<"segment 1 of type 'float': expected a float or an integer but got: some_atom">>} =
        ?ERROR_INFO(<<Atom:64/float>>, #{}),

    {_, _, <<"segment 2 of type 'float': expected a float or an integer but got: some_atom">>} =
        ?ERROR_INFO(<<Atom:64/float>>, #{override_segment_position => 2}),

    {_, _, <<"segment 1 of type 'float': expected a float or an integer but got: 'some_atom'">>} =
        ?ERROR_INFO(<<Atom:64/float>>, #{pretty_printer => PP}),

    ok.

error_info_verify(Reason, Stk0, Expr, Overrides) ->
    [{?MODULE, Fun, Arity, Info0}|Rest] = Stk0,
    {value, {error_info, ErrorInfo}, Info1} = lists:keytake(error_info, 1, Info0),
    #{cause := Cause, module := Module, function := Function} = ErrorInfo,
    Info2 = maps:merge(ErrorInfo, Overrides),
    Stk1 = [{?MODULE, Fun, Arity, Info1 ++ [{error_info, Info2}]} | Rest],
    Result = Module:Function(Reason, Stk1),
    #{general := String} = Result,
    true = is_binary(String),
    io:format("~ts: ~ts\n", [Expr,String]),
    {Reason, Cause, String}.

little(_Config) ->
    _ = rand:uniform(),				%Seed generator
    io:format("Seed: ~p", [rand:export_seed()]),

    RandBytes = rand:bytes(10),
    _ = [do_little(RandBytes, N) || N <- lists:seq(0, 10*8)],

    ok.

do_little(Bin0, N) ->
    <<Bin1:N/bits,Bin2/bits>> = Bin0,
    Bin2Size = bit_size(Bin2),

    <<I1:N/little-integer>> = Bin1,
    <<I2:Bin2Size/little-integer>> = Bin2,

    Bin1 = <<I1:N/little-integer>>,
    Bin1 = do_little_1(N, id(I1)),

    Bin2 = <<I2:Bin2Size/little-integer>>,
    Bin2 = do_little_1(Bin2Size, id(I2)),

    ok.

do_little_1(0, I) -> <<I:0/little-integer>>;
do_little_1(1, I) -> <<I:1/little-integer>>;
do_little_1(2, I) -> <<I:2/little-integer>>;
do_little_1(3, I) -> <<I:3/little-integer>>;
do_little_1(4, I) -> <<I:4/little-integer>>;
do_little_1(5, I) -> <<I:5/little-integer>>;
do_little_1(6, I) -> <<I:6/little-integer>>;
do_little_1(7, I) -> <<I:7/little-integer>>;
do_little_1(8, I) -> <<I:8/little-integer>>;
do_little_1(9, I) -> <<I:9/little-integer>>;
do_little_1(10, I) -> <<I:10/little-integer>>;
do_little_1(11, I) -> <<I:11/little-integer>>;
do_little_1(12, I) -> <<I:12/little-integer>>;
do_little_1(13, I) -> <<I:13/little-integer>>;
do_little_1(14, I) -> <<I:14/little-integer>>;
do_little_1(15, I) -> <<I:15/little-integer>>;
do_little_1(16, I) -> <<I:16/little-integer>>;
do_little_1(17, I) -> <<I:17/little-integer>>;
do_little_1(18, I) -> <<I:18/little-integer>>;
do_little_1(19, I) -> <<I:19/little-integer>>;
do_little_1(20, I) -> <<I:20/little-integer>>;
do_little_1(21, I) -> <<I:21/little-integer>>;
do_little_1(22, I) -> <<I:22/little-integer>>;
do_little_1(23, I) -> <<I:23/little-integer>>;
do_little_1(24, I) -> <<I:24/little-integer>>;
do_little_1(25, I) -> <<I:25/little-integer>>;
do_little_1(26, I) -> <<I:26/little-integer>>;
do_little_1(27, I) -> <<I:27/little-integer>>;
do_little_1(28, I) -> <<I:28/little-integer>>;
do_little_1(29, I) -> <<I:29/little-integer>>;
do_little_1(30, I) -> <<I:30/little-integer>>;
do_little_1(31, I) -> <<I:31/little-integer>>;
do_little_1(32, I) -> <<I:32/little-integer>>;
do_little_1(33, I) -> <<I:33/little-integer>>;
do_little_1(34, I) -> <<I:34/little-integer>>;
do_little_1(35, I) -> <<I:35/little-integer>>;
do_little_1(36, I) -> <<I:36/little-integer>>;
do_little_1(37, I) -> <<I:37/little-integer>>;
do_little_1(38, I) -> <<I:38/little-integer>>;
do_little_1(39, I) -> <<I:39/little-integer>>;
do_little_1(40, I) -> <<I:40/little-integer>>;
do_little_1(41, I) -> <<I:41/little-integer>>;
do_little_1(42, I) -> <<I:42/little-integer>>;
do_little_1(43, I) -> <<I:43/little-integer>>;
do_little_1(44, I) -> <<I:44/little-integer>>;
do_little_1(45, I) -> <<I:45/little-integer>>;
do_little_1(46, I) -> <<I:46/little-integer>>;
do_little_1(47, I) -> <<I:47/little-integer>>;
do_little_1(48, I) -> <<I:48/little-integer>>;
do_little_1(49, I) -> <<I:49/little-integer>>;
do_little_1(50, I) -> <<I:50/little-integer>>;
do_little_1(51, I) -> <<I:51/little-integer>>;
do_little_1(52, I) -> <<I:52/little-integer>>;
do_little_1(53, I) -> <<I:53/little-integer>>;
do_little_1(54, I) -> <<I:54/little-integer>>;
do_little_1(55, I) -> <<I:55/little-integer>>;
do_little_1(56, I) -> <<I:56/little-integer>>;
do_little_1(57, I) -> <<I:57/little-integer>>;
do_little_1(58, I) -> <<I:58/little-integer>>;
do_little_1(59, I) -> <<I:59/little-integer>>;
do_little_1(60, I) -> <<I:60/little-integer>>;
do_little_1(61, I) -> <<I:61/little-integer>>;
do_little_1(62, I) -> <<I:62/little-integer>>;
do_little_1(63, I) -> <<I:63/little-integer>>;
do_little_1(64, I) -> <<I:64/little-integer>>;
do_little_1(65, I) -> <<I:65/little-integer>>;
do_little_1(66, I) -> <<I:66/little-integer>>;
do_little_1(67, I) -> <<I:67/little-integer>>;
do_little_1(68, I) -> <<I:68/little-integer>>;
do_little_1(69, I) -> <<I:69/little-integer>>;
do_little_1(70, I) -> <<I:70/little-integer>>;
do_little_1(71, I) -> <<I:71/little-integer>>;
do_little_1(72, I) -> <<I:72/little-integer>>;
do_little_1(73, I) -> <<I:73/little-integer>>;
do_little_1(74, I) -> <<I:74/little-integer>>;
do_little_1(75, I) -> <<I:75/little-integer>>;
do_little_1(76, I) -> <<I:76/little-integer>>;
do_little_1(77, I) -> <<I:77/little-integer>>;
do_little_1(78, I) -> <<I:78/little-integer>>;
do_little_1(79, I) -> <<I:79/little-integer>>;
do_little_1(80, I) -> <<I:80/little-integer>>;
do_little_1(81, I) -> <<I:81/little-integer>>;
do_little_1(82, I) -> <<I:82/little-integer>>;
do_little_1(83, I) -> <<I:83/little-integer>>;
do_little_1(84, I) -> <<I:84/little-integer>>;
do_little_1(85, I) -> <<I:85/little-integer>>;
do_little_1(86, I) -> <<I:86/little-integer>>;
do_little_1(87, I) -> <<I:87/little-integer>>;
do_little_1(88, I) -> <<I:88/little-integer>>;
do_little_1(89, I) -> <<I:89/little-integer>>;
do_little_1(90, I) -> <<I:90/little-integer>>;
do_little_1(91, I) -> <<I:91/little-integer>>;
do_little_1(92, I) -> <<I:92/little-integer>>;
do_little_1(93, I) -> <<I:93/little-integer>>;
do_little_1(94, I) -> <<I:94/little-integer>>;
do_little_1(95, I) -> <<I:95/little-integer>>;
do_little_1(96, I) -> <<I:96/little-integer>>;
do_little_1(97, I) -> <<I:97/little-integer>>;
do_little_1(98, I) -> <<I:98/little-integer>>;
do_little_1(99, I) -> <<I:99/little-integer>>;
do_little_1(100, I) -> <<I:100/little-integer>>;
do_little_1(101, I) -> <<I:101/little-integer>>;
do_little_1(102, I) -> <<I:102/little-integer>>;
do_little_1(103, I) -> <<I:103/little-integer>>;
do_little_1(104, I) -> <<I:104/little-integer>>;
do_little_1(105, I) -> <<I:105/little-integer>>;
do_little_1(106, I) -> <<I:106/little-integer>>;
do_little_1(107, I) -> <<I:107/little-integer>>;
do_little_1(108, I) -> <<I:108/little-integer>>;
do_little_1(109, I) -> <<I:109/little-integer>>;
do_little_1(110, I) -> <<I:110/little-integer>>;
do_little_1(111, I) -> <<I:111/little-integer>>;
do_little_1(112, I) -> <<I:112/little-integer>>;
do_little_1(113, I) -> <<I:113/little-integer>>;
do_little_1(114, I) -> <<I:114/little-integer>>;
do_little_1(115, I) -> <<I:115/little-integer>>;
do_little_1(116, I) -> <<I:116/little-integer>>;
do_little_1(117, I) -> <<I:117/little-integer>>;
do_little_1(118, I) -> <<I:118/little-integer>>;
do_little_1(119, I) -> <<I:119/little-integer>>;
do_little_1(120, I) -> <<I:120/little-integer>>;
do_little_1(121, I) -> <<I:121/little-integer>>;
do_little_1(122, I) -> <<I:122/little-integer>>;
do_little_1(123, I) -> <<I:123/little-integer>>;
do_little_1(124, I) -> <<I:124/little-integer>>;
do_little_1(125, I) -> <<I:125/little-integer>>;
do_little_1(126, I) -> <<I:126/little-integer>>;
do_little_1(127, I) -> <<I:127/little-integer>>;
do_little_1(128, I) -> <<I:128/little-integer>>.

%% GH-7469: The unit of variable-sized segments wasn't checked properly,
%% resulting in the creation of heap binaries for non-binary bitstrings.
heap_binary_unit(_Config) ->
    {ok, 14524} = heap_binary_unit_1(id(<<184,188,2,66,172,19,0,3>>)),
    ok.

heap_binary_unit_1(<<2:2/integer,Rest:62/bitstring>>) ->
    heap_binary_unit_2(<<2:2/integer>>, Rest).

heap_binary_unit_2(Variant, Rest) ->
    VariantSize = bit_size(Variant),
    ClockHiSize = 8 - VariantSize,
    ClockSize = 8 + ClockHiSize,
    case Rest of
        <<ClockHi:ClockHiSize/bitstring,
          ClockLo:8/bitstring,
          _:48/bitstring>> ->
            case
                <<ClockHi:ClockHiSize/bitstring,
                  ClockLo:8/bitstring>>
            of
                <<Clock:ClockSize/integer-unsigned>> ->
                    {ok, Clock};
                Bin1 ->
                    {error1, Bin1}
            end;
        Bin2 ->
            {error2, Bin2}
    end.

otp_24_code_gh_8238(Config) ->
    case ?MODULE of
        bs_construct_SUITE ->
            %% GH-8238. Code compiled with Erlang/OTP 24 would crash
            %% when run on OTP-26.2.3.
            DataDir = proplists:get_value(data_dir, Config),
            Asm = filename:join(DataDir, atom_to_list(?FUNCTION_NAME) ++ ".S"),
            {ok,Mod,Beam} = compile:file(Asm, [binary,from_asm,report]),
            {module,Mod} = code:load_binary(Mod, "", Beam),
            Mod:Mod(),
            ok;
        _ ->
            {skip,"Enough to run once"}
    end.

%%%
%%% Common utilities.
%%%

id(I) -> I.
