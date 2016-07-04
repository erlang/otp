%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%% Purpose : Common utilities used by several optimization passes.
%% 

-module(bs_construct_SUITE).

-export([all/0, suite/0,
         init_per_suite/1, end_per_suite/1,
	 test1/1, test2/1, test3/1, test4/1, test5/1, testf/1,
	 not_used/1, in_guard/1,
	 mem_leak/1, coerce_to_float/1, bjorn/1,
	 huge_float_field/1, huge_binary/1, system_limit/1, badarg/1,
	 copy_writable_binary/1, kostis/1, dynamic/1, bs_add/1,
	 otp_7422/1, zero_width/1, bad_append/1, bs_add_overflow/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {seconds, 10}}].

all() -> 
    [test1, test2, test3, test4, test5, testf, not_used,
     in_guard, mem_leak, coerce_to_float, bjorn,
     huge_float_field, huge_binary, system_limit, badarg,
     copy_writable_binary, kostis, dynamic, bs_add, otp_7422, zero_width,
     bad_append, bs_add_overflow].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    application:stop(os_mon).

big(1) ->
    57285702734876389752897683.

i(X) -> X.

r(L) ->
    lists:reverse(L).

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
	    ct:fail(comp)
    end,
    if
	E_bin == Bin ->
	    ok;
	true ->
	    io:format("ERROR: Interpreted: ~p. Expected ~p. Got ~p.~n",
		      [Str, Bytes, binary_to_list(E_bin)]),
	    ct:fail(comp)
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
    I_13 = i(13),
    I_big1 = big(1),
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
    ?COF(387498738948729893849444444443),
    ?COF(-37489378937773899999999999999993),
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

huge_binary(Config) when is_list(Config) ->
    ct:timetrap({seconds, 60}),
    16777216 = size(<<0:(id(1 bsl 26)),(-1):(id(1 bsl 26))>>),
    garbage_collect(),
    {Shift,Return} = case free_mem() of
			 undefined ->
			     %% This test has to be inlined inside the case to
			     %% use a literal Shift
			     garbage_collect(),
			     id(<<0:((1 bsl 32)-1)>>),
			     {32,ok};
			 Mb when Mb > 600 ->
			     garbage_collect(),
			     id(<<0:((1 bsl 32)-1)>>),
			     {32,ok};
			 Mb when Mb > 300 ->
			     garbage_collect(),
			     id(<<0:((1 bsl 31)-1)>>),
			     {31,"Limit huge binaries to 256 Mb"};
			 _ ->
			     garbage_collect(),
			     id(<<0:((1 bsl 30)-1)>>),
			     {30,"Limit huge binary to 128 Mb"}
		     end,
    garbage_collect(),
    id(<<0:((1 bsl Shift)-1)>>),
    garbage_collect(),
    id(<<0:(id((1 bsl Shift)-1))>>),
    garbage_collect(),
    case Return of
	ok -> ok;	     
	Comment -> {comment, Comment}
    end.

free_mem() ->
    {ok,Apps} = application:ensure_all_started(os_mon),
    Mem = memsup:get_system_memory_data(),
    [ok = application:stop(App)||App <- Apps],
    case proplists:get_value(free_memory,Mem) of
        undefined -> undefined;
        Val -> Val div 1024
    end.

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
    {'EXIT',{badarg,_}} = (catch <<0:(id(1 bsl 100)),0:(id(-1))>>),
    {'EXIT',{badarg,_}} = (catch <<0:(id(1 bsl 100)),0:(id(-(1 bsl 70)))>>),
    {'EXIT',{badarg,_}} = (catch <<0:(id(-(1 bsl 70))),0:(id(1 bsl 100))>>),
    {'EXIT',{badarg,_}} = (catch <<(id(<<>>))/binary,0:(id(-(1 bsl 100)))>>),
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

%% Test that the bs_add/5 instruction handles big numbers correctly.
bs_add(Config) when is_list(Config) ->
    Mod = bs_construct_bs_add,
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
    Expected = SmallestBig + N,
    DoTest = fun() ->
		     exit(Mod:bs_add(SmallestBig, -SmallestBig))
	     end,
    {Pid,Mref} = spawn_monitor(DoTest),
    receive
	{'DOWN',Mref,process,Pid,Res} -> ok
    end,
    Expected = Res,

    %% Clean up.
    ok = file:delete(AsmFile),
    ok = file:delete(code:which(Mod)),
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

%% Produce a large result of bs_add that, if cast to signed int, would overflow
%% into a negative number that fits a smallnum.
bs_add_overflow(_Config) ->
    Memsize = memsize(),
    io:format("Memsize = ~w Bytes~n", [Memsize]),
    case erlang:system_info(wordsize) of
	8 ->
	    {skip, "64-bit architecture"};
        _ when Memsize < (2 bsl 30) ->
	    {skip, "Less then 2 GB of memory"};
	4 ->
	    Large = <<0:((1 bsl 30)-1)>>,
	    {'EXIT',{system_limit,_}} =
		(catch <<Large/bits, Large/bits, Large/bits, Large/bits,
                         Large/bits, Large/bits, Large/bits, Large/bits,
                         Large/bits>>),
	    ok
    end.

id(I) -> I.

memsize() ->
    application:ensure_all_started(os_mon),
    {Tot,_Used,_}  = memsup:get_memory_data(),
    Tot.
