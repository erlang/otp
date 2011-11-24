%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2011. All Rights Reserved.
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

%%% While similar to bs_construct_SUITE in the emulator test suite,
%%% this module is more corncerned with testing sizes than the contents
%%% of binaries.

-module(bs_construct_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 two/1,test1/1,fail/1,float_bin/1,in_guard/1,in_catch/1,
	 nasty_literals/1,coerce_to_float/1,side_effect/1,
	 opt/1,otp_7556/1,float_arith/1,otp_8054/1]).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
    [two, test1, fail, float_bin, in_guard, in_catch,
     nasty_literals, side_effect, opt, otp_7556, float_arith,
     otp_8054].

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


init_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Dog = test_server:timetrap(?t:minutes(1)),
    [{watchdog,Dog}|Config].

end_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

two(Config) when is_list(Config) ->
    <<0,1,2,3,4,6,7,8,9>> = two_1([0], [<<1,2,3,4>>,<<6,7,8,9>>]),
    ok.

two_1(P, L) ->
    list_to_binary([P|L]).


big(1) ->
    57285702734876389752897683.

i(X) -> X.

id(I) -> I.

-define(T(B, L), {B, ??B, L}).
-define(N(B), {B, ??B, unknown}).

-define(FAIL(Expr), ?line {'EXIT',{badarg,_}} = (catch Expr)).

l(I_13, I_big1, I_16, Bin) ->
    [
     ?T(<<I_13:0>>,
	[]),
     ?T(<<-43>>,
	[256-43]),
     ?T(<<4:4,7:4>>,
	[4*16+7]),
     ?T(<<45:I_16/little>>,
        [45,0]),
     ?T(<<777:16/little>>,
	[9,3]),
     ?T(<<777:I_13,13:3>>,
        [24,77]),
     ?T(<<5:4,987:I_13,537:7>>,
        [81,237,153]),
     ?T(<<0.0:32/float>>,
	[0,0,0,0]),
     ?T(<<0.125:32/float>>,
	[62,0,0,0]),
     ?T(<<1.0:32/little-float>>,
	[0,0,128,63]),
     ?T(<<I_big1:32>>,
	[138,99,0,147]),
     ?T(<<57285702734876389752897684:(I_16+16)>>,
	[138,99,0,148]),
     ?T(<<-1:17/unit:8>>,
	lists:duplicate(17, 255)),
     ?T(<<-1:8/unit:17>>,
	lists:duplicate(17, 255)),
     ?T(<<4:(I_16-8)/unit:2,5:2/unit:8>>,
	[0,4,0,5]),
     ?T(<<1:1, 0:(I_13-7), 1:1>>,
	[129]),
     ?T(<<1:3,"string",9:5>>,
	[46,110,142,77,45,204,233]),
     ?T(<<37.98:64/native-float>>,
	native_3798()),
     ?T(<<32978297842987249827298387697777669766334937:128/native-integer>>,
	native_bignum()),

     ?T(<<Bin/binary>>,
        [165,90,195]),
     ?T(<<79,Bin/binary>>,
        [79,165,90,195]),
     ?T(<<3479:I_13,Bin/binary,7:3>>,
        [108,189,42,214,31]),
     ?T(<<3479:I_13,Bin/binary,7:1/unit:3>>,
        [108,189,42,214,31]),
     ?T(<<869:16/little,3479:I_13,Bin/binary,7:1/unit:3>>,
        [101,3,108,189,42,214,31]),
     ?T(<<869:16/little,3479:I_13,Bin/binary,7:1/unit:3,Bin/binary>>,
        [101,3,108,189,42,214,31,165,90,195]),

     %% Test of aligment flag.
     ?T(<<0:I_13/unit:8,1:6,0:2>>,
	[0,0,0,0,0,0,0,0,0,0,0,0,0,4]),

     %% Test of literals (coverage).
     ?T(<<0:128>>,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]),
     ?T(<<0:13/little,7:3>>,[0,7]),
     ?T(<<16#77FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF:264>>,
	[0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
	 16#77,16#FF,16#FF,16#FF,16#FF,16#FF,16#FF,16#FF,16#FF,16#FF,16#FF,
	 16#FF,16#FF,16#FF,16#FF,16#FF,16#FF]),

     %% Mix different units.
     ?T(<<37558955:(I_16-12)/unit:8,1:1>>,
	[2,61,26,171,<<1:1>>])
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
    Bin = list_to_bitstring(Bytes),
    if
	C_bin == Bin ->
	    ok;
	true ->
	    io:format("ERROR: Compiled: ~p. Expected ~p. Got ~p.~n",
		      [Str, Bytes, bitstring_to_list(C_bin)]),
	    test_server:fail(comp)
    end,
    if
	E_bin == Bin ->
	    ok;
	true ->
	    io:format("ERROR: Interpreted: ~p. Expected ~p. Got ~p.~n",
		      [Str, Bytes, bitstring_to_list(E_bin)]),
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
	    case equal_lists(bitstring_to_list(C_bin),
			     bitstring_to_list(E_bin),
			     Arbitrary) of
		false ->
		    io:format("ERROR: Compiled not equal to interpreted:"
			      "~n ~p, ~p.~n",
			      [bitstring_to_list(C_bin), bitstring_to_list(E_bin)]),
		    test_server:fail(comp);
		0 ->
		    ok;
		%% For situations where the final bits may not matter, like
		%% for floats:
		N when is_integer(N) ->
		    io:format("Info: compiled and interpreted differ in the"
			      " last bytes:~n ~p, ~p.~n",
			      [bitstring_to_list(C_bin), bitstring_to_list(E_bin)]),
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

test1(Config) when is_list(Config) ->
    ?line I_13 = i(13),
    ?line I_big1 = big(1),
    ?line I_16 = i(16),
    ?line Bin = i(<<16#A5,16#5A,16#C3>>),
    ?line Vars = lists:sort([{'I_13',I_13},
			     {'I_big1',I_big1},
			     {'I_16',I_16},
			     {'Bin',Bin}]),
    ?line lists:foreach(fun one_test/1, eval_list(l(I_13, I_big1, I_16, Bin), Vars)).

fail(Config) when is_list(Config) ->
    I_minus_777 = i(-777),
    I_minus_2047 = i(-2047),

    %% One negative field size, but the sum of field sizes will be 1 byte.
    %% Make sure that we reject that properly.

    ?line {'EXIT',{badarg,_}} = (catch <<I_minus_777:2048/unit:8,
					57:I_minus_2047/unit:8>>),

    %% Same thing, but use literals.
    ?line {'EXIT',{badarg,_}} = (catch <<I_minus_777:2048/unit:8,
					57:(-2047)/unit:8>>),

    %% Not numbers.
    ?line {'EXIT',{badarg,_}} = (catch <<45:(i(not_a_number))>>),
    ?line {'EXIT',{badarg,_}} = (catch <<13:8,45:(i(not_a_number))>>),

    %% Unaligned sizes.
    BadSz = i(7),
    Bitstr = i(<<42:17>>),

    ?line {'EXIT',{badarg,_}} = (catch <<Bitstr:4/binary>>),
    ?line {'EXIT',{badarg,_}} = (catch <<Bitstr:BadSz/binary>>),

    ?line [] = [X || {X} <- [], X == <<Bitstr:BadSz/binary>>],
    ?line [] = [X || {X} <- [], X == <<Bitstr:4/binary>>],

    %% Literals with incorrect type.
    ?line {'EXIT',{badarg,_}} = (catch <<42.0/integer>>),
    ?line {'EXIT',{badarg,_}} = (catch <<42/binary>>),
    ?line {'EXIT',{badarg,_}} = (catch <<an_atom/integer>>),
    
    ok.

float_bin(Config) when is_list(Config) ->
    %% Some more coverage.
    ?line {<<1,2,3>>,7.0} = float_bin_1(4),
    F = 42.0,
    ?line <<42,0,0,0,0,0,0,69,64>> = <<(id(42)),F/little-float>>,
    ok.

float_bin_1(F) ->
    {<<1,2,3>>,F+3.0}.

in_guard(Config) when is_list(Config) ->
    ?line 1 = in_guard_1(<<16#74ad:16>>, 16#e95, 5),
    ?line 2 = in_guard_1(<<16#3A,16#F7,"hello">>, 16#3AF7, <<"hello">>),
    ?line 3 = in_guard_1(<<16#FBCD:14,3.1415/float,3:2>>, 16#FBCD, 3.1415),
    ?line nope = in_guard_1(<<1>>, 42, b),
    ?line nope = in_guard_1(<<1>>, a, b),
    ?line nope = in_guard_1(<<1,2>>, 1, 1),
    ?line nope = in_guard_1(<<4,5>>, 1, 2.71),
    ?line nope = in_guard_1(<<4,5>>, 1, <<12,13>>),

    ?line 1 = in_guard_2(<<0,56>>, 7, blurf),
    ?line 2 = in_guard_2(<<1,255>>, 511, blurf),
    ?line 3 = in_guard_2(<<0,3>>, 0, blurf),
    ?line 4 = in_guard_2(<<>>, 1, {<<7:16>>}),
    ?line nope = in_guard_2(<<4,5>>, 1, blurf),

    ?line 42 = in_guard_3(<<1,2,3,42>>, <<1,2,3>>),
    ?line 42 = in_guard_3(<<1,2,3,42>>, <<1,2,3>>),
    ?line nope = in_guard_3(<<>>, <<>>),

    ?line ok = in_guard_4(<<15:4>>, 255),
    ?line nope = in_guard_4(<<15:8>>, 255),
    ok.

in_guard_1(Bin, A, B) when <<A:13,B:3>> == Bin -> 1;
in_guard_1(Bin, A, B) when <<A:16,B/binary>> == Bin -> 2;
in_guard_1(Bin, A, B) when <<A:14,B/float,3:2>> == Bin -> 3;
in_guard_1(Bin, A, B) when {a,b,<<A:14,B/float,3:2>>} == Bin -> cant_happen;
in_guard_1(_, _, _) -> nope.

in_guard_2(Bin, A, _T) when <<A:13,0:3>> == Bin -> 1;
in_guard_2(Bin, A, _T) when <<A:16>> == Bin -> 2;
in_guard_2(Bin, A, _T) when <<A:14,3:2>> == Bin -> 3;
in_guard_2(_Bin, A, T) when {A,b} > {0,1}, {<<A:14,3:2>>} == T -> 4;
in_guard_2(_, _, _) -> nope.

in_guard_3(Bin, A) when <<A/binary,42>> =:= Bin -> 42;
in_guard_3(_, _) -> nope.

in_guard_4(Bin, A) when <<A:4>> =:= Bin -> ok;
in_guard_4(_, _) -> nope.

in_catch(Config) when is_list(Config) ->
    ?line <<42,0,5>> = small(42, 5),
    ?line <<255>> = small(255, <<1,2,3,4,5,6,7,8,9>>),
    ?line <<1,2>> = small(<<7,8,9,10>>, 258),
    ?line <<>> = small(<<1,2,3,4,5>>, <<7,8,9,10>>),
    ok.

small(A, B) ->
    case begin
	     case catch <<A:8>> of
		 {'EXIT',_} -> <<>>;
		 ResA0 -> ResA0
	     end
	 end of
	ResA -> ok
    end,
    case begin
	     case catch <<B:16>> of
		 {'EXIT',_} -> <<>>;
		 ResB0 -> ResB0
	     end
	 end of
	ResB -> ok
    end,
    <<ResA/binary,ResB/binary>>.

nasty_literals(Config) when is_list(Config) ->
    case erlang:system_info(endian) of
	big ->
	    ?line [0,42] = binary_to_list(id(<<42:16/native>>));
	little ->
	    ?line [42,0] = binary_to_list(id(<<42:16/native>>))
    end,

    ?line Bin0 = id(<<1,2,3,0:10000000,4,5,6>>),
    ?line 1250006 = size(Bin0),
    ?line <<1,2,3,0:10000000,4,5,6>> = Bin0,

    ?line Bin1 = id(<<0:10000000,7,8,-1:10000000,9,10,0:10000000>>),
    ?line 3750004 = size(Bin1),
    ?line <<0:10000000,7,8,-1:10000000/signed,9,10,0:10000000>> = Bin1,

    ?line <<255,255,0,0,0>> = id(<<255,255,0,0,0>>),

    %% Coverage.
    I = 16#7777FFFF7777FFFF7777FFFF7777FFFF7777FFFF7777FFFF,
    id(<<I:260>>),

    ok.

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

side_effect(Config) when is_list(Config) ->
    {'EXIT',{badarg,_}} = (catch side_effect_1(a)),
    {'EXIT',{badarg,_}} = (catch side_effect_1(<<>>)),
    ?line ok = side_effect_1(42),
    ok.

side_effect_1(A) ->
    <<A:17>>,					%Warning intentional.
    ok.

-record(otp_7029, {a,b}).

opt(Config) when is_list(Config) ->
    ?line 42 = otp_7029(#otp_7029{a = <<>>,b = 42}),
    N = 16,
    ?line <<1,3,65>> = id(<<1,833:N>>),
    ?line <<1,66,3>> = id(<<1,834:N/little>>),
    ?line <<1,65,136,0,0>> = id(<<1,17.0:32/float>>),
    ?line <<1,64,8,0,0,0,0,0,0>> = id(<<1,3.0:N/float-unit:4>>),
    ?line <<1,0,0,0,0,0,0,8,64>> = id(<<1,3.0:N/little-float-unit:4>>),
    ?line {'EXIT',{badarg,_}} = (catch id(<<3.1416:N/float>>)),

    B = <<1,2,3,4,5>>,
    ?line <<0,1,2,3,4,5>> = id(<<0,B/binary>>),
    ?line <<1,2,3,4,5,19>> = id(<<B:5/binary,19>>),
    ?line <<1,2,3,42>> = id(<<B:3/binary,42>>),

    ?line {'EXIT',_} = (catch <<<<23,56,0,2>>:(2.5)/binary>>),
    ?line {'EXIT',_} = (catch <<<<23,56,0,2>>:(-16)/binary>>),
    ?line {'EXIT',_} = (catch <<<<23,56,0,2>>:(anka)>>),
    ?line {'EXIT',_} = (catch <<<<23,56,0,2>>:64/float>>),
    ?line {'EXIT',_} = (catch <<<<23,56,0,2:7>>/binary>>),

    case id(false) of
	true -> ?line opt_dont_call_me();
	false -> ok
    end,

    ok.

opt_dont_call_me() ->
    N = 16#12345678,
    <<0:N>>.

otp_7029(R) ->
    #otp_7029{a = <<>>} = R,
    R#otp_7029.b.

otp_7556(Config) when is_list(Config) ->
    [otp_7556(<<>>, 1024, 1024, 1024) || _ <- lists:seq(0, 1023)],
    ok.

otp_7556(Bin, A, B, C) ->
    %% When allocating the binary, the sizes 16*A and 16*A would
    %% be forgotten.
    <<Bin/binary,(-1):A/unit:16,0:B/unit:16,(-1):C/unit:16>>.

%% Test binary construction combined with floating point operations
%% (mostly to cover code in beam_flatten that combines the allocation
%% for a binary construction with a later allocation).

float_arith(Config) when is_list(Config) ->
    ?line {<<1,2,3,64,69,0,0,0,0,0,0>>,21.0} = do_float_arith(<<1,2,3>>, 42, 2),
    ok.

do_float_arith(Bin0, X, Y)  ->
    Bin = <<Bin0/binary,X/float>>,
    {Bin,X / Y}.

otp_8054(Config) when is_list(Config) ->
    ?line <<"abc">> = otp_8054_1([null,1,2,3], <<"abc">>),
    ok.

otp_8054_1([H|T], Bin) ->
    _ = case H of
	    null ->
		%% The beam_validator would complain about {x,3}
		%% not being live in bs_append/8 because of a live
		%% optimization bug.
		<<Bin/binary>>;
	    _ ->
		ok
	end,
    otp_8054_1(T, Bin);
otp_8054_1([], Bin) -> Bin.
