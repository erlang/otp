%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
-module(bs_match_misc_SUITE).

-export([all/0, suite/0,
	 bound_var/1,bound_tail/1,t_float/1,little_float/1,sean/1,
	 kenneth/1,encode_binary/1,native/1,happi/1,
	 size_var/1,wiger/1,x0_context/1,huge_float_field/1,
	 writable_binary_matched/1,otp_7198/1,unordered_bindings/1,
	 float_middle_endian/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {seconds, 10}}].

all() -> 
    [bound_var, bound_tail, t_float, little_float, sean,
     kenneth, encode_binary, native, happi, size_var, wiger,
     x0_context, huge_float_field, writable_binary_matched,
     otp_7198, unordered_bindings, float_middle_endian].


%% Test matching of bound variables.
bound_var(Config) when is_list(Config) ->
    ok = bound_var(42, 13, <<42,13>>),
    nope = bound_var(42, 13, <<42,255>>),
    nope = bound_var(42, 13, <<154,255>>),
    ok.

bound_var(A, B, <<A:8,B:8>>) -> ok;
bound_var(_, _, _) -> nope.

%% Test matching of a bound tail.
bound_tail(Config) when is_list(Config) ->
    ok = bound_tail(<<>>, <<13,14>>),
    ok = bound_tail(<<2,3>>, <<1,1,2,3>>),
    nope = bound_tail(<<2,3>>, <<1,1,2,7>>),
    nope = bound_tail(<<2,3>>, <<1,1,2,3,4>>),
    nope = bound_tail(<<2,3>>, <<>>),
    ok.

bound_tail(T, <<_:16,T/binary>>) -> ok;
bound_tail(_, _) -> nope.

t_float(Config) when is_list(Config) ->
    F = f1(),
    G = f_one(),

    G = match_float(<<63,128,0,0>>, 32, 0),
    G = match_float(<<63,240,0,0,0,0,0,0>>, 64, 0),

    fcmp(F, match_float(<<F:32/float>>, 32, 0)),
    fcmp(F, match_float(<<F:64/float>>, 64, 0)),
    fcmp(F, match_float(<<1:1,F:32/float,127:7>>, 32, 1)),
    fcmp(F, match_float(<<1:1,F:64/float,127:7>>, 64, 1)),
    fcmp(F, match_float(<<1:13,F:32/float,127:3>>, 32, 13)),
    fcmp(F, match_float(<<1:13,F:64/float,127:3>>, 64, 13)),

    {'EXIT',{{badmatch,_},_}} = (catch match_float(<<0,0>>, 16, 0)),
    {'EXIT',{{badmatch,_},_}} = (catch match_float(<<0,0>>, 16#7fffffff, 0)),

    ok.

float_middle_endian(Config) when is_list(Config) ->
    F = 9007199254740990.0, % turns to -NaN when word-swapped
    fcmp(F, match_float(<<F:64/float>>, 64, 0)),
    fcmp(F, match_float(<<1:1,F:64/float,127:7>>, 64, 1)),
    fcmp(F, match_float(<<1:13,F:64/float,127:3>>, 64, 13)),
    ok.


fcmp(F1, F2) when (F1 - F2) / F2 < 0.0000001 -> ok.
    
match_float(Bin0, Fsz, I) ->
    Bin = make_sub_bin(Bin0),
    Bsz = size(Bin) * 8,
    Tsz = Bsz - Fsz - I,
    <<_:I,F:Fsz/float,_:Tsz>> = Bin,
    F.

little_float(Config) when is_list(Config) ->
    F = f2(),
    G = f_one(),

    G = match_float_little(<<0,0,0,0,0,0,240,63>>, 64, 0),
    G = match_float_little(<<0,0,128,63>>, 32, 0),

    fcmp(F, match_float_little(<<F:32/float-little>>, 32, 0)),
    fcmp(F, match_float_little(<<F:64/float-little>>, 64, 0)),
    fcmp(F, match_float_little(<<1:1,F:32/float-little,127:7>>, 32, 1)),
    fcmp(F, match_float_little(<<1:1,F:64/float-little,127:7>>, 64, 1)),
    fcmp(F, match_float_little(<<1:13,F:32/float-little,127:3>>, 32, 13)),
    fcmp(F, match_float_little(<<1:13,F:64/float-little,127:3>>, 64, 13)),

    ok.

match_float_little(Bin0, Fsz, I) ->
    Bin = make_sub_bin(Bin0),
    Bsz = size(Bin) * 8,
    Tsz = Bsz - Fsz - I,
    <<_:I,F:Fsz/float-little,_:Tsz>> = Bin,
    F.


make_sub_bin(Bin0) ->
    Sz = size(Bin0),
    Bin1 = <<37,Bin0/binary,38,39>>,
    <<_:8,Bin:Sz/binary,_:8,_:8>> = Bin1,
    Bin.

f1() ->
    3.1415.

f2() ->
    2.7133.

f_one() ->
    1.0.

sean(Config) when is_list(Config) ->
    small = sean1(<<>>),
    small = sean1(<<1>>),
    small = sean1(<<1,2>>),
    small = sean1(<<1,2,3>>),
    large = sean1(<<1,2,3,4>>),

    small = sean1(<<4>>),
    small = sean1(<<4,5>>),
    small = sean1(<<4,5,6>>),
    {'EXIT',{function_clause,_}} = (catch sean1(<<4,5,6,7>>)),
    ok.

sean1(<<B/binary>>) when byte_size(B) < 4 -> small;
sean1(<<1, _B/binary>>) -> large.

kenneth(Config) when is_list(Config) ->
    {ok,[145,148,113,129,0,0,0,0]} = 
	msisdn_internal_storage(<<145,148,113,129,0,0,0,0>>, []).

msisdn_internal_storage(<<>>,MSISDN) ->
    {ok,lists:reverse(MSISDN)};
msisdn_internal_storage(<<2#11111111:8,_Rest/binary>>,MSISDN) ->
    {ok,lists:reverse(MSISDN)};
msisdn_internal_storage(<<2#1111:4,DigitN:4,_Rest/binary>>,MSISDN) when
    DigitN < 10 ->
    {ok,lists:reverse([(DigitN bor 2#11110000)|MSISDN])};
msisdn_internal_storage(<<DigitNplus1:4,DigitN:4,Rest/binary>>,MSISDN) when
    DigitNplus1 < 10,
    DigitN < 10 ->
    NewMSISDN=[((DigitNplus1 bsl 4) bor DigitN)|MSISDN],
    msisdn_internal_storage(Rest,NewMSISDN);
msisdn_internal_storage(_Rest,_MSISDN) ->
    {fault}. %% Mandatory IE incorrect

encode_binary(Config) when is_list(Config) ->
    "C2J2QiSc" = encodeBinary(<<11,98,118,66,36,156>>, []),
    ok.

encodeBinary(<<>>, Output) ->
    lists:reverse(Output);
encodeBinary(<<Data:1/binary>>, Output) ->
    <<DChar1:6, DChar2:2>> = Data,
    Char1 = getBase64Char(DChar1),
    Char2 = getBase64Char(DChar2),
    Char3 = "=",
    Char4 = "=",
    NewOutput = Char4 ++ Char3 ++ Char2 ++ Char1 ++ Output,
    encodeBinary(<<>>, NewOutput);
encodeBinary(<<Data:2/binary>>, Output) ->
    <<DChar1:6, DChar2:6, DChar3:4>> = Data,
    Char1 = getBase64Char(DChar1),
    Char2 = getBase64Char(DChar2),
    Char3 = getBase64Char(DChar3),
    Char4 = "=",
    NewOutput = Char4 ++ Char3 ++ Char2 ++ Char1 ++ Output,
    encodeBinary(<<>>, NewOutput);
encodeBinary(<<Data:3/binary, Rest/binary>>, Output) ->
    <<DChar1:6, DChar2:6, DChar3:6, DChar4:6>> = Data,
    Char1 = getBase64Char(DChar1),
    Char2 = getBase64Char(DChar2),
    Char3 = getBase64Char(DChar3),
    Char4 = getBase64Char(DChar4),
    NewOutput = Char4 ++ Char3 ++ Char2 ++ Char1 ++ Output,
    encodeBinary(Rest, NewOutput);
encodeBinary(_Data, _) ->
    error.

getBase64Char(0)  -> "A";
getBase64Char(1)  -> "B";
getBase64Char(2)  -> "C";
getBase64Char(3)  -> "D";
getBase64Char(4)  -> "E";
getBase64Char(5)  -> "F";
getBase64Char(6)  -> "G";
getBase64Char(7)  -> "H";
getBase64Char(8)  -> "I";
getBase64Char(9)  -> "J";
getBase64Char(10) -> "K";
getBase64Char(11) -> "L";
getBase64Char(12) -> "M";
getBase64Char(13) -> "N";
getBase64Char(14) -> "O";
getBase64Char(15) -> "P";
getBase64Char(16) -> "Q";
getBase64Char(17) -> "R";
getBase64Char(18) -> "S";
getBase64Char(19) -> "T";
getBase64Char(20) -> "U";
getBase64Char(21) -> "V";
getBase64Char(22) -> "W";
getBase64Char(23) -> "X";
getBase64Char(24) -> "Y";
getBase64Char(25) -> "Z";
getBase64Char(26) -> "a";
getBase64Char(27) -> "b";
getBase64Char(28) -> "c";
getBase64Char(29) -> "d";
getBase64Char(30) -> "e";
getBase64Char(31) -> "f";
getBase64Char(32) -> "g";
getBase64Char(33) -> "h";
getBase64Char(34) -> "i";
getBase64Char(35) -> "j";
getBase64Char(36) -> "k";
getBase64Char(37) -> "l";
getBase64Char(38) -> "m";
getBase64Char(39) -> "n";
getBase64Char(40) -> "o";
getBase64Char(41) -> "p";
getBase64Char(42) -> "q";
getBase64Char(43) -> "r";
getBase64Char(44) -> "s";
getBase64Char(45) -> "t";
getBase64Char(46) -> "u";
getBase64Char(47) -> "v";
getBase64Char(48) -> "w";
getBase64Char(49) -> "x";
getBase64Char(50) -> "y";
getBase64Char(51) -> "z";
getBase64Char(52) -> "0";
getBase64Char(53) -> "1";
getBase64Char(54) -> "2";
getBase64Char(55) -> "3";
getBase64Char(56) -> "4";
getBase64Char(57) -> "5";
getBase64Char(58) -> "6";
getBase64Char(59) -> "7";
getBase64Char(60) -> "8";
getBase64Char(61) -> "9";
getBase64Char(62) -> "+";
getBase64Char(63) -> "/";
getBase64Char(_Else) ->
    %% This is an illegal input.
%    cgLogEM:log(error, ?MODULE, getBase64Char, [Else],
%		"illegal input",
%		?LINE, version()),
    "**".

-define(M(F), <<F>> = <<F>>).

native(Config) when is_list(Config) ->
    ?M(3.14:64/native-float), 
    ?M(333:16/native),
    ?M(38658345:32/native),
    case <<1:16/native>> of
	<<0,1>> -> native_big();
	<<1,0>> -> native_little()
    end.

native_big() ->
    <<37.33:64/native-float>> = <<37.33:64/big-float>>,
    <<3974:16/native-integer>> = <<3974:16/big-integer>>,
    {comment,"Big endian"}.

native_little() ->
    <<37869.32343:64/native-float>> = <<37869.32343:64/little-float>>,
    <<7974:16/native-integer>> = <<7974:16/little-integer>>,
    {comment,"Little endian"}.

happi(Config) when is_list(Config) ->
    Bin = <<".123">>,
    <<"123">> = lex_digits1(Bin, 1, []),
    <<"123">> = lex_digits2(Bin, 1, []),
    ok.

lex_digits1(<<$., Rest/binary>>,_Val,_Acc) ->
    Rest;
lex_digits1(<<N, Rest/binary>>,Val, Acc) when N >= $0 , N =< $9 ->
    lex_digits1(Rest,Val*10+dec(N),Acc);
lex_digits1(_Other,_Val,_Acc) ->
    not_ok.

lex_digits2(<<N, Rest/binary>>,Val, Acc) when N >= $0 , N =< $9 ->
    lex_digits2(Rest,Val*10+dec(N),Acc);
lex_digits2(<<$., Rest/binary>>,_Val,_Acc) ->
    Rest;
lex_digits2(_Other,_Val,_Acc) ->
    not_ok.

dec(A) ->
    A-$0.  

size_var(Config) when is_list(Config) ->
    {<<45>>,<<>>} = split(<<1:16,45>>),
    {<<45>>,<<46,47>>} = split(<<1:16,45,46,47>>),
    {<<45,46>>,<<47>>} = split(<<2:16,45,46,47>>),

    {<<45,46,47>>,<<48>>} = split_2(<<16:8,3:16,45,46,47,48>>),
    
    {<<45,46>>,<<47>>} = split(2, <<2:16,45,46,47>>),
    {'EXIT',{function_clause,_}} = (catch split(42, <<2:16,45,46,47>>)),

    <<"cdef">> = skip(<<2:8,"abcdef">>),
    
    ok.

split(<<N:16,B:N/binary,T/binary>>) ->
    {B,T}.

split(N, <<N:16,B:N/binary,T/binary>>) ->
    {B,T}.

split_2(<<N0:8,N:N0,B:N/binary,T/binary>>) ->
    {B,T}.

skip(<<N:8,_:N/binary,T/binary>>) -> T.

wiger(Config) when is_list(Config) ->
    ok1 = wcheck(<<3>>),
    ok2 = wcheck(<<1,2,3>>),
    ok3 = wcheck(<<4>>),
    {error,<<1,2,3,4>>} = wcheck(<<1,2,3,4>>),
    {error,<<>>} = wcheck(<<>>),
    ok.

wcheck(<<A>>) when A==3->
    ok1;
wcheck(<<_,_:2/binary>>) ->
    ok2;
wcheck(<<_>>) ->
    ok3;
wcheck(Other) ->
    {error,Other}.

%% Test that having the match context in x(0) works.

x0_context(Config) when is_list(Config) ->
    x0_0([], <<3.0:64/float,42:16,123456:32>>).

x0_0(_, Bin) ->
    <<3.0:64/float,42:16,_/binary>> = Bin,
    x0_1([], Bin, 64, 16, 2).

x0_1(_, Bin, FloatSz, IntSz, BinSz) ->
    <<_:FloatSz/float,42:IntSz,B:BinSz/binary,C:1/binary,D/binary>> = Bin,
    id({B,C,D}),
    <<_:FloatSz/float,42:IntSz,B:BinSz/binary,_/binary>> = Bin,
    x0_2([], Bin).

x0_2(_, Bin) ->
    <<_:64,0:7,42:9,_/binary>> = Bin,
    x0_3([], Bin).

x0_3(_, Bin) ->
    case Bin of
        <<_:72,7:8,_/binary>> ->
            ct:fail(bs_matched_1);
        <<_:64,0:16,_/binary>> ->
            ct:fail(bs_matched_2);
        <<_:64,42:16,123456:32,_/binary>> ->
            ok
    end.


huge_float_field(Config) when is_list(Config) ->
    Sz = 1 bsl 27,
    Bin = <<0:Sz>>,

    nomatch = overflow_huge_float_skip_32(Bin),
    nomatch = overflow_huge_float_32(Bin),

    ok = overflow_huge_float(Bin, lists:seq(25, 32)++lists:seq(50, 64)),
    ok = overflow_huge_float_unit128(Bin, lists:seq(25, 32)++lists:seq(50, 64)),
    ok.

overflow_huge_float_skip_32(<<_:4294967296/float,0,_/binary>>) -> 1; % 1 bsl 32
overflow_huge_float_skip_32(<<_:33554432/float-unit:128,0,_/binary>>) -> 2; % 1 bsl 25
overflow_huge_float_skip_32(<<_:67108864/float-unit:64,0,_/binary>>) -> 3; % 1 bsl 26
overflow_huge_float_skip_32(<<_:134217728/float-unit:32,0,_/binary>>) -> 4; % 1 bsl 27
overflow_huge_float_skip_32(<<_:268435456/float-unit:16,0,_/binary>>) -> 5; % 1 bsl 28
overflow_huge_float_skip_32(<<_:536870912/float-unit:8,0,_/binary>>) -> 6; % 1 bsl 29
overflow_huge_float_skip_32(<<_:1073741824/float-unit:8,0,_/binary>>) -> 7; % 1 bsl 30
overflow_huge_float_skip_32(<<_:2147483648/float-unit:8,0,_/binary>>) -> 8; % 1 bsl 31
overflow_huge_float_skip_32(_) -> nomatch.

overflow_huge_float_32(<<F:4294967296/float,_/binary>>) -> {1,F}; % 1 bsl 32
overflow_huge_float_32(<<F:33554432/float-unit:128,0,_/binary>>) -> {2,F}; % 1 bsl 25
overflow_huge_float_32(<<F:67108864/float-unit:128,0,_/binary>>) -> {3,F}; % 1 bsl 26
overflow_huge_float_32(<<F:134217728/float-unit:128,0,_/binary>>) -> {4,F}; % 1 bsl 27
overflow_huge_float_32(<<F:268435456/float-unit:128,0,_/binary>>) -> {5,F}; % 1 bsl 28
overflow_huge_float_32(<<F:536870912/float-unit:128,0,_/binary>>) -> {6,F}; % 1 bsl 29
overflow_huge_float_32(<<F:1073741824/float-unit:128,0,_/binary>>) -> {7,F}; % 1 bsl 30
overflow_huge_float_32(<<F:2147483648/float-unit:128,0,_/binary>>) -> {8,F}; % 1 bsl 31
overflow_huge_float_32(_) -> nomatch.


overflow_huge_float(Bin, [Sz0|Sizes]) ->
    Sz = id(1 bsl Sz0),
    case Bin of
	<<_:Sz/float-unit:8,0,_/binary>> ->
	    {error,Sz};
	_ ->
	    case Bin of
		<<Var:Sz/float-unit:8,0,_/binary>> ->
		    {error,Sz,Var};
		_ ->
		    overflow_huge_float(Bin, Sizes)
	    end
    end;
overflow_huge_float(_, []) -> ok.

overflow_huge_float_unit128(Bin, [Sz0|Sizes]) ->
    Sz = id(1 bsl Sz0),
    case Bin of
        <<_:Sz/float-unit:128,0,_/binary>> ->
            {error,Sz};
        _ ->
            case Bin of
                <<Var:Sz/float-unit:128,0,_/binary>> ->
                    {error,Sz,Var};
                _ ->
                    overflow_huge_float_unit128(Bin, Sizes)
            end
    end;
overflow_huge_float_unit128(_, []) -> ok.


%%
%% Test that a writable binary can be safely matched.
%%

writable_binary_matched(Config) when is_list(Config) ->
    WritableBin = create_writeable_binary(),
    writable_binary_matched(WritableBin, WritableBin, 500).

writable_binary_matched(<<0>>, _, N) ->
    if N =:= 0 -> ok;
       true ->
           put(grow_heap, [N|get(grow_heap)]),
           WritableBin = create_writeable_binary(),
           writable_binary_matched(WritableBin, WritableBin, N-1)
    end;
writable_binary_matched(<<B:8,T/binary>>, WritableBin0, N) ->
    WritableBin = writable_binary(WritableBin0, B),
    writable_binary_matched(T, WritableBin, N).

writable_binary(WritableBin0, B) when is_binary(WritableBin0) ->
    %% Heavy append to force the binary to move.
    WritableBin = <<WritableBin0/binary,0:(size(WritableBin0))/unit:8,B>>,
    id(<<(id(0)):128/unit:8>>),
    WritableBin.

create_writeable_binary() ->
  <<(id(<<>>))/binary,1,2,3,4,5,6,0>>.

otp_7198(Config) when is_list(Config) ->
    %% When a match context was reused, and grown at the same time to
    %% increase the number of saved positions, the thing word was not updated
    %% to account for the new size. Therefore, if there was a garbage collection,
    %% the new slots would be included in the garbage collection.
    [do_otp_7198(FillerSize) || FillerSize <- lists:seq(0, 256)],
    ok.

do_otp_7198(FillerSize) ->
    Filler = erlang:make_tuple(FillerSize, 42),
    {Pid,Ref} = spawn_monitor(fun() -> do_otp_7198_test(Filler) end),
    receive
	{'DOWN',Ref,process,Pid,normal} ->
	    ok;
	{'DOWN',Ref,process,Pid,Reason} ->
	    ct:fail("unexpected: ~p", [Reason])
    end.
    
do_otp_7198_test(_) ->
    [{'KEYWORD',114},
     {'KEYWORD',101},
     {'KEYWORD',103},
     {'KEYWORD',105},
     {'KEYWORD',111},
     {'FIELD',110},
     {'KEYWORD',119},
     {'KEYWORD',104},
     {'KEYWORD',97},
     {'KEYWORD',116},
     {'KEYWORD',101},
     {'KEYWORD',118},
     {'KEYWORD',101},
     {'KEYWORD',114},
     '$thats_all_folks$'] = otp_7198_scan(<<"region:whatever">>, []).

    
otp_7198_scan(<<>>, TokAcc) ->
        lists:reverse(['$thats_all_folks$' | TokAcc]);

otp_7198_scan(<<D, Z, Rest/binary>>, TokAcc) when
                        (D =:= $D orelse D =:= $d) and
                        ((Z =:= $\s) or (Z =:= $() or (Z =:= $))) ->
    otp_7198_scan(<<Z, Rest/binary>>, ['AND' | TokAcc]);

otp_7198_scan(<<D>>, TokAcc) when
                        (D =:= $D) or (D =:= $d) ->
    otp_7198_scan(<<>>, ['AND' | TokAcc]);

otp_7198_scan(<<N, Z, Rest/binary>>, TokAcc) when
                        (N =:= $N orelse N =:= $n) and
                        ((Z =:= $\s) or (Z =:= $() or (Z =:= $))) ->
    otp_7198_scan(<<Z, Rest/binary>>, ['NOT' | TokAcc]);

otp_7198_scan(<<C, Rest/binary>>, TokAcc) when
                                (C >= $A) and (C =< $Z);
                                (C >= $a) and (C =< $z);
                                (C >= $0) and (C =< $9) ->
    case Rest of
        <<$:, R/binary>> ->
            otp_7198_scan(R, [{'FIELD', C} | TokAcc]);
        _ ->
            otp_7198_scan(Rest, [{'KEYWORD', C} | TokAcc])
    end.

unordered_bindings(Config) when is_list(Config) ->
    {<<1,2,3,4>>,<<42,42>>,<<3,3,3>>} =
	unordered_bindings(4, 2, 3, <<1,2,3,4, 42,42, 3,3,3, 3>>),
    ok.

unordered_bindings(CompressedLength, HashSize, PadLength, T) ->
    <<Content:CompressedLength/binary,Mac:HashSize/binary,
     Padding:PadLength/binary,PadLength>> = T,
    {Content,Mac,Padding}.


id(I) -> I.
