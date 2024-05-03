%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2023. All Rights Reserved.
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

-module(bs_utf_SUITE).

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1,
	 utf8_roundtrip/1,utf16_roundtrip/1,utf32_roundtrip/1,
	 utf8_illegal_sequences/1,utf16_illegal_sequences/1,
	 utf32_illegal_sequences/1,
	 bad_construction/1,
         utf8_big_file/1]).

-include_lib("common_test/include/ct.hrl").

-define(FAIL(Expr), fail_check(catch Expr, ??Expr, [])).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 6}}].

all() ->
    [utf8_roundtrip, utf16_roundtrip, utf32_roundtrip,
     utf8_illegal_sequences, utf16_illegal_sequences,
     utf32_illegal_sequences, bad_construction,
     utf8_big_file].

init_per_suite(Config) ->
    %% Make sure that calls to id/1 will hide types.
    id(Config),
    Config.

end_per_suite(Config) ->
    Config.

utf8_roundtrip(Config) when is_list(Config) ->
    utf8_roundtrip(0, 16#D7FF),
    utf8_roundtrip(16#E000, 16#10FFFF),
    ok.

utf8_roundtrip(First, Last) ->
    %% Hide types.
    do_utf8_roundtrip(id(First), id(Last)).

do_utf8_roundtrip(First, Last) when First =< Last ->
    Bin = int_to_utf8(id(First)),
    Bin = id(<<First/utf8>>),
    Bin = id(<<(id(<<>>))/binary,First/utf8>>),

    <<0:7/unit:8,Bin/binary>> = id(<<0:7/unit:8,First/utf8>>),

    %% Here a heap binary and a sub binary will be allocated. If the
    %% write in the utf8 segment extends beyond the end of heap binary,
    %% it will overwrite the header for the sub binary.
    <<-1:(64-9)/signed,Bin/binary>> = id(<<-1:(64-9),First/utf8>>),
    <<-1:63/signed,Bin/binary>> = id(<<-1:63,First/utf8>>),

    if
        is_integer(First) ->
            Bin = id(<<First/utf8>>)
    end,

    <<1:1,Bin/binary>> = id(<<1:1,First/utf8>>),
    <<0:1,Bin/binary>> = id(<<0:1,First/utf8>>),
    <<3:2,Bin/binary>> = id(<<3:2,First/utf8>>),
    <<5:3,Bin/binary>> = id(<<5:3,First/utf8>>),
    <<13:4,Bin/binary>> = id(<<13:4,First/utf8>>),
    <<21:5,Bin/binary>> = id(<<21:5,First/utf8>>),
    <<51:6,Bin/binary>> = id(<<51:6,First/utf8>>),
    <<107:7,Bin/binary>> = id(<<107:7,First/utf8>>),

    <<First/utf8>> = Bin,
    <<First/utf8>> = make_unaligned(Bin),

    %% Matching of utf8 segments use different code paths dependending
    %% on the the number of bytes available in the binary. Make sure
    %% we test both code paths.
    <<First/utf8,0:64>> = id(<<Bin/binary,0:64>>),
    <<0:3,First/utf8,0:64>> = id(<<0:3,Bin/binary,0:64>>),

    unaligned_match(First),

    Bin = id(<<First/utf8>>),
    do_utf8_roundtrip(First+1, Last);
do_utf8_roundtrip(_, _) -> ok.

unaligned_match(Char) ->
    %% We create a REFC binary so that we can create sub binaries
    %% and control the contents just beyond the end of the binary.
    _ = [begin
             Bin = id(<<0:64/unit:8,0:Offset,Char/utf8>>),
             <<0:64/unit:8,0:Offset,Char/utf8>> = Bin,
             unaligned_match(Bin, Offset, 8)
         end || Offset <- lists:seq(1, 7)],
    ok.

unaligned_match(_Bin, _Offset, 0) ->
    ok;
unaligned_match(Bin, Offset, N) ->
    Size = bit_size(Bin),
    <<Shorter:(Size-1)/bits,_:1>> = Bin,
    try
        <<0:64/unit:8,0:Offset,Char/utf8>> = Shorter,
        ct:fail({short_binary_accepted,Shorter,Char})
    catch
        error:{badmatch,_} ->
            unaligned_match(Shorter, Offset, N - 1)
    end.

utf16_roundtrip(Config) when is_list(Config) ->
    Big = fun utf16_big_roundtrip/1,
    Little = fun utf16_little_roundtrip/1,
    PidRefs = [spawn_monitor(fun() ->
                                     do_utf16_roundtrip(Fun)
                             end) || Fun <- [Big,Little]],
    [receive {'DOWN',Ref,process,Pid,Reason} -> normal=Reason end || {Pid,Ref} <- PidRefs],
    ok.

do_utf16_roundtrip(Fun) ->
    do_utf16_roundtrip(0, 16#D7FF, Fun),
    do_utf16_roundtrip(16#E000, 16#10FFFF, Fun).

do_utf16_roundtrip(First, Last, Fun) when First =< Last ->
    Fun(First),
    do_utf16_roundtrip(First+1, Last, Fun);
do_utf16_roundtrip(_, _, _) -> ok.

utf16_big_roundtrip(Char) ->
    Bin = id(<<Char/utf16>>),
    Bin = id(<<(id(<<>>))/binary,Char/utf16>>),
    Unaligned = id(<<3:2,Char/utf16>>),
    <<_:2,Bin/binary>> = Unaligned,
    <<Char/utf16>> = Bin,
    <<Char/utf16>> = make_unaligned(Bin),
    ok.

utf16_little_roundtrip(Char) ->
    Bin = id(<<Char/little-utf16>>),
    Bin = id(<<(id(<<>>))/binary,Char/little-utf16>>),
    Unaligned = id(<<3:2,Char/little-utf16>>),
    <<_:2,Bin/binary>> = Unaligned,
    <<Char/little-utf16>> = Bin,
    <<Char/little-utf16>> = make_unaligned(Bin),
    ok.

utf32_roundtrip(Config) when is_list(Config) ->
    Big = fun utf32_big_roundtrip/1,
    Little = fun utf32_little_roundtrip/1,
    PidRefs = [spawn_monitor(fun() ->
				     do_utf32_roundtrip(Fun)
			     end) || Fun <- [Big,Little]],
    [receive {'DOWN',Ref,process,Pid,Reason} -> normal=Reason end ||
	{Pid,Ref} <- PidRefs],
    ok.

do_utf32_roundtrip(Fun) ->
    do_utf32_roundtrip(0, 16#D7FF, Fun),
    do_utf32_roundtrip(16#E000, 16#10FFFF, Fun).

do_utf32_roundtrip(First, Last, Fun) when First =< Last ->
    Fun(First),
    do_utf32_roundtrip(First+1, Last, Fun);
do_utf32_roundtrip(_, _, _) -> ok.

utf32_big_roundtrip(Char) ->
    Bin = id(<<Char/utf32>>),
    Bin = id(<<(id(<<>>))/binary,Char/utf32>>),
    Unaligned = id(<<3:2,Char/utf32>>),
    <<_:2,Bin/binary>> = Unaligned,
    <<Char/utf32>> = Bin,
    <<Char/utf32>> = make_unaligned(Bin),
    ok.

utf32_little_roundtrip(Char) ->
    Bin = id(<<Char/little-utf32>>),
    Bin = id(<<(id(<<>>))/binary,Char/little-utf32>>),
    Unaligned = id(<<3:2,Char/little-utf32>>),
    <<_:2,Bin/binary>> = Unaligned,
    <<Char/little-utf32>> = Bin,
    <<Char/little-utf32>> = make_unaligned(Bin),
    ok.

utf8_illegal_sequences(Config) when is_list(Config) ->
    fail_range(16#10FFFF+1, 16#10FFFF+512), %Too large.
    fail_range(16#D800, 16#DFFF),		%Reserved for UTF-16.

    %% Illegal first character.
    [fail(<<I,16#8F,16#8F,16#8F>>) || I <- lists:seq(16#80, 16#BF)],

    %% Short sequences.
    short_sequences(16#80, 16#10FFFF),

    %% Overlong sequences. (Using more bytes than necessary
    %% is not allowed.)
    overlong(0, 127, 2),
    overlong(128, 16#7FF, 3),
    overlong(16#800, 16#FFFF, 4),
    ok.

fail_range(Char, End) when Char =< End ->
    {'EXIT',_} = (catch <<Char/utf8>>),
    Bin = int_to_utf8(Char),
    fail(Bin),
    fail(<<Bin/binary,0:64>>),
    fail_range(Char+1, End);
fail_range(_, _) -> ok.

short_sequences(Char, End) ->
    Step = (End - Char) div erlang:system_info(schedulers) + 1,
    PidRefs = short_sequences_1(Char, Step, End),
    [receive {'DOWN',Ref,process,Pid,Reason} -> normal=Reason end ||
	{Pid,Ref} <- PidRefs],
    ok.

short_sequences_1(Char, Step, End) when Char =< End ->
    CharEnd = lists:min([Char+Step-1,End]),
    [spawn_monitor(fun() ->
                           io:format("~p - ~p\n", [Char,CharEnd]),
                           do_short_sequences(Char, CharEnd)
                   end)|short_sequences_1(Char+Step, Step, End)];
short_sequences_1(_, _, _) -> [].

do_short_sequences(Char, End) when Char =< End ->
    short_sequence(Char),
    do_short_sequences(Char+1, End);
do_short_sequences(_, _) -> ok.

short_sequence(I) ->
    case int_to_utf8(I) of
	<<S0:3/binary,_:8>> ->
	    <<S1:2/binary,R1:8>> = S0,
	    <<S2:1/binary,_:8>> = S1,
	    fail(S0),
	    fail(S1),
	    fail(S2),
	    fail(<<S2/binary,16#7F,R1,R1>>),
	    fail(<<S1/binary,16#7F,R1>>),
	    fail(<<S0/binary,16#7F>>);
	<<S0:2/binary,_:8>> ->
	    <<S1:1/binary,R1:8>> = S0,
	    fail(S0),
	    fail(S1),
	    fail(<<S0/binary,16#7F>>),
	    fail(<<S1/binary,16#7F>>),
	    fail(<<S1/binary,16#7F,R1>>);
	<<S:1/binary,_:8>> ->
	    fail(S),
	    fail(<<S/binary,16#7F>>)
    end.

overlong(Char, Last, NumBytes) when Char =< Last ->
    overlong(Char, NumBytes),
    overlong(Char+1, Last, NumBytes);
overlong(_, _, _) -> ok.

overlong(Char, NumBytes) when NumBytes < 5 ->
    Bin = int_to_utf8(Char, NumBytes),
    case <<(int_to_utf8(Char, NumBytes))/binary>> of
	<<Char/utf8>>=Bin ->
	    ct:fail({illegal_encoding_accepted,Bin,Char});
	<<OtherChar/utf8>>=Bin ->
	    ct:fail({illegal_encoding_accepted,Bin,Char,OtherChar});
	_ -> ok
    end,
    case <<(int_to_utf8(Char, NumBytes))/binary,0:64>> of
	<<Char/utf8,0:64>>=Bin2 ->
	    ct:fail({illegal_encoding_accepted,Bin2,Char});
	<<OtherChar2/utf8,0:64>>=Bin2 ->
	    ct:fail({illegal_encoding_accepted,Bin2,Char,OtherChar2});
	_ -> ok
    end,
    overlong(Char, NumBytes+1);
overlong(_, _) -> ok.

fail(Bin) ->
    fail_1(Bin),
    fail_1(make_unaligned(Bin)),
    BinExt = <<Bin/binary,0:64>>,
    fail_2(BinExt),
    fail_2(make_unaligned(BinExt)).

fail_1(<<Char/utf8>>=Bin) ->
    ct:fail({illegal_encoding_accepted,Bin,Char});
fail_1(_) -> ok.

fail_2(<<Char/utf8,0:64>>=Bin) ->
    ct:fail({illegal_encoding_accepted,Bin,Char});
fail_2(_) -> ok.


utf16_illegal_sequences(Config) when is_list(Config) ->
    utf16_fail_range(16#10FFFF+1, 16#10FFFF+512), %Too large.
    utf16_fail_range(16#D800, 16#DFFF),	          %Reserved for UTF-16.

    lonely_hi_surrogate(16#D800, 16#DFFF),
    leading_lo_surrogate(16#DC00, 16#DFFF),
    
    ok.

utf16_fail_range(Char, End) when Char =< End ->
    {'EXIT',_} = (catch <<Char/big-utf16>>),
    {'EXIT',_} = (catch <<Char/little-utf16>>),
    utf16_fail_range(Char+1, End);
utf16_fail_range(_, _) -> ok.

lonely_hi_surrogate(Char, End) when Char =< End ->
    BinBig = <<Char:16/big>>,
    BinLittle = <<Char:16/little>>,
    case {BinBig,BinLittle} of
	{<<Bad/big-utf16>>,_} ->
	    ct:fail({lonely_hi_surrogate_accepted,Bad});
	{_,<<Bad/little-utf16>>} ->
	    ct:fail({lonely_hi_surrogate_accepted,Bad});
	{_,_} ->
	    ok
    end,
    lonely_hi_surrogate(Char+1, End);
lonely_hi_surrogate(_, _) -> ok.

leading_lo_surrogate(Char, End) when Char =< End ->
    leading_lo_surrogate(Char, 16#D800, 16#DFFF),
    leading_lo_surrogate(Char+1, End);
leading_lo_surrogate(_, _) -> ok.

leading_lo_surrogate(HiSurr, LoSurr, End) when LoSurr =< End ->
    BinBig = <<HiSurr:16/big,LoSurr:16/big>>,
    BinLittle = <<HiSurr:16/little,LoSurr:16/little>>,
    case {BinBig,BinLittle} of
	{<<Bad/big-utf16,_/bits>>,_} ->
	    ct:fail({leading_lo_surrogate_accepted,Bad});
	{_,<<Bad/little-utf16,_/bits>>} ->
	    ct:fail({leading_lo_surrogate_accepted,Bad});
	{_,_} ->
	    ok
    end,
    leading_lo_surrogate(HiSurr, LoSurr+1, End);
leading_lo_surrogate(_, _, _) -> ok.

utf32_illegal_sequences(Config) when is_list(Config) ->
    utf32_fail_range(16#10FFFF+1, 16#10FFFF+512), %Too large.
    utf32_fail_range(16#D800, 16#DFFF),		%Reserved for UTF-16.
    utf32_fail_range(-100, -1),

    <<>> = id(<< 0 || <<X/utf32>> <= <<"àxxx">>, _ = X >>),
    <<>> = id(<< 0 || <<X/little-utf32>> <= <<"àxxx">>, _ = X >>),

    ok.

utf32_fail_range(Char, End) when Char =< End ->
    {'EXIT',_} = (catch <<Char/big-utf32>>),
    {'EXIT',_} = (catch <<Char/little-utf32>>),
    case {<<Char:32>>,<<Char:32/little>>} of
        {<<Unexpected/utf32>>,_} ->
            ct:fail(Unexpected);
        {_,<<Unexpected/little-utf32>>} ->
            ct:fail(Unexpected);
        {_,_} -> ok
    end,
    utf32_fail_range(Char+1, End);
utf32_fail_range(_, _) -> ok.

bad_construction(Config) when is_list(Config) ->
    ?FAIL(<<3.14/utf8>>),
    ?FAIL(<<3.1415/utf16>>),
    ?FAIL(<<3.1415/utf32>>),
    {'EXIT',_} = (catch <<(id(3.14))/utf8>>),
    {'EXIT',_} = (catch <<(id(3.1415))/utf16>>),
    {'EXIT',_} = (catch <<(id(3.1415))/utf32>>),

    ?FAIL(<<(-1)/utf8>>),
    ?FAIL(<<(-1)/utf16>>),
    {'EXIT',_} = (catch <<(id(-1))/utf8>>),
    {'EXIT',_} = (catch <<(id(-1))/utf16>>),
    {'EXIT',_} = (catch <<(id(-1))/utf32>>),

    ?FAIL(<<16#D800/utf8>>),
    ?FAIL(<<16#D800/utf16>>),
    ?FAIL(<<16#D800/utf32>>),
    {'EXIT',_} = (catch <<(id(16#D800))/utf8>>),
    {'EXIT',_} = (catch <<(id(16#D800))/utf16>>),
    {'EXIT',_} = (catch <<(id(16#D800))/utf32>>),

    ok.

utf8_big_file(Config) ->
    DataDir = get_data_dir(Config),
    {ok, Bin} = file:read_file(filename:join(DataDir, "NormalizationTest.txt")),
    List = unicode:characters_to_list(Bin),
    _ = [begin
             io:format("~p\n", [Offset]),
             <<0:Offset, Rest/binary>> = id(<<0:Offset, Bin/binary>>),
             List = [Char || <<Char/utf8>> <= Rest]
         end || Offset <- lists:seq(0, 8)],
    ok.

%% This function intentionally allows construction of
%% UTF-8 sequence in illegal ranges.
int_to_utf8(I) when I =< 16#7F ->
    <<I>>;
int_to_utf8(I) when I =< 16#7FF ->
    B2 = I,
    B1 = (I bsr 6),
    <<1:1,1:1,0:1,B1:5,1:1,0:1,B2:6>>;
int_to_utf8(I) when I =< 16#FFFF ->
    B3 = I,
    B2 = (I bsr 6),
    B1 = (I bsr 12),
    <<1:1,1:1,1:1,0:1,B1:4,1:1,0:1,B2:6,1:1,0:1,B3:6>>;
int_to_utf8(I) when I =< 16#3FFFFF ->
    B4 = I,
    B3 = (I bsr 6),
    B2 = (I bsr 12),
    B1 = (I bsr 18),
    <<1:1,1:1,1:1,1:1,0:1,B1:3,1:1,0:1,B2:6,1:1,0:1,B3:6,1:1,0:1,B4:6>>;
int_to_utf8(I) when I =< 16#3FFFFFF ->
    B5 = I,
    B4 = (I bsr 6),
    B3 = (I bsr 12),
    B2 = (I bsr 18),
    B1 = (I bsr 24),
    <<1:1,1:1,1:1,1:1,1:1,0:1,B1:2,1:1,0:1,B2:6,1:1,0:1,B3:6,1:1,0:1,B4:6,
     1:1,0:1,B5:6>>.

%% int_to_utf8(I, NumberOfBytes) -> Binary.
%%  This function can be used to construct overlong sequences.
int_to_utf8(I, 1) ->
    <<I>>;
int_to_utf8(I, 2) ->
    B2 = I,
    B1 = (I bsr 6),
    <<1:1,1:1,0:1,B1:5,1:1,0:1,B2:6>>;
int_to_utf8(I, 3) ->
    B3 = I,
    B2 = (I bsr 6),
    B1 = (I bsr 12),
    <<1:1,1:1,1:1,0:1,B1:4,1:1,0:1,B2:6,1:1,0:1,B3:6>>;
int_to_utf8(I, 4) ->
    B4 = I,
    B3 = (I bsr 6),
    B2 = (I bsr 12),
    B1 = (I bsr 18),
    <<1:1,1:1,1:1,1:1,0:1,B1:3,1:1,0:1,B2:6,1:1,0:1,B3:6,1:1,0:1,B4:6>>.

make_unaligned(Bin0) when is_binary(Bin0) ->
    Bin1 = <<0:3,Bin0/binary,31:5>>,
    Sz = byte_size(Bin0),
    <<0:3,Bin:Sz/binary,31:5>> = id(Bin1),
    Bin.

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

evaluate(Str, Vars) ->
    {ok,Tokens,_} =
	erl_scan:string(Str ++ " . "),
    {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
    case erl_eval:expr(Expr, Vars) of
	{value, Result, _} ->
	    Result
    end.

%% Retrieve the original data directory for cloned modules.
get_data_dir(Config) ->
    Data = proplists:get_value(data_dir, Config),
    Opts = [{return,list}],
    Suffixes = ["_no_opt_SUITE",
                "_r25_SUITE",
                "_stripped_types_SUITE"],
    lists:foldl(fun(Suffix, Acc) ->
                        Opts = [{return,list}],
                        re:replace(Acc, Suffix, "_SUITE", Opts)
                end, Data, Suffixes).

id(I) -> I.
