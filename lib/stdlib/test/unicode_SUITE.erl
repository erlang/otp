%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
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
-module(unicode_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,
	 utf8_illegal_sequences_bif/1,
	 utf16_illegal_sequences_bif/1,
	 random_lists/1,
	 roundtrips/1,
	 latin1/1,
	 exceptions/1,
	 binaries_errors_limit/1,
	 ex_binaries_errors_utf8/1,
	 ex_binaries_errors_utf16_little/1,
	 ex_binaries_errors_utf16_big/1,
	 ex_binaries_errors_utf32_little/1,
	 ex_binaries_errors_utf32_big/1,
         normalize/1
        ]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,20}}].

all() -> 
    [utf8_illegal_sequences_bif,
     utf16_illegal_sequences_bif, random_lists, roundtrips,
     latin1, exceptions,
     binaries_errors_limit,
     normalize,
     {group,binaries_errors}].

groups() -> 
    [{binaries_errors,[parallel],
      [ex_binaries_errors_utf8,
       ex_binaries_errors_utf16_little,
       ex_binaries_errors_utf16_big,
       ex_binaries_errors_utf32_little,
       ex_binaries_errors_utf32_big]}].

binaries_errors_limit(Config) when is_list(Config) ->
    setlimit(10),
    ex_binaries_errors_utf8(Config),
    setlimit(default),
    ok.

ex_binaries_errors_utf8(Config) when is_list(Config) ->
    %% Original smoke test, we should not forget the original offset...
    <<_:8,_:8,RR2/binary>> = <<$a,$b,164,165,$c>>,
    {error,[],<<164,165,$c>>} = unicode:characters_to_list(RR2),
    %% Now, try with longer binary (trapping)
    BrokenPart = list_to_binary(lists:seq(128,255)),
    BrokenSz = byte_size(BrokenPart),
    Seq255 = lists:seq(1,255),
    [ begin
	  OKList = lists:flatten(lists:duplicate(N,Seq255)),
	  OKBin = unicode:characters_to_binary(OKList),
	  OKLen = length(OKList),
	  %% Copy to avoid that the binary get's writable
	  PartlyBroken = binary:copy(<<OKBin/binary, BrokenPart/binary>>),
	  PBSz = byte_size(PartlyBroken),
	  {error,OKList,DeepBrokenPart} = 
	      unicode:characters_to_list(PartlyBroken),
	  BrokenPart = iolist_to_binary(DeepBrokenPart),
	  [ begin
		NewList = lists:nthtail(X, OKList),
		NewSz = byte_size(unicode:characters_to_binary(NewList)) + 
		    BrokenSz,
		Chomped = binary:part(PartlyBroken,PBSz - NewSz, NewSz),
		true = (binary:referenced_byte_size(Chomped) =:= PBSz),
		{error,NewList,DeepBrokenPart2} =  
		    unicode:characters_to_list(Chomped),
		BrokenPart = iolist_to_binary(DeepBrokenPart2)
	    end || X <- lists:seq(1,OKLen) ]
      end || N <- lists:seq(1,21,4) ],
    ok.

ex_binaries_errors_utf16_little(Config) when is_list(Config) ->
    ex_binaries_errors_utf16(little).

ex_binaries_errors_utf16_big(Config) when is_list(Config) ->
    ex_binaries_errors_utf16(big).

ex_binaries_errors_utf16(Endian) ->
    BrokenSeq = lists:seq(16#DC00, 16#DFFF),
    BrokenPart = case Endian of
		     little ->
			 << <<X:16/little>> || X <- BrokenSeq >>;
		     big ->
			 << <<X:16/big>> || X <- BrokenSeq >>
		 end,
    BrokenSz = byte_size(BrokenPart),
    Seq255 = lists:seq(1, 255),
    [ begin
	  OKList = lists:append(lists:duplicate(N, Seq255)),
	  OKBin = unicode:characters_to_binary(OKList, unicode, {utf16,Endian}),
	  PartlyBroken = iolist_to_binary([OKBin,BrokenPart]),
	  PBSz = byte_size(PartlyBroken),
	  {error,OKList,DeepBrokenPart} = 
	      unicode:characters_to_list(PartlyBroken, {utf16,Endian}),
	  BrokenPart = iolist_to_binary(DeepBrokenPart),
	  utf16_inner_loop(OKList, BrokenPart, BrokenSz,
			   PartlyBroken, PBSz, Endian)
      end || N <- lists:seq(1, 16, 3) ],
    ok.

utf16_inner_loop([_|List], BrokenPart, BrokenSz, PartlyBroken, PBSz, Endian) ->
    Sz = length(List)*2 + BrokenSz,
    Chomped = binary:part(PartlyBroken, PBSz - Sz, Sz),
    true = binary:referenced_byte_size(Chomped) =:= PBSz,
    {error,List,DeepBrokenPart} =
	unicode:characters_to_list(Chomped, {utf16,Endian}),
    BrokenPart = iolist_to_binary(DeepBrokenPart),
    utf16_inner_loop(List, BrokenPart, BrokenSz, PartlyBroken, PBSz, Endian);
utf16_inner_loop([], _, _, _, _, _) ->
    ok.

ex_binaries_errors_utf32_big(Config) when is_list(Config) ->
    ex_binaries_errors_utf32(big).

ex_binaries_errors_utf32_little(Config) when is_list(Config) ->
    ex_binaries_errors_utf32(little).

ex_binaries_errors_utf32(Endian) ->
    BrokenSeq = lists:seq(16#DC00, 16#DFFF),
    BrokenPart = case Endian of
		     little ->
			 << <<X:32/little>> || X <- BrokenSeq >>;
		     big ->
			 << <<X:32/big>> || X <- BrokenSeq >>
		 end,
    BrokenSz = byte_size(BrokenPart),
    Seq255 = lists:seq(1, 255),
    [ begin
	  OKList = lists:append(lists:duplicate(N, Seq255)),
	  OKBin = unicode:characters_to_binary(OKList, unicode, {utf32,Endian}),
	  PartlyBroken = iolist_to_binary([OKBin,BrokenPart]),
	  PBSz = byte_size(PartlyBroken),
	  {error,OKList,DeepBrokenPart} = 
	      unicode:characters_to_list(PartlyBroken, {utf32,Endian}),
	  BrokenPart = iolist_to_binary(DeepBrokenPart),
	  utf32_inner_loop(OKList, BrokenPart, BrokenSz,
			   PartlyBroken, PBSz, Endian)
      end || N <- lists:seq(1, 16, 3) ],
    ok.

utf32_inner_loop([_|List], BrokenPart, BrokenSz, PartlyBroken, PBSz, Endian) ->
    Sz = length(List)*4 + BrokenSz,
    Chomped = binary:part(PartlyBroken, PBSz - Sz, Sz),
    true = binary:referenced_byte_size(Chomped) =:= PBSz,
    {error,List,DeepBrokenPart} =
	unicode:characters_to_list(Chomped, {utf32,Endian}),
    BrokenPart = iolist_to_binary(DeepBrokenPart),
    utf32_inner_loop(List, BrokenPart, BrokenSz, PartlyBroken, PBSz, Endian);
utf32_inner_loop([], _, _, _, _, _) ->
    ok.

exceptions(Config) when is_list(Config) ->
    setlimit(10),
    ex_exceptions(Config),
    setlimit(default),
    ex_exceptions(Config).

ex_exceptions(Config) when is_list(Config) ->
    L = lists:seq(0,255),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L++255,unicode)),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary({1,2,3},unicode)),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(1,unicode)),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(1.0,unicode)),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary('1',unicode)),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary([1,2,3,apa],unicode)),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary([1,2,3,4.0],unicode)),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L++255,latin1)),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary({1,2,3},latin1)),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(1,latin1)),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(1.0,latin1)),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary('1',latin1)),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary([1,2,3,apa],latin1)),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary([1,2,3,4.0],latin1)),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,gnarfl)),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,L)),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,{latin1})),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,[latin1])),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,1)),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,1.0)),
    Encodings = [unicode, utf8,utf16,utf32,{utf16,big},
		 {utf16,little},{utf32,big},{utf32,little}],
    [ begin
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L++255,unicode,Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary({1,2,3},unicode,Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(1,unicode,Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(1.0,unicode,Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary('1',unicode,Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary([1,2,3,apa],unicode,
								    Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary([1,2,3,4.0],unicode,
								    Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L++255,latin1,Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary({1,2,3},latin1,Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(1,latin1,Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(1.0,latin1,Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary('1',latin1,Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary([1,2,3,apa],latin1,
								    Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary([1,2,3,4.0],latin1,
								    Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,gnarfl,Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,L,Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,{latin1},Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,[latin1],Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,1,Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,1.0,Enc))
      end || Enc <- Encodings ],


    Encodings2 = [latin1, unicode, utf8,utf16,utf32,{utf16,big},
		  {utf16,little},{utf32,big},{utf32,little}],
    [ begin
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_list(L++255,Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_list({1,2,3},Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_list(1,Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_list(1.0,Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_list('1',Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_list([1,2,3,apa],Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_list([1,2,3,4.0],Enc)),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_list(L,{Enc})),
	  {'EXIT',{badarg,_}} = (catch unicode:characters_to_list(L,[Enc]))
      end || Enc <- Encodings2 ],
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_list(L,gnarfl)),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_list(L,L)),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_list(L,1)),
    {'EXIT',{badarg,_}} = (catch unicode:characters_to_list(L,1.0)),
    [ begin
	  Bx = unicode:characters_to_binary(L,latin1, Enc),
	  L = unicode:characters_to_list(Bx,Enc)
      end || Enc <- Encodings ],
    B = unicode:characters_to_binary(L,latin1),
    L = unicode:characters_to_list(B,unicode),
    L = unicode:characters_to_list(list_to_binary(L),latin1),
    More = <<B/binary,0,1,2>>,
    B2 = list_to_binary([254,255]),
    B3 = list_to_binary([0,1,2,254,255]),
    {error,B,Rest1} = unicode:characters_to_binary([L,B2],unicode),
    B2 = iolist_to_binary(Rest1),
    {error,More,Rest2} = unicode:characters_to_binary([L,B3],unicode),
    [ begin {error,_,_} = unicode:characters_to_binary([L,B2],unicode,Enc) end
      || Enc <- Encodings ],
    Valid0 = unicode:characters_to_binary([L,254,255],unicode),
    Valid1 = unicode:characters_to_binary([L,254,255],latin1),
    Valid2 = unicode:characters_to_binary([L,254,255,256,257],unicode),
    Valid3 = unicode:characters_to_binary([L,B2],latin1),
    true = is_binary(Valid0),
    true = is_binary(Valid1),
    true = is_binary(Valid2),
    true = is_binary(Valid3),
    Valid4 = unicode:characters_to_binary([L,B3],latin1),
    true = is_binary(Valid4),
    B2 = iolist_to_binary(Rest2),
    true = (L ++ [254,255] =:=  unicode:characters_to_list(Valid0,unicode)),
    true = (L ++ [254,255,256,257] =:=  unicode:characters_to_list(Valid2,unicode)),
    lists:foreach(fun(Enco) ->
			  Valid0x = unicode:characters_to_binary([L,254,255],unicode,Enco),
			  Valid1x = unicode:characters_to_binary([L,254,255],latin1,Enco),
			  Valid2x = unicode:characters_to_binary([L,254,255,256,257],unicode,Enco),
			  Valid3x = unicode:characters_to_binary([L,B2],latin1,Enco),
			  true = is_binary(Valid0x),
			  true = is_binary(Valid1x),
			  true = is_binary(Valid2x),
			  true = is_binary(Valid3x)

		  end, Encodings),
    ok.


latin1(Config) when is_list(Config) ->
    setlimit(10),
    ex_latin1(Config),
    setlimit(default),
    ex_latin1(Config).

ex_latin1(Config) when is_list(Config) ->
    All = lists:seq(0,255),
    AllBin = list_to_binary(All),
    AllUtf8 = unicode:characters_to_binary(All,latin1),
    AllUtf8 = unicode:characters_to_binary(AllBin,latin1),
    AllUtf8 = unicode:characters_to_binary([AllBin],latin1),
    AllUtf8 = unicode:characters_to_binary(make_unaligned(AllBin),latin1),
    AllUtf8 = unicode:characters_to_binary([make_unaligned(AllBin)],latin1),
    AllUtf8 = list_to_utf8_bsyntax([AllBin],latin1),
    AllUtf8 = list_to_utf8_bsyntax([make_unaligned(AllBin)],latin1),
    AllUtf8 = unicode_mixed_to_utf8_1(All),

    AllUtf16_Big = unicode:characters_to_binary(All,latin1,utf16),
    AllUtf16_Big = unicode:characters_to_binary(AllBin,latin1,utf16),
    AllUtf16_Big = unicode:characters_to_binary([AllBin],latin1,utf16),
    AllUtf16_Big = unicode:characters_to_binary(make_unaligned(AllBin),latin1,utf16),
    AllUtf16_Big = unicode:characters_to_binary([make_unaligned(AllBin)],latin1,utf16),
    AllUtf16_Big = list_to_utf16_big_bsyntax([AllBin],latin1),
    AllUtf16_Big = list_to_utf16_big_bsyntax([make_unaligned(AllBin)],latin1),

    AllUtf16_Little = unicode:characters_to_binary(All,latin1,{utf16,little}),
    AllUtf16_Little = unicode:characters_to_binary(AllBin,latin1,{utf16,little}),
    AllUtf16_Little = unicode:characters_to_binary([AllBin],latin1,{utf16,little}),
    AllUtf16_Little = unicode:characters_to_binary(make_unaligned(AllBin),latin1,
						   {utf16,little}),
    AllUtf16_Little = unicode:characters_to_binary([make_unaligned(AllBin)],latin1,
						   {utf16,little}),
    AllUtf16_Little = list_to_utf16_little_bsyntax([AllBin],latin1),
    AllUtf16_Little = list_to_utf16_little_bsyntax([make_unaligned(AllBin)],latin1),

    AllUtf32_Big = unicode:characters_to_binary(All,latin1,utf32),
    AllUtf32_Big = unicode:characters_to_binary(AllBin,latin1,utf32),
    AllUtf32_Big = unicode:characters_to_binary([AllBin],latin1,utf32),
    AllUtf32_Big = unicode:characters_to_binary(make_unaligned(AllBin),latin1,utf32),
    AllUtf32_Big = unicode:characters_to_binary([make_unaligned(AllBin)],latin1,utf32),
    AllUtf32_Big = list_to_utf32_big_bsyntax([AllBin],latin1),
    AllUtf32_Big = list_to_utf32_big_bsyntax([make_unaligned(AllBin)],latin1),

    AllUtf32_Little = unicode:characters_to_binary(All,latin1,{utf32,little}),
    AllUtf32_Little = unicode:characters_to_binary(AllBin,latin1,{utf32,little}),
    AllUtf32_Little = unicode:characters_to_binary([AllBin],latin1,{utf32,little}),
    AllUtf32_Little = unicode:characters_to_binary(make_unaligned(AllBin),latin1,
						   {utf32,little}),
    AllUtf32_Little = unicode:characters_to_binary([make_unaligned(AllBin)],latin1,
						   {utf32,little}),
    AllUtf32_Little = list_to_utf32_little_bsyntax([AllBin],latin1),
    AllUtf32_Little = list_to_utf32_little_bsyntax([make_unaligned(AllBin)],latin1),

    DoubleUtf8 = <<AllUtf8/binary,AllUtf8/binary>>,
    DoubleUtf8 = unicode:characters_to_binary([All,AllBin],latin1),
    DoubleUtf8 =
	unicode:characters_to_binary([All,make_unaligned(AllBin)],latin1),
    DoubleUtf8 = unicode:characters_to_binary([All|AllBin],latin1),
    DoubleUtf8 =
	unicode:characters_to_binary([All|make_unaligned(AllBin)],latin1),
    DoubleUtf8 = unicode:characters_to_binary([AllBin,All],latin1),
    DoubleUtf8 = unicode:characters_to_binary([AllBin|All],latin1),
    DoubleUtf8 = list_to_utf8_bsyntax([AllBin|All],latin1),

    DoubleUtf16 = <<AllUtf16_Big/binary,AllUtf16_Big/binary>>,
    DoubleUtf16 = unicode:characters_to_binary([All,AllBin],latin1,{utf16,big}),
    DoubleUtf16 =
	unicode:characters_to_binary([All,make_unaligned(AllBin)],latin1,{utf16,big}),
    DoubleUtf16 = unicode:characters_to_binary([All|AllBin],latin1,{utf16,big}),
    DoubleUtf16 =
	unicode:characters_to_binary([All|make_unaligned(AllBin)],latin1,{utf16,big}),
    DoubleUtf16 = unicode:characters_to_binary([AllBin,All],latin1,{utf16,big}),
    DoubleUtf16 = unicode:characters_to_binary([AllBin|All],latin1,{utf16,big}),
    DoubleUtf16 = list_to_utf16_big_bsyntax([AllBin|All],latin1),

    All = unicode:characters_to_list(AllUtf8,unicode),
    All = unicode:characters_to_list(make_unaligned(AllUtf8),unicode),
    All = utf8_to_list_bsyntax(AllUtf8),
    AllAll = All ++ All,
    AllAll = unicode:characters_to_list(DoubleUtf8,unicode),
    AllAll = unicode:characters_to_list(make_unaligned(DoubleUtf8),unicode),
    AllAll = utf8_to_list_bsyntax(DoubleUtf8),
    {error,AllUtf8,Rest1} =  unicode:characters_to_binary(All++[16#FFF],latin1),
    [16#FFF] = lists:flatten(Rest1),
    {error,DoubleUtf8,Rest2} =
	unicode:characters_to_binary([All,AllBin,16#FFF],latin1),
    {error,DoubleUtf16,Rest2x} =
	unicode:characters_to_binary([All,AllBin,16#FFF],latin1,utf16),
    [16#FFF] = lists:flatten(Rest2),
    [16#FFF] = lists:flatten(Rest2x),
    {error,AllUtf8,Rest3} =
	unicode:characters_to_binary([All,16#FFF,AllBin,16#FFF],
				     latin1),
    {error,AllUtf8,Rest3} =
	unicode:characters_to_binary([All,16#FFF,make_unaligned(AllBin),16#FFF],
				     latin1),
    {error,AllUtf16_Big,Rest3x} =
	unicode:characters_to_binary([All,16#FFF,AllBin,16#FFF],
				     latin1,{utf16,big}),
    {error,AllUtf16_Big,Rest3x} =
	unicode:characters_to_binary([All,16#FFF,make_unaligned(AllBin),16#FFF],
				     latin1,{utf16,big}),
    [16#FFF,AllBin,16#FFF] = lists:flatten(Rest3),
    [16#FFF,AllBin,16#FFF] = lists:flatten(Rest3x),
    DoubleSize = byte_size(DoubleUtf8),
    AllBut1 = DoubleSize - 1,
    AllBut2 = DoubleSize - 2,
    <<MissingLastByte:AllBut1/binary,_>> = DoubleUtf8,
    <<_:AllBut2/binary,MissingStart:1/binary,_>> = DoubleUtf8,
    {ChompedList,_} = lists:split(length(AllAll) - 1,AllAll),
    {incomplete,ChompedList,MissingStart} =
	unicode:characters_to_list(MissingLastByte,unicode),
    {incomplete,ChompedList,MissingStart} =
	unicode:characters_to_list(make_unaligned(MissingLastByte),unicode),

    DoubleSize16 = byte_size(DoubleUtf16),
    DoubleUtf16_2 = list_to_binary([DoubleUtf16,<<16#FFFFF/utf16-big>>]),
    DoubleSize16_2 = byte_size(DoubleUtf16_2),
    AllBut1_16 = DoubleSize16 - 1,
    AllBut2_16_2 = DoubleSize16_2 - 2,
    <<MissingLastBytes16:AllBut2_16_2/binary,_,_>> = DoubleUtf16_2,
    <<MissingLastByte16:AllBut1_16/binary,_>> = DoubleUtf16,
    {incomplete,AllAll,_} =
	unicode:characters_to_list(MissingLastBytes16,utf16),
    {incomplete,AllAll,_} =
	unicode:characters_to_list(make_unaligned(MissingLastBytes16),utf16),
    {incomplete,ChompedList,_} =
	unicode:characters_to_list(MissingLastByte16,utf16),
    {incomplete,ChompedList,_} =
	unicode:characters_to_list(make_unaligned(MissingLastByte16),utf16),
    ok.

roundtrips(Config) when is_list(Config) ->
    setlimit(10),
    ex_roundtrips(Config),
    setlimit(default),
    ex_roundtrips(Config).

ex_roundtrips(Config) when is_list(Config) ->
    L1 = ranges(0, 16#D800 - 1,
		erlang:system_info(context_reductions) * 11),
    L2 = ranges(16#DFFF + 1, 16#10000 - 1,
		erlang:system_info(context_reductions) * 11),
    L3 = ranges(16#FFFFF, 16#10FFFF,
		erlang:system_info(context_reductions) * 11),
    L = L1 ++ L2 ++ L3,
    LLen = length(L),
    Parts = erlang:system_info(schedulers),
    Lists = splitup(L,LLen,Parts),
    PidRefs = [spawn_monitor(fun() ->
				     do_roundtrips(MyPart)
			     end) || MyPart <- Lists],
    [receive {'DOWN',Ref,process,Pid,Reason} -> normal=Reason end ||
	{Pid,Ref} <- PidRefs],
    ok.

do_roundtrips([]) ->
    ok;
do_roundtrips([{Start,Stop}|T]) ->
    erlang:display({Start,Stop}),
    List = lists:seq(Start,Stop),
    Utf = unicode:characters_to_binary(List,unicode),
    Utf16_Big = unicode:characters_to_binary(List,unicode,{utf16,big}),
    Utf16_Little = unicode:characters_to_binary(List,unicode,{utf16,little}),
    Utf32_Big = unicode:characters_to_binary(List,unicode,{utf32,big}),
    Utf32_Little = unicode:characters_to_binary(List,unicode,{utf32,little}),

    Utf = unicode:characters_to_binary([Utf],unicode),
    Utf16_Big = unicode:characters_to_binary([Utf16_Big],{utf16,big},{utf16,big}),
    Utf16_Little = unicode:characters_to_binary([Utf16_Little],{utf16,little},{utf16,little}),
    Utf32_Big = unicode:characters_to_binary([Utf32_Big],{utf32,big},{utf32,big}),
    Utf32_Little = unicode:characters_to_binary([Utf32_Little],{utf32,little},{utf32,little}),

    Utf = list_to_utf8_bsyntax(List,unicode),
    Utf16_Big = list_to_utf16_big_bsyntax(List,{utf16,big}),
    Utf16_Little = list_to_utf16_little_bsyntax(List,{utf16,little}),
    Utf32_Big = list_to_utf32_big_bsyntax(List,{utf32,big}),
    Utf32_Little = list_to_utf32_little_bsyntax(List,{utf32,little}),

    Utf = unicode_mixed_to_utf8_1(List),

    List = unicode:characters_to_list(Utf,unicode),
    List = unicode:characters_to_list(Utf16_Big,{utf16,big}),
    List = unicode:characters_to_list(Utf16_Little,{utf16,little}),
    List = unicode:characters_to_list(Utf32_Big,{utf32,big}),
    List = unicode:characters_to_list(Utf32_Little,{utf32,little}),
    List = utf8_to_list_bsyntax(Utf),
    List = utf16_big_to_list_bsyntax(Utf16_Big),
    List = utf16_little_to_list_bsyntax(Utf16_Little),
    List = utf32_big_to_list_bsyntax(Utf32_Big),
    List = utf32_little_to_list_bsyntax(Utf32_Little),
    List = utf8_to_list(Utf),
    List = utf16_big_to_list(Utf16_Big),
    List = utf16_little_to_list(Utf16_Little),
    List = utf32_big_to_list(Utf32_Big),
    List = utf32_little_to_list(Utf32_Little),
    do_roundtrips(T).


random_lists(Config) when is_list(Config) ->
    setlimit(10),
    ex_random_lists(Config),
    setlimit(default),
    ex_random_lists(Config).
ex_random_lists(Config) when is_list(Config) ->
    PlainFlatten1 = fun(L) ->
			    unicode:characters_to_binary(flat(L),latin1)
		    end,
    PlainFlatten2 = fun(L) ->
			    unicode:characters_to_binary(L,latin1)
		    end,
    PlainFlatten3 = fun(L) ->
			    unicode:characters_to_binary(flatb(L),latin1)
		    end,
    PlainFlatten4 = fun(L) ->
			    iolist_to_binary([int_to_utf8(X) || X <- unicode:characters_to_list(flatb(L),latin1)])
		    end,
    random_iolist:run(150, PlainFlatten1, PlainFlatten3),
    random_iolist:run(150, PlainFlatten2, PlainFlatten3),
    random_iolist:run(150, PlainFlatten1, PlainFlatten2),
    random_iolist:run(150, PlainFlatten1, PlainFlatten4),
    SelfMade = fun(L) ->
		       iolist_to_binary(lists:map(fun(X) ->
							  int_to_utf8(X)
						  end,
						  flatb(L)))
	       end,
    SelfMadeA = fun(L) -> 
			case (catch list_to_utf8_bsyntax(L,latin1)) of
			    {'EXIT', Reason} ->
				io:format("Exit: ~p (~w)~n",[Reason,L]),
				exit(Reason);
			    Other ->
				Other
			end
		end,
    random_iolist:run(150, PlainFlatten1, SelfMade),
    random_iolist:run(150, PlainFlatten2, SelfMadeA),

    RoundTrip11 = fun(L) ->
			  unicode:characters_to_list(unicode:characters_to_binary(L,latin1),unicode)
		  end,
    RoundTrip21 = fun(L) ->
			  utf8_to_list_bsyntax(unicode:characters_to_binary(L,latin1))
		  end,
    RoundTrip31 = fun(L) ->
			  unicode:characters_to_list(list_to_utf8_bsyntax(L,latin1),unicode)
		  end,
    RoundTrip41 = fun(L) ->
			  utf8_to_list_bsyntax(list_to_utf8_bsyntax(L,latin1))
		  end,
    RoundTrip51 = fun(L) ->
			  unicode:characters_to_list(L,latin1)
		  end,
    random_iolist:run(150, RoundTrip11,RoundTrip21),
    random_iolist:run(150, RoundTrip21,RoundTrip31),
    random_iolist:run(150, RoundTrip31,RoundTrip41),
    random_iolist:run(150, RoundTrip11,RoundTrip41),
    random_iolist:run(150, RoundTrip21,RoundTrip41),
    random_iolist:run(150, RoundTrip11,RoundTrip31),
    random_iolist:run(150, RoundTrip11,RoundTrip51),


    UniFlatten1 = fun(L) ->
			  unicode:characters_to_binary(flat(L),unicode)
		  end,
    UniFlatten2 = fun(L) ->
			  unicode:characters_to_binary(L,unicode)
		  end,
    UniFlatten3 = fun(L) ->
			  unicode:characters_to_binary(flatx(L),unicode)
		  end,
    UniFlatten4 = fun(L) ->
			  unicode:characters_to_binary(unicode:characters_to_list(L,unicode),unicode)
		  end,
    random_unicode_list:run(150, UniFlatten1,UniFlatten2),
    random_unicode_list:run(150, UniFlatten1,UniFlatten3),
    random_unicode_list:run(150, UniFlatten2,UniFlatten4),
    random_unicode_list:run(150, UniFlatten2,UniFlatten3),

    Encodings = [utf8,{utf16,big},
		 {utf16,little},{utf32,big},{utf32,little}],
    lists:foreach(fun(OutEnc1) ->
			  lists:foreach(fun(InEnc1) -> 
						Uni16BigFlatten1 = fun(L) ->
									   unicode:characters_to_binary(flat(L),InEnc1,OutEnc1)
								   end,
						Uni16BigFlatten2 = fun(L) ->
									   unicode:characters_to_binary(L,InEnc1,OutEnc1)
								   end,
						Uni16BigFlatten3 = fun(L) ->
									   unicode:characters_to_binary(flatx(L),InEnc1,OutEnc1)
								   end,
						Uni16BigFlatten4 = fun(L) ->
									   unicode:characters_to_binary(unicode:characters_to_list(L,InEnc1),InEnc1,OutEnc1)
								   end,
						random_unicode_list:run(150, Uni16BigFlatten1,Uni16BigFlatten2,InEnc1),
						random_unicode_list:run(150, Uni16BigFlatten1,Uni16BigFlatten3,InEnc1),
						random_unicode_list:run(150, Uni16BigFlatten2,Uni16BigFlatten4,InEnc1),
						random_unicode_list:run(150, Uni16BigFlatten2,Uni16BigFlatten3,InEnc1)
					end, Encodings)
		  end, Encodings),
    SelfMade1 = fun(L) ->
			unicode_mixed_to_utf8_1(L)
		end,
    SelfMade2 = fun(L) ->
			unicode_mixed_to_utf8_2(L)
		end,
    SelfMade3 = fun(L) ->
			list_to_utf8_bsyntax(L,unicode)
		end,
    random_unicode_list:run(150, SelfMade1,SelfMade2),
    random_unicode_list:run(150, UniFlatten2, SelfMade1),
    random_unicode_list:run(150, UniFlatten2, SelfMade2),
    random_unicode_list:run(150, UniFlatten2, SelfMade3),
    RoundTrip1 = fun(L) ->
			 unicode:characters_to_list(unicode:characters_to_binary(L,unicode),unicode)
		 end,
    RoundTrip2 = fun(L) ->
			 utf8_to_list_bsyntax(unicode:characters_to_binary(L,unicode))
		 end,
    RoundTrip3 = fun(L) ->
			 unicode:characters_to_list(list_to_utf8_bsyntax(L,unicode),unicode)
		 end,
    RoundTrip4 = fun(L) ->
			 utf8_to_list_bsyntax(list_to_utf8_bsyntax(L,unicode))
		 end,
    random_unicode_list:run(150, RoundTrip1,RoundTrip2),
    random_unicode_list:run(150, RoundTrip2,RoundTrip3),
    random_unicode_list:run(150, RoundTrip3,RoundTrip4),
    random_unicode_list:run(150, RoundTrip1,RoundTrip4),
    random_unicode_list:run(150, RoundTrip2,RoundTrip4),
    random_unicode_list:run(150, RoundTrip1,RoundTrip3),
    lists:foreach(fun(OutEnc2) ->
			  lists:foreach(fun(InEnc2) ->
						RoundTripUtf16_Big_1 = fun(L) ->
									       unicode:characters_to_list(unicode:characters_to_binary(L,InEnc2,OutEnc2),OutEnc2)
								       end,
						RoundTripUtf16_Big_2 = fun(L) ->
									       x_to_list_bsyntax(OutEnc2,unicode:characters_to_binary(L,InEnc2,OutEnc2))
								       end,
						RoundTripUtf16_Big_3 = fun(L) ->
									       unicode:characters_to_list(list_to_x_bsyntax(InEnc2,L,InEnc2),InEnc2)
								       end,
						RoundTripUtf16_Big_4 = fun(L) ->
									       x_to_list_bsyntax(InEnc2,list_to_x_bsyntax(InEnc2,L,InEnc2))
								       end,
						random_unicode_list:run(150, RoundTripUtf16_Big_1,RoundTripUtf16_Big_2,InEnc2),
						random_unicode_list:run(150, RoundTripUtf16_Big_2,RoundTripUtf16_Big_3,InEnc2),
						random_unicode_list:run(150, RoundTripUtf16_Big_3,RoundTripUtf16_Big_4,InEnc2),
						random_unicode_list:run(150, RoundTripUtf16_Big_1,RoundTripUtf16_Big_4,InEnc2),
						random_unicode_list:run(150, RoundTripUtf16_Big_2,RoundTripUtf16_Big_4,InEnc2),
						random_unicode_list:run(150, RoundTripUtf16_Big_1,RoundTripUtf16_Big_3,InEnc2)
					end, Encodings)
		  end, Encodings),
    ToList1 = fun(L) ->
		      unicode:characters_to_list(L,unicode)
	      end,
    ToList2 = fun(L) ->
		      unicode:characters_to_list(unicode:characters_to_binary(L,unicode),unicode)
	      end,
    ToList3 = fun(L) ->
		      unicode:characters_to_list(unicode_mixed_to_utf8_2(L),unicode)
	      end,
    ToList4 = fun(L) ->
		      utf8_to_list(unicode_mixed_to_utf8_2(L))
	      end,
    random_unicode_list:run(150, ToList1,ToList2),
    random_unicode_list:run(150, ToList2,ToList3),
    random_unicode_list:run(150, ToList3,ToList4),
    random_unicode_list:run(150, ToList1,ToList4),
    random_unicode_list:run(150, ToList2,ToList4),
    random_unicode_list:run(150, ToList1,ToList3),

    ok.

utf16_illegal_sequences_bif(Config) when is_list(Config) ->
    setlimit(10),
    ex_utf16_illegal_sequences_bif(Config),
    setlimit(default),
    ex_utf16_illegal_sequences_bif(Config).

ex_utf16_illegal_sequences_bif(Config) when is_list(Config) ->
    utf16_fail_range_bif_simple(16#10FFFF+1, 16#10FFFF+512), %Too large.
    utf16_fail_range_bif(16#D800, 16#DFFF),		%Reserved for UTF-16.

    lonely_hi_surrogate_bif(16#D800, 16#DBFF,incomplete),
    lonely_hi_surrogate_bif(16#DC00, 16#DFFF,error),
    leading_lo_surrogate_bif(16#DC00, 16#DFFF),

    ok.

utf16_fail_range_bif(Char, End) when Char =< End ->
    {error,_,_} = unicode:characters_to_binary([Char],{utf16,big}),
    BigBin = int_to_utf16_big(Char),
    fail_bif(BigBin,{utf16,big}),
    {error,_,_} = unicode:characters_to_binary([Char],{utf16,little}),
    LittleBin = int_to_utf16_little(Char),
    fail_bif(LittleBin,{utf16,little}),
    utf16_fail_range_bif(Char+1, End);
utf16_fail_range_bif(_, _) -> ok.

utf16_fail_range_bif_simple(Char, End) when Char =< End ->
    {error,_,_} = unicode:characters_to_binary([Char],{utf16,big}),
    {error,_,_} = unicode:characters_to_binary([Char],{utf16,little}),
    utf16_fail_range_bif_simple(Char+1, End);
utf16_fail_range_bif_simple(_, _) -> ok.


lonely_hi_surrogate_bif(Char, End, EType) when Char =< End ->
    BinBig = <<Char:16/big>>,
    BinLittle = <<Char:16/little>>,
    case unicode:characters_to_binary(BinBig,{utf16,big}) of
	{EType,_,_} -> 
	    ok;
	Other ->
	    exit({lonely_hi_surrogate_accepted,BinBig,{utf16,big},Other})
    end,
    case unicode:characters_to_binary(BinLittle,{utf16,little}) of
	{EType,_,_} -> 
	    ok;
	Other2 ->
	    exit({lonely_hi_surrogate_accepted,BinLittle,{utf16,little},Other2})
    end,
    lonely_hi_surrogate_bif(Char+1, End, EType);
lonely_hi_surrogate_bif(_, _, _) -> ok.

leading_lo_surrogate_bif(Char, End) when Char =< End ->
    leading_lo_surrogate_bif(Char, 16#D800, 16#DFFF),
    leading_lo_surrogate_bif(Char+1, End);
leading_lo_surrogate_bif(_, _) -> ok.

leading_lo_surrogate_bif(HiSurr, LoSurr, End) when LoSurr =< End ->
    BinBig = <<HiSurr:16/big,LoSurr:16/big>>,
    BinLittle = <<HiSurr:16/little,LoSurr:16/little>>,
    case unicode:characters_to_binary(BinBig,{utf16,big}) of
	{error,_,_} -> 
	    ok;
	Other ->
	    exit({leading_lo_surrogate_accepted,BinBig,{utf16,big},Other})
    end,
    case unicode:characters_to_binary(BinLittle,{utf16,little}) of
	{error,_,_} -> 
	    ok;
	Other2 ->
	    exit({leading_lo_surrogate_accepted,BinLittle,{utf16,little},Other2})
    end,
    leading_lo_surrogate_bif(HiSurr, LoSurr+1, End);
leading_lo_surrogate_bif(_, _, _) -> ok.

utf8_illegal_sequences_bif(Config) when is_list(Config) ->
    ct:timetrap({minutes,40}), %% valgrind needs a lot
    setlimit(10),
    ex_utf8_illegal_sequences_bif(Config),
    setlimit(default),
    ex_utf8_illegal_sequences_bif(Config).

ex_utf8_illegal_sequences_bif(Config) when is_list(Config) ->
    fail_range_bif(16#10FFFF+1, 16#10FFFF+512), %Too large.
    fail_range_bif(16#D800, 16#DFFF),		%Reserved for UTF-16.

    %% Illegal first character.
    [fail_bif(<<I,16#8F,16#8F,16#8F>>,unicode) || I <- lists:seq(16#80, 16#BF)],

    %% Short sequences.
    short_sequences_bif(16#80, 16#10FFFF),

    %% Overlong sequences. (Using more bytes than necessary
    %% is not allowed.)
    overlong_bif(0, 127, 2),
    overlong_bif(128, 16#7FF, 3),
    overlong_bif(16#800, 16#FFFF, 4),
    ok.

fail_range_bif(Char, End) when Char =< End ->
    {error,_,_} = unicode:characters_to_binary([Char],unicode),
    {error,_,_} = unicode:characters_to_binary([Char],unicode,utf16),
    {error,_,_} = unicode:characters_to_binary([Char],unicode,utf32),
    Bin = int_to_utf8(Char),
    fail_bif(Bin,unicode),
    fail_range_bif(Char+1, End);
fail_range_bif(_, _) -> ok.

short_sequences_bif(Char, End) ->
    Step = (End - Char) div erlang:system_info(schedulers) + 1,
    PidRefs = short_sequences_bif_1(Char, Step, End),
    [receive {'DOWN',Ref,process,Pid,Reason} -> normal=Reason end ||
	{Pid,Ref} <- PidRefs],
    ok.

short_sequences_bif_1(Char, Step, End) when Char =< End ->
    CharEnd = lists:min([Char+Step-1,End]),
    [spawn_monitor(fun() ->
			   io:format("~p - ~p\n", [Char,CharEnd]),
			   do_short_sequences_bif(Char, CharEnd)
		   end)|short_sequences_bif_1(Char+Step, Step, End)];
short_sequences_bif_1(_, _, _) -> [].

do_short_sequences_bif(Char, End) when Char =< End ->
    short_sequence_bif(Char),
    do_short_sequences_bif(Char+1, End);
do_short_sequences_bif(_, _) -> ok.

short_sequence_bif(I) ->
    case int_to_utf8(I) of
	<<S0:3/binary,_:8>> ->
	    <<S1:2/binary,R1:8>> = S0,
	    <<S2:1/binary,_:8>> = S1,
	    incomplete_bif(S0,S0),
	    incomplete_bif(S1,S1),
	    incomplete_bif(S2,S2),
	    only_fail_bif(<<S2/binary,16#7F,R1,R1>>,unicode),
	    only_fail_bif(<<S1/binary,16#7F,R1>>,unicode),
	    only_fail_bif(<<S0/binary,16#7F>>,unicode);
	<<S0:2/binary,_:8>> ->
	    <<S1:1/binary,R1:8>> = S0,
	    incomplete_bif(S0,S0),
	    incomplete_bif(S1,S1),
	    only_fail_bif(<<S0/binary,16#7F>>,unicode),
	    only_fail_bif(<<S1/binary,16#7F>>,unicode),
	    only_fail_bif(<<S1/binary,16#7F,R1>>,unicode);
	<<S:1/binary,_:8>> ->
	    incomplete_bif(S,S),
	    only_fail_bif(<<S/binary,16#7F>>,unicode)
    end.


overlong_bif(Char, Last, NumBytes) when Char =< Last ->
    overlong_bif(Char, NumBytes),
    overlong_bif(Char+1, Last, NumBytes);
overlong_bif(_, _, _) -> ok.

overlong_bif(Char, NumBytes) when NumBytes < 5 ->
    case unicode:characters_to_binary([int_to_utf8(Char, NumBytes)],unicode) of
	{error,_,_} -> 
	    ok;
	Other->
	    exit({illegal_encoding_accepted,Char,NumBytes,Other})
    end,
    overlong_bif(Char, NumBytes+1);
overlong_bif(_, _) -> ok.

incomplete_bif(Bin,Tail) ->
    incomplete_bif_1(Bin,Tail),
    incomplete_bif_1(make_unaligned(Bin),Tail),
    incomplete_bif_r_1(Bin,Tail),
    incomplete_bif_r_1(make_unaligned(Bin),Tail),
    ok.

incomplete_bif_1(Bin,Tail) ->
    case unicode:characters_to_binary([Bin],unicode) of
	{incomplete,_,Tail} ->
	    case unicode:characters_to_binary(Bin,unicode) of
		{incomplete,_,Tail} ->
		    ok;
		Other0 ->
		    exit({incomplete_encoding_accepted,Bin,Other0})
	    end;
	Other ->
	    exit({incomplete_encoding_accepted,[Bin],Other})
    end.
incomplete_bif_r_1(Bin,Tail) ->
    case unicode:characters_to_list([Bin],unicode) of
	{incomplete,_,Tail} ->
	    case unicode:characters_to_list(Bin,unicode) of
		{incomplete,_,Tail} ->
		    ok;
		Other ->
		    exit({incomplete_encoding_accepted_r,[Bin],Other})
	    end;
	Other ->
	    exit({incomplete_encoding_accepted_r,[Bin],Other})
    end.

only_fail_bif(Bin,Coding) ->
    only_fail_bif_1(Bin,Coding),
    only_fail_bif_1(make_unaligned(Bin),Coding),
    only_fail_bif_r_1(Bin,Coding),
    only_fail_bif_r_1(make_unaligned(Bin),Coding),
    ok.

only_fail_bif_r_1(Bin,Coding) ->
    case unicode:characters_to_list([Bin],Coding) of
	{error,_,_} ->
	    case unicode:characters_to_list(Bin,Coding) of
		{error,_,_} ->
		    ok;
		Other ->
		    exit({faulty_encoding_accepted_r,Bin,Coding,Other})
	    end;
	Other ->
	    exit({faulty_encoding_accepted_r,Bin,Coding,Other})
    end.
only_fail_bif_1(Bin,Coding) ->
    case unicode:characters_to_binary([Bin],Coding) of
	{error,_,_} ->
	    case unicode:characters_to_binary(Bin,Coding) of
		{error,_,_} ->
		    ok;
		Other0 ->
		    exit({faulty_encoding_accepted,Bin,Coding,Other0})
	    end;
	Other ->
	    exit({faulty_encoding_accepted,[Bin],Coding,Other})
    end.




fail_bif(Bin,Coding) ->
    fail_bif_1(Bin,Coding),
    fail_bif_1(make_unaligned(Bin),Coding),
    fail_bif_r_1(Bin,Coding),
    fail_bif_r_1(make_unaligned(Bin),Coding),
    ok.
fail_bif_r_1(Bin,Coding) ->
    case unicode:characters_to_list(Bin,Coding) of
	L when is_list(L) ->
	    exit({illegal_encoding_accepted,Bin,Coding});
	_ ->
	    ok
    end.

fail_bif_1(Bin,Coding) ->
    case unicode:characters_to_binary([Bin],Coding) of
	Bin2 when is_binary(Bin2) ->
	    exit({illegal_encoding_accepted,Bin,Coding});
	_ ->
	    ok
    end.


normalize(_) ->
    %% More tests are in unicode_util_SUITE.erl and str_SUITE.erl
    {'EXIT', _} = (catch unicode:characters_to_nfc_list({tuple})),
    {'EXIT', _} = (catch unicode:characters_to_nfd_list({tuple})),
    {'EXIT', _} = (catch unicode:characters_to_nfkc_list({tuple})),
    {'EXIT', _} = (catch unicode:characters_to_nfkd_list({tuple})),
    {'EXIT', _} = (catch unicode:characters_to_nfc_binary({tuple})),
    {'EXIT', _} = (catch unicode:characters_to_nfd_binary({tuple})),
    {'EXIT', _} = (catch unicode:characters_to_nfkc_binary({tuple})),
    {'EXIT', _} = (catch unicode:characters_to_nfkd_binary({tuple})),
    String = ["abc..åäö", <<"Ωµe`è"/utf8>>, "œŒþæÆħ§ß ホンダ"],
    NFD_l = unicode:characters_to_nfd_list(String),
    NFD_b = unicode:characters_to_nfd_binary(String),
    NFC_l = unicode:characters_to_nfc_list(String),
    NFC_b = unicode:characters_to_nfc_binary(String),

    NFD_l = unicode:characters_to_nfd_list(NFD_l),
    NFD_l = unicode:characters_to_nfd_list(NFD_b),
    NFD_l = unicode:characters_to_nfd_list(NFC_l),
    NFD_l = unicode:characters_to_nfd_list(NFC_b),

    NFD_b = unicode:characters_to_nfd_binary(NFD_b),
    NFD_b = unicode:characters_to_nfd_binary(NFD_l),
    NFD_b = unicode:characters_to_nfd_binary(NFC_b),
    NFD_b = unicode:characters_to_nfd_binary(NFC_l),

    NFC_l = unicode:characters_to_nfc_list(NFD_l),
    NFC_l = unicode:characters_to_nfc_list(NFD_b),
    NFC_l = unicode:characters_to_nfc_list(NFC_l),
    NFC_l = unicode:characters_to_nfc_list(NFC_b),

    NFC_b = unicode:characters_to_nfc_binary(NFD_b),
    NFC_b = unicode:characters_to_nfc_binary(NFD_l),
    NFC_b = unicode:characters_to_nfc_binary(NFC_b),
    NFC_b = unicode:characters_to_nfc_binary(NFC_l),

    Str = [lists:duplicate(20,lists:seq($a, $q))|String],
    StrD_bin = unicode:characters_to_binary(unicode:characters_to_nfd_list(Str)),
    StrD_bin = unicode:characters_to_nfd_binary(Str),
    StrC_bin = unicode:characters_to_binary(unicode:characters_to_nfc_list(StrD_bin)),
    StrC_bin = unicode:characters_to_nfc_binary(Str),

    NFKD_l = unicode:characters_to_nfkd_list(String),
    NFKD_b = unicode:characters_to_nfkd_binary(String),
    NFKC_l = unicode:characters_to_nfkc_list(String),
    NFKC_b = unicode:characters_to_nfkc_binary(String),

    NFKD_l = unicode:characters_to_nfkd_list(NFKD_l),
    NFKD_l = unicode:characters_to_nfkd_list(NFKD_b),
    NFKD_l = unicode:characters_to_nfkd_list(NFKC_l),
    NFKD_l = unicode:characters_to_nfkd_list(NFKC_b),

    NFKD_b = unicode:characters_to_nfd_binary(NFKD_b),
    NFKD_b = unicode:characters_to_nfd_binary(NFKD_l),
    NFKD_b = unicode:characters_to_nfd_binary(NFKC_b),
    NFKD_b = unicode:characters_to_nfd_binary(NFKC_l),

    NFKC_l = unicode:characters_to_nfc_list(NFKD_l),
    NFKC_l = unicode:characters_to_nfc_list(NFKD_b),
    NFKC_l = unicode:characters_to_nfc_list(NFKC_l),
    NFKC_l = unicode:characters_to_nfc_list(NFKC_b),

    NFKC_b = unicode:characters_to_nfc_binary(NFKD_b),
    NFKC_b = unicode:characters_to_nfc_binary(NFKD_l),
    NFKC_b = unicode:characters_to_nfc_binary(NFKC_b),
    NFKC_b = unicode:characters_to_nfc_binary(NFKC_l),

    StrKD_bin = unicode:characters_to_binary(unicode:characters_to_nfkd_list(Str)),
    StrKD_bin = unicode:characters_to_nfkd_binary(Str),
    StrKC_bin = unicode:characters_to_binary(unicode:characters_to_nfkc_list(StrD_bin)),
    StrKC_bin = unicode:characters_to_nfkc_binary(Str),

    true = unicode:characters_to_nfkc_list("ホンダ") =:= unicode:characters_to_nfkc_list("ﾎﾝﾀﾞ"),
    true = unicode:characters_to_nfkd_list("32") =:= unicode:characters_to_nfkd_list("３２"),

    {error, [0], <<128>>} = unicode:characters_to_nfc_list(<<0, 128>>),
    {error, [0], <<128>>} = unicode:characters_to_nfkc_list(<<0, 128>>),
    {error, [0], <<128>>} = unicode:characters_to_nfd_list(<<0, 128>>),
    {error, [0], <<128>>} = unicode:characters_to_nfkd_list(<<0, 128>>),

    {error, <<0>>, <<128>>} = unicode:characters_to_nfc_binary(<<0, 128>>),
    {error, <<0>>, <<128>>} = unicode:characters_to_nfkc_binary(<<0, 128>>),
    {error, <<0>>, <<128>>} = unicode:characters_to_nfd_binary(<<0, 128>>),
    {error, <<0>>, <<128>>} = unicode:characters_to_nfkd_binary(<<0, 128>>),

    LargeBin = binary:copy(<<"abcde">>, 50),
    LargeList = binary_to_list(LargeBin),

    {error, LargeList, <<128>>} = unicode:characters_to_nfc_list(<<LargeBin/binary, 128>>),
    {error, LargeList, <<128>>} = unicode:characters_to_nfkc_list(<<LargeBin/binary, 128>>),
    {error, LargeList, <<128>>} = unicode:characters_to_nfd_list(<<LargeBin/binary, 128>>),
    {error, LargeList, <<128>>} = unicode:characters_to_nfkd_list(<<LargeBin/binary, 128>>),

    {error, LargeBin, <<128>>} = unicode:characters_to_nfc_binary(<<LargeBin/binary, 128>>),
    {error, LargeBin, <<128>>} = unicode:characters_to_nfkc_binary(<<LargeBin/binary, 128>>),
    {error, LargeBin, <<128>>} = unicode:characters_to_nfd_binary(<<LargeBin/binary, 128>>),
    {error, LargeBin, <<128>>} = unicode:characters_to_nfkd_binary(<<LargeBin/binary, 128>>),

    ok.


%%
%% Diverse utilities
%%

ranges(X,Y,_N) when X >= Y ->
    [];
ranges(X,Y,N) when X + N > Y ->
    [{X,Y}];
ranges(X,Y,N) ->
    Upper = X+N,
    [{X,Upper}|ranges(Upper+1,Y,N)].

splitup(L,_Len,1) ->
    [L];
splitup(L,Len,Parts) ->
    Num = Len div Parts,
    {A,B} = lists:split(Num,L),
    [A| splitup(B,Len - Num,Parts - 1)].

flat(List) ->
    lists:reverse(flat(List,[])).

flat([H|T],Acc) ->
    NewAcc = flat(H,Acc),
    flat(T,NewAcc);
flat([],Acc) ->
    Acc;
flat(X,Acc) ->
    [X|Acc].

flatb(List) ->
    lists:reverse(flatb(List,[])).

flatb(<<X:8,Rest/binary>>,Acc) ->
    flatb(Rest,[X|Acc]);
flatb(<<>>,Acc) ->
    Acc;
flatb([H|T],Acc) ->
    NewAcc = flatb(H,Acc),
    flatb(T,NewAcc);
flatb([],Acc) ->
    Acc;
flatb(X,Acc) ->
    [X|Acc].
flatx(List) ->
    lists:reverse(flatx(List,[])).

flatx([B1,B2|T],Acc) when is_binary(B1), is_binary(B2) ->
    flatx([<<B1/binary,B2/binary>>|T],Acc);
flatx([H|T],Acc) ->
    NewAcc = flatx(H,Acc),
    flatx(T,NewAcc);
flatx([],Acc) ->
    Acc;
flatx(X,Acc) ->
    [X|Acc].


unicode_mixed_to_utf8_1(L) ->
    Flist = flatx([L]),
    ExpList = [ case is_binary(E) of
		    true ->
			utf8_to_list(E);
		    false ->
			E
		end || E <- Flist ],
    iolist_to_binary([int_to_utf8(I) || I <- flat(ExpList)]).

unicode_mixed_to_utf8_2(L) ->
    Flist = flatx([L]),
    ExpList = [ case is_binary(E) of
		    true ->
			E;
		    false ->
			int_to_utf8(E)
		end || E <- Flist ],
    iolist_to_binary([ExpList]).




utf8_to_list_bsyntax(<<>>) ->
    [];
utf8_to_list_bsyntax(<<C/utf8,R/binary>>) ->
    [C|utf8_to_list_bsyntax(R)].

list_to_utf8_bsyntax(List,unicode) ->
    FList = flatx(List),
    list_to_binary([ if
			 is_binary(E) ->
			     E;
			 true ->
			     <<E/utf8>>
		     end || E <- FList ]);
list_to_utf8_bsyntax(List,latin1) ->
    FList = flatb(List),
    list_to_binary([ <<E/utf8>> || E <- FList ]).





%%
%% Conversion utilities
%%

int_to_utf16_big(U) when U < 16#10000 ->
    <<U:16/big>>;
int_to_utf16_big(U) when U >= 16#10000, U =< 16#10FFFF ->
    UPrim = U - 16#10000,
    HI = (16#D800 bor (UPrim bsr 10)),
    LO = (16#DC00 bor (UPrim band 16#3FF)),
    <<HI:16/big,LO:16/big>>.

int_to_utf16_little(U) when U < 16#10000 ->
    <<U:16/little>>;
int_to_utf16_little(U) when U >= 16#10000, U =< 16#10FFFF ->
    UPrim = U - 16#10000,
    HI = (16#D800 bor (UPrim bsr 10)),
    LO = (16#DC00 bor (UPrim band 16#3FF)),
    <<HI:16/little,LO:16/little>>.


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

utf16_big_to_list_bsyntax(<<>>) ->
    [];
utf16_big_to_list_bsyntax(<<C/utf16-big,R/binary>>) ->
    [C|utf16_big_to_list_bsyntax(R)].

list_to_utf16_big_bsyntax(List,{utf16,big}) ->
    FList = flatx(List),
    list_to_binary([ if
			 is_binary(E) ->
			     E;
			 true ->
			     <<E/utf16-big>>
		     end || E <- FList ]);
list_to_utf16_big_bsyntax(List,latin1) ->
    FList = flatb(List),
    list_to_binary([ <<E/utf16-big>> || E <- FList ]).


utf16_little_to_list_bsyntax(<<>>) ->
    [];
utf16_little_to_list_bsyntax(<<C/utf16-little,R/binary>>) ->
    [C|utf16_little_to_list_bsyntax(R)].

list_to_utf16_little_bsyntax(List,{utf16,little}) ->
    FList = flatx(List),
    list_to_binary([ if
			 is_binary(E) ->
			     E;
			 true ->
			     <<E/utf16-little>>
		     end || E <- FList ]);
list_to_utf16_little_bsyntax(List,latin1) ->
    FList = flatb(List),
    list_to_binary([ <<E/utf16-little>> || E <- FList ]).



utf32_big_to_list_bsyntax(<<>>) ->
    [];
utf32_big_to_list_bsyntax(<<C/utf32-big,R/binary>>) ->
    [C|utf32_big_to_list_bsyntax(R)].

list_to_utf32_big_bsyntax(List,{utf32,big}) ->
    FList = flatx(List),
    list_to_binary([ if
			 is_binary(E) ->
			     E;
			 true ->
			     <<E/utf32-big>>
		     end || E <- FList ]);
list_to_utf32_big_bsyntax(List,latin1) ->
    FList = flatb(List),
    list_to_binary([ <<E/utf32-big>> || E <- FList ]).


utf32_little_to_list_bsyntax(<<>>) ->
    [];
utf32_little_to_list_bsyntax(<<C/utf32-little,R/binary>>) ->
    [C|utf32_little_to_list_bsyntax(R)].

list_to_utf32_little_bsyntax(List,{utf32,little}) ->
    FList = flatx(List),
    list_to_binary([ if
			 is_binary(E) ->
			     E;
			 true ->
			     <<E/utf32-little>>
		     end || E <- FList ]);
list_to_utf32_little_bsyntax(List,latin1) ->
    FList = flatb(List),
    list_to_binary([ <<E/utf32-little>> || E <- FList ]).



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

utf8_to_list(<<>>) ->
    [];
utf8_to_list(Bin) ->
    N = utf8_siz(Bin),
    <<X:N/binary,Rest/binary>> = Bin,
    [utf8_to_int(X) | utf8_to_list(Rest)].
utf8_siz(<<0:1,_:7,_/binary>>) ->
    1;
utf8_siz(<<1:1,1:1,0:1,_:5,_/binary>>) ->
    2;
utf8_siz(<<1:1,1:1,1:1,0:1,_:4,_/binary>>) ->
    3;
utf8_siz(<<1:1,1:1,1:1,1:1,0:1,_:3,_/binary>>) ->
    4.

utf8_to_int(<<0:1,B:7>>) ->
    B;
utf8_to_int(<<1:1,1:1,0:1,B1:5,1:1,0:1,B2:6>>) ->
    (B1 bsl 6) bor B2;
utf8_to_int(<<1:1,1:1,1:1,0:1,B1:4,1:1,0:1,B2:6,1:1,0:1,B3:6>>) ->
    (B1 bsl 12) bor (B2 bsl 6) bor B3; 
utf8_to_int(<<1:1,1:1,1:1,1:1,0:1,B1:3,1:1,0:1,
	      B2:6,1:1,0:1,B3:6,1:1,0:1,B4:6>>) ->
    Res = (B1 bsl 18) bor (B2 bsl 12) bor (B3 bsl 6) bor B4,
    case Res of
	X when X > 16#10FFFF ->
	    exit(unsupported_utf8);
	Other ->
	    Other
    end;
utf8_to_int(_) ->
    exit(unsupported_utf8).


utf16_big_to_list(<<>>) ->
    [];
utf16_big_to_list(Bin) ->
    N = utf16_big_siz(Bin),
    <<X:N/binary,Rest/binary>> = Bin,
    [utf16_big_to_int(X) | utf16_big_to_list(Rest)].

utf16_big_siz(<<1:1,1:1,0:1,1:1,1:1,0:1,_:1,_:1,_/binary>>) ->
    4;
utf16_big_siz(_) ->
    2.

utf16_big_to_int(<<1:1,1:1,0:1,1:1,1:1,0:1,W1:10,1:1,1:1,0:1,1:1,1:1,1:1,W2:10>>) ->
    ((W1 bsl 10) bor W2) + 16#10000;
utf16_big_to_int(<<W:16>>) ->
    W;
utf16_big_to_int(_) ->
    exit(unsupported_utf16_big).

utf16_little_to_list(<<>>) ->
    [];
utf16_little_to_list(Bin) ->
    N = utf16_little_siz(Bin),
    <<X:N/binary,Rest/binary>> = Bin,
    [utf16_little_to_int(X) | utf16_little_to_list(Rest)].
utf16_little_siz(<<_:8,1:1,1:1,0:1,1:1,1:1,0:1,_:1,_:1,_/binary>>) ->
    4;
utf16_little_siz(_) ->
    2.

utf16_little_to_int(<<W1B:8,1:1,1:1,0:1,1:1,1:1,0:1,W1A:2,W2B:8,1:1,1:1,0:1,1:1,1:1,1:1,W2A:2>>) ->
    W1 = (W1A bsl 8) bor W1B,
    W2 = (W2A bsl 8) bor W2B,
    ((W1 bsl 10) bor W2) + 16#10000;
utf16_little_to_int(<<W:16/little>>) ->
    W;
utf16_little_to_int(_) ->
    exit(unsupported_utf16_little).

utf32_big_to_list(<<>>) ->
    [];
utf32_big_to_list(<<I:32,Rest/binary>>) ->
    [ I | utf32_big_to_list(Rest)].
utf32_little_to_list(<<>>) ->
    [];
utf32_little_to_list(<<I:32/little,Rest/binary>>) ->
    [ I | utf32_little_to_list(Rest)].


x_to_list_bsyntax(utf8,Bin) ->
    utf8_to_list_bsyntax(Bin);
x_to_list_bsyntax({utf16,big},Bin) ->
    utf16_big_to_list_bsyntax(Bin);
x_to_list_bsyntax({utf16,little},Bin) ->
    utf16_little_to_list_bsyntax(Bin);
x_to_list_bsyntax({utf32,big},Bin) ->
    utf32_big_to_list_bsyntax(Bin);
x_to_list_bsyntax({utf32,little},Bin) ->
    utf32_little_to_list_bsyntax(Bin).

list_to_x_bsyntax(utf8,L,utf8) ->
    list_to_utf8_bsyntax(L,unicode);
list_to_x_bsyntax(utf8,L,Enc) ->
    list_to_utf8_bsyntax(L,Enc);
list_to_x_bsyntax({utf16,big},L,Enc) ->
    list_to_utf16_big_bsyntax(L,Enc);
list_to_x_bsyntax({utf16,little},L,Enc) ->
    list_to_utf16_little_bsyntax(L,Enc);
list_to_x_bsyntax({utf32,big},L,Enc) ->
    list_to_utf32_big_bsyntax(L,Enc);
list_to_x_bsyntax({utf32,little},L,Enc) ->
    list_to_utf32_little_bsyntax(L,Enc).


make_unaligned(Bin0) when is_binary(Bin0) ->
    Bin1 = <<0:3,Bin0/binary,31:5>>,
    Sz = byte_size(Bin0),
    <<0:3,Bin:Sz/binary,31:5>> = id(Bin1),
    Bin.

id(I) -> I.

setlimit(X) ->
    erts_debug:set_internal_state(available_internal_state,true),
    io:format("Setting loop limit, old: ~p, now set to ~p~n",
	      [erts_debug:set_internal_state(unicode_loop_limit,X),X]).
