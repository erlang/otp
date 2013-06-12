%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2013. All Rights Reserved.
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
-module(unicode_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,	 
	 init_per_testcase/2,
	 end_per_testcase/2,
	 utf8_illegal_sequences_bif/1,
	 utf16_illegal_sequences_bif/1,
	 random_lists/1,
	 roundtrips/1,
	 latin1/1,
	 exceptions/1, binaries_errors/1]).
	 
init_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Dog=?t:timetrap(?t:minutes(20)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [utf8_illegal_sequences_bif,
     utf16_illegal_sequences_bif, random_lists, roundtrips,
     latin1, exceptions, binaries_errors].

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

binaries_errors(Config) when is_list(Config) ->
    setlimit(10),
    ex_binaries_errors_utf8(Config),
    setlimit(default),
    ex_binaries_errors_utf8(Config),
    ex_binaries_errors_utf16_little(Config),
    ex_binaries_errors_utf16_big(Config),
    ex_binaries_errors_utf32_little(Config),
    ex_binaries_errors_utf32_big(Config).
    
ex_binaries_errors_utf8(Config) when is_list(Config) ->
    %% Original smoke test, we should not forget the original offset...
    <<_:8,_:8,RR2/binary>> = <<$a,$b,164,165,$c>>,
    {error,[],<<164,165,$c>>} = unicode:characters_to_list(RR2),
    %% Now, try with longer binary (trapping)
    BrokenPart = list_to_binary(lists:seq(128,255)),
    BrokenSz = byte_size(BrokenPart),
    [ begin
	  OKList = lists:flatten(lists:duplicate(N,lists:seq(1,255))),
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
      end || N <- lists:seq(1,20) ],
    ok.

ex_binaries_errors_utf16_little(Config) when is_list(Config) ->
    BrokenPart = << <<X:16/little>> || X <- lists:seq(16#DC00,16#DFFF) >>,
    BrokenSz = byte_size(BrokenPart),
    [ begin
	  OKList = lists:flatten(lists:duplicate(N,lists:seq(1,255))),
	  OKBin = unicode:characters_to_binary(OKList,unicode,{utf16,little}),
	  OKLen = length(OKList),
	  %% Copy to avoid that the binary get's writable
	  PartlyBroken = binary:copy(<<OKBin/binary, BrokenPart/binary>>),
	  PBSz = byte_size(PartlyBroken),
	  {error,OKList,DeepBrokenPart} = 
	      unicode:characters_to_list(PartlyBroken,{utf16,little}),
	  BrokenPart = iolist_to_binary(DeepBrokenPart),
	  [ begin
		NewList = lists:nthtail(X, OKList),
		NewSz = byte_size(unicode:characters_to_binary(NewList,unicode,{utf16,little})) + 
		    BrokenSz,
		Chomped = binary:part(PartlyBroken,PBSz - NewSz, NewSz),
		true = (binary:referenced_byte_size(Chomped) =:= PBSz),
		{error,NewList,DeepBrokenPart2} =  
		    unicode:characters_to_list(Chomped,{utf16,little}),
		BrokenPart = iolist_to_binary(DeepBrokenPart2)
	    end || X <- lists:seq(1,OKLen) ]
      end || N <- lists:seq(1,15) ],
    ok.
ex_binaries_errors_utf16_big(Config) when is_list(Config) ->
    BrokenPart = << <<X:16/big>> || X <- lists:seq(16#DC00,16#DFFF) >>,
    BrokenSz = byte_size(BrokenPart),
    [ begin
	  OKList = lists:flatten(lists:duplicate(N,lists:seq(1,255))),
	  OKBin = unicode:characters_to_binary(OKList,unicode,{utf16,big}),
	  OKLen = length(OKList),
	  %% Copy to avoid that the binary get's writable
	  PartlyBroken = binary:copy(<<OKBin/binary, BrokenPart/binary>>),
	  PBSz = byte_size(PartlyBroken),
	  {error,OKList,DeepBrokenPart} = 
	      unicode:characters_to_list(PartlyBroken,{utf16,big}),
	  BrokenPart = iolist_to_binary(DeepBrokenPart),
	  [ begin
		NewList = lists:nthtail(X, OKList),
		NewSz = byte_size(unicode:characters_to_binary(NewList,unicode,{utf16,big})) + 
		    BrokenSz,
		Chomped = binary:part(PartlyBroken,PBSz - NewSz, NewSz),
		true = (binary:referenced_byte_size(Chomped) =:= PBSz),
		{error,NewList,DeepBrokenPart2} =  
		    unicode:characters_to_list(Chomped,{utf16,big}),
		BrokenPart = iolist_to_binary(DeepBrokenPart2)
	    end || X <- lists:seq(1,OKLen) ]
      end || N <- lists:seq(1,15) ],
    ok.
    
ex_binaries_errors_utf32_big(Config) when is_list(Config) ->
    BrokenPart = << <<X:32/big>> || X <- lists:seq(16#DC00,16#DFFF) >>,
    BrokenSz = byte_size(BrokenPart),
    [ begin
	  OKList = lists:flatten(lists:duplicate(N,lists:seq(1,255))),
	  OKBin = unicode:characters_to_binary(OKList,unicode,{utf32,big}),
	  OKLen = length(OKList),
	  %% Copy to avoid that the binary get's writable
	  PartlyBroken = binary:copy(<<OKBin/binary, BrokenPart/binary>>),
	  PBSz = byte_size(PartlyBroken),
	  {error,OKList,DeepBrokenPart} = 
	      unicode:characters_to_list(PartlyBroken,{utf32,big}),
	  BrokenPart = iolist_to_binary(DeepBrokenPart),
	  [ begin
		NewList = lists:nthtail(X, OKList),
		NewSz = byte_size(unicode:characters_to_binary(NewList,unicode,{utf32,big})) + 
		    BrokenSz,
		Chomped = binary:part(PartlyBroken,PBSz - NewSz, NewSz),
		true = (binary:referenced_byte_size(Chomped) =:= PBSz),
		{error,NewList,DeepBrokenPart2} =  
		    unicode:characters_to_list(Chomped,{utf32,big}),
		BrokenPart = iolist_to_binary(DeepBrokenPart2)
	    end || X <- lists:seq(1,OKLen) ]
      end || N <- lists:seq(1,15) ],
    ok.
    
ex_binaries_errors_utf32_little(Config) when is_list(Config) ->
    BrokenPart = << <<X:32/little>> || X <- lists:seq(16#DC00,16#DFFF) >>,
    BrokenSz = byte_size(BrokenPart),
    [ begin
	  OKList = lists:flatten(lists:duplicate(N,lists:seq(1,255))),
	  OKBin = unicode:characters_to_binary(OKList,unicode,{utf32,little}),
	  OKLen = length(OKList),
	  %% Copy to avoid that the binary get's writable
	  PartlyBroken = binary:copy(<<OKBin/binary, BrokenPart/binary>>),
	  PBSz = byte_size(PartlyBroken),
	  {error,OKList,DeepBrokenPart} = 
	      unicode:characters_to_list(PartlyBroken,{utf32,little}),
	  BrokenPart = iolist_to_binary(DeepBrokenPart),
	  [ begin
		NewList = lists:nthtail(X, OKList),
		NewSz = byte_size(unicode:characters_to_binary(NewList,unicode,{utf32,little})) + 
		    BrokenSz,
		Chomped = binary:part(PartlyBroken,PBSz - NewSz, NewSz),
		true = (binary:referenced_byte_size(Chomped) =:= PBSz),
		{error,NewList,DeepBrokenPart2} =  
		    unicode:characters_to_list(Chomped,{utf32,little}),
		BrokenPart = iolist_to_binary(DeepBrokenPart2)
	    end || X <- lists:seq(1,OKLen) ]
      end || N <- lists:seq(1,15) ],
    ok.
    


exceptions(Config) when is_list(Config) ->
    setlimit(10),
    ex_exceptions(Config),
    setlimit(default),
    ex_exceptions(Config).

ex_exceptions(Config) when is_list(Config) ->
    ?line L = lists:seq(0,255),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L++255,unicode)),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary({1,2,3},unicode)),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(1,unicode)),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(1.0,unicode)),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary('1',unicode)),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary([1,2,3,apa],unicode)),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary([1,2,3,4.0],unicode)),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L++255,latin1)),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary({1,2,3},latin1)),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(1,latin1)),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(1.0,latin1)),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary('1',latin1)),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary([1,2,3,apa],latin1)),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary([1,2,3,4.0],latin1)),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,gnarfl)),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,L)),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,{latin1})),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,[latin1])),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,1)),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,1.0)),
    Encodings = [unicode, utf8,utf16,utf32,{utf16,big},
		 {utf16,little},{utf32,big},{utf32,little}],
    [ begin
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L++255,unicode,Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary({1,2,3},unicode,Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(1,unicode,Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(1.0,unicode,Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary('1',unicode,Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary([1,2,3,apa],unicode,
									  Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary([1,2,3,4.0],unicode,
									  Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L++255,latin1,Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary({1,2,3},latin1,Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(1,latin1,Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(1.0,latin1,Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary('1',latin1,Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary([1,2,3,apa],latin1,
									  Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary([1,2,3,4.0],latin1,
									  Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,gnarfl,Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,L,Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,{latin1},Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,[latin1],Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,1,Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_binary(L,1.0,Enc))
      end || Enc <- Encodings ],


    Encodings2 = [latin1, unicode, utf8,utf16,utf32,{utf16,big},
		 {utf16,little},{utf32,big},{utf32,little}],
    [ begin
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_list(L++255,Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_list({1,2,3},Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_list(1,Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_list(1.0,Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_list('1',Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_list([1,2,3,apa],Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_list([1,2,3,4.0],Enc)),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_list(L,{Enc})),
	  ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_list(L,[Enc]))
      end || Enc <- Encodings2 ],
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_list(L,gnarfl)),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_list(L,L)),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_list(L,1)),
    ?line {'EXIT',{badarg,_}} = (catch unicode:characters_to_list(L,1.0)),
    [ begin
	  ?line Bx = unicode:characters_to_binary(L,latin1, Enc),
	  ?line L = unicode:characters_to_list(Bx,Enc)
      end || Enc <- Encodings ],
    ?line B = unicode:characters_to_binary(L,latin1),
    ?line L = unicode:characters_to_list(B,unicode),
    ?line L = unicode:characters_to_list(list_to_binary(L),latin1),
    ?line More = <<B/binary,0,1,2>>,
    ?line B2 = list_to_binary([254,255]),
    ?line B3 = list_to_binary([0,1,2,254,255]),
    ?line {error,B,Rest1} = unicode:characters_to_binary([L,B2],unicode),
    ?line B2 = iolist_to_binary(Rest1),
    ?line {error,More,Rest2} = unicode:characters_to_binary([L,B3],unicode),
    [ begin ?line {error,_,_} = unicode:characters_to_binary([L,B2],unicode,Enc) end
      || Enc <- Encodings ],
    ?line Valid0 = unicode:characters_to_binary([L,254,255],unicode),
    ?line Valid1 = unicode:characters_to_binary([L,254,255],latin1),
    ?line Valid2 = unicode:characters_to_binary([L,254,255,256,257],unicode),
    ?line Valid3 = unicode:characters_to_binary([L,B2],latin1),
    ?line true = is_binary(Valid0),
    ?line true = is_binary(Valid1),
    ?line true = is_binary(Valid2),
    ?line true = is_binary(Valid3),
    ?line Valid4 = unicode:characters_to_binary([L,B3],latin1),
    ?line true = is_binary(Valid4),
    ?line B2 = iolist_to_binary(Rest2),
    ?line true = (L ++ [254,255] =:=  unicode:characters_to_list(Valid0,unicode)),
    ?line true = (L ++ [254,255,256,257] =:=  unicode:characters_to_list(Valid2,unicode)),
    lists:foreach(fun(Enco) ->
			  ?line Valid0x = unicode:characters_to_binary([L,254,255],unicode,Enco),
			  ?line Valid1x = unicode:characters_to_binary([L,254,255],latin1,Enco),
			  ?line Valid2x = unicode:characters_to_binary([L,254,255,256,257],unicode,Enco),
			  ?line Valid3x = unicode:characters_to_binary([L,B2],latin1,Enco),
			  ?line true = is_binary(Valid0x),
			  ?line true = is_binary(Valid1x),
			  ?line true = is_binary(Valid2x),
			  ?line true = is_binary(Valid3x)

		  end, Encodings),
    ok.
    

latin1(Config) when is_list(Config) ->
    setlimit(10),
    ex_latin1(Config),
    setlimit(default),
    ex_latin1(Config).

ex_latin1(Config) when is_list(Config) ->
    ?line All = lists:seq(0,255),
    ?line AllBin = list_to_binary(All),
    ?line AllUtf8 = unicode:characters_to_binary(All,latin1),
    ?line AllUtf8 = unicode:characters_to_binary(AllBin,latin1),
    ?line AllUtf8 = unicode:characters_to_binary([AllBin],latin1),
    ?line AllUtf8 = unicode:characters_to_binary(make_unaligned(AllBin),latin1),
    ?line AllUtf8 = unicode:characters_to_binary([make_unaligned(AllBin)],latin1),
    ?line AllUtf8 = list_to_utf8_bsyntax([AllBin],latin1),
    ?line AllUtf8 = list_to_utf8_bsyntax([make_unaligned(AllBin)],latin1),
    ?line AllUtf8 = unicode_mixed_to_utf8_1(All),

    ?line AllUtf16_Big = unicode:characters_to_binary(All,latin1,utf16),
    ?line AllUtf16_Big = unicode:characters_to_binary(AllBin,latin1,utf16),
    ?line AllUtf16_Big = unicode:characters_to_binary([AllBin],latin1,utf16),
    ?line AllUtf16_Big = unicode:characters_to_binary(make_unaligned(AllBin),latin1,utf16),
    ?line AllUtf16_Big = unicode:characters_to_binary([make_unaligned(AllBin)],latin1,utf16),
    ?line AllUtf16_Big = list_to_utf16_big_bsyntax([AllBin],latin1),
    ?line AllUtf16_Big = list_to_utf16_big_bsyntax([make_unaligned(AllBin)],latin1),

    ?line AllUtf16_Little = unicode:characters_to_binary(All,latin1,{utf16,little}),
    ?line AllUtf16_Little = unicode:characters_to_binary(AllBin,latin1,{utf16,little}),
    ?line AllUtf16_Little = unicode:characters_to_binary([AllBin],latin1,{utf16,little}),
    ?line AllUtf16_Little = unicode:characters_to_binary(make_unaligned(AllBin),latin1,
							 {utf16,little}),
    ?line AllUtf16_Little = unicode:characters_to_binary([make_unaligned(AllBin)],latin1,
							 {utf16,little}),
    ?line AllUtf16_Little = list_to_utf16_little_bsyntax([AllBin],latin1),
    ?line AllUtf16_Little = list_to_utf16_little_bsyntax([make_unaligned(AllBin)],latin1),

    ?line AllUtf32_Big = unicode:characters_to_binary(All,latin1,utf32),
    ?line AllUtf32_Big = unicode:characters_to_binary(AllBin,latin1,utf32),
    ?line AllUtf32_Big = unicode:characters_to_binary([AllBin],latin1,utf32),
    ?line AllUtf32_Big = unicode:characters_to_binary(make_unaligned(AllBin),latin1,utf32),
    ?line AllUtf32_Big = unicode:characters_to_binary([make_unaligned(AllBin)],latin1,utf32),
    ?line AllUtf32_Big = list_to_utf32_big_bsyntax([AllBin],latin1),
    ?line AllUtf32_Big = list_to_utf32_big_bsyntax([make_unaligned(AllBin)],latin1),

    ?line AllUtf32_Little = unicode:characters_to_binary(All,latin1,{utf32,little}),
    ?line AllUtf32_Little = unicode:characters_to_binary(AllBin,latin1,{utf32,little}),
    ?line AllUtf32_Little = unicode:characters_to_binary([AllBin],latin1,{utf32,little}),
    ?line AllUtf32_Little = unicode:characters_to_binary(make_unaligned(AllBin),latin1,
							 {utf32,little}),
    ?line AllUtf32_Little = unicode:characters_to_binary([make_unaligned(AllBin)],latin1,
							 {utf32,little}),
    ?line AllUtf32_Little = list_to_utf32_little_bsyntax([AllBin],latin1),
    ?line AllUtf32_Little = list_to_utf32_little_bsyntax([make_unaligned(AllBin)],latin1),

    ?line DoubleUtf8 = <<AllUtf8/binary,AllUtf8/binary>>,
    ?line DoubleUtf8 = unicode:characters_to_binary([All,AllBin],latin1),
    ?line DoubleUtf8 = 
	unicode:characters_to_binary([All,make_unaligned(AllBin)],latin1),
    ?line DoubleUtf8 = unicode:characters_to_binary([All|AllBin],latin1),
    ?line DoubleUtf8 = 
	unicode:characters_to_binary([All|make_unaligned(AllBin)],latin1),
    ?line DoubleUtf8 = unicode:characters_to_binary([AllBin,All],latin1),
    ?line DoubleUtf8 = unicode:characters_to_binary([AllBin|All],latin1),
    ?line DoubleUtf8 = list_to_utf8_bsyntax([AllBin|All],latin1), 

    ?line DoubleUtf16 = <<AllUtf16_Big/binary,AllUtf16_Big/binary>>,
    ?line DoubleUtf16 = unicode:characters_to_binary([All,AllBin],latin1,{utf16,big}),
    ?line DoubleUtf16 = 
	unicode:characters_to_binary([All,make_unaligned(AllBin)],latin1,{utf16,big}),
    ?line DoubleUtf16 = unicode:characters_to_binary([All|AllBin],latin1,{utf16,big}),
    ?line DoubleUtf16 = 
	unicode:characters_to_binary([All|make_unaligned(AllBin)],latin1,{utf16,big}),
    ?line DoubleUtf16 = unicode:characters_to_binary([AllBin,All],latin1,{utf16,big}),
    ?line DoubleUtf16 = unicode:characters_to_binary([AllBin|All],latin1,{utf16,big}),
    ?line DoubleUtf16 = list_to_utf16_big_bsyntax([AllBin|All],latin1), 

    ?line All = unicode:characters_to_list(AllUtf8,unicode),
    ?line All = unicode:characters_to_list(make_unaligned(AllUtf8),unicode),
    ?line All = utf8_to_list_bsyntax(AllUtf8),
    ?line AllAll = All ++ All,
    ?line AllAll = unicode:characters_to_list(DoubleUtf8,unicode),
    ?line AllAll = unicode:characters_to_list(make_unaligned(DoubleUtf8),unicode),
    ?line AllAll = utf8_to_list_bsyntax(DoubleUtf8),
    ?line {error,AllUtf8,Rest1} =  unicode:characters_to_binary(All++[16#FFF],latin1),
    ?line [16#FFF] = lists:flatten(Rest1),
    ?line {error,DoubleUtf8,Rest2} = 
	unicode:characters_to_binary([All,AllBin,16#FFF],latin1),
    ?line {error,DoubleUtf16,Rest2x} = 
	unicode:characters_to_binary([All,AllBin,16#FFF],latin1,utf16),
    ?line [16#FFF] = lists:flatten(Rest2),
    ?line [16#FFF] = lists:flatten(Rest2x),
    ?line {error,AllUtf8,Rest3} = 
	unicode:characters_to_binary([All,16#FFF,AllBin,16#FFF],
			    latin1),
    ?line {error,AllUtf8,Rest3} = 
	unicode:characters_to_binary([All,16#FFF,make_unaligned(AllBin),16#FFF],
			    latin1),
    ?line {error,AllUtf16_Big,Rest3x} = 
	unicode:characters_to_binary([All,16#FFF,AllBin,16#FFF],
			    latin1,{utf16,big}),
    ?line {error,AllUtf16_Big,Rest3x} = 
	unicode:characters_to_binary([All,16#FFF,make_unaligned(AllBin),16#FFF],
			    latin1,{utf16,big}),
    ?line [16#FFF,AllBin,16#FFF] = lists:flatten(Rest3),
    ?line [16#FFF,AllBin,16#FFF] = lists:flatten(Rest3x),
    ?line DoubleSize = byte_size(DoubleUtf8),
    ?line AllBut1 = DoubleSize - 1,
    ?line AllBut2 = DoubleSize - 2,
    ?line <<MissingLastByte:AllBut1/binary,_>> = DoubleUtf8,
    ?line <<_:AllBut2/binary,MissingStart:1/binary,_>> = DoubleUtf8,
    ?line {ChompedList,_} = lists:split(length(AllAll) - 1,AllAll),
    ?line {incomplete,ChompedList,MissingStart} = 
	unicode:characters_to_list(MissingLastByte,unicode),
    ?line {incomplete,ChompedList,MissingStart} = 
	unicode:characters_to_list(make_unaligned(MissingLastByte),unicode),

    ?line DoubleSize16 = byte_size(DoubleUtf16),
    ?line DoubleUtf16_2 = list_to_binary([DoubleUtf16,<<16#FFFFF/utf16-big>>]),
    ?line DoubleSize16_2 = byte_size(DoubleUtf16_2),
    ?line AllBut1_16 = DoubleSize16 - 1,
    ?line AllBut2_16_2 = DoubleSize16_2 - 2,
    ?line <<MissingLastBytes16:AllBut2_16_2/binary,_,_>> = DoubleUtf16_2,
    ?line <<MissingLastByte16:AllBut1_16/binary,_>> = DoubleUtf16,
    ?line {incomplete,AllAll,_} = 
	unicode:characters_to_list(MissingLastBytes16,utf16),
    ?line {incomplete,AllAll,_} = 
	unicode:characters_to_list(make_unaligned(MissingLastBytes16),utf16),
    ?line {incomplete,ChompedList,_} = 
	unicode:characters_to_list(MissingLastByte16,utf16),
    ?line {incomplete,ChompedList,_} = 
	unicode:characters_to_list(make_unaligned(MissingLastByte16),utf16),
    ok.
    
roundtrips(Config) when is_list(Config) ->
    setlimit(10),
    ex_roundtrips(Config),
    setlimit(default),
    ex_roundtrips(Config).

ex_roundtrips(Config) when is_list(Config) ->
    ?line L1 = ranges(0, 16#D800 - 1, 
		      erlang:system_info(context_reductions) * 11),
    ?line L2 = ranges(16#DFFF + 1, 16#10000 - 1,
		      erlang:system_info(context_reductions) * 11),
    %?line L3 = ranges(16#FFFF + 1, 16#10FFFF, 
    %		      erlang:system_info(context_reductions) * 11),
    ?line L3 = ranges(16#FFFFF, 16#10FFFF, 
		      erlang:system_info(context_reductions) * 11),
    ?line L = L1 ++ L2 ++ L3,
    ?line LLen = length(L),
    ?line Parts = erlang:system_info(schedulers),
    ?line Lists = splitup(L,LLen,Parts),
    ?line PidRefs = [spawn_monitor(fun() ->
					   do_roundtrips(MyPart)
				   end) || MyPart <- Lists],
    ?line [receive {'DOWN',Ref,process,Pid,Reason} -> normal=Reason end ||
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
    ?line random_iolist:run(150, PlainFlatten1, PlainFlatten3),
    ?line random_iolist:run(150, PlainFlatten2, PlainFlatten3),
    ?line random_iolist:run(150, PlainFlatten1, PlainFlatten2),
    ?line random_iolist:run(150, PlainFlatten1, PlainFlatten4),
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
    ?line random_iolist:run(150, PlainFlatten1, SelfMade),
    ?line random_iolist:run(150, PlainFlatten2, SelfMadeA),

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
    ?line random_iolist:run(150, RoundTrip11,RoundTrip21),
    ?line random_iolist:run(150, RoundTrip21,RoundTrip31),
    ?line random_iolist:run(150, RoundTrip31,RoundTrip41),
    ?line random_iolist:run(150, RoundTrip11,RoundTrip41),
    ?line random_iolist:run(150, RoundTrip21,RoundTrip41),
    ?line random_iolist:run(150, RoundTrip11,RoundTrip31),
    ?line random_iolist:run(150, RoundTrip11,RoundTrip51),


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
    ?line random_unicode_list:run(150, UniFlatten1,UniFlatten2),
    ?line random_unicode_list:run(150, UniFlatten1,UniFlatten3),
    ?line random_unicode_list:run(150, UniFlatten2,UniFlatten4),
    ?line random_unicode_list:run(150, UniFlatten2,UniFlatten3),

    ?line Encodings = [utf8,{utf16,big},
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
						%erlang:display({InEnc1,OutEnc1}),
						?line random_unicode_list:run(150, Uni16BigFlatten1,Uni16BigFlatten2,InEnc1),
						?line random_unicode_list:run(150, Uni16BigFlatten1,Uni16BigFlatten3,InEnc1),
						?line random_unicode_list:run(150, Uni16BigFlatten2,Uni16BigFlatten4,InEnc1),
						?line random_unicode_list:run(150, Uni16BigFlatten2,Uni16BigFlatten3,InEnc1)
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
    ?line random_unicode_list:run(150, SelfMade1,SelfMade2),
    ?line random_unicode_list:run(150, UniFlatten2, SelfMade1),
    ?line random_unicode_list:run(150, UniFlatten2, SelfMade2),
    ?line random_unicode_list:run(150, UniFlatten2, SelfMade3),
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
    ?line random_unicode_list:run(150, RoundTrip1,RoundTrip2),
    ?line random_unicode_list:run(150, RoundTrip2,RoundTrip3),
    ?line random_unicode_list:run(150, RoundTrip3,RoundTrip4),
    ?line random_unicode_list:run(150, RoundTrip1,RoundTrip4),
    ?line random_unicode_list:run(150, RoundTrip2,RoundTrip4),
    ?line random_unicode_list:run(150, RoundTrip1,RoundTrip3),
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
						?line random_unicode_list:run(150, RoundTripUtf16_Big_1,RoundTripUtf16_Big_2,InEnc2),
						?line random_unicode_list:run(150, RoundTripUtf16_Big_2,RoundTripUtf16_Big_3,InEnc2),
						?line random_unicode_list:run(150, RoundTripUtf16_Big_3,RoundTripUtf16_Big_4,InEnc2),
						?line random_unicode_list:run(150, RoundTripUtf16_Big_1,RoundTripUtf16_Big_4,InEnc2),
						?line random_unicode_list:run(150, RoundTripUtf16_Big_2,RoundTripUtf16_Big_4,InEnc2),
						?line random_unicode_list:run(150, RoundTripUtf16_Big_1,RoundTripUtf16_Big_3,InEnc2)
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
    ?line random_unicode_list:run(150, ToList1,ToList2),
    ?line random_unicode_list:run(150, ToList2,ToList3),
    ?line random_unicode_list:run(150, ToList3,ToList4),
    ?line random_unicode_list:run(150, ToList1,ToList4),
    ?line random_unicode_list:run(150, ToList2,ToList4),
    ?line random_unicode_list:run(150, ToList1,ToList3),

    ok.

utf16_illegal_sequences_bif(Config) when is_list(Config) ->
    setlimit(10),
    ex_utf16_illegal_sequences_bif(Config),
    setlimit(default),
    ex_utf16_illegal_sequences_bif(Config).

ex_utf16_illegal_sequences_bif(Config) when is_list(Config) ->
    ?line utf16_fail_range_bif_simple(16#10FFFF+1, 16#10FFFF+512), %Too large.
    ?line utf16_fail_range_bif(16#D800, 16#DFFF),		%Reserved for UTF-16.

    ?line lonely_hi_surrogate_bif(16#D800, 16#DBFF,incomplete),
    ?line lonely_hi_surrogate_bif(16#DC00, 16#DFFF,error),
    ?line leading_lo_surrogate_bif(16#DC00, 16#DFFF),
    
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
    setlimit(10),
    ex_utf8_illegal_sequences_bif(Config),
    setlimit(default),
    ex_utf8_illegal_sequences_bif(Config).

ex_utf8_illegal_sequences_bif(Config) when is_list(Config) ->
    ?line fail_range_bif(16#10FFFF+1, 16#10FFFF+512), %Too large.
    ?line fail_range_bif(16#D800, 16#DFFF),		%Reserved for UTF-16.

    %% Illegal first character.
    ?line [fail_bif(<<I,16#8F,16#8F,16#8F>>,unicode) || I <- lists:seq(16#80, 16#BF)],

    %% Short sequences.
    ?line short_sequences_bif(16#80, 16#10FFFF),

    %% Overlong sequences. (Using more bytes than necessary
    %% is not allowed.)
    ?line overlong_bif(0, 127, 2),
    ?line overlong_bif(128, 16#7FF, 3),
    ?line overlong_bif(16#800, 16#FFFF, 4),
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
%    Step = (End - Char) + 1,
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
%    put(c_count,get(c_count)+1),    
    Bin1 = <<0:3,Bin0/binary,31:5>>,
    Sz = byte_size(Bin0),
    <<0:3,Bin:Sz/binary,31:5>> = id(Bin1),
    Bin.

id(I) -> I.

setlimit(X) ->
    erts_debug:set_internal_state(available_internal_state,true),
    io:format("Setting loop limit, old: ~p, now set to ~p~n",
	      [erts_debug:set_internal_state(unicode_loop_limit,X),X]).


%%
%% Tracing utility
%%

%% tr_dump() ->
%%     erlang:display(lists:sort(ets:tab2list(values))).

%% tr_off(Pid) ->
%%     receive after 10000 -> ok end,
%%     tr_dump(),
%%     Ref = erlang:monitor(process,Pid),
%%     exit(Pid,kill),
%%     receive
%% 	{'DOWN',Ref,_,_,_} -> ok
%%     end,
%%     ok.

%% tr_on() ->   
%%     catch ets:delete(values),
%%     ets:new(values,[named_table,public]),
%%     ets:insert(values,{traps,0}),
%%     catch ets:delete(state),
%%     ets:new(state,[named_table,public]),
%%     Pid = spawn(?MODULE,trace_recv,[values,state]),
%%     erlang:trace(new,true,[garbage_collection,{tracer,Pid},timestamp,call]),
%%     erlang:trace_pattern({erlang,list_to_utf8,2},[{'_',[],[{return_trace}]}],[global]),
%%     Pid.

%% ts_to_int({Mega,Sec,Micro}) ->
%%     ((Mega * 1000000) + Sec) * 1000000 + Micro.

%% trace_recv(Values,State) ->
%%     receive
%% 	{trace_ts,Pid,call,_,TS} ->
%% 	    case ets:lookup(State,{call,Pid}) of
%% 		[{{call,Pid},_}] ->
%% 		    ets:update_counter(values,traps,1);
%% 		_ ->
%% 		    ok
%% 	    end,
%% 	    ets:insert(State,{{call,Pid},ts_to_int(TS)});
%% 	{trace_ts,Pid,return_from,_,_,TS} ->
%% 	    case ets:lookup(State,{call,Pid}) of
%% 		[{{call,Pid},TS2}] ->
%% 		    ets:delete(State,{call,Pid}),
%% 		    Elapsed = ts_to_int(TS) - TS2,
%% 		    case ets:lookup(Values,Pid) of
%% 			[{Pid,GCNum,CallNum,GCTime,CallTime}] ->
%% 			    ets:insert(Values,{Pid,GCNum,CallNum+1,GCTime,CallTime+Elapsed});
%% 			[] ->
%% 			     ets:insert(Values,{Pid,0,1,0,Elapsed})
%% 		    end;
%% 		_Other ->
%% 		    erlang:display({what2,Pid})
%% 	    end;
%% 	{trace_ts,Pid,gc_start,_,TS} ->
%% 	    ets:insert(State,{{gc,Pid},ts_to_int(TS)});
%% 	{trace_ts,Pid,gc_end,_,TS} ->
%% 	    case ets:lookup(State,{gc,Pid}) of
%% 		[{{gc,Pid},TS2}] ->
%% 		    ets:delete(State,{gc,Pid}),
%% 		    Elapsed = ts_to_int(TS) - TS2,
%% 		    case ets:lookup(Values,Pid) of
%% 			[{Pid,Num,CNum,Time,CTime}] ->
%% 			    ets:insert(Values,{Pid,Num+1,CNum,Time+Elapsed,CTime});
%% 			[] ->
%% 			     ets:insert(Values,{Pid,1,0,Elapsed,0})
%% 		    end;
%% 		_Other ->
%% 		    erlang:display({what,Pid})
%% 	    end;
%% 	X ->
%% 	    erlang:display({trace_recv,X})
%%     end,
%%     trace_recv(Values,State).
