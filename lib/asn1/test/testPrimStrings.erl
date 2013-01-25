%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2013. All Rights Reserved.
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
%%
-module(testPrimStrings).

-export([bit_string/1]).
-export([octet_string/1]).
-export([numeric_string/1]).
-export([other_strings/1]).
-export([more_strings/1]).
-export([universal_string/1]).
-export([bmp_string/1]).
-export([times/1]).
-export([utf8_string/1]).

-include_lib("test_server/include/test_server.hrl").

bit_string(Rules) ->
    
    %%==========================================================
    %% Bs1 ::= BIT STRING
    %%==========================================================

    bs_roundtrip('Bs1', 0, <<>>),
    bs_roundtrip('Bs1', 4, <<1:3>>),
    bs_roundtrip('Bs1', 15, <<15:4>>),
    bs_roundtrip('Bs1', 255, <<255:8>>),

    bs_roundtrip('Bs1', 256, [0,0,0,0,0,0,0,0,1]),
    bs_roundtrip('Bs1', 257, [1,0,0,0,0,0,0,0,1]),
    bs_roundtrip('Bs1', 444, [0,0,1,1,1,1,0,1,1]),
    
    {ok,Enc1} = 'PrimStrings':encode('Bs1', 12345678901234567890),
    {ok,_} = 'PrimStrings':decode('Bs1', Enc1),

    bs_roundtrip('Bs1', [1,1,1,1,1,1,1,1]),
    bs_roundtrip('Bs1', [0,1,0,0,1,0]),
    bs_roundtrip('Bs1', [1,0,0,0,0,0,0,0,0]),
    bs_roundtrip('Bs1', [0,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,0,1,1]),
    
    case asn1_wrapper:erule(Rules) of
	ber ->
	    bs_decode('Bs1', <<35,8,3,2,0,73,3,2,4,32>>,
		      [0,1,0,0,1,0,0,1,0,0,1,0]),
	    bs_decode('Bs1', <<35,9,3,2,0,234,3,3,7,156,0>>,
		      [1,1,1,0,1,0,1,0,1,0,0,1,1,1,0,0,0]),
	    bs_decode('Bs1', <<35,128,3,2,0,234,3,3,7,156,0,0,0>>,
		      [1,1,1,0,1,0,1,0,1,0,0,1,1,1,0,0,0]);
	per ->
	    ok
    end,

    
    %%==========================================================
    %% Bs2 ::= BIT STRING {su(0), mo(1), tu(2), we(3), th(4), fr(5), sa(6) } (SIZE (7))
    %%==========================================================
    
    ?line {ok,Bytes21} = asn1_wrapper:encode('PrimStrings','Bs2',[mo,tu,fr]),
    ?line {ok,[mo,tu,fr]} = asn1_wrapper:decode('PrimStrings','Bs2',lists:flatten(Bytes21)),
    
    ?line {ok,Bytes22} = asn1_wrapper:encode('PrimStrings','Bs2',[0,1,1,0,0,1,0]),
    ?line {ok,[mo,tu,fr]} = asn1_wrapper:decode('PrimStrings','Bs2',lists:flatten(Bytes22)),
    ok,
%% skip this because it is wrong
%     ?line case asn1_wrapper:erule(Rules) of
% 	      ber -> 
% 		  ?line {ok,[mo,tu,fr,su,mo,th]} = 
% 		      asn1_wrapper:decode('PrimStrings','Bs2',[35,8,3,2,0,101,3,2,2,200]),
		  
% 		  ?line {ok,[mo,tu,fr,su,mo,th]} = 
% 		      asn1_wrapper:decode('PrimStrings','Bs2',[35,128,3,2,1,100,3,2,2,200,0,0]),
% 		  ok;
	      
% 	      per ->
% 		  ok
% 	  end,
    
    
    
    %%==========================================================
    %% Bs3 ::= BIT STRING {su(0), mo(1), tu(2), we(3), th(4), fr(5), sa(6) } (SIZE (1..7))
    %%==========================================================
    
    roundtrip('Bs3', [mo,tu,fr]),
    bs_roundtrip('Bs3', [0,1,1,0,0,1,0], [mo,tu,fr]),
    
    %%==========================================================
    %% Bs7 ::= BIT STRING (SIZE (24))
    %%==========================================================

    bs_roundtrip('Bs7', 53245,
		 [1,0,1,1,1,1,1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,0,0]),
    bs_roundtrip('Bs7', [1,0,1,0],
		 [1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]),
    
    %%==========================================================
    %% BsPri ::= [PRIVATE 61] BIT STRING
    %%==========================================================
    
    bs_roundtrip('BsPri', 45, [1,0,1,1,0,1]),
    
    bs_roundtrip('BsPri', 211, [1,1,0,0,1,0,1,1]),
    
    case asn1_wrapper:erule(Rules) of
	ber ->
	    bs_decode('BsPri', <<223,61,4,5,75,226,96>>,
		      [0,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,0,1,1]),
	    bs_decode('BsPri', <<255,61,128,3,4,5,75,226,96,0,0>>,
		      [0,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,0,1,1]),
	    bs_decode('BsPri', <<255,61,9,3,2,0,75,3,3,5,226,96>>,
		      [0,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,0,1,1]),
	    bs_decode('BsPri', <<255,61,128,3,2,0,75,3,3,5,226,96,0,0>>,
		      [0,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,0,1,1]);
	per ->
	    ok
    end,
    
    
    %%==========================================================
    %% BsExpPri ::= [PRIVATE 61] EXPLICIT BIT STRING
    %%==========================================================
    
    bs_roundtrip('BsExpPri', 45, [1,0,1,1,0,1]),
    bs_roundtrip('BsExpPri', 211, [1,1,0,0,1,0,1,1]),
    
    case asn1_wrapper:erule(Rules) of
	ber ->
	    bs_decode('BsExpPri', <<255,61,6,3,4,5,75,226,96>>,
		      [0,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,0,1,1]);
	per ->
	    ok
    end,
    
    %%==========================================================
    %% TestS ::= BIT STRING {a(0),b(1)} (SIZE (3..8)), test case for OTP-4353
    %%==========================================================

    ?line {ok,Bytes53} = asn1_wrapper:encode('PrimStrings','TestS',[a]),
    ?line {ok,[a]} = 
	asn1_wrapper:decode('PrimStrings','TestS',lists:flatten(Bytes53)),

    %%==========================================================
    %% PersonalStatus ::= BIT STRING {married(0),employed(1),
    %%    veteran(2), collegeGraduate(3)}, test case for OTP-5710
    %%==========================================================
    
    ?line {ok,Bytes54} = asn1_wrapper:encode('BitStr','PersonalStatus',[]),
    ?line {ok,[]} = asn1_wrapper:decode('BitStr','PersonalStatus',Bytes54),
    
    %%==========================================================
    %% BS5932 ::= BIT STRING (SIZE (5..MAX))
    %% test case for OTP-5932
    %%==========================================================
    bs_roundtrip('BSMAX', [1,0,1,0,1]),
    case asn1_wrapper:erule(Rules) of
	ber ->
	    {error,_} = 'PrimStrings':encode('BSMAX', [1,0,1]);
	_ ->
	    ok
    end,
    
    %%==========================================================
    %% BS255 ::= BIT STRING (SIZE (255))
    %% BS256 ::= BIT STRING (SIZE (256))
    %% BS1024 ::= BIT STRING (SIZE (1024))
    %% test case for OTP-7602
    %%==========================================================
    BSmaker =
	fun(_F,S,S,_,Acc) -> 
		Acc;
	   (F,Ix,S,{A,B},Acc) -> 
		F(F,Ix+1,S,{B,A},[A|Acc]) 
	end,
    
    BSList255 = BSmaker(BSmaker,0,255,{1,0},[]),
    bs_roundtrip('BS255', BSList255),
    BSList256 = BSmaker(BSmaker,0,256,{1,0},[]),
    bs_roundtrip('BS256', BSList256),
    BSList1024 = BSmaker(BSmaker,0,1024,{1,0},[]),
    bs_roundtrip('BS1024', BSList1024),

    bs_roundtrip('TransportLayerAddress', [0,1,1,0]).

octet_string(Rules) ->

    %%==========================================================
    %% Os ::= OCTET STRING
    %%==========================================================

    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {ok,"Jones"} = 
		      asn1_wrapper:decode('PrimStrings','Os',[4,5,16#4A,16#6F,16#6E,16#65,16#73]),

		  ?line {ok,"Jones"} = 
		      asn1_wrapper:decode('PrimStrings','Os',[36,9,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73]),
		  
		  ?line {ok,"Jones"} = 
		      asn1_wrapper:decode('PrimStrings','Os',[36,128,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73,0,0]),
		  ok;
	      
	      per ->
		  ok
	  end,

    roundtrip('Os', [47,23,99,255,1]),
    roundtrip('OsCon', [47,23,99,255,1]),
    roundtrip('OsPri', [47,23,99,255,1]),
    roundtrip('OsApp', [47,23,99,255,1]),

    roundtrip('OsExpCon', [47,23,99,255,1]),
    roundtrip('OsExpPri', [47,23,99,255,1]),
    roundtrip('OsExpApp', [47,23,99,255,1]),

    roundtrip('Os', []),
    roundtrip('OsApp', []),
    roundtrip('OsExpApp',[]),
    
    OsR = "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890",

    roundtrip('Os', OsR),
    roundtrip('OsCon', OsR),
    roundtrip('OsExpApp', OsR),


    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','OsExpApp',[127,62,7,4,5,16#4A,16#6F,16#6E,16#65,16#73]),
		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','OsExpApp',[127,62,11,36,9,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73]),
		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','OsExpApp',[127,62,13,36,128,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73,0,0]),
		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','OsExpApp',[127,62,128,36,128,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73,0,0,0,0]),
		  ?line {ok,"JonesJones"} = asn1_wrapper:decode('PrimStrings','OsExpApp',[127,62,128,36,128,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73,0,0,36,128,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73,0,0,0,0]),
		  ok;
	      
	      per ->
		  ok
	  end,

    fragmented_octet_string(Rules),

    S255 = lists:seq(1, 255),
    FixedStrings = {'OsFixedStrings',true,"","1","12","345",true,
		    S255,[$a|S255],[$a,$b|S255],397},
    roundtrip('OsFixedStrings', FixedStrings),
    ok.
    
fragmented_octet_string(Erules) ->
    K16 = 1 bsl 14,
    K32 = K16 + K16,
    K48 = K32 + K16,
    K64 = K48 + K16,
    Lens = [0,1,14,15,16,17,127,128,
	    K16-1,K16,K16+1,K16+(1 bsl 7)-1,K16+(1 bsl 7),K16+(1 bsl 7)+1,
	    K32-1,K32,K32+1,K32+(1 bsl 7)-1,K32+(1 bsl 7),K32+(1 bsl 7)+1,
	    K48-1,K48,K48+1,K48+(1 bsl 7)-1,K48+(1 bsl 7),K48+(1 bsl 7)+1,
	    K64-1,K64,K64+1,K64+(1 bsl 7)-1,K64+(1 bsl 7),K64+(1 bsl 7)+1,
	    K64+K16-1,K64+K16,K64+K16+1],
    Types = ['Os','OsFrag'],
    [fragmented_octet_string(Erules, Types, L) || L <- Lens],
    fragmented_octet_string(Erules, ['FixedOs65536'], 65536),
    fragmented_octet_string(Erules, ['FixedOs65537'], 65537),

    %% Make sure that octet alignment works.
    roundtrip('OsAlignment',
	      {'OsAlignment',false,make_value(70000),true,make_value(66666),
	       false,make_value(65536),42}),
    roundtrip('OsAlignment',
	      {'OsAlignment',false,make_value(0),true,make_value(0),
	       false,make_value(65536),42}),
    ok.

fragmented_octet_string(Erules, Types, L) ->
    Value = make_value(L),
    [begin
	 Encoded = enc_frag(Erules, Type, Value),
	 {ok,Value} = 'PrimStrings':decode(Type, Encoded)
     end || Type <- Types],
    ok.

enc_frag(Erules, Type, Value) ->
    {ok,Encoded} = 'PrimStrings':encode(Type, Value),
    case Erules of
	ber ->
	    Encoded;
	_ ->
	    %% Validate encoding with our own encoder.
	    Encoded = enc_frag_1(<<>>, list_to_binary(Value))
    end.

enc_frag_1(Res, Bin0) ->
    K16 = 1 bsl 14,
    Sz = byte_size(Bin0),
    if
	Sz >= K16 ->
	    F = min(Sz div K16, 4),
	    FragSize = F * K16,
	    <<Frag:FragSize/binary-unit:8,Bin/binary>> = Bin0,
	    enc_frag_1(<<Res/binary,3:2,F:6,Frag/binary>>, Bin);
	Sz >= 128 ->
	    <<Res/binary,1:1,0:1,Sz:14,Bin0/binary>>;
	true ->
	    <<Res/binary,0:1,Sz:7,Bin0/binary>>
    end.

make_value(L) ->
    make_value(L, 0, []).

make_value(0, _, Acc) ->
    Acc;
make_value(N, Byte, Acc) when Byte =< 255 ->
    make_value(N-1, Byte+7, [Byte|Acc]);
make_value(N, Byte, Acc) ->
    make_value(N, Byte band 16#FF, Acc).


numeric_string(Rules) ->

    %%==========================================================
    %% Ns ::= NumericString
    %%==========================================================

    roundtrip('Ns', []),

    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {ok,BytesNs1} = asn1_wrapper:encode('PrimStrings','Ns',[48,49,32,51,52]),
		  ?line {ok,[48,49,32,51,52]} = asn1_wrapper:decode('PrimStrings','Ns',lists:flatten(BytesNs1)),

		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','Ns',[16#12,5,16#4A,16#6F,16#6E,16#65,16#73]),
		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','Ns',[16#32,9,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73]),
		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','Ns',[16#32,128,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73,0,0]),
		  ok;
	      
	      per ->
		  ?line {ok,BytesNs1} = asn1_wrapper:encode('PrimStrings','Ns',[48,49,32,51,52]),
		  ?line {ok,"01 34"} = asn1_wrapper:decode('PrimStrings','Ns',lists:flatten(BytesNs1)),
		  ok
	  end,

    

		       
    %%==========================================================
    %% NsCon ::= [70] NumericString
    %%==========================================================

    roundtrip('NsCon', []),

    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {ok,BytesNs11} = asn1_wrapper:encode('PrimStrings','NsCon',[48,49,32,51,52]),
		  ?line {ok,[48,49,32,51,52]} = asn1_wrapper:decode('PrimStrings','NsCon',lists:flatten(BytesNs11)),

		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','NsCon',[16#9F,16#46,5,16#4A,16#6F,16#6E,16#65,16#73]),
		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','NsCon',[16#BF,16#46,9,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73]),
		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','NsCon',[16#BF,16#46,128,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73,0,0]),
		  ok;
	      
	      per ->
		  ?line {ok,BytesNs11} = asn1_wrapper:encode('PrimStrings','NsCon',[48,49,32,51,52]),
		  ?line {ok,"01 34"} = asn1_wrapper:decode('PrimStrings','NsCon',lists:flatten(BytesNs11)),
		  ok
	  end,


		       
    %%==========================================================
    %% NsExpCon ::= [71] EXPLICIT NumericString
    %%==========================================================

    roundtrip('NsExpCon', []),

    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {ok,BytesNs21} = asn1_wrapper:encode('PrimStrings','NsExpCon',[48,49,32,51,52]),
		  ?line {ok,[48,49,32,51,52]} = asn1_wrapper:decode('PrimStrings','NsExpCon',lists:flatten(BytesNs21)),

		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','NsExpCon',[16#BF,16#47,16#07,16#12,16#05,16#4A,16#6F,16#6E,16#65,16#73]),
		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','NsExpCon',[16#BF,16#47,11,16#32,9,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73]),
		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','NsExpCon',[16#BF,16#47,128,16#32,128,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73,0,0,0,0]),
		  ?line {ok,"JonesJones"} = asn1_wrapper:decode('PrimStrings','NsExpCon',[16#BF,16#47,26,16#32,128,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73,0,0,16#32,128,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73,0,0]),
		  ?line {ok,"JonesJones"} = asn1_wrapper:decode('PrimStrings','NsExpCon',[16#BF,16#47,128,16#32,128,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73,0,0,16#32,128,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73,0,0,0,0]),
		  ok;
	      
	      per ->
		  ?line {ok,BytesNs21} = asn1_wrapper:encode('PrimStrings','NsExpCon',[48,49,32,51,52]),
		  ?line {ok,"01 34"} = asn1_wrapper:decode('PrimStrings','NsExpCon',lists:flatten(BytesNs21)),
		  ok
	  end,

    ok.

		       
other_strings(_Rules) ->

    %%==========================================================
    %% Ps ::= PrintableString
    %%==========================================================

    roundtrip('Ps', [47,23,99,75,47]),
    roundtrip('Ps', []),
    roundtrip('Ps11', "*0123456789*"),
		       
    %%==========================================================
    %% Vis ::= VisibleString
    %%==========================================================

    roundtrip('Vis', [47,23,99,75,47]),
    roundtrip('Vis', []),
    roundtrip('Vis8', "7654321001234567"),
    roundtrip('Vis8', []),
		       
    %%==========================================================
    %% IA5 ::= IA5String
    %%==========================================================

    roundtrip('IA5', [47,23,99,75,47]),
    roundtrip('IA5', []),

    IA5_1 = "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890",
    roundtrip('IA5', IA5_1),

    roundtrip('IA5Visible', lists:seq($\s, $~)),

    ok.


more_strings(_Rules) ->
    %%==========================================================
    %% Ts ::= TeletexString
    %%==========================================================

    roundtrip('Ts', [47,23,99,75,47]),
    roundtrip('Ts', []),

		       
    %%==========================================================
    %% Vxs ::= VideotexString
    %%==========================================================

    roundtrip('Vxs', [47,23,99,75,47]),
    roundtrip('Vxs', []),

    
    %%==========================================================
    %% Grs ::= GraphicString
    %%==========================================================

    roundtrip('Grs',[47,23,99,75,47]),
    roundtrip('Grs', []),
    

    %%==========================================================
    %% ODesc ::= ObjectDescriptor, test case for OTP-4161
    %%==========================================================

    roundtrip('ODesc', [79,98,106,101,99,116,68,101,115,99,114,
			105,112,116,111,114]),
    roundtrip('ODesc', []),
		       
    %%==========================================================
    %% Ges ::= GeneralString
    %%==========================================================

    roundtrip('Ges', [47,23,99,75,47]),
    roundtrip('Ges', []),

    ok.

		       
universal_string(Rules) ->
    
    %%==========================================================
    %% Us ::= UniversalString
    %%==========================================================
    
    roundtrip('Us', [{47,23,99,47},{0,0,55,66}]),
    
    ?line {ok,Bytes2} = 
	asn1_wrapper:encode('PrimStrings','Us',[{47,23,99,255},{0,0,0,201}]),
    ?line {ok,[{47,23,99,255},201]} = 
	asn1_wrapper:decode('PrimStrings','Us',lists:flatten(Bytes2)),
    
    roundtrip('Us', "Universal String"),
    roundtrip('Us', []),
    roundtrip('Us', [{47,23,99,47}]),
    
    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
    
		  ?line {ok,[{47,23,99,255},{0,0,2,201}]} = 
		      asn1_wrapper:decode('PrimStrings','Us',lists:flatten([16#3C,12,28,4,47,23,99,255,28,4,0,0,2,201])),
		  ?line {ok,[{47,23,99,255},{0,0,2,201}]} = 
		      asn1_wrapper:decode('PrimStrings','Us',lists:flatten([16#3C,16#80,28,4,47,23,99,255,28,4,0,0,2,201,0,0]));
	      _ ->
		  ok
	  end,


    Us1 = "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890",
    roundtrip('IA5', Us1),


%%==========================================================
%% UsCon ::= [70] UniversalString
%%==========================================================

    roundtrip('UsCon', [{47,23,99,255},{0,0,2,201}]),
    
    ?line {ok,Bytes12} = 
	asn1_wrapper:encode('PrimStrings','UsCon',[{47,23,99,255},{0,0,0,201}]),
    ?line {ok,[{47,23,99,255},201]} = 
	asn1_wrapper:decode('PrimStrings','UsCon',lists:flatten(Bytes12)),
    
    roundtrip('UsCon', "Universal String"),
    roundtrip('UsCon', []),
    
    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {ok,[{47,23,99,255},{0,0,2,201}]} = 
		      asn1_wrapper:decode('PrimStrings','UsCon',lists:flatten([16#BF,16#46,12,28,4,47,23,99,255,28,4,0,0,2,201])),
		  ?line {ok,[{47,23,99,255},{0,0,2,201}]} = 
		      asn1_wrapper:decode('PrimStrings','UsCon',lists:flatten([16#BF,16#46,16#80,28,4,47,23,99,255,28,4,0,0,2,201,0,0]));
	      _ -> ok
	  end,



%%==========================================================
%% UsExpCon ::= [71] EXPLICIT UniversalString
%%==========================================================

    roundtrip('UsExpCon', [{47,23,99,255},{0,0,2,201}]),
    
    ?line {ok,Bytes22} = 
	asn1_wrapper:encode('PrimStrings','UsExpCon',[{47,23,99,255},{0,0,0,201}]),
    ?line {ok,[{47,23,99,255},201]} = 
	asn1_wrapper:decode('PrimStrings','UsExpCon',lists:flatten(Bytes22)),
    
    roundtrip('UsExpCon', "Universal String"),
    roundtrip('UsExpCon', []),
    
    ?line case asn1_wrapper:erule(Rules) of
	      ber ->     
		  ?line {ok,[{47,23,99,255},{0,0,2,201}]} = 
		      asn1_wrapper:decode('PrimStrings','UsExpCon',lists:flatten([16#BF,16#47,14,60,12,28,4,47,23,99,255,28,4,0,0,2,201])),
		  ?line {ok,[{47,23,99,255},{0,0,2,201}]} = 
		      asn1_wrapper:decode('PrimStrings','UsExpCon',lists:flatten([16#BF,16#47,16,60,16#80,28,4,47,23,99,255,28,4,0,0,2,201,0,0]));
	      _ -> ok
	  end,

    ok.

		       
bmp_string(_Rules) ->
		       
    %%==========================================================
    %% BMP ::= BMPString
    %%==========================================================

    roundtrip('BMP', [{0,0,99,48},{0,0,2,201}]),
    
    ?line {ok,Bytes2} = 
	asn1_wrapper:encode('PrimStrings','BMP',[{0,0,0,48},{0,0,2,201}]),
    ?line {ok,[48,{0,0,2,201}]} = 
	asn1_wrapper:decode('PrimStrings','BMP',lists:flatten(Bytes2)),

    roundtrip('BMP', "BMP String"),
    roundtrip('BMP', []),

    BMP1 = "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890",
    roundtrip('BMP', BMP1),
		     
    ok.




		       
times(_Rules) ->

    %%==========================================================
    %% Gt ::= GeneralizedTime
    %%==========================================================

    roundtrip('Gt', "19970923110723.2"),
    roundtrip('Gt', "19970923110723.2Z"),
    roundtrip('Gt', "19970923110723.2-0500"),


    %%==========================================================
    %% UTC ::= UTCTime
    %%==========================================================

    roundtrip('UTC', "9709211107Z"),
    roundtrip('UTC', "9709211107-0500"),

    ok.


utf8_string(_Rules) ->
		       
    %%==========================================================
    %% UTF ::= UTF8String
    %%==========================================================

    %% test values in all ranges

    ValLbR1 = [16#00],
    ValUbR1 = [16#7f],
    ValLbR2 = [16#80],
    ValUbR2 = [16#7ff],
    ValLbR3 = [16#800],
    ValUbR3 = [16#ffff],
    ValLbR4 = [16#10000],
    ValUbR4 = [16#1fffff],
    ValLbR5 = [16#200000],
    ValUbR5 = [16#3ffffff],
    ValLbR6 = [16#4000000],
    ValUbR6 = [16#7fffffff],
    
    ?line {ok,UTF8L1} = asn1rt:utf8_list_to_binary(ValLbR1),
    ?line {ok,Bytes1} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L1),
    ?line {ok,Bin1} = asn1_wrapper:decode('PrimStrings','UTF',Bytes1),
    ?line {ok,ValLbR1} = wrapper_utf8_binary_to_list(Bin1),
    
    ?line {ok,UTF8L2} = asn1rt:utf8_list_to_binary(ValUbR1),
    ?line {ok,Bytes2} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L2),
    ?line {ok,Bin2} = asn1_wrapper:decode('PrimStrings','UTF',Bytes2),
    ?line {ok,ValUbR1} = wrapper_utf8_binary_to_list(Bin2),

    ?line {ok,UTF8L3} = asn1rt:utf8_list_to_binary(ValLbR2),
    ?line {ok,Bytes3} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L3),
    ?line {ok,Bin3} = asn1_wrapper:decode('PrimStrings','UTF',Bytes3),
    ?line {ok,ValLbR2} = wrapper_utf8_binary_to_list(Bin3),

    ?line {ok,UTF8L4} = asn1rt:utf8_list_to_binary(ValUbR2),
    ?line {ok,Bytes4} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L4),
    ?line {ok,Bin4} = asn1_wrapper:decode('PrimStrings','UTF',Bytes4),
    ?line {ok,ValUbR2} = wrapper_utf8_binary_to_list(Bin4),

    ?line {ok,UTF8L5} = asn1rt:utf8_list_to_binary(ValLbR3),
    ?line {ok,Bytes5} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L5),
    ?line {ok,Bin5} = asn1_wrapper:decode('PrimStrings','UTF',Bytes5),
    ?line {ok,ValLbR3} = wrapper_utf8_binary_to_list(Bin5),

    ?line {ok,UTF8L6} = asn1rt:utf8_list_to_binary(ValUbR3),
    ?line {ok,Bytes6} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L6),
    ?line {ok,Bin6} = asn1_wrapper:decode('PrimStrings','UTF',Bytes6),
    ?line {ok,ValUbR3} = wrapper_utf8_binary_to_list(Bin6),

    ?line {ok,UTF8L7} = asn1rt:utf8_list_to_binary(ValLbR4),
    ?line {ok,Bytes7} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L7),
    ?line {ok,Bin7} = asn1_wrapper:decode('PrimStrings','UTF',Bytes7),
    ?line {ok,ValLbR4} = wrapper_utf8_binary_to_list(Bin7),

    ?line {ok,UTF8L8} = asn1rt:utf8_list_to_binary(ValUbR4),
    ?line {ok,Bytes8} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L8),
    ?line {ok,Bin8} = asn1_wrapper:decode('PrimStrings','UTF',Bytes8),
    ?line {ok,ValUbR4} = wrapper_utf8_binary_to_list(Bin8),

    ?line {ok,UTF8L9} = asn1rt:utf8_list_to_binary(ValLbR5),
    ?line {ok,Bytes9} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L9),
    ?line {ok,Bin9} = asn1_wrapper:decode('PrimStrings','UTF',Bytes9),
    ?line {ok,ValLbR5} = wrapper_utf8_binary_to_list(Bin9),

    ?line {ok,UTF8L10} = asn1rt:utf8_list_to_binary(ValUbR5),
    ?line {ok,Bytes10} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L10),
    ?line {ok,Bin10} = asn1_wrapper:decode('PrimStrings','UTF',Bytes10),
    ?line {ok,ValUbR5} = wrapper_utf8_binary_to_list(Bin10),

    ?line {ok,UTF8L11} = asn1rt:utf8_list_to_binary(ValLbR6),
    ?line {ok,Bytes11} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L11),
    ?line {ok,Bin11} = asn1_wrapper:decode('PrimStrings','UTF',Bytes11),
    ?line {ok,ValLbR6} = wrapper_utf8_binary_to_list(Bin11),

    ?line {ok,UTF8L12} = asn1rt:utf8_list_to_binary(ValUbR6),
    ?line {ok,Bytes12} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L12),
    ?line {ok,Bin12} = asn1_wrapper:decode('PrimStrings','UTF',Bytes12),
    ?line {ok,ValUbR6} = wrapper_utf8_binary_to_list(Bin12),
    
    LVal = ValLbR1++ValUbR1++ValLbR2++ValUbR2++ValLbR3++ValUbR3++
	ValLbR4++ValUbR4++ValLbR5++ValUbR5++ValLbR6++ValUbR6,
    LongVal = LVal++LVal++LVal++LVal++LVal++LVal++LVal++"hello",
    
    ?line {ok,UTF8L13} = asn1rt:utf8_list_to_binary(LongVal),
    ?line {ok,Bytes13} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L13),
    ?line {ok,Bin13} = asn1_wrapper:decode('PrimStrings','UTF',Bytes13),
    ?line {ok,LongVal} = wrapper_utf8_binary_to_list(Bin13).

wrapper_utf8_binary_to_list(L) when is_list(L) ->
    asn1rt:utf8_binary_to_list(list_to_binary(L));
wrapper_utf8_binary_to_list(B) ->
    asn1rt:utf8_binary_to_list(B).

roundtrip(Type, Value) ->
    {ok,Encoded} = 'PrimStrings':encode(Type, Value),
    {ok,Value} = 'PrimStrings':decode(Type, Encoded),
    ok.

bs_roundtrip(Type, Value) ->
    bs_roundtrip(Type, Value, Value).

bs_roundtrip(Type, Value, Expected) ->
    M = 'PrimStrings',
    {ok,Encoded} = M:encode(Type, Value),
    case M:decode(Type, Encoded) of
	{ok,Expected} ->
	    ok;
	{ok,Other} ->
	    Expected = convert(Other, Expected)
    end.

bs_decode(Type, Encoded, Expected) ->
    M = 'PrimStrings',
    case M:decode(Type, Encoded) of
	{ok,Expected} ->
	    ok;
	{ok,Other} ->
	    Expected = convert(Other, Expected)
    end.

convert(Val, E) when is_bitstring(Val) ->
    convert_1(Val, E);
convert({Unused,Bin}, E) ->
    Sz = bit_size(Bin) - Unused,
    <<Val:Sz/bitstring,_:Unused>> = Bin,
    convert_1(Val, E);
convert(List, E) when is_list(List) ->
    Val = << <<B:1>> || B <- List >>,
    convert_1(Val, E).

convert_1(Val, E) when is_list(E) ->
    [B || <<B:1>> <= Val];
convert_1(Val, E) when is_bitstring(E) -> Val.
