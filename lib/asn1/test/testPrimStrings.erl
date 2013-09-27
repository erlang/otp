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

-export([bit_string/2]).
-export([octet_string/1]).
-export([numeric_string/1]).
-export([other_strings/1]).
-export([more_strings/1]).
-export([universal_string/1]).
-export([bmp_string/1]).
-export([times/1]).
-export([utf8_string/1]).
-export([fragmented/1]).

-include_lib("test_server/include/test_server.hrl").

fragmented(Rules) ->
    Lens = fragmented_lengths(),
    fragmented_octet_string(Rules, Lens),
    case Rules of
	per ->
	    %% NYI.
	    ok;
	_ ->
	    fragmented_strings(Lens)
    end.

fragmented_strings(Lens) ->
    Types = ['Ns','Ps','Ps11','Vis','IA5'],
    [fragmented_strings(Len, Types) || Len <- Lens],
    ok.

fragmented_strings(Len, Types) ->
    Str = make_ns_value(Len),
    [roundtrip(Type, Str) || Type <- Types],
    ok.

make_ns_value(0) -> [];
make_ns_value(N) -> [($0 - 1) + random:uniform(10)|make_ns_value(N-1)].

fragmented_lengths() ->
    K16 = 1 bsl 14,
    K32 = K16 + K16,
    K48 = K32 + K16,
    K64 = K48 + K16,
    [0,1,14,15,16,17,127,128,
     K16-1,K16,K16+1,K16+(1 bsl 7)-1,K16+(1 bsl 7),K16+(1 bsl 7)+1,
     K32-1,K32,K32+1,K32+(1 bsl 7)-1,K32+(1 bsl 7),K32+(1 bsl 7)+1,
     K48-1,K48,K48+1,K48+(1 bsl 7)-1,K48+(1 bsl 7),K48+(1 bsl 7)+1,
     K64-1,K64,K64+1,K64+(1 bsl 7)-1,K64+(1 bsl 7),K64+(1 bsl 7)+1,
     K64+K16-1,K64+K16,K64+K16+1].

bit_string(Rules, Opts) ->
    
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


    case {Rules,Opts} of
	{ber,[]} ->
	    bs_decode('Bs1', <<35,8,3,2,0,73,3,2,4,32>>,
		      [0,1,0,0,1,0,0,1,0,0,1,0]),
	    bs_decode('Bs1', <<35,9,3,2,0,234,3,3,7,156,0>>,
		      [1,1,1,0,1,0,1,0,1,0,0,1,1,1,0,0,0]),
	    bs_decode('Bs1', <<35,128,3,2,0,234,3,3,7,156,0,0,0>>,
		      [1,1,1,0,1,0,1,0,1,0,0,1,1,1,0,0,0]);
	_ ->
	    %% DER, PER, UPER
	    consistent_def_enc('BsDef1',
			       [2#111101,
				[1,0,1,1,1,1],
				{2,<<2#101111:6,0:2>>},
				<<2#101111:6>>]),
	    consistent_def_enc('BsDef2',
			       [[1,1,0,1, 1,1,1,0, 1,0,1,0, 1,1,0,1,
				 1,0,1,1, 1,1,1,0, 1,1,1,0, 1,1,1,1],
				{0,<<16#DEADBEEF:4/unit:8>>},
				<<16#DEADBEEF:4/unit:8>>])
    end,

    
    %%==========================================================
    %% Bs2 ::= BIT STRING {su(0), mo(1), tu(2), we(3), th(4), fr(5), sa(6) } (SIZE (7))
    %%==========================================================
    
    roundtrip('Bs2', [mo,tu,fr]),
    roundtrip('Bs2', [0,1,1,0,0,1,0], [mo,tu,fr]),

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
    
    case Rules of
	ber ->
	    bs_decode('BsPri', <<223,61,4,5,75,226,96>>,
		      [0,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,0,1,1]),
	    bs_decode('BsPri', <<255,61,128,3,4,5,75,226,96,0,0>>,
		      [0,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,0,1,1]),
	    bs_decode('BsPri', <<255,61,9,3,2,0,75,3,3,5,226,96>>,
		      [0,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,0,1,1]),
	    bs_decode('BsPri', <<255,61,128,3,2,0,75,3,3,5,226,96,0,0>>,
		      [0,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,0,1,1]);
	_ ->
	    ok
    end,
    
    
    %%==========================================================
    %% BsExpPri ::= [PRIVATE 61] EXPLICIT BIT STRING
    %%==========================================================
    
    bs_roundtrip('BsExpPri', 45, [1,0,1,1,0,1]),
    bs_roundtrip('BsExpPri', 211, [1,1,0,0,1,0,1,1]),
    
    case Rules of
	ber ->
	    bs_decode('BsExpPri', <<255,61,6,3,4,5,75,226,96>>,
		      [0,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,0,1,1]);
	_ ->
	    ok
    end,
    
    %%==========================================================
    %% TestS ::= BIT STRING {a(0),b(1)} (SIZE (3..8)), test case for OTP-4353
    %%==========================================================

    roundtrip('TestS', [a]),

    %%==========================================================
    %% PersonalStatus ::= BIT STRING {married(0),employed(1),
    %%    veteran(2), collegeGraduate(3)}, test case for OTP-5710
    %%==========================================================
    
    {ok,Bytes54} = 'BitStr':encode('PersonalStatus', []),
    {ok,[]} = 'BitStr':decode('PersonalStatus', Bytes54),
    
    %%==========================================================
    %% BS5932 ::= BIT STRING (SIZE (5..MAX))
    %% test case for OTP-5932
    %%==========================================================
    bs_roundtrip('BSMAX', [1,0,1,0,1]),
    case Rules of
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

    bs_roundtrip('TransportLayerAddress', [0,1,1,0]),

    case Rules of
	ber -> ok;
	_ -> per_bs_strings()
    end.

consistent_def_enc(Type, Vs) ->
    M = 'PrimStrings',
    {ok,Enc} = M:encode(Type, {Type,asn1_DEFAULT}),
    {ok,Val} = M:decode(Type, Enc),

    %% Ensure that the value has the correct format.
    case {M:bit_string_format(),Val} of
	{bitstring,{_,Bs}} when is_bitstring(Bs) -> ok;
	{compact,{_,{Unused,Bin}}} when is_integer(Unused),
					is_binary(Bin) -> ok;
	{legacy,{_,Bs}} when is_list(Bs) -> ok
    end,

    %% All values should be recognized and encoded as the
    %% the default value (i.e. not encoded at all).
    _ = [{ok,Enc} = M:encode(Type, {Type,V}) || V <- Vs],
    ok.

%% The PER encoding rules requires that a BIT STRING with
%% named positions should never have any trailing zeroes
%% (except to reach the minimum number of bits as given by
%% a SIZE constraint).

per_bs_strings() ->
    bs_roundtrip('Bs3', [0,0,1,0,0,0,0], [tu]),
    bs_roundtrip('Bs3', <<2#0010000:7>>, [tu]),
    bs_roundtrip('Bs3', {1,<<2#00100000:8>>}, [tu]),

    bs_roundtrip('Bs4', [0,1,1,0,0,1,0], [mo,tu,fr]),
    bs_roundtrip('Bs4', <<2#0110010:7>>, [mo,tu,fr]),
    bs_roundtrip('Bs4', {1,<<2#01100100:8>>}, [mo,tu,fr]),

    bs_roundtrip('Bs4', [0,1,1,0,0,0,0], [mo,tu]),
    bs_roundtrip('Bs4', <<2#011:3,0:32>>, [mo,tu]),
    bs_roundtrip('Bs4', {5,<<2#011:3,0:32,0:5>>}, [mo,tu]),

    [per_trailing_zeroes(B) || B <- lists:seq(0, 255)],
    ok.

%% Trailing zeroes should be removed from BIT STRINGs with named
%% bit positions.

per_trailing_zeroes(Byte) ->
    L = lists:reverse(make_bit_list(Byte+16#10000)),
    L = make_bit_list(Byte+16#10000, []),
    Pos = positions(L, 0),
    ExpectedSz = case lists:last(Pos) of
		     su -> 1;
		     {bit,LastBitPos} -> LastBitPos+1
		 end,

    %% List of zeroes and ones.
    named_roundtrip(L, Pos, ExpectedSz),
    named_roundtrip(L++[0,0,0,0,0], Pos, ExpectedSz),

    %% Bitstrings.
    Bs = << <<B:1>> || B <- L >>,
    Sz = bit_size(Bs),
    named_roundtrip(Bs, Pos, ExpectedSz),
    Bin = <<Bs:Sz/bits,0:16,0:7>>,
    named_roundtrip(Bin, Pos, ExpectedSz),

    %% Compact bitstring.
    named_roundtrip({7,Bin}, Pos, ExpectedSz),

    %% Integer bitstring (obsolete).
    IntBs = intlist_to_integer(L, 0, 0),
    named_roundtrip(IntBs, Pos, ExpectedSz),

    ok.

make_bit_list(0) -> [];
make_bit_list(B) -> [B band 1|make_bit_list(B bsr 1)].

make_bit_list(0, Acc) -> Acc;
make_bit_list(B, Acc) -> make_bit_list(B bsr 1, [B band 1|Acc]).

positions([1|T], 0) -> [su|positions(T, 1)];
positions([1|T], Pos) -> [{bit,Pos}|positions(T, Pos+1)];
positions([0|T], Pos) -> positions(T, Pos+1);
positions([], _) -> [].

intlist_to_integer([B|T], Shift, Acc) ->
    intlist_to_integer(T, Shift+1, (B bsl Shift) + Acc);
intlist_to_integer([], _, Acc) -> Acc.

named_roundtrip(Value, Expected, ExpectedSz) ->
    M = 'PrimStrings',
    Type = 'Bs4',
    {ok,Encoded} = M:encode(Type, Value),
    {ok,Encoded} = M:encode(Type, Expected),
    {ok,Expected} = M:decode(Type, Encoded),

    %% Verify the size in the first byte.
    <<ExpectedSz:8,_/bits>> = Encoded,
    ok.

octet_string(Rules) ->

    %%==========================================================
    %% Os ::= OCTET STRING
    %%==========================================================

    case Rules of
	ber ->
	    {ok,"Jones"} =
		'PrimStrings':decode('Os', <<4,5,16#4A,16#6F,16#6E,16#65,16#73>>),
	    {ok,"Jones"} =
		'PrimStrings':decode('Os', <<36,9,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73>>),
	    {ok,"Jones"} =
		'PrimStrings':decode('Os', <<36,128,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73,0,0>>),
	    ok;
	_ ->
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


    case Rules of
	ber ->
	    {ok,"Jones"} = 'PrimStrings':decode('OsExpApp', <<127,62,7,4,5,16#4A,16#6F,16#6E,16#65,16#73>>),
	    {ok,"Jones"} = 'PrimStrings':decode('OsExpApp', <<127,62,11,36,9,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73>>),
	    {ok,"Jones"} = 'PrimStrings':decode('OsExpApp', <<127,62,13,36,128,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73,0,0>>),
	    {ok,"Jones"} = 'PrimStrings':decode('OsExpApp', <<127,62,128,36,128,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73,0,0,0,0>>),
	    {ok,"JonesJones"} = 'PrimStrings':decode('OsExpApp', <<127,62,128,36,128,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73,0,0,36,128,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73,0,0,0,0>>),
	    ok;

	_->
	    ok
    end,

    S255 = lists:seq(1, 255),
    Strings = {type,true,"","1","12","345",true,
	       S255,[$a|S255],[$a,$b|S255],397},
    p_roundtrip('OsFixedStrings', Strings),
    p_roundtrip('OsFixedStringsExt', Strings),
    p_roundtrip('OsVarStringsExt', Strings),
    ShortenedStrings = shorten_by_two(Strings),
    p_roundtrip('OsFixedStringsExt', ShortenedStrings),
    p_roundtrip('OsVarStringsExt', ShortenedStrings),
    ok.

fragmented_octet_string(Erules, Lens) ->
    Types = ['Os','OsFrag','OsFragExt'],
    [fragmented_octet_string(Erules, Types, L) || L <- Lens],
    fragmented_octet_string(Erules, ['FixedOs65536'], 65536),
    fragmented_octet_string(Erules, ['FixedOs65537'], 65537),
    fragmented_octet_string(Erules, ['FixedOs65536Ext'], 65536),
    fragmented_octet_string(Erules, ['FixedOs65537Ext'], 65537),

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
    roundtrip('Ns', "01 34"),
    case Rules of
	ber ->
	    {ok,"Jones"} = 'PrimStrings':decode('Ns',
						<<16#12,5,16#4A,16#6F,
						 16#6E,16#65,16#73>>),
	    {ok,"Jones"} = 'PrimStrings':decode('Ns',
						<<16#32,9,18,3,16#4A,16#6F,
						 16#6E,18,2,16#65,16#73>>),
	    {ok,"Jones"} = 'PrimStrings':decode('Ns',
						<<16#32,128,18,3,16#4A,16#6F,
						 16#6E,18,2,16#65,16#73,0,0>>),
	    ok;
	_ ->
	    ok
    end,

    %%==========================================================
    %% NsCon ::= [70] NumericString
    %%==========================================================

    roundtrip('NsCon', []),
    roundtrip('NsCon', "01 34"),

    case Rules of
	ber ->
	    {ok,"Jones"} = 'PrimStrings':decode('NsCon', <<16#9F,16#46,5,16#4A,16#6F,16#6E,16#65,16#73>>),
	    {ok,"Jones"} = 'PrimStrings':decode('NsCon', <<16#BF,16#46,9,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73>>),
	    {ok,"Jones"} = 'PrimStrings':decode('NsCon', <<16#BF,16#46,128,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73,0,0>>),
	    ok;
	_ ->
	    ok
    end,

    %%==========================================================
    %% NsExpCon ::= [71] EXPLICIT NumericString
    %%==========================================================

    roundtrip('NsExpCon', []),
    roundtrip('NsExpCon', "01 34"),

    case Rules of
	ber ->
	    {ok,"Jones"} = 'PrimStrings':decode('NsExpCon', <<16#BF,16#47,16#07,16#12,16#05,16#4A,16#6F,16#6E,16#65,16#73>>),
	    {ok,"Jones"} = 'PrimStrings':decode('NsExpCon', <<16#BF,16#47,11,16#32,9,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73>>),
	    {ok,"Jones"} = 'PrimStrings':decode('NsExpCon', <<16#BF,16#47,128,16#32,128,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73,0,0,0,0>>),
	    {ok,"JonesJones"} = 'PrimStrings':decode('NsExpCon', <<16#BF,16#47,26,16#32,128,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73,0,0,16#32,128,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73,0,0>>),
	    {ok,"JonesJones"} = 'PrimStrings':decode('NsExpCon', <<16#BF,16#47,128,16#32,128,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73,0,0,16#32,128,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73,0,0,0,0>>),
	    ok;
	_ ->
	    ok
    end.

		       
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

    S255 = lists:seq(0, 127) ++ lists:seq(1, 127),
    Strings = {type,true,"","1","12","345",true,"6789",true,
	       S255,[$a|S255],[$a,$b|S255],397},
    p_roundtrip('IA5FixedStrings', Strings),
    p_roundtrip('IA5FixedStringsExt', Strings),
    p_roundtrip('IA5VarStringsExt', Strings),
    ShortenedStrings = shorten_by_two(Strings),
    p_roundtrip('IA5VarStringsExt', ShortenedStrings),

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
    roundtrip('Us',
	      [{47,23,99,255},{0,0,0,201}],
	      [{47,23,99,255},201]),
    roundtrip('Us', "Universal String"),
    roundtrip('Us', []),
    roundtrip('Us', [{47,23,99,47}]),
    
    case Rules of
	ber ->
	    {ok,[{47,23,99,255},{0,0,2,201}]} =
		'PrimStrings':decode('Us', <<16#3C,12,28,4,47,23,99,255,28,4,0,0,2,201>>),
	    {ok,[{47,23,99,255},{0,0,2,201}]} =
		'PrimStrings':decode('Us', <<16#3C,16#80,28,4,47,23,99,255,28,4,0,0,2,201,0,0>>);
	      _ ->
		  ok
	  end,


    Us1 = "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890",
    roundtrip('IA5', Us1),


%%==========================================================
%% UsCon ::= [70] UniversalString
%%==========================================================

    roundtrip('UsCon', [{47,23,99,255},{0,0,2,201}]),
    roundtrip('UsCon',
	      [{47,23,99,255},{0,0,0,201}],
	      [{47,23,99,255},201]),
    roundtrip('UsCon', "Universal String"),
    roundtrip('UsCon', []),
    
    case Rules of
	ber ->
	    {ok,[{47,23,99,255},{0,0,2,201}]} =
		'PrimStrings':decode('UsCon', <<16#BF,16#46,12,28,4,47,23,99,255,28,4,0,0,2,201>>),
	    {ok,[{47,23,99,255},{0,0,2,201}]} =
		'PrimStrings':decode('UsCon', <<16#BF,16#46,16#80,28,4,47,23,99,255,28,4,0,0,2,201,0,0>>);
	_ ->
	    ok
    end,


%%==========================================================
%% UsExpCon ::= [71] EXPLICIT UniversalString
%%==========================================================

    roundtrip('UsExpCon', [{47,23,99,255},{0,0,2,201}]),
    roundtrip('UsExpCon',
	      [{47,23,99,255},{0,0,0,201}],
	      [{47,23,99,255},201]),
    roundtrip('UsExpCon', "Universal String"),
    roundtrip('UsExpCon', []),
    
    case Rules of
	ber ->
	    {ok,[{47,23,99,255},{0,0,2,201}]} =
		'PrimStrings':decode('UsExpCon', <<16#BF,16#47,14,60,12,28,4,47,23,99,255,28,4,0,0,2,201>>),
	    {ok,[{47,23,99,255},{0,0,2,201}]} =
		'PrimStrings':decode('UsExpCon', <<16#BF,16#47,16,60,16#80,28,4,47,23,99,255,28,4,0,0,2,201,0,0>>);
	_ ->
	    ok
    end.

		       
bmp_string(_Rules) ->
		       
    %%==========================================================
    %% BMP ::= BMPString
    %%==========================================================

    roundtrip('BMP', [{0,0,99,48},{0,0,2,201}]),
    roundtrip('BMP',
	      [{0,0,0,48},{0,0,2,201}],
	      [48,{0,0,2,201}]),
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

    AllRanges = [16#00,
		 16#7f,
		 16#80,
		 16#7ff,
		 16#800,
		 16#ffff,
		 16#10000,
		 16#1fffff,
		 16#200000,
		 16#3ffffff,
		 16#4000000,
		 16#7fffffff],
    [begin
	 {ok,UTF8} = asn1rt:utf8_list_to_binary([Char]),
	 {ok,[Char]} = asn1rt:utf8_binary_to_list(UTF8),
	 roundtrip('UTF', UTF8)
     end || Char <- AllRanges],

    {ok,UTF8} = asn1rt:utf8_list_to_binary(AllRanges),
    {ok,AllRanges} = asn1rt:utf8_binary_to_list(UTF8),
    roundtrip('UTF', UTF8),
    ok.


shorten_by_two(Tuple) ->
    L = [case E of
	     [_,_|T] -> T;
	     _ -> E
	 end || E <- tuple_to_list(Tuple)],
    list_to_tuple(L).

p_roundtrip(Type, Value0) ->
    Value = setelement(1, Value0, Type),
    roundtrip(Type, Value).

roundtrip(Type, Value) ->
    roundtrip(Type, Value, Value).

roundtrip(Type, Value, Expected) ->
    asn1_test_lib:roundtrip('PrimStrings', Type, Value, Expected).

bs_roundtrip(Type, Value) ->
    bs_roundtrip(Type, Value, Value).

bs_roundtrip(Type, Value, Expected) ->
    M = 'PrimStrings',
    {ok,Encoded} = M:encode(Type, Value),
    {ok,Encoded} = M:encode(Type, Expected),
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
