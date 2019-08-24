%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%%
-module(testSeqSetDefaultVal).
-export([main/2]).

-include("External.hrl").
-include_lib("common_test/include/ct.hrl").

-record('SeqInts',{a = asn1_DEFAULT, 
		   b = asn1_DEFAULT, 
		   c = asn1_DEFAULT,
		   d = asn1_DEFAULT}).
-record('SetInts',{a = asn1_DEFAULT, 
		   b = asn1_DEFAULT, 
		   c = asn1_DEFAULT,
		   d = asn1_DEFAULT}).
-record('SeqBS',{a = asn1_DEFAULT, 
		 b = asn1_DEFAULT, 
		 c = asn1_DEFAULT,
		 d = asn1_DEFAULT,
		 e = asn1_DEFAULT}).
-record('SeqBS2',{bs = asn1_DEFAULT}).
-record('SetBS',{a = asn1_DEFAULT, 
		 b = asn1_DEFAULT, 
		 c = asn1_DEFAULT,
		 d = asn1_DEFAULT}).
-record('SeqOS',{a = asn1_DEFAULT, 
		 b = asn1_DEFAULT, 
		 c = asn1_DEFAULT}).
-record('SetOS',{a = asn1_DEFAULT, 
		 b = asn1_DEFAULT, 
		 c = asn1_DEFAULT}).
-record('SeqOI',{a = asn1_DEFAULT, 
		 b = asn1_DEFAULT, 
		 c = asn1_DEFAULT}).
-record('SetOI',{a = asn1_DEFAULT, 
		 b = asn1_DEFAULT, 
		 c = asn1_DEFAULT}).
-record('SeqEnum',{a = asn1_DEFAULT, 
		   b = asn1_DEFAULT}).
-record('SetEnum',{a = asn1_DEFAULT, 
		   b = asn1_DEFAULT}).
-record('SeqIntBool',{a = asn1_DEFAULT, 
		      b = asn1_DEFAULT, 
		      c = asn1_DEFAULT}).
-record('SeqIntBool_a',{aa = asn1_DEFAULT, 
			ab = asn1_DEFAULT}).
-record('SetIntBool',{a = asn1_DEFAULT, 
		      b = asn1_DEFAULT, 
		      c = asn1_DEFAULT}).
-record('SetIntBool_a',{aa = asn1_DEFAULT, 
			ab = asn1_DEFAULT}).
-record('SeqStrings',{a = asn1_DEFAULT, 
		      b1 = asn1_DEFAULT,
		      b2 = asn1_DEFAULT,
		      b3 = asn1_DEFAULT,
		      c = asn1_DEFAULT,
		      d = asn1_DEFAULT}).
-record('SetStrings',{a = asn1_DEFAULT, 
		      b1 = asn1_DEFAULT,
		      b2 = asn1_DEFAULT,
		      b3 = asn1_DEFAULT,
		      c = asn1_DEFAULT,
		      d = asn1_DEFAULT}).
-record('S1',{a = asn1_DEFAULT, 
	      b = asn1_DEFAULT}).
-record('S1_a',{aa = asn1_DEFAULT, 
		ab = asn1_DEFAULT}).
-record('S2',{a = asn1_DEFAULT, b=asn1_NOVALUE}).
-record('S3',{a = asn1_DEFAULT, 
	      b = asn1_DEFAULT,
	      c = asn1_DEFAULT,
	      d = asn1_DEFAULT}).
-record('S3set',{a = asn1_DEFAULT, 
		 b = asn1_DEFAULT}).
-record('S4',{a = asn1_DEFAULT, 
	      b = asn1_DEFAULT}).
-record('S4_b',{ba = asn1_DEFAULT, 
		bb = asn1_DEFAULT}).
-record('SeqNamedInts',
	{i1 = asn1_DEFAULT,
	 i2 = asn1_DEFAULT}).
-record('S5',{s3 = asn1_DEFAULT,
	      so = asn1_DEFAULT,
	      soe = asn1_DEFAULT}).
-record('SOI', {soi = asn1_DEFAULT}).

main(ber, []) ->
    %% Nothing to test because plain BER will only use
    %% default values when explicitly told to do so by
    %% asn1_DEFAULT.
    ok;
main(Rule, Opts) ->
    %% DER, PER, UPER. These encodings should not encode
    %% values that are equal to the default value.

    case {Rule,Opts} of
	{ber,[der]} ->
	    der(),
	    case 'Default':legacy_erlang_types() of
		false -> der_new_types();
		true -> der_legacy()
	    end;
	{_,_} ->
	    ok
    end,

    Ts = [{#'SeqInts'{},
	   [{#'SeqInts'.c,
	     [asn1_DEFAULT,
	      three,
	      3]}]},

	  {#'SeqBS'{},
	   [{#'SeqBS'.a,
	     [asn1_DEFAULT,			%Always.
	      <<1:1,0:1,1:1,0:1,1:1,1:1,0:1>>],
	     [2#0110101,			%Legacy only.
	      [1,0,1,0,1,1,0],
	      {1,<<16#AC>>}]},
	    {#'SeqBS'.b,
	     [asn1_DEFAULT,
	      <<16#A8:8,16#A:4>>],
	     [2#10100010101,
	      [1,0,1,0,1,0,0,0,1,0,1,0],
	      {4,<<16#A8,16#A0>>}]},
	    {#'SeqBS'.c,
	     [asn1_DEFAULT,
	      [second],
	      <<1:2>>],
	     [[0,1],
	      {6,<<0:1,1:1,0:6>>}]},
	    {#'SeqBS'.c,			%Zeroes on the right
	     [asn1_DEFAULT,
	      [second],
	      <<1:2,0:17>>],
	     [[0,1,0,0,0],
	      {4,<<0:1,1:1,0:6>>}]},
	    {#'SeqBS'.d,
	     [asn1_DEFAULT,
	      <<2#1001:4>>],
	     [2#1001,
	      [1,0,0,1],
	      {4,<<2#1001:4,0:4>>}]},
	    {#'SeqBS'.e,
	     [asn1_DEFAULT,
	      <<2#01011010:8>>],
	     [[0,1,0,1,1,0,1,0],
	      {0,<<2#01011010:8>>}]},
	    %% Not EQUAL to DEFAULT.
	    {#'SeqBS'.b,
	     [<<6:3>>],
	     [[1,1,0],				%Not equal to DEFAULT
	      {5,<<6:3,0:5>>}]}
	   ]},

	  {#'SeqOS'{},
	   [{#'SeqOS'.a,
	     [asn1_DEFAULT,
	      <<172>>]}]},

	  {#'SeqOI'{},
	   [{#'SeqOI'.a,
	     [asn1_DEFAULT,
	      {1,2,14,15}]},
	    {#'SeqOI'.b,
	     [asn1_DEFAULT,
	      %%	      {iso,'member-body',250,3,4},
	      {1,2,250,3,4}]},
	    {#'SeqOI'.c,
	     [asn1_DEFAULT,
	      %%	      {iso,standard,8571,2,250,4},
	      {1,0,8571,2,250,4}]}]}
	 ],
    R0 = [[consistency(Rec, PosVs) || PosVs <- Fs] || {Rec,Fs} <- Ts],
    case lists:flatten(R0) of
	[] ->
	    ok;
	[_|_]=R ->
	    io:format("~p\n", [R]),
	    ?t:fail()
    end.

legacy_filter({_,_}=Keep) ->
    Keep;
legacy_filter({Rec,Standard,Legacy}) ->
    case 'Default':legacy_erlang_types() of
	false ->
	    {Rec,Standard};
	true ->
	    {Rec,Standard++Legacy}
    end.

consistency(Rec0, PosVs) ->
    {Pos,[V|Vs]=AllVs} = legacy_filter(PosVs),
    T = element(1, Rec0),
    io:format("~p: ~p\n", [T,AllVs]),
    Rec = setelement(Pos, Rec0, V),
    {ok,Enc} = 'Default':encode(T, Rec),
    {ok,_SmokeTest} = 'Default':decode(T, Enc),
    consistency_1(Vs, Rec0, Pos, Enc).

consistency_1([V|Vs], Rec0, Pos, Enc) ->
    Rec = setelement(Pos, Rec0, V),
    case 'Default':encode(element(1, Rec), Rec) of
	{ok,Enc} ->
	    consistency_1(Vs, Rec0, Pos, Enc);
	{ok,WrongEnc} ->
	    [{Rec,{wrong,WrongEnc},{should_be,Enc}}|
	     consistency_1(Vs, Rec0, Pos, Enc)]
    end;
consistency_1([], _, _, _) -> [].

der() ->
    io:put_chars("Performing DER-specific tests..."),
    roundtrip(<<48,0>>,
	      'SeqInts',
	      #'SeqInts'{a=asn1_DEFAULT,b=asn1_DEFAULT,
			 c=asn1_DEFAULT,d=asn1_DEFAULT},
	      #'SeqInts'{a=1,b=-1,c=3,d=1}),
    roundtrip(<<48,0>>,
	      'SeqInts',
	      #'SeqInts'{a=1,b=-1,c=three,d=1},
	      #'SeqInts'{a=1,b=-1,c=3,d=1}),

    roundtrip(<<49,0>>,
	      'SetInts',
	      #'SetInts'{a=asn1_DEFAULT,b=asn1_DEFAULT,
			 c=asn1_DEFAULT,d=asn1_DEFAULT},
	      #'SetInts'{a=1,b=-1,c=3,d=1}),
    roundtrip(<<49,0>>,
	      'SetInts',
	      #'SetInts'{a=1,b=-1,c=three,d=1},
	      #'SetInts'{a=1,b=-1,c=3,d=1}),

    roundtrip(<<48,0>>,
	      'SeqOI',
	      #'SeqOI'{a={1,2,14,15},
		       b={iso,'member-body',250,3,4},
		       c={iso,standard,8571,2,250,4}},
	      #'SeqOI'{a={1,2,14,15},
		       b={1,2,250,3,4},
		       c={1,0,8571,2,250,4}}),

    roundtrip(<<49,0>>,
	      'SetOI',
	      #'SetOI'{a={1,2,14,15},
		       b={iso,'member-body',250,3,4},
		       c={iso,standard,8571,2,250,4}},
	      #'SetOI'{a={1,2,14,15},
		       b={1,2,250,3,4},
		       c={1,0,8571,2,250,4}}),

    roundtrip(<<48,0>>, 'SeqEnum', #'SeqEnum'{a=b4,b=b2}),
    roundtrip(<<49,0>>, 'SetEnum', #'SetEnum'{a=b4,b=b2}),

    roundtrip(<<48,0>>,
	      'SeqIntBool',
	      #'SeqIntBool'{a=#'SeqIntBool_a'{aa=12,ab=13},
			    b=#'S2'{a=14,b=true},
			    c=#'S2'{a=15,b=false}}),
    roundtrip(<<48,0>>,
	      'SeqIntBool',
	      #'SeqIntBool'{a=asn1_DEFAULT,b=asn1_DEFAULT,c=asn1_DEFAULT},
	      #'SeqIntBool'{a=#'SeqIntBool_a'{aa=12,ab=13},
			    b=#'S2'{a=14,b=true},
			    c=#'S2'{a=15,b=false}}),

    roundtrip(<<49,0>>,
	      'SetIntBool',
	      #'SetIntBool'{a=#'SetIntBool_a'{aa=12,ab=13},
			    b=#'S2'{a=14,b=true},
			    c=#'S2'{a=15,b=false}}),
    roundtrip(<<49,0>>,
	      'SetIntBool',
	      #'SetIntBool'{a=asn1_DEFAULT,b=asn1_DEFAULT,c=asn1_DEFAULT},
	      #'SetIntBool'{a=#'SetIntBool_a'{aa=12,ab=13},
			    b=#'S2'{a=14,b=true},
			    c=#'S2'{a=15,b=false}}),

    roundtrip(<<48,0>>,
	      'SeqStrings',
	      #'SeqStrings'{a="123456789",b1="abcdef",
			    b2={0,13},
			    b3={"First line",{0,13},"Second line"},
			    c="Printable string",
			    d={0,0,1,14}},
	      #'SeqStrings'{a="123456789",b1="abcdef",
			    b2=[0,13],
			    b3=["First line",[0,13],"Second line"],
			    c="Printable string",
			    d=[0,0,1,14]}),

    roundtrip(<<49,0>>,
	      'SetStrings',
	      #'SetStrings'{a="123456789",b1="abcdef",
			    b2={0,13},
			    b3={"First line",{0,13},"Second line"},
			    c="Printable string",
			    d={0,0,1,14}},
	      #'SetStrings'{a="123456789",b1="abcdef",
			    b2=[0,13],
			    b3=["First line",[0,13],"Second line"],
			    c="Printable string",
			    d=[0,0,1,14]}),

    roundtrip(<<48,0>>,
	      'S1',
	      #'S1'{a=#'S1_a'{aa=1,ab=#'S2'{a=2,b=true}},
		    b=#'S4'{a=#'S2'{a=2,b=true},b=#'S4_b'{ba=true,bb=5}}}),

    roundtrip(<<48,3,129,1,255>>, 'S2', #'S2'{a=1,b=true}),

    roundtrip(<<48,0>>,
	      'S3',
	      #'S3'{a="\v\f\r",
		    b=[{a,11},{b,true},{c,13}],
		    c=[1,2,3,4],
		    d=[#'S2'{a=20,b=true},#'S2'{a=30,b=false}]}),
    roundtrip(<<48,0>>,
	      'S3',
	      #'S3'{a=[11,13,12],
		    b=[{b,true},{a,11},{c,13}],
		    c=[3,4,1,2],
		    d=[#'S2'{a=30,b=false},#'S2'{a=20,b=true}]},
	      #'S3'{a=[11,12,13],
		    b=[{a,11},{b,true},{c,13}],
		    c=[1,2,3,4],
		    d=[#'S2'{a=20,b=true},#'S2'{a=30,b=false}]}),
    roundtrip(<<48,0>>,
	      'S3',
	      #'S3'{a=asn1_DEFAULT,b=asn1_DEFAULT,
		    c=asn1_DEFAULT,d=asn1_DEFAULT},
	      #'S3'{a=[11,12,13],
		    b=[{a,11},{b,true},{c,13}],
		    c=[1,2,3,4],
		    d=[#'S2'{a=20,b=true},#'S2'{a=30,b=false}]}),

    roundtrip(<<49,0>>,
	      'S3set',
	      #'S3set'{a=[{c,#'S2'{a=3,b=true}},{b,17},{a,false}],
		       b=[1,2,3,4]}),
    roundtrip(<<49,0>>,
	      'S3set',
	      #'S3set'{a=[{b,17},{c,#'S2'{a=3,b=true}},{a,false}],
		       b=[1,3,4,2]},
	      #'S3set'{a=[{c,#'S2'{a=3,b=true}},{b,17},{a,false}],
		       b=[1,2,3,4]}),
    roundtrip(<<49,0>>,
	      'S3set',
	      #'S3set'{a=asn1_DEFAULT,b=asn1_DEFAULT},
	      #'S3set'{a=[{c,#'S2'{a=3,b=true}},{b,17},{a,false}],
		       b=[1,2,3,4]}),

    roundtrip(<<48,0>>,
	      'S4',
	      #'S4'{a=#'S2'{a=1,b=asn1_NOVALUE},b=#'S4_b'{ba=true,bb=0}},
	      #'S4'{a=#'S2'{a=1,b=asn1_NOVALUE},b=#'S4_b'{ba=true,bb=0}}),

    roundtrip(<<48,0>>,
	      'SeqBS',
	      #'SeqBS'{a = <<2#1010110:7>>, b = <<16#A8A:12>>,
		       c=[second], d = <<2#1001:4>>,
		       e = <<2#01011010:8>>}),
    roundtrip(<<49,0>>,
	      'SetBS',
	      #'SetBS'{a = <<2#1010110:7>>, b = <<16#A8A:12>>,
		       c=[second], d = <<2#1001:4>>}),

    %% None of the default values are used.
    roundtrip(<<48,19,128,2,7,128,129,2,5,64,130,2,5,32,131,1,0,132,2,5,224>>,
	      'SeqBS',
	      #'SeqBS'{a = <<1:1>>,
		       b = <<2:3>>,
		       c = [third],
		       d = <<>>,
		       e = <<7:3>>}),
    roundtrip(<<49,3,131,1,0>>,
	      'SetBS',
	      #'SetBS'{a = <<2#1010110:7>>, b = <<16#A8A:12>>,
		       c=[second], d = <<>>}),

    %% SeqNamedInts
    roundtrip(<<48,0>>,
	      'SeqNamedInts',
	      #'SeqNamedInts'{i1=15,i2=31}),
    roundtrip(<<48,0>>,
	      'SeqNamedInts',
	      #'SeqNamedInts'{},
	      #'SeqNamedInts'{i1=15,i2=31}),
    roundtrip(<<48,0>>,
	      'SeqNamedInts',
	      #'SeqNamedInts'{i2=last},
	      #'SeqNamedInts'{i1=15,i2=31}),
    roundtrip(<<48,3,128,1,0>>,
	      'SeqNamedInts',
	      #'SeqNamedInts'{i1=first,i2=31},
	      #'SeqNamedInts'{i1=first,i2=31}),

    %% S5
    roundtrip(<<48,0>>,
	      'S5',
	      #'S5'{s3=#'S3'{a=[11,12,13],
			     b=[{a,11},{b,true},{c,13}],
			     c=[1,2,3,4],
			     d=[#'S2'{a=20,b=true},#'S2'{a=30,b=false}]},
		    so=[{0,1,999},{0,1,555}],
		    soe=[]}),
    roundtrip(<<48,0>>,
	      'S5',
	      #'S5'{},
	      #'S5'{s3=#'S3'{a=[11,12,13],
			     b=[{a,11},{b,true},{c,13}],
			     c=[1,2,3,4],
			     d=[#'S2'{a=20,b=true},#'S2'{a=30,b=false}]},
		    so=[{0,1,999},{0,1,555}],
		    soe=[]}),

    %% SOI
    roundtrip(<<48,0>>,
	      'SOI',
	      #'SOI'{},
	      #'SOI'{soi=[{1,2,250,9,55},{1,2,250,3,4}]}),

    %% SeqBS2
    roundtrip(<<48,0>>,
	      'SeqBS2',
	      #'SeqBS2'{bs= <<16#5:3>>}),
    roundtrip(<<48,0>>,
	      'SeqBS2',
	      #'SeqBS2'{bs= <<16#5:3,0:4>>},
	      #'SeqBS2'{bs= <<16#5:3>>}),

    ok.

der_new_types() ->
    io:put_chars("Performing DER-specific tests with new types..."),

    roundtrip(<<48,0>>, 'SeqOS',
	      #'SeqOS'{a = <<172>>,b = <<16#A8,16#A0>>,c='NULL'}),

    roundtrip(<<49,0>>, 'SetOS',
	      #'SetOS'{a = <<172>>,b = <<16#A8,16#A0>>,c='NULL'}),
    ok.

der_legacy() ->
    io:put_chars("Performing DER-specific tests with legacy types..."),

    roundtrip(<<48,0>>, 'SeqOS',
	      #'SeqOS'{a=[172],b=[16#A8,16#A0],c='NULL'}),
    roundtrip(<<49,0>>, 'SetOS',
	      #'SetOS'{a=[172],b=[16#A8,16#A0],c='NULL'}),

    roundtrip(<<48,0>>,
	      'SeqBS',
	      #'SeqBS'{a=2#0110101,
		       b=2#010100010101,
		       c=[second],
		       d=[1,0,0,1]},
	      #'SeqBS'{a = <<2#1010110:7>>, b = <<16#A8A:12>>,
		       c=[second], d = <<2#1001:4>>,
		       e = <<2#01011010:8>>}),
    roundtrip(<<48,0>>,
	      'SeqBS',
	      #'SeqBS'{a=[1,0,1,0,1,1,0],
		       b=[1,0,1,0,1,0,0,0,1,0,1,0],
		       c={5,<<64>>},
		       d=2#1001},
	      #'SeqBS'{a = <<2#1010110:7>>, b = <<16#A8A:12>>,
		       c=[second], d = <<2#1001:4>>,
		       e = <<2#01011010:8>>}),
    roundtrip(<<48,3,131,1,0>>,
	      'SeqBS',
	      #'SeqBS'{a=[1,0,1,0,1,1,0],
		       b=[1,0,1,0,1,0,0,0,1,0,1,0],
		       c={5,<<64>>},
		       d=0},
	      #'SeqBS'{a = <<2#1010110:7>>, b = <<16#A8A:12>>,
		       c=[second], d = <<>>,
		       e = <<2#01011010:8>>}),
    roundtrip(<<48,3,131,1,0>>,
	      'SeqBS',
	      #'SeqBS'{a = <<1:1,0:1,1:1,0:1,1:1,1:1,0:1>>,
		       b = <<1:1,0:1,1:1,0:1,1:1,0:1,0:1,0:1,1:1,0:1,1:1,0:1>>,
		       c = <<2:3>>,
		       d=0,
		       e = <<16#5A:8>>},
	      #'SeqBS'{a = <<2#1010110:7>>, b = <<16#A8A:12>>,
		       c=[second], d = <<>>,
		       e = <<2#01011010:8>>}),

    %% None of the default values are used.
    roundtrip(<<48,19,128,2,7,128,129,2,5,64,130,2,5,32,131,1,0,132,2,5,224>>,
	      'SeqBS',
	      #'SeqBS'{a = <<1:1>>,
		       b = {5,<<64>>},
		       c = [third],
		       d = 0,
		       e = <<7:3>>},
	      #'SeqBS'{a = <<1:1>>,
		       b = <<2:3>>,
		       c = [third],
		       d = <<>>,
		       e = <<7:3>>}),
    roundtrip(<<49,0>>,
	      'SetBS',
	      #'SetBS'{a=2#0110101,
		       b=2#010100010101,
		       c=[second],
		       d=[1,0,0,1]},
	      #'SetBS'{a = <<2#1010110:7>>, b = <<16#A8A:12>>,
		       c=[second], d = <<2#1001:4>>}),
    roundtrip(<<49,0>>,
	      'SetBS',
	      #'SetBS'{a=[1,0,1,0,1,1,0],
		       b=[1,0,1,0,1,0,0,0,1,0,1,0],
		       c={5,<<64>>},
		       d=9},
	      #'SetBS'{a = <<2#1010110:7>>, b = <<16#A8A:12>>,
		       c=[second], d = <<2#1001:4>>}),
    roundtrip(<<49,3,131,1,0>>,
	      'SetBS',
	      #'SetBS'{a=[1,0,1,0,1,1,0],
		       b=[1,0,1,0,1,0,0,0,1,0,1,0],
		       c={5,<<64>>},
		       d=0},
	      #'SetBS'{a = <<2#1010110:7>>, b = <<16#A8A:12>>,
		       c=[second], d = <<>>}),
    roundtrip(<<49,3,131,1,0>>,
	      'SetBS',
	      #'SetBS'{a = <<1:1,0:1,1:1,0:1,1:1,1:1,0:1>>,
		       b = <<1:1,0:1,1:1,0:1,1:1,0:1,0:1,0:1,1:1,0:1,1:1,0:1>>,
		       c = <<2:3>>,
		       d=0},
	      #'SetBS'{a = <<2#1010110:7>>, b = <<16#A8A:12>>,
		       c=[second], d = <<>>}),

    ok.

roundtrip(Encoded, Type, Value) ->
    roundtrip(Encoded, Type, Value, Value).

roundtrip(Encoded, Type, Value, ExpectedValue) ->
    Encoded = asn1_test_lib:roundtrip_enc('Default', Type,
					  Value, ExpectedValue),
    ok.
