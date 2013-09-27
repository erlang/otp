%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2013. All Rights Reserved.
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
-module(testSeqSetDefaultVal).
-export([main/1]).

-include("External.hrl").
-include_lib("test_server/include/test_server.hrl").

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
		 d = asn1_DEFAULT}).
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

main(_Rules) ->
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
	      'SeqBS',
	      #'SeqBS'{a=2#1010110,b=16#A8A,c=[second],d=[1,0,0,1]},
	      #'SeqBS'{a=[1,0,1,0,1,1,0],b=16#A8A,c=[second],d=[1,0,0,1]}),
    roundtrip(<<48,0>>,
	      'SeqBS',
	      #'SeqBS'{a=[1,0,1,0,1,1,0],
		       b=[1,0,1,0,1,0,0,0,1,0,1,0],
		       c={5,<<64>>},
		       d=2#1001},
	      #'SeqBS'{a=[1,0,1,0,1,1,0],b=16#A8A,c=[second],d=[1,0,0,1]}),
    roundtrip(<<48,3,131,1,0>>,
	      'SeqBS',
	      #'SeqBS'{a=[1,0,1,0,1,1,0],
		       b=[1,0,1,0,1,0,0,0,1,0,1,0],
		       c={5,<<64>>},
		       d=0},
	      #'SeqBS'{a=[1,0,1,0,1,1,0],
		       b=16#A8A,
		       c=[second],
		       d = <<>>}),

    roundtrip(<<49,0>>,
	      'SetBS',
	      #'SetBS'{a=2#1010110,b=16#A8A,c=[second],d=[1,0,0,1]},
	      #'SetBS'{a=[1,0,1,0,1,1,0],b=16#A8A,c=[second],d=[1,0,0,1]}),
    roundtrip(<<49,0>>,
	      'SetBS',
	      #'SetBS'{a=[1,0,1,0,1,1,0],
		       b=[1,0,1,0,1,0,0,0,1,0,1,0],
		       c={5,<<64>>},
		       d=9},
	      #'SetBS'{a=[1,0,1,0,1,1,0],
		       b=16#A8A,
		       c=[second],
		       d=[1,0,0,1]}),
    roundtrip(<<49,3,131,1,0>>,
	      'SetBS',
	      #'SetBS'{a=[1,0,1,0,1,1,0],
		       b=[1,0,1,0,1,0,0,0,1,0,1,0],
		       c={5,<<64>>},
		       d=0},
	      #'SetBS'{a=[1,0,1,0,1,1,0],
		       b=16#A8A,
		       c=[second],
		       d = <<>>}),

    roundtrip(<<48,0>>, 'SeqOS',
	      #'SeqOS'{a=[172],b=[16#A8,16#A0],c='NULL'}),
    roundtrip(<<48,0>>,
	      'SeqOS',
	      #'SeqOS'{a=172,b=43168,c='NULL'},
	      #'SeqOS'{a=[172],b=[16#A8,16#A0],c='NULL'}),

    roundtrip(<<49,0>>, 'SetOS', #'SetOS'{a=[172],b=[16#A8,16#A0],c='NULL'}),
    roundtrip(<<49,0>>,
	      'SetOS',
	      #'SetOS'{a=172,b=43168,c='NULL'},
	      #'SetOS'{a=[172],b=[16#A8,16#A0],c='NULL'}),

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

    ok.

roundtrip(Encoded, Type, Value) ->
    roundtrip(Encoded, Type, Value, Value).

roundtrip(Encoded, Type, Value, ExpectedValue) ->
    Encoded = asn1_test_lib:roundtrip_enc('Default', Type,
					  Value, ExpectedValue),
    ok.
