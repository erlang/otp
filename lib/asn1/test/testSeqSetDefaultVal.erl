%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2009. All Rights Reserved.
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

-include("External.hrl").
-export([compile/2]).
-export([main/1]).

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


compile(Config,Rules) ->
    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "Default",
			      [Rules,der,{outdir,OutDir}]).

main(_Rules) ->
    
    ?line {ok,[48,0]} = 
	asn1_wrapper:encode('Default','SeqInts',#'SeqInts'{}),
    ?line {ok,[48,0]} =
	asn1_wrapper:encode('Default','SeqInts',#'SeqInts'{a=1,b=-1,c=three,
							   d=1}),
    ?line {ok,{'SeqInts',1,-1,3,1}} = 
	asn1_wrapper:decode('Default','SeqInts',[48,0]),
    
    ?line {ok,[49,0]} = 
	asn1_wrapper:encode('Default','SetInts',#'SetInts'{}),
    ?line {ok,[49,0]} =
	asn1_wrapper:encode('Default','SetInts',#'SetInts'{a=1,b=-1,c=three,
							   d=1}),
    ?line {ok,{'SetInts',1,-1,3,1}} = 
	asn1_wrapper:decode('Default','SetInts',[49,0]),
    
    
    ?line {ok,[48,0]} = 
	asn1_wrapper:encode('Default','SeqBS',
			    #'SeqBS'{a=2#1010110,
				     b=16#A8A,
				     c=[second],
				     d=[1,0,0,1]}),

    ?line {ok,[48,0]} = 
	asn1_wrapper:encode('Default','SeqBS',
			    #'SeqBS'{a=[1,0,1,0,1,1,0],
				     b=[1,0,1,0,1,0,0,0,1,0,1,0],
				     c={5,<<64>>},
				     d=9}),

    ?line {ok,[48,3,131,1,0]} = 
	asn1_wrapper:encode('Default','SeqBS',
			    #'SeqBS'{a=[1,0,1,0,1,1,0],
				     b=[1,0,1,0,1,0,0,0,1,0,1,0],
				     c={5,<<64>>},
				     d=0}),

    ?line {ok,{'SeqBS',[1,0,1,0,1,1,0],2698,[second],[]}} =
	asn1_wrapper:decode('Default','SeqBS',[48,3,131,1,0]),

    ?line {ok,{'SeqBS',[1,0,1,0,1,1,0],2698,[second],[1,0,0,1]}} =
	asn1_wrapper:decode('Default','SeqBS',[48,0]),

    ?line {ok,[49,0]} = 
	asn1_wrapper:encode('Default','SetBS',
			    #'SetBS'{a=2#1010110,
				     b=16#A8A,
				     c=[second],
				     d=[1,0,0,1]}),

    ?line {ok,[49,0]} = 
	asn1_wrapper:encode('Default','SetBS',
			    #'SetBS'{a=[1,0,1,0,1,1,0],
				     b=[1,0,1,0,1,0,0,0,1,0,1,0],
				     c={5,<<64>>},
				     d=9}),

    ?line {ok,[49,3,131,1,0]} = 
	asn1_wrapper:encode('Default','SetBS',
			    #'SetBS'{a=[1,0,1,0,1,1,0],
				     b=[1,0,1,0,1,0,0,0,1,0,1,0],
				     c={5,<<64>>},
				     d=0}),

    ?line {ok,{'SetBS',[1,0,1,0,1,1,0],2698,[second],[]}} =
	asn1_wrapper:decode('Default','SetBS',[49,3,131,1,0]),

    ?line {ok,{'SetBS',[1,0,1,0,1,1,0],2698,[second],[1,0,0,1]}} =
	asn1_wrapper:decode('Default','SetBS',[49,0]),

    ?line {ok,[48,0]} = 
	asn1_wrapper:encode('Default','SeqOS',
			    #'SeqOS'{a=[172],
				     b=[16#A8,16#A0],
				     c='NULL'}),

    ?line {ok,[48,0]} = 
	asn1_wrapper:encode('Default','SeqOS',
			    #'SeqOS'{a=2#10101100,
				     b=16#A8A0,
				     c='NULL'}),

    ?line {ok,{'SeqOS',[172],[16#A8,16#A0],'NULL'}} =
	asn1_wrapper:decode('Default','SeqOS',[48,0]),

    ?line {ok,[49,0]} = 
	asn1_wrapper:encode('Default','SetOS',
			    #'SetOS'{a=[172],
				     b=[16#A8,16#A0],
				     c='NULL'}),

    ?line {ok,[49,0]} = 
	asn1_wrapper:encode('Default','SetOS',
			    #'SetOS'{a=2#10101100,
				     b=16#A8A0,
				     c='NULL'}),

    ?line {ok,{'SetOS',[172],[16#A8,16#A0],'NULL'}} =
	asn1_wrapper:decode('Default','SetOS',[49,0]),

    ?line {ok,[48,0]} =
	asn1_wrapper:encode('Default','SeqOI',
			    #'SeqOI'{a={1,2,14,15},
				     b={iso,'member-body',250,3,4},
				     c={iso,standard,8571,2,250,4}}),
    
    ?line {ok,{'SeqOI',{1,2,14,15},{1,2,250,3,4},{1,0,8571,2,250,4}}} =
	asn1_wrapper:decode('Default','SeqOI',[48,0]),

    ?line {ok,[49,0]} =
	asn1_wrapper:encode('Default','SetOI',
			    #'SetOI'{a={1,2,14,15},
				     b={iso,'member-body',250,3,4},
				     c={iso,standard,8571,2,250,4}}),
    
    ?line {ok,{'SetOI',{1,2,14,15},{1,2,250,3,4},{1,0,8571,2,250,4}}} =
	asn1_wrapper:decode('Default','SetOI',[49,0]),
    
    ?line {ok,[48,0]} = 
	asn1_wrapper:encode('Default','SeqEnum',#'SeqEnum'{a=b4,b=b2}),

    ?line {ok,{'SeqEnum',b4,b2}} =
	asn1_wrapper:decode('Default','SeqEnum',[48,0]),

    ?line {ok,[49,0]} = 
	asn1_wrapper:encode('Default','SetEnum',#'SetEnum'{a=b4,b=b2}),

    ?line {ok,{'SetEnum',b4,b2}} =
	asn1_wrapper:decode('Default','SetEnum',[49,0]),
    
    ?line {ok,[48,0]} =
	asn1_wrapper:encode('Default','SeqIntBool',
			    #'SeqIntBool'{a=#'SeqIntBool_a'{aa=12,ab=13},
					  b=#'S2'{a=14,b=true},
					  c=#'S2'{a=15,b=false}}),
    
    ?line {ok,[48,0]} =
	asn1_wrapper:encode('Default','SeqIntBool',
			    #'SeqIntBool'{}),

    ?line {ok,{'SeqIntBool',{'SeqIntBool_a',12,13},
	       {'S2',14,true},{'S2',15,false}}} =
	asn1_wrapper:decode('Default','SeqIntBool',[48,0]),

    ?line {ok,[49,0]} =
	asn1_wrapper:encode('Default','SetIntBool',
			    #'SetIntBool'{a=#'SetIntBool_a'{aa=12,ab=13},
					  b=#'S2'{a=14,b=true},
					  c=#'S2'{a=15,b=false}}),
    
    ?line {ok,[49,0]} =
	asn1_wrapper:encode('Default','SetIntBool',
			    #'SetIntBool'{}),

    ?line {ok,{'SetIntBool',{'SetIntBool_a',12,13},
	       {'S2',14,true},{'S2',15,false}}} =
	asn1_wrapper:decode('Default','SetIntBool',[49,0]),
    
    ?line {ok,[48,0]} = 
	asn1_wrapper:encode('Default','SeqStrings',
			    #'SeqStrings'{a="123456789",
					  b1="abcdef",
					  b2={0,13},
					  b3={"First line",{0,13},"Second line"},
					  c="Printable string",
					  d={0,0,1,14}}),
 
    ?line {ok,{'SeqStrings',"123456789","abcdef",[0,13],
	       ["First line",[0,13],"Second line"],"Printable string",
	       [0,0,1,14]}} = 
	asn1_wrapper:decode('Default','SeqStrings',[48,0]),

    ?line {ok,[49,0]} = 
	asn1_wrapper:encode('Default','SetStrings',
			    #'SetStrings'{a="123456789",
					  b1="abcdef",
					  b2={0,13},
					  b3={"First line",{0,13},"Second line"},
					  c="Printable string",
					  d={0,0,1,14}}),
 
    ?line {ok,{'SetStrings',"123456789","abcdef",[0,13],
	       ["First line",[0,13],"Second line"],"Printable string",
	       [0,0,1,14]}} = 
	asn1_wrapper:decode('Default','SetStrings',[49,0]),


    ?line {ok,[48,0]} = 
	asn1_wrapper:encode('Default','S1',
			    #'S1'{a=#'S1_a'{aa=1,
					    ab=#'S2'{a=2,b=true}},
				  b=#'S4'{a=#'S2'{a=2,b=true},
					  b=#'S4_b'{ba=true,
						    bb=5}}}),

    ?line {ok,{'S1',{'S1_a',1,{'S2',2,true}},
	       {'S4',{'S2',2,true},{'S4_b',true,5}}}} =
	asn1_wrapper:decode('Default','S1',[48,0]),

    ?line {ok,[48,3,129,1,255]} =
	asn1_wrapper:encode('Default','S2',
			    #'S2'{a=1,b=true}),

    ?line {ok,[48,0]} =
	asn1_wrapper:encode('Default','S3',
			    #'S3'{a=[11,12,13],
				  b=[{a,11},{b,true},{c,13}],
				  c=[1,2,3,4],
				  d=[#'S2'{a=20,b=true},#'S2'{a=30,b=false}]}),

    ?line {ok,[48,0]} =
	asn1_wrapper:encode('Default','S3',
			    #'S3'{a=[11,13,12],
				  b=[{b,true},{a,11},{c,13}],
				  c=[3,4,1,2],
				  d=[#'S2'{a=30,b=false},#'S2'{a=20,b=true}]}),

    ?line {ok,[48,0]} = asn1_wrapper:encode('Default','S3',#'S3'{}),

    ?line {ok,[49,0]} = 
	asn1_wrapper:encode('Default','S3set',
			    #'S3set'{a=[{c,#'S2'{a=3,b=true}},
					{b,17},{a,false}],
				     b=[1,2,3,4]}),

    ?line {ok,[49,0]} = 
	asn1_wrapper:encode('Default','S3set',
			    #'S3set'{a=[{b,17},{c,#'S2'{a=3,b=true}},
					{a,false}],
				     b=[1,3,4,2]}),

    ?line {ok,[49,0]} = asn1_wrapper:encode('Default','S3set',#'S3set'{}),
    
    ?line {ok,[48,0]} = 
	asn1_wrapper:encode('Default','S4',#'S4'{a={'S2',1,asn1_NOVALUE},
						 b=#'S4_b'{ba=true,bb=0}}),
    ok.
