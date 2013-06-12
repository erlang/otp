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
-module(testPrim).

-export([bool/1]).
-export([int/1]).
-export([enum/1]).
-export([obj_id/1]).
-export([rel_oid/1]).
-export([null/1]).
-export([real/1]).

-include_lib("test_server/include/test_server.hrl").

bool(Rules) ->
    Types = ['Bool','BoolCon','BoolPri','BoolApp',
	     'BoolExpCon','BoolExpPri','BoolExpApp'],
    [roundtrip(T, V) || T <- Types, V <- [true,false]],
    case Rules of
	ber ->
	    [begin
		 {error,{asn1,{encode_boolean,517}}} =
		     (catch 'Prim':encode(T, 517))
	     end || T <- Types],
	    ok;
	_ ->
	    ok
    end.


int(Rules) ->
    %% OTP-2666: encoding should use minimum number of octets; x.690 8.3.2
    Bytes0 = roundtrip('Int', -128),
    case Rules of
	ber ->
	    <<_,1,128>> = Bytes0;
	_ ->
	    ok
    end,

    Values = [0,2,3,4,127,128,254,255,256,257,444,
	      16383,16384,16385,65534,65535,65536,65537,
	      123456789,12345678901234567890,
	      -1,-2,-3,-4,-100,-127,-255,-256,-257,
	      -1234567890,-2147483648],
    [roundtrip(T, V) ||
	T <- ['Int','IntCon','IntPri','IntApp',
	      'IntExpCon','IntExpPri','IntExpApp'],
	V <- [1|Values]],

    %%==========================================================
    %% IntEnum ::=  INTEGER {first(1),last(31)} 
    %%==========================================================

    [roundtrip('IntEnum', V) || V <- Values],

    roundtrip('IntEnum', first),
    roundtrip('IntEnum', last),

    roundtrip('ASeq', {'ASeq',true,254,false,255,true,256,true,68789}),
    roundtrip('ASeq', {'ASeq',false,250,true,200,true,199,true,77788}),
    roundtrip('ASeq', {'ASeq',true,0,false,0,true,0,true,68789}),

    ok.


enum(Rules) ->

    %%==========================================================
    %% Enum ::=  ENUMERATED {monday(1),tuesday(2),wednesday(3),thursday(4),
    %%                       friday(5),saturday(6),sunday(7)}
    %%==========================================================

    roundtrip('Enum', monday),
    roundtrip('Enum', thursday),
    {error,{asn1,{_,4}}} = (catch 'Prim':encode('Enum', 4)),

    case Rules of
	Per when Per =:= per; Per =:= uper ->
	    {ok,<<0>>} = 'Prim':encode('SingleEnumVal', true),
	    {ok,<<0>>} = 'Prim':encode('SingleEnumValExt', true);
	ber ->
	    ok
    end,
    ok.


obj_id(_) ->

    %%==========================================================
    %% ObjId ::= OBJECT IDENTIFIER
    %%==========================================================

    [roundtrip('ObjId', V) ||
	V <- [{0,22,3},{1,39,3},{2,100,3},{2,16303,3},{2,16304,3}]],
    ok.

rel_oid(_Rules) ->
    %%==========================================================
    %% RelOid ::= RELATIVE-OID
    %%==========================================================

    [roundtrip('RelOid', V) ||
	V <- [{0,22,3},{1,39,3},{2,100,3},{2,16303,3},
	      {2,16304,3},{8,16304,16#ffff}]],
    ok.


null(_Rules) ->

    %%==========================================================
    %% Null ::= NULL
    %%==========================================================

    {ok,Bytes1} = asn1_wrapper:encode('Prim','Null',monday),
    {ok,'NULL'} = asn1_wrapper:decode('Prim','Null',lists:flatten(Bytes1)),
    ok.

roundtrip(T, V) ->
    {ok,E} = 'Prim':encode(T, V),
    {ok,V} = 'Prim':decode(T, E),
    E.

real(_Rules) ->
    %%==========================================================
    %% AngleInRadians ::= REAL
    %%==========================================================
    
    %% Base 2
    ?line {ok,Bytes1} = asn1_wrapper:encode('Real','AngleInRadians',{1,2,1}),
    ?line {ok,{1,2,1}} = asn1_wrapper:decode('Real','AngleInRadians',Bytes1),

    ?line {ok,Bytes2} = asn1_wrapper:encode('Real','AngleInRadians',{129,2,1}),
    ?line {ok,{129,2,1}} = asn1_wrapper:decode('Real','AngleInRadians',Bytes2),

    ?line {ok,Bytes3} = asn1_wrapper:encode('Real','AngleInRadians',{128,2,1}),
    ?line {ok,{1,2,8}} = asn1_wrapper:decode('Real','AngleInRadians',Bytes3),

    ?line {ok,Bytes4} = asn1_wrapper:encode('Real','AngleInRadians',{128,2,-7}),
    ?line {ok,{1,2,0}} = asn1_wrapper:decode('Real','AngleInRadians',Bytes4),

    ?line {ok,Bytes5} = asn1_wrapper:encode('Real','AngleInRadians',{16#f1f1f1,2,128}),
    ?line {ok,{16#f1f1f1,2,128}} = asn1_wrapper:decode('Real','AngleInRadians',Bytes5),

    %% Base 10, tuple format
    ?line {ok,Bytes6} = asn1_wrapper:encode('Real','AngleInRadians',{1,10,1}),
    ?line {ok,"1.E1"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes6),
    
    ?line {ok,Bytes7} = asn1_wrapper:encode('Real','AngleInRadians',{100,10,1}),
    ?line {ok,"1.E3"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes7),

    ?line {ok,Bytes8} = asn1_wrapper:encode('Real','AngleInRadians',{-100,10,1}),
    ?line {ok,"-1.E3"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes8),

    ?line {ok,Bytes9} = asn1_wrapper:encode('Real','AngleInRadians',{00002,10,1}),
    ?line {ok,"2.E1"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes9),

    ?line {ok,Bytes10} = asn1_wrapper:encode('Real','AngleInRadians',{123000,10,0}),
    ?line {ok,"123.E3"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes10),

    ?line {ok,Bytes11} = asn1_wrapper:encode('Real','AngleInRadians',{123456789,10,123456789}),
    ?line {ok,"123456789.E123456789"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes11),

    ?line {ok,Bytes12} = asn1_wrapper:encode('Real','AngleInRadians',{-12345,10,-12345}),
    ?line {ok,"-12345.E-12345"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes12),

    %% Base 10, string format NR3
    
    ?line {ok,Bytes13} = asn1_wrapper:encode('Real','AngleInRadians',"123.123E123"),
    ?line {ok,"123123.E120"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes13),

    ?line {ok,Bytes14} = asn1_wrapper:encode('Real','AngleInRadians',"0.0E0"),
    ?line {ok,"0.E+0"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes14),

    ?line {ok,Bytes15} = asn1_wrapper:encode('Real','AngleInRadians',"0.0123"),
    ?line {ok,"123.E-4"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes15),

    ?line {ok,Bytes16} = asn1_wrapper:encode('Real','AngleInRadians',"0"),
    ?line {ok,"0.E+0"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes16),
    
    ?line {ok,Bytes17} = asn1_wrapper:encode('Real','AngleInRadians',"-123.45"),
    ?line {ok,"-12345.E-2"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes17),

    ?line {ok,Bytes18} = 
	asn1_wrapper:encode('Real','AngleInRadians',"123456789E123456789"),
    ?line {ok,"123456789.E123456789"} = 
	asn1_wrapper:decode('Real','AngleInRadians',Bytes18),

    ?line {ok,Bytes19} = asn1_wrapper:encode('Real','AngleInRadians',"01.000E1"),
    ?line {ok,"1.E1"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes19),

    ?line {ok,Bytes20} = asn1_wrapper:encode('Real','AngleInRadians',"120.0001"),
    ?line {ok,"1200001.E-4"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes20).
