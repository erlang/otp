%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
-module(testPrim).

-export([bool/1]).
-export([int/1]).
-export([enum/1]).
-export([obj_id/1]).
-export([rel_oid/1]).
-export([null/1]).
-export([real/1]).

-include_lib("common_test/include/ct.hrl").

bool(Rules) ->
    Types = ['Bool','BoolCon','BoolPri','BoolApp',
	     'BoolExpCon','BoolExpPri','BoolExpApp'],
    [roundtrip(T, V) || T <- Types, V <- [true,false]],
    Tag = case Rules of
              ber -> encode_boolean;
              _ -> illegal_boolean
          end,
    [{Tag,517} = enc_error(T, 517) || T <- Types],
    ok.


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
    Types = ['Int','IntCon','IntPri','IntApp',
             'IntExpCon','IntExpPri','IntExpApp'],
    _ = [roundtrip(T, V) || T <- Types, V <- [1|Values]],
    Tag = case Rules of
              ber -> encode_integer;
              _ -> illegal_integer
          end,
    _ = [{Tag,V} = enc_error(T, V) ||
            T <- Types, V <- [atom,42.0,{a,b,c}]],
    case Rules of
        ber ->
            ok;
        _ ->
            _ = [{Tag,V} = enc_error('IntConstrained', V) ||
                    V <- [atom,-1,256,42.0]]
    end,

    %%==========================================================
    %% IntEnum ::=  INTEGER {first(1),last(31)} 
    %%==========================================================

    [roundtrip('IntEnum', V) || V <- Values],

    roundtrip('IntEnum', first),
    roundtrip('IntEnum', last),

    roundtrip('ASeq', {'ASeq',true,254,false,255,true,256,true,68789}),
    roundtrip('ASeq', {'ASeq',false,250,true,200,true,199,true,77788}),
    roundtrip('ASeq', {'ASeq',true,0,false,0,true,0,true,68789}),

    %%==========================================================
    %% Longitude ::= INTEGER {
    %%  oneMicrodegreeEast(10),
    %%  oneMicrodegreeWest(-10),
    %%  unavailable(1800000001)
    %% } (-1799999999..1800000001)
    %%==========================================================

    Enc10 = encoding(Rules, oneMicrodegreeEast),
    Enc10 = roundtrip('Longitude', oneMicrodegreeEast),
    Enc10 = roundtrip('Longitude', 10, oneMicrodegreeEast),

    Enc20 = encoding(Rules, oneMicrodegreeWest),
    Enc20 = roundtrip('Longitude', oneMicrodegreeWest),
    Enc20 = roundtrip('Longitude', -10, oneMicrodegreeWest),

    Enc30 = roundtrip('Longitude', unavailable),
    Enc30 = roundtrip('Longitude', 1800000001, unavailable),

    ok.

encoding(Rules, Type) ->
    asn1_test_lib:hex_to_bin(encoding_1(Rules, Type)).

encoding_1(ber, oneMicrodegreeEast) -> "02010A";
encoding_1(per, oneMicrodegreeEast) -> "C06B49D2 09";
encoding_1(uper, oneMicrodegreeEast) -> "6B49D209";

encoding_1(ber, oneMicrodegreeWest) -> "0201F6";
encoding_1(per, oneMicrodegreeWest) -> "C06B49D1 F5";
encoding_1(uper, oneMicrodegreeWest) -> "6B49D1F5".

enum(Rules) ->

    %%==========================================================
    %% Enum ::=  ENUMERATED {monday(1),tuesday(2),wednesday(3),thursday(4),
    %%                       friday(5),saturday(6),sunday(7)}
    %%==========================================================

    roundtrip('Enum', monday),
    roundtrip('Enum', thursday),
    Tag = case Rules of
              ber -> enumerated_not_in_range;
              _ -> illegal_enumerated
          end,
    {Tag,4} = enc_error('Enum', 4),

    case Rules of
	Per when Per =:= per; Per =:= uper ->
	    <<0>> = roundtrip('SingleEnumVal', true),
	    <<0>> = roundtrip('SingleEnumValExt', true);
	ber ->
	    ok
    end,

    roundtrip('NegEnumVal', neg),
    roundtrip('NegEnumVal', zero),
    roundtrip('EnumVal128', val),

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
    roundtrip('Null', monday, 'NULL'),
    ok.

roundtrip(T, V) ->
    roundtrip(T, V, V).

roundtrip(Type, Value, ExpectedValue) ->
    case get(no_ok_wrapper) of
	false ->
	    asn1_test_lib:roundtrip_enc('Prim', Type, Value, ExpectedValue);
	true ->
	    M = 'Prim',
	    Enc = M:encode(Type, Value),
	    ExpectedValue = M:decode(Type, Enc),
	    Enc
    end.

enc_error(T, V) ->
    case get(no_ok_wrapper) of
	false ->
	    {error,{asn1,{Reason,Stk}}} = 'Prim':encode(T, V),
            [{_,_,_,_}|_] = Stk,
            Reason;
	true ->
	    try 'Prim':encode(T, V) of
		_ ->
		    ?t:fail()
	    catch
		_:{error,{asn1,Reason}} ->
		    Reason
	    end
    end.

real(_Rules) ->
    %%==========================================================
    %% AngleInRadians ::= REAL
    %%==========================================================
    
    %% Base 2
    real_roundtrip('AngleInRadians', {1,2,1}),
    real_roundtrip('AngleInRadians', {129,2,1}),
    real_roundtrip('AngleInRadians', {128,2,1}, {1,2,8}),
    real_roundtrip('AngleInRadians', {128,2,-7}, {1,2,0}),
    real_roundtrip('AngleInRadians', {16#f1f1f1,2,128}),

    %% Base 10, tuple format
    real_roundtrip('AngleInRadians', {1,10,1}, "1.E1"),
    real_roundtrip('AngleInRadians', {100,10,1}, "1.E3"),
    real_roundtrip('AngleInRadians', {-100,10,1}, "-1.E3"),
    real_roundtrip('AngleInRadians', {2,10,1}, "2.E1"),
    real_roundtrip('AngleInRadians', {123000,10,0}, "123.E3"),
    real_roundtrip('AngleInRadians', {123456789,10,123456789},
		   "123456789.E123456789" ),
    real_roundtrip('AngleInRadians', {-12345,10,-12345}, "-12345.E-12345"),

    %% Base 10, string format NR3

    real_roundtrip('AngleInRadians', "123.123E123", "123123.E120"),
    real_roundtrip('AngleInRadians', "0.0E0", "0.E+0"),
    real_roundtrip('AngleInRadians', "0.0123", "123.E-4"),
    real_roundtrip('AngleInRadians', "0", "0.E+0"),
    real_roundtrip('AngleInRadians', "-123.45", "-12345.E-2"),
    real_roundtrip('AngleInRadians', "123456789E123456789",
		   "123456789.E123456789"),
    real_roundtrip('AngleInRadians', "01.000E1", "1.E1"),
    real_roundtrip('AngleInRadians', "120.0001", "1200001.E-4"),

    ok.

real_roundtrip(T, V) ->
    real_roundtrip(T, V, V).

real_roundtrip(Type, Value, ExpectedValue) ->
    case get(no_ok_wrapper) of
	false ->
	    asn1_test_lib:roundtrip('Real', Type, Value, ExpectedValue);
	true ->
	    M = 'Real',
	    ExpectedValue = M:decode(Type, M:encode(Type, Value)),
	    ok
    end.
