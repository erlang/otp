%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2012. All Rights Reserved.
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
-module(testConstraints).

-export([int_constraints/1,refed_NNL_name/1]).


-include_lib("test_server/include/test_server.hrl").

int_constraints(Rules) ->

    %%==========================================================
    %% SingleValue ::=  INTEGER (1) 
    %%==========================================================

    range_error(Rules, 'SingleValue', 0),
    roundtrip('SingleValue', 1),
    range_error(Rules, 'SingleValue', 2),
    range_error(Rules, 'SingleValue', 1000),

    %%==========================================================
    %% SingleValue2 ::=  INTEGER (1..20) 
    %%==========================================================

    range_error(Rules, 'SingleValue2', 0),
    roundtrip('SingleValue2', 1),
    roundtrip('SingleValue2', 20),
    range_error(Rules, 'SingleValue2', 21),
    range_error(Rules, 'SingleValue2', 1000),

    %%==========================================================
    %% SingleValue3 ::=  INTEGER (Predefined | 5 | 10)
    %% Testcase for OTP-10139. A single value subtyping of an integer type
    %% where one value is predefined.
    %%==========================================================

    roundtrip('SingleValue3', 1),
    roundtrip('SingleValue3', 5),
    roundtrip('SingleValue3', 10),

    %%==========================================================
    %% Range2to19 ::=  INTEGER (1<..<20) 
    %%==========================================================

    range_error(Rules, 'Range2to19', 1),
    roundtrip('Range2to19', 2),
    roundtrip('Range2to19', 19),
    range_error(Rules, 'Range2to19', 20),

    %%==========================================================
    %% Tests for Range above 16^4 up to maximum supported by asn1 assuming the
    %% octet length field is encoded on max 8 bits
    %%==========================================================
    LastNumWithoutLengthEncoding = 65536,
    roundtrip('Range256to65536', LastNumWithoutLengthEncoding),

    FirstNumWithLengthEncoding = 65537,
    roundtrip('LargeConstraints', 'RangeMax', FirstNumWithLengthEncoding),

    FirstNumOver16_6 = 16777217,
    roundtrip('LargeConstraints', 'RangeMax', FirstNumOver16_6),
    
    FirstNumOver16_8 = 4294967297,
    roundtrip('LargeConstraints', 'RangeMax', FirstNumOver16_8),

    FirstNumOver16_10 = 1099511627776,
    roundtrip('LargeConstraints', 'RangeMax', FirstNumOver16_10),

    FirstNumOver16_10 = 1099511627776,
    roundtrip('LargeConstraints', 'RangeMax', FirstNumOver16_10),

    HalfMax = 1 bsl (128*8),
    roundtrip('LargeConstraints', 'RangeMax', HalfMax),

    Max = 1 bsl (255*8),
    roundtrip('LargeConstraints', 'RangeMax', Max),
    
    %% Random number within longlong range
    LongLong = 12672809400538808320,
    roundtrip('LongLong', LongLong),

    %%==========================================================
    %%  Constraint Combinations (Duboisson p. 285)
    %%  I ::= INTEGER (0|15..269)
    %%==========================================================

    range_error(Rules, 'I', -1),
    roundtrip('I', 0),
    roundtrip('I', 15),
    roundtrip('I', 20),
    roundtrip('I', 269),
    range_error(Rules, 'I', 270),

    %%==========================================================
    %%  Constraint Combinations (Duboisson p. 285)
    %%  X1 ::= INTEGER (1..4|8|10|20)
    %%==========================================================

    range_error(Rules, 'X1', 0),
    roundtrip('X1', 1),
    roundtrip('X1', 4),
    roundtrip('X1', 8),
    roundtrip('X1', 10),
    roundtrip('X1', 20),
    range_error(Rules, 'X1', 21),

    %%==========================================================
    %%  SIZE Constraint (Duboisson p. 268)
    %%  T ::=  IA5String (SIZE (1|2, ..., SIZE (1|2|3)))
    %%  T2 ::= IA5String (SIZE (1|2, ..., 3))
    %%==========================================================

    roundtrip('T', "IA"),
    roundtrip('T2', "IA").

refed_NNL_name(_Erule) ->
    ?line {ok,_} = asn1_wrapper:encode('Constraints','AnotherThing',fred),
    ?line {error,_Reason} = 
	asn1_wrapper:encode('Constraints','AnotherThing',fred3).

roundtrip(Type, Value) ->
    roundtrip('Constraints', Type, Value).

roundtrip(Module, Type, Value) ->
    {ok,Encoded} = Module:encode(Type, Value),
    {ok,Value} = Module:decode(Type, Encoded),
    ok.

range_error(ber, Type, Value) ->
    %% BER: Values outside the effective range should be rejected
    %% on decode.
    {ok,Encoded} = 'Constraints':encode(Type, Value),
    {error,{asn1,{integer_range,_,_}}} = 'Constraints':decode(Type, Encoded),
    ok;
range_error(Per, Type, Value) when Per =:= per; Per =:= uper ->
    %% (U)PER: Values outside the effective range should be rejected
    %% on encode.
    {error,_} = 'Constraints':encode(Type, Value),
    ok.
