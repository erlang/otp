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
    %%  SemiConstrained
    %%==========================================================

    roundtrip('SemiConstrained', 100),
    v_roundtrip(Rules, 'SemiConstrained', 100+128),
    roundtrip('SemiConstrained', 397249742397243),
    roundtrip('SemiConstrained', 100 + 1 bsl 128*8),
    roundtrip('SemiConstrained', 100 + 1 bsl 256*8),

    roundtrip('NegSemiConstrained', -128),
    v_roundtrip(Rules, 'NegSemiConstrained', 0),
    roundtrip('NegSemiConstrained', -1),
    roundtrip('NegSemiConstrained', 500),

    roundtrip('SemiConstrainedExt', -65536),
    roundtrip('SemiConstrainedExt', 0),
    roundtrip('SemiConstrainedExt', 42),
    v_roundtrip(Rules, 'SemiConstrainedExt', 42+128),
    roundtrip('SemiConstrainedExt', 100),
    roundtrip('SemiConstrainedExt', 47777789),
    roundtrip('SemiConstrainedExt', 42 + 1 bsl 128*8),
    roundtrip('SemiConstrainedExt', 42 + 1 bsl 256*8),

    roundtrip('NegSemiConstrainedExt', -1023),
    roundtrip('NegSemiConstrainedExt', -128),
    roundtrip('NegSemiConstrainedExt', -1),
    v_roundtrip(Rules, 'NegSemiConstrainedExt', 0),
    roundtrip('NegSemiConstrainedExt', 500),

    %%==========================================================
    %%  SIZE Constraint (Duboisson p. 268)
    %%  T ::=  IA5String (SIZE (1|2, ..., SIZE (1|2|3)))
    %%  T2 ::= IA5String (SIZE (1|2, ..., 3))
    %%==========================================================

    roundtrip('T', "IA"),
    roundtrip('T', "IAB"),
    roundtrip('T', "IABC"),
    roundtrip('T2', "IA"),
    roundtrip('T2', "IAB"),
    roundtrip('T2', "IABC"),

    %%==========================================================
    %%  More SIZE Constraints
    %%==========================================================

    roundtrip('FixedSize', "0123456789"),
    roundtrip('FixedSize2', "0123456789"),
    roundtrip('FixedSize2', "0123456789abcdefghij"),

    range_error(Rules, 'FixedSize', "short"),
    range_error(Rules, 'FixedSize2', "short"),

    [roundtrip('VariableSize', lists:seq($A, $A+L-1)) ||
	L <- lists:seq(1, 10)],

    roundtrip_enc('ShorterExt', "a", shorter_ext(Rules, "a")),
    roundtrip('ShorterExt', "abcde"),
    roundtrip('ShorterExt', "abcdef"),

    ok.

%% PER: Ensure that if the lower bound is Lb, Lb+16#80 is encoded
%% in two bytes as 16#0180. (Not in three bytes as 16#010080.)
v(ber, 'SemiConstrained', 100+128) -> "020200E4";
v(per, 'SemiConstrained', 100+128) -> "0180";
v(uper, 'SemiConstrained', 100+128) -> "0180";
v(ber, 'NegSemiConstrained', 0) -> "020100";
v(per, 'NegSemiConstrained', 0) -> "0180";
v(uper, 'NegSemiConstrained', 0) -> "0180";
v(ber, 'SemiConstrainedExt', 42+128) -> "020200AA";
v(per, 'SemiConstrainedExt', 42+128) -> "000180";
v(uper, 'SemiConstrainedExt', 42+128) -> "00C000";
v(ber, 'NegSemiConstrainedExt', 0) -> "020100";
v(per, 'NegSemiConstrainedExt', 0) -> "000180";
v(uper, 'NegSemiConstrainedExt', 0) -> "00C000".

shorter_ext(per, "a") -> <<16#80,16#01,16#61>>;
shorter_ext(uper, "a") -> <<16#80,16#E1>>;
shorter_ext(ber, _) -> none.

refed_NNL_name(_Erule) ->
    ?line {ok,_} = asn1_wrapper:encode('Constraints','AnotherThing',fred),
    ?line {error,_Reason} = 
	asn1_wrapper:encode('Constraints','AnotherThing',fred3).

v_roundtrip(Erule, Type, Value) ->
    Encoded = asn1_test_lib:hex_to_bin(v(Erule, Type, Value)),
    Encoded = roundtrip('Constraints', Type, Value).

roundtrip(Type, Value) ->
    roundtrip('Constraints', Type, Value).

roundtrip(Module, Type, Value) ->
    {ok,Encoded} = Module:encode(Type, Value),
    {ok,Value} = Module:decode(Type, Encoded),
    Encoded.

roundtrip_enc(Type, Value, Enc) ->
    Module = 'Constraints',
    {ok,Encoded} = Module:encode(Type, Value),
    {ok,Value} = Module:decode(Type, Encoded),
    case Enc of
	none -> ok;
	Encoded -> ok
    end.

range_error(ber, Type, Value) ->
    %% BER: Values outside the effective range should be rejected
    %% on decode.
    {ok,Encoded} = 'Constraints':encode(Type, Value),
    {error,{asn1,_}} = 'Constraints':decode(Type, Encoded),
    ok;
range_error(Per, Type, Value) when Per =:= per; Per =:= uper ->
    %% (U)PER: Values outside the effective range should be rejected
    %% on encode.
    {error,_} = 'Constraints':encode(Type, Value),
    ok.
