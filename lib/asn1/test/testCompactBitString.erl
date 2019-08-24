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
-module(testCompactBitString).

-export([compact_bit_string/1, bit_string_unnamed/1,otp_4869/1,
	 ticket_7734/1]).

compact_bit_string(Rules) ->

    %%==========================================================
    %% Bs1 ::= BIT STRING
    %%==========================================================

    roundtrip('Bs1', 0, {0,<<>>}),
    roundtrip('Bs1', 4, {5,<<2#00100000>>}),
    roundtrip('Bs1', 15, {4,<<2#11110000>>}),
    roundtrip('Bs1', 255, {0,<<2#11111111>>}),
    roundtrip('Bs1', 256, {7,<<16#00,16#80>>}),
    roundtrip('Bs1', 257, {7,<<16#80,16#80>>}),
    roundtrip('Bs1', 444, {7,<<16#3D,16#80>>}),
    roundtrip('Bs1', 12345678901234567890,
	      {0,<<75,80,248,215,49,149,42,213>>}),

    roundtrip('Bs1', [1,1,1,1,1,1,1,1], {0,<<255>>}),
    roundtrip('Bs1', [0,1,0,0,1,0], {2,<<16#48>>}),
    roundtrip('Bs1', [1,0,0,0,0,0,0,0,0], {7,<<16#80,0>>}),
    roundtrip('Bs1', [0,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,0,1,1],
	      {5,<<75,226,96>>}),
    
    case Rules of
	ber ->
	    {ok,{4,<<73,32>>}} =
		'PrimStrings':decode('Bs1', <<35,8,3,2,0,73,3,2,4,32>>),
	    {ok,{7,<<234,156,0>>}} =
		'PrimStrings':decode('Bs1', <<35,9,3,2,0,234,3,3,7,156,0>>),
	    {ok,{4,<<73,32>>}} =
		'PrimStrings':decode('Bs1', <<35,128,3,2,0,73,3,2,4,32,0,0>>),
	    {ok,{7,<<234,156,0>>}} =
		'PrimStrings':decode('Bs1',
				     <<35,128,3,2,0,234,3,3,7,156,0,0,0>>);
	_ ->
	    ok
    end,

    %% Test OTP-4200
    roundtrip('Bs1',  {0,<<0,0,1,1>>}),

    %%==========================================================
    %% Bs2 ::= BIT STRING {su(0), mo(1), tu(2), we(3), th(4), fr(5), sa(6) } (SIZE (7))
    %%==========================================================

    roundtrip('Bs2', [mo,tu,fr]),
    roundtrip('Bs2', [0,1,1,0,0,1,0], [mo,tu,fr]),
    
    %%==========================================================
    %% Bs3 ::= BIT STRING {su(0), mo(1), tu(2), we(3), th(4), fr(5), sa(6) } (SIZE (1..7))
    %%==========================================================
    
    roundtrip('Bs3', [mo,tu,fr]),
    roundtrip('Bs3', [0,1,1,0,0,1,0], [mo,tu,fr]),
    
    %%==========================================================
    %% BsPri ::= [PRIVATE 61] BIT STRING
    %%==========================================================

    roundtrip('BsPri', 45, {2,<<180>>}),
    roundtrip('BsPri', 211, {0,<<203>>}),

    case Rules of
	ber ->
	    {ok,{5,<<75,226,96>>}} =
		'PrimStrings':decode('BsPri',
				     <<223,61,4,5,75,226,96>>),

	    {ok,{5,<<75,226,96>>}} =
		'PrimStrings':decode('BsPri',
				     <<255,61,128,3,4,5,75,226,96,0,0>>),

	    {ok,{5,<<75,226,96>>}} =
		'PrimStrings':decode('BsPri',
				     <<255,61,9,3,2,0,75,3,3,5,226,96>>),

	    {ok,{5,<<75,226,96>>}} =
		'PrimStrings':decode('BsPri',
				     <<255,61,128,3,2,0,75,3,3,5,226,96,0,0>>),
	    ok;
	_ ->
	    ok
    end,
    
    
    %%==========================================================
    %% BsExpPri ::= [PRIVATE 61] EXPLICIT BIT STRING
    %%==========================================================

    roundtrip('BsExpPri', 45, {2,<<180>>}),
    roundtrip('BsExpPri', 211, {0,<<203>>}),

    case Rules of
	ber ->
	    {ok,{5,<<75,226,96>>}} =
		'PrimStrings':decode('BsExpPri', <<255,61,6,3,4,5,75,226,96>>);
	_ ->
	    ok
    end,

    ok.

ticket_7734(_) ->
    BS = {0,list_to_binary(lists:duplicate(128, 0))},
    roundtrip('BS1024', BS).

bit_string_unnamed(_Rules) ->
    roundtrip('TransportLayerAddress', [0,1,1,0], {4,<<96>>}).

otp_4869(per) ->
    Val1 = {'IP',[0],{0,<<62,235,90,50,0,0,0,0,0,0,0,0,0,0,0,0>>},asn1_NOVALUE},
    Val2 = {'IP',[0],[0,0,1,1,1,1,1,0,1,1,1,0,1,0,1,1,0,1,0,1,1,0,
		      1,0,0,0,1,1,0,0,1,0] ++
	    lists:duplicate(128 - 32, 0),asn1_NOVALUE},
    {ok,Encoded} = 'Constraints':encode('IP', Val1),
    {ok,Encoded} = 'Constraints':encode('IP', Val2),
    ok;
otp_4869(_) ->
    ok.

roundtrip(Type, Val) ->
    roundtrip_1('PrimStrings', Type, Val, Val).

roundtrip(Type, Val1, Val2) ->
    roundtrip_1('PrimStrings', Type, Val1, Val2).

roundtrip_1(Mod, Type, In, Out) ->
    {ok,Encoded} = Mod:encode(Type, In),
    {ok,Out} = Mod:decode(Type, Encoded),
    %% Test that compact BIT STRINGs can be encoded.
    {ok,Encoded} = Mod:encode(Type, Out),
    ok.
