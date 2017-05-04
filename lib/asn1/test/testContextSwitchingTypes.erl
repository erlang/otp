%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2017. All Rights Reserved.
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
-module(testContextSwitchingTypes).

-export([test/1]).

-include_lib("common_test/include/ct.hrl").

test(Config) ->
    ValT_1 = 'ContextSwitchingTypes':'val1-T'(),
    check_EXTERNAL(enc_dec('T', ValT_1)),

    ValT_2 = 'ContextSwitchingTypes':'val2-T'(),
    check_EXTERNAL(enc_dec('T', ValT_2)),

    ValT_3 = 'ContextSwitchingTypes':'val3-T'(),
    check_EXTERNAL(enc_dec('T', ValT_3)),

    ValT_4 = 'ContextSwitchingTypes':'val4-T'(),
    check_EXTERNAL(enc_dec('T', ValT_4)),

    {ok,ValT2} = asn1ct:value('ContextSwitchingTypes', 'T',
			      [{i,proplists:get_value(case_dir, Config)}]),
    io:format("ValT2 ~p~n",[ValT2]),
    check_EXTERNAL(enc_dec('T', ValT2)),

    ValEP = 'ContextSwitchingTypes':'val1-EP'(),
    ValEPDec = enc_dec('EP', ValEP),
    io:format("~p\n~p\n", [ValEP,ValEPDec]),

    ValCS = 'ContextSwitchingTypes':'val1-CS'(),
    ValCSDec = enc_dec('EP', ValCS),
    io:format("~p\n~p\n", [ValCS,ValCSDec]),
    ok.


check_EXTERNAL({'EXTERNAL',Identif,DVD,DV})->
    %% EXTERNAL in the 1994 format.
    case Identif of
	{'context-negotiation',_} ->
	    ok;
	{'presentation-context-id',Id} ->
	    true = is_integer(Id);
	{syntax,ObjId} ->
	    check_object_identifier(ObjId)
    end,
    check_EXTERNAL_DVD(DVD),
    check_EXTERNAL_DV(DV);
check_EXTERNAL({'EXTERNAL',ObjId,IndirectRef,Descriptor,Enc})->
    %% EXTERNAL in the 1990 format.
    check_object_identifier(ObjId),
    true = is_integer(IndirectRef),
    true = is_binary(Descriptor) orelse is_list(Descriptor),
    case Enc of
	{arbitrary,_} -> ok;
	{'single-ASN1-type',_} -> ok;
	{'octet-aligned',_} -> ok
    end.

check_EXTERNAL_DVD(DVD) when is_list(DVD) -> ok;
check_EXTERNAL_DVD(asn1_NOVALUE) -> ok.

check_EXTERNAL_DV(DV) when is_list(DV); is_binary(DV) -> ok.

check_object_identifier(Tuple) when is_tuple(Tuple) ->
    %% An OBJECT IDENTIFIER is a tuple with integer elements.
    case [E || E <- tuple_to_list(Tuple),
	       not is_integer(E)] of
	[] -> ok
    end.

enc_dec(T, V0) ->
    M = 'ContextSwitchingTypes',
    {ok,Enc} = M:encode(T, V0),
    asn1_test_lib:map_roundtrip(M, T, Enc),
    {ok,V} = M:decode(T, Enc),
    V.
