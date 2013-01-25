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
-module(testContextSwitchingTypes).

-export([test/1]).

-include_lib("test_server/include/test_server.hrl").

test(Config) ->
    ?line ValT = 'ContextSwitchingTypes':'val1-T'(),
    ?line {ok,Bytes1} =
	asn1_wrapper:encode('ContextSwitchingTypes','T',ValT),
    ?line {ok,Result1} = 
	asn1_wrapper:decode('ContextSwitchingTypes','T',Bytes1),
    ?line ok = check_EXTERNAL(Result1),
    ?line {ok,ValT2} = asn1ct:value('ContextSwitchingTypes','T',
                                    [{i, ?config(case_dir, Config)}]),
    ?line {ok,Bytes1_2} =
	asn1_wrapper:encode('ContextSwitchingTypes','T',ValT2),
    ?line {ok,Result1_2} = 
	asn1_wrapper:decode('ContextSwitchingTypes','T',Bytes1_2),
    ?line ok = check_EXTERNAL(Result1_2),

    ?line ValEP = 'ContextSwitchingTypes':'val1-EP'(),
    ?line {ok,Bytes2} =
	asn1_wrapper:encode('ContextSwitchingTypes','EP',ValEP),
    ?line {ok,_Result2} = 
	asn1_wrapper:decode('ContextSwitchingTypes','EP',Bytes2),

    ?line ValCS = 'ContextSwitchingTypes':'val1-CS'(),
    ?line {ok,Bytes3} =
	asn1_wrapper:encode('ContextSwitchingTypes','CS',ValCS),
    ?line {ok,_Result3} = 
	asn1_wrapper:decode('ContextSwitchingTypes','CS',Bytes3).


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
