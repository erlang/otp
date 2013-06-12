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
-module(testChoExtension).

-export([extension/1]).

-include_lib("test_server/include/test_server.hrl").


extension(_Rules) ->
    roundtrip('ChoExt1', {bool,true}),
    roundtrip('ChoExt1', {int,33}),

    %% A trick to encode with another compatible CHOICE type to test reception
    %% extension alternative

    {ok,Bytes2x} = asn1_wrapper:encode('ChoExtension','ChoExt1x',{str,"abc"}),
    {ok,Val2x} =
	asn1_wrapper:decode('ChoExtension','ChoExt1',lists:flatten(Bytes2x)),
    io:format("Choice extension alternative = ~p~n",[Val2x]),

    roundtrip('ChoExt2', {bool,true}),
    roundtrip('ChoExt2', {int,33}),
    roundtrip('ChoExt3', {bool,true}),
    roundtrip('ChoExt3', {int,33}),
    roundtrip('ChoExt4', {str,"abc"}),

    roundtrip('ChoEmptyRoot', {bool,false}),
    roundtrip('ChoEmptyRoot', {bool,true}),
    roundtrip('ChoEmptyRoot', {int,0}),
    roundtrip('ChoEmptyRoot', {int,7}),

    ok.


roundtrip(Type, Value) ->
    {ok,Encoded} = 'ChoExtension':encode(Type, Value),
    {ok,Value} = 'ChoExtension':decode(Type, Encoded),
    ok.
