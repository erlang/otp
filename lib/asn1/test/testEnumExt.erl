%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2012. All Rights Reserved.
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
-module(testEnumExt).

-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

main(Rule) when Rule =:= per; Rule =:= uper ->
    io:format("main(~p)~n",[Rule]),

    %% ENUMERATED with extensionmark (value is in root set)
    B32 = <<32>>,
    B32 = roundtrip('Ext', red),

    %% ENUMERATED with extensionmark (value is an extensionvalue)
    Or = roundtrip('Ext1', orange),
    %% unknown extensionvalue
    {ok,{asn1_enum,0}} = asn1_wrapper:decode('EnumExt','Ext',Or),

    %% ENUMERATED no extensionmark 
    B64 = <<64>>,
    B64 = roundtrip('Noext', red),
    ok;

main(ber) ->
    io:format("main(ber)~n",[]),
    %% ENUMERATED with extensionmark (value is in root set)
    roundtrip('Ext', red),

    %% value is an extensionvalue
    {ok,Bytes1_1} = asn1_wrapper:encode('EnumExt','Ext1',orange),
    {ok,{asn1_enum,7}} = asn1_wrapper:decode('EnumExt','Ext',lists:flatten(Bytes1_1)),

    %% ENUMERATED no extensionmark
    roundtrip('Noext', red),
    ?line {error,{asn1,_}} = (catch asn1_wrapper:encode('EnumExt','Noext',orange)),
    
    %% ENUMERATED with atom 'com'
    roundtrip('Globalstate', preop),
    roundtrip('Globalstate', com),

    ok.

roundtrip(Type, Value) ->
    {ok,Encoded} = 'EnumExt':encode(Type, Value),
    {ok,Value} = 'EnumExt':decode(Type, Encoded),
    Encoded.
