%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2013. All Rights Reserved.
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
    {ok,{asn1_enum,0}} = 'EnumExt':decode('Ext', Or),

    %% ENUMERATED no extensionmark 
    B64 = <<64>>,
    B64 = roundtrip('Noext', red),
    common(Rule);
main(ber) ->
    io:format("main(ber)~n",[]),
    %% ENUMERATED with extensionmark (value is in root set)
    roundtrip('Ext', red),

    %% value is an extensionvalue
    {ok,Bytes1_1} = 'EnumExt':encode('Ext1', orange),
    {ok,{asn1_enum,7}} = 'EnumExt':decode('Ext', Bytes1_1),

    %% ENUMERATED no extensionmark
    roundtrip('Noext', red),
    {error,{asn1,_}} = (catch 'EnumExt':encode('Noext', orange)),
    
    %% ENUMERATED with atom 'com'
    roundtrip('Globalstate', preop),
    roundtrip('Globalstate', com),

    common(ber).

common(Erule) ->
    roundtrip('SubExt1', blue),
    roundtrip('SubExt1', orange),
    roundtrip('SubExt1', black),

    roundtrip('Seq', {'Seq',blue,42}),
    roundtrip('Seq', {'Seq',red,42}),
    roundtrip('Seq', {'Seq',green,42}),
    roundtrip('Seq', {'Seq',orange,47}),
    roundtrip('Seq', {'Seq',black,4711}),
    roundtrip('Seq', {'Seq',magenta,4712}),

    [begin
	 S = io_lib:format("e~2.016.0b", [I]),
	 E = list_to_atom(lists:flatten(S)),
	 roundtrip('SeqBig', {'SeqBig',true,E,9357})
     end || I <- lists:seq(0, 128)],

    v_roundtrip(Erule, 'SeqBig', {'SeqBig',true,e40,9357}),
    v_roundtrip(Erule, 'SeqBig', {'SeqBig',true,e80,9357}),
    ok.

roundtrip(Type, Value) ->
    asn1_test_lib:roundtrip_enc('EnumExt', Type, Value).

v_roundtrip(Erule, Type, Value) ->
    Encoded = roundtrip(Type, Value),
    Encoded = asn1_test_lib:hex_to_bin(v(Erule, Value)).

v(ber, {'SeqBig',true,e40,9357}) -> "300A8001 FF810141 8202248D";
v(ber, {'SeqBig',true,e80,9357}) -> "300B8001 FF810200 81820224 8D";
v(per, {'SeqBig',true,e40,9357}) -> "E0014002 248D";
v(per, {'SeqBig',true,e80,9357}) -> "E0018002 248D";
v(uper, {'SeqBig',true,e40,9357}) -> "E0280044 91A0";
v(uper, {'SeqBig',true,e80,9357}) -> "E0300044 91A0".
