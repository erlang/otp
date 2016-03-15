%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
-module(testEnumExt).

-export([main/1]).

-include_lib("common_test/include/ct.hrl").

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

    v_roundtrip(Erule, 'EnumSkip', d),

    ok.

roundtrip(Type, Value) ->
    asn1_test_lib:roundtrip_enc('EnumExt', Type, Value).

v_roundtrip(Erule, Type, Value) ->
    Encoded = roundtrip(Type, Value),
    Encoded = asn1_test_lib:hex_to_bin(v(Erule, Type, Value)).

v(Erule, 'SeqBig', Value) ->
    v_seq_big(Erule, Value);
v(Erule, 'EnumSkip', Value) ->
    v_enum_skip(Erule, Value).

v_seq_big(ber, {'SeqBig',true,e40,9357}) -> "300A8001 FF810141 8202248D";
v_seq_big(ber, {'SeqBig',true,e80,9357}) -> "300B8001 FF810200 81820224 8D";
v_seq_big(per, {'SeqBig',true,e40,9357}) -> "E0014002 248D";
v_seq_big(per, {'SeqBig',true,e80,9357}) -> "E0018002 248D";
v_seq_big(uper, {'SeqBig',true,e40,9357}) -> "E0280044 91A0";
v_seq_big(uper, {'SeqBig',true,e80,9357}) -> "E0300044 91A0".

v_enum_skip(per, d) -> "82";
v_enum_skip(uper, d) -> "82";
v_enum_skip(ber, d) -> "0A0103".
