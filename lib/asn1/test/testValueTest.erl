%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2016. All Rights Reserved.
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
-module(testValueTest).

-export([main/0]).

main() ->
    M = 'ValueTest',

    %% Basic types
    12 = M:'vANY'(),
    true = M:'vBOOLEAN'(),
    12 = M:'vINTEGER'(),
    0 = M:'vINTEGERNNL'(),
    button1 = M:'vENUMERATED'(),
    [zero,two] = M:'vBS'(),
    'NULL' = M:'vNULL'(),
    <<16#31,16#32,16#33>> = M:'vOS'(),

    %% OID
    {2,1,1} = M:'vOD'(),
    {1,2} = M:'integer-first'(),
    {2,4,5} = M:'rel-oid-1'(),
    {0,2,4,5} = M:'include-roid'(),
    {1,2,1} = M:'include-oid'(),
    {1,2,1,2,4,5,42} = M:'include-all'(),

    %% Character strings
    "01234567" = M:'numericstring'(),
    "PrintableString" = M:'printablestring'(),
    "VisibleString" = M:'visiblestring'(),
    [0,13] = M:'cr'(),
    ["First line",[0,13],"Second line"] = M:'ia5string1'(),
    [[5,5],[4,4],[6,6]] = M:'ia5string2'(),
    "TeletexString" = M:'teletexstring'(),
    "VideotexString" = M:'videotexstring'(),
    "97100211-0500" = M:'utctime'(),
    "19971002103130.5" = M:'generalizedtime'(),
    "ObjectDescriptor" = M:'objectdescriptor'(),
    "GraphicString" = M:'graphicstring'(),
    "GeneralString" = M:'generalstring'(),
    "BMPString" = M:'bmpstring1'(),
    [0,0,0,65] = M:'latinCapitalLetterA'(),
    [0,0,3,145] = M:'greekCapitalLetterSigma'(),
    ["This is a capital A: ",
     [0,0,0,65],
     ", and a capital sigma: ",
     [0,0,3,145],
     "; try and spot the difference!"] = M:'my-universalstring'(),

    %% Integers
    42 = M:someInteger(),
    42 = M:otherInteger(),
    {'IntegerSeq',42} = M:integerSeq1(),

    %% Value from object
    2 = M:'int-from-object-1'(),
    4 = M:'int-from-object-2'(),
    roundtrip_error('II', 1),
    roundtrip('II', 2),
    roundtrip('II', 3),
    roundtrip('II', 4),
    roundtrip_error('II', 5),

    %% Recursive value definitions.
    {'OctetStringSeq',<<16#40,16#41,16#42>>} = M:octetStringSeq1(),
    <<16#40,16#41,16#42>> = M:otherOctetString(),
    <<16#40,16#41,16#42>> = M:someOctetString(),
    {'OctetStringSeq',<<16#40,16#41,16#42>>} = M:octetStringSeq2(),
    {'OctetStringSeq',<<16#40,16#41,16#FF>>} = M:octetStringSeq3(),
    <<16#40,16#41,16#FF>> = M:'os-1'(),
    <<16#40,16#41,16#FF>> = M:'os-2'(),

    %% Recursive BIT STRING definitions.
    {'BsSeq',<<2#101101:6>>,[c]} = M:bsSeq1(),
    {'BsSeq',<<2#101101:6>>,[c]} = M:bsSeq2(),
    {'BsSeq',<<2#101:3>>,[a,c]} = M:bsSeq3(),
    <<2#101101:6>> = M:someBitString(),
    <<2#101101:6>> = M:otherBitString(),
    <<2#101:3>> = M:bsFromObject(),
    <<2#101:3>> = M:bsFromObjectInd(),
    [c] = M:someNamedBs(),
    [c] = M:someOtherNamedBs(),

    ok.


roundtrip(T, V) ->
    asn1_test_lib:roundtrip('ValueTest', T, V).

roundtrip_error(T, V) ->
    try asn1_test_lib:roundtrip('ValueTest', T, V) of
	ok ->
	    test_server:fail()
    catch _:_ ->
	    ok
    end.
