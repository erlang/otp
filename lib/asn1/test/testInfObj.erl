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

-module(testInfObj).

-export([main/1]).

-record('InitiatingMessage',{procedureCode,criticality,value}).
-record('InitiatingMessage2',{procedureCode,criticality,value}).
-record('Iu-ReleaseCommand',{first,second}).

main(_Erule) ->
    Val1 = #'InitiatingMessage'{procedureCode=1,
				criticality=ignore,
				value=#'Iu-ReleaseCommand'{
				  first=13,
				  second=true}},
    roundtrip('RANAPextract1', 'InitiatingMessage', Val1),
    roundtrip('InfObj', 'InitiatingMessage', Val1),

    Val2 = Val1#'InitiatingMessage'{procedureCode=2},
    {error,_R1} = 'InfObj':encode('InitiatingMessage', Val2),
    

    %% Test case for OTP-4275
    Val3 = #'InitiatingMessage2'{procedureCode=3,
				 criticality=reject,
				 value=#'Iu-ReleaseCommand'{
				   first=13,
				   second=true}},

    roundtrip('RANAPextract1', 'InitiatingMessage2', Val3),

    roundtrip('InfObj', 'MyPdu', {'MyPdu',42,12,false,"string"}),
    roundtrip('InfObj', 'MyPdu', {'MyPdu',{'Seq',1023,<<"hello">>},
				  42,true,"longer string"}),
    roundtrip('InfObj', 'MyPdu', {'MyPdu',"75712346",43,true,"string"}),

    roundtrip('InfObj', 'ConstructedPdu',
	      {'ConstructedPdu',1,{'CONSTRUCTED-DEFAULT_Type',-2001,true}}),
    roundtrip('InfObj', 'ConstructedPdu',
	      {'ConstructedPdu',2,{'CONSTRUCTED-DEFAULT_Type',999,false}}),
    roundtrip('InfObj', 'ConstructedPdu',
	      {'ConstructedPdu',3,true}),
    {'ConstructedPdu',4,{_,42,<<13:7>>}} =
	enc_dec('InfObj', 'ConstructedPdu',
		{'ConstructedPdu',4,{'',42,<<13:7>>}}),
    roundtrip('InfObj', 'ConstructedPdu',
	      {'ConstructedPdu',5,{i,-250138}}),
    roundtrip('InfObj', 'ConstructedPdu',
	      {'ConstructedPdu',5,{b,<<13456:15>>}}),
    roundtrip('InfObj', 'ConstructedPdu',
	      {'ConstructedPdu',6,[]}),
    roundtrip('InfObj', 'ConstructedPdu',
	      {'ConstructedPdu',6,[10,7,16,1,5,13,12]}),
    roundtrip('InfObj', 'ConstructedPdu',
	      {'ConstructedPdu',7,[]}),
    roundtrip('InfObj', 'ConstructedPdu',
	      {'ConstructedPdu',7,[64,1,19,17,35]}),
    {'ConstructedPdu',8,[{_,-15,35},{_,533,-70}]} =
	enc_dec('InfObj', 'ConstructedPdu',
		{'ConstructedPdu',8,[{'_',-15,35},{'_',533,-70}]}),
    {'ConstructedPdu',9,[{RecTag9,-15,35},{RecTag9,533,-70}]} =
	enc_dec('InfObj', 'ConstructedPdu',
		{'ConstructedPdu',9,[{'_',-15,35},{'_',533,-70}]}),

    roundtrip('InfObj', 'ConstructedSet',
	      {'ConstructedSet',1,{'CONSTRUCTED-DEFAULT_Type',-2001,true}}),
    roundtrip('InfObj', 'ConstructedSet',
	      {'ConstructedSet',2,{'CONSTRUCTED-DEFAULT_Type',999,false}}),
    roundtrip('InfObj', 'ConstructedSet',
	      {'ConstructedSet',3,true}),
    {'ConstructedSet',4,{_,42,<<13:7>>}} =
	enc_dec('InfObj', 'ConstructedSet',
		{'ConstructedSet',4,{'',42,<<13:7>>}}),
    roundtrip('InfObj', 'ConstructedSet',
	      {'ConstructedSet',5,{i,-250138}}),
    roundtrip('InfObj', 'ConstructedSet',
	      {'ConstructedSet',5,{b,<<13456:15>>}}),
    roundtrip('InfObj', 'ConstructedSet',
	      {'ConstructedSet',6,[]}),
    roundtrip('InfObj', 'ConstructedSet',
	      {'ConstructedSet',6,[10,7,16,1,5,13,12]}),
    roundtrip('InfObj', 'ConstructedSet',
	      {'ConstructedSet',7,[]}),
    roundtrip('InfObj', 'ConstructedSet',
	      {'ConstructedSet',7,[64,1,19,17,35]}),
    {'ConstructedSet',8,[{_,-15,35},{_,533,-70}]} =
	enc_dec('InfObj', 'ConstructedSet',
		{'ConstructedSet',8,[{'_',-15,35},{'_',533,-70}]}),
    {'ConstructedSet',9,[{_,-15,35},{_,533,-70}]} =
	enc_dec('InfObj', 'ConstructedSet',
		{'ConstructedSet',9,[{'_',-15,35},{'_',533,-70}]}),

    roundtrip('InfObj', 'Seq2',
	      {'Seq2',42,[true,false,false,true],
	       [false,true,false]}),

    roundtrip('InfObj', 'OptionalInSeq', {'OptionalInSeq',3,true}),
    roundtrip('InfObj', 'OptionalInSeq', {'OptionalInSeq',3,asn1_NOVALUE}),

    roundtrip('InfObj', 'DefaultInSeq', {'DefaultInSeq',3,false}),
    roundtrip('InfObj', 'DefaultInSeq', {'DefaultInSeq',3,true}),
    {'DefaultInSeq',3,true} =
	enc_dec('InfObj', 'DefaultInSeq', {'DefaultInSeq',3,asn1_DEFAULT}),

    roundtrip('InfObj', 'Multiple-Optionals',
	      {'Multiple-Optionals',1,42,true,<<"abc">>}),
    roundtrip('InfObj', 'Multiple-Optionals',
	      {'Multiple-Optionals',1,asn1_NOVALUE,true,<<"abc">>}),
    roundtrip('InfObj', 'Multiple-Optionals',
	      {'Multiple-Optionals',1,42,asn1_NOVALUE,<<"abc">>}),
    roundtrip('InfObj', 'Multiple-Optionals',
	      {'Multiple-Optionals',1,42,true,asn1_NOVALUE}),
    roundtrip('InfObj', 'Multiple-Optionals',
	      {'Multiple-Optionals',1,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE}),

    test_objset('OstSeq12', [1,2]),
    test_objset('OstSeq123', [1,2,3]),
    test_objset('OstSeq1234', [1,2,3,4]),
    test_objset('OstSeq45', [4,5]),
    test_objset('OstSeq12345', [1,2,3,4,5]),

    test_objset('OstSeq12Except', [1,2]),
    test_objset('OstSeq123Except', [1,2]),

    test_objset('ExOstSeq12', [1,2]),
    test_objset('ExOstSeq123', [1,2,3]),
    test_objset('ExOstSeq1234', [1,2,3,4]),
    test_objset('ExOstSeq45', [4,5]),
    test_objset('ExOstSeq12345', [1,2,3,4,5]),

    test_objset('ExOstSeq12Except', [1,2]),
    test_objset('ExOstSeq123Except', [1,2]),

    roundtrip('InfObj', 'ExtClassSeq', {'ExtClassSeq', 4}),

    {1,2,42} = 'InfObj':'value-1'(),
    {1,2,42,25} = 'InfObj':'value-2'(),
    {100,101} = 'InfObj':'value-3'(),
    {1,2,100,101} = 'InfObj':'value-4'(),

    roundtrip('InfObj', 'Rdn', {'Rdn',{2,5,4,41},"abc"}),

    roundtrip('InfObj', 'TiAliasSeq',
	      {'TiAliasSeq',{'TiAliasSeq_prf',{2,1,2},'NULL'}}),

    roundtrip('InfObj', 'ContentInfo',
	      {'ContentInfo',{2,7,8,9},"string"}),
    {2,7,8,9} = 'InfObj':'id-content-type'(),

    <<2#1011:4>> = 'InfObj':'tricky-bit-string'(),
    <<16#CAFE:16>> = 'InfObj':'tricky-octet-string'(),

    ok.

test_objset(Type, Keys) ->
    _ = [test_object(Type, Key) || Key <- Keys],
    _ = [(catch test_object(Type, Key)) ||
	    Key <- lists:seq(1, 5) -- Keys],
    ok.

test_object(T, 1) ->
    roundtrip('InfObj', T, {T,1,<<42:7>>});
test_object(T, 2) ->
    roundtrip('InfObj', T, {T,2,<<"abc">>});
test_object(T, 3) ->
    roundtrip('InfObj', T, {T,3,donald}),
    roundtrip('InfObj', T, {T,3,scrooge});
test_object(T, 4) ->
    roundtrip('InfObj', T, {T,4,true}),
    roundtrip('InfObj', T, {T,4,false});
test_object(T, 5) ->
    roundtrip('InfObj', T, {T,5,0}),
    roundtrip('InfObj', T, {T,5,15}).

roundtrip(M, T, V) ->
    asn1_test_lib:roundtrip(M, T, V).

enc_dec(M, T, V0) ->
    {ok,Enc} = M:encode(T, V0),
    asn1_test_lib:map_roundtrip(M, T, Enc),
    {ok,V} = M:decode(T, Enc),
    V.
