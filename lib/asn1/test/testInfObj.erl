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
    roundtrip('InfObj', 'MyPdu', {'MyPdu',{'Seq',1023,"hello"},
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
	      {'Multiple-Optionals',1,42,true,"abc"}),
    roundtrip('InfObj', 'Multiple-Optionals',
	      {'Multiple-Optionals',1,asn1_NOVALUE,true,"abc"}),
    roundtrip('InfObj', 'Multiple-Optionals',
	      {'Multiple-Optionals',1,42,asn1_NOVALUE,"abc"}),
    roundtrip('InfObj', 'Multiple-Optionals',
	      {'Multiple-Optionals',1,42,true,asn1_NOVALUE}),
    roundtrip('InfObj', 'Multiple-Optionals',
	      {'Multiple-Optionals',1,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE}).

roundtrip(M, T, V) ->
    asn1_test_lib:roundtrip(M, T, V).

enc_dec(M, T, V0) ->
    {ok,Enc} = M:encode(T, V0),
    {ok,V} = M:decode(T, Enc),
    V.
