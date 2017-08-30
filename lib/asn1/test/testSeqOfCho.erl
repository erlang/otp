%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(testSeqOfCho).

-export([main/1]).

-include_lib("common_test/include/ct.hrl").

-record('SeqChoDef',{bool1, int1, seq1 = asn1_DEFAULT}).
-record('SeqChoOpt',{bool1, int1, seq1 = asn1_NOVALUE}).
-record('SeqChoEmbDef',{bool1, int1, seq1 = asn1_DEFAULT}).
-record('SeqChoEmbOpt',{bool1, int1, seq1 = asn1_NOVALUE}).
-record('SeqOfChoEmbDef_SEQOF',{bool1, int1, seq1 = asn1_DEFAULT}).
-record('SeqOfChoEmbOpt_SEQOF',{bool1, int1, seq1 = asn1_NOVALUE}).

main(_Rules) ->
    roundtrip('SeqChoDef',
	      #'SeqChoDef'{bool1=true,int1=17,seq1=asn1_DEFAULT},
	      #'SeqChoDef'{bool1=true,int1=17,seq1=[]}),
    roundtrip('SeqChoDef',
	      #'SeqChoDef'{bool1=true,int1=17,
			   seq1=[{boolIn,true},{intIn,25}]}),
    roundtrip('SeqChoOpt',
	      #'SeqChoOpt'{bool1=true,int1=17,seq1=asn1_NOVALUE}),
    roundtrip('SeqChoOpt',
	      #'SeqChoOpt'{bool1=true,int1=17,
			   seq1=[{boolIn,true},{intIn,25}]}),

    roundtrip('SeqChoEmbDef',
	      #'SeqChoEmbDef'{bool1=true,int1=17,seq1=asn1_DEFAULT},
	      #'SeqChoEmbDef'{bool1=true,int1=17,seq1=[]}),
    roundtrip('SeqChoEmbDef',
	      #'SeqChoEmbDef'{bool1=true,int1=17,
			      seq1=[{boolIn,true},{intIn,25}]}),
    roundtrip('SeqChoEmbOpt',
	      #'SeqChoEmbOpt'{bool1=true,int1=17,seq1=asn1_NOVALUE}),
    roundtrip('SeqChoEmbOpt',
	      #'SeqChoEmbOpt'{bool1=true,int1=17,
			      seq1=[{boolIn,true},{intIn,25}]}),

    roundtrip('SeqOfChoEmbDef',
	      [#'SeqOfChoEmbDef_SEQOF'{bool1=true,int1=17,seq1=asn1_DEFAULT}],
	      [#'SeqOfChoEmbDef_SEQOF'{bool1=true,int1=17,seq1=[]}]),
    roundtrip('SeqOfChoEmbDef',
	      [#'SeqOfChoEmbDef_SEQOF'{bool1=true,int1=17,
				       seq1=[{boolIn,true},{intIn,25}]}]),
    roundtrip('SeqOfChoEmbOpt',
	      [#'SeqOfChoEmbOpt_SEQOF'{bool1=true,int1=17,seq1=asn1_NOVALUE}]),
    roundtrip('SeqOfChoEmbOpt',
	      [#'SeqOfChoEmbOpt_SEQOF'{bool1=true,int1=17,
				       seq1=[{boolIn,true},{intIn,25}]}]),
    ok.

roundtrip(Type, Value) ->
    roundtrip(Type, Value, Value).

roundtrip(Type, Value, ExpectedValue) ->
    asn1_test_lib:roundtrip('SeqOfCho', Type, Value, ExpectedValue).
