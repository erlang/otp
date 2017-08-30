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
-module(testSeqOptional).

-include("External.hrl").
-export([main/1]).

-include_lib("common_test/include/ct.hrl").

-record('SeqOpt1',{bool1 = asn1_NOVALUE, int1, seq1 = asn1_NOVALUE}).
-record('SeqOpt1Imp',{bool1 = asn1_NOVALUE, int1, seq1 = asn1_NOVALUE}).
-record('SeqOpt1Exp',{bool1 = asn1_NOVALUE, int1, seq1 = asn1_NOVALUE}).
-record('SeqOpt2',{seq2 = asn1_NOVALUE, bool2, int2}).
-record('SeqOpt2Imp',{seq2 = asn1_NOVALUE, bool2, int2}).
-record('SeqOpt2Exp',{seq2 = asn1_NOVALUE, bool2, int2}).
-record('SeqOpt3',{bool3 = asn1_NOVALUE, seq3 = asn1_NOVALUE, int3 = asn1_NOVALUE}).
-record('SeqOpt3Imp',{bool3 = asn1_NOVALUE, seq3 = asn1_NOVALUE, int3 = asn1_NOVALUE}).
-record('SeqOpt3Exp',{bool3 = asn1_NOVALUE, seq3 = asn1_NOVALUE, int3 = asn1_NOVALUE}).
-record('SeqIn',{boolIn, intIn}).
-record('SeqChoOpt',{int, cho = asn1_NOVALUE}).

main(_Rules) ->
    roundtrip('SeqOpt1', #'SeqOpt1'{bool1=true,int1=15,
				    seq1=#'SeqIn'{boolIn=true,intIn=66}}),
    roundtrip('SeqOpt1', #'SeqOpt1'{bool1=asn1_NOVALUE,
				    int1=15,seq1=asn1_NOVALUE}),
    roundtrip('SeqOpt2', #'SeqOpt2'{seq2=#'SeqIn'{boolIn=true,intIn=66},
				    bool2=true,int2=15}),
    roundtrip('SeqOpt2', #'SeqOpt2'{seq2=asn1_NOVALUE,bool2=true,int2=15}),
    roundtrip('SeqOpt3', #'SeqOpt3'{bool3=true,
				    seq3=#'SeqIn'{boolIn=true,
						  intIn=66},int3=15}),
    roundtrip('SeqOpt3', #'SeqOpt3'{bool3=asn1_NOVALUE,
				    seq3=asn1_NOVALUE,int3=15}),
    roundtrip('SeqOpt1Imp', #'SeqOpt1Imp'{bool1=true,int1=15,
              seq1=#'SeqIn'{boolIn=true,intIn=66}}),
    roundtrip('SeqOpt1Imp', #'SeqOpt1Imp'{bool1=asn1_NOVALUE,
					  int1=15,seq1=asn1_NOVALUE}),
    roundtrip('SeqOpt2Imp', #'SeqOpt2Imp'{seq2=#'SeqIn'{boolIn=true,intIn=66},
					  bool2=true,int2=15}),
    roundtrip('SeqOpt2Imp', #'SeqOpt2Imp'{seq2=asn1_NOVALUE,
					  bool2=true,int2=15}),
    roundtrip('SeqOpt3Imp', #'SeqOpt3Imp'{bool3=true,
					  seq3=#'SeqIn'{boolIn=true,intIn=66},
					  int3=15}),
    roundtrip('SeqOpt3Imp', #'SeqOpt3Imp'{bool3=asn1_NOVALUE,
					  seq3=asn1_NOVALUE,int3=15}),
    roundtrip('SeqOpt1Exp', #'SeqOpt1Exp'{bool1=true,int1=15,
					  seq1=#'SeqIn'{boolIn=true,
							intIn=66}}),
    roundtrip('SeqOpt1Exp', #'SeqOpt1Exp'{bool1=asn1_NOVALUE,
					  int1=15,seq1=asn1_NOVALUE}),
    roundtrip('SeqOpt2Exp', #'SeqOpt2Exp'{seq2=#'SeqIn'{boolIn=true,intIn=66},
					  bool2=true,int2=15}),
    roundtrip('SeqOpt2Exp', #'SeqOpt2Exp'{seq2=asn1_NOVALUE,
					  bool2=true,int2=15}),
    roundtrip('SeqOpt3Exp', #'SeqOpt3Exp'{bool3=true,
					  seq3=#'SeqIn'{boolIn=true,intIn=66},
					  int3=15}),
    roundtrip('SeqOpt3Exp', #'SeqOpt3Exp'{bool3=asn1_NOVALUE,
					  seq3=asn1_NOVALUE,int3=15}),
    roundtrip('SeqChoOpt', #'SeqChoOpt'{int=15,cho={boolC,true}}),
    roundtrip('SeqChoOpt', #'SeqChoOpt'{int=15,cho=asn1_NOVALUE}),
    ok.

roundtrip(Type, Value) ->
    asn1_test_lib:roundtrip('SeqOptional', Type, Value).
