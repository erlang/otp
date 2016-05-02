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
-module(testSeqTypeRefSeq).

-export([main/1]).

-include_lib("common_test/include/ct.hrl").

-record('Seq1',{bool1, int1, seq1}).
-record('Seq2',{seq2, bool2, int2}).
-record('Seq3',{bool3, seq3, int3}).
-record('Seq4',{seq41, seq42, seq43}).
-record('SeqIn',{boolIn, intIn}).
-record('SeqS1',{boolS1, intS1, seqS1}).
-record('SeqS1_seqS1',{boolIn, intIn}).
-record('SeqS2',{seqS2, boolS2, intS2}).
-record('SeqS2_seqS2',{boolIn, intIn}).
-record('SeqS3',{boolS3, seqS3, intS3}).
-record('SeqS3_seqS3',{boolIn, intIn}).
-record('SeqSTag',{seqS1, seqS2, seqS3}).
-record('SeqSTag_seqS1',{b1, i1}).
-record('SeqSTag_seqS2',{b2, i2}).
-record('SeqSTag_seqS3',{b3, i3}).
-record('SeqTRseq',{seqSeq, seqSeqI, seqSeqE, 'seqSeq-I', 'seqSeqI-I', 'seqSeqE-I', 'seqSeq-E', 'seqSeqI-E', 'seqSeqE-E'}).
-record('SeqSeq',{seqInt, seqOs}).
-record('SeqSeqImp',{seqInt, seqOs}).
-record('SeqSeqExp',{seqInt, seqOs}).


main(_Rules) ->
    roundtrip('Seq1',
	      #'Seq1'{bool1=true,int1=15,
		      seq1=#'SeqIn'{boolIn=true,intIn=66}}),
    roundtrip('Seq2',
	      #'Seq2'{seq2=#'SeqIn'{boolIn=true,intIn=66},
		      bool2=true,int2=15}),
    roundtrip('Seq3',
	      #'Seq3'{bool3=true,seq3=#'SeqIn'{boolIn=true,intIn=66},int3=15}),
    roundtrip('Seq4',
	      #'Seq4'{seq41=#'SeqIn'{boolIn=true,intIn=66},
		      seq42=#'SeqIn'{boolIn=true,intIn=66},
		      seq43=#'SeqIn'{boolIn=true,intIn=66}}),
    roundtrip('SeqS1',
	      #'SeqS1'{boolS1=true,intS1=15,
		       seqS1=#'SeqS1_seqS1'{boolIn=true,intIn=66}}),
    roundtrip('SeqS2',
	      #'SeqS2'{seqS2=#'SeqS2_seqS2'{boolIn=true,intIn=66},
		       boolS2=true,intS2=15}),
    roundtrip('SeqS3',
	      #'SeqS3'{boolS3=true,seqS3=#'SeqS3_seqS3'{boolIn=true,intIn=66},
		       intS3=15}),
    roundtrip('SeqSTag',
	      #'SeqSTag'{seqS1=#'SeqSTag_seqS1'{b1=true,i1=11},
			 seqS2=#'SeqSTag_seqS2'{b2=true,i2=22},
			 seqS3=#'SeqSTag_seqS3'{b3=true,i3=33}}),
    roundtrip('SeqTRseq',
	      #'SeqTRseq'{seqSeq=#'SeqSeq'{seqInt=2,seqOs = <<"A1">>},
			  seqSeqI=#'SeqSeq'{seqInt=2,seqOs = <<"A2">>},
			  seqSeqE=#'SeqSeq'{seqInt=2,seqOs = <<"A3">>},
			  'seqSeq-I'=#'SeqSeqImp'{seqInt=2,seqOs = <<"A4">>},
			  'seqSeqI-I'=#'SeqSeqImp'{seqInt=2,seqOs = <<"A5">>},
			  'seqSeqE-I'=#'SeqSeqImp'{seqInt=2,seqOs = <<"A6">>},
			  'seqSeq-E'=#'SeqSeqExp'{seqInt=2,seqOs = <<"A7">>},
			  'seqSeqI-E'=#'SeqSeqExp'{seqInt=2,seqOs = <<"A8">>},
			  'seqSeqE-E'=#'SeqSeqExp'{seqInt=2,seqOs = <<"A9">>}}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SeqTypeRefSeq', T, V).
