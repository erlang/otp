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
-module(testSeqDefault).

-include("External.hrl").
-export([main/1]).

-include_lib("common_test/include/ct.hrl").

-record('SeqDef1',{bool1 = asn1_DEFAULT, int1, seq1 = asn1_DEFAULT}).
-record('SeqDef1Imp',{bool1 = asn1_DEFAULT, int1, seq1 = asn1_DEFAULT}).
-record('SeqDef1Exp',{bool1 = asn1_DEFAULT, int1, seq1 = asn1_DEFAULT}).
-record('SeqDef2',{seq2 = asn1_DEFAULT, bool2 = asn1_DEFAULT, int2}).
-record('SeqDef2Imp',{seq2 = asn1_DEFAULT, bool2 = asn1_DEFAULT, int2}).
-record('SeqDef2Exp',{seq2 = asn1_DEFAULT, bool2, int2}).
-record('SeqDef3',{bool3 = asn1_DEFAULT, seq3 = asn1_DEFAULT, int3 = asn1_DEFAULT}).
-record('SeqDef3Imp',{bool3 = asn1_DEFAULT, seq3 = asn1_DEFAULT, int3 = asn1_DEFAULT}).
-record('SeqDef3Exp',{bool3 = asn1_DEFAULT, seq3 = asn1_DEFAULT, int3 = asn1_DEFAULT}).
-record('SeqIn',{boolIn = asn1_NOVALUE, intIn = 12}).


main(_Rules) ->
    roundtrip('SeqDef1', #'SeqDef1'{bool1=true,int1=15,seq1=#'SeqIn'{boolIn=true,intIn=66}}),
    roundtrip('SeqDef1',
	      #'SeqDef1'{bool1=asn1_DEFAULT,int1=15,seq1=asn1_DEFAULT},
	      #'SeqDef1'{bool1=true,int1=15,seq1=#'SeqIn'{}}),

    roundtrip('SeqDef2', #'SeqDef2'{seq2=#'SeqIn'{boolIn=true,intIn=66},bool2=true,int2=15}),
    roundtrip('SeqDef2',
	      #'SeqDef2'{seq2=asn1_DEFAULT,bool2=asn1_DEFAULT,int2=15},
	      #'SeqDef2'{seq2=#'SeqIn'{},bool2=true,int2=15}),

    roundtrip('SeqDef3', #'SeqDef3'{bool3=true,seq3=#'SeqIn'{boolIn=true,intIn=66},int3=15}),
    roundtrip('SeqDef3',
	      #'SeqDef3'{bool3=asn1_DEFAULT,seq3=asn1_DEFAULT,int3=15},
	      #'SeqDef3'{bool3=true,seq3=#'SeqIn'{},int3=15}),

    roundtrip('SeqDef1Imp', #'SeqDef1Imp'{bool1=true,int1=15,
              seq1=#'SeqIn'{boolIn=true,intIn=66}}),
    roundtrip('SeqDef1Imp',
	      #'SeqDef1Imp'{bool1=asn1_DEFAULT,int1=15,seq1=asn1_DEFAULT},
	      #'SeqDef1Imp'{bool1=true,int1=15,seq1=#'SeqIn'{}}),

    roundtrip('SeqDef2Imp', #'SeqDef2Imp'{seq2=#'SeqIn'{boolIn=true,intIn=66},
					  bool2=true,int2=15}),
    roundtrip('SeqDef2Imp',
	      #'SeqDef2Imp'{seq2=asn1_DEFAULT,bool2=asn1_DEFAULT,int2=15},
	      #'SeqDef2Imp'{seq2=#'SeqIn'{},bool2=true,int2=15}),

    roundtrip('SeqDef3Imp',
	      #'SeqDef3Imp'{bool3=true,seq3=#'SeqIn'{boolIn=true,intIn=66},int3=15}),
    roundtrip('SeqDef3Imp',
	      #'SeqDef3Imp'{bool3=asn1_DEFAULT,seq3=asn1_DEFAULT,int3=15},
	      #'SeqDef3Imp'{bool3=true,seq3=#'SeqIn'{},int3=15}),

    roundtrip('SeqDef1Exp',
	      #'SeqDef1Exp'{bool1=true,int1=15,seq1=#'SeqIn'{boolIn=true,intIn=66}}),
    roundtrip('SeqDef1Exp',
	      #'SeqDef1Exp'{bool1=asn1_DEFAULT,int1=15,seq1=asn1_DEFAULT},
	      #'SeqDef1Exp'{bool1=true,int1=15,seq1=#'SeqIn'{}}),

    roundtrip('SeqDef2Exp', #'SeqDef2Exp'{seq2=#'SeqIn'{boolIn=true,intIn=66},
              bool2=true,int2=15}),
    roundtrip('SeqDef2Exp',
	      #'SeqDef2Exp'{seq2=asn1_DEFAULT,bool2=true,int2=15},
	      #'SeqDef2Exp'{seq2=#'SeqIn'{},bool2=true,int2=15}),

    roundtrip('SeqDef3Exp',
	      #'SeqDef3Exp'{bool3=true,seq3=#'SeqIn'{boolIn=true,intIn=66},int3=15}),
    roundtrip('SeqDef3Exp',
	      #'SeqDef3Exp'{bool3=asn1_DEFAULT,seq3=asn1_DEFAULT,int3=15},
	      #'SeqDef3Exp'{bool3=true,seq3=#'SeqIn'{},int3=15}),
    ok.

roundtrip(Type, Value) ->
    roundtrip(Type, Value, Value).

roundtrip(Type, Value, ExpectedValue) ->
    asn1_test_lib:roundtrip('SeqDefault', Type, Value, ExpectedValue).
