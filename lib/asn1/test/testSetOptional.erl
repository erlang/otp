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
-module(testSetOptional).

-include("External.hrl").
-export([main/1]).
-export([ticket_7533/1]).

-record('SetOpt1',{bool1 = asn1_NOVALUE, int1, set1 = asn1_NOVALUE}).
-record('SetOpt1Imp',{bool1 = asn1_NOVALUE, int1, set1 = asn1_NOVALUE}).
-record('SetOpt1Exp',{bool1 = asn1_NOVALUE, int1, set1 = asn1_NOVALUE}).
-record('SetOpt2',{set2 = asn1_NOVALUE, bool2, int2}).
-record('SetOpt2Imp',{set2 = asn1_NOVALUE, bool2, int2}).
-record('SetOpt2Exp',{set2 = asn1_NOVALUE, bool2, int2}).
-record('SetOpt3',{bool3 = asn1_NOVALUE, set3 = asn1_NOVALUE, int3 = asn1_NOVALUE}).
-record('SetOpt3Imp',{bool3 = asn1_NOVALUE, set3 = asn1_NOVALUE, int3 = asn1_NOVALUE}).
-record('SetOpt3Exp',{bool3 = asn1_NOVALUE, set3 = asn1_NOVALUE, int3 = asn1_NOVALUE}).
-record('SetIn',{boolIn, intIn}).

main(_Rules) ->
    roundtrip('SetOpt1',
	      #'SetOpt1'{bool1=true,int1=15,
			 set1=#'SetIn'{boolIn=true,intIn=66}}),
    roundtrip('SetOpt1', #'SetOpt1'{int1=15}),

    roundtrip('SetOpt2', #'SetOpt2'{bool2=true,int2=15,
				    set2=#'SetIn'{boolIn=true,intIn=66}}),
    roundtrip('SetOpt2', #'SetOpt2'{int2=15,bool2=true}),

    roundtrip('SetOpt3', #'SetOpt3'{bool3=true,int3=15,
				    set3=#'SetIn'{boolIn=true,intIn=66}}),
    roundtrip('SetOpt3', #'SetOpt3'{int3=15}),

    roundtrip('SetOpt1Imp',
	      #'SetOpt1Imp'{bool1=true,int1 = 15,
			    set1=#'SetIn'{boolIn = true,intIn = 66}}),
    roundtrip('SetOpt1Imp', #'SetOpt1Imp'{int1=15}),
    

    roundtrip('SetOpt2Imp',
	      #'SetOpt2Imp'{bool2=true,int2=15,
			    set2=#'SetIn'{boolIn=true,intIn=66}}),
    roundtrip('SetOpt2Imp',#'SetOpt2Imp'{int2=15,bool2=true}),
    

    roundtrip('SetOpt3Imp',
	      #'SetOpt3Imp'{bool3=true,int3=15,
			    set3=#'SetIn'{boolIn=true,intIn=66}}),
    roundtrip('SetOpt3Imp', #'SetOpt3Imp'{int3=15}),

    roundtrip('SetOpt1Exp',
	      #'SetOpt1Exp'{bool1=true,int1=15,
			    set1=#'SetIn'{boolIn=true,intIn=66}}),
    roundtrip('SetOpt1Exp', #'SetOpt1Exp'{int1=15}),

    roundtrip('SetOpt2Exp',
	      #'SetOpt2Exp'{bool2=true,int2=15,
			    set2=#'SetIn'{boolIn=true,intIn=66}}),
    roundtrip('SetOpt2Exp', #'SetOpt2Exp'{int2=15,bool2=true}),
    
    roundtrip('SetOpt3Exp',
	      #'SetOpt3Exp'{bool3=true,int3=15,
			    set3=#'SetIn'{boolIn=true,intIn=66}}),
    roundtrip('SetOpt3Exp', #'SetOpt3Exp'{int3=15}),
    
    ok.


ticket_7533(Ber) when Ber == ber ->
    Val = #'SetOpt1'{bool1=true,int1=12,set1=#'SetIn'{boolIn=false,intIn=13}},
    roundtrip('SetOpt1', Val),
    CorruptVal = <<49,14,1,1,255,2,1,12,0:8/unit:8>>,
    {error,_} = 'SetOptional':decode('SetOpt1', CorruptVal),
    ok;
ticket_7533(_) ->
    ok.

roundtrip(Type, Value) ->
    asn1_test_lib:roundtrip('SetOptional', Type, Value).
