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
-module(testSetOfCho).

-export([main/1]).

-include_lib("common_test/include/ct.hrl").

-record('SetChoDef',{bool1, int1, set1 = asn1_DEFAULT}).
-record('SetChoOpt',{bool1, int1, set1 = asn1_NOVALUE}).
-record('SetChoEmbDef',{bool1, int1, set1 = asn1_DEFAULT}).
-record('SetChoEmbOpt',{bool1, int1, set1 = asn1_NOVALUE}).
-record('SetOfChoEmbDef_SETOF',{bool1, int1, set1 = asn1_DEFAULT}).
-record('SetOfChoEmbOpt_SETOF',{bool1, int1, set1 = asn1_NOVALUE}).

main(_Rules) ->
    roundtrip('SetChoDef',
	      #'SetChoDef'{bool1=true,int1=17,set1=asn1_DEFAULT},
	      #'SetChoDef'{bool1=true,int1=17,set1=[]}),
    roundtrip('SetChoDef',
	      #'SetChoDef'{bool1=true,int1=17,set1=[{boolIn,true},{intIn,25}]}),
    roundtrip('SetChoOpt',
	      #'SetChoOpt'{bool1=true,int1=17,set1=asn1_NOVALUE}),
    roundtrip('SetChoOpt',
	      #'SetChoOpt'{bool1=true,int1=17,set1=[{boolIn,true},{intIn,25}]}),

    roundtrip('SetChoEmbDef',
	      #'SetChoEmbDef'{bool1=true,int1=17,set1=asn1_DEFAULT},
	      #'SetChoEmbDef'{bool1=true,int1=17,set1=[]}),
    roundtrip('SetChoEmbDef',
	      #'SetChoEmbDef'{bool1=true,int1=17,
			      set1=[{boolIn,true},{intIn,25}]}),
    roundtrip('SetChoEmbOpt',
	      #'SetChoEmbOpt'{bool1=true,int1=17,set1=asn1_NOVALUE}),
    roundtrip('SetChoEmbOpt',
	      #'SetChoEmbOpt'{bool1=true,int1=17,
			      set1=[{boolIn,true},{intIn,25}]}),

    roundtrip('SetOfChoEmbDef',
	      [#'SetOfChoEmbDef_SETOF'{bool1=true,int1=17,set1=asn1_DEFAULT}],
	      [#'SetOfChoEmbDef_SETOF'{bool1=true,int1=17,set1=[]}]),
    roundtrip('SetOfChoEmbDef',
	      [#'SetOfChoEmbDef_SETOF'{bool1=true,int1=17,
				       set1=[{boolIn,true},{intIn,25}]}]),

    roundtrip('SetOfChoEmbOpt',
	      [#'SetOfChoEmbOpt_SETOF'{bool1=true,int1=17,set1=asn1_NOVALUE}]),
    roundtrip('SetOfChoEmbOpt',
	      [#'SetOfChoEmbOpt_SETOF'{bool1=true,int1=17,
				       set1=[{boolIn,true},{intIn,25}]}]),

    ok.

roundtrip(T, V) ->
    roundtrip(T, V, V).

roundtrip(Type, Value, ExpectedValue) ->
    asn1_test_lib:roundtrip('SetOfCho', Type, Value, ExpectedValue).
