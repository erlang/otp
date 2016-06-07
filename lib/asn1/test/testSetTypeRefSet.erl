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
-module(testSetTypeRefSet).

-export([main/1]).

-include_lib("common_test/include/ct.hrl").

-record('Set1',{bool1, int1, set1}).
-record('Set2',{set2, bool2, int2}).
-record('Set3',{bool3, set3, int3}).
-record('Set4',{set41, set42, set43}).
-record('SetIn',{boolIn, intIn}).
-record('SetS1',{boolS1, intS1, setS1}).
-record('SetS1_setS1',{boolIn, intIn}).
-record('SetS2',{setS2, boolS2, intS2}).
-record('SetS2_setS2',{boolIn, intIn}).
-record('SetS3',{boolS3, setS3, intS3}).
-record('SetS3_setS3',{boolIn, intIn}).
-record('SetSTag',{setS1, setS2, setS3}).
-record('SetSTag_setS1',{b1, i1}).
-record('SetSTag_setS2',{b2, i2}).
-record('SetSTag_setS3',{b3, i3}).
-record('SetTRset',{setSet, setSetI, setSetE, 'setSet-I', 'setSetI-I', 'setSetE-I', 'setSet-E', 'setSetI-E', 'setSetE-E'}).
-record('SetSet',{setInt, setOs}).
-record('SetSetImp',{setInt, setOs}).
-record('SetSetExp',{setInt, setOs}).



main(_Rules) ->
    roundtrip('Set1',
	      #'Set1'{bool1=true,int1=15,set1=#'SetIn'{boolIn=true,intIn=66}}),
    roundtrip('Set2',
	      #'Set2'{set2=#'SetIn'{boolIn=true,intIn=66},bool2=true,int2=15}),
    roundtrip('Set3',
	      #'Set3'{bool3=true,set3=#'SetIn'{boolIn=true,intIn=66},int3=15}),
    roundtrip('Set4',
	      #'Set4'{set41=#'SetIn'{boolIn=true,intIn=66},
		      set42=#'SetIn'{boolIn=true,intIn=66},
		      set43=#'SetIn'{boolIn=true,intIn=66}}),
    roundtrip('SetS1',
	      #'SetS1'{boolS1=true,intS1=15,
		       setS1=#'SetS1_setS1'{boolIn=true,intIn=66}}),
    roundtrip('SetS2',
	      #'SetS2'{setS2=#'SetS2_setS2'{boolIn=true,intIn=66},
		       boolS2=true,intS2=15}),
    roundtrip('SetS3',
	      #'SetS3'{boolS3=true,
		       setS3=#'SetS3_setS3'{boolIn=true,intIn=66},
		       intS3=15}),
    roundtrip('SetSTag',
	      #'SetSTag'{setS1=#'SetSTag_setS1'{b1=true,i1=11},
			 setS2=#'SetSTag_setS2'{b2=true,i2=22},
			 setS3=#'SetSTag_setS3'{b3=true,i3=33}}),
    roundtrip('SetTRset',
	      #'SetTRset'{setSet=#'SetSet'{setInt=2,setOs = <<"A1">>},
			  setSetI=#'SetSet'{setInt=2,setOs = <<"A2">>},
			  setSetE=#'SetSet'{setInt=2,setOs = <<"A3">>},
			  'setSet-I'=#'SetSetImp'{setInt=2,setOs = <<"A4">>},
			  'setSetI-I'=#'SetSetImp'{setInt=2,setOs = <<"A5">>},
			  'setSetE-I'=#'SetSetImp'{setInt=2,setOs = <<"A6">>},
			  'setSet-E'=#'SetSetExp'{setInt=2,setOs = <<"A7">>},
			  'setSetI-E'=#'SetSetExp'{setInt=2,setOs = <<"A8">>},
			  'setSetE-E'=#'SetSetExp'{setInt=2,setOs = <<"A9">>}}),
    
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SetTypeRefSet', T, V).
