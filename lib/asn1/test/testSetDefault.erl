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
-module(testSetDefault).

-export([main/1]).

-include_lib("common_test/include/ct.hrl").

-record('SetDef1',{bool1 = asn1_DEFAULT, int1, set1 = asn1_DEFAULT}).
-record('SetDef2',{set2 = asn1_DEFAULT, bool2, int2}).
-record('SetDef3',{bool3 = asn1_DEFAULT, set3 = asn1_DEFAULT, int3 = asn1_DEFAULT}).
-record('SetIn', {boolIn = asn1_NOVALUE, intIn = 12}).


main(_Rules) ->
    roundtrip('SetDef1',
	      #'SetDef1'{bool1=true,int1=15,
			 set1=#'SetIn'{boolIn=true,intIn=66}}),
    roundtrip('SetDef1',
	      #'SetDef1'{bool1=asn1_DEFAULT,int1=15,set1=asn1_DEFAULT},
	      #'SetDef1'{bool1=true,int1=15,set1=#'SetIn'{}}),

    roundtrip('SetDef2',
	      #'SetDef2'{set2=#'SetIn'{boolIn=true,intIn=66},
			 bool2=true,int2=15}),
    roundtrip('SetDef2',
	      #'SetDef2'{set2=asn1_DEFAULT,bool2=true,int2=15},
	      #'SetDef2'{set2=#'SetIn'{},bool2=true,int2=15}),

    roundtrip('SetDef3',
	      #'SetDef3'{bool3=true,set3=#'SetIn'{boolIn=true,intIn=66},
			 int3=15}),
    roundtrip('SetDef3',
	      #'SetDef3'{bool3=asn1_DEFAULT,set3=asn1_DEFAULT,int3=15},
	      #'SetDef3'{bool3=true,set3=#'SetIn'{},int3=15}),
    ok.

roundtrip(Type, Value) ->
    roundtrip(Type, Value, Value).

roundtrip(Type, Value, ExpectedValue) ->
    asn1_test_lib:roundtrip('SetDefault', Type, Value, ExpectedValue).
