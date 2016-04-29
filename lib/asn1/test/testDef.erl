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
-module(testDef).

-export([main/1]).

-include_lib("common_test/include/ct.hrl").

-record('Def1',{bool0, 
		bool1 = asn1_DEFAULT, 
		bool2 = asn1_DEFAULT, 
		bool3 = asn1_DEFAULT}).
-record('Def2',{bool10, 
		bool11 = asn1_DEFAULT, 
		bool12 = asn1_DEFAULT, 
		bool13}).
-record('Def3',{bool30 = asn1_DEFAULT, 
		bool31 = asn1_DEFAULT, 
		bool32 = asn1_DEFAULT, 
		bool33 = asn1_DEFAULT}).

main(_Rules) ->
    roundtrip('Def1', #'Def1'{bool0=true,bool1=true,bool2=true,bool3=true}),
    roundtrip('Def1',
	      #'Def1'{bool0=true},
	      #'Def1'{bool0=true,bool1=false,bool2=false,bool3=false}),
    roundtrip('Def1',
	      #'Def1'{bool0=true,bool2=false},
	      #'Def1'{bool0=true,bool1=false,bool2=false,bool3=false}),
    roundtrip('Def1',
	      #'Def1'{bool0=false,bool3=false},
	      #'Def1'{bool0=false,bool1=false,bool2=false,bool3=false}),

    roundtrip('Def2', #'Def2'{bool10=false,bool11=false,bool12=false,bool13=false}),
    roundtrip('Def2',
	      #'Def2'{bool10=true,bool13=false},
	      #'Def2'{bool10=true,bool11=false,bool12=false,bool13=false}),
    roundtrip('Def2',
	      #'Def2'{bool10=true,bool11=false,bool13=false},
	      #'Def2'{bool10=true,bool11=false,bool12=false,bool13=false}),
    roundtrip('Def2',
	      #'Def2'{bool10=false,bool12=false,bool13=false},
	      #'Def2'{bool10=false,bool11=false,bool12=false,bool13=false}),

    roundtrip('Def3', #'Def3'{bool30=false,bool31=false,bool32=false,bool33=false}),
    roundtrip('Def3',
	      #'Def3'{},
	      #'Def3'{bool30=false,bool31=false,bool32=false,bool33=false}),
    roundtrip('Def3',
	      #'Def3'{bool30=true},
	      #'Def3'{bool30=true,bool31=false,bool32=false,bool33=false}),
    roundtrip('Def3',
	      #'Def3'{bool32=false},
	      #'Def3'{bool30=false,bool31=false,bool32=false,bool33=false}),
    roundtrip('Def3',
	      #'Def3'{bool33=false},
	      #'Def3'{bool30=false,bool31=false,bool32=false,bool33=false}),
    ok.

roundtrip(Type, Value) ->
    roundtrip(Type, Value, Value).

roundtrip(Type, Value, ExpectedValue) ->
    asn1_test_lib:roundtrip('Def', Type, Value, ExpectedValue).
