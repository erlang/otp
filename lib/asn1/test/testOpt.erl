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
-module(testOpt).
-export([main/1]).

-include_lib("common_test/include/ct.hrl").

-record('Opt1',{bool0, 
		bool1 = asn1_NOVALUE, 
		bool2 = asn1_NOVALUE, 
		bool3 = asn1_NOVALUE}).

-record('Opt2',{bool10, 
		bool11 = asn1_NOVALUE, 
		bool12 = asn1_NOVALUE, 
		bool13}).

-record('Opt3',{bool30 = asn1_NOVALUE, 
		bool31 = asn1_NOVALUE, 
		bool32 = asn1_NOVALUE, 
		bool33 = asn1_NOVALUE}).

main(_Rules) ->
    roundtrip('Opt1', #'Opt1'{bool0=true,bool1=true,bool2=true,bool3=true}),
    roundtrip('Opt1', #'Opt1'{bool0=true,bool1=asn1_NOVALUE,bool2=asn1_NOVALUE,
        bool3=asn1_NOVALUE}),
    roundtrip('Opt1', #'Opt1'{bool0=true,bool1=asn1_NOVALUE,bool2=false,bool3=asn1_NOVALUE}),
    roundtrip('Opt1', #'Opt1'{bool0=false,bool1=asn1_NOVALUE,bool2=asn1_NOVALUE,bool3=false}),

    roundtrip('Opt2', #'Opt2'{bool10=false,bool11=false,bool12=false,bool13=false}),
    roundtrip('Opt2', #'Opt2'{bool10=true,bool11=asn1_NOVALUE,bool12=asn1_NOVALUE,
        bool13=false}),
    roundtrip('Opt2', #'Opt2'{bool10=true,bool11=false,bool12=asn1_NOVALUE,bool13=false}),
    roundtrip('Opt2', #'Opt2'{bool10=false,bool11=asn1_NOVALUE,bool12=false,bool13=false}),

    roundtrip('Opt3', #'Opt3'{bool30=false,bool31=false,bool32=false,bool33=false}),
    roundtrip('Opt3', #'Opt3'{bool30=asn1_NOVALUE,bool31=asn1_NOVALUE,bool32=asn1_NOVALUE,
        bool33=asn1_NOVALUE}),
    roundtrip('Opt3', #'Opt3'{bool30=true,bool31=asn1_NOVALUE,bool32=asn1_NOVALUE,
        bool33=asn1_NOVALUE}),
    roundtrip('Opt3', #'Opt3'{bool30=asn1_NOVALUE,bool31=asn1_NOVALUE,bool32=false,
        bool33=asn1_NOVALUE}),
    roundtrip('Opt3', #'Opt3'{bool30=asn1_NOVALUE,bool31=asn1_NOVALUE,bool32=asn1_NOVALUE,
        bool33=false}),
    ok.

roundtrip(Type, Value) ->
    asn1_test_lib:roundtrip('Opt', Type, Value).
