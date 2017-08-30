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
-module(testChoOptional).
-export([run/0]).

-record('Seq1', {bool, int = asn1_NOVALUE, cho = asn1_NOVALUE}).
-record('Seq2', {int = asn1_NOVALUE, cho = asn1_NOVALUE, bool}).
-record('Seq3', {cho = asn1_NOVALUE, int = asn1_NOVALUE, bool}).

run() ->
    roundtrip('Seq1', #'Seq1'{bool=true,int=asn1_NOVALUE,cho=asn1_NOVALUE}),
    roundtrip('Seq1', #'Seq1'{bool=true,int=233,cho=asn1_NOVALUE}),
    roundtrip('Seq1', #'Seq1'{bool=true,int=asn1_NOVALUE,
			      cho={vsCho,"Vs Str"}}),
    roundtrip('Seq1', #'Seq1'{bool=true,int=asn1_NOVALUE,
			      cho={ocStrCho,<<"Oct Str">>}}),
    roundtrip('Seq2', #'Seq2'{int=asn1_NOVALUE,cho=asn1_NOVALUE,bool=true}),
    roundtrip('Seq2', #'Seq2'{int=233,cho=asn1_NOVALUE,bool=true}),
    roundtrip('Seq2', #'Seq2'{int=asn1_NOVALUE,cho={vsCho,"Vs Str"},bool=true}),
    roundtrip('Seq2', #'Seq2'{int=asn1_NOVALUE,cho={ocStrCho,<<"Oct Str">>},
			      bool=true}),
    roundtrip('Seq3', #'Seq3'{cho=asn1_NOVALUE,int=asn1_NOVALUE,bool=true}),
    roundtrip('Seq3', #'Seq3'{cho=asn1_NOVALUE,int=233,bool=true}),
    roundtrip('Seq3', #'Seq3'{cho={vsCho,"Vs Str"},int=asn1_NOVALUE,bool=true}),
    roundtrip('Seq3', #'Seq3'{cho={ocStrCho,<<"Oct Str">>},
			      int=asn1_NOVALUE,bool=true}),
    ok.

roundtrip(Type, Value) ->
    roundtrip('ChoOptional', Type, Value),
    roundtrip('ChoOptionalImplicitTag', Type, Value).

roundtrip(Mod, Type, Value) ->
    asn1_test_lib:roundtrip(Mod, Type, Value).
