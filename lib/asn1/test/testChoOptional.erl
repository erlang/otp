%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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
    roundtrip('Seq1', #'Seq1'{bool=true,int=asn1_NOVALUE,cho={vsCho,"Vs Str"}}),
    roundtrip('Seq1', #'Seq1'{bool=true,int=asn1_NOVALUE,cho={ocStrCho,"Oct Str"}}),
    roundtrip('Seq2', #'Seq2'{int=asn1_NOVALUE,cho=asn1_NOVALUE,bool=true}),
    roundtrip('Seq2', #'Seq2'{int=233,cho=asn1_NOVALUE,bool=true}),
    roundtrip('Seq2', #'Seq2'{int=asn1_NOVALUE,cho={vsCho,"Vs Str"},bool=true}),
    roundtrip('Seq2', #'Seq2'{int=asn1_NOVALUE,cho={ocStrCho,"Oct Str"},bool=true}),
    roundtrip('Seq3', #'Seq3'{cho=asn1_NOVALUE,int=asn1_NOVALUE,bool=true}),
    roundtrip('Seq3', #'Seq3'{cho=asn1_NOVALUE,int=233,bool=true}),
    roundtrip('Seq3', #'Seq3'{cho={vsCho,"Vs Str"},int=asn1_NOVALUE,bool=true}),
    roundtrip('Seq3', #'Seq3'{cho={ocStrCho,"Oct Str"},int=asn1_NOVALUE,bool=true}),
    ok.

roundtrip(Type, Value) ->
    roundtrip('ChoOptional', Type, Value),
    roundtrip('ChoOptionalImplicitTag', Type, Value).

roundtrip(Mod, Type, Value) ->
    asn1_test_lib:roundtrip(Mod, Type, Value).
