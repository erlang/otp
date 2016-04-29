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
-module(testChoTypeRefPrim).

-export([prim/1]).

-include_lib("common_test/include/ct.hrl").

prim(_Rules) ->
    roundtrip('ChoTR', {bool,true}),
    roundtrip('ChoTR', {octStr,<<11,12,13,14,15,16,17>>}),
    roundtrip('ChoTR', {int,233}),
    roundtrip('ChoTR', {octStr,<<"Stringing in the rain">>}),
    roundtrip('ChoTR2', {octStr,<<"A string">>}),
    roundtrip('ChoTR2', {octStrI,<<"A string">>}),
    roundtrip('ChoTR2', {octStrE,<<"A string">>}),
    roundtrip('ChoTR2', {'octStr-I',<<"A string">>}),
    roundtrip('ChoTR2', {'octStrI-I',<<"A string">>}),
    roundtrip('ChoTR2', {'octStrE-I',<<"A string">>}),
    roundtrip('ChoTR2', {'octStr-E',<<"A string">>}),
    roundtrip('ChoTR2', {'octStrI-E',<<"A string">>}),
    roundtrip('ChoTR2', {'octStrE-E',<<"A string">>}),
    ok.

roundtrip(Type, Value) ->
    asn1_test_lib:roundtrip('ChoTypeRefPrim', Type, Value).
