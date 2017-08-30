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
-module(testChoExternal).
-export([external/1]).

-include_lib("common_test/include/ct.hrl").
-include("External.hrl").

external(_Rules) ->
    roundtrip('ChoXCho', {boolCho,true}),
    roundtrip('ChoXCho', {intCho,77}),

    roundtrip('ChoXBool', {xbool,true}),
    roundtrip('ChoXBool', {xboolImp,true}),
    roundtrip('ChoXBool', {xboolExp,true}),

    roundtrip('NT', {os,<<"kalle">>}),
    roundtrip('Exp', {os,<<"kalle">>}),
    roundtrip('NTNT', {os,<<"kalle">>}),
    roundtrip('NTExp', {os,<<"kalle">>}),
    roundtrip('ExpNT', {os,<<"kalle">>}),
    roundtrip('ExpExp', {os,<<"kalle">>}),
    roundtrip('XNTNT', {os,<<"kalle">>}),
    roundtrip('XNTExp', {os,<<"kalle">>}),
    roundtrip('XExpNT', {os,<<"kalle">>}),
    roundtrip('XExpExp', {os,<<"kalle">>}),
    
    ok.

roundtrip(Type, Value) ->
    asn1_test_lib:roundtrip('ChoExternal', Type, Value).
