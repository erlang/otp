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
-module(testChoExtension).

-export([extension/1]).

-include_lib("common_test/include/ct.hrl").


extension(_Rules) ->
    roundtrip('ChoExt1', {bool,true}),
    roundtrip('ChoExt1', {int,33}),
    {int, 1} = 'ChoExtension':choExt1(),

    %% A trick to encode with another compatible CHOICE type to test reception
    %% extension alternative

    roundtrip('ChoExt1x', {str,<<"abc">>}),

    roundtrip('ChoExt2', {bool,true}),
    roundtrip('ChoExt2', {int,33}),
    roundtrip('ChoExt3', {bool,true}),
    roundtrip('ChoExt3', {int,33}),
    roundtrip('ChoExt4', {str,<<"abc">>}),

    ok.


roundtrip(Type, Value) ->
    asn1_test_lib:roundtrip('ChoExtension', Type, Value).
