%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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
-module(testExtensionDefault).

-export([main/1]).

main(_Erule) ->
    roundtrip('Message', {'Message',1,low}),    %Will be explicitly encoded.
    roundtrip('Message', {'Message',1,high}),
    roundtrip('Message', {'Message',1,asn1_DEFAULT}, {'Message',1,low}),

    map_roundtrip('Message', #{id=>1,priority=>low}), %Will be explicitly encoded.
    map_roundtrip('Message', #{id=>1,priority=>high}),
    map_roundtrip('Message', #{id=>1}, #{id=>1,priority=>low}),
    ok.

roundtrip(Type, Value) ->
    asn1_test_lib:roundtrip('ExtensionDefault', Type, Value).

roundtrip(Type, Value, Expected) ->
    %% asn1_test_lib:roundtrip/3 will invoke map_roundtrip/3, which will
    %% not work in this case. Therefore, implement the roundtrip ourselves.
    M = 'ExtensionDefault',
    {ok,Enc} = M:encode(Type, Value),
    {ok,Expected} = M:decode(Type, Enc),
    ok.

map_roundtrip(Type, Value) ->
    map_roundtrip(Type, Value, Value).

map_roundtrip(Type, Value, Expected) ->
    M = 'maps_ExtensionDefault',
    Enc = M:encode(Type, Value),
    Expected = M:decode(Type, Enc),
    ok.
