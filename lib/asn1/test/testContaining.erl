%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023. All Rights Reserved.
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
-module(testContaining).

-export([containing/1]).

containing(Rules) ->
    RoundTrip =
        case Rules of
            per_jer ->
                fun roundtrip_per_jer/2;
            _ ->
                fun roundtrip/2
        end,
    RoundTrip('Config', {'Config',42,true}),
    RoundTrip('Seq', {'Seq',17,<<"abc">>}),
    RoundTrip('Str', <<"xyz">>),
    RoundTrip('Other', <<"other">>),
    RoundTrip('NewSequence', <<1,2,3,4>>),

    ok.

roundtrip(Type, Value) ->
    asn1_test_lib:roundtrip('Containing', Type, Value).

roundtrip_per_jer(Type, Value) ->
    Mod = 'Containing',
    case Mod:jer_encode(Type, Value) of
        {ok,Encoded} ->
            {ok,Value} = Mod:jer_decode(Type, Encoded);
        Encoded when is_binary(Encoded) ->
            Value = Mod:jer_decode(Type, Encoded)
    end,
    Encoded.
