%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
-module(testSelectionTypes).
-export([test/0]).

-include_lib("common_test/include/ct.hrl").

test() ->
    ["Es"] = Val2 = ['SelectionType':einsteinium()],
    roundtrip('MendeleyevTable', ["fox","tree","cat","stone"]),
    roundtrip('MendeleyevTable', Val2),
    roundtrip('MendeleyevSet', [42,57,93,101]),

    M = 'SelectionType',
    true = M:boolv(),
    4 = M:intv(),
    <<2#1001:4>> = M:bsv(),
    <<16#3130:16>> = M:osv(),
    'NULL' = M:nullv(),
    {2,1,1} = M:oiv(),
    "ObjectDesc" = M:odv(),
    "utf8" = M:utfv(),
    {5,32767,256} = M:rov(),
    "089" = M:numsv(),
    "telet" = M:teletv(),
    "t61" = M:t61v(),
    "video" = M:videov(),
    "ia5" = M:ia5v(),
    "9805281429Z" = M:utctimev(),
    "19980528142905.1" = M:gTime(),
    "graphic" = M:gsv(),
    "visible" = M:vsv(),
    "general" = M:gStringv(),
    "Universal" = M:univv(),
    "bmp" = M:bmov(),

    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SelectionType', T, V).
