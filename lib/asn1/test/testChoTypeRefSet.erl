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
-module(testChoTypeRefSet).

-export([set/1]).

-include_lib("common_test/include/ct.hrl").

-record('ChoSet', {setInt, setOs}).
-record('ChoSetImp', {setInt, setOs}).
-record('ChoSetExp', {setInt, setOs}).

set(_Rules) ->
    roundtrip('ChoTRset', {choSet,#'ChoSet'{setInt=88,
					    setOs = <<"A string">>}}),
    roundtrip('ChoTRset', {choSetI,#'ChoSet'{setInt=88,
					     setOs = <<"A string">>}}),
    roundtrip('ChoTRset', {choSetE,#'ChoSet'{setInt=88,
					     setOs = <<"A string">>}}),
    roundtrip('ChoTRset', {'choSet-I',#'ChoSetImp'{setInt=88,
						   setOs = <<"A string">>}}),
    roundtrip('ChoTRset', {'choSetI-I',#'ChoSetImp'{setInt=88,
						    setOs = <<"A string">>}}),
    roundtrip('ChoTRset', {'choSetE-I',#'ChoSetImp'{setInt=88,
						    setOs = <<"A string">>}}),
    roundtrip('ChoTRset', {'choSet-E',#'ChoSetExp'{setInt=88,
						   setOs = <<"A string">>}}),
    roundtrip('ChoTRset', {'choSetI-E',#'ChoSetExp'{setInt=88,
						    setOs = <<"A string">>}}),
    roundtrip('ChoTRset', {'choSetE-E',#'ChoSetExp'{setInt=88,
						    setOs = <<"A string">>}}),
    ok.

roundtrip(Type, Value) ->
    asn1_test_lib:roundtrip('ChoTypeRefSet', Type, Value).
