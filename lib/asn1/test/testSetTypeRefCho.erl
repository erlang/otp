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
-module(testSetTypeRefCho).

-export([main/1]).

-include_lib("common_test/include/ct.hrl").
-include("External.hrl").

-record('SetTRcho',{setCho, setChoE, 'setCho-E', 'setChoE-E'}).


main(_Rules) ->
    roundtrip('SetTRcho',
	      #'SetTRcho'{'setCho' = {choOs,<<"A string 1">>},
			  'setChoE' = {choOs,<<"A string 3">>},
			  'setCho-E' = {choOs,<<"A string 7">>},
			  'setChoE-E' = {choOs,<<"A string 9">>}}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SetTypeRefCho', T, V).
