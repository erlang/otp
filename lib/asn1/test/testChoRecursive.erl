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
-module(testChoRecursive).


-export([recursive/1]).

-include_lib("common_test/include/ct.hrl").

-record('ChoRec_something',{a, b, c}).
-record('ChoRec2_something',{a, b, c}).

recursive(_Rules) ->
    roundtrip('ChoRec',
	      {something,
	       #'ChoRec_something'{a = 77,
				   b = <<"some octets here">>,
				   c = {nothing,'NULL'}}}),
    roundtrip('ChoRec', {nothing,'NULL'}),
    roundtrip('ChoRec2',
	      {something,
	       #'ChoRec2_something'{a = 77,
				    b = <<"some octets here">>,
				    c = {nothing,'NULL'}}}),
    roundtrip('ChoRec2', {nothing,'NULL'}),
    ok.

roundtrip(Type, Value) ->
    asn1_test_lib:roundtrip('ChoRecursive', Type, Value).
