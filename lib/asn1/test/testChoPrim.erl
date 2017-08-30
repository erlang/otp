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
-module(testChoPrim).

-export([bool/1]).
-export([int/1]).

-include_lib("common_test/include/ct.hrl").

bool(Rules) ->
    roundtrip('ChoCon', {bool0,true}),
    roundtrip('ChoCon', {bool1,true}),
    roundtrip('ChoCon', {int2,233}),
    case Rules of
	ber ->
	    {error,{asn1,{invalid_choice_type,wrong}}} =
		(catch 'ChoPrim':encode('ChoCon', {wrong,233})),
	    {error,{asn1,{invalid_choice_tag,_WrongTag}}} =
		(catch 'ChoPrim':decode('ChoCon', <<131,2,0,233>>));
	per ->
	    ok;
	uper ->
	    ok
    end,
    ok.

int(Rules) ->
    roundtrip('ChoExp', {int10,1}, {int10,first}),
    roundtrip('ChoExp', {int10,first}),
    roundtrip('ChoExp', {int10,last}),
    roundtrip('ChoExp', {bool11,true}),
    roundtrip('ChoExp', {enum12,one}),
    roundtrip('ChoExp', {bool11,true}),

    {error,{asn1,_}} = (catch 'ChoPrim':encode('ChoExp', {enum12,four})),
    {error,{asn1,_}} = (catch 'ChoPrim':encode('ChoExp', {wrong,233})),
    case Rules of
	ber ->
	    {error,{asn1,_}} = (catch 'ChoPrim':decode('ChoExp', <<107,3,2,1,1>>));
	per ->
	    ok;
	uper ->
	    ok
    end,
    ok.

roundtrip(Type, Value) ->
    roundtrip(Type, Value, Value).

roundtrip(Type, Value, ExpectedValue) ->
    asn1_test_lib:roundtrip('ChoPrim', Type, Value, ExpectedValue).
