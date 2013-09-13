%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%
-module(testChoPrim).

-export([bool/1]).
-export([int/1]).

-include_lib("test_server/include/test_server.hrl").

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
