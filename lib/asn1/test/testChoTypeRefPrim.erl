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
-module(testChoTypeRefPrim).

-export([prim/1]).

-include_lib("test_server/include/test_server.hrl").

prim(_Rules) ->
    roundtrip('ChoTR', {bool,true}),
    roundtrip('ChoTR', {octStr,<<11,12,13,14,15,16,17>>}),
    roundtrip('ChoTR', {int,233}),
    roundtrip('ChoTR', {octStr,<<"Stringing in the rain">>}),
    roundtrip('ChoTR2', {octStr,<<"A string">>}),
    roundtrip('ChoTR2', {octStrI,<<"A string">>}),
    roundtrip('ChoTR2', {octStrE,<<"A string">>}),
    roundtrip('ChoTR2', {'octStr-I',<<"A string">>}),
    roundtrip('ChoTR2', {'octStrI-I',<<"A string">>}),
    roundtrip('ChoTR2', {'octStrE-I',<<"A string">>}),
    roundtrip('ChoTR2', {'octStr-E',<<"A string">>}),
    roundtrip('ChoTR2', {'octStrI-E',<<"A string">>}),
    roundtrip('ChoTR2', {'octStrE-E',<<"A string">>}),
    ok.

roundtrip(Type, Value) ->
    asn1_test_lib:roundtrip('ChoTypeRefPrim', Type, Value).
