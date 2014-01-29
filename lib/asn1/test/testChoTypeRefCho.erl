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
-module(testChoTypeRefCho).

-export([choice/1]).

-include_lib("test_server/include/test_server.hrl").

choice(_Rules) ->
    roundtrip('ChoTRcho', {choCho,{choInt,88}}),
    roundtrip('ChoTRcho', {choChoE,{choInt,88}}),
    roundtrip('ChoTRcho', {'choCho-E',{choInt,88}}),
    roundtrip('ChoTRcho', {'choChoE-E',{choInt,88}}),
    roundtrip('ChoChoInline', {bool1,true}),
    roundtrip('ChoChoInline', {choCho,{bool,true}}),
    roundtrip('ChoChoInline', {choCho,{octStr,<<"kk">>}}),
    roundtrip('ChoChoInline', {choCho,{int,55}}),
    ok.

roundtrip(Type, Value) ->
    asn1_test_lib:roundtrip('ChoTypeRefCho', Type, Value).
