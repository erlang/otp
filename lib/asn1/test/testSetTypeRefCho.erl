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
-module(testSetTypeRefCho).

-export([main/1]).

-include_lib("test_server/include/test_server.hrl").
-include("External.hrl").

-record('SetTRcho',{setCho, setChoE, 'setCho-E', 'setChoE-E'}).


main(_Rules) ->
    roundtrip('SetTRcho',
	      #'SetTRcho'{'setCho' = {choOs,"A string 1"},
			  'setChoE' = {choOs,"A string 3"},
			  'setCho-E' = {choOs,"A string 7"},
			  'setChoE-E' = {choOs,"A string 9"}}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SetTypeRefCho', T, V).
