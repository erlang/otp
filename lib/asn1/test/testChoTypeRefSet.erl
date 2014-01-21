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
-module(testChoTypeRefSet).

-export([set/1]).

-include_lib("test_server/include/test_server.hrl").

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
