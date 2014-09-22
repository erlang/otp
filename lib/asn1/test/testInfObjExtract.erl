%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014. All Rights Reserved.
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

-module(testInfObjExtract).

-export([main/0]).

main() ->
    roundtrip('DataSeq-1', {'DataSeq-1',1,true}),
    roundtrip('DataSeq-1', {'DataSeq-1',2,<<"abc">>}),
    roundtrip('DataSeq-1', {'DataSeq-1',3,<<42:5>>}),
    roundtrip_error('DataSeq-1', {'DataSeq-1',4,42}),

    roundtrip('DataSeq-2', {'DataSeq-2',1,true}),
    roundtrip_error('DataSeq-2', {'DataSeq',2,<<"abc">>}),
    roundtrip_error('DataSeq-2', {'DataSeq',3,<<42:5>>}),
    roundtrip_error('DataSeq-2', {'DataSeq',999,42}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('InfObjExtract', T, V).

roundtrip_error(T, V) ->
    try asn1_test_lib:roundtrip('InfObjExtract', T, V) of
	ok ->
	    test_server:fail()
    catch
	_:_ ->
	    ok
    end.
