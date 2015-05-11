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

-module(testImporting).
-export([main/0]).

main() ->
    M = 'Importing',
    roundtrip('Seq', {'Seq',5}),
    roundtrip('OtherSeq', {'Seq',42,true}),
    {'Seq',42,true} = M:seq(),
    roundtrip('ObjSeq', {'ObjSeq',1,<<"XYZ">>}),
    roundtrip('ObjSeq', {'ObjSeq',2,19}),
    ok.

roundtrip(Type, Value) ->
    asn1_test_lib:roundtrip('Importing', Type, Value).
