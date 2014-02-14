%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2013. All Rights Reserved.
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
-module(testTypeValueNotation).

-export([main/2]).

-record('Seq', {octstr, int, bool, enum, bitstr, null, oid, vstr}).

main(_Rule, _Option) ->
    Value = #'Seq'{octstr = <<1,2,3,4>>,
		   int = 12,
		   bool = true,
		   enum = a,
		   bitstr = <<2#1010:4>>,
		   null = 'NULL',
		   oid = {1, 2, 55},
		   vstr = "Hello World"},
    roundtrip('Seq', Value).

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('SeqTypeRefPrim', T, V).
