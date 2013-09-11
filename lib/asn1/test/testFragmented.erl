%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
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
-module(testFragmented).

-export([main/1]).

main(_Erule) ->
    roundtrip('PDU', {'PDU',1,false,["abc","def"]}),
    B256 = lists:seq(0, 255),
    K1 = lists:duplicate(4, B256),
    K8 = binary_to_list(iolist_to_binary(lists:duplicate(8, K1))),
    roundtrip('PDU', {'PDU',1,false,[K8,K8]}),
    roundtrip('PDU', {'PDU',1,false,[K8,K8,K8,K8]}),
    roundtrip('PDU', {'PDU',1,false,[K8,K8,K8,K8,K8,K8]}),
    roundtrip('PDU', {'PDU',1,false,[K8,K8,K8,K8,K8,K8,K8,K8]}),
    roundtrip('PDU', {'PDU',1,false,[K8,K8,K8,K8,K8,K8,K8,K8,
				     K8,K8,K8,K8,K8,K8]}),
    roundtrip('PDU', {'PDU',1,false,[K8,K8,K8,K8,K8,K8,K8,K8,
				     K8,K8,K8,K8,K8,K8,K8,K8]}),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('Fragmented', T, V).
