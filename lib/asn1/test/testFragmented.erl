%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
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
-module(testFragmented).

-export([main/1]).

main(_Erule) ->
    roundtrip('PDU', {'PDU',1,false,[<<"abc">>,<<"def">>]}),
    B256 = lists:seq(0, 255),
    K1 = lists:duplicate(4, B256),
    K8 = iolist_to_binary(lists:duplicate(8, K1)),
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
