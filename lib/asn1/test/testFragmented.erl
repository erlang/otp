%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2022. All Rights Reserved.
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

main(jer) -> ok;
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

    K16 = 16384,
    K64 = 4 * K16,
    K144 = 2 * K64 + K16,
    roundtrips([1, 2, 3, 17,
                K16-1, K16, K16+1,
                2*K16-1, 2*K16, 2*K16+1,
                3*K16-1, 3*K16, 3*K16+1,
                K64-1, K64, K64+1,
                K64+K16,
                K144-1, K144, K144+1]),
    ok.

roundtrips([Size|Sizes]) ->
    L = make_seq(Size, []),
    io:format("~p: ~P\n", [Size,L,6]),
    roundtrip('IntBoolSeqsU', L),
    if
        Size =< 65536 ->
            roundtrip('IntBoolSeqs', L);
        true ->
            ok
    end,
    roundtrips(Sizes);
roundtrips([]) ->
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('Fragmented', T, V).

make_seq(0, Acc) ->
    Acc;
make_seq(N, Acc) ->
    make_seq(N - 1, [{'IntBoolSeq',N,N rem 7 =:= 0}|Acc]).
