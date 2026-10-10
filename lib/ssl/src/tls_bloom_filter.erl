%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2007-2026. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose: Bloom Filter implementation for anti-replay protection
%%          in TLS 1.3 (stateless tickets)
%%----------------------------------------------------------------------

-module(tls_bloom_filter).
-moduledoc false.

-export([add_elem/2,
         contains/2,
         new/2,
         rotate/1]).

%%--------------------------------------------------------------------
%% API ---------------------------------------------------------------
%%--------------------------------------------------------------------

%% Create new Bloom Filter with k hashes, m bits in the filter
new(K, M) ->
    Size = round(math:ceil(M / 8)),
    BitField = binary:copy(<<0>>, Size),
    #{k => K,
      m => M,
      current => BitField,
      old => BitField
     }.


%% Add new element to Bloom Filter
add_elem(#{k := K,
           m := M,
           current := BitField0} = BloomFilter,
         Elem) ->
    Hash = hash(Elem, K, M),
    BitField = set_bits(BitField0, Hash),
    BloomFilter#{current => BitField}.


%% Check if Bloom Filter contains element.
contains(#{k := K,
           m := M,
           current := BFCurrent,
           old := BFOld},
         Elem) ->
    Hash = hash(Elem, K, M),
    lists:all(fun (Pos) -> bit_is_set(BFCurrent, Pos) end, Hash) orelse
        lists:all(fun (Pos) -> bit_is_set(BFOld, Pos) end, Hash).


rotate(#{m := M,
         current := BFCurrent} = BloomFilter) ->
    Size = round(math:ceil(M / 8)),
    BFNew = binary:copy(<<0>>, Size),
    BloomFilter#{current := BFNew,
                 old := BFCurrent}.


%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------

%% Kirsch-Mitzenmacher-Optimization
%% Compute the two base hashes once, derive K positions arithmetically.
hash(Elem, K, M) ->
    H1 = erlang:phash2({Elem, 0}, M),
    H2 = erlang:phash2({Elem, 1}, M),
    hash(H1, H2, K, M, []).

hash(_, _, 0, _, Acc) ->
    Acc;
hash(H1, H2, K, M, Acc) ->
    H = (H1 + (K - 1) * H2) rem M,
    hash(H1, H2, K - 1, M, [H | Acc]).

%% Convert bit position to {ByteOffset, BitWithinByte} and operate on
%% whole bytes — avoids bignum creation from bit-offset binary matching.
bit_is_set(BitField, N) ->
    ByteOffset = N bsr 3,          %% N div 8
    BitOffset = 7 - (N band 7),    %% bit 0 = MSB of byte (matching original semantics)
    <<_:ByteOffset/binary, Byte:8, _/binary>> = BitField,
    (Byte bsr BitOffset) band 1 =:= 1.

set_bits(BitField, []) ->
    BitField;
set_bits(BitField, [H | T]) ->
    set_bits(set_bit(BitField, H), T).

set_bit(BitField, N) ->
    ByteOffset = N bsr 3,
    BitOffset = 7 - (N band 7),
    <<Front:ByteOffset/binary, Byte:8, Rest/binary>> = BitField,
    <<Front/binary, (Byte bor (1 bsl BitOffset)):8, Rest/binary>>.
