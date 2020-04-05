%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2019. All Rights Reserved.
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
bit_is_set(<<1:1,_/bitstring>>, 0) ->
    true;
bit_is_set(BitField, N) ->
    case BitField of
	<<_:N,1:1,_/bitstring>> ->
	    true;
	_ ->
	    false
    end.


set_bits(BitField, []) ->
    BitField;
set_bits(BitField, [H|T]) ->
    set_bits(set_bit(BitField, H), T).


set_bit(BitField, 0) ->
    <<_:1,Rest/bitstring>> = BitField,
    <<1:1,Rest/bitstring>>;
set_bit(BitField, B) ->
    <<Front:B,_:1,Rest/bitstring>>  = BitField,
    <<Front:B,1:1,Rest/bitstring>>.


%% Kirsch-Mitzenmacher-Optimization
hash(Elem, K, M) ->
    hash(Elem, K, M, []).
%%
hash(_, 0, _, Acc) ->
    Acc;
hash(Elem, K, M, Acc) ->
    H = (erlang:phash2({Elem, 0}, M) + (K - 1) * erlang:phash2({Elem, 1}, M)) rem M,
    hash(Elem, K - 1, M, [H|Acc]).
