%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2017. All Rights Reserved.
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

-module(crypto_ng_api).

-compile(export_all).

-proptest(eqc).
-proptest([triq,proper]).

-ifndef(EQC).
-ifndef(PROPER).
-ifndef(TRIQ).
%%-define(EQC,true).
-define(PROPER,true).
%%-define(TRIQ,true).
-endif.
-endif.
-endif.

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-define(MOD_eqc,eqc).

-else.
-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-define(MOD_eqc,proper).
-else.
-ifdef(TRIQ).
-define(MOD_eqc,triq).
-include_lib("triq/include/triq.hrl").

-endif.
-endif.
-endif.


%%% Properties:

prop__crypto_one_time() ->
    numtests(10000,
             ?FORALL({TextPlain, Cipher, Key, IV}, ?LET(Ciph,cipher(),
                                                        {text_plain(), Ciph, key(Ciph), iv(Ciph)}),
                     equal(TextPlain,
                           full_blocks(TextPlain, Cipher),
                           decrypt_encrypt_one_time(Cipher, Key, IV, TextPlain))
                    )
            ).

%%% Generators
text_plain() -> iolist().

cipher() -> oneof( non_aead_ciphers() -- [aes_ige256] ).

key(Cipher) ->
    %% Can't be shrinked
    crypto:strong_rand_bytes( key_length(Cipher) ).
    
iv(Cipher) ->
    %% Can't be shrinked
    crypto:strong_rand_bytes( iv_length(Cipher) ).

iolist() -> oneof([list( oneof([list(byte()), binary(), list(binary())])),
                   binary(1056)
                  ]).
    
%%% Lib

equal(_, T, T) -> true;
equal(F, Tp, Td) ->
    ct:pal("Full:  ~p~n"
           "Block: ~p~n"
           "Decr:  ~p~n",
           [F, Tp, Td]),
    false.


non_aead_ciphers() ->
    [C || C <- crypto:supports(ciphers),
          C =/= chacha20_poly1305,
          begin
              #{mode := Mode} = crypto:cipher_info(C),
              not lists:member(Mode, [ccm_mode, gcm_mode])
          end].

decrypt_encrypt_one_time(Cipher, Key, IV, TextPlain) ->
    TextCrypto = crypto:crypto_one_time(Cipher, Key, IV, TextPlain, true),
    crypto:crypto_one_time(Cipher, Key, IV, TextCrypto, false).

    
full_blocks(TextPlain, Cipher) ->
    TextPlainBin = iolist_to_binary(TextPlain),
    {Head,_Tail} = split_binary(TextPlainBin, (size(TextPlainBin) - num_rest_bytes(TextPlainBin,Cipher))),
    Head.

num_rest_bytes(Bin, Cipher) -> size(Bin) rem block_size(Cipher).

block_size(Cipher) -> maps:get(block_size, crypto:cipher_info(Cipher)).

key_length(Cipher) -> maps:get(key_length, crypto:cipher_info(Cipher)).

iv_length(Cipher)  -> maps:get(iv_length, crypto:cipher_info(Cipher)).
