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

-module(crypto_prop_generators).

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

%%%================================================================
%%% Generators

text_plain() -> iolist().

cipher() -> oneof( non_aead_ciphers() -- [aes_ige256] ).

key(Cipher) ->
    %% Can't be shrinked
    crypto:strong_rand_bytes( key_length(Cipher) ).
    
iv(Cipher) ->
    %% Can't be shrinked
    crypto:strong_rand_bytes( iv_length(Cipher) ).

iolist() -> frequency([{5, list( oneof([list(byte()),
                                        binary(),
                                        list(binary())]))},
                       {1, mybinary(50000)}
                      ]).

mybinary(MaxSize) -> ?LET(Sz, integer(0,MaxSize), binary(Sz)).


%%%================================================================
non_aead_ciphers() ->
    [C || C <- crypto:supports(ciphers),
          C =/= chacha20_poly1305,
          begin
              #{mode := Mode} = crypto:cipher_info(C),
              not lists:member(Mode, [ccm_mode, gcm_mode])
          end].

block_size(Cipher) -> maps:get(block_size, crypto:cipher_info(Cipher)).

key_length(Cipher) -> maps:get(key_length, crypto:cipher_info(Cipher)).

iv_length(Cipher)  -> maps:get(iv_length, crypto:cipher_info(Cipher)).
