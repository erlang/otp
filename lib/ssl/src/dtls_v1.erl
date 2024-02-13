%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2024. All Rights Reserved.
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
-module(dtls_v1).
-moduledoc false.

-include("ssl_cipher.hrl").
-include("ssl_record.hrl").

-export([suites/1,
         all_suites/1,
         anonymous_suites/1,
         exclusive_suites/1,
         exclusive_anonymous_suites/1,
         hmac_hash/3,
         ecc_curves/1,
         corresponding_tls_version/1,
         corresponding_dtls_version/1,
         cookie_secret/0,
         cookie_timeout/0]).

-define(COOKIE_BASE_TIMEOUT, 30000).

-spec suites(ssl_record:ssl_version()) -> [ssl_cipher_format:cipher_suite()].

suites(Version) ->
    lists:filter(fun(Cipher) -> 
                         is_acceptable_cipher(ssl_cipher_format:suite_bin_to_map(Cipher)) 
                 end,
                 tls_v1:suites(corresponding_tls_version(Version))).
all_suites(Version) ->
    lists:filter(fun(Cipher) -> 
                         is_acceptable_cipher(ssl_cipher_format:suite_bin_to_map(Cipher)) 
                 end,
                 ssl_cipher:all_suites(corresponding_tls_version(Version))).

anonymous_suites(Version) ->
    lists:filter(fun(Cipher) -> 
                         is_acceptable_cipher(ssl_cipher_format:suite_bin_to_map(Cipher)) 
                 end, 
                 ssl_cipher:anonymous_suites(corresponding_tls_version(Version))).

exclusive_suites(Version) ->
    lists:filter(fun(Cipher) ->
                         is_acceptable_cipher(ssl_cipher_format:suite_bin_to_map(Cipher))
                 end,
                 tls_v1:exclusive_suites(corresponding_tls_version(Version))).

exclusive_anonymous_suites(Version) ->
    lists:filter(fun(Cipher) ->
                         is_acceptable_cipher(ssl_cipher_format:suite_bin_to_map(Cipher))
                 end,
                 tls_v1:exclusive_anonymous_suites(corresponding_tls_version(Version))).


hmac_hash(MacAlg, MacSecret, Value) ->
    tls_v1:hmac_hash(MacAlg, MacSecret, Value).

ecc_curves(Version) ->
    tls_v1:ecc_curves(corresponding_tls_version(Version)).


corresponding_tls_version(?DTLS_1_0) ->
    ?TLS_1_1;
corresponding_tls_version(?DTLS_1_2) ->
    ?TLS_1_2.

cookie_secret() ->
    crypto:strong_rand_bytes(32).

cookie_timeout() ->
    %% Cookie will live for two timeouts periods
    round(rand:uniform() * ?COOKIE_BASE_TIMEOUT/2).


corresponding_dtls_version(?TLS_1_1) ->
    ?DTLS_1_0;
corresponding_dtls_version(?TLS_1_2) ->
    ?DTLS_1_2.

is_acceptable_cipher(Suite) ->
    not ssl_cipher:is_stream_ciphersuite(Suite).
