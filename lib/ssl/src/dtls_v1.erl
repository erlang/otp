%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2018. All Rights Reserved.
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

-include("ssl_cipher.hrl").

-export([suites/1, all_suites/1, anonymous_suites/1,hmac_hash/3, ecc_curves/1, 
         corresponding_tls_version/1, corresponding_dtls_version/1,
         cookie_secret/0, cookie_timeout/0]).

-define(COOKIE_BASE_TIMEOUT, 30000).

-spec suites(Minor:: 253|255) -> [ssl_cipher_format:cipher_suite()].

suites(Minor) ->
    lists:filter(fun(Cipher) -> 
                         is_acceptable_cipher(ssl_cipher_format:suite_bin_to_map(Cipher)) 
                 end,
                 tls_v1:suites(corresponding_minor_tls_version(Minor))).
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
                 
hmac_hash(MacAlg, MacSecret, Value) ->
    tls_v1:hmac_hash(MacAlg, MacSecret, Value).

ecc_curves({_Major, Minor}) ->
    tls_v1:ecc_curves(corresponding_minor_tls_version(Minor)).

corresponding_tls_version({254, Minor}) ->
    {3, corresponding_minor_tls_version(Minor)}.

cookie_secret() ->
    crypto:strong_rand_bytes(32).

cookie_timeout() ->
    %% Cookie will live for two timeouts periods
    round(rand:uniform() * ?COOKIE_BASE_TIMEOUT/2).
    
corresponding_minor_tls_version(255) ->
    2;
corresponding_minor_tls_version(253) ->
    3.

corresponding_dtls_version({3, Minor}) -> 
    {254, corresponding_minor_dtls_version(Minor)}.

corresponding_minor_dtls_version(2) ->
    255;
corresponding_minor_dtls_version(3) ->
    253.
is_acceptable_cipher(Suite) ->
    not ssl_cipher:is_stream_ciphersuite(Suite).
