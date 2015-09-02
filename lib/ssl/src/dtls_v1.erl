%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2014. All Rights Reserved.
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

-export([suites/1, mac_hash/7, ecc_curves/1, corresponding_tls_version/1]).

-spec suites(Minor:: 253|255) -> [ssl_cipher:cipher_suite()].

suites(Minor) ->
   tls_v1:suites(corresponding_minor_tls_version(Minor)).

mac_hash(Version, MacAlg, MacSecret, SeqNo, Type, Length, Fragment) ->
    tls_v1:mac_hash(MacAlg, MacSecret, SeqNo, Type, corresponding_tls_version(Version),
		    Length, Fragment).

ecc_curves({_Major, Minor}) ->
    tls_v1:ecc_curves(corresponding_minor_tls_version(Minor)).

corresponding_tls_version({254, Minor}) ->
    {3, corresponding_minor_tls_version(Minor)}.

corresponding_minor_tls_version(255) ->
    2;
corresponding_minor_tls_version(253) ->
    3.
