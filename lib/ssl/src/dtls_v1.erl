%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2013. All Rights Reserved.
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
-module(dtls_v1).

-include("ssl_cipher.hrl").

-export([suites/1, mac_hash/7, ecc_curves/1, corresponding_tls_version/1]).

-spec suites(Minor:: 253|255) -> [cipher_suite()].

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
