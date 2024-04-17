%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023-2024. All Rights Reserved.
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

-module(ssl_eqc_ecc).

%%-export([prop_tls_orded_path/1]).
-compile(export_all).

-proptest(eqc).
-proptest([triq,proper]).

-ifndef(EQC).
-ifndef(PROPER).
-ifndef(TRIQ).
-define(EQC,true).
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

-include_lib("public_key/include/public_key.hrl").
%%--------------------------------------------------------------------
%% Properties --------------------------------------------------------
%%--------------------------------------------------------------------
prop_tls_ecc() ->
    ?FORALL({ClientOptions, ServerOptions}, ?LET(Version, tls_version(), options(Version)),
            try
                [TLSVersion] = proplists:get_value(versions, ClientOptions),
                SigAlgs = signature_algs(TLSVersion),
                ssl_test_lib:basic_test(SigAlgs ++ ClientOptions,
                                        SigAlgs ++ ServerOptions, [{server_type, erlang},
                                                                   {client_type, erlang},
                                                                   {version, TLSVersion}
                                                                  ])
            of
                _ ->
                    true
            catch
                _:_ ->
                    false
            end
	   ).

%%--------------------------------------------------------------------
%% Generators
%%--------------------------------------------------------------------
tls_version() ->
    Versions = [Version || Version <- ['tlsv1.3', 'tlsv1.2', 'dtlsv1.2'],
                           ssl_test_lib:sufficient_crypto_support(Version)
               ],
    oneof(Versions).

curve('tlsv1.3') ->
    oneof(groups());
curve('tlsv1.2' = Version) ->
    oneof(eccs(Version));
curve('dtlsv1.2' = Version) ->
    oneof(eccs(Version)).

hash() ->
    oneof([sha256, sha384, sha512]).

eccs(Version) ->
    Curves = ssl:eccs(Version),
    Eccs = crypto:supports(curves),
    [ecc_to_sign_curve(Curve) || Curve <- Curves, lists:member(Curve, Eccs)].

groups() ->
    Groups = ssl:groups() -- [ffdhe2048,ffdhe3072,ffdhe4096,ffdhe6144,ffdhe8192],
    Eccs = crypto:supports(curves),
    [group_to_sign_curve(Group) || Group <- Groups, lists:member(group_to_sign_curve(Group), Eccs)].

protocol('dtlsv1.2') ->
    {protocol, dtls};
protocol(_) ->
    {protocol,tls}.

intermediates(Curve, _,  N) when Curve == ed25519;
                                 Curve == ed448 ->
    lists:duplicate(N, [{key, {namedCurve, Curve}}]);
intermediates(Curve, Hash, N) ->
    lists:duplicate(N, [{key, {namedCurve, Curve}}, {digest, Hash}]).

cert(Curve, _) when Curve == ed25519;
                    Curve == ed448 ->
    [{key, {namedCurve, Curve}}];
cert(Curve, Hash) ->
    [{key, {namedCurve, Curve}}, {digest, Hash}].

options(Version) ->
    ?LET({Curve, Hash}, {curve(Version), hash()}, options(Version, Curve, Hash)).

options(Version, Curve, Hash) ->
    #{server_config := ServerConf,
      client_config := ClientConf} =
        public_key:pkix_test_data(#{server_chain => #{root => cert(Curve, Hash),
                                                      intermediates => intermediates(Curve, Hash, 1),
                                                      peer => cert(Curve, Hash)},
                                    client_chain => #{root => cert(Curve, Hash),
                                                      intermediates => intermediates(Curve, Hash, 1),
                                                      peer => cert(Curve, Hash)}}),
 {client_options(Version) ++ [protocol(Version), {versions, [Version]} | ClientConf],
  server_options(Version) ++ [protocol(Version), {versions, [Version]} | ServerConf]}.

server_options('tlsv1.3') ->
    [{verify, verify_peer},
     {fail_if_no_peer_cert, true},
     {reuseaddr, true}];
server_options(_) ->
    [{verify, verify_peer},
     {fail_if_no_peer_cert, true},
     {reuse_sessions, false},
     {reuseaddr, true}].

client_options(_) ->
    [{verify, verify_peer}].

signature_algs('tlsv1.3') ->
    [ssl_test_lib:all_sig_algs()];
signature_algs(Version) when Version == 'tlsv1.2';
                             Version == 'dtlsv1.2' ->
    [ssl_test_lib:all_1_2_sig_algs()].

%% Edward curves not supported for
%% cert signing befor TLS-1.3 and pre TLS-1.3
%% cert sign and key-exchange curve may differ
ecc_to_sign_curve(x25519) ->
    secp256r1;
ecc_to_sign_curve(x448) ->
    secp256r1;
ecc_to_sign_curve(Curve) ->
    Curve.
%% Brainpool curves for TLS-1.3 have suffix in RFCs
group_to_sign_curve(brainpoolP512r1tls13) ->
    brainpoolP512r1;
group_to_sign_curve(brainpoolP384r1tls13) ->
    brainpoolP384r1;
group_to_sign_curve(brainpoolP256r1tls13) ->
    brainpoolP256r1;
%% Edwards curves have different names
%% for different uses.
group_to_sign_curve(x25519) ->
    ed25519;
group_to_sign_curve(x448) ->
    ed448;
group_to_sign_curve(Curve) ->
    Curve.
