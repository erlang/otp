%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2023. All Rights Reserved.
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

-module(ssl_eqc_chain).

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
prop_tls_unordered_path(PrivDir) ->
    ?FORALL({ClientOptions, ServerOptions}, ?LET(Version, tls_version(), unordered_options(Version, PrivDir)),
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

prop_tls_extraneous_path(PrivDir) ->
    ?FORALL({ClientOptions, ServerOptions}, ?LET(Version, tls_version(), extraneous_options(Version, PrivDir)),
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

prop_tls_extraneous_paths() ->
    ?FORALL({ClientOptions, ServerOptions}, ?LET(Version, tls_version(), extra_extraneous_options(Version)),
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

prop_tls_extraneous_and_unordered_path() ->
    ?FORALL({ClientOptions, ServerOptions}, ?LET(Version, tls_version(), unordered_extraneous_options(Version)),
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

prop_client_cert_auth() ->
    ?FORALL({ClientOptions, ServerOptions}, ?LET(Version, tls_version(), client_cert_auth_opts(Version)),
            try
                [TLSVersion] = proplists:get_value(versions, ClientOptions),
                SigAlgs = signature_algs(TLSVersion),
                ssl_test_lib:basic_test(SigAlgs ++ ClientOptions,
                                        SigAlgs ++ ServerOptions,
                                        [{server_type, erlang},
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
%% Chain Generators  -----------------------------------------------
%%--------------------------------------------------------------------
tls_version() ->
    Versions = [Version || Version <- ['tlsv1.3', 'tlsv1.2', 'tlsv1.1', 'tlsv1', 'dtlsv1.2', 'dtlsv1'],
                           ssl_test_lib:sufficient_crypto_support(Version)
               ],
    oneof(Versions).

key_alg(Version) when Version == 'tlsv1.3';
                      Version == 'tlsv1.2';
                      Version == 'dtlsv1.2'->
    oneof([rsa, ecdsa]);
key_alg(_) ->
    oneof([rsa]).

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

unordered_options(Version, PrivDir) ->
    oneof([der_unordered_options(Version), pem_unordered_options(Version, PrivDir)]).

der_unordered_options(Version) ->
    ?LET(Alg, key_alg(Version), unordered_der_cert_chain_opts(Version, Alg)).

pem_unordered_options(Version, PrivDir) ->
    ?LET(Alg, key_alg(Version), unordered_pem_cert_chain_opts(Version, Alg, PrivDir)).

unordered_der_cert_chain_opts(Version, Alg) ->
    #{server_config := ServerConf,
      client_config := ClientConf} = public_key:pkix_test_data(#{server_chain => #{root => root_key(Alg),
                                                                                    intermediates => intermediates(Alg, 4),
                                                                                    peer => peer_key(Alg)},
                                                                  client_chain => #{root => root_key(Alg),
                                                                                    intermediates => intermediates(Alg, 4),
                                                                                    peer => peer_key(Alg)}}),
    {client_options(Version) ++ [protocol(Version), {versions, [Version]} | unordered_der_conf(ClientConf)],
     server_options(Version) ++ [protocol(Version), {versions, [Version]} | unordered_der_conf(ServerConf)]}.

unordered_pem_cert_chain_opts(Version, Alg, PrivDir) ->
    Index =  integer_to_list(erlang:unique_integer()),
    DerConfig = public_key:pkix_test_data(#{server_chain => #{root => root_key(Alg),
                                                              intermediates => intermediates(Alg, 4),
                                                              peer => peer_key(Alg)},
                                            client_chain => #{root => root_key(Alg),
                                                              intermediates => intermediates(Alg, 4),
                                                              peer => peer_key(Alg)}}),

    ClientBase = filename:join(PrivDir, "client_prop_test" ++ Index),
    SeverBase =  filename:join(PrivDir, "server_prop_test" ++ Index),
    PemConfig = x509_test:gen_pem_config_files(DerConfig, ClientBase, SeverBase),
    ClientConf = proplists:get_value(client_config, PemConfig),
    ServerConf = proplists:get_value(server_config, PemConfig),
    {client_options(Version) ++ [protocol(Version), {versions, [Version]}  | unordered_pem_conf(ClientConf)],
     server_options(Version) ++ [protocol(Version), {versions, [Version]} | unordered_pem_conf(ServerConf)]}.

unordered_der_conf(Config) ->
    Cert = proplists:get_value(cert, Config),
    {ok, ExtractedCAs} = ssl_pkix_db:extract_trusted_certs({der, proplists:get_value(cacerts, Config)}),
    {ok, _, [Cert | Path]} = ssl_certificate:certificate_chain(Cert,  ets:new(foo, []), ExtractedCAs, [], encoded),
    [{cert, [Cert | lists:reverse(Path)]}| proplists:delete(cert, Config)].

unordered_pem_conf(Config) ->
    CertFile = proplists:get_value(certfile, Config),
    CACertFile = proplists:get_value(cacertfile, Config),
    [{_, Cert, _}| _] = ssl_test_lib:pem_to_der(CertFile),
    PemCAs =  ssl_test_lib:pem_to_der(CACertFile),
    DerList = [DerCert || {'Certificate', DerCert, not_encrypted} <- PemCAs],
    {ok, ExtractedCAs} = ssl_pkix_db:extract_trusted_certs({der, DerList}),
    {ok, _, [Cert | Path]} = ssl_certificate:certificate_chain(Cert, ets:new(foo, []), ExtractedCAs, [], encoded),
    Unorded = lists:reverse(Path),
    UnordedPemEntries = [{'Certificate', DerCert, not_encrypted} || DerCert <- Unorded],
    PEM = public_key:pem_encode([{'Certificate', Cert, not_encrypted} |UnordedPemEntries]),
    file:write_file(CertFile, PEM),
    Config.

extraneous_options(Version, PrivDir) ->
    oneof([der_extraneous_options(Version),
           pem_extraneous_options(Version, PrivDir)
          ]).
extra_extraneous_options(Version) ->
    oneof([extra_der_extraneous_options(Version)]).

der_extraneous_options(Version) ->
    ?LET(Alg, key_alg(Version), extraneous_der_cert_chain_opts(Version, Alg)).

pem_extraneous_options(Version, PrivDir) ->
    ?LET(Alg, key_alg(Version), extraneous_pem_cert_chain_opts(Version, Alg, PrivDir)).

extra_der_extraneous_options(Version) ->
    ?LET(Alg, key_alg(Version), extra_extraneous_der_cert_chain_opts(Version, Alg)).

unordered_extraneous_options(Version) ->
    oneof([der_extraneous_and_unorder_options(Version)]).

der_extraneous_and_unorder_options(Version) ->
    ?LET(Alg, key_alg(Version), der_extraneous_and_unorder_chain(Version, Alg)).

client_cert_auth_opts(Version) ->
    ?LET({SAlg, CAlg}, {key_alg(Version), key_alg(Version)}, der_cert_chains(Version, CAlg,SAlg)).

extraneous_der_cert_chain_opts(Version, Alg) ->
    #{cert := OrgSRoot} = SRoot = public_key:pkix_test_root_cert("OTP test server ROOT", root_key(Alg)),
    #{cert := OrgCRoot} = CRoot = public_key:pkix_test_root_cert("OTP test client ROOT", root_key(Alg)),

    #{server_config := ServerConf0,
      client_config := ClientConf0} = public_key:pkix_test_data(#{server_chain => #{root => SRoot,
                                                                                    intermediates => intermediates(Alg, 1),
                                                                                    peer => peer_key(ecdsa)},
                                                                  client_chain => #{root => CRoot,
                                                                                    intermediates => intermediates(Alg, 1),
                                                                                    peer => peer_key(ecdsa)}}),

    {ClientChain, ClientRoot} = extraneous_chain_and_root(ClientConf0, "OTP test client ROOT", 1),
    {ServerChain, ServerRoot} = extraneous_chain_and_root(ServerConf0, "OTP test server ROOT", 1),


    {client_options(Version) ++ [protocol(Version), {versions, [Version]} |
                                 extraneous_der_conf(ClientChain, ServerRoot, [OrgSRoot], ClientConf0)],
     server_options(Version) ++ [protocol(Version), {versions, [Version]} |
                                 extraneous_der_conf(ServerChain, ClientRoot, [OrgCRoot], ServerConf0)]}.

extraneous_pem_cert_chain_opts(Version, Alg, PrivDir) ->
    #{cert := OrgSRoot} = SRoot = public_key:pkix_test_root_cert("OTP test server ROOT", root_key(Alg)),
    #{cert := OrgCRoot} = CRoot = public_key:pkix_test_root_cert("OTP test client ROOT", root_key(Alg)),

    #{server_config := ServerConf0,
      client_config := ClientConf0} = public_key:pkix_test_data(#{server_chain => #{root => SRoot,
                                                                                    intermediates => intermediates(Alg, 1),
                                                                                    peer => peer_key(ecdsa)},
                                                                  client_chain => #{root => CRoot,
                                                                                    intermediates => intermediates(Alg, 1),
                                                                                    peer => peer_key(ecdsa)}}),

    {ClientChain, ClientRoot} = extraneous_chain_and_root(ClientConf0, "OTP test client ROOT", 1),
    {ServerChain, ServerRoot} = extraneous_chain_and_root(ServerConf0, "OTP test server ROOT", 1),

    %% Only use files in final step
    {client_options(Version) ++ [protocol(Version), {versions, [Version]} |
                                 extraneous_pem_conf(ClientChain, ServerRoot, OrgSRoot, ClientConf0, PrivDir)],
     server_options(Version) ++ [protocol(Version), {versions, [Version]} |
                                 extraneous_pem_conf(ServerChain, ClientRoot, OrgCRoot, ServerConf0, PrivDir)]}.

extra_extraneous_der_cert_chain_opts(Version, Alg) ->
    #{cert := OrgSRoot} = SRoot = public_key:pkix_test_root_cert("OTP test server ROOT", root_key(Alg)),
    #{cert := OrgCRoot} = CRoot = public_key:pkix_test_root_cert("OTP test client ROOT", root_key(Alg)),

    #{server_config := ServerConf0,
      client_config := ClientConf0} = public_key:pkix_test_data(#{server_chain => #{root => SRoot,
                                                                                    intermediates => intermediates(Alg, 3),
                                                                                    peer => peer_key(ecdsa)},
                                                                  client_chain => #{root => CRoot,
                                                                                    intermediates => intermediates(Alg, 3),
                                                                                    peer => peer_key(ecdsa)}}),


    {ClientChain0, ClientRoot0} = extraneous_chain_and_root(ClientConf0, "OTP test client ROOT", 2),
    {ServerChain0, ServerRoot0} = extraneous_chain_and_root(ServerConf0, "OTP test server ROOT", 2),

    {ClientChain1, ClientRoot1} = extraneous_chain_and_root(ClientConf0, "OTP test client ROOT", 2),
    {ServerChain1, ServerRoot1} = extraneous_chain_and_root(ServerConf0, "OTP test server ROOT", 2),

    {ClientChain, ServerChain} = create_extraneous_chains(ClientChain0, ClientChain1,
                                                          ServerChain0, ServerChain1),

    {client_options(Version) ++ [protocol(Version), {versions, [Version]} |
                                 extraneous_der_conf(ClientChain, ServerRoot1, [OrgSRoot, ServerRoot0], ClientConf0)],
     server_options(Version) ++ [protocol(Version), {versions, [Version]} |
                                 extraneous_der_conf(ServerChain, ClientRoot1, [OrgCRoot, ClientRoot0], ServerConf0)]}.


der_extraneous_and_unorder_chain(Version, Alg) ->
    #{cert := OrgSRoot} = SRoot = public_key:pkix_test_root_cert("OTP test server ROOT", root_key(Alg)),
    #{cert := OrgCRoot} = CRoot = public_key:pkix_test_root_cert("OTP test client ROOT", root_key(Alg)),

    #{server_config := ServerConf0,
      client_config := ClientConf0} =
        public_key:pkix_test_data(#{server_chain => #{root => SRoot,
                                                      intermediates => intermediates(Alg, 3),
                                                      peer => peer_key(ecdsa)},
                                    client_chain => #{root => CRoot,
                                                      intermediates => intermediates(Alg, 3),
                                                      peer => peer_key(ecdsa)}}),

    {ClientChain0, ClientRoot0} = chain_and_root(ClientConf0),
    {ServerChain0, ServerRoot0} = chain_and_root(ServerConf0),

    {ClientChain1, ClientRoot1} = extraneous_chain_and_root(ClientConf0, "OTP test client ROOT", 2),
    {ServerChain1, ServerRoot1} = extraneous_chain_and_root(ServerConf0, "OTP test server ROOT", 2),

    {ClientChain, ServerChain} = create_extraneous_and_unorded(ClientChain0, ClientChain1,
                                                              ServerChain0, ServerChain1),

    {client_options(Version) ++ [protocol(Version), {versions, [Version]} |
                                 extraneous_der_conf(ClientChain, ServerRoot1, [OrgSRoot, ServerRoot0], ClientConf0)],
     server_options(Version) ++ [protocol(Version), {versions, [Version]} |
                                 extraneous_der_conf(ServerChain, ClientRoot1, [OrgCRoot, ClientRoot0], ServerConf0)]}.

der_cert_chains(Version, CAlg, SAlg) ->
    SRoot = public_key:pkix_test_root_cert("OTP test server ROOT", root_key(SAlg)),
    CRoot = public_key:pkix_test_root_cert("OTP test client ROOT", root_key(CAlg)),

    #{server_config := ServerConf,
      client_config := ClientConf} = public_key:pkix_test_data(#{server_chain => #{root => SRoot,
                                                                                    intermediates => intermediates(SAlg, 1),
                                                                                    peer => peer_key(SAlg)},
                                                                  client_chain => #{root => CRoot,
                                                                                    intermediates => intermediates(CAlg, 1),
                                                                                    peer => peer_key(CAlg)}}),
    {client_options(Version) ++ [protocol(Version), {versions, [Version]} | ClientConf],
     server_options(Version) ++ [protocol(Version), {versions, [Version]} | ServerConf]}.

chain_and_root(Config) ->
    OwnCert = proplists:get_value(cert, Config),
    {ok, ExtractedCAs} = ssl_pkix_db:extract_trusted_certs({der, proplists:get_value(cacerts, Config)}),
    {ok, Root, Chain} = ssl_certificate:certificate_chain(OwnCert, ets:new(foo, []), ExtractedCAs, [], encoded),
    {Chain, Root}.

extraneous_chain_and_root(Config, Name, 1) ->
    #{cert := NewRoot, key := Key} = public_key:pkix_test_root_cert(Name, root_key(ecdsa)),
    {[OwnCert, CA0, OldRoot], OldRoot} = chain_and_root(Config),
    CA1 = new_intermediat(CA0, Key),
    {[OwnCert, CA1, CA0], NewRoot};
extraneous_chain_and_root(Config, Name, 2) ->
    #{cert := NewRoot, key := Key} = public_key:pkix_test_root_cert(Name, root_key(ecdsa)),
     {[OwnCert, CA0, CA1, CA2, OldRoot], OldRoot} = chain_and_root(Config),
    CA3 = new_intermediat(CA2, Key),
    {[OwnCert, CA0, CA1, CA2, CA3], NewRoot}.

extraneous_der_conf(Chain, NewRoot, OrgRoots,Config0) ->
    CaCerts = proplists:get_value(cacerts, Config0),
    Config1 = [{cert, Chain} | proplists:delete(cert, Config0)],
    [{cacerts, [NewRoot | CaCerts -- OrgRoots]} | proplists:delete(cacerts, Config1)].

extraneous_pem_conf(Chain, NewRoot, OldRoot, Config0, PrivDir) ->
    Int = erlang:unique_integer(),
    FileName = filename:join(PrivDir, "prop_test" ++ integer_to_list(Int)),
    CaCerts = proplists:get_value(cacerts, Config0),
    NewCas = [NewRoot | CaCerts -- [OldRoot]],
    Entries = [{'Certificate', DerCert,  not_encrypted} || DerCert <- Chain],
    PemBin = public_key:pem_encode(Entries),
    file:write_file(FileName, PemBin),
    Config1 =  [{cacerts, NewCas} | proplists:delete(cacerts, Config0)],
    [{certfile, FileName} | proplists:delete(cert, Config1)].

protocol('dtlsv1.2') ->
    {protocol, dtls};
protocol('dtlsv1') ->
    {protocol, dtls};
protocol(_) ->
    {protocol,tls}.

create_extraneous_chains([Client, _CCA0, _CCA1, CCA2, _CCA3], [Client, OCCA0, OCCA1, OCCA2, OCROOT],
                         [Server, _SCA0, _SCA1, SCA2, _SROOT], [Server, OSCA0, OSCA1, OSCA2, OSROOT]) ->
    {[Client, OCCA0, OCCA1, CCA2, OCCA2, OCROOT], [Server, OSCA0, OSCA1, SCA2, OSCA2, OSROOT]}.
create_extraneous_and_unorded([Client, _CCA0, _CCA1, CCA2, _CCA3], [Client, OCCA0, OCCA1, OCCA2, OCROOT],
                              [Server, _SCA0, _SCA1, SCA2, _SROOT], [Server, OSCA0, OSCA1, OSCA2, OSROOT]) ->
    {[Client, OCCA0, CCA2, OCCA2, OCROOT, OCCA1], [Server, OSCA0, SCA2, OSCA2, OSROOT, OSCA1]}.

root_key(ecdsa) ->
    [{key,{namedCurve, ?secp256r1}}]; %% Use a curve that will be default supported in all TLS versions
root_key(rsa) ->
    %% As rsa keygen is not guaranteed to be fast
    [{key, ssl_test_lib:hardcode_rsa_key(6)}].

peer_key(ecdsa) ->
    [{key, {namedCurve, ?secp256r1}}]; %% Use a curve that will be default supported in all TLS versions
peer_key(rsa) ->
    %% As rsa keygen is not guaranteed to be fast
    [{key, ssl_test_lib:hardcode_rsa_key(6)}].

intermediates(ecdsa, N) ->
    lists:duplicate(N, []);
intermediates(rsa, N) when N =< 4 ->
    Default = lists:duplicate(N, []),
    %% As rsa keygen is not guaranteed to be fast
    hardcode_rsa_keys(Default, N, []).

hardcode_rsa_keys([], 0, Acc) ->
    Acc;
hardcode_rsa_keys([Head | Tail], N, Acc) ->
    hardcode_rsa_keys(Tail, N-1, [[{key, ssl_test_lib:hardcode_rsa_key(N)} | Head] | Acc]).

new_intermediat(CA0, Key) ->
    OTPCert = public_key:pkix_decode_cert(CA0, otp),
    TBSCert = OTPCert#'OTPCertificate'.tbsCertificate,
    Num = TBSCert#'OTPTBSCertificate'.serialNumber,
    public_key:pkix_sign(TBSCert#'OTPTBSCertificate'{serialNumber = Num+1}, Key).


signature_algs('tlsv1.3') ->
    [ssl_test_lib:all_sig_algs()];
signature_algs(Version) when Version == 'tlsv1.2';
                             Version == 'dtlsv1.2' ->
    [ssl_test_lib:all_1_2_sig_algs()];
signature_algs(_) ->
    [].

