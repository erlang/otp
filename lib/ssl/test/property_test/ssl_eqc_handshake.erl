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

-module(ssl_eqc_handshake).

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

-include_lib("kernel/include/inet.hrl").
-include_lib("ssl/src/tls_handshake_1_3.hrl").
-include_lib("ssl/src/tls_handshake.hrl").
-include_lib("ssl/src/ssl_handshake.hrl").
-include_lib("ssl/src/ssl_alert.hrl").
-include_lib("ssl/src/ssl_internal.hrl").
-include_lib("ssl/src/ssl_record.hrl").


%%--------------------------------------------------------------------
%% Properties --------------------------------------------------------
%%--------------------------------------------------------------------

prop_tls_hs_encode_decode() ->
    ?FORALL({Handshake, TLSVersion}, ?LET(Version, tls_version(), {tls_msg(Version), Version}),
            try 
                [Type, _Length, Data] = tls_handshake:encode_handshake(Handshake, TLSVersion),
                case tls_handshake:decode_handshake(TLSVersion, Type, Data) of
                    Handshake ->
                        true;
                    _ ->
                        false
                end
            catch
                throw:#alert{} ->
                    true
            end
	   ).

%%--------------------------------------------------------------------
%% Message Generators  -----------------------------------------------
%%--------------------------------------------------------------------

tls_msg(?TLS_1_3= Version) ->
    oneof([client_hello(Version),
           server_hello(Version),
           %%new_session_ticket()
          #end_of_early_data{},
           encrypted_extensions(),
           certificate(),
           certificate_request_1_3(),
           certificate_verify_1_3(),
           finished(),
           key_update()
          ]);
tls_msg(Version) ->
    oneof([
           #hello_request{},
           client_hello(Version),
           server_hello(Version),
           certificate(),
           %%server_key_exchange()
           certificate_request(Version),
           #server_hello_done{},
           %%certificate_verify()
           %%client_key_exchange()
           finished()
          ]).

%%
%% Shared messages
%%
client_hello(?TLS_1_3 = Version) ->
    #client_hello{session_id = session_id(),
                  client_version = ?TLS_1_2,
                  cipher_suites = cipher_suites(Version),
                  random = client_random(Version),
                  extensions = client_hello_extensions(Version)
                 };
client_hello(Version) ->
    #client_hello{session_id = session_id(),
		  client_version = Version,
                  cipher_suites = cipher_suites(Version),
		  random = client_random(Version),
		  extensions = client_hello_extensions(Version)    
                 }.

server_hello(?TLS_1_3 = Version) ->
    #server_hello{server_version = ?TLS_1_2,
		  session_id = session_id(),
                  random = server_random(Version),
                  cipher_suite = cipher_suite(Version),
		  extensions = server_hello_extensions(Version)    
                 };
server_hello(Version) ->
    #server_hello{server_version = Version,
		  session_id = session_id(),
                  random = server_random(Version),
                  cipher_suite = cipher_suite(Version),
		  extensions = server_hello_extensions(Version)    
                 }.

certificate() ->
    #certificate{
       asn1_certificates = certificate_chain()
      }.

certificate_1_3() ->
     ?LET(Certs, certificate_chain(),
          #certificate_1_3{
             certificate_request_context = certificate_request_context(),
             certificate_list = certificate_entries(Certs, [])
            }).

certificate_verify_1_3() ->
     ?LET(Certs, certificate_chain(),
          #certificate_verify_1_3{
             algorithm = sig_scheme(),
             signature = signature()
            }).

finished() ->
    ?LET(Size, digest_size(),
         #finished{verify_data = crypto:strong_rand_bytes(Size)}).

%%
%% TLS 1.0-1.2 messages
%%



%%
%% TLS 1.3 messages
%%

encrypted_extensions() ->
    ?LET(Exts, extensions(?TLS_1_3, encrypted_extensions),
         #encrypted_extensions{extensions = Exts}).


key_update() ->
    #key_update{request_update = request_update()}.


%%--------------------------------------------------------------------
%% Message Data Generators  -------------------------------------------
%%--------------------------------------------------------------------

tls_version() ->
    oneof([?TLS_1_3, ?TLS_1_2, ?TLS_1_1, ?TLS_1_0]).

cipher_suite(Version) ->
    oneof(cipher_suites(Version)).

cipher_suites(Version) ->
    ssl_cipher:suites(Version).

session_id() ->
    crypto:strong_rand_bytes(?NUM_OF_SESSION_ID_BYTES).

client_random(_) ->
    crypto:strong_rand_bytes(32).

server_random(_) ->
    crypto:strong_rand_bytes(32).


client_hello_extensions(Version) ->
    ?LET(Exts, extensions(Version, client_hello),
         maps:merge(ssl_handshake:empty_extensions(Version, client_hello),
                    Exts)).

server_hello_extensions(Version) ->
    ?LET(Exts, extensions(Version, server_hello),
         maps:merge(ssl_handshake:empty_extensions(Version, server_hello),
                    Exts)).

key_share_client_hello() ->
     oneof([undefined]).
    %%oneof([#key_share_client_hello{}, undefined]).

key_share_server_hello() ->
     oneof([undefined]).
    %%oneof([#key_share_server_hello{}, undefined]).

pre_shared_keyextension() ->
     oneof([undefined]).
     %%oneof([#pre_shared_keyextension{},undefined]).

%% +--------------------------------------------------+-------------+
%% | Extension                                        |     TLS 1.3 |
%% +--------------------------------------------------+-------------+
%% | server_name [RFC6066]                            |      CH, EE |
%% |                                                  |             |
%% | max_fragment_length [RFC6066]                    |      CH, EE |
%% |                                                  |             |
%% | status_request [RFC6066]                         |  CH, CR, CT |
%% |                                                  |             |
%% | supported_groups [RFC7919]                       |      CH, EE |
%% |                                                  |             |
%% | signature_algorithms (RFC 8446)                  |      CH, CR |
%% |                                                  |             |
%% | use_srtp [RFC5764]                               |      CH, EE |
%% |                                                  |             |
%% | heartbeat [RFC6520]                              |      CH, EE |
%% |                                                  |             |
%% | application_layer_protocol_negotiation [RFC7301] |      CH, EE |
%% |                                                  |             |
%% | signed_certificate_timestamp [RFC6962]           |  CH, CR, CT |
%% |                                                  |             |
%% | client_certificate_type [RFC7250]                |      CH, EE |
%% |                                                  |             |
%% | server_certificate_type [RFC7250]                |      CH, EE |
%% |                                                  |             |
%% | padding [RFC7685]                                |          CH |
%% |                                                  |             |
%% | key_share (RFC 8446)                             | CH, SH, HRR |
%% |                                                  |             |
%% | pre_shared_key (RFC 8446)                        |      CH, SH |
%% |                                                  |             |
%% | psk_key_exchange_modes (RFC 8446)                |          CH |
%% |                                                  |             |
%% | early_data (RFC 8446)                            | CH, EE, NST |
%% |                                                  |             |
%% | cookie (RFC 8446)                                |     CH, HRR |
%% |                                                  |             |
%% | supported_versions (RFC 8446)                    | CH, SH, HRR |
%% |                                                  |             |
%% | certificate_authorities (RFC 8446)               |      CH, CR |
%% |                                                  |             |
%% | oid_filters (RFC 8446)                           |          CR |
%% |                                                  |             |
%% | post_handshake_auth (RFC 8446)                   |          CH |
%% |                                                  |             |
%% | signature_algorithms_cert (RFC 8446)             |      CH, CR |
%% +--------------------------------------------------+-------------+
extensions(?TLS_1_3 = Version, MsgType = client_hello) ->
     ?LET({
           ServerName,
           %% MaxFragmentLength,
           %% StatusRequest,
           SupportedGroups,
           SignatureAlgorithms,
           UseSrtp,
           %% Heartbeat,
           ALPN,
           %% SignedCertTimestamp,
           %% ClientCertiticateType,
           %% ServerCertificateType,
           %% Padding,
           KeyShare,
           PreSharedKey,
           PSKKeyExchangeModes,
           %% EarlyData,
           %% Cookie,
           SupportedVersions,
           CertAuthorities,
           %% PostHandshakeAuth,
           SignatureAlgorithmsCert
          },
          {
           oneof([server_name(), undefined]),
           %% oneof([max_fragment_length(), undefined]),
           %% oneof([status_request(), undefined]),
           oneof([supported_groups(Version), undefined]),
           oneof([signature_algs(Version), undefined]),
           oneof([use_srtp(), undefined]),
           %% oneof([heartbeat(), undefined]),
           oneof([alpn(), undefined]),
           %% oneof([signed_cert_timestamp(), undefined]),
           %% oneof([client_cert_type(), undefined]),
           %% oneof([server_cert_type(), undefined]),
           %% oneof([padding(), undefined]),
           oneof([key_share(MsgType), undefined]),
           oneof([pre_shared_key(MsgType), undefined]),
           oneof([psk_key_exchange_modes(), undefined]),
           %% oneof([early_data(), undefined]),
           %% oneof([cookie(), undefined]),
           oneof([client_hello_versions(Version)]),
           oneof([cert_auths(), undefined]),
           %% oneof([post_handshake_auth(), undefined]),
           oneof([signature_algs_cert(), undefined])
          },
          maps:filter(fun(_, undefined) ->
                              false;
                         (_,_) ->
                              true
                      end,
                      #{
                        sni => ServerName,
                        %% max_fragment_length => MaxFragmentLength,
                        %% status_request => StatusRequest,
                        elliptic_curves => SupportedGroups,
                        signature_algs => SignatureAlgorithms,
                        use_srtp => UseSrtp,
                        %% heartbeat => Heartbeat,
                        alpn => ALPN,
                        %% signed_cert_timestamp => SignedCertTimestamp,
                        %% client_cert_type => ClientCertificateType,
                        %% server_cert_type => ServerCertificateType,
                        %% padding => Padding,
                        key_share => KeyShare,
                        pre_shared_key => PreSharedKey,
                        psk_key_exchange_modes => PSKKeyExchangeModes,
                        %% early_data => EarlyData,
                        %% cookie => Cookie,
                        client_hello_versions => SupportedVersions,
                        certificate_authorities => CertAuthorities,
                        %% post_handshake_auth => PostHandshakeAuth,
                        signature_algs_cert => SignatureAlgorithmsCert
                       }));
extensions(Version, client_hello) ->
    ?LET({
          SNI,
          ECPoitF,
          ECCurves,
          ALPN,
          NextP,
          SRP
          %% RenegotiationInfo
         },
         {
          oneof([sni(), undefined]),
          oneof([ec_point_formats(), undefined]),
          oneof([elliptic_curves(Version), undefined]), 
          oneof([alpn(),  undefined]), 
          oneof([next_protocol_negotiation(), undefined]),
          oneof([srp(), undefined])
          %% oneof([renegotiation_info(), undefined])
         },
         maps:filter(fun(_, undefined) -> 
                             false;
                        (_,_) -> 
                             true 
                     end, 
                     #{
                       sni => SNI,
                       ec_point_formats => ECPoitF,
                       elliptic_curves => ECCurves,
                       alpn => ALPN,
                       next_protocol_negotiation => NextP,
                       srp => SRP
                       %% renegotiation_info => RenegotiationInfo
                      }));
extensions(?TLS_1_3 = Version, MsgType = server_hello) ->
    ?LET({
          KeyShare,
          PreSharedKey,
          SupportedVersions
         },
         {
          oneof([key_share(MsgType), undefined]),
          oneof([pre_shared_key(MsgType),  undefined]),
          oneof([server_hello_selected_version()])
         },
         maps:filter(fun(_, undefined) ->
                             false;
                        (_,_) ->
                             true
                     end,
                     #{
                       key_share => KeyShare,
                       pre_shared_key => PreSharedKey,
                       server_hello_selected_version => SupportedVersions
                      }));
extensions(Version, server_hello) ->
    ?LET({
          ECPoitF,
          ALPN,
          NextP
          %% RenegotiationInfo,
         },
         {
          oneof([ec_point_formats(), undefined]),
          oneof([alpn(),  undefined]),
          oneof([next_protocol_negotiation(), undefined])
          %% oneof([renegotiation_info(), undefined]),
         },
         maps:filter(fun(_, undefined) ->
                             false;
                        (_,_) ->
                             true
                     end,
                     #{
                       ec_point_formats => ECPoitF,
                       alpn => ALPN,
                       next_protocol_negotiation => NextP
                       %% renegotiation_info => RenegotiationInfo
                      }));
extensions(?TLS_1_3 = Version, encrypted_extensions) ->
     ?LET({
           ServerName,
           %% MaxFragmentLength,
           SupportedGroups,
           %% UseSrtp,
           %% Heartbeat,
           ALPN
           %% ClientCertiticateType,
           %% ServerCertificateType,
           %% EarlyData
          },
          {
           oneof([server_name(), undefined]),
           %% oneof([max_fragment_length(), undefined]),
           oneof([supported_groups(Version), undefined]),
           %% oneof([use_srtp(), undefined]),
           %% oneof([heartbeat(), undefined]),
           oneof([alpn(), undefined])
           %% oneof([client_cert_type(), undefined]),
           %% oneof([server_cert_type(), undefined]),
           %% oneof([early_data(), undefined])
          },
          maps:filter(fun(_, undefined) ->
                              false;
                         (_,_) ->
                              true
                      end,
                      #{
                        sni => ServerName,
                        %% max_fragment_length => MaxFragmentLength,
                        elliptic_curves => SupportedGroups,
                        %% use_srtp => UseSrtp,
                        %% heartbeat => Heartbeat,
                        alpn => ALPN
                        %% client_cert_type => ClientCertificateType,
                        %% server_cert_type => ServerCertificateType,
                        %% early_data => EarlyData
                       })).

server_name() ->
  ?LET(ServerName, sni(),
       ServerName).
    %% sni().

signature_algs_cert() ->
    ?LET(List,  sig_scheme_list(),
         #signature_algorithms_cert{signature_scheme_list = List}).

signature_algorithms() ->
    ?LET(List,  sig_scheme_list(), 
         #signature_algorithms{signature_scheme_list = List}).

sig_scheme_list() ->
    oneof([[rsa_pkcs1_sha256],
           [rsa_pkcs1_sha256, ecdsa_sha1],
           [rsa_pkcs1_sha256,
            rsa_pkcs1_sha384,
            rsa_pkcs1_sha512,
            ecdsa_secp256r1_sha256,
            ecdsa_secp384r1_sha384,
            ecdsa_secp521r1_sha512,
            ecdsa_brainpoolP256r1tls13_sha256,
            ecdsa_brainpoolP384r1tls13_sha384,
            ecdsa_brainpoolP512r1tls13_sha512,
            rsa_pss_rsae_sha256,
            rsa_pss_rsae_sha384,
            rsa_pss_rsae_sha512,
            rsa_pss_pss_sha256,
            rsa_pss_pss_sha384,
            rsa_pss_pss_sha512,
            rsa_pkcs1_sha1,
            ecdsa_sha1]
          ]).

sig_scheme() ->
    oneof([rsa_pkcs1_sha256,
           rsa_pkcs1_sha384,
           rsa_pkcs1_sha512,
           ecdsa_secp256r1_sha256,
           ecdsa_secp384r1_sha384,
           ecdsa_secp521r1_sha512,
           ecdsa_brainpoolP256r1tls13_sha256,
           ecdsa_brainpoolP384r1tls13_sha384,
           ecdsa_brainpoolP512r1tls13_sha512,
           rsa_pss_rsae_sha256,
           rsa_pss_rsae_sha384,
           rsa_pss_rsae_sha512,
           rsa_pss_pss_sha256,
           rsa_pss_pss_sha384,
           rsa_pss_pss_sha512,
           rsa_pkcs1_sha1,
           ecdsa_sha1]).

signature() ->
    <<44,119,215,137,54,84,156,26,121,212,64,173,189,226,
      191,46,76,89,204,2,78,79,163,228,90,21,89,179,4,198,
      109,14,52,26,230,22,56,8,170,129,86,0,7,132,245,81,
      181,131,62,70,79,167,112,85,14,171,175,162,110,29,
      212,198,45,188,83,176,251,197,224,104,95,74,89,59,
      26,60,63,79,238,196,137,65,23,199,127,145,176,184,
      216,3,48,116,172,106,97,83,227,172,246,137,91,79,
      173,119,169,60,67,1,177,117,9,93,38,86,232,253,73,
      140,17,147,130,110,136,245,73,10,91,70,105,53,225,
      158,107,60,190,30,14,26,92,147,221,60,117,104,53,70,
      142,204,7,131,11,183,192,120,246,243,68,99,147,183,
      49,149,48,188,8,218,17,150,220,121,2,99,194,140,35,
      13,249,201,37,216,68,45,87,58,18,10,106,11,132,241,
      71,170,225,216,197,212,29,107,36,80,189,184,202,56,
      86,213,45,70,34,74,71,48,137,79,212,194,172,151,57,
      57,30,126,24,157,198,101,220,84,162,89,105,185,245,
      76,105,212,176,25,6,148,49,194,106,253,241,212,200,
      37,154,227,53,49,216,72,82,163>>.

client_hello_versions(?TLS_1_3) ->
    ?LET(SupportedVersions,
         oneof([[?TLS_1_3],
                %% This list breaks the property but can be used for negative tests
                %% [?TLS_1_2,?TLS_1_3],
                [?TLS_1_3,?TLS_1_2],
                [?TLS_1_3,?TLS_1_2,?TLS_1_1,?TLS_1_0,?SSL_3_0]
               ]),
        #client_hello_versions{versions = SupportedVersions});
client_hello_versions(_) ->
    ?LET(SupportedVersions,
         oneof([[?TLS_1_2],
                [?TLS_1_2,?TLS_1_1],
                [?TLS_1_2,?TLS_1_1,?TLS_1_0,?SSL_3_0]
               ]),
        #client_hello_versions{versions = SupportedVersions}).

server_hello_selected_version() ->
    #server_hello_selected_version{selected_version = ?TLS_1_3}.

request_update() ->
     oneof([update_not_requested, update_requested]).

certificate_chain()->
    Conf = cert_conf(),
    ?LET(Chain, 
         choose_certificate_chain(Conf),
         Chain).

choose_certificate_chain(#{server_config := ServerConf,
                           client_config := ClientConf}) -> 
    oneof([certificate_chain(ServerConf), certificate_chain(ClientConf)]).

certificate_request_context() ->
    oneof([<<>>,
           <<1>>,
           <<"foobar">>
          ]).
certificate_entries([], Acc) ->
    lists:reverse(Acc);
certificate_entries([Cert | Rest], Acc) ->
    certificate_entries(Rest, [certificate_entry(Cert) | Acc]).

certificate_entry(Cert) ->
    #certificate_entry{data = Cert,
                       extensions = certificate_entry_extensions()
                      }.
certificate_entry_extensions() ->
    #{}.

certificate_chain(Conf) ->  
    CAs = proplists:get_value(cacerts, Conf),
    Cert = proplists:get_value(cert, Conf),
    %% Middle argument are of correct type but will not be used
    {ok, _, Chain} = ssl_certificate:certificate_chain(Cert, ets:new(foo, []), make_ref(), CAs, encoded), 
    Chain.

cert_conf()->
    Hostname = net_adm:localhost(),
    {ok, #hostent{h_addr_list = [_IP |_]}} = inet:gethostbyname(net_adm:localhost()),
    public_key:pkix_test_data(#{server_chain => 
                                    #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)}],
                                      intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)}]],
                                      peer => [{extensions, [#'Extension'{extnID = 
                                                                              ?'id-ce-subjectAltName',
                                                                          extnValue = [{dNSName, Hostname}],
                                                                          critical = false}]},
                                               {key, ssl_test_lib:hardcode_rsa_key(3)}
                                              ]},
                                client_chain => 
                                    #{root => [{key, ssl_test_lib:hardcode_rsa_key(4)}],
                                      intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(5)}]],
                                      peer => [{key, ssl_test_lib:hardcode_rsa_key(6)}]}}).

cert_auths() ->
    certificate_authorities(?TLS_1_3).

certificate_request_1_3() ->
    #certificate_request_1_3{certificate_request_context = <<>>,
                             extensions = #{certificate_authorities => certificate_authorities(?TLS_1_3)}
                            }.
certificate_request(Version) ->
    #certificate_request{certificate_types = certificate_types(Version),
			 hashsign_algorithms = hashsign_algorithms(Version),
			 certificate_authorities = certificate_authorities(Version)}.

certificate_types(_) ->
    iolist_to_binary([<<?BYTE(?ECDSA_SIGN)>>, <<?BYTE(?RSA_SIGN)>>, <<?BYTE(?DSS_SIGN)>>]).

signature_algs(?TLS_1_3) ->
    ?LET(Algs, signature_algorithms(),
         Algs);
signature_algs(?TLS_1_2 = Version) ->
        #hash_sign_algos{hash_sign_algos = hash_alg_list(Version)};
signature_algs(Version) when ?TLS_LT(Version, ?TLS_1_2) ->
    undefined.



hashsign_algorithms(Version) when ?TLS_GTE(Version, ?TLS_1_2) ->
    #hash_sign_algos{hash_sign_algos = hash_alg_list(Version)};
hashsign_algorithms(_) -> 
    undefined.

hash_alg_list(Version) ->
    ?LET(NumOf, choose(1,15),
	 ?LET(List, [hash_alg(Version) || _ <- lists:seq(1,NumOf)],
	      lists:usort(List)
             )).
    
hash_alg(Version) ->
   ?LET(Alg, sign_algorithm(Version),
         {hash_algorithm(Version, Alg), Alg}
       ).

hash_algorithm(?TLS_1_3, _) ->
    oneof([sha, sha224, sha256, sha384, sha512]);
hash_algorithm(?TLS_1_2, rsa) ->
    oneof([sha, sha224, sha256, sha384, sha512]);
hash_algorithm(_, rsa) ->
    oneof([md5, sha, sha224, sha256, sha384, sha512]);
hash_algorithm(_, ecdsa) ->
    oneof([sha, sha224, sha256, sha384, sha512]);
hash_algorithm(_, dsa) ->
    sha.

sign_algorithm(?TLS_1_3) ->
    oneof([rsa, ecdsa]);
sign_algorithm(_) ->
    oneof([rsa, dsa, ecdsa]).

use_srtp() ->
    FullProfiles = [<<0,1>>, <<0,2>>, <<0,5>>],
    NullProfiles = [<<0,5>>],
    ?LET(PP, oneof([FullProfiles, NullProfiles]), #use_srtp{protection_profiles = PP, mki = <<>>}).

certificate_authorities(?TLS_1_3) ->
    Auths = certificate_authorities(?TLS_1_2),

    #certificate_authorities{authorities = Auths};
certificate_authorities(_) ->
    #{server_config := ServerConf} = cert_conf(), 
    Certs = proplists:get_value(cacerts, ServerConf),
    Auths = fun(#'OTPCertificate'{tbsCertificate = TBSCert}) ->
                    public_key:pkix_normalize_name(TBSCert#'OTPTBSCertificate'.subject)
            end,
    [Auths(public_key:pkix_decode_cert(Cert, otp)) || Cert <- Certs].

digest_size()->
   oneof([160,224,256,384,512]).

key_share_entry() ->
    undefined.
    %%#key_share_entry{}.

server_hello_selected_version(Version) ->
    #server_hello_selected_version{selected_version = Version}.

sni() ->
    #sni{hostname = net_adm:localhost()}.

ec_point_formats() ->
    #ec_point_formats{ec_point_format_list = ec_point_format_list()}.
 
ec_point_format_list() ->
    [?ECPOINT_UNCOMPRESSED].

elliptic_curves(Version) when ?TLS_LT(Version, ?TLS_1_3) ->
    Curves = tls_v1:ecc_curves(ssl:eccs()),
    #elliptic_curves{elliptic_curve_list = Curves}.

%% RFC 8446 (TLS 1.3) renamed the "elliptic_curve" extension.
supported_groups(Version) when ?TLS_GTE(Version, ?TLS_1_3) ->
    SupportedGroups = tls_v1:groups(),
    #supported_groups{supported_groups = SupportedGroups}.


alpn() ->
    ?LET(ExtD,  alpn_protocols(), #alpn{extension_data = ExtD}).

alpn_protocols() ->
    oneof([<<"spdy/2">>, <<"spdy/3">>, <<"http/2">>, <<"http/1.0">>, <<"http/1.1">>]).
    
next_protocol_negotiation() ->
   %% Predecessor to APLN
    ?LET(ExtD,  alpn_protocols(), #next_protocol_negotiation{extension_data = ExtD}).

srp() ->
   ?LET(Name, gen_name(),  #srp{username = list_to_binary(Name)}).

renegotiation_info() ->
    #renegotiation_info{renegotiated_connection = 0}.

gen_name() -> 
    ?LET(Size, choose(1,10), gen_string(Size)).

gen_char() -> 
    choose($a,$z).

gen_string(N) ->
    gen_string(N, []).

gen_string(0, Acc) ->
    Acc;
gen_string(N, Acc) ->
    ?LET(Char, gen_char(), gen_string(N-1, [Char | Acc])).

key_share(client_hello) ->
    ?LET(ClientShares, key_share_entry_list(),
        #key_share_client_hello{
          client_shares = ClientShares});
key_share(server_hello) ->
    ?LET([ServerShare], key_share_entry_list(1),
        #key_share_server_hello{
          server_share = ServerShare}).

key_share_entry_list() ->
    Max = length(ssl:groups()),
    ?LET(Size, choose(1,Max), key_share_entry_list(Size)).
%%
key_share_entry_list(N) ->
    key_share_entry_list(N, ssl:groups(), []).
%%
key_share_entry_list(0, _Pool, Acc) ->
    Acc;
key_share_entry_list(N, Pool, Acc) ->
    R = rand:uniform(length(Pool)),
    G = lists:nth(R, Pool),
    P = generate_public_key(G),
    KeyShareEntry =
        #key_share_entry{
          group = G,
          key_exchange = P},
    key_share_entry_list(N - 1, Pool -- [G], [KeyShareEntry|Acc]).

generate_public_key(Group)
  when Group =:= secp256r1 orelse
       Group =:= secp384r1 orelse
       Group =:= secp521r1 orelse
       Group =:= x448 orelse
       Group =:= x25519 ->
    #'ECPrivateKey'{publicKey = PublicKey} =
        public_key:generate_key({namedCurve, Group}),
    PublicKey;
generate_public_key(Group)
  when Group =:= brainpoolP512r1tls13 orelse
       Group =:= brainpoolP384r1tls13 orelse
       Group =:= brainpoolP256r1tls13 ->
    #'ECPrivateKey'{publicKey = PublicKey} =
        public_key:generate_key({namedCurve, group_to_curve(Group)}),
    PublicKey;

generate_public_key(Group) ->
    {PublicKey, _} =
        public_key:generate_key(ssl_dh_groups:dh_params(Group)),
    PublicKey.

groups() ->
    Max = length(ssl:groups()),
    ?LET(Size, choose(1,Max), group_list(Size)).

group_list(N) ->
    group_list(N, ssl:groups(), []).
%%
group_list(0, _Pool, Acc) ->
    Acc;
group_list(N, Pool, Acc) ->
    R = rand:uniform(length(Pool)),
    G = lists:nth(R, Pool),
    group_list(N - 1, Pool -- [G], [G|Acc]).


ke_modes() ->
    oneof([[psk_ke],[psk_dhe_ke],[psk_ke,psk_dhe_ke]]).

psk_key_exchange_modes() ->
    ?LET(KEModes, ke_modes(),
         #psk_key_exchange_modes{
            ke_modes = KEModes}).

pre_shared_key(client_hello) ->
    ?LET(OfferedPsks, offered_psks(),
         #pre_shared_key_client_hello{
            offered_psks = OfferedPsks});
pre_shared_key(server_hello) ->
    ?LET(SelectedIdentity, selected_identity(),
         #pre_shared_key_server_hello{
           selected_identity = SelectedIdentity}).

selected_identity() ->
    rand:uniform(32).

offered_psks() ->
    ?LET(Size, choose(1,5),
         #offered_psks{
            identities = psk_identities(Size),
            binders = psk_binders(Size)}).

psk_identities(Size) ->
    psk_identities(Size, []).
%%
psk_identities(0, Acc) ->
    Acc;
psk_identities(N, Acc) ->
    psk_identities(N - 1, [psk_identity()|Acc]).

psk_identity() ->
    Len = 8 + rand:uniform(8),
    Identity = crypto:strong_rand_bytes(Len),
    <<?UINT32(Age)>> = crypto:strong_rand_bytes(4),
    #psk_identity{
      identity = Identity,
      obfuscated_ticket_age = Age}.

psk_binders(Size) ->
    psk_binders(Size, []).
%%
psk_binders(0, Acc) ->
    Acc;
psk_binders(N, Acc) ->
    psk_binders(N - 1, [psk_binder()|Acc]).

psk_binder() ->
    Len = rand:uniform(224) + 31,
    crypto:strong_rand_bytes(Len).

group_to_curve(brainpoolP512r1tls13) ->
    brainpoolP512r1;
group_to_curve(brainpoolP384r1tls13) ->
    brainpoolP384r1;
group_to_curve(brainpoolP256r1tls13) ->
    brainpoolP256r1;
group_to_curve(Curve) ->
    Curve.
