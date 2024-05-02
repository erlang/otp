%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2024. All Rights Reserved.
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

%%% Purpose : Main API module for the SSL application that implements TLS and DTLS 
%%% SSL is a legacy name.

-module(ssl).

-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/logger.hrl").

-include("ssl_internal.hrl").
-include("ssl_api.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_handshake.hrl").
-include("ssl_srp.hrl").

%% Needed to make documentation rendering happy
-ifndef(VSN).
-define(VSN,"unknown").
-endif.

%% Application handling
-export([start/0, 
         start/1, 
         stop/0, 
         clear_pem_cache/0]).

%% Socket handling
-export([connect/3, 
         connect/2, 
         connect/4,
	 listen/2, 
         transport_accept/1, 
         transport_accept/2,
	 handshake/1, 
         handshake/2, 
         handshake/3, 
         handshake_continue/2,
         handshake_continue/3, 
         handshake_cancel/1,
	 controlling_process/2, 
         peername/1, 
         peercert/1, 
         sockname/1,
	 close/1, 
         close/2, 
         shutdown/2, 
         recv/2, 
         recv/3, 
         send/2,
	 getopts/2, 
         setopts/2, 
         getstat/1, 
         getstat/2
	]).

%% SSL/TLS protocol handling
-export([cipher_suites/2, 
         cipher_suites/3,
         filter_cipher_suites/2,
         prepend_cipher_suites/2, 
         append_cipher_suites/2,
         signature_algs/2,
         eccs/0, 
         eccs/1, 
         versions/0, 
         groups/0, 
         groups/1,
         format_error/1, 
         renegotiate/1, 
         update_keys/2,
         export_key_materials/4,
         export_key_materials/5,
         prf/5, 
         negotiated_protocol/1, 
	 connection_information/1, 
         connection_information/2]).
%% Misc
-export([handle_options/3,
         update_options/3,
         tls_version/1,
         suite_to_str/1,
         suite_to_openssl_str/1,
         str_to_suite/1]).
%% Tracing
-export([handle_trace/3]).

-removed({ssl_accept, '_', 
          "use ssl_handshake/1,2,3 instead"}).
-removed({cipher_suites, 0, 
          "use cipher_suites/2,3 instead"}).
-removed({cipher_suites, 1, 
          "use cipher_suites/2,3 instead"}).
-removed([{negotiated_next_protocol,1,
          "use ssl:negotiated_protocol/1 instead"}]).
-removed([{connection_info,1,
          "use ssl:connection_information/[1,2] instead"}]).

-export_type([socket/0,
              sslsocket/0,
              socket_option/0,
              active_msgs/0,
              host/0,
              tls_option/0,              
              tls_client_option/0,
              tls_server_option/0,                            
              erl_cipher_suite/0,
              old_cipher_suite/0,
              ciphers/0,             
              cipher/0,
              hash/0,
              key/0,
              kex_algo/0,
              prf_random/0, 
              cipher_filters/0,
              sign_algo/0,
              protocol_version/0,
              protocol_extensions/0,
              session_id/0,
              error_alert/0,
              tls_alert/0,
              srp_param_type/0,
              named_curve/0,
              sign_scheme/0,
              signature_algs/0,
              group/0,
              connection_info/0
             ]).

%% -------------------------------------------------------------------------------------------------------

-type socket()                   :: gen_tcp:socket(). % exported
-type socket_option()            :: gen_tcp:connect_option() | gen_tcp:listen_option() | gen_udp:option(). % exported
-type sslsocket()                :: any(). % exported
-type tls_option()               :: tls_client_option() | tls_server_option(). % exported
-type tls_client_option()        :: client_option() | common_option() | socket_option() |  transport_option(). % exported
-type tls_server_option()        :: server_option() | common_option() | socket_option() | transport_option(). % exported
-type active_msgs()              :: {ssl, sslsocket(), Data::binary() | list()} | {ssl_closed, sslsocket()} |
                                    {ssl_error, sslsocket(), Reason::any()} | {ssl_passive, sslsocket()}. % exported
-type transport_option()         :: {cb_info, {CallbackModule::atom(), DataTag::atom(),
                                               ClosedTag::atom(), ErrTag::atom()}} |  
                                    {cb_info, {CallbackModule::atom(), DataTag::atom(),
                                               ClosedTag::atom(), ErrTag::atom(), PassiveTag::atom()}}.
-type host()                     :: inet:hostname() | inet:ip_address(). % exported
-type session_id()               :: binary(). % exported
-type protocol_version()         :: tls_version() | dtls_version(). % exported
-type tls_version()              :: 'tlsv1.2' | 'tlsv1.3' | tls_legacy_version().
-type dtls_version()             :: 'dtlsv1.2' | dtls_legacy_version().
-type tls_legacy_version()       ::  tlsv1 | 'tlsv1.1' .
-type dtls_legacy_version()      :: 'dtlsv1'.
-type verify_type()              :: verify_none | verify_peer.
-type cipher()                   :: aes_256_gcm
                                  | aes_128_gcm
                                  | aes_256_ccm
                                  | aes_128_ccm
                                  | chacha20_poly1305
                                  | aes_256_ccm_8
                                  | aes_128_ccm_8
                                  | aes_128_cbc
                                  | aes_256_cbc
                                  |  legacy_cipher(). % exported
-type legacy_cipher()            :: '3des_ede_cbc'
                                  | des_cbc
                                  | rc4_128.

-type hash()                     :: sha2()
                                  | legacy_hash(). % exported

-type sha2()                    :: sha512
                                 | sha384
                                 | sha256.

-type legacy_hash()             :: sha224
                                 | sha
                                 | md5.

-type sign_algo()               :: eddsa
                                 | ecdsa
                                 | rsa
                                 | dsa. % exported

-type sign_schemes()            :: [sign_scheme()].

-type sign_scheme()             :: eddsa_ed25519
                                 | eddsa_ed448
                                 | ecdsa_secp384r1_sha384
                                 | ecdsa_secp521r1_sha512
                                 | ecdsa_secp256r1_sha256
                                 | rsassa_pss_scheme()
                                 | sign_scheme_legacy() . % exported

-type rsassa_pss_scheme()       :: rsa_pss_rsae_sha512
                                 | rsa_pss_rsae_sha384
                                 | rsa_pss_rsae_sha256
                                 | rsa_pss_pss_sha512
                                 | rsa_pss_pss_sha384
                                 | rsa_pss_pss_sha256.

-type sign_scheme_legacy()      :: rsa_pkcs1_sha512
                                 | rsa_pkcs1_sha384
                                 | rsa_pkcs1_sha256
                                 | ecdsa_sha1
                                 | rsa_pkcs1_sha1.

-type kex_algo()                :: ecdhe_ecdsa
                                 | ecdh_ecdsa
                                 | ecdh_rsa
                                 | rsa
                                 | dhe_rsa
                                 | dhe_dss
                                 | srp_rsa
                                 | srp_dss
                                 | dhe_psk
                                 | rsa_psk
                                 | psk
                                 | ecdh_anon
                                 | dh_anon
                                 | srp_anon
                                 |  any. %% TLS 1.3 (any of TLS-1.3 keyexchanges) , exported

-type erl_cipher_suite()       :: #{key_exchange := kex_algo(),
                                    cipher := cipher(),
                                    mac    := hash() | aead,
                                    prf    := hash() | default_prf %% Old cipher suites, version dependent
                                   }.  

-type old_cipher_suite()       :: {kex_algo(), cipher(), hash()} % Pre TLS 1.2
                                  %% TLS 1.2, internally PRE TLS 1.2 will use default_prf
                                | {kex_algo(), cipher(), hash() | aead, hash()}.

-type named_curve()            :: x25519
                                | x448
                                | secp521r1
                                | brainpoolP512r1
                                | brainpoolP384r1
                                | secp384r1
                                | brainpoolP256r1
                                | secp256r1
                                | legacy_named_curve(). % exported

-type legacy_named_curve()     :: sect571r1
                                | sect571k1
                                | sect409k1
                                | sect409r1
                                | sect283k1
                                | sect283r1
                                | secp256k1
                                | sect239k1
                                | sect233k1
                                | sect233r1
                                | secp224k1
                                | secp224r1
                                | sect193r1
                                | sect193r2
                                | secp192k1
                                | secp192r1
                                | sect163k1
                                | sect163r1
                                | sect163r2
                                | secp160k1
                                | secp160r1
                                | secp160r2.

-type group()                  :: x25519
                                | x448
                                | secp256r1
                                | secp384r1
                                | secp521r1
                                | ffdhe2048
                                | ffdhe3072
                                | ffdhe4096
                                | ffdhe6144
                                | ffdhe8192. % exported

-type srp_param_type()        :: srp_8192
                               | srp_6144
                               | srp_4096
                               | srp_3072
                               | srp_2048
                               | srp_1536
                               | srp_1024. % exported

-type error_alert()           :: {tls_alert, {tls_alert(), Description::string()}}. % exported

-type tls_alert()             :: close_notify | 
                                 unexpected_message | 
                                 bad_record_mac | 
                                 record_overflow | 
                                 handshake_failure |
                                 bad_certificate | 
                                 unsupported_certificate | 
                                 certificate_revoked | 
                                 certificate_expired | 
                                 certificate_unknown |
                                 illegal_parameter | 
                                 unknown_ca | 
                                 access_denied | 
                                 decode_error | 
                                 decrypt_error | 
                                 export_restriction| 
                                 protocol_version |
                                 insufficient_security |
                                 internal_error |
                                 inappropriate_fallback |
                                 user_canceled |
                                 no_renegotiation |
                                 unsupported_extension |
                                 certificate_unobtainable |
                                 unrecognized_name |
                                 bad_certificate_status_response |
                                 bad_certificate_hash_value |
                                 unknown_psk_identity |
                                 no_application_protocol. % exported

%% -------------------------------------------------------------------------------------------------------
-type common_option()        :: {protocol, protocol()} |
                                {handshake, handshake_completion()} |
                                {cert, cert() | [cert()]} |
                                {certfile, cert_pem()} |
                                {key, key()} |
                                {keyfile, key_pem()} |
                                {password, key_pem_password()} |
                                {certs_keys, certs_keys()} |
                                {ciphers, cipher_suites()} |
                                {eccs, [named_curve()]} |
                                {signature_algs, signature_algs()} |
                                {signature_algs_cert, sign_schemes()} |
                                {supported_groups, supported_groups()} |
                                {secure_renegotiate, secure_renegotiation()} |
                                {keep_secrets, keep_secrets()} |
                                {depth, allowed_cert_chain_length()} |
                                {verify_fun, custom_verify()} |
                                {crl_check, crl_check()} |
                                {crl_cache, crl_cache_opts()} |
                                {max_handshake_size, handshake_size()} |
                                {partial_chain, root_fun()} |
                                {versions, protocol_versions()} |
                                {user_lookup_fun, custom_user_lookup()} |
                                {log_level, logging_level()} |
                                {log_alert, log_alert()} |
                                {hibernate_after, hibernate_after()} |
                                {padding_check, padding_check()} |
                                {beast_mitigation, beast_mitigation()} |
                                {ssl_imp, ssl_imp()} |
                                {session_tickets, session_tickets()} |
                                {key_update_at, key_update_at()} |
                                {receiver_spawn_opts, spawn_opts()} |
                                {sender_spawn_opts, spawn_opts()}.

-type protocol()                  :: tls | dtls.
-type handshake_completion()      :: hello | full.
-type cert()                      :: public_key:der_encoded().
-type cert_pem()                  :: file:filename().
-type key()                       :: {'RSAPrivateKey'| 'DSAPrivateKey' | 'ECPrivateKey' |'PrivateKeyInfo', 
                                           public_key:der_encoded()} | 
                                     #{algorithm := rsa | dss | ecdsa, 
                                       engine := crypto:engine_ref(), 
                                       key_id := crypto:key_id(), 
                                       password => crypto:password()}. % exported
-type key_pem()                   :: file:filename().
-type key_pem_password()          :: iodata() | fun(() -> iodata()).
-type certs_keys()                :: [cert_key_conf()].
-type cert_key_conf()             :: #{cert => cert(),
                                       key => key(),
                                       certfile => cert_pem(),
                                       keyfile => key_pem(),
                                       password => key_pem_password()}.
-type cipher_suites()             :: ciphers().
-type ciphers()                   :: [erl_cipher_suite()] |
                                     string(). % (according to old API) exported
-type cipher_filters()            :: list({key_exchange | cipher | mac | prf,
                                        algo_filter()}). % exported
-type algo_filter()               :: fun((kex_algo()|cipher()|hash()|aead|default_prf) -> true | false).
-type keep_secrets()              :: boolean().
-type secure_renegotiation()      :: boolean(). 
-type allowed_cert_chain_length() :: integer().

-type custom_verify()               ::  {Verifyfun :: fun(), InitialUserState :: any()}.
-type crl_check()                :: boolean() | peer | best_effort.
-type crl_cache_opts()           :: {Module :: atom(),
                                     {DbHandle :: internal | term(),
                                      Args :: list()}}.
-type handshake_size()           :: integer().
-type hibernate_after()          :: timeout().
-type root_fun()                 ::  fun().
-type protocol_versions()        ::  [protocol_version()].
-type signature_algs()           ::  [{hash(), sign_algo()} | sign_scheme()].
-type supported_groups()         ::  [group()].
-type custom_user_lookup()       ::  {Lookupfun :: fun(), UserState :: any()}.
-type padding_check()            :: boolean(). 
-type beast_mitigation()         :: one_n_minus_one | zero_n | disabled.
-type srp_identity()             :: {Username :: string(), Password :: string()}.
-type psk_identity()             :: string().
-type log_alert()                :: boolean().
-type logging_level()            :: logger:level() | none | all.
-type client_session_tickets()   :: disabled | manual | auto.
-type server_session_tickets()   :: disabled | stateful | stateless | stateful_with_cert | stateless_with_cert.
-type session_tickets()          :: client_session_tickets() | server_session_tickets().
-type key_update_at()            :: pos_integer().
-type bloom_filter_window_size()    :: integer().
-type bloom_filter_hash_functions() :: integer().
-type bloom_filter_bits()           :: integer().
-type anti_replay()              :: '10k' | '100k' |
                                    {bloom_filter_window_size(),    %% number of seconds in time window
                                     bloom_filter_hash_functions(), %% k - number of hash functions
                                     bloom_filter_bits()}.          %% m - number of bits in bit vector
-type use_ticket()               :: [binary()].
-type middlebox_comp_mode()      :: boolean().
-type client_early_data()        :: binary().
-type server_early_data()        :: disabled | enabled.
-type use_srtp()                 :: #{protection_profiles := [binary()], mki => binary()}.
-type spawn_opts()               :: [erlang:spawn_opt_option()].

%% -------------------------------------------------------------------------------------------------------

-type client_option()        :: {verify, client_verify_type()} |
                                {reuse_session, client_reuse_session()} |
                                {reuse_sessions, client_reuse_sessions()} |
                                {cacerts, client_cacerts()} |
                                {cacertfile, client_cafile()} |
                                {alpn_advertised_protocols, client_alpn()} |
                                {client_preferred_next_protocols, client_preferred_next_protocols()} |
                                {psk_identity, client_psk_identity()} |
                                {srp_identity, client_srp_identity()} |
                                {server_name_indication, sni()} |
                                {max_fragment_length, max_fragment_length()} |
                                {customize_hostname_check, customize_hostname_check()} |
                                {fallback, fallback()} |
                                {middlebox_comp_mode, middlebox_comp_mode()} |
                                {certificate_authorities, client_certificate_authorities()} |
                                {session_tickets, client_session_tickets()} |
                                {use_ticket, use_ticket()} |
                                {early_data, client_early_data()} |
                                {use_srtp, use_srtp()}.
                                %% {ocsp_stapling, ocsp_stapling()} |
                                %% {ocsp_responder_certs, ocsp_responder_certs()} |
                                %% {ocsp_nonce, ocsp_nonce()}.

-type client_verify_type()       :: verify_type().
-type client_reuse_session()     :: session_id() | {session_id(), SessionData::binary()}.
-type client_reuse_sessions()    :: boolean() | save.
-type client_certificate_authorities()  :: boolean().
-type client_cacerts()           :: [public_key:der_encoded()] | [public_key:combined_cert()].
-type client_cafile()            :: file:filename().
-type app_level_protocol()       :: binary().
-type client_alpn()              :: [app_level_protocol()].
-type client_preferred_next_protocols() :: {Precedence :: server | client, 
                                            ClientPrefs :: [app_level_protocol()]} |
                                           {Precedence :: server | client, 
                                            ClientPrefs :: [app_level_protocol()], 
                                            Default::app_level_protocol()}.
-type client_psk_identity()             :: psk_identity().
-type client_srp_identity()             :: srp_identity().
-type customize_hostname_check() :: list().
-type sni()                      :: inet:hostname() | disable. 
-type max_fragment_length()      :: undefined | 512 | 1024 | 2048 | 4096.
-type fallback()                 :: boolean().
-type ssl_imp()                  :: new | old.
%% -type ocsp_stapling()            :: boolean().
%% -type ocsp_responder_certs()     :: [public_key:der_encoded()].
%% -type ocsp_nonce()               :: boolean().

%% -------------------------------------------------------------------------------------------------------

-type server_option()        :: {cacerts, server_cacerts()} |
                                {cacertfile, server_cafile()} |
                                {dh, dh_der()} |
                                {dhfile, dh_file()} |
                                {verify, server_verify_type()} |
                                {fail_if_no_peer_cert, fail_if_no_peer_cert()} |
                                {certificate_authorities, server_certificate_authorities()} |
                                {reuse_sessions, server_reuse_sessions()} |
                                {reuse_session, server_reuse_session()} |
                                {alpn_preferred_protocols, server_alpn()} |
                                {next_protocols_advertised, server_next_protocol()} |
                                {psk_identity, server_psk_identity()} |
                                {sni_hosts, sni_hosts()} |
                                {sni_fun, sni_fun()} |
                                {honor_cipher_order, honor_cipher_order()} |
                                {honor_ecc_order, honor_ecc_order()} |
                                {client_renegotiation, client_renegotiation()}|
                                {session_tickets, server_session_tickets()} |
                                {stateless_tickets_seed, stateless_tickets_seed()} |
                                {anti_replay, anti_replay()} |
                                {cookie, cookie()} |
                                {early_data, server_early_data()} |
                                {use_srtp, use_srtp()}.

-type server_cacerts()           :: [public_key:der_encoded()] | [public_key:combined_cert()].
-type server_cafile()            :: file:filename().
-type server_alpn()              :: [app_level_protocol()].
-type server_next_protocol()     :: [app_level_protocol()].
-type server_psk_identity()      :: psk_identity().
-type dh_der()                   :: binary().
-type dh_file()                  :: file:filename().
-type server_verify_type()       :: verify_type().
-type fail_if_no_peer_cert()     :: boolean().
-type server_reuse_session()     :: fun().
-type server_reuse_sessions()    :: boolean().
-type sni_hosts()                :: [{inet:hostname(), [server_option() | common_option()]}].
-type sni_fun()                  :: fun((string()) -> [] | undefined).
-type honor_cipher_order()       :: boolean().
-type honor_ecc_order()          :: boolean().
-type client_renegotiation()     :: boolean().
-type stateless_tickets_seed()   :: binary().
-type cookie()                   :: boolean().
-type server_certificate_authorities() :: boolean().
%% -------------------------------------------------------------------------------------------------------
-type prf_random() :: client_random | server_random. % exported
-type protocol_extensions()  :: #{renegotiation_info => binary(),
                                  signature_algs => signature_algs(),
                                  alpn =>  app_level_protocol(),
                                  srp  => binary(),
                                  next_protocol => app_level_protocol(),
                                  max_frag_enum  => 1..4,
                                  ec_point_formats  => [0..2],
                                  elliptic_curves => [public_key:oid()],
                                  sni => inet:hostname()}. % exported
%% -------------------------------------------------------------------------------------------------------
-type connection_info() :: [common_info() | curve_info() | ssl_options_info() | security_info()].
-type common_info() :: {protocol, protocol_version()} |
                       {session_id, session_id()} |
                       {session_resumption, boolean()} |
                       {selected_cipher_suite, erl_cipher_suite()} |
                       {sni_hostname, term()} |
                       {srp_username, term()}.
-type curve_info() :: {ecc, {named_curve, term()}}.
-type ssl_options_info() :: tls_option().
-type security_info() :: {client_random, binary()} |
                         {server_random, binary()} |
                         {master_secret, binary()}.
-type connection_info_items() :: [connection_info_item()].
-type connection_info_item() :: protocol |
                                session_id |
                                session_resumption |
                                selected_cipher_suite |
                                sni_hostname |
                                srp_username |
                                ecc |
                                client_random |
                                server_random |
                                master_secret |
                                keylog |
                                tls_options_name().
-type tls_options_name() :: atom().
%% -------------------------------------------------------------------------------------------------------

%%%--------------------------------------------------------------------
%%% API
%%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%%
%% Description: Utility function that starts the ssl and applications
%% that it depends on.
%% see application(3)
%%--------------------------------------------------------------------
-spec start() -> ok  | {error, reason()}.
start() ->
    start(temporary).
-spec start(permanent | transient | temporary) -> ok | {error, reason()}.
start(Type) ->
    case application:ensure_all_started(ssl, Type) of
	{ok, _} ->
	    ok;
	Other ->
	    Other
    end.
%%--------------------------------------------------------------------
-spec stop() -> ok.
%%
%% Description: Stops the ssl application.
%%--------------------------------------------------------------------
stop() ->
    application:stop(ssl).

%%--------------------------------------------------------------------
%%
%% Description: Connect to an ssl server.
%%--------------------------------------------------------------------

-spec connect(TCPSocket, TLSOptions) ->
                     {ok, sslsocket()} |
                     {error, reason()} |
                     {option_not_a_key_value_tuple, any()} when
      TCPSocket :: socket(),
      TLSOptions :: [tls_client_option()].

connect(Socket, SslOptions) ->
    connect(Socket, SslOptions, infinity).

-spec connect(TCPSocket, TLSOptions, Timeout) ->
                     {ok, sslsocket()} | {error, reason()} when
      TCPSocket :: socket(),
      TLSOptions :: [tls_client_option()],
      Timeout :: timeout();
             (Host, Port, TLSOptions) ->
                     {ok, sslsocket()} |
                     {ok, sslsocket(),Ext :: protocol_extensions()} |
                     {error, reason()} |
                     {option_not_a_key_value_tuple, any()} when
      Host :: host(),
      Port :: inet:port_number(),
      TLSOptions :: [tls_client_option()].

connect(Socket, SslOptions0, Timeout) when is_list(SslOptions0) andalso 
                                           (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    
    try
        CbInfo = handle_option_cb_info(SslOptions0, tls),
        Transport = element(1, CbInfo),
        {ok, Config} = handle_options(Transport, Socket, SslOptions0, client, undefined),
        tls_socket:upgrade(Socket, Config, Timeout)
    catch
        _:{error, Reason} ->
            {error, Reason}
    end; 
connect(Host, Port, Options) ->
    connect(Host, Port, Options, infinity).

-spec connect(Host, Port, TLSOptions, Timeout) ->
                     {ok, sslsocket()} |
                     {ok, sslsocket(),Ext :: protocol_extensions()} |
                     {error, reason()} |
                     {option_not_a_key_value_tuple, any()} when
      Host :: host(),
      Port :: inet:port_number(),
      TLSOptions :: [tls_client_option()],
      Timeout :: timeout().

connect(Host, Port, Options, Timeout) when (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    try
	{ok, Config} = handle_options(Options, client, Host),
	case Config#config.connection_cb of
	    tls_gen_connection ->
		tls_socket:connect(Host,Port,Config,Timeout);
	    dtls_gen_connection ->
		dtls_socket:connect(Host,Port,Config,Timeout)
	end
    catch
	throw:Error ->
	    Error
    end.

%%--------------------------------------------------------------------
-spec listen(Port, Options) -> {ok, ListenSocket} | {error, reason()} when
      Port::inet:port_number(),
      Options::[tls_server_option()],
      ListenSocket :: sslsocket().

%%
%% Description: Creates an ssl listen socket.
%%--------------------------------------------------------------------
listen(_Port, []) ->
    {error, nooptions};
listen(Port, Options0) ->
    try
	{ok, Config} = handle_options(Options0, server, undefined),
        do_listen(Port, Config, Config#config.connection_cb)
    catch
	Error = {error, _} ->
	    Error
    end.
%%--------------------------------------------------------------------
%%
%% Description: Performs transport accept on an ssl listen socket
%%--------------------------------------------------------------------
-spec transport_accept(ListenSocket) -> {ok, SslSocket} |
					{error, reason()} when
      ListenSocket :: sslsocket(),
      SslSocket :: sslsocket().

transport_accept(ListenSocket) ->
    transport_accept(ListenSocket, infinity).

-spec transport_accept(ListenSocket, Timeout) -> {ok, SslSocket} |
					{error, reason()} when
      ListenSocket :: sslsocket(),
      Timeout :: timeout(),
      SslSocket :: sslsocket().

transport_accept(#sslsocket{pid = {ListenSocket,
				   #config{connection_cb = ConnectionCb} = Config}}, Timeout) 
  when (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    case ConnectionCb of
	tls_gen_connection ->
	    tls_socket:accept(ListenSocket, Config, Timeout);
	dtls_gen_connection ->
	    dtls_socket:accept(ListenSocket, Config, Timeout)
    end.
  
%%--------------------------------------------------------------------
%%
%% Description: Performs accept on an ssl listen socket. e.i. performs
%%              ssl handshake.
%%--------------------------------------------------------------------

%% Performs the SSL/TLS/DTLS server-side handshake.
-spec handshake(HsSocket) -> {ok, SslSocket} | {ok, SslSocket, Ext} | {error, Reason} when
      HsSocket :: sslsocket(),
      SslSocket :: sslsocket(),
      Ext :: protocol_extensions(),
      Reason :: closed | timeout | error_alert().

handshake(ListenSocket) ->
    handshake(ListenSocket, infinity).

-spec handshake(HsSocket, Timeout) -> {ok, SslSocket} | {ok, SslSocket, Ext} | {error, Reason} when
      HsSocket :: sslsocket(),
      Timeout :: timeout(),
      SslSocket :: sslsocket(),
      Ext :: protocol_extensions(),
      Reason :: closed | timeout | error_alert();
               (Socket, Options) -> {ok, SslSocket} | {ok, SslSocket, Ext} | {error, Reason} when
      Socket :: socket() | sslsocket(),
      SslSocket :: sslsocket(),
      Options :: [server_option()],
      Ext :: protocol_extensions(),
      Reason :: closed | timeout | error_alert().

handshake(#sslsocket{} = Socket, Timeout) when  (is_integer(Timeout) andalso Timeout >= 0) or 
                                                (Timeout == infinity) ->
    ssl_gen_statem:handshake(Socket, Timeout);

%% If Socket is a ordinary socket(): upgrades a gen_tcp, or equivalent, socket to
%% an SSL socket, that is, performs the SSL/TLS server-side handshake and returns
%% the SSL socket.
%%
%% If Socket is an sslsocket(): provides extra SSL/TLS/DTLS options to those
%% specified in ssl:listen/2 and then performs the SSL/TLS/DTLS handshake.
handshake(ListenSocket, SslOptions) ->
    handshake(ListenSocket, SslOptions, infinity).
-spec handshake(Socket, Options, Timeout) ->
                       {ok, SslSocket} |
                       {ok, SslSocket, Ext} |
                       {error, Reason} when
      Socket :: socket() | sslsocket(),
      SslSocket :: sslsocket(),
      Options :: [server_option()],
      Timeout :: timeout(),
      Ext :: protocol_extensions(),
      Reason :: closed | timeout | {options, any()} | error_alert().

handshake(#sslsocket{} = Socket, [], Timeout) when (is_integer(Timeout) andalso Timeout >= 0) or 
                                                   (Timeout == infinity)->
    handshake(Socket, Timeout);
handshake(#sslsocket{fd = {_, _, _, Trackers}} = Socket, SslOpts, Timeout) when
      (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity)->
    try
        Tracker = proplists:get_value(option_tracker, Trackers),
	{ok, EmOpts, _} = tls_socket:get_all_opts(Tracker),
	ssl_gen_statem:handshake(Socket, {SslOpts,
					  tls_socket:emulated_socket_options(EmOpts, #socket_options{})}, Timeout)
    catch
	Error = {error, _Reason} -> Error
    end;
handshake(#sslsocket{pid = [Pid|_], fd = {_, _, _}} = Socket, SslOpts, Timeout) when
      (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity)->
    try
        {ok, EmOpts, _} = dtls_packet_demux:get_all_opts(Pid),
	ssl_gen_statem:handshake(Socket, {SslOpts,
                                          tls_socket:emulated_socket_options(EmOpts, #socket_options{})}, Timeout)
    catch
	Error = {error, _Reason} -> Error
    end;
handshake(Socket, SslOptions, Timeout) when (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    try
        CbInfo = handle_option_cb_info(SslOptions, tls),
        Transport = element(1, CbInfo),
        ConnetionCb = connection_cb(SslOptions),
        {ok, #config{transport_info = CbInfo, ssl = SslOpts, emulated = EmOpts}} =
            handle_options(Transport, Socket, SslOptions, server, undefined),
        ok = tls_socket:setopts(Transport, Socket, tls_socket:internal_inet_values()),
        {ok, Port} = tls_socket:port(Transport, Socket),
        {ok, SessionIdHandle} = tls_socket:session_id_tracker(ssl_unknown_listener, SslOpts),
        ssl_gen_statem:handshake(ConnetionCb, Port, Socket,
                                 {SslOpts, 
                                  tls_socket:emulated_socket_options(EmOpts, #socket_options{}),
                                  [{session_id_tracker, SessionIdHandle}]},
                                 self(), CbInfo, Timeout)
    catch
        Error = {error, _Reason} -> Error
    end.   

%%--------------------------------------------------------------------
-spec handshake_continue(HsSocket, Options) ->
                                {ok, SslSocket} | {error, Reason} when
      HsSocket :: sslsocket(),
      Options :: [tls_client_option() | tls_server_option()],
      SslSocket :: sslsocket(),
      Reason :: closed | timeout | error_alert().
%%
%%
%% Description: Continues the handshake possible with newly supplied options.
%%--------------------------------------------------------------------
handshake_continue(Socket, SSLOptions) ->
    handshake_continue(Socket, SSLOptions, infinity).
%%--------------------------------------------------------------------
-spec handshake_continue(HsSocket, Options, Timeout) ->
                                {ok, SslSocket} | {error, Reason} when
      HsSocket :: sslsocket(),
      Options :: [tls_client_option() | tls_server_option()],
      Timeout :: timeout(),
      SslSocket :: sslsocket(),
      Reason :: closed | timeout | error_alert().
%%
%%
%% Description: Continues the handshake possible with newly supplied options.
%%--------------------------------------------------------------------
handshake_continue(Socket, SSLOptions, Timeout) ->
    ssl_gen_statem:handshake_continue(Socket, SSLOptions, Timeout).
%%--------------------------------------------------------------------
-spec  handshake_cancel(#sslsocket{}) -> any().
%%
%% Description: Cancels the handshakes sending a close alert.
%%--------------------------------------------------------------------
handshake_cancel(Socket) ->
    ssl_gen_statem:handshake_cancel(Socket).

%%--------------------------------------------------------------------
-spec  close(SslSocket) -> ok | {error, Reason} when
      SslSocket :: sslsocket(),
      Reason :: any().
%%
%% Description: Close an ssl connection
%%--------------------------------------------------------------------
close(#sslsocket{pid = [Pid|_]}) when is_pid(Pid) ->
    ssl_gen_statem:close(Pid, {close, ?DEFAULT_TIMEOUT});
close(#sslsocket{pid = {dtls, #config{dtls_handler = {_, _}}}} = DTLSListen) ->
    dtls_socket:close(DTLSListen);
close(#sslsocket{pid = {ListenSocket, #config{transport_info={Transport,_,_,_,_}}}}) ->
    Transport:close(ListenSocket).

%%--------------------------------------------------------------------
-spec  close(SslSocket, How) -> ok | {ok, port()} | {ok, port(), Data} | {error,Reason} when
      SslSocket :: sslsocket(),
      How :: timeout() | {NewController::pid(), timeout()},
      Data :: binary(),
      Reason :: any().
%%
%% Description: Close an ssl connection
%%--------------------------------------------------------------------
close(#sslsocket{pid = [TLSPid|_]},
      {Pid, Timeout} = DownGrade) when is_pid(TLSPid),
				       is_pid(Pid),
				       (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    case ssl_gen_statem:close(TLSPid, {close, DownGrade}) of
        ok -> %% In normal close {error, closed} is regarded as ok, as it is not interesting which side
            %% that got to do the actual close. But in the downgrade case only {ok, Port} is a success.
            {error, closed};
        Other ->
            Other
    end;
close(#sslsocket{pid = [TLSPid|_]}, Timeout) when is_pid(TLSPid),
					      (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    ssl_gen_statem:close(TLSPid, {close, Timeout});
close(#sslsocket{pid = {dtls, #config{dtls_handler = {_, _}}}} = DTLSListen, _) ->
    dtls_socket:close(DTLSListen);
close(#sslsocket{pid = {ListenSocket, #config{transport_info={Transport,_,_,_,_}}}}, _) ->
    tls_socket:close(Transport, ListenSocket).

%%--------------------------------------------------------------------
-spec send(SslSocket, Data) -> ok | {error, reason()} when
      SslSocket :: sslsocket(),
      Data :: iodata().
%%
%% Description: Sends data over the ssl connection
%%--------------------------------------------------------------------
send(#sslsocket{pid = [Pid]}, Data) when is_pid(Pid) ->
    ssl_gen_statem:send(Pid, Data);
send(#sslsocket{pid = [_, Pid]}, Data) when is_pid(Pid) ->
    tls_sender:send_data(Pid,  erlang:iolist_to_iovec(Data));
send(#sslsocket{pid = {_, #config{transport_info={_, udp, _, _}}}}, _) ->
    {error,enotconn}; %% Emulate connection behaviour
send(#sslsocket{pid = {dtls,_}}, _) ->
    {error,enotconn};  %% Emulate connection behaviour
send(#sslsocket{pid = {ListenSocket, #config{transport_info = Info}}}, Data) ->
    Transport = element(1, Info),
    tls_socket:send(Transport, ListenSocket, Data). %% {error,enotconn}

%%--------------------------------------------------------------------
%%
%% Description: Receives data when active = false
%%--------------------------------------------------------------------
-spec recv(SslSocket, Length) -> {ok, Data} | {error, reason()} when
      SslSocket :: sslsocket(),
      Length :: non_neg_integer(),
      Data :: binary() | list() | HttpPacket,
      HttpPacket :: any().

recv(Socket, Length) ->
    recv(Socket, Length, infinity).

-spec recv(SslSocket, Length, Timeout) -> {ok, Data} | {error, reason()} when
      SslSocket :: sslsocket(),
      Length :: non_neg_integer(),
      Data :: binary() | list() | HttpPacket,
      Timeout :: timeout(),
      HttpPacket :: any().

recv(#sslsocket{pid = [Pid|_]}, Length, Timeout)
  when is_pid(Pid) andalso
       (is_integer(Length) andalso Length >= 0) andalso
       ((is_integer(Timeout) andalso Timeout >= 0) orelse Timeout == infinity) ->
    ssl_gen_statem:recv(Pid, Length, Timeout);
recv(#sslsocket{pid = {dtls,_}}, _, _) ->
    {error,enotconn};
recv(#sslsocket{pid = {Listen,
		       #config{transport_info = Info}}},_,_) ->
    Transport = element(1, Info),
    Transport:recv(Listen, 0). %% {error,enotconn}

%%--------------------------------------------------------------------
-spec controlling_process(SslSocket, NewOwner) -> ok | {error, Reason} when
      SslSocket :: sslsocket(),
      NewOwner :: pid(),
      Reason :: any().
%%
%% Description: Changes process that receives the messages when active = true
%% or once.
%%--------------------------------------------------------------------
controlling_process(#sslsocket{pid = [Pid|_]}, NewOwner) when is_pid(Pid), is_pid(NewOwner) ->
    ssl_gen_statem:new_user(Pid, NewOwner);
controlling_process(#sslsocket{pid = {dtls, _}},
		    NewOwner) when is_pid(NewOwner) ->
    ok; %% Meaningless but let it be allowed to conform with TLS 
controlling_process(#sslsocket{pid = {Listen,
				      #config{transport_info = {Transport,_,_,_,_}}}},
		    NewOwner) when is_pid(NewOwner) ->
    %% Meaningless but let it be allowed to conform with normal sockets
    Transport:controlling_process(Listen, NewOwner).

%%--------------------------------------------------------------------
-spec connection_information(SslSocket) -> {ok, Result} | {error, reason()} when
      SslSocket :: sslsocket(),
      Result :: connection_info().
%%
%% Description: Return SSL information for the connection
%%--------------------------------------------------------------------
connection_information(#sslsocket{pid = [Pid|_]}) when is_pid(Pid) -> 
    case ssl_gen_statem:connection_information(Pid, false) of
	{ok, Info} ->
	    {ok, [Item || Item = {_Key, Value} <- Info,  Value =/= undefined]};
	Error ->
            Error
    end;
connection_information(#sslsocket{pid = {_Listen, #config{}}}) ->
    {error, enotconn}.
%%--------------------------------------------------------------------
-spec connection_information(SslSocket, Items) -> {ok, Result} | {error, reason()} when
      SslSocket :: sslsocket(),
      Items :: connection_info_items(),
      Result :: connection_info().
%%
%% Description: Return SSL information for the connection
%%--------------------------------------------------------------------
connection_information(#sslsocket{pid = [Pid|_]}, Items) when is_pid(Pid) -> 
    case ssl_gen_statem:connection_information(Pid, include_security_info(Items)) of
        {ok, Info} ->
            {ok, [Item || Item = {Key, Value} <- Info,  lists:member(Key, Items),
			  Value =/= undefined]};
	Error ->
            Error
    end.

%%--------------------------------------------------------------------
-spec peername(SslSocket) -> {ok, {Address, Port}} |
                             {error, reason()} when
      SslSocket :: sslsocket(),
      Address :: inet:ip_address(),
      Port :: inet:port_number().
%%
%% Description: same as inet:peername/1.
%%--------------------------------------------------------------------
peername(#sslsocket{pid = [Pid|_], fd = {Transport, Socket,_}}) when is_pid(Pid)->
    dtls_socket:peername(Transport, Socket);
peername(#sslsocket{pid = [Pid|_], fd = {Transport, Socket,_,_}}) when is_pid(Pid)->
    tls_socket:peername(Transport, Socket);
peername(#sslsocket{pid = {dtls, #config{dtls_handler = {_Pid,_}}}}) ->
    dtls_socket:peername(dtls, undefined);
peername(#sslsocket{pid = {ListenSocket,  #config{transport_info = {Transport,_,_,_,_}}}}) ->
    tls_socket:peername(Transport, ListenSocket); %% Will return {error, enotconn}
peername(#sslsocket{pid = {dtls,_}}) ->
    {error,enotconn}.

%%--------------------------------------------------------------------
-spec peercert(SslSocket) -> {ok, Cert} | {error, reason()} when
      SslSocket :: sslsocket(),
      Cert :: public_key:der_encoded().
%%
%% Description: Returns the peercert.
%%--------------------------------------------------------------------
peercert(#sslsocket{pid = [Pid|_]}) when is_pid(Pid) ->
    case ssl_gen_statem:peer_certificate(Pid) of
	{ok, undefined} ->
	    {error, no_peercert};
        Result ->
	    Result
    end;
peercert(#sslsocket{pid = {dtls, _}}) ->
    {error, enotconn};
peercert(#sslsocket{pid = {_Listen, #config{}}}) ->
    {error, enotconn}.

%%--------------------------------------------------------------------
-spec negotiated_protocol(SslSocket) -> {ok, Protocol} | {error, Reason} when
      SslSocket :: sslsocket(),
      Protocol :: binary(),
      Reason :: protocol_not_negotiated | closed.
%%
%% Description: Returns the protocol that has been negotiated. If no
%% protocol has been negotiated will return {error, protocol_not_negotiated}
%%--------------------------------------------------------------------
negotiated_protocol(#sslsocket{pid = [Pid|_]}) when is_pid(Pid) ->
    ssl_gen_statem:negotiated_protocol(Pid).

%%--------------------------------------------------------------------
-spec cipher_suites(Description, Version) -> ciphers() when
      Description :: default | all | exclusive | anonymous | exclusive_anonymous,
      Version :: protocol_version() | ssl_record:ssl_version().

%% Description: Returns all default and all supported cipher suites for a
%% TLS/DTLS version
%%--------------------------------------------------------------------
cipher_suites(Description, Version) when Version == 'tlsv1.3';
                                  Version == 'tlsv1.2';
                                  Version == 'tlsv1.1';
                                  Version == tlsv1 ->
    cipher_suites(Description, tls_record:protocol_version_name(Version));
cipher_suites(Description, Version)  when Version == 'dtlsv1.2';
                                   Version == 'dtlsv1'->
    cipher_suites(Description, dtls_record:protocol_version_name(Version));
cipher_suites(Description, Version) ->
    [ssl_cipher_format:suite_bin_to_map(Suite) || Suite <- supported_suites(Description, Version)].

%%--------------------------------------------------------------------
-spec cipher_suites(Description, Version, rfc | openssl) -> [string()] when
      Description :: default | all | exclusive | anonymous,
      Version :: protocol_version() | ssl_record:ssl_version().

%% Description: Returns all default and all supported cipher suites for a
%% TLS/DTLS version
%%--------------------------------------------------------------------
cipher_suites(Description, Version, StringType) when  Version == 'tlsv1.3';
                                               Version == 'tlsv1.2';
                                               Version == 'tlsv1.1';
                                               Version == tlsv1 ->
    cipher_suites(Description, tls_record:protocol_version_name(Version), StringType);
cipher_suites(Description, Version, StringType)  when Version == 'dtlsv1.2';
                                               Version == 'dtlsv1'->
    cipher_suites(Description, dtls_record:protocol_version_name(Version), StringType);
cipher_suites(Description, Version, rfc) ->
    [ssl_cipher_format:suite_map_to_str(ssl_cipher_format:suite_bin_to_map(Suite))
     || Suite <- supported_suites(Description, Version)];
cipher_suites(Description, Version, openssl) ->
    [ssl_cipher_format:suite_map_to_openssl_str(ssl_cipher_format:suite_bin_to_map(Suite))
     || Suite <- supported_suites(Description, Version)].

%%--------------------------------------------------------------------
-spec filter_cipher_suites(Suites, Filters) -> Ciphers when
      Suites :: ciphers(),
      Filters :: cipher_filters(),
      Ciphers :: ciphers().

%% Description: Removes cipher suites if any of the filter functions returns false
%% for any part of the cipher suite. This function also calls default filter functions
%% to make sure the cipher suite are supported by crypto.
%%--------------------------------------------------------------------
filter_cipher_suites(Suites, Filters0) ->
    #{key_exchange_filters := KexF,
      cipher_filters := CipherF,
      mac_filters := MacF,
      prf_filters := PrfF}
        = ssl_cipher:crypto_support_filters(),
    Filters = #{key_exchange_filters => add_filter(proplists:get_value(key_exchange, Filters0), KexF),
                cipher_filters => add_filter(proplists:get_value(cipher, Filters0), CipherF),
                mac_filters => add_filter(proplists:get_value(mac, Filters0), MacF),
                prf_filters => add_filter(proplists:get_value(prf, Filters0), PrfF)},
    ssl_cipher:filter_suites(Suites, Filters).
%%--------------------------------------------------------------------
-spec prepend_cipher_suites(Preferred, Suites) -> ciphers() when
      Preferred :: ciphers() | cipher_filters(),
      Suites :: ciphers().

%% Description: Make <Preferred> suites become the most preferred
%%      suites that is put them at the head of the cipher suite list
%%      and remove them from <Suites> if present. <Preferred> may be a
%%      list of cipher suites or a list of filters in which case the
%%      filters are use on Suites to extract the the preferred
%%      cipher list.
%% --------------------------------------------------------------------
prepend_cipher_suites([First | _] = Preferred, Suites0) when is_map(First) ->
    Suites = Preferred ++ (Suites0 -- Preferred),
    Suites;
prepend_cipher_suites(Filters, Suites) ->
    Preferred = filter_cipher_suites(Suites, Filters), 
    Preferred ++ (Suites -- Preferred).
%%--------------------------------------------------------------------
-spec append_cipher_suites(Deferred, Suites) -> ciphers() when
      Deferred :: ciphers() | cipher_filters(),
      Suites :: ciphers().

%% Description: Make <Deferred> suites suites become the 
%% least preferred suites that is put them at the end of the cipher suite list
%% and removed them from <Suites> if present.
%%
%%--------------------------------------------------------------------
append_cipher_suites([First | _] = Deferred, Suites0) when is_map(First)->
    Suites = (Suites0 -- Deferred) ++ Deferred,
    Suites;
append_cipher_suites(Filters, Suites) ->
    Deferred = filter_cipher_suites(Suites, Filters), 
    (Suites -- Deferred) ++  Deferred.

%%--------------------------------------------------------------------
-spec signature_algs(Description, Version) -> signature_algs() when
      Description :: default | all | exclusive,
      Version :: protocol_version().

%% Description: Returns possible signature algorithms/schemes
%% for TLS/DTLS version
%%--------------------------------------------------------------------

signature_algs(default, 'tlsv1.3') ->
    tls_v1:default_signature_algs([tls_record:protocol_version_name('tlsv1.3'), 
                                   tls_record:protocol_version_name('tlsv1.2')]);
signature_algs(default, 'tlsv1.2') ->
    tls_v1:default_signature_algs([tls_record:protocol_version_name('tlsv1.2')]);
signature_algs(all, 'tlsv1.3') ->
    tls_v1:default_signature_algs([tls_record:protocol_version_name('tlsv1.3'),
                                   tls_record:protocol_version_name('tlsv1.2')]) ++
        [ecdsa_sha1, rsa_pkcs1_sha1 | tls_v1:legacy_signature_algs_pre_13()] -- [{sha, ecdsa}, {sha, rsa}];
signature_algs(all, 'tlsv1.2') ->
    tls_v1:default_signature_algs([tls_record:protocol_version_name('tlsv1.2')]) ++ 
        tls_v1:legacy_signature_algs_pre_13();
signature_algs(exclusive, 'tlsv1.3') ->
    tls_v1:default_signature_algs([tls_record:protocol_version_name('tlsv1.3')]);
signature_algs(exclusive, 'tlsv1.2') ->
    Algs = tls_v1:default_signature_algs([tls_record:protocol_version_name('tlsv1.2')]),
    Algs ++ tls_v1:legacy_signature_algs_pre_13();
signature_algs(Description, 'dtlsv1.2') ->
    signature_algs(Description, 'tlsv1.2');
signature_algs(Description, Version) when Description == default;
                                          Description == all;
                                          Description == exclusive->
    erlang:error({signature_algs_not_supported_in_protocol_version, Version});
signature_algs(Description, Version) ->
    erlang:error(badarg, [Description, Version]).

%%--------------------------------------------------------------------
-spec eccs() -> NamedCurves when
      NamedCurves :: [named_curve()].
%%--------------------------------------------------------------------
eccs() ->
    tls_v1:ec_curves(all, 'tlsv1.2').

%%--------------------------------------------------------------------
-spec eccs(Version) -> NamedCurves when
      Version :: 'tlsv1.2' | 'tlsv1.1' | 'tlsv1' | 'dtlsv1.2' | 'dtlsv1',
      NamedCurves :: [named_curve()].
%%--------------------------------------------------------------------
eccs('dtlsv1') ->
    eccs('tlsv1.1');
eccs('dtlsv1.2') ->
    eccs('tlsv1.2');
eccs(Version) when Version == 'tlsv1.2';
                   Version == 'tlsv1.1';
                   Version == tlsv1 ->
    tls_v1:ec_curves(default, Version);
eccs('tlsv1.3') ->
    erlang:error({badarg, not_sup_in, 'tlsv1.3'});
eccs(Other) ->
    erlang:error({badarg, Other}).

%%--------------------------------------------------------------------
-spec groups() -> [group()].
%% Description: returns all supported groups (TLS 1.3 and later)
%%--------------------------------------------------------------------
groups() ->
    tls_v1:groups().

%%--------------------------------------------------------------------
-spec groups(default) -> [group()].
%% Description: returns the default groups (TLS 1.3 and later)
%%--------------------------------------------------------------------
groups(default) ->
    tls_v1:default_groups().

%%--------------------------------------------------------------------
-spec getopts(SslSocket, OptionNames) ->
		     {ok, [gen_tcp:option()]} | {error, reason()} when
      SslSocket :: sslsocket(),
      OptionNames :: [gen_tcp:option_name()].
%%
%% Description: Gets options
%%--------------------------------------------------------------------
getopts(#sslsocket{pid = [Pid|_]}, OptionTags) when is_pid(Pid), is_list(OptionTags) ->
    ssl_gen_statem:get_opts(Pid, OptionTags);
getopts(#sslsocket{pid = {dtls, #config{transport_info = {Transport,_,_,_,_}}}} = ListenSocket, OptionTags) when is_list(OptionTags) ->
    try dtls_socket:getopts(Transport, ListenSocket, OptionTags) of
        {ok, _} = Result ->
            Result;
	{error, InetError} ->
	    {error, {options, {socket_options, OptionTags, InetError}}}
    catch
	_:Error ->
	    {error, {options, {socket_options, OptionTags, Error}}}
    end;
getopts(#sslsocket{pid = {_,  #config{transport_info = {Transport,_,_,_,_}}}} = ListenSocket,
	OptionTags) when is_list(OptionTags) ->
    try tls_socket:getopts(Transport, ListenSocket, OptionTags) of
	{ok, _} = Result ->
	    Result;
	{error, InetError} ->
	    {error, {options, {socket_options, OptionTags, InetError}}}
    catch
	_:Error ->
	    {error, {options, {socket_options, OptionTags, Error}}}
    end;
getopts(#sslsocket{}, OptionTags) ->
    {error, {options, {socket_options, OptionTags}}}.

%%--------------------------------------------------------------------
-spec setopts(SslSocket, Options) -> ok | {error, reason()} when
      SslSocket :: sslsocket(),
      Options :: [gen_tcp:option()].
%%
%% Description: Sets options
%%--------------------------------------------------------------------
setopts(#sslsocket{pid = [Pid, Sender]}, Options0) when is_pid(Pid), is_list(Options0)  ->
    try proplists:expand([{binary, [{mode, binary}]},
			  {list, [{mode, list}]}], Options0) of
        Options ->
            case proplists:get_value(packet, Options, undefined) of
                undefined ->
                    ssl_gen_statem:set_opts(Pid, Options);
                PacketOpt ->
                    case tls_sender:setopts(Sender, [{packet, PacketOpt}]) of
                        ok ->
                            ssl_gen_statem:set_opts(Pid, Options);
                        Error ->
                            Error
                    end
            end
    catch
        _:_ ->
            {error, {options, {not_a_proplist, Options0}}}
    end;
setopts(#sslsocket{pid = [Pid|_]}, Options0) when is_pid(Pid), is_list(Options0)  ->
    try proplists:expand([{binary, [{mode, binary}]},
			  {list, [{mode, list}]}], Options0) of
	Options ->
	    ssl_gen_statem:set_opts(Pid, Options)
    catch
	_:_ ->
	    {error, {options, {not_a_proplist, Options0}}}
    end;
setopts(#sslsocket{pid = {dtls, #config{transport_info = {Transport,_,_,_,_}}}} = ListenSocket, Options) when is_list(Options) ->
    try dtls_socket:setopts(Transport, ListenSocket, Options) of
	ok ->
	    ok;
	{error, InetError} ->
	    {error, {options, {socket_options, Options, InetError}}}
    catch
	_:Error ->
	    {error, {options, {socket_options, Options, Error}}}
    end;
setopts(#sslsocket{pid = {_, #config{transport_info = {Transport,_,_,_,_}}}} = ListenSocket, Options) when is_list(Options) ->
    try tls_socket:setopts(Transport, ListenSocket, Options) of
	ok ->
	    ok;
	{error, InetError} ->
	    {error, {options, {socket_options, Options, InetError}}}
    catch
	_:Error ->
	    {error, {options, {socket_options, Options, Error}}}
    end;
setopts(#sslsocket{}, Options) ->
    {error, {options,{not_a_proplist, Options}}}.

%%---------------------------------------------------------------
-spec getstat(SslSocket) ->
                     {ok, OptionValues} | {error, inet:posix()} when
      SslSocket :: sslsocket(),
      OptionValues :: [{inet:stat_option(), integer()}].
%%
%% Description: Get all statistic options for a socket.
%%--------------------------------------------------------------------
getstat(Socket) ->
	getstat(Socket, inet:stats()).

%%---------------------------------------------------------------
-spec getstat(SslSocket, Options) ->
                     {ok, OptionValues} | {error, inet:posix()} when
      SslSocket :: sslsocket(),
      Options :: [inet:stat_option()],
      OptionValues :: [{inet:stat_option(), integer()}].
%%
%% Description: Get one or more statistic options for a socket.
%%--------------------------------------------------------------------
getstat(#sslsocket{pid = {dtls, #config{transport_info = Info,
                                        dtls_handler = {Listener, _}}}},
        Options) when is_list(Options) ->
    Transport = element(1, Info),
    dtls_socket:getstat(Transport, Listener, Options);
getstat(#sslsocket{pid = {Listen,  #config{transport_info = Info}}},
        Options) when is_list(Options) ->
    Transport = element(1, Info),
    tls_socket:getstat(Transport, Listen, Options);
getstat(#sslsocket{pid = [Pid|_], fd = {Transport, Socket, _, _}},
        Options) when is_pid(Pid), is_list(Options) ->
    tls_socket:getstat(Transport, Socket, Options);
getstat(#sslsocket{pid = [Pid|_], fd = {Transport, Socket, _}},
        Options) when is_pid(Pid), is_list(Options) ->
    dtls_socket:getstat(Transport, Socket, Options).

%%---------------------------------------------------------------
-spec shutdown(SslSocket, How) ->  ok | {error, reason()} when
      SslSocket :: sslsocket(),
      How :: read | write | read_write.
%%
%% Description: Same as gen_tcp:shutdown/2
%%--------------------------------------------------------------------
shutdown(#sslsocket{pid = {dtls, #config{transport_info = Info}}}, _) ->
    Transport = element(1, Info),
    %% enotconn is what gen_tcp:shutdown on a listen socket will result with.
    %% shutdown really is handling TCP functionality not present
    %% with gen_udp or gen_sctp, but if a callback wrapper is supplied let
    %% the error be the same as for gen_tcp as a wrapper could have
    %% supplied it own logic and this is backwards compatible.
    case Transport of
        gen_udp ->
            {error, notsup};
        gen_sctp ->
            {error, notsup};
        _  ->
            {error, enotconn}
    end;
shutdown(#sslsocket{pid = {Listen, #config{transport_info = Info}}}, How) ->
    Transport = element(1, Info),
    Transport:shutdown(Listen, How);    
shutdown(#sslsocket{pid = [Pid|_]}, How) when is_pid(Pid) ->
    ssl_gen_statem:shutdown(Pid, How).

%%--------------------------------------------------------------------
-spec sockname(SslSocket) ->
                      {ok, {Address, Port}} | {error, reason()} when
      SslSocket :: sslsocket(),
      Address :: inet:ip_address(),
      Port :: inet:port_number().
%%
%% Description: Same as inet:sockname/1
%%--------------------------------------------------------------------
sockname(#sslsocket{pid = {dtls, #config{dtls_handler = {Pid, _}}}}) ->
    dtls_packet_demux:sockname(Pid);
sockname(#sslsocket{pid = {Listen,  #config{transport_info = Info}}}) ->
    Transport = element(1, Info),
    tls_socket:sockname(Transport, Listen);
sockname(#sslsocket{pid = [Pid|_], fd = {Transport, Socket,_}}) when is_pid(Pid) ->
    dtls_socket:sockname(Transport, Socket);
sockname(#sslsocket{pid = [Pid| _], fd = {Transport, Socket,_,_}}) when is_pid(Pid) ->
    tls_socket:sockname(Transport, Socket).

%%---------------------------------------------------------------
-spec versions() -> [VersionInfo] when
      VersionInfo :: {ssl_app, string()} |
                     {supported | available | implemented, [tls_version()]} |
                     {supported_dtls | available_dtls | implemented_dtls, [dtls_version()]}.
%%
%% Description: Returns a list of relevant versions.
%%--------------------------------------------------------------------
versions() ->
    ConfTLSVsns = tls_record:supported_protocol_versions(),
    ConfDTLSVsns = dtls_record:supported_protocol_versions(),
    ImplementedTLSVsns =  ?ALL_AVAILABLE_VERSIONS,
    ImplementedDTLSVsns = ?ALL_AVAILABLE_DATAGRAM_VERSIONS,

     TLSCryptoSupported = fun(Vsn) -> 
                                  tls_record:sufficient_crypto_support(Vsn)
                          end,
     DTLSCryptoSupported = fun(Vsn) -> 
                                   tls_record:sufficient_crypto_support(dtls_v1:corresponding_tls_version(Vsn))  
                           end,
    SupportedTLSVsns = [tls_record:protocol_version(Vsn) || Vsn <- ConfTLSVsns,  TLSCryptoSupported(Vsn)],
    SupportedDTLSVsns = [dtls_record:protocol_version(Vsn) || Vsn <- ConfDTLSVsns, DTLSCryptoSupported(Vsn)],

    AvailableTLSVsns = [Vsn || Vsn <- ImplementedTLSVsns, TLSCryptoSupported(tls_record:protocol_version_name(Vsn))],
    AvailableDTLSVsns = [Vsn || Vsn <- ImplementedDTLSVsns, DTLSCryptoSupported(dtls_record:protocol_version_name(Vsn))],
                                    
    [{ssl_app, ?VSN}, 
     {supported, SupportedTLSVsns}, 
     {supported_dtls, SupportedDTLSVsns}, 
     {available, AvailableTLSVsns}, 
     {available_dtls, AvailableDTLSVsns},
     {implemented, ImplementedTLSVsns},
     {implemented_dtls, ImplementedDTLSVsns}
    ].

%%---------------------------------------------------------------
-spec renegotiate(SslSocket) -> ok | {error, reason()} when
      SslSocket :: sslsocket().
%%
%% Description: Initiates a renegotiation.
%%--------------------------------------------------------------------
renegotiate(#sslsocket{pid = [Pid, Sender |_]} = Socket) when is_pid(Pid),
                                                     is_pid(Sender) ->
    case ssl:connection_information(Socket, [protocol]) of
        {ok, [{protocol, 'tlsv1.3'}]} ->
            {error, notsup};
        _ ->
            case tls_sender:renegotiate(Sender) of
                {ok, Write} ->
                    tls_dtls_connection:renegotiation(Pid, Write);
                Error ->
                    Error
            end
    end;
renegotiate(#sslsocket{pid = [Pid |_]}) when is_pid(Pid) ->
    tls_dtls_connection:renegotiation(Pid);
renegotiate(#sslsocket{pid = {dtls,_}}) ->
    {error, enotconn};
renegotiate(#sslsocket{pid = {_Listen, #config{}}}) ->
    {error, enotconn}.

%%---------------------------------------------------------------
-spec update_keys(SslSocket, Type) -> ok | {error, reason()} when
      SslSocket :: sslsocket(),
      Type :: write | read_write.
%%
%% Description: Initiate a key update.
%%--------------------------------------------------------------------
update_keys(#sslsocket{pid = [Pid, Sender |_]}, Type0) when is_pid(Pid) andalso
                                                            is_pid(Sender) andalso
                                                            (Type0 =:= write orelse
                                                             Type0 =:= read_write) ->
    Type = case Type0 of
               write ->
                   update_not_requested;
               read_write ->
                   update_requested
           end,
    tls_gen_connection_1_3:send_key_update(Sender, Type);
update_keys(_, Type) ->
    {error, {illegal_parameter, Type}}.

%%--------------------------------------------------------------------
-spec export_key_materials(SslSocket, Labels, Contexts, WantedLengths) ->
                 {ok, ExportKeyMaterials} | {error, reason()} when
      SslSocket :: sslsocket(),
      Labels :: [binary()],
      Contexts :: [binary() | no_context],
      WantedLengths :: [non_neg_integer()],
      ExportKeyMaterials :: [binary()].
%%--------------------------------------------------------------------
export_key_materials(#sslsocket{pid = [Pid|_]}, Labels, Contexts, WantedLengths) when is_pid(Pid) ->
    ssl_gen_statem:call(Pid, {export_key_materials, Labels, Contexts, WantedLengths, true});
export_key_materials(#sslsocket{pid = {_Listen, #config{}}}, _,_,_) ->
    {error, enotconn}.

%%--------------------------------------------------------------------
-spec export_key_materials(SslSocket, Labels, Contexts, WantedLengths, ConsumeSecret) ->
                 {ok, ExportKeyMaterials} | {error, exporter_master_secret_already_consumed | bad_input} when
      SslSocket :: sslsocket(),
      Labels :: [binary()],
      Contexts :: [binary() | no_context],
      WantedLengths :: [non_neg_integer()],
      ConsumeSecret :: boolean(),
      ExportKeyMaterials :: [binary()].
%%--------------------------------------------------------------------
export_key_materials(#sslsocket{pid = [Pid|_]}, Labels, Contexts, WantedLengths, ConsumeSecret) when is_pid(Pid) ->
    ssl_gen_statem:call(Pid, {export_key_materials, Labels, Contexts, WantedLengths, ConsumeSecret});
export_key_materials(#sslsocket{pid = {_Listen, #config{}}}, _,_,_, _) ->
    {error, enotconn}.

%%--------------------------------------------------------------------
-spec prf(SslSocket, Secret, Label, Seed, WantedLength) ->
                 {ok, binary()} | {error, reason()} when
      SslSocket :: sslsocket(),
      Secret :: binary() | 'master_secret',
      Label :: binary(),
      Seed :: [binary() | prf_random()],
      WantedLength :: non_neg_integer().
%%
%% Description: use a ssl sessions TLS PRF to generate key material
%%--------------------------------------------------------------------
prf(#sslsocket{pid = [Pid|_]} = Socket,
    master_secret, Label, [client_random, server_random], WantedLength) when is_pid(Pid) ->
    case export_key_materials(Socket, [Label], [no_context], [WantedLength], true) of
        {ok, [KeyMaterial]} ->
            {ok, KeyMaterial};
        Error ->
            Error
    end;
prf(#sslsocket{pid = [Pid|_]} = Socket,
    master_secret, Label, [client_random, server_random, Context], WantedLength) when is_pid(Pid),
                                                                                      is_binary(Context) ->
    case export_key_materials(Socket, [Label], [Context], [WantedLength], true) of
        {ok, [KeyMaterial]} ->
            {ok, KeyMaterial};
        Error ->
            Error
    end;
prf(#sslsocket{pid = {_Listen, #config{}}}, _,_,_,_) ->
    {error, enotconn};
%% Legacy backwards compatible clause. This makes no sense, was probably added for
%% testing purposes by contributor, but these tests does not really test the correct thing.
prf(Socket, Secret, Label, Context, WantedLength) ->
    {ok, [{selected_cipher_suite, #{prf := PRFAlg}}]} = connection_information(Socket, [selected_cipher_suite]),
    {ok, tls_v1:prf(PRFAlg, Secret, Label, erlang:iolist_to_binary(Context), WantedLength)}.

%%--------------------------------------------------------------------
-spec clear_pem_cache() -> ok.
%%
%% Description: Clear the PEM cache
%%--------------------------------------------------------------------
clear_pem_cache() ->
    ssl_pem_cache:clear().

%%---------------------------------------------------------------
-spec format_error(Reason | {error, Reason}) -> string() when
      Reason :: any().
%%
%% Description: Creates error string.
%%--------------------------------------------------------------------
format_error({error, Reason}) ->
    do_format_error(Reason);
format_error(Reason) ->
    do_format_error(Reason).

tls_version(Version) when ?TLS_1_X(Version) ->
    Version;
tls_version(Version) when ?DTLS_1_X(Version) ->
    dtls_v1:corresponding_tls_version(Version).

%%--------------------------------------------------------------------
-spec suite_to_str(CipherSuite) -> string() when
      CipherSuite :: erl_cipher_suite();
                  (CipherSuite) -> string() when
      %% For internal use!
      CipherSuite :: #{key_exchange := null,
                       cipher := null,
                       mac := null,
                       prf := null}.
%%
%% Description: Return the string representation of a cipher suite.
%%--------------------------------------------------------------------
suite_to_str(Cipher) ->
    ssl_cipher_format:suite_map_to_str(Cipher).

%%--------------------------------------------------------------------
-spec suite_to_openssl_str(CipherSuite) -> string() when
      CipherSuite :: erl_cipher_suite().                
%%
%% Description: Return the string representation of a cipher suite.
%%--------------------------------------------------------------------
suite_to_openssl_str(Cipher) ->
    ssl_cipher_format:suite_map_to_openssl_str(Cipher).

%%
%%--------------------------------------------------------------------
-spec str_to_suite(CipherSuiteName) -> erl_cipher_suite()  | {error, {not_recognized, CipherSuiteName}} when
      CipherSuiteName :: string().
%%
%% Description: Return the map representation of a cipher suite.
%%--------------------------------------------------------------------
str_to_suite(CipherSuiteName) ->
    try
        %% Note in TLS-1.3 OpenSSL conforms to RFC names
        %% so if CipherSuiteName starts with TLS this
        %% function will call ssl_cipher_format:suite_str_to_map
        %% so both RFC names and legacy OpenSSL names of supported
        %% cipher suites will be handled
        ssl_cipher_format:suite_openssl_str_to_map(CipherSuiteName)
    catch
        _:_ ->
            {error, {not_recognized, CipherSuiteName}}
    end.
           
%%%--------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------
supported_suites(exclusive, Version) when ?TLS_1_X(Version) ->
    tls_v1:exclusive_suites(Version);
supported_suites(exclusive, Version) when ?DTLS_1_X(Version) ->
    dtls_v1:exclusive_suites(Version);
supported_suites(default, Version) ->  
    ssl_cipher:suites(Version);
supported_suites(all, Version) ->  
    ssl_cipher:all_suites(Version);
supported_suites(anonymous, Version) ->
    ssl_cipher:anonymous_suites(Version);
supported_suites(exclusive_anonymous, Version) when ?TLS_1_X(Version) ->
    tls_v1:exclusive_anonymous_suites(Version);
supported_suites(exclusive_anonymous, Version) when ?DTLS_1_X(Version) ->
    dtls_v1:exclusive_anonymous_suites(Version).

do_listen(Port, #config{transport_info = {Transport, _, _, _,_}} = Config, tls_gen_connection) ->
    tls_socket:listen(Transport, Port, Config);

do_listen(Port,  Config, dtls_gen_connection) ->
    dtls_socket:listen(Port, Config).

ssl_options() ->
    [
     alpn_advertised_protocols, alpn_preferred_protocols,
     anti_replay,
     beast_mitigation,
     cacertfile, cacerts,
     cert,  certs_keys,certfile,
     certificate_authorities,
     ciphers,
     client_renegotiation,
     cookie,
     crl_cache, crl_check,
     customize_hostname_check,
     depth,
     dh, dhfile,

     early_data,
     eccs,
     erl_dist,
     fail_if_no_peer_cert,
     fallback,
     handshake,
     hibernate_after,
     honor_cipher_order,  honor_ecc_order,
     keep_secrets,
     key, keyfile,
     key_update_at,
     ktls,

     log_level,
     max_handshake_size,
     middlebox_comp_mode,
     max_fragment_length,
     next_protocol_selector,  next_protocols_advertised,
     ocsp_stapling, ocsp_responder_certs, ocsp_nonce,
     padding_check,
     partial_chain,
     password,
     protocol,
     psk_identity,
     receiver_spawn_opts,
     renegotiate_at,
     reuse_session, reuse_sessions,

     secure_renegotiate,
     sender_spawn_opts,
     server_name_indication,
     session_tickets,
     stateless_tickets_seed,
     signature_algs,  signature_algs_cert,
     sni_fun,
     sni_hosts,
     srp_identity,
     supported_groups,
     use_ticket,
     use_srtp,
     user_lookup_fun,
     verify, verify_fun,
     versions
    ].

%% Handle ssl options at handshake, handshake_continue
-spec update_options([any()], client | server, map()) -> map().
update_options(Opts, Role, InheritedSslOpts) when is_map(InheritedSslOpts) ->
    {UserSslOpts, _} = split_options(Opts, ssl_options()),
    process_options(UserSslOpts, InheritedSslOpts, #{role => Role}).

process_options(UserSslOpts, SslOpts0, Env) ->
    %% Reverse option list so we get the last set option if set twice,
    %% users depend on it.
    UserSslOptsMap = proplists:to_map(lists:reverse(UserSslOpts)),
    SslOpts1  = opt_protocol_versions(UserSslOptsMap, SslOpts0, Env),
    SslOpts2  = opt_verification(UserSslOptsMap, SslOpts1, Env),
    SslOpts3  = opt_certs(UserSslOptsMap, SslOpts2, Env),
    SslOpts4  = opt_tickets(UserSslOptsMap, SslOpts3, Env),
    SslOpts5  = opt_ocsp(UserSslOptsMap, SslOpts4, Env),
    SslOpts6  = opt_sni(UserSslOptsMap, SslOpts5, Env),
    SslOpts7  = opt_signature_algs(UserSslOptsMap, SslOpts6, Env),
    SslOpts8  = opt_alpn(UserSslOptsMap, SslOpts7, Env),
    SslOpts9  = opt_mitigation(UserSslOptsMap, SslOpts8, Env),
    SslOpts10 = opt_server(UserSslOptsMap, SslOpts9, Env),
    SslOpts11 = opt_client(UserSslOptsMap, SslOpts10, Env),
    SslOpts12 = opt_renegotiate(UserSslOptsMap, SslOpts11, Env),
    SslOpts13 = opt_reuse_sessions(UserSslOptsMap, SslOpts12, Env),
    SslOpts14 = opt_identity(UserSslOptsMap, SslOpts13, Env),
    SslOpts15 = opt_supported_groups(UserSslOptsMap, SslOpts14, Env),
    SslOpts16 = opt_crl(UserSslOptsMap, SslOpts15, Env),
    SslOpts17 = opt_handshake(UserSslOptsMap, SslOpts16, Env),
    SslOpts18 = opt_use_srtp(UserSslOptsMap, SslOpts17, Env),
    SslOpts = opt_process(UserSslOptsMap, SslOpts18, Env),
    SslOpts.

-spec handle_options([any()], client | server, undefined|host()) -> {ok, #config{}}.
handle_options(Opts, Role, Host) ->
    handle_options(undefined, undefined, Opts, Role, Host).

%% Handle all options in listen, connect and handshake
handle_options(Transport, Socket, Opts0, Role, Host) ->
    {UserSslOptsList, SockOpts0} = split_options(Opts0, ssl_options()),

    Env = #{role => Role, host => Host},
    SslOpts = process_options(UserSslOptsList, #{}, Env),

    %% Handle special options
    #{protocol := Protocol} = SslOpts,
    {Sock, Emulated} = emulated_options(Transport, Socket, Protocol, SockOpts0),
    ConnetionCb = connection_cb(Protocol),
    CbInfo = handle_option_cb_info(Opts0, Protocol),

    {ok, #config{
            ssl = SslOpts,
            emulated = Emulated,
            inet_ssl = Sock,
            inet_user = Sock,
            transport_info = CbInfo,
            connection_cb = ConnetionCb
           }}.


opt_protocol_versions(UserOpts, Opts, Env) ->
    {_, PRC} = get_opt_of(protocol, [tls, dtls], tls, UserOpts, Opts),

    LogLevels = [none, all, emergency, alert, critical, error,
                 warning, notice, info, debug],

    DefaultLevel = case logger:get_module_level(?MODULE) of
                       [] -> notice;
                       [{ssl,Level}] -> Level
                   end,

    {_, LL} = get_opt_of(log_level, LogLevels, DefaultLevel, UserOpts, Opts),

    Opts1 = set_opt_bool(keep_secrets, false, UserOpts, Opts),

    {DistW, Dist} = get_opt_bool(erl_dist, false, UserOpts, Opts1),
    option_incompatible(PRC =:= dtls andalso Dist, [{protocol, PRC}, {erl_dist, Dist}]),
    Opts2 = set_opt_new(DistW, erl_dist, false, Dist, Opts1),

    {KtlsW, Ktls} = get_opt_bool(ktls, false, UserOpts, Opts1),
    option_incompatible(PRC =:= dtls andalso Ktls, [{protocol, PRC}, {ktls, Ktls}]),
    Opts3 = set_opt_new(KtlsW, ktls, false, Ktls, Opts2),

    opt_versions(UserOpts, Opts3#{protocol => PRC, log_level => LL}, Env).

opt_versions(UserOpts, #{protocol := Protocol} = Opts, _Env) ->
    Versions = case get_opt(versions, unbound, UserOpts, Opts) of
                   {default, unbound} -> default_versions(Protocol);
                   {new, Vs} -> validate_versions(Protocol, Vs);
                   {old, Vs} -> Vs
               end,

    {Where, MCM} = get_opt_bool(middlebox_comp_mode, true, UserOpts, Opts),
    assert_version_dep(Where =:= new, middlebox_comp_mode, Versions, ['tlsv1.3']),
    Opts1 = set_opt_new(Where, middlebox_comp_mode, true, MCM, Opts),
    Opts1#{versions => Versions}.

default_versions(tls) ->
    Vsns0 = tls_record:supported_protocol_versions(),
    lists:sort(fun tls_record:is_higher/2, Vsns0);
default_versions(dtls) ->
    Vsns0 = dtls_record:supported_protocol_versions(),
    lists:sort(fun dtls_record:is_higher/2, Vsns0).

validate_versions(tls, Vsns0) ->
    Validate =
        fun(Version) ->
                try tls_record:sufficient_crypto_support(Version) of
                    true -> tls_record:protocol_version_name(Version);
                    false -> option_error(insufficient_crypto_support,
                                          {Version, {versions, Vsns0}})
                catch error:function_clause ->
                        option_error(Version, {versions, Vsns0})
                end
        end,
    Vsns = [Validate(V) || V <- Vsns0],
    tls_validate_version_gap(Vsns0),
    option_error([] =:= Vsns, versions, Vsns0),
    lists:sort(fun tls_record:is_higher/2, Vsns);
validate_versions(dtls, Vsns0) ->
    Validate =
        fun(Version) ->
                try tls_record:sufficient_crypto_support(
                      dtls_v1:corresponding_tls_version(
                        dtls_record:protocol_version_name(Version))) of
                    true -> dtls_record:protocol_version_name(Version);
                    false-> option_error(insufficient_crypto_support,
                                         {Version, {versions, Vsns0}})
                catch error:function_clause ->
                        option_error(Version, {versions, Vsns0})
                end
        end,
    Vsns = [Validate(V) || V <- Vsns0],
    option_error([] =:= Vsns, versions, Vsns0),
    lists:sort(fun dtls_record:is_higher/2, Vsns).

opt_verification(UserOpts, Opts0, #{role := Role} = Env) ->
    {Verify, Opts1} =
        case get_opt_of(verify, [verify_none, verify_peer], default_verify(Role), UserOpts, Opts0) of
            {old, Val} ->
                {Val, Opts0};
            {_, verify_none} ->
                {verify_none, Opts0#{verify => verify_none, verify_fun => {none_verify_fun(), []}}};
            {_, verify_peer} ->
                %% If 'verify' is changed from verify_none to verify_peer, (via update_options/3)
                %% the 'verify_fun' must also be changed to undefined.
                %% i.e remove verify_none fun
                Temp = Opts0#{verify => verify_peer, verify_fun => undefined},
                {verify_peer, maps:remove(fail_if_no_peer_cert, Temp)}
        end,
    Opts2 = opt_cacerts(UserOpts, Opts1, Env),
    {_, PartialChain} = get_opt_fun(partial_chain, 1, fun(_) -> unknown_ca end, UserOpts, Opts2),

    DefFailNoPeer = Role =:= server andalso Verify =:= verify_peer,
    {_, FailNoPeerCert} = get_opt_bool(fail_if_no_peer_cert, DefFailNoPeer, UserOpts, Opts2),
    assert_server_only(Role, FailNoPeerCert, fail_if_no_peer_cert),
    option_incompatible(FailNoPeerCert andalso Verify =:= verify_none,
                        [{verify, verify_none}, {fail_if_no_peer_cert, true}]),

    Opts = set_opt_int(depth, 0, 255, ?DEFAULT_DEPTH, UserOpts, Opts2),

    case Role of
        client ->
            opt_verify_fun(UserOpts, Opts#{partial_chain => PartialChain},
                           Env);
        server ->
            opt_verify_fun(UserOpts, Opts#{partial_chain => PartialChain,
                                           fail_if_no_peer_cert => FailNoPeerCert},
                           Env)
    end.

default_verify(client) ->
    %% Server authenication is by default requiered
    verify_peer;
default_verify(server) ->
    %% Client certification is an optional part of the protocol
    verify_none.

opt_verify_fun(UserOpts, Opts, _Env) ->
    %%DefVerifyNoneFun = {default_verify_fun(), []},
    VerifyFun = case get_opt(verify_fun, undefined, UserOpts, Opts) of
                    {_, {F,_} = FA} when is_function(F, 3); is_function(F, 4) ->
                        FA;
                    {_, UserFun} when is_function(UserFun, 1) ->
                        {convert_verify_fun(), UserFun};
                    {_, undefined} ->
                        undefined;
                    {_, Value} ->
                        option_error(verify_fun, Value)
                end,
    Opts#{verify_fun => VerifyFun}.

none_verify_fun() ->
     fun(_, {bad_cert, _}, UserState) ->
             {valid, UserState};
        (_, {extension, #'Extension'{critical = true}}, UserState) ->
             %% This extension is marked as critical, so
             %% certificate verification should fail if we don't
             %% understand the extension.  However, this is
             %% `verify_none', so let's accept it anyway.
             {valid, UserState};
        (_, {extension, _}, UserState) ->
             {unknown, UserState};
        (_, valid, UserState) ->
            {valid, UserState};
        (_, valid_peer, UserState) ->
             {valid, UserState}
     end.

convert_verify_fun() ->
    fun(_,{bad_cert, _} = Reason, OldFun) ->
            case OldFun([Reason]) of
                true ->  {valid, OldFun};
                false -> {fail, Reason}
            end;
       (_,{extension, _}, UserState) ->
            {unknown, UserState};
       (_, valid, UserState) ->
            {valid, UserState};
       (_, valid_peer, UserState) ->
            {valid, UserState}
    end.

opt_certs(UserOpts, #{log_level := LogLevel} = Opts0, Env) ->
    case get_opt_list(certs_keys, [], UserOpts, Opts0) of
        {Where, []} when Where =/= new ->
            opt_old_certs(UserOpts, #{}, Opts0, Env);
        {old, [CertKey]} ->
            opt_old_certs(UserOpts, CertKey, Opts0, Env);
        {Where, CKs} when is_list(CKs) ->
            warn_override(Where, UserOpts, certs_keys, [cert,certfile,key,keyfile,password], LogLevel),
            Opts0#{certs_keys => [check_cert_key(CK, #{}, LogLevel) || CK <- CKs]}
    end.

opt_old_certs(UserOpts, CertKeys, #{log_level := LogLevel}=SSLOpts, _Env) ->
    CK = check_cert_key(UserOpts, CertKeys, LogLevel),
    case maps:keys(CK) =:= [] of
        true ->
            SSLOpts#{certs_keys => []};
        false ->
            SSLOpts#{certs_keys => [CK]}
    end.

check_cert_key(UserOpts, CertKeys, LogLevel) ->
    CertKeys0 = case get_opt(cert, undefined, UserOpts, CertKeys) of
                    {Where, Cert} when is_binary(Cert) ->
                        warn_override(Where, UserOpts, cert, [certfile], LogLevel),
                        CertKeys#{cert => [Cert]};
                    {Where, [C0|_] = Certs} when is_binary(C0) ->
                        warn_override(Where, UserOpts, cert, [certfile], LogLevel),
                        CertKeys#{cert => Certs};
                    {new, Err0} ->
                        option_error(cert, Err0);
                    {_, undefined} ->
                        case get_opt_file(certfile, unbound, UserOpts, CertKeys) of
                            {default, unbound} -> CertKeys;
                            {_, CertFile} -> CertKeys#{certfile => CertFile}
                        end
                end,

    CertKeys1 = case get_opt(key, undefined, UserOpts, CertKeys) of
                    {_, undefined} ->
                        case get_opt_file(keyfile, <<>>, UserOpts, CertKeys) of
                            {new, KeyFile} ->
                                CertKeys0#{keyfile => KeyFile};
                            {_, <<>>} ->
                                case maps:get(certfile, CertKeys0, unbound) of
                                    unbound -> CertKeys0;
                                    CF -> CertKeys0#{keyfile => CF}
                                end;
                            {old, _} ->
                                CertKeys0
                        end;
                    {_, {KF, K0} = Key}
                      when is_binary(K0), KF =:= rsa; KF =:= dsa;
                           KF == 'RSAPrivateKey'; KF == 'DSAPrivateKey';
                           KF == 'ECPrivateKey'; KF == 'PrivateKeyInfo' ->
                        CertKeys0#{key => Key};
                    {_, #{engine := _, key_id := _, algorithm := _} = Key} ->
                        CertKeys0#{key => Key};
                    {new, Err1} ->
                        option_error(key, Err1)
                end,

    CertKeys2 = case get_opt(password, unbound, UserOpts,CertKeys) of
                    {default, _} -> CertKeys1;
                    {_, Pwd} when is_binary(Pwd); is_list(Pwd) ->
                        CertKeys1#{password => fun() -> Pwd end};
                    {_, Pwd} when is_function(Pwd, 0) ->
                        CertKeys1#{password => Pwd};
                    {_, Err2} ->
                        option_error(password, Err2)
                end,
    CertKeys2.

opt_cacerts(UserOpts, #{verify := Verify, log_level := LogLevel, versions := Versions} = Opts,
            #{role := Role}) ->
    {_, CaCerts} = get_opt_list(cacerts, undefined, UserOpts, Opts),

    CaCertFile = case get_opt_file(cacertfile, <<>>, UserOpts, Opts) of
                     {Where1, _FileName} when CaCerts =/= undefined ->
                         warn_override(Where1, UserOpts, cacerts, [cacertfile], LogLevel),
                         <<>>;
                     {new, FileName} -> unambiguous_path(FileName);
                     {_, FileName} -> FileName
                 end,
    option_incompatible(CaCertFile =:= <<>> andalso CaCerts =:= undefined andalso Verify =:= verify_peer,
                        [{verify, verify_peer}, {cacerts, undefined}]),

    {Where2, CA} = get_opt_bool(certificate_authorities, Role =:= server, UserOpts, Opts),
    assert_version_dep(Where2 =:= new, certificate_authorities, Versions, ['tlsv1.3']),

    Opts1 = set_opt_new(new, cacertfile, <<>>, CaCertFile, Opts),
    Opts2 = set_opt_new(Where2, certificate_authorities, Role =:= server, CA, Opts1),
    Opts2#{cacerts => CaCerts}.

opt_tickets(UserOpts, #{versions := Versions} = Opts, #{role := client}) ->
    {_, SessionTickets} = get_opt_of(session_tickets, [disabled,manual,auto], disabled, UserOpts, Opts),
    assert_version_dep(SessionTickets =/= disabled, session_tickets, Versions, ['tlsv1.3']),

    {_, UseTicket} = get_opt_list(use_ticket, undefined, UserOpts, Opts),
    option_error(UseTicket =:= [], use_ticket, UseTicket),
    option_incompatible(UseTicket =/= undefined andalso SessionTickets =/= manual,
                        [{use_ticket, UseTicket}, {session_tickets, SessionTickets}]),

    {_, EarlyData} = get_opt_bin(early_data, undefined, UserOpts, Opts),
    option_incompatible(is_binary(EarlyData) andalso SessionTickets =:= disabled,
                        [early_data, {session_tickets, disabled}]),
    option_incompatible(is_binary(EarlyData) andalso SessionTickets =:= manual andalso UseTicket =:= undefined,
                        [early_data, {session_tickets, manual}, {use_ticket, undefined}]),

    assert_server_only(anti_replay, UserOpts),
    assert_server_only(stateless_tickets_seed, UserOpts),
    Opts#{session_tickets => SessionTickets, use_ticket => UseTicket, early_data => EarlyData};
opt_tickets(UserOpts, #{versions := Versions} = Opts, #{role := server}) ->
    {_, SessionTickets} =
        get_opt_of(session_tickets,
                   [disabled, stateful, stateless, stateful_with_cert, stateless_with_cert],
                   disabled,
                   UserOpts,
                   Opts),
    assert_version_dep(SessionTickets =/= disabled, session_tickets, Versions, ['tlsv1.3']),

    {_, EarlyData} = get_opt_of(early_data, [enabled, disabled], disabled, UserOpts, Opts),
    option_incompatible(SessionTickets =:= disabled andalso EarlyData =:= enabled,
                        [early_data, {session_tickets, disabled}]),

    Stateless = lists:member(SessionTickets, [stateless, stateless_with_cert]),

    AntiReplay =
        case get_opt(anti_replay, undefined, UserOpts, Opts) of
            {_, undefined} -> undefined;
            {_,AR} when not Stateless ->
                option_incompatible([{anti_replay, AR}, {session_tickets, SessionTickets}]);
            {_,'10k'}  -> {10, 5, 72985};  %% n = 10000 p = 0.030003564 (1 in 33) m = 72985 (8.91KiB) k = 5
            {_,'100k'} -> {10, 5, 729845}; %% n = 10000 p = 0.03000428 (1 in 33) m = 729845 (89.09KiB) k = 5
            {_, {_,_,_} = AR} -> AR;
            {_, AR} -> option_error(anti_replay, AR)
        end,

    {_, STS} = get_opt_bin(stateless_tickets_seed, undefined, UserOpts, Opts),
    option_incompatible(STS =/= undefined andalso not Stateless,
                        [stateless_tickets_seed, {session_tickets, SessionTickets}]),

    assert_client_only(use_ticket, UserOpts),
    Opts#{session_tickets => SessionTickets, early_data => EarlyData,
          anti_replay => AntiReplay, stateless_tickets_seed => STS}.

opt_ocsp(UserOpts, #{versions := _Versions} = Opts, #{role := Role}) ->
    {Stapling, SMap} =
        case get_opt(ocsp_stapling, ?DEFAULT_OCSP_STAPLING, UserOpts, Opts) of
            {old, Map} when is_map(Map) -> {true, Map};
            {_, Bool} when is_boolean(Bool) -> {Bool, #{}};
            {_, Value} -> option_error(ocsp_stapling, Value)
        end,
    assert_client_only(Role, Stapling, ocsp_stapling),
    {_, Nonce} = get_opt_bool(ocsp_nonce, ?DEFAULT_OCSP_NONCE, UserOpts, SMap),
    option_incompatible(Stapling =:= false andalso Nonce =:= false,
                        [{ocsp_nonce, false}, {ocsp_stapling, false}]),
    {_, ORC} = get_opt_list(ocsp_responder_certs, ?DEFAULT_OCSP_RESPONDER_CERTS,
                            UserOpts, SMap),
    CheckBinary = fun(Cert) when is_binary(Cert) -> ok;
                     (_Cert) -> option_error(ocsp_responder_certs, ORC)
                  end,
    [CheckBinary(C) || C <- ORC],
    option_incompatible(Stapling =:= false andalso ORC =/= [],
                        [ocsp_responder_certs, {ocsp_stapling, false}]),
    case Stapling of
        true ->
            Opts#{ocsp_stapling =>
                      #{ocsp_nonce => Nonce,
                        ocsp_responder_certs => ORC}};
        false ->
            Opts
    end.

opt_sni(UserOpts, #{versions := _Versions} = Opts, #{role := server}) ->
    {_, SniHosts} = get_opt_list(sni_hosts, [], UserOpts, Opts),
    %% Postpone option checking until all other options are checked FIXME
    Check = fun({[_|_], SO}) when is_list(SO) ->
                    case proplists:get_value(sni_hosts, SO, undefined) of
                        undefined -> ok;
                        Recursive -> option_error(sni_hosts, Recursive)
                    end;
               (HostOpts) -> option_error(sni_hosts, HostOpts)
            end,
    [Check(E) || E <- SniHosts],

    {Where, SniFun0} = get_opt_fun(sni_fun, 1, undefined, UserOpts, Opts),

    option_incompatible(is_function(SniFun0) andalso SniHosts =/= [] andalso Where =:= new,
                        [sni_fun, sni_hosts]),
    assert_client_only(server_name_indication, UserOpts),

    SniFun = case SniFun0 =:= undefined of
                 true -> fun(Host) -> proplists:get_value(Host, SniHosts) end;
                 false -> SniFun0
             end,

    Opts#{sni_fun => SniFun};
opt_sni(UserOpts, #{versions := _Versions} = Opts, #{role := client} = Env) ->
    %% RFC 6066, Section 3: Currently, the only server names supported are
    %% DNS hostnames
    %% case inet_parse:domain(Value) of
    %%     false ->
    %%         throw({error, {options, {{Opt, Value}}}});
    %%     true ->
    %%         Value
    %% end;
    %%
    %% But the definition seems very diffuse, so let all strings through
    %% and leave it up to public_key to decide...
    SNI = case get_opt(server_name_indication, unbound, UserOpts, Opts) of
              {_, unbound} -> server_name_indication_default(maps:get(host, Env, undefined));
              {_, [_|_] = SN} -> SN;
              {_, disable} -> disable;
              {_, SN} -> option_error(server_name_indication, SN)
          end,
    assert_server_only(sni_fun, UserOpts),
    assert_server_only(sni_hosts, UserOpts),
    Opts#{server_name_indication => SNI}.

server_name_indication_default(Host) when is_list(Host) ->
    %% SNI should not contain a trailing dot that a hostname may
    string:strip(Host, right, $.);
server_name_indication_default(_) ->
    undefined.

opt_signature_algs(UserOpts, #{versions := Versions} = Opts, _Env) ->
    [TlsVersion|_] = TlsVsns = [tls_version(V) || V <- Versions],
    SA = case get_opt_list(signature_algs, undefined, UserOpts, Opts) of
             {default, undefined} when ?TLS_GTE(TlsVersion, ?TLS_1_2) ->
                 DefAlgs = tls_v1:default_signature_algs(TlsVsns),
                 handle_hashsigns_option(DefAlgs, TlsVersion);
             {new, Algs} ->
                 assert_version_dep(signature_algs, Versions, ['tlsv1.2', 'tlsv1.3']),
                 SA0 = handle_hashsigns_option(Algs, TlsVersion),
                 option_error(SA0 =:= [], no_supported_algorithms, {signature_algs, Algs}),
                 SA0;
             {_, Algs} ->
                 Algs
         end,
    SAC = case get_opt_list(signature_algs_cert, undefined, UserOpts, Opts) of
              {new, Schemes} ->
                  %% Do not send by default
                  assert_version_dep(signature_algs_cert, Versions, ['tlsv1.2', 'tlsv1.3']),
                  SAC0 = handle_signature_algorithms_option(Schemes, TlsVersion),
                  option_error(SAC0 =:= [], no_supported_signature_schemes, {signature_algs_cert, Schemes}),
                  SAC0;
              {_, Schemes} ->
                  Schemes
          end,
    Opts#{signature_algs => SA, signature_algs_cert => SAC}.

opt_alpn(UserOpts, #{versions := Versions} = Opts, #{role := server}) ->
    {_, APP} = get_opt_list(alpn_preferred_protocols, undefined, UserOpts, Opts),
    validate_protocols(is_list(APP), alpn_preferred_protocols, APP),

    {Where, NPA} = get_opt_list(next_protocols_advertised, undefined, UserOpts, Opts),
    validate_protocols(is_list(NPA), next_protocols_advertised, NPA),
    assert_version_dep(is_list(NPA), next_protocols_advertised, Versions, ['tlsv1','tlsv1.1','tlsv1.2']),

    assert_client_only(alpn_advertised_protocols, UserOpts),
    assert_client_only(client_preferred_next_protocols, UserOpts),

    Opts1 = set_opt_new(Where, next_protocols_advertised, undefined, NPA, Opts),
    Opts1#{alpn_preferred_protocols => APP};
opt_alpn(UserOpts, #{versions := Versions} = Opts, #{role := client}) ->
    {_, AAP} = get_opt_list(alpn_advertised_protocols, undefined, UserOpts, Opts),
    validate_protocols(is_list(AAP), alpn_advertised_protocols, AAP),

    {Where, NPS} = case get_opt(client_preferred_next_protocols, undefined, UserOpts, Opts) of
                       {new, CPNP} ->
                           assert_version_dep(client_preferred_next_protocols,
                                              Versions, ['tlsv1','tlsv1.1','tlsv1.2']),
                           {new, make_next_protocol_selector(CPNP)};
                       CPNP ->
                           CPNP
                   end,

    validate_protocols(is_list(NPS), client_preferred_next_protocols, NPS),

    assert_server_only(alpn_preferred_protocols, UserOpts),
    assert_server_only(next_protocols_advertised, UserOpts),

    Opts1 = set_opt_new(Where, next_protocol_selector, undefined, NPS, Opts),
    Opts1#{alpn_advertised_protocols => AAP}.

validate_protocols(false, _Opt, _List) -> ok;
validate_protocols(true, Opt, List) ->
    Check = fun(Bin) ->
                    IsOK = is_binary(Bin) andalso byte_size(Bin) > 0 andalso byte_size(Bin) < 256,
                    option_error(not IsOK, Opt, {invalid_protocol, Bin})
            end,
    lists:foreach(Check, List).

opt_mitigation(UserOpts, #{versions := Versions} = Opts, _Env) ->
    DefBeast = case ?TLS_GT(lists:last(Versions), ?TLS_1_0) of
                   true -> disabled;
                   false -> one_n_minus_one
               end,
    {Where1, BM} = get_opt_of(beast_mitigation, [disabled, one_n_minus_one, zero_n], DefBeast, UserOpts, Opts),
    assert_version_dep(Where1 =:= new, beast_mitigation, Versions, ['tlsv1']),

    {Where2, PC} = get_opt_bool(padding_check, true, UserOpts, Opts),
    assert_version_dep(Where2 =:= new, padding_check, Versions, ['tlsv1']),

    %% Use 'new' we need to check for non default 'one_n_minus_one'
    Opts1 = set_opt_new(new, beast_mitigation, disabled, BM, Opts),
    set_opt_new(Where2, padding_check, true, PC, Opts1).

opt_server(UserOpts, #{versions := Versions, log_level := LogLevel} = Opts, #{role := server}) ->
    {_, ECC} = get_opt_bool(honor_ecc_order, false, UserOpts, Opts),

    {_, Cipher} = get_opt_bool(honor_cipher_order, false, UserOpts, Opts),

    {Where1, Cookie} = get_opt_bool(cookie, true, UserOpts, Opts),
    assert_version_dep(Where1 =:= new, cookie, Versions, ['tlsv1.3']),

    {Where2, ReNeg} = get_opt_bool(client_renegotiation, true, UserOpts, Opts),
    assert_version_dep(Where2 =:= new, client_renegotiation, Versions, ['tlsv1','tlsv1.1','tlsv1.2']),

    Opts1 = case get_opt(dh, undefined, UserOpts, Opts) of
                {Where, DH} when is_binary(DH) ->
                    warn_override(Where, UserOpts, dh, [dhfile], LogLevel),
                    Opts#{dh => DH};
                {new, DH} ->
                    option_error(dh, DH);
                {_, undefined} ->
                    case get_opt_file(dhfile, unbound, UserOpts, Opts) of
                        {default, unbound} -> Opts;
                        {_, DHFile} -> Opts#{dhfile => DHFile}
                    end
            end,

    Opts1#{honor_ecc_order => ECC, honor_cipher_order => Cipher,
           cookie => Cookie, client_renegotiation => ReNeg};
opt_server(UserOpts, Opts, #{role := client}) ->
    assert_server_only(honor_ecc_order, UserOpts),
    assert_server_only(honor_cipher_order, UserOpts),
    assert_server_only(cookie, UserOpts),
    assert_server_only(client_renegotiation, UserOpts),
    assert_server_only(dh, UserOpts),
    assert_server_only(dhfile, UserOpts),
    Opts.

opt_client(UserOpts, #{versions := Versions} = Opts, #{role := client}) ->
    {Where, FB} = get_opt_bool(fallback, false, UserOpts, Opts),
    assert_version_dep(Where =:= new, fallback, Versions, ['tlsv1','tlsv1.1','tlsv1.2']),

    {_, CHC} = get_opt_list(customize_hostname_check, [], UserOpts, Opts),

    ValidMFL = [undefined, ?MAX_FRAGMENT_LENGTH_BYTES_1, ?MAX_FRAGMENT_LENGTH_BYTES_2,  %% RFC 6066, Section 4
                ?MAX_FRAGMENT_LENGTH_BYTES_3, ?MAX_FRAGMENT_LENGTH_BYTES_4],
    {_, MFL} = get_opt_of(max_fragment_length, ValidMFL, undefined, UserOpts, Opts),

    Opts#{fallback => FB, customize_hostname_check => CHC, max_fragment_length => MFL};
opt_client(UserOpts, Opts, #{role := server}) ->
    assert_client_only(fallback, UserOpts),
    assert_client_only(customize_hostname_check, UserOpts),
    assert_client_only(max_fragment_length, UserOpts),
    Opts#{customize_hostname_check => []}.

opt_renegotiate(UserOpts, #{versions := Versions} = Opts, _Env) ->
    {Where1, KUA} = get_opt_pos_int(key_update_at, ?KEY_USAGE_LIMIT_AES_GCM, UserOpts, Opts),
    assert_version_dep(Where1 =:= new, key_update_at, Versions, ['tlsv1.3']),

    %% Undocumented, old ?
    {_, RA0} = get_opt_pos_int(renegotiate_at, ?DEFAULT_RENEGOTIATE_AT, UserOpts, Opts),
    RA = min(RA0, ?DEFAULT_RENEGOTIATE_AT),  %% Override users choice without notifying ??

    {Where3, SR} = get_opt_bool(secure_renegotiate, true, UserOpts, Opts),
    assert_version_dep(Where3 =:= new, secure_renegotiate, Versions, ['tlsv1','tlsv1.1','tlsv1.2']),

    Opts#{secure_renegotiate => SR, key_update_at => KUA, renegotiate_at => RA}.

opt_reuse_sessions(UserOpts, #{versions := Versions} = Opts, #{role := client}) ->
    {Where1, RUSS} = get_opt_of(reuse_sessions, [true, false, save], true, UserOpts, Opts),

    {Where2, RS} = RST = get_opt(reuse_session, undefined, UserOpts, Opts),
    case RST of
        {new, Bin} when is_binary(Bin) -> ok;
        {new, {B1,B2}} when is_binary(B1), is_binary(B2) -> ok;
        {new, Bad} -> option_error(reuse_session, Bad);
        {_, _} -> ok
    end,

    assert_version_dep(Where1 =:= new, reuse_sessions, Versions, ['tlsv1','tlsv1.1','tlsv1.2']),
    assert_version_dep(Where2 =:= new, reuse_session, Versions, ['tlsv1','tlsv1.1','tlsv1.2']),
    Opts#{reuse_sessions => RUSS, reuse_session => RS};
opt_reuse_sessions(UserOpts, #{versions := Versions} = Opts, #{role := server}) ->
    {Where1, RUSS} = get_opt_bool(reuse_sessions, true, UserOpts, Opts),

    DefRS = fun(_, _, _, _) -> true end,
    {Where2, RS} = get_opt_fun(reuse_session, 4, DefRS, UserOpts, Opts),

    assert_version_dep(Where1 =:= new, reuse_sessions, Versions, ['tlsv1','tlsv1.1','tlsv1.2']),
    assert_version_dep(Where2 =:= new, reuse_session, Versions, ['tlsv1','tlsv1.1','tlsv1.2']),
    Opts#{reuse_sessions => RUSS, reuse_session => RS}.

opt_identity(UserOpts, #{versions := Versions} = Opts, _Env) ->
    PSK = case get_opt_list(psk_identity, undefined, UserOpts, Opts) of
              {new, PSK0} ->
                  PSK1 = unicode:characters_to_binary(PSK0),
                  PSKSize = byte_size(PSK1),
                  assert_version_dep(psk_identity, Versions, ['tlsv1','tlsv1.1','tlsv1.2']),
                  option_error(not (0 < PSKSize andalso PSKSize < 65536),
                                psk_identity, {psk_identity, PSK0}),
                  PSK1;
              {_, PSK0} ->
                  PSK0
          end,

    SRP = case get_opt(srp_identity, undefined, UserOpts, Opts) of
              {new, {S1, S2}} when is_list(S1), is_list(S2) ->
                  User = unicode:characters_to_binary(S1),
                  UserSize = byte_size(User),
                  assert_version_dep(srp_identity, Versions, ['tlsv1','tlsv1.1','tlsv1.2']),
                  option_error(not (0 < UserSize andalso UserSize < 65536),
                                srp_identity, {srp_identity, PSK0}),
                  {User, unicode:characters_to_binary(S2)};
              {new, Err} ->
                  option_error(srp_identity, Err);
              {_, SRP0} ->
                  SRP0
          end,

    ULF = case get_opt(user_lookup_fun, undefined, UserOpts, Opts) of
              {new, {Fun, _} = ULF0} when is_function(Fun, 3) ->
                  assert_version_dep(user_lookup_fun, Versions, ['tlsv1','tlsv1.1','tlsv1.2']),
                  ULF0;
              {new, ULF0} ->
                  option_error(user_lookup_fun, ULF0);
              {_, ULF0} ->
                  ULF0
          end,

    Opts#{psk_identity => PSK, srp_identity => SRP, user_lookup_fun => ULF}.

opt_supported_groups(UserOpts, #{versions := TlsVsns} = Opts, _Env) ->
    SG = case get_opt_list(supported_groups,  undefined, UserOpts, Opts) of
             {default, undefined} ->
                 handle_supported_groups_option(groups(default));
             {new, SG0} ->
                 assert_version_dep(supported_groups, TlsVsns, ['tlsv1.3']),
                 handle_supported_groups_option(SG0);
             {old, SG0} ->
                 SG0
         end,

    CPHS = case get_opt_list(ciphers, [], UserOpts, Opts) of
               {old, CPS0} -> CPS0;
               {_, CPS0} -> handle_cipher_option(CPS0, TlsVsns)
           end,

    ECCS =  try assert_version_dep(eccs, TlsVsns, ['tlsv1.2', 'tlsv1.1', 'tlsv1']) of
                _ ->
                    case get_opt_list(eccs, undefined, UserOpts, Opts) of
                        {old, ECCS0} -> ECCS0;
                        {default, _} -> handle_eccs_option(tls_v1:ec_curves(default, 'tlsv1.2'));
                        {new, ECCS0} -> handle_eccs_option(ECCS0)
                    end
            catch
                throw:_ ->
                    []
            end,
    Opts#{ciphers => CPHS, eccs => ECCS, supported_groups => SG}.

opt_crl(UserOpts, Opts, _Env) ->
    {_, Check} = get_opt_of(crl_check, [best_effort, peer, true, false], false, UserOpts, Opts),
    Cache = case get_opt(crl_cache, {ssl_crl_cache, {internal, []}}, UserOpts, Opts) of
                {_, {Cb, {_Handle, Options}} = Value} when is_atom(Cb), is_list(Options) ->
                    Value;
                {_, Err} ->
                    option_error(crl_cache, Err)
            end,
    Opts#{crl_check => Check, crl_cache => Cache}.

opt_handshake(UserOpts, Opts, _Env) ->
    {_, HS} = get_opt_of(handshake, [hello, full], full, UserOpts, Opts),

    {_, MHSS} = get_opt_int(max_handshake_size, 1, ?MAX_UNIT24, ?DEFAULT_MAX_HANDSHAKE_SIZE,
                            UserOpts, Opts),

    Opts#{handshake => HS, max_handshake_size => MHSS}.

opt_use_srtp(UserOpts, #{protocol := Protocol} = Opts, _Env) ->
    UseSRTP = case get_opt_map(use_srtp, undefined, UserOpts, Opts) of
                  {old, UseSRTP0} ->
                      UseSRTP0;
                  {default, undefined} ->
                      undefined;
                  {new, UseSRTP1} ->
                      assert_protocol_dep(use_srtp, Protocol, [dtls]),
                      validate_use_srtp(UseSRTP1)
              end,
    case UseSRTP of
        #{} -> Opts#{use_srtp => UseSRTP};
        _ -> Opts
    end.

validate_use_srtp(#{protection_profiles := [_|_] = PPs} = UseSRTP) ->
    case maps:keys(UseSRTP) -- [protection_profiles, mki] of
        [] -> ok;
        Extra -> option_error(use_srtp, {unknown_parameters, Extra})
    end,
    IsValidProfile = fun(<<_, _>>) -> true; (_) -> false end,
    case lists:all(IsValidProfile, PPs) of
        true -> ok;
        false -> option_error(use_srtp, {invalid_protection_profiles, PPs})
    end,
    case UseSRTP of
        #{mki := MKI} when not is_binary(MKI) ->
            option_error(use_srtp, {invalid_mki, MKI});
        #{mki := _} ->
            UseSRTP;
        #{} ->
            UseSRTP#{mki => <<>>}
    end;

validate_use_srtp(#{} = UseSRTP) ->
    option_error(use_srtp, {no_protection_profiles, UseSRTP}).


opt_process(UserOpts, Opts0, _Env) ->
    Opts1 = set_opt_list(receiver_spawn_opts, [], UserOpts, Opts0),
    Opts2 = set_opt_list(sender_spawn_opts, [], UserOpts, Opts1),
    %% {_, SSO} = get_opt_list(sender_spawn_opts, [], UserOpts, Opts),
    %% Opts = Opts1#{receiver_spawn_opts => RSO, sender_spawn_opts => SSO},
    set_opt_int(hibernate_after, 0, infinity, infinity, UserOpts, Opts2).

%%%%

get_opt(Opt, Default, UserOpts, Opts) ->
    case maps:get(Opt, UserOpts, unbound) of
        unbound ->
            case maps:get(maybe_map_key_internal(Opt), Opts, unbound) of
                unbound -> %% Uses default value
                    {default, Default};
                Value ->   %% Uses already set value (merge)
                    {old, Value}
            end;
        Value ->           %% Uses new user option
            {new, Value}
    end.

get_opt_of(Opt, Valid, Default, UserOpts, Opts) ->
    case get_opt(Opt, Default, UserOpts, Opts) of
        {new, Value} = Res ->
            case lists:member(Value, Valid) of
                true -> Res;
                false -> option_error(Opt, Value)
            end;
        Res ->
            Res
    end.

get_opt_bool(Opt, Default, UserOpts, Opts) ->
    case get_opt(Opt, Default, UserOpts, Opts) of
        {_, Value} = Res when is_boolean(Value) -> Res;
        {_, Value} -> option_error(Opt, Value)
    end.

get_opt_pos_int(Opt, Default, UserOpts, Opts) ->
    get_opt_int(Opt, 1, infinity, Default, UserOpts, Opts).

get_opt_int(Opt, Min, Max, Default, UserOpts, Opts) ->
    case get_opt(Opt, Default, UserOpts, Opts) of
        {_, Value} = Res when is_integer(Value), Min =< Value, Value =< Max ->
            Res;
        {_, Value} = Res when Value =:= infinity, Max =:= infinity ->
            Res;
        {_, Value} ->
            option_error(Opt, Value)
    end.

get_opt_fun(Opt, Arity, Default, UserOpts, Opts) ->
    case get_opt(Opt, Default, UserOpts, Opts) of
        {_, Fun} = Res when is_function(Fun, Arity) -> Res;
        {new, Err} -> option_error(Opt, Err);
        Res -> Res
    end.

get_opt_list(Opt, Default, UserOpts, Opts) ->
    case get_opt(Opt, Default, UserOpts, Opts) of
        {new, Err} when not is_list(Err) -> option_error(Opt, Err);
        Res -> Res
    end.

get_opt_bin(Opt, Default, UserOpts, Opts) ->
    case get_opt(Opt, Default, UserOpts, Opts) of
        {new, Err} when not is_binary(Err) -> option_error(Opt, Err);
        Res -> Res
    end.

get_opt_file(Opt, Default, UserOpts, Opts) ->
    case get_opt(Opt, Default, UserOpts, Opts) of
        {new, File} -> {new, validate_filename(File, Opt)};
        Res -> Res
    end.

set_opt_bool(Opt, Default, UserOpts, Opts) ->
    case maps:get(Opt, UserOpts, Default) of
        Default -> Opts;
        Value when is_boolean(Value) -> Opts#{Opt => Value};
        Value -> option_error(Opt, Value)
    end.

get_opt_map(Opt, Default, UserOpts, Opts) ->
    case get_opt(Opt, Default, UserOpts, Opts) of
        {new, Err} when not is_map(Err) -> option_error(Opt, Err);
        Res -> Res
    end.

set_opt_int(Opt, Min, Max, Default, UserOpts, Opts) ->
    case maps:get(Opt, UserOpts, Default) of
        Default ->
            Opts;
        Value when is_integer(Value), Min =< Value, Value =< Max ->
            Opts#{Opt => Value};
        Value when Value =:= infinity, Max =:= infinity ->
            Opts#{Opt => Value};
        Value ->
            option_error(Opt, Value)
    end.

set_opt_list(Opt, Default, UserOpts, Opts) ->
    case maps:get(Opt, UserOpts, []) of
        Default ->
            Opts;
        List when is_list(List) ->
            Opts#{Opt => List};
        Value ->
            option_error(Opt, Value)
    end.

set_opt_new(new, Opt, Default, Value, Opts)
  when Default =/= Value ->
    Opts#{Opt => Value};
set_opt_new(_, _, _, _, Opts) ->
    Opts.

%%%%

default_cb_info(tls) ->
    {gen_tcp, tcp, tcp_closed, tcp_error, tcp_passive};
default_cb_info(dtls) ->
    {gen_udp, udp, udp_closed, udp_error, udp_passive}.

handle_cb_info({V1, V2, V3, V4}) ->
    {V1,V2,V3,V4, list_to_atom(atom_to_list(V2) ++ "_passive")};
handle_cb_info(CbInfo) when tuple_size(CbInfo) =:= 5 ->
    CbInfo;
handle_cb_info(CbInfo) ->
    option_error(cb_info, CbInfo).

handle_option_cb_info(Options, Protocol) ->
    CbInfo = proplists:get_value(cb_info, Options, default_cb_info(Protocol)),
    handle_cb_info(CbInfo).

maybe_map_key_internal(client_preferred_next_protocols) ->
    next_protocol_selector;
maybe_map_key_internal(K) ->
    K.

split_options(Opts0, AllOptions) ->
    Opts1 = proplists:expand([{binary, [{mode, binary}]},
                              {list, [{mode, list}]}], Opts0),
    Opts2 = handle_option_format(Opts1, []),
    %% Remove deprecated ssl_imp option
    Opts = proplists:delete(ssl_imp, Opts2),

    DeleteUserOpts = fun(Key, PropList) -> proplists:delete(Key, PropList) end,
    AllOpts = [cb_info, client_preferred_next_protocols] ++ AllOptions,
    SockOpts = lists:foldl(DeleteUserOpts, Opts, AllOpts),
    {Opts -- SockOpts, SockOpts}.

assert_server_only(Option, Opts) ->
    Value = maps:get(Option, Opts, undefined),
    role_error(Value =/= undefined, server_only, Option).
assert_client_only(Option, Opts) ->
    Value = maps:get(Option, Opts, undefined),
    role_error(Value =/= undefined, client_only, Option).

assert_server_only(client, Bool, Option) ->
    role_error(Bool, server_only, Option);
assert_server_only(_, _, _) ->
    ok.
assert_client_only(server, Bool, Option) ->
    role_error(Bool, client_only, Option);
assert_client_only(_, _, _) ->
    ok.

role_error(false, _ErrorDesc, _Option) ->
    ok;
role_error(true, ErrorDesc, Option)
  when ErrorDesc =:= client_only; ErrorDesc =:= server_only ->
    throw_error({option, ErrorDesc, Option}).

option_incompatible(false, _Options) -> ok;
option_incompatible(true, Options) -> option_incompatible(Options).

-spec option_incompatible(_) -> no_return().
option_incompatible(Options) ->
    throw_error({options, incompatible, Options}).

option_error(false, _, _What) -> true;
option_error(true, Tag, What) -> option_error(Tag,What).

-spec option_error(_,_) -> no_return().
option_error(Tag, What) ->
    throw_error({options, {Tag, What}}).

-spec throw_error(_) -> no_return().
throw_error(Err) ->
    throw({error, Err}).

assert_protocol_dep(Option, Protocol, AllowedProtos) ->
    case lists:member(Protocol, AllowedProtos) of
        true -> ok;
        false -> option_incompatible([Option, {protocol, Protocol}])
    end.

assert_version_dep(Option, Vsns, AllowedVsn) ->
    assert_version_dep(true, Option, Vsns, AllowedVsn).

assert_version_dep(false, _, _, _) -> true;
assert_version_dep(true, Option, SSLVsns, AllowedVsn) ->
    case is_dtls_configured(SSLVsns) of
        true -> %% TODO: Check option dependency for DTLS
            true;
        false ->
            APIVsns = lists:map(fun tls_record:protocol_version/1, SSLVsns),
            Set1 = sets:from_list(APIVsns),
            Set2 = sets:from_list(AllowedVsn),
            case sets:size(sets:intersection(Set1, Set2)) > 0 of
                true -> ok;
                false -> option_incompatible([Option, {versions, APIVsns}])
            end
    end.

warn_override(new, UserOpts, NewOpt, OldOpts, LogLevel) ->
    Check = fun(Key) -> maps:is_key(Key,UserOpts) end,
    case lists:filter(Check, OldOpts) of
        [] -> ok;
        Ignored ->
            Desc = lists:flatten(io_lib:format("Options ~w are ignored", [Ignored])),
            Reas = lists:flatten(io_lib:format("Option ~w is set", [NewOpt])),
            ssl_logger:log(notice, LogLevel, #{description => Desc, reason => Reas}, ?LOCATION)
    end;
warn_override(_, _UserOpts, _NewOpt, _OldOpts, _LogLevel) ->
    ok.

is_dtls_configured(Versions) ->
    lists:any(fun (Ver) -> ?DTLS_1_X(Ver) end, Versions).

handle_hashsigns_option(Value, Version) ->
    try
        if ?TLS_GTE(Version, ?TLS_1_3) ->
                tls_v1:signature_schemes(Version, Value);
           (Version =:= ?TLS_1_2) ->
                tls_v1:signature_algs(Version, Value);
           true ->
                undefined
        end
    catch error:function_clause ->
            option_error(signature_algs, Value)
    end.

handle_signature_algorithms_option(Value, Version) ->
    try tls_v1:signature_schemes(Version, Value)
    catch error:function_clause ->
            option_error(signature_algs_cert, Value)
    end.

validate_filename(FN, _Option) when is_binary(FN), FN =/= <<>> ->
    FN;
validate_filename([_|_] = FN, _Option) ->
    Enc = file:native_name_encoding(),
    unicode:characters_to_binary(FN, unicode, Enc);
validate_filename(FN, Option) ->
    option_error(Option, FN).

%% Do not allow configuration of TLS 1.3 with a gap where TLS 1.2 is not supported
%% as that configuration can trigger the built in version downgrade protection
%% mechanism and the handshake can fail with an Illegal Parameter alert.
tls_validate_version_gap(Versions) ->
    case lists:member('tlsv1.3', Versions) of
        true when length(Versions) >= 2 ->
            case lists:member('tlsv1.2', Versions) of
                true ->
                    Versions;
                false ->
                    throw({error, {options, missing_version, {'tlsv1.2', {versions, Versions}}}})
            end;
        _ ->
            Versions
    end.

emulated_options(undefined, undefined, Protocol, Opts) ->
    case Protocol of
	tls ->
	    tls_socket:emulated_options(Opts);
	dtls ->
	    dtls_socket:emulated_options(Opts)
    end;
emulated_options(Transport, Socket, Protocol, Opts) ->
    EmulatedOptions = tls_socket:emulated_options(),
    {ok, Original} = tls_socket:getopts(Transport, Socket, EmulatedOptions),
    {Inet, Emulated0} = emulated_options(undefined, undefined, Protocol, Opts),
    {Inet, lists:ukeymerge(1, Emulated0, Original)}.

handle_cipher_option(Value, Versions)  when is_list(Value) ->       
    try binary_cipher_suites(Versions, Value) of
	Suites ->
	    Suites
    catch
	exit:_ ->
	    option_error(ciphers, Value);
	error:_->
	    option_error(ciphers, Value)
    end.

binary_cipher_suites([?TLS_1_3], []) ->
    %% Defaults to all supported suites that does
    %% not require explicit configuration TLS-1.3
    %% only mode.
    default_binary_suites(exclusive, ?TLS_1_3);
binary_cipher_suites([Version| _], []) -> 
    %% Defaults to all supported suites that does
    %% not require explicit configuration
    default_binary_suites(default, Version);
binary_cipher_suites(Versions, [Map|_] = Ciphers0) when is_map(Map) ->
    Ciphers = [ssl_cipher_format:suite_map_to_bin(C) || C <- Ciphers0],
    binary_cipher_suites(Versions, Ciphers);
binary_cipher_suites(Versions, [Tuple|_] = Ciphers0) when is_tuple(Tuple) ->
    Ciphers = [ssl_cipher_format:suite_map_to_bin(tuple_to_map(C)) || C <- Ciphers0],
    binary_cipher_suites(Versions, Ciphers);
binary_cipher_suites(Versions, [Cipher0 | _] = Ciphers0) when is_binary(Cipher0) ->
    All = all_suites(Versions),
    case [Cipher || Cipher <- Ciphers0, lists:member(Cipher, All)] of
	[] ->
	    %% Defaults to all supported suites that does
	    %% not require explicit configuration
	    binary_cipher_suites(Versions, []);
	Ciphers ->
	    Ciphers
    end;
binary_cipher_suites(Versions, [Head | _] = Ciphers0) when is_list(Head) ->
    %% Format: ["RC4-SHA","RC4-MD5"]
    Ciphers = [ssl_cipher_format:suite_openssl_str_to_map(C) || C <- Ciphers0],
    binary_cipher_suites(Versions, Ciphers);
binary_cipher_suites(Versions, Ciphers0)  ->
    %% Format: "RC4-SHA:RC4-MD5"
    Ciphers = [ssl_cipher_format:suite_openssl_str_to_map(C) || C <- string:lexemes(Ciphers0, ":")],
    binary_cipher_suites(Versions, Ciphers).

default_binary_suites(exclusive, Version) ->
    ssl_cipher:filter_suites(tls_v1:exclusive_suites(Version));
default_binary_suites(default, Version) ->
    ssl_cipher:filter_suites(ssl_cipher:suites(Version)).

all_suites([?TLS_1_3]) ->
    tls_v1:exclusive_suites(?TLS_1_3);
all_suites([?TLS_1_3, Version1 |_]) ->
    all_suites([?TLS_1_3]) ++
        ssl_cipher:all_suites(Version1) ++
        ssl_cipher:anonymous_suites(Version1);
all_suites([Version|_]) ->
      ssl_cipher:all_suites(Version) ++
        ssl_cipher:anonymous_suites(Version).

tuple_to_map({Kex, Cipher, Mac}) ->
    #{key_exchange => Kex,
      cipher => Cipher,
      mac => Mac,
      prf => default_prf};
tuple_to_map({Kex, Cipher, Mac, Prf}) ->
    #{key_exchange => Kex,
      cipher => Cipher,
      mac => tuple_to_map_mac(Cipher, Mac),
      prf => Prf}.

%% Backwards compatible
tuple_to_map_mac(aes_128_gcm, _) -> 
    aead;
tuple_to_map_mac(aes_256_gcm, _) -> 
    aead;
tuple_to_map_mac(chacha20_poly1305, _) ->
    aead;
tuple_to_map_mac(_, MAC) ->
    MAC.

handle_eccs_option(Value) when is_list(Value) ->
    try tls_v1:ecc_curves(Value) of
        Curves ->
            option_error(Curves =:= [], eccs, none_valid),
            #elliptic_curves{elliptic_curve_list = Curves}
    catch
        exit:_ -> option_error(eccs, Value);
        error:_ -> option_error(eccs, Value)
    end.

handle_supported_groups_option(Value) when is_list(Value) ->
    try tls_v1:groups(Value) of
        Groups ->
            option_error(Groups =:= [], supported_groups, none_valid),
            #supported_groups{supported_groups = Groups}
    catch
        exit:_ -> option_error(supported_groups, Value);
        error:_ -> option_error(supported_groups, Value)
    end.


-spec do_format_error( string()
                     | closed
                     | {tls_alert, {_, Description :: string()}}
                     | {options, Options :: term()}
                     | {options, {socket_options, Option :: term()}}
                     | {options, {socket_options, Option :: term(), Error}}
                     | {options, {FileType, File :: string(), Error}}
                     | InetError
                     | OtherReason) -> string()
              when
      FileType    :: cacertfile | certfile | keyfile | dhfile,
      OtherReason :: term(),
      Error       :: term(),
      InetError   :: inet:posix() | system_limit.

do_format_error(Reason) when is_list(Reason) ->
    Reason;
do_format_error(closed) ->
    "TLS connection is closed";
do_format_error({tls_alert, {_, Description}}) ->
    Description;
do_format_error({options,{FileType, File, Reason}})
  when FileType == cacertfile;
       FileType == certfile;
       FileType == keyfile;
       FileType == dhfile ->
    Error = file_error_format(Reason),
    file_desc(FileType) ++ File ++ ": " ++ Error;
do_format_error ({options, {socket_options, Option, Error}}) ->
    lists:flatten(io_lib:format("Invalid transport socket option ~p: ~s", [Option, do_format_error(Error)]));
do_format_error({options, {socket_options, Option}}) ->
    lists:flatten(io_lib:format("Invalid socket option: ~p", [Option]));
do_format_error({options, incompatible, Opts}) ->
    lists:flatten(io_lib:format("Options (or their values) can not be combined: ~p", [Opts]));
do_format_error({option, Reason, Opts}) ->
    lists:flatten(io_lib:format("Invalid option ~w ~w", [Opts, Reason]));
do_format_error({options, Reason, Opts}) ->
    lists:flatten(io_lib:format("Invalid option ~w ~w", [Opts, Reason]));
do_format_error({options, {missing_version=R, Opts}}) ->
    lists:flatten(io_lib:format("Invalid option ~w ~w", [Opts, R]));
do_format_error({options, {option_not_a_key_value_tuple=R, Opts}}) ->
    lists:flatten(io_lib:format("Invalid option ~w ~w", [Opts, R]));
do_format_error({options, {no_supported_algorithms=R, Opts}}) ->
    lists:flatten(io_lib:format("Invalid option ~w ~w", [Opts, R]));
do_format_error({options, {no_supported_signature_schemes=R, Opts}}) ->
    lists:flatten(io_lib:format("Invalid option ~w ~w", [Opts, R]));
do_format_error({options, {insufficient_crypto_support=R, Opts}}) ->
    lists:flatten(io_lib:format("Invalid option ~w ~w", [Opts, R]));

do_format_error({options, Options}) ->
    lists:flatten(io_lib:format("Invalid TLS option: ~p", [Options]));

do_format_error(Error) ->
    case inet:format_error(Error) of
        "unknown POSIX" ++ _ ->
            unexpected_format(Error);
        Other ->
            Other
    end.

unexpected_format(Error) ->
    lists:flatten(io_lib:format("Unexpected error: ~p", [Error])).

file_error_format({error, Error})->
    case file:format_error(Error) of
	"unknown POSIX error" ++ _ ->
	    "decoding error";
	Str ->
	    Str
    end;
file_error_format(_) ->
    "decoding error".

file_desc(cacertfile) ->
    "Invalid CA certificate file ";
file_desc(certfile) ->
    "Invalid certificate file ";
file_desc(keyfile) ->
    "Invalid key file ";
file_desc(dhfile) ->
    "Invalid DH params file ".

make_next_protocol_selector(undefined) ->
    undefined;
make_next_protocol_selector({Precedence, PrefProtcol} = V) ->
    option_error(not is_list(PrefProtcol), client_preferred_next_protocols, V),
    make_next_protocol_selector({Precedence, PrefProtcol, ?NO_PROTOCOL});
make_next_protocol_selector({Precedence, AllProtocols, DefP} = V) ->
    option_error(not is_list(AllProtocols), client_preferred_next_protocols, V),
    option_error(not (is_binary(DefP) andalso byte_size(DefP) < 256), client_preferred_next_protocols, V),
    validate_protocols(true, client_preferred_next_protocols, AllProtocols),
    case Precedence of
        client ->                 
            fun(Advertised) ->
                    Search = fun(P) -> lists:member(P, Advertised) end,
                    case lists:search(Search, AllProtocols) of
                        false -> DefP;
                        {value, Preferred} -> Preferred
                    end
            end;
        server ->
            fun(Advertised) ->
                    Search = fun(P) -> lists:member(P, AllProtocols) end,
                    case lists:search(Search, Advertised) of
                        false -> DefP;
                        {value, Preferred} -> Preferred
                    end
            end;
        Value ->
            option_error(client_preferred_next_protocols, {invalid_precedence, Value})
    end;
make_next_protocol_selector(What) ->
    option_error(client_preferred_next_protocols, What).

connection_cb(tls) ->
    tls_gen_connection;
connection_cb(dtls) ->
    dtls_gen_connection;
connection_cb(Opts) ->
   connection_cb(proplists:get_value(protocol, Opts, tls)).


%% Assert that basic options are on the format {Key, Value}
%% with a few exceptions and phase out log_alert 
handle_option_format([], Acc) ->
    lists:reverse(Acc);
handle_option_format([{log_alert, Bool} | Rest], Acc) when is_boolean(Bool) ->
    case proplists:get_value(log_level, Acc ++ Rest, undefined) of
        undefined ->
            handle_option_format(Rest, [{log_level, 
                                         map_log_level(Bool)} | Acc]);
        _ ->
            handle_option_format(Rest, Acc)
    end;
handle_option_format([{Key,_} = Opt | Rest], Acc) when is_atom(Key) ->
    handle_option_format(Rest, [Opt | Acc]);
%% Handle exceptions 
handle_option_format([{raw,_,_,_} = Opt | Rest], Acc) ->
    handle_option_format(Rest,  [Opt | Acc]);
handle_option_format([inet = Opt | Rest], Acc) ->
    handle_option_format(Rest,  [Opt | Acc]);
handle_option_format([inet6 = Opt | Rest], Acc) ->
    handle_option_format(Rest,  [Opt | Acc]);
handle_option_format([Value | _], _) ->
    option_error(option_not_a_key_value_tuple, Value).

map_log_level(true) ->
    notice;
map_log_level(false) ->
    none.

include_security_info([]) ->
    false;
include_security_info([Item | Items]) ->
    case lists:member(Item, [client_random, server_random, master_secret, keylog]) of
        true ->
            true;
        false  ->
            include_security_info(Items)
    end.


add_filter(undefined, Filters) ->
    Filters;
add_filter(Filter, Filters) ->
    [Filter | Filters].

unambiguous_path(Value) ->
    AbsName = filename:absname(Value),
    UP = case file:read_link(AbsName) of
             {ok, PathWithNoLink} ->
                 case filename:pathtype(PathWithNoLink) of
                     relative ->
                         Dirname = filename:dirname(AbsName),
                         filename:join([Dirname, PathWithNoLink]);
                     _ ->
                         PathWithNoLink
                 end;
             _ ->
                 AbsName
         end,
    validate_filename(UP, cacertfile).

%%%################################################################
%%%#
%%%# Tracing
%%%#
handle_trace(csp, {call, {?MODULE, opt_ocsp, [UserOpts | _]}}, Stack) ->
    {format_ocsp_params(UserOpts), Stack};
handle_trace(csp, {return_from, {?MODULE, opt_ocsp, 3}, Return}, Stack) ->
    {format_ocsp_params(Return), Stack};
handle_trace(rle, {call, {?MODULE, listen, Args}}, Stack0) ->
    Role = server,
    {io_lib:format("(*~w) Args = ~W", [Role, Args, 10]), [{role, Role} | Stack0]};
handle_trace(rle, {call, {?MODULE, connect, Args}}, Stack0) ->
    Role = client,
    {io_lib:format("(*~w) Args = ~W", [Role, Args, 10]), [{role, Role} | Stack0]}.

format_ocsp_params(Map) ->
    Stapling = maps:get(ocsp_stapling, Map, '?'),
    Nonce = maps:get(ocsp_nonce, Map, '?'),
    Certs = maps:get(ocsp_responder_certs, Map, '?'),
    io_lib:format("Stapling = ~W Nonce = ~W Certs = ~W",
                   [Stapling, 5, Nonce, 5, Certs, 5]).
