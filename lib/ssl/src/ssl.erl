%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2020. All Rights Reserved.
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

%%% Purpose : Main API module for SSL see also tls.erl and dtls.erl

-module(ssl).

-include_lib("public_key/include/public_key.hrl").

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
         ssl_accept/1, 
         ssl_accept/2, 
         ssl_accept/3,
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
-export([cipher_suites/0, 
         cipher_suites/1, 
         cipher_suites/2, 
         cipher_suites/3,
         filter_cipher_suites/2,
         prepend_cipher_suites/2, 
         append_cipher_suites/2,
         eccs/0, 
         eccs/1, 
         versions/0, 
         groups/0, 
         groups/1,
         format_error/1, 
         renegotiate/1, 
         update_keys/2,
         prf/5, 
         negotiated_protocol/1, 
	 connection_information/1, 
         connection_information/2]).
%% Misc
-export([handle_options/2,
         handle_options/3,
         tls_version/1, 
         suite_to_str/1,
         suite_to_openssl_str/1,
         str_to_suite/1]).

-deprecated({ssl_accept, '_', "use ssl_handshake/1,2,3 instead"}).

-deprecated({cipher_suites, 0, "use cipher_suites/2,3 instead"}).
-deprecated({cipher_suites, 1, "use cipher_suites/2,3 instead"}).

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
              group/0]).

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
-type host()                     :: hostname() | ip_address(). % exported
-type hostname()                 :: string().
-type ip_address()               :: inet:ip_address().
-type session_id()               :: binary(). % exported
-type protocol_version()         :: tls_version() | dtls_version(). % exported
-type tls_version()              :: 'tlsv1.2' | 'tlsv1.3' | tls_legacy_version().
-type dtls_version()             :: 'dtlsv1.2' | dtls_legacy_version().
-type tls_legacy_version()       ::  tlsv1 | 'tlsv1.1' .
-type dtls_legacy_version()      :: 'dtlsv1'.
-type verify_type()              :: verify_none | verify_peer.
-type cipher()                   :: aes_128_cbc |
                                    aes_256_cbc |
                                    aes_128_gcm |
                                    aes_256_gcm |
                                    aes_128_ccm |
                                    aes_256_ccm |
                                    aes_128_ccm_8 |
                                    aes_256_ccm_8 |                                    
                                    chacha20_poly1305 |
                                    legacy_cipher(). % exported
-type legacy_cipher()            ::  rc4_128 |
                                     des_cbc |
                                     '3des_ede_cbc'.

-type hash()                     :: sha |
                                    sha2() |
                                    legacy_hash(). % exported

-type sha2()                    ::  sha224 |
                                    sha256 |
                                    sha384 |
                                    sha512.

-type legacy_hash()             :: md5.

-type sign_algo()               :: rsa | dsa | ecdsa. % exported

-type sign_scheme()             :: rsa_pkcs1_sha256 
                                 | rsa_pkcs1_sha384
                                 | rsa_pkcs1_sha512
                                 | ecdsa_secp256r1_sha256
                                 | ecdsa_secp384r1_sha384
                                 | ecdsa_secp521r1_sha512
                                 | rsa_pss_rsae_sha256
                                 | rsa_pss_rsae_sha384
                                 | rsa_pss_rsae_sha512
                                 | rsa_pss_pss_sha256
                                 | rsa_pss_pss_sha384
                                 | rsa_pss_pss_sha512
                                 | rsa_pkcs1_sha1
                                 | ecdsa_sha1. % exported

-type kex_algo()                :: rsa |
                                   dhe_rsa | dhe_dss |
                                   ecdhe_ecdsa | ecdh_ecdsa | ecdh_rsa |
                                   srp_rsa| srp_dss |
                                   psk | dhe_psk | rsa_psk |
                                   dh_anon | ecdh_anon | srp_anon |
                                   any. %% TLS 1.3 , exported
-type erl_cipher_suite()       :: #{key_exchange := kex_algo(),
                                    cipher := cipher(),
                                    mac    := hash() | aead,
                                    prf    := hash() | default_prf %% Old cipher suites, version dependent
                                   }.  

-type old_cipher_suite() :: {kex_algo(), cipher(), hash()} % Pre TLS 1.2 
                             %% TLS 1.2, internally PRE TLS 1.2 will use default_prf
                           | {kex_algo(), cipher(), hash() | aead, hash()}. 

-type named_curve()           :: sect571r1 |
                                 sect571k1 |
                                 secp521r1 |
                                 brainpoolP512r1 |
                                 sect409k1 |
                                 sect409r1 |
                                 brainpoolP384r1 |
                                 secp384r1 |
                                 sect283k1 |
                                 sect283r1 |
                                 brainpoolP256r1 |
                                 secp256k1 |
                                 secp256r1 |
                                 sect239k1 |
                                 sect233k1 |
                                 sect233r1 |
                                 secp224k1 |
                                 secp224r1 |
                                 sect193r1 |
                                 sect193r2 |
                                 secp192k1 |
                                 secp192r1 |
                                 sect163k1 |
                                 sect163r1 |
                                 sect163r2 |
                                 secp160k1 |
                                 secp160r1 |
                                 secp160r2. % exported

-type group() :: secp256r1 | secp384r1 | secp521r1 | ffdhe2048 |
                 ffdhe3072 | ffdhe4096 | ffdhe6144 | ffdhe8192. % exported

-type srp_param_type()        :: srp_1024 |
                                 srp_1536 |
                                 srp_2048 |
                                 srp_3072 |
                                 srp_4096 |
                                 srp_6144 |
                                 srp_8192. % exported

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
                                {cert, cert()} |
                                {certfile, cert_pem()} |
                                {key, key()} |
                                {keyfile, key_pem()} |
                                {password, key_password()} |
                                {ciphers, cipher_suites()} |
                                {eccs, [named_curve()]} |
                                {signature_algs_cert, signature_schemes()} |
                                {supported_groups, supported_groups()} |
                                {secure_renegotiate, secure_renegotiation()} |
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
                                {middlebox_comp_mode, middlebox_comp_mode()}.

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
-type key_password()              :: string().
-type cipher_suites()             :: ciphers().    
-type ciphers()                   :: [erl_cipher_suite()] |
                                     string(). % (according to old API) exported
-type cipher_filters()            :: list({key_exchange | cipher | mac | prf,
                                        algo_filter()}). % exported
-type algo_filter()               :: fun((kex_algo()|cipher()|hash()|aead|default_prf) -> true | false).
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
-type signature_algs()           ::  [{hash(), sign_algo()}].
-type signature_schemes()        ::  [sign_scheme()].
-type supported_groups()         ::  [group()].
-type custom_user_lookup()       ::  {Lookupfun :: fun(), UserState :: any()}.
-type padding_check()            :: boolean(). 
-type beast_mitigation()         :: one_n_minus_one | zero_n | disabled.
-type srp_identity()             :: {Username :: string(), Password :: string()}.
-type psk_identity()             :: string().
-type log_alert()                :: boolean().
-type logging_level()            :: logger:level().
-type client_session_tickets()   :: disabled | manual | auto.
-type server_session_tickets()   :: disabled | stateful | stateless.
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
                                {signature_algs, client_signature_algs()} |
                                {fallback, fallback()} |
                                {session_tickets, client_session_tickets()} |
                                {use_ticket, use_ticket()} |
                                {ocsp_stapling, ocsp_stapling()} |
                                {ocsp_responder_certs, ocsp_responder_certs()} |
                                {ocsp_nonce, ocsp_nonce()}.

-type client_verify_type()       :: verify_type().
-type client_reuse_session()     :: session_id().
-type client_reuse_sessions()    :: boolean() | save.
-type client_cacerts()           :: [public_key:der_encoded()].
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
-type sni()                      :: HostName :: hostname() | disable. 
-type max_fragment_length()      :: undefined | 512 | 1024 | 2048 | 4096.
-type client_signature_algs()    :: signature_algs().
-type fallback()                 :: boolean().
-type ssl_imp()                  :: new | old.
-type ocsp_stapling()            :: boolean().
-type ocsp_responder_certs()     :: [public_key:der_encoded()].
-type ocsp_nonce()               :: boolean().

%% -------------------------------------------------------------------------------------------------------

-type server_option()        :: {cacerts, server_cacerts()} |
                                {cacertfile, server_cafile()} |
                                {dh, dh_der()} |
                                {dhfile, dh_file()} |
                                {verify, server_verify_type()} |
                                {fail_if_no_peer_cert, fail_if_no_peer_cert()} |
                                {reuse_sessions, server_reuse_sessions()} |
                                {reuse_session, server_reuse_session()} |
                                {alpn_preferred_protocols, server_alpn()} |
                                {next_protocols_advertised, server_next_protocol()} |
                                {psk_identity, server_psk_identity()} |
                                {honor_cipher_order, boolean()} |
                                {sni_hosts, sni_hosts()} |
                                {sni_fun, sni_fun()} |
                                {honor_cipher_order, honor_cipher_order()} |
                                {honor_ecc_order, honor_ecc_order()} |
                                {client_renegotiation, client_renegotiation()}|
                                {signature_algs, server_signature_algs()} |
                                {session_tickets, server_session_tickets()} |
                                {anti_replay, anti_replay()}.

-type server_cacerts()           :: [public_key:der_encoded()].
-type server_cafile()            :: file:filename().
-type server_alpn()              :: [app_level_protocol()].
-type server_next_protocol()     :: [app_level_protocol()].
-type server_psk_identity()      :: psk_identity().
-type dh_der()                   :: binary().
-type dh_file()                  :: file:filename().
-type server_verify_type()       :: verify_type().
-type fail_if_no_peer_cert()     :: boolean().
-type server_signature_algs()    :: signature_algs().
-type server_reuse_session()     :: fun().
-type server_reuse_sessions()    :: boolean().
-type sni_hosts()                :: [{hostname(), [server_option() | common_option()]}].
-type sni_fun()                  :: fun().
-type honor_cipher_order()       :: boolean().
-type honor_ecc_order()          :: boolean().
-type client_renegotiation()     :: boolean().
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
                                  sni => hostname()}. % exported
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

connect(Socket, SslOptions) when is_port(Socket) ->
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

connect(Socket, SslOptions0, Timeout) when is_port(Socket),
                                           (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    CbInfo = handle_option_cb_info(SslOptions0, tls),

    Transport = element(1, CbInfo),
    EmulatedOptions = tls_socket:emulated_options(),
    {ok, SocketValues} = tls_socket:getopts(Transport, Socket, EmulatedOptions),
    try handle_options(SslOptions0 ++ SocketValues, client) of
	{ok, Config} ->
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
	    tls_connection ->
		tls_socket:connect(Host,Port,Config,Timeout);
	    dtls_connection ->
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
	{ok, Config} = handle_options(Options0, server),
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
	tls_connection ->
	    tls_socket:accept(ListenSocket, Config, Timeout);
	dtls_connection ->
	    dtls_socket:accept(ListenSocket, Config, Timeout)
    end.
  
%%--------------------------------------------------------------------
%%
%% Description: Performs accept on an ssl listen socket. e.i. performs
%%              ssl handshake.
%%--------------------------------------------------------------------
-spec ssl_accept(SslSocket) ->
                        ok |
                        {error, Reason} when
      SslSocket :: sslsocket(),
      Reason :: closed | timeout | error_alert().

ssl_accept(ListenSocket) ->
    ssl_accept(ListenSocket, [], infinity).

-spec ssl_accept(Socket, TimeoutOrOptions) ->
			ok |
                        {ok, sslsocket()} | {error, Reason} when
      Socket :: sslsocket() | socket(),
      TimeoutOrOptions :: timeout() | [tls_server_option()],
      Reason :: timeout | closed | {options, any()} | error_alert().

ssl_accept(Socket, Timeout)  when  (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    ssl_accept(Socket, [], Timeout);
ssl_accept(ListenSocket, SslOptions) when is_port(ListenSocket) ->
    ssl_accept(ListenSocket, SslOptions, infinity);
ssl_accept(Socket, Timeout) ->
    ssl_accept(Socket, [], Timeout).

-spec ssl_accept(Socket, Options, Timeout) ->
			ok | {ok, sslsocket()} | {error, Reason} when
      Socket :: sslsocket() | socket(),
      Options :: [tls_server_option()],
      Timeout :: timeout(),
      Reason :: timeout | closed | {options, any()} | error_alert().

ssl_accept(Socket, SslOptions, Timeout) when is_port(Socket) ->
    handshake(Socket, SslOptions, Timeout);
ssl_accept(Socket, SslOptions, Timeout) ->
     case handshake(Socket, SslOptions, Timeout) of
        {ok, _} ->
            ok;
        Error ->
            Error
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
    ssl_connection:handshake(Socket, Timeout);

%% If Socket is a ordinary socket(): upgrades a gen_tcp, or equivalent, socket to
%% an SSL socket, that is, performs the SSL/TLS server-side handshake and returns
%% the SSL socket.
%%
%% If Socket is an sslsocket(): provides extra SSL/TLS/DTLS options to those
%% specified in ssl:listen/2 and then performs the SSL/TLS/DTLS handshake.
handshake(ListenSocket, SslOptions)  when is_port(ListenSocket) ->
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
	ssl_connection:handshake(Socket, {SslOpts, 
					  tls_socket:emulated_socket_options(EmOpts, #socket_options{})}, Timeout)
    catch
	Error = {error, _Reason} -> Error
    end;
handshake(#sslsocket{pid = [Pid|_], fd = {_, _, _}} = Socket, SslOpts, Timeout) when
      (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity)->
    try
        {ok, EmOpts, _} = dtls_packet_demux:get_all_opts(Pid),
	ssl_connection:handshake(Socket, {SslOpts,  
                                          tls_socket:emulated_socket_options(EmOpts, #socket_options{})}, Timeout)
    catch
	Error = {error, _Reason} -> Error
    end;
handshake(Socket, SslOptions, Timeout) when is_port(Socket),
                                            (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    CbInfo = handle_option_cb_info(SslOptions, tls),

    Transport = element(1, CbInfo),
    EmulatedOptions = tls_socket:emulated_options(),
    {ok, SocketValues} = tls_socket:getopts(Transport, Socket, EmulatedOptions),
    ConnetionCb = connection_cb(SslOptions),
    try handle_options(SslOptions ++ SocketValues, server) of
	{ok, #config{transport_info = CbInfo, ssl = SslOpts, emulated = EmOpts}} ->
	    ok = tls_socket:setopts(Transport, Socket, tls_socket:internal_inet_values()),
	    {ok, Port} = tls_socket:port(Transport, Socket),
	    ssl_connection:handshake(ConnetionCb, Port, Socket,
                                     {SslOpts, 
                                      tls_socket:emulated_socket_options(EmOpts, #socket_options{}), undefined},
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
%% Description: Continues the handshke possible with newly supplied options.
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
%% Description: Continues the handshke possible with newly supplied options.
%%--------------------------------------------------------------------
handshake_continue(Socket, SSLOptions, Timeout) ->
    ssl_connection:handshake_continue(Socket, SSLOptions, Timeout).
%%--------------------------------------------------------------------
-spec  handshake_cancel(#sslsocket{}) -> any().
%%
%% Description: Cancels the handshakes sending a close alert.
%%--------------------------------------------------------------------
handshake_cancel(Socket) ->
    ssl_connection:handshake_cancel(Socket).

%%--------------------------------------------------------------------
-spec  close(SslSocket) -> ok | {error, Reason} when
      SslSocket :: sslsocket(),
      Reason :: any().
%%
%% Description: Close an ssl connection
%%--------------------------------------------------------------------
close(#sslsocket{pid = [Pid|_]}) when is_pid(Pid) ->
    ssl_connection:close(Pid, {close, ?DEFAULT_TIMEOUT});
close(#sslsocket{pid = {dtls, #config{dtls_handler = {_, _}}}} = DTLSListen) ->
    dtls_socket:close(DTLSListen);
close(#sslsocket{pid = {ListenSocket, #config{transport_info={Transport,_,_,_,_}}}}) ->
    Transport:close(ListenSocket).

%%--------------------------------------------------------------------
-spec  close(SslSocket, How) -> ok | {ok, port()} | {error,Reason} when
      SslSocket :: sslsocket(),
      How :: timeout() | {NewController::pid(), timeout()},
      Reason :: any().
%%
%% Description: Close an ssl connection
%%--------------------------------------------------------------------
close(#sslsocket{pid = [TLSPid|_]},
      {Pid, Timeout} = DownGrade) when is_pid(TLSPid),
				       is_pid(Pid),
				       (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    case ssl_connection:close(TLSPid, {close, DownGrade}) of
        ok -> %% In normal close {error, closed} is regarded as ok, as it is not interesting which side
            %% that got to do the actual close. But in the downgrade case only {ok, Port} is a sucess.
            {error, closed};
        Other ->
            Other
    end;
close(#sslsocket{pid = [TLSPid|_]}, Timeout) when is_pid(TLSPid),
					      (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    ssl_connection:close(TLSPid, {close, Timeout});
close(#sslsocket{pid = {dtls = ListenSocket, #config{transport_info={Transport,_,_,_,_}}}}, _) ->
    dtls_socket:close(Transport, ListenSocket);    
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
    ssl_connection:send(Pid, Data);
send(#sslsocket{pid = [_, Pid]}, Data) when is_pid(Pid) ->
    tls_sender:send_data(Pid,  erlang:iolist_to_iovec(Data));
send(#sslsocket{pid = {_, #config{transport_info={_, udp, _, _}}}}, _) ->
    {error,enotconn}; %% Emulate connection behaviour
send(#sslsocket{pid = {dtls,_}}, _) ->
    {error,enotconn};  %% Emulate connection behaviour
send(#sslsocket{pid = {ListenSocket, #config{transport_info = Info}}}, Data) ->
    Transport = element(1, Info),
    Transport:send(ListenSocket, Data). %% {error,enotconn}

%%--------------------------------------------------------------------
%%
%% Description: Receives data when active = false
%%--------------------------------------------------------------------
-spec recv(SslSocket, Length) -> {ok, Data} | {error, reason()} when
      SslSocket :: sslsocket(),
      Length :: integer(),
      Data :: binary() | list() | HttpPacket,
      HttpPacket :: any().

recv(Socket, Length) ->
    recv(Socket, Length, infinity).

-spec recv(SslSocket, Length, Timeout) -> {ok, Data} | {error, reason()} when
      SslSocket :: sslsocket(),
      Length :: integer(),
      Data :: binary() | list() | HttpPacket,
      Timeout :: timeout(),
      HttpPacket :: any().

recv(#sslsocket{pid = [Pid|_]}, Length, Timeout) when is_pid(Pid),
						  (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity)->
    ssl_connection:recv(Pid, Length, Timeout);
recv(#sslsocket{pid = {dtls,_}}, _, _) ->
    {error,enotconn};
recv(#sslsocket{pid = {Listen,
		       #config{transport_info = Info}}},_,_) when is_port(Listen)->
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
    ssl_connection:new_user(Pid, NewOwner);
controlling_process(#sslsocket{pid = {dtls, _}},
		    NewOwner) when is_pid(NewOwner) ->
    ok; %% Meaningless but let it be allowed to conform with TLS 
controlling_process(#sslsocket{pid = {Listen,
				      #config{transport_info = {Transport,_,_,_,_}}}},
		    NewOwner) when is_port(Listen),
				   is_pid(NewOwner) ->
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
    case ssl_connection:connection_information(Pid, false) of
	{ok, Info} ->
	    {ok, [Item || Item = {_Key, Value} <- Info,  Value =/= undefined]};
	Error ->
            Error
    end;
connection_information(#sslsocket{pid = {Listen, _}}) when is_port(Listen) -> 
    {error, enotconn};
connection_information(#sslsocket{pid = {dtls,_}}) ->
    {error,enotconn}. 

%%--------------------------------------------------------------------
-spec connection_information(SslSocket, Items) -> {ok, Result} | {error, reason()} when
      SslSocket :: sslsocket(),
      Items :: connection_info_items(),
      Result :: connection_info().
%%
%% Description: Return SSL information for the connection
%%--------------------------------------------------------------------
connection_information(#sslsocket{pid = [Pid|_]}, Items) when is_pid(Pid) -> 
    case ssl_connection:connection_information(Pid, include_security_info(Items)) of
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
      Cert :: binary().
%%
%% Description: Returns the peercert.
%%--------------------------------------------------------------------
peercert(#sslsocket{pid = [Pid|_]}) when is_pid(Pid) ->
    case ssl_connection:peer_certificate(Pid) of
	{ok, undefined} ->
	    {error, no_peercert};
        Result ->
	    Result
    end;
peercert(#sslsocket{pid = {dtls, _}}) ->
    {error, enotconn};
peercert(#sslsocket{pid = {Listen, _}}) when is_port(Listen) ->
    {error, enotconn}.

%%--------------------------------------------------------------------
-spec negotiated_protocol(SslSocket) -> {ok, Protocol} | {error, Reason} when
      SslSocket :: sslsocket(),
      Protocol :: binary(),
      Reason :: protocol_not_negotiated.
%%
%% Description: Returns the protocol that has been negotiated. If no
%% protocol has been negotiated will return {error, protocol_not_negotiated}
%%--------------------------------------------------------------------
negotiated_protocol(#sslsocket{pid = [Pid|_]}) when is_pid(Pid) ->
    ssl_connection:negotiated_protocol(Pid).

%%--------------------------------------------------------------------
-spec cipher_suites() -> [old_cipher_suite()] | [string()].
%%--------------------------------------------------------------------
cipher_suites() ->
    cipher_suites(erlang).
%%--------------------------------------------------------------------
-spec cipher_suites(Type) -> [old_cipher_suite() | string()] when
      Type :: erlang | openssl | all.

%% Description: Returns all supported cipher suites.
%%--------------------------------------------------------------------
cipher_suites(erlang) ->
    [ssl_cipher_format:suite_legacy(Suite) || Suite <- available_suites(default)];

cipher_suites(openssl) ->
    [ssl_cipher_format:suite_map_to_openssl_str(ssl_cipher_format:suite_bin_to_map(Suite)) ||
        Suite <- available_suites(default)];

cipher_suites(all) ->
    [ssl_cipher_format:suite_legacy(Suite) || Suite <- available_suites(all)].

%%--------------------------------------------------------------------
-spec cipher_suites(Description, Version) -> ciphers() when
      Description :: default | all | exclusive | anonymous,
      Version :: protocol_version().

%% Description: Returns all default and all supported cipher suites for a
%% TLS/DTLS version
%%--------------------------------------------------------------------
cipher_suites(Description, Version) when Version == 'tlsv1.3';
                                  Version == 'tlsv1.2';
                                  Version == 'tlsv1.1';
                                  Version == tlsv1 ->
    cipher_suites(Description, tls_record:protocol_version(Version));
cipher_suites(Description, Version)  when Version == 'dtlsv1.2';
                                   Version == 'dtlsv1'->
    cipher_suites(Description, dtls_record:protocol_version(Version));
cipher_suites(Description, Version) ->
    [ssl_cipher_format:suite_bin_to_map(Suite) || Suite <- supported_suites(Description, Version)].

%%--------------------------------------------------------------------
-spec cipher_suites(Description, Version, rfc | openssl) -> [string()] when
      Description :: default | all | exclusive | anonymous,
      Version :: protocol_version().

%% Description: Returns all default and all supported cipher suites for a
%% TLS/DTLS version
%%--------------------------------------------------------------------
cipher_suites(Description, Version, StringType) when  Version == 'tlsv1.3';
                                               Version == 'tlsv1.2';
                                               Version == 'tlsv1.1';
                                               Version == tlsv1 ->
    cipher_suites(Description, tls_record:protocol_version(Version), StringType);
cipher_suites(Description, Version, StringType)  when Version == 'dtlsv1.2';
                                               Version == 'dtlsv1'->
    cipher_suites(Description, dtls_record:protocol_version(Version), StringType);
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

%% Description: Make <Preferred> suites become the most prefered
%%      suites that is put them at the head of the cipher suite list
%%      and remove them from <Suites> if present. <Preferred> may be a
%%      list of cipher suits or a list of filters in which case the
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
%% least prefered suites that is put them at the end of the cipher suite list
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
-spec eccs() -> NamedCurves when
      NamedCurves :: [named_curve()].

%% Description: returns all supported curves across all versions
%%--------------------------------------------------------------------
eccs() ->
    Curves = tls_v1:ecc_curves(all), % only tls_v1 has named curves right now
    eccs_filter_supported(Curves).

%%--------------------------------------------------------------------
-spec eccs(Version) -> NamedCurves when
      Version :: protocol_version(),
      NamedCurves :: [named_curve()].

%% Description: returns the curves supported for a given version of
%% ssl/tls.
%%--------------------------------------------------------------------
eccs('dtlsv1') ->
    eccs('tlsv1.1');
eccs('dtlsv1.2') ->
    eccs('tlsv1.2');
eccs(Version) when Version == 'tlsv1.2';
                   Version == 'tlsv1.1';
                   Version == tlsv1 ->
    Curves = tls_v1:ecc_curves(all),
    eccs_filter_supported(Curves).

eccs_filter_supported(Curves) ->
    CryptoCurves = crypto:ec_curves(),
    lists:filter(fun(Curve) -> proplists:get_bool(Curve, CryptoCurves) end,
                 Curves).

%%--------------------------------------------------------------------
-spec groups() -> [group()].
%% Description: returns all supported groups (TLS 1.3 and later)
%%--------------------------------------------------------------------
groups() ->
    tls_v1:groups(4).

%%--------------------------------------------------------------------
-spec groups(default) -> [group()].
%% Description: returns the default groups (TLS 1.3 and later)
%%--------------------------------------------------------------------
groups(default) ->
    tls_v1:default_groups(4).

%%--------------------------------------------------------------------
-spec getopts(SslSocket, OptionNames) ->
		     {ok, [gen_tcp:option()]} | {error, reason()} when
      SslSocket :: sslsocket(),
      OptionNames :: [gen_tcp:option_name()].
%%
%% Description: Gets options
%%--------------------------------------------------------------------
getopts(#sslsocket{pid = [Pid|_]}, OptionTags) when is_pid(Pid), is_list(OptionTags) ->
    ssl_connection:get_opts(Pid, OptionTags);
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
                    ssl_connection:set_opts(Pid, Options);
                PacketOpt ->
                    case tls_sender:setopts(Sender, [{packet, PacketOpt}]) of
                        ok ->
                            ssl_connection:set_opts(Pid, Options);
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
	    ssl_connection:set_opts(Pid, Options)
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
getstat(#sslsocket{pid = {dtls, #config{transport_info = {Transport, _, _, _, _},
                                        dtls_handler = {Listner, _}}}},
        Options) when is_list(Options) ->
    dtls_socket:getstat(Transport, Listner, Options);
getstat(#sslsocket{pid = {Listen,  #config{transport_info = {Transport, _, _, _, _}}}},
        Options) when is_port(Listen), is_list(Options) ->
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
shutdown(#sslsocket{pid = {Listen, #config{transport_info = Info}}},
	 How) when is_port(Listen) ->
    Transport = element(1, Info),
    Transport:shutdown(Listen, How);
shutdown(#sslsocket{pid = {dtls,_}},_) ->
    {error, enotconn};
shutdown(#sslsocket{pid = [Pid|_]}, How) when is_pid(Pid) ->
    ssl_connection:shutdown(Pid, How).

%%--------------------------------------------------------------------
-spec sockname(SslSocket) ->
                      {ok, {Address, Port}} | {error, reason()} when
      SslSocket :: sslsocket(),
      Address :: inet:ip_address(),
      Port :: inet:port_number().
%%
%% Description: Same as inet:sockname/1
%%--------------------------------------------------------------------
sockname(#sslsocket{pid = {Listen,  #config{transport_info = {Transport,_,_,_,_}}}}) when is_port(Listen) ->
    tls_socket:sockname(Transport, Listen);
sockname(#sslsocket{pid = {dtls, #config{dtls_handler = {Pid, _}}}}) ->
    dtls_packet_demux:sockname(Pid);
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

    AvailableTLSVsns = [Vsn || Vsn <- ImplementedTLSVsns, TLSCryptoSupported(tls_record:protocol_version(Vsn))],
    AvailableDTLSVsns = [Vsn || Vsn <- ImplementedDTLSVsns, DTLSCryptoSupported(dtls_record:protocol_version(Vsn))],
                                    
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
renegotiate(#sslsocket{pid = [Pid, Sender |_]}) when is_pid(Pid),
                                                     is_pid(Sender) ->
    case tls_sender:renegotiate(Sender) of
        {ok, Write} ->
            tls_connection:renegotiation(Pid, Write);
        Error ->
            Error
    end;
renegotiate(#sslsocket{pid = [Pid |_]}) when is_pid(Pid) ->
    ssl_connection:renegotiation(Pid);
renegotiate(#sslsocket{pid = {dtls,_}}) ->
    {error, enotconn};
renegotiate(#sslsocket{pid = {Listen,_}}) when is_port(Listen) ->
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
    tls_connection:send_key_update(Sender, Type);
update_keys(_, Type) ->
    {error, {illegal_parameter, Type}}.

%%--------------------------------------------------------------------
-spec prf(SslSocket, Secret, Label, Seed, WantedLength) ->
                 {ok, binary()} | {error, reason()} when
      SslSocket :: sslsocket(),
      Secret :: binary() | 'master_secret',
      Label::binary(),
      Seed :: [binary() | prf_random()],
      WantedLength :: non_neg_integer().
%%
%% Description: use a ssl sessions TLS PRF to generate key material
%%--------------------------------------------------------------------
prf(#sslsocket{pid = [Pid|_]},
    Secret, Label, Seed, WantedLength) when is_pid(Pid) ->
    ssl_connection:prf(Pid, Secret, Label, Seed, WantedLength);
prf(#sslsocket{pid = {dtls,_}}, _,_,_,_) ->
    {error, enotconn};
prf(#sslsocket{pid = {Listen,_}}, _,_,_,_) when is_port(Listen) ->
    {error, enotconn}.

%%--------------------------------------------------------------------
-spec clear_pem_cache() -> ok.
%%
%% Description: Clear the PEM cache
%%--------------------------------------------------------------------
clear_pem_cache() ->
    ssl_pem_cache:clear().

%%---------------------------------------------------------------
-spec format_error({error, Reason}) -> string() when
      Reason :: any().
%%
%% Description: Creates error string.
%%--------------------------------------------------------------------
format_error({error, Reason}) ->
    format_error(Reason);
format_error(Reason) when is_list(Reason) ->
    Reason;
format_error(closed) ->
    "TLS connection is closed";
format_error({tls_alert, {_, Description}}) ->
    Description;
format_error({options,{FileType, File, Reason}}) when FileType == cacertfile;
						      FileType == certfile;
						      FileType == keyfile;
						      FileType == dhfile ->
    Error = file_error_format(Reason),
    file_desc(FileType) ++ File ++ ": " ++ Error;
format_error({options, {socket_options, Option, Error}}) ->
    lists:flatten(io_lib:format("Invalid transport socket option ~p: ~s", [Option, format_error(Error)]));
format_error({options, {socket_options, Option}}) ->
    lists:flatten(io_lib:format("Invalid socket option: ~p", [Option]));
format_error({options, Options}) ->
    lists:flatten(io_lib:format("Invalid TLS option: ~p", [Options]));

format_error(Error) ->
    case inet:format_error(Error) of
        "unknown POSIX" ++ _ ->
            unexpected_format(Error);
        Other ->
            Other
    end.

tls_version({3, _} = Version) ->
    Version;
tls_version({254, _} = Version) ->
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
-spec str_to_suite(CipherSuiteName) -> erl_cipher_suite() when
      CipherSuiteName :: string() | {error, {not_recognized, CipherSuiteName :: string()}}.
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
  
%% Possible filters out suites not supported by crypto 
available_suites(default) ->  
    Version = tls_record:highest_protocol_version([]),			  
    ssl_cipher:filter_suites(ssl_cipher:suites(Version));
available_suites(all) ->  
    Version = tls_record:highest_protocol_version([]),			  
    ssl_cipher:filter_suites(ssl_cipher:all_suites(Version)).

supported_suites(exclusive, {3,Minor}) ->
    tls_v1:exclusive_suites(Minor);
supported_suites(default, Version) ->  
    ssl_cipher:suites(Version);
supported_suites(all, Version) ->  
    ssl_cipher:all_suites(Version);
supported_suites(anonymous, Version) -> 
    ssl_cipher:anonymous_suites(Version).

do_listen(Port, #config{transport_info = {Transport, _, _, _,_}} = Config, tls_connection) ->
    tls_socket:listen(Transport, Port, Config);

do_listen(Port,  Config, dtls_connection) ->
    dtls_socket:listen(Port, Config).
	


-spec handle_options([any()], client | server) -> {ok, #config{}};
                    ([any()], ssl_options()) -> ssl_options().

handle_options(Opts, Role) ->
    handle_options(Opts, Role, undefined).   


%% Handle ssl options at handshake, handshake_continue
handle_options(Opts0, Role, InheritedSslOpts) when is_map(InheritedSslOpts) ->
    {SslOpts, _} = expand_options(Opts0, ?RULES),
    process_options(SslOpts, InheritedSslOpts, #{role => Role,
                                                 rules => ?RULES});
%% Handle all options in listen, connect and handshake
handle_options(Opts0, Role, Host) ->
    {SslOpts0, SockOpts} = expand_options(Opts0, ?RULES),

    %% Ensure all options are evaluated at startup
    SslOpts1 = add_missing_options(SslOpts0, ?RULES),
    SslOpts = #{protocol := Protocol}
        = process_options(SslOpts1,
                          #{},
                          #{role => Role,
                            host => Host,
                            rules => ?RULES}),
    
    %% Handle special options
    {Sock, Emulated} = emulated_options(Protocol, SockOpts),
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


%% process_options(SSLOptions, OptionsMap, Env) where
%% SSLOptions is the following tuple:
%%   {InOptions, SkippedOptions, Counter}
%%
%% The list of options is processed in multiple passes. When
%% processing an option all dependencies must already be resolved.
%% If there are unresolved dependencies the option will be
%% skipped and processed in a subsequent pass.
%% Counter is equal to the number of unprocessed options at
%% the beginning of a pass. Its value must monotonically decrease
%% after each successful pass.
%% If the value of the counter is unchanged at the end of a pass,
%% the processing stops due to faulty input data.
process_options({[], [], _}, OptionsMap, _Env) ->
    OptionsMap;
process_options({[], [_|_] = Skipped, Counter}, OptionsMap, Env)
  when length(Skipped) < Counter ->
    %% Continue handling options if current pass was successful
    process_options({Skipped, [], length(Skipped)}, OptionsMap, Env);
process_options({[], [_|_], _Counter}, _OptionsMap, _Env) ->
    throw({error, faulty_configuration});
process_options({[{K0,V} = E|T], S, Counter}, OptionsMap0, Env) ->
    K = maybe_map_key_internal(K0),
    case check_dependencies(K, OptionsMap0, Env) of
        true ->
            OptionsMap = handle_option(K, V, OptionsMap0, Env),
            process_options({T, S, Counter}, OptionsMap, Env);
        false ->
            %% Skip option for next pass
            process_options({T, [E|S], Counter}, OptionsMap0, Env)
    end.

handle_option(anti_replay = Option, unbound, OptionsMap, #{rules := Rules}) ->
    Value = validate_option(Option, default_value(Option, Rules)),
    OptionsMap#{Option => Value};
handle_option(anti_replay = Option, Value0,
              #{session_tickets := SessionTickets,
                versions := Versions} = OptionsMap, #{rules := Rules}) ->
    assert_option_dependency(Option, versions, Versions, ['tlsv1.3']),
    assert_option_dependency(Option, session_tickets, [SessionTickets], [stateless]),
    case SessionTickets of
        stateless ->
            Value = validate_option(Option, Value0),
            OptionsMap#{Option => Value};
        _ ->
            OptionsMap#{Option => default_value(Option, Rules)}
    end;
handle_option(beast_mitigation = Option, unbound, OptionsMap, #{rules := Rules}) ->
    Value = validate_option(Option, default_value(Option, Rules)),
    OptionsMap#{Option => Value};
handle_option(beast_mitigation = Option, Value0,  #{versions := Versions} = OptionsMap, _Env) ->
    assert_option_dependency(Option, versions, Versions, ['tlsv1']),
    Value = validate_option(Option, Value0),
    OptionsMap#{Option => Value};
handle_option(cacertfile = Option, unbound, #{cacerts := CaCerts,
                                              verify := Verify,
                                              verify_fun := VerifyFun} = OptionsMap, _Env)
  when Verify =:= verify_none orelse
       Verify =:= 0 ->
    Value = validate_option(Option, ca_cert_default(verify_none, VerifyFun, CaCerts)),
    OptionsMap#{Option => Value};
handle_option(cacertfile = Option, unbound, #{cacerts := CaCerts,
                                              verify := Verify,
                                              verify_fun := VerifyFun} = OptionsMap, _Env)
  when Verify =:= verify_peer orelse
       Verify =:= 1 orelse
       Verify =:= 2 ->
    Value =  validate_option(Option, ca_cert_default(verify_peer, VerifyFun, CaCerts)),
    OptionsMap#{Option => Value};
handle_option(cacertfile = Option, Value0, OptionsMap, _Env) ->
    Value = validate_option(Option, Value0),
    OptionsMap#{Option => Value};
handle_option(ciphers = Option, unbound, #{versions := Versions} = OptionsMap, #{rules := Rules}) ->
    Value = handle_cipher_option(default_value(Option, Rules), Versions),
    OptionsMap#{Option => Value};
handle_option(ciphers = Option, Value0, #{versions := Versions} = OptionsMap, _Env) ->
    Value = handle_cipher_option(Value0, Versions),
    OptionsMap#{Option => Value};
handle_option(client_renegotiation = Option, unbound, OptionsMap, #{role := Role}) ->
    Value = default_option_role(server, true, Role),
    OptionsMap#{Option => Value};
handle_option(client_renegotiation = Option, Value0,
              #{versions := Versions} = OptionsMap, #{role := Role}) ->
    assert_role(server_only, Role, Option, Value0),
    assert_option_dependency(Option, versions, Versions,
                             ['tlsv1','tlsv1.1','tlsv1.2']),
    Value = validate_option(Option, Value0),
    OptionsMap#{Option => Value};
handle_option(eccs = Option, unbound, #{versions := [HighestVersion|_]} = OptionsMap, #{rules := _Rules}) ->
    Value = handle_eccs_option(eccs(), HighestVersion),
    OptionsMap#{Option => Value};
handle_option(eccs = Option, Value0, #{versions := [HighestVersion|_]} = OptionsMap, _Env) ->
    Value = handle_eccs_option(Value0, HighestVersion),
    OptionsMap#{Option => Value};
handle_option(fallback = Option, unbound, OptionsMap, #{role := Role}) ->
    Value = default_option_role(client, false, Role),
    OptionsMap#{Option => Value};
handle_option(fallback = Option, Value0, OptionsMap, #{role := Role}) ->
    assert_role(client_only, Role, Option, Value0),
    Value = validate_option(Option, Value0),
    OptionsMap#{Option => Value};
handle_option(honor_cipher_order = Option, unbound, OptionsMap, #{role := Role}) ->
    Value = default_option_role(server, false, Role),
    OptionsMap#{Option => Value};
handle_option(honor_cipher_order = Option, Value0, OptionsMap, #{role := Role}) ->
    assert_role(server_only, Role, Option, Value0),
    Value = validate_option(Option, Value0),
    OptionsMap#{Option => Value};
handle_option(honor_ecc_order = Option, unbound, OptionsMap, #{role := Role}) ->
    Value = default_option_role(server, false, Role),
    OptionsMap#{Option => Value};
handle_option(honor_ecc_order = Option, Value0, OptionsMap, #{role := Role}) ->
    assert_role(server_only, Role, Option, Value0),
    Value = validate_option(Option, Value0),
    OptionsMap#{Option => Value};
handle_option(keyfile = Option, unbound, #{certfile := CertFile} = OptionsMap, _Env) ->
    Value = validate_option(Option, CertFile),
    OptionsMap#{Option => Value};
handle_option(key_update_at = Option, unbound, OptionsMap, #{rules := Rules}) ->
    Value = validate_option(Option, default_value(Option, Rules)),
    OptionsMap#{Option => Value};
handle_option(key_update_at = Option, Value0, #{versions := Versions} = OptionsMap, _Env) ->
    assert_option_dependency(Option, versions, Versions, ['tlsv1.3']),
    Value = validate_option(Option, Value0),
    OptionsMap#{Option => Value};
handle_option(next_protocols_advertised = Option, unbound, OptionsMap,
              #{rules := Rules}) ->
    Value = validate_option(Option, default_value(Option, Rules)),
    OptionsMap#{Option => Value};
handle_option(next_protocols_advertised = Option, Value0,
              #{versions := Versions} = OptionsMap, _Env) ->
    assert_option_dependency(next_protocols_advertised, versions, Versions,
                             ['tlsv1','tlsv1.1','tlsv1.2']),
    Value = validate_option(Option, Value0),
    OptionsMap#{Option => Value};
handle_option(next_protocol_selector = Option, unbound, OptionsMap, #{rules := Rules}) ->
    Value = default_value(Option, Rules),
    OptionsMap#{Option => Value};
handle_option(next_protocol_selector = Option, Value0,
              #{versions := Versions} = OptionsMap, _Env) ->
    assert_option_dependency(client_preferred_next_protocols, versions, Versions,
                             ['tlsv1','tlsv1.1','tlsv1.2']),
    Value = make_next_protocol_selector(
              validate_option(client_preferred_next_protocols, Value0)),
    OptionsMap#{Option => Value};
handle_option(padding_check = Option, unbound, OptionsMap, #{rules := Rules}) ->
    Value = validate_option(Option, default_value(Option, Rules)),
    OptionsMap#{Option => Value};
handle_option(padding_check = Option, Value0,  #{versions := Versions} = OptionsMap, _Env) ->
    assert_option_dependency(Option, versions, Versions, ['tlsv1']),
    Value = validate_option(Option, Value0),
    OptionsMap#{Option => Value};
handle_option(psk_identity = Option, unbound, OptionsMap, #{rules := Rules}) ->
    Value = validate_option(Option, default_value(Option, Rules)),
    OptionsMap#{Option => Value};
handle_option(psk_identity = Option, Value0, #{versions := Versions} = OptionsMap, _Env) ->
    assert_option_dependency(Option, versions, Versions,
                             ['tlsv1','tlsv1.1','tlsv1.2']),
    Value = validate_option(Option, Value0),
    OptionsMap#{Option => Value};
handle_option(secure_renegotiate = Option, unbound, OptionsMap, #{rules := Rules}) ->
    Value = validate_option(Option, default_value(Option, Rules)),
    OptionsMap#{Option => Value};
handle_option(secure_renegotiate= Option, Value0,
              #{versions := Versions} = OptionsMap, _Env) ->
    assert_option_dependency(secure_renegotiate, versions, Versions,
                             ['tlsv1','tlsv1.1','tlsv1.2']),
    Value = validate_option(Option, Value0),
    OptionsMap#{Option => Value};
handle_option(reuse_session = Option, unbound, OptionsMap, #{role := Role}) ->
    Value =
        case Role of
            client ->
                undefined;
            server ->
                fun(_, _, _, _) -> true end
        end,
    OptionsMap#{Option => Value};
handle_option(reuse_session = Option, Value0,
              #{versions := Versions} = OptionsMap, _Env) ->
    assert_option_dependency(reuse_session, versions, Versions,
                             ['tlsv1','tlsv1.1','tlsv1.2']),
    Value = validate_option(Option, Value0),
    OptionsMap#{Option => Value};
%% TODO: validate based on role
handle_option(reuse_sessions = Option, unbound, OptionsMap, #{rules := Rules}) ->
    Value = validate_option(Option, default_value(Option, Rules)),
    OptionsMap#{Option => Value};
handle_option(reuse_sessions = Option, Value0,
              #{versions := Versions} = OptionsMap, _Env) ->
    assert_option_dependency(reuse_sessions, versions, Versions,
                             ['tlsv1','tlsv1.1','tlsv1.2']),
    Value = validate_option(Option, Value0),
    OptionsMap#{Option => Value};
handle_option(server_name_indication = Option, unbound, OptionsMap, #{host := Host,
                                                                      role := Role}) ->
    Value = default_option_role(client, server_name_indication_default(Host), Role),
    OptionsMap#{Option => Value};
handle_option(server_name_indication = Option, Value0, OptionsMap, _Env) ->
    Value = validate_option(Option, Value0),
    OptionsMap#{Option => Value};
handle_option(session_tickets = Option, unbound, OptionsMap, #{rules := Rules}) ->
    Value = validate_option(Option, default_value(Option, Rules)),
    OptionsMap#{Option => Value};
handle_option(session_tickets = Option, Value0, #{versions := Versions} = OptionsMap, #{role := Role}) ->
    assert_option_dependency(Option, versions, Versions, ['tlsv1.3']),
    assert_role_value(Role, Option, Value0, [disabled, stateful, stateless], [disabled, manual, auto]),
    Value = validate_option(Option, Value0),
    OptionsMap#{Option => Value};
handle_option(signature_algs = Option, unbound, #{versions := [HighestVersion|_]} = OptionsMap, #{role := Role}) ->
    Value =
        handle_hashsigns_option(
          default_option_role_sign_algs(
            server,
            tls_v1:default_signature_algs(HighestVersion),
            Role,
            HighestVersion),
          tls_version(HighestVersion)),
    OptionsMap#{Option => Value};
handle_option(signature_algs = Option, Value0, #{versions := [HighestVersion|_]} = OptionsMap, _Env) ->
    Value = handle_hashsigns_option(Value0, tls_version(HighestVersion)),
    OptionsMap#{Option => Value};
handle_option(signature_algs_cert = Option, unbound, #{versions := [HighestVersion|_]} = OptionsMap, _Env) ->
    %% Do not send by default
    Value = handle_signature_algorithms_option(undefined, tls_version(HighestVersion)),
    OptionsMap#{Option => Value};
handle_option(signature_algs_cert = Option, Value0, #{versions := [HighestVersion|_]} = OptionsMap, _Env) ->
    Value = handle_signature_algorithms_option(Value0, tls_version(HighestVersion)),
    OptionsMap#{Option => Value};
handle_option(sni_fun = Option, unbound, OptionsMap, #{rules := Rules}) ->
    Value = default_value(Option, Rules),
    OptionsMap#{Option => Value};
handle_option(sni_fun = Option, Value0, OptionsMap, _Env) ->
    validate_option(Option, Value0),
    OptHosts = maps:get(sni_hosts, OptionsMap, undefined),
    Value =
        case {Value0, OptHosts} of
            {undefined, _} ->
                Value0;
            {_, []} ->
                Value0;
            _ ->
                throw({error, {conflict_options, [sni_fun, sni_hosts]}})
        end,
    OptionsMap#{Option => Value};
handle_option(srp_identity = Option, unbound, OptionsMap, #{rules := Rules}) ->
    Value = validate_option(Option, default_value(Option, Rules)),
    OptionsMap#{Option => Value};
handle_option(srp_identity = Option, Value0,
              #{versions := Versions} = OptionsMap, _Env) ->
    assert_option_dependency(srp_identity, versions, Versions,
                             ['tlsv1','tlsv1.1','tlsv1.2']),
    Value = validate_option(Option, Value0),
    OptionsMap#{Option => Value};
handle_option(supported_groups = Option, unbound, #{versions := [HighestVersion|_]} = OptionsMap, #{rules := _Rules}) ->
    Value = handle_supported_groups_option(groups(default), HighestVersion),
    OptionsMap#{Option => Value};
handle_option(supported_groups = Option, Value0,
              #{versions := [HighestVersion|_] = Versions} = OptionsMap, _Env) ->
    assert_option_dependency(Option, versions, Versions, ['tlsv1.3']),
    Value = handle_supported_groups_option(Value0, HighestVersion),
    OptionsMap#{Option => Value};
handle_option(use_ticket = Option, unbound, OptionsMap, #{rules := Rules}) ->
    Value = validate_option(Option, default_value(Option, Rules)),
    OptionsMap#{Option => Value};
handle_option(use_ticket = Option, Value0,
              #{versions := Versions} = OptionsMap, _Env) ->
    assert_option_dependency(Option, versions, Versions, ['tlsv1.3']),
    Value = validate_option(Option, Value0),
    OptionsMap#{Option => Value};
handle_option(user_lookup_fun = Option, unbound, OptionsMap, #{rules := Rules}) ->
    Value = validate_option(Option, default_value(Option, Rules)),
    OptionsMap#{Option => Value};
handle_option(user_lookup_fun = Option, Value0,
              #{versions := Versions} = OptionsMap, _Env) ->
    assert_option_dependency(Option, versions, Versions, ['tlsv1','tlsv1.1','tlsv1.2']),
    Value = validate_option(Option, Value0),
    OptionsMap#{Option => Value};
handle_option(verify = Option, unbound, OptionsMap, #{rules := Rules}) ->
    handle_verify_option(default_value(Option, Rules), OptionsMap);
handle_option(verify = _Option, Value, OptionsMap, _Env) ->
    handle_verify_option(Value, OptionsMap);
handle_option(verify_fun = Option, unbound, #{verify := Verify} = OptionsMap, #{rules := Rules})
  when Verify =:= verify_none ->
    OptionsMap#{Option => default_value(Option, Rules)};
handle_option(verify_fun = Option, unbound, #{verify := Verify} = OptionsMap, _Env)
  when Verify =:= verify_peer ->
    OptionsMap#{Option => undefined};
handle_option(verify_fun = Option, Value0, OptionsMap, _Env) ->
    Value = validate_option(Option, Value0),
    OptionsMap#{Option => Value};
handle_option(versions = Option, unbound, #{protocol := Protocol} = OptionsMap, _Env) ->
    RecordCb = record_cb(Protocol),
    Vsns0 = RecordCb:supported_protocol_versions(),
    Value = lists:sort(fun RecordCb:is_higher/2, Vsns0),
    OptionsMap#{Option => Value};
handle_option(versions = Option, Vsns0, #{protocol := Protocol} = OptionsMap, _Env) ->
    validate_option(versions, Vsns0),
    RecordCb = record_cb(Protocol),
    Vsns1 = [RecordCb:protocol_version(Vsn) || Vsn <- Vsns0],
    Value = lists:sort(fun RecordCb:is_higher/2, Vsns1),
    OptionsMap#{Option => Value};
%% Special options
handle_option(cb_info = Option, unbound, #{protocol := Protocol} = OptionsMap, _Env) ->
    Default = default_cb_info(Protocol),
    validate_option(Option, Default),
    Value = handle_cb_info(Default),
    OptionsMap#{Option => Value};
handle_option(cb_info = Option, Value0, OptionsMap, _Env) ->
    validate_option(Option, Value0),
    Value = handle_cb_info(Value0),
    OptionsMap#{Option => Value};
%% Generic case
handle_option(Option, unbound, OptionsMap, #{rules := Rules}) ->
    Value = validate_option(Option, default_value(Option, Rules)),
    OptionsMap#{Option => Value};
handle_option(Option, Value0, OptionsMap, _Env) ->
    Value = validate_option(Option, Value0),
    OptionsMap#{Option => Value}.

handle_option_cb_info(Options, Protocol) ->
    Value = proplists:get_value(cb_info, Options, default_cb_info(Protocol)),
    #{cb_info := CbInfo} = handle_option(cb_info, Value, #{protocol => Protocol}, #{}),
    CbInfo.


maybe_map_key_internal(client_preferred_next_protocols) ->
    next_protocol_selector;
maybe_map_key_internal(K) ->
    K.


maybe_map_key_external(next_protocol_selector) ->
    client_preferred_next_protocols;
maybe_map_key_external(K) ->
    K.


check_dependencies(K, OptionsMap, Env) ->
    Rules =  maps:get(rules, Env),
    Deps = get_dependencies(K, Rules),
    case Deps of
        [] ->
            true;
        L ->
            option_already_defined(K,OptionsMap) orelse
                dependecies_already_defined(L, OptionsMap)
    end.


%% Handle options that are not present in the map
get_dependencies(K, _) when K =:= cb_info orelse K =:= log_alert->
    [];
get_dependencies(K, Rules) ->
    {_, Deps} = maps:get(K, Rules),
    Deps.


option_already_defined(K, Map) ->
    maps:get(K, Map, unbound) =/= unbound.


dependecies_already_defined(L, OptionsMap) ->
    Fun = fun (E) -> option_already_defined(E, OptionsMap) end,
    lists:all(Fun, L).


expand_options(Opts0, Rules) ->
    Opts1 = proplists:expand([{binary, [{mode, binary}]},
                      {list, [{mode, list}]}], Opts0),
    assert_proplist(Opts1),

    %% Remove depricated ssl_imp option
    Opts = proplists:delete(ssl_imp, Opts1),
    AllOpts = maps:keys(Rules),
    SockOpts = lists:foldl(fun(Key, PropList) -> proplists:delete(Key, PropList) end,
                           Opts,
                           AllOpts ++
                               [ssl_imp,                          %% TODO: remove ssl_imp
                                cb_info,
                                client_preferred_next_protocols,  %% next_protocol_selector
                                log_alert]),                      %% obsoleted by log_level

    SslOpts = {Opts -- SockOpts, [], length(Opts -- SockOpts)},
    {SslOpts, SockOpts}.


add_missing_options({L0, S, _C}, Rules) ->
    Fun = fun(K0, Acc) ->
                  K = maybe_map_key_external(K0),
                  case proplists:is_defined(K, Acc) of
                      true ->
                          Acc;
                      false ->
                          Default = unbound,
                          [{K, Default}|Acc]
                  end
          end,
    AllOpts = maps:keys(Rules),
    L = lists:foldl(Fun, L0, AllOpts),
    {L, S, length(L)}.


default_value(Key, Rules) ->
    {Default, _} = maps:get(Key, Rules, {undefined, []}),
    Default.


assert_role(client_only, client, _, _) ->
    ok;
assert_role(server_only, server, _, _) ->
    ok;
assert_role(client_only, _, _, undefined) ->
    ok;
assert_role(server_only, _, _, undefined) ->
    ok;
assert_role(Type, _, Key, _) ->
    throw({error, {option, Type, Key}}).


assert_role_value(client, Option, Value, _, ClientValues) ->
        case lists:member(Value, ClientValues) of
            true ->
                ok;
            false ->
                %% throw({error, {option, client, Option, Value, ClientValues}})
                throw({error, {options, role, {Option, {Value, {client, ClientValues}}}}})
        end;
assert_role_value(server, Option, Value, ServerValues, _) ->
        case lists:member(Value, ServerValues) of
            true ->
                ok;
            false ->
                %% throw({error, {option, server, Option, Value, ServerValues}})
                throw({error, {options, role, {Option, {Value, {server, ServerValues}}}}})
        end.

assert_option_dependency(Option, OptionDep, Values0, AllowedValues) ->
    case is_dtls_configured(Values0) of
        true ->
            %% TODO: Check option dependency for DTLS
            ok;
        false ->
            %% special handling for version
            Values =
                case OptionDep of
                    versions ->
                        lists:map(fun tls_record:protocol_version/1, Values0);
                    _ ->
                        Values0
                end,
            Set1 = sets:from_list(Values),
            Set2 = sets:from_list(AllowedValues),
            case sets:size(sets:intersection(Set1, Set2)) > 0 of
                true ->
                    ok;
                false ->
                    throw({error, {options, dependency,
                                   {Option, {OptionDep, AllowedValues}}}})
            end
    end.

is_dtls_configured(Versions) ->
    Fun = fun (Version) when Version =:= {254, 253} orelse
                             Version =:= {254, 255} ->
                  true;
              (_) ->
                  false
          end,
    lists:any(Fun, Versions).

validate_option(versions, Versions)  ->
    validate_versions(Versions, Versions);
validate_option(verify, Value)
  when Value == verify_none; Value == verify_peer ->
    Value;
validate_option(verify_fun, undefined)  ->
    undefined;
%% Backwards compatibility
validate_option(verify_fun, Fun) when is_function(Fun) ->
    {fun(_,{bad_cert, _} = Reason, OldFun) ->
	     case OldFun([Reason]) of
		 true ->
		     {valid, OldFun};
		 false ->
		     {fail, Reason}
	     end;
	(_,{extension, _}, UserState) ->
	     {unknown, UserState};
	(_, valid, UserState) ->
	     {valid, UserState};
	(_, valid_peer, UserState) ->
	     {valid, UserState}
     end, Fun};
validate_option(verify_fun, {Fun, _} = Value) when is_function(Fun) ->
   Value;
validate_option(partial_chain, Value) when is_function(Value) ->
    Value;
validate_option(fail_if_no_peer_cert, Value) when is_boolean(Value) ->
    Value;
validate_option(depth, Value) when is_integer(Value),
                                   Value >= 0, Value =< 255->
    Value;
validate_option(cert, Value) when Value == undefined;
                                 is_binary(Value) ->
    Value;
validate_option(certfile, undefined = Value) ->
    Value;
validate_option(certfile, Value) when is_binary(Value) ->
    Value;
validate_option(certfile, Value) when is_list(Value) ->
    binary_filename(Value);

validate_option(key, undefined) ->
    undefined;
validate_option(key, {KeyType, Value}) when is_binary(Value),
					    KeyType == rsa; %% Backwards compatibility
					    KeyType == dsa; %% Backwards compatibility
					    KeyType == 'RSAPrivateKey';
					    KeyType == 'DSAPrivateKey';
					    KeyType == 'ECPrivateKey';
					    KeyType == 'PrivateKeyInfo' ->
    {KeyType, Value};
validate_option(key, #{algorithm := _} = Value) ->
    Value;
validate_option(keyfile, undefined) ->
   <<>>;
validate_option(keyfile, Value) when is_binary(Value) ->
    Value;
validate_option(keyfile, Value) when is_list(Value), Value =/= "" ->
    binary_filename(Value);
validate_option(key_update_at, Value) when is_integer(Value) andalso
                                           Value > 0 ->
    Value;
validate_option(password, Value) when is_list(Value) ->
    Value;

validate_option(cacerts, Value) when Value == undefined;
				     is_list(Value) ->
    Value;
%% certfile must be present in some cases otherwhise it can be set
%% to the empty string.
validate_option(cacertfile, undefined) ->
   <<>>;
validate_option(cacertfile, Value) when is_binary(Value) ->
    Value;
validate_option(cacertfile, Value) when is_list(Value), Value =/= ""->
    binary_filename(Value);
validate_option(dh, Value) when Value == undefined;
				is_binary(Value) ->
    Value;
validate_option(dhfile, undefined = Value)  ->
    Value;
validate_option(dhfile, Value) when is_binary(Value) ->
    Value;
validate_option(dhfile, Value) when is_list(Value), Value =/= "" ->
    binary_filename(Value);
validate_option(psk_identity, undefined) ->
    undefined;
validate_option(psk_identity, Identity)
  when is_list(Identity), Identity =/= "", length(Identity) =< 65535 ->
    binary_filename(Identity);
validate_option(user_lookup_fun, undefined) ->
    undefined;
validate_option(user_lookup_fun, {Fun, _} = Value) when is_function(Fun, 3) ->
   Value;
validate_option(srp_identity, undefined) ->
    undefined;
validate_option(srp_identity, {Username, Password})
  when is_list(Username), is_list(Password), Username =/= "", length(Username) =< 255 ->
    {unicode:characters_to_binary(Username),
     unicode:characters_to_binary(Password)};

validate_option(reuse_session, undefined) ->
    undefined;
validate_option(reuse_session, Value) when is_function(Value) ->
    Value;
validate_option(reuse_session, Value) when is_binary(Value) ->
    Value;
validate_option(reuse_sessions, Value) when is_boolean(Value) ->
    Value;
validate_option(reuse_sessions, save = Value) ->
    Value;
validate_option(secure_renegotiate, Value) when is_boolean(Value) ->
    Value;
validate_option(client_renegotiation, Value) when is_boolean(Value) ->
    Value;
validate_option(renegotiate_at, Value) when is_integer(Value) ->
    erlang:min(Value, ?DEFAULT_RENEGOTIATE_AT);

validate_option(hibernate_after, undefined) -> %% Backwards compatibility
    infinity;
validate_option(hibernate_after, infinity) ->
    infinity;
validate_option(hibernate_after, Value) when is_integer(Value), Value >= 0 ->
    Value;

validate_option(erl_dist,Value) when is_boolean(Value) ->
    Value;
validate_option(Opt, Value) when Opt =:= alpn_advertised_protocols orelse Opt =:= alpn_preferred_protocols,
                                 is_list(Value) ->
    validate_binary_list(Opt, Value),
    Value;
validate_option(Opt, Value)
  when Opt =:= alpn_advertised_protocols orelse Opt =:= alpn_preferred_protocols,
       Value =:= undefined ->
    undefined;
validate_option(client_preferred_next_protocols, {Precedence, PreferredProtocols})
  when is_list(PreferredProtocols) ->
    validate_binary_list(client_preferred_next_protocols, PreferredProtocols),
    validate_npn_ordering(Precedence),
    {Precedence, PreferredProtocols, ?NO_PROTOCOL};
validate_option(client_preferred_next_protocols, {Precedence, PreferredProtocols, Default} = Value)
  when is_list(PreferredProtocols), is_binary(Default),
       byte_size(Default) > 0, byte_size(Default) < 256 ->
    validate_binary_list(client_preferred_next_protocols, PreferredProtocols),
    validate_npn_ordering(Precedence),
    Value;
validate_option(client_preferred_next_protocols, undefined) ->
    undefined;
validate_option(log_alert, true) ->
    notice;
validate_option(log_alert, false) ->
    warning;
validate_option(log_level, Value) when
      is_atom(Value) andalso
      (Value =:= emergency orelse
       Value =:= alert orelse
       Value =:= critical orelse
       Value =:= error orelse
       Value =:= warning orelse
       Value =:= notice orelse
       Value =:= info orelse
       Value =:= debug) ->
    Value;
validate_option(middlebox_comp_mode, Value) when is_boolean(Value) ->
    Value;
validate_option(next_protocols_advertised, Value) when is_list(Value) ->
    validate_binary_list(next_protocols_advertised, Value),
    Value;
validate_option(next_protocols_advertised, undefined) ->
    undefined;
validate_option(server_name_indication, Value) when is_list(Value) ->
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
    Value;
validate_option(server_name_indication, undefined) ->
    undefined;
validate_option(server_name_indication, disable) ->
    disable;

%% RFC 6066, Section 4
validate_option(max_fragment_length, I) when I == ?MAX_FRAGMENT_LENGTH_BYTES_1; I == ?MAX_FRAGMENT_LENGTH_BYTES_2;
                                             I == ?MAX_FRAGMENT_LENGTH_BYTES_3; I == ?MAX_FRAGMENT_LENGTH_BYTES_4 ->
    I;
validate_option(max_fragment_length, undefined) ->
    undefined;

validate_option(sni_hosts, []) ->
    [];
validate_option(sni_hosts, [{Hostname, SSLOptions} | Tail]) when is_list(Hostname) ->
	RecursiveSNIOptions = proplists:get_value(sni_hosts, SSLOptions, undefined),
	case RecursiveSNIOptions of
		undefined ->
			[{Hostname, validate_options(SSLOptions)} | validate_option(sni_hosts, Tail)];
		_ ->
			throw({error, {options, {sni_hosts, RecursiveSNIOptions}}})
	end;
validate_option(sni_fun, undefined) ->
    undefined;
validate_option(sni_fun, Fun) when is_function(Fun) ->
    Fun;
validate_option(honor_cipher_order, Value) when is_boolean(Value) ->
    Value;
validate_option(honor_ecc_order, Value) when is_boolean(Value) ->
    Value;
validate_option(padding_check, Value) when is_boolean(Value) ->
    Value;
validate_option(fallback, Value) when is_boolean(Value) ->
    Value;
validate_option(crl_check, Value) when is_boolean(Value)  ->
    Value;
validate_option(crl_check, Value) when (Value == best_effort) or (Value == peer) -> 
    Value;
validate_option(crl_cache, {Cb, {_Handle, Options}} = Value) when is_atom(Cb) and is_list(Options) ->
    Value;
validate_option(beast_mitigation, Value) when Value == one_n_minus_one orelse
                                              Value == zero_n orelse
                                              Value == disabled ->
  Value;
validate_option(max_handshake_size, Value) when is_integer(Value)  andalso Value =< ?MAX_UNIT24 ->
    Value;
validate_option(protocol, Value = tls) ->
    Value;
validate_option(protocol, Value = dtls) ->
    Value;
validate_option(handshake, hello = Value) ->
    Value;
validate_option(handshake, full = Value) ->
    Value;
validate_option(customize_hostname_check, Value) when is_list(Value) ->
    Value;
validate_option(cb_info, {V1, V2, V3, V4} = Value) when is_atom(V1),
                                                        is_atom(V2),
                                                        is_atom(V3),
                                                        is_atom(V4)
                                                ->
    Value;
validate_option(cb_info, {V1, V2, V3, V4, V5} = Value) when is_atom(V1),
                                                            is_atom(V2),
                                                            is_atom(V3),
                                                            is_atom(V4),
                                                            is_atom(V5)
                                                ->
    Value;
validate_option(use_ticket, Value) when is_list(Value) ->
    Value;
validate_option(session_tickets, Value) when Value =:= disabled orelse
                                             Value =:= manual orelse
                                             Value =:= auto orelse
                                             Value =:= stateless orelse
                                             Value =:= stateful ->
    Value;
validate_option(anti_replay, '10k') ->
    %% n = 10000
    %% p = 0.030003564 (1 in 33)
    %% m = 72985 (8.91KiB)
    %% k = 5
    {10, 5, 72985};
validate_option(anti_replay, '100k') ->
    %% n = 100000
    %% p = 0.03000428 (1 in 33)
    %% m = 729845 (89.09KiB)
    %% k = 5
    {10, 5, 729845};
validate_option(anti_replay, Value) when (is_tuple(Value) andalso
                                          tuple_size(Value) =:= 3) ->
    Value;
validate_option(ocsp_stapling, Value) when Value =:= true orelse
                                           Value =:= false ->
    Value;
%% The OCSP responders' certificates can be given as a suggestion and
%% will be used to verify the OCSP response.
validate_option(ocsp_responder_certs, Value) when is_list(Value) ->
    [public_key:pkix_decode_cert(CertDer, plain) || CertDer <- Value,
                                                    is_binary(CertDer)];
validate_option(ocsp_nonce, Value) when Value =:= true orelse
                                        Value =:= false ->
    Value;
validate_option(Opt, undefined = Value) ->
    AllOpts = maps:keys(?RULES),
    case lists:member(Opt, AllOpts) of
        true ->
            Value;
        false ->
            throw({error, {options, {Opt, Value}}})
    end;
validate_option(Opt, Value) ->
    throw({error, {options, {Opt, Value}}}).

handle_cb_info({V1, V2, V3, V4}) ->
    {V1,V2,V3,V4, list_to_atom(atom_to_list(V2) ++ "_passive")};
handle_cb_info(CbInfo) ->
    CbInfo.

handle_hashsigns_option(Value, Version) when is_list(Value)
                                             andalso Version >= {3, 4} ->
    case tls_v1:signature_schemes(Version, Value) of
	[] ->
	    throw({error, {options,
                           no_supported_signature_schemes,
                           {signature_algs, Value}}});
	_ ->
	    Value
    end;
handle_hashsigns_option(Value, Version) when is_list(Value) 
                                             andalso Version =:= {3, 3} ->
    case tls_v1:signature_algs(Version, Value) of
	[] ->
	    throw({error, {options, no_supported_algorithms, {signature_algs, Value}}});
	_ ->	
	    Value
    end;
handle_hashsigns_option(_, Version) when Version =:= {3, 3} ->
    handle_hashsigns_option(tls_v1:default_signature_algs(Version), Version);
handle_hashsigns_option(_, _Version) ->
    undefined.

handle_signature_algorithms_option(Value, Version) when is_list(Value)
                                                        andalso Version >= {3, 4} ->
    case tls_v1:signature_schemes(Version, Value) of
	[] ->
	    throw({error, {options,
                           no_supported_signature_schemes,
                           {signature_algs_cert, Value}}});
	_ ->
	    Value
    end;
handle_signature_algorithms_option(_, _Version) ->
    undefined.

validate_options([]) ->
	[];
validate_options([{Opt, Value} | Tail]) ->
	[{Opt, validate_option(Opt, Value)} | validate_options(Tail)].

validate_npn_ordering(client) ->
    ok;
validate_npn_ordering(server) ->
    ok;
validate_npn_ordering(Value) ->
    throw({error, {options, {client_preferred_next_protocols, {invalid_precedence, Value}}}}).

validate_binary_list(Opt, List) ->
    lists:foreach(
        fun(Bin) when is_binary(Bin),
                      byte_size(Bin) > 0,
                      byte_size(Bin) < 256 ->
            ok;
           (Bin) ->
            throw({error, {options, {Opt, {invalid_protocol, Bin}}}})
        end, List).
validate_versions([], Versions) ->
    Versions;
validate_versions([Version | Rest], Versions) when Version == 'tlsv1.3';
                                                   Version == 'tlsv1.2';
                                                   Version == 'tlsv1.1';
                                                   Version == tlsv1 ->
    case tls_record:sufficient_crypto_support(Version) of
        true ->
            tls_validate_versions(Rest, Versions);
        false ->
            throw({error, {options, {insufficient_crypto_support, {Version, {versions, Versions}}}}})
    end; 
validate_versions([Version | Rest], Versions) when Version == 'dtlsv1';
                                                   Version == 'dtlsv1.2'->
    DTLSVer = dtls_record:protocol_version(Version),
    case tls_record:sufficient_crypto_support(dtls_v1:corresponding_tls_version(DTLSVer)) of
        true ->
            dtls_validate_versions(Rest, Versions);
        false ->
            throw({error, {options, {insufficient_crypto_support, {Version, {versions, Versions}}}}})
    end;        
validate_versions([Version| _], Versions) ->
    throw({error, {options, {Version, {versions, Versions}}}}).

tls_validate_versions([], Versions) ->
    tls_validate_version_gap(Versions);
tls_validate_versions([Version | Rest], Versions) when Version == 'tlsv1.3';
                                                       Version == 'tlsv1.2';
                                                       Version == 'tlsv1.1';
                                                       Version == tlsv1 ->
    tls_validate_versions(Rest, Versions);                  
tls_validate_versions([Version| _], Versions) ->
    throw({error, {options, {Version, {versions, Versions}}}}).

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
dtls_validate_versions([], Versions) ->
    Versions;
dtls_validate_versions([Version | Rest], Versions) when  Version == 'dtlsv1';
                                                         Version == 'dtlsv1.2'->
    dtls_validate_versions(Rest, Versions);
dtls_validate_versions([Ver| _], Versions) ->
    throw({error, {options, {Ver, {versions, Versions}}}}).

%% The option cacerts overrides cacertsfile
ca_cert_default(_,_, [_|_]) ->
    undefined;
ca_cert_default(verify_none, _, _) ->
    undefined;
ca_cert_default(verify_peer, {Fun,_}, _) when is_function(Fun) ->
    undefined;
%% Server that wants to verify_peer and has no verify_fun must have
%% some trusted certs.
ca_cert_default(verify_peer, undefined, _) ->
    "".
emulated_options(Protocol, Opts) ->
    case Protocol of
	tls ->
	    tls_socket:emulated_options(Opts);
	dtls ->
	    dtls_socket:emulated_options(Opts)
    end.

handle_cipher_option(Value, Versions)  when is_list(Value) ->       
    try binary_cipher_suites(Versions, Value) of
	Suites ->
	    Suites
    catch
	exit:_ ->
	    throw({error, {options, {ciphers, Value}}});
	error:_->
	    throw({error, {options, {ciphers, Value}}})
    end.

binary_cipher_suites([{3,4} = Version], []) -> 
    %% Defaults to all supported suites that does
    %% not require explicit configuration TLS-1.3
    %% only mode.
    default_binary_suites(exclusive, Version);
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
binary_cipher_suites([Version |_] = Versions, [Cipher0 | _] = Ciphers0) when is_binary(Cipher0) ->
    All = ssl_cipher:all_suites(Version) ++ 
        ssl_cipher:anonymous_suites(Version),
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

default_binary_suites(exclusive, {_, Minor}) ->
    ssl_cipher:filter_suites(tls_v1:exclusive_suites(Minor));
default_binary_suites(default, Version) ->
    ssl_cipher:filter_suites(ssl_cipher:suites(Version)).

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

handle_eccs_option(Value, Version) when is_list(Value) ->
    {_Major, Minor} = tls_version(Version),
    try tls_v1:ecc_curves(Minor, Value) of
        Curves -> #elliptic_curves{elliptic_curve_list = Curves}
    catch
        exit:_ -> throw({error, {options, {eccs, Value}}});
        error:_ -> throw({error, {options, {eccs, Value}}})
    end.

handle_supported_groups_option(Value, Version) when is_list(Value) ->
    {_Major, Minor} = tls_version(Version),
    try tls_v1:groups(Minor, Value) of
        Groups -> #supported_groups{supported_groups = Groups}
    catch
        exit:_ -> throw({error, {options, {supported_groups, Value}}});
        error:_ -> throw({error, {options, {supported_groups, Value}}})
    end.


unexpected_format(Error) ->
    lists:flatten(io_lib:format("Unexpected error: ~p", [Error])).

file_error_format({error, Error})->
    case file:format_error(Error) of
	"unknown POSIX error" ->
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

detect(_Pred, []) ->
    undefined;
detect(Pred, [H|T]) ->
    case Pred(H) of
        true ->
            H;
        _ ->
            detect(Pred, T)
    end.

make_next_protocol_selector(undefined) ->
    undefined;
make_next_protocol_selector({client, AllProtocols, DefaultProtocol}) ->
    fun(AdvertisedProtocols) ->
        case detect(fun(PreferredProtocol) ->
			    lists:member(PreferredProtocol, AdvertisedProtocols)
		    end, AllProtocols) of
            undefined ->
		DefaultProtocol;
            PreferredProtocol ->
		PreferredProtocol
        end
    end;

make_next_protocol_selector({server, AllProtocols, DefaultProtocol}) ->
    fun(AdvertisedProtocols) ->
	    case detect(fun(PreferredProtocol) ->
				lists:member(PreferredProtocol, AllProtocols)
			end,
			AdvertisedProtocols) of
		undefined ->
		    DefaultProtocol;
            PreferredProtocol ->
		    PreferredProtocol
	    end
    end.

connection_cb(tls) ->
    tls_connection;
connection_cb(dtls) ->
    dtls_connection;
connection_cb(Opts) ->
   connection_cb(proplists:get_value(protocol, Opts, tls)).

record_cb(tls) ->
    tls_record;
record_cb(dtls) ->
    dtls_record;
record_cb(Opts) ->
    record_cb(proplists:get_value(protocol, Opts, tls)).

binary_filename(FileName) ->
    Enc = file:native_name_encoding(),
    unicode:characters_to_binary(FileName, unicode, Enc).

assert_proplist([]) ->
    true;
assert_proplist([{Key,_} | Rest]) when is_atom(Key) ->
    assert_proplist(Rest);
%% Handle exceptions 
assert_proplist([{raw,_,_,_} | Rest]) ->
    assert_proplist(Rest);
assert_proplist([inet | Rest]) ->
    assert_proplist(Rest);
assert_proplist([inet6 | Rest]) ->
    assert_proplist(Rest);
assert_proplist([Value | _]) ->
    throw({option_not_a_key_value_tuple, Value}).


handle_verify_option(verify_none, #{fail_if_no_peer_cert := false} = OptionsMap) ->
    OptionsMap#{verify => verify_none};
handle_verify_option(verify_none, #{fail_if_no_peer_cert := true}) ->
    throw({error, {options, incompatible,
                   {verify, verify_none},
                   {fail_if_no_peer_cert, true}}});
handle_verify_option(verify_peer, OptionsMap) ->
    OptionsMap#{verify => verify_peer};
handle_verify_option(Value, _) ->
    throw({error, {options, {verify, Value}}}).

%% Added to handle default values for signature_algs in TLS 1.3
default_option_role_sign_algs(_, Value, _, Version) when Version >= {3,4} ->
    Value;
default_option_role_sign_algs(Role, Value, Role, _) ->
    Value;
default_option_role_sign_algs(_, _, _, _) ->
    undefined.

default_option_role(Role, Value, Role) ->
    Value;
default_option_role(_,_,_) ->
    undefined.


default_cb_info(tls) ->
    {gen_tcp, tcp, tcp_closed, tcp_error, tcp_passive};
default_cb_info(dtls) ->
    {gen_udp, udp, udp_closed, udp_error, udp_passive}.

include_security_info([]) ->
    false;
include_security_info([Item | Items]) ->
    case lists:member(Item, [client_random, server_random, master_secret]) of
        true ->
            true;
        false  ->
            include_security_info(Items)
    end.

server_name_indication_default(Host) when is_list(Host) ->
    %% SNI should not contain a trailing dot that a hostname may
    string:strip(Host, right, $.);
server_name_indication_default(_) ->
    undefined.

add_filter(undefined, Filters) ->
    Filters;
add_filter(Filter, Filters) ->
    [Filter | Filters].
