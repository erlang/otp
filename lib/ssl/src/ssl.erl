%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2018. All Rights Reserved.
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
-include("ssl_internal.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_handshake.hrl").
-include("ssl_srp.hrl").

%% Application handling
-export([start/0, start/1, stop/0, clear_pem_cache/0]).

%% Socket handling
-export([connect/3, connect/2, connect/4,
	 listen/2, transport_accept/1, transport_accept/2,
	 handshake/1, handshake/2, handshake/3, handshake_continue/2,
         handshake_continue/3, handshake_cancel/1,
         ssl_accept/1, ssl_accept/2, ssl_accept/3,
	 controlling_process/2, peername/1, peercert/1, sockname/1,
	 close/1, close/2, shutdown/2, recv/2, recv/3, send/2,
	 getopts/2, setopts/2, getstat/1, getstat/2
	]).

%% SSL/TLS protocol handling
-export([cipher_suites/0, cipher_suites/1, cipher_suites/2, filter_cipher_suites/2,
         prepend_cipher_suites/2, append_cipher_suites/2,
         eccs/0, eccs/1, versions/0, groups/0, groups/1,
         format_error/1, renegotiate/1, prf/5, negotiated_protocol/1, 
	 connection_information/1, connection_information/2]).
%% Misc
-export([handle_options/2, tls_version/1, new_ssl_options/3, suite_to_str/1,
         set_log_level/1]).

-deprecated({ssl_accept, 1, eventually}).
-deprecated({ssl_accept, 2, eventually}).
-deprecated({ssl_accept, 3, eventually}).

-export_type([socket/0,
              sslsocket/0,
              socket_option/0,
              tls_client_option/0,
              tls_option/0,
              tls_server_option/0,
              active_msgs/0,
              erl_cipher_suite/0,
              protocol_version/0,
              dtls_version/0,
              tls_version/0,
              prf_random/0, 
              hello_extensions/0,
              error_alert/0,
              session_id/0, 
              path/0, 
              hostname/0,
              host/0,
              prf/0, 
              srp_param_type/0,
              cipher_filters/0,
              ssl_imp/0,
              private_key_type/0,
              cipher/0,
              hash/0,
              key_algo/0,
              sign_algo/0
             ]).
%% -------------------------------------------------------------------------------------------------------
-type socket()                   :: gen_tcp:socket().
-type socket_option()    ::  socket_connect_option() | socket_listen_option(). 
-type socket_connect_option()    :: gen_tcp:connect_option() | gen_udp:option().
-type socket_listen_option()     :: gen_tcp:listen_option() | gen_udp:option().
-opaque sslsocket()              :: #sslsocket{}.
-type tls_option()                :: tls_client_option() | tls_server_option().
-type tls_client_option()            :: client_option() | socket_connect_option() |  transport_option().
-type tls_server_option()            :: server_option() | socket_listen_option() | transport_option().
-type active_msgs()  :: {ssl, sslsocket(), Data::binary() | list()} | {ssl_closed, sslsocket()} |
                        {ssl_error, sslsocket(), Reason::term()}.
-type transport_option() :: {cb_info, {CallbackModule::atom(), DataTag::atom(),
				       ClosedTag::atom(), ErrTag::atom()}}.
-type path()         :: file:filename().
-type host()         :: hostname() | ip_address().
-type hostname()     :: string().
-type ip_address()   :: inet:ip_address().
-type session_id()   :: binary().
-type protocol_version() :: tls_version() | dtls_version().
-type tls_version()  :: tlsv1 | 'tlsv1.1' | 'tlsv1.2' | 'tlsv1.3' | legacy_version().
-type dtls_version() :: 'dtlsv1' | 'dtlsv1.2'.
-type legacy_version() :: sslv3.
-type verify_type()  :: verify_none | verify_peer.
-type cipher()            :: aes_128_cbc |
                             aes_256_cbc |
                             aes_128_gcm |
                             aes_256_gcm |
                             chacha20_poly1305 |
                             legacy_cipher().
-type legacy_cipher()     ::  rc4_128 |
                              des_cbc |
                             '3des_ede_cbc'.

-type hash()              :: sha |
                             sha2() |
                             legacy_hash().

-type sha2()              ::  sha224 |
                              sha256 |
                              sha384 |
                              sha512.

-type legacy_hash()        :: md5.

-type sign_algo()         :: rsa | dsa | ecdsa.
-type key_algo()          :: rsa |
                             dhe_rsa | dhe_dss |
                             ecdhe_ecdsa | ecdh_ecdsa | ecdh_rsa |
                             srp_rsa| srp_dss |
                             psk | dhe_psk | rsa_psk |
                             dh_anon | ecdh_anon | srp_anon |
                             any. %% TLS 1.3
-type prf()               :: hash() | default_prf.
-type erl_cipher_suite()  :: #{key_exchange := key_algo(),
                               cipher := cipher(),
                               mac    := hash() | aead,
                               prf    := hash() | default_prf %% Old cipher suites, version dependent
                              }.  

-type named_curve()       :: sect571r1 |
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
                             secp160r2.

-type srp_param_type()    :: srp_1024 |
                             srp_1536 |
                             srp_2048 |
                             srp_3072 |
                             srp_4096 |
                             srp_6144 |
                             srp_8192.

-type error_alert() :: {tls_alert, {tls_alert(), Description::string()}}.

-type tls_alert() :: 
        close_notify | 
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
        no_application_protocol.
%% -------------------------------------------------------------------------------------------------------
-type common_option()        :: {protocol, protocol()} |
                                {handshake, handshake_completion()} |
                                {cert, cert()} |
                                {certfile, cert_pem()} |
                                {key, key()} |
                                {keyfile, key_pem()} |
                                {password, key_password()} |
                                {ciphers, cipher_suites()} |
                                {eccs, eccs()} |
                                {secure_renegotiate, secure_renegotiation()} |
                                {depth, allowed_cert_chain_length()} |
                                {verify_fun, custom_verify()} |
                                {crl_check, crl_check()} |
                                {crl_cache, crl_cache_opts()} |
                                {max_handshake_size, handshake_size()} |
                                {partial_chain, root_fun()} |
                                {versions, protocol_versions()} |
                                {user_lookup_fun, custom_user_lookup()} |
                                {log_alert, log_alert()} |
                                {hibernate_after, hibernate_after()} |
                                {padding_check, padding_check()} |
                                {beast_mitigation, beast_mitigation()}. 

-type protocol()                 :: tls | dtls.
-type handshake_completion()     ::  hello | full.
-type cert()                     :: public_key:der_encoded().
-type cert_pem()                 :: ssl:path().
-type key()                      :: {'RSAPrivateKey'| 'DSAPrivateKey' | 'ECPrivateKey' |'PrivateKeyInfo', 
                                           public_key:der_encoded()} | 
                                     #{algorithm := rsa | dss | ecdsa, 
                                       engine := crypto:engine_ref(), 
                                       key_id := crypto:key_id(), 
                                       password => crypto:password()}.
-type key_pem()                  :: ssl:path().
-type key_password()                 :: string().
-type cipher_suites() :: ciphers().    
-type ciphers()      :: [erl_cipher_suite()] |
			string(). % (according to old API)
-type cipher_filters()    :: list({key_exchange | cipher | mac | prf,
                                   algo_filter()}).
-type algo_filter()       :: fun((key_algo()|cipher()|hash()|aead|default_prf) -> true | false).
-type eccs()                     :: [named_curve()].  
-type secure_renegotiation()     :: boolean(). 
-type allowed_cert_chain_length() :: integer().
-type custom_verify()               ::  {Verifyfun :: fun(), InitialUserState :: term()}.
-type crl_check()                :: boolean() | peer | best_effort.
-type crl_cache_opts()           :: [term()].
-type handshake_size()           :: integer().
-type hibernate_after()          :: timeout().
-type root_fun()                 ::  fun().
-type protocol_versions()        ::  [protocol_version()].
-type signature_algs()           ::  [{hash(), sign_algo()}].
-type custom_user_lookup()       ::  {Lookupfun :: fun(), UserState :: term()}.
-type padding_check()            :: boolean(). 
-type beast_mitigation()         :: one_n_minus_one | zero_n | disabled.
-type srp_identity()             :: {Username :: string(), Password :: string()}.
-type psk_identity()             :: string().
-type log_alert()                :: boolean().

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
                                {customize_hostname_check, customize_hostname_check()} |
                                {signature_algs, client_signature_algs()} |                                    
                                {fallback, fallback()}.

-type client_verify_type()       :: verify_type().
-type client_reuse_session()     :: ssl:session_id().
-type client_reuse_sessions()    :: boolean() | save.
-type client_cacerts()           :: [public_key:der_encoded()].
-type client_cafile()            :: ssl:path().
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
-type sni()                      :: HostName :: ssl:hostname() | disable. 
-type client_signature_algs()    :: signature_algs().
-type fallback()                 :: boolean().

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
                                {signature_algs, server_signature_algs()}.

-type server_cacerts()           :: [public_key:der_encoded()].
-type server_cafile()            :: ssl:path().
-type server_alpn()              :: [app_level_protocol()].
-type server_next_protocol()     :: [app_level_protocol()].
-type server_psk_identity()      :: psk_identity().
-type dh_der()                   :: binary().
-type dh_file()                  :: ssl:path().
-type server_verify_type()       :: verify_type().
-type fail_if_no_peer_cert()     :: boolean().
-type server_signature_algs()    :: signature_algs().
-type server_reuse_session()     :: fun().
-type server_reuse_sessions()    :: boolean().
-type sni_hosts()                :: [{ssl:hostname(), [server_option() | common_option()]}].
-type sni_fun()                  :: fun().
-type honor_cipher_order()       :: boolean().
-type honor_ecc_order()          :: boolean().
-type client_renegotiation()     :: boolean().
%% -------------------------------------------------------------------------------------------------------

-type ssl_imp()      :: new | old.


-type prf_random() :: client_random | server_random.

-type private_key_type() :: rsa | %% Backwards compatibility
                            dsa | %% Backwards compatibility
                            'RSAPrivateKey' |
                            'DSAPrivateKey' |
                            'ECPrivateKey' |
                            'PrivateKeyInfo'.

-type hello_extensions()  :: #{signature_algs => sign_algo()}. %% TODO
%% -------------------------------------------------------------------------------------------------------
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
-spec connect(host() | port(), [tls_client_option()]) -> {ok, #sslsocket{}} |
                                                      {error, reason()}.
connect(Socket, SslOptions) when is_port(Socket) ->
    connect(Socket, SslOptions, infinity).

-spec connect(host() | port(), [tls_client_option()] | inet:port_number(),
	      timeout() | list()) ->
		     {ok, #sslsocket{}} | {error, reason()}.
connect(Socket, SslOptions0, Timeout) when is_port(Socket),
					    (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    {Transport,_,_,_} = proplists:get_value(cb_info, SslOptions0,
					      {gen_tcp, tcp, tcp_closed, tcp_error}),
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

-spec connect(host() | port(), inet:port_number(), list(), timeout()) ->
		     {ok, #sslsocket{}} | {error, reason()}.

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
-spec listen(inet:port_number(), [tls_server_option()]) ->{ok, #sslsocket{}} | {error, reason()}.

%%
%% Description: Creates an ssl listen socket.
%%--------------------------------------------------------------------
listen(_Port, []) ->
    {error, nooptions};
listen(Port, Options0) ->
    try
	{ok, Config} = handle_options(Options0, server),
	do_listen(Port, Config, connection_cb(Options0))
    catch
	Error = {error, _} ->
	    Error
    end.
%%--------------------------------------------------------------------
%%
%% Description: Performs transport accept on an ssl listen socket
%%--------------------------------------------------------------------
-spec transport_accept(#sslsocket{}) -> {ok, #sslsocket{}} |
					{error, reason()}.
transport_accept(ListenSocket) ->
    transport_accept(ListenSocket, infinity).

-spec transport_accept(#sslsocket{}, timeout()) -> {ok, #sslsocket{}} |
						   {error, reason()}.
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
-spec ssl_accept(#sslsocket{}) -> ok | {error, timeout | closed | {options, any()}| error_alert()}.
ssl_accept(ListenSocket) ->
    ssl_accept(ListenSocket, [], infinity).

-spec ssl_accept(#sslsocket{} | port(), timeout()| [tls_server_option()]) ->
			ok | {ok, #sslsocket{}} | {error, timeout | closed | {options, any()}| error_alert()}.
ssl_accept(Socket, Timeout)  when  (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    ssl_accept(Socket, [], Timeout);
ssl_accept(ListenSocket, SslOptions) when is_port(ListenSocket) ->
    ssl_accept(ListenSocket, SslOptions, infinity);
ssl_accept(Socket, Timeout) ->
    ssl_accept(Socket, [], Timeout).

-spec ssl_accept(#sslsocket{} | port(), [tls_server_option()], timeout()) ->
			ok | {ok, #sslsocket{}} | {error, timeout | closed | {options, any()}| error_alert()}.
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
-spec handshake(#sslsocket{}) -> {ok, #sslsocket{}} | {error, timeout | closed | {options, any()} | error_alert()}.

handshake(ListenSocket) ->
    handshake(ListenSocket, infinity).

-spec handshake(#sslsocket{} | port(), timeout()| [tls_server_option()]) ->
                       {ok, #sslsocket{}} | {error, timeout | closed | {options, any()} | error_alert()}.
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

-spec handshake(#sslsocket{} | port(), [tls_server_option()], timeout()) ->
			{ok, #sslsocket{}} | {error, timeout | closed | {options, any()} | error_alert()}.
handshake(#sslsocket{} = Socket, [], Timeout) when (is_integer(Timeout) andalso Timeout >= 0) or 
                                                    (Timeout == infinity)->
    handshake(Socket, Timeout);
handshake(#sslsocket{fd = {_, _, _, Tracker}} = Socket, SslOpts, Timeout) when
      (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity)->
    try
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
    {Transport,_,_,_} =
	proplists:get_value(cb_info, SslOptions, {gen_tcp, tcp, tcp_closed, tcp_error}),
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
-spec handshake_continue(#sslsocket{}, [tls_client_option() | tls_server_option()]) -> 
                                {ok, #sslsocket{}} | {error, reason()}.
%%
%%
%% Description: Continues the handshke possible with newly supplied options.
%%--------------------------------------------------------------------
handshake_continue(Socket, SSLOptions) ->
    handshake_continue(Socket, SSLOptions, infinity).
%%--------------------------------------------------------------------
-spec handshake_continue(#sslsocket{}, [tls_client_option() | tls_server_option()], timeout()) -> 
                                {ok, #sslsocket{}} | {error, reason()}.
%%
%%
%% Description: Continues the handshke possible with newly supplied options.
%%--------------------------------------------------------------------
handshake_continue(Socket, SSLOptions, Timeout) ->
    ssl_connection:handshake_continue(Socket, SSLOptions, Timeout).
%%--------------------------------------------------------------------
-spec  handshake_cancel(#sslsocket{}) -> term().
%%
%% Description: Cancels the handshakes sending a close alert.
%%--------------------------------------------------------------------
handshake_cancel(Socket) ->
    ssl_connection:handshake_cancel(Socket).

%%--------------------------------------------------------------------
-spec  close(#sslsocket{}) -> term().
%%
%% Description: Close an ssl connection
%%--------------------------------------------------------------------
close(#sslsocket{pid = [Pid|_]}) when is_pid(Pid) ->
    ssl_connection:close(Pid, {close, ?DEFAULT_TIMEOUT});
close(#sslsocket{pid = {dtls, #config{dtls_handler = {Pid, _}}}}) ->
   dtls_packet_demux:close(Pid);
close(#sslsocket{pid = {ListenSocket, #config{transport_info={Transport,_, _, _}}}}) ->
    Transport:close(ListenSocket).

%%--------------------------------------------------------------------
-spec  close(#sslsocket{}, timeout() | {pid(), integer()}) -> term().
%%
%% Description: Close an ssl connection
%%--------------------------------------------------------------------
close(#sslsocket{pid = [TLSPid|_]},
      {Pid, Timeout} = DownGrade) when is_pid(TLSPid),
				       is_pid(Pid),
				       (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    ssl_connection:close(TLSPid, {close, DownGrade});
close(#sslsocket{pid = [TLSPid|_]}, Timeout) when is_pid(TLSPid),
					      (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity) ->
    ssl_connection:close(TLSPid, {close, Timeout});
close(#sslsocket{pid = {ListenSocket, #config{transport_info={Transport,_, _, _}}}}, _) ->
    Transport:close(ListenSocket).

%%--------------------------------------------------------------------
-spec send(#sslsocket{}, iodata()) -> ok | {error, reason()}.
%%
%% Description: Sends data over the ssl connection
%%--------------------------------------------------------------------
send(#sslsocket{pid = [Pid]}, Data) when is_pid(Pid) ->
    ssl_connection:send(Pid, Data);
send(#sslsocket{pid = [_, Pid]}, Data) when is_pid(Pid) ->
    tls_sender:send_data(Pid,  erlang:iolist_to_binary(Data));
send(#sslsocket{pid = {_, #config{transport_info={_, udp, _, _}}}}, _) ->
    {error,enotconn}; %% Emulate connection behaviour
send(#sslsocket{pid = {dtls,_}}, _) ->
    {error,enotconn};  %% Emulate connection behaviour
send(#sslsocket{pid = {ListenSocket, #config{transport_info={Transport, _, _, _}}}}, Data) ->
    Transport:send(ListenSocket, Data). %% {error,enotconn}

%%--------------------------------------------------------------------
%%
%% Description: Receives data when active = false
%%--------------------------------------------------------------------
-spec recv(#sslsocket{}, integer()) -> {ok, binary()| list()} | {error, reason()}.
recv(Socket, Length) ->
    recv(Socket, Length, infinity).

-spec recv(#sslsocket{}, integer(), timeout()) -> {ok, binary()| list()} | {error, reason()}.
recv(#sslsocket{pid = [Pid|_]}, Length, Timeout) when is_pid(Pid),
						  (is_integer(Timeout) andalso Timeout >= 0) or (Timeout == infinity)->
    ssl_connection:recv(Pid, Length, Timeout);
recv(#sslsocket{pid = {dtls,_}}, _, _) ->
    {error,enotconn};
recv(#sslsocket{pid = {Listen,
		       #config{transport_info = {Transport, _, _, _}}}}, _,_) when is_port(Listen)->
    Transport:recv(Listen, 0). %% {error,enotconn}

%%--------------------------------------------------------------------
-spec controlling_process(#sslsocket{}, pid()) -> ok | {error, reason()}.
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
				      #config{transport_info = {Transport, _, _, _}}}},
		    NewOwner) when is_port(Listen),
				   is_pid(NewOwner) ->
     %% Meaningless but let it be allowed to conform with normal sockets  
    Transport:controlling_process(Listen, NewOwner).


%%--------------------------------------------------------------------
-spec connection_information(#sslsocket{}) -> {ok, list()} | {error, reason()}.
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
-spec connection_information(#sslsocket{}, [atom()]) -> {ok, list()} | {error, reason()}.
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
-spec peername(#sslsocket{}) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, reason()}.
%%
%% Description: same as inet:peername/1.
%%--------------------------------------------------------------------
peername(#sslsocket{pid = [Pid|_], fd = {Transport, Socket, _}}) when is_pid(Pid)->
    dtls_socket:peername(Transport, Socket);
peername(#sslsocket{pid = [Pid|_], fd = {Transport, Socket, _, _}}) when is_pid(Pid)->
    tls_socket:peername(Transport, Socket);
peername(#sslsocket{pid = {dtls, #config{dtls_handler = {_Pid, _}}}}) ->
    dtls_socket:peername(dtls, undefined);
peername(#sslsocket{pid = {ListenSocket,  #config{transport_info = {Transport,_,_,_}}}}) ->
    tls_socket:peername(Transport, ListenSocket); %% Will return {error, enotconn}
peername(#sslsocket{pid = {dtls,_}}) ->
    {error,enotconn}.

%%--------------------------------------------------------------------
-spec peercert(#sslsocket{}) ->{ok, DerCert::binary()} | {error, reason()}.
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
-spec negotiated_protocol(#sslsocket{}) -> {ok, binary()} | {error, reason()}.
%%
%% Description: Returns the protocol that has been negotiated. If no
%% protocol has been negotiated will return {error, protocol_not_negotiated}
%%--------------------------------------------------------------------
negotiated_protocol(#sslsocket{pid = [Pid|_]}) when is_pid(Pid) ->
    ssl_connection:negotiated_protocol(Pid).

%%--------------------------------------------------------------------
-spec cipher_suites() -> [ssl_cipher_format:old_erl_cipher_suite()] | [string()].
%%--------------------------------------------------------------------
cipher_suites() ->
    cipher_suites(erlang).
%%--------------------------------------------------------------------
-spec cipher_suites(erlang | openssl | all) -> 
                           [ssl_cipher_format:old_erl_cipher_suite() | string()].
%% Description: Returns all supported cipher suites.
%%--------------------------------------------------------------------
cipher_suites(erlang) ->
    [ssl_cipher_format:erl_suite_definition(Suite) || Suite <- available_suites(default)];

cipher_suites(openssl) ->
    [ssl_cipher_format:openssl_suite_name(Suite) ||
        Suite <- available_suites(default)];

cipher_suites(all) ->
    [ssl_cipher_format:erl_suite_definition(Suite) || Suite <- available_suites(all)].

%%--------------------------------------------------------------------
-spec cipher_suites(default | all | anonymous, ssl_record:ssl_version() |
                    tls_record:tls_atom_version() |  dtls_record:dtls_atom_version()) -> 
                           [erl_cipher_suite()].
%% Description: Returns all default and all supported cipher suites for a
%% TLS/DTLS version
%%--------------------------------------------------------------------
cipher_suites(Base, Version) when Version == 'tlsv1.2';
                                  Version == 'tlsv1.1';
                                  Version == tlsv1;
                                  Version == sslv3 ->
    cipher_suites(Base, tls_record:protocol_version(Version));
cipher_suites(Base, Version)  when Version == 'dtlsv1.2';
                                   Version == 'dtlsv1'->
    cipher_suites(Base, dtls_record:protocol_version(Version));                   
cipher_suites(Base, Version) ->
    [ssl_cipher_format:suite_definition(Suite) || Suite <- supported_suites(Base, Version)].

%%--------------------------------------------------------------------
-spec filter_cipher_suites([erl_cipher_suite()] | [ssl_cipher_format:cipher_suite()] , 
                           [{key_exchange | cipher | mac | prf, fun()}] | []) -> 
                                  [erl_cipher_suite()] | [ssl_cipher_format:cipher_suite()].

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
-spec prepend_cipher_suites([erl_cipher_suite()] | 
                            [{key_exchange | cipher | mac | prf, fun()}],
                            [erl_cipher_suite()]) -> 
                                   [erl_cipher_suite()].
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
-spec append_cipher_suites(Deferred :: [erl_cipher_suite()] | 
                                       [{key_exchange | cipher | mac | prf, fun()}],
                           [erl_cipher_suite()]) -> 
                                  [erl_cipher_suite()].
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
-spec eccs() -> tls_v1:curves().
%% Description: returns all supported curves across all versions
%%--------------------------------------------------------------------
eccs() ->
    Curves = tls_v1:ecc_curves(all), % only tls_v1 has named curves right now
    eccs_filter_supported(Curves).

%%--------------------------------------------------------------------
-spec eccs(tls_record:tls_atom_version() |
           ssl_record:ssl_version() | dtls_record:dtls_atom_version()) ->
                  tls_v1:curves().
%% Description: returns the curves supported for a given version of
%% ssl/tls.
%%--------------------------------------------------------------------
eccs({3,0}) ->
    [];
eccs({3,_}) ->
    Curves = tls_v1:ecc_curves(all),
    eccs_filter_supported(Curves);
eccs({254,_} = Version) ->
    eccs(dtls_v1:corresponding_tls_version(Version));
eccs(Version) when Version == 'tlsv1.2';
                   Version == 'tlsv1.1';
                   Version == tlsv1;
                   Version == sslv3 ->
    eccs(tls_record:protocol_version(Version));
eccs(Version) when Version == 'dtlsv1.2';
                   Version == 'dtlsv1'->
    eccs(dtls_v1:corresponding_tls_version(dtls_record:protocol_version(Version))).

eccs_filter_supported(Curves) ->
    CryptoCurves = crypto:ec_curves(),
    lists:filter(fun(Curve) -> proplists:get_bool(Curve, CryptoCurves) end,
                 Curves).

%%--------------------------------------------------------------------
-spec groups() -> tls_v1:supported_groups().
%% Description: returns all supported groups (TLS 1.3 and later)
%%--------------------------------------------------------------------
groups() ->
    tls_v1:groups(4).

%%--------------------------------------------------------------------
-spec groups(default) -> tls_v1:supported_groups().
%% Description: returns the default groups (TLS 1.3 and later)
%%--------------------------------------------------------------------
groups(default) ->
    tls_v1:default_groups(4).

%%--------------------------------------------------------------------
-spec getopts(#sslsocket{}, [gen_tcp:option_name()]) ->
		     {ok, [gen_tcp:option()]} | {error, reason()}.
%%
%% Description: Gets options
%%--------------------------------------------------------------------
getopts(#sslsocket{pid = [Pid|_]}, OptionTags) when is_pid(Pid), is_list(OptionTags) ->
    ssl_connection:get_opts(Pid, OptionTags);
getopts(#sslsocket{pid = {dtls, #config{transport_info = {Transport,_,_,_}}}} = ListenSocket, OptionTags) when is_list(OptionTags) ->
    try dtls_socket:getopts(Transport, ListenSocket, OptionTags) of
        {ok, _} = Result ->
            Result;
	{error, InetError} ->
	    {error, {options, {socket_options, OptionTags, InetError}}}
    catch
	_:Error ->
	    {error, {options, {socket_options, OptionTags, Error}}}
    end;
getopts(#sslsocket{pid = {_,  #config{transport_info = {Transport,_,_,_}}}} = ListenSocket,
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
-spec setopts(#sslsocket{},  [gen_tcp:option()]) -> ok | {error, reason()}.
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
setopts(#sslsocket{pid = {dtls, #config{transport_info = {Transport,_,_,_}}}} = ListenSocket, Options) when is_list(Options) ->
    try dtls_socket:setopts(Transport, ListenSocket, Options) of
	ok ->
	    ok;
	{error, InetError} ->
	    {error, {options, {socket_options, Options, InetError}}}
    catch
	_:Error ->
	    {error, {options, {socket_options, Options, Error}}}
    end;
setopts(#sslsocket{pid = {_, #config{transport_info = {Transport,_,_,_}}}} = ListenSocket, Options) when is_list(Options) ->
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
-spec getstat(Socket) ->
	{ok, OptionValues} | {error, inet:posix()} when
      Socket :: #sslsocket{},
      OptionValues :: [{inet:stat_option(), integer()}].
%%
%% Description: Get all statistic options for a socket.
%%--------------------------------------------------------------------
getstat(Socket) ->
	getstat(Socket, inet:stats()).

%%---------------------------------------------------------------
-spec getstat(Socket, Options) ->
	{ok, OptionValues} | {error, inet:posix()} when
      Socket :: #sslsocket{},
      Options :: [inet:stat_option()],
      OptionValues :: [{inet:stat_option(), integer()}].
%%
%% Description: Get one or more statistic options for a socket.
%%--------------------------------------------------------------------
getstat(#sslsocket{pid = {Listen,  #config{transport_info = {Transport, _, _, _}}}}, Options) when is_port(Listen), is_list(Options) ->
    tls_socket:getstat(Transport, Listen, Options);

getstat(#sslsocket{pid = [Pid|_], fd = {Transport, Socket, _, _}}, Options) when is_pid(Pid), is_list(Options) ->
    tls_socket:getstat(Transport, Socket, Options).

%%---------------------------------------------------------------
-spec shutdown(#sslsocket{}, read | write | read_write) ->  ok | {error, reason()}.
%%
%% Description: Same as gen_tcp:shutdown/2
%%--------------------------------------------------------------------
shutdown(#sslsocket{pid = {Listen, #config{transport_info = {Transport,_, _, _}}}},
	 How) when is_port(Listen) ->
    Transport:shutdown(Listen, How);
shutdown(#sslsocket{pid = {dtls,_}},_) ->
    {error, enotconn};
shutdown(#sslsocket{pid = [Pid|_]}, How) when is_pid(Pid) ->
    ssl_connection:shutdown(Pid, How).

%%--------------------------------------------------------------------
-spec sockname(#sslsocket{}) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, reason()}.
%%
%% Description: Same as inet:sockname/1
%%--------------------------------------------------------------------
sockname(#sslsocket{pid = {Listen,  #config{transport_info = {Transport, _, _, _}}}}) when is_port(Listen) ->
    tls_socket:sockname(Transport, Listen);
sockname(#sslsocket{pid = {dtls, #config{dtls_handler = {Pid, _}}}}) ->
    dtls_packet_demux:sockname(Pid);
sockname(#sslsocket{pid = [Pid|_], fd = {Transport, Socket, _}}) when is_pid(Pid) ->
    dtls_socket:sockname(Transport, Socket);
sockname(#sslsocket{pid = [Pid| _], fd = {Transport, Socket, _, _}}) when is_pid(Pid) ->
    tls_socket:sockname(Transport, Socket).

%%---------------------------------------------------------------
-spec versions() -> [{ssl_app, string()} | {supported, [tls_record:tls_atom_version()]} |
                     {supported_dtls, [dtls_record:dtls_atom_version()]} |
		     {available, [tls_record:tls_atom_version()]} |
                     {available_dtls, [dtls_record:dtls_atom_version()]}].
%%
%% Description: Returns a list of relevant versions.
%%--------------------------------------------------------------------
versions() ->
    TLSVsns = tls_record:supported_protocol_versions(),
    DTLSVsns = dtls_record:supported_protocol_versions(),
    SupportedTLSVsns = [tls_record:protocol_version(Vsn) || Vsn <- TLSVsns],
    SupportedDTLSVsns = [dtls_record:protocol_version(Vsn) || Vsn <- DTLSVsns],
    AvailableTLSVsns = ?ALL_AVAILABLE_VERSIONS,
    AvailableDTLSVsns = ?ALL_AVAILABLE_DATAGRAM_VERSIONS,
    [{ssl_app, "9.2"}, {supported, SupportedTLSVsns}, 
     {supported_dtls, SupportedDTLSVsns}, 
     {available, AvailableTLSVsns}, 
     {available_dtls, AvailableDTLSVsns}].


%%---------------------------------------------------------------
-spec renegotiate(#sslsocket{}) -> ok | {error, reason()}.
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

%%--------------------------------------------------------------------
-spec prf(#sslsocket{}, binary() | 'master_secret', binary(),
	  [binary() | prf_random()], non_neg_integer()) ->
		 {ok, binary()} | {error, reason()}.
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
-spec format_error({error, term()}) -> list().
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
-spec suite_to_str(erl_cipher_suite()) -> string().
%%
%% Description: Return the string representation of a cipher suite.
%%--------------------------------------------------------------------
suite_to_str(Cipher) ->
    ssl_cipher_format:suite_to_str(Cipher).


%%--------------------------------------------------------------------
-spec set_log_level(atom()) -> ok | {error, term()}.
%%
%% Description: Set log level for the SSL application
%%--------------------------------------------------------------------
set_log_level(Level) ->
    case application:get_all_key(ssl) of
        {ok, PropList} ->
            Modules = proplists:get_value(modules, PropList),
            set_module_level(Modules, Level);
        undefined ->
            {error, ssl_not_started}
    end.

set_module_level(Modules, Level) ->
    Fun = fun (Module) ->
                  ok = logger:set_module_level(Module, Level)
          end,
    try lists:map(Fun, Modules) of
        _ ->
            ok
    catch
        error:{badmatch, Error} ->
            Error
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

supported_suites(default, Version) ->  
    ssl_cipher:suites(Version);
supported_suites(all, Version) ->  
    ssl_cipher:all_suites(Version);
supported_suites(anonymous, Version) -> 
    ssl_cipher:anonymous_suites(Version).

do_listen(Port, #config{transport_info = {Transport, _, _, _}} = Config, tls_connection) ->
    tls_socket:listen(Transport, Port, Config);

do_listen(Port,  Config, dtls_connection) ->
    dtls_socket:listen(Port, Config).
	
%% Handle extra ssl options given to ssl_accept
-spec handle_options([any()], #ssl_options{}) -> #ssl_options{}
      ;             ([any()], client | server) -> {ok, #config{}}.
handle_options(Opts, Role) ->
    handle_options(Opts, Role, undefined).   


handle_options(Opts0, #ssl_options{protocol = Protocol, cacerts = CaCerts0,
				   cacertfile = CaCertFile0} = InheritedSslOpts, _) ->
    RecordCB = record_cb(Protocol),
    CaCerts = handle_option(cacerts, Opts0, CaCerts0),
    {Verify, FailIfNoPeerCert, CaCertDefault, VerifyFun, PartialChainHanlder,
     VerifyClientOnce} = handle_verify_options(Opts0, CaCerts),
    CaCertFile = case proplists:get_value(cacertfile, Opts0, CaCertFile0) of
		     undefined ->
			 CaCertDefault;
		     CAFile ->
			 CAFile
		 end,

    NewVerifyOpts = InheritedSslOpts#ssl_options{cacerts = CaCerts,
						 cacertfile = CaCertFile,
						 verify = Verify,
						 verify_fun = VerifyFun,
						 partial_chain = PartialChainHanlder,
						 fail_if_no_peer_cert = FailIfNoPeerCert,
						 verify_client_once = VerifyClientOnce},
    SslOpts1 = lists:foldl(fun(Key, PropList) ->
				   proplists:delete(Key, PropList)
			   end, Opts0, [cacerts, cacertfile, verify, verify_fun, partial_chain,
					fail_if_no_peer_cert, verify_client_once]),
    case handle_option(versions, SslOpts1, []) of
	[] ->
	    new_ssl_options(SslOpts1, NewVerifyOpts, RecordCB);
	Value ->
            Versions0 = [RecordCB:protocol_version(Vsn) || Vsn <- Value],
            Versions1 = lists:sort(fun RecordCB:is_higher/2, Versions0),
	    new_ssl_options(proplists:delete(versions, SslOpts1), 
			    NewVerifyOpts#ssl_options{versions = Versions1}, record_cb(Protocol))
    end;

%% Handle all options in listen and connect
handle_options(Opts0, Role, Host) ->
    Opts = proplists:expand([{binary, [{mode, binary}]},
			     {list, [{mode, list}]}], Opts0),
    assert_proplist(Opts),
    RecordCb = record_cb(Opts),
    CaCerts = handle_option(cacerts, Opts, undefined),

    {Verify, FailIfNoPeerCert, CaCertDefault, VerifyFun, PartialChainHanlder, VerifyClientOnce} =
	handle_verify_options(Opts, CaCerts),
    
    CertFile = handle_option(certfile, Opts, <<>>),
    RecordCb = record_cb(Opts),
    
    [HighestVersion|_] = Versions =
        case handle_option(versions, Opts, []) of
            [] ->
                RecordCb:supported_protocol_versions();
            Vsns  ->
                Versions0 = [RecordCb:protocol_version(Vsn) || Vsn <- Vsns],
                lists:sort(fun RecordCb:is_higher/2, Versions0)
        end,

    Protocol = handle_option(protocol, Opts, tls),

    case Versions of
        [{3, 0}] ->
            reject_alpn_next_prot_options(Opts);
        _ ->
            ok
    end,
   
    SSLOptions0 = #ssl_options{
		    versions   = Versions,
		    verify     = validate_option(verify, Verify),
		    verify_fun = VerifyFun,
		    partial_chain = PartialChainHanlder,
		    fail_if_no_peer_cert = FailIfNoPeerCert,
		    verify_client_once = VerifyClientOnce,
		    depth      = handle_option(depth,  Opts, 1),
		    cert       = handle_option(cert, Opts, undefined),
		    certfile   = CertFile,
		    key        = handle_option(key, Opts, undefined),
		    keyfile    = handle_option(keyfile,  Opts, CertFile),
		    password   = handle_option(password, Opts, ""),
		    cacerts    = CaCerts,
		    cacertfile = handle_option(cacertfile, Opts, CaCertDefault),
		    dh         = handle_option(dh, Opts, undefined),
		    dhfile     = handle_option(dhfile, Opts, undefined),
		    user_lookup_fun = handle_option(user_lookup_fun, Opts, undefined),
		    psk_identity = handle_option(psk_identity, Opts, undefined),
		    srp_identity = handle_option(srp_identity, Opts, undefined),
		    ciphers    = handle_cipher_option(proplists:get_value(ciphers, Opts, []), 
						      HighestVersion),
		    eccs       = handle_eccs_option(proplists:get_value(eccs, Opts, eccs()),
                                                    HighestVersion),
                    supported_groups = handle_supported_groups_option(
                                         proplists:get_value(supported_groups, Opts, groups(default)),
                                         HighestVersion),
		    signature_algs =
                         handle_hashsigns_option(
                           proplists:get_value(
                             signature_algs,
                             Opts,
                             default_option_role_sign_algs(server,
                                                 tls_v1:default_signature_algs(HighestVersion),
                                                 Role,
                                                 HighestVersion)),
                           tls_version(HighestVersion)),
                    signature_algs_cert =
                         handle_signature_algorithms_option(
                           proplists:get_value(
                             signature_algs_cert,
                             Opts,
                             undefined),  %% Do not send by default
                           tls_version(HighestVersion)),
                    reuse_sessions = handle_reuse_sessions_option(reuse_sessions, Opts, Role),
		    reuse_session = handle_reuse_session_option(reuse_session, Opts, Role),
		    secure_renegotiate = handle_option(secure_renegotiate, Opts, true),
		    client_renegotiation = handle_option(client_renegotiation, Opts, 
							 default_option_role(server, true, Role), 
							 server, Role),
		    renegotiate_at = handle_option(renegotiate_at, Opts, ?DEFAULT_RENEGOTIATE_AT),
		    hibernate_after = handle_option(hibernate_after, Opts, infinity),
		    erl_dist = handle_option(erl_dist, Opts, false),
		    alpn_advertised_protocols =
			handle_option(alpn_advertised_protocols, Opts, undefined),
		    alpn_preferred_protocols =
			handle_option(alpn_preferred_protocols, Opts, undefined),
		    next_protocols_advertised =
			handle_option(next_protocols_advertised, Opts, undefined),
		    next_protocol_selector =
			make_next_protocol_selector(
			  handle_option(client_preferred_next_protocols, Opts, undefined)),
		    server_name_indication = handle_option(server_name_indication, Opts, 
                                                           default_option_role(client,
                                                                               server_name_indication_default(Host), Role)),
		    sni_hosts = handle_option(sni_hosts, Opts, []),
		    sni_fun = handle_option(sni_fun, Opts, undefined),
		    honor_cipher_order = handle_option(honor_cipher_order, Opts, 
						       default_option_role(server, false, Role), 
						       server, Role),
		    honor_ecc_order = handle_option(honor_ecc_order, Opts,
						       default_option_role(server, false, Role),
						       server, Role),
		    protocol = Protocol,
		    padding_check =  proplists:get_value(padding_check, Opts, true),
		    beast_mitigation = handle_option(beast_mitigation, Opts, one_n_minus_one),
		    fallback = handle_option(fallback, Opts,
					     proplists:get_value(fallback, Opts,    
								 default_option_role(client, 
										     false, Role)),
					     client, Role),
		    crl_check = handle_option(crl_check, Opts, false),
		    crl_cache = handle_option(crl_cache, Opts, {ssl_crl_cache, {internal, []}}),
                    max_handshake_size = handle_option(max_handshake_size, Opts, ?DEFAULT_MAX_HANDSHAKE_SIZE),
                    handshake = handle_option(handshake, Opts, full),
                    customize_hostname_check = handle_option(customize_hostname_check, Opts, [])
		   },
    LogLevel = handle_option(log_alert, Opts, true),
    SSLOptions = SSLOptions0#ssl_options{
                   log_level = handle_option(log_level, Opts, LogLevel)
                  },

    CbInfo  = proplists:get_value(cb_info, Opts, default_cb_info(Protocol)),
    SslOptions = [protocol, versions, verify, verify_fun, partial_chain,
		  fail_if_no_peer_cert, verify_client_once,
		  depth, cert, certfile, key, keyfile,
		  password, cacerts, cacertfile, dh, dhfile,
		  user_lookup_fun, psk_identity, srp_identity, ciphers,
		  reuse_session, reuse_sessions, ssl_imp, client_renegotiation,
		  cb_info, renegotiate_at, secure_renegotiate, hibernate_after,
		  erl_dist, alpn_advertised_protocols, sni_hosts, sni_fun,
		  alpn_preferred_protocols, next_protocols_advertised,
		  client_preferred_next_protocols, log_alert, log_level,
		  server_name_indication, honor_cipher_order, padding_check, crl_check, crl_cache,
		  fallback, signature_algs, signature_algs_cert, eccs, honor_ecc_order,
                  beast_mitigation, max_handshake_size, handshake, customize_hostname_check,
                  supported_groups],

    SockOpts = lists:foldl(fun(Key, PropList) ->
				   proplists:delete(Key, PropList)
			   end, Opts, SslOptions),

    {Sock, Emulated} = emulated_options(Protocol, SockOpts),
    ConnetionCb = connection_cb(Opts),

    {ok, #config{ssl = SSLOptions, emulated = Emulated, inet_ssl = Sock,
		 inet_user = Sock, transport_info = CbInfo, connection_cb = ConnetionCb
		}}.

handle_option(OptionName, Opts, Default, Role, Role) ->
    handle_option(OptionName, Opts, Default);
handle_option(_, _, undefined = Value, _, _) ->
    Value.

handle_option(sni_fun, Opts, Default) ->
    OptFun = validate_option(sni_fun,
                             proplists:get_value(sni_fun, Opts, Default)),
    OptHosts = proplists:get_value(sni_hosts, Opts, undefined),
    case {OptFun, OptHosts} of
        {Default, _} ->
            Default;
        {_, undefined} ->
            OptFun;
        _ ->
            throw({error, {conflict_options, [sni_fun, sni_hosts]}})
    end;
handle_option(OptionName, Opts, Default) ->
    validate_option(OptionName,
		    proplists:get_value(OptionName, Opts, Default)).

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
validate_option(verify_client_once, Value) when is_boolean(Value) ->
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
validate_option(Opt, Value) ->
    throw({error, {options, {Opt, Value}}}).

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

handle_reuse_sessions_option(Key, Opts, client) ->
    Value = proplists:get_value(Key, Opts, true),
    validate_option(Key, Value),
    Value;
handle_reuse_sessions_option(Key, Opts0, server) ->
    Opts = proplists:delete({Key, save}, Opts0),
    Value = proplists:get_value(Key, Opts, true),
    validate_option(Key, Value),
    Value.

handle_reuse_session_option(Key, Opts, client) ->
    Value = proplists:get_value(Key, Opts, undefined),
    validate_option(Key, Value),
    Value;
handle_reuse_session_option(Key, Opts, server) ->
    ReuseSessionFun = fun(_, _, _, _) -> true end,
    Value = proplists:get_value(Key, Opts, ReuseSessionFun),
    validate_option(Key, Value),
    Value.

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
                                                   Version == tlsv1;
                                                   Version == sslv3 ->
    tls_validate_versions(Rest, Versions);                                      
validate_versions([Version | Rest], Versions) when Version == 'dtlsv1';
                                                   Version == 'dtlsv1.2'->
    dtls_validate_versions(Rest, Versions);
validate_versions([Ver| _], Versions) ->
    throw({error, {options, {Ver, {versions, Versions}}}}).

tls_validate_versions([], Versions) ->
    Versions;
tls_validate_versions([Version | Rest], Versions) when Version == 'tlsv1.3';
                                                       Version == 'tlsv1.2';
                                                       Version == 'tlsv1.1';
                                                       Version == tlsv1;
                                                       Version == sslv3 ->
    tls_validate_versions(Rest, Versions);                  
tls_validate_versions([Ver| _], Versions) ->
    throw({error, {options, {Ver, {versions, Versions}}}}).

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

handle_cipher_option(Value, Version)  when is_list(Value) ->
    try binary_cipher_suites(Version, Value) of
	Suites ->
	    Suites
    catch
	exit:_ ->
	    throw({error, {options, {ciphers, Value}}});
	error:_->
	    throw({error, {options, {ciphers, Value}}})
    end.

binary_cipher_suites(Version, []) -> 
    %% Defaults to all supported suites that does
    %% not require explicit configuration
    default_binary_suites(Version);
binary_cipher_suites(Version, [Map|_] = Ciphers0) when is_map(Map) ->
    Ciphers = [ssl_cipher_format:suite(C) || C <- Ciphers0],
    binary_cipher_suites(Version, Ciphers);
binary_cipher_suites(Version, [Tuple|_] = Ciphers0) when is_tuple(Tuple) ->
    Ciphers = [ssl_cipher_format:suite(tuple_to_map(C)) || C <- Ciphers0],
    binary_cipher_suites(Version, Ciphers);
binary_cipher_suites(Version, [Cipher0 | _] = Ciphers0) when is_binary(Cipher0) ->
    All = ssl_cipher:all_suites(Version) ++ 
        ssl_cipher:anonymous_suites(Version),
    case [Cipher || Cipher <- Ciphers0, lists:member(Cipher, All)] of
	[] ->
	    %% Defaults to all supported suites that does
	    %% not require explicit configuration
	    default_binary_suites(Version);
	Ciphers ->
	    Ciphers
    end;
binary_cipher_suites(Version, [Head | _] = Ciphers0) when is_list(Head) ->
    %% Format: ["RC4-SHA","RC4-MD5"]
    Ciphers = [ssl_cipher_format:openssl_suite(C) || C <- Ciphers0],
    binary_cipher_suites(Version, Ciphers);
binary_cipher_suites(Version, Ciphers0)  ->
    %% Format: "RC4-SHA:RC4-MD5"
    Ciphers = [ssl_cipher_format:openssl_suite(C) || C <- string:lexemes(Ciphers0, ":")],
    binary_cipher_suites(Version, Ciphers).

default_binary_suites(Version) ->
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

new_ssl_options([], #ssl_options{} = Opts, _) -> 
    Opts;
new_ssl_options([{verify_client_once, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{verify_client_once = 
					       validate_option(verify_client_once, Value)}, RecordCB); 
new_ssl_options([{depth, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{depth = validate_option(depth, Value)}, RecordCB);
new_ssl_options([{cert, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{cert = validate_option(cert, Value)}, RecordCB);
new_ssl_options([{certfile, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{certfile = validate_option(certfile, Value)}, RecordCB);
new_ssl_options([{key, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{key = validate_option(key, Value)}, RecordCB);
new_ssl_options([{keyfile, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{keyfile = validate_option(keyfile, Value)}, RecordCB);
new_ssl_options([{password, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{password = validate_option(password, Value)}, RecordCB);
new_ssl_options([{dh, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{dh = validate_option(dh, Value)}, RecordCB);
new_ssl_options([{dhfile, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{dhfile = validate_option(dhfile, Value)}, RecordCB); 
new_ssl_options([{user_lookup_fun, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{user_lookup_fun = validate_option(user_lookup_fun, Value)}, RecordCB);
new_ssl_options([{psk_identity, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{psk_identity = validate_option(psk_identity, Value)}, RecordCB);
new_ssl_options([{srp_identity, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{srp_identity = validate_option(srp_identity, Value)}, RecordCB);
new_ssl_options([{ciphers, Value} | Rest], #ssl_options{versions = Versions} = Opts, RecordCB) -> 
    Ciphers = handle_cipher_option(Value, RecordCB:highest_protocol_version(Versions)),
    new_ssl_options(Rest, 
		    Opts#ssl_options{ciphers = Ciphers}, RecordCB);
new_ssl_options([{reuse_session, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{reuse_session = validate_option(reuse_session, Value)}, RecordCB);
new_ssl_options([{reuse_sessions, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{reuse_sessions = validate_option(reuse_sessions, Value)}, RecordCB);
new_ssl_options([{ssl_imp, _Value} | Rest], #ssl_options{} = Opts, RecordCB) -> %% Not used backwards compatibility
    new_ssl_options(Rest, Opts, RecordCB);
new_ssl_options([{renegotiate_at, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{ renegotiate_at = validate_option(renegotiate_at, Value)}, RecordCB);
new_ssl_options([{secure_renegotiate, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{secure_renegotiate = validate_option(secure_renegotiate, Value)}, RecordCB); 
new_ssl_options([{client_renegotiation, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{client_renegotiation = validate_option(client_renegotiation, Value)}, RecordCB); 
new_ssl_options([{hibernate_after, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{hibernate_after = validate_option(hibernate_after, Value)}, RecordCB);
new_ssl_options([{alpn_advertised_protocols, Value} | Rest], #ssl_options{} = Opts, RecordCB) ->
	new_ssl_options(Rest, Opts#ssl_options{alpn_advertised_protocols = validate_option(alpn_advertised_protocols, Value)}, RecordCB);
new_ssl_options([{alpn_preferred_protocols, Value} | Rest], #ssl_options{} = Opts, RecordCB) ->
	new_ssl_options(Rest, Opts#ssl_options{alpn_preferred_protocols = validate_option(alpn_preferred_protocols, Value)}, RecordCB);
new_ssl_options([{next_protocols_advertised, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{next_protocols_advertised = validate_option(next_protocols_advertised, Value)}, RecordCB);
new_ssl_options([{client_preferred_next_protocols, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{next_protocol_selector = 
					       make_next_protocol_selector(validate_option(client_preferred_next_protocols, Value))}, RecordCB);
new_ssl_options([{log_alert, Value} | Rest], #ssl_options{} = Opts, RecordCB) ->
    new_ssl_options(Rest, Opts#ssl_options{log_level = validate_option(log_alert, Value)}, RecordCB);
new_ssl_options([{log_level, Value} | Rest], #ssl_options{} = Opts, RecordCB) ->
    new_ssl_options(Rest, Opts#ssl_options{log_level = validate_option(log_level, Value)}, RecordCB);
new_ssl_options([{server_name_indication, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{server_name_indication = validate_option(server_name_indication, Value)}, RecordCB);
new_ssl_options([{honor_cipher_order, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{honor_cipher_order = validate_option(honor_cipher_order, Value)}, RecordCB);
new_ssl_options([{honor_ecc_order, Value} | Rest], #ssl_options{} = Opts, RecordCB) ->
    new_ssl_options(Rest, Opts#ssl_options{honor_ecc_order = validate_option(honor_ecc_order, Value)}, RecordCB);
new_ssl_options([{eccs, Value} | Rest], #ssl_options{} = Opts, RecordCB) ->
    new_ssl_options(Rest,
		    Opts#ssl_options{eccs =
			 handle_eccs_option(Value, RecordCB:highest_protocol_version())
		    },
		    RecordCB);
new_ssl_options([{supported_groups, Value} | Rest], #ssl_options{} = Opts, RecordCB) ->
    new_ssl_options(Rest,
		    Opts#ssl_options{supported_groups =
			 handle_supported_groups_option(Value, RecordCB:highest_protocol_version())
		    },
		    RecordCB);
new_ssl_options([{signature_algs, Value} | Rest], #ssl_options{} = Opts, RecordCB) -> 
    new_ssl_options(Rest, 
		    Opts#ssl_options{signature_algs = 
					 handle_hashsigns_option(Value, 
								 tls_version(RecordCB:highest_protocol_version()))}, 
		    RecordCB);
new_ssl_options([{signature_algs_cert, Value} | Rest], #ssl_options{} = Opts, RecordCB) ->
    new_ssl_options(
      Rest,
      Opts#ssl_options{signature_algs_cert =
                           handle_signature_algorithms_option(
                             Value,
                             tls_version(RecordCB:highest_protocol_version()))},
      RecordCB);
new_ssl_options([{protocol, dtls = Value} | Rest], #ssl_options{} = Opts, dtls_record = RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{protocol = Value}, RecordCB);
new_ssl_options([{protocol, tls = Value} | Rest], #ssl_options{} = Opts, tls_record = RecordCB) -> 
    new_ssl_options(Rest, Opts#ssl_options{protocol = Value}, RecordCB);
new_ssl_options([{Key, Value} | _Rest], #ssl_options{}, _) -> 
    throw({error, {options, {Key, Value}}}).


handle_verify_options(Opts, CaCerts) ->
    DefaultVerifyNoneFun =
	{fun(_,{bad_cert, _}, UserState) ->
		 {valid, UserState};
	    (_,{extension, #'Extension'{critical = true}}, UserState) ->
		 %% This extension is marked as critical, so
		 %% certificate verification should fail if we don't
		 %% understand the extension.  However, this is
		 %% `verify_none', so let's accept it anyway.
		 {valid, UserState};
	    (_,{extension, _}, UserState) ->
		 {unknown, UserState};
	    (_, valid, UserState) ->
		 {valid, UserState};
	    (_, valid_peer, UserState) ->
		 {valid, UserState}
	 end, []},
    VerifyNoneFun = handle_option(verify_fun, Opts, DefaultVerifyNoneFun),

    UserFailIfNoPeerCert = handle_option(fail_if_no_peer_cert, Opts, false),
    UserVerifyFun = handle_option(verify_fun, Opts, undefined),
    
    PartialChainHanlder = handle_option(partial_chain, Opts,
					fun(_) -> unknown_ca end),

    VerifyClientOnce = handle_option(verify_client_once, Opts, false),

    %% Handle 0, 1, 2 for backwards compatibility
    case proplists:get_value(verify, Opts, verify_none) of
	0 ->
	    {verify_none, false,
		 ca_cert_default(verify_none, VerifyNoneFun, CaCerts),
	     VerifyNoneFun, PartialChainHanlder, VerifyClientOnce};
	1  ->
	    {verify_peer, false,
	     ca_cert_default(verify_peer, UserVerifyFun, CaCerts),
	     UserVerifyFun, PartialChainHanlder, VerifyClientOnce};
	2 ->
	    {verify_peer, true,
	     ca_cert_default(verify_peer, UserVerifyFun, CaCerts),
	     UserVerifyFun, PartialChainHanlder, VerifyClientOnce};
	verify_none ->
	    {verify_none, false,
	     ca_cert_default(verify_none, VerifyNoneFun, CaCerts),
	     VerifyNoneFun, PartialChainHanlder, VerifyClientOnce};
	verify_peer ->
	    {verify_peer, UserFailIfNoPeerCert,
	     ca_cert_default(verify_peer, UserVerifyFun, CaCerts),
	     UserVerifyFun, PartialChainHanlder, VerifyClientOnce};
	Value ->
	    throw({error, {options, {verify, Value}}})
    end.

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
    {gen_tcp, tcp, tcp_closed, tcp_error};
default_cb_info(dtls) ->
    {gen_udp, udp, udp_closed, udp_error}.

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
    Host;
server_name_indication_default(_) ->
    undefined.


reject_alpn_next_prot_options(Opts) ->
    AlpnNextOpts = [alpn_advertised_protocols,
                    alpn_preferred_protocols,
                    next_protocols_advertised,
                    next_protocol_selector,
                    client_preferred_next_protocols],
    reject_alpn_next_prot_options(AlpnNextOpts, Opts).

reject_alpn_next_prot_options([], _) ->
    ok;
reject_alpn_next_prot_options([Opt| AlpnNextOpts], Opts) ->
    case lists:keyfind(Opt, 1, Opts) of
        {Opt, Value} ->
            throw({error, {options, {not_supported_in_sslv3, {Opt, Value}}}});
        false ->
            reject_alpn_next_prot_options(AlpnNextOpts, Opts)
    end.

add_filter(undefined, Filters) ->
    Filters;
add_filter(Filter, Filters) ->
    [Filter | Filters].
