%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2025. All Rights Reserved.
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

-module(ssl_config).
-moduledoc false.

-include("ssl_internal.hrl").
-include("ssl_connection.hrl").
-include_lib("public_key/include/public_key.hrl"). 

-define(DEFAULT_MAX_SESSION_CACHE, 1000).
-define(TWO_HOURS, 7200).
-define(SEVEN_DAYS, 604800). 

%% Connection parameter configuration
-export([init/2,
         new_emulated/2,
         handle_options/3,
         handle_options/4,
         update_options/3
        ]).

%% Application configuration
-export([pre_1_3_session_opts/1,
         get_max_early_data_size/0,
         get_ticket_lifetime/0,
         get_ticket_store_size/0,
         get_internal_active_n/0,
         get_internal_active_n/1
        ]).

%% Tracing
-export([handle_trace/3]).

%%====================================================================
%% Internal application API
%%====================================================================
init(SslOpts, Role) ->
    init_manager_name(maps:get(erl_dist, SslOpts, false)),
    #{pem_cache := PemCache} = Config = init_cacerts(SslOpts, Role),
    DHParams = init_diffie_hellman(PemCache, SslOpts, Role),
    CertKeyAlts = init_certs_keys(SslOpts, Role, PemCache),
    {ok, Config#{cert_key_alts => CertKeyAlts, dh_params => DHParams}}.

new_emulated([], EmOpts) ->
    EmOpts;
new_emulated(NewEmOpts, _) ->
    NewEmOpts.

handle_options(Opts, Role, Host) ->
    handle_options(undefined, undefined, Opts, Role, Host).

handle_options(Socket, Opts, Role, Host) ->
    CbInfo = handle_option_cb_info(Opts, tls, Socket),
    Transport = element(1, CbInfo),
    handle_options(Transport, Socket, Opts, Role, Host).

%% Handle all options in listen, connect and handshake
handle_options(Transport, Socket, Opts0, Role, Host) ->
    {UserSslOptsList, SockOpts0} = split_options(Opts0, ssl_options()),
    NeedValidate = not (Socket == undefined) andalso Role =:= server, %% handshake options
    Env = #{role => Role, host => Host,
            validate_certs_or_anon_ciphers => NeedValidate
           },
    SslOpts = process_options(UserSslOptsList, #{}, Env),

    %% Handle special options
    #{protocol := Protocol} = SslOpts,
    {Sock, Emulated} = emulated_options(Transport, Socket, Protocol, SockOpts0),
    ConnetionCb = connection_cb(Protocol),
    CbInfo = handle_option_cb_info(Opts0, Protocol, Socket),

    {ok, #config{
            ssl = SslOpts,
            emulated = Emulated,
            inet_ssl = Sock,
            inet_user = Sock,
            transport_info = CbInfo,
            connection_cb = ConnetionCb
           }}.


pre_1_3_session_opts(Role) ->
    {Cb, InitArgs} = session_cb_opts(Role),
    CbOpts = #{session_cb => Cb,
               session_cb_init_args => InitArgs},
    LifeTime = session_lifetime(Role),
    Max = max_session_cache_size(Role),
    CbOpts#{lifetime => LifeTime, max => Max}.

get_ticket_lifetime() ->
    case application_int(server_session_ticket_lifetime, ?TWO_HOURS) of
	Seconds when Seconds =< ?SEVEN_DAYS ->
            Seconds;
        _  ->
            ?TWO_HOURS
    end.

get_ticket_store_size() ->
    application_int(server_session_ticket_store_size, 1000).

get_max_early_data_size() ->
    application_int(server_session_ticket_max_early_data, ?DEFAULT_MAX_EARLY_DATA_SIZE).

get_internal_active_n() ->
    get_internal_active_n(false).

get_internal_active_n(true) ->
    %% Start with a random number between 1 and ?INTERNAL_ACTIVE_N
    %% In most cases distribution connections are established all at
    %%  the same time, and flow control engages with ?INTERNAL_ACTIVE_N for
    %%  all connections. Which creates a wave of "passive" messages, leading
    %%  to significant bump of memory & scheduler utilisation. Starting with
    %%  a random number between 1 and ?INTERNAL_ACTIVE_N helps to spread the
    %%  spike.
    erlang:system_time() rem ?INTERNAL_ACTIVE_N + 1;
get_internal_active_n(false) ->
    application_int(internal_active_n, ?INTERNAL_ACTIVE_N).

%%====================================================================
%% Internal functions 
%%====================================================================	     

%%====================================================================
%% Certificate and  Key configuration
%%====================================================================
init_certs_keys(#{certs_keys := CertsKeys} = Opts, Role, PemCache) ->
    Pairs = lists:map(fun(CertKey) -> 
                              init_cert_key_pair(CertKey, Role, PemCache) 
                      end, CertsKeys),
    CertKeyGroups = group_pairs(Pairs),
    prioritize_groups(CertKeyGroups, Opts).

init_cert_key_pair(CertKey, Role, PemCache) ->
    Certs = init_certificates(CertKey, PemCache, Role),
    PrivateKey = init_private_key(maps:get(key, CertKey, undefined), 
                                  CertKey, PemCache),
    #{private_key => PrivateKey, certs => Certs}.

group_pairs([#{certs := []}]) ->
    #{eddsa => [],
      ecdsa => [],
      rsa_pss_pss => [],
      rsa => [],
      dsa => []
     };
group_pairs(Pairs) ->
    group_pairs(Pairs, #{eddsa => [],
                         ecdsa => [],
                         rsa_pss_pss => [],
                         rsa => [],
                         dsa => []
                        }).

group_pairs([#{private_key := #'ECPrivateKey'{parameters = {namedCurve, ?'id-Ed25519'}}} = Pair | Rest], 
            #{eddsa := EDDSA} = Group) ->
    group_pairs(Rest, Group#{eddsa => [Pair | EDDSA]});
group_pairs([#{private_key := #'ECPrivateKey'{parameters = {namedCurve, ?'id-Ed448'}}} = Pair | Rest], 
            #{eddsa := EDDSA} = Group) ->
    group_pairs(Rest, Group#{eddsa => [Pair | EDDSA]});
group_pairs([#{private_key := #'ECPrivateKey'{}} = Pair | Rest], #{ecdsa := ECDSA} = Group) ->
    group_pairs(Rest, Group#{ecdsa => [Pair | ECDSA]});
group_pairs([#{private_key := {#'RSAPrivateKey'{}, #'RSASSA-PSS-params'{}}} = Pair | Rest], 
            #{rsa_pss_pss := RSAPSS} = Group) ->
    group_pairs(Rest, Group#{rsa_pss_pss => [Pair | RSAPSS]});
group_pairs([#{private_key := #'RSAPrivateKey'{}} = Pair | Rest], #{rsa := RSA} = Group) ->
    group_pairs(Rest, Group#{rsa => [Pair | RSA]});
group_pairs([#{private_key := #'DSAPrivateKey'{}} = Pair | Rest], #{dsa := DSA} = Group) ->
    group_pairs(Rest, Group#{dsa => [Pair | DSA]});
group_pairs([#{private_key := #{algorithm := dss, engine := _}} = Pair | Rest], Group) ->
    Pairs = maps:get(dsa, Group),
    group_pairs(Rest, Group#{dsa => [Pair | Pairs]});
group_pairs([#{private_key := #{algorithm := Alg, sign_fun := _}} = Pair | Rest], Group) ->
    Pairs = maps:get(Alg, Group),
    group_pairs(Rest, Group#{Alg => [Pair | Pairs]});
group_pairs([#{private_key := #{algorithm := Alg, engine := _}} = Pair | Rest], Group) ->
    Pairs = maps:get(Alg, Group),
    group_pairs(Rest, Group#{Alg => [Pair | Pairs]});
group_pairs([], Group) ->
    Group.

prioritize_groups(#{eddsa := EDDSA,
                    ecdsa := ECDSA,
                    rsa_pss_pss := RSAPSS,
                    rsa := RSA,
                    dsa := DSA} = CertKeyGroups, Opts) ->
    EC = ecdsa_support(Opts),
    CertKeyGroups#{eddsa => prio_eddsa(EDDSA),
                   ecdsa => prio_ecdsa(ECDSA, EC),
                   rsa_pss_pss => prio_rsa_pss(RSAPSS),
                   rsa => prio_rsa(RSA),
                   dsa => prio_dsa(DSA)}.

prio_eddsa(EDDSA) ->
    %% Engine not supported yet
    SignFunPairs = [Pair || Pair = #{private_key := #{sign_fun := _}} <- EDDSA],
    SignFunPairs
        ++ using_curve({namedCurve, ?'id-Ed25519'}, EDDSA, [])
        ++ using_curve({namedCurve, ?'id-Ed448'}, EDDSA, []).

prio_ecdsa(ECDSA, Curves) ->
    EnginePairs = [Pair || Pair = #{private_key := #{engine := _}} <- ECDSA],
    SignFunPairs = [Pair || Pair = #{private_key := #{sign_fun := _}} <- ECDSA],
    EnginePairs
        ++ SignFunPairs
        ++ lists:foldr(
            fun(Curve, AccIn) ->
                CurveOid = pubkey_cert_records:namedCurves(Curve),
                Pairs = using_curve({namedCurve, CurveOid}, ECDSA -- EnginePairs -- SignFunPairs, []),
                Pairs ++ AccIn
             end, [], Curves).

using_curve(_, [], Acc) ->
    lists:reverse(Acc);
using_curve(Curve, [#{private_key := #'ECPrivateKey'{parameters = Curve}} = Pair | Rest], Acc) ->
    using_curve(Curve, Rest, [Pair | Acc]);
using_curve(Curve, [_ | Rest], Acc) ->
    using_curve(Curve, Rest, Acc).

prio_rsa_pss(RSAPSS) ->
       Order = fun(#{privat_key := {#'RSAPrivateKey'{modulus = N}, Params1}},
                   #{private_key := {#'RSAPrivateKey'{modulus = N}, Params2}}) ->
                       prio_params_1(Params1, Params2);
                  (#{private_key := {#'RSAPrivateKey'{modulus = N}, _}},
                   #{private_key := {#'RSAPrivateKey'{modulus = M}, _}}) when M > N ->
                       true;
                  (#{private_key := #{engine := _}}, _) ->
                       true;
                  (_,_) ->
                       false
               end,
    lists:sort(Order, RSAPSS).

prio_params_1(#'RSASSA-PSS-params'{hashAlgorithm = #'HashAlgorithm'{algorithm = Oid1}},
              #'RSASSA-PSS-params'{hashAlgorithm = #'HashAlgorithm'{algorithm = Oid2}}) ->
    public_key:pkix_hash_type(Oid1) > public_key:pkix_hash_type(Oid2).

prio_rsa(RSA) ->
    Order = fun(#{key := #'RSAPrivateKey'{modulus = N}},
                #{key := #'RSAPrivateKey'{modulus = M}}) when M > N ->
                    true;
               (#{private_key := #{engine := _}}, _) ->
                    true;
               (_,_) ->
                    false
            end,
    lists:sort(Order, RSA).

prio_dsa(DSA) ->
    Order = fun(#{key := #'DSAPrivateKey'{q = N}},
                #{key := #'DSAPrivateKey'{q = M}}) when M > N ->
                    true;
               (#{private_key := #{engine := _}}, _) ->
                    true;
               (_,_) ->
                    false
    end,
    lists:sort(Order, DSA).

init_manager_name(false) ->
    put(ssl_manager, ssl_manager:name(normal)),
    put(ssl_pem_cache, ssl_pem_cache:name(normal));
init_manager_name(true) ->
    put(ssl_manager, ssl_manager:name(dist)),
    put(ssl_pem_cache, ssl_pem_cache:name(dist)).

init_cacerts(#{cacerts := CaCerts, crl_cache := CRLCache} = Opts, Role) ->
    CACertFile = maps:get(cacertfile, Opts, <<>>),
    {ok, Config} =
	try
	    Certs = case CaCerts of
			undefined -> CACertFile;
			_ -> {der, CaCerts}
		    end,
	    {ok,_} = ssl_manager:connection_init(Certs, Role, CRLCache)
	catch
	    _:Reason ->
		file_error(CACertFile, {cacertfile, Reason})
	end,
    Config.

init_certificates(CertKey, PemCache, Role) ->
    case maps:get(cert, CertKey, undefined) of
        undefined ->
            init_certificate_file(maps:get(certfile, CertKey, <<>>), PemCache, Role);
        Bin when is_binary(Bin) ->
            [Bin];
        Certs when is_list(Certs) ->
            Certs
    end.

init_certificate_file(<<>>, _PemCache, _Role) ->
    [];
init_certificate_file(CertFile, PemCache, Role) ->
    try %% OwnCert | [OwnCert | Chain]
        ssl_certificate:file_to_certificats(CertFile, PemCache)
    catch
        _Error:_Reason when Role =:= client ->
            [];
        _Error:Reason ->
            file_error(CertFile, {certfile, Reason})
    end.

init_private_key(#{algorithm := _, sign_fun := _SignFun} = Key, _, _) ->
    Key;
init_private_key(#{algorithm := Alg} = Key, _, _PemCache)
  when Alg =:= ecdsa; Alg =:= rsa; Alg =:= dss ->
    case maps:is_key(engine, Key) andalso maps:is_key(key_id, Key) of
        true ->  Key;
        false -> throw({key, {invalid_key_id, Key}})
    end;
init_private_key({Asn1Type, PrivateKey},_,_) ->
    private_key(public_key:der_decode(Asn1Type, PrivateKey));
init_private_key(undefined, CertKey, DbHandle) ->
    case maps:get(keyfile, CertKey, undefined) of
        undefined ->
            #{};
        KeyFile ->
            Password = maps:get(password, CertKey, undefined),
            try
                {ok, List} = ssl_manager:cache_pem_file(KeyFile, DbHandle),
                [PemEntry] = [PemEntry || PemEntry = {PKey, _ , _} <- List,
                                          PKey =:= 'RSAPrivateKey' orelse
                                              PKey =:= 'DSAPrivateKey' orelse
                                              PKey =:= 'ECPrivateKey' orelse
                                              PKey =:= 'PrivateKeyInfo'
                             ],
                private_key(public_key:pem_entry_decode(PemEntry, Password))
            catch
                _:Reason ->
                    file_error(KeyFile, {keyfile, Reason})
            end
    end.

private_key(#'PrivateKeyInfo'{privateKeyAlgorithm =
				 #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?'rsaEncryption'},
			     privateKey = Key}) ->
    public_key:der_decode('RSAPrivateKey', iolist_to_binary(Key));

private_key(#'PrivateKeyInfo'{privateKeyAlgorithm =
				 #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?'id-dsa'},
			     privateKey = Key}) ->
    public_key:der_decode('DSAPrivateKey', iolist_to_binary(Key));
private_key(#'PrivateKeyInfo'{privateKeyAlgorithm = 
                                  #'PrivateKeyInfo_privateKeyAlgorithm'{algorithm = ?'id-ecPublicKey',
                                                                        parameters =  {asn1_OPENTYPE, Parameters}},
                              privateKey = Key}) ->
    ECKey = public_key:der_decode('ECPrivateKey',  iolist_to_binary(Key)),
    ECParameters = public_key:der_decode('EcpkParameters', Parameters),
    ECKey#'ECPrivateKey'{parameters = ECParameters};
private_key(Key) ->
    Key.

ecdsa_support(#{versions := [?TLS_1_3]}) ->
    [secp521r1,
     secp384r1,
     secp256r1,
     brainpoolP512r1,
     brainpoolP384r1,
     brainpoolP256r1
    ];
ecdsa_support(_) ->
    ssl:eccs() -- [x25519, x448].


-spec(file_error(_,_) -> no_return()).
file_error(File, Throw) ->
    case Throw of
	{Opt,{badmatch, {error, {badmatch, Error}}}} ->
	    throw({options, {Opt, binary_to_list(File), Error}});
	{Opt, {badmatch, Error}} ->
	    throw({options, {Opt, binary_to_list(File), Error}});
	_ ->
	    throw(Throw)
    end.

init_diffie_hellman(_, _, client) ->
    undefined;
init_diffie_hellman(DbHandle, Opts, server) ->
    case maps:get(dh, Opts, undefined) of
        Bin when is_binary(Bin) ->
            public_key:der_decode('DHParameter', Bin);
        _ ->
            case maps:get(dhfile, Opts, undefined) of
                undefined ->
                    ?DEFAULT_DIFFIE_HELLMAN_PARAMS;
                DHParamFile ->
                    dh_file(DbHandle, DHParamFile)
            end
    end.

dh_file(DbHandle, DHParamFile) ->
    try
        {ok, List} = ssl_manager:cache_pem_file(DHParamFile,DbHandle),
        case [Entry || Entry = {'DHParameter', _ , _} <- List] of
            [Entry] ->
                public_key:pem_entry_decode(Entry);
            [] ->
                ?DEFAULT_DIFFIE_HELLMAN_PARAMS
        end
    catch
        _:Reason ->
            file_error(DHParamFile, {dhfile, Reason}) 
    end.

%%====================================================================
%% Connection options
%%====================================================================
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
     stapling,
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
     verify, verify_fun, cert_policy_opts,
     allow_any_ca_purpose,
     versions
    ].

%% Handle ssl options at handshake, handshake_continue
-doc false.
-spec update_options([any()], client | server, map()) -> map().
update_options(NewOpts, Role, OriginalSslOpts) when is_map(OriginalSslOpts) ->
    {UserSslOpts, _} = split_options(NewOpts, ssl_options()),
    Env = #{role => Role, validate_certs_or_anon_ciphers => Role == server},
    OrigVersionsOpt = maps:get(versions, OriginalSslOpts, []),
    NewVersions0 = proplists:get_value(versions, NewOpts, []),
    {Record, NewVersions} =
        case maps:get(protocol, OriginalSslOpts, tls) of
            tls ->
                validate_updated_versions(tls, NewVersions0),
                {tls_record, NewVersions0};
            dtls ->
                validate_updated_versions(dtls, NewVersions0),
                {dtls_record, NewVersions0}
             end,
    OrigVersions = [Record:protocol_version(V) || V <- OrigVersionsOpt],
    %% Newversions is on atom format that will sort
    %% correctly on term format for both tls and dtls.
    %% tls_record | dtls_record:is_higher works on {Major:integer(), Minor:integer()}
    %% RFC version format.
    VersionsOpt = lists:sort(fun(V1, V2) -> V1 > V2 end, NewVersions),
    FallBackOptions = handle_possible_version_change(OrigVersions, VersionsOpt,
                                                     OriginalSslOpts, Record),
    process_options(UserSslOpts, FallBackOptions, Env).

validate_updated_versions(_, []) ->
    true;
validate_updated_versions(tls, [_| _] = NewVersions) ->
    validate_versions(tls, NewVersions);
validate_updated_versions(dtls, [_|_] = NewVersions) ->
    validate_versions(dtls, NewVersions).

process_options(UserSslOpts, SslOpts0, Env) ->
    %% Reverse option list so we get the last set option if set twice,
    %% users depend on it.
    UserSslOptsMap = proplists:to_map(lists:reverse(UserSslOpts)),
    SslOpts1  = opt_protocol_versions(UserSslOptsMap, SslOpts0, Env),
    SslOpts2  = opt_verification(UserSslOptsMap, SslOpts1, Env),
    SslOpts3  = opt_certs(UserSslOptsMap, SslOpts2, Env),
    SslOpts4  = opt_tickets(UserSslOptsMap, SslOpts3, Env),
    SslOpts5  = opt_stapling(UserSslOptsMap, SslOpts4, Env),
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
    validate_server_cert_opts(SslOpts, Env),
    SslOpts.

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

    Opts3 = set_opt_int(depth, 0, 255, ?DEFAULT_DEPTH, UserOpts, Opts2),

    Opts4 = case Role of
               client ->
                   opt_verify_fun(UserOpts, Opts3#{partial_chain => PartialChain},
                                  Env);
               server ->
                   opt_verify_fun(UserOpts, Opts3#{partial_chain => PartialChain,
                                                   fail_if_no_peer_cert => FailNoPeerCert},
                                  Env)
           end,
    Opts = opt_policies(UserOpts, Opts4),
    opt_extend_keyusage(UserOpts, Opts).

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

opt_policies(UserOpts, Opts) ->
    case get_opt(cert_policy_opts, [], UserOpts, Opts) of
        {default, []} ->
            Opts#{cert_policy_opts => []};
        {old, POpts} ->
            Opts#{cert_policy_opts => POpts};
        {_, POpts} ->
            validate_policy_opts(POpts),
            Opts#{cert_policy_opts => POpts}
    end.

opt_extend_keyusage(UserOpts, Opts) ->
    case get_opt_bool(allow_any_ca_purpose, false, UserOpts, Opts) of
        {default, Value} ->
            Opts#{allow_any_ca_purpose => Value};
        {old, _OldValue} ->
            Opts;
        {new, NewValue} ->
            Opts#{allow_any_ca_purpose => NewValue}
    end.

validate_policy_opts([]) ->
    true;
validate_policy_opts([{policy_set, OidList} | Rest]) when is_list(OidList) ->
    validate_policy_opts(Rest);
validate_policy_opts([{Opt, Bool} | Rest]) when Opt == explicit_policy;
                                                Opt == inhibit_policy_mapping;
                                                Opt == inhibit_any_policy ->
    case is_boolean(Bool) of
        true ->
            validate_policy_opts(Rest);
        false ->
            option_error(cert_policy_opts, {Opt, Bool})
    end;
validate_policy_opts([Opt| _]) ->
    option_error(cert_policy_opts, Opt).

opt_certs(UserOpts, #{log_level := LogLevel, versions := Versions} = Opts0, Env) ->
    case get_opt_list(certs_keys, [], UserOpts, Opts0) of
        {Where, []} when Where =/= new ->
            opt_old_certs(UserOpts, #{}, Opts0, Env);
        {old, [CertKey]} ->
            opt_old_certs(UserOpts, CertKey, Opts0, Env);
        {Where, CKs0} when is_list(CKs0) ->
            warn_override(Where, UserOpts, certs_keys, [cert,certfile,key,keyfile,password], LogLevel),
            CKs = lists:foldl(fun(CK0, Acc) ->
                                      CK = check_legacy_cert_key(Versions, CK0, #{}, LogLevel),
                                      case maps:size(CK) =:= 0 of
                                          true ->
                                              Acc;
                                          false ->
                                              [CK|Acc]
                                      end
                              end, [], CKs0),
            Opts0#{certs_keys => lists:reverse(CKs)}
    end.

opt_old_certs(UserOpts, CertKeys, #{log_level := LogLevel, versions := Versions}=SSLOpts, _Env) ->
    CK = check_legacy_cert_key(Versions, UserOpts, CertKeys, LogLevel),
    case maps:size(CK) =:= 0 of
        true ->
            SSLOpts#{certs_keys => []};
        false ->
            SSLOpts#{certs_keys => [CK]}
    end.

check_legacy_cert_key(Versions, UserOpts, CertKeys0, LogLevel) ->
    CertKeys1 = handle_legacy_cert_opt(UserOpts, CertKeys0, LogLevel),
    CertKeys = handle_legacy_key_opt(Versions, UserOpts, CertKeys1), 
    handle_possible_legacy_key_password(UserOpts, CertKeys).

handle_legacy_cert_opt(UserOpts, CertKeys, LogLevel) ->
    case get_opt(cert, undefined, UserOpts, CertKeys) of
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
    end.

handle_legacy_key_opt(Versions, UserOpts, CertKeys0) ->
    case get_opt(key, undefined, UserOpts, CertKeys0) of
        {_, undefined} ->
            case get_opt_file(keyfile, <<>>, UserOpts, CertKeys0) of
                {new, KeyFile} ->
                    CertKeys0#{keyfile => KeyFile};
                {_, <<>>} ->
                    case maps:get(certfile, CertKeys0, unbound) of
                        unbound -> CertKeys0;
                        KeyFile -> CertKeys0#{keyfile => KeyFile}
                    end;
                {old, _} ->
                    CertKeys0
            end;
        {_, {KF, K0} = Key}
          when is_binary(K0), KF =:= rsa; KF =:= dsa;
               KF == 'RSAPrivateKey'; KF == 'DSAPrivateKey';
               KF == 'ECPrivateKey'; KF == 'PrivateKeyInfo' ->
            CertKeys0#{key => Key};
        {_, #{engine := _, key_id := _, algorithm := Algo} = Key} ->
            check_key_algo_version_dep(Versions, Algo),
            CertKeys0#{key => Key};
        {_, #{sign_fun := _, algorithm := Algo} = Key} ->
            check_key_algo_version_dep(Versions, Algo),
            check_key_legacy_version_dep(Versions, Key, Algo),
            CertKeys0#{key => Key};
                    {_, #{encrypt_fun := _, algorithm := rsa} = Key} ->
            check_key_legacy_version_dep(Versions, Key),
                        CertKeys0#{key => Key};
        {new, Err1} ->
            option_error(key, Err1)
    end.

handle_possible_legacy_key_password(UserOpts, CertKeys0) ->
    case get_opt(password, unbound, UserOpts, CertKeys0) of
        {default, _} -> CertKeys0;
        {_, Pwd} when is_binary(Pwd); is_list(Pwd) ->
            CertKeys0#{password => fun() -> Pwd end};
        {_, Pwd} when is_function(Pwd, 0) ->
            CertKeys0#{password => Pwd};
        {_, Err2} ->
            option_error(password, Err2)
    end.

check_key_algo_version_dep(Versions, eddsa) ->
    assert_version_dep(key, Versions, ['tlsv1.3']);
check_key_algo_version_dep(Versions, rsa_pss_pss) ->
    assert_version_dep(key, Versions, ['tlsv1.3', 'tlsv1.2']);
check_key_algo_version_dep(Versions, dsa) ->
    assert_version_dep(key, Versions, ['tlsv1.2', 'tlsv1.1', 'tlsv1']);
check_key_algo_version_dep(_,_) ->
    true.

check_key_legacy_version_dep(Versions, Key, rsa) ->
    check_key_legacy_version_dep(Versions, Key);
check_key_legacy_version_dep(_,_,_) ->
    true.

check_key_legacy_version_dep(Versions, Key) ->
    EncryptFun = maps:get(encrypt_fun, Key, undefined),
    case EncryptFun of
        undefined ->
            assert_version_dep(key, Versions, ['tlsv1.3', 'tlsv1.2']);
        _  ->
            assert_version_dep(key, Versions, ['tlsv1.1', 'tlsv1'])
    end.

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
    case Role of
        server ->
            assert_version_dep(Where2 =:= new, certificate_authorities,
                               Versions, ['tlsv1.3', 'tlsv1.2', 'tlsv1.1', 'tlsv1']);
        client ->
            assert_version_dep(Where2 =:= new, certificate_authorities, Versions, ['tlsv1.3'])
    end,
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

opt_stapling(UserOpts, #{versions := _Versions} = Opts, #{role := client}) ->
    {Stapling, Nonce} =
        case get_opt(stapling, ?DEFAULT_STAPLING_OPT, UserOpts, Opts) of
            {old, StaplingMap} when is_map(StaplingMap) ->
                {true, maps:get(ocsp_nonce, StaplingMap, ?DEFAULT_OCSP_NONCE_OPT)};
            {_, staple} ->
                {true, ?DEFAULT_OCSP_NONCE_OPT};
            {_, no_staple} ->
                {false, ignore};
            {_, Map} when is_map(Map) ->
                {true, maps:get(ocsp_nonce, Map, ?DEFAULT_OCSP_NONCE_OPT)};
            {_, Value} ->
                option_error(stapling, Value)
        end,
    case Stapling of
        true ->
            Opts#{stapling =>
                      #{ocsp_nonce => Nonce}};
        false ->
            Opts
    end;
opt_stapling(UserOpts, Opts, #{role := server}) ->
    assert_client_only(stapling, UserOpts),
    Opts.

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
    [TlsVersion|_] = TlsVsns = [ssl:tls_version(V) || V <- Versions],
    case ?TLS_GTE(TlsVersion, ?TLS_1_2) of
        true ->
            opt_signature_algs_valid(UserOpts, Opts, TlsVsns);
        false ->
            opt_signature_algs_not_valid(UserOpts, Opts)
    end.

opt_signature_algs_valid(UserOpts, Opts, [TlsVersion| _] = TlsVsns)->
    AlgCertSchemes0 = valid_signature_algs_cert(Opts, UserOpts, TlsVersion),
    {AlgScemes, AlgCertSchemes} = valid_signature_algs(AlgCertSchemes0, Opts, UserOpts, TlsVsns),
    Opts#{signature_algs => AlgScemes, signature_algs_cert => AlgCertSchemes}.

valid_signature_algs_cert(#{versions := Versions} = Opts, UserOpts, TlsVersion) ->
    case get_opt_list(signature_algs_cert, undefined, UserOpts, Opts) of
        {new, Schemes} ->
            assert_version_dep(signature_algs_cert, Versions, ['tlsv1.2', 'tlsv1.3']),
            SAC0 = handle_signature_algorithms_option(Schemes, TlsVersion),
            option_error(SAC0 =:= [], no_supported_signature_schemes,
                         {signature_algs_cert, Schemes}),
            SAC0;
        {_, Schemes} ->
            Schemes
    end.
valid_signature_algs(AlgCertSchemes0, #{versions := Versions} = Opts, UserOpts, [TlsVersion| _] = TlsVsns) ->
    case get_opt_list(signature_algs, undefined, UserOpts, Opts) of
        {default, undefined}  ->
            %% Smooth upgrade path allow rsa_pkcs1_sha1 for signatures_algs_cert
            %% by default as long as signature_algs is set to default
            DefAlgs0 = tls_v1:default_signature_algs(TlsVsns),
            DefAlgs = handle_hashsigns_option(DefAlgs0, TlsVersion),
            AlgCertSchemes = case AlgCertSchemes0 of
                                 undefined ->
                                     [default | DefAlgs ++ sha_rsa(TlsVersion)];
                                 _ ->
                                     AlgCertSchemes0
                             end,
            {DefAlgs, AlgCertSchemes};
        {new, Algs} ->
            assert_version_dep(signature_algs, Versions, ['tlsv1.2', 'tlsv1.3']),
            SAlgs = handle_hashsigns_option(Algs, TlsVersion),
            option_error(SAlgs =:= [], no_supported_algorithms, {signature_algs, Algs}),
            AlgCertSchemes = case AlgCertSchemes0 of
                                 %% If user sets signature_algs, signature_algs_cert default should
                                 %% be undefined.
                                 [default |_] ->
                                     undefined;
                                 AlgCertSchemes0 ->
                                     AlgCertSchemes0
                             end,
            {SAlgs, AlgCertSchemes};
        {old, Algs} ->
            {Algs, AlgCertSchemes0}
    end.
    
opt_signature_algs_not_valid(UserOpts, #{versions := Versions} = Opts0) ->
    Opts =
        case get_opt_list(signature_algs, undefined, UserOpts, Opts0) of
            {default, undefined} ->
                Opts0#{signature_algs => undefined};
            {old, _} ->
                Opts0;
            _ ->
                option_incompatible([signature_algs, {versions, Versions}])
        end,
    case get_opt_list(signature_algs_cert, undefined, UserOpts, Opts) of
        {default, undefined} ->
            Opts#{signature_algs_cert => undefined};
        {old, _} ->
            Opts;
        _ ->
            option_incompatible([signature_algs_cert, {versions, Versions}])
    end.

sha_rsa(?TLS_1_2) ->
    [{sha, rsa}];
sha_rsa(?TLS_1_3) ->
    [rsa_pkcs1_sha1].

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
    {Where1, BM} = get_opt_of(beast_mitigation, [disabled, one_n_minus_one, zero_n],
                              DefBeast, UserOpts, Opts),
    assert_version_dep(Where1 =:= new, beast_mitigation, Versions, ['tlsv1']),

    {Where2, PC} = get_opt_bool(padding_check, true, UserOpts, Opts),
    assert_version_dep(Where2 =:= new, padding_check, Versions, ['tlsv1']),

    %% Use 'new' we need to check for non default 'one_n_minus_one'
    Opts1 = if
                DefBeast =:= one_n_minus_one, BM =:= disabled ->
                    Opts#{beast_mitigation => BM};
                true ->
                    set_opt_new(new, beast_mitigation, disabled, BM, Opts)
            end,
    set_opt_new(Where2, padding_check, true, PC, Opts1).

opt_server(UserOpts, #{versions := Versions, log_level := LogLevel} = Opts, #{role := server}) ->
    {_, ECC} = get_opt_bool(honor_ecc_order, false, UserOpts, Opts),

    {_, Cipher} = get_opt_bool(honor_cipher_order, false, UserOpts, Opts),

    {Where1, Cookie} = get_opt_bool(cookie, true, UserOpts, Opts),
    assert_version_dep(Where1 =:= new, cookie, Versions, ['tlsv1.3']),

    {Where2, ReNeg} = get_opt_bool(client_renegotiation, true, UserOpts, Opts),
    assert_version_dep(Where2 =:= new, client_renegotiation, Versions,
                       ['tlsv1','tlsv1.1','tlsv1.2']),

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

opt_identity(UserOpts, Opts, _Env) ->
    PSK = handle_psk(UserOpts, Opts),
    SRP = handle_srp(UserOpts, Opts, PSK),
    Lookup = handle_user_lookup(UserOpts, Opts),
    Opts#{psk_identity => PSK, srp_identity => SRP, user_lookup_fun => Lookup}.


handle_psk(UserOpts, #{versions := Versions} = Opts) ->
    case get_opt_list(psk_identity, undefined, UserOpts, Opts) of
        {new, PSK0} ->
            PSK1 = unicode:characters_to_binary(PSK0),
            PSKSize = byte_size(PSK1),
            assert_version_dep(psk_identity, Versions, ['tlsv1','tlsv1.1','tlsv1.2']),
            option_error(not (0 < PSKSize andalso PSKSize < 65536),
                         psk_identity, {psk_identity, PSK0}),
            PSK1;
        {_, PSK0} ->
            PSK0
    end.

handle_srp(UserOpts, #{versions := Versions} = Opts, PSK) ->
    case get_opt(srp_identity, undefined, UserOpts, Opts) of
        {new, {S1, S2}} when is_list(S1), is_list(S2) ->
            User = unicode:characters_to_binary(S1),
            UserSize = byte_size(User),
            assert_version_dep(srp_identity, Versions, ['tlsv1','tlsv1.1','tlsv1.2']),
            option_error(not (0 < UserSize andalso UserSize < 65536),
                         srp_identity, {srp_identity, PSK}),
            {User, unicode:characters_to_binary(S2)};
        {new, Err} ->
            option_error(srp_identity, Err);
        {_, SRP0} ->
            SRP0
    end.

handle_user_lookup(UserOpts, #{versions := Versions} = Opts) ->
    case get_opt(user_lookup_fun, undefined, UserOpts, Opts) of
        {new, {Fun, _} = Lookup} when is_function(Fun, 3) ->
            assert_version_dep(user_lookup_fun, Versions, ['tlsv1','tlsv1.1','tlsv1.2']),
            Lookup;
        {new, Lookup} ->
            option_error(user_lookup_fun, Lookup);
        {_, Lookup} ->
            Lookup
    end.
    

opt_supported_groups(UserOpts, #{versions := TlsVsns} = Opts, _Env) ->
    SG = case get_opt_list(supported_groups,  undefined, UserOpts, Opts) of
             {default, undefined} ->
                 handle_supported_groups_option(ssl:groups(default));
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

default_cb_info(tls_socket) ->
    tls_socket_tcp:cb_info();
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

handle_option_cb_info(Options, tls, {'$socket', _}) ->
    handle_option_cb_info(Options, tls_socket);
handle_option_cb_info(Options, Protocol, _) ->
    handle_option_cb_info(Options, Protocol).

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

validate_server_cert_opts(_Opts, #{validate_certs_or_anon_ciphers := false}) ->
    ok;
validate_server_cert_opts(#{handshake := hello}, _) ->
    %% This verification should be done only when handshake := full, as options
    %% to fulfill the requirement can be supplied at that time.
    ok;
validate_server_cert_opts(#{certs_keys := [_|_]=CertsKeys, ciphers := CPHS, versions := Versions}, _) ->
    validate_certs_or_anon_ciphers(CertsKeys, CPHS, Versions);
validate_server_cert_opts(#{ciphers := CPHS, versions := Versions}, _) ->
    validate_anon_ciphers(CPHS, Versions).

validate_certs_or_anon_ciphers(CertsKeys, Ciphers, Versions) ->
    CheckCertsAndKeys =
        fun(Map) ->
                (maps:is_key(cert, Map) orelse maps:is_key(certfile, Map))
                    andalso (maps:is_key(key, Map) orelse maps:is_key(keyfile, Map))
        end,
    case lists:any(CheckCertsAndKeys, CertsKeys) of
        true -> ok;
        false -> validate_anon_ciphers(Ciphers, Versions)
    end.

validate_anon_ciphers(Ciphers, Versions) ->
    MakeSet = fun(Version, Acc) ->
                      Set = sets:from_list(ssl_cipher:anonymous_suites(Version)),
                      sets:union(Set, Acc)
              end,
    Anonymous = lists:foldl(MakeSet, sets:new(), Versions),
    CiphersSet = sets:from_list(Ciphers, [{version,2}]),
    case sets:is_disjoint(Anonymous, CiphersSet) of
        false -> ok;
        true -> option_error(certs_keys, cert_and_key_required)
    end.

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
    {ok, Inherited} = case Socket of
                          {'$socket', _} ->
                              %% This can't be set on a socket socket,
                              %% so set Inherited the only possibly defaults.
                              {ok, tls_socket:internal_inet_values(tcp)};
                          _ ->
                              tls_socket:getopts(Transport, Socket, EmulatedOptions)
                      end,
    Get = fun(Key) ->
                  {Key, proplists:get_value(Key, Opts, proplists:get_value(Key, Inherited))}
          end,
    {Inet, _} = emulated_options(undefined, undefined, Protocol, Opts),
    Emulated = [Get(Key) || Key <- EmulatedOptions],
    {Inet, Emulated}.


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


%%====================================================================
%% Application options (application environment variables)
%%====================================================================

application_int(EnvVar, Default) ->
      case application:get_env(ssl, EnvVar) of
	{ok, Int} when is_integer(Int) ->
              Int;
          _ ->
              Default
      end.

application_list(EnvVar, Default) ->
      case application:get_env(ssl, EnvVar) of
	{ok, List} when is_list(List) ->
              List;
	_ ->
              Default
      end.

application_atom(EnvVar, Default) ->
    case application:get_env(ssl, EnvVar) of
        {ok, Atom} when is_atom(Atom) ->
            Atom;
	_ ->
            Default
    end.

session_cb_init_args(client) ->
    case application_list(client_session_cb_init_args, undefined) of
        undefined ->
            application_list(session_cb_init_args, []);
        List ->
            List
    end;
session_cb_init_args(server) ->
     case application_list(server_session_cb_init_args, undefined) of
        undefined ->
            application_list(session_cb_init_args, []);
        List ->
            List
    end.

session_lifetime(_Role) ->
    application_int(session_lifetime, ?'24H_in_sec').

max_session_cache_size(client) ->
    application_int(session_cache_client_max, ?DEFAULT_MAX_SESSION_CACHE);
max_session_cache_size(server) ->
    application_int(session_cache_server_max, ?DEFAULT_MAX_SESSION_CACHE).

session_cb_opts(client = Role)->
    case application_atom(session_cb, ssl_client_session_cache_db) of
        ssl_client_session_cache_db = ClientCb ->
            {ClientCb, []};
        ClientCb ->
            {ClientCb, session_cb_init_args(Role)}
    end;
session_cb_opts(server = Role) ->
    case application_atom(session_cb, ssl_server_session_cache_db) of
        ssl_server_session_cache_db = ServerCb ->
            {ServerCb, []};
        ServerCb ->
            {ServerCb, session_cb_init_args(Role)}
    end.

connection_cb(tls) ->
    tls_gen_connection;
connection_cb(dtls) ->
    dtls_gen_connection.

handle_possible_version_change([Version|_], [Version|_] = VersionOpt, OrigSSLOpts, _) ->   
    filter_for_versions(VersionOpt, OrigSSLOpts);
handle_possible_version_change(_, [], OrigSSLOpts, _) ->
    OrigSSLOpts;
handle_possible_version_change(_, VersionsOpt, #{ciphers := Suites} = OrigSSLOpts, Record) ->   
    FallbackSuites = ciphers_for_version(VersionsOpt, Suites, Record),
    filter_for_versions(VersionsOpt, OrigSSLOpts#{ciphers => FallbackSuites}).

filter_for_versions(['tlsv1.3'], OrigSSLOptions) ->
    Opts = ?'PRE_TLS-1_3_ONLY_OPTIONS' ++ ?'TLS-1_0_ONLY_OPTIONS',
    maps:without(Opts, OrigSSLOptions);
filter_for_versions(['tlsv1.3', 'tlsv1.2'| Rest], OrigSSLOptions) ->
    maybe_exclude_tlsv1(Rest, OrigSSLOptions);
filter_for_versions(['tlsv1.2'], OrigSSLOptions) ->
    Opts = ?'TLS-1_3_ONLY_OPTIONS' ++ ?'TLS-1_0_ONLY_OPTIONS',
    maps:without(Opts, OrigSSLOptions);
filter_for_versions(['tlsv1.2' | Rest], OrigSSLOptions) ->
    Opts = ?'TLS-1_3_ONLY_OPTIONS',
    maybe_exclude_tlsv1(Rest, maps:without(Opts, OrigSSLOptions));
filter_for_versions(['tlsv1.1'], OrigSSLOptions) ->
    Opts = ?'TLS-1_3_ONLY_OPTIONS' ++ ?'FROM_TLS-1_2_ONLY_OPTIONS'++ ?'TLS-1_0_ONLY_OPTIONS',
    maps:without(Opts, OrigSSLOptions);
filter_for_versions(['tlsv1.1'| Rest], OrigSSLOptions) ->
    Opts = ?'TLS-1_3_ONLY_OPTIONS' ++ ?'FROM_TLS-1_2_ONLY_OPTIONS',
    maybe_exclude_tlsv1(Rest, maps:without(Opts, OrigSSLOptions));
filter_for_versions(['tlsv1'], OrigSSLOptions) ->
    OrigSSLOptions;
filter_for_versions(['dtlsv1.2'| _], OrigSSLOptions) ->
    OrigSSLOptions; %% dtls1.3 not yet supported
filter_for_versions(['dtlsv1'], OrigSSLOptions) ->
    filter_for_versions(['tlsv1.1'], OrigSSLOptions). %% dtlsv1 is equivialent to tlsv1.1


maybe_exclude_tlsv1(Versions, Options) ->
    case lists:member('tlsv1', Versions) of
        false ->
            Opts = ?'TLS-1_0_ONLY_OPTIONS',
            maps:without(Opts, Options);
        true ->
            Options
    end.

ciphers_for_version([AtomVersion | _], CurrentSuites, Record) ->
    Version = Record:protocol_version_name(AtomVersion),
    Suites = ssl_cipher:all_suites(Version),
    Intersection = sets:intersection(sets:from_list(Suites),
                                     sets:from_list(CurrentSuites)),
    case sets:is_empty(Intersection) of
        true ->
            tls_v1:default_suites(ssl:tls_version(Version));
        false ->
            [Suite || Suite <- CurrentSuites, lists:member(Suite, Suites)]
    end.
%%%--------------------------------------------------------------
%%% Tracing
%%%--------------------------------------------------------------------
handle_trace(csp, {call, {?MODULE, opt_stapling, [UserOpts | _]}}, Stack) ->
    {format_ocsp_params(UserOpts), Stack};
handle_trace(csp, {return_from, {?MODULE, opt_stapling, 3}, Return}, Stack) ->
    {format_ocsp_params(Return), Stack}.

format_ocsp_params(Map) ->
    Stapling = maps:get(stapling, Map, '?'),
    Nonce = maps:get(ocsp_nonce, Map, '?'),
    io_lib:format("Stapling = ~W Nonce = ~W", [Stapling, 5, Nonce, 5]).

