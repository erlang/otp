%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2022. All Rights Reserved.
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

-include("ssl_internal.hrl").
-include("ssl_connection.hrl").
-include_lib("public_key/include/public_key.hrl"). 

-define(DEFAULT_MAX_SESSION_CACHE, 1000).

-export([init/2,
         pre_1_3_session_opts/1,
         get_max_early_data_size/0,
         get_ticket_lifetime/0,
         get_ticket_store_size/0,
         get_internal_active_n/0,
         get_internal_active_n/1
        ]).

%%====================================================================
%% Internal application API
%%====================================================================
init(#{erl_dist := ErlDist,
       dh := DH,
       dhfile := DHFile} = SslOpts, Role) ->
    
    init_manager_name(ErlDist),
    #{pem_cache := PemCache} = Config = init_cacerts(SslOpts, Role),
    DHParams = init_diffie_hellman(PemCache, DH, DHFile, Role),

    CertKeyAlts = init_certs_keys(SslOpts, Role, PemCache),

    {ok, Config#{cert_key_alts => CertKeyAlts, dh_params => DHParams}}.

init_certs_keys(#{certs_keys := CertsKeys}, Role, PemCache) ->
    Pairs = lists:map(fun(CertKey) -> cert_key_pair(CertKey, Role, PemCache) end, CertsKeys),
    CertKeyGroups = group_pairs(Pairs),
    prioritize_groups(CertKeyGroups);
init_certs_keys(SslOpts, Role, PemCache) ->
    KeyPair = init_cert_key_pair(SslOpts, Role, PemCache),
    group_pairs([KeyPair]).

init_cert_key_pair(#{key := Key,
                      keyfile := KeyFile,
                      password :=  Password} = Opts, Role, PemCache) ->
    {ok, Certs} = init_certificates(Opts, PemCache, Role),
    PrivateKey =
	init_private_key(PemCache, Key, KeyFile, Password, Role),
    #{private_key => PrivateKey, certs => Certs}.

cert_key_pair(CertKey, Role, PemCache) ->
    CertKeyPairConf = cert_conf(key_conf(CertKey)),
    init_cert_key_pair(CertKeyPairConf, Role, PemCache).


group_pairs([#{certs := [[]]}]) ->
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
group_pairs([], Group) ->
    Group;
group_pairs([#{private_key := #'ECPrivateKey'{parameters = {namedCurve, ?'id-Ed25519'}}} = Pair | Rest], #{eddsa := EDDSA} = Group) ->
    group_pairs(Rest, Group#{eddsa => [Pair | EDDSA]});
group_pairs([#{private_key := #'ECPrivateKey'{parameters = {namedCurve, ?'id-Ed448'}}} = Pair | Rest], #{eddsa := EDDSA} = Group) ->
    group_pairs(Rest, Group#{eddsa => [Pair | EDDSA]});
group_pairs([#{private_key := #'ECPrivateKey'{}} = Pair | Rest], #{ecdsa := ECDSA} = Group) ->
    group_pairs(Rest, Group#{ecdsa => [Pair | ECDSA]});
group_pairs([#{private_key := {#'RSAPrivateKey'{}, #'RSASSA-PSS-params'{}}} = Pair | Rest], #{rsa_pss_pss := RSAPSS} = Group) ->
    group_pairs(Rest, Group#{rsa_pss_pss => [Pair | RSAPSS]});
group_pairs([#{private_key := #'RSAPrivateKey'{}} = Pair | Rest], #{rsa := RSA} = Group) ->
    group_pairs(Rest, Group#{rsa => [Pair | RSA]});
group_pairs([#{private_key := #'DSAPrivateKey'{}} = Pair | Rest], #{dsa := DSA} = Group) ->
    group_pairs(Rest, Group#{dsa => [Pair | DSA]});
group_pairs([#{private_key := #{algorithm := dss, engine := _}} = Pair | Rest], Group) ->
    Pairs = maps:get(dsa, Group),
    group_pairs(Rest, Group#{dsa => [Pair | Pairs]});
group_pairs([#{private_key := #{algorithm := Alg, engine := _}} = Pair | Rest], Group) ->
    Pairs = maps:get(Alg, Group),
    group_pairs(Rest, Group#{Alg => [Pair | Pairs]}).

prioritize_groups(#{eddsa := EDDSA,
                    ecdsa := ECDSA,
                    rsa_pss_pss := RSAPSS,
                    rsa := RSA,
                    dsa := DSA} = CertKeyGroups) ->
    CertKeyGroups#{eddsa => prio_eddsa(EDDSA),
                   ecdsa => prio_ecdsa(ECDSA),
                   rsa_pss_pss => prio_rsa_pss(RSAPSS),
                   rsa => prio_rsa(RSA),
                   dsa => prio_dsa(DSA)}.

prio_eddsa(EDDSA) ->
    %% Engine not supported yet
    using_curve({namedCurve, ?'id-Ed25519'}, EDDSA, []) ++ using_curve({namedCurve, ?'id-Ed448'}, EDDSA, []).

prio_ecdsa(ECDSA) ->
    EnginePairs = [Pair || Pair = #{private_key := #{engine := _}} <- ECDSA],
    Curves = tls_v1:ecc_curves(all),
    EnginePairs ++ lists:foldr(fun(Curve, AccIn) ->
                                       CurveOid = pubkey_cert_records:namedCurves(Curve),
                                       Pairs = using_curve({namedCurve, CurveOid}, ECDSA -- EnginePairs, []),
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

key_conf(#{key := _} = Conf) ->
    Conf#{certfile => <<>>,
          keyfile => <<>>,
          password => undefined};
key_conf(#{keyfile := _} = Conf) ->
    case maps:get(password, Conf, undefined) of
        undefined ->
            Conf#{key => undefined,
                  password => undefined};
        _ ->
            Conf#{key => undefined}
    end.

cert_conf(#{cert := Bin} = Conf) when is_binary(Bin)->
    Conf#{cert => [Bin]};
cert_conf(#{cert := _} = Conf) ->
    Conf#{certfile => <<>>};
cert_conf(#{certfile := _} = Conf) ->
      Conf#{cert => undefined}.

pre_1_3_session_opts(Role) ->
    {Cb, InitArgs} = session_cb_opts(Role),
    CbOpts = #{session_cb => Cb,
               session_cb_init_args => InitArgs},
    LifeTime = session_lifetime(Role),
    Max = max_session_cache_size(Role),
    CbOpts#{lifetime => LifeTime, max => Max}.

get_ticket_lifetime() ->
    case application:get_env(ssl, server_session_ticket_lifetime) of
	{ok, Seconds} when is_integer(Seconds) andalso
                           Seconds =< 604800 ->  %% MUST be less than 7 days
	    Seconds;
	_  ->
	    7200 %% Default 2 hours
    end.

get_ticket_store_size() ->
    case application:get_env(ssl, server_session_ticket_store_size) of
	{ok, Size} when is_integer(Size) ->
	    Size;
	_  ->
	    1000
    end.

get_max_early_data_size() ->
    case application:get_env(ssl, server_session_ticket_max_early_data) of
	{ok, Size} when is_integer(Size) ->
	    Size;
	_  ->
	    ?DEFAULT_MAX_EARLY_DATA_SIZE
    end.

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
    case application:get_env(ssl, internal_active_n) of
        {ok, N} when is_integer(N) ->
            N;
         _  ->
            ?INTERNAL_ACTIVE_N
    end.

%%====================================================================
%% Internal functions 
%%====================================================================	     
init_manager_name(false) ->
    put(ssl_manager, ssl_manager:name(normal)),
    put(ssl_pem_cache, ssl_pem_cache:name(normal));
init_manager_name(true) ->
    put(ssl_manager, ssl_manager:name(dist)),
    put(ssl_pem_cache, ssl_pem_cache:name(dist)).

init_cacerts(#{cacerts := CaCerts,
               cacertfile := CACertFile,
               crl_cache := CRLCache
              }, Role) ->
    {ok, Config} =
	try 
	    Certs = case CaCerts of
			undefined ->
			    CACertFile;
			_ ->
			    {der, CaCerts}
		    end,
	    {ok,_} = ssl_manager:connection_init(Certs, Role, CRLCache)
	catch
	    _:Reason ->
		file_error(CACertFile, {cacertfile, Reason})
	end,
    Config.

init_certificates(#{certfile := CertFile,
                    cert := OwnCerts}, PemCache, Role) ->
    init_certificates(OwnCerts, PemCache, CertFile, Role).

init_certificates(undefined, _, <<>>, _) ->
    {ok, [[]]};
init_certificates(undefined, PemCache, CertFile, client) ->
    try 
        %% OwnCert | [OwnCert | Chain]
	OwnCerts = ssl_certificate:file_to_certificats(CertFile, PemCache),
	{ok, OwnCerts}
    catch _Error:_Reason  ->
	    {ok, [[]]}
    end; 
init_certificates(undefined, PemCache, CertFile, server) ->
    try
        %% OwnCert | [OwnCert | Chain]
	OwnCerts = ssl_certificate:file_to_certificats(CertFile, PemCache),
	{ok, OwnCerts}
    catch
	_:Reason ->
	    file_error(CertFile, {certfile, Reason})	    
    end;
init_certificates(OwnCerts, _, _, _) when is_binary(OwnCerts)->
    {ok, [OwnCerts]};
init_certificates(OwnCerts, _, _, _) ->
    {ok, OwnCerts}.

init_private_key(_, #{algorithm := Alg} = Key, _, _Password, _Client) when Alg == ecdsa;
                                                                           Alg == rsa;
                                                                           Alg == dss ->
    case maps:is_key(engine, Key) andalso maps:is_key(key_id, Key) of
        true ->
            Key;
        false ->
            throw({key, {invalid_key_id, Key}})
    end;
init_private_key(_, undefined, <<>>, _Password, _Client) ->
    #{};
init_private_key(DbHandle, undefined, KeyFile, Password, _) ->
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
    end;

init_private_key(_,{Asn1Type, PrivateKey},_,_,_) ->
    private_key(init_private_key(Asn1Type, PrivateKey)).

init_private_key(Asn1Type, PrivateKey) ->
    public_key:der_decode(Asn1Type, PrivateKey).

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

init_diffie_hellman(_,Params, _,_) when is_binary(Params)->
    public_key:der_decode('DHParameter', Params);
init_diffie_hellman(_,_,_, client) ->
    undefined;
init_diffie_hellman(_,_,undefined, _) ->
    ?DEFAULT_DIFFIE_HELLMAN_PARAMS;
init_diffie_hellman(DbHandle,_, DHParamFile, server) ->
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


session_cb_init_args(client) ->
    case application:get_env(ssl, client_session_cb_init_args) of
        undefined ->
            case application:get_env(ssl, session_cb_init_args) of
                {ok, Args} when is_list(Args) ->
                    Args;
                _  ->
                    []
            end;
        {ok, Args} ->
            Args
    end;
session_cb_init_args(server) ->
    case application:get_env(ssl, server_session_cb_init_args) of
        undefined ->
            case application:get_env(ssl, session_cb_init_args) of
                {ok, Args} when is_list(Args) ->
                    Args;
                _  ->
                    []
            end;
        {ok, Args} ->
            Args
    end.

session_lifetime(_Role) ->
    case application:get_env(ssl, session_lifetime) of
	{ok, Time} when is_integer(Time) ->
            Time;
        _  ->
            ?'24H_in_sec'
    end.

max_session_cache_size(client) ->
    case application:get_env(ssl, session_cache_client_max) of
	{ok, Size} when is_integer(Size) ->
	    Size;
	_ ->
	   ?DEFAULT_MAX_SESSION_CACHE
    end;
max_session_cache_size(server) ->
    case application:get_env(ssl, session_cache_server_max) of
	{ok, Size} when is_integer(Size) ->
	    Size;
	_ ->
	   ?DEFAULT_MAX_SESSION_CACHE
    end.

session_cb_opts(client = Role)->
    case application:get_env(ssl, session_cb, ssl_client_session_cache_db) of
        ssl_client_session_cache_db = ClientCb ->
            {ClientCb, []};
        ClientCb ->
            {ClientCb, session_cb_init_args(Role)}
    end;
session_cb_opts(server = Role) ->
    case application:get_env(ssl, session_cb, ssl_server_session_cache_db) of
        ssl_server_session_cache_db = ServerCb ->
            {ServerCb, []};
        ServerCb ->
            {ServerCb, session_cb_init_args(Role)}
    end.
