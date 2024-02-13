%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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
-module(public_key_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([
         suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         init_common_per_testcase/1,
         end_per_testcase/2,
         app/0,
         app/1,
         appup/0,
         appup/1,
         dsa_pem/0,
         dsa_pem/1,
         dsa_priv_pkcs8/0,
         dsa_priv_pkcs8/1,
         rsa_pem/0,
         rsa_pem/1,
         rsa_pss_pss_pem/0,
         rsa_pss_pss_pem/1,
         rsa_priv_pkcs8/0,
         rsa_priv_pkcs8/1,
         ec_pem/0,
         ec_pem/1,
         ec_pem2/0,
         ec_pem2/1,
         ec_priv_pkcs8/0,
         ec_priv_pkcs8/1,
         eddsa_priv_pkcs8/0,
         eddsa_priv_pkcs8/1,
         eddsa_priv_rfc5958/0,
         eddsa_priv_rfc5958/1,
         eddsa_sign_verify_24_compat/1,
         init_ec_pem_encode_generated/1,
         ec_pem_encode_generated/0,
         ec_pem_encode_generated/1,
         encrypted_pem_pwdstring/0,
         encrypted_pem_pwdstring/1,
         encrypted_pem_pwdfun/0,
         encrypted_pem_pwdfun/1,
         dh_pem/0,
         dh_pem/1,
         pkcs10_pem/0,
         pkcs10_pem/1,
         pkcs7_pem/0,
         pkcs7_pem/1,
         cert_pem/0,
         cert_pem/1,
         encrypt_decrypt/0,
         encrypt_decrypt/1,
         encrypt_decrypt_sign_fun/0,
         encrypt_decrypt_sign_fun/1,
         rsa_sign_verify/0,
         rsa_sign_verify/1,
         rsa_pss_sign_verify/0,
         rsa_pss_sign_verify/1,
         dsa_sign_verify/0,
         dsa_sign_verify/1,
         custom_sign_fun_verify/0,
         custom_sign_fun_verify/1,
         pkix/0,
         pkix/1,
         pkix_countryname/0,
         pkix_countryname/1,
         pkix_emailaddress/0,
         pkix_emailaddress/1,
         pkix_decode_cert/0,
         pkix_decode_cert/1,
         pkix_path_validation/0,
         pkix_path_validation/1,
         pkix_path_validation_root_expired/0,
         pkix_path_validation_root_expired/1,
         pkix_ext_key_usage/0,
         pkix_ext_key_usage/1,
         pkix_path_validation_bad_date/0,
         pkix_path_validation_bad_date/1,
         pkix_verify_hostname_cn/1,
         pkix_verify_hostname_subjAltName/1,
         pkix_verify_hostname_options/1,
         pkix_verify_hostname_subjAltName_IP/1,
         pkix_dist_point_uri/0,
         pkix_dist_point_uri/1,
         pkix_iso_rsa_oid/0,
         pkix_iso_rsa_oid/1,
         pkix_iso_dsa_oid/0,
         pkix_iso_dsa_oid/1,
         pkix_dsa_sha2_oid/0,
         pkix_dsa_sha2_oid/1,
         pkix_crl/0,
         pkix_crl/1,
         general_name/0,
         general_name/1,
         pkix_hash_type/0,
         pkix_hash_type/1,
         pkix_test_data_all_default/0,
         pkix_test_data_all_default/1,
         pkix_test_data/0,
         pkix_test_data/1,
         pkix_is_issuer/0,
         pkix_is_issuer/1,
         pkix_ocsp_validate/0, pkix_ocsp_validate/1,
         short_cert_issuer_hash/0,
         short_cert_issuer_hash/1,
         short_crl_issuer_hash/0,
         short_crl_issuer_hash/1,
         gen_ec_param_prime_field/0,
         gen_ec_param_prime_field/1,
         gen_ec_param_char_2_field/0,
         gen_ec_param_char_2_field/1,
         cacerts_load/0, cacerts_load/1,
         ocsp_extensions/0, ocsp_extensions/1
        ]).

-export([list_cacerts/0]).  % debug exports


-define(TIMEOUT, 120000). % 2 min
-define(PASSWORD1, "1234abcd").
-define(PASSWORD2, "4567efgh").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() -> 
    [].

all() -> 
    [app, 
     appup,
     {group, pem_decode_encode},
     encrypt_decrypt,
     encrypt_decrypt_sign_fun,
     {group, sign_verify},
     pkix, 
     pkix_countryname, 
     pkix_emailaddress, 
     pkix_decode_cert,
     pkix_path_validation,
     pkix_path_validation_root_expired,
     pkix_ext_key_usage,
     pkix_path_validation_bad_date,
     pkix_iso_rsa_oid, 
     pkix_iso_dsa_oid, 
     pkix_dsa_sha2_oid,
     pkix_crl, 
     pkix_hash_type,
     general_name,
     pkix_verify_hostname_cn,
     pkix_verify_hostname_subjAltName,
     pkix_verify_hostname_subjAltName_IP,
     pkix_verify_hostname_options,
     pkix_dist_point_uri,
     pkix_test_data_all_default,
     pkix_test_data,
     pkix_is_issuer,
     short_cert_issuer_hash, 
     short_crl_issuer_hash,
     cacerts_load,
     ocsp_extensions,
     pkix_ocsp_validate
    ].

groups() -> 
    [{pem_decode_encode, [], [dsa_pem, rsa_pem, rsa_pss_pss_pem, ec_pem,
			      encrypted_pem_pwdstring, encrypted_pem_pwdfun,
			      dh_pem, cert_pem, pkcs7_pem, pkcs10_pem, ec_pem2,
			      rsa_priv_pkcs8, dsa_priv_pkcs8, ec_priv_pkcs8,
			      eddsa_priv_pkcs8, eddsa_priv_rfc5958,
			      ec_pem_encode_generated, gen_ec_param_prime_field,
			      gen_ec_param_char_2_field]},
     {sign_verify, [], [rsa_sign_verify, rsa_pss_sign_verify, dsa_sign_verify,
                        eddsa_sign_verify_24_compat, custom_sign_fun_verify]}
    ].
%%-------------------------------------------------------------------
init_per_suite(Config) ->
    application:stop(crypto),
    try crypto:start() of
        ok ->
            application:start(asn1),
            Config
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    application:stop(asn1),
    application:stop(crypto).

%%-------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.
%%-------------------------------------------------------------------

init_per_testcase(pkix_test_data_all_default, Config) ->
     case crypto:ec_curves() of
         [] ->
             {skip, missing_ecc_support};
         _ ->
             init_common_per_testcase(Config)
     end;

init_per_testcase(gen_ec_param_prime_field=TC, Config) ->
    init_per_testcase_gen_ec_param(TC, secp521r1, Config);

init_per_testcase(gen_ec_param_char_2_field=TC, Config) ->
    init_per_testcase_gen_ec_param(TC, sect571r1, Config);

init_per_testcase(rsa_pss_sign_verify, Config) ->
    Supports = crypto:supports(),
    RSAOpts = proplists:get_value(rsa_opts, Supports),

    case lists:member(rsa_pkcs1_pss_padding, RSAOpts) 
        andalso lists:member(rsa_pss_saltlen, RSAOpts) 
        andalso lists:member(rsa_mgf1_md, RSAOpts) of
        true ->
            Config;
        false ->
            {skip, not_supported_by_crypto}
    end;

init_per_testcase(eddsa_sign_verify_24_compat, Config) ->
    case lists:member(eddsa, crypto:supports(public_keys)) of
        true ->
            Config;
        false ->
            {skip, eddsa_not_supported_by_crypto}
    end;

init_per_testcase(TestCase, Config) ->
    case TestCase of
        ec_pem_encode_generated ->
            init_ec_pem_encode_generated(Config);
	_ -> init_common_per_testcase(Config)
    end.
	
init_common_per_testcase(Config0) ->
    Config = lists:keydelete(watchdog, 1, Config0),
    Dog = ct:timetrap(?TIMEOUT),
    [{watchdog, Dog} | Config].


end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

app() ->
    [{doc, "Test that the public_key app file is ok"}].
app(Config) when is_list(Config) ->
    ok = test_server:app_test(public_key).

%%--------------------------------------------------------------------

appup() ->
    [{doc, "Test that the public_key appup file is ok"}].
appup(Config) when is_list(Config) ->
    ok = test_server:appup_test(public_key).

%%--------------------------------------------------------------------

dsa_pem() ->
    [{doc, "DSA PEM-file decode/encode"}].
dsa_pem(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

     [{'DSAPrivateKey', DerDSAKey, not_encrypted} = Entry0 ] =
	erl_make_certs:pem_to_der(filename:join(Datadir, "dsa.pem")),

    DSAKey = public_key:der_decode('DSAPrivateKey', DerDSAKey),

    DSAKey = public_key:pem_entry_decode(Entry0),

    {ok, DSAPubPem} = file:read_file(filename:join(Datadir, "dsa_pub.pem")),
    [{'SubjectPublicKeyInfo', _, _} = PubEntry0] =
        public_key:pem_decode(DSAPubPem),
    DSAPubKey = public_key:pem_entry_decode(PubEntry0),
    true = check_entry_type(DSAPubKey, 'DSAPublicKey'),
    PubEntry0 = public_key:pem_entry_encode('SubjectPublicKeyInfo', DSAPubKey),
    DSAPubPemNoEndNewLines = strip_superfluous_newlines(DSAPubPem),
    DSAPubPemNoEndNewLines = strip_superfluous_newlines(public_key:pem_encode([PubEntry0])).

dsa_priv_pkcs8() ->
    [{doc, "DSA PKCS8 private key decode/encode"}].
dsa_priv_pkcs8(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    {ok, DsaPem} = file:read_file(filename:join(Datadir, "dsa_key_pkcs8.pem")),
    [{'PrivateKeyInfo', DerDSAKey, not_encrypted} = Entry0 ] = public_key:pem_decode(DsaPem),
    DSAKey = public_key:der_decode('PrivateKeyInfo', DerDSAKey),
    DSAKey = public_key:pem_entry_decode(Entry0),
    true = check_entry_type(DSAKey, 'DSAPrivateKey'),
    PrivEntry0 = public_key:pem_entry_encode('PrivateKeyInfo', DSAKey),
    DSAPemNoEndNewLines = strip_superfluous_newlines(DsaPem),
    DSAPemNoEndNewLines = strip_superfluous_newlines(public_key:pem_encode([PrivEntry0])).

%%--------------------------------------------------------------------

rsa_pem() ->
    [{doc, "RSA PEM-file decode/encode"}].
rsa_pem(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    [{'RSAPrivateKey', DerRSAKey, not_encrypted} =  Entry0 ] =
	erl_make_certs:pem_to_der(filename:join(Datadir, "client_key.pem")),

    RSAKey0 = public_key:der_decode('RSAPrivateKey', DerRSAKey),

    RSAKey0 = public_key:pem_entry_decode(Entry0),
    
    [{'RSAPrivateKey', _, {_,_}} = Entry1] =
	erl_make_certs:pem_to_der(filename:join(Datadir, "rsa.pem")),

    true = check_entry_type(public_key:pem_entry_decode(Entry1, "abcd1234"),
			    'RSAPrivateKey'),

    {ok, RSAPubPem} = file:read_file(filename:join(Datadir, "rsa_pub.pem")),
    [{'SubjectPublicKeyInfo', _, _} = PubEntry0] =
        public_key:pem_decode(RSAPubPem),
    RSAPubKey = public_key:pem_entry_decode(PubEntry0),
    true = check_entry_type(RSAPubKey, 'RSAPublicKey'),
    PubEntry0 = public_key:pem_entry_encode('SubjectPublicKeyInfo', RSAPubKey),
    RSAPubPemNoEndNewLines = strip_superfluous_newlines(RSAPubPem),
    RSAPubPemNoEndNewLines = strip_superfluous_newlines(public_key:pem_encode([PubEntry0])),

    {ok, RSARawPem} = file:read_file(filename:join(Datadir, "rsa_pub_key.pem")),
    [{'RSAPublicKey', _, _} = PubEntry1] =
        public_key:pem_decode(RSARawPem),
    RSAPubKey = public_key:pem_entry_decode(PubEntry1),
    RSARawPemNoEndNewLines = strip_superfluous_newlines(RSARawPem),
    RSARawPemNoEndNewLines = strip_superfluous_newlines(public_key:pem_encode([PubEntry1])).

rsa_pss_pss_pem() ->
    [{doc, "RSA PKCS8 RSASSA-PSS private key decode/encode"}].
rsa_pss_pss_pem(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    {ok, RsaPem} = file:read_file(filename:join(Datadir, "rsa_pss_pss_key.pem")),
    [{'PrivateKeyInfo', DerRSAKey, not_encrypted} = Entry0 ] = public_key:pem_decode(RsaPem),
    {RSAKey, Parms} = public_key:der_decode('PrivateKeyInfo', DerRSAKey),
    {RSAKey, Parms} = public_key:pem_entry_decode(Entry0),
    true = check_entry_type(RSAKey, 'RSAPrivateKey'),
    PrivEntry0 = public_key:pem_entry_encode('PrivateKeyInfo', {RSAKey, Parms}),
    RSAPemNoEndNewLines = strip_superfluous_newlines(RsaPem),
    RSAPemNoEndNewLines = strip_superfluous_newlines(public_key:pem_encode([PrivEntry0])).

rsa_priv_pkcs8() ->
    [{doc, "RSA PKCS8 private key decode/encode"}].
rsa_priv_pkcs8(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    {ok, RsaPem} = file:read_file(filename:join(Datadir, "rsa_key_pkcs8.pem")),
    [{'PrivateKeyInfo', DerRSAKey, not_encrypted} = Entry0 ] = public_key:pem_decode(RsaPem),
    RSAKey = public_key:der_decode('PrivateKeyInfo', DerRSAKey),
    RSAKey = public_key:pem_entry_decode(Entry0),
    true = check_entry_type(RSAKey, 'RSAPrivateKey'),
    PrivEntry0 = public_key:pem_entry_encode('PrivateKeyInfo', RSAKey),
    RSAPemNoEndNewLines = strip_superfluous_newlines(RsaPem),
    RSAPemNoEndNewLines = strip_superfluous_newlines(public_key:pem_encode([PrivEntry0])).

%%--------------------------------------------------------------------

ec_pem() ->
    [{doc, "EC key PEM-file decode/encode"}].
ec_pem(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    {ok, ECPubPem} = file:read_file(filename:join(Datadir, "ec_pubkey.pem")),
    [{'SubjectPublicKeyInfo', _, _} = PubEntry0] =
        public_key:pem_decode(ECPubPem),
    ECPubKey = public_key:pem_entry_decode(PubEntry0),
    true = check_entry_type(ECPubKey, 'ECPoint'),
    PubEntry0 = public_key:pem_entry_encode('SubjectPublicKeyInfo', ECPubKey),
    ECPubPemNoEndNewLines = strip_superfluous_newlines(ECPubPem),
    ECPubPemNoEndNewLines = strip_superfluous_newlines(public_key:pem_encode([PubEntry0])),
    
    {ok, ECPrivPem} = file:read_file(filename:join(Datadir, "ec_key.pem")),
    [{'EcpkParameters', _, not_encrypted} = Entry1,
     {'ECPrivateKey', _, not_encrypted} = Entry2] = public_key:pem_decode(ECPrivPem),
    
    ECParams = public_key:pem_entry_decode(Entry1),
    true = check_entry_type(ECParams, 'EcpkParameters'),
    ECPrivKey = public_key:pem_entry_decode(Entry2),
    true = check_entry_type(ECPrivKey, 'ECPrivateKey'),
    true = check_entry_type(ECPrivKey#'ECPrivateKey'.parameters, 'EcpkParameters'),
    ECPemNoEndNewLines = strip_superfluous_newlines(ECPrivPem),
    ECPemNoEndNewLines = strip_superfluous_newlines(public_key:pem_encode([Entry1, Entry2])).
    
ec_pem2() ->
    [{doc, "EC key w/explicit params PEM-file decode/encode"}].
ec_pem2(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    %% Load key with explicit curve parameters.  Generated with...
    %% openssl ecparam -name secp521r1 -genkey -param_enc explicit -out ec_key2.pem
    {ok, ECPrivPem} = file:read_file(filename:join(Datadir, "ec_key2.pem")),
    [{'EcpkParameters', _, not_encrypted} = Entry1,
     {'ECPrivateKey', _, not_encrypted} = Entry2] = public_key:pem_decode(ECPrivPem),

    ECParams = public_key:pem_entry_decode(Entry1),
    true = check_entry_type(ECParams, 'EcpkParameters'),
    ECPrivKey = public_key:pem_entry_decode(Entry2),
    true = check_entry_type(ECPrivKey, 'ECPrivateKey'),
    true = check_entry_type(ECPrivKey#'ECPrivateKey'.parameters, 'EcpkParameters'),
    ECPemNoEndNewLines = strip_superfluous_newlines(ECPrivPem),
    ECPemNoEndNewLines = strip_superfluous_newlines(public_key:pem_encode([Entry1, Entry2])).

ec_priv_pkcs8() ->
    [{doc, "EC PKCS8 private key decode/encode"}].
ec_priv_pkcs8(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    {ok, ECPrivPem} = file:read_file(filename:join(Datadir, "ec_key_pkcs8.pem")),
    [{'PrivateKeyInfo', _, not_encrypted} = PKCS8Key] = public_key:pem_decode(ECPrivPem),
    ECPrivKey = public_key:pem_entry_decode(PKCS8Key),
    true = check_entry_type(ECPrivKey, 'ECPrivateKey'),
    true = check_entry_type(ECPrivKey#'ECPrivateKey'.parameters, 'EcpkParameters'),
    PrivEntry0 = public_key:pem_entry_encode('PrivateKeyInfo', ECPrivKey),
    ECPemNoEndNewLines = strip_superfluous_newlines(ECPrivPem),
    ECPemNoEndNewLines = strip_superfluous_newlines(public_key:pem_encode([PrivEntry0])).

eddsa_priv_pkcs8() ->
    [{doc, "EDDSA PKCS8 private key decode/encode"}].
eddsa_priv_pkcs8(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    {ok, ECPrivPem} = file:read_file(filename:join(Datadir, "eddsa_key_pkcs8.pem")),
    [{'PrivateKeyInfo', _, not_encrypted} = PKCS8Key] = public_key:pem_decode(ECPrivPem),
    ECPrivKey = public_key:pem_entry_decode(PKCS8Key),
    true = check_entry_type(ECPrivKey, 'ECPrivateKey'),
    true = ECPrivKey#'ECPrivateKey'.parameters == {namedCurve, ?'id-Ed25519'},
    true = size(ECPrivKey#'ECPrivateKey'.privateKey) == 32,
    PrivEntry0 = public_key:pem_entry_encode('PrivateKeyInfo', ECPrivKey),
    ECPemNoEndNewLines = strip_superfluous_newlines(ECPrivPem),
    ECPemNoEndNewLines = strip_superfluous_newlines(public_key:pem_encode([PrivEntry0])).

eddsa_priv_rfc5958() ->
    [{doc, "EDDSA PKCS8 private key decode/encode"}].
eddsa_priv_rfc5958(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    {ok, ECPrivPem} = file:read_file(filename:join(Datadir, "eddsa_key_rfc5958.pem")),
    [{'PrivateKeyInfo', _, not_encrypted} = PKCS8Key] = public_key:pem_decode(ECPrivPem),
    ECPrivKey = public_key:pem_entry_decode(PKCS8Key),
    true = check_entry_type(ECPrivKey, 'ECPrivateKey'),
    true = ECPrivKey#'ECPrivateKey'.parameters == {namedCurve, ?'id-Ed25519'},
    true = size(ECPrivKey#'ECPrivateKey'.privateKey) == 32,
    PrivEntry0 = public_key:pem_entry_encode('OneAsymmetricKey', ECPrivKey),
    ECPemNoEndNewLines = strip_superfluous_newlines(ECPrivPem),
    ECPemNoEndNewLines = strip_superfluous_newlines(public_key:pem_encode([PrivEntry0])).

eddsa_sign_verify_24_compat(_Config) ->
    Key =
        {'ECPrivateKey',1,
         <<15,192,10,239,169,93,9,105,143,13,221,71,191,255,201,
           60,8,80,43,234,82,68,151,219,233,144,174,41,227,241,
           229,232>>,
         {namedCurve,{1,3,101,112}},
         <<209,208,142,135,125,251,57,203,2,49,232,74,238,214,170,
           181,23,107,221,39,187,225,106,19,34,133,117,198,138,
           180,16,70>>,
         asn1_NOVALUE},
    Body =
        <<83,83,72,83,73,71,0,0,0,4,116,101,120,116,0,0,0,0,0,0,0,6,115,104,97,
          53,49,50,0,0,0,64,119,199,206,154,93,134,187,56,109,68,59,185,99,144,
          250,161,32,99,49,88,105,156,136,68,195,11,19,171,11,249,39,96,183,228,
          65,106,234,57,125,185,27,74,192,229,221,86,184,239,126,75,6,97,98,171,
          31,220,8,131,25,206,109,239,200,118>>,
    ExpectedSignature =
        <<203,148,171,54,165,4,216,251,189,124,35,227,88,183,187,225,142,10,132,163,98,
          48,167,195,67,12,49,148,85,146,41,14,58,0,198,68,103,114,90,61,31,38,200,198,
          64,179,135,138,31,172,236,105,0,71,50,195,168,247,216,110,210,61,159,5>>,
    lists:foreach(
      fun(Sha) ->
              ct:log("Try Sha = ~p", [Sha]),
              try
                  case public_key:sign(Body, Sha, Key) of
                      ExpectedSignature ->
                          ct:log("sign Sha ~p ok", [Sha]),
                          ExpectedSignature;
                      Others ->
                          ct:log("Sha: ~p~nGot:    ~p~nExpect: ~p", [Sha,Others,ExpectedSignature]),
                          ct:fail("Bad sign result")
                  end
              of
                  Sig ->
                      try
                          case public_key:verify(Body, Sha, Sig, Key) of
                              true ->
                                  ct:log("verify Sha ~p ok", [Sha]);
                              false ->
                                  ct:fail("Bad verify result for ~p",[Sha])
                          end
                      catch
                          C:E ->
                              ct:log("Verify: ~p:~p  Sha = ~p", [C,E,Sha]),
                              ct:fail("Bad verify",[])
                      end
              catch
                  C:E ->
                      ct:log("Sign: ~p:~p  Sha = ~p", [C,E,Sha]),
                      ct:fail("Bad sign",[])
              end
      end, [undefined, none | crypto:supports(hashs)]).


init_ec_pem_encode_generated(Config) ->
    case catch true = lists:member('secp384r1', crypto:ec_curves()) of
        {'EXIT', _} -> {skip, {'secp384r1', not_supported}};
        _           -> init_common_per_testcase(Config)
    end.

ec_pem_encode_generated() ->
    [{doc, "PEM-encode generated EC key"}].
ec_pem_encode_generated(_Config) ->

    Key1 = public_key:generate_key({namedCurve, 'secp384r1'}),
    public_key:pem_entry_encode('ECPrivateKey', Key1),

    Key2 = public_key:generate_key({namedCurve, ?'secp384r1'}),
    public_key:pem_entry_encode('ECPrivateKey', Key2).


%%--------------------------------------------------------------------

encrypted_pem_pwdstring() ->
    [{doc, "Encrypted PEM-file decode/encode with password string used"}].
encrypted_pem_pwdstring(Config) when is_list(Config) ->
    encrypted_pem(Config, ?PASSWORD1, ?PASSWORD2).

encrypted_pem_pwdfun() ->
    [{doc, "Encrypted PEM-file decode/encode with password fun used"}].
encrypted_pem_pwdfun(Config) when is_list(Config) ->
    encrypted_pem(Config, fun() -> ?PASSWORD1 end, fun() -> ?PASSWORD2 end).

encrypted_pem(Config, Password1, Password2) ->
    Datadir = proplists:get_value(data_dir, Config),
    [{'RSAPrivateKey', DerRSAKey, not_encrypted}] =
        erl_make_certs:pem_to_der(filename:join(Datadir, "client_key.pem")),
    RSAKey = public_key:der_decode('RSAPrivateKey', DerRSAKey),
    SupportedCiphers = crypto:supports(ciphers),
    SupportedECB = lists:member(des_ecb, SupportedCiphers),
    SupportedDES = lists:member(des_cbc, SupportedCiphers),
    case SupportedECB of
        true ->
            encrypted_pem_des_ede(Datadir, RSAKey, Password1);
        false ->
            ct:comment("DES-EDE3-CBC not supported")
    end,
    case SupportedDES of
        true ->
            encrypted_pem_des_cbc(Datadir, RSAKey, Password2);
        false ->
            ct:comment("DES-CBC not supported")
    end.
encrypted_pem_des_ede(Datadir, RSAKey, Password) ->
    Salt = crypto:strong_rand_bytes(8),
    Entry = public_key:pem_entry_encode('RSAPrivateKey', RSAKey,
                                        {{"DES-EDE3-CBC", Salt}, ?PASSWORD1}),
    RSAKey = public_key:pem_entry_decode(Entry, Password),
    Des3KeyFile = filename:join(Datadir, "des3_client_key.pem"),
    erl_make_certs:der_to_pem(Des3KeyFile, [Entry]),
    [{'RSAPrivateKey', _, {"DES-EDE3-CBC", Salt}}] =
        erl_make_certs:pem_to_der(Des3KeyFile).

encrypted_pem_des_cbc(Datadir, RSAKey, Password) ->
    Salt = crypto:strong_rand_bytes(8),
    Entry = public_key:pem_entry_encode('RSAPrivateKey', RSAKey,
                                        {{"DES-CBC", Salt}, ?PASSWORD2}),
    DesKeyFile = filename:join(Datadir, "des_client_key.pem"),
    erl_make_certs:der_to_pem(DesKeyFile, [Entry]),
    [{'RSAPrivateKey', _, {"DES-CBC", Salt}} = Entry] =
        erl_make_certs:pem_to_der(DesKeyFile),
    {ok, Pem} = file:read_file(DesKeyFile),
    check_encapsulated_header(Pem),
    true = check_entry_type(public_key:pem_entry_decode(Entry, Password),
                            'RSAPrivateKey').

%%--------------------------------------------------------------------

dh_pem() ->
    [{doc, "DH parametrs PEM-file decode/encode"}].
dh_pem(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    [{'DHParameter', _DerDH, not_encrypted} = Entry] =
	erl_make_certs:pem_to_der(filename:join(Datadir, "dh.pem")),
    asn1_encode_decode(Entry).

%%--------------------------------------------------------------------

pkcs10_pem() ->
   [{doc, "PKCS-10 PEM-file decode/encode"}].
pkcs10_pem(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    [{'CertificationRequest', _DerPKCS10, not_encrypted} = Entry] =
	erl_make_certs:pem_to_der(filename:join(Datadir, "req.pem")),
    asn1_encode_decode(Entry).
%%--------------------------------------------------------------------
pkcs7_pem() ->
    [{doc, "PKCS-7 PEM-file decode/encode"}].
pkcs7_pem(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    [{'ContentInfo', _, not_encrypted} = Entry0] =
	erl_make_certs:pem_to_der(filename:join(Datadir, "pkcs7_cert.pem")),
    [{'ContentInfo', _, not_encrypted} = Entry1] =
	erl_make_certs:pem_to_der(filename:join(Datadir, "pkcs7_ext.pem")),
    asn1_encode_decode(Entry0),
    asn1_encode_decode(Entry1).
      
%%--------------------------------------------------------------------
cert_pem() ->
    [{doc, "Certificate PEM-file decode/encode"}].
cert_pem(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
   
    [{'Certificate', _, not_encrypted} = Entry0] =  
	erl_make_certs:pem_to_der(filename:join(Datadir, "client_cert.pem")),
    
    asn1_encode_decode(Entry0),
    
    [{'Certificate', _, not_encrypted} = Entry1, 
     {'Certificate', _, not_encrypted} = Entry2] = 
        erl_make_certs:pem_to_der(filename:join(Datadir, "cacerts.pem")),
    
    asn1_encode_decode(Entry1),
    asn1_encode_decode(Entry2).

%%--------------------------------------------------------------------
encrypt_decrypt() ->
    [{doc, "Test public_key:encrypt_private and public_key:decrypt_public"}].
encrypt_decrypt(Config) when is_list(Config) -> 
    {PrivateKey, _DerKey} = erl_make_certs:gen_rsa(64),
    #'RSAPrivateKey'{modulus=Mod, publicExponent=Exp} = PrivateKey,
    PublicKey = #'RSAPublicKey'{modulus=Mod, publicExponent=Exp},
    Msg = list_to_binary(lists:duplicate(5, "Foo bar 100")),
    RsaEncrypted = public_key:encrypt_private(Msg, PrivateKey),
    Msg = public_key:decrypt_public(RsaEncrypted, PublicKey),
    RsaEncrypted2 = public_key:encrypt_public(Msg, PublicKey),
    Msg = public_key:decrypt_private(RsaEncrypted2, PrivateKey),
    ok.

%%--------------------------------------------------------------------
encrypt_decrypt_sign_fun() ->
    [{doc, "Test public_key:encrypt_private with user provided sign_fun"}].
encrypt_decrypt_sign_fun(Config) when is_list(Config) ->
    {PrivateKey, _DerKey} = erl_make_certs:gen_rsa(64),
    #'RSAPrivateKey'{modulus=Mod, publicExponent=Exp} = PrivateKey,
    EncryptFun = fun (PlainText, Options) ->
            public_key:encrypt_private(PlainText, PrivateKey, Options)
        end,
    CustomPrivKey = #{encrypt_fun => EncryptFun},
    PublicKey = #'RSAPublicKey'{modulus=Mod, publicExponent=Exp},
    Msg = list_to_binary(lists:duplicate(5, "Foo bar 100")),
    RsaEncrypted = public_key:encrypt_private(Msg, CustomPrivKey),
    Msg = public_key:decrypt_public(RsaEncrypted, PublicKey),
    ok.
       
%%--------------------------------------------------------------------
rsa_sign_verify() ->
    [{doc, "Checks that we can sign and verify rsa signatures."}].
rsa_sign_verify(Config) when is_list(Config) ->
    Ca = {_, CaKey} = erl_make_certs:make_cert([]),
    {Cert1, _} = erl_make_certs:make_cert([{key, dsa}, {issuer, Ca}]),
    PrivateRSA = #'RSAPrivateKey'{modulus=Mod, publicExponent=Exp} = 
	public_key:pem_entry_decode(CaKey),
    PublicRSA = #'RSAPublicKey'{modulus=Mod, publicExponent=Exp},
    true = public_key:pkix_verify(Cert1, PublicRSA),

    Msg = list_to_binary(lists:duplicate(5, "Foo bar 100")),
    RSASign = public_key:sign(Msg, sha, PrivateRSA),
    true = public_key:verify(Msg, sha, RSASign, PublicRSA), 
    false = public_key:verify(<<1:8, Msg/binary>>, sha, RSASign, PublicRSA), 
    false = public_key:verify(Msg, sha, <<1:8, RSASign/binary>>, PublicRSA), 

    RSASign1 = public_key:sign(Msg, md5, PrivateRSA),
    true = public_key:verify(Msg, md5, RSASign1, PublicRSA).
    
%%--------------------------------------------------------------------
rsa_pss_sign_verify() ->
    [{doc, "Checks that we can sign and verify rsa pss signatures."}].
rsa_pss_sign_verify(Config) when is_list(Config) ->
    CertChainConf  = #{server_chain => 
                           #{root => [],
                             intermediates => [],
                             peer => []},
                       client_chain => 
                           #{root => [{key, {hardcode_rsa_key(1), pss_params(sha256)}}],
                             intermediates => [],
                             peer => []}},
    #{client_config := ClientConf} = public_key:pkix_test_data(CertChainConf),
    Cert = proplists:get_value(cert, ClientConf),
    {#'RSAPrivateKey'{modulus=Mod, publicExponent=Exp}, Parms} = {hardcode_rsa_key(1), pss_params(sha256)},
           
    true = public_key:pkix_verify(Cert, {#'RSAPublicKey'{modulus=Mod, publicExponent=Exp}, Parms}).
    
%%--------------------------------------------------------------------

dsa_sign_verify() ->
    [{doc, "Checks that we can sign and verify dsa signatures."}].
dsa_sign_verify(Config) when is_list(Config) ->
    Ca = erl_make_certs:make_cert([]),
    CertInfo = {_,CertKey1} = erl_make_certs:make_cert([{key, dsa}, {issuer, Ca}]),
    {Cert2,_CertKey} = erl_make_certs:make_cert([{issuer, CertInfo}]),

    #'DSAPrivateKey'{p=P, q=Q, g=G, y=Y, x=_X} =
	public_key:pem_entry_decode(CertKey1),
    true = public_key:pkix_verify(Cert2, {Y, #'Dss-Parms'{p=P, q=Q, g=G}}),

    Datadir = proplists:get_value(data_dir, Config),
    [DsaKey = {'DSAPrivateKey', _, _}] = 
	erl_make_certs:pem_to_der(filename:join(Datadir, "dsa.pem")), 
    DSAPrivateKey = public_key:pem_entry_decode(DsaKey),
    #'DSAPrivateKey'{p=P1, q=Q1, g=G1, y=Y1, x=_X1} = DSAPrivateKey,

    Msg = list_to_binary(lists:duplicate(5, "Foo bar 100")),
    DSASign = public_key:sign(Msg, sha, DSAPrivateKey),
    DSAPublicKey = Y1,
    DSAParams = #'Dss-Parms'{p=P1, q=Q1, g=G1},
    true = public_key:verify(Msg, sha, DSASign, {DSAPublicKey, DSAParams}), 
    false = public_key:verify(<<1:8, Msg/binary>>, sha, DSASign, 
			      {DSAPublicKey, DSAParams}), 
    false = public_key:verify(Msg, sha, <<1:8, DSASign/binary>>, 
			      {DSAPublicKey, DSAParams}), 
    
    Digest = crypto:hash(sha,Msg),
    DigestSign = public_key:sign(Digest, none, DSAPrivateKey),
    true = public_key:verify(Digest, none, DigestSign, {DSAPublicKey, DSAParams}), 
    <<_:8, RestDigest/binary>> = Digest,
    false = public_key:verify(<<1:8, RestDigest/binary>>, none, DigestSign, 
			      {DSAPublicKey, DSAParams}), 
    false = public_key:verify(Digest, none, <<1:8, DigestSign/binary>>, 
			      {DSAPublicKey, DSAParams}).
%%--------------------------------------------------------------------

custom_sign_fun_verify() ->
    [{doc, "Checks that public_key:sign correctly calls the `sign_fun`"}].
custom_sign_fun_verify(Config) when is_list(Config) ->
    {_, CaKey} = erl_make_certs:make_cert([{key, rsa}]),
    PrivateRSA = public_key:pem_entry_decode(CaKey),
    #'RSAPrivateKey'{modulus=Mod, publicExponent=Exp} = PrivateRSA,
    PublicRSA = #'RSAPublicKey'{modulus=Mod, publicExponent=Exp},
    SignFun = fun (Msg, HashAlgo, Options) ->
            public_key:sign(Msg, HashAlgo, PrivateRSA, Options)
        end,
    CustomKey = #{algorithm => rsa, sign_fun => SignFun},

    Msg = list_to_binary(lists:duplicate(5, "Foo bar 100")),
    RSASign = public_key:sign(Msg, sha, CustomKey),
    true = public_key:verify(Msg, sha, RSASign, PublicRSA),
    false = public_key:verify(<<1:8, Msg/binary>>, sha, RSASign, PublicRSA),
    false = public_key:verify(Msg, sha, <<1:8, RSASign/binary>>, PublicRSA),

    RSASign1 = public_key:sign(Msg, md5, CustomKey),
    true = public_key:verify(Msg, md5, RSASign1, PublicRSA).

%%--------------------------------------------------------------------
pkix() ->
    [{doc, "Misc pkix tests not covered elsewhere"}].
pkix(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    Certs0 = erl_make_certs:pem_to_der(filename:join(Datadir, "cacerts.pem")),
    Certs1 = erl_make_certs:pem_to_der(filename:join(Datadir, "client_cert.pem")),
    TestTransform = fun({'Certificate', CertDer, not_encrypted}) ->
			    PlainCert = public_key:pkix_decode_cert(CertDer, plain),
			    OtpCert = public_key:pkix_decode_cert(CertDer, otp),
			    CertDer = 
				public_key:pkix_encode('OTPCertificate', OtpCert, otp),
			    CertDer = 
				public_key:pkix_encode('Certificate', PlainCert, plain),
			    OTPTBS = OtpCert#'OTPCertificate'.tbsCertificate,
			    OTPSubj = OTPTBS#'OTPTBSCertificate'.subject, 
			    DNEncoded = public_key:pkix_encode('Name', OTPSubj, otp),
			    PlainTBS = PlainCert#'Certificate'.tbsCertificate,
			    Subj2 = PlainTBS#'TBSCertificate'.subject,
			    DNEncoded = public_key:pkix_encode('Name', Subj2, plain),

			    false = public_key:pkix_is_fixed_dh_cert(CertDer)
		    end,
    [TestTransform(Cert) || Cert <- Certs0 ++ Certs1],

    Root = element(2, hd(Certs0)),
    Peer = element(2, hd(Certs1)), 

    true = public_key:pkix_is_self_signed(Root),
    false = public_key:pkix_is_self_signed(Peer),

    CaIds = [element(2, public_key:pkix_issuer_id(Cert, self)) || 
		{'Certificate', Cert, _} <- Certs0],
    {ok, IssuerId} = 
	public_key:pkix_issuer_id(Peer, other),
    
    {ok, Id} = public_key:pkix_issuer_id(Root, self),
    Id = public_key:pkix_subject_id(Root),

    true = lists:member(IssuerId, CaIds),

    %% Should be normalized already
    TestStr   = {rdnSequence, 
		 [[{'AttributeTypeAndValue', {2,5,4,3},{printableString,"ERLANGCA"}}],
		  [{'AttributeTypeAndValue', {2,5,4,3},{printableString," erlang  ca "}}]]},
    VerifyStr = {rdnSequence, 
		 [[{'AttributeTypeAndValue', {2,5,4,3},{printableString,"erlangca"}}],
		  [{'AttributeTypeAndValue', {2,5,4,3},{printableString,"erlang ca"}}]]},   
    VerifyStr = public_key:pkix_normalize_name(TestStr).
    
  
%%--------------------------------------------------------------------
pkix_countryname() ->
    [{doc, "Test workaround for certs that code x509countryname as utf8"}].
pkix_countryname(Config) when is_list(Config) ->
    Cert = incorrect_countryname_pkix_cert(),
    OTPCert = public_key:pkix_decode_cert(Cert, otp),
    TBSCert = OTPCert#'OTPCertificate'.tbsCertificate,
    Issuer = TBSCert#'OTPTBSCertificate'.issuer,
    Subj   = TBSCert#'OTPTBSCertificate'.subject,
    check_countryname(Issuer),
    check_countryname(Subj).

%%--------------------------------------------------------------------
pkix_emailaddress() ->
    [{doc, "Test workaround for certs that code emailAddress as utf8"}].
pkix_emailaddress(Config) when is_list(Config) ->
    Cert = incorrect_emailaddress_pkix_cert(),
    OTPCert = public_key:pkix_decode_cert(Cert, otp),
    TBSCert = OTPCert#'OTPCertificate'.tbsCertificate,
    Issuer = TBSCert#'OTPTBSCertificate'.issuer,
    Subj   = TBSCert#'OTPTBSCertificate'.subject,
    check_emailaddress(Issuer),
    check_emailaddress(Subj).


%%--------------------------------------------------------------------
pkix_decode_cert() ->
    [{doc, "Test that extension IssuerDistributionPoint is not decoded in 'otp' decoding mode. We want to leave it for later "
      "to increase interopability for sites that does not use this extension and will not care if it is properly encoded"}].
pkix_decode_cert(Config) when is_list(Config) ->
    Der = base64:decode(
            <<"MIICXDCCAgKgAwIBAgIBATAKBggqhkjOPQQDAjApMRkwFwYDVQQFExBjOTY4NDI4OTMyNzUwOGRiMQwwCgYDVQQMDANURUUwHhcNMjIxMDI5MTczMTA3WhcNMjkwNDE2MjAzNDUzWjAfMR0wGwYDVQQDExRBbmRyb2lkIEtleXN0b3JlIEtleTBZMBMGByqGSM49AgEGCCqGSM49AwEHA0IABFmIQDus/jIZ0cPnRCITCzUUuCjQBw8MetO6154mmTL8O/fFlGgYkZ6C8jSSntKC/lMwaZHxAgW1AGgoCrPuX5ejggEjMIIBHzALBgNVHQ8EBAMCB4AwCAYDVR0fBAEAMIIBBAYKKwYBBAHWeQIBEQSB9TCB8gIBAgoBAQIBAwoBAQQgyvsSa116xqleaXs6xA84wqpAPWFgaaTjCWBnZpHslmoEADBEv4VFQAQ+MDwxFjAUBAxjb20ud2hhdHNhcHACBA0+oAQxIgQgOYfQQ9EK769ahxCzZxQY/lfg4ZtlPJ34JVj+tf/OXUQweqEFMQMCAQKiAwIBA6MEAgIBAKUIMQYCAQYCAQSqAwIBAb+DdwIFAL+FPQgCBgGEJMweob+FPgMCAQC/hUAqMCgEIFNB5rJkaXmnDldlMAeh8xAWlCHsm92fGlZI91reAFrxAQH/CgEAv4VBBQIDAV+Qv4VCBQIDAxUYMAoGCCqGSM49BAMCA0gAMEUCIF0BwvRQipVoaz5SIhsYbIeK+FHbAjWPgOxWgQ6Juq64AiEA83ZLsK37DjZ/tZNRi271VHQqIU8mdqUIMboVUiy3DaM=">>),

    #'OTPCertificate'{} = public_key:pkix_decode_cert(Der, otp).

%%--------------------------------------------------------------------
pkix_path_validation() ->
    [{doc, "Test PKIX path validation"}].
pkix_path_validation(Config) when is_list(Config) ->
    CaK = {Trusted,_} = 
	erl_make_certs:make_cert([{key, dsa},
			     {subject, [
					{name, "Public Key"},
					{?'id-at-name', {printableString, "public_key"}},
					{?'id-at-pseudonym', {printableString, "pubkey"}},
					{city, "Stockholm"},
					{country, "SE"},
					{org, "erlang"},
					{org_unit, "testing dep"}
				       ]}
			    ]),
    ok = erl_make_certs:write_pem("./", "public_key_cacert", CaK),

    CertK1 = {Cert1, _} = erl_make_certs:make_cert([{issuer, CaK}]),
    CertK2 = {Cert2,_} = erl_make_certs:make_cert([{issuer, CertK1}, 
					      {digest, md5}, {extensions, false}]),
    ok = erl_make_certs:write_pem("./", "public_key_cert", CertK2),
    
    {ok, _} = public_key:pkix_path_validation(Trusted, [Cert1], []),
    
    {error, {bad_cert,invalid_issuer}} = 
	public_key:pkix_path_validation(Trusted, [Cert2], []),
   
    {ok, _} = public_key:pkix_path_validation(Trusted, [Cert1, Cert2], []),    

    {error, {bad_cert, duplicate_cert_in_path}} =
	public_key:pkix_path_validation(Trusted, [Cert1, Cert1, Cert2], []),

    {error, issuer_not_found} = public_key:pkix_issuer_id(Cert2, other),

    CertK3 = {Cert3,_}  = erl_make_certs:make_cert([{issuer, CertK1}, 
					       {extensions, [{basic_constraints, false}]}]),
    {Cert4,_}  = erl_make_certs:make_cert([{issuer, CertK3}, {extensions, [{key_usage, undefined}]}]),

    {error, {bad_cert,missing_basic_constraint}} =
	public_key:pkix_path_validation(Trusted, [Cert1, Cert3,Cert4], []),

    VerifyFunAndState0  = {fun(_,{bad_cert, missing_basic_constraint}, UserState) ->
				   {valid, UserState};
			      (_,{bad_cert, _} = Reason, _) ->
				   {fail, Reason};
			      (_,{extension, _}, UserState) ->
				   {unknown, UserState};
			      (_, valid, UserState) ->
				   {valid, UserState};
			      (_, valid_peer, UserState) ->
				   {valid, UserState}
			   end, []},
    {ok, _} =
	public_key:pkix_path_validation(Trusted, [Cert1, Cert3,Cert4],
					[{verify_fun, VerifyFunAndState0}]),

    {error, {bad_cert, unknown_ca}} =
	public_key:pkix_path_validation(unknown_ca, [Cert1, Cert3, Cert4], []),

    VerifyFunAndState1 =
	{fun(_,{bad_cert, unknown_ca}, UserState) ->
		 {valid, UserState};
	    (_,{bad_cert, _} = Reason, _) ->
		 {fail, Reason};
	    (_,{extension, _}, UserState) ->
		 {unknown, UserState};
	    (_, valid, UserState) ->
		 {valid, UserState}
	 end, []},

    {ok, _} =
	public_key:pkix_path_validation(unknown_ca, [Cert1], [{verify_fun,
							      VerifyFunAndState1}]),

    VerifyFunAndState2 =
        {fun(_, {bad_cert, selfsigned_peer}, _UserState) ->
                  {fail, custom_reason};
            (_,{extension, _}, UserState) ->
		          {unknown, UserState};
	        (_, valid, UserState) ->
		          {valid, UserState}
        end, []},

    {error, custom_reason} =
        public_key:pkix_path_validation(selfsigned_peer, [Trusted], [{verify_fun,
                                                                      VerifyFunAndState2}]),
    % check RSASSA-PSS key
    % RsaPssKey = {public_key:generate_key({rsa, 1024, 65537}), pss_params(sha256)},
    RsaPssKey = {hardcode_rsa_key(1), pss_params(sha256)},

    _CaKPSS = {TrustedPSSCert,_} = erl_make_certs:make_cert([{key, RsaPssKey},
                 {subject, [
                    {name, "RSASSA-PSS Public Key"},
                    {?'id-at-name', {printableString, "public_key"}},
                    {?'id-at-pseudonym', {printableString, "pubkey"}},
                    {city, "Stockholm"},
                    {country, "SE"},
                    {org, "erlang"},
                    {org_unit, "testing dep"}
                       ]}
                ]),
    _ChainPSSCert = {CertPSS, _} = erl_make_certs:make_cert([{issuer, {TrustedPSSCert,RsaPssKey}}]),
    {ok, _} = public_key:pkix_path_validation(TrustedPSSCert, [CertPSS], []).

pkix_path_validation_root_expired() ->
    [{doc, "Test root expiration so that it does not fall between chairs"}].
pkix_path_validation_root_expired(Config) when is_list(Config) ->
    {Year, Month, Day} = date(),
    SRoot = public_key:pkix_test_root_cert("OTP test server ROOT", [{validity, {{Year-2, Month, Day}, 
                                                                                {Year-1, Month, Day}}}]),
    #{server_config := Conf} = public_key:pkix_test_data(#{server_chain => #{root => SRoot,
                                                                             intermediates => [],
                                                                             peer => []},
                                                           client_chain => #{root => [], 
                                                                             intermediates => [],
                                                                             peer => []}}),
    [ICA, Root] = proplists:get_value(cacerts, Conf),
    true = public_key:pkix_is_self_signed(Root),
    Peer = proplists:get_value(cert, Conf),
    {error, {bad_cert, cert_expired}} = public_key:pkix_path_validation(Root, [ICA, Peer], []).
    
pkix_ext_key_usage() ->
    [{doc, "Extended key usage is usually in end entity certs, may be in CA but should not be critical in such case"}].
pkix_ext_key_usage(Config) when is_list(Config) ->
    SRootSpec = public_key:pkix_test_root_cert("OTP test server ROOT", []),
    CRootSpec = public_key:pkix_test_root_cert("OTP test client ROOT", []),

    FailCAExt = [#'Extension'{extnID = ?'id-ce-extKeyUsage',
                              extnValue = [?'anyExtendedKeyUsage'],
                              critical = true}],
    CAExt = [#'Extension'{extnID = ?'id-ce-extKeyUsage',
                          extnValue = [?'anyExtendedKeyUsage'],
                          critical = false}],

    #{server_config := SConf,
      client_config := CConf} = public_key:pkix_test_data(#{server_chain => #{root => SRootSpec,
                                                                             intermediates => [[{extensions, FailCAExt}]],
                                                                             peer => []},
                                                           client_chain => #{root => CRootSpec,
                                                                             intermediates => [[{extensions, CAExt}]],
                                                                             peer => []}}),
    [_STRoot, SICA, SRoot] = proplists:get_value(cacerts, SConf),
    [_CTRoot, CICA, CRoot] = proplists:get_value(cacerts, CConf),
    SPeer = proplists:get_value(cert, SConf),
    CPeer = proplists:get_value(cert, CConf),

    {error, {bad_cert, invalid_ext_key_usage}} = public_key:pkix_path_validation(SRoot, [SICA, SPeer], []),

    {ok, _} = public_key:pkix_path_validation(CRoot, [CICA, CPeer], []).

pkix_path_validation_bad_date() ->
    [{doc, "Ensure bad date formats in `validity` are handled gracefully by verify fun"}].
pkix_path_validation_bad_date(Config) when is_list(Config) ->
    % Load PEM certchain from file
    DataDir = proplists:get_value(data_dir, Config),
    {ok, Bin} = file:read_file(filename:join(DataDir,"bad_date_certchain.pem")),

    % Decode and extract raw der encoded certificates
    CertificateList = public_key:pem_decode(Bin),
    [Root | CertificateChain] = lists:map(fun({'Certificate', Der, _}) -> Der end, CertificateList),

    % First test error `invalid_validity_dates` being returned correctly without `verify_fun` override
    {error, {bad_cert, invalid_validity_dates}} = public_key:pkix_path_validation(Root, CertificateChain, []),

    % Then test no exception thrown if verify_fun function traps the date error
    {ok, _} = public_key:pkix_path_validation(Root, CertificateChain, [
       {verify_fun, % This is the same as ?DEFAULT_VERIFYFUN, but it handles `invalid_validity_dates` gracefully.
            {fun
                % Test if we can successfully override `invalid_validity_dates`
                (_, {bad_cert, invalid_validity_dates}, UserState) ->
                    {valid, UserState};
                (_,{extension, _}, UserState) ->
		            {unknown, UserState};
                (_, valid_peer, UserState) ->
				    {valid, UserState};
                (_, valid, UserState) ->
                    {valid, UserState}
            end, []}
        }
    ]).

%%--------------------------------------------------------------------
%% To generate the PEM file contents:
%%
%% openssl req -x509 -nodes -newkey rsa:1024 -keyout /dev/null -subj '/C=SE/CN=example.com/CN=*.foo.example.com/CN=a*b.bar.example.com/O=erlang.org' > public_key_SUITE_data/pkix_verify_hostname_cn.pem
%%
%% Note that the same pem-file is used in pkix_verify_hostname_options/1
%%
%% Subject: C=SE, CN=example.com, CN=*.foo.example.com, CN=a*b.bar.example.com, O=erlang.org
%% extensions = no subjAltName

pkix_verify_hostname_cn(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    {ok,Bin} = file:read_file(filename:join(DataDir,"pkix_verify_hostname_cn.pem")),
    Cert = public_key:pkix_decode_cert(element(2,hd(public_key:pem_decode(Bin))), otp),

    %% Check that 1) only CNs are checked,
    %%            2) an empty label does not match a wildcard and
    %%            3) a wildcard does not match more than one label
    false = public_key:pkix_verify_hostname(Cert, [{dns_id,"erlang.org"},
						   {dns_id,"foo.EXAMPLE.com"},
						   {dns_id,"b.a.foo.EXAMPLE.com"}]),

    %% Check that a hostname is extracted from a https-uri and used for checking:
    true =  public_key:pkix_verify_hostname(Cert, [{uri_id,"HTTPS://EXAMPLE.com"}]),

    %% Check wildcard matching one label:
    true =  public_key:pkix_verify_hostname(Cert, [{dns_id,"a.foo.EXAMPLE.com"}]),

    %% Check wildcard with surrounding chars matches one label:
    true =  public_key:pkix_verify_hostname(Cert, [{dns_id,"accb.bar.EXAMPLE.com"}]),

    %% Check that a wildcard with surrounding chars matches an empty string:
    true =  public_key:pkix_verify_hostname(Cert, [{uri_id,"https://ab.bar.EXAMPLE.com"}]).

%%--------------------------------------------------------------------
%% To generate the PEM file contents:
%%
%% openssl req -x509 -nodes -newkey rsa:1024 -keyout /dev/null -extensions SAN -config  public_key_SUITE_data/verify_hostname.conf 2>/dev/null > public_key_SUITE_data/pkix_verify_hostname_subjAltName.pem
%%
%% Subject: C=SE, CN=example.com
%% Subject Alternative Name: DNS:kb.example.org, DNS:*.example.org, URI:http://www.example.org, URI:https://wws.example.org

pkix_verify_hostname_subjAltName(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    {ok,Bin} = file:read_file(filename:join(DataDir,"pkix_verify_hostname_subjAltName.pem")),
    Cert = public_key:pkix_decode_cert(element(2,hd(public_key:pem_decode(Bin))), otp),

    %% Check that neither a uri nor dns hostname matches a CN if subjAltName is present:
    false = public_key:pkix_verify_hostname(Cert, [{uri_id,"https://example.com"},
						   {dns_id,"example.com"}]),

    %% Check that a uri_id matches a URI subjAltName:
    true =  public_key:pkix_verify_hostname(Cert, [{uri_id,"https://wws.example.org"}]),

    %% Check that a dns_id does not match a URI subjAltName:
    false = public_key:pkix_verify_hostname(Cert, [{dns_id,"www.example.org"},
						   {dns_id,"wws.example.org"}]),

    %% Check that a dns_id matches a DNS subjAltName:
    true =  public_key:pkix_verify_hostname(Cert, [{dns_id,"kb.example.org"}]),
    true =  public_key:pkix_verify_hostname(Cert, [{dns_id,"KB.EXAMPLE.ORG"}]),

    %% Check that a dns_id does not match a DNS subjAltName with wildcard
    false =  public_key:pkix_verify_hostname(Cert, [{dns_id,"other.example.org"}]),

    %% Check that a dns_id does match a DNS subjAltName with wildcard with matchfun
    MatchFun = {match_fun, public_key:pkix_verify_hostname_match_fun(https)},
    true =  public_key:pkix_verify_hostname(Cert, [{dns_id,"other.example.org"}], [MatchFun]),
    true =  public_key:pkix_verify_hostname(Cert, [{dns_id,"OTHER.EXAMPLE.ORG"}], [MatchFun]),

    %% Check that a uri_id does not match a DNS subjAltName with wildcard
    false =  public_key:pkix_verify_hostname(Cert, [{uri_id,"https://other.example.org"}]),
    false =  public_key:pkix_verify_hostname(Cert, [{uri_id,"https://OTHER.EXAMPLE.ORG"}]),

    %% Check that a dns_id does match a DNS subjAltName with wildcard with matchfun
    true =  public_key:pkix_verify_hostname(Cert, [{uri_id,"https://other.example.org"}], [MatchFun]),
    true =  public_key:pkix_verify_hostname(Cert, [{uri_id,"https://OTHER.EXAMPLE.ORG"}], [MatchFun]),
    true =  public_key:pkix_verify_hostname(Cert, [{uri_id,"https://OTHER.example.org"}], [MatchFun]),

    ok.

%%--------------------------------------------------------------------
%% Uses the pem-file for pkix_verify_hostname_cn
%% Subject: C=SE, CN=example.com, CN=*.foo.example.com, CN=a*b.bar.example.com, O=erlang.org
pkix_verify_hostname_options(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    {ok,Bin} = file:read_file(filename:join(DataDir,"pkix_verify_hostname_cn.pem")),
    Cert = public_key:pkix_decode_cert(element(2,hd(public_key:pem_decode(Bin))), otp),
    
    %% Check that the fail_callback is called and is presented the correct certificate:
    true = public_key:pkix_verify_hostname(Cert, [{dns_id,"erlang.org"}],
					   [{fail_callback,
					     fun(#'OTPCertificate'{}=C) when C==Cert -> 
						     true; % To test the return value matters
						(#'OTPCertificate'{}=C) -> 
						     ct:log("~p:~p: Wrong cert:~n~p~nExpect~n~p",
							    [?MODULE, ?LINE, C, Cert]),
						     ct:fail("Wrong cert, see log");
						(C) -> 
						     ct:log("~p:~p: Bad cert: ~p",[?MODULE,?LINE,C]),
						     ct:fail("Bad cert, see log")
					     end}]),
    
    %% Check the callback for user-provided match functions:
    true =  public_key:pkix_verify_hostname(Cert, [{dns_id,"very.wrong.domain"}],
					    [{match_fun,
					      fun("very.wrong.domain", {cn,"example.com"}) ->
						      true;
						 (_, _) ->
						      false
					      end}]),
    false = public_key:pkix_verify_hostname(Cert, [{dns_id,"not.example.com"}],
					    [{match_fun, fun(_, _) -> default end}]),
    true =  public_key:pkix_verify_hostname(Cert, [{dns_id,"example.com"}],
					    [{match_fun, fun(_, _) -> default end}]),

    %% Check the callback for user-provided fqdn extraction:
    true =  public_key:pkix_verify_hostname(Cert, [{uri_id,"some://very.wrong.domain"}],
					    [{fqdn_fun,
					      fun({uri_id, "some://very.wrong.domain"}) ->
						      "example.com";
						 (_) ->
						      ""
					      end}]),
    true =  public_key:pkix_verify_hostname(Cert, [{uri_id,"https://example.com"}],
					    [{fqdn_fun, fun(_) -> default end}]),
    false =  public_key:pkix_verify_hostname(Cert, [{uri_id,"some://very.wrong.domain"}]),

    true = public_key:pkix_verify_hostname(Cert, [{dns_id,"example.com"}]),
    true = public_key:pkix_verify_hostname(Cert, [{dns_id,"abb.bar.example.com"}]),
    false = public_key:pkix_verify_hostname(Cert, [{dns_id,"example.com"},
                                                   {dns_id,"abb.bar.example.com"}],
                                            [{fqdn_fun,fun(_)->undefined end}]),
    %% Test that a common name is matched fully, that is do not allow prefix matches
    %% with less dots (".")
    {ok, PrefixBin} = file:read_file(filename:join(DataDir,"prefix-dots.pem")),
    PrefixCert = public_key:pkix_decode_cert(element(2,hd(public_key:pem_decode(PrefixBin))), otp),
    true = public_key:pkix_verify_hostname(PrefixCert, [{dns_id,"..a"}]),
    false = public_key:pkix_verify_hostname(PrefixCert, [{dns_id,".a"}]).

%%--------------------------------------------------------------------
%% To generate the PEM file contents:
%%
%% openssl req -x509 -nodes -newkey rsa:1024 -keyout /dev/null -extensions SAN -config  public_key_SUITE_data/verify_hostname_ip.conf 2>/dev/null > public_key_SUITE_data/pkix_verify_hostname_subjAltName_IP.pem
%%
%% Subject: C=SE, CN=example.com
%% Subject Alternative Name: DNS:1.2.3.4, DNS: abcd:ef::1, IP:10.67.16.75, URI:https://10.11.12.13

pkix_verify_hostname_subjAltName_IP(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    {ok,Bin} = file:read_file(filename:join(DataDir,"pkix_verify_hostname_subjAltName_IP.pem")),
    Cert = public_key:pkix_decode_cert(element(2,hd(public_key:pem_decode(Bin))), otp),

    %% Print the tests that a matchfun has to handle
    catch public_key:pkix_verify_hostname(Cert, [{some_tag,"some.domain"},
                                                 {ip, {10,67,16,75}}
                                                ],
                                          [{match_fun,
                                            fun(Ref,Pres) -> 
                                                    ct:pal("~p:~p:~nRef : ~p~nPres: ~p",[?MODULE,?LINE,Ref,Pres]),
                                                    false
                                            end}]),

    false =  public_key:pkix_verify_hostname(Cert, [{uri_id,"https://1.2.3.4"}]),
    true  =  public_key:pkix_verify_hostname(Cert, [{uri_id,"https://10.11.12.13"}]),
    true  =  public_key:pkix_verify_hostname(Cert, [{dns_id,"1.2.3.4"}]),
    true  =  public_key:pkix_verify_hostname(Cert, [{dns_id,<<"1.2.3.4">>}]),
    false =  public_key:pkix_verify_hostname(Cert, [{dns_id,"10.67.16.75"}]),
    true  =  public_key:pkix_verify_hostname(Cert, [{ip, "aBcD:ef:0::0:1"}]),
    true  =  public_key:pkix_verify_hostname(Cert, [{ip, {16#abcd,16#ef,0,0,0,0,0,1}}]),
    true  =  public_key:pkix_verify_hostname(Cert, [{ip, "10.67.16.75"}]),
    true  =  public_key:pkix_verify_hostname(Cert, [{ip, <<"10.67.16.75">>}]),
    true  =  public_key:pkix_verify_hostname(Cert, [{ip, {10,67,16,75}}]),
    false =  public_key:pkix_verify_hostname(Cert, [{ip, {1,2,3,4}}]),
    false =  public_key:pkix_verify_hostname(Cert, [{ip, {10,11,12,13}}]).


%%--------------------------------------------------------------------

pkix_dist_point_uri() ->
 [{doc, "Disregard ldap URIs in code path handling HTTP URIs"}].
pkix_dist_point_uri(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    {ok, PemCert} = file:read_file(filename:join(Datadir, "ldap_uri_cert.pem")),
    [{_, Cert, _}] = public_key:pem_decode(PemCert),
    #'OTPCertificate'{tbsCertificate = TBSCert} = public_key:pkix_decode_cert(Cert, otp),

    Extensions = pubkey_cert:extensions_list(TBSCert#'OTPTBSCertificate'.extensions),
    DpExt = pubkey_cert:select_extension(?'id-ce-cRLDistributionPoints', Extensions),
    #'Extension'{extnValue = DPs} = DpExt,
    [#'DistributionPoint'{distributionPoint = {fullName, DPNames}}|_] = DPs,
    ct:pal("~p", [DPNames]),
    true = pubkey_crl:match_one(DPNames, [{uniformResourceIdentifier, "http://ca.eait.uq.edu.au/crl/labs-LILY-CA"}]).

%%--------------------------------------------------------------------

pkix_iso_rsa_oid() ->
 [{doc, "Test workaround for supporting certs that use ISO oids"
   " 1.3.14.3.2.29 instead of PKIX/PKCS oid"}].
pkix_iso_rsa_oid(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    {ok, PemCert} = file:read_file(filename:join(Datadir, "rsa_ISO.pem")),
    [{_, Cert, _}] = public_key:pem_decode(PemCert),
    OTPCert = public_key:pkix_decode_cert(Cert, otp),
    SigAlg = OTPCert#'OTPCertificate'.signatureAlgorithm,
    {_, rsa} = public_key:pkix_sign_types(SigAlg#'SignatureAlgorithm'.algorithm).

%%--------------------------------------------------------------------
pkix_iso_dsa_oid() ->
 [{doc, "Test workaround for supporting certs that use ISO oids"
   "1.3.14.3.2.27 instead of PKIX/PKCS oid"}].
pkix_iso_dsa_oid(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    {ok, PemCert} = file:read_file(filename:join(Datadir, "dsa_ISO.pem")),
    [{_, Cert, _}] = public_key:pem_decode(PemCert),
    OTPCert = public_key:pkix_decode_cert(Cert, otp),
    SigAlg = OTPCert#'OTPCertificate'.signatureAlgorithm,
    {_, dsa} = public_key:pkix_sign_types(SigAlg#'SignatureAlgorithm'.algorithm).

%%--------------------------------------------------------------------
pkix_dsa_sha2_oid() ->
 [{doc, "Test support dsa_sha2 oid"}].
pkix_dsa_sha2_oid(Config) when is_list(Config) ->
    {sha224, dsa} = public_key:pkix_sign_types(?'id-dsa-with-sha224'),
    {sha256, dsa} = public_key:pkix_sign_types(?'id-dsa-with-sha256').
    
%%--------------------------------------------------------------------

pkix_crl() ->
    [{doc, "test pkix_crl_* functions"}].

pkix_crl(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    {ok, PemCRL} = file:read_file(filename:join(Datadir, "idp_crl.pem")),
    [{_, CRL, _}] = public_key:pem_decode(PemCRL),
    
    {ok, IDPPemCert} = file:read_file(filename:join(Datadir, "idp_cert.pem")),
    [{_, IDPCert, _}] = public_key:pem_decode(IDPPemCert),

    {ok, SignPemCert} = file:read_file(filename:join(Datadir, "crl_signer.pem")),
    [{_, SignCert, _}] = public_key:pem_decode(SignPemCert),
    
    OTPIDPCert = public_key:pkix_decode_cert(IDPCert, otp),
    OTPSignCert = public_key:pkix_decode_cert(SignCert, otp),
    ERLCRL = public_key:der_decode('CertificateList',CRL),

    {rdnSequence,_} = public_key:pkix_crl_issuer(CRL),
    {rdnSequence,_} = public_key:pkix_crl_issuer(ERLCRL),
    
    true = public_key:pkix_crl_verify(CRL, SignCert),
    true = public_key:pkix_crl_verify(ERLCRL, OTPSignCert),

    [#'DistributionPoint'{}|_] = public_key:pkix_dist_points(IDPCert),
    [#'DistributionPoint'{}|_] = public_key:pkix_dist_points(OTPIDPCert),

    #'DistributionPoint'{cRLIssuer = asn1_NOVALUE,
     			 reasons = asn1_NOVALUE,
			 distributionPoint =  Point} = public_key:pkix_dist_point(IDPCert),
    #'DistributionPoint'{cRLIssuer = asn1_NOVALUE,
			 reasons = asn1_NOVALUE,
			 distributionPoint =  Point} = public_key:pkix_dist_point(OTPIDPCert).

general_name() ->
    [{doc, "Test that decoding of general name filed may have other values"
      " than {rdnSequence,  Seq}"}].

general_name(Config) when is_list(Config) ->
    DummyRfc822Name = "CN=CNDummy, OU=OUDummy, O=ODummy, C=SE",
    {ok, {1,  DummyRfc822Name}} = 
	pubkey_cert:cert_auth_key_id(
	  #'AuthorityKeyIdentifier'{authorityCertIssuer = 
					[{rfc822Name, DummyRfc822Name}],
				    authorityCertSerialNumber = 
					1}).

%%--------------------------------------------------------------------

pkix_hash_type() ->
     [{doc, "Test API function pkix_hash_type/1"}].

pkix_hash_type(Config) when is_list(Config) ->
    sha = public_key:pkix_hash_type(?'id-sha1'), 
    sha512 = public_key:pkix_hash_type(?'id-sha512'),
    sha384 = public_key:pkix_hash_type(?'id-sha384'),
    sha256 = public_key:pkix_hash_type(?'id-sha256'), 
    sha224 = public_key:pkix_hash_type('id-sha224'),
    md5 = public_key:pkix_hash_type('id-md5').


%%--------------------------------------------------------------------

pkix_test_data_all_default() ->
    [{doc, "Test API function pkix_test_data/1"}].

pkix_test_data_all_default(Config) when is_list(Config) ->
    #{server_config := ServerConf0,
      client_config := ClientConf0} = public_key:pkix_test_data(#{server_chain => 
                                                                     #{root => [],
                                                                       intermediates => [[]],
                                                                       peer => []},
                                                                 client_chain => 
                                                                     #{root => [],
                                                                       intermediates => [[]],
                                                                       peer => []}}),
    check_conf_member(ServerConf0, [key, cert, cacerts]),
    check_conf_member(ClientConf0, [key, cert, cacerts]),
    
    3 = length(proplists:get_value(cacerts, ServerConf0)),
    3 = length(proplists:get_value(cacerts, ServerConf0)),

    #{server_config := ServerConf1,
      client_config := ClientConf1} = public_key:pkix_test_data(#{server_chain => 
                                                                     #{root => [],
                                                                       peer => []},
                                                                 client_chain => 
                                                                     #{root => [],
                                                                       peer => []}}),
    2 = length(proplists:get_value(cacerts, ServerConf1)),
    2 = length(proplists:get_value(cacerts, ServerConf1)),
    
    check_conf_member(ServerConf1, [key, cert, cacerts]),
    check_conf_member(ClientConf1, [key, cert, cacerts]).
    
%%--------------------------------------------------------------------

pkix_test_data() ->
    [{doc, "Test API function pkix_test_data/1"}].

pkix_test_data(Config) when is_list(Config) ->
    {Year, Month, Day} = date(),
    Keygen = 
        case crypto:ec_curves() of
        [] ->
            {rsa, 2048, 17};
        [Curve |_] ->
            Oid = pubkey_cert_records:namedCurves(Curve),
            {namedCurve, Oid}
        end,
    #{server_config := ServerConf0,
      client_config := ClientConf0} = 
        public_key:pkix_test_data(#{server_chain => 
                                        #{root => [],
                                          intermediates => [],
                                          peer => [{key, hardcode_rsa_key(1)}]},
                                    client_chain => 
                                        #{root => [{validity, {{Year-2, Month, Day}, 
                                                               {Year-1, Month, Day}}}],
                                          intermediates => 
                                              [[{extensions, [#'Extension'{extnID = ?'id-ce-basicConstraints',
                                                                           extnValue = #'BasicConstraints'{cA=true, 
                                                                                             pathLenConstraint = 1},
                                                                           critical = true}]}]],
                                               peer => [{key, Keygen}, {digest, sha1}]}}),
    check_conf_member(ServerConf0, [key, cert, cacerts]),
    check_conf_member(ClientConf0, [key, cert, cacerts]).

   
                                 
check_conf_member(_, []) ->
    true;
check_conf_member(Conf, [Member | Rest]) ->
    case lists:keymember(Member, 1, Conf) of
        true ->
            check_conf_member(Conf, Rest);
        false ->
            ct:fail({misssing_conf, Member})
    end.
                              
%%--------------------------------------------------------------------
pkix_is_issuer() ->
    [{doc, "Test pubkey_cert:pkix_is_issuer with cert that have diffent cases on countryname"}].

pkix_is_issuer(Config) when is_list(Config) ->
    Upper = {rdnSequence,
             [[{'AttributeTypeAndValue',{2,5,4,6},"GB"}],
              [{'AttributeTypeAndValue',{2,5,4,10},{utf8String,<<"MYORG">>}}],
              [{'AttributeTypeAndValue',{2,5,4,11},{utf8String,<<"INTERMEDIATE">>}}],
              [{'AttributeTypeAndValue',{2,5,4,3},{utf8String,<<"INTERMEDIATE">>}}]]},
    Lower = {rdnSequence,
             [[{'AttributeTypeAndValue',{2,5,4,6},"gb"}],
              [{'AttributeTypeAndValue',{2,5,4,10},{utf8String,<<"MYORG">>}}],
              [{'AttributeTypeAndValue',{2,5,4,11},{utf8String,<<"INTERMEDIATE">>}}],
              [{'AttributeTypeAndValue',{2,5,4,3},{utf8String,<<"INTERMEDIATE">>}}]]},
    true = pubkey_cert:is_issuer(Upper, Lower).

%%--------------------------------------------------------------------
short_cert_issuer_hash() ->
    [{doc, "Test OpenSSL-style hash for certificate issuer"}].

short_cert_issuer_hash(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),
    [{'Certificate', CertDER, _}] =
	erl_make_certs:pem_to_der(filename:join(Datadir, "client_cert.pem")),

    %% This hash value was obtained by running:
    %% openssl x509 -in client_cert.pem -issuer_hash -noout
    CertIssuerHash = "d4c8d7e5",

    #'OTPCertificate'{tbsCertificate = #'OTPTBSCertificate'{issuer = Issuer}} =
	public_key:pkix_decode_cert(CertDER, otp),

    CertIssuerHash = public_key:short_name_hash(Issuer).

%%--------------------------------------------------------------------
short_crl_issuer_hash() ->
    [{doc, "Test OpenSSL-style hash for CRL issuer"}].

short_crl_issuer_hash(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),
    [{'CertificateList', CrlDER, _}] =
	erl_make_certs:pem_to_der(filename:join(Datadir, "idp_crl.pem")),

    %% This hash value was obtained by running:
    %% openssl crl -in idp_crl.pem -hash -noout
    CrlIssuerHash = "d6134ed3",

    Issuer = public_key:pkix_crl_issuer(CrlDER),

    CrlIssuerHash = public_key:short_name_hash(Issuer).

%%--------------------------------------------------------------------
gen_ec_param_prime_field() ->
    [{doc, "Generate key with EC prime_field parameters"}].
gen_ec_param_prime_field(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    do_gen_ec_param(filename:join(Datadir, "ec_key_param0.pem")).

%%--------------------------------------------------------------------
gen_ec_param_char_2_field() ->
    [{doc, "Generate key with EC characteristic_two_field parameters"}].
gen_ec_param_char_2_field(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    do_gen_ec_param(filename:join(Datadir, "ec_key_param1.pem")).

%%--------------------------------------------------------------------
ocsp_extensions() ->
    [{doc, "Check OCSP extensions"}].
ocsp_extensions(_Config) ->
    Nonce = <<4,8,66,243,220,236,16,118,51,215>>,
    ExpectedExtentions =
        [{'Extension',
          ?'id-pkix-ocsp-nonce',
          asn1_DEFAULT,
          <<4,8,66,243,220,236,16,118,51,215>>},
         {'Extension',
          ?'id-pkix-ocsp-response',
          asn1_DEFAULT,
          <<48,11,6,9,43,6,1,5,5,7,48,1,1>>}],
    ExpectedExtentions = public_key:ocsp_extensions(Nonce).

pkix_ocsp_validate() ->
    [{doc, "Check OCSP extensions"}].
pkix_ocsp_validate(_Config) ->
    Cert =
        {'OTPCertificate',{'OTPTBSCertificate',v3,9,
                           {'SignatureAlgorithm',{1,2,840,113549,1,1,11},'NULL'},
                           {rdnSequence,[[{'AttributeTypeAndValue',{2,5,4,3},{utf8String,<<"otpCA">>}}],[{'AttributeTypeAndValue',{2,5,4,11},{utf8String,<<"Erlang OTP">>}}],[{'AttributeTypeAndValue',{2,5,4,10},{utf8String,<<"Ericsson AB">>}}],[{'AttributeTypeAndValue',{2,5,4,6},"SE"}],[{'AttributeTypeAndValue',{2,5,4,7},{utf8String,<<"Stockholm">>}}],[{'AttributeTypeAndValue',{1,2,840,113549,1,9,1},"peter@erix.ericsson.se"}]]},
                           {'Validity',{utcTime,"230721110721Z"},{utcTime,"330529110721Z"}},
                           {rdnSequence,[[{'AttributeTypeAndValue',{2,5,4,3},{utf8String,<<"a.server">>}}],[{'AttributeTypeAndValue',{2,5,4,11},{utf8String,<<"Erlang OTP">>}}],[{'AttributeTypeAndValue',{2,5,4,10},{utf8String,<<"Ericsson AB">>}}],[{'AttributeTypeAndValue',{2,5,4,6},"SE"}],[{'AttributeTypeAndValue',{2,5,4,7},{utf8String,<<"Stockholm">>}}],[{'AttributeTypeAndValue',{1,2,840,113549,1,9,1},"peter@erix.ericsson.se"}]]},
                           {'OTPSubjectPublicKeyInfo',{'PublicKeyAlgorithm',{1,2,840,113549,1,1,1},'NULL'},
                            {'RSAPublicKey',19254743747256260264207569423711759377779938665145630924415701722071839009286238971264967781043993434178803001083069740412920664146137571550852074547463946025114390093775800702438227109245066854329070921351832849321692114677809046259034306196616912261365770291322044071697789183279204771685063580949070504947864713748039312242300503875879444809664605423001542854874228001872895975468648787616073960661286876663709764410812833966560999459482926236332297043685455899393823175706646393051956438518613689798667608292659880957737510004003274559865311466147775473832468655042097383293967251824412697382839864114388741712057,
                             65537}},
                           asn1_NOVALUE,asn1_NOVALUE,
                           [{'Extension',{2,5,29,19},false,{'BasicConstraints',false,asn1_NOVALUE}},
                            {'Extension',{2,5,29,15},false,[digitalSignature,nonRepudiation,keyEncipherment]},
                            {'Extension',{2,5,29,14},false,<<175,14,85,35,212,170,133,20,114,234,90,223,163,49,255,87,86,93,165,56>>},
                            {'Extension',{2,5,29,35},
                             false,
                             {'AuthorityKeyIdentifier',<<123,93,133,100,41,175,227,134,140,47,217,84,132,181,89,186,102,41,30,255>>,
                              [{directoryName,{rdnSequence,[[{'AttributeTypeAndValue',{2,5,4,3},{utf8String,<<"erlangCA">>}}],
                                                            [{'AttributeTypeAndValue',{2,5,4,11},{utf8String,<<"Erlang OTP">>}}],
                                                            [{'AttributeTypeAndValue',{2,5,4,10},{utf8String,<<"Ericsson AB">>}}],
                                                            [{'AttributeTypeAndValue',{2,5,4,7},{utf8String,<<"Stockholm">>}}],
                                                            [{'AttributeTypeAndValue',{2,5,4,6},"SE"}],
                                                            [{'AttributeTypeAndValue',{1,2,840,113549,1,9,1},"peter@erix.ericsson.se"}]]}}],
                              1}},
                            {'Extension',{2,5,29,17},false,[{dNSName,"host.example.com"}]},
                            {'Extension',{2,5,29,18},false,[{rfc822Name,"peter@erix.ericsson.se"}]}]},
         {'SignatureAlgorithm',{1,2,840,113549,1,1,11},'NULL'},
         <<23,196,208,23,144,187,135,84,233,168,123,81,115,112,33,52,77,238,239,70,248,131,119,160,178,216,252,166,176,20,252,211,108,160,202,140,96,84,98,209,7,149,30,184,0,196,139,48,122,36,45,10,198,106,98,33,183,254,48,11,88,64,93,232,152,233,133,216,191,128,35,96,183,221,122,87,230,30,191,199,226,203,164,217,236,101,83,158,113,211,177,52,217,39,96,108,242,87,70,44,246,68,124,122,121,88,188,254,22,48,98,121,238,158,4,160,141,249,255,93,147,83,42,86,62,5,118,164,54,75,87,49,111,
           126,197,89,32,226,89,40,154,70,165,118,239,26,249,59,48,52,237,152,240,131,100,187,14,157,201,103,102,27,81,198,226,121,221,68,244,119,130,149,231,179,35,64,96,254,245,5,199,112,145,65,69,80,87,235,140,137,20,220,148,157,94,123,177,186,187,66,99,92,150,213,147,129,36,126,93,4,10,123,70,238,175,247,102,91,42,201,27,123,76,212,45,115,11,31,114,173,124,27,156,248,36,37,195,111,206,236,43,224,157,50,98,109,179,87,223,187,8,204,197,202,155,60>>},
    IssuerCert =
        {'OTPCertificate',{'OTPTBSCertificate',v3,1,
                           {'SignatureAlgorithm',{1,2,840,113549,1,1,11},'NULL'},
                           {rdnSequence,[[{'AttributeTypeAndValue',{2,5,4,3},{utf8String,<<"erlangCA">>}}],[{'AttributeTypeAndValue',{2,5,4,11},{utf8String,<<"Erlang OTP">>}}],[{'AttributeTypeAndValue',{2,5,4,10},{utf8String,<<"Ericsson AB">>}}],[{'AttributeTypeAndValue',{2,5,4,7},{utf8String,<<"Stockholm">>}}],[{'AttributeTypeAndValue',{2,5,4,6},"SE"}],[{'AttributeTypeAndValue',{1,2,840,113549,1,9,1},"peter@erix.ericsson.se"}]]},
                           {'Validity',{utcTime,"230721110720Z"},{utcTime,"330529110720Z"}},
                           {rdnSequence,[[{'AttributeTypeAndValue',{2,5,4,3},{utf8String,<<"otpCA">>}}],[{'AttributeTypeAndValue',{2,5,4,11},{utf8String,<<"Erlang OTP">>}}],[{'AttributeTypeAndValue',{2,5,4,10},{utf8String,<<"Ericsson AB">>}}],[{'AttributeTypeAndValue',{2,5,4,6},"SE"}],[{'AttributeTypeAndValue',{2,5,4,7},{utf8String,<<"Stockholm">>}}],[{'AttributeTypeAndValue',{1,2,840,113549,1,9,1},"peter@erix.ericsson.se"}]]},
                           {'OTPSubjectPublicKeyInfo',{'PublicKeyAlgorithm',{1,2,840,113549,1,1,1},'NULL'},
                            {'RSAPublicKey',21858379260819365313885475389172639523863567481982302063462584029790343874819317972475546206568963022785252583910194728269078148431804871680312638323851125861707159230343297343111968246731095811513561212201088276841624533346998017512000090901290490304174895932870845288899008429347052837949441312958652271962356020302617279856538736007013593572768976262766464136388094144122584736630529987720049486299302127652434926700165727330943325372510516379103006575448279898129379834740761468401572505064753618409945975591285059206889943804512145915054818226570266582909516966602868100682823910151957272535898084374557112395143,
                             65537}},
                           asn1_NOVALUE,asn1_NOVALUE,
                           [{'Extension',{2,5,29,19},true,{'BasicConstraints',true,asn1_NOVALUE}},
                            {'Extension',{2,5,29,15},false,[keyCertSign,cRLSign]},
                            {'Extension',{2,5,29,14},false,<<123,93,133,100,41,175,227,134,140,47,217,84,132,181,89,186,102,41,30,255>>},
                            {'Extension',{2,5,29,35},
                             false,
                             {'AuthorityKeyIdentifier',<<229,159,14,81,153,72,30,27,33,37,234,91,103,205,230,72,95,185,112,95>>,
                              [{directoryName,{rdnSequence,[[{'AttributeTypeAndValue',{2,5,4,3},{utf8String,<<"erlangCA">>}}],
                                                            [{'AttributeTypeAndValue',{2,5,4,11},{utf8String,<<"Erlang OTP">>}}],
                                                            [{'AttributeTypeAndValue',{2,5,4,10},{utf8String,<<"Ericsson AB">>}}],
                                                            [{'AttributeTypeAndValue',{2,5,4,7},{utf8String,<<"Stockholm">>}}],
                                                            [{'AttributeTypeAndValue',{2,5,4,6},"SE"}],
                                                            [{'AttributeTypeAndValue',{1,2,840,113549,1,9,1},"peter@erix.ericsson.se"}]]}}],
                              674805639123712796695508479052504582494838106155}},
                            {'Extension',{2,5,29,17},false,[{rfc822Name,"peter@erix.ericsson.se"}]},
                            {'Extension',{2,5,29,18},false,[{rfc822Name,"peter@erix.ericsson.se"}]}]},
         {'SignatureAlgorithm',{1,2,840,113549,1,1,11},'NULL'},
         <<44,128,75,220,253,223,223,77,33,57,30,205,101,103,200,211,254,81,122,195,123,239,98,5,118,58,179,193,24,93,12,243,124,194,160,163,206,243,199,49,143,11,73,192,218,193,154,93,146,232,1,191,99,201,129,94,131,59,107,227,216,17,31,101,67,153,177,189,164,194,224,164,78,160,42,79,131,65,37,78,226,201,200,180,128,38,101,164,193,72,82,196,88,204,145,94,235,84,13,243,0,149,99,175,203,211,108,177,156,17,27,40,87,195,19,56,39,102,103,42,27,60,30,44,204,157,107,121,128,68,93,216,123,
           106,112,105,74,7,142,155,171,1,8,31,123,245,78,142,111,142,178,127,169,202,110,125,35,192,199,23,203,201,103,44,99,100,192,156,214,62,109,71,205,66,32,81,252,124,138,238,225,88,247,85,255,65,141,131,234,184,248,20,51,81,71,19,98,102,114,96,49,77,1,79,27,18,218,79,37,232,194,204,172,54,124,167,188,158,43,54,183,230,40,230,152,216,12,27,56,66,104,238,235,52,176,110,159,88,151,7,228,201,248,195,82,131,220,31,104,44,239,147,61,71,35,245>>},
    OcspRespDer =
        <<48,130,7,36,10,1,0,160,130,7,29,48,130,7,25,6,9,43,6,1,5,5,7,48,1,1,4,130,7,10,48,130,7,6,48,130,1,10,161,129,134,48,129,131,49,14,48,12,6,3,85,4,3,12,5,111,116,112,67,65,49,19,48,17,6,3,85,4,11,12,10,69,114,108,97,110,103,32,79,84,80,49,20,48,18,6,3,85,4,10,12,11,69,114,105,99,115,115,111,110,32,65,66,49,11,48,9,6,3,85,4,6,19,2,83,69,49,18,48,16,6,3,85,4,7,12,9,83,116,111,99,107,104,111,108,109,49,37,48,35,6,9,42,134,72,134,247,13,1,9,1,22,22,112,101,116,101,114,64,101,114,105,120,46,101,
          114,105,99,115,115,111,110,46,115,101,24,15,50,48,50,51,48,55,50,49,49,49,48,55,50,53,90,48,81,48,79,48,58,48,9,6,5,43,14,3,2,26,5,0,4,20,227,147,252,182,155,101,129,45,194,162,22,93,127,46,112,193,196,28,241,232,4,20,123,93,133,100,41,175,227,134,140,47,217,84,132,181,89,186,102,41,30,255,2,1,9,128,0,24,15,50,48,50,51,48,55,50,49,49,49,48,55,50,53,90,161,27,48,25,48,23,6,9,43,6,1,5,5,7,48,1,2,4,10,4,8,244,183,192,191,230,8,236,82,48,13,6,9,42,134,72,134,247,13,1,1,11,5,0,3,130,1,1,0,151,99,
          102,238,65,164,80,97,143,115,223,2,201,56,75,220,145,150,17,27,9,169,149,158,40,226,29,109,8,35,234,24,59,113,1,26,123,144,32,68,235,210,36,55,61,215,0,183,49,156,52,153,132,237,180,231,43,45,18,138,126,118,173,130,246,213,225,216,15,85,248,146,35,220,27,100,93,232,234,91,206,224,98,18,48,52,95,213,129,117,11,174,228,48,220,235,82,141,157,179,13,119,17,244,189,21,77,102,114,166,227,25,160,113,148,244,142,33,232,161,77,189,187,72,196,144,82,70,200,250,222,68,154,153,20,33,60,4,252,151,16,64,
          207,109,4,30,49,47,75,150,122,24,90,22,226,156,91,30,83,141,79,29,116,58,13,185,66,215,89,19,64,194,190,72,113,112,136,61,75,5,138,239,108,222,87,212,193,155,108,150,47,180,73,3,110,216,68,189,146,8,179,94,110,147,207,86,2,65,251,193,111,254,43,200,77,72,154,214,13,40,48,209,104,42,105,175,163,52,160,39,92,238,240,174,145,3,33,49,33,231,26,14,5,32,33,220,74,149,25,163,131,65,30,63,134,148,160,130,4,224,48,130,4,220,48,130,4,216,48,130,3,192,160,3,2,1,2,2,1,1,48,13,6,9,42,134,72,134,247,13,1,
          1,11,5,0,48,129,134,49,17,48,15,6,3,85,4,3,12,8,101,114,108,97,110,103,67,65,49,19,48,17,6,3,85,4,11,12,10,69,114,108,97,110,103,32,79,84,80,49,20,48,18,6,3,85,4,10,12,11,69,114,105,99,115,115,111,110,32,65,66,49,18,48,16,6,3,85,4,7,12,9,83,116,111,99,107,104,111,108,109,49,11,48,9,6,3,85,4,6,19,2,83,69,49,37,48,35,6,9,42,134,72,134,247,13,1,9,1,22,22,112,101,116,101,114,64,101,114,105,120,46,101,114,105,99,115,115,111,110,46,115,101,48,30,23,13,50,51,48,55,50,49,49,49,48,55,50,48,90,23,13,
          51,51,48,53,50,57,49,49,48,55,50,48,90,48,129,131,49,14,48,12,6,3,85,4,3,12,5,111,116,112,67,65,49,19,48,17,6,3,85,4,11,12,10,69,114,108,97,110,103,32,79,84,80,49,20,48,18,6,3,85,4,10,12,11,69,114,105,99,115,115,111,110,32,65,66,49,11,48,9,6,3,85,4,6,19,2,83,69,49,18,48,16,6,3,85,4,7,12,9,83,116,111,99,107,104,111,108,109,49,37,48,35,6,9,42,134,72,134,247,13,1,9,1,22,22,112,101,116,101,114,64,101,114,105,120,46,101,114,105,99,115,115,111,110,46,115,101,48,130,1,34,48,13,6,9,42,134,72,134,247,
          13,1,1,1,5,0,3,130,1,15,0,48,130,1,10,2,130,1,1,0,173,38,214,237,131,195,86,49,85,177,225,21,254,222,227,229,5,62,193,131,224,141,51,233,36,68,108,11,164,68,95,160,243,171,55,43,32,228,15,5,179,194,124,9,53,219,33,15,243,77,206,104,255,63,250,231,185,218,111,190,98,34,71,38,139,51,202,112,110,85,248,177,207,156,210,51,28,18,236,236,4,188,58,33,169,250,181,59,114,133,246,82,217,36,166,28,3,70,49,82,68,36,134,32,57,142,168,231,193,73,219,102,49,13,97,199,40,138,118,250,244,41,206,121,115,208,
          19,230,5,243,38,239,1,36,41,13,232,86,191,182,144,86,6,211,57,117,243,216,229,51,99,224,126,39,125,40,127,104,11,72,234,205,113,200,92,92,16,19,136,114,193,132,13,94,240,242,21,211,46,12,85,64,205,36,26,63,69,187,206,233,0,170,217,10,160,20,147,236,233,244,66,234,133,95,84,34,109,40,107,163,119,22,202,156,112,153,240,188,17,145,105,157,23,239,140,106,7,155,196,161,187,21,174,181,169,137,91,242,134,9,35,52,159,36,160,30,169,36,130,60,61,61,245,235,229,135,2,3,1,0,1,163,130,1,80,48,130,1,76,48,
          15,6,3,85,29,19,1,1,255,4,5,48,3,1,1,255,48,11,6,3,85,29,15,4,4,3,2,1,6,48,29,6,3,85,29,14,4,22,4,20,123,93,133,100,41,175,227,134,140,47,217,84,132,181,89,186,102,41,30,255,48,129,198,6,3,85,29,35,4,129,190,48,129,187,128,20,229,159,14,81,153,72,30,27,33,37,234,91,103,205,230,72,95,185,112,95,161,129,140,164,129,137,48,129,134,49,17,48,15,6,3,85,4,3,12,8,101,114,108,97,110,103,67,65,49,19,48,17,6,3,85,4,11,12,10,69,114,108,97,110,103,32,79,84,80,49,20,48,18,6,3,85,4,10,12,11,69,114,105,99,
          115,115,111,110,32,65,66,49,18,48,16,6,3,85,4,7,12,9,83,116,111,99,107,104,111,108,109,49,11,48,9,6,3,85,4,6,19,2,83,69,49,37,48,35,6,9,42,134,72,134,247,13,1,9,1,22,22,112,101,116,101,114,64,101,114,105,120,46,101,114,105,99,115,115,111,110,46,115,101,130,20,118,51,84,213,187,124,136,133,219,84,17,35,72,97,52,24,238,100,168,43,48,33,6,3,85,29,17,4,26,48,24,129,22,112,101,116,101,114,64,101,114,105,120,46,101,114,105,99,115,115,111,110,46,115,101,48,33,6,3,85,29,18,4,26,48,24,129,22,112,101,
          116,101,114,64,101,114,105,120,46,101,114,105,99,115,115,111,110,46,115,101,48,13,6,9,42,134,72,134,247,13,1,1,11,5,0,3,130,1,1,0,44,128,75,220,253,223,223,77,33,57,30,205,101,103,200,211,254,81,122,195,123,239,98,5,118,58,179,193,24,93,12,243,124,194,160,163,206,243,199,49,143,11,73,192,218,193,154,93,146,232,1,191,99,201,129,94,131,59,107,227,216,17,31,101,67,153,177,189,164,194,224,164,78,160,42,79,131,65,37,78,226,201,200,180,128,38,101,164,193,72,82,196,88,204,145,94,235,84,13,243,0,149,
          99,175,203,211,108,177,156,17,27,40,87,195,19,56,39,102,103,42,27,60,30,44,204,157,107,121,128,68,93,216,123,106,112,105,74,7,142,155,171,1,8,31,123,245,78,142,111,142,178,127,169,202,110,125,35,192,199,23,203,201,103,44,99,100,192,156,214,62,109,71,205,66,32,81,252,124,138,238,225,88,247,85,255,65,141,131,234,184,248,20,51,81,71,19,98,102,114,96,49,77,1,79,27,18,218,79,37,232,194,204,172,54,124,167,188,158,43,54,183,230,40,230,152,216,12,27,56,66,104,238,235,52,176,110,159,88,151,7,228,201,
          248,195,82,131,220,31,104,44,239,147,61,71,35,245>>,
    NonceExt = <<4,8,244,183,192,191,230,8,236,82>>,
    ok =
        public_key:pkix_ocsp_validate(Cert, IssuerCert, OcspRespDer, NonceExt, []).

%%--------------------------------------------------------------------
cacerts_load() ->
    [{doc, "Basic tests of cacerts functionality"}].
cacerts_load(Config) ->
    Datadir = proplists:get_value(data_dir, Config),
    {error, enoent} = public_key:cacerts_load("/dummy.file"),

    %% White box testing of paths loading
    %% TestDirs
    ok = pubkey_os_cacerts:load([filename:join(Datadir, "non_existing_dir"),
                                 Datadir,
                                 filename:join(Datadir, "cacerts.pem")
                                ]),
    true = 10 < length(public_key:cacerts_get()),
    %% We currently pick the first found in input order
    ok = pubkey_os_cacerts:load([filename:join(Datadir, "non_existing_file"),
                                 filename:join(Datadir, "ldap_uri_cert.pem"),
                                 filename:join(Datadir, "cacerts.pem")]),
    1 = length(public_key:cacerts_get()),
    ok = pubkey_os_cacerts:load([filename:join(Datadir, "non_existing_file"),
                                 filename:join(Datadir, "cacerts.pem"),
                                 filename:join(Datadir, "ldap_uri_cert.pem")]),
    2 = length(public_key:cacerts_get()),

    true = public_key:cacerts_clear(),

    LinkedCaCerts = filename:join(Datadir, "link_to_cacerts.pem"),
    case file:make_symlink(filename:join(Datadir, "cacerts.pem"), LinkedCaCerts) of
        ok ->
            ok = pubkey_os_cacerts:load([LinkedCaCerts]),
            2 = length(public_key:cacerts_get()),
            true = public_key:cacerts_clear(),
            ok = file:delete(LinkedCaCerts);
        _ ->
            ok
    end,

    %% Load default OS certs
    %%    there is no default installed OS certs on netbsd
    %%    can be installed with 'pkgin install mozilla-rootcerts'
    IsNetBsd = element(2, os:type()) =:= netbsd,
    OsCerts = try
                  Certs = public_key:cacerts_get(),
                  true = public_key:cacerts_clear(),
                  Certs
              catch _:{badmatch, {error, enoent}} when IsNetBsd -> netbsd
              end,

    false = public_key:cacerts_clear(),

    %% Reload from file
    ok = public_key:cacerts_load(filename:join(Datadir, "cacerts.pem")),
    [_TestCert1, _TestCert2] = public_key:cacerts_get(),

    %% Re-Load default OS certs
    try
        process_flag(trap_exit, true),
        flush(),
        ok = public_key:cacerts_load(),
        [] = flush(),
        ct:log("~p: ~p~n", [os:type(), length(OsCerts)]),
        OsCerts = public_key:cacerts_get(),
        Ids = cert_info(OsCerts),
        Check = fun(ShouldBeThere) ->
                        lists:any(fun(#{id:=Id}) -> lists:prefix(ShouldBeThere, Id) end, Ids)
                end,
        case lists:partition(Check, ["digicert", "globalsign"]) of
            {_, []} -> ok;
            {_, Fail} ->
                cert_info(OsCerts),
                [] = Fail
        end,
        ok
    catch _:{badmatch, {error, enoent}} when IsNetBsd ->
            ok
    end.

flush() ->
    receive Msg -> [Msg|flush()]
    after 500 -> []
    end.

cert_info([#cert{der=Der, otp=#'OTPCertificate'{tbsCertificate = C0}=Cert}|Rest]) when is_binary(Der) ->
    #'OTPTBSCertificate'{subject = Subject, serialNumber = _Nr, issuer = Issuer0} = C0,
    C = case public_key:pkix_is_self_signed(Cert) of
            true  -> #{id => subject(Subject), ss => true};
            false ->
                case public_key:pkix_issuer_id(Cert, other) of
                    {ok, {_IsNr, Issuer}} ->
                        #{id => subject(Subject), ss => false, issuer => subject(Issuer)};
                    {error, _} ->
                        #{id => subject(Subject), ss => false, issuer => subject(Issuer0)}
                end
        end,
    [C|cert_info(Rest)];
cert_info([]) ->
    [].


subject(S) ->
    string:lowercase(subject(public_key:pkix_normalize_name(S), "unknown")).

subject({rdnSequence, Seq}, Def) ->
    subject(Seq, Def);
subject([[{'AttributeTypeAndValue', ?'id-at-commonName', Name0}]|_], _Def) ->
    case Name0 of
        {printableString, Name} -> Name;
        {utf8String, Name} -> unicode:characters_to_list(Name);
        Name -> Name
    end;
subject([[{'AttributeTypeAndValue', ?'id-at-organizationName', Name0}]|Rest], _Def) ->
    Name = case Name0 of
               {printableString, Name1} -> Name1;
               {utf8String, Name1} -> unicode:characters_to_list(Name1);
               Name1 -> Name1
           end,
    subject(Rest, Name);
subject([_|R], Def) ->
    subject(R, Def);
subject([], Def) ->
    Def.

list_cacerts() ->
    Certs = public_key:cacerts_get(),
    %% io:format("~P~n",[Certs, 20]),
    IO = fun(C, N) -> io:format("~.3w:~0p~n", [N,C]), N+1 end,
    lists:foldl(IO, 0, lists:sort(cert_info(Certs))),
    ok.


%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
asn1_encode_decode({Asn1Type, Der, not_encrypted} = Entry) ->
    Decoded = public_key:der_decode(Asn1Type, Der),
    Decoded = public_key:pem_entry_decode(Entry),
    Entry = public_key:pem_entry_encode(Asn1Type, Decoded),
    ok.
    
check_countryname({rdnSequence,DirName}) ->
    do_check_countryname(DirName).
do_check_countryname([]) ->
    ok;
do_check_countryname([#'AttributeTypeAndValue'{type = ?'id-at-countryName',
					       value = "US"}|_]) ->
    ok;
do_check_countryname([#'AttributeTypeAndValue'{type = ?'id-at-countryName',
					       value =  Value}|_]) ->
    ct:fail({incorrect_country_name, Value});
do_check_countryname([_| Rest]) ->
    do_check_countryname(Rest).

check_emailaddress({rdnSequence,DirName}) ->
    do_check_emailaddress(DirName).
do_check_emailaddress([]) ->
    ok;
do_check_emailaddress([#'AttributeTypeAndValue'{type = ?'id-emailAddress',
					       value = "invalid@email.com"}|_]) ->
    ok;
do_check_emailaddress([#'AttributeTypeAndValue'{type = ?'id-emailAddress',
					       value =  Value}|_]) ->
    ct:fail({incorrect_email_address, Value});
do_check_emailaddress([_| Rest]) ->
    do_check_emailaddress(Rest).

check_entry_type(#'DSAPrivateKey'{}, 'DSAPrivateKey') ->
    true;
check_entry_type(#'RSAPrivateKey'{}, 'RSAPrivateKey') ->
    true;
check_entry_type(#'RSAPublicKey'{}, 'RSAPublicKey') ->
    true;
check_entry_type({_Int, #'Dss-Parms'{}}, 'DSAPublicKey') when is_integer(_Int) ->
    true;
check_entry_type(#'DHParameter'{}, 'DHParameter') ->
    true;
check_entry_type(#'Certificate'{}, 'Certificate') ->
    true;
check_entry_type({#'ECPoint'{}, _}, 'ECPoint') ->
    true;
check_entry_type(#'ECPrivateKey'{}, 'ECPrivateKey') ->
    true;
check_entry_type({namedCurve, _}, 'EcpkParameters') ->
    true;
check_entry_type({ecParameters, #'ECParameters'{}}, 'EcpkParameters') ->
    true;
check_entry_type(_,_) ->
    false.

check_encapsulated_header(Pem) when is_binary(Pem)->
    check_encapsulated_header( binary:split(Pem, <<"\n">>, [global]));
check_encapsulated_header([<<"DEK-Info: DES-CBC,FB7577791A9056A1">>, <<>> | _]) ->
    true;
check_encapsulated_header([ _ | Rest]) ->
    check_encapsulated_header(Rest);
check_encapsulated_header([]) ->
    false.

strip_superfluous_newlines(Bin) ->
    Str = string:strip(binary_to_list(Bin), right, 10),
    re:replace(Str,"\n\n","\n", [{return,list}, global]).

do_gen_ec_param(File) ->    
    {ok, KeyPem} = file:read_file(File),
    Entries = public_key:pem_decode(KeyPem),
    [ParamInfo] = [Entry || Entry={'EcpkParameters', _, not_encrypted} <- Entries],
    {ecParameters, Params} = public_key:pem_entry_decode(ParamInfo),
    Key = public_key:generate_key(Params),
    case check_entry_type(Key, 'ECPrivateKey') of
        true ->
            ok;
        false ->
            ct:fail({key_gen_fail, File})
    end.

init_per_testcase_gen_ec_param(_TC, Curve, Config) ->
    case crypto:ec_curves() of
        [] ->
            {skip, missing_ec_support};
        Curves ->
            case lists:member(Curve, Curves)
                andalso crypto_supported_curve(Curve, Curves)
            of
                true ->
                    init_common_per_testcase(Config);
                false ->
                    {skip, {missing_ec_support, Curve}}
            end
    end.


crypto_supported_curve(Curve, _Curves) ->
    try crypto:generate_key(ecdh, Curve) of
        {error,_} -> false; % Just in case crypto is changed in the future...
        _-> true
    catch
        _:_-> false
    end.


incorrect_countryname_pkix_cert() ->
    <<48,130,5,186,48,130,4,162,160,3,2,1,2,2,7,7,250,61,63,6,140,137,48,13,6,9,42, 134,72,134,247,13,1,1,5,5,0,48,129,220,49,11,48,9,6,3,85,4,6,19,2,85,83,49, 16,48,14,6,3,85,4,8,19,7,65,114,105,122,111,110,97,49,19,48,17,6,3,85,4,7,19, 10,83,99,111,116,116,115,100,97,108,101,49,37,48,35,6,3,85,4,10,19,28,83,116, 97,114,102,105,101,108,100,32,84,101,99,104,110,111,108,111,103,105,101,115, 44,32,73,110,99,46,49,57,48,55,6,3,85,4,11,19,48,104,116,116,112,58,47,47,99, 101,114,116,105,102,105,99,97,116,101,115,46,115,116,97,114,102,105,101,108, 100,116,101,99,104,46,99,111,109,47,114,101,112,111,115,105,116,111,114,121, 49,49,48,47,6,3,85,4,3,19,40,83,116,97,114,102,105,101,108,100,32,83,101,99, 117,114,101,32,67,101,114,116,105,102,105,99,97,116,105,111,110,32,65,117, 116,104,111,114,105,116,121,49,17,48,15,6,3,85,4,5,19,8,49,48,54,56,56,52,51, 53,48,30,23,13,49,48,49,48,50,51,48,49,51,50,48,53,90,23,13,49,50,49,48,50, 51,48,49,51,50,48,53,90,48,122,49,11,48,9,6,3,85,4,6,12,2,85,83,49,11,48,9,6, 3,85,4,8,12,2,65,90,49,19,48,17,6,3,85,4,7,12,10,83,99,111,116,116,115,100, 97,108,101,49,38,48,36,6,3,85,4,10,12,29,83,112,101,99,105,97,108,32,68,111, 109,97,105,110,32,83,101,114,118,105,99,101,115,44,32,73,110,99,46,49,33,48, 31,6,3,85,4,3,12,24,42,46,108,111,103,105,110,46,115,101,99,117,114,101,115, 101,114,118,101,114,46,110,101,116,48,130,1,34,48,13,6,9,42,134,72,134,247, 13,1,1,1,5,0,3,130,1,15,0,48,130,1,10,2,130,1,1,0,185,136,240,80,141,36,124, 245,182,130,73,19,188,74,166,117,72,228,185,209,43,129,244,40,44,193,231,11, 209,12,234,88,43,142,1,162,48,122,17,95,230,105,171,131,12,147,46,204,36,80, 250,171,33,253,35,62,83,22,71,212,186,141,14,198,89,89,121,204,224,122,246, 127,110,188,229,162,67,95,6,74,231,127,99,131,7,240,85,102,203,251,50,58,58, 104,245,103,181,183,134,32,203,121,232,54,32,188,139,136,112,166,126,14,91, 223,153,172,164,14,61,38,163,208,215,186,210,136,213,143,70,147,173,109,217, 250,169,108,31,211,104,238,103,93,182,59,165,43,196,189,218,241,30,148,240, 109,90,69,176,194,52,116,173,151,135,239,10,209,179,129,192,102,75,11,25,168, 223,32,174,84,223,134,70,167,55,172,143,27,130,123,226,226,7,34,142,166,39, 48,246,96,231,150,84,220,106,133,193,55,95,159,227,24,249,64,36,1,142,171,16, 202,55,126,7,156,15,194,22,116,53,113,174,104,239,203,120,45,131,57,87,84, 163,184,27,83,57,199,91,200,34,43,98,61,180,144,76,65,170,177,2,3,1,0,1,163, 130,1,224,48,130,1,220,48,15,6,3,85,29,19,1,1,255,4,5,48,3,1,1,0,48,29,6,3, 85,29,37,4,22,48,20,6,8,43,6,1,5,5,7,3,1,6,8,43,6,1,5,5,7,3,2,48,14,6,3,85, 29,15,1,1,255,4,4,3,2,5,160,48,56,6,3,85,29,31,4,49,48,47,48,45,160,43,160, 41,134,39,104,116,116,112,58,47,47,99,114,108,46,115,116,97,114,102,105,101, 108,100,116,101,99,104,46,99,111,109,47,115,102,115,50,45,48,46,99,114,108, 48,83,6,3,85,29,32,4,76,48,74,48,72,6,11,96,134,72,1,134,253,110,1,7,23,2,48, 57,48,55,6,8,43,6,1,5,5,7,2,1,22,43,104,116,116,112,115,58,47,47,99,101,114, 116,115,46,115,116,97,114,102,105,101,108,100,116,101,99,104,46,99,111,109, 47,114,101,112,111,115,105,116,111,114,121,47,48,129,141,6,8,43,6,1,5,5,7,1, 1,4,129,128,48,126,48,42,6,8,43,6,1,5,5,7,48,1,134,30,104,116,116,112,58,47, 47,111,99,115,112,46,115,116,97,114,102,105,101,108,100,116,101,99,104,46,99, 111,109,47,48,80,6,8,43,6,1,5,5,7,48,2,134,68,104,116,116,112,58,47,47,99, 101,114,116,105,102,105,99,97,116,101,115,46,115,116,97,114,102,105,101,108, 100,116,101,99,104,46,99,111,109,47,114,101,112,111,115,105,116,111,114,121, 47,115,102,95,105,110,116,101,114,109,101,100,105,97,116,101,46,99,114,116, 48,31,6,3,85,29,35,4,24,48,22,128,20,73,75,82,39,209,27,188,242,161,33,106, 98,123,81,66,122,138,215,213,86,48,59,6,3,85,29,17,4,52,48,50,130,24,42,46, 108,111,103,105,110,46,115,101,99,117,114,101,115,101,114,118,101,114,46,110, 101,116,130,22,108,111,103,105,110,46,115,101,99,117,114,101,115,101,114,118, 101,114,46,110,101,116,48,29,6,3,85,29,14,4,22,4,20,138,233,191,208,157,203, 249,85,242,239,20,195,48,10,148,49,144,101,255,116,48,13,6,9,42,134,72,134, 247,13,1,1,5,5,0,3,130,1,1,0,82,31,121,162,49,50,143,26,167,202,143,61,71, 189,201,199,57,81,122,116,90,192,88,24,102,194,174,48,157,74,27,87,210,223, 253,93,3,91,150,109,120,1,110,27,11,200,198,141,222,246,14,200,71,105,41,138, 13,114,122,106,63,17,197,181,234,121,61,89,74,65,41,231,248,219,129,83,176, 219,55,107,55,211,112,98,38,49,69,77,96,221,108,123,152,12,210,159,157,141, 43,226,55,187,129,3,82,49,136,66,81,196,91,234,196,10,82,48,6,80,163,83,71, 127,102,177,93,209,129,26,104,2,84,24,255,248,161,3,244,169,234,92,122,110, 43,4,17,113,185,235,108,219,210,236,132,216,177,227,17,169,58,162,159,182, 162,93,160,229,200,9,163,229,110,121,240,168,232,14,91,214,188,196,109,210, 164,222,0,109,139,132,113,91,16,118,173,178,176,80,132,34,41,199,51,206,250, 224,132,60,115,192,94,107,163,219,212,226,225,65,169,148,108,213,46,174,173, 103,110,189,229,166,149,254,31,51,44,144,108,187,182,11,251,201,206,86,138, 208,59,51,86,132,235,81,225,88,34,190,8,184>>.

incorrect_emailaddress_pkix_cert() ->
    <<48,130,3,74,48,130,2,50,2,9,0,133,49,203,25,198,156,252,230,48,13,6,9,42,134, 72,134,247,13,1,1,5,5,0,48,103,49,11,48,9,6,3,85,4,6,19,2,65,85,49,19,48,17, 6,3,85,4,8,12,10,83,111,109,101,45,83,116,97,116,101,49,33,48,31,6,3,85,4,10, 12,24,73,110,116,101,114,110,101,116,32,87,105,100,103,105,116,115,32,80,116, 121,32,76,116,100,49,32,48,30,6,9,42,134,72,134,247,13,1,9,1,12,17,105,110, 118,97,108,105,100,64,101,109,97,105,108,46,99,111,109,48,30,23,13,49,51,49, 49,48,55,50,48,53,54,49,56,90,23,13,49,52,49,49,48,55,50,48,53,54,49,56,90, 48,103,49,11,48,9,6,3,85,4,6,19,2,65,85,49,19,48,17,6,3,85,4,8,12,10,83,111, 109,101,45,83,116,97,116,101,49,33,48,31,6,3,85,4,10,12,24,73,110,116,101, 114,110,101,116,32,87,105,100,103,105,116,115,32,80,116,121,32,76,116,100,49, 32,48,30,6,9,42,134,72,134,247,13,1,9,1,12,17,105,110,118,97,108,105,100,64, 101,109,97,105,108,46,99,111,109,48,130,1,34,48,13,6,9,42,134,72,134,247,13, 1,1,1,5,0,3,130,1,15,0,48,130,1,10,2,130,1,1,0,190,243,49,213,219,60,232,105, 1,127,126,9,130,15,60,190,78,100,148,235,246,223,21,91,238,200,251,84,55,212, 78,32,120,61,85,172,0,144,248,5,165,29,143,79,64,178,51,153,203,76,115,238, 192,49,173,37,121,203,89,62,157,13,181,166,30,112,154,40,202,140,104,211,157, 73,244,9,78,236,70,153,195,158,233,141,42,238,2,143,160,225,249,27,30,140, 151,176,43,211,87,114,164,108,69,47,39,195,123,185,179,219,28,218,122,53,83, 77,48,81,184,14,91,243,12,62,146,86,210,248,228,171,146,225,87,51,146,155, 116,112,238,212,36,111,58,41,67,27,6,61,61,3,84,150,126,214,121,57,38,12,87, 121,67,244,37,45,145,234,131,115,134,58,194,5,36,166,52,59,229,32,47,152,80, 237,190,58,182,248,98,7,165,198,211,5,31,231,152,116,31,108,71,218,64,188, 178,143,27,167,79,15,112,196,103,116,212,65,197,94,37,4,132,103,91,217,73, 223,207,185,7,153,221,240,232,31,44,102,108,82,83,56,242,210,214,74,71,246, 177,217,148,227,220,230,4,176,226,74,194,37,2,3,1,0,1,48,13,6,9,42,134,72, 134,247,13,1,1,5,5,0,3,130,1,1,0,89,247,141,154,173,123,123,203,143,85,28,79, 73,37,164,6,17,89,171,224,149,22,134,17,198,146,158,192,241,41,253,58,230, 133,71,189,43,66,123,88,15,242,119,227,249,99,137,61,200,54,161,0,177,167, 169,114,80,148,90,22,97,78,162,181,75,93,209,116,245,46,81,232,64,157,93,136, 52,57,229,113,197,218,113,93,42,161,213,104,205,137,30,144,183,58,10,98,47, 227,177,96,40,233,98,150,209,217,68,22,221,133,27,161,152,237,46,36,179,59, 172,97,134,194,205,101,137,71,192,57,153,20,114,27,173,233,166,45,56,0,61, 205,45,202,139,7,132,103,248,193,157,184,123,43,62,172,236,110,49,62,209,78, 249,83,219,133,1,213,143,73,174,16,113,143,189,41,84,60,128,222,30,177,104, 134,220,52,239,171,76,59,176,36,113,176,214,118,16,44,235,21,167,199,216,200, 76,219,142,248,13,70,145,205,216,230,226,148,97,223,216,179,68,209,222,63, 140,137,24,164,192,149,194,79,119,247,75,159,49,116,70,241,70,116,11,40,119, 176,157,36,160,102,140,255,34,248,25,231,136,59>>.

hardcode_rsa_key(1) ->
    #'RSAPrivateKey'{
       version = 'two-prime',
       modulus = 23995666614853919027835084074500048897452890537492185072956789802729257783422306095699263934587064480357348855732149402060270996295002843755712064937715826848741191927820899197493902093529581182351132392364214171173881547273475904587683433713767834856230531387991145055273426806331200574039205571401702219159773947658558490957010003143162250693492642996408861265758000254664396313741422909188635443907373976005987612936763564996605457102336549804831742940035613780926178523017685712710473543251580072875247250504243621640157403744718833162626193206685233710319205099867303242759099560438381385658382486042995679707669,
       publicExponent = 17,
       privateExponent = 11292078406990079542510627799764728892919007311761028269626724613049062486316379339152594792746853873109340637991599718616598115903530750002688030558925094987642913848386305504703012749896273497577003478759630198199473669305165131570674557041773098755873191241407597673069847908861741446606684974777271632545629600685952292605647052193819136445675100211504432575554351515262198132231537860917084269870590492135731720141577986787033006338680118008484613510063003323516659048210893001173583018220214626635609151105287049126443102976056146630518124476470236027123782297108342869049542023328584384300970694412006494684657,
       prime1 = 169371138592582642967021557955633494538845517070305333860805485424261447791289944610138334410987654265476540480228705481960508520379619587635662291973699651583489223555422528867090299996446070521801757353675026048850480903160224210802452555900007597342687137394192939372218903554801584969667104937092080815197,
       prime2 = 141675062317286527042995673340952251894209529891636708844197799307963834958115010129693036021381525952081167155681637592199810112261679449166276939178032066869788822014115556349519329537177920752776047051833616197615329017439297361972726138285974555338480581117881706656603857310337984049152655480389797687577,
       exponent1 = 119556097830058336212015217380447172615655659108450823901745048534772786676204666783627059584226579481512852103690850928442711896738555003036938088452023283470698275450886490965004917644550167427154181661417665446247398284583687678213495921811770068712485038160606780733330990744565824684470897602653233516609,
       exponent2 = 41669135975672507953822256864985956439473391144599032012999352737636422046504414744027363535700448809435637398729893409470532385959317485048904982111185902020526124121798693043976273393287623750816484427009887116945685005129205106462566511260580751570141347387612266663707016855981760014456663376585234613993,
       coefficient = 76837684977089699359024365285678488693966186052769523357232308621548155587515525857011429902602352279058920284048929101483304120686557782043616693940283344235057989514310975192908256494992960578961614059245280827077951132083993754797053182279229469590276271658395444955906108899267024101096069475145863928441,
       otherPrimeInfos = asn1_NOVALUE};

hardcode_rsa_key(2) ->
    #'RSAPrivateKey'{
       version = 'two-prime',
       modulus = 21343679768589700771839799834197557895311746244621307033143551583788179817796325695589283169969489517156931770973490560582341832744966317712674900833543896521418422508485833901274928542544381247956820115082240721897193055368570146764204557110415281995205343662628196075590438954399631753508888358737971039058298703003743872818150364935790613286541190842600031570570099801682794056444451081563070538409720109449780410837763602317050353477918147758267825417201591905091231778937606362076129350476690460157227101296599527319242747999737801698427160817755293383890373574621116766934110792127739174475029121017282777887777,
       publicExponent = 17,
       privateExponent = 18832658619343853622211588088997845201745658451136447382185486691577805721584993260814073385267196632785528033211903435807948675951440868570007265441362261636545666919252206383477878125774454042314841278013741813438699754736973658909592256273895837054592950290554290654932740253882028017801960316533503857992358685308186680144968293076156011747178275038098868263178095174694099811498968993700538293188879611375604635940554394589807673542938082281934965292051746326331046224291377703201248790910007232374006151098976879987912446997911775904329728563222485791845480864283470332826504617837402078265424772379987120023773,
       prime1 = 146807662748886761089048448970170315054939768171908279335181627815919052012991509112344782731265837727551849787333310044397991034789843793140419387740928103541736452627413492093463231242466386868459637115999163097726153692593711599245170083315894262154838974616739452594203727376460632750934355508361223110419,
       prime2 = 145385325050081892763917667176962991350872697916072592966410309213561884732628046256782356731057378829876640317801978404203665761131810712267778698468684631707642938779964806354584156202882543264893826268426566901882487709510744074274965029453915224310656287149777603803201831202222853023280023478269485417083,
       exponent1 = 51814469205489445090252393754177758254684624060673510353593515699736136004585238510239335081623236845018299924941168250963996835808180162284853901555621683602965806809675350150634081614988136541809283687999704622726877773856604093851236499993845033701707873394143336209718962603456693912094478414715725803677,
       exponent2 = 51312467664734785681382706062457526359131540440966797517556579722433606376221663384746714140373192528191755406283051201483646739222992016094510128871300458249756331334105225772206172777487956446433115153562317730076172132768497908567634716277852432109643395464627389577600646306666889302334125933506877206029,
       coefficient = 30504662229874176232343608562807118278893368758027179776313787938167236952567905398252901545019583024374163153775359371298239336609182249464886717948407152570850677549297935773605431024166978281486607154204888016179709037883348099374995148481968169438302456074511782717758301581202874062062542434218011141540,
       otherPrimeInfos = asn1_NOVALUE};
hardcode_rsa_key(3) -> 
    #'RSAPrivateKey'{ 
       version = 'two-prime',
       modulus = 25089040456112869869472694987833070928503703615633809313972554887193090845137746668197820419383804666271752525807484521370419854590682661809972833718476098189250708650325307850184923546875260207894844301992963978994451844985784504212035958130279304082438876764367292331581532569155681984449177635856426023931875082020262146075451989132180409962870105455517050416234175675478291534563995772675388370042873175344937421148321291640477650173765084699931690748536036544188863178325887393475703801759010864779559318631816411493486934507417755306337476945299570726975433250753415110141783026008347194577506976486290259135429,
       publicExponent = 17,
       privateExponent = 8854955455098659953931539407470495621824836570223697404931489960185796768872145882893348383311931058684147950284994536954265831032005645344696294253579799360912014817761873358888796545955974191021709753644575521998041827642041589721895044045980930852625485916835514940558187965584358347452650930302268008446431977397918214293502821599497633970075862760001650736520566952260001423171553461362588848929781360590057040212831994258783694027013289053834376791974167294527043946669963760259975273650548116897900664646809242902841107022557239712438496384819445301703021164043324282687280801738470244471443835900160721870265,
       prime1 = 171641816401041100605063917111691927706183918906535463031548413586331728772311589438043965564336865070070922328258143588739626712299625805650832695450270566547004154065267940032684307994238248203186986569945677705100224518137694769557564475390859269797990555863306972197736879644001860925483629009305104925823,
       prime2 =146170909759497809922264016492088453282310383272504533061020897155289106805616042710009332510822455269704884883705830985184223718261139908416790475825625309815234508695722132706422885088219618698987115562577878897003573425367881351537506046253616435685549396767356003663417208105346307649599145759863108910523,
       exponent1 = 60579464612132153154728441333538327425711971378777222246428851853999433684345266860486105493295364142377972586444050678378691780811632637288529186629507258781295583787741625893888579292084087601124818789392592131211843947578009918667375697196773859928702549128225990187436545756706539150170692591519448797349,
       exponent2 = 137572620950115585809189662580789132500998007785886619351549079675566218169991569609420548245479957900898715184664311515467504676010484619686391036071176762179044243478326713135456833024206699951987873470661533079532774988581535389682358631768109586527575902839864474036157372334443583670210960715165278974609,
       coefficient = 15068630434698373319269196003209754243798959461311186548759287649485250508074064775263867418602372588394608558985183294561315208336731894947137343239541687540387209051236354318837334154993136528453613256169847839789803932725339395739618592522865156272771578671216082079933457043120923342632744996962853951612,
       otherPrimeInfos = asn1_NOVALUE};
hardcode_rsa_key(4) ->
    #'RSAPrivateKey'{
       version ='two-prime',
       modulus = 28617237755030755643854803617273584643843067580642149032833640135949799721163782522787597288521902619948688786051081993247908700824196122780349730169173433743054172191054872553484065655968335396052034378669869864779940355219732200954630251223541048434478476115391643898092650304645086338265930608997389611376417609043761464100338332976874588396803891301015812818307951159858145399281035705713082131199940309445719678087542976246147777388465712394062188801177717719764254900022006288880246925156931391594131839991579403409541227225173269459173129377291869028712271737734702830877034334838181789916127814298794576266389,
       publicExponent = 17,
       privateExponent = 26933870828264240605980991639786903194205240075898493207372837775011576208154148256741268036255908348187001210401018346586267012540419880263858569570986761169933338532757527109161473558558433313931326474042230460969355628442100895016122589386862163232450330461545076609969553227901257730132640573174013751883368376011370428995523268034111482031427024082719896108094847702954695363285832195666458915142143884210891427766607838346722974883433132513540317964796373298134261669479023445911856492129270184781873446960437310543998533283339488055776892320162032014809906169940882070478200435536171854883284366514852906334641,
       prime1 = 177342190816702392178883147766999616783253285436834252111702533617098994535049411784501174309695427674025956656849179054202187436663487378682303508229883753383891163725167367039879190685255046547908384208614573353917213168937832054054779266431207529839577747601879940934691505396807977946728204814969824442867,
       prime2 = 161367340863680900415977542864139121629424927689088951345472941851682581254789586032968359551717004797621579428672968948552429138154521719743297455351687337112710712475376510559020211584326773715482918387500187602625572442687231345855402020688502483137168684570635690059254866684191216155909970061793538842967,
       exponent1 = 62591361464718491357252875682470452982324688977706206627659717747211409835899792394529826226951327414362102349476180842659595565881230839534930649963488383547255704844176717778780890830090016428673547367746320007264898765507470136725216211681602657590439205035957626212244060728285168687080542875871702744541,
       exponent2 = 28476589564178982426348978152495139111074987239250991413906989738532220221433456358759122273832412611344984605059935696803369847909621479954699550944415412431654831613301737157474154985469430655673456186029444871051571607533040825739188591886206320553618003159523945304574388238386685203984112363845918619347,
       coefficient = 34340318160575773065401929915821192439103777558577109939078671096408836197675640654693301707202885840826672396546056002756167635035389371579540325327619480512374920136684787633921441576901246290213545161954865184290700344352088099063404416346968182170720521708773285279884132629954461545103181082503707725012,
       otherPrimeInfos = asn1_NOVALUE}.

pss_params(sha256) ->
    #'RSASSA-PSS-params'{
       hashAlgorithm = #'HashAlgorithm'{algorithm = ?'id-sha256'},
       maskGenAlgorithm = #'MaskGenAlgorithm'{algorithm = ?'id-mgf1',
                                              parameters = #'HashAlgorithm'{algorithm = ?'id-sha256'}
                                             },
       saltLength = 32,
       trailerField = 1}.
