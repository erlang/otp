%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(TIMEOUT, 120000). % 2 min


%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() -> 
    [].

all() -> 
    [app, appup,
     {group, pem_decode_encode},
     {group, ssh_public_key_decode_encode},
     encrypt_decrypt,
     {group, sign_verify},
     pkix, pkix_countryname, pkix_emailaddress, pkix_path_validation,
     pkix_iso_rsa_oid, pkix_iso_dsa_oid, 
     pkix_dsa_sha2_oid,
     pkix_crl, general_name,
     pkix_verify_hostname_cn,
     pkix_verify_hostname_subjAltName,
     pkix_verify_hostname_subjAltName_IP,
     pkix_verify_hostname_options,
     pkix_test_data_all_default,
     pkix_test_data,
     short_cert_issuer_hash, short_crl_issuer_hash,
     ssh_hostkey_fingerprint_md5_implicit,
     ssh_hostkey_fingerprint_md5,
     ssh_hostkey_fingerprint_sha,
     ssh_hostkey_fingerprint_sha256,
     ssh_hostkey_fingerprint_sha384,
     ssh_hostkey_fingerprint_sha512,
     ssh_hostkey_fingerprint_list
    ].

groups() -> 
    [{pem_decode_encode, [], [dsa_pem, rsa_pem, ec_pem, encrypted_pem,
			      dh_pem, cert_pem, pkcs7_pem, pkcs10_pem, ec_pem2,
			      rsa_priv_pkcs8, dsa_priv_pkcs8, ec_priv_pkcs8,
                              ec_pem_encode_generated,
                              gen_ec_param_prime_field, gen_ec_param_char_2_field
                             ]},
     {ssh_public_key_decode_encode, [],
      [ssh_rsa_public_key, ssh_dsa_public_key, ssh_ecdsa_public_key,
       ssh_rfc4716_rsa_comment, ssh_rfc4716_dsa_comment,
       ssh_rfc4716_rsa_subject,
       ssh_known_hosts,
       ssh_auth_keys, ssh1_known_hosts, ssh1_auth_keys, ssh_openssh_public_key_with_comment,
       ssh_openssh_public_key_long_header]},
     {sign_verify, [], [rsa_sign_verify, dsa_sign_verify]}
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

init_per_testcase(TestCase, Config) ->
    case TestCase of
	ssh_hostkey_fingerprint_md5_implicit -> init_fingerprint_testcase([md5], Config);
	ssh_hostkey_fingerprint_md5 ->    init_fingerprint_testcase([md5], Config);
	ssh_hostkey_fingerprint_sha ->    init_fingerprint_testcase([sha], Config);
	ssh_hostkey_fingerprint_sha256 -> init_fingerprint_testcase([sha256], Config);
	ssh_hostkey_fingerprint_sha384 -> init_fingerprint_testcase([sha384], Config);
	ssh_hostkey_fingerprint_sha512 -> init_fingerprint_testcase([sha512], Config);
	ssh_hostkey_fingerprint_list   -> init_fingerprint_testcase([sha,md5], Config);
    ec_pem_encode_generated        -> init_ec_pem_encode_generated(Config);
	_ -> init_common_per_testcase(Config)
    end.
	
init_fingerprint_testcase(Algs, Config) ->
    Hashs = proplists:get_value(hashs, crypto:supports(), []),
    case Algs -- Hashs of
        [] -> init_common_per_testcase(Config);
        UnsupportedAlgs ->  {skip,{UnsupportedAlgs,not_supported}}
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
    ok = ?t:app_test(public_key).

%%--------------------------------------------------------------------

appup() ->
    [{doc, "Test that the public_key appup file is ok"}].
appup(Config) when is_list(Config) ->
    ok = ?t:appup_test(public_key).

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

encrypted_pem() ->
    [{doc, "Encrypted PEM-file decode/encode"}].
encrypted_pem(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    [{'RSAPrivateKey', DerRSAKey, not_encrypted}] =
	erl_make_certs:pem_to_der(filename:join(Datadir, "client_key.pem")),

    RSAKey = public_key:der_decode('RSAPrivateKey', DerRSAKey),

    Salt0 = crypto:strong_rand_bytes(8),
    Entry0 = public_key:pem_entry_encode('RSAPrivateKey', RSAKey,
					 {{"DES-EDE3-CBC", Salt0}, "1234abcd"}),
    RSAKey = public_key:pem_entry_decode(Entry0,"1234abcd"),
    Des3KeyFile = filename:join(Datadir, "des3_client_key.pem"),
    erl_make_certs:der_to_pem(Des3KeyFile, [Entry0]),
    [{'RSAPrivateKey', _, {"DES-EDE3-CBC", Salt0}}] =
	erl_make_certs:pem_to_der(Des3KeyFile),

    Salt1 = crypto:strong_rand_bytes(8),
    Entry1 = public_key:pem_entry_encode('RSAPrivateKey', RSAKey,
					   {{"DES-CBC", Salt1}, "4567efgh"}),
    DesKeyFile = filename:join(Datadir, "des_client_key.pem"),
    erl_make_certs:der_to_pem(DesKeyFile, [Entry1]),
    [{'RSAPrivateKey', _, {"DES-CBC", Salt1}} =Entry2] =
	erl_make_certs:pem_to_der(DesKeyFile),
    {ok, Pem} = file:read_file(DesKeyFile),
    check_encapsulated_header(Pem),
    true = check_entry_type(public_key:pem_entry_decode(Entry2, "4567efgh"),
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
ssh_rsa_public_key() ->
    [{doc, "ssh rsa public key decode/encode"}].
ssh_rsa_public_key(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, RSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_rsa_pub")),
    [{PubKey, Attributes1}] = public_key:ssh_decode(RSARawSsh2, public_key),
    [{PubKey, Attributes1}] = public_key:ssh_decode(RSARawSsh2, rfc4716_public_key),

    {ok, RSARawOpenSsh} = file:read_file(filename:join(Datadir, "openssh_rsa_pub")),
    [{PubKey, Attributes2}] = public_key:ssh_decode(RSARawOpenSsh, public_key),
    [{PubKey, Attributes2}] = public_key:ssh_decode(RSARawOpenSsh, openssh_public_key),

    %% Can not check EncodedSSh == RSARawSsh2 and EncodedOpenSsh
    %% = RSARawOpenSsh as line breakpoints may differ

    EncodedSSh = public_key:ssh_encode([{PubKey, Attributes1}], rfc4716_public_key),
    EncodedOpenSsh = public_key:ssh_encode([{PubKey, Attributes2}], openssh_public_key),

    [{PubKey, Attributes1}] =
	public_key:ssh_decode(EncodedSSh, public_key),
    [{PubKey, Attributes2}] =
	public_key:ssh_decode(EncodedOpenSsh, public_key).

%%--------------------------------------------------------------------

ssh_dsa_public_key() ->
    [{doc, "ssh dsa public key decode/encode"}].
ssh_dsa_public_key(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, DSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_dsa_pub")),
    [{PubKey, Attributes1}] = public_key:ssh_decode(DSARawSsh2, public_key),
    [{PubKey, Attributes1}] = public_key:ssh_decode(DSARawSsh2, rfc4716_public_key),

    {ok, DSARawOpenSsh} = file:read_file(filename:join(Datadir, "openssh_dsa_pub")),
    [{PubKey, Attributes2}] = public_key:ssh_decode(DSARawOpenSsh, public_key),
    [{PubKey, Attributes2}] = public_key:ssh_decode(DSARawOpenSsh, openssh_public_key),

    %% Can not check EncodedSSh == DSARawSsh2 and EncodedOpenSsh
    %% = DSARawOpenSsh as line breakpoints may differ

    EncodedSSh = public_key:ssh_encode([{PubKey, Attributes1}], rfc4716_public_key),
    EncodedOpenSsh = public_key:ssh_encode([{PubKey, Attributes2}], openssh_public_key),

    [{PubKey, Attributes1}] =
	public_key:ssh_decode(EncodedSSh, public_key),
    [{PubKey, Attributes2}] =
	public_key:ssh_decode(EncodedOpenSsh, public_key).

%%--------------------------------------------------------------------

ssh_ecdsa_public_key() ->
    [{doc, "ssh ecdsa public key decode/encode"}].
ssh_ecdsa_public_key(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, ECDSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_ecdsa_pub")),
    [{PubKey, Attributes1}] = public_key:ssh_decode(ECDSARawSsh2, public_key),
    [{PubKey, Attributes1}] = public_key:ssh_decode(ECDSARawSsh2, rfc4716_public_key),

    {ok, ECDSARawOpenSsh} = file:read_file(filename:join(Datadir, "openssh_ecdsa_pub")),
    [{PubKey, Attributes2}] = public_key:ssh_decode(ECDSARawOpenSsh, public_key),
    [{PubKey, Attributes2}] = public_key:ssh_decode(ECDSARawOpenSsh, openssh_public_key),

    %% Can not check EncodedSSh == ECDSARawSsh2 and EncodedOpenSsh
    %% = ECDSARawOpenSsh as line breakpoints may differ

    EncodedSSh = public_key:ssh_encode([{PubKey, Attributes1}], rfc4716_public_key),
    EncodedOpenSsh = public_key:ssh_encode([{PubKey, Attributes2}], openssh_public_key),

    [{PubKey, Attributes1}] =
	public_key:ssh_decode(EncodedSSh, public_key),
    [{PubKey, Attributes2}] =
	public_key:ssh_decode(EncodedOpenSsh, public_key).

%%--------------------------------------------------------------------
ssh_rfc4716_rsa_comment() ->
    [{doc, "Test comment header and rsa key"}].
ssh_rfc4716_rsa_comment(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, RSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_rsa_comment_pub")),
    [{#'RSAPublicKey'{} = PubKey, Attributes}] =
        public_key:ssh_decode(RSARawSsh2, public_key),

    Headers = proplists:get_value(headers, Attributes),

    Value = proplists:get_value("Comment", Headers, undefined),
    true = Value =/= undefined,
    RSARawSsh2 = public_key:ssh_encode([{PubKey, Attributes}], rfc4716_public_key).

%%--------------------------------------------------------------------
ssh_rfc4716_dsa_comment() ->
     [{doc, "Test comment header and dsa key"}].
ssh_rfc4716_dsa_comment(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, DSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_dsa_comment_pub")),
    [{{_, #'Dss-Parms'{}} = PubKey, Attributes}] =
        public_key:ssh_decode(DSARawSsh2, public_key),

    Headers = proplists:get_value(headers, Attributes),

    Value = proplists:get_value("Comment", Headers, undefined),
    true = Value =/= undefined,

    %% Can not check Encoded == DSARawSsh2 as line continuation breakpoints may differ
    Encoded  = public_key:ssh_encode([{PubKey, Attributes}], rfc4716_public_key),
    [{PubKey, Attributes}] =
        public_key:ssh_decode(Encoded, public_key).

%%--------------------------------------------------------------------
ssh_rfc4716_rsa_subject() ->
    [{doc,  "Test another header value than comment"}].
ssh_rfc4716_rsa_subject(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, RSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_subject_pub")),
    [{#'RSAPublicKey'{} = PubKey, Attributes}] =
        public_key:ssh_decode(RSARawSsh2, public_key),

    Headers = proplists:get_value(headers, Attributes),

    Value = proplists:get_value("Subject", Headers, undefined),
    true = Value =/= undefined,

    %% Can not check Encoded == RSARawSsh2 as line continuation breakpoints may differ
    Encoded  = public_key:ssh_encode([{PubKey, Attributes}], rfc4716_public_key),
    [{PubKey, Attributes}] =
        public_key:ssh_decode(Encoded, public_key).

%%--------------------------------------------------------------------
ssh_known_hosts() ->
    [{doc, "ssh known hosts file encode/decode"}].
ssh_known_hosts(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, SshKnownHosts} = file:read_file(filename:join(Datadir, "known_hosts")),
    [{#'RSAPublicKey'{}, Attributes1}, {#'RSAPublicKey'{}, Attributes2},
     {#'RSAPublicKey'{}, Attributes3}, {#'RSAPublicKey'{}, Attributes4}] = Decoded =
        public_key:ssh_decode(SshKnownHosts, known_hosts),

    Comment1 = undefined,
    Comment2 = "foo@bar.com",
    Comment3 = "Comment with whitespaces",
    Comment4 = "foo@bar.com Comment with whitespaces",
    	
    Comment1 = proplists:get_value(comment, Attributes1, undefined),
    Comment2 = proplists:get_value(comment, Attributes2),
    Comment3 = proplists:get_value(comment, Attributes3),
    Comment4 = proplists:get_value(comment, Attributes4),	

    Value1 = proplists:get_value(hostnames, Attributes1, undefined),
    Value2 = proplists:get_value(hostnames, Attributes2, undefined),
    true = (Value1 =/= undefined) and (Value2 =/= undefined),

    Encoded = public_key:ssh_encode(Decoded, known_hosts),
    Decoded = public_key:ssh_decode(Encoded, known_hosts).

%%--------------------------------------------------------------------

ssh1_known_hosts() ->
    [{doc, "ssh (ver 1) known hosts file encode/decode"}].
ssh1_known_hosts(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, SshKnownHosts} = file:read_file(filename:join(Datadir, "ssh1_known_hosts")),
    [{#'RSAPublicKey'{}, Attributes1}, {#'RSAPublicKey'{}, Attributes2},{#'RSAPublicKey'{}, Attributes3}] 
	= Decoded = public_key:ssh_decode(SshKnownHosts, known_hosts),

    Value1 = proplists:get_value(hostnames, Attributes1, undefined),
    Value2 = proplists:get_value(hostnames, Attributes2, undefined),
    true = (Value1 =/= undefined) and (Value2 =/= undefined),

    Comment ="dhopson@VMUbuntu-DSH comment with whitespaces",
    Comment = proplists:get_value(comment, Attributes3),

    Encoded = public_key:ssh_encode(Decoded, known_hosts),
    Decoded = public_key:ssh_decode(Encoded, known_hosts).

%%--------------------------------------------------------------------
ssh_auth_keys() ->
    [{doc, "ssh authorized keys file encode/decode"}].
ssh_auth_keys(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, SshAuthKeys} = file:read_file(filename:join(Datadir, "auth_keys")),
    [{#'RSAPublicKey'{}, Attributes1}, {{_, #'Dss-Parms'{}}, Attributes2},
     {#'RSAPublicKey'{}, Attributes3}, {{_, #'Dss-Parms'{}}, Attributes4}
    ] = Decoded =
        public_key:ssh_decode(SshAuthKeys, auth_keys),

    Value1 = proplists:get_value(options, Attributes1, undefined),
    true = Value1 =/= undefined,

    Comment1 = Comment2 = "dhopson@VMUbuntu-DSH",
    Comment3 = Comment4 ="dhopson@VMUbuntu-DSH comment with whitespaces",
    
    Comment1 = proplists:get_value(comment, Attributes1),
    Comment2 = proplists:get_value(comment, Attributes2),
    Comment3 = proplists:get_value(comment, Attributes3),
    Comment4 = proplists:get_value(comment, Attributes4),

    Encoded = public_key:ssh_encode(Decoded, auth_keys),
    Decoded = public_key:ssh_decode(Encoded, auth_keys).

%%--------------------------------------------------------------------
ssh1_auth_keys() ->
    [{doc, "ssh (ver 1) authorized keys file encode/decode"}].
ssh1_auth_keys(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, SshAuthKeys} = file:read_file(filename:join(Datadir, "ssh1_auth_keys")),
    [{#'RSAPublicKey'{}, Attributes1},
     {#'RSAPublicKey'{}, Attributes2}, {#'RSAPublicKey'{}, Attributes3},
     {#'RSAPublicKey'{}, Attributes4}, {#'RSAPublicKey'{}, Attributes5}] = Decoded =
        public_key:ssh_decode(SshAuthKeys, auth_keys),

    Value1 = proplists:get_value(bits, Attributes2, undefined),
    Value2 = proplists:get_value(bits, Attributes3, undefined),
    true = (Value1 =/= undefined) and (Value2 =/= undefined),

    Comment2 = Comment3 = "dhopson@VMUbuntu-DSH",
    Comment4 = Comment5 ="dhopson@VMUbuntu-DSH comment with whitespaces",
    
    undefined = proplists:get_value(comment, Attributes1, undefined),
    Comment2 = proplists:get_value(comment, Attributes2),
    Comment3 = proplists:get_value(comment, Attributes3),
    Comment4 = proplists:get_value(comment, Attributes4),
    Comment5 = proplists:get_value(comment, Attributes5),

    Encoded = public_key:ssh_encode(Decoded, auth_keys),
    Decoded = public_key:ssh_decode(Encoded, auth_keys).

%%--------------------------------------------------------------------
ssh_openssh_public_key_with_comment() ->
    [{doc, "Test that emty lines and lines starting with # are ignored"}].
ssh_openssh_public_key_with_comment(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, DSARawOpenSsh} = file:read_file(filename:join(Datadir, "openssh_dsa_with_comment_pub")),
    [{{_, #'Dss-Parms'{}}, _}] = public_key:ssh_decode(DSARawOpenSsh, openssh_public_key).

%%--------------------------------------------------------------------
ssh_openssh_public_key_long_header() ->
  [{doc, "Test that long headers are handled"}].
ssh_openssh_public_key_long_header(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok,RSARawOpenSsh} = file:read_file(filename:join(Datadir, "ssh_rsa_long_header_pub")),
    [{#'RSAPublicKey'{}, _}] = Decoded = public_key:ssh_decode(RSARawOpenSsh, public_key),

    Encoded = public_key:ssh_encode(Decoded, rfc4716_public_key),
    Decoded = public_key:ssh_decode(Encoded, rfc4716_public_key).

%%--------------------------------------------------------------------
%% Check of different host keys left to later
ssh_hostkey_fingerprint_md5_implicit(_Config) ->
    Expected = "4b:0b:63:de:0f:a7:3a:ab:2c:cc:2d:d1:21:37:1d:3a",
    Expected = public_key:ssh_hostkey_fingerprint(ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Check of different host keys left to later
ssh_hostkey_fingerprint_md5(_Config) ->
    Expected = "MD5:4b:0b:63:de:0f:a7:3a:ab:2c:cc:2d:d1:21:37:1d:3a",
    Expected = public_key:ssh_hostkey_fingerprint(md5, ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Since this kind of fingerprint is not available yet on standard
%% distros, we do like this instead. The Expected is generated with:
%%       $ openssh-7.3p1/ssh-keygen -E sha1 -lf <file>
%%       2048 SHA1:Soammnaqg06jrm2jivMSnzQGlmk none@example.org (RSA)
ssh_hostkey_fingerprint_sha(_Config) ->
    Expected = "SHA1:Soammnaqg06jrm2jivMSnzQGlmk",
    Expected = public_key:ssh_hostkey_fingerprint(sha, ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Since this kind of fingerprint is not available yet on standard
%% distros, we do like this instead.
ssh_hostkey_fingerprint_sha256(_Config) ->
    Expected = "SHA256:T7F1BahkJWR7iJO8+rpzWOPbp7LZP4MlNrDExdNYOvY",
    Expected = public_key:ssh_hostkey_fingerprint(sha256, ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Since this kind of fingerprint is not available yet on standard
%% distros, we do like this instead.
ssh_hostkey_fingerprint_sha384(_Config) ->
    Expected = "SHA384:QhkLoGNI4KXdPvC//HxxSCP3uTQVADqxdajbgm+Gkx9zqz8N94HyP1JmH8C4/aEl",
    Expected = public_key:ssh_hostkey_fingerprint(sha384, ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Since this kind of fingerprint is not available yet on standard
%% distros, we do like this instead.
ssh_hostkey_fingerprint_sha512(_Config) ->
    Expected = "SHA512:ezUismvm3ADQQb6Nm0c1DwQ6ydInlJNfsnSQejFkXNmABg1Aenk9oi45CXeBOoTnlfTsGG8nFDm0smP10PBEeA",
    Expected = public_key:ssh_hostkey_fingerprint(sha512, ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Since this kind of fingerprint is not available yet on standard
%% distros, we do like this instead.
ssh_hostkey_fingerprint_list(_Config) ->
    Expected = ["SHA1:Soammnaqg06jrm2jivMSnzQGlmk",
                "MD5:4b:0b:63:de:0f:a7:3a:ab:2c:cc:2d:d1:21:37:1d:3a"],
    Expected = public_key:ssh_hostkey_fingerprint([sha,md5], ssh_hostkey(rsa)).

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

    true = public_key:pkix_is_self_signed(element(2,hd(Certs0))),
    false = public_key:pkix_is_self_signed(element(2,hd(Certs1))),

    CaIds = [element(2, public_key:pkix_issuer_id(Cert, self)) || 
		{'Certificate', Cert, _} <- Certs0],
    {ok, IssuerId = {_, _IssuerName}} = 
	public_key:pkix_issuer_id(element(2,hd(Certs1)), other),

    true = lists:member(IssuerId, CaIds),

    %% Should be normalized allready
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
    ok.

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

    %% Check that a dns_id does not match a DNS subjAltName wiht wildcard
    false =  public_key:pkix_verify_hostname(Cert, [{dns_id,"other.example.org"}]),

    %% Check that a dns_id does match a DNS subjAltName wiht wildcard with matchfun
    true =  public_key:pkix_verify_hostname(Cert, [{dns_id,"other.example.org"}],
                                            [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}
                                            ]
                                             ),

    %% Check that a uri_id does not match a DNS subjAltName wiht wildcard
    false =  public_key:pkix_verify_hostname(Cert, [{uri_id,"https://other.example.org"}]),

    %% Check that a dns_id does match a DNS subjAltName wiht wildcard with matchfun
    true =  public_key:pkix_verify_hostname(Cert, [{uri_id,"https://other.example.org"}],
                                            [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}
                                            ]
                                             ).

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
                                            [{fqdn_fun,fun(_)->undefined end}]).
    

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
                                          peer => [{key, hardcode_rsa_key()}]},
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



ssh_hostkey(rsa) ->
    [{PKdecoded,_}] =
	public_key:ssh_decode(
	  <<"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDYXcYmsyJBstl4EfFYzfQJmSiUE162zvSGSoMYybShYOI6rnnyvvihfw8Aml+2gZ716F2tqG48FQ/yPZEGWNPMrCejPpJctaPWhpNdNMJ8KFXSEgr5bY2mEpa19DHmuDeXKzeJJ+X7s3fVdYc4FMk5731KIW6Huf019ZnTxbx0VKG6b1KAJBg3vpNsDxEMwQ4LFMB0JHVklOTzbxmpaeULuIxvl65A+eGeFVeo2Q+YI9UnwY1vSgmc9Azwy8Ie9Z0HpQBN5I7Uc5xnknT8V6xDhgNfXEfzsgsRdDfZLECt1WO/1gP9wkosvAGZWt5oG8pbNQWiQdFq536ck8WQD9WD none@example.org">>,
	  public_key),
    PKdecoded.

hardcode_rsa_key() ->
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
       otherPrimeInfos = asn1_NOVALUE}.
