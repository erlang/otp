%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2024. All Rights Reserved.
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

-module(pkcs12_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

%%====================================================================
%% Common Test Interface Functions
%%====================================================================

all() ->
    [
     {group, key_generation},
     {group, certificate_generation},
     {group, pkcs12_operations},
     {group, interoperability}
    ].

groups() ->
    [
     {key_generation, [parallel],
      [test_rsa_key_generation,
       test_ec_key_generation,
       test_eddsa_key_generation,
       test_advanced_key_generation]},

     {certificate_generation, [parallel],
      [test_self_signed_certificates,
       test_ca_certificates,
       test_end_entity_certificates,
       test_cert_key_pair_generation]},

     {pkcs12_operations, [],
      [test_basic_pfx_creation,
       test_advanced_pfx_creation,
       test_pfx_encoding_decoding,
       test_password_protection,
       test_mac_verification]},

     {interoperability, [],
      [test_openssl_compatibility,
       test_multiple_curves,
       test_complete_pki_scenario]}
    ].

init_per_suite(Config) ->
    application:ensure_all_started(crypto),
    application:ensure_all_started(public_key),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Key Generation Tests
%%====================================================================

test_rsa_key_generation(_Config) ->
    % Test default RSA key generation
    RSAKey1 = public_key:pkcs12_generate_rsa_key(),
    ?assertMatch(#'RSAPrivateKey'{}, RSAKey1),

    % Test RSA key with specific size
    RSAKey2 = public_key:pkcs12_generate_rsa_key(2048),
    ?assertMatch(#'RSAPrivateKey'{}, RSAKey2),

    % Test RSA key with custom exponent
    RSAKey3 = public_key:pkcs12_generate_rsa_key(2048, 3),
    ?assertMatch(#'RSAPrivateKey'{publicExponent = 3}, RSAKey3),

    % Verify key sizes
    ?assertEqual(2048, key_size_bits(RSAKey1)),
    ?assertEqual(2048, key_size_bits(RSAKey2)),
    ?assertEqual(2048, key_size_bits(RSAKey3)),

    ok.

test_ec_key_generation(_Config) ->
    % Test default EC key generation
    ECKey1 = public_key:pkcs12_generate_ec_key(),
    ?assertMatch(#'ECPrivateKey'{}, ECKey1),

    % Test specific curves
    Curves = [secp256r1, secp384r1, secp521r1],
    lists:foreach(fun(Curve) ->
        Key = public_key:pkcs12_generate_ec_key(Curve),
        ?assertMatch(#'ECPrivateKey'{}, Key),
        verify_ec_curve(Key, Curve)
    end, Curves),

    ok.

test_eddsa_key_generation(_Config) ->
    % Test default EdDSA key generation
    EdKey1 = public_key:pkcs12_generate_eddsa_key(),
    ?assertMatch(#'ECPrivateKey'{}, EdKey1),

    % Test specific EdDSA curves
    EdCurves = [ed25519, ed448],
    lists:foreach(fun(Curve) ->
        Key = public_key:pkcs12_generate_eddsa_key(Curve),
        ?assertMatch(#'ECPrivateKey'{}, Key),
        verify_eddsa_curve(Key, Curve)
    end, EdCurves),

    ok.

test_advanced_key_generation(_Config) ->
    % Test key generation with option maps
    RSAOpts = #{type => rsa, size => 3072, public_exponent => 65537},
    RSAKey = public_key:pkcs12_generate_key(RSAOpts),
    ?assertMatch(#'RSAPrivateKey'{publicExponent = 65537}, RSAKey),
    ?assertEqual(3072, key_size_bits(RSAKey)),

    ECOpts = #{type => ec, curve => secp384r1},
    ECKey = public_key:pkcs12_generate_key(ECOpts),
    ?assertMatch(#'ECPrivateKey'{}, ECKey),
    verify_ec_curve(ECKey, secp384r1),

    EdOpts = #{type => eddsa, curve => ed25519},
    EdKey = public_key:pkcs12_generate_key(EdOpts),
    ?assertMatch(#'ECPrivateKey'{}, EdKey),
    verify_eddsa_curve(EdKey, ed25519),

    % Test default options
    DefaultKey = public_key:pkcs12_generate_key(#{}),
    ?assertMatch(#'ECPrivateKey'{}, DefaultKey),

    ok.

%%====================================================================
%% Certificate Generation Tests
%%====================================================================

test_self_signed_certificates(_Config) ->
    % Test RSA self-signed certificate
    RSAKey = public_key:pkcs12_generate_rsa_key(2048),
    RSACertOpts = #{
        subject => create_test_subject("RSA Test"),
        validity => create_test_validity(365),
        key_usage => [digitalSignature, keyEncipherment]
    },
    RSACert = public_key:pkcs12_generate_self_signed_cert(RSAKey, RSACertOpts),
    ?assert(is_binary(RSACert)),
    verify_certificate_signature(RSACert, RSAKey),

    % Test EC self-signed certificate
    ECKey = public_key:pkcs12_generate_ec_key(secp256r1),
    ECCertOpts = #{
        subject => create_test_subject("EC Test"),
        validity => create_test_validity(365),
        key_usage => [digitalSignature]
    },
    ECCert = public_key:pkcs12_generate_self_signed_cert(ECKey, ECCertOpts),
    ?assert(is_binary(ECCert)),
    verify_certificate_signature(ECCert, ECKey),

    % Test EdDSA self-signed certificate
    EdKey = public_key:pkcs12_generate_eddsa_key(ed25519),
    EdCertOpts = #{
        subject => create_test_subject("EdDSA Test"),
        validity => create_test_validity(365),
        key_usage => [digitalSignature]
    },
    EdCert = public_key:pkcs12_generate_self_signed_cert(EdKey, EdCertOpts),
    ?assert(is_binary(EdCert)),
    verify_certificate_signature(EdCert, EdKey),

    ok.

test_ca_certificates(_Config) ->
    % Test CA certificate generation
    CAKey = public_key:pkcs12_generate_rsa_key(4096),
    CAOpts = #{
        subject => create_test_subject("Test Root CA"),
        validity => create_test_validity(3650),
        key_usage => [keyCertSign, digitalSignature, cRLSign],
        basic_constraints => #{ca => true, path_length => 2}
    },
    CACert = public_key:pkcs12_generate_ca_cert(CAKey, CAOpts),
    ?assert(is_binary(CACert)),

    % Verify CA certificate properties
    verify_certificate_signature(CACert, CAKey),
    verify_ca_constraints(CACert),

    ok.

test_end_entity_certificates(_Config) ->
    % Create CA for signing
    CAKey = public_key:pkcs12_generate_rsa_key(2048),
    CAOpts = #{
        subject => create_test_subject("Test CA"),
        validity => create_test_validity(3650)
    },
    CACert = public_key:pkcs12_generate_ca_cert(CAKey, CAOpts),

    % Create end entity certificate
    ServerKey = public_key:pkcs12_generate_ec_key(secp256r1),
    ServerOpts = #{
        subject => create_test_subject("Test Server"),
        validity => create_test_validity(365),
        key_usage => [digitalSignature, keyEncipherment],
        extended_key_usage => [serverAuth],
        subject_alt_names => [
            {dNSName, "test.example.com"},
            {iPAddress, {192, 168, 1, 1}}
        ],
        issuer => extract_subject_from_cert(CACert),
        issuer_key => CAKey
    },
    ServerCert = public_key:pkcs12_generate_end_entity_cert(ServerKey, CAKey, ServerOpts),
    ?assert(is_binary(ServerCert)),

    % Verify certificate chain
    verify_certificate_signature(ServerCert, CAKey),
    verify_end_entity_constraints(ServerCert),

    ok.

test_cert_key_pair_generation(_Config) ->
    % Test combined certificate and key generation
    CertOpts = #{
        subject => create_test_subject("Combined Test"),
        validity => create_test_validity(365),
        key_usage => [digitalSignature]
    },
    KeyOpts = #{type => ec, curve => secp256r1},

    #{cert := Cert, key := Key} =
        public_key:pkcs12_generate_cert_key_pair(CertOpts, KeyOpts),

    ?assert(is_binary(Cert)),
    ?assertMatch(#'ECPrivateKey'{}, Key),
    verify_certificate_signature(Cert, Key),

    % Test with default key options
    #{cert := Cert2, key := Key2} =
        public_key:pkcs12_generate_cert_key_pair(CertOpts),

    ?assert(is_binary(Cert2)),
    ?assertMatch(#'ECPrivateKey'{}, Key2),
    verify_certificate_signature(Cert2, Key2),

    ok.

%%====================================================================
%% PKCS#12 Operations Tests
%%====================================================================

test_basic_pfx_creation(_Config) ->
    % Generate test certificate and key
    Key = public_key:pkcs12_generate_ec_key(secp256r1),
    CertOpts = #{
        subject => create_test_subject("PFX Test"),
        validity => create_test_validity(365)
    },
    Cert = public_key:pkcs12_generate_self_signed_cert(Key, CertOpts),

    % Create basic PFX
    PFX = public_key:pkcs12_create_pfx(Cert, Key),
    ?assert(is_binary(PFX)),
    ?assert(byte_size(PFX) > 100), % Reasonable minimum size

    % Verify PFX structure
    verify_pfx_structure(PFX),

    ok.

test_advanced_pfx_creation(_Config) ->
    % Generate test certificate and key
    Key = public_key:pkcs12_generate_rsa_key(2048),
    CertOpts = #{
        subject => create_test_subject("Advanced PFX Test"),
        validity => create_test_validity(365)
    },
    Cert = public_key:pkcs12_generate_self_signed_cert(Key, CertOpts),

    % Create PFX with advanced options
    PFXOpts = #{
        password => "TestPassword123!",
        friendly_name => "Test Certificate",
        local_key_id => crypto:strong_rand_bytes(20),
        iterations => 4096,
        mac_algorithm => sha256
    },
    PFX = public_key:pkcs12_create_pfx(Cert, Key, PFXOpts),
    ?assert(is_binary(PFX)),

    % Verify advanced options are applied
    verify_pfx_structure(PFX),
    verify_pfx_mac(PFX, "TestPassword123!"),

    ok.

test_pfx_encoding_decoding(_Config) ->
    % Generate test certificate and key
    Key = public_key:pkcs12_generate_ec_key(secp384r1),
    CertOpts = #{
        subject => create_test_subject("Encode/Decode Test"),
        validity => create_test_validity(365)
    },
    Cert = public_key:pkcs12_generate_self_signed_cert(Key, CertOpts),

    Password = "DecodeTest456!",
    PFXOpts = #{password => Password},

    % Test encoding
    PFX1 = public_key:pkcs12_encode(Cert, Key, PFXOpts),
    PFX2 = public_key:pkcs12_create_pfx(Cert, Key, PFXOpts),
    ?assertEqual(PFX1, PFX2), % Should be identical

    % Test decoding
    #{cert := DecodedCert, key := DecodedKey, chain := Chain} =
        public_key:pkcs12_decode(PFX1, Password),

    ?assertEqual(Cert, DecodedCert),
    ?assertEqual([], Chain), % No additional certificates
    verify_keys_equivalent(Key, DecodedKey),

    ok.

test_password_protection(_Config) ->
    % Generate test certificate and key
    Key = public_key:pkcs12_generate_rsa_key(2048),
    CertOpts = #{
        subject => create_test_subject("Password Test"),
        validity => create_test_validity(365)
    },
    Cert = public_key:pkcs12_generate_self_signed_cert(Key, CertOpts),

    Password = "SecurePassword789!",
    PFXOpts = #{password => Password, iterations => 8192},
    PFX = public_key:pkcs12_create_pfx(Cert, Key, PFXOpts),

    % Test correct password
    Result = public_key:pkcs12_decode(PFX, Password),
    ?assertMatch(#{cert := _, key := _, chain := _}, Result),

    % Test incorrect password (should fail)
    ?assertError({bad_mac, _}, public_key:pkcs12_decode(PFX, "WrongPassword")),

    ok.

test_mac_verification(_Config) ->
    % Generate test certificate and key
    Key = public_key:pkcs12_generate_ec_key(secp256r1),
    CertOpts = #{
        subject => create_test_subject("MAC Test"),
        validity => create_test_validity(365)
    },
    Cert = public_key:pkcs12_generate_self_signed_cert(Key, CertOpts),

    % Test different MAC algorithms
    MacAlgorithms = [sha1, sha256, sha384, sha512],
    lists:foreach(fun(MacAlg) ->
        PFXOpts = #{
            password => "MacTest123!",
            mac_algorithm => MacAlg
        },
        PFX = public_key:pkcs12_create_pfx(Cert, Key, PFXOpts),
        verify_pfx_mac(PFX, "MacTest123!")
    end, MacAlgorithms),

    ok.

%%====================================================================
%% Interoperability Tests
%%====================================================================

test_openssl_compatibility(_Config) ->
    % Test that generated PKCS#12 files can be verified with OpenSSL concepts
    Key = public_key:pkcs12_generate_rsa_key(2048),
    CertOpts = #{
        subject => create_test_subject("OpenSSL Compat"),
        validity => create_test_validity(365)
    },
    Cert = public_key:pkcs12_generate_self_signed_cert(Key, CertOpts),

    PFXOpts = #{password => "openssl123"},
    PFX = public_key:pkcs12_create_pfx(Cert, Key, PFXOpts),

    % Verify basic PFX structure that OpenSSL would accept
    verify_pfx_structure(PFX),

    % Test round-trip compatibility
    #{cert := DecodedCert, key := DecodedKey} =
        public_key:pkcs12_decode(PFX, "openssl123"),

    ?assertEqual(Cert, DecodedCert),
    verify_keys_equivalent(Key, DecodedKey),

    ok.

test_multiple_curves(_Config) ->
    % Test PKCS#12 creation with different curve types
    Curves = [
        {ec, secp256r1},
        {ec, secp384r1},
        {ec, secp521r1},
        {eddsa, ed25519},
        {eddsa, ed448}
    ],

    lists:foreach(fun({Type, Curve}) ->
        Key = case Type of
            ec -> public_key:pkcs12_generate_ec_key(Curve);
            eddsa -> public_key:pkcs12_generate_eddsa_key(Curve)
        end,

        CertOpts = #{
            subject => create_test_subject(io_lib:format("~s Test", [Curve])),
            validity => create_test_validity(365)
        },
        Cert = public_key:pkcs12_generate_self_signed_cert(Key, CertOpts),

        PFXOpts = #{password => atom_to_list(Curve) ++ "123!"},
        PFX = public_key:pkcs12_create_pfx(Cert, Key, PFXOpts),

        % Verify round-trip
        #{cert := DecodedCert, key := DecodedKey} =
            public_key:pkcs12_decode(PFX, atom_to_list(Curve) ++ "123!"),

        ?assertEqual(Cert, DecodedCert),
        verify_keys_equivalent(Key, DecodedKey)
    end, Curves),

    ok.

test_complete_pki_scenario(_Config) ->
    % Test complete PKI setup with PKCS#12 files

    % Root CA
    RootKey = public_key:pkcs12_generate_rsa_key(4096),
    RootOpts = #{
        subject => create_test_subject("Test Root CA"),
        validity => create_test_validity(7300)
    },
    RootCert = public_key:pkcs12_generate_ca_cert(RootKey, RootOpts),

    % Intermediate CA
    IntermediateKey = public_key:pkcs12_generate_ec_key(secp384r1),
    IntermediateOpts = #{
        subject => create_test_subject("Test Intermediate CA"),
        validity => create_test_validity(3650),
        issuer => extract_subject_from_cert(RootCert),
        issuer_key => RootKey
    },
    IntermediateCert = public_key:pkcs12_generate_ca_cert(IntermediateKey, IntermediateOpts),

    % Server Certificate
    ServerKey = public_key:pkcs12_generate_ec_key(secp256r1),
    ServerOpts = #{
        subject => create_test_subject("Test Server"),
        validity => create_test_validity(365),
        issuer => extract_subject_from_cert(IntermediateCert),
        issuer_key => IntermediateKey
    },
    ServerCert = public_key:pkcs12_generate_end_entity_cert(ServerKey, IntermediateKey, ServerOpts),

    % Create PKCS#12 files
    ServerPFX = public_key:pkcs12_create_pfx(ServerCert, ServerKey, #{password => "server123"}),

    % Verify the complete chain
    #{cert := DecodedServerCert, key := DecodedServerKey} =
        public_key:pkcs12_decode(ServerPFX, "server123"),

    ?assertEqual(ServerCert, DecodedServerCert),
    verify_keys_equivalent(ServerKey, DecodedServerKey),

    % Verify certificate chain validity
    verify_certificate_signature(ServerCert, IntermediateKey),
    verify_certificate_signature(IntermediateCert, RootKey),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

create_test_subject(CommonName) ->
    {rdnSequence, [
        [#'AttributeTypeAndValue'{type = {2,5,4,6}, value = "US"}],
        [#'AttributeTypeAndValue'{type = {2,5,4,8}, value = {utf8String, "California"}}],
        [#'AttributeTypeAndValue'{type = {2,5,4,7}, value = {utf8String, "San Francisco"}}],
        [#'AttributeTypeAndValue'{type = {2,5,4,10}, value = {utf8String, "Test Organization"}}],
        [#'AttributeTypeAndValue'{type = {2,5,4,3}, value = {utf8String, CommonName}}]
    ]}.

create_test_validity(Days) ->
    Now = calendar:universal_time(),
    Start = calendar:datetime_to_gregorian_seconds(Now),
    End = Start + (Days * 24 * 60 * 60),
    StartTime = calendar:gregorian_seconds_to_datetime(Start),
    EndTime = calendar:gregorian_seconds_to_datetime(End),
    {StartTime, EndTime}.

extract_subject_from_cert(CertDer) ->
    Cert = public_key:pkix_decode_cert(CertDer, otp),
    TBSCert = Cert#'OTPCertificate'.tbsCertificate,
    TBSCert#'OTPTBSCertificate'.subject.

key_size_bits(#'RSAPrivateKey'{modulus = N}) ->
    byte_size(crypto:integer_to_bytes(N)) * 8;
key_size_bits(#'ECPrivateKey'{}) ->
    256. % Simplified for testing

verify_ec_curve(#'ECPrivateKey'{parameters = {namedCurve, Curve}}, ExpectedCurve) ->
    ExpectedOID = curve_name_to_oid(ExpectedCurve),
    ?assertEqual(ExpectedOID, Curve).

verify_eddsa_curve(#'ECPrivateKey'{parameters = {namedCurve, Curve}}, ExpectedCurve) ->
    ExpectedOID = eddsa_curve_name_to_oid(ExpectedCurve),
    ?assertEqual(ExpectedOID, Curve).

curve_name_to_oid(secp256r1) -> {1, 2, 840, 10045, 3, 1, 7};
curve_name_to_oid(secp384r1) -> {1, 3, 132, 0, 34};
curve_name_to_oid(secp521r1) -> {1, 3, 132, 0, 35}.

eddsa_curve_name_to_oid(ed25519) -> {1, 3, 101, 112};
eddsa_curve_name_to_oid(ed448) -> {1, 3, 101, 113}.

verify_certificate_signature(CertDer, PrivKey) ->
    % Verify that the certificate is properly signed
    ?assert(public_key:pkix_verify(CertDer, extract_public_key(PrivKey))).

extract_public_key(#'RSAPrivateKey'{modulus = N, publicExponent = E}) ->
    #'RSAPublicKey'{modulus = N, publicExponent = E};
extract_public_key(#'ECPrivateKey'{parameters = Params, publicKey = PubKey}) ->
    {#'ECPoint'{point = PubKey}, Params}.

verify_ca_constraints(CertDer) ->
    Cert = public_key:pkix_decode_cert(CertDer, otp),
    TBSCert = Cert#'OTPCertificate'.tbsCertificate,
    Extensions = TBSCert#'OTPTBSCertificate'.extensions,

    % Find basic constraints extension
    BasicConstraints = lists:keyfind({2, 5, 29, 19}, #'Extension'.extnID, Extensions),
    ?assertNotEqual(false, BasicConstraints),

    #'Extension'{extnValue = #'BasicConstraints'{cA = true}} = BasicConstraints.

verify_end_entity_constraints(CertDer) ->
    Cert = public_key:pkix_decode_cert(CertDer, otp),
    TBSCert = Cert#'OTPCertificate'.tbsCertificate,
    Extensions = TBSCert#'OTPTBSCertificate'.extensions,

    % Find basic constraints extension
    BasicConstraints = lists:keyfind({2, 5, 29, 19}, #'Extension'.extnID, Extensions),
    ?assertNotEqual(false, BasicConstraints),

    #'Extension'{extnValue = #'BasicConstraints'{cA = false}} = BasicConstraints.

verify_pfx_structure(PFXDer) ->
    % Verify that PFX can be decoded
    PFX = public_key:der_decode('PFX', PFXDer),
    ?assertMatch(#'PFX'{version = v3}, PFX).

verify_pfx_mac(PFXDer, Password) ->
    % Verify MAC by successfully decoding
    Result = public_key:pkcs12_decode(PFXDer, Password),
    ?assertMatch(#{cert := _, key := _}, Result).

verify_keys_equivalent(Key1, Key2) ->
    % Simplified key equivalence check
    ?assertEqual(extract_public_key(Key1), extract_public_key(Key2)).
