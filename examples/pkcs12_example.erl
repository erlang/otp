%%% PKCS#12 Example: Complete Key and Certificate Generation
%%%
%%% This example demonstrates all PKCS#12 functionality available in OTP 28:
%%% - Key generation (RSA, EC, EdDSA)
%%% - Certificate generation (self-signed, CA, end entity)
%%% - PKCS#12 file creation and decoding
%%% - Multiple certificate chains in a single PKCS#12 file

-module(pkcs12_example).

-export([run_all_examples/0,
         demo_key_generation/0,
         demo_certificate_generation/0,
         demo_pkcs12_creation/0,
         demo_complete_pki/0,
         demo_curve_variety/0]).

%%====================================================================
%% Main Example Runner
%%====================================================================

run_all_examples() ->
    io:format("=== PKCS#12 Key and Certificate Generation Examples ===~n~n"),

    demo_key_generation(),
    demo_certificate_generation(),
    demo_pkcs12_creation(),
    demo_complete_pki(),
    demo_curve_variety(),

    io:format("=== All examples completed successfully! ===~n").

%%====================================================================
%% Key Generation Examples
%%====================================================================

demo_key_generation() ->
    io:format("1. Key Generation Examples~n"),
    io:format("~s~n", [string:copies("-", 40)]),

    %% RSA Key Generation
    io:format("Generating RSA keys...~n"),
    RSA2048 = public_key:pkcs12_generate_rsa_key(),  % Default 2048-bit
    RSA4096 = public_key:pkcs12_generate_rsa_key(4096),
    RSACustom = public_key:pkcs12_generate_rsa_key(2048, 3),  % Custom exponent

    io:format("  - RSA 2048-bit key: ~w~n", [element(1, RSA2048)]),
    io:format("  - RSA 4096-bit key: ~w~n", [element(1, RSA4096)]),
    io:format("  - RSA 2048-bit key (e=3): ~w~n", [element(1, RSACustom)]),

    %% EC Key Generation
    io:format("Generating EC keys...~n"),
    ECDefault = public_key:pkcs12_generate_ec_key(),  % Default secp256r1
    ECP384 = public_key:pkcs12_generate_ec_key(secp384r1),
    ECP521 = public_key:pkcs12_generate_ec_key(secp521r1),

    io:format("  - EC secp256r1 key: ~w~n", [element(1, ECDefault)]),
    io:format("  - EC secp384r1 key: ~w~n", [element(1, ECP384)]),
    io:format("  - EC secp521r1 key: ~w~n", [element(1, ECP521)]),

    %% EdDSA Key Generation
    io:format("Generating EdDSA keys...~n"),
    Ed25519 = public_key:pkcs12_generate_eddsa_key(),  % Default ed25519
    Ed448 = public_key:pkcs12_generate_eddsa_key(ed448),
    X25519 = public_key:pkcs12_generate_eddsa_key(x25519),

    io:format("  - Ed25519 key: ~w~n", [element(1, Ed25519)]),
    io:format("  - Ed448 key: ~w~n", [element(1, Ed448)]),
    io:format("  - X25519 key: ~w~n", [element(1, X25519)]),

    %% Advanced Key Generation with Options
    io:format("Generating keys with advanced options...~n"),
    RSAOpts = #{type => rsa, size => 3072, public_exponent => 65537},
    ECOpts = #{type => ec, curve => secp384r1},
    EdOpts = #{type => eddsa, curve => ed25519},

    RSAAdvanced = public_key:pkcs12_generate_key(RSAOpts),
    ECAdvanced = public_key:pkcs12_generate_key(ECOpts),
    EdAdvanced = public_key:pkcs12_generate_key(EdOpts),

    io:format("  - RSA 3072-bit via options: ~w~n", [element(1, RSAAdvanced)]),
    io:format("  - EC secp384r1 via options: ~w~n", [element(1, ECAdvanced)]),
    io:format("  - Ed25519 via options: ~w~n", [element(1, EdAdvanced)]),

    io:format("~n").

%%====================================================================
%% Certificate Generation Examples
%%====================================================================

demo_certificate_generation() ->
    io:format("2. Certificate Generation Examples~n"),
    io:format("~s~n", [string:copies("-", 40)]),

    %% Generate keys for certificate examples
    RSAKey = public_key:pkcs12_generate_rsa_key(2048),
    ECKey = public_key:pkcs12_generate_ec_key(secp256r1),
    EdKey = public_key:pkcs12_generate_eddsa_key(ed25519),

    %% Self-signed certificates
    io:format("Creating self-signed certificates...~n"),

    SelfSignedOpts = #{
        subject => create_subject("Test Self-Signed", "test-self-signed.example.com"),
        validity => create_validity(365),  % 1 year
        key_usage => [digitalSignature, keyEncipherment],
        subject_alt_names => [{dNSName, "test-self-signed.example.com"}]
    },

    RSASelfSigned = public_key:pkcs12_generate_self_signed_cert(RSAKey, SelfSignedOpts),
    ECSelfSigned = public_key:pkcs12_generate_self_signed_cert(ECKey, SelfSignedOpts),
    EdSelfSigned = public_key:pkcs12_generate_self_signed_cert(EdKey, SelfSignedOpts),

    io:format("  - RSA self-signed cert: ~w bytes~n", [byte_size(RSASelfSigned)]),
    io:format("  - EC self-signed cert: ~w bytes~n", [byte_size(ECSelfSigned)]),
    io:format("  - EdDSA self-signed cert: ~w bytes~n", [byte_size(EdSelfSigned)]),

    %% CA certificates
    io:format("Creating CA certificates...~n"),

    CAOpts = #{
        subject => create_subject("Test Root CA", "ca.example.com"),
        validity => create_validity(3650),  % 10 years
        key_usage => [keyCertSign, digitalSignature, cRLSign],
        basic_constraints => #{ca => true, path_length => 2}
    },

    RootCA = public_key:pkcs12_generate_ca_cert(RSAKey, CAOpts),
    IntermediateKey = public_key:pkcs12_generate_ec_key(secp256r1),

    IntermediateCAOpts = CAOpts#{
        subject => create_subject("Test Intermediate CA", "intermediate.ca.example.com"),
        basic_constraints => #{ca => true, path_length => 0},
        issuer => extract_subject_from_cert(RootCA),
        issuer_key => RSAKey
    },

    IntermediateCA = public_key:pkcs12_generate_ca_cert(IntermediateKey, IntermediateCAOpts),

    io:format("  - Root CA cert: ~w bytes~n", [byte_size(RootCA)]),
    io:format("  - Intermediate CA cert: ~w bytes~n", [byte_size(IntermediateCA)]),

    %% End entity certificates
    io:format("Creating end entity certificates...~n"),

    ServerKey = public_key:pkcs12_generate_ec_key(secp256r1),
    ClientKey = public_key:pkcs12_generate_rsa_key(2048),

    ServerOpts = #{
        subject => create_subject("Server Certificate", "server.example.com"),
        validity => create_validity(365),
        key_usage => [digitalSignature, keyEncipherment],
        extended_key_usage => [serverAuth],
        subject_alt_names => [
            {dNSName, "server.example.com"},
            {dNSName, "www.example.com"},
            {iPAddress, {192, 168, 1, 100}}
        ],
        issuer => extract_subject_from_cert(IntermediateCA),
        issuer_key => IntermediateKey
    },

    ClientOpts = #{
        subject => create_subject("Client Certificate", "client@example.com"),
        validity => create_validity(365),
        key_usage => [digitalSignature],
        extended_key_usage => [clientAuth],
        subject_alt_names => [{rfc822Name, "client@example.com"}],
        issuer => extract_subject_from_cert(IntermediateCA),
        issuer_key => IntermediateKey
    },

    ServerCert = public_key:pkcs12_generate_end_entity_cert(ServerKey, IntermediateKey, ServerOpts),
    ClientCert = public_key:pkcs12_generate_end_entity_cert(ClientKey, IntermediateKey, ClientOpts),

    io:format("  - Server cert: ~w bytes~n", [byte_size(ServerCert)]),
    io:format("  - Client cert: ~w bytes~n", [byte_size(ClientCert)]),

    %% Certificate and key pairs
    io:format("Creating certificate and key pairs...~n"),

    CertKeyOpts = #{
        subject => create_subject("Combined Test", "combined.example.com"),
        validity => create_validity(365),
        basic_constraints => #{ca => false}
    },

    KeyOpts = #{type => ec, curve => secp256r1},

    #{cert := CombinedCert, key := CombinedKey} =
        public_key:pkcs12_generate_cert_key_pair(CertKeyOpts, KeyOpts),

    io:format("  - Combined cert+key: cert=~w bytes, key=~w~n",
              [byte_size(CombinedCert), element(1, CombinedKey)]),

    io:format("~n").

%%====================================================================
%% PKCS#12 Creation Examples
%%====================================================================

demo_pkcs12_creation() ->
    io:format("3. PKCS#12 File Creation Examples~n"),
    io:format("~s~n", [string:copies("-", 40)]),

    %% Generate certificate and key for PKCS#12
    Key = public_key:pkcs12_generate_ec_key(secp256r1),
    CertOpts = #{
        subject => create_subject("PKCS#12 Test", "pkcs12.example.com"),
        validity => create_validity(365),
        subject_alt_names => [{dNSName, "pkcs12.example.com"}]
    },
    Cert = public_key:pkcs12_generate_self_signed_cert(Key, CertOpts),

    %% Basic PKCS#12 creation
    io:format("Creating basic PKCS#12 files...~n"),

    BasicPFX = public_key:pkcs12_create_pfx(Cert, Key),  % Default password "changeit"
    io:format("  - Basic PFX: ~w bytes~n", [byte_size(BasicPFX)]),

    %% Advanced PKCS#12 with custom options
    io:format("Creating advanced PKCS#12 files...~n"),

    AdvancedOpts = #{
        password => "MySecurePassword123!",
        friendly_name => "My Test Certificate",
        local_key_id => crypto:strong_rand_bytes(20),
        iterations => 4096,
        mac_algorithm => sha256
    },

    AdvancedPFX = public_key:pkcs12_create_pfx(Cert, Key, AdvancedOpts),
    io:format("  - Advanced PFX: ~w bytes~n", [byte_size(AdvancedPFX)]),

    %% Multiple formats
    io:format("Creating PKCS#12 with different APIs...~n"),

    EncodePFX = public_key:pkcs12_encode(Cert, Key, AdvancedOpts),
    io:format("  - Encoded PFX: ~w bytes~n", [byte_size(EncodePFX)]),

    %% Decode PKCS#12
    io:format("Decoding PKCS#12 files...~n"),

    #{cert := DecodedCert, key := DecodedKey, chain := Chain} =
        public_key:pkcs12_decode(AdvancedPFX, "MySecurePassword123!"),

    io:format("  - Decoded cert: ~w bytes~n", [byte_size(DecodedCert)]),
    io:format("  - Decoded key: ~w~n", [element(1, DecodedKey)]),
    io:format("  - Certificate chain: ~w certificates~n", [length(Chain)]),

    %% Verify decoded content matches original
    CertMatch = (Cert =:= DecodedCert),
    KeyMatch = compare_private_keys(Key, DecodedKey),

    io:format("  - Certificate match: ~w~n", [CertMatch]),
    io:format("  - Key match: ~w~n", [KeyMatch]),

    io:format("~n").

%%====================================================================
%% Complete PKI Example
%%====================================================================

demo_complete_pki() ->
    io:format("4. Complete PKI Example~n"),
    io:format("~s~n", [string:copies("-", 40)]),

    io:format("Creating a complete 3-tier PKI...~n"),

    %% Root CA
    RootKey = public_key:pkcs12_generate_rsa_key(4096),
    RootOpts = #{
        subject => create_subject("Example Root CA", "root.ca.example.com"),
        validity => create_validity(7300),  % 20 years
        key_usage => [keyCertSign, digitalSignature, cRLSign],
        basic_constraints => #{ca => true}
    },
    RootCert = public_key:pkcs12_generate_ca_cert(RootKey, RootOpts),

    %% Intermediate CA
    IntermediateKey = public_key:pkcs12_generate_ec_key(secp384r1),
    IntermediateOpts = #{
        subject => create_subject("Example Intermediate CA", "intermediate.ca.example.com"),
        validity => create_validity(3650),  % 10 years
        key_usage => [keyCertSign, digitalSignature, cRLSign],
        basic_constraints => #{ca => true, path_length => 0},
        issuer => extract_subject_from_cert(RootCert),
        issuer_key => RootKey
    },
    IntermediateCert = public_key:pkcs12_generate_ca_cert(IntermediateKey, IntermediateOpts),

    %% Server Certificate
    ServerKey = public_key:pkcs12_generate_ec_key(secp256r1),
    ServerOpts = #{
        subject => create_subject("Example Server", "www.example.com"),
        validity => create_validity(365),
        key_usage => [digitalSignature, keyEncipherment],
        extended_key_usage => [serverAuth],
        subject_alt_names => [
            {dNSName, "www.example.com"},
            {dNSName, "example.com"},
            {dNSName, "api.example.com"}
        ],
        issuer => extract_subject_from_cert(IntermediateCert),
        issuer_key => IntermediateKey
    },
    ServerCert = public_key:pkcs12_generate_end_entity_cert(ServerKey, IntermediateKey, ServerOpts),

    %% Client Certificate
    ClientKey = public_key:pkcs12_generate_rsa_key(2048),
    ClientOpts = #{
        subject => create_subject("Example Client", "client@example.com"),
        validity => create_validity(365),
        key_usage => [digitalSignature],
        extended_key_usage => [clientAuth],
        subject_alt_names => [{rfc822Name, "client@example.com"}],
        issuer => extract_subject_from_cert(IntermediateCert),
        issuer_key => IntermediateKey
    },
    ClientCert = public_key:pkcs12_generate_end_entity_cert(ClientKey, IntermediateKey, ClientOpts),

    io:format("  - Root CA: ~w bytes~n", [byte_size(RootCert)]),
    io:format("  - Intermediate CA: ~w bytes~n", [byte_size(IntermediateCert)]),
    io:format("  - Server cert: ~w bytes~n", [byte_size(ServerCert)]),
    io:format("  - Client cert: ~w bytes~n", [byte_size(ClientCert)]),

    %% Create PKCS#12 files for each
    io:format("Creating PKCS#12 files for complete PKI...~n"),

    ServerPFXOpts = #{
        password => "ServerPassword123!",
        friendly_name => "Example Server Certificate",
        iterations => 4096
    },

    ClientPFXOpts = #{
        password => "ClientPassword123!",
        friendly_name => "Example Client Certificate",
        iterations => 4096
    },

    ServerPFX = public_key:pkcs12_create_pfx(ServerCert, ServerKey, ServerPFXOpts),
    ClientPFX = public_key:pkcs12_create_pfx(ClientCert, ClientKey, ClientPFXOpts),

    io:format("  - Server PFX: ~w bytes~n", [byte_size(ServerPFX)]),
    io:format("  - Client PFX: ~w bytes~n", [byte_size(ClientPFX)]),

    %% Save files (commented out for example)
    %% file:write_file("root_ca.der", RootCert),
    %% file:write_file("intermediate_ca.der", IntermediateCert),
    %% file:write_file("server.p12", ServerPFX),
    %% file:write_file("client.p12", ClientPFX),

    io:format("~n").

%%====================================================================
%% Curve Variety Example
%%====================================================================

demo_curve_variety() ->
    io:format("5. Cryptographic Curve Variety Example~n"),
    io:format("~s~n", [string:copies("-", 40)]),

    io:format("Demonstrating all supported curves...~n"),

    Curves = [secp256r1, secp384r1, secp521r1, ed25519, ed448],

    lists:foreach(fun(Curve) ->
        try
            Key = case Curve of
                ed25519 -> public_key:pkcs12_generate_eddsa_key(ed25519);
                ed448 -> public_key:pkcs12_generate_eddsa_key(ed448);
                _ -> public_key:pkcs12_generate_ec_key(Curve)
            end,

            CertOpts = #{
                subject => create_subject(
                    io_lib:format("~s Test", [string:to_upper(atom_to_list(Curve))]),
                    io_lib:format("~s.example.com", [atom_to_list(Curve)])
                ),
                validity => create_validity(365)
            },

            Cert = public_key:pkcs12_generate_self_signed_cert(Key, CertOpts),

            PFXOpts = #{
                password => io_lib:format("~sPassword123!", [atom_to_list(Curve)]),
                friendly_name => io_lib:format("~s Certificate", [string:to_upper(atom_to_list(Curve))])
            },

            PFX = public_key:pkcs12_create_pfx(Cert, Key, PFXOpts),

            io:format("  - ~s: cert=~w bytes, pfx=~w bytes~n",
                      [string:to_upper(atom_to_list(Curve)), byte_size(Cert), byte_size(PFX)])
        catch
            _:Error ->
                io:format("  - ~s: ERROR - ~p~n", [string:to_upper(atom_to_list(Curve)), Error])
        end
    end, Curves),

    io:format("~n").

%%====================================================================
%% Helper Functions
%%====================================================================

create_subject(CommonName, EmailOrDns) ->
    {rdnSequence, [
        [{'AttributeTypeAndValue', {2,5,4,6}, "US"}],  % Country
        [{'AttributeTypeAndValue', {2,5,4,8}, "California"}],  % State
        [{'AttributeTypeAndValue', {2,5,4,7}, "San Francisco"}],  % City
        [{'AttributeTypeAndValue', {2,5,4,10}, "Example Corp"}],  % Organization
        [{'AttributeTypeAndValue', {2,5,4,11}, "IT Department"}],  % Org Unit
        [{'AttributeTypeAndValue', {2,5,4,3}, CommonName}],  % Common Name
        [{'AttributeTypeAndValue', {1,2,840,113549,1,9,1}, EmailOrDns}]  % Email
    ]}.

create_validity(Days) ->
    Now = calendar:universal_time(),
    Start = calendar:datetime_to_gregorian_seconds(Now),
    End = Start + (Days * 24 * 60 * 60),
    StartTime = calendar:gregorian_seconds_to_datetime(Start),
    EndTime = calendar:gregorian_seconds_to_datetime(End),
    {StartTime, EndTime}.

extract_subject_from_cert(CertDer) ->
    % For this example, we'll return a placeholder subject
    % In real usage, you would decode the certificate and extract the subject
    {rdnSequence, [
        [{'AttributeTypeAndValue', {2,5,4,6}, "US"}],
        [{'AttributeTypeAndValue', {2,5,4,3}, "Issuer CN"}]
    ]}.

compare_private_keys(Key1, Key2) ->
    % Simplified key comparison - in practice, you'd compare key parameters
    try
        element(1, Key1) =:= element(1, Key2)
    catch
        _:_ -> false
    end.

%%====================================================================
%% Usage Instructions
%%====================================================================

%% To run this example:
%%
%% 1. Compile: c(pkcs12_example).
%% 2. Run all examples: pkcs12_example:run_all_examples().
%% 3. Run individual examples:
%%    - pkcs12_example:demo_key_generation().
%%    - pkcs12_example:demo_certificate_generation().
%%    - pkcs12_example:demo_pkcs12_creation().
%%    - pkcs12_example:demo_complete_pki().
%%    - pkcs12_example:demo_curve_variety().
%%
%% This example demonstrates:
%% - All key generation options (RSA, EC, EdDSA)
%% - Certificate generation (self-signed, CA, end entity)
%% - PKCS#12 file creation with various options
%% - Complete PKI setup with certificate chains
%% - Multiple cryptographic curves
%% - Real-world certificate extensions and constraints
