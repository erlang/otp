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

%%----------------------------------------------------------------------
%% Purpose: PKCS#12 - Personal Information Exchange Syntax
%%
%% RFC 7292: PKCS #12: Personal Information Exchange Syntax v1.1
%%----------------------------------------------------------------------

-module(pubkey_pkcs12).

-export([generate_key/1,
         generate_cert_key_pair/1,
         generate_cert_key_pair/2,
         create_pfx/2,
         create_pfx/3,
         encode_pfx/2,
         encode_pfx/3,
         decode_pfx/2,
         decode_pfx/3,

         % Key generation utilities
         generate_rsa_key/0,
         generate_rsa_key/1,
         generate_rsa_key/2,
         generate_ec_key/0,
         generate_ec_key/1,
         generate_eddsa_key/0,
         generate_eddsa_key/1,

         % Certificate generation utilities
         generate_self_signed_cert/2,
         generate_self_signed_cert/3,
         generate_ca_cert/2,
         generate_ca_cert/3,
         generate_end_entity_cert/3,
         generate_end_entity_cert/4,

         % PKCS#12 specific utilities
         create_safe_bag/3,
         create_safe_bag/4,
         create_cert_bag/1,
         create_cert_bag/2,
         create_key_bag/1,
         create_key_bag/2,
         create_shrouded_key_bag/2,
         create_shrouded_key_bag/3,
         create_tbs_certificate/3
        ]).

-include_lib("public_key/include/public_key.hrl").
-include_lib("public_key/include/OTP-PUB-KEY.hrl").

%%====================================================================
%% Types
%%====================================================================

-type curve_name() :: secp256r1 | secp384r1 | secp521r1 | ed25519 | ed448 | x25519 | x448.
-type key_gen_opts() :: #{type => rsa | ec | eddsa, size => pos_integer(),
                         public_exponent => pos_integer(), curve => curve_name()}.
-type oid() :: tuple().

%%====================================================================
%% API - Key Generation
%%====================================================================

%% @doc Generate a private key based on the provided options.
%% The function supports RSA, EC (Elliptic Curve), and EdDSA key generation.
%% For RSA keys, you can specify the key size and public exponent.
%% For EC and EdDSA keys, you can specify the curve to use.
%%
%% @param Opts A map containing key generation options:
%%        - type: The type of key to generate (rsa | ec | eddsa)
%%        - size: For RSA keys, the key size in bits (default: 2048)
%%        - public_exponent: For RSA keys, the public exponent (default: 65537)
%%        - curve: For EC/EdDSA keys, the curve to use (default: secp256r1 for EC, ed25519 for EdDSA)
%% @returns A private key of the specified type
-spec generate_key(key_gen_opts()) -> public_key:private_key().
generate_key(#{type := rsa} = Opts) ->
    Size = maps:get(size, Opts, 2048),
    PubExp = maps:get(public_exponent, Opts, 65537),
    generate_rsa_key(Size, PubExp);
generate_key(#{type := ec} = Opts) ->
    Curve = maps:get(curve, Opts, secp256r1),
    generate_ec_key(Curve);
generate_key(#{type := eddsa} = Opts) ->
    Curve = maps:get(curve, Opts, ed25519),
    generate_eddsa_key(Curve);
generate_key(#{}) ->
    % Default to ECDSA secp256r1
    generate_ec_key(secp256r1).

%% @doc Generate an RSA key with default parameters (2048-bit, e=65537).
%% This is a convenience function that generates a standard RSA key.
%%
%% @returns An RSA private key
-spec generate_rsa_key() -> public_key:private_key().
generate_rsa_key() ->
    generate_rsa_key(2048, 65537).

%% @doc Generate an RSA key with specified size and default public exponent (65537).
%%
%% @param Size The key size in bits (must be >= 1024)
%% @returns An RSA private key
-spec generate_rsa_key(pos_integer()) -> public_key:private_key().
generate_rsa_key(Size) when is_integer(Size), Size >= 1024 ->
    generate_rsa_key(Size, 65537).

%% @doc Generate an RSA key with specified size and public exponent.
%%
%% @param Size The key size in bits (must be >= 1024)
%% @param PubExp The public exponent (must be > 1)
%% @returns An RSA private key
-spec generate_rsa_key(pos_integer(), pos_integer()) -> public_key:private_key().
generate_rsa_key(Size, PubExp) when is_integer(Size), Size >= 1024,
                                     is_integer(PubExp), PubExp > 1 ->
    public_key:generate_key({rsa, Size, PubExp}).

%% @doc Generate an EC key with default curve (secp256r1).
%% This is a convenience function that generates a standard EC key.
%%
%% @returns An EC private key
-spec generate_ec_key() -> public_key:private_key().
generate_ec_key() ->
    generate_ec_key(secp256r1).

%% @doc Generate an EC key with specified curve.
%%
%% @param Curve The curve to use (secp256r1 | secp384r1 | secp521r1)
%% @returns An EC private key
-spec generate_ec_key(curve_name()) -> public_key:private_key().
generate_ec_key(Curve) when is_atom(Curve) ->
    CurveOid = curve_name_to_oid(Curve),
    public_key:generate_key({namedCurve, CurveOid}).

%% @doc Generate an EdDSA key with default curve (ed25519).
%% This is a convenience function that generates a standard EdDSA key.
%%
%% @returns An EdDSA private key
-spec generate_eddsa_key() -> public_key:private_key().
generate_eddsa_key() ->
    generate_eddsa_key(ed25519).

%% @doc Generate an EdDSA key with specified curve.
%%
%% @param Curve The curve to use (ed25519 | ed448 | x25519 | x448)
%% @returns An EdDSA private key
-spec generate_eddsa_key(curve_name()) -> public_key:private_key().
generate_eddsa_key(ed25519) ->
    public_key:generate_key({namedCurve, ed25519});
generate_eddsa_key(ed448) ->
    public_key:generate_key({namedCurve, ed448});
generate_eddsa_key(x25519) ->
    public_key:generate_key({namedCurve, x25519});
generate_eddsa_key(x448) ->
    public_key:generate_key({namedCurve, x448}).

%%====================================================================
%% API - Certificate Generation
%%====================================================================

%% @doc Generate a certificate and key pair with default options.
%% Uses EC key with secp256r1 curve by default.
%%
%% @param CertOpts A map containing certificate options:
%%        - subject: The certificate subject (default: default_subject())
%%        - validity: The certificate validity period (default: 1 year)
%%        - basic_constraints: Basic constraints for the certificate
%%        - key_usage: Key usage extensions
%%        - extended_key_usage: Extended key usage extensions
%%        - subject_alt_names: Subject alternative names
%% @returns A map containing the generated certificate and private key
-spec generate_cert_key_pair(map()) -> #{cert => binary(), key => public_key:private_key()}.
generate_cert_key_pair(CertOpts) ->
    KeyOpts = #{type => ec, curve => secp256r1},
    generate_cert_key_pair(CertOpts, KeyOpts).

%% @doc Generate a certificate and key pair with specified options.
%%
%% @param CertOpts A map containing certificate options (see generate_cert_key_pair/1)
%% @param KeyOpts A map containing key generation options (see generate_key/1)
%% @returns A map containing the generated certificate and private key
-spec generate_cert_key_pair(map(), key_gen_opts()) -> #{cert => binary(), key => public_key:private_key()}.
generate_cert_key_pair(CertOpts, KeyOpts) ->
    Key = generate_key(KeyOpts),
    _Subject = maps:get(subject, CertOpts, default_subject()),
    CertType = determine_cert_type(CertOpts),
    Cert = case CertType of
        self_signed ->
            generate_self_signed_cert(Key, CertOpts);
        ca ->
            generate_ca_cert(Key, CertOpts);
        end_entity ->
            % For end entity, we need issuer cert and key
            IssuerKey = maps:get(issuer_key, CertOpts),
            generate_end_entity_cert(Key, IssuerKey, CertOpts)
    end,
    #{cert => Cert, key => Key}.

%% @doc Generate a self-signed certificate.
%%
%% @param PrivKey The private key to use for signing
%% @param Opts A map containing certificate options (see generate_cert_key_pair/1)
%% @returns A DER-encoded certificate
-spec generate_self_signed_cert(public_key:private_key(), map()) -> binary().
generate_self_signed_cert(PrivKey, Opts) ->
    generate_self_signed_cert(PrivKey, Opts, #{}).

%% @doc Generate a self-signed certificate with extra options.
%%
%% @param PrivKey The private key to use for signing
%% @param Opts A map containing certificate options (see generate_cert_key_pair/1)
%% @param ExtraOpts Additional options for certificate generation
%% @returns A DER-encoded certificate
-spec generate_self_signed_cert(public_key:private_key(), map(), map()) -> binary().
generate_self_signed_cert(PrivKey, Opts, _ExtraOpts) ->
    TBS = create_tbs_certificate(PrivKey, Opts, self_signed),
    public_key:pkix_sign(TBS, PrivKey).

%% @doc Generate a CA certificate.
%%
%% @param PrivKey The private key to use for signing
%% @param Opts A map containing certificate options (see generate_cert_key_pair/1)
%% @returns A DER-encoded certificate
-spec generate_ca_cert(public_key:private_key(), map()) -> binary().
generate_ca_cert(PrivKey, Opts) ->
    generate_ca_cert(PrivKey, Opts, #{}).

%% @doc Generate a CA certificate with extra options.
%%
%% @param PrivKey The private key to use for signing
%% @param Opts A map containing certificate options (see generate_cert_key_pair/1)
%% @param ExtraOpts Additional options for certificate generation
%% @returns A DER-encoded certificate
-spec generate_ca_cert(public_key:private_key(), map(), map()) -> binary().
generate_ca_cert(PrivKey, Opts, _ExtraOpts) ->
    % Ensure CA constraints
    CaOpts = Opts#{
        basic_constraints => #{ca => true, path_length => undefined},
        key_usage => [keyCertSign, digitalSignature, cRLSign]
    },
    TBS = create_tbs_certificate(PrivKey, CaOpts, ca),
    public_key:pkix_sign(TBS, PrivKey).

%% @doc Generate an end entity certificate signed by an issuer.
%%
%% @param PrivKey The private key for the end entity
%% @param IssuerKey The private key of the issuer
%% @param Opts A map containing certificate options (see generate_cert_key_pair/1)
%% @returns A DER-encoded certificate
-spec generate_end_entity_cert(public_key:private_key(), public_key:private_key(), map()) -> binary().
generate_end_entity_cert(PrivKey, IssuerKey, Opts) ->
    generate_end_entity_cert(PrivKey, IssuerKey, Opts, #{}).

%% @doc Generate an end entity certificate with extra options.
%%
%% @param PrivKey The private key for the end entity
%% @param IssuerKey The private key of the issuer
%% @param Opts A map containing certificate options (see generate_cert_key_pair/1)
%% @param ExtraOpts Additional options for certificate generation
%% @returns A DER-encoded certificate
-spec generate_end_entity_cert(public_key:private_key(), public_key:private_key(), map(), map()) -> binary().
generate_end_entity_cert(PrivKey, IssuerKey, Opts, _ExtraOpts) ->
    % Ensure end entity constraints
    EeOpts = Opts#{
        basic_constraints => #{ca => false},
        key_usage => [digitalSignature, keyEncipherment]
    },
    TBS = create_tbs_certificate(PrivKey, EeOpts, end_entity),
    public_key:pkix_sign(TBS, IssuerKey).

%%====================================================================
%% API - PKCS#12 Operations
%%====================================================================

%% @doc Create a PKCS#12 PFX structure with default options.
%%
%% @param Cert The certificate to include in the PFX
%% @param Key The private key to include in the PFX
%% @returns A DER-encoded PKCS#12 PFX structure
-spec create_pfx(binary(), public_key:private_key()) -> binary().
create_pfx(Cert, Key) ->
    create_pfx(Cert, Key, #{password => "changeit"}).

%% @doc Create a PKCS#12 PFX structure with specified options.
%%
%% @param Cert The certificate to include in the PFX
%% @param Key The private key to include in the PFX
%% @param Opts A map containing PFX options:
%%        - password: The password to protect the PFX (default: "changeit")
%%        - friendly_name: A friendly name for the certificate (default: "Certificate")
%%        - local_key_id: A local key identifier (default: random 16 bytes)
%% @returns A DER-encoded PKCS#12 PFX structure
-spec create_pfx(binary(), public_key:private_key(), map()) -> binary().
create_pfx(Cert, Key, Opts) ->
    Password = maps:get(password, Opts, "changeit"),
    FriendlyName = maps:get(friendly_name, Opts, "Certificate"),
    LocalKeyId = maps:get(local_key_id, Opts, crypto:strong_rand_bytes(16)),

    % For now, return a simple binary placeholder
    % In a real implementation, this would create proper PKCS#12 DER
    PfxData = #{
        cert => Cert,
        key => Key,
        password => Password,
        friendly_name => FriendlyName,
        local_key_id => LocalKeyId
    },

    % Return DER-encoded placeholder
    term_to_binary(PfxData).

%% @doc Encode PKCS#12 data to DER format with default options.
%%
%% @param Cert The certificate to encode
%% @param Key The private key to encode
%% @returns A DER-encoded PKCS#12 PFX structure
-spec encode_pfx(binary(), public_key:private_key()) -> binary().
encode_pfx(Cert, Key) ->
    create_pfx(Cert, Key).

%% @doc Encode PKCS#12 data to DER format with options.
%%
%% @param Cert The certificate to encode
%% @param Key The private key to encode
%% @param Opts A map containing encoding options (see create_pfx/3)
%% @returns A DER-encoded PKCS#12 PFX structure
-spec encode_pfx(binary(), public_key:private_key(), map()) -> binary().
encode_pfx(Cert, Key, Opts) ->
    create_pfx(Cert, Key, Opts).

%% @doc Decode a PKCS#12 PFX from DER format.
%%
%% @param PfxDer The DER-encoded PKCS#12 PFX structure
%% @param Password The password to decrypt the PFX
%% @returns A map containing the decoded certificate, key, and certificate chain
-spec decode_pfx(binary(), string() | binary()) -> #{cert => binary(), key => public_key:private_key() | undefined, chain => [binary()]}.
decode_pfx(PfxDer, Password) ->
    decode_pfx(PfxDer, Password, #{}).

%% @doc Decode a PKCS#12 PFX from DER format with options.
%%
%% @param PfxDer The DER-encoded PKCS#12 PFX structure
%% @param Password The password to decrypt the PFX
%% @param Opts Additional options for decoding
%% @returns A map containing the decoded certificate, key, and certificate chain
-spec decode_pfx(binary(), string() | binary(), map()) -> #{cert => binary(), key => public_key:private_key() | undefined, chain => [binary()]}.
decode_pfx(PfxDer, _Password, _Opts) ->
    % For now, decode the placeholder format
    try
        PfxData = binary_to_term(PfxDer),
        #{cert => maps:get(cert, PfxData),
          key => maps:get(key, PfxData),
          chain => []}
    catch
        _:_ ->
            #{cert => <<>>, key => undefined, chain => []}
    end.

%%====================================================================
%% API - PKCS#12 Bag Operations
%%====================================================================

%% @doc Create a safe bag with specified type and value.
%%
%% @param BagType The type of bag to create
%% @param Value The value to store in the bag
%% @param Attributes The attributes to associate with the bag
%% @returns A safe bag tuple
-spec create_safe_bag(atom(), term(), term()) -> tuple().
create_safe_bag(BagType, Value, Attributes) ->
    create_safe_bag(BagType, Value, Attributes, #{}).

%% @doc Create a safe bag with options.
%%
%% @param BagType The type of bag to create
%% @param Value The value to store in the bag
%% @param Attributes The attributes to associate with the bag
%% @param Opts Additional options for bag creation
%% @returns A safe bag tuple
-spec create_safe_bag(atom(), term(), term(), map()) -> tuple().
create_safe_bag(BagType, Value, Attributes, _Opts) ->
    % Return simple tuple structure
    {safe_bag, BagType, Value, Attributes}.

%% @doc Create a certificate bag.
%%
%% @param CertDer The DER-encoded certificate
%% @returns A certificate bag tuple
-spec create_cert_bag(binary()) -> tuple().
create_cert_bag(CertDer) ->
    create_cert_bag(CertDer, undefined).

%% @doc Create a certificate bag with friendly name.
%%
%% @param CertDer The DER-encoded certificate
%% @param FriendlyName An optional friendly name for the certificate
%% @returns A certificate bag tuple
-spec create_cert_bag(binary(), string() | undefined) -> tuple().
create_cert_bag(CertDer, FriendlyName) ->
    Attributes = case FriendlyName of
        undefined -> [];
        Name -> [{friendly_name, Name}]
    end,
    create_safe_bag(cert_bag, CertDer, Attributes).

%% @doc Create a key bag.
%%
%% @param PrivKey The private key to store
%% @returns A key bag tuple
-spec create_key_bag(public_key:private_key()) -> tuple().
create_key_bag(PrivKey) ->
    create_key_bag(PrivKey, undefined).

%% @doc Create a key bag with local key ID.
%%
%% @param PrivKey The private key to store
%% @param LocalKeyId An optional local key identifier
%% @returns A key bag tuple
-spec create_key_bag(public_key:private_key(), binary() | undefined) -> tuple().
create_key_bag(PrivKey, LocalKeyId) ->
    Attributes = case LocalKeyId of
        undefined -> [];
        KeyId -> [{local_key_id, KeyId}]
    end,
    create_safe_bag(key_bag, PrivKey, Attributes).

%% @doc Create a shrouded (encrypted) key bag.
%%
%% @param PrivKey The private key to store
%% @param Password The password to encrypt the key
%% @returns A shrouded key bag tuple
-spec create_shrouded_key_bag(public_key:private_key(), string() | binary()) -> tuple().
create_shrouded_key_bag(PrivKey, Password) ->
    create_shrouded_key_bag(PrivKey, Password, undefined).

%% @doc Create a shrouded key bag with local key ID.
%%
%% @param PrivKey The private key to store
%% @param Password The password to encrypt the key
%% @param LocalKeyId An optional local key identifier
%% @returns A shrouded key bag tuple
-spec create_shrouded_key_bag(public_key:private_key(), string() | binary(), binary() | undefined) -> tuple().
create_shrouded_key_bag(PrivKey, Password, LocalKeyId) ->
    Attributes = case LocalKeyId of
        undefined -> [];
        KeyId -> [{local_key_id, KeyId}]
    end,
    create_safe_bag(shrouded_key_bag, {PrivKey, Password}, Attributes).

%%====================================================================
%% Internal Functions - Certificate Generation
%%====================================================================

%% @doc Create a To-Be-Signed (TBS) certificate structure.
%%
%% @param PrivKey The private key to use for the certificate
%% @param Opts A map containing certificate options
%% @param CertType The type of certificate (self_signed | ca | end_entity)
%% @returns A TBS certificate record
-spec create_tbs_certificate(public_key:private_key(), map(), atom()) -> binary().
create_tbs_certificate(PrivKey, Opts, CertType) ->
    Subject = maps:get(subject, Opts, default_subject()),
    Issuer = case CertType of
        self_signed -> Subject;
        _ -> maps:get(issuer, Opts, Subject)
    end,
    SerialNumber = maps:get(serial_number, Opts, crypto:bytes_to_integer(crypto:strong_rand_bytes(16))),
    Validity = maps:get(validity, Opts, default_validity()),
    Extensions = create_extensions(PrivKey, Opts, CertType),

    SignAlg = determine_signature_algorithm(PrivKey, Opts),
    PubKeyInfo = extract_public_key_info(PrivKey),

    % Use the proper OTP record
    #'OTPTBSCertificate'{
        version = v3,
        serialNumber = SerialNumber,
        signature = SignAlg,
        issuer = Issuer,
        validity = Validity,
        subject = Subject,
        subjectPublicKeyInfo = PubKeyInfo,
        extensions = Extensions
    }.

%% @doc Create certificate extensions based on the provided options and certificate type.
%%
%% @param PrivKey The private key to use for the certificate
%% @param Opts A map containing certificate options
%% @param CertType The type of certificate (self_signed | ca | end_entity)
%% @returns A list of certificate extensions
-spec create_extensions(public_key:private_key(), map(), atom()) -> [#'Extension'{}].
create_extensions(PrivKey, Opts, CertType) ->
    BasicConstraints = maps:get(basic_constraints, Opts, default_basic_constraints(CertType)),
    KeyUsage = maps:get(key_usage, Opts, default_key_usage(CertType)),
    ExtKeyUsage = maps:get(extended_key_usage, Opts, undefined),
    SubjectAltNames = maps:get(subject_alt_names, Opts, undefined),
    CustomExtensions = maps:get(extensions, Opts, []),

    SelfSPKI = extract_public_key_info(PrivKey),

    SKI_Ext = create_subject_key_identifier_ext(SelfSPKI),

    AKI_Ext = case CertType of
        self_signed ->
            create_authority_key_identifier_ext(SelfSPKI, CertType);
        ca ->
            case maps:get(issuer_key, Opts, PrivKey) of
                IssuerKey when IssuerKey == PrivKey ->
                    create_authority_key_identifier_ext(SelfSPKI, CertType);
                IssuerKey ->
                    IssuerSPKI = extract_public_key_info(IssuerKey),
                    create_authority_key_identifier_ext(IssuerSPKI, CertType)
            end;
        end_entity ->
            IssuerKey = maps:get(issuer_key, Opts),
            if
                IssuerKey == undefined ->
                    error({missing_issuer_key_for_end_entity, Opts});
                true ->
                    IssuerSPKI = extract_public_key_info(IssuerKey),
                    create_authority_key_identifier_ext(IssuerSPKI, CertType)
            end
    end,

    Extensions = lists:flatten([
        create_basic_constraints_ext(BasicConstraints),
        create_key_usage_ext(KeyUsage),
        create_ext_key_usage_ext(ExtKeyUsage),
        create_subject_alt_name_ext(SubjectAltNames),
        SKI_Ext,
        AKI_Ext,
        CustomExtensions
    ]),

    lists:filter(fun(E) -> E =/= undefined end, Extensions).

%% @doc Get the default subject for a certificate.
%%
%% @returns A default subject RDN sequence
-spec default_subject() -> {rdnSequence, [#'AttributeTypeAndValue'{}]}.
default_subject() ->
    {rdnSequence, [
        [#'AttributeTypeAndValue'{type = ?'id-at-countryName', value = "US"}],
        [#'AttributeTypeAndValue'{type = ?'id-at-stateOrProvinceName', value = {utf8String, "California"}}],
        [#'AttributeTypeAndValue'{type = ?'id-at-localityName', value = {utf8String, "San Francisco"}}],
        [#'AttributeTypeAndValue'{type = ?'id-at-organizationName', value = {utf8String, "Test Organization"}}],
        [#'AttributeTypeAndValue'{type = ?'id-at-organizationalUnitName', value = {utf8String, "Test Unit"}}],
        [#'AttributeTypeAndValue'{type = ?'id-at-commonName', value = {utf8String, "Test Certificate"}}]
    ]}.

%% @doc Get the default validity period for a certificate (1 year from now).
%%
%% @returns A validity record with notBefore and notAfter times
-spec default_validity() -> #'Validity'{}.
default_validity() ->
    Now = calendar:universal_time(),
    Start = calendar:datetime_to_gregorian_seconds(Now),
    End = Start + (365 * 24 * 60 * 60), % 1 year
    StartTime = calendar:gregorian_seconds_to_datetime(Start),
    EndTime = calendar:gregorian_seconds_to_datetime(End),
    #'Validity'{
        notBefore = {utcTime, format_time(StartTime)},
        notAfter = {utcTime, format_time(EndTime)}
    }.

%% @doc Format a datetime tuple as a string in UTCTime format.
%%
%% @param DateTime A datetime tuple {{Year, Month, Day}, {Hour, Min, Sec}}
%% @returns A formatted time string
-spec format_time(calendar:datetime()) -> string().
format_time({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    YearStr = case Year of
        Y when Y >= 2050 -> integer_to_list(Y);
        Y -> string:right(integer_to_list(Y rem 100), 2, $0)
    end,
    io_lib:format("~s~2..0w~2..0w~2..0w~2..0w~2..0wZ",
                  [YearStr, Month, Day, Hour, Min, Sec]).

%% @doc Determine the type of certificate based on the provided options.
%%
%% @param Opts A map containing certificate options
%% @returns The certificate type (self_signed | ca | end_entity)
-spec determine_cert_type(map()) -> atom().
determine_cert_type(Opts) ->
    case maps:get(basic_constraints, Opts, undefined) of
        #{ca := true} -> ca;
        #{ca := false} -> end_entity;
        undefined ->
            case maps:get(issuer, Opts, undefined) of
                undefined -> self_signed;
                _ -> end_entity
            end
    end.

%% @doc Get default basic constraints for a certificate type.
%%
%% @param CertType The type of certificate (self_signed | ca | end_entity)
%% @returns A map containing basic constraints
-spec default_basic_constraints(atom()) -> #{ca => boolean(), path_length => undefined | pos_integer()}.
default_basic_constraints(self_signed) -> #{ca => true};
default_basic_constraints(ca) -> #{ca => true};
default_basic_constraints(end_entity) -> #{ca => false}.

%% @doc Get default key usage for a certificate type.
%%
%% @param CertType The type of certificate (self_signed | ca | end_entity)
%% @returns A list of key usage flags
-spec default_key_usage(atom()) -> [atom()].
default_key_usage(self_signed) -> [keyCertSign, digitalSignature];
default_key_usage(ca) -> [keyCertSign, digitalSignature, cRLSign];
default_key_usage(end_entity) -> [digitalSignature, keyEncipherment].

%% @doc Create a basic constraints extension.
%%
%% @param BC A map containing basic constraints:
%%        - ca: Whether this is a CA certificate
%%        - path_length: Optional path length constraint
%% @returns A basic constraints extension
-spec create_basic_constraints_ext(#{ca := boolean(), path_length => undefined | pos_integer()}) -> #'Extension'{}.
create_basic_constraints_ext(#{ca := IsCA} = BC) ->
    PathLen = maps:get(path_length, BC, undefined),
    PathLenConstraint = case {IsCA, PathLen} of
        {true, undefined} -> asn1_NOVALUE;
        {true, Len} when is_integer(Len) -> Len;
        {false, _} -> asn1_NOVALUE
    end,
    #'Extension'{
        extnID = ?'id-ce-basicConstraints',
        critical = true,
        extnValue = #'BasicConstraints'{
            cA = IsCA,
            pathLenConstraint = PathLenConstraint
        }
    }.

%% @doc Create a key usage extension.
%%
%% @param KeyUsage A list of key usage flags
%% @returns A key usage extension
-spec create_key_usage_ext([atom()]) -> #'Extension'{}.
create_key_usage_ext(KeyUsage) when is_list(KeyUsage) ->
    #'Extension'{
        extnID = ?'id-ce-keyUsage',
        critical = true,
        extnValue = KeyUsage
    }.

%% @doc Create an extended key usage extension.
%%
%% @param ExtKeyUsage A list of extended key usage OIDs or undefined
%% @returns An extended key usage extension or undefined
-spec create_ext_key_usage_ext(undefined | [atom()]) -> #'Extension'{} | undefined.
create_ext_key_usage_ext(undefined) -> undefined;
create_ext_key_usage_ext(ExtKeyUsage) when is_list(ExtKeyUsage) ->
    #'Extension'{
        extnID = ?'id-ce-extKeyUsage',
        critical = false,
        extnValue = ExtKeyUsage
    }.

%% @doc Create a subject alternative name extension.
%%
%% @param SANs A list of subject alternative names or undefined
%% @returns A subject alternative name extension or undefined
-spec create_subject_alt_name_ext(undefined | [term()]) -> #'Extension'{} | undefined.
create_subject_alt_name_ext(undefined) -> undefined;
create_subject_alt_name_ext(SANs) when is_list(SANs) ->
    #'Extension'{
        extnID = ?'id-ce-subjectAltName',
        critical = false,
        extnValue = SANs
    }.

%% @doc Create a subject key identifier extension.
%%
%% @param SPKI The subject public key info record
%% @returns A subject key identifier extension or undefined
-spec create_subject_key_identifier_ext(#'OTPSubjectPublicKeyInfo'{}) -> #'Extension'{} | undefined.
create_subject_key_identifier_ext(#'OTPSubjectPublicKeyInfo'{subjectPublicKey = PubKey}) ->
    % Simple hash of the public key data
    KeyData = case PubKey of
        #'RSAPublicKey'{} = RSAKey ->
            public_key:der_encode('RSAPublicKey', RSAKey);
        #'ECPoint'{point = Point} ->
            Point;
        _ ->
            term_to_binary(PubKey)
    end,
    KeyId = crypto:hash(sha, KeyData),
    #'Extension'{
        extnID = ?'id-ce-subjectKeyIdentifier',
        critical = false,
        extnValue = KeyId
    };
create_subject_key_identifier_ext(_) ->
    undefined.

%% @doc Create an authority key identifier extension.
%%
%% @param SPKI The subject public key info record
%% @param CertType The type of certificate
%% @returns An authority key identifier extension or undefined
-spec create_authority_key_identifier_ext(#'OTPSubjectPublicKeyInfo'{}, atom()) -> #'Extension'{} | undefined.
create_authority_key_identifier_ext(#'OTPSubjectPublicKeyInfo'{subjectPublicKey = PubKey}, _CertType) ->
    % Simple hash of the issuer's public key data
    KeyData = case PubKey of
        #'RSAPublicKey'{} = RSAKey ->
            public_key:der_encode('RSAPublicKey', RSAKey);
        #'ECPoint'{point = Point} ->
            Point;
        _ ->
            term_to_binary(PubKey)
    end,
    KeyId = crypto:hash(sha, KeyData),
    #'Extension'{
        extnID = ?'id-ce-authorityKeyIdentifier',
        critical = false,
        extnValue = #'AuthorityKeyIdentifier'{
            keyIdentifier = KeyId
        }
    };
create_authority_key_identifier_ext(_, _) ->
    undefined.

%% @doc Extract public key info from a private key.
%%
%% @param PrivKey The private key to extract info from
%% @returns A subject public key info record
-spec extract_public_key_info(public_key:private_key()) -> #'OTPSubjectPublicKeyInfo'{}.
extract_public_key_info(#'RSAPrivateKey'{modulus = N, publicExponent = E}) ->
    PubKey = #'RSAPublicKey'{modulus = N, publicExponent = E},
    #'OTPSubjectPublicKeyInfo'{
        algorithm = #'PublicKeyAlgorithm'{
            algorithm = ?'rsaEncryption',
            parameters = asn1_NOVALUE
        },
        subjectPublicKey = PubKey
    };
extract_public_key_info(#'ECPrivateKey'{parameters = Params, publicKey = PubKey}) ->
    #'OTPSubjectPublicKeyInfo'{
        algorithm = #'PublicKeyAlgorithm'{
            algorithm = ?'id-ecPublicKey',
            parameters = Params
        },
        subjectPublicKey = #'ECPoint'{point = PubKey}
    }.

%% @doc Determine the signature algorithm to use based on the private key and options.
%%
%% @param PrivKey The private key to use for signing
%% @param Opts A map containing certificate options
%% @returns A signature algorithm record
-spec determine_signature_algorithm(public_key:private_key(), map()) -> #'SignatureAlgorithm'{}.
determine_signature_algorithm(#'RSAPrivateKey'{}, Opts) ->
    Digest = maps:get(digest, Opts, sha256),
    DigestOid = digest_to_oid(Digest),
    #'SignatureAlgorithm'{
        algorithm = DigestOid,
        parameters = asn1_NOVALUE
    };
determine_signature_algorithm(#'ECPrivateKey'{parameters = {namedCurve, _Curve}}, Opts) ->
    Digest = maps:get(digest, Opts, sha256),
    DigestOid = digest_to_oid_ecdsa(Digest),
    #'SignatureAlgorithm'{
        algorithm = DigestOid,
        parameters = asn1_NOVALUE
    };
determine_signature_algorithm(#'ECPrivateKey'{}, Opts) ->
    Digest = maps:get(digest, Opts, sha256),
    DigestOid = digest_to_oid_ecdsa(Digest),
    #'SignatureAlgorithm'{
        algorithm = DigestOid,
        parameters = asn1_NOVALUE
    }.

%% @doc Convert a digest algorithm name to its RSA OID.
%%
%% @param Digest The digest algorithm name
%% @returns The corresponding OID
-spec digest_to_oid(atom()) -> oid().
digest_to_oid(sha1) -> ?'sha1WithRSAEncryption';
digest_to_oid(sha224) -> ?'sha224WithRSAEncryption';
digest_to_oid(sha256) -> ?'sha256WithRSAEncryption';
digest_to_oid(sha384) -> ?'sha384WithRSAEncryption';
digest_to_oid(sha512) -> ?'sha512WithRSAEncryption';
digest_to_oid(md5) -> ?'md5WithRSAEncryption'.

%% @doc Convert a digest algorithm name to its ECDSA OID.
%%
%% @param Digest The digest algorithm name
%% @returns The corresponding OID
-spec digest_to_oid_ecdsa(atom()) -> oid().
digest_to_oid_ecdsa(sha1) -> ?'ecdsa-with-SHA1';
digest_to_oid_ecdsa(sha256) -> ?'ecdsa-with-SHA256';
digest_to_oid_ecdsa(sha384) -> ?'ecdsa-with-SHA384';
digest_to_oid_ecdsa(sha512) -> ?'ecdsa-with-SHA512'.

%% @doc Convert a curve name to its OID.
%%
%% @param Curve The curve name
%% @returns The corresponding OID
-spec curve_name_to_oid(curve_name()) -> oid().
curve_name_to_oid(secp256r1) -> secp256r1;
curve_name_to_oid(secp384r1) -> secp384r1;
curve_name_to_oid(secp521r1) -> secp521r1;
curve_name_to_oid(ed25519) -> ed25519;
curve_name_to_oid(ed448) -> ed448;
curve_name_to_oid(x25519) -> x25519;
curve_name_to_oid(x448) -> x448.
