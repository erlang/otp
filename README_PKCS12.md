# PKCS#12 Support in Erlang/OTP 28

## Overview

This implementation provides comprehensive PKCS#12 (Personal Information Exchange) functionality for Erlang/OTP 28's `public_key` module. PKCS#12 is a binary format for storing and transporting cryptographic objects including private keys, certificates, and certificate chains, commonly used in enterprise environments and web browsers.

## Features

### Key Generation
- **RSA Keys**: Configurable key sizes (1024-4096 bits) and public exponents
- **Elliptic Curve Keys**: Support for NIST curves (secp256r1, secp384r1, secp521r1)
- **Edwards Curve Keys**: Support for Ed25519, Ed448, X25519, X448
- **Flexible API**: Generate keys with simple defaults or detailed configuration

### Certificate Generation
- **Self-signed Certificates**: Root CA and standalone certificates
- **CA Certificates**: Certificate Authority certificates with proper constraints
- **End Entity Certificates**: Server and client certificates with appropriate extensions
- **X.509 Extensions**: Support for Subject Alternative Names, Key Usage, Extended Key Usage, Basic Constraints
- **Certificate Chains**: Build complete PKI hierarchies

### PKCS#12 Operations
- **PFX Creation**: Package certificates and private keys into PKCS#12 files
- **Password Protection**: Secure private keys with password-based encryption
- **MAC Verification**: Integrity protection using HMAC-SHA1/SHA256
- **Friendly Names**: Human-readable certificate names
- **Local Key IDs**: Binary identifiers linking certificates and keys
- **Decoding**: Extract certificates and keys from existing PKCS#12 files

## API Reference

### Key Generation Functions

#### Basic Key Generation

```erlang
%% Generate keys with default parameters
RSAKey = public_key:pkcs12_generate_rsa_key(),           % 2048-bit, e=65537
ECKey = public_key:pkcs12_generate_ec_key(),             % secp256r1
EdKey = public_key:pkcs12_generate_eddsa_key(),          % ed25519

%% Generate keys with specific parameters
RSA4096 = public_key:pkcs12_generate_rsa_key(4096),      % 4096-bit RSA
ECP384 = public_key:pkcs12_generate_ec_key(secp384r1),   % secp384r1 curve
Ed448 = public_key:pkcs12_generate_eddsa_key(ed448),     % Ed448 curve
```

#### Advanced Key Generation

```erlang
%% Generate keys with option maps
RSAOpts = #{type => rsa, size => 3072, public_exponent => 65537},
ECOpts = #{type => ec, curve => secp384r1},
EdOpts = #{type => eddsa, curve => ed25519},

RSAKey = public_key:pkcs12_generate_key(RSAOpts),
ECKey = public_key:pkcs12_generate_key(ECOpts),
EdKey = public_key:pkcs12_generate_key(EdOpts).
```

### Certificate Generation Functions

#### Self-signed Certificates

```erlang
Key = public_key:pkcs12_generate_ec_key(secp256r1),

CertOpts = #{
    subject => create_subject("My Certificate", "example.com"),
    validity => {{{2024,1,1},{0,0,0}}, {{2025,1,1},{0,0,0}}},
    key_usage => [digitalSignature, keyEncipherment],
    subject_alt_names => [{dNSName, "example.com"}]
},

Cert = public_key:pkcs12_generate_self_signed_cert(Key, CertOpts).
```

#### CA Certificates

```erlang
CAKey = public_key:pkcs12_generate_rsa_key(4096),

CAOpts = #{
    subject => create_subject("Root CA", "ca.example.com"),
    validity => {{{2024,1,1},{0,0,0}}, {{2034,1,1},{0,0,0}}},  % 10 years
    key_usage => [keyCertSign, digitalSignature, cRLSign],
    basic_constraints => #{ca => true, path_length => 2}
},

CACert = public_key:pkcs12_generate_ca_cert(CAKey, CAOpts).
```

#### End Entity Certificates

```erlang
ServerKey = public_key:pkcs12_generate_ec_key(secp256r1),

ServerOpts = #{
    subject => create_subject("Server", "www.example.com"),
    validity => {{{2024,1,1},{0,0,0}}, {{2025,1,1},{0,0,0}}},
    key_usage => [digitalSignature, keyEncipherment],
    extended_key_usage => [serverAuth],
    subject_alt_names => [
        {dNSName, "www.example.com"},
        {dNSName, "example.com"},
        {iPAddress, {192,168,1,100}}
    ],
    issuer => IssuerSubject,  % From CA certificate
    issuer_key => CAKey
},

ServerCert = public_key:pkcs12_generate_end_entity_cert(ServerKey, CAKey, ServerOpts).
```

#### Combined Certificate and Key Generation

```erlang
CertOpts = #{
    subject => create_subject("Combined", "combined.example.com"),
    validity => {{{2024,1,1},{0,0,0}}, {{2025,1,1},{0,0,0}}}
},

KeyOpts = #{type => ec, curve => secp256r1},

#{cert := Cert, key := Key} =
    public_key:pkcs12_generate_cert_key_pair(CertOpts, KeyOpts).
```

### PKCS#12 Operations

#### Creating PKCS#12 Files

```erlang
%% Basic PKCS#12 creation (default password "changeit")
PFX = public_key:pkcs12_create_pfx(Cert, Key),

%% Advanced PKCS#12 with custom options
PFXOpts = #{
    password => "MySecurePassword123!",
    friendly_name => "My Certificate",
    local_key_id => crypto:strong_rand_bytes(20),
    iterations => 4096,
    mac_algorithm => sha256
},

AdvancedPFX = public_key:pkcs12_create_pfx(Cert, Key, PFXOpts).
```

#### Decoding PKCS#12 Files

```erlang
#{cert := DecodedCert, key := DecodedKey, chain := CertChain} =
    public_key:pkcs12_decode(PFXData, "MySecurePassword123!").
```

#### Alternative APIs

```erlang
%% Encoding (alias for create_pfx)
PFX = public_key:pkcs12_encode(Cert, Key, Opts),

%% Decoding with options
Result = public_key:pkcs12_decode(PFXData, Password, #{}).
```

## Type Specifications

### Key Generation Options

```erlang
-type pkcs12_key_gen_opts() :: #{
    type => rsa | ec | eddsa,
    size => pos_integer(),           % for RSA (default: 2048)
    public_exponent => pos_integer(),% for RSA (default: 65537)
    curve => pkcs12_curve_name()     % for EC/EdDSA (default: secp256r1)
}.

-type pkcs12_curve_name() :: secp256r1 | secp384r1 | secp521r1 |
                             ed25519 | ed448 | x25519 | x448.
```

### Certificate Options

```erlang
-type pkcs12_cert_opts() :: #{
    subject => term(),
    issuer => term(),
    serial_number => integer(),
    validity => {calendar:datetime(), calendar:datetime()},
    extensions => [#'Extension'{}],
    digest => digest_type(),
    key_usage => [atom()],
    extended_key_usage => [atom()],
    basic_constraints => #{ca => boolean(), path_length => non_neg_integer() | undefined},
    subject_alt_names => [term()],
    issuer_key => private_key()  % for end entity certificates
}.
```

### PKCS#12 Options

```erlang
-type pkcs12_pfx_opts() :: #{
    password => string() | binary(),
    iterations => pos_integer(),     % PBKDF2 iterations (default: 2048)
    mac_algorithm => sha1 | sha224 | sha256 | sha384 | sha512,
    encryption_algorithm => des_ede3_cbc | aes128_cbc | aes192_cbc | aes256_cbc,
    friendly_name => string(),       % human-readable certificate name
    local_key_id => binary()         % binary ID linking cert and key
}.
```

## Examples

### Complete PKI Setup

```erlang
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
        {dNSName, "example.com"}
    ],
    issuer => extract_subject_from_cert(IntermediateCert),
    issuer_key => IntermediateKey
},
ServerCert = public_key:pkcs12_generate_end_entity_cert(ServerKey, IntermediateKey, ServerOpts),

%% Create PKCS#12 file
ServerPFXOpts = #{
    password => "ServerPassword123!",
    friendly_name => "Example Server Certificate",
    iterations => 4096
},
ServerPFX = public_key:pkcs12_create_pfx(ServerCert, ServerKey, ServerPFXOpts).
```

### Multi-curve Demonstration

```erlang
Curves = [secp256r1, secp384r1, secp521r1, ed25519, ed448],

lists:foreach(fun(Curve) ->
    Key = case Curve of
        ed25519 -> public_key:pkcs12_generate_eddsa_key(ed25519);
        ed448 -> public_key:pkcs12_generate_eddsa_key(ed448);
        _ -> public_key:pkcs12_generate_ec_key(Curve)
    end,

    CertOpts = #{
        subject => create_subject(
            atom_to_list(Curve) ++ " Test",
            atom_to_list(Curve) ++ ".example.com"
        ),
        validity => create_validity(365)
    },

    Cert = public_key:pkcs12_generate_self_signed_cert(Key, CertOpts),

    PFXOpts = #{
        password => atom_to_list(Curve) ++ "Password123!",
        friendly_name => atom_to_list(Curve) ++ " Certificate"
    },

    PFX = public_key:pkcs12_create_pfx(Cert, Key, PFXOpts),

    io:format("~s: cert=~w bytes, pfx=~w bytes~n",
              [Curve, byte_size(Cert), byte_size(PFX)])
end, Curves).
```

## Security Considerations

### Password Strength
- Use strong passwords for PKCS#12 files (minimum 12 characters with mixed case, numbers, symbols)
- Consider using key derivation with high iteration counts (4096+ iterations)
- Default iteration count is 2048 (increase for better security)

### Key Security
- Private keys are encrypted using PBES2 with AES-256-CBC and PBKDF2
- MAC verification uses HMAC-SHA1 by default (SHA256 recommended for new files)
- Local Key IDs should be cryptographically random (use crypto:strong_rand_bytes/1)

### Algorithm Selection
- **RSA**: Minimum 2048 bits (3072+ recommended for new deployments)
- **EC**: secp256r1 minimum (secp384r1 or secp521r1 for higher security)
- **EdDSA**: Ed25519 recommended for most uses, Ed448 for highest security

## File Format Compatibility

This implementation generates PKCS#12 files compatible with:
- **OpenSSL**: `openssl pkcs12` commands
- **Java KeyStore**: Can import generated PKCS#12 files
- **Web Browsers**: Chrome, Firefox, Safari, Edge
- **Microsoft Certificate Store**: Windows certificate management
- **Apple Keychain**: macOS and iOS certificate storage

## Building and Testing

### ASN.1 Compilation
The PKCS-12.asn1 module is automatically compiled as part of the public_key build process.

### Running Examples
```bash
# Start Erlang shell
erl

# Compile example
1> c(pkcs12_example).

# Run all examples
2> pkcs12_example:run_all_examples().

# Run specific examples
3> pkcs12_example:demo_key_generation().
4> pkcs12_example:demo_certificate_generation().
5> pkcs12_example:demo_pkcs12_creation().
```

### Testing PKCS#12 Files
```bash
# Test with OpenSSL
openssl pkcs12 -in server.p12 -info -nokeys -passin pass:ServerPassword123!

# View certificate details
openssl pkcs12 -in server.p12 -nokeys -clcerts -passin pass:ServerPassword123! | openssl x509 -text
```

## Implementation Details

### ASN.1 Structures
- **PFX**: Top-level PKCS#12 structure with version, authSafe, and macData
- **AuthenticatedSafe**: Sequence of ContentInfo structures
- **SafeContents**: Sequence of SafeBag structures containing certificates and keys
- **SafeBag**: Individual container for certificates, keys, or other data

### Cryptographic Operations
- **Key Derivation**: PKCS#12 key derivation function with configurable iterations
- **Encryption**: PBES2 with AES-256-CBC for private key protection
- **MAC**: HMAC-SHA1/SHA256 for integrity verification
- **Random Generation**: Cryptographically secure random numbers for salts and IDs

### Dependencies
- **crypto**: Cryptographic operations and random number generation
- **public_key**: ASN.1 encoding/decoding and certificate operations
- **calendar**: Date/time operations for certificate validity

## Standards Compliance

- **RFC 7292**: PKCS #12: Personal Information Exchange Syntax v1.1
- **RFC 5912**: New ASN.1 Modules for PKIX
- **RFC 5208**: PKCS #8: Private-Key Information Syntax
- **RFC 3280**: Internet X.509 Public Key Infrastructure Certificate and CRL Profile
- **RFC 5280**: Internet X.509 Public Key Infrastructure Certificate and CRL Profile (Updated)

## Future Enhancements

- Support for additional encryption algorithms (ChaCha20-Poly1305)
- Enhanced certificate chain handling and validation
- Integration with HSM (Hardware Security Module) support
- PKCS#12 file format validation and repair utilities
- Certificate template system for common use cases
