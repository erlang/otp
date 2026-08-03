<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2026-2026. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->

# TLS Hardening Guide
This guide describes how to harden Transport Layer Security (TLS)
connections configured with the ssl application. It covers
cryptographic algorithm selection, protocol version configuration,
certificate verification, and revocation checking.

> #### Note {: .info }
There can exist trade-offs between
interoperability and security, and between resource consumption and
security. If you make such trade-offs it is important that you make an
informed decision and understand the consequences. Also this guide
will always reflect the status of defaults for the release where it
is published, unless otherwise specifically stated. Hence using older
releases may need more configuration to achieve the same result.

Erlang's TLS implementation provided by the `ssl` application strives to
have secure defaults. That said, we will avoid changing defaults in
non-major releases.

For embedded systems with constrained bandwidth, cipher suites using
Advanced Encryption Standard (AES) with counter cipher block chaining
message authentication code, that is AES-CCM-8 (8-byte authentication
tag) can be configured and provides 64-bit integrity instead of
128-bit.

> #### Warning {: .warning }
Be aware that enabling anything documented as legacy will make your
application less secure.

> #### Warning {: .warning }
Network Security Services Key Log Format (NSS Key Logging)
is a debugging functionality that should not be enabled in
production systems as it defeats the purpose of the TLS protocol.

## Cryptographic Algorithms
The available cryptographic algorithms depend on the version of
OpenSSL cryptolib that Erlang/OTP is built and linked with.

When you configure algorithm options such as
[`supported_groups`](`t:ssl:common_option_tls13/0`), or
[`signature_algs`](`t:ssl:common_option/0`), the ssl application
automatically removes any algorithms not supported by the underlying
OpenSSL cryptolib. The exception is the
[`ciphers`](`t:ssl:common_option/0`) option: user-supplied cipher
suites are not automatically filtered against crypto support, this is
due to the more complex composition of algorithms in cipher suites
pre TLS-1.3. In this case use `ssl:filter_cipher_suites/2`
to remove suites that are not supported by linked `OpenSSL cryptolib`.

> #### Note {: .info }
The TLS protocol is implemented in Erlang, and therefore only the cryptographic
functions come from the `OpenSSL cryptolib`. This reduces the error
surface by excluding buffer overflows and other pointer related errors
that does not exist in Erlang programs.

### Algorithms selection
The TLS protocol typically specifies that the client's algorithm
preference determines the algorithm selection. The server option
[`honor_cipher_order`](`t:ssl:server_option/0`) overrides this for
cipher suite selection in all TLS versions. For TLS-1.2 and legacy
versions, [`honor_ecc_order`](`t:ssl:server_option_pre_tls13/0`)
does the same for ECC curve selection.

### Signature Algorithms
The [`signature_algs`](`t:ssl:signature_algs/0`) option configures
which signature algorithms are acceptable for TLS protocol messages.
The [`signature_algs_cert`](`t:ssl:signature_algs/0`), option
separately configures acceptable algorithms for certificate chain
signatures.

If [`signature_algs`](`t:ssl:signature_algs/0`) is specified but not
[`signature_algs_cert`](`t:ssl:signature_algs/0`) the value of
[`signature_algs`](`t:ssl:signature_algs/0`) will implicitly be used
for [`signature_algs_cert`](`t:ssl:signature_algs/0`); however if none
of them are explicitly specified certificate signatures are verified
against default [`signature_algs`](`t:ssl:signature_algs/0`) (with
additional legacy SHA-1 algorithms — see note below).

> #### Note {: .info }
Currently using the default
[`signature_algs`](`t:ssl:signature_algs/0`) will add
[`signature_algs_cert`](`t:ssl:signature_algs/0`) that allows
signature algorithms using SHA-1 as pseudo random function for
certificate signatures. This is not allowed by default for protocol
signatures. It is expected to be removed after 2030, when such
certificates should have been phased out.

### Post Quantum Algorithms
Post-Quantum Cryptography (PQC) algorithms are designed to resist
attacks from future cryptographically relevant quantum computers
(CRQCs), which are expected to break algorithms such as RSA and ECC.
PQC is only available in TLS-1.3.

> #### Note {: .info }
Even though CRQCs do not yet exist, enabling PQC protects against
"harvest now, decrypt later" attacks where an adversary records
encrypted traffic today for future decryption.

PQC algorithms generally require more computation and network
bandwidth than classical algorithms. Key sizes, signatures, and
ciphertexts are larger.

For key exchange, several ML-KEM hybrid groups are supported. By
default, only x25519mlkem768 is offered — a hybrid that combines
classical X25519 with ML-KEM-768 to balance current security with
future quantum resistance, as the newer algorithms are less
battle-tested.

For authentication, the signature algorithms ML-DSA and SLH-DSA are
supported. ML-DSA is preferred due to faster signing and smaller
signatures. Using PQC signature algorithms requires appropriate PQC
certificates and keys.

## Privacy and Integrity
The protocol version TLS-1.3 is a major upgrade of the protocol
over previous versions, and supporting both TLS-1.3 and
TLS-1.2 will require a configuration including options from both.

TLS 1.3 significantly improves session handling over TLS 1.2 by
reducing the handshake from two round-trips (2-RTT) to one (1-RTT).
It mandates forward secrecy, encrypts more of the handshake, and
introduces 0-RTT resumption for previously visited sites.

The protocol itself will prevent version downgrade when using TLS-1.3
and TLS-1.2, which are the default supported versions. All other TLS
versions still configurable are legacy versions. Datagram transport
layer security, DTLS-1.2 over User Datagram Protocol (UDP) can also be
configured and has functionality similar to TLS-1.2 but will always be
less reliable and may lose application data just like plain UDP
communication. DTLS over other transports can possibly be achieved but
not out of the box. DTLS-1.3 is currently not supported.

### Cipher Suites
TLS-1.3 does not share any cipher suites with previous protocol versions.
You will need to include at least one cipher suite for both
TLS-1.3 and TLS-1.2 to be able to support both versions in the
[`ciphers`](`t:ssl:common_option/0`) option.

TLS 1.3 uses only modern Authenticated Encryption with Associated Data (AEAD)
cipher suites, so does the ssl application's TLS-1.2 default.

### Session Key Renewal
Renegotiation options only apply to versions prior to TLS-1.3.
TLS-1.3 replaces renegotiation with the key update mechanism, which
only handles session key rotation and does not allow renegotiating
cipher suites or certificates mid-connection.

A server can disable client-initiated renegotiation with the
[`client_renegotiation`](`t:ssl:server_option_pre_tls13/0`) option to
avoid possible [DoS-attacks](https://en.wikipedia.org/wiki/Denial-of-service_attack).

By default, the server mitigates renegotiation abuse by enforcing a
12-second delay between client initiated renegotiations.

### Key Exchange Groups
TLS-1.3 decouples key exchange algorithms from cipher suites. The key
exchange algorithms are configured using the
[`supported_groups`](`t:ssl:common_option_tls13/0`) option. In TLS-1.2
in addition to the cipher suites the options
[`eccs`](`t:ssl:common_option_pre_tls13/0`),
[`dh`](`t:ssl:server_option_pre_tls13/0`),
[`dhfile`](`t:ssl:server_option_pre_tls13/0`), are available for
configuring the key exchange.

TLS 1.3 removes static RSA, supporting only ephemeral Diffie-Hellman,
which provides mandatory Perfect Forward Secrecy (PFS),
so does the ssl application's TLS-1.2 default.

### Pre-Shared Keys
> #### Warning {: .warning }
Do not use the same Pre-Shared Key (PSK) across both TLS 1.2 and TLS
1.3, as it is considered unsafe.

In TLS-1.2, PSK is supported by PSK cipher suites, which generally
lack forward secrecy unless combined with DHE or ECDHE key exchange.
In TLS-1.3, PSK are most commonly used together with a key exchange
algorithm, providing forward secrecy, although a PSK only option still
exists.

### Early Data
Early data (0-RTT) in TLS-1.3 is not forward secret and is
vulnerable to replay attacks. The server option
[`early_data`](`t:ssl:server_option/0`) is disabled by default.

> #### Warning {: .warning }
Only enable early data if your application can safely handle
replayed requests (that is for instance, idempotent HTTP methods).

See [Early Data in TLS-1.3](using_ssl.md#early-data-in-tls-1-3) for
mitigation strategies and examples.


## Authenticity
Certificate verification is essential for establishing trust in TLS
connections. The `ssl` application provides several mechanisms for
customizing and strengthening certificate validation.

### Certificate and Keys
Use the [`certs_keys`](`t:ssl:common_option_cert/0`) option to be able
to configure more than one possible certificate for the client or
server. The `best` one compatible with the peer will be chosen.

### Verify Function
The [`verify_fun`](`t:ssl:common_option_cert/0`) option lets you
customize certificate path validation for both TLS clients and
servers. Use it to add application-specific checks beyond the
standard chain verification.

> #### Warning {: .warning }
However, the [`verify_fun`](`t:ssl:common_option_cert/0`) can also be
used to accept certain errors. Doing so is at your own risk and will
weaken the security properties of your application. Legacy client
[`verify`](`t:ssl:client_option_cert/0`) option set to the value
`verify_none` skips all certificate verification errors using
a special [`verify_fun`](`t:ssl:common_option_cert/0`)
and should only be used for testing and debugging purposes.

> #### Note {: .info }
On the server side, `verify_none` is default as client certification
is an optional feature of the protocol and it needs configuring. When
client certification is configured, setting
[`verify`](`t:ssl:server_option_cert/0`) option to `verify_peer`, the
server legacy option
[`fail_if_no_peer_cert`](`t:ssl:server_option_legacy/0`) defaults to
true.

To summarize:
- Always use `verify_peer` on the client side (default since OTP 26)
- Avoid accepting `{bad_cert, _}` errors in [`verify_fun`](`t:ssl:common_option_cert/0`) unless
  there is some edge-case workaround that you have confidence in excluding.
- When enabling client certification on the server, ensure
[`fail_if_no_peer_cert`](`t:ssl:server_option_legacy/0`) is true (default since OTP 26).


### Certificate Revocation Verification
Certificate revocation verification is important for security and
needs to be performed. However it requires some application specific
knowledge to set it up correctly, hence the option
[`crl_check`](`t:ssl:common_option_cert/0`) default to
`false`. Use true to verify the entire certificate chain, or peer to
verify only the peer certificate.
> #### Warning {: .warning }
The `best_effort` value is legacy and insecure — it silently
accepts certificates when revocation status cannot be determined.

The built-in Certificate Revocation List (CRL) cache can
be pre-populated via `ssl_crl_cache:insert/1` or configured to
fetch CRLs from HTTP distribution points in certificates.
For custom CRL sources, implement the
[`ssl_crl_cache_api behaviour`](`m:ssl_crl_cache_api`).

The client can request Online Certificate Status Protocol (OCSP)
stapling from the server with the
[`stapling`](`t:ssl:client_option_cert/0`) option (disabled by
default). In TLS-1.2, only end-entity certificate stapling is
supported as opposed to TLS-1.3. If the server does not provide a
staple, the connection by default fails with `{bad_cert,
missing_ocsp_staple}`.

A custom [`verify_fun`](`t:ssl:common_option_cert/0`) can intercept
this to implement fallback revocation checking (that is, direct CRL
fetch).

> #### Warning {: .warning }
Accepting `{bad_cert, missing_ocsp_staple}` without performing
alternative revocation checking is insecure, as it allows a MITM
attacker to suppress revocation information by omitting the staple.

> #### Note {: .info }
The ssl application does not perform direct OCSP queries (client
contacting the OCSP responder via the certificate's Authority
Information Access (AIA) extension), only OCSP stapling
(server-provided responses in the TLS handshake) is supported to be
verified by the client. The Erlang server has no stapling support.
Also, OCSP support seems to be on the decline, for instance
`Let's Encrypt` has dropped it.
