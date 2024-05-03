<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

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
# FIPS mode

[](){: #fips } This chapter describes FIPS mode support in the crypto
application.

## Background

OpenSSL can be built to provide FIPS 140-2 validated cryptographic services. It
is not the OpenSSL application that is validated, but a special software
component called the OpenSSL FIPS Object Module. However applications do not use
this Object Module directly, but through the regular API of the OpenSSL library.

The crypto application supports using OpenSSL in FIPS mode. In this scenario
only the validated algorithms provided by the Object Module are accessible,
other algorithms usually available in OpenSSL (like md5) or implemented in the
Erlang code (like SRP) are disabled.

## Enabling FIPS mode

1. Build or install the FIPS Object Module and a FIPS enabled OpenSSL library.

You should read and precisely follow the instructions of the
[Security Policy](http://csrc.nist.gov/groups/STM/cmvp/documents/140-1/140sp/140sp1747.pdf)
and [User Guide](https://www.openssl.org/docs/fips/UserGuide-2.0.pdf).

> #### Warning {: .warning }
>
> It is very easy to build a working OpenSSL FIPS Object Module and library from
> the source. However it _does not_ qualify as FIPS 140-2 validated if the
> numerous restrictions in the Security Policy are not properly followed.

1. Configure and build Erlang/OTP with FIPS support:

```text
$ cd $ERL_TOP
$ ./otp_build configure --enable-fips
...
checking for FIPS_mode_set... yes
...
$ make
```

If `FIPS_mode_set` returns `no` the OpenSSL library is not FIPS enabled and
crypto won't support FIPS mode either.

1. Set the `fips_mode` configuration setting of the crypto application to `true`
   _before loading the crypto module_.

The best place is in the `sys.config` system configuration file of the release.

1. Start and use the crypto application as usual. However take care to avoid the
   non-FIPS validated algorithms, they will all throw exception `not_supported`.

Entering and leaving FIPS mode on a node already running crypto is not
supported. The reason is that OpenSSL is designed to prevent an application
requesting FIPS mode to end up accidentally running in non-FIPS mode. If
entering FIPS mode fails (e.g. the Object Module is not found or is compromised)
any subsequent use of the OpenSSL API would terminate the emulator.

An on-the-fly FIPS mode change would thus have to be performed in a critical
section protected from any concurrently running crypto operations. Furthermore
in case of failure all crypto calls would have to be disabled from the Erlang or
nif code. This would be too much effort put into this not too important feature.

## Incompatibilities with regular builds

The Erlang API of the crypto application is identical regardless of building
with or without FIPS support. However the nif code internally uses a different
OpenSSL API.

This means that the context (an opaque type) returned from streaming crypto
functions (`hash_(init|update|final)`, `hmac_(init|update|final)` and
`stream_(init|encrypt|decrypt)`) is different and incompatible with regular
builds when compiling crypto with FIPS support.

## Common caveats

In FIPS mode non-validated algorithms are disabled. This may cause some
unexpected problems in application relying on crypto.

> #### Warning {: .warning }
>
> Do not try to work around these problems by using alternative implementations
> of the missing algorithms\! An application can only claim to be using a FIPS
> 140-2 validated cryptographic module if it uses it exclusively for every
> cryptographic operation.

### Restrictions on key sizes

Although public key algorithms are supported in FIPS mode they can only be used
with secure key sizes. The Security Policy requires the following minimum
values:

- **RSA** - 1024 bit

- **DSS** - 1024 bit

- **EC algorithms** - 160 bit

### Restrictions on elliptic curves

The Erlang API allows using arbitrary curve parameters, but in FIPS mode only
those allowed by the Security Policy shall be used.

### Avoid md5 for hashing

Md5 is a popular choice as a hash function, but it is not secure enough to be
validated. Try to use sha instead wherever possible.

For exceptional, non-cryptographic use cases one may consider switching to
`erlang:md5/1` as well.

### Certificates and encrypted keys

As md5 is not available in FIPS mode it is only possible to use certificates
that were signed using sha hashing. When validating an entire certificate chain
all certificates (including the root CA's) must comply with this rule.

For similar dependency on the md5 and des algorithms most encrypted private keys
in PEM format do not work either. However, the PBES2 encryption scheme allows
the use of stronger FIPS verified algorithms which is a viable alternative.

### SNMP v3 limitations

It is only possible to use `usmHMACSHAAuthProtocol` and `usmAesCfb128Protocol`
for authentication and privacy respectively in FIPS mode. The snmp application
however won't restrict selecting disabled protocols in any way, and using them
would result in run time crashes.

### TLS 1.2 is required

All SSL and TLS versions prior to TLS 1.2 use a combination of md5 and sha1
hashes in the handshake for various purposes:

- Authenticating the integrity of the handshake messages.
- In the exchange of DH parameters in cipher suites providing non-anonymous PFS
  (perfect forward secrecy).
- In the PRF (pseud-random function) to generate keying materials in cipher
  suites not using PFS.

OpenSSL handles these corner cases in FIPS mode, however the Erlang crypto and
ssl applications are not prepared for them and therefore you are limited to TLS
1.2 in FIPS mode.

On the other hand it worth mentioning that at least all cipher suites that would
rely on non-validated algorithms are automatically disabled in FIPS mode.

> #### Note {: .info }
>
> Certificates using weak (md5) digests may also cause problems in TLS. Although
> TLS 1.2 has an extension for specifying which type of signatures are accepted,
> and in FIPS mode the ssl application will use it properly, most TLS
> implementations ignore this extension and simply send whatever certificates
> they were configured with.
