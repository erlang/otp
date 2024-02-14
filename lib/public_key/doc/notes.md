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
# public_key Release Notes

## Public_Key 1.15

### Fixed Bugs and Malfunctions

- ssl application will validate id-kp-serverAuth and id-kp-clientAuth extended
  key usage only in end entity certificates. public_key application will
  disallow "anyExtendedKeyUsage" for CA certificates that includes the extended
  key usage extension and marks it critical.

  Own Id: OTP-18739

- Modernize ECC handling so that crypto FIPS support works as expected.

  Own Id: OTP-18854

### Improvements and New Features

- Support certificate policies in path_validation - as described by RFC 5280.

  Own Id: OTP-17844 Aux Id: ERIERL-738

- Add more search paths for cacerts on Illumos.

  Own Id: OTP-18814 Aux Id: PR-7435

- Make it possible to handle invalid date formats in the verify_fun for
  pkix_path_validation/3

  Own Id: OTP-18867 Aux Id: GH-7515

## Public_Key 1.14.1

### Fixed Bugs and Malfunctions

- Country name comparison shall be case insensitive

  Own Id: OTP-18718 Aux Id: GH-7546

- Add check to disallow duplicate certs in a path

  Own Id: OTP-18723 Aux Id: GH-6394

## Public_Key 1.14

### Improvements and New Features

- Handling of `on_load` modules during boot has been improved by adding an extra
  step in the boot order for embedded mode that runs all `on_load` handlers,
  instead of relying on explicit invocation of them, later, when the kernel
  supervision tree starts.

  This is mostly a code improvement and OTP internal simplification to avoid
  future bugs and to simplify code maintenance.

  Own Id: OTP-18447

## Public_Key 1.13.3.2

### Fixed Bugs and Malfunctions

- ssl application will validate id-kp-serverAuth and id-kp-clientAuth extended
  key usage only in end entity certificates. public_key application will
  disallow "anyExtendedKeyUsage" for CA certificates that includes the extended
  key usage extension and marks it critical.

  Own Id: OTP-18739

## Public_Key 1.13.3.1

### Fixed Bugs and Malfunctions

- Country name comparison shall be case insensitive

  Own Id: OTP-18718 Aux Id: GH-7546

## Public_Key 1.13.3

### Fixed Bugs and Malfunctions

- As different solutions of verifying certificate revocation exists move the
  decode of 'CRLDistributionPoints' so that it will only be decode. When it is
  actually used in the verification process. This would enable interoperability
  with systems that use certificates with an invalid empty CRLDistributionPoints
  extension that they want to ignore and make verification by other means.

  Own Id: OTP-18316 Aux Id: GH-6402, PR-6883

- public_key:pkix_path_validation validates certificates expiring after 2050

  Own Id: OTP-18356 Aux Id: GH-6403

- Do not leave exit message in message queue after calling `cacerts_load()` on
  MacOS.

  Own Id: OTP-18392 Aux Id: GH-6656

### Improvements and New Features

- Replace size/1 with either tuple_size/1 or byte_size/1

  The [`size/1`](`size/1`) BIF is not optimized by the JIT, and its use can
  result in worse types for Dialyzer.

  When one knows that the value being tested must be a tuple,
  [`tuple_size/1`](`tuple_size/1`) should always be preferred.

  When one knows that the value being tested must be a binary,
  [`byte_size/1`](`byte_size/1`) should be preferred. However,
  [`byte_size/1`](`byte_size/1`) also accepts a bitstring (rounding up size to a
  whole number of bytes), so one must make sure that the call to `byte_size/` is
  preceded by a call to [`is_binary/1`](`is_binary/1`) to ensure that bitstrings
  are rejected. Note that the compiler removes redundant calls to
  [`is_binary/1`](`is_binary/1`), so if one is not sure whether previous code
  had made sure that the argument is a binary, it does not harm to add an
  [`is_binary/1`](`is_binary/1`) test immediately before the call to
  [`byte_size/1`](`byte_size/1`).

  Own Id: OTP-18432 Aux Id:
  GH-6672,PR-6793,PR-6784,PR-6787,PR-6785,PR-6682,PR-6800,PR-6797,PR-6798,PR-6799,PR-6796,PR-6813,PR-6671,PR-6673,PR-6684,PR-6694,GH-6677,PR-6696,PR-6670,PR-6674

## Public_Key 1.13.2

### Fixed Bugs and Malfunctions

- Disregard LDAP URIs when HTTP URIs are expected.

  Own Id: OTP-18333 Aux Id: GH-6363

## Public_Key 1.13.1

### Fixed Bugs and Malfunctions

- Support more Linux distributions in cacerts_load/0.

  Own Id: OTP-18154 Aux Id: PR-6002

- Correct asn1 typenames available in type pki_asn1_type()

  Own Id: OTP-18189 Aux Id: ERIERL-829

- Sign/verify does now behave as in OTP-24 and earlier for eddsa.

  Own Id: OTP-18205 Aux Id: GH-6219

## Public_Key 1.13

### Improvements and New Features

- Added functions to retrieve OS provided CA-certs.

  Own Id: OTP-17798 Aux Id: GH-5760

- Allow key file passwords to be input as a single binary, that is we change the
  data type to be the more for the purpose logical data type iodata() instead of
  string().

  Own Id: OTP-17890

- The deprecated public_key functions ssh_decode/2, ssh_encode/2,
  ssh_hostkey_fingerprint/1 and ssh_hostkey_fingerprint/2 are removed.

  They are replaced by ssh_file:decode/2, ssh_file:encode/2,
  ssh:hostkey_fingerprint/1 and ssh:hostkey_fingerprint/2 respectively.

  Note that the decode/2 and encode/2 are not exact replacement functions, some
  minor changes may be needed. Se the manual for more information.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17921

## Public_Key 1.12.0.2

### Fixed Bugs and Malfunctions

- Country name comparison shall be case insensitive

  Own Id: OTP-18718 Aux Id: GH-7546

## Public_Key 1.12.0.1

### Fixed Bugs and Malfunctions

- Correct asn1 typenames available in type pki_asn1_type()

  Own Id: OTP-18189 Aux Id: ERIERL-829

## Public_Key 1.12

### Improvements and New Features

- Support password fun for protected keyfiles in ssl:connect function.

  Own Id: OTP-17816 Aux Id: PR-5607

## Public_Key 1.11.3

### Fixed Bugs and Malfunctions

- Avoid re-encoding of decoded certificates. This could cause unexpected
  failures as some subtle encoding errors can be tolerated when decoding but
  hence creating another sequence of bytes if the decoded value is re-encoded.

  Own Id: OTP-17657

## Public_Key 1.11.2

### Fixed Bugs and Malfunctions

- public_key:pkix_sign/2 now honors the salt length from the provided input
  parameters. Earlier this could result in incorrect signatures if not using
  recommended defaults.

  Own Id: OTP-17534 Aux Id: GH-5054, PR-5057

### Improvements and New Features

- When decoding an 'ECPrivateKey' unwrap the private key. For more precise
  information see RFC 8410, section 7.

  Own Id: OTP-17609 Aux Id: GH-5157, GH-5156

## Public_Key 1.11.1

### Fixed Bugs and Malfunctions

- Handle cross-signed root certificates when old root expired as reported in
  GH-4877.

  Own Id: OTP-17475 Aux Id: GH-4877

## Public_Key 1.11

### Improvements and New Features

- TLS connections now support EdDSA certificates.

  Own Id: OTP-17142 Aux Id: PR-4756, GH-4637, GH-4650

- The functions public_key:ssh_encode/2, public_key:ssh_decode/2,
  public_key:ssh_hostkey_fingerprint/1 and public_key:ssh_hostkey_fingerprint/2
  are deprecated.

  Replacement functions are available in SSH, see the
  [Deprecations](`e:general_info:deprecations.md#otp-24`) chapter in the
  Erlang/OTP documentation.

  Own Id: OTP-17352

- Enhance documentation and logging of certificate handling.

  Own Id: OTP-17384 Aux Id: GH-4800

## Public_Key 1.10.0.1

### Fixed Bugs and Malfunctions

- Handle cross-signed root certificates when old root expired as reported in
  GH-4877.

  Own Id: OTP-17475 Aux Id: GH-4877

## Public_Key 1.10

### Fixed Bugs and Malfunctions

- Fixed case insensitive hostname check.

  Own Id: OTP-17242 Aux Id: GH-4500

### Improvements and New Features

- Add sanity check of trusted anchor certificate expiration to
  pkix_path_validation/3. Although the anchor is considered a trusted input this
  sanity check does provide extra security for the users of the public_key
  application as this property needs to be checked at time of usage and fits
  very well with the other checks performed here.

  Own Id: OTP-16907

- Adjust generation of test certificates to conform to RFC 5280 rules for
  formatting of the certificates validity

  Own Id: OTP-17111

## Public_Key 1.9.2

### Improvements and New Features

- Corrected dialyzer spec for pkix_path_validation/3

  Own Id: OTP-17069

## Public_Key 1.9.1

### Fixed Bugs and Malfunctions

- Fix the issue that pem_decode will crash with an invalid input.

  Own Id: OTP-16902 Aux Id: ERIERL-534

## Public_Key 1.9

### Fixed Bugs and Malfunctions

- Fixed an insignificant whitespace issue when decoding PEM file.

  Own Id: OTP-16801 Aux Id: ERL-1309

### Improvements and New Features

- Experimental OCSP client support.

  Own Id: OTP-16448

- Use user returned path validation error for selfsigned cert. It allows users
  of the ssl application to customize the generated TLS alert, within the range
  of defined alerts.

  Own Id: OTP-16592

- add API function to retrieve the subject-ID of an X509 certificate

  Own Id: OTP-16705

## Public_Key 1.8

### Improvements and New Features

- Added support for RSA-PSS signature schemes

  Own Id: OTP-15247

- Calls of deprecated functions in the
  [Old Crypto API](`e:crypto:new_api.md#the-old-api`) are replaced by calls of
  their [substitutions](`e:crypto:new_api.md#the-new-api`).

  Own Id: OTP-16346

## Public_Key 1.7.2

### Improvements and New Features

- Add support for key exchange with Edward curves and PSS-RSA padding in
  signature verification.

  Own Id: OTP-16528

## Public_Key 1.7.1

### Fixed Bugs and Malfunctions

- Corrected CRL handling which could cause CRL verification to fail. This could
  happen when the CRL distribution point explicitly specifies the CRL issuer,
  that is not using the fallback.

  Own Id: OTP-16156 Aux Id: ERL-1030

## Public_Key 1.7

### Fixed Bugs and Malfunctions

- Support Password based encryption with AES

  Own Id: OTP-15870 Aux Id: ERL-952

### Improvements and New Features

- Change dialyzer spec to avoid confusion

  Own Id: OTP-15843 Aux Id: ERL-915

## Public_Key 1.6.7

### Fixed Bugs and Malfunctions

- RSA options passed to crypto for encrypt and decrypt with public or private
  key.

  Own Id: OTP-15754 Aux Id: ERL-878

- Fix dialyzer warnings caused by a faulty type specification for digest_type().

  This change updates digest_type() and the functions operating with this
  argument type to accept both 'sha1' and 'sha' as digest_type().

  Own Id: OTP-15776

### Improvements and New Features

- Add possibility to read PEM files encrypted with old PEM encryption using
  AES-256

  Own Id: OTP-13726

- Relax decoding of certificates to so that "harmless" third party encoding
  errors may be accepted but not created by the public_key application. This
  adds acceptance of using an incorrect three character country code, the PKIX
  standard use two character country codes. It is also accepted that the country
  code is utf8 encoded but the specification says it should be ASCII.

  Own Id: OTP-15687 Aux Id: PR-2162

## Public_Key 1.6.6.1

### Fixed Bugs and Malfunctions

- Support Password based encryption with AES

  Own Id: OTP-15870 Aux Id: ERL-952

## Public_Key 1.6.6

### Improvements and New Features

- Back port of bug fix ERL-893 from OTP-22 and document enhancements that will
  solve dialyzer warnings for users of the ssl application.

  This change also affects public_key, eldap (and inet doc).

  Own Id: OTP-15785 Aux Id: ERL-929, ERL-893, PR-2215

## Public_Key 1.6.5

### Improvements and New Features

- Add export of dialyzer type

  Own Id: OTP-15624

## Public_Key 1.6.4

### Improvements and New Features

- Added ed25519 and ed448 sign/verify.

  Requires OpenSSL 1.1.1 or higher as cryptolib under the OTP application
  `crypto`.

  Own Id: OTP-15419 Aux Id: OTP-15094

## Public_Key 1.6.3

### Fixed Bugs and Malfunctions

- Add DSA SHA2 oids in public_keys ASN1-spec and public_key:pkix_sign_types/1

  Own Id: OTP-15367

## Public_Key 1.6.2

### Fixed Bugs and Malfunctions

- Removed `#DSAPrivateKey{}` as acceptable input to `public_key:verify/5`.

  Own Id: OTP-15284

### Improvements and New Features

- The typing in the CRYPTO and PUBLIC_KEY applications are reworked and a few
  mistakes are corrected.

  The documentation is now generated from the typing and some clarifications are
  made.

  A new chapter on Algorithm Details such as key sizes and availability is added
  to the CRYPTO User's Guide.

  Own Id: OTP-15134

## Public_Key 1.6.1

### Fixed Bugs and Malfunctions

- Some of the keylengths in the newly generated moduli file in public_key are
  not universally supported. This could cause the SSH key exchange
  diffie-hellman-group-exchange-sha\* to fail.

  Those keylengths are now removed.

  Own Id: OTP-15151 Aux Id: OTP-15113

## Public_Key 1.6

### Fixed Bugs and Malfunctions

- Update calls to the base64 module to conform to that module's type
  specifications.

  Own Id: OTP-14788 Aux Id: OTP-14624

### Improvements and New Features

- Use uri_string module instead of http_uri.

  Own Id: OTP-14902

- A new function - `public_key:pkix_verify_hostname_match_fun/1` \- returns a
  fun to be given as option `match_fun` to `public_key:pkix_verify_hostname/3`
  or via ssl.

  The fun makes the verify hostname matching according to the specific rules for
  the protocol in the argument. Presently only `https` is supported.

  Own Id: OTP-14962 Aux Id: ERL-542, OTP-15102

- Complete PKCS-8 encoding support and enhance the decoding of 'PrivateKeyInfo'
  to conform to the rest of Erlang public_key API.

  Own Id: OTP-15093

- A new moduli file is generated. This file is used for the recommended
  `diffie-hellman-group-exchange-sha256` key exchange algorithm in SSH.

  Own Id: OTP-15113

## Public_Key 1.5.2

### Fixed Bugs and Malfunctions

- Fixed a bug in `public_key:ssh_encode/2` that made it possible to erroneously
  encode e.g. an RSA key with another type e.g. ECDSA in the resulting binary.

  Own Id: OTP-14570 Aux Id: ERIERL-52, OTP-14676

- Corrected handling of parameterized EC keys in public_key:generate_key/1 so
  that it will work as expected instead of causing a runtime error in crypto.

  Own Id: OTP-14620

## Public_Key 1.5.1

### Improvements and New Features

- Hostname verification: Add handling of the general name `iPAddress` in
  certificate's subject alternative name extension (`subjAltName`).

  Own Id: OTP-14653

- Correct key handling in pkix_test_data/1 and use a generic example mail
  address instead of an existing one.

  Own Id: OTP-14766

## Public_Key 1.5

### Fixed Bugs and Malfunctions

- public_key now handles elliptic curve parameters in a consistent way so that
  decoded ECDSA keys can be correctly re-encoded.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14621 Aux Id: ERL-480, ERL-481

### Improvements and New Features

- Extend crypto:sign, crypto:verify, public_key:sign and public_key:verify with:

  \* support for RSASSA-PS padding for signatures and for saltlength setting  
  \* X9.31 RSA padding.  
  \* sha, sha224, sha256, sha384, and sha512 for dss signatures as mentioned in
  NIST SP 800-57 Part 1.  
  \* ripemd160 to be used for RSA signatures.

  This is a manual merge of half of the pull request 838 by potatosalad from
  Sept 2015.

  Own Id: OTP-13704 Aux Id: PR838

- Add API function pkix_test_data/1 for facilitating automated testing. This is
  useful for applications that perform X509-certifcate path validation of so
  called certificate chains, such as TLS.

  Own Id: OTP-14181

- Improved error propagation and reports

  Own Id: OTP-14236

- RSAPrivateKey version is set to 'two-prime' instead of using the underlying
  enumeration value directly.

  Own Id: OTP-14534

- Deprecated function `crypto:rand_uniform/2` is replaced by `rand:uniform/1`.

  Own Id: OTP-14608

## Public_Key 1.4.1

### Fixed Bugs and Malfunctions

- Bug for `public_key:generate_key({namedCurve,OID})` fixed.

  Own Id: OTP-14258

### Improvements and New Features

- Modernized internal representation used for crl validation by use of maps.

  Own Id: OTP-14111

- Support EC key in pkix_sign/2

  Own Id: OTP-14294

## Public_Key 1.4

### Improvements and New Features

- New function `pkix_verify_hostname/2,3` Implements certificate hostname
  checking. See the manual and RFC 6125.

  Own Id: OTP-13009

- The ssh host key fingerprint generation now also takes a list of algorithms
  and returns a list of corresponding fingerprints. See
  `public_key:ssh_hostkey_fingerprint/2` and the option `silently_accept_hosts`
  in `ssh:connect`.

  Own Id: OTP-14223

## Public_Key 1.3

### Improvements and New Features

- New function `public_key:ssh_hostkey_fingerprint/1,2` to calculate the SSH
  host key fingerprint string.

  Own Id: OTP-13888 Aux Id: OTP-13887

## Public_Key 1.2

### Fixed Bugs and Malfunctions

- The ASN-1 type GeneralName can have more values, then the most common
  directory name, the code now handles this.

  Own Id: OTP-13554

### Improvements and New Features

- Handle PEM encoded EC public keys

  Own Id: OTP-13408

## Public_Key 1.1.1

### Fixed Bugs and Malfunctions

- An encapsulated PEM header shall be followed by a blank line

  Own Id: OTP-13381 Aux Id: seq13070

## Public_Key 1.1

### Improvements and New Features

- The 'ecdsa-sha2-nistp256', 'ecdsa-sha2-nistp384' and 'ecdsa-sha2-nistp521'
  signature algorithms for ssh are implemented. See RFC 5656.

  Own Id: OTP-12936

- There is now a file (public_key/priv/moduli) which lists
  size-generator-modulus triples. The purpose is to give servers the possibility
  to select the crypto primes randomly among a list of pregenerated triples.
  This reduces the risk for some attacks on diffie-hellman negotiation.

  See the reference manual for public_key:dh_gex_group/4 where the handling of
  this is described.

  The ssh server (ssh:daemon) uses this.

  Own Id: OTP-13054 Aux Id: OTP-13052

- Add different upper bounds for different string types as suggested by comment
  in PKIX1Explicit88.

  Own Id: OTP-13132

## Public_Key 1.0.1

### Improvements and New Features

- Document enhancements

  Own Id: OTP-12986

## Public_Key 1.0

### Improvements and New Features

- public_key: Remove legacy switch compact_bit_string

  E.i bitstrings will not be decode as \{Unused, Binary\}, they are now Erlang
  bitstrings.

  Also the compact_bit_string implies the legacy_erlang_types switch So removing
  the switch will also make OCTET STRING values be represented as binaries.

  Undecoded open type will now be wrapped in a asn1_OPENTYPE tuple.

  This will change some values in records returned by the public_key API making
  this change a potentiall incompatibility.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12110

## Public_Key 0.23

### Improvements and New Features

- Improve/extend support for CRL handling.

  Own Id: OTP-12547 Aux Id: OTP-10362

## Public_Key 0.22.1

### Fixed Bugs and Malfunctions

- Added missing encoding support for PBES2, and also completed support for PBES1
  that was incomplete.

  Own Id: OTP-11915

## Public_Key 0.22

### Fixed Bugs and Malfunctions

- Fix incorrect dialyzer spec and types, also enhance documentation.

  Thanks to Ayaz Tuncer.

  Own Id: OTP-11627

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

### Improvements and New Features

- Moved elliptic curve definition from the crypto NIF/OpenSSL into Erlang code,
  adds the RFC-5639 brainpool curves and makes TLS use them (RFC-7027).

  Thanks to Andreas Schultz

  Own Id: OTP-11578

- Handle v1 CRLs, with no extensions and fixes issues with IDP (Issuing
  Distribution Point) comparison during CRL validation.

  Thanks to Andrew Thompson

  Own Id: OTP-11761

## Public_Key 0.21

### Improvements and New Features

- Fixed a little typo in public_key documentation. Thanks to Tomas Morstein.

  Own Id: OTP-11380

- public_key: Workaround for incorrectly encoded utf8 emailAddress. Thanks to
  Andrew Bennett.

  Own Id: OTP-11470

## Public_Key 0.20

### Improvements and New Features

- Extend PKCS-7 to support SCEP (Simple Certificate Enrollment Protocol).

  Own Id: OTP-10874

- public_key:pem_entry_decode/2 now handles AES-128-CBC ciphered keys. Thanks to
  Simon Cornish.

  Own Id: OTP-11281

## Public_Key 0.19

### Improvements and New Features

- Add support for ISO oids 1.3.14.3.2.29 and 1.3.14.3.2.27 that are sometimes
  used instead of the PKCS defined oids 1.2.840.113549.1.1.5 and
  1.2.840.10040.4.3. Add function pkix_sign_types:/1 that translates oids to to
  algorithm atoms ex:

  > public_key:pkix_sign_types(\{1,3,14,3,2,29\}). \{sha,rsa\}

  Own Id: OTP-10873

- Integrate elliptic curve contribution from Andreas Schultz

  In order to be able to support elliptic curve cipher suites in SSL/TLS,
  additions to handle elliptic curve infrastructure has been added to public_key
  and crypto.

  This also has resulted in a rewrite of the crypto API to gain consistency and
  remove unnecessary overhead. All OTP applications using crypto has been
  updated to use the new API.

  Impact: Elliptic curve cryptography (ECC) offers equivalent security with
  smaller key sizes than other public key algorithms. Smaller key sizes result
  in savings for power, memory, bandwidth, and computational cost that make ECC
  especially attractive for constrained environments.

  Own Id: OTP-11009

## Public_Key 0.18

### Fixed Bugs and Malfunctions

- Fix subjectPublicKeyInfo type comment in public_key. Thanks to Ryosuke Nakai.

  Own Id: OTP-10670

### Improvements and New Features

- public_key now supports CRL validation and documents the function
  public_key:pkix_path_validation/3

  Own Id: OTP-7045

- Some examples overflowing the width of PDF pages have been corrected.

  Own Id: OTP-10665

- Fixed typo's in public_key spec.

  Own Id: OTP-10723

- Corrected PKCS-10 documentation and added some PKCS-9 support that is fairly
  commonly used by PKCS-10. Full support for PKCS-9 will be added later.

  Own Id: OTP-10767

## Public_Key 0.17

### Fixed Bugs and Malfunctions

- ssh_decode now handles comments, at the end of the line, containing with
  spaces correctly

  Own Id: OTP-9361

- Add missing references to sha224 and sha384

  Own Id: OTP-9362 Aux Id: seq12116

### Improvements and New Features

- public_key now supports PKCS-10 and includes experimental support for PKCS-7

  Own Id: OTP-10509 Aux Id: kunagi-291 \[202]

## Public_Key 0.16

### Improvements and New Features

- Add crypto and public_key support for the hash functions SHA224, SHA256,
  SHA384 and SHA512 and also hmac and rsa_sign/verify support using these hash
  functions. Thanks to Andreas Schultz for making a prototype.

  Own Id: OTP-9908

- Optimize RSA private key handling in `crypto` and `public_key`.

  Own Id: OTP-10065

## Public_Key 0.15

### Improvements and New Features

- Changed ssh implementation to use the public_key application for all public
  key handling. This is also a first step for enabling a callback API for
  supplying public keys and handling keys protected with password phrases.

  Additionally the test suites where improved so that they do not copy the users
  keys to test server directories as this is a security liability. Also ipv6 and
  file access issues found in the process has been fixed.

  This change also solves OTP-7677 and OTP-7235

  This changes also involves some updates to public_keys ssh-functions.

  Own Id: OTP-9911

## Public_Key 0.14

### Improvements and New Features

- public_key, ssl and crypto now supports PKCS-8

  Own Id: OTP-9312

- The asn1 decoder/encoder now uses a runtime nif from the asn1 application if
  it is available.

  Own Id: OTP-9414

## Public_Key 0.13

### Fixed Bugs and Malfunctions

- replace "a ssl" with "an ssl" reindent pkix_path_validation/3 Trivial
  documentation fixes (Thanks to Christian von Roques )

  Own Id: OTP-9464

## Public_Key 0.12

### Improvements and New Features

- The public_key application now supports encode/decode of ssh public-key files.

  Own Id: OTP-9144

## Public_Key 0.11

### Improvements and New Features

- Allows the public_key module to decode and encode RSA and DSA keys encoded
  using the SubjectPublicKeyInfo format. When pem_entry_encode is called on an
  RSA or DSA public key type, the key is wrapped in the SubjectPublicKeyInfo
  format.

  Own Id: OTP-9061

## Public_Key 0.10

### Improvements and New Features

- Improved dialyzer specs.

  Own Id: OTP-8964

## Public_Key 0.9

### Improvements and New Features

- Updated ssl to ignore CA certs that violate the asn1-spec for a certificate,
  and updated public key asn1 spec to handle inherited DSS-params.

  Own Id: OTP-7884

- Changed ssl implementation to retain backwards compatibility for old option
  \{verify, 0\} that shall be equivalent to \{verify, verify_none\}, also
  separate the cases unknown ca and selfsigned peer cert, and restored return
  value of deprecated function public_key:pem_to_der/1.

  Own Id: OTP-8858

- Better handling of v1 and v2 certificates. V1 and v2 certificates does not
  have any extensions so then validate_extensions should just accept that there
  are none and not end up in missing_basic_constraints clause.

  Own Id: OTP-8867

- Changed the verify fun so that it differentiate between the peer certificate
  and CA certificates by using valid_peer or valid as the second argument to the
  verify fun. It may not always be trivial or even possible to know when the
  peer certificate is reached otherwise.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8873

## Public_Key 0.8

### Fixed Bugs and Malfunctions

- Handling of unknown CA certificates was changed in ssl and public_key to work
  as intended.

  Own Id: OTP-8788

### Improvements and New Features

- Revise the public_key API - Cleaned up and documented the public_key API to
  make it useful for general use, also changed ssl to use the new API.

  Own Id: OTP-8722

- Added the functionality so that the verification fun will be called when a
  certificate is considered valid by the path validation to allow access to each
  certificate in the path to the user application. Also try to verify
  subject-AltName, if unable to verify it let the application verify it.

  Own Id: OTP-8825

## Public_Key 0.7

### Fixed Bugs and Malfunctions

- Certificates without any extensions could not be handled by public_key.

  Own Id: OTP-8626

### Improvements and New Features

- Code cleanup and minor bugfixes.

  Own Id: OTP-8649

## Public_Key 0.6

### Improvements and New Features

- Support for Diffie-Hellman. ssl-3.11 requires public_key-0.6.

  Own Id: OTP-7046

- Moved extended key usage test for ssl values to ssl.

  Own Id: OTP-8553 Aux Id: seq11541, OTP-8554

## Public_Key 0.5

### Improvements and New Features

- Added `public_key:pkix_transform/2` to enable ssl to send CA list during
  Certificate Request.

  `NOTE`: SSL (new_ssl) requires public_key-0.5. ssl usage.

  Own Id: OTP-8372

## Public_Key 0.4

### Improvements and New Features

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8250

## Public_Key 0.3

### Fixed Bugs and Malfunctions

- Unknown attributes in certificates are left encoded instead of crashing. Patch
  by Will "wglozer" thanks.

  Own Id: OTP-8100

### Improvements and New Features

- Allow public_key:pem_to_der/\[1,2] to take a binary as argument in addition to
  a filename. Patch by Geoff Cant, thanks.

  Own Id: OTP-8142

## Public_Key 0.2

### Improvements and New Features

- X509 certificate handling has been extended and improved as a result of more
  extensive testing of both the ssl and public_key application. Even more
  extensions of the certificate handling is yet to be implemented.

  Own Id: OTP-7860

## Public_Key 0.1

### Improvements and New Features

- First version.

  Own Id: OTP-7637
