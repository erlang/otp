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
# Public_Key Application

The Public Key application deals with public-key related file formats,
digital signatures, and [X-509
certificates](http://www.ietf.org/rfc/rfc5280.txt).  It handles
validation of certificate paths and certificate revocation lists
(CRLs) and other functions for handling of certificates, keys and
CRLs. It is a library application that does not read or write files,
it expects or returns file contents or partial file contents as
binaries. Except for the functions `public-key:cacerts_load/0`,
`public-key:cacerts_load/1`, and `public-key:cacerts_get/0`
that reads files.

## Supported PKIX functionality

- Supports [RFC 5280 ](http://www.ietf.org/rfc/rfc5280.txt)\- Internet X.509
  Public-Key Infrastructure Certificate and Certificate Revocation List (CRL)
  Profile. Certificate policies supported since OTP-26.2
- Supports [PKCS-1 ](http://www.ietf.org/rfc/rfc3447.txt)\- RSA Cryptography
  Standard
- Supports
  [DSS](http://csrc.nist.gov/publications/fips/fips186-3/fips_186-3.pdf) \-
  Digital Signature Standard (DSA - Digital Signature Algorithm)
- Supports
  [PKCS-3 ](https://web.archive.org/web/20170417091930/https://www.emc.com/emc-plus/rsa-labs/standards-initiatives/pkcs-3-diffie-hellman-key-agreement-standar.htm)\-
  Diffie-Hellman Key Agreement Standard
- Supports [PKCS-5](http://www.ietf.org/rfc/rfc2898.txt) \- Password-Based
  Cryptography Standard
- Supports [AES ](http://www.ietf.org/rfc/fc3565.txt)\- Use of the Advanced
  Encryption Standard (AES) Algorithm in Cryptographic Message Syntax (CMS)
- Supports [PKCS-8](http://www.ietf.org/rfc/rfc5208.txt) \- Private-Key
  Information Syntax Standard
- Supports [PKCS-10](http://www.ietf.org/rfc/rfc5967.txt) \- Certification
  Request Syntax Standard

## Dependencies

The `public_key` application uses the Crypto application to perform
cryptographic operations and the ASN-1 application to handle PKIX-ASN-1
specifications, hence these applications must be loaded for the `public_key`
application to work. In an embedded environment this means they must be started
with `application:start/[1,2]` before the `public_key` application is started.

## Error Logger and Event Handlers

The `public_key` application is a library application and does not use the error
logger. The functions will either succeed or fail with a runtime error.

## See Also

`m:application`
