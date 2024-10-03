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
# SSL Release Notes

This document describes the changes made to the SSL application.

## SSL 11.2.3

### Fixed Bugs and Malfunctions

- Starting from TLS-1.3 some server handshake alerts might arrive after ssl:connection/2,3,4 has returned. If the socket is in active mode the controlling process will get the alert message, but passive sockets would only get {error, closed} on next call to ssl:recv/2,3 or ssl/setopts/2. Passive sockets calls will now return  {error,  error_alert()}  instead.

  Own Id: OTP-19236 Aux Id: [PR-8261]

- Servers configured to support only version (pre TLS-1.2) should ignore hello version extension, as it is an unknown extension to them, this will result in that new clients that do not support the old server version will get an insufficient security alert from the server and not a protocol version alert, this is consistent with how old servers not able to support higher protocol versions work.

  Own Id: OTP-19257

[PR-8261]: https://github.com/erlang/otp/pull/8261

## SSL 11.2.2

### Fixed Bugs and Malfunctions

- A race in the kTLS flavour of SSL distribution has been fixed so that `inet_drv.c` doesn't read ahead too much data, which could cause the kTLS encryption to be activated too late when some encrypted data had already been read into the `inet_drv.c` buffer as unencrypted.

  Own Id: OTP-19175 Aux Id: [GH-8561], [PR-8690]

[GH-8561]: https://github.com/erlang/otp/issues/8561
[PR-8690]: https://github.com/erlang/otp/pull/8690

### Improvements and New Features

- All TLS-1.3 terminations are now graceful (previous TLS version terminations already were).

  Own Id: OTP-17848

- It is now possible to use a verification fun of arity 4, giving the user fun access to both encoded and decoded versions of the certificate. This is desirable as a workaround for encoding errors preventing re-encoding from being reliable. This also saves some work load if the encoded version is needed.
  
  Note that calling `public_key:pkix_path_validation/3` with only decoded certs is not recommended, due to the decoding workarounds, although it will work as long as the workarounds are not needed.
  
  If the decoded version is needed before thecall to `m:public_key` it is recommend to use the combined_cert- type to avoid double decoding.  Note that the path validation algorithm itself always needs both the encoded and decoded versions of the certs.
  
  The ssl implementation will now benefit from using this function instead of emulating the verify_fun/4.

  Own Id: OTP-19169

- Compiler warnings for some removed functions have been corrected to point out the correct replacement functions.

  Own Id: OTP-19186 Aux Id: [PR-8709]

- Include more information in logging of SNI (Server Name Indication) mismatch error.

  Own Id: OTP-19187

[PR-8709]: https://github.com/erlang/otp/pull/8709

## SSL 11.2.1

### Fixed Bugs and Malfunctions

- Check  for TLS-1.3 support should check minimum requirements.

  Own Id: OTP-19094 Aux Id: [GH-8489]

- If both TLS-1.3 and TLS-1.2 is supported
  and TLS-1.2 negotiated convert TLS-1.3 ECDSA schemes to TLS-1.2 hash and signature pairs for increased interoperability.

  Own Id: OTP-19107 Aux Id: [GH-8376]

- TLS-1.3 negotiation now uses SNI based options correctly instead of ignoring them.

  Own Id: OTP-19140

[GH-8489]: https://github.com/erlang/otp/issues/8489
[GH-8376]: https://github.com/erlang/otp/issues/8376

### Improvements and New Features

- Make it easier to distinguish between a invalid signature and unsupported signature.

  Own Id: OTP-19091

- Enhance ALERT logs to help understand what causes the alert.

  Own Id: OTP-19092 Aux Id: [GH-8482]

- When the default value for signature_algs is used, default the signature_algs_cert to the default value + rsa_pkcs1_sha1 to allow this algorithms for certificates but not for the TLS protocol. This is for better interoperability.  If signature_algs is set explicitly signature_algs_cert must also be set explicitly if they should be different.

  Own Id: OTP-19152 Aux Id: [GH-8588]

[GH-8482]: https://github.com/erlang/otp/issues/8482
[GH-8588]: https://github.com/erlang/otp/issues/8588

## SSL 11.2

### Fixed Bugs and Malfunctions

- Starting a TLS server without sufficient credentials (certificate or anonymous cipher) would work, but it was impossible to connect to it.
  
  This has been corrected to return an error instead of starting the server.

  Own Id: OTP-18887 Aux Id: [GH-7493], [PR-7918]

- ASN.1 decoding errors are handled in more places to ensure that errors are returned instead of cause a crash.

  Own Id: OTP-18969 Aux Id: [GH-8058], [PR-8256]

- Improved error checking on the API functions.

  Own Id: OTP-18992 Aux Id: [GH-8066] [PR-8156]

[GH-7493]: https://github.com/erlang/otp/issues/7493
[PR-7918]: https://github.com/erlang/otp/pull/7918
[GH-8058]: https://github.com/erlang/otp/issues/8058
[PR-8256]: https://github.com/erlang/otp/pull/8256
[GH-8066]: https://github.com/erlang/otp/issues/8066
[PR-8156]: https://github.com/erlang/otp/pull/8156

### Improvements and New Features

- The `ssl` client can negotiate and handle certificate status request (OCSP stapling support on the client side).
  
  Thanks to voltone for interop testing and related discussions.

  Own Id: OTP-18606 Aux Id: OTP-16875,OTP-16448

- Memory consumption has been reduced and performance increased by refactoring internal data structures and their usage.

  Own Id: OTP-18665 Aux Id: [PR-7447]

- Added `c:ssl_crl_cache_api:lookup/2` as an optional `-callback` attribute.

  Own Id: OTP-18788 Aux Id: [PR-7700]

- Key customization support has been extended to allow flexibility for implementers of  for instance hardware security modules (HSM) or trusted platform modules (TPM).

  Own Id: OTP-18876 Aux Id: [PR-7898], [PR-7475]

- The `proc_lib:set_label/1` function is now used to increase observability of `ssl` processes.

  Own Id: OTP-18879

- Brainpool elliptic curves are now supported in TLS-1.3.

  Own Id: OTP-18884 Aux Id: [PR-8056]

- The documentation has been migrated to use Markdown and ExDoc.

  Own Id: OTP-18955 Aux Id: [PR-8026]

- For security reasons, the CBC ciphers are now longer included in the list of default ciphers for TLS-1.2.

  *** POTENTIAL INCOMPATIBILITY ***

  Own Id: OTP-19025 Aux Id: [PR-8250]

- There is a new `cert_policy_opts` option to configure certificate policy options for the certificate path validation.

  Own Id: OTP-19027 Aux Id: [PR-8255]

[PR-7447]: https://github.com/erlang/otp/pull/7447
[PR-7700]: https://github.com/erlang/otp/pull/7700
[PR-7898]: https://github.com/erlang/otp/pull/7898
[PR-7475]: https://github.com/erlang/otp/pull/7475
[PR-8056]: https://github.com/erlang/otp/pull/8056
[PR-8026]: https://github.com/erlang/otp/pull/8026
[PR-8250]: https://github.com/erlang/otp/pull/8250
[PR-8255]: https://github.com/erlang/otp/pull/8255

## SSL 11.1.4.3

### Fixed Bugs and Malfunctions

* A race in the kTLS flavour of SSL distribution has been fixed so inet_drv.c doesn't read ahead too much data which could cause the kTLS encryption to be activated too late when some encrypted data had already been read into the inet_drv.c buffer as unencrypted.

  Own Id: OTP-19175 Aux Id: GH-8561, PR-8690

### Improvements and New Features

* Make sure all TLS-1.3 terminations are graceful (previous TLS version terminations already are).

  Own Id: OTP-17848
* Include more information in logging of SNI (Server Name Indication) mismatch error.

  Own Id: OTP-19187

## SSL 11.1.4.2

### Improvements and New Features

* When the default value for signature_algs is used, default the signature_algs_cert to the default value + rsa_pkcs1_sha1 to allow this algorithms for certificates but not for the TLS protocol. This is for better interoperability. If signature_algs is set explicitly signature_algs_cert must also be set explicitly if they should be different.

  Own Id: OTP-19152 Aux Id: GH-8588

## SSL 11.1.4.1

### Fixed Bugs and Malfunctions

* Check for TLS-1.3 support should check minimum requirements.

  Own Id: OTP-19094 Aux Id: GH-8489
* If both TLS-1.3 and TLS-1.2 is supported and TLS-1.2 negotiated convert TLS-1.3 ECDSA schemes to TLS-1.2 hash and signature pairs for increased interoperability.

  Own Id: OTP-19107 Aux Id: GH-8376
* TLS-1.3 negotiation now uses SNI based options correctly instead of ignoring them.

  Own Id: OTP-19140

### Improvements and New Features

* Make it easier to distinguish between a invalid signature and unsupported signature.

  Own Id: OTP-19091
* Enhance ALERT logs to help understand what causes the alert.

  Own Id: OTP-19092 Aux Id: GH-8482

## SSL 11.1.4

### Fixed Bugs and Malfunctions

* Fix certificate authorities check so that CA closest to peer is not lost. It could manifest itself in a failed connection as the client failed to realize it had a valid certificate chain to send to the server.

  Own Id: OTP-19065 Aux Id: GH-8356, PR-8367
* ssl:signature_algs/2 did not list some legacy algorithm schemes correctly when listing all algorithms available.

  Own Id: OTP-19067 Aux Id: PR-8379

## SSL 11.1.3

### Fixed Bugs and Malfunctions

* Cleanup and close all connections in DTLS when the listen socket owner dies.

  Improved IPv6 handling in DTLS.

  Own Id: OTP-19037 Aux Id: GH-7951 GH-7955
* Fixed a crash in dtls accept.

  Own Id: OTP-19059 Aux Id: GH-8338

## SSL 11.1.2

### Fixed Bugs and Malfunctions

* ssl:prf/5, will start working instead of hanging in a TLS-1.3 context if called appropriately. Note that the implementation has changed and in OTP-27 a more adequate API will be documented.

  Own Id: OTP-18890 Aux Id: GH-7911
* Server name verification didn't work if a connection was made with IP-address as a string.

  Own Id: OTP-18909 Aux Id: GH-7968
* The fallback after "dh" ssl option was undefined was to get "dh" from ssl options again. This is clearly wrong and now changed to the documented fallback "dhfile" ssl option.

  Own Id: OTP-18919 Aux Id: PR-7984
* Correct default value selection for DTLS. Will only affect users linked with really old version of cryptolib library.

  Own Id: OTP-18962 Aux Id: GH-8079
* Adhere elliptic curves with RFC 8422 pre TLS-1.3, that is Edwards curves are added to curves that can be used for key exchange, and documentation and implementation of eccs/0,1 are aligned.

  Own Id: OTP-18991

### Improvements and New Features

* Improve alert reason when ecdhe_rsa key_exchange does not have any common curves to use

  Own Id: OTP-18985

## SSL 11.1.1

### Fixed Bugs and Malfunctions

* Legacy name handling could cause interop problems between TLS-1.3/1.2 client and TLS-1.2 server.

  Own Id: OTP-18917 Aux Id: GH-7978

## SSL 11.1

### Fixed Bugs and Malfunctions

- ssl application will validate id-kp-serverAuth and id-kp-clientAuth extended
  key usage only in end entity certificates. public_key application will
  disallow "anyExtendedKeyUsage" for CA certificates that includes the extended
  key usage extension and marks it critical.

  Own Id: OTP-18739

- Replaced unintentional Erlang Public License 1.1 headers in some files with
  the intended Apache License 2.0 header.

  Own Id: OTP-18815 Aux Id: PR-7780

- Correct handling of TLS-1.3 legacy scheme names, could cause interop failures
  for TLS-1.2 clients.

  Own Id: OTP-18817

- Add missing export for connection_info() API type.

  Own Id: OTP-18886

### Improvements and New Features

- Fixed `server name indication` which was not handled properly.

  Own Id: OTP-18836 Aux Id: GH-7795

- Align documentation and implementation

  Own Id: OTP-18853 Aux Id: PR-7841

- Improve connection setup by optimizing certificate lookup.

  Own Id: OTP-18893 Aux Id: PR-7920 PR-7921

## SSL 11.0.3

### Fixed Bugs and Malfunctions

- Avoid function clause error in ssl:getopts/2 by handling that inet:getopts may
  return an empty list during some circumstances, such as the socket being in a
  closing state.

  Own Id: OTP-18697 Aux Id: GH-7506

- The API function \`ssl:recv/3\` has been tightened to disallow negative
  length, which has never been documented to work, but was passed through and
  caused strange errors.

  Own Id: OTP-18700 Aux Id: GH-7507

- When a client initiated renegotiation was rejected and the client socket was
  in active mode the expected error message to the controlling process was not
  sent.

  Own Id: OTP-18712 Aux Id: GH-7431

### Improvements and New Features

- Add some guidance for signature algorithms configuration in ssl applications
  users guide.

  Own Id: OTP-18631

## SSL 11.0.2

### Fixed Bugs and Malfunctions

- Added keylog information to all protocol versions in
  `ssl:connection_information/2`.

  Own Id: OTP-18643 Aux Id: ERIERL-932

### Improvements and New Features

- Add RFC-6083 considerations for DTLS to enable gen_sctp based callback for the
  transport.

  Own Id: OTP-18618 Aux Id: ERIERL-932

## SSL 11.0.1

### Fixed Bugs and Malfunctions

- Make sure that selection of client certificates handle both TLS-1.3 and
  TLS-1.2 names correctly. Could cause valid client certificate to not be
  selected, and an empty client certificate message to be sent to server.

  Own Id: OTP-18588 Aux Id: GH-7264, PR-7277

- Improved `ssl:format_error/1` to handle more error tuples.

  Own Id: OTP-18596 Aux Id: GH-7247

- Fixed hanging `ssl:connect` when ssl application is not started.

  Own Id: OTP-18603 Aux Id: GH-7297

- Correct handling of retransmission timers, current behavior could cause
  unwanted delays.

  Own Id: OTP-18632 Aux Id: PR-7300, GH-7301

## SSL 11.0

### Improvements and New Features

- Remove less that 256 bit ECC from default supported ECC pre TLS-1.3

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14771

- Improved error checking and handling of ssl options.

  Own Id: OTP-15903

- With this change, stateless tickets generated by server with anti_replay
  option enabled can be used for creating ClientHello throughout ticket
  lifetime. Without this change, usability was limited to WindowSize number of
  seconds configured for anti_replay option.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18168 Aux Id: PR-6019, GH-6014

- Support for Kernel TLS (kTLS), has been added to the SSL application, for TLS
  distribution (`-proto_dist inet_tls`), the SSL option `{ktls, true}`. Using
  this for general SSL sockets is uncomfortable, undocumented and not
  recommended since it requires very platform dependent raw options.

  This, for now, only works for some not too old Linux distributions. Roughly, a
  kernel 5.2.0 or later with support for UserLand Protocols and the kernel
  module `tls` is required.

  Own Id: OTP-18235 Aux Id: PR-6104, PR-5840

- With this change, TLS 1.3 server can be configured to include client
  certificate in session ticket.

  Own Id: OTP-18253

- With this change, it is possible to configure encryption seed to be used with
  TLS1.3 stateless tickets. This enables using tickets on different server
  instances.

  Own Id: OTP-18254 Aux Id: PR-5982

- Debugging enhancements.

  Own Id: OTP-18312

- With this change, maybe keyword atom is not used as function name in ssl code.

  Own Id: OTP-18335

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

  Own Id: OTP-18405 Aux Id:
  GH-6672,PR-6702,PR-6768,PR-6700,PR-6769,PR-6812,PR-6814

- For security reasons remove support for SHA1 and DSA algorithms from default
  values.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18438 Aux Id: GH-6679

- Mitigate memory usage from large certificate chains by lowering the maximum
  handshake size. This should not effect the common cases, if needed it can be
  configured to a higher value.

  Own Id: OTP-18453

- Change the client default verify option to verify_peer. Note that this makes
  it mandatory to also supply trusted CA certificates or explicitly set verify
  to verify_none. This also applies when using the so called anonymous test
  cipher suites defined in TLS versions pre TLS-1.3.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18455 Aux Id: GH-5899

- Erlang distribution code in Kernel and SSL has been refactored a bit to
  facilitate debugging and re-usability, which shouldn't have any noticeable
  effects on behaviour or performance.

  Own Id: OTP-18456

- Add encoding and decoding of use_srtp hello extension to facilitate for DTLS
  users to implement SRTP functionality.

  Own Id: OTP-18459

- Refactors the (`ssl` application to use macros for TLS and DTLS versions
  instead of hard-coded tuple numbers. This change improves the maintainability
  of `ssl`

  Own Id: OTP-18465 Aux Id: GH-7065

- If the function ssl:renegotiate/1 is called on connection that is running
  TLS-1.3 return an error instead of hanging or timing out.

  Own Id: OTP-18507

- If a user cancel alert with level warning is received during handshake make it
  be handled the same regardless of TLS version. If it is received in connection
  in TLS-1.3 regard it as an error as it is inappropriate.

  In TLS-1.3 all error alerts are considered FATAL regardless of legacy alert
  type. But make sure legacy type is printed in logs to not confuse users that
  are expecting the same legacy type as sent by peer.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18531

- Make `fail_if_no_peer_cert` default true if verify_peer is set on the server,
  otherwise the server will accept the connection if verify_peer is set and the
  user have forgot to set the fail_if_no_peer_cert and the client did not send a
  certificate.

  Own Id: OTP-18567

- To make it easier to configure signature algorithms with algorithms that are
  moved from the default add the API function signature_algs/2 that lists
  possible values. Also make sha224 a non default value.

  Own Id: OTP-18572

## SSL 10.9.1.5

### Fixed Bugs and Malfunctions

* TLS-1.3 negotiation now uses SNI based options correctly instead of ignoring them.

  Own Id: OTP-19140

## SSL 10.9.1.4

### Fixed Bugs and Malfunctions

* Fix certificate authorities check so that CA closest to peer is not lost. It could manifest itself in a failed connection as the client failed to realize it had a valid certificate chain to send to the server.

  Own Id: OTP-19065 Aux Id: GH-8356, PR-8367

## SSL 10.9.1.3

### Fixed Bugs and Malfunctions

- ssl application will validate id-kp-serverAuth and id-kp-clientAuth extended
  key usage only in end entity certificates. public_key application will
  disallow "anyExtendedKeyUsage" for CA certificates that includes the extended
  key usage extension and marks it critical.

  Own Id: OTP-18739

- Add missing export for connection_info() API type.

  Own Id: OTP-18886

## SSL 10.9.1.2

### Fixed Bugs and Malfunctions

- The API function \`ssl:recv/3\` has been tightened to disallow negative
  length, which has never been documented to work, but was passed through and
  caused strange errors.

  Own Id: OTP-18700 Aux Id: GH-7507

- When a client initiated renegotiation was rejected and the client socket was
  in active mode the expected error message to the controlling process was not
  sent.

  Own Id: OTP-18712 Aux Id: GH-7431

## SSL 10.9.1.1

### Fixed Bugs and Malfunctions

- Added keylog information to all protocol versions in
  `ssl:connection_information/2`.

  Own Id: OTP-18643 Aux Id: ERIERL-932

### Improvements and New Features

- Add RFC-6083 considerations for DTLS to enable gen_sctp based callback for the
  transport.

  Own Id: OTP-18618 Aux Id: ERIERL-932

## SSL 10.9.1

### Fixed Bugs and Malfunctions

- With this change, ssl:connection_information/2 returns correct keylog data
  after TLS1.3 key update.

  Own Id: OTP-18489

- Client signature algorithm list input order is now honored again , it was
  accidently reversed by a previous fix.

  Own Id: OTP-18550

## SSL 10.9

### Fixed Bugs and Malfunctions

- Fixed that new `dtls` connections from the same client ip port combination
  works. If there is a process waiting for accept the new connection will
  connect to that, otherwise it will try to re-connect to the old server
  connection.

  Own Id: OTP-18371 Aux Id: GH-6160

- When shutting down a node that uses SSL distribution (`-proto_dist inet_tls`),
  a confusing error message about an unexpected process exit was printed. This
  particular message is no longer generated.

  Own Id: OTP-18443 Aux Id: PR-6810

### Improvements and New Features

- fixes the type spec for ssl:format_error/1

  Own Id: OTP-18366 Aux Id: PR-6565, GH-6506

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

## SSL 10.8.7

### Improvements and New Features

- Maximize compatibility by ignoring change_cipher_spec during handshake even if
  middle_box_mode is not negotiated (mandated by client)

  Own Id: OTP-18433 Aux Id: GH-6772

- Move assert of middlebox message after an hello_retry_request to maximize
  interoperability. Does not changes semantics of the protocol only allows
  unexpected message delay from server.

  Own Id: OTP-18467 Aux Id: GH-6807

## SSL 10.8.6

### Fixed Bugs and Malfunctions

- With this change, tls_sender process is hibernated after sufficient
  inactivity.

  Own Id: OTP-18314 Aux Id: GH-6373

- Correct handling of legacy schemes so that ECDSA certs using sha1 may be used
  for some TLS-1.3 configurations.

  Own Id: OTP-18332 Aux Id: GH-6435, PR-6435, ERL-6435

- With this change, tls_sender does not cause logger crash upon key update.

  Own Id: OTP-18349

### Improvements and New Features

- Enhance warning message

  Own Id: OTP-18257 Aux Id: GH-6307

- Provide server option to make certificate_authorities extension in the TLS-1.3
  servers certificate request optional. This will allow clients to send
  incomplete chains that may be reconstructable and thereby verifiable by the
  server, but that would not adhere to the certificate_authorities extension.

  Own Id: OTP-18267 Aux Id: PR-6228, GH-6106

- If the `verify_fun` handles four arguments the DER cert will be supplied as
  one of the arguments.

  Own Id: OTP-18302 Aux Id: ERIERL-867

## SSL 10.8.5

### Fixed Bugs and Malfunctions

- Fixes handling of symlinks in cacertfile option.

  Own Id: OTP-18266 Aux Id: GH-6328

## SSL 10.8.4

### Fixed Bugs and Malfunctions

- Reject unexpected application data in all relevant places for all TLS
  versions. Also, handle TLS-1.3 middlebox compatibility with more care. This
  will make malicious connections fail early and further, mitigate possible DoS
  attacks, that would be caught by the handshake timeout.

  Thanks to Aina Toky Rasoamanana and Olivier Levillain from Télécom SudParis
  for alerting us of the issues in our implementation.

  Own Id: OTP-18044

- With this change, value of cacertfile option will be adjusted before loading
  certs from the file. Adjustments include converting relative paths to absolute
  and converting symlinks to actual file path.

  Thanks to Marcus Johansson

  Own Id: OTP-18099 Aux Id: PR-6287

- In TLS-1.3, if chain certs are missing (so server auth domain adherence can
  not be determined) send peer cert and hope the server is able to recreate a
  chain in its auth domain.

  Own Id: OTP-18191 Aux Id: GH-6105

- Make sure periodical refresh of CA certificate files repopulates cache
  properly.

  Own Id: OTP-18195

- Correct internal CRL cache functions to use internal format consistently.

  Own Id: OTP-18203 Aux Id: PR-5996

- Incorrect handling of client middlebox negotiation for TLS-1.3 could result in
  that a TLS-1.3 server would not use middlebox mode although the client was
  expecting it too and failing the negotiation with unexpected message.

  Own Id: OTP-18219 Aux Id: GH-6241, PR-6249

- If the "User" process, the process starting the TLS connection, gets killed in
  the middle of spawning the dynamic connection tree make sure we do not leave
  any processes behind.

  Own Id: OTP-18233 Aux Id: GH-6244, PR-6270

### Improvements and New Features

- A vulnerability has been discovered and corrected. It is registered as
  CVE-2022-37026 "Client Authentication Bypass". Corrections have been released
  on the supported tracks with patches 23.3.4.15, 24.3.4.2, and 25.0.2. The
  vulnerability might also exist in older OTP versions. We recommend that
  impacted users upgrade to one of these versions or later on the respective
  tracks. OTP 25.1 would be an even better choice. Impacted are those who are
  running an ssl/tls/dtls server using the ssl application either directly or
  indirectly via other applications. For example via inets (httpd), cowboy, etc.
  Note that the vulnerability only affects servers that request client
  certification, that is sets the option \{verify, verify_peer\}.

  Own Id: OTP-18241

## SSL 10.8.3

### Fixed Bugs and Malfunctions

- The link to crypto:engine_load refered the function with wrong arity.

  Own Id: OTP-18173

## SSL 10.8.2

### Fixed Bugs and Malfunctions

- Improved handling of unexpected messages during the handshake, taking the
  right action for unexpected messages.

  Own Id: OTP-18145

## SSL 10.8.1

### Fixed Bugs and Malfunctions

- When a TLS-1.3 enabled client tried to talk to a TLS-1.2 server that coalesces
  TLS-1.2 handshake message over one TLS record, the connection could fail due
  to some message being handled in the wrong state, this has been fixed.

  Own Id: OTP-18087 Aux Id: GH-5961

- Correctly handles supported protocol version change from default to something
  else by sni_fun supplied to ssl:handshake/\[2,3] together with a TCP-socket
  (so called upgrade).

  Own Id: OTP-18100 Aux Id: GH-5985

- Also, TLS-1.3 should respond with a protocol version alert if previous
  versions, that are supported but not configured, are attempted.

  Own Id: OTP-18129 Aux Id: GH-5950

## SSL 10.8

### Fixed Bugs and Malfunctions

- When a TLS-1.3 enabled client tried to talk to a TLS-1.2 server that coalesces
  TLS-1.2 handshake message over one TLS record, the connection could fail due
  to some message being handled in the wrong state, this has been fixed.

  Own Id: OTP-18087 Aux Id: GH-5961

- Fixed tls-1.3 session ticket lifetime which was discarded to quickly before.

  Own Id: OTP-18092 Aux Id: PR-5959

### Improvements and New Features

- With this change, it is possible to provide several certificates. Most
  appropriate will be selected based on negotiated properties.

  Own Id: OTP-15993 Aux Id: GH-4143

- Add options for users to be able to set spawn_opts for TLS processes (sender
  and receiver) this may be useful for tuning trade-offs between CPU and Memory
  usage.

  Own Id: OTP-17855 Aux Id: PR-5328

- Allow key file passwords to be input as a single binary, that is we change the
  data type to be the more for the purpose logical data type iodata() instead of
  string().

  Own Id: OTP-17890

- Logging enhancement, add location information to the warning log message.

  Own Id: OTP-18000 Aux Id: PR-5790

- Now also accepts the signature_algs_cert option in TLS-1.2 configuration.

  Own Id: OTP-18014

- Handle certificate selection correctly for server fallback and certificate
  authorities considerations.

  Own Id: OTP-18045 Aux Id: ERIERL-792, OTP-15993

- Enhance handling of handshake decoding errors, especially for certificate
  authorities extension to ensure graceful termination.

  Own Id: OTP-18085

## SSL 10.7.3.9

### Fixed Bugs and Malfunctions

- When a client initiated renegotiation was rejected and the client socket was
  in active mode the expected error message to the controlling process was not
  sent.

  Own Id: OTP-18712 Aux Id: GH-7431

## SSL 10.7.3.8

### Fixed Bugs and Malfunctions

- Added keylog information to all protocol versions in
  `ssl:connection_information/2`.

  Own Id: OTP-18643 Aux Id: ERIERL-932

### Improvements and New Features

- Add RFC-6083 considerations for DTLS to enable gen_sctp based callback for the
  transport.

  Own Id: OTP-18618 Aux Id: ERIERL-932

## SSL 10.7.3.7

### Fixed Bugs and Malfunctions

- Client signature algorithm list input order is now honored again , it was
  accidently reversed by a previous fix.

  Own Id: OTP-18550

## SSL 10.7.3.6

### Improvements and New Features

- Maximize compatibility by ignoring change_cipher_spec during handshake even if
  middle_box_mode is not negotiated (mandated by client)

  Own Id: OTP-18433 Aux Id: GH-6772

- Move assert of middlebox message after an hello_retry_request to maximize
  interoperability. Does not changes semantics of the protocol only allows
  unexpected message delay from server.

  Own Id: OTP-18467 Aux Id: GH-6807

## SSL 10.7.3.5

### Fixed Bugs and Malfunctions

- Fixes handling of symlinks in cacertfile option.

  Own Id: OTP-18266 Aux Id: GH-6328

## SSL 10.7.3.4

### Fixed Bugs and Malfunctions

- With this change, value of cacertfile option will be adjusted before loading
  certs from the file. Adjustments include converting relative paths to absolute
  and converting symlinks to actual file path.

  Thanks to Marcus Johansson

  Own Id: OTP-18099 Aux Id: PR-6287

- Incorrect handling of client middlebox negotiation for TLS-1.3 could result in
  that a TLS-1.3 server would not use middlebox mode although the client was
  expecting it too and failing the negotiation with unexpected message.

  Own Id: OTP-18219 Aux Id: GH-6241, PR-6249

- If the "User" process, the process starting the TLS connection, gets killed in
  the middle of spawning the dynamic connection tree make sure we do not leave
  any processes behind.

  Own Id: OTP-18233 Aux Id: GH-6244, PR-6270

## SSL 10.7.3.3

### Fixed Bugs and Malfunctions

- Reject unexpected application data in all relevant places for all TLS
  versions. Also, handle TLS-1.3 middlebox compatibility with more care. This
  will make malicious connections fail early and further, mitigate possible DoS
  attacks, that would be caught by the handshake timeout.

  Thanks to Aina Toky Rasoamanana and Olivier Levillain from Télécom SudParis
  for alerting us of the issues in our implementation.

  Own Id: OTP-18044

- The link to crypto:engine_load refered the function with wrong arity.

  Own Id: OTP-18173

- Make sure periodical refresh of CA certificate files repopulates cache
  properly.

  Own Id: OTP-18195

## SSL 10.7.3.2

### Fixed Bugs and Malfunctions

- Improved handling of unexpected messages during the handshake, taking the
  right action for unexpected messages.

  Own Id: OTP-18145

## SSL 10.7.3.1

### Fixed Bugs and Malfunctions

- When a TLS-1.3 enabled client tried to talk to a TLS-1.2 server that coalesces
  TLS-1.2 handshake message over one TLS record, the connection could fail due
  to some message being handled in the wrong state, this has been fixed.

  Own Id: OTP-18087 Aux Id: GH-5961

- Fixed tls-1.3 session ticket lifetime which was discarded to quickly before.

  Own Id: OTP-18092 Aux Id: PR-5959

- Correctly handles supported protocol version change from default to something
  else by sni_fun supplied to ssl:handshake/\[2,3] together with a TCP-socket
  (so called upgrade).

  Own Id: OTP-18100 Aux Id: GH-5985

- Also, TLS-1.3 should respond with a protocol version alert if previous
  versions, that are supported but not configured, are attempted.

  Own Id: OTP-18129 Aux Id: GH-5950

### Improvements and New Features

- Enhance handling of handshake decoding errors, especially for certificate
  authorities extension to ensure graceful termination.

  Own Id: OTP-18085

## SSL 10.7.3

### Fixed Bugs and Malfunctions

- Client certification could fail if TLS-1.3 enabled client negotiated TLS-1.2
  connection with the server, this is due to the wrong version being used when
  decoding the certificate request message from the server.

  Own Id: OTP-18028 Aux Id: GH-5835

- socket option packet_size was not handled in ssl:setops/2 and ssl:getotps/2

  Own Id: OTP-18062 Aux Id: GH-5898

- Remove legacy code to fix interoperability with new socket inet_backend.

  Own Id: OTP-18071 Aux Id: GH-5930

## SSL 10.7.2

### Fixed Bugs and Malfunctions

- With this change, potential hanging of pre TLS1.3 client receiving OSCP staple
  message is avoided.

  Own Id: OTP-17994

## SSL 10.7.1

### Fixed Bugs and Malfunctions

- Client certification could fail for TLS-1.3 servers that did not include the
  certificate_authorities extension in its certificate request message.

  Own Id: OTP-17971 Aux Id: GH-5783

## SSL 10.7

### Fixed Bugs and Malfunctions

- Improved error handling.

  Own Id: OTP-17759 Aux Id: GH-5367

- Before this change, net_kernel used with TLS distribution might be leaking
  processes in case of connectivity issues.

  Own Id: OTP-17815 Aux Id: GH-5332

- Fix makefile dependency bugs.

  Own Id: OTP-17847 Aux Id: PR-5574 GH-5548

- Make sure the TLS sender process handles explicit calls to
  erlang:disconnect_node properly, avoiding potential hanging problems in
  net_kernel.

  Own Id: OTP-17929 Aux Id: GH-5708

### Improvements and New Features

- Add support for TLS-1.3 certificate_authorities extension. And process
  certificate_authorities field in pre-TLS-1.3 certificate requests.

  Own Id: OTP-15719

- Support password fun for protected keyfiles in ssl:connect function.

  Own Id: OTP-17816 Aux Id: PR-5607

- Add in some cases earlier detection of possible DoS attacks by malicious
  clients sending unexpected TLS messages instead of the client hello. Note that
  such attacks are already mitigated by providing a timeout for the TLS
  handshake.

  Own Id: OTP-17903

## SSL 10.6.1

### Fixed Bugs and Malfunctions

- Improve SNI (server name indication) handling so that protocol version can be
  selected with regards to SNI. Also, make sure that
  ssl:connection_information/1 returns the correct SNI value.

  Own Id: OTP-17794 Aux Id: GH-5341, GH-4450

- Fixed cipher suite listing functions so that the listing of all cipher suites
  will be complete. Another fix for cipher suite handling in OTP-24.1
  accidentally excludes a few cipher suites from the listing of all cipher
  suites.

  Own Id: OTP-17829 Aux Id: ERIERL-708

- Reenable legacy cipher suite TLS_RSA_WITH_3DES_EDE_CBC_SHA for explicit
  configuration in TLS-1.2, not supported by default.

  Own Id: OTP-17879 Aux Id: GH-5624

### Improvements and New Features

- Avoid unnecessary logs by better adjusting the tls_sender process to the new
  supervisor structure in OTP-24.2

  Own Id: OTP-17831

## SSL 10.6

### Fixed Bugs and Malfunctions

- Allow re-connect on DTLS sockets

  Can happen when a computer reboots and connects from the same client port
  without the server noticing should be allowed according to RFC.

  Own Id: OTP-17411 Aux Id: ERL-1203, GH-4393

- Fix tls and non-tls distribution to use erl_epmd:address_please to figure out
  if IPv4 or IPv6 addresses should be used when connecting to the remote node.

  Before this fix, a dns lookup of the remote node hostname determined which IP
  version was to be used which meant that the hostname had to resolve to a valid
  ip address.

  Own Id: OTP-17809 Aux Id: PR-5337 GH-5334

### Improvements and New Features

- Use supervisor significant child to manage tls connection process and tls
  sender process dependency.

  Own Id: OTP-17417

- Random generation adjustment for TLS1.3

  Own Id: OTP-17699

- Allow any \{03,XX\} TLS record version in the client hello for maximum
  interoperability

  Own Id: OTP-17761 Aux Id: GH-5380

## SSL 10.5.3

### Fixed Bugs and Malfunctions

- Correct typo of ECC curve name in signature algorithm handling. Will make the
  signature algorithm ecdsa_secp521r1_sha512 succeed.

  Own Id: OTP-17756 Aux Id: GH-5383, PR-5397

- Suppress authenticity warning when option verify_none is explicitly supplied.

  Own Id: OTP-17757 Aux Id: GH-5352, PR-5395

## SSL 10.5.2

### Fixed Bugs and Malfunctions

- Fix TLS-1.2 RSA-PSS negotiation and also fix broken certificate request
  message for pre-TLS-1.3 servers.

  Own Id: OTP-17688 Aux Id: GH-5255

- Fix CRL issuer verification that under some circumstances could fail with a
  function_clause error.

  Own Id: OTP-17723 Aux Id: GH-5300

## SSL 10.5.1

### Fixed Bugs and Malfunctions

- Before that change, TLS downgrade could occasionally fail when data intended
  for downgraded socket were delivered together with CLOSE_NOTIFY alert to ssl
  app.

  Own Id: OTP-17393

- Avoid re-encoding of decoded certificates. This could cause unexpected
  failures as some subtle encoding errors can be tolerated when decoding but
  hence creating another sequence of bytes if the decoded value is re-encoded.

  Own Id: OTP-17657

- Fix possible process leak when the process doing ssl:transport_accept dies
  before initiating the TLS handshake.

  Own Id: OTP-17666 Aux Id: GH-5239

- Fix dtls memory leak, the replay window code was broken.

  Own Id: OTP-17670 Aux Id: GH-5224

## SSL 10.5

### Fixed Bugs and Malfunctions

- Fix Makefile dependency generation to work no matter what the `ERL_TOP` folder
  is called.

  Own Id: OTP-17423 Aux Id: GH-4823 PR-4829

- If trying to downgrade a TLS-1.3 connection to a plain TCP connection,
  possible TLS-1.3 session ticket messages will be ignored in the "downgrade"
  state while waiting for the close notify alert.

  Own Id: OTP-17517 Aux Id: GH-5009

- Corrected error handling to correctly generate an insufficient security alert
  when there are no suitable groups that can be negotiated in TLS-1.3 instead of
  crashing resulting in an internal error alert.

  Own Id: OTP-17521

- Properly handle default session data storage.

  When a client tries to reuse an expired session the default server storage
  handling would crash losing other session data. This would cause a error
  report and possible loss of abbreviated handshakes.

  Own Id: OTP-17635 Aux Id: GH-5192

### Improvements and New Features

- Add support for RSA-PSS-PSS signatures and signature_algorithms_cert in
  TLS-1.2. This is a TLS-1.3 RFC requirement to backport this functionality.

  Own Id: OTP-16590 Aux Id: ERL-625, GH-5029

- Use inet:monitor/1 to monitor listen-sockets so that we are compatible with
  the new socket backend for gen_tcp.

  Own Id: OTP-17392 Aux Id: PR-5050

- Enhance ssl:prf/4 handling and testing

  Own Id: OTP-17464

- Enhanced cipher suite filtering functionality, making sure TLS-1.3 and TLS-1.2
  cipher suites can be supported correctly together even when TLS-1.2 anonymous
  ciphers are included.

  Own Id: OTP-17501 Aux Id: GH-4978

- Enhance gracefulness especially in TLS-1.3

  Own Id: OTP-17530

## SSL 10.4.2

### Fixed Bugs and Malfunctions

- Handle cross-signed root certificates when old root expired as reported in
  GH-4877.

  Own Id: OTP-17475 Aux Id: GH-4877

- The signature selection algorithm has been changed to also verify if the
  client supports signatures using the elliptic curve of the server's
  public/private key pair. This change fixes #4958.

  Own Id: OTP-17529 Aux Id: PR-4979, GH-4958

### Improvements and New Features

- Slight optimization of certificate decoding.

  Own Id: OTP-17150 Aux Id: GH-4877

## SSL 10.4.1

### Fixed Bugs and Malfunctions

- Fix cache invalidation problem for CA certs provided by the cacertfile option.

  Own Id: OTP-17435 Aux Id: ERIERL-653

## SSL 10.4

### Fixed Bugs and Malfunctions

- Missing runtime dependencies has been added to this application.

  Own Id: OTP-17243 Aux Id: PR-4557

- TLS handshake should fail if OCSP staple is requested but missing. Note that
  OCSP support is still considered experimental and only partially implemented.

  Own Id: OTP-17343

### Improvements and New Features

- Removed ssl:ssl_accept/1,2,3 and ssl:cipher:suites/0,1 use ssl:handshake/1,2,3
  and ssl:cipher_suites/2,3 instead.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16974

- Make TLS handshakes in Erlang distribution concurrent.

  Own Id: OTP-17044 Aux Id: PR-2654

- Randomize internal `{active,n}` optimization when running Erlang distribution
  over TLS to spread RAM/CPU spike that may occur when starting up a big
  cluster.

  Own Id: OTP-17117 Aux Id: PR-2933

- TLS connections now support EdDSA certificates.

  Own Id: OTP-17142 Aux Id: PR-4756, GH-4637, GH-4650

- Enhance documentation and logging of certificate handling.

  Own Id: OTP-17384 Aux Id: GH-4800

## SSL 10.3.1.5

### Fixed Bugs and Malfunctions

- Correct corner case of unexpected message handling for pre TLS-1.3 versions,
  could cause "late failure" and make the server dependent on its handshake
  timeout to prevent possible DoS attacks.

  Own Id: OTP-18224

## SSL 10.3.1.4

### Fixed Bugs and Malfunctions

- The link to crypto:engine_load refered the function with wrong arity.

  Own Id: OTP-18173

## SSL 10.3.1.3

### Fixed Bugs and Malfunctions

- Improved handling of unexpected messages during the handshake, taking the
  right action for unexpected messages.

  Own Id: OTP-18145

## SSL 10.3.1.2

### Fixed Bugs and Malfunctions

- Handle cross-signed root certificates when old root expired as reported in
  GH-4877.

  Own Id: OTP-17475 Aux Id: GH-4877

- The signature selection algorithm has been changed to also verify if the
  client supports signatures using the elliptic curve of the server's
  public/private key pair. This change fixes #4958.

  Own Id: OTP-17529 Aux Id: PR-4979, GH-4958

### Improvements and New Features

- Slight optimization of certificate decoding.

  Own Id: OTP-17150 Aux Id: GH-4877

## SSL 10.3.1.1

### Fixed Bugs and Malfunctions

- Fix cache invalidation problem for CA certs provided by the cacertfile option.

  Own Id: OTP-17435 Aux Id: ERIERL-653

## SSL 10.3.1

### Fixed Bugs and Malfunctions

- Retain backwards compatible behavior of verify_fun when handling incomplete
  chains that are not verifiable.

  Own Id: OTP-17296 Aux Id: GH-4682

- Avoid server session handler crash, this will increase session ruse
  opportunities.

  Own Id: OTP-17348 Aux Id: ERIERL-641

## SSL 10.3

### Fixed Bugs and Malfunctions

- Fix CRL handling that previously could fail to find the issuer cert under some
  circumstances.

  Own Id: OTP-17261 Aux Id: GH-4589

- TLS-1.3 client could, under some circumstances, select an incorrect algorithm
  to sign the certificate verification message causing a TLS Decrypt Alert being
  issued by the server.

  Own Id: OTP-17281 Aux Id: GH-4620

- Correct handling of default values for emulated socket options and retain the
  order of the ssl options list to ensure backwards compatible behavior if
  options should be set more than once.

  Own Id: OTP-17282

### Improvements and New Features

- Enhance pre TLS-1.3 session handling so the client and server side handling is
  completely separated and client disregards oldest session when reaching max
  limit of the session table.

  Own Id: OTP-16876

- This change implements the early data feature for TLS 1.3 clients.

  TLS 1.3 allows clients to send data in the first flight using a Pre-Shared Key
  to authenticate the server and to encrypt the early data.

  Own Id: OTP-16985

- This change implements the early data feature for TLS 1.3 servers.

  Own Id: OTP-17042

## SSL 10.2.4.4

### Fixed Bugs and Malfunctions

- Improved handling of unexpected messages during the handshake, taking the
  right action for unexpected messages.

  Own Id: OTP-18145

## SSL 10.2.4.3

### Fixed Bugs and Malfunctions

- Fix cache invalidation problem for CA certs provided by the cacertfile option.

  Own Id: OTP-17435 Aux Id: ERIERL-653

## SSL 10.2.4.2

### Fixed Bugs and Malfunctions

- Fix handling of emulated socket options, the previous patch was incomplete,

  Own Id: OTP-17305

## SSL 10.2.4.1

### Fixed Bugs and Malfunctions

- Backport of OTP-17282

  Correct handling of default values for emulated socket options and retain the
  order of the ssl options list to ensure backwards compatible behavior if
  options should be set more than once.

  Own Id: OTP-17289 Aux Id: GH-4585

## SSL 10.2.4

### Fixed Bugs and Malfunctions

- Enhance logging option log_level to support none and all, also restore
  backwards compatibility for log_alert option.

  Own Id: OTP-17228 Aux Id: ERIERL-614

## SSL 10.2.3

### Fixed Bugs and Malfunctions

- Avoid race when the first two upgrade server handshakes (that is servers that
  use a gen_tcp socket as input to ssl:handshake/2,3) start close to each other.
  Could lead to that one of the handshakes would fail.

  Own Id: OTP-17190 Aux Id: ERIERL-606

## SSL 10.2.2

### Fixed Bugs and Malfunctions

- Avoid that upgrade (from TCP to TLS) servers starts multiple session cache
  handlers for the same server. This applies to Erlang distribution over TLS
  servers.

  Own Id: OTP-17139 Aux Id: ERL-1458, OTP-16239

- Legacy cipher suites defined before TLS-1.2 (but still supported) should be
  possible to use in TLS-1.2. They where accidentally excluded for available
  cipher suites for TLS-1.2 in OTP-23.2.2.

  Own Id: OTP-17174 Aux Id: ERIERL-597

### Improvements and New Features

- Enable Erlang distribution over TLS to run TLS-1.3, although TLS-1.2 will
  still be default.

  Own Id: OTP-16239 Aux Id: ERL-1458, OTP-17139

## SSL 10.2.1

### Fixed Bugs and Malfunctions

- Fix CVE-2020-35733 this only affects ssl-10.2 (OTP-23.2). This vulnerability
  could enable a man in the middle attack using a fake chain to a known trusted
  ROOT. Also limits alternative chain handling, for handling of possibly
  extraneous certs, to improve memory management.

  Own Id: OTP-17098

### Improvements and New Features

- Add support for AES CCM based cipher suites defined in RFC 7251

  Also Correct cipher suite name conversion to OpenSSL names. A few names where
  corrected earlier in OTP-16267 For backwards compatible reasons we support
  usage of openSSL names for cipher suites. Mostly anonymous suites names where
  incorrect, but also some legacy suites.

  Own Id: OTP-17100

## SSL 10.2

### Fixed Bugs and Malfunctions

- SSL's Erlang Distribution Protocol modules inet_tls_dist and inet6_tls_dist
  lacked a callback function, so the start flag "-dist_listen false" did not
  work, which has now been fixed.

  Own Id: OTP-15126 Aux Id: ERL-1375

- Correct OpenSSL names for newer cipher suites using DHE in their name that
  accidentally got the wrong value when fixing other older names using EDH
  instead.

  Own Id: OTP-16267 Aux Id: ERIERL-571, ERIERL-477

- This change improves the handling of DTLS listening dockets, making it
  possible to open multiple listeners on the same port with different IP
  addresses.

  Own Id: OTP-16849 Aux Id: ERL-1339

- Fix a bug that causes cross-build failure.

  This change excludes the ssl.d dependency file from the source tarballs.

  Own Id: OTP-16921

- This change fixes ssl:peername/1 when called on a DTLS client socket.

  Own Id: OTP-16923 Aux Id: ERL-1341, PR-2786

- Retain emulation of active once on a closed socket to behave as before 23.1

  Own Id: OTP-17018 Aux Id: ERL-1409

- Corrected server session cache entry deletion pre TLS-1.3. May increase
  session reuse.

  Own Id: OTP-17019 Aux Id: ERL-1412

### Improvements and New Features

- Handle extraneous certs in certificate chains as well as chains that are
  incomplete but can be reconstructed or unordered chains. The cert and certfile
  options will now accept a list of certificates so that the user may specify
  the chain explicitly.

  Also, the default value of the depth option has been increased to allow longer
  chains by default.

  Own Id: OTP-16277

- This change implements optional NSS-style keylog in
  ssl:connection_information/2 for debugging purposes.

  The keylog contains various TLS secrets that can be loaded in Wireshark to
  decrypt TLS packets.

  Own Id: OTP-16445 Aux Id: PR-2823

- Use new gen_statem feature of changing callback mode to improve code
  maintainability.

  Own Id: OTP-16529

- The handling of Service Name Indication has been aligned with RFC8446.

  Own Id: OTP-16762

- Add explicit session reuse option to TLS clients for pre TLS-1.3 sessions.
  Also, add documentation to Users Guide for such sessions.

  Own Id: OTP-16893

## SSL 10.1

### Fixed Bugs and Malfunctions

- If a passive socket is created, ssl:recv/2,3 is never called and then the peer
  closes the socket the controlling process will no longer receive an active
  close message.

  Own Id: OTP-16697 Aux Id: ERIERL-496

- Data deliver with ssl:recv/2,3 could fail for when using packet mode. This has
  been fixed by correcting the flow control handling of passive sockets when
  packet mode is used.

  Own Id: OTP-16764

- This change fixes a potential man-in-the-middle vulnerability when the ssl
  client is configured to automatically handle session tickets
  (\{session_tickets, auto\}).

  Own Id: OTP-16765

- Fix the internal handling of options 'verify' and 'verify_fun'.

  This change fixes a vulnerability when setting the ssl option 'verify' to
  verify_peer in a continued handshake won't take any effect resulting in the
  acceptance of expired peer certificates.

  Own Id: OTP-16767 Aux Id: ERIERL-512

- This change fixes the handling of stateless session tickets when anti-replay
  is enabled.

  Own Id: OTP-16776 Aux Id: ERL-1316

- Fix a crash due to the faulty handling of stateful session tickets received by
  servers expecting stateless session tickets.

  This change also improves the handling of faulty/invalid tickets.

  Own Id: OTP-16777 Aux Id: ERL-1317

- Correct flow ctrl checks from OTP-16764 to work as intended. Probably will not
  have a noticeable affect but will make connections more well behaved under
  some circumstances.

  Own Id: OTP-16837 Aux Id: ERL-1319, OTP-16764

- Distribution over TLS could exhibit livelock-like behaviour when there is a
  constant stream of distribution messages. Distribution data is now chunked
  every 16 Mb to avoid that.

  Own Id: OTP-16851 Aux Id: PR-2703

### Improvements and New Features

- Implement the cookie extension for TLS 1.3.

  Own Id: OTP-15855

- Experimental OCSP client support.

  Own Id: OTP-16448

- TLS 1.0 -TLS-1.2 sessions tables now have a absolute max value instead of
  using a shrinking mechanism when reaching the limit. To avoid out of memory
  problems under heavy load situations. Note that this change infers that
  implementations of ssl_session_cache_api needs to implement the size function
  (introduce in OTP 19) for session reuse to be optimally utilized.

  Own Id: OTP-16802 Aux Id: ERIERL-516

## SSL 10.0

### Fixed Bugs and Malfunctions

- Fix a bug that causes cross-build failure.

  This change excludes the ssl.d dependency file from the source tar balls.

  Own Id: OTP-16562 Aux Id: ERL-1168

- Correct translation of OpenSSL legacy names for two legacy cipher suites

  Own Id: OTP-16573 Aux Id: ERIERL-477

- Correct documentation for PSK identity and SRP username.

  Own Id: OTP-16585

- Make sure client hostname check is run when client uses its own verify_fun

  Own Id: OTP-16626 Aux Id: ERL-1232

- Improved signature selection mechanism in TLS 1.3 for increased
  interoperability.

  Own Id: OTP-16638 Aux Id: ERL-1206

### Improvements and New Features

- Drop support for SSL-3.0. Support for this legacy TLS version has not been
  enabled by default since OTP 19. Now all code to support it has been removed,
  that is SSL-3.0 protocol version can not be used and is considered invalid.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14790

- Added support for RSA-PSS signature schemes

  Own Id: OTP-15247

- Improve interoperability by implementing the middlebox compatibility mode.

  The middlebox compatibility mode makes the TLS 1.3 handshake look more like a
  TLS 1.2 handshake and increases the chance of successfully establishing TLS
  1.3 connections through legacy middleboxes.

  Own Id: OTP-15589

- Utilize new properties of
  [`erlang:dist_ctrl_get_data()`](`erlang:dist_ctrl_get_data/1`) for performance
  improvement of Erlang distribution over TLS.

  Own Id: OTP-16127 Aux Id: OTP-15618

- Calls of deprecated functions in the
  [Old Crypto API](`e:crypto:new_api.md#the-old-api`) are replaced by calls of
  their [substitutions](`e:crypto:new_api.md#the-new-api`).

  Own Id: OTP-16346

- Implement cipher suite TLS_AES_128_CCM_8_SHA256.

  Own Id: OTP-16391

- This change adds TLS-1.3 to the list of default supported versions. That is,
  TLS-1.3 and TLS-1.2 are configured when ssl option 'versions' is not
  explicitly set.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16400

- Refactored the internal handling of deprecated and removed functions.

  Own Id: OTP-16469

- Extended ssl:versions so that it lists supported, available and implemented
  TLS/DTLS versions.

  Own Id: OTP-16519

- Added new option exclusive for ssl:cipher_suites/2,3

  Own Id: OTP-16532

- Avoid DoS attack against stateful session_tickets by making session ticket ids
  unpredictable.

  Own Id: OTP-16533

- Add support for the max_fragment_length extension (RFC 6066).

  Own Id: OTP-16547 Aux Id: PR-2547

- Add srp_username in ssl:connection_info, update the document with types of
  this function.

  Own Id: OTP-16584

## SSL 9.6.2.3

### Fixed Bugs and Malfunctions

- Correct flow ctrl checks from OTP-16764 to work as intended. Probably will not
  have a noticeable affect but will make connections more well behaved under
  some circumstances.

  Own Id: OTP-16837 Aux Id: ERL-1319, OTP-16764

- Fix a bug that causes cross-build failure.

  This change excludes the ssl.d dependency file from the source tar balls.

  Own Id: OTP-16921

## SSL 9.6.2.2

### Fixed Bugs and Malfunctions

- Data deliver with ssl:recv/2,3 could fail for when using packet mode. This has
  been fixed by correcting the flow control handling of passive sockets when
  packet mode is used.

  Own Id: OTP-16764

- Fix the internal handling of options 'verify' and 'verify_fun'.

  This change fixes a vulnerability when setting the ssl option 'verify' to
  verify_peer in a continued handshake won't take any effect resulting in the
  acceptance of expired peer certificates.

  Own Id: OTP-16767 Aux Id: ERIERL-512

## SSL 9.6.2.1

### Improvements and New Features

- If a passive socket is created, ssl:recv/2,3 is never called and then the peer
  closes the socket the controlling process will no longer receive an active
  close message.

  Own Id: OTP-16697 Aux Id: ERIERL-496

## SSL 9.6.2

### Fixed Bugs and Malfunctions

- Fix timing bug that could cause ssl sockets to become unresponsive after an
  ssl:recv/3 call timed out

  Own Id: OTP-16619 Aux Id: ERL-1213

## SSL 9.6.1

### Fixed Bugs and Malfunctions

- Correct error handling when the partial_chain fun claims a certificate to be
  the trusted cert that is not part of the chain. This bug would hide the
  appropriate alert generating an "INTERNAL_ERROR" alert instead.

  Own Id: OTP-16567 Aux Id: ERIERL-481

## SSL 9.6

### Fixed Bugs and Malfunctions

- Correct handling of TLS record limit in TLS-1.3. The max value differs from
  previous versions. Also the payload data max record check was broken, that is
  record overflow problems could occur if user sent large amounts of data.

  Own Id: OTP-16258

- Correct close handling for DTLS

  Own Id: OTP-16348 Aux Id: ERL-1110

- Fix ssl:getstat/1-2 to also work for DTLS sockets

  Own Id: OTP-16352 Aux Id: ERL-1099

- Correct internal handling och socket active mode to avoid reviving TCP data
  aimed for a downgraded TLS socket.

  Own Id: OTP-16425

- When using the host name as fallback for SNI (server name indication) strip a
  possible trailing dot that is allowed in a host name but not in the SNI. Also
  if the server receives a SNI with a trailing dot send an UNRECOGNIZED_NAME
  alert.

  Own Id: OTP-16437 Aux Id: ERL-1135

- Immediately remove session entries if handshake is abruptly closed at
  transport level.

  Own Id: OTP-16479

### Improvements and New Features

- Implementation of the key and initialization vector update feature, and
  general hardening of TLS 1.3.

  There are cryptographic limits on the amount of plaintext which can be safely
  encrypted under a given set of keys.

  This change enforces those limits by triggering automatic key updates on TLS
  1.3 connections.

  Own Id: OTP-15856

- Add support for TLS 1.3 Session Tickets (stateful and stateless). This allows
  session resumption using keying material from a previous successful handshake.

  Own Id: OTP-16253

- Add support for key exchange with Edward curves and PSS-RSA padding in
  signature verification.

  Own Id: OTP-16528

## SSL 9.5.3

### Fixed Bugs and Malfunctions

- Enhance error handling, all ALERTS shall be handled gracefully and not cause a
  crash.

  Own Id: OTP-16413 Aux Id: ERL-1136

- Enhance alert logging, in some places the role indication of the alert origin
  was missing. So the log would say undefined instead of client or server.

  Own Id: OTP-16424

- Two different optimizations did not work together and resulted in the possible
  breakage of connections using stream ciphers (that is RC4). Reworked the
  implementation to avoid this.

  Own Id: OTP-16426 Aux Id: ERL-1136

## SSL 9.5.2

### Fixed Bugs and Malfunctions

- Fix the handling of GREASE values sent by web browsers when establishing TLS
  1.3 connections. This change improves handling of GREASE values in various
  protocol elements sent in a TLS 1.3 ClientHello.

  Own Id: OTP-16388 Aux Id: ERL-1130

- Correct DTLS listen emulation, could cause problems with opening a new DTLS
  listen socket for a port previously used by a now closed DTLS listen socket.

  Own Id: OTP-16396 Aux Id: ERL-1118

## SSL 9.5.1

### Fixed Bugs and Malfunctions

- Add missing alert handling clause for TLS record handling. Could sometimes
  cause confusing error behaviors of TLS connections.

  Own Id: OTP-16357 Aux Id: ERL-1166

- Fix handling of ssl:recv that happens during a renegotiation. Using the
  passive receive function ssl:recv/\[2,3] during a renegotiation would fail the
  connection with unexpected msg.

  Own Id: OTP-16361

## SSL 9.5

### Fixed Bugs and Malfunctions

- Corrected CRL handling which could cause CRL verification to fail. This could
  happen when the CRL distribution point explicitly specifies the CRL issuer,
  that is not using the fallback.

  Own Id: OTP-16156 Aux Id: ERL-1030

- Correct handling of unordered chains so that it works as expected

  Own Id: OTP-16293

- Fix bug causing ssl application to crash when handshake is paused and
  ClientHello contains extensions for session resumption
  (psk_key_exchange_modes, pre_shared_key).

  Own Id: OTP-16295 Aux Id: ERL-1095

- Fix connectivity problems with legacy servers when client is configured to
  support a range of protocol versions including TLS 1.3.

  Own Id: OTP-16303

### Improvements and New Features

- Improve session handling for TLS-1.3 compatibility mode and cleaner internal
  handling so that removal of old session data can be more efficient, hopefully
  mitigating problems with big session tables during heavy load.

  Own Id: OTP-15524 Aux Id: OTP-15352

- Correct handling of DTLS listen socket emulation. Could cause failure to
  create new listen socket after process that owned previous listen socket died.

  Own Id: OTP-15809 Aux Id: ERL-917

- Add detailed info in ALERT description when client does not send a requested
  cert.

  Own Id: OTP-16266

## SSL 9.4

### Fixed Bugs and Malfunctions

- Handling of zero size fragments in TLS could cause an infinite loop. This has
  now been corrected.

  Own Id: OTP-15328 Aux Id: ERIERL-379

- DTLS record check needs to consider that a resent hello message can have a
  different version than the negotiated.

  Own Id: OTP-15807 Aux Id: ERL-920

### Improvements and New Features

- Basic support for TLS 1.3 Client for experimental use. For more information
  see the Standards Compliance chapter of the User's Guide.

  Own Id: OTP-15431

- Correct solution for retaining tcp flow control OTP-15802 (ERL-934) as to not
  break ssl:recv as reported in (ERL-938)

  Own Id: OTP-15823 Aux Id: ERL-934, ERL-938

- Enhance dialyzer specs to reflect implementation better and avoid dialyzer
  warnings for the user that wants to use TLS with unix domain sockets.

  Own Id: OTP-15851 Aux Id: PR-2235

- Add support for ECDSA signature algorithms in TLS 1.3.

  Own Id: OTP-15854

- Correct error handling of TLS downgrade, possible return values form
  ssl:close/2 when downgrading is \{ok, Port\} or \{error, Reason\}, it could
  happen that only ok was returned instead of \{error, closed\} when downgrade
  failed due to that the peer closed the TCP connection.

  Own Id: OTP-16027

## SSL 9.3.5

### Improvements and New Features

- Enhance error handling for erroneous alerts from the peer.

  Own Id: OTP-15943

## SSL 9.3.4

### Fixed Bugs and Malfunctions

- Fix handling of certificate decoding problems in TLS 1.3 similarly as in TLS
  1.2.

  Own Id: OTP-15900

- Hibernation now works as expected in all cases, was accidentally broken by
  optimization efforts.

  Own Id: OTP-15910

- Fix interoperability problems with openssl when the TLS 1.3 server is
  configured with the option signature_algs_cert.

  Own Id: OTP-15913

## SSL 9.3.3

### Fixed Bugs and Malfunctions

- Correct handshake handling, might cause strange symptoms such as ASN.1
  certificate decoding issues.

  Own Id: OTP-15879 Aux Id: ERL-968

- Fix handling of the signature_algorithms_cert extension in the ClientHello
  handshake message.

  Own Id: OTP-15887 Aux Id: ERL-973

- Handle new ClientHello extensions when handshake is paused by the \{handshake,
  hello\} ssl option.

  Own Id: OTP-15888 Aux Id: ERL-975

## SSL 9.3.2

### Fixed Bugs and Malfunctions

- Returned "alert error string" is now same as logged alert string

  Own Id: OTP-15844

- Fix returned extension map fields to follow the documentation.

  Own Id: OTP-15862 Aux Id: ERL-951

- Avoid DTLS crash due to missing gen_server return value in DTLS packet demux
  process.

  Own Id: OTP-15864 Aux Id: ERL-962

## SSL 9.3.1

### Fixed Bugs and Malfunctions

- Missing check of size of user_data_buffer made internal socket behave as an
  active socket instead of active N. This could cause memory problems.

  Own Id: OTP-15825 Aux Id: ERL-934, OTP-15823

## SSL 9.3

### Fixed Bugs and Malfunctions

- The distribution handshake with TLS distribution (`inet_tls_dist`) does now
  utilize the socket option `{nodelay, true}`, which decreases the distribution
  setup time significantly.

  Own Id: OTP-14792

- Correct shutdown reason to avoid an incorrect crash report

  Own Id: OTP-15710 Aux Id: ERL-893

- Enhance documentation and type specifications.

  Own Id: OTP-15746 Aux Id: ERIERL-333

### Improvements and New Features

- TLS-1.0, TLS-1.1 and DTLS-1.0 are now considered legacy and not supported by
  default

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14865

- Use new logger API in ssl. Introduce log levels and verbose debug logging for
  SSL.

  Own Id: OTP-15055

- Add new API function str_to_suite/1, cipher_suites/3 (list cipher suites as
  rfc or OpenSSL name strings) and suite_to_openssl_str/1

  Own Id: OTP-15483 Aux Id: ERL-924

- Basic support for TLS 1.3 Server for experimental use. The client is not yet
  functional, for more information see the Standards Compliance chapter of the
  User's Guide.

  Own Id: OTP-15591

- Add support for PSK CCM ciphers from RFC 6655

  Own Id: OTP-15626

## SSL 9.2.3.7

### Fixed Bugs and Malfunctions

- Data deliver with ssl:recv/2,3 could fail for when using packet mode. This has
  been fixed by correcting the flow control handling of passive sockets when
  packet mode is used.

  Own Id: OTP-16764

## SSL 9.2.3.6

### Fixed Bugs and Malfunctions

- Fix timing bug that could cause ssl sockets to become unresponsive after an
  ssl:recv/3 call timed out

  Own Id: OTP-16619 Aux Id: ERL-1213

## SSL 9.2.3.5

### Fixed Bugs and Malfunctions

- Handling of zero size fragments in TLS could cause an infinite loop. This has
  now been corrected.

  Own Id: OTP-15328 Aux Id: ERIERL-379

## SSL 9.2.3.4

### Fixed Bugs and Malfunctions

- Hibernation now works as expected in all cases, was accidentally broken by
  optimization efforts.

  Own Id: OTP-15910

## SSL 9.2.3.3

### Fixed Bugs and Malfunctions

- Correct handshake handling, might cause strange symptoms such as ASN.1
  certificate decoding issues.

  Own Id: OTP-15879 Aux Id: ERL-968

## SSL 9.2.3.2

### Fixed Bugs and Malfunctions

- Returned "alert error string" is now same as logged alert string

  Own Id: OTP-15844

## SSL 9.2.3.1

### Fixed Bugs and Malfunctions

- Correct solution for retaining tcp flow control OTP-15802 (ERL-934) as to not
  break ssl:recv as reported in (ERL-938)

  Own Id: OTP-15823 Aux Id: ERL-934, ERL-938

## SSL 9.2.3

### Fixed Bugs and Malfunctions

- Missing check of size of user_data_buffer made internal socket behave as an
  active socket instead of active N. This could cause memory problems.

  Own Id: OTP-15802 Aux Id: ERL-934

### Improvements and New Features

- Back port of bug fix ERL-893 from OTP-22 and document enhancements that will
  solve dialyzer warnings for users of the ssl application.

  This change also affects public_key, eldap (and inet doc).

  Own Id: OTP-15785 Aux Id: ERL-929, ERL-893, PR-2215

## SSL 9.2.2

### Fixed Bugs and Malfunctions

- With the default BEAST Mitigation strategy for TLS 1.0 an empty TLS fragment
  could be sent after a one-byte fragment. This glitch has been fixed.

  Own Id: OTP-15054 Aux Id: ERIERL-346

## SSL 9.2.1

### Fixed Bugs and Malfunctions

- The timeout for a passive receive was sometimes not cancelled and later caused
  a server crash. This bug has now been corrected.

  Own Id: OTP-14701 Aux Id: ERL-883, ERL-884

- Add tag for passive message (active N) in cb_info to retain transport
  transparency.

  Own Id: OTP-15679 Aux Id: ERL-861

## SSL 9.2

### Fixed Bugs and Malfunctions

- Fix bug that an incorrect return value for gen_statem could be created when
  alert was a result of handling renegotiation info extension

  Own Id: OTP-15502

- Correct check for 3des_ede_cbc, could cause ssl to claim to support
  3des_ede_cbc when cryptolib does not.

  Own Id: OTP-15539

- Improved DTLS error handling, avoids unexpected connection failure in rare
  cases.

  Own Id: OTP-15561

- Corrected active once emulation bug that could cause the ssl_closed meassage
  to not be sent. Bug introduced by OTP-15449

  Own Id: OTP-15666 Aux Id: ERIERL-316,

### Improvements and New Features

- Add client option \{reuse_session, SessionID::binary()\} that can be used
  together with new option value \{reuse_sessions, save\}. This makes it
  possible to reuse a session from a specific connection establishment.

  Own Id: OTP-15369

- The Reason part of of the error return from the functions connect and
  handshake has a better and documented format. This will sometimes differ from
  previous returned reasons, however those where only documented as term() and
  should for that reason not be relied on.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15423

- Refactor of state handling to improve TLS application data throughput and
  reduce CPU overhead

  Own Id: OTP-15445

- The SSL code has been optimized in many small ways to reduce CPU load for
  encryption/decryption, especially for Erlang's distribution protocol over TLS.

  Own Id: OTP-15529

- Add support for active N

  Own Id: OTP-15665 Aux Id: ERL-811, PR-2072

## SSL 9.1.2

### Fixed Bugs and Malfunctions

- Fix encoding of the SRP extension length field in ssl. The old encoding of the
  SRP extension length could cause interoperability problems with third party
  SSL implementations when SRP was used.

  Own Id: OTP-15477 Aux Id: ERL-790

- Guarantee active once data delivery, handling TCP stream properly.

  Own Id: OTP-15504 Aux Id: ERL-371

- Correct gen_statem returns for some error cases

  Own Id: OTP-15505

## SSL 9.1.1

### Fixed Bugs and Malfunctions

- Fixed renegotiation bug. Client did not handle server initiated renegotiation
  correctly after rewrite to two connection processes, due to ERL-622 commit
  d87ac1c55188f5ba5cdf72384125d94d42118c18. This could manifest it self as a "
  bad_record_mac" alert.

  Also included are some optimizations

  Own Id: OTP-15489 Aux Id: ERL-308

## SSL 9.1

### Fixed Bugs and Malfunctions

- PEM cache was not evicting expired entries due to due to timezone confusion.

  Own Id: OTP-15368

- Make sure an error is returned if a "transport_accept socket" is used in some
  other call than ssl:handshake\* or ssl:controlling_process

  Own Id: OTP-15384 Aux Id: ERL-756

- Fix timestamp handling in the PEM-cache could cause entries to not be
  invalidated at the correct time.

  Own Id: OTP-15402

- Extend check for undelivered data at closing, could under some circumstances
  fail to deliver all data that was actually received.

  Own Id: OTP-15412 Aux Id: ERL-731

- Correct signature check for TLS-1.2 that allows different algorithms for
  signature of peer cert and peer cert key. Not all allowed combinations where
  accepted.

  Own Id: OTP-15415 Aux Id: ERL-763

- Correct gen_statem return value, could cause renegotiation to fail.

  Own Id: OTP-15418 Aux Id: ERL-770

### Improvements and New Features

- Add engine support for RSA key exchange

  Own Id: OTP-15420 Aux Id: ERIERL-268

- ssl now uses active n internally to boost performance. Old active once
  behavior can be restored by setting application variable see manual page for
  ssl application (man 6).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15449

## SSL 9.0.3

### Fixed Bugs and Malfunctions

- Correct alert handling with new TLS sender process, from ssl-9.0.2. CLOSE
  ALERTS could under some circumstances be encoded using an incorrect cipher
  state. This would cause the peer to regard them as unknown messages.

  Own Id: OTP-15337 Aux Id: ERL-738

- Correct handling of socket packet option with new TLS sender process, from
  ssl-9.0.2. When changing the socket option \{packet, 1|2|3|4\} with
  ssl:setopts/2 the option must internally be propagated to the sender process
  as well as the reader process as this particular option also affects the data
  to be sent.

  Own Id: OTP-15348 Aux Id: ERL-747

## SSL 9.0.2

### Fixed Bugs and Malfunctions

- Use separate processes for sending and receiving application data for TLS
  connections to avoid potential deadlock that was most likely to occur when
  using TLS for Erlang distribution. Note does not change the API.

  Own Id: OTP-15122

- Correct handling of empty server SNI extension

  Own Id: OTP-15168

- Correct PSK cipher suite handling and add selected_cipher_suite to connection
  information

  Own Id: OTP-15172

- Adopt to the fact that cipher suite sign restriction are relaxed in TLS-1.2

  Own Id: OTP-15173

- Enhance error handling of non existing PEM files

  Own Id: OTP-15174

- Correct close handling of transport accepted sockets in the error state

  Own Id: OTP-15216

- Correct PEM cache to not add references to empty entries when PEM file does
  not exist.

  Own Id: OTP-15224

- Correct handling of all PSK cipher suites

  Before only some PSK suites would be correctly negotiated and most PSK ciphers
  suites would fail the connection.

  Own Id: OTP-15285

### Improvements and New Features

- TLS will now try to order certificate chains if they appear to be unordered.
  That is prior to TLS 1.3, “certificate_list” ordering was required to be
  strict, however some implementations already allowed for some flexibility. For
  maximum compatibility, all implementations SHOULD be prepared to handle
  potentially extraneous certificates and arbitrary orderings from any TLS
  version.

  Own Id: OTP-12983

- TLS will now try to reconstructed an incomplete certificate chains from its
  local CA-database and use that data for the certificate path validation. This
  especially makes sense for partial chains as then the peer might not send an
  intermediate CA as it is considered the trusted root in that case.

  Own Id: OTP-15060

- Option keyfile defaults to certfile and should be trumped with key. This
  failed for engine keys.

  Own Id: OTP-15193

- Error message improvement when own certificate has decoding issues, see also
  issue ERL-668.

  Own Id: OTP-15234

- Correct dialyzer spec for key option

  Own Id: OTP-15281

## SSL 9.0.1

### Fixed Bugs and Malfunctions

- Correct cipher suite handling for ECDHE\_\*, the incorrect handling could
  cause an incorrrect suite to be selected and most likely fail the handshake.

  Own Id: OTP-15203

## SSL 9.0

### Fixed Bugs and Malfunctions

- Correct handling of ECDH suites.

  Own Id: OTP-14974

- Proper handling of clients that choose to send an empty answer to a
  certificate request

  Own Id: OTP-15050

### Improvements and New Features

- Distribution over SSL (inet_tls) has, to improve performance, been rewritten
  to not use intermediate processes and ports.

  Own Id: OTP-14465

- Add support for ECDHE_PSK cipher suites

  Own Id: OTP-14547

- For security reasons no longer support 3-DES cipher suites by default

  \*** INCOMPATIBILITY with possibly \***

  Own Id: OTP-14768

- For security reasons RSA-key exchange cipher suites are no longer supported by
  default

  \*** INCOMPATIBILITY with possible \***

  Own Id: OTP-14769

- The interoperability option to fallback to insecure renegotiation now has to
  be explicitly turned on.

  \*** INCOMPATIBILITY with possibly \***

  Own Id: OTP-14789

- Drop support for SSLv2 enabled clients. SSLv2 has been broken for decades and
  never supported by the Erlang SSL/TLS implementation. This option was by
  default disabled and enabling it has proved to sometimes break connections not
  using SSLv2 enabled clients.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14824

- Remove CHACHA20_POLY1305 ciphers form default for now. We have discovered
  interoperability problems, ERL-538, that we believe needs to be solved in
  crypto.

  \*** INCOMPATIBILITY with possibly \***

  Own Id: OTP-14882

- Generalize DTLS packet multiplexing to make it easier to add future DTLS
  features and uses.

  Own Id: OTP-14888

- Use uri_string module instead of http_uri.

  Own Id: OTP-14902

- The SSL distribution protocol `-proto inet_tls` has stopped setting the SSL
  option `server_name_indication`. New verify funs for client and server in
  `inet_tls_dist` has been added, not documented yet, that checks node name if
  present in peer certificate. Usage is still also yet to be documented.

  Own Id: OTP-14969 Aux Id: OTP-14465, ERL-598

- Deprecate ssl:ssl_accept/\[1,2,3] in favour of ssl:handshake/\[1,2,3]

  Own Id: OTP-15056

- Customizes the hostname verification of the peer certificate, as different
  protocols that use TLS such as HTTP or LDAP may want to do it differently

  Own Id: OTP-15102 Aux Id: ERL-542, OTP-14962

- Add utility function for converting erlang cipher suites to a string
  representation (ERL-600).

  Own Id: OTP-15106

- First version with support for DTLS

  Own Id: OTP-15142

## SSL 8.2.6.4

### Fixed Bugs and Malfunctions

- Add engine support for RSA key exchange

  Own Id: OTP-15420

## SSL 8.2.6.3

### Fixed Bugs and Malfunctions

- Extend check for undelivered data at closing, could under some circumstances
  fail to deliverd all data that was acctualy recivied.

  Own Id: OTP-15412

## SSL 8.2.6.2

### Fixed Bugs and Malfunctions

- Correct handling of empty server SNI extension

  Own Id: OTP-15168

- Correct cipher suite handling for ECDHE\_\*, the incorrect handling could
  cause an incorrrect suite to be selected and most likely fail the handshake.

  Own Id: OTP-15203

## SSL 8.2.6.1

### Fixed Bugs and Malfunctions

- Improve cipher suite handling correcting ECC and TLS-1.2 requierments.
  Backport of solution for ERL-641

  Own Id: OTP-15178

### Improvements and New Features

- Option keyfile defaults to certfile and should be trumped with key. This
  failed for engine keys.

  Own Id: OTP-15193

## SSL 8.2.6

### Fixed Bugs and Malfunctions

- Proper handling of clients that choose to send an empty answer to a
  certificate request

  Own Id: OTP-15050

## SSL 8.2.5

### Fixed Bugs and Malfunctions

- Fix filter function to not incorrectly exclude AEAD cipher suites

  Own Id: OTP-14981

## SSL 8.2.4

### Fixed Bugs and Malfunctions

- Optimization of bad merge conflict resolution causing dubble decode

  Own Id: OTP-14843

- Restore error propagation to OTP-19.3 behaviour, in OTP-20.2 implementation
  adjustments to gen_statem needed some further adjustments to avoid a race
  condition. This could cause a TLS server to not always report file path errors
  correctly.

  Own Id: OTP-14852

- Corrected RC4 suites listing function to regard TLS version

  Own Id: OTP-14871

- Fix alert handling so that unexpected messages are logged and alerted
  correctly

  Own Id: OTP-14919

- Correct handling of anonymous cipher suites

  Own Id: OTP-14952

### Improvements and New Features

- Added new API functions to facilitate cipher suite handling

  Own Id: OTP-14760

- Correct TLS_FALLBACK_SCSV handling so that this special flag suite is always
  placed last in the cipher suite list in accordance with the specs. Also make
  sure this functionality is used in DTLS.

  Own Id: OTP-14828

- Add TLS record version sanity check for early as possible error detection and
  consistency in ALERT codes generated

  Own Id: OTP-14892

## SSL 8.2.3

### Fixed Bugs and Malfunctions

- Packet options cannot be supported for unreliable transports, that is, packet
  option for DTLS over udp will not be supported.

  Own Id: OTP-14664

- Ensure data delivery before close if possible. This fix is related to fix in
  PR-1479.

  Own Id: OTP-14794

### Improvements and New Features

- The crypto API is extended to use private/public keys stored in an Engine for
  sign/verify or encrypt/decrypt operations.

  The ssl application provides an API to use this new engine concept in TLS.

  Own Id: OTP-14448

- Implemented renegotiation for DTLS

  Own Id: OTP-14563

- A new command line option `-ssl_dist_optfile` has been added to facilitate
  specifying the many options needed when using SSL as the distribution
  protocol.

  Own Id: OTP-14657

## SSL 8.2.2

### Fixed Bugs and Malfunctions

- TLS sessions must be registered with SNI if provided, so that sessions where
  client hostname verification would fail cannot connect reusing a session
  created when the server name verification succeeded.

  Own Id: OTP-14632

- An erlang TLS server configured with cipher suites using rsa key exchange, may
  be vulnerable to an Adaptive Chosen Ciphertext attack (AKA Bleichenbacher
  attack) against RSA, which when exploited, may result in plaintext recovery of
  encrypted messages and/or a Man-in-the-middle (MiTM) attack, despite the
  attacker not having gained access to the server’s private key itself.
  [CVE-2017-1000385](https://nvd.nist.gov/vuln/detail/CVE-2017-1000385)

  Exploiting this vulnerability to perform plaintext recovery of encrypted
  messages will, in most practical cases, allow an attacker to read the
  plaintext only after the session has completed. Only TLS sessions established
  using RSA key exchange are vulnerable to this attack.

  Exploiting this vulnerability to conduct a MiTM attack requires the attacker
  to complete the initial attack, which may require thousands of server
  requests, during the handshake phase of the targeted session within the window
  of the configured handshake timeout. This attack may be conducted against any
  TLS session using RSA signatures, but only if cipher suites using RSA key
  exchange are also enabled on the server. The limited window of opportunity,
  limitations in bandwidth, and latency make this attack significantly more
  difficult to execute.

  RSA key exchange is enabled by default although least prioritized if server
  order is honored. For such a cipher suite to be chosen it must also be
  supported by the client and probably the only shared cipher suite.

  Captured TLS sessions encrypted with ephemeral cipher suites (DHE or ECDHE)
  are not at risk for subsequent decryption due to this vulnerability.

  As a workaround if default cipher suite configuration was used you can
  configure the server to not use vulnerable suites with the ciphers option like
  this:

  `{ciphers, [Suite || Suite <- ssl:cipher_suites(), element(1,Suite) =/= rsa]}`

  that is your code will look somethingh like this:

  `ssl:listen(Port, [{ciphers, [Suite || Suite <- ssl:cipher_suites(), element(1,S) =/= rsa]} | Options]).`

  Thanks to Hanno Böck, Juraj Somorovsky and Craig Young for reporting this
  vulnerability.

  Own Id: OTP-14748

### Improvements and New Features

- If no SNI is available and the hostname is an IP-address also check for
  IP-address match. This check is not as good as a DNS hostname check and
  certificates using IP-address are not recommended.

  Own Id: OTP-14655

## SSL 8.2.1

### Fixed Bugs and Malfunctions

- Max session table works correctly again

  Own Id: OTP-14556

### Improvements and New Features

- Customize alert handling for DTLS over UDP to mitigate DoS attacks

  Own Id: OTP-14078

- Improved error propagation and reports

  Own Id: OTP-14236

## SSL 8.2

### Fixed Bugs and Malfunctions

- ECDH-ECDSA key exchange supported, was accidentally dismissed in earlier
  versions.

  Own Id: OTP-14421

- Correct close semantics for active once connections. This was a timing
  dependent bug the resulted in the close message not always reaching the ssl
  user process.

  Own Id: OTP-14443

### Improvements and New Features

- TLS-1.2 clients will now always send hello messages on its own format, as
  opposed to earlier versions that will send the hello on the lowest supported
  version, this is a change supported by the latest RFC.

  This will make interoperability with some newer servers smoother. Potentially,
  but unlikely, this could cause a problem with older servers if they do not
  adhere to the RFC and ignore unknown extensions.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13820

- Allow Erlang/OTP to use OpenSSL in FIPS-140 mode, in order to satisfy specific
  security requirements (mostly by different parts of the US federal
  government).

  See the new crypto users guide "FIPS mode" chapter about building and using
  the FIPS support which is disabled by default.

  (Thanks to dszoboszlay and legoscia)

  Own Id: OTP-13921 Aux Id: PR-1180

- Implemented DTLS cookie generation, required by spec, instead of using a
  hardcoded value.

  Own Id: OTP-14076

- Implement sliding window replay protection of DTLS records.

  Own Id: OTP-14077

- TLS client processes will by default call public_key:pkix_verify_hostname/2 to
  verify the hostname of the connection with the server certificates specified
  hostname during certificate path validation. The user may explicitly disables
  it. Also if the hostname cannot be derived from the first argument to connect
  or is not supplied by the server name indication option, the check will not be
  performed.

  Own Id: OTP-14197

- Extend connection_information/\[1,2] . The values session_id, master_secret,
  client_random and server_random can no be accessed by
  connection_information/2. Note only session_id will be added to
  connection_information/1. The rational is that values concerning the
  connection security should have to be explicitly requested.

  Own Id: OTP-14291

- Chacha cipher suites are currently not tested enough to be most preferred ones

  Own Id: OTP-14382

- Basic support for DTLS that been tested together with OpenSSL.

  Test by providing the option \{protocol, dtls\} to the ssl API functions
  connect and listen.

  Own Id: OTP-14388

## SSL 8.1.3.1.1

### Fixed Bugs and Malfunctions

- Fix alert handling so that unexpected messages are logged and alerted
  correctly

  Own Id: OTP-14929

## SSL 8.1.3.1

### Fixed Bugs and Malfunctions

- An erlang TLS server configured with cipher suites using rsa key exchange, may
  be vulnerable to an Adaptive Chosen Ciphertext attack (AKA Bleichenbacher
  attack) against RSA, which when exploited, may result in plaintext recovery of
  encrypted messages and/or a Man-in-the-middle (MiTM) attack, despite the
  attacker not having gained access to the server’s private key itself.
  [CVE-2017-1000385](https://nvd.nist.gov/vuln/detail/CVE-2017-1000385)

  Exploiting this vulnerability to perform plaintext recovery of encrypted
  messages will, in most practical cases, allow an attacker to read the
  plaintext only after the session has completed. Only TLS sessions established
  using RSA key exchange are vulnerable to this attack.

  Exploiting this vulnerability to conduct a MiTM attack requires the attacker
  to complete the initial attack, which may require thousands of server
  requests, during the handshake phase of the targeted session within the window
  of the configured handshake timeout. This attack may be conducted against any
  TLS session using RSA signatures, but only if cipher suites using RSA key
  exchange are also enabled on the server. The limited window of opportunity,
  limitations in bandwidth, and latency make this attack significantly more
  difficult to execute.

  RSA key exchange is enabled by default although least prioritized if server
  order is honored. For such a cipher suite to be chosen it must also be
  supported by the client and probably the only shared cipher suite.

  Captured TLS sessions encrypted with ephemeral cipher suites (DHE or ECDHE)
  are not at risk for subsequent decryption due to this vulnerability.

  As a workaround if default cipher suite configuration was used you can
  configure the server to not use vulnerable suites with the ciphers option like
  this:

  `{ciphers, [Suite || Suite <- ssl:cipher_suites(), element(1,Suite) =/= rsa]}`

  that is your code will look somethingh like this:

  `ssl:listen(Port, [{ciphers, [Suite || Suite <- ssl:cipher_suites(), element(1,S) =/= rsa]} | Options]).`

  Thanks to Hanno Böck, Juraj Somorovsky and Craig Young for reporting this
  vulnerability.

  Own Id: OTP-14748

## SSL 8.1.3

### Fixed Bugs and Malfunctions

- Remove debug printout

  Own Id: OTP-14396

## SSL 8.1.2

### Fixed Bugs and Malfunctions

- Correct active once emulation, for TLS. Now all data received by the
  connection process will be delivered through active once, even when the active
  once arrives after that the gen_tcp socket is closed by the peer.

  Own Id: OTP-14300

## SSL 8.1.1

### Fixed Bugs and Malfunctions

- Corrected termination behavior, that caused a PEM cache bug and sometimes
  resulted in connection failures.

  Own Id: OTP-14100

- Fix bug that could hang ssl connection processes when failing to require more
  data for very large handshake packages. Add option max_handshake_size to
  mitigate DoS attacks.

  Own Id: OTP-14138

- Improved support for CRL handling that could fail to work as intended when an
  id-ce-extKeyUsage was present in the certificate. Also improvements where
  needed to distributionpoint handling so that all revocations actually are
  found and not deemed to be not determinable.

  Own Id: OTP-14141

- A TLS handshake might accidentally match old sslv2 format and ssl application
  would incorrectly aborted TLS handshake with ssl_v2_client_hello_no_supported.
  Parsing was altered to avoid this problem.

  Own Id: OTP-14222

- Correct default cipher list to prefer AES 128 before 3DES

  Own Id: OTP-14235

### Improvements and New Features

- Move PEM cache to a dedicated process, to avoid making the SSL manager process
  a bottleneck. This improves scalability of TLS connections.

  Own Id: OTP-13874

## SSL 8.1

### Fixed Bugs and Malfunctions

- List of possible anonymous suites, never supported by default, where incorrect
  for some TLS versions.

  Own Id: OTP-13926

### Improvements and New Features

- Experimental version of DTLS. It is runnable but not complete and cannot be
  considered reliable for production usage.

  Own Id: OTP-12982

- Add API options to handle ECC curve selection.

  Own Id: OTP-13959

## SSL 8.0.3

### Fixed Bugs and Malfunctions

- A timing related bug in event handling could cause interoperability problems
  between an erlang TLS server and some TLS clients, especially noticed with
  Firefox as TLS client.

  Own Id: OTP-13917

- Correct ECC curve selection, the error could cause the default to always be
  selected.

  Own Id: OTP-13918

## SSL 8.0.2

### Fixed Bugs and Malfunctions

- Correctly formed handshake messages received out of order will now correctly
  fail the connection with unexpected message.

  Own Id: OTP-13853

- Correct handling of signature algorithm selection

  Own Id: OTP-13711

### Improvements and New Features

- ssl application now behaves gracefully also on partially incorrect input from
  peer.

  Own Id: OTP-13834

- Add application environment configuration bypass_pem_cache. This can be used
  as a workaround for the current implementation of the PEM-cache that has
  proven to be a bottleneck.

  Own Id: OTP-13883

## SSL 8.0.1

### Fixed Bugs and Malfunctions

- The TLS/SSL protocol version selection for the SSL server has been corrected
  to follow RFC 5246 Appendix E.1 especially in case where the list of supported
  versions has gaps. Now the server selects the highest protocol version it
  supports that is not higher than what the client supports.

  Own Id: OTP-13753 Aux Id: seq13150

## SSL 8.0

### Fixed Bugs and Malfunctions

- Server now rejects, a not requested client cert, as an incorrect handshake
  message and ends the connection.

  Own Id: OTP-13651

### Improvements and New Features

- Remove default support for DES cipher suites

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13195

- Deprecate the function `crypto:rand_bytes` and make sure that
  `crypto:strong_rand_bytes` is used in all places that are cryptographically
  significant.

  Own Id: OTP-13214

- Better error handling of user error during TLS upgrade. ERL-69 is solved by
  gen_statem rewrite of ssl application.

  Own Id: OTP-13255

- Provide user friendly error message when crypto rejects a key

  Own Id: OTP-13256

- Add ssl:getstat/1 and ssl:getstat/2

  Own Id: OTP-13415

- TLS distribution connections now allow specifying the options `verify_fun`,
  `crl_check` and `crl_cache`. See the documentation. GitHub pull req #956
  contributed by Magnus Henoch.

  Own Id: OTP-13429 Aux Id: Pull#956

- Remove confusing error message when closing a distributed erlang node running
  over TLS

  Own Id: OTP-13431

- Remove default support for use of md5 in TLS 1.2 signature algorithms

  Own Id: OTP-13463

- ssl now uses gen_statem instead of gen_fsm to implement the ssl connection
  process, this solves some timing issues in addition to making the code more
  intuitive as the behaviour can be used cleanly instead of having a lot of
  workaround for shortcomings of the behaviour.

  Own Id: OTP-13464

- Phase out interoperability with clients that offer SSLv2. By default they are
  no longer supported, but an option to provide interoperability is offered.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13465

- OpenSSL has functions to generate short (eight hex digits) hashes of issuers
  of certificates and CRLs. These hashes are used by the "c_rehash" script to
  populate directories of CA certificates and CRLs, e.g. in the Apache web
  server. Add functionality to let an Erlang program find the right CRL for a
  given certificate in such a directory.

  Own Id: OTP-13530

- Some legacy TLS 1.0 software does not tolerate the 1/n-1 content split BEAST
  mitigation technique. Add a beast_mitigation SSL option (defaulting to
  one_n_minus_one) to select or disable the BEAST mitigation technique.

  Own Id: OTP-13629

- Enhance error log messages to facilitate for users to understand the error

  Own Id: OTP-13632

- Increased default DH params to 2048-bit

  Own Id: OTP-13636

- Propagate CRL unknown CA error so that public_key validation process continues
  correctly and determines what should happen.

  Own Id: OTP-13656

- Introduce a flight concept for handshake packages. This is a preparation for
  enabling DTLS, however it can also have a positive effects for TLS on slow and
  unreliable networks.

  Own Id: OTP-13678

## SSL 7.3.3.2

### Fixed Bugs and Malfunctions

- An erlang TLS server configured with cipher suites using rsa key exchange, may
  be vulnerable to an Adaptive Chosen Ciphertext attack (AKA Bleichenbacher
  attack) against RSA, which when exploited, may result in plaintext recovery of
  encrypted messages and/or a Man-in-the-middle (MiTM) attack, despite the
  attacker not having gained access to the server’s private key itself.
  [CVE-2017-1000385](https://nvd.nist.gov/vuln/detail/CVE-2017-1000385)

  Exploiting this vulnerability to perform plaintext recovery of encrypted
  messages will, in most practical cases, allow an attacker to read the
  plaintext only after the session has completed. Only TLS sessions established
  using RSA key exchange are vulnerable to this attack.

  Exploiting this vulnerability to conduct a MiTM attack requires the attacker
  to complete the initial attack, which may require thousands of server
  requests, during the handshake phase of the targeted session within the window
  of the configured handshake timeout. This attack may be conducted against any
  TLS session using RSA signatures, but only if cipher suites using RSA key
  exchange are also enabled on the server. The limited window of opportunity,
  limitations in bandwidth, and latency make this attack significantly more
  difficult to execute.

  RSA key exchange is enabled by default although least prioritized if server
  order is honored. For such a cipher suite to be chosen it must also be
  supported by the client and probably the only shared cipher suite.

  Captured TLS sessions encrypted with ephemeral cipher suites (DHE or ECDHE)
  are not at risk for subsequent decryption due to this vulnerability.

  As a workaround if default cipher suite configuration was used you can
  configure the server to not use vulnerable suites with the ciphers option like
  this:

  `{ciphers, [Suite || Suite <- ssl:cipher_suites(), element(1,Suite) =/= rsa]}`

  that is your code will look somethingh like this:

  `ssl:listen(Port, [{ciphers, [Suite || Suite <- ssl:cipher_suites(), element(1,S) =/= rsa]} | Options]).`

  Thanks to Hanno Böck, Juraj Somorovsky and Craig Young for reporting this
  vulnerability.

  Own Id: OTP-14748

## SSL 7.3.3

### Fixed Bugs and Malfunctions

- Correct ssl:prf/5 to use the negotiated cipher suite's prf function in
  ssl:prf/5 instead of the default prf.

  Own Id: OTP-13546

- Timeouts may have the value 0, guards have been corrected to allow this

  Own Id: OTP-13635

- Change of internal handling of hash sign pairs as the used one enforced to
  much restrictions making some valid combinations unavailable.

  Own Id: OTP-13670

## SSL 7.3.3.0.1

### Fixed Bugs and Malfunctions

- An erlang TLS server configured with cipher suites using rsa key exchange, may
  be vulnerable to an Adaptive Chosen Ciphertext attack (AKA Bleichenbacher
  attack) against RSA, which when exploited, may result in plaintext recovery of
  encrypted messages and/or a Man-in-the-middle (MiTM) attack, despite the
  attacker not having gained access to the server’s private key itself.
  [CVE-2017-1000385](https://nvd.nist.gov/vuln/detail/CVE-2017-1000385)

  Exploiting this vulnerability to perform plaintext recovery of encrypted
  messages will, in most practical cases, allow an attacker to read the
  plaintext only after the session has completed. Only TLS sessions established
  using RSA key exchange are vulnerable to this attack.

  Exploiting this vulnerability to conduct a MiTM attack requires the attacker
  to complete the initial attack, which may require thousands of server
  requests, during the handshake phase of the targeted session within the window
  of the configured handshake timeout. This attack may be conducted against any
  TLS session using RSA signatures, but only if cipher suites using RSA key
  exchange are also enabled on the server. The limited window of opportunity,
  limitations in bandwidth, and latency make this attack significantly more
  difficult to execute.

  RSA key exchange is enabled by default although least prioritized if server
  order is honored. For such a cipher suite to be chosen it must also be
  supported by the client and probably the only shared cipher suite.

  Captured TLS sessions encrypted with ephemeral cipher suites (DHE or ECDHE)
  are not at risk for subsequent decryption due to this vulnerability.

  As a workaround if default cipher suite configuration was used you can
  configure the server to not use vulnerable suites with the ciphers option like
  this:

  `{ciphers, [Suite || Suite <- ssl:cipher_suites(), element(1,Suite) =/= rsa]}`

  that is your code will look somethingh like this:

  `ssl:listen(Port, [{ciphers, [Suite || Suite <- ssl:cipher_suites(), element(1,S) =/= rsa]} | Options]).`

  Thanks to Hanno Böck, Juraj Somorovsky and Craig Young for reporting this
  vulnerability.

  Own Id: OTP-14748

### Improvements and New Features

- Create a little randomness in sending of session invalidation messages, to
  mitigate load when whole table is invalidated.

  Own Id: OTP-13490

## SSL 7.3.2

### Fixed Bugs and Malfunctions

- Correct cipher suites conversion and guard expression. Caused problems with
  GCM cipher suites and client side option to set signature_algorithms extension
  values.

  Own Id: OTP-13525

## SSL 7.3.1

### Fixed Bugs and Malfunctions

- Corrections to cipher suite handling using the 3 and 4 tuple format in
  addition to commit 89d7e21cf4ae988c57c8ef047bfe85127875c70c

  Own Id: OTP-13511

### Improvements and New Features

- Make values for the TLS-1.2 signature_algorithms extension configurable

  Own Id: OTP-13261

## SSL 7.3

### Fixed Bugs and Malfunctions

- Make sure there is only one poller validator at a time for validating the
  session cache.

  Own Id: OTP-13185

- A timing related issue could cause ssl to hang, especially happened with newer
  versions of OpenSSL in combination with ECC ciphers.

  Own Id: OTP-13253

- Work around a race condition in the TLS distribution start.

  Own Id: OTP-13268

- Big handshake messages are now correctly fragmented in the TLS record layer.

  Own Id: OTP-13306

- Improve portability of ECC tests in Crypto and SSL for "exotic" OpenSSL
  versions.

  Own Id: OTP-13311

- Certificate extensions marked as critical are ignored when using verify_none

  Own Id: OTP-13377

- If a certificate doesn't contain a CRL Distribution Points extension, and the
  relevant CRL is not in the cache, and the `crl_check` option is not set to
  `best_effort` , the revocation check should fail.

  Own Id: OTP-13378

- Enable TLS distribution over IPv6

  Own Id: OTP-13391

### Improvements and New Features

- Improve error reporting for TLS distribution

  Own Id: OTP-13219

- Include options from connect, listen and accept in
  `connection_information/1,2`

  Own Id: OTP-13232

- Allow adding extra options for outgoing TLS distribution connections, as
  supported for plain TCP connections.

  Own Id: OTP-13285

- Use loopback as server option in TLS-distribution module

  Own Id: OTP-13300

- Verify certificate signature against original certificate binary.

  This avoids bugs due to encoding errors when re-encoding a decode certificate.
  As there exists several decode step and using of different ASN.1 specification
  this is a risk worth avoiding.

  Own Id: OTP-13334

- Use `application:ensure_all_started/2` instead of hard-coding dependencies

  Own Id: OTP-13363

## SSL 7.2

### Fixed Bugs and Malfunctions

- Honor distribution port range options

  Own Id: OTP-12838

- Correct supervisor specification in TLS distribution.

  Own Id: OTP-13134

- Correct cache timeout

  Own Id: OTP-13141

- Avoid crash and restart of ssl process when key file does not exist.

  Own Id: OTP-13144

- Enable passing of raw socket options on the format \{raw,_,_,\_\} to the
  underlying socket.

  Own Id: OTP-13166

- Hibernation with small or a zero timeout will now work as expected

  Own Id: OTP-13189

### Improvements and New Features

- Add upper limit for session cache, configurable on ssl application level.

  If upper limit is reached, invalidate the current cache entries, e.i the
  session lifetime is the max time a session will be kept, but it may be
  invalidated earlier if the max limit for the table is reached. This will keep
  the ssl manager process well behaved, not exhusting memory. Invalidating the
  entries will incrementally empty the cache to make room for fresh sessions
  entries.

  Own Id: OTP-12392

- Use new time functions to measure passed time.

  Own Id: OTP-12457

- Improved error handling in TLS distribution

  Own Id: OTP-13142

- Distribution over TLS now honors the nodelay distribution flag

  Own Id: OTP-13143

## SSL 7.1

### Fixed Bugs and Malfunctions

- Add DER encoded ECPrivateKey as valid input format for key option.

  Own Id: OTP-12974

- Correct return value of default session callback module

  This error had the symptom that the client check for unique session would
  always fail, potentially making the client session table grow a lot and
  causing long setup times.

  Own Id: OTP-12980

### Improvements and New Features

- Add possibility to downgrade an SSL/TLS connection to a tcp connection, and
  give back the socket control to a user process.

  This also adds the possibility to specify a timeout to the ssl:close function.

  Own Id: OTP-11397

- Add application setting to be able to change fatal alert shutdown timeout,
  also shorten the default timeout. The fatal alert timeout is the number of
  milliseconds between sending of a fatal alert and closing the connection.
  Waiting a little while improves the peers chances to properly receiving the
  alert so it may shutdown gracefully.

  Own Id: OTP-12832

## SSL 7.0

### Fixed Bugs and Malfunctions

- Ignore signature_algorithm (TLS 1.2 extension) sent to TLS 1.0 or TLS 1.1
  server

  Own Id: OTP-12670

- Improve error handling in TLS distribution module to avoid lingering sockets.

  Own Id: OTP-12799 Aux Id: Tom Briden

- Add option \{client_renegotiation, boolean()\} option to the server-side of
  the SSL application.

  Own Id: OTP-12815

### Improvements and New Features

- Add new API functions to handle CRL-verification

  Own Id: OTP-10362 Aux Id: kunagi-215 \[126]

- Remove default support for SSL-3.0, due to Poodle vunrability in protocol
  specification.

  Add padding check for TLS-1.0 to remove Poodle vunrability from TLS 1.0, also
  add the option padding_check. This option only affects TLS-1.0 connections and
  if set to false it disables the block cipher padding check to be able to
  interoperate with legacy software.

  Remove default support for RC4 cipher suites, as they are consider too weak.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12390

- Add support for TLS ALPN (Application-Layer Protocol Negotiation) extension.

  Own Id: OTP-12580

- Add SNI (Server Name Indication) support for the server side.

  Own Id: OTP-12736

## SSL 6.0.1.1

### Fixed Bugs and Malfunctions

- Gracefully ignore proprietary hash_sign algorithms

  Own Id: OTP-12829

## SSL 6.0.1

### Fixed Bugs and Malfunctions

- Terminate gracefully when receiving bad input to premaster secret calculation

  Own Id: OTP-12783

## SSL 6.0

### Fixed Bugs and Malfunctions

- Exclude self-signed trusted anchor certificates from certificate prospective
  certification path according to RFC 3280.

  This will avoid some unnecessary certificate processing.

  Own Id: OTP-12449

### Improvements and New Features

- Separate client and server session cache internally.

  Avoid session table growth when client starts many connections in such a
  manner that many connections are started before session reuse is possible.
  Only save a new session in client if there is no equivalent session already
  stored.

  Own Id: OTP-11365

- The PEM cache is now validated by a background process, instead of always
  keeping it if it is small enough and clearing it otherwise. That strategy
  required that small caches where cleared by API function if a file changes on
  disk.

  However export the API function to clear the cache as it may still be useful.

  Own Id: OTP-12391

- Add padding check for TLS-1.0 to remove Poodle vulnerability from TLS 1.0,
  also add the option padding_check. This option only affects TLS-1.0
  connections and if set to false it disables the block cipher padding check to
  be able to interoperate with legacy software.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12420

- Add support for TLS_FALLBACK_SCSV used to prevent undesired TLS version
  downgrades. If used by a client that is vulnerable to the POODLE attack, and
  the server also supports TLS_FALLBACK_SCSV, the attack can be prevented.

  Own Id: OTP-12458

## SSL 5.3.8

### Fixed Bugs and Malfunctions

- Make sure the clean rule for ssh, ssl, eunit and otp_mibs actually removes
  generated files.

  Own Id: OTP-12200

### Improvements and New Features

- Change code to reflect that state data may be secret to avoid breaking
  dialyzer contracts.

  Own Id: OTP-12341

## SSL 5.3.7

### Fixed Bugs and Malfunctions

- Handle the fact that servers may send an empty SNI extension to the client.

  Own Id: OTP-12198

## SSL 5.3.6

### Fixed Bugs and Malfunctions

- Corrected handling of ECC certificates, there where several small issues with
  the handling of such certificates in the ssl and public_key application. Now
  ECC signed ECC certificates shall work and not only RSA signed ECC
  certificates.

  Own Id: OTP-12026

- Check that the certificate chain ends with a trusted ROOT CA e.i. a
  self-signed certificate, but provide an option partial_chain to enable the
  application to define an intermediat CA as trusted.

  Own Id: OTP-12149

### Improvements and New Features

- Add decode functions for SNI (Server Name Indication)

  Own Id: OTP-12048

## SSL 5.3.5

### Fixed Bugs and Malfunctions

- ssl:recv now returns \{error, einval\} if applied to a non passive socket, the
  same as gen_tcp:recv.

  Thanks to Danil Zagoskin for reporting this issue

  Own Id: OTP-11878

- Corrected handling of default values for signature_algorithms extension in
  TLS-1.2 and corresponding values used in previous versions that does not
  support this extension.

  Thanks to Danil Zagoskin

  Own Id: OTP-11886

- Handle socket option inheritance when pooling of accept sockets is used

  Own Id: OTP-11897

- Make sure that the list of versions, possibly supplied in the versions option,
  is not order dependent.

  Thanks to Ransom Richardson for reporting this issue

  Own Id: OTP-11912

- Reject connection if the next_protocol message is sent twice.

  Own Id: OTP-11926

- Correct options handling when ssl:ssl_accept/3 is called with new ssl options
  after calling ssl:listen/2

  Own Id: OTP-11950

### Improvements and New Features

- Gracefully handle unknown alerts

  Thanks to Atul Atri for reporting this issue

  Own Id: OTP-11874

- Gracefully ignore cipher suites sent by client not supported by the SSL/TLS
  version that the client has negotiated.

  Thanks to Danil Zagoskin for reporting this issue

  Own Id: OTP-11875

- Gracefully handle structured garbage, i.e a client sends some garbage in a ssl
  record instead of a valid fragment.

  Thanks to Danil Zagoskin

  Own Id: OTP-11880

- Gracefully handle invalid alerts

  Own Id: OTP-11890

- Generalize handling of default ciphers

  Thanks to Andreas Schultz

  Own Id: OTP-11966

- Make sure change cipher spec is correctly handled

  Own Id: OTP-11975

## SSL 5.3.4

### Fixed Bugs and Malfunctions

- Fix incorrect dialyzer spec and types, also enhance documentation.

  Thanks to Ayaz Tuncer.

  Own Id: OTP-11627

- Fix possible mismatch between SSL/TLS version and default ciphers. Could
  happen when you specified SSL/TLS-version in optionlist to listen or accept.

  Own Id: OTP-11712

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

- Unicode adaptations

  Own Id: OTP-11620

- Added option honor_cipher_order. This instructs the server to prefer its own
  cipher ordering rather than the client's and can help protect against things
  like BEAST while maintaining compatibility with clients which only support
  older ciphers.

  Thanks to Andrew Thompson for the implementation, and Andreas Schultz for the
  test cases.

  Own Id: OTP-11621

- Replace boolean checking in validate_option with is_boolean guard.

  Thanks to Andreas Schultz.

  Own Id: OTP-11634

- Some function specs are corrected or moved and some edoc comments are
  corrected in order to allow use of edoc. (Thanks to Pierre Fenoll)

  Own Id: OTP-11702

- Correct clean up of certificate database when certs are inputted in pure DER
  format.The incorrect code could cause a memory leek when certs where inputted
  in DER. Thanks to Bernard Duggan for reporting this.

  Own Id: OTP-11733

- Improved documentation of the cacertfile option

  Own Id: OTP-11759 Aux Id: seq12535

- Avoid next protocol negotiation failure due to incorrect option format.

  Own Id: OTP-11760

- Handle v1 CRLs, with no extensions and fixes issues with IDP (Issuing
  Distribution Point) comparison during CRL validation.

  Thanks to Andrew Thompson

  Own Id: OTP-11761

- Server now ignores client ECC curves that it does not support instead of
  crashing.

  Thanks to Danil Zagoskin for reporting the issue and suggesting a solution.

  Own Id: OTP-11780

- Handle SNI (Server Name Indication) alert unrecognized_name and gracefully
  deal with unexpected alerts.

  Thanks to Masatake Daimon for reporting this.

  Own Id: OTP-11815

- Add possibility to specify ssl options when calling ssl:ssl_accept

  Own Id: OTP-11837

## SSL 5.3.3

### Fixed Bugs and Malfunctions

- Add missing validation of the server_name_indication option and test for its
  explicit use. It was not possible to set or disable the default
  server_name_indication as the validation of the option was missing.

  Own Id: OTP-11567

- Elliptic curve selection in server mode now properly selects a curve suggested
  by the client, if possible, and the fallback alternative is changed to a more
  widely supported curve.

  Own Id: OTP-11575

- Bug in the TLS hello extension handling caused the server to behave as it did
  not understand secure renegotiation.

  Own Id: OTP-11595

## SSL 5.3.2

### Fixed Bugs and Malfunctions

- Honors the clients advertised support of elliptic curves and no longer sends
  incorrect elliptic curve extension in server hello.

  Own Id: OTP-11370

- Fix initialization of DTLS fragment reassembler, in previously contributed
  code, for future support of DTLS . Thanks to Andreas Schultz.

  Own Id: OTP-11376

- Corrected type error in client_preferred_next_protocols documentation. Thanks
  to Julien Barbot.

  Own Id: OTP-11457

### Improvements and New Features

- TLS code has been refactored to prepare for future DTLS support. Also some
  DTLS code is in place but not yet runnable, some of it contributed by Andreas
  Schultz and some of it written by the OTP team. Thanks to to Andreas for his
  participation.

  Own Id: OTP-11292

- Remove extraneous dev debug code left in the close function. Thanks to Ken
  Key.

  Own Id: OTP-11447

- Add SSL Server Name Indication (SNI) client support. Thanks to Julien Barbot.

  Own Id: OTP-11460

## SSL 5.3.1

### Fixed Bugs and Malfunctions

- Setopts during renegotiation caused the renegotiation to be unsuccessful.

  If calling setopts during a renegotiation the FSM state might change during
  the handling of the setopts messages, this is now handled correctly.

  Own Id: OTP-11228

- Now handles signature_algorithm field in digitally_signed properly with proper
  defaults. Prior to this change some elliptic curve cipher suites could fail
  reporting the error "bad certificate".

  Own Id: OTP-11229

- The code emulating the inet header option was changed in the belief that it
  made it inet compatible. However the testing is a bit hairy as the inet option
  is actually broken, now the tests are corrected and the header option should
  work in the same broken way as inet again, preferably use the bitsyntax
  instead.

  Own Id: OTP-11230

### Improvements and New Features

- Make the ssl manager name for erlang distribution over SSL/TLS relative to the
  module name of the ssl_manager.

  This can be beneficial when making tools that rename modules for internal
  processing in the tool.

  Own Id: OTP-11255

- Add documentation regarding log_alert option.

  Own Id: OTP-11271

## SSL 5.3

### Fixed Bugs and Malfunctions

- Honor the versions option to ssl:connect and ssl:listen.

  Own Id: OTP-10905

- Next protocol negotiation with reused sessions will now succeed

  Own Id: OTP-10909

### Improvements and New Features

- Add support for PSK (Pre Shared Key) and SRP (Secure Remote Password) cipher
  suites, thanks to Andreas Schultz.

  Own Id: OTP-10450 Aux Id: kunagi-269 \[180]

- Fix SSL Next Protocol Negotiation documentation. Thanks to Julien Barbot.

  Own Id: OTP-10955

- Fix ssl_connection to support reading proxy/chain certificates. Thanks to
  Valentin Kuznetsov.

  Own Id: OTP-10980

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

## SSL 5.2.1

### Improvements and New Features

- Transport callback handling is changed so that gen_tcp is treated as a special
  case where inet will be called directly for functions such as setopts, as
  gen_tcp does not have its own setopts. This will enable users to use the
  transport callback for other customizations such as websockets.

  Own Id: OTP-10847

- Follow up to OTP-10451 solved in ssl-5.2 R16A. Make sure format_error return
  good strings. Replace confusing legacy atoms with more descriptive atoms.

  Own Id: OTP-10864

## SSL 5.1.2.1

### Improvements and New Features

- Make log_alert configurable as option in ssl, SSLLogLevel added as option to
  inets conf file

  Own Id: OTP-11259

## SSL 5.2

### Fixed Bugs and Malfunctions

- SSL: TLS 1.2, advertise sha224 support, thanks to Andreas Schultz.

  Own Id: OTP-10586

- If an ssl server is restarted with new options and a client tries to reuse a
  session the server must make sure that it complies to the new options before
  agreeing to reuse it.

  Own Id: OTP-10595

- Now handles cleaning of CA-certificate database correctly so that there will
  be no memory leek, bug was introduced in ssl- 5.1 when changing implementation
  to increase parallel execution.

  Impact: Improved memory usage, especially if you have many different
  certificates and upgrade tcp-connections to TLS-connections.

  Own Id: OTP-10710

### Improvements and New Features

- Support Next Protocol Negotiation in TLS, thanks to Ben Murphy for the
  contribution.

  Impact: Could give performance benefit if used as it saves a round trip.

  Own Id: OTP-10361 Aux Id: kunagi-214 \[125]

- TLS 1.2 will now be the default TLS version if sufficient crypto support is
  available otherwise TLS 1.1 will be default.

  Impact: A default TLS connection will have higher security and hence it may be
  perceived as slower then before.

  Own Id: OTP-10425 Aux Id: kunagi-275 \[186]

- It is now possible to call controlling_process on a listen socket, same as in
  gen_tcp.

  Own Id: OTP-10447

- Remove filter mechanisms that made error messages backwards compatible with
  old ssl but hid information about what actually happened.

  This does not break the documented API however other reason terms may be
  returned, so code that matches on the reason part of \{error, Reason\} may
  fail.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-10451 Aux Id: kunagi-270 \[181]

- Added missing dependencies to Makefile

  Own Id: OTP-10594

- Removed deprecated function ssl:pid/0, it has been pointless since R14 but has
  been keep for backwards compatibility.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-10613 Aux Id: kunagi-331 \[242]

- Refactor to simplify addition of key exchange methods, thanks to Andreas
  Schultz.

  Own Id: OTP-10709

## SSL 5.1.2

### Fixed Bugs and Malfunctions

- ssl:ssl_accept/2 timeout is no longer ignored

  Own Id: OTP-10600

## SSL 5.1.1

### Fixed Bugs and Malfunctions

- ssl:recv/3 could "loose" data when the timeout occurs. If the timeout in
  ssl:connect or ssl:ssl_accept expired the ssl connection process was not
  terminated as it should, this due to gen_fsm:send_all_state_event timeout is a
  client side time out. These timouts are now handled by the gen_fsm-procss
  instead.

  Own Id: OTP-10569

### Improvements and New Features

- Better termination handling that avoids hanging.

  Own Id: OTP-10574

## SSL 5.1

### Fixed Bugs and Malfunctions

- Sometimes the client process could receive an extra \{error, closed\} message
  after ssl:recv had returned \{error, closed\}.

  Own Id: OTP-10118

- ssl v3 alert number 41 (no_certificate_RESERVED) is now recognized

  Own Id: OTP-10196

### Improvements and New Features

- Experimental support for TLS 1.1 is now available, will be officially
  supported from OTP-R16. Thanks to Andreas Schultz for implementing the first
  version.

  Own Id: OTP-8871

- Experimental support for TLS 1.2 is now available, will be officially
  supported from OTP-R16. Thanks to Andreas Schultz for implementing the first
  version.

  Own Id: OTP-8872

- Removed some bottlenecks increasing the applications parallelism especially
  for the client side.

  Own Id: OTP-10113

- Workaround for handling certificates that wrongly encode X509countryname in
  utf-8 when the actual value is a valid ASCCI value of length 2. Such
  certificates are accepted by many browsers such as Chrome and Fierfox so for
  interoperability reasons we will too.

  Own Id: OTP-10222

## SSL 5.0.1

### Fixed Bugs and Malfunctions

- Robustness and improvement to distribution over SSL

  Fix a bug where ssl_tls_dist_proxy would crash at caller timeout. Fix a bug
  where a timeout from the SSL layer would block the distribution indefinitely.
  Run the proxy exclusively on the loopback interface. (Thanks to Paul Guyot)

  Own Id: OTP-9915

- Fix setup loop of SSL TLS dist proxy

  Fix potential leak of processes waiting indefinitely for data from closed
  sockets during socket setup phase. (Thanks to Paul Guyot)

  Own Id: OTP-9916

- Correct spelling of registered (Thanks to Richard Carlsson)

  Own Id: OTP-9925

- Added TLS PRF function to the SSL API for generation of additional key
  material from a TLS session. (Thanks to Andreas Schultz)

  Own Id: OTP-10024

## SSL 5.0

### Fixed Bugs and Malfunctions

- Invalidation handling of sessions could cause the time_stamp field in the
  session record to be set to undefined crashing the session clean up process.
  This did not affect the connections but would result in that the session table
  would grow.

  Own Id: OTP-9696 Aux Id: seq11947

- Changed code to use ets:foldl and throw instead of ets:next traversal,
  avoiding the need to explicitly call ets:safe_fixtable. It was possible to get
  a badarg-crash under special circumstances.

  Own Id: OTP-9703 Aux Id: seq11947

- Send ssl_closed notification to active ssl user when a tcp error occurs.

  Own Id: OTP-9734 Aux Id: seq11946

- If a passive receive was ongoing during a renegotiation the process evaluating
  ssl:recv could be left hanging for ever.

  Own Id: OTP-9744

### Improvements and New Features

- Support for the old ssl implementation is dropped and the code is removed.

  Own Id: OTP-7048

- The erlang distribution can now be run over the new ssl implementation. All
  options can currently not be set but it is enough to replace to old ssl
  implementation.

  Own Id: OTP-7053

- public_key, ssl and crypto now supports PKCS-8

  Own Id: OTP-9312

- Implements a CBC timing attack counter measure. Thanks to Andreas Schultz for
  providing the patch.

  Own Id: OTP-9683

- Mitigates an SSL/TLS Computational DoS attack by disallowing the client to
  renegotiate many times in a row in a short time interval, thanks to Tuncer
  Ayaz for alerting us about this.

  Own Id: OTP-9739

- Implements the 1/n-1 splitting countermeasure to the Rizzo Duong BEAST attack,
  affects SSL 3.0 and TLS 1.0. Thanks to Tuncer Ayaz for alerting us about this.

  Own Id: OTP-9750

## SSL 4.1.6

### Fixed Bugs and Malfunctions

- replace "a ssl" with "an ssl" reindent pkix_path_validation/3 Trivial
  documentation fixes (Thanks to Christian von Roques )

  Own Id: OTP-9464

### Improvements and New Features

- Adds function clause to avoid denial of service attack. Thanks to Vinod for
  reporting this vulnerability.

  Own Id: OTP-9364

- Error handling code now takes care of inet:getopts/2 and inets:setopts/2
  crashes. Thanks to Richard Jones for reporting this.

  Own Id: OTP-9382

- Support explicit use of packet option httph and httph_bin

  Own Id: OTP-9461

- Decoding of hello extensions could fail to come to the correct conclusion due
  to an error in a binary match pattern. Thanks to Ben Murphy.

  Own Id: OTP-9589

## SSL 4.1.5

### Improvements and New Features

- Calling gen_tcp:connect with option \{ip, \{127,0,0,1\}\} results in an exit
  with reason badarg. Neither SSL nor INETS This was not caught, resulting in
  crashes with incomprehensible reasons.

  Own Id: OTP-9289 Aux Id: seq11845

## SSL 4.1.3

### Fixed Bugs and Malfunctions

- Fixed error in cache-handling fix from ssl-4.1.2

  Own Id: OTP-9018 Aux Id: seq11739

- Verification of a critical extended_key_usage-extension corrected

  Own Id: OTP-9029 Aux Id: seq11541

## SSL 4.1.2

### Fixed Bugs and Malfunctions

- The ssl application caches certificate files, it will now invalidate cache
  entries if the diskfile is changed.

  Own Id: OTP-8965 Aux Id: seq11739

- Now runs the terminate function before returning from the call made by
  ssl:close/1, as before the caller of ssl:close/1 could get problems with the
  reuseaddr option.

  Own Id: OTP-8992

## SSL 4.1.1

### Fixed Bugs and Malfunctions

- Correct handling of client certificate verify message When checking the client
  certificate verify message the server used the wrong algorithm identifier to
  determine the signing algorithm, causing a function clause error in the
  public_key application when the key-exchange algorithm and the public key
  algorithm of the client certificate happen to differ.

  Own Id: OTP-8897

### Improvements and New Features

- For testing purposes ssl now also support some anonymous cipher suites when
  explicitly configured to do so.

  Own Id: OTP-8870

- Sends an error alert instead of crashing if a crypto function for the selected
  cipher suite fails.

  Own Id: OTP-8930 Aux Id: seq11720

## SSL 4.1

### Improvements and New Features

- Updated ssl to ignore CA certs that violate the asn1-spec for a certificate,
  and updated public key asn1 spec to handle inherited DSS-params.

  Own Id: OTP-7884

- Changed ssl implementation to retain backwards compatibility for old option
  \{verify, 0\} that shall be equivalent to \{verify, verify_none\}, also
  separate the cases unknown ca and selfsigned peer cert, and restored return
  value of deprecated function public_key:pem_to_der/1.

  Own Id: OTP-8858

- Changed the verify fun so that it differentiate between the peer certificate
  and CA certificates by using valid_peer or valid as the second argument to the
  verify fun. It may not always be trivial or even possible to know when the
  peer certificate is reached otherwise.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8873

## SSL 4.0.1

### Fixed Bugs and Malfunctions

- The server now verifies the client certificate verify message correctly,
  instead of causing a case-clause.

  Own Id: OTP-8721

- The client hello message now always include ALL available cipher suites (or
  those specified by the ciphers option). Previous implementation would filter
  them based on the client certificate key usage extension (such filtering only
  makes sense for the server certificate).

  Own Id: OTP-8772

- Fixed handling of the option \{mode, list\} that was broken for some packet
  types for instance line.

  Own Id: OTP-8785

- Empty packets were not delivered to the client.

  Own Id: OTP-8790

- Building in a source tree without prebuilt platform independent build results
  failed on the SSL examples when:

  - cross building. This has been solved by not building the SSL examples during
    a cross build.
  - building on Windows.

  Own Id: OTP-8791

- Fixed a handshake error which occurred on some ssl implementations.

  Own Id: OTP-8793

### Improvements and New Features

- Revise the public_key API - Cleaned up and documented the public_key API to
  make it useful for general use, also changed ssl to use the new API.

  Own Id: OTP-8722

- Added support for inputing certificates and keys directly in DER format these
  options will override the pem-file options if specified.

  Own Id: OTP-8723

- To gain interoperability ssl will not check for padding errors when using TLS
  1.0. It is first in TLS 1.1 that checking the padding is an requirement.

  Own Id: OTP-8740

- Changed the semantics of the verify_fun option in the ssl-application so that
  it takes care of both application handling of path validation errors and
  verification of application specific extensions. This means that it is now
  possible for the server application in verify_peer mode to handle path
  validation errors. This change moved some functionality earlier in ssl to the
  public_key application.

  Own Id: OTP-8770

- Added the functionality so that the verification fun will be called when a
  certificate is considered valid by the path validation to allow access to each
  certificate in the path to the user application. Also try to verify
  subject-AltName, if unable to verify it let the application verify it.

  Own Id: OTP-8825

## SSL 4.0

### Improvements and New Features

- New ssl now support client/server-certificates signed by dsa keys.

  Own Id: OTP-8587

- Ssl has now switched default implementation and removed deprecated certificate
  handling. All certificate handling is done by the public_key application.

  Own Id: OTP-8695
