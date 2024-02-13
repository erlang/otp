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
# SSH

The ssh application implements the Secure Shell (SSH) protocol and provides an
SSH File Transfer Protocol (SFTP) client and server.

## Description

The `ssh` application is an implementation of the SSH protocol in Erlang. `ssh`
offers API functions to write customized SSH clients and servers as well as
making the Erlang shell available over SSH. An SFTP client, `ssh_sftp`, and
server, `ssh_sftpd`, are also included.

## DEPENDENCIES

The `ssh` application uses the applications `m:public_key` and `m:crypto` to
handle public keys and encryption. Hence, these applications must be loaded for
the `ssh` application to work. The call `ssh:start/0` will do the necessary
calls to [application:start/1,2](`application:start/1`) before it starts the
`ssh` itself.

## CONFIGURATION

The SSH application uses Configuration Parameters. Where to set them are
described in [config User's Guide](`e:kernel:config.md`) with SSH details in
[Configuration in SSH](configurations.md).

Some special configuration files from OpenSSH are also used:

- `known_hosts`
- `authorized_keys`
- `authorized_keys2`
- `id_dsa` _(supported but disabled by default)_
- `id_rsa` _(SHA1 sign/verify are supported but disabled by default from
  OTP-24)_
- `id_ecdsa`
- `id_ed25519`
- `id_ed448`
- `ssh_host_dsa_key` _(supported but disabled by default)_
- `ssh_host_rsa_key` _(SHA1 sign/verify are supported but disabled by default
  from OTP-24)_
- `ssh_host_ecdsa_key`
- `ssh_host_ed25519_key`
- `ssh_host_ed448_key`

By default, `ssh` looks for `id_*`, `known_hosts`, and `authorized_keys` in
`~/.ssh`, and for the ssh*host*\*\_key files in `/etc/ssh`. These locations can
be changed by the options [`user_dir`](`t:ssh_file:user_dir_common_option/0`)
and [`system_dir`](`t:ssh_file:system_dir_daemon_option/0`). More about where to
set them is described in [Configuration in SSH](configurations.md).

Public key handling can also be customized through a callback module that
implements the behaviors `m:ssh_client_key_api` and `m:ssh_server_key_api`.

See also the default callback module documentation in `m:ssh_file`.

Disabled public key algorithms can be enabled with the
[preferred_algorithms](`t:ssh:preferred_algorithms_common_option/0`) or
[modify_algorithms](`t:ssh:modify_algorithms_common_option/0`) options. See
[Example 9](configure_algos.md#example-9) in
[Configuring algorithms in SSH](configure_algos.md) for a description.

## Public Keys

`id_*` are the users private key files. Notice that the public key is part of
the private key so the `ssh` application does not use the `id_*.pub` files.
These are for the user's convenience when it is needed to convey the user's
public key.

See [ssh_file](`m:ssh_file#FILE-id_STAR`) for details.

## Known Hosts

The `known_hosts` file contains a list of approved servers and their public
keys. Once a server is listed, it can be verified without user interaction.

See [ssh_file](`m:ssh_file#FILE-known_hosts`) for details.

## Authorized Keys

The `authorized_key` file keeps track of the user's authorized public keys. The
most common use of this file is to let users log in without entering their
password, which is supported by the Erlang `ssh` daemon.

See [ssh_file](`m:ssh_file#FILE-authorized_keys`) for details.

## Host Keys

RSA, DSA (if enabled), ECDSA, ED25519 and ED448 host keys are supported and are
expected to be found in files named `ssh_host_rsa_key`, `ssh_host_dsa_key`,
`ssh_host_ecdsa_key`, `ssh_host_ed25519_key` and `ssh_host_ed448_key`.

See [ssh_file](`m:ssh_file#FILE-ssh_host_STAR_key`) for details.

## ERROR LOGGER AND EVENT HANDLERS

The `ssh` application uses the default [OTP error logger](`m:error_logger`) to
log unexpected errors or print information about special events.

[](){: #supported }

## SUPPORTED SPECIFICATIONS AND STANDARDS

The supported SSH version is 2.0.

## Algorithms

The actual set of algorithms may vary depending on which OpenSSL crypto library
that is installed on the machine. For the list on a particular installation, use
the command `ssh:default_algorithms/0`. The user may override the default
algorithm configuration both on the server side and the client side. See the
options [preferred_algorithms](`t:ssh:preferred_algorithms_common_option/0`) and
[modify_algorithms](`t:ssh:modify_algorithms_common_option/0`) in the
[ssh:daemon/1,2,3](`ssh:daemon/1`) and [ssh:connect/3,4](`ssh:connect/3`)
functions.

Supported algorithms are (in the default order):

[](){: #supported_algos }

- **Key exchange algorithms** - \* ecdh-sha2-nistp384

  - ecdh-sha2-nistp521
  - ecdh-sha2-nistp256
  - diffie-hellman-group-exchange-sha256
  - diffie-hellman-group16-sha512
  - diffie-hellman-group18-sha512
  - diffie-hellman-group14-sha256
  - curve25519-sha256
  - curve25519-sha256@libssh.org
  - curve448-sha512

  The following unsecure `SHA1` algorithms are now disabled by default:

  - (diffie-hellman-group14-sha1)
  - (diffie-hellman-group-exchange-sha1)
  - (diffie-hellman-group1-sha1)

  They can be enabled with the
  [preferred_algorithms](`t:ssh:preferred_algorithms_common_option/0`) or
  [modify_algorithms](`t:ssh:modify_algorithms_common_option/0`) options. Use
  for example the Option value
  `{modify_algorithms, [{append, [{kex,['diffie-hellman-group1-sha1']}]}]}`)

- **Public key algorithms** - \* ecdsa-sha2-nistp384

  - ecdsa-sha2-nistp521
  - ecdsa-sha2-nistp256
  - ssh-ed25519
  - ssh-ed448
  - rsa-sha2-256
  - rsa-sha2-512

  The following unsecure `SHA1` algorithms are supported but disabled by
  default:

  - (ssh-dss)
  - (ssh-rsa)

  See Disabled public key algorithms can be enabled with the
  [preferred_algorithms](`t:ssh:preferred_algorithms_common_option/0`) or
  [modify_algorithms](`t:ssh:modify_algorithms_common_option/0`) options. See
  [Example 9](configure_algos.md#example-9) in
  [Configuring algorithms in SSH](configure_algos.md) for a description.

- **MAC algorithms** - \* hmac-sha2-256-etm@openssh.com

  - hmac-sha2-512-etm@openssh.com
  - hmac-sha1-etm@openssh.com
  - hmac-sha2-256
  - hmac-sha2-512
  - hmac-sha1

  The following unsecure `SHA1` algorithm is disabled by default:

  - (hmac-sha1-96)

  It can be enabled with the
  [preferred_algorithms](`t:ssh:preferred_algorithms_common_option/0`) or
  [modify_algorithms](`t:ssh:modify_algorithms_common_option/0`) options. Use
  for example the Option value
  `{modify_algorithms, [{append, [{mac,['hmac-sha1-96']}]}]}`)

- **Encryption algorithms (ciphers)** - \* chacha20-poly1305@openssh.com

  - aes256-gcm@openssh.com
  - aes256-ctr
  - aes192-ctr
  - aes128-gcm@openssh.com
  - aes128-ctr
  - aes256-cbc
  - aes192-cbc
  - aes128-cbc
  - 3des-cbc
  - (AEAD_AES_128_GCM, not enabled per default)
  - (AEAD_AES_256_GCM, not enabled per default)

  See the text at the description of
  [the rfc 5647 further down](ssh_app.md#rfc5647_note) for more information
  regarding AEAD*AES*\*\_GCM.

  Following the internet de-facto standard, the cipher and mac algorithm
  AEAD_AES_128_GCM is selected when the cipher aes128-gcm@openssh.com is
  negotiated. The cipher and mac algorithm AEAD_AES_256_GCM is selected when the
  cipher aes256-gcm@openssh.com is negotiated.

- **Compression algorithms** - \* none
  - zlib@openssh.com
  - zlib

## Unicode support

Unicode filenames are supported if the emulator and the underlying OS support
it. See section DESCRIPTION in the `m:file` manual page in Kernel for
information about this subject.

The shell and the cli both support unicode.

## Rfcs

The following rfc:s are supported:

- [RFC 4251](https://tools.ietf.org/html/rfc4251), The Secure Shell (SSH)
  Protocol Architecture.

  Except

  - 9\.4.6 Host-Based Authentication
  - 9\.5.2 Proxy Forwarding
  - 9\.5.3 X11 Forwarding

- [RFC 4252](https://tools.ietf.org/html/rfc4252), The Secure Shell (SSH)
  Authentication Protocol.

  Except

  - 9\. Host-Based Authentication: "hostbased"

- [RFC 4253](https://tools.ietf.org/html/rfc4253), The Secure Shell (SSH)
  Transport Layer Protocol.

  Except

  - 8\.1. diffie-hellman-group1-sha1
  - 6\.6. Public Key Algorithms
    - ssh-dss
    - ssh-rsa

  They are disabled by default as they now are regarded insecure, but they can
  be enabled with the
  [preferred_algorithms](`t:ssh:preferred_algorithms_common_option/0`) or
  [modify_algorithms](`t:ssh:modify_algorithms_common_option/0`) options. See
  [Example 8](configure_algos.md#example-8) (diffie-hellman-group1-sha1) and
  [Example 9](configure_algos.md#example-9) (ssh-dss) in
  [Configuring algorithms in SSH](configure_algos.md) for descriptions.

- [RFC 4254](https://tools.ietf.org/html/rfc4254), The Secure Shell (SSH)
  Connection Protocol.

  Except

  - 6\.3. X11 Forwarding
  - 7\. TCP/IP Port Forwarding

- [RFC 4256](https://tools.ietf.org/html/rfc4256), Generic Message Exchange
  Authentication for the Secure Shell Protocol (SSH).

  Except

  - `num-prompts > 1`
  - password changing
  - other identification methods than userid-password

- [RFC 4419](https://tools.ietf.org/html/rfc4419), Diffie-Hellman Group Exchange
  for the Secure Shell (SSH) Transport Layer Protocol.

  Except

  - 4\.1. diffie-hellman-group-exchange-sha1

  It is disabled by defaultas as it now is regarded insecure, but it can be
  enabled with the
  [preferred_algorithms](`t:ssh:preferred_algorithms_common_option/0`) or
  [modify_algorithms](`t:ssh:modify_algorithms_common_option/0`) options.

- [RFC 4716](https://tools.ietf.org/html/rfc4716), The Secure Shell (SSH) Public
  Key File Format.
- [RFC 5647](https://tools.ietf.org/html/rfc5647), AES Galois Counter Mode for
  the Secure Shell Transport Layer Protocol.

  [](){: #rfc5647_note } There is an ambiguity in the synchronized selection of
  cipher and mac algorithm. This is resolved by OpenSSH in the ciphers
  aes128-gcm@openssh.com and aes256-gcm@openssh.com which are implemented. If
  the explicit ciphers and macs AEAD_AES_128_GCM or AEAD_AES_256_GCM are needed,
  they could be enabled with the options
  [preferred_algorithms](`t:ssh:preferred_algorithms_common_option/0`) or
  [modify_algorithms](`t:ssh:modify_algorithms_common_option/0`).

  > #### Warning {: .warning }
  >
  > If the client or the server is not Erlang/OTP, it is the users
  > responsibility to check that other implementation has the same
  > interpretation of AEAD*AES*_\_GCM as the Erlang/OTP SSH before enabling
  > them. The aes_-gcm@openssh.com variants are always safe to use since they
  > lack the ambiguity.

  The second paragraph in section 5.1 is resolved as:

  1. If the negotiated cipher is AEAD_AES_128_GCM, the mac algorithm is set to
     AEAD_AES_128_GCM.
  1. If the negotiated cipher is AEAD_AES_256_GCM, the mac algorithm is set to
     AEAD_AES_256_GCM.
  1. If the mac algorithm is AEAD_AES_128_GCM, the cipher is set to
     AEAD_AES_128_GCM.
  1. If the mac algorithm is AEAD_AES_256_GCM, the cipher is set to
     AEAD_AES_256_GCM.

  The first rule that matches when read in order from the top is applied

- [RFC 5656](https://tools.ietf.org/html/rfc5656), Elliptic Curve Algorithm
  Integration in the Secure Shell Transport Layer.

  Except

  - 5\. ECMQV Key Exchange
  - 6\.4. ECMQV Key Exchange and Verification Method Name
  - 7\.2. ECMQV Message Numbers
  - 10\.2. Recommended Curves

- [RFC 6668](https://tools.ietf.org/html/rfc6668), SHA-2 Data Integrity
  Verification for the Secure Shell (SSH) Transport Layer Protocol

  Comment: Defines hmac-sha2-256 and hmac-sha2-512

- [Draft-ietf-curdle-ssh-kex-sha2 (work in progress)](https://tools.ietf.org/html/draft-ietf-curdle-ssh-kex-sha2),
  Key Exchange (KEX) Method Updates and Recommendations for Secure Shell (SSH).

  Deviations:

  - `diffie-hellman-group1-sha1`
  - `diffie-hellman-group-exchange-sha1`
  - `diffie-hellman-group14-sha1`

  are not enabled by default as they now are regarded insecure, but are still
  supported and can be enabled with the options
  [preferred_algorithms](`t:ssh:preferred_algorithms_common_option/0`) or
  [modify_algorithms](`t:ssh:modify_algorithms_common_option/0`).

- [RFC 8332](https://tools.ietf.org/html/rfc8332), Use of RSA Keys with SHA-256
  and SHA-512 in the Secure Shell (SSH) Protocol.
- [](){: #supported-ext-info } [RFC 8308](https://tools.ietf.org/html/rfc8308),
  Extension Negotiation in the Secure Shell (SSH) Protocol.

  Implemented are:

  - The Extension Negotiation Mechanism
  - The extension `server-sig-algs`

- [Secure Shell (SSH) Key Exchange Method Using Curve25519 and Curve448](https://tools.ietf.org/html/rfc8731)
- [RFC 8709](https://tools.ietf.org/html/rfc8709) Ed25519 and Ed448 public key
  algorithms for the Secure Shell (SSH) protocol

## SEE ALSO

`m:application`
