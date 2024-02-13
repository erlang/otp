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
# Algorithm Details

This chapter describes details of algorithms in the crypto application.

The tables only documents the supported cryptos and key lengths. The user should
not draw any conclusion on security from the supplied tables.

## Ciphers

A [cipher](`t:crypto:cipher/0`) in the [new api](new_api.md#the-new-api) is
categorized as either [cipher_no_iv()](`t:crypto:cipher_no_iv/0`),
[cipher_iv()](`t:crypto:cipher_iv/0`) or
[cipher_aead()](`t:crypto:cipher_aead/0`). The letters IV are short for
_Initialization Vector_ and AEAD is an abbreviation of _Authenticated Encryption
with Associated Data_.

Due to irregular naming conventions, some cipher names in the old api are
substituted by new names in the new api. For a list of retired names, see
[Retired cipher names](new_api.md#retired-cipher-names).

To dynamically check availability, check that the name in the _Cipher and Mode_
column is present in the list returned by
[crypto:supports(ciphers)](`crypto:supports/1`).

### Ciphers without an IV - cipher_no_iv()

To be used with:

- [crypto_one_time/4](`crypto:crypto_one_time/4`)
- [crypto_init/3](`crypto:crypto_init/3`)

The ciphers are:

| **Cipher and Mode** | **Key length** **\[bytes]** | **Block size** **\[bytes]** |
| ------------------- | --------------------------- | --------------------------- |
| `aes_128_ecb`       | 16                          | 16                          |
| `aes_192_ecb`       | 24                          | 16                          |
| `aes_256_ecb`       | 32                          | 16                          |
| `blowfish_ecb`      | 16                          |  8                          |
| `des_ecb`           |  8                          |  8                          |
| `rc4`               | 16                          |  1                          |

_Table: Ciphers without IV_

### Ciphers with an IV - cipher_iv()

To be used with:

- [crypto_one_time/5](`crypto:crypto_one_time/5`)
- [crypto_init/4](`crypto:crypto_init/4`)

The ciphers are:

| **Cipher and Mode** | **Key length** **\[bytes]** | **IV length** **\[bytes]** | **Block size** **\[bytes]** | **Limited to** **OpenSSL versions** |
| ------------------- | --------------------------- | -------------------------- | --------------------------- | ----------------------------------- |
| `aes_128_cbc`       | 16                          | 16                         | 16                          |                                     |
| `aes_192_cbc`       | 24                          | 16                         | 16                          |                                     |
| `aes_256_cbc`       | 32                          | 16                         | 16                          |                                     |
| `aes_128_cfb8`      | 16                          | 16                         |  1                          |                                     |
| `aes_192_cfb8`      | 24                          | 16                         |  1                          |                                     |
| `aes_256_cfb8`      | 32                          | 16                         |  1                          |                                     |
| `aes_128_cfb128`    | 16                          | 16                         |  1                          |                                     |
| `aes_192_cfb128`    | 24                          | 16                         |  1                          |                                     |
| `aes_256_cfb128`    | 32                          | 16                         |  1                          |                                     |
| `aes_128_ctr`       | 16                          | 16                         |  1                          |                                     |
| `aes_192_ctr`       | 24                          | 16                         |  1                          |                                     |
| `aes_256_ctr`       | 32                          | 16                         |  1                          |                                     |
| `aes_128_ofb`       | 16                          | 16                         |  1                          |                                     |
| `aes_192_ofb`       | 24                          | 16                         |  1                          |                                     |
| `aes_256_ofb`       | 32                          | 16                         |  1                          |                                     |
| `blowfish_cbc`      | 16                          |  8                         |  8                          |                                     |
| `blowfish_cfb64`    | 16                          |  8                         |  1                          |                                     |
| `blowfish_ofb64`    | 16                          |  8                         |  1                          |                                     |
| `chacha20`          | 32                          | 16                         |  1                          | ≥1.1.0d                             |
| `des_cbc`           |  8                          |  8                         |  8                          |                                     |
| `des_ede3_cbc`      | 24                          |  8                         |  8                          |                                     |
| `des_cfb`           |  8                          |  8                         |  1                          |                                     |
| `des_ede3_cfb`      | 24                          |  8                         |  1                          |                                     |
| `rc2_cbc`           | 16                          |  8                         |  8                          |                                     |

_Table: Ciphers with IV_

### Ciphers with AEAD - cipher_aead()

To be used with:

- [crypto_one_time_aead/6](`crypto:crypto_one_time_aead/6`)
- [crypto_one_time_aead/7](`crypto:crypto_one_time_aead/7`)

The ciphers are:

| **Cipher and Mode** | **Key length** **\[bytes]** | **IV length** **\[bytes]** | **AAD length** **\[bytes]** | **Tag length** **\[bytes]** | **Block size** **\[bytes]** | **Limited to** **OpenSSL versions** |
| ------------------- | --------------------------- | -------------------------- | --------------------------- | --------------------------- | --------------------------- | ----------------------------------- |
| `aes_128_ccm`       | 16                          | 7-13                       | any                         | even 4-16 default: 12       | any                         | ≥1.0.1                              |
| `aes_192_ccm`       | 24                          | 7-13                       | any                         | even 4-16 default: 12       | any                         | ≥1.0.1                              |
| `aes_256_ccm`       | 32                          | 7-13                       | any                         | even 4-16 default: 12       | any                         | ≥1.0.1                              |
| `aes_128_gcm`       | 16                          | ≥1                         | any                         | 1-16 default: 16            | any                         | ≥1.0.1                              |
| `aes_192_gcm`       | 24                          | ≥1                         | any                         | 1-16 default: 16            | any                         | ≥1.0.1                              |
| `aes_256_gcm`       | 32                          | ≥1                         | any                         | 1-16 default: 16            | any                         | ≥1.0.1                              |
| `chacha20_poly1305` | 32                          | 1-16                       | any                         | 16                          | any                         | ≥1.1.0                              |

_Table: AEAD ciphers_

## Message Authentication Codes (MACs)

To be used in [mac/4](`crypto:mac/4`) and
[related functions](new_api.md#macs-message-authentication-codes).

### CMAC

CMAC with the following ciphers are available with OpenSSL 1.0.1 or later if not
disabled by configuration.

To dynamically check availability, check that the name `cmac` is present in the
list returned by [crypto:supports(macs)](`crypto:supports/1`). Also check that
the name in the _Cipher and Mode_ column is present in the list returned by
[crypto:supports(ciphers)](`crypto:supports/1`).

| **Cipher and Mode** | **Key length** **\[bytes]** | **Max Mac Length** **(= default length)** **\[bytes]** |
| ------------------- | --------------------------- | ------------------------------------------------------ |
| `aes_128_cbc`       | 16                          | 16                                                     |
| `aes_192_cbc`       | 24                          | 16                                                     |
| `aes_256_cbc`       | 32                          | 16                                                     |
| `aes_128_ecb`       | 16                          | 16                                                     |
| `aes_192_ecb`       | 24                          | 16                                                     |
| `aes_256_ecb`       | 32                          | 16                                                     |
| `blowfish_cbc`      | 16                          |  8                                                     |
| `blowfish_ecb`      | 16                          |  8                                                     |
| `des_cbc`           |  8                          |  8                                                     |
| `des_ecb`           |  8                          |  8                                                     |
| `des_ede3_cbc`      | 24                          |  8                                                     |
| `rc2_cbc`           | 16                          |  8                                                     |

_Table: CMAC cipher key lengths_

### HMAC

Available in all OpenSSL compatible with Erlang CRYPTO if not disabled by
configuration.

To dynamically check availability, check that the name `hmac` is present in the
list returned by [crypto:supports(macs)](`crypto:supports/1`) and that the hash
name is present in the list returned by
[crypto:supports(hashs)](`crypto:supports/1`).

| **Hash**    | **Max Mac Length** **(= default length)** **\[bytes]** |
| ----------- | ------------------------------------------------------ |
| `sha`       | 20                                                     |
| `sha224`    | 28                                                     |
| `sha256`    | 32                                                     |
| `sha384`    | 48                                                     |
| `sha512`    | 64                                                     |
| `sha3_224`  | 28                                                     |
| `sha3_256`  | 32                                                     |
| `sha3_384`  | 48                                                     |
| `sha3_512`  | 64                                                     |
| `shake128`  | 64                                                     |
| `shake256`  | 64                                                     |
| `blake2b`   | 64                                                     |
| `blake2s`   | 32                                                     |
| `md4`       | 16                                                     |
| `md5`       | 16                                                     |
| `ripemd160` | 20                                                     |

_Table: HMAC output sizes_

### POLY1305

POLY1305 is available with OpenSSL 1.1.1 or later if not disabled by
configuration.

To dynamically check availability, check that the name `poly1305` is present in
the list returned by [crypto:supports(macs)](`crypto:supports/1`).

The poly1305 mac wants an 32 bytes key and produces a 16 byte MAC by default.

## Hash

To dynamically check availability, check that the wanted name in the _Names_
column is present in the list returned by
[crypto:supports(hashs)](`crypto:supports/1`).

| **Type** | **Names**                                                  | **Limited to** **OpenSSL versions** |
| -------- | ---------------------------------------------------------- | ----------------------------------- |
| SHA1     | sha                                                        |                                     |
| SHA2     | sha224, sha256, sha384, sha512                             |                                     |
| SHA3     | sha3_224, sha3_256, sha3_384, sha3_512, shake128, shake256 | ≥1.1.1                              |
| MD4      | md4                                                        |                                     |
| MD5      | md5                                                        |                                     |
| RIPEMD   | ripemd160                                                  |                                     |

## Public Key Cryptography

### RSA

RSA is available with all OpenSSL versions compatible with Erlang CRYPTO if not
disabled by configuration. To dynamically check availability, check that the
atom `rsa` is present in the list returned by
[crypto:supports(public_keys)](`crypto:supports/1`).

> #### Warning {: .warning }
>
> The RSA options are experimental.
>
> The exact set of options and there syntax _may_ be changed without prior
> notice.

| **Option**                                                                                                            | **sign/verify**   | **public encrypt** **private decrypt** | **private encrypt** **public decrypt** |
| --------------------------------------------------------------------------------------------------------------------- | ----------------- | -------------------------------------- | -------------------------------------- |
| \{rsa_padding,rsa_x931_padding\}                                                                                      | x                 |                                        | x                                      |
| \{rsa_padding,rsa_pkcs1_padding\}                                                                                     | x                 | x                                      | x                                      |
| \{rsa_padding,rsa_pkcs1_pss_padding\} \{rsa_pss_saltlen, -2..\} \{rsa_mgf1_md, atom()\}                               | x (2) x (2) x (2) |                                        |                                        |
| \{rsa_padding,rsa_pkcs1_oaep_padding\} \{rsa_mgf1_md, atom()\} \{rsa_oaep_label, binary()\}\} \{rsa_oaep_md, atom()\} |                   | x (2) x (2) x (3) x (3)                |                                        |
| \{rsa_padding,rsa_no_padding\}                                                                                        | x (1)             |                                        |                                        |

Notes:

1. (1) OpenSSL ≤ 1.0.0
1. (2) OpenSSL ≥ 1.0.1
1. (3) OpenSSL ≥ 1.1.0

### DSS

DSS is available with OpenSSL versions compatible with Erlang CRYPTO if not
disabled by configuration. To dynamically check availability, check that the
atom `dss` is present in the list returned by
[crypto:supports(public_keys)](`crypto:supports/1`).

### ECDSA

ECDSA is available with OpenSSL 0.9.8o or later if not disabled by
configuration. To dynamically check availability, check that the atom `ecdsa` is
present in the list returned by
[crypto:supports(public_keys)](`crypto:supports/1`). If the atom `ec_gf2m` also
is present, the characteristic two field curves are available.

The actual supported named curves could be checked by examining the list
returned by [crypto:supports(curves)](`crypto:supports/1`).

### EdDSA

EdDSA is available with OpenSSL 1.1.1 or later if not disabled by configuration.
To dynamically check availability, check that the atom `eddsa` is present in the
list returned by [crypto:supports(public_keys)](`crypto:supports/1`).

Support for the curves ed25519 and ed448 is implemented. The actual supported
named curves could be checked by examining the list with the list returned by
[crypto:supports(curves)](`crypto:supports/1`).

### Diffie-Hellman

Diffie-Hellman computations are available with OpenSSL versions compatible with
Erlang CRYPTO if not disabled by configuration. To dynamically check
availability, check that the atom `dh` is present in the list returned by
[crypto:supports(public_keys)](`crypto:supports/1`).

### Elliptic Curve Diffie-Hellman

Elliptic Curve Diffie-Hellman is available with OpenSSL 0.9.8o or later if not
disabled by configuration. To dynamically check availability, check that the
atom `ecdh` is present in the list returned by
[crypto:supports(public_keys)](`crypto:supports/1`).

The Edward curves `x25519` and `x448` are supported with OpenSSL 1.1.1 or later
if not disabled by configuration.

The actual supported named curves could be checked by examining the list
returned by [crypto:supports(curves)](`crypto:supports/1`).
