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
# Engine Stored Keys

[](){: #engine_key } This chapter describes the support in the crypto
application for using public and private keys stored in encryption engines.

## Background

[OpenSSL](https://www.openssl.org/) exposes an Engine API, which makes it
possible to plug in alternative implementations for some of the cryptographic
operations implemented by OpenSSL. See the chapter
[Engine Load](engine_load.md#engine_load) for details and how to load an Engine.

An engine could among other tasks provide a storage for private or public keys.
Such a storage could be made safer than the normal file system. Those techniques
are not described in this User's Guide. Here we concentrate on how to use
private or public keys stored in such an engine.

The storage engine must call `ENGINE_set_load_privkey_function` and
`ENGINE_set_load_pubkey_function`. See the OpenSSL cryptolib's
[manpages](https://www.openssl.org/docs/manpages.html).

OTP/Crypto requires that the user provides two or three items of information
about the key. The application used by the user is usually on a higher level,
for example in [SSL](`t:ssl:key/0`). If using the crypto application directly,
it is required that:

- an Engine is loaded, see the chapter on
  [Engine Load](engine_load.md#engine_load) or the
  [Reference Manual](`crypto:engine_load/3`)
- a reference to a key in the Engine is available. This should be an Erlang
  string or binary and depends on the Engine loaded
- an Erlang map is constructed with the Engine reference, the key reference and
  possibly a key passphrase if needed by the Engine. See the
  [Reference Manual](`t:crypto:engine_key_ref/0`) for details of the map.

## Use Cases

### Sign with an engine stored private key

This example shows how to construct a key reference that is used in a sign
operation. The actual key is stored in the engine that is loaded at prompt 1.

```erlang
1> {ok, EngineRef} = crypto:engine_load(....).
...
{ok,#Ref<0.2399045421.3028942852.173962>}
2> PrivKey = #{engine => EngineRef,
               key_id => "id of the private key in Engine"}.
...
3> Signature = crypto:sign(rsa, sha, <<"The message">>, PrivKey).
<<65,6,125,254,54,233,84,77,83,63,168,28,169,214,121,76,
  207,177,124,183,156,185,160,243,36,79,125,230,231,...>>
```

### Verify with an engine stored public key

Here the signature and message in the last example is verifyed using the public
key. The public key is stored in an engine, only to exemplify that it is
possible. The public key could of course be handled openly as usual.

```erlang
4> PublicKey = #{engine => EngineRef,
                 key_id => "id of the public key in Engine"}.
...
5> crypto:verify(rsa, sha, <<"The message">>, Signature, PublicKey).
true
6>
```

### Using a password protected private key

The same example as the first sign example, except that a password protects the
key down in the Engine.

```erlang
6> PrivKeyPwd = #{engine => EngineRef,
                  key_id => "id of the pwd protected private key in Engine",
		  password => "password"}.
...
7> crypto:sign(rsa, sha, <<"The message">>, PrivKeyPwd).
<<140,80,168,101,234,211,146,183,231,190,160,82,85,163,
  175,106,77,241,141,120,72,149,181,181,194,154,175,76,
  223,...>>
8>
```
