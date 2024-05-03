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
# New and Old API

This chapter describes the new api to encryption and decryption.

## Background

The CRYPTO app has evolved during its lifetime. Since also the OpenSSL cryptolib
has changed the API several times, there are parts of the CRYPTO app that uses a
very old one internally and other parts that uses the latest one. The internal
definitions of e.g cipher names was a bit hard to maintain.

It turned out that using the old api in the new way (more about that later), and
still keep it backwards compatible, was not possible. Specially as more
precision in the error messages is desired it could not be combined with the old
standard.

Therefore the old api (see next section) is kept for now but internally
implemented with new primitives.

## The old API

The old functions - deprecated from 23.0 and removed from OTP 24.0 - are for
ciphers:

- `block_encrypt/3`
- `block_encrypt/4`
- `block_decrypt/3`
- `block_decrypt/4`
- `stream_init/2`
- `stream_init/3`
- `stream_encrypt/2`
- `stream_decrypt/2`
- `next_iv/2`
- `next_iv/3`

for lists of supported algorithms:

- `supports/0`

and for MACs (Message Authentication Codes):

- `cmac/3`
- `cmac/4`
- `hmac/3`
- `hmac/4`
- `hmac_init/2`
- `hmac_update/2`
- `hmac_final/1`
- `hmac_final_n/2`
- `poly1305/2`

## The new API

### Encryption and decryption

The new functions for encrypting or decrypting one single binary are:

- [crypto_one_time/4](`crypto:crypto_one_time/4`)
- [crypto_one_time/5](`crypto:crypto_one_time/5`)
- [crypto_one_time_aead/6](`crypto:crypto_one_time_aead/6`)
- [crypto_one_time_aead/7](`crypto:crypto_one_time_aead/7`)

In those functions the internal crypto state is first created and initialized
with the cipher type, the key and possibly other data. Then the single binary is
encrypted or decrypted, the crypto state is de-allocated and the result of the
crypto operation is returned.

The `crypto_one_time_aead` functions are for the ciphers of mode `ccm` or `gcm`,
and for the cipher `chacha20-poly1305`.

For repeated encryption or decryption of a text divided in parts, where the
internal crypto state is initialized once, and then many binaries are encrypted
or decrypted with the same state, the functions are:

- [crypto_init/4](`crypto:crypto_init/4`)
- [crypto_init/3](`crypto:crypto_init/3`)
- [crypto_update/2](`crypto:crypto_update/2`)
- [crypto_final/1](`crypto:crypto_final/1`)

The `crypto_init` initialies an internal cipher state, and one or more calls of
`crypto_update` does the actual encryption or decryption. Note that AEAD ciphers
can't be handled this way due to their nature.

An example of where those functions are needed, is when handling the TLS
protocol.

If padding was not enabled, the call to
[crypto_final/1](`crypto:crypto_final/1`) may be excluded.

For information about available algorithms, use:

- [supports/1](`crypto:supports/1`)
- [hash_info/1](`crypto:hash_info/1`)
- [cipher_info/1](`crypto:cipher_info/1`)

The `next_iv/2` and `next_iv/3` are not needed since the `crypto_init` and
`crypto_update` includes this functionality.

### MACs (Message Authentication Codes)

The new functions for calculating a MAC of a single piece of text are:

- [mac/3](`crypto:mac/3`)
- [mac/4](`crypto:mac/4`)
- [macN/4](`crypto:macN/4`)
- [macN/5](`crypto:macN/5`)

For calculating a MAC of a text divided in parts use:

- [mac_init/2](`crypto:mac_init/2`)
- [mac_init/3](`crypto:mac_init/3`)
- [mac_update/2](`crypto:mac_update/2`)
- [mac_final/1](`crypto:mac_final/1`)
- [mac_finalN/2](`crypto:mac_finalN/2`)

## Examples of the new api

### Examples of crypto_init/4 and crypto_update/2

The functions [crypto_init/4](`crypto:crypto_init/4`) and
[crypto_update/2](`crypto:crypto_update/2`) are intended to be used for
encrypting or decrypting a sequence of blocks. First one call of `crypto_init/4`
initialises the crypto context. One or more calls `crypto_update/2` does the
actual encryption or decryption for each block.

This example shows first the encryption of two blocks and then decryptions of
the cipher text, but divided into three blocks just to show that it is possible
to divide the plain text and cipher text differently for some ciphers:

```erlang
	1> crypto:start().
	ok
	2> Key = <<1:128>>.
	<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>
	3> IV = <<0:128>>.
	<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>
	4> StateEnc = crypto:crypto_init(aes_128_ctr, Key, IV, true). % encrypt -> true
	#Ref<0.3768901617.1128660993.124047>
	5> crypto:crypto_update(StateEnc, <<"First bytes">>).
	<<67,44,216,166,25,130,203,5,66,6,162>>
	6> crypto:crypto_update(StateEnc, <<"Second bytes">>).
	<<16,79,94,115,234,197,94,253,16,144,151,41>>
	7>
	7> StateDec = crypto:crypto_init(aes_128_ctr, Key, IV, false). % decrypt -> false
	#Ref<0.3768901617.1128660994.124255>
	8> crypto:crypto_update(StateDec, <<67,44,216,166,25,130,203>>).
	<<"First b">>
	9> crypto:crypto_update(StateDec, <<5,66,6,162,16,79,94,115,234,197,
        94,253,16,144,151>>).
	<<"ytesSecond byte">>
	10> crypto:crypto_update(StateDec, <<41>>).
	<<"s">>
	11>
```

Note that the internal data that the `StateEnc` and `StateDec` references are
destructivly updated by the calls to
[crypto_update/2](`crypto:crypto_update/2`). This is to gain time in the calls
of the nifs interfacing the cryptolib. In a loop where the state is saved in the
loop's state, it also saves one update of the loop state per crypto operation.

For example, a simple server receiving text parts to encrypt and send the result
back to the one who sent them (the `Requester`):

```erlang
	encode(Crypto, Key, IV) ->
	crypto_loop(crypto:crypto_init(Crypto, Key, IV, true)).

	crypto_loop(State) ->
	receive
        {Text, Requester} ->
        Requester ! crypto:crypto_update(State, Text),
	loop(State)
	end.
```

### Example of crypto_one_time/5

The same example as in the
[previous section](new_api.md#examples-of-crypto_init-4-and-crypto_update-2),
but now with one call to [crypto_one_time/5](`crypto:crypto_one_time/5`):

```erlang
	1> Key = <<1:128>>.
	<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>
	2> IV = <<0:128>>.
	<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>
	3> Txt = [<<"First bytes">>,<<"Second bytes">>].
	[<<"First bytes">>,<<"Second bytes">>]
	4> crypto:crypto_one_time(aes_128_ctr, Key, IV, Txt, true).
	<<67,44,216,166,25,130,203,5,66,6,162,16,79,94,115,234,
	197,94,253,16,144,151,41>>
	5>
```

The `[<<"First bytes">>,<<"Second bytes">>]` could of course have been one
single binary: `<<"First bytesSecond bytes">>`.

### Example of crypto_one_time_aead/6

The same example as in the
[previous section](new_api.md#example-of-crypto_one_time-5), but now with one
call to [crypto_one_time_aead/6](`crypto:crypto_one_time_aead/6`):

```erlang
	1> Key = <<1:128>>.
	<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>
	2> IV = <<0:128>>.
	<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>
	3> Txt = [<<"First bytes">>,<<"Second bytes">>].
	[<<"First bytes">>,<<"Second bytes">>]
	4> AAD = <<"Some additional auth data">>.
	<<"Some additional auth data">>
	5> crypto:crypto_one_time_aead(aes_128_gcm, Key, IV, Txt, AAD, true).
	{<<240,130,38,96,130,241,189,52,3,190,179,213,132,1,72,
	192,103,176,90,104,15,71,158>>,
	<<131,47,45,91,142,85,9,244,21,141,214,71,31,135,2,155>>}
	6>
```

The `[<<"First bytes">>,<<"Second bytes">>]` could of course have been one
single binary: `<<"First bytesSecond bytes">>`.

### Example of mac_init mac_update and mac_final

```erlang
	1> Key = <<1:128>>.
	<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>
	2> StateMac = crypto:mac_init(cmac, aes_128_cbc, Key).
	#Ref<0.2424664121.2781478916.232610>
	3> crypto:mac_update(StateMac, <<"First bytes">>).
	#Ref<0.2424664121.2781478916.232610>
	4> crypto:mac_update(StateMac, " ").
	#Ref<0.2424664121.2781478916.232610>
	5> crypto:mac_update(StateMac, <<"last bytes">>).
	#Ref<0.2424664121.2781478916.232610>
	6> crypto:mac_final(StateMac).
	<<68,191,219,128,84,77,11,193,197,238,107,6,214,141,160,
	249>>
	7>
```

and compare the result with a single calculation just for this example:

```erlang
	7> crypto:mac(cmac, aes_128_cbc, Key, "First bytes last bytes").
	<<68,191,219,128,84,77,11,193,197,238,107,6,214,141,160,
	249>>
	8> v(7) == v(6).
	true
	9>
```

## Retired cipher names

This table lists the retired cipher names in the first column and suggests names
to replace them with in the second column.

The new names follows the OpenSSL libcrypto names. The format is
ALGORITM_KEYSIZE_MODE.

Examples of algorithms are aes, chacha20 and des. The keysize is the number of
bits and examples of the mode are cbc, ctr and gcm. The mode may be followed by
a number depending on the mode. An example is the ccm mode which has a variant
called ccm8 where the so called tag has a length of eight bits.

The old names had by time lost any common naming convention which the new names
now introduces. The new names include the key length which improves the error
checking in the lower levels of the crypto application.

| **Instead of:** | **Use:**                                         |
| --------------- | ------------------------------------------------ |
| `aes_cbc128`    | `aes_128_cbc`                                    |
| `aes_cbc256`    | `aes_256_cbc`                                    |
| `aes_cbc`       | `aes_128_cbc, aes_192_cbc, aes_256_cbc`          |
| `aes_ccm`       | `aes_128_ccm, aes_192_ccm, aes_256_ccm`          |
| `aes_cfb128`    | `aes_128_cfb128, aes_192_cfb128, aes_256_cfb128` |
| `aes_cfb8`      | `aes_128_cfb8, aes_192_cfb8, aes_256_cfb8`       |
| `aes_ctr`       | `aes_128_ctr, aes_192_ctr, aes_256_ctr`          |
| `aes_gcm`       | `aes_128_gcm, aes_192_gcm, aes_256_gcm`          |
| `des3_cbc`      | `des_ede3_cbc`                                   |
| `des3_cbf`      | `des_ede3_cfb`                                   |
| `des3_cfb`      | `des_ede3_cfb`                                   |
| `des_ede3`      | `des_ede3_cbc`                                   |
| `des_ede3_cbf`  | `des_ede3_cfb`                                   |
