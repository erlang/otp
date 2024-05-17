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
# Configuring algorithms in SSH

## Introduction

To fully understand how to configure the algorithms, it is essential to have a
basic understanding of the SSH protocol and how OTP SSH app handles the
corresponding items.

The first subsection will give a short background of the SSH protocol while
later sections describes the implementation and provides some examples.

How the different levels of configuration "interfere" with this, see the section
[Algorithm Configuration](configurations.md#algorithm-configuration) in the
chapter [Configuration in SSH](configurations.md).

### Basics of the ssh protocol's algorithms handling

SSH uses different sets of algorithms in different phases of a session. Which
algorithms to use is negotiated by the client and the server at the beginning of
a session. See [RFC 4253](https://tools.ietf.org/html/rfc4253), "The Secure
Shell (SSH) Transport Layer Protocol" for details.

The negotiation is simple: both peers sends their list of supported alghorithms
to the other part. The first algorithm on the client's list that also in on the
server's list is selected. So it is the client's ordering of the list that
gives the priority for the algorithms.

There are five lists exchanged in the connection setup. Three of them are also
divided in two directions, to and from the server.

The lists are (named as in the SSH application's options):

- **`kex`** - Key exchange

  An algorithm is selected for computing a secret encryption key. Among examples
  are: the old nowadays week `'diffie-hellman-group-exchange-sha1'` and the very
  strong and modern `'ecdh-sha2-nistp512'`.

- **`public_key`** - Server host key

  The asymmetric encryption algorithm used in the server's private-public host
  key pair. Examples include the well-known RSA `'ssh-rsa'` and elliptic curve
  `'ecdsa-sha2-nistp521'`.

- **`cipher`** - Symmetric cipher algorithm used for the payload encryption.
  This algorithm will use the key calculated in the kex phase (together with
  other info) to generate the actual key used. Examples are tripple-DES
  `'3des-cbc'` and one of many AES variants `'aes192-ctr'`.

  This list is actually two - one for each direction server-to-client and
  client-to-server. Therefore it is possible but rare to have different
  algorithms in the two directions in one connection.

- **`mac`** - Message authentication code

  "Check sum" of each message sent between the peers. Examples are SHA
  `'hmac-sha1'` and SHA2 `'hmac-sha2-512'`.

  This list is also divided into two for the both directions

- **`compression`** - If and how to compress the message. Examples are `none`,
  that is, no compression and `zlib`.

  This list is also divided into two for the both directions

### The SSH app's mechanism

The set of algorithms that the SSH app uses by default depends on the algorithms
supported by the:

- `m:crypto` app,
- The cryptolib OTP is linked with, usually the one the OS uses, probably
  OpenSSL,
- and finally what the SSH app implements

Due to this, it impossible to list in documentation what algorithms that are
available in a certain installation.

There is an important command to list the actual algorithms and their ordering:
`ssh:default_algorithms/0`.

```erlang
0> ssh:default_algorithms().
[{kex,['ecdh-sha2-nistp384','ecdh-sha2-nistp521',
       'ecdh-sha2-nistp256','diffie-hellman-group-exchange-sha256',
       'diffie-hellman-group16-sha512',
       'diffie-hellman-group18-sha512',
       'diffie-hellman-group14-sha256',
       'diffie-hellman-group14-sha1',
       'diffie-hellman-group-exchange-sha1']},
 {public_key,['ecdsa-sha2-nistp384','ecdsa-sha2-nistp521',
              'ecdsa-sha2-nistp256','ssh-rsa','rsa-sha2-256',
              'rsa-sha2-512','ssh-dss']},
 {cipher,[{client2server,['aes256-gcm@openssh.com',
                          'aes256-ctr','aes192-ctr','aes128-gcm@openssh.com',
                          'aes128-ctr','aes128-cbc','3des-cbc']},
          {server2client,['aes256-gcm@openssh.com','aes256-ctr',
                          'aes192-ctr','aes128-gcm@openssh.com','aes128-ctr',
                          'aes128-cbc','3des-cbc']}]},
 {mac,[{client2server,['hmac-sha2-256','hmac-sha2-512',
                       'hmac-sha1']},
       {server2client,['hmac-sha2-256','hmac-sha2-512',
                       'hmac-sha1']}]},
 {compression,[{client2server,[none,'zlib@openssh.com',zlib]},
               {server2client,[none,'zlib@openssh.com',zlib]}]}]
```

{: #example_default_algorithms }

To change the algorithm list, there are two options which can be used in
[ssh:connect/2,3,4](`ssh:connect/3`) and [ssh:daemon/2,3](`ssh:daemon/2`). The
options could of course be used in all other functions that initiates
connections.

The options are
[preferred_algorithms](`t:ssh:preferred_algorithms_common_option/0`) and
[modify_algorithms](`t:ssh:modify_algorithms_common_option/0`). The first one
replaces the default set, while the latter modifies the default set.

## Replacing the default set: preferred_algorithms

See the [Reference Manual](`t:ssh:preferred_algorithms_common_option/0`) for
details

Here follows a series of examples ranging from simple to more complex.

To forsee the effect of an option there is an experimental function
`ssh:chk_algos_opts(Opts)`. It mangles the options `preferred_algorithms` and
`modify_algorithms` in the same way as `ssh:daemon`, `ssh:connect` and their
friends does.

### Example 1

Replace the kex algorithms list with the single algorithm
`'diffie-hellman-group14-sha256'`:

```erlang
1> ssh:chk_algos_opts(
               [{preferred_algorithms,
                     [{kex, ['diffie-hellman-group14-sha256']}
                     ]
                }
              ]).
[{kex,['diffie-hellman-group14-sha256']},
 {public_key,['ecdsa-sha2-nistp384','ecdsa-sha2-nistp521',
              'ecdsa-sha2-nistp256','ssh-rsa','rsa-sha2-256',
              'rsa-sha2-512','ssh-dss']},
 {cipher,[{client2server,['aes256-gcm@openssh.com',
                          'aes256-ctr','aes192-ctr','aes128-gcm@openssh.com',
                          'aes128-ctr','aes128-cbc','3des-cbc']},
          {server2client,['aes256-gcm@openssh.com','aes256-ctr',
                          'aes192-ctr','aes128-gcm@openssh.com','aes128-ctr',
                          'aes128-cbc','3des-cbc']}]},
 {mac,[{client2server,['hmac-sha2-256','hmac-sha2-512',
                       'hmac-sha1']},
       {server2client,['hmac-sha2-256','hmac-sha2-512',
                       'hmac-sha1']}]},
 {compression,[{client2server,[none,'zlib@openssh.com',zlib]},
               {server2client,[none,'zlib@openssh.com',zlib]}]}]
```

Note that the unmentioned lists (`public_key`, `cipher`, `mac` and
`compression`) are unchanged.

### Example 2

In the lists that are divided in two for the two directions (for example `cipher`) it is
possible to change both directions at once:

```erlang
2> ssh:chk_algos_opts(
               [{preferred_algorithms,
                     [{cipher,['aes128-ctr']}
                     ]
                }
              ]).
[{kex,['ecdh-sha2-nistp384','ecdh-sha2-nistp521',
       'ecdh-sha2-nistp256','diffie-hellman-group-exchange-sha256',
       'diffie-hellman-group16-sha512',
       'diffie-hellman-group18-sha512',
       'diffie-hellman-group14-sha256',
       'diffie-hellman-group14-sha1',
       'diffie-hellman-group-exchange-sha1']},
 {public_key,['ecdsa-sha2-nistp384','ecdsa-sha2-nistp521',
              'ecdsa-sha2-nistp256','ssh-rsa','rsa-sha2-256',
              'rsa-sha2-512','ssh-dss']},
 {cipher,[{client2server,['aes128-ctr']},
          {server2client,['aes128-ctr']}]},
 {mac,[{client2server,['hmac-sha2-256','hmac-sha2-512',
                       'hmac-sha1']},
       {server2client,['hmac-sha2-256','hmac-sha2-512',
                       'hmac-sha1']}]},
 {compression,[{client2server,[none,'zlib@openssh.com',zlib]},
               {server2client,[none,'zlib@openssh.com',zlib]}]}]
```

Note that both lists in `cipher` has been changed to the provided value
(`'aes128-ctr'`).

### Example 3

In the lists that are divided in two for the two directions (for example `cipher`) it is
possible to change only one of the directions:

```erlang
3> ssh:chk_algos_opts(
               [{preferred_algorithms,
                     [{cipher,[{client2server,['aes128-ctr']}]}
                     ]
                }
              ]).
[{kex,['ecdh-sha2-nistp384','ecdh-sha2-nistp521',
       'ecdh-sha2-nistp256','diffie-hellman-group-exchange-sha256',
       'diffie-hellman-group16-sha512',
       'diffie-hellman-group18-sha512',
       'diffie-hellman-group14-sha256',
       'diffie-hellman-group14-sha1',
       'diffie-hellman-group-exchange-sha1']},
 {public_key,['ecdsa-sha2-nistp384','ecdsa-sha2-nistp521',
              'ecdsa-sha2-nistp256','ssh-rsa','rsa-sha2-256',
              'rsa-sha2-512','ssh-dss']},
 {cipher,[{client2server,['aes128-ctr']},
          {server2client,['aes256-gcm@openssh.com','aes256-ctr',
                          'aes192-ctr','aes128-gcm@openssh.com','aes128-ctr',
                          'aes128-cbc','3des-cbc']}]},
 {mac,[{client2server,['hmac-sha2-256','hmac-sha2-512',
                       'hmac-sha1']},
       {server2client,['hmac-sha2-256','hmac-sha2-512',
                       'hmac-sha1']}]},
 {compression,[{client2server,[none,'zlib@openssh.com',zlib]},
               {server2client,[none,'zlib@openssh.com',zlib]}]}]
```

### Example 4

It is of course possible to change more than one list:

```erlang
4> ssh:chk_algos_opts(
               [{preferred_algorithms,
                     [{cipher,['aes128-ctr']},
		      {mac,['hmac-sha2-256']},
                      {kex,['ecdh-sha2-nistp384']},
		      {public_key,['ssh-rsa']},
		      {compression,[{server2client,[none]},
		                    {client2server,[zlib]}]}
                     ]
                }
              ]).
[{kex,['ecdh-sha2-nistp384']},
 {public_key,['ssh-rsa']},
 {cipher,[{client2server,['aes128-ctr']},
          {server2client,['aes128-ctr']}]},
 {mac,[{client2server,['hmac-sha2-256']},
       {server2client,['hmac-sha2-256']}]},
 {compression,[{client2server,[zlib]},
               {server2client,[none]}]}]
```

Note that the order of the tuples in the lists does not matter.

## Modifying the default set: modify_algorithms

A situation where it might be useful to add an algorithm is when one need to use
a supported but disabled one. An example is the `'diffie-hellman-group1-sha1'`
which nowadays is very unsecure and therefore disabled. It is however still
supported and might be used.

The option `preferred_algorithms` may be complicated to use for adding or
removing single algorithms. First one has to list them with
`ssh:default_algorithms()` and then do changes in the lists.

To facilitate addition or removal of algorithms the option `modify_algorithms`
is available. See the
[Reference Manual](`t:ssh:modify_algorithms_common_option/0`) for details.

The option takes a list with instructions to append, prepend or remove
algorithms:

```erlang
{modify_algorithms, [{append,  ...},
                     {prepend, ...},
		     {rm,      ...}
		    ]}
```

Each of the `...` can be a `algs_list()` as the argument to the
`preferred_algorithms` option.

### Example 5

As an example let's add the Diffie-Hellman Group1 first in the kex list. It is
supported according to [Supported algorithms](ssh_app.md#supported_algos).

```erlang
5> ssh:chk_algos_opts(
         [{modify_algorithms,
	       [{prepend,
	           [{kex,['diffie-hellman-group1-sha1']}]
		   }
	       ]
          }
        ]).
[{kex,['diffie-hellman-group1-sha1','ecdh-sha2-nistp384',
       'ecdh-sha2-nistp521','ecdh-sha2-nistp256',
       'diffie-hellman-group-exchange-sha256',
       'diffie-hellman-group16-sha512',
       'diffie-hellman-group18-sha512',
       'diffie-hellman-group14-sha256',
       'diffie-hellman-group14-sha1',
       'diffie-hellman-group-exchange-sha1']},
 {public_key,['ecdsa-sha2-nistp384','ecdsa-sha2-nistp521',
              'ecdsa-sha2-nistp256','ssh-rsa','rsa-sha2-256',
              'rsa-sha2-512','ssh-dss']},
 {cipher,[{client2server,['aes256-gcm@openssh.com',
                          'aes256-ctr','aes192-ctr','aes128-gcm@openssh.com',
                          'aes128-ctr','aes128-cbc','3des-cbc']},
          {server2client,['aes256-gcm@openssh.com','aes256-ctr',
                          'aes192-ctr','aes128-gcm@openssh.com','aes128-ctr',
                          'aes128-cbc','3des-cbc']}]},
 {mac,[{client2server,['hmac-sha2-256','hmac-sha2-512',
                       'hmac-sha1']},
       {server2client,['hmac-sha2-256','hmac-sha2-512',
                       'hmac-sha1']}]},
 {compression,[{client2server,[none,'zlib@openssh.com',zlib]},
               {server2client,[none,'zlib@openssh.com',zlib]}]}]
```

And the result shows that the Diffie-Hellman Group1 is added at the head of the
kex list

### Example 6

In this example, we in put the 'diffie-hellman-group1-sha1' first and also move
the `'ecdh-sha2-nistp521'` to the end in the kex list, that is, `append` it.

```erlang
6> ssh:chk_algos_opts(
         [{modify_algorithms,
	       [{prepend,
	           [{kex, ['diffie-hellman-group1-sha1']}
		   ]},
		{append,
                   [{kex, ['ecdh-sha2-nistp521']}
                   ]}
	       ]
          }
        ]).
[{kex,['diffie-hellman-group1-sha1','ecdh-sha2-nistp384',
       'ecdh-sha2-nistp256','diffie-hellman-group-exchange-sha256',
       'diffie-hellman-group16-sha512',
       'diffie-hellman-group18-sha512',
       'diffie-hellman-group14-sha256',
       'diffie-hellman-group14-sha1',
       'diffie-hellman-group-exchange-sha1','ecdh-sha2-nistp521']},
 {public_key,['ecdsa-sha2-nistp384','ecdsa-sha2-nistp521',
   .....
]
```

Note that the appended algorithm is removed from its original place and then
appended to the same list.

### Example 7

In this example, we use both options (`preferred_algorithms` and
`modify_algorithms`) and also try to prepend an unsupported algorithm. Any
unsupported algorithm is quietly removed.

```erlang
7> ssh:chk_algos_opts(
         [{preferred_algorithms,
               [{cipher,['aes128-ctr']},
	        {mac,['hmac-sha2-256']},
                {kex,['ecdh-sha2-nistp384']},
		{public_key,['ssh-rsa']},
		{compression,[{server2client,[none]},
		              {client2server,[zlib]}]}
               ]
           },
          {modify_algorithms,
	       [{prepend,
	           [{kex, ['some unsupported algorithm']}
		   ]},
		{append,
                   [{kex, ['diffie-hellman-group1-sha1']}
                   ]}
	       ]
          }
        ]).
[{kex,['ecdh-sha2-nistp384','diffie-hellman-group1-sha1']},
 {public_key,['ssh-rsa']},
 {cipher,[{client2server,['aes128-ctr']},
          {server2client,['aes128-ctr']}]},
 {mac,[{client2server,['hmac-sha2-256']},
       {server2client,['hmac-sha2-256']}]},
 {compression,[{client2server,[zlib]},
               {server2client,[none]}]}]
```

It is of course questionable why anyone would like to use the both these options
together, but it is possible if an unforeseen need should arise.

### Example 8

In this example, we need to use a diffie-hellman-group1-sha1 key exchange
algorithm although it is unsafe and disabled by default.

We use the [modify_algorithms](`t:ssh:modify_algorithms_common_option/0`)
option, because we want to keep all other algorithm definitions.

We add the option:

```erlang
  {modify_algorithms, [{append, [{kex,['diffie-hellman-group1-sha1']}]}]}
```

either to the Options list in a function call, in the `ssh.app` file or in a
`.config` file for the `erl` command. See the chapter
[Configuration in SSH](configurations.md) in the SSH User's Guide.

### Example 9

In this example, we need to use a DSA key for sign and verify. It might be
either as a user's key, a host's key or both.

To do that, we enable the 'ssh-dss' algorithm that is disabled by default by
security reasons. We use the
[modify_algorithms](`t:ssh:modify_algorithms_common_option/0`) option, because
we want to keep all other algorithm definitions.

We add the option:

```erlang
  {modify_algorithms, [{append, [{public_key,['ssh-dss']}]}]}
```

either to the Options list in a function call, in the `ssh.app` file or in a
`.config` file for the `erl` command. See the chapter
[Configuration in SSH](configurations.md) in the SSH User's Guide.
