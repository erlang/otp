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
# Configuration in SSH

## Introduction

The OTP SSH app can be configurated by a large amount of _Options_. This chapter
will not go into details of what each of the options does. It will however
describe and define different ways by which they could be entered.

Options for hardening are described in the [Hardening SSH](hardening.md)
chapter. How the options for algorithm configuration interact are described in
the [Configuring algorithms in SSH](configure_algos.md) chapter.

## Options configuration

There are from OTP-23.0 two main ways to set an option:

- Like before, in the `Options` parameter in the Erlang code in a call to for
  example `ssh:daemon/3` or `ssh:connect/3` or any of their variants. Example:

  ```erlang
  ssh:connect(22, [{user,"foo"}])
  ```

- In [OTP Configuration Parameters](`e:kernel:config.md`):

  - In the erl command line:

    ```text
    erl -ssh user \"foo\"
    ```

  - In the `ssh.app` file, in the `env` part

    ```erlang
    {application, ssh,
     [{description, "SSH-2 for Erlang/OTP"},
      {vsn, "4.9"},
      {modules, [ssh,
            ...
    	     ssh_xfer]},
      {registered, []},
      {applications, [kernel, stdlib, crypto, public_key]},
      {env, [{user, "bar"]}, % <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< HERE
      {mod, {ssh_app, []}},
           ...
    ```

  - In a .config file:

    ```text
    erl -config ex1
    ```

    where `ex1.config` contains:

    ```erlang
    [
    {ssh, [{user, "foo"}]}
    ].
    ```

  If the option is intended only for a server or for a client, it may be set in
  this way:

  ```erlang
  [
  {ssh, [{server_options,[{user, "foo"}]},
         {client_options,[{user, "bar"}]}
  ].
  ```

  A server (daemon) will use the user name `foo`, and a client will use the name
  `bar`.

## Precedence

If an option is set in more than one way, what happens?

There is an ordering, which is:

- Level 0: Hard-coded default values in the OTP SSH source code
- Level 1: [OTP Configuration Parameters](`e:kernel:config.md`)
- Level 2: Options in the [OTP Configuration Parameters](`e:kernel:config.md`)
  `server_options` or `client_options`
- Level 3: Options in argument list to a function

If the same option is set at two different levels, the one at the highest level
is used.

The only exception is the
[modify_algorithms](`t:ssh:modify_algorithms_common_option/0`) common option.
They are all applied in ascending level order on the set of algorithms. So a
`modify_algorithms` on level one is applied before one of level two and so on.

If there is an
[preferred_algorithms](`t:ssh:preferred_algorithms_common_option/0`) option on
some level the whole set is replaced by that in that option and _all
modify_algorithms are applied_ in level ordering.

The reason for applying all
[modify_algorithms](`t:ssh:modify_algorithms_common_option/0`) in level order,
is to enable the user to add an algorithm that has been removed from the default
set without code changes, only by adding an option in a config file. This can be
used to interoperate with legacy systems that still uses algorithms no longer
considered secure enough to be supported by default.

### Algorithm configuration

There is a [separate chapter](configure_algos.md#introduction) about how
[preferred_algorithms](`t:ssh:preferred_algorithms_common_option/0`) and
[modify_algorithms](`t:ssh:modify_algorithms_common_option/0`) co-operate. How
the different configuration levels affect them, is described here in this
section.

#### The ssh:start/0 function

If the application SSH is _not_ [started](`ssh:start/0`), the command
`ssh:default_algorithms/0` delivers the list of default (hardcoded) algorithms
with respect to the support in the current cryptolib.

If the application SSH _is_ [started](`ssh:start/0`), the command
`ssh:default_algorithms/0` delvers the list of algorithms after application of
level 0 and level 1 configurations.

Here is an example. The config-file has the following contents:

```text
$ cat ex2.config
[
 {ssh, [{preferred_algorithms, [{cipher, ['aes192-ctr']},
       			        {public_key, ['ssh-rsa']},
                                {kex, ['ecdh-sha2-nistp384']},
                                {mac, ['hmac-sha1']}]}]}
].
```

Erlang is started with `ex2.config` as configuration and we check the default
set of algorithms before starting ssh:

```text
$ erl -config ex2
Erlang/OTP 23 [RELEASE CANDIDATE 1] [erts-10.6.4] [source-96a0823109] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe]

Eshell V10.6.4  (abort with ^G)
1> ssh:default_algorithms().
[{kex,['ecdh-sha2-nistp384','ecdh-sha2-nistp521',
       'ecdh-sha2-nistp256','diffie-hellman-group-exchange-sha256',
       'diffie-hellman-group16-sha512',
       'diffie-hellman-group18-sha512',
       'diffie-hellman-group14-sha256','curve25519-sha256',
       'curve25519-sha256@libssh.org','curve448-sha512',
       'diffie-hellman-group14-sha1',
       'diffie-hellman-group-exchange-sha1']},
 {public_key,['ecdsa-sha2-nistp384','ecdsa-sha2-nistp521',
              'ecdsa-sha2-nistp256','ssh-ed25519','ssh-ed448','ssh-rsa',
              'rsa-sha2-256','rsa-sha2-512','ssh-dss']},
 {cipher,[{client2server,['chacha20-poly1305@openssh.com',
                          'aes256-gcm@openssh.com','aes256-ctr','aes192-ctr',
                          'aes128-gcm@openssh.com','aes128-ctr','aes256-cbc',
                          'aes192-cbc','aes128-cbc','3des-cbc']},
          {server2client,['chacha20-poly1305@openssh.com',
                          'aes256-gcm@openssh.com','aes256-ctr','aes192-ctr',
                          'aes128-gcm@openssh.com','aes128-ctr','aes256-cbc',
                          'aes192-cbc','aes128-cbc','3des-cbc']}]},
 {mac,[{client2server,['hmac-sha2-256','hmac-sha2-512',
                       'hmac-sha1']},
       {server2client,['hmac-sha2-256','hmac-sha2-512',
                       'hmac-sha1']}]},
 {compression,[{client2server,[none,'zlib@openssh.com',zlib]},
               {server2client,[none,'zlib@openssh.com',zlib]}]}]
```

Note that the algorithms in the file `ex2.config` is not yet applied. They will
be when we start ssh:

```erlang
2> ssh:start().
ok
3> ssh:default_algorithms().
[{kex,['ecdh-sha2-nistp384']},
 {public_key,['ssh-rsa']},
 {cipher,[{client2server,['aes192-ctr']},
          {server2client,['aes192-ctr']}]},
 {mac,[{client2server,['hmac-sha1']},
       {server2client,['hmac-sha1']}]},
 {compression,[{client2server,[none,'zlib@openssh.com',zlib]},
               {server2client,[none,'zlib@openssh.com',zlib]}]}]
4>
```

We see that the algorithm set is changed to the one in the `ex2.config`. Since
`compression` is not specified in the file, the hard-coded default is still used
for that entry.

#### Establishing a connection (ssh:connect et al) or starting a daemon (ssh:daemon)

Both when the client establishes a connection with ssh:connect or other
functions, or a daemon is started with ssh:daemon, the option lists in the
function calls are also used.

If a client is started (ssh:connect et al), the environment variable
`client_options` is used. Similarly for a daemon the `server_options` variable
is handled.

If any [preferred_algorithms](`t:ssh:preferred_algorithms_common_option/0`) is
present, the one with the highest level is used, that is, the `Option` list
parameter has the highest priority. Then the
[modify_algorithms](`t:ssh:modify_algorithms_common_option/0`) on all levels in
order starting with level 0 are applied.

We continue the example above by connecting to a server and modifying the `kex`
algorithm set. We remove the only one (`'ecdh-sha2-nistp384'`) and add
`'curve25519-sha256@libssh.org'` by appending it to the now empty list:

```erlang
4> {ok,C} = ssh:connect(loopback, 22,
                        [{modify_algorithms,
                                 [{rm,
                                     [ {kex,['ecdh-sha2-nistp384']} ]
				  },
                                  {append,
			             [ {kex,['curve25519-sha256@libssh.org']} ]
				  }
				 ]
	                 }
                        ]).
{ok,>0.118.0>}
```

We check which algorithms are negotiated by the client and the server, and note
that the (only) `kex` algorithm `'curve25519-sha256@libssh.org'` was selected:

```erlang
5> ssh:connection_info(C, algorithms).
{algorithms,[{kex,'curve25519-sha256@libssh.org'},
             {hkey,'ssh-rsa'},
             {send_mac,'hmac-sha1'},
             {recv_mac,'hmac-sha1'},
             {encrypt,'aes192-ctr'},
             {decrypt,'aes192-ctr'},
             {compress,none},
             {decompress,none},
             {send_ext_info,false},
             {recv_ext_info,true}]}
```

#### Example of modify_algorithms handling

We will now check if the
[modify_algorithms](`t:ssh:modify_algorithms_common_option/0`) on a lower level
is applied to a
[preferred_algorithms](`t:ssh:preferred_algorithms_common_option/0`) on a higher
level. We will do that by enabling the `ssh-dss` algorithm that is supported,
but not in the default set.

The config file `ex3.config` has the contents:

```erlang
[
 {ssh, [{modify_algorithms,
         [ {prepend, [{public_key, ['ssh-dss']}]} ]
        }]}
].
```

A newly started erlang shell shows that no `'ssh-dss'` is present in the
`public_key` entry:

```erlang
1> proplists:get_value(public_key, ssh:default_algorithms()).
['ecdsa-sha2-nistp384','ecdsa-sha2-nistp521',
 'ecdsa-sha2-nistp256','ssh-ed25519','ssh-ed448',
 'rsa-sha2-256','rsa-sha2-512','ssh-rsa']
2>
```

A call to `ssh:connect/3` removes all algorithms but one of each type:

```erlang
2> ssh:start().
ok
3> {ok,C} = ssh:connect(loopback, 22,
                        [{preferred_algorithms,
                         [{public_key, ['ecdsa-sha2-nistp256']},
			  {kex, ['ecdh-sha2-nistp256']},
		          {cipher, ['chacha20-poly1305@openssh.com']},
			  {mac, ['hmac-sha2-256']},
			  {compression, [none]}
			  ]}
			 ]).
{ok,<0.101.0>}
4> ssh:connection_info(C,algorithms).
{algorithms,[{kex,'ecdh-sha2-nistp256'},
             {hkey,'ssh-dss'},
             {send_mac,'chacha20-poly1305@openssh.com'},
             {recv_mac,'chacha20-poly1305@openssh.com'},
             {encrypt,'chacha20-poly1305@openssh.com'},
             {decrypt,'chacha20-poly1305@openssh.com'},
             {compress,none},
             {decompress,none},
             {send_ext_info,false},
             {recv_ext_info,true}]}
5>
```

But `'ssh-dss'` is selected although the call inserted _only_
`'ecdsa-sha2-nistp256'` as acceptable.

This example showed that we could augment the set of algorithms with a
config-file without the need to change the actual call.

For demonstration purposes we used `prepend` instead of `append`. This forces
the negotiation to select `ssh-dss` since the the full list of public key
algorithms was `['ssh-dss','ecdsa-sha2-nistp256']`. Normally it is safer to
append a non-default algorithm.
