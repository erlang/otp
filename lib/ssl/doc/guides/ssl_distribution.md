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
# Using TLS for Erlang Distribution

This section describes how the Erlang distribution can use TLS to get extra
verification and security.

The Erlang distribution can in theory use almost any connection-based protocol
as bearer. However, a module that implements the protocol-specific parts of the
connection setup is needed. The default distribution module is `inet_tcp_dist`
in the Kernel application. When starting an Erlang node distributed,
`net_kernel` uses this module to set up listen ports and connections.

In the SSL application, an extra distribution module, `inet_tls_dist`, can be
used as an alternative. All distribution connections will use TLS and all
participating Erlang nodes in a distributed system must use this distribution
module.

The security level depends on the parameters provided to the TLS connection
setup. Erlang node cookies are however always used, as they can be used to
differentiate between two different Erlang networks.

To set up Erlang distribution over TLS:

- _Step 1:_ Build boot scripts including the SSL application.
- _Step 2:_ Specify the distribution module for `net_kernel`.
- _Step 3:_ Specify the security options and other SSL options.
- _Step 4:_ Set up the environment to always use TLS.

The following sections describe these steps.

## Building Boot Scripts Including the SSL Application

Boot scripts are built using the `systools` utility in the SASL application. For
more information on `systools`, see the SASL documentation. This is only an
example of what can be done.

The simplest boot script possible includes only the Kernel and STDLIB
applications. Such a script is located in the `bin` directory of the Erlang
distribution. The source for the script is found under the Erlang installation
top directory under `releases/<OTP version>/start_clean.rel`.

Do the following:

- Copy that script to another location (and preferably another name).
- Add the applications Crypto, Public Key, and SSL with their current version
  numbers after the STDLIB application.

The following shows an example `.rel` file with TLS added:

```erlang
      {release, {"OTP  APN 181 01","R15A"}, {erts, "5.9"},
      [{kernel,"2.15"},
      {stdlib,"1.18"},
      {crypto, "2.0.3"},
      {public_key, "0.12"},
      {asn1, "4.0"},
      {ssl, "5.0"}
      ]}.
```

The version numbers differ in your system. Whenever one of the applications
included in the script is upgraded, change the script.

Do the following:

- Build the boot script.

  Assuming the `.rel file` is stored in a file `start_ssl.rel` in the current
  directory, a boot script can be built as follows:

```text
   1> systools:make_script("start_ssl",[]).
```

There is now a `start_ssl.boot` file in the current directory.

Do the following:

- Test the boot script. To do this, start Erlang with the `-boot` command-line
  parameter specifying this boot script (with its full path, but without the
  `.boot` suffix). In UNIX it can look as follows:

```text
$ erl -boot /home/me/ssl/start_ssl
Erlang (BEAM) emulator version 5.0

Eshell V5.0  (abort with ^G)
1> whereis(ssl_manager).
<0.41.0>
```

The `whereis` function-call verifies that the SSL application is started.

As an alternative to building a bootscript, you can explicitly add the path to
the SSL `ebin` directory on the command line. This is done with command-line
option `-pa`. This works as the SSL application does not need to be started for
the distribution to come up, as a clone of the SSL application is hooked into
the Kernel application. So, as long as the SSL application code can be reached,
the distribution starts. The `-pa` method is only recommended for testing
purposes.

> #### Note {: .info }
>
> The clone of the SSL application must enable the use of the SSL code in such
> an early bootstage as needed to set up the distribution. However, this makes
> it impossible to soft upgrade the SSL application.

## Specifying Distribution Module for net_kernel

The distribution module for TLS is named `inet_tls_dist` and is specified on the
command line with option `-proto_dist`. The argument to `-proto_dist` is to be
the module name without suffix `_dist`. So, this distribution module is
specified with `-proto_dist inet_tls` on the command line.

Extending the command line gives the following:

```text
$ erl -boot /home/me/ssl/start_ssl -proto_dist inet_tls
```

For the distribution to be started, give the emulator a name as well:

```text
$ erl -boot /home/me/ssl/start_ssl -proto_dist inet_tls -sname ssl_test
Erlang (BEAM) emulator version 5.0 [source]

Eshell V5.0  (abort with ^G)
(ssl_test@myhost)1>
```

However, a node started in this way refuses to talk to other nodes, as no TLS
parameters are supplied (see the next section).

## Specifying TLS Options

The TLS distribution options can be written into a file that is consulted when
the node is started. This file name is then specified with the command line
argument `-ssl_dist_optfile`.

Any available TLS option can be specified in an options file, but note that
options that take a `fun()` has to use the syntax `fun Mod:Func/Arity` since a
function body cannot be compiled when consulting a file.

Do not tamper with the socket options `list`, `binary`, `active`, `packet`,
`nodelay` and `deliver` since they are used by the distribution protocol handler
itself. Other raw socket options such as `packet_size` may interfere severely,
so beware\!

For TLS to work, at least a public key and a certificate must be specified for
the server side. In the following example, the PEM file
`"/home/me/ssl/erlserver.pem"` contains both the server certificate and its
private key.

Create a file named for example `"/home/me/ssl/ssl_test@myhost.conf"`:

```erlang
[{server,
  [{certfile, "/home/me/ssl/erlserver.pem"},
   {secure_renegotiate, true}]},
 {client,
  [{secure_renegotiate, true}]}].
```

And then start the node like this (line breaks in the command are for
readability, and shall not be there when typed):

```text
$ erl -boot /home/me/ssl/start_ssl -proto_dist inet_tls
  -ssl_dist_optfile "/home/me/ssl/ssl_test@myhost.conf"
  -sname ssl_test
```

The options in the `{server, Opts}` tuple are used when calling
`ssl:handshake/3`, and the options in the `{client, Opts}` tuple are used when
calling `ssl:connect/4`.

For the client, the option `{server_name_indication, atom_to_list(TargetNode)}`
is added when connecting. This makes it possible to use the client option
`{verify, verify_peer}`, and the client will verify that the certificate matches
the node name you are connecting to. This only works if the the server
certificate is issued to the name
[`atom_to_list(TargetNode)`](`atom_to_list/1`).

For the server it is also possible to use the option `{verify, verify_peer}` and
the server will only accept client connections with certificates that are
trusted by a root certificate that the server knows. A client that presents an
untrusted certificate will be rejected. This option is preferably combined with
`{fail_if_no_peer_cert, true}` or a client will still be accepted if it does not
present any certificate.

A node started in this way is fully functional, using TLS as the distribution
protocol.

## Specifying TLS Options (Legacy)

As in the previous section the PEM file `"/home/me/ssl/erlserver.pem"` contains
both the server certificate and its private key.

On the `erl` command line you can specify options that the TLS distribution adds
when creating a socket.

The simplest TLS options in the following list can be specified by adding the
prefix `server_` or `client_` to the option name:

- `certfile`
- `keyfile`
- `password`
- `cacertfile`
- `verify`
- `verify_fun` (write as `{Module, Function, InitialUserState}`)
- `crl_check`
- `crl_cache` (write as Erlang term)
- `reuse_sessions`
- `secure_renegotiate`
- `depth`
- `hibernate_after`
- `ciphers` (use old string format)

Note that `verify_fun` needs to be written in a different form than the
corresponding TLS option, since funs are not accepted on the command line.

The server can also take the options `dhfile` and `fail_if_no_peer_cert` (also
prefixed).

`client_`\-prefixed options are used when the distribution initiates a
connection to another node. `server_`\-prefixed options are used when accepting
a connection from a remote node.

Raw socket options, such as `packet` and `size` must not be specified on the
command line.

The command-line argument for specifying the TLS options is named
`-ssl_dist_opt` and is to be followed by pairs of SSL options and their values.
Argument `-ssl_dist_opt` can be repeated any number of times.

An example command line doing the same as the example in the previous section
can now look as follows (line breaks in the command are for readability, and
shall not be there when typed):

```text
$ erl -boot /home/me/ssl/start_ssl -proto_dist inet_tls
  -ssl_dist_opt server_certfile "/home/me/ssl/erlserver.pem"
  -ssl_dist_opt server_secure_renegotiate true client_secure_renegotiate true
  -sname ssl_test
Erlang (BEAM) emulator version 5.0 [source]

Eshell V5.0  (abort with ^G)
(ssl_test@myhost)1>
```

## Setting up Environment to Always Use TLS (Legacy)

A convenient way to specify arguments to Erlang is to use environment variable
`ERL_FLAGS`. All the flags needed to use the TLS distribution can be specified
in that variable and are then interpreted as command-line arguments for all
subsequent invocations of Erlang.

In a Unix (Bourne) shell, it can look as follows (line breaks are for
readability, they are not to be there when typed):

```erlang
$ ERL_FLAGS="-boot /home/me/ssl/start_ssl -proto_dist inet_tls
  -ssl_dist_opt server_certfile /home/me/ssl/erlserver.pem
  -ssl_dist_opt server_secure_renegotiate true client_secure_renegotiate true"
$ export ERL_FLAGS
$ erl -sname ssl_test
Erlang (BEAM) emulator version 5.0 [source]

Eshell V5.0  (abort with ^G)
(ssl_test@myhost)1> init:get_arguments().
[{root,["/usr/local/erlang"]},
 {progname,["erl "]},
 {sname,["ssl_test"]},
 {boot,["/home/me/ssl/start_ssl"]},
 {proto_dist,["inet_tls"]},
 {ssl_dist_opt,["server_certfile","/home/me/ssl/erlserver.pem"]},
 {ssl_dist_opt,["server_secure_renegotiate","true",
                "client_secure_renegotiate","true"]
 {home,["/home/me"]}]
```

The `init:get_arguments()` call verifies that the correct arguments are supplied
to the emulator.

## Using TLS distribution over IPv6

It is possible to use TLS distribution over IPv6 instead of IPv4. To do this,
pass the option `-proto_dist inet6_tls` instead of `-proto_dist inet_tls` when
starting Erlang, either on the command line or in the `ERL_FLAGS` environment
variable.

An example command line with this option would look like this:

```text
$ erl -boot /home/me/ssl/start_ssl -proto_dist inet6_tls
  -ssl_dist_optfile "/home/me/ssl/ssl_test@myhost.conf"
  -sname ssl_test
```

A node started in this way will only be able to communicate with other nodes
using TLS distribution over IPv6.
