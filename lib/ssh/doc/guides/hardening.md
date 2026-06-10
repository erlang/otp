<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2023-2026. All Rights Reserved.

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
# Hardening

The Erlang/OTP SSH application is intended to be used in other applications as a
library.

Ensure the Erlang VM runs as a non-root OS user. All SSH services (shell,
exec, SFTP) inherit the OS-level rights of the VM process. See
[Terminology](terminology.md) for details on the rights model.

Different applications using this library may have very different requirements.
One application could be running on a high performance server, while another is
running on a small device with very limited cpu capacity. For example, the first
one may accept many users simultaneously logged in, while the second one wants
to limit them to only one.

That simple example shows that it is impossible to deliver the SSH application
with default values on hardening options as well on other options that suit
every need.

The purpose of this guide is to discuss the different hardening options
available, as a guide to the reader. Configuration in general is described in
the [Configuration in SSH](configurations.md) chapter.

## DoS Resilience (Server)

[](){: #resilience-to-dos-attacks }

The following applies to daemons (servers).

DoS (Denial of Service) attacks are hard to fight at the node level. Here are
firewalls and other means needed, but that is out of scope for this guide.
However, some measures could be taken in the configuration of the SSH server to
increase the resilence. The options to use are:

### Counters and Parallelism

- **[max_sessions](`m:ssh#hardening_daemon_options-max_sessions`)** - The
  maximum number of simultaneous sessions that are accepted at any time for this
  daemon. This includes sessions that are being authenticated. The default is that
  an unlimited number of simultaneous sessions are allowed. It is a good
  candidate to set if the capacity of the server is low or a capacity margin is
  needed.

- **[max_channels](`m:ssh#hardening_daemon_options-max_channels`)** - The
  maximum number of channels that are accepted for each connection. The default
  is unlimited.

- **[parallel_login](`m:ssh#hardening_daemon_options-parallel_login`)** - If set
  to false (the default value), only one login is handled at a time. If set to
  true, the number of simultaneous login attempts are limited by the value of
  the [max_sessions](`m:ssh#hardening_daemon_options-max_sessions`) option.

### Timeouts

- **[hello_timeout](`t:ssh:hello_timeout_daemon_option/0`)** - If the client
  fails to send the first ssh message after a tcp connection setup within this
  time (in milliseconds), the connection is closed. The default value is 30
  seconds. This is actually a generous time, so it can lowered to make the
  daemon less prone to DoS attacks.

- **[negotiation_timeout](`t:ssh:negotiation_timeout_daemon_option/0`)** -
  Maximum time in milliseconds for the authentication negotiation counted from
  the TCP connection establishment. If the client fails to log in within this
  time the connection is closed. The default value is 2 minutes. It is quite a
  long time, but can lowered if the client is supposed to be fast like if it is
  a program logging in.

- **[idle_time](`t:ssh:max_idle_time_common_option/0`)** - Sets a time-out on a
  connection when no channels are left after closing the final one. It defaults
  to infinity.

- **[max_initial_idle_time](`t:ssh:max_initial_idle_time_daemon_option/0`)** -
  Sets a time-out on a connection that will expire if no channel is opened on
  the connection. The timeout is started when the authentication phase is
  completed. It defaults to infinity.

- **[alive](`t:ssh:alive_common_option/0`)** -
  Sets the interval and the maximum number of alive messages that may be sent without
  receiving any message back. Alive messages are typically used to detect that a connection
  became unresponsive.

The following table clarifies when a timeout is started and when it triggers:

| # | Event | Timeout started | Timeout ended |
|---|-------|-----------------|---------------|
| 1 | TCP connected | `hello_timeout`, `negotiation_timeout` | |
| 2 | First SSH message received | | `hello_timeout` |
| 3 | Key Exchange finished | | |
| 4 | Authenticated | `max_initial_idle_time` | `negotiation_timeout` |
| 5 | Channel 1 opened | | `max_initial_idle_time` |
| 6 | Channel *n* opened | | |
| 7 | Channel *x_1* closed | | |
| 8 | Channel *x_n* closed (all channels closed) | `idle_time` | |
| 9 | Connection closed | | `idle_time` |

### Compression

SSH supports compression of the data stream.

Reasonable finite [max_sessions](`m:ssh#hardening_daemon_options-max_sessions`)
option is highly recommended if compression is used to prevent excessive resource
usage by the compression library.
See [Counters and parallelism](#counters-and-parallelism).

The `'zlib@openssh.com'` algorithm is recommended because it only activates
after successful authentication.

The `'zlib'` algorithm (deprecated in SSH and it's usage is scheduled for removal in OTP 30.0)
is not recommended because it activates before authentication completes,
allowing unauthenticated clients to expose potential
vulnerabilities in compression libraries, and increases attack surface of
compression-based side-channel and traffic-analysis attacks.

In both algorithms decompression is protected by a size limit that prevents
excessive memory consumption.

## Reducing Attack Surface

### Shell and Exec Services

A daemon has two services for evaluating tasks on behalf of a remote client. The
_exec_ server-side service takes a string provided by the client, evaluates it
and returns the result. The _shell_ function enables the client to open a shell
in the shell host.

The options [exec](`t:ssh:exec_daemon_option/0`) and
[shell](`t:ssh:shell_daemon_option/0`) are disabled per default.
The same options could also install handlers for
the string(s) passed from the client to the server.

### SFTP Subsystem

The SFTP subsystem is not enabled by default. When enabled, SFTP provides
access to the file system with the rights of the OS process running the
Erlang emulator, regardless of the authenticated SSH user. See the
[Terminology](terminology.md) section for details.

The [subsystems](`t:ssh:subsystem_daemon_option/0`) option controls which
subsystems are available. To enable SFTP:

```erlang
ssh:daemon({192, 168, 1, 10}, Port,
           [{subsystems, [ssh_sftpd:subsystem_spec([])]} | Options]).
```

**Root directory isolation**

The `root` option (see `m:ssh_sftpd`) restricts SFTP users to a
specific directory tree, preventing access to files outside that directory.

```erlang
ssh:daemon(Port, [
    {subsystems, [ssh_sftpd:subsystem_spec([{root, "/home/sftpuser"}])]},
    ...
]).
```

The `root` option is configured per daemon, not per user. All
users connecting to the same daemon share the same root directory. For per-user
isolation, consider running separate daemon instances on different ports or
using OS-level mechanisms (PAM chroot, containers, file permissions).

For high-security deployments, combine the `root` option
with OS-level isolation mechanisms such as chroot jails, containers, or
mandatory access control (SELinux, AppArmor).

**Resource limits**

When enabling the SFTP subsystem via `ssh_sftpd:subsystem_spec/1`, additional
resource limits can be configured to protect against resource exhaustion attacks:

`max_handles` limits the maximum number of file and directory handles that can
be opened simultaneously per SFTP connection. The default is 1000.

Recommended values by deployment type:

- **High-security/restricted environments**: 100-256
  - Minimal attack surface
  - Suitable for simple file transfers
  - May impact batch operations

- **Standard production**: 500-1000
  - Balances security and functionality
  - Supports most legitimate use cases
  - Recommended for general deployments
  - Note: The default value is 1000

- **High-throughput automation**: 1000-2000
  - For backup systems, CI/CD pipelines
  - Parallel file operations
  - Monitor actual usage before increasing

`max_path` limits the maximum path length accepted from SFTP clients. The
default is 4096 bytes.

Recommended values:

- **Standard**: 4096 (default)
  - Accommodates most filesystem limits
  - Linux: typically 4096 bytes
  - Windows: 260 characters (legacy), 32767 (extended)

- **Restricted environments**: 1024-2048
  - If application uses shorter paths
  - Additional defense layer
  - Verify compatibility first

`max_files` limits the number of filenames returned per READDIR request. The
default is 0 (unlimited).

This option prevents memory exhaustion from large directory listings. Unlike
max_handles and max_path, this is primarily a performance/memory protection
rather than security mitigation.

Recommended values:

- **Standard**: 0 (unlimited, default)
  - No artificial restrictions
  - Client handles pagination

- **Large directories**: 1000-10000
  - Prevents memory spikes
  - Improves response time

- **Memory-constrained systems**: 100-1000
  - Embedded systems
  - Resource-limited containers

**Example configuration**

```erlang
ssh:daemon(Port, [
    {system_dir, "/etc/ssh"},
    {subsystems, [
        ssh_sftpd:subsystem_spec([
            {root, "/sftp/chroot"},
            {max_handles, 256},
            {max_path, 4096},
            {max_files, 1000}
        ])
    ]},
    {max_sessions, 10}
]).
```

See `m:ssh_sftpd` for complete documentation of subsystem options.

### The ID String

One way to reduce the risk of intrusion is to not convey which software and
which version the intruder is connected to. This limits the risk of an intruder
exploiting known faults or peculiarities learned by reading the public code.

Each SSH client or daemon presents themselves to each other with brand and
version. This may look like

```text
SSH-2.0-Erlang/4.10
```

or

```text
SSH-2.0-OpenSSH_7.6p1 Ubuntu-4ubuntu0.3
```

This brand and version may be changed with the option
[id_string](`t:ssh:id_string_common_option/0`). We start a daemon with that
option:

```erlang
        ssh:daemon({192, 168, 1, 10}, 1234, [{id_string,"hi there"}, ... ]).
```

and the daemon will present itself as:

```text
SSH-2.0-hi there
```

It is possible to replace the string with one randomly generated for each
connection attempt. See the reference manual for
[id_string](`t:ssh:id_string_common_option/0`).

## SFTP symlink risks

Symbolic links inside the SFTP root that point outside it can be followed by
file operations such as `open`, `read`, and `stat`.  The `root` option
confines path resolution done by the daemon itself, but it does not prevent
the operating system from following symlinks when the daemon opens the
resulting path.

Symlinks created by SFTP clients are not exploitable this way: the daemon
converts the target to an absolute path and clamps it to the configured root
before writing the link.  However, symlinks that already exist inside the root
directory, or are created through other access channels (shell access, another
service, etc.), can point outside the root and will be followed.

Mitigations:

- **Filesystem permissions** - Run the SSH daemon as a dedicated user with
  minimal privileges.  Ensure that user cannot read or write sensitive files
  outside the root.

- **Control the root contents** - Ensure no pre-existing symlinks in the root
  point outside it, and that no other process can create such links.

- **OS-level protections** - On Linux, mount the root directory with the
  `nosymfollow` mount option, or use filesystem namespaces to prevent
  symlink traversal outside the root.

## Client Connection Options

A client could limit the time for the initial tcp connection establishment with
the option [connect_timeout](`t:ssh:connect_timeout_client_option/0`). The time
is in milliseconds, and the initial value is infinity.

The negotiation (session setup time) time can be limited with the _parameter_
`NegotiationTimeout` in a call establishing an ssh session, for example
`ssh:connect/3`.

## Network-Level Security

### IP Binding Restrictions

`ssh:daemon/1` and `ssh:daemon/2` bind to **all network interfaces** by
default. For hardened deployments, use `ssh:daemon/3` with an explicit
IP address or `loopback`:

```erlang
ssh:daemon({192, 168, 1, 10}, 2222, Options).  % Specific interface
ssh:daemon(loopback, 2222, Options).            % Localhost only
```

**Note**: In the examples above, `HostAddress` (1st argument) takes precedence
over a potentially provided `{ip, Address}` in `Options` (3rd argument).

## Advanced Authentication

The following techniques provide enhanced authentication controls using
custom callbacks. These require implementation specific to your environment.

### Public Key Validation

Use a custom [`key_cb`](`t:ssh:key_cb_common_option/0`) module implementing
the `m:ssh_server_key_api` behaviour. The `is_auth_key` callback can enforce
client key strength requirements (e.g. reject RSA keys shorter than 2048 bits)
and log key usage for auditing. Enable
[`pk_check_user`](`m:ssh#option-pk_check_user`) to verify that the username
is known before accepting public key authentication.

```erlang
ssh:daemon({192, 168, 1, 10}, Port, [
    {key_cb, {my_key_handler, []}},
    {pk_check_user, true}
]).
```

### Account Lockout Policies

Use [`pwdfun`](`t:ssh:pwdfun_4/0`) with an ETS table to track failed
attempts across connections and lock accounts after repeated failures.
Return `disconnect` when the account is locked — this immediately
terminates the connection with `SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE`:

> #### Note {: .info }
>
> The lockout example below is conceptual. With
> [`parallel_login`](`m:ssh#hardening_daemon_options-parallel_login`) enabled,
> race conditions may reduce lockout accuracy.

```erlang
lockout_pwdfun(User, Password, _PeerAddr, State) ->
    case ets:lookup(ssh_lockouts, {locked, User}) of
        [_] ->
            disconnect;
        [] ->
            case validate_password(User, Password) of
                true ->
                    ets:delete(ssh_lockouts, {attempts, User}),
                    {true, State};
                false ->
                    N = ets:update_counter(ssh_lockouts,
                            {attempts, User}, 1,
                            {{attempts, User}, 0}),
                    case N >= ?LOCKOUT_THRESHOLD of
                        true ->
                            ets:insert(ssh_lockouts,
                                {{locked, User}, true}),
                            ets:delete(ssh_lockouts,
                                {attempts, User});
                        false ->
                            ok
                    end,
                    {false, State}
            end
    end.
```
