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
# SSH Release Notes

## Ssh 5.1.2

### Fixed Bugs and Malfunctions

* With this change, Curve25519 and Curve448 KEX methods become most preferred (related to RFC8731).

  Own Id: OTP-18964

## Ssh 5.1.1

### Fixed Bugs and Malfunctions

- With this change (being response to CVE-2023-48795), ssh can negotiate "strict
  KEX" OpenSSH extension with peers supporting it; also
  'chacha20-poly1305@openssh.com' algorithm becomes a less preferred cipher.

  If strict KEX availability cannot be ensured on both connection sides,
  affected encryption modes(CHACHA and CBC) can be disabled with standard ssh
  configuration. This will provide protection against vulnerability, but at a
  cost of affecting interoperability. See
  [Configuring algorithms in SSH](configure_algos.md).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18897

## Ssh 5.1

### Fixed Bugs and Malfunctions

- Replaced unintentional Erlang Public License 1.1 headers in some files with
  the intended Apache License 2.0 header.

  Own Id: OTP-18815 Aux Id: PR-7780

- Avoid outputting ansi escape sequences to dumb ssh clients.

  Own Id: OTP-18861 Aux Id: PR-7627

- With this change, connection handler does not execute socket operations until
  it becomes socket owner. Previously errors could occur if connection handler
  tried to work with socket whose owner exited.

  Own Id: OTP-18869 Aux Id: PR-7849,GH-7571

### Improvements and New Features

- With this change, reverse search works with ssh shell and non dumb terminals.

  Own Id: OTP-18730 Aux Id: PR-7499

## Ssh 5.0.1

### Fixed Bugs and Malfunctions

- Added multiline editing support to ssh clients connected through OTP ssh
  daemon.

  Own Id: OTP-18653 Aux Id: PR-7242

## Ssh 5.0

### Improvements and New Features

- The ssh_cli has been updated to work with the changes introduced in the new
  Erlang shell implementation.

  Own Id: OTP-18231 Aux Id: OTP-17932 PR-6144

- Typing `Ctrl+L` in a shell now clears the screen and redraws the current line
  instead of only redrawing the current line. To only redraw the current line,
  you must now type `Alt+L`. This brings the behaviour of `Ctrl+L` closer to how
  bash and other shells work.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18285 Aux Id: PR-6262

- Deprecates `dbg:stop_clear/0` because it is simply a function alias to
  `dbg:stop/0`

  Own Id: OTP-18478 Aux Id: GH-6903

- The implementation has been fixed to use `proc_lib:init_fail/2,3` where
  appropriate, instead of `proc_lib:init_ack/1,2`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18490 Aux Id: OTP-18471, GH-6339, PR-6843

## Ssh 4.15.3.2

### Fixed Bugs and Malfunctions

* With this change, Curve25519 and Curve448 KEX methods become most preferred (related to RFC8731).

  Own Id: OTP-18964

## Ssh 4.15.3.1

### Fixed Bugs and Malfunctions

- With this change, connection handler does not execute socket operations until
  it becomes socket owner. Previously errors could occur if connection handler
  tried to work with socket whose owner exited.

  Own Id: OTP-18869 Aux Id: PR-7849,GH-7571

- With this change (being response to CVE-2023-48795), ssh can negotiate "strict
  KEX" OpenSSH extension with peers supporting it; also
  'chacha20-poly1305@openssh.com' algorithm becomes a less preferred cipher.

  If strict KEX availability cannot be ensured on both connection sides,
  affected encryption modes(CHACHA and CBC) can be disabled with standard ssh
  configuration. This will provide protection against vulnerability, but at a
  cost of affecting interoperability. See
  [Configuring algorithms in SSH](configure_algos.md).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18897

## Ssh 4.15.3

### Fixed Bugs and Malfunctions

- With this change, PKCS8 formatted private key file is properly decoded and SSH
  daemon with such key can be started.

  Own Id: OTP-18446 Aux Id: GH-6475

### Improvements and New Features

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

## Ssh 4.15.2

### Fixed Bugs and Malfunctions

- With this change, ssh application does not crash when formatting some of info
  reports for unsuccessful connections.

  Own Id: OTP-18386 Aux Id: PR-6611

- With this change, ssh does not log extensively long messages.

  Own Id: OTP-18417 Aux Id: DAFH-1349,ERIERL-888,IA18357

## Ssh 4.15.1

### Fixed Bugs and Malfunctions

- graceful shutdown of ssh_conection_handler when connection is closed by peer

  Own Id: OTP-18326 Aux Id: ERIERL-865

## Ssh 4.15

### Fixed Bugs and Malfunctions

- Handling rare race condition at channel close.

  Own Id: OTP-18220 Aux Id: ERIERL-666, ERIERL-661

### Improvements and New Features

- New ssh option `no_auth_needed` to skip the ssh authentication. Use with
  caution\!

  Own Id: OTP-18134 Aux Id: GH-6021

- This change fixes dialyzer warnings generated for inets/httpd examples
  (includes needed adjustment of spec for ssh_sftp module).

  Own Id: OTP-18178 Aux Id: ERIERL-833, ERIERL-834, ERIERL-835

- The new function `ssh:daemon_replace_options/2` makes it possible to change
  the `Options` in a running SSH server.

  Established connections are not affected, only those created after the call to
  this new function.

  Own Id: OTP-18196

- Add a timeout as option `max_initial_idle_time`. It closes a connection that
  does not allocate a channel within the timeout time.

  For more information about timeouts, see the
  [Timeouts section ](hardening.md#timeouts)in the User's Guide
  [Hardening](hardening.md) chapter.

  Own Id: OTP-18207 Aux Id: PR-6231

## Ssh 4.14.1

### Fixed Bugs and Malfunctions

- Binaries can be limited in logs with the parameter `max_log_item_len`. The
  default value is 500 bytes.

  Own Id: OTP-18094

## Ssh 4.14

### Improvements and New Features

- The representation of Edward curves (ed25519 and ed448) inside ssh had a
  temporary representation (ed_pri and ed_pub).

  That is now changed to the public_key form. See the manual for more
  information.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17920

- Former internal function `ssh_file:extract_public_key/1` documented publicly.

  Internally it was previously in ssh_transport.

  Own Id: OTP-18079 Aux Id: GH-5767

## Ssh 4.13.2.5

### Fixed Bugs and Malfunctions

* With this change, Curve25519 and Curve448 KEX methods become most preferred (related to RFC8731).

  Own Id: OTP-18964

## Ssh 4.13.2.4

### Fixed Bugs and Malfunctions

- With this change, connection handler does not execute socket operations until
  it becomes socket owner. Previously errors could occur if connection handler
  tried to work with socket whose owner exited.

  Own Id: OTP-18869 Aux Id: PR-7849,GH-7571

- With this change (being response to CVE-2023-48795), ssh can negotiate "strict
  KEX" OpenSSH extension with peers supporting it; also
  'chacha20-poly1305@openssh.com' algorithm becomes a less preferred cipher.

  If strict KEX availability cannot be ensured on both connection sides,
  affected encryption modes(CHACHA and CBC) can be disabled with standard ssh
  configuration. This will provide protection against vulnerability, but at a
  cost of affecting interoperability. See
  [Configuring algorithms in SSH](configure_algos.md).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18897

## Ssh 4.13.2.3

### Fixed Bugs and Malfunctions

- With this change, error logging related crashes in ssh_connection_handler
  module are fixed.

  Own Id: OTP-18620 Aux Id: OTP-18386,PR-6611

## Ssh 4.13.2.2

### Fixed Bugs and Malfunctions

- With this change, ssh application does not crash when formatting some of info
  reports for unsuccessful connections.

  Own Id: OTP-18386 Aux Id: PR-6611

- With this change, ssh does not log extensively long messages.

  Own Id: OTP-18417 Aux Id: DAFH-1349,ERIERL-888,IA18357

## Ssh 4.13.2.1

### Fixed Bugs and Malfunctions

- Binaries can be limited in logs with the parameter `max_log_item_len`. The
  default value is 500 bytes.

  Own Id: OTP-18094

## Ssh 4.13.2

### Fixed Bugs and Malfunctions

- Fix makefile dependency bugs.

  Own Id: OTP-17847 Aux Id: PR-5574 GH-5548

- Fixed faulty OpenSSH decoding of Ed25519/Ed448 keys in the OpenSSH format
  `openssh_key_v1`.

  Own Id: OTP-17868 Aux Id: PR-5520

- Correction of ssh_file typing, specially for the experimental openssh-key-v1
  encoding.

  Own Id: OTP-17912 Aux Id: GH-5680

- Improper tag for private ED keys when encoding with ssh:encode/2.

  The tuple had `ed_priv` as first element, but should have had `ed_pri`. This
  is now corrected.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17928 Aux Id: PR-5679

### Improvements and New Features

- Add support for Ed25519/Ed448 SSH host keys in the RFC 4716 format
  ("`-----BEGIN EC PRIVATE KEY-----`") generated by for example openssl or via
  Erlang functions (i.e. `public_key:generate_key({namedCurve, ed25519})`).

  Ed25519 SSH host keys generated by `ssh-keygen` was, and are still, supported.

  Own Id: OTP-17857 Aux Id: PR-5532

## Ssh 4.13.1

### Fixed Bugs and Malfunctions

- The ssh sever `parallel_login` option was missing in OTP-24

  Own Id: OTP-17850 Aux Id: ERIERL-764

## Ssh 4.13

### Fixed Bugs and Malfunctions

- The value of the `connect_timeout` option is now used as default value for the
  negotiation timeout.

  Own Id: OTP-17707 Aux Id: ERIERL-706

### Improvements and New Features

- Add better error handling in connect/2,3,4. Detect incorrect arguments and
  return an informative error tuple instead of throwing a function_clause or
  similar.

  Own Id: OTP-17515 Aux Id: ERIERL-648

- Make ssh algorithm selection better handle dynamic changes changes in crypto
  fips mode.

  Own Id: OTP-17795

## Ssh 4.12.5

### Fixed Bugs and Malfunctions

- Fixed a race condition in the acceptor loop: if a client disconnected
  immediately after the tcp connect, the server could cease handling connection
  on that address:port.

  Own Id: OTP-17764 Aux Id: ERIERL-726

## Ssh 4.12.4

### Fixed Bugs and Malfunctions

- Fixed that a slow start (>30s) of a client subsystem could cause a log entry
  with the password.

  Own Id: OTP-17390 Aux Id: ERIERL-648

- Fixed an error when running as an sftp server and a client requests a
  directory contents listing.

  The fix is to handle the error code `{error, eacces}` as `{error, enoent}` in
  the `ssh_sftpd:get_attrs/5` internal function; that is, just skip it.

  Own Id: OTP-17586 Aux Id: GH-5014

### Improvements and New Features

- The "Key exchange failed" Info Report is now more informative.

  Own Id: OTP-17450 Aux Id: ERIERL-655

## Ssh 4.12.3

### Fixed Bugs and Malfunctions

- Filter out sensitive data (passwords etc) from progress reports and supervisor
  reports.

  Own Id: OTP-17468 Aux Id: ERIERL-656

## Ssh 4.12.2

### Fixed Bugs and Malfunctions

- Avoid an extra blank line in the ssh known_hosts file

  Own Id: OTP-17427

## Ssh 4.12.1

### Improvements and New Features

- Add missing `known_hosts` and `authorized_keys` file types to
  `ssh_file:decode/2` and `ssh_file:encode/2`.

  Own Id: OTP-17397

## Ssh 4.12

### Fixed Bugs and Malfunctions

- Missing runtime dependencies has been added to this application.

  Own Id: OTP-17243 Aux Id: PR-4557

- The send window handling is changed to not initialize a too large window on
  some occasions.

  Own Id: OTP-17353

### Improvements and New Features

- Removed usage of `erlang:is_port/1` from the SSH implementation.

  Own Id: OTP-16750

- Internal connection setup refactoring.

  Own Id: OTP-17051

- Refactor SSH fsm into a (hopefully) more comprehensible set of gen_statem
  callback-files.

  Own Id: OTP-17140

- The RSA SHA1 sign/verify variants are disabled by default. That is, ssh-rsa is
  disabled by default as well as the SHA1 sign/verify with RSA keys from id_rsa
  and ssh_host_rsa_key. All SHA2 sign/verify are enabled by default.

  The reason is that SHA1 is now considered easy to break.

  To enable RSA with SHA1, for example for a very old and unsafe peer, see
  [Example 9](configure_algos.md#example-9) in the User's Guide chapter
  [Configuring algorithms in SSH](configure_algos.md).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17259 Aux Id: OTP-16511, ERIERL-619

- Adapt ssh supervisors to the new 'significant' and 'auto_shutdown' flags in
  supervisor.

  Own Id: OTP-17322 Aux Id: PR-4638, EEP-56, OTP-17334

- The functions public_key:ssh_encode/2, public_key:ssh_decode/2,
  public_key:ssh_hostkey_fingerprint/1 and public_key:ssh_hostkey_fingerprint/2
  are deprecated.

  Replacement functions are available in SSH, see the
  [Deprecations](`e:general_info:deprecations.md#otp-24`) chapter in the
  Erlang/OTP documentation.

  Own Id: OTP-17352

## Ssh 4.11.1.6

### Fixed Bugs and Malfunctions

- Binaries can be limited in logs with the parameter `max_log_item_len`. The
  default value is 500 bytes.

  Own Id: OTP-18094

## Ssh 4.11.1.5

### Fixed Bugs and Malfunctions

- Fixed problem with blocked server after multiple restarts. Applies to daemons
  with options `{parallel_login,true}` and also `{max_sessions, int()>0}`.

  Own Id: OTP-17835 Aux Id: ERIERL-721

## Ssh 4.11.1.4

### Fixed Bugs and Malfunctions

- The value of the `connect_timeout` option is now used as default value for the
  negotiation timeout.

  Own Id: OTP-17707 Aux Id: ERIERL-706

## Ssh 4.11.1.3

### Fixed Bugs and Malfunctions

- Filter out sensitive data (passwords etc) from progress reports and supervisor
  reports.

  Own Id: OTP-17468 Aux Id: ERIERL-656

### Improvements and New Features

- The "Key exchange failed" Info Report is now more informative.

  Own Id: OTP-17450 Aux Id: ERIERL-655

## Ssh 4.11.1.2

### Fixed Bugs and Malfunctions

- Avoid an extra blank line in the ssh known_hosts file

  Own Id: OTP-17427

## Ssh 4.11.1.1

### Fixed Bugs and Malfunctions

- Fixed that a slow start (>30s) of a client subsystem could cause a log entry
  with the password.

  Own Id: OTP-17390 Aux Id: ERIERL-648

## Ssh 4.11.1

### Fixed Bugs and Malfunctions

- The idle_time timer was not cancelled when a channel was opened within the
  timeout time on an empty connection that have had channels previously.

  Own Id: OTP-17279

## Ssh 4.11

### Improvements and New Features

- The long name field in SSH_FXP_NAME responses to display file information in
  sftp version 3 now contains the expanded format defined in the sftp draft. It
  is similar to what is returned by "ls -l" on Unix systems.

  Own Id: OTP-17197 Aux Id: PR- 3049

## Ssh 4.10.8

### Fixed Bugs and Malfunctions

- Don't timeout slow connection setups and tear-downs. A rare crash risk for the
  controller is also removed.

  Own Id: OTP-17173 Aux Id: ERIERL-581

## Ssh 4.10.7

### Fixed Bugs and Malfunctions

- The SSH daemon erroneously replaced LF with CRLF also when there was no pty
  requested from the server.

  Own Id: OTP-17108 Aux Id: ERL-1442

## Ssh 4.10.6

### Fixed Bugs and Malfunctions

- Fixed problems in the ssh cli/shell handling. Most important are:

  1. the ssh:shell function did sometimes cause the input to be echoed twice,

  2. the ssh:shell function didn't transfer the LANG and LC_ALL shell variables
     to the connected server which sometimes made Unicode handling erroneous,

  3. Unicode was not always transferred correctly to and from the peer.

  Own Id: OTP-16799

- The SSH protocol message SSH_MSG_DISCONNECT was sometimes sent instead of
  SSH_MSG_CHANNEL_FAILURE

  Own Id: OTP-16900

- The ssh_cli module now always sends the exit-status to connected clients so
  they can use that to check for successful command execution.

  Own Id: OTP-16908 Aux Id: PR-2753

### Improvements and New Features

- A new option [`pk_check_user`](`m:ssh#option-pk_check_user`) enables checking
  of the client's user name in the server when doing public key authentication.

  Own Id: OTP-16889

## Ssh 4.10.5

### Fixed Bugs and Malfunctions

- An ssh-client can take an accepted socket from a listening socket and do an
  ssh:connect/2 on it.

  Multiple clients on sockets accepted from the same listening socket had
  stopped working. This is corrected now.

  Own Id: OTP-17021 Aux Id: ERIERL-567

## Ssh 4.10.4.1

### Fixed Bugs and Malfunctions

- Filter out sensitive data (passwords etc) from progress reports and supervisor
  reports.

  Own Id: OTP-17468 Aux Id: ERIERL-656

## Ssh 4.10.4

### Fixed Bugs and Malfunctions

- The inet option raw was not passed on from the ssh option list to inet.

  Own Id: OTP-17016 Aux Id: ERIERL-562

## Ssh 4.10.3

### Fixed Bugs and Malfunctions

- A supervisor sub-tree could be left if the connection handler process is
  brutally killed. This will make the max_sessions checking option to count the
  existing sessions erroneously and could finally block further sessions.

  Own Id: OTP-17006 Aux Id: ERIERL-556

## Ssh 4.10.2

### Fixed Bugs and Malfunctions

- Fix decoder bug.

  Own Id: OTP-16904

## Ssh 4.10.1

### Fixed Bugs and Malfunctions

- Fixed a bug when a message to ssh-agent was divided into separate packets.

  Own Id: OTP-16761 Aux Id: PR-2679

- Fix a bug that could crash the cli server if a too large cli-window was
  requested from the client.

  Own Id: OTP-16791 Aux Id: ERIERL-520

### Improvements and New Features

- Increased test coverage.

  Own Id: OTP-14106

- A chapter about [hardening the OTP SSH](hardening.md) is added to the User's
  Guide.

  Own Id: OTP-16411

- The internal Diffie-Hellman high level API for key generation was slow in old
  and by OpenSSL now unsupported cryptolib versions (1.0.1 and earlier).

  If such a cryptolib is used anyhow, the low-level API is used internally in
  the crypto application.

  Own Id: OTP-16774

- A new timeout is defined for daemons:
  [hello_timeout](`t:ssh:hello_timeout_daemon_option/0`).

  The timeout is supposed to be used as a simple
  [DoS attack protection](hardening.md#resilience-to-dos-attacks). It closes an
  incoming TCP-connection if no valid first SSH message is received from the
  client within the timeout limit after the TCP initial connection setup.

  The initial value is 30s by compatibility reasons, but could be lowered if
  needed, for example in the code or in a [config file](configurations.md).

  Own Id: OTP-16803

## Ssh 4.10

### Fixed Bugs and Malfunctions

- Fix error in ssh_sftpd typespec.

  Own Id: OTP-16363

### Improvements and New Features

- The plug-in file ssh_file.erl, that is responsible for default file handling,
  is re-factored, optimized and re-written.

  Own Id: OTP-11688 Aux Id: OTP-12699

- OpenSSH 6.5 introduced a new file representation of keys called
  [openssh-key-v1](https://cvsweb.openbsd.org/src/usr.bin/ssh/PROTOCOL.key?annotate=1.1).

  OTP/SSH had an experimental implementation of this format. That implementation
  is now improved and supported with the exception of handling encrypted keys.

  Own Id: OTP-15434

- TCP/IP port forwarding, a.k.a tunneling a.k.a tcp-forward/direct-tcp is
  implemented. In the OpenSSH client, this corresponds to the options -L and -R.

  The client or server listens to a specified socket, and when something
  connects to it with TCP/IP, that connection is forwarded in an encrypted
  tunnel to the peer. The peer then connects to a predefined IP/port pair and
  then acts as a proxy.

  See the manual, `ssh:tcpip_tunnel_to_server/6` and
  `ssh:tcpip_tunnel_from_server/6`.

  The functionality is disabled per default but can be enabled when starting a
  daemon.

  Own Id: OTP-15998 Aux Id: PR-2376, PR-2368

- The client-side of the supervisor tree (under sshc_sup) was previously not
  complete; the channel handling processes were handled with links but had no
  supervisors.

  This is now corrected with a client-side supervisor tree under `sshc_sup`,
  similar to the server-side supervisor tree under `sshd_sup`.

  Own Id: OTP-16026 Aux Id: PR-2368, (OTP-15998)

- The extension
  [posix-rename@openssh.com](https://cvsweb.openbsd.org/src/usr.bin/ssh/PROTOCOL?annotate=HEAD)
  is added to the [ssh/sftp rename](`ssh_sftp:rename/3`) operation.

  Own Id: OTP-16289 Aux Id: PR-2448

- Calls of deprecated functions in the
  [Old Crypto API](`e:crypto:new_api.md#the-old-api`) are replaced by calls of
  their [substitutions](`e:crypto:new_api.md#the-new-api`).

  Own Id: OTP-16346

- The default known_hosts file handling is improved to include ports.

  The handling of the contents in that file is updated to support the
  [full syntax](https://man.openbsd.org/sshd#SSH_KNOWN_HOSTS_FILE_FORMAT), with
  exception of 1) the wildcard '?', 2) wildcards in canonical names and 3) the
  option '@cert-authority'

  Own Id: OTP-16506

- The MAC (Message Authorization Code) algorithms

  - hmac-sha1-etm@openssh.com
  - hmac-sha2-256-etm@openssh.com
  - hmac-sha2-512-etm@openssh.com

  are implemented.

  Own Id: OTP-16508

- The key-exchange algorithms `'diffie-hellman-group14-sha1'` and
  `'diffie-hellman-group-exchange-sha1'` are disabled per default. The reason is
  that SHA1 now is considered insecure.

  They can be enabled if needed, see [SSH (App)](ssh_app.md#algorithms).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16509

- The public key algorithm `'ssh-dss'` is disabled per default. The reason is
  that it is now considered as insecure.

  It can be enabled if needed, see [SSH (App)](ssh_app.md#algorithms).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16510

- The public key `'ssh-rsa'` is now considered as insecure because of its usage
  of SHA1.

  It is therefore deprecated and will no longer be enabled per default in
  OTP-24.0.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16511

- An option [optimize (optimize_key_lookup)](`t:ssh_file:optimize_key_lookup/0`)
  is introduced for the file interface ssh_file.erl

  The option enables the user to select between the default handling which is
  fast but memory consuming vs memory efficient but not as fast. The effect
  might be observable only for large files.

  See the manual for `ssh_file:is_host_key/5` and `ssh_file:is_auth_key/3`.

  Own Id: OTP-16512

- The ssh agent is now implemented in the ssh_agent key callback module.

  Enable with the the option `{key_cb, {ssh_agent, []}}` in for example
  ssh:connect/3.

  See the [ssh_agent manual](`m:ssh_agent`) for details.

  Own Id: OTP-16513

- Algorithm configuration could now be done in a .config file.

  This is useful for example to enable an algorithm that is disabled by default.
  It could now be enabled in an .config-file without changing the code,

  See the SSH User's Guide chapter ["Configuration in SSH"](configurations.md).

  Own Id: OTP-16540

- Documented which gen_tcp socket options can't be used in calls to ssh:connect
  and ssh:daemon.

  Own Id: OTP-16589

- Added [kb_int_fun_4()](`t:ssh:kb_int_fun_4/0`) to the
  [authentication_daemon_options()](`t:ssh:authentication_daemon_options/0`) to
  enable generating dynamic keyboard-interactive prompts from the user's state
  returned from the authentication fun [pwdfun_4()](`t:ssh:pwdfun_4/0`).

  Own Id: OTP-16622 Aux Id: PR-2604

## Ssh 4.9.1.4

### Fixed Bugs and Malfunctions

- The value of the `connect_timeout` option is now used as default value for the
  negotiation timeout.

  Own Id: OTP-17707 Aux Id: ERIERL-706

## Ssh 4.9.1.3

### Fixed Bugs and Malfunctions

- The idle_time timer was not cancelled when a channel was opened within the
  timeout time on an empty connection that have had channels previously.

  Own Id: OTP-17279

## Ssh 4.9.1.2

### Fixed Bugs and Malfunctions

- Fix decoder bug.

  Own Id: OTP-16904

## Ssh 4.9.1.1

### Fixed Bugs and Malfunctions

- Fix a bug that could crash the cli server if a too large cli-window was
  requested from the client.

  Own Id: OTP-16791 Aux Id: ERIERL-520

### Improvements and New Features

- A new timeout is defined for daemons: `hello_timeout`.

  It closes an incoming TCP-connection if no valid 1st message is received from
  the client within the timeout limit.

  Own Id: OTP-16803

## Ssh 4.9.1

### Fixed Bugs and Malfunctions

- Potential hazard between re-keying decision and socket close.

  Own Id: OTP-16462 Aux Id: ERIERL-464

## Ssh 4.9

### Fixed Bugs and Malfunctions

- Unicode problems for ssh_sftp:write fixed.

  Own Id: OTP-16377

### Improvements and New Features

- Changes to the internal api of the experimental ssh_dbg tool.

  Own Id: OTP-16353

- The new functions `ssh:set_sock_opts/2` and `ssh:get_sock_opts/2` sets and
  reads option values for the underlying TCP stream.

  Own Id: OTP-16485

## Ssh 4.8.2

### Fixed Bugs and Malfunctions

- Fixed that `ssh_connection:send` could allocate a large amount of memory if
  given an iolist() as input data.

  Own Id: OTP-16373

- Safe atom conversions.

  Own Id: OTP-16375

- Constant time comparisons added.

  Own Id: OTP-16376

## Ssh 4.8.1

### Fixed Bugs and Malfunctions

- The ssh cli (e.g shell) server behaved strangely when characters were inserted
  in a string such that the last characters tried to wrap the line.

  Own Id: OTP-14849 Aux Id: ERL-545

- If an OTP SSH server was serving an "exec" request and the executed code used
  Erlang `standard_io` for input/output, the I/O was erroneously handled by the
  _server's_ group leader, so the I/O turned up in the the server's Erlang shell
  (if any). The user at the client side did therefore not see that I/O.

  This is corrected now, so the client - for example the ssh OS shell command -
  handles the I/O. The user could send input to the server side exec handling
  code by writing on the terminal, and server side output from for example
  io:format is presented on the terminal - not only the functional result.

  NOTE 1: Servers executing exec requests with the old, undocumented ways of
  specifying the custom exec handler is not changed. Changed are only the two
  cases where the server's 'exec' option either:

  1. is not specified (i.e. using the default shell) or,
  2. it has the `{direct, fun(...) -> ... end}` value format.

  NOTE 2: Previously an end-of-line marker was appended on the result and error
  reports at the client side. They are removed now and the error reports are
  slightly enhanced.

  TECHNICAL DETAILS: The server's device `standard_input` receives data events
  from the exec request's channel, and the device `standard_output` is sending
  its data by data events to the client on that channel. The result is that
  `standard_io` is now performed by the client's group leader.

  Own Id: OTP-15417 Aux Id: OTP-16108

- The functions ssh:shell/1,2,3 left the connection open when they returned.
  That leakage is fixed now.

  Own Id: OTP-16047

- Corrected that an Erlang SSH server could return the status code 4294967295
  instead of 255 on some errors of an exec request.

  Own Id: OTP-16123

### Improvements and New Features

- Internal simplification of ssh_sftp/ssh_xfer

  Own Id: OTP-15972

- The documentation of [One-Time Execution](using_ssh.md#one-time-execution) in
  the User's Guide is updated with more examples.

  Own Id: OTP-16108 Aux Id: OTP-15417

- The new value `'disabled'` is introduced in the SSH daemon options 'exec' and
  'shell'. Previously they lacked a clear way of disabling them.

  Own Id: OTP-16113

- The old algorithms 'aes192_cbc', 'aes256_cbc' and 'hmac-sha1-96' are added for
  compatibility with older peers.

  The mac 'hmac-sha1-96' is nowadays not recommended and must therefore be
  explicitly enabled. Use for example the Option value
  `{modify_algorithms, [{append, [{mac,['hmac-sha1-96']}]}]}`

  Own Id: OTP-16170

## Ssh 4.8

### Fixed Bugs and Malfunctions

- Fixed wrong type definition for the daemon option `subsystems`.

  Own Id: OTP-15820

- Fixed a possible SSH logging crash if there was a problem in an early stage of
  session setup.

  Own Id: OTP-15962 Aux Id: ERL-990

### Improvements and New Features

- The documentation for the modules ssh_connection, ssh_sftp and ssh_sftpd are
  now generated from the -spec:s.

  Own Id: OTP-15395

- Internal cleanup including removal of the internal file `ssh_userauth.hrl`.

  Own Id: OTP-15876 Aux Id: PR-2255, PR-2256

- Removed unused definitions in `ssh.hrl`.

  Own Id: OTP-15929 Aux Id: PR-2297

- Removed unused fields in the internal `#connection{}` record.

  Own Id: OTP-15984

- To get information of a `connection_ref()` from for example `ssh:connect/3`,
  there was previously one function available namely `ssh:connection_info/2`.
  This ticket adds `ssh:connection_info/1` which returns all information.

  For daemons (servers) started with for example `ssh:daemon/2` the function
  `ssh:daemon_info/1` returning all information was available. This ticket adds
  `ssh:daemon_info/2` which returns only the information specified in the second
  argument.

  The info of connections and of daemons now also includes the item '`options`'.
  Only those options that does not have their default values are returned.

  For a connection also the items '`algorithms`' and '`channels`' are added.

  Own Id: OTP-16040

## Ssh 4.7.7

### Improvements and New Features

- SSH uses the new crypto API.

  Own Id: OTP-15673

## Ssh 4.7.6.6

### Fixed Bugs and Malfunctions

- The idle_time timer was not cancelled when a channel was opened within the
  timeout time on an empty connection that have had channels previously.

  Own Id: OTP-17279

## Ssh 4.7.6.5

### Fixed Bugs and Malfunctions

- Fix decoder bug.

  Own Id: OTP-16904

## Ssh 4.7.6.4

### Fixed Bugs and Malfunctions

- Potential hazard between re-keying decision and socket close.

  Own Id: OTP-16462 Aux Id: ERIERL-464

## Ssh 4.7.6.3

### Fixed Bugs and Malfunctions

- Fixed that `ssh_connection:send` could allocate a large amount of memory if
  given an iolist() as input data.

  Own Id: OTP-16373

- Safe atom conversions.

  Own Id: OTP-16375

- Constant time comparisons added.

  Own Id: OTP-16376

## Ssh 4.7.6.2

### Fixed Bugs and Malfunctions

- The ssh cli (e.g shell) server behaved strangely when characters were inserted
  in a string so that the last characters tried to wrap the line.

  Own Id: OTP-14849 Aux Id: ERL-545

## Ssh 4.7.6.1

### Fixed Bugs and Malfunctions

- Fixed a possible SSH logging crash if there was a problem in an early stage of
  session setup.

  Own Id: OTP-15962 Aux Id: ERL-990

## Ssh 4.7.6

### Improvements and New Features

- When an SSH server receives the very first message on a new TCP connection,
  and that message is not the expected one, the 64 first bytes of the received
  message are now dumped in the INFO REPORT that reports the Protocol Error.

  This facilitates the debugging of who sends the bad message or of detecting a
  possible port scanning.

  Own Id: OTP-15772

## Ssh 4.7.5

### Fixed Bugs and Malfunctions

- The callback `ssh_channel:init/1` was missing in OTP-21

  Own Id: OTP-15762

- If a client was connected to an server on an already open socket, the callback
  `fun(PeerName,FingerPrint)` in the `accept_callback` option passed the local
  name in the argument PeerName instead of the remote name.

  Own Id: OTP-15763

## Ssh 4.7.4

### Fixed Bugs and Malfunctions

- SSH sftp daemon now accepts an SSH_FXP_STAT message encoded according to the
  wrong sftp version. Some clients sends such messages.

  Own Id: OTP-15498 Aux Id: ERL-822, PR-2077

## Ssh 4.7.3

### Fixed Bugs and Malfunctions

- Fixed port leakage if a ssh:daemon call failed.

  Own Id: OTP-15397 Aux Id: ERL-801

## Ssh 4.7.2

### Fixed Bugs and Malfunctions

- Incompatibility with newer OpenSSH fixed. Previously versions 7.8 and later
  could cause Erlang SSH to exit.

  Own Id: OTP-15413

- The '`exec`' option for ssh daemons had wrong format in the documentation.

  Own Id: OTP-15416

### Improvements and New Features

- Added public key methods ssh-ed25519 and ssh-ed448.

  Requires OpenSSL 1.1.1 or higher as cryptolib under the OTP application
  `crypto`.

  Own Id: OTP-15094 Aux Id: OTP-15419

- The SSH property tests are now adapted to the PropEr testing tool.

  Own Id: OTP-15312

- The term "user" was not documented in the SSH app. A new chapter with
  terminology is added to the User's Manual where the term "user" is defined.

  A reference manual page about the module `ssh_file` is also added. This is the
  default callback module for user's keys, host keys etc.

  Own Id: OTP-15314

- Host and user key checking is made more robust.

  Own Id: OTP-15424

## Ssh 4.7.1

### Improvements and New Features

- Extended the undocumented `ssh_dbg` debug module with an api for a circular
  trace buffer. This makes it easy to record the last low-level events before an
  error is detected. It is intended for solving difficult errors.

  Own Id: OTP-15020

- The key exchange methods `'curve25519-sha256@libssh.org'`,
  `'curve25519-sha256'` and `'curve448-sha512'` are implemented. The last two
  are defined in https://tools.ietf.org/html/draft-ietf-curdle-ssh-curves

  They all depends on that OpenSSL 1.1.1 or higher is used as cryptolib.

  Own Id: OTP-15133 Aux Id: OTP-15240

- The cipher '`chacha20-poly1305@openssh.com`' is now supported if OpenSSL 1.1.1
  or higher is used as cryptolib.

  Own Id: OTP-15209 Aux Id: OTP-15164

## Ssh 4.7

### Fixed Bugs and Malfunctions

- If the daemon port listener is restarted, it could potentially fail with
  `eaddrinuse` if the timing is unlucky. It will now retry and exponentially
  back off the listener restart a few times before failing.

  Own Id: OTP-14955

- A channel callback module always got the module name as reason in a call to
  terminate. Now it will get the proper Reason, usually 'normal'.

  Own Id: OTP-15084

### Improvements and New Features

- The option `exec` has new option values defined to make it much more easy to
  implement an own `exec` server.

  An option called `exec` for daemons implementing the handling of 'exec'
  requests has existed a long time but has been undocumented. The old
  undocumented value - as well as its behavior - is kept for compatibility
  EXCEPT that error messages are changed and are sent as "stderror" text.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14851

- Updated ssh_connection:shell/2 documentation.

  Own Id: OTP-14880

- The experimental `ssh_dbg` module is completely re-written. Its purpose is to
  make tracing and debugging easier on deployed systems.

  Own Id: OTP-14896

- The SSH supervisor structure has been slightly changed. This makes stopping
  the ssh application considerably faster if there are open connections. This is
  important in for example restarts.

  Own Id: OTP-14988

- The type specifications in SSH are completely reworked and the following types
  are renamed:

  `ssh:ssh_connection_ref()` is changed to
  [`ssh:connection_ref()`](`t:ssh:connection_ref/0`),

  `ssh:ssh_daemon_ref()` is changed to
  [`ssh:daemon_ref()`](`t:ssh:daemon_ref/0`),

  `ssh:ssh_channel_id()` is changed to
  [`ssh:channel_id()`](`t:ssh:channel_id/0`).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15002 Aux Id: OTP-15030

- The internal timer handling in SSH is now based on the gen_statem timers.

  Own Id: OTP-15019

- Removed the undocumented and unused modules `ssh_client_key.erl` and
  `ssh_server_key.erl`.

  Own Id: OTP-15028

- The Reference Manual pages are partly updated.

  The ssh page is now generated from specs and types, is restructured and is
  partly rephrased.

  The ssh_channel, ssh_connection, ssh_client_key_api, ssh_server_key_api and
  ssh_sftp pages are updated with links, correct type names and some minor
  changes.

  Own Id: OTP-15030 Aux Id: OTP-15002

- The behaviors `ssh_channel` and `ssh_daemon_channel` are renamed to
  `ssh_client_channel` and `ssh_server_channel` respectively.

  The old modules are kept for compatibility but should preferably be replaced
  when updating callback modules referring them.

  Own Id: OTP-15041

- New test suite for channels.

  Own Id: OTP-15051

- The `rekey_limit` option could now set the max time as well as the previously
  max data amount.

  Own Id: OTP-15069 Aux Id: ERL-617

- Changed process exit supervision from links to monitors.

  Own Id: OTP-15082

- Better handling of misbehaving channel callback modules.

  Own Id: OTP-15083

- A new moduli file is generated. This file is used for the recommended
  `diffie-hellman-group-exchange-sha256` key exchange algorithm in SSH.

  Own Id: OTP-15113

## Ssh 4.6.9.7

### Fixed Bugs and Malfunctions

- Fixed possible hanging in `ssh_sftp:stop_channel/1`.

  Own Id: OTP-16507 Aux Id: ERIERL-470

## Ssh 4.6.9.6

### Fixed Bugs and Malfunctions

- Fixed that `ssh_connection:send` could allocate a large amount of memory if
  given an iolist() as input data.

  Own Id: OTP-16373

- Safe atom conversions.

  Own Id: OTP-16375

- Constant time comparisons added.

  Own Id: OTP-16376

## Ssh 4.6.9.5

### Fixed Bugs and Malfunctions

- The ssh cli (e.g shell) server behaved strangely when characters were inserted
  in a string so that the last characters tried to wrap the line.

  Own Id: OTP-14849 Aux Id: ERL-545

## Ssh 4.6.9.4

### Fixed Bugs and Malfunctions

- If a client was connected to an server on an already open socket, the callback
  `fun(PeerName,FingerPrint)` in the `accept_callback` option passed the local
  name in the argument PeerName instead of the remote name.

  Own Id: OTP-15763

## Ssh 4.6.9.3

### Fixed Bugs and Malfunctions

- Fixed port leakage if a ssh:daemon call failed.

  Own Id: OTP-15397 Aux Id: ERL-801

## Ssh 4.6.9.2

### Fixed Bugs and Malfunctions

- Incompatibility with newer OpenSSH fixed. Previously versions 7.8 and later
  could cause Erlang SSH to exit.

  Own Id: OTP-15413

## Ssh 4.6.9.1

### Fixed Bugs and Malfunctions

- SFTP clients reported the error reason `""` if a non-OTP sftp server was
  killed during a long file transmission.

  Now the signal name (for example `"KILL"`) will be the error reason if the
  server's reason is empty.

  The documentation also lacked type information about this class of errors.

  Own Id: OTP-15148 Aux Id: ERIERL-194

- Fix ssh_sftp decode error for sftp protocol version 4

  Own Id: OTP-15149 Aux Id: ERIERL-199

## Ssh 4.6.9

### Fixed Bugs and Malfunctions

- Host key hash erroneously calculated for clients following draft-00 of RFC
  4419, for example PuTTY

  Own Id: OTP-15064

- Renegotiation could fail in some states

  Own Id: OTP-15066

## Ssh 4.6.8

### Fixed Bugs and Malfunctions

- An ssh_sftp server (running version 6) could fail if it is told to remove a
  file which in fact is a directory.

  Own Id: OTP-15004

- Fix rare spurious shutdowns of ssh servers when receiving `{'EXIT',_,normal}`
  messages.

  Own Id: OTP-15018

## Ssh 4.6.7

### Fixed Bugs and Malfunctions

- Fix bad spec in ssh.hrl: `double_algs()`.

  Own Id: OTP-14990

## Ssh 4.6.6

### Fixed Bugs and Malfunctions

- Remove a blocking risk when a channel is closed and an operation is tried on
  that channel after at least a second's time gap.

  Own Id: OTP-14939

### Improvements and New Features

- Added ssh_compat_SUITE.

  This suite contains a number of interoperability tests mainly with OpenSSH.
  The tests start docker containers with different OpenSSH and
  OpenSSL/LibreSSLcryptolib versions and performs a number of tests of supported
  algorithms.

  All login methods and all user's public key types are tested both for the
  client and the server.

  All algorithms for kex, cipher, host key, mac and compressions are tested with
  a number of exec and sftp tests, both for the client and the server.

  Own Id: OTP-14194 Aux Id: OTP-12487

- Default exec is disabled when a user-defined shell is enabled.

  Own Id: OTP-14881

## Ssh 4.6.5

### Fixed Bugs and Malfunctions

- Adjusted supervisor timeouts

  Own Id: OTP-14907

- Remove ERROR messages for slow process exits

  Own Id: OTP-14930

### Improvements and New Features

- Add option `save_accepted_host` to `ssh:connection`. This option, if set to
  false, inhibits saving host keys to e.g the file `known_hosts`.

  Own Id: OTP-14935

## Ssh 4.6.4

### Fixed Bugs and Malfunctions

- Fix problem with OpenSSH 7.2 (and later) clients that has used sha1 instead of
  sha2 for rsa-sha-256/512 user's public keys.

  Own Id: OTP-14827 Aux Id: ERL-531

## Ssh 4.6.3

### Fixed Bugs and Malfunctions

- Passphrase option for ecdsa public keys was missing.

  Own Id: OTP-14602

### Improvements and New Features

- The host and user public key handling is hardened so that a faulty plugin
  can't deliver a key of wrong type.

  Better checks in the server of the available hostkey's types at start and at
  each accept.

  Better checks in the client of the available user public key types at connect.

  Own Id: OTP-14676 Aux Id: ERIERL-52, OTP-14570

- SSH can now fetch the host key from the private keys stored in an Engine. See
  the crypto application for details about Engines.

  Own Id: OTP-14757

## Ssh 4.6.2

### Fixed Bugs and Malfunctions

- Trailing white space was removed at end of the hello-string. This caused
  interoperability problems with some other ssh-implementations (e.g OpenSSH
  7.3p1 on Solaris 11)

  Own Id: OTP-14763 Aux Id: ERIERL-74

- Fixes that tcp connections that was immediately closed (SYN, SYNACK, ACK, RST)
  by a client could be left in a zombie state.

  Own Id: OTP-14778 Aux Id: ERIERL-104

## Ssh 4.6.1

### Fixed Bugs and Malfunctions

- Fixed broken printout

  Own Id: OTP-14645

### Improvements and New Features

- Disable aes_gcm ciphers if peer is OpenSSH 6.2 which is known to have trouble
  with them in some cases.

  Own Id: OTP-14638

## Ssh 4.6

### Fixed Bugs and Malfunctions

- Enables the `ssh_io module` to also accept binary values when reading
  standard_io instead of getting stuck in the receive clause.

  Own Id: OTP-14506 Aux Id: PR1503

- Previously, the file owner access permission in response to
  ssh_sftp:read_file_info/2 function was always `read_write`. With this fix, the
  actual value of file owner access permission is added to the returning record.
  That value is calculated from file mode value.

  Own Id: OTP-14550 Aux Id: PR1533

### Improvements and New Features

- A new option `modify_algorithms` is implemented. It enables specifying changes
  on the default algorithms list. See the reference manual and the SSH User's
  Guide chapter "Configuring algorithms in SSH".

  Own Id: OTP-14568

## Ssh 4.5.1

### Fixed Bugs and Malfunctions

- All unknown options are sent to the transport handler regardless of type.

  Own Id: OTP-14541 Aux Id: EIRERL-63

## Ssh 4.5

### Improvements and New Features

- The internal handling of SSH options is re-written.

  Previously there were no checks if a client option was given to a daemon or
  vice versa. This is corrected now. If your code has e.g. a client-only option
  in a call to start a daemon, the call will fail.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12872

- Modernization of key exchange algorithms. See draft-ietf-curdle-ssh-kex-sha2
  for a discussion.

  Removed an outdated weak algorithm and added stronger replacements to keep
  interoperability with other modern ssh clients and servers. The default
  ordering of the algorithms is also adjusted.

  Retired: The nowadays unsecure key-exchange `diffie-hellman-group1-sha1` is
  not enabled by default, but can be enabled with the option
  `preferred-algorithms`.

  Added: The new stronger key-exchange `diffie-hellman-group16-sha512`,
  `diffie-hellman-group18-sha512` and `diffie-hellman-group14-sha256` are added
  and enabled by default.

  The questionable \[RFC 6194] sha1-based algorithms
  `diffie-hellman-group-exchange-sha1` and `diffie-hellman-group14-sha1` are
  however still kept enabled by default for compatibility with ancient clients
  and servers that lack modern key-exchange alternatives. When the
  draft-ietf-curdle-ssh-kex-sha2 becomes an rfc, those sha1-based algorithms and
  `diffie-hellman-group1-sha1` will be deprecated by IETF. They might then be
  removed from the default list in Erlang/OTP.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14110

- Modernized internal representation of sftp by use of maps.

  Own Id: OTP-14117

- The Extension Negotiation Mechanism and the extension `server-sig-algs` in
  draft-ietf-curdle-ssh-ext-info-05 are implemented.

  The related draft-ietf-curdle-rsa-sha2-05 is implemented and introduces the
  signature algorithms `rsa-sha2-256` and `rsa-sha2-512`.

  Own Id: OTP-14193

- The 'timeout' and 'connect_timeout' handling in ssh_sftp:start_channel
  documentation is clarified.

  Own Id: OTP-14216

- The functions `ssh:connect`, `ssh:shell` and `ssh:start_channel` now accept an
  IP-tuple as Host destination argument.

  Own Id: OTP-14243

- The function `ssh:daemon_info/1` now returns Host and Profile as well as the
  Port info in the property list.

  Own Id: OTP-14259

- Removed the option `public_key_alg` which was deprecated in 18.2. Use
  `pref_public_key_algs` instead.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14263

- The SSH application is refactored regarding daemon starting. The resolution of
  contradicting `Host` argument and `ip` option were not described. There were
  also strange corner cases when the `'any'` value was used in `Host` argument
  or `ip` option. This is (hopefully) resolved now, but it may cause
  incompatibilities for code using both `Host` and the `ip` option. The value
  'loopback' has been added for a correct way of naming those addresses.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14264

- The supervisor code is refactored. The naming of listening IP-Port-Profile
  triples are slightly changed to improve consistency in strange corner cases as
  resolved by OTP-14264

  Own Id: OTP-14267 Aux Id: OTP-14266

- The `idle_time` option can now be used in daemons.

  Own Id: OTP-14312

- Added test cases for IETF-CURDLE Extension Negotiation (ext-info)

  Own Id: OTP-14361

- Testcases for IETF-CURDLE extension `server-sig-algs` including `rsa-sha2-*`

  Own Id: OTP-14362 Aux Id: OTP-14361

- The option `auth_methods` can now also be used in clients to select which
  authentication options that are used and in which order.

  Own Id: OTP-14399

- Checks that a ECDSA public key (`ecdsa-sha2-nistp*`) stored in a file has the
  correct size.

  Own Id: OTP-14410

## Ssh 4.4.2.4

### Fixed Bugs and Malfunctions

- Fix rare spurious shutdowns of ssh servers when receiving `{'EXIT',_,normal}`
  messages.

  Own Id: OTP-15018

- Host key hash erroneously calculated for clients following draft-00 of RFC
  4419, for example PuTTY

  Own Id: OTP-15064

- Renegotiation could fail in some states

  Own Id: OTP-15066

## Ssh 4.4.2.3

### Fixed Bugs and Malfunctions

- An ssh_sftp server (running version 6) could fail if it is told to remove a
  file which in fact is a directory.

  Own Id: OTP-15004

## Ssh 4.4.2.2

### Improvements and New Features

- Default exec is disabled when a user-defined shell is enabled.

  Own Id: OTP-14881

## Ssh 4.4.2.1

### Fixed Bugs and Malfunctions

- Trailing white space was removed at end of the hello-string. This caused
  interoperability problems with some other ssh-implementations (e.g OpenSSH
  7.3p1 on Solaris 11)

  Own Id: OTP-14763 Aux Id: ERIERL-74

## Ssh 4.4.2

### Fixed Bugs and Malfunctions

- ssh:daemon_info/1 crashed if the listening IP was not 'any'

  Own Id: OTP-14298 Aux Id: seq13294

## Ssh 4.4.1

### Fixed Bugs and Malfunctions

- Fix bug when opening connections. If the tcp setup failed, that would in some
  cases not result in an error return value.

  Own Id: OTP-14108

- Reduce information leakage in case of decryption errors.

  Own Id: OTP-14109

- The key exchange algorithm diffie-hellman-group-exchange-sha\* has a
  server-option `{dh_gex_limits,{Min,Max}}`. There was a hostkey signature
  validation error on the client side if the option was used and the `Min` or
  the `Max` differed from the corresponding values obtained from the client.

  This bug is now corrected.

  Own Id: OTP-14166

- The sftpd server now correctly uses `root_dir` and `cwd` when resolving file
  paths if both are provided. The `cwd` handling is also corrected.

  Thanks to kape1395\!

  Own Id: OTP-14225 Aux Id: PR-1331, PR-1335

- Ssh_cli used a function that does not handle non-utf8 unicode correctly.

  Own Id: OTP-14230 Aux Id: ERL-364

### Improvements and New Features

- The implementation of the key exchange algorithms
  diffie-hellman-group-exchange-sha\* are optimized, up to a factor of 11 for
  the slowest ( = biggest and safest) group size.

  Own Id: OTP-14169 Aux Id: seq-13261

- The ssh host key fingerprint generation now also takes a list of algorithms
  and returns a list of corresponding fingerprints. See
  `public_key:ssh_hostkey_fingerprint/2` and the option `silently_accept_hosts`
  in `ssh:connect`.

  Own Id: OTP-14223

## Ssh 4.4

### Fixed Bugs and Malfunctions

- A file read with an sftp client could loose data if the packet_size is set to
  larger than 64k. This is corrected now in such a way that the packet_size is
  silently lowered if there is a risk for data loss.

  Own Id: OTP-13857 Aux Id: ERL-238, OTP-13858

- When user defined SSH shell REPL process exits with reason normal, the SSH
  channel callback module should report successful exit status to the SSH
  client. This provides simple way for SSH clients to check for successful
  completion of executed commands. (Thanks to isvilen)

  Own Id: OTP-13905 Aux Id: PR-1173

### Improvements and New Features

- Extended the option `silently_accept_hosts` for `ssh:connect` to make it
  possible for the client to check the SSH host key fingerprint string. Se the
  reference manual for SSH.

  Own Id: OTP-13887 Aux Id: OTP-13888

## Ssh 4.3.6

### Fixed Bugs and Malfunctions

- Re-negotiation problems with OpenSSH client solved.

  Own Id: OTP-13972

## Ssh 4.3.5

### Fixed Bugs and Malfunctions

- If a client illegaly sends an info-line and then immediately closes the
  TCP-connection, a badmatch exception was raised.

  Own Id: OTP-13966

## Ssh 4.3.4

### Fixed Bugs and Malfunctions

- Intermittent ssh ERROR REPORT mentioning nonblocking_sender

  Own Id: OTP-13953 Aux Id: seq13199

## Ssh 4.3.3

### Fixed Bugs and Malfunctions

- Handle all possible exit values that should be interpreted as \{error,
  closed\}. Failing to do so could lead to unexpected crashes for users of the
  ssh application.

  Own Id: OTP-13932 Aux Id: seq13189

## Ssh 4.3.2

### Fixed Bugs and Malfunctions

- Upgrade of an established client connection could crash because the ssh client
  supervisors children had wrong type. This is fixed now.

  Own Id: OTP-13782 Aux Id: seq13158

- Partly checks the public key early in public key authorization

  Own Id: OTP-13847 Aux Id: defensics-ssh3.1.0-190243,205277,219318

- Corrected handling of SHA for ECDSA (Elliptic curve public keys)

  Own Id: OTP-13850 Aux Id: defensics-ssh3.1.0-214168

- Problems found by test suites as well as by Codenomicon/Defensics fixed: -
  reduce max random padding to 15 bytes (Codenomicon/Defensics) - inclomplete
  pdu handling (Codenomicon/Defensics) - badmatch in test suite - non-blocking
  send fixes deadlock in ssh_connection_SUITE:interrupted_send

  Own Id: OTP-13854

- Caller is now notified when a tcp close is received.

  Own Id: OTP-13859 Aux Id: seq13177

### Improvements and New Features

- Use application:ensure_all_started/2 instead of hard-coding deps

  Own Id: OTP-13843 Aux Id: PR-1147

## Ssh 4.3.1

### Fixed Bugs and Malfunctions

- SSH client does not any longer retry a bad password given as option to
  ssh:connect et al.

  Own Id: OTP-13674 Aux Id: TR-HU92273

- Removed possible hanging risk for a certain timing sequence when communicating
  client and server executes on the same node.

  Own Id: OTP-13715

## Ssh 4.3

### Improvements and New Features

- A socket created and connected by gen_tcp could now be used as input to
  ssh:connect, ssh:shell, ssh_sftp:start_channel and ssh:daemon.

  Own Id: OTP-12860

- Some time optimization mainly in message encoding.

  Own Id: OTP-13131

- Optimized the sftp client time by setting new packet and window sizes.

  Own Id: OTP-13175

- The `ssh_connection_handler` module in SSH is changed and now uses the new
  behaviour `gen_statem`.

  The module can be used as an example of a `gen_statem` callback module but
  with a warning: This commit of ssh is just a straightforward port from gen_fsm
  to gen_statem with some code cleaning. Since the state machine and the state
  callbacks are almost unchanged the ssh module does not demonstrate the full
  potential of the new behaviour.

  The "new" state machine uses compound states. The ssh server and client state
  machines are quite similar but differences exist. With `gen_fsm` there were
  flags in the user data which in fact implemented "substates". Now with
  `gen_statem` those are made explicit in the state names, eg. the state
  `userauth` and the binary `role`\-flag becomes the two state names
  `{userauth, server}` and `{userauth, client}`.

  Own Id: OTP-13267

- The `{error, Reason}` tuples returned from `ssh_sftp` api functions are
  described.

  Own Id: OTP-13347 Aux Id: ERL-86

- Added -spec in ssh

  Own Id: OTP-13479

- It is now possible to call `ssh:daemon/{1,2,3}` with `Port=0`. This makes the
  daemon select a free listening tcp port before opening it. To find this port
  number after the call, use the new function `ssh:daemon_info/1`. See the
  reference manual for details.

  Own Id: OTP-13527

## Ssh 4.2.2.6

### Fixed Bugs and Malfunctions

- Fix rare spurious shutdowns of ssh servers when receiving `{'EXIT',_,normal}`
  messages.

  Own Id: OTP-15018

## Ssh 4.2.2.5

### Improvements and New Features

- Default exec is disabled when a user-defined shell is enabled.

  Own Id: OTP-14881

## Ssh 4.2.2.4

### Fixed Bugs and Malfunctions

- Trailing white space was removed at end of the hello-string. This caused
  interoperability problems with some other ssh-implementations (e.g OpenSSH
  7.3p1 on Solaris 11)

  Own Id: OTP-14763 Aux Id: ERIERL-74

## Ssh 4.2.2.3

### Fixed Bugs and Malfunctions

- The key exchange algorithm diffie-hellman-group-exchange-sha\* has a
  server-option `{dh_gex_limits,{Min,Max}}`. There was a hostkey signature
  validation error on the client side if the option was used and the `Min` or
  the `Max` differed from the corresponding values obtained from the client.

  This bug is now corrected.

  Own Id: OTP-14166

### Improvements and New Features

- Key exchange algorithms diffie-hellman-group-exchange-sha\* optimized, up to a
  factor of 11 for the slowest ( = biggest and safest) one.

  Own Id: OTP-14169 Aux Id: seq-13261

## Ssh 4.2.2.2

### Fixed Bugs and Malfunctions

- Upgrade of an established client connection could crash because the ssh client
  supervisors children had wrong type. This is fixed now.

  Own Id: OTP-13782 Aux Id: seq13158

## Ssh 4.2.2.1

### Fixed Bugs and Malfunctions

- SSH client does not any longer retry a bad password given as option to
  ssh:connect et al.

  Own Id: OTP-13674 Aux Id: TR-HU92273

## Ssh 4.2.2

### Fixed Bugs and Malfunctions

- Documentation correction of `ssh_sftp:position/4`

  Thanks to Rabbe Fogelholm.

  Own Id: OTP-13305 Aux Id: ERL-87

## Ssh 4.2.1

### Fixed Bugs and Malfunctions

- The authentication method 'keyboard-interactive' failed in the Erlang client
  when the server after successful authentication continued by asking for zero
  more passwords.

  Own Id: OTP-13225

## Ssh 4.2

### Fixed Bugs and Malfunctions

- Better error handling in ssh_file. There was some rare errors when a
  NFS-mounted file was opened by ssh_file and then remotely deleted during
  reading. That caused an endless loop.

  That bug is now fixed.

  Own Id: OTP-12699 Aux Id: OTP-11688

- Fixed a bug in the compression algorithm zlib@openssh.com.

  Own Id: OTP-12759

- It is now possible to start more than one daemon with a file descriptor given
  in option fd. Each daemon must of course have a unique file descriptor.

  Own Id: OTP-12966 Aux Id: seq12945

- Fixed a bug that caused the option `dh_gex_limit` to be ignored.

  Own Id: OTP-13029

- A problem is fixed with the `ssh:connect` option `pref_public_key_algs`
  specifying user keys.

  Own Id: OTP-13158

### Improvements and New Features

- Document updates in the ssh reference manual: app doc file and ssh_connection.

  Own Id: OTP-12003

- The authorization phase is made stateful to prevent ssh acting on messages
  sent in wrong order.

  Own Id: OTP-12787

- Testcases for bad message lengths and for bad subfield lengths added.

  Own Id: OTP-12792 Aux Id: Codenomicon #5214, 6166

- The 'ecdsa-sha2-nistp256', 'ecdsa-sha2-nistp384' and 'ecdsa-sha2-nistp521'
  signature algorithms for ssh are implemented. See RFC 5656.

  Own Id: OTP-12936

- The crypto algorithms 'aes192-ctr' and 'aes256-ctr' are implemented. See
  RFC 4344.

  Own Id: OTP-12939

- The ciphers and macs AEAD_AES_128_GCM and AEAD_AES_256_GCM are implemented but
  not enabled per default. See the SSH App Reference Manual and RFC5647 for
  details.

  The ciphers aes128-gcm@openssh.com and aes256-gcm@openssh.com are also
  implemented and available in the default configuration.

  Own Id: OTP-13018

- The ssh:daemon option dh_gex_groups is extended to read a user provided ssh
  moduli file with generator-modulus pairs. The file is in openssh format.

  Own Id: OTP-13052 Aux Id: OTP-13054

- There is now a file (public_key/priv/moduli) which lists
  size-generator-modulus triples. The purpose is to give servers the possibility
  to select the crypto primes randomly among a list of pregenerated triples.
  This reduces the risk for some attacks on diffie-hellman negotiation.

  See the reference manual for public_key:dh_gex_group/4 where the handling of
  this is described.

  The ssh server (ssh:daemon) uses this.

  Own Id: OTP-13054 Aux Id: OTP-13052

- The ssh:daemon option pwdfun now also takes a fun/4. This enables the user
  to 1) check userid-password in another way than the builtin algorithm, 2)
  implement rate limiting per user or source IP or IP+Port, and 3) implement
  blocking of missbehaving peers.

  The old fun/2 still works as previously.

  Own Id: OTP-13055 Aux Id: OTP-13053

- There is now a new option to make the server limit the size range of moduli
  available for the diffie-hellman group exchange negotiation. See option
  `{dh_gex_limits,{Min,Max}}` in ssh:daemon/3.

  Own Id: OTP-13066

- Ecdh key exchange now validates compressed and uncompressed keys as defined in
  rfc5656

  Own Id: OTP-13067

- Search order for the .ssh directory are changed so `$HOME` is tried before
  `init:get_argument(home)`.

  Own Id: OTP-13109

- The sftp receive window handling is optimized so it will not update the remote
  end too often. This makes "sftp mget" considerable faster.

  Own Id: OTP-13130

- The option `key_cb` is extended to take an optional list that is passed to the
  callback module as an option. With this it is possible to have different keys
  depending on which host that is connected. Another possibility is to write a
  callback module that fetches keys etc from a database.

  Thanks to Vipin Nair.

  Own Id: OTP-13156

## Ssh 4.1.3

### Known Bugs and Problems

- SSH_MSG_KEX_DH_GEX_REQUEST_OLD implemented to make PuTTY work with erl server.

  Own Id: OTP-13140

## Ssh 4.1.2

### Fixed Bugs and Malfunctions

- Add a 1024 group to the list of key group-exchange groups

  Own Id: OTP-13046

## Ssh 4.1.1

### Improvements and New Features

- A new option `max_channels` limits the number of channels with active
  server-side subsystems that are accepted.

  Own Id: OTP-13036

## Ssh 4.1

### Fixed Bugs and Malfunctions

- Send an understandable disconnect message when the key exchange phase can't
  find a common algorithm. There are also some test cases added.

  Own Id: OTP-11531

- The third parameter in `ssh_sftp:write_file` is now accepting iolists again.
  Unicode handling adjusted.

  Own Id: OTP-12853 Aux Id: seq12891

### Improvements and New Features

- First part of ssh test suite re-organization and extension.

  Own Id: OTP-12230

- The key exchange algorithms 'ecdh-sha2-nistp256', 'ecdh-sha2-nistp384' and
  'ecdh-sha2-nistp521' are implemented. See RFC 5656.

  This raises the security level considerably.

  Own Id: OTP-12622 Aux Id: OTP-12671, OTP-12672

- The key exchange algorithm 'diffie-hellman-group14-sha1' is implemented. See
  RFC 4253.

  This raises the security level.

  Own Id: OTP-12671 Aux Id: OTP-12672, OTP-12622

- The key exchange algorithms 'diffie-hellman-group-exchange-sha1' and
  'diffie-hellman-group-exchange-sha256' are implemented. See RFC 4419.

  This raises the security level.

  Own Id: OTP-12672 Aux Id: OTP-12671, OTP-12622

- Adding random length extra padding as recommended in RFC 4253 section 6.

  Own Id: OTP-12831

- New test library for low-level protocol testing. There is also a test suite
  using it for some preliminary tests. The intention is to build on that for
  more testing of individual ssh messages. See
  `lib/ssh/test/ssh_trpt_test_lib.erl` and `ssh_protocol_SUITE.erl` in the same
  directory.

  Own Id: OTP-12858

- Increased default values for diffie-hellman-group-exchange-sha\* to Min =
  1024, N = 6144, Max = 8192.

  Added 6144 and 8192 bit default gex groups.

  Own Id: OTP-12937

- The mac algorithm 'hmac-sha2-512' is implemented. See RFC 6668.

  Own Id: OTP-12938

## Ssh 4.0

### Fixed Bugs and Malfunctions

- Ssh crashed if a message was sent on a channel with packet_size = 0.

  A new option for ssh:daemon is also introduced:
  `minimal_remote_max_packet_size`. This option sets the least max packet size
  declaration that the daemon will accept from a client. The default value is 0
  to maintain compatibility with OpenSSH and the rfc:s.

  Own Id: OTP-12645 Aux Id: seq12816

- Included test of the 'e' and 'f' parameters in diffie-hellman key exchange as
  specified in rfc 4253 section 8.

  Own Id: OTP-12649

- Fixes the bug that once the `rekey_limit` bytes (by default, 1GB) had been
  transmitted the connection was rekeyed every minute, not after the next
  transferred 'rekey_limit' chunk.

  Thanks to Simon Cornish for the report and the fix\!

  Own Id: OTP-12692

- Fixes a bug that causes an SFTP connection to always fail when \{timeout,
  Timeout\} option is used with ssh_sftp:start_channel.

  Thanks to Simon Cornish

  Own Id: OTP-12708

- Fix various ssh key exchange problems.

  Thanks to Simon Cornish

  Own Id: OTP-12760 Aux Id:
  [pull req 715](https://github.com/erlang/otp/pull/715)

- The options `system_dir` and `user_dir` assumes that the value is a path to a
  directory which is readable. This is now checked early, so `ssh:daemon` and
  `ssh:connect` will fail with an error message immediately.

  Own Id: OTP-12788

- A daemon now checks that a client doesn't try to authorize with methods not in
  the option auth_methods.

  Own Id: OTP-12790

- Disconnectfun now should trigger on all disconnects.

  Own Id: OTP-12811

### Improvements and New Features

- Better usage of binary matching in ssh_auth.erl and ssh_message.erl

  Own Id: OTP-11697

- A new option 'preferred_algorithms' is available for `ssh:daemon` and
  `ssh:connect`.

  This option defines the algorithms presented to the peer in the algorithm
  negotiation phase of the ssh protocol.

  The default list can be obtained from the new function
  `ssh:default_algorithms/0`.

  \*** INCOMPATIBILITY with removed undocumented options 'role' and
  'compression' \***

  Own Id: OTP-12029

- The internal group to user_drv protocol has been changed to be synchronous in
  order to guarantee that output sent to a process implementing the user_drv
  protocol is printed before replying. This protocol is used by the
  standard_output device and the ssh application when acting as a client.

  This change changes the previous unlimited buffer when printing to standard_io
  and other devices that end up in user_drv to 1KB.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12240

- If ssh_connection:subsystem/4 fails we do not want to crash but rather
  terminate gracefully.

  Own Id: OTP-12648 Aux Id: seq12834

- New option `id_string` for `ssh:daemon` and `ssh:connect` for limiting banner
  grabbing attempts.

  The possible values are: `{id_string,string()}` and `{id_string,random}`. The
  latter will make ssh generate a random nonsense id-string for each new
  connection.

  Own Id: OTP-12659

- To enable the ssh daemon to run in a virtualized environment, where there can
  be more that one server that has the same ip-address and port, we add a new
  option profile.

  Own Id: OTP-12675

- Upgrade test suite added.

  Own Id: OTP-12676

- A new option for handling the SSH_MSG_DEBUG message's printouts. A fun could
  be given in the options that will be called whenever the SSH_MSG_DEBUG message
  arrives. This enables the user to format the printout or just discard it.

  Own Id: OTP-12738 Aux Id: seq12860

- Testcase improvements and corrections:

  \* Add testcases for the `disconnectfun` option on both server and client
  sides

  \* Timeout testcases adjusted for slow machines where they sometimes failed

  Own Id: OTP-12786

- The option `disconnectfun` can now be used both on the client and server side.

  Own Id: OTP-12789

- A new option unknown_msgfun/2 for ssh:connect and ssh:daemon for handling
  unknown messages. With the option it is possible to intercept before an INFO
  log message is generated.

  One usage is to filter out messages that are not wanted in the error logger as
  info reports. An example of such a message is the 'etimedout' tcp error
  message that will be received if a connection has keep_alive and the peer is
  restarted.

  Own Id: OTP-12813 Aux Id: seq12881

## Ssh 3.2.4

### Fixed Bugs and Malfunctions

- Gracefully terminate if sockets is unexpectedly closed.

  Own Id: OTP-12782

- Made Codenomicon Defensics test suite pass:

  - limit number of algorithms in kexinit message
  - check 'e' and 'f' parameters in kexdh
  - implement 'keyboard-interactive' user authentication on server side
  - return plain text message to bad version exchange message

  Own Id: OTP-12784

## Ssh 3.2.3

### Fixed Bugs and Malfunctions

- A new option for handling the SSH_MSG_DEBUG message's printouts. A fun could
  be given in the options that will be called whenever the SSH_MSG_DEBUG message
  arrives. This enables the user to format the printout or just discard it.

  Own Id: OTP-12738 Aux Id: seq12860

## Ssh 3.2.2

### Improvements and New Features

- New option `id_string` for `ssh:daemon` and `ssh:connect` for limiting banner
  grabbing attempts.

  The possible values are: `{id_string,string()}` and `{id_string,random}`. The
  latter will make ssh generate a random nonsense id-string for each new
  connection.

  Own Id: OTP-12659

## Ssh 3.2.1

### Fixed Bugs and Malfunctions

- Ssh crashed if a message was sent on a channel with packet_size = 0.

  A new option for ssh:daemon is also introduced:
  `minimal_remote_max_packet_size`. This option sets the least max packet size
  declaration that the daemon will accept from a client. The default value is 0
  to maintain compatibility with OpenSSH and the rfc:s.

  Own Id: OTP-12645 Aux Id: seq12816

## Ssh 3.2

### Fixed Bugs and Malfunctions

- If a channel is closed by the peer while using a function with call semantics
  in ssh_connection.erl return \{error, closed\}. Document that the functions
  can return \{error, timeout | closed\} and not only ssh_request_status()

  Own Id: OTP-12004

- Bug that causes ssh:connect to return `{error,int()}` instead of
  `{error,timeout}` when ssh handshake takes too long time.

  Own Id: OTP-12369

- Documentation corrections. (Thanks to Rabbe Fogelholm)

  Own Id: OTP-12399

### Improvements and New Features

- Example of ssh_connection:exec added.

  Own Id: OTP-12558

## Ssh 3.1

### Fixed Bugs and Malfunctions

- Make sure the clean rule for ssh, ssl, eunit and otp_mibs actually removes
  generated files.

  Own Id: OTP-12200

- Improved Property Tests (Thanks to Thomas, John and Tobias at QuviQ)

  Own Id: OTP-12256

- Correct typo of renegotiate that could cause rekeying to fail

  Own Id: OTP-12277 Aux Id: seq12736

- The \{timeout, Timeout\} option passed to ssh_sftp:start_channel was not
  applied to the early phases of the SSH protocol. This patch passes the Timeout
  through to ssh:connect. In case the timeout occurs during these phases,
  \{error, timeout\} is returned. (Thanks to Simon Cornish)

  Own Id: OTP-12306

### Improvements and New Features

- Added API functions ptty_alloc/3 and ptty_alloc/4, to allocate a pseudo tty.

  Own Id: OTP-11542 Aux Id: seq12493, OTP-11631

- Supports tar file creation on other media than file systems mounted on the
  local machine.

  The `erl_tar` api is extended with `erl_tar:init/3` that enables usage of user
  provided media storage routines. A ssh-specific set of such routines is hidden
  in the new function `ssh_sftp:open_tar/3` to simplify creating a tar archive
  on a remote ssh server.

  A chunked file reading option is added to `erl_tar:add/3,4` to save memory on
  e.g small embedded systems. The size of the slices read from a file in that
  case can be specified.

  Own Id: OTP-12180 Aux Id: seq12715

- Always send SSH_DISCONNECT protocol messages when peer sends corrupt messages.

  Own Id: OTP-12185

- Hooks for funs that can change binaries sent to remote sites from erl_tar for
  renote tar file creation are added. See `ssh_sftp:open_tar/3,4` for details.
  The hooks could also be used to read remote tar files that need transformation
  before file extraction.

  Those hooks are intended for encryption and decryption of tar files. Effort is
  put into memory, disk and network resource economy.

  Own Id: OTP-12312 Aux Id: OTP-12180

## Ssh 3.0.8

### Fixed Bugs and Malfunctions

- Fixes of login blocking after port scanning.

  Own Id: OTP-12247 Aux Id: seq12726

## Ssh 3.0.7

### Fixed Bugs and Malfunctions

- Add option sftp_vsn to SFTP

  Own Id: OTP-12227

### Improvements and New Features

- Fix option user_interaction to work as expected. When password authentication
  is implemented with ssh keyboard-interactive method and the password is
  already supplied, so that we do not need to query user, then connections
  should succeed even though user_interaction option is set to false.

  Own Id: OTP-11329 Aux Id: seq12420, seq12335

## Ssh 3.0.6

### Fixed Bugs and Malfunctions

- Gracefully handle bad data from the client when expecting ssh version
  exchange.

  Own Id: OTP-12157 Aux Id: seq12706

- When restarting an ssh daemon, that was stopped with ssh:stop_listner/ \[1,2]
  new options given shall replace old ones.

  Own Id: OTP-12168 Aux Id: seq12711

### Improvements and New Features

- ssh now has a format_status function to avoid printing sensitive information
  in error loggs.

  Own Id: OTP-12030

### Known Bugs and Problems

- The option `parallel_login` didn't work with the value `true`. All logins were
  serial.

  Own Id: OTP-12194

## Ssh 3.0.5

### Fixed Bugs and Malfunctions

- When starting an ssh-daemon giving the option \{parallel_login, true\}, the
  timeout for authentication negotiation (\{negotiation_timeout, integer()\})
  was never removed.

  This caused the session to always be terminated after the timeout if
  parallel_login was set.

  Own Id: OTP-12057 Aux Id: seq12663

### Improvements and New Features

- Warning: this is experimental and may disappear or change without previous
  warning.

  Experimental support for running Quickcheck and PropEr tests from common_test
  suites is added to common_test. See the reference manual for the new module
  `ct_property_testing`.

  Experimental property tests are added under
  `lib/{inet,ssh}/test/property_test`. They can be run directly or from the
  commont_test suites `inet/ftp_property_test_SUITE.erl` and
  `ssh/test/ssh_property_test_SUITE.erl`.

  See the code in the `test` directories and the man page for details.

  (Thanks to Tuncer Ayaz for a patch adding Triq)

  Own Id: OTP-12119

## Ssh 3.0.4

### Fixed Bugs and Malfunctions

- When starting an ssh-daemon giving the option \{parallel_login, true\}, the
  timeout for authentication negotiation (\{negotiation_timeout, integer()\})
  was never removed.

  This caused the session to always be terminated after the timeout if
  parallel_login was set.

  Own Id: OTP-12057 Aux Id: seq12663

## Ssh 3.0.3

### Fixed Bugs and Malfunctions

- Removed mail address from error reports and corrected spelling error
  (Stacktace -> stacktrace)

  Own Id: OTP-11883 Aux Id: seq12586

- Decode/encode fixes in SSH_MSG_IGNORE and SSH_MSG_UNIMPLEMENTED.

  Own Id: OTP-11983

### Improvements and New Features

- Accepts that some older OpenSSH clients sends incorrect disconnect messages.

  Own Id: OTP-11972

- Handle inet and inet6 option correctly

  Own Id: OTP-11976

## Ssh 3.0.2

### Fixed Bugs and Malfunctions

- Fixed timeout bug in ssh:connect.

  Own Id: OTP-11908

### Improvements and New Features

- Option `max_sessions` added to `ssh:daemon/{2,3}`. This option, if set, limits
  the number of simultaneous connections accepted by the daemon.

  Own Id: OTP-11885

## Ssh 3.0.1

### Fixed Bugs and Malfunctions

- Fixes the problem that ssh_cli in some cases could delay the prompt if a tty
  was not requested by the client.

  Own Id: OTP-10732

- The variable NewCol is now correctly calculated allowing for tab-completion of
  function calls even when preceded with blank space (Thanks to Alexander
  Demidenko)

  Own Id: OTP-11566

- Fix incorrect dialyzer spec and types, also enhance documentation.

  Thanks to Ayaz Tuncer.

  Own Id: OTP-11627

- Fixed a bug when ssh:exec executes a linux command on a linux ssh daemon. If
  the result is sent back from standard error, the length information was not
  stripped off correctly.

  Own Id: OTP-11667

- Fixed a bug with the ssh file 'known_hosts' which made the file grow with many
  equal entries.

  Own Id: OTP-11671

- Some local implementations of removing the last element from a list are
  replaced by `lists:droplast/1`. Note that this requires at least `stdlib-2.0`,
  which is the stdlib version delivered in OTP 17.0. (Thanks to Hans Svensson)

  Own Id: OTP-11678

- Bug fix for `ssh:daemon/2,3` so that the failfun is called when it should.

  Own Id: OTP-11680

- Fixed bug which crashed ssh when SSH_MSG_KEX_DH_GEX_GROUP is received. This
  could cause a vm-crash for eheap_alloc during garbage collect.

  Own Id: OTP-11696 Aux Id: 12547, 12532

- Fixes a bug that breaks keyboard-interactive authentication. Thanks to Simon
  Cornish for reporting and suggesting a fix.

  Own Id: OTP-11698

- dialyzer specs are now correct for `ssh:start/0`, `ssh:start/1`, `ssh:stop/0`
  and `ssh_connection_handler:open_channel/5`. (Thanks to Johannes Weißl )

  Own Id: OTP-11705

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

- Fixed dialyzer warning for `ssh_connection:send`.

  Own Id: OTP-11821

- `ssh:daemon/2,3` : Added options `negotiation_timeout` and `parallel_login` to
  tune the authentication behaviour.

  Own Id: OTP-11823

### Improvements and New Features

- Ssh now fully supports unicode filenames, filecontents, shell and cli. Please
  note that the underlying os and emulator must also give support for unicode.
  You may want to start the emulator with "`erl +fnu`" on Linux.

  Own Id: OTP-10953

## Ssh 3.0

### Fixed Bugs and Malfunctions

- The ssh cli is now faster at close and before new prompt.

  Own Id: OTP-11339 Aux Id: seq12423

- Ssh process structure was redesigned to better map to what is truly parallel
  this has solved a lot of strange timing issues that sometimes would occur, for
  instance a process leak could happen when a lot of connections where taken up
  and down in parallel in a short period of time. Also backwards compatible
  clauses to "original" but never supported features has been removed.

  Impact: Increases flow efficiency

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11363

- Fix various typos in erts, kernel and ssh. Thanks to Martin Hässler.

  Own Id: OTP-11414

- Correct private_key type documentation in ssh_server_key_api. Thanks to
  Tristan Sloughter.

  Own Id: OTP-11449

- The functions in ssh_no_io.erl did not mimic the functions in ssh_io.erl
  correctly, the arity was incorrect for some functions which caused ssh to fail
  in the wrong way.

  Own Id: OTP-11490

### Improvements and New Features

- Add option to disallow CLI

  Own Id: OTP-10976

- Add sockname and user to ssh:connection_info/2

  Own Id: OTP-11296

## Ssh 2.1.8

### Improvements and New Features

- Do not chmod ~/.ssh unnecessarily.

  Own Id: OTP-11189

- Make ssh_cli.erl handle CTRL+C. Thanks to Stefan Zegenhagen.

  Own Id: OTP-11199

- Clarified timeout options in documentation.

  Own Id: OTP-11249

- Add openssh_zlib compression type to ssh_transport. Thanks to Louis-Philippe
  Gauthier.

  Own Id: OTP-11256

## Ssh 2.1.7

### Fixed Bugs and Malfunctions

- ssh:daemon will get fed with an argument even if it is not a valid expression.

  Own Id: OTP-10975

### Improvements and New Features

- Properly ignore everything in lib/ssh/doc/html/. Thanks to Anthony Ramine.

  Own Id: OTP-10983

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

## Ssh 2.1.6

### Fixed Bugs and Malfunctions

- Fixed timing rekeying bug.

  Own Id: OTP-10940

## Ssh 2.1.5

### Fixed Bugs and Malfunctions

- Bug in rekeying for daemon fixed.

  Own Id: OTP-10911

### Improvements and New Features

- Enhanced error message and added test for ssh clients trying to start non
  existing subsystems.

  Own Id: OTP-10714

## Ssh 2.1.4

### Improvements and New Features

- Better quality on the error messages for when key exchange failed.

  Own Id: OTP-10553 Aux Id: seq12152

- Fix link to documentation for ssh:connect/3,4. Thanks to Martin Hässler.

  Own Id: OTP-10862

## Ssh 2.1.3

### Fixed Bugs and Malfunctions

- It is now possible to send an empty binary using ssh_connection:send/3, this
  corner case previously caused ssh_connection:send to hang.

  Own Id: OTP-9478 Aux Id: kunagi-226 \[137]

- Fix typo in keyboard-interactive string. Thanks to Daniel Goertzen

  Own Id: OTP-10456

- ssh_connectino:send/3 will not return until all data has been sent. Previously
  it could return too early, resulting in things such premature close of the
  connection. Also improved error handling of closed SSH channels.

  Own Id: OTP-10467

- Fixed ssh_cli.erl crashes because #state.buf is yet 'undefined'.

  Fixed Client terminateing connections due to channel_request message response
  is sent to the wrong id.

  Affected SSH clients: - all clients based on SSH-2.0-TrileadSSH2Java_213
  (problem #1) - SSH Term Pro (problem #2)

  Thanks to Stefan Zegenhagen

  Own Id: OTP-10475

- Fixed various syntax errors in SSH appup file

  Own Id: OTP-10657

### Improvements and New Features

- SSH_FX_FILE_IS_A_DIRECTORY message for sftp implemented

  Own Id: OTP-6406 Aux Id: kunagi-218 \[129]

- SSH Rekeying fixed

  Own Id: OTP-7785 Aux Id: kunagi-220 \[131]

- Added User Guide for the SSH application

  Own Id: OTP-7786 Aux Id: kunagi-221 \[132]

- Documentation regarding failfun, connectfun and disconnectfun provided

  Own Id: OTP-7792 Aux Id: kunagi-222 \[133]

- SSH connection timer implementation

  New option, \{idle_time, integer()\}, sets a timeout on connection when no
  channels are active, defaults to infinity

  Own Id: OTP-10514 Aux Id: seq12020

- Some examples overflowing the width of PDF pages have been corrected.

  Own Id: OTP-10665

- Fixed internal error on when client and server cannot agree o which authmethod
  to use.

  Own Id: OTP-10731 Aux Id: seq12237

## Ssh 2.1.2.1

### Improvements and New Features

- Removed error report in ssh_connection_handler triggered by badmatch failure.

  Own Id: OTP-11188

## Ssh 2.1.2

### Fixed Bugs and Malfunctions

- SSH quiet mode

  A new option to ssh:connect/3,4, quiet_mode. If true, the client will not
  print out anything on authorization.

  Own Id: OTP-10429 Aux Id: kunagi-273 \[184]

- Restrict which key algorithms to use

  A new option to ssh:connect/3,4 is introduced, public_key_algs, where you can
  restrict which key algorithms to use and in which order to try them.

  Own Id: OTP-10498 Aux Id: kunagi-289 \[200]

- Confidentiality of client password

  Unsets clients password after authentication.

  Own Id: OTP-10511 Aux Id: kunagi-292 \[203]

- Fixed user interaction for SSH

  It's now available to accept hosts and input password

  Own Id: OTP-10513 Aux Id: kunagi-293 \[204]

## Ssh 2.1.1

### Fixed Bugs and Malfunctions

- Ssh now only sends one channel close message under all circumstances, before
  it would sometimes incorrectly send two.

  Own Id: OTP-10060

- The options check mistreated the ip_v6_disable-option, and did not handle
  some, at the moment, undocumented options correctly.

  Own Id: OTP-10061

- The channel id in a channel failure message, sent to the peer, is now in all
  cases the remote channel id

  Own Id: OTP-10062

- Improved handling of multiple closes to avoid occasional crashes when a
  channel is closed more than once.

  Own Id: OTP-10112

- Fix lib/src/test/ssh_basic_SUITE.erl to fix IPv6 option typos

  Fixed incorrect option "ipv6_disable" to "ipv6_disabled" as documented in the
  ssh manual.

  Own Id: OTP-10219

- SSH: Make "auth_methods" server option re-usable

  The 'auth_methods' option is used by the server side of the SSH code to tell a
  connecting SSH client about the authentication methods that are supported by
  the server. The code still extracts and handles the 'auth_methods' option from
  Opts in appropriate places, but the Opts checking code in ssh.erl didn't allow
  that option to be specified.

  Own Id: OTP-10224

- Use the correct channel id when adjusting the channel window

  Own Id: OTP-10232

## Ssh 2.1

### Fixed Bugs and Malfunctions

- All keys in authorized_keys are considered, wrongly only the first one was
  before.

  Own Id: OTP-7235

- ssh daemon now properly handles ras host keys, in previous versions only dsa
  host keys sufficed to set up a connection.

  Own Id: OTP-7677

- ssh:shell/3 and ssh:connect/3 does not hang anymore if connection negotiation
  fails

  Own Id: OTP-8111

- Improve check so that we will not try to read ssh packet length indicator if
  not sure we have enough data.

  Own Id: OTP-8380

- Do not try to use user interaction when it is disabled.

  Own Id: OTP-9466 Aux Id: seq11886

- Improved error handling of internal errors i the ssh connection handling
  process

  Own Id: OTP-9905

- sftp daemon generates file handles correct

  Own Id: OTP-9948

### Improvements and New Features

- Document supported algorithms

  Own Id: OTP-8109

- Graceful handling of premature close from an sftp client.

  Own Id: OTP-9391 Aux Id: seq11838

- Changed ssh implementation to use the public_key application for all public
  key handling. This is also a first step for enabling a callback API for
  supplying public keys and handling keys protected with password phrases.

  Additionally the test suites where improved so that they do not copy the users
  keys to test server directories as this is a security liability. Also ipv6 and
  file access issues found in the process has been fixed.

  This change also solves OTP-7677 and OTP-7235

  This changes also involves some updates to public_keys ssh-functions.

  Own Id: OTP-9911

- Added options for the ssh client to support user keys files that are password
  protected.

  Own Id: OTP-10036 Aux Id: OTP-6400, Seq10595

## Ssh 2.0.9

### Improvements and New Features

- Erlang/OTP can now be built using parallel make if you limit the number of
  jobs, for instance using '`make -j6`' or '`make -j10`'. '`make -j`' does not
  work at the moment because of some missing dependencies.

  Own Id: OTP-9451

- Ssh behaviours now use the new directive "-callback". Parameters will be
  further specified in a later version of ssh.

  Own Id: OTP-9796

## Ssh 2.0.8

### Fixed Bugs and Malfunctions

- Calling ssh_sftp:stop_channel/1 resulted in that the trap_exit flag was set to
  true for the invoking process.

  Own Id: OTP-9386 Aux Id: seq11865

## Ssh 2.0.7

### Fixed Bugs and Malfunctions

- An unexpected message would crash the ssh_connection_handler and close the
  connection. Now an error message is generated instead.

  Own Id: OTP-9273

## Ssh 2.0.6

### Fixed Bugs and Malfunctions

- A memory leak has been fixed. I.e. per terminated connection the size of a pid
  and the length of a user name string was not cleared.

  Own Id: OTP-9232

## Ssh 2.0.5

### Improvements and New Features

- Strengthened random number generation. (Thanks to Geoff Cant)

  Own Id: OTP-9225

## Ssh 2.0.4

### Fixed Bugs and Malfunctions

- In some cases SSH returned \{error, normal\} when a channel was terminated
  unexpectedly. This has now been changed to \{error, channel_closed\}.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8987 Aux Id: seq11748

- SSH did not handle the error reason enetunreach when trying to open a IPv6
  connection.

  Own Id: OTP-9031

### Improvements and New Features

- It is now possible to use SSH to sign and verify binary data.

  Own Id: OTP-8986

- SSH now ensures that the .ssh directory exists before trying to access files
  located in that directory.

  Own Id: OTP-9010

## Ssh 2.0.3

### Fixed Bugs and Malfunctions

- The fix regarding OTP-8849 was not included in the previous version as stated.

  Own Id: OTP-8918

## Ssh 2.0.2

### Fixed Bugs and Malfunctions

- The ssh_system_sup did not catch noproc and shutdown messages.

  Own Id: OTP-8863

- In some cases a crash report was generated when a connection was closing down.
  This was caused by a race condition between two processes.

  Own Id: OTP-8881 Aux Id: seq11656, seq11648

### Improvements and New Features

- SSH no longer use deprecated public_key functions.

  Own Id: OTP-8849

## Ssh 2.0.1

### Fixed Bugs and Malfunctions

- SSH in some cases terminated channels with reason normal when it should have
  been shutdown.

  Own Id: OTP-8714

- SSH in some cases generated a crash report when a channel was closed in a
  normal way.

  Own Id: OTP-8735 Aux Id: seq11615

- The processes ssh_subsystem_sup and one ssh_channel_sup was not terminated
  when a connection was closed.

  Own Id: OTP-8807

## Ssh 2.0

### Fixed Bugs and Malfunctions

- The function ssh:connect/4 was not exported.

  Own Id: OTP-8550 Aux Id:

- Aligned error message with used version (SSH_FX_FAILURE vs
  SSH_FX_NOT_A_DIRECTORY, the latter introduced in version 6).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8644 Aux Id: seq11574

- Resolved race condition when another connection is started before a channel is
  opened in the first connection.

  Own Id: OTP-8645 Aux Id: seq11577

### Improvements and New Features

- The configuration parameter ip_v6_disabled is now available, which makes it
  possible for the user to alter the IP version SSH shall use.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8535 Aux Id:

- The ssh_connection:send operation now accepts infinity as timeout.

  Own Id: OTP-8534 Aux Id:

- The connection handler now include stack traces when a channel message is not
  handled correctly.

  Own Id: OTP-8524 Aux Id:

- Removed deprecated modules (ssh_ssh, ssh_sshd and ssh_cm) and functions
  (ssh_sftp:connect and ssh_sftp:stop).

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8596 Aux Id:

## Ssh 1.1.8

### Fixed Bugs and Malfunctions

- In some cases SSH ceased to collect more data from the transport layer.

  Own Id: OTP-8401 Aux Id: seq11479

### Improvements and New Features

- Old release notes removed.

  Own Id: OTP-8356 Aux Id:

## Ssh 1.1.7

### Fixed Bugs and Malfunctions

- Now clear all processes when a connection is terminated.

  Own Id: OTP-8121 Aux Id:

- In some rare cases the connection handler could enter an infinite loop.

  Own Id: OTP-8277 Aux Id: seq11428

- If an SFTP server did not respond with EOF, the function ssh_sftp:list_dir/2/3
  would enter an infinite loop.

  Own Id: OTP-8278 Aux Id: seq11450

### Improvements and New Features

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8201 Aux Id:

## Ssh 1.1.6

### Fixed Bugs and Malfunctions

- ssh_sftp:start_channel did not handle all possible return values from
  ssh_channel:start correctly.

  Own Id: OTP-8176 Aux Id:

- SFTPD did not handle rename command (version 4) correctly.

  Own Id: OTP-8175 Aux Id: seq11373

- If a connection manager already had been terminated it could cause a channel
  to generate a crash report when it was about to stop.

  Own Id: OTP-8174 Aux Id: seq11377

- Requests could result in badarg or badmatch EXIT messages in the connection
  manager if the channel no longer existed.

  Own Id: OTP-8173 Aux Id: seq11379

- ssh_transport:unpack/3 could cause a badarg error.

  Own Id: OTP-8162 Aux Id:

### Improvements and New Features

- The encryption algorithm aes128-cbc is now supported. Requires that
  crypto-1.6.1 is available.

  Own Id: OTP-8110 Aux Id:

## Ssh 1.1.5

### Fixed Bugs and Malfunctions

- ssh_sftp:start_channel/3 did not handle timeout correctly.

  Own Id: OTP-8159 Aux Id: seq11386

- If a progress message was not received after invoking ssh:connect/3 the call
  could hang for ever. A timeout option has also been added.

  Own Id: OTP-8160 Aux Id: seq11386

- A comma has been missing in the ssh.appup file since SSH-1.0.2.

  Own Id: OTP-8161 Aux Id:

## Ssh 1.1.4

### Fixed Bugs and Malfunctions

- SSH sometimes caused a crash report at disconnect.

  Own Id: OTP-8071 Aux Id: seq11319

## Ssh 1.1.3

### Fixed Bugs and Malfunctions

- The operation ssh_sftp:stop_channel/1 returned an exception if the connection
  already had been closed.

  Own Id: OTP-7996 Aux Id: seq11281

- SSH did not handle if supervisor:start_child/2 returned \{error,
  already_present\}.

  Own Id: OTP-8034 Aux Id: seq11307

- SSH no longer cause supervisor reports when a connection is terminated in a
  controlled manner.

  Own Id: OTP-8035 Aux Id: seq11308

## Ssh 1.1.2

### Fixed Bugs and Malfunctions

- Ssh confused local and remote channel id's, which in some cases resulted in
  that messages were discarded.

  Own Id: OTP-7914 Aux Id: seq11234

- Ssh could not handle echo values other than 0 and 1.

  Own Id: OTP-7917 Aux Id: seq11238

- A crash occurred if a non-valid channel reference was received.

  Own Id: OTP-7918 Aux Id: seq11238

- Sftpd connections was not closed after receiving eof from a client.

  Own Id: OTP-7921 Aux Id: seq11222

- It was not possible to start a SFTP subsystem on certain platforms, i.e. those
  who do not support symbolic links.

  Own Id: OTP-7930 Aux Id:

- In some cases the message \{ssh_cm, ssh_connection_ref(), \{closed,
  ssh_channel_id()\}\} was not passed to the registered callback module.

  Own Id: OTP-7957 Aux Id:

### Improvements and New Features

- By using the sftpd option \{max_files, Integer\}, the message size for READDIR
  commands can be reduced.

  Own Id: OTP-7919 Aux Id: seq11230

## Ssh 1.1.1

### Fixed Bugs and Malfunctions

- The erlang ssh server has presented itself incorrectly, using the special
  version ssh-1.99, although it never has supported versions below 2.0. Since
  ssh-1.1 client versions below 2.0 are correctly rejected instead of letting
  the server crash later on. Alas the problem with the presentation string was
  not discovered until after ssh.1.1 was released. Now the server will present
  itself as ssh-2.0.

  Own Id: OTP-7795

- An internal function call used an incorrect parameter, which caused problem
  when the old listen API was used. This was introduced in Ssh-1.1.

  Own Id: OTP-7920 Aux Id: seq11211

### Improvements and New Features

- Ssh timeouts will now behave as expected i.e. defaults to infinity only the
  user of the ssh application can know of a reasonable timeout value for their
  application.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7807

- The implementation of timeouts added as a patch in ssh-1.0.1 was slightly
  changed and is now documented.

  Own Id: OTP-7808

- To honor the multiplexing of channels over one ssh connection concept
  ssh_sftp:connect/ \[1,2,3] is deprecated and replaced by
  ssh_sftp:start_channel/\[1,2,3] and ssh_sftp:stop/1 is deprecated and replaced
  by ssh_sftp:stop_channel/1 and to stop the ssh connection ssh:close/ 1 should
  be called.

  Own Id: OTP-7809

- Added the message \{ssh_channel_up, ChannelId, ConnectionManager\} that shall
  be handled by the channel callback handle_msg/2. This makes the function
  handle_msg/2 a mandatory function for ssh channels implementations which it
  was not in ssh-1.1.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7828

## Ssh 1.1

### Fixed Bugs and Malfunctions

- A flaw in the implementation of the supervision tree caused the ssh daemon to
  close the connections to all currently logged in users if one user logged out.
  Another problem related to the supervision tree caused the closing down of
  clients to leak processes i.e. all processes was not shutdown correctly.

  Own Id: OTP-7676

- Tabs could cause ssh_cli to print things in a surprising way.

  Own Id: OTP-7683 Aux Id: seq11102

- \[sftp, sftpd] - Added patch to make sftp timestamps more correct, in the long
  run it would be nice to have better support in file to be able to make it
  always behave correctly now it will be correct 99 % of time instead of almost
  never correct, at least on unix-based platforms.

  Own Id: OTP-7685 Aux Id: seq11082

- \[sftpd] - Added patch to further improve handling of symbolic links in the
  sftp-server.

  Own Id: OTP-7766 Aux Id: seq11101

- Ssh incorrectly sent the local id instead of the remote id of a channel to the
  peer. For simpler cases these ids often happen to have the same value. One
  case when they do not is when the client sends an exec command two times in a
  raw on the same ssh connection (different channels of course as the channel
  will be closed when the exec command has been evaluated) .

  Own Id: OTP-7767

- Packet data could be lost under high load due to the fact that buffered data
  was sometimes wrongly discarded before it had been sent.

  Own Id: OTP-7768

- Improved ipv6-handling as some assumptions about inet functions where
  incorrect.

  Own Id: OTP-7770

### Improvements and New Features

- Added new API function ssh:connection_info/2.

  Own Id: OTP-7456

- Now starts ssh channel processes later avoiding synchronization problems
  between processes.

  Own Id: OTP-7516

- Ssh now rejects old versions of the ssh protocol for security reasons. (Even
  if they where not correctly rejected before the connection would probably have
  failed anyway due to other reasons.)

  Own Id: OTP-7645 Aux Id: seq11094

- New API module ssh_channel has been added. This is a behaviour to facilitate
  the implementation of ssh clients and plug in subsystems to the ssh daemon.
  Note that this slightly changes the options to the API function
  ssh:daemon/\[1,2,3] deprecating all no longer documented options. Note that
  the new API enforces the "logical way" of using the old API i.e. making the
  subsystem process part of the ssh applications supervisor tree, so missuses of
  the old API are not compatible with the new API.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7769

### Known Bugs and Problems

- Public keys protected by a password are currently not handled by the erlang
  ssh application.

  Own Id: OTP-6400 Aux Id: 10595

## Ssh 1.0.2

### Fixed Bugs and Malfunctions

- \[sftpd] - Listing of symbolic link directories should now work as expected.

  Own Id: OTP-7141 Aux Id: seq10856

## Ssh 1.0.1

### Fixed Bugs and Malfunctions

- \[sftp] - When listing a directory with more than 100 files only the first 100
  where listed. This has now been fixed.

  Own Id: OTP-7318 Aux Id: seq10953

- When restarting an ssh-system the expected return value from
  ssh_system_sup:restart_acceptor/2 was incorrect, this is no longer the case.

  Own Id: OTP-7564 Aux Id: seq11055

- A few minor bugs where fixed in ssh_userreg.erl and ssh_connection_manager and
  a a ssh_cli option was added to restore backwards compatibility with the old
  ssh_cm - API.

  Own Id: OTP-7565

- Fixed bug in ipv6 support and added option to disable ipv6 as a workaround for
  badly configured computers.

  Own Id: OTP-7566

### Improvements and New Features

- \[sftp] - Option added to set timeout value in sftp.

  Own Id: OTP-7305 Aux Id: seq10945

## Ssh 1.0

### Fixed Bugs and Malfunctions

- Removed some special handling of prompts that made ssh behave differently than
  openssh.

  Own Id: OTP-7485 Aux Id: seq11025

- Bug in encoding of pty opts has been fixed.

  Own Id: OTP-7504

### Improvements and New Features

- The architecture of the ssh processes has been reconstructed to fit in a
  supervision tree as to become a real OTP application and benefit from this
  when starting and stopping.

  Own Id: OTP-7356 Aux Id: seq10899

- Support for pty option echo off added. Requires kernel from R12B-4.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7502 Aux Id: seq10959

- The ssh API has been enhanced a lot of old API functions has become
  deprecated.

  Own Id: OTP-7503
