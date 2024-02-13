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
# ssl

The ssl application provides secure communication over sockets.

## Description

The ssl application is an implementation of the SSL, TLS and DTLS protocols in
Erlang.

For current statement of standards compliance see the
[User's Guide](standards_compliance.md).

## DEPENDENCIES

The SSL application uses the `public_key`, `asn1` and Crypto application to
handle public keys and encryption, hence these applications must be loaded for
the SSL application to work. In an embedded environment this means they must be
started with `application:start/[1,2]` before the SSL application is started.

## CONFIGURATION

The application environment configuration parameters in this section are defined
for the SSL application. For more information about configuration parameters,
see the `m:application` manual page in Kernel.

The environment parameters can be set on the command line, for example:

`erl -ssl protocol_version "['tlsv1.2', 'tlsv1.1']"`

- **`protocol_version = ``t:ssl:tls_version/0` | [`t:ssl:tls_version/0`]
  `<optional>`** - Protocol supported by started clients and servers. If this
  option is not set, it defaults to all TLS protocols currently supported, more
  might be configurable, by the SSL application. This option can be overridden
  by the version option to `ssl:connect/[2,3]` and `ssl:listen/2`.

- **`dtls_protocol_version = ``t:ssl:dtls_version/0` | [`t:ssl:dtls_version/0`]
  `<optional>`** - Protocol supported by started clients and servers. If this
  option is not set, it defaults to all DTLS protocols currently supported, more
  might be configurable, by the SSL application. This option can be overridden
  by the version option to `ssl:connect/[2,3]` and `ssl:listen/2`.

- **`session_lifetime = integer() <optional>`** - Maximum lifetime of the
  session data in seconds. Defaults to 24 hours which is the maximum recommended
  lifetime by [RFC 5246](http://www.ietf.org/rfc/5246rfc.txt). However sessions
  may be invalidated earlier due to the maximum limitation of the session cache
  table.

- **`session_cb = atom() <optional>`** - Deprecated Since OTP-23.3 replaced by
  `client_session_cb` and `server_session_cb`

- **`client_session_cb = atom() <optional>`** - Since OTP-23.3 Name client of
  the session cache callback module that implements the `ssl_session_cache_api`
  behavior. Defaults to `ssl_client_session_cache_db`.

- **`server_session_cb = atom() <optional>`** - Since OTP-23.3 Name of the
  server session cache callback module that implements the
  `ssl_session_cache_api` behavior. Defaults to `ssl_server_session_cache_db`.

- **`session_cb_init_args = proplist:proplist() <optional>`** - Deprecated Since
  OTP-23.3 replaced by `client_session_cb_init_args` and
  `server_session_cb_init_args`

- **`client_session_cb_init_args = proplist:proplist() <optional>`** - List of
  extra user-defined arguments to the `init` function in the session cache
  callback module. Defaults to `[]`.

- **`server_session_cb_init_args = proplist:proplist() <optional>`** - List of
  extra user-defined arguments to the `init` function in the session cache
  callback module. Defaults to `[]`.

- **`session_cache_client_max = integer() <optional>`  
  **  
  Limits the growth of the clients session cache, that is how many sessions
  towards servers that are cached to be used by new client connections. If the
  maximum number of sessions is reached, the current cache entries will be
  invalidated regardless of their remaining lifetime. Defaults to 1000.
  Recommended ssl-8.2.1 or later for this option to work as intended.

- **`session_cache_server_max = integer() <optional>`** - Limits the growth of
  the servers session cache, that is how many client sessions are cached by the
  server. If the maximum number of sessions is reached, the current cache
  entries will be invalidated regardless of their remaining lifetime. Defaults
  to 1000. Recommended ssl-8.2.1 or later for this option to work as intended.

- **`ssl_pem_cache_clean = integer() <optional>`** - Number of milliseconds
  between PEM cache validations. Defaults to 2 minutes.

  Note: The cache can be reloaded by calling `ssl:clear_pem_cache/0`.

- **`bypass_pem_cache = boolean() <optional>`** - Introduced in ssl-8.0.2.
  Disables the PEM-cache. Can be used as a workaround for the PEM-cache
  bottleneck before ssl-8.1.1. Defaults to false.

- **`alert_timeout = integer() <optional>`** - Number of milliseconds between
  sending of a fatal alert and closing the connection. Waiting a little while
  improves the peers chances to properly receiving the alert so it may shutdown
  gracefully. Defaults to 5000 milliseconds.

- **`internal_active_n = integer() <optional>`** - For TLS connections this
  value is used to handle the internal socket. As the implementation was changed
  from an active once to an active N behavior (N = 100), for performance
  reasons, this option exist for possible tweaking or restoring of the old
  behavior (internal_active_n = 1) in unforeseen scenarios. The option will not
  affect erlang distribution over TLS that will always run in active N mode.
  Added in ssl-9.1 (OTP-21.2).

- **`server_session_tickets_amount = integer() <optional>`** - Number of session
  tickets sent by the server. It must be greater than 0. Defaults to 3.

- **`server_session_ticket_lifetime = integer() <optional>`** - Lifetime of
  session tickets sent by the server. Servers must not use any value greater
  than 604800 seconds (7 days). Expired tickets are automatically removed.
  Defaults to 7200 seconds (2 hours).

- **`server_session_ticket_store_size = integer() <optional>`** - Sets the
  maximum size of the server session ticket store (stateful tickets). Defaults
  to 1000. Size limit is enforced by dropping old tickets.

- **`server_session_ticket_max_early_data = integer() <optional>`** - Sets the
  maximum size of the early data that the server accepts and also configures its
  NewSessionTicket messages to include this same size limit in their
  early_data_indication extension. Defaults to 16384. Size limit is enforced by
  both client and server.

- **`client_session_ticket_lifetime = integer() <optional>`** - Lifetime of
  session tickets in the client ticket store. Expired tickets are automatically
  removed. Defaults to 7200 seconds (2 hours).

- **`client_session_ticket_store_size = integer() <optional>`** - Sets the
  maximum size of the client session ticket store. Defaults to 1000. Size limit
  is enforced by dropping old tickets.

## ERROR LOGGER AND EVENT HANDLERS

The SSL application uses [OTP logger](`m:logger`). TLS/DTLS alerts are logged on
notice level. Unexpected errors are logged on error level. These log entries
will by default end up in the default Erlang log. The option `log_level` may be
used to in run-time to set the log level of a specific TLS connection, which is
handy when you want to use level debug to inspect the TLS handshake setup.

## SEE ALSO

`m:application`
