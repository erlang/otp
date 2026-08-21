<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2026-2026. All Rights Reserved.

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

## Introduction

The Erlang/OTP `inets` application provides an HTTP client (`httpc`) and an
HTTP server (`httpd`), intended to be used as libraries in other applications.

Different deployments have very different security requirements. A development
tool running on localhost has different needs than a service exposed to the
public internet. Because of this, `inets` ships with permissive defaults that
prioritize ease of use. This guide describes how to tighten those defaults for
production deployments.

This guide targets **OTP 29**. Where behavior differs from OTP 28 and earlier,
the differences are noted inline. A summary of changes is provided in the
[Changes from OTP 28](#changes-from-otp-28) section.

For general configuration, see the `m:httpc` and `m:httpd` reference manuals.

## HTTP Client (httpc)

### TLS Configuration

Since OTP 26.0, `httpc` verifies the server certificate by default when no
`{ssl, ...}` option is given. The default is equivalent to calling
`httpc:ssl_verify_host_options(true)`, which enables `verify_peer` with the
system CA store.

If you need to customize TLS settings (for example, to pin a specific CA or
restrict protocol versions), pass them explicitly:

```erlang
SslOpts = [{verify, verify_peer},
           {cacertfile, "/path/to/ca-bundle.crt"},
           {versions, ['tlsv1.2', 'tlsv1.3']}],
httpc:request(get, {"https://example.com", []}, [{ssl, SslOpts}], []).
```

> #### Warning {: .warning }
>
> Setting `{ssl, [{verify, verify_none}]}` disables certificate verification
> entirely. This makes the connection vulnerable to man-in-the-middle attacks
> and should only be used for testing.

### Timeouts

The default value for both `timeout` and `connect_timeout` is `infinity`,
meaning a request can hang indefinitely. Always set explicit timeouts in
production:

```erlang
HttpOpts = [{timeout, 30000},        %% 30 s total request timeout
            {connect_timeout, 5000}], %% 5 s TCP connect timeout
httpc:request(get, {"https://example.com", []}, HttpOpts, []).
```

- **[`timeout`](`m:httpc#opt_timeout`)** - Maximum time for the entire request (connect + send +
  receive). Default: `infinity`.

- **[`connect_timeout`](`m:httpc#opt_connect_timeout`)** - Maximum time for the TCP connection setup. Defaults
  to the value of `timeout`.

- **[`autoretry`](`m:httpc#opt_autoretry`)** *(since OTP 28.4)* - Controls how long the client honors a
  server's `Retry-After` header before retrying. The client automatically
  retries the request once on a 503 response with a `Retry-After` header.
  Default: `infinity` (always honor the server's value). Set to `0` to disable
  automatic retries, or to a finite number of milliseconds to cap the wait.
  If the server's `Retry-After` value exceeds the configured limit, no retry
  is performed. Before OTP 28.4, the retry behavior was hardcoded to only
  honor `Retry-After` values of 99 seconds or less.

  ```erlang
  HttpOpts = [{timeout, 30000},
              {connect_timeout, 5000},
              {autoretry, 10000}].  %% Wait at most 10 s for Retry-After
  ```

### Redirect Following

By default, `httpc` follows HTTP redirects automatically
([`{autoredirect, true}`](`m:httpc#opt_autoredirect`)). This can be exploited for Server-Side Request
Forgery (SSRF) if the client is used to fetch user-supplied URLs, because a
redirect can point to internal hosts or services.

For requests to untrusted URLs, disable automatic redirects and validate the
target manually. Check that the redirect target does not resolve to a private
network address (10.x, 127.x, 169.254.x, etc.) before following it:

```erlang
httpc:request(get, {UserUrl, []},
              [{autoredirect, false}, {timeout, 10000}], []).
```

### Client Connection Limits

Profile-level options control how many connections `httpc` keeps open. The
per-host defaults are conservative, but the global limit defaults to
`infinity`:

- **[`max_connections_open`](`m:httpc#opt_max_connections_open`)** *(since OTP 29)* - Maximum total number of open
  handlers across all hosts. Default: `infinity`. Set a finite value to
  prevent unbounded resource consumption:

  ```erlang
  httpc:set_options([{max_connections_open, 100}]).
  ```

- **[`max_sessions`](`m:httpc#opt_max_sessions`)** - Maximum persistent connections per host:port. Default:
  `2`.

- **[`max_keep_alive_length`](`m:httpc#opt_max_keep_alive_length`)** - Maximum queued requests on a keep-alive
  connection. Default: `5`.

- **[`max_pipeline_length`](`m:httpc#opt_max_pipeline_length`)** - Maximum pipelined requests per connection.
  Default: `2`.

- **[`pipeline_timeout`](`m:httpc#opt_pipeline_timeout`)** - Idle time before closing a pipelined connection, in
  milliseconds. Default: `0` (pipelining disabled). Pipelining is only used
  when this is set to a positive value.

- **[`keep_alive_timeout`](`m:httpc#opt_keep_alive_timeout`)** - Idle time before closing a persistent connection.
  Default: `120000` ms (2 minutes).

For a service making requests to many different hosts, set
`max_connections_open` to a value appropriate for your system's file
descriptor limits.

### Cookie Handling

Cookies are disabled by default (`{cookies, disabled}`). If you enable them,
prefer `verify` mode, which lets you inspect cookies before they are stored
via `httpc:store_cookies/3`:

```erlang
httpc:set_options([{cookies, verify}]).
```

### Proxy Configuration

When using a proxy, ensure that the `no_proxy` list excludes internal hosts
that should not be routed through the proxy:

```erlang
httpc:set_options([
    {proxy, {{"proxy.example.com", 8080},
             ["localhost", "*.internal.example.com"]}}
]).
```

For HTTPS traffic through a proxy, configure `https_proxy` separately. The
client uses the HTTP CONNECT method to establish a tunnel through the proxy.
By default, `https_proxy` inherits the value of `proxy`.

### Sensitive Options

> #### Warning {: .warning }
>
> The following options can introduce injection, file overwrite, or data
> leakage vulnerabilities if misused.

Per-request options:

- **[`{socket_opts, Opts}`](`m:httpc#opt_socket_opts`)** - Passed directly to the transport layer without
  validation. Avoid exposing this to untrusted input.

- **[`{headers_as_is, true}`](`m:httpc#opt_headers_as_is`)** - Bypasses header normalization. This can enable
  header injection if header values come from untrusted sources.

- **[`{stream, {self, once}}`](`m:httpc#opt_stream`)** or **`{stream, Filename}`** - Streaming to a
  file path could overwrite files. Validate paths before use.

Per-profile options:

- **[`{verbose, debug | trace}`](`m:httpc#opt_verbose`)** - May log sensitive data such as headers
  containing authorization tokens.

## HTTP Server (httpd)

### Enable TLS

By default, `httpd` listens in plaintext ([`{socket_type, ip_comm}`](`m:httpd#prop_socket_type`)). For any
deployment beyond localhost, enable TLS:

```erlang
httpd:start_service([{port, 8443},
                     {server_root, "/var/www"},
                     {document_root, "/var/www/htdocs"},
                     {socket_type, {ssl, [{cert_keys, [#{certfile => "/path/to/cert.pem",
                                                          keyfile => "/path/to/key.pem"}]},
                                          {versions, ['tlsv1.2', 'tlsv1.3']}]}}]).
```

### Bind Address

The default [`{bind_address, any}`](`m:httpd#prop_bind_address`) listens on all network interfaces. Restrict
this to the intended interface:

```erlang
{bind_address, {127, 0, 0, 1}}   %% Localhost only
```

or

```erlang
{bind_address, "192.168.1.10"}    %% Specific interface
```

### Request Size Limits

Several size limits default to `nolimit`, which allows arbitrarily large
requests that can exhaust memory. Set explicit limits:

```erlang
[{max_uri_size, 8192},              %% 8 KB URI limit
 {max_header_size, 10240},          %% 10 KB (already the default)
 {max_body_size, 10_485_760},       %% 10 MB body limit
 {max_content_length, 10_485_760}]  %% 10 MB Content-Length check
```

- **[`max_uri_size`](`m:httpd#prop_max_uri`)** - Maximum URI length in bytes. Default: `nolimit`.

- **[`max_header_size`](`m:httpd#prop_max_header_size`)** - Maximum total header size. Default: `10240` (10 KB).

- **[`max_body_size`](`m:httpd#prop_max_body_size`)** - Maximum received body size during parsing. Default:
  `nolimit`.

- **[`max_content_length`](`m:httpd#prop_max_content_length`)** - Rejects requests whose `Content-Length` header
  exceeds this value with a 413 response, before reading the body. Default:
  `100_000_000` (100 MB).

- **[`max_client_body_chunk`](`m:httpd#max_client_body_chunk`)** - When handling large PUT or POST bodies via
  `mod_esi`, setting this option enforces chunked delivery to the ESI
  callback. This prevents the server from buffering the entire request body
  in memory, which could be exploited to cause memory exhaustion.

> #### Note {: .info }
>
> `max_body_size` and `max_content_length` serve different purposes.
> `max_content_length` is a fast pre-check on the header value.
> `max_body_size` limits the actual bytes received. Set both for
> defense in depth.

### Server Connection Limits

- **[`max_clients`](`m:httpd#prop_max_clients`)** - Maximum simultaneous connections. Default: `150`. Tune
  this to your expected load and available resources.

- **[`keep_alive`](`m:httpd#prop_keep_alive`)** - Controls whether persistent connections are used. Default:
  `true`. Disabling persistent connections (`false`) eliminates certain classes
  of connection-reuse attacks at the cost of performance.

- **[`max_keep_alive_request`](`m:httpd#prop_max_keep_alive_req`)** - Maximum requests per persistent connection.
  Default: `infinity`. Set a finite value to prevent a single connection from
  monopolizing server resources:

  ```erlang
  {max_keep_alive_request, 1000}
  ```

- **[`keep_alive_timeout`](`m:httpd#prop_keep_alive_timeout`)** - Seconds before closing an idle persistent
  connection. Default: `150`.

- **[`minimum_bytes_per_second`](`m:httpd#prop_minimum_bytes_per_second`)** - Closes connections that transfer data below
  this rate. Not set by default. Enable it to mitigate slow-rate DoS attacks
  (for example, Slowloris):

  ```erlang
  {minimum_bytes_per_second, 100}
  ```

The following table summarizes when each limit is checked during the request
lifecycle:

| Stage | Event | Limit checked |
|-------|-------|---------------|
| 1 | TCP connected | `max_clients` |
| 2 | Headers received | `max_uri_size`, `max_header_size` |
| 3 | Content-Length checked | `max_content_length` |
| 4 | Body received | `max_body_size` |
| 5 | Idle on keep-alive | `keep_alive_timeout` |
| 6 | Slow transfer | `minimum_bytes_per_second` |

### Server Identity

The default [`{server_tokens, minimal}`](`m:httpd#prop_server_tokens`) reveals the complete `inets` version
string in the `Server` response header (for example, `inets/9.3.1`). Despite
the name, `minimal` still includes the full version number. This helps
attackers identify known vulnerabilities. Reduce information disclosure:

```erlang
{server_tokens, none}   %% Omit the Server header entirely
```

or

```erlang
{server_tokens, prod}   %% Just "inets", no version
```

### Module Chain

The [`{modules, ...}`](`m:httpd#prop_modules`) option controls which httpd modules are active. In OTP 29,
`mod_cgi` and `mod_actions` are deprecated and removed from the default module
list. The default is:

```erlang
[mod_alias, mod_auth, mod_esi,
 mod_dir, mod_get, mod_head, mod_log, mod_disk_log]
```

> #### Warning {: .warning }
>
> In OTP 28 and earlier, the default module list also includes `mod_cgi` and
> `mod_actions`, enabling CGI execution out of the box. If you are running an
> older release, removing these modules manually is especially important.

Review this list and remove modules you do not need:

- **`mod_cgi`** and **`mod_actions`** - Enable CGI script execution.
  Deprecated in OTP 29 and scheduled for removal in OTP 30. If you still
  need CGI support, add them explicitly to your module list. CGI introduces a
  large attack surface (arbitrary process execution, environment variable
  injection).

- **`mod_dir`** - Enables directory listing. Remove it to prevent information
  disclosure about your file structure.

- **`mod_esi`** - Enables Erlang Scripting Interface. If used, restrict it
  with [`erl_script_alias`](`m:httpd#prop_esi_alias`) to a whitelist of allowed modules.

- **`mod_trace`** - Handles HTTP TRACE requests. TRACE can be exploited in
  cross-site tracing (XST) attacks. This module is not in the default list
  but should never be added in production.

A minimal module chain for a static file server:

```erlang
{modules, [mod_alias, mod_get, mod_head, mod_log]}
```

### Authentication (mod_auth)

If using `mod_auth`:

- **Avoid [`{auth_type, plain}`](`m:httpd#prop_auth_type`)** in production. It stores passwords in
  cleartext files. Prefer `dets` or `mnesia`.

- **Place auth files outside `document_root`**. The [`auth_user_file`](`m:httpd#prop_auth_user_file`) and
  [`auth_group_file`](`m:httpd#prop_auth_group_file`) must not be accessible via HTTP.

- **Set [`auth_access_password`](`m:httpd#prop_auth_access_passwd`)** to a strong value. When not set or set to
  `"NoPassword"`, no password is required for the authentication management
  API.

- **Use IP-based restrictions** ([`allow_from`](`m:httpd#prop_allow_from`), [`deny_from`](`m:httpd#prop_deny_from`)) as an additional
  layer, not as the sole access control mechanism.

> #### Warning {: .warning }
>
> The `mod_auth` module implements HTTP Basic Authentication, which transmits
> credentials in base64 encoding (effectively cleartext). **Always use TLS**
> when authentication is enabled.

### Brute Force Protection (mod_security)

`mod_security` acts as a filter on top of `mod_auth`, tracking failed login
attempts per user and temporarily blocking users who exceed a threshold. This
mitigates credential-stuffing and brute-force password guessing attacks.

Enable `mod_security` to throttle authentication brute force attempts:

```erlang
{security_directory, {"/protected", [
    {data_file, "/var/lib/httpd/security.dat"},
    {max_retries, 3},
    {block_time, 60},        %% Block for 60 minutes
    {fail_expire_time, 30},  %% Remember failures for 30 minutes
    {auth_timeout, 30}
]}}
```

- **[`data_file`](`m:httpd#prop_data_file`)** - Path to the persistent security
  data file. Store this outside `document_root`. Required for `mod_security`
  to persist blocked-user state across server restarts.

- **[`max_retries`](`m:httpd#prop_max_retries`)** - Maximum failed authentication
  attempts before the user is blocked. Default: `3`.

- **[`block_time`](`m:httpd#prop_block_time`)** - Minutes a blocked user remains
  locked out. Default: `60`.

- **[`fail_expire_time`](`m:httpd#prop_fail_exp_time`)** - Minutes before a
  failed attempt is forgotten. If the user does not retry within this window,
  the failure counter resets. Default: `30`.

- **[`auth_timeout`](`m:httpd#prop_auth_timeout`)** - Seconds a successful
  authentication is remembered. After expiry the user must re-authenticate.
  Default: `30`.

> #### Warning {: .warning }
>
> `mod_security` must appear **after** `mod_auth` in the module chain.
> It relies on `mod_auth` to perform the actual authentication; `mod_security`
> only observes the results and enforces blocking policy.

For runtime inspection and manual blocking, see `m:mod_security`
([`list_blocked_users/1`](`mod_security:list_blocked_users/1`), [`block_user/5`](`mod_security:block_user/5`), [`unblock_user/4`](`mod_security:unblock_user/4`)).

### CGI and ESI Execution

- **[`script_alias`](`m:httpd#prop_script_alias`)** maps URL paths to CGI script directories. Ensure the
  mapped directory contains only intended scripts and is not writable by the
  web server process.

- **[`erl_script_alias`](`m:httpd#prop_esi_alias`)** controls which Erlang modules can be called via ESI.
  Always specify an explicit whitelist:

  ```erlang
  {erl_script_alias, {"/esi", [my_allowed_module]}}
  ```

  Never use a wildcard or overly broad module list.

- **[`script_timeout`](`m:httpd#prop_script_timeout`)** and **[`erl_script_timeout`](`m:httpd#prop_esi_timeout`)** default to 15 seconds.
  Review whether this is appropriate for your use case.

- **[`script_nocache`](`m:httpd#prop_script_nocache`)** and **[`erl_script_nocache`](`m:httpd#prop_esi_nocache`)** - When set to `true`, the
  server adds HTTP header fields preventing proxies from caching dynamic
  responses. Default: `false`. Enable these to prevent stale or sensitive
  dynamic content from being served from proxy caches.

> #### Warning {: .warning }
>
> The `script_alias` path resolution can bypass `mod_auth` directory
> protections depending on module ordering. Ensure `mod_auth` appears
> before `mod_cgi` in the module chain, and test that authentication
> is enforced on CGI paths.

### Logging

Enable logging to detect and investigate security incidents:

```erlang
[{error_log, "/var/log/httpd/error.log"},
 {security_log, "/var/log/httpd/security.log"},
 {transfer_log, "/var/log/httpd/access.log"},
 {log_format, combined}]  %% Includes referer and user-agent (default: common)
```

For production systems, the `m:disk_log` variants (`transfer_disk_log`,
`error_disk_log`, `security_disk_log`) are recommended as they support wrap
logs with configurable size limits, preventing log files from consuming all
available disk space.

For integration with the OTP logger framework, where `my_httpd` is the
`ServerID` atom used in the logger domain hierarchy
`[otp, inets, httpd, ServerID, error]`:

```erlang
{logger, [{error, my_httpd}]}
```

Ensure log files are rotated and that log directories are not writable by the
web server process.

## Hardened Example

The following is a complete example combining the recommendations above for an
httpd deployment serving static files with TLS:

```erlang
httpd:start_service([
    {port, 8443},
    {bind_address, {127, 0, 0, 1}},
    {server_root, "/var/www"},
    {document_root, "/var/www/htdocs"},
    {server_tokens, none},
    {socket_type, {ssl, [{cert_keys, [#{certfile => "/path/to/cert.pem",
                                        keyfile => "/path/to/key.pem"}]},
                         {versions, ['tlsv1.2', 'tlsv1.3']}]}},
    {modules, [mod_alias, mod_auth, mod_security, mod_get, mod_head, mod_log]},
    {max_clients, 100},
    {max_keep_alive_request, 1000},
    {keep_alive_timeout, 60},
    {max_uri_size, 8192},
    {max_header_size, 10240},
    {max_body_size, 10_485_760},
    {max_content_length, 10_485_760},
    {minimum_bytes_per_second, 100},
    {error_log, "/var/log/httpd/error.log"},
    {transfer_log, "/var/log/httpd/access.log"},
    {log_format, combined}
]).
```

And a hardened `httpc` request:

```erlang
HttpOpts = [{timeout, 30000},
            {connect_timeout, 5000},
            {autoredirect, false},
            {autoretry, 10000},
            {ssl, [{verify, verify_peer},
                   {cacerts, public_key:cacerts_get()},
                   {versions, ['tlsv1.2', 'tlsv1.3']}]}],
httpc:request(get, {"https://example.com", []}, HttpOpts, []).
```

## General Recommendations

### Defense in Depth

The `inets` application provides application-level security controls. For
production deployments, combine these with:

- **Network-level controls** - Firewalls, network segmentation, and rate
  limiting.
- **OS-level controls** - Run the BEAM VM as an unprivileged user. Use file
  system permissions to protect configuration files, log files, and credential
  stores.
- **Reverse proxy** - Consider placing `httpd` behind a dedicated reverse proxy
  (such as Nginx or HAProxy) that provides additional request filtering, rate
  limiting, and TLS termination.

### Monitoring

Use the OTP `m:logger` framework to monitor for:

- Repeated authentication failures (potential brute-force attacks).
- Unusual request patterns (potential scanning or fuzzing).
- Connection limit exhaustion (potential DoS).

### Keep Up to Date

Security vulnerabilities are fixed in new releases of Erlang/OTP. Monitor the
[Erlang/OTP releases](https://github.com/erlang/otp/releases) and apply updates
promptly.

## Changes from OTP 28

If you are upgrading from OTP 28 or running an older release, be aware of the
following differences:

### httpc

- **`autoretry` not available before OTP 28.4** - The `{autoretry, timeout()}`
  option for `httpc` was introduced in OTP 28.4. On older releases, the client
  still retries on 503 with `Retry-After`, but only honors values of 99 seconds
  or less (hardcoded). The new option allows configuring this limit or
  disabling retries entirely with `{autoretry, 0}`.

- **`max_connections_open` not available before OTP 29** - The
  `{max_connections_open, integer()}` profile option for `httpc` was introduced
  in OTP 29. On older releases, there is no global limit on the number of open
  handlers.

- **TLS verification default** - `httpc` has verified server certificates by
  default since OTP 26.0. On releases before 26.0, you must pass
  `{ssl, httpc:ssl_verify_host_options(true)}` explicitly or connections will
  proceed without certificate verification.

### httpd

- **`mod_cgi` and `mod_actions` in default modules** - In OTP 28 and earlier,
  the default `{modules, ...}` list includes `mod_cgi` and `mod_actions`,
  enabling CGI execution by default. In OTP 29 these modules are deprecated
  and removed from the defaults. On older releases, remove them explicitly.
