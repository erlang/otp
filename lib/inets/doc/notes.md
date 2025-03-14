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
# Inets Release Notes

## Inets 9.3.2

### Fixed Bugs and Malfunctions

- Improved robustness of httpd startup procedure.

  Own Id: OTP-19486 Aux Id: ERIERL-1190, [PR-9408]

[PR-9408]: https://github.com/erlang/otp/pull/9408

## Inets 9.3.1

### Fixed Bugs and Malfunctions

- The HTTP client now correctly takes into account the `full_result` option when returning an asynchronous request.

  *** POTENTIAL INCOMPATIBILITY ***

  Own Id: OTP-19158

- A synchronous httpc:request now timeouts after the `Timeout` specified in `HttpOption {timeout, Timeout}`.

  Own Id: OTP-19221 Aux Id: ERIERL-1091, [PR-8788], [PR-8801]

- Fixed a bug where calling httpc:set_options/2 when one of keys: *ipfamily* or *unix_socket*, was not present, would cause the other value to get overriden by the default value. The validation of these options was also improved.

  Own Id: OTP-19379 Aux Id: [PR-8878], [GH-8829]

[PR-8788]: https://github.com/erlang/otp/pull/8788
[PR-8801]: https://github.com/erlang/otp/pull/8801
[PR-8878]: https://github.com/erlang/otp/pull/8878
[GH-8829]: https://github.com/erlang/otp/issues/8829

### Improvements and New Features

- The variable `Env` in the `mod_esi` callback will now have an additional property `{connect_addr, Addr}` indicating on which address the server received a connection.

  Own Id: OTP-19377 Aux Id: ERIERL-1152, [PR-9127]

[PR-9127]: https://github.com/erlang/otp/pull/9127

## Inets 9.3

### Improvements and New Features

- The documentation for the `m:httpd` module has been improved, along with correction of headings and types.

  Own Id: OTP-19171 Aux Id: [PR-8578]

- Userinfo is now properly percent-decoded before usage in headers.

  Own Id: OTP-19172 Aux Id: [PR-8575]

[PR-8578]: https://github.com/erlang/otp/pull/8578
[PR-8575]: https://github.com/erlang/otp/pull/8575

## Inets 9.2

### Improvements and New Features

- Introduced a default value for httpd_server name configuration to improve ease of use.

  *** POTENTIAL INCOMPATIBILITY ***

  Own Id: OTP-18641 Aux Id: [PR-7316]

- The `httpd` module has been extended with an API for simple serving directory content over HTTP. With this change, the current working directory can be served like this:
  
  ```text
  erl -S httpd
  ```
  
  An arbitrary directory can be served like this:
  
  ```text
  erl -S httpd serve path/to/dir
  ```

  Own Id: OTP-18727 Aux Id: [PR-7299]

- Added `-callback` attributes to `m:httpd`, `m:mod_esi`, and `m:mod_security`.

  Own Id: OTP-18786 Aux Id: [PR-7700]

- Inets now uses a relative redirect with an absolute path to prevent whoever is running Inets from having to configure the `ServerName` to match the network-reachable host name of the server.

  Own Id: OTP-18809 Aux Id: [GH-7617], [PR-7678]

- `inets` processes now use `proc_lib:set_label/1` to improve observeability.

  Own Id: OTP-18927 Aux Id: [PR-8029]

- The documentation has been migrated to use Markdown and ExDoc.

  Own Id: OTP-18955 Aux Id: [PR-8026]

- The implementations of `http_uri:encode/1` and `http_uri:decode/1` are now replaced with their equivalent, but bug free versions from module `m:uri_string`, namely `uri_string:quote/1` and `uri_string:unquote/1`.

  *** POTENTIAL INCOMPATIBILITY ***

  Own Id: OTP-19022

- With this change, the API specs are updated.

  Own Id: OTP-19033

[PR-7316]: https://github.com/erlang/otp/pull/7316
[PR-7299]: https://github.com/erlang/otp/pull/7299
[PR-7700]: https://github.com/erlang/otp/pull/7700
[GH-7617]: https://github.com/erlang/otp/issues/7617
[PR-7678]: https://github.com/erlang/otp/pull/7678
[PR-8029]: https://github.com/erlang/otp/pull/8029
[PR-8026]: https://github.com/erlang/otp/pull/8026

## Inets 9.1.0.2

### Fixed Bugs and Malfunctions

* Fixed a bug where calling httpc:set_options/2 when one of keys: *ipfamily* or *unix_socket*, was not present, would cause the other value to get overriden by the default value. The validation of these options was also improved.

  Own Id: OTP-19379 Aux Id: PR-8878, GH-8829

## Inets 9.1.0.1

### Fixed Bugs and Malfunctions

* With this change, HTTP client, when returning an asynchronous request, now correctly takes into account \`OptionRequest - full_result\`

  \*** POTENTIAL INCOMPATIBILITY ***

  Own Id: OTP-19158
* With this change, synchronous httpc:request now timeouts after \`Timeout\` specified in \`HttpOption \{timeout, Timeout\}\`

  Own Id: OTP-19221 Aux Id: ERIERL-1091, PR-8788, PR-8801

## Inets 9.1

### Fixed Bugs and Malfunctions

- Replaced unintentional Erlang Public License 1.1 headers in some files with
  the intended Apache License 2.0 header.

  Own Id: OTP-18815 Aux Id: PR-7780

- Correct IP protocol handling so that redirects always uses correct IP-family
  options and not fails.

  Own Id: OTP-18855

### Improvements and New Features

- `inets` app starts `ssl` by default

  Own Id: OTP-18735 Aux Id: PR-7596, GH-7580

- Avoid httpd returning 500 internal server error when unable to open a file.
  404 or 503 will be returned instead.

  Own Id: OTP-18882

- Properly handle documented option mime_type, for backwards compatibility
  fallback to undocumented option default_type if mime_type is not set.

  Own Id: OTP-18891 Aux Id: PR-7843, GH-7827

## Inets 9.0.2

### Fixed Bugs and Malfunctions

- With this change, re_write httpd works as expected and does not return error.

  Own Id: OTP-18582 Aux Id: GH-6074,PR-6892

- Fixed a bug so `httpd` does not crash when stopped at the wrong time during
  TLS connection negotiation, or any other theoretically as slow connection
  setup.

  Own Id: OTP-18688 Aux Id: ERIERL-962

- Enhance error handling and avoid that the HTTP client hangs on headers
  provided on the wrong format.

  Own Id: OTP-18694 Aux Id: GH-7482

- With this change, error report generated by httpd during connection setup
  contains socket type information.

  Own Id: OTP-18704 Aux Id: ERIERL-962, PR-7513, OTP-18688

- Stop and restart of the `httpd` server in the Inets application has been
  refactored to a more synchronous and OTP supervisor friendly approach.

  This should increase stability and for example avoid a supervisor report from
  `httpd_connection_sup` about killed child process(es) in some cases when
  stopping or restarting `httpd`.

  Own Id: OTP-18708 Aux Id: ERIERL-962, OTP-18688

## Inets 9.0.1

### Fixed Bugs and Malfunctions

- Do not make the default ssl options by calling
  `httpc:ssl_verify_host_options(true)` if ssl options are supplied by the user.

  Own Id: OTP-18604 Aux Id: PR-7306 GH-7303

## Inets 9.0

### Fixed Bugs and Malfunctions

- Correct timing related pipelining/keepalive queue bug, that could result in
  unexpected "socket_remotly_closed" errors.

  Own Id: OTP-18476 Aux Id: GH-6380

### Improvements and New Features

- By default ssl connections will use options from `ssl_default_options(true)`

  Own Id: OTP-18167

- Runtime dependencies have been updated.

  Own Id: OTP-18350

- Deprecates `dbg:stop_clear/0` because it is simply a function alias to
  `dbg:stop/0`

  Own Id: OTP-18478 Aux Id: GH-6903

- The implementation has been fixed to use `proc_lib:init_fail/2,3` where
  appropriate, instead of `proc_lib:init_ack/1,2`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18490 Aux Id: OTP-18471, GH-6339, PR-6843

- IP display string will now always be in lower case, effects ipv6 addresses.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18530

- Removed deprecated functions

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18536

- Update the code in the inets example directory to reflect the latest
  implementation

  Own Id: OTP-18544 Aux Id: GH-5276

## Inets 8.3.1.5

### Fixed Bugs and Malfunctions

* Fixed a bug where calling httpc:set_options/2 when one of keys: *ipfamily* or *unix_socket*, was not present, would cause the other value to get overriden by the default value. The validation of these options was also improved.

  Own Id: OTP-19379 Aux Id: PR-8878, GH-8829

## Inets 8.3.1.4

### Fixed Bugs and Malfunctions

* With this change, HTTP client, when returning an asynchronous request, now correctly takes into account \`OptionRequest - full_result\`

  \*** POTENTIAL INCOMPATIBILITY ***

  Own Id: OTP-19158
* With this change, synchronous httpc:request now timeouts after \`Timeout\` specified in \`HttpOption \{timeout, Timeout\}\`

  Own Id: OTP-19221 Aux Id: ERIERL-1091, PR-8788, PR-8801

## Inets 8.3.1.3

### Fixed Bugs and Malfunctions

* Fixed runtime dependencies.

  Own Id: OTP-19064

## Inets 8.3.1.2

### Fixed Bugs and Malfunctions

- With this change, error report generated by httpd during connection setup
  contains socket type information.

  Own Id: OTP-18704 Aux Id: ERIERL-962, PR-7513, OTP-18688

- Stop and restart of the `httpd` server in the Inets application has been
  refactored to a more synchronous and OTP supervisor friendly approach.

  This should increase stability and for example avoid a supervisor report from
  `httpd_connection_sup` about killed child process(es) in some cases when
  stopping or restarting `httpd`.

  Own Id: OTP-18708 Aux Id: ERIERL-962, OTP-18688

## Inets 8.3.1.1

### Fixed Bugs and Malfunctions

- Fixed a bug so `httpd` does not crash when stopped at the wrong time during
  TLS connection negotiation, or any other theoretically as slow connection
  setup.

  Own Id: OTP-18688 Aux Id: ERIERL-962

## Inets 8.3.1

### Fixed Bugs and Malfunctions

- Correct timing related pipelining/keepalive queue bug, that could result in
  unexpected "socket_remotly_closed" errors.

  Own Id: OTP-18509 Aux Id: OTP-18476

- With this change, upon remote socket closure current request is added to a
  retried queue (either pipeline or keep_alive, but not both).

  Own Id: OTP-18545 Aux Id: OTP-18509, ERIERL-937, ERIERL-928

## Inets 8.3

### Fixed Bugs and Malfunctions

- With this change, handling of URI to a folder, with missing trailing / and a
  query component present is fixed.

  Own Id: OTP-18472 Aux Id: DAFH-1592

### Improvements and New Features

- Adds more type information to the `inets` app, thus improving the errors that
  static analysis tools can detect.

  The addition of type information to records and the updates to function heads
  help static analysis tools to understand that some values in the records
  cannot be `'undefined'`, thus making static tools to type check correctly more
  modules in the `inets` app

  Own Id: OTP-18390 Aux Id: PR-6661

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

## Inets 8.2.2

### Fixed Bugs and Malfunctions

- Ensure graceful shutdown

  Own Id: OTP-18461 Aux Id: ERIERL-890

- Return type of the type specification for function
  `httpc:cookie_header/{1,2,3}` has been fixed from
  `-spec cookie_header(url()) -> [{ field(), value() }] | {error, Reason}` to
  `-spec cookie_header(url()) -> { field(), value() } | {error, Reason}`

  Own Id: OTP-18462 Aux Id: GH-6846

## Inets 8.2.1

### Fixed Bugs and Malfunctions

- fixes a missing case of the type specification for httpd:info/2/3/4

  Own Id: OTP-18362 Aux Id: GH-6558, ERIERL-895

## Inets 8.2

### Improvements and New Features

- This change allows body requests to `httpc:request/5` be an `t:iolist/0`

  Own Id: OTP-18250

- addition of type specs in `httpc.erl`

  Own Id: OTP-18251 Aux Id: GH-6245

- httpc: Add support for HTTP 308 status code

  Own Id: OTP-18280 Aux Id: GH-6290, PR-6291

## Inets 8.1

### Improvements and New Features

- Add `httpc:ssl_verify_host_options/1` to help setting default ssl options for
  the https client.

  Own Id: OTP-18118

- This change fixes dialyzer warnings generated for inets/httpd examples
  (includes needed adjustment of spec for ssh_sftp module).

  Own Id: OTP-18178 Aux Id: ERIERL-833, ERIERL-834, ERIERL-835

- Remove documentation of no longer supported callback.

  Own Id: OTP-18193 Aux Id: GH-6122

## Inets 8.0

### Fixed Bugs and Malfunctions

- Adjust uri_string:normalize behavior for URIs with undefined port (URI string
  with a port colon but no port value or URI map with port => undefined).

  Remove redundant normalization from http_request module.

  Before this change, normalize would not remove port subcomponent in such cases
  and could for example return "http://localhost:" URI.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17627

- Fixed typo in Reason term returned from httpc_handler:handle_http_body.

  After this change, could_not_establish_ssl_tunnel atom is returned within
  Reason term.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17889

- With this change, inet6fb4 option is documented for inets/httpc. Option can be
  used when IP family needs to be discovered by a connection attempt.

  Own Id: OTP-18063 Aux Id: ERIERL-798

### Improvements and New Features

- This change removes deprecated functions: http_uri:parse/1, http_uri:parse/2
  and http_uri:scheme_defaults/0.

  This change delays until OTP-26 removal of deprecated functions:
  http_uri:encode/1 and http_uri:decode/1.

  This change marks httpd_util:decode_hex/1 and httpd_util:encode_hex/1 as
  deprecated.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-17866

- After this change, connect_timeout value is re-used when upgrading TCP
  connection to TLS over a proxy.

  Own Id: OTP-17997 Aux Id: GH-5782

- Remove reference to unsupported Apache-like config file from httpd manual.

  Own Id: OTP-18088 Aux Id: GH-5276

## Inets 7.5.3.4

### Fixed Bugs and Malfunctions

- With this change, upon remote socket closure current request is added to a
  retried queue (either pipeline or keep_alive, but not both).

  Own Id: OTP-18545 Aux Id: OTP-18509, ERIERL-937, ERIERL-928

## Inets 7.5.3.3

### Fixed Bugs and Malfunctions

- Correct timing related pipelining/keepalive queue bug, that could result in
  unexpected "socket_remotly_closed" errors.

  Own Id: OTP-18509 Aux Id: OTP-18476

## Inets 7.5.3.2

### Fixed Bugs and Malfunctions

- With this change, handling of URI to a folder, with missing trailing / and a
  query component present is fixed.

  Own Id: OTP-18472 Aux Id: DAFH-1592

## Inets 7.5.3.1

### Improvements and New Features

- Remove documentation of no longer supported callback.

  Own Id: OTP-18193 Aux Id: GH-6122

## Inets 7.5.3

### Fixed Bugs and Malfunctions

- Fix handling of erl_script_script option in httpd when atom 'all' is used as
  AllowedModule.

  Own Id: OTP-18069 Aux Id: ERIERL-805

## Inets 7.5.2

### Fixed Bugs and Malfunctions

- The compilation time is no longer recorded in BEAM files. There remained
  several undocumented functions that attempted to retrieve compilation times.
  Those have now been removed.

  Own Id: OTP-17962

### Improvements and New Features

- Documentation fix for inets:services_info/0, which now describes that Info
  might be a Reason term() in case when \{error, Reason\} is returned as service
  info.

  Own Id: OTP-17931 Aux Id: ERIERL-761

## Inets 7.5.1

### Fixed Bugs and Malfunctions

- Avoid intermediate ungraceful shutdown of the HTTP server.

  Own Id: OTP-17922 Aux Id: ERIERL-743

## Inets 7.5

### Fixed Bugs and Malfunctions

- Correct HTTP server URI handling to fully rely on uri_string. The server could
  mistreat some URI paths that in turn could result in incorrect responses being
  generated.

  Own Id: OTP-17818 Aux Id: ERIERL-731

### Improvements and New Features

- Extend header values to httpc:request/5 to allow binary() as well. Make error
  detection of invalid arguments to httpc:request/5 be more precise so an error
  is returned in more cases instead of causing a hang or function_clause. Be
  more precise in documentation regarding the types of arguments being accepted.

  Own Id: OTP-17579 Aux Id: GH-5074

## Inets 7.4.2

### Fixed Bugs and Malfunctions

- Before this change hrefs in dir listing page contained percentage encoded
  forward slashes which did not work properly with httpd.

  Own Id: OTP-17383 Aux Id: GH-4677

- Restored HTTP headers handling in inets/mod_esi.

  Own Id: OTP-17600

- inets/httpd dir listing icons and other improvements

  Own Id: OTP-17624 Aux Id: GH-4855

### Improvements and New Features

- httpc: Improve performance by removing redundant URI handling

  Own Id: OTP-17460

## Inets 7.4.1

### Fixed Bugs and Malfunctions

- Improved user input handling in inets/mod_esi preventing unnecessary atom
  creation.

  Own Id: OTP-17490

## Inets 7.4

### Improvements and New Features

- Drop all support for ftp and tftp in inets code.

  Own Id: OTP-16722

- Deprecate following functions in `httpd_util` module: `flatlength/1`,
  `lhexlist_to_integer/1`, `integer_to_hexlist/1`, `strip/1`, and `suffix/1`.

  Own Id: OTP-16723

- Remove support of HTTP 0.9 in httpd.

  Own Id: OTP-16724

- Remove support of HTTP 0.9 in httpc.

  Own Id: OTP-16725

- Fixed warnings in code matching on underscore prefixed variables.

  Own Id: OTP-17385 Aux Id: OTP-17123

## Inets 7.3.2.3

### Improvements and New Features

- Remove documentation of no longer supported callback.

  Own Id: OTP-18193 Aux Id: GH-6122

## Inets 7.3.2.2

### Fixed Bugs and Malfunctions

- Restored HTTP headers handling in inets/mod_esi.

  Own Id: OTP-17600

## Inets 7.3.2.1

### Fixed Bugs and Malfunctions

- Improved user input handling in inets/mod_esi preventing unnecessary atom
  creation.

  Own Id: OTP-17490

## Inets 7.3.2

### Fixed Bugs and Malfunctions

- Solves CVE-2021-27563, that is make sure no form of relative path can be used
  to go outside webservers directory.

  Own Id: OTP-17205 Aux Id: ERIERL-608

- Make sure HEAD requests rejects directory links

  Own Id: OTP-17220

## Inets 7.3.1

### Fixed Bugs and Malfunctions

- Fix an issue about HTML-escaped filename in inets.

  Own Id: OTP-16873 Aux Id: ERL-330

## Inets 7.3

### Fixed Bugs and Malfunctions

- Clarify the handling of percent encoded characters in http client.

  Own Id: OTP-16650 Aux Id: ERL-1215, PR-2629

- fix crash for undefined port in uri.

  Own Id: OTP-16663 Aux Id: ERL-1241

- Avoid timing issue when setting active once on a socket that is being closed
  by the peer.

  Own Id: OTP-16735 Aux Id: OTP-16697, ERIERL-496

- Handle message body of response with 1XX status code as next http message.

  Own Id: OTP-16746 Aux Id: ERL-1268

- Fix a crash in http server when setopts is called on a socket closed by the
  peer.

  Own Id: OTP-16775 Aux Id: ERIERL-519

- A vulnerability in the httpd module (inets application) regarding directory
  traversal that was introduced in OTP 22.3.1 and corrected in OTP 22.3.4.6. It
  was also introduced in OTP 23.0 and corrected in OTP 23.1 The vulnerability is
  registered as CVE-2020-25623

  The vulnerability is only exposed if the http server (httpd) in the inets
  application is used. The vulnerability makes it possible to read arbitrary
  files which the Erlang system has read access to with for example a specially
  prepared http request.

  Own Id: OTP-16790 Aux Id: ERIERL-522

### Improvements and New Features

- Add support of PATCH method in mod_esi.

  Own Id: OTP-16591 Aux Id: ERIERL-484

## Inets 7.2

### Improvements and New Features

- Remove support for deprecated functionality. Support for mod_esi eval scheme,
  mod_htacess, mod_browser, apache config files and deprecated httpd_conf
  functions are dropped. Module http_uri is deprecated.

  Own Id: OTP-16252

- Refactored the internal handling of deprecated and removed functions.

  Own Id: OTP-16469

## Inets 7.1.3.3

### Fixed Bugs and Malfunctions

- Corrected an error regarding decode of percent encoded URLs introduced in
  inets-7.1.3.

  Own Id: OTP-16790 Aux Id: ERIERL-522

## Inets 7.1.3.2

### Fixed Bugs and Malfunctions

- Fix a crash in http server when setopts is called on a socket closed by the
  peer.

  Own Id: OTP-16775 Aux Id: ERIERL-519

## Inets 7.1.3.1

### Fixed Bugs and Malfunctions

- Avoid timing issue when setting active once on a socket that is being closed
  by the peer.

  Own Id: OTP-16735 Aux Id: OTP-16697, ERIERL-496

## Inets 7.1.3

### Fixed Bugs and Malfunctions

- Remove use of http_uri and mod_esi eval API.

  This is a backport from OTP 23 that improves the check of URIs to ensure that
  invalid URIs does not cause vulnerabilities. This will render the deprecated
  mod_esi eval API unusable as it used URI that does not conform to valid URI
  syntax.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-16556

## Inets 7.1.2

### Fixed Bugs and Malfunctions

- Inets will honor that valid HTTP headers can not have white space between
  field-name and the colon.

  Own Id: OTP-16169 Aux Id: ERL-1053

- Changed error propagation in httpc:request/1 to return expected error tuple
  instead of crashing.

  Own Id: OTP-16290 Aux Id: PR-2437, ERL-1083

- Fix handling of HEAD request with chunked transfer-encoding (httpc).

  Own Id: OTP-16300 Aux Id: ERL-1090

## Inets 7.1.1

### Improvements and New Features

- Add HTTP server error logging vi logger

  Own Id: OTP-16019

## Inets 7.1

### Improvements and New Features

- httpd - Accept single LF as line terminator

  Own Id: OTP-15893 Aux Id: PR-2206

- mod_esi will now always propagate the actual HTTP status code that it answered
  with, to later mod-modules, and not in some cases hardcode 200.

  Own Id: OTP-16049 Aux Id: ERIERL-395

## Inets 7.0.9

### Fixed Bugs and Malfunctions

- Fix a regression in http client that causes a crash when request URI has no
  scheme.

  Own Id: OTP-15930 Aux Id: ERL-969

## Inets 7.0.8

### Fixed Bugs and Malfunctions

- All incorrect (that is, all) uses of "can not" has been corrected to "cannot"
  in source code comments, documentation, examples, and so on.

  Own Id: OTP-14282 Aux Id: PR-1891

## Inets 7.0.7.2

### Improvements and New Features

- Add HTTP server error logging vi logger

  Own Id: OTP-16019

## Inets 7.0.7.1

### Improvements and New Features

- mod_esi will now always propagate the actual HTTP status code that it answered
  with, to later mod-modules, and not in some cases hardcode 200.

  Own Id: OTP-16049 Aux Id: ERIERL-395

## Inets 7.0.7

### Fixed Bugs and Malfunctions

- Fix the internal handling of the option erl_script_timeout in httpd. If
  explicit erl_script_timeout value was supplied in seconds it was not correctly
  converted to millisecond units for internal usage.

  This change fixes the handling of erl_script_timeout in all possible
  configuration scenarios.

  Own Id: OTP-15769 Aux Id: ERIERL-345

## Inets 7.0.6

### Fixed Bugs and Malfunctions

- Fix the internal handling of the option erl_script_timeout in httpd. When
  httpd was started with explicit erl_script_timeout, the value of the option
  was converted to milliseconds before storage. Subsequent calls to httpd:info/1
  returned the input value multiplied by 1000.

  This change fixes the handing of erl_script_timeout by storing the timeout in
  seconds and converting to milliseconds before usage.

  Own Id: OTP-15669 Aux Id: ERIERL-321

### Improvements and New Features

- Enhance documentation

  Own Id: OTP-15508 Aux Id: ERL-816

## Inets 7.0.5

### Fixed Bugs and Malfunctions

- Fixed bug that causes a crash in http client when using hostnames (e.g.
  localhost) with the the option ipv6_host_with_brackets set to true.

  This change also fixes a regression: httpc:request fails with connection error
  (nxdomain) if option ipv6_host_with_brackets set to true and host component of
  the URI is an IPv6 address.

  Own Id: OTP-15554 Aux Id: ERIERL-289

## Inets 7.0.4

### Fixed Bugs and Malfunctions

- Make sure ipv6 addresses with brackets in URIs are converted correctly before
  passing to lower level functions like gen_tcp and ssl functions. Could cause
  connection to fail.

  Own Id: OTP-15544 Aux Id: ERIERL-289

## Inets 7.0.3

### Fixed Bugs and Malfunctions

- Fixed http client to not send 'content-length' header in chunked encoded
  requests.

  Own Id: OTP-15338 Aux Id: ERL-733

- Fixed http client to not drop explicit 'Content-Type' header in requests
  without a body such as requests with the 'Content-Type' of
  application/x-www-form-urlencoded.

  Own Id: OTP-15339 Aux Id: ERL-736

## Inets 7.0.2

### Fixed Bugs and Malfunctions

- Enhance error handling, that is mod_get will return 403 if a path is a
  directory and not a file.

  Own Id: OTP-15192

- Do not use chunked-encoding with 1xx, 204 and 304 responses when using
  mod_esi. Old behavior was not compliant with HTTP/1.1 RFC and could cause
  clients to hang when they received 1xx, 204 or 304 responses that included an
  empty chunked-encoded body.

  Own Id: OTP-15241

- Add robust handling of chunked-encoded HTTP responses with an empty body (1xx,
  204, 304). Old behavior could cause the client to hang when connecting to a
  faulty server implementation.

  Own Id: OTP-15242

## Inets 7.0.1

### Fixed Bugs and Malfunctions

- Change status code for no mod found to handle request to 501

  Own Id: OTP-15215

## Inets 7.0

### Fixed Bugs and Malfunctions

- Fixed HTTP content injection bug in httpc (ERL-456).

  Own Id: OTP-14726

- Fixed support for URI-references in HTTP 'Location' header (ERL-333).

  Own Id: OTP-14729

- Fix broken 'Content-Type' handling in httpc (ERL-536).

  Own Id: OTP-15006

- Fix handling of relative paths in the script_alias property of httpd
  (ERL-574).

  Own Id: OTP-15021

- Fix httpd:reload_config/2 with path() as the first argument (ERL-578).

  Own Id: OTP-15025

- Improved gracefulness.

  Own Id: OTP-15042

### Improvements and New Features

- Split inets and create separate ftp and tftp apps.

  Own Id: OTP-14113

## Inets 6.5.2.4

### Fixed Bugs and Malfunctions

- Do not use chunked-encoding with 1xx, 204 and 304 responses when using
  mod_esi. Old behavior was not compliant with HTTP/1.1 RFC and could cause
  clients to hang when they received 1xx, 204 or 304 responses that included an
  empty chunked-encoded body.

  Own Id: OTP-15241

- Add robust handling of chunked-encoded HTTP responses with an empty body (1xx,
  204, 304). Old behavior could cause the client to hang when connecting to a
  faulty server implementation.

  Own Id: OTP-15242

## Inets 6.5.2.3

### Fixed Bugs and Malfunctions

- Change status code for no mod found to handle request to 501

  Own Id: OTP-15215

## Inets 6.5.2.2

### Fixed Bugs and Malfunctions

- Enhance error handling, that is mod_get will return 403 if a path is a
  directory and not a file.

  Own Id: OTP-15192

## Inets 6.5.2.1

### Improvements and New Features

- Options added for setting low-level properties on the underlying TCP
  connections. The options are: `sock_ctrl`, `sock_data_act` and
  `sock_data_pass`. See the manual for details.

  Own Id: OTP-15120 Aux Id: ERIERL-192

## Inets 6.5.2

### Fixed Bugs and Malfunctions

- inets: httpd - Gracefully handle bad headers

  The option max_headers operated on the individual header length instead of the
  total length of all headers. Also headers with empty keys are now discarded.

  Own Id: OTP-15092

## Inets 6.5.1

### Fixed Bugs and Malfunctions

- Fix broken options handling in httpc (ERL-441).

  Own Id: OTP-15007

## Inets 6.5

### Fixed Bugs and Malfunctions

- httpc_manager crashes when a long running request is sent on a persistent HTTP
  connection (keep-alive). Fixed httpc_manager to use proper timeouts on
  keep-alive connections.

  Own Id: OTP-14908

### Improvements and New Features

- Add support for unix domain sockets in the http client.

  Own Id: OTP-14854

## Inets 6.4.5

### Fixed Bugs and Malfunctions

- CGI environment variable CONTENT_LENGTH shall be a string

  Own Id: OTP-14679

- In relaxed mode disregard Content-Length header if there is also a
  Transfer-Encoding header.

  Own Id: OTP-14727

- Eliminated race condition, that could cause http request to sporadically fail
  to complete successfully, when keep-alive connections are used.

  Own Id: OTP-14783

## Inets 6.4.4

### Fixed Bugs and Malfunctions

- Correct the handling of location headers so that the status code is not hard
  coded. This should have been fixed by commit
  2cc5ba70cbbc6b3ace81a2a0324417c3b65265bb but unfortunately was broken during a
  code refactoring and unnoticed due to a faulty placed test case.

  Own Id: OTP-14761

## Inets 6.4.3

### Improvements and New Features

- Fix broken handling of POST requests

  New chunk mechanism of body data in POST requests added in
  5d01c70ca399edf28e99dc760506329689fab6ba broke handling of POST body data not
  using the new mechanism.

  Own Id: OTP-14656

- Make sure ints:stop/2 of the service httpd is synchronous

  Own Id: OTP-14696

- Honor status code returned by ESI script and modernize "location" header
  handling.

  Own Id: OTP-14716

## Inets 6.4.2

### Fixed Bugs and Malfunctions

- Make sure mod_log uses the correct status code

  Own Id: OTP-14510

- Correct behaviour of mod_disk_log to proparly handle repair options

  Own Id: OTP-14530

## Inets 6.4.1

### Fixed Bugs and Malfunctions

- http_uri aligned to follow RFC 3986 and not convert "+" to space when decoding
  URIs.

  Own Id: OTP-14573

### Improvements and New Features

- Added new option max_client_body_chunk to httpd server to allow chunked
  delivery of PUT and POST data to mod_esi callback. Note, new mod_esi callback
  implementation is required.

  Also correct value provided by server_name environment variable

  Own Id: OTP-14450

## Inets 6.4

### Fixed Bugs and Malfunctions

- httpd_util:rfc1123_date/1 gracefully handle invalid DST dates by returning the
  original time in the expected rfc1123 format.

  Own Id: OTP-14394

### Improvements and New Features

- Add unicode binary support to http_uri functions

  Own Id: OTP-14404

- httpc - Change timeout handling so the redirects cause a new timer to be set.
  This means that a simple redirected request could return after 2\*timeout
  milliseconds.

  Own Id: OTP-14429

## Inets 6.3.9

### Fixed Bugs and Malfunctions

- The close of a chunked file reception crashed in a certain timing sequence.

  Own Id: OTP-14391 Aux Id: seq13306

## Inets 6.3.8

### Improvements and New Features

- Added missing release note for inets-6.3.7

  Own Id: OTP-14383

## Inets 6.3.7

### Fixed Bugs and Malfunctions

- Fixed a bug in ftp that made further operations after a recv_chunk operation
  impossible.

  Own Id: OTP-14242

- Make default port, 80 and 443, implicit in automatic redirection.

  Own Id: OTP-14301

## Inets 6.3.6

### Fixed Bugs and Malfunctions

- Chunk size decoding could fail. The symptom was that chunk decoding sometimes
  failed depending on timing of the received stream. If chunk size was split
  into two different packets decoding would fail.

  Own Id: OTP-13571 Aux Id: ERL-116

- Prevent httpc user process to hang if httpc_handler process terminates
  unexpectedly

  Own Id: OTP-14091

- Correct Host header, to include port number, when redirecting requests.

  Own Id: OTP-14097

- Shutdown gracefully on connection or TLS handshake errors

  Own Id: OTP-14173 Aux Id: seq13262

## Inets 6.3.5

### Fixed Bugs and Malfunctions

- Correct mistakes in ftp client introduced in inets-6.3.4

  Own Id: OTP-14203 Aux Id: OTP-13982

## Inets 6.3.4

### Fixed Bugs and Malfunctions

- Fixes a bug that makes the ftp client end up in bad state if there is a multi
  line response from the server and the response number is in the message being
  sent.

  Own Id: OTP-13960 Aux Id: PR1196

- The ftp client could stop consuming messages when the multiline response
  handling was corrected.

  Own Id: OTP-13967

- Fix keep-alive https through proxy connections so that all requests, following
  the first one, will run as expected instead of failing.

  Own Id: OTP-14041

- Fix bug from commit fdfda2fab0921d409789174556582db28141448e that could make
  listing of group members in mod_auth callbacks fail.

  Own Id: OTP-14082

### Improvements and New Features

- Update behavior of httpc:request to match RFC-7231

  Own Id: OTP-13902

- Fixed dialyzer warnings as well as some white-space issues. Thanks to Kostis.

  Own Id: OTP-13982 Aux Id: PR-1207

## Inets 6.3.3

### Fixed Bugs and Malfunctions

- The legacy option 'inet6fb4' for inets had stopped working. This bug has now
  been corrected. Fix by Edwin Fine in bugs.erlang.org ERL-200 and Github
  PR#1132.

  Own Id: OTP-13776 Aux Id: ERL-200 PR-1132

## Inets 6.3.2

### Improvements and New Features

- PUT and DELETE support has been added to mod_esi

  Own Id: OTP-13688 Aux Id: seq13149

## Inets 6.3.1

### Fixed Bugs and Malfunctions

- A debug message was accidentally left enabled in the ftp client.

  Own Id: OTP-13712 Aux Id: seq13143

## Inets 6.3

### Fixed Bugs and Malfunctions

- Ftp client fixes: 1) Corrected a bug that the ftp client gen_server crashed if
  the listening data socket was closed.

  2. Corrections of ftp client error codes so they are as defined in the
     reference manual

  Own Id: OTP-13644

### Improvements and New Features

- Remove usage of erlang:now().

  Own Id: OTP-12441

- Add handling of DELETE Body to http client.

  Own Id: OTP-13383 Aux Id: PR-972

- Removed references to mod_include and webtool from examples and tests.

  Own Id: OTP-13445 Aux Id: PR-988

- Remove module inets_regexp. Module re should be used instead.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13561

## Inets 6.2.4

### Improvements and New Features

- Handle multiple \\t in mime types file

  Own Id: OTP-13663 Aux Id: seq13132

## Inets 6.2.3

### Improvements and New Features

- Put back unused module inets_regexp and remove it in OTP 19 instead as it is
  an incompatibility, although it is an undocumented module and should not
  affect other applications.

  Own Id: OTP-13533

## Inets 6.2.2

### Improvements and New Features

- Add environment information item peer_cert to mod_esi

  Own Id: OTP-13510

## Inets 6.2.1

### Fixed Bugs and Malfunctions

- Mend ipv6_host_with_brackets option in httpc

  Own Id: OTP-13417

## Inets 6.2

### Fixed Bugs and Malfunctions

- The TFTP client/server has been fixed to allow file sizes larger than 32MB
  block by allowing the 16 bit block counter to wrap. Since this is a commonly
  accepted behavior we regard it as a bug fix.

  Own Id: OTP-13403

### Improvements and New Features

- Handle HTTP PATCH method in client.

  Own Id: OTP-13286

- Expected termination should not be logged as an application error.

  Own Id: OTP-13389

## Inets 6.1.1.1

### Fixed Bugs and Malfunctions

- Mend ipv6_host_with_brackets option in httpc

  Own Id: OTP-13417

## Inets 6.1.1

### Fixed Bugs and Malfunctions

- mod_alias now traverses all aliases picking the longest match and not the
  first match.

  Own Id: OTP-13248

## Inets 6.1

### Fixed Bugs and Malfunctions

- Replace obs-folds with spaces instead of failing

  Own Id: OTP-13069

- Add validation fun for URI scheme to http_uri API

  Own Id: OTP-13071

- Handle stream bodies as documented.

  Own Id: OTP-13093

- Correct error handling of mod_esi generated chunks. Send warning headers in
  chunk trailers instead of generating an unexpected additional 500 request
  response, when problems, such as a timeout occurs.

  Own Id: OTP-13110

- HTTP client terminates gracefully when an invalid chunked length header is
  encountered.

  Own Id: OTP-13117

### Improvements and New Features

- Add default for SNI (Server Name Indication) when running https using the
  inets HTTP-client.

  Own Id: OTP-12985

- Be forgiving to chunked sizes that have trailing whitespaces as prior
  implementation was. Also some legacy embedded devices does actually have
  trailing whitespaces even though this in not according to the spec.

  Own Id: OTP-13116

## Inets 6.0.3

### Fixed Bugs and Malfunctions

- Improved error handling and gracfully termination when an invalid chunked
  length header is encountered.

  Own Id: OTP-13061

### Improvements and New Features

- Add possibility to set socket options, such as nodelay, for httpd. Also phase
  out legacy option value inet6bf4 for the ipfamily option. This value will be
  translated to the value inet.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-13062

## Inets 6.0.2

### Fixed Bugs and Malfunctions

- Avoid crash in mod_auth_server and mod_security_server due to using an atom
  instead of a string when creating a name.

  Own Id: OTP-13022

### Improvements and New Features

- Add function response_default_headers/0 to httpd customize API, to allow user
  to specify default values for HTTP response headers.

  Own Id: OTP-13013

## Inets 6.0.1

### Fixed Bugs and Malfunctions

- Fix broken socket feature, that is on Linux systems a socket may be opened
  before starting Erlang and then passed to Erlang's httpd daemon. This is
  useful as the wrap program can open a privileged port and Erlang does not have
  to be run as root.

  Own Id: OTP-12875 Aux Id: seq12878

- Fix broken socket feature, that is on Linux systems a socket may be opened
  before starting Erlang and then passed to Erlangs tftp daemon. This is useful
  as the wrap program can open a privileged port and Erlang does not have to be
  run as root.

  Own Id: OTP-12898 Aux Id: seq12900

- httpc_handler should react properly to cancel requests even when the request
  to be canceled was already finished but httpc_manager did not get notified
  about that yet.

  Own Id: OTP-12922

### Improvements and New Features

- Added format_status function to httpd process to avoid sensitive information
  to be printed in supervisor logs.

  Own Id: OTP-12976

- Return meaningful error reason disregarding whether a http proxy is used or
  not.

  Own Id: OTP-12984

## Inets 6.0

### Fixed Bugs and Malfunctions

- Fix race condition in httpc. If the socket is closed by the peer do not try to
  close it again.

  Own Id: OTP-11845

- Avoid process leak by gracefully terminating httpc request handler process
  when send operation fails.

  Own Id: OTP-12362

- Reject messages with a Content-Length less than 0

  Own Id: OTP-12739 Aux Id: seq12860

- Let gen_tcp:controlling_process/2 and inet_sctp:connect/\[45] propagate
  prim_inet:setopt/3 errors instead of having them generate badmatch exceptions.

  Own Id: OTP-12798

### Improvements and New Features

- Remove Server Side Include support from inets, as this is an old technique
  that has security issues and was not well tested.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-12156

- New value in `server_tokens` config for limiting banner grabbing attempts.

  By setting `{server_tokens, none}` in `ServiceConfig` for
  `inets:start(httpd, ServiceConfig)`, the "Server:" header will not be set in
  messages from the server.

  Own Id: OTP-12661 Aux Id: seq12840

- To enable the HTTP server to run in a virtualized environment, where there can
  be more that one server that has the same ip-address and port, we add a new
  option profile.

  Own Id: OTP-12674

- httpc: Fix implementation of graceful shudown to work as intended for keep
  alive connections not using pipelining.

  Own Id: OTP-12803

- Correct handling of proxy options when using persistent connections.

  Own Id: OTP-12822

## Inets 5.10.9

### Improvements and New Features

- Add behaviour with optional callbacks to customize the inets HTTP server.

  Own Id: OTP-12776

## Inets 5.10.8

### Fixed Bugs and Malfunctions

- Reject messages with a Content-Length less than 0

  Own Id: OTP-12739 Aux Id: seq12860

## Inets 5.10.7

### Improvements and New Features

- New value in `server_tokens` config for limiting banner grabbing attempts.

  By setting `{server_tokens, none}` in `ServiceConfig` for
  `inets:start(httpd, ServiceConfig)`, the "Server:" header will not be set in
  messages from the server.

  Own Id: OTP-12661 Aux Id: seq12840

## Inets 5.10.6

### Fixed Bugs and Malfunctions

- inets: parse correctly 'Set-Cookie' header with empty value

  httpc_cookie should parse cookies with empty values and no attributes set in
  the 'Set-Cookie' headers.

  Own Id: OTP-12455

### Improvements and New Features

- Add parsing of URI fragments to http_uri:parse

  This fixes a bug in httpc where redirection URIs could lead to bad requests if
  they contained fragments.

  Own Id: OTP-12398

- httpc: http client now ignores invalid set-cookie headers

  Own Id: OTP-12430

## Inets 5.10.5

### Fixed Bugs and Malfunctions

- mod_alias now handles https-URIs properly

  Consistent view of configuration parameter keep_alive_timeout, should be
  presented in the httpd:info/\[1,2] function in the same unit as it is
  inputted.

  Own Id: OTP-12436 Aux Id: seq12786

### Improvements and New Features

- Gracefully handle invalid content-length headers instead of crashing in
  list_to_integer.

  Own Id: OTP-12429

## Inets 5.10.4

### Fixed Bugs and Malfunctions

- Fixed a spelling mistake in httpc documentation.

  Own Id: OTP-12221

### Improvements and New Features

- Add option \{ftp_extension, boolean\} to enable use of extended commands EPSV
  and EPRT, as specified in RFC 2428, for IPv4 instead of using the legacy
  commands. Ipv6 cannot be supported without the extended commands.

  Own Id: OTP-12255

## Inets 5.10.3

### Fixed Bugs and Malfunctions

- Fix some spelling mistakes in documentation

  Own Id: OTP-12152

### Improvements and New Features

- httpd: Separate timeout for TLS/SSL handshake from keepalive timeout

  Own Id: OTP-12013

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

## Inets 5.10.2

### Fixed Bugs and Malfunctions

- httpc: Fix streaming bugs when handling small responses

  Own Id: OTP-11992

## Inets 5.10.1

### Fixed Bugs and Malfunctions

- Correct distirbing mode for httpd:reload_config/2

  Own Id: OTP-11914

### Improvements and New Features

- Improved handling of invalid strings in the HTTP request line.

  Impact: May improve memory consumption

  Own Id: OTP-11925 Aux Id: Sequence 12601

## Inets 5.10

### Fixed Bugs and Malfunctions

- Fixed a spelling mistake in httpc doc (Thanks to Wasif Riaz Malik)

  Own Id: OTP-11538

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

- ftp now sanitize file name, user name and passwords from <CR> and <LF> tags
  (Thanks to Sergei Golovan)

  Own Id: OTP-11750

- Corrected error handling in the HTTP client, making it behave more graceful.

  Thanks to Kirilll Zaborsky

  Own Id: OTP-11794

- Support identity transfer-encoding in httpc.

  Thanks to Anthony Ramine

  Own Id: OTP-11802

- Ignore empty Set-Cookie headers to increase interoperability with servers that
  violate the RFC.

  Thanks to Kirilll Zaborsky

  Own Id: OTP-11803

### Improvements and New Features

- The commit 6189bc07 "inets: httpc improve pipelining" has been reverted, as it
  turned out to break things rather than improve pipelining utilization. It is
  instead up to the user to configure httpc and use it wisely to be able to get
  the most out of pipelining.

  Own Id: OTP-11756

- Handle all response codes in httpd_util:message/3

  Own Id: OTP-11838

## Inets 5.9.8

### Improvements and New Features

- Mend max_clients check that was broken and avoid too extensive logging that
  could cause memory problems.

  Own Id: OTP-11557 Aux Id: seq12478

## Inets 5.9.7

### Fixed Bugs and Malfunctions

- Fix httpd config option 'script_timeout' and fixed httpd config option
  'keep_alive_timeout'. Thanks to Johannes Weissl.

  Own Id: OTP-11276

- Make httpc:request_cancel/\[1,2] asynchronous. Previously these functions
  tried to guarantee request answer would not reach the client, which only
  worked for some of the use cases. Now these functions are totally asynchronous
  which makes it the clients responsibility to disregard possible answers to
  canceled requests.

  Also pipelining implementation has been changed to improve the utilization
  factor. Further investigation of possible enhancements in this area are
  planned for later.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-11312

- \[httpd] Add handling of new response for mod_head (otherwise causing
  case_clause crash). Also updated logging: Removed logging for keep-alive
  connections timeout (this is a normal occurrence and not an error) and some
  access-log body size corrections.

  Own Id: OTP-11328

### Improvements and New Features

- The ftp client now supports ftp over tls (ftps).

  Own Id: OTP-11037

## Inets 5.9.6

### Improvements and New Features

- httpc: Allow content body in DELETE requests. Thanks to James Wheare.

  Own Id: OTP-11190

- Add missing brackets to report formatting on ftp_progress process exit. Thanks
  to Artur Wilniewczyc.

  Own Id: OTP-11202

- Fix some errors in the inets documentation. Thanks to Johannes Weissl.

  Own Id: OTP-11210

- Fix various typos in httpd, inets. Thanks to Tomohiko Aono.

  Own Id: OTP-11226

- Fix httpd config option 'erl_script_nocache'. Thanks to Johannes Weissl.

  Own Id: OTP-11260

## Inets 5.9.5

### Fixed Bugs and Malfunctions

- Reverted incorrect commit that broke cookie handling when using
  httpc-profiles.

  Own Id: OTP-10956

### Improvements and New Features

- Fix http_request:http_headers/1 to send content-length when length is zero.
  Thanks to CA Meijer.

  Own Id: OTP-10934

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

- Fix \{stream, \{self, once\}\} in httpc to work as expected. Thanks to
  Masatake Daimon

  Own Id: OTP-11122

## Inets 5.9.4

### Improvements and New Features

- httpd: The modules option now defaults to the documented value.

  Own Id: OTP-10844

- httpc: Fixed persistent connection implementation that was broken by a patch
  to R13. The patch made persistent connections behaved the same way as
  pipelining.

  Own Id: OTP-10845

- httpd: Simplified configuration of ssl in httpd, this also enables all ssl
  options to be configured. The old and limited way is no longer documented but
  will be supported for backwards comatibility for some time.

  Own Id: OTP-10846

- Handle correctly the "No files found or file unavailable" error code. Thanks
  to Serge Aleynikov

  Own Id: OTP-10886

## Inets 5.9.3

### Improvements and New Features

- httpc: The HTTP client now supports HTTPS through proxies

  Own Id: OTP-10256 Aux Id: kunagi-2 \[ce2e800e-c99f-4050-a1c4-f47023d9c7aa-1]

- Some examples overflowing the width of PDF pages have been corrected.

  Own Id: OTP-10665

- Fix autoredirect for POST requests responding 303. Thanks to Hans Svensson.

  Own Id: OTP-10765

## Inets 5.9.2.2

### Improvements and New Features

- Make log_alert configurable as option in ssl, SSLLogLevel added as option to
  inets conf file

  Own Id: OTP-11259

## Inets 5.9.2.1

### Improvements and New Features

- Fixed obsolete error report in inets.

  Own Id: OTP-11185 Aux Id: seq12357

## Inets 5.9.2

### Improvements and New Features

- Minimum bytes per second

  New option to http server, \{minimum_bytes_per_second, integer()\}, for a
  connection, if it is not reached the socket will close for that specific
  connection. Can be used to prevent hanging requests from faulty clients.

  Own Id: OTP-10392

## Inets 5.9.1

### Improvements and New Features

- Better handling of errorI(s) during update of the session database.

  Also added and updated some debugging functions
  [which_sessions/0,1](`httpc:which_sessions/0`) and
  [info/0](`httpc:info/0`).

  Own Id: OTP-10093

  Aux Id: Seq 12062

- Removed R14B compatible version of (inets-service and tftp) behaviour
  definition.

  Own Id: OTP-10095

- \[httpc] Documentation of KeepAlive and Pipeline timeout options have been
  improved.

  Own Id: OTP-10114

### Fixed Bugs and Malfunctions

- \[httpc] Cancel request does not work due to incorrect handler table creation
  (wrong keypos).

  Vyacheslav Vorobyov

  Own Id: OTP-10092

### Incompatibilities

-

## Inets 5.9

### Improvements and New Features

- \[httpd] Make the server header configurable with new config option
  [server_tokens](`m:httpd#prop_server_tokens`). The value of the server header,
  which was previously hard-coded (at compile time), is now possible to
  manipulate through the means of the
  [server_tokens](`m:httpd#prop_server_tokens`) config option.

  Own Id: OTP-9805

- Improve inets support for inets as an included application.

  `inets_app` calls `supervisor:start_link/3` directly rather than calling the
  root supervisor function `inets_sup:start_link/0`. This precludes using
  included_applications to start inets without having a wrapper function.

  Jay Nelson

  Own Id: OTP-9960

- \[httpc] Add function for retrieving current options,
  [get_options/1,2](`httpc:get_options/1`).

  Own Id: OTP-9979

- Utility module `m:http_uri` now officially supported.

  Also, the `http_uri:parse` function has been extended with more scheme support
  and a way to provide your own scheme info.

  Own Id: OTP-9983

  Aux Id: Seq 12022

### Fixed Bugs and Malfunctions

-

## Inets 5.8.1

### Improvements and New Features

-

### Fixed Bugs and Malfunctions

- \[ftp] Fails to open IPv6 connection due to badly formatted IPv6 address in
  EPRT command. The address part of the command incorrectly contained decimal
  elements instead of hexadecimal.

  Own Id: OTP-9827

  Aux Id: Seq 11970

- \[httpc] Bad Keep Alive Mode. When selecting a session, the "state" of the
  session (specifically if the server has responded) was not taken into account.

  Own Id: OTP-9847

- \[httpc] The client incorrectly streams 404 responses. The documentation
  specifies that only 200 and 206 responses shall be streamed.

  Shane Evens

  Own Id: OTP-9860

## Inets 5.8

### Improvements and New Features

- \[ftpc] Add a config option to specify a
  [data connect timeout](`m:ftp#dtimeout`). That is how long the ftp client will
  wait for the server to connect to the data socket. If this timeout occurs, an
  error will be returned to the caller and the ftp client process will be
  terminated.

  Own Id: OTP-9545

- \[httpc] Wrong Host header in IPv6 HTTP requests. When a URI with a IPv6 host
  is parsed, the brackets that encapsulates the address part is removed. This
  value is then supplied as the host header. This can cause problems with some
  servers. A workaround for this is to use headers_as_is and provide the host
  header with the request call. To solve this a new option has been added,
  [ipv6_host_with_brackets](`m:httpc#ipv6_host_with_brackets`). This option
  specifies if the host value of the host header shall include the brackets or
  not. By default, it does not (as before).

  Own Id: OTP-9628

### Fixed Bugs and Malfunctions

- \[httpd] Fix logging of content length in mod_log.

  Garrett Smith

  Own Id: OTP-9715

- \[httpd] Sometimes entries in the transfer log was written with the message
  size as list of numbers. This list was actually the size as a string, e.g.
  "123", written with the control sequence ~w. This has now been corrected so
  that any string is converted to an integer (if possible).

  Own Id: OTP-9733

- Fixed various problems detected by Dialyzer.

  Own Id: OTP-9736

### Incompatibilities

- \[httpc] Deprecated interface module `http` has been removed. It has (long)
  been replaced by http client interface module `m:httpc`.

  Own Id: OTP-9359

- \[httpc|httpd] The old ssl implementation (based on OpenSSL), has been
  deprecated. The config option that specified usage of this version of the ssl
  app, `ossl`, has been removed.

  Own Id: OTP-9522

## Inets 5.7.2

### Improvements and New Features

-

### Fixed Bugs and Malfunctions

- \[httpd] XSS prevention did not work for hex-encoded URL's.

  Own Id: OTP-9655

- \[httpd] GET request with malformed header date caused server crash
  (non-fatal) with no reply to client. Will now result in a reply with status
  code 400.

  Own Id: OTP-9674

  Aux Id: seq11936

## Inets 5.7.1

### Improvements and New Features

-

### Fixed Bugs and Malfunctions

- \[httpc] Parsing of a cookie expire date should be more forgiving. That is, if
  the parsing fails, the date should be ignored. Also added support for (yet
  another) date format: "Tue Jan 01 08:00:01 2036 GMT".

  Own Id: OTP-9433

- \[httpc] Rewrote cookie parsing. Among other things solving cookie processing
  from www.expedia.com.

  Own Id: OTP-9434

- \[httpd] Fix httpd directory traversal on Windows. Directory traversal was
  possible on Windows where backward slash is used as directory separator.

  András Veres-Szentkirályi.

  Own Id: OTP-9561

## Inets 5.7

### Improvements and New Features

- \[httpc|httpd] Added support for IPv6 with ssl.

  Own Id: OTP-5566

### Fixed Bugs and Malfunctions

- \[httpc] Remove unnecessary usage of iolist_to_binary when processing body
  (for PUT and POST).

  Filipe David Manana

  Own Id: OTP-9317

- \[ftp] FTP client doesn't work with IPv6 host.

  Attila Rajmund Nohl

  Own Id: OTP-9342 Aux Id: seq11853

- \[httpd] Peer/sockname resolv doesn't work with IPv6 addrs in HTTP.

  Attila Rajmund Nohl.

  Own Id: OTP-9343

- \[httpc] Clients started stand-alone not properly handled. Also it was not
  documented how to use them, that is that once started, they are represented by
  a `t:pid/0` and not by their `profile()`.

  Own Id: OTP-9365

## Inets 5.6

### Improvements and New Features

- \[httpc] Add support for upload body streaming (PUT and POST).

  For more info, see the definition of the `Body` argument of the
  [request/4,5](`httpc:request/4`) function.

  Filipe David Manana

  Own Id: OTP-9094

- \[ftp] Added (type) spec for all exported functions.

  Own Id: OTP-9114 Aux Id: seq11799

- \[httpd] `mod_esi:deliver/2` made to accept binary data.

  Bernard Duggan

  Own Id: OTP-9123

- \[httpd] Prevent XSS in error pages. Prevent user controlled input from being
  interpreted as HTML in error pages by encoding the reserved HTML characters.

  Michael Santos

  Own Id: OTP-9124

- \[httpd] Improved error messages.

  Ricardo Catalinas Jiménez

  Own Id: OTP-9157

- \[httpd] Extended support for file descriptors. In order to be able to bind to
  a privileged port without running the erlang VM as root, the support for using
  file descriptors has been improved. It is now possible to add the file
  descriptor to the config (option fd) when calling the
  [inets:start(httpd, ...)](`inets:start/2`) function.

  Attila Rajmund Nohl

  Own Id: OTP-9202

  Aux Id: seq11819

- The default ssl kind has now been changed to `essl`.

  `ossl` will work for as long as the ssl application supports it.

  See the httpd [socket_type](`m:httpd#props_comm`) communication property or
  the httpc [request/4,5](`httpc:request/4`) function for more info.

  Own Id: OTP-9230

  \*** POTENTIAL INCOMPATIBILITY \***

### Fixed Bugs and Malfunctions

- \[httpd] Wrong [security property](`m:httpd#props_sec`) names used in
  documentation.

  `security_data_file` used instead of `data_file`.

  `security_max_retries` used instead of `max_retries`.

  `security_block_time` used instead of `block_time`.

  `security_fail_expire_time` used instead of `fail_expire_time`.

  `security_auth_timeout` used instead of `auth_timeout`.

  Garrett Smith

  Own Id: OTP-9131

- \[httpd] Fix timeout message generated by mod_esi. When a mod_esi request
  times out, the code to send a timeout response was incorrect and generated an
  internal server error as well as an invalid response line.

  Bernard Duggan

  Own Id: OTP-9158

- \[httpc] httpc manager crashes. When a request results in a retry, the request
  id will be "reused" in the previous implementation a race condition could
  occur causing the manager to crash.

  This is now avoided by using proc_lib:init_ack and gen_server:enter_loop to
  allow more advanced initialization of httpc_handlers without blocking the
  httpc_manger and eliminating extra processes that can cause race conditions.

  Own Id: OTP-9246

- \[httpc] Issuing a request (`httpc:request`) to an host with the ssl option
  `{ip, {127,0,0,1}}` results in an handler crash. The reason was that the
  connect call resulted in an exit with reason `badarg` (this was the same for
  both `ssl` and `gen_tcp`).

  Exits was not caught. This has now been improved.

  Own Id: OTP-9289

  Aux Id: seq11845

## Inets 5.5.2

### Improvements and New Features

-

### Fixed Bugs and Malfunctions

- \[httpd] httpd_response:send_chunk handles empty list and empty binary - i.e.
  no chunk is sent, but it does not handle a list with an empty binary \[<<>>].
  This will be sent as an empty chunk - which in turn will be encoded by
  http_chunk to the same as a final chunk, which will make the http client
  believe that the end of the page is reached.

  Own Id: OTP-8906

## Inets 5.5.1

### Improvements and New Features

- Miscellaneous inet6 related problems.

  Own Id: OTP-8927

- Updated http-server to make sure URLs in error-messages are URL-encoded. Added
  support in http-client to use URL-encoding. Also added the missing include
  directory for the inets application.

  Own Id: OTP-8940

  Aux Id: seq11735

### Fixed Bugs and Malfunctions

- Fix format_man_pages so it handles all man sections and remove warnings/errors
  in various man pages.

  Own Id: OTP-8600

- \[httpc] Pipelined and queued requests not processed when connection closed
  remotelly.

  Own Id: OTP-8906

## Inets 5.5

### Fixed Bugs and Malfunctions

- \[httpc] If a request times out (not connect timeout), the handler process
  exited (normal) but neglected to inform the manager process. For this reason,
  the manager did not clean up the request table., resulting in a memory leak.
  Also the manager did not create a monitor for the handler, so in an unforeseen
  handler crash, this could also create a memory leak.

  Own Id: OTP-8739

- The service tftp was spelled wrong in documentation and in some parts of the
  code. It should be tftp.

  Own Id: OTP-8741 Aux Id: seq11635

- \[httpc] Replaced the old http client api module (http) with the new, httpc in
  the users guide.

  Own Id: OTP-8742

### Improvements and New Features

- Eliminated warnings for auto-imported BIF clashes.

  Own Id: OTP-8840

## Inets 5.4

### Improvements and New Features

- \[httpc|httpd] - Now allow the use of the "new" ssl, by using the `essl` tag
  instead.

  See the `http_option` option in the [request/4,5](`httpc:request/4`) or the
  [socket-type](`m:httpd#props_comm`) section of the Communication properties
  chapter for more info,

  Own Id: OTP-7907

- Deprecated functions designated to be removed in R14 has been removed. Also,
  some new functions has been marked as deprecated (the old http client api
  module).

  Own Id: OTP-8564

  \*** POTENTIAL INCOMPATIBILITY \***

- \[httpd] - Improved mod_alias. Now able to do better URL rewrites.

  See [URL aliasing properties](`m:httpd#props_alias`) and the
  [CGI properties](`m:httpd#props_cgi`) section(s) for more info,

  Own Id: OTP-8573

### Fixed Bugs and Malfunctions

-

## Inets 5.3.3

### Improvements and New Features

-

### Fixed Bugs and Malfunctions

- \[httpc] - Made cookie handling more case insensitive.

  Own Id: OTP-8609

  Nicolas Thauvin

- \[httpc|httpd] - Netscape cookie dates can also be given with a 2-digit year
  (e.g. 06 = 2006).

  Own Id: OTP-8610

  Nicolas Thauvin

- \[httpd] - Added support (again) for the documented debugging features. See
  the User's Guide [Configuration](http_server.md#config) chapter for more info.

  Own Id: OTP-8624

## Inets 5.3.2

### Improvements and New Features

-

### Fixed Bugs and Malfunctions

- \[httpc] - Memory leak plugged. The profile manager never cleaned up in its
  handler database. This meant that with each new request handler, another entry
  was created that was never deleted. Eventually the request id counter (used as
  a key) would wrap, but the machine would most likely run out of memory before
  that happened.

  Own Id: OTP-8542

  Lev Walkin

- \[httpc] - https requests with default port (443) not handled properly.

  Own Id: OTP-8607

  jebu ittiachen

## Inets 5.3.1

### Improvements and New Features

-

### Fixed Bugs and Malfunctions

- \[httpc] - Badly formatted error reason for errors occurring during initial
  connect to a server. Also, the possible error reasons was not properly
  documented.

  Own Id: OTP-8508

  Aux Id: seq11407

- \[httpd] - Issues with ESI erl_script_timeout.

  - The `erl_script_timeout` config option is ducumented as a number of seconds.
    But when parsing the config, in the new format (not a config file), it was
    handled as if in number of milliseconds.
  - When the erl-script-timeout time was exceeded, the server incorrectly marked
    the answer as sent, thereby leaving client hanging (with an incomplete
    answer). This has been changed, so that now the socket will be closed.

  Own Id: OTP-8509

## Inets 5.3

### Improvements and New Features

- \[httpc] - Allow users to pass socket options to the transport module when
  making requests.

  See the `socket_opts` option in the [request/4](`httpc:request/4`) or
  [set_options/1,2](`httpc:set_options/1`) for more info,

  Own Id: OTP-8352

- \[httpc] Fix bug crafting Host header when port is not 80.

  The host header should include the port number as well as the host name when
  making a request to a server listening on a port other than the HTTP default
  of 80. Currently, only the host name is included. This is important to make
  the http client more compliant with the HTTP specification.

  Own Id: OTP-8371

  Kelly McLaughlin

- \[httpc|httpd] http_chunk data handling/passing improvement.

  This is a modification to the http_chunk module to forward any full chunk
  received, regardless of whether the size field for the following chunk has
  been received yet. This allows http_chunk to be used in situations where a
  long term HTTP connection is used to send periodic status updates as
  individual chunks. Previously a given chunk would not be forwarded to the
  client process until the size for the next chunk had been read which rendered
  the module difficult to use for the scenario described.

  Bernard Duggan

  Own Id: OTP-8351

- Include the inets test suite in the release of the application.

  Own Id: OTP-8349

- \[httpc] - It is now possible to configure the client to deliver an async
  reply to more receivers then the calling process.

  See the [receiver](`httpc:request/2`) option for more info,

  Own Id: OTP-8106

- \[httpd] - Methods "PUT" and "DELETE" now allowed.

  huntermorris@gmail.com

  Own Id: OTP-8103

- \[httpc] Several more or less critical fixes:

  - Initial call between the httpc manager and request handler was synchronous.

    When the manager starts a new request handler, this is no longer a
    synchronous operation. Previously, the new request handler made the
    connection to the server and issuing of the first request (the reason for
    starting it) in the gen_server init function. If the connection for some
    reason "took some time", the manager hanged, leaving all other activities by
    that manager also hanging.

  As a side-effect of these changes, some modules was also renamed, and a new
  api module, `m:httpc`, has been introduced (the old module `http` is _not_
  removed, but is now just wrapper for `httpc`).

  Own Id: OTP-8016

  \*** POTENTIAL INCOMPATIBILITY \***

### Fixed Bugs and Malfunctions

- \[httpd] The server did not fully support the documented module callback api.
  Specifically, the load function should be able to return the atom `ok`, but
  this was not accepted.

  Own Id: OTP-8359

- Fixing various documentation-related bugs (bad quotes).

  Own Id: OTP-8327

- Fixing minor Dialyzer and copyright problem(s).

  Own Id: OTP-8315

- \[httpc] - Added basic sanity check of option value combinations.

  adam.kocoloski@gmail.com

  Own Id: OTP-8056

## Inets 5.2

### Improvements and New Features

- \[ftpc] - Start of the FTP client has been changed in the following way:

  - It is now also possible to start a standalone FTP client process using the
    re-introduced `ftp:open/2` function.

    This is an alternative to starting the client using the
    inets service framework.

    The old `ftp:open/1`, undocumented, function, caused the client to be hooken
    into the inets service supervision framework. This is _no_ longer the case.

    \*** POTENTIAL INCOMPATIBILITY \***

  - Previously, the FTP client attempted to use IPv6, unless otherwise
    instructed (the `ip_v6_disabled` flag), and only used IPv4 if this did not
    work. This has now been _changed_.

    A new option, [ipfamily](`m:ftp#ipfamily`), has been introduced, with the
    default value `inet` (IPv4).

    See `ftp:open/2` for more info.

    \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8258

- The documentation is now built with open source tools (_xsltproc_ and _fop_)
  that exists on most platforms. One visible change is that the frames are
  removed.

  Own Id: OTP-8249

### Fixed Bugs and Malfunctions

- \[httpc] - Streaming to file did not work.

  dizzyd@gmail.com

  Own Id: OTP-8204

- \[ftpc] - The `ftp:ls/2` function (LIST command) and the
  `ftp:nlist/2` function (NLST command) with wildcards did not work
  properly.

  These functions is documented as working on directories, but this is actually
  not according the standard. The LIST and NLST commands are specified to
  operate on a directory or other group of files, or a file.

  Previously, an attempt was made to check if the listing returned by the server
  was actually an error message. This was done by changing remote directory (cd)
  into the (assumed) "directory". This may work if Pathname was actually a
  directory, but as this is not always the case, this test does not work.
  Instead, we now return the actual server result and leave the interpretation
  to the caller.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8247

  Aux Id: seq11407

- \[httpc] - Fixes various bugs in timeout and keep-alive queue handling.

  - When a queued request times, out the error mssage is sent the owner of the
    active request.
  - Requests in the keep-alive queue is forgotten when handler terminates.
  - Timeout out requests are retried.

  Jean-Sébastien Pédron

  Own Id: OTP-8248

- \[httpd] - Unnecessarily strict matching when handling closing sockets.

  Own Id: OTP-8280

## Inets 5.1.3

### Improvements and New Features

-

### Fixed Bugs and Malfunctions

- \[httpc] - Raise condition. When http:request is called and httpc_manager
  selects a session where there's already a pending request, then the connection
  handler for that session effectively resets its parser, readying it for the
  response to the second request. But if there are still some inbound packets
  for the response to the first request, things get confused.

  tomas.abrahamsson@gmail.com

  Own Id: OTP-8154

## Inets 5.1.2

### Improvements and New Features

- \[httpc] - Added http option `connect_timeout` for http client request. The
  `connect_timeout` option is used for the initial request, when the client
  connects to the server. Default value is that of the `timeout` option.

  See the [request/4,5](`httpc:request/4`) function for more info.

  Own Id: OTP-7298

### Fixed Bugs and Malfunctions

- \[httpd] - Failed to create listen socket with invalid option combo. The
  http-server failed to create its listen socket when the bind-address was an
  IPv4-address (a tuple of size 4) and the ipfamily option was inet6fb4.

  Own Id: OTP-8118

  Aux Id: seq11321

- \[httpd] - Removed documentation for non-existing function
  (httpd_util:header/2,3,4).

  Own Id: OTP-8101

## Inets 5.1.1

### Improvements and New Features

- \[httpd] - When starting inets (the web-server) and supplying a descriptor on
  the command line (example: erl -httpd_8888 <descriptor>) it is now possible to
  specify which ip-family to use: `inet | inet6 | inet6fb4`.

  Example: erl -httpd_8888 10|inet6

  When starting the web-server either using a file with property list (the
  proplist_file) or a an property list, using the ipfamily option:
  `{ipfamily, inet | inet6 | inet6fb4}`.

  Finally, when starting the web-server using the classical apache-style config
  file, the `BindAddress` directive has been augmented to allow the
  specification of the IpFamily: `BindAddress blirk.ericsson.se|inet`

  Default is `inet6fb4` which emulates the behaviour of the previous version.

  See the [Communication properties](`m:httpd#props_comm`) section for more
  info.

  Own Id: OTP-8069

  Aux Id: seq11086

### Fixed Bugs and Malfunctions

- \[httpc] - Reception of unexpected data causes handler crash.

  Own Id: OTP-8052

## Inets 5.1

### Improvements and New Features

- \[httpc] Added support for web services using only basic auth, with a token as
  the user part and no password part.

  twoggle@gmail.com

  Own Id: OTP-7998

- \[httpc] - Bind HTTP client to IP-addr. It is now possible to specify an
  alternate ip-address and port to be used when the client connects to the
  server.

  As a side-effect of this, the option `ipv6` has been removed and replaced by
  the `ipfamily` option.

  See [http:set_options/1,2](`httpc:set_options/1`) for more info.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-8004

### Fixed Bugs and Malfunctions

- Updated guard tests (i.e. is_list(L) instead of list(L) and possibly
  andalso/orelse instead of ","/";").

  Own Id: OTP-7994

- \[httpc] - Remove use of the deprecated regexp module.

  Own Id: OTP-8001

- \[httpc] - The option `max_keep_alive_length` was not handled properly.

  Own Id: OTP-8005
