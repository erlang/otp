%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%%

-module(httpd).
-moduledoc """
HTTP server API

An implementation of an HTTP 1.1 compliant web server, as defined in
[RFC 2616](http://www.ietf.org/rfc/rfc2616.txt). Provides web server start
options, administrative functions, and an Erlang callback API.

## Data types

Type definitions that are used more than once in this module:

`boolean() = true | false`

`t:string/0` = list of ASCII characters

`path() = string()` representing a file or a directory path

`ip_address() = {N1,N2,N3,N4} % IPv4 | {K1,K2,K3,K4,K5,K6,K7,K8} % IPv6`

`hostname() = string()` representing a host, for example, "foo.bar.com"

`property() = atom()`

## HTTP server service start & stop

A web server can be configured to start when starting the `Inets` application,
or dynamically in runtime by calling the `Inets` application API
`inets:start(httpd, ServiceConfig)` or `inets:start(httpd, ServiceConfig, How)`,
see `m:inets`. The configuration options, also called properties, are as
follows:

[](){: #props_file }

### File Properties

When the web server is started at application start time, the properties are to
be fetched from a configuration file that can consist of a regular Erlang
property list, that is, `[{Option, Value}]`, where `Option = property() `and
`Value = term()`, followed by a full stop. If the web server is started
dynamically at runtime, a file can still be specified but also the complete
property list.

- [](){: #prop_proplist_file } **`{proplist_file, path()}`**  
  If this property is defined, `Inets` expects to find all other properties
  defined in this file. The file must include all properties listed under
  mandatory properties.

> #### Note {: .info }
>
> Note support for legacy configuration file with Apache syntax is dropped in
> OTP-23.

[](){: #props_mand }

### Mandatory Properties

- [](){: #prop_port } **`{port, integer()}`**  
  The port that the HTTP server listen to. If zero is specified as port, an
  arbitrary available port is picked and function `httpd:info/2` can be used to
  determine which port was picked.

- [](){: #prop_server_root } **`{server_root, path()}`**  
  Defines the home directory of the server, where log files, and so on, can be
  stored. Relative paths specified in other properties refer to this directory.

- [](){: #prop_doc_root } **`{document_root, path()}`**  
  Defines the top directory for the documents that are available on the HTTP
  server.

[](){: #props_comm }

### Communication Properties

- [](){: #prop_bind_address } **`{bind_address, ip_address() | hostname() |
  any}`**  
  Default is `any`

- [](){: #prop_server_name } **`{server_name, string()}`**  
  The name of your server, normally a fully qualified domain name.

  If not given, this defaults to `net_adm:localhost()`.

- [](){: #profile } **`{profile, atom()}`**  
  Used together with [`bind_address`](`m:httpd#prop_bind_address`) and
  [`port`](`m:httpd#prop_port`) to uniquely identify a HTTP server. This can be
  useful in a virtualized environment, where there can be more that one server
  that has the same bind_address and port. If this property is not explicitly
  set, it is assumed that the [`bind_address`](`m:httpd#prop_bind_address`) and
  [`port`](`m:httpd#prop_port`) uniquely identifies the HTTP server.

- [](){: #prop_socket_type } **`{socket_type, ip_comm | {ip_comm, Config::proplist()} | {ssl, Config::proplist()}}`**  
  For `ip_comm` configuration options, see `gen_tcp:listen/2`, some options that
  are used internally by httpd cannot be set.

  For `SSL` configuration options, see `ssl:listen/2`.

  Default is `ip_comm`.

  > #### Note {: .info }
  >
  > OTP-25 deprecates the communication properties
  > `{socket_type, ip_comm | {ip_comm, Config::proplist()} | {essl, Config::proplist()}}`
  > replacing it by
  > `{socket_type, ip_comm | {ip_comm, Config::proplist()} | {ssl, Config::proplist()}}`.

- [](){: #prop_ipfamily } **`{ipfamily, inet | inet6}`**  
  Default is `inet`, legacy option `inet6fb4` no longer makes sense and will be
  translated to inet.

- [](){: #prop_minimum_bytes_per_second } **`{minimum_bytes_per_second,
  integer()}`**  
  If given, sets a minimum of bytes per second value for connections.

  If the value is unreached, the socket closes for that connection.

  The option is good for reducing the risk of "slow DoS" attacks.

[](){: #props_api_modules }

### Erlang Web Server API Modules

- [](){: #prop_modules } **`{modules, [atom()]}`**  
  Defines which modules the HTTP server uses when handling requests. Default is
  `[mod_alias, mod_auth, mod_esi, mod_actions, mod_cgi, mod_dir, mod_get, mod_head, mod_log, mod_disk_log]`.
  Notice that some `mod`\-modules are dependent on others, so the order cannot
  be entirely arbitrary. See the [Inets Web Server Modules](http_server.md) in
  the User's Guide for details.

[](){: #props_limit }

### Limit properties

- [](){: #prop_customize } **`{customize, atom()}`**  
  A callback module to customize the inets HTTP servers behaviour see
  `m:httpd_custom_api`

- [](){: #prop_disable_chunked_encoding }
  **`{disable_chunked_transfer_encoding_send, boolean()}`**  
  Allows you to disable chunked transfer-encoding when sending a response to an
  HTTP/1.1 client. Default is `false`.

- [](){: #prop_keep_alive } **`{keep_alive, boolean()}`**  
  Instructs the server whether to use persistent connections when the client
  claims to be HTTP/1.1 compliant. Default is `true`.

- [](){: #prop_keep_alive_timeout } **`{keep_alive_timeout, integer()}`**  
  The number of seconds the server waits for a subsequent request from the
  client before closing the connection. Default is `150`.

- [](){: #prop_max_body_size } **`{max_body_size, integer()}`**  
  Limits the size of the message body of an HTTP request. Default is no limit.

- [](){: #prop_max_clients } **`{max_clients, integer()}`**  
  Limits the number of simultaneous requests that can be supported. Default is
  `150`.

- [](){: #prop_max_header_size } **`{max_header_size, integer()}`**  
  Limits the size of the message header of an HTTP request. Default is `10240`.

- [](){: #prop_max_content_length } **`{max_content_length, integer()}`**  
  Maximum content-length in an incoming request, in bytes. Requests with content
  larger than this are answered with status 413. Default is `100000000` (100
  MB).

- [](){: #prop_max_uri } **`{max_uri_size, integer()}`**  
  Limits the size of the HTTP request URI. Default is no limit.

- [](){: #prop_max_keep_alive_req } **`{max_keep_alive_request, integer()}`**  
  The number of requests that a client can do on one connection. When the server
  has responded to the number of requests defined by `max_keep_alive_requests`,
  the server closes the connection. The server closes it even if there are
  queued request. Default is no limit.

- [](){: #max_client_body_chunk } **`{max_client_body_chunk, integer()}`**  
  Enforces chunking of a HTTP PUT or POST body data to be delivered to the
  mod_esi callback. Note this is not supported for mod_cgi. Default is no limit
  e.i the whole body is delivered as one entity, which could be very memory
  consuming. `m:mod_esi`.

[](){: #props_admin }

### Administrative Properties

- [](){: #prop_mime_types } **`{mime_types, [{MimeType, Extension}] | path()}`**  
  `MimeType = string()` and `Extension = string()`. Files delivered to the
  client are MIME typed according to RFC 1590. File suffixes are mapped to MIME
  types before file delivery. The mapping between file suffixes and MIME types
  can be specified in the property list.

  Mime types can also be read from a file. The file should contain lines in the
  form `MediaType [Extensions...]`, such as `text/html html htm`. To configure
  this, specify the path to it, such as `{mime_types, "/etc/mime.types"}`.

  If unset, `conf/mime.types` under `server_root` will be used if it exists,
  otherwise, the default is `[{"html","text/html"},{"htm","text/html"}]`.

- [](){: #prop_mime_type } **`{mime_type, string()}`**  
  When the server is asked to provide a document type that cannot be determined
  by the MIME Type Settings, the server uses this default type.

- [](){: #prop_server_admin } **`{server_admin, string()}`**  
  Defines the email-address of the server administrator to be included in any
  error messages returned by the server.

- [](){: #prop_server_tokens } **`{server_tokens,
  none|prod|major|minor|minimal|os|full|{private, string()}}`**  
  Defines the look of the value of the server header.

  Example: Assuming the version of `Inets` is 5.8.1, the server header string
  can look as follows for the different values of server-tokens:

  - **`none`** - "" % A Server: header will not be generated

  - **`prod`** - "inets"

  - **`major`** - "inets/5"

  - **`minor`** - "inets/5.8"

  - **`minimal`** - "inets/5.8.1"

  - **`os`** - "inets/5.8.1 (unix)"

  - **`full`** - "inets/5.8.1 (unix/linux) OTP/R15B"

  - **`{private, "foo/bar"}`** - "foo/bar"

  By default, the value is as before, that is, `minimal`.

- [](){: #prop_logger } **`{logger, Options::list()}`**  
  Currently only one option is supported:

  - **`{error, ServerID::atom()}`** - Produces
    [logger events](`t:logger:log_event/0`) on logger
    [level error](`t:logger:level/0`) under the hierarchical logger
    [domain:](`t:logger:log_event/0`) `[otp, inets, httpd, ServerID, error]` The
    built in logger formatting function produces log entries from the error
    reports:

    ```c
    #{server_name => string()
      protocol => internal | 'TCP' | 'TLS' | 'HTTP',
      transport => "TCP" | "TLS", %% Present when protocol = 'HTTP'
      uri => string(), %% Present when protocol = 'HTTP' and URI is valid
      peer => inet:peername(),
      host => inet:hostname(),
      reason => term()
    }
    ```

    An example of a log entry with only default settings of logger

    ```text
    =ERROR REPORT==== 9-Oct-2019::09:33:27.350235 ===
       Server: My Server
     Protocol: HTTP
    Transport: TLS
          URI: /not_there
         Host: 127.0.1.1:80
         Peer: 127.0.0.1:45253
       Reason: [{statuscode,404},{description,"Object Not Found"}]
    ```

    Using this option makes mod_log and mod_disk_log error logs redundant.

    Add the filter

    ```erlang
    {fun logger_filters:domain/2,
    	{log,equal,[otp,inets, httpd, ServerID, error]}
    ```

    to appropriate logger handler to handle the events. For example to write the
    error log from an httpd server with a `ServerID` of `my_server` to a file
    you can use the following sys.config:

    ```erlang
    [{kernel,
     [{logger,
      [{handler, http_error_test, logger_std_h,
        #{config => #{ file => "log/http_error.log" },
          filters => [{inets_httpd, {fun logger_filters:domain/2,
                                     {log, equal,
                                      [otp, inets, httpd, my_server, error]
                                     }}}],
          filter_default => stop }}]}]}].
    ```

    or if you want to add it to the default logger via an API:

    ```erlang
    logger:add_handler_filter(default,
                              inets_httpd,
                              {fun logger_filters:domain/2,
                               {log, equal,
                                [otp, inets, httpd, my_server, error]}}).
    ```

- [](){: #prop_log_format } **`{log_format, common | combined}`**  
  Defines if access logs are to be written according to the `common` log format
  or the extended common log format. The `common` format is one line looking
  like this: `remotehost rfc931 authuser [date] "request" status bytes`.

  Here:

  - **`remotehost`** - Remote.

  - **`rfc931`** - The remote username of the client
    ([RFC 931](http://www.ietf.org/rfc/rfc931.txt)).

  - **`authuser`** - The username used for authentication.

  - **`[date]`** - Date and time of the request
    ([RFC 1123](http://www.ietf.org/rfc/rfc1123.txt)).

  - **`"request"`** - The request line as it came from the client
    ([RFC 1945](http://www.ietf.org/rfc/rfc1945.txt)).

  - **`status`** - The HTTP status code returned to the client
    ([RFC 1945](http://www.ietf.org/rfc/rfc1945.txt)).

  - **`bytes`** - The content-length of the document transferred.

  The `combined` format is one line looking like this:
  `remotehost rfc931 authuser [date] "request" status bytes "referer" "user_agent"`

  In addition to the earlier:

  - **`"referer"`** - The URL the client was on before requesting the URL (if it
    could not be determined, a minus sign is placed in this field).

  - **`"user_agent"`** - The software the client claims to be using (if it could
    not be determined, a minus sign is placed in this field).

  This affects the access logs written by `mod_log` and `mod_disk_log`.

- [](){: #prop_elog_format } **`{error_log_format, pretty | compact}`**  
  Default is `pretty`. If the error log is meant to be read directly by a human,
  `pretty` is the best option.

  `pretty` has a format corresponding to:

  ```erlang
  io:format("[~s] ~s, reason: ~n ~p ~n~n", [Date, Msg, Reason]).
  ```

  `compact` has a format corresponding to:

  ```erlang
  io:format("[~s] ~s, reason: ~w ~n", [Date, Msg, Reason]).
  ```

  This affects the error logs written by `mod_log` and `mod_disk_log`.

[](){: #props_alias }

### URL Aliasing Properties - Requires mod_alias

- [](){: #prop_alias } **`{alias, {Alias, RealName}}`**  
  `Alias = string()` and `RealName = string()`. `alias` allows documents to be
  stored in the local file system instead of the `document_root` location. URLs
  with a path beginning with url-path is mapped to local files beginning with
  directory-filename, for example:

  ```erlang
  {alias, {"/image", "/ftp/pub/image"}}
  ```

  Access to http://your.server.org/image/foo.gif would refer to the file
  /ftp/pub/image/foo.gif.

- [](){: #prop_re_write } **`{re_write, {Re, Replacement}}`**  
  `Re = string()` and `Replacement = string()`. `re_write` allows documents to
  be stored in the local file system instead of the `document_root` location.
  URLs are rewritten by `re:replace/3` to produce a path in the local
  file-system, for example:

  ```erlang
  {re_write, {"^/[~]([^/]+)(.*)$", "/home/\\1/public\\2"}}
  ```

  Access to http://your.server.org/~bob/foo.gif would refer to the file
  /home/bob/public/foo.gif.

- [](){: #prop_dir_idx } **`{directory_index, [string()]}`**  
  `directory_index` specifies a list of resources to look for if a client
  requests a directory using a `/` at the end of the directory name. `file`
  depicts the name of a file in the directory. Several files can be given, in
  which case the server returns the first it finds, for example:

  ```erlang
  {directory_index, ["index.html", "welcome.html"]}
  ```

  Access to http://your.server.org/docs/ would return
  http://your.server.org/docs/index.html or
  http://your.server.org/docs/welcome.html if index.html does not exist.

[](){: #props_cgi }

### CGI Properties - Requires mod_cgi

- [](){: #prop_script_alias } **`{script_alias, {Alias, RealName}}`**  
  `Alias = string()` and `RealName = string()`. Have the same behavior as
  property `alias`, except that they also mark the target directory as
  containing CGI scripts. URLs with a path beginning with url-path are mapped to
  scripts beginning with directory-filename, for example:

  ```text
  {script_alias, {"/cgi-bin/", "/web/cgi-bin/"}}
  ```

  Access to http://your.server.org/cgi-bin/foo would cause the server to run the
  script /web/cgi-bin/foo.

- [](){: #prop_script_re_write } **`{script_re_write, {Re, Replacement}}`**  
  `Re = string()` and `Replacement = string()`. Have the same behavior as
  property `re_write`, except that they also mark the target directory as
  containing CGI scripts. URLs with a path beginning with url-path are mapped to
  scripts beginning with directory-filename, for example:

  ```text
  {script_re_write, {"^/cgi-bin/(\\d+)/", "/web/\\1/cgi-bin/"}}
  ```

  Access to http://your.server.org/cgi-bin/17/foo would cause the server to run
  the script /web/17/cgi-bin/foo.

- [](){: #prop_script_nocache } **`{script_nocache, boolean()}`**  
  If `script_nocache` is set to `true`, the HTTP server by default adds the
  header fields necessary to prevent proxies from caching the page. Generally
  this is preferred. Default to `false`.

- [](){: #prop_script_timeout } **`{script_timeout, integer()}`**  
  The time in seconds the web server waits between each chunk of data from the
  script. If the CGI script does not deliver any data before the timeout, the
  connection to the client is closed. Default is `15`.

- [](){: #prop_action } **`{action, {MimeType, CgiScript}}`** - requires `mod_actions`  
  `MimeType = string()` and `CgiScript = string()`. `action` adds an action
  activating a CGI script whenever a file of a certain MIME type is requested.
  It propagates the URL and file path of the requested document using the
  standard CGI PATH_INFO and PATH_TRANSLATED environment variables.

  Example:

  ```text
  {action, {"text/plain", "/cgi-bin/log_and_deliver_text"}}
  ```

- [](){: #prop_script } **`{script, {Method, CgiScript}}`** - requires `mod_actions`  
  `Method = string()` and `CgiScript = string()`. `script` adds an action
  activating a CGI script whenever a file is requested using a certain HTTP
  method. The method is either GET or POST, as defined in
  [RFC 1945](http://www.ietf.org/rfc/rfc1945.txt). It propagates the URL and
  file path of the requested document using the standard CGI PATH_INFO and
  PATH_TRANSLATED environment variables.

  Example:

  ```erlang
  {script, {"PUT", "/cgi-bin/put"}}
  ```

[](){: #props_esi }

### ESI Properties - Requires mod_esi

- [](){: #prop_esi_alias } **`{erl_script_alias, {URLPath, [AllowedModule]}}`**  
  `URLPath = string()` and `AllowedModule = atom()`. `erl_script_alias` marks
  all URLs matching url-path as erl scheme scripts. A matching URL is mapped
  into a specific module and function, for example:

  ```erlang
  {erl_script_alias, {"/cgi-bin/example", [httpd_example]}}
  ```

  A request to http://your.server.org/cgi-bin/example/httpd_example:yahoo would
  refer to httpd_example:yahoo/3 or, if that does not exist,
  httpd_example:yahoo/2 and http://your.server.org/cgi-bin/example/other:yahoo
  would not be allowed to execute.

- [](){: #prop_esi_nocache } **`{erl_script_nocache, boolean()}`**  
  If `erl_script_nocache` is set to `true`, the server adds HTTP header fields
  preventing proxies from caching the page. This is generally a good idea for
  dynamic content, as the content often varies between each request. Default is
  `false`.

- [](){: #prop_esi_timeout } **`{erl_script_timeout, integer()}`**  
  If `erl_script_timeout` sets the time in seconds the server waits between each
  chunk of data to be delivered through `mod_esi:deliver/2`. Default is `15`.
  This is only relevant for scripts that use the erl scheme.

[](){: #props_log }

### Log Properties - Requires mod_log

- [](){: #prop_elog } **`{error_log, path()}`**  
  Defines the filename of the error log file to be used to log server errors. If
  the filename does not begin with a slash (/), it is assumed to be relative to
  the `server_root`.

- [](){: #prop_slog } **`{security_log, path()}`**  
  Defines the filename of the access log file to be used to log security events.
  If the filename does not begin with a slash (/), it is assumed to be relative
  to the `server_root`.

- [](){: #prop_tlog } **`{transfer_log, path()}`**  
  Defines the filename of the access log file to be used to log incoming
  requests. If the filename does not begin with a slash (/), it is assumed to be
  relative to the `server_root`.

[](){: #props_dlog }

### Disk Log Properties - Requires mod_disk_log

- [](){: #prop_dlog_format } **`{disk_log_format, internal | external}`**  
  Defines the file format of the log files. See `disk_log` for details. If the
  internal file format is used, the log file is repaired after a crash. When a
  log file is repaired, data can disappear. When the external file format is
  used, `httpd` does not start if the log file is broken. Default is `external`.

- [](){: #prop_edlog } **`{error_disk_log, path()}`**  
  Defines the filename of the (`m:disk_log`) error log file to be used to log
  server errors. If the filename does not begin with a slash (/), it is assumed
  to be relative to the `server_root`.

- [](){: #prop_edlog_size } **`{error_disk_log_size, {MaxBytes, MaxFiles}}`**  
  `MaxBytes = integer()` and `MaxFiles = integer()`. Defines the properties of
  the (`m:disk_log`) error log file. This file is of type wrap log and max bytes
  is written to each file and max files is used before the first file is
  truncated and reused.

- [](){: #prop_sdlog } **`{security_disk_log, path()}`**  
  Defines the filename of the (`m:disk_log`) access log file logging incoming
  security events, that is, authenticated requests. If the filename does not
  begin with a slash (/), it is assumed to be relative to the `server_root`.

- [](){: #prop_sdlog_size } **`{security_disk_log_size, {MaxBytes, MaxFiles}}`**  
  `MaxBytes = integer()` and `MaxFiles = integer()`. Defines the properties of
  the `m:disk_log` access log file. This file is of type wrap log and max bytes
  is written to each file and max files is used before the first file is
  truncated and reused.

- [](){: #prop_tdlog } **`{transfer_disk_log, path()}`**  
  Defines the filename of the (`m:disk_log`) access log file logging incoming
  requests. If the filename does not begin with a slash (/), it is assumed to be
  relative to the `server_root`.

- [](){: #prop_tdlog_size } **`{transfer_disk_log_size, {MaxBytes, MaxFiles}}`**  
  `MaxBytes = integer()` and `MaxFiles = integer()`. Defines the properties of
  the `m:disk_log` access log file. This file is of type wrap log and max bytes
  is written to each file and max files is used before the first file is
  truncated and reused.

[](){: #props_auth }

### Authentication Properties - Requires mod_auth

[](){: #prop_dri }

```erlang
{directory, {path(), [{property(), term()}]}}
```

[](){: #props_dir }

The properties for directories are as follows:

- [](){: #prop_allow_from } **`{allow_from, all | [RegxpHostString]}`**  
  Defines a set of hosts to be granted access to a given directory, for example:

  ```erlang
  {allow_from, ["123.34.56.11", "150.100.23"]}
  ```

  The host `123.34.56.11` and all machines on the `150.100.23` subnet are
  allowed access.

- [](){: #prop_deny_from } **`{deny_from, all | [RegxpHostString]}`**  
  Defines a set of hosts to be denied access to a given directory, for example:

  ```text
  {deny_from, ["123.34.56.11", "150.100.23"]}
  ```

  The host `123.34.56.11` and all machines on the `150.100.23` subnet are not
  allowed access.

- [](){: #prop_auth_type } **`{auth_type, plain | dets | mnesia}`**  
  Sets the type of authentication database that is used for the directory. The
  key difference between the different methods is that dynamic data can be saved
  when Mnesia and Dets are used.

- [](){: #prop_auth_user_file } **`{auth_user_file, path()}`**  
  Sets the name of a file containing the list of users and passwords for user
  authentication. The filename can be either absolute or relative to the
  `server_root`. If using the plain storage method, this file is a plain text
  file where each line contains a username followed by a colon, followed by the
  non-encrypted password. If usernames are duplicated, the behavior is
  undefined.

  Example:

  ```text
  ragnar:s7Xxv7
  edward:wwjau8
  ```

  If the Dets storage method is used, the user database is maintained by Dets
  and must not be edited by hand. Use the API functions in module `mod_auth` to
  create/edit the user database. This directive is ignored if the Mnesia storage
  method is used. For security reasons, ensure that `auth_user_file` is stored
  outside the document tree of the web server. If it is placed in the directory
  that it protects, clients can download it.

- [](){: #prop_auth_group_file } **`{auth_group_file, path()}`**  
  Sets the name of a file containing the list of user groups for user
  authentication. The filename can be either absolute or relative to the
  `server_root`. If the plain storage method is used, the group file is a plain
  text file, where each line contains a group name followed by a colon, followed
  by the members usernames separated by spaces.

  Example:

  ```text
  group1: bob joe ante
  ```

  If the Dets storage method is used, the group database is maintained by Dets
  and must not be edited by hand. Use the API for module `mod_auth` to
  create/edit the group database. This directive is ignored if the Mnesia
  storage method is used. For security reasons, ensure that the
  `auth_group_file` is stored outside the document tree of the web server. If it
  is placed in the directory that it protects, clients can download it.

- [](){: #prop_auth_name } **`{auth_name, string()}`**  
  Sets the name of the authorization realm (auth-domain) for a directory. This
  string informs the client about which username and password to use.

- [](){: #prop_auth_access_passwd } **`{auth_access_password, string()}`**  
  If set to other than `"NoPassword"`, the password is required for all API calls.
  If the password is set to `"DummyPassword"`, the password must be changed before
  any other API calls. To secure the authenticating data, the password must be
  changed after the web server is started. Otherwise it is written in clear text
  in the configuration file.

- [](){: #prop_req_user } **`{require_user, [string()]}`**  
  Defines users to grant access to a given directory using a secret password.

- [](){: #prop_req_grp } **`{require_group, [string()]}`**  
  Defines users to grant access to a given directory using a secret password.

[](){: #props_sec }

### Security Properties - Requires mod_security

[](){: #prop_sec_dir }

```erlang
{security_directory, {path(), [{property(), term()}]}}
```

[](){: #props_sdir }

The properties for the security directories are as follows:

- [](){: #prop_data_file } **`{data_file, path()}`**  
  Name of the security data file. The filename can either be absolute or
  relative to the `server_root`. This file is used to store persistent data for
  module `mod_security`.

- [](){: #prop_max_retries } **`{max_retries, integer()}`**  
  Specifies the maximum number of attempts to authenticate a user before the
  user is blocked out. If a user successfully authenticates while blocked, the
  user receives a 403 (Forbidden) response from the server. If the user makes a
  failed attempt while blocked, the server returns 401 (Unauthorized), for
  security reasons. Default is `3`. Can be set to infinity.

- [](){: #prop_block_time } **`{block_time, integer()}`**  
  Specifies the number of minutes a user is blocked. After this time has passed,
  the user automatically regains access. Default is `60`.

- [](){: #prop_fail_exp_time } **`{fail_expire_time, integer()}`**  
  Specifies the number of minutes a failed user authentication is remembered. If
  a user authenticates after this time has passed, the previous failed
  authentications are forgotten. Default is `30`.

- [](){: #prop_auth_timeout } **`{auth_timeout, integer()}`**  
  Specifies the number of seconds a successful user authentication is
  remembered. After this time has passed, the authentication is no longer
  reported. Default is `30`.

## Web server API data types

The Erlang web server API data types are as follows:

```erlang
ModData = #mod{}

-record(mod, {
    data = [],
    socket_type = ip_comm,
    socket,
    config_db,
    method,
    absolute_uri,
    request_uri,
    http_version,
    request_line,
    parsed_header = [],
    entity_body,
    connection
}).
```

To access the record in your callback-module use:

```erlang
-include_lib("inets/include/httpd.hrl").
```

The fields of record `mod` have the following meaning:

- **`data`** - Type `[{InteractionKey,InteractionValue}]` is used to propagate
  data between modules. Depicted `interaction_data()` in function type
  declarations.

- **`socket_type`** - `socket_type()` indicates whether it is an IP socket or an
  `ssl` socket.

- **`socket`** - The socket, in format `ip_comm` or `ssl`, depending on
  `socket_type`.

- **`config_db`** - The config file directives stored as key-value tuples in an
  ETS table. Depicted `config_db()` in function type declarations.

- **`method`** - Type `"GET" | "POST" | "HEAD" | "TRACE"`, that is, the HTTP
  method.

- **`absolute_uri`** - If the request is an HTTP/1.1 request, the URI can be in
  the absolute URI format. In that case, `httpd` saves the absolute URI in this
  field. An Example of an absolute URI is
  `"http://ServerName:Part/cgi-bin/find.pl?person=jocke"`

- **`request_uri`** - The `Request-URI` as defined in
  [RFC 1945](http://www.ietf.org/rfc/rfc1945.txt), for example,
  `"/cgi-bin/find.pl?person=jocke"`.

- **`http_version`** - The `HTTP` version of the request, that is, "HTTP/1.0",
  or "HTTP/1.1".

- **`request_line`** - The `Request-Line` as defined
  in[RFC 1945](http://www.ietf.org/rfc/rfc1945.txt), for example,
  `"GET /cgi-bin/find.pl?person=jocke HTTP/1.0"`.

- **`parsed_header`** - Type `[{HeaderKey,HeaderValue}]`. `parsed_header`
  contains all HTTP header fields from the HTTP request stored in a list as
  key-value tuples. See [RFC 2616](http://www.ietf.org/rfc/rfc2616.txt) for a
  listing of all header fields. For example, the date field is stored as
  `{"date","Wed, 15 Oct 1997 14:35:17 GMT"}`. RFC 2616 defines that HTTP is a
  case-insensitive protocol and the header fields can be in lower case or upper
  case. `httpd` ensures that all header field names are in lower case.

- **`entity_body`** - The `entity-Body` as defined in
  [RFC 2616](http://www.ietf.org/rfc/rfc2616.txt), for example, data sent from a
  CGI script using the POST method.

- **`connection`** - `true | false`. If set to `true`, the connection to the
  client is a persistent connection and is not closed when the request is
  served.

### See also

[RFC 2616](http://www.ietf.org/rfc/rfc2616.txt), `m:inets`, `m:ssl`
""".

-behaviour(inets_service).

-include("httpd_internal.hrl").

%% Behavior callbacks
-export([
	 start_standalone/1, 
	 start_service/1, 
	 stop_service/1, 
	 services/0, 
	 service_info/1
	]).

%% API
-export([
         parse_query/1,
         reload_config/2,
         info/1,
         info/2,
         info/3,
         info/4
        ]).
-export_type([socket_type/0]).

%% Command line interface
-export([start/1, serve/1]).

-deprecated({parse_query, 1,
            "use uri_string:dissect_query/1 instead"}).

%%%========================================================================
%%% Types
%%%========================================================================
-type property() :: atom().
-type socket_type() :: ip_comm | ssl.

%%%========================================================================
%%% Callbacks
%%%========================================================================
-doc """
When a valid request reaches `httpd`, it calls [`do/1`](`c:do/1`) in each
module, defined by the configuration option of `Module`. The function can
generate data for other modules or a response that can be sent back to the
client.

The field `data` in `ModData` is a list. This list is the list returned from the
last call to [`do/1`](`c:do/1`).

`Body` is the body of the HTTP response that is sent back to the client. An
appropriate header is appended to the message. `StatusCode` is the status code
of the response, see [RFC 2616](http://www.ietf.org/rfc/rfc2616.txt) for the
appropriate values.

`Head` is a key value list of HTTP header fields. The server constructs an HTTP
header from this data. See [RFC 2616](http://www.ietf.org/rfc/rfc2616.txt) for
the appropriate value for each header field. If the client is an HTTP/1.0
client, the server filters the list so that only HTTP/1.0 header fields are sent
back to the client.

If `Body` is returned and equal to `{Fun,Arg}`, the web server tries
[`apply/2`](`apply/2`) on `Fun` with `Arg` as argument. The web server expects
that the fun either returns a list `(Body)` that is an HTTP response, or the
atom `sent` if the HTTP response is sent back to the client. If `close` is
returned from the fun, something has gone wrong and the server signals this to
the client by closing the connection.
""".
-doc(#{group => <<"ERLANG WEB SERVER API CALLBACK FUNCTIONS">>}).
-callback do(ModData) -> {proceed, OldData} | {proceed, NewData} | {break, NewData} | done when
      ModData :: [{data,NewData} | {'Body', Body} | {'Head',Head}],
      OldData :: list(),
      NewData :: [{response, {StatusCode, Body}}],
      StatusCode :: integer(),
      Body :: iolist() | nobody | {Fun, FunArg},
      Head :: [HeaderOption],
      HeaderOption :: {Option, Value} | {code, StatusCode},
      Option :: accept_ranges | allow,
      Value :: string(),
      FunArg :: [term()],
      Fun :: fun((FunArg) -> sent | close | Body).

-doc """
When `httpd` is shut down, it tries to execute [`remove/1`](`c:remove/1`) in
each Erlang web server callback module. The programmer can use this function to
clean up resources created in the store function.
""".
-doc(#{group => <<"ERLANG WEB SERVER API CALLBACK FUNCTIONS">>}).
-callback remove(ConfigDB) -> ok | {error, Reason} when
      ConfigDB :: ets:tid(), Reason :: term().

-doc """
Checks the validity of the configuration options before saving them in the
internal database. This function can also have a side effect, that is, setup of
necessary extra resources implied by the configuration option. It can also
resolve possible dependencies among configuration options by changing the value
of the option. This function only needs clauses for the options implemented by
this particular callback module.
""".
-doc(#{group => <<"ERLANG WEB SERVER API CALLBACK FUNCTIONS">>}).
-callback store({Option, Value}, Config) ->
    {ok, {Option, NewValue}} | {error, Reason} when
      Option :: property(),
      Config :: [{Option, Value}],
      Value :: term(),
      NewValue :: term(),
      Reason :: term().

-optional_callbacks([remove/1, store/2]).

%%%========================================================================
%%% API
%%%========================================================================

-doc """
[`parse_query/1`](`parse_query/1`) parses incoming data to `erl` and `eval`
scripts (see `m:mod_esi`) as defined in the standard URL format, that is, '+'
becomes 'space' and decoding of hexadecimal characters (`%xx`).
""".
-doc(#{group => <<"Web server API help functions">>}).
-spec parse_query(QueryString) -> QueryList | uri_string:error() when
      QueryString :: string(),
      QueryList :: [{unicode:chardata(), unicode:chardata() | true}].
parse_query(String) ->
    uri_string:dissect_query(String).

-doc """
Reloads the HTTP server configuration without restarting the server. Incoming
requests are answered with a temporary down message during the reload time.

> #### Note {: .info }
>
> Available properties are the same as the start options of the server, but the
> properties `bind_address` and `port` cannot be changed.

If mode is disturbing, the server is blocked forcefully, all ongoing requests
terminates, and the reload starts immediately. If mode is non-disturbing, no new
connections are accepted, but ongoing requests are allowed to complete before
the reload is done.
""".
-spec reload_config(Config, Mode) -> ok | {error, Reason} | no_return() when
      Config :: file:name_all() | [{Option, Value}],
      Mode   :: non_disturbing | disturbing | blocked,
      Option :: atom(),
      Value  :: term(),
      Reason :: term().
reload_config(Config = [Value| _], Mode) when is_tuple(Value) ->
    do_reload_config(Config, Mode);
reload_config(ConfigFile, Mode) ->
    try file:consult(ConfigFile) of
        {ok, [PropList]} ->
            %% Erlang terms format
            do_reload_config(PropList, Mode)
    catch
        exit:_ ->
            throw({error, {could_not_consult_proplist_file, ConfigFile}})
    end.

-doc(#{equiv => info/2}).
-spec info(Pid) -> HttpInformation when
      Pid :: pid(),
      Path :: file:name_all(),
      HttpInformation :: [CommonOption]
                       | [CommunicationOption]
                       | [ModOption]
                       | [LimitOption]
                       | [AdminOption],
      CommonOption :: {port, non_neg_integer()}
                | {server_name, string()}
                | {server_root, Path}
                | {document_root, Path},
      CommunicationOption :: {bind_address, inet:ip_address() | inet:hostname() | any}
        | {profile, atom()}
        | { socket_type,
            ip_comm | {ip_comm, ssl:tls_option() | gen_tcp:option()} | {ssl, ssl:tls_option() | gen_tcp:option()}}
        | {ipfamily, inet | inet6}
        | {minimum_bytes_per_second, integer()},
      ModOption :: {modules, atom()},
      LimitOption :: {customize, atom()}
                   | {disable_chunked_transfer_encoding_send, boolean()}
                   | {keep_alive, boolean()}
                   | {keep_alive_timeout, integer()}
                   | {max_body_size, integer()}
                   | {max_clients, integer()}
                   | {max_header_size, integer()}
                   | {max_content_length, integer()}
                   | {max_uri_size, integer()}
                   | {max_keep_alive_request, integer()}
                   | {max_client_body_chunk, integer()},
      AdminOption :: {mime_types, [{MimeType :: string(), Extension :: string()}] | Path}
                   | {mime_type, string()}
                   | {server_admin, string()}
                   | {server_tokens, none|prod|major|minor|minimal|os|full|{private, string()}}
                   | {logger, Options::list()}
                   | {log_format, common | combined}
                   | {error_log_format, pretty | compact}.
info(Pid) when is_pid(Pid) ->
    info(Pid, []).

-doc """
Fetches information about the HTTP server. When called with only the pid, all
properties are fetched. When called with a list of specific properties, they are
fetched. The available properties are the same as the start options of the
server.

> #### Note {: .info }
>
> Pid is the pid returned from `inets:start/[2,3]`. Can also be retrieved form
> `inets:services/0` and `inets:services_info/0`, see `m:inets`.
""".
-doc(#{equiv => info/4}).
-spec info(Pid, Properties) -> HttpInformation  when
      Pid     :: pid(),
      Properties :: [atom()],
      HttpInformation :: [CommonOption]
                       | [CommunicationOption]
                       | [ModOption]
                       | [LimitOption]
                       | [AdminOption],
      CommonOption :: {port, non_neg_integer()}
                | {server_name, string()}
                | {server_root, Path}
                | {document_root, Path},
      CommunicationOption :: {bind_address, inet:ip_address() | inet:hostname() | any}
        | {profile, atom()}
        | { socket_type,
            ip_comm | {ip_comm, ssl:tls_option() | gen_tcp:option()} | {ssl, ssl:tls_option() | gen_tcp:option()}}
        | {ipfamily, inet | inet6}
        | {minimum_bytes_per_second, integer()},
      ModOption :: {modules, atom()},
      LimitOption :: {customize, atom()}
                   | {disable_chunked_transfer_encoding_send, boolean()}
                   | {keep_alive, boolean()}
                   | {keep_alive_timeout, integer()}
                   | {max_body_size, integer()}
                   | {max_clients, integer()}
                   | {max_header_size, integer()}
                   | {max_content_length, integer()}
                   | {max_uri_size, integer()}
                   | {max_keep_alive_request, integer()}
                   | {max_client_body_chunk, integer()},
      AdminOption :: {mime_types, [{MimeType :: string(), Extension :: string()}] | Path}
                   | {mime_type, string()}
                   | {server_admin, string()}
                   | {server_tokens, none|prod|major|minor|minimal|os|full|{private, string()}}
                   | {logger, Options::list()}
                   | {log_format, common | combined}
                   | {error_log_format, pretty | compact};
          (Address, Port) -> HttpInformation when
      Address :: inet:ip_address(),
      Port    :: integer(),
      Path :: file:name_all(),
      HttpInformation :: [CommonOption]
                       | [CommunicationOption]
                       | [ModOption]
                       | [LimitOption]
                       | [AdminOption],
      CommonOption :: {port, non_neg_integer()}
                | {server_name, string()}
                | {server_root, Path}
                | {document_root, Path},
      CommunicationOption :: {bind_address, inet:ip_address() | inet:hostname() | any}
        | {profile, atom()}
        | { socket_type,
            ip_comm | {ip_comm, ssl:tls_option() | gen_tcp:option()} | {ssl, ssl:tls_option() | gen_tcp:option()}}
        | {ipfamily, inet | inet6}
        | {minimum_bytes_per_second, integer()},
      ModOption :: {modules, atom()},
      LimitOption :: {customize, atom()}
                   | {disable_chunked_transfer_encoding_send, boolean()}
                   | {keep_alive, boolean()}
                   | {keep_alive_timeout, integer()}
                   | {max_body_size, integer()}
                   | {max_clients, integer()}
                   | {max_header_size, integer()}
                   | {max_content_length, integer()}
                   | {max_uri_size, integer()}
                   | {max_keep_alive_request, integer()}
                   | {max_client_body_chunk, integer()},
      AdminOption :: {mime_types, [{MimeType :: string(), Extension :: string()}] | Path}
                   | {mime_type, string()}
                   | {server_admin, string()}
                   | {server_tokens, none|prod|major|minor|minimal|os|full|{private, string()}}
                   | {logger, Options::list()}
                   | {log_format, common | combined}
                   | {error_log_format, pretty | compact}.
info(Pid, Properties) when is_pid(Pid) andalso is_list(Properties) ->
    {ok, ServiceInfo} = service_info(Pid), 
    Address = proplists:get_value(bind_address, ServiceInfo),
    Port = proplists:get_value(port, ServiceInfo),
    Profile = proplists:get_value(profile, ServiceInfo, default),
    case Properties of
	[] ->
	    info(Address, Port, Profile);
	_ ->
	    info(Address, Port, Profile, Properties)
    end; 

info(Address, Port) when is_integer(Port) ->
    info(Address, Port, default).

-doc(#{equiv => info/4}).
-spec info(Address, Port, Profile) -> HttpInformation when
      Address :: inet:ip_address() | any,
      Port    :: integer(),
      Profile :: atom(),
      Path :: file:name_all(),
      HttpInformation :: [CommonOption]
                       | [CommunicationOption]
                       | [ModOption]
                       | [LimitOption]
                       | [AdminOption],
      CommonOption :: {port, non_neg_integer()}
                | {server_name, string()}
                | {server_root, Path}
                | {document_root, Path},
      CommunicationOption :: {bind_address, inet:ip_address() | inet:hostname() | any}
        | {profile, atom()}
        | { socket_type,
            ip_comm | {ip_comm, ssl:tls_option() | gen_tcp:option()} | {ssl, ssl:tls_option() | gen_tcp:option()}}
        | {ipfamily, inet | inet6}
        | {minimum_bytes_per_second, integer()},
      ModOption :: {modules, atom()},
      LimitOption :: {customize, atom()}
                   | {disable_chunked_transfer_encoding_send, boolean()}
                   | {keep_alive, boolean()}
                   | {keep_alive_timeout, integer()}
                   | {max_body_size, integer()}
                   | {max_clients, integer()}
                   | {max_header_size, integer()}
                   | {max_content_length, integer()}
                   | {max_uri_size, integer()}
                   | {max_keep_alive_request, integer()}
                   | {max_client_body_chunk, integer()},
      AdminOption :: {mime_types, [{MimeType :: string(), Extension :: string()}] | Path}
                   | {mime_type, string()}
                   | {server_admin, string()}
                   | {server_tokens, none|prod|major|minor|minimal|os|full|{private, string()}}
                   | {logger, Options::list()}
                   | {log_format, common | combined}
                   | {error_log_format, pretty | compact};
          (Address, Port, Properties) -> HttpInformation when
      Address :: inet:ip_address() | any,
      Port    :: integer(),
      Properties :: [atom()],
      Path :: file:name_all(),
      HttpInformation :: [CommonOption]
                       | [CommunicationOption]
                       | [ModOption]
                       | [LimitOption]
                       | [AdminOption],
      CommonOption :: {port, non_neg_integer()}
                | {server_name, string()}
                | {server_root, Path}
                | {document_root, Path},
      CommunicationOption :: {bind_address, inet:ip_address() | inet:hostname() | any}
        | {profile, atom()}
        | { socket_type,
            ip_comm | {ip_comm, ssl:tls_option() | gen_tcp:option()} | {ssl, ssl:tls_option() | gen_tcp:option()}}
        | {ipfamily, inet | inet6}
        | {minimum_bytes_per_second, integer()},
      ModOption :: {modules, atom()},
      LimitOption :: {customize, atom()}
                   | {disable_chunked_transfer_encoding_send, boolean()}
                   | {keep_alive, boolean()}
                   | {keep_alive_timeout, integer()}
                   | {max_body_size, integer()}
                   | {max_clients, integer()}
                   | {max_header_size, integer()}
                   | {max_content_length, integer()}
                   | {max_uri_size, integer()}
                   | {max_keep_alive_request, integer()}
                   | {max_client_body_chunk, integer()},
      AdminOption :: {mime_types, [{MimeType :: string(), Extension :: string()}] | Path}
                   | {mime_type, string()}
                   | {server_admin, string()}
                   | {server_tokens, none|prod|major|minor|minimal|os|full|{private, string()}}
                   | {logger, Options::list()}
                   | {log_format, common | combined}
                   | {error_log_format, pretty | compact}.
info(Address, Port, Profile) when is_integer(Port), is_atom(Profile) ->
    httpd_conf:get_config(Address, Port, Profile);

info(Address, Port, Properties) when is_integer(Port) andalso 
				     is_list(Properties) ->    
    httpd_conf:get_config(Address, Port, default, Properties).

-doc """
Fetches information about the HTTP server. When called with only `Address` and
`Port`, all properties are fetched. When called with a list of specific
properties, they are fetched. The available properties are the same as the start
options of the server.

> #### Note {: .info }
>
> The `Address` must be the IP address and cannot be the hostname.
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec info(Address, Port, Profile, Properties) -> HttpInformation when
      Address :: inet:ip_address() | any,
      Port    :: integer(),
      Profile :: atom(),
      Properties :: [atom()],
      Path :: file:name_all(),
      HttpInformation :: [CommonOption]
                       | [CommunicationOption]
                       | [ModOption]
                       | [LimitOption]
                       | [AdminOption],
      CommonOption :: {port, non_neg_integer()}
                | {server_name, string()}
                | {server_root, Path}
                | {document_root, Path},
      CommunicationOption :: {bind_address, inet:ip_address() | inet:hostname() | any}
        | {profile, atom()}
        | { socket_type,
            ip_comm | {ip_comm, ssl:tls_option() | gen_tcp:option()} | {ssl, ssl:tls_option() | gen_tcp:option()}}
        | {ipfamily, inet | inet6}
        | {minimum_bytes_per_second, integer()},
      ModOption :: {modules, atom()},
      LimitOption :: {customize, atom()}
                   | {disable_chunked_transfer_encoding_send, boolean()}
                   | {keep_alive, boolean()}
                   | {keep_alive_timeout, integer()}
                   | {max_body_size, integer()}
                   | {max_clients, integer()}
                   | {max_header_size, integer()}
                   | {max_content_length, integer()}
                   | {max_uri_size, integer()}
                   | {max_keep_alive_request, integer()}
                   | {max_client_body_chunk, integer()},
      AdminOption :: {mime_types, [{MimeType :: string(), Extension :: string()}] | Path}
                   | {mime_type, string()}
                   | {server_admin, string()}
                   | {server_tokens, none|prod|major|minor|minimal|os|full|{private, string()}}
                   | {logger, Options::list()}
                   | {log_format, common | combined}
                   | {error_log_format, pretty | compact}.
info(Address, Port, Profile, Properties) when is_integer(Port) andalso
					      is_atom(Profile) andalso is_list(Properties) ->    
    httpd_conf:get_config(Address, Port, Profile, Properties).


%%%========================================================================
%%% Behavior callbacks
%%%========================================================================

-doc false.
start_standalone(Config0) ->
    Config = httpd_ssl_wrapper(Config0),
    httpd_sup:start_link([{httpd, Config}], stand_alone).

-doc false.
start_service(Config0) ->
    Config = httpd_ssl_wrapper(Config0),
    httpd_sup:start_child(Config).

httpd_ssl_wrapper(Config0) ->
    case proplists:get_value(socket_type, Config0) of
        {essl, Value} ->
            lists:keyreplace(socket_type, 1, Config0, {socket_type, {ssl, Value}});
        _ -> Config0
    end.


-doc false.
stop_service({Address, Port}) ->
    stop_service({Address, Port, ?DEFAULT_PROFILE});
stop_service({Address, Port, Profile}) ->
    Name  = httpd_util:make_name("httpd_instance_sup", Address, Port, Profile),
    Pid = whereis(Name),
    MonitorRef = erlang:monitor(process, Pid),
    Result = httpd_sup:stop_child(Address, Port, Profile),
    receive
        {'DOWN', MonitorRef, _, _, _} ->
            Result
    end;     
stop_service(Pid) when is_pid(Pid) ->
    case service_info(Pid)  of
	{ok, Info} ->	   
	    Address = proplists:get_value(bind_address, Info),
	    Port = proplists:get_value(port, Info),
	    Profile = proplists:get_value(profile, Info, ?DEFAULT_PROFILE),
	    stop_service({Address, Port, Profile});
	Error ->
	    Error
    end.
	    
-doc false.
services() ->
    [{httpd, ChildPid} || {_, ChildPid, _, _} <- 
			      supervisor:which_children(httpd_sup)].
-doc false.
service_info(Pid) ->
    try
	[{ChildName, ChildPid} || 
	    {ChildName, ChildPid, _, _} <- 
		supervisor:which_children(httpd_sup)] of
	Children ->
	    child_name2info(child_name(Pid, Children))
    catch
	exit:{noproc, _} ->
	    {error, service_not_available} 
    end.

%%%--------------------------------------------------------------
%%% Command line interface
%%%--------------------------------------------------------------------

parse_ip_address(Input) ->
    case inet:parse_address(Input) of
        {ok, Address} -> Address;
        {error, einval} -> error(badarg)
    end.

%% Try to locate good mime types to use for the server.
%% If none were found on the host, uses a slim default.
default_mime_types() ->
    Locations = [
        "/etc/mime.types"
        % Note nginx installations also occasionally host a `mime.types` file,
        % but this is usually in nginx's own configuration file format. Apache,
        % on the other hand, uses the standard format and can be used.
    ],
    find_mime_types(Locations).

find_mime_types([Path | Paths]) ->
    case filelib:is_file(Path) of
        true -> Path;
        false -> find_mime_types(Paths)
    end;

find_mime_types([]) ->
    [
        {"html", "text/html"}, {"htm", "text/html"}, {"js", "text/javascript"},
        {"css","text/css"}, {"gif", "image/gif"}, {"jpg", "image/jpeg"},
        {"jpeg", "image/jpeg"}, {"png", "image/png"}
    ].

serve_cli() ->
    #{
      arguments => [
        #{
          name => directory,
          type => string,
          help => "Directory to serve data from.",
          default => "."
        },
        #{
          name => help,
          type => boolean,
          short => $h,
          long => "-help",
          help => "Show this description."
        },
        #{
          name => port,
          type => {integer, [{min, 0}, {max, 65535}]},
          short => $p,
          long => "-port",
          default => 8000,
          help => (
            "Port to bind on. Use '0' for the OS to automatically assign "
            "a port which can then be seen on server startup."
          )
        },
        #{
          name => address,
          type => {custom, fun parse_ip_address/1},
          short => $b,
          long => "-bind",
          default => {127, 0, 0, 1},
          help => "IP address to listen on. Use 0.0.0.0 or :: for all interfaces."
        }
      ],
      help => "Start a HTTP server serving files from DIRECTORY.",
      handler => fun do_serve/1
    }.

-doc false.
start(Args) ->
    %% `-S` without a function and without arguments
    serve(Args).

-doc false.
serve(Args) ->
    argparse:run(Args, serve_cli(), #{progname => "erl -S httpd serve"}).

do_serve(#{help := true}) ->
    io:format("~ts", [argparse:help(serve_cli())]),
    erlang:halt(0);
do_serve(#{address := Address, port := Port, directory := Path}) ->
    AbsPath = string:trim(filename:absname(Path), trailing, "/."),
    inets:start(),
    IpFamilyOpts = case Address of 
        {_, _, _, _} -> [];
        _ -> [{ipfamily, inet6}]
    end,
    {ok, Pid} = start_service(
      [
         {bind_address, Address},
         {document_root, AbsPath},
         {server_root, AbsPath},
         {directory_index, ["index.html"]},
         {port, Port},
         {mime_type, "application/octet-stream"},
         {mime_types, default_mime_types()},
         {modules, [mod_alias, mod_dir, mod_get]}
      ] ++ IpFamilyOpts
    ),
    % This is needed to support random port assignment (--port 0)
    [{port, ActualPort}] = info(Pid, [port]),
    io:fwrite("Started HTTP server on http://~s:~w at ~s~n",
              [inet:ntoa(Address), ActualPort, AbsPath]),
    receive
        {From, shutdown} ->
            ok = stop_service(Pid),
            From ! done
    end.

%%%--------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------

child_name(_, []) ->
    undefined;
child_name(Pid, [{Name, Pid} | _]) ->
    Name;
child_name(Pid, [_ | Children]) ->
    child_name(Pid, Children).

-spec child_name2info(undefined | HTTPSup) -> Object when
      HTTPSup :: {httpd_instance_sup, any, Port, Profile}
               | {httpd_instance_sup, Address, Port, Profile},
      Port    :: integer(),
      Address :: inet:ip_address() | any,
      Profile :: atom(),
      Object  :: {error, no_such_service} | {ok, [tuple()]}.
child_name2info(undefined) ->
    {error, no_such_service};
child_name2info({httpd_instance_sup, any, Port, Profile}) ->
    {ok, Host} = inet:gethostname(),
    Info = info(any, Port, Profile, [server_name]),
    {ok, [{bind_address,  any}, {host, Host}, {port, Port} | Info]};
child_name2info({httpd_instance_sup, Address, Port, Profile}) ->
    Info = info(Address, Port, Profile, [server_name]),
    case inet:gethostbyaddr(Address) of
	{ok, {_, Host, _, _,_, _}} ->
	    {ok, [{bind_address, Address}, 
		  {host, Host}, {port, Port} | Info]};
	_  ->
	    {ok, [{bind_address, Address}, {port, Port} | Info]}
    end.


reload(Config, Address, Port, Profile) ->
    Name = make_name(Address,Port, Profile),
    case whereis(Name) of
	Pid when is_pid(Pid) ->
	    httpd_manager:reload(Pid, Config);
	_ ->
	    {error,not_started}
    end.

    
%%% =========================================================
%%% Function:    block/3, block/4
%%%              block(Addr, Port, Mode)
%%%              block(ConfigFile, Mode, Timeout)
%%%              block(Addr, Port, Mode, Timeout)
%%% 
%%% Returns:     ok | {error,Reason}
%%%              
%%% Description: This function is used to block an HTTP server.
%%%              The blocking can be done in two ways, 
%%%              disturbing or non-disturbing. Default is disturbing.
%%%              When a HTTP server is blocked, all requests are rejected
%%%              (status code 503).
%%% 
%%%              disturbing:
%%%              By performing a disturbing block, the server
%%%              is blocked forcefully and all ongoing requests
%%%              are terminated. No new connections are accepted.
%%%              If a timeout time is given then, on-going requests
%%%              are given this much time to complete before the
%%%              server is forcefully blocked. In this case no new 
%%%              connections is accepted.
%%% 
%%%              non-disturbing:
%%%              A non-disturbing block is more graceful. No
%%%              new connections are accepted, but the ongoing 
%%%              requests are allowed to complete.
%%%              If a timeout time is given, it waits this long before
%%%              giving up (the block operation is aborted and the 
%%%              server state is once more not-blocked).
%%%
%%% Types:       Port       -> integer()             
%%%              Addr       -> {A,B,C,D} | string() | undefined
%%%              ConfigFile -> string()
%%%              Mode       -> disturbing | non_disturbing
%%%              Timeout    -> integer()
%%%

block(Addr, Port, Profile, disturbing) when is_integer(Port) ->
    do_block(Addr, Port, Profile, disturbing);
block(Addr, Port, Profile, non_disturbing) when is_integer(Port) ->
    do_block(Addr, Port, Profile, non_disturbing).
do_block(Addr, Port, Profile, Mode) when is_integer(Port) andalso is_atom(Mode) -> 
    Name = make_name(Addr, Port, Profile),
    case whereis(Name) of
	Pid when is_pid(Pid) ->
	    httpd_manager:block(Pid, Mode);
	_ ->
	    {error,not_started}
    end.
    
%%% =========================================================
%%% Function:    unblock/2
%%%              unblock(Addr, Port)
%%%              
%%% Description: This function is used to reverse a previous block 
%%%              operation on the HTTP server.
%%%
%%% Types:       Port       -> integer()             
%%%              Addr       -> {A,B,C,D} | string() | undefined
%%%              ConfigFile -> string()
%%%

unblock(Addr, Port, Profile) when is_integer(Port) -> 
    Name = make_name(Addr,Port, Profile),
    case whereis(Name) of
	Pid when is_pid(Pid) ->
	    httpd_manager:unblock(Pid);
	_ ->
	    {error,not_started}
    end.


make_name(Addr, Port, Profile) ->
    httpd_util:make_name("httpd", Addr, Port, Profile).


do_reload_config(ConfigList, Mode) ->
    case (catch httpd_conf:validate_properties(ConfigList)) of
	{ok, Config} ->
	    Address = proplists:get_value(bind_address, Config, any), 
	    Port    = proplists:get_value(port, Config, 80),
	    Profile = proplists:get_value(profile, Config, default),
	    case block(Address, Port, Profile, Mode) of
		ok ->
		    reload(Config, Address, Port, Profile),
		    unblock(Address, Port, Profile);
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

%%%--------------------------------------------------------------
%%% Deprecated 
%%%--------------------------------------------------------------
