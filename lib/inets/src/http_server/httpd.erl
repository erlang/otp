%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1997-2026. All Rights Reserved.
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

## HTTP server service start & stop

A web server can be configured to start when starting the `Inets` application,
or dynamically in runtime by calling the `Inets` application API
`inets:start(httpd, ServiceConfig)` or `inets:start(httpd, ServiceConfig, How)`,
see `m:inets`.

When the web server is started at application start time, the properties are to be part of the [inets applications sys config](`e:kernel:config.md`). If the web server is started dynamically at runtime, a file can still be specified but also the complete property list.

The configuration options, also called properties, are as
follows:

* [File Properties](`t:file_option/0`)
* [Mandatory Properties](`t:mandatory_option/0`)
* [Communication Properties](`t:communication_option/0`)
* [Module Properties](`t:mod_option/0`)
* [Limit Properties](`t:limit_option/0`)
* [Admin Properties](`t:admin_option/0`)

Properties for specific modules:

* [URL Aliasing Properties](`t:mod_alias:url_alias_option/0`) - Requires `m:mod_alias`
* [ESI Properties](`t:mod_esi:esi_option/0`) - Requires `m:mod_esi`
* [Log Properties](`t:mod_log:log_option/0`) - Requires `m:mod_log`
* [Disk Log Properties](`t:mod_disk_log:disk_log_option/0`) - Requires `m:mod_disk_log`
* [Authentication Properties](`t:mod_auth:auth_option/0`) - Requires `m:mod_auth`
* [Security Properties](`t:mod_security:security_option/0`) - Requires `m:mod_security`

> #### Note {: .info }
>
> In OTP 30, `mod_cgi` and `mod_actions` were removed.

### See also

[RFC 2616](http://www.ietf.org/rfc/rfc2616.txt), `m:inets`, `m:ssl`
""".

-compile([{nowarn_possibly_unsafe_function, {file, consult, 1}}]).

-behaviour(inets_service).

-include("httpd_internal.hrl").
-include("../../include/httpd.hrl").

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
-export_type([socket_type/0, config_db/0, file_option/0]).

%% Command line interface
-export([start/1, serve/1]).

-deprecated({parse_query, 1,
            "use uri_string:dissect_query/1 instead"}).

%%%========================================================================
%%% Types
%%%========================================================================
-type property() :: atom().
-type socket_type() :: ip_comm | ssl.
-doc """
- [](){: #prop_proplist_file } **`{proplist_file, path()}`**  
  If this property is defined, `Inets` expects to find all other properties
  defined in this file. The file must include all properties listed under
  mandatory properties.

> #### Note {: .info }
>
> Note support for legacy configuration file with Apache syntax was dropped in
> OTP-23.

""".
-type file_option() :: {proplist_file, Path :: file:name_all()}.

-doc """
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
""".
-type mandatory_option() :: {port, non_neg_integer()}
                | {server_root, Path :: file:name_all()}
                | {document_root, Path :: file:name_all()}.

-doc """
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
""".
-type limit_option() :: {customize, atom()}
                   | {disable_chunked_transfer_encoding_send, boolean()}
                   | {keep_alive, boolean()}
                   | {keep_alive_timeout, integer()}
                   | {max_body_size, integer()}
                   | {max_clients, integer()}
                   | {max_header_size, integer()}
                   | {max_content_length, integer()}
                   | {max_uri_size, integer()}
                   | {max_keep_alive_request, integer()}
                   | {max_client_body_chunk, integer()}.
-doc """
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
""".
-type admin_option() :: {mime_types, [{MimeType :: string(), Extension :: string()}] | Path :: file:name_all()}
                   | {mime_type, string()}
                   | {server_admin, string()}
                   | {server_tokens, none|prod|major|minor|minimal|os|full|{private, string()}}
                   | {logger, Options::list()}
                   | {log_format, common | combined}
                   | {error_log_format, pretty | compact}.
-doc """
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
""".
-type communication_option() :: {bind_address, inet:ip_address() | inet:hostname() | any}
        | {server_name, string()}
        | {profile, atom()}
        | { socket_type,
            ip_comm | {ip_comm, ssl:tls_option() | gen_tcp:option()} | {ssl, ssl:tls_option() | gen_tcp:option()}}
        | {ipfamily, inet | inet6}
        | {minimum_bytes_per_second, integer()}.
-doc """
- [](){: #prop_modules } **`{modules, [atom()]}`**  
  Defines which modules the HTTP server uses when handling requests. Default is
  `[mod_alias, mod_auth, mod_esi, mod_dir, mod_get, mod_head, mod_log, mod_disk_log]`.
  Notice that some `mod`\-modules are dependent on others, so the order cannot
  be entirely arbitrary. See the [Inets Web Server Modules](http_server.md) in
  the User's Guide for details.
""".
-type mod_option() :: {modules, atom()}.

-doc """
The Erlang web server API data type `t:mod_data/0` is a record of type `mod` that is used to propagate data between modules.

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
  method as a string.

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

""".
-type mod_data() ::
    #mod{ data :: interaction_data(),
          socket_type :: socket_type(),
          socket :: gen_tcp:socket() | ssl:sslsocket(),
          config_db :: config_db(),
          method :: string(),
          absolute_uri :: string() | undefined,
          request_uri :: string(),
          http_version :: string(),
          request_line :: string(),
          parsed_header :: [{HeaderKey :: string(), HeaderValue :: string()}],
          entity_body :: iolist() | undefined,
          connection :: boolean()
}.

-doc """
The config file directives stored as key-value tuples in an ETS table, as
described in the [Inets User's Guide](http_server.md). This is the value held in
the `config_db` field of `t:mod_data/0`.
""".
-type config_db() :: ets:table().

-doc """
Data used to propagate information between callback modules while a single
request is processed. This is the value held in the `data` field of
`t:mod_data/0`.
""".
-type interaction_data() :: [{InteractionKey :: term(), InteractionValue :: term()}].

%%%========================================================================
%%% Callbacks
%%%========================================================================
-doc """
When a valid request reaches `httpd`, it calls [`do/1`](`c:do/1`) in each
module, defined by the configuration option of `t:mod_option/0`. The function can
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

> #### Note {: .info }
>
> It is strongly advised to use NewDataFormat in the return value of `do/1`
> as it relies on a newer mechanism for parsing and sending headers,
> provides more accurate status codes, and supports a wider range of Body formats.
>

""".
-callback do(ModData :: mod_data()) -> {proceed, OldData} | {proceed, NewData} | {break, NewData} | done when
      OldData :: list(),
      NewData :: [{response, NewDataCompatFormat}] | [{response, NewDataFormat}],
      NewDataCompatFormat :: {StatusCode, Body},
      NewDataFormat :: {response, Head, Body} | {already_sent, StatusCode, Size},
      StatusCode :: integer(),
      Size :: non_neg_integer(),
      Body :: iolist() | nobody | {Fun, FunArg},
      Head :: [HeaderOption],
      HeaderOption :: {Option, Value} | {code, StatusCode},
      Option :: accept_ranges | allow
              | cache_control | content_MD5
              | content_encoding | content_language
              | content_length | content_location
              | content_range | content_type | date
              | etag | expires | last_modified
              | location | pragma | retry_after
              | server | trailer | transfer_encoding,
      Value :: string(),
      FunArg :: [term()],
      Fun :: fun((FunArg) -> sent | close | Body).

-doc """
When `httpd` is shut down, it tries to execute [`remove/1`](`c:remove/1`) in
each Erlang web server callback module. The programmer can use this function to
clean up resources created in the store function.
""".
-callback remove(ConfigDB) -> ok | {error, Reason} when
      ConfigDB :: config_db(), Reason :: term().

-doc """
Checks the validity of the configuration options before saving them in the
internal database. This function can also have a side effect, that is, setup of
necessary extra resources implied by the configuration option. It can also
resolve possible dependencies among configuration options by changing the value
of the option. This function only needs clauses for the options implemented by
this particular callback module.
""".
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
      HttpInformation :: [MandatoryOption]
                       | [CommunicationOption]
                       | [ModOption]
                       | [LimitOption]
                       | [AdminOption],
      MandatoryOption :: mandatory_option(),
      CommunicationOption :: communication_option(),
      ModOption :: mod_option(),
      LimitOption :: limit_option(),
      AdminOption :: admin_option().
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
      HttpInformation :: [MandatoryOption]
                       | [CommunicationOption]
                       | [ModOption]
                       | [LimitOption]
                       | [AdminOption],
      MandatoryOption :: mandatory_option(),
      CommunicationOption :: communication_option(),
      ModOption :: {modules, atom()},
      LimitOption :: limit_option(),
      AdminOption :: admin_option();
          (Address, Port) -> HttpInformation when
      Address :: inet:ip_address(),
      Port :: non_neg_integer(),
      HttpInformation :: [MandatoryOption]
                       | [CommunicationOption]
                       | [ModOption]
                       | [LimitOption]
                       | [AdminOption],
      MandatoryOption :: mandatory_option(),
      CommunicationOption :: communication_option(),
      ModOption :: mod_option(),
      LimitOption :: limit_option(),
      AdminOption :: admin_option().
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
      HttpInformation :: [MandatoryOption]
                       | [CommunicationOption]
                       | [ModOption]
                       | [LimitOption]
                       | [AdminOption],
      MandatoryOption :: mandatory_option(),
      CommunicationOption :: communication_option(),
      ModOption :: mod_option(),
      LimitOption :: limit_option(),
      AdminOption :: admin_option();
          (Address, Port, Properties) -> HttpInformation when
      Address :: inet:ip_address() | any,
      Port    :: integer(),
      Properties :: [atom()],
      HttpInformation :: [MandatoryOption]
                       | [CommunicationOption]
                       | [ModOption]
                       | [LimitOption]
                       | [AdminOption],
      MandatoryOption :: mandatory_option(),
      CommunicationOption :: communication_option(),
      ModOption :: mod_option(),
      LimitOption :: limit_option(),
      AdminOption :: admin_option().
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
      HttpInformation :: [MandatoryOption]
                       | [CommunicationOption]
                       | [ModOption]
                       | [LimitOption]
                       | [AdminOption],
      MandatoryOption :: mandatory_option(),
      CommunicationOption :: communication_option(),
      ModOption :: mod_option(),
      LimitOption :: limit_option(),
      AdminOption :: admin_option().
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
                    Result = reload(Config, Address, Port, Profile),
                    unblock(Address, Port, Profile),
                    Result;
                Error ->
                    Error
            end;
	Error ->
	    Error
    end.

%%%--------------------------------------------------------------
%%% Deprecated 
%%%--------------------------------------------------------------
