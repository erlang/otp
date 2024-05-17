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
# HTTP server

## Configuration

[](){: #config }

The HTTP server, also referred to as httpd, handles HTTP requests as described
in [RFC 2616](http://www.ietf.org/rfc/rfc2616.txt) with a few exceptions, such
as gateway and proxy functionality. The server supports IPv6 as long as the
underlying mechanisms also do so.

The server implements numerous features, such as:

- Secure Sockets Layer (SSL)
- Erlang Scripting Interface (ESI)
- Common Gateway Interface (CGI)
- User Authentication (using Mnesia, Dets or plain text database)
- Common Logfile Format (with or without disk_log(3) support)
- URL Aliasing
- Action Mappings
- Directory Listings

The configuration of the server is provided as an Erlang property list.

As of `Inets` 5\.0 the HTTP server is an easy to start/stop and customize web
server providing the most basic web server functionality. Inets is designed for
embedded systems and if you want a full-fledged web server there are other
erlang open source alternatives.

Almost all server functionality has been implemented using an especially crafted
server API, which is described in the Erlang Web Server API. This API can be
used to enhance the core server functionality, for example with custom logging
and authentication.

The following is to be put in the Erlang node application configuration file to
start an HTTP server at application startup:

```erlang
[{inets, [{services, [{httpd, [{proplist_file,
           "/var/tmp/server_root/conf/8888_props.conf"}]},
          {httpd, [{proplist_file,
           "/var/tmp/server_root/conf/8080_props.conf"}]}]}]}].
```

The server is configured using an Erlang property list. For the available
properties, see `m:httpd`.

The available configuration properties are as follows:

```erlang
httpd_service() -> {httpd, httpd()}
httpd()         -> [httpd_config()]
httpd_config()  -> {proplist_file, file()}
                   {debug, debug()} |
                   {accept_timeout, integer()}
debug()         -> disable | [debug_options()]
debug_options() -> {all_functions, modules()} |
                   {exported_functions, modules()} |
                   {disable, modules()}
modules()       -> [atom()]
```

Here:

- **`{proplist_file, file()}`** - File containing an Erlang property list,
  followed by a full stop, describing the HTTP server configuration.

- **`{debug, debug()}`** - Can enable trace on all functions or only exported
  functions on chosen modules.

- **`{accept_timeout, integer()}`** - Sets the wanted time-out value for the
  server to set up a request connection.

## Getting Started

[](){: #using_http_server_api }

Start `Inets`:

```erlang
1> inets:start().
ok
```

Start an HTTP server with minimal required configuration. If you specify port
`0`, an arbitrary available port is used, and you can use function `info` to
find which port number that was picked:

```erlang
2> {ok, Pid} = inets:start(httpd, [{port, 0}, {server_root,"/tmp"},
.. {document_root,"/tmp/htdocs"}, {bind_address, "localhost"}]).
{ok, 0.79.0}
```

Call `info`:

```erlang
3> httpd:info(Pid).
[{mime_types,[{"html","text/html"},{"htm","text/html"}]},
 {server_name,"machine.local"},
 {bind_address, {127,0,0,1}},
 {server_root,"/tmp"},
 {port,59408},
 {document_root,"/tmp/htdocs"},
 {ipfamily,inet}]
```

Reload the configuration without restarting the server:

```erlang
4> httpd:reload_config([{port, 59408},
.. {server_root,"/tmp/www_test"}, {document_root,"/tmp/www_test/htdocs"},
.. {bind_address, "localhost"}], non_disturbing).
ok.
```

> #### Note {: .info }
>
> `port` and `bind_address` cannot be changed. Clients trying to access the
> server during the reload get a service temporary unavailable answer.

```erlang
5> httpd:info(Pid, [server_root, document_root]).
[{server_root,"/tmp/www_test"},{document_root,"/tmp/www_test/htdocs"}]
```

```erlang
6> ok = inets:stop(httpd, Pid).
```

Alternative:

```erlang
6> ok = inets:stop(httpd, {{127,0,0,1}, 59408}).
```

Notice that `bind_address` must be the IP address reported by function `info`
and cannot be the hostname that is allowed when putting in `bind_address`.

## Dynamic Web Pages

[](){: #dynamic_we_pages }

`Inets` HTTP server provides two ways of creating dynamic web pages, each with
its own advantages and disadvantages:

- **_CGI scripts_** - Common Gateway Interface (CGI) scripts can be written in
  any programming language. CGI scripts are standardized and supported by most
  web servers. The drawback with CGI scripts is that they are resource-intensive
  because of their design. CGI requires the server to fork a new OS process for
  each executable it needs to start.

- **_ESI-functions_** - Erlang Server Interface (ESI) functions provide a tight
  and efficient interface to the execution of Erlang functions. This interface,
  on the other hand, is `Inets` specific.

### CGI Version 1.1, RFC 3875

The module `mod_cgi` enables execution of
[CGI scripts](http://www.ietf.org/rfc/rfc3875.txt) on the server. A file
matching the definition of a ScriptAlias config directive is treated as a CGI
script. A CGI script is executed by the server and its output is returned to the
client.

The CGI script response comprises a message header and a message body, separated
by a blank line. The message header contains one or more header fields. The body
can be empty.

Example:

```text
"Content-Type:text/plain\nAccept-Ranges:none\n\nsome very
	plain text"
```

The server interprets the message headers and most of them are transformed into
HTTP headers and sent back to the client together with the message-body.

Support for CGI-1.1 is implemented in accordance with
[RFC 3875](http://www.ietf.org/rfc/rfc3875.txt).

### ESI

The Erlang server interface is implemented by module `mod_esi`.

#### ERL Scheme

The erl scheme is designed to mimic plain CGI, but without the extra overhead.
An URL that calls an Erlang `erl` function has the following syntax (regular
expression):

```text
http://your.server.org/***/Module[:/]Function(?QueryString|/PathInfo)
```

\*\*\* depends on how the ErlScriptAlias config directive has been used.

The module `Module` referred to must be found in the code path, and it must
define a function `Function` with an arity of two or three. It is preferable to
implement a function with arity three, as it permits to send chunks of the web
page to the client during the generation phase instead of first generating the
whole web page and then sending it to the client. The option to implement a
function with arity two is only kept for backwards compatibility reasons. For
implementation details of the ESI callback function, see `m:mod_esi`.

## Logging

Three types of logs are supported: transfer logs, security logs, and error logs.
The de-facto standard Common Logfile Format is used for the transfer and
security logging. There are numerous statistics programs available to analyze
Common Logfile Format. The Common Logfile Format looks as follows:

_remotehost rfc931 authuser \[date] "request" status bytes_

Here:

- **_remotehost_** - Remote hostname.

- **_rfc931_** - The client remote username
  ([RFC 931](http://www.ietf.org/rfc/rfc931.txt)).

- **_authuser_** - The username used for authentication.

- **_\[date]_** - Date and time of the request
  ([RFC 1123](http://www.ietf.org/rfc/rfc1123.txt)).

- **_"request"_** - The request line exactly as it came from the client
  ([RFC 1945](http://www.ietf.org/rfc/rfc1945.txt)).

- **_status_** - The HTTP status code returned to the client
  ([RFC 1945](http://www.ietf.org/rfc/rfc1945.txt)).

- **_bytes_** - The content-length of the document transferred.

Internal server errors are recorded in the error log file. The format of this
file is a more unplanned format than the logs using Common Logfile Format, but
conforms to the following syntax:

_\[date]_ access to _path_ failed for _remotehost_, reason: _reason_

## Erlang Web Server API

The process of handling an HTTP request involves several steps, such as:

- Setting up connections, sending and receiving data.
- URI to filename translation.
- Authentication/access checks.
- Retrieving/generating the response.
- Logging.

To provide customization and extensibility of the request handling of the HTTP
servers, most of these steps are handled by one or more modules. These modules
can be replaced or removed at runtime and new ones can be added. For each
request, all modules are traversed in the order specified by the module
directive in the server configuration file. Some parts, mainly the
communication- related steps, are considered server core functionality and are
not implemented using the Erlang web server API. A description of functionality
implemented by the Erlang webserver API is described in
[Section Inets Web Server Modules](http_server.md#Inets_Web_Server_Modules).

A module can use data generated by previous modules in the Erlang webserver API
module sequence or generate data to be used by consecutive Erlang Web Server API
modules. This is possible owing to an internal list of key-value tuples,
referred to as interaction data.

> #### Note {: .info }
>
> Interaction data enforces module dependencies and is to be avoided if
> possible. This means that the order of modules in the modules property is
> significant.

### API Description

Each module that implements server functionality using the Erlang web server API
is to implement the following call back functions:

- `do/1` (mandatory) - the function called when a request is to be handled
- `load/2`
- `store/2`
- `remove/1`

The latter functions are needed only when new config directives are to be
introduced. For details, see `m:httpd`.

## Inets Web Server Modules

[](){: #Inets_Web_Server_Modules }

The convention is that all modules implementing some web server functionality
has the name `mod_*`. When configuring the web server, an appropriate selection
of these modules is to be present in the module directive. Notice that there are
some interaction dependencies to take into account, so the order of the modules
cannot be random.

### mod_action - Filetype/Method-Based Script Execution

This module runs CGI scripts whenever a file of a certain type or HTTP method
(see [RFC 1945](http://tools.ietf.org/html/rfc1945)) is requested.

Uses the following Erlang Web Server API interaction data:

- `real_name` \- from `m:mod_alias`.

Exports the following Erlang Web Server API interaction data, if possible:

- **`{new_request_uri, RequestURI}`** - An alternative `RequestURI` has been
  generated.

### mod_alias - URL Aliasing

The `m:mod_alias` module makes it possible to map different parts of the host
file system into the document tree, that is, creates aliases and redirections.

Exports the following Erlang Web Server API interaction data, if possible:

- **`{real_name, PathData}`** - `PathData` is the argument used for API function
  `mod_alias:path/3`.

### mod_auth - User Authentication

The `m:mod_auth` module provides for basic user authentication using textual
files, Dets databases as well as Mnesia databases.

Uses the following Erlang Web Server API interaction data:

- `real_name` \- from `m:mod_alias`

Exports the following Erlang Web Server API interaction data:

- **`{remote_user, User}`** - The username used for authentication.

#### Mnesia As Authentication Database

If Mnesia is used as storage method, Mnesia must be started before the HTTP
server. The first time Mnesia is started, the schema and the tables must be
created before Mnesia is started. A simple example of a module with two
functions that creates and start Mnesia is provided here. Function
`first_start/0` is to be used the first time. It creates the schema and the
tables. `start/0` is to be used in consecutive startups. `start/0` starts Mnesia
and waits for the tables to be initiated. This function must only be used when
the schema and the tables are already created.

```erlang
-module(mnesia_test).
-export([start/0,load_data/0]).
-include_lib("mod_auth.hrl").

first_start() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(httpd_user,
                        [{type, bag},
                         {disc_copies, [node()]},
                         {attributes, record_info(fields,
                                                  httpd_user)}]),
    mnesia:create_table(httpd_group,
                        [{type, bag},
                         {disc_copies, [node()]},
                         {attributes, record_info(fields,
                                                  httpd_group)}]),
    mnesia:wait_for_tables([httpd_user, httpd_group], 60000).

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([httpd_user, httpd_group], 60000).
```

To create the Mnesia tables, we use two records defined in `mod_auth.hrl`, so
that file must be included. `first_start/0` creates a schema that specifies on
which nodes the database is to reside. Then it starts Mnesia and creates the
tables. The first argument is the name of the tables, the second argument is a
list of options of how to create the table, see `m:mnesia`, documentation for
more information. As the implementation of the `mod_auth_mnesia` saves one row
for each user, the type must be `bag`. When the schema and the tables are
created, function `mnesia:start/0` is used to start Mnesia and waits for the
tables to be loaded. Mnesia uses the directory specified as `mnesia_dir` at
startup if specified, otherwise Mnesia uses the current directory. For security
reasons, ensure that the Mnesia tables are stored outside the document tree of
the HTTP server. If they are placed in the directory which it protects, clients
can download the tables. Only the Dets and Mnesia storage methods allow writing
of dynamic user data to disk. `plain` is a read only method.

### mod_cgi - CGI Scripts

This module handles invoking of CGI scripts.

### mod_dir - Directories

This module generates an HTML directory listing (Apache-style) if a client sends
a request for a directory instead of a file. This module must be removed from
the Modules config directive if directory listings is unwanted.

Uses the following Erlang Web Server API interaction data:

- `real_name` \- from `m:mod_alias`

Exports the following Erlang Web Server API interaction data:

- **`{mime_type, MimeType}`** - The file suffix of the incoming URL mapped into
  a `MimeType`.

### mod_disk_log - Logging Using Disk_Log.

Standard logging using the "Common Logfile Format" and `m:disk_log`.

Uses the following Erlang Web Server API interaction data:

- `remote_user` \- from `mod_auth`

### mod_esi - Erlang Server Interface

The `m:mod_esi` module implements the Erlang Server Interface (ESI) providing a
tight and efficient interface to the execution of Erlang functions.

Uses the following Erlang web server API interaction data:

- `remote_user` \- from `mod_auth`

Exports the following Erlang web server API interaction data:

- **`{mime_type, MimeType}`** - The file suffix of the incoming URL mapped into
  a `MimeType`

### mod_get - Regular GET Requests

This module is responsible for handling GET requests to regular files. GET
requests for parts of files is handled by `mod_range`.

Uses the following Erlang web server API interaction data:

- `real_name` \- from `m:mod_alias`

### mod_head - Regular HEAD Requests

This module is responsible for handling HEAD requests to regular files. HEAD
requests for dynamic content is handled by each module responsible for dynamic
content.

Uses the following Erlang Web Server API interaction data:

- `real_name` \- from `m:mod_alias`

### mod_log - Logging Using Text Files.

Standard logging using the "Common Logfile Format" and text files.

Uses the following Erlang Web Server API interaction data:

- `remote_user` \- from `mod_auth`

### mod_range - Requests with Range Headers

This module responses to requests for one or many ranges of a file. This is
especially useful when downloading large files, as a broken download can be
resumed.

Notice that request for multiple parts of a document report a size of zero to
the log file.

Uses the following Erlang Web Server API interaction data:

- `real_name` \- from `m:mod_alias`

### mod_response_control - Requests with If\* Headers

This module controls that the conditions in the requests are fulfilled. For
example, a request can specify that the answer only is of interest if the
content is unchanged since the last retrieval. If the content is changed, the
range request is to be converted to a request for the whole file instead.

If a client sends more than one of the header fields that restricts the servers
right to respond, the standard does not specify how this is to be handled.
`m:httpd` controls each field in the following order and if one of the fields
does not match the current state, the request is rejected with a proper
response:

`If-modified`

`If-Unmodified`

`If-Match`

`If-Nomatch`

Uses the following Erlang Web Server API interaction data:

- `real_name` \- from `m:mod_alias`

Exports the following Erlang Web Server API interaction data:

- **`{if_range, send_file}`** - The conditions for the range request are not
  fulfilled. The response must not be treated as a range request, instead it
  must be treated as an ordinary get request.

### mod_security - Security Filter

The `m:mod_security` module serves as a filter for authenticated requests
handled in `m:mod_auth`. It provides a possibility to restrict users from access
for a specified amount of time if they fail to authenticate several times. It
logs failed authentication as well as blocking of users, and it calls a
configurable callback module when the events occur.

There is also an API to block or unblock users manually. This API can also list
blocked users or users who have been authenticated within a configurable amount
of time.

### mod_trace - TRACE Request

`mod_trace` is responsible for handling of TRACE requests. Trace is a new
request method in HTTP/1.1. The intended use of trace requests is for testing.
The body of the trace response is the request message that the responding web
server or proxy received.

## Serving files from the command line

httpd includes functionality to quickly serve files from the command line. In
its simplest form, `erl -S httpd` will serve files in the local directory on
localhost.

- **`--port`** - Sets the port to bind on. Defaults to `8000`.

- **`--bind`** - Sets the bind address to listen on. Defaults to `127.0.0.1`.

- **_DIRECTORY_** - Sets the directory to serve data from. Defaults to the
  current directory.

For example, to serve files from directory `test_results` on port `4000`:

```text
erl -S httpd serve --port 4000 test_results
```

For a full reference of all options, run `erl -S httpd serve --help`.
