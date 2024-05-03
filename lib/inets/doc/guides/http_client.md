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
# HTTP Client

## Configuration

The HTTP client default profile is started when the `Inets` application is
started and is then available to all processes on that Erlang node. Other
profiles can also be started at application startup, or profiles can be started
and stopped dynamically in runtime. Each client profile spawns a new process to
handle each request, unless a persistent connection can be used with or without
pipelining. The client adds a `host` header and an empty `te` header if there
are no such headers present in the request.

The client supports IPv6 as long as the underlying mechanisms also do so.

The following is to be put in the Erlang node application configuration file to
start a profile at application startup:

```erlang
      [{inets, [{services, [{httpc, PropertyList}]}]}]
```

For valid properties, see `m:httpc`.

## Getting Started

Start `Inets`:

```erlang
 1 > inets:start().
      ok
```

The following calls use the default client profile. Use the proxy
`"www-proxy.mycompany.com:8000"`, except from requests to localhost. This
applies to all the following requests.

Example:

```erlang
      2 > httpc:set_options([{proxy, {{"www-proxy.mycompany.com", 8000},
      ["localhost"]}}]).
      ok
```

The following is an ordinary synchronous request:

```erlang
      3 > {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} =
      httpc:request(get, {"http://www.erlang.org", []}, [], []).
```

With all the default values presented, a get request can also be written as
follows:

```erlang
      4 > {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} =
      httpc:request("http://www.erlang.org").
```

The following is a https request and with verification of the host:

```erlang
      5 > {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} =
      httpc:request(get, {"https://www.erlang.org", []}, [{ssl, httpc:ssl_verify_host_options(true)}], []).
```

The following is an ordinary asynchronous request:

```erlang
      6 > {ok, RequestId} =
      httpc:request(get, {"http://www.erlang.org", []}, [], [{sync, false}]).
```

The result is sent to the calling process as `{http, {ReqestId, Result}}`.

In this case, the calling process is the shell, so the following result is
received:

```erlang
      7 > receive {http, {RequestId, Result}} -> ok after 500 -> error end.
      ok
```

This sends a request with a specified connection header:

```erlang
      8 > {ok, {{NewVersion, 200, NewReasonPhrase}, NewHeaders, NewBody}} =
      httpc:request(get, {"http://www.erlang.org", [{"connection", "close"}]},
      [], []).
```

This sends an HTTP request over a unix domain socket (experimental):

```erlang
      9 > httpc:set_options([{ipfamily, local},
      {unix_socket,"/tmp/unix_socket/consul_http.sock"}]).
      10 > {ok, {{NewVersion, 200, NewReasonPhrase}, NewHeaders, NewBody}} =
      httpc:request(put, {"http:///v1/kv/foo", [], [], "hello"}, [], []).
```

Start an HTTP client profile:

```erlang
      10 > {ok, Pid} = inets:start(httpc, [{profile, foo}]).
      {ok, <0.45.0>}
```

The new profile has no proxy settings, so the connection is refused:

```erlang
      11 > httpc:request("http://www.erlang.org", foo).
      {error, econnrefused}
```

Stop the HTTP client profile:

```erlang
      12 > inets:stop(httpc, foo).
      ok
```

Alternative way to stop the HTTP client profile:

```erlang
      13 > inets:stop(httpc, Pid).
      ok
```
