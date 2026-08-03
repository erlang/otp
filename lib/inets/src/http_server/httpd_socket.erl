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
-module(httpd_socket).
-moduledoc """
Communication utility functions to be used by the Erlang web server API
programmer.

This module provides the Erlang web server API module programmer with utility
functions for generic sockets communication. The appropriate communication
mechanism is transparently used, that is, `ip_comm` or `ssl`.

### See also

`m:httpd`
""".

%% API  (document close ?)
-export([deliver/3,  peername/2, resolve/0,  close/2]).

-include("httpd.hrl").

-define(VMODULE,"SOCKET").
-include_lib("kernel/include/inet.hrl").

-doc """
`deliver/3` sends `Data` over `Socket` using the specified `SocketType`.
`Socket` and `SocketType` is to be the socket and the `socket_type` form the
`mod` record as defined in `httpd.hrl`
""".
-spec deliver(SocketType, Socket, Data) -> Result when
      SocketType :: httpd:socket_type(),
      Socket :: inet:socket(),
      Data :: iolist() | binary(),
      Result :: ok | socket_closed.
deliver(SocketType, Socket, IOListOrBinary)  ->
    case http_transport:send(SocketType, Socket, IOListOrBinary) of
	{error, _Reason} ->
	    (catch close(SocketType, Socket)), 
	    socket_closed;
	_ ->
	    ok
    end.

-doc """
`peername/2` returns the `Port` and `IPAddress` of the remote `Socket`.
""".
-spec peername(SocketType, Socket) -> {Port, IpAdress} when
      SocketType :: httpd:socket_type(),
      Socket :: inet:socket() | ssl:sslsocket(),
      Port :: inet:port_number(),
      IpAdress :: inet:ip4_address() | inet:ip6_address() | string().
peername(SocketType, Socket) ->
    http_transport:peername(SocketType, Socket).

-doc """
`resolve/0` returns the official `HostName` of the current host.
""".
-spec resolve() -> HostName when
      HostName :: inet:hostname().
resolve() ->
   http_transport:resolve().

-doc false.
close(SocketType, Socket) ->
    close_sleep(SocketType, 1000),
    Res = 
	case (catch http_transport:close(SocketType, Socket)) of
	    ok ->                  ok;
	    {error,Reason} ->      {error,Reason};
	    {'EXIT',{noproc,_}} -> {error,closed};
	    {'EXIT',Reason} ->     {error,Reason};
	    Otherwise ->           {error,Otherwise}
	end,
    Res.

%% Workaround for ssl problem when ssl does not deliver the message
%% sent prior to the close before the close signal.
close_sleep({ssl, _}, Time) ->
    sleep(Time);
close_sleep(_, _) -> 
    ok.

sleep(T) -> receive after T -> ok end.
