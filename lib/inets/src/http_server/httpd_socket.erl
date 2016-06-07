%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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

%% API  (document close ?)
-export([deliver/3,  peername/2, resolve/0,  close/2]).

-include("httpd.hrl").

-define(VMODULE,"SOCKET").
-include_lib("kernel/include/inet.hrl").

deliver(SocketType, Socket, IOListOrBinary)  ->
    case http_transport:send(SocketType, Socket, IOListOrBinary) of
	{error, _Reason} ->
	    (catch close(SocketType, Socket)), 
	    socket_closed;
	_ ->
	    ok
    end.

peername(SocketType, Socket) ->
    http_transport:peername(SocketType, Socket).

resolve() ->
   http_transport:resolve().

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
