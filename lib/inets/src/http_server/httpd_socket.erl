%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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
