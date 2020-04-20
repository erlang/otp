%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2018-2019. All Rights Reserved.
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

-module(socket_test_ttest_tcp_gen).

-export([
	 accept/1, accept/2,
	 active/2,
	 close/1,
	 connect/2, connect/3,
	 controlling_process/2,
	 listen/0, listen/1, listen/2,
	 peername/1,
	 port/1,
	 recv/2, recv/3,
	 send/2,
	 shutdown/2,
	 sockname/1
	]).


-define(LIB, socket_test_lib).

%% ==========================================================================

accept(Sock) ->
    case gen_tcp:accept(Sock) of
	{ok, NewSock} ->
	    {ok, NewSock};
	{error, _} = ERROR ->
	    ERROR
    end.

accept(Sock, Timeout) ->
    case gen_tcp:accept(Sock, Timeout) of
	{ok, NewSock} ->
	    {ok, NewSock};
	{error, _} = ERROR ->
	    ERROR
    end.


active(Sock, NewActive) 
  when (is_boolean(NewActive) orelse (NewActive =:= once)) ->
    inet:setopts(Sock, [{active, NewActive}]).


close(Sock) ->
    gen_tcp:close(Sock).


connect(Addr, Port) ->
    Opts = [binary, {packet, raw}, {active, false}, {buffer, 32*1024}], 
    do_connect(Addr, Port, Opts).

connect(Addr, Port, #{domain := Domain}) ->
    Opts = [Domain, binary, {packet, raw}, {active, false}, {buffer, 32*1024}], 
    do_connect(Addr, Port, Opts).

do_connect(Addr, Port, Opts) ->
    case gen_tcp:connect(Addr, Port, Opts) of
	{ok, Sock} ->
	    {ok, Sock};
	{error, _} = ERROR ->
	    ERROR
    end.

controlling_process(Sock, NewPid) ->
    gen_tcp:controlling_process(Sock, NewPid).


%% Create a listen socket
listen() ->
    listen(0).

listen(Port) ->
    listen(Port, #{domain => inet}).

listen(Port, #{domain := Domain}) when is_integer(Port) andalso (Port >= 0) ->
    case ?LIB:which_local_host_info(Domain) of
	{ok, #{addr := Addr}} ->
	    Opts = [Domain,
		    binary, {ip, Addr}, {packet, raw}, {active, false},
		    {buffer, 32*1024}],
	    gen_tcp:listen(Port, Opts);
	{error, _} = ERROR ->
	    ERROR
    end.


peername(Sock) ->
    inet:peername(Sock).


port(Sock) ->
    inet:port(Sock).


recv(Sock, Length) ->
    gen_tcp:recv(Sock, Length).
recv(Sock, Length, Timeout) ->
    gen_tcp:recv(Sock, Length, Timeout).


send(Sock, Data) ->
    gen_tcp:send(Sock, Data).


shutdown(Sock, How) ->
    gen_tcp:shutdown(Sock, How).


sockname(Sock) ->
    inet:sockname(Sock).


%% ==========================================================================


			   
