%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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

%%% Purpose: Example of SSL client and server using example certificates.

-module(client_server).

-export([start/0, start/1, init_connect/1]).

start() ->
    start([ssl, subject]).

start(CertOpts) ->
    %% Start ssl application
    application:start(ssl),

    %% Always seed 
    ssl:seed("ellynatefttidppohjeh"),

    %% Let the current process be the server that listens and accepts
    %% Listen
    {ok, LSock} = ssl:listen(0, mk_opts(listen)),
    {ok, {_, LPort}} = ssl:sockname(LSock),
    io:fwrite("Listen: port = ~w.~n", [LPort]),

    %% Spawn the client process that connects to the server
    spawn(?MODULE, init_connect, [{LPort, CertOpts}]),

    %% Accept
    {ok, ASock} = ssl:transport_accept(LSock),
    ok = ssl:ssl_accept(ASock),
    io:fwrite("Accept: accepted.~n"),
    {ok, Cert} = ssl:peercert(ASock, CertOpts),
    io:fwrite("Accept: peer cert:~n~p~n", [Cert]),
    io:fwrite("Accept: sending \"hello\".~n"),
    ssl:send(ASock, "hello"),
    {error, closed} = ssl:recv(ASock, 0),
    io:fwrite("Accept: detected closed.~n"),
    ssl:close(ASock),
    io:fwrite("Listen: closing and terminating.~n"),
    ssl:close(LSock),
    application:stop(ssl).


%% Client connect
init_connect({LPort, CertOpts}) ->
    {ok, Host} = inet:gethostname(), 
    {ok, CSock} = ssl:connect(Host, LPort, mk_opts(connect)),
    io:fwrite("Connect: connected.~n"),
    {ok, Cert} = ssl:peercert(CSock, CertOpts),
    io:fwrite("Connect: peer cert:~n~p~n", [Cert]),
    {ok, Data} = ssl:recv(CSock, 0),
    io:fwrite("Connect: got data: ~p~n", [Data]),
    io:fwrite("Connect: closing and terminating.~n"),
    ssl:close(CSock).

mk_opts(listen) ->
    mk_opts("server");
mk_opts(connect) ->
    mk_opts("client");
mk_opts(Role) ->
    Dir = filename:join([code:lib_dir(ssl), "examples", "certs", "etc"]),
    [{active, false}, 
     {verify, 2},
     {depth, 2},
     {cacertfile, filename:join([Dir, Role, "cacerts.pem"])}, 
     {certfile, filename:join([Dir, Role, "cert.pem"])}, 
     {keyfile, filename:join([Dir, Role, "key.pem"])}].

