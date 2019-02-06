%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2018. All Rights Reserved.
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

%%% Purpose: Example of SSL client and server using example certificates.

-module(client_server).

-export([start/0, init_connect/1]).

start() ->
    %% Start ssl application
    {ok, StartedApps} = application:ensure_all_started(ssl),

    %% Let the current process be the server that listens and accepts
    %% Listen
    {ok, LSock} = ssl:listen(0, mk_opts(listen)),
    {ok, {_, LPort}} = ssl:sockname(LSock),
    io:fwrite("Listen: port = ~w.~n", [LPort]),

    %% Spawn the client process that connects to the server
    spawn(?MODULE, init_connect, [LPort]),

    %% Accept
    {ok, ASock} = ssl:transport_accept(LSock),
    {ok, SslSocket} = ssl:handshake(ASock),
    io:fwrite("Accept: accepted.~n"),
    {ok, Cert} = ssl:peercert(SslSocket),
    io:fwrite("Accept: peer cert:~n~p~n", [public_key:pkix_decode_cert(Cert, otp)]),
    io:fwrite("Accept: sending \"hello\".~n"),
    ssl:send(SslSocket, "hello"),
    {error, closed} = ssl:recv(SslSocket, 0),
    io:fwrite("Accept: detected closed.~n"),
    ssl:close(SslSocket),
    io:fwrite("Listen: closing and terminating.~n"),
    ssl:close(LSock),

    lists:foreach(fun application:stop/1, lists:reverse(StartedApps)).


%% Client connect
init_connect(LPort) ->
    {ok, Host} = inet:gethostname(), 
    {ok, CSock} = ssl:connect(Host, LPort, mk_opts(connect)),
    io:fwrite("Connect: connected.~n"),
    {ok, Cert} = ssl:peercert(CSock),
    io:fwrite("Connect: peer cert:~n~p~n", [public_key:pkix_decode_cert(Cert, otp)]),
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
     {server_name_indication, disable},
     {cacertfile, filename:join([Dir, Role, "cacerts.pem"])}, 
     {certfile, filename:join([Dir, Role, "cert.pem"])}, 
     {keyfile, filename:join([Dir, Role, "key.pem"])}].
