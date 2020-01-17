%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2019. All Rights Reserved.
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
-module(dtls_api_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

all() ->
    [
     {group, 'dtlsv1.2'},
     {group, 'dtlsv1'}
    ].

groups() ->
    [
     {'dtlsv1.2', [],  api_tests()},
     {'dtlsv1', [],  api_tests()}
    ].

api_tests() ->
    [
     dtls_listen_owner_dies,
     dtls_listen_reopen
    ].

init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
	    ssl_test_lib:clean_start(),
	    ssl_test_lib:make_rsa_cert(Config0)
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:unload(ssl),
    application:stop(crypto).


init_per_group(GroupName, Config) ->
    case ssl_test_lib:is_tls_version(GroupName) of
	true ->
	    case ssl_test_lib:sufficient_crypto_support(GroupName) of
		true ->
		    ssl_test_lib:init_tls_version(GroupName, Config);
		false ->
		    {skip, "Missing crypto support"}
	    end;
	_ ->
	    ssl:start(),
	    Config
    end.

end_per_group(GroupName, Config) ->
    case ssl_test_lib:is_tls_version(GroupName) of
        true ->
            ssl_test_lib:clean_tls_version(Config);
        false ->
            Config
    end.

init_per_testcase(_TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 10}),
    Config.

end_per_testcase(_TestCase, Config) ->     
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

dtls_listen_owner_dies() ->
    [{doc, "Test that you can start new DTLS 'listner' if old owner dies"}].

dtls_listen_owner_dies(Config) when is_list(Config) ->    
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Port = ssl_test_lib:inet_port(ServerNode),
    Test = self(),
    Pid = spawn(fun() -> {ok, _} =
                             ssl:listen(Port, [{protocol, dtls} | ServerOpts]),
                         {error, _} = ssl:listen(Port, [{protocol, dtls} | ServerOpts]),
                         Test ! {self(), listened}
                end),
    receive
        {Pid, listened} ->
            ok
    end,
    {ok, LSocket} = ssl:listen(Port, [{protocol, dtls} | ServerOpts]),
    spawn(fun() -> 
                  {ok, ASocket} = ssl:transport_accept(LSocket),
                  {ok, Socket} = ssl:handshake(ASocket),
                   receive 
                       {ssl, Socket, "from client"} ->
                           ssl:send(Socket, "from server"),
                           ssl:close(Socket)
                   end
          end),
    {ok, Client} = ssl:connect(Hostname, Port, ClientOpts),

    ssl:send(Client, "from client"),
    receive 
        {ssl, Client, "from server"} ->
            ssl:close(Client)
    end.

dtls_listen_reopen() ->
    [{doc, "Test that you close a DTLS 'listner' socket and open a new one for the same port"}].

dtls_listen_reopen(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Port = ssl_test_lib:inet_port(ServerNode),
    {ok, LSocket0} = ssl:listen(Port, [{protocol, dtls} | ServerOpts]),
     spawn(fun() ->
                  {ok, ASocket} = ssl:transport_accept(LSocket0),
                   {ok, Socket} = ssl:handshake(ASocket),
                   receive
                       {ssl, Socket, "from client"} ->
                           ssl:send(Socket, "from server 1"),
                           ssl:close(Socket)
                   end
           end),
    {ok, Client1} = ssl:connect(Hostname, Port, ClientOpts),
    ok = ssl:close(LSocket0),
    {ok, LSocket1} = ssl:listen(Port, [{protocol, dtls} | ServerOpts]),
    spawn(fun() ->
                  {ok, ASocket} = ssl:transport_accept(LSocket1),
                  {ok, Socket} = ssl:handshake(ASocket),
                  receive
                      {ssl, Socket, "from client"} ->
                          ssl:send(Socket, "from server 2"),
                          ssl:close(Socket)
                   end
          end),
    {ok, Client2} = ssl:connect(Hostname, Port, [{protocol, dtls} | ClientOpts]),
    ssl:send(Client2, "from client"),
    ssl:send(Client1, "from client"),
    receive
        {ssl, Client1, "from server 1"} ->
            ssl:close(Client1)
    end,
    receive
        {ssl, Client2, "from server 2"} ->
            ssl:close(Client2)
    end.
%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------

