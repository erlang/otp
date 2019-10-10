%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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

-module(ssl_payload_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(TIMEOUT, 600000).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() -> 
    [
     {group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'sslv3'}
    ].

groups() ->
    [
     {'tlsv1.2', [], payload_tests()},
     {'tlsv1.1', [], payload_tests()},
     {'tlsv1', [], payload_tests()},
     {'sslv3', [], payload_tests()}
    ].

payload_tests() ->
    [server_echos_passive_small,
     server_echos_passive_chunk_small,
     server_echos_active_once_small,
     server_echos_active_small,
     client_echos_passive_small,
     client_echos_passive_chunk_small,
     client_echos_active_once_small,
     client_echos_active_small,
     server_echos_passive_big,
     server_echos_passive_chunk_big,
     server_echos_active_once_big,
     server_echos_active_big,
     client_echos_passive_big,
     client_echos_passive_chunk_big,
     client_echos_active_once_big,
     client_echos_active_big,
     server_echos_passive_huge,
     server_echos_passive_chunk_huge,
     server_echos_active_once_huge,
     server_echos_active_huge,
     client_echos_passive_huge,
     client_echos_passive_chunk_huge,
     client_echos_active_once_huge,
     client_echos_active_huge,
     client_active_once_server_close].

init_per_suite(Config) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
	    ssl_test_lib:clean_start(),
	    {ok, _} =
                make_certs:all(
                  proplists:get_value(data_dir, Config),
                  proplists:get_value(priv_dir, Config)),
	    ssl_test_lib:cert_options(Config)
    catch _:_  ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
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

init_per_testcase(TestCase, Config)
  when TestCase == server_echos_passive_huge;
       TestCase == server_echos_passive_chunk_huge;
       TestCase == server_echos_active_once_huge;
       TestCase == server_echos_active_huge;
       TestCase == client_echos_passive_huge;
       TestCase == client_echos_passive_chunk_huge;
       TestCase == client_echos_active_once_huge;
       TestCase == client_echos_active_huge ->
    case erlang:system_info(system_architecture) of
	"sparc-sun-solaris2.10" ->
	    {skip,"Will take to long time on an old Sparc"};
	_ ->
	    ct:timetrap({seconds, 90}),
	    Config
    end;

init_per_testcase(TestCase, Config)
  when TestCase == server_echos_passive_big;
       TestCase == server_echos_passive_chunk_big;
       TestCase == server_echos_active_once_big;
       TestCase == server_echos_active_big;
       TestCase == client_echos_passive_big;
       TestCase == client_echos_passive_chunk_big;
       TestCase == client_echos_active_once_big;
       TestCase == client_echos_active_big ->
    ct:timetrap({seconds, 60}),
    Config;

init_per_testcase(_TestCase, Config) ->
    ct:timetrap({seconds, 15}),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.
%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

server_echos_passive_small() ->
    [{doc, "Client sends 1000 bytes in passive mode to server, that receives them, "
     "sends them back, and closes."}].

server_echos_passive_small(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 100),
    server_echos_passive(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).

%%--------------------------------------------------------------------

server_echos_passive_chunk_small() ->
    [{doc, "Client sends 1000 bytes in passive mode to server, that receives them in chunks, "
     "sends them back, and closes."}].

server_echos_passive_chunk_small(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 100),
    server_echos_passive_chunk(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).


%%--------------------------------------------------------------------

server_echos_active_once_small() ->
    [{doc, "Client sends 1000 bytes in active once mode to server, that receives "
     " them, sends them back, and closes."}].

server_echos_active_once_small(Config) when is_list(Config) -> 
        ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 100),
    server_echos_active_once(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).

%%--------------------------------------------------------------------

server_echos_active_small() ->
    [{doc, "Client sends 1000 bytes in active mode to server, that receives them, "
     "sends them back, and closes."}].

server_echos_active_small(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 100),
    server_echos_active(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).

%%--------------------------------------------------------------------
client_echos_passive_small() ->
    [{doc, "Server sends 1000 bytes in passive mode to client, that receives them, "
      "sends them back, and closes."}].

client_echos_passive_small(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 100),
    client_echos_passive(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).

%%--------------------------------------------------------------------
client_echos_passive_chunk__small() ->
    [{doc, "Server sends 1000 bytes in passive mode to client, that receives them in chunks, "
      "sends them back, and closes."}].

client_echos_passive_chunk_small(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 100),
    client_echos_passive_chunk(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).


%%--------------------------------------------------------------------
client_echos_active_once_small() ->
    ["Server sends 1000 bytes in active once mode to client, that receives "
     "them, sends them back, and closes."].

client_echos_active_once_small(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 100),
    client_echos_active_once(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).
   
%%--------------------------------------------------------------------
client_echos_active_small() ->
    [{doc, "Server sends 1000 bytes in active mode to client, that receives them, "
      "sends them back, and closes."}].

client_echos_active_small(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 100),
    client_echos_active(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).


%%--------------------------------------------------------------------
server_echos_passive_big() ->
    [{doc, "Client sends 50000 bytes to server in passive mode, that receives them, "
     "sends them back, and closes."}].

server_echos_passive_big(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 5000),
    server_echos_passive(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).
%%--------------------------------------------------------------------
server_echos_passive_chunk_big() ->
    [{doc, "Client sends 50000 bytes to server in passive mode, that receives them, "
     "sends them back, and closes."}].

server_echos_passive_chunk_big(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 5000),
    server_echos_passive_chunk(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).

%%--------------------------------------------------------------------

server_echos_active_once_big() ->
    [{doc,"Client sends 50000 bytes to server in active once mode, that receives "
      "them, sends them back, and closes."}].

server_echos_active_once_big(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 5000),
    server_echos_active_once(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).

%%--------------------------------------------------------------------

server_echos_active_big() ->
    [{doc, "Client sends 50000 bytes to server in active once mode, that receives "
      " them, sends them back, and closes."}].

server_echos_active_big(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 5000),
    server_echos_active(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).

%%--------------------------------------------------------------------
client_echos_passive_big() ->
    [{doc, "Server sends 50000 bytes to client in passive mode, that receives them, "
     "sends them back, and closes."}].

client_echos_passive_big(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 5000),
    client_echos_passive(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).


%%--------------------------------------------------------------------
client_echos_passive_chunk_big() ->
    [{doc, "Server sends 50000 bytes to client in passive mode, that receives them, "
     "sends them back, and closes."}].

client_echos_passive_chunk_big(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 5000),
    client_echos_passive_chunk(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).


%%--------------------------------------------------------------------
client_echos_active_once_big() ->
    [{doc, "Server sends 50000 bytes to client in active once mode, that receives"
      " them, sends them back, and closes."}].

client_echos_active_once_big(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 5000),
    client_echos_active_once(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).
   
%%--------------------------------------------------------------------
client_echos_active_big() ->
    [{doc, "Server sends 50000 bytes to client in active mode, that receives them, "
      "sends them back, and closes."}].

client_echos_active_big(Config) when is_list(Config) -> 
     ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 5000),
    client_echos_active(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).

%%--------------------------------------------------------------------
server_echos_passive_huge() ->
    [{doc, "Client sends 500000 bytes to server in passive mode, that receives "
      " them, sends them back, and closes."}].

server_echos_passive_huge(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 50000),
    server_echos_passive(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).

%%--------------------------------------------------------------------
server_echos_passive_chunk_huge() ->
    [{doc, "Client sends 500000 bytes to server in passive mode, that receives "
      " them, sends them back, and closes."}].

server_echos_passive_chunk_huge(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 50000),
    server_echos_passive_chunk(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).

%%--------------------------------------------------------------------
server_echos_active_once_huge() ->
    [{doc, "Client sends 500000 bytes to server in active once mode, that receives "
      "them, sends them back, and closes."}].

server_echos_active_once_huge(Config) when is_list(Config) -> 
        ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 50000),
    server_echos_active_once(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).

%%--------------------------------------------------------------------
server_echos_active_huge() ->
    [{doc, "Client sends 500000 bytes to server in active mode, that receives them, "
     "sends them back, and closes."}].

server_echos_active_huge(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 50000),
    server_echos_active(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).

%%--------------------------------------------------------------------
client_echos_passive_huge() ->
    [{doc, "Server sends 500000 bytes to client in passive mode, that receives "
     "them, sends them back, and closes."}].

client_echos_passive_huge(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 50000),
    client_echos_passive(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).
%%--------------------------------------------------------------------
client_echos_passive_chunk_huge() ->
    [{doc, "Server sends 500000 bytes to client in passive mode, that receives "
     "them, sends them back, and closes."}].

client_echos_passive_chunk_huge(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 50000),
    client_echos_passive_chunk(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).
%%--------------------------------------------------------------------
client_echos_active_once_huge() ->
    [{doc, "Server sends 500000 bytes to client in active once mode, that receives "
      "them, sends them back, and closes."}].

client_echos_active_once_huge(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 50000),
    client_echos_active_once(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).
   
%%--------------------------------------------------------------------
client_echos_active_huge() ->
    [{doc, "Server sends 500000 bytes to client in active mode, that receives them, "
     "sends them back, and closes."}].

client_echos_active_huge(Config) when is_list(Config) -> 
     ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 50000),
    client_echos_active(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).
 

%%--------------------------------------------------------------------
client_active_once_server_close() ->
    [{doc, "Server sends 500000 bytes and immediately after closes the connection"
     "Make sure client recives all data if possible"}].

client_active_once_server_close(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    %%
    Data = binary:copy(<<"1234567890">>, 50000),
    client_active_once_server_close(
      Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname).
 


%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------

server_echos_passive(
  Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname) ->
    Length = byte_size(Data),
    Server =
        ssl_test_lib:start_server(
          [{node, ServerNode}, {port, 0},
           {from, self()},
           {mfa, {?MODULE, echoer, [Length]}},
           {options, [{active, false}, {mode, binary} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
        ssl_test_lib:start_client(
          [{node, ClientNode}, {port, Port},
           {host, Hostname},
           {from, self()},
           {mfa, {?MODULE, sender, [Data]}},
           {options, [{active, false}, {mode, binary} | ClientOpts]}]),
    %%
    ssl_test_lib:check_result(Server, ok, Client, ok),
    %%
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

server_echos_passive_chunk(
  Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname) ->
    Length = byte_size(Data),
    Server =
        ssl_test_lib:start_server(
          [{node, ServerNode}, {port, 0},
           {from, self()},
           {mfa, {?MODULE, echoer_chunk, [Length]}},
           {options, [{active, false}, {mode, binary} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
        ssl_test_lib:start_client(
          [{node, ClientNode}, {port, Port},
           {host, Hostname},
           {from, self()},
           {mfa, {?MODULE, sender, [Data]}},
           {options, [{active, false}, {mode, binary} | ClientOpts]}]),
    %%
    ssl_test_lib:check_result(Server, ok, Client, ok),
    %%
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

server_echos_active_once(
  Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname) ->
    Length = byte_size(Data),
    Server =
        ssl_test_lib:start_server(
          [{node, ServerNode}, {port, 0},
           {from, self()},
           {mfa, {?MODULE, echoer_active_once, [Length]}},
           {options, [{active, once}, {mode, binary} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
        ssl_test_lib:start_client(
          [{node, ClientNode}, {port, Port},
           {host, Hostname},
           {from, self()},
           {mfa, {?MODULE, sender_active_once, [Data]}},
           {options, [{active, once}, {mode, binary} | ClientOpts]}]),
    %%
    ssl_test_lib:check_result(Server, ok, Client, ok),
    %%
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


server_echos_active(
  Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname) ->
    Length = byte_size(Data),
    Server =
        ssl_test_lib:start_server(
          [{node, ServerNode}, {port, 0},
           {from, self()},
           {mfa, {?MODULE, echoer_active, [Length]}},
           {options, [{active, true}, {mode, binary} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
        ssl_test_lib:start_client(
          [{node, ClientNode}, {port, Port},
           {host, Hostname},
           {from, self()},
           {mfa, {?MODULE, sender_active, [Data]}},
           {options, [{active, true}, {mode, binary} | ClientOpts]}]),
    %%
    ssl_test_lib:check_result(Server, ok, Client, ok),
    %%
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

client_echos_passive(
  Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname) ->
    Length = byte_size(Data),
    Server =
        ssl_test_lib:start_server(
          [{node, ServerNode}, {port, 0},
           {from, self()},
           {mfa, {?MODULE, sender, [Data]}},
           {options, [{active, false}, {mode, binary} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
        ssl_test_lib:start_client(
          [{node, ClientNode}, {port, Port},
           {host, Hostname},
           {from, self()},
           {mfa, {?MODULE, echoer, [Length]}},
           {options, [{active, false}, {mode, binary} | ClientOpts]}]),
    %%
    ssl_test_lib:check_result(Server, ok, Client, ok),
    %%
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


client_echos_passive_chunk(
  Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname) ->
    Length = byte_size(Data),
    Server =
        ssl_test_lib:start_server(
          [{node, ServerNode}, {port, 0},
           {from, self()},
           {mfa, {?MODULE, sender, [Data]}},
           {options, [{active, false}, {mode, binary} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
        ssl_test_lib:start_client(
          [{node, ClientNode}, {port, Port},
           {host, Hostname},
           {from, self()},
           {mfa, {?MODULE, echoer_chunk, [Length]}},
           {options, [{active, false}, {mode, binary} | ClientOpts]}]),
    %%
    ssl_test_lib:check_result(Server, ok, Client, ok),
    %%
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


client_echos_active_once(
  Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname) ->
    Length = byte_size(Data),
    Server =
        ssl_test_lib:start_server(
          [{node, ServerNode}, {port, 0},
           {from, self()},
           {mfa, {?MODULE, sender_active_once, [Data]}},
           {options, [{active, once}, {mode, binary} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
        ssl_test_lib:start_client(
          [{node, ClientNode}, {port, Port},
           {host, Hostname},
           {from, self()},
           {mfa, {?MODULE, echoer_active_once, [Length]}},
           {options,[{active, once}, {mode, binary} | ClientOpts]}]),
    %%
    ssl_test_lib:check_result(Server, ok, Client, ok),
    %%
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

client_echos_active(
  Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname) ->
    Length = byte_size(Data),
    Server =
        ssl_test_lib:start_server(
          [{node, ServerNode}, {port, 0},
           {from, self()},
           {mfa, {?MODULE, sender_active, [Data]}},
           {options, [{active, true}, {mode, binary} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
        ssl_test_lib:start_client(
          [{node, ClientNode}, {port, Port},
           {host, Hostname},
           {from, self()},
           {mfa, {?MODULE, echoer_active, [Length]}},
           {options, [{active, true}, {mode, binary} | ClientOpts]}]),
    %
    ssl_test_lib:check_result(Server, ok, Client, ok),
    %%
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

client_active_once_server_close(
  Data, ClientOpts, ServerOpts, ClientNode, ServerNode, Hostname) ->
    Length = byte_size(Data),
    Server =
        ssl_test_lib:start_server(
          [{node, ServerNode}, {port, 0},
           {from, self()},
           {mfa, {?MODULE, send_close, [Data]}},
           {options, [{active, once}, {mode, binary} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
        ssl_test_lib:start_client(
          [{node, ClientNode}, {port, Port},
           {host, Hostname},
           {from, self()},
           {mfa, {ssl_test_lib, active_once_recv, [Length]}},
           {options,[{active, once}, {mode, binary} | ClientOpts]}]),
    %%
    ssl_test_lib:check_result(Server, ok, Client, ok).

send(_Socket, _Data, 0, _) ->
    ok;
send(Socket, Data, Count, RecvEcho) ->
    ok = ssl:send(Socket, Data),
    RecvEcho(),
    send(Socket, Data, Count - 1, RecvEcho).

send_close(Socket, Data) ->
    ok = ssl:send(Socket, Data),
    ssl:close(Socket).

sender(Socket, Data) ->
    ct:log("Sender recv: ~p~n", [ssl:getopts(Socket, [active])]),
    send(Socket, Data, 100,
              fun() -> 
                      ssl_test_lib:recv_disregard(Socket, byte_size(Data)) 
              end).

sender_active_once(Socket, Data) ->
    ct:log("Sender active once: ~p~n", [ssl:getopts(Socket, [active])]),
    send(Socket, Data, 100,
         fun() -> 
                 ssl_test_lib:active_once_disregard(Socket, byte_size(Data)) 
         end).

sender_active(Socket, Data) ->
    ct:log("Sender active: ~p~n", [ssl:getopts(Socket, [active])]),
    send(Socket, Data, 100,
         fun() -> 
                 ssl_test_lib:active_disregard(Socket, byte_size(Data)) 
         end).

echoer(Socket, Size) ->
    ct:log("Echoer recv: ~p~n", [ssl:getopts(Socket, [active])]),
    echo_recv(Socket, Size * 100).

echoer_chunk(Socket, Size) ->
    ct:log("Echoer recv: ~p~n", [ssl:getopts(Socket, [active])]),
    echo_recv_chunk(Socket, Size, Size * 100).

echoer_active_once(Socket, Size) ->
    ct:log("Echoer active once: ~p~n", [ssl:getopts(Socket, [active])]),
    echo_active_once(Socket, Size * 100).

echoer_active(Socket, Size) ->
    ct:log("Echoer active: ~p~n", [ssl:getopts(Socket, [active])]),
    echo_active(Socket, Size * 100).


%% Receive Size bytes
echo_recv(_Socket, 0) ->
    ok;
echo_recv(Socket, Size) ->
    {ok, Data} = ssl:recv(Socket, 0),
    spawn(fun() -> ssl:send(Socket, Data) end),
    echo_recv(Socket, Size - byte_size(Data)).


%% Receive Size bytes
echo_recv_chunk(_Socket, _, 0) ->
    ok;
echo_recv_chunk(Socket, ChunkSize, Size) ->
    {ok, Data} = ssl:recv(Socket, ChunkSize),
    spawn(fun() -> ssl:send(Socket, Data) end),
    echo_recv_chunk(Socket, ChunkSize, Size - ChunkSize).


%% Receive Size bytes
echo_active_once(_Socket, 0) ->
    ok;
echo_active_once(Socket, Size) ->
    receive
        {ssl, Socket, Data} ->
            spawn(fun() -> ssl:send(Socket, Data) end),
            NewSize = Size - byte_size(Data),
            ssl:setopts(Socket, [{active, once}]),
            echo_active_once(Socket, NewSize)
    end.

%% Receive Size bytes
echo_active(_Socket, 0) ->
    ok;
echo_active(Socket, Size) ->
    receive
        {ssl, Socket, Data} ->
            spawn(fun() -> ssl:send(Socket, Data) end),
            echo_active(Socket, Size - byte_size(Data))
    end.    
        
