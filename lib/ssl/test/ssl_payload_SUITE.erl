%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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

-module(ssl_payload_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server.hrl").

-define(TIMEOUT, 600000).

%% Test server callback functions
%%--------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Initialization before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    crypto:start(),
    application:start(public_key),
    ssl:start(),
    make_certs:all(?config(data_dir, Config), ?config(priv_dir, Config)),
    ssl_test_lib:cert_options(Config).

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ssl:stop(),
    crypto:stop().

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config) -> Config
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initialization before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%% Description: Initialization before each test case
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config0) ->
    Config = lists:keydelete(watchdog, 1, Config0),
    Dog = ssl_test_lib:timetrap(?TIMEOUT),
    [{watchdog, Dog} | Config].

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
    Dog = ?config(watchdog, Config),
    case Dog of 
	undefined ->
	    ok;
	_ ->
	    test_server:timetrap_cancel(Dog)
    end.

%%--------------------------------------------------------------------
%% Function: all(Clause) -> TestCases
%% Clause - atom() - suite | doc
%% TestCases - [Case] 
%% Case - atom()
%%   Name of a test case.
%% Description: Returns a list of all test cases in this test suite
%%--------------------------------------------------------------------
all(doc) -> 
    ["Test payload over ssl in all socket modes, active, active_once,"
     "and passive mode."];

all(suite) -> 
    [server_echos_passive_small, server_echos_active_once_small, 
     server_echos_active_small,
     client_echos_passive_small, client_echos_active_once_small, 
     client_echos_active_small,
     server_echos_passive_big, server_echos_active_once_big, 
     server_echos_active_big,
     client_echos_passive_big, client_echos_active_once_big, 
     client_echos_active_big,
     server_echos_passive_huge, server_echos_active_once_huge, 
     server_echos_active_huge,
     client_echos_passive_huge, client_echos_active_once_huge, 
     client_echos_active_huge    
    ].

%% Test cases starts here.
%%--------------------------------------------------------------------
server_echos_passive_small(doc) -> 
    ["Client sends 1000 bytes in passive mode to server, that receives them, "
     "sends them back, and closes."];

server_echos_passive_small(suite) -> 
    [];

server_echos_passive_small(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Str = "1234567890", 
    
    server_echos_passive(Str, 1000, ClientOpts, ServerOpts, 
			 ClientNode, ServerNode, Hostname).

%%--------------------------------------------------------------------

server_echos_active_once_small(doc) -> 
    ["Client sends 1000 bytes in active once mode to server, that receives "
     " them, sends them back, and closes."];

server_echos_active_once_small(suite) -> 
    [];

server_echos_active_once_small(Config) when is_list(Config) -> 
        ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Str = "1234567890", 
    
    server_echos_active_once(Str, 1000, ClientOpts, ServerOpts, 
			     ClientNode, ServerNode, Hostname).

%%--------------------------------------------------------------------

server_echos_active_small(doc) -> 
    ["Client sends 1000 bytes in active mode to server, that receives them, "
     "sends them back, and closes."];

server_echos_active_small(suite) -> 
    [];

server_echos_active_small(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Str = "1234567890", 
    
    server_echos_active(Str, 1000, ClientOpts, ServerOpts, 
			ClientNode, ServerNode, Hostname).

%%--------------------------------------------------------------------
client_echos_passive_small(doc) -> 
    ["Server sends 1000 bytes in passive mode to client, that receives them, "
     "sends them back, and closes."];

client_echos_passive_small(suite) -> 
    [];

client_echos_passive_small(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Str = "1234567890", 

    client_echos_passive(Str, 1000, ClientOpts, ServerOpts, ClientNode, 
			 ServerNode, Hostname).

%%--------------------------------------------------------------------
client_echos_active_once_small(doc) -> 
    ["Server sends 1000 bytes in active once mode to client, that receives "
     "them, sends them back, and closes."];

client_echos_active_once_small(suite) -> 
    [];

client_echos_active_once_small(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Str = "1234567890", 
    
    client_echos_active_once(Str, 1000, ClientOpts, ServerOpts, ClientNode, 
			     ServerNode, Hostname).
   
%%--------------------------------------------------------------------
client_echos_active_small(doc) -> 
    ["Server sends 1000 bytes in active mode to client, that receives them, "
     "sends them back, and closes."];

client_echos_active_small(suite) -> 
    [];

client_echos_active_small(Config) when is_list(Config) -> 
     ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Str = "1234567890",     

    client_echos_active(Str, 1000, ClientOpts, ServerOpts, ClientNode, 
			ServerNode, Hostname).


%%--------------------------------------------------------------------
server_echos_passive_big(doc) -> 
    ["Client sends 50000 bytes to server in passive mode, that receives them, "
     "sends them back, and closes."];

server_echos_passive_big(suite) -> 
    [];

server_echos_passive_big(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Str = "1234567890", 
    
    server_echos_passive(Str, 50000, ClientOpts, ServerOpts, ClientNode, 
			 ServerNode, Hostname).

%%--------------------------------------------------------------------

server_echos_active_once_big(doc) -> 
    ["Client sends 50000 bytes to server in active once mode, that receives "
     "them, sends them back, and closes."];

server_echos_active_once_big(suite) -> 
    [];

server_echos_active_once_big(Config) when is_list(Config) -> 
        ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Str = "1234567890", 
    
    server_echos_active_once(Str, 50000, ClientOpts, ServerOpts, ClientNode, 
			     ServerNode, Hostname).

%%--------------------------------------------------------------------

server_echos_active_big(doc) -> 
    ["Client sends 50000 bytes to server in active once mode, that receives "
     " them, sends them back, and closes."];

server_echos_active_big(suite) -> 
    [];

server_echos_active_big(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Str = "1234567890", 
    
    server_echos_active(Str, 50000, ClientOpts, ServerOpts, ClientNode, 
			ServerNode, Hostname).

%%--------------------------------------------------------------------
client_echos_passive_big(doc) -> 
    ["Server sends 50000 bytes to client in passive mode, that receives them, "
     "sends them back, and closes."];

client_echos_passive_big(suite) -> 
    [];

client_echos_passive_big(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Str = "1234567890", 

    client_echos_passive(Str, 50000, ClientOpts, ServerOpts, ClientNode, 
			 ServerNode, Hostname).

%%--------------------------------------------------------------------
client_echos_active_once_big(doc) -> 
    ["Server sends 50000 bytes to client in active once mode, that receives"
     " them, sends them back, and closes."];

client_echos_active_once_big(suite) -> 
    [];

client_echos_active_once_big(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Str = "1234567890", 

    client_echos_active_once(Str, 50000, ClientOpts, ServerOpts, ClientNode, 
			     ServerNode, Hostname).
   
%%--------------------------------------------------------------------
client_echos_active_big(doc) -> 
    ["Server sends 50000 bytes to client in active mode, that receives them, "
     "sends them back, and closes."];

client_echos_active_big(suite) -> 
    [];

client_echos_active_big(Config) when is_list(Config) -> 
     ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Str = "1234567890", 
    
    client_echos_active(Str, 50000, ClientOpts, ServerOpts, ClientNode, 
			ServerNode, Hostname).

%%--------------------------------------------------------------------
server_echos_passive_huge(doc) -> 
    ["Client sends 500000 bytes to server in passive mode, that receives "
     " them, sends them back, and closes."];

server_echos_passive_huge(suite) -> 
    [];

server_echos_passive_huge(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Str = "1234567890", 
    
    server_echos_passive(Str, 500000, ClientOpts, ServerOpts, ClientNode, 
			 ServerNode, Hostname).

%%--------------------------------------------------------------------
server_echos_active_once_huge(doc) -> 
    ["Client sends 500000 bytes to server in active once mode, that receives "
     "them, sends them back, and closes."];

server_echos_active_once_huge(suite) -> 
    [];

server_echos_active_once_huge(Config) when is_list(Config) -> 
        ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Str = "1234567890", 
    
    server_echos_active_once(Str, 500000, ClientOpts, ServerOpts, ClientNode, 
			     ServerNode, Hostname).

%%--------------------------------------------------------------------
server_echos_active_huge(doc) -> 
    ["Client sends 500000 bytes to server in active mode, that receives them, "
     "sends them back, and closes."];

server_echos_active_huge(suite) -> 
    [];

server_echos_active_huge(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Str = "1234567890", 
    
    server_echos_active(Str, 500000, ClientOpts, ServerOpts, ClientNode, 
			ServerNode, Hostname).

%%--------------------------------------------------------------------
client_echos_passive_huge(doc) -> 
    ["Server sends 500000 bytes to client in passive mode, that receives "
     "them, sends them back, and closes."];

client_echos_passive_huge(suite) -> 
    [];

client_echos_passive_huge(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Str = "1234567890", 
    client_echos_passive(Str, 500000, ClientOpts, ServerOpts, ClientNode, 
			 ServerNode, Hostname).

%%--------------------------------------------------------------------
client_echos_active_once_huge(doc) -> 
    ["Server sends 500000 bytes to client in active once mode, that receives "
     "them, sends them back, and closes."];

client_echos_active_once_huge(suite) -> 
    [];

client_echos_active_once_huge(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Str = "1234567890", 
    client_echos_active_once(Str, 500000, ClientOpts, ServerOpts, ClientNode, 
			     ServerNode, Hostname).
   
%%--------------------------------------------------------------------
client_echos_active_huge(doc) -> 
    ["Server sends 500000 bytes to client in active mode, that receives them, "
     "sends them back, and closes."];

client_echos_active_huge(suite) -> 
    [];

client_echos_active_huge(Config) when is_list(Config) -> 
     ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Str = "1234567890", 
    client_echos_active(Str, 500000, ClientOpts, ServerOpts, ClientNode,
			ServerNode, Hostname).
 
%%--------------------------------------------------------------------

server_echos_passive(Data, Length, ClientOpts, ServerOpts, 
		     ClientNode, ServerNode, Hostname) ->
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, 
					 {?MODULE, echoer, 
					  [Data, Length]}},
					{options, 
					 [{active, false},{mode, binary} 
					  | ServerOpts]}]),
				       Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, 
					 {?MODULE, sender, 
					       [Data, 
						Length]}},
					{options, 
					 [{active, false}, {mode, binary} |
					  ClientOpts]}]),
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


server_echos_active_once(Data, Length, ClientOpts, ServerOpts, ClientNode,
			 ServerNode, Hostname) ->
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, 
					 {?MODULE, echoer_once, 
					  [Data, Length]}},
					{options, [{active, once},
						   {mode, binary}| 
						   ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, 
					 {?MODULE, sender_once, 
					  [Data, Length]}},
					{options, [{active, once}, 
						   {mode, binary} | 
						   ClientOpts]}]),
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


server_echos_active(Data, Length, ClientOpts, ServerOpts, 
		    ClientNode, ServerNode, Hostname) ->
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, 
					 {?MODULE, echoer_active, 
					  [Data, Length]}},
					{options,  
					 [{active, true},
					  {mode, binary} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, 
					 {?MODULE, sender_active, 
					       [Data, 
						Length]}},
					{options,  
					 [{active, true}, {mode, binary}
					  | ClientOpts]}]),
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

client_echos_passive(Data, Length, ClientOpts, ServerOpts, 
		     ClientNode, ServerNode, Hostname) ->
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, 
					 {?MODULE, sender, 
					  [Data, Length]}},
					{options, 
					 [{active, false}, {mode, binary} | 
					  ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, 
					 {?MODULE, echoer, 
					       [Data, 
						Length]}},
					{options, 
					 [{active, false}, {mode, binary}
					  | ClientOpts]}]),
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

client_echos_active_once(Data, Length, 
			 ClientOpts, ServerOpts, ClientNode, ServerNode,
			 Hostname) ->
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, 
					 {?MODULE, sender_once, 
					  [Data, Length]}},
					{options, [{active, once},
						   {mode, binary} | 
						   ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, 
					 {?MODULE, echoer_once, 
					  [Data, 
					   Length]}},
					{options,[{active, once},
						  {mode, binary}
						  | ClientOpts]}]),
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

client_echos_active(Data, Length, ClientOpts, ServerOpts, ClientNode, 
		    ServerNode,
			 Hostname) ->
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, 
					 {?MODULE, sender_active, 
					  [Data, Length]}},
					{options, [{active, true}, 
						   {mode, binary}
						   | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, 
					 {?MODULE, echoer_active, 
					       [Data, 
						Length]}},
					{options, [{active, true},
						   {mode, binary}
						   | ClientOpts]}]),
    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

send(_, _, _, 0,_) ->
    ok;
send(Socket, Data, Size, Repeate,F) ->
    NewData = lists:duplicate(Size div 10, Data),
    ssl:send(Socket, NewData),
    F(),
    send(Socket, Data, Size, Repeate - 1,F).
 
sender(Socket, Data, Size) ->
    ok = send(Socket, Data, Size, 100, fun() -> do_recv(Socket, Data, Size, <<>>, false) end),
    test_server:format("Sender recv: ~p~n", [ssl:getopts(Socket, [active])]),
    ok.

sender_once(Socket, Data, Size) ->
    send(Socket, Data, Size, 100, 
	 fun() -> do_active_once(Socket, Data, Size, <<>>, false) end),
    test_server:format("Sender active once: ~p~n", 
		       [ssl:getopts(Socket, [active])]),
    ok.

sender_active(Socket, Data, Size) ->
    F = fun() -> do_active(Socket, Data, Size, <<>>, false) end,
    send(Socket, Data, Size, 100, F),
    test_server:format("Sender active: ~p~n", [ssl:getopts(Socket, [active])]),
    ok.

echoer(Socket, Data, Size) ->
    test_server:format("Echoer recv: ~p~n", [ssl:getopts(Socket, [active])]),
    echo(fun() -> do_recv(Socket, Data, Size, <<>>, true) end, 100).

echoer_once(Socket, Data, Size) ->
    test_server:format("Echoer active once: ~p ~n",
		       [ssl:getopts(Socket, [active])]),
    echo(fun() -> do_active_once(Socket, Data, Size, <<>>, true) end, 100).

echoer_active(Socket, Data, Size) ->
    test_server:format("Echoer active: ~p~n", [ssl:getopts(Socket, [active])]),
    echo(fun() -> do_active(Socket, Data, Size, <<>>, true) end, 100).

echo(_Fun, 0) -> ok;
echo(Fun, N) ->
    Fun(),
    echo(Fun, N-1).


do_recv(_Socket, _Data, 0, _Acc, true) ->
    ok;
do_recv(_Socket, Data, 0, Acc, false) ->
    Data = lists:sublist(binary_to_list(Acc), 10);

do_recv(Socket, Data, Size, Acc, Echo) ->
    {ok, NewData} = ssl:recv(Socket, 0),
    NewSize = size(NewData), 
    case Echo of
	true ->
	    ssl:send(Socket, NewData),
	    NewSize = size(NewData), 
	    do_recv(Socket, Data, Size - NewSize, [], Echo);
	false ->
	    case size(Acc) < 10 of
		true ->
		    do_recv(Socket, Data, Size - NewSize, 
			    <<Acc/binary, NewData/binary>>, Echo);
		false ->
		    do_recv(Socket, Data, Size - NewSize, Acc, Echo) 
	    end
    end.

do_active_once(_Socket, _Data, 0, _Acc, true) ->
    ok;
do_active_once(_Socket, Data, 0, Acc, false) ->
    Data = lists:sublist(binary_to_list(Acc), 10);

do_active_once(Socket, Data, Size, Acc, Echo) ->    
    receive 
	{ssl, Socket, NewData} ->
	    NewSize = size(NewData), 
	    case Echo of
		true ->
		    ssl:send(Socket, NewData),
		    ssl:setopts(Socket, [{active, once}]),
		    do_active_once(Socket, Data, Size - NewSize, [], Echo);
		false ->
		    case size(Acc) < 10 of
			true ->
			    ssl:setopts(Socket, [{active, once}]),
			    do_active_once(Socket, Data, Size - NewSize, 
					   <<Acc/binary, NewData/binary>>, 
					   Echo);
			false ->
			    ssl:setopts(Socket, [{active, once}]),
			    do_active_once(Socket, Data, 
					   Size - NewSize, Acc, Echo) 
		    end 
	    end
    end.
    
do_active(_Socket, _Data, 0, _Acc, true) ->
    ok;
do_active(_Socket, Data, 0, Acc, false) ->
    Data = lists:sublist(binary_to_list(Acc), 10);

do_active(Socket, Data, Size, Acc, Echo) ->    
    receive 
	{ssl, Socket, NewData} ->
	    NewSize = size(NewData), 
	    case Echo of
		true ->
		    ssl:send(Socket, NewData),
		    do_active(Socket, Data, Size - NewSize, [], Echo);
		false ->
		    case size(Acc) < 10 of
			true ->
			    do_active(Socket, Data, Size - NewSize, 
					   <<Acc/binary, NewData/binary>>, 
				      Echo);
			false ->
			    do_active(Socket, Data, 
				      Size - NewSize, Acc, Echo) 
		    end 
	    end
    end.
