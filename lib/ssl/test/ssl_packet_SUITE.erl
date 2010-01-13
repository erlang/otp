%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
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
-module(ssl_packet_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("test_server.hrl").

-define(BYTE(X),     X:8/unsigned-big-integer).
-define(UINT16(X),   X:16/unsigned-big-integer).
-define(UINT24(X),   X:24/unsigned-big-integer).
-define(UINT32(X),   X:32/unsigned-big-integer).
-define(UINT64(X),   X:64/unsigned-big-integer).
-define(STRING(X),   ?UINT32((size(X))), (X)/binary).

-define(byte(X),   << ?BYTE(X) >> ).
-define(uint16(X), << ?UINT16(X) >> ).
-define(uint24(X), << ?UINT24(X) >> ).
-define(uint32(X), << ?UINT32(X) >> ).
-define(uint64(X), << ?UINT64(X) >> ).
-define(TIMEOUT, 60000).

-define(MANY, 1000).
-define(SOME, 50).


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
    ssl:start(),
    Result = 
	(catch make_certs:all(?config(data_dir, Config), 
			      ?config(priv_dir, Config))),
    test_server:format("Make certs  ~p~n", [Result]),
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
    ["Test that erlang:decode_packet/3 seems to be handled correctly."
    "We only use the most basic packet types in our tests as testing of" 
    "the packet types are for inet to verify" 
    ];

all(suite) -> 
    [packet_raw_passive_many_small, 
     packet_0_passive_many_small, packet_1_passive_many_small,
     packet_2_passive_many_small, packet_4_passive_many_small, 
     packet_raw_passive_some_big, packet_0_passive_some_big, 
     packet_1_passive_some_big,
     packet_2_passive_some_big, packet_4_passive_some_big, 
     packet_raw_active_once_many_small, 
     packet_0_active_once_many_small, packet_1_active_once_many_small,
     packet_2_active_once_many_small, packet_4_active_once_many_small,
     packet_raw_active_once_some_big, 
     packet_0_active_once_some_big, packet_1_active_once_some_big,
     packet_2_active_once_some_big, packet_4_active_once_some_big,
     packet_raw_active_many_small, packet_0_active_many_small, 
     packet_1_active_many_small,
     packet_2_active_many_small, packet_4_active_many_small,
     packet_raw_active_some_big, packet_0_active_some_big, 
     packet_1_active_some_big, packet_2_active_some_big, 
     packet_4_active_some_big,
     packet_wait_passive, packet_wait_active,
     packet_baddata_passive, packet_baddata_active,
     packet_size_passive, packet_size_active
    ].

%% Test cases starts here.
%%--------------------------------------------------------------------
packet_raw_passive_many_small(doc) -> 
    ["Test packet option {packet, raw} in passive mode."];

packet_raw_passive_many_small(suite) -> 
    [];

packet_raw_passive_many_small(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "Packet option is {packet, raw}",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()}, 
			   {mfa, {?MODULE, send_raw ,[Data, ?MANY]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, passive_raw, [Data, ?MANY]}},
					{options, 
					 [{active, false},
					  {packet, raw} | 
					   ClientOpts]}]),
				       
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------

packet_raw_passive_some_big(doc) -> 
    ["Test packet option {packet, raw} in passive mode."];

packet_raw_passive_some_big(suite) -> 
    [];

packet_raw_passive_some_big(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = lists:append(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()}, 
			   {mfa, {?MODULE, send_raw ,[Data, ?SOME]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, passive_raw, [Data, ?SOME]}},
					{options, 
					 [{active, false},
					  {packet, raw} | 
					   ClientOpts]}]),
				       
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_0_passive_many_small(doc) -> 
    ["Test packet option {packet, 0} in passive mode."];

packet_0_passive_many_small(suite) -> 
    [];

packet_0_passive_many_small(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "Packet option is {packet, 0}, equivalent to packet raw.",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_raw ,[Data, ?MANY]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, passive_raw, [Data, ?MANY]}},
					{options, [{active, false},
						   {packet, 0} | 
						   ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_0_passive_some_big(doc) -> 
    ["Test packet option {packet, 0} in passive mode."];

packet_0_passive_some_big(suite) -> 
    [];

packet_0_passive_some_big(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = lists:append(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_raw ,[Data, ?SOME]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, passive_raw, [Data, ?SOME]}},
					{options, [{active, false},
						   {packet, 0} | 
						   ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_1_passive_many_small(doc) -> 
    ["Test packet option {packet, 1} in passive mode."];

packet_1_passive_many_small(suite) -> 
    [];

packet_1_passive_many_small(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "Packet option is {packet, 1}",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_1 ,[Data, ?MANY]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, passive_recv_packet,
				  [Data, ?MANY]}},
					{options, [{active, false},
						   {packet, 1} |
						   ClientOpts]}]),
        
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_1_passive_some_big(doc) -> 
    ["Test packet option {packet, 1} in passive mode."];

packet_1_passive_some_big(suite) -> 
    [];

packet_1_passive_some_big(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = lists:append(lists:duplicate(255, "1")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_1 ,[Data, ?SOME]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, passive_recv_packet,
				  [Data, ?SOME]}},
					{options, [{active, false},
						   {packet, 1} |
						   ClientOpts]}]),
        
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_2_passive_many_small(doc) -> 
    ["Test packet option {packet, 2} in passive mode"];

packet_2_passive_many_small(suite) -> 
    [];

packet_2_passive_many_small(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "Packet option is {packet, 2}",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_2 ,[Data, ?MANY]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, passive_recv_packet,
				  [Data, ?MANY]}},
					{options, [{active, false},
						   {packet, 2} | 
						   ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_2_passive_some_big(doc) -> 
    ["Test packet option {packet, 2} in passive mode"];

packet_2_passive_some_big(suite) -> 
    [];

packet_2_passive_some_big(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = lists:append(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_2 ,[Data, ?SOME]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, passive_recv_packet,
				  [Data, ?SOME]}},
					{options, [{active, false},
						   {packet, 2} | 
						   ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
    
%%--------------------------------------------------------------------
packet_4_passive_many_small(doc) -> 
    ["Test packet option {packet, 4} in passive mode"];

packet_4_passive_many_small(suite) -> 
    [];

packet_4_passive_many_small(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "Packet option is {packet, 4}",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
					{mfa, 
					 {?MODULE, send_4 ,[Data, ?MANY]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, passive_recv_packet,
				  [Data, ?MANY]}},
					{options, [{active, false},
						   {packet, 4} | 
						   ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_4_passive_some_big(doc) -> 
    ["Test packet option {packet, 4} in passive mode"];

packet_4_passive_some_big(suite) -> 
    [];

packet_4_passive_some_big(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = lists:append(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_4 ,[Data, ?SOME]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, passive_recv_packet,
				  [Data, ?SOME]}},
					{options, [{active, false},
						   {packet, 4} | 
						   ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
packet_raw_active_once_many_small(doc) -> 
    ["Test packet option {packet, raw} in active once mode."];

packet_raw_active_once_many_small(suite) -> 
    [];

packet_raw_active_once_many_small(Config) when is_list(Config) -> 
   ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "Packet option is {packet, raw}",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()}, 
			   {mfa, {?MODULE, send_raw ,[Data, ?MANY]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, active_once_raw, [Data, ?MANY]}},
					{options, [{active, once},
						   {packet, raw} | 
						   ClientOpts]}]),
        
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_raw_active_once_some_big(doc) -> 
    ["Test packet option {packet, raw} in active once mode."];

packet_raw_active_once_some_big(suite) -> 
    [];

packet_raw_active_once_some_big(Config) when is_list(Config) -> 
   ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = lists:append(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()}, 
			   {mfa, {?MODULE, send_raw ,[Data, ?SOME]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, active_once_raw, [Data, ?SOME]}},
					{options, [{active, once},
						   {packet, raw} | 
						   ClientOpts]}]),
        
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_0_active_once_many_small(doc) -> 
    ["Test packet option {packet, 0} in active once mode."];

packet_0_active_once_many_small(suite) -> 
    [];

packet_0_active_once_many_small(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "Packet option is {packet, 0}",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()}, 
			   {mfa, {?MODULE, send_raw ,[Data, ?MANY]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, 
					 {?MODULE, active_once_raw, 
					  [Data, ?MANY]}},
					{options, [{active, once},
						   {packet, 0} | 
						   ClientOpts]}]),
        
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
packet_0_active_once_some_big(doc) -> 
    ["Test packet option {packet, 0} in active once mode."];

packet_0_active_once_some_big(suite) -> 
    [];

packet_0_active_once_some_big(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = lists:append(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()}, 
					{mfa, {?MODULE, send_raw ,
					       [Data, ?SOME]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, 
					 {?MODULE, active_once_raw,
					  [Data, ?SOME]}},
					{options, [{active, once},
						   {packet, 0} | 
						   ClientOpts]}]),
        
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_1_active_once_many_small(doc) -> 
    ["Test packet option {packet, 1} in active once mode."];

packet_1_active_once_many_small(suite) -> 
    [];

packet_1_active_once_many_small(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "Packet option is {packet, 1}",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_1 ,[Data, ?MANY]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, 
					 {?MODULE, 
					  active_once_packet, 
					  [Data, ?MANY]}},
					{options, [{active, once},
						   {packet, 1} |
						   ClientOpts]}]),
        
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_1_active_once_some_big(doc) -> 
    ["Test packet option {packet, 1} in active once mode."];

packet_1_active_once_some_big(suite) -> 
    [];

packet_1_active_once_some_big(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = lists:append(lists:duplicate(255, "1")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_1 ,[Data, ?SOME]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
					{mfa, 
					 {?MODULE, 
					  active_once_packet,
					  [Data, ?SOME]}},
					{options, [{active, once},
						   {packet, 1} |
						   ClientOpts]}]),
        
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_2_active_once_many_small(doc) -> 
    ["Test packet option {packet, 2} in active once mode"];

packet_2_active_once_many_small(suite) -> 
    [];

packet_2_active_once_many_small(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "Packet option is {packet, 2}",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_2 ,[Data, ?MANY]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
					{mfa, 
					 {?MODULE, 
					  active_once_packet, 
					  [Data, ?MANY]}},
					{options, [{active, once},
						   {packet, 2} |
						   ClientOpts]}]),
        
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
packet_2_active_once_some_big(doc) -> 
    ["Test packet option {packet, 2} in active once mode"];

packet_2_active_once_some_big(suite) -> 
    [];

packet_2_active_once_some_big(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = lists:append(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_2 ,[Data, ?SOME]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
					{mfa, 
					 {?MODULE, 
					  active_once_packet,
					  [Data, ?SOME]}},
					{options, [{active, once},
						   {packet, 2} |
						   ClientOpts]}]),
        
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_4_active_once_many_small(doc) -> 
    ["Test packet option {packet, 4} in active once mode"];

packet_4_active_once_many_small(suite) -> 
    [];

packet_4_active_once_many_small(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "Packet option is {packet, 4}",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_4 ,[Data, ?MANY]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
					{mfa, 
					 {?MODULE, 
					  active_once_packet, 
					  [Data, ?MANY]}},
					{options, [{active, once},
						   {packet, 4} |
						   ClientOpts]}]),
        
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_4_active_once_some_big(doc) -> 
    ["Test packet option {packet, 4} in active once mode"];

packet_4_active_once_some_big(suite) -> 
    [];

packet_4_active_once_some_big(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = lists:append(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_4 ,[Data, ?SOME]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
					{mfa, 
					 {?MODULE, 
					  active_once_packet, 
					  [Data, ?SOME]}},
					{options, [{active, once},
						   {packet, 4} |
						   ClientOpts]}]),
        
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_raw_active_many_small(doc) -> 
    ["Test packet option {packet, raw} in active mode."];

packet_raw_active_many_small(suite) -> 
    [];

packet_raw_active_many_small(Config) when is_list(Config) ->
 ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "Packet option is {packet, raw}",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()}, 
			   {mfa, {?MODULE, send_raw ,[Data, ?MANY]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, active_raw, 
				  [Data, ?MANY]}},
					{options, [{active, true},
						   {packet, raw} | 
						   ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
packet_raw_active_some_big(doc) -> 
    ["Test packet option {packet, raw} in active mode."];

packet_raw_active_some_big(suite) -> 
    [];

packet_raw_active_some_big(Config) when is_list(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = lists:append(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()}, 
			   {mfa, {?MODULE, send_raw ,[Data, ?SOME]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, active_raw, [Data, ?SOME]}},
					{options, [{active, true},
						   {packet, raw} | 
						   ClientOpts]}]),
        
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_0_active_many_small(doc) -> 
    ["Test packet option {packet, 0} in active mode."];

packet_0_active_many_small(suite) -> 
    [];

packet_0_active_many_small(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "Packet option is {packet, 0}",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()}, 
			   {mfa, {?MODULE, send_raw ,[Data, ?MANY]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, 
					 {?MODULE, active_raw, 
					  [Data, ?MANY]}},
					{options, [{active, true},
						   {packet, 0} | 
						   ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_0_active_some_big(doc) -> 
    ["Test packet option {packet, 0} in active mode."];

packet_0_active_some_big(suite) -> 
    [];

packet_0_active_some_big(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
   
    Data = lists:append(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0},
					{from, self()}, 
			   {mfa, {?MODULE, send_raw ,[Data, ?SOME]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, 
					 {?MODULE, active_raw, 
					  [Data, ?SOME]}},
					{options, [{active, true},
						   {packet, 0} | 
						   ClientOpts]}]),
        
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
packet_1_active_many_small(doc) -> 
    ["Test packet option {packet, 1} in active mode."];

packet_1_active_many_small(suite) -> 
    [];

packet_1_active_many_small(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "Packet option is {packet, 1}",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_1 ,[Data, ?MANY]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
					{mfa, 
					 {?MODULE, 
					  active_packet, [Data, ?MANY]}},
					{options, [{active, true},
						   {packet, 1} |
						   ClientOpts]}]),
        
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_1_active_some_big(doc) -> 
    ["Test packet option {packet, 1} in active mode."];

packet_1_active_some_big(suite) -> 
    [];

packet_1_active_some_big(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = lists:append(lists:duplicate(255, "1")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_1 ,[Data, ?SOME]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
					{mfa, 
					 {?MODULE, 
					  active_packet, [Data, ?SOME]}},
					{options, [{active, true},
						   {packet, 1} |
						   ClientOpts]}]),
        
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_2_active_many_small(doc) -> 
    ["Test packet option {packet, 2} in active mode"];

packet_2_active_many_small(suite) -> 
    [];

packet_2_active_many_small(Config) when is_list(Config) -> 
 ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "Packet option is {packet, 2}",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_2 ,[Data, ?MANY]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
					{mfa, 
					 {?MODULE, 
					  active_packet, [Data, ?MANY]}},
					{options, [{active, true},
						   {packet, 2} |
						   ClientOpts]}]),
        
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_2_active_some_big(doc) -> 
    ["Test packet option {packet, 2} in active mode"];

packet_2_active_some_big(suite) -> 
    [];

packet_2_active_some_big(Config) when is_list(Config) -> 
 ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = lists:append(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_2 ,[Data, ?SOME]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
					{mfa, 
					 {?MODULE, 
					  active_packet, [Data, ?SOME]}},
					{options, [{active, true},
						   {packet, 2} |
						   ClientOpts]}]),
        
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
packet_4_active_many_small(doc) -> 
    ["Test packet option {packet, 4} in active mode"];

packet_4_active_many_small(suite) -> 
    [];

packet_4_active_many_small(Config) when is_list(Config) -> 
     ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "Packet option is {packet, 4}",

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_4 ,[Data, ?MANY]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
					{mfa, 
					 {?MODULE, 
					  active_packet, [Data, ?MANY]}},
					{options, [{active, true},
						   {packet, 4} |
						   ClientOpts]}]),
        
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
packet_4_active_some_big(doc) -> 
    ["Test packet option {packet, 4} in active mode"];

packet_4_active_some_big(suite) -> 
    [];

packet_4_active_some_big(Config) when is_list(Config) -> 
     ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = lists:append(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, send_4 ,[Data, ?SOME]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
					{mfa, 
					 {?MODULE, 
					  active_packet, [Data, ?SOME]}},
					{options, [{active, true},
						   {packet, 4} |
						   ClientOpts]}]),
        
    ssl_test_lib:check_result(Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).




%%--------------------------------------------------------------------
packet_wait_active(doc) -> 
    ["Test waiting when complete packages have not arrived"];

packet_wait_active(suite) -> 
    [];

packet_wait_active(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = list_to_binary(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, send_incomplete ,[Data, ?SOME]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, 
					 {?MODULE, active_packet,
					  [binary_to_list(Data), ?SOME]}},
					{options, [{active, true},
						   {packet, 4} |
						   ClientOpts]}]),

    ssl_test_lib:check_result(Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
packet_wait_passive(doc) -> 
    ["Test waiting when complete packages have not arrived"];

packet_wait_passive(suite) -> 
    [];

packet_wait_passive(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = list_to_binary(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, send_incomplete ,[Data, ?SOME]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, passive_recv_packet,
					       [binary_to_list(Data), ?SOME]}},
					{options, [{active, false},
						   {packet, 4} |
						   ClientOpts]}]),

    ssl_test_lib:check_result(Client, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
packet_baddata_active(doc) -> 
    ["Test that if a bad packet arrives error msg is sent and socket is closed"];
packet_baddata_active(suite) -> 
    [];

packet_baddata_active(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = list_to_binary(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, send_incomplete ,[Data, 1]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, active_packet, [Data, 1]}},
					{options, [{active, true},
						   {packet, cdr} |
						   ClientOpts]}]),
    receive
	{Client, {other, {ssl_error, _Socket, {invalid_packet, _}},{error,closed},1}} -> ok;
	Unexpected ->
	    test_server:fail({unexpected, Unexpected})
    end,    

    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
packet_baddata_passive(doc) -> 
    ["Test that if a bad packet arrives error msg is sent and socket is closed"];

packet_baddata_passive(suite) -> 
    [];

packet_baddata_passive(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = list_to_binary(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, send_incomplete ,[Data, 1]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, passive_recv_packet,
					       [Data, 1]}},
					{options, [{active, false},
						   {packet, cdr} |
						   ClientOpts]}]),
    
    receive
	{Client, {other, {error, {invalid_packet, _}},{error,closed}, 1}} -> ok;
	Unexpected ->
	    test_server:fail({unexpected, Unexpected})
    end,    

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
packet_size_active(doc) -> 
    ["Test that if a packet of size larger than packet_size arrives error msg is sent and socket is closed"];
packet_size_active(suite) -> 
    [];

packet_size_active(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = list_to_binary(lists:duplicate(100, "1234567890")),
    
    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, send_incomplete ,[Data, 1]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, active_packet, [Data, 1]}},
					{options, [{active, true},
						   {packet, 4}, {packet_size, 10} |
						   ClientOpts]}]),
    receive
	{Client, {other, {ssl_error, _Socket, {invalid_packet, _}},{error,closed},1}} -> ok;
	Unexpected ->
	    test_server:fail({unexpected, Unexpected})
    end,    

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
packet_size_passive(doc) -> 
    ["Test that if a packet of size larger than packet_size arrives error msg is sent and socket is closed"];
packet_size_passive(suite) -> 
    [];

packet_size_passive(Config) when is_list(Config) -> 
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = list_to_binary(lists:duplicate(100, "1234567890")),

    Server = ssl_test_lib:start_server([{node, ClientNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, send_incomplete ,[Data, 1]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ServerNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, passive_recv_packet, [Data, 1]}},
					{options, [{active, false},
						   {packet, 4}, {packet_size, 30} |
						   ClientOpts]}]),
    receive
	{Client, {other, {error, {invalid_packet, _}},{error,closed},1}} -> ok;
	Unexpected ->
	    test_server:fail({unexpected, Unexpected})
    end,

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
%% Internal functions

send_raw(_,_, 0) ->
    no_result_msg;
send_raw(Socket, Data, N) ->
    ssl:send(Socket, Data),
    send_raw(Socket, Data, N-1).

passive_raw(_, _, 0) ->
    ok;
passive_raw(Socket, Data, N) ->
    Length = length(Data),
    {ok, Data} = ssl:recv(Socket, Length),
    passive_raw(Socket, Data, N-1).

passive_recv_packet(_, _, 0) ->
    ok;
passive_recv_packet(Socket, Data, N) ->
    case ssl:recv(Socket, 0) of
	{ok, Data} -> 
	    passive_recv_packet(Socket, Data, N-1);
	Other ->
	    {other, Other, ssl:session_info(Socket), N}
    end.
    
send_1(_,_, 0) ->
    no_result_msg;
send_1(Socket, Data, N) ->
    Length = length(Data),
    ssl:send(Socket, [?byte(Length), Data]),
    send_1(Socket, Data, N-1).

send_2(_,_, 0) ->
    no_result_msg;
send_2(Socket, Data, N) ->
    Length = length(Data),
    ssl:send(Socket, [?uint16(Length), Data]),
    send_2(Socket, Data, N-1).

send_4(_,_, 0) ->
    no_result_msg;
send_4(Socket, Data, N) ->
    Length = length(Data),
    ssl:send(Socket, [?uint32(Length), Data]),
    send_4(Socket, Data, N-1).

send_incomplete(Socket, Data, N) ->
    send_incomplete(Socket, Data, N, <<>>).
send_incomplete(Socket, _Data, 0, Prev) ->
    ssl:send(Socket, Prev),
    no_result_msg;
send_incomplete(Socket, Data, N, Prev) ->
    Length = size(Data),
    <<Part1:42/binary, Rest/binary>> = Data,
    ssl:send(Socket, [Prev, ?uint32(Length), Part1]),
    send_incomplete(Socket, Data, N-1, Rest).

active_once_raw(Socket, Data, N) ->
    active_once_raw(Socket, Data, N, []).

active_once_raw(_, _, 0, _) ->
    ok;
active_once_raw(Socket, Data, N, Acc) ->
    receive 
	{ssl, Socket, Data} ->
	    ssl:setopts(Socket, [{active, once}]),
	    active_once_raw(Socket, Data, N-1, []);
	{ssl, Socket, Other} ->
	    case Acc ++ Other of
		Data ->
		    ssl:setopts(Socket, [{active, once}]),
		    active_once_raw(Socket, Data, N-1, []);
		NewAcc ->
		    ssl:setopts(Socket, [{active, once}]),
		    active_once_raw(Socket, Data, N, NewAcc)
	    end
    end.
		    
active_once_packet(_,_, 0) ->
    ok;
active_once_packet(Socket, Data, N) ->
    receive 
	{ssl, Socket, Data} ->
	    ok
    end,
    ssl:setopts(Socket, [{active, once}]),
    active_once_packet(Socket, Data, N-1).

active_raw(Socket, Data, N) ->
    active_raw(Socket, Data, N, []).

active_raw(_, _, 0, _) ->
    ok;
active_raw(Socket, Data, N, Acc) ->
    receive 
	{ssl, Socket, Data} ->
	    active_raw(Socket, Data, N-1, []);
	{ssl, Socket, Other} ->
	    case Acc ++ Other of
		Data ->
		    active_raw(Socket, Data, N-1, []);
		NewAcc ->
		    active_raw(Socket, Data, NewAcc)
	    end
    end.

active_packet(_, _, 0) ->
    ok;
active_packet(Socket, Data, N) ->
    receive 
	{ssl, Socket, Data} ->
	    active_packet(Socket, Data, N -1);
	Other ->
	    {other, Other, ssl:session_info(Socket),N}
    end.
