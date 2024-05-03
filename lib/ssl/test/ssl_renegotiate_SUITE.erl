%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2023. All Rights Reserved.
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

-module(ssl_renegotiate_SUITE).

-behaviour(ct_suite).

-include("ssl_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").
-include("ssl_record.hrl").

%% Common test
-export([all/0,
         groups/0,
         init_per_suite/1,
         init_per_group/2,
         init_per_testcase/2,
         end_per_suite/1,
         end_per_group/2,
         end_per_testcase/2
        ]).

%% Test cases
-export([client_renegotiate/0,
         client_renegotiate/1,
         server_renegotiate/0,
         server_renegotiate/1,
         client_secure_renegotiate/0,
         client_secure_renegotiate/1,
         client_secure_renegotiate_fallback/0,
         client_secure_renegotiate_fallback/1,
         client_renegotiate_reused_session/0,
         client_renegotiate_reused_session/1,
         server_renegotiate_reused_session/0,
         server_renegotiate_reused_session/1,
         client_no_wrap_sequence_number/0,
         client_no_wrap_sequence_number/1,
         server_no_wrap_sequence_number/0,
         server_no_wrap_sequence_number/1,
         renegotiate_dos_mitigate_active/0,
         renegotiate_dos_mitigate_active/1,
         renegotiate_dos_mitigate_passive/0,
         renegotiate_dos_mitigate_passive/1,
         renegotiate_dos_mitigate_absolute/0,
         renegotiate_dos_mitigate_absolute/1,
         active_error_disallowed_client_renegotiate/0,
         active_error_disallowed_client_renegotiate/1
        ]).

%% Apply export
-export([renegotiate/2,
         renegotiate_reuse_session/2,
         renegotiate_immediately/1,
         renegotiate_rejected/1,
         erlang_ssl_receive/2]).

-define(SLEEP, 500).
-define(RENEGOTIATION_DISABLE_TIME, 12000).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() -> 
    [
     {group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'dtlsv1.2'},
     {group, 'dtlsv1'}
    ].

groups() ->
    [{'dtlsv1.2', [], renegotiate_tests()},
     {'dtlsv1',   [], renegotiate_tests()},
     {'tlsv1.2',  [], renegotiate_tests()},
     {'tlsv1.1',  [], renegotiate_tests()},
     {'tlsv1',    [], renegotiate_tests()}
    ].

renegotiate_tests() ->
    [client_renegotiate,
     server_renegotiate,
     client_secure_renegotiate,
     client_secure_renegotiate_fallback,
     client_renegotiate_reused_session,
     server_renegotiate_reused_session,
     client_no_wrap_sequence_number,
     server_no_wrap_sequence_number,
     renegotiate_dos_mitigate_active,
     renegotiate_dos_mitigate_passive,
     renegotiate_dos_mitigate_absolute,
     active_error_disallowed_client_renegotiate].

init_per_suite(Config) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
	    ssl_test_lib:clean_start(),
            ssl_test_lib:make_rsa_cert(Config)
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group(GroupName, Config). 

end_per_group(GroupName, Config) ->
  ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(TestCase, Config)  when TestCase == renegotiate_dos_mitigate_active;
                                          TestCase == renegotiate_dos_mitigate_passive;
                                          TestCase == renegotiate_dos_mitigate_absolute ->
    ct:timetrap({seconds, 25}),
    Config;
init_per_testcase(_, Config) ->
    ct:timetrap({seconds, 15}),
    Config.

end_per_testcase(_, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
client_renegotiate() ->
    [{doc,"Test ssl:renegotiate/1 on client."}].
client_renegotiate(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From erlang to erlang",

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       renegotiate, [Data]}},
					{options, [{reuse_sessions, false} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok, Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
client_secure_renegotiate() ->
    [{doc,"Test ssl:renegotiate/1 on client."}].
client_secure_renegotiate(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From erlang to erlang",

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
				   {options, [{secure_renegotiate, true} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       renegotiate, [Data]}},
					{options, [{reuse_sessions, false},
						   {secure_renegotiate, true}| ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok, Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
client_secure_renegotiate_fallback() ->
    [{doc,"Test that we can set secure_renegotiate to false that is "
      "fallback option, we however do not have a insecure server to test against!"}].
client_secure_renegotiate_fallback(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From erlang to erlang",

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
				   {options, [{secure_renegotiate, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       renegotiate, [Data]}},
					{options, [{reuse_sessions, false},
						   {secure_renegotiate, false}| ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok, Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
server_renegotiate() ->
    [{doc,"Test ssl:renegotiate/1 on server."}].
server_renegotiate(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From erlang to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE,
					       renegotiate, [Data]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, erlang_ssl_receive, [Data]}},
					{options, [{reuse_sessions, false} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
client_renegotiate_reused_session() ->
    [{doc,"Test ssl:renegotiate/1 on client when the ssl session will be reused."}].
client_renegotiate_reused_session(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From erlang to erlang",

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {?MODULE, erlang_ssl_receive, [Data]}},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       renegotiate_reuse_session, [Data]}},
					{options, [{reuse_sessions, true} | ClientOpts]}]),
    
    ssl_test_lib:check_result(Client, ok, Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
server_renegotiate_reused_session() ->
    [{doc,"Test ssl:renegotiate/1 on server when the ssl session will be reused."}].
server_renegotiate_reused_session(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Data = "From erlang to erlang",

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {?MODULE,
					       renegotiate_reuse_session, [Data]}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE, erlang_ssl_receive, [Data]}},
					{options, [{reuse_sessions, true} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
client_no_wrap_sequence_number() ->
    [{doc,"Test that erlang client will renegotiate session when",
     "max sequence number celing is about to be reached. Although"
     "in the testcase we use the test option renegotiate_at"
     " to lower treashold substantially."}].

client_no_wrap_sequence_number(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    ErlData = "From erlang to erlang",
    N = 12,

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Version = ssl_test_lib:protocol_version(Config, tuple),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib,
					       trigger_renegotiate, [[ErlData, treashold(N, Version)]]}},
					{options, [{reuse_sessions, false},
						   {renegotiate_at, N} | ClientOpts]}]),

    ssl_test_lib:check_result(Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
server_no_wrap_sequence_number() ->
    [{doc, "Test that erlang server will renegotiate session when",
     "max sequence number celing is about to be reached. Although"
     "in the testcase we use the test option renegotiate_at"
     " to lower treashold substantially."}].

server_no_wrap_sequence_number(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Data = "From erlang to erlang",
    N = 12,

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib,
					       trigger_renegotiate, [[Data, N+2]]}},
					{options, [{renegotiate_at, N} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, no_result, []}},
					{options, [{reuse_sessions, false} | ClientOpts]}]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
renegotiate_dos_mitigate_active() ->
    [{doc, "Mitigate DOS computational attack by not allowing client to renegotiate many times in a row",
      "immediately after each other"}].
renegotiate_dos_mitigate_active(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result_active, []}},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       renegotiate_immediately, []}},
					{options, ClientOpts}]),

    ssl_test_lib:check_result(Client, ok, Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
renegotiate_dos_mitigate_passive() ->
    [{doc, "Mitigate DOS computational attack by not allowing client to renegotiate many times in a row",
      "immediately after each other"}].
renegotiate_dos_mitigate_passive(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options, [{active, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
 
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, 
					       renegotiate_immediately, []}},
					{options, ClientOpts}]),
    
    ssl_test_lib:check_result(Client, ok, Server, ok), 
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
renegotiate_dos_mitigate_absolute() ->
    [{doc, "Mitigate DOS computational attack by not allowing client to initiate renegotiation"}].
renegotiate_dos_mitigate_absolute(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, send_recv_result_active, []}},
				   {options, [{client_renegotiation, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {?MODULE,
					       renegotiate_rejected,
					       []}},
					{options, ClientOpts}]),

    ssl_test_lib:check_result(Client, ok, Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
active_error_disallowed_client_renegotiate() ->
    [{doc,"Test that an active client socket gets an error when server denies client renegotiation."}].
active_error_disallowed_client_renegotiate(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options, [{client_renegotiation, false} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    {ok, Client} = ssl:connect(Hostname, Port, [{renegotiate_at, 1}, {active, true} | ClientOpts]),

    {error, closed} = ssl:send(Client, crypto:strong_rand_bytes(20)),

    receive
        {ssl_error, Client, _} ->
            ok
    end.

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
renegotiate(Socket, Data) ->
    ?CT_LOG("Renegotiating ~n", []),
    Result = ssl:renegotiate(Socket),
    ?CT_LOG("Result ~p~n", [Result]),
    ssl:send(Socket, Data),
    case Result of
	ok ->
	    ok;
	Other ->
	    Other
    end.

renegotiate_reuse_session(Socket, Data) ->
    %% Make sure session is registered
    ct:sleep(?SLEEP),
    renegotiate(Socket, Data).

renegotiate_immediately(Socket) ->
    _ = ssl_test_lib:active_recv(Socket, 11),
    ok = ssl:renegotiate(Socket),  
    {error, renegotiation_rejected} = ssl:renegotiate(Socket),
    ct:sleep(?RENEGOTIATION_DISABLE_TIME + ?SLEEP),
    ok = ssl:renegotiate(Socket),
    ?CT_LOG("Renegotiated again"),
    ssl:send(Socket, "Hello world"),
    ok.

renegotiate_rejected(Socket) ->
    _ = ssl_test_lib:active_recv(Socket, 11),
    {error, renegotiation_rejected} = ssl:renegotiate(Socket),
    {error, renegotiation_rejected} = ssl:renegotiate(Socket),
    ct:sleep(?RENEGOTIATION_DISABLE_TIME +1),
    {error, renegotiation_rejected} = ssl:renegotiate(Socket),
    ?CT_LOG("Failed to renegotiate again"),
    ssl:send(Socket, "Hello world"),
    ok.

%% First two clauses handles 1/n-1 splitting countermeasure Rizzo/Duong-Beast
treashold(N, ?TLS_1_0) ->
    (N div 2) + 1;
treashold(N, _) ->
    N + 1.

erlang_ssl_receive(Socket, Data) ->
    case ssl_test_lib:active_recv(Socket, length(Data)) of
        Data ->
            ok;
        Other ->
            ct:fail({{expected, Data}, {got, Other}})
    end.
