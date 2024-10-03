%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2023. All Rights Reserved.
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
-module(ssl_alpn_SUITE).

-behaviour(ct_suite).

-include("ssl_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").

%% Callback functions
-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Testcases
-export([empty_protocols_are_not_allowed/1,
         protocols_must_be_a_binary_list/1,
         empty_client/1,
         empty_server/1,
         empty_client_empty_server/1,
         no_matching_protocol/1,
         client_alpn_and_server_alpn/1,
         client_alpn_and_server_no_support/1,
         client_no_support_and_server_alpn/1,
         client_renegotiate/1,
         session_reused/1,
         client_alpn_npn_and_server_alpn_npn/1,
         client_alpn_and_server_alpn_npn/1,
         client_alpn_npn_and_server_alpn/1
        ]).

%% Apply export

-export([assert_alpn/2,
         assert_alpn_and_renegotiate_and_send_data/3,
         ssl_send_and_assert_alpn/3,
         ssl_receive_and_assert_alpn/3,
         ssl_send/2,
         ssl_receive/2,
         connection_info_result/1
        ]).

-define(SLEEP, 500).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [
     {group, 'tlsv1.3'},
     {group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'dtlsv1.2'},
     {group, 'dtlsv1'}
    ].

groups() ->
    [
     {'tlsv1.3', [], alpn_tests() -- [client_renegotiate, session_reused]},
     {'tlsv1.2', [], alpn_tests() ++ alpn_npn_coexist()},
     {'tlsv1.1', [], alpn_tests() ++ alpn_npn_coexist()},
     {'tlsv1', [], alpn_tests() ++ alpn_npn_coexist()},
     {'dtlsv1.2', [], alpn_tests() ++ alpn_npn_coexist()},
     {'dtlsv1', [], alpn_tests() ++ alpn_npn_coexist()}
    ].

alpn_tests() ->
    [empty_protocols_are_not_allowed,
     protocols_must_be_a_binary_list,
     empty_client,
     empty_server,
     empty_client_empty_server,
     no_matching_protocol,
     client_alpn_and_server_alpn,
     client_alpn_and_server_no_support,
     client_no_support_and_server_alpn,
     client_renegotiate,
     session_reused
    ].

alpn_npn_coexist() ->
    [
     client_alpn_npn_and_server_alpn_npn,
     client_alpn_and_server_alpn_npn,
     client_alpn_npn_and_server_alpn
    ].


init_per_suite(Config0) ->
    catch application:stop(crypto),
    try application:start(crypto) of
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
    ssl_test_lib:init_per_group(GroupName, Config). 

end_per_group(GroupName, Config) ->
  ssl_test_lib:end_per_group(GroupName, Config).


init_per_testcase(_TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 10}),
    Config.

end_per_testcase(_TestCase, Config) ->     
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

empty_protocols_are_not_allowed(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {error, {options, {alpn_preferred_protocols, {invalid_protocol, <<>>}}}}
	= (catch ssl:listen(9443,
			    [{alpn_preferred_protocols, [<<"foo/1">>, <<"">>]}| ServerOpts])),
    {error, {options, {alpn_advertised_protocols, {invalid_protocol, <<>>}}}}
	= (catch ssl:connect({127,0,0,1}, 9443,
			     [{alpn_advertised_protocols, [<<"foo/1">>, <<"">>]} | ServerOpts])).

%--------------------------------------------------------------------------------

protocols_must_be_a_binary_list(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    Option1 = {alpn_preferred_protocols, hello},
    {error, {options, Option1}} = (catch ssl:listen(9443, [Option1 | ServerOpts])),
    Option2 = {alpn_preferred_protocols, [<<"foo/1">>, hello]},
    {error, {options, {alpn_preferred_protocols, {invalid_protocol, hello}}}}
        = (catch ssl:listen(9443, [Option2 | ServerOpts])),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    Option3 = {alpn_advertised_protocols, hello},
    {error, {options, Option3}} = (catch ssl:connect({127,0,0,1}, 9443, [Option3 | ClientOpts])),
    Option4 = {alpn_advertised_protocols, [<<"foo/1">>, hello]},
    {error, {options, {alpn_advertised_protocols, {invalid_protocol, hello}}}}
        = (catch ssl:connect({127,0,0,1}, 9443, [Option4 | ClientOpts])).

%--------------------------------------------------------------------------------

empty_client(Config) when is_list(Config) ->
    run_failing_handshake(Config,
                          [{alpn_advertised_protocols, []}],
                          [{alpn_preferred_protocols, [<<"spdy/2">>, <<"spdy/3">>, <<"http/2">>]}],
                          no_application_protocol).

%--------------------------------------------------------------------------------

empty_server(Config) when is_list(Config) ->
    run_failing_handshake(Config,
                          [{alpn_advertised_protocols, [<<"http/1.0">>, <<"http/1.1">>]}],
                          [{alpn_preferred_protocols, []}],
                          no_application_protocol).

%--------------------------------------------------------------------------------

empty_client_empty_server(Config) when is_list(Config) ->
    run_failing_handshake(Config,
                          [{alpn_advertised_protocols, []}],
                          [{alpn_preferred_protocols, []}],
                          no_application_protocol).

%--------------------------------------------------------------------------------

no_matching_protocol(Config) when is_list(Config) ->
    run_failing_handshake(Config,
                          [{alpn_advertised_protocols, [<<"http/1.0">>, <<"http/1.1">>]}],
                          [{alpn_preferred_protocols, [<<"spdy/2">>, <<"spdy/3">>, <<"http/2">>]}],
                          no_application_protocol).

%--------------------------------------------------------------------------------

client_alpn_and_server_alpn(Config) when is_list(Config) ->
    run_handshake(Config,
                  [{alpn_advertised_protocols, [<<"http/1.0">>, <<"http/1.1">>]}],
                  [{alpn_preferred_protocols, [<<"spdy/2">>, <<"http/1.1">>, <<"http/1.0">>]}],
                  {ok, <<"http/1.1">>}).

%--------------------------------------------------------------------------------

client_alpn_and_server_no_support(Config) when is_list(Config) ->
    run_handshake(Config,
		    [{alpn_advertised_protocols, [<<"http/1.0">>, <<"http/1.1">>]}],
		    [],
		    {error, protocol_not_negotiated}).

%--------------------------------------------------------------------------------

client_no_support_and_server_alpn(Config) when is_list(Config) ->
    run_handshake(Config,
		    [],
		    [{alpn_preferred_protocols, [<<"spdy/2">>, <<"http/1.1">>, <<"http/1.0">>]}],
		    {error, protocol_not_negotiated}).

%--------------------------------------------------------------------------------

client_alpn_npn_and_server_alpn(Config) when is_list(Config) ->
    run_handshake(Config,
		    [{alpn_advertised_protocols, [<<"http/1.0">>, <<"http/1.1">>]},
		        {client_preferred_next_protocols, {client, [<<"spdy/2">>], <<"spdy/3">>}}],
		    [{alpn_preferred_protocols, [<<"spdy/2">>, <<"http/1.1">>, <<"http/1.0">>]}],
		    {ok, <<"http/1.1">>}).

%--------------------------------------------------------------------------------

client_alpn_npn_and_server_alpn_npn(Config) when is_list(Config) ->
    run_handshake(Config,
		    [{alpn_advertised_protocols, [<<"http/1.0">>, <<"http/1.1">>]},
		        {client_preferred_next_protocols, {client, [<<"spdy/2">>], <<"spdy/3">>}}],
		    [{alpn_preferred_protocols, [<<"spdy/2">>, <<"http/1.1">>, <<"http/1.0">>]},
		        {next_protocols_advertised, [<<"spdy/2">>, <<"http/1.0">>]}],
		    {ok, <<"http/1.1">>}).

%--------------------------------------------------------------------------------

client_alpn_and_server_alpn_npn(Config) when is_list(Config) ->
    run_handshake(Config,
		    [{alpn_advertised_protocols, [<<"http/1.0">>, <<"http/1.1">>]}],
		    [{alpn_preferred_protocols, [<<"spdy/2">>, <<"http/1.1">>, <<"http/1.0">>]},
		        {next_protocols_advertised, [<<"spdy/2">>, <<"http/1.0">>]}],
		    {ok, <<"http/1.1">>}).

%--------------------------------------------------------------------------------

client_renegotiate(Config) when is_list(Config) ->
    Data = "hello world",
    
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ClientOpts = [{alpn_advertised_protocols, [<<"http/1.0">>]}] ++ ClientOpts0,
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ServerOpts = [{alpn_preferred_protocols, [<<"spdy/2">>, <<"http/1.1">>, <<"http/1.0">>]}] ++  ServerOpts0,
    ExpectedProtocol = {ok, <<"http/1.0">>},

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                    {from, self()},
                    {mfa, {?MODULE, ssl_receive_and_assert_alpn, [ExpectedProtocol, Data]}},
                    {options, ServerOpts}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
               {host, Hostname},
               {from, self()},
               {mfa, {?MODULE, assert_alpn_and_renegotiate_and_send_data, [ExpectedProtocol, Data]}},
               {options, ClientOpts}]),

    ssl_test_lib:check_result(Server, ok, Client, ok).

%--------------------------------------------------------------------------------

session_reused(Config) when  is_list(Config)->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ClientOpts = [{alpn_advertised_protocols, [<<"http/1.0">>]}] ++ ClientOpts0,
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ServerOpts = [{alpn_preferred_protocols, [<<"spdy/2">>, <<"http/1.1">>, <<"http/1.0">>]}] ++  ServerOpts0,

    ssl_test_lib:reuse_session(ClientOpts, ServerOpts, Config).

%%--------------------------------------------------------------------
%% callback functions ------------------------------------------------
%%--------------------------------------------------------------------

assert_alpn(Socket, Protocol) ->
    ?CT_LOG("Negotiated Protocol ~p, Expecting: ~p ~n",
		       [ssl:negotiated_protocol(Socket), Protocol]),
    Protocol = ssl:negotiated_protocol(Socket).

assert_alpn_and_renegotiate_and_send_data(Socket, Protocol, Data) ->
    assert_alpn(Socket, Protocol),
    ?CT_LOG("Renegotiating ~n", []),
    ok = ssl:renegotiate(Socket),
    ssl:send(Socket, Data),
    assert_alpn(Socket, Protocol),
    ok.

ssl_send_and_assert_alpn(Socket, Protocol, Data) ->
    assert_alpn(Socket, Protocol),
    ssl_send(Socket, Data).

ssl_receive_and_assert_alpn(Socket, Protocol, Data) ->
    assert_alpn(Socket, Protocol),
    ssl_receive(Socket, Data).

ssl_send(Socket, Data) ->
    ?CT_LOG("Connection info: ~p~n",
               [ssl:connection_information(Socket)]),
    ssl:send(Socket, Data).

ssl_receive(Socket, Data) ->
    ssl_receive(Socket, Data, []).

ssl_receive(Socket, Data, Buffer) ->
    ?CT_LOG("Connection info: ~p~n",
           [ssl:connection_information(Socket)]),
    receive
    {ssl, Socket, MoreData} ->
        ?CT_LOG("Received ~p~n",[MoreData]),
        NewBuffer = Buffer ++ MoreData,
        case NewBuffer of
            Data ->
                ssl:send(Socket, "Got it"),
                ok;
            _ ->
                ssl_receive(Socket, Data, NewBuffer)
        end;
    Other ->
        ct:fail({unexpected_message, Other})
    after 4000 ->
        ct:fail({did_not_get, Data})
    end.

connection_info_result(Socket) ->
    ssl:connection_information(Socket).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------

run_failing_handshake(Config, ClientExtraOpts, ServerExtraOpts, ExpectedAlert) ->
    ClientOpts = ClientExtraOpts ++ ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ServerExtraOpts ++ ssl_test_lib:ssl_options(server_rsa_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
                    {from, self()},
                    {mfa, {?MODULE, placeholder, []}},
                    {options, ServerOpts}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
                                           {host, Hostname},
                                           {from, self()},
                                           {mfa, {?MODULE, placeholder, []}},
                                           {options, ClientOpts}]),
    ssl_test_lib:check_client_alert(Server, Client, ExpectedAlert).

run_handshake(Config, ClientExtraOpts, ServerExtraOpts, ExpectedProtocol) ->
    Data = "hello world",

    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ClientOpts = ClientExtraOpts ++ ClientOpts0,
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ServerOpts = ServerExtraOpts ++  ServerOpts0,

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                    {from, self()},
                    {mfa, {?MODULE, ssl_receive_and_assert_alpn, [ExpectedProtocol, Data]}},
                    {options, ServerOpts}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
               {host, Hostname},
               {from, self()},
               {mfa, {?MODULE, ssl_send_and_assert_alpn, [ExpectedProtocol, Data]}},
               {options, ClientOpts}]),

    ssl_test_lib:check_result(Server, ok, Client, ok).
