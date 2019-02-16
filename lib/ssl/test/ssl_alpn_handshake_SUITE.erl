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

%%
-module(ssl_alpn_handshake_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

-define(SLEEP, 500).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [{group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'sslv3'},
     {group, 'dtlsv1.2'},
     {group, 'dtlsv1'}
    ].

groups() ->
    [
     {'tlsv1.2', [], alpn_tests()},
     {'tlsv1.1', [], alpn_tests()},
     {'tlsv1', [], alpn_tests()},
     {'sslv3', [], alpn_not_supported()},
     {'dtlsv1.2', [], alpn_tests() -- [client_renegotiate]},
     {'dtlsv1', [], alpn_tests() -- [client_renegotiate]}
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
     client_alpn_npn_and_server_alpn,
     client_alpn_npn_and_server_alpn_npn,
     client_alpn_and_server_alpn_npn,
     client_renegotiate,
     session_reused
    ].

alpn_not_supported() ->
    [alpn_not_supported_client,
     alpn_not_supported_server
    ].

init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
	    ssl_test_lib:clean_start(),
	    Config = ssl_test_lib:make_rsa_cert(Config0),
	    ssl_test_lib:cert_options(Config)
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
%--------------------------------------------------------------------------------

alpn_not_supported_client(Config) when is_list(Config) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    PrefProtocols = {client_preferred_next_protocols,
		     {client, [<<"http/1.0">>], <<"http/1.1">>}},
    ClientOpts = [PrefProtocols] ++ ClientOpts0,
    {ClientNode, _ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Client = ssl_test_lib:start_client_error([{node, ClientNode}, 
			    {port, 8888}, {host, Hostname},
			    {from, self()},  {options, ClientOpts}]),
    
    ssl_test_lib:check_result(Client, {error, 
				       {options, 
					{not_supported_in_sslv3, PrefProtocols}}}).

%--------------------------------------------------------------------------------

alpn_not_supported_server(Config) when is_list(Config)->
    ServerOpts0 =  ssl_test_lib:ssl_options(server_rsa_opts, Config),
    AdvProtocols = {next_protocols_advertised, [<<"spdy/2">>, <<"http/1.1">>, <<"http/1.0">>]},
    ServerOpts = [AdvProtocols] ++  ServerOpts0,
  
    {error, {options, {not_supported_in_sslv3, AdvProtocols}}} = ssl:listen(0, ServerOpts).

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

assert_alpn(Socket, Protocol) ->
    ct:log("Negotiated Protocol ~p, Expecting: ~p ~n",
		       [ssl:negotiated_protocol(Socket), Protocol]),
    Protocol = ssl:negotiated_protocol(Socket).

assert_alpn_and_renegotiate_and_send_data(Socket, Protocol, Data) ->
    assert_alpn(Socket, Protocol),
    ct:log("Renegotiating ~n", []),
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
    ct:log("Connection info: ~p~n",
               [ssl:connection_information(Socket)]),
    ssl:send(Socket, Data).

ssl_receive(Socket, Data) ->
    ssl_receive(Socket, Data, []).

ssl_receive(Socket, Data, Buffer) ->
    ct:log("Connection info: ~p~n",
               [ssl:connection_information(Socket)]),
    receive
    {ssl, Socket, MoreData} ->
        ct:log("Received ~p~n",[MoreData]),
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
