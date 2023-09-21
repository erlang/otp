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
-module(ssl_npn_SUITE).

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
-export([validate_empty_protocols_are_not_allowed/1,
         validate_empty_advertisement_list_is_allowed/1,
         validate_advertisement_must_be_a_binary_list/1,
         validate_client_protocols_must_be_a_tuple/1,
         normal_npn_handshake_server_preference/1,
         normal_npn_handshake_client_preference/1,
         fallback_npn_handshake/1,
         fallback_npn_handshake_server_preference/1,
         client_negotiate_server_does_not_support/1,
         no_client_negotiate_but_server_supports_npn/1,
         renegotiate_from_client_after_npn_handshake/1,
         npn_handshake_session_reused/1
        ]).

-export([assert_npn/2,
         assert_npn_and_renegotiate_and_send_data/3,
         ssl_send_and_assert_npn/3,
         ssl_send/2,
         ssl_receive/2,
         ssl_receive_and_assert_npn/3,
         connection_info_result/1
        ]).

-define(TIMEOUT, {seconds, 5}).
-define(SLEEP, 500).
%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [{group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'dtlsv1.2'},
     {group, 'dtlsv1'}
    ].

groups() ->
    [
     {'tlsv1.2', [], next_protocol_tests()},
     {'tlsv1.1', [], next_protocol_tests()},
     {'tlsv1', [], next_protocol_tests()},
     {'dtlsv1.2', [], next_protocol_tests()},
     {'dtlsv1', [], next_protocol_tests()}
    ].

next_protocol_tests() ->
    [validate_empty_protocols_are_not_allowed,
     validate_empty_advertisement_list_is_allowed,
     validate_advertisement_must_be_a_binary_list,
     validate_client_protocols_must_be_a_tuple,
     normal_npn_handshake_server_preference,
     normal_npn_handshake_client_preference,
     fallback_npn_handshake,
     fallback_npn_handshake_server_preference,
     client_negotiate_server_does_not_support,
     no_client_negotiate_but_server_supports_npn,
     renegotiate_from_client_after_npn_handshake,
     npn_handshake_session_reused
    ].

init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
            ssl_test_lib:clean_start(),
            ssl:clear_pem_cache(),
	    Config = ssl_test_lib:make_rsa_cert(Config0),
	    ssl_test_lib:cert_options(Config)
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

init_per_testcase(_TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    Version = proplists:get_value(version, Config),
    ?CT_LOG("Ciphers: ~p~n ", [ ssl:cipher_suites(default, Version)]),
    ct:timetrap(?TIMEOUT),
    Config.

end_per_testcase(_TestCase, Config) ->     
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

validate_empty_protocols_are_not_allowed(Config) when is_list(Config) ->
    {error, {options, {next_protocols_advertised, {invalid_protocol, <<>>}}}}
	= (catch ssl:listen(9443,
			    [{next_protocols_advertised, [<<"foo/1">>, <<"">>]}])),
    {error, {options, {client_preferred_next_protocols, {invalid_protocol, <<>>}}}}
	= (catch ssl:connect({127,0,0,1}, 9443,
			     [{verify, verify_none}, {client_preferred_next_protocols,
			       {client, [<<"foo/1">>, <<"">>], <<"foox/1">>}}], infinity)),
    Option = {client_preferred_next_protocols, {invalid_protocol, <<"">>}},
    {error, {options, Option}} = (catch ssl:connect({127,0,0,1}, 9443, 
                                                    [{verify, verify_none}, Option], infinity)).

%--------------------------------------------------------------------------------

validate_empty_advertisement_list_is_allowed(Config) when is_list(Config) ->
    Option = {next_protocols_advertised, []},
    {ok, Socket} = ssl:listen(0, [Option]),
    ssl:close(Socket).
%--------------------------------------------------------------------------------

validate_advertisement_must_be_a_binary_list(Config) when is_list(Config) ->
    Option = {next_protocols_advertised, blah},
    {error, {options, Option}} = (catch ssl:listen(9443, [{verify, verify_none}, Option])).
%--------------------------------------------------------------------------------

validate_client_protocols_must_be_a_tuple(Config) when is_list(Config)  ->
    Option = {client_preferred_next_protocols, [<<"foo/1">>]},
    {error, {options, Option}} = (catch ssl:connect({127,0,0,1}, 9443,
                                                    [{verify, verify_none}, Option])).

%--------------------------------------------------------------------------------

normal_npn_handshake_server_preference(Config) when is_list(Config) ->
    run_npn_handshake(Config,
			   [{client_preferred_next_protocols,
			     {server, [<<"http/1.0">>, <<"http/1.1">>], <<"http/1.1">>}}],
			   [{next_protocols_advertised, [<<"spdy/2">>, <<"http/1.1">>, <<"http/1.0">>]}],
			   {ok, <<"http/1.1">>}).
%--------------------------------------------------------------------------------

normal_npn_handshake_client_preference(Config) when is_list(Config) ->
    run_npn_handshake(Config,
			   [{client_preferred_next_protocols,
			     {client, [<<"http/1.0">>, <<"http/1.1">>], <<"http/1.1">>}}],
			   [{next_protocols_advertised, [<<"spdy/2">>, <<"http/1.1">>, <<"http/1.0">>]}],
			   {ok, <<"http/1.0">>}).

%--------------------------------------------------------------------------------

fallback_npn_handshake(Config) when is_list(Config) ->
    run_npn_handshake(Config,
			   [{client_preferred_next_protocols, {client, [<<"spdy/2">>], <<"http/1.1">>}}],
			   [{next_protocols_advertised, [<<"spdy/1">>, <<"http/1.1">>, <<"http/1.0">>]}],
			   {ok, <<"http/1.1">>}).
%--------------------------------------------------------------------------------

fallback_npn_handshake_server_preference(Config) when is_list(Config) ->
    run_npn_handshake(Config,
			   [{client_preferred_next_protocols, {server, [<<"spdy/2">>], <<"http/1.1">>}}],
			   [{next_protocols_advertised, [<<"spdy/1">>, <<"http/1.1">>, <<"http/1.0">>]}],
			   {ok, <<"http/1.1">>}).

%--------------------------------------------------------------------------------

no_client_negotiate_but_server_supports_npn(Config) when is_list(Config) ->
    run_npn_handshake(Config,
			   [],
			   [{next_protocols_advertised, [<<"spdy/1">>, <<"http/1.1">>, <<"http/1.0">>]}],
			   {error, protocol_not_negotiated}).
%--------------------------------------------------------------------------------


client_negotiate_server_does_not_support(Config) when is_list(Config) ->
    run_npn_handshake(Config,
			   [{client_preferred_next_protocols, {client, [<<"spdy/2">>], <<"http/1.1">>}}],
			   [],
			   {error, protocol_not_negotiated}).

%--------------------------------------------------------------------------------
renegotiate_from_client_after_npn_handshake(Config) when is_list(Config) ->
    Data = "hello world",
    
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ClientOpts = [{client_preferred_next_protocols,
		   {client, [<<"http/1.0">>], <<"http/1.1">>}}] ++ ClientOpts0,
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ServerOpts = [{next_protocols_advertised,
		   [<<"spdy/2">>, <<"http/1.1">>, <<"http/1.0">>]}] ++  ServerOpts0,
    ExpectedProtocol = {ok, <<"http/1.0">>},

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                    {from, self()},
                    {mfa, {?MODULE, ssl_receive_and_assert_npn, [ExpectedProtocol, Data]}},
                    {options, ServerOpts}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
               {host, Hostname},
               {from, self()},
               {mfa, {?MODULE, assert_npn_and_renegotiate_and_send_data, [ExpectedProtocol, Data]}},
               {options, ClientOpts}]),

    ssl_test_lib:check_result(Server, ok, Client, ok).

%--------------------------------------------------------------------------------
npn_handshake_session_reused(Config) when  is_list(Config)->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ClientOpts = [{client_preferred_next_protocols,
		   {client, [<<"http/1.0">>], <<"http/1.1">>}}] ++ ClientOpts0,
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ServerOpts =[{next_protocols_advertised,
		   [<<"spdy/2">>, <<"http/1.1">>, <<"http/1.0">>]}]  ++ ServerOpts0,

    ssl_test_lib:reuse_session(ClientOpts, ServerOpts, Config).
    

%%--------------------------------------------------------------------
%% callback functions ------------------------------------------------
%%--------------------------------------------------------------------

assert_npn(Socket, Protocol) ->
    ?CT_LOG("Negotiated Protocol ~p, Expecting: ~p ~n",
		       [ssl:negotiated_protocol(Socket), Protocol]),
    Protocol = ssl:negotiated_protocol(Socket).

assert_npn_and_renegotiate_and_send_data(Socket, Protocol, Data) ->
    assert_npn(Socket, Protocol),
    ?CT_LOG("Renegotiating ~n", []),
    ok = ssl:renegotiate(Socket),
    ssl:send(Socket, Data),
    assert_npn(Socket, Protocol),
    ok.

ssl_send_and_assert_npn(Socket, Protocol, Data) ->
    assert_npn(Socket, Protocol),
    ssl_send(Socket, Data).

ssl_receive_and_assert_npn(Socket, Protocol, Data) ->
    assert_npn(Socket, Protocol),
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
run_npn_handshake(Config, ClientExtraOpts, ServerExtraOpts, ExpectedProtocol) ->
    Data = "hello world",

    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ClientOpts = ClientExtraOpts ++ ClientOpts0,
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ServerOpts = ServerExtraOpts ++  ServerOpts0,

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                    {from, self()},
                    {mfa, {?MODULE, ssl_receive_and_assert_npn, [ExpectedProtocol, Data]}},
                    {options, ServerOpts}]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
               {host, Hostname},
               {from, self()},
               {mfa, {?MODULE, ssl_send_and_assert_npn, [ExpectedProtocol, Data]}},
               {options, ClientOpts}]),

    ssl_test_lib:check_result(Server, ok, Client, ok).
