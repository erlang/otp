%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2012. All Rights Reserved.
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
-module(ssl_npn_handshake_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

init_per_suite(Config) ->
    try crypto:start() of
	ok ->
	    application:start(public_key),
	    ssl:start(),
	    Result =
		(catch make_certs:all(?config(data_dir, Config),
				      ?config(priv_dir, Config))),
	    test_server:format("Make certs  ~p~n", [Result]),
	    ssl_test_lib:cert_options(Config)
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).


all() ->
    [validate_empty_protocols_are_not_allowed_test,
     validate_empty_advertisement_list_is_allowed_test,
     validate_advertisement_must_be_a_binary_list_test,
     validate_client_protocols_must_be_a_tuple_test,
     perform_normal_npn_handshake_server_preference_test,
     perform_normal_npn_handshake_client_preference_test,
     perform_fallback_npn_handshake_test,
     perform_fallback_npn_handshake_server_preference_test,
     perform_client_tries_to_negotiate_but_server_does_not_support_test,
     perform_client_does_not_try_to_negotiate_but_server_supports_npn_test,
     perform_renegotiate_from_client_after_npn_handshake].

connection_info_result(Socket) ->
    ssl:connection_info(Socket).

validate_empty_protocols_are_not_allowed_test(_Config) ->
    {error, {eoptions, {next_protocols_advertised, {invalid_protocol, <<>>}}}}
	= (catch ssl:listen(9443,
			    [{next_protocols_advertised, [<<"foo/1">>, <<"">>]}])),
    {error, {eoptions, {client_preferred_next_protocols, {invalid_protocol, <<>>}}}}
	= (catch ssl:connect({127,0,0,1}, 9443,
			     [{client_preferred_next_protocols,
			       {client, [<<"foo/1">>, <<"">>], <<"foox/1">>}}], infinity)),
    Option = {client_preferred_next_protocols, {invalid_protocol, <<"">>}},
    {error, {eoptions, Option}} = (catch ssl:connect({127,0,0,1}, 9443, [Option], infinity)).

validate_empty_advertisement_list_is_allowed_test(_Config) ->
    Option = {next_protocols_advertised, []},
    {ok, Socket} = ssl:listen(0, [Option]),
    ssl:close(Socket).

validate_advertisement_must_be_a_binary_list_test(_Config) ->
    Option = {next_protocols_advertised, blah},
    {error, {eoptions, Option}} = (catch ssl:listen(9443, [Option])).

validate_client_protocols_must_be_a_tuple_test(_Config) ->
    Option = {client_preferred_next_protocols, [<<"foo/1">>]},
    {error, {eoptions, Option}} = (catch ssl:connect({127,0,0,1}, 9443, [Option])).



perform_client_does_not_try_to_negotiate_but_server_supports_npn_test(Config) ->
    run_npn_handshake_test(Config,
        [],
        [{next_protocols_advertised, [<<"spdy/1">>, <<"http/1.1">>, <<"http/1.0">>]}],
        {error, next_protocol_not_negotiated}).

perform_client_tries_to_negotiate_but_server_does_not_support_test(Config) ->
    run_npn_handshake_test(Config,
        [{client_preferred_next_protocols, {client, [<<"spdy/2">>], <<"http/1.1">>}}],
        [],
        {error, next_protocol_not_negotiated}).

perform_fallback_npn_handshake_test(Config) ->
    run_npn_handshake_test(Config,
        [{client_preferred_next_protocols, {client, [<<"spdy/2">>], <<"http/1.1">>}}],
        [{next_protocols_advertised, [<<"spdy/1">>, <<"http/1.1">>, <<"http/1.0">>]}],
        {ok, <<"http/1.1">>}).

perform_fallback_npn_handshake_server_preference_test(Config) ->
    run_npn_handshake_test(Config,
        [{client_preferred_next_protocols, {server, [<<"spdy/2">>], <<"http/1.1">>}}],
        [{next_protocols_advertised, [<<"spdy/1">>, <<"http/1.1">>, <<"http/1.0">>]}],
        {ok, <<"http/1.1">>}).


perform_normal_npn_handshake_client_preference_test(Config) ->
    run_npn_handshake_test(Config,
        [{client_preferred_next_protocols,
	  {client, [<<"http/1.0">>, <<"http/1.1">>], <<"http/1.1">>}}],
        [{next_protocols_advertised, [<<"spdy/2">>, <<"http/1.1">>, <<"http/1.0">>]}],
        {ok, <<"http/1.0">>}).

perform_normal_npn_handshake_server_preference_test(Config) ->
    run_npn_handshake_test(Config,
        [{client_preferred_next_protocols,
	  {server, [<<"http/1.0">>, <<"http/1.1">>], <<"http/1.1">>}}],
        [{next_protocols_advertised, [<<"spdy/2">>, <<"http/1.1">>, <<"http/1.0">>]}],
        {ok, <<"http/1.1">>}).


perform_renegotiate_from_client_after_npn_handshake(Config) ->
    Data = "hello world",

    ClientOpts0 = ?config(client_opts, Config),
    ClientOpts = [{client_preferred_next_protocols,
		   {client, [<<"http/1.0">>], <<"http/1.1">>}}] ++ ClientOpts0,
    ServerOpts0 = ?config(server_opts, Config),
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

run_npn_handshake_test(Config, ClientExtraOpts, ServerExtraOpts, ExpectedProtocol) ->
    Data = "hello world",

    ClientOpts0 = ?config(client_opts, Config),
    ClientOpts = ClientExtraOpts ++ ClientOpts0,
    ServerOpts0 = ?config(server_opts, Config),
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

assert_npn(Socket, Protocol) ->
    test_server:format("Negotiated Protocol ~p, Expecting: ~p ~n",
		       [ssl:negotiated_next_protocol(Socket), Protocol]),
    Protocol = ssl:negotiated_next_protocol(Socket).

assert_npn_and_renegotiate_and_send_data(Socket, Protocol, Data) ->
    assert_npn(Socket, Protocol),
    test_server:format("Renegotiating ~n", []),
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
    test_server:format("Connection info: ~p~n",
               [ssl:connection_info(Socket)]),
    ssl:send(Socket, Data).

ssl_receive(Socket, Data) ->
    ssl_receive(Socket, Data, []).

ssl_receive(Socket, Data, Buffer) ->
    test_server:format("Connection info: ~p~n",
               [ssl:connection_info(Socket)]),
    receive
    {ssl, Socket, MoreData} ->
        test_server:format("Received ~p~n",[MoreData]),
        NewBuffer = Buffer ++ MoreData,
        case NewBuffer of
            Data ->
                ssl:send(Socket, "Got it"),
                ok;
            _ ->
                ssl_receive(Socket, Data, NewBuffer)
        end;
    Other ->
        test_server:fail({unexpected_message, Other})
    after 4000 ->
        test_server:fail({did_not_get, Data})
    end.
