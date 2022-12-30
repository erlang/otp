-module(ssl_options_SUITE).

-behaviour(ct_suite).

-include_lib("common_test/include/ct.hrl").
-include_lib("ssl/src/ssl_api.hrl").
-include_lib("public_key/include/public_key.hrl").

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

%% Callbacks
-export([log/2]).

%% Test cases
-export([warn_verify_none/0,
         warn_verify_none/1]).


%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [
     {group, 'client'}
    ].

groups() ->
    [
        {'client', [], [warn_verify_none]}
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
    case ssl_test_lib:is_protocol_version(GroupName) of
        true  ->
            ssl_test_lib:init_per_group(GroupName, 
                                        [{client_type, erlang},
                                         {server_type, erlang},
                                         {version, GroupName}
                                        | Config]);
        false -> 
            Config
    end.

end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(prf, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 10}),
    [Config];
init_per_testcase(handshake_continue_tls13_client, Config) ->
    case ssl_test_lib:sufficient_crypto_support('tlsv1.3') of
        true ->
            ssl_test_lib:ct_log_supported_protocol_versions(Config),
            ct:timetrap({seconds, 10}),
            Config;
        false ->
            {skip, "Missing crypto support: TLS 1.3 not supported"}
    end;
init_per_testcase(connection_information_with_srp, Config) ->
    PKAlg = proplists:get_value(public_keys, crypto:supports()),
    case lists:member(srp, PKAlg) of
        true ->
            Config;
        false ->
            {skip, "Missing SRP crypto support"}
    end;
init_per_testcase(conf_signature_algs, Config) ->
    case ssl_test_lib:appropriate_sha(crypto:supports()) of
        sha256 ->
            ssl_test_lib:ct_log_supported_protocol_versions(Config),
            ct:timetrap({seconds, 10}),
            Config;
        sha ->
            {skip, "Tests needs certs with sha256"}
    end;
init_per_testcase(check_random_nonce, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 20}),
    Config;
init_per_testcase(select_best_cert, Config) ->
    ct:timetrap({seconds, 10}),
    Version = ssl_test_lib:protocol_version(Config),
    %% We need to make sure TLS-1.3 can be supported as
    %% want to generate a TLS-1.3 specific certificate that will not
    %% be chosen
    case Version of
        'tlsv1.2' ->
              case ssl_test_lib:sufficient_crypto_support('tlsv1.3') of
                  true ->
                      Config;
                  false ->
                      {skip, "Crypto does not support EDDSA"}
              end;
        _ ->
            Config
    end;
init_per_testcase(_TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 10}),
    Config.

end_per_testcase(internal_active_n, _Config) ->
    application:unset_env(ssl, internal_active_n);
end_per_testcase(_TestCase, Config) ->     
    Config.

%%--------------------------------------------------------------------

log(#{msg := {report, #{description := "Server authenticity is not verified since certificate path validation is not enabled"}}}, #{config := TestPid}) ->
    TestPid ! "catched",
    ok;
log(_,_) ->
    ok.

warn_verify_none() ->
    [{doc,"Test to verify that a log is not emitted if warn_verify_none is set "
        "to true in client config"}].
warn_verify_none(Config) when is_list(Config) -> 
    ClientOpts1 = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ClientOpts = ClientOpts1 ++ [{warn_verify_none, false}],
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = 
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
				   {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options,  [{active, false}, {signature_algs, [{sha256, rsa}]}, 
                                               {versions, ['tlsv1.2']} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),
    logger:add_handler(mylog, ssl_options_SUITE, #{
        id => mylog,
        config => self(),
        level => debug,
        module => ssl_options_SUITE
    }),
    Client = 
	ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
				   {host, Hostname},
				   {from, self()}, 
				   {mfa, {ssl_test_lib, send_recv_result, []}},
				   {options, [{active, false}, {signature_algs, [{sha256, rsa}]},
                                              {versions, ['tlsv1.2']} | ClientOpts]}]),
    
    message_not_emitted = receive 
        "catched" ->
            catched
        after 
            100 -> message_not_emitted
    end,

    ssl_test_lib:check_result(Server, ok, Client, ok),
    
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).