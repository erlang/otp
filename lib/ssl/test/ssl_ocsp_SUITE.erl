%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2020. All Rights Reserved.
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

-module(ssl_ocsp_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() -> 
    [{group, 'tlsv1.2'},
     {group, 'tlsv1.3'},
     {group, 'dtlsv1.2'}].

groups() -> 
    [{'tlsv1.2', [], ocsp_tests()},
     {'tlsv1.3', [], ocsp_tests()},
     {'dtlsv1.2', [], ocsp_tests()}].

ocsp_tests() ->
    [ocsp_stapling_basic,
     ocsp_stapling_with_nonce,
     ocsp_stapling_with_responder_cert,
     ocsp_stapling_revoked
    ].

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
        ssl_test_lib:clean_start(),
        DataDir = proplists:get_value(data_dir, Config),
        PrivDir = proplists:get_value(priv_dir, Config),

        %% Prepare certs
        {ok, _} = make_certs:all(DataDir, PrivDir),

        ResponderPort = get_free_port(),
        Pid =
            start_ocsp_responder(
                erlang:integer_to_list(ResponderPort), PrivDir),

        NewConfig =
        lists:merge(
            [{responder_port, ResponderPort},
             {responder_pid, Pid}
            ], Config),

	    ssl_test_lib:cert_options(NewConfig)
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.


end_per_suite(Config) ->
    ResponderPid = proplists:get_value(responder_pid, Config),
    stop_ocsp_responder(ResponderPid),
    ok = ssl:stop(),
    application:stop(crypto).

%%--------------------------------------------------------------------
init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group(GroupName, [{group, GroupName} | Config]).

end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, proplists:delete(group, Config)).

%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 10}),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

ocsp_stapling_basic() ->
    [{doc, "Verify OCSP stapling works without nonce "
           "and responder certs."}].
ocsp_stapling_basic(Config)
  when is_list(Config) ->
    Data = "ping",  %% 4 bytes
    GroupName = proplists:get_value(group, Config),
    ServerOpts = [{log_level, debug},
                  {group, GroupName}],
    Server = ssl_test_lib:start_server(openssl_ocsp,
                                       [{options, ServerOpts}], Config),
    Port = ssl_test_lib:inet_port(Server),

    ClientOpts = [{log_level, debug},
                  {ocsp_stapling, true},
                  {ocsp_nonce, false}] ++ dtls_client_opt(GroupName),
    Client = ssl_test_lib:start_client(erlang,
                                       [{port, Port},
                                        {options, ClientOpts}], Config),
    ssl_test_lib:send(Server, Data),
    Data = ssl_test_lib:check_active_receive(Client, Data),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

ocsp_stapling_with_nonce() ->
    [{doc, "Verify OCSP stapling works with nonce."}].
ocsp_stapling_with_nonce(Config)
  when is_list(Config) ->
    Data = "ping",  %% 4 bytes
    GroupName = proplists:get_value(group, Config),
    ServerOpts = [{log_level, debug},
                  {group, GroupName}],
    Server = ssl_test_lib:start_server(openssl_ocsp,
                                       [{options, ServerOpts}], Config),
    Port = ssl_test_lib:inet_port(Server),

    ClientOpts = [{log_level, debug},
                  {ocsp_stapling, true},
                  {ocsp_nonce, true}] ++ dtls_client_opt(GroupName),
    Client = ssl_test_lib:start_client(erlang,
                                       [{port, Port},
                                        {options, ClientOpts}], Config),
    ssl_test_lib:send(Server, Data),
    Data = ssl_test_lib:check_active_receive(Client, Data),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

ocsp_stapling_with_responder_cert() ->
    [{doc, "Verify OCSP stapling works with nonce "
           "and responder certs."}].
ocsp_stapling_with_responder_cert(Config)
  when is_list(Config) ->
    Data = "ping",  %% 4 bytes
    GroupName = proplists:get_value(group, Config),
    ServerOpts = [{log_level, debug},
                  {group, GroupName}],
    Server = ssl_test_lib:start_server(openssl_ocsp,
                                       [{options, ServerOpts}], Config),
    Port = ssl_test_lib:inet_port(Server),

    PrivDir = proplists:get_value(priv_dir, Config),
    {ok, ResponderCert} =
        file:read_file(filename:join(PrivDir, "b.server/cert.pem")),
    [{'Certificate', Der, _IsEncrypted}] =
        public_key:pem_decode(ResponderCert),

    ClientOpts = [{log_level, debug},
                  {ocsp_stapling, true},
                  {ocsp_nonce, true},
                  {ocsp_responder_certs, [Der]}] ++ dtls_client_opt(GroupName),
    Client = ssl_test_lib:start_client(erlang,
                                       [{port, Port},
                                        {options, ClientOpts}], Config),
    ssl_test_lib:send(Server, Data),
    Data = ssl_test_lib:check_active_receive(Client, Data),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

ocsp_stapling_revoked() ->
    [{doc, "Verify OCSP stapling works with revoked certificate."}].
ocsp_stapling_revoked(Config)
  when is_list(Config) ->
    GroupName = proplists:get_value(group, Config),
    ServerOpts = [{log_level, debug},
                  {group, GroupName}],
    Server = ssl_test_lib:start_server(openssl_ocsp_revoked,
                                       [{options, ServerOpts}], Config),
    Port = ssl_test_lib:inet_port(Server),

    ClientOpts = [{log_level, debug},
                  {ocsp_stapling, true},
                  {ocsp_nonce, true},
                  {verify, verify_peer}] ++ dtls_client_opt(GroupName),
    {connect_failed, {tls_alert, {certificate_revoked, _RevokedInfo}}} =
    ssl_test_lib:start_client(erlang,
                              [{port, Port},
                               {options, ClientOpts}], Config),

    ssl_test_lib:close(Server).

%%--------------------------------------------------------------------
%% Intrernal functions -----------------------------------------------
%%--------------------------------------------------------------------
start_ocsp_responder(ResponderPort, PrivDir) ->
    erlang:spawn(
        ?MODULE, do_start_ocsp_responder, [ResponderPort, PrivDir]).

do_start_ocsp_responder(ResponderPort, PrivDir) ->
    Index = filename:join(PrivDir, "otpCA/index.txt"),
    CACerts = filename:join(PrivDir, "b.server/cacerts.pem"),
    Cert = filename:join(PrivDir, "b.server/cert.pem"),
    Key = filename:join(PrivDir, "b.server/key.pem"),

    Args = ["ocsp", "-index", Index, "-CA", CACerts, "-rsigner", Cert,
            "-rkey", Key, "-port", ResponderPort],
    process_flag(trap_exit, true),
    SSLPort = ssl_test_lib:portable_open_port("openssl", Args),
    true = port_command(SSLPort, "Hello world"),
    ocsp_responder_loop(SSLPort).

ocsp_responder_loop(SSLPort) ->
    receive
	stop_ocsp_responder ->
	    ct:log("Shut down OCSP responder!~n"),
        ok = ssl_test_lib:close_port(SSLPort);
	{_Port, closed} ->
	    ct:log("Port Closed~n"),
	    ok;
	{'EXIT', _Port, Reason} ->
	    ct:log("Port Closed ~p~n",[Reason]),
	    ok;
	Msg ->
	    ct:log("Port Msg ~p~n",[Msg]),
	    ocsp_responder_loop(SSLPort)
    after 600000 ->
        ssl_test_lib:close_port(SSLPort)
    end.

stop_ocsp_responder(Pid) ->
    Pid ! stop_ocsp_responder.

get_free_port() ->
    {ok, Listen} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(Listen),
    ok = gen_tcp:close(Listen),
    Port.

do_test_ocsp_stapling(SOpts, COpts, Config) ->
    Data = "123456789012345",  %% 15 bytes
    Server = ssl_test_lib:start_server(openssl_ocsp,
                                       [{options, SOpts}], Config),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client(erlang,
                                       [{port, Port},
                                        {options, COpts}], Config),
    ssl_test_lib:send(Client, Data),
    Data = ssl_test_lib:check_active_receive(Server, Data),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

dtls_client_opt('dtlsv1.2') ->
    [{protocol, dtls}];
dtls_client_opt(_Other) ->
    [].