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
    [{group, tls1_2},
     {group, tls1_3},
     {group, revoked_1_2},
     {group, revoked_1_3}].

groups() -> 
    [{tls1_2, [], tls1_2_tests()},
     {tls1_3, [], tls1_3_tests()},
     {revoked_1_2, [], tls1_2_revoked_tests()},
     {revoked_1_3, [], tls1_3_revoked_tests()}].

tls1_2_tests() ->
    [ocsp_stapling_without_nonce_and_responder_certs_tls1_2,
     ocsp_stapling_with_nonce_tls1_2,
     ocsp_stapling_with_responder_cert_tls1_2].

tls1_2_revoked_tests() ->
    [ocsp_stapling_revoked_tls1_2].

tls1_3_tests() ->
    [ocsp_stapling_without_nonce_and_responder_certs_tls1_3,
     ocsp_stapling_with_nonce_and_responder_certs_tls1_3].

tls1_3_revoked_tests() ->
    [ocsp_stapling_revoked_tls1_3].

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
init_per_group(tls1_2, Config) ->
    setup_tls_server_for_group(tls1_2, Config);
init_per_group(tls1_3, Config) ->
    setup_tls_server_for_group(tls1_3, Config);
init_per_group(revoked_1_2, Config) ->
    setup_tls_server_for_group(revoked_1_2, Config);
init_per_group(revoked_1_3, Config) ->
    setup_tls_server_for_group(revoked_1_3, Config);
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Pid = proplists:get_value(server_pid, Config),
    stop_tls_server(Pid),
    Config.

%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

ocsp_stapling_without_nonce_and_responder_certs_tls1_2() ->
    [{doc, "Verify OCSP stapling works without nonce "
           "and responder certs for tls1.2."}].
ocsp_stapling_without_nonce_and_responder_certs_tls1_2(Config)
  when is_list(Config) ->
    Port = proplists:get_value(server_port, Config),
    {ok, Sock} =
    ssl:connect({127,0,0,1}, Port, proplists:get_value(client_opts, Config) ++
        [{keepalive, true},
         {versions, ['tlsv1.2']},
         {ocsp_stapling, true},
         {ocsp_nonce, false},
         {log_level, debug}], 5000),
    ok = ssl:send(Sock, <<"ok">>),
    ssl:close(Sock).

ocsp_stapling_with_nonce_tls1_2() ->
    [{doc, "Verify OCSP stapling works with nonce "
           "for tls1.2."}].
ocsp_stapling_with_nonce_tls1_2(Config)
  when is_list(Config) ->
    Port = proplists:get_value(server_port, Config),
    {ok, Sock} =
    ssl:connect({127,0,0,1}, Port, proplists:get_value(client_opts, Config) ++
        [{keepalive, true},
         {versions, ['tlsv1.2']},
         {ocsp_stapling, true},
         {log_level, debug}], 5000),
    ok = ssl:send(Sock, <<"ok">>),
    ssl:close(Sock).

ocsp_stapling_with_responder_cert_tls1_2() ->
    [{doc, "Verify OCSP stapling works with OCSP responder cert "
           "for tls1.2."}].
ocsp_stapling_with_responder_cert_tls1_2(Config)
  when is_list(Config) ->
    Port = proplists:get_value(server_port, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    {ok, ResponderCert} =
        file:read_file(filename:join(PrivDir, "b.server/cert.pem")),
    [{'Certificate', Der, _IsEncrypted}] =
        public_key:pem_decode(ResponderCert),
    {ok, Sock} =
    ssl:connect({127,0,0,1}, Port, proplists:get_value(client_opts, Config) ++
        [{keepalive, true},
         {versions, ['tlsv1.2']},
         {ocsp_stapling, true},
         {ocsp_responder_certs, [Der]},
         {log_level, debug}], 5000),
    ok = ssl:send(Sock, <<"ok">>),
    ssl:close(Sock).

ocsp_stapling_revoked_tls1_2() ->
    [{doc, "Verify OCSP stapling works for revoked cert scenario "
           "for tls1.2."}].
ocsp_stapling_revoked_tls1_2(Config)
  when is_list(Config) ->
    Port = proplists:get_value(server_port, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    {ok, ResponderCert} =
        file:read_file(filename:join(PrivDir, "b.server/cert.pem")),
    [{'Certificate', Der, _IsEncrypted}] =
        public_key:pem_decode(ResponderCert),
    {error, {tls_alert, {bad_certificate_status_response, _Info}}} =
    ssl:connect({127,0,0,1}, Port, proplists:get_value(client_opts, Config) ++
        [{keepalive, true},
         {versions, ['tlsv1.2']},
         {ocsp_stapling, true},
         {ocsp_responder_certs, [Der]},
         {log_level, debug}], 5000).


ocsp_stapling_without_nonce_and_responder_certs_tls1_3() ->
    [{doc, "Verify OCSP stapling works without nonce "
           "and responder certs for tls1.3."}].
ocsp_stapling_without_nonce_and_responder_certs_tls1_3(Config)
  when is_list(Config) ->
    Port = proplists:get_value(server_port, Config),
    {ok, Sock} =
    ssl:connect({127,0,0,1}, Port, proplists:get_value(client_opts, Config) ++
        [{keepalive, true},
         {versions, ['tlsv1.3']},
         {ocsp_stapling, true},
         {ocsp_nonce, false},
         {log_level, debug}], 5000),
    ok = ssl:send(Sock, <<"ok">>),
    ssl:close(Sock).

ocsp_stapling_with_nonce_and_responder_certs_tls1_3() ->
    [{doc, "Verify OCSP stapling works with nonce "
           "and responder certs for tls1.3."}].
ocsp_stapling_with_nonce_and_responder_certs_tls1_3(Config)
  when is_list(Config) ->
    Port = proplists:get_value(server_port, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    {ok, ResponderCert} =
        file:read_file(filename:join(PrivDir, "b.server/cert.pem")),
    [{'Certificate', Der, _IsEncrypted}] =
        public_key:pem_decode(ResponderCert),
    {ok, Sock} =
    ssl:connect({127,0,0,1}, Port, proplists:get_value(client_opts, Config) ++
        [{keepalive, true},
         {versions, ['tlsv1.3']},
         {ocsp_stapling, true},
         {ocsp_responder_certs, [Der]},
         {log_level, debug}], 5000),
    ok = ssl:send(Sock, <<"ok">>),
    ssl:close(Sock).

ocsp_stapling_revoked_tls1_3() ->
    [{doc, "Verify OCSP stapling works for revoked cert scenario "
           "for tls1.3."}].
ocsp_stapling_revoked_tls1_3(Config)
  when is_list(Config) ->
    Port = proplists:get_value(server_port, Config),
    {error, {tls_alert, {bad_certificate_status_response, _Info}}} =
    ssl:connect({127,0,0,1}, Port, proplists:get_value(client_opts, Config) ++
        [{keepalive, true},
         {versions, ['tlsv1.3']},
         {ocsp_stapling, true},
         {log_level, debug}], 5000).

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

start_tls_server(Version, ResponderPort, ServerPort, PrivDir) ->
    erlang:spawn(
        ?MODULE, do_start_tls_server,
        [Version, ResponderPort, ServerPort, PrivDir]).

do_start_tls_server(revoked_1_2, ResponderPort, ServerPort, PrivDir) ->
    Cert = filename:join(PrivDir, "revoked/cert.pem"),
    Key = filename:join(PrivDir, "revoked/key.pem"),
    CACerts = filename:join(PrivDir, "revoked/cacerts.pem"),

    Args = ["s_server", "-cert", Cert, "-port", ServerPort, "-key", Key,
            "-CAfile", CACerts, "-status_verbose", "-status_url",
            "http://127.0.0.1:" ++ ResponderPort,
            "-tls1_2"] ++ ["-msg", "-debug"],
    process_flag(trap_exit, true),
    SSLPort = ssl_test_lib:portable_open_port("openssl", Args),
    true = port_command(SSLPort, "Hello world"),
    tls_server_loop(SSLPort);
do_start_tls_server(revoked_1_3, ResponderPort, ServerPort, PrivDir) ->
    Cert = filename:join(PrivDir, "revoked/cert.pem"),
    Key = filename:join(PrivDir, "revoked/key.pem"),
    CACerts = filename:join(PrivDir, "revoked/cacerts.pem"),

    Args = ["s_server", "-cert", Cert, "-port", ServerPort, "-key", Key,
            "-CAfile", CACerts, "-status_verbose", "-status_url",
            "http://127.0.0.1:" ++ ResponderPort,
            "-tls1_3"] ++ ["-msg", "-debug"],
    process_flag(trap_exit, true),
    SSLPort = ssl_test_lib:portable_open_port("openssl", Args),
    true = port_command(SSLPort, "Hello world"),
    tls_server_loop(SSLPort);
do_start_tls_server(Version, ResponderPort, ServerPort, PrivDir) ->
    Cert = filename:join(PrivDir, "a.server/cert.pem"),
    Key = filename:join(PrivDir, "a.server/key.pem"),
    CACerts = filename:join(PrivDir, "a.server/cacerts.pem"),

    Args = ["s_server", "-cert", Cert, "-port", ServerPort, "-key", Key,
            "-CAfile", CACerts, "-status_verbose", "-status_url",
            "http://127.0.0.1:" ++ ResponderPort,
            "-" ++ atom_to_list(Version)] ++ ["-msg", "-debug"],
    process_flag(trap_exit, true),
    SSLPort = ssl_test_lib:portable_open_port("openssl", Args),
    true = port_command(SSLPort, "Hello world"),
    tls_server_loop(SSLPort).

tls_server_loop(SSLPort) ->
    receive
	stop_tls_server ->
	    ct:log("Shut down TLS responder!~n"),
        ok = ssl_test_lib:close_port(SSLPort);
	{_Port, closed} ->
	    ct:log("Port Closed~n"),
	    ok;
	{'EXIT', _Port, Reason} ->
	    ct:log("Port Closed ~p~n",[Reason]),
	    ok;
	Msg ->
	    ct:log("Port Msg ~p~n",[Msg]),
	    tls_server_loop(SSLPort)
    after 600000 ->
        ssl_test_lib:close_port(SSLPort)
    end.

stop_tls_server(Pid) ->
    Pid ! stop_tls_server.

get_free_port() ->
    {ok, Listen} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(Listen),
    ok = gen_tcp:close(Listen),
    Port.

setup_tls_server_for_group(Group, Config) ->
    ResponderPort = proplists:get_value(responder_port, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    Port = get_free_port(),
    Pid = start_tls_server(
        Group, erlang:integer_to_list(ResponderPort),
        erlang:integer_to_list(Port), PrivDir),
    lists:merge(
        [{server_port, Port},
         {server_pid, Pid}
        ], Config).
