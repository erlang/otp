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

-module(openssl_ocsp_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

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
-export([ocsp_stapling_basic/0,ocsp_stapling_basic/1,
         ocsp_stapling_with_nonce/0, ocsp_stapling_with_nonce/1,
         ocsp_stapling_with_responder_cert/0,ocsp_stapling_with_responder_cert/1,
         ocsp_stapling_revoked/0, ocsp_stapling_revoked/1,
         ocsp_stapling_undetermined/0, ocsp_stapling_undetermined/1,
         ocsp_stapling_no_staple/0, ocsp_stapling_no_staple/1
        ]).

%% spawn export
-export([ocsp_responder_init/3]).


%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() -> 
    [{group, 'tlsv1.3'},
     {group, 'tlsv1.2'},
     {group, 'dtlsv1.2'}].

groups() -> 
    [{'tlsv1.3', [], ocsp_tests()},
     {'tlsv1.2', [], ocsp_tests()},
     {'dtlsv1.2', [], ocsp_tests()}].

ocsp_tests() ->
    [ocsp_stapling_basic,
     ocsp_stapling_with_nonce,
     ocsp_stapling_with_responder_cert,
     ocsp_stapling_revoked,
     ocsp_stapling_undetermined,
     ocsp_stapling_no_staple
    ].

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    case ssl_test_lib:openssl_ocsp_support() of
        true ->
            do_init_per_suite(Config);
        false ->
            {skip, "OCSP not well supported in openSSL"}
    end.

do_init_per_suite(Config) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
        ssl_test_lib:clean_start(),
        DataDir = proplists:get_value(data_dir, Config),
        PrivDir = proplists:get_value(priv_dir, Config),

        %% Prepare certs
        {ok, _} = make_certs:all(DataDir, PrivDir),

        ResponderPort = get_free_port(),
        Pid = start_ocsp_responder(ResponderPort, PrivDir),

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
    ssl_test_lib:close(ResponderPid),
    ok = ssl:stop(),
    application:stop(crypto).

%%--------------------------------------------------------------------
init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group_openssl(GroupName, Config).

end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

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
    PrivDir = proplists:get_value(priv_dir, Config),
    CACertsFile = filename:join(PrivDir, "a.server/cacerts.pem"),

    Data = "ping",  %% 4 bytes
    GroupName = proplists:get_value(group, Config),
    ServerOpts = [{log_level, debug},
                  {group, GroupName}],
    Server = ssl_test_lib:start_server(openssl_ocsp,
                                       [{options, ServerOpts}], Config),
    Port = ssl_test_lib:inet_port(Server),

    ClientOpts = [{log_level, debug},
                  {verify, verify_peer},
                  {cacertfile, CACertsFile},
                  {server_name_indication, disable},
                  {ocsp_stapling, true},
                  {ocsp_nonce, false}] ++ dtls_client_opt(GroupName),
    Client = ssl_test_lib:start_client(erlang,
                                       [{port, Port},
                                        {options, ClientOpts}], Config),
    ssl_test_lib:send(Server, Data),
    Data = ssl_test_lib:check_active_receive(Client, Data),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
ocsp_stapling_with_nonce() ->
    [{doc, "Verify OCSP stapling works with nonce."}].
ocsp_stapling_with_nonce(Config)
  when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    CACertsFile = filename:join(PrivDir, "a.server/cacerts.pem"),
    
    Data = "ping",  %% 4 bytes
    GroupName = proplists:get_value(group, Config),
    ServerOpts = [{log_level, debug},
                  {group, GroupName}],
    Server = ssl_test_lib:start_server(openssl_ocsp,
                                       [{options, ServerOpts}], Config),
    Port = ssl_test_lib:inet_port(Server),

    ClientOpts = [{log_level, debug},
                  {verify, verify_peer},
                  {cacertfile, CACertsFile},
                  {server_name_indication, disable},
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
    PrivDir = proplists:get_value(priv_dir, Config),
    CACertsFile = filename:join(PrivDir, "a.server/cacerts.pem"),
 
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
                  {verify, verify_peer},
                  {cacertfile, CACertsFile},
                  {server_name_indication, disable},
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
%%--------------------------------------------------------------------
ocsp_stapling_revoked() ->
    [{doc, "Verify OCSP stapling works with revoked certificate."}].
ocsp_stapling_revoked(Config)
  when is_list(Config) ->    
    PrivDir = proplists:get_value(priv_dir, Config),
    CACertsFile = filename:join(PrivDir, "revoked/cacerts.pem"),

    GroupName = proplists:get_value(group, Config),
    ServerOpts = [{log_level, debug},
                  {group, GroupName}],
    {ClientNode, _ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server(openssl_ocsp_revoked,
                                       [{options, ServerOpts}], Config),
    Port = ssl_test_lib:inet_port(Server),

    ClientOpts = [{log_level, debug},
                  {verify, verify_peer},
                  {server_name_indication, disable},
                  {cacertfile, CACertsFile},
                  {ocsp_stapling, true},
                  {ocsp_nonce, true}
                 ] ++ dtls_client_opt(GroupName),
    
    Client = ssl_test_lib:start_client_error([{node, ClientNode},{port, Port},
                                              {host, Hostname}, {from, self()},
                                              {options, ClientOpts}]),

    ssl_test_lib:check_client_alert(Client, certificate_revoked).

%%--------------------------------------------------------------------
ocsp_stapling_undetermined() ->
    [{doc, "Verify OCSP stapling works with certificate with undetermined status."}].
ocsp_stapling_undetermined(Config)
  when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    CACertsFile = filename:join(PrivDir, "undetermined/cacerts.pem"),

    GroupName = proplists:get_value(group, Config),
    ServerOpts = [{log_level, debug},
                  {group, GroupName}],
    {ClientNode, _ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server(openssl_ocsp_undetermined,
                                       [{options, ServerOpts}], Config),
    Port = ssl_test_lib:inet_port(Server),

    ClientOpts = [{log_level, debug},
                  {verify, verify_peer},
                  {server_name_indication, disable},
                  {cacertfile, CACertsFile},
                  {ocsp_stapling, true},
                  {ocsp_nonce, true}
                 ] ++ dtls_client_opt(GroupName),

    Client = ssl_test_lib:start_client_error([{node, ClientNode},{port, Port},
                                              {host, Hostname}, {from, self()},
                                              {options, ClientOpts}]),

    ssl_test_lib:check_client_alert(Client, bad_certificate).

%%--------------------------------------------------------------------
ocsp_stapling_no_staple() ->
    [{doc, "Verify OCSP stapling works with a missing OCSP response."}].
ocsp_stapling_no_staple(Config)
  when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    CACertsFile = filename:join(PrivDir, "a.server/cacerts.pem"),

    GroupName = proplists:get_value(group, Config),
    ServerOpts = [{log_level, debug},
                  {group, GroupName}],
    {ClientNode, _ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    %% Start a server that will not include an OCSP response.
    Server = ssl_test_lib:start_server(openssl,
                                       [{options, ServerOpts}], Config),
    Port = ssl_test_lib:inet_port(Server),

    ClientOpts = [{log_level, debug},
                  {verify, verify_peer},
                  {server_name_indication, disable},
                  {cacertfile, CACertsFile},
                  {ocsp_stapling, true},
                  {ocsp_nonce, true}
                 ] ++ dtls_client_opt(GroupName),

    Client = ssl_test_lib:start_client_error([{node, ClientNode},{port, Port},
                                              {host, Hostname}, {from, self()},
                                              {options, ClientOpts}]),

    ssl_test_lib:check_client_alert(Client, bad_certificate).

%%--------------------------------------------------------------------
%% Intrernal functions -----------------------------------------------
%%--------------------------------------------------------------------
start_ocsp_responder(ResponderPort, PrivDir) ->
    Starter = self(),
    Pid = erlang:spawn_link(
            ?MODULE, ocsp_responder_init, [ResponderPort, PrivDir, Starter]),
    receive
        {started, Pid} ->
            Pid;
        {'EXIT', Pid, Reason} ->
            throw({unable_to_start_ocsp_service, Reason})
    end.

ocsp_responder_init(ResponderPort, PrivDir, Starter) ->
    Index = filename:join(PrivDir, "otpCA/index.txt"),
    CACerts = filename:join(PrivDir, "b.server/cacerts.pem"),
    Cert = filename:join(PrivDir, "b.server/cert.pem"),
    Key = filename:join(PrivDir, "b.server/key.pem"),

    Args = ["ocsp", "-index", Index, "-CA", CACerts, "-rsigner", Cert,
            "-rkey", Key, "-port",  erlang:integer_to_list(ResponderPort)],
    process_flag(trap_exit, true),
    Port = ssl_test_lib:portable_open_port("openssl", Args),
    ocsp_responder_loop(Port, {new, Starter}).

ocsp_responder_loop(Port, {Status, Starter} = State) ->
    receive
	stop_ocsp_responder ->
	    ct:log("Shut down OCSP responder!~n"),
            ok = ssl_test_lib:close_port(Port);
	{_Port, closed} ->
	    ct:log("Port Closed~n"),
	    ok;
	{'EXIT', _Port, Reason} ->
	    ct:log("Port Closed ~p~n",[Reason]),
	    ok;
	{Port, {data, _Msg}} when Status == new ->
            Starter ! {started, self()},
	    ocsp_responder_loop(Port, {started, undefined});
        {Port, {data, Msg}} ->
	    ct:pal("Responder Msg ~p~n",[Msg]),
            ocsp_responder_loop(Port, State)
    after 1000 ->
            case Status of
                new ->
                    exit(no_ocsp_server);
                _  ->
                    ocsp_responder_loop(Port, State)
            end
    end.

stop_ocsp_responder(Pid) ->
    Pid ! stop_ocsp_responder.

get_free_port() ->
    {ok, Listen} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(Listen),
    ok = gen_tcp:close(Listen),
    Port.

dtls_client_opt('dtlsv1.2') ->
    [{protocol, dtls}];
dtls_client_opt(_Other) ->
    [].