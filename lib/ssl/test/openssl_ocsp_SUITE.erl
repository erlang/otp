%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2023. All Rights Reserved.
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
-include("ssl_test_lib.hrl").

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
-export([stapling_basic/0, stapling_basic/1,
         stapling_with_nonce/0, stapling_with_nonce/1,
         stapling_with_responder_cert/0, stapling_with_responder_cert/1,
         stapling_revoked/0, stapling_revoked/1,
         stapling_undetermined/0, stapling_undetermined/1,
         stapling_no_staple/0, stapling_no_staple/1
        ]).

%% spawn export
-export([ocsp_responder_init/4]).
-define(OCSP_RESPONDER_LOG, "ocsp_resp_log.txt").
-define(DEBUG, false).

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
    [stapling_basic,
     stapling_with_nonce,
     stapling_with_responder_cert,
     stapling_revoked,
     stapling_undetermined,
     stapling_no_staple
    ].

%%--------------------------------------------------------------------
init_per_suite(Config0) ->
    Config = lists:merge([{debug, ?DEBUG}],
                         ssl_test_lib:init_per_suite(Config0, openssl)),
    case ssl_test_lib:openssl_ocsp_support(Config) of
        true ->
            do_init_per_suite(Config);
        false ->
            {skip, "OCSP not well supported in openSSL"}
    end.

do_init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),

    %% Prepare certs
    {ok, _} = make_certs:all(DataDir, PrivDir),

    ResponderPort = get_free_port(),
    Pid = start_ocsp_responder(ResponderPort, PrivDir, ?config(debug, Config)),

    NewConfig =
        lists:merge(
          [{responder_port, ResponderPort},
           {responder_pid, Pid}
          ], Config),

    ssl_test_lib:cert_options(NewConfig).

end_per_suite(Config) ->
    ResponderPid = proplists:get_value(responder_pid, Config),
    ssl_test_lib:close(ResponderPid),
    [ssl_test_lib:ct_pal_file(?OCSP_RESPONDER_LOG) || ?config(debug, Config)],
    ssl_test_lib:end_per_suite(Config).

%%--------------------------------------------------------------------
init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group_openssl(GroupName, Config).

end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    ct:timetrap({seconds, 10}),
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
stapling_basic() ->
    [{doc, "Verify OCSP stapling works without nonce and responder certs."}].
stapling_basic(Config)
  when is_list(Config) ->
    stapling_helper(Config, [{ocsp_nonce, false}]).

stapling_with_nonce() ->
    [{doc, "Verify OCSP stapling works with nonce."}].
stapling_with_nonce(Config)
  when is_list(Config) ->
    stapling_helper(Config, [{ocsp_nonce, true}]).

stapling_with_responder_cert() ->
    [{doc, "Verify OCSP stapling works with nonce and responder certs."}].
stapling_with_responder_cert(Config)
  when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    {ok, ResponderCert} =
        file:read_file(filename:join(PrivDir, "b.server/cert.pem")),
    [{'Certificate', Der, _IsEncrypted}] =
        public_key:pem_decode(ResponderCert),
    stapling_helper(Config, [{ocsp_nonce, true}, {ocsp_responder_certs, [Der]}]).

stapling_helper(Config, Opts) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    CACertsFile = filename:join(PrivDir, "a.server/cacerts.pem"),
    Data = "ping",  %% 4 bytes
    GroupName = undefined,
    ServerOpts = [{group, GroupName}],
    Server = ssl_test_lib:start_server(openssl_ocsp,
                                       [{options, ServerOpts}], Config),
    Port = ssl_test_lib:inet_port(Server),

    ClientOpts = ssl_test_lib:ssl_options([{verify, verify_peer},
                                           {cacertfile, CACertsFile},
                                           {server_name_indication, disable},
                                           {ocsp_stapling, true}] ++ Opts,
                                          Config),
    Client = ssl_test_lib:start_client(erlang,
                                       [{port, Port},
                                        {options, ClientOpts}], Config),
    true = is_pid(Client),
    ssl_test_lib:send(Server, Data),
    Data = ssl_test_lib:check_active_receive(Client, Data),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
%%--------------------------------------------------------------------
stapling_revoked() ->
    [{doc, "Verify OCSP stapling works with revoked certificate."}].
stapling_revoked(Config)
  when is_list(Config) ->
    stapling_negative_helper(Config, "revoked/cacerts.pem",
                                  openssl_ocsp_revoked, certificate_revoked).

stapling_undetermined() ->
    [{doc, "Verify OCSP stapling works with certificate with undetermined status."}].
stapling_undetermined(Config)
  when is_list(Config) ->
    stapling_negative_helper(Config, "undetermined/cacerts.pem",
                                  openssl_ocsp_undetermined, bad_certificate).

stapling_no_staple() ->
    [{doc, "Verify OCSP stapling works with a missing OCSP response."}].
stapling_no_staple(Config)
  when is_list(Config) ->
    %% Start a server that will not include an OCSP response.
    stapling_negative_helper(Config, "a.server/cacerts.pem",
                                  openssl, bad_certificate).

stapling_negative_helper(Config, CACertsPath, ServerVariant, ExpectedError) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    CACertsFile = filename:join(PrivDir, CACertsPath),
    GroupName = undefined,
    ServerOpts = [{group, GroupName}],
    {ClientNode, _ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server(ServerVariant,
                                       [{options, ServerOpts}], Config),
    Port = ssl_test_lib:inet_port(Server),

    ClientOpts = ssl_test_lib:ssl_options([{verify, verify_peer},
                                           {server_name_indication, disable},
                                           {cacertfile, CACertsFile},
                                           {ocsp_stapling, true},
                                           {ocsp_nonce, true}
                                          ], Config),
    Client = ssl_test_lib:start_client_error([{node, ClientNode},{port, Port},
                                              {host, Hostname}, {from, self()},
                                              {options, ClientOpts}]),
    true = is_pid(Client),
    ssl_test_lib:check_client_alert(Client, ExpectedError).

%%--------------------------------------------------------------------
%% Internal functions -----------------------------------------------
%%--------------------------------------------------------------------
start_ocsp_responder(ResponderPort, PrivDir, Debug) ->
    Starter = self(),
    Pid = erlang:spawn(
            ?MODULE, ocsp_responder_init,
            [ResponderPort, PrivDir, Starter, Debug]),
    receive
        {started, Pid} ->
            Pid;
        {'EXIT', Pid, Reason} ->
            throw({unable_to_start_ocsp_service, Reason})
    end.

ocsp_responder_init(ResponderPort, PrivDir, Starter, Debug) ->
    Index = filename:join(PrivDir, "otpCA/index.txt"),
    CACerts = filename:join(PrivDir, "b.server/cacerts.pem"),
    Cert = filename:join(PrivDir, "b.server/cert.pem"),
    Key = filename:join(PrivDir, "b.server/key.pem"),
    DebugArgs = case Debug of
                    true -> ["-text", "-out", ?OCSP_RESPONDER_LOG];
                    _ -> []
                end,
    Args = ["ocsp", "-index", Index, "-CA", CACerts, "-rsigner", Cert,
            "-rkey", Key, "-port",  erlang:integer_to_list(ResponderPort)] ++
        DebugArgs,
    process_flag(trap_exit, true),
    Port = ssl_test_lib:portable_open_port("openssl", Args),
    ?CT_LOG("OCSP responder: Started Port = ~p", [Port]),
    ocsp_responder_loop(Port, {new, Starter}).

ocsp_responder_loop(Port, {Status, Starter} = State) ->
    receive
        close ->
            ?CT_LOG("OCSP responder: received close", []),
            ok;
	{Port, closed} ->
	    ?CT_LOG("OCSP responder: Port = ~p Closed", [Port]),
	    ok;
	{'EXIT', Sender, _Reason} ->
	    ?CT_LOG("OCSP responder: Sender = ~p Closed",[Sender]),
	    ok;
	{Port, {data, Msg}} when Status == new ->
            ?CT_LOG("OCSP responder: Msg = ~p", [Msg]),
            Starter ! {started, self()},
	    ocsp_responder_loop(Port, {started, undefined});
        {Port, {data, Msg}} ->
	    ?CT_LOG("OCSP responder: Responder Msg = ~p",[Msg]),
            ocsp_responder_loop(Port, State)
    after 1000 ->
            case Status of
                new ->
                    exit(no_ocsp_server);
                _  ->
                    ocsp_responder_loop(Port, State)
            end
    end.

get_free_port() ->
    {ok, Listen} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(Listen),
    ok = gen_tcp:close(Listen),
    Port.
