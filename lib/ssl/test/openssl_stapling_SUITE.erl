%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2024. All Rights Reserved.
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

-module(openssl_stapling_SUITE).

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
-export([staple_by_issuer/0, staple_by_issuer/1,
         staple_by_designated/0, staple_by_designated/1,
         staple_by_trusted/0, staple_by_trusted/1,
         staple_not_designated/0, staple_not_designated/1,
         staple_wrong_issuer/0, staple_wrong_issuer/1,
         staple_with_nonce/0, staple_with_nonce/1,
         cert_status_revoked/0, cert_status_revoked/1,
         cert_status_undetermined/0, cert_status_undetermined/1,
         staple_missing/0, staple_missing/1
        ]).

%% spawn export
-export([ocsp_responder_init/3]).
-define(OCSP_RESPONDER_LOG, "ocsp_resp_log.txt").
-define(DEBUG, false).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() ->
    [{group, 'tlsv1.3'},
     {group, no_next_update},
     {group, 'tlsv1.2'},
     {group, 'dtlsv1.2'}].

groups() ->
    [{'tlsv1.3', [], ocsp_tests()},
     {no_next_update, [], [{group, 'tlsv1.3'}]},
     {'tlsv1.2', [], ocsp_tests()},
     {'dtlsv1.2', [], ocsp_tests()}].

ocsp_tests() ->
    positive() ++ negative().

positive() ->
    [staple_by_issuer,
     staple_by_designated,
     staple_by_trusted,
     staple_with_nonce].

negative() ->
    [staple_not_designated,
     staple_wrong_issuer,
     cert_status_revoked,
     cert_status_undetermined,
     staple_missing].

%%--------------------------------------------------------------------
init_per_suite(Config0) ->
    Config = [{debug, ?DEBUG}] ++
        ssl_test_lib:init_per_suite(Config0, openssl),
    case ssl_test_lib:openssl_ocsp_support(Config) of
        true ->
            do_init_per_suite(Config);
        false ->
            {skip, "OCSP not well supported in openSSL"}
    end.

do_init_per_suite(Config) ->
    {ok, _} = make_certs:all(?config(data_dir, Config),
                             ?config(priv_dir, Config)),
    ssl_test_lib:cert_options(Config).

end_per_suite(Config) ->
    ssl_test_lib:end_per_suite(Config).

%%--------------------------------------------------------------------
init_per_group(no_next_update, Config) ->
    Config;
init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group_openssl(GroupName, Config).

end_per_group(no_next_update, Config) ->
    Config;
end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

%%--------------------------------------------------------------------
init_per_testcase(staple_by_trusted = Testcase, Config) ->
    PrivDir = ?config(priv_dir, Config),
    ok = public_key:cacerts_load(filename:join(PrivDir, "otpCA/cacerts.pem")),
    init_per_testcase_helper(Testcase, Config);
init_per_testcase(Testcase, Config) ->
    init_per_testcase_helper(Testcase, Config).

init_per_testcase_helper(Testcase, Config0) ->
    ct:timetrap({seconds, 10}),
    Default = "otpCA",
    TestcaseMapping = #{staple_by_issuer => Default,
                        staple_by_trusted => "erlangCA",
                        staple_by_designated => "b.server",
                        staple_not_designated => "a.server",
                        staple_wrong_issuer => "localhost"},
    ResponderFolder = maps:get(Testcase, TestcaseMapping, Default),
    Config = start_ocsp_responder(
               [{responder_folder, ResponderFolder} | Config0]) ++ Config0,
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    Config.

end_per_testcase(staple_by_trusted, Config) ->
    public_key:cacerts_load(),
    end_per_testcase_helper(Config);
end_per_testcase(_Testcase, Config) ->
    end_per_testcase_helper(Config).

end_per_testcase_helper(Config) ->
    ResponderPid = ?config(responder_pid, Config),
    ssl_test_lib:close(ResponderPid),
    [ssl_test_lib:ct_pal_file(?OCSP_RESPONDER_LOG) || ?config(debug, Config)],
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
%% Test various certs used for signing OCSP response
%% Assuming Issuer issued a.server certificate used by TLS server
%% 1. otpCA - [OK] Issuer signs response directly
%% 2. b.server - [OK] Responder certificate issued directly by Issuer
%%                     and designated for OCSP signing
%% 3 localhost - [OK] Certificate not issued by Issuer, but present in trust store
%% 4. a.server - [NOK] Certificate signed directly by Issuer but not designated
%% 5. localhost - [NOK] Certificate not issued by Issuer

staple_by_issuer() ->
    [{doc, "Verify OCSP stapling works without nonce."
      "Response signed directly by issuer of server certificate"}].
staple_by_issuer(Config)
  when is_list(Config) ->
    stapling_helper(Config, #{ocsp_nonce => false}).

staple_by_designated() ->
    [{doc,"Verify OCSP stapling works without nonce."
      "Response signed with certificate issued directly by issuer of server "
      "certificate and is designated for OCSP signing (extKeyUsage allows "
      "for OCSP signing)."}].
staple_by_designated(Config)
  when is_list(Config) ->
    stapling_helper(Config, #{ocsp_nonce => false}).

staple_by_trusted() ->
    [{doc,"Verify OCSP stapling works without nonce."
      "Response signed with certificate issued directly by issuer of server "
      "certificate and is designated for OCSP signing (extKeyUsage allows "
      "for OCSP signing)."}].
staple_by_trusted(Config)
  when is_list(Config) ->
    stapling_helper(Config, #{ocsp_nonce => false}).

staple_with_nonce() ->
    [{doc, "Verify OCSP stapling works with nonce."}].
staple_with_nonce(Config)
  when is_list(Config) ->
    stapling_helper(Config, #{ocsp_nonce => true}).

stapling_helper(Config, StaplingOpt) ->
    %% ok = logger:set_application_level(ssl, debug),
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
                                           {stapling, StaplingOpt}],
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
staple_not_designated() ->
    [{doc,"Verify OCSP stapling works without nonce."
      "Response signed with certificate issued directly by issuer of server "
      "certificate but not  designated for OCSP signing (extKeyUsage missing "
      "OCSP signing)."}].
staple_not_designated(Config)
  when is_list(Config) ->
    stapling_negative_helper(Config, "a.server/cacerts.pem",
                                  openssl_ocsp, bad_certificate).

staple_wrong_issuer() ->
    [{doc,"Verify OCSP stapling works without nonce."
      "Response signed with certificate not related to issuer of server "
      "certificate."}].
staple_wrong_issuer(Config)
  when is_list(Config) ->
    stapling_negative_helper(Config, "a.server/cacerts.pem",
                                  openssl_ocsp, bad_certificate).

cert_status_revoked() ->
    [{doc, "Verify OCSP stapling works with revoked certificate."}].
cert_status_revoked(Config)
  when is_list(Config) ->
    stapling_negative_helper(Config, "revoked/cacerts.pem",
                                  openssl_ocsp_revoked, certificate_revoked).

cert_status_undetermined() ->
    [{doc, "Verify OCSP stapling works with certificate with undetermined status."}].
cert_status_undetermined(Config)
  when is_list(Config) ->
    stapling_negative_helper(Config, "undetermined/cacerts.pem",
                                  openssl_ocsp_undetermined, bad_certificate).

staple_missing() ->
    [{doc, "Verify OCSP stapling works with a missing OCSP response."}].
staple_missing(Config)
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
                                           {stapling, #{ocsp_nonce => true}}],
                                          Config),
    Client = ssl_test_lib:start_client_error([{node, ClientNode},{port, Port},
                                              {host, Hostname}, {from, self()},
                                              {options, ClientOpts}]),
    true = is_pid(Client),
    ssl_test_lib:check_client_alert(Client, ExpectedError).

%%--------------------------------------------------------------------
%% Internal functions -----------------------------------------------
%%--------------------------------------------------------------------
start_ocsp_responder(Config) ->
    ResponderPort = get_free_port(),
    Starter = self(),
    process_flag(trap_exit, true),
    Pid = erlang:spawn_link(?MODULE, ocsp_responder_init,
                            [ResponderPort, Starter, Config]),
    receive
        {started, Pid} ->
            [{responder_port, ResponderPort}, {responder_pid, Pid}];
        {'EXIT', Pid, Reason} ->
            throw({unable_to_start_ocsp_service, Reason})
    end.

ocsp_responder_init(ResponderPort, Starter, Config) ->
    ResponderFolder = ?config(responder_folder, Config),
    PrivDir = ?config(priv_dir, Config),
    Index = filename:join(PrivDir, "otpCA/index.txt"),
    CACerts = filename:join(PrivDir, "otpCA/cacerts.pem"),
    Cert = filename:join(PrivDir, ResponderFolder ++ "/cert.pem"),
    %% search for key.pem file, since generated intermediate CAs
    %% "hide" their key.pem inside "private" subfolder
    [Key] = filelib:fold_files(filename:join(PrivDir, ResponderFolder),
                               "key.pem", true, fun(X, Acc) -> [X | Acc] end, []),
    Debug = case ?config(debug, Config) of
                    true -> ["-text", "-out", ?OCSP_RESPONDER_LOG];
                    _ -> []
                end,
    NextUpdate = case ?config(tc_group_path, Config) of
                     [[{name,no_next_update}]] ->
                         [];
                     _ ->
                         ["-nmin", "5"]
                 end,
    Args = ["ocsp", "-index", Index, "-CA", CACerts, "-rsigner", Cert,
            "-rkey", Key, "-port",  erlang:integer_to_list(ResponderPort)] ++
        Debug ++ NextUpdate,
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
