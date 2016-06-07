%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2016. All Rights Reserved.
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

-module(ssl_sni_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() -> [no_sni_header, 
	  sni_match, 
	  sni_no_match,
	  no_sni_header_fun, 
	  sni_match_fun, 
	  sni_no_match_fun].

init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
        ok ->
            ssl:start(),
	    {ok, _} = make_certs:all(proplists:get_value(data_dir, Config0),
				     proplists:get_value(priv_dir, Config0)),
            ssl_test_lib:cert_options(Config0)
    catch _:_  ->
            {skip, "Crypto did not start"}
    end.

end_per_suite(_) ->
    ssl:stop(),
    application:stop(crypto).

init_per_testcase(_TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:log("Ciphers: ~p~n ", [ ssl:cipher_suites()]),
    ct:timetrap({seconds, 5}),
    Config.

end_per_testcase(_TestCase, Config) ->     
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
no_sni_header(Config) ->
    run_handshake(Config, undefined, undefined, "server").

no_sni_header_fun(Config) ->
    run_sni_fun_handshake(Config, undefined, undefined, "server").

sni_match(Config) ->
    run_handshake(Config, "a.server", "a.server", "a.server").

sni_match_fun(Config) ->
    run_sni_fun_handshake(Config, "a.server", "a.server", "a.server").

sni_no_match(Config) ->
    run_handshake(Config, "c.server", undefined, "server").

sni_no_match_fun(Config) ->
    run_sni_fun_handshake(Config, "c.server", undefined, "server").


%%--------------------------------------------------------------------
%% Internal Functions ------------------------------------------------
%%--------------------------------------------------------------------
ssl_recv(SSLSocket, Expect) ->
    ssl_recv(SSLSocket, "", Expect).

ssl_recv(SSLSocket, CurrentData, ExpectedData) ->
    receive
        {ssl, SSLSocket, Data} ->
            NeweData = CurrentData ++ Data,
            case NeweData of
                ExpectedData ->
                    ok;
                _  ->
                    ssl_recv(SSLSocket, NeweData, ExpectedData)
            end;
        Other ->
            ct:fail({unexpected_message, Other})
    after 4000 ->
            ct:fail({timeout, CurrentData, ExpectedData})
    end.

send_and_hostname(SSLSocket) ->
    ssl:send(SSLSocket, "OK"),
    case  ssl:connection_information(SSLSocket, [sni_hostname]) of
	{ok, [{sni_hostname, Hostname}]} ->
	    Hostname;
	{ok, []} ->
	    undefined
    end.

rdnPart([[#'AttributeTypeAndValue'{type=Type, value=Value} | _] | _], Type) -> 
    Value;
rdnPart([_ | Tail], Type) -> 
    rdnPart(Tail, Type);
rdnPart([], _) -> 
    unknown.

rdn_to_string({utf8String, Binary}) ->
    erlang:binary_to_list(Binary);
rdn_to_string({printableString, String}) ->
    String.

recv_and_certificate(SSLSocket) ->
    ssl_recv(SSLSocket, "OK"),
    {ok, PeerCert} = ssl:peercert(SSLSocket),
    #'OTPCertificate'{tbsCertificate = #'OTPTBSCertificate'{subject = {rdnSequence, Subject}}} 
	= public_key:pkix_decode_cert(PeerCert, otp),
    ct:log("Subject of certificate received from server: ~p", [Subject]),
    rdn_to_string(rdnPart(Subject, ?'id-at-commonName')).

run_sni_fun_handshake(Config, SNIHostname, ExpectedSNIHostname, ExpectedCN) ->
    ct:log("Start running handshake for sni_fun, Config: ~p, SNIHostname: ~p, "
	   "ExpectedSNIHostname: ~p, ExpectedCN: ~p", 
	   [Config, SNIHostname, ExpectedSNIHostname, ExpectedCN]),
    [{sni_hosts, ServerSNIConf}] = proplists:get_value(sni_server_opts, Config),
    SNIFun = fun(Domain) -> proplists:get_value(Domain, ServerSNIConf, undefined) end,
    ServerOptions = proplists:get_value(server_opts, Config) ++ [{sni_fun, SNIFun}],
    ClientOptions = 
    case SNIHostname of
        undefined ->
            proplists:get_value(client_opts, Config);
        _ ->
            [{server_name_indication, SNIHostname}] ++ proplists:get_value(client_opts, Config)
    end,
    ct:log("Options: ~p", [[ServerOptions, ClientOptions]]),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()}, {mfa, {?MODULE, send_and_hostname, []}},
                                        {options, ServerOptions}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                        {host, Hostname}, {from, self()},
                                        {mfa, {?MODULE, recv_and_certificate, []}},
                                        {options, ClientOptions}]),
    ssl_test_lib:check_result(Server, ExpectedSNIHostname, Client, ExpectedCN),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

run_handshake(Config, SNIHostname, ExpectedSNIHostname, ExpectedCN) ->
    ct:log("Start running handshake, Config: ~p, SNIHostname: ~p, "
	   "ExpectedSNIHostname: ~p, ExpectedCN: ~p", 
	   [Config, SNIHostname, ExpectedSNIHostname, ExpectedCN]),
    ServerOptions = proplists:get_value(sni_server_opts, Config) ++ proplists:get_value(server_opts, Config),
    ClientOptions = 
    case SNIHostname of
        undefined ->
            proplists:get_value(client_opts, Config);
        _ ->
            [{server_name_indication, SNIHostname}] ++ proplists:get_value(client_opts, Config)
    end,
    ct:log("Options: ~p", [[ServerOptions, ClientOptions]]),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()}, {mfa, {?MODULE, send_and_hostname, []}},
                                        {options, ServerOptions}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                        {host, Hostname}, {from, self()},
                                        {mfa, {?MODULE, recv_and_certificate, []}},
                                        {options, ClientOptions}]),
    ssl_test_lib:check_result(Server, ExpectedSNIHostname, Client, ExpectedCN),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
