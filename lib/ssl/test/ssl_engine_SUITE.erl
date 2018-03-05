%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2017. All Rights Reserved.
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
-module(ssl_engine_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() ->
    [
     private_key
    ].

init_per_suite(Config) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
            case crypto:info_lib() of
                [{_,_, <<"OpenSSL 1.0.1s-freebsd  1 Mar 2016">>}] ->
                    {skip, "Problem with engine on OpenSSL 1.0.1s-freebsd"};
                _ ->
                    ssl_test_lib:clean_start(),
                    case crypto:get_test_engine() of
                         {ok, EngineName} ->
                            try crypto:engine_load(<<"dynamic">>,
                                                    [{<<"SO_PATH">>, EngineName},
                                                     <<"LOAD">>],
                                                   []) of
                                {ok, Engine} ->
                                    [{engine, Engine} |Config];
                                {error, Reason} ->
                                    ct:pal("Reason ~p", [Reason]),
                                    {skip, "No dynamic engine support"}
                            catch error:notsup ->
                                    {skip, "No engine support in OpenSSL"}    
                            end;
                        {error, notexist} ->
                            {skip, "Test engine not found"}
                    end
            end
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(Config) ->
    Engine = proplists:get_value(engine, Config),
    crypto:engine_unload(Engine),
    ssl:stop(),
    application:stop(crypto).


init_per_testcase(_TestCase, Config) ->
    ssl:stop(),
    ssl:start(),
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 10}),
    Config.

end_per_testcase(_TestCase, Config) ->     
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

private_key(Config) when is_list(Config) ->
    ClientFileBase = filename:join([proplists:get_value(priv_dir, Config), "client_engine"]),
    ServerFileBase = filename:join([proplists:get_value(priv_dir, Config), "server_engine"]),
    #{server_config := ServerConf,
      client_config := ClientConf} = GenCertData =
        public_key:pkix_test_data(#{server_chain => 
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)}],
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)}]],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(3)}
                                                  ]},
                                    client_chain => 
                                        #{root => [{key, ssl_test_lib:hardcode_rsa_key(4)}],
                                          intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(5)}]],
                                          peer => [{key, ssl_test_lib:hardcode_rsa_key(6)}]}}),
    [{server_config, FileServerConf}, 
     {client_config, FileClientConf}] = 
        x509_test:gen_pem_config_files(GenCertData, ClientFileBase, ServerFileBase),
    
    Engine = proplists:get_value(engine, Config),

    ClientKey = engine_key(FileClientConf),
    ServerKey = engine_key(FileServerConf),

    EngineClientConf = [{key, #{algorithm => rsa,
                                engine => Engine,
                                key_id => ClientKey}} | proplists:delete(key, ClientConf)],
    
    EngineServerConf = [{key, #{algorithm => rsa,
                                engine => Engine,
                                key_id => ServerKey}} | proplists:delete(key, ServerConf)],
    %% Test with engine
    test_tls_connection(EngineServerConf, EngineClientConf, Config),
    %% Test that sofware fallback is available
    test_tls_connection(ServerConf, [{reuse_sessions, false} |ClientConf], Config).
    
engine_key(Conf) ->
    FileStr = proplists:get_value(keyfile, Conf),
    list_to_binary(FileStr).
    

test_tls_connection(ServerConf, ClientConf, Config) ->
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options, [{verify, verify_peer}
                                                   | ServerConf]}]),
    Port  = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
                                        {from, self()},
                                        {mfa, {ssl_test_lib, send_recv_result_active, []}},
                                        {options,  [{verify, verify_peer} | ClientConf]}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).
