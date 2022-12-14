%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2022. All Rights Reserved.
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

-module(openssl_session_SUITE).

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
-export([reuse_session_erlang_server/0,
         reuse_session_erlang_server/1,
         reuse_session_erlang_client/0,
         reuse_session_erlang_client/1
         ]).

-define(SLEEP, 1000).
-define(EXPIRE, 10).
-define(TIMEOUT, {seconds, 120}).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() -> 
    case ssl_test_lib:openssl_sane_dtls() of 
        true ->
            [{group, 'tlsv1.2'},
             {group, 'tlsv1.1'},
             {group, 'tlsv1'},
             {group, 'dtlsv1.2'},
             {group, 'dtlsv1'}];
        false ->
            [{group, 'tlsv1.2'},
             {group, 'tlsv1.1'},
             {group, 'tlsv1'}
             ]
    end.

groups() ->
     case ssl_test_lib:openssl_sane_dtls() of 
         true ->
             [{'tlsv1.2', [], tests()},
              {'tlsv1.1', [], tests()},
              {'tlsv1', [], tests()},
              {'dtlsv1.2', [], tests()},
              {'dtlsv1', [], tests()}
             ];
        false ->
             [{'tlsv1.2', [], tests()},
              {'tlsv1.1', [], tests()},
              {'tlsv1', [], tests()}
           ]
     end.
 
tests() ->
    [    
         reuse_session_erlang_server,
         reuse_session_erlang_client
    ].


init_per_suite(Config0) ->
    Config = ssl_test_lib:init_per_suite(Config0, openssl),
    {ClientOpts, ServerOpts} = ssl_test_lib:make_rsa_cert_chains(
                                 [{server_chain, ssl_test_lib:default_cert_chain_conf()},
                                  {client_chain, ssl_test_lib:default_cert_chain_conf()}],
                                 Config, "openssl_session_SUITE"),
    [{client_opts, ClientOpts}, {server_opts, ServerOpts} | Config].

end_per_suite(Config) ->
    ssl_test_lib:end_per_suite(Config).

init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group_openssl(GroupName, Config).

end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(reuse_session_erlang_client, Config) ->
    ct:timetrap(?EXPIRE * 1000 * 5),
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, session_lifetime, ?EXPIRE),
    ssl:start(),
    Config;
init_per_testcase(reuse_session_erlang_server, Config) ->
    case ssl_test_lib:working_openssl_client(Config) of
        true ->
            Version = ssl_test_lib:protocol_version(Config),
            case ssl_test_lib:is_dtls_version(Version) of
                true ->
                    case ssl_test_lib:openssl_sane_dtls_session_reuse() of
                        true ->
                            ct:timetrap(?TIMEOUT),
                            Config;
                        false ->
                            {skip, "Broken OpenSSL DTLS session reuse"}
                    end;
                false ->
                    ct:timetrap(?TIMEOUT),
                    Config
            end;
        false  ->
            {skip, "Broken OpenSSL s_client"}
    end;
init_per_testcase(_TestCase, Config) ->
    ct:timetrap(?TIMEOUT),
    Config.

end_per_testcase(reuse_session_erlang_client, Config) ->
    application:unset_env(ssl, session_lifetime),
    Config;
end_per_testcase(_, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
reuse_session_erlang_server() ->
    [{doc, "Test erlang server with openssl client that reconnects with the"
      "same session id, to test reusing of sessions."}].
reuse_session_erlang_server(Config) when is_list(Config) ->
    ClientOpts = proplists:get_value(client_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_opts, Config),
    Version = ssl_test_lib:protocol_version(Config),
    
    {_, ServerNode, _} = ssl_test_lib:run_where(Config),
    

    Data = "From openssl to erlang",
    
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
                                        {from, self()},
                                        {mfa, {ssl_test_lib, active_recv, [length(Data)]}},
                                        {reconnect_times, 5},
                                        {options, [{versions, [Version]}| ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    
    {_Client, OpenSSLPort} = ssl_test_lib:start_client(openssl, [{port, Port}, 
                                                                 {reconnect, true},
                                                                 {options, ClientOpts}, 
                                                                 return_port], Config),
    true = port_command(OpenSSLPort, Data),
    
    ssl_test_lib:check_result(Server, Data),
    ssl_test_lib:close(Server).

%%--------------------------------------------------------------------

reuse_session_erlang_client() ->
    [{doc, "Test erlang ssl client that wants to reuse sessions"}].
reuse_session_erlang_client(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_opts, Config),
    ServerOpts = proplists:get_value(server_opts, Config),
    Version = ssl_test_lib:protocol_version(Config),

    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server(openssl, [], 
                                       [{server_opts, ServerOpts} | Config]),
    Port = ssl_test_lib:inet_port(Server),    
    
    Client0 =
        ssl_test_lib:start_client([{node, ClientNode},
                                   {port, Port}, {host, Hostname},
                                   {mfa, {ssl_test_lib, session_id, []}},
                                   {from, self()}, 
                                   {options, [{reuse_sessions, save}, 
                                              {verify, verify_peer},
                                              {versions, [Version]} | ClientOpts]}]),
    
    SID = receive
              {Client0, Id0} ->
                  Id0
          end,
    
    ssl_test_lib:close(Client0),
    
    Client1 =
        ssl_test_lib:start_client([{node, ClientNode},
                                   {port, Port}, {host, Hostname},
                                   {mfa, {ssl_test_lib, session_id, []}},
                                   {from, self()},  {options, [{versions, [Version]},
                                                               {reuse_session, SID} | ClientOpts]}]),
    receive
        {Client1, SID} ->
            ok
    after ?SLEEP ->
            ct:fail(session_not_reused)
    end,
    
    
    ssl_test_lib:close(Client1),
    %% Make sure session is unregistered due to expiration
    ct:sleep(20000),
    
    Client2 =
        ssl_test_lib:start_client([{node, ClientNode},
                                   {port, Port}, {host, Hostname},
                                   {mfa, {ssl_test_lib, session_id, []}},
                                   {from, self()},  {options, [{versions, [Version]} | ClientOpts]}]),
    receive
        {Client2, ID} ->
            case ID of
                SID ->    
                    ct:fail(expired_session_reused);
                _  ->
                    ok
            end
    end,
    ssl_test_lib:close(Client2).

