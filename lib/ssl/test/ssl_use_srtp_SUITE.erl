%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2023. All Rights Reserved.
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

-module(ssl_use_srtp_SUITE).

-behaviour(ct_suite).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/inet.hrl").

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
-export([srtp_profiles/1,
         srtp_mki/1
        ]).

-define(TIMEOUT, {seconds, 6}).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [
     {group, 'dtlsv1.2'},
     {group, 'dtlsv1'}
    ].

groups() ->
    [
     {'dtlsv1.2', [], use_srtp_tests()},
     {'dtlsv1', [], use_srtp_tests()}
    ].

use_srtp_tests() ->
    [
     srtp_profiles,
     srtp_mki
    ].

init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
        ok ->
            ssl_test_lib:clean_start(),
            {#{server_config := _ServerConf,
               client_config := ClientConf},
             #{server_config := _LServerConf,
               client_config := LClientConf}} = ssl_test_lib:make_rsa_sni_configs(),
            %% RSA certs files needed by *dot cases
            ssl_test_lib:make_rsa_cert([{client_opts, ClientConf},
                                        {client_local_opts, LClientConf}
                                       | Config0])
    catch _:_  ->
            {skip, "Crypto did not start"}
    end.
init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group(GroupName, Config).

end_per_group(GroupName, Config) ->
   ssl_test_lib:end_per_group(GroupName, Config).

end_per_suite(_) ->
    ssl:stop(),
    application:stop(crypto).

init_per_testcase(_TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap(?TIMEOUT),
    Config.

end_per_testcase(_TestCase, Config) ->     
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

srtp_profiles(Config) ->
    % Client sends a list of SRTP profiles it supports in client_hello
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ClentSrtpOpts = [{use_srtp, #{protection_profiles => [<<0,1>>,<<0,2>>,<<0,5>>]}}],
    ClientOpts = ClentSrtpOpts ++ [{handshake, hello}] ++ ClientOpts0,
    ClientContOpts = [{continue_options, [{want_ext, self()}]}],
    % Server responds with a single chosen profile in server_hello
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ServerOpts = [{handshake, hello}] ++ ServerOpts0,
    ServerSrtpOts = [{use_srtp, #{protection_profiles => [<<0,2>>]}}],
    ServerContOpts = [{continue_options, [{want_ext, self()}|ServerSrtpOts]}],

    Server = ssl_test_lib:start_server(ServerContOpts,
                                       [{server_opts, ServerOpts} | Config]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{port, Port} | ClientContOpts],
                                       [{client_opts, ClientOpts} | Config]),

    receive
        {Server, {ext, C2SExt}} ->
            C2SSRTP = maps:get(use_srtp, C2SExt),
            #{protection_profiles := [<<0,1>>,<<0,2>>,<<0,5>>]} = C2SSRTP,
            #{mki := <<>>} = C2SSRTP,
            ssl_test_lib:close(Server)
    end,
    receive
        {Client, {ext, S2CExt}} ->
            S2CSRTP = maps:get(use_srtp, S2CExt),
            #{protection_profiles := [<<0,2>>]} = S2CSRTP,
            #{mki := <<>>} = S2CSRTP,
            ssl_test_lib:close(Client)
    end,
    ok.


srtp_mki(Config) ->
    % Client sends some MKI in a client_hello
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ClientSrtpOpts = [{use_srtp, #{protection_profiles => [<<0,1>>,<<0,2>>,<<0,5>>],
                                   mki => <<"client_mki">>}}],
    ClientOpts = ClientSrtpOpts ++ [{handshake, hello}] ++ ClientOpts0,
    ClientContOpts = [{continue_options, [{want_ext, self()}]}],
    % Server responds with its own MKI just to ensure it is delivered to the client
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ServerOpts = [{handshake, hello}] ++ ServerOpts0,
    ServerSrtpOpts = [{use_srtp, #{protection_profiles => [<<0,2>>],
                                   mki => <<"server_mki">>}}],
    ServerContOpts = [{continue_options, [{want_ext, self()}|ServerSrtpOpts]}],

    Server = ssl_test_lib:start_server(ServerContOpts,
                                       [{server_opts, ServerOpts} | Config]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client(
               [{port, Port}, {options, ClientOpts} | ClientContOpts], Config),

    receive
        {Server, {ext, C2SExt}} ->
            C2SSRTP = maps:get(use_srtp, C2SExt),
            #{mki := <<"client_mki">>} = C2SSRTP,
            ssl_test_lib:close(Server)
    end,
    receive
        {Client, {ext, S2CExt}} ->
            S2CSRTP = maps:get(use_srtp, S2CExt),
            #{mki := <<"server_mki">>} = S2CSRTP,
            ssl_test_lib:close(Client)
    end,
    ok.
