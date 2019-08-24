%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2019. All Rights Reserved.
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
-module(ssl_app_env_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("ssl/src/ssl_api.hrl").

-define(SLEEP, 500).
%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [
     {group, 'tlsv1.3'},
     {group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'sslv3'},
     {group, 'dtlsv1.2'},
     {group, 'dtlsv1'}
    ].

groups() ->
    [
     {'tlsv1.3', [], tests()},
     {'tlsv1.2', [],  tests()},
     {'tlsv1.1', [],  tests()},
     {'tlsv1', [],  tests()},
     {'sslv3', [],  tests()},
     {'dtlsv1.2', [], tests()},
     {'dtlsv1', [],  tests()}
    ].

tests() ->
    [
     internal_active_1,
     protocol_versions,
     empty_protocol_versions
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
    case ssl_test_lib:is_tls_version(GroupName) of
	true ->
	    case ssl_test_lib:sufficient_crypto_support(GroupName) of
		true ->
		    [{client_type, erlang},
                     {server_type, erlang} | ssl_test_lib:init_tls_version(GroupName, Config)];
		false ->
		    {skip, "Missing crypto support"}
	    end;
	_ ->
	    ssl:start(),
	    Config
    end.

end_per_group(GroupName, Config) ->
    case ssl_test_lib:is_tls_version(GroupName) of
        true ->
            ssl_test_lib:clean_tls_version(Config);
        false ->
            Config
    end.

init_per_testcase(internal_active_1, Config) ->
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, internal_active_n, 1),
    ssl:start(),
    ct:timetrap({seconds, 5}),
    Config;
init_per_testcase(protocol_versions, Config) ->
    Version = ssl_test_lib:protocol_version(Config),
    case atom_to_list(Version) of
        "d" ++ _ ->
            ssl:stop(),
            application:load(ssl),
            application:set_env(ssl, dtls_protocol_version, [Version]),
            ssl:start();
        _ ->  
            ssl:stop(),
            application:load(ssl),
            application:set_env(ssl, protocol_version, [Version]),
            ssl:start()
    end,
    ct:timetrap({seconds, 5}),
    Config;
init_per_testcase(empty_protocol_versions, Config)  ->
    ssl:stop(),
    application:load(ssl),
    ssl_test_lib:clean_env(),
    application:set_env(ssl, protocol_version, []),
    application:set_env(ssl, dtls_protocol_version, []),
    ssl:start(),
    ct:timetrap({seconds, 5}),
    Config;
init_per_testcase(_TestCase, Config) ->
    ct:timetrap({seconds, 5}),
    Config.

end_per_testcase(_, _Config) ->
    ssl_test_lib:clean_start().

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
internal_active_1() ->
    [{doc,"Test internal active 1 (behave as internal active once)"}].

internal_active_1(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).
    
%%--------------------------------------------------------------------
protocol_versions() ->
    [{doc,"Test to set a list of protocol versions in app environment."}].

protocol_versions(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

%%--------------------------------------------------------------------
empty_protocol_versions() ->
    [{doc,"Test to set an empty list of protocol versions in app environment."}].

empty_protocol_versions(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ssl_test_lib:basic_test(ClientOpts, ServerOpts, Config).

