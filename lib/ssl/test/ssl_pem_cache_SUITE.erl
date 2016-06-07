%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2015. All Rights Reserved.
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

-module(ssl_pem_cache_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-define(CLEANUP_INTERVAL, 5000).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() ->
    [pem_cleanup].

groups() ->
    [].

init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
	    ssl:start(),
	    %% make rsa certs using oppenssl
	    {ok, _} =  make_certs:all(proplists:get_value(data_dir, Config0),
				      proplists:get_value(priv_dir, Config0)),
	    Config1 = ssl_test_lib:make_dsa_cert(Config0),
	    ssl_test_lib:cert_options(Config1)
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    application:stop(crypto).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(pem_cleanup = Case, Config) ->
    end_per_testcase(Case, Config) ,
    application:load(ssl),
    application:set_env(ssl, ssl_pem_cache_clean, ?CLEANUP_INTERVAL),
    ssl:start(),
    ct:timetrap({minutes, 1}),
    Config.

end_per_testcase(_TestCase, Config) ->
    ssl:stop(),
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
pem_cleanup() ->
    [{doc, "Test pem cache invalidate mechanism"}].
pem_cleanup(Config)when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts = proplists:get_value(client_opts, Config),
    ServerOpts = proplists:get_value(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
	ssl_test_lib:start_client([{node, ClientNode},
		      {port, Port}, {host, Hostname},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {from, self()}, {options, ClientOpts}]),

    Size = ssl_pkix_db:db_size(get_pem_cache()),
    Certfile = proplists:get_value(certfile, ServerOpts),
    {ok, FileInfo} = file:read_file_info(Certfile),
    Time = later(), 
    ok = file:write_file_info(Certfile, FileInfo#file_info{mtime = Time}),
    ct:sleep(2 * ?CLEANUP_INTERVAL),
    Size1 = ssl_pkix_db:db_size(get_pem_cache()),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    false = Size == Size1.
       
get_pem_cache() ->
    {status, _, _, StatusInfo} = sys:get_status(whereis(ssl_manager)),
    [_, _,_, _, Prop] = StatusInfo,
    State = ssl_test_lib:state(Prop),
    case element(6, State) of
	[_CertDb, _FileRefDb, PemCache| _] ->
	    PemCache;
	_ ->
	    undefined
    end.

later()->
    DateTime = calendar:now_to_local_time(os:timestamp()), 
    Gregorian = calendar:datetime_to_gregorian_seconds(DateTime),
    calendar:gregorian_seconds_to_datetime(Gregorian + (2 * ?CLEANUP_INTERVAL)).
	
