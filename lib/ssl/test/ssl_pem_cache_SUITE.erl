%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2015. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.2
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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
	    Result =
		(catch make_certs:all(?config(data_dir, Config0),
				      ?config(priv_dir, Config0))),
	    ct:log("Make certs  ~p~n", [Result]),

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

init_per_testcase(pem_cleanup, Config) ->
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, ssl_pem_cache_clean, ?CLEANUP_INTERVAL),
    ssl:start(),
    Config.

end_per_testcase(_TestCase, Config) ->
    %%ssl:stop(),
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
pem_cleanup() ->
    [{doc, "Test pem cache invalidate mechanism"}].
pem_cleanup(Config)when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
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
    case element(5, State) of
	[_CertDb, _FileRefDb, PemChace] ->
	    PemChace;
	_ ->
	    undefined
    end.

later()->
    DateTime = calendar:now_to_local_time(os:timestamp()), 
    Gregorian = calendar:datetime_to_gregorian_seconds(DateTime),
    calendar:gregorian_seconds_to_datetime(Gregorian + (2 * ?CLEANUP_INTERVAL)).
	
