%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2015. All Rights Reserved.
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
-module(ssl_upgrade_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-record(state, {
	  config,
	  server,
	  client,
	  soft
	 }).

all() -> 
    [
     minor_upgrade,
     major_upgrade
    ].

init_per_suite(Config0) ->
    catch crypto:stop(),
    try {crypto:start(), erlang:system_info({wordsize, internal}) == erlang:system_info({wordsize, external})} of
	{ok, true} ->
	    case ct_release_test:init(Config0) of
		{skip, Reason} ->
		    {skip, Reason};
		Config ->
		    {ok, _} = make_certs:all(?config(data_dir, Config),
					      ?config(priv_dir, Config)),
		    ssl_test_lib:cert_options(Config)
	    end;
	{ok, false} ->
	    {skip, "Test server will not handle halfwordemulator correctly. Skip as halfwordemulator is deprecated"} 
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(Config) ->
    ct_release_test:cleanup(Config),
    crypto:stop().

init_per_testcase(_TestCase, Config) ->
    ct:log("TLS/SSL version ~p~n ", [tls_record:supported_protocol_versions()]),
    ct:timetrap({minutes, 1}),
    Config.

end_per_testcase(_TestCase, Config) ->     
    Config.

major_upgrade(Config) when is_list(Config) ->
    ct_release_test:upgrade(ssl, major,{?MODULE, #state{config = Config}}, Config).

minor_upgrade(Config) when is_list(Config) ->
    ct_release_test:upgrade(ssl, minor,{?MODULE, #state{config = Config}}, Config).

upgrade_init(CTData, #state{config = Config} = State) -> 
    {ok, {_, _, Up, _Down}} = ct_release_test:get_appup(CTData, ssl),
    ct:pal("Up: ~p", [Up]),
    Soft = is_soft(Up), %% It is symmetrical, if upgrade is soft so is downgrade  
    case Soft of 
	true ->
	    {Server, Client} = soft_start_connection(Config),
	    State#state{server = Server, client = Client,
			soft = Soft};
	false ->
	    State#state{soft = Soft}
    end.

upgrade_upgraded(_, #state{soft = false, config = Config} = State) -> 
    {Server, Client} = restart_start_connection(Config),
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    State;

upgrade_upgraded(_, #state{server = Server0, client = Client0,
			   config = Config, soft = true} = State) -> 
    Server0 ! changed_version,
    Client0 ! changed_version,
    ssl_test_lib:check_result(Server0, ok, Client0, ok),
    ssl_test_lib:close(Server0),
    ssl_test_lib:close(Client0),
    {Server, Client} = soft_start_connection(Config),
    State#state{server = Server, client = Client}.

upgrade_downgraded(_, #state{soft = false, config = Config} = State) -> 
    {Server, Client} = restart_start_connection(Config),
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    State;

upgrade_downgraded(_, #state{server = Server, client = Client, soft = true} = State) -> 
    Server ! changed_version,
    Client ! changed_version,
    ssl_test_lib:check_result(Server, ok, Client, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    State.

use_connection(Socket) ->
    ssl_test_lib:send_recv_result_active(Socket),
    receive 
	changed_version ->
	    ssl_test_lib:send_recv_result_active(Socket)
    end.

soft_start_connection(Config) ->
    ClientOpts = ?config(client_verification_opts, Config),
    ServerOpts = ?config(server_verification_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {?MODULE, use_connection, []}},
					{options, ServerOpts}]),
    
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()}, 
					{mfa, {?MODULE, use_connection, []}},
					{options, ClientOpts}]),
    {Server, Client}.

restart_start_connection(Config) ->
    ClientOpts = ?config(client_verification_opts, Config),
    ServerOpts = ?config(server_verification_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()}, 
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ClientOpts}]),
    {Server, Client}.

is_soft([{restart_application, ssl}]) ->	       
    false;
is_soft(_) ->	
    true.
