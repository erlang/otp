%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2023. All Rights Reserved.
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

-behaviour(ct_suite).

-include("ssl_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").

%% Common test
-export([all/0,
         init_per_suite/1,
         init_per_testcase/2,
         end_per_suite/1,
         end_per_testcase/2,
         upgrade_init/2,
         upgrade_upgraded/2,
         upgrade_downgraded/2]).

%% Test cases
-export([minor_upgrade/1,
         major_upgrade/1]).

%% spawn/apply export
-export([result_proxy_init/1, use_connection/1]).


-record(state, {
	  config,
	  server,
	  client,
	  soft,
	  result_proxy,
	  skip
	 }).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() -> 
    [
     minor_upgrade,
     major_upgrade
    ].

init_per_suite(Config0) ->
    catch application:stop(crypto),
    try application:start(crypto) of
        ok ->
            ssl_test_lib:clean_start(),
            case ct_release_test:init(Config0) of
                {skip, Reason} ->
                    {skip, Reason};
                Config ->
                    ssl_test_lib:make_rsa_cert(Config)
            end
    catch _:_ ->
              {skip, "Crypto did not start"}
    end.

end_per_suite(Config) ->
    ct_release_test:cleanup(Config),
    application:stop(crypto).

init_per_testcase(_TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({minutes, 1}),
    Config.

end_per_testcase(_TestCase, Config) ->     
    Config.

upgrade_init(CtData, State) -> 
    {ok,{FromVsn,ToVsn}} = ct_release_test:get_app_vsns(CtData, ssl),
    upgrade_init(FromVsn, ToVsn, CtData, State).

upgrade_upgraded(_, #state{skip = true} = State) ->
    State;
upgrade_upgraded(_, #state{soft = false, config = Config, result_proxy = Pid} = State) ->
    ?CT_PAL("Restart upgrade ~n", []),
    {Server, Client} = restart_start_connection(Config, Pid),
    Result = check_result(Pid, Server, Client),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    ok = Result,
    State;
upgrade_upgraded(_, #state{server = Server0, client = Client0,
			   config = Config, soft = true,
			   result_proxy = Pid} = State) ->
    ?CT_PAL("Soft upgrade: ~n", []),
    Server0 ! changed_version,
    Client0 ! changed_version,
    Result = check_result(Pid, Server0, Client0),
    ssl_test_lib:close(Server0),
    ssl_test_lib:close(Client0),
    ok = Result,
    {Server, Client} = soft_start_connection(Config, Pid),
    State#state{server = Server, client = Client}.

upgrade_downgraded(_, #state{skip = true} = State) ->
    State;
upgrade_downgraded(_, #state{soft = false, config = Config, result_proxy = Pid} = State) ->
    ?CT_PAL("Restart downgrade: ~n", []),
    {Server, Client} = restart_start_connection(Config, Pid),
    Result = check_result(Pid, Server, Client),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    Pid ! stop,
    ok = Result,
    State;
upgrade_downgraded(_, #state{server = Server, client = Client, soft = true, result_proxy = Pid} = State) ->
    ?CT_PAL("Soft downgrade: ~n", []),
    Server ! changed_version,
    Client ! changed_version,
    Result = check_result(Pid, Server, Client),
    Pid ! stop,
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    ok = Result,
    State.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
major_upgrade(Config) when is_list(Config) ->
    ct_release_test:upgrade(ssl, major,{?MODULE, #state{config = Config}}, Config).

minor_upgrade(Config) when is_list(Config) ->
    ct_release_test:upgrade(ssl, minor,{?MODULE, #state{config = Config}}, Config).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
upgrade_init(Ver, Ver, _, State) ->
    %% No upgrade if to and from version is the same!
    State#state{skip = true};
upgrade_init(_, "8.0.2", _, State) ->
    %% Requires stdlib upgrade so it will be a node upgrade!
    State#state{skip = true};
upgrade_init(_, _, CtData, #state{config = Config} = State) ->
    {ok, {_, _, Up, _Down}} = ct_release_test:get_appup(CtData, ssl),
    ?CT_PAL("Up: ~p", [Up]),
    Soft = is_soft(Up), %% It is symmetrical, if upgrade is soft so is downgrade
    Pid = spawn(?MODULE, result_proxy_init, [[]]),
    case Soft of
	true ->
	    {Server, Client} = soft_start_connection(Config, Pid),
	    State#state{server = Server, client = Client,
			soft = Soft,
			result_proxy = Pid};
	false ->
	    State#state{soft = Soft, result_proxy = Pid}
    end.

use_connection(Socket) ->
    ssl_test_lib:send_recv_result_active(Socket),
    receive 
	changed_version ->
	    ssl_test_lib:send_recv_result_active(Socket)
    end.

soft_start_connection(Config, ResulProxy) ->
    ClientOpts = proplists:get_value(client_rsa_verify_opts, Config),
    ServerOpts = proplists:get_value(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = start_server([{node, ServerNode}, {port, 0},
			   {from, ResulProxy},
			   {mfa, {?MODULE, use_connection, []}},
			   {options, ServerOpts}]),
    
    Port = inet_port(ResulProxy, Server),
    Client = start_client([{node, ClientNode}, {port, Port},
			   {host, Hostname},
			   {from, ResulProxy},
			   {mfa, {?MODULE, use_connection, []}},
			   {options, ClientOpts}]),
    {Server, Client}.

restart_start_connection(Config, ResulProxy) ->
    ClientOpts = proplists:get_value(client_rsa_verify_opts, Config),
    ServerOpts = proplists:get_value(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = start_server([{node, ServerNode}, {port, 0},
					{from, ResulProxy},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = inet_port(ResulProxy, Server),
    Client = start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, ResulProxy},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ClientOpts}]),
    {Server, Client}.

is_soft([{restart_application, ssl}]) ->	       
    false;
is_soft(_) ->	
    true.

result_proxy_init(Args) ->
    result_proxy_loop(Args).

result_proxy_loop(Args) ->
    receive
	{Pid, {check_result, Server, Client}} ->
	    Result = do_check_result(Server, ok, Client, ok),
	    Pid ! {self(), Result},
	    result_proxy_loop(Args);
	{Pid, port, Server} ->
	    Port = recv_port(Server),
	    Pid ! Port,
	    result_proxy_loop(Args);
	{Pid, listen} ->
	    recv_listen(),
	    Pid ! ok,
	    result_proxy_loop(Args);
	{Pid, connected} ->
	    Connected = recv_connected(),
	     Pid ! Connected,
	    result_proxy_loop(Args)
    end.

check_result(Pid, Server, Client) ->
    Pid ! {self(), {check_result, Server, Client}},
    receive
	{Pid, Result} ->
	    Result
    end.

do_check_result(Server, ServerMsg, Client, ClientMsg) ->
    receive
	{Server, ServerMsg} ->
	    do_check_result(Client, ClientMsg);

	{Client, ClientMsg} ->
	    do_check_result(Server, ServerMsg);
	Unexpected ->
	    {{expected, {Client, ClientMsg}},
	     {expected, {Server, ServerMsg}}, {got, Unexpected}}
    end.

do_check_result(Pid, Msg) ->
    receive
	{Pid, Msg} ->
	    ok;
	Unexpected ->
	    {{expected, {Pid, Msg}},
	     {got, Unexpected}}
    end.

inet_port(Pid, Server) ->
    Pid ! {self(), port, Server},
    receive
	{port, Port} ->
	    Port
    end.

recv_port(Server) ->
    receive
	{Server, {port, Port}} ->
	    {port, Port}
    end.

recv_connected() ->
    receive
	{connected, _Socket} ->
	    ok;
	{connect_failed, Reason} ->
	    {connect_failed, Reason}
    end.


start_server(Args) ->
    Pid = proplists:get_value(from, Args),
    Result = spawn_link(ssl_test_lib, run_server, [Args]),
    Pid ! {self(), listen},
    receive
	ok ->
	    ok
    end,
    Result.

start_client(Args) ->
    Pid = proplists:get_value(from, Args),
    Result = spawn_link(ssl_test_lib, run_client_init, [lists:delete(return_socket, Args)]),
    Pid  ! {self(), connected},
    receive
	ok ->
	    Result;
	Reason ->
	    exit(Reason)
    end.

recv_listen()->
    receive
	{listen, up} ->
	    ok
    end.
