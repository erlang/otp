%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: The HDLT server module.
%%          This is just a stub, making future expansion easy.
%%          All code in this module is executed in the local node!
%%----------------------------------------------------------------------

-module(hdlt_server).

-export([start/1, stop/0, start_inets/0, start_service/1]).

-export([proxy/1]).

-include_lib("kernel/include/file.hrl").
-include("hdlt_logger.hrl").


-define(PROXY, hdlt_proxy).


%% This function is used to start the proxy process
%% This function is called *after* the nodes has been 
%% "connected" with the controller/collector node.

start(Debug) ->
    proc_lib:start(?MODULE, proxy, [Debug]).

stop() ->
    ?PROXY ! stop.

start_inets() ->
    ?PROXY ! start_inets.

start_service(Config) ->
    ?PROXY ! {server_start, Config, self()},
    receive
	{server_start_result, Result} ->
	    Result
    after 15000 ->
	    {error, timeout}
    end.


proxy(Debug) ->
    process_flag(trap_exit, true),
    erlang:register(?PROXY, self()),
    ?SET_NAME("HDLT PROXY"),
    ?SET_LEVEL(Debug), 
    ?LOG("starting", []),
    Ref = await_for_controller(10), 
    CtrlNode = node(Ref), 
    erlang:monitor_node(CtrlNode, true),
    proc_lib:init_ack({ok, self()}),
    ?DEBUG("started", []),
    proxy_loop(Ref, CtrlNode).

await_for_controller(N) when N > 0 ->
    case global:whereis_name(hdlt_ctrl) of
	Pid when is_pid(Pid) ->
	    erlang:monitor(process, Pid);
	_ ->
	    timer:sleep(1000),
	    await_for_controller(N-1)
    end;
await_for_controller(_) ->
    proc_lib:init_ack({error, controller_not_found, nodes()}),
    timer:sleep(500),
    halt().

    
proxy_loop(Ref, CtrlNode) ->
    ?DEBUG("await command", []),
    receive
	stop ->
	    ?LOG("received stop", []),
	    halt();

	start_inets ->
	    ?LOG("start the inets service framework", []),
	    case (catch inets:start()) of
		ok ->
		    ?LOG("framework started", []),
		    proxy_loop(Ref, CtrlNode);
		Error ->
		    ?LOG("failed starting inets service framework: "
			 "~n   Error: ~p", [Error]),
		    halt()
	    end;

	{server_start, Config, From} ->
	    ?LOG("start-server", []),
	    maybe_start_crypto_and_ssl(Config),
	    %% inets:enable_trace(max, "/tmp/inets-httpd-trace.log", httpd),
	    %% inets:enable_trace(max, "/tmp/inets-httpd-trace.log", all),
	    case (catch inets:start(httpd, Config)) of
		{ok, _} ->
		    ?LOG("server started when"
			 "~n   which(inets): ~p"
			 "~n   RootDir:      ~p"
			 "~n   System info:  ~p", [code:which(inets), 
						   code:root_dir(), 
						   get_node_info()]),
		    From ! {server_start_result, ok},
		    proxy_loop(Ref, CtrlNode);
		Error ->
		    ?INFO("server start failed"
			  "~n   Error: ~p", [Error]),
		    From ! {server_start_result, Error},
		    halt()
	    end;

	{nodedown, CtrlNode} ->
	    ?LOG("received nodedown for controller node - terminate", []), 
	    halt();

	{'DOWN', Ref, process, _, _} ->
	    ?LOG("received DOWN message for controller - terminate", []),
	    %% The controller has terminated, time to die
	    halt()

    end.


maybe_start_crypto_and_ssl(Config) ->
    case lists:keysearch(socket_type, 1, Config) of
	{value, {socket_type, SocketType}} when ((SocketType =:= ssl) orelse
						 (SocketType =:= ossl) orelse
						 (SocketType =:= essl)) ->
	    ?LOG("maybe start crypto and ssl", []),
	    (catch crypto:start()), 
	    ssl:start();
	_ ->
	    ok
    end.


get_node_info() ->
    [{cpu_topology,        erlang:system_info(cpu_topology)},
     {heap_type,           erlang:system_info(heap_type)},
     {nof_schedulers,      erlang:system_info(schedulers)},
     {otp_release,         erlang:system_info(otp_release)}, 
     {version,             erlang:system_info(version)}, 
     {system_version,      erlang:system_info(system_version)},
     {system_architecture, erlang:system_info(system_architecture)}].

