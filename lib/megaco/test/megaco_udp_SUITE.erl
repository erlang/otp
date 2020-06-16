%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2020 All Rights Reserved.
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
%% Purpose:
%%----------------------------------------------------------------------
-module(megaco_udp_SUITE).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include_lib("megaco/src/udp/megaco_udp.hrl").
-include("megaco_test_lib.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
	 suite/0, all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2,

	 start_normal/1,
	 start_invalid_opt/1,
         start_and_stop/1,
	 sendreceive/1,
         block_unblock/1,
	 socket_failure/1

         ]).


%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([
	 receive_message/4,
	 process_received_message/4
        ]).


%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

-define(CH,             megaco_test_command_handler).
-define(TEST_VERBOSITY, debug).


%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------


%%======================================================================
%% Common Test interface functions
%%======================================================================

suite() -> 
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    [
     {group, start},
     {group, sending},
     {group, error}
    ].

groups() -> 
    [
     {start,   [], start_cases()},
     {sending, [], sending_cases()},
     {error,   [], error_cases()}
    ].

start_cases() ->
    [
     start_normal,
     start_invalid_opt,
     start_and_stop
    ].

sending_cases() ->
    [
     sendreceive,
     block_unblock
    ].

error_cases() ->
    [
     socket_failure
    ].



%%
%% -----
%%

init_per_suite(suite) ->
    [];
init_per_suite(doc) ->
    [];
init_per_suite(Config0) when is_list(Config0) ->

    ?ANNOUNCE_SUITE_INIT(),

    p("init_per_suite -> entry with"
      "~n      Config: ~p"
      "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    case ?LIB:init_per_suite(Config0) of
        {skip, _} = SKIP ->
            SKIP;

        Config1 when is_list(Config1) ->

            %% We need a (local) monitor on this node also
            megaco_test_sys_monitor:start(),

            p("init_per_suite -> end when"
              "~n      Config: ~p"
              "~n      Nodes:  ~p", [Config1, erlang:nodes()]),

            Config1
    end.

end_per_suite(suite) -> [];
end_per_suite(doc) -> [];
end_per_suite(Config0) when is_list(Config0) ->

    p("end_per_suite -> entry with"
      "~n      Config: ~p"
      "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    megaco_test_sys_monitor:stop(),
    Config1 = ?LIB:end_per_suite(Config0),

    p("end_per_suite -> end when"
      "~n      Nodes:  ~p", [erlang:nodes()]),

    Config1.


%%
%% -----
%%

init_per_group(Group, Config) ->
    ?ANNOUNCE_GROUP_INIT(Group),
    Config.

end_per_group(_Group, Config) ->
    Config.



%%
%% -----
%%

init_per_testcase(Case, Config) ->

    p("init_per_testcase -> entry with"
      "~n   Config: ~p"
      "~n   Nodes:  ~p", [Config, erlang:nodes()]),
    
    megaco_test_global_sys_monitor:reset_events(),

    megaco_test_lib:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->

    p("end_per_testcase -> entry with"
      "~n   Config: ~p"
      "~n   Nodes:  ~p", [Config, erlang:nodes()]),

    p("system events during test: "
      "~n   ~p", [megaco_test_global_sys_monitor:events()]),

    megaco_test_lib:end_per_testcase(Case, Config).




%% =================================================
%%
%% ------------------ start ------------------------
%% 
%% =================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_normal(suite) ->
    [];
start_normal(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Opts = [{port, 0}, {receive_handle, apa}],
    {ok, Pid} = start_case(Opts, ok),
    megaco_udp:stop_transport(Pid),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_invalid_opt(suite) ->
    [];
start_invalid_opt(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Opts = [{port, 0}, {receivehandle, apa}],
    start_case(Opts, error).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_and_stop(suite) ->
    [];
start_and_stop(doc) ->
    ["This test case sets up a connection and then cloises it. "
     "No data is sent. "];
start_and_stop(Config) when is_list(Config) ->
    Factor = ?config(megaco_factor, Config),
    ct:timetrap(Factor * ?SECS(45)),
    Pre = fun() ->
		  p("create nodes"),
		  ServerNode = make_node_name(server),
		  ClientNode = make_node_name(client),
		  Nodes = [ServerNode, ClientNode],
		  ok = ?START_NODES(Nodes),
		  Nodes
	  end,
    Case = fun(X) -> do_start_and_stop(Factor, X) end,
    Post = fun(Nodes) ->
                   p("stop nodes"),
                   ?STOP_NODES(lists:reverse(Nodes))
           end,
    try_tc(start_and_stop, Pre, Case, Post).

do_start_and_stop(Factor, [ServerNode, ClientNode]) ->
    %% Create command sequences
    TOCalc = fun(BaseTO) -> to_calc(Factor, BaseTO) end,
    TO     = TOCalc(?SECS(5)),
    p("create command sequences"),
    ServerPort = 2944,
    ServerCmds = start_and_stop_server_commands(ServerPort),
    {ok, ServerHost} = inet:gethostname(),
    ClientCmds = start_and_stop_client_commands(TO, ServerPort, ServerHost),

    %% Start the test procs used in the test-case, one for each node
    p("start command handlers"),
    Server = server_start_command_handler(ServerNode, ServerCmds),
    p("server command handler started: ~p", [Server]),
    Client = client_start_command_handler(ClientNode, ClientCmds),
    p("client command handler started: ~p", [Client]),
    
    ok     =
        receive
            {operational, Server} ->
                p("received listening message from server [~p] => "
                  "send continue to client [~p]~n", [Server, Client]),
                Client ! {continue, self()},
                ok;
	    {'EXIT', Server, {skip, Reason}} ->
		?SKIP(Reason);
	    {'EXIT', Client, {skip, Reason}} ->
		?SKIP(Reason)
        after TO ->
                {error, server_timeout}
        end,

    ok = await_command_handler_completion([Server, Client], TOCalc(?SECS(20))),
    p("done"),
    ok.


start_and_stop_server_commands(Port) ->
    Opts = [{port, Port}],
    Self = self(),
    [
     #{id   => 1,
       desc => "Command sequence init",
       cmd  => fun(State) ->
		       {ok, State#{parent => Self}}
	       end},

     #{id   => 2,
       desc => "Start transport",
       cmd  => fun(State) ->
		       server_start_transport(State)
	       end},

     #{id   => 3,
       desc => "Open",
       cmd  => fun(State) ->
		      server_open(State, Opts)
	      end},

     #{id   => 4,
       desc => "Notify operational",
       cmd  => fun(State) ->
		      server_notify_operational(State)
	      end},
     
     #{id   => 5,
       desc => "Await nothing",
       cmd  => fun(State) ->
		      server_await_nothing(State, 5000)
	      end},
     
     #{id   => 6,
       desc => "Close",
       cmd  => fun(State) ->
		      server_close(State)
	      end},
     
     #{id   => 7,
       desc => "Stop",
       cmd  => fun(State) ->
		      server_stop_transport(State)
	      end}

    ].

start_and_stop_client_commands(TO, ServerPort, _ServerHost) ->
    Opts = [{port, ServerPort}],
    Self = self(),
    [
     #{id   => 1,
       desc => "Command sequence init",
       cmd  => fun(State) ->
		       {ok, State#{parent => Self}}
	       end},

     #{id   => 2,
       desc => "Start transport",
       cmd  => fun(State) ->
		       client_start_transport(State)
	       end},

     #{id   => 3,
       desc => "Open",
       cmd  => fun(State) ->
		       client_open(State, Opts)
	       end},

     #{id   => 4,
       desc => "Await continue",
       cmd  => fun(State) ->
		       client_await_continue_signal(State, TO)
	       end},

     #{id   => 5,
       desc => "Await nothing",
       cmd  => fun(State) ->
		       client_await_nothing(State, 5000)
	       end},

     #{id   => 6,
       desc => "Close",
       cmd  => fun(State) ->
		       client_close(State)
	       end},

     #{id   => 7,
       desc => "Stop transport",
       cmd  => fun(State) ->
		       client_stop_transport(State)
	       end}
    ].



%% =================================================
%%
%% ------------------ sending ------------------------
%% 
%% =================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sendreceive(suite) ->
    [];
sendreceive(doc) ->
    ["Test send and receive with the UDP transport. "];
sendreceive(Config) when is_list(Config) ->
    Factor = ?config(megaco_factor, Config),
    ct:timetrap(Factor * ?SECS(30)),
    Pre = fun() ->
		  p("create nodes"),
		  ServerNode = make_node_name(server),
		  ClientNode = make_node_name(client),
		  Nodes = [ServerNode, ClientNode],
		  ok = ?START_NODES(Nodes),
		  Nodes
	  end,
    Case = fun(X) -> do_sendreceive(Factor, X) end,
    Post = fun(Nodes) ->
                   p("stop nodes"),
                   ?STOP_NODES(lists:reverse(Nodes))
           end,
    try_tc(sendreceive, Pre, Case, Post).

do_sendreceive(Factor, [ServerNode, ClientNode]) ->
    %% Create command sequences
    p("create command sequences"),
    TOCalc = fun(BaseTO) -> to_calc(Factor, BaseTO) end,
    TO     = TOCalc(?SECS(5)),
    ServerPort = 2944,
    ServerCmds = sendreceive_server_commands(TO, ServerPort),
    {ok, ServerHost} = inet:gethostname(),
    ClientCmds = sendreceive_client_commands(TO, ServerPort, ServerHost),

    %% Start the test procs used in the test-case, one for each node
    p("start command handlers"),
    Server = server_start_command_handler(ServerNode, ServerCmds),
    p("server command handler started: ~p", [Server]),
    Client = client_start_command_handler(ClientNode, ClientCmds),
    p("client command handler started: ~p", [Client]),

    ok =
        receive
            {operational, Server} ->
                p("received operational message from server [~p] => "
                  "send continue to client [~p]~n", [Server, Client]),
                Client ! {continue, self()},
                ok;
	    {'EXIT', Server, {skip, Reason}} ->
		?SKIP(Reason);
	    {'EXIT', Client, {skip, Reason}} ->
		?SKIP(Reason)        
	after TO ->
                {error, server_timeout}
        end,

    ok = await_command_handler_completion([Server, Client], TOCalc(?SECS(20))),
    p("done"),
    ok.


sendreceive_server_commands(TO, Port) ->
    Opts = [{port, Port}], 
    Self = self(),
    [
     #{id   => 1,
       desc => "Command sequence init",
       cmd  => fun(State) -> 
		       {ok, State#{parent => Self}} 
	       end},

     #{id   => 2,
       desc => "Start transport",
       cmd  => fun(State) -> 
		       server_start_transport(State) 
	       end},

     #{id   => 3,
       desc => "Open",
       cmd  => fun(State) ->
		       server_open(State, Opts)
	       end},

     #{id   => 4,
       desc => "Notify operational",
       cmd  => fun(State) ->
		       server_notify_operational(State)
	       end},

     #{id   => 5,
       desc => "Await initial message (ping)",
       cmd  => fun(State) -> 
		       server_await_initial_message(State, "ping", TO)
	       end},

     #{id   => 6,
       desc => "Send reply (pong) to initial message",
       cmd  => fun(State) -> 
		       server_send_message(State, "pong") 
	       end},

     #{id   => 7,
       desc => "Await nothing before sending a message (hejsan)",
       cmd  => fun(State) -> 
		       server_await_nothing(State, TO div 5)
	       end},

     #{id   => 8,
       desc => "Send message (hejsan)",
       cmd  => fun(State) -> 
		       server_send_message(State, "hejsan") 
	       end},

     #{id   => 9,
       desc => "Await reply (hoppsan) to message",
       cmd  => fun(State) -> 
		       server_await_message(State, "hoppsan", TO div 5) 
	       end},

     #{id   => 10,
       desc => "Await nothing before closing",
       cmd  => fun(State) -> 
		       server_await_nothing(State, TO div 5)
	       end},
     
     #{id   => 11,
       desc => "Close",
       cmd  => fun(State) -> 
		       server_close(State) 
	       end},
     
     #{id   => 12,
       desc => "Await nothing before stopping transport",
       cmd  => fun(State) -> 
		       server_await_nothing(State, TO div 5)
	       end},
     
     #{id   => 13,
       desc => "Stop",
       cmd  => fun(State) -> 
		       server_stop_transport(State) 
	       end}
    ].

sendreceive_client_commands(TO, ServerPort, ServerHost) ->
    OwnPort = ServerPort+1, 
    Opts    = [{port, OwnPort}], 
    Self    = self(),
    [
     #{id   => 1,
       desc => "Command sequence init",
       cmd  => fun(State) -> 
		       {ok, State#{parent => Self}} 
	       end},

     #{id   => 2,
       desc => "Start transport",
       cmd  => fun(State) -> 
		       client_start_transport(State) 
	       end},

     #{id   => 3,
       desc => "Open",
       cmd  => fun(State) ->
		       client_open(State, Opts)
	       end},

     #{id   => 4,
       desc => "Await continue",
       cmd  => fun(State) ->
		       client_await_continue_signal(State, TO)
	       end},

     #{id   => 5,
       desc => "Connect",
       cmd  => fun(State) ->
		       client_connect(State, ServerHost, ServerPort)
	       end},

     #{id   => 6,
       desc => "Send initial message (ping)",
       cmd  => fun(State) -> 
		       client_send_message(State, "ping") 
	       end},

     #{id   => 7,
       desc => "Await reply (pong) to initial message",
       cmd  => fun(State) -> 
		       client_await_message(State, "pong", TO div 5)
	       end},

     #{id   => 8,
       desc => "Await message (hejsan)",
       cmd  => fun(State) -> 
		       client_await_message(State, "hejsan", TO)
	       end},

     #{id   => 9,
       desc => "Send reply (hoppsan) to message",
       cmd  => fun(State) -> 
		       client_send_message(State, "hoppsan") 
	       end},

     #{id   => 10,
       desc => "Await nothing before closing",
       cmd  => fun(State) -> 
		       client_await_nothing(State, TO div 5)
	       end},

     #{id   => 11,
       desc => "Close",
       cmd  => fun(State) -> 
		       client_close(State) 
	       end},

     #{id   => 12,
       desc => "Await nothing before stopping transport",
       cmd  => fun(State) -> 
		       client_await_nothing(State, TO div 5)
	       end},

     #{id   => 13,
       desc => "Stop transport",
       cmd  => fun(State) -> 
		       client_stop_transport(State) 
	       end}
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

block_unblock(suite) ->
    [];
block_unblock(doc) ->
    ["Test the block/unblock functions of the UDP transport. "];
block_unblock(Config) when is_list(Config) ->
    Factor = ?config(megaco_factor, Config),
    ct:timetrap(Factor * ?MINS(1)),
    Pre = fun() ->
		  p("create nodes"),
		  ServerNode = make_node_name(server),
		  ClientNode = make_node_name(client),
		  Nodes = [ServerNode, ClientNode],
		  ok = ?START_NODES(Nodes),
		  Nodes
	  end,
    Case = fun(X) -> do_block_unblock(Factor, X) end,
    Post = fun(Nodes) ->
                   p("stop nodes"),
                   ?STOP_NODES(lists:reverse(Nodes))
           end,
    try_tc(block_unblock, Pre, Case, Post).

do_block_unblock(Factor, [ServerNode, ClientNode]) ->
    %% Create command sequences
    p("create command sequences"),
    TOCalc = fun(BaseTO) -> to_calc(Factor, BaseTO) end,
    TO     = TOCalc(?SECS(5)),
    ServerPort = 2944,
    ServerCmds = block_unblock_server_commands(TO, ServerPort),
    {ok, ServerHost} = inet:gethostname(),
    ClientCmds = block_unblock_client_commands(TO, ServerPort, ServerHost),

    %% Start the test procs used in the test-case, one for each node
    p("start command handlers"),
    Server = server_start_command_handler(ServerNode, ServerCmds),
    p("server command handler started: ~p", [Server]),
    Client = client_start_command_handler(ClientNode, ClientCmds),
    p("client command handler started: ~p", [Client]),

    %% Wait for the server to become ready for operation
    %% and then tell the client to continue
    ok =
        receive
            {operational, Server} ->
                p("received operational message from server [~p] => "
                  "send continue to client [~p]~n", [Server, Client]),
                Client ! {continue, self()},
                ok;
	    {'EXIT', Server, {skip, Reason1}} ->
		?SKIP(Reason1);
	    {'EXIT', Client, {skip, Reason2}} ->
		?SKIP(Reason2)
        after TO ->
                {error, server_timeout}
        end,

    %% Wait for the client to become blocked
    %% and then tell the server to continue
    ok = 
	receive
	    {blocked, Client} ->
		p("received blocked message from client [~p] => "
		  "send continue to server [~p]~n", [Client, Server]),
		Server ! {continue, self()},
		ok;
	    {'EXIT', Server, {skip, Reason3}} ->
		?SKIP(Reason3);
	    {'EXIT', Client, {skip, Reason4}} ->
		?SKIP(Reason4)
	after TO ->
		{error, timeout}
	end,

    ok = await_command_handler_completion([Server, Client], TOCalc(?SECS(20))),
    p("done"),
    ok.


block_unblock_server_commands(TO, Port) ->
    Opts = [{port, Port}], 
    Self = self(),
    [
     #{id   => 1,
       desc => "Command sequence init",
       cmd  => fun(State) -> 
		       {ok, State#{parent => Self}} 
	       end},

     #{id   => 2,
       desc => "Start transport",
       cmd  => fun(State) -> 
		       server_start_transport(State) 
	       end},

     #{id   => 3,
       desc => "Open",
       cmd  => fun(State) ->
		       server_open(State, Opts)
	       end},

     #{id   => 4,
       desc => "Notify operational",
       cmd  => fun(State) ->
		       server_notify_operational(State)
	       end},

     #{id   => 5,
       desc => "Await initial message (ping)",
       cmd  => fun(State) -> 
		       server_await_initial_message(State, "ping", TO)
	       end},

     #{id   => 6,
       desc => "Send reply (pong) to initial message",
       cmd  => fun(State) -> 
		       server_send_message(State, "pong") 
	       end},

     #{id   => 7,
       desc => "Await continue",
       cmd  => fun(State) ->
		       server_await_continue_signal(State, TO)
	       end},

     #{id   => 8,
       desc => "Send message (hejsan)",
       cmd  => fun(State) -> 
		       server_send_message(State, "hejsan") 
	       end},

     #{id   => 9,
       desc => "Await nothing before receiving (hoppsan) reply",
       cmd  => fun(State) -> 
		       server_await_nothing(State, TO)
	       end},

     #{id   => 10,
       desc => "Await reply (hoppsan) to message",
       cmd  => fun(State) -> 
		       server_await_message(State, "hoppsan", TO div 2)
	       end},

     #{id   => 11,
       desc => "Await nothing before closing",
       cmd  => fun(State) -> 
		       server_await_nothing(State, TO div 5)
	       end},

     #{id   => 12,
       desc => "Close",
       cmd  => fun(State) -> 
		       server_close(State) 
	       end},

     #{id   => 13,
       desc => "Await nothing before stopping transport",
       cmd  => fun(State) -> 
		       server_await_nothing(State, TO div 5)
	       end},

     #{id   => 14,
       desc => "Stop",
       cmd  => fun(State) -> 
		       server_stop_transport(State) 
	       end}

    ].

block_unblock_client_commands(TO, ServerPort, ServerHost) ->
    OwnPort = ServerPort+1, 
    Opts    = [{port, OwnPort}], 
    Self    = self(),
    [
     #{id   => 1,
       desc => "Command sequence init",
       cmd  => fun(State) -> 
		       {ok, State#{parent => Self}} 
	       end},

     #{id   => 2,
       desc => "Start transport",
       cmd  => fun(State) -> 
		       client_start_transport(State) 
	       end},

     #{id   => 3,
       desc => "Open",
       cmd  => fun(State) ->
		       client_open(State, Opts)
	       end},

     #{id   => 4,
       desc => "Await continue",
       cmd  => fun(State) ->
		       client_await_continue_signal(State, TO)
	       end},

     #{id   => 5,
       desc => "[pseudo] Connect",
       cmd  => fun(State) ->
		       client_connect(State, ServerHost, ServerPort)
	       end},

     #{id   => 6,
       desc => "Send initial message (ping)",
       cmd  => fun(State) -> 
		       client_send_message(State, "ping") 
	       end},

     #{id   => 7,
       desc => "Await reply (pong) to initial message",
       cmd  => fun(State) -> 
		       client_await_message(State, "pong", TO div 5)
	       end},

     #{id   => 8,
       desc => "Pre-Block info",
       cmd  => fun(#{socket := Socket} = State) -> 
                       p("Socket Info: "
                         "~n      Port Info: ~p", [erlang:port_info(Socket)]),
		       {ok, State}
	       end},

     #{id   => 9,
       desc => "Block",
       cmd  => fun(State) -> 
		       client_block(State) 
	       end},

     #{id   => 10,
       desc => "Post-Block info",
       cmd  => fun(#{socket := Socket} = State) ->
                       Active =
                           case inet:getopts(Socket, [active]) of
                               {ok, [{active, Act}]} ->
                                   Act;
                               _ ->
                                   undefined
                           end,
                       p("Socket Info: "
                         "~n      Active:    ~p"
                         "~n      Port Info: ~p",
                         [Active, erlang:port_info(Socket)]),
		       {ok, State}
	       end},

     #{id   => 11,
       desc => "Notify blocked",
       cmd  => fun(State) -> 
		       client_notify_blocked(State) 
	       end},

     #{id   => 12,
       desc => "Await nothing before unblocking",
       cmd  => fun(#{socket := Socket} = State) -> 
                       Fail =
                           fun(_) ->
                                   Active =
                                       case inet:getopts(Socket, [active]) of
                                           {ok, [{active, Act}]} ->
                                               Act;
                                           _ ->
                                               undefined
                                       end,
                                   p("Socket Info: "
                                     "~n      Active:    ~p"
                                     "~n      Port Info: ~p",
                                     [Active, erlang:port_info(Socket)]),
                                   ok
                           end,
		       client_await_nothing(State, Fail, TO)
	       end},

     #{id   => 13,
       desc => "Pre-Unblock info",
       cmd  => fun(#{socket := Socket} = State) ->
                       Active =
                           case inet:getopts(Socket, [active]) of
                               {ok, [{active, Act}]} ->
                                   Act;
                               _ ->
                                   undefined
                           end,
                       p("Socket Info: "
                         "~n      Active:    ~p"
                         "~n      Port Info: ~p",
                         [Active, erlang:port_info(Socket)]),
		       {ok, State}
	       end},

     #{id   => 14,
       desc => "Unblock",
       cmd  => fun(State) -> 
		       client_unblock(State) 
	       end},

     #{id   => 15,
       desc => "Post-Unblock info",
       cmd  => fun(#{socket := Socket} = State) ->
                       Active =
                           case inet:getopts(Socket, [active]) of
                               {ok, [{active, Act}]} ->
                                   Act;
                               _ ->
                                   undefined
                           end,
                       p("Socket Info: "
                         "~n      Active:    ~p"
                         "~n      Port Info: ~p",
                         [Active, erlang:port_info(Socket)]),
		       {ok, State}
	       end},

     #{id   => 16,
       desc => "Await message (hejsan)",
       cmd  => fun(State) -> 
		       client_await_message(State, "hejsan", TO)
	       end},

     #{id   => 17,
       desc => "Send reply (hoppsan) to message",
       cmd  => fun(State) -> 
		       client_send_message(State, "hoppsan") 
	       end},

     #{id   => 18,
       desc => "Await nothing before closing",
       cmd  => fun(State) -> 
		       client_await_nothing(State, TO)
	       end},

     #{id   => 19,
       desc => "Close",
       cmd  => fun(State) -> 
		       client_close(State) 
	       end},

     #{id   => 20,
       desc => "Await nothing before stopping transport",
       cmd  => fun(State) -> 
		       client_await_nothing(State, TO)
	       end},

     #{id   => 21,
       desc => "Stop transport",
       cmd  => fun(State) -> 
		       client_stop_transport(State) 
	       end}
    ].


%% =================================================
%%
%% ------------------ errors ------------------------
%% 
%% =================================================

socket_failure(suite) ->
    [];
socket_failure(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    failing_socket().


%%======================================================================
%% Test functions
%%======================================================================

start_case(Opts, Expect) ->
    case (catch megaco_udp:start_transport()) of
	{ok, Pid} ->
	    case (catch megaco_udp:open(Pid, Opts)) of
		{ok, _Handle, _CtrlPid} when Expect =:= ok ->
		    {ok, Pid};
		{ok, Handle, CtrlPid} ->
		    megaco_udp:stop_transport(Pid),
		    ?ERROR({unexpected_start_sucesss, Handle, CtrlPid});
		{error, _Reason} when Expect =:= error ->
		    megaco_udp:stop_transport(Pid),
		    ok;
		{error, Reason} ->
		    megaco_udp:stop_transport(Pid),
		    ?ERROR({unexpected_start_failure, Reason});
		Error ->
		    ?ERROR({unexpected_result, Error})
	    end;
	{error, Reason} ->
	    ?ERROR({failed_starting_transport, Reason})
    end.


failing_socket() ->
    ?SKIP(not_yet_implemented).



%%----------------------------------------------------------------------
%% Message Callback functions 
%%----------------------------------------------------------------------

receive_message(ReceiveHandle, ControlPid, SendHandle, BinMsg) 
  when is_pid(ReceiveHandle) andalso is_binary(BinMsg) ->
    Msg = binary_to_list(BinMsg), 
    ReceiveHandle ! {receive_message, {ControlPid, SendHandle, Msg}},
    ok.
    
process_received_message(ReceiveHandle, ControlPid, SendHandle, BinMsg) 
  when is_pid(ReceiveHandle) andalso is_binary(BinMsg) ->
    Msg = binary_to_list(BinMsg), 
    ReceiveHandle ! {process_received_message, {ControlPid, SendHandle, Msg}},
    ok.


%%======================================================================
%% Internal functions
%%======================================================================

%% -------  Server command handler and utility functions ----------

server_start_command_handler(Node, Commands) ->
    start_command_handler(Node, Commands, #{}, "server").

server_start_transport(State) when is_map(State) ->
    case (catch megaco_udp:start_transport()) of
	{ok, Ref} ->
	    {ok, State#{transport_ref => Ref}};
	Error ->
	    Error
    end.

server_open(#{transport_ref := Ref} = State, Options) 
  when is_list(Options) ->
    Opts = [{receive_handle, self()}, {module, ?MODULE} | Options], 
    try megaco_udp:open(Ref, Opts) of
	{ok, Socket, ControlPid} ->
	    {ok, State#{handle      => {socket, Socket},  % Temporary
			control_pid => ControlPid}};
	{error, {could_not_open_udp_port, eaddrinuse}} ->
	    {skip, {server, eaddrinuse}};
	{error, _} = ERROR ->
	    ERROR
    catch
        C:E:S ->
            {error, {catched, C, E, S}}
    end.

server_notify_operational(#{parent := Parent} = State) ->
    Parent ! {operational, self()},
    {ok, State}.

server_await_continue_signal(#{parent := Parent} = State, Timeout) ->
    receive
	{continue, Parent} ->
	    {ok, State}
    after Timeout ->
	    {error, timeout}
    end.
    
server_await_initial_message(State, InitialMessage, Timeout) 
  when is_map(State) ->
    receive 
	{receive_message, {ControlPid, Handle, InitialMessage}} ->
	    p("received expected event with: "
	      "~n   ControlPid: ~p"
	      "~n   Handle:     ~p", [ControlPid, Handle]),
	    NewState = State#{handle => Handle},
	    {ok, NewState};

	Any ->
	    p("received unexpected event: ~p", [Any]),
	    {error, {unexpected_event, Any}}

    after Timeout ->
	    {error, timeout}
    end.

server_send_message(#{handle := Handle} = State, Message) ->
    Bin = if
	      is_list(Message) ->
		  list_to_binary(Message);
	      true ->
		  Message
	  end,
    megaco_udp:send_message(Handle, Bin),
    {ok, State}.

server_await_nothing(State, Timeout) 
  when is_map(State) ->
    receive 
	Any ->
	    p("received unexpected event: ~p", [Any]),
	    {error, {unexpected_event, Any}}

    after Timeout ->
	    {ok, State}
    end.

server_await_message(State, ExpectMessage, Timeout) 
  when is_map(State) ->
    receive
	{receive_message, {_, _, ExpectMessage}} ->
	    p("received expected message [~p]", [ExpectMessage]),
	    {ok, State};

	Any ->
	    p("received unexpected event: ~p", [Any]),
	    {error, {unexpected_event, Any}}

    after Timeout ->
	    {error, timeout}
    end.

server_close(#{handle := {socket, Socket}} = State) ->
    megaco_udp:close(Socket),
    {ok, State#{handle => undefined, control_pid => undefined}};
server_close(#{handle := Handle} = State) 
  when (Handle =/= undefined) ->
    megaco_udp:close(Handle),
    {ok, State#{handle => undefined, control_pid => undefined}}.

server_stop_transport(#{transport_ref := Ref} = State) 
  when (Ref =/= undefined) ->
    megaco_udp:stop_transport(Ref),
    {ok, State#{transport_ref => undefined}}.


%% -------  Client command handler and utility functions ----------

client_start_command_handler(Node, Commands) ->
    start_command_handler(Node, Commands, #{}, "client").
		  
client_start_transport(State) when is_map(State) ->
    case (catch megaco_udp:start_transport()) of
	{ok, Ref} ->
	    {ok, State#{transport_ref => Ref}};
	Error ->
	    Error
    end.

client_open(#{transport_ref := Ref} = State, Options) 
  when is_list(Options) ->
    Opts = [{receive_handle, self()}, {module, ?MODULE} | Options], 
    try megaco_udp:open(Ref, Opts) of
	{ok, Socket, ControlPid} ->
	    {ok, State#{handle      => {socket, Socket},
                        socket      => Socket,
			control_pid => ControlPid}};
	{error, {could_not_open_udp_port, eaddrinuse}} ->
	    {skip, {client, eaddrinuse}};
	{error, _} = ERROR ->
	    ERROR
    catch
        C:E:S ->
            {error, {catched, C, E, S}}
    end.

client_await_continue_signal(#{parent := Parent} = State, Timeout) ->
    receive
	{continue, Parent} ->
	    {ok, State}
    after Timeout ->
	    {error, timeout}
    end.
    
client_notify_blocked(#{parent := Parent} = State) ->
    Parent ! {blocked, self()},
    {ok, State}.

client_await_nothing(State, Timeout) ->
    client_await_nothing(State, fun(_) -> ok end, Timeout).

client_await_nothing(State, Fail, Timeout)
  when is_map(State) andalso is_function(Fail, 1) ->
    receive 
	Any ->
	    p("received unexpected event: ~p", [Any]),
            (catch Fail(Any)),
	    {error, {unexpected_event, Any}}
    after Timeout ->
	    {ok, State}
    end.

client_connect(#{handle := {socket, Socket}} = State, Host, Port) ->
    Handle = megaco_udp:create_send_handle(Socket, Host, Port),
    {ok, State#{handle => Handle}}.

client_send_message(#{handle := Handle} = State, Message) ->
    Bin = if
	      is_list(Message) ->
		  list_to_binary(Message);
	      true ->
		  Message
	  end,
    megaco_udp:send_message(Handle, Bin),
    {ok, State}.

client_await_message(State, ExpectMessage, Timeout) 
  when is_map(State) ->
    receive
	{receive_message, {_, _, ExpectMessage}} ->
	    {ok, State};

	Any ->
	    p("received unexpected event: ~p", [Any]),
	    {error, {unexpected_event, Any}}

    after Timeout ->
	    {error, timeout}
    end.

client_block(#{handle := Handle} = State) 
  when (Handle =/= undefined) ->
    ok = megaco_udp:block(Handle),
    {ok, State}.

client_unblock(#{handle := Handle} = State) 
  when (Handle =/= undefined) ->
    ok = megaco_udp:unblock(Handle),
    {ok, State}.

client_close(#{handle := {socket, Socket}} = State) ->
    megaco_udp:close(Socket),
    {ok, State#{handle => undefined, control_pid => undefined}};
client_close(#{handle := Handle} = State) 
  when (Handle =/= undefined) ->
    megaco_udp:close(Handle),
    {ok, State#{handle => undefined, control_pid => undefined}}.

client_stop_transport(#{transport_ref := Ref} = State) 
  when (Ref =/= undefined) ->
    megaco_udp:stop_transport(Ref),
    {ok, State#{transport_ref => undefined}}.

    
%% -------- Command handler interface ---------

start_command_handler(Node, Commands, State, ShortName) ->
    ?CH:start(Node, Commands, State, ShortName).


await_command_handler_completion(Pids, Timeout) ->
    ?CH:await_completion(Pids, Timeout).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_tc(TCName, Pre, Case, Post) ->
    try_tc(TCName, "TEST", ?TEST_VERBOSITY, Pre, Case, Post).

try_tc(TCName, Name, Verbosity, Pre, Case, Post) ->
    ?TRY_TC(TCName, Name, Verbosity, Pre, Case, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ------- Misc functions --------

make_node_name(Name) ->
    case string:tokens(atom_to_list(node()), [$@]) of
        [_,Host] ->
            list_to_atom(lists:concat([atom_to_list(Name) ++ "@" ++ Host]));
        _ ->
            exit("Test node must be started with '-sname'")
    end.


to_calc(1 = _Factor, BaseTO) when is_integer(BaseTO) andalso (BaseTO > 0) ->
    BaseTO;
to_calc(Factor, BaseTO) when is_integer(Factor) andalso (Factor > 0) andalso
                             is_integer(BaseTO) andalso (BaseTO > 0) ->
    trunc( ((Factor + 1) / 2) * BaseTO ).


p(F) ->
    p(F, []).

p(F, A) ->
    p(get(sname), F, A).

p(S, F, A) when is_list(S) ->
    io:format("*** [~s] ~p ~s ***" 
	      "~n   " ++ F ++ "~n", 
	      [?FTS(), self(), S | A]);
p(_S, F, A) ->
    io:format("*** [~s] ~p *** "
	      "~n   " ++ F ++ "~n", 
	      [?FTS(), self() | A]).


%% ms() ->
%%     erlang:monotonic_time(milli_seconds).
    

