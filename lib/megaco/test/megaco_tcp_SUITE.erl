%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2022. All Rights Reserved.
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
-module(megaco_tcp_SUITE).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("megaco/src/tcp/megaco_tcp.hrl").
-include("megaco_test_lib.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
	 suite/0, all/0, groups/0,
	 init_per_suite/1,    end_per_suite/1, 
         init_per_group/2,    end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2, 

	 start_normal/1,
	 start_invalid_opt/1,
	 start_and_stop/1,
	 sendreceive/1, 
	 block_unblock/1, 
	 socket_failure/1,
	 accept_process/1,
	 accept_supervisor/1,
	 connection_supervisor/1,
	 tcp_server/1

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

%% We have some strange reuseaddr issue (*some* times we eaddrinuse),
%% so this is intended to "ensure" we get no conflict.
-define(SERVER_PORT(C, I),
        2944 + proplists:get_value(adjust_server_port, C) + (I-1)).


%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------


%%======================================================================
%% Common Test interface functions
%%======================================================================

suite() -> 
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    %% This is a temporary messure to ensure that we can 
    %% test the socket backend without effecting *all*
    %% applications on *all* machines.
    %% This flag is set only for *one* host.
    case ?TEST_INET_BACKENDS() of
        true ->
            [
             {group, inet_backend_default},
             {group, inet_backend_inet},
             {group, inet_backend_socket}
            ];
        _ ->
            [
             {group, inet_backend_default}
            ]
    end.

groups() -> 
    [
     {inet_backend_default,          [], inet_backend_default_cases()},
     {inet_backend_inet,             [], inet_backend_inet_cases()},
     {inet_backend_socket,           [], inet_backend_socket_cases()},

     {all,                           [], all_cases()},
     {start,                         [], start_cases()},
     {sending,                       [], sending_cases()},
     {error,                         [], error_cases()}
    ].

inet_backend_default_cases() ->
    [{all, [], all_cases()}].

inet_backend_inet_cases() ->
    [{all, [], all_cases()}].

inet_backend_socket_cases() ->
    [{all, [], all_cases()}].

all_cases() -> 
    [
     {group, start},
     {group, sending},
     {group, error}
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
     socket_failure,
     accept_process,
     accept_supervisor,
     connection_supervisor,
     tcp_server
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

init_per_group(inet_backend_default = Group, Config) ->
    ?ANNOUNCE_GROUP_INIT(Group),
    [{adjust_server_port, 0},
     {socket_create_opts, []} | Config];
init_per_group(inet_backend_inet = Group, Config) ->
    ?ANNOUNCE_GROUP_INIT(Group),
    case ?EXPLICIT_INET_BACKEND() of
        true ->
            %% The environment trumps us,
            %% so only the default group should be run!
            {skip, "explicit inet backend"};
        false ->
            [{adjust_server_port, 100},
             {socket_create_opts, [{inet_backend, inet}]} | Config]
    end;
init_per_group(inet_backend_socket = Group, Config) ->
    ?ANNOUNCE_GROUP_INIT(Group),
    case ?EXPLICIT_INET_BACKEND() of
        true ->
            %% The environment trumps us,
            %% so only the default group should be run!
            {skip, "explicit inet backend"};
        false ->
            [{adjust_server_port, 200},
             {socket_create_opts, [{inet_backend, socket}]} | Config]
    end;
init_per_group(Group, Config) ->
    ?ANNOUNCE_GROUP_INIT(Group),
    Config.

end_per_group(GroupName, Config) when (inet_backend_default =:= GroupName) orelse
                                      (inet_backend_init    =:= GroupName) orelse
                                      (inet_backend_socket  =:= GroupName) ->
    ?SLEEP(?SECS(5)),
    Config;
end_per_group(_GroupName, Config) ->
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
      "~n   Config:  ~p"
      "~n   Nodes:   ~p",
      [Config, erlang:nodes()]),

    p("system events during test: "
      "~n   ~p", [megaco_test_global_sys_monitor:events()]),

    megaco_test_lib:end_per_testcase(Case, Config).




%% ------------------ start ------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_normal(suite) ->
    [];
start_normal(Config) when is_list(Config) ->
    put(sname, "start_normal"),
    p("BEGIN TEST-CASE"), 
    Options = [{port, 20000}, {receive_handle, apa}],
    {ok, Pid} = start_case(Config, Options, ok),
    megaco_tcp:stop_transport(Pid),
    p("done"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_invalid_opt(suite) ->
    [];
start_invalid_opt(Config) when is_list(Config) ->
    put(sname, "start_invalid_opt"),
    p("BEGIN TEST-CASE"), 
    Options = [{port, 20000}, {receivehandle, apa}],
    ok = start_case(Config, Options, error),
    p("done"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_and_stop(suite) ->
    [];
start_and_stop(doc) ->
    ["This test case sets up a connection and then cloises it. "
     "No data is sent. "];
start_and_stop(Config) when is_list(Config) ->
    Pre = fun() ->
		  p("create nodes"),
		  ServerNode = make_node_name(t_sas_server),
		  ClientNode = make_node_name(t_sas_client),
		  Nodes = [ServerNode, ClientNode], 
		  ok = ?START_NODES(Nodes, true),
		  Nodes
	  end,
    Case = fun(X) -> do_start_and_stop(Config, X) end,
    %% Case = fun(X) -> do_start_and_stop(Config, X, 10) end,
    Post = fun(Nodes) ->
                   p("stop nodes"),
                   ?STOP_NODES(lists:reverse(Nodes))
           end,
    try_tc(start_and_stop, Pre, Case, Post).


%% do_start_and_stop(_Config, _Nodes, 0) ->
%%     ok;
%% do_start_and_stop(Config, Nodes, N) ->
%%     do_start_and_stop(Config, Nodes),
%%     do_start_and_stop(Config, Nodes, N-1).

do_start_and_stop(Config, [ServerNode, ClientNode]) ->
    %% Create command sequences
    p("create command sequences"),
    ServerPort = ?SERVER_PORT(Config, 1),  % 2944,
    ServerCmds = start_and_stop_server_commands(Config, ServerPort),
    {ok, ServerHost} = inet:gethostname(),
    ClientCmds = start_and_stop_client_commands(Config, ServerPort, ServerHost),

    %% Start the test procs used in the test-case, one for each node
    p("start command handlers"),
    Server = server_start_command_handler(ServerNode, ServerCmds),
    p("server command handler started: ~p", [Server]),
    Client = client_start_command_handler(ClientNode, ClientCmds),
    p("client command handler started: ~p", [Client]),

    await_server_listening(Server, Client),

    ok = await_command_handler_completion([Server, Client], ?SECS(20)),
    p("done"),
    ok.


start_and_stop_server_commands(Config, Port) ->
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
       desc => "Listen",
       cmd  => fun(State) -> 
                       server_listen(Config, State, Opts) 
	      end},

     #{id   => 4,
       desc => "Notify listening",
       cmd  => fun(State) -> 
		      server_notify_listening(State) 
	      end},

     #{id   => 5,
       desc => "Await nothing",
       cmd  => fun(State) -> 
		      server_await_nothing(State, 6000) 
	      end},

     #{id   => 6,
       desc => "Stop",
       cmd  => fun(State) -> 
		      server_stop_transport(State) 
	      end}

    ].


start_and_stop_client_commands(Config, ServerPort, ServerHost) ->
    Opts = [{port, ServerPort}, {host, ServerHost}], 
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
       desc => "Await continue",
       cmd  => fun(State) -> 
		      client_await_continue_signal(State, 5000) 
	      end},

     #{id   => 4,
       desc => "Connect",
       cmd  => fun(State) -> 
		      client_connect(Config, State, Opts) 
	      end},

     #{id   => 5,
       desc => "Await nothing",
       cmd  => fun(State) -> 
		      client_await_nothing(State, 5000) 
	      end},

     #{id   => 6,
       desc => "Disconnect",
       cmd  => fun(State) -> 
		      client_disconnect(State) 
	      end},

     #{id   => 7,
       desc => "Stop transport",
       cmd  => fun(State) -> 
		      client_stop_transport(State) 
	      end}
    ].


%% ------------------ sending ------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sendreceive(suite) ->
    [];
sendreceive(Config) when is_list(Config) ->
    Pre = fun() ->
		  p("create nodes"),
		  ServerNode = make_node_name(t_sr_server),
		  ClientNode = make_node_name(t_sr_client),
		  Nodes = [ServerNode, ClientNode], 
		  ok = ?START_NODES(Nodes, true),
		  Nodes
	  end,
    Case = fun(X) -> do_sendreceive(Config, X) end,
    Post = fun(Nodes) ->
                   p("stop nodes"),
                   ?STOP_NODES(lists:reverse(Nodes))
           end,
    try_tc(sendreceive, Pre, Case, Post).

do_sendreceive(Config, [ServerNode, ClientNode]) ->
    p("retrieve server info (host and port)"),
    ServerPort       = ?SERVER_PORT(Config, 2), % 2944,
    {ok, ServerHost} = inet:gethostname(),

    %% Create command sequences
    p("create server command sequence: "
      "~n      Server Port: ~p"
      "~n      Server Host: ~p", [ServerPort, ServerHost]),
    ServerCmds = sendreceive_server_commands(Config, ServerPort, ServerHost),
    p("create client command sequence: "
      "~n      Server Port: ~p"
      "~n      Server Host: ~p", [ServerPort, ServerHost]),
    ClientCmds = sendreceive_client_commands(Config, ServerPort, ServerHost),

    %% Start the test procs used in the test-case, one for each node
    p("start command handler(s)"),
    Server = server_start_command_handler(ServerNode, ServerCmds),
    p("server command handler started: ~p", [Server]),
    Client = client_start_command_handler(ClientNode, ClientCmds),
    p("client command handler started: ~p", [Client]),

    await_server_listening(Server, Client),
    
    ok = await_command_handler_completion([Server, Client], ?SECS(20)),
    p("done"),
    ok.


sendreceive_server_commands(Config, Port, Host) ->
    Opts = [{host, Host}, {port, Port}], 
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
       desc => "Listen",
       cmd  => fun(State) -> 
		       server_listen(Config, State, Opts) 
	       end},

     #{id   => 4,
       desc => "Notify listening",
       cmd  => fun(State) -> 
		       server_notify_listening(State) 
	       end},

     #{id   => 5,
       desc => "Await initial message (ping)",
       cmd  => fun(State) -> 
		       server_await_initial_message(State, "ping", 5000) 
	       end},

     #{id   => 6,
       desc => "Send reply (pong) to initial message",
       cmd  => fun(State) -> 
		       server_send_message(State, "pong") 
	       end},

     #{id   => 7,
       desc => "Await nothing before sending a message (hejsan)",
       cmd  => fun(State) -> 
		       server_await_nothing(State, 1000) 
	       end},

     #{id   => 8,
       desc => "Send message (hejsan)",
       cmd  => fun(State) -> 
		       server_send_message(State, "hejsan") 
	       end},

     #{id   => 9,
       desc => "Await reply (hoppsan) to message",
       cmd  => fun(State) -> 
		       server_await_message(State, "hoppsan", 1000) 
	       end},

     #{id   => 10,
       desc => "Await nothing before disconnecting",
       cmd  => fun(State) -> 
		       server_await_nothing(State, 1000) 
	       end},

     #{id   => 11,
       desc => "Disconnect",
       cmd  => fun(State) -> 
		       server_disconnect(State) 
	       end},

     #{id   => 12,
       desc => "Await nothing before stopping transport",
       cmd  => fun(State) -> 
		       server_await_nothing(State, 1000) 
	       end},

     #{id   => 13,
       desc => "Stop",
       cmd  => fun(State) -> 
		       server_stop_transport(State) 
	       end}

    ].

sendreceive_client_commands(Config, ServerPort, ServerHost) ->
    Opts = [{port, ServerPort}, {host, ServerHost}], 
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
       desc => "Await continue",
       cmd  => fun(State) -> 
		       client_await_continue_signal(State, 5000) 
	       end},

     #{id   => 4,
       desc => "Connect",
       cmd  => fun(State) -> 
		       client_connect(Config, State, Opts) 
	       end},

     #{id   => 5,
       desc => "Send initial message (ping)",
       cmd  => fun(State) -> 
		       client_send_message(State, "ping") 
	       end},

     #{id   => 6,
       desc => "Await reply (pong) to initial message",
       cmd  => fun(State) -> 
		       client_await_message(State, "pong", 1000) 
	       end},

     #{id   => 7,
       desc => "Await message (hejsan)",
       cmd  => fun(State) -> 
		       client_await_message(State, "hejsan", 5000) 
	       end},

     #{id   => 8,
       desc => "Send reply (hoppsan) to message",
       cmd  => fun(State) -> 
		       client_send_message(State, "hoppsan") 
	       end},

     #{id   => 9,
       desc => "Await nothing before disconnecting",
       cmd  => fun(State) -> 
		       client_await_nothing(State, 1000) 
	       end},

     #{id   => 10,
       desc => "Disconnect",
       cmd  => fun(State) -> 
		       client_disconnect(State) 
	       end},

     #{id   => 11,
       desc => "Await nothing before stopping transport",
       cmd  => fun(State) -> 
		       client_await_nothing(State, 1000) 
	       end},

     #{id   => 12,
       desc => "Stop transport",
       cmd  => fun(State) -> 
		       client_stop_transport(State) 
	       end}
    ].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

block_unblock(suite) ->
    [];
block_unblock(Config) when is_list(Config) ->
    Pre = fun() ->
		  p("create nodes"),
		  ServerNode = make_node_name(t_bb_server),
		  ClientNode = make_node_name(t_bb_client),
		  Nodes = [ServerNode, ClientNode], 
		  ok = ?START_NODES(Nodes, true),
		  Nodes
	  end,
    Case = fun(X) -> do_block_unblock(Config, X) end,
    Post = fun(Nodes) ->
                   p("stop nodes"),
                   ?STOP_NODES(lists:reverse(Nodes))
           end,
    try_tc(block_unblock, Pre, Case, Post).

do_block_unblock(Config, [ServerNode, ClientNode]) ->
    %% Create command sequences
    p("create command sequences"),
    ServerPort = ?SERVER_PORT(Config, 3), % 2944, 
    ServerCmds = block_unblock_server_commands(Config, ServerPort),
    {ok, ServerHost} = inet:gethostname(),
    ClientCmds = block_unblock_client_commands(Config, ServerPort, ServerHost),

    %% Start the test procs used in the test-case, one for each node
    p("start command handlers"),
    Server = server_start_command_handler(ServerNode, ServerCmds),
    p("server command handler started: ~p", [Server]),
    Client = client_start_command_handler(ClientNode, ClientCmds),
    p("client command handler started: ~p", [Client]),

    await_server_listening(Server, Client),

    await_client_blocked(Server, Client),

    ok = await_command_handler_completion([Server, Client], ?SECS(30)),
    p("done"),
    ok.


block_unblock_server_commands(Config, Port) ->
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
       desc => "Listen",
       cmd  => fun(State) -> 
		       server_listen(Config, State, Opts) 
	       end},

     #{id   => 4,
       desc => "Notify listening",
       cmd  => fun(State) -> 
		       server_notify_listening(State) 
	       end},

     #{id   => 5,
       desc => "Await initial message (ping)",
       cmd  => fun(State) -> 
		       server_await_initial_message(State, "ping", 5000) 
	       end},

     #{id   => 6,
       desc => "Send reply (pong) to initial message",
       cmd  => fun(State) -> 
		       server_send_message(State, "pong") 
	       end},

     #{id   => 7,
       desc => "Await continue",
       cmd  => fun(State) -> 
		       server_await_continue_signal(State, 5000) 
	       end},

     %% #{id   => 9,
     %%   desc => "Await nothing before sending a message (hejsan)",
     %%   cmd  => fun(State) -> 
     %%    	       server_await_nothing(State, 1000) 
     %%           end},

     #{id   => 10,
       desc => "Send message (hejsan)",
       cmd  => fun(State) -> 
		       server_send_message(State, "hejsan") 
	       end},

     #{id   => 11,
       desc => "Await reply (hoppsan) to message",
       cmd  => fun(State) -> 
		       server_await_message(State, "hoppsan", 10000) 
	       end},

     #{id   => 12,
       desc => "Await nothing before disconnecting",
       cmd  => fun(State) -> 
		       server_await_nothing(State, 1000) 
	       end},

     #{id   => 13,
       desc => "Disconnect",
       cmd  => fun(State) -> 
		       server_disconnect(State) 
	       end},

     #{id   => 14,
       desc => "Await nothing before stopping transport",
       cmd  => fun(State) -> 
		       server_await_nothing(State, 1000) 
	       end},

     #{id   => 15,
       desc => "Stop",
       cmd  => fun(State) -> 
		       server_stop_transport(State) 
	       end}

    ].

block_unblock_client_commands(Config, ServerPort, ServerHost) ->
    Opts = [{port, ServerPort}, {host, ServerHost}], 
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
       desc => "Await continue",
       cmd  => fun(State) -> 
		       client_await_continue_signal(State, 5000) 
	       end},

     #{id   => 4,
       desc => "Connect",
       cmd  => fun(State) -> 
		       client_connect(Config, State, Opts) 
	       end},

     #{id   => 5,
       desc => "Send initial message (ping)",
       cmd  => fun(State) -> 
		       client_send_message(State, "ping") 
	       end},

     #{id   => 6,
       desc => "Await reply (pong) to initial message",
       cmd  => fun(State) -> 
		       client_await_message(State, "pong", 1000) 
	       end},

     #{id   => 7,
       desc => "Await nothing before blocking",
       cmd  => fun(State) -> 
		       client_await_nothing(State, 1000) 
	       end},

     #{id   => 8,
       desc => "Block",
       cmd  => fun(State) -> 
		       client_block(State) 
	       end},

     #{id   => 9,
       desc => "Notify blocked",
       cmd  => fun(State) -> 
		       client_notify_blocked(State) 
	       end},

     #{id   => 10,
       desc => "Await nothing before unblocking",
       cmd  => fun(State) -> 
		       client_await_nothing(State, 5000) 
	       end},

     #{id   => 11,
       desc => "Unblock",
       cmd  => fun(State) -> 
		       client_unblock(State) 
	       end},

     #{id   => 12,
       desc => "Await message (hejsan)",
       cmd  => fun(State) -> 
		       client_await_message(State, "hejsan", 500) 
	       end},

     #{id   => 13,
       desc => "Send reply (hoppsan) to message",
       cmd  => fun(State) -> 
		       client_send_message(State, "hoppsan") 
	       end},

     #{id   => 14,
       desc => "Await nothing before disconnecting",
       cmd  => fun(State) -> 
		       client_await_nothing(State, 1000) 
	       end},

     #{id   => 15,
       desc => "Disconnect",
       cmd  => fun(State) -> 
		       client_disconnect(State) 
	       end},

     #{id   => 16,
       desc => "Await nothing before stopping transport",
       cmd  => fun(State) -> 
		       client_await_nothing(State, 1000) 
	       end},

     #{id   => 17,
       desc => "Stop transport",
       cmd  => fun(State) -> 
		       client_stop_transport(State) 
	       end}
    ].



%% ------------------ errors ------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

socket_failure(suite) ->
    [];
socket_failure(Config) when is_list(Config) ->
    put(sname, "socket_failure"),
    p("BEGIN TEST-CASE"), 

    %% process_flag(trap_exit, true),

    socket_faulure().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accept_process(suite) ->
    [];
accept_process(Config) when is_list(Config) ->
    put(sname, "accept_process"),
    p("BEGIN TEST-CASE"), 

    %% process_flag(trap_exit, true),

    failing_accept_process().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accept_supervisor(suite) ->
    [];
accept_supervisor(Config) when is_list(Config) ->
    put(sname, "accept_supervisor"),
    p("BEGIN TEST-CASE"), 

    %% process_flag(trap_exit, true),

    failing_accept_supervisor().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connection_supervisor(suite) ->
    [];
connection_supervisor(Config) when is_list(Config) ->
    put(sname, "connection_supervisor"),
    p("BEGIN TEST-CASE"), 

    %% process_flag(trap_exit, true),

    failing_connection_supervisor().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tcp_server(suite) ->
    [];
tcp_server(Config) when is_list(Config) ->
    put(sname, "tcp_server"),
    p("BEGIN TEST-CASE"), 

    %% process_flag(trap_exit, true),

    failing_tcp_server().


%%======================================================================
%% Test functions
%%======================================================================

start_case(Config, Options, Expect) ->
    p("start transport"),
    case (catch megaco_tcp:start_transport()) of
	{ok, Pid} ->
	    p("create listen socket"),
	    case (catch ?LISTEN(Config, Pid, Options)) of
		ok when Expect =:= ok ->
		    p("extected listen result [ok]"),
		    {ok, Pid};
		ok ->
		    p("unexpected listen result [ok] - stop transport"),
		    megaco_tcp:stop_transport(Pid),
		    ?ERROR(unexpected_start_sucesss);
		{error, Reason} when Expect =:= error ->
		    p("expected listen result [error] - stop transport: "
                      "~n      Reason: ~p", [Reason]),
		    megaco_tcp:stop_transport(Pid),
		    ok;
		{error, Reason} ->
		    p("unexpected listen result [error] - stop transport: "
                      "~n      Reason: ~p", [Reason]),
		    megaco_tcp:stop_transport(Pid),
		    ?ERROR({unexpected_start_failure, Reason});
		Error ->
		    p("unexpected listen result: "
                      "~n      Error: ~p", [Error]),
		    megaco_tcp:stop_transport(Pid),
		    ?ERROR({unexpected_result, Error})
	    end;
	{error, Reason} ->
	    p("unexpected start_transport result"),
	    ?ERROR({failed_starting_transport, Reason})
    end.

socket_faulure() ->
    ?SKIP(not_yet_implemented).

failing_accept_process() ->
    ?SKIP(not_yet_implemented).

failing_accept_supervisor() ->
    ?SKIP(not_yet_implemented).

failing_connection_supervisor() ->
    ?SKIP(not_yet_implemented).

failing_tcp_server() ->
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

await_server_listening(Server, Client) ->
    receive
        {listening, Server} ->
            p("[await-server-listening] "
              "received listening message from server [~p] => "
              "send continue to client [~p]"
              "~n", [Server, Client]),
            Client ! {continue, self()},
            ok;
	{'EXIT', Server, SReason} ->
            p("[await-server-listening] unexpected server (~p) exit: "
              "~n       ~p"
              "~n", [Server, SReason]),
	    exit({server_crash, SReason});
	{'EXIT', Client, CReason} ->
            p("[await-server-listening] "
              "unexpected client (~p) exit: "
              "~n       ~p"
              "~n", [Client, CReason]),
	    exit({client_crash, CReason})
    after 5000 ->
            %% There is no normal reason why this should take any time.
            %% Normally, this takes a few milli seconds. So, if we are not
            %% up and running after 5 seconds, we give up and skip!!
            exit(Server, kill),
            exit(Client, kill),
            ?SKIP("Server timeout (listen)")
    end.


await_client_blocked(Server, Client) ->
    receive
        {blocked, Client} ->
            p("[await-client-blocked] "
              "received blocked message from client [~p] => "
              "send continue to server [~p]~n", [Client, Server]),
            Server ! {continue, self()},
            ok;
	{'EXIT', Client, CReason} ->
            p("[await-client-blocked] "
              "unexpected client (~p) exit: "
              "~n       ~p"
              "~n", [Client, CReason]),
	    exit({client_crash, CReason});
	{'EXIT', Server, SReason} ->
            p("[await-client-blocked] "
              "unexpected server (~p) exit: "
              "~n       ~p"
              "~n", [Server, SReason]),
	    exit({server_crash, SReason})
    after 5000 ->
            %% There is no normal reason why this should take any time.
            %% Normally, this takes a few milli seconds. So, if we are not
            %% up and running after 5 seconds, we give up and skip!!
            exit(Client, kill),
            exit(Server, kill),
            ?SKIP("Client timeout (blocked)")
    end.

    

%% -------  Server command handler and utility functions ----------

server_start_command_handler(Node, Commands) ->
    start_command_handler(Node, Commands, #{}, "server").

server_start_transport(State) when is_map(State) ->
    case (catch megaco_tcp:start_transport()) of
	{ok, Ref} ->
	    {ok, State#{transport_ref => Ref}};
	Error ->
	    Error
    end.

server_listen(Config, #{transport_ref := Ref} = State, Options) 
  when is_list(Options) ->
    ?SLEEP(1000),
    Opts = [{receive_handle, self()}, {module, ?MODULE} | Options], 
    case (catch ?LISTEN(Config, Ref, Opts)) of
	ok ->
	    {ok, State};
	Error ->
	    Error
    end.

server_notify_listening(#{parent := Parent} = State) ->
    Parent ! {listening, self()},
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
	    NewState = State#{control_pid => ControlPid,
			      handle      => Handle},
	    {ok, NewState};

	Any ->
	    p("received unexpected event: ~p", [Any]),
	    {error, {unexpected_event, Any}}

    after Timeout ->
	    {error, timeout}
    end.

server_send_message(#{handle := Handle} = State, Message) ->
    megaco_tcp:send_message(Handle, Message),
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
	    {ok, State};

	Any ->
	    p("received unexpected event: ~p", [Any]),
	    {error, {unexpected_event, Any}}

    after Timeout ->
	    {error, timeout}
    end.

server_disconnect(#{handle := Handle} = State) 
  when (Handle =/= undefined) ->
     megaco_tcp:close(Handle),
    {ok, State#{handle => undefined}}.

server_stop_transport(#{transport_ref := Ref} = State) 
  when (Ref =/= undefined) ->
    megaco_tcp:stop_transport(Ref),
    {ok, State}.


%% -------  Client command handler and utility functions ----------

client_start_command_handler(Node, Commands) ->
    start_command_handler(Node, Commands, #{}, "client").
		  
client_start_transport(State) when is_map(State) ->
    case (catch megaco_tcp:start_transport()) of
	{ok, Ref} ->
	    {ok, State#{transport_ref => Ref}};
	Error ->
	    Error
    end.

client_connect(Config, #{transport_ref := Ref} = State, Options) 
  when is_list(Options) ->
    Opts = [{receive_handle, self()}, {module, ?MODULE} | Options], 
    case (catch ?CONNECT(Config, Ref, Opts)) of
	{ok, Handle, ControlPid} ->
	    {ok, State#{control_pid => ControlPid, 
			handle      => Handle}};
	Error ->
	    Error
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

client_await_nothing(State, Timeout) 
  when is_map(State) ->
    receive 
	Any ->
	    p("received unexpected event: ~p", [Any]),
	    {error, {unexpected_event, Any}}
    after Timeout ->
	    {ok, State}
    end.

client_send_message(#{handle := Handle} = State, Message) ->
    megaco_tcp:send_message(Handle, Message),
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
    megaco_tcp:block(Handle),
    {ok, State}.

client_unblock(#{handle := Handle} = State) 
  when (Handle =/= undefined) ->
    megaco_tcp:unblock(Handle),
    {ok, State}.

client_disconnect(#{handle := Handle} = State) 
  when (Handle =/= undefined) ->
    megaco_tcp:close(Handle),
    {ok, State#{handle => undefined, control_pid => undefined}}.

client_stop_transport(#{transport_ref := Ref} = State) 
  when (Ref =/= undefined) ->
    megaco_tcp:stop_transport(Ref),
    {ok, State}.

    
%% -------- Command handler ---------

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


