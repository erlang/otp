%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2019. All Rights Reserved.
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
-module(megaco_tcp_test).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("megaco/src/tcp/megaco_tcp.hrl").
-include("megaco_test_lib.hrl").

%% -compile(export_all).


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
	 all/0,groups/0,init_per_group/2,end_per_group/2,
	 start_normal/1,
	 start_invalid_opt/1,
	 start_and_stop/1,
	 sendreceive/1, 
	 block_unblock/1, 
	 socket_failure/1,
	 accept_process/1,
	 accept_supervisor/1,
	 connection_supervisor/1,
	 tcp_server/1, 
	 
	 init_per_testcase/2, end_per_testcase/2, 

	 t/0, t/1
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

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------

-record(command, {id, desc, cmd}).
-record(server,  {parent, transport_ref, control_pid, handle}).
-record(client,  {parent, transport_ref, control_pid, handle}).


%%======================================================================
%% External functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function: t/0
%% Description: Run all test cases
%%----------------------------------------------------------------------
t() -> megaco_test_lib:t(?MODULE).


%%----------------------------------------------------------------------
%% Function: t/1
%% Description: Run the specified test cases 
%%----------------------------------------------------------------------
t(Case) -> megaco_test_lib:t({?MODULE, Case}).
    

%%======================================================================
%% Test server callbacks
%%======================================================================
%%----------------------------------------------------------------------
%% Function: init_per_testcase/2
%% Description: 
%%----------------------------------------------------------------------
init_per_testcase(Case, Config) ->
    megaco_test_lib:init_per_testcase(Case, Config).


%%----------------------------------------------------------------------
%% Function: end_per_testcase/2
%% Description: 
%%----------------------------------------------------------------------
end_per_testcase(Case, Config) ->
    megaco_test_lib:end_per_testcase(Case, Config).


%%======================================================================
%% Test case definitions
%%======================================================================
all() -> 
    [
     {group, start},
     {group, sending},
     {group, errors}
    ].

groups() -> 
    [{start,   [], start_cases()},
     {sending, [], sending_cases()},
     {errors,  [], errors_cases()}].

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

errors_cases() ->
    [
     socket_failure,
     accept_process,
     accept_supervisor,
     connection_supervisor,
     tcp_server
    ].


init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% ------------------ start ------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_normal(suite) ->
    [];
start_normal(Config) when is_list(Config) ->
    put(sname, "start_normal"),
    p("BEGIN TEST-CASE"), 
    Options = [{port, 20000}, {receive_handle, apa}],
    {ok, Pid} = start_case(Options, ok),
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
    ok = start_case(Options, error),
    p("done"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_and_stop(suite) ->
    [];
start_and_stop(doc) ->
    ["This test case sets up a connection and then cloises it. "
     "No data is sent. "];
start_and_stop(Config) when is_list(Config) ->
    put(sname, "start_and_stop"),
    p("BEGIN TEST-CASE"), 

    process_flag(trap_exit, true),

    p("create nodes"),
    ServerNode = make_node_name(server),
    ClientNode = make_node_name(client),
    Nodes = [ServerNode, ClientNode], 
    ok = megaco_test_lib:start_nodes(Nodes, ?FILE, ?LINE),

    %% Create command sequences
    p("create command sequences"),
    ServerPort = 2944, 
    ServerCmds = start_and_stop_server_commands(ServerPort),
    {ok, ServerHost} = inet:gethostname(),
    ClientCmds = start_and_stop_client_commands(ServerPort, ServerHost),

    %% Start the test procs used in the test-case, one for each node
    p("start command handlers"),
    Server = server_start_command_handler(ServerNode, ServerCmds),
    p("server command handler started: ~p", [Server]),
    Client = client_start_command_handler(ClientNode, ClientCmds),
    p("client command handler started: ~p", [Client]),

    await_server_listening(Server, Client),

    await_command_handler_completion([Server, Client], timer:seconds(20)),
    p("done"),
    ok.


start_and_stop_server_commands(Port) ->
    Opts = [{port, Port}], 
    Self = self(),
    [
     #command{id   = 1,
	      desc = "Command sequence init",
	      cmd  = fun(State) -> 
			     {ok, State#server{parent = Self}} 
		     end},

     #command{id   = 2,
	      desc = "Start transport",
	      cmd  = fun(State) -> 
			     server_start_transport(State) 
		     end},

     #command{id   = 3,
	      desc = "Listen",
	      cmd  = fun(State) -> 
			     server_listen(State, Opts) 
		     end},

     #command{id   = 4,
	      desc = "Notify listening",
	      cmd  = fun(State) -> 
			     server_notify_listening(State) 
		     end},

     #command{id   = 5,
	      desc = "Await nothing",
	      cmd  = fun(State) -> 
			     server_await_nothing(State, 6000) 
		     end},

     #command{id   = 6,
	      desc = "Stop",
	      cmd  = fun(State) -> 
			     server_stop_transport(State) 
		     end}

    ].


start_and_stop_client_commands(ServerPort, ServerHost) ->
    Opts = [{port, ServerPort}, {host, ServerHost}], 
    Self = self(),
    [
     #command{id   = 1,
	      desc = "Command sequence init",
	      cmd  = fun(State) -> 
			     {ok, State#client{parent = Self}} 
		     end},

     #command{id   = 2,
	      desc = "Start transport",
	      cmd  = fun(State) -> 
			     client_start_transport(State) 
		     end},

     #command{id   = 3,
	      desc = "Await continue",
	      cmd  = fun(State) -> 
			     client_await_continue_signal(State, 5000) 
		     end},

     #command{id   = 4,
	      desc = "Connect",
	      cmd  = fun(State) -> 
			     client_connect(State, Opts) 
		     end},

     #command{id   = 5,
	      desc = "Await nothing",
	      cmd  = fun(State) -> 
			     client_await_nothing(State, 5000) 
		     end},

     #command{id   = 6,
	      desc = "Disconnect",
	      cmd  = fun(State) -> 
			     client_disconnect(State) 
		     end},

     #command{id   = 7,
	      desc = "Stop transport",
	      cmd  = fun(State) -> 
			     client_stop_transport(State) 
		     end}
    ].


%% ------------------ sending ------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sendreceive(suite) ->
    [];
sendreceive(Config) when is_list(Config) ->
    put(sname, "sendreceive"),
    p("BEGIN TEST-CASE"), 

    process_flag(trap_exit, true),

    p("create nodes"),
    ServerNode = make_node_name(server),
    ClientNode = make_node_name(client),
    Nodes = [ServerNode, ClientNode], 
    ok = megaco_test_lib:start_nodes(Nodes, ?FILE, ?LINE),

    %% Create command sequences
    p("create command sequences"),
    ServerPort = 2944, 
    ServerCmds = sendreceive_server_commands(ServerPort),
    {ok, ServerHost} = inet:gethostname(),
    ClientCmds = sendreceive_client_commands(ServerPort, ServerHost),

    %% Start the test procs used in the test-case, one for each node
    p("start command handlers"),
    Server = server_start_command_handler(ServerNode, ServerCmds),
    p("server command handler started: ~p", [Server]),
    Client = client_start_command_handler(ClientNode, ClientCmds),
    p("client command handler started: ~p", [Client]),

    await_server_listening(Server, Client),
    
    await_command_handler_completion([Server, Client], timer:seconds(20)),
    p("done"),
    ok.


sendreceive_server_commands(Port) ->
    Opts = [{port, Port}], 
    Self = self(),
    [
     #command{id   = 1,
	      desc = "Command sequence init",
	      cmd  = fun(State) -> 
			     {ok, State#server{parent = Self}} 
		     end},

     #command{id   = 2,
	      desc = "Start transport",
	      cmd  = fun(State) -> 
			     server_start_transport(State) 
		     end},

     #command{id   = 3,
	      desc = "Listen",
	      cmd  = fun(State) -> 
			     server_listen(State, Opts) 
		     end},

     #command{id   = 4,
	      desc = "Notify listening",
	      cmd  = fun(State) -> 
			     server_notify_listening(State) 
		     end},

     #command{id   = 5,
	      desc = "Await initial message (ping)",
	      cmd  = fun(State) -> 
			     server_await_initial_message(State, "ping", 5000) 
		     end},

     #command{id   = 6,
	      desc = "Send reply (pong) to initial message",
	      cmd  = fun(State) -> 
			     server_send_message(State, "pong") 
		     end},

     #command{id   = 7,
	      desc = "Await nothing before sending a message (hejsan)",
	      cmd  = fun(State) -> 
			     server_await_nothing(State, 1000) 
		     end},

     #command{id   = 8,
	      desc = "Send message (hejsan)",
	      cmd  = fun(State) -> 
			     server_send_message(State, "hejsan") 
		     end},

     #command{id   = 9,
	      desc = "Await reply (hoppsan) to message",
	      cmd  = fun(State) -> 
			     server_await_message(State, "hoppsan", 1000) 
		     end},

     #command{id   = 10,
	      desc = "Await nothing before disconnecting",
	      cmd  = fun(State) -> 
			     server_await_nothing(State, 1000) 
		     end},

     #command{id   = 11,
	      desc = "Disconnect",
	      cmd  = fun(State) -> 
			     server_disconnect(State) 
		     end},

     #command{id   = 12,
	      desc = "Await nothing before stopping transport",
	      cmd  = fun(State) -> 
			     server_await_nothing(State, 1000) 
		     end},

     #command{id   = 13,
	      desc = "Stop",
	      cmd  = fun(State) -> 
			     server_stop_transport(State) 
		     end}

    ].

sendreceive_client_commands(ServerPort, ServerHost) ->
    Opts = [{port, ServerPort}, {host, ServerHost}], 
    Self = self(),
    [
     #command{id   = 1,
	      desc = "Command sequence init",
	      cmd  = fun(State) -> 
			     {ok, State#client{parent = Self}} 
		     end},

     #command{id   = 2,
	      desc = "Start transport",
	      cmd  = fun(State) -> 
			     client_start_transport(State) 
		     end},

     #command{id   = 3,
	      desc = "Await continue",
	      cmd  = fun(State) -> 
			     client_await_continue_signal(State, 5000) 
		     end},

     #command{id   = 4,
	      desc = "Connect",
	      cmd  = fun(State) -> 
			     client_connect(State, Opts) 
		     end},

     #command{id   = 5,
	      desc = "Send initial message (ping)",
	      cmd  = fun(State) -> 
			     client_send_message(State, "ping") 
		     end},

     #command{id   = 6,
	      desc = "Await reply (pong) to initial message",
	      cmd  = fun(State) -> 
			     client_await_message(State, "pong", 1000) 
		     end},

     #command{id   = 7,
	      desc = "Await message (hejsan)",
	      cmd  = fun(State) -> 
			     client_await_message(State, "hejsan", 5000) 
		     end},

     #command{id   = 8,
	      desc = "Send reply (hoppsan) to message",
	      cmd  = fun(State) -> 
			     client_send_message(State, "hoppsan") 
		     end},

     #command{id   = 9,
	      desc = "Await nothing before disconnecting",
	      cmd  = fun(State) -> 
			     client_await_nothing(State, 1000) 
		     end},

     #command{id   = 10,
	      desc = "Disconnect",
	      cmd  = fun(State) -> 
			     client_disconnect(State) 
		     end},

     #command{id   = 11,
	      desc = "Await nothing before stopping transport",
	      cmd  = fun(State) -> 
			     client_await_nothing(State, 1000) 
		     end},

     #command{id   = 12,
	      desc = "Stop transport",
	      cmd  = fun(State) -> 
			     client_stop_transport(State) 
		     end}
    ].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

block_unblock(suite) ->
    [];
block_unblock(Config) when is_list(Config) ->
    put(sname, "block_unblock"),
    p("BEGIN TEST-CASE"), 

    process_flag(trap_exit, true),

    p("create nodes"),
    ServerNode = make_node_name(server),
    ClientNode = make_node_name(client),
    Nodes = [ServerNode, ClientNode], 
    ok = megaco_test_lib:start_nodes(Nodes, ?FILE, ?LINE),

    %% Create command sequences
    p("create command sequences"),
    ServerPort = 2944, 
    ServerCmds = block_unblock_server_commands(ServerPort),
    {ok, ServerHost} = inet:gethostname(),
    ClientCmds = block_unblock_client_commands(ServerPort, ServerHost),

    %% Start the test procs used in the test-case, one for each node
    p("start command handlers"),
    Server = server_start_command_handler(ServerNode, ServerCmds),
    p("server command handler started: ~p", [Server]),
    Client = client_start_command_handler(ClientNode, ClientCmds),
    p("client command handler started: ~p", [Client]),

    await_server_listening(Server, Client),

    await_client_blocked(Server, Client),

    await_command_handler_completion([Server, Client], timer:seconds(30)),
    p("done"),
    ok.


block_unblock_server_commands(Port) ->
    Opts = [{port, Port}], 
    Self = self(),
    [
     #command{id   = 1,
	      desc = "Command sequence init",
	      cmd  = fun(State) -> 
			     {ok, State#server{parent = Self}} 
		     end},

     #command{id   = 2,
	      desc = "Start transport",
	      cmd  = fun(State) -> 
			     server_start_transport(State) 
		     end},

     #command{id   = 3,
	      desc = "Listen",
	      cmd  = fun(State) -> 
			     server_listen(State, Opts) 
		     end},

     #command{id   = 4,
	      desc = "Notify listening",
	      cmd  = fun(State) -> 
			     server_notify_listening(State) 
		     end},

     #command{id   = 5,
	      desc = "Await initial message (ping)",
	      cmd  = fun(State) -> 
			     server_await_initial_message(State, "ping", 5000) 
		     end},

     #command{id   = 6,
	      desc = "Send reply (pong) to initial message",
	      cmd  = fun(State) -> 
			     server_send_message(State, "pong") 
		     end},

     #command{id   = 7,
	      desc = "Await continue",
	      cmd  = fun(State) -> 
			     server_await_continue_signal(State, 5000) 
		     end},

     #command{id   = 9,
	      desc = "Await nothing before sending a message (hejsan)",
	      cmd  = fun(State) -> 
			     server_await_nothing(State, 1000) 
		     end},

     #command{id   = 10,
	      desc = "Send message (hejsan)",
	      cmd  = fun(State) -> 
			     server_send_message(State, "hejsan") 
		     end},

     #command{id   = 11,
	      desc = "Await reply (hoppsan) to message",
	      cmd  = fun(State) -> 
			     server_await_message(State, "hoppsan", 10000) 
		     end},

     #command{id   = 12,
	      desc = "Await nothing before disconnecting",
	      cmd  = fun(State) -> 
			     server_await_nothing(State, 1000) 
		     end},

     #command{id   = 13,
	      desc = "Disconnect",
	      cmd  = fun(State) -> 
			     server_disconnect(State) 
		     end},

     #command{id   = 14,
	      desc = "Await nothing before stopping transport",
	      cmd  = fun(State) -> 
			     server_await_nothing(State, 1000) 
		     end},

     #command{id   = 15,
	      desc = "Stop",
	      cmd  = fun(State) -> 
			     server_stop_transport(State) 
		     end}

    ].

block_unblock_client_commands(ServerPort, ServerHost) ->
    Opts = [{port, ServerPort}, {host, ServerHost}], 
    Self = self(),
    [
     #command{id   = 1,
	      desc = "Command sequence init",
	      cmd  = fun(State) -> 
			     {ok, State#client{parent = Self}} 
		     end},

     #command{id   = 2,
	      desc = "Start transport",
	      cmd  = fun(State) -> 
			     client_start_transport(State) 
		     end},

     #command{id   = 3,
	      desc = "Await continue",
	      cmd  = fun(State) -> 
			     client_await_continue_signal(State, 5000) 
		     end},

     #command{id   = 4,
	      desc = "Connect",
	      cmd  = fun(State) -> 
			     client_connect(State, Opts) 
		     end},

     #command{id   = 5,
	      desc = "Send initial message (ping)",
	      cmd  = fun(State) -> 
			     client_send_message(State, "ping") 
		     end},

     #command{id   = 6,
	      desc = "Await reply (pong) to initial message",
	      cmd  = fun(State) -> 
			     client_await_message(State, "pong", 1000) 
		     end},

     #command{id   = 7,
	      desc = "Await nothing before blocking",
	      cmd  = fun(State) -> 
			     client_await_nothing(State, 1000) 
		     end},

     #command{id   = 8,
	      desc = "Block",
	      cmd  = fun(State) -> 
			     client_block(State) 
		     end},

     #command{id   = 9,
	      desc = "Notify blocked",
	      cmd  = fun(State) -> 
			     client_notify_blocked(State) 
		     end},

     #command{id   = 10,
	      desc = "Await nothing before unblocking",
	      cmd  = fun(State) -> 
			     client_await_nothing(State, 5000) 
		     end},

     #command{id   = 11,
	      desc = "Unblock",
	      cmd  = fun(State) -> 
			     client_unblock(State) 
		     end},

     #command{id   = 12,
	      desc = "Await message (hejsan)",
	      cmd  = fun(State) -> 
			     client_await_message(State, "hejsan", 100) 
		     end},

     #command{id   = 13,
	      desc = "Send reply (hoppsan) to message",
	      cmd  = fun(State) -> 
			     client_send_message(State, "hoppsan") 
		     end},

     #command{id   = 14,
	      desc = "Await nothing before disconnecting",
	      cmd  = fun(State) -> 
			     client_await_nothing(State, 1000) 
		     end},

     #command{id   = 15,
	      desc = "Disconnect",
	      cmd  = fun(State) -> 
			     client_disconnect(State) 
		     end},

     #command{id   = 16,
	      desc = "Await nothing before stopping transport",
	      cmd  = fun(State) -> 
			     client_await_nothing(State, 1000) 
		     end},

     #command{id   = 17,
	      desc = "Stop transport",
	      cmd  = fun(State) -> 
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

start_case(Options, Expect) ->
    p("start transport"),
    case (catch megaco_tcp:start_transport()) of
	{ok, Pid} ->
	    p("create listen socket"),
	    case (catch megaco_tcp:listen(Pid, Options)) of
		ok when Expect =:= ok ->
		    p("extected listen result [ok]"),
		    {ok, Pid};
		ok ->
		    p("unextected listen result [ok] - stop transport"),
		    megaco_tcp:stop_transport(Pid),
		    ?ERROR(unexpected_start_sucesss);
		{error, _Reason} when Expect =:= error ->
		    p("extected listen result [error] - stop transport"),
		    megaco_tcp:stop_transport(Pid),
		    ok;
		{error, Reason} ->
		    p("unextected listen result [error] - stop transport"),
		    megaco_tcp:stop_transport(Pid),
		    ?ERROR({unexpected_start_failure, Reason});
		Error ->
		    p("unextected listen result"),
		    ?ERROR({unexpected_result, Error})
	    end;
	{error, Reason} ->
	    p("unextected start_transport result"),
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
            p("received listening message from server [~p] => "
              "send continue to client [~p]"
              "~n", [Server, Client]),
            Client ! {continue, self()},
            ok
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
            p("received blocked message from client [~p] => "
              "send continue to server [~p]~n", [Client, Server]),
            Server ! {continue, self()},
            ok
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
    start_command_handler(Node, Commands, #server{}, "server").

server_start_transport(State) when is_record(State, server) ->
    case (catch megaco_tcp:start_transport()) of
	{ok, Ref} ->
	    {ok, State#server{transport_ref = Ref}};
	Error ->
	    Error
    end.

server_listen(#server{transport_ref = Ref} = State, Options) 
  when is_record(State, server) andalso is_list(Options) ->
    Opts = [{receive_handle, self()}, {module, ?MODULE} | Options], 
    case (catch megaco_tcp:listen(Ref, Opts)) of
	ok ->
	    {ok, State};
	Error ->
	    Error
    end.

server_notify_listening(#server{parent = Parent} = State) 
  when is_record(State, server) ->
    Parent ! {listening, self()},
    {ok, State}.

server_await_continue_signal(#server{parent = Parent} = State, Timeout) ->
    receive
	{continue, Parent} ->
	    {ok, State}
    after Timeout ->
	    {error, timeout}
    end.
    
server_await_initial_message(State, InitialMessage, Timeout) 
  when is_record(State, server) ->
    receive 
	{receive_message, {ControlPid, Handle, InitialMessage}} ->
	    NewState = State#server{control_pid = ControlPid,
				    handle      = Handle},
	    {ok, NewState};

	Any ->
	    p("received unexpected event: ~p", [Any]),
	    {error, {unexpected_event, Any}}

    after Timeout ->
	    {error, timeout}
    end.

server_send_message(#server{handle = Handle} = State, Message) ->
    megaco_tcp:send_message(Handle, Message),
    {ok, State}.

server_await_nothing(State, Timeout) 
  when is_record(State, server) ->
    receive 
	Any ->
	    p("received unexpected event: ~p", [Any]),
	    {error, {unexpected_event, Any}}

    after Timeout ->
	    {ok, State}
    end.


server_await_message(State, ExpectMessage, Timeout) 
  when is_record(State, server) ->
    receive
	{receive_message, {_, _, ExpectMessage}} ->
	    {ok, State};

	Any ->
	    p("received unexpected event: ~p", [Any]),
	    {error, {unexpected_event, Any}}

    after Timeout ->
	    {error, timeout}
    end.

server_disconnect(#server{handle = Handle} = State) 
  when (Handle =/= undefined) ->
     megaco_tcp:close(Handle),
    {ok, State#server{handle = undefined}}.

%% server_block(#server{handle = Handle} = State) 
%%   when (Handle =/= undefined) ->
%%      megaco_tcp:block(Handle),
%%     {ok, State}.

%% server_unblock(#server{handle = Handle} = State) 
%%   when (Handle =/= undefined) ->
%%      megaco_tcp:unblock(Handle),
%%     {ok, State}.

server_stop_transport(#server{transport_ref = Ref} = State) 
  when (Ref =/= undefined) ->
    megaco_tcp:stop_transport(Ref),
    {ok, State}.


%% -------  Client command handler and utility functions ----------

client_start_command_handler(Node, Commands) ->
    start_command_handler(Node, Commands, #client{}, "client").
		  
client_start_transport(State) when is_record(State, client) ->
    case (catch megaco_tcp:start_transport()) of
	{ok, Ref} ->
	    {ok, State#client{transport_ref = Ref}};
	Error ->
	    Error
    end.

client_connect(#client{transport_ref = Ref} = State, Options) 
  when is_record(State, client) andalso is_list(Options) ->
    Opts = [{receive_handle, self()}, {module, ?MODULE} | Options], 
    case (catch megaco_tcp:connect(Ref, Opts)) of
	{ok, Handle, ControlPid} ->
	    {ok, State#client{control_pid = ControlPid, 
			      handle      = Handle}};
	Error ->
	    Error
    end.

client_await_continue_signal(#client{parent = Parent} = State, Timeout) ->
    receive
	{continue, Parent} ->
	    {ok, State}
    after Timeout ->
	    {error, timeout}
    end.
    
client_notify_blocked(#client{parent = Parent} = State) ->
    Parent ! {blocked, self()},
    {ok, State}.

client_await_nothing(State, Timeout) 
  when is_record(State, client) ->
    receive 
	Any ->
	    p("received unexpected event: ~p", [Any]),
	    {error, {unexpected_event, Any}}
    after Timeout ->
	    {ok, State}
    end.

client_send_message(#client{handle = Handle} = State, Message) ->
    megaco_tcp:send_message(Handle, Message),
    {ok, State}.

client_await_message(State, ExpectMessage, Timeout) 
  when is_record(State, client) ->
    receive
	{receive_message, {_, _, ExpectMessage}} ->
	    {ok, State};

	Any ->
	    p("received unexpected event: ~p", [Any]),
	    {error, {unexpected_event, Any}}

    after Timeout ->
	    {error, timeout}
    end.

client_block(#client{handle = Handle} = State) 
  when (Handle =/= undefined) ->
    megaco_tcp:block(Handle),
    {ok, State}.

client_unblock(#client{handle = Handle} = State) 
  when (Handle =/= undefined) ->
    megaco_tcp:unblock(Handle),
    {ok, State}.

client_disconnect(#client{handle = Handle} = State) 
  when (Handle =/= undefined) ->
    megaco_tcp:close(Handle),
    {ok, State#client{handle = undefined, control_pid = undefined}}.

client_stop_transport(#client{transport_ref = Ref} = State) 
  when (Ref =/= undefined) ->
    megaco_tcp:stop_transport(Ref),
    {ok, State}.

    
%% -------- Command handler ---------

start_command_handler(Node, Commands, State, ShortName) ->
    Fun = fun() ->
		  put(sname, ShortName), 
		  process_flag(trap_exit, true),
		  Result = (catch command_handler(Commands, State)),
		  p("command handler terminated with: "
		    "~n   Result: ~p", [Result]),
		  exit(Result)
	  end,
    erlang:spawn_link(Node, Fun).
		  
command_handler([], State) ->
    p("command_handler -> entry when done with"
      "~n   State: ~p", [State]),
    {ok, State};
command_handler([#command{id   = Id,
			  desc = Desc,
			  cmd  = Cmd}|Commands], State) ->
    p("command_handler -> entry with"
      "~n   Id:   ~p"
      "~n   Desc: ~p", [Id, Desc]),
    case (catch Cmd(State)) of
	{ok, NewState} ->
	    p("command_handler -> cmd ~w ok", [Id]),
	    command_handler(Commands, NewState);
	{error, Reason} ->
	    p("command_handler -> cmd ~w error: "
	      "~n   Reason: ~p", [Id, Reason]),
	    {error, {cmd_error, Reason}};
	{'EXIT', Reason} ->
	    p("command_handler -> cmv ~w exit: "
	      "~n   Reason: ~p", [Id, Reason]),
	    {error, {cmd_exit, Reason}};
	Error ->
	    p("command_handler -> cmd ~w failure: "
	      "~n   Error: ~p", [Id, Error]),
	    {error, {cmd_failure, Error}}
    end.


await_command_handler_completion(Pids, Timeout) ->
    await_command_handler_completion(Pids, [], [], Timeout).

await_command_handler_completion([], [], _Good, _Timeout) ->
    p("await_command_handler_completion -> entry when done"),
    ok;
await_command_handler_completion([], Bad, Good, _Timeout) ->
    p("await_command_handler_completion -> entry when done with bad result: "
      "~n   Bad:  ~p"
      "~n   Good: ~p", [Bad, Good]),
    ok;
await_command_handler_completion(Pids, Bad, Good, Timeout) ->
    p("await_command_handler_completion -> entry when waiting for"
      "~n   Pids:    ~p"
      "~n   Bad:     ~p"
      "~n   Good:    ~p"
      "~n   Timeout: ~p", [Pids, Bad, Good, Timeout]), 
    Begin = ms(), 
    receive 
	{'EXIT', Pid, {ok, FinalState}} ->
	    p("await_command_handler_completion -> "
	      "received ok EXIT signal from ~p", [Pid]), 
	    case lists:delete(Pid, Pids) of
		Pids ->
		    await_command_handler_completion(Pids, Bad, Good, 
						     Timeout - (ms() - Begin));
		Pids2 ->
		    p("await_command_handler_completion -> ~p done", [Pid]), 
		    await_command_handler_completion(Pids2, 
						     Bad, 
						     [{Pid, FinalState}|Good],
						     Timeout - (ms() - Begin))
	    end;

	{'EXIT', Pid, {error, Reason}} ->
	    p("await_command_handler_completion -> "
	      "received error EXIT signal from ~p", [Pid]), 
	    case lists:delete(Pid, Pids) of
		Pids ->
		    await_command_handler_completion(Pids, Bad, Good, 
						     Timeout - (ms() - Begin));
		Pids2 ->
		    p("await_command_handler_completion -> ~p done", [Pid]), 
		    await_command_handler_completion(Pids2, 
						     [{Pid, Reason}|Bad], 
						     Good, 
						     Timeout - (ms() - Begin))
	    end

    after Timeout ->
	    p("await_command_handler_completion -> timeout"), 
	    exit({timeout, Pids})
    end.



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
    io:format("*** [~s] ~p ~s *** "
	      "~n   " ++ F ++ "~n", 
	      [?FTS(), self(), "undefined" | A]).


ms() ->
    erlang:monotonic_time(milli_seconds).


