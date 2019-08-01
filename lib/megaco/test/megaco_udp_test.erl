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
-module(megaco_udp_test).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("megaco/src/udp/megaco_udp.hrl").
-include("megaco_test_lib.hrl").


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
-record(server,  {parent, transport_ref, control_pid, handle, remote}).
-record(client,  {parent, transport_ref, control_pid, handle, remote}).


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
    [{group, start}, {group, sending}, {group, errors}].

groups() -> 
    [{start, [],
      [start_normal, start_invalid_opt, start_and_stop]},
     {sending, [], [sendreceive, block_unblock]},
     {errors, [], [socket_failure]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


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

    ok =
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
        after 5000 ->
                {error, server_timeout}
        end,

    ok = await_command_handler_completion([Server, Client], timer:seconds(20)),
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
              desc = "Open",
              cmd  = fun(State) ->
                             server_open(State, Opts)
                     end},

     #command{id   = 4,
              desc = "Notify operational",
              cmd  = fun(State) ->
                             server_notify_operational(State)
                     end},

     #command{id   = 5,
              desc = "Await nothing",
              cmd  = fun(State) ->
                             server_await_nothing(State, 5000)
                     end},

     #command{id   = 6,
              desc = "Close",
              cmd  = fun(State) ->
                             server_close(State)
                     end},

     #command{id   = 7,
              desc = "Stop",
              cmd  = fun(State) ->
                             server_stop_transport(State)
                     end}

    ].

start_and_stop_client_commands(ServerPort, _ServerHost) ->
    Opts = [{port, ServerPort}],
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
              desc = "Open",
              cmd  = fun(State) ->
                             client_open(State, Opts)
                     end},

     #command{id   = 4,
              desc = "Await continue",
              cmd  = fun(State) ->
                             client_await_continue_signal(State, 5000)
                     end},

     #command{id   = 5,
              desc = "Await nothing",
              cmd  = fun(State) ->
                             client_await_nothing(State, 5000)
                     end},

     #command{id   = 6,
              desc = "Close",
              cmd  = fun(State) ->
                             client_close(State)
                     end},

     #command{id   = 7,
              desc = "Stop transport",
              cmd  = fun(State) ->
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
	after 5000 ->
                {error, server_timeout}
        end,

    ok = await_command_handler_completion([Server, Client], timer:seconds(20)),
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
              desc = "Open",
              cmd  = fun(State) ->
                             server_open(State, Opts)
                     end},

     #command{id   = 4,
              desc = "Notify operational",
              cmd  = fun(State) ->
                             server_notify_operational(State)
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
	      desc = "Await nothing before closing",
	      cmd  = fun(State) -> 
			     server_await_nothing(State, 1000) 
		     end},

     #command{id   = 11,
	      desc = "Close",
	      cmd  = fun(State) -> 
			     server_close(State) 
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
    OwnPort = ServerPort+1, 
    Opts = [{port, OwnPort}], 
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
              desc = "Open",
              cmd  = fun(State) ->
                             client_open(State, Opts)
                     end},

     #command{id   = 4,
              desc = "Await continue",
              cmd  = fun(State) ->
                             client_await_continue_signal(State, 5000)
                     end},

     #command{id   = 5,
              desc = "Connect",
              cmd  = fun(State) ->
                             client_connect(State, ServerHost, ServerPort)
                     end},

     #command{id   = 6,
	      desc = "Send initial message (ping)",
	      cmd  = fun(State) -> 
			     client_send_message(State, "ping") 
		     end},

     #command{id   = 7,
	      desc = "Await reply (pong) to initial message",
	      cmd  = fun(State) -> 
			     client_await_message(State, "pong", 1000) 
		     end},

     #command{id   = 8,
	      desc = "Await message (hejsan)",
	      cmd  = fun(State) -> 
			     client_await_message(State, "hejsan", 5000) 
		     end},

     #command{id   = 9,
	      desc = "Send reply (hoppsan) to message",
	      cmd  = fun(State) -> 
			     client_send_message(State, "hoppsan") 
		     end},

     #command{id   = 10,
	      desc = "Await nothing before closing",
	      cmd  = fun(State) -> 
			     client_await_nothing(State, 1000) 
		     end},

     #command{id   = 11,
	      desc = "Close",
	      cmd  = fun(State) -> 
			     client_close(State) 
		     end},

     #command{id   = 12,
	      desc = "Await nothing before stopping transport",
	      cmd  = fun(State) -> 
			     client_await_nothing(State, 1000) 
		     end},

     #command{id   = 13,
	      desc = "Stop transport",
	      cmd  = fun(State) -> 
			     client_stop_transport(State) 
		     end}
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

block_unblock(suite) ->
    [];
block_unblock(doc) ->
    ["Test the block/unblock functions of the UDP transport. "];
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
        after 5000 ->
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
	after 5000 ->
		{error, timeout}
	end,

    ok = await_command_handler_completion([Server, Client], timer:seconds(20)),
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
              desc = "Open",
              cmd  = fun(State) ->
                             server_open(State, Opts)
                     end},

     #command{id   = 4,
              desc = "Notify operational",
              cmd  = fun(State) ->
                             server_notify_operational(State)
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

     #command{id   = 8,
	      desc = "Send message (hejsan)",
	      cmd  = fun(State) -> 
			     server_send_message(State, "hejsan") 
		     end},

     #command{id   = 9,
	      desc = "Await nothing before receiving (hoppsan) reply",
	      cmd  = fun(State) -> 
			     server_await_nothing(State, 4000) 
		     end},

     #command{id   = 10,
	      desc = "Await reply (hoppsan) to message",
	      cmd  = fun(State) -> 
			     server_await_message(State, "hoppsan", 2000) 
		     end},

     #command{id   = 11,
	      desc = "Await nothing before closing",
	      cmd  = fun(State) -> 
			     server_await_nothing(State, 1000) 
		     end},

     #command{id   = 12,
	      desc = "Close",
	      cmd  = fun(State) -> 
			     server_close(State) 
		     end},

     #command{id   = 13,
	      desc = "Await nothing before stopping transport",
	      cmd  = fun(State) -> 
			     server_await_nothing(State, 1000) 
		     end},

     #command{id   = 14,
	      desc = "Stop",
	      cmd  = fun(State) -> 
			     server_stop_transport(State) 
		     end}

    ].

block_unblock_client_commands(ServerPort, ServerHost) ->
    OwnPort = ServerPort+1, 
    Opts = [{port, OwnPort}], 
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
              desc = "Open",
              cmd  = fun(State) ->
                             client_open(State, Opts)
                     end},

     #command{id   = 4,
              desc = "Await continue",
              cmd  = fun(State) ->
                             client_await_continue_signal(State, 5000)
                     end},

     #command{id   = 5,
              desc = "[pseudo] Connect",
              cmd  = fun(State) ->
                             client_connect(State, ServerHost, ServerPort)
                     end},

     #command{id   = 6,
	      desc = "Send initial message (ping)",
	      cmd  = fun(State) -> 
			     client_send_message(State, "ping") 
		     end},

     #command{id   = 7,
	      desc = "Await reply (pong) to initial message",
	      cmd  = fun(State) -> 
			     client_await_message(State, "pong", 1000) 
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

     #command{id   = 8,
	      desc = "Await message (hejsan)",
	      cmd  = fun(State) -> 
			     client_await_message(State, "hejsan", 5000) 
		     end},

     #command{id   = 9,
	      desc = "Send reply (hoppsan) to message",
	      cmd  = fun(State) -> 
			     client_send_message(State, "hoppsan") 
		     end},

     #command{id   = 10,
	      desc = "Await nothing before closing",
	      cmd  = fun(State) -> 
			     client_await_nothing(State, 1000) 
		     end},

     #command{id   = 11,
	      desc = "Close",
	      cmd  = fun(State) -> 
			     client_close(State) 
		     end},

     #command{id   = 12,
	      desc = "Await nothing before stopping transport",
	      cmd  = fun(State) -> 
			     client_await_nothing(State, 1000) 
		     end},

     #command{id   = 13,
	      desc = "Stop transport",
	      cmd  = fun(State) -> 
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
    start_command_handler(Node, Commands, #server{}, "server").

server_start_transport(State) when is_record(State, server) ->
    case (catch megaco_udp:start_transport()) of
	{ok, Ref} ->
	    {ok, State#server{transport_ref = Ref}};
	Error ->
	    Error
    end.

server_open(#server{transport_ref = Ref} = State, Options) 
  when is_record(State, server) andalso is_list(Options) ->
    Opts = [{receive_handle, self()}, {module, ?MODULE} | Options], 
    case (catch megaco_udp:open(Ref, Opts)) of
	{ok, Socket, ControlPid} ->
	    {ok, State#server{handle      = {socket, Socket},  % Temporary
			      control_pid = ControlPid}};
	{error, {could_not_open_udp_port, eaddrinuse}} ->
	    {skip, {server, eaddrinuse}};
	Error ->
	    Error
    end.

server_notify_operational(#server{parent = Parent} = State) 
  when is_record(State, server) ->
    Parent ! {operational, self()},
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
	    p("received expected event with: "
	      "~n   ControlPid: ~p"
	      "~n   Handle:     ~p", [ControlPid, Handle]),
	    NewState = State#server{handle = Handle},
	    {ok, NewState};

	Any ->
	    p("received unexpected event: ~p", [Any]),
	    {error, {unexpected_event, Any}}

    after Timeout ->
	    {error, timeout}
    end.

server_send_message(#server{handle = Handle} = State, Message) ->
    Bin = if
	      is_list(Message) ->
		  list_to_binary(Message);
	      true ->
		  Message
	  end,
    megaco_udp:send_message(Handle, Bin),
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
	    p("received expected message [~p]", [ExpectMessage]),
	    {ok, State};

	Any ->
	    p("received unexpected event: ~p", [Any]),
	    {error, {unexpected_event, Any}}

    after Timeout ->
	    {error, timeout}
    end.

server_close(#server{handle = {socket, Socket}} = State) ->
    megaco_udp:close(Socket),
    {ok, State#server{handle = undefined, control_pid = undefined}};
server_close(#server{handle = Handle} = State) 
  when (Handle =/= undefined) ->
    megaco_udp:close(Handle),
    {ok, State#server{handle = undefined, control_pid = undefined}}.

%% server_block(#server{handle = Handle} = State) 
%%   when (Handle =/= undefined) ->
%%     megaco_udp:block(Handle),
%%     {ok, State}.

%% server_unblock(#server{handle = Handle} = State) 
%%   when (Handle =/= undefined) ->
%%     megaco_udp:unblock(Handle),
%%     {ok, State}.

server_stop_transport(#server{transport_ref = Ref} = State) 
  when (Ref =/= undefined) ->
    megaco_udp:stop_transport(Ref),
    {ok, State#server{transport_ref = undefined}}.


%% -------  Client command handler and utility functions ----------

client_start_command_handler(Node, Commands) ->
    start_command_handler(Node, Commands, #client{}, "client").
		  
client_start_transport(State) when is_record(State, client) ->
    case (catch megaco_udp:start_transport()) of
	{ok, Ref} ->
	    {ok, State#client{transport_ref = Ref}};
	Error ->
	    Error
    end.

client_open(#client{transport_ref = Ref} = State, Options) 
  when is_record(State, client) andalso is_list(Options) ->
    Opts = [{receive_handle, self()}, {module, ?MODULE} | Options], 
    case (catch megaco_udp:open(Ref, Opts)) of
	{ok, Socket, ControlPid} ->
	    {ok, State#client{handle      = {socket, Socket}, 
			      control_pid = ControlPid}};
	{error, {could_not_open_udp_port, eaddrinuse}} ->
	    {skip, {client, eaddrinuse}};
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

client_connect(#client{handle = {socket, Socket}} = State, Host, Port) ->
    Handle = megaco_udp:create_send_handle(Socket, Host, Port),
    {ok, State#client{handle = Handle}}.

client_send_message(#client{handle = Handle} = State, Message) ->
    Bin = if
	      is_list(Message) ->
		  list_to_binary(Message);
	      true ->
		  Message
	  end,
    megaco_udp:send_message(Handle, Bin),
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
    megaco_udp:block(Handle),
    {ok, State}.

client_unblock(#client{handle = Handle} = State) 
  when (Handle =/= undefined) ->
    megaco_udp:unblock(Handle),
    {ok, State}.

client_close(#client{handle = {socket, Socket}} = State) ->
    megaco_udp:close(Socket),
    {ok, State#client{handle = undefined, control_pid = undefined}};
client_close(#client{handle = Handle} = State) 
  when (Handle =/= undefined) ->
    megaco_udp:close(Handle),
    {ok, State#client{handle = undefined, control_pid = undefined}}.

client_stop_transport(#client{transport_ref = Ref} = State) 
  when (Ref =/= undefined) ->
    megaco_udp:stop_transport(Ref),
    {ok, State#client{transport_ref = undefined}}.

    
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
	{skip, _} = SKIP ->
	    p("command_handler -> cmd ~w skip", [Id]),
	    SKIP;
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
    {error, Bad, Good};
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
		    p("await_command_handler_completion -> ~p done with"
		      "~n   ~p", [Pid, Reason]), 
		    await_command_handler_completion(Pids2, 
						     [{Pid, Reason}|Bad], 
						     Good, 
						     Timeout - (ms() - Begin))
	    end;

	{'EXIT', Pid, {skip, Reason}} ->
	    p("await_command_handler_completion -> "
	      "received skip EXIT signal from ~p with"
	      "~p", [Pid, Reason]), 
	    ?SKIP(Reason)

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
    

