%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%
-module(old_transport_accept_SUITE).
-include("test_server.hrl").
-include("test_server_line.hrl").

%% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).
-define(application, ssh).

-export([all/1,
	 init_per_testcase/2,
	 fin_per_testcase/2,
	 config/1,
	 echo_once/1,
	 echo_twice/1,
	 close_before_ssl_accept/1,
	 server/5,
	 tolerant_server/5,
	 client/5
	]).

init_per_testcase(_Case, Config) ->
    WatchDog = ssl_test_lib:timetrap(?default_timeout),
    [{watchdog, WatchDog}, {protomod, gen_tcp}, {serialize_accept, true}| 
     Config].

fin_per_testcase(_Case, Config) ->
    WatchDog = ?config(watchdog, Config),
    test_server:timetrap_cancel(WatchDog).

all(doc) ->
    "Test transport_accept and ssl_accept";
all(suite) ->
    [config, echo_once, echo_twice, close_before_ssl_accept].

config(doc) ->
    "Want to se what Config contains.";
config(suite) ->
    [];
config(Config) ->
    io:format("Config: ~p~n", [Config]),
    ok.

echo_once(doc) ->
    "Client sends 256 bytes to server, that receives them, sends them "
	"back, and closes. Client waits for close. Both have certs.";
echo_once(suite) ->
    [];
echo_once(Config) when list(Config) ->
    process_flag(trap_exit, true),
    LPort = 3456,
    {ok, Host} = inet:gethostname(),
    {ok, {COpts, SOpts}} = ssl_test_MACHINE:mk_ssl_cert_opts(Config),
    N = 1,
    Msg = lists:seq(0, 255),
    Self = self(),
    Params = "-pa " ++ filename:dirname(code:which(?MODULE)),
    Node = start_node(server, Params),
    CNode = start_node(client, Params),
    Server = spawn_link(Node, ?MODULE, server, [Self, LPort, SOpts, Msg, N]),
    Client = spawn_link(Node, ?MODULE, client, [Host, LPort, COpts, Msg, N]),
    ok = receive
	     {Server, listening} ->
		 Client ! {Server, listening},
		 ok;
	     E ->
		 io:format("bad receive (1) ~p\n", [E]),
		 E
	 end,
    receive
	{Server, done} ->
	    ok
    end,
    test_server:stop_node(Node),
    test_server:stop_node(CNode).

close_before_ssl_accept(doc) ->
    "Client sends 256 bytes to server, that receives them, sends them "
	"back, and closes. Client waits for close. Both have certs.";
close_before_ssl_accept(suite) ->
    [];
close_before_ssl_accept(Config) when list(Config) ->
    process_flag(trap_exit, true),
    LPort = 3456,
    {ok, Host} = inet:gethostname(),
    {ok, {COpts, SOpts}} = ssl_test_MACHINE:mk_ssl_cert_opts(Config),
    Msg = lists:seq(0, 255),
    Self = self(),
    Params = "-pa " ++ filename:dirname(code:which(?MODULE)),
    Node = start_node(server, Params),
    CNode = start_node(client, Params),
    Server = spawn_link(Node, ?MODULE, tolerant_server,
			[Self, LPort, SOpts, Msg, 2]),
    Client = spawn_link(Node, ?MODULE, client,
			[Host, LPort, COpts, Msg, 1]),
    ok = receive
	     {Server, listening} ->
		 {ok, S} = gen_tcp:connect(Host, LPort, []),
		 gen_tcp:close(S),
		 Client ! {Server, listening},
		 ok;
	     E ->
		 io:format("bad receive (1) ~p\n", [E]),
		 E
	 end,
    receive
	{Server, done} ->
	    ok
    end,
    test_server:stop_node(Node),
    test_server:stop_node(CNode).

client(Host, LPort, COpts, Msg, N) ->
    ok = receive
	     {_Server, listening} ->
		 ok;
	     E ->
		 io:format("bad receive (2) ~p\n", [E]),
		 E
	 end,
    Opts = COpts ++ [{packet, raw}, {active, false}],
    app(),
    lists:foreach(fun(_) ->
			  {ok, S} = ssl:connect(Host, LPort, Opts),
			  ssl:send(S, Msg),
			  {ok, Msg} = ssl:recv(S, length(Msg)),
			  ssl:close(S)
		  end, lists:seq(1, N)).

echo_twice(doc) ->
    "Two clients sends 256 bytes to server, that receives them, sends them "
	"back, and closes. Client waits for close. Both have certs.";
echo_twice(suite) ->
    [];
echo_twice(Config) when list(Config) ->
    process_flag(trap_exit, true),
    LPort = 3456,
    {ok, Host} = inet:gethostname(),
    {ok, {COpts, SOpts}} = ssl_test_MACHINE:mk_ssl_cert_opts(Config),
    N = 2,
    Msg = lists:seq(0, 255),
    Self = self(),
    Params = "-pa " ++ filename:dirname(code:which(?MODULE)),
    Node = start_node(server, Params),
    CNode = start_node(client, Params),
    Server = spawn_link(Node, ?MODULE, server,
			[Self, LPort, SOpts, Msg, N]),
    Client = spawn_link(Node, ?MODULE, client,
			[Host, LPort, COpts, Msg, N]),
    ok = receive
	     {Server, listening} ->
		 Client ! {Server, listening},
		 ok;
	     E ->
		 io:format("bad receive (3) ~p\n", [E]),
		 E
	 end,
    receive
	{Server, done} ->
	    ok
    end,
    test_server:stop_node(Node),
    test_server:stop_node(CNode).

server(Client, Port, SOpts, Msg, N) ->
    app(),
    process_flag(trap_exit, true),
    Opts = SOpts ++ [{packet, raw}, {active, false}],
    {ok, LSock} = ssl:listen(Port, Opts),
    Client ! {self(), listening},
    server_loop(Client, LSock, Msg, N).

server_loop(Client, _, _, 0) ->
    Client ! {self(), done};
server_loop(Client, LSock, Msg, N) ->
    {ok, S} = ssl:transport_accept(LSock),
    ok = ssl:ssl_accept(S),
    %% P = ssl:controlling_process(S, Proxy),
    {ok, Msg} = ssl:recv(S, length(Msg)),
    ok = ssl:send(S, Msg),
    ok = ssl:close(S),
    server_loop(Client, LSock, Msg, N-1).

tolerant_server(Client, Port, SOpts, Msg, N) ->
    app(),
    process_flag(trap_exit, true),
    Opts = SOpts ++ [{packet, raw}, {active, false}],
    {ok, LSock} = ssl:listen(Port, Opts),
    Client ! {self(), listening},
    tolerant_server_loop(Client, LSock, Msg, N).

tolerant_server_loop(Client, _, _, 0) ->
    Client ! {self(), done};
tolerant_server_loop(Client, LSock, Msg, N) ->
    {ok, S} = ssl:transport_accept(LSock),
    case ssl:ssl_accept(S) of
	ok ->
	    %% P = ssl:controlling_process(S, Proxy),
	    {ok, Msg} = ssl:recv(S, length(Msg)),
	    ok = ssl:send(S, Msg),
	    ok = ssl:close(S);
	E ->
	    io:format("ssl_accept error: ~p\n", [E])
    end,
    tolerant_server_loop(Client, LSock, Msg, N-1).

app() ->
    crypto:start(),
    application:start(public_key),
    ssl:start().

start_node(Kind, Params) ->
    S = atom_to_list(?MODULE)++"_" ++ atom_to_list(Kind),
    {ok, Node} = test_server:start_node(list_to_atom(S), slave, [{args, Params}]),
    Node.

