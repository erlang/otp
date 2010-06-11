%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2010. All Rights Reserved.
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
-module(ssl_test_MACHINE).

-export([many_conns/0, mk_ssl_cert_opts/1, test_one_listener/7, 
	 test_server_only/6]).

-export([process_init/3, do_start/1]).


-include("test_server.hrl").
-include("ssl_test_MACHINE.hrl").

-define(WAIT_TIMEOUT, 10000).
-define(CLOSE_WAIT, 1000).

%%
%% many_conns() -> ManyConnections
%%
%% Choose a suitable number of "many connections" depending on platform
%% and current limit for file descriptors.
%%
many_conns() ->
    case os:type() of
	{unix,_} -> many_conns_1();
	_ -> 10
    end.

many_conns_1() ->
    N0 = os:cmd("ulimit -n"),
    N1 = lists:reverse(N0),
    N2 = lists:dropwhile(fun($\r) -> true;
			    ($\n) -> true;
			    (_) -> false
			 end, N1),
    N = list_to_integer(lists:reverse(N2)),
    lists:min([(N - 10) div 2, 501]).
	
%%
%% mk_ssl_cert_opts(Config) -> {ok, {COpts, SOpts}}
%%
%% 
mk_ssl_cert_opts(_Config) ->
    Dir = filename:join([code:lib_dir(ssl), "examples", "certs", "etc"]),
    COpts = [{ssl_imp, old},
	     {cacertfile, filename:join([Dir, "client", "cacerts.pem"])}, 
	     {certfile, filename:join([Dir, "client", "cert.pem"])}, 
	     {keyfile, filename:join([Dir, "client", "key.pem"])}], 
    SOpts = [{ssl_imp, old},
	     {cacertfile, filename:join([Dir, "server", "cacerts.pem"])}, 
	     {certfile, filename:join([Dir, "server", "cert.pem"])}, 
	     {keyfile, filename:join([Dir, "server", "key.pem"])}], 
    {ok, {COpts, SOpts}}.

%%
%% Cmds:
%%		{protomod, gen_tcp | ssl}             		default = ssl
%%		{serialize_accept, true | false}  	default = false
%%		{timeout, Timeout}
%%		{sockopts, Opts}
%%		{sslopts, Opts}
%%		{protocols, Protocols}  [sslv2|sslv3|tlsv1]
%%		{listen, Port}
%%		{lsock, LSock}		listen socket for acceptor
%%		peercert
%%		accept
%%		{connect, {Host, Port}}
%%		{recv, N}
%%		{send, N}
%%		{echo, N}		async echo back
%%		close			close connection socket
%%		{close, Time}		wait time and then close socket
%%		lclose			close listen socket
%%		await_close		wait for close
%%		wait_sync		listener's wait for sync from parent
%%		connection_info		
%%		{exit, Reason}		exit
%%
%%
%% We cannot have more than `backlog' acceptors at the same time.
%%


%%
%% test_one_listener(NConns, LCmds, ACmds, CCmds, Timeout, Suite, Config)
%%
%% Creates one client and one server node, and runs one listener on
%% the server node (according to LCmds), and creates NConns acceptors
%% on the server node, and the same number of connectors on the client
%% node. The acceptors and and connectors execute according to ACmds
%% and CCmds, respectively.
%%
%% It is a good idea to have the backlog size in LCmds set to
%% be at least as large as NConns.
%%
test_one_listener(NConns, LCmds0, ACmds0, CCmds0, Timeout, Suite, Config) ->
    ProtoMod = get_protomod(Config), 
    SerializeAccept = get_serialize_accept(Config), 
    ?line {ok, {CNode, SNode}} = start_client_server_nodes(Suite),
    case ProtoMod of
	ssl -> 
	    ?line ok = start_ssl([CNode, SNode], Config);
	gen_tcp ->
	    ok
    end, 
    LCmds = [{protomod, ProtoMod}| LCmds0], 
    ACmds = [{protomod, ProtoMod}, {serialize_accept, SerializeAccept}| 
	     ACmds0], 
    CCmds = [{protomod, ProtoMod}| CCmds0], 

    ?line {ok, Listener} = start_process(SNode, self(), LCmds, listener),
    ?line {ok, LSock} = wait_lsock(Listener, ?WAIT_TIMEOUT),
    ?line {ok, Accs0} = start_processes(NConns, SNode, self(), 
				       [{lsock, LSock}| ACmds], acceptor),
    Accs = case ProtoMod of 
	       gen_tcp ->
		   [Acc1| Accs1] = Accs0,
		   Acc1 ! {continue_accept, self()},
		   Accs1;
	       ssl  ->
		   Accs0
	   end,
    ?line {ok, Conns} = start_processes(NConns, CNode, self(), 
					CCmds, connector),
    ?line case wait_ack(Accs, Accs0 ++ Conns, Timeout) of
	      ok ->
		  ?line sync([Listener]),
		  ?line wait_ack([], [Listener], ?WAIT_TIMEOUT);
	      {error, Reason} ->
		  ?line stop_node(SNode),
		  ?line stop_node(CNode),
		  exit(Reason)
	  end,
    ?line stop_node(SNode),
    ?line stop_node(CNode),
    ok.

%%
%% test_server_only(NConns, LCmds, ACmds, Timeout, Suite, Config)
%%
%% Creates only one server node, and runs one listener on
%% the server node (according to LCmds), and creates NConns acceptors
%% on the server node. The acceptors execute according to ACmds.
%% There are no connectors.
%%
test_server_only(NConns, LCmds0, ACmds0, Timeout, Suite, Config) ->
    ProtoMod = get_protomod(Config), 
    ?line {ok, SNode} = start_server_node(Suite),
    case ProtoMod of
	ssl -> 
	    ?line ok = start_ssl([SNode], Config);
	gen_tcp ->
	    ok
    end, 
    LCmds = [{protomod, ProtoMod}| LCmds0], 
    ACmds = [{protomod, ProtoMod}| ACmds0], 
    ?line {ok, Listener} = start_process(SNode, self(), LCmds, listener),
    ?line {ok, LSock} = wait_lsock(Listener, ?WAIT_TIMEOUT),
    ?line {ok, Accs0} = start_processes(NConns, SNode, self(), 
				       [{lsock, LSock}| ACmds], acceptor),
    Accs = case ProtoMod of 
	       gen_tcp ->
		   [Acc1| Accs1] = Accs0,
		   Acc1 ! {continue_accept, self()},
		   Accs1;
	       ssl  ->
		   Accs0
	   end,
    ?line case wait_ack(Accs, Accs0, Timeout) of
	      ok ->
		  ?line sync([Listener]),
		  ?line wait_ack([], [Listener], ?WAIT_TIMEOUT);
	      {error, Reason} ->
		  ?line stop_node(SNode),
		  exit(Reason)
	  end,
    ?line stop_node(SNode),
    ok.

%%
%% start_client_server_nodes(Suite) -> {ok, {CNode, SNode}}
%%
start_client_server_nodes(Suite) ->
    {ok, CNode} = start_client_node(Suite),
    {ok, SNode} = start_server_node(Suite),
    {ok, {CNode, SNode}}.

start_client_node(Suite) ->
    start_node(lists:concat([Suite, "_client"])).

start_server_node(Suite) ->
    start_node(lists:concat([Suite, "_server"])).

%%
%% start_ssl(Nodes, Config)
%%
start_ssl(Nodes, Config) -> 
    Env0 = lists:flatten([Env00 || {env, Env00} <- Config]),
    Env1 = case os:getenv("SSL_DEBUG") of
	       false ->
		   [];
	       _ ->
		   Dir = ?config(priv_dir, Config),
		   [{debug, true}, {debugdir, Dir}]
	   end,
    Env = Env0 ++ Env1,
    lists:foreach(
      fun(Node) -> rpc:call(Node, ?MODULE, do_start, [Env]) end, Nodes),
    ok.

do_start(Env) ->
    application:start(crypto),
    application:start(public_key),
    application:load(ssl),
    lists:foreach(
      fun({Par, Val}) -> application:set_env(ssl, Par, Val) end, Env),
    application:start(ssl).

    
%%
%% start_node(Name) -> {ok, Node}
%% start_node(Name, ExtraParams) -> {ok, Node}
%%
start_node(Name) ->
    start_node(Name, []).
start_node(Name, ExtraParams) ->
    Params = "-pa " ++ filename:dirname(code:which(?MODULE)) ++ " " ++
	ExtraParams,
    test_server:start_node(Name, slave, [{args, Params}]).

stop_node(Node) ->
    test_server:stop_node(Node).

%%
%% start_processes(N, Node, Parent, Cmds, Type) -> {ok, Pids}
%%
start_processes(M, Node, Parent, Cmds, Type) ->
    start_processes1(0, M, Node, Parent, Cmds, Type, []).
start_processes1(M, M, _, _, _, _, Pids) ->
    {ok, lists:reverse(Pids)};
start_processes1(N, M, Node, Parent, Cmds, Type, Pids) ->
    {ok, Pid} = start_process(Node, Parent, Cmds, {Type, N + 1}),
    start_processes1(N + 1, M, Node, Parent, Cmds, Type, [Pid| Pids]).

%%
%% start_process(Node, Parent, Cmds, Type) -> {ok, Pid}
%%
start_process(Node, Parent, Cmds0, Type) ->
    Cmds = case os:type() of 
	       {win32, _} ->
		   lists:map(fun(close) -> {close, ?CLOSE_WAIT};
				(Term) -> Term end, Cmds0);
	       _ ->
		   Cmds0
	   end,
    Pid = spawn_link(Node, ?MODULE, process_init, [Parent, Cmds, Type]),
    {ok, Pid}.

process_init(Parent, Cmds, Type) ->
    ?debug("#### ~w start~n", [{Type, self()}]),
    pre_main_loop(Cmds, #st{parent = Parent, type = Type}).

%%
%% pre_main_loop
%%
pre_main_loop([], St) ->
    ?debug("#### ~w end~n", [{St#st.type, self()}]),
    main_loop([], St);
pre_main_loop(Cmds, St) ->
    ?debug("#### ~w -> ~w~n", 
	   [{St#st.type, self(), St#st.sock, St#st.port, 
	     St#st.peer, St#st.active}, hd(Cmds)]),
    main_loop(Cmds, St).
    
%%
%% main_loop(Cmds, St)
%%
main_loop([{protomod, ProtoMod}| Cmds], St) ->
    pre_main_loop(Cmds, St#st{protomod = ProtoMod});

main_loop([{serialize_accept, Bool}| Cmds], St) ->
    pre_main_loop(Cmds, St#st{serialize_accept = Bool});

main_loop([{sockopts, Opts}| Cmds], St) ->
    pre_main_loop(Cmds, St#st{sockopts = Opts});

main_loop([{sslopts, Opts}| Cmds], St) ->
    pre_main_loop(Cmds, St#st{sslopts = Opts});

main_loop([{protocols, Protocols}| Cmds], St) ->
    pre_main_loop(Cmds, St#st{protocols = Protocols});

main_loop([{timeout, T}| Cmds], St) ->
    pre_main_loop(Cmds, St#st{timeout = T});

main_loop([{lsock, LSock}| Cmds], St) ->
    pre_main_loop(Cmds, St#st{lsock = LSock});

main_loop([{seed, Data}| Cmds], St) ->
    case ssl:seed("tjosan") of
	ok ->
	    pre_main_loop(Cmds, St);
	{error, Reason} ->
	    ?error("#### ~w(~w) in seed: error: ~w~n", 
		   [St#st.type, self(), Reason]),
	    exit(Reason)
    end;

main_loop([{listen, Port}| Cmds], St) ->
    case listen(St, Port) of    
	{ok, LSock} ->
	    ack_lsock(St#st.parent, LSock),
	    NSt = get_active(St#st{port = Port, sock = LSock, lsock = LSock}),
	    pre_main_loop(Cmds, St);
	{error, Reason} ->
	    ?error("#### ~w(~w) in listen: error: ~w~n", 
		   [St#st.type, self(), Reason]),
	    exit(Reason)
    end;

main_loop([accept| Cmds], St) ->
    case St#st.serialize_accept of
	true ->
	    Parent = St#st.parent, 
	    receive 
		{continue_accept, Parent} ->
		    ok
	    end;
	false ->
	    ok
    end,
    case accept(St) of
	{ok, Sock, Port, Peer} ->
	    case St#st.serialize_accept of
		true ->
		    St#st.parent ! {one_accept_done, self()};
		false ->
		    ok
	    end,
	    NSt = get_active(St#st{sock = Sock, port = Port, peer = Peer}),
	    pre_main_loop(Cmds, NSt);
	{error, Reason} ->
	    ?error("#### ~w(~w) in accept: error: ~w~n", 
		   [St#st.type, self(), Reason]),
	    exit(Reason)
    end;
	
main_loop([accept_timeout| Cmds], St) ->
    case accept(St) of
	{error, timeout} ->
	    pre_main_loop(Cmds, St);
	{error, Reason} ->
	    ?error("#### ~w(~w) in accept_timeout: error: ~w~n", 
		   [St#st.type, self(), Reason]),
	    exit(Reason)
    end;
	

main_loop([{connect, {Host, Port}}| Cmds], St) ->
    case connect(St, Host, Port) of
	{ok, Sock, LPort, Peer} ->
	    NSt = get_active(St#st{sock = Sock, port = LPort, peer = Peer}),
	    pre_main_loop(Cmds, NSt);
	{error, Reason} ->
	    ?error("#### ~w(~w) in connect: error: ~w~n", 
		   [St#st.type, self(), Reason]),
	    exit(Reason)
    end;

main_loop([connection_info| Cmds], St) ->
    case connection_info(St) of
	{ok, ProtoInfo} ->
	    io:fwrite("Got connection_info:~n~p~n", [ProtoInfo]),
	    pre_main_loop(Cmds, St);
	{error, Reason} ->
	    ?error("#### ~w(~w) in connection_info: error: ~w~n", 
		   [St#st.type, self(), Reason]),
	    exit(Reason)
    end;

main_loop([peercert| Cmds], St) ->
    case peercert(St) of
	{ok, Cert} ->
	    io:fwrite("Got cert:~n~p~n", [Cert]),
	    pre_main_loop(Cmds, St);
	{error, Reason} ->
	    ?error("#### ~w(~w) in peercert: error: ~w~n", 
		   [St#st.type, self(), Reason]),
	    exit(Reason)
    end;

main_loop([nopeercert| Cmds], St) ->
    case peercert(St) of
	{error, Reason} ->
	    io:fwrite("Got no cert as expected. reason:~n~p~n", [Reason]),
	    pre_main_loop(Cmds, St);
	{ok, Cert} ->
	    ?error("#### ~w(~w) in peercert: error: got cert: ~p~n", 
		   [St#st.type, self(), Cert]),
	    exit(peercert)
    end;

main_loop([{recv, N}| Cmds], St) ->
    recv_loop([{recv, N}| Cmds], fun recv/1, St); % Returns to main_loop/2.

main_loop([{send, N}| Cmds], St) ->
    Msg = mk_msg(N),
    case send(St, Msg) of 
	ok ->
	    pre_main_loop(Cmds, St);
	{error, Reason} ->
	    ?error("#### ~w(~w) in send: error: ~w~n", 
		   [St#st.type, self(), Reason]),
	    exit(Reason)
    end;

main_loop([{echo, N}| Cmds], St) ->
    recv_loop([{echo, N}| Cmds], fun echo/1, St); % Returns to main_loop/2.

main_loop([{close, WaitTime}| Cmds], St) ->
    wait(WaitTime),
    pre_main_loop([close| Cmds], St);

main_loop([close| Cmds], St) ->
    case close(St) of
	ok ->
	    pre_main_loop(Cmds, St#st{sock = nil});
	{error, Reason} ->
	    ?error("#### ~w(~w) in close: error: ~w~n", 
		   [St#st.type, self(), Reason]),
	    exit(Reason)
    end;

main_loop([lclose| Cmds], St) ->
    case lclose(St) of
	ok ->
	    pre_main_loop(Cmds, St#st{lsock = nil});
	{error, Reason} ->
	    ?error("#### ~w(~w) in lclose: error: ~w~n", 
		   [St#st.type, self(), Reason]),
	    exit(Reason)
    end;

main_loop([await_close| Cmds], St) ->
    case await_close(St) of
	ok ->
	    pre_main_loop(Cmds, St#st{sock = nil});
	{error, Reason} ->
	    ?error("#### ~w(~w) in await_close: error: ~w~n", 
		   [St#st.type, self(), Reason]),
	    exit(Reason)
    end;

main_loop([wait_sync| Cmds], St) ->
    wait_sync(St),
    pre_main_loop(Cmds, St);

main_loop({exit, Reason}, _St) ->
    exit(Reason);

main_loop([], _St) ->
    ok.

%%
%% recv_loop(Cmds, F, St)
%%
%% F = recv/1 | echo/1
%%
recv_loop([{_Tag, 0}| Cmds], _, St) ->
    pre_main_loop(Cmds, St);
recv_loop([{_Tag, N}| _Cmds], _, St) when N < 0 ->
    ?error("#### ~w(~w) in recv_loop: error: too much: ~w~n", 
	   [St#st.type, self(), N]),
    exit(toomuch);                               % XXX or {error, Reason}?
recv_loop([{Tag, N}| Cmds], F, St) ->
    case F(St) of
	{ok, Len} ->
	    NSt = St#st{active = new_active(St#st.active)},
	    if
		Len == N ->
		    pre_main_loop(Cmds, NSt);
		true ->
		    ?debug("#### ~w -> ~w~n", 
			   [{NSt#st.type, self(), NSt#st.sock, NSt#st.port, 
			     NSt#st.peer, NSt#st.active}, {Tag, N - Len}]),
		    recv_loop([{Tag, N - Len}| Cmds], F, NSt)
	    end;
	{error, Reason} ->
	    ?error("#### ~w(~w) in recv_loop: error: ~w, ~w bytes remain~n", 
		   [St#st.type, self(), Reason, N]),
	    exit(Reason)
    end.

new_active(once) ->
    false;
new_active(A) ->
    A.

get_active(St) ->
    A = case proplists:get_value(active, St#st.sockopts, undefined) of
	    undefined ->
		Mod = case St#st.protomod of
			  ssl ->
			      ssl;
			  gen_tcp ->
			      inet
		      end, 
		{ok, [{active, Ax}]} = Mod:getopts(St#st.sock, [active]),
		Ax;
	    Ay ->
		Ay
	end,
    ?debug("#### ~w(~w) get_active: ~p\n", [St#st.type, self(), A]),
    St#st{active = A}.


%%
%% SOCKET FUNCTIONS
%%

%%
%% ssl
%%

%%
%% listen(St, LPort) -> {ok, LSock} | {error, Reason}
%%
listen(St, LPort) ->
    case St#st.protomod of
	ssl ->
	    ssl:listen(LPort, [{ssl_imp, old} | St#st.sockopts ++ St#st.sslopts]);
	gen_tcp ->
	    gen_tcp:listen(LPort, St#st.sockopts)
    end.

%%
%% accept(St) -> {ok, Sock} | {error, Reason}
%%
accept(St) ->
    case St#st.protomod of
	ssl ->
	    case ssl:transport_accept(St#st.lsock, St#st.timeout) of
		{ok, Sock} ->
		    case ssl:ssl_accept(Sock, St#st.timeout) of			
			ok ->
			    {ok, Port} = ssl:sockname(Sock),
			    {ok, Peer} = ssl:peername(Sock),
			    {ok, Sock, Port, Peer};
			Other  ->
			    Other
		    end;
		Other  ->
		    Other
	    end;
	gen_tcp ->
	    case gen_tcp:accept(St#st.lsock, St#st.timeout) of
		{ok, Sock} ->
		    {ok, Port} = inet:port(Sock),
		    {ok, Peer} = inet:peername(Sock),
		    {ok, Sock, Port, Peer};
		Other  ->
		    Other
	    end
    end.

%%
%% connect(St, Host, Port) -> {ok, Sock} | {error, Reason}
%%
connect(St, Host, Port) ->
    
    case St#st.protomod of
	ssl ->
	    case ssl:connect(Host, Port, 
			     [{ssl_imp, old} | St#st.sockopts ++ St#st.sslopts], 
			     St#st.timeout) of
		{ok, Sock} ->
		    {ok, LPort} = ssl:sockname(Sock),
		    {ok, Peer} = ssl:peername(Sock),
		    {ok, Sock, LPort, Peer};
		Other  ->
		    Other
	    end;
	gen_tcp ->
	    case gen_tcp:connect(Host, Port, St#st.sockopts, St#st.timeout) of
		{ok, Sock} ->
		    {ok, LPort} = inet:port(Sock),
		    {ok, Peer} = inet:peername(Sock),
		    {ok, Sock, LPort, Peer};
		Other  ->
		    Other
	    end
    end.

%%
%% peercert(St) -> {ok, Cert} | {error, Reason}
%%
peercert(St) ->
    case St#st.protomod of
	ssl ->
	    ssl:peercert(St#st.sock, [ssl]);
	gen_tcp  ->
	    {ok, <<>>}
    end.

%%
%% connection_info(St) -> {ok, ProtoInfo} | {error, Reason}
%%
connection_info(St) ->
    case St#st.protomod of
	ssl ->
	    case ssl:connection_info(St#st.sock) of
		Res = {ok, {Proto, _}} ->
		    case St#st.protocols of
			[] ->
			    Res;
			Protocols ->
			    case lists:member(Proto, Protocols) of
				true ->
				    Res;
				false ->
				    {error, Proto}
			    end
		    end;
		Error ->
		    Error
	    end;
	gen_tcp  ->
	    {ok, <<>>}
    end.

%%
%% close(St) -> ok | {error, Reason}
%%

close(St) ->
    Mod = St#st.protomod,
    case St#st.sock of
	nil ->
	    ok;
	_ ->
	    Mod:close(St#st.sock)
    end.

%%
%% lclose(St) -> ok | {error, Reason}
%%
lclose(St) ->
    Mod = St#st.protomod,
    case St#st.lsock of
	nil ->
	    ok;
	_ ->
	    Mod:close(St#st.lsock)
    end.

%%
%% recv(St) = {ok, Len} | {error, Reason}
%%
recv(St) ->
    case do_recv(St) of
	{ok, Msg} ->
	    {ok, length(Msg)};
	{error, Reason} ->
	    {error, Reason}
    end.

do_recv(St) when St#st.active == false ->
    %% First check that we do *not* have any ssl/gen_tcp messages in the
    %% message queue, then call the receive function.
    Sock = St#st.sock,
    case St#st.protomod of
	ssl ->
	    receive 
		M = {ssl, Sock, _Msg} ->
		    {error, {unexpected_messagex, M}};
		M = {ssl_closed, Sock} ->
		    {error, {unexpected_message, M}};
		M = {ssl_error, Sock, _Reason} ->
		    {error, {unexpected_message, M}}
	    after 0 ->
		    ssl:recv(St#st.sock, 0, St#st.timeout)
	    end;
	gen_tcp  ->
	    receive 
		M = {tcp, Sock, _Msg} ->
		    {error, {unexpected_message, M}};
		M = {tcp_closed, Sock} ->
		    {error, {unexpected_message, M}};
		M = {tcp_error, Sock, _Reason} ->
		    {error, {unexpected_message, M}}
	    after 0 ->
		    gen_tcp:recv(St#st.sock, 0, St#st.timeout)
	    end
    end;
do_recv(St) ->
    Sock = St#st.sock,
    Timeout = St#st.timeout,
    case St#st.protomod of
	ssl ->
	    receive 
		{ssl, Sock, Msg} ->
		    {ok, Msg};
		{ssl_closed, Sock} ->
		    {error, closed};
		{ssl_error, Sock, Reason} ->
		    {error, Reason}
	    after Timeout ->
		    {error, timeout}
	    end;
	gen_tcp  ->
	    receive 
		{tcp, Sock, Msg} ->
		    {ok, Msg};
		{tcp_closed, Sock} ->
		    {error, closed};
		{tcp_error, Sock, Reason} ->
		    {error, Reason}
	    after Timeout ->
		    {error, timeout}
	    end
    end.

%%
%% echo(St) = {ok, Len} | {error, Reason}
%%
echo(St) ->
    Sock = St#st.sock,
    case do_recv(St) of
	{ok, Msg} ->
	    Mod = St#st.protomod, 
	    case Mod:send(Sock, Msg) of
		ok ->
		    {ok, length(Msg)};
		{error, Reason} ->
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%%
%% send(St, Msg) -> ok | {error, Reason}
%%
send(St, Msg) ->
    Mod = St#st.protomod,
    Mod:send(St#st.sock, Msg).

%%
%% await_close(St) -> ok | {error, Reason}
%%
await_close(St) when St#st.active == false ->
    %% First check that we do *not* have any ssl/gen_tcp messages in the
    %% message queue, then call the receive function.
    Sock = St#st.sock,
    Res = case St#st.protomod of
	      ssl ->
		  receive 
		      M = {ssl, Sock, _Msg0} ->
			  {error, {unexpected_message, M}};
		      M = {ssl_closed, Sock} ->
			  {error, {unexpected_message, M}};
		      M = {ssl_error, Sock, _Reason} ->
			  {error, {unexpected_message, M}}
		  after 0 ->
			  ok
		  end;
	      gen_tcp  ->
		  receive 
		      M = {tcp, Sock, _Msg0} ->
			  {error, {unexpected_message, M}};
		      M = {tcp_closed, Sock} ->
			  {error, {unexpected_message, M}};
		      M = {tcp_error, Sock, _Reason} ->
			  {error, {unexpected_message, M}}
		  after 0 ->
			  ok
		  end
	  end,
    case Res of
	ok ->
	    Mod = St#st.protomod, 
	    case Mod:recv(St#st.sock, 0, St#st.timeout) of
		{ok, _Msg} ->
		    {error, toomuch};
		{error, _} ->
		    ok
	    end;
	_  ->
	    Res
    end;
await_close(St) ->
    Sock = St#st.sock,
    Timeout = St#st.timeout,
    case St#st.protomod of 
	ssl ->
	    receive 
		{ssl, Sock, _Msg} ->
		    {error, toomuch};
		{ssl_closed, Sock} ->
		    ok;
		{ssl_error, Sock, Reason} ->
		    {error, Reason}
	    after Timeout ->
		    {error, timeout}
	    end;
	gen_tcp  ->
	    receive 
		{tcp, Sock, _Msg} ->
		    {error, toomuch};
		{tcp_closed, Sock} ->
		    ok;
		{tcp_error, Sock, Reason} ->
		    {error, Reason}
	    after Timeout ->
		    {error, timeout}
	    end
    end.


%%
%% HELP FUNCTIONS
%%

wait_ack(_, [], _) ->
    ok;
wait_ack(AccPids0, Pids, Timeout) ->
    ?debug("#### CONTROLLER: waiting for ~w~n", [Pids]),
    receive
	{one_accept_done, Pid} ->
	    case lists:delete(Pid, AccPids0) of
		[] ->
		    wait_ack([], Pids, Timeout);
		[AccPid| AccPids1] ->
		    AccPid ! {continue_accept, self()},
		    wait_ack(AccPids1, Pids, Timeout)
	    end;
	{'EXIT', Pid, normal} ->
	    wait_ack(AccPids0, lists:delete(Pid, Pids), Timeout);
	{'EXIT', Pid, Reason} ->
	    ?error("#### CONTROLLER got abnormal exit: ~w, ~w~n", 
		   [Pid, Reason]),
	    {error, Reason}
    after Timeout ->
	    ?error("#### CONTROLLER exiting because of timeout = ~w~n", 
		   [Timeout]),
	    {error, Timeout}
    end.


%%
%% ack_lsock(Pid, LSock)
%%
ack_lsock(Pid, LSock) ->
    Pid ! {lsock, self(), LSock}.

wait_lsock(Pid, Timeout) ->
    receive
	{lsock, Pid, LSock} ->
	    {ok, LSock}
    after Timeout ->
	    exit(timeout)
    end.

%%
%% sync(Pids)
%%
sync(Pids) ->
    lists:foreach(fun (Pid) -> Pid ! {self(), sync} end, Pids).

%%
%% wait_sync(St)
%%
wait_sync(St) ->
    Pid = St#st.parent, 
    receive
	{Pid, sync} ->
	    ok
    end.

%% 
%% wait(Time)
%%
wait(Time) ->
    receive
    after Time ->
	    ok
    end.

%%
%% mk_msg(Size)
%%
mk_msg(Size) ->
    mk_msg(0, Size, []).

mk_msg(_, 0, Acc) ->
    Acc;
mk_msg(Pos, Size, Acc) ->
    C = (((Pos + Size) rem 256) - 1) band 255,
    mk_msg(Pos, Size - 1, [C| Acc]).

%%
%% get_protomod(Config)
%%
get_protomod(Config) ->
    case lists:keysearch(protomod, 1, Config) of
	{value, {_, ProtoMod}} ->
	    ProtoMod;
	false  ->
	    ssl
    end.

%%
%% get_serialize_accept(Config)
%%
get_serialize_accept(Config) ->
    case lists:keysearch(serialize_accept, 1, Config) of
	{value, {_, Val}} ->
	    Val;
	false  ->
	    false
    end.

