%%%-------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2016. All Rights Reserved.
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
-module(ssl_bench_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct_event.hrl").

-define(remote_host, "NETMARKS_REMOTE_HOST").

suite() -> [{ct_hooks,[{ts_install_cth,[{nodenames,2}]}]}].

all() -> [{group, setup}, {group, payload}].

groups() ->
    [{setup, [{repeat, 3}], [setup_sequential, setup_concurrent]},
     {payload, [{repeat, 3}], [payload_simple]}
    ].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_suite(Config) ->
    try
	Server = setup(ssl, node()),
	[{server_node, Server}|Config]
    catch _:_ ->
	    {skipped, "Benchmark machines only"}
    end.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Func, Conf) ->
    Conf.

end_per_testcase(_Func, _Conf) ->
    ok.


-define(COUNT, 400).
-define(TC(Cmd), tc(fun() -> Cmd end, ?MODULE, ?LINE)).

-define(FPROF_CLIENT, false).
-define(FPROF_SERVER, false).
-define(EPROF_CLIENT, false).
-define(EPROF_SERVER, false).
-define(PERCEPT_SERVER, false).

%% Current numbers gives roughly a testcase per minute on todays hardware..

setup_sequential(Config) ->
    Server = proplists:get_value(server_node, Config),
    Server =/= undefined orelse error(no_server),
    {ok, Result} = do_test(ssl, setup_connection, ?COUNT * 20, 1, Server),
    ct_event:notify(#event{name = benchmark_data,
			   data=[{value, Result},
				 {suite, "ssl"}, {name, "Sequential setup"}]}),
    ok.

setup_concurrent(Config) ->
    Server = proplists:get_value(server_node, Config),
    Server =/= undefined orelse error(no_server),
    {ok, Result} = do_test(ssl, setup_connection, ?COUNT, 100, Server),
    ct_event:notify(#event{name = benchmark_data,
			   data=[{value, Result},
				 {suite, "ssl"}, {name, "Concurrent setup"}]}),
    ok.

payload_simple(Config) ->
    Server = proplists:get_value(server_node, Config),
    Server =/= undefined orelse error(no_server),
    {ok, Result} = do_test(ssl, payload, ?COUNT*300, 10, Server),
    ct_event:notify(#event{name = benchmark_data,
			   data=[{value, Result},
				 {suite, "ssl"}, {name, "Payload simple"}]}),
    ok.


ssl() ->
    test(ssl, ?COUNT, node()).

test(Type, Count, Host) ->
    Server = setup(Type, Host),
    (do_test(Type, setup_connection, Count * 20, 1, Server)),
    (do_test(Type, setup_connection, Count, 100, Server)),
    (do_test(Type, payload, Count*300, 10, Server)),
    ok.

do_test(Type, TC, Loop, ParallellConnections, Server) ->
    _ = ssl:stop(),
    {ok, _} = ensure_all_started(ssl, []),

    {ok, {SPid, Host, Port}} = rpc:call(Server, ?MODULE, setup_server_init,
					[Type, TC, Loop, ParallellConnections]),
    link(SPid),
    Me = self(),
    Test = fun(Id) ->
		   CData = client_init(Me, Type, TC, Host, Port),
		   receive
		       go ->
			   ?FPROF_CLIENT andalso Id =:= 1 andalso
			       start_profile(fprof, [self(),new]),
			   ?EPROF_CLIENT andalso Id =:= 1 andalso
			       start_profile(eprof, [ssl_connection_sup, ssl_manager]),
			   ok = ?MODULE:TC(Loop, Type, CData),
			   ?FPROF_CLIENT andalso Id =:= 1 andalso
			       stop_profile(fprof, "test_connection_client_res.fprof"),
			   ?EPROF_CLIENT andalso Id =:= 1 andalso
			       stop_profile(eprof, "test_connection_client_res.eprof"),
			   Me ! self()
		   end
	   end,
    Spawn = fun(Id) ->
		    Pid = spawn(fun() -> Test(Id) end),
		    receive {Pid, init} -> Pid end
	    end,
    Pids = [Spawn(Id) || Id <- lists:seq(ParallellConnections, 1, -1)],
    Run  = fun() ->
		   [Pid ! go || Pid <- Pids],
		   [receive Pid -> ok end || Pid <- Pids]
	   end,
    {TimeInMicro, _} = timer:tc(Run),
    TotalTests = ParallellConnections * Loop,
    TestPerSecond = 1000000 * TotalTests div TimeInMicro,
    io:format("TC ~p ~p ~p ~p 1/s~n", [TC, Type, ParallellConnections, TestPerSecond]),
    unlink(SPid),
    SPid ! quit,
    {ok, TestPerSecond}.

server_init(ssl, setup_connection, _, _, Server) ->
    {ok, Socket} = ssl:listen(0, ssl_opts(listen)),
    {ok, {_Host, Port}} = ssl:sockname(Socket),
    {ok, Host} = inet:gethostname(),
    ?FPROF_SERVER andalso start_profile(fprof, [whereis(ssl_manager), new]),
    %%?EPROF_SERVER andalso start_profile(eprof, [ssl_connection_sup, ssl_manager]),
    ?EPROF_SERVER andalso start_profile(eprof, [ssl_manager]),
    ?PERCEPT_SERVER andalso percept:profile("/tmp/ssl_server.percept"),
    Server ! {self(), {init, Host, Port}},
    Test = fun(TSocket) ->
		   ok = ssl:ssl_accept(TSocket),
		   ssl:close(TSocket)
	   end,
    setup_server_connection(Socket, Test);
server_init(ssl, payload, Loop, _, Server) ->
    {ok, Socket} = ssl:listen(0, ssl_opts(listen)),
    {ok, {_Host, Port}} = ssl:sockname(Socket),
    {ok, Host} = inet:gethostname(),
    Server ! {self(), {init, Host, Port}},
    Test = fun(TSocket) ->
		   ok = ssl:ssl_accept(TSocket),
		   Size = byte_size(msg()),
		   server_echo(TSocket, Size, Loop),
		   ssl:close(TSocket)
	   end,
    setup_server_connection(Socket, Test);

server_init(Type, Tc, _, _, Server) ->
    io:format("No server init code for ~p ~p~n",[Type, Tc]),
    Server ! {self(), no_init}.

client_init(Master, ssl, setup_connection, Host, Port) ->
    Master ! {self(), init},
    {Host, Port, ssl_opts(connect)};
client_init(Master, ssl, payload, Host, Port) ->
    {ok, Sock} = ssl:connect(Host, Port, ssl_opts(connect)),
    Master ! {self(), init},
    Size = byte_size(msg()),
    {Sock, Size};
client_init(_Me, Type, Tc, Host, Port) ->
    io:format("No client init code for ~p ~p~n",[Type, Tc]),
    {Host, Port}.

setup_server_connection(LSocket, Test) ->
    receive quit ->
	    ?FPROF_SERVER andalso stop_profile(fprof, "test_server_res.fprof"),
	    ?EPROF_SERVER andalso stop_profile(eprof, "test_server_res.eprof"),
	    ?PERCEPT_SERVER andalso stop_profile(percept, "/tmp/ssl_server.percept"),
	    ok
    after 0 ->
	    case ssl:transport_accept(LSocket, 2000) of
		{ok, TSocket} -> spawn_link(fun() -> Test(TSocket) end);
		{error, timeout} -> ok
	    end,
	    setup_server_connection(LSocket, Test)
    end.

server_echo(Socket, Size, Loop) when Loop > 0 ->
    {ok, Msg} = ssl:recv(Socket, Size),
    ok = ssl:send(Socket, Msg),
    server_echo(Socket, Size, Loop-1);
server_echo(_, _, _) -> ok.

setup_connection(N, ssl, Env = {Host, Port, Opts}) when N > 0 ->
    case ssl:connect(Host, Port, Opts) of
	{ok, Sock} ->
	    ssl:close(Sock),
	    setup_connection(N-1, ssl, Env);
	{error, Error} ->
	    io:format("Error: ~p (~p)~n",[Error, length(erlang:ports())]),
	    setup_connection(N, ssl, Env)
    end;
setup_connection(_, _, _) ->
    ok.

payload(Loop, ssl, D = {Socket, Size}) when Loop > 0 ->
    ok = ssl:send(Socket, msg()),
    {ok, _} = ssl:recv(Socket, Size),
    payload(Loop-1, ssl, D);
payload(_, _, {Socket, _}) ->
    ssl:close(Socket).

msg() ->
    <<"Hello", 
      0:(512*8), 
      "asdlkjsafsdfoierwlejsdlkfjsdf", 
      1:(512*8),
      "asdlkjsafsdfoierwlejsdlkfjsdf">>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setup(_Type, nonode@nohost) ->
    exit(dist_not_enabled);
setup(Type, _This) ->
    Host = case os:getenv(?remote_host) of
	       false ->
		   {ok, This} = inet:gethostname(),
		   This;
	       RemHost ->
		   RemHost
	   end,
    Node = list_to_atom("perf_server@" ++ Host),
    SlaveArgs = case init:get_argument(pa) of
	       {ok, PaPaths} ->
		   lists:append([" -pa " ++ P || [P] <- PaPaths]);
	       _ -> []
	   end,
    %% io:format("Slave args: ~p~n",[SlaveArgs]),
    Prog =
	case os:find_executable("erl") of
	    false -> "erl";
	    P -> P
	end,
    io:format("Prog = ~p~n", [Prog]),

    case net_adm:ping(Node) of
	pong -> ok;
	pang ->
	    {ok, Node} = slave:start(Host, perf_server, SlaveArgs, no_link, Prog)
    end,
    Path = code:get_path(),
    true = rpc:call(Node, code, set_path, [Path]),
    ok = rpc:call(Node, ?MODULE, setup_server, [Type, node()]),
    io:format("Client (~p) using ~s~n",[node(), code:which(ssl)]),
    (Node =:= node()) andalso restrict_schedulers(client),
    Node.

setup_server(_Type, ClientNode) ->
    (ClientNode =:= node()) andalso restrict_schedulers(server),
    io:format("Server (~p) using ~s~n",[node(), code:which(ssl)]),
    ok.


ensure_all_started(App, Ack) ->
    case application:start(App) of
	ok -> {ok, [App|Ack]};
	{error, {not_started, Dep}} ->
	    {ok, Ack1} = ensure_all_started(Dep, Ack),
	    ensure_all_started(App, Ack1);
	{error, {already_started, _}} ->
	    {ok, Ack}
    end.

setup_server_init(Type, Tc, Loop, PC) ->
    _ = ssl:stop(),
    {ok, _} = ensure_all_started(ssl, []),
    Me = self(),
    Pid = spawn_link(fun() -> server_init(Type, Tc, Loop, PC, Me) end),
    Res = receive
	      {Pid, {init, Host, Port}} -> {ok, {Pid, Host, Port}};
	      {Pid, Error} -> {error, Error}
	  end,
    unlink(Pid),
    Res.

restrict_schedulers(Type) ->
    %% We expect this to run on 8 core machine
    Extra0 = 1,
    Extra =  if (Type =:= server) -> -Extra0; true -> Extra0 end,
    Scheds = erlang:system_info(schedulers),
    erlang:system_flag(schedulers_online, (Scheds div 2) + Extra).

tc(Fun, Mod, Line) ->
    case timer:tc(Fun) of
	{_,{'EXIT',Reason}} ->
	    io:format("Process EXITED ~p:~p \n", [Mod, Line]),
	    exit(Reason);
	{_T,R={error,_}} ->
	    io:format("Process Error ~p:~p \n", [Mod, Line]),
	    R;
	{T,R} ->
	    io:format("~p:~p: Time: ~p\n", [Mod, Line, T]),
	    R
    end.

start_profile(eprof, Procs) ->
    profiling = eprof:start_profiling(Procs),
    io:format("(E)Profiling ...",[]);
start_profile(fprof, Procs) ->
    fprof:trace([start, {procs, Procs}]),
    io:format("(F)Profiling ...",[]).

stop_profile(percept, File) ->
    percept:stop_profile(),
    percept:analyze(File),
    {started, _Host, Port} = percept:start_webserver(),
    wx:new(),
    wx_misc:launchDefaultBrowser("http://" ++ net_adm:localhost() ++ ":" ++ integer_to_list(Port)),
    ok;
stop_profile(eprof, File) ->
    profiling_stopped = eprof:stop_profiling(),
    eprof:log(File),
    io:format(".analysed => ~s ~n",[File]),
    eprof:analyze(total),
    eprof:stop();
stop_profile(fprof, File) ->
    fprof:trace(stop),
    io:format("..collect..",[]),
    fprof:profile(),
    fprof:analyse([{dest, File},{totals, true}]),
    io:format(".analysed => ~s ~n",[File]),
    fprof:stop(),
    ok.

ssl_opts(listen) ->
    [{backlog, 500} | ssl_opts("server")];
ssl_opts(connect) ->
    [{verify, verify_peer}
     | ssl_opts("client")];
ssl_opts(Role) ->
    Dir = filename:join([code:lib_dir(ssl), "examples", "certs", "etc"]),
    [{active, false},
     {depth, 2},
     {reuseaddr, true},
     {mode,binary},
     {nodelay, true},
     {ciphers, [{dhe_rsa,aes_256_cbc,sha}]},
     {cacertfile, filename:join([Dir, Role, "cacerts.pem"])},
     {certfile, filename:join([Dir, Role, "cert.pem"])},
     {keyfile, filename:join([Dir, Role, "key.pem"])}].
