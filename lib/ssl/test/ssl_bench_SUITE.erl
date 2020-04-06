%%%-------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2020. All Rights Reserved.
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
    [{setup, [{repeat, 3}], [setup_sequential, setup_sequential_noreuse, setup_sequential_13,
                             setup_concurrent, setup_concurrent_noreuse, setup_concurrent_13]},
     {payload, [{repeat, 3}], [payload, payload_13]}
    ].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_suite(Config) ->
    ct:timetrap({minutes, 1}),
    case node() of
        nonode@nohost ->
            {skipped, "Node not distributed"};
        _ ->
            ssl_test_lib:clean_start(),
            [{server_node, ssl_bench_test_lib:setup(perf_server)}|Config]
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

%% Current numbers gives roughly a testcase per minute on todays hardware..

setup_sequential(Config) ->
    Server = proplists:get_value(server_node, Config),
    Server =/= undefined orelse error(no_server),
    Cfg = [{version, 'tlsv1.2'}],
    {ok, Result} = do_test(ssl, {setup_connection,Cfg}, ?COUNT * 20, 1, Server),
    ct_event:notify(#event{name = benchmark_data,
			   data=[{value, Result},
				 {suite, "ssl"}, {name, "Sequential setup"}]}),
    ok.

setup_sequential_noreuse(Config) ->
    Server = proplists:get_value(server_node, Config),
    Server =/= undefined orelse error(no_server),
    Cfg = [{version, 'tlsv1.2'}, no_reuse],
    {ok, Result} = do_test(ssl, {setup_connection,Cfg}, ?COUNT * 20, 1, Server),
    ct_event:notify(#event{name = benchmark_data,
			   data=[{value, Result},
				 {suite, "ssl"}, {name, "Seq setup 1.2 no session"}]}),
    ok.

setup_sequential_13(Config) ->
    Server = proplists:get_value(server_node, Config),
    Server =/= undefined orelse error(no_server),
    Cfg = [{version, 'tlsv1.3'}],
    {ok, Result} = do_test(ssl, {setup_connection,Cfg}, ?COUNT * 20, 1, Server),
    ct_event:notify(#event{name = benchmark_data,
			   data=[{value, Result},
				 {suite, "ssl"}, {name, "Seq setup 1.3"}]}),
    ok.

setup_concurrent(Config) ->
    Server = proplists:get_value(server_node, Config),
    Server =/= undefined orelse error(no_server),
    Cfg = [{version, 'tlsv1.2'}],
    {ok, Result} = do_test(ssl, {setup_connection,Cfg}, ?COUNT, 100, Server),
    ct_event:notify(#event{name = benchmark_data,
			   data=[{value, Result},
				 {suite, "ssl"}, {name, "Concurrent setup"}]}),
    ok.

setup_concurrent_noreuse(Config) ->
    Server = proplists:get_value(server_node, Config),
    Server =/= undefined orelse error(no_server),
    Cfg = [{version, 'tlsv1.2'}, no_reuse],
    {ok, Result} = do_test(ssl, {setup_connection,Cfg}, ?COUNT, 100, Server),
    ct_event:notify(#event{name = benchmark_data,
			   data=[{value, Result},
				 {suite, "ssl"}, {name, "Conc setup 1.2 no session"}]}),
    ok.

setup_concurrent_13(Config) ->
    Server = proplists:get_value(server_node, Config),
    Server =/= undefined orelse error(no_server),
    Cfg = [{version, 'tlsv1.3'}],
    {ok, Result} = do_test(ssl, {setup_connection,Cfg}, ?COUNT, 100, Server),
    ct_event:notify(#event{name = benchmark_data,
			   data=[{value, Result},
				 {suite, "ssl"}, {name, "Conc setup 1.3"}]}),
    ok.

payload(Config) ->
    Server = proplists:get_value(server_node, Config),
    Server =/= undefined orelse error(no_server),
    Cfg = [{version, 'tlsv1.2'}],
    {ok, Result} = do_test(ssl, {payload, Cfg}, ?COUNT*300, 10, Server),
    ct_event:notify(#event{name = benchmark_data,
			   data=[{value, Result},
				 {suite, "ssl"}, {name, "Payload simple"}]}),
    ok.

payload_13(Config) ->
    Server = proplists:get_value(server_node, Config),
    Server =/= undefined orelse error(no_server),
    Cfg = [{version, 'tlsv1.3'}],
    {ok, Result} = do_test(ssl, {payload, Cfg}, ?COUNT*300, 10, Server),
    ct_event:notify(#event{name = benchmark_data,
			   data=[{value, Result},
				 {suite, "ssl"}, {name, "Payload 1.3"}]}),
    ok.

ssl() ->
    test(ssl, ?COUNT).

test(Type, Count) ->
    Server = ssl_bench_test_lib:setup(perf_server),
    (do_test(Type, {setup_connection, [{version, 'tlsv1.2'}]}, Count * 20, 1, Server)),
    (do_test(Type, {setup_connection, [{version, 'tlsv1.3'}]}, Count * 20, 1, Server)),
    (do_test(Type, {setup_connection, [{version, 'tlsv1.2'}, no_reuse]}, Count * 20, 1, Server)),
    (do_test(Type, {setup_connection, [{version, 'tlsv1.2'}]}, Count, 100, Server)),
    (do_test(Type, {payload, [{version, 'tlsv1.2'}]}, Count*300, 10, Server)),
    ok.

do_test(Type, {Func, _}=TC, Loop, ParallellConnections, Server) ->
    _ = ssl:stop(),
    {ok, _} = ensure_all_started(ssl, []),
    Certs = cert_data(),
    {ok, {SPid, Host, Port}} = rpc:call(Server, ?MODULE, setup_server_init,
					[Type, TC, Loop, ParallellConnections, Certs]),
    link(SPid),
    Me = self(),
    Test = fun(Id) ->
		   CData = client_init(Me, Type, TC, Host, Port, Certs),
		   receive
		       go ->
			   ?FPROF_CLIENT andalso Id =:= 1 andalso
			       start_profile(fprof, [self(),new]),
			   ?EPROF_CLIENT andalso Id =:= 1 andalso
			       start_profile(eprof, [ssl_connection_sup, ssl_manager]),
			   ok = ?MODULE:Func(Loop, Type, CData),
			   ?FPROF_CLIENT andalso Id =:= 1 andalso
			       stop_profile(fprof, "test_connection_client_res.fprof"),
			   ?EPROF_CLIENT andalso Id =:= 1 andalso
			       stop_profile(eprof, "test_connection_client_res.eprof"),
			   Me ! self()
		   end
	   end,
    Spawn = fun(Id) ->
		    Pid = spawn_link(fun() -> Test(Id) end),
		    receive {Pid, init} -> Pid end
	    end,
    Pids = [Spawn(Id) || Id <- lists:seq(ParallellConnections, 1, -1)],
    Run  = fun() ->
		   [Pid ! go || Pid <- Pids],
		   [receive Pid -> ok end || Pid <- Pids]
	   end,
    {TimeInMicro, _} = timer:tc(Run),
    TotalTests = ParallellConnections * Loop,
    TestPerSecond = case TimeInMicro of
                        0 ->
                            undefined;
                        _ ->
                            1000000 * TotalTests div TimeInMicro
                    end,
    io:format("TC ~p ~p ~p ~p 1/s~n", [TC, Type, ParallellConnections, TestPerSecond]),
    unlink(SPid),
    SPid ! quit,
    {ok, TestPerSecond}.

server_init(ssl, {setup_connection, Opts}, _, _, Server, Certs) ->
    {ok, LSocket} = ssl:listen(0, ssl_opts(listen, Opts, Certs)),
    {ok, {_Host, Port}} = ssl:sockname(LSocket),
    {ok, Host} = inet:gethostname(),
    ?FPROF_SERVER andalso start_profile(fprof, [whereis(ssl_manager), new]),
    %%?EPROF_SERVER andalso start_profile(eprof, [ssl_connection_sup, ssl_manager]),
    ?EPROF_SERVER andalso start_profile(eprof, [ssl_manager]),
    Server ! {self(), {init, Host, Port}},
    Test = fun(TSocket) ->
		   {ok, Socket} = ssl:handshake(TSocket),
		   ssl:close(Socket)
	   end,
    setup_server_connection(LSocket, Test);
server_init(ssl, {payload, Opts}, Loop, _, Server, Certs) ->
    {ok, LSocket} = ssl:listen(0, ssl_opts(listen, Opts, Certs)),
    {ok, {_Host, Port}} = ssl:sockname(LSocket),
    {ok, Host} = inet:gethostname(),
    Server ! {self(), {init, Host, Port}},
    Test = fun(TSocket) ->
		   {ok, Socket} = ssl:handshake(TSocket),
		   Size = byte_size(msg()),
		   server_echo(Socket, Size, Loop),
		   ssl:close(Socket)
	   end,
    setup_server_connection(LSocket, Test);

server_init(Type, Tc, _, _, Server, _) ->
    io:format("No server init code for ~p ~p~n",[Type, Tc]),
    Server ! {self(), no_init}.

client_init(Master, ssl, {setup_connection, Opts}, Host, Port, Certs) ->
    Master ! {self(), init},
    {Host, Port, ssl_opts(connect, Opts, Certs)};
client_init(Master, ssl, {payload, Opts}, Host, Port, Certs) ->
    {ok, Sock} = ssl:connect(Host, Port, ssl_opts(connect, Opts, Certs)),
    Master ! {self(), init},
    Size = byte_size(msg()),
    {Sock, Size};
client_init(_Me, Type, Tc, Host, Port, _) ->
    io:format("No client init code for ~p ~p~n",[Type, Tc]),
    {Host, Port}.

setup_server_connection(LSocket, Test) ->
    receive quit ->
	    ?FPROF_SERVER andalso stop_profile(fprof, "test_server_res.fprof"),
	    ?EPROF_SERVER andalso stop_profile(eprof, "test_server_res.eprof"),
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

ensure_all_started(App, Ack) ->
    case application:start(App) of
	ok -> {ok, [App|Ack]};
	{error, {not_started, Dep}} ->
	    {ok, Ack1} = ensure_all_started(Dep, Ack),
	    ensure_all_started(App, Ack1);
	{error, {already_started, _}} ->
	    {ok, Ack}
    end.

setup_server_init(Type, Tc, Loop, PC, Certs) ->
    _ = ssl:stop(),
    {ok, _} = ensure_all_started(ssl, []),
    Me = self(),
    Pid = spawn_link(fun() -> server_init(Type, Tc, Loop, PC, Me, Certs) end),
    Res = receive
	      {Pid, {init, Host, Port}} -> {ok, {Pid, Host, Port}};
	      {Pid, Error} -> {error, Error}
	  end,
    unlink(Pid),
    Res.

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

ssl_opts(listen, Opts, Certs) ->
    [{backlog, 500} | ssl_opts(server_config, Opts, Certs)];
ssl_opts(connect, Opts, Certs) ->
    [{verify, verify_peer} | ssl_opts(client_config, Opts, Certs)];
ssl_opts(Role, TCOpts, Certs) ->
    CertData = maps:get(Role, Certs),
    {Version, KeyEx} =
        case proplists:get_value(version, TCOpts) of
            'tlsv1.2' = V -> {V, ecdhe_ecdsa};
            'tlsv1.3' = V -> {V, any}
        end,
    Opts0 = [{active, false},
             {depth, 2},
             {reuseaddr, true},
             {mode,binary},
             {nodelay, true},
             {versions, [Version]},
             {ciphers, [ #{key_exchange => KeyEx, cipher => aes_128_gcm,
                           mac => aead, prf => sha256}
                       ]}
            | CertData ],
    Opts1 = case Role of
                client_config -> [{server_name_indication, disable} | Opts0];
                server_config -> Opts0
            end,
    Opts = case proplists:get_value(no_reuse, TCOpts) of
               true -> [{reuse_sessions, false}|Opts1];
               _ -> Opts1
           end,
    Opts.

cert_data() ->
    ssl_test_lib:make_cert_chains_der(ecdhe_ecdsa, []).

bypass_pem_cache_supported() ->
    %% This function is currently critical to support cache bypass
    %% and did not exist in prior versions.
    catch ssl_pkix_db:module_info(), % ensure module is loaded
    erlang:function_exported(ssl_pkix_db, extract_trusted_certs, 1).

