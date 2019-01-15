%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2018. All Rights Reserved.
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
-module(httpd_bench_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/file.hrl").

-define(remote_host, "NETMARKS_REMOTE_HOST").
-define(LF, [10]).
-define(CR, [13]).
-define(CRLF, ?CR ++ ?LF).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
suite() -> 
    [{timetrap, {minutes, 1}},
     {ct_hooks,[{ts_install_cth,[{nodenames,2}]}]}].

all() -> 
    [
     {group, http_dummy},
     {group, http_inets},
     {group, http_nginx},
     {group, https_inets},
     {group, https_dummy},
     {group, https_nginx},
     {group, http_dummy_keep_alive},
     {group, http_inets_keep_alive},
     {group, http_nginx_keep_alive},
     {group, https_inets_keep_alive},
     {group, https_dummy_keep_alive},
     {group, https_nginx_keep_alive}
    ].

groups() -> 
    [
     {http_dummy, [],  client_tests()},
     {http_inets, [],   client_tests()},
     {http_nginx, [],   client_tests()},
     {https_dummy, [],  client_tests()}, 
     {https_inets, [],  client_tests()},
     {https_nginx, [],  client_tests()},
     {http_dummy_keep_alive, [],  client_tests()},
     {http_inets_keep_alive, [],  client_tests()},
     {http_nginx_keep_alive, [],  client_tests()},
     {https_dummy_keep_alive, [], client_tests()},
     {https_inets_keep_alive, [], client_tests()},
     {https_nginx_keep_alive, [], client_tests()}
    ].

    
client_tests() ->
    [wget_small,
     erl_dummy_small,
     httpc_small,
     wget_big,
     erl_dummy_big, 
     httpc_big
    ].

init_per_suite(Config) -> 
    try	
	{Node, Host} = setup(Config, node()),
	init_ssl(Config),
	[{iter, 10}, {server_node, Node}, {server_host, Host} | Config]
    catch _:_ ->
	    {skipped, "Benchmark machines only"}
    end.

end_per_suite(_Config) -> 
    [application:stop(App) || App <- [asn1, crypto, public_key, ssl, inets]].

init_per_group(Group, Config) when Group == http_dummy_keep_alive; 
				   Group == https_dummy_keep_alive;
				   Group == http_inets_keep_alive; 
				   Group == https_inets_keep_alive;
				   Group == http_nginx_keep_alive;
				   Group == https_nginx_keep_alive ->
    Version = http_version(Group),
    start_web_server(Group,
		     [{keep_alive, true}, 
		      {reuse_sessions, false},
		      {http_version, Version},
		      {http_opts,[{version, Version}]},
		      {http_headers, [{"connection", "keep-alive"}]},
		      {httpc_opts, [{keep_alive_timeout, 1500}, 
				    {max_keep_alive_length, ?config(iter, Config)}]}
		      | Config]);
init_per_group(Group, Config)  when Group == http_dummy; 
				    Group == https_dummy;
				    Group == http_inets; 
				    Group == https_inets;
				    Group == http_nginx;
				    Group == https_nginx ->
    Version = http_version(Group),
    start_web_server(Group, 
		     [{keep_alive, false}, 
		      {reuse_sessions, false},
		      {http_version, Version},
		      {http_headers, [{"connection", "close"}]},
		      {http_opts,[{version, Version}]},
		      {httpc_opts, [{keep_alive_timeout, 0}, {max_keep_alive_length, 0}]}
		      | Config]);


init_per_group(_, Config) ->
    Config.

end_per_group(Group, Config) ->
    stop_web_server(Group, Config).

init_per_testcase(TestCase, Config) when TestCase == httpc_small;
					 TestCase == httpc_big
					 -> 
    Opts = ?config(httpc_opts, Config),
    inets:start(httpc, [{profile, TestCase}, {socket_opts, [{nodelay, true}]}]),
    httpc:set_options(Opts, TestCase),
    [{profile, TestCase} | proplists:delete(profile, Config)];

init_per_testcase(_, Config) -> 
    Config.
end_per_testcase(TestCase, _Config) when TestCase == httpc_small;
					 TestCase == httpc_big ->	
    ok = inets:stop(httpc, TestCase);
end_per_testcase(_TestCase, Config) ->	
    Config.
%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

erl_dummy_small(Config) when is_list(Config) -> 
    {ok, Result} = run_test(httpd_lib_client, "1k_file", Config),
    notify(Result, Config, "erl_1k_file"). 

erl_dummy_big(Config)  when is_list(Config) -> 
    {ok, Result} = run_test(httpd_lib_client, "1M_file", Config),
    notify(Result, Config, "erl_1M_file"). 

wget_small(Config) when is_list(Config) -> 
    {ok, Result} = run_test(wget_client, "1k_file", Config),
    notify(Result, Config, "wget_1k_file"). 

wget_big(Config)  when is_list(Config) -> 
    {ok, Result} = run_test(wget_client, "1M_file", Config),
    notify(Result, Config, "wget_1M_file"). 

httpc_small(Config) when is_list(Config) -> 
    {ok, Result} = run_test(httpc_client, "1k_file", Config),
    notify(Result, Config, "httpc_1k_file"). 

httpc_big(Config)  when is_list(Config) -> 
    {ok, Result} = run_test(httpc_client, "1M_file", Config),
    notify(Result, Config, "httpc_1M_file"). 

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Report benchmark results  ------------------------------------------------
%%--------------------------------------------------------------------

notify({TestPerSec, _MBps}, Config, Suffix) ->
    Name = lists:concat([?config(protocol,Config), " ", 
			 server_name(Config, [dummy_pid, httpd_pid, nginx_port]), 
			 "", Suffix]),
    ct:comment("~p tps", [TestPerSec]),
    ct_event:notify(#event{name = benchmark_data,
			   data=[{value, TestPerSec},
				 {suite, ?MODULE},
				 {name, Name}]}),
			 ok.
%%--------------------------------------------------------------------
%% Setup erlang nodes  ------------------------------------------------
%%--------------------------------------------------------------------

server_name(Config, [Server | Rest]) ->
    case proplists:get_value(Server, Config) of
	undefined ->
	    server_name(Config, Rest);
	_ ->
	    server_name(Server)
    end.

server_name(httpd_pid) ->   
   "inets";
server_name(nginx_port) -> 
    "nginx";
server_name(dummy_pid) ->
    "erlang".

setup(_Config, nonode@nohost) ->
    exit(dist_not_enabled);
setup(_Config, _LocalNode) ->
    Host = case os:getenv(?remote_host) of
	       false ->
		   {ok, This} = inet:gethostname(),
		   This;
	       RemHost ->
		   RemHost
	   end,
    Node = list_to_atom("inets_perf_server@" ++ Host),
    SlaveArgs = case init:get_argument(pa) of
	       {ok, PaPaths} ->
		   lists:append([" -pa " ++ P || [P] <- PaPaths]);
	       _ -> []
	   end,
    Prog =
	case os:find_executable("erl") of
	    false -> "erl";
	    P -> P
	end,
    case net_adm:ping(Node) of
	pong -> ok;
	pang ->
	    {ok, Node} = slave:start(Host, inets_perf_server, SlaveArgs, no_link, Prog)
    end,
    Path = code:get_path(),
    true = rpc:call(Node, code, set_path, [Path]),
    [ensure_started(Node, App) || App <- [asn1, crypto, public_key, ssl, inets]],
    [ensure_started(node(), App) || App <- [asn1, crypto, public_key, ssl, inets]],
    (Node =:= node()) andalso restrict_schedulers(client),
    {Node, Host}.

ensure_started(Node, App) ->
     ok = rpc:call(Node, application, ensure_started, [App]).


restrict_schedulers(Type) ->
    %% We expect this to run on 8 core machine
    Extra0 = 1,
    Extra =  if (Type =:= server) -> -Extra0; true -> Extra0 end,
    Scheds = erlang:system_info(schedulers),
    erlang:system_flag(schedulers_online, (Scheds div 2) + Extra).

%%--------------------------------------------------------------------
%% Setup TLS input files  ------------------------------------------------
%%--------------------------------------------------------------------

init_ssl(Config) ->
    DDir = ?config(data_dir, Config),
    PDir = ?config(priv_dir, Config),
    {ok, _} = make_certs:all(DDir,
			     PDir).
cert_opts(Config) ->
    ClientCaCertFile = filename:join([?config(priv_dir, Config), 
				      "client", "cacerts.pem"]),
    ClientCertFile = filename:join([?config(priv_dir, Config), 
				    "client", "cert.pem"]),
    ServerCaCertFile = filename:join([?config(priv_dir, Config), 
				      "server", "cacerts.pem"]),
    ServerCertFile = filename:join([?config(priv_dir, Config), 
				    "server", "cert.pem"]),
    ServerKeyFile = filename:join([?config(priv_dir, Config), 
			     "server", "key.pem"]),
    ClientKeyFile = filename:join([?config(priv_dir, Config), 
				   "client", "key.pem"]),
    [{server_verification_opts, [{reuseaddr, true}, 
				 {cacertfile, ServerCaCertFile},
				 {ciphers, ["ECDHE-RSA-AES256-GCM-SHA384"]},
				 {certfile, ServerCertFile}, {keyfile, ServerKeyFile}]},
     {client_verification_opts, [
				 %%{verify, verify_peer},
				 {cacertfile, ClientCaCertFile}, 
				 {certfile, ClientCertFile},  
				 {keyfile, ClientKeyFile}]}]. 

%%--------------------------------------------------------------------
%% Run clients  ------------------------------------------------
%%--------------------------------------------------------------------

run_test(Client, File, Config) -> 
    Parent = self(),
    Pid = spawn(fun() ->
			receive
			    go -> 
				Parent ! {self(),
					  do_runs(Client, [{file, File} | Config])}
			end
		end),
    Pid ! go,
    receive
	{Pid,{{tps, Tps}, {mbps, MBps}}} ->
	    ct:pal("Tps: ~p  Bps~p", [Tps, MBps]),
	    {ok, {Tps, MBps}}
    end.

do_runs(Client, Config) ->
    N = ?config(iter, Config),
    DataDir = ?config(data_dir, Config),
    File = ?config(file, Config),
    Name = filename:join(DataDir, File), 
    Args = ?MODULE:Client(Config),
    ?MODULE:Client({init, Args}),
    Run = 
	fun() ->
		ok = ?MODULE:Client(Args, N)	
	end,
    {ok, Info} = file:read_file_info(Name, []),
    Length = Info#file_info.size,
    {TimeInMicro, _} = timer:tc(Run),
    ReqPerSecond = (1000000 * N) div TimeInMicro,
    BytesPerSecond = (1000000 * N * Length) div TimeInMicro,
    {{tps, ReqPerSecond}, {mbps, BytesPerSecond}}.


httpc_client({init, [_, Profile, URL, Headers, HTTPOpts]}) ->
     %% Make sure pipelining feature will kick in when appropriate. 
    {ok, {{_ ,200, "OK"}, _,_}} = httpc:request(get,{URL, Headers}, HTTPOpts, 
						[{body_format, binary}, 
						 {socket_opts, [{nodelay, true}]}], Profile),
    ct:sleep(1000);
httpc_client(Config) ->
    File = ?config(file, Config),
    Protocol = ?config(protocol, Config),
    Profile = ?config(profile, Config),
    URL = (?config(urlfun,Config))(File),
    Headers =  ?config(http_headers, Config),
    HTTPOpts = ?config(http_opts, Config),
    [Protocol, Profile, URL, Headers, HTTPOpts].
httpc_client(_,0) ->
    ok;
httpc_client([Protocol, Profile, URL, Headers, HTTPOpts], N) ->
    {ok, {{_ ,200,"OK"}, _,_}} = httpc:request(get,{URL, Headers}, HTTPOpts, [{body_format, binary},
									     {socket_opts, [{nodelay, true}]}], Profile),
    httpc_client([Protocol, Profile, URL, Headers, HTTPOpts], N-1).

httpd_lib_client({init, [_, Type, Version, Request, Host, Port, Opts]}) ->
    ok = httpd_test_lib:verify_request(Type, Host, 
     				       Port,  
     				       Opts, node(),
     				       Request,
     				       [{statuscode, 200},
     					{version, Version}], infinity),
    ct:sleep(1000);
httpd_lib_client(Config) ->
    File = ?config(file, Config),
    KeepAlive = ?config(keep_alive, Config),
    Host = ?config(server_host, Config),
    Port = ?config(port, Config),
    ReuseSession = ?config(reuse_sessions, Config),
    {Type, Opts} = 
	case ?config(protocol, Config) of
	    "http" ->
		{ip_comm, [{active, true}, {mode, binary},{nodelay, true}]};
	    "https" ->	
		SSLOpts =  proplists:get_value(client_verification_opts, cert_opts(Config)),
		{ssl, [{active, true}, {mode, binary}, {nodelay, true},  
		       {reuse_sessions, ReuseSession} | SSLOpts]}
		    
	end,
    Version = ?config(http_version, Config),
    Request = case KeepAlive of
		  true ->
		      http_request("GET /" ++ File ++ " ", Version, Host, {"connection:keep-alive\r\n", ""});
		  false ->
		      http_request("GET /" ++ File ++ " ", Version, Host)
	      end,
    
    Args = [KeepAlive, Type, Version, Request, Host, Port, Opts],
    httpd_lib_client(Args, 1),
    Args.

httpd_lib_client(_, 0) ->
    ok;
httpd_lib_client([true, Type, Version, Request, Host, Port, Opts], N) ->
    ok = httpd_test_lib:verify_request_N(Type, Host, 
					 Port,  
					 Opts, node(),
					 Request,
					 [{statuscode, 200},
					  {version, Version}], infinity, N);
httpd_lib_client([false, Type, Version, Request, Host, Port, Opts] = List, N) ->
    ok = httpd_test_lib:verify_request(Type, Host, 
				       Port,  
				       Opts, node(),
				       Request,
				       [{statuscode, 200},
					{version, Version}], infinity),
    httpd_lib_client(List, N-1).

wget_client({init,_}) ->
    ok;
wget_client(Config) ->
    File = ?config(file, Config),
    URL = (?config(urlfun,Config))(File),
    KeepAlive = ?config(keep_alive, Config),
    PrivDir = ?config(priv_dir, Config),
    Protocol = ?config(protocol, Config),
    Iter = ?config(iter, Config),
    FileName = filename:join(PrivDir, "wget_req"),
    ProtocolOpts = case Protocol of
		    "http" ->
			   [];
		       "https" ->
			   proplists:get_value(client_verification_opts, cert_opts(Config))
		   end,
    wget_req_file(FileName,URL,Iter),
    [KeepAlive, FileName, URL, Protocol, ProtocolOpts, Iter].
wget_client([KeepAlive, WgetFile, _URL, Protocol, ProtocolOpts, _], _) ->
    process_flag(trap_exit, true),
    Cmd = wget_N(KeepAlive, WgetFile, Protocol, ProtocolOpts),
    %%ct:pal("Wget cmd: ~p", [Cmd]),
    Port = open_port({spawn, Cmd}, [stderr_to_stdout]), 
    wait_for_wget(Port).


%%--------------------------------------------------------------------
%% Start/stop servers  ------------------------------------------------
%%--------------------------------------------------------------------
start_web_server(Group, Config) when Group == http_dummy;
				     Group == http_dummy_keep_alive ->
    start_dummy("http", Config);

start_web_server(Group, Config) when Group == https_dummy;
				     Group == https_dummy_keep_alive ->
    start_dummy("https", Config);

start_web_server(Group, Config) when Group == http_inets;
				     Group == http_inets_keep_alive ->
    start_inets("http", [], Config);
    
start_web_server(Group, Config) when Group == https_inets;
				     Group == https_inets_keep_alive ->
    Opts = proplists:get_value(server_verification_opts, cert_opts(Config)),
    ReuseSessions = ?config(reuse_sessions, Config),
    SSLConfHttpd = [{socket_type, {essl,
				   [{nodelay, true}, {reuse_sessions, ReuseSessions} | Opts]}}],
    start_inets("https", SSLConfHttpd, Config);

start_web_server(Group, Config)  when Group == http_nginx;
				      Group == http_nginx_keep_alive ->
    case os:find_executable("nginx") of
	false ->
	    {skip, "nginx not found"};
	_ ->
	    start_nginx("http",  Config)
    end;

start_web_server(Group, Config)  when Group == https_nginx;
				      Group == https_nginx_keep_alive ->
     case os:find_executable("nginx") of
	false ->
	    {skip, "nginx not found"};
	 _ ->
	     start_nginx("https",  cert_opts(Config) ++ Config)  
     end.
   
start_inets(Protocol, ConfHttpd, Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    Node = ?config(server_node, Config),
    Host = ?config(server_host, Config),    
    HTTPVersion = ?config(http_version, Config),    
    Conf = [httpd, [{port,0},
		    {http_version, HTTPVersion},
		    {ipfamily, inet},
		    {server_name, "inets_test"},
		    {server_root, PrivDir}, 
		    {document_root, DataDir},
		    {keep_alive, ?config(keep_alive, Config)},
		    {keep_alive_timeout, 360}
		    | ConfHttpd]],
    {ok, Pid} = rpc:call(Node, inets, start, Conf),
    Port = proplists:get_value(port,  rpc:call(Node, httpd, info, [Pid])),
    F = fun(File) -> 
		lists:concat([Protocol,"://",Host,":",Port,"/",File]) 
	end,
    [{httpd_pid,Pid},{urlfun,F},{protocol,Protocol},{port,Port} | Config].

start_dummy("http"= Protocol, Config) ->
    HTTPVersion = ?config(http_version, Config),    
    Node = ?config(server_node, Config),
    %%DataDir= ?config(data_dir, Config),
    Host = ?config(server_host, Config),
    Conf = [
	    %%{big, filename:join(DataDir, "1M_file")},
	    %%{small, filename:join(DataDir, "1k_file")},
	    {big, {gen,  crypto:strong_rand_bytes(1000000)}},
	    {small, {gen,  crypto:strong_rand_bytes(1000)}},
	    {http_version, HTTPVersion},
	    {keep_alive,  ?config(keep_alive, Config)}
	   ],
    {Pid, Port} = rpc:call(Node, http_test_lib, dummy_server, [ip_comm, inet, [{content_cb, ?MODULE}, {conf, Conf}]]),
    F = fun(File) -> 
		lists:concat([Protocol,"://",Host,":",Port,"/",File]) 
	end,
    [{dummy_pid,Pid},{urlfun,F},{protocol, Protocol},{port,Port} | Config];

start_dummy("https" = Protocol, Config) ->
    HTTPVersion = ?config(http_version, Config),    
    Node = ?config(server_node, Config),
    %% DataDir= ?config(data_dir, Config),
    Host = ?config(server_host, Config),
    SSLOpts =  proplists:get_value(server_verification_opts, cert_opts(Config)),
    Opts = [{active, true}, {nodelay, true}, {reuseaddr, true} | SSLOpts],
    Conf = [%%{big, filename:join(DataDir, "1M_file")},
	    %%{small, filename:join(DataDir, "1k_file")},
	    {big, {gen, crypto:strong_rand_bytes(1000000)}},
	    {small, {gen, crypto:strong_rand_bytes(1000)}},
	    {http_version, HTTPVersion},
	    {keep_alive, ?config(keep_alive, Config)}
	   ],
    {Pid, Port} = rpc:call(Node, http_test_lib, dummy_server,
			   [ssl, inet, [{ssl, Opts}, {content_cb, ?MODULE}, {conf, Conf}]]),
    F = fun(File) -> 
		lists:concat([Protocol,"://",Host,":",Port,"/",File]) 
	end,
    [{dummy_pid,Pid},{urlfun,F},{protocol,Protocol},{port,Port} | Config].

start_nginx(Protocol, Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    Host = ?config(server_host, Config),    
    Port = inet_port(node()),
    
    ConfFile = filename:join(PrivDir, "nginx.conf"),
    nginx_conf(ConfFile, [{port, Port}, {protocol, Protocol} | Config]),
    Cmd = "nginx -c " ++ ConfFile, 
    NginxPort =  open_port({spawn, Cmd}, [{cd, DataDir}, stderr_to_stdout]), 

    F = fun(File) -> 
 		lists:concat([Protocol,"://",Host,":",Port,"/",File]) 
	end,
    
    wait_for_nginx_up(Host, Port),
   
    [{port, Port},{nginx_port, NginxPort},{urlfun,F},{protocol, Protocol} | Config ].

stop_nginx(Config)->
    PrivDir = ?config(priv_dir, Config),    
    {ok, Bin} = file:read_file(filename:join(PrivDir, "nginx.pid")),
    Pid = string:strip(binary_to_list(Bin), right, $\n),
    Cmd = "kill " ++ Pid,
    os:cmd(Cmd).
    
stop_web_server(Group, Config) when  Group == http_inets;
				     Group == http_inets_keep_alive;
				     Group == https_inets;
				     Group == https_inets_keep_alive -> 
    ServerNode = ?config(server_node, Config),
    rpc:call(ServerNode, inets, stop, [httpd, ?config(httpd_pid, Config)]);
stop_web_server(Group, Config) when  Group == http_dummy;
				     Group == http_dummy_keep_alive;
				     Group == https_dummy;
				     Group == https_dummy_keep_alive -> 
    stop_dummy_server(Config);
stop_web_server(Group, Config) when  Group == http_nginx;
				     Group == http_nginx_keep_alive;
				     Group == https_nginx;
				     Group == https_nginx_keep_alive -> 
    stop_nginx(Config).

stop_dummy_server(Config) ->    
      case ?config(dummy_pid, Config) of
	  Pid when is_pid(Pid) ->
	      exit(Pid, kill);
	  _ ->
	      ok
      end.

%%--------------------------------------------------------------------
%% Misc  ------------------------------------------------
%%--------------------------------------------------------------------
http_request(Request, "HTTP/1.1" = Version, Host, {Headers, Body}) ->
    Request ++ Version ++ "\r\nhost:" ++ Host ++ "\r\n" ++ Headers ++ "\r\n" ++ Body;
http_request(Request, Version, _, {Headers, Body}) ->
    Request ++ Version ++ "\r\n" ++ Headers  ++ "\r\n" ++ Body.

http_request(Request, "HTTP/1.1" = Version, Host) ->
    Request ++ Version ++ "\r\nhost:" ++ Host  ++ "\r\n\r\n";
http_request(Request, Version, _) ->
    Request ++ Version ++ "\r\n\r\n".

http_version(_) ->
    "HTTP/1.1".

inet_port(Node) ->
    {Port, Socket} = do_inet_port(Node),
     rpc:call(Node, gen_tcp, close, [Socket]),
     Port.

do_inet_port(Node) ->
    {ok, Socket} = rpc:call(Node, gen_tcp, listen, [0, [{reuseaddr, true}]]),
    {ok, Port} = rpc:call(Node, inet, port, [Socket]),
    {Port, Socket}.
 
%%--------------------------------------------------------------------
%% Dummy server callbacks  ------------------------------------------------
%%--------------------------------------------------------------------

handle_request(CB, S, "/1M_file" ++ _, Opts) ->
    Name = proplists:get_value(big, Opts),
    KeepAlive = proplists:get_value(keep_alive, Opts),
    do_handle_request(CB, S, Name, Opts, KeepAlive);
handle_request(CB, S, "/1k_file" ++ _, Opts) ->
    Name = proplists:get_value(small, Opts),
    KeepAlive = proplists:get_value(keep_alive, Opts),
    do_handle_request(CB, S, Name, Opts, KeepAlive).

do_handle_request(CB, S, Name, Opts, KeepAlive) when is_list(Name) ->
    Version = proplists:get_value(http_version, Opts),
    {ok, Fdesc} = file:open(Name, [read, binary]),
    {ok, Info} = file:read_file_info(Name, []),
    Length = Info#file_info.size,
    Response = response_status_line_and_headers(Version, "Content-Length:" 
						++ integer_to_list(Length) ++ ?CRLF, keep_alive(KeepAlive)), 
    CB:send(S, Response),
    send_file(CB, S, Fdesc);
do_handle_request(CB, S, {gen, Data}, Opts, KeepAlive) ->
    Version = proplists:get_value(http_version, Opts),
    Length = size(Data),
    Response = response_status_line_and_headers(Version, "Content-Length:" 
						++ integer_to_list(Length) ++ ?CRLF, keep_alive(KeepAlive)), 
    CB:send(S, Response),
    send_file(CB, S, {gen, Data}).
    
send_file(CB, S, {gen, Data})  ->
    CB:send(S, Data);
    %% ChunkSize = 64*1024,
    %% case size(Data) of
    %% 	N when N > ChunkSize ->
    %% 	    <<Chunk:N/binary, Rest/binary>> = Data,
    %% 	    %%{Chunk, Rest} = lists:split(N, Data),
    %% 	    CB:send(S, Chunk),
    %% 	    send_file(CB, S, {gen, Rest});
    %% 	_ ->
    %% 	    CB:send(S, Data)
    %% end; 

send_file(CB, S, FileDesc) ->
    case file:read(FileDesc, 64*1024) of
	{ok, Chunk} ->
	    CB:send(S, Chunk),
	    send_file(CB, S, FileDesc);	
	eof ->
	    file:close(FileDesc),
	    ok
    end.

response_status_line_and_headers(Version, Headers,  ConnectionHeader) -> 
    StatusLine = [Version, " ", "200 OK", ?CRLF],
    [StatusLine, Headers, ConnectionHeader, ?CRLF].

keep_alive(true)->
    "Connection:keep-alive\r\n";
keep_alive(false) ->
    "Connection:close\r\n".

handle_http_msg({_Method, RelUri, _, {_, _Headers}, _Body}, Socket, Conf) ->
    handle_request(connect_cb(Socket), Socket, RelUri, Conf),
    case proplists:get_value(keep_alive, Conf) of
	true ->
	    <<>>;
	false ->
	    stop
    end.
    
connect_cb({sslsocket, _, _}) ->
    ssl;
connect_cb(_) ->
    gen_tcp.

%%--------------------------------------------------------------------
%% Setup wget  ------------------------------------------------
%%--------------------------------------------------------------------
wget_req_file(FileName, Url, Iter) ->
    {ok, File} = file:open(FileName, [write]),
    write_urls(File, Url, Iter).

write_urls(File, Url, 1) ->
    file:write(File, Url), 
    file:close(File);
write_urls(File, Url, N) ->
    file:write(File, Url), 
    file:write(File, "\n"),
    write_urls(File, Url, N-1).
    
wait_for_wget(Port) ->
    receive 
	{Port, {data, _Data}} when is_port(Port) ->
	    wait_for_wget(Port);
	{Port, closed} -> 
	    ok;
	{'EXIT', Port, _Reason} ->
	    ok
    end.

wget_N(KeepAlive, WegetFile, "http", _ProtocolOpts) ->
    "wget -i " ++ WegetFile ++ " " ++ wget_keep_alive(KeepAlive) ++ 
	" --no-cache --timeout=120" ;
wget_N(KeepAlive, WegetFile, "https", ProtocolOpts) ->
    
    "wget -i " ++ WegetFile ++ " " ++ wget_keep_alive(KeepAlive) 
	++ wget_cert(ProtocolOpts) ++ wget_key(ProtocolOpts)
	++ wget_cacert(ProtocolOpts) ++ 
	" --no-cache --timeout=120".

wget(KeepAlive, URL, "http", _ProtocolOpts) ->
    "wget " ++ URL ++ " " ++ wget_keep_alive(KeepAlive) ++ 
	" --no-cache --timeout=120" ;
wget(KeepAlive, URL, "https", ProtocolOpts) ->
    
    "wget " ++ URL ++ " " ++ wget_keep_alive(KeepAlive) 
	++ wget_cert(ProtocolOpts) ++ wget_key(ProtocolOpts)
	++ wget_cacert(ProtocolOpts) ++ 
	" --no-cache --timeout=120".

wget_keep_alive(true)->
    "";
wget_keep_alive(false) ->
   "--no-http-keep-alive ".

wget_cacert(ProtocolOpts) ->
    "--ca-certificate=" ++ proplists:get_value(cacertfile, ProtocolOpts) ++ " ".

wget_cert(ProtocolOpts) ->
    "--certificate=" ++ proplists:get_value(certfile, ProtocolOpts) ++ " ".

wget_key(ProtocolOpts) ->
    "--private-key=" ++ proplists:get_value(keyfile, ProtocolOpts) ++ " ".

%%--------------------------------------------------------------------
%% Setup nginx  ------------------------------------------------
%%--------------------------------------------------------------------
nginx_conf(ConfFile, Config)->
    Protocol = ?config(protocol, Config),
    file:write_file(ConfFile,
		    [format_nginx_conf(nginx_global(Config)),
		     format_nginx_conf(nginx_events(Config)),
		     format_nginx_conf(nginx_http(Protocol, Config))]).
       
format_nginx_conf(Directives) ->
    lists:map(fun({Key, Value}) ->
			  io_lib:format("~s ~s;\n", [Key, Value]);
		     (Str) ->
			  Str    
		  end, Directives).


nginx_global(Config) ->
    PrivDir = ?config(priv_dir, Config),
    [{"pid", filename:join(PrivDir, "nginx.pid")},
     {"error_log",  filename:join(PrivDir, "nginx.pid")},
     {"worker_processes", "1"}].

nginx_events(_Config) ->
    ["events {\n",
     {"worker_connections",  "1024"},
     "\n}"
    ].

nginx_http("http", Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    Port = ?config(port, Config),
    ["http {\n" |
     nginx_defaults(PrivDir) ++
	 [" server {",
	  {root,                DataDir}, 
	  {listen,              integer_to_list(Port)},
	  " location / {\n  try_files $uri $uri/ /index.html;\n}"
	  "}\n", "}\n"
	 ]
    ];
	
nginx_http("https", Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    Port = ?config(port, Config),
    SSLOpts = ?config(server_verification_opts, Config),
    Ciphers = proplists:get_value(ciphers, SSLOpts),
    ReuseSession = ?config(reuse_sessions, Config),
    ["http {" |
     nginx_defaults(PrivDir) ++
	 [" server {",
	  {"root",                DataDir}, 
	  {"listen",              integer_to_list(Port) ++ " ssl"},
	  {"ssl_certificate",     ?config(certfile, SSLOpts)},
	  {"ssl_certificate_key", ?config(keyfile, SSLOpts)},
	  {"ssl_protocols",       "TLSv1 TLSv1.1 TLSv1.2"},
	  {"ssl_ciphers",         Ciphers}, 
	  {"ssl_session_cache",    nginx_reuse_session(ReuseSession)},
	  " location / {\n  try_files $uri $uri/ /index.html;\n}"
	  "}\n", "}\n"
	 ]
    ].
	
nginx_defaults(PrivDir) ->
    [
     %% Set temp and cache file options that will otherwise default to
     %% restricted locations accessible only to root.
     {"client_body_temp_path", filename:join(PrivDir, "client_body")},
     {"fastcgi_temp_path",   filename:join(PrivDir, "fastcgi_temp")}, 
     {"proxy_temp_path", filename:join(PrivDir, "proxy_temp")},
     {"scgi_temp_path", filename:join(PrivDir, "scgi_temp")},
     {"uwsgi_temp_path", filename:join(PrivDir, "uwsgi_temp_path")},
     {"access_log",  filename:join(PrivDir, "access.log")},
     {"error_log",   filename:join(PrivDir, "error.log")},
     %% Standard options
     {"sendfile", "on"},
     {"tcp_nopush", "on"},
     {"tcp_nodelay", "on"},
     {"keepalive_timeout",  "360"},
     {"types_hash_max_size", "2048"},
     {"include", "/etc/nginx/mime.types"},
     {"default_type", "application/octet-stream"}
    ].

nginx_reuse_session(true) ->
    "on";
nginx_reuse_session(false) ->
    "off".

wait_for_nginx_up(Host, Port) ->
    case gen_tcp:connect(Host, Port, []) of
	{ok, Socket} ->
	    gen_tcp:close(Socket);
	_  ->
	    ct:sleep(100),
	    wait_for_nginx_up(Host, Port)
    end.
		
