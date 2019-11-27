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
%% Purpose: The httpd load test (hdlt) controller/collector module, 
%%          This module contains all the code of the httpd load test 
%%          controller/collector. It sets up the test, starts all
%%          server and client nodes and applications and finally 
%%          collects test data.
%%----------------------------------------------------------------------

-module(hdlt_ctrl).

-export([start/1, stop/0, help/0]).

-export([init/1, proxy/7]).

-include_lib("kernel/include/file.hrl").
-include("hdlt_logger.hrl").

-define(DEFAULT_SENDRATE,           89).
-define(DEFAULT_TEST_TIME,          120). % 2 minutes
-define(DEFAULT_PORT,               8889).
-define(TIMEOUT,                    60000).
-define(DEFAULT_MAX_NOF_SCHEDULERS, 8).
-define(DEFAULT_SERVER_DIR,         "/tmp/hdlt").
-define(DEFAULT_WORK_DIR,           "/tmp/hdlt").
-define(SSH_PORT,                   22).
-define(DEFAULT_SOCKET_TYPE,        ip_comm).
-define(DEFAULT_SERVER_CERT,        "hdlt_ssl_server_cert.pem").
-define(DEFAULT_CLIENT_CERT,        "hdlt_ssl_client_cert.pem").
-define(SSH_CONNECT_TIMEOUT,        5000).
-define(NODE_START_TIMEOUT,         5000).
-define(LOCAL_PROXY_START_TIMEOUT,  ?NODE_START_TIMEOUT * 4).
-define(DEFAULT_DEBUGS, 
	[{ctrl, info}, {slave, silence}, {proxy, silence}, {client, silence}]).
-define(DEFAULT_WORK_SIM,           10000).
-define(DEFAULT_DATA_SIZE_START,    500).
-define(DEFAULT_DATA_SIZE_END,      1500).
-define(DEFAULT_DATA_SIZE_INCR,     1).
-define(DEFAULT_DATA_SIZE,          {?DEFAULT_DATA_SIZE_START,
				     ?DEFAULT_DATA_SIZE_END,
				     ?DEFAULT_DATA_SIZE_INCR}).


%% hdlt = httpd load test

-define(COLLECTOR,   hdlt_ctrl).
-define(RESULTS_TAB, hdlt_results).

-define(CLIENT_MOD,       hdlt_client).
-define(CLIENT_NODE_NAME, ?CLIENT_MOD).

-define(SERVER_MOD,       hdlt_server).
-define(SERVER_NODE_NAME, ?SERVER_MOD).

-define(LOGGER, hdlt_logger).


-record(state, 
	{
	 url,
	 test_time,
	 send_rate,
	 http_server, 
	 http_port,
	 results = ?RESULTS_TAB, 
	 nodes,
	 server_root,
	 doc_root,
	 server_dir,
	 work_dir,
	 server_conn,
	 client_conns = [],
	 client_mod   = ?CLIENT_MOD, 
	 clients,
	 nof_schedulers = 0,
	 max_nof_schedulers,
	 socket_type,
	 server_cert_file,
	 client_cert_file,
	 debugs,
	 client_sz_from, 
	 client_sz_to, 
	 client_sz_incr 
	}
       ).

-record(proxy, 
	{
	 mode,
	 mod, 
	 connection, 
	 channel, 
	 host, 
	 cmd, 
	 node_name, 
	 node,
	 ref,
	 erl_path, 
	 paths, 
	 args
	}).

-record(connection, 
	{
	 proxy,
	 node, 
	 node_name,
	 host
	}).


-record(client, {host, path, version}).
-record(server, {host, path, version}).


start(Config) when is_list(Config) ->
    proc_lib:start_link(?MODULE, init, [Config]).

stop() ->
    global:send(?COLLECTOR, stop).

init(Config) ->
    %% io:format("Config: ~n~p~n", [Config]),
    case (catch do_init(Config)) of
	{ok, State} ->
	    proc_lib:init_ack({ok, self()}),
	    loop(State);
	{error, _Reason} = Error ->
	    proc_lib:init_ack(Error),
	    ok;
	{'EXIT', Reason} ->
	    proc_lib:init_ack({error, Reason}),
	    ok
    end.

do_init(Config) ->
    %% Do not trap exit, but register ourself
    global:register_name(?COLLECTOR, self()), 

    State = #state{},
    ets:new(State#state.results, [bag, named_table]),
    
    hdlt_logger:start(),
    global:sync(),
    
    %% Maybe enable debug
    Debugs = get_debugs(Config),
    ?SET_NAME("HDLT CTRL"),
    set_debug_level(Debugs), 

    ?DEBUG("network info: "
	   "~n   Global names: ~p"
	   "~n   Nodes:        ~p", [global:registered_names(), nodes()]),

    %% Read config
    ?LOG("read config", []),
    SendRate         = get_send_rate(Config),
    Clients          = get_clients(Config), 
    TestTime         = get_test_time(Config), 
    Server           = get_server(Config),
    Port             = get_port(Config),
    ServerDir        = get_server_dir(Config),
    WorkingDir       = get_work_dir(Config),
    MaxNofSchedulers = get_max_nof_schedulers(Config),
    SocketType       = get_socket_type(Config),
    ServerCertFile   = get_server_cert_file(Config),
    ClientCertFile   = get_client_cert_file(Config),
    WorkSim          = get_work_sim(Config),
    {From, To, Incr} = get_data_size(Config),

    URL        = url(Server, Port, SocketType, WorkSim), 
    ServerRoot = filename:join(ServerDir, "server_root"),  
    DocRoot    = ServerRoot, %% Not really used in this test

    %% Start used applications
    ?DEBUG("ensure crypto started", []), 
    crypto:start(),
    ?DEBUG("ensure ssh started", []), 
    ssh:start(),

    State2 = State#state{server_root        = ServerRoot, 
			 doc_root           = DocRoot,
			 server_dir         = ServerDir, 
			 work_dir           = WorkingDir,
			 max_nof_schedulers = MaxNofSchedulers, 
			 socket_type        = SocketType,
			 server_cert_file   = ServerCertFile,
			 client_cert_file   = ClientCertFile,
			 http_server        = Server, 
			 http_port          = Port, 
			 url                = URL,
			 test_time          = TestTime, 
			 send_rate          = SendRate,
			 clients            = Clients,
			 debugs             = Debugs, 
			 client_sz_from     = From, 
			 client_sz_to       = To, 
			 client_sz_incr     = Incr},
    
    ?LOG("prepare server host", []),
    prepare_server_host(State2),

    ?LOG("prepare client hosts", []),
    State3 = prepare_client_hosts(State2),

    ?LOG("basic init done", []),
    {ok, State3}.


loop(#state{nof_schedulers = N, max_nof_schedulers = M} = State) when N > M -> 
    
    ?INFO("Starting to analyse data", []),
    
    AnalysedTab = analyse_data(State),
    
    Files = save_results_to_file(AnalysedTab, State),
    io:format("~n******************************************************"
	      "~n~nResult(s) saved to: ~n~p~n", [Files]),
    clean_up(State);

loop(#state{url            = URL, 
	    test_time      = TestTime, 
	    send_rate      = SendRate,
	    nof_schedulers = NofSchedulers} = State) ->
    
    {StartH, StartM, StartS} = erlang:time(),
	
    ?INFO("Performing test with ~p smp-scheduler(s): ~n"
	  "  It will take a minimum of: ~p seconds. ~n"
	  "  Start time:                ~.2.0w:~.2.0w:~.2.0w", 
	  [NofSchedulers, round(TestTime/1000), StartH, StartM, StartS]),

    %% Start the server node 
    %% (The local proxy, the node, the remote proxy, and the inets framework)
    State1 = start_server_node(State),
    ?DEBUG("nodes after server start: ~p", [nodes() -- [node()]]),
    
    %% Start the client node(s)
    %% (The local proxy, the node, the remote proxy, and the inets framework)
    ?LOG("start client node(s)", []),
    State2 = start_client_nodes(State1),
    ?DEBUG("nodes after client(s) start: ~p", [nodes() -- [node()]]),

    ?LOG("start server", []),
    start_server(State2), 

    ?LOG("start clients", []),
    start_clients(State2, URL, TestTime, SendRate),

    ?LOG("release clients", []),
    release_clients(State2),

    ?LOG("collect data", []),
    collect_data(State2),

    ?LOG("stop all nodes", []),
    State3 = stop_nodes(State2),

    ?INFO("Test with ~p smp-scheduler(s) complete"
	  "~n~n"
	  "****************************************************************"
	  "~n", 
	  [NofSchedulers]),
    loop(State3#state{nof_schedulers = NofSchedulers + 1}).
    

prepare_server_host(#state{server_root      = ServerRoot, 
			   http_server      = #server{host = Host},
			   socket_type      = SocketType,
			   server_cert_file = CertFile}) ->
    ?INFO("prepare server host ~s", [Host]),
    Opts = [{user_interaction,      false},
	    {silently_accept_hosts, true},
	    {timeout,               2*?SSH_CONNECT_TIMEOUT}, 
	    {connect_timeout,       ?SSH_CONNECT_TIMEOUT}], 
    case ssh_sftp:start_channel(Host, Opts) of
	{ok, Sftp, ConnectionRef} ->
	    ?DEBUG("sftp connection established - now transer server content",
		   []),
	    create_server_content(Sftp, ServerRoot, SocketType, CertFile),
	    ?DEBUG("server content transfered - now close ssh connection ", 
		   []),
	    ssh:close(ConnectionRef),
	    ?DEBUG("server preparation complete ", []),
	    ok;
	Error ->
	    ?INFO("FAILED creating sftp channel to server host ~s: "
		  "~n   ~p", [Host, Error]),
	    exit({failed_establishing_sftp_connection, Error})
    end.

create_server_content(Sftp, ServerRoot, SocketType, CertFile) -> 
    %% Create server root
    ?DEBUG("ensure existence of ~p", [ServerRoot]),
    ensure_remote_dir_exist(Sftp, ServerRoot),

    %% Create the server ebin dir (for the starter module)
    EBIN = filename:join(ServerRoot, "ebin"), 
    ?DEBUG("make ebin dir: ~p", [EBIN]),
    maybe_create_remote_dir(Sftp, EBIN),

    %% Create the server ebin dir (for the starter module)
    LOG = filename:join(ServerRoot, "log"), 
    ?DEBUG("make log dir: ~p", [LOG]),
    maybe_create_remote_dir(Sftp, LOG),

    LocalServerMod = local_server_module(), 
    ?DEBUG("copy server stub/proxy module ~s", [LocalServerMod]), 
    RemoteServerMod = remote_server_module(EBIN), 
    {ok, ServerModBin} = file:read_file(LocalServerMod),
    ok = ssh_sftp:write_file(Sftp, RemoteServerMod, ServerModBin),

    LocalSlaveMod = local_slave_module(), 
    ?DEBUG("copy slave module ~s", [LocalSlaveMod]), 
    RemoteSlaveMod = remote_slave_module(EBIN), 
    {ok, SlaveModBin} = file:read_file(LocalSlaveMod),
    ok = ssh_sftp:write_file(Sftp, RemoteSlaveMod, SlaveModBin),

    LocalLoggerMod = local_logger_module(), 
    ?DEBUG("copy logger module ~s", [LocalLoggerMod]), 
    RemoteLoggerMod = remote_logger_module(EBIN), 
    {ok, LoggerModBin} = file:read_file(LocalLoggerMod),
    ok = ssh_sftp:write_file(Sftp, RemoteLoggerMod, LoggerModBin),

    %% Create the inets server data dir
    CGI = filename:join(ServerRoot, "cgi-bin"), 
    ?DEBUG("make cgi dir: ~p", [CGI]),
    maybe_create_remote_dir(Sftp, CGI),
    
    LocalRandomMod = local_random_html_module(), 
    ?DEBUG("copy random-html module ~s", [LocalRandomMod]), 
    RemoteRandomMod = remote_random_html_module(EBIN), 
    {ok, RandomModBin} = file:read_file(LocalRandomMod),
    ok = ssh_sftp:write_file(Sftp, RemoteRandomMod, RandomModBin),

    case SocketType of
	ip_comm ->
	    ok;
	_ ->
	    SSLDir = filename:join(ServerRoot, "ssl"), 
	    ?DEBUG("make conf dir: ~p", [SSLDir]),
	    maybe_create_remote_dir(Sftp, SSLDir),
	    ?DEBUG("copy ssl cert file ~s", [CertFile]),
	    {ok, CertBin} = file:read_file(CertFile),
	    RemoteCertFile = filename:join(SSLDir, 
					   filename:basename(CertFile)),
	    ok = ssh_sftp:write_file(Sftp, RemoteCertFile, CertBin),
	    ok
    end,

    ?DEBUG("done", []),
    ok.

remote_server_module(Path) ->
    Mod = server_module(), 
    filename:join(Path, Mod).

local_server_module() ->
    Mod = server_module(), 
    case code:where_is_file(Mod) of
	Path when is_list(Path) ->
	    Path;
	_ ->
	    exit({server_module_not_found, Mod})
    end.

server_module() ->
    module(?SERVER_MOD).


prepare_client_hosts(#state{work_dir         = WorkDir, 
			    clients          = Clients,
			    socket_type      = SocketType,
			    client_cert_file = CertFile} = State) ->
    Clients2 = 
	prepare_client_hosts(WorkDir, SocketType, CertFile, Clients, []),
    State#state{clients = Clients2}.

prepare_client_hosts(_WorkDir, _SocketType, _CertFile, [], Acc) ->
    lists:reverse(Acc);
prepare_client_hosts(WorkDir, SocketType, CertFile, [Client|Clients], Acc) ->
    case prepare_client_host(WorkDir, SocketType, CertFile, Client) of
	ok ->
	    prepare_client_hosts(WorkDir, SocketType, CertFile, Clients, 
				 [Client|Acc]);
	_ ->
	    prepare_client_hosts(WorkDir, SocketType, CertFile, Clients, Acc)
    end.

prepare_client_host(WorkDir, SocketType, CertFile, #client{host = Host}) ->
    ?INFO("prepare client host ~s", [Host]),
    Opts = [{user_interaction,      false},
	    {silently_accept_hosts, true},
	    {timeout,               2*?SSH_CONNECT_TIMEOUT}, 
	    {connect_timeout,       ?SSH_CONNECT_TIMEOUT}], 
    case ssh_sftp:start_channel(Host, Opts) of
	{ok, Sftp, ConnectionRef} ->
	    ?DEBUG("sftp connection established - now transer client content",
		   []),
	    create_client_content(Sftp, WorkDir, SocketType, CertFile),
	    ?DEBUG("client content transered - now close ssh connection ", []),
	    ssh:close(ConnectionRef),
	    ?DEBUG("client preparation complete ", []),
	    ok;
	Error ->
	    ?INFO("FAILED creating sftp channel to client host ~s: skipping"
		  "~n   ~p", [Host, Error]),
	    Error
    end.

create_client_content(Sftp, WorkDir, SocketType, CertFile) -> 
    %% Create work dir
    ?DEBUG("ensure existence of ~p", [WorkDir]),
    ensure_remote_dir_exist(Sftp, WorkDir),

    %% Create the client ebin dir
    EBIN = filename:join(WorkDir, "ebin"), 
    RemoteClientMod = remote_client_module(EBIN), 
    ?DEBUG("make ebin dir: ~p", [EBIN]),
    maybe_create_remote_dir(Sftp, EBIN),

    LocalClientMod = local_client_module(), 
    ?DEBUG("copy client stub/proxy module ~s", [LocalClientMod]), 
    {ok, ClientModBin} = file:read_file(LocalClientMod),
    ok = ssh_sftp:write_file(Sftp, RemoteClientMod, ClientModBin),

    LocalSlaveMod = local_slave_module(), 
    ?DEBUG("copy slave module ~s", [LocalSlaveMod]), 
    RemoteSlaveMod = remote_slave_module(EBIN), 
    {ok, SlaveModBin} = file:read_file(LocalSlaveMod),
    ok = ssh_sftp:write_file(Sftp, RemoteSlaveMod, SlaveModBin),

    LocalLoggerMod = local_logger_module(), 
    ?DEBUG("copy logger module ~s", [LocalLoggerMod]), 
    RemoteLoggerMod = remote_logger_module(EBIN), 
    {ok, LoggerModBin} = file:read_file(LocalLoggerMod),
    ok = ssh_sftp:write_file(Sftp, RemoteLoggerMod, LoggerModBin),

    case SocketType of
	ip_comm ->
	    ok;
	_ ->
	    %% We should really store the remote path somewhere as
	    %% we use it when starting the client service...
	    SSLDir = filename:join(WorkDir, "ssl"), 
	    ?DEBUG("make ssl dir: ~p", [SSLDir]),
	    maybe_create_remote_dir(Sftp, SSLDir),
	    ?DEBUG("copy ssl cert file ~s", [CertFile]),
	    {ok, CertBin} = file:read_file(CertFile),
	    RemoteCertFile = filename:join(SSLDir, 
					   filename:basename(CertFile)),
	    ok = ssh_sftp:write_file(Sftp, RemoteCertFile, CertBin),
	    ok
    end,

    ?DEBUG("done", []),
    ok.

remote_client_module(Path) ->
    Mod = client_module(), 
    filename:join(Path, Mod).

local_client_module() ->
    Mod = client_module(), 
    case code:where_is_file(Mod) of
	Path when is_list(Path) ->
	    Path;
	_ ->
	    exit({client_module_not_found, Mod})
    end.

client_module() ->
    module(?CLIENT_MOD).


remote_slave_module(Path) ->
    Mod = slave_module(), 
    filename:join(Path, Mod).

local_slave_module() ->
    Mod = slave_module(), 
    case code:where_is_file(Mod) of
	Path when is_list(Path) ->
	    Path;
	_ ->
	    exit({slave_module_not_found, Mod})
    end.

slave_module() ->
    module(hdlt_slave).


remote_logger_module(Path) ->
    Mod = logger_module(), 
    filename:join(Path, Mod).

local_logger_module() ->
    Mod = logger_module(), 
    case code:where_is_file(Mod) of
	Path when is_list(Path) ->
	    Path;
	_ ->
	    exit({logger_module_not_found, Mod})
    end.

logger_module() ->
    module(hdlt_logger).


remote_random_html_module(Path) ->
    Mod = random_html_module(), 
    filename:join(Path, Mod).

local_random_html_module() ->
    Mod = random_html_module(), 
    case code:where_is_file(Mod) of
	Path when is_list(Path) ->
	    Path;
	_ ->
	    exit({random_module_not_found, Mod})
    end.

random_html_module() ->
    module(hdlt_random_html).


module(Mod) ->
    Ext = string:to_lower(erlang:system_info(machine)), 
    lists:flatten(io_lib:format("~w.~s", [Mod, Ext])).


%% -----------------------------------------------------------------------
%% - For every node created (server and client both) there is both 
%%   a local and remote proxy. 
%% - The local proxy is running on the local (controller/collector) node.
%% - The remote proxy is running on the client or server node(s).
%% - The local (ctrl) proxy monitor the remote (server/client) proxy.
%% - The remote (server/client) proxy monitor the local (ctrl) proxy.
%% 

start_client_nodes(#state{clients = Clients, 
			  work_dir = WorkDir,
			  debugs   = Debugs} = State) ->
    Connections = 
	[start_client_node(Client, WorkDir, Debugs) || Client <- Clients],
    State#state{client_conns = Connections}.

start_client_node(#client{path = ErlPath, host = Host}, WorkDir, Debugs) ->
    ?INFO("start client on host ~p", [Host]),
    EbinDir = filename:join(WorkDir, "ebin"), 
    start_client_node(Host, ErlPath, [EbinDir], Debugs).

start_client_node(Host, ErlPath, Paths, Debugs) ->
    start_node(Host, ?CLIENT_NODE_NAME, 
	       ErlPath, Paths, [], ?CLIENT_MOD, Debugs).


start_server_node(#state{http_server    = #server{path = ErlPath, host = Host},
			 server_root    = ServerRoot,
			 nof_schedulers = NofScheds,
			 debugs         = Debugs} = State) ->
    ?INFO("start server on host ~p", [Host]),
    CgiBinDir  = filename:join(ServerRoot, "cgi-bin"),
    EbinDir    = filename:join(ServerRoot, "ebin"), 
    Connection = 
	start_server_node(Host, ErlPath, [CgiBinDir, EbinDir], 
			  Debugs, NofScheds),
    State#state{server_conn = Connection}.

start_server_node(Host, ErlPath, Paths, Debugs, NofScheds) ->
    Args = 
	if
	    NofScheds =:= 0 ->
		"-smp disable";
	    true ->
		lists:flatten(io_lib:format("-smp +S ~w", [NofScheds]))
	end,
    start_node(Host, ?SERVER_NODE_NAME, 
	       ErlPath, Paths, Args, ?SERVER_MOD, Debugs).


%% -----------------------------------------------------------------------
%% - For every node created (server and client both) there is both 
%%   a local and remote proxy. 
%% - The local proxy is running on the local (controller/collector) node.
%% - The remote proxy is running on the client or server node(s).
%% - The local (ctrl) proxy monitor the remote (server/client) proxy.
%% - The remote (server/client) proxy monitor the local (ctrl) proxy.
%% 

start_node(Host, NodeName, ErlPath, Paths, Args, Module, Debugs) ->
    %% Start the (local) proxy
    ?DEBUG("start_node -> start local proxy and remote node", []),
    ProxyDebug = proplists:get_value(proxy, Debugs, silence), 
    Proxy = proxy_start(Host, NodeName, ErlPath, Paths, Args, Module, 
			ProxyDebug),

    ?DEBUG("start_node -> local proxy started - now start node", []),
    SlaveDebug = proplists:get_value(slave, Debugs, silence), 
    Node = proxy_start_node(Proxy, SlaveDebug),

    ?DEBUG("start_node -> sync global", []),
    global:sync(),

    ?DEBUG("start_node -> start remote proxy", []),
    proxy_start_remote(Proxy),

    ?DEBUG("start_node -> start (remote) inets framework", []),
    proxy_start_inets(Proxy), 

    ?DEBUG("start_node -> done", []),
    #connection{proxy = Proxy, node = Node, node_name = NodeName, host = Host}.

    
proxy_start(Host, NodeName, ErlPath, Paths, Args, Module, Debug) ->
    ?LOG("try starting local proxy for ~p@~s", [NodeName, Host]), 
    ProxyArgs = [Host, NodeName, ErlPath, Paths, Args, Module, Debug], 
    case proc_lib:start_link(?MODULE, proxy, 
			     ProxyArgs, ?LOCAL_PROXY_START_TIMEOUT) of
	{ok, Proxy} ->
	    Proxy;
	Error ->
	    exit({failed_starting_proxy, Error})
    end.

proxy_start_node(Proxy, Debug) ->
    {ok, Node} = proxy_request(Proxy, {start_node, Debug}),
    Node.

proxy_start_remote(Proxy) ->
    proxy_request(Proxy, start_remote_proxy).

proxy_start_inets(Proxy) ->
    proxy_request(Proxy, start_inets).

proxy_start_service(Proxy, Args) ->
    proxy_request(Proxy, {start_service, Args}).

proxy_release(Proxy) ->
    proxy_request(Proxy, release).

proxy_stop(Proxy) ->
    StopResult = proxy_request(Proxy, stop),
    ?DEBUG("proxy stop result: ~p", [StopResult]),
    StopResult.

proxy_request(Proxy, Req) ->
    Ref = make_ref(),
    Proxy ! {proxy_request, Ref, self(), Req},
    receive
	{proxy_reply, Ref, Proxy, Rep} ->
	    Rep
    end.

proxy_reply(From, Ref, Rep) ->
    From ! {proxy_reply, Ref, self(), Rep}.

proxy(Host, NodeName, ErlPath, Paths, Args, Module, Debug) ->
    process_flag(trap_exit, true),
    SName = lists:flatten(
	      io_lib:format("HDLT CTRL PROXY[~p,~s,~w]", 
			    [self(), Host, NodeName])), 
    ?SET_NAME(SName),
    ?SET_LEVEL(Debug), 
    ?LOG("starting with"
	"~n   Host:     ~p"
	"~n   NodeName: ~p"
	"~n   ErlPath:  ~p"
	"~n   Paths:    ~p"
	"~n   Args:     ~p"
	"~n   Module:   ~p", [Host, NodeName, ErlPath, Paths, Args, Module]), 
    State = #proxy{mode      = started,
		   mod       = Module, 
		   host      = Host,
		   node_name = NodeName,
		   erl_path  = ErlPath,
		   paths     = Paths,
		   args      = Args},
    proc_lib:init_ack({ok, self()}),
    ?DEBUG("started", []), 
    proxy_loop(State).


proxy_loop(#proxy{mode = stopping}) ->
    receive
	{proxy_request, Ref, From, stop} ->
	    ?LOG("[stopping] received stop order", []),
	    proxy_reply(From, Ref, ok),
	    exit(normal);

	{'EXIT', Pid, Reason} ->
	    ?INFO("[stopping] received exit message from ~p: "
		  "~n   Reason: ~p", [Pid, Reason]),
	    exit(Reason)
    
    end;

proxy_loop(#proxy{mode      = started,
		  host      = Host,
		  node_name = NodeName,
		  erl_path  = ErlPath,
		  paths     = Paths,
		  args      = Args} = State) ->
    receive
	{proxy_request, Ref, From, {start_node, Debug}} ->
	    ?LOG("[starting] received start_node order", []),
	    case hdlt_slave:start_link(Host, NodeName, 
				       ErlPath, Paths, Args, 
				       Debug) of
		{ok, Node} ->
		    ?DEBUG("[starting] node ~p started - now monitor", [Node]),
		    erlang:monitor_node(Node, true),
		    State2 = State#proxy{mode = operational,
					node = Node},
		    proxy_reply(From, Ref, {ok, Node}),
		    proxy_loop(State2);
		{error, Reason} ->
		    ?INFO("[starting] failed starting node: "
			  "~n   Reason: ~p", [Reason]),
		    exit({failed_starting_node, {Host, NodeName, Reason}})
	    end;

	{'EXIT', Pid, Reason} ->
	    ?INFO("[stopping] received exit message from ~p: "
		  "~n   Reason: ~p", [Pid, Reason]),
	    exit(Reason)
    
    end;

proxy_loop(#proxy{mode = operational,
		  mod  = Mod,
		  node = Node} = State) ->
    ?DEBUG("[operational] await command", []),
    receive 
	{proxy_request, Ref, From, start_remote_proxy} ->
	    ?LOG("[operational] start remote proxy", []),
	    case rpc:call(Node, Mod, start, [?GET_LEVEL()]) of
		{ok, Pid} ->
		    ?DEBUG("[operational] remote proxy started (~p) - "
			   "create monitor", [Pid]),
		    ProxyRef = erlang:monitor(process, Pid),
		    ?DEBUG("[operational] monitor: ~p", [Ref]),
		    proxy_reply(From, Ref, ok),
		    proxy_loop(State#proxy{ref = ProxyRef});
		Error ->
		    ?INFO("[operational] failed starting remote proxy"
			 "~n   Error: ~p", [Error]),
		    ReplyReason = {failed_starting_remote_proxy, 
				   {Node, Error}},
		    Reply = {error, ReplyReason}, 
		    proxy_reply(From, Ref, Reply),
		    exit({failed_starting_remote_proxy, {Node, Error}})
	    end;

	{proxy_request, Ref, From, start_inets} ->
	    ?INFO("[operational] start inets framework", []),
	    rpc:cast(Node, Mod, start_inets, []),
	    proxy_reply(From, Ref, ok),
	    proxy_loop(State);

	{proxy_request, Ref, From, {start_service, Args}} ->
	    ?INFO("[operational] start service with"
		  "~n   ~p", [Args]),
	    case rpc:call(Node, Mod, start_service, Args) of
		ok ->
		    ?DEBUG("[operational] service started", []),
		    proxy_reply(From, Ref, ok), 
		    proxy_loop(State);
		Error ->
		    ?INFO("[operational] failed starting service: "
			  "~n   Args.  ~p"
			  "~n   Error: ~p", [Args, Error]),
		    erlang:demonitor(State#proxy.ref, [flush]),
		    Reply = {error, {failed_starting_service, Node, Error}}, 
		    proxy_reply(From, Ref, Reply),
		    exit({failed_starting_service, Node, Error})
	    end;

	{proxy_request, Ref, From, release} ->
	    ?INFO("[operational] release", []),
	    rpc:call(Node, Mod, release, []),
	    proxy_reply(From, Ref, ok), 
	    proxy_loop(State);

	{proxy_request, Ref, From, stop} ->
	    ?INFO("[operational] received stop order", []),
	    erlang:demonitor(State#proxy.ref, [flush]),
	    ?DEBUG("[operational] rpc cast stop order", []),
	    rpc:cast(Node, Mod, stop, []), 
	    %% And wait for the node death to be reported
	    Reason = 
		receive 
		    {nodedown, Node} when State#proxy.node =:= Node ->
			ok
		after 10000 ->
			?INFO("Node did not die within expected time frame", 
			      []),
			{node_death_timeout, Node}
		end,
	    ?DEBUG("[operational] ack stop", []),
	    proxy_reply(From, Ref, Reason),
	    exit(normal);

	{nodedown, Node} when State#proxy.node =:= Node ->
	    ?INFO("[operational] received unexpected nodedoen message", []),
	    exit({node_died, Node});

	{'DOWN', Ref, process, _, normal} when State#proxy.ref =:= Ref ->
	    ?INFO("[operational] remote proxy terminated normally", []),
	    proxy_loop(State#proxy{ref        = undefined,
				   connection = undefined,
				   mode       = stopping});

	{'DOWN', Ref, process, _, noconnection} when State#proxy.ref =:= Ref ->
	    ?INFO("[operational] remote proxy terminated - no node", []),
	    proxy_loop(State#proxy{ref        = undefined,
				   connection = undefined,
				   mode       = stopping});

	{'DOWN', Ref, process, _, Reason} when State#proxy.ref =:= Ref ->
	    ?INFO("[operational] remote proxy terminated: "
		  "~n   Reason: ~p", [Reason]),
	    exit({remote_proxy_crash, Reason});

	{'EXIT', Pid, Reason} ->
	    ?INFO("[operational] received unexpected exit message from ~p: "
		  "~n   Reason: ~p", [Pid, Reason]),
	    proxy_loop(State)
    
    end.


stop_nodes(#state{server_conn  = ServerConn, 
		  client_conns = ClientConns} = State) ->
    lists:foreach(
      fun(#connection{proxy = Proxy, node_name = NodeName, host = Host}) -> 
	      ?DEBUG("stop_erlang_nodes -> send stop order to local proxy ~p"
		  "~n   for node ~p on ~s", [Proxy, NodeName, Host]),
	      proxy_stop(Proxy)
      end, 
      ClientConns ++ [ServerConn]),
    ?DEBUG("stop_erlang_nodes -> sleep some to give the nodes time to die", 
	   []),
    timer:sleep(1000),
    ?DEBUG("stop_erlang_nodes -> and a final cleanup round", []),
    lists:foreach(fun(Node) ->
			  ?INFO("try brutal stop node ~p", [Node]),
			  rpc:cast(Node, erlang, halt, [])
		  end, 
		  nodes() -- [node()]),
    ?DEBUG("stop_erlang_nodes -> done", []),
    State#state{server_conn = undefined, client_conns = []}.


%% The nodes on which the HDLT clients run have been started previously
start_clients(#state{client_conns     = Connections, 
		     debugs           = Debugs,
		     work_dir         = WorkDir,
		     socket_type      = SocketType,
		     client_cert_file = CertFile, 
		     client_sz_from   = From,
		     client_sz_to     = To,
		     client_sz_incr   = Incr}, 
	      URL, TestTime, SendRate) ->
    Debug = proplists:get_value(client, Debugs, silence), 
    StartClient = 
	fun(#connection{host = Host} = Connection) ->
		?DEBUG("start client on ~p", [Host]),
		start_client(Connection, 
			     WorkDir, SocketType, CertFile, 
			     URL, From, To, Incr, 
			     TestTime, SendRate, Debug);
	   (_) ->
		ok
	end,
    lists:foreach(StartClient, Connections).

start_client(#connection{proxy = Proxy}, 
	     WorkDir, SocketType, LocalCertFile, 
	     URL, From, To, Incr, 
	     TestTime, SendRate, Debug) ->
    SSLDir  = filename:join(WorkDir, "ssl"), 
    CertFile = filename:join(SSLDir, filename:basename(LocalCertFile)),
    Sizes    = randomized_sizes(From, To, Incr),
    Args = [SocketType, CertFile, URL, Sizes, TestTime, SendRate, Debug],
    proxy_start_service(Proxy, [Args]).
    
release_clients(#state{client_conns = Connections}) ->
    ReleaseClient = 
	fun(#connection{proxy = Proxy,
			host  = Host}) ->
		?DEBUG("release client on ~p", [Host]),
		proxy_release(Proxy);
	   (_) ->
		ok
	end,
    lists:foreach(ReleaseClient, Connections).

    
start_server(#state{server_conn      = #connection{proxy = Proxy}, 
		    http_port        = Port,
		    server_root      = ServerRoot,
		    doc_root         = DocRoot,
		    socket_type      = SocketType,
		    server_cert_file = CertFile}) ->

    HttpdConfig = 
	httpd_config(Port, "hdlt", ServerRoot, DocRoot, SocketType, CertFile),
    ?LOG("start the httpd inets service with config: "
	 "~n   ~p", [HttpdConfig]),
    proxy_start_service(Proxy, [HttpdConfig]), 
    ?DEBUG("start_server -> done", []),
    ok.
 

httpd_config(Port, ServerName, ServerRoot, DocRoot, 
	     SocketType, LocalCertFile) ->  
    LogDir = filename:join(ServerRoot, "log"),
    ErrorLog = filename:join(LogDir, "error_log"),
    TransferLog = filename:join(LogDir, "access_log"),
    
    SSL = 
	case SocketType of
	    ip_comm ->
		[];
	    _ -> % ssl
		SSLDir = filename:join(ServerRoot, "ssl"), 
		CertFile = 
		    filename:join(SSLDir, filename:basename(LocalCertFile)),
		[
		 {ssl_certificate_file,     CertFile},
		 {ssl_certificate_key_file, CertFile},
		 {ssl_verify_client,        0}
		]
	end,
    [{port,          Port},
     {server_name,   ServerName}, 
     {server_root,   ServerRoot},
     {document_root, DocRoot}, 
     {error_log,     ErrorLog}, 
     {error_log_format, pretty}, 
     {transfer_log,  TransferLog}, 
     {socket_type,   SocketType},
     {max_clients,   10000},
     {modules,       [mod_alias, mod_auth, mod_esi, mod_actions, mod_cgi, 
		      mod_dir, mod_get, mod_head, mod_log, mod_disk_log]}, 
     {script_alias,       {"/cgi-bin", filename:join(ServerRoot, "cgi-bin")}},
     {erl_script_alias,   {"/cgi-bin", [hdlt_random_html]}}, 
     {erl_script_timeout, 120000} | SSL].


clean_up(#state{server_root = ServerRoot, 
		work_dir    = WorkDir, 
		http_server = #server{host = Host},
		clients     = Clients}) ->
    ?DEBUG("begin server cleanup", []),
    server_clean_up(ServerRoot, WorkDir, Host),
    ?DEBUG("begin lient cleanup", []),
    clients_clean_up(WorkDir, Clients),
    ?DEBUG("cleanup done", []),
    ok.

server_clean_up(ServerRoot, WorkDir, Host) ->
    ?DEBUG("server cleanup - create sftp channel", []),
    {ok, Sftp, ConnectionRef} = 
	ssh_sftp:start_channel(Host, [{user_interaction, false},
				      {silently_accept_hosts, true}]),
    ?DEBUG("server cleanup - delete ~p dirs", [ServerRoot]),
    del_dirs(Sftp, ServerRoot),
    ?DEBUG("server cleanup - delete ~p dirs", [WorkDir]),
    del_dirs(Sftp, WorkDir),
    ?DEBUG("server cleanup - close sftp channel", []),
    ssh:close(ConnectionRef).

clients_clean_up(_WorkDir, []) ->
    ok;
clients_clean_up(WorkDir, [Client|Clients]) ->
    client_clean_up(WorkDir, Client),
    clients_clean_up(WorkDir, Clients).

client_clean_up(WorkDir, #client{host = Host}) ->
    ?DEBUG("client cleanup - create sftp channel to ~p", [Host]),
    {ok, Sftp, ConnectionRef} = 
	ssh_sftp:start_channel(Host, [{user_interaction, false},
				      {silently_accept_hosts, true}]),
    ?DEBUG("client cleanup - delete ~p dirs", [WorkDir]),
    del_dirs(Sftp, WorkDir),
    ?DEBUG("client cleanup - close sftp channel", []),
    ssh:close(ConnectionRef).
    

del_dirs(Sftp, Dir) ->
    case ssh_sftp:list_dir(Sftp, Dir) of
	{ok, []} ->
	    ssh_sftp:del_dir(Sftp, Dir);
	{ok, Files} ->
	    Files2 = [F || F <- Files, (F =/= "..") andalso (F =/= ".")], 
	    lists:foreach(fun(File) when ((File =/= "..") andalso 
					  (File =/= ".")) ->
				  FullPath = filename:join(Dir, File),
				  case ssh_sftp:read_file_info(Sftp, 
							       FullPath) of
				      {ok, #file_info{type = directory}} ->
					  del_dirs(Sftp, FullPath),
					  ssh_sftp:del_dir(Sftp, FullPath);
				      {ok, _} ->
					  ssh_sftp:delete(Sftp, FullPath)
				  end 
			  end, Files2);
	_ ->
	    ok
    end.

collect_data(#state{clients = Clients} = State) ->
    N = length(Clients),
    collect_req_reply(N, State),
    collect_time(N, State).
     
collect_req_reply(0, _State) ->
    ?DEBUG("all reply data collected", []), 
    ok;
collect_req_reply(N, #state{nof_schedulers = NofScheduler, 
			    results        = Db,
			    client_conns   = Conns} = State) ->
    ?DEBUG("await reply data from ~p client(s)", [N]),
    receive 
	{load_data, 
	 {req_reply, Client, NoRequests, NoReplys}} ->
	    ?DEBUG("received req_reply load-data from client ~p: "
		 "~n   Number of requests: ~p"
		 "~n   Number of replies:  ~p", 
		 [Client, NoRequests, NoReplys]),
	    ets:insert(Db, {{NofScheduler, Client}, 
			    {req_reply, NoRequests, NoReplys}});
	stop ->
	    ?INFO("received stop", []),
	    exit(self(), stop);

	{client_exit, Client, Node, Reason} ->
	    ?INFO("Received unexpected client exit from ~p on node ~p "
		  "while collecting replies: "
		  "~n   ~p", [Client, Node, Reason]),
	    case lists:keysearch(Node, #connection.node, Conns) of
		{value, Conn} ->
		    ?LOG("Found problem connection: "
			 "~n   ~p", [Conn]),
		    exit({unexpected_client_exit, Reason});
		false ->
		    collect_req_reply(N, State)
	    end
    end,
    collect_req_reply(N-1, State).		 
	 
collect_time(0, _State) -> 
    ?DEBUG("all time data collected", []), 
    ok;
collect_time(N, #state{nof_schedulers = NofScheduler, 
		       results        = Db,
		       client_conns   = Conns} = State) ->
    ?DEBUG("await time data from ~p clients", [N]),
    receive 
	{load_data, 
	 {time_to_complete, Client, StopTime, LastResponseTime}} ->
	    ?LOG("received time load-data from client ~p: "
		 "~n   Time of stop:           ~p"
		 "~n   Time of last response:  ~p", 
		 [Client, StopTime, LastResponseTime]),
	    ets:insert(Db, {{NofScheduler, Client}, 
			    {time, StopTime, LastResponseTime}});
	stop ->
	    ?INFO("received stop while collecting data, when N = ~p", [N]),
	    exit(self(), stop);

	{client_exit, Client, Node, Reason} ->
	    ?INFO("Received unexpected exit from client ~p on node ~p "
		  "while collecting time data: "
		  "~n   ~p", [Client, Node, Reason]),
	    case lists:keysearch(Node, #connection.node, Conns) of
		{value, Conn} ->
		    ?LOG("Found problem connection: "
			 "~n   ~p", [Conn]),
		    exit({unexpected_client_exit, Reason});
		false ->
		    collect_req_reply(N, State)
	    end;
	    
	Else -> %%% Something is wrong!
	    ?INFO("RECEIVED UNEXPECTED MESSAGE WHILE COLLECTING TIME DATA: "
		  "~n   ~p", [Else]),
	    collect_time(N, State)	     
    end,
    collect_time(N-1, State).

analyse_data(#state{results            = Db, 
		    max_nof_schedulers = MaxNofSchedulers,
		    test_time          = MicroSec}) ->
    Tab = ets:new(analysed_results, [set]),
    lists:foreach(fun(NofSchedulers) ->
			  Result = analyse(NofSchedulers, Db, MicroSec),
			  ets:insert(Tab, Result)
		  end, [N || N <- lists:seq(0, MaxNofSchedulers)]),
    Tab.

    
no_requests_replys(NoSchedulers, Tab) ->
    NoRequests = 
	ets:select(Tab, [{{{NoSchedulers,'_'},{req_reply, '$1', '_'}},
			  [],['$$']}]),
    NoReplys = 
	ets:select(Tab, [{{{NoSchedulers, '_'}, {req_reply, '_', '$1'}}, 
			  [], ['$$']}]),
    
    {lists:sum(lists:append(NoRequests)), 
     lists:sum(lists:append(NoReplys))}. 

max_time_to_final_response(NofSchedulers, Tab) ->
    Candidates = 
	ets:select(Tab, [{{{NofSchedulers, '_'}, {time, '$1', '$2'}},
			  [], ['$$']}]),
    
    NewCandidates = lists:map(
		      fun([StopTime, LastTime]) ->
			      round(
				timer:now_diff(LastTime, StopTime) / 100000)/10
			      end, Candidates),
    
    lists:max(NewCandidates).


analyse(NofSchedulers, Db, TestTime) ->
    Sec = TestTime / 1000,
    {NoRequests, NoReplys} = no_requests_replys(NofSchedulers, Db),
    {NofSchedulers, round(NoReplys / Sec), NoRequests,
     max_time_to_final_response(NofSchedulers, Db)}.


save_results_to_file(AnalysedTab, 
		     #state{socket_type        = SocketType, 
			    http_server        = #server{host = Server},
			    max_nof_schedulers = MaxNofSchedulers}) ->
    FileName = fun(Post) -> 
		       File = 
			   lists:flatten(
			     io_lib:format("~s_~w_~s", 
					   [Server, SocketType, Post])),
		       filename:join("./", File) 
	       end,
    Reps     = FileName("replys_per_sec.txt"),
    Reqs     = FileName("total_requests.txt"),
    Decay    = FileName("decay_time.txt"),
    
    [FdReps, FdReqs, FdDecay] = 
	lists:map(fun(File) ->
			  {ok, Fd} = file:open(File, [write]),
			  Fd
		  end, [Reps, Reqs, Decay]),
    lists:foreach(fun(NofSchedulers) ->
			  save_result_to_file(NofSchedulers,
					       FdReps, FdReqs, 
					       FdDecay, AnalysedTab)
		  end, [N || N <- lists:seq(0, MaxNofSchedulers)]),
    [Reps, Reqs, Decay].
    
save_result_to_file(NofSchedulers,
		    FdReps, FdReqs, FdDecay, AnalysedTab) ->

    [{NofSchedulers, NofRepsPerSec, NofReqs, MaxFinalResponseTime}] = 
	ets:lookup(AnalysedTab, NofSchedulers),
    
    file:write(FdReps, io_lib:format("~p,~p~n", 
				       [NofRepsPerSec, NofSchedulers])),
    file:write(FdReqs,  io_lib:format("~p,~p~n", 
					  [NofReqs, NofSchedulers])),
    file:write(FdDecay, io_lib:format("~p,~p~n", [MaxFinalResponseTime,
						  NofSchedulers])).


help() ->
    io:format("hdlt:start(Options). Where options:~n "
	      " ~n~p~n~n hdlt:start([]). -> hdlt:start(~p)~n~n",
	      [[{send_rate, "integer()", 
		 "Numer of outstanding requests that a client "
		 "should have during the test to create a load situation."},
		{clients, "[{path(), host()}]", "Paths to erlang and names of hosts to run clients on."},
		{test_time, "{hours(), mins(), sec()}", 
		 "How long the test should be run."},
		{server, "{path(), host()}", "Path to erl and name of host to run the HTTP-server on."},
		{port, "port()", "The port that the HTTP-server should use."},
		{server_dir, "dir()", "The directory where the HTTP server "
		 " stores its contents and configuration."},
		{work_dir, "dir()", "Path on the computer, where the test "
		 "is run, to a directory where the results can be saved."},
 		{max_no_schedulers, "integer()", 
		 "Max number of schedulers to run."},
	       {socket_type, "Httpd configuration option socket_type"}],
	       defaults()]).


defaults() ->
    [{send_rate,          ?DEFAULT_SENDRATE},
     %% {clients,           []},
     {test_time,          ?DEFAULT_TEST_TIME}, 
     %% {server,            ?DEFAULT_SERVER},
     {port,               ?DEFAULT_PORT},
     {server_dir,         ?DEFAULT_SERVER_DIR},
     {work_dir,           ?DEFAULT_WORK_DIR},
     {max_nof_schedulers, ?DEFAULT_MAX_NOF_SCHEDULERS},
     {socket_type,        ?DEFAULT_SOCKET_TYPE}].


get_debugs(Config) ->
    ?DEBUG("get debugs", []),
    Debugs = proplists:get_value(debug, Config, ?DEFAULT_DEBUGS),
    verify_debugs(Debugs),
    Debugs.

verify_debugs([]) ->
    ok;
verify_debugs([{Tag, Debug}|Debugs]) ->
    verify_debug(Tag, Debug),
    verify_debugs(Debugs).

verify_debug(Tag, Debug) ->
    case lists:member(Tag, [ctrl, proxy, slave, client]) of
	true ->
	    ok;
	false ->
	    exit({bad_debug_tag, Tag})
    end,
    case lists:member(Debug, [silence, info, log, debug]) of
	true ->
	    ok;
	false ->
	    exit({bad_debug_level, Debug})
    end.

get_send_rate(Config) ->
    ?DEBUG("get send_rate", []),
    case proplists:get_value(send_rate, Config, ?DEFAULT_SENDRATE) of
	SendRate when is_integer(SendRate) andalso (SendRate > 0) ->
	    SendRate;
	BadSendRate ->
	    exit({bad_sendrate, BadSendRate})
    end.
    

get_clients(Config) ->
    ?DEBUG("get clients", []),
    case proplists:get_value(clients, Config, undefined) of
	undefined ->
	    missing_mandatory_config(clients);
	Clients when is_list(Clients) andalso (length(Clients) > 0) ->
	    case [#client{path = Path, host = Host} || 
		     {Path, Host} <- Clients] of
		Clients2 when (length(Clients2) > 0) ->
		    Clients2;
		_ ->
		    exit({bad_clients, Clients})
	    end;
	
	BadClients ->
	    exit({bad_clients, BadClients})

    end.
    
get_server(Config) ->	     
    ?DEBUG("get server", []),
    case proplists:get_value(server, Config) of
	{Path, Host} when is_list(Path) andalso is_list(Host) ->
	    #server{path = Path, host = Host};
	undefined ->
	    missing_mandatory_config(server)
    end.

get_server_dir(Config) ->
    ?DEBUG("get server_dir", []),
    get_dir(server_dir, Config, ?DEFAULT_SERVER_DIR).

get_work_dir(Config) ->
    ?DEBUG("get work_dir", []),
    get_dir(work_dir, Config, ?DEFAULT_WORK_DIR).

get_dir(Key, Config, Default) ->
    Dir = proplists:get_value(Key, Config, Default),
    ensure_absolute(Dir),
    Dir.
    
ensure_absolute(Path) ->
    case filename:pathtype(Path) of
	absolute ->
	    ok;
	PathType ->
	    exit({bad_pathtype, Path, PathType})
    end.

get_port(Config) ->
    ?DEBUG("get port", []),
    case proplists:get_value(port, Config, ?DEFAULT_PORT) of
	Port when is_integer(Port) andalso (Port > 0) ->
	    Port;
	BadPort ->
	    exit({bad_port, BadPort})
    end.

get_socket_type(Config) ->
    ?DEBUG("get socket_type", []),
    case proplists:get_value(socket_type, Config, ?DEFAULT_SOCKET_TYPE) of
	SocketType when ((SocketType =:= ip_comm) orelse
			 (SocketType =:= ssl) orelse
			 (SocketType =:= essl) orelse
			 (SocketType =:= ossl)) ->
	    SocketType;
	BadSocketType ->
	    exit({bad_socket_type, BadSocketType})
    end.

get_test_time(Config) ->
    ?DEBUG("get test_time", []),
    case proplists:get_value(test_time, Config, ?DEFAULT_TEST_TIME) of
	Seconds when is_integer(Seconds) andalso (Seconds > 0) -> 
	    timer:seconds(Seconds);
	BadTestTime ->
	    exit({bad_test_time, BadTestTime})
    end.

get_max_nof_schedulers(Config) ->
    ?DEBUG("get max_nof_schedulers", []),
    case proplists:get_value(max_nof_schedulers, 
			     Config, 
			     ?DEFAULT_MAX_NOF_SCHEDULERS) of
	MaxNofScheds when (is_integer(MaxNofScheds) andalso 
			   (MaxNofScheds >= 0)) ->
	    MaxNofScheds;
	BadMaxNofScheds ->
	    exit({bad_max_nof_schedulers, BadMaxNofScheds})
    end.
	    

get_server_cert_file(Config) ->
    ?DEBUG("get server cert file", []),
    get_cert_file(server_cert_file, ?DEFAULT_SERVER_CERT, Config).

get_client_cert_file(Config) ->
    ?DEBUG("get client cert file", []),
    get_cert_file(client_cert_file, ?DEFAULT_CLIENT_CERT, Config).

get_cert_file(Tag, DefaultCertFileName, Config) -> 
    LibDir  = code:lib_dir(inets),
    HdltDir = filename:join(LibDir, "examples/httpd_load_test"),
    DefaultCertFile = filename:join(HdltDir, DefaultCertFileName),
    case proplists:get_value(Tag, Config, DefaultCertFile) of
	F when is_list(F) ->
	    case file:read_file_info(F) of
		{ok, #file_info{type = regular}} ->
		    F;
		{ok, #file_info{type = Type}} ->
		    exit({wrong_file_type, Tag, F, Type});
		{error, Reason} ->
		    exit({failed_readin_file_info, Tag, F, Reason})
	    end;
	BadFile ->
	    exit({bad_cert_file, Tag, BadFile})
    end.

	    
get_work_sim(Config) ->
    ?DEBUG("get work_sim", []),
    case proplists:get_value(work_simulator, Config, ?DEFAULT_WORK_SIM) of
	WS when is_integer(WS) andalso (WS > 0) ->
	    WS;
	BadWS ->
	    exit({bad_work_simulator, BadWS})
    end.

	    
get_data_size(Config) ->
    ?DEBUG("get data_size", []),
    case proplists:get_value(data_size, Config, ?DEFAULT_DATA_SIZE) of
	{From, To, Incr} = DS when (is_integer(From) andalso 
				    is_integer(To)   andalso 
				    is_integer(Incr) andalso 
				    (To > From)      andalso 
				    (From > 0)       andalso 
				    (Incr > 0)) ->
	    DS;
	{From, To} when (is_integer(From) andalso 
			 is_integer(To)   andalso 
			 (To > From)      andalso 
			 (From > 0)) ->
	    {From, To, ?DEFAULT_DATA_SIZE_INCR};
	BadDS ->
	    exit({bad_data_size, BadDS})
    end.

	    
url(#server{host = Host}, Port, SocketType, WorkSim) ->
    Scheme = 
	case SocketType of
	    ip_comm ->
		"http";
	    _ -> %% SSL
		"https"
	end,
    lists:flatten(
      io_lib:format("~s://~s:~w/cgi-bin/hdlt_random_html:page?~w:", 
		    [Scheme, Host, Port, WorkSim])).


missing_mandatory_config(Missing) ->
    exit({missing_mandatory_config, Missing}).


ensure_remote_dir_exist(Sftp, Path0) ->
    case filename:split(Path0) of
	[Root, Dir | Rest] ->
	    %% We never accept creating the root directory, 
	    %% or the next level, so these *must* exist:
	    Path = filename:join(Root, Dir),
	    case ssh_sftp:read_file_info(Sftp, Path) of
		{ok, #file_info{type = directory}} ->
		    ensure_remote_dir_exist(Sftp, Path, Rest);
		{ok, #file_info{type = Type}} ->
		    ?INFO("Not a dir: ~p (~p)", [Path, Type]),
		    exit({not_a_dir, Path, Type});
		{error, Reason} ->
		    ?INFO("Failed reading file info for ~p: ~p", 
			  [Path, Reason]),
		    exit({failed_reading_file_info, Path, Reason})
	    end;
	BadSplit ->
	    ?INFO("Bad remote dir path: ~p -> ~p", [Path0, BadSplit]),
	    exit({bad_dir, Path0})
    end.

ensure_remote_dir_exist(_Sftp, _Dir, []) ->
    ok;
ensure_remote_dir_exist(Sftp, Path, [Dir|Rest]) ->
    NewPath = filename:join(Path, Dir),
    case ssh_sftp:read_file_info(Sftp, NewPath) of
	{ok, #file_info{type = directory}} ->
	    ensure_remote_dir_exist(Sftp, NewPath, Rest);
	{ok, #file_info{type = Type}} ->
	    %% Exist, but is not a dir
	    ?INFO("Not a dir: ~p (~p)", [NewPath, Type]),
	    exit({not_a_dir, NewPath, Type});
	{error, Reason} ->
	    %% This *could* be because the dir does not exist,
	    %% but it could also be some other error.
	    %% As usual, the error reason of the sftp is
	    %% a pease of crap, so we cannot use the 
	    %% error reason. 
	    %% The simplest way to test this is to simply
	    %% try to create the directory, since we should
	    %% ensure its existence anyway..
	    case ssh_sftp:make_dir(Sftp, NewPath) of
		ok ->
		    ensure_remote_dir_exist(Sftp, NewPath, Rest);
		_ ->
		    ?INFO("Failed reading file info for ~p: ~p", 
			  [Dir, Reason]),
		    exit({failed_reading_file_info, NewPath, Reason})
	    end
    end.
    
maybe_create_remote_dir(Sftp, Dir) ->
    case ssh_sftp:read_file_info(Sftp, Dir) of
	{ok, #file_info{type = directory}} ->
	    ok;
	{ok, #file_info{type = Type}} ->
	    %% Exist, but is not a dir
	    ?INFO("Not a dir: ~p (~p)", [Dir, Type]),
	    exit({not_a_dir, Dir, Type});
	{error, Reason} ->
	    %% Assume dir noes not exist...
	    case ssh_sftp:make_dir(Sftp, Dir) of
		ok ->
		    ok;
		_ ->
		    ?INFO("Failed reading file info for ~p: ~p", 
			  [Dir, Reason]),
		    exit({failed_reading_file_info, Dir, Reason})
	    end
    end.
    

set_debug_level(Debugs) ->
    Debug = proplists:get_value(ctrl, Debugs, silence), 
    ?SET_LEVEL(Debug).


%% Generates a list of numbers between A and B, such that 
%% there is exact one number between A and B and then 
%% randomizes that list.


randomized_sizes(From, To, Incr) ->
    L   = lists:seq(From, To, Incr),
    Len = length(L),
    randomized_sizes2(L, 0, Len-1).

randomized_sizes2(L, N, Len) when N >= Len ->
    L;
randomized_sizes2(L, N, Len) ->
    SplitWhere = rand:uniform(Len),
    {A, B} = lists:split(SplitWhere, L),
    randomized_sizes2(B ++ A, N+1, Len).
