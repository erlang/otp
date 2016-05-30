%%--------------------------------------------------------------------
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

%%% @doc Common Test Framework functions for starting and stopping nodes for
%%% Large Scale Testing.
%%%
%%% <p>This module exports functions which are used by the Common Test Master
%%% to start and stop "slave" nodes. It is the default callback module for the
%%% <code>{init, node_start}</code> term of the Test Specification.</p>

%%----------------------------------------------------------------------
%% File    : ct_slave.erl
%% Description : CT module for starting nodes for large-scale testing.
%%
%% Created : 7 April 2010
%%----------------------------------------------------------------------
-module(ct_slave).

-export([start/1, start/2, start/3, stop/1, stop/2]).

-export([slave_started/2, slave_ready/2, monitor_master/1]).

-record(options, {username, password, boot_timeout, init_timeout,
		  startup_timeout, startup_functions, monitor_master,
		  kill_if_fail, erl_flags, env, ssh_port, ssh_opts}).

%%%-----------------------------------------------------------------
%%% @spec start(Node) -> Result
%%%   Node = atom()
%%%   Result = {ok, NodeName} |
%%%            {error, Reason, NodeName}
%%%   Reason = already_started |
%%%            started_not_connected |
%%%            boot_timeout |
%%%            init_timeout |
%%%            startup_timeout |
%%%            not_alive
%%%   NodeName = atom()
%%% @doc Starts an Erlang node with name <code>Node</code> on the local host.
%%% @see start/3
start(Node) ->
    start(gethostname(), Node).

%%%-----------------------------------------------------------------
%%% @spec start(HostOrNode, NodeOrOpts) -> Result
%%%   HostOrNode = atom()
%%%   NodeOrOpts = atom() | list()
%%%   Result = {ok, NodeName} |
%%%            {error, Reason, NodeName}
%%%   Reason = already_started |
%%%            started_not_connected |
%%%            boot_timeout |
%%%            init_timeout |
%%%            startup_timeout |
%%%            not_alive
%%%   NodeName = atom()
%%% @doc Starts an Erlang node with default options on a specified
%%% host, or on the local host with specified options. That is,
%%% the call is interpreted as <code>start(Host, Node)</code> when the
%%% second argument is atom-valued and <code>start(Node, Opts)</code>
%%% when it's list-valued.
%%% @see start/3
start(_HostOrNode = Node, _NodeOrOpts = Opts) %% match to satiate edoc
  when is_list(Opts) ->
    start(gethostname(), Node, Opts);

start(Host, Node) ->
    start(Host, Node, []).

%%%-----------------------------------------------------------------
%%% @spec start(Host, Node, Opts) -> Result
%%%   Node = atom()
%%%   Host = atom()
%%%   Opts = [OptTuples]
%%%   OptTuples = {username, Username} |
%%%		  {password, Password} |
%%%   		  {boot_timeout, BootTimeout} | {init_timeout, InitTimeout} |
%%%		  {startup_timeout, StartupTimeout} |
%%%		  {startup_functions, StartupFunctions} |
%%%		  {monitor_master, Monitor} |
%%%		  {kill_if_fail, KillIfFail} |
%%%		  {erl_flags, ErlangFlags} |
%%%               {env, [{EnvVar,Value}]}
%%%   Username = string()
%%%   Password = string()
%%%   BootTimeout = integer()
%%%   InitTimeout = integer()
%%%   StartupTimeout = integer()
%%%   StartupFunctions = [StartupFunctionSpec]
%%%   StartupFunctionSpec = {Module, Function, Arguments}
%%%   Module = atom()
%%%   Function = atom()
%%%   Arguments = [term]
%%%   Monitor = bool()
%%%   KillIfFail = bool()
%%%   ErlangFlags = string()
%%%   EnvVar = string()
%%%   Value = string()
%%%   Result = {ok, NodeName} |
%%%            {error, Reason, NodeName}
%%%   Reason = already_started |
%%%            started_not_connected |
%%%            boot_timeout |
%%%            init_timeout |
%%%            startup_timeout |
%%%            not_alive
%%%   NodeName = atom()
%%% @doc Starts an Erlang node with name <code>Node</code> on host
%%% <code>Host</code> as specified by the combination of options in
%%% <code>Opts</code>.
%%%
%%% <p>Options <code>Username</code> and <code>Password</code> will be used
%%% to log in onto the remote host <code>Host</code>.
%%% Username, if omitted, defaults to the current user name,
%%% and password is empty by default.</p>
%%%
%%% <p>A list of functions specified in the <code>Startup</code> option will be
%%% executed after startup of the node. Note that all used modules should be
%%% present in the code path on the <code>Host</code>.</p>
%%%
%%% <p>The timeouts are applied as follows:</p>
%%% <list>
%%%  <item>
%%%   <code>BootTimeout</code> - time to start the Erlang node, in seconds.
%%%   Defaults to 3 seconds. If node does not become pingable within this time,
%%%   the result <code>{error, boot_timeout, NodeName}</code> is returned;
%%%  </item>
%%%  <item>
%%%   <code>InitTimeout</code> - time to wait for the node until it calls the
%%%    internal callback function informing master about successfull startup.
%%%    Defaults to one second.
%%%    In case of timed out message the result
%%%    <code>{error, init_timeout, NodeName}</code> is returned;
%%%  </item>
%%%  <item>
%%%   <code>StartupTimeout</code> - time to wait intil the node finishes to run
%%%    the <code>StartupFunctions</code>. Defaults to one second.
%%%    If this timeout occurs, the result
%%%    <code>{error, startup_timeout, NodeName}</code> is returned.
%%%  </item>
%%% </list>
%%%
%%% <p>Option <code>monitor_master</code> specifies, if the slave node should be
%%% stopped in case of master node stop. Defaults to false.</p>
%%%
%%% <p>Option <code>kill_if_fail</code> specifies, if the slave node should be
%%% killed in case of a timeout during initialization or startup.
%%% Defaults to true. Note that node also may be still alive it the boot
%%% timeout occurred, but it will not be killed in this case.</p>
%%%
%%% <p>Option <code>erlang_flags</code> specifies, which flags will be added
%%% to the parameters of the <code>erl</code> executable.</p>
%%%
%%% <p>Option <code>env</code> specifies a list of environment variables
%%% that will extended the environment.</p>
%%%
%%% <p>Special return values are:</p>
%%% <list>
%%%  <item><code>{error, already_started, NodeName}</code> - if the node with
%%%   the given name is already started on a given host;</item>
%%%  <item><code>{error, started_not_connected, NodeName}</code> - if node is
%%%  started, but not connected to the master node.</item>
%%%  <item><code>{error, not_alive, NodeName}</code> - if node on which the
%%%   <code>ct_slave:start/3</code> is called, is not alive. Note that
%%%   <code>NodeName</code> is the name of current node in this case.</item>
%%% </list>
%%%
start(Host, Node, Opts) ->
    ENode = enodename(Host, Node),
    case erlang:is_alive() of
	false->
	    {error, not_alive, node()};
	true->
	    case is_started(ENode) of
		false->
		    OptionsRec = fetch_options(Opts),
		    do_start(Host, Node, OptionsRec);
		{true, not_connected}->
		    {error, started_not_connected, ENode};
		{true, connected}->
		    {error, already_started, ENode}
	    end
    end.

%%% @spec stop(Node) -> Result
%%%   Node = atom()
%%%   Result = {ok, NodeName} |
%%%            {error, Reason, NodeName}
%%%   Reason = not_started |
%%%            not_connected |
%%%            stop_timeout

%%%   NodeName = atom()
%%% @doc Stops the running Erlang node with name <code>Node</code> on
%%% the localhost.
stop(Node) ->
    stop(gethostname(), Node).

%%% @spec stop(Host, Node) -> Result
%%%   Host = atom()
%%%   Node = atom()
%%%   Result = {ok, NodeName} |
%%%            {error, Reason, NodeName}
%%%   Reason = not_started |
%%%            not_connected |
%%%            stop_timeout
%%%   NodeName = atom()
%%% @doc Stops the running Erlang node with name <code>Node</code> on
%%% host <code>Host</code>.
stop(Host, Node) ->
    ENode = enodename(Host, Node),
    case is_started(ENode) of
	{true, connected}->
	     do_stop(ENode);
	{true, not_connected}->
	     {error, not_connected, ENode};
	false->
	     {error, not_started, ENode}
    end.

%%% fetch an option value from the tagged tuple list with default
get_option_value(Key, OptionList, Default) ->
    case lists:keyfind(Key, 1, OptionList) of
	false->
	     Default;
	{Key, Value}->
	     Value
    end.

%%% convert option list to the option record, fill all defaults
fetch_options(Options) ->
    UserName = get_option_value(username, Options, []),
    Password = get_option_value(password, Options, []),
    BootTimeout = get_option_value(boot_timeout, Options, 3),
    InitTimeout = get_option_value(init_timeout, Options, 1),
    StartupTimeout = get_option_value(startup_timeout, Options, 1),
    StartupFunctions = get_option_value(startup_functions, Options, []),
    Monitor = get_option_value(monitor_master, Options, false),
    KillIfFail = get_option_value(kill_if_fail, Options, true),
    ErlFlags = get_option_value(erl_flags, Options, []),
    EnvVars = get_option_value(env, Options, []),
    SSHPort = get_option_value(ssh_port, Options, []),
    SSHOpts = get_option_value(ssh_opts, Options, []),
    #options{username=UserName, password=Password,
	     boot_timeout=BootTimeout, init_timeout=InitTimeout,
	     startup_timeout=StartupTimeout, startup_functions=StartupFunctions,
	     monitor_master=Monitor, kill_if_fail=KillIfFail,
	     erl_flags=ErlFlags, env=EnvVars, ssh_port=SSHPort, ssh_opts=SSHOpts}.

% send a message when slave node is started
% @hidden
slave_started(ENode, MasterPid) ->
    MasterPid ! {node_started, ENode},
    ok.

% send a message when slave node has finished startup
% @hidden
slave_ready(ENode, MasterPid) ->
    MasterPid ! {node_ready, ENode},
    ok.

% start monitoring of the master node
% @hidden
monitor_master(MasterNode) ->
    spawn(fun() -> monitor_master_int(MasterNode) end).

% code of the masterdeath-waiter process
monitor_master_int(MasterNode) ->
    erlang:monitor_node(MasterNode, true),
    receive
        {nodedown, MasterNode}->
	    init:stop()
    end.

% check if node is listed in the nodes()
is_connected(ENode) ->
    [N||N<-nodes(), N==ENode] == [ENode].

% check if node is alive (ping and disconnect if pingable)
is_started(ENode) ->
    case is_connected(ENode) of
	true->
	    {true, connected};
	false->
	    case net_adm:ping(ENode) of
		pang->
		    false;
		pong->
		    erlang:disconnect_node(ENode),
		    {true, not_connected}
	    end
    end.

% make a Erlang node name from name and hostname
enodename(Host, Node) ->
    list_to_atom(atom_to_list(Node)++"@"++atom_to_list(Host)).

% performs actual start of the "slave" node
do_start(Host, Node, Options) ->
    ENode = enodename(Host, Node),
    Functions =
	lists:append([[{ct_slave, slave_started, [ENode, self()]}],
		      Options#options.startup_functions,
		      [{ct_slave, slave_ready, [ENode, self()]}]]),
    Functions2 = if
	Options#options.monitor_master->
	    [{ct_slave, monitor_master, [node()]}|Functions];
	true->
	    Functions
    end,
    MasterHost = gethostname(),
    _ = if
	MasterHost == Host ->
	    spawn_local_node(Node, Options);
	true->
	    spawn_remote_node(Host, Node, Options)
    end,

    BootTimeout = Options#options.boot_timeout,
    InitTimeout = Options#options.init_timeout,
    StartupTimeout = Options#options.startup_timeout,
    Result = case wait_for_node_alive(ENode, BootTimeout) of
	pong->
	    case test_server:is_cover() of
		true ->
		    MainCoverNode = cover:get_main_node(),
		    rpc:call(MainCoverNode,cover,start,[ENode]);
		false ->
		    ok
	    end,
            call_functions(ENode, Functions2),
	    receive
		{node_started, ENode}->
		    receive
			{node_ready, ENode}->
			    {ok, ENode}
		    after StartupTimeout*1000->
			{error, startup_timeout, ENode}
		    end
	    after InitTimeout*1000 ->
		{error, init_timeout, ENode}
	    end;
        pang->
	    {error, boot_timeout, ENode}
    end,
    _ = case Result of
	{ok, ENode}->
	     ok;
	{error, Timeout, ENode}
	     when ((Timeout==init_timeout) or (Timeout==startup_timeout)) and
		  Options#options.kill_if_fail->
	     do_stop(ENode);
	_-> ok
    end,
    Result.

% are we using fully qualified hostnames
long_or_short() ->
    case net_kernel:longnames() of
	true->
	    " -name ";
	false->
	    " -sname "
    end.

% get the localhost's name, depending on the using name policy
gethostname() ->
    Hostname = case net_kernel:longnames() of
	true->
	    net_adm:localhost();
	_->
	    {ok, Name}=inet:gethostname(),
	    Name
    end,
    list_to_atom(Hostname).

% get cmd for starting Erlang
get_cmd(Node, Flags) ->
    Cookie = erlang:get_cookie(),
    "erl -detached -noinput -setcookie "++ atom_to_list(Cookie) ++
    long_or_short() ++ atom_to_list(Node) ++ " " ++ Flags.

% spawn node locally
spawn_local_node(Node, Options) ->
    #options{env=Env,erl_flags=ErlFlags} = Options,
    Cmd = get_cmd(Node, ErlFlags),
    open_port({spawn, Cmd}, [stream,{env,Env}]).

% spawn node remotely
spawn_remote_node(Host, Node, Options) ->
    #options{username=Username,
	     password=Password,
	     erl_flags=ErlFlags,
	     env=Env,
       ssh_port=MaybeSSHPort,
       ssh_opts=SSHOpts} = Options,
    SSHPort = case MaybeSSHPort of
                [] -> 22; % Use default SSH port
                A  -> A
              end,
    SSHOptions = case {Username, Password} of
	{[], []}->
	    [];
	{_, []}->
	    [{user, Username}];
	{_, _}->
	    [{user, Username}, {password, Password}]
    end ++ [{silently_accept_hosts, true}] ++ SSHOpts,
    {ok, _} = application:ensure_all_started(ssh),
    {ok, SSHConnRef} = ssh:connect(atom_to_list(Host), SSHPort, SSHOptions),
    {ok, SSHChannelId} = ssh_connection:session_channel(SSHConnRef, infinity),
    ssh_setenv(SSHConnRef, SSHChannelId, Env),
    ssh_connection:exec(SSHConnRef, SSHChannelId, get_cmd(Node, ErlFlags), infinity).

ssh_setenv(SSHConnRef, SSHChannelId, [{Var, Value} | Vars])
  when is_list(Var), is_list(Value) ->
    success = ssh_connection:setenv(SSHConnRef, SSHChannelId,
				    Var, Value, infinity),
    ssh_setenv(SSHConnRef, SSHChannelId, Vars);
ssh_setenv(_SSHConnRef, _SSHChannelId, []) -> ok.

% call functions on a remote Erlang node
call_functions(_Node, []) ->
    ok;
call_functions(Node, [{M, F, A}|Functions]) ->
    rpc:call(Node, M, F, A),
    call_functions(Node, Functions).

% wait N seconds until node is pingable
wait_for_node_alive(_Node, 0) ->
    pang;
wait_for_node_alive(Node, N) ->
    timer:sleep(1000),
    case net_adm:ping(Node) of
	pong->
	    pong;
	pang->
	    wait_for_node_alive(Node, N-1)
    end.

% call init:stop on a remote node
do_stop(ENode) ->
    {Cover,MainCoverNode} =
	case test_server:is_cover() of
	    true ->
		Main = cover:get_main_node(),
		rpc:call(Main,cover,flush,[ENode]),
		{true,Main};
	    false ->
		{false,undefined}
    end,
    spawn(ENode, init, stop, []),
    case wait_for_node_dead(ENode, 5) of
	{ok,ENode} ->
	    if Cover ->
		    %% To avoid that cover is started again if a node
		    %% with the same name is started later.
		    rpc:call(MainCoverNode,cover,stop,[ENode]);
	       true ->
		    ok
	    end,
	    {ok,ENode};
	Error ->
	    Error
    end.

% wait N seconds until node is disconnected
wait_for_node_dead(Node, 0) ->
    {error, stop_timeout, Node};
wait_for_node_dead(Node, N) ->
    timer:sleep(1000),
    case lists:member(Node, nodes()) of
	true->
	    wait_for_node_dead(Node, N-1);
	false->
	    {ok, Node}
    end.
