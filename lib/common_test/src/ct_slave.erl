%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2010-2025. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% File    : ct_slave.erl
%% Description : CT module for starting nodes for large-scale testing.
%%
%% Created : 7 April 2010
%%----------------------------------------------------------------------
-module(ct_slave).
-moduledoc """
`Common Test` framework functions for starting and stopping nodes for
Large-Scale Testing.

This module exports functions used by the `Common Test` Master to start and stop
"slave" nodes. It is the default callback module for the `{init, node_start}`
term in the Test Specification.
""".
-moduledoc(#{since => "OTP R14B"}).

-export([start/1, start/2, start/3, stop/1, stop/2]).

-export([slave_started/2, slave_ready/2, monitor_master/1]).

-deprecated([{'_','_',"use ?CT_PEER(), or the 'peer' module instead"}]).

-record(options, {username, password, boot_timeout, init_timeout,
		  startup_timeout, startup_functions, monitor_master,
		  kill_if_fail, erl_flags, env, ssh_port, ssh_opts,
		  stop_timeout}).

-doc "Options used for starting `ct_slave` node.".
-type start_options() :: [{'username', string()}
                         | {'password', string()}
                         | {'boot_timeout', non_neg_integer()}
                         | {'init_timeout', non_neg_integer()}
                         | {'startup_timeout', non_neg_integer()}
                         | {'startup_functions', [mfa()]}
                         | {'monitor_master', boolean()}
                         | {'kill_if_fail', boolean()}
                         | {'erl_flags', string()}
                         | {'env', [{Name :: os:env_var_name(), Val :: os:env_var_value() | false}]}
                         | {'ssh_port', inet:port_number()}
                         | {'ssh_opts', ssh:client_options()}].

-doc "Options used for stopping `ct_slave` node.".
-type stop_options() :: [{'stop_timeout', non_neg_integer()}].

-export_type([start_options/0, stop_options/0]).

-doc """
Starts an Erlang node with name `Node` on the local host.

See also [`ct_slave:start/3`](`start/3`).
""".
-doc(#{since => <<"OTP R14B">>}).
-spec start(Node) -> 'ok' | {'error', Reason, Node}
              when Node :: node(),
                   Reason :: atom().
start(Node) ->
    start(gethostname(), Node).

-doc """
Starts an Erlang node with default options on a specified host, or on the local
host with specified options. That is, the call is interpreted as
[`start(Host, Node)`](`start/2`) when the second argument is atom-valued and
[`start(Node, Opts)`](`start/2`) when it is list-valued.

See also [`ct_slave:start/3`](`start/3`).
""".
-doc(#{since => <<"OTP R14B">>}).
-spec start(HostOrNode, NodeOrOpts) -> 'ok' | {'error', Reason, Node}
              when HostOrNode :: atom() | node(),
                   NodeOrOpts :: node() | start_options(),
                   Reason :: atom(),
                   Node :: node().
start(_HostOrNode = Node, _NodeOrOpts = Opts) %% match to satiate edoc
  when is_list(Opts) ->
    start(gethostname(), Node, Opts);

start(Host, Node) ->
    start(Host, Node, []).

-doc """
Starts an Erlang node with name `Node` on host `Host` as specified by the
combination of options in `Opts`.

Options `Username` and `Password` are used to log on to the remote host `Host`.
`Username`, if omitted, defaults to the current username. `Password` is empty by
default.

A list of functions specified in option `Startup` are executed after startup of
the node. Notice that all used modules are to be present in the code path on
`Host`.

The time-outs are applied as follows:

- **`BootTimeout`** - The time to start the Erlang node, in seconds. Defaults to
  3 seconds. If the node is not pingable within this time, the result
  `{error, boot_timeout, NodeName}` is returned.

- **`InitTimeout`** - The time to wait for the node until it calls the internal
  callback function informing master about a successful startup. Defaults to 1
  second. In case of a timed out message, the result
  `{error, init_timeout, NodeName}` is returned.

- **`StartupTimeout`** - The time to wait until the node stops to run
  `StartupFunctions`. Defaults to 1 second. If this time-out occurs, the result
  `{error, startup_timeout, NodeName}` is returned.

_Options:_

- **`monitor_master`** - Specifies if the slave node is to be stopped if the
  master node stops. Defaults to `false`.

- **`kill_if_fail`** - Specifies if the slave node is to be killed if a time-out
  occurs during initialization or startup. Defaults to `true`. Notice that the
  node can also be still alive it the boot time-out occurred, but it is not
  killed in this case.

- **`erl_flags`** - Specifies which flags are added to the parameters of the
  executable `erl`.

- **`env`** - Specifies a list of environment variables that will extend the
  environment.

_Special return values:_

- `{error, already_started, NodeName}` if the node with the specified name is
  already started on a specified host.
- `{error, started_not_connected, NodeName}` if the node is started, but not
  connected to the master node.
- `{error, not_alive, NodeName}` if the node on which
  [`ct_slave:start/3`](`start/3`) is called, is not alive. Notice that
  `NodeName` is the name of the current node in this case.
""".
-doc(#{since => <<"OTP R14B">>}).
-spec start(Host, Node, Opts) -> 'ok' | {'error', Reason, Node}
              when Host :: atom(),
                   Node :: node(),
                   Opts :: start_options(),
                   Reason :: atom().
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

-doc """
Stops the running Erlang node with name `Node` on the local host.
""".
-doc(#{since => <<"OTP R14B">>}).
-spec stop(Node) -> {'ok', Node} | {'error', Reason, Node}
              when Node :: node(),
                   Reason :: atom().
stop(Node) ->
    stop(gethostname(), Node).

-doc """
Stops the running Erlang node with name `Node` on host `Host`.
""".
-doc(#{since => <<"OTP R14B">>}).
-spec stop(HostOrNode, NodeOrOpts) -> {'ok', Node} | {'error', Reason, Node}
              when HostOrNode :: atom() | Node,
                   Node :: node(),
                   NodeOrOpts :: Node | stop_options(),
                   Reason :: atom().
stop(_HostOrNode = Node, _NodeOrOpts = Opts) %% match to satiate edoc
  when is_list(Opts) ->
    stop(gethostname(), Node, Opts);

stop(Host, Node) ->
    stop(Host, Node, []).

stop(Host, Node, Opts) ->
    ENode = enodename(Host, Node),
    case is_started(ENode) of
	{true, connected}->
	     OptionsRec = fetch_options(Opts),
	     do_stop(ENode, OptionsRec);
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
    StopTimeout = get_option_value(stop_timeout, Options, 5),
    #options{username=UserName, password=Password,
	     boot_timeout=BootTimeout, init_timeout=InitTimeout,
	     startup_timeout=StartupTimeout, startup_functions=StartupFunctions,
	     monitor_master=Monitor, kill_if_fail=KillIfFail,
	     erl_flags=ErlFlags, env=EnvVars, ssh_port=SSHPort, ssh_opts=SSHOpts,
	     stop_timeout=StopTimeout}.

% send a message when slave node is started
-doc false.
slave_started(ENode, MasterPid) ->
    MasterPid ! {node_started, ENode},
    ok.

% send a message when slave node has finished startup
-doc false.
slave_ready(ENode, MasterPid) ->
    MasterPid ! {node_ready, ENode},
    ok.

% start monitoring of the master node
-doc false.
monitor_master(MasterNode) ->
    spawn(fun() -> monitor_master_int(MasterNode) end).

% code of the masterdeath-waiter process
monitor_master_int(MasterNode) ->
    ct_util:mark_process(),
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
    case lists:member($@, atom_to_list(Node)) of
        true ->
            Node;
        false ->
            list_to_atom(atom_to_list(Node)++"@"++atom_to_list(Host))
    end.

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
    do_stop(ENode, fetch_options([])).
do_stop(ENode, Options) ->
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
    StopTimeout = Options#options.stop_timeout,
    case wait_for_node_dead(ENode, StopTimeout) of
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
