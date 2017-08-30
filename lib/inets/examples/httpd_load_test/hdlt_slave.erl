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
-module(hdlt_slave).


-export([start_link/4, start_link/5, start_link/6, stop/1]).

%% Internal exports 
-export([wait_for_slave/9, slave_start/1, wait_for_master_to_die/3]).

-include("hdlt_logger.hrl").

-define(SSH_PORT, 22).
-define(TIMEOUT,  60000).
-define(LOGGER,   hdlt_logger).


%% ***********************************************************************
%% start_link/4,5 --
%%
%% The start/4,5 functions are used to start a slave Erlang node.
%% The node on which the start/N functions are used is called the
%% master in the description below.
%%
%% If hostname is the same for the master and the slave,
%% the Erlang node will simply be spawned.  The only requirment for
%% this to work is that the 'erl' program can be found in PATH.
%%
%% If the master and slave are on different hosts, start/N uses
%% the 'rsh' program to spawn an Erlang node on the other host.
%% Alternative, if the master was started as
%% 'erl -sname xxx -rsh my_rsh...', then 'my_rsh' will be used instead
%% of 'rsh' (this is useful for systems where the rsh program is named
%% 'remsh').
%%
%% For this to work, the following conditions must be fulfilled:
%%
%% 1. There must be an Rsh program on computer; if not an error
%%    is returned.
%%
%% 2. The hosts must be configured to allowed 'rsh' access without
%%    prompts for password.
%%
%% The slave node will have its filer and user server redirected
%% to the master.  When the master node dies, the slave node will
%% terminate.  For the start_link functions, the slave node will
%% terminate also if the process which called start_link terminates.
%%
%% Returns: {ok, Name@Host} |
%%	    {error, timeout} |
%%          {error, no_rsh} |
%%	    {error, {already_running, Name@Host}}

start_link(Host, Name, ErlPath, Paths) ->
    start_link(Host, Name, ErlPath, Paths, [], silence).

start_link(Host, Name, ErlPath, Paths, DebugLevel) when is_atom(DebugLevel) ->
    start_link(Host, Name, ErlPath, Paths, [], DebugLevel);
start_link(Host, Name, ErlPath, Paths, Args) when is_list(Args) ->
    start_link(Host, Name, ErlPath, Paths, Args, silence).

start_link(Host, Name, ErlPath, Paths, Args, DebugLevel) ->
    Node = list_to_atom(lists:concat([Name, "@", Host])),
    case net_adm:ping(Node) of
	pang ->
	    start_it(Host, Name, Node, ErlPath, Paths, Args, DebugLevel);
	pong -> 
	    {error, {already_running, Node}}
    end.

%% Stops a running node.

stop(Node) ->
    rpc:call(Node, erlang, halt, []),
    ok.


%% Starts a new slave node.

start_it(Host, Name, Node, ErlPath, Paths, Args, DebugLevel) ->
    Prog = filename:join([ErlPath, "erl"]), 
    spawn(?MODULE, wait_for_slave, [self(), Host, Name, Node, Paths, Args, self(), Prog, DebugLevel]),
    receive
	{result, Result} -> Result
    end.

%% Waits for the slave to start.

wait_for_slave(Parent, Host, Name, Node, Paths, Args, 
	       LinkTo, Prog, DebugLevel) ->
    ?SET_NAME("HDLT SLAVE STARTER"), 
    ?SET_LEVEL(DebugLevel),
    ?DEBUG("begin", []),
    Waiter = register_unique_name(0),
    case mk_cmd(Host, Name, Paths, Args, Waiter, Prog) of
	{ok, Cmd} ->
  	    ?DEBUG("command generated: ~n~s", [Cmd]),
	    case (catch ssh_slave_start(Host, Cmd)) of
		{ok, Conn, _Chan} ->
 		    ?DEBUG("ssh channel created", []),
		    receive
			{SlavePid, slave_started} ->
 			    ?DEBUG("slave started: ~p", [SlavePid]),
			    unregister(Waiter),
			    slave_started(Parent, LinkTo, SlavePid, Conn, 
					  DebugLevel)
		    after 32000 ->
			    ?INFO("slave node failed to report in on time", 
				  []),
			    %% If it seems that the node was partially started,
			    %% try to kill it.
			    case net_adm:ping(Node) of
				pong ->
				    spawn(Node, erlang, halt, []),
				    ok;
				_ ->
				    ok
			    end,
			    Parent ! {result, {error, timeout}}
		    end;
		{error, Reason} = Error ->
		    ?INFO("FAILED starting node: "
			  "~n   ~p"
			  "~n   ~p", [Reason, Cmd]),
		    Parent ! {result, Error}
	    end;
	Other ->
	    ?INFO("FAILED creating node command string: "
		  "~n   ~p", [Other]),
	    Parent ! {result, Other}
    end.


ssh_slave_start(Host, ErlCmd) ->
    ?DEBUG("ssh_slave_start -> try connect to ~p", [Host]),
    Connection = 
	case (catch ssh:connect(Host, ?SSH_PORT, 
				[{silently_accept_hosts, true}])) of
	    {ok, Conn} ->
 		?DEBUG("ssh_exec_erl -> connected: ~p", [Conn]),
		Conn;
	    Error1 ->
 		?LOG("failed connecting to ~p: ~p", [Host, Error1]),
		throw({error, {ssh_connect_failed, Error1}})
	end,

    ?DEBUG("ssh_exec_erl -> connected - now create channel", []), 
    Channel = 
	case (catch ssh_connection:session_channel(Connection, ?TIMEOUT)) of
	    {ok, Chan} ->
 		?DEBUG("ssh_exec_erl -> channel ~p created", [Chan]),
		Chan;
	    Error2 ->
 		?LOG("failed creating channel: ~p", [Error2]),
		throw({error, {ssh_channel_create_failed, Error2}})
	end,

    ?DEBUG("ssh_exec_erl -> channel created - now exec command: "
 	"~n   ~p", [ErlCmd]), 
    case (catch ssh_connection:exec(Connection, Channel, ErlCmd, infinity)) of
	success ->
	    ?DEBUG("ssh_exec_erl -> command exec'ed - clean ssh msg", []), 
	    clean_ssh_msg(),
 	    ?DEBUG("ssh_exec_erl -> done", []), 
	    {ok, Connection, Channel};
	Error3 ->
	    ?LOG("failed exec command: ~p", [Error3]),
	    throw({error, {ssh_exec_failed, Error3}})
    end.

clean_ssh_msg() ->
    receive
	{ssh_cm, _X, _Y} ->
	    clean_ssh_msg()
    after 1000 ->
	    ok
    end.
    

slave_started(ReplyTo, Master, Slave, Conn, Level) 
  when is_pid(Master) andalso is_pid(Slave) ->
    process_flag(trap_exit, true),
    SName = lists:flatten(
	      io_lib:format("HDLT SLAVE CTRL[~p,~p]", 
			    [self(), node(Slave)])), 
    ?SET_NAME(SName),
    ?SET_LEVEL(Level), 
    ?LOG("initiating", []),
    MasterRef = erlang:monitor(process, Master),
    SlaveRef  = erlang:monitor(process, Slave),
    ReplyTo ! {result, {ok, node(Slave)}},
    slave_running(Master, MasterRef, Slave, SlaveRef, Conn).


%% The slave node will be killed if the master process terminates, 
%% The master process will not be killed if the slave node terminates.

slave_running(Master, MasterRef, Slave, SlaveRef, Conn) ->
    ?DEBUG("await message", []),
    receive
	{'DOWN', MasterRef, process, _Object, _Info} ->
	    ?LOG("received DOWN from master", []),
	    erlang:demonitor(SlaveRef, [flush]),
	    Slave ! {nodedown, node()},
	    ssh:close(Conn);

	{'DOWN', SlaveRef, process, Object, _Info} ->
	    ?LOG("received DOWN from slave (~p)", [Object]),
	    erlang:demonitor(MasterRef, [flush]),
	    ssh:close(Conn);

	Other ->
	    ?DEBUG("received unknown: ~n~p", [Other]),
	    slave_running(Master, MasterRef, Slave, SlaveRef, Conn)

    end.

register_unique_name(Number) ->
    Name = list_to_atom(lists:concat([?MODULE, "_waiter_", Number])),
    case catch register(Name, self()) of
	true ->
	    Name;
	{'EXIT', {badarg, _}} ->
	    register_unique_name(Number+1)
    end.


%% Makes up the command to start the nodes.
%% If the node should run on the local host, there is
%% no need to use rsh.

mk_cmd(Host, Name, Paths, Args, Waiter, Prog) ->
    PaPaths = [[" -pa ", Path] || Path <- Paths], 
    {ok, lists:flatten(
	   lists:concat([Prog,
			 " -detached -nopinput ", 
			 Args, " ", 
			 " -sname ", Name, "@", Host,
			 " -s ", ?MODULE, " slave_start ", node(),
			 " ", Waiter,
			 " ", PaPaths]))}.


%% This function will be invoked on the slave, using the -s option of erl.
%% It will wait for the master node to terminate.

slave_start([Master, Waiter]) ->
    spawn(?MODULE, wait_for_master_to_die, [Master, Waiter, silence]);
slave_start([Master, Waiter, Level]) ->
    spawn(?MODULE, wait_for_master_to_die, [Master, Waiter, Level]).
	

wait_for_master_to_die(Master, Waiter, Level) ->
    process_flag(trap_exit, true),
    SName = lists:flatten(
	      io_lib:format("HDLT-SLAVE MASTER MONITOR[~p,~p,~p]", 
			    [self(), node(), Master])), 
    ?SET_NAME(SName),
    ?SET_LEVEL(Level), 
    erlang:monitor_node(Master, true),
    {Waiter, Master} ! {self(), slave_started},
    wloop(Master).

wloop(Master) ->
    ?DEBUG("await message", []),
    receive
	{nodedown, Master} ->
	    ?INFO("received master nodedown", []),
	    halt();
	_Other ->
	    wloop(Master)
    end.



