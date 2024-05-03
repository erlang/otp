%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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
-module(slave).
-moduledoc """
This module provides functions for starting Erlang slave nodes.

All slave nodes that are started by a master terminate automatically when the
master terminates. All terminal output produced at the slave is sent back to
the master node. File I/O is done through the master.

Slave nodes on other hosts than the current one are started with the `ssh`
program. The user must be allowed to `ssh` to the remote hosts without being
prompted for a password. This can be arranged in a number of ways (for details,
see the `ssh` documentation). A slave node started on the same host as the
master inherits certain environment values from the master, such as the current
directory and the environment variables. For what can be assumed about the
environment when a slave is started on another host, see the documentation for
the `ssh` program.

An alternative to the `ssh` program can be specified on the command line to
[`erl(1)`](`e:erts:erl_cmd.md`) as follows:

```text
-rsh Program
```

Note that the command specified with the `-rsh` flag is treated as a file name
which may contain spaces. It is thus not possible to include any command line
options. The remote node will be launched as
`"$RSH" "$REMOTE_HOSTNAME" erl -detached -noinput ...`, so the `erl` command
must be found in the path on the remote host.

The slave node is to use the same file system at the master. At least,
Erlang/OTP is to be installed in the same place on both computers and the same
version of Erlang is to be used.

A node running on Windows can only start slave nodes on the host on which it is
running.

The master node must be alive.
""".

-deprecated([{'_','_',"use the 'peer' module instead"}]).

%% If the macro DEBUG is defined during compilation, 
%% debug printouts are done through erlang:display/1.
%% Activate this feature by starting the compiler 
%% with> erlc -DDEBUG ... 
%% or by> setenv ERL_COMPILER_FLAGS DEBUG 
%% before running make (in the OTP make system)
%% (the example is for tcsh)


-export([pseudo/1,
	 pseudo/2,
	 start/1, start/2, start/3,
	 start/5,
	 start_link/1, start_link/2, start_link/3,
	 stop/1,
	 relay/1]).

%% Internal exports 
-export([wait_for_slave/7, slave_start/1, wait_for_master_to_die/2]).

-import(error_logger, [error_msg/2]).


-ifdef(DEBUG).
-define(dbg(Tag,Data), erlang:display({Tag,Data})).
-else.
-define(dbg(Tag,Data), true).
-endif.


%% Start a list of pseudo servers on the local node
-doc """
pseudo([Master | ServerList])

Calls [`pseudo(Master, ServerList)`](`pseudo/2`). If you want to start a node
from the command line and set up a number of pseudo servers, an Erlang runtime
system can be started as follows:

```text
% erl -name abc -s slave pseudo klacke@super x --
```
""".
-spec pseudo([Master :: node() | ServerList :: [atom()]]) -> ok.
pseudo([Master | ServerList]) ->
    pseudo(Master , ServerList);
pseudo(_) ->
    error_msg("No master node given to slave:pseudo/1~n",[]).

-doc """
Starts a number of pseudo servers. A pseudo server is a server with a registered
name that does nothing but pass on all message to the real server that executes
at a master node. A pseudo server is an intermediary that only has the same
registered name as the real server.

For example, if you have started a slave node `N` and want to execute `pxw`
graphics code on this node, you can start server `pxw_server` as a pseudo server
at the slave node. This is illustrated as follows:

```erlang
rpc:call(N, slave, pseudo, [node(), [pxw_server]]).
```
""".
-spec pseudo(Master, ServerList) -> ok when
      Master :: node(),
      ServerList :: [atom()].

pseudo(_, []) -> ok;
pseudo(Master, [S|Tail]) ->
    start_pseudo(S, whereis(S), Master),
    pseudo(Master, Tail).

start_pseudo(Name, undefined, Master) ->
    X = rpc:call(Master,erlang, whereis,[Name]),
    register(Name, spawn(slave, relay, [X]));

start_pseudo(_,_,_) -> ok.  %% It's already there


%% This relay can be used to relay all messages directed to a process.

-doc """
Runs a pseudo server. This function never returns any value and the process that
executes the function receives messages. All messages received are simply passed
on to `Pid`.
""".
-spec relay(Pid) -> no_return() when
      Pid :: pid().

relay({badrpc,Reason}) ->
    error_msg(" ** exiting relay server ~w :~tw  **~n", [self(),Reason]),
    exit(Reason);
relay(undefined) ->
    error_msg(" ** exiting relay server ~w  **~n", [self()]),
    exit(undefined);
relay(Pid) when is_pid(Pid) ->
    relay1(Pid).

relay1(Pid) ->
    receive
        X ->
            Pid ! X
    end,
    relay1(Pid).

%% start/1,2,3 --
%% start_link/1,2,3 --
%%
%% The start/1,2,3 functions are used to start a slave Erlang node.
%% The node on which the start/N functions are used is called the
%% master in the description below.
%%
%% If hostname is the same for the master and the slave,
%% the Erlang node will simply be spawned.  The only requirment for
%% this to work is that the 'erl' program can be found in PATH.
%%
%% If the master and slave are on different hosts, start/N uses
%% the 'ssh' program to spawn an Erlang node on the other host.
%% Alternative, if the master was started as
%% 'erl -sname xxx -rsh my_rsh...', then 'my_rsh' will be used instead
%% of 'ssh' (this is useful for systems still using rsh or remsh).
%%
%% For this to work, the following conditions must be fulfilled:
%%
%% 1. There must be an ssh program on computer; if not an error
%%    is returned.
%%
%% 2. The hosts must be configured to allow 'ssh' access without
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

-doc """
Equivalent to [`start(Host, Name)`](`start/2`) where `Name` is the same
as the node that executes this call.
""".
-spec start(Host) -> {ok, Node} | {error, Reason} when
      Host :: inet:hostname(),
      Node :: node(),
      Reason :: timeout | no_rsh | {already_running, Node}.

start(Host) ->
    L = atom_to_list(node()),
    Name = upto($@, L),
    start(Host, Name, [], no_link).

-doc(#{equiv => start(Host, Name, [])}).
-spec start(Host, Name) -> {ok, Node} | {error, Reason} when
      Host :: inet:hostname(),
      Name :: atom() | string(),
      Node :: node(),
      Reason :: timeout | no_rsh | {already_running, Node}.

start(Host, Name) ->
    start(Host, Name, []).

-doc """
Starts a slave node on host `Host`. Host names need not necessarily be specified
as fully qualified names; short names can also be used. This is the same
condition that applies to names of distributed Erlang nodes.

The name of the started node becomes `Name@Host`.

The slave node resets its `t:io:user/0` process so that all terminal I/O that is
produced at the slave is automatically relayed to the master. Also, the file
server is relayed to the master.

Argument `Args` is used to set `erl` command-line arguments. It is
passed to the new node and can be used for a variety of purposes; see
[`erl(1)`](`e:erts:erl_cmd.md`).

As an example, suppose that you want to start a slave node at host `H` with node
name `Name@H` and want the slave node to have the following properties:

- Directory `Dir` is to be added to the code path.
- The Mnesia directory is to be set to `M`.
- The Unix `DISPLAY` environment variable is to be set to the display of the
  master node.

The following code is executed to achieve this:

```erlang
E = " -env DISPLAY " ++ net_adm:localhost() ++ ":0 ",
Arg = "-mnesia_dir " ++ M ++ " -pa " ++ Dir ++ E,
slave:start(H, Name, Arg).
```

The function returns `{ok, Node}`, where `Node` is the name of the new node,
otherwise `{error, Reason}`, where `Reason` can be one of:

- **`timeout`** - The master node failed to get in contact with the slave node.
  This can occur in a number of circumstances:

  - Erlang/OTP is not installed on the remote host.
  - The file system on the other host has a different structure to the the
    master.
  - The Erlang nodes have different cookies.

- **`no_rsh`** - No remote shell program was found on the computer. Note that
  `ssh` is used by default, but this can be overridden with the `-rsh` flag.

- **`{already_running, Node}`** - A node with name `Name@Host` already exists.
""".
-spec start(Host, Name, Args) -> {ok, Node} | {error, Reason} when
      Host :: inet:hostname(),
      Name :: atom() | string(),
      Args :: string(),
      Node :: node(),
      Reason :: timeout | no_rsh | {already_running, Node}.

start(Host, Name, Args) ->
    start(Host, Name, Args, no_link).

-doc(#{equiv => start_link/3}).
-spec start_link(Host) -> {ok, Node} | {error, Reason} when
      Host :: inet:hostname(),
      Node :: node(),
      Reason :: timeout | no_rsh | {already_running, Node}.

start_link(Host) ->
    L = atom_to_list(node()),
    Name = upto($@, L),
    start(Host, Name, [], self()).

-doc(#{equiv => start_link/3}).
-spec start_link(Host, Name) -> {ok, Node} | {error, Reason} when
      Host :: inet:hostname(),
      Name :: atom() | string(),
      Node :: node(),
      Reason :: timeout | no_rsh | {already_running, Node}.

start_link(Host, Name) ->
    start_link(Host, Name, []).

-doc """
Starts a slave node in the same way as `start/1,2,3`, except that the slave node
is linked to the currently executing process. If that process terminates, the
slave node also terminates.

For a description of arguments and return values, see
[`start/1,2,3`](`start/1`).
""".
-spec start_link(Host, Name, Args) -> {ok, Node} | {error, Reason} when
      Host :: inet:hostname(),
      Name :: atom() | string(),
      Args :: string(),
      Node :: node(),
      Reason :: timeout | no_rsh | {already_running, Node}.

start_link(Host, Name, Args) ->
    start(Host, Name, Args, self()).

start(Host0, Name, Args, LinkTo) ->
    Prog = progname(),
    start(Host0, Name, Args, LinkTo, Prog).

-doc false.
start(Host0, Name, Args, LinkTo, Prog) ->
    Host =
	case net_kernel:longnames() of
	    true -> dns(Host0);
	    false -> strip_host_name(to_list(Host0));
	    ignored -> exit(not_alive)
	end,
    Node = list_to_atom(lists:concat([Name, "@", Host])),
    case net_adm:ping(Node) of
	pang ->
	    start_it(Host, Name, Node, Args, LinkTo, Prog);
	pong -> 
	    {error, {already_running, Node}}
    end.

%% Stops a running node.

-doc "Stops (kills) a node.".
-spec stop(Node) -> ok when
      Node :: node().

stop(Node) ->
    rpc:call(Node, erlang, halt, []),
    ok.

%% Starts a new slave node.

start_it(Host, Name, Node, Args, LinkTo, Prog) ->
    spawn(?MODULE, wait_for_slave, [self(), Host, Name, Node, Args, LinkTo,
				    Prog]),
    receive
	{result, Result} -> Result
    end.

%% Waits for the slave to start.

-doc false.
wait_for_slave(Parent, Host, Name, Node, Args, LinkTo, Prog) ->
    Waiter = register_unique_name(0),
    case mk_cmd(Host, Name, Args, Waiter, Prog) of
	{ok, Cmd} ->
	    open_port({spawn, Cmd}, [stream]),
	    receive
		{SlavePid, slave_started} ->
		    unregister(Waiter),
		    slave_started(Parent, LinkTo, SlavePid)
	    after 32000 ->
		    %% If it seems that the node was partially started,
		    %% try to kill it.
		    Node = list_to_atom(lists:concat([Name, "@", Host])),
		    case net_adm:ping(Node) of
			pong ->
			    spawn(Node, erlang, halt, []),
			    ok;
			_ ->
			    ok
		    end,
		    Parent ! {result, {error, timeout}}
	    end;
	Other ->
	    Parent ! {result, Other}
    end.

slave_started(ReplyTo, no_link, Slave) when is_pid(Slave) ->
    ReplyTo ! {result, {ok, node(Slave)}};
slave_started(ReplyTo, Master, Slave) when is_pid(Master), is_pid(Slave) ->
    process_flag(trap_exit, true),
    link(Master),
    link(Slave),
    ReplyTo ! {result, {ok, node(Slave)}},
    one_way_link(Master, Slave).

%% This function simulates a one-way link, so that the slave node
%% will be killed if the master process terminates, but the master
%% process will not be killed if the slave node terminates.

one_way_link(Master, Slave) ->
    receive
	{'EXIT', Master, _Reason} ->
	    unlink(Slave),
	    Slave ! {nodedown, node()};
	{'EXIT', Slave, _Reason} ->
	    unlink(Master);
	_Other ->
	    one_way_link(Master, Slave)
    end.

register_unique_name(Number) ->
    Name = list_to_atom(lists:concat(["slave_waiter_", Number])),
    case catch register(Name, self()) of
	true ->
	    Name;
	{'EXIT', {badarg, _}} ->
	    register_unique_name(Number+1)
    end.

%% Makes up the command to start the nodes.
%% If the node should run on the local host, there is
%% no need to use a remote shell.

mk_cmd(Host, Name, Args, Waiter, Prog0) ->
    Prog = quote_progname(Prog0),
    BasicCmd = lists:concat([Prog,
			     " -detached -noinput -master ", node(),
			     " ", long_or_short(), Name, "@", Host,
			     " -s slave slave_start ", node(),
			     " ", Waiter,
			     " ", Args]),
    case after_char($@, atom_to_list(node())) of
	Host ->
	    {ok, BasicCmd};
	_ ->
	    case rsh() of
		{ok, Rsh} ->
		    {ok, lists:concat([Rsh, " ", Host, " ", BasicCmd])};
		Other ->
		    Other
	    end
    end.

%% Return the name of the script that starts (this) erlang
progname() ->
    case init:get_argument(progname) of
	{ok, [[Prog]]} ->
	    Prog;
	_Other ->
	    "no_prog_name"
    end.

%% This is an attempt to distinguish between spaces in the program
%% path and spaces that separate arguments. The program is quoted to
%% allow spaces in the path.
%%
%% Arguments could exist either if the executable is excplicitly given
%% (through start/5) or if the -program switch to beam is used and
%% includes arguments (typically done by cerl in OTP test environment
%% in order to ensure that slave/peer nodes are started with the same
%% emulator and flags as the test node. The result from progname()
%% could then typically be '/<full_path_to>/cerl -gcov').
quote_progname(Progname) ->
    do_quote_progname(string:lexemes(to_list(Progname)," ")).

do_quote_progname([Prog]) ->
    "\""++Prog++"\"";
do_quote_progname([Prog,Arg|Args]) ->
    case os:find_executable(Prog) of
	false ->
	    do_quote_progname([Prog++" "++Arg | Args]);
	_ ->
	    %% this one has an executable - we assume the rest are arguments
	    "\""++Prog++"\""++
		lists:flatten(lists:map(fun(X) -> [" ",X] end, [Arg|Args]))
    end.

%% Give the user an opportunity to run another program than "ssh".
%%
%% Also checks that the given program exists.
%%
%% Returns: {ok, RshPath} | {error, Reason}

rsh() ->
    Rsh =
	case init:get_argument(rsh) of
	    {ok, [[Prog]]} -> Prog;
	    _ -> "ssh"
	end,
    case os:find_executable(Rsh) of
	false -> {error, no_rsh};
	Path -> {ok, Path}
    end.

long_or_short() -> 
    case net_kernel:longnames() of
	true -> " -name ";
	false -> " -sname "
    end.

%% This function will be invoked on the slave, using the -s option of erl.
%% It will wait for the master node to terminate.

-doc false.
slave_start([Master, Waiter]) ->
    ?dbg({?MODULE, slave_start}, [[Master, Waiter]]),
    spawn(?MODULE, wait_for_master_to_die, [Master, Waiter]).

-doc false.
wait_for_master_to_die(Master, Waiter) ->
    ?dbg({?MODULE, wait_for_master_to_die}, [Master, Waiter]),
    process_flag(trap_exit, true),
    monitor_node(Master, true),
    {Waiter, Master} ! {self(), slave_started},
    wloop(Master).

wloop(Master) ->
    receive
	{nodedown, Master} ->
	    ?dbg({?MODULE, wloop}, 
		 [[Master], {received, {nodedown, Master}}, halting_node] ),
	    halt();
	_Other ->
	    wloop(Master)
    end.

%% Just the short hostname, not the qualified, for convenience.

strip_host_name([]) -> [];
strip_host_name([$.|_]) -> [];
strip_host_name([H|T]) -> [H|strip_host_name(T)].

dns(H) -> {ok, Host} = net_adm:dns_hostname(H), Host.

to_list(X) when is_list(X) -> X;
to_list(X) when is_atom(X) -> atom_to_list(X).

upto(_, []) -> [];
upto(Char, [Char|_]) -> [];
upto(Char, [H|T]) -> [H|upto(Char, T)].

after_char(_, []) -> [];
after_char(Char, [Char|Rest]) -> Rest;
after_char(Char, [_|Rest]) -> after_char(Char, Rest).
