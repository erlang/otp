%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
pseudo([Master | ServerList]) ->
    pseudo(Master , ServerList);
pseudo(_) ->
    error_msg("No master node given to slave:pseudo/1~n",[]).

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

-spec start(Host) -> {ok, Node} | {error, Reason} when
      Host :: inet:hostname(),
      Node :: node(),
      Reason :: timeout | no_rsh | {already_running, Node}.

start(Host) ->
    L = atom_to_list(node()),
    Name = upto($@, L),
    start(Host, Name, [], no_link).

-spec start(Host, Name) -> {ok, Node} | {error, Reason} when
      Host :: inet:hostname(),
      Name :: atom() | string(),
      Node :: node(),
      Reason :: timeout | no_rsh | {already_running, Node}.

start(Host, Name) ->
    start(Host, Name, []).

-spec start(Host, Name, Args) -> {ok, Node} | {error, Reason} when
      Host :: inet:hostname(),
      Name :: atom() | string(),
      Args :: string(),
      Node :: node(),
      Reason :: timeout | no_rsh | {already_running, Node}.

start(Host, Name, Args) ->
    start(Host, Name, Args, no_link).

-spec start_link(Host) -> {ok, Node} | {error, Reason} when
      Host :: inet:hostname(),
      Node :: node(),
      Reason :: timeout | no_rsh | {already_running, Node}.

start_link(Host) ->
    L = atom_to_list(node()),
    Name = upto($@, L),
    start(Host, Name, [], self()).

-spec start_link(Host, Name) -> {ok, Node} | {error, Reason} when
      Host :: inet:hostname(),
      Name :: atom() | string(),
      Node :: node(),
      Reason :: timeout | no_rsh | {already_running, Node}.

start_link(Host, Name) ->
    start_link(Host, Name, []).

-spec start_link(Host, Name, Args) -> {ok, Node} | {error, Reason} when
      Host :: inet:hostname(),
      Name :: atom() | string(),
      Args :: string(),
      Node :: node(),
      Reason :: timeout | no_rsh | {already_running, Node}.

start_link(Host, Name, Args) ->
    start(Host, Name, Args, self()).

start(Host0, Name, Args, LinkTo) ->
    Prog = lib:progname(),
    start(Host0, Name, Args, LinkTo, Prog).

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
%% no need to use rsh.

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

%% This is an attempt to distinguish between spaces in the program
%% path and spaces that separate arguments. The program is quoted to
%% allow spaces in the path.
%%
%% Arguments could exist either if the executable is excplicitly given
%% (through start/5) or if the -program switch to beam is used and
%% includes arguments (typically done by cerl in OTP test environment
%% in order to ensure that slave/peer nodes are started with the same
%% emulator and flags as the test node. The return from lib:progname()
%% could then typically be '/<full_path_to>/cerl -gcov').
quote_progname(Progname) ->
    do_quote_progname(string:tokens(to_list(Progname)," ")).

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

%% Give the user an opportunity to run another program,
%% than the "rsh".  On HP-UX rsh is called remsh; thus HP users
%% must start erlang as erl -rsh remsh.
%%
%% Also checks that the given program exists.
%%
%% Returns: {ok, RshPath} | {error, Reason}

rsh() ->
    Rsh =
	case init:get_argument(rsh) of
	    {ok, [[Prog]]} -> Prog;
	    _ -> "rsh"
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

slave_start([Master, Waiter]) ->
    ?dbg({?MODULE, slave_start}, [[Master, Waiter]]),
    spawn(?MODULE, wait_for_master_to_die, [Master, Waiter]).

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
