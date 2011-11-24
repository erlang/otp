%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
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
-module(heart_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, start/1, restart/1, 
	 reboot/1, set_cmd/1, clear_cmd/1, get_cmd/1,
	 dont_drop/1, kill_pid/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

-export([start_heart_stress/1, mangle/1, suicide_by_heart/0]).

-define(DEFAULT_TIMEOUT_SECS, 120).

init_per_testcase(_Func, Config) ->
    Dog=test_server:timetrap(test_server:seconds(?DEFAULT_TIMEOUT_SECS)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Func, Config) ->
    Nodes = nodes(),
    lists:foreach(fun(X) ->
			  NNam = list_to_atom(hd(string:tokens(atom_to_list(X),"@"))),
			  case NNam of
			      heart_test ->
				  ?t:format(1, "WARNING: Killed ~p~n", [X]),
				  rpc:cast(X, erlang, halt, []);
			      _ ->
				  ok
			  end
		  end, Nodes),
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog).

%%-----------------------------------------------------------------
%% Test suite for heart.
%% Should be started in a CC view with:
%% erl -sname master -rsh ctrsh
%%-----------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [start, restart, reboot, set_cmd, clear_cmd, get_cmd, kill_pid].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_suite(Config) when is_list(Config) ->
    case os:type() of
	{win32, windows} ->
	    {skipped, "No use to run on Windows 95/98"};
	_ ->
	    Config
    end.
end_per_suite(Config) when is_list(Config) ->
    Config.

start_check(Type, Name) ->
    Args = case ?t:os_type() of
	       {win32,_} -> "-heart -env HEART_COMMAND no_reboot";
	       _ -> "-heart"
	   end,
    {ok, Node} = case Type of
		     loose ->
			 loose_node:start(Name, Args, ?DEFAULT_TIMEOUT_SECS);
		     _ ->
			 ?t:start_node(Name, Type, [{args, Args}])
		 end,
    erlang:monitor_node(Node, true),
    case rpc:call(Node, erlang, whereis, [heart]) of
	Pid when is_pid(Pid) ->
	    ok;
	_ ->
	    test_server:fail(heart_not_started)
    end,
    {ok, Node}.

start(doc) -> [];
start(suite) -> {req, [{time, 10}]};
start(Config) when is_list(Config) ->
    ?line {ok, Node} = start_check(slave, heart_test),
    ?line rpc:call(Node, init, reboot, []),
    receive
	{nodedown, Node} ->
	    ok
    after 2000 ->
	    test_server:fail(node_not_closed)
    end,
    test_server:sleep(5000),
    ?line case net_adm:ping(Node) of
	      pang ->
		  ok;
	      _ -> 
		  test_server:fail(node_rebooted)
	  end,
    test_server:stop_node(Node).

%% Also test fixed bug in R1B (it was not possible to
%% do init:stop/0 on a restarted system before)
%% Slave executes erlang:halt() on master nodedown.
%% Therefore the slave process has to be killed
%% before restart.
restart(doc) -> [];
restart(suite) -> 
   case ?t:os_type() of
	{Fam, _} when Fam == unix; Fam == win32 ->
	    {req, [{time,10}]};
	_ ->
	    {skip, "Only run on unix and win32"}
    end;
restart(Config) when is_list(Config) ->
    ?line {ok, Node} = start_check(loose, heart_test),
    ?line rpc:call(Node, init, restart, []),
    receive
	{nodedown, Node} ->
	    ok
    after 2000 ->
	    test_server:fail(node_not_closed)
    end,
    test_server:sleep(5000),

    ?line case net_adm:ping(Node) of
	      pong ->
		  erlang:monitor_node(Node, true),
		  ?line rpc:call(Node, init, stop, []),
		  receive
		      {nodedown, Node} ->
			  ok
		  after 2000 ->
			  test_server:fail(node_not_closed2)
		  end,
		  ok;
	      _ ->
		  test_server:fail(node_not_restarted)
	  end,
    loose_node:stop(Node).

reboot(doc) -> [];
reboot(suite) -> {req, [{time, 10}]};
reboot(Config) when is_list(Config) ->
    {ok, Node} = start_check(slave, heart_test),

    ?line ok = rpc:call(Node, heart, set_cmd,
			[atom_to_list(lib:progname()) ++ 
			 " -noshell -heart " ++ name(Node) ++ "&"]),
    ?line rpc:call(Node, init, reboot, []),
    receive
	{nodedown, Node} ->
	    ok
    after 2000 ->
	    test_server:fail(node_not_closed)
    end,
    test_server:sleep(5000),
    ?line case net_adm:ping(Node) of
	      pong ->
		  erlang:monitor_node(Node, true),
		  ?line rpc:call(Node, init, reboot, []),
		  receive
		      {nodedown, Node} ->
			  ok
		  after 2000 ->
			  test_server:fail(node_not_closed2)
		  end,
		  ok;
	      _ ->
		  test_server:fail(node_not_rebooted)
	  end,
    ok.

%% Only tests bad command, correct behaviour is tested in reboot/1.
set_cmd(suite) -> [];
set_cmd(Config) when is_list(Config) ->
    ?line {ok, Node} = start_check(slave, heart_test),
    Cmd = wrong_atom,
    ?line {error, {bad_cmd, Cmd}} = rpc:call(Node, heart, set_cmd, [Cmd]),
    Cmd1 = lists:duplicate(2047, $a),
    ?line {error, {bad_cmd, Cmd1}} = rpc:call(Node, heart, set_cmd, [Cmd1]),
    Cmd2 = lists:duplicate(28, $a),
    ?line ok = rpc:call(Node, heart, set_cmd, [Cmd2]),
    Cmd3 = lists:duplicate(2000, $a),
    ?line ok = rpc:call(Node, heart, set_cmd, [Cmd3]),
    stop_node(Node),
    ok.

clear_cmd(suite) -> {req,[{time,15}]};
clear_cmd(Config) when is_list(Config) ->
    ?line {ok, Node} = start_check(slave, heart_test),
    ?line ok = rpc:call(Node, heart, set_cmd,
			[atom_to_list(lib:progname()) ++
			 " -noshell -heart " ++ name(Node) ++ "&"]),
    ?line rpc:call(Node, init, reboot, []),
    receive
	{nodedown, Node} ->
	    ok
    after 2000 ->
	    test_server:fail(node_not_closed)
    end,
    test_server:sleep(5000),
    ?line case net_adm:ping(Node) of
	      pong ->
		  erlang:monitor_node(Node, true);
	      _ ->
		  test_server:fail(node_not_rebooted)
	  end,
    ?line ok = rpc:call(Node, heart, set_cmd,
			["erl -noshell -heart " ++ name(Node) ++ "&"]),
    ?line ok = rpc:call(Node, heart, clear_cmd, []),
    ?line rpc:call(Node, init, reboot, []),
    receive
	{nodedown, Node} ->
	    ok
    after 2000 ->
	    test_server:fail(node_not_closed)
    end,
    test_server:sleep(5000),
    ?line case net_adm:ping(Node) of
	      pang ->
		  ok;
	      _ -> 
		  test_server:fail(node_rebooted)
	  end,
    ok.

get_cmd(suite) -> [];
get_cmd(Config) when is_list(Config) ->
    ?line {ok, Node} = start_check(slave, heart_test),
    Cmd = "test",
    ?line ok = rpc:call(Node, heart, set_cmd, [Cmd]),
    ?line {ok, Cmd} = rpc:call(Node, heart, get_cmd, []),
    stop_node(Node),
    ok.

dont_drop(suite) -> 
%%% Removed as it may crash epmd/distribution in colourful
%%% ways. While we ARE finding out WHY, it would
%%% be nice for others to be able to run the kernel test suite
%%% without "exploding machines", so thats why I removed it for now.
    [];
dont_drop(doc) ->
    ["Tests that the heart command does not get dropped when ",
     "set just before halt on very high I/O load."];
dont_drop(Config) when is_list(Config) ->
    %%% Have to do it some times to make it happen...
    case os:type() of
	vxworks ->
	    {comment, "No use to run with slaves on other nodes..."};
	_ ->
	    [ok,ok,ok,ok,ok,ok,ok,ok,ok,ok] = do_dont_drop(Config,10),
	    ok
    end.

do_dont_drop(_,0) ->
    [];
do_dont_drop(Config,N) ->
    %% Name of first slave node
    ?line NN1 = atom_to_list(?MODULE) ++ "slave_1",
    %% Name of node started by heart on failure
    ?line NN2 = atom_to_list(?MODULE) ++ "slave_2",
    %% Name of node started by heart on success
    ?line NN3 = atom_to_list(?MODULE) ++ "slave_3",
    ?line Host = hd(tl(string:tokens(atom_to_list(node()),"@"))),
    %% The initial heart command
    ?line FirstCmd = erl() ++ name(NN2 ++ "@" ++ Host),
    %% Separated the parameters to start_node_run for clarity...
    ?line Name = list_to_atom(NN1),
    ?line Env = [{"HEART_COMMAND", FirstCmd}],
    ?line Func = "start_heart_stress",
    ?line Arg = NN3 ++ "@" ++ Host ++ " " ++ 
	filename:join(?config(data_dir, Config), "simple_echo"),
    ?line start_node_run(Name,Env,Func,Arg),
    ?line case wait_for_any_of(list_to_atom(NN2 ++ "@" ++ Host),
			       list_to_atom(NN3 ++ "@" ++ Host)) of
	      2 ->
		  ?line [ok | do_dont_drop(Config,N-1)];
	      _ ->
		  ?line false
	  end.

wait_for_any_of(N1,N2) ->
    ?line wait_for_any_of(N1,N2,45).

wait_for_any_of(_N1,_N2,0) ->
    ?line false;

wait_for_any_of(N1,N2,Times) ->
    ?line receive 
	  after 1000 ->
		  ?line ok
	  end,
    ?line case net_adm:ping(N1) of
	      pang ->
		  ?line case net_adm:ping(N2) of
			    pang ->
				?line wait_for_any_of(N1,N2,Times - 1);
			    pong ->
				?line rpc:call(N2,init,stop,[]),  
				?line 2
			end;
	      pong ->
		  ?line rpc:call(N1,init,stop,[]),  
		  ?line 1
	  end.


kill_pid(suite) ->
    [];
kill_pid(doc) ->
    ["Tests that heart kills the old erlang node before executing ",
     "heart command."];
kill_pid(Config) when is_list(Config) ->
    %%% Have to do it some times to make it happen...
    case os:type() of
	vxworks ->
	    {comment, "No use to run with slaves on other nodes..."};
	_ ->
	    ok = do_kill_pid(Config)
    end.

do_kill_pid(_Config) ->
    Name = heart_test,
    Env = [{"HEART_COMMAND", "nickeNyfikenFarEttJobb"}],
    {ok,Node} = start_node_run(Name,Env,suicide_by_heart,[]),
    ok = wait_for_node(Node,15),
    erlang:monitor_node(Node, true),
    receive
	{nodedown,Node} ->
	    ok
    after 30000 ->
	    false
    end.

wait_for_node(_,0) ->
    false;
wait_for_node(Node,N) ->
    receive
    after 1000 ->
	    ok
    end,
    case net_adm:ping(Node) of
	pong ->
	    ok;
	pang ->
	    wait_for_node(Node,N-1)
    end.

erl() ->	   
    case os:type() of
	{win32,_} ->
	    "werl ";
	_ ->
	    "erl "
    end.
    
name(Node) when is_list(Node) -> name(Node,[]);
name(Node) when is_atom(Node) -> name(atom_to_list(Node),[]).

name([$@|Node], Name) ->
    case lists:member($., Node) of
	true ->
	    "-name " ++ lists:reverse(Name);
	_ ->
	    "-sname " ++ lists:reverse(Name)
    end;
name([H|T], Name) ->
    name(T, [H|Name]).


atom_conv(A) when is_atom(A) ->
    atom_to_list(A);
atom_conv(A) when is_list(A) ->
    A.

env_conv([]) ->
    [];
env_conv([{X,Y}|T]) ->
    atom_conv(X) ++ " \"" ++ atom_conv(Y) ++ "\" " ++ env_conv(T).

%%%
%%% Starts a node and runs a function in this
%%% module.
%%% Name is the node name as either atom or string,
%%% Env is a list of Tuples containing name-value pairs.
%%% Function is the function to run in this module
%%% Argument is the argument(s) to send through erl -s
%%%
start_node_run(Name, Env, Function, Argument) -> 
    ?line PA = filename:dirname(code:which(?MODULE)),
    ?line Params = "-heart -env " ++ env_conv(Env) ++ " -pa " ++ PA ++ 
	" -s " ++ 
	atom_conv(?MODULE) ++ " " ++ atom_conv(Function) ++ " " ++
	atom_conv(Argument),
    ?line start_node(Name, Params).

start_node(Name, Param) ->
    test_server:start_node(Name, slave, [{args, Param}]).

stop_node(Node) ->
    test_server:stop_node(Node).


%%% This code is run in a slave node to ensure that 
%%% A heart command really gets set syncronously 
%%% and cannot get "dropped".

send_to(_,_,0) ->
    ok;
send_to(Port,D,N) ->
    Port ! {self(),{command,D}},
    send_to(Port,D,N-1).

receive_from(_,_,0) ->
    ok;

receive_from(Port,D,N) ->
    receive
	{Port, {data,{eol,_Data}}} ->
	    receive_from(Port,D,N-1);
	X ->
	    io:format("Got garbage ~p~n",[X])
    end.

mangle(PP) when is_list(PP) ->
    Port = open_port({spawn,PP},[{line,100}]),
    mangle(Port);

mangle(Port) ->
    send_to(Port, "ABCDEFGHIJ" ++ io_lib:nl(),1),
    receive_from(Port,"ABCDEFGHIJ",1),
    mangle(Port).



explode(0,_) ->
    ok;
explode(N,PP) ->
    spawn(?MODULE,mangle,[PP]),
    explode(N-1,PP).

start_heart_stress([NewName,PortProgram]) ->
    explode(10,atom_to_list(PortProgram)),
    NewCmd = erl() ++ name(NewName), 
    %%io:format("~p~n",[NewCmd]),
    receive
    after 10000 ->
	    heart:set_cmd(NewCmd),
	    halt()
    end.

suicide_by_heart() ->
    %%io:format("Suicide starting...~n"),
    open_port({spawn,"heart -ht 11 -pid "++os:getpid()},[{packet,2}]),
    receive X -> X end,
    %% Just hang and wait for heart to timeout
    receive
	{makaronipudding} ->
	    sallad
    end.
