%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(init_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([get_arguments/1, get_argument/1, boot_var/1, restart/1,
	 many_restarts/0, many_restarts/1,
	 get_plain_arguments/1,
	 reboot/1, stop_status/1, stop/1, get_status/1, script_id/1]).
-export([boot1/1, boot2/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

-export([init/1, fini/1]).

-define(DEFAULT_TIMEOUT_SEC, 100).

%%-----------------------------------------------------------------
%% Test suite for init. (Most code is run during system start/stop.
%% Should be started in a CC view with:
%% erl -sname master -rsh ctrsh
%%-----------------------------------------------------------------
suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,2}}].

all() -> 
    [get_arguments, get_argument, boot_var,
     many_restarts,
     get_plain_arguments, restart, stop_status, get_status, script_id,
     {group, boot}].

groups() -> 
    [{boot, [], [boot1, boot2]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(Func, Config) ->
    Config.

end_per_testcase(_Func, _Config) ->
    ok.

init(Config) when is_list(Config) ->
    Config.

fini(Config) when is_list(Config) ->
    Host = list_to_atom(from($@, atom_to_list(node()))),
    Node = list_to_atom(lists:concat([init_test, "@", Host])),
    stop_node(Node),
    Config.

get_arguments(Config) when is_list(Config) ->
    Args = args(),
    {ok, Node} = start_node(init_test, Args),
    case rpc:call(Node, init, get_arguments, []) of
	Arguments when is_list(Arguments) ->
	    stop_node(Node),
	    check_a(Arguments),
	    check_b(Arguments),
	    check_c(Arguments),
	    check_d(Arguments);
	_ ->
	    stop_node(Node),
	    ct:fail(get_arguments)
    end,
    ok.

check_a(Args) ->
    case lists:keysearch(a,1,Args) of
	{value, {a,["kalle"]}} ->
	    Args1 = lists:keydelete(a,1,Args),
	    case lists:keysearch(a,1,Args1) of
		false ->
		    ok;
		_ ->
		    ct:fail(check_a1)
	    end;
	_ ->
	    ct:fail(check_a2)
    end.

check_b(Args) ->
    case lists:keysearch(b,1,Args) of
	{value, {b,["hej", "hopp"]}} ->
	    Args1 = lists:keydelete(b,1,Args),
	    case lists:keysearch(b,1,Args1) of
		{value, {b,["san", "sa"]}} ->
		    Args2 = lists:keydelete(b,1,Args1),
		    case lists:keysearch(b,1,Args2) of
			false ->
			    ok;
			_ ->
			    ct:fail(check_b1)
		    end;
		_ ->
		    ct:fail(check_b2)
	    end;
	_ ->
	    ct:fail(check_b3)
    end.

check_c(Args) ->
    case lists:keysearch(c,1,Args) of
	{value, {c,["4", "5", "6"]}} ->
	    Args1 = lists:keydelete(c,1,Args),
	    case lists:keysearch(c,1,Args1) of
		{value, {c,["7", "8", "9"]}} ->
		    Args2 = lists:keydelete(c,1,Args1),
		    case lists:keysearch(c,1,Args2) of
			false ->
			    ok;
			_ ->
			    ct:fail(check_c1)
		    end;
		_ ->
		    ct:fail(check_c2)
	    end;
	_ ->
	    ct:fail(check_c3)
    end.

check_d(Args) ->
    case lists:keysearch(d,1,Args) of
	{value, {d,[]}} ->
	    Args1 = lists:keydelete(d,1,Args),
	    case lists:keysearch(d,1,Args1) of
		false ->
		    ok;
		_ ->
		    ct:fail(check_d1)
	    end;
	_ ->
	    ct:fail(check_d2)
    end.

get_argument(Config) when is_list(Config) ->
    Args = args(),
    {ok, Node} = start_node(init_test, Args),
    case rpc:call(Node, init, get_argument, [b]) of
	{ok, [["hej", "hopp"],["san", "sa"]]} ->
	    ok;
	_ ->
	    stop_node(Node),
	    ct:fail({get_argument, b})
    end,
    case rpc:call(Node, init, get_argument, [a]) of
	{ok, [["kalle"]]} ->
	    ok;
	_ ->
	    stop_node(Node),
	    ct:fail({get_argument, a})
    end,
    case rpc:call(Node, init, get_argument, [c]) of
	{ok, [["4", "5", "6"], ["7", "8", "9"]]} ->
	    ok;
	_ ->
	    stop_node(Node),
	    ct:fail({get_argument, c})
    end,
    case rpc:call(Node, init, get_argument, [d]) of
	{ok, [[]]} ->
	    ok;
	_ ->
	    stop_node(Node),
	    ct:fail({get_argument, d})
    end,
    case rpc:call(Node, init, get_argument, [e]) of
	error ->
	    ok;
	_ ->
	    stop_node(Node),
	    ct:fail({get_argument, e})
    end,
    stop_node(Node),
    ok.

get_plain_arguments(Config) when is_list(Config) ->
    Longstring = 
	"fjdkfjdkfjfdaa2fjdkfjdkfjfdaa2fjdkfjdkfjfdaa2"
	"fjdkfjdkfjfdaa2fjdkfjdkfjfdaa2fjdkfjdkfjfdaa2"
	"fjdkfjdkfjfdaa2fjdkfjdkfjfdaa2fjdkfjdkfjfdaa2"
	"fjdkfjdkfjfdaa2fjdkfjdkfjfdaa2fjdkfjdkfjfdaa2"
	"fjdkfjdkfjfdaa2fjdkfjdkfjfdaa2fjdkfjdkfjfdaa2"
	"fjdkfjdkfjfdaa2fjdkfjdkfjfdaa2fjdkfjdkfjfdaa2"
	"fjdkfjdkfjfdaa2fjdkfjdkfjfdaa2fjdkfjdkfjfdaa2"
	"fjdkfjdkfjfdaa2fjdkfjdkfjfdaa2fjdkfjdkfjfdaa2"
	"fjdkfjdkfjfdaa2fjdkfjdkfjfdaa2fjdkfjdkfjfdaa2",
    true = (length(Longstring) > 255),
    Args = long_args(Longstring),
    {ok, Node} = start_node(init_test, Args),
    case rpc:call(Node, init, get_plain_arguments, []) of
	["a", "b", "c", Longstring] ->
	    ok;
	As ->
	    stop_node(Node),
	    ct:fail({get_argument, As})
    end,
    stop_node(Node),

    ok.


%% ------------------------------------------------
%% Use -boot_var flag to set $TEST_VAR in boot script.
%% ------------------------------------------------
boot_var(Config) when is_list(Config) ->
    {BootScript, TEST_VAR, KernelVsn, StdlibVsn} = create_boot(Config),

    %% Should fail as we have not given -boot_var TEST_VAR
    {error, timeout} =
	start_node(init_test, "-boot " ++ BootScript),

    case is_real_system(KernelVsn, StdlibVsn) of
	true ->
	    %% Now it should work !!
	    {ok, Node} =
		start_node(init_test,
			   "-boot " ++ BootScript ++
			       " -boot_var TEST_VAR \"" ++
			       TEST_VAR ++ "\""),
	    stop_node(Node),
	    Res = ok;
	_ ->
	    %% What we need is not so much version numbers on the directories, but
	    %% for the boot var TEST_VAR to appear in the boot script, and it doesn't
	    %% if we give the 'local' option to systools:make_script.
	    io:format(
	      "Test case not complete as we are not~n"
	      "running in a real system!~n"
	      "Probably this test is performed in a "
	      "clearcase view or source tree.~n"
	      "Need version numbers on the kernel and "
	      "stdlib directories!~n",
	      []),
	    Res = {skip,
		   "Test case only partially run since it is run "
		   "in a clearcase view or in a source tree. "
		   "Need an installed system to complete this test."}
    end,
    Res.

create_boot(Config) ->
    {ok, OldDir} = file:get_cwd(),
    {LatestDir, LatestName, KernelVsn, StdlibVsn} =
	create_script(Config),
    LibDir = code:lib_dir(),
    ok = file:set_cwd(LatestDir),
    ok = systools:make_script(LatestName,
			      [{variables, [{"TEST_VAR", LibDir}]}]),
    ok = file:set_cwd(OldDir),
    {LatestDir ++ "/" ++ LatestName, LibDir, KernelVsn, StdlibVsn}.

is_real_system(KernelVsn, StdlibVsn) ->
    LibDir = code:lib_dir(),
    filelib:is_dir(filename:join(LibDir, "kernel-"++KernelVsn)) andalso
	filelib:is_dir(filename:join(LibDir, "stdlib-"++StdlibVsn)).

%% ------------------------------------------------
%% Slave executes erlang:halt() on master nodedown.
%% Therefore the slave process must be killed
%% before restart.
%% ------------------------------------------------
many_restarts() ->
    [{timetrap,{minutes,8}}].

many_restarts(Config) when is_list(Config) ->
    {ok, Node} = loose_node:start(init_test, "", ?DEFAULT_TIMEOUT_SEC),
    loop_restart(50,Node,rpc:call(Node,erlang,whereis,[error_logger])),
    loose_node:stop(Node),
    ok.

loop_restart(0,_,_) ->
    ok;
loop_restart(N,Node,EHPid) ->
    erlang:monitor_node(Node, true),
    ok = rpc:call(Node, init, restart, []),
    receive
	{nodedown, Node} ->
	    ok
    after 10000 ->
	    loose_node:stop(Node),
	    ct:fail(not_stopping)
    end,
    ok = wait_for(30, Node, EHPid),
    loop_restart(N-1,Node,rpc:call(Node,erlang,whereis,[error_logger])).

wait_for(0,Node,_) ->
    loose_node:stop(Node),    
    error;
wait_for(N,Node,EHPid) ->
    case rpc:call(Node, erlang, whereis, [error_logger]) of
	Pid when is_pid(Pid), Pid =/= EHPid ->
	    %% erlang:display(ok),
	    ok;
	_X ->
	    %% erlang:display(_X),
	    %% Procs = rpc:call(Node, erlang, processes, []),
	    %% erlang:display(Procs),
	    %% case is_list(Procs) of
	    %%     true ->
	    %% 	  [(catch erlang:display(
	    %% 			  rpc:call(Node,
	    %% 				   erlang,
	    %% 				   process_info,
	    %% 				   [Y,registered_name])))
	    %% 		 || Y <- Procs];
	    %%     _ ->
	    %% 	  ok
	    %% end,
	    receive
	    after 100 ->
		    ok
	    end,
	    wait_for(N-1,Node,EHPid)
    end.

%% ------------------------------------------------
%% Slave executes erlang:halt() on master nodedown.
%% Therefore the slave process must be killed
%% before restart.
%% ------------------------------------------------
restart(Config) when is_list(Config) ->
    Args = args(),

    %% Currently test_server:start_node cannot be used. The restarted
    %% node immediately halts due to the implementation of
    %% test_server:start_node.
    {ok, Node} = loose_node:start(init_test, Args, ?DEFAULT_TIMEOUT_SEC),
    %% Ok, the node is up, now the real test test begins.
    erlang:monitor_node(Node, true),
    InitPid = rpc:call(Node, erlang, whereis, [init]),
    PurgerPid = rpc:call(Node, erlang, whereis, [erts_code_purger]),
    Procs = rpc:call(Node, erlang, processes, []),
    MaxPid = lists:last(Procs),
    ok = rpc:call(Node, init, restart, []),
    receive
	{nodedown, Node} ->
	    ok
    after 10000 ->
	    loose_node:stop(Node),
	    ct:fail(not_stopping)
    end,
    ok = wait_restart(30, Node),

    %% Still the same init process!
    InitPid1 = rpc:call(Node, erlang, whereis, [init]),
    InitP = pid_to_list(InitPid),
    InitP = pid_to_list(InitPid1),

    %% and same purger process!
    PurgerPid1 = rpc:call(Node, erlang, whereis, [erts_code_purger]),
    PurgerP = pid_to_list(PurgerPid),
    PurgerP = pid_to_list(PurgerPid1),

    NewProcs0 = rpc:call(Node, erlang, processes, []),
    NewProcs = NewProcs0 -- [InitPid1, PurgerPid1],
    case check_processes(NewProcs, MaxPid) of
	true ->
	    ok;
	_ ->
	    loose_node:stop(Node),
	    ct:fail(processes_not_greater)
    end,

    %% Test that, for instance, the same argument still exists.
    case rpc:call(Node, init, get_argument, [c]) of
	{ok, [["4", "5", "6"], ["7", "8", "9"]]} ->
	    ok;
	_ ->
	    loose_node:stop(Node),
	    ct:fail({get_argument, restart_fail})
    end,
    loose_node:stop(Node),
    ok.

wait_restart(0, _Node) ->
    ct:fail(not_restarted);
wait_restart(N, Node) ->
    case net_adm:ping(Node) of
	pong -> ok;
	_ -> 
	    ct:sleep(1000),
	    wait_restart(N - 1, Node)
    end.

check_processes(NewProcs, MaxPid) ->
    [N,P,I] = apid(MaxPid),
    case lists:filter(fun(Pid) ->
			      case apid(Pid) of
				  [N,P1,_I1] when P1 > P -> false;
				  [N,_P1,I1] when I1 > I -> false;
				  _                      -> true
			      end
		      end, NewProcs) of
	[] ->
	    true;
	_  ->
	    false
    end.

apid(Pid) ->
    [N,P,I] = string:tokens(pid_to_list(Pid),"<>."),
    [list_to_integer(N),list_to_integer(P),list_to_integer(I)].

%% ------------------------------------------------
%% Just test that the system is halted here.
%% The reboot facility using heart is tested
%% in the heart_SUITE.
%% ------------------------------------------------
reboot(Config) when is_list(Config) ->
    Args = args(),
    {ok, Node} = start_node(init_test, Args),
    erlang:monitor_node(Node, true),
    ok = rpc:call(Node, init, reboot, []),
    receive
	{nodedown, Node} ->
	    ok
    after 10000 ->
	    stop_node(Node),
	    ct:fail(not_stopping)
    end,
    ct:sleep(5000),
    case net_adm:ping(Node) of
	pang ->
	    ok;
	_ ->
	    stop_node(Node),
	    ct:fail(system_rebooted)
    end,
    ok.

%% ------------------------------------------------
%%
%% ------------------------------------------------
stop_status(Config) when is_list(Config) ->
    badarg = catch_stop([65,[66],67]),  % flat strings only
    badarg = catch_stop([65, 666, 67]),  % only bytes in string
    badarg = catch_stop(abort),  % 'abort' not allowed
    badarg = catch_stop(true),  % other atoms not allowed
    badarg = catch_stop(-1),  % no negative statuses
    ok.

catch_stop(Status) ->
    try init:stop(Status) catch error:badarg -> badarg end.

%% ------------------------------------------------
%%
%% ------------------------------------------------
stop(Config) when is_list(Config) ->
    Args = args(),
    {ok, Node} = start_node(init_test, Args),
    erlang:monitor_node(Node, true),
    ok = rpc:call(Node, init, reboot, []),
    receive
	{nodedown, Node} ->
	    ok
    after 10000 ->
	    stop_node(Node),
	    ct:fail(not_stopping)
    end,
    ct:sleep(5000),
    case net_adm:ping(Node) of
	pang ->
	    ok;
	_ ->
	    stop_node(Node),
	    ct:fail(system_rebooted)
    end,
    ok.

%% ------------------------------------------------
%% 
%% ------------------------------------------------
get_status(Config) when is_list(Config) ->
    {Start, _} = init:get_status(),

    %% Depending on how the test_server is started Start has
    %% different values. staring if test_server started with
    %% -s flag.
    case lists:member(Start, [started, starting]) of
	true ->
	    ok;
	_ ->
	    ct:fail(get_status)
    end.

%% ------------------------------------------------
%% 
%% ------------------------------------------------
script_id(Config) when is_list(Config) ->
    {Name, Vsn} = init:script_id(),
    if
	is_list(Name), is_list(Vsn) ->
	    ok;
	true ->
	    ct:fail(not_standard_script)
    end,
    ok.

%% ------------------------------------------------
%% Start the slave system with -boot flag.
%% ------------------------------------------------

boot1(Config) when is_list(Config) ->
    Args = args() ++ " -boot start_sasl",
    {ok, Node} = start_node(init_test, Args),
    stop_node(Node),

    %% Try to start with non existing boot file.
    Args1 = args() ++ " -boot dummy_script",
    {error, timeout} = start_node(init_test, Args1),

    ok.

boot2(Config) when is_list(Config) ->
    %% Absolute boot file name
    Boot = filename:join([code:root_dir(), "bin", "start_sasl"]),

    Args = args() ++ " -boot \"" ++ Boot++"\"",
    {ok, Node} = start_node(init_test, Args),
    stop_node(Node),

    case os:type() of 
	{win32, _} ->
	    %% Absolute boot file name for Windows -- all slashes are
	    %% converted to backslashes.
	    Win_boot = lists:map(fun
				     ($/) -> $\\;
				     (C) -> C
				end, Boot),
	    Args2 = args() ++ " -boot \"" ++ Win_boot ++ "\"",
	    {ok, Node2} = start_node(init_test, Args2),
	    stop_node(Node2);
	_ ->
	    ok
    end,

    ok.

%% Misc. functions    

start_node(Name, Param) ->
    test_server:start_node(Name, slave, [{args, Param}]).

stop_node(Node) ->
    test_server:stop_node(Node).

from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(_, []) -> [].

args() ->
    "-a kalle -- a b -d -b hej hopp -- c d -b san sa -c 4 5 6 -c 7 8 9".

long_args(A) ->
    lists:flatten(
      io_lib:format("-a kalle -- a b -d -b hej hopp -- c "
		    "~s -b san sa -c 4 5 6 -c 7 8 9",
		    [A])).

create_script(Config) ->
    PrivDir = proplists:get_value(priv_dir,Config),
    Name = PrivDir ++ "boot_var_test",
    Apps = application_controller:which_applications(),
    {value,{_,_,KernelVer}} = lists:keysearch(kernel,1,Apps),
    {value,{_,_,StdlibVer}} = lists:keysearch(stdlib,1,Apps),
    {ok,Fd} = file:open(Name ++ ".rel", [write]),
    io:format(Fd,
	      "{release, {\"Test release 3\", \"P2A\"}, \n"
	      " {erts, \"4.4\"}, \n"
	      " [{kernel, \"~s\"}, {stdlib, \"~s\"}]}.\n",
	      [KernelVer,StdlibVer]),
    file:close(Fd),
    {filename:dirname(Name), filename:basename(Name),
     KernelVer, StdlibVer}.

