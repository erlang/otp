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
-module(init_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([get_arguments/1, get_argument/1, boot_var/1, restart/1,
	 many_restarts/0, many_restarts/1, restart_with_mode/1,
	 get_plain_arguments/1,
	 reboot/1, stop_status/1, stop/1, get_status/1, script_id/1,
         dot_erlang/1, unknown_module/1, dash_S/1, dash_extra/1,
         dash_run/1, dash_s/1,
	 find_system_processes/0
         ]).
-export([boot1/1, boot2/1]).
-export([test_dash_S/1, test_dash_s/1, test_dash_extra/0,
         test_dash_run/0, test_dash_run/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

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
     many_restarts, restart_with_mode,
     get_plain_arguments, restart, stop_status, get_status, script_id,
     dot_erlang, unknown_module, {group, boot},
     dash_S, dash_extra, dash_run, dash_s].

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


init_per_testcase(_Func, Config) ->
    Config.

end_per_testcase(_Func, _Config) ->
    ok.

get_arguments(Config) when is_list(Config) ->
    Args = args(),
    {ok, Peer, Node} = ?CT_PEER(Args),
    case rpc:call(Node, init, get_arguments, []) of
	Arguments when is_list(Arguments) ->
	    peer:stop(Peer),
	    check_a(Arguments),
	    check_b(Arguments),
	    check_c(Arguments),
	    check_d(Arguments);
	_ ->
            peer:stop(Peer),
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
    {ok, Peer, Node} = ?CT_PEER(Args),
    case rpc:call(Node, init, get_argument, [b]) of
	{ok, [["hej", "hopp"],["san", "sa"]]} ->
	    ok;
	_ ->
	    peer:stop(Peer),
	    ct:fail({get_argument, b})
    end,
    case rpc:call(Node, init, get_argument, [a]) of
	{ok, [["kalle"]]} ->
	    ok;
	_ ->
	    peer:stop(Peer),
	    ct:fail({get_argument, a})
    end,
    case rpc:call(Node, init, get_argument, [c]) of
	{ok, [["4", "5", "6"], ["7", "8", "9"]]} ->
	    ok;
	_ ->
	    peer:stop(Peer),
	    ct:fail({get_argument, c})
    end,
    case rpc:call(Node, init, get_argument, [d]) of
	{ok, [[]]} ->
	    ok;
	_ ->
	    peer:stop(Peer),
	    ct:fail({get_argument, d})
    end,
    case rpc:call(Node, init, get_argument, [e]) of
	error ->
	    ok;
	_ ->
	    peer:stop(Peer),
	    ct:fail({get_argument, e})
    end,
    peer:stop(Peer),
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
    {ok, Peer, Node} = ?CT_PEER(Args),
    case rpc:call(Node, init, get_plain_arguments, []) of
	["a", "b", "c", Longstring] ->
	    ok;
	As ->
	    peer:stop(Peer),
	    ct:fail({get_argument, As})
    end,
    peer:stop(Peer),

    ok.


%% ------------------------------------------------
%% Use -boot_var flag to set $TEST_VAR in boot script.
%% ------------------------------------------------
boot_var(Config) when is_list(Config) ->
    {BootScript, TEST_VAR, KernelVsn, StdlibVsn} = create_boot(Config),

    %% Should fail as we have not given -boot_var TEST_VAR
    try
        ?CT_PEER(#{args => ["-boot", BootScript], connection => standard_io}),
        ct:fail({boot_var, "Should not start without TEST_VAR"})
    catch
        exit:{boot_failed,{exit_status,1}} ->
            ok
    end,


    case is_real_system(KernelVsn, StdlibVsn) of
	true ->
	    %% Now it should work !!
	    {ok, Peer, _} = ?CT_PEER(["-boot", BootScript, "-boot_var", "TEST_VAR", TEST_VAR]),
	    peer:stop(Peer),
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
    [{timetrap,{minutes,16}}].

many_restarts(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(#{connection => standard_io}),
    loop_restart(50,Peer,Node,rpc:call(Node,erlang,whereis,[logger])),
    peer:stop(Peer),
    ok.

loop_restart(0,_,_,_) ->
    ok;
loop_restart(N,Peer,Node,EHPid) ->
    erlang:monitor_node(Node, true),
    ok = rpc:call(Node, init, restart, []),
    receive
	{nodedown, Node} ->
	    ok
    after 10000 ->
	    peer:stop(Peer),
	    ct:fail(not_stopping)
    end,
    ok = wait_for(60, Peer, Node, EHPid),
    loop_restart(N-1,Peer,Node,rpc:call(Node,erlang,whereis,[logger])).

wait_for(0,Peer,_Node,_) ->
    peer:stop(Peer),
    error;
wait_for(N,Peer,Node,EHPid) ->
    case rpc:call(Node, erlang, whereis, [logger]) of
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
	    wait_for(N-1,Peer,Node,EHPid)
    end.

restart_with_mode(Config) when is_list(Config) ->
    %% We cannot use loose_node because it doesn't run in
    %% embedded mode so we quickly start one that exits after restarting
    {ok,[[Erl]]} = init:get_argument(progname),

    Quote = case os:type() of
                {win32,_} ->
                    [$"];
                {unix,_} ->
                    [$']
            end,

    Eval1 = Quote ++ "Mode=code:get_mode(), io:fwrite(Mode), case Mode of interactive -> init:restart([{mode,embedded}]); embedded -> erlang:halt() end" ++ Quote,
    Cmd1 = Erl ++ " -mode interactive -noshell -eval " ++ Eval1,
    "interactiveembedded" = os:cmd(Cmd1),

    Eval2 = Quote ++ "Mode=code:get_mode(), io:fwrite(Mode), case Mode of embedded -> init:restart([{mode,interactive}]); interactive -> erlang:halt() end" ++ Quote,
    Cmd2 = Erl ++ " -mode embedded -noshell -eval " ++ Eval2,
    "embeddedinteractive" = os:cmd(Cmd2),

    ok.

%% ------------------------------------------------
%% Slave executes erlang:halt() on master nodedown.
%% Therefore the slave process must be killed
%% before restart.
%% ------------------------------------------------
restart(Config) when is_list(Config) ->
    Args = args(),
    {ok, Peer, Node} = ?CT_PEER(#{args => Args, connection => standard_io}),
    %% Ok, the node is up, now the real test test begins.
    erlang:monitor_node(Node, true),
    SysProcs0 = rpc:call(Node, ?MODULE, find_system_processes, []),
    io:format("SysProcs0=~p~n", [SysProcs0]),
    [InitPid, PurgerPid, LitCollectorPid,
     DirtySigNPid, DirtySigHPid, DirtySigMPid,
     PrimFilePid,
     ESockRegPid] = SysProcs0,
    InitPid = rpc:call(Node, erlang, whereis, [init]),
    PurgerPid = rpc:call(Node, erlang, whereis, [erts_code_purger]),
    Procs = rpc:call(Node, erlang, processes, []),
    MsgFlags = fetch_socket_msg_flags(Node),
    MaxPid = lists:last(Procs),
    ok = rpc:call(Node, init, restart, []),
    receive
	{nodedown, Node} ->
	    ok
    after 10000 ->
	    peer:stop(Peer),
	    ct:fail(not_stopping)
    end,
    ok = wait_restart(30, Node),

    SysProcs1 = rpc:call(Node, ?MODULE, find_system_processes, []),
    io:format("SysProcs1=~p~n", [SysProcs1]),
    [InitPid1, PurgerPid1, LitCollectorPid1,
     DirtySigNPid1, DirtySigHPid1, DirtySigMPid1,
     PrimFilePid1,
     ESockRegPid1] = SysProcs1,

    %% Still the same init process!
    InitPid1 = rpc:call(Node, erlang, whereis, [init]),
    InitP = pid_to_list(InitPid),
    InitP = pid_to_list(InitPid1),

    %% and same purger process!
    PurgerPid1 = rpc:call(Node, erlang, whereis, [erts_code_purger]),
    PurgerP = pid_to_list(PurgerPid),
    PurgerP = pid_to_list(PurgerPid1),

    %% and same literal area collector process!
    LitCollectorP = pid_to_list(LitCollectorPid),
    LitCollectorP = pid_to_list(LitCollectorPid1),

    %% and same normal dirty signal handler process!
    DirtySigNP = pid_to_list(DirtySigNPid),
    DirtySigNP = pid_to_list(DirtySigNPid1),
    %% and same high dirty signal handler process!
    DirtySigHP = pid_to_list(DirtySigHPid),
    DirtySigHP = pid_to_list(DirtySigHPid1),
    %% and same max dirty signal handler process!
    DirtySigMP = pid_to_list(DirtySigMPid),
    DirtySigMP = pid_to_list(DirtySigMPid1),

    %% and same prim_file helper process!
    PrimFileP = pid_to_list(PrimFilePid),
    PrimFileP = pid_to_list(PrimFilePid1),

    %% and same socket_registry helper process!
    if ESockRegPid =:= undefined, ESockRegPid1 =:= undefined ->
            %% No socket registry on either node
            ok;
       true ->
            ESockRegP = pid_to_list(ESockRegPid),
            ESockRegP = pid_to_list(ESockRegPid1)
    end,

    NewProcs0 = rpc:call(Node, erlang, processes, []),
    NewProcs = NewProcs0 -- SysProcs1,
    case check_processes(NewProcs, MaxPid) of
	true ->
	    ok;
	_ ->
	    peer:stop(Peer),
	    ct:fail(processes_not_greater)
    end,

    %% Check that socket tables has been re-initialized; check one
    MsgFlags = fetch_socket_msg_flags(Node),

    %% Test that, for instance, the same argument still exists.
    case rpc:call(Node, init, get_argument, [c]) of
	{ok, [["4", "5", "6"], ["7", "8", "9"]]} ->
	    ok;
	_ ->
	    peer:stop(Peer),
	    ct:fail({get_argument, restart_fail})
    end,
    peer:stop(Peer),
    ok.

-record(sys_procs, {init,
		    code_purger,
		    literal_collector,
		    dirty_sig_handler_normal,
		    dirty_sig_handler_high,
		    dirty_sig_handler_max,
                    prim_file,
                    socket_registry}).

find_system_processes() ->
    find_system_procs(processes(), #sys_procs{}).

find_system_procs([], SysProcs) ->
    [SysProcs#sys_procs.init,
     SysProcs#sys_procs.code_purger,
     SysProcs#sys_procs.literal_collector,
     SysProcs#sys_procs.dirty_sig_handler_normal,
     SysProcs#sys_procs.dirty_sig_handler_high,
     SysProcs#sys_procs.dirty_sig_handler_max,
     SysProcs#sys_procs.prim_file,
     SysProcs#sys_procs.socket_registry];
find_system_procs([P|Ps], SysProcs) ->
    case process_info(P, [initial_call, priority]) of
	[{initial_call,{erl_init,start,2}},_] ->
	    undefined = SysProcs#sys_procs.init,
	    find_system_procs(Ps, SysProcs#sys_procs{init = P});
	[{initial_call,{erts_code_purger,start,0}},_] ->
	    undefined = SysProcs#sys_procs.code_purger,
	    find_system_procs(Ps, SysProcs#sys_procs{code_purger = P});
	[{initial_call,{erts_literal_area_collector,start,0}},_] ->
	    undefined = SysProcs#sys_procs.literal_collector,
	    find_system_procs(Ps, SysProcs#sys_procs{literal_collector = P});
	[{initial_call,{erts_dirty_process_signal_handler,start,0}},
         {priority,normal}] ->
	    undefined = SysProcs#sys_procs.dirty_sig_handler_normal,
	    find_system_procs(Ps, SysProcs#sys_procs{dirty_sig_handler_normal = P});
	[{initial_call,{erts_dirty_process_signal_handler,start,0}},
                       {priority,high}] ->
	    undefined = SysProcs#sys_procs.dirty_sig_handler_high,
	    find_system_procs(Ps, SysProcs#sys_procs{dirty_sig_handler_high = P});
	[{initial_call,{erts_dirty_process_signal_handler,start,0}},
         {priority,max}] ->
	    undefined = SysProcs#sys_procs.dirty_sig_handler_max,
	    find_system_procs(Ps, SysProcs#sys_procs{dirty_sig_handler_max = P});
        [{initial_call,{prim_file,start,0}},_] ->
	    undefined = SysProcs#sys_procs.prim_file,
	    find_system_procs(Ps, SysProcs#sys_procs{prim_file = P});
        [{initial_call,{socket_registry,start,0}},_] ->
	    undefined = SysProcs#sys_procs.socket_registry,
	    find_system_procs(Ps, SysProcs#sys_procs{socket_registry = P});
	_ ->
	    find_system_procs(Ps, SysProcs)
    end.

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

fetch_socket_msg_flags(Node) ->
    case code:is_loaded(prim_socket) of
        {file,preloaded} ->
            lists:sort(rpc:call(Node, socket, supports, [msg_flags]));
        _ ->
            ok
    end.

%% ------------------------------------------------
%% Just test that the system is halted here.
%% The reboot facility using heart is tested
%% in the heart_SUITE.
%% ------------------------------------------------
reboot(Config) when is_list(Config) ->
    Args = args(),
    {ok, Peer, Node} = ?CT_PEER(Args),
    erlang:monitor_node(Node, true),
    ok = rpc:call(Node, init, reboot, []),
    receive
	{nodedown, Node} ->
	    ok
    after 10000 ->
	    peer:stop(Peer),
	    ct:fail(not_stopping)
    end,
    ct:sleep(5000),
    case net_adm:ping(Node) of
	pang ->
	    ok;
	_ ->
	    peer:stop(Peer),
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
    {ok, Peer, Node} = ?CT_PEER(Args),
    erlang:monitor_node(Node, true),
    ok = rpc:call(Node, init, reboot, []),
    receive
	{nodedown, Node} ->
	    ok
    after 10000 ->
	    peer:stop(Peer),
	    ct:fail(not_stopping)
    end,
    ct:sleep(5000),
    case net_adm:ping(Node) of
	pang ->
	    ok;
	_ ->
	    peer:stop(Peer),
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
%% Test that .erlang file works as it should
%% ------------------------------------------------
dot_erlang(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),

    TestHome = filename:join(PrivDir, ?FUNCTION_NAME),
    ok = file:make_dir(TestHome),

    HomeEnv = case os:type() of
                  {win32, _} ->
                      [Drive | Path] = filename:split(TestHome),
                      [{"APPDATA", filename:join(TestHome,"AppData")},
                       {"HOMEDRIVE", Drive}, {"HOMEPATH", filename:join(Path)}];
                  _ ->
                      [{"HOME", TestHome}]
              end,

    NodeOpts = #{ env => HomeEnv },

    %% Get basedir of peer node
    {ok, CreatorPeer, CreatorNode} = ?CT_PEER(NodeOpts),
    UserConfig = erpc:call(CreatorNode, filename, basedir, [user_config,"erlang"]),
    peer:stop(CreatorPeer),

    XDGErlang = filename:join([UserConfig, ".erlang"]),
    ok = filelib:ensure_dir(XDGErlang),
    ok = file:write_file(XDGErlang, "application:set_env(kernel,test,xdg)."),

    {ok, Peer, Node} = ?CT_PEER(NodeOpts),

    ?assertEqual({ok, xdg}, erpc:call(Node, application, get_env, [kernel, test])),
    peer:stop(Peer),

    HomeErlang = filename:join([TestHome, ".erlang"]),
    ok = file:write_file(HomeErlang, "application:set_env(kernel,test,home)."),
    {ok, Peer2, Node2} = ?CT_PEER(NodeOpts),

    ?assertEqual({ok, home}, erpc:call(Node2, application, get_env, [kernel, test])),
    peer:stop(Peer2),

    ok.

unknown_module(Config) when is_list(Config) ->
    Port = open_port({spawn, "erl -s unknown_module"},
                     [exit_status, use_stdio, stderr_to_stdout]),
    Error = "Error! Failed to load module 'unknown_module' because it cannot be found.",
    [_ | _] = string:find(collect_until_exit_one(Port), Error),
    ok.

collect_until_exit_one(Port) ->
    receive
        {Port, {data, Msg}} -> Msg ++ collect_until_exit_one(Port);
        {Port, {exit_status, 1}} -> []
    after
        30_000 -> ct:fail(erl_timeout)
    end.

%% ------------------------------------------------
%% Start the slave system with -boot flag.
%% ------------------------------------------------

boot1(Config) when is_list(Config) ->
    Args = args() ++ ["-boot", "start_sasl"],
    {ok, Peer, _Node} = ?CT_PEER(Args),
    peer:stop(Peer),

    %% Try to start with non existing boot file.
    Args1 = args() ++ ["-boot", "dummy_script"],
    try
        ?CT_PEER(#{args => Args1, connection => standard_io}),
        ct:fail({boot1, "started with non existing boot file"})
    catch
        exit:{boot_failed, {exit_status, 1}} ->
            ok
    end.

boot2(Config) when is_list(Config) ->
    %% Absolute boot file name
    Boot = filename:join([code:root_dir(), "bin", "start_sasl"]),

    Args = args() ++ ["-boot", Boot],
    {ok, Peer, _Node} = ?CT_PEER(Args),
    peer:stop(Peer),

    case os:type() of 
	{win32, _} ->
	    %% Absolute boot file name for Windows -- all slashes are
	    %% converted to backslashes.
	    Win_boot = lists:map(fun
				     ($/) -> $\\;
				     (C) -> C
				end, Boot),
	    Args2 = args() ++ ["-boot", Win_boot],
	    {ok, Peer2, _Node2} = ?CT_PEER(Args2),
	    peer:stop(Peer2);
	_ ->
	    ok
    end,

    ok.

dash_S(_Config) ->

    %% Test that arguments are passed correctly
    {[],[],[]} = run_dash_S_test([]),
    {["a"],[],[]} = run_dash_S_test(["a"]),
    {["-S","--"],[],[]} = run_dash_S_test(["-S","--"]),
    {["--help"],[],[]} = run_dash_S_test(["--help"]),
    {["-extra"],[],[]} = run_dash_S_test(["-extra"]),
    {["-run"],[],[]} = run_dash_S_test(["-run"]),
    {["-args_file"],[],[]} = run_dash_S_test(["-args_file"]),
    {["+A","-1"],[],[]} = run_dash_S_test(["+A","-1"]),
    {["-s","init","stop"],[],[]} = run_dash_S_test(["-s","init","stop"]),

    %% Test that environment variables are handled correctly
    {["a"],["b","c","d"],[]} =
        run_dash_S_test([{"ERL_AFLAGS","b"},{"ERL_FLAGS","c"},{"ERL_ZFLAGS","d"}],["a"]),
    %% test that -S in environment variables are interpreted as flags
    {["a"],[],[["a"],["b"],["c"]]} =
        run_dash_S_test([{"ERL_AFLAGS","+S 1 -S a"},{"ERL_FLAGS","-S b"},
                         {"ERL_ZFLAGS","-S c"}],["a"]),

    %% Test that -s and -run work
    ?assertMatch(
       "[a].{[\"a\""++_,
       run_dash_test(["-s",?MODULE,"test_dash_s","a","-S",?MODULE,"test_dash_S","a"])),
    ?assertMatch(
       "[\"a\"].{[\"a\""++_,
       run_dash_test(["-run",?MODULE,"test_dash_s","a","-S",?MODULE,"test_dash_S","a"])),

    %% Test error conditions
    ?assertNotEqual(
       nomatch,
       string:find(run_dash_test(["-S"]),
                   "Error! The -S option must be followed by at least a module to start")),

    ?assertNotEqual(
       nomatch,
       string:find(run_dash_test(["-S","a"]),
                   "Error! Failed to load module 'a' because it cannot be found.")),

    ?assertNotEqual(
       nomatch,
       string:find(run_dash_test(["-S",?MODULE,"a"]),
                   "Error! init_SUITE:a/1 is not exported.")),

    ok.

run_dash_S_test(Args) ->
    run_dash_S_test("", Args).
run_dash_S_test(Prefix, Args) ->
    run_dash_test(Prefix, ["-S", ?MODULE, "test_dash_S" | Args]).

test_dash_S(Args) ->
    AllArgs = {Args, init:get_plain_arguments(),
               proplists:get_all_values('S',init:get_arguments()),
               erlang:system_info(emu_args)},
    io:format("~p.",[AllArgs]),
    erlang:halt().

test_dash_s(Args) ->
    io:format("~p.",[Args]).

dash_run(_Config) ->

    {undefined,[]} =
        run_dash_test(["-run",?MODULE,"test_dash_run","-s","init","stop"]),

    {["a"],["b"]} =
        run_dash_test(["-run",?MODULE,"test_dash_run","a","--","b","-s","init","stop"]),

    %% Test error conditions
    ?assertNotEqual(
       nomatch,
       string:find(run_dash_test(["-run","a"]),
                   "Error! Failed to load module 'a' because it cannot be found.")),

    ?assertNotEqual(
       nomatch,
       string:find(run_dash_test(["-run",?MODULE]),
                   "Error! init_SUITE:start/0 is not exported.")),

    ?assertNotEqual(
       nomatch,
       string:find(run_dash_test(["-run",?MODULE,"a"]),
                   "Error! init_SUITE:a/0 is not exported.")),

    ok.

test_dash_run() ->
    test_dash_run(undefined).
test_dash_run(Args) ->
    io:format("~p.",[{Args, init:get_plain_arguments(), erlang:system_info(emu_args)}]),
    ok.

dash_s(_Config) ->

    {undefined,[]} =
        run_dash_test(["-s",?MODULE,"test_dash_run","-s","init","stop"]),

    {[a],["b"]} =
        run_dash_test(["-s",?MODULE,"test_dash_run","a","--","b","-s","init","stop"]),

    %% Test error conditions
    ?assertNotEqual(
       nomatch,
       string:find(run_dash_test(["-s","a"]),
                   "Error! Failed to load module 'a' because it cannot be found.")),

    ?assertNotEqual(
       nomatch,
       string:find(run_dash_test(["-s",?MODULE]),
                   "Error! init_SUITE:start/0 is not exported.")),

    ?assertNotEqual(
       nomatch,
       string:find(run_dash_test(["-s",?MODULE,"a"]),
                   "Error! init_SUITE:a/0 is not exported.")),

    ok.

dash_extra(Config) ->
    %% Test that arguments are passed correctly
    {[]} = run_dash_extra_test([]),
    {["a"]} = run_dash_extra_test(["a"]),
    {["--help"]} = run_dash_extra_test(["--help"]),
    {["-S","--"]} = run_dash_extra_test(["-S","--"]),
    {["-extra","--"]} = run_dash_extra_test(["-extra","--"]),
    {["-run"]} = run_dash_extra_test(["-run"]),
    {["-args_file"]} = run_dash_extra_test(["-args_file"]),
    {["+A","-1"]} = run_dash_extra_test(["+A","-1"]),
    {["-s","init","stop"]} = run_dash_extra_test(["-s","init","stop"]),

    %% Test that environment variables are handled correctly
    {["b","c","d","a"]} =
        run_dash_extra_test([{"ERL_AFLAGS","b"},{"ERL_FLAGS","c"},{"ERL_ZFLAGS","d"}],
                            ["a"]),
    {["c","d","+A","1","--","a"]} =
        run_dash_extra_test([{"ERL_AFLAGS","-extra +A 1 --"},{"ERL_FLAGS","c"},{"ERL_ZFLAGS","d"}],
                            ["a"]),
    {["+A","a","+B","+C"]} =
        run_dash_extra_test([{"ERL_AFLAGS","-extra +A"},
                             {"ERL_FLAGS","-extra +B"},
                             {"ERL_ZFLAGS","-extra +C"}],["a"]),

    %% Test that arguments from -args_file work as they should
    ArgsFile = filename:join(?config(priv_dir, Config),
                             atom_to_list(?MODULE) ++ "_args_file.args"),
    NestedArgsFile = filename:join(?config(priv_dir, Config),
                             atom_to_list(?MODULE) ++ "_nexted_args_file.args"),
    file:write_file(NestedArgsFile,"y -extra +Y"),

    file:write_file(ArgsFile,["z -args_file ",NestedArgsFile," -extra +Z"]),

    {["c","z","y","d",
      %% -extra starts here
      "a","+Y","+Z","b"]} =
        run_dash_extra_test([{"ERL_FLAGS",["c -args_file ",ArgsFile," d -extra b"]}],["a"]),

    ok.

run_dash_extra_test(Args) ->
    run_dash_extra_test([], Args).
run_dash_extra_test(Prefix, Args) ->
    run_dash_test(Prefix, ["-run", ?MODULE, "test_dash_extra", "-extra" | Args]).

test_dash_extra() ->
    AllArgs = {init:get_plain_arguments(), erlang:system_info(emu_args)},
    io:format("~p.",[AllArgs]),
    erlang:halt().


run_dash_test(Args) ->
    run_dash_test([],Args).
run_dash_test(Env, Args) ->
    [Exec | ExecArgs] = string:split(ct:get_progname()," ", all),
    PortExec = os:find_executable(Exec),
    PortArgs = ExecArgs ++ ["-pa",filename:dirname(code:which(?MODULE)),
                            "-noshell" | Args],
    PortEnv = [{"ERL_CRASH_DUMP_SECONDS","0"} |
               [{K,lists:flatten(V)} || {K, V} <- Env]],
    ct:log("Exec: ~p~nPortArgs: ~p~nPortEnv: ~p~n",[PortExec, PortArgs, PortEnv]),
    Port =
        open_port({spawn_executable, PortExec},
                  [stderr_to_stdout,binary,out,in,hide,exit_status,
                   {args, PortArgs}, {env, PortEnv}]),
    receive
        {Port,{exit_status,N}} ->
            N
    after 5000 ->
            ct:fail({timeout, receive M -> M after 0 -> [] end})
    end,
    Res = unicode:characters_to_list(
            iolist_to_binary(
              (fun F() ->
                       receive
                           {Port,{data,Data}} ->
                               [Data | F()]
                       after 0 ->
                               []
                       end
               end)())),
    ct:log("Res: ~ts~n",[Res]),
    maybe
        {ok, Toks, _} ?= erl_scan:string(Res),
        {ok, Tuple} ?= erl_parse:parse_term(Toks),
        erlang:delete_element(tuple_size(Tuple), Tuple)
    else
        _ ->
            Res
    end.

%% Misc. functions    

args() ->
    ["-a", "kalle", "--", "a", "b", "-d", "-b", "hej", "hopp",
        "--", "c", "d", "-b", "san", "sa", "-c", "4", "5", "6", "-c", "7", "8", "9"].

long_args(A) ->
    ["-a", "kalle", "--", "a", "b", "-d", "-b", "hej", "hopp", "--", "c", A,
        "-b", "san", "sa", "-c", "4", "5", "6", "-c", "7", "8", "9"].

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

