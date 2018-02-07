%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2016. All Rights Reserved.
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
-module(release_handler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("test_lib.hrl").

-compile([export_all, nowarn_export_all]).
-export([scheduler_wall_time/0, garbage_collect/0]). %% rpc'ed

% Default timetrap timeout (set in init_per_testcase).
%-define(default_timeout, ?t:minutes(40)).
-define(default_timeout, ?t:minutes(10)).

suite() ->
    [{ct_hooks, [ts_install_cth]}].

init_per_suite(Config) ->
    init_priv_dir(Config),
    application:start(sasl),
    Config.

end_per_suite(Config) ->
    clean_priv_dir(Config,true),
    ok.

all() -> 
    case os:type() of
	{unix, _} -> unix_cases();
	{win32, _} -> win32_cases()
    end.

unix_cases() -> 
    RunErl = filename:join([code:root_dir(),"bin","run_erl"]),
    RunErlCases = case filelib:is_file(RunErl) of
		true ->  [{group, release}];
		false -> [no_run_erl]
	    end,
    [target_system, target_system_unicode] ++ RunErlCases ++ cases().

win32_cases() ->
    [{group,release} | cases()].

%% Cases that can be run on all platforms
cases() ->
    [otp_2740, otp_2760, otp_5761, otp_9402, otp_9417,
     otp_9395_check_old_code, otp_9395_check_and_purge,
     otp_9395_update_many_mods, otp_9395_rm_many_mods,
     instructions, eval_appup, eval_appup_with_restart,
     supervisor_which_children_timeout,
     release_handler_which_releases, install_release_syntax_check,
     upgrade_supervisor, upgrade_supervisor_fail, otp_9864,
     otp_10463_upgrade_script_regexp, no_dot_erlang, unicode_upgrade].

groups() ->
    [{release,[],
      [
       {group,release_single},
       {group,release_gg}
      ]},
     {release_single,[],
      [
       upgrade,
       upgrade_restart,
       client1,
       client2
      ]},
     {release_gg,[],
      [
       upgrade_gg
      ]}].

%% {group,release}
%% Top group for all cases using run_erl
init_per_group(release, Config) ->
    case {os:type(), os:version()} of
	{{win32, nt}, Vsn} when Vsn > {6,1,999999} ->
	    {skip, "Requires admin privileges on Win 8 and later"};
	_ ->
	    Dog = ?t:timetrap(?default_timeout),
	    P1gInstall = filename:join(priv_dir(Config),p1g_install),
	    ok = create_p1g(Config,P1gInstall),
	    ok = create_p1h(Config),
	    ?t:timetrap_cancel(Dog)
    end;

%% {group,release_single}
%% Subgroup of {group,release}, contains all cases that are not
%% related to global_group
init_per_group(release_single, Config) ->
    Dog = ?t:timetrap(?default_timeout),

    %% Create some more releases to upgrade to
    ok = create_p1i(Config),
    ok = create_p2a(Config),
    ok = create_p2b(Config),

    ?t:timetrap_cancel(Dog);

%% {group,release_gg}
%% Subgroup of {group,release}. global_group tests.
init_per_group(release_gg, Config0) ->
    Config = [{sname_prefix,release_gg}|Config0],

    PrivDir = priv_dir(Config),
    Dog = ?t:timetrap(?default_timeout),

    reg_print_proc(), %% starts a printer process on this node

    Snames = [Gg1Sname,Gg2Sname,Gg3Sname,Gg4Sname,Gg5Sname,Gg6Sname] =
	gg_node_snames(Config),

    %% kill all possible nodes which are to be used
    ok = stop_nodes([node_name(Sname) || Sname <- Snames]),

    %% For gg1, gg3, gg4 and gg5: create a target system running
    %% P1G, and with P1H unpacked.
    %% For gg2 and gg6: create a target system running P1H.
    %% Use gg2 for unpacking and permanenting P1H.
    ok = copy_installed(Config,p1g_install,[Gg2Sname]),
    InstallNode = unpack_p1h(Config,Gg2Sname),
    ok = copy_installed(Config,Gg2Sname,[Gg1Sname,Gg3Sname,Gg4Sname,Gg5Sname]),
    ok = permanent_p1h(InstallNode),
    ok = stop_nodes([InstallNode]),
    ok = copy_installed(Config,Gg2Sname,[Gg6Sname]),

    %% Replace the sys.config files
    %% The reason for not creating the releases with these configs in
    %% the first place (create_p1g, create_p1h) is that then the
    %% InstallNode (gg2) will be very slow started since it will try
    %% to synch with the other nodes in the global group.
    %% Also, the rpc call for installing the P1H release (in
    %% permanent_p1h/1) would return {rpc,nodedown} due to change of
    %% global groups.
    lists:foreach(
      fun(Sname) ->
	      ReleasesDir = filename:join([PrivDir,Sname,"releases"]),
	      write_term_file(filename:join([ReleasesDir,"P1G","sys.config"]),
			      gg_config([Gg1Sname,Gg3Sname,Gg4Sname,Gg5Sname])),
	      write_term_file(filename:join([ReleasesDir,"P1H","sys.config"]),
			      gg_config([Gg1Sname,Gg2Sname,Gg4Sname,
					 Gg5Sname,Gg6Sname]))
      end,
      Snames),

    ?t:timetrap_cancel(Dog),
    [{snames,Snames}|Config].


end_per_group(release, Config) ->
    Dog = ?t:timetrap(?default_timeout),
    stop_print_proc(),
    case os:type() of
	{win32,_} -> delete_all_services();
	_ -> ok
    end,
    ?t:timetrap_cancel(Dog),
    Config;
end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(Case, Config0) ->
    Dog = test_server:timetrap(?default_timeout),
    Config = [{sname_prefix,Case},{watchdog, Dog}|Config0],
    try apply(?MODULE,Case,[cleanup,Config])
    catch error:undef -> ok
    end,
    ?t:format("~n======= init_per_testcase done =======~n",[]),
    Config.

end_per_testcase(Case, Config) ->
    ?t:format("~n======= start end_per_testcase =======~n",[]),
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),

    try apply(?MODULE,Case,[cleanup,Config])
    catch error:undef -> ok
    end,

    %% DEBUG
    case ?config(tc_status,Config) of
	ok ->
	    ok;
	_Fail ->
	    %% save logs from master and client nodes
	    PrivDir = priv_dir(Config),
	    SaveDir = filename:join(PrivDir,save),
	    FailDir = filename:join(SaveDir,lists:concat(["failed-",Case])),
	    ok = filelib:ensure_dir(filename:join(FailDir,"*")),

	    LogDirs =
		filelib:wildcard(filename:join([PrivDir,"*",log])) ++
		filelib:wildcard(filename:join([PrivDir,"*",clients,
						type1,"*",log])),

	    lists:foreach(
	      fun(LogDir) ->
		      ["log",Sname|_] = lists:reverse(filename:split(LogDir)),
		      copy_tree(Config,LogDir,Sname,FailDir)
	      end,
	      LogDirs),

	    case filelib:is_file("sasl_erl_crash.dump") of
		true ->
		    copy_file("sasl_erl_crash.dump",FailDir);
		_ ->
		    ok
	    end

    end,
    %% End DEBUG

    %% Remove any remaining sasl_erl_crash.dump
    %% These can occur when a new master@<host> is started, before
    %% the old usage of the name is unregistered, causing the node to
    %% terminate. (This has no effect on the test case, as the node is
    %% immediately restarted by heart and the test cases wait until
    %% the node is actually up and running -- see wait_nodes_up/2)
    file:delete("sasl_erl_crash.dump"),
    ok.

gg_node_snames(Config) ->
    [tc_sname(Config,X) || X <- [gg1,gg2,gg3,gg4,gg5,gg6]].


%%%-----------------------------------------------------------------
%%% TEST CASES


%% Executed instead of release group when no run_erl program exists
no_run_erl(Config) when is_list(Config) ->
    {comment, "No run_erl program"}.

break(Config) ->
	erlang:display(test_break),
	?t:break(priv_dir(Config)),
	ok.

%% Test upgrade and downgrade of erts and other apps on embedded node
upgrade(Conf) when is_list(Conf) ->
    reg_print_proc(), %% starts a printer process on test_server node
    ?t:format("upgrade ~p~n",[reg_print_proc]),
    PrivDir = priv_dir(Conf),
    Sname = tc_sname(Conf), % nodename for use in this testcase

    %% Copy the P1G release to a directory for use in this testcase
    ok = copy_installed(Conf,p1g_install,[Sname]),

    %% start the test node
    [TestNode] = start_nodes(Conf,[Sname],"upgrade start"),

    %% unpack and install P1H
    ok = rpc_inst(TestNode, install_1, [PrivDir]),
    stop_cover(TestNode),
    reboot_and_wait(TestNode,"install_1"),

    %% reinstall P1H and make it permanent
    ok = rpc_inst(TestNode, install_2, []),
    stop_cover(TestNode),
    reboot_and_wait(TestNode,"install_2",[a]),

    %% check that P1H is permanent, unpack and install P1I, unpack P2A
    ok = rpc_inst(TestNode, install_3, [PrivDir]),
    stop_cover(TestNode),
    reboot_and_wait(TestNode,"install_3",[a]),

    %% check that P1H is used, install P2A
    TestNodeInit1 = rpc:call(TestNode,erlang,whereis,[init]),
    stop_cover(TestNode),
    ok = rpc_inst(TestNode, install_4, []),
    wait_nodes_up([{TestNode,TestNodeInit1}],"install_4",[a]),

    %% check that P2A is used, then downgrade to P1I
    TestNodeInit2 = rpc:call(TestNode,erlang,whereis,[init]),
    ok = rpc_inst(TestNode, install_5, []),
    stop_cover(TestNode),
    ok = rpc_inst(TestNode, install_5a, []),
    wait_nodes_up([{TestNode,TestNodeInit2}],"install_5",[a]),

    %% Check that P1I is used, then make P1I permanent and install P2A
    TestNodeInit3 = rpc:call(TestNode,erlang,whereis,[init]),
    ok = rpc_inst(TestNode, install_6, []),
    stop_cover(TestNode),
    ok = rpc_inst(TestNode, install_6a, []),
    wait_nodes_up([{TestNode,TestNodeInit3}],"install_6",[a]),

    %% check that P2A is used, then downgrade to P1H
    TestNodeInit4 = rpc:call(TestNode,erlang,whereis,[init]),
    ok = rpc_inst(TestNode, install_7, []),
    stop_cover(TestNode),
    ok = rpc_inst(TestNode, install_7a, []),
    wait_nodes_up([{TestNode,TestNodeInit4}],"install_7",[a]),

    %% check that P1H is used, then install P1I and check that it is permanent
    %% then reinstall P2A
    TestNodeInit5 = rpc:call(TestNode,erlang,whereis,[init]),
    ok = rpc_inst(TestNode, install_8, []),
    stop_cover(TestNode),
    ok = rpc_inst(TestNode, install_8a, []),
    wait_nodes_up([{TestNode,TestNodeInit5}],"install_8",[a]),

    %% check that P2A is used, make P2A permanent
    ok = rpc_inst(TestNode, install_9, []),
    stop_cover(TestNode),
    reboot_and_wait(TestNode,"install_9",[a]),

    %% check that P2A is permanent, reboot to old P1H
    TestNodeInit6 = rpc:call(TestNode,erlang,whereis,[init]),
    stop_cover(TestNode),
    ok = rpc_inst(TestNode, install_10, []),
    wait_nodes_up([{TestNode,TestNodeInit6}],"install_10",[a]),

    %% check that P1H is permanent, remove P1I and P2A
    ok = rpc_inst(TestNode, install_11, []),
    stop_cover(TestNode),
    reboot_and_wait(TestNode,"install_11",[a]),

    %% check that P1H is permanent, reboot old P1G
    TestNodeInit7 = rpc:call(TestNode,erlang,whereis,[init]),
    stop_cover(TestNode),
    ok = rpc_inst(TestNode, install_12, []),
    wait_nodes_up([{TestNode,TestNodeInit7}],"install_12"),

    %% check that P1G is permanent, remove P1H
    ok = rpc_inst(TestNode, install_13, []),
    stop_cover(TestNode),
    reboot_and_wait(TestNode,"install_13"),

    %% check that P1G is permanent
    ok = rpc_inst(TestNode, install_14, []),

    ok.

upgrade(cleanup,Config) ->
    TestNode = tc_full_node_name(Config),
    ok = stop_nodes([TestNode]).

reboot_and_wait(Node,Tag) ->
    reboot_and_wait(Node,Tag,[]).

reboot_and_wait(Node,Tag,Apps) ->
    InitPid = rpc:call(Node,erlang,whereis,[init]),
    ok = rpc:call(Node,init,reboot,[]),
    wait_nodes_up([{Node,InitPid}],Tag,Apps).


%% Test upgrade and downgrade of erts in combination with the
%% restart_emulator option to systools:make_relup. For upgrade, this
%% should cause one restart before the upgrade code, and one
%% after. For downgrade, there will be one restart only - at the end.
upgrade_restart(Conf) when is_list(Conf) ->
    reg_print_proc(), %% starts a printer process on test_server node
    ?t:format("upgrade_restart ~p~n",[reg_print_proc]),
    PrivDir = priv_dir(Conf),
    Sname = tc_sname(Conf), % nodename for use in this testcase

    %% Copy the P1G release to a directory for use in this testcase
    ok = copy_installed(Conf,p1g_install,[Sname]),

    %% start the test node
    [TestNode] = start_nodes(Conf,[Sname],"upgrade_restart start"),

    %% unpack and install P2B
    TestNodeInit1 = rpc:call(TestNode,erlang,whereis,[init]),
    ok = rpc_inst(TestNode, upgrade_restart_1, [PrivDir]),
    stop_cover(TestNode),
    ok = rpc_inst(TestNode, upgrade_restart_1a, []),
    wait_nodes_up([{TestNode,TestNodeInit1}],"upgrade_restart_1",[a]),

    %% install P1G
    case rpc_inst(TestNode, upgrade_restart_2, []) of
	ok ->
	    ok;
	{wait,TestNodeInit2a} ->
	    %% We catched the node too early - it was supposed to
	    %% restart twice, so let's wait for one more restart.
	    wait_nodes_up([{TestNode,TestNodeInit2a}],"upgrade_restart_2a",[]),
	    ok = rpc_inst(TestNode, upgrade_restart_2a, [])
    end,
    TestNodeInit2 = rpc:call(TestNode,erlang,whereis,[init]),
    stop_cover(TestNode),
    ok = rpc_inst(TestNode, upgrade_restart_2b, []),
    wait_nodes_up([{TestNode,TestNodeInit2}],"upgrade_restart_2b",[]),

    %% Check that P1G is going again
    ok = rpc_inst(TestNode, upgrade_restart_3, []),

    ok.

upgrade_restart(cleanup,Config) ->
    TestNode = tc_full_node_name(Config),
    ok = stop_nodes([TestNode]).


%% Test upgrade and downgrade of erts, diskless
client1(Conf) when is_list(Conf) ->
    reg_print_proc(), %% starts a printer process on test_server node
    PrivDir = priv_dir(Conf),
    Master = tc_sname(Conf,master),
    Client = tc_sname(Conf,client),
    MasterDir = filename:join(PrivDir,Master),

    %% Copy the P1G release to a directory for use in this testcase
    ok = copy_installed(Conf,p1g_install,[Master]),
    ok = copy_client(Conf,Master,Client,client1),

    %% start the master node
    [TestNode] = start_nodes(Conf,[Master],"client1"),

    ok = rpc_inst(TestNode, client1_1, [PrivDir,MasterDir,Client]),

    ok.

client1(cleanup,Config) ->
    MasterNode = tc_full_node_name(Config,master),
    ClientNode = tc_full_node_name(Config,client),
    ok = stop_nodes([MasterNode,ClientNode]).



%% Test diskless release handling when illegal master node
client2(Conf) when is_list(Conf) ->
    reg_print_proc(), %% starts a printer process on test_server node
    PrivDir = priv_dir(Conf),
    Master = tc_sname(Conf,master),
    Client = tc_sname(Conf,client),

    %% Copy the P1G release to a directory for use in this testcase
    ok = copy_installed(Conf,p1g_install,[Master]),
    ok = copy_client(Conf,Master,Client,client2),

    %% start the master node
    [TestNode] = start_nodes(Conf,[Master],"client2"),

    ok = rpc_inst(TestNode, client2, [PrivDir,Client]),

    ok.

client2(cleanup,Config) ->
    MasterNode = tc_full_node_name(Config,master),
    ClientNode = tc_full_node_name(Config,client),
    ok = stop_nodes([MasterNode,ClientNode]).



%% Test instructions _not_ tested by the installer module.
instructions(Conf) when is_list(Conf) ->
    DataDir = ?config(data_dir, Conf),

    Dir = filename:join(DataDir, "c"),
    true = code:add_path(Dir),
    check_bstate("no", []),
    ok = application:start(c),
    ok = wait_for(bb),
    check_bstate("first", []),
    FirstBB = whereis(bb),

    case whereis(cc) of
        Pid when is_pid(Pid) -> ok;
        _ -> ?t:fail("cc not started")
    end,

    %% Stop and start cc process
    S1 = [point_of_no_return,
          {stop, [aa]},
          {apply, {?MODULE, no_cc, []}},
          {start, [aa]}],
    {ok, _} = release_handler_1:eval_script(S1),

    case whereis(cc) of
        Pid2 when is_pid(Pid2) -> ok;
        _ -> ?t:fail("cc not started")
    end,

    %% Make bb run old version of b.
    S2 = [point_of_no_return,
          {remove, {b, soft_purge, soft_purge}}],
    {ok, [{b, soft_purge}]} = release_handler_1:eval_script(S2),
    check_bstate("first", [FirstBB]),

    false = code:is_loaded(b),
    {error,{old_processes,b}} = release_handler_1:eval_script(S2),
    check_bstate("first", [FirstBB]),

    %% Let supervisor restart bb with new code
    S3 = [point_of_no_return,
          {purge, [b]}],
    {ok, []} = release_handler_1:eval_script(S3),
    ok = wait_for(bb),
    check_bstate("second", []),
    SecondBB = whereis(bb),

    if
        SecondBB =:= FirstBB -> ?t:fail("bb not killed");
        true -> ok
    end,

    %% Restart bb yet another time
    ok = application:stop(c),
    ok = application:start(c),
    ok = wait_for(bb),
    check_bstate("third", []),
    ThirdBB = whereis(bb),

    case ThirdBB of
        _ when is_pid(ThirdBB) -> ok;
        undefined -> ?t:fail("bb not started")
    end,

    %% Make bb run old version of b.
    %%c:l(b),
    check_bstate("third", []),
    false = code:purge(b),
    check_bstate("third", []),
    {module,b} = code:load_file(b),
    check_bstate("third", [ThirdBB]),

    %% Let supervisor restart bb yet another time
    S4 = [point_of_no_return,
          {remove, {b, brutal_purge, soft_purge}}],
    {ok, HopefullyEmpty} = release_handler_1:eval_script(S4),
    ok = wait_for(bb),
    FourthBB = whereis(bb),

    case HopefullyEmpty of
	[{b, soft_purge}] ->
	    %% The process managed to start between purge and delete
	    check_bstate("fourth", [FourthBB]);
	[] ->
	    %% The process started after delete
	    check_bstate("fourth", [])
    end,

    application:stop(c),
    check_bstate("no", []),
    ok.

instructions(cleanup,Conf) ->
    application:stop(c),
    really_del_code([aa,b,c_sup]),
    code:del_path(filename:join(?config(data_dir,Conf), "c")),
    ok.

really_del_code(Mods) ->
    lists:foreach(fun(Mod) ->
			  code:purge(Mod), % remove old code
			  code:delete(Mod),% make current code old
			  code:purge(Mod)  % remove old code
		  end,
		  Mods).

check_bstate(Slogan,ExpectedProcs) ->
    BB = whereis(bb),
    ActualProcs = lists:sort([P || P <- processes(),
				   erlang:check_process_code(P, b)]),
    ExpectedProcs2 = lists:sort(ExpectedProcs),
    ?t:format("check_bstate:~n~p~n~p~n",
	      [{"bb process", Slogan, BB},
	       {"Processes running old b code", ActualProcs}]),
    if
	Slogan =:= "no", BB =/= undefined ->
	    ?t:fail("instructions failed; process bb is running");
	Slogan =/= "no", BB =:= undefined ->
	    ?t:fail("instructions failed; process bb is not running");
	ExpectedProcs2 =:= [], ActualProcs =/= ExpectedProcs2 ->
	    ?t:fail("instructions failed; old b processes are running");
	ActualProcs =/= ExpectedProcs2 ->
	    ?t:fail("instructions failed; wrong number of old b processes are running");
	true ->
	    ok
    end.

wait_for(Name) ->
    case whereis(Name) of
        undefined ->
            timer:sleep(100),
            wait_for(Name);
        Pid when is_pid(Pid) ->
            ok
    end.

no_cc() ->
    case whereis(cc) of
        Pid when is_pid(Pid) -> ?t:fail("cc not stopped");
        _ -> ok
    end.



%%%-----------------------------------------------------------------
%%% Testing of reported bugs and other tickets.
%%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% release_handler_1:get_supervised_procs/0 test
%%-----------------------------------------------------------------
supervisor_which_children_timeout(Conf) ->
    PrivDir = priv_dir(Conf),
    Dir = filename:join(PrivDir,"supervisor_which_children_timeout"),
    DataDir = ?config(data_dir,Conf),
    LibDir = filename:join([DataDir,release_handler_timeouts]),

    Rel1 = create_and_install_fake_first_release(Dir,[{dummy,"0.1",LibDir}]),

    {ok, Node} = t_start_node(supervisor_which_children_timeout, Rel1, []),
    Proc = rpc:call(Node, erlang, whereis, [dummy_sup_2]),
    ok = rpc:call(Node, sys, suspend, [Proc]),
    Result = {badrpc, {'EXIT', {suspended_supervisor, _}}} =
        rpc:call(Node, release_handler_1, get_supervised_procs, []),
    ?t:format("release_handler_1:get_supervised_procs/0: ~p~n", [Result]),

    ok.

supervisor_which_children_timeout(cleanup, _Conf) ->
    stop_node(node_name(supervisor_which_children_timeout)).


%% Test that check_install_release will fail for illegal relup
%% instructions, even after point of no return.
install_release_syntax_check(Conf) when is_list(Conf) ->

    S1 = [point_of_no_return, illegal_instruction],
    {error,{illegal_instruction_after_point_of_no_return,illegal_instruction}} =
	release_handler_1:check_script(S1,[]),

    S2 = [point_of_no_return,restart_new_emulator],
    {error,{illegal_instruction_after_point_of_no_return,restart_new_emulator}} =
	release_handler_1:check_script(S2,[]),

    ok.


%%-----------------------------------------------------------------
%% release_handler:which_releases/0 and 1 test
%%-----------------------------------------------------------------
release_handler_which_releases(Conf) ->
    PrivDir = priv_dir(Conf),
    Dir = filename:join(PrivDir,"release_handler_which_releases"),
    DataDir = ?config(data_dir,Conf),
    LibDir = filename:join([DataDir,release_handler_timeouts]),

    Rel1 = create_and_install_fake_first_release(Dir,[{dummy,"0.1",LibDir}]),

    {ok, Node} = t_start_node(release_handler_which_releases, Rel1, []),
    Releases0 = rpc:call(Node, release_handler, which_releases, []),
    Releases1 = rpc:call(Node, release_handler, which_releases, [permanent]),
    Releases2 = rpc:call(Node, release_handler, which_releases, [old]),

    1 = length(Releases0),
    1 = length(Releases1),
    0 = length(Releases2),

    ?t:format("release_handler:which_releases/0: ~p~n", [Releases0]),
    ?t:format("release_handler:which_releases/1: ~p~n", [Releases1]),
    ?t:format("release_handler:which_releases/1: ~p~n", [Releases2]),

    ok.

release_handler_which_releases(cleanup,_Conf) ->
    stop_node(node_name(release_handler_which_releases)).

%%-----------------------------------------------------------------
%% Ticket: OTP-2740
%% Slogan: vsn not numeric doesn't work so good in release_handling
%%-----------------------------------------------------------------
%% Test vsn.
otp_2740(Conf) ->
    DataDir = ?config(data_dir, Conf),
    Dir = filename:join(DataDir, "otp_2740"),
    true = code:add_path(Dir),

    {module, vsn_numeric} = c:l(vsn_numeric),
    {module, vsn_tuple} = c:l(vsn_tuple),
    {module, vsn_list} = c:l(vsn_list),
    {module, vsn_atom} = c:l(vsn_atom),
    {module, vsn_string} = c:l(vsn_string),

    231894 = release_handler_1:get_current_vsn(vsn_numeric),
    {tuple,["of",terms]} = release_handler_1:get_current_vsn(vsn_tuple),
    [list,"of",{some,terms}] = release_handler_1:get_current_vsn(vsn_list),
    atom = release_handler_1:get_current_vsn(vsn_atom),
    "a string" = release_handler_1:get_current_vsn(vsn_string),

    true = code:del_path(Dir),
    ok.

%%-----------------------------------------------------------------
%% Ticket: OTP-2760
%% Slogan: when an application is removed from a node it is not unloaded
%%-----------------------------------------------------------------
%% Test that when an application is removed from a node it is also unloaded.
otp_2760(Conf) ->
    PrivDir = priv_dir(Conf),
    Dir = filename:join(PrivDir,"otp_2760"),
    DataDir = ?config(data_dir,Conf),
    LibDir = filename:join([DataDir,app1_app2,lib1]),

    Rel1 = create_and_install_fake_first_release(Dir,[{app1,"1.0",LibDir}]),
    Rel2 = create_fake_upgrade_release(Dir,"after",[],{[Rel1],[Rel1],[LibDir]}),
    Rel2Dir = filename:dirname(Rel2),

    %% Start a node with Rel1.boot and check that the app1 module is loaded
    {ok, Node} = t_start_node(otp_2760, Rel1, []),
    {file, _} = rpc:call(Node, code, is_loaded, [app1]),

    %% Execute the relup script and check that app1 is unloaded
    {ok, [{"after", [{_Rel1Vsn, _Descr, Script}], _}]} =
	file:consult(filename:join(Rel2Dir, "relup")),
    {ok, []} = rpc:call(Node, release_handler_1, eval_script, [Script]),
    false = rpc:call(Node, code, is_loaded, [app1]),

    ok.

otp_2760(cleanup,_Conf) ->
    stop_node(node_name(otp_2760)).


%% Test upgrade using other filesystem than the defined in OTP and
%% option {update_paths, true}
otp_5761(Conf) when is_list(Conf) ->

    %% In the following test case, the release upgrade is somewhat
    %% simplified (since it is not this procedure in itself we want to
    %% test, but that application code directories are set correctly.)
    %% Existing Erlang release is used as base, instead of creating
    %% a new one.

    %% Set some paths
    PrivDir = priv_dir(Conf),
    Dir = filename:join(PrivDir,"otp_5761"),
    RelDir = filename:join(?config(data_dir, Conf), "app1_app2"),
    LibDir1 = filename:join(RelDir, "lib1"),
    LibDir2 = filename:join(RelDir, "lib2"),

    %% Create the releases
    Rel1 = create_and_install_fake_first_release(Dir,
						 [{app1,"1.0",LibDir1},
						  {app2,"1.0",LibDir1}]),
    Rel2 = create_fake_upgrade_release(Dir,
				       "2",
				       [{app1,"2.0",LibDir2},
					{app2,"1.0",LibDir2}],
				       {[Rel1],[Rel1],[LibDir1]}),
    Rel1Dir = filename:dirname(Rel1),
    Rel2Dir = filename:dirname(Rel2),
    
    %% Start a slave node
    {ok, Node} = t_start_node(otp_5761, Rel1, filename:join(Rel1Dir,"sys.config")),

    %% Bind some variable names that will be used in patternmatching below 
    App11Dir = filename:join([LibDir1, "app1-1.0"]),
    App12Dir = filename:join([LibDir2, "app1-2.0"]),
    App2aDir = filename:join([LibDir1, "app2-1.0"]),
    App2bDir = filename:join([LibDir2, "app2-1.0"]),

    %% Make sure correct code paths are used
    App11Dir = rpc:call(Node, code, lib_dir, [app1]),
    App2aDir = rpc:call(Node, code, lib_dir, [app2]),

    %% Unpack rel2 (make sure it does not work if an AppDir is bad)
    LibDir3 = filename:join(RelDir, "lib3"),
    {error, {no_such_directory, _}} =
	rpc:call(Node, release_handler, set_unpacked,
		 [Rel2++".rel", [{app1,"2.0",LibDir2}, {app2,"1.0",LibDir3}]]),
    {ok, RelVsn2} =
	rpc:call(Node, release_handler, set_unpacked,
		 [Rel2++".rel", [{app1,"2.0",LibDir2}, {app2,"1.0",LibDir2}]]),
    ok = rpc:call(Node, release_handler, install_file,
			[RelVsn2, filename:join(Rel2Dir, "relup")]),
    ok = rpc:call(Node, release_handler, install_file,
			[RelVsn2, filename:join(Rel2Dir, "start.boot")]),
    ok = rpc:call(Node, release_handler, install_file,
			[RelVsn2, filename:join(Rel2Dir, "sys.config")]),

    %% Install RelVsn2 without {update_paths, true} option
    {ok, RelVsn1, []} =
	rpc:call(Node, release_handler, install_release, [RelVsn2]),
    App12Dir = rpc:call(Node, code, lib_dir, [app1]),
    App2aDir = rpc:call(Node, code, lib_dir, [app2]),

    %% Install RelVsn1 again
    {ok, RelVsn1, []} =
	rpc:call(Node, release_handler, install_release, [RelVsn1]),

    %% Install RelVsn2 with {update_paths, true} option
    {ok, RelVsn1, []} =
	rpc:call(Node, release_handler, install_release,
		 [RelVsn2, [{update_paths, true}]]),
    App12Dir = rpc:call(Node, code, lib_dir, [app1]),
    App2bDir = rpc:call(Node, code, lib_dir, [app2]),

    %% Install RelVsn1 again
    {ok, RelVsn1, []} =
	rpc:call(Node, release_handler, install_release,
		 [RelVsn1, [{update_paths, true}]]),
    App11Dir = rpc:call(Node, code, lib_dir, [app1]),
    App2aDir = rpc:call(Node, code, lib_dir, [app2]),

    ok.

otp_5761(cleanup,_Conf) ->
    stop_node(node_name(otp_5761)).


%% When a new version of an application is added, but no module is
%% changed - the path was not updated - i.e. code:priv_dir would point
%% to the old location.
otp_9402(Conf) when is_list(Conf) ->
    %% Set some paths
    PrivDir = priv_dir(Conf),
    Dir = filename:join(PrivDir,"otp_9402"),
    LibDir = filename:join(?config(data_dir, Conf), "lib"),

    %% Create the releases
    Rel1 = create_and_install_fake_first_release(Dir,
						 [{a,"1.1",LibDir}]),
    Rel2 = create_fake_upgrade_release(Dir,
				       "2",
				       [{a,"1.2",LibDir}],
				       {[Rel1],[Rel1],[LibDir]}),
    Rel1Dir = filename:dirname(Rel1),
    Rel2Dir = filename:dirname(Rel2),

    %% Start a slave node
    {ok, Node} = t_start_node(otp_9402, Rel1, filename:join(Rel1Dir,"sys.config")),

    %% Check path
    Dir1 = filename:join([LibDir, "a-1.1"]),
    Dir1 = rpc:call(Node, code, lib_dir, [a]),
    ABeam = rpc:call(Node, code, which, [a]),

    %% Install second release, with no changed modules
    {ok, RelVsn2} =
	rpc:call(Node, release_handler, set_unpacked,
		 [Rel2++".rel", [{a,"1.2",LibDir}]]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "relup")]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "start.boot")]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "sys.config")]),

    {ok, RelVsn1, []} =
	rpc:call(Node, release_handler, install_release, [RelVsn2]),

    %% Check path
    Dir2 = filename:join([LibDir, "a-1.2"]),
    Dir2 = rpc:call(Node, code, lib_dir, [a]),
    APrivDir2 = rpc:call(Node, code, priv_dir, [a]),
    true = filelib:is_regular(filename:join(APrivDir2,"file")),

    %% Just to make sure no modules have been re-loaded
    ABeam = rpc:call(Node, code, which, [a]),

    %% Install RelVsn1 again
    {ok, _OtherVsn, []} =
	rpc:call(Node, release_handler, install_release, [RelVsn1]),

    %% Check path
    Dir1 = rpc:call(Node, code, lib_dir, [a]),
    APrivDir1 = rpc:call(Node, code, priv_dir, [a]),
    false = filelib:is_regular(filename:join(APrivDir1,"file")),

    %% Just to make sure no modules have been re-loaded
    ABeam = rpc:call(Node, code, which, [a]),

    ok.

otp_9402(cleanup,_Conf) ->
    stop_node(node_name(otp_9402)).


%% When a module is deleted in an appup instruction, the upgrade
%% failed if the module was not loaded.
otp_9417(Conf) when is_list(Conf) ->
    %% Set some paths
    PrivDir = priv_dir(Conf),
    Dir = filename:join(PrivDir,"otp_9417"),
    LibDir = filename:join(?config(data_dir, Conf), "lib"),

    %% Create the releases
    Rel1 = create_and_install_fake_first_release(Dir,
						 [{b,"1.0",LibDir}]),
    Rel2 = create_fake_upgrade_release(Dir,
				       "2",
				       [{b,"2.0",LibDir}],
				       {[Rel1],[Rel1],[LibDir]}),
    Rel1Dir = filename:dirname(Rel1),
    Rel2Dir = filename:dirname(Rel2),

    %% Start a slave node
    {ok, Node} = t_start_node(otp_9417, Rel1, filename:join(Rel1Dir,"sys.config")),

    %% Check paths
    Dir1 = filename:join([LibDir, "b-1.0"]),
    Dir1 = rpc:call(Node, code, lib_dir, [b]),
    BLibBeam = filename:join([Dir1,"ebin","b_lib.beam"]),
    BLibBeam = rpc:call(Node,code,which,[b_lib]),
    false = rpc:call(Node,code,is_loaded,[b_lib]),
    false = rpc:call(Node,code,is_loaded,[b_server]),

    %% Install second release, which removes b_lib module
    {ok, RelVsn2} =
	rpc:call(Node, release_handler, set_unpacked,
		 [Rel2++".rel", [{b,"2.0",LibDir}]]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "relup")]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "start.boot")]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "sys.config")]),

    {ok, _RelVsn1, []} =
	rpc:call(Node, release_handler, install_release, [RelVsn2]),

    %% Check that the module does no longer exist
    false = rpc:call(Node, code, is_loaded, [b_lib]),
    non_existing = rpc:call(Node, code, which, [b_lib]),

    %% And check some paths
    Dir2 = filename:join([LibDir, "b-2.0"]),
    Dir2 = rpc:call(Node, code, lib_dir, [b]),
    BServerBeam = filename:join([Dir2,"ebin","b_server.beam"]),
    {file,BServerBeam} = rpc:call(Node,code,is_loaded,[b_server]),
    ok.

otp_9417(cleanup,_Conf) ->
    stop_node(node_name(otp_9417)).


%% OTP-9395 - performance problems when there are MANY processes
%% Test that the procedure of checking for old code before an upgrade
%% can be started is faster when there is no old code in
%% the system.
otp_9395_check_old_code(Conf) when is_list(Conf) ->

    NProcs = 1000,
    MPath = filename:join([?config(data_dir,Conf),"lib","many_mods-1.0","ebin"]),
    code:add_path(MPath),

    %% Start NProc processes, each referencing each module
    {Modules,Pids} = m:start(NProcs),

    %% Load each module again in order to get old code
    [code:load_file(Mod) || Mod <- Modules],
    true = erlang:check_old_code(m10),

    S = [point_of_no_return |
	 [{remove,{M,soft_purge,soft_purge}} || M <- Modules]],

    %% Do the old code check, then purge, and redo
    {T1,{ok,PurgeMods}} = timer:tc(release_handler_1,check_script,[S,[]]),
    true = (lists:sort(PurgeMods) == lists:sort(Modules)),
    [code:purge(M) || M <- PurgeMods],
    {T2,{ok,[]}} = timer:tc(release_handler_1,check_script,[S,[]]),

    %% Cleanup
    lists:foreach(fun(Pid) -> Pid ! stop end, Pids),
    lists:foreach(fun(Mod) -> code:purge(Mod),
			      code:delete(Mod),
			      code:purge(Mod)
		  end, Modules),
    code:del_path(MPath),

    %% Test that second run was much faster than the first
    if T2 > 0 ->
	    X = T1/T2,
	    ct:log("~p procs, ~p mods -> ~n"
		   "\tWith old code: ~.2f sec~n"
		   "\tAfter purge: ~.2f sec~n"
		   "\tT1/T2: ~.2f",
		   [NProcs,length(Modules),T1/1000000,T2/1000000,X]),
	    if X < 1 ->
		    ct:fail({no_improvement_after_purge,X});
	       true ->
		    ok
	    end;
       T1 > 0 -> %% Means T1/T2 = infinite
	    ok;
       true ->
	    ct:fail({unexpected_values,T1,T2})
    end,
    ok.


%% OTP-9395 - performance problems when there are MANY processes
%% Added option 'purge' to check_install_release
otp_9395_check_and_purge(Conf) when is_list(Conf) ->
    %% Set some paths
    PrivDir = priv_dir(Conf),
    Dir = filename:join(PrivDir,"otp_9395_check_and_purge"),
    LibDir = filename:join(?config(data_dir, Conf), "lib"),

    %% Create the releases
    Rel1 = create_and_install_fake_first_release(Dir,
						 [{b,"1.0",LibDir}]),
    Rel2 = create_fake_upgrade_release(Dir,
				       "2",
				       [{b,"2.0",LibDir}],
				       {[Rel1],[Rel1],[LibDir]}),
    Rel1Dir = filename:dirname(Rel1),
    Rel2Dir = filename:dirname(Rel2),

    %% Start a slave node
    {ok, Node} = t_start_node(otp_9395_check_and_purge, Rel1,
			      filename:join(Rel1Dir,"sys.config")),

    %% Make sure there is old code for b_lib and b_server
    rpc:call(Node,code,load_file,[b_lib]),
    rpc:call(Node,code,load_file,[b_lib]),
    rpc:call(Node,code,load_file,[b_server]),
    rpc:call(Node,code,load_file,[b_server]),
    true = rpc:call(Node,erlang,check_old_code,[b_lib]),
    true = rpc:call(Node,erlang,check_old_code,[b_server]),

    %% Unpack second release, which removes b_lib module and loads b_server
    {ok, RelVsn2} =
	rpc:call(Node, release_handler, set_unpacked,
		 [Rel2++".rel", [{b,"2.0",LibDir}]]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "relup")]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "start.boot")]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "sys.config")]),

    %% Do check_install_release, and check that old code still exists
    {ok, _RelVsn1, []} =
	rpc:call(Node, release_handler, check_install_release, [RelVsn2]),
    true = rpc:call(Node,erlang,check_old_code,[b_lib]),
    true = rpc:call(Node,erlang,check_old_code,[b_server]),

    %% Do check_install_release with option 'purge' and check that old
    %% code is gone
    {ok, _RelVsn1, []} =
	rpc:call(Node, release_handler, check_install_release, [RelVsn2,[purge]]),
    false = rpc:call(Node,erlang,check_old_code,[b_lib]),
    false = rpc:call(Node,erlang,check_old_code,[b_server]),

    ok.

otp_9395_check_and_purge(cleanup,_Conf) ->
    stop_node(node_name(otp_9395_check_and_purge)).


%% OTP-9395 - performance problems when there are MANY processes
%% Upgrade which updates many modules (brutal_purge)
otp_9395_update_many_mods(Conf) when is_list(Conf) ->

    %% "nain" is very slow - it fails this test quite often due to a
    %% long sys call
    %% /proc/cpuinfo: "clock: 1249MHz"
    inet:gethostname() == {ok,"nain"} andalso throw({skip,"slow test host"}),

    %% Set some paths
    PrivDir = priv_dir(Conf),
    Dir = filename:join(PrivDir,"otp_9395_update_many_mods"),
    LibDir = filename:join(?config(data_dir, Conf), "lib"),

    %% Create the releases
    Rel1 = create_and_install_fake_first_release(Dir,
						 [{many_mods,"1.0",LibDir}]),
    Rel2 = create_fake_upgrade_release(Dir,
				       "2",
				       [{many_mods,"1.1",LibDir}],
				       {[Rel1],[Rel1],[LibDir]}),
    Rel1Dir = filename:dirname(Rel1),
    Rel2Dir = filename:dirname(Rel2),

    %% Start a slave node
    PA = filename:dirname(code:which(?MODULE)),
    {ok, Node} = t_start_node(otp_9395_update_many_mods, Rel1,
			      filename:join(Rel1Dir,"sys.config"), "-pa " ++ PA),

    %% Start a lot of processes on the new node, all with refs to each
    %% module that will be updated
    NProcs = 1000,
    {Modules,Pids1} = rpc:call(Node,m,start,[NProcs]),

    %% Then load modules in order to get old code
    [rpc:call(Node,code,load_file,[Mod]) || Mod <- Modules],
    true = rpc:call(Node,erlang,check_old_code,[m10]),

    %% Unpack second release, which updates all mX modules
    {ok, RelVsn2} =
	rpc:call(Node, release_handler, set_unpacked,
		 [Rel2++".rel", [{many_mods,"1.1",LibDir}]]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "relup")]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "start.boot")]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "sys.config")]),

    %% First, install release directly and check how much time it takes
    rpc:call(Node,?MODULE,garbage_collect,[]),
    SWTFlag0 = spawn_link(Node, ?MODULE, scheduler_wall_time, []),
    {TInst0,{ok, _, []}} =
	timer:tc(rpc,call,[Node, release_handler, install_release, [RelVsn2]]),
    SWT0 = rpc:call(Node,erlang,statistics,[scheduler_wall_time]),
%    ct:log("install_release: ~.2f",[TInst0/1000000]),

    %% Restore to old release, spawn processes again and load to get old code
    {_,RelVsn1} = init:script_id(),
    {_TInst1,{ok, _, []}} =
	timer:tc(rpc,call,[Node, release_handler, install_release, [RelVsn1]]),
%    ct:log("install_release: ~.2f",[_TInst1/1000000]),

    [exit(Pid,kill) || Pid <- Pids1],
    {Modules,_Pids2} = rpc:call(Node,m,start,[NProcs]),
    [rpc:call(Node,code,load_file,[Mod]) || Mod <- Modules],
    true = rpc:call(Node,erlang,check_old_code,[m10]),

    %% Run check_install_release with purge before install this time
    {_TCheck,{ok, _RelVsn1, []}} =
	timer:tc(rpc,call,[Node, release_handler, check_install_release,
			   [RelVsn2,[purge]]]),
%    ct:log("check_install_release with purge: ~.2f",[_TCheck/1000000]),

    %% Finally install release after check and purge, and check that
    %% this install was faster than the first.
    SWTFlag0 ! die,
    rpc:call(Node,?MODULE,garbage_collect,[]),
    _SWTFlag1 = spawn_link(Node, ?MODULE, scheduler_wall_time, []),
    {TInst2,{ok, _RelVsn1, []}} =
	timer:tc(rpc,call,[Node, release_handler, install_release, [RelVsn2]]),
    SWT2 = rpc:call(Node,erlang,statistics,[scheduler_wall_time]),
%    ct:log("install_release: ~.2f",[TInst2/1000000]),

    %% Calculate and print real time and CPU utilization
    SumFun = fun({_,A,T},{AAcc,TAcc}) -> {A+AAcc,T+TAcc} end,
    {SumA0,SumT0} = lists:foldl(SumFun,{0,0},SWT0),
    {SumA2,SumT2} = lists:foldl(SumFun,{0,0},SWT2),
    TI0=TInst0/1000000,
    TI2=TInst2/1000000,
    CPU0=SumA0/SumT0,
    CPU2=SumA2/SumT2,
    X0 = TI0*CPU0,
    X2 = TI2*CPU2,
    ct:log("First run:  T=~.2fsec, CPU=~.2f, T*CPU=~.2f~n"
	   "Second run: T=~.2fsec, CPU=~.2f, T*CPU=~.2f~n",
	   [TI0, CPU0, X0, TI2, CPU2, X2]),

    true = (X2 =< X0),  % disregarding wait time for file access etc.

    ok.

scheduler_wall_time() ->
    erlang:system_flag(scheduler_wall_time,true),
    receive _Msg -> normal end.

garbage_collect() ->
    Pids = processes(),
    [erlang:garbage_collect(Pid) || Pid <- Pids].


otp_9395_update_many_mods(cleanup,_Conf) ->
    stop_node(node_name(otp_9395_update_many_mods)).


%% OTP-9395 - performance problems when there are MANY processes
%% Upgrade which removes many modules (brutal_purge)
otp_9395_rm_many_mods(Conf) when is_list(Conf) ->

    %% "nain" is very slow - it fails this test quite often due to a
    %% long sys call
    %% /proc/cpuinfo: "clock: 1249MHz"
    inet:gethostname() == {ok,"nain"} andalso throw({skip,"slow test host"}),

    %% Set some paths
    PrivDir = priv_dir(Conf),
    Dir = filename:join(PrivDir,"otp_9395_rm_many_mods"),
    LibDir = filename:join(?config(data_dir, Conf), "lib"),

    %% Create the releases
    Rel1 = create_and_install_fake_first_release(Dir,
						 [{many_mods,"1.0",LibDir}]),
    Rel2 = create_fake_upgrade_release(Dir,
				       "2",
				       [{many_mods,"2.0",LibDir}],
				       {[Rel1],[Rel1],[LibDir]}),
    Rel1Dir = filename:dirname(Rel1),
    Rel2Dir = filename:dirname(Rel2),

    %% Start a slave node
    PA = filename:dirname(code:which(?MODULE)),
    {ok, Node} = t_start_node(otp_9395_rm_many_mods, Rel1,
			      filename:join(Rel1Dir,"sys.config"), "-pa " ++ PA),

    %% Start a lot of processes on the new node, all with refs to each
    %% module that will be updated
    NProcs = 1000,
    {Modules,Pids1} = rpc:call(Node,m,start,[NProcs]),

    %% Then load modules in order to get old code
    [rpc:call(Node,code,load_file,[Mod]) || Mod <- Modules],
    true = rpc:call(Node,erlang,check_old_code,[m10]),

    %% Unpack second release, which removes all mX modules
    {ok, RelVsn2} =
	rpc:call(Node, release_handler, set_unpacked,
		 [Rel2++".rel", [{many_mods,"2.0",LibDir}]]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "relup")]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "start.boot")]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "sys.config")]),

    %% First, install release directly and check how much time it takes
    rpc:call(Node,?MODULE,garbage_collect,[]),
    SWTFlag0 = spawn_link(Node, ?MODULE, scheduler_wall_time, []),
    {TInst0,{ok, _, []}} =
	timer:tc(rpc,call,[Node, release_handler, install_release, [RelVsn2]]),
    SWT0 = rpc:call(Node,erlang,statistics,[scheduler_wall_time]),
%    ct:log("install_release: ~.2f",[TInst0/1000000]),

    %% Restore to old release, spawn processes again and load to get old code
    {_,RelVsn1} = init:script_id(),
    {_TInst1,{ok, _, []}} =
	timer:tc(rpc,call,[Node, release_handler, install_release, [RelVsn1]]),
%    ct:log("install_release: ~.2f",[_TInst1/1000000]),

    [exit(Pid,kill) || Pid <- Pids1],
    {Modules,_Pids2} = rpc:call(Node,m,start,[NProcs]),
    [rpc:call(Node,code,load_file,[Mod]) || Mod <- Modules],
    true = rpc:call(Node,erlang,check_old_code,[m10]),

    %% Run check_install_release with purge before install this time
    {_TCheck,{ok, _RelVsn1, []}} =
	timer:tc(rpc,call,[Node, release_handler, check_install_release,
			   [RelVsn2,[purge]]]),
%    ct:log("check_install_release with purge: ~.2f",[_TCheck/1000000]),

    %% Finally install release after check and purge, and check that
    %% this install was faster than the first.
    SWTFlag0 ! die,
    rpc:call(Node,?MODULE,garbage_collect,[]),
    _SWTFlag1 = spawn_link(Node, ?MODULE, scheduler_wall_time, []),
    {TInst2,{ok, _RelVsn1, []}} =
	timer:tc(rpc,call,[Node, release_handler, install_release, [RelVsn2]]),
    SWT2 = rpc:call(Node,erlang,statistics,[scheduler_wall_time]),
%    ct:log("install_release: ~.2f",[TInst2/1000000]),

    %% Calculate and print real time and CPU utilization
    SumFun = fun({_,A,T},{AAcc,TAcc}) -> {A+AAcc,T+TAcc} end,
    {SumA0,SumT0} = lists:foldl(SumFun,{0,0},SWT0),
    {SumA2,SumT2} = lists:foldl(SumFun,{0,0},SWT2),
    TI0=TInst0/1000000,
    TI2=TInst2/1000000,
    CPU0=SumA0/SumT0,
    CPU2=SumA2/SumT2,
    X0 = TI0*CPU0,
    X2 = TI2*CPU2,
    ct:log("First run:  T=~.2fsec, CPU=~.2f, T*CPU=~.2f~n"
	   "Second run: T=~.2fsec, CPU=~.2f, T*CPU=~.2f~n",
	   [TI0, CPU0, X0, TI2, CPU2, X2]),

    true = (X2 =< X0),  % disregarding wait time for file access etc.

    ok.

otp_9395_rm_many_mods(cleanup,_Conf) ->
    stop_node(node_name(otp_9395_rm_many_mods)).

otp_9864(Conf) ->
    case os:type() of
	{win32,_} ->
	    {skip,"Testing handling of symlinks - skipped on windows"};
	_ ->
	    do_otp_9864(Conf)
    end.
do_otp_9864(Conf) ->
    %% Set some paths
    PrivDir = priv_dir(Conf),
    Dir = filename:join(PrivDir,"otp_9864"),
    RelDir = filename:join(?config(data_dir, Conf), "app1_app2"),

    %% Copy libs to priv_dir because remove_release will remove some
    %% of these again, and we don't want to remove anything from
    %% data_dir
    copy_tree(Conf,filename:join(RelDir, "lib1"),Dir),
    copy_tree(Conf,filename:join(RelDir, "lib2"),Dir),
    LibDir1 = filename:join(Dir, "lib1"),
    LibDir2 = filename:join(Dir, "lib2"),

    %% Create the releases
    Rel1 = create_and_install_fake_first_release(Dir,
						 [{app1,"1.0",LibDir1},
						  {app2,"1.0",LibDir1}]),
    Rel2 = create_fake_upgrade_release(Dir,
				       "2",
				       [{app1,"2.0",LibDir2},
					{app2,"1.0",LibDir2}],
				       {[Rel1],[Rel1],[LibDir1]}),
    Rel1Dir = filename:dirname(Rel1),
    Rel2Dir = filename:dirname(Rel2),

    %% Start a slave node
    {ok, Node} = t_start_node(otp_9864, Rel1, filename:join(Rel1Dir,"sys.config")),
    
    %% Unpack rel2 (make sure it does not work if an AppDir is bad)
    {ok, RelVsn2} =
	rpc:call(Node, release_handler, set_unpacked,
		 [Rel2++".rel", [{app1,"2.0",LibDir2}, {app2,"1.0",LibDir2}]]),
    ok = rpc:call(Node, release_handler, install_file,
			[RelVsn2, filename:join(Rel2Dir, "relup")]),
    ok = rpc:call(Node, release_handler, install_file,
			[RelVsn2, filename:join(Rel2Dir, "start.boot")]),
    ok = rpc:call(Node, release_handler, install_file,
			[RelVsn2, filename:join(Rel2Dir, "sys.config")]),

    %% Install RelVsn2
    {ok, RelVsn1, []} =
	rpc:call(Node, release_handler, install_release, [RelVsn2]),

    %% Create a symlink inside release 2
    Releases2Dir = filename:join([Dir,"releases","2"]),
    Link = filename:join(Releases2Dir,"foo_symlink_dir"),
    file:make_symlink(Releases2Dir,Link),

    %% Back down to RelVsn1
    {ok, RelVsn1, []} =
	rpc:call(Node, release_handler, install_release, [RelVsn1]),

    %% This will fail if symlinks are not handled
    ok = rpc:call(Node, release_handler, remove_release, [RelVsn2]),

    ok.

otp_9864(cleanup,_Conf) ->
    stop_node(node_name(otp_9864)).


upgrade_supervisor(Conf) when is_list(Conf) ->
    %% Set some paths
    PrivDir = priv_dir(Conf),
    Dir = filename:join(PrivDir,"upgrade_supervisor"),
    LibDir = filename:join(?config(data_dir, Conf), "lib"),

    %% Create the releases
    Lib1 = [{a,"1.0",LibDir}],
    Lib2 = [{a,"9.0",LibDir}],
    Rel1 = create_and_install_fake_first_release(Dir,Lib1),
    Rel2 = create_fake_upgrade_release(Dir,"2",Lib2,{[Rel1],[Rel1],[LibDir]}),
    Rel1Dir = filename:dirname(Rel1),
    Rel2Dir = filename:dirname(Rel2),

    %% Start a slave node
    {ok, Node} = t_start_node(upgrade_supervisor, Rel1,
			      filename:join(Rel1Dir,"sys.config")),

    %% Check path
    Dir1 = filename:join([LibDir, "a-1.0"]),
    Dir1 = rpc:call(Node, code, lib_dir, [a]),
    ASupBeam1 = filename:join([Dir1,ebin,"a_sup.beam"]),
    ASupBeam1 = rpc:call(Node, code, which, [a_sup]),

    %% Install second release, with no changed modules
    {ok, RelVsn2} = rpc:call(Node, release_handler, set_unpacked,
			     [Rel2++".rel", Lib2]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "relup")]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "start.boot")]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "sys.config")]),

    {ok, _RelVsn1, []} =
	rpc:call(Node, release_handler, install_release, [RelVsn2]),

    %% Check that libdir is changed
    Dir2 = filename:join([LibDir, "a-9.0"]),
    Dir2 = rpc:call(Node, code, lib_dir, [a]),
    ASupBeam2 = filename:join([Dir2,ebin,"a_sup.beam"]),
    ASupBeam2 = rpc:call(Node, code, which, [a_sup]),

    %% Check that the restart strategy and child spec is updated
    {status, _, {module, _}, [_, _, _, _, [_,_,{data,[{"State",State}]}|_]]} =
	rpc:call(Node,sys,get_status,[a_sup]),
    {state,_,RestartStrategy,{[a],Db},_,_,_,_,_,_,_} = State,
    one_for_all = RestartStrategy, % changed from one_for_one
    {child,_,_,_,_,brutal_kill,_,_} = maps:get(a,Db), % changed from timeout 2000

    ok.

upgrade_supervisor(cleanup,_Condf) ->
    stop_node(node_name(upgrade_supervisor)).

%% Check that if the supervisor fails, then the upgrade is rolled back
%% and an ok error message is returned
upgrade_supervisor_fail(Conf) when is_list(Conf) ->
    %% Set some paths
    PrivDir = priv_dir(Conf),
    Dir = filename:join(PrivDir,"upgrade_supervisor_fail"),
    LibDir = filename:join(?config(data_dir, Conf), "lib"),

    %% Create the releases
    Lib1 = [{a,"1.0",LibDir}],
    Lib2 = [{a,"9.1",LibDir}],
    Rel1 = create_and_install_fake_first_release(Dir,Lib1),
    Rel2 = create_fake_upgrade_release(Dir,"2",Lib2,{[Rel1],[Rel1],[LibDir]}),
    Rel1Dir = filename:dirname(Rel1),
    Rel2Dir = filename:dirname(Rel2),

    %% Start a slave node
    {ok, Node} = t_start_node(upgrade_supervisor_fail, Rel1,
			      filename:join(Rel1Dir,"sys.config")),

    %% Check path
    Dir1 = filename:join([LibDir, "a-1.0"]),
    Dir1 = rpc:call(Node, code, lib_dir, [a]),
    ASupBeam1 = filename:join([Dir1,ebin,"a_sup.beam"]),
    ASupBeam1 = rpc:call(Node, code, which, [a_sup]),

    %% Install second release, with no changed modules
    {ok, RelVsn2} = rpc:call(Node, release_handler, set_unpacked,
			     [Rel2++".rel", Lib2]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "relup")]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "start.boot")]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "sys.config")]),
    ok = net_kernel:monitor_nodes(true),

    {error,{code_change_failed,_Pid,a_sup,_Vsn,
	    {error,{invalid_shutdown,brutal_kil}}}} =
	rpc:call(Node, release_handler, install_release,
		 [RelVsn2, [{error_action,reboot}]]),

    %% Check that the upgrade is terminated - normally this would be a
    %% rollback, but
    %%
    %% 1. Default rollback is done with init:restart(), which does not
    %%    reboot the emulator, it only restarts the system inside the
    %%    running erlang node.
    %%
    %% 2. This does not work well on a slave node since, if timing is
    %%    right (bad), the slave node will get the nodedown from its
    %%    master (because distribution is terminated as part of
    %%    init:restart()) and then it will do halt() and thus never be
    %%    restarted (see slave:wloop/1)
    %%
    %% 3. Sometimes, though, init:restart() will manage to finish its
    %%    job before the nodedown is received, making the node
    %%    actually restart - in which case it might very well confuse
    %%    the next test case.
    %%
    %% 4. So, to avoid unstability we use {error_action,reboot} above,
    %%    to ensure that the node is actually stopped. Of course, in a
    %%    real system this must be used together with heart
    %%    supervision, and then the node will be restarted anyway. But
    %%    here in this simple test case we are satisfied to see that
    %%    the node terminates.
    receive {nodedown,Node} -> ok
    after 10000 -> ct:fail(failed_upgrade_never_restarted_node)
    end,

    ok.

upgrade_supervisor_fail(cleanup,_Condf) ->
    stop_node(node_name(upgrade_supervisor_fail)).

%% Test upgrade and downgrade of applications
eval_appup(Conf) when is_list(Conf) ->

    %% OTP-6162
    %% Create an ETS table which is updated by app1 if there is any
    %% change made to the application configuration parameter 'var'
    %% (see config_change/3 in myrel/lib1|2/app1-1|2.0/src/app1.erl)
    ets:new(otp_6162, [set, public, named_table]),

    %% Set some paths
    RelDir = filename:join(?config(data_dir, Conf), "app1_app2"),
    App11Dir = filename:join([RelDir, "lib1", "app1-1.0"]),
    App12Dir = filename:join([RelDir, "lib2", "app1-2.0"]),
    EbinDir = filename:join(App11Dir, "ebin"),

    %% Start app1-1.0
    code:add_patha(EbinDir),
    ok = application:start(app1),
    App11Dir = code:lib_dir(app1),
    ok = gen_server:call(harry, error),

    %% Read appup script
    {ok,"2.0",UpScript} = release_handler:upgrade_script(app1,App12Dir),
    [{load_object_code,_},
     point_of_no_return,
     {load,_}] = UpScript,

    %% Upgrade to app1-2.0
    {ok, []} = release_handler:upgrade_app(app1, App12Dir),
    App12Dir = code:lib_dir(app1),
    error = gen_server:call(harry, error),

    %% OTP-6162
    %% Value of config parameter 'var' should now be 'val2'
    %% (see myrel/lib2/app1-2.0/ebin/app1.app)
    [{var,val2}] = ets:lookup(otp_6162, var),

    %% Read appup script
    {ok,DnScript} = release_handler:downgrade_script(app1,"1.0",App11Dir),
    [{load_object_code,_},
     point_of_no_return,
     {load,_}] = DnScript,

    %% Downgrade to app1-1.0
    {ok, []} = release_handler:downgrade_app(app1,"1.0",App11Dir),
    App11Dir = code:lib_dir(app1),
    ok = gen_server:call(harry, error),

    %% OTP-6162
    %% Value of config parameter 'var' should now be 'val1'
    %% (see myrel/lib1/app1-1.0/ebin/app1.app)
    [{var,val1}] = ets:lookup(otp_6162, var),

    ok = application:stop(app1),
    ok = application:unload(app1),

    true = code:del_path(EbinDir),
    ok.


%% Test upgrade and downgrade of applications when appup contains
%% restart_emulator and restart_new_emulator instructions
eval_appup_with_restart(Conf) when is_list(Conf) ->

    %% Set some paths
    RelDir = filename:join(?config(data_dir, Conf), "app1_app2"),
    App11Dir = filename:join([RelDir, "lib1", "app1-1.0"]),
    App13Dir = filename:join([RelDir, "lib3", "app1-3.0"]), %restart_emulator
    App14Dir = filename:join([RelDir, "lib4", "app1-4.0"]), %restart_new_emulator
    EbinDir1 = filename:join(App11Dir, "ebin"),
    EbinDir3 = filename:join(App13Dir, "ebin"),
    EbinDir4 = filename:join(App14Dir, "ebin"),

    %% Start app1-1.0
    code:add_patha(EbinDir1),
    ok = application:start(app1),
    App11Dir = code:lib_dir(app1),

    %% Read appup script
    {ok,"3.0",UpScript3} = release_handler:upgrade_script(app1,App13Dir),
    [{load_object_code,_},
     point_of_no_return,
     {load,_},
     restart_emulator] = UpScript3,

    %% Upgrade to app1-3.0 - restart_emulator
    restart_emulator = release_handler:upgrade_app(app1, App13Dir),
    App13Dir = code:lib_dir(app1),

    %% Fake full upgrade to 3.0
    {ok,AppSpec} = file:consult(filename:join([App13Dir,"ebin","app1.app"])),
    application_controller:change_application_data(AppSpec,[]),

    %% Read appup script
    {ok,"4.0",UpScript4} = release_handler:upgrade_script(app1,App14Dir),
    [restart_new_emulator,point_of_no_return] = UpScript4,

    %% Try pgrade to app1-4.0 - restart_new_emulator
    {error,restart_new_emulator} = release_handler:upgrade_app(app1, App14Dir),
    App13Dir = code:lib_dir(app1),

    %% Read appup script
    {ok,DnScript1} = release_handler:downgrade_script(app1,"1.0",App11Dir),
    [{load_object_code,_},
     point_of_no_return,
     {load,_},
     restart_emulator] = DnScript1,

    %% Still running 3.0 - downgrade to app1-1.0 - restart_emulator
    restart_emulator = release_handler:downgrade_app(app1,"1.0",App11Dir),
    App11Dir = code:lib_dir(app1),

    ok = application:stop(app1),
    ok = application:unload(app1),
    true = code:del_path(EbinDir1),

    %% Start again as version 4.0
    code:add_patha(EbinDir4),
    ok = application:start(app1),
    App14Dir = code:lib_dir(app1),

    %% Read appup script
    {ok,DnScript3} = release_handler:downgrade_script(app1,"3.0",App13Dir),
    [point_of_no_return,restart_emulator] = DnScript3,

    %% Downgrade to app1-3.0 - restart_new_emulator
    restart_emulator = release_handler:downgrade_app(app1,"3.0",App13Dir),
    App13Dir = code:lib_dir(app1),

    ok = application:stop(app1),
    ok = application:unload(app1),

    true = code:del_path(EbinDir3),
    false = code:del_path(EbinDir1),
    false = code:del_path(EbinDir4),

    ok.


%% Test the example/target_system.erl module
target_system(Conf) when is_list(Conf) ->
    PrivDir = priv_dir(Conf),
    target_system1(Conf,PrivDir).

target_system1(Conf,PrivDir) ->
    DataDir = ?config(data_dir,Conf),

    TargetCreateDir = filename:join([PrivDir,"target_system","create"]),
    TargetInstallDir = filename:join([PrivDir,"target_system","install"]),

    ok = filelib:ensure_dir(filename:join(TargetCreateDir,"xx")),
    ok = filelib:ensure_dir(filename:join(TargetInstallDir,"xx")),

    %% Create the .rel file
    RelName = filename:join(TargetCreateDir,"ts-1.0"),
    RelFile = RelName++".rel",
    RelVsn = "R1A",
    create_rel_file(RelFile,RelName,RelVsn,current,[{a, "1.0"}]),

    %% Build the target_system module
    ExamplesEbin = filename:join([code:lib_dir(sasl),examples,ebin]),
    TSPath =
	case filelib:is_file(filename:join(ExamplesEbin,"target_system.beam")) of
	    true ->
		ExamplesEbin;
	    false ->
		{ok,_} =
		    compile:file(filename:join(DataDir,"target_system.erl"),
				 [{outdir,TargetCreateDir}]),
		TargetCreateDir
    end,
    code:add_path(TSPath),

    %% Create the release
    target_system:create(RelName,[{path,[filename:join([DataDir,
							lib,
							"a-1.0",
							ebin])]}]),

    %% Install the release
    target_system:install(RelName,TargetInstallDir),

    code:del_path(TSPath),

    %% Check that all files exist in installation
    ErtsDir = app_dir(erts,current),
    true = filelib:is_dir(filename:join(TargetInstallDir,ErtsDir)),
    LibDir = filename:join(TargetInstallDir,lib),
    KernelVsn = vsn(kernel,current),
    StdlibVsn = vsn(stdlib,current),
    SaslVsn = vsn(sasl,current),
    RelFileBasename = filename:basename(RelFile),
    KernelLibDir = filename:join(LibDir,"kernel-"++KernelVsn),
    true = filelib:is_dir(KernelLibDir),
    true = filelib:is_dir(filename:join(LibDir,"stdlib-"++StdlibVsn)),
    true = filelib:is_dir(filename:join(LibDir,"sasl-"++SaslVsn)),
    true = filelib:is_dir(filename:join(LibDir,"a-1.0")),
    RelDir = filename:join(TargetInstallDir,releases),
    true = filelib:is_regular(filename:join(RelDir,"RELEASES")),
    true = filelib:is_regular(filename:join(RelDir,"start_erl.data")),
    true = filelib:is_regular(filename:join(RelDir,RelFileBasename)),
    true = filelib:is_dir(filename:join(RelDir,RelVsn)),
    StartBoot = filename:join([RelDir,RelVsn,"start.boot"]),
    true = filelib:is_regular(StartBoot),
    true = filelib:is_regular(filename:join([RelDir,RelVsn,RelFileBasename])),
    BinDir = filename:join(TargetInstallDir,bin),
    Erl = filename:join(BinDir,erl),
    true = filelib:is_regular(Erl),
    true = filelib:is_regular(filename:join(BinDir,"start.boot")),
    true = filelib:is_regular(filename:join(BinDir,start_erl)),
    true = filelib:is_regular(filename:join(BinDir,start)),
    true = filelib:is_regular(filename:join(BinDir,epmd)),
    true = filelib:is_regular(filename:join(BinDir,run_erl)),
    true = filelib:is_regular(filename:join(BinDir,to_erl)),

    %% Check content of files
    ErtsVsn = vsn(erts,current),
    {ok,SED} = file:read_file(filename:join(RelDir,"start_erl.data")),
    [ErtsVsn,RelVsn] = string:tokens(binary_to_list(SED),"\s\n"),

    %% Check that installation can be started
    Sname = list_to_atom(atom_to_list(?MODULE) ++ "-target_system"),
    {ok,Node} = start_target_node_with_erl(Erl,Sname,StartBoot),

    TargetInstallDir = rpc:call(Node,code,root_dir,[]),
    KernelLibDir = rpc:call(Node,code,lib_dir,[kernel]),
    [{RelName,RelVsn,_Apps,permanent}] =
	rpc:call(Node,release_handler,which_releases,[]),

    ?t:format("Target node ok:~nRootDir: ~ts~nKernelLibDir: ~ts~nRelease: ~ts",
	      [TargetInstallDir,KernelLibDir,RelName]),

    ok.

target_system(cleanup,_Conf) ->
    Sname = list_to_atom(atom_to_list(?MODULE) ++ "-target_system"),
    stop_target_node(node_name(Sname)),
    ok.

start_target_node_with_erl(Erl,Sname,Boot) ->
    FullName = node_name(Sname),
    FilenameMode = case file:native_name_encoding() of
		       latin1 -> "+fnl";
		       utf8 -> "+fnui"
		   end,
    Args = [FilenameMode,"-detached", "-noinput","-sname",atom_to_list(Sname),
	   "-boot",filename:rootname(Boot)],
    ?t:format("Starting node ~p: ~ts~n",
	      [FullName, lists:flatten([[X," "] || X <- [Erl|Args]])]),
    case rh_test_lib:cmd(Erl,Args,[]) of
	ok ->
	    ok = wait_nodes_up([FullName],"target_system test node"),
	    {ok,FullName};
	Error ->
            ?t:fail({failed_to_start_node, FullName, Error})
    end.

stop_target_node(Node) ->
    monitor_node(Node, true),
    _ = rpc:call(Node,erlang,halt,[]),
    receive {nodedown, Node} -> ok end.

%% Test that the example/target_system.erl module can create and
%% install under a path which includes unicode characters
target_system_unicode(Conf) when is_list(Conf) ->
    PrivDir = priv_dir(Conf),
    UnicodePrivDir = filename:join(PrivDir,"αβ"),

    PA = filename:dirname(code:which(?MODULE)),

    %% Make sure this runs on a node with unicode file name mode
    Sname = list_to_atom(atom_to_list(?MODULE) ++ "-target_system_unicode"),
    {ok,Node} = ?t:start_node(Sname,peer,[{args,"+fnui -pa " ++ PA}]),
    ok = rpc:call(Node,file,make_dir,[UnicodePrivDir]),
    case rpc:call(Node,application,start,[sasl]) of
	ok -> ok;
	{error,{already_started,sasl}} -> ok;
	Error -> ?t:fail({failed_to_start_sasl_on_test_node,Node,Error})
    end,
    ok = rpc:call(Node,?MODULE,target_system1,[Conf,UnicodePrivDir]),
    ok.

target_system_unicode(cleanup,Conf) ->
    Sname = list_to_atom(atom_to_list(?MODULE) ++ "-target_system_unicode"),
    Node = node_name(Sname),
    _ = rpc:call(Node,?MODULE,target_system,[cleanup,Conf]),
    _ = stop_node(Node),
    ok.

%%%=================================================================
%%% Testing global groups.
%%%=================================================================

%% This test case involves P1G and P1H with the sys.config as
%% specified in gg_config/1. The test case checks that the global
%% group information is correct before and after the upgrade and also
%% after terminating one of the nodes. The flow is as follows:
%% 1. Start all four nodes of global group gg1 with P1G
%% 2. Terminate one of the nodes, and upgrade the others to P1H. P1H
%%    config adds to more nodes to the global group.
%% 3. Start the two remaining nodes with P1H
upgrade_gg(Conf) ->
    [Gg1Sname,Gg2Sname,Gg3Sname,Gg4Sname,Gg5Sname,Gg6Sname] =
	?config(snames,Conf),

    %% start gg1, gg3, gg4, gg5 and check that global group info is ok
    Nodes1 = [Gg1,Gg3,Gg4,Gg5] =
	start_nodes(Conf,[Gg1Sname,Gg3Sname,Gg4Sname,Gg5Sname],"upgrade_gg"),

    [check_gg_info(Node,Nodes1,[],Nodes1--[Node]) || Node <- Nodes1],

    %% register a process on each of the nodes
    ok = rpc:call(Gg1, installer, reg_proc, [reg1]),
    ok = rpc:call(Gg3, installer, reg_proc, [reg3]),
    ok = rpc:call(Gg4, installer, reg_proc, [reg4]),
    ok = rpc:call(Gg5, installer, reg_proc, [reg5]),
    are_names_reg_gg(Gg1, [reg1, reg3, reg4, reg5]),

    %% Stop gg3, then upgrade gg1, gg4 and gg5 to P1H
    ok = stop_nodes([Gg3]),

    ok = install_release_changed_gg(Gg1,"P1H"),
    ok = install_release_changed_gg(Gg4,"P1H"),
    ok = install_release_changed_gg(Gg5,"P1H"),

    %% Check global group info
    Gg2 = node_name(Gg2Sname),
    Gg6 = node_name(Gg6Sname),
    Nodes2 = [Gg1,Gg4,Gg5],
    [check_gg_info(Node,Nodes2,[Gg2,Gg6],Nodes2--[Node]) || Node <- Nodes2],

    %% start gg2 and gg6
    [Gg2,Gg6] = start_nodes(Conf,[Gg2Sname,Gg6Sname],"upgrade_gg start gg2/gg6"),

    %% reg proc on each of the nodes
    ok = rpc:call(Gg2, installer, reg_proc, [reg2]),
    ok = rpc:call(Gg6, installer, reg_proc, [reg6]),
    are_names_reg_gg(Gg1, [reg1, reg2, reg4, reg5, reg6]),

    %% Check global group info
    Nodes3 = [Gg1,Gg2,Gg4,Gg5,Gg6],
    [check_gg_info(Node,Nodes3,[],Nodes3--[Node]) || Node <- Nodes3],

    ok.

upgrade_gg(cleanup,Config) ->
    Snames = ?config(snames,Config),
    NodeNames = [node_name(Sname) || Sname <- Snames],
    ok = stop_nodes(NodeNames).


%%%-----------------------------------------------------------------
%%% OTP-10463, Bug - release_handler could not handle regexp in appup
%%% files.
otp_10463_upgrade_script_regexp(Config) ->
    DataDir = ?config(data_dir,Config),
    code:add_path(filename:join([DataDir,regexp_appup,app1,ebin])),
    application:start(app1),
    {ok,"1.1",_} = release_handler:upgrade_script(app1,code:lib_dir(app1)),
    ok.

otp_10463_upgrade_script_regexp(cleanup,Config) ->
    DataDir = ?config(data_dir,Config),
    application:stop(app1),
    code:del_path(filename:join([DataDir,regexp_appup,app1,ebin])),
    ok.

no_dot_erlang(_Conf) ->
    case init:get_argument(home) of
        {ok,[[Home]]} when is_list(Home) ->
            no_dot_erlang_1(Home);
        _ -> ok
    end.

no_dot_erlang_1(Home) ->
    DotErlang = filename:join(Home, ".erlang"),
    BupErlang = filename:join(Home, ".erlang_testbup"),
    try
        {ok, Wd} = file:get_cwd(),
        case filelib:is_file(DotErlang) of
            true -> {ok, _} = file:copy(DotErlang, BupErlang);
            false -> ok
        end,
	Erl0 =  filename:join([code:root_dir(),"bin","erl"]),
	Erl = filename:nativename(Erl0),
	Quote = "\"",
	Args = " -noinput -run c pwd -run erlang halt",
	ok = file:write_file(DotErlang, <<"io:put_chars(\"DOT_ERLANG_READ\\n\").\n">>),

	CMD1 = Quote ++ Erl ++ Quote ++ Args ,
	case os:cmd(CMD1) of
	    "DOT_ERLANG_READ" ++ _ ->
                io:format("~p: Success~n", [?LINE]);
	    Other1 ->
		io:format("Failed: ~ts~n",[CMD1]),
		io:format("Expected: ~s ++ _~n",["DOT_ERLANG_READ "]),
		io:format("Got: ~ts~n",[Other1]),
		exit({failed_to_start, test_error})
	end,
	NO_DOT_ERL = " -boot no_dot_erlang",
	CMD2 = Quote ++ Erl ++ Quote ++ NO_DOT_ERL ++ Args,
	case lists:prefix(Wd, Other2 = os:cmd(CMD2)) of
	    true -> io:format("~p: Success~n", [?LINE]);
	    false ->
		io:format("Failed: ~ts~n",[CMD2]),
		io:format("Expected: ~s~n",["TESTOK"]),
		io:format("Got: ~ts~n",[Other2]),
		exit({failed_to_start, no_dot_erlang})
	end
    after
        case filelib:is_file(BupErlang) of
            true ->
                {ok, _} = file:copy(BupErlang, DotErlang),
                _ = file:delete(BupErlang);
            false ->
                _ = file:delete(DotErlang)
        end
    end.

%%%-----------------------------------------------------------------
%%% Test unicode handling. Make sure that release name, application
%%% description, and application environment variables may contain
%%% unicode characters.
unicode_upgrade(Conf) ->
    %% Set some paths
    DataDir = ?config(data_dir, Conf),
    PrivDir = priv_dir(Conf),
    Dir = filename:join(PrivDir,"unicode"),
    LibDir0 = filename:join(DataDir, "unicode"),
    LibDir =
        case {file:native_name_encoding(),os:type()} of
            {utf8,{Os,_}} when Os =/= win32 ->
                LD = filename:join(DataDir,"unicode_αβ"),
                file:make_symlink("unicode",LD),
                LD;
            _ ->
                LibDir0
        end,

    %% Create the releases
    RelName = "unicode_rel_αβ",
    Rel1 = create_and_install_fake_first_release(Dir,{RelName,"1"},
						 [{u,"1.0",LibDir}]),
    Rel2 = create_fake_upgrade_release(Dir,
				       {RelName,"2"},
				       [{u,"1.1",LibDir}],
				       {[Rel1],[Rel1],[LibDir]}),
    Rel1Dir = filename:dirname(Rel1),
    Rel2Dir = filename:dirname(Rel2),

    %% Start a slave node
    {ok, Node} = t_start_node(unicode_upgrade, Rel1,
                              filename:join(Rel1Dir,"sys.config"), "+pc unicode"),

    %% Check
    Dir1 = filename:join([LibDir, "u-1.0"]),
    Dir1 = rpc:call(Node, code, lib_dir, [u]),
    UBeam1 = filename:join([Dir1,"ebin","u.beam"]),
    UBeam1 = rpc:call(Node,code,which,[u]),
    {RelName,"1"} = rpc:call(Node,init,script_id,[]),
    {Env,state} = rpc:call(Node,u,u,[]),
    'val_αβ' = proplists:get_value('key_αβ',Env),
    [{RelName,"1",_,permanent}|_] =
        rpc:call(Node,release_handler,which_releases,[]),
    {ok,ReleasesDir} = rpc:call(Node,application,get_env,[sasl,releases_dir]),
    {ok,[[{release,RelName,"1",_,_,permanent}|_]]} =
        file:consult(filename:join(ReleasesDir,"RELEASES")),

    %% Install second release
    {ok, RelVsn2} =
	rpc:call(Node, release_handler, set_unpacked,
		 [Rel2++".rel", [{u,"1.1",LibDir}]]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "relup")]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "start.boot")]),
    ok = rpc:call(Node, release_handler, install_file,
		  [RelVsn2, filename:join(Rel2Dir, "sys.config")]),

    {ok, _RelVsn1, []} =
	rpc:call(Node, release_handler, install_release, [RelVsn2]),

    %% And check
    Dir2 = filename:join([LibDir, "u-1.1"]),
    Dir2 = rpc:call(Node, code, lib_dir, [u]),
    UBeam2 = filename:join([Dir2,"ebin","u.beam"]),
    {file,UBeam2} = rpc:call(Node,code,is_loaded,[u]),
    {RelName,"1"} = rpc:call(Node,init,script_id,[]),
    {Env,{state,'αβ'}} = rpc:call(Node,u,u,[]),
    [{RelName,"2",_,current}|_] =
        rpc:call(Node,release_handler,which_releases,[]),
    {ok,ReleasesDir2} = rpc:call(Node,application,get_env,[sasl,releases_dir]),
    {ok,<<"%% coding: utf-8\n[{release,\"unicode_rel_αβ\",\"2\""/utf8,_/binary>>}=
        file:read_file(filename:join(ReleasesDir2,"RELEASES")),
    ok.

unicode_upgrade(cleanup,_Conf) ->
    stop_node(node_name(unicode_upgrade)).


%%%=================================================================
%%% Misceleaneous functions
%%%=================================================================
stop_nodes(Nodes) ->
    ?t:format("Stopping nodes: ~p~n",[Nodes]),
    Running = 
	lists:foldl(fun(Node,Acc) -> 
			    Now = now(),
			    stop_cover(Node),
			    case rpc:call(Node,installer,stop,[Now]) of
				{badrpc,nodedown} ->
				    Acc;
				Other ->
				    ?t:format("Stop ~p(~p): ~p~n",
					      [Node,Now,Other]),
				    [Node|Acc]
			    end
		    end, [], Nodes),
    wait_nodes_down(Running).


wait_nodes_down(Nodes) ->
    ?t:format( "wait_nodes_down ~p:",[Nodes]),
    wait_nodes_down(Nodes, 30).

wait_nodes_down(Nodes, 0) ->
    test_server:fail({error, {"could not kill nodes", Nodes}});
wait_nodes_down(Nodes, N) ->
    Fun = fun(Node, A) ->
		  case net_adm:ping(Node) of
		      pong ->
			  ?t:format( "  net_adm:ping(~p) = pong", [Node]),
			  [Node|A];
		      pang ->
			  ?t:format( "  net_adm:ping(~p) = pang", [Node]),
			  A
		  end
	  end,
    Pang = lists:foldl(Fun, [], Nodes),
    case Pang of
	[] -> 
	    ?t:format("",[]),
	    ok;
	_ ->
	    timer:sleep(1000),
	    wait_nodes_down(Pang, N-1)
    end.



wait_nodes_up(Nodes, Tag) ->
    wait_nodes_up(Nodes, Tag, []).

wait_nodes_up(Nodes0, Tag, Apps) ->
    ?t:format("wait_nodes_up(~p, ~p, ~p):",[Nodes0, Tag, Apps]),
    Nodes = fix_nodes(Nodes0),
    wait_nodes_up(Nodes, Tag, lists:umerge(Apps,[kernel,stdlib,sasl]), 60).

fix_nodes([{Node,InitPid}|Nodes]) ->
    [{Node,InitPid} | fix_nodes(Nodes)];
fix_nodes([Node|Nodes]) ->
    [{Node,fake_init_pid} | fix_nodes(Nodes)];
fix_nodes([]) ->
    [].

wait_nodes_up(Nodes, Tag, Apps, 0) ->
    test_server:fail({error, {"nodes not started", Nodes, Tag, Apps}});
wait_nodes_up(Nodes, Tag, Apps, N) ->
    Fun = 
	fun(NodeInfo={Node,OldInitPid}, A) ->
		case rpc:call(Node, application, which_applications, []) of
		    {badrpc, nodedown} ->
			?t:format( "  ~p = {badarg, nodedown}",[Node]),
			[NodeInfo | A];
		    List when is_list(List)->
			?t:format( "  ~p = [~p]",[Node, List]),
			case lists:all(fun(App) -> 
					       lists:keymember(App,1,List) 
				       end, Apps) of
			    true ->
				case rpc:call(Node,erlang,whereis,[init]) of
				    OldInitPid ->
					[NodeInfo | A];
				    _ ->
					start_cover(Node),
					A
				end;
			    false ->
				[NodeInfo | A]
			end
		end
	end,
    Pang = lists:foldl(Fun,[],Nodes),
    case Pang of
	[] ->
	    ?t:format("",[]),
	    ok;
	_ ->
	    timer:sleep(2000),
	    wait_nodes_up(Pang, Tag, Apps, N-1)
    end.




are_names_reg_gg(Node, Names) ->
    ?t:format( "are_names_reg_gg ~p~n",[Names]),
    are_names_reg_gg(Node, Names, 30).

are_names_reg_gg(Node, Names, N) ->
    case lists:sort(rpc:call(Node, global, registered_names, [])) of
	Names ->
	    ok;
	Regs when N > 0 ->
	    timer:sleep(1000),
	    ?t:format( "are_names_reg_gg Regs ~p~n",[Regs]),
	    are_names_reg_gg(Node, Names, N-1);
	Regs ->
	    ?t:fail({error, {"Names not registered",
			     {{"should :", Names},
			      {"was :", Regs}}}})
    end.



t_start_node(Name, Boot, SysConfig) ->
    t_start_node(Name, Boot, SysConfig, "").
t_start_node(Name, Boot, SysConfig, ArgStr) ->
    Args = 
	case Boot of
	    [] -> [];
	    _ -> " -boot " ++ Boot
	end ++
	case SysConfig of
	    [] -> [];
	    _ -> " -config " ++ SysConfig
	end ++
        " " ++ ArgStr,
    test_server:start_node(Name, peer, [{args, Args}]).

stop_node(Node) ->
    ?t:stop_node(Node).


copy_client(Conf,Master,Sname,Client) ->
    ?t:format("copy_client(Conf)"),

    DataDir = ?config(data_dir, Conf),
    MasterDir = filename:join(priv_dir(Conf),Master),

    {ClientArgs,RelCliDir} = rh_test_lib:get_client_args(Client,Sname,MasterDir,
							 node_name(Master)),

    Cli = filename:join([MasterDir, RelCliDir]),
    ok = filelib:ensure_dir(filename:join([Cli,"bin","."])),
    ok = filelib:ensure_dir(filename:join([Cli,"releases","."])),
    ok = filelib:ensure_dir(filename:join([Cli,"log","."])),

    P1GOrig = filename:join([MasterDir, "releases", "P1G"]),
    ok = copy_tree(Conf,P1GOrig,filename:join(Cli,"releases")),
    
    case os:type() of
	{unix,_} ->
	    ok = subst_file(filename:join([DataDir, "start_client"]),
			    filename:join([Cli,"bin","start"]),
			    [{"ROOT",MasterDir},
			     {"CLIENTARGS",ClientArgs}],
			    [{chmod,8#0755}]);
	_ ->
	    ok
    end,
    
    StartErlData = filename:join([MasterDir, "releases", "start_erl.data"]),
    CliRelDir = filename:join([Cli, "releases"]),
    copy_file(StartErlData, CliRelDir),
    
    RR = filename:join([MasterDir, "releases", "RELEASES"]),
    copy_file(RR, CliRelDir),

    ok.


clean_priv_dir(Conf,Save) ->
    PrivDir = priv_dir(Conf),
    rh_test_lib:clean_dir(PrivDir,Save),
    case file:list_dir(PrivDir) of
	{ok,[]} -> _ = file:del_dir(PrivDir);
	_ -> ok
    end.

node_name(Sname) when is_atom(Sname) ->
    {ok,Host} = inet:gethostname(),
    list_to_atom(atom_to_list(Sname) ++ "@" ++ Host).

copy_file(Src, Dest) ->
    copy_file(Src, Dest, []).
copy_file(Src, Dest, Opts) ->
    case file:copy(Src,Dest) of
	{ok,_} ->
	    preserve(Src,Dest,Opts),
	    chmod(Dest,Opts),
	    ok;
	{error,eisdir} ->
	    NewDest = filename:join(Dest, filename:basename(Src)),
	    case file:copy(Src,NewDest) of
		{ok,_} -> 
		    preserve(Src,NewDest,Opts),
		    chmod(NewDest,Opts);
		{error,Reason} -> 
		    copy_error(Src,Dest,Reason)
	    end;
	{error,Reason} ->
	    copy_error(Src,Dest,Reason)
    end.

preserve(Src,Dest,Opts) ->
    case lists:member(preserve, Opts) of
	true ->
	    {ok, FileInfo} = file:read_file_info(Src),
	    ok = file:write_file_info(Dest, FileInfo);
	false ->
	    ok
    end.

chmod(Dest,Opts) ->
    case lists:keyfind(chmod,1,Opts) of
	{chmod,Mode} ->
	    ok = file:change_mode(Dest, Mode);
	false ->
	    ok
    end.



copy_error(Src, Dest, Reason) ->
    ?t:format("Copy ~ts to ~ts failed: ~ts\n",
	      [Src,Dest,file:format_error(Reason)]),
    ?t:fail(file_copy_failed).

copy_tree(Conf, Src, DestDir) ->
    case catch copy_tree(Conf, Src, filename:basename(Src), DestDir) of
	ok ->
	    ok;
	{'EXIT', {{badmatch,Error},_Stack}} ->
	    %% Most probably, an erl_tar call has failed.
	    %% Known to happen on some platforms (symbolic_link_too_long)
	    Error;
	{'EXIT', Reason} ->
	    {error, Reason}
    end.

copy_tree(Conf, Src, NewName, DestDir) ->
    PrivDir = priv_dir(Conf),
    TempTarName = filename:join(PrivDir, "temp_tar_file.tar"),
    %% Not compressing tar file here since that would increase test
    %% suite time by almost 100%, and the tar file is deleted
    %% imediately anyway.
    {ok,Tar} = erl_tar:open(TempTarName, [write]),
    ok = erl_tar:add(Tar, Src, NewName, []),
    ok = erl_tar:close(Tar),
    ok = erl_tar:extract(TempTarName, [{cwd,DestDir}]),
    ok = file:delete(TempTarName),
    ok.

%% subst_file(Src, Dest, Vars)
%% Src = Dest = string(), filename and path
%% Vars = [{Var, Val}]
%% Var = Val = string()
%% Substitute all occurrences of %Var% for Val in Src, using the list
%% of variables in Vars. Result is written to Dest.
%%
subst_file(Src, Dest, Vars) ->
    subst_file(Src, Dest, Vars, []).
subst_file(Src, Dest, Vars, Opts) ->
    {ok, Bin} = file:read_file(Src),
    Conts = binary_to_list(Bin), % The source will always be latin1
    NConts = subst(Conts, Vars),
    %% The destination must be utf8 if file name encoding is unicode
    Enc = file:native_name_encoding(),
    ok = file:write_file(Dest, unicode:characters_to_binary(NConts,Enc,Enc)),
    preserve(Src,Dest,Opts),
    chmod(Dest,Opts).

subst(Str, Vars) ->
    subst(Str, Vars, []).

subst([$%, C| Rest], Vars, Result) when $A =< C, C =< $Z ->
    subst_var([C| Rest], Vars, Result, []);
subst([$%, C| Rest], Vars, Result) when $a =< C, C =< $z ->
    subst_var([C| Rest], Vars, Result, []);
subst([$%, C| Rest], Vars, Result) when  C == $_ ->
    subst_var([C| Rest], Vars, Result, []);
subst([C| Rest], Vars, Result) ->
    subst(Rest, Vars, [C| Result]);
subst([], _Vars, Result) ->
    lists:reverse(Result).

subst_var([$%| Rest], Vars, Result, VarAcc) ->
    Key = lists:reverse(VarAcc),
    case lists:keysearch(Key, 1, Vars) of
        {value, {Key, Value}} ->
            subst(Rest, Vars, lists:reverse(Value, Result));
        false ->
            subst(Rest, Vars, [$%| VarAcc ++ [$%| Result]])
    end;
subst_var([C| Rest], Vars, Result, VarAcc) ->
    subst_var(Rest, Vars, Result, [C| VarAcc]);
subst_var([], Vars, Result, VarAcc) ->
    subst([], Vars, [VarAcc ++ [$%| Result]]).


priv_dir(Conf) ->
    %% Due to problem with long paths on windows => creating a new
    %% priv_dir under data_dir
    %% And get rid of trailing slash (absname does that)
    %% And if file name translation mode is utf8, use a path with
    %% unicode characters
    PrivDir =
	case file:native_name_encoding() of
	    utf8 ->
		"priv_dir_αβ";
	    _ ->
		"priv_dir"
	end,
    filename:absname(filename:join([?config(data_dir, Conf),PrivDir])).

init_priv_dir(Conf) ->
    Dir = priv_dir(Conf),
    case filelib:is_dir(Dir) of
	true ->
	    clean_priv_dir(Conf,false);
	false ->
	    ok
    end,
    filelib:ensure_dir(filename:join(Dir,"*")).

latest_version(Dir) ->
    List = filelib:wildcard(Dir ++ "*"),
    lists:last(lists:sort(List)).

%% A printer process which receives messages from other nodes and
%% prints in the log
reg_print_proc() ->
    catch unregister(rh_print),
    Pid = spawn_link(?MODULE, rh_print, []),
    register(rh_print, Pid),
    ok.

rh_print() ->
    receive
	{print, {Module,Line}, [H|T]} ->
	    ?t:format("=== ~p:~p - ~p",[Module,Line,H]),
	    lists:foreach(fun(Term) -> ?t:format("    ~tp",[Term]) end, T),
	    ?t:format("",[]),
	    rh_print();
	kill ->
	    exit(normal)
    end.

stop_print_proc() ->
    case whereis(rh_print) of %%removes the printer process
	undefined ->
	    ok;
	Pid when is_pid(Pid) ->
	    rh_print ! kill
    end.

%% Create the first target release, vsn P1G. This release is used for
%% all test cases in {group,release}
create_p1g(Conf,TargetDir) ->
    DataDir = ?config(data_dir,Conf),
    PrivDir = priv_dir(Conf),
    ErtsDir = app_dir(erts,old),
    KernelDir = app_dir(kernel,old),
    StdlibDir = app_dir(stdlib,old),

    %% Fake earlier version of kernel and stdlib
    SystemLib = system_lib(PrivDir),
    ok = filelib:ensure_dir(filename:join(SystemLib,"*")),
    KernelLib = code:lib_dir(kernel),
    StdlibLib = code:lib_dir(stdlib),
    ok = copy_tree(Conf,KernelLib,KernelDir,SystemLib),
    ok = copy_tree(Conf,StdlibLib,StdlibDir,SystemLib),
    fix_version(SystemLib,kernel),
    fix_version(SystemLib,stdlib),

    %% Create dirs
    BinDir = filename:join(TargetDir,bin),
    ReleasesDir = filename:join(TargetDir,releases),
    LogDir = filename:join(TargetDir,log),
    ok = filelib:ensure_dir(filename:join(BinDir,"*")),
    ok = filelib:ensure_dir(filename:join(ReleasesDir,"*")),
    ok = filelib:ensure_dir(filename:join(LogDir,"*")),

    %% Copy stuff 
    ErtsLatest = latest_version(filename:join(code:root_dir(),"erts")),
    ok = copy_tree(Conf, ErtsLatest, ErtsDir, TargetDir),
    ErtsBinDir = filename:join([TargetDir,ErtsDir,bin]),

    case os:type() of
	{unix, _} ->
	    copy_file(filename:join([ErtsBinDir, "epmd"]), BinDir, [preserve]),
	    copy_file(filename:join([ErtsBinDir, "run_erl"]), BinDir, [preserve]),
	    copy_file(filename:join([ErtsBinDir, "to_erl"]), BinDir, [preserve]),

	    %% Create the start_erl shell script
	    ok = subst_file(filename:join([ErtsBinDir,"start_erl.src"]),
			    filename:join([BinDir,"start_erl"]),
			    [{"EMU","beam"}],
			    [{chmod,8#0755}]);
	{win32,_} ->
	    %% Add a batch file to use as HEART_COMMAND
	    ok = copy_file(filename:join(DataDir, "heart_restart.bat"),
			   ErtsBinDir,[preserve])
    end,

    copy_file(filename:join(DataDir, "../installer.beam"),
	      filename:join([DataDir,lib,"installer-1.0",ebin])),
    copy_file(filename:join(DataDir, "../rh_test_lib.beam"),
	      filename:join([DataDir,lib,"installer-1.0",ebin])),

    %% Create .rel, .script and .boot files
    RelName = "rel0",
    RelVsn = "P1G",
    RelDir = filename:join(PrivDir,RelName),
    RelFileName = filename:join(RelDir,RelName),
    RelFile = RelFileName ++ ".rel",
    ok = filelib:ensure_dir(RelFile),

    TarFile = create_basic_release(Conf,RelFile,RelVsn,{old,false}),

    %% Extract tar file in target directory (i.e. same directory as erts etc.)
    ok = erl_tar:extract(TarFile, [{cwd, TargetDir}, compressed]),

    %% Create start_erl.data
    StartErlDataFile = filename:join([ReleasesDir, "start_erl.data"]),
    StartErlData = io_lib:fwrite("~s ~s~n", [vsn(erts,old), RelVsn]),
    ok = file:write_file(StartErlDataFile, StartErlData),

    %% Create RELEASES
    ok = release_handler:create_RELEASES(TargetDir,ReleasesDir,RelFile,[]),

    ok.

fix_version(SystemLib,App) ->
    FromVsn = re:replace(vsn(App,current),"\\.","\\\\.",[{return,binary}]),
    ToVsn = re:replace(vsn(App,old),"\\.","\\\\.",[{return,binary}]),
    Rootname = filename:join([SystemLib,app_dir(App,old),ebin,atom_to_list(App)]),

    AppFile = Rootname ++ ".app",
    {ok,OrigApp} = file:read_file(AppFile),
    ok = file:write_file(AppFile,re:replace(OrigApp,FromVsn,ToVsn,
					    [{return,binary}])),
    AppupFile = Rootname ++ ".appup",
    {ok,OrigAppup} = file:read_file(AppupFile),
    ok = file:write_file(AppupFile,re:replace(OrigAppup,FromVsn,ToVsn,
					      [{return,binary}])).


%% Create version P1H - which is P1G + a-1.0
%% Must have run create_p1g first!!
create_p1h(Conf) ->
    create_upgrade_release(Conf,"rel1","P1H",{old,false},[{a,"1.0"}],
			   [{a,[{key2,val2}]}],[{"rel0",[new_appl]}]).

%% Create version P1I - which is P1H, but with application a upgraded to a-1.1
%% Must have run create_p1h first!!
create_p1i(Conf) ->
    create_upgrade_release(Conf,"rel2","P1I",{old,false},[{a,"1.1"}],
			   [{a,[{key2,newval2}]}],
			   [{"rel1",[{extra,gott}]}]).

%% Create version P2A - which is P1I, but with erts-<latest>
%% Must have run create_p1i first!!
create_p2a(Conf) ->
    create_upgrade_release(Conf,"rel3","P2A",{current,code:root_dir()},
			   [{a,"1.1"}],[{a,[{key2,newval2}]}],
			   [{"rel1",[new_emu,new_appl]},{"rel2",[new_emu]}],
			   [{"rel1",[old_emu,old_appl]},{"rel2",[old_emu]}]).

%% Create version P2B - which is P2A, but with relup containing an
%% extra reboot.
%% Can be upgraded to from P1G - so must have run create_p1g first!!
create_p2b(Conf) ->
    create_upgrade_release(Conf,"rel4","P2B",{current,code:root_dir()},
			   [{a,"1.1"}],[{a,[{key2,newval2}]}],
			   [{"rel0",[new_emu,add_appl]}],
			   [{"rel0",[old_emu,rm_appl]}],
			   [restart_emulator]).

%% Create a release tar package which can be installed on top of P1G
create_upgrade_release(Conf,RelName,RelVsn,Erts,Apps,Config,UpFrom) ->
    create_upgrade_release(Conf,RelName,RelVsn,Erts,Apps,Config,UpFrom,[]).
create_upgrade_release(Conf,RelName,RelVsn,Erts,Apps,Config,UpFrom,DownTo) ->
    create_upgrade_release(Conf,RelName,RelVsn,Erts,Apps,Config,UpFrom,DownTo,[]).
create_upgrade_release(Conf,RelName,RelVsn,Erts,Apps,Config,UpFrom0,DownTo0,RelupOpts) ->
    PrivDir = priv_dir(Conf),
    RelDir = filename:join(PrivDir,RelName),
    RelFileName = filename:join(RelDir,RelName),
    RelFile = RelFileName ++ ".rel",
    ok = filelib:ensure_dir(RelFile),

    UpFrom = [{filename:join([PrivDir,UpFromName,UpFromName]),Descr} ||
		 {UpFromName,Descr} <- UpFrom0],
    DownTo = [{filename:join([PrivDir,DownToName,DownToName]),Descr} ||
		 {DownToName,Descr} <- DownTo0],

    create_basic_release(Conf,RelFile,RelVsn,Erts,Apps,Config,
			 UpFrom,DownTo,RelupOpts),
    ok.

%% Create .rel, .script, .boot, sys.config and tar
create_basic_release(Conf,RelFile,RelVsn,{Erts,ErtsDir}) ->
    create_basic_release(Conf, RelFile,RelVsn,{Erts,ErtsDir},[],[],[],[],[]).
create_basic_release(Conf,RelFile,RelVsn,{Erts,ErtsDir},ExtraApps,Config,UpFrom,DownTo,RelupOpts) ->
    DataDir = ?config(data_dir,Conf),
    PrivDir = priv_dir(Conf),
    SystemLib = system_lib(PrivDir),
    LibPath = [filename:join([SystemLib,"*",ebin]),
	       filename:join([DataDir,lib,"*",ebin])],

    RelDir = filename:dirname(RelFile),
    RelFileName = filename:rootname(RelFile),

    %% Create .rel file
    create_installer_rel_file(RelFile,RelVsn,Erts,ExtraApps),

    %% Generate .script and .boot
    ok = systools:make_script(RelFileName,
			      [{path,LibPath},
			       {outdir,RelDir}]),

    %% Generate relup
    ok = systools:make_relup(RelFileName,UpFrom,DownTo,[{path,LibPath},
							{outdir,RelDir} |
							RelupOpts]),

    %% Create sys.config
    ok = write_term_file(filename:join(RelDir,"sys.config"),Config),


    %% Create tar file (i.e. collect all lib/app-*/* and system files)
    ok = systools:make_tar(RelFileName,
			   [{path,LibPath},
			    {outdir,RelDir} |
			    case ErtsDir of
				false -> [];
				_ -> [{erts,ErtsDir}]
			    end]),
    
    TarFileName = RelFileName ++ ".tar.gz",

    case os:type() of
	{win32,_} when ErtsDir=/=false -> modify_tar_win32(Conf, TarFileName);
	_ -> ok
    end,

    TarFileName.

%% Create a .rel file
create_installer_rel_file(RelFile,RelVsn,Erts,ExtraApps) ->
    create_rel_file(RelFile,"SASL-test",RelVsn,Erts,
		    [{installer,"1.0"}|ExtraApps]).

create_rel_file(RelFile,RelName,RelVsn,Erts,ExtraApps) ->
    ErtsVsn = vsn(erts,Erts),
    KernelVsn = vsn(kernel,Erts),
    StdlibVsn = vsn(stdlib,Erts),
    SaslVsn = vsn(sasl,current),
    application:load(tools),
    ToolsVsn = vsn(tools,current),
    application:load(runtime_tools),
    RuntimeToolsVsn = vsn(runtime_tools,current),
    
    RelFileContent = {release,
		      {RelName, RelVsn},
		      {erts, ErtsVsn},
		      [{kernel, KernelVsn},
		       {stdlib, StdlibVsn},
		       {sasl, SaslVsn},
		       {runtime_tools, RuntimeToolsVsn},
		       {tools, ToolsVsn} |
		       ExtraApps]},
    ok = write_term_file(RelFile,RelFileContent).

%% Insert a term in a file, which can be read with file:consult/1.
write_term_file(File,Term) ->
    Str = io_lib:format("%% ~s~n~tp.~n",[epp:encoding_to_string(utf8),Term]),
    Bin = unicode:characters_to_binary(Str),
    ok = file:write_file(File,Bin).
    

%% Check that global group info is correct - try again for a maximum of 5 sec
check_gg_info(Node,OtherAlive,OtherDead,Synced) ->
    check_gg_info(Node,OtherAlive,OtherDead,Synced,5).

check_gg_info(Node,OtherAlive,OtherDead,Synced,N) ->
    GGI = rpc:call(Node, global_group, info, []),
    GI = rpc:call(Node, global, info,[]),
    try do_check_gg_info(OtherAlive,OtherDead,Synced,GGI,GI) 
    catch _:E:Stacktrace when N==0 ->
	    ?t:format("~nERROR: check_gg_info failed for ~p:~n~p~n"
		      "when GGI was: ~p~nand GI was: ~p~n",
		      [Node,{E,Stacktrace},GGI,GI]),
	    ?t:fail("check_gg_info failed");
	  _:E:Stacktrace ->
	    ?t:format("~nWARNING: check_gg_info failed for ~p:~n~p~n"
		      "when GGI was: ~p~nand GI was: ~p~n",
		      [Node,{E,Stacktrace},GGI,GI]),
	    timer:sleep(1000),
	    check_gg_info(Node,OtherAlive,OtherDead,Synced,N-1)
    end.

do_check_gg_info(OtherAlive,OtherDead,Synced,GGI,GI) ->
    {_,gg1} = lists:keyfind(own_group_name,1,GGI),
    {_,synced} = lists:keyfind(state,1,GGI),
    {_,AllNodes} = lists:keyfind(own_group_nodes,1,GGI),
    true = lists:sort(AllNodes) =:= lists:sort(OtherAlive++OtherDead),
    {_,[]} = lists:keyfind(sync_error,1,GGI),
    {_,[{gg2,[_,_]}]} = lists:keyfind(other_groups,1,GGI),

    %% There is a known bug in global_group (OTP-9177) which causes
    %% the following to fail every now and then:
    %% {_,SyncedNodes} = lists:keyfind(synced_nodes,1,GGI),
    %% true = lists:sort(SyncedNodes) =:= lists:sort(Synced),
    %% {_,NoContact} = lists:keyfind(no_contact,1,GGI),
    %% true = lists:sort(NoContact) =:= lists:sort(OtherDead),

    %% Therefore we use global:info instead for this part
    {state,_,_,SyncedNodes,_,_,_,_,_,_,_} = GI,
    true = lists:sort(SyncedNodes) =:= lists:sort(Synced),

    %% .. and we only check that all OtherDead are listed as
    %% no_contact (due to th bug there might be more nodes in this
    %% list)
    {_,NoContact} = lists:keyfind(no_contact,1,GGI),
    true =
	lists:sort(OtherDead) =:=
	lists:sort([NC || NC <- NoContact,lists:member(NC,OtherDead)]),

    ok.

%% Return the configuration (to be inserted in sys.config) for global group tests
gg_config(Snames) ->
    Nodes = [node_name(Sname) || Sname <- Snames],
    [{kernel, [{sync_nodes_optional, Nodes},
	       {sync_nodes_timeout, 10000},
	       {global_groups, 
		[{gg1, Nodes},
		 {gg2, [node_name(Sname) || Sname <- [ggq,ggw]]}]}]},
     {a, [{key2, val2}]}].

%% Start a node with short name SnameStr, and unpack P1H
unpack_p1h(Conf,Sname) ->
    PrivDir = priv_dir(Conf),
    [Node] = start_nodes(Conf,[Sname],"create_p1h"),
    ok = rpc_inst(Node, unpack_p1h, [PrivDir]),
    Node.

%% On the given node, install P1H and make it permanent
%% This function is to be called after unpack_p1h/2, with the same node.
permanent_p1h(Node) ->
    ok = rpc_inst(Node, permanent_p1h, []).

%% For each node in ToNodes, create a target installation which is
%% indentical to the target installation for FromNode.
copy_installed(Conf,FromNode,ToNodes) ->
    PrivDir = priv_dir(Conf),
    DataDir = ?config(data_dir,Conf),

    %% Instead of using copy_tree on the complete node directory, I'm
    %% splitting this in separate tar files per subdirectory so the
    %% log directory can be completely skipped. The reason for this is
    %% that the tar file might become faulty if the node is alive and
    %% writing to the log while the tar is created.
    FromDir = filename:join(PrivDir,FromNode),
    {ok,FromDirNames} = file:list_dir(FromDir),
    TempTarFiles =
	[begin
	     TempTarFile = filename:join(PrivDir,"temp_" ++ FDN ++ ".tar"),
	     {ok,Tar} = erl_tar:open(TempTarFile,[write]),
	     ok = erl_tar:add(Tar,filename:join(FromDir,FDN),FDN,[]),
	     ok = erl_tar:close(Tar),
	     TempTarFile
         end || FDN <- FromDirNames, FDN=/="log"],
    lists:foreach(
      fun(Node) ->
	      NodeDir = filename:join(PrivDir,Node),
	      ok = filelib:ensure_dir(filename:join([NodeDir,"log","*"])),
              lists:foreach(
		fun(TempTarFile) ->
                        ok = erl_tar:extract(TempTarFile,[{cwd,NodeDir}])
		end, TempTarFiles),
	      case os:type() of
		  {unix,_} ->
		      %% Create start script
		      %% Using a customized start script from DataDir
		      %% where some options (heart and nodename) are
		      %% added compared to the start.src in the erlang
		      %% distribution.
		      ok = subst_file(filename:join(DataDir, "start"),
				      filename:join([NodeDir, "bin", "start"]),
				      [{"ROOT",NodeDir}],
				      [preserve]);
		  {win32,_} ->
		      %% Write erl.ini
		      ErtsDirs =
			  filelib:wildcard(filename:join(NodeDir,"erts-*")),
		      lists:foreach(
			fun(ErtsDir) ->
				ok = subst_file(
				       filename:join(DataDir, "erl.ini.src"),
				       filename:join([ErtsDir, "bin", "erl.ini"]),
				       [{"ROOTDIR",NodeDir},
					{"BINDIR",filename:join(ErtsDir,"bin")}])
			end,
			ErtsDirs),

		      %% The service on windows runs as local
		      %% administrator (not otptest user), so we need
		      %% to chmod the release in order to allow the
		      %% executing node to install releases, write
		      %% logs etc.
		      chmod_release_win32(NodeDir)
	      end
      end,
      ToNodes),

    lists:foreach(fun(TempTarFile) -> file:delete(TempTarFile) end, TempTarFiles),
    ok.

chmod_release_win32(Dir) ->
    os:cmd("echo y|cacls " ++ Dir ++ " /T /E /G Administrators:F").

start_nodes(Conf,Snames,Tag) ->
    PrivDir = priv_dir(Conf),
    Nodes = 
	lists:map(
	  fun(Sname) ->
		  NodeDir = filename:join(PrivDir,Sname),
		  Node = node_name(Sname),

		  case os:type() of
		      {unix,_} ->
			  start_node_unix(Sname,NodeDir);
		      {win32,_} ->
			  start_node_win32(Sname,NodeDir)
		  end,
		  Node
	  end,
	  Snames),
    wait_nodes_up(Nodes,Tag),
    Nodes.
    
start_node_unix(Sname,NodeDir) ->
    Script = filename:join([NodeDir,"bin","start"]),
    ?t:format("Starting ~p: ~ts~n", [Sname,Script]),
    case rh_test_lib:cmd(Script,[],[{"NODENAME",atom_to_list(Sname)}]) of
	ok ->
	    {ok,node_name(Sname)};
        Error ->
            ?t:fail({failed_to_start_node, Sname, Error})
    end.


start_node_win32(Sname,NodeDir) ->
    Name = atom_to_list(Sname) ++ "_P1G",
    ErtsBinDir = filename:join([NodeDir,app_dir(erts,old),"bin"]),

    StartErlArgs = rh_test_lib:get_start_erl_args(NodeDir),
    ServiceArgs = rh_test_lib:get_service_args(NodeDir, Sname, StartErlArgs),

    Erlsrv = filename:nativename(filename:join(ErtsBinDir,"erlsrv")),
    rh_test_lib:erlsrv(Erlsrv,stop,Name),
    rh_test_lib:erlsrv(Erlsrv,remove,Name),
    ok = rh_test_lib:erlsrv(Erlsrv,add,Name,ServiceArgs),
    ok = rh_test_lib:erlsrv(Erlsrv,start,Name),
    ok.

%% Create a unique node name for each test case
tc_sname(Config) ->
    tc_sname(Config,"").
tc_sname(Config,Fix) when is_atom(Fix) ->
    tc_sname(Config,atom_to_list(Fix));
tc_sname(Config,Fix) when is_list(Fix) ->
    list_to_atom(
      atom_to_list(?MODULE)
      ++ "-" ++ atom_to_list(?config(sname_prefix, Config)) ++
	  case Fix of
	      "" -> "";
	      _ -> "-" ++ Fix
	  end).

tc_full_node_name(Config) ->
    tc_full_node_name(Config,"").
tc_full_node_name(Config,Fix) ->
    node_name(tc_sname(Config,Fix)).


%% When installing a release for which the sys.config includes added
%% or changed global group(s), this node (test_sever@host) will be
%% disconnected from the test node (Node) by global_group.erl. This
%% will cause the rpc:call to terminate with {badrpc,nodedown} even if
%% the installation succeeds. This function installs the release,
%% accepts the faulty return value and then checks if the release was
%% successfully installed.
install_release_changed_gg(Node,RelVsn) ->
    stop_cover(Node),
    {badrpc,nodedown} = rpc:call(Node,release_handler,install_release,[RelVsn]),
    timer:sleep(100),
    wait_installed(Node,RelVsn,4).

wait_installed(Node,RelVsn,0) ->
    ?t:fail("install_release_changed_gg failed for " ++ RelVsn ++ 
	    " on " ++ atom_to_list(Node));
wait_installed(Node,RelVsn,N) ->
    Rels = rpc:call(Node,release_handler,which_releases,[]),
    case lists:keyfind(RelVsn, 2, Rels) of
	{"SASL-test", RelVsn, _Libs, current} -> 
	    start_cover(Node),
	    ok;
	_ -> 
	    timer:sleep(500),
	    wait_installed(Node,RelVsn,N-1)
    end.

%% Start/stop cover measurements on the given node
start_cover(Node) ->
    cover_fun(Node,start).
stop_cover(Node) ->
    cover_fun(Node,stop).

cover_fun(Node,Func) ->
    case ?t:is_cover() of
	true ->
	    cover:Func(Node);
	false ->
	    ok
    end.

%%%-----------------------------------------------------------------
%%% Create a fake release ....

%% This function will create and install a release build on the
%% current running OTP release. It includes kernel, stdlib and sasl,
%% and possibly other applications if they are listed in AppDirs =
%% [{App,Vsn,LibDir}]
create_and_install_fake_first_release(Dir,AppDirs) ->
    create_and_install_fake_first_release(Dir,init:script_id(),AppDirs).
create_and_install_fake_first_release(Dir,{RelName,RelVsn},AppDirs) ->
    {Rel,_} = create_fake_release(Dir,RelName,RelVsn,AppDirs),
    ReleasesDir = filename:join(Dir, "releases"),
    RelDir = filename:dirname(Rel),

    %% And install it
    RelVsnDir = filename:join(ReleasesDir, RelVsn),
    ok = filelib:ensure_dir(filename:join(RelVsnDir,"*")),

    ok = copy_file(Rel++".rel",RelVsnDir),
    ok = copy_file(Rel++".boot",filename:join(RelVsnDir, "start.boot")),
    ok = copy_file(filename:join(RelDir,"sys.config"),RelVsnDir),

    ok = release_handler:create_RELEASES(code:root_dir(),
					 ReleasesDir,
					 Rel++".rel",
					 AppDirs),

    Rel.
    
%% This function create a new release, including a relup file. It can
%% be upgraded to from the release created by
%% create_and_install_fake_first_release/2. Unpack first by calls to
%% release_handler:set_unpacked and release_handler:install_file.
create_fake_upgrade_release(Dir,RelVsn,AppDirs,UpgrInstr) when not is_tuple(RelVsn) ->
    {RelName,_} = init:script_id(),
    create_fake_upgrade_release(Dir,{RelName,RelVsn},AppDirs,UpgrInstr);
create_fake_upgrade_release(Dir,{RelName,RelVsn},AppDirs,{UpFrom,DownTo,ExtraLibs}) ->
    %% Create a new release
    {Rel,Paths} = create_fake_release(Dir,RelName,RelVsn,AppDirs),
    RelDir = filename:dirname(Rel),

    %% And a relup file so it can be upgraded to
    RelupPath = Paths ++ [filename:join([Lib,"*","ebin"]) || Lib <- ExtraLibs],
    ok = systools:make_relup(Rel,UpFrom,DownTo,[{path,RelupPath},
						{outdir,RelDir}]),
    
    Rel.


create_fake_release(Dir,RelName,RelVsn,AppDirs) ->
    %% Create .rel files
    RelDir = filename:join(Dir,"rel_" ++ RelVsn),
    Rel = filename:join([RelDir,"rel_" ++ RelVsn]),
    ok = filelib:ensure_dir(Rel),

    {Apps,Paths} = 
	lists:foldl(fun({App,Vsn,Lib},{As,Ps}) ->
			    {[{App,Vsn}|As],
			     lists:umerge([filename:join([Lib,"*",ebin])],Ps)}
		    end,
		    {[],[]},
		    AppDirs),

    create_rel_file(Rel++".rel",RelName,RelVsn,current,Apps),
    
    %% Generate boot scripts
    ok = systools:make_script(Rel,[local,
				   {path, Paths},
				   {outdir,RelDir}]),
    ok = copy_file(Rel++".boot", filename:join(RelDir,"start.boot")),

    %% Use an own 'releases' directory - we don't want to change the
    %% contents of $OTP_ROOT/releases
    %% Inform SASL about this via sys.config
    ReleasesDir = filename:join(Dir, "releases"),
    Config = [{sasl,[{releases_dir,ReleasesDir}]}],
    ok = write_term_file(filename:join(RelDir,"sys.config"), Config),
    
    {Rel,Paths}.
    

rpc_inst(Node,Func,Args) ->
    rpc:call(Node,installer,Func,[node()|Args]).

delete_all_services() ->
    ErlSrv = erlsrv:erlsrv(erlang:system_info(version)),
    [_|Serviceinfo] = string:tokens(os:cmd("\"" ++ ErlSrv ++ "\" list"),"\n"),
    Services =
	[lists:takewhile(fun($\t) -> false; (_) -> true end,S)
	 || S <- Serviceinfo],
    ?t:format("Services to remove: ~p~n",[Services]),
    lists:foreach(fun(S) ->
			  rh_test_lib:erlsrv(ErlSrv,stop,S),
			  rh_test_lib:erlsrv(ErlSrv,remove,S)
		  end,
		  Services).

modify_tar_win32(Conf, TarFileName) ->
    DataDir = ?config(data_dir,Conf),
    PrivDir = priv_dir(Conf),
    TmpDir = filename:join(PrivDir,"tmp_modify_tar_win32"),
    ok = erl_tar:extract(TarFileName,[{cwd,TmpDir},compressed]),

    ErtsBinDir = filelib:wildcard(filename:join([TmpDir,"erts-*","bin"])),
    ok = copy_file(filename:join(DataDir, "heart_restart.bat"),
		   ErtsBinDir,[preserve]),

    {ok,Fs} = file:list_dir(TmpDir),
    {ok,T} = erl_tar:open(TarFileName,[write,compressed]),
    [ok = erl_tar:add(T,filename:join(TmpDir,F),F,[]) || F <- Fs],
    ok = erl_tar:close(T),
    ok.

app_dir(App,Vsn) ->
    atom_to_list(App) ++ "-" ++ vsn(App,Vsn).
vsn(erts,old) -> ?ertsvsn;
vsn(kernel,old) -> ?kernelvsn;
vsn(stdlib,old) -> ?stdlibvsn;
vsn(erts,current) -> erlang:system_info(version);
vsn(App,current) ->
    {ok,Vsn} = application:get_key(App,vsn),
    Vsn.

system_lib(PrivDir) ->
    filename:join(PrivDir,"system_lib").
