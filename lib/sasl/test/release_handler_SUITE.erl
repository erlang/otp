%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011. All Rights Reserved.
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
-module(release_handler_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

% Default timetrap timeout (set in init_per_testcase).
%-define(default_timeout, ?t:minutes(40)).
-define(default_timeout, ?t:minutes(10)).

suite() ->
    [{ct_hooks, [ts_install_cth]}].

init_per_suite(Config) ->
    application:start(sasl),
    Config.

end_per_suite(_Config) ->
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
    [target_system] ++ RunErlCases ++ cases().

win32_cases() -> 
    cases().

%% Cases that can be run on all platforms
cases() ->
    [otp_2740, otp_2760, otp_5761, instructions, eval_appup].

groups() ->
    [{release,[],
      [
       {group,release_single},
       {group,release_gg}
      ]},
     {release_single,[],
      [
       upgrade,
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
    Dog = ?t:timetrap(?default_timeout),
    P1gInstall = filename:join(priv_dir(Config),p1g_install),
    ok = do_create_p1g(Config,P1gInstall),
    ok = create_p1h(Config),
    ?t:timetrap_cancel(Dog);

%% {group,release_single}
%% Subgroup of {group,release}, contains all cases that are not
%% related to global_group
init_per_group(release_single, Config) ->
    Dog = ?t:timetrap(?default_timeout),

    %% Create some more releases to upgrade to
    ok = create_p1i(Config),
    ok = create_p2a(Config),

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
    delete_release(Config),
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

	    LogDirs = filelib:wildcard(filename:join([PrivDir,"*",log])),

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

    try apply(?MODULE,Case,[cleanup,Config])
    catch error:undef -> ok
    end,
    ok.

gg_node_snames(Config) ->
    [tc_sname(Config,X) || X <- [gg1,gg2,gg3,gg4,gg5,gg6]].


%%%-----------------------------------------------------------------
%%% TEST CASES


%% Executed instead of release group when no run_erl program exists
no_run_erl(Config) when is_list(Config) ->
    {comment, "No run_erl program"}.



%% Test upgrade and downgrade of erts
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

    %% check that P1H is permanent, unpack and install P1I, unpack and install P2A
    TestNodeInit1 = rpc:call(TestNode,erlang,whereis,[init]),
    ok = rpc_inst(TestNode, install_3, [PrivDir]),
    stop_cover(TestNode),
    ok = rpc_inst(TestNode, install_3a, []),
    wait_nodes_up([{TestNode,TestNodeInit1}],"install_3",[a]),

    %% check that P2A is used, reboot from P1I
    ok = rpc_inst(TestNode, install_4, []),
    stop_cover(TestNode),
    reboot_and_wait(TestNode,"install_4",[a]),

    %% check that P1I, reinstall P2A
    TestNodeInit2 = rpc:call(TestNode,erlang,whereis,[init]),
    ok = rpc_inst(TestNode, install_5, []),
    stop_cover(TestNode),
    ok = rpc_inst(TestNode, install_5a, []),
    wait_nodes_up([{TestNode,TestNodeInit2}],"install_5",[a]),

    %% check that P2A is used, make P2A permanent
    ok = rpc_inst(TestNode, install_6, []),
    stop_cover(TestNode),
    reboot_and_wait(TestNode,"install_6",[a]),

    %% check that P2A is permanent, install old P1H
    TestNodeInit3 = rpc:call(TestNode,erlang,whereis,[init]),
    stop_cover(TestNode),
    ok = rpc_inst(TestNode, install_7, []),
    wait_nodes_up([{TestNode,TestNodeInit3}],"install_7",[a]),

    %% check that P1H is permanent, remove P1I and P2A
    ok = rpc_inst(TestNode, install_8, []),
    stop_cover(TestNode),
    reboot_and_wait(TestNode,"install_8",[a]),

    %% check that P1H is permanent, reboot old P1G
    TestNodeInit4 = rpc:call(TestNode,erlang,whereis,[init]),
    stop_cover(TestNode),
    ok = rpc_inst(TestNode, install_9, []),
    wait_nodes_up([{TestNode,TestNodeInit4}],"install_9"),

    %% check that P1G is permanent, remove P1H
    ok = rpc_inst(TestNode, install_10, []),
    stop_cover(TestNode),
    reboot_and_wait(TestNode,"install_10"),

    %% check that P1G is permanent
    ok = rpc_inst(TestNode, install_11, []),

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


%% Test upgrade and downgrade of erts, diskless
client1(Conf) when is_list(Conf) ->
    reg_print_proc(), %% starts a printer process on test_server node
    PrivDir = priv_dir(Conf),
    Master = tc_sname(Conf,master),
    Client = tc_sname(Conf,client),
    MasterDir = filename:join(PrivDir,Master),

    %% Copy the P1G release to a directory for use in this testcase
    ok = copy_installed(Conf,p1g_install,[Master]),
    ok = copy_client(Conf,Master,Client,"start_cli1"),

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
    ok = copy_client(Conf,Master,Client,"start_cli2"),

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
    {ok, _} = release_handler_1:eval_script(S1, [], []),

    case whereis(cc) of
        Pid2 when is_pid(Pid2) -> ok;
        _ -> ?t:fail("cc not started")
    end,

    %% Make bb run old version of b.
    S2 = [point_of_no_return,
          {remove, {b, soft_purge, soft_purge}}],
    {ok, [{b, soft_purge}]} = release_handler_1:eval_script(S2, [], []),
    check_bstate("first", [FirstBB]),

    false = code:is_loaded(b),
    {error,{old_processes,b}} = release_handler_1:eval_script(S2,[],[]),
    check_bstate("first", [FirstBB]),

    %% Let supervisor restart bb with new code
    S3 = [point_of_no_return,
          {purge, [b]}],
    {ok, []} = release_handler_1:eval_script(S3, [], []),
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
    {ok, HopefullyEmpty} = release_handler_1:eval_script(S4, [], []),
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
    Rel2 = create_fake_upgrade_release(Dir,"after",[],{Rel1,Rel1,[LibDir]}),
    Rel2Dir = filename:dirname(Rel2),

    %% Start a node with Rel1.boot and check that the app1 module is loaded
    {ok, Node} = t_start_node(otp_2760, Rel1, []),
    {file, _} = rpc:call(Node, code, is_loaded, [app1]),

    %% Execute the relup script and check that app1 is unloaded
    {ok, [{"after", [{_Rel1Vsn, _Descr, Script}], _}]} =
	file:consult(filename:join(Rel2Dir, "relup")),
    {ok, []} = rpc:call(Node, release_handler_1, eval_script,
			      [Script, [], []]),
    false = rpc:call(Node, code, is_loaded, [app1]),

    true = stop_node(Node),
    ok.

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
				       {Rel1,Rel1,[LibDir1]}),
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

    %% Stop the slave node
    true = stop_node(Node),
    ok.

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

    %% Upgrade to app1-2.0
    {ok, []} = release_handler:upgrade_app(app1, App12Dir),
    App12Dir = code:lib_dir(app1),
    error = gen_server:call(harry, error),

    %% OTP-6162
    %% Value of config parameter 'var' should now be 'val2'
    %% (see myrel/lib2/app1-2.0/ebin/app1.app)
    [{var,val2}] = ets:lookup(otp_6162, var),

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


%% Test the example/target_system.erl module
target_system(Conf) when is_list(Conf) ->
    PrivDir = priv_dir(Conf),
    DataDir = ?config(data_dir,Conf),

    TargetCreateDir = filename:join([PrivDir,"target_system","create"]),
    TargetInstallDir = filename:join([PrivDir,"target_system","install"]),

    ok = filelib:ensure_dir(filename:join(TargetCreateDir,"xx")),
    ok = filelib:ensure_dir(filename:join(TargetInstallDir,"xx")),


    %% Create the .rel file
    ErtsVsn = erlang:system_info(version),
    RelName = filename:join(TargetCreateDir,"ts-1.0"),
    RelFile = RelName++".rel",
    RelVsn = "R1A",
    create_rel_file(RelFile,RelName,RelVsn,ErtsVsn,[{a, "1.0"}]),

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
    true = filelib:is_dir(filename:join(TargetInstallDir,"erts-"++ErtsVsn)),
    LibDir = filename:join(TargetInstallDir,lib),
    {ok,KernelVsn} = application:get_key(kernel,vsn),
    {ok,StdlibVsn} = application:get_key(stdlib,vsn),
    {ok,SaslVsn} = application:get_key(sasl,vsn),
    true = filelib:is_dir(filename:join(LibDir,"kernel-"++KernelVsn)),
    true = filelib:is_dir(filename:join(LibDir,"stdlib-"++StdlibVsn)),
    true = filelib:is_dir(filename:join(LibDir,"sasl-"++SaslVsn)),
    true = filelib:is_dir(filename:join(LibDir,"a-1.0")),
    RelDir = filename:join(TargetInstallDir,releases),
    true = filelib:is_regular(filename:join(RelDir,"RELEASES")),
    true = filelib:is_regular(filename:join(RelDir,"start_erl.data")),
    true = filelib:is_regular(filename:join(RelDir,
						  filename:basename(RelFile))),
    true = filelib:is_dir(filename:join(RelDir,RelVsn)),
    true = filelib:is_regular(filename:join([RelDir,RelVsn,"start.boot"])),
    BinDir = filename:join(TargetInstallDir,bin),
    true = filelib:is_regular(filename:join(BinDir,"start.boot")),
    true = filelib:is_regular(filename:join(BinDir,erl)),
    true = filelib:is_regular(filename:join(BinDir,start_erl)),
    true = filelib:is_regular(filename:join(BinDir,start)),
    true = filelib:is_regular(filename:join(BinDir,epmd)),
    true = filelib:is_regular(filename:join(BinDir,run_erl)),
    true = filelib:is_regular(filename:join(BinDir,to_erl)),

    %% Check content of files
    {ok,SED} = file:read_file(filename:join(RelDir,"start_erl.data")),
    [ErtsVsn,RelVsn] = string:tokens(binary_to_list(SED),"\s\n"),
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

    %% Give some time to synch nodes, then check global group info.
    timer:sleep(1000),
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
    wait_nodes_up(Nodes, Tag, lists:umerge(Apps,[kernel,stdlib,sasl]), 30).

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
	    timer:sleep(1000),
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
    Args = 
	case Boot of
	    [] -> [];
	    _ -> " -boot " ++ Boot
	end ++
	case SysConfig of
	    [] -> [];
	    _ -> " -config " ++ SysConfig
	end,
    test_server:start_node(Name, slave, [{args, Args}]).

stop_node(Node) ->
    ?t:stop_node(Node).


copy_client(Conf,Master,Sname,StartScript) ->
    io:format("copy_client(Conf)"),

    DataDir = ?config(data_dir, Conf),
    MasterDir = filename:join(priv_dir(Conf),Master),

    {ok,Host} = inet:gethostname(),
    {ok,IpTuple} = inet:getaddr(Host,inet),
    IpAddr =  inet_parse:ntoa(IpTuple),

    CliNode = node_name(Sname),

    Cli = filename:join([MasterDir, "clients", "type1", CliNode]),
    ok = filelib:ensure_dir(filename:join([Cli,"bin","."])),
    ok = filelib:ensure_dir(filename:join([Cli,"releases","."])),
    ok = filelib:ensure_dir(filename:join([Cli,"log","."])),

    P1GOrig = filename:join([MasterDir, "releases", "P1G"]),
    ok = copy_tree(Conf,P1GOrig,filename:join(Cli,"releases")),
    
    ok = subst_file(filename:join([DataDir, "clients", StartScript]),
		    filename:join([Cli,"bin","start"]),
		    [{"ROOT",MasterDir},
		     {"MASTER",atom_to_list(Master)},
		     {"IPADDR",IpAddr}],
		    [{chmod,8#0755}]),
    
    StartErlData = filename:join([MasterDir, "releases", "start_erl.data"]),
    CliRelDir = filename:join([Cli, "releases"]),
    copy_file(StartErlData, CliRelDir),
    
    RR = filename:join([MasterDir, "releases", "RELEASES"]),
    copy_file(RR, CliRelDir),

    ok.


delete_release(Conf) ->
    PrivDir = priv_dir(Conf),

    {ok, OrigWd} = file:get_cwd(),

    ok = file:set_cwd(PrivDir),
    ?t:format("========  current dir ~p~n",[PrivDir]),
    {ok, Dirs} = file:list_dir(PrivDir),
    ?t:format("========  deleting  ~p~n",[Dirs]),

    ok = delete_release_os(Dirs),
    ?t:format("========  remaining  ~p~n",[file:list_dir(PrivDir)]),
    ok = file:set_cwd(OrigWd),
    ok.


delete_release_os(Dirs) ->
    case os:type() of
	      {unix, _} ->
		  delete_release_unix(Dirs);
	      {win32, _} ->
		  delete_release_win32(Dirs);
	      Os ->
		  test_server:fail({error, {not_yet_implemented_os, Os}})
	  end.


delete_release_unix([]) ->
    ok;
delete_release_unix(["save"|Dirs]) ->
    delete_release_unix(Dirs);
delete_release_unix([Dir|Dirs]) ->
    Rm = string:concat("rm -rf ", Dir),
    ?t:format("============== COMMAND ~p~n",[Rm]),
    case file:list_dir(Dir) of
	{error, enotdir} ->
	    ok;
	X ->
	    ?t:format("------- Dir ~p~n       ~p~n",[Dir, X])
    end,
    case os:cmd(Rm) of
	      [] ->
		  ?t:format("------- Result of COMMAND ~p~n",[ok]);
	      Y ->
		  ?t:format("!!!!!!! delete ERROR  Dir ~p Error ~p~n",[Dir, Y]),
		  ?t:format("------- ls -al  ~p~n",[os:cmd("ls -al " ++ Dir)])
	  end,

    delete_release_unix(Dirs).

delete_release_win32([]) ->
    ok;
delete_release_win32(["save"|Dirs]) ->
    delete_release_win32(Dirs);
delete_release_win32([Dir|Dirs]) ->
    Rm = string:concat("rmdir /s ", Dir),
    [] = os:cmd(Rm),
    delete_release_win32(Dirs).


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
    io:format("Copy ~s to ~s failed: ~s\n",
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
    Conts = binary_to_list(Bin),
    NConts = subst(Conts, Vars),
    ok = file:write_file(Dest, NConts),
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
    filename:absname(?config(priv_dir, Conf)). % Get rid of trailing slash

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
	    lists:foreach(fun(Term) -> ?t:format("    ~p",[Term]) end, T),
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
create_p1g(Conf,Sname) ->
    do_create_p1g(Conf,filename:join(priv_dir(Conf),Sname)).

do_create_p1g(Conf,TargetDir) ->
    PrivDir = priv_dir(Conf),
    DataDir = ?config(data_dir,Conf),
    ErtsVsn = "4.4",
    ErtsDir = "erts-"++ErtsVsn,

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
    copy_file(filename:join([ErtsBinDir, "epmd"]), BinDir, [preserve]),
    copy_file(filename:join([ErtsBinDir, "run_erl"]), BinDir, [preserve]),
    copy_file(filename:join([ErtsBinDir, "to_erl"]), BinDir, [preserve]),

    copy_file(filename:join(DataDir, "../installer.beam"),
	      filename:join([DataDir,lib,"installer-1.0",ebin])),

    %% Create .rel, .script and .boot files
    RelName = "rel0",
    RelVsn = "P1G",
    RelDir = filename:join(PrivDir,RelName),
    RelFileName = filename:join(RelDir,RelName),
    RelFile = RelFileName ++ ".rel",
    ok = filelib:ensure_dir(RelFile),
    LibPath = filename:join([DataDir,lib,"*",ebin]),

    TarFile = create_basic_release(RelFile, RelVsn, {ErtsVsn,false},
				   LibPath, [], [], [], []),

    %% Extract tar file in target directory (i.e. same directory as erts etc.)
    ok = erl_tar:extract(TarFile, [{cwd, TargetDir}, compressed]),

    %% Create start_erl.data
    StartErlDataFile = filename:join([ReleasesDir, "start_erl.data"]),
    StartErlData = io_lib:fwrite("~s ~s~n", [ErtsVsn, RelVsn]),
    ok = file:write_file(StartErlDataFile, StartErlData),

    %% Create RELEASES
    ok = release_handler:create_RELEASES(TargetDir,ReleasesDir,RelFile,[]),

    %% Create start_erl
    ok = subst_file(filename:join([ErtsBinDir,"start_erl.src"]),
		    filename:join([BinDir,"start_erl"]),
		    [{"EMU","beam"}],
		    [{chmod,8#0755}]),
    
    %% Create start script
    %% Using a customized start script from DataDir where some options
    %% (heart and nodename) are added compared to the start.src in the
    %% erlang distribution.
    ok = subst_file(filename:join(DataDir, "start"),
		    filename:join([BinDir, "start"]),
		    [{"ROOT",TargetDir}],
		    [preserve]),
    ok.

%% Create version P1H - which is P1G + a-1.0
%% Must have run create_p1g first!!
create_p1h(Conf) ->
    create_upgrade_release(Conf,"rel1","P1H",{"4.4",false},[{a,"1.0"}],
			   [{a,[{key2,val2}]}],{"rel0",[new_appl]}).

%% Create version P1I - which is P1H, but with application a upgraded to a-1.1
%% Must have run create_p1h first!!
create_p1i(Conf) ->
    create_upgrade_release(Conf,"rel2","P1I",{"4.4",false},[{a,"1.1"}],
			   [{a,[{key2,newval2}]}],
			   {"rel1",[{extra,gott}]}).

%% Create version P2A - which is P1I, but with erts-<latest>
%% Must have run create_p1i first!!
create_p2a(Conf) ->
    ErtsVsn = erlang:system_info(version),
    create_upgrade_release(Conf,"rel3","P2A",{ErtsVsn,code:root_dir()},
			   [{a,"1.1"}],[{a,[{key2,newval2}]}],
			   {"rel2",[new_emu]}).

%% Create a release tar package which can be installed on top of P1G
create_upgrade_release(Conf,RelName,RelVsn,Erts,Apps,Config,{UpFromName,Descr}) ->
    PrivDir = priv_dir(Conf),
    DataDir = ?config(data_dir,Conf),

    RelDir = filename:join(PrivDir,RelName),
    RelFileName = filename:join(RelDir,RelName),
    RelFile = RelFileName ++ ".rel",
    ok = filelib:ensure_dir(RelFile),
    LibPath = filename:join([DataDir,lib,"*",ebin]),

    UpFrom = [{filename:join([PrivDir,UpFromName,UpFromName]),Descr}],

    create_basic_release(RelFile, RelVsn, Erts, LibPath,
			 Apps, Config, UpFrom, []),
    ok.

%% Create .rel, .script, .boot, sys.config and tar
create_basic_release(RelFile,RelVsn,{ErtsVsn,ErtsDir},LibPath,ExtraApps,Config,UpFrom,DownTo) ->
    RelDir = filename:dirname(RelFile),
    RelFileName = filename:rootname(RelFile),

    %% Create .rel file
    create_installer_rel_file(RelFile,RelVsn,ErtsVsn,ExtraApps),

    %% Generate .script and .boot
    ok = systools:make_script(RelFileName,
			      [{path,[LibPath]},
			       {outdir,RelDir}]),

    %% Generate relup
    ok = systools:make_relup(RelFileName,UpFrom,DownTo,[{path,[LibPath]},
							      {outdir,RelDir}]),

    %% Create sys.config
    ok = write_term_file(filename:join(RelDir,"sys.config"),Config),


    %% Create tar file (i.e. collect all lib/app-*/* and system files)
    ok = systools:make_tar(RelFileName,
			   [{path,[LibPath]},
			    {outdir,RelDir} |
			    case ErtsDir of
				false -> [];
				_ -> [{erts,ErtsDir}]
			    end]),
    
    RelFileName ++ ".tar.gz".

%% Create a .rel file
create_installer_rel_file(RelFile,RelVsn,ErtsVsn,ExtraApps) ->
    create_rel_file(RelFile,"SASL-test",RelVsn,ErtsVsn,
		    [{installer,"1.0"}|ExtraApps]).

create_rel_file(RelFile,RelName,RelVsn,ErtsVsn,ExtraApps) ->
    {ok,KernelVsn} = application:get_key(kernel,vsn),
    {ok,StdlibVsn} = application:get_key(stdlib,vsn),
    {ok,SaslVsn} = application:get_key(sasl,vsn),
    application:load(tools),
    {ok,ToolsVsn} = application:get_key(tools,vsn),
    application:load(runtime_tools),
    {ok,RuntimeToolsVsn} = application:get_key(runtime_tools,vsn),
    
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
    ok = file:write_file(File,io_lib:format("~p.~n",[Term])).
    

%% Check that global group info is correct
check_gg_info(Node,OtherAlive,OtherDead,Synced) ->
    GGI = rpc:call(Node, global_group, info, []),
    GI = rpc:call(Node, global, info,[]),
    try do_check_gg_info(OtherAlive,OtherDead,Synced,GGI,GI) 
    catch _:E ->
	    ?t:format("~ncheck_gg_info failed for ~p: ~p~nwhen GGI was: ~p~n"
		      "and GI was: ~p~n",
		      [Node,E,GGI,GI]),
	    ?t:fail("check_gg_info failed")
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
    lists:foreach(
      fun(Node) ->
	      ok = copy_tree(Conf,filename:join(PrivDir,FromNode),Node,PrivDir),
	      NodeDir = filename:join(PrivDir,Node),
	      ok = subst_file(filename:join(DataDir, "start"),
			      filename:join([NodeDir, "bin", "start"]),
			      [{"ROOT",NodeDir}]),
	      LogDir = filename:join(NodeDir,log),
	      {ok,Logs} = file:list_dir(LogDir),
	      lists:foreach(fun(Log) ->
				    file:delete(filename:join(LogDir,Log))
			    end,
			    Logs)
      end,
      ToNodes).

start_nodes(Conf,Snames,Tag) ->
    PrivDir = priv_dir(Conf),
    Nodes = 
	lists:map(
	  fun(Sname) ->
		  NodeDir = filename:join(PrivDir,Sname),
		  Node = node_name(Sname),
		  
		  Script = filename:join([NodeDir,"bin","start"]),
		  Cmd = "env NODENAME="++atom_to_list(Sname) ++ " " ++ Script,
		  %% {ok,StartFile} = file:read_file(Cmd),
		  %% io:format("~s:\n~s~n~n",[Start,binary_to_list(StartFile)]),
		  Res = os:cmd(Cmd),
		  io:format("Start ~p: ~p~n=>\t~p~n", [Sname,Cmd,Res]),
		  Node
	  end,
	  Snames),
    wait_nodes_up(Nodes,Tag),
    Nodes.
    
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
    %% Create the first release
    {RelName,RelVsn} = init:script_id(),
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
create_fake_upgrade_release(Dir,RelVsn,AppDirs,{UpFrom,DownTo,ExtraLibs}) ->
    %% Create a new release
    {RelName,_} = init:script_id(),
    {Rel,Paths} = create_fake_release(Dir,RelName,RelVsn,AppDirs),
    RelDir = filename:dirname(Rel),

    %% And a relup file so it can be upgraded to
    RelupPath = Paths ++ [filename:join([Lib,"*","ebin"]) || Lib <- ExtraLibs],
    ok = systools:make_relup(Rel,[UpFrom],[DownTo],
				   [{path,RelupPath},
				    {outdir,RelDir}]),
    
    Rel.


create_fake_release(Dir,RelName,RelVsn,AppDirs) ->
    %% Create .rel files
    RelDir = filename:join(Dir,"rel_" ++ RelVsn),
    Rel = filename:join([RelDir,"rel_" ++ RelVsn]),
    ok = filelib:ensure_dir(Rel),
    ErtsVsn = erlang:system_info(version),

    {Apps,Paths} = 
	lists:foldl(fun({App,Vsn,Lib},{As,Ps}) ->
			    {[{App,Vsn}|As],
			     lists:umerge([filename:join([Lib,"*",ebin])],Ps)}
		    end,
		    {[],[]},
		    AppDirs),

    create_rel_file(Rel++".rel",RelName,RelVsn,ErtsVsn,Apps),
    
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
