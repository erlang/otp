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

-module(installer).

%%-compile(export_all).
-export([install_1/2]).
-export([install_2/1]).
-export([install_3/2]).
-export([install_3a/1]).
-export([install_4/1]).
-export([install_5/1]).
-export([install_5a/1]).
-export([install_6/1]).
-export([install_7/1]).
-export([install_8/1]).
-export([install_9/1]).
-export([install_10/1]).
-export([install_11/1]).
-export([client1_1/4]).
-export([client2/3]).
-export([stop/1]).
-export([unpack_p1h/2]).
-export([permanent_p1h/1]).
-export([reg_proc/1]).
-export([registered_loop/1]).

-define(print(List), {rh_print, TestNode} ! {print, {?MODULE, ?LINE}, List}).
-define(print_line(Line,List), {rh_print, TestNode} ! {print, {?MODULE, Line}, List}).
-define(fail(Term), exit({?MODULE, ?LINE, Term})).
-define(fail_line(Line,Term), exit({?MODULE, Line, Term})).

-define(check_release(Vsn,Status,Apps),
	check_release(TestNode,node(),Vsn,Status,Apps,?LINE)).
-define(check_release_client(Node,Vsn,Status,Apps),
	check_release(TestNode,Node,Vsn,Status,Apps,?LINE)).

-define(check_running_app(App,Vsn),
	check_running_app(TestNode,node(),App,Vsn,?LINE)).
-define(check_running_app_client(Node,App,Vsn),
	check_running_app(TestNode,Node,App,Vsn,?LINE)).


install_1(TestNode,PrivDir) ->
    ?print([TestNode]),
    ?print(["install_1 start"]),

    % Unpack and install P1H
    {ok, "P1H"} = unpack_release(PrivDir,"rel1"),
    ?print(["unpack_release P1H ok"]),
    ?check_release("P1H",unpacked,["a-1.0"]),
    {ok,"P1G",[new_appl]} = release_handler:install_release("P1H"),
    ?print(["install_release P1H ok"]),
    ?check_release("P1H",current,["a-1.0"]),
    ?check_running_app(a,"1.0"),
    X = a:a(),
    ?print(["X", X]),
    {key2, val2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),
    ?print(["install_1 end OK"]),
    ok.
    % release_handler_SUITE will reboot this node now!

install_2(TestNode) ->
    ?print(["install_2 start"]),

    % Check that P1H is still unpacked, install it and make_permanent
    ?check_release("P1H",unpacked,["a-1.0"]),
    ?print(["install_2 P1H unpacked"]),
    {ok,"P1G",[new_appl]} = release_handler:install_release("P1H"),
    ?print(["install_2 install_release ok"]),
    ?check_release("P1H",current,["a-1.0"]),
    ?check_running_app(a,"1.0"),
    ok = release_handler:make_permanent("P1H").
    % release_handler_SUITE will reboot this node now!

install_3(TestNode,PrivDir) ->
    ?print(["install_3 start"]),

    % Check that P1H is permanent
    ?check_release("P1H",permanent,["a-1.0"]),
    X = a:a(),
    {key2, val2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),

    % Unpack and install P1I
    {ok, "P1I"} = unpack_release(PrivDir,"rel2"),
    ?print(["install_3 unpack_release P1I ok"]),
    ?check_release("P1I",unpacked,["a-1.1"]),
    {ok,"P1H",[{extra, gott}]} = release_handler:check_install_release("P1I"),
    {error,_} = release_handler:check_install_release("P1J"),
    {ok,"P1H",[{extra, gott}]} = release_handler:install_release("P1I"),
    ?print(["install_3 install_release P1I ok"]),
    ?check_release("P1I",current,["a-1.1"]),
    ?check_running_app(a,"1.1"),
    X2 = a:a(),
    {key2, newval2} = lists:keyfind(key2, 1, X2),
    {key1, val1} = lists:keyfind(key1, 1, X2),
    {ok, bval} = a:b(),

    % Unpack and install P2A
    {ok, "P2A"} = unpack_release(PrivDir,"rel3"),
    ?print(["install_3 unpack_release P2A ok"]),
    ?check_release("P2A",unpacked,["a-1.1"]),
    {ok, "P1I", [new_emu]} = release_handler:check_install_release("P2A"),
    ?print(["install_3 check_install_release P2A ok"]),
    ok = release_handler:make_permanent("P1I"),
    ?print(["install_3 make_permanent P1I ok"]),
    ?check_release("P1I",permanent,["a-1.1"]),
    ok.

install_3a(TestNode) ->
    {ok, "P1I", [new_emu]} = release_handler:install_release("P2A"),
    %% Node is rebooted by the release_handler:install_release
    %% (init:reboot) because P2A includes a new erts vsn and the relup
    %% file contains a 'restart_new_emulator' instruction.
    ?print(["install_3 P2A installed"]),
    ok.



install_4(TestNode) ->
    ?print(["install_4 start"]),

    % Check that P2A is in use.
    ?check_release("P2A",current,["a-1.1"]),
    ?check_running_app(a,"1.1"),
    X = a:a(),
    {key2, newval2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),
    {ok, bval} = a:b(),
    ok.
    % release_handler_SUITE will reboot this node now!

install_5(TestNode) ->
    ?print(["install_5 start"]),

    % Check that P1I is used
    {ok, "P1I", [new_emu]} = release_handler:check_install_release("P2A"),
    ok.

install_5a(TestNode) ->
    % Install P2A again
    {ok, "P1I", [new_emu]} = release_handler:install_release("P2A"),
    %% Node is rebooted by the release_handler:install_release
    %% (init:reboot) because P2A includes a new erts vsn and the relup
    %% file contains a 'restart_new_emulator' instruction.
    ?print(["install_5 P2A installed"]),
    ok.

install_6(TestNode) ->
    ?print(["install_6 start"]),

    % Check that P2A is used
    ?check_release("P2A",current,["a-1.1"]),
    ?check_running_app(a,"1.1"),
    X = a:a(),
    {key2, newval2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),
    {ok, bval} = a:b(),
    ok = release_handler:make_permanent("P2A").
    % release_handler_SUITE will reboot this node now!


install_7(TestNode) ->
    ?print(["install_7 start"]),

    % Check that P2A is used
    ?check_release("P2A",permanent,["a-1.1"]),

    % Install old P1H
    ok = release_handler:reboot_old_release("P1H"),
    ok.

install_8(TestNode) ->
    ?print(["install_8 start"]),

    % Check that P1H is permanent
    ?check_release("P1H",permanent,["a-1.0"]),
    X = a:a(),
    {key2, val2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),

    %% Remove P1I and P2A and check that a-1.1 and erts-<latest> are removed
    ok = release_handler:remove_release("P2A"),
    ok = release_handler:remove_release("P1I"),
    {ok, Libs} = file:list_dir(code:lib_dir()),
    {_,_,StdlibVsn} = lists:keyfind(stdlib,1,application:which_applications()),
    true = lists:member("stdlib-"++StdlibVsn, Libs),
    true = lists:member("a-1.0", Libs),
    false = lists:member("a-1.1", Libs),
    {ok, Dirs} = file:list_dir(code:root_dir()),
    ["erts-4.4"] = lists:filter(fun(Dir) -> lists:prefix("erts-",Dir) end, Dirs),
    ok.
    % release_handler_SUITE will reboot this node now!

install_9(TestNode) ->
    ?print(["install_9 start"]),

    % Check that P1H is permanent
    ?check_release("P1H",permanent,["a-1.0"]),
    X = a:a(),
    {key2, val2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),

    % Install old P1G
    ok = release_handler:reboot_old_release("P1G"),
    ok.

install_10(TestNode) ->
    ?print(["install_10 start"]),

    % Check that P1G is permanent
    ?check_release("P1G",permanent,[]),
    ?check_release("P1H",old,["a-1.0"]),

    %% Remove P1H and check that both versions of application a is removed
    ok = release_handler:remove_release("P1H"),
    {ok, Libs} = file:list_dir(code:lib_dir()),
    {_,_,StdlibVsn} = lists:keyfind(stdlib,1,application:which_applications()),
    true = lists:member("stdlib-"++StdlibVsn, Libs),
    false = lists:member("a-1.0", Libs),
    false = lists:member("a-1.1", Libs),
    ok.
    % release_handler_SUITE will reboot this node now!

install_11(TestNode) ->
    ?print(["install_11 start"]),

    % Check that P1G is permanent
    ?check_release("P1G",permanent,[]),
    ok.



%%-----------------------------------------------------------------
%% This test starts a client node which uses this node as master
%% for the release_handler.
%% The client node runs all tests as in installer/1 test case.
%% Thus, the client node will be rebooted several times.
%% The to_erl /tmp/NODENAME@HOSTNAME/ command can be used to connect
%% to the client node.
%% run_erl logs for the client can be found in the directory:
%%   code:root_dir() ++ "/clients/type1/NODENAME@HOSTNAME/log
%%-----------------------------------------------------------------


client1_1(TestNode,PrivDir,MasterDir,ClientSname) ->
    TestHost = test_host(),
    ?print(["client1_1 start"]),

    {ok,IP} = inet:getaddr(TestHost,inet),
    erl_boot_server:start([IP]),

    ok = net_kernel:monitor_nodes(true),
    Node = start_client(TestNode,client1,ClientSname),
    trace_disallowed_calls(Node),

    %% Check env var for SASL on client node
    SaslEnv = rpc:call(Node, application, get_all_env, [sasl]),
    ?print([{client1_1,sasl_env},SaslEnv]),
    {_,CliDir} = lists:keyfind(client_directory,1,SaslEnv),
    {_,[Master]} = lists:keyfind(masters,1,SaslEnv),
    {_,StartCli} = lists:keyfind(start_prg,1,SaslEnv),
    NodeStr = atom_to_list(Node),
    [NodeStr,"type1","clients"|_] = lists:reverse(filename:split(CliDir)),
    true = (Master =:= node()),
    case os:type() of
	{unix,_} ->
	    true = (StartCli =:= filename:join([CliDir,"bin","start"]));
	_ ->
	    ok
    end,

    %% Unpack P1H on master
    {ok, "P1H"} = unpack_release(PrivDir,"rel1"),

    %% Unpack and install P1H on client
    Root = code:root_dir(),
    P1HDir = filename:join([Root, "releases", "P1H"]),

    %% The AppDirs argument (last arg to set_unpacked) below is really
    %% not necessary, it could just be [] since the path is the same
    %% as default. But it is given here in order to force hitting the
    %% release_handler:check_path function so it can be checked that
    %% it does not use file:read_file_info on the client node, see
    %% trace_disallowed_calls/1 and check_disallowed_calls/0 below.
    %% (OTP-9142)
    {ok, "P1H"} = rpc:call(Node, release_handler, set_unpacked,
			   [filename:join(P1HDir, "rel1.rel"),
			    [{a,"1.0",filename:join(MasterDir,lib)}]]),
    
    ?check_release_client(Node,"P1H",unpacked,["a-1.0"]),
    
    ok = rpc:call(Node, release_handler, install_file,
		  ["P1H", filename:join(P1HDir, "start.boot")]),
    ok = rpc:call(Node, release_handler, install_file,
		  ["P1H", filename:join(P1HDir, "sys.config")]),
    ok = rpc:call(Node, release_handler, install_file,
		  ["P1H", filename:join(P1HDir, "relup")]),
    ?print([{release_handler_state, Node}, 
	    rpc:call(Node, sys, get_status, [release_handler])]),

    {ok,"P1G",[new_appl]} = 
	rpc:call(Node, release_handler, check_install_release, ["P1H"]),
    
    {ok,"P1G",[new_appl]} = 
	rpc:call(Node, release_handler, install_release, ["P1H"]),

    Apps = rpc:call(Node, application, which_applications, []),
    {a,"A  CXC 138 11","1.0"} = lists:keyfind(a, 1, Apps),
    X = rpc:call(Node, a, a, []),
    {key2, val2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),

    check_disallowed_calls(),
    reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_2(TestNode,PrivDir,Node).

client1_2(TestNode,PrivDir,Node) ->
    ?print(["client1_2 start"]),

    %% Check that P1H is still unpacked, install it and make_permanent
    ?check_release_client(Node,"P1H",unpacked,["a-1.0"]),

    {ok,"P1G",[new_appl]} = 
	rpc:call(Node, release_handler, install_release, ["P1H"]),
    ?check_release_client(Node,"P1H",current,["a-1.0"]),
    ?check_running_app_client(Node,a,"1.0"),

    ok = rpc:call(Node, release_handler, make_permanent, ["P1H"]),
    ?check_release_client(Node,"P1H",permanent,["a-1.0"]),

    check_disallowed_calls(),
    reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_3(TestNode,PrivDir,Node).

client1_3(TestNode,PrivDir,Node) ->
    ?print(["client1_3 start"]),

    %% Check that P1H is permanent
    ?check_release_client(Node,"P1H",permanent,["a-1.0"]),
    X = rpc:call(Node, a, a, []),
    {key2, val2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),

    %% Unpack P1I on master
    {ok, "P1I"} = unpack_release(PrivDir,"rel2"),

    MasterRoot = code:root_dir(),

    %% Unpack and install P1I on client
    P1IDir = filename:join([MasterRoot, "releases", "P1I"]),
    {ok, "P1I"} = rpc:call(Node, release_handler, set_unpacked,
			   [filename:join(P1IDir, "rel2.rel"),[]]),

    ?check_release_client(Node,"P1I",unpacked,["a-1.1"]),

    ok = rpc:call(Node, release_handler, install_file,
                  ["P1I", filename:join(P1IDir, "start.boot")]),
    ok = rpc:call(Node, release_handler, install_file,
                  ["P1I", filename:join(P1IDir, "sys.config")]),
    ok = rpc:call(Node, release_handler, install_file,
                  ["P1I", filename:join(P1IDir, "relup")]),

    {ok,"P1H",[{extra, gott}]} =
        rpc:call(Node, release_handler, check_install_release, ["P1I"]),
    {error,_} = rpc:call(Node, release_handler, check_install_release, ["P1J"]),
    {ok,"P1H",[{extra, gott}]} =
        rpc:call(Node, release_handler, install_release, ["P1I"]),

    ?check_running_app_client(Node,a,"1.1"),
    X2 = rpc:call(Node, a, a, []),
    {key2, newval2} = lists:keyfind(key2, 1, X2),
    {key1, val1} = lists:keyfind(key1, 1, X2),
    {ok, bval} = rpc:call(Node, a, b, []),

    %% Unpack P2A on master
    {ok, "P2A"} = unpack_release(PrivDir,"rel3"),

    %% Unpack and install P2A on client
    P2ADir = filename:join([MasterRoot, "releases", "P2A"]),
    {ok, "P2A"} =
        rpc:call(Node, release_handler, set_unpacked,
                 [filename:join(P2ADir, "rel3.rel"),[]]),

    ok = rpc:call(Node, release_handler, install_file,
                  ["P2A", filename:join(P2ADir, "start.boot")]),
    ok = rpc:call(Node, release_handler, install_file,
                  ["P2A", filename:join(P2ADir, "sys.config")]),
    ok = rpc:call(Node, release_handler, install_file,
                  ["P2A", filename:join(P2ADir, "relup")]),

    {ok, "P1I", [new_emu]} = 
	rpc:call(Node, release_handler, check_install_release, ["P2A"]),
    ok = rpc:call(Node, release_handler, make_permanent, ["P1I"]),
    ?check_release_client(Node,"P1I",permanent,["a-1.1"]),

    %% since the install_release below reboot the node...
    check_disallowed_calls(),
    cover_client(TestNode,Node,stop_cover), 

    {ok, "P1I", [new_emu]} = 
	rpc:call(Node, release_handler, install_release, ["P2A"]),
    %% Reboots the client !

    check_reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_4(TestNode,Node).

client1_4(TestNode,Node) ->
    ?print(["client1_4 start"]),

    %% Check that P2A is in use.
    ?check_release_client(Node,"P2A",current,["a-1.1"]),
    ?check_running_app_client(Node,a,"1.1"),
    X = rpc:call(Node, a, a, []),
    {key2, newval2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),
    {ok, bval} = rpc:call(Node, a, b, []),

    %% Reboot from P1I
    check_disallowed_calls(),
    reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_5(TestNode,Node).

client1_5(TestNode,Node) ->
    ?print(["client1_5 start"]),

    %% Check that P1I is used
    {ok, "P1I", [new_emu]} = 
	rpc:call(Node, release_handler, check_install_release, ["P2A"]),

    %% since the install_release below will reboot the node...
    check_disallowed_calls(),
    cover_client(TestNode,Node,stop_cover),

    %% Install P2A again
    {ok, "P1I", [new_emu]} = 
	rpc:call(Node, release_handler, install_release, ["P2A"]),

    %% We are rebooted again.
    check_reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_6(TestNode,Node).

client1_6(TestNode,Node) ->
    ?print(["client1_6 start"]),

    %% Check that P2A is used
    ?check_release_client(Node,"P2A",current,["a-1.1"]),
    ?check_running_app_client(Node,a,"1.1"),
    X = rpc:call(Node, a, a, []),
    {key2, newval2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),
    {ok, bval} = rpc:call(Node, a, b, []),

    %% Make P2A permanent
    ok = rpc:call(Node, release_handler, make_permanent, ["P2A"]),

    %% Reboot from P2A
    check_disallowed_calls(),
    reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_7(TestNode,Node).

client1_7(TestNode,Node) ->
    ?print(["client1_7 start"]),

    %% Check that P2A is used
    ?check_release_client(Node,"P2A",permanent,["a-1.1"]),

    %% since the reboot_old_release below will reboot the node
    check_disallowed_calls(),
    cover_client(TestNode,Node,stop_cover),

    %% Install old P1H
    rpc:call(Node, release_handler, reboot_old_release, ["P1H"]),
    %% We are rebooted.
    check_reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_8(TestNode,Node).

client1_8(TestNode,Node) ->
    ?print(["client1_8 start"]),

    %% Check that P1H is permanent
    ?check_release_client(Node,"P1H",permanent,["a-1.0"]),
    X = rpc:call(Node, a, a, []),
    {key2, val2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),

    %% Remove P1I and P2I from client
    ok = rpc:call(Node, release_handler, set_removed, ["P2A"]),
    ok = rpc:call(Node, release_handler, set_removed, ["P1I"]),

    check_disallowed_calls(),
    reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_9(TestNode,Node).

client1_9(TestNode,Node) ->
    ?print(["client1_9 start"]),

    %% Check that P2A and P1I does not exists and that PiH is permanent.
    Rels = rpc:call(Node, release_handler, which_releases, []),
    false = lists:keysearch("P2A", 2, Rels),
    false = lists:keysearch("P1I", 2, Rels),
    ?check_release_client(Node,"P1H",permanent,["a-1.0"]),
    X = rpc:call(Node, a, a, []),
    {key2, val2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),

    %% since the reboot_old_release below will reboot the node
    check_disallowed_calls(),
    cover_client(TestNode,Node,stop_cover),

    %% Install old P1G
    rpc:call(Node, release_handler, reboot_old_release, ["P1G"]),
    %% We are rebooted.
    check_reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_10(TestNode,Node).

client1_10(TestNode,Node) ->
    ?print(["client1_10 start"]),

    %% Check that P1G is permanent
    ?check_release_client(Node,"P1G",permanent,[]),
    ?check_release_client(Node,"P1H",old,["a-1.0"]),
    {error,client_node} = rpc:call(Node,release_handler,remove_release,["P1H"]),
    ok = rpc:call(Node, release_handler, set_removed, ["P1H"]),

    check_disallowed_calls(),
    reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_11(TestNode,Node).

client1_11(TestNode,Node) ->
    ?print(["client1_11 start"]),

    %% Check that P1G is permanent
    ?check_release_client(Node,"P1G",permanent,[]),

    check_disallowed_calls(),
    stop_client(TestNode,Node),  %% TEST IS OK !!
    net_kernel:monitor_nodes(false),

    ok = release_handler:remove_release("P2A"),
    ok = release_handler:remove_release("P1I"),
    ok = release_handler:remove_release("P1H"),
    ok.

%% Start tracing of the file module on the client node. This module
%% shall never be called, since
%% 1) the node is a client from the release_handler's point of view,
%%    so all file access should be done via rpc calls to the master
%% 2) it is started with erl_prim_loader loader set to 'inet' so all
%%    code loading should be done via the inet to the master
%% (OTP-9142)
%% This function is called each time the client node is started and to
%% check if a call has been made, call check_disallowed_node/0
trace_disallowed_calls(Node) ->
    MasterProc = self(),
    rpc:call(Node,dbg,tracer,[process,{fun(T,_) -> MasterProc ! T end,[]}]),
    rpc:call(Node,dbg,p,[all,call]),
    rpc:call(Node,dbg,tp,[file,[{'_',[],[{message,{caller}}]}]]).

check_disallowed_calls() ->
    receive
	Trace when element(1,Trace)==trace ->
	    exit({disallowed_function_call,Trace})
    after 0 ->
	    ok
    end.

start_client(TestNode,Client,Sname) ->
    Node = list_to_atom(lists:concat([Sname,"@",test_host()])),
    case os:type() of
	{unix,_} -> start_client_unix(TestNode,Sname,Node);
	{win32,_} -> start_client_win32(TestNode,Client,Sname)
    end,
    receive
        {nodeup, Node} ->
            wait_started(TestNode,Node)
    after 30000 ->
	    ?print([{start_client,failed,Node},net_adm:ping(Node)]),
            ?fail({"can not start", Node})
    end.

start_client_unix(TestNode,Sname,Node) ->
    Start = filename:join(["clients", "type1", Node, "bin", "start"]),
    Cmd = lists:concat(["env NODENAME=",Sname," ",
			filename:join(code:root_dir(), Start)]),
    ?print([{start_client,Sname},Cmd]),
    Res = os:cmd(Cmd),
    ?print([{start_client,result},Res]).

start_client_win32(TestNode,Client,ClientSname) ->
    Name = atom_to_list(ClientSname) ++ "_P1G",
    RootDir = code:root_dir(),
    ErtsBinDir = filename:join(RootDir,"erts-4.4/bin"),

    {ClientArgs,RelClientDir} = rh_test_lib:get_client_args(Client,ClientSname,
							    RootDir),
    StartErlArgs = rh_test_lib:get_start_erl_args(RootDir,RelClientDir,
						  ClientArgs),
    ServiceArgs = rh_test_lib:get_service_args(RootDir, RelClientDir,
					       ClientSname, StartErlArgs),

    ?print([{start_client,ClientSname},ServiceArgs]),
    Erlsrv = filename:nativename(filename:join(ErtsBinDir,"erlsrv")),
    rh_test_lib:erlsrv(Erlsrv,stop,Name),
    rh_test_lib:erlsrv(Erlsrv,remove,Name),
    ok = rh_test_lib:erlsrv(Erlsrv,add,Name,ServiceArgs),
    ok = rh_test_lib:erlsrv(Erlsrv,start,Name),
    ?print([{start_client,result},ok]),
    ok.

reboot(TestNode,Node) ->
    cover_client(TestNode,Node,stop_cover),
    rpc:call(Node, init, reboot, []),
    check_reboot(TestNode,Node).

%% This way of checking that the node is rebooted will only work if
%% the nodes are automatically re-connected after the reboot. This
%% happens for master/client (when sasl is started on the client).
check_reboot(TestNode,Node) ->
    receive
        {nodedown, Node} ->
            receive
                {nodeup, Node} -> wait_started(TestNode,Node)
            after 30000 ->
		    ?fail({Node, "not rebooted",net_adm:ping(Node)})
            end
    after 30000 ->
            ?fail({Node, "not closing down",net_adm:ping(Node)})
    end.

stop_client(TestNode,Node) ->
    cover_client(TestNode,Node,stop_cover),
    rpc:call(Node, init, stop, []),
    receive
        {nodedown, Node} -> ok
    after 30000 ->
            ?fail({Node, "not stopping"})
    end.

wait_started(TestNode,Node) ->
    case rpc:call(Node, init, get_status, []) of
        {started, _} ->
	    cover_client(TestNode,Node,start_cover),
            Node;
        _ ->
            timer:sleep(1000),
            wait_started(TestNode,Node)
    end.

cover_client(TestNode,Node,Func) ->
    R = rpc:call(TestNode,release_handler_SUITE,Func,[Node]),
    ?print([{Func,Node,R}]).


%%-----------------------------------------------------------------
%% This test starts a client node which uses this node as master
%% for the release_handler.
%% The client node has the name cli2@HOSTNAME.
%% The client node is not allowed to do ANY release updates
%% as it also have another (non-existing) master node.
%%
%% The to_erl /tmp/cli2@HOSTNAME/ command can be used to connect
%% to the client node.
%% run_erl logs for the client can be found in the directory:
%%   code:root_dir() ++ "/clients/type1/cli2@HOSTNAME/log
%%-----------------------------------------------------------------
client2(TestNode,PrivDir,ClientSname) ->
    TestHost = test_host(),
    ?print(["client2 start"]),
    
    %% Clean up if previous test case failed
    release_handler:remove_release("P1H"), 

    ok = net_kernel:monitor_nodes(true),
    Node = start_client(TestNode,client2,ClientSname),

    %% Check env var for SASL on client node
    SaslEnv = rpc:call(Node, application, get_all_env, [sasl]),
    ?print([{client1_1,sasl_env},SaslEnv]),
    {_,CliDir} = lists:keyfind(client_directory,1,SaslEnv),
    {_,[Master,Master2]} = lists:keyfind(masters,1,SaslEnv),
    {_,StartCli} = lists:keyfind(start_prg,1,SaslEnv),
    NodeStr = atom_to_list(Node),
    [NodeStr,"type1","clients"|_] = lists:reverse(filename:split(CliDir)),
    true = (Master =:= node()),
    true = (Master2 =:= list_to_atom("master2@"++TestHost)),
    case os:type() of
	{unix,_} ->
	    true = (StartCli =:= filename:join([CliDir,"bin","start"]));
	_ ->
	    ok
    end,

    {ok, "P1H"} = unpack_release(PrivDir,"rel1"),

    Root = code:root_dir(),
    {error,{bad_masters,[Master2]}} =
	rpc:call(Node, release_handler, set_unpacked,
		 [filename:join([Root, "releases", "P1H", "rel1.rel"]),[]]),

    {error,{no_such_release,"P1H"}} =
	rpc:call(Node, release_handler, check_install_release, ["P1H"]),

    stop_client(TestNode,Node),  %% TEST IS OK !!
    net_kernel:monitor_nodes(false),

    release_handler:remove_release("P1H"),
    ok.


stop(Now) ->
    %% The timestamp is only used for debugging. It is printed by
    %% release_handler_SUITE also.
    R = init:stop(),
    erlang:display({init_stop,Now,R}),
    R.

unpack_p1h(TestNode,PrivDir) ->
    {ok, "P1H"} = unpack_release(PrivDir,"rel1"), 
    ?check_release("P1H",unpacked,["a-1.0"]),
    ok.

permanent_p1h(TestNode) ->
    ?check_release("P1H",unpacked,["a-1.0"]),
    {ok,"P1G",[new_appl]} = release_handler:install_release("P1H"),
    ?check_release("P1H",current,["a-1.0"]),
    ok = release_handler:make_permanent("P1H"),
    ?check_release("P1H",permanent,["a-1.0"]),
    ok.


reg_proc(Name) ->
    catch unregister(Name),
    Pid = spawn_link(?MODULE, registered_loop, [Name]),
    global:register_name(Name, Pid),
    ok.

registered_loop(_Name) ->
    receive
        kill ->
            exit(killed)
    end.

check_release(TestNode,Node,Vsn,Status,Apps,Line) ->
    case rpc:call(Node,release_handler,which_releases,[]) of
	{badrpc,_}=Error ->
	    ?fail_line(Line,{check_release,Node,Vsn,Status,Error});
	Rels ->
	    ?print_line(Line,["check_release:", Rels]),
	    {"SASL-test", Vsn, Libs, Status} = lists:keyfind(Vsn, 2, Rels),
	    true = lists:all(fun(App) -> lists:member(App,Libs) end,Apps),
	    ok
    end.

check_running_app(TestNode,Node,App,Vsn,Line) ->
    case rpc:call(Node,application,which_applications,[]) of
	{badrpc,_}=Error ->
	    ?fail_line(Line,{check_running_app,Node,App,Vsn,Error});
	Apps ->
	    ?print_line(Line,["check_running_app:", Apps]),
	    {App, _, Vsn} = lists:keyfind(a, 1, Apps),
	    ok
    end.

test_host() ->
    {ok,Host} = inet:gethostname(),
    Host.

unpack_release(PrivDir,Rel) ->
    copy(filename:join([PrivDir,Rel,Rel++".tar.gz"]),
	 filename:join(code:root_dir(),releases)),
    release_handler:unpack_release(Rel).

copy(Src, DestDir) ->
    Dest = filename:join(DestDir, filename:basename(Src)),
    {ok,_} = file:copy(Src, Dest),
    ok.

