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

-module(installer).

-include("test_lib.hrl").

%%-compile(export_all).
-export([install_1/2]).
-export([install_2/1]).
-export([install_3/2]).
-export([install_6a/1]).
-export([install_4/1]).
-export([install_5/1]).
-export([install_5a/1]).
-export([install_6/1]).
-export([install_7/1]).
-export([install_7a/1]).
-export([install_8/1]).
-export([install_8a/1]).
-export([install_9/1]).
-export([install_10/1]).
-export([install_11/1]).
-export([install_12/1]).
-export([install_13/1]).
-export([install_14/1]).
-export([upgrade_restart_1/2]).
-export([upgrade_restart_1a/1]).
-export([upgrade_restart_2/1]).
-export([upgrade_restart_2a/1]).
-export([upgrade_restart_2b/1]).
-export([upgrade_restart_3/1]).
-export([client1_1/4]).
-export([client2/3]).
-export([stop/1]).
-export([unpack_p1h/2]).
-export([permanent_p1h/1]).
-export([reg_proc/1]).
-export([registered_loop/1]).

-define(print(List),
	io:format(user,"(~w:~w) ~tp~n",[?MODULE,?LINE,List]),
	{rh_print, TestNode} ! {print, {?MODULE, ?LINE}, List}).
-define(print_line(Line,List),
	io:format(user,"(~w:~w) ~tp~n",[?MODULE,Line,List]),
	{rh_print, TestNode} ! {print, {?MODULE, Line}, List}).
-define(fail(Term), exit({?MODULE, ?LINE, Term})).
-define(fail_line(Line,Term), exit({?MODULE, Line, Term})).

-define(check_release_states(States),
	check_release_states(TestNode,node(),States,?LINE)).
-define(check_release_states_client(Node,States),
	check_release_states(TestNode,Node,States,?LINE)).

-define(check_release_lib(Vsn,Apps),
	check_release_lib(TestNode,node(),Vsn,Apps,?LINE)).
-define(check_release_lib_client(Node,Vsn,Apps),
	check_release_lib(TestNode,Node,Vsn,Apps,?LINE)).

-define(check_running_app(App,Vsn),
	check_running_app(TestNode,node(),App,Vsn,?LINE)).
-define(check_running_app_client(Node,App,Vsn),
	check_running_app(TestNode,Node,App,Vsn,?LINE)).

-define(check_disallowed_calls,check_disallowed_calls(TestNode,?LINE)).


install_1(TestNode,PrivDir) ->
    ?print([TestNode]),
    ?print(["install_1 start"]),
    ?check_release_states([permanent]),

    % Unpack and install P1H
    {ok, "P1H"} = unpack_release(PrivDir,"rel1"),
    ?check_release_states([permanent,unpacked]),
    ?check_release_lib("P1H",["a-1.0"]),
    {ok,"P1G",[new_appl]} = release_handler:install_release("P1H"),
    ?check_release_states([permanent,current]),
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
    ?check_release_states([permanent,unpacked]),
    {ok,"P1G",[new_appl]} = release_handler:install_release("P1H"),
    ?print(["install_2 install_release ok"]),
    ?check_release_states([permanent,current]),
    ?check_running_app(a,"1.0"),
    ok = release_handler:make_permanent("P1H"),
    ?print(["install_2 make permanent P1H ok"]),
    ?check_release_states([old,permanent]),
    ?check_running_app(a,"1.0"),
    ok.
    % release_handler_SUITE will reboot this node now!

install_3(TestNode,PrivDir) ->
    ?print(["install_3 start"]),

    % Check that P1H is permanent
    ?check_release_states([old,permanent]),
    ?check_running_app(a,"1.0"),
    X = a:a(),
    {key2, val2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),

    % Unpack and install P1I
    {ok, "P1I"} = unpack_release(PrivDir,"rel2"),
    ?check_release_states([old,permanent,unpacked]),
    ?check_release_lib("P1I",["a-1.1"]),
    {ok,"P1H",[{extra, gott}]} = release_handler:check_install_release("P1I"),
    ?print(["install_3 check_install_release P1I ok"]),
    {error,_} = release_handler:check_install_release("P1J"),
    ?print(["install_3 check_install_release P1J fails - ok"]),
    {ok,"P1H",[{extra, gott}]} = release_handler:install_release("P1I"),
    ?check_release_states([old,permanent,current]),
    ?check_running_app(a,"1.1"),
    X2 = a:a(),
    {key2, newval2} = lists:keyfind(key2, 1, X2),
    {key1, val1} = lists:keyfind(key1, 1, X2),
    {ok, bval} = a:b(),
    ?print(["install_3 env ok"]),

    % Unpack P2A
    {ok, "P2A"} = unpack_release(PrivDir,"rel3"),
    ?check_release_states([old,permanent,current,unpacked]),
    ?check_release_lib("P2A",["a-1.1"]),
    {ok, "P1I", [new_emu]} = release_handler:check_install_release("P2A"),
    ?print(["install_3 check_install_release P2A ok"]),
    ok.
    % release_handler_SUITE will reboot this node now!

install_4(TestNode) ->
    ?print(["install_4 start"]),

    %% Check that P1H is the one that is used
    ?check_release_states([old,permanent,unpacked,unpacked]),
    ?check_running_app(a,"1.0"),

    %% Install P2A
    {continue_after_restart, "P1H", [new_emu,new_appl]} =
	release_handler:install_release("P2A"),
    %% Node is rebooted by the release_handler:install_release
    %% (init:reboot) because P2A includes a new erts vsn and the relup
    %% file contains a 'restart_new_emulator' instruction.
    ?print(["install_4 P2A installed"]),
    ok.


install_5(TestNode) ->
    ?print(["install_5 start"]),

    %% Check that the upgrade was done via a temporary release due to
    %% new emulator version.
    {"SASL-test","__new_emulator__P1H"} = init:script_id(),

    %% Check that P2A is in use.
    ?check_release_states([old,permanent,unpacked,current]),
    ?check_running_app(a,"1.1"),
    X = a:a(),
    {key2, newval2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),
    {ok, bval} = a:b(),
    ?print(["install_5 check env ok"]),
    ok.

install_5a(TestNode) ->
    ?print(["install_5a start"]),

    %% Install P1I (this will be a downgrade)
    {ok, "P1I", [old_emu]} = release_handler:install_release("P1I"),
    %% Node is rebooted by the release_handler:install_release
    %% (init:reboot) because P2A includes a new erts vsn and the relup
    %% file contains a 'restart_new_emulator' instruction.
    ?print(["install_5a P1I installed"]),
    ok.

install_6(TestNode) ->
    ?print(["install_6 start"]),

    %% Check that P1I is used
    ?check_release_states([old,permanent,current,old]),
    ?check_running_app(a,"1.1"),

    %% Make P1I permanent
    ok = release_handler:make_permanent("P1I"),
    ?check_release_states([old,old,permanent,old]),
    ?check_running_app(a,"1.1"),
    ok.

install_6a(TestNode) ->
    %% Install P2A
    {continue_after_restart, "P1I", [new_emu]} =
	release_handler:install_release("P2A"),
    %% Node is rebooted by the release_handler:install_release
    %% (init:reboot) because P2A includes a new erts vsn and the relup
    %% file contains a 'restart_new_emulator' instruction.
    ?print(["install_6a P2A installed"]),
    ok.

install_7(TestNode) ->
    ?print(["install_7 start"]),

    %% Check that the upgrade was done via a temporary release due to
    %% new emulator version.
    {"SASL-test","__new_emulator__P1I"} = init:script_id(),

    % Check that P2A is in use.
    ?check_release_states([old,old,permanent,current]),
    ?check_running_app(a,"1.1"),
    X = a:a(),
    {key2, newval2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),
    {ok, bval} = a:b(),
    ?print(["install_7 check env ok"]),
    ok.

install_7a(TestNode) ->
    %% Install P1H (this will be a downgrade)
    {ok, "P1H", [old_emu,old_appl]} = release_handler:install_release("P1H"),
    %% Node is rebooted by the release_handler:install_release
    %% (init:reboot) because P2A includes a new erts vsn and the relup
    %% file contains a 'restart_new_emulator' instruction.
    ?print(["install_7a P1H installed"]),
    ok.

install_8(TestNode) ->
    ?print(["install_8 start"]),

    %% Check that P1H is used
    ?check_release_states([old,current,permanent,old]),
    ?check_running_app(a,"1.0"),
    {ok,"P1H",[new_emu,new_appl]} = release_handler:check_install_release("P2A"),
    ?print(["install_8 check_install_release P2A ok"]),

    %% Install P1I and check that it is permanent
    {ok,"P1H",[{extra, gott}]} = release_handler:install_release("P1I"),
    ?check_release_states([old,old,permanent,old]),
    ?check_running_app(a,"1.1"),
    ok.

install_8a(TestNode) ->
    % Install P2A again
    {continue_after_restart, "P1I", [new_emu]} =
	release_handler:install_release("P2A"),
    %% Node is rebooted by the release_handler:install_release
    %% (init:reboot) because P2A includes a new erts vsn and the relup
    %% file contains a 'restart_new_emulator' instruction.
    ?print(["install_8a P2A installed"]),
    ok.

install_9(TestNode) ->
    ?print(["install_9 start"]),

    %% Check that the upgrade was done via a temporary release due to
    %% new emulator version.
    {"SASL-test","__new_emulator__P1I"} = init:script_id(),

    % Check that P2A is used
    ?check_release_states([old,old,permanent,current]),
    ?check_running_app(a,"1.1"),
    X = a:a(),
    {key2, newval2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),
    {ok, bval} = a:b(),
    ?print(["install_9 check env ok"]),
    ok = release_handler:make_permanent("P2A"),
    ?check_release_states([old,old,old,permanent]),
    ?check_running_app(a,"1.1"),
    ok.
    % release_handler_SUITE will reboot this node now!


install_10(TestNode) ->
    ?print(["install_10 start"]),

    % Check that P2A is used
    ?check_release_states([old,old,old,permanent]),
    ?check_running_app(a,"1.1"),

    % Install old P1H
    ok = release_handler:reboot_old_release("P1H"),
    ?print(["install_10 reboot_old ok"]),
    ok.


install_11(TestNode) ->
    ?print(["install_11 start"]),

    % Check that P1H is permanent
    ?check_release_states([old,permanent,old,old]),
    ?check_running_app(a,"1.0"),
    X = a:a(),
    {key2, val2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),
    ?print(["install_11 check env ok"]),

    %% Remove P1I and P2A and check that a-1.1 and erts-<latest> are removed
    ok = release_handler:remove_release("P2A"),
    ?check_release_states([old,permanent,old]),
    ok = release_handler:remove_release("P1I"),
    ?check_release_states([old,permanent]),
    {ok, Libs} = file:list_dir(code:lib_dir()),
    {_,_,StdlibVsn} = lists:keyfind(stdlib,1,application:which_applications()),
    true = lists:member("stdlib-"++StdlibVsn, Libs),
    true = lists:member("a-1.0", Libs),
    false = lists:member("a-1.1", Libs),
    {ok, Dirs} = file:list_dir(code:root_dir()),
    ErtsDir = "erts-"++?ertsvsn,
    [ErtsDir] = lists:filter(fun(Dir) -> lists:prefix("erts-",Dir) end, Dirs),
    ?print(["install_11 file checks ok"]),
    ok.
    % release_handler_SUITE will reboot this node now!

install_12(TestNode) ->
    ?print(["install_12 start"]),

    % Check that P1H is permanent
    ?check_release_states([old,permanent]),
    ?check_running_app(a,"1.0"),
    X = a:a(),
    {key2, val2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),
    ?print(["install_12 check env ok"]),

    % Install old P1G
    ok = release_handler:reboot_old_release("P1G"),
    ?print(["install_12 reboot_old ok"]),
    ok.

install_13(TestNode) ->
    ?print(["install_13 start"]),

    % Check that P1G is permanent
    ?check_release_states([permanent,old]),
    false = lists:keysearch(a,1,application:loaded_applications()),
    ?print(["install_13 no a application found - ok"]),

    %% Remove P1H and check that both versions of application a is removed
    ok = release_handler:remove_release("P1H"),
    ?check_release_states([permanent]),
    {ok, Libs} = file:list_dir(code:lib_dir()),
    {_,_,StdlibVsn} = lists:keyfind(stdlib,1,application:which_applications()),
    true = lists:member("stdlib-"++StdlibVsn, Libs),
    false = lists:member("a-1.0", Libs),
    false = lists:member("a-1.1", Libs),
    ?print(["install_13 file checks ok"]),
    ok.
    % release_handler_SUITE will reboot this node now!

install_14(TestNode) ->
    ?print(["install_14 start"]),

    % Check that P1G is permanent
    ?check_release_states([permanent]),
    false = lists:keysearch(a,1,application:loaded_applications()),
    ?print(["install_13 no a application found - ok"]),
    ok.


%%%-----------------------------------------------------------------
%%% Ths test checks that an upgrade which both upgrades to a new
%%% emulator version, and had a restart_emulator option to
%%% systools:make_relup will be restarted twice on upgrade.
%%% (On downgrade it will happen only once.)
upgrade_restart_1(TestNode,PrivDir) ->
    ?print([TestNode]),
    ?print(["upgrade_restart_1 start"]),
    ?check_release_states([permanent]),

    {ok, "P2B"} = unpack_release(PrivDir,"rel4"),
    ?check_release_states([permanent,unpacked]),
    ?check_release_lib("P2B",["a-1.1"]),
    ok.

upgrade_restart_1a(TestNode) ->
    ?print(["upgrade_restart_1a start"]),

    {continue_after_restart,"P1G",[new_emu,add_appl]} =
	release_handler:install_release("P2B"),
    ?print(["upgrade_restart_1a P2B installed"]),
    ok.

upgrade_restart_2(TestNode) ->
    ?print(["upgrade_restart_2 start"]),

    %% Check that the node has been restarted once more after the tmp release
    case init:script_id() of
	{"SASL-test","P2B"} ->
	    upgrade_restart_2a(TestNode);
	{"SASL-test","__new_emulator__P1G"} ->
	    %% catched the node too early - give it another try
	    {wait,whereis(init)}
    end.

upgrade_restart_2a(TestNode) ->
    ?print(["upgrade_restart_2a start"]),

    %% This time we must be there, else something is definitely wrong
    {"SASL-test","P2B"} = init:script_id(),

    ?check_release_states([permanent,current]),
    ?check_running_app(a,"1.1"),

    ok = release_handler:make_permanent("P2B"),
    ?check_release_states([old,permanent]),

    ok.

upgrade_restart_2b(TestNode) ->
    ?print(["upgrade_restart_2b start"]),

    {ok,"P1G",[old_emu,rm_appl]} = release_handler:install_release("P1G"),
    ?print(["upgrade_restart_2b P1G installed"]),
    ok.

upgrade_restart_3(TestNode) ->
    ?print(["upgrade_restart_3 start"]),

    %% Ideally we should test that the node has only been restarted
    %% once... but that's not so easy. Let's just check that P1G is running.
    ?check_release_states([current,permanent]),
    false = lists:keysearch(a,1,application:loaded_applications()),
    ?print(["upgrade_restart_3 no a application found - ok"]),

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

    ?check_release_states_client(Node,[permanent]),

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
    %% trace_disallowed_calls/1 and check_disallowed_calls/2 below.
    %% (OTP-9142)
    {ok, "P1H"} = rpc:call(Node, release_handler, set_unpacked,
			   [filename:join(P1HDir, "rel1.rel"),
			    [{a,"1.0",filename:join(MasterDir,lib)}]]),
    
    ?check_release_states_client(Node,[permanent,unpacked]),
    ?check_release_lib_client(Node,"P1H",["a-1.0"]),
    
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

    ?check_release_states_client(Node,[permanent,current]),
    ?check_running_app_client(Node,a,"1.0"),

    Apps = rpc:call(Node, application, which_applications, []),
    {a,"A  CXC 138 11","1.0"} = lists:keyfind(a, 1, Apps),
    X = rpc:call(Node, a, a, []),
    {key2, val2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),

    ?check_disallowed_calls,
    reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_2(TestNode,PrivDir,Node).

client1_2(TestNode,PrivDir,Node) ->
    ?print(["client1_2 start"]),

    %% Check that P1H is still unpacked, install it and make_permanent
    ?check_release_states_client(Node,[permanent,unpacked]),

    {ok,"P1G",[new_appl]} = 
	rpc:call(Node, release_handler, install_release, ["P1H"]),
    ?check_release_states_client(Node,[permanent,current]),
    ?check_running_app_client(Node,a,"1.0"),

    ok = rpc:call(Node, release_handler, make_permanent, ["P1H"]),
    ?check_release_states_client(Node,[old,permanent]),

    ?check_disallowed_calls,
    reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_3(TestNode,PrivDir,Node).

client1_3(TestNode,PrivDir,Node) ->
    ?print(["client1_3 start"]),

    %% Check that P1H is permanent
    ?check_release_states_client(Node,[old,permanent]),
    ?check_running_app_client(Node,a,"1.0"),

    %% Unpack P1I on master
    {ok, "P1I"} = unpack_release(PrivDir,"rel2"),

    MasterRoot = code:root_dir(),

    %% Unpack and install P1I on client
    P1IDir = filename:join([MasterRoot, "releases", "P1I"]),
    {ok, "P1I"} = rpc:call(Node, release_handler, set_unpacked,
			   [filename:join(P1IDir, "rel2.rel"),[]]),

    ?check_release_states_client(Node,[old,permanent,unpacked]),
    ?check_release_lib_client(Node,"P1I",["a-1.1"]),

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

    ?check_release_states_client(Node,[old,permanent,current]),
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

    ?check_release_states_client(Node,[old,permanent,current,unpacked]),
    ?check_release_lib_client(Node,"P2A",["a-1.1"]),

    ok = rpc:call(Node, release_handler, install_file,
                  ["P2A", filename:join(P2ADir, "start.boot")]),
    ok = rpc:call(Node, release_handler, install_file,
                  ["P2A", filename:join(P2ADir, "sys.config")]),
    ok = rpc:call(Node, release_handler, install_file,
                  ["P2A", filename:join(P2ADir, "relup")]),

    {ok, "P1I", [new_emu]} = 
	rpc:call(Node, release_handler, check_install_release, ["P2A"]),

    %% Reboot from P1H
    ?check_disallowed_calls,
    reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_4(TestNode,Node).

client1_4(TestNode,Node) ->
    ?print(["client1_4 start"]),

    %% check that P1H is used
    ?check_release_states_client(Node,[old,permanent,unpacked,unpacked]),

    %% since the install_release below reboot the node...
    ?check_disallowed_calls,
    cover_client(TestNode,Node,stop_cover),

    {continue_after_restart, "P1H", [new_emu,new_appl]} =
	rpc:call(Node, release_handler, install_release, ["P2A"]),
    %% Reboots the client !

    check_reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_5(TestNode,Node).

client1_5(TestNode,Node) ->
    ?print(["client1_5 start"]),

    %% Check that P2A is in use.
    ?check_release_states_client(Node,[old,permanent,unpacked,current]),
    ?check_running_app_client(Node,a,"1.1"),
    X = rpc:call(Node, a, a, []),
    {key2, newval2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),
    {ok, bval} = rpc:call(Node, a, b, []),

    %% since the install_release below reboot the node...
    ?check_disallowed_calls,
    cover_client(TestNode,Node,stop_cover),

    {ok,"P1I",[old_emu]} =
        rpc:call(Node, release_handler, install_release, ["P1I"]),

    check_reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_6(TestNode,Node).

client1_6(TestNode,Node) ->
    ?print(["client1_6 start"]),

    ?check_release_states_client(Node,[old,permanent,current,old]),
    ?check_running_app_client(Node,a,"1.1"),

    ok = rpc:call(Node, release_handler, make_permanent, ["P1I"]),
    ?check_release_states_client(Node,[old,old,permanent,old]),

    %% since the install_release below reboot the node...
    ?check_disallowed_calls,
    cover_client(TestNode,Node,stop_cover), 

    {continue_after_restart, "P1I", [new_emu]} =
	rpc:call(Node, release_handler, install_release, ["P2A"]),
    %% Reboots the client !

    check_reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_7(TestNode,Node).

client1_7(TestNode,Node) ->
    ?print(["client1_7 start"]),

    %% Check that P2A is in use.
    ?check_release_states_client(Node,[old,old,permanent,current]),
    ?check_running_app_client(Node,a,"1.1"),
    X = rpc:call(Node, a, a, []),
    {key2, newval2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),
    {ok, bval} = rpc:call(Node, a, b, []),

    %% since the install_release below reboot the node...
    ?check_disallowed_calls,
    cover_client(TestNode,Node,stop_cover),

    {ok,"P1H",[old_emu,old_appl]} =
        rpc:call(Node, release_handler, install_release, ["P1H"]),

    check_reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_8(TestNode,Node).

client1_8(TestNode,Node) ->
    ?print(["client1_8 start"]),

    %% Check that P1H is used
    ?check_release_states_client(Node,[old,current,permanent,old]),
    ?check_running_app_client(Node,a,"1.0"),
    {ok, "P1H", [new_emu,new_appl]} =
	rpc:call(Node, release_handler, check_install_release, ["P2A"]),


    {ok,"P1H",[{extra, gott}]} =
        rpc:call(Node, release_handler, install_release, ["P1I"]),
    ?check_release_states_client(Node,[old,old,permanent,old]),
    ?check_running_app_client(Node,a,"1.1"),


    %% since the install_release below will reboot the node...
    ?check_disallowed_calls,
    cover_client(TestNode,Node,stop_cover),

    %% Install P2A again
    {continue_after_restart, "P1I", [new_emu]} =
	rpc:call(Node, release_handler, install_release, ["P2A"]),

    %% We are rebooted again.
    check_reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_9(TestNode,Node).

client1_9(TestNode,Node) ->
    ?print(["client1_9 start"]),

    %% Check that P2A is used
    ?check_release_states_client(Node,[old,old,permanent,current]),
    ?check_running_app_client(Node,a,"1.1"),
    X = rpc:call(Node, a, a, []),
    {key2, newval2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),
    {ok, bval} = rpc:call(Node, a, b, []),

    %% Make P2A permanent
    ok = rpc:call(Node, release_handler, make_permanent, ["P2A"]),
    ?check_release_states_client(Node,[old,old,old,permanent]),

    %% Reboot from P2A
    ?check_disallowed_calls,
    reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_10(TestNode,Node).

client1_10(TestNode,Node) ->
    ?print(["client1_10 start"]),

    %% Check that P2A is used
    ?check_release_states_client(Node,[old,old,old,permanent]),

    %% since the reboot_old_release below will reboot the node
    ?check_disallowed_calls,
    cover_client(TestNode,Node,stop_cover),

    %% Install old P1H
    rpc:call(Node, release_handler, reboot_old_release, ["P1H"]),
    %% We are rebooted.
    check_reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_11(TestNode,Node).

client1_11(TestNode,Node) ->
    ?print(["client1_11 start"]),

    %% Check that P1H is permanent
    ?check_release_states_client(Node,[old,permanent,old,old]),
    ?check_running_app_client(Node,a,"1.0"),
    X = rpc:call(Node, a, a, []),
    {key2, val2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),

    %% Remove P1I and P2A from client
    ok = rpc:call(Node, release_handler, set_removed, ["P2A"]),
    ?check_release_states_client(Node,[old,permanent,old]),
    ok = rpc:call(Node, release_handler, set_removed, ["P1I"]),
    ?check_release_states_client(Node,[old,permanent]),

    %% Check that P2A and P1I does not exists
    Rels = rpc:call(Node, release_handler, which_releases, []),
    false = lists:keysearch("P2A", 2, Rels),
    false = lists:keysearch("P1I", 2, Rels),
    X = rpc:call(Node, a, a, []),
    {key2, val2} = lists:keyfind(key2, 1, X),
    {key1, val1} = lists:keyfind(key1, 1, X),

    ?check_disallowed_calls,
    reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_12(TestNode,Node).

client1_12(TestNode,Node) ->
    ?print(["client1_12 start"]),

    ?check_release_states_client(Node,[old,permanent]),

    %% since the reboot_old_release below will reboot the node
    ?check_disallowed_calls,
    cover_client(TestNode,Node,stop_cover),

    %% Install old P1G
    rpc:call(Node, release_handler, reboot_old_release, ["P1G"]),
    %% We are rebooted.
    check_reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_13(TestNode,Node).

client1_13(TestNode,Node) ->
    ?print(["client1_13 start"]),

    %% Check that P1G is permanent
    ?check_release_states_client(Node,[permanent,old]),
    {error,client_node} = rpc:call(Node,release_handler,remove_release,["P1H"]),
    ok = rpc:call(Node, release_handler, set_removed, ["P1H"]),
    ?check_release_states_client(Node,[permanent]),

    ?check_disallowed_calls,
    reboot(TestNode,Node),
    trace_disallowed_calls(Node),

    client1_14(TestNode,Node).

client1_14(TestNode,Node) ->
    ?print(["client1_14 start"]),

    %% Check that P1G is permanent
    ?check_release_states_client(Node,[permanent]),

    ?check_disallowed_calls,
    stop_client(TestNode,Node),  %% TEST IS OK !!
    net_kernel:monitor_nodes(false),

    %% Remove releases from master
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
    rpc:call(Node,dbg,tp,[file,[{'_',[],[{message,{caller}}]}]]),
    %% File:native_name_encoding/0 is a BIF and OK to use
    rpc:call(Node,dbg,ctp,[file,native_name_encoding,0]).

check_disallowed_calls(TestNode,Line) ->
    receive
	Trace when element(1,Trace)==trace ->
	    ?print_line(Line,["Disallowed function called",Trace]),
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
    Cmd = filename:join(code:root_dir(), Start),
    ?print([{start_client,Sname},Cmd]),
    Res = rh_test_lib:cmd(Cmd,[],[{"NODENAME",atom_to_list(Sname)}]),
    ?print([{start_client,result},Res]).

start_client_win32(TestNode,Client,ClientSname) ->
    Name = atom_to_list(ClientSname) ++ "_P1G",
    RootDir = code:root_dir(),
    ErtsBinDir = filename:join([RootDir,"erts-"++?ertsvsn,"bin"]),

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

    %% Unpack P1H on master
    {ok, "P1H"} = unpack_release(PrivDir,"rel1"),

    %% Try to set P1H unpacked on client
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
    ?check_release_states([permanent,unpacked]),
    ?check_release_lib("P1H",["a-1.0"]),
    ok.

permanent_p1h(TestNode) ->
    ?check_release_states([permanent,unpacked]),
    ?check_release_lib("P1H",["a-1.0"]),
    {ok,"P1G",[new_appl]} = release_handler:install_release("P1H"),
    ?check_release_states([permanent,current]),
    ok = release_handler:make_permanent("P1H"),
    ?check_release_states([old,permanent]),
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

%% Checks that the list of states for all releases (sorted on vsn)
%% equals the input States
check_release_states(TestNode,Node,States,Line) ->
    case rpc:call(Node,release_handler,which_releases,[]) of
	{badrpc,_}=Error ->
	    ?fail_line(Line,{check_release_states,Node,States,Error});
	Rels ->
	    ?print_line(Line,["check_release_states:", Rels]),
	    States = [Status || {_,_,_,Status} <- lists:keysort(2,Rels)],
	    ok
    end.

%% Check that the given release (Vsn) sees the correct vsn of App.
check_release_lib(TestNode,Node,Vsn,Apps,Line) ->
    case rpc:call(Node,release_handler,which_releases,[]) of
	{badrpc,_}=Error ->
	    ?fail_line(Line,{check_release_lib,Node,Vsn,Apps,Error});
	Rels ->
	    ?print_line(Line,["check_release_lib:", Rels]),
	    {"SASL-test", Vsn, Libs, _Status} = lists:keyfind(Vsn, 2, Rels),
	    true = lists:all(fun(App) -> lists:member(App,Libs) end,Apps),
	    ok
    end.

%% Check that the given Vsn of App is executed
check_running_app(TestNode,Node,App,Vsn,Line) ->
    case rpc:call(Node,application,which_applications,[]) of
	{badrpc,_}=Error ->
	    ?fail_line(Line,{check_running_app,Node,App,Vsn,Error});
	Apps ->
	    ?print_line(Line,["check_running_app:", Apps]),
	    {App, _, Vsn} = lists:keyfind(App, 1, Apps),
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

