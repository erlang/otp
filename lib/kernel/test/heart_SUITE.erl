%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2022. All Rights Reserved.
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
-module(heart_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test suite exports
-export([
    all/0, suite/0,
    init_per_suite/1, end_per_suite/1
]).

%% Test case exports
-export([
    start/0, start/1,
    restart/0, restart/1,
    reboot/0, reboot/1,
    node_start_immediately_after_crash/1,
    node_start_soon_after_crash/1,
    set_cmd/1, clear_cmd/1, get_cmd/1,
    callback_api/1, options_api/1, dont_drop/1,
    kill_pid/0, kill_pid/1,
    heart_no_kill/0, heart_no_kill/1
]).

%% Internal exports used by peer nodes
-export([
    start_heart_stress/1, mangle/1,
    suicide_by_heart/0, non_suicide_by_heart/0,
    exhaust_atoms/0
]).

%%-----------------------------------------------------------------
%% Test suite for heart
suite() ->
    [{ct_hooks, [ts_install_cth]}, {timetrap, {minutes,2}}].

all() ->
    [
        start, restart, reboot,
        node_start_immediately_after_crash,
        node_start_soon_after_crash,
        set_cmd, clear_cmd, get_cmd, callback_api,
        options_api, kill_pid, heart_no_kill].

init_per_suite(Config) when is_list(Config) ->
    case os:type() of
        {win32, windows} ->
            {skipped, "No use to run on Windows 95/98"};
        _ ->
            ignore_cores:init(Config)
    end.

end_per_suite(Config) when is_list(Config) ->
    ignore_cores:fini(Config).

%%-----------------------------------------------------------------
%% Test suite for heart

heart_args() ->
    Common = ["-heart"],
    case os:type() of
        {win32, _} ->
            Common ++ ["-env", "HEART_COMMAND", "no_reboot"];
        _ ->
            Common
    end.

heart_monitor(Node) ->
    erlang:monitor_node(Node, true),
    %% assert that peer node has a heart
    is_pid(rpc:call(Node, erlang, whereis, [heart])) orelse
        ct:fail(heart_not_started),
    ok.

start() ->
    [{doc, "Test that node is down after init:reboot"}].

start(Config) when is_list(Config) ->
    {ok, _Peer, Node} = ?CT_PEER(heart_args()),
    heart_monitor(Node),
    ok = rpc:call(Node, init, reboot, []),
    receive
        {nodedown, Node} ->
            ok
    after
        2000 -> ct:fail(node_not_closed)
    end,
    timer:sleep(5000),
    net_adm:ping(Node) =:= pang orelse
        ct:fail(node_rebooted).

restart() ->
    [{doc, "Check that a node is up and running after a init:restart/0"}].

%% Also test fixed bug in R1B (it was not possible to
%% do init:stop/0 on a restarted system before)
%% Slave executes erlang:halt() on master nodedown.
%% Therefore the slave process has to be killed
%% before restart.
restart(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(#{connection => standard_io, args => heart_args()}),
    heart_monitor(Node),
    ok = rpc:call(Node, init, restart, []),
    receive
	    {nodedown, Node} ->
	        ok
    after 5000 ->
	    ct:fail(node_not_closed)
    end,
    timer:sleep(5000),
    node_check_up_down(Node, 2000),
    peer:stop(Peer).

reboot() ->
    [{doc, "Check that a node is up and running after a init:reboot/0"}].

reboot(Config) when is_list(Config) ->
    {ok, _Peer, Node} = ?CT_PEER(heart_args()),
    heart_monitor(Node),
    ok = rpc:call(Node, heart, set_cmd,
        [ct:get_progname() ++ " -noshell -heart " ++ name(Node) ++ "&"]),
    ok = rpc:call(Node, init, reboot, []),
    receive
        {nodedown, Node} ->
            ok
    after 2000 ->
	    ct:fail(node_not_closed)
    end,
    timer:sleep(5000),
    node_check_up_down(Node, 2000),
    ok.

%% node_start_immediately_after_crash
%% Purpose:
%%   Check that a node is up and running after a crash.
%%   This test exhausts the atom table on the remote node.
%%   ERL_CRASH_DUMP_SECONDS=0 will force beam not to dump an erl_crash.dump.
%% May currently dump core in beam debug build due to lock-order violation
%% This should be removed when a non-locked information retriever is implemented
%% for crash dumps
node_start_immediately_after_crash(Config) when is_list(Config) ->
    Config2 = ignore_cores:setup(?MODULE, ?FUNCTION_NAME, Config, true),
    try
        node_start_immediately_after_crash_test(Config2)
    after
        ignore_cores:restore(Config2)
    end.

node_start_immediately_after_crash_test(Config) when is_list(Config) ->
    {ok, _Peer, Node} = ?CT_PEER(#{connection => standard_io,
        args => heart_args() ++ ["-env", "ERL_CRASH_DUMP_SECONDS", "0"]}),
    heart_monitor(Node),

    ok = rpc:call(Node, heart, set_cmd,
        [ct:get_progname() ++ " -noshell -heart " ++ name(Node) ++ "&"]),

    %% crash it with atom exhaustion
    rpc:cast(Node, ?MODULE, exhaust_atoms, []),

    T0 = erlang:monotonic_time(),
    receive
        {nodedown, Node} ->
        Msec = erlang:convert_time_unit(erlang:monotonic_time() - T0, native, millisecond),
        io:format("Took ~.2f s. for node to go down~n", [Msec / 1000]),
        ok
    %% timeout is very liberal here. nodedown is received in about 1 s. on linux (palantir)
    %% and in about 10 s. on solaris (carcharoth)
    after (15000 * test_server:timetrap_scale_factor()) ->
        ct:fail(node_not_closed)
    end,
    timer:sleep(3000),
    node_check_up_down(Node, 2000).

%% node_start_soon_after_crash
%% Purpose:
%%   Check that a node is up and running after a crash.
%%   This test exhausts the atom table on the remote node.
%%   ERL_CRASH_DUMP_SECONDS=10 will force beam
%%   to only dump an erl_crash.dump for 10 seconds.
node_start_soon_after_crash(Config) when is_list(Config) ->
    Config2 = ignore_cores:setup(?MODULE, ?FUNCTION_NAME, Config, true),
    try
        node_start_soon_after_crash_test(Config2)
    after
        ignore_cores:restore(Config2)
    end.

node_start_soon_after_crash_test(Config) when is_list(Config) ->
    {ok, _Peer, Node} = ?CT_PEER(#{connection => standard_io,
        args => heart_args() ++ ["-env", "ERL_CRASH_DUMP_SECONDS", "10"]}),
    heart_monitor(Node),

    ok = rpc:call(Node, heart, set_cmd,
        [ct:get_progname() ++ " -noshell -heart " ++ name(Node) ++ "&"]),

    rpc:cast(Node, ?MODULE, exhaust_atoms, []),

    receive {nodedown, Node} -> ok
    after (15000 * test_server:timetrap_scale_factor()) -> ct:fail(node_not_closed)
    end,
    timer:sleep(20000),
    node_check_up_down(Node, 15000).


node_check_up_down(Node, Tmo) ->
    case net_adm:ping(Node) of
        pong ->
            erlang:monitor_node(Node, true),
            rpc:call(Node, init, reboot, []),
            receive
                {nodedown, Node} -> ok
            after Tmo ->
                ct:fail(node_not_closed2)
            end;
        _ ->
            ct:fail(node_not_rebooted)
    end.

%% Only tests bad command, correct behaviour is tested in reboot/1.
set_cmd(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(heart_args()),
    heart_monitor(Node),
    Cmd = wrong_atom,
    {error, {bad_cmd, Cmd}} = rpc:call(Node, heart, set_cmd, [Cmd]),
    Cmd1 = lists:duplicate(2047, $a),
    {error, {bad_cmd, Cmd1}} = rpc:call(Node, heart, set_cmd, [Cmd1]),
    Cmd2 = lists:duplicate(28, $a),
    ok = rpc:call(Node, heart, set_cmd, [Cmd2]),
    Cmd3 = lists:duplicate(2000, $a),
    ok = rpc:call(Node, heart, set_cmd, [Cmd3]),
    peer:stop(Peer),
    ok.

clear_cmd(Config) when is_list(Config) ->
    {ok, _Peer, Node} = ?CT_PEER(heart_args()),
    heart_monitor(Node),
    ok = rpc:call(Node, heart, set_cmd,
        [ct:get_progname() ++
            " -noshell -heart " ++ name(Node) ++ "&"]),
    rpc:call(Node, init, reboot, []),
    receive
        {nodedown, Node} ->
            ok
    after 2000 ->
        ct:fail(node_not_closed)
    end,
    timer:sleep(5000),
    case net_adm:ping(Node) of
        pong ->
            erlang:monitor_node(Node, true);
        _ ->
            ct:fail(node_not_rebooted)
    end,
    ok = rpc:call(Node, heart, set_cmd,
        ["erl -noshell -heart " ++ name(Node) ++ "&"]),
    ok = rpc:call(Node, heart, clear_cmd, []),
    rpc:call(Node, init, reboot, []),
    receive
        {nodedown, Node} ->
            ok
    after 2000 ->
        ct:fail(node_not_closed)
    end,
    timer:sleep(5000),
    case net_adm:ping(Node) of
        pang ->
            ok;
        _ ->
            ct:fail(node_rebooted)
    end,
    ok.

get_cmd(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(heart_args()),
    heart_monitor(Node),

    ShortCmd = "test",
    ok  = rpc:call(Node, heart, set_cmd, [ShortCmd]),
    {ok, ShortCmd} = rpc:call(Node, heart, get_cmd, []),

    %% This would hang prior to OTP-15024 being fixed.
    LongCmd = [$a || _ <- lists:seq(1, 160)],
    ok  = rpc:call(Node, heart, set_cmd, [LongCmd]),
    {ok, LongCmd} = rpc:call(Node, heart, get_cmd, []),

    peer:stop(Peer),
    ok.

callback_api(Config) when is_list(Config) ->
    {ok, _Peer, Node} = ?CT_PEER(heart_args()),
    heart_monitor(Node),
    none = rpc:call(Node, heart, get_callback, []),
    M0 = self(),
    F0 = ok,
    {error, {bad_callback, {M0,F0}}} = rpc:call(Node, heart, set_callback, [M0,F0]),
    none = rpc:call(Node, heart, get_callback, []),
    M1 = lists:duplicate(28, $a),
    F1 = lists:duplicate(28, $b),
    {error, {bad_callback, {M1,F1}}} = rpc:call(Node, heart, set_callback, [M1,F1]),
    none = rpc:call(Node, heart, get_callback, []),

    M2 = heart_check_module,
    F2 = cb_ok,
    F3 = cb_error,
    Code0 = generate(M2, [], [
	    atom_to_list(F2) ++ "() -> ok.",
            atom_to_list(F3) ++ "() -> exit(\"callback_error (as intended)\")."
	]),
    {module, M2} = rpc:call(Node, erlang, load_module, [M2, Code0]),
    ok = rpc:call(Node, M2, F2, []),
    ok = rpc:call(Node, heart, set_callback, [M2,F2]),
    {ok, {M2,F2}} = rpc:call(Node, heart, get_callback, []),
    ok = rpc:call(Node, heart, clear_callback, []),
    none = rpc:call(Node, heart, get_callback, []),
    ok = rpc:call(Node, heart, set_callback, [M2,F2]),
    {ok, {M2,F2}} = rpc:call(Node, heart, get_callback, []),
    ok = rpc:call(Node, heart, set_callback, [M2,F3]),
    receive {nodedown, Node} -> ok
    after 5000 -> ct:fail(node_not_killed)
    end,
    ok.

options_api(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(heart_args()),
    heart_monitor(Node),
    none = rpc:call(Node, heart, get_options, []),
    M0 = self(),
    F0 = ok,
    {error, {bad_options, {M0,F0}}} = rpc:call(Node, heart, set_options, [{M0,F0}]),
    none = rpc:call(Node, heart, get_options, []),
    Ls = lists:duplicate(28, $b),
    {error, {bad_options, Ls}} = rpc:call(Node, heart, set_options, [Ls]),
    none = rpc:call(Node, heart, get_options, []),

    ok = rpc:call(Node, heart, set_options, [[check_schedulers]]),
    {ok, [check_schedulers]} = rpc:call(Node, heart, get_options, []),
    ok = rpc:call(Node, heart, set_options, [[]]),
    none = rpc:call(Node, heart, get_options, []),

    ok = rpc:call(Node, heart, set_options, [[check_schedulers]]),
    {ok, [check_schedulers]} = rpc:call(Node, heart, get_options, []),
    {error, {bad_options, Ls}} = rpc:call(Node, heart, set_options, [Ls]),
    {ok, [check_schedulers]} = rpc:call(Node, heart, get_options, []),

    receive after 3000 -> ok end, %% wait 3 secs

    ok = rpc:call(Node, heart, set_options, [[]]),
    none = rpc:call(Node, heart, get_options, []),
    peer:stop(Peer),
    ok.


%%% Removed as it may crash epmd/distribution in colourful
%%% ways. While we ARE finding out WHY, it would
%%% be nice for others to be able to run the kernel test suite
%%% without "exploding machines", so that's why I removed it for now.

%% Tests that the heart command does not get dropped when
%% set just before halt on very high I/O load..
dont_drop(Config) when is_list(Config) ->
    %%% Have to do it some times to make it happen...
    [ok,ok,ok,ok,ok,ok,ok,ok,ok,ok] = do_dont_drop(Config,10),
    ok.

do_dont_drop(_,0) -> [];
do_dont_drop(Config,N) ->
    %% Name of first slave node
    NN1 = atom_to_list(?MODULE) ++ "slave_1",
    %% Name of node started by heart on failure
    NN2 = atom_to_list(?MODULE) ++ "slave_2",
    %% Name of node started by heart on success
    NN3 = atom_to_list(?MODULE) ++ "slave_3",
    Host = hd(tl(string:tokens(atom_to_list(node()),"@"))),
    %% The initial heart command
    FirstCmd = erl() ++ name(NN2 ++ "@" ++ Host),
    %% Separated the parameters to start_node_run for clarity...
    Name = list_to_atom(NN1),
    EchoPath = filename:join(proplists:get_value(data_dir, Config), "simple_echo"),
    Peer3Name = list_to_atom(NN3 ++ "@" ++ Host),
    %%
    PA = filename:dirname(code:which(?MODULE)),
    Params = ["-heart", "-pa", PA, "-env", "HEART_COMMAND", FirstCmd],
    {ok, Peer, _Node} = ?CT_PEER(#{name => Name, args => Params}),
    peer:call(Peer, ?MODULE, start_heart_stress, [Peer3Name, EchoPath]),
    %%
    case wait_for_any_of(list_to_atom(NN2 ++ "@" ++ Host),
	    list_to_atom(NN3 ++ "@" ++ Host)) of
	2 ->
	    [ok | do_dont_drop(Config,N-1)];
	_ ->
	    false
    end.

wait_for_any_of(N1,N2) ->
    wait_for_any_of(N1,N2,45).

wait_for_any_of(_N1,_N2,0) ->
    false;

wait_for_any_of(N1,N2,Times) ->
    receive after 1000 -> ok end,
    case net_adm:ping(N1) of
	pang ->
	    case net_adm:ping(N2) of
		pang ->
		    wait_for_any_of(N1,N2,Times - 1);
		pong ->
		    rpc:call(N2,init,stop,[]),
		    2
	    end;
	pong ->
	    rpc:call(N1,init,stop,[]),
	    1
    end.


kill_pid() ->
    [{doc, "Tests that heart kills the old erlang node before executing heart command."}].

kill_pid(Config) when is_list(Config) ->
    {ok, _Peer, Node} = ?CT_PEER(heart_args() ++ ["-env", "HEART_COMMAND", "nickeNyfikenFarEttJobb"]),
    heart_monitor(Node),
    true = rpc:cast(Node, ?MODULE, suicide_by_heart, []),
    erlang:monitor_node(Node, true),
    receive {nodedown, Node} -> ok
    after 30000 ->
        ct:fail(heart_did_not_kill_old_node)
    end.


heart_no_kill() ->
    [{doc, "Tests that heart doesn't kill the old erlang node when HEART_NO_KILL is set."}].

heart_no_kill(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(heart_args()),
    heart_monitor(Node),
    true = rpc:cast(Node, ?MODULE, non_suicide_by_heart, []),
    receive
        {nodedown, Node} ->
            ct:fail(heart_killed_old_node)
    after 30000 ->
        peer:stop(Peer)
    end.

erl() ->	   
    case os:type() of
	{win32,_} -> "werl ";
	_ -> "erl "
    end.

name(Node) when is_list(Node) -> name(Node, []);
name(Node) when is_atom(Node) -> name(atom_to_list(Node), []).

name([$@ | Node], Name) ->
    case lists:member($., Node) of
        true ->
            "-name " ++ lists:reverse(Name);
        _ ->
            "-sname " ++ lists:reverse(Name)
    end;
name([H | T], Name) ->
    name(T, [H | Name]).


%%% This code is run in a peer node to ensure that
%%% A heart command really gets set synchronously
%%% and cannot get "dropped".

send_to(_, _, 0) ->
    ok;
send_to(Port, D, N) ->
    Port ! {self(), {command, D}},
    send_to(Port, D, N - 1).

receive_from(_, _, 0) ->
    ok;

receive_from(Port, D, N) ->
    receive
        {Port, {data, {eol, _Data}}} ->
            receive_from(Port, D, N - 1);
        X ->
            io:format("Got garbage ~p~n", [X])
    end.

mangle(PP) when is_list(PP) ->
    Port = open_port({spawn, PP}, [{line, 100}]),
    mangle(Port);

mangle(Port) ->
    send_to(Port, "ABCDEFGHIJ" ++ io_lib:nl(), 1),
    receive_from(Port, "ABCDEFGHIJ", 1),
    mangle(Port).



explode(0, _) ->
    ok;
explode(N, PP) ->
    spawn(?MODULE, mangle, [PP]),
    explode(N - 1, PP).

start_heart_stress([NewName, PortProgram]) ->
    explode(10, atom_to_list(PortProgram)),
    NewCmd = erl() ++ name(NewName),
    %%io:format("~p~n",[NewCmd]),
    receive
    after 10000 ->
        heart:set_cmd(NewCmd),
        halt()
    end.

suicide_by_heart() ->
    %%io:format("Suicide starting...~n"),
    open_port({spawn, "heart -ht 11 -pid " ++ os:getpid()}, [{packet, 2}]),
    receive X -> X end,
    %% Just hang and wait for heart to timeout
    receive
        {makaronipudding} ->
            sallad
    end.

non_suicide_by_heart() ->
    P = open_port({spawn, "heart -ht 11 -pid " ++ os:getpid()},
        [exit_status, {env, [{"HEART_NO_KILL", "TRUE"}]},
            {packet, 2}]),
    receive X -> X end,
    %% Just hang and wait for heart to timeout
    receive
        {P, {exit_status, _}} ->
            ok
    after
        20000 ->
            exit(timeout)
    end.

exhaust_atoms() ->
    Set = lists:seq($a,$z),
    [list_to_atom([A,B,C,D,E]) || A <- Set, B <- Set, C <- Set, E <- Set, D <- Set].

%% generate a module from binary
generate(Module, Attributes, FunStrings) ->
    FunForms = function_forms(FunStrings),
    Forms = [
        {attribute, a(1), module, Module},
        {attribute, a(2), export, [FA || {FA, _} <- FunForms]}
    ] ++ [{attribute, a(3), A, V} || {A, V} <- Attributes] ++
        [Function || {_, Function} <- FunForms],
    {ok, Module, Bin} = compile:forms(Forms),
    Bin.

a(L) ->
    erl_anno:new(L).

function_forms([]) -> [];
function_forms([S | Ss]) ->
    {ok, Ts, _} = erl_scan:string(S),
    {ok, Form} = erl_parse:parse_form(Ts),
    Fun = element(3, Form),
    Arity = element(4, Form),
    [{{Fun, Arity}, Form} | function_forms(Ss)].
