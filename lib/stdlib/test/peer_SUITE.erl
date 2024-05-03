%%% @doc
%%% Smoke tests for peer node controller
%%% @end
-module(peer_SUITE).
-author("maximfca@gmail.com").

%% Common Test headers
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([
    suite/0,
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2
]).

%% Test cases
-export([
    dist/0, dist/1,
    peer_down_crash/0, peer_down_crash/1,
    peer_down_crash_tcp/1,
    peer_down_continue/0, peer_down_continue/1,
    peer_down_boot/0, peer_down_boot/1,
    dist_io_redirect/0, dist_io_redirect/1,
    dist_localhost/0, dist_localhost/1,
    errors/0, errors/1,
    basic/0, basic/1,
    peer_states/0, peer_states/1,
    cast/0, cast/1,
    detached/0, detached/1,
    dyn_peer/0, dyn_peer/1,
    stop_peer/0, stop_peer/1,
    shutdown_halt/0, shutdown_halt/1,
    shutdown_halt_timeout/0, shutdown_halt_timeout/1,
    shutdown_stop/0, shutdown_stop/1,
    shutdown_stop_timeout/0, shutdown_stop_timeout/1,
    shutdown_close/0, shutdown_close/1,
    init_debug/0, init_debug/1,
    io_redirect/0, io_redirect/1,
    multi_node/0, multi_node/1,
    dist_up_down/0, dist_up_down/1,
    duplicate_name/0, duplicate_name/1,
    old_release/0, old_release/1,
    ssh/0, ssh/1,
    docker/0, docker/1,
    post_process_args/0, post_process_args/1,
    attached/0, attached/1,
    attached_cntrl_channel_handler_crash/0, attached_cntrl_channel_handler_crash/1,
    cntrl_channel_handler_crash/0, cntrl_channel_handler_crash/1,
    cntrl_channel_handler_crash_old_release/0, cntrl_channel_handler_crash_old_release/1
]).

suite() ->
    [{timetrap, {minutes, 1}}].

init_per_suite(Config) ->
    %% Restrict number of schedulers so that we do not run out of
    %% file descriptors during test
    os:putenv("ERL_FLAGS","+S1 +SDio 1"),
    Config.

end_per_suite(_Config) ->
    os:unsetenv("ERL_FLAGS"),
    ok.

shutdown_alternatives() ->
    [shutdown_halt, shutdown_halt_timeout, shutdown_stop, shutdown_stop_timeout, shutdown_close].

alternative() ->
    [basic, peer_states, cast, detached, dyn_peer, stop_peer,
     io_redirect, multi_node, duplicate_name, attached, attached_cntrl_channel_handler_crash,
     cntrl_channel_handler_crash, cntrl_channel_handler_crash_old_release | shutdown_alternatives()].

groups() ->
    [
        {dist, [parallel], [errors, dist, peer_down_crash, peer_down_continue, peer_down_boot,
                            dist_up_down, dist_localhost, post_process_args, attached,
                            attached_cntrl_channel_handler_crash, cntrl_channel_handler_crash,
                            cntrl_channel_handler_crash_old_release | shutdown_alternatives()]},
        {dist_seq, [], [dist_io_redirect,      %% Cannot be run in parallel in dist group
                        peer_down_crash_tcp]},
        {tcp, [parallel], alternative()},
        {standard_io, [parallel], [init_debug | alternative()]},
        {compatibility, [parallel], [old_release]},
        {remote, [parallel], [ssh]}
    ].

all() ->
    [{group, dist}, {group, dist_seq}, {group, tcp}, {group, standard_io},
     {group, compatibility}, {group, remote}].

init_per_group(remote, Config) ->
    %% check that SSH can connect to localhost, skip the test if not
    try os:cmd("timeout 10s ssh localhost echo ok") of
        "ok\n" -> Config;
        _ -> {skip, "'ssh localhost echo ok' did not return ok"}
    catch
        Class:Reason ->
            SkipReason = io_lib:format("'ssh localhost echo ok' failed with ~s:~p", [Class, Reason]),
            {skip, SkipReason}
    end;
init_per_group(dist, Config) ->
    case erlang:is_alive() of
        true -> Config;
        false -> {skip, "origin is not distributed"}
    end;
init_per_group(tcp, Config) ->
    [{connection, 0} | Config];
init_per_group(standard_io, Config) ->
    [{connection, standard_io} | Config];
init_per_group(_Group, Config) ->
    Config.

end_per_group(_TestCase, Config) ->
    proplists:delete(connection, Config).

%% -------------------------------------------------------------------
%% Distribution-enabled cases

errors() ->
    [{doc, "Verify that invalid command line is rejected immediately"}].

errors(Config) when is_list(Config) ->
    ?assertException(error, {invalid_arg, atom}, peer:start(#{args => atom})),
    ?assertException(error, {invalid_arg, $s}, peer:start(#{args => "string"})),
    ?assertException(error, {invalid_arg, ["2"]}, peer:start(#{args => ["1", ["2"]]})),
    ?assertException(error, {exec, atom}, peer:start(#{exec => atom, name => name})),
    ?assertException(error, {exec, atom}, peer:start(#{exec => {atom, []}, name => name})),
    ?assertException(error, {exec, atom}, peer:start(#{exec => {"erl", [atom]}, name => name})),
    ?assertException(error, not_alive, peer:start(#{})). %% peer node won't start - it's not alive

dist() ->
    [{doc, "Classic behaviour: detached peer with no alternative, connects via dist"}].

%% Classic 'slave': start new node locally, with random name, ask peer control process to exit normally
dist(Config) when is_list(Config) ->
    {ok, Peer, Node} = peer:start_link(),
    %% distribution is expected to be connected
    ?assertEqual(Node, erpc:call(Node, erlang, node, [])),
    %% but not alternative...
    ?assertException(error, noconnection, peer:call(Peer, erlang, node, [])),
    ?assertEqual(true, net_kernel:disconnect(Node)),
    %% ^^^ this makes the node go down, but we also need to ensure that net_kernel
    %%  finished processing
    ct:sleep(500),
    _ = sys:get_state(net_kernel),
    ?assertException(exit, {noproc, _}, peer:get_state(Peer)).

peer_down_crash() ->
    [{doc, "Tests peer_down handling when crash mode is requested"}].

peer_down_crash(Config) when is_list(Config) ->
    %% two-way link: "crash" mode
    {ok, Peer, Node} = peer:start_link(#{name => peer:random_name(?FUNCTION_NAME),
        args => ["-connect_all", "false"], peer_down => crash}),
    %% verify node started locally
    ?assertEqual(Node, erpc:call(Node, erlang, node, [])),
    %% verify there is no alternative connection
    ?assertException(error, noconnection, peer:call(Peer, erlang, node, [])),
    %% unlink and monitor
    unlink(Peer),
    MRef = monitor(process, Peer),
    ?assertEqual(true, net_kernel:disconnect(Node)),
    %% ^^^ this makes the node go down
    %% since two-way link is requested, it triggers peer to stop
    receive
        {'DOWN', MRef, process, Peer, {nodedown, Node}} ->
            ok
    after 2000 ->
        link(Peer),
        {fail, disconnect_timeout}
    end.

%% Verify option combo #{peer_down=>crash, connection=>0}
%% exits control process abnormally.
peer_down_crash_tcp(Config) when is_list(Config) ->
    %% two-way link: "crash" mode
    {ok, Peer, Node} = peer:start_link(#{name => peer:random_name(?FUNCTION_NAME),
                                         peer_down => crash,
                                         connection => 0}),
    %% verify node started locally
    ?assertEqual(Node, peer:call(Peer, erlang, node, [])),
    %% verify there is no distribution connection
    ?assertEqual([], erlang:nodes(connected)),
    %% unlink and monitor
    unlink(Peer),
    MRef = monitor(process, Peer),
    %% Make the node go down
    ok = erpc:cast(Node, erlang, halt, [0]),

    %% since two-way link is requested, it triggers peer to stop
    receive
        {'DOWN', MRef, process, Peer, tcp_closed} ->
            ok
    after 5000 ->
        link(Peer),
        {fail, disconnect_timeout}
    end.

peer_down_continue() ->
    [{doc, "Tests peer_down handling for continue setting"}].

peer_down_continue(Config) when is_list(Config) ->
    {ok, Peer, Node} = peer:start_link(#{name => peer:random_name(?FUNCTION_NAME),
        args => ["-connect_all", "false"], peer_down => continue}),
    ?assertEqual(ok, erpc:cast(Node, erlang, halt, [])),
    ct:sleep(500),
    sys:replace_state(net_kernel, fun(S) -> sys:get_state(Peer), S end),
    ?assertMatch({down, _}, peer:get_state(Peer)),
    peer:stop(Peer).

peer_down_boot() ->
    [{doc, "Tests that peer node failing boot fails start_link correctly"}].

peer_down_boot(Config) when is_list(Config) ->
    ?assertException(exit, {boot_failed, {exit_status, 1}},
        peer:start_link(#{connection => standard_io, name => peer:random_name(), args => ["-no_epmd"]})).

dist_io_redirect() ->
    [{doc, "Tests i/o redirection working for dist"}].

dist_io_redirect(Config) when is_list(Config) ->
    %% Common Test changes group leader process to capture output.
    %% 'peer' relays output via control process group leader.
    ct:capture_start(),
    {ok, Peer, Node} = peer:start_link(#{name => peer:random_name(?FUNCTION_NAME),
        args => ["-connect_all", "false", "-eval", "io:format(\"out\")."]}),
    %% RPC is smart enough to set the group leader, so force 'user' output
    %% to check that peer node redirects 'user' to the current process group
    %% leader.
    ?assertEqual(ok, erpc:call(Node, io, format, [user, "STRONGFLOUR.", []])),
    %% verify that 'send' is ignored when no alternative connection is done
    ?assertEqual(ok, peer:send(Peer, init, {stop, stop})),
    %% check that RPC sets the group leader, even via 'apply'
    ?assertEqual(ok, erpc:call(Node, erlang, apply, [io, format, ["second."]])),
    %% 'eval' at the end may be quite slow, so have to wait here
    ct:sleep(500),
    peer:stop(Peer),
    ct:capture_stop(),
    Texts = ct:capture_get(),
    %% order is not guaranteed, so sort explicitly
    ?assertEqual(lists:sort(["STRONGFLOUR.", "second.", "out"]), lists:sort(Texts)).

dist_up_down() ->
    [{doc, "Test that Erlang distribution can go up and down (with TCP alternative)"}].

dist_up_down(Config) when is_list(Config) ->
    %% skip establishing full mesh, for it makes 'global' hang
    %% TODO: fix 'global.erl' locker process so it does not hang
    {ok, Peer, Node} = peer:start_link(#{name => peer:random_name(?FUNCTION_NAME),
        connection => {{127, 0, 0, 1}, 0}, args => ["-connect_all", "false"]}),
    ?assertEqual(true, net_kernel:connect_node(Node)),
    ?assertEqual(true, net_kernel:disconnect(Node)),
    ?assertEqual(true, net_kernel:connect_node(Node)),
    peer:stop(Peer).

dist_localhost() ->
    [{doc, "Test that localhost and gethostname operate together"}].

dist_localhost(Config) when is_list(Config) ->
    {ok, Peer, Node} = peer:start_link(#{name => peer:random_name(?FUNCTION_NAME), host => "localhost"}),
    ?assertMatch([_, "localhost"], string:lexemes(atom_to_list(Node), "@")),
    %% start second peer, ensure they see each other
    {ok, Host} = inet:gethostname(),
    {ok, Peer2, Node2} = peer:start_link(#{name => peer:random_name(?FUNCTION_NAME), host => Host}),
    true = erpc:call(Node, net_kernel, connect_node, [Node2]),
    peer:stop(Peer),
    peer:stop(Peer2).

%% -------------------------------------------------------------------
%% alternative connection cases

%% Runs in the peer node context, forward a message from peer node to origin
%%  node via alternative connection.
-spec forward(Dest :: pid() | atom(), Message :: term()) -> term().
forward(Dest, Message) ->
    group_leader() ! {message, Dest, Message}.

basic() ->
    [{doc, "Tests peer node start, and do some RPC via stdin/stdout"}].

basic(Config) when is_list(Config) ->
    Conn = proplists:get_value(connection, Config),
    {ok, Peer, _Node} = peer:start_link(#{connection => Conn}),
    %% test the alternative connection
    ?assertEqual('nonode@nohost', peer:call(Peer, erlang, node, [])),
    ?assertException(throw, ball, peer:call(Peer, erlang, throw, [ball])),
    %% setup code path to this module (needed to "fancy RPC")
    Path = filename:dirname(code:which(?MODULE)),
    ?assertEqual(true, peer:call(Peer, code, add_path, [Path])),
    %% fancy RPC via message exchange (uses forwarding from the peer)
    Control = self(),
    RFun = fun() -> receive do -> forward(Control, done) end end,
    RemotePid = peer:call(Peer, erlang, spawn, [RFun]),
    peer:send(Peer, RemotePid, do),
    %% wait back from that process
    receive done -> ok end,
    %% shutdown the node
    ?assertEqual(ok, peer:stop(Peer)),
    ?assertNot(is_process_alive(Peer)).

peer_states() ->
    [{doc, "Tests peer node states"}].

peer_states(Config) when is_list(Config) ->
    Conn = proplists:get_value(connection, Config),
    {ok, Peer} = peer:start_link(#{connection => Conn, wait_boot => {self(), ?FUNCTION_NAME},
        peer_down => continue}),
    ?assertEqual(booting, peer:get_state(Peer)),
    %% running
    receive {?FUNCTION_NAME, {started, _Node, Peer}} -> ok end,
    ?assertEqual(running, peer:get_state(Peer)),
    peer:cast(Peer, erlang, halt, []),
    ct:sleep(1000), %% source of flakiness, should switch to some better notification mechanism
    %% down
    ?assertMatch({down, _}, peer:get_state(Peer)),
    peer:stop(Peer).

cast() ->
    [{doc, "Tests casts via alternative connections"}].

cast(Config) when is_list(Config) ->
    Conn = proplists:get_value(connection, Config),
    {ok, Peer, _Node} = peer:start_link(#{connection => Conn}),
    %% cast RPC
    ?assertEqual(undefined, peer:call(Peer, application, get_env, [kernel, foo])),
    peer:cast(Peer, application, set_env, [kernel, foo, bar]),
    %% this is only to ensure application_controller completed processing
    peer:call(Peer, sys, get_state, [application_controller]),
    ?assertEqual({ok, bar}, peer:call(Peer, application, get_env, [kernel, foo])),
    peer:stop(Peer).

detached() ->
    [{doc, "Tests detached node (RPC via alternative connection)"}].

detached(Config) when is_list(Config) ->
    Conn = proplists:get_value(connection, Config),
    {ok, Peer, _Node} = peer:start_link(#{connection => Conn}),
    ?assertEqual('nonode@nohost', peer:call(Peer, erlang, node, [])),
    %% check exceptions
    ?assertException(throw, ball, peer:call(Peer, erlang, throw, [ball])),
    %% check tcp forwarding
    Path = filename:dirname(code:which(?MODULE)),
    ?assertEqual(true, peer:call(Peer, code, add_path, [Path])),
    %% fancy RPC via message exchange (uses forwarding from the peer)
    Control = self(),
    RFun = fun() -> receive do -> forward(Control, done) end end,
    RemotePid = peer:call(Peer, erlang, spawn, [RFun]),
    %% test that sending message over alternative TCP connection works
    peer:send(Peer, RemotePid, do),
    %% wait back from that process
    receive done -> ok end,
    %% logging via TCP
    ct:capture_start(),
    peer:call(Peer, io, format, ["one."]),
    peer:call(Peer, erlang, apply, [io, format, ["two."]]),
    peer:stop(Peer),
    ct:capture_stop(),
    Texts = ct:capture_get(),
    %% just stop
    ?assertEqual(["one.", "two."], Texts).

dyn_peer() ->
    [{doc, "Origin is not distributed, and peer becomes distributed dynamically"}].

dyn_peer(Config) when is_list(Config) ->
    Conn = proplists:get_value(connection, Config),
    {ok, Peer, 'nonode@nohost'} = peer:start_link(#{connection => Conn}), %% start not distributed
    Node = list_to_atom(lists:concat([peer:random_name(?FUNCTION_NAME), "@forced.host"])),
    {ok, _} = peer:call(Peer, net_kernel, start, [[Node, longnames]]),
    ?assertEqual(Node, peer:call(Peer, erlang, node, [])),
    peer:stop(Peer).

stop_peer() ->
    [{doc, "Test that peer shuts down even when node sleeps, but control connection closed"}].

stop_peer(Config) when is_list(Config) ->
    Conn = proplists:get_value(connection, Config),
    {ok, Peer, _Node} = peer:start_link(#{name => peer:random_name(?FUNCTION_NAME),
        connection => Conn, args => ["-eval", "timer:sleep(60000)."]}),
    %% shutdown node
    peer:stop(Peer).

shutdown_halt() ->
    [{doc, "Test that peer shutdown halt wait until node connection is down"}].
shutdown_halt(Config) when is_list(Config) ->
    false = shutdown_test(Config, ?FUNCTION_NAME, halt, 500, false, 1000),
    ok.

shutdown_halt_timeout() ->
    [{doc, "Test that peer shutdown halt forcefully takes down connection on timeout"}].
shutdown_halt_timeout(Config) when is_list(Config) ->
    false = shutdown_test(Config, ?FUNCTION_NAME, {halt, 1000}, 5000, true, 1500),
    ok.

shutdown_stop() ->
    [{doc, "Test that peer shutdown stop wait until node connection is down"}].
shutdown_stop(Config) when is_list(Config) ->
    false = shutdown_test(Config, ?FUNCTION_NAME, infinity, 500, false, 2000),
    ok.

shutdown_stop_timeout() ->
    [{doc, "Test that peer shutdown stop forcefully takes down connection on timeout"}].
shutdown_stop_timeout(Config) when is_list(Config) ->
    false = shutdown_test(Config, ?FUNCTION_NAME, 1000, 5000, true, 2500),
    ok.

shutdown_close() ->
    [{doc, "Test that peer shutdown close does not wait for dist connection"}].
shutdown_close(Config) when is_list(Config) ->
    _ = shutdown_test(Config, ?FUNCTION_NAME, close, 5000, true, 200),
    ok.

shutdown_test(Config, TC, Shutdown, BlockTime, StopWhileBlocked, MaxWaitTime) ->
    Options0 = #{name => ?CT_PEER_NAME(TC),
                 shutdown => Shutdown,
                 args => ["-hidden", "-pa", filename:dirname(code:which(?MODULE)),
                          "-setcookie", atom_to_list(erlang:get_cookie())]},
    Options = case proplists:get_value(connection, Config) of
                  undefined -> Options0;
                  Conn -> maps:put(connection, Conn, Options0)
              end,
    {ok, Peer, Node} = peer:start_link(Options),
    EnsureBlockedWait = 500,
    BlockStart = erlang:monotonic_time(millisecond),
    erpc:cast(Node,
              fun () ->
                      erts_debug:set_internal_state(available_internal_state, true),
                      erts_debug:set_internal_state(block, BlockTime+EnsureBlockedWait)
              end),
    receive after EnsureBlockedWait -> ok end, %% Ensure blocked...
    Start = erlang:monotonic_time(millisecond),
    peer:stop(Peer),
    End = erlang:monotonic_time(millisecond),
    WaitTime = End - Start,
    BlockTimeLeft = BlockTime + EnsureBlockedWait - (Start - BlockStart),
    Connected = lists:member(Node, nodes(connected)),
    ct:pal("Connected = ~p~nWaitTime = ~p~nBlockTimeLeft = ~p~n",
           [Connected, WaitTime, BlockTimeLeft]),
    true = WaitTime =< MaxWaitTime,
    case StopWhileBlocked of
        true -> ok;
        false -> true = WaitTime >= BlockTimeLeft, ok
    end,
    Connected.

init_debug() ->
    [{doc, "Test that debug messages in init work"}].

init_debug(Config) when is_list(Config) ->
    ct:capture_start(),
    {ok, Peer, _Node} = peer:start_link(#{name => peer:random_name(?FUNCTION_NAME), shutdown => 1000,
        connection => standard_io, args => ["-init_debug"]}),
    ct:sleep(200), %% without this sleep, peer is not fast enough to print
    peer:stop(Peer),
    ct:capture_stop(),
    Texts = lists:append([string:trim(Str, trailing, "\r\n") || Str <- ct:capture_get()]),
    %% every boot script starts with this
    Expected = "{progress,preloaded}",
    Actual = lists:sublist(Texts, 1, length(Expected)),
    ?assertEqual(Expected, Actual).

io_redirect() ->
    [{doc, "Tests i/o redirection working for std"}].

io_redirect(Config) when is_list(Config) ->
    Conn = proplists:get_value(connection, Config),
    {ok, Peer, _Node} = peer:start_link(#{connection => Conn}),
    ct:capture_start(),
    peer:call(Peer, io, format, [user, "test.", []]),
    peer:call(Peer, erlang, apply, [io, format, ["second."]]),
    %% ensure no dist connection is set up
    ?assertNot(lists:member(Peer, nodes()), {dist_connected, Peer, nodes()}),
    ct:capture_stop(),
    Texts = ct:capture_get(),
    peer:stop(Peer),
    ?assertEqual(["test.", "second."], Texts).

multi_node() ->
    [{doc, "Tests several nodes starting concurrently"}].

multi_node(Config) when is_list(Config) ->
    Conn = proplists:get_value(connection, Config),
    Peers = [
        peer:start_link(#{name => peer:random_name(?FUNCTION_NAME),
            wait_boot => {self(), tag}, connection => Conn})
        || _ <- lists:seq(1, 4)],
    Nodes = [receive {tag, {started, Node, Peer}} -> Node end || {ok, Peer} <- Peers],
    ?assertEqual(4, length(Nodes)),
    [?assertEqual(ok, peer:stop(Peer)) || {ok, Peer} <- Peers].

duplicate_name() ->
    [{doc, "Tests that a node with the same name fails to start"}].

duplicate_name(Config) when is_list(Config) ->
    Conn = proplists:get_value(connection, Config),
    {ok, Peer, _Node} = peer:start_link(#{connection => Conn, name => ?FUNCTION_NAME, register => false}),
    ?assertException(exit, _, peer:start_link(#{connection => standard_io, name => ?FUNCTION_NAME})),
    peer:stop(Peer).

post_process_args() ->
    [{doc, "Test that the post_process_args option works"}].

post_process_args(Config) when is_list(Config) ->
    case {os:type(),os:find_executable("bash")} of
        {{win32,_}, _Bash} ->
            {skip,"Test does not work on windows"};
        {_, false} ->
            {skip,"Test needs bash to run"};
        {_, Bash} ->
            Erl = string:split(ct:get_progname()," ",all),
            [throw({skip, "Needs bash to run"}) || Bash =:= false],
            {ok, Peer, _Node} =
                peer:start_link(
                  #{ name => ?CT_PEER_NAME(),
                     exec => {Bash,["-c"|Erl]},
                     post_process_args =>
                         fun(["-c"|Args]) ->
                                 ["-c", lists:flatten(lists:join($\s, Args))]
                         end }),
            peer:stop(Peer)
    end.

%% -------------------------------------------------------------------
%% Compatibility: old releases
old_release() ->
    [{doc, "Verity running with previous OTP release"}].

old_release(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    %% don't expect OTP 10 to be available
    ?assertEqual(not_available, ?CT_PEER_REL([], "10", PrivDir)),
    PrevRel = integer_to_list(list_to_integer(erlang:system_info(otp_release)) - 2),
    case ?CT_PEER_REL([], PrevRel, PrivDir) of
        not_available ->
            {skip, "OTP " ++ PrevRel ++ " not found"};
        {ok, Peer, Node} ->
            ?assertEqual(PrevRel, rpc:call(Node, erlang, system_info, [otp_release])),
            peer:stop(Peer)
    end.

%% -------------------------------------------------------------------
%% SSH/Docker cases

ssh() ->
    [{doc, "Tests ssh (localhost) node support"}].

ssh(Config) when is_list(Config) ->
    %% figure out path to 'erl' locally ('erl' may not be in path via SSH)
    case {os:find_executable("erl"), os:find_executable("ssh")} of
        {false, _} ->
            {skip, "erl not found"};
        {ErlPath, SshPath} ->
            Name = peer:random_name(?FUNCTION_NAME),
            {OsName, _} = os:type(),
            Options = #{exec => {SshPath, ["localhost", ErlPath]},
                        connection => standard_io, name => Name, host => "localhost"},
            {ok, Peer, _Node} =
                try peer:start_link(Options) of
                    Result -> Result
                catch error:{boot_failed, normal} when OsName =:= win32 ->
                        %% If the boot fails on windows, ssh may have ended up
                        %% in wsl, so we try to boot using a wsl path
                        WslPath = string:trim(os:cmd("wsl wslpath -u " ++ ErlPath)),
                        peer:start_link(
                          Options#{ exec => {SshPath, ["localhost", WslPath]}})
                end,

            %% TODO: how to check it really goes over SSH?
            %% ssh-ed node is not distributed
            ?assertEqual(list_to_atom(Name ++ "@localhost"), peer:call(Peer, erlang, node, [])),
            peer:stop(Peer)
    end.

docker() ->
    [{doc, "Tests starting peer node in Docker container"}, {timetrap, {seconds, 60}}].

build_release(Dir) ->
    application:load(sasl), %% otherwise application:get_key will fail
    %% build release (tarball)
    RelFile = filename:join(Dir, "lambda.rel"),
    Release = {release, {"lambda", "1.0.0"}, {erts, erlang:system_info(version)},
        [{App, begin {ok, Vsn} = application:get_key(App, vsn), Vsn end} || App <- [kernel, stdlib, sasl]]},
    ok = file:write_file(RelFile, list_to_binary(lists:flatten(io_lib:format("~tp.", [Release])))),
    RelFileNoExt = filename:join(Dir, "lambda"),
    {ok, systools_make, []} = systools:make_script(RelFileNoExt, [silent, {outdir, Dir}]),
    ok = systools:make_tar(RelFileNoExt, [{erts, code:root_dir()}]).

build_image(Dir) ->
    %% build docker image
    BuildScript = filename:join(Dir, "Dockerfile"),
    Dockerfile =
        "FROM ubuntu:20.04 as runner\n"
        "WORKDIR /opt/lambda\n"
        "COPY lambda.tar.gz /tmp\n"
        "RUN tar -zxvf /tmp/lambda.tar.gz -C /opt/lambda\n"
        "ENTRYPOINT [\"/opt/lambda/erts-" ++ erlang:system_info(version) ++ "/bin/dyn_erl\", \"-boot\", \"/opt/lambda/releases/1.0.0/start\"]\n",
    ok = file:write_file(BuildScript, Dockerfile),
    Output = os:cmd("docker build -t lambda " ++ Dir),
    ct:pal("Build result: ~s~n", [Output]).

docker(Config) when is_list(Config) ->
    case os:find_executable("docker") of
        false ->
            {skip, "Docker not found"};
        Docker ->
            PrivDir = proplists:get_value(priv_dir, Config),
            build_release(PrivDir),
            build_image(PrivDir),

            {ok, Peer, Node} = peer:start_link(#{name => ?CT_PEER_NAME(),
                exec => {Docker, ["run", "-i", "lambda"]}, connection => standard_io}),
            ?assertEqual(Node, peer:call(Peer, erlang, node, [])),
            peer:stop(Peer)
    end.

attached() ->
    [{doc, "Test that it is possible to start a peer node using run_erl aka attached"}].

attached(Config) ->
    attached_test(false, Config).

attached_cntrl_channel_handler_crash() ->
    [{doc, "Test that peer node is halted if peer control channel handler process crashes and peer node is attached"}].

attached_cntrl_channel_handler_crash(Config) ->
    attached_test(true, Config).

attached_test(CrashConnectionHandler, Config) ->
    RunErl = os:find_executable("run_erl"),
    [throw({skip, "Could not find run_erl"}) || RunErl =:= false],
    Erl = string:split(ct:get_progname()," ",all),
    RunErlDir = filename:join(proplists:get_value(priv_dir, Config),?FUNCTION_NAME),
    filelib:ensure_path(RunErlDir),
    Connection = proplists:get_value(connection, Config),
    Conn = if Connection =:= undefined -> #{ name => ?CT_PEER_NAME() };
              true ->
                   case CrashConnectionHandler of
                       false ->
                           #{ connection => Connection };
                       true ->
                           #{name => ?CT_PEER_NAME(),
                             connection => Connection}
                   end
           end,
    try peer:start(
           Conn#{
                 exec => {RunErl, Erl},
                 detached => false,
                 post_process_args =>
                     fun(Args) ->
                             [RunErlDir ++ "/", RunErlDir,
                              lists:flatten(lists:join(" ",[[$',A,$'] || A <- Args]))]
                     end
                }) of
        {ok, Peer, Node} when Connection =:= undefined; Connection =:= 0 ->
            case CrashConnectionHandler of
                false -> peer:stop(Peer);
                true -> cntrl_channel_handler_crash_test(Node)
            end
    catch error:{detached,_} when Connection =:= standard_io ->
            ok
    end.

cntrl_channel_handler_crash() ->
    [{doc, "Test that peer node is halted if peer control channel handler process crashes"}].

cntrl_channel_handler_crash(Config) ->
    NameOpts = #{name => ?CT_PEER_NAME()},
    Opts = case proplists:get_value(connection, Config) of
               undefined -> NameOpts;
               Conn -> NameOpts#{connection => Conn}
           end,
    {ok, _Peer, Node} = peer:start_link(Opts),
    cntrl_channel_handler_crash_test(Node).

cntrl_channel_handler_crash_old_release() ->
    [{doc, "Test that peer node running an old release is halted if peer control channel handler process crashes"}].

cntrl_channel_handler_crash_old_release(Config) ->
    NameOpts = #{name => ?CT_PEER_NAME()},
    Opts = case proplists:get_value(connection, Config) of
               undefined -> NameOpts;
               Conn -> NameOpts#{connection => Conn}
           end,
    PrivDir = proplists:get_value(priv_dir, Config),
    OldRel = integer_to_list(list_to_integer(erlang:system_info(otp_release)) - 2),
    case ?CT_PEER_REL(Opts, OldRel, PrivDir) of
        not_available ->
            {skip, "No OTP " ++ OldRel ++ " installation found"};
        {ok, _Peer, Node} ->
            cntrl_channel_handler_crash_test(Node)
    end.

cntrl_channel_handler_crash_test(Node) ->
    true = monitor_node(Node, true),
    ChkStck = fun ChkStck (_Pid, []) ->
                      ok;
                  ChkStck (Pid, [{peer, io_server_loop, _, _} | _]) ->
                      throw(Pid);
                  ChkStck (Pid, [{peer, origin_link, _, _} | _]) ->
                      throw(Pid);
                  ChkStck (Pid, [_SF|SFs]) ->
                      ChkStck(Pid, SFs)
              end,
    ChkConnHandler = fun (undefined) ->
                             ok;
                         (Pid) when is_pid(Pid) ->
                             case erpc:call(Node, erlang, process_info,
                                            [Pid, current_stacktrace]) of
                                 {current_stacktrace, STrace} ->
                                     ChkStck(Pid, STrace);
                                 _ ->
                                     ok
                             end
                     end,
    ConnHandler = try
                      ChkConnHandler(erpc:call(Node, erlang, whereis, [user])),
                      lists:foreach(fun (Pid) ->
                                            ChkConnHandler(Pid)
                                    end, erpc:call(Node, erlang, processes, [])),
                      error(no_cntrl_channel_handler_found)
                  catch
                      throw:Pid when is_pid(Pid) -> Pid
                  end,
    PeerSup = erpc:call(Node, erlang, whereis, [peer_supervision]),
    ct:log("peer_supervision state: ~p~n", [erpc:call(Node, sys, get_state, [PeerSup])]),
    {links, Links} = erpc:call(Node, erlang, process_info, [PeerSup, links]),
    true = lists:member(ConnHandler, Links),
    ok = erpc:cast(Node, erlang, exit, [ConnHandler, kill]),
    receive
        {nodedown, Node} ->
            ok
    after
        5000 ->
            ct:fail(peer_did_not_halt)
    end.
