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
    init_per_group/2,
    end_per_group/2
]).

%% Test cases
-export([
    dist/0, dist/1,
    peer_down_crash/0, peer_down_crash/1,
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
    init_debug/0, init_debug/1,
    io_redirect/0, io_redirect/1,
    multi_node/0, multi_node/1,
    dist_up_down/0, dist_up_down/1,
    duplicate_name/0, duplicate_name/1,
    old_release/0, old_release/1,
    ssh/0, ssh/1,
    docker/0, docker/1
]).

suite() ->
    [{timetrap, {minutes, 1}}].

alternative() ->
    [basic, peer_states, cast, detached, dyn_peer, stop_peer, io_redirect,
        multi_node, duplicate_name].

groups() ->
    [
        {dist, [parallel], [errors, dist, peer_down_crash, peer_down_continue, peer_down_boot,
            dist_io_redirect, dist_up_down, dist_localhost]},
        {tcp, [parallel], alternative()},
        {standard_io, [parallel], [init_debug | alternative()]},
        {compatibility, [parallel], [old_release]},
        {remote, [parallel], [ssh]}
    ].

all() ->
    [{group, dist}, {group, tcp}, {group, standard_io}, {group, compatibility}, {group, remote}].

init_per_group(remote, Config) ->
    %% check that SSH can connect to localhost, skip the test if not
    try os:cmd("ssh localhost echo ok") of
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
    {ok, Peer, Node} = peer:start_link(#{name => peer:random_name(?FUNCTION_NAME), peer_down => crash}),
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

peer_down_continue() ->
    [{doc, "Tests peer_down handling for continue setting"}].

peer_down_continue(Config) when is_list(Config) ->
    {ok, Peer, Node} = peer:start_link(#{name => peer:random_name(?FUNCTION_NAME), peer_down => continue}),
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
        args => ["-eval", "io:format(\"out\")."]}),
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

%% -------------------------------------------------------------------
%% Compatibility: old releases
old_release() ->
    [{doc, "Verity running with previous OTP release"}].

old_release(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    %% don't expect OTP 10 to be available
    ?assertEqual(not_available, ?CT_PEER([], "10", PrivDir)),
    PrevRel = integer_to_list(list_to_integer(erlang:system_info(otp_release)) - 2),
    case ?CT_PEER([], PrevRel, PrivDir) of
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
            {ok, Peer, _Node} = peer:start_link(#{exec => {SshPath, ["localhost", ErlPath]},
                connection => standard_io, name => Name,
                args => ["-env", "BINDIR", os:getenv("BINDIR")], host => "localhost"}),
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
