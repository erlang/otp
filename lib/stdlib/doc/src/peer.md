Start and control linked Erlang nodes.

This module provides functions for starting linked Erlang nodes. The node
spawning new nodes is called _origin_, and newly started nodes are _peer_ nodes,
or peers. A peer node automatically terminates when it loses the _control
connection_ to the origin. This connection could be an Erlang distribution
connection, or an alternative - TCP or standard I/O. The alternative connection
provides a way to execute remote procedure calls even when Erlang Distribution
is not available, allowing to test the distribution itself.

Peer node terminal input/output is relayed through the origin. If a standard I/O
alternative connection is requested, console output also goes via the origin,
allowing debugging of node startup and boot script execution (see
[`-init_debug`](`e:erts:erl_cmd.md#init_debug`)). File I/O is not redirected,
contrary to `m:slave` behaviour.

The peer node can start on the same or a different host (via `ssh`) or in a
separate container (for example Docker). When the peer starts on the same host
as the origin, it inherits the current directory and environment variables from
the origin.

> #### Note {: .info }
>
> This module is designed to facilitate multi-node testing with Common Test. Use
> the `?CT_PEER()` macro to start a linked peer node according to Common Test
> conventions: crash dumps written to specific location, node name prefixed with
> module name, calling function, and origin OS process ID). Use `random_name/1`
> to create sufficiently unique node names if you need more control.
>
> A peer node started without alternative connection behaves similarly to
> `m:slave`. When an alternative connection is requested, the behaviour is
> similar to `test_server:start_node(Name, peer, Args).`

## Example

The following example implements a test suite starting extra Erlang nodes. It
employs a number of techniques to speed up testing and reliably shut down peer
nodes:

- peers start linked to test runner process. If the test case fails, the peer
  node is stopped automatically, leaving no rogue nodes running in the
  background
- arguments used to start the peer are saved in the control process state for
  manual analysis. If the test case fails, the CRASH REPORT contains these
  arguments
- multiple test cases can run concurrently speeding up overall testing process,
  peer node names are unique even when there are multiple instances of the same
  test suite running in parallel

```erlang
-module(my_SUITE).
-behaviour(ct_suite).
-export([all/0, groups/0]).
-export([basic/1, args/1, named/1, restart_node/1, multi_node/1]).

-include_lib("common_test/include/ct.hrl").

groups() ->
    [{quick, [parallel],
        [basic, args, named, restart_node, multi_node]}].

all() ->
    [{group, quick}].

basic(Config) when is_list(Config) ->
    {ok, Peer, _Node} = ?CT_PEER(),
    peer:stop(Peer).

args(Config) when is_list(Config) ->
    %% specify additional arguments to the new node
    {ok, Peer, _Node} = ?CT_PEER(["-emu_flavor", "smp"]),
    peer:stop(Peer).

named(Config) when is_list(Config) ->
    %% pass test case name down to function starting nodes
    Peer = start_node_impl(named_test),
    peer:stop(Peer).

start_node_impl(ActualTestCase) ->
    {ok, Peer, Node} = ?CT_PEER(#{name => ?CT_PEER_NAME(ActualTestCase)}),
    %% extra setup needed for multiple test cases
    ok = rpc:call(Node, application, set_env, [kernel, key, value]),
    Peer.

restart_node(Config) when is_list(Config) ->
    Name = ?CT_PEER_NAME(),
    {ok, Peer, Node} = ?CT_PEER(#{name => Name}),
    peer:stop(Peer),
    %% restart the node with the same name as before
    {ok, Peer2, Node} = ?CT_PEER(#{name => Name, args => ["+fnl"]}),
    peer:stop(Peer2).
```

The next example demonstrates how to start multiple nodes concurrently:

```erlang
multi_node(Config) when is_list(Config) ->
    Peers = [?CT_PEER(#{wait_boot => {self(), tag}})
        || _ <- lists:seq(1, 4)],
    %% wait for all nodes to complete boot process, get their names:
    _Nodes = [receive {tag, {started, Node, Peer}} -> Node end
        || {ok, Peer} <- Peers],
    [peer:stop(Peer) || {ok, Peer} <- Peers].
```

Start a peer on a different host. Requires `ssh` key-based authentication set
up, allowing "another_host" connection without password prompt.

```erlang
Ssh = os:find_executable("ssh"),
peer:start_link(#{exec => {Ssh, ["another_host", "erl"]},
    connection => standard_io}),
```

The following Common Test case demonstrates Docker integration, starting two
containers with hostnames "one" and "two". In this example Erlang nodes running
inside containers form an Erlang cluster.

```erlang
docker(Config) when is_list(Config) ->
    Docker = os:find_executable("docker"),
    PrivDir = proplists:get_value(priv_dir, Config),
    build_release(PrivDir),
    build_image(PrivDir),

    %% start two Docker containers
    {ok, Peer, Node} = peer:start_link(#{name => lambda,
        connection => standard_io,
        exec => {Docker, ["run", "-h", "one", "-i", "lambda"]}}),
    {ok, Peer2, Node2} = peer:start_link(#{name => lambda,
        connection => standard_io,
        exec => {Docker, ["run", "-h", "two", "-i", "lambda"]}}),

    %% find IP address of the second node using alternative connection RPC
    {ok, Ips} = peer:call(Peer2, inet, getifaddrs, []),
    {"eth0", Eth0} = lists:keyfind("eth0", 1, Ips),
    {addr, Ip} = lists:keyfind(addr, 1, Eth0),

    %% make first node to discover second one
    ok = peer:call(Peer, inet_db, set_lookup, [[file]]),
    ok = peer:call(Peer, inet_db, add_host, [Ip, ["two"]]),

    %% join a cluster
    true = peer:call(Peer, net_kernel, connect_node, [Node2]),
    %% verify that second peer node has only the first node visible
    [Node] = peer:call(Peer2, erlang, nodes, []),

    %% stop peers, causing containers to also stop
    peer:stop(Peer2),
    peer:stop(Peer).

build_release(Dir) ->
    %% load sasl.app file, otherwise application:get_key will fail
    application:load(sasl),
    %% create *.rel - release file
    RelFile = filename:join(Dir, "lambda.rel"),
    Release = {release, {"lambda", "1.0.0"},
        {erts, erlang:system_info(version)},
        [{App, begin {ok, Vsn} = application:get_key(App, vsn), Vsn end}
            || App <- [kernel, stdlib, sasl]]},
    ok = file:write_file(RelFile, list_to_binary(lists:flatten(
        io_lib:format("~tp.", [Release])))),
    RelFileNoExt = filename:join(Dir, "lambda"),

    %% create boot script
    {ok, systools_make, []} = systools:make_script(RelFileNoExt,
        [silent, {outdir, Dir}]),
    %% package release into *.tar.gz
    ok = systools:make_tar(RelFileNoExt, [{erts, code:root_dir()}]).

build_image(Dir) ->
    %% Create Dockerfile example, working only for Ubuntu 20.04
    %% Expose port 4445, and make Erlang distribution to listen
    %%  on this port, and connect to it without EPMD
    %% Set cookie on both nodes to be the same.
    BuildScript = filename:join(Dir, "Dockerfile"),
    Dockerfile =
      "FROM ubuntu:20.04 as runner\n"
      "EXPOSE 4445\n"
      "WORKDIR /opt/lambda\n"
      "COPY lambda.tar.gz /tmp\n"
      "RUN tar -zxvf /tmp/lambda.tar.gz -C /opt/lambda\n"
      "ENTRYPOINT [\"/opt/lambda/erts-" ++ erlang:system_info(version) ++
      "/bin/dyn_erl\", \"-boot\", \"/opt/lambda/releases/1.0.0/start\","
      " \"-kernel\", \"inet_dist_listen_min\", \"4445\","
      " \"-erl_epmd_port\", \"4445\","
      " \"-setcookie\", \"secret\"]\n",
    ok = file:write_file(BuildScript, Dockerfile),
    os:cmd("docker build -t lambda " ++ Dir).
```
