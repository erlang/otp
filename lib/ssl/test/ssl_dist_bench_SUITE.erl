%%%-------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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
-module(ssl_dist_bench_SUITE).

-include_lib("common_test/include/ct_event.hrl").
-include_lib("public_key/include/public_key.hrl").

%% CT meta
-export([suite/0, all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export(
   [setup/1,
    roundtrip/1,
    throughput_1024/1,
    throughput_4096/1,
    throughput_16384/1,
    throughput_65536/1,
    throughput_262144/1,
    throughput_1048576/1]).

%% Debug
-export([payload/1]).

%%%-------------------------------------------------------------------

suite() -> [{ct_hooks, [{ts_install_cth, [{nodenames, 2}]}]}].

all() -> [{group, ssl}, {group, plain}].

groups() ->
    [{ssl, all_groups()},
     {plain, all_groups()},
     %%
     {setup, [{repeat, 1}], [setup]},
     {roundtrip, [{repeat, 1}], [roundtrip]},
     {throughput, [{repeat, 1}],
      [throughput_1024,
       throughput_4096,
       throughput_16384,
       throughput_65536,
       throughput_262144,
       throughput_1048576]}].

all_groups() ->
    [{group, setup},
     {group, roundtrip},
     {group, throughput}].

init_per_suite(Config) ->
    Digest = sha1,
    ECCurve = secp521r1,
    TLSVersion = 'tlsv1.2',
    TLSCipher = {ecdhe_ecdsa,aes_128_cbc,sha256,sha256},
    %%
    Node = node(),
    try
        Node =/= nonode@nohost orelse
            throw({skipped,"Node not distributed"}),
        {supported, SSLVersions} =
            lists:keyfind(supported, 1, ssl:versions()),
        lists:member(TLSVersion, SSLVersions) orelse
            throw(
              {skipped,
               "SSL does not support " ++ term_to_string(TLSVersion)}),
        lists:member(ECCurve, ssl:eccs(TLSVersion)) orelse
            throw(
              {skipped,
               "SSL does not support " ++ term_to_string(ECCurve)}),
        lists:member(TLSCipher, ssl:cipher_suites()) orelse
            throw(
              {skipped,
               "SSL does not support " ++ term_to_string(TLSCipher)})
    of
        _ ->
            PrivDir = proplists:get_value(priv_dir, Config),
            %%
            [_, HostA] = string:split(atom_to_list(Node), "@"),
            NodeAName = ?MODULE_STRING ++ "_node_a",
            NodeAString = NodeAName ++ "@" ++ HostA,
            NodeAConfFile = filename:join(PrivDir, NodeAString ++ ".conf"),
            NodeA = list_to_atom(NodeAString),
            %%
            ServerNode = ssl_bench_test_lib:setup(dist_server),
            [_, HostB] = string:split(atom_to_list(ServerNode), "@"),
            NodeBName = ?MODULE_STRING ++ "_node_b",
            NodeBString = NodeBName ++ "@" ++ HostB,
            NodeBConfFile = filename:join(PrivDir, NodeBString ++ ".conf"),
            NodeB = list_to_atom(NodeBString),
            %%
            CertOptions =
                [{digest, Digest},
                 {key, {namedCurve, ECCurve}}],
            RootCert =
                public_key:pkix_test_root_cert(
                  ?MODULE_STRING ++ " ROOT CA", CertOptions),
            SSLConf =
                [{verify, verify_peer},
                 {versions, [TLSVersion]},
                 {ciphers, [TLSCipher]}],
            %%
            write_node_conf(
              NodeAConfFile, NodeA,
              [{fail_if_no_peer_cert, true} | SSLConf], SSLConf,
              CertOptions, RootCert),
            write_node_conf(
              NodeBConfFile, NodeB,
              [{fail_if_no_peer_cert, true} | SSLConf], SSLConf,
              CertOptions, RootCert),
            %%
            [{node_a_name, NodeAName},
             {node_a, NodeA},
             {node_a_dist_args,
              "-proto_dist inet_tls "
              "-ssl_dist_optfile " ++ NodeAConfFile ++ " "},
             {node_b_name, NodeBName},
             {node_b, NodeB},
             {node_b_dist_args,
              "-proto_dist inet_tls "
              "-ssl_dist_optfile " ++ NodeBConfFile ++ " "},
             {server_node, ServerNode}
             |Config]
    catch
        throw:Result ->
            Result
    end.

end_per_suite(Config) ->
    ServerNode = proplists:get_value(server_node, Config),
    slave:stop(ServerNode).

init_per_group(ssl, Config) ->
    [{ssl_dist, true}, {ssl_dist_prefix, "SSL"}|Config];
init_per_group(plain, Config) ->
    [{ssl_dist, false}, {ssl_dist_prefix, "Plain"}|Config];
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_Func, Conf) ->
    Conf.

end_per_testcase(_Func, _Conf) ->
    ok.

-define(COUNT, 400).

%%%-------------------------------------------------------------------
%%% CommonTest API helpers

write_node_conf(
  ConfFile, Node, ServerConf, ClientConf, CertOptions, RootCert) ->
    Conf =
        public_key:pkix_test_data(
          #{root => RootCert,
            peer =>
                [{extensions,
                  [#'Extension'{
                      extnID = ?'id-ce-subjectAltName',
                      extnValue = [{dNSName, atom_to_list(Node)}],
                      critical = false}]} | CertOptions]}),
    NodeConf =
        [{server, ServerConf ++ Conf}, {client, ClientConf ++ Conf}],
    {ok, Fd} = file:open(ConfFile, [write]),
    ok = file:change_mode(ConfFile, 8#400),
    io:format(Fd, "~p.~n", [NodeConf]),
    ok = file:close(Fd).


%%%-------------------------------------------------------------------
%%% Test cases

%%-----------------------
%% Connection setup speed

setup(Config) ->
    run_nodepair_test(fun setup/5, Config).

setup(A, B, Prefix, HA, HB) ->
    Rounds = 10,
    [] = ssl_apply(HA, erlang, nodes, []),
    [] = ssl_apply(HB, erlang, nodes, []),
    {SetupTime, CycleTime} =
        ssl_apply(HA, fun () -> setup_runner(A, B, Rounds) end),
    [] = ssl_apply(HA, erlang, nodes, []),
    [] = ssl_apply(HB, erlang, nodes, []),
    SetupSpeed = round((Rounds*1000000*1000) / SetupTime),
    CycleSpeed = round((Rounds*1000000*1000) / CycleTime),
    _ = report(Prefix++" Setup", SetupSpeed, "setups/1000s"),
    report(Prefix++" Setup Cycle", CycleSpeed, "cycles/1000s").

%% Runs on node A against rex in node B
setup_runner(A, B, Rounds) ->
    StartTime = start_time(),
    SetupTime = setup_loop(A, B, 0, Rounds),
    {microseconds(SetupTime), microseconds(elapsed_time(StartTime))}.

setup_loop(_A, _B, T, 0) ->
    T;
setup_loop(A, B, T, N) ->
    StartTime = start_time(),
    [A] = rpc:block_call(B, erlang, nodes, []),
    Time = elapsed_time(StartTime),
    [B] = erlang:nodes(),
    Mref = erlang:monitor(process, {rex,B}),
    true = net_kernel:disconnect(B),
    receive
        {'DOWN',Mref,process,_,_} ->
            [] = erlang:nodes(),
            setup_loop(A, B, Time + T, N - 1)
    end.


%%----------------
%% Roundtrip speed

roundtrip(Config) ->
    run_nodepair_test(fun roundtrip/5, Config).

roundtrip(A, B, Prefix, HA, HB) ->
    Rounds = 40000,
    [] = ssl_apply(HA, erlang, nodes, []),
    [] = ssl_apply(HB, erlang, nodes, []),
    Time = ssl_apply(HA, fun () -> roundtrip_runner(A, B, Rounds) end),
    [B] = ssl_apply(HA, erlang, nodes, []),
    [A] = ssl_apply(HB, erlang, nodes, []),
    Speed = round((Rounds*1000000) / Time),
    report(Prefix++" Roundtrip", Speed, "pings/s").

%% Runs on node A and spawns a server on node B
roundtrip_runner(A, B, Rounds) ->
    ClientPid = self(),
    [A] = rpc:call(B, erlang, nodes, []),
    ServerPid =
        erlang:spawn(
          B,
          fun () -> roundtrip_server(ClientPid, Rounds) end),
    ServerMon = erlang:monitor(process, ServerPid),
    microseconds(
      roundtrip_client(ServerPid, ServerMon, start_time(), Rounds)).

roundtrip_server(_Pid, 0) ->
    ok;
roundtrip_server(Pid, N) ->
    receive
        N ->
            Pid ! N,
            roundtrip_server(Pid, N-1)
    end.

roundtrip_client(_Pid, Mon, StartTime, 0) ->
    Time = elapsed_time(StartTime),
    receive
        {'DOWN', Mon, _, _, normal} ->
            Time;
        {'DOWN', Mon, _, _, Other} ->
            exit(Other)
    end;
roundtrip_client(Pid, Mon, StartTime, N) ->
    Pid ! N,
    receive
        N ->
            roundtrip_client(Pid, Mon, StartTime, N - 1)
    end.


%%-----------------
%% Throughput speed

throughput_1024(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 100000, 1024)
      end, Config).

throughput_4096(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 50000, 4096)
      end, Config).

throughput_16384(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 10000, 16384)
      end, Config).

throughput_65536(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 2000, 65536)
      end, Config).

throughput_262144(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 500, 262144)
      end, Config).

throughput_1048576(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 200, 1048576)
      end, Config).

throughput(A, B, Prefix, HA, HB, Packets, Size) ->
    [] = ssl_apply(HA, erlang, nodes, []),
    [] = ssl_apply(HB, erlang, nodes, []),
    Time =
        ssl_apply(HA, fun () -> throughput_runner(A, B, Packets, Size) end),
    [B] = ssl_apply(HA, erlang, nodes, []),
    [A] = ssl_apply(HB, erlang, nodes, []),
    Speed = round((Packets*Size*1000000) / (1024*Time)),
    report(Prefix++" Throughput_"++integer_to_list(Size), Speed, "kB/s").

%% Runs on node A and spawns a server on node B
throughput_runner(A, B, Rounds, Size) ->
    Payload = payload(Size),
    ClientPid = self(),
    [A] = rpc:call(B, erlang, nodes, []),
    ServerPid =
        erlang:spawn(
          B,
          fun () -> throughput_server(ClientPid, Rounds) end),
    ServerMon = erlang:monitor(process, ServerPid),
    microseconds(
      throughput_client(
        ServerPid, ServerMon, Payload, start_time(), Rounds)).

throughput_server(_Pid, 0) ->
    ok;
throughput_server(Pid, N) ->
    receive
        [N|_] ->
            throughput_server(Pid, N-1)
    end.

throughput_client(_Pid, Mon, _Payload, StartTime, 0) ->
    receive
        {'DOWN', Mon, _, _, normal} ->
            elapsed_time(StartTime);
        {'DOWN', Mon, _, _, Other} ->
            exit(Other)
    end;
throughput_client(Pid, Mon, Payload, StartTime, N) ->
    Pid ! [N|Payload],
    throughput_client(Pid, Mon, Payload, StartTime, N - 1).

%%%-------------------------------------------------------------------
%%% Test cases helpers

run_nodepair_test(TestFun, Config) ->
    A = proplists:get_value(node_a, Config),
    B = proplists:get_value(node_b, Config),
    Prefix = proplists:get_value(ssl_dist_prefix, Config),
    HA = start_ssl_node_a(Config),
    HB = start_ssl_node_b(Config),
    try TestFun(A, B, Prefix, HA, HB)
    after
        stop_ssl_node_a(HA),
        stop_ssl_node_b(HB, Config),
        ok
    end.

ssl_apply(Handle, M, F, Args) ->
    case ssl_dist_test_lib:apply_on_ssl_node(Handle, M, F, Args) of
        {'EXIT',Reason} ->
            error(Reason);
        Result ->
            Result
    end.

ssl_apply(Handle, Fun) ->
    case ssl_dist_test_lib:apply_on_ssl_node(Handle, Fun) of
        {'EXIT',Reason} ->
            error(Reason);
        Result ->
            Result
    end.

start_ssl_node_a(Config) ->
    Name = proplists:get_value(node_a_name, Config),
    Args = get_node_args(node_a_dist_args, Config),
    ssl_dist_test_lib:start_ssl_node(Name, Args).

start_ssl_node_b(Config) ->
    Name = proplists:get_value(node_b_name, Config),
    Args = get_node_args(node_b_dist_args, Config),
    ServerNode = proplists:get_value(server_node, Config),
    rpc:call(
      ServerNode, ssl_dist_test_lib, start_ssl_node, [Name, Args]).

stop_ssl_node_a(HA) ->
    ssl_dist_test_lib:stop_ssl_node(HA).

stop_ssl_node_b(HB, Config) ->
    ServerNode = proplists:get_value(server_node, Config),
    rpc:call(ServerNode, ssl_dist_test_lib, stop_ssl_node, [HB]).

get_node_args(Tag, Config) ->
    case proplists:get_value(ssl_dist, Config) of
        true ->
            proplists:get_value(Tag, Config);
        false ->
            ""
    end.



payload(Size) ->
    iolist_to_binary(
      [case Size bsr 8 of
           0 ->
               [];
           Blocks ->
               payload(Blocks, create_binary(256))
       end | create_binary(Size band 255)]).
%%
payload(0, _) ->
    [];
payload(Blocks, Block) ->
    Half = payload(Blocks bsr 1, Block),
    [Half, Half |
     if
         Blocks band 1 =:= 1 ->
             Block;
         true ->
             []
     end].

create_binary(Size) ->
    create_binary(Size, <<>>).
%%
create_binary(0, Bin) ->
    Bin;
create_binary(Size, Bin) ->
    NextSize = Size - 1,
    create_binary(NextSize, <<Bin/binary, NextSize>>).

start_time() ->
    erlang:system_time().

elapsed_time(StartTime) ->
    erlang:system_time() - StartTime.

microseconds(Time) ->
    erlang:convert_time_unit(Time, native, microsecond).

report(Name, Value, Unit) ->
    ct:pal("~s: ~w ~s", [Name, Value, Unit]),
    ct_event:notify(
      #event{
         name = benchmark_data,
         data = [{value, Value}, {suite, "ssl_dist"}, {name, Name}]}),
    {comment, term_to_string(Value) ++ " " ++ Unit}.

term_to_string(Term) ->
    unicode:characters_to_list(
      io_lib:write(Term, [{encoding, unicode}])).
