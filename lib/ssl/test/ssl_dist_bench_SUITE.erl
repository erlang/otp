%%%-------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2019. All Rights Reserved.
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
    sched_utilization/1,
    throughput_0/1,
    throughput_64/1,
    throughput_1024/1,
    throughput_4096/1,
    throughput_16384/1,
    throughput_65536/1,
    throughput_262144/1,
    throughput_1048576/1]).

%% Debug
-export([payload/1, roundtrip_runner/3, setup_runner/3, throughput_runner/4]).

%%%-------------------------------------------------------------------

suite() -> [{ct_hooks, [{ts_install_cth, [{nodenames, 2}]}]}].

all() ->
    [{group, ssl},
     {group, crypto},
     {group, plain}].

groups() ->
    [{ssl, all_groups()},
     {crypto, all_groups()},
     {plain, all_groups()},
     %%
     {setup, [{repeat, 1}], [setup]},
     {roundtrip, [{repeat, 1}], [roundtrip]},
     {sched_utilization,[{repeat, 1}], [sched_utilization]},
     {throughput, [{repeat, 1}],
      [throughput_0,
       throughput_64,
       throughput_1024,
       throughput_4096,
       throughput_16384,
       throughput_65536,
       throughput_262144,
       throughput_1048576]}].

all_groups() ->
    [{group, setup},
     {group, roundtrip},
     {group, throughput},
     {group, sched_utilization}
    ].

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
        verify_node_src_addr(),
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
            [_, HostA] = split_node(Node),
            NodeAName = ?MODULE_STRING ++ "_node_a",
            NodeAString = NodeAName ++ "@" ++ HostA,
            NodeAConfFile = filename:join(PrivDir, NodeAString ++ ".conf"),
            NodeA = list_to_atom(NodeAString),
            %%
            ServerNode = ssl_bench_test_lib:setup(dist_server),
            [_, HostB] = split_node(ServerNode),
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
            ServerConf =
                [{fail_if_no_peer_cert, true},
                 {verify_fun,
                  {fun inet_tls_dist:verify_client/3,[]}}
                 | SSLConf],
            ClientConf = SSLConf,
            %%
            write_node_conf(
              NodeAConfFile, NodeA, ServerConf, ClientConf,
              CertOptions, RootCert),
            write_node_conf(
              NodeBConfFile, NodeB, ServerConf, ClientConf,
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
init_per_group(crypto, Config) ->
    [{ssl_dist, false}, {ssl_dist_prefix, "Crypto"},
     {ssl_dist_args,
      "-proto_dist inet_crypto"}
     |Config];
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

verify_node_src_addr() ->
    Msg = "Hello, world!",
    {ok,Host} = inet:gethostname(),
    {ok,DstAddr} = inet:getaddr(Host, inet),
    {ok,Socket} = gen_udp:open(0, [{active,false}]),
    {ok,Port} = inet:port(Socket),
    ok = gen_udp:send(Socket, DstAddr, Port, Msg),
    case gen_udp:recv(Socket, length(Msg) + 1, 1000) of
        {ok,{DstAddr,Port,Msg}} ->
            ok;
        {ok,{SrcAddr,Port,Msg}} ->
            throw({skipped,
                   "Src and dst address mismatch: " ++
                       term_to_string(SrcAddr) ++ " =:= " ++
                       term_to_string(DstAddr)});
        Weird ->
            error(Weird)
    end.

write_node_conf(
  ConfFile, Node, ServerConf, ClientConf, CertOptions, RootCert) ->
    [Name,Host] = split_node(Node),
    Conf =
        public_key:pkix_test_data(
          #{root => RootCert,
            peer =>
                [{extensions,
                  [
                   #'Extension'{
                      extnID = ?'id-ce-subjectAltName',
                      extnValue = [{dNSName, Host}],
                      critical = true},
                   #'Extension'{
                      extnID = ?'id-ce-subjectAltName',
                      extnValue =
                          [{directoryName,
                            {rdnSequence,
                             [[#'AttributeTypeAndValue'{
                                  type = ?'id-at-commonName',
                                  value =
                                      {utf8String,
                                       unicode:characters_to_binary(
                                         Name, utf8)
                                      }
                                 }]]}}],
                      critical = true}
                  ]} | CertOptions]}),
    NodeConf =
        [{server, ServerConf ++ Conf}, {client, ClientConf ++ Conf}],
    {ok, Fd} = file:open(ConfFile, [write]),
    ok = file:change_mode(ConfFile, 8#400),
    io:format(Fd, "~p.~n", [NodeConf]),
    ok = file:close(Fd).

split_node(Node) ->
    string:split(atom_to_list(Node), "@").

%%%-------------------------------------------------------------------
%%% Test cases

%%-----------------------
%% Connection setup speed

setup(Config) ->
    run_nodepair_test(fun setup/5, Config).

setup(A, B, Prefix, HA, HB) ->
    Rounds = 50,
    [] = ssl_apply(HA, erlang, nodes, []),
    [] = ssl_apply(HB, erlang, nodes, []),
    {SetupTime, CycleTime} =
        ssl_apply(HA, fun () -> setup_runner(A, B, Rounds) end),
    ok = ssl_apply(HB, fun () -> setup_wait_nodedown(A, 10000) end),
    %% [] = ssl_apply(HA, erlang, nodes, []),
    %% [] = ssl_apply(HB, erlang, nodes, []),
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
    [N,A] = [N|rpc:block_call(B, erlang, nodes, [])],
    Time = elapsed_time(StartTime),
    [N,B] = [N|erlang:nodes()],
    Mref = erlang:monitor(process, {rex,B}),
    true = net_kernel:disconnect(B),
    receive
        {'DOWN',Mref,process,_,_} ->
            [] = erlang:nodes(),
            setup_loop(A, B, Time + T, N - 1)
    end.

setup_wait_nodedown(A, Time) ->
    ok = net_kernel:monitor_nodes(true),
    case nodes() of
        [] ->
            ok;
        [A] ->
            receive
                {nodedown,A} ->
                    ok;
                Unexpected ->
                    {error,{unexpected,Unexpected}}
            after Time ->
                    {error,timeout}
            end
    end.


%%----------------
%% Roundtrip speed

roundtrip(Config) ->
    run_nodepair_test(fun roundtrip/5, Config).

roundtrip(A, B, Prefix, HA, HB) ->
    Rounds = 40000,
    [] = ssl_apply(HA, erlang, nodes, []),
    [] = ssl_apply(HB, erlang, nodes, []),
    ok = ssl_apply(HA, net_kernel, allow, [[B]]),
    ok = ssl_apply(HB, net_kernel, allow, [[A]]),
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

%%---------------------------------------
%% Scheduler utilization at constant load


sched_utilization(Config) ->
    run_nodepair_test(
      fun(A, B, Prefix, HA, HB) ->
              sched_utilization(A, B, Prefix, HA, HB, proplists:get_value(ssl_dist, Config))
      end, Config).

sched_utilization(A, B, Prefix, HA, HB, SSL) ->
    [] = ssl_apply(HA, erlang, nodes, []),
    [] = ssl_apply(HB, erlang, nodes, []),
    {ClientMsacc, ServerMsacc, Msgs} =
        ssl_apply(HA, fun () -> sched_util_runner(A, B, SSL) end),
    [B] = ssl_apply(HA, erlang, nodes, []),
    [A] = ssl_apply(HB, erlang, nodes, []),
    msacc:print(ClientMsacc),
    msacc:print(ServerMsacc),
    ct:pal("Got ~p busy_dist_port msgs",[length(Msgs)]),
    ct:log("Stats of B from A: ~p",
           [ssl_apply(HA, net_kernel, node_info, [B])]),
    ct:log("Stats of A from B: ~p",
           [ssl_apply(HB, net_kernel, node_info, [A])]),
    SchedUtilClient =
        round(10000 * msacc:stats(system_runtime,ClientMsacc) /
                  msacc:stats(system_realtime,ClientMsacc)),
    SchedUtilServer =
        round(10000 * msacc:stats(system_runtime,ServerMsacc) /
                  msacc:stats(system_realtime,ServerMsacc)),
    Verdict =
        case Msgs of
            [] ->
                "";
            _ ->
                " ???"
        end,
    {comment, ClientComment} =
        report(Prefix ++ " Sched Utilization Client" ++ Verdict,
               SchedUtilClient, "/100 %" ++ Verdict),
    {comment, ServerComment} =
        report(Prefix++" Sched Utilization Server" ++ Verdict,
               SchedUtilServer, "/100 %" ++ Verdict),
    {comment, "Client " ++ ClientComment ++ ", Server " ++ ServerComment}.

%% Runs on node A and spawns a server on node B
%% We want to avoid getting busy_dist_port as it hides the true SU usage
%% of the receiver and sender.
sched_util_runner(A, B, true) ->
    sched_util_runner(A, B, 250);
sched_util_runner(A, B, false) ->
    sched_util_runner(A, B, 250);
sched_util_runner(A, B, Senders) ->
    Payload = payload(5),
    [A] = rpc:call(B, erlang, nodes, []),
    ServerPids =
        [erlang:spawn_link(
           B, fun () -> throughput_server() end)
         || _ <- lists:seq(1, Senders)],
    ServerMsacc =
        erlang:spawn(
          B,
          fun() ->
                  receive
                      {start,Pid} ->
                          msacc:start(10000),
                          receive
                              {done,Pid} ->
                                  Pid ! {self(),msacc:stats()}
                          end
                  end
          end),
    erlang:system_monitor(self(),[busy_dist_port]),
    %% We spawn 250 senders which should mean that we
    %% have a load of 250 msgs/msec
    [spawn_link(
       fun() ->
               throughput_client(Pid, Payload)
       end) || Pid <- ServerPids],
    %%
    receive after 1000 -> ok end,
    ServerMsacc ! {start,self()},
    msacc:start(10000),
    ClientMsaccStats = msacc:stats(),
    receive after 1000 -> ok end,
    ServerMsacc ! {done,self()},
    ServerMsaccStats = receive {ServerMsacc,Stats} -> Stats end,
    %%
    {ClientMsaccStats,ServerMsaccStats, flush()}.

flush() ->
    receive
        M ->
            [M | flush()]
    after 0 ->
            []
    end.

throughput_server() ->
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    throughput_server().

throughput_client(Pid, Payload) ->
    Pid ! Payload,
    receive after 1 -> throughput_client(Pid, Payload) end.

%%-----------------
%% Throughput speed

throughput_0(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 500000, 0)
      end, Config).

throughput_64(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 500000, 64)
      end, Config).

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
    #{time := Time,
      client_dist_stats := ClientDistStats,
      client_msacc_stats := ClientMsaccStats,
      client_prof := ClientProf,
      server_msacc_stats := ServerMsaccStats,
      server_prof := ServerProf,
      server_gc_before := Server_GC_Before,
      server_gc_after := Server_GC_After} =
        ssl_apply(HA, fun () -> throughput_runner(A, B, Packets, Size) end),
    [B] = ssl_apply(HA, erlang, nodes, []),
    [A] = ssl_apply(HB, erlang, nodes, []),
    ClientMsaccStats =:= undefined orelse
        msacc:print(ClientMsaccStats),
    io:format("ClientDistStats: ~p~n", [ClientDistStats]),
    Overhead =
        50 % Distribution protocol headers (empirical) (TLS+=54)
        + byte_size(erlang:term_to_binary([0|<<>>])), % Benchmark overhead
    Bytes = Packets * (Size + Overhead),
    io:format("~w bytes, ~.4g s~n", [Bytes,Time/1000000]),
    SizeString = integer_to_list(Size),
    ClientMsaccStats =:= undefined orelse
        report(
          Prefix ++ " Sender_RelativeCoreLoad_" ++ SizeString,
          round(msacc:stats(system_runtime, ClientMsaccStats)
                * 1000000 / Bytes),
          "ps/byte"),
    ServerMsaccStats =:= undefined orelse
        begin
            report(
              Prefix ++ " Receiver_RelativeCoreLoad_" ++ SizeString,
              round(msacc:stats(system_runtime, ServerMsaccStats)
                    * 1000000 / Bytes),
              "ps/byte"),
            msacc:print(ServerMsaccStats)
        end,
    io:format("******* ClientProf:~n", []), prof_print(ClientProf),
    io:format("******* ServerProf:~n", []), prof_print(ServerProf),
    io:format("******* Server GC Before:~n~p~n", [Server_GC_Before]),
    io:format("******* Server GC After:~n~p~n", [Server_GC_After]),
    Speed = round((Bytes * 1000000) / (1024 * Time)),
    report(Prefix ++ " Throughput_" ++ SizeString, Speed, "kB/s").

%% Runs on node A and spawns a server on node B
throughput_runner(A, B, Rounds, Size) ->
    Payload = payload(Size),
    [A] = rpc:call(B, erlang, nodes, []),
    ClientPid = self(),
    ServerPid =
        erlang:spawn_opt(
          B,
          fun () -> throughput_server(ClientPid, Rounds) end,
          [{message_queue_data, off_heap}]),
    ServerMon = erlang:monitor(process, ServerPid),
    msacc_available() andalso
        begin
            msacc:stop(),
            msacc:reset(),
            msacc:start(),
            ok
        end,
    prof_start(),
    #{time := Time} = Result =
        throughput_client(ServerPid, ServerMon, Payload, Rounds),
    prof_stop(),
    MsaccStats =
        case msacc_available() of
            true ->
                MStats = msacc:stats(),
                msacc:stop(),
                MStats;
            false ->
                undefined
        end,
    Prof = prof_end(),
    [{_Node,Socket}] = dig_dist_node_sockets(),
    DistStats = inet:getstat(Socket),
    Result#{time := microseconds(Time),
            client_dist_stats => DistStats,
            client_msacc_stats => MsaccStats,
            client_prof => Prof}.

dig_dist_node_sockets() ->
    [case DistCtrl of
         {_Node,Socket} = NodeSocket when is_port(Socket) ->
             NodeSocket;
         {Node,DistCtrlPid} when is_pid(DistCtrlPid) ->
             [{links,DistCtrlLinks}] = process_info(DistCtrlPid, [links]),
             case [S || S <- DistCtrlLinks, is_port(S)] of
                 [Socket] ->
                     {Node,Socket};
                 [] ->
                     [{monitors,[{process,DistSenderPid}]}] =
                         process_info(DistCtrlPid, [monitors]),
                     [{links,DistSenderLinks}] =
                         process_info(DistSenderPid, [links]),
                     [Socket] = [S || S <- DistSenderLinks, is_port(S)],
                     {Node,Socket}
             end
     end || DistCtrl <- erlang:system_info(dist_ctrl)].


throughput_server(Pid, N) ->
    GC_Before = get_server_gc_info(),
    %% dbg:tracer(port, dbg:trace_port(file, "throughput_server_gc.log")),
    %% dbg:p(TLSDistReceiver, garbage_collection),
    msacc_available() andalso
        begin
            msacc:stop(),
            msacc:reset(),
            msacc:start(),
            ok
        end,
    prof_start(),
    throughput_server_loop(Pid, GC_Before, N).

throughput_server_loop(_Pid, GC_Before, 0) ->
    prof_stop(),
    MsaccStats =
        case msacc_available() of
            true ->
                msacc:stop(),
                MStats = msacc:stats(),
                msacc:reset(),
                MStats;
            false ->
                undefined
        end,
    Prof = prof_end(),
    %% dbg:flush_trace_port(),
    exit(#{server_msacc_stats => MsaccStats,
           server_prof => Prof,
           server_gc_before => GC_Before,
           server_gc_after => get_server_gc_info()});
throughput_server_loop(Pid, GC_Before, N) ->
    receive
        Msg ->
            case Msg of
                {Pid, N, _} ->
                    throughput_server_loop(Pid, GC_Before, N - 1);
                Other ->
                    erlang:error({self(),?FUNCTION_NAME,Other})
            end
    end.

get_server_gc_info() ->
    case whereis(ssl_connection_sup_dist) of
        undefined ->
            undefined;
        SupPid ->
            [{_Id,TLSDistReceiver,_Type,_Modules}|_] =
                supervisor:which_children(SupPid),
            erlang:process_info(
              TLSDistReceiver, [garbage_collection,garbage_collection_info])
    end.

throughput_client(Pid, Mon, Payload, N) ->
    throughput_client_loop(Pid, Mon, Payload, N, start_time()).

throughput_client_loop(_Pid, Mon, _Payload, 0, StartTime) ->
    receive
        {'DOWN', Mon, _, _, #{} = Result} ->
            Result#{time => elapsed_time(StartTime)};
        {'DOWN', Mon, _, _, Other} ->
            exit(Other)
    end;
throughput_client_loop(Pid, Mon, Payload, N, StartTime) ->
    Pid ! {self(), N, Payload},
    throughput_client_loop(Pid, Mon, Payload, N - 1, StartTime).


-define(prof, none). % none | cprof | eprof

-if(?prof =:= cprof).
prof_start() ->
    cprof:stop(),
    cprof:start(),
    ok.
-elif(?prof =:= eprof).
prof_start() ->
    catch eprof:stop(),
    {ok,_} = eprof:start(),
    profiling = eprof:start_profiling(processes()),
    ok.
-elif(?prof =:= none).
prof_start() ->
    ok.
-endif.

-if(?prof =:= cprof).
prof_stop() ->
    cprof:pause(),
    ok.
-elif(?prof =:= eprof).
prof_stop() ->
    _ = eprof:stop_profiling(),
    ok.
-elif(?prof =:= none).
prof_stop() ->
    ok.
-endif.

-if(?prof =:= cprof).
prof_end() ->
    Prof = cprof:analyse(),
    cprof:stop(),
    Prof.
-elif(?prof =:= eprof).
prof_end() ->
    eprof:dump_data().
-elif(?prof =:= none).
prof_end() ->
    [].
-endif.

-if(?prof =:= cprof).
prof_print(Prof) ->
    io:format("~p.~n", [Prof]).
-elif(?prof =:= eprof).
prof_print(Dump) ->
    eprof:analyze(undefined, total, [], Dump).
-elif(?prof =:= none).
prof_print([]) ->
    ok.
-endif.

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
            proplists:get_value(ssl_dist_args, Config, "")
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

msacc_available() ->
    msacc:available().
