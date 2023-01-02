%%%-------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2023. All Rights Reserved.
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
-feature(maybe_expr, enable).

-behaviour(ct_suite).

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
    parallel_setup/1,
    roundtrip/1,
    sched_utilization/1,
    mean_load_cpu_margin/1,
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
    [{group, smoketest}].

groups() ->
    [{smoketest, protocols()},
     {benchmark, protocols()},
     %%
     %% protocols()
     {ssl,    encryption_backends()},
     {crypto, categories()},
     {plain,  categories()},
     {socket, categories()},
     %%
     %% encryption_backends()
     {tls,     categories()},
     {ktls, categories()},
     %%
     %% categories()
     {setup, [{repeat, 1}],
      [setup,
       parallel_setup]},
     {roundtrip, [{repeat, 1}], [roundtrip]},
     {sched_utilization,[{repeat, 1}],
      [sched_utilization,
       mean_load_cpu_margin]},
     {throughput, [{repeat, 1}],
      [throughput_0,
       throughput_64,
       throughput_1024,
       throughput_4096,
       throughput_16384,
       throughput_65536,
       throughput_262144,
       throughput_1048576]}].

protocols() ->
    [{group, ssl},
     {group, crypto},
     {group, plain},
     {group, socket}].

encryption_backends() ->
    [{group,tls},
     {group,ktls}].

categories() ->
    [{group, setup},
     {group, roundtrip},
     {group, throughput},
     {group, sched_utilization}
    ].

init_per_suite(Config) ->
    Digest = sha1,
    ECCurve = secp521r1,
%%%     TLSVersion = 'tlsv1.2',
%%%     TLSCipher =
%%%         #{key_exchange => ecdhe_ecdsa,
%%%           cipher       => aes_128_cbc,
%%%           mac          => sha256,
%%%           prf          => sha256},
    TLSVersion = 'tlsv1.3',
    TLSCipher =
        #{key_exchange => ecdhe_ecdsa,
          cipher       => aes_256_gcm,
          mac          => aead,
          prf          => sha384},
    %%
    Node = node(),
    Skip = make_ref(),
    try
        Node =/= nonode@nohost orelse
            throw({Skip,"Node not distributed"}),
        verify_node_src_addr(),
        {supported, SSLVersions} =
            lists:keyfind(supported, 1, ssl:versions()),
        lists:member(TLSVersion, SSLVersions) orelse
            throw(
              {Skip,
               "SSL does not support " ++ term_to_string(TLSVersion)}),
%%%         lists:member(ECCurve, ssl:eccs(TLSVersion)) orelse
%%%             throw(
%%%               {Skip,
%%%                "SSL does not support " ++ term_to_string(ECCurve)}),
        TLSCipherKeys = maps:keys(TLSCipher),
        lists:any(
          fun (Cipher) ->
                  maps:with(TLSCipherKeys, Cipher) =:= TLSCipher
          end,
          ssl:cipher_suites(default, TLSVersion)) orelse
            throw(
              {Skip,
               "SSL does not support " ++ term_to_string(TLSCipher)}),
        %%
        %%
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
        PrivDir = proplists:get_value(priv_dir, Config),
        %%
        ServerNode = ssl_bench_test_lib:setup(dist_server),
        [_, ServerHost] = split_node(ServerNode),
        ServerName = ?MODULE_STRING ++ "_server",
        ServerString = ServerName ++ "@" ++ ServerHost,
        ServerConfFile = filename:join(PrivDir, ServerString ++ ".conf"),
        Server = list_to_atom(ServerString),
        %%
        write_node_conf(
          ServerConfFile, Server, ServerConf, ClientConf,
          CertOptions, RootCert),
        %%
        Schedulers =
            erpc:call(ServerNode, erlang, system_info, [schedulers]),
        [_, ClientHost] = split_node(Node),
        [{server_node, ServerNode},
         {server_name, ServerName},
         {server, Server},
         {server_dist_args,
          "-proto_dist inet_tls "
          "-ssl_dist_optfile " ++ ServerConfFile ++ " "},
         {clients, Schedulers} |
         init_client_node(
           ClientHost, Schedulers, PrivDir, ServerConf, ClientConf,
           CertOptions, RootCert, Config)]
    catch
        throw : {Skip, Reason} ->
            {skip, Reason};
        Class : Reason : Stacktrace ->
            {fail, {Class, Reason, Stacktrace}}
    end.

init_client_node(
  _ClientHost, 0, _PrivDir, _ServerConf, _ClientConf,
  _CertOptions, _RootCert, Config) ->
    Config;
init_client_node(
  ClientHost, N, PrivDir, ServerConf, ClientConf,
  CertOptions, RootCert, Config) ->
    ClientName = ?MODULE_STRING ++ "_client_" ++ integer_to_list(N),
    ClientString = ClientName ++ "@" ++ ClientHost,
    ClientConfFile = filename:join(PrivDir, ClientString ++ ".conf"),
    Client = list_to_atom(ClientString),
    %%
    write_node_conf(
      ClientConfFile, Client, ServerConf, ClientConf,
      CertOptions, RootCert),
    init_client_node(
      ClientHost, N - 1, PrivDir, ServerConf, ClientConf,
      CertOptions, RootCert,
      [{{client_name, N}, ClientName},
       {{client, N}, Client},
       {{client_dist_args, N},
        "-proto_dist inet_tls "
        "-ssl_dist_optfile " ++ ClientConfFile ++ " "} | Config]).

end_per_suite(Config) ->
    ServerNode = proplists:get_value(server_node, Config),
    ssl_bench_test_lib:cleanup(ServerNode).

init_per_group(benchmark, Config) ->
    [{effort,10}|Config];
%%
init_per_group(ssl, Config) ->
    [{ssl_dist, true}, {ssl_dist_prefix, "SSL"}|Config];
init_per_group(crypto, Config) ->
    try inet_epmd_socket_cryptcookie:supported() of
        ok ->
            [{ssl_dist, false}, {ssl_dist_prefix, "Crypto"},
             {ssl_dist_args,
              "-proto_dist inet_epmd -inet_epmd socket_cryptcookie"}
            | Config];
        Problem ->
            {skip, Problem}
    catch
        Class : Reason : Stacktrace ->
            {fail, {Class, Reason, Stacktrace}}
    end;
init_per_group(plain, Config) ->
    [{ssl_dist, false}, {ssl_dist_prefix, "Plain"}|Config];
%%
init_per_group(socket, Config) ->
    [{ssl_dist, false},
     {ssl_dist_prefix, "Socket"},
     {ssl_dist_args,
      "-proto_dist inet_epmd -inet_epmd socket"}
     | Config];
%%
init_per_group(ktls, Config) ->
    {ok, Listen} = gen_tcp:listen(0, [{active, false}]),
    {ok, Port} = inet:port(Listen),
    {ok, Client} =
        gen_tcp:connect({127,0,0,1}, Port, [{active, false}]),
    {ok, Server} = gen_tcp:accept(Listen),
    try
        maybe
            ok ?= ssl_test_lib:ktls_check_os(),
            ok ?= ssl_test_lib:ktls_set_ulp(Client),
            ok ?= ssl_test_lib:ktls_set_cipher(Client, tx, 1),
            [{ktls, true},
             {ssl_dist_prefix,
              proplists:get_value(ssl_dist_prefix, Config) ++ "-kTLS"}
            | proplists:delete(ssl_dist_prefix, Config)]
        else
            {error, Reason} ->
                {skip, {ktls, Reason}}
        end
    after
        _ = gen_tcp:close(Server),
        _ = gen_tcp:close(Client),
        _ = gen_tcp:close(Listen)
    end;
%%
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(Func, Conf) ->
    case proplists:is_defined(effort, Conf) of
        false ->
            %% Not a benchmark run
            case atom_to_list(Func) of
                "throughput_64" ->
                    Conf;
                "throughput_"++_ ->
                    {skipped, "Benchmarks run separately"};
                _ ->
                    Conf
            end;
        true ->
            Conf
    end.

end_per_testcase(_Func, _Conf) ->
    ok.

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
    run_nodepair_test(fun setup/6, Config).

setup(A, B, Prefix, Effort, HA, HB) ->
    Rounds = 100 * Effort,
    [] = ssl_apply(HA, erlang, nodes, []),
    [] = ssl_apply(HB, erlang, nodes, []),
    pong = ssl_apply(HA, net_adm, ping, [B]),
    _ = ssl_apply(HA, fun () -> set_cpu_affinity(client) end),
    {Log, Before, After} =
        ssl_apply(HB, fun () -> set_cpu_affinity(server) end),
    ct:pal("Server CPU affinity: ~w -> ~w~n~s", [Before, After, Log]),
    MemStart = mem_start(HA, HB),
    ChildCountResult =
        ssl_dist_test_lib:apply_on_ssl_node(
          HA, supervisor, count_children, [tls_dist_connection_sup]),
    ct:log("TLS Connection Child Count Result: ~p", [ChildCountResult]),
    {SetupTime, CycleTime} =
        ssl_apply(HA, fun () -> setup_runner(A, B, Rounds) end),
    ok = ssl_apply(HB, fun () -> setup_wait_nodedown(A, 10000) end),
    {MemA, MemB, MemSuffix} = mem_stop(HA, HB, MemStart),
    %% [] = ssl_apply(HA, erlang, nodes, []),
    %% [] = ssl_apply(HB, erlang, nodes, []),
    SetupSpeed = round((Rounds*1000000*1000) / SetupTime),
    CycleSpeed = round((Rounds*1000000*1000) / CycleTime),
    _ = report(Prefix++" Setup Mem A", MemA, "KByte"),
    _ = report(Prefix++" Setup Mem B", MemB, "KByte"),
    _ = report(Prefix++" Setup", SetupSpeed, "setups/1000s"),
    report(Prefix++" Setup Cycle", CycleSpeed, "cycles/1000s " ++ MemSuffix).

%% Runs on node A against rex in node B
setup_runner(A, B, Rounds) ->
    StartTime = start_time(),
    SetupTime = setup_loop(A, B, 0, Rounds),
    {microseconds(SetupTime), microseconds(elapsed_time(StartTime))}.

setup_loop(_A, _B, T, 0) ->
    T;
setup_loop(A, B, T, N) ->
    StartTime = start_time(),
    try erpc:call(B, net_adm, ping, [A]) of
        pong -> ok;
        Other ->
            error({N,Other})
    catch
        Class : Reason : Stacktrace ->
            erlang:raise(Class, {N,Reason}, Stacktrace)
    end,
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


set_cpu_affinity(client) ->
    {LogicalProcessors, _} = split_cpus(),
    update_schedulers(taskset(LogicalProcessors));
set_cpu_affinity(server) ->
    {_, LogicalProcessors} = split_cpus(),
    update_schedulers(taskset(LogicalProcessors)).

taskset(LogicalProcessors) ->
    os:cmd(
      "taskset -c -p " ++
          lists:flatten(
            lists:join(
              ",",
              [integer_to_list(Id) || Id <- LogicalProcessors]),
            " ") ++ os:getpid()).

split_cpus() ->
    split_cpus(erlang:system_info(cpu_topology)).

split_cpus([{_Tag, List}]) ->
    split_cpus(List);
split_cpus(List = [_ | _]) ->
    {A, B} = lists:split(length(List) bsr 1, List),
    {logical_processors(A), logical_processors(B)}.

logical_processors([{_Tag, {logical, Id}} | Items]) ->
    [Id | logical_processors(Items)];
logical_processors([{_Tag, List} | Items]) ->
    logical_processors(List) ++ logical_processors(Items);
logical_processors([]) ->
    [].

update_schedulers(CmdResult) ->
    _ = erlang:system_info(update_cpu_info),
    Schedulers = erlang:system_info(logical_processors_available),
    {CmdResult,
     erlang:system_flag(schedulers_online, Schedulers),
     Schedulers}.


%%----------------
%% Parallel setup

parallel_setup(Config) ->
    Clients = proplists:get_value(clients, Config),
    parallel_setup(Config, Clients, Clients, []).

parallel_setup(Config, Clients, I, HNs) when 0 < I ->
    Key = {client, I},
    Node = proplists:get_value(Key, Config),
    Handle = start_ssl_node(Key, Config),
    _ = ssl_apply(Handle, fun () -> set_cpu_affinity(client) end),
    try
        parallel_setup(Config, Clients, I - 1, [{Handle, Node} | HNs])
    after
        stop_ssl_node(Key, Handle, Config)
    end;
parallel_setup(Config, Clients, _0, HNs) ->
    Key = server,
    ServerNode = proplists:get_value(Key, Config),
    ServerHandle = start_ssl_node(Key, Config, 1),
    Effort = proplists:get_value(effort, Config, 1),
    TotalRounds = 1000 * Effort,
    Rounds = round(TotalRounds / Clients),
    try
        {Log, Before, After} =
            ssl_apply(ServerHandle, fun () -> set_cpu_affinity(server) end),
        ct:pal("Server CPU affinity: ~w -> ~w~n~s", [Before, After, Log]),
        ServerMemBefore =
            ssl_apply(ServerHandle, fun mem/0),
        parallel_setup_result(
          Config, TotalRounds, ServerHandle, ServerMemBefore,
          [parallel_setup_runner(Handle, Node, ServerNode, Rounds)
           || {Handle, Node} <- HNs])
    after
        stop_ssl_node(Key, ServerHandle, Config)
    end.

parallel_setup_runner(Handle, Node, ServerNode, Rounds) ->
    Collector = self(),
    Tag = make_ref(),
    _ =
        spawn_link(
          fun () ->
                  Collector !
                      {Tag,
                       try
                           MemBefore =
                               ssl_apply(Handle, fun mem/0),
                           Result =
                               ssl_apply(
                                 Handle, ?MODULE, setup_runner,
                                 [Node, ServerNode, Rounds]),
                           MemAfter =
                               ssl_apply(Handle, fun mem/0),
                           {MemBefore, Result, MemAfter}
                       catch Class : Reason : Stacktrace ->
                               {Class, Reason, Stacktrace}
                       end}
          end),
    Tag.

parallel_setup_result(
  Config, TotalRounds, ServerHandle, ServerMemBefore, Tags) ->
    parallel_setup_result(
      Config, TotalRounds, ServerHandle, ServerMemBefore, Tags,
      0, 0, 0).
%%
parallel_setup_result(
  Config, TotalRounds, ServerHandle, ServerMemBefore, [Tag | Tags],
  SetupTime, CycleTime, Mem) ->
    receive
        {Tag, {{_, _, MaxEver1}, {ST, CT}, {_, _, MaxEver2}}}
          when is_integer(ST), is_integer(CT),
               is_integer(MaxEver1), is_integer(MaxEver2) ->
            parallel_setup_result(
              Config, TotalRounds, ServerHandle, ServerMemBefore, Tags,
              SetupTime + ST, CycleTime + CT, Mem + MaxEver2 - MaxEver1);
        {Tag, Error} ->
            exit(Error)
    end;
parallel_setup_result(
  Config, TotalRounds, ServerHandle, ServerMemBefore, [],
  SetupTime, CycleTime, Mem) ->
    ServerMemAfter =
        ssl_apply(ServerHandle, fun mem/0),
    ServerMem =
        case {ServerMemBefore, ServerMemAfter} of
            {{_, _, ServerMaxEver1}, {_, _, ServerMaxEver2}}
              when is_integer(ServerMaxEver1), is_integer(ServerMaxEver2) ->
                ServerMaxEver2 - ServerMaxEver1;
            Other ->
                exit(Other)
        end,
    Clients = proplists:get_value(clients, Config),
    Prefix = proplists:get_value(ssl_dist_prefix, Config),
    SetupSpeed = 1000 * round(TotalRounds / (SetupTime/1000000)),
    CycleSpeed = 1000 * round(TotalRounds / (CycleTime/1000000)),
    {MemC, MemS, MemSuffix} = mem_result(Mem / Clients, ServerMem),
    _ = report(Prefix++" Parallel Setup Mem Clients", MemC, "KByte"),
    _ = report(Prefix++" Parallel Setup Mem Server", MemS, "KByte"),
    _ = report(Prefix++" Parallel Setup", SetupSpeed, "setups/1000s"),
    report(
      Prefix++" Parallel Setup Cycle", CycleSpeed, "cycles/1000s "
      ++ MemSuffix).

%%----------------
%% Roundtrip speed

roundtrip(Config) ->
    run_nodepair_test(fun roundtrip/6, Config).

roundtrip(A, B, Prefix, Effort, HA, HB) ->
    Rounds = 4000 * Effort,
    [] = ssl_apply(HA, erlang, nodes, []),
    [] = ssl_apply(HB, erlang, nodes, []),
    MemStart = mem_start(HA, HB),
    ok = ssl_apply(HA, net_kernel, allow, [[B]]),
    ok = ssl_apply(HB, net_kernel, allow, [[A]]),
    Time = ssl_apply(HA, fun () -> roundtrip_runner(A, B, Rounds) end),
    [B] = ssl_apply(HA, erlang, nodes, []),
    [A] = ssl_apply(HB, erlang, nodes, []),
    {MemA, MemB, MemSuffix} = mem_stop(HA, HB, MemStart),
    Speed = round((Rounds*1000000) / Time),
    _ = report(Prefix++" Roundtrip Mem A", MemA, "KByte"),
    _ = report(Prefix++" Roundtrip Mem B", MemB, "KByte"),
    report(Prefix++" Roundtrip", Speed, "pings/s " ++ MemSuffix).

%% Runs on node A and spawns a server on node B
roundtrip_runner(A, B, Rounds) ->
    ClientPid = self(),
    [A] = erpc:call(B, erlang, nodes, []),
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
      fun(A, B, Prefix, Effort, HA, HB) ->
              sched_utilization(A, B, Prefix, Effort, HA, HB, Config)
      end, Config).

sched_utilization(A, B, Prefix, Effort, HA, HB, Config) ->
    SSL = proplists:get_value(ssl_dist, Config),
    [] = ssl_apply(HA, erlang, nodes, []),
    [] = ssl_apply(HB, erlang, nodes, []),
    MemStart = mem_start(HA, HB),
    PidA = ssl_apply(HA, os, getpid, []),
    PidB = ssl_apply(HB, os, getpid, []),
    ct:pal("Starting scheduler utilization run effort ~w:~n"
           "    [~s] ~w~n"
           "    [~s] ~w~n",
           [Effort, PidA, A, PidB, B]),
    {ClientMsacc, ServerMsacc, BusyDistPortMsgs} =
        ssl_apply(
          HA,
          fun () ->
                  Result = sched_util_runner(A, B, Effort, SSL, Config),
                  fs_log(
                    Config,
                    "sched_utilization.Result", Result),
                  Result
          end),
    ct:log("Got ~p busy_dist_port msgs",[tail(BusyDistPortMsgs)]),
    [B] = ssl_apply(HA, erlang, nodes, []),
    [A] = ssl_apply(HB, erlang, nodes, []),
    {MemA, MemB, MemSuffix} = mem_stop(HA, HB, MemStart),
    ct:log("Microstate accounting for node ~w:", [A]),
    msacc:print(ClientMsacc),
    ct:log("Microstate accounting for node ~w:", [B]),
    msacc:print(ServerMsacc),
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
        if
            BusyDistPortMsgs =:= 0 ->
                "";
            is_integer(BusyDistPortMsgs) ->
                " ?";
            true ->
                ct:log("Stray Msgs: ~p", [BusyDistPortMsgs]),
                " ???"
        end,
    _ = report(Prefix++" Sched Utilization Client Mem", MemA, "KByte"),
    _ = report(Prefix++" Sched Utilization Server Mem", MemB, "KByte"),
    {comment, ClientComment} =
        report(Prefix ++ " Sched Utilization Client" ++ Verdict,
               SchedUtilClient, " %" ++ Verdict),
    {comment, ServerComment} =
        report(Prefix++" Sched Utilization Server" ++ Verdict,
               SchedUtilServer, " %" ++ Verdict),
    {comment,
     "Client " ++ ClientComment ++ ", Server " ++ ServerComment ++
         " " ++ MemSuffix}.

%% Runs on node A and spawns a server on node B
%% We want to avoid getting busy_dist_port as it hides the true SU usage
%% of the receiver and sender.
sched_util_runner(A, B, Effort, true, Config) ->
    sched_util_runner(A, B, Effort, 100, Config);
sched_util_runner(A, B, Effort, false, Config) ->
    sched_util_runner(A, B, Effort, 100, Config);
sched_util_runner(A, B, Effort, Senders, Config) ->
    process_flag(trap_exit, true),
    Payload = payload(100),
    Time = 1000 * Effort,
    [A] = erpc:call(B, erlang, nodes, []),
    ServerPids =
        [erlang:spawn_link(
           B, fun () -> throughput_server() end)
         || _ <- lists:seq(1, Senders)],
    Tag = make_ref(),
    ServerMsacc =
        erlang:spawn_link(
          B,
          fun() ->
                  receive
                      {start,Tag,Pid} ->
                          fs_log(
                            Config,
                            "sched_util_runner.Server.msacc.self",
                            self()),
                          msacc:start(Time),
                          fs_log(
                            Config,
                            "sched_util_runner.Server.msacc:start",
                            ok),
                          receive
                              {done,Tag,Pid} ->
                                  fs_log(
                                    Config,
                                    "sched_util_runner.Server.msacc:stats",
                                    ok),
                                  ServerStats = msacc:stats(),
                                  fs_log(Config,
                                         "sched_util_runner.Server.msacc:stats",
                                         ServerStats),
                                  exit({result,Tag,ServerStats})
                          end
                  end
          end),
    erlang:system_monitor(self(),[busy_dist_port]),
    %% We spawn 100 senders that send a message every 10 ms
    %% which should produce a load of 10000 msgs/s with
    %% payload 100 bytes each -> 1 MByte/s
    _Clients =
        [spawn_link(
           fun() ->
                   throughput_client(Pid, Payload)
           end) || Pid <- ServerPids],
    %%
    receive after 1000 -> ok end,
    ServerMsacc ! {start,Tag,self()},
    fs_log(Config, "sched_util_runner.Client.self", self()),
    msacc:start(Time),
    fs_log(Config, "sched_util_runner.Client.msacc:start", ok),
    ClientMsaccStats = msacc:stats(),
    fs_log(Config, "sched_util_runner.Client.msacc.stats", ClientMsaccStats),
    receive after 1000 -> ok end,
    ServerMsacc ! {done,Tag,self()},
    ServerMsaccStats =
        receive
            {'EXIT',ServerMsacc,{result,Tag,Stats}} ->
                Stats;
            {'EXIT',ServerMsacc,Other} ->
                exit({other,ServerMsacc,Other})
        end,
    fs_log(Config, "sched_util_runner.ServerMsaccStats", ServerMsaccStats),
    %%
    {ClientMsaccStats,ServerMsaccStats, busy_dist_port_msgs()}.

fs_log(Config, Name, Term) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    DistPrefix = proplists:get_value(ssl_dist_prefix, Config),
    _ = file:write_file(
          filename:join(PrivDir, DistPrefix ++ "_" ++ Name),
          io_lib:format(
            "~p~n",
            [{{erlang:unique_integer([positive,monotonic]),
               os:system_time(1000000)},
             Term}])),
    ok.

busy_dist_port_msgs() ->
    busy_dist_port_msgs(0).
%%
busy_dist_port_msgs(N) ->
    receive
        M ->
            case M of
                {monitor, P1, busy_dist_port, P2}
                  when is_pid(P1), is_pid(P2) ->
                    busy_dist_port_msgs(N + 1);
                Stray ->
                    [Stray | busy_dist_port_msgs(N)]
            end
    after 0 ->
            N
    end.

tail([_|Tail]) ->
    tail(Tail);
tail(Tail) ->
    Tail.

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
    receive after 10 -> throughput_client(Pid, Payload) end.

%%-----------------
%% Mean load CPU margin
%%
%% Start pairs of processes with the client on node A
%% and the server on node B.  The clients sends requests
%% with random interval and payload and the servers reply
%% immediately.
%%
%% Also, besides each server there is a compute process
%% that does CPU work with low process priority and we measure
%% how much such work that gets done.

mean_load_cpu_margin(Config) ->
    run_nodepair_test(fun run_mlcm/6, Config).

-define(MLCM_NO, 100).

run_mlcm(A, B, Prefix, Effort, HA, HB) ->
    [] = ssl_apply(HA, erlang, nodes, []),
    [] = ssl_apply(HB, erlang, nodes, []),
    MemStart = mem_start(HA, HB),
    pong = ssl_apply(HB, net_adm, ping, [A]),
    Count = ssl_apply(HA, fun () -> mlcm(B, Effort) end),
    {MemA, MemB, MemSuffix} = mem_stop(HA, HB, MemStart),
    _ = report(Prefix++" CPU Margin Mem A", MemA, "KByte"),
    _ = report(Prefix++" CPU Margin Mem B", MemB, "KByte"),
    report(
      Prefix++" CPU Margin",
      round(Count/?MLCM_NO/Effort),
      "stones " ++ MemSuffix).

mlcm(Node, Effort) ->
    Payloads = mlcm_payloads(),
    Clients =
        [mlcm_client_start(Node, Payloads) || _ <- lists:seq(1, ?MLCM_NO)],
    receive after 1000 * Effort -> ok end,
    [Alias ! {Alias,stop} || {_Monitor, Alias} <- Clients],
    Counts =
        [receive
             {'DOWN',Monitor,_,_,{Alias, Count}} ->
                 Count;
             {'DOWN',Monitor,_,_,Reason} ->
                 exit(Reason)
         end || {Monitor, Alias} <- Clients],
    lists:sum(Counts).

mlcm_payloads() ->
    Bin = list_to_binary([rand:uniform(256) - 1 || _ <- lists:seq(1, 512)]),
    lists:foldl(
      fun (N, Payloads) ->
              Payloads#{N => binary:copy(Bin, N)}
      end, #{}, lists:seq(0, 255)).

%%-------

mlcm_client_start(Node, Payloads) ->
    Parent = self(),
    StartRef = make_ref(),
    {_,Monitor} =
        spawn_monitor(
          fun () ->
                  Alias = alias(),
                  Parent ! {StartRef, Alias},
                  Server = mlcm_server_start(Node, Alias),
                  mlcm_client(Alias, Server, Payloads, 0)
          end),
    receive
        {StartRef, Alias} ->
            {Monitor, Alias};
        {'DOWN',Monitor,_,_,Reason} ->
            exit(Reason)
    end.

mlcm_client(Alias, Server, Payloads, Seq) ->
    {Time, Index} = mlcm_rand(),
    Payload = maps:get(Index, Payloads),
    receive after Time -> ok end,
    Server ! {Alias, Seq, Payload},
    receive
        {Alias, Seq, Pl} when byte_size(Pl) =:= byte_size(Payload) ->
            mlcm_client(Alias, Server, Payloads, Seq + 1);
        {Alias, stop} = Msg ->
            Server ! Msg,
            receive after infinity -> ok end
    end.

%% Approximate normal distribution Index with an average of 6 uniform bytes
%% and use the 7:th byte for uniform Time
mlcm_rand() ->
    mlcm_rand(6, rand:uniform(1 bsl (1+6)*8) - 1, 0).
%%
mlcm_rand(0, X, I) ->
    Time = X + 1, % 1..256
    Index = abs((I - 3*256) div 3), % 0..255 upper half or normal distribution
    {Time, Index};
mlcm_rand(N, X, I) ->
    mlcm_rand(N - 1, X bsr 8, I + (X band 255)).

%%-------

mlcm_server_start(Node, Alias) ->
    spawn_link(
      Node,
      fun () ->
              Compute = mlcm_compute_start(Alias),
              mlcm_server(Alias, 0, Compute)
      end).

mlcm_server(Alias, Seq, Compute) ->
    receive
        {Alias, Seq, _Payload} = Msg ->
            Alias ! Msg,
      mlcm_server(Alias, Seq + 1, Compute);
        {Alias, stop} = Msg ->
            Compute ! Msg,
            receive after infinity -> om end
    end.

%%-------

mlcm_compute_start(Alias) ->
    spawn_opt(
      fun () ->
              rand:seed(exro928ss),
              mlcm_compute(Alias, 0, 0)
      end,
      [link, {priority,low}]).

mlcm_compute(Alias, State, Count) ->
    receive {Alias, stop} -> exit({Alias, Count})
    after 0 -> ok
    end,
    mlcm_compute(
      Alias,
      %% CPU payload
      (State +
           lists:sum([rand:uniform(1 bsl 48) || _ <- lists:seq(1, 999)]))
          div 1000,
      Count + 1).

%%-----------------
%% Throughput speed

throughput_0(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, Effort, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 50000 * Effort, 0)
      end, Config).

throughput_64(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, Effort, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 50000 * Effort, 64)
      end, Config).

throughput_1024(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, Effort, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 10000 * Effort, 1024)
      end, Config).

throughput_4096(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, Effort, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 5000 * Effort, 4096)
      end, Config).

throughput_16384(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, Effort, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 1000 * Effort, 16384)
      end, Config).

throughput_65536(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, Effort, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 200 * Effort, 65536)
      end, Config).

throughput_262144(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, Effort, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 50 * Effort, 262144)
      end, Config).

throughput_1048576(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, Effort, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 20 * Effort, 1048576)
      end, Config).

throughput(A, B, Prefix, HA, HB, Packets, Size) ->
    [] = ssl_apply(HA, erlang, nodes, []),
    [] = ssl_apply(HB, erlang, nodes, []),
    MemStart = mem_start(HA, HB),
    #{time := Time,
      client_msacc_stats := ClientMsaccStats,
      client_prof := ClientProf,
      server_msacc_stats := ServerMsaccStats,
      server_prof := ServerProf,
      server_gc_before := Server_GC_Before,
      server_gc_after := Server_GC_After} =
        ssl_apply(HA, fun () -> throughput_runner(A, B, Packets, Size) end),
    [B] = ssl_apply(HA, erlang, nodes, []),
    [A] = ssl_apply(HB, erlang, nodes, []),
    {MemA, MemB, MemSuffix} = mem_stop(HA, HB, MemStart),
    ClientMsaccStats =:= undefined orelse
        msacc:print(ClientMsaccStats),
    Overhead =
        50 % Distribution protocol headers (empirical) (TLS+=54)
        + byte_size(erlang:term_to_binary([0|<<>>])), % Benchmark overhead
    Bytes = Packets * (Size + Overhead),
    io:format("~w bytes, ~.4g s~n", [Bytes,Time/1000000]),
    SizeString = integer_to_list(Size),
    _ = report(
          Prefix++" Throughput_" ++ SizeString ++ " Mem A", MemA, "KByte"),
    _ = report(
          Prefix++" Throughput_" ++ SizeString ++ " Mem B", MemB, "KByte"),
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
    report(
      Prefix ++ " Throughput_" ++ SizeString, Speed, "kB/s " ++ MemSuffix).

%% Runs on node A and spawns a server on node B
throughput_runner(A, B, Rounds, Size) ->
    Payload = payload(Size),
    [A] = erpc:call(B, erlang, nodes, []),
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
    Result#{time := microseconds(Time),
            client_msacc_stats => MsaccStats,
            client_prof => Prof}.

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
    A = proplists:get_value({client,1}, Config),
    B = proplists:get_value(server, Config),
    Prefix = proplists:get_value(ssl_dist_prefix, Config),
    Effort = proplists:get_value(effort, Config, 1),
    HA = start_ssl_node({client,1}, Config),
    try
        HB = start_ssl_node(server, Config),
        try TestFun(A, B, Prefix, Effort, HA, HB)
        after
            stop_ssl_node(server, HB, Config)
        end
    after
        stop_ssl_node({client,1}, HA, Config)
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

start_ssl_node(Spec, Config) ->
    start_ssl_node(Spec, Config, 0).
%%
start_ssl_node({client, N}, Config, Verbose) ->
    Name = proplists:get_value({client_name, N}, Config),
    Args = get_node_args({client_dist_args, N}, Config),
    Pa = filename:dirname(code:which(?MODULE)),
    ssl_dist_test_lib:start_ssl_node(
      Name, "-pa " ++ Pa ++ " " ++ Args, Verbose);
start_ssl_node(server, Config, Verbose) ->
    Name = proplists:get_value(server_name, Config),
    Args = get_node_args(server_dist_args, Config),
    Pa = filename:dirname(code:which(?MODULE)),
    ServerNode = proplists:get_value(server_node, Config),
    erpc:call(
      ServerNode, ssl_dist_test_lib, start_ssl_node,
      [Name, "-pa " ++ Pa ++ " " ++ Args, Verbose]).

stop_ssl_node({client, _}, HA, _Config) ->
    ssl_dist_test_lib:stop_ssl_node(HA);
stop_ssl_node(server, HB, Config) ->
    ServerNode = proplists:get_value(server_node, Config),
    erpc:call(ServerNode, ssl_dist_test_lib, stop_ssl_node, [HB]).

get_node_args(Tag, Config) ->
    case proplists:get_value(ssl_dist, Config) of
        true ->
            case proplists:get_value(ktls, Config, false) of
                true ->
                    "-ssl_dist_opt client_ktls true server_ktls true ";
                false ->
                    ""
            end ++ proplists:get_value(Tag, Config);
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

report(Name, Value, Suffix) ->
    ct:pal("~s: ~w ~s", [Name, Value, Suffix]),
    ct_event:notify(
      #event{
         name = benchmark_data,
         data = [{value, Value}, {suite, "ssl_dist"}, {name, Name}]}),
    {comment, term_to_string(Value) ++ " " ++ Suffix}.

term_to_string(Term) ->
    unicode:characters_to_list(
      io_lib:write(Term, [{encoding, unicode}])).

msacc_available() ->
    msacc:available().


mem_start(HA, HB) ->
    {_, _, MaxEverA} = ssl_apply(HA, fun mem/0),
    {_, _, MaxEverB} = ssl_apply(HB, fun mem/0),
    {MaxEverA, MaxEverB}.

mem_stop(HA, HB, {MaxEverA1, MaxEverB1}) ->
    {_, _, MaxEverA2} = ssl_apply(HA, fun mem/0),
    {_, _, MaxEverB2} = ssl_apply(HB, fun mem/0),
    MemA = MaxEverA2 - MaxEverA1,
    MemB = MaxEverB2 - MaxEverB1,
    mem_result(MemA, MemB).

mem_result(MemA, MemB) ->
    MemSuffix =
        io_lib:format(
          "~.5g|~.5g MByte", [MemA / (1 bsl 20), MemB / (1 bsl 20)]),
    {round(MemA / (1 bsl 10)), round(MemB / (1 bsl 10)), MemSuffix}.

mem() ->
    mem(
      0, 0, 0,
      [erlang:system_info({allocator_sizes, Alloc})
       || Alloc <- erlang:system_info(alloc_util_allocators)]).

mem(C, S, E, []) ->
    {C, S, E};
mem(C, S, E, [Items | Stack]) ->
    mem(C, S, E, Stack, Items).

mem(C, S, E, Stack, []) ->
    mem(C, S, E, Stack);
mem(C, S, E, Stack, [Item | Items]) ->
    mem(C, S, E, Stack, Items, Item).

mem(C, S, E, Stack, Items, Item) ->
    case Item of
        {size, Current} ->
            mem(C + Current, S, E, Stack, Items);
        {size, Current, MaxSize, MaxEver} ->
            mem(C + Current, S + MaxSize, E + MaxEver, Stack, Items);
        _ when is_list(element(tuple_size(Item), Item)) ->
            NewItems = element(tuple_size(Item), Item),
            mem(C, S, E, [Items | Stack], NewItems);
        _ ->
            mem(C, S, E, Stack, Items)
    end.
