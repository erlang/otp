%%%-------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2025. All Rights Reserved.
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

-behaviour(ct_suite).

-include("ssl_test_lib.hrl").
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
-export([payload/1, roundtrip_runner/2, setup_runner/2, throughput_runner/3,
        mem/0]).

%%%-------------------------------------------------------------------

suite() -> [{ct_hooks, [{ts_install_cth, [{nodenames, 2}]}]}].

all() ->
    [{group, smoketest}].

groups() ->
    [{smoketest,    protocols()},
     {benchmark,    protocols()},
     {perf_record,  protocols()},
     %%
     %% protocols()
     {ssl,          ssl_backends()},
     {cryptcookie,  cryptcookie_backends()},
     {ssl_socket,   categories()},
     {plain,        categories()},
     {plain2,       categories()},
     {socket,       categories()},
     %%
     %% ssl_backends()
     {tls,          categories()},
     {ktls,         categories()},
     %%
     %% cryptcookie_backends()
     {dist_cryptcookie_socket,  categories()},
     {cryptcookie_socket_ktls,  categories()},
     {dist_cryptcookie_inet,    categories()},
     {cryptcookie_inet_ktls,    categories()},
     {cryptcookie_inet_ktls_ih, categories()},
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
     {group, cryptcookie},
     {group, ssl_socket},
     {group, plain},
     {group, plain2},
     {group, socket}].

ssl_backends() ->
    [{group, tls},
     {group, ktls}].

cryptcookie_backends() ->
    [{group, dist_cryptcookie_socket},
     {group, cryptcookie_socket_ktls},
     {group, dist_cryptcookie_inet},
     {group, cryptcookie_inet_ktls},
     {group, cryptcookie_inet_ktls_ih}].

categories() ->
    [{group, setup},
     {group, roundtrip},
     {group, throughput},
     {group, sched_utilization}
    ].

init_per_suite(Config) ->
    Digest = sha256,
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
          "-ssl_dist_optfile " ++ ServerConfFile ++ " "},
         {clients, Schedulers} |
         init_client_node(
           ClientHost, Schedulers, PrivDir, ServerConf, ClientConf,
           CertOptions, RootCert, Config)]
    catch
        throw : {Skip, Reason} ->
            {skip, Reason};
        throw : {skipped, Reason} ->
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
        "-ssl_dist_optfile " ++ ClientConfFile ++ " "} | Config]).

end_per_suite(Config) ->
    ServerNode = proplists:get_value(server_node, Config),
    ssl_bench_test_lib:cleanup(ServerNode).

init_per_group(benchmark, Config) ->
    [{effort,5}|Config];
init_per_group(perf_record, Config) ->
    [{perf_record,true}, {effort,10}|Config];
%%
init_per_group(ssl, Config) ->
    [{ssl_dist, true}, {ssl_dist_prefix, "SSL"},
     {ssl_dist_args, "-proto_dist inet_tls "}
    |Config];
init_per_group(ssl_socket, Config) ->
    [{ssl_dist, true}, {ssl_dist_prefix, "SSL-Socket"},
     {ssl_dist_args, "-proto_dist inet_epmd -inet_epmd tls_socket"}
    | Config];
init_per_group(dist_cryptcookie_socket, Config) ->
    try inet_epmd_dist_cryptcookie_socket:supported() of
        ok ->
            [{ssl_dist, false}, {ssl_dist_prefix, "Crypto-Socket"},
             {ssl_dist_args,
              "-proto_dist inet_epmd -inet_epmd dist_cryptcookie_socket"}
            | Config];
        Problem ->
            {skip, Problem}
    catch
        Class : Reason : Stacktrace ->
            {fail, {Class, Reason, Stacktrace}}
    end;
init_per_group(cryptcookie_socket_ktls, Config) ->
    try inet_epmd_cryptcookie_socket_ktls:supported() of
        ok ->
            [{ssl_dist, false}, {ssl_dist_prefix, "Crypto-Socket-kTLS"},
             {ssl_dist_args,
              "-proto_dist inet_epmd -inet_epmd cryptcookie_socket_ktls"}
            | Config];
        Problem ->
            {skip, Problem}
    catch
        Class : Reason : Stacktrace ->
            {fail, {Class, Reason, Stacktrace}}
    end;
init_per_group(dist_cryptcookie_inet, Config) ->
    try inet_epmd_dist_cryptcookie_inet:supported() of
        ok ->
            [{ssl_dist, false}, {ssl_dist_prefix, "Crypto-Inet"},
             {ssl_dist_args,
              "-proto_dist inet_epmd -inet_epmd dist_cryptcookie_inet"}
            | Config];
        Problem ->
            {skip, Problem}
    catch
        Class : Reason : Stacktrace ->
            {fail, {Class, Reason, Stacktrace}}
    end;
init_per_group(cryptcookie_inet_ktls, Config) ->
    try inet_epmd_cryptcookie_inet_ktls:supported() of
        ok ->
            [{ssl_dist, false}, {ssl_dist_prefix, "Crypto-Inet-kTLS"},
             {ssl_dist_args,
              "-proto_dist inet_epmd -inet_epmd cryptcookie_inet_ktls "
              "-inet_ktls port"}
            | Config];
        Problem ->
            {skip, Problem}
    catch
        Class : Reason : Stacktrace ->
            {fail, {Class, Reason, Stacktrace}}
    end;
init_per_group(cryptcookie_inet_ktls_ih, Config) ->
    try inet_epmd_cryptcookie_inet_ktls:supported() of
        ok ->
            [{ssl_dist, false}, {ssl_dist_prefix, "Crypto-Inet-kTLS-IH"},
             {ssl_dist_args,
              "-proto_dist inet_epmd -inet_epmd cryptcookie_inet_ktls "
              "-inet_ktls input_handler"}
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
init_per_group(plain2, Config) ->
    try inet_epmd_socket:supported() of
        ok ->
            [{ssl_dist, false},
             {ssl_dist_prefix, "Plain2"},
             {ssl_dist_args, "-proto_dist inet_epmd -inet_epmd dist"}
            | Config];
        Problem ->
            {skip, Problem}
    catch
        Class : Reason : Stacktrace ->
            {fail, {Class, Reason, Stacktrace}}
    end;
%%
init_per_group(socket, Config) ->
    try inet_epmd_socket:supported() of
        ok ->
            [{ssl_dist, false},
             {ssl_dist_prefix, "Socket"},
             {ssl_dist_args, "-proto_dist inet_epmd -inet_epmd socket"}
            | Config];
        Problem ->
            {skip, Problem}
    catch
        Class : Reason : Stacktrace ->
            {fail, {Class, Reason, Stacktrace}}
    end;
%%
init_per_group(ktls, Config) ->
    case ktls_supported() of
        ok ->
            [{ktls, true},
             {ssl_dist_prefix,
              proplists:get_value(ssl_dist_prefix, Config) ++ "-kTLS"}
            | proplists:delete(ssl_dist_prefix, Config)];
        {error, Reason} ->
            {skip, Reason}
    end;
%%
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(Func, Conf) ->
    PerfRecord = proplists:get_value(perf_record, Conf, false),
    Effort = proplists:is_defined(effort, Conf),
    if
        PerfRecord ->
            case atom_to_list(Func) of
                "throughput_65536" ->
                    Conf;
                "throughput_"++_ ->
                    {skipped, "Don't perf record all sizes"};
                _ ->
                    Conf
            end;
        Effort ->
            Conf;
        true ->
            %% Not a benchmark run
            case atom_to_list(Func) of
                "throughput_64" ->
                    Conf;
                "throughput_"++_ ->
                    {skipped, "Benchmarks run separately"};
                _ ->
                    Conf
            end
    end.

end_per_testcase(_Func, _Conf) ->
    ok.


ktls_supported() ->
    {ok, Listen} = gen_tcp:listen(0, [{active, false}]),
    {ok, Port} = inet:port(Listen),
    {ok, Client} =
        gen_tcp:connect({127,0,0,1}, Port, [{active, false}]),
    try
        maybe
            {ok, OS} ?= ssl_test_lib:ktls_os(),
            ok ?= ssl_test_lib:ktls_set_ulp(Client, OS),
            ssl_test_lib:ktls_set_cipher(Client, OS, tx, 1)
        end
    after
        _ = gen_tcp:close(Client),
        _ = gen_tcp:close(Listen)
    end.


%%%-------------------------------------------------------------------
%%% CommonTest API helpers

verify_node_src_addr() ->
    Msg = "Hello, world!",
    {ok,Host} = inet:gethostname(),
    {ok,DstAddr} = inet:getaddr(Host, inet),
    {ok,Socket} = gen_udp:open(0, [{active,false}]),
    {ok,Port} = inet:port(Socket),
    ok = gen_udp:send(Socket, DstAddr, Port, Msg),
    %% Ubuntu has got 127.0.1.1 in /etc/hosts, but that IP address is
    %% not assigned to the interface.
    %% `ip addr add 127.0.1.1 dev lo` as root solves that problem
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


-define(REPORT_FORMAT, "~s: ~w ~s").
-define(REPORT_PAL(Name, Value, Info),
        ?CT_PAL(?REPORT_FORMAT,
                [begin Name end, begin Value end, begin Info end])).

-define(REPORT(Name, Value, Info),
        begin
            report(
              ?SSL_TEST_LIB_FORMAT, ?SSL_TEST_LIB_ARGS,
              begin Name end, begin Value end, begin Info end)
        end).
report(LFormat, LArgs, Name, Value, Info) ->
    ct:pal(LFormat ++ ?REPORT_FORMAT, LArgs ++ [Name, Value, Info]),
    ct_event:notify(
      #event{
         name = benchmark_data,
         data = [{value, Value}, {suite, "ssl_dist"}, {name, Name}]}),
    {comment,
     term_to_string(Value) ++ " " ++ unicode:characters_to_list(Info)}.

-define(REPORT(Name, Value, Info, Time),
        begin
            report(
              ?SSL_TEST_LIB_FORMAT, ?SSL_TEST_LIB_ARGS,
              begin Name end, begin Value end, begin Info end, begin Time end)
        end).
report(LFormat, LArgs, Name, Value, Info, Time) -> % Time in microseconds
    report(
      LFormat, LArgs, Name, Value,
      io_lib:fwrite("~ts [~.1f s]", [Info, Time/1000_000])).

term_to_string(Term) ->
    unicode:characters_to_list(
      io_lib:write(Term, [{encoding, unicode}])).

per_s(Bytes, Microseconds) ->
    round((Bytes * 1000_000) / Microseconds).
per_s(What) -> What ++ "/s".

per_ks(Rounds, Microseconds) ->
    round((Rounds*1000_000*1000) / Microseconds).
per_ks(What) -> What ++ "/ks".

%%%-------------------------------------------------------------------
%%% Test cases

%%-----------------------
%% Connection setup speed

setup(Config) ->
    run_nodepair_test(fun setup/6, Config).

setup(A, B, Prefix, Effort, HA, HB) ->
    Rounds = 1000 * Effort,
    [] = ssl_apply(HA, erlang, nodes, []),
    [] = ssl_apply(HB, erlang, nodes, []),
    pong = ssl_apply(HA, net_adm, ping, [B]),
    _ = ssl_apply(HA, fun () -> set_cpu_affinity(client) end),
    {Log, Before, After} =
        ssl_apply(HB, fun () -> set_cpu_affinity(server) end),
    ?CT_PAL("Server CPU affinity: ~w -> ~w~n~s", [Before, After, Log]),
    ChildCountResult =
        catch ssl_apply(
                HA, supervisor, count_children, [tls_dist_connection_sup]),
    ?CT_LOG("TLS Connection Child Count Result: ~p", [ChildCountResult]),
    io:format("~w(~p, ~p)~n", [setup_runner, B, Rounds]),
    {AccSetupTime, TotalTime, {MemA, MemB, MemText}} =
        ssl_apply(HA, fun () -> setup_runner(B, Rounds) end),
    ok = ssl_apply(HB, fun () -> setup_wait_nodedown(A, 10_000) end),
    %% [] = ssl_apply(HA, erlang, nodes, []),
    %% [] = ssl_apply(HB, erlang, nodes, []),
    SetupSpeed = per_ks(Rounds, AccSetupTime),
    CycleSpeed = per_ks(Rounds, TotalTime),
    _ = ?REPORT(Prefix++" Setup Mem A", MemA, "KByte"),
    _ = ?REPORT(Prefix++" Setup Mem B", MemB, "KByte"),
    _ = ?REPORT(Prefix++" Setup", SetupSpeed, per_ks("setups")),
    ?REPORT(Prefix++" Setup Cycle",
           CycleSpeed, per_ks("cycles") ++ " " ++ MemText,
           TotalTime).

%% Runs on node A against rex in node B.
%%
%% Can be run on two manually started nodes outside CommonTest
%%
setup_runner(B, Rounds) ->
    MemStart = mem_start(B),
    StartTime = start_time(),
    AccSetupTime = setup_loop(B, 0, Rounds),
    TotalTime = elapsed_time(StartTime),
    MemResult = mem_stop(MemStart),
    true = net_kernel:disconnect(B),
    {microseconds(AccSetupTime), microseconds(TotalTime), MemResult}.

setup_loop(_B, AccSetupTime, 0) ->
    AccSetupTime;
setup_loop(B, AccSetupTime, N) ->
    StartTime = start_time(),
    try erpc:call(B, net_adm, ping, [node()]) of
        pong -> ok;
        Other ->
            error({N,Other})
    catch
        Class : Reason : Stacktrace ->
            erlang:raise(Class, {N,Reason}, Stacktrace)
    end,
    SetupTime = elapsed_time(StartTime),
    [N,B] = [N|erlang:nodes()],
    Mref = erlang:monitor(process, {rex,B}),
    true = net_kernel:disconnect(B),
    receive
        {'DOWN',Mref,process,_,_} ->
            [] = erlang:nodes(),
            setup_loop(B, AccSetupTime + SetupTime, N - 1)
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
    set_cpu_affinity(1);
set_cpu_affinity(server) ->
    set_cpu_affinity(2);
set_cpu_affinity(Index) when is_integer(Index) ->
    case erlang:system_info(cpu_topology) of
        undefined ->
            {"", undefined, undefined};
        CpuTopology ->
            Log = taskset(element(Index, split_cpus(CpuTopology))),
            %% Update Schedulers
            _ = erlang:system_info(update_cpu_info),
            Schedulers = erlang:system_info(schedulers_online),
            {Log,
             erlang:system_flag(schedulers_online, Schedulers),
             Schedulers}
    end.

taskset(LogicalProcessors) ->
    os:cmd(
      "taskset -c -p " ++
          lists:flatten(
            lists:join(
              ",",
              [integer_to_list(Id) || Id <- LogicalProcessors]),
            " ") ++ os:getpid()).

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
    ServerHandle = start_ssl_node(Key, Config, 0),
    Effort = proplists:get_value(effort, Config, 1),
    TotalRounds = 1000 * Effort,
    Rounds = round(TotalRounds / Clients),
    try
        {Log, Before, After} =
            ssl_apply(ServerHandle, fun () -> set_cpu_affinity(server) end),
        ?CT_PAL("~nClients: ~w"
                "~nServer CPU affinity: ~w -> ~w~n~s",
                [Clients, Before, After, Log]),
        ServerMemBefore =
            ssl_apply(ServerHandle, fun mem/0),
        parallel_setup_result(
          Config, TotalRounds, ServerHandle, ServerMemBefore,
          [parallel_setup_runner(Handle, ServerNode, Rounds)
           || {Handle, _Node} <- HNs])
    after
        stop_ssl_node(Key, ServerHandle, Config)
    end.

parallel_setup_runner(Handle, ServerNode, Rounds) ->
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
                                 Handle,
                                 fun () -> % See setup_runner/2
                                         StartTime = start_time(),
                                         AccSetupTime =
                                             setup_loop(ServerNode, 0, Rounds),
                                         TotalTime = elapsed_time(StartTime),
                                         {microseconds(AccSetupTime),
                                          microseconds(TotalTime)}
                                 end),
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
  SumSetupTime, SumTotalTime, Mem) ->
    receive
        {Tag, {Mem1, {AccSetupTime, TotalTime}, Mem2}}
          when is_integer(AccSetupTime), is_integer(TotalTime) ->
            parallel_setup_result(
              Config, TotalRounds, ServerHandle, ServerMemBefore, Tags,
              SumSetupTime + AccSetupTime, SumTotalTime + TotalTime,
              Mem + Mem2 - Mem1);
        {Tag, Error} ->
            exit(Error)
    end;
parallel_setup_result(
  Config, TotalRounds, ServerHandle, ServerMemBefore, [],
  SumSetupTime, SumTotalTime, Mem) ->
    ServerMemAfter =
        ssl_apply(ServerHandle, fun mem/0),
    ServerMem = ServerMemAfter - ServerMemBefore,
    Clients = proplists:get_value(clients, Config),
    Prefix = proplists:get_value(ssl_dist_prefix, Config),
    SetupSpeed = per_ks(TotalRounds, SumSetupTime),
    CycleSpeed = per_ks(TotalRounds, SumTotalTime),
    {MemC, MemS, MemText} = mem_result({Mem / Clients, ServerMem}),
    _ = ?REPORT(Prefix++" Parallel Setup Mem Clients", MemC, "KByte"),
    _ = ?REPORT(Prefix++" Parallel Setup Mem Server", MemS, "KByte"),
    _ = ?REPORT(Prefix++" Parallel Setup", SetupSpeed, per_ks("setups")),
    ?REPORT(
      Prefix++" Parallel Setup Cycle",
      CycleSpeed, per_ks("cycles") ++ " " ++ MemText,
      SumTotalTime / Clients).

%%----------------
%% Roundtrip speed

roundtrip(Config) ->
    run_nodepair_test(fun roundtrip/6, Config).

roundtrip(A, B, Prefix, Effort, HA, HB) ->
    Rounds = 20_000 * Effort,
    [] = ssl_apply(HA, erlang, nodes, []),
    [] = ssl_apply(HB, erlang, nodes, []),
    ok = ssl_apply(HA, net_kernel, allow, [[B]]),
    ok = ssl_apply(HB, net_kernel, allow, [[A]]),
    io:format("~w(~p, ~p)~n", [roundtrip_runner, B, Rounds]),
    {Time, {MemA, MemB, MemText}} =
        ssl_apply(HA, fun () -> roundtrip_runner(B, Rounds) end),
    [B] = ssl_apply(HA, erlang, nodes, []),
    [A] = ssl_apply(HB, erlang, nodes, []),
    _ = ?REPORT(Prefix++" Roundtrip Mem A", MemA, "KByte"),
    _ = ?REPORT(Prefix++" Roundtrip Mem B", MemB, "KByte"),
    ?REPORT(Prefix++" Roundtrip",
            per_s(Rounds, Time), per_s("pings") ++ " " ++ MemText, Time).

%% Runs on node A and spawns a server on node B
%%
%% Can be run on two manually started nodes outside CommonTest
%%
roundtrip_runner(B, Rounds) ->
    A = node(),
    ClientPid = self(),
    [A] = erpc:call(B, erlang, nodes, []),
    ServerPid =
        erlang:spawn(
          B,
          fun () ->
                  roundtrip_server(ClientPid, Rounds)
          end),
    ServerMon = erlang:monitor(process, ServerPid),
    MemStart = mem_start(B),
    roundtrip_client(ServerPid, ServerMon, start_time(), MemStart, Rounds).

roundtrip_server(_Pid, 0) ->
    exit(ok);
roundtrip_server(Pid, N) ->
    receive
        N ->
            Pid ! N,
            roundtrip_server(Pid, N-1)
    end.

roundtrip_client(_Pid, Mon, StartTime, MemStart, 0) ->
    Time = microseconds(elapsed_time(StartTime)),
    receive
        {'DOWN', Mon, _, _, ok} ->
            {Time, mem_stop(MemStart)};
        {'DOWN', Mon, _, _, Other} ->
            exit(Other)
    end;
roundtrip_client(Pid, Mon, StartTime, MemStart, N) ->
    Pid ! N,
    receive
        N ->
            roundtrip_client(Pid, Mon, StartTime, MemStart, N - 1)
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
    PidA = ssl_apply(HA, os, getpid, []),
    PidB = ssl_apply(HB, os, getpid, []),
    ?CT_PAL("Starting scheduler utilization run effort ~w:~n"
           "    [~s] ~w~n"
           "    [~s] ~w~n",
           [Effort, PidA, A, PidB, B]),
    {ClientMsacc, ServerMsacc, BusyDistPortMsgs, {MemA, MemB, MemText}} =
        ssl_apply(
          HA,
          fun () ->
                  Result = sched_util_runner(A, B, Effort, SSL, Config),
                  fs_log(
                    Config,
                    "sched_utilization.Result", Result),
                  Result
          end),
    ?CT_LOG("Got ~p busy_dist_port msgs",[tail(BusyDistPortMsgs)]),
    [B] = ssl_apply(HA, erlang, nodes, []),
    [A] = ssl_apply(HB, erlang, nodes, []),
    ?CT_LOG("Microstate accounting for node ~w:", [A]),
    msacc:print(ClientMsacc),
    ?CT_LOG("Microstate accounting for node ~w:", [B]),
    msacc:print(ServerMsacc),
    ?CT_LOG("Stats of B from A: ~p",
           [ssl_apply(HA, net_kernel, node_info, [B])]),
    ?CT_LOG("Stats of A from B: ~p",
           [ssl_apply(HB, net_kernel, node_info, [A])]),
    SchedUtilClient =
        round(1000 * msacc:stats(system_runtime,ClientMsacc) /
                  msacc:stats(system_realtime,ClientMsacc)),
    ServerRealtime = msacc:stats(system_realtime,ServerMsacc),
    SchedUtilServer =
        round(1000 * msacc:stats(system_runtime,ServerMsacc) /
                  ServerRealtime),
    Verdict =
        if
            BusyDistPortMsgs =:= 0 ->
                "";
            is_integer(BusyDistPortMsgs) ->
                " ?";
            true ->
                ?CT_LOG("Stray Msgs: ~p", [BusyDistPortMsgs]),
                " ???"
        end,
    _ = ?REPORT(Prefix++" Sched Utilization Client Mem", MemA, "KByte"),
    _ = ?REPORT(Prefix++" Sched Utilization Server Mem", MemB, "KByte"),
    {comment, ClientComment} =
        ?REPORT(Prefix ++ " Sched Utilization Client" ++ Verdict,
               SchedUtilClient, Verdict ++ " | "),
    {comment, ServerComment} =
        ?REPORT(Prefix++" Sched Utilization Server" ++ Verdict,
               SchedUtilServer, "per mille" ++ Verdict ++ " " ++ MemText,
               round(ServerRealtime / length(ServerMsacc))),
    {comment, ClientComment ++ ServerComment}.

%% Runs on node A and spawns a server on node B
%% We want to avoid getting busy_dist_port as it hides the true SU usage
%% of the receiver and sender.
sched_util_runner(A, B, Effort, _SSL = true, Config) ->
    sched_util_runner(A, B, Effort, 200, Config);
sched_util_runner(A, B, Effort, _SSL = false, Config) ->
    %% We spawn 200 senders that send a message every 10 ms
    %% which should produce a load of 20_000 msgs/s with
    %% payload 1000 bytes each -> 20 MB/s
    sched_util_runner(A, B, Effort, 200, Config);
sched_util_runner(A, B, Effort, Senders, Config) when is_integer(Senders)  ->
    process_flag(trap_exit, true),
    Delay = 2,
    Payload = payload(1000),
    Time = 1000 * Effort,
    [A] = erpc:call(B, erlang, nodes, []),
    MemStart = mem_start(B),
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
    _Clients =
        [spawn_link(
           fun() ->
                   throughput_client(Pid, Delay, Payload)
           end) || Pid <- ServerPids],
    %%
    receive after 1000 -> ok end,
    ServerMsacc ! {start,Tag,self()},
    fs_log(Config, "sched_util_runner.Client.self", self()),
    msacc:start(Time),
    fs_log(Config, "sched_util_runner.Client.msacc:start", ok),
    ClientMsaccStats = msacc:stats(),
    fs_log(Config, "sched_util_runner.Client.msacc.stats", ClientMsaccStats),
    MemResult = mem_stop(MemStart),
    receive after 1000 -> ok end,
    ServerMsacc ! {done,Tag,self()},
    ServerMsaccStats =
        receive
            {'EXIT',ServerMsacc,{result,Tag,Stats}} ->
                Stats;
            {'EXIT',ServerMsacc,Other} ->
                exit({other,ServerMsacc,Other})
        end,
    erlang:system_monitor(self(),[]),
    fs_log(Config, "sched_util_runner.ServerMsaccStats", ServerMsaccStats),
    %%
    {ClientMsaccStats,ServerMsaccStats, busy_dist_port_msgs(), MemResult}.

fs_log(Config, Name, Term) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    DistPrefix = proplists:get_value(ssl_dist_prefix, Config),
    _ = file:write_file(
          filename:join(PrivDir, DistPrefix ++ "_" ++ Name),
          io_lib:format(
            "~p~n",
            [{{erlang:unique_integer([positive,monotonic]),
               os:system_time(1000_000)},
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

throughput_client(Pid, Delay, Payload) ->
    Pid ! Payload,
    receive after Delay -> throughput_client(Pid, Delay, Payload) end.

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

run_mlcm(A, B, Prefix, Effort, HA, HB) ->
    ClientServerPairs = 200,
    [] = ssl_apply(HA, erlang, nodes, []),
    [] = ssl_apply(HB, erlang, nodes, []),
    pong = ssl_apply(HB, net_adm, ping, [A]),
    {{Count, Bytes}, {MemA, MemB, MemText}, Time} =
        ssl_apply(HA, fun () -> mlcm(B, Effort, ClientServerPairs) end),
    ?REPORT_PAL("Data rate", per_s(Bytes bsr 10, Time), per_s("KB")),
    _ = ?REPORT(Prefix++" CPU Margin Mem A", MemA, "KB"),
    _ = ?REPORT(Prefix++" CPU Margin Mem B", MemB, "KB"),
    ?REPORT(
      Prefix++" CPU Margin",
      round(Count/ClientServerPairs/Effort),
      "stones " ++ MemText, Time).

mlcm(Node, Effort, ClientServerPairs) ->
    Delay = 10,
    BlockSize = 512,
    Payloads = mlcm_payloads(BlockSize),
    RunTime = 1000 * Effort,
    MemStart = mem_start(Node),
    Clients =
        [mlcm_client_start(Node, Delay, Payloads) ||
            _ <- lists:seq(1, ClientServerPairs)],
    receive after RunTime -> ok end,
    [Alias ! {Alias,stop} || {_Monitor, Alias} <- Clients],
    Counts =
        [receive
             {'DOWN',Monitor,_,_,{Alias, Count, Bytes}} ->
                 {Count, Bytes};
             {'DOWN',Monitor,_,_,Reason} ->
                 exit(Reason)
         end || {Monitor, Alias} <- Clients],
    MemResult = mem_stop(MemStart),
    {lists_sum_t2(Counts), MemResult, 1000 * RunTime}.

lists_sum_t2(L) -> lists_sum_t2(L, 0, 0).
%%
lists_sum_t2([], Sa, Sb) ->
    {Sa, Sb};
lists_sum_t2([{A, B} | L], Sa, Sb) ->
    lists_sum_t2(L, Sa + A, Sb + B).

%% Returns #{ I := binary(size I * BlockSize) }, I = 0..255
mlcm_payloads(BlockSize) ->
    Bin = rand:bytes(BlockSize),
    lists:foldl(
      fun (N, Payloads) ->
              Payloads#{N => binary:copy(Bin, N)}
      end, #{}, lists:seq(0, 255)).

%%-------

mlcm_client_start(Node, Delay, Payloads) ->
    Parent = self(),
    StartRef = make_ref(),
    {_,Monitor} =
        spawn_monitor(
          fun () ->
                  Alias = alias(),
                  Parent ! {StartRef, Alias},
                  Server = mlcm_server_start(Node, Alias),
                  mlcm_client(Alias, Server, Delay, Payloads, 0, 0)
          end),
    receive
        {StartRef, Alias} ->
            {Monitor, Alias};
        {'DOWN',Monitor,_,_,Reason} ->
            exit(Reason)
    end.

mlcm_client(Alias, Server, Delay, Payloads, Seq, Bytes) ->
    {Time, Index} = mlcm_rand(Delay),
    Payload = maps:get(Index, Payloads),
    PayloadSize = byte_size(Payload),
    receive after Time -> ok end,
    Server ! {Alias, Seq, Payload},
    receive
        {Alias, Seq, Pl} when byte_size(Pl) =:= PayloadSize ->
            mlcm_client(
              Alias, Server, Delay, Payloads, Seq + 1, Bytes + PayloadSize);
        {Alias, stop} ->
            Server ! {Alias, stop, Bytes},
            receive after infinity -> ok end
    end.

%% Approximate normal distribution Index with an average of 6 uniform bytes
%% and use the 7:th byte for uniform Time
mlcm_rand(Delay) ->
    mlcm_rand(6, rand:uniform(Delay bsl (1 + 6*8)) - 1, 0).
%%
mlcm_rand(0, X, I) ->
    Time = X + 1, % 1 .. 2*Delay; average Delay
    Index = abs((I - 3*255) div 3), % 0..255 upper half or normal distribution
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
        {Alias, Seq, _Payload} = Msg when is_integer(Seq) ->
            Alias ! Msg,
            mlcm_server(Alias, Seq + 1, Compute);
        {Alias, stop, _Bytes} = Msg ->
            Compute ! Msg,
            receive after infinity -> ok end
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
    receive {Alias, stop, Bytes} -> exit({Alias, Count, Bytes})
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
              throughput(A, B, Prefix, HA, HB, 500_000 * Effort, 0)
      end, Config).

throughput_64(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, Effort, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 500_000 * Effort, 64)
      end, Config).

throughput_1024(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, Effort, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 500_000 * Effort, 1024)
      end, Config).

throughput_4096(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, Effort, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 200_000 * Effort, 4096)
      end, Config).

throughput_16384(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, Effort, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 100_000 * Effort, 16384)
      end, Config).

throughput_65536(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, Effort, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 50_000 * Effort, 65536)
      end, Config).

throughput_262144(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, Effort, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 10_000 * Effort, 262144)
      end, Config).

throughput_1048576(Config) ->
    run_nodepair_test(
      fun (A, B, Prefix, Effort, HA, HB) ->
              throughput(A, B, Prefix, HA, HB, 2_000 * Effort, 1048576)
      end, Config).

throughput(A, B, Prefix, HA, HB, Packets, Size) ->
    [] = ssl_apply(HA, erlang, nodes, []),
    [] = ssl_apply(HB, erlang, nodes, []),
    io:format("~w(~p, ~p, ~p)~n", [throughput_runner, B, Packets, Size]),
    #{time := Time,
      client_msacc_stats := ClientMsaccStats,
      client_prof := ClientProf,
      server_msacc_stats := ServerMsaccStats,
      server_prof := ServerProf,
      server_gc_before := Server_GC_Before,
      server_gc_after := Server_GC_After,
      mem_a := MemA, mem_b := MemB, mem_text := MemText} =
        ssl_apply(HA, fun () -> throughput_runner(B, Packets, Size) end),
    [B] = ssl_apply(HA, erlang, nodes, []),
    [A] = ssl_apply(HB, erlang, nodes, []),
    ClientMsaccStats =:= undefined orelse
        msacc:print(ClientMsaccStats),
    Overhead =
        50 % Distribution protocol headers (empirical) (TLS+=54)
        + byte_size(erlang:term_to_binary([0|<<>>])), % Benchmark overhead
    Bytes = Packets * (Size + Overhead),
    io:format("~w bytes, ~.4g s~n", [Bytes,Time/1000_000]),
    SizeString = integer_to_list(Size),
    _ = ?REPORT(
          Prefix++" Throughput_" ++ SizeString ++ " Mem A", MemA, "KB"),
    _ = ?REPORT(
          Prefix++" Throughput_" ++ SizeString ++ " Mem B", MemB, "KB"),
    ClientMsaccStats =:= undefined orelse
        ?REPORT(
          Prefix ++ " Sender_RelativeCoreLoad_" ++ SizeString,
          round(msacc:stats(system_runtime, ClientMsaccStats)
                * 1000_000 / Bytes),
          "ps/byte"),
    ServerMsaccStats =:= undefined orelse
        begin
            ?REPORT(
              Prefix ++ " Receiver_RelativeCoreLoad_" ++ SizeString,
              round(msacc:stats(system_runtime, ServerMsaccStats)
                    * 1000_000 / Bytes),
              "ps/byte"),
            msacc:print(ServerMsaccStats)
        end,
    io:format("******* ClientProf:~n", []), prof_print(ClientProf),
    io:format("******* ServerProf:~n", []), prof_print(ServerProf),
    io:format("******* Server GC Before:~n~p~n", [Server_GC_Before]),
    io:format("******* Server GC After:~n~p~n", [Server_GC_After]),
    ?REPORT(
      Prefix ++ " Throughput_" ++ SizeString,
      per_s(Bytes bsr 10, Time), per_s("KB") ++ " " ++ MemText,
      Time).

%% Runs on node A and spawns a server on node B
%%
%% Can be run on two manually started nodes outside CommonTest
%%
throughput_runner(B, Rounds, Size) ->
    Payload = payload(Size),
    A = node(),
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
    MemStart = mem_start(B),
    #{time := Time} = Result =
        throughput_client(ServerPid, ServerMon, Payload, Rounds),
    {MemA, MemB, MemText} = mem_stop(MemStart),
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
    Result#{time := Time,
            client_msacc_stats => MsaccStats,
            client_prof => Prof,
            mem_a => MemA,
            mem_b => MemB,
            mem_text => MemText}.

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
            Result#{time => microseconds(elapsed_time(StartTime))};
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
        try
            TestFun(A, B, Prefix, Effort, HA, HB)
        after
            stop_ssl_node(server, HB, Config)
        end
    after
        stop_ssl_node({client,1}, HA, Config)
    end.

ssl_apply({Handle,_PerfTag}, M, F, Args) ->
    case ssl_dist_test_lib:apply_on_ssl_node(Handle, M, F, Args) of
        {'EXIT',Reason} ->
            error(Reason);
        Result ->
            Result
    end.

ssl_apply({Handle,_PerfTag}, Fun) ->
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
    {EmuPerfArg, PerfStarter} = perf_starter(Name, Config),
    Handle =
        ssl_dist_test_lib:start_ssl_node(
          Name,
          "-pa " ++ Pa ++ EmuPerfArg ++ " +Muacul 0 +IOs false " ++ Args,
          Verbose),
    PerfStarter(Handle);
start_ssl_node(server, Config, Verbose) ->
    Name = proplists:get_value(server_name, Config),
    Args = get_node_args(server_dist_args, Config),
    Pa = filename:dirname(code:which(?MODULE)),
    ServerNode = proplists:get_value(server_node, Config),
    {EmuPerfArg, PerfStarter} = perf_starter(Name, Config),
    Handle =
        erpc:call(
          ServerNode, ssl_dist_test_lib, start_ssl_node,
          [Name,
           "-pa " ++ Pa ++ EmuPerfArg ++ " +Muacul 0 +IOs false " ++ Args,
           Verbose]),
    PerfStarter(Handle).

perf_starter(Name, Config) ->
    Parent  = self(),
    PerfTag = make_ref(),
    case proplists:lookup(perf_record, Config) of
        {_, true} ->
            {" +JPperf true",
             fun (Handle) ->
                     NodeHandle = {Handle,PerfTag},
                     NodePid = ssl_apply(NodeHandle, os, getpid, []),
                     %% The --output option is actually required since
                     %% it seems perf record, when facing a pipe as output
                     %% will per default write the collected data to it
                     PerfCmd =
                         "perf record -p " ++ NodePid ++ " "
                         "--output=" ++ Name ++ ".data --call-graph=lbr",
                     ?CT_PAL("~nPerfCmd: ~s~n", [PerfCmd]),
                     _ = spawn_link(
                           fun () ->
                                   Parent ! {PerfTag, os:cmd(PerfCmd)},
                                   void
                           end),
                     NodeHandle
             end};
        _ ->
            {"",
             fun (Handle) ->
                     Parent ! {PerfTag, none},
                     {Handle,PerfTag}
             end}
    end.

stop_ssl_node({client, _}, {HA,PerfTag}, _Config) ->
    Result = ssl_dist_test_lib:stop_ssl_node(HA),
    perf_result(PerfTag),
    Result;
stop_ssl_node(server, {HB,PerfTag}, Config) ->
    ServerNode = proplists:get_value(server_node, Config),
    Result = erpc:call(ServerNode, ssl_dist_test_lib, stop_ssl_node, [HB]),
    perf_result(PerfTag),
    Result.

perf_result(PerfTag) ->
    receive
        {PerfTag, none} -> ok;
        {PerfTag, PerfResult} ->
            ?CT_PAL("~n"
                    "Perf CWD: ~s~n"
                    "Perf result:~n~s~n",
                    [element(2, file:get_cwd()), PerfResult]), ok
    end.

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
            ""
    end ++ proplists:get_value(ssl_dist_args, Config, "").



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


msacc_available() ->
    msacc:available().

mem_start(B) ->
    MemStartAB = {mem(), erpc:call(B, fun mem/0)},
    {MemStartAB, B}.

mem_stop({MemStartAB, B}) ->
    MemStopAB = {mem(), erpc:call(B, fun mem/0)},
    mem_result(mem_diff(MemStartAB, MemStopAB)).

mem_diff({MemA1, MemB1}, {MemA2, MemB2}) ->
    {MemA2 - MemA1, MemB2 - MemB1}.

mem_result({MemDiffA, MemDiffB}) ->
    MemA = round(MemDiffA / (1 bsl 10)),
    MemB = round(MemDiffB / (1 bsl 10)),
    MemText = io_lib:format("~w|~w KB", [MemA, MemB]),
    {MemA, MemB, MemText}.

memory(Type) ->
    try erlang:memory(Type)
    catch error : notsup ->
            0
    end.

-ifdef(undefined).

mem() ->
    lists:foldl(
      fun ({Type, F}, Acc) ->
              F*memory(Type) + Acc
      end, 0,
      [{total, 1}, {processes, -1}, {atom, -1}, {code, -1},
       {processes_used, 1}, {atom_used, 1}]).

-else.

mem() ->
    {_Current, _MaxSince, MaxEver} =
        traverse(
          fun mem/3,
          [erlang:system_info({allocator_sizes, Alloc})
           || Alloc <- erlang:system_info(alloc_util_allocators)],
          {0, 0, 0}),
    MaxEver - memory(code). % Kind of assuming code stays allocated

%% allocator_sizes traversal fun
mem(
  T = {instance, _, L}, [], Acc)
  when is_list(L) ->
    {tuple_size(T), Acc};
mem(
  T = {_, L}, [{instance, _, _}], Acc)
  when is_list(L) ->
    {tuple_size(T), Acc};
mem(
  T = {blocks, L}, [{_, _}, {instance, _, _}], Acc)
  when is_list(L) ->
    {tuple_size(T), Acc};
mem(
  T = {_, L}, [{blocks, _}, {_, _}, {instance, _, _}], Acc)
  when is_list(L) ->
    {tuple_size(T), Acc};
mem(
  {size, Current, MaxSince, MaxEver},
  [{_, _}, {blocks, _}, {_, _}, {instance, _, _}],
  {C, S, E}) ->
    {0, {C + Current, S + MaxSince, E + MaxEver}};
mem(
  {size, Current},
  [{_, _}, {blocks, _}, {_, _}, {instance, _, _}],
  {C, S, E}) ->
    %% Use Current as Max since we do not have any Max values
    %% XXX future improvement when that gets added to
    %% erlang:system_info(allocator_sizes, _)
    {0, {C + Current, S + Current, E + Current}};
mem(_, _, Acc) ->
    {0, Acc}.

%% Traverse (Fold) over all lists in a deep term;
%% descend into the selected element of a tuple;
%% record the descent Path and supply it to Fun
%%
%% Acc cannot be an integer
traverse(Fun, Term, Acc) ->
    traverse(Fun, Term, [], Acc).
%%
traverse(Fun, Term, Path, Acc) ->
    if
        is_list(Term) ->
            traverse_list(Fun, Term, Path, Acc);
        is_tuple(Term) ->
            case Fun(Term, Path, Acc) of
                {0, NewAcc} ->
                    NewAcc;
                {N, NewAcc} when is_integer(N) ->
                    traverse(Fun, element(N, Term), [Term | Path], NewAcc)
            end;
        true ->
            Acc
    end.

traverse_list(Fun, [Term | Terms], Path, Acc) ->
    NewAcc = traverse(Fun, Term, Path, Acc),
    traverse_list(Fun, Terms, Path, NewAcc);
traverse_list(_Fun, [], _Path, Acc) ->
    Acc.

-endif.
