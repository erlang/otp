%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2012-2025. All Rights Reserved.
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
-module(httpd_bench_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").
-include_lib("kernel/include/file.hrl").

-export([suite/0, all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2, wget_small/1, erl_dummy_small/1, httpc_small/1, wget_big/1,
         erl_dummy_big/1, httpc_big/1]).
-export([httpc_client/1, httpd_lib_client/1, wget_client/1, wget/4]).
-export([handle_http_msg/3]). % httpd_test_lib callback

-define(remote_host, "NETMARKS_REMOTE_HOST").
-define(LF, [10]).
-define(CR, [13]).
-define(CRLF, ?CR ++ ?LF).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {seconds, 10}}].

all() ->
    [
     {group, http_dummy},
     {group, http_inets},
     {group, http_nginx},
     {group, https_inets},
     {group, https_dummy},
     {group, https_nginx},
     {group, http_dummy_keep_alive},
     {group, http_inets_keep_alive},
     {group, http_nginx_keep_alive},
     {group, https_inets_keep_alive},
     {group, https_dummy_keep_alive},
     {group, https_nginx_keep_alive}
    ].

groups() ->
    [
     {http_dummy, [],  client_tests()},
     {http_inets, [],   client_tests()},
     {http_nginx, [],   client_tests()},
     {https_dummy, [],  client_tests()},
     {https_inets, [],  client_tests()},
     {https_nginx, [],  client_tests()},
     {http_dummy_keep_alive, [],  client_tests()},
     {http_inets_keep_alive, [],  client_tests()},
     {http_nginx_keep_alive, [],  client_tests()},
     {https_dummy_keep_alive, [], client_tests()},
     {https_inets_keep_alive, [], client_tests()},
     {https_nginx_keep_alive, [], client_tests()}
    ].


client_tests() ->
    [wget_small,
     erl_dummy_small,
     httpc_small,
     wget_big,
     erl_dummy_big,
     httpc_big
    ].

init_per_suite(Config) ->
    Setup = setup(Config, node()),
    init_ssl(Config),
    Setup ++ [{iter, 10} | Config].

end_per_suite(Config) ->
    [application:stop(App) || App <- [asn1, crypto, public_key, ssl, inets]],
    PeerPid = proplists:get_value(server_pid, Config),
    peer:stop(PeerPid).

%% Init keepalive servers group
init_per_group(Group, Config) when Group == http_dummy_keep_alive;
                                   Group == https_dummy_keep_alive;
                                   Group == http_inets_keep_alive;
                                   Group == https_inets_keep_alive;
                                   Group == http_nginx_keep_alive;
                                   Group == https_nginx_keep_alive ->
    Version = http_version(Group),
    start_web_server(Group,
                     [{keep_alive, true},
                      {reuse_sessions, false},
                      {http_version, Version},
                      {http_opts,[{version, Version}]},
                      {http_headers, [{"connection", "keep-alive"}]},
                      {httpc_opts, [{keep_alive_timeout, 1500},
                                    {max_keep_alive_length, ?config(iter, Config)}]}
                     | Config]);
%% Init non-keepalive servers group
init_per_group(Group, Config)  when Group == http_dummy;
                                    Group == https_dummy;
                                    Group == http_inets;
                                    Group == https_inets;
                                    Group == http_nginx;
                                    Group == https_nginx ->
    Version = http_version(Group),
    start_web_server(Group,
                     [{keep_alive, false},
                      {reuse_sessions, false},
                      {http_version, Version},
                      {http_headers, [{"connection", "close"}]},
                      {http_opts,[{version, Version}]},
                      {httpc_opts, [{keep_alive_timeout, 0}, {max_keep_alive_length, 0}]}
                     | Config]);


init_per_group(_, Config) ->
    Config.

end_per_group(Group, Config) ->
    stop_web_server(Group, Config).

init_per_testcase(TestCase, Config) when TestCase == httpc_small;
                                         TestCase == httpc_big
                                         ->
    Opts = ?config(httpc_opts, Config),
    inets:start(httpc, [{profile, TestCase}, {socket_opts, [{nodelay, true}]}]),
    httpc:set_options(Opts, TestCase),
    [{profile, TestCase} | proplists:delete(profile, Config)];

init_per_testcase(_, Config) ->
    Config.
end_per_testcase(TestCase, _Config) when TestCase == httpc_small;
                                         TestCase == httpc_big ->
    ok = inets:stop(httpc, TestCase);
end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

erl_dummy_small(Config) when is_list(Config) ->
    {ok, Result} = run_test(httpd_lib_client, "1k_file", Config),
    notify(Result, Config, "erl_1k_file").

erl_dummy_big(Config)  when is_list(Config) ->
    {ok, Result} = run_test(httpd_lib_client, "1M_file", Config),
    notify(Result, Config, "erl_1M_file").

wget_small(Config) when is_list(Config) ->
    ensure_non_windows_and_executable_exists("wget",
        fun() ->
            {ok, Result} = run_test(wget_client, "1k_file", Config),
            notify(Result, Config, "wget_1k_file")
        end).

wget_big(Config)  when is_list(Config) ->
    ensure_non_windows_and_executable_exists("wget",
        fun() ->
            {ok, Result} = run_test(wget_client, "1M_file", Config),
            notify(Result, Config, "wget_1M_file")
        end).

httpc_small(Config) when is_list(Config) ->
    {ok, Result} = run_test(httpc_client, "1k_file", Config),
    notify(Result, Config, "httpc_1k_file").

httpc_big(Config)  when is_list(Config) ->
    {ok, Result} = run_test(httpc_client, "1M_file", Config),
    notify(Result, Config, "httpc_1M_file").

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
%%

%%--------------------------------------------------------------------
%% Report benchmark results  ------------------------------------------------
%%--------------------------------------------------------------------

notify({TestPerSec, _MBps}, Config, Suffix) ->
    Name = lists:concat([?config(protocol,Config), " ",
                         server_name(Config, [dummy_pid, httpd_pid, nginx_port]),
                         "", Suffix]),
    ct:comment("~p Req/s", [TestPerSec]),
    ct_event:notify(#event{name = benchmark_data,
                           data=[{value, TestPerSec},
                                 {suite, ?MODULE},
                                 {name, Name}]}),
    ok.
%%--------------------------------------------------------------------
%% Setup erlang nodes  ------------------------------------------------
%%--------------------------------------------------------------------

server_name(Config, [Server | Rest]) ->
    case proplists:get_value(Server, Config) of
        undefined ->
            server_name(Config, Rest);
        _ ->
            server_name(Server)
    end.

server_name(httpd_pid) ->
    "inets";
server_name(nginx_port) ->
    "nginx";
server_name(dummy_pid) ->
    "erlang".

create_test_peer_opts() ->
    {Host, IsRemote} = case os:getenv(?remote_host) of
                           false -> {net_adm:localhost(), false};
                           RemHost -> {RemHost, true}
                       end,
    PeerArgs = case init:get_argument(pa) of
                   {ok, PaPaths} -> ["-pa"] ++ lists:concat(PaPaths);
                   _ -> []
               end,
    Prog = case os:find_executable("erl") of
               false -> "erl";
               P -> P
           end,
    PeerOpts = #{
                 name => inets_perf_server,
                 args => PeerArgs,
                 peer_down => crash,
                 exec => Prog
                },
    PeerOpts1 = case IsRemote of
                    true -> PeerOpts#{host => Host};
                    false -> PeerOpts % on localhost do not specify 'host' in peer:start
                end,
    {PeerOpts1, Host}.

setup_peers_after_start(Node) ->
    Path = code:get_path(),
    (Node =/= node()) andalso
        begin
            true = rpc:call(Node, code, set_path, [Path]),
            [ensure_started(Node, App) || App <- [asn1, crypto, public_key, ssl, inets]]
        end,
    [ensure_started(node(), App) || App <- [asn1, crypto, public_key, ssl, inets]],
    (Node =:= node()) andalso restrict_schedulers(client).

setup(_Config, nonode@nohost) ->
    exit(dist_not_enabled);
setup(_Config, _LocalNode) ->
    {PeerOpts, Host} = create_test_peer_opts(),
    {ok, PeerPid, Node} = peer:start(PeerOpts),
    setup_peers_after_start(Node),
    %% Return also the pid for peer control
    [{server_node, Node}, {server_host, Host}, {server_pid, PeerPid}].

ensure_started(Node, App) ->
    ok = rpc:call(Node, application, ensure_started, [App]).

restrict_schedulers(Type) ->
    %% We expect this to run on 8 core machine
    Extra0 = 1,
    Extra =  if (Type =:= server) -> -Extra0; true -> Extra0 end,
    Scheds = erlang:system_info(schedulers),
    erlang:system_flag(schedulers_online, (Scheds div 2) + Extra).

%%--------------------------------------------------------------------
%% Setup TLS input files  ------------------------------------------------
%%--------------------------------------------------------------------

init_ssl(Config) ->
    PDir = ?config(priv_dir, Config),
    httpd_bench_certs:make_cert_files(PDir).

cert_opts(Config) ->
    PrivDir = ?config(priv_dir, Config),
    ServerCaCertFile = filename:join([PrivDir, "server-cacerts.pem"]),
    ServerCertFile = filename:join([PrivDir, "server-cert.pem"]),
    ServerKeyFile = filename:join([PrivDir, "server-key.pem"]),
    [
     {server_verification_opts, [
                                 {cacertfile, ServerCaCertFile},
                                 {ciphers, ["ECDHE-RSA-AES256-GCM-SHA384", "TLS_AES_256_GCM_SHA384"]},
                                 {certfile, ServerCertFile},
                                 {keyfile, ServerKeyFile}
                                ]},
     {client_verification_opts, [
                                 {cacertfile, ServerCaCertFile}
                                ]}
    ].

%%--------------------------------------------------------------------
%% Run clients  ------------------------------------------------
%%--------------------------------------------------------------------

run_test(Client, File, Config) ->
    Parent = self(),
    Pid = spawn(
            fun() ->
                    receive
                        go ->
                            Parent ! {self(), do_runs(Client, [{file, File} | Config])}
                    end
            end),
    Pid ! go,
    receive
        {Pid, #{req_s := Tps, bytes_s := Bps}} ->
            ct:log("Req/s=~p Bytes/s=~p", [Tps, Bps]),
            {ok, {Tps, Bps}}
    end.

-type client_impl() :: fun(({create_args, Config :: proplists:proplist()}) -> map())
                     | fun(({init, map()}) -> ok)
                     | fun(({run, map(), N :: integer()}) -> ok).

-spec do_runs(Client :: client_impl(), Config :: proplists:proplist()) ->
          #{req_s => integer(), bytes_s => integer()}.
do_runs(Client, Config) ->
    N = ?config(iter, Config),
    DataDir = ?config(data_dir, Config),
    File = ?config(file, Config),
    Name = filename:join(DataDir, File),

    Args = ?MODULE:Client({create_args, Config}),
    ?MODULE:Client({init, Args}),
    Run = fun() -> ok = ?MODULE:Client({run, Args, N}) end,

    {ok, Info} = file:read_file_info(Name, []),
    Length = Info#file_info.size,
    {TimeInMicro, _} = timer:tc(Run),
    ReqPerSecond = (1_000_000 * N) div TimeInMicro,
    BytesPerSecond = (1_000_000 * N * Length) div TimeInMicro,
    #{req_s => ReqPerSecond, bytes_s => BytesPerSecond}.

%% Client handler for httpc-based test cases
%% httpc_client {create_args, Config} is called once with the config, to create args which will be then passed
%% again into httpc_client with {init, Args}, which are then passed into httpc_client with {run, Args, N}.
httpc_client({create_args, Config}) ->
    File = ?config(file, Config),
    Protocol = ?config(protocol, Config),
    Profile = ?config(profile, Config),
    URL = (?config(urlfun,Config))(File),
    Headers =  ?config(http_headers, Config),
    HTTPOpts = ?config(http_opts, Config)
        ++ case Protocol of
               "http" -> [];
               "https" -> % httpc would like to know more about certificates used in the test
                   AllCertOpts = proplists:get_value(client_verification_opts, cert_opts(Config)),
                   SSLOpts = [
                              {verify, verify_peer}, % this is the default
                              {cacertfile, proplists:get_value(cacertfile, AllCertOpts)}
                             ],
                   [{ssl, SSLOpts}]
           end,
    #{protocol => Protocol, profile => Profile, url => URL, headers => Headers, http_opts => HTTPOpts};
httpc_client({init, #{profile := Profile, url := URL, headers := Headers, http_opts := HTTPOpts}}) ->
    %% Make sure pipelining feature will kick in when appropriate.
    {ok, {{_ ,200, "OK"}, _,_}} = httpc:request(
                                    get,{URL, Headers}, HTTPOpts,
                                    [{body_format, binary}, {socket_opts, [{nodelay, true}]}],
                                    Profile),
    ct:sleep(1_000);
httpc_client({run, _, 0}) ->
    ok;
httpc_client({run, Args = #{profile := Profile, url := URL, headers := Headers, http_opts := HTTPOpts}, N}) ->
    {ok, {{_ ,200,"OK"}, _,_}} = httpc:request(
                                   get,{URL, Headers}, HTTPOpts,
                                   [{body_format, binary}, {socket_opts, [{nodelay, true}]}],
                                   Profile),
    httpc_client({run, Args, N-1}).

%% Client handler based on httpd_test_lib
%% httpd_lib_client {create_args, Config} is called once with the config, to create args which will be then passed
%% again into httpd_lib_client with {init, Args}, which are then passed into httpd_lib_client with {run, Args, N}.
httpd_lib_client({create_args, Config}) ->
    File = ?config(file, Config),
    KeepAlive = ?config(keep_alive, Config),
    Host = ?config(server_host, Config),
    Port = ?config(port, Config),
    ReuseSession = ?config(reuse_sessions, Config),
    {Type, Opts} =
        case ?config(protocol, Config) of
            "http" ->
                {ip_comm, [{active, true}, {mode, binary},{nodelay, true}]};
            "https" ->
                SSLOpts =  proplists:get_value(client_verification_opts, cert_opts(Config)),
                {ssl, [{active, true}, {mode, binary}, {nodelay, true},
                       {reuse_sessions, ReuseSession} | SSLOpts]}

        end,
    Version = ?config(http_version, Config),
    Request = case KeepAlive of
                  true ->
                      http_request("GET /" ++ File ++ " ", Version, Host, {"connection:keep-alive\r\n", ""});
                  false ->
                      http_request("GET /" ++ File ++ " ", Version, Host)
              end,

    Args = #{keepalive => KeepAlive, type => Type, version => Version,
             request => Request, host => Host, port => Port, opts => Opts},
    httpd_lib_client({run, Args, 1}), % warm-up run,
    Args;
httpd_lib_client({init, #{type := Type, version := Version, request := Request, host := Host,
                          port := Port, opts := Opts}}) ->
    ok = httpd_test_lib:verify_request(Type, Host,
                                       Port,
                                       Opts, node(),
                                       Request,
                                       [{statuscode, 200}, {version, Version}],
                                       infinity),
    ct:sleep(1_000);
httpd_lib_client({run, _, 0}) ->
    ok;
httpd_lib_client({run, #{keepalive := true, type := Type, version := Version, request := Request,
                         host := Host, port := Port, opts := Opts}, N}) ->
    ok = httpd_test_lib:verify_request_N(Type, Host,
                                         Port, Opts, node(), Request,
                                         [{statuscode, 200}, {version, Version}], infinity, N);
httpd_lib_client({run, Args = #{keepalive := false, type := Type, version := Version, request := Request,
                                host := Host, port := Port, opts := Opts}, N}) ->
    ok = httpd_test_lib:verify_request(Type, Host,
                                       Port, Opts, node(), Request,
                                       [{statuscode, 200}, {version, Version}], infinity),
    httpd_lib_client({run, Args, N - 1}).

%% Client handler for wget-based test cases
%% wget_client {create_args, Config} is called once with the config, to create args which will be then passed
%% again into wget_client with {init, Args}, which are then passed into wget_client with {run, Args, N}.
wget_client({create_args, Config}) ->
    File = ?config(file, Config),
    URL = (?config(urlfun,Config))(File),
    KeepAlive = ?config(keep_alive, Config),
    PrivDir = ?config(priv_dir, Config),
    Protocol = ?config(protocol, Config),
    Iter = ?config(iter, Config),
    FileName = filename:join(PrivDir, "wget_req"),
    ProtocolOpts = case Protocol of
                       "http" -> [];
                       "https" -> proplists:get_value(client_verification_opts, cert_opts(Config))
                   end,
    wget_req_file(FileName,URL,Iter),
    #{keepalive => KeepAlive, filename => FileName, url => URL, protocol => Protocol,
      protocol_opts => ProtocolOpts, iter => Iter};
wget_client({init, _}) ->
    ok;
wget_client({run, #{keepalive := KeepAlive, filename := WgetFile, protocol := Protocol,
                    protocol_opts := ProtocolOpts}, _N}) ->
    process_flag(trap_exit, true),
    Cmd = wget_N(KeepAlive, WgetFile, Protocol, ProtocolOpts),
    %%ct:log("Wget cmd: ~p", [Cmd]),
    Port = open_port({spawn, Cmd}, [stderr_to_stdout]),
    wait_for_wget(Port).


%%--------------------------------------------------------------------
%% Start/stop servers  ------------------------------------------------
%%--------------------------------------------------------------------
start_web_server(Group, Config) when Group == http_dummy;
                                     Group == http_dummy_keep_alive ->
    start_http_test_lib_server("http", Config);

start_web_server(Group, Config) when Group == https_dummy;
                                     Group == https_dummy_keep_alive ->
    start_http_test_lib_server("https", Config);

start_web_server(Group, Config) when Group == http_inets;
                                     Group == http_inets_keep_alive ->
    start_inets_server("http", [], Config);

start_web_server(Group, Config) when Group == https_inets;
                                     Group == https_inets_keep_alive ->
    Opts = proplists:get_value(server_verification_opts, cert_opts(Config)),
    ReuseSessions = ?config(reuse_sessions, Config),
    SSLConfHttpd = [{socket_type,
                     {ssl, [{nodelay, true}, {reuse_sessions, ReuseSessions} | Opts]}
                    }],
    start_inets_server("https", SSLConfHttpd, Config);

start_web_server(Group, Config)  when Group == http_nginx;
                                      Group == http_nginx_keep_alive ->
    ensure_non_windows_and_executable_exists("nginx",
        fun() -> start_nginx("http",  Config) end);

start_web_server(Group, Config)  when Group == https_nginx;
                                      Group == https_nginx_keep_alive ->
    ensure_non_windows_and_executable_exists("nginx",
        fun() -> start_nginx("https", cert_opts(Config) ++ Config) end).

ensure_non_windows_and_executable_exists(Executable, SuccessFn) ->
    case os:type() of
        {win32, _} ->
            {skip, "skip: not running the benchmark on Windows"};
        _ ->
            case os:find_executable(Executable) of
                false -> {skip, "skip: executable not found: " ++ Executable};
                _Filename  -> SuccessFn()
            end
    end.

start_inets_server(Protocol, ConfHttpd, Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    Node = ?config(server_node, Config),
    Host = ?config(server_host, Config),
    HTTPVersion = ?config(http_version, Config),
    Conf = [httpd, [{port,0},
                    {http_version, HTTPVersion},
                    {ipfamily, inet},
                    {server_name, net_adm:localhost()}, % also the default
                    {server_root, PrivDir},
                    {document_root, DataDir},
                    {keep_alive, ?config(keep_alive, Config)},
                    {keep_alive_timeout, 360}
                   | ConfHttpd]],
    {ok, Pid} = rpc:call(Node, inets, start, Conf),
    Port = proplists:get_value(port,  rpc:call(Node, httpd, info, [Pid])),
    F = fun(File) ->
                lists:concat([Protocol, "://", Host, ":", Port, "/", File])
        end,
    %% Return CT config (init_per_group result)
    [{httpd_pid, Pid}, {urlfun, F}, {protocol, Protocol}, {port, Port} | Config].

start_http_test_lib_server("http"= Protocol, Config) ->
    HTTPVersion = ?config(http_version, Config),
    Node = ?config(server_node, Config),
    %%DataDir= ?config(data_dir, Config),
    Host = ?config(server_host, Config),
    Conf = [
            %%{big, filename:join(DataDir, "1M_file")},
            %%{small, filename:join(DataDir, "1k_file")},
            {big, {gen,  crypto:strong_rand_bytes(1_000_000)}},
            {small, {gen,  crypto:strong_rand_bytes(1_000)}},
            {http_version, HTTPVersion},
            {keep_alive,  ?config(keep_alive, Config)}
           ],
    {Pid, Port} = rpc:call(Node, http_test_lib, dummy_server, [ip_comm, inet, [{content_cb, ?MODULE}, {conf, Conf}]]),
    F = fun(File) ->
                lists:concat([Protocol,"://",Host,":",Port,"/",File])
        end,
    %% Return CT config (init_per_group result)
    [{dummy_pid, Pid}, {urlfun, F}, {protocol, Protocol}, {port, Port} | Config];

start_http_test_lib_server("https" = Protocol, Config) ->
    HTTPVersion = ?config(http_version, Config),
    Node = ?config(server_node, Config),
    %% DataDir= ?config(data_dir, Config),
    Host = ?config(server_host, Config),
    SSLOpts =  proplists:get_value(server_verification_opts, cert_opts(Config)),
    Opts = [{active, true}, {nodelay, true} | SSLOpts],
    Conf = [%%{big, filename:join(DataDir, "1M_file")},
            %%{small, filename:join(DataDir, "1k_file")},
            {big, {gen, crypto:strong_rand_bytes(1_000_000)}},
            {small, {gen, crypto:strong_rand_bytes(1_000)}},
            {http_version, HTTPVersion},
            {keep_alive, ?config(keep_alive, Config)}
           ],
    {Pid, Port} = rpc:call(Node, http_test_lib, dummy_server,
                           [ssl, inet, [{ssl, Opts}, {content_cb, ?MODULE}, {conf, Conf}]]),
    F = fun(File) ->
                lists:concat([Protocol,"://",Host,":",Port,"/",File])
        end,
    [{dummy_pid,Pid},{urlfun,F},{protocol,Protocol},{port,Port} | Config].

start_nginx(Protocol, Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    Host = ?config(server_host, Config),
    Port = inet_port(node()),

    ConfFile = filename:join(PrivDir, "nginx-" ++ Protocol ++ ".conf"),
    nginx_conf(ConfFile, [{port, Port}, {protocol, Protocol} | Config]),
    Cmd = "nginx -c " ++ ConfFile,
    NginxPort =  open_port({spawn, Cmd}, [{cd, DataDir}, stderr_to_stdout]),

    F = fun(File) ->
                lists:concat([Protocol,"://",Host,":",Port,"/",File])
        end,

    wait_for_nginx_up(Host, Port),

    [{port, Port},{nginx_port, NginxPort},{urlfun,F},{protocol, Protocol} | Config ].

stop_nginx(Config)->
    PrivDir = ?config(priv_dir, Config),
    {ok, Bin} = file:read_file(filename:join(PrivDir, "nginx.pid")),
    Pid = string:strip(binary_to_list(Bin), right, $\n),
    Cmd = "kill " ++ Pid,
    os:cmd(Cmd).

stop_web_server(Group, Config) when  Group == http_inets;
                                     Group == http_inets_keep_alive;
                                     Group == https_inets;
                                     Group == https_inets_keep_alive ->
    ServerNode = ?config(server_node, Config),
    rpc:call(ServerNode, inets, stop, [httpd, ?config(httpd_pid, Config)]);
stop_web_server(Group, Config) when  Group == http_dummy;
                                     Group == http_dummy_keep_alive;
                                     Group == https_dummy;
                                     Group == https_dummy_keep_alive ->
    stop_dummy_server(Config);
stop_web_server(Group, Config) when  Group == http_nginx;
                                     Group == http_nginx_keep_alive;
                                     Group == https_nginx;
                                     Group == https_nginx_keep_alive ->
    stop_nginx(Config).

stop_dummy_server(Config) ->
    case ?config(dummy_pid, Config) of
        Pid when is_pid(Pid) -> exit(Pid, kill);
        _ -> ok
    end.

%%--------------------------------------------------------------------
%% Misc  ------------------------------------------------
%%--------------------------------------------------------------------
http_request(Request, "HTTP/1.1" = Version, Host, {Headers, Body}) ->
    Request ++ Version ++ "\r\nhost:" ++ Host ++ "\r\n" ++ Headers ++ "\r\n" ++ Body;
http_request(Request, Version, _, {Headers, Body}) ->
    Request ++ Version ++ "\r\n" ++ Headers  ++ "\r\n" ++ Body.

http_request(Request, "HTTP/1.1" = Version, Host) ->
    Request ++ Version ++ "\r\nhost:" ++ Host  ++ "\r\n\r\n";
http_request(Request, Version, _) ->
    Request ++ Version ++ "\r\n\r\n".

http_version(_) ->
    "HTTP/1.1".

inet_port(Node) ->
    {Port, Socket} = do_inet_port(Node),
    rpc:call(Node, gen_tcp, close, [Socket]),
    Port.

do_inet_port(Node) ->
    {ok, Socket} = rpc:call(Node, gen_tcp, listen, [0, [{reuseaddr, true}]]),
    {ok, Port} = rpc:call(Node, inet, port, [Socket]),
    {Port, Socket}.

%%--------------------------------------------------------------------
%% Dummy server callbacks  ------------------------------------------------
%%--------------------------------------------------------------------

handle_request(CB, S, "/1M_file" ++ _, Opts) ->
    Name = proplists:get_value(big, Opts),
    KeepAlive = proplists:get_value(keep_alive, Opts),
    do_handle_request(CB, S, Name, Opts, KeepAlive);
handle_request(CB, S, "/1k_file" ++ _, Opts) ->
    Name = proplists:get_value(small, Opts),
    KeepAlive = proplists:get_value(keep_alive, Opts),
    do_handle_request(CB, S, Name, Opts, KeepAlive).

do_handle_request(CB, S, Name, Opts, KeepAlive) when is_list(Name) ->
    Version = proplists:get_value(http_version, Opts),
    {ok, Fdesc} = file:open(Name, [read, binary]),
    {ok, Info} = file:read_file_info(Name, []),
    Length = Info#file_info.size,
    Response = response_status_line_and_headers(Version, "Content-Length:"
                                                ++ integer_to_list(Length) ++ ?CRLF, keep_alive(KeepAlive)),
    CB:send(S, Response),
    send_file(CB, S, Fdesc);
do_handle_request(CB, S, {gen, Data}, Opts, KeepAlive) ->
    Version = proplists:get_value(http_version, Opts),
    Length = byte_size(Data),
    Response = response_status_line_and_headers(Version, "Content-Length:"
                                                ++ integer_to_list(Length) ++ ?CRLF, keep_alive(KeepAlive)),
    CB:send(S, Response),
    send_file(CB, S, {gen, Data}).

send_file(CB, S, {gen, Data})  ->
    CB:send(S, Data);
%% ChunkSize = 64*1024,
%% case byte_size(Data) of
%%      N when N > ChunkSize ->
%%          <<Chunk:N/binary, Rest/binary>> = Data,
%%          %%{Chunk, Rest} = lists:split(N, Data),
%%          CB:send(S, Chunk),
%%          send_file(CB, S, {gen, Rest});
%%      _ ->
%%          CB:send(S, Data)
%% end;

send_file(CB, S, FileDesc) ->
    case file:read(FileDesc, 64*1024) of
        {ok, Chunk} ->
            CB:send(S, Chunk),
            send_file(CB, S, FileDesc);
        eof ->
            file:close(FileDesc),
            ok
    end.

response_status_line_and_headers(Version, Headers,  ConnectionHeader) ->
    StatusLine = [Version, " ", "200 OK", ?CRLF],
    [StatusLine, Headers, ConnectionHeader, ?CRLF].

keep_alive(true)->
    "Connection:keep-alive\r\n";
keep_alive(false) ->
    "Connection:close\r\n".

handle_http_msg({_Method, RelUri, _, {_, _Headers}, _Body}, Socket, Conf) ->
    handle_request(connect_cb(Socket), Socket, RelUri, Conf),
    case proplists:get_value(keep_alive, Conf) of
        true -> <<>>;
        false -> stop
    end.

%% #ssl_socket{} arity has increased in later versions of OTP, not arity 3 anymore
connect_cb(SSLSocket) when element(1, SSLSocket) =:= sslsocket ->
    ssl;
connect_cb(_) ->
    gen_tcp.

%%--------------------------------------------------------------------
%% Setup wget  ------------------------------------------------
%%--------------------------------------------------------------------
wget_req_file(FileName, Url, Iter) ->
    {ok, File} = file:open(FileName, [write]),
    write_urls(File, Url, Iter).

write_urls(File, Url, 1) ->
    file:write(File, Url),
    file:close(File);
write_urls(File, Url, N) ->
    file:write(File, Url),
    file:write(File, "\n"),
    write_urls(File, Url, N-1).

wait_for_wget(Port) ->
    receive
        {Port, {data, _Data}} when is_port(Port) ->
            wait_for_wget(Port);
        {Port, closed} ->
            ok;
        {'EXIT', Port, _Reason} ->
            ok
    end.

wget_N(KeepAlive, WegetFile, "http", _ProtocolOpts) ->
    "wget -i " ++ WegetFile ++ " " ++ wget_keep_alive(KeepAlive) ++
        " --no-cache --timeout=120" ;
wget_N(KeepAlive, WegetFile, "https", ProtocolOpts) ->
    "wget -i " ++ WegetFile ++ " " ++ wget_keep_alive(KeepAlive)
        ++ wget_cacert(ProtocolOpts) ++
        " --no-cache --timeout=120".

wget(KeepAlive, URL, "http", _ProtocolOpts) ->
    "wget " ++ URL ++ " " ++ wget_keep_alive(KeepAlive) ++
        " --no-cache --timeout=120" ;
wget(KeepAlive, URL, "https", ProtocolOpts) ->
    "wget " ++ URL ++ " " ++ wget_keep_alive(KeepAlive)
        ++ wget_cacert(ProtocolOpts) ++
        " --no-cache --timeout=120".

wget_keep_alive(true)->
    "";
wget_keep_alive(false) ->
    "--no-http-keep-alive ".

wget_cacert(ProtocolOpts) ->
    "--ca-certificate=" ++ proplists:get_value(cacertfile, ProtocolOpts) ++ " ".

%%--------------------------------------------------------------------
%% Setup nginx  ------------------------------------------------
%%--------------------------------------------------------------------
nginx_conf(ConfFile, Config) ->
    Protocol = ?config(protocol, Config),
    ConfIodata = [
                  format_nginx_conf(nginx_global(Config), 0),
                  format_nginx_conf(nginx_events_section(Config), 0),
                  format_nginx_conf(nginx_httpserver_section(Protocol, Config), 0)
                 ],
    ok = file:write_file(ConfFile, ConfIodata).

%% Output keys or string values, terminated with semicolon and newline
format_nginx_conf(Directives, Indent) when is_list(Directives) ->
    lists:map(fun(D) -> format_nginx_conf(D, Indent) end, Directives);
format_nginx_conf({section, Name, SubDirectives}, Indent) ->
    IndentStr = [$\s || _ <- lists:seq(1, Indent)],
    [IndentStr, Name, " {\n",
     format_nginx_conf(SubDirectives, Indent + 4),
     IndentStr, "}\n"];
format_nginx_conf({Key, Value}, Indent) ->
    IndentStr = [$\s || _ <- lists:seq(1, Indent)],
    [IndentStr, Key, " ", Value, ";\n"].

%% Nginx global section
nginx_global(Config) ->
    PrivDir = ?config(priv_dir, Config),
    [
     {"pid", filename:join(PrivDir, "nginx.pid")},
     {"error_log", filename:join(PrivDir, "nginx.pid")},
     {"worker_processes", "1"}
    ].

nginx_events_section(_Config) ->
    {section, "events", [
                         {"worker_connections", "1024"}
                        ]}.

nginx_httpserver_section("http", Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    Port = ?config(port, Config),
    {section, "http", nginx_defaults(PrivDir) ++ [
                                                  {section, "server", [
                                                                       {"root", DataDir},
                                                                       {"listen", integer_to_list(Port)},
                                                                       {section, "location /", [
                                                                                                {"try_files", "$uri $uri/ /index.html"}
                                                                                               ]}
                                                                      ]}
                                                 ]};
nginx_httpserver_section("https", Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    Port = ?config(port, Config),
    SSLOpts = ?config(server_verification_opts, Config),
    Ciphers = proplists:get_value(ciphers, SSLOpts),
    ReuseSession = ?config(reuse_sessions, Config),
    {section, "http", nginx_defaults(PrivDir) ++
         [{section, "server", [
                               {"root", DataDir},
                               {"listen", integer_to_list(Port) ++ " ssl"},
                               {"ssl_certificate", ?config(certfile, SSLOpts)},
                               {"ssl_certificate_key", ?config(keyfile, SSLOpts)},
                               {"ssl_protocols", "TLSv1 TLSv1.1 TLSv1.2"},
                               %% Nginx sends this string to OpenSSL in its format described here:
                               %% https://docs.openssl.org/3.0/man1/openssl-ciphers/#cipher-list-format
                               %% Ciphers list separated by ":" is a valid format
                               {"ssl_ciphers", string:join(Ciphers, ":")},
                               {"ssl_prefer_server_ciphers", "on"},
                               {"ssl_session_cache", nginx_reuse_session(ReuseSession)},
                               {section, "location /", [
                                                        {"try_files", "$uri $uri/ /index.html"}
                                                       ]}
                              ]}
         ]}.

%% Searches for a file in several directories, and returns the first found file path, or 'not_found'
find_file(_, []) -> not_found;
find_file(FileName, [Dir | Rest]) ->
    TryThisFile = filename:join(Dir, FileName),
    case filelib:is_file(TryThisFile) of
        true -> TryThisFile;
        false -> find_file(FileName, Rest)
    end.

nginx_defaults(PrivDir) ->
    SearchDirs = ["/etc/nginx", "/opt/Homebrew/etc/nginx", "/usr/local/etc/nginx"],
    MimeTypes = case find_file("mime.types", SearchDirs) of
                    not_found -> erlang:throw({file_not_found, "mime.types", SearchDirs});
                    Filename -> Filename
                end,
    [
     %% Set temp and cache file options that will otherwise default to
     %% restricted locations accessible only to root.
     {"client_body_temp_path", filename:join(PrivDir, "client_body")},
     {"fastcgi_temp_path", filename:join(PrivDir, "fastcgi_temp")},
     {"proxy_temp_path", filename:join(PrivDir, "proxy_temp")},
     {"scgi_temp_path", filename:join(PrivDir, "scgi_temp")},
     {"uwsgi_temp_path", filename:join(PrivDir, "uwsgi_temp_path")},
     {"access_log", filename:join(PrivDir, "access.log")},
     {"error_log", filename:join(PrivDir, "error.log")},
     %% Standard options
     {"sendfile", "on"},
     {"tcp_nopush", "on"},
     {"tcp_nodelay", "on"},
     {"keepalive_timeout", "360"},
     {"types_hash_max_size", "2048"},
     {"include", MimeTypes},
     {"default_type", "application/octet-stream"}
    ].

nginx_reuse_session(true) ->
    "on";
nginx_reuse_session(false) ->
    "off".

wait_for_nginx_up(Host, Port) ->
    case gen_tcp:connect(Host, Port, []) of
        {ok, Socket} ->
            gen_tcp:close(Socket);
        _  ->
            ct:sleep(100),
            wait_for_nginx_up(Host, Port)
    end.
