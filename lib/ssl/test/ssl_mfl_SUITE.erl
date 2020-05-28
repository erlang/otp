%%
-module(ssl_mfl_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

-define(SLEEP, 500).
%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [{group, 'tlsv1.3'},
     {group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'dtlsv1.2'},
     {group, 'dtlsv1'}].

groups() ->
    [{'tlsv1.3', [], tls_mfl_1_3()},
     {'tlsv1.2', [], tls_mfl()},
     {'tlsv1.1', [], tls_mfl()},
     {'tlsv1', [], tls_mfl()},
     {'dtlsv1.2', [], tls_mfl()},
     {'dtlsv1', [], tls_mfl()}
    ].

tls_mfl_common() ->
    [mfl_client_option, mfl_server_option, mfl_openssl_client, mfl_openssl_server, handshake_continue].

tls_mfl() ->
    tls_mfl_common() ++ [reuse_session, reuse_session_erlang_server, reuse_session_erlang_client].

tls_mfl_1_3() ->
    tls_mfl_common().

init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
            ssl_test_lib:clean_start(),
            ssl:clear_pem_cache(),
	    Config = ssl_test_lib:make_rsa_cert(Config0),
	    ssl_test_lib:cert_options(Config)
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group_openssl(GroupName, Config).

end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(TestCase, Config) when TestCase == mfl_openssl_server;
                                         TestCase == mfl_openssl_client;
                                         TestCase == reuse_session_erlang_server;
                                         TestCase == reuse_session_erlang_client ->
    case os:cmd("openssl version") of
        %% Max fragmentation support introduced in OpenSSL 1.1.1
	"OpenSSL 1.1.1" ++ _ = OpenSSLVersion ->
            case lists:member({protocol, dtls}, Config) of
                true ->
                    %% Fixed but not yet released https://github.com/openssl/openssl/commit/dfbaef6
                    {skip, "Broken DTLS max fragmentation support in "++OpenSSLVersion};
                false ->
                    init_per_testcase(all, Config)
            end;
        Other ->
            {skip, "No/unknown max fragmentation support in "++Other}
    end;
init_per_testcase(_TestCase, Config) ->
    ct:timetrap({seconds, 10}),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
%--------------------------------------------------------------------------------
%% check max_fragment_length option on the client is accepted
%% and both sides can successfully send > MFL
mfl_client_option(Config) when is_list(Config) ->
    ok = run_mfl_handshake(Config,  512),
    ok = run_mfl_handshake(Config, 1024),
    ok = run_mfl_handshake(Config, 2048),
    ok = run_mfl_handshake(Config, 4096),
    ok = run_mfl_handshake(Config, undefined),
    ok.

%--------------------------------------------------------------------------------
%% check max_fragment_length option on the server is ignored
%% and both sides can successfully send > 512 bytes
mfl_server_option(Config) when is_list(Config) ->
    Data = "mfl_server_options " ++ lists:duplicate(512, $x),
    run_mfl_handshake(Config, undefined, Data, [], [{max_fragment_length, 512}]).

%--------------------------------------------------------------------------------
%% check max_fragment_length interworking with openssl server
mfl_openssl_server(Config) when is_list(Config) ->
    mfl_openssl_server(512, Config),
    mfl_openssl_server(2048, Config).

%--------------------------------------------------------------------------------
%% check max_fragment_length interworking with openssl client
mfl_openssl_client(Config) when is_list(Config) ->
    mfl_openssl_client(1024, Config),
    mfl_openssl_client(4096, Config).

%--------------------------------------------------------------------------------
%% check max_fragment_length option on the client is accepted and reused
reuse_session(Config) when is_list(Config) ->
    MFL = 512,
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config) ++
        [{max_fragment_length, MFL}],
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),

    ssl_test_lib:reuse_session(ClientOpts, ServerOpts, Config).

%%--------------------------------------------------------------------

reuse_session_erlang_server(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = proplists:get_value(client_rsa_opts, Config),

    {_, ServerNode, _} = ssl_test_lib:run_where(Config),

    MFL = 512,
    Data = "reuse_session_erlang_server " ++ lists:duplicate(MFL, $r),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()},
                                        {mfa, {ssl_test_lib, active_recv, [length(Data)]}},
                                        {reconnect_times, 5},
                                        {options,  ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    {_Client, OpenSSLPort} = ssl_test_lib:start_client(openssl, [{port, Port}, 
                                                                 {reconnect, true},
                                                                 {maxfrag, MFL},
                                                                 {options, ClientOpts}, 
                                                                 return_port], Config),
    max_frag_len_test(Server, OpenSSLPort, MFL, Data),
    ssl_test_lib:close(Server).

%%--------------------------------------------------------------------

reuse_session_erlang_client(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = proplists:get_value(server_rsa_opts, Config),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),

    MFL = 512,
    Data = "reuse_session_erlang_client " ++ lists:duplicate(MFL, $r),
    ClientOpts = [{max_fragment_length, MFL} | ClientOpts0],

    {Server, OpenSSLPort} = ssl_test_lib:start_server(openssl, [{maxfrag, MFL}, return_port], 
                                                      [{server_opts, ServerOpts} | Config]),
    Port = ssl_test_lib:inet_port(Server),


    Client0 =
        ssl_test_lib:start_client([{node, ClientNode},
                                   {port, Port}, {host, Hostname},
                                   {mfa, {ssl_test_lib, session_id, []}},
                                   {from, self()},
                                   {options, [{reuse_sessions, save}, {verify, verify_peer}| ClientOpts]}]),

    SID = receive
              {Client0, Id0} ->
                  Id0
          end,

    %% quit s_server's current session so we can interact with the next client
    true = port_command(OpenSSLPort, "q\n"),
    ssl_test_lib:close(Client0),

    Client1 =
        ssl_test_lib:start_client([{node, ClientNode},
                                   {port, Port}, {host, Hostname},
                                   {mfa, {ssl_test_lib, session_id, []}},
                                   {from, self()},  {options, [{reuse_session, SID} | ClientOpts]}]),
    receive
        {Client1, SID} ->
            ok
    after ?SLEEP ->
            ct:fail(session_not_reused)
    end,

    ErlRecvFun = fun() ->
                         Data = ssl_test_lib:check_active_receive(Client1, Data)
                 end,
    max_frag_len_test(Client1, OpenSSLPort, MFL, Data, ErlRecvFun),
    ssl_test_lib:close(Client1).

%%--------------------------------------------------------------------

handshake_continue(Config) when is_list(Config) ->
    ok = run_mfl_handshake_continue(Config, 1024),
    ok = run_mfl_handshake_continue(Config, undefined),
    ok.

run_mfl_handshake_continue(Config, MFL) ->
    Data = if is_integer(MFL) ->
                   "hello world" ++ lists:duplicate(MFL, $u);
              true ->
                   "hello world" ++ lists:duplicate(999, $x)
           end,
    ClientExtraOpts = [{handshake, hello}, {max_fragment_length, MFL}],
    ServerExtraOpts = [{handshake, hello}],
    ExtraStartOpts = [{continue_options, [{want_ext, self()}]}],
    MflEnum = mfl_enum(MFL),
    PostF = fun(Server, Client) ->
                    receive {Server, {ext, ServerExt}} ->
                            ct:log("Server handshake Ext ~p~n", [ServerExt]),
                            MflEnum = maps:get(max_frag_enum, ServerExt, undefined)
                    end,
                    receive {Client, {ext, ClientExt}} ->
                            ct:log("Client handshake Ext ~p~n", [ClientExt]),
                            case maps:get(server_hello_selected_version, ClientExt, undefined) of
                                {3,4} ->
                                    %% For TLS 1.3 the ssl {handshake, hello} API is inconsistent:
                                    %% the server gets all the extensions CH+EE, but the client only CH
                                    ignore;
                                _ ->
                                    MflEnum = maps:get(max_frag_enum, ClientExt, undefined)
                            end
                    end,
                    ok
            end,

    run_mfl_handshake(Config, MFL, Data, ClientExtraOpts, ServerExtraOpts, ExtraStartOpts, ExtraStartOpts, PostF).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
run_mfl_handshake(Config, MFL) when is_integer(MFL) ->
    Data = "hello world" ++ lists:duplicate(MFL, $0),
    ClientExtraOpts = [{max_fragment_length, MFL}],
    run_mfl_handshake(Config, MFL, Data, ClientExtraOpts, []);
run_mfl_handshake(Config, MFL) ->
    Data = "hello world" ++ lists:duplicate(512, $9),
    ClientExtraOpts = [],
    run_mfl_handshake(Config, MFL, Data, ClientExtraOpts, []).

run_mfl_handshake(Config, MFL, Data, ClientExtraOpts, ServerExtraOpts) ->
    run_mfl_handshake(Config, MFL, Data, ClientExtraOpts, ServerExtraOpts, [], [], fun(_,_) -> ok end).

run_mfl_handshake(Config, MFL, Data, ClientExtraOpts, ServerExtraOpts, ClientExtraStartOpts, ServerExtraStartOpts,
                  PostFun) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ClientOpts = ClientExtraOpts ++ ClientOpts0,
    ServerOpts0 = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ServerOpts = ServerExtraOpts ++ ServerOpts0,

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                    {from, self()},
                    {mfa, {?MODULE, assert_mfl_and_send_first, [MFL, Data]}},
                    {options, ServerOpts} | ServerExtraStartOpts]),

    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
               {host, Hostname},
               {from, self()},
               {mfa, {?MODULE, assert_mfl_and_recv_first, [MFL, Data]}},
               {options, ClientOpts} | ClientExtraStartOpts]),

    ok = PostFun(Server, Client),

    ssl_test_lib:check_result(Server, ok, Client, ok).

assert_mfl(Socket, undefined) ->
    InfoMFL = ssl:connection_information(Socket, [max_fragment_length]),
    ct:log("Connection MFL ~p, Expecting: [] ~n", [InfoMFL]),
    {ok, []} = InfoMFL;
assert_mfl(Socket, MFL) ->
    InfoMFL = ssl:connection_information(Socket, [max_fragment_length]),
    ct:log("Connection MFL ~p, Expecting: ~p ~n", [InfoMFL, MFL]),
    {ok, [{max_fragment_length, ConnMFL}]} = InfoMFL,
    ConnMFL = MFL.

assert_mfl_and_send_first(Socket, MFL, Data) ->
    assert_mfl(Socket, MFL),
    ssl_send(Socket, Data),
    ssl_receive(Socket, "Got it"++lists:reverse(Data)).

assert_mfl_and_recv_first(Socket, MFL, Data) ->
    assert_mfl(Socket, MFL),
    ssl_receive(Socket, Data),
    ssl_send(Socket, lists:reverse(Data)).

ssl_send(Socket, Data) ->
    ssl:send(Socket, Data).

ssl_receive(Socket, Data) ->
    ssl_receive(Socket, Data, []).

ssl_receive(Socket, Data, Buffer) ->
    receive
    {ssl, Socket, MoreData} ->
        ct:log("Received ~p~n",[MoreData]),
        NewBuffer = Buffer ++ MoreData,
        case NewBuffer of
            Data ->
                ssl:send(Socket, "Got it"),
                ok;
            _ ->
                ssl_receive(Socket, Data, NewBuffer)
        end;
    Other ->
        ct:fail({unexpected_message, Other})
    after 4000 ->
        ct:fail({did_not_get, Data})
    end.

%% ------------------------------------------------------------
mfl_openssl_client(MFL, Config) ->  
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = proplists:get_value(client_rsa_opts, Config),
    {_, ServerNode, _} = ssl_test_lib:run_where(Config),

    Data = "mfl_openssl_server " ++ lists:duplicate(MFL, $s),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()},
                                        {mfa, {ssl_test_lib, active_recv, [length(Data)]}},
                                        {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    
    {_Client, OpenSSLPort} = ssl_test_lib:start_client(openssl, [{port, Port}, 
                                                                 {maxfrag, MFL},
                                                                 {options, ClientOpts}, 
                                                                 return_port], Config),

    max_frag_len_test(Server, OpenSSLPort, MFL, Data).

%% ------------------------------------------------------------
mfl_openssl_server(MFL, Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = proplists:get_value(server_rsa_opts, Config),
    {ClientNode, _, Hostname} = ssl_test_lib:run_where(Config),
    Data = "mfl_openssl_server " ++ lists:duplicate(MFL, $s),

    {Server, OpenSSLPort} = ssl_test_lib:start_server(openssl, [{maxfrag, MFL},
                                                                return_port], 
                                                      [{server_opts, ServerOpts} | Config]),
    Port = ssl_test_lib:inet_port(Server),
    
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
                                        {host, Hostname},
                                        {from, self()},
                                        {mfa, {ssl_test_lib,
                                               active_recv, [length(Data)]}},
                                        {options, [{max_fragment_length, MFL} | ClientOpts]}]),
    
    max_frag_len_test(Client, OpenSSLPort, MFL, Data).

%% ------------------------------------------------------------
max_frag_len_test(ErlProc, OpenSSL, MFL, Data) ->
    ErlRecvFun = fun() ->
                         receive
                             {ErlProc, Data} ->
                                 ok
                         end
                 end,
    max_frag_len_test(ErlProc, OpenSSL, MFL, Data, ErlRecvFun).

max_frag_len_test(ErlProc, OpenSSL, MFL, Data, ErlRecvFun) ->
    true = port_command(OpenSSL, Data),
    ErlRecvFun(),

    ErlProc ! get_socket,
    ErlSocket = receive
                    {ErlProc, {socket, ErlSocket0}} ->
                        ErlSocket0
                end,
    assert_mfl(ErlSocket, MFL).


%% RFC 6066
mfl_enum(512) -> 1;
mfl_enum(1024) -> 2;
mfl_enum(2048) -> 3;
mfl_enum(4096) -> 4;
mfl_enum(undefined) -> undefined.
