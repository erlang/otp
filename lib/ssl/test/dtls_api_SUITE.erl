%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2022. All Rights Reserved.
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

%%
-module(dtls_api_SUITE).

%% Callback functions
-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Testcases
-export([
         replay_window/0, replay_window/1,
         dtls_listen_owner_dies/0,
         dtls_listen_owner_dies/1,
         dtls_listen_close/0,
         dtls_listen_close/1,
         dtls_listen_reopen/0,
         dtls_listen_reopen/1,
         dtls_listen_two_sockets_1/0,
         dtls_listen_two_sockets_1/1,
         dtls_listen_two_sockets_2/0,
         dtls_listen_two_sockets_2/1,
         dtls_listen_two_sockets_3/0,
         dtls_listen_two_sockets_3/1,
         dtls_listen_two_sockets_4/0,
         dtls_listen_two_sockets_4/1,
         dtls_listen_two_sockets_5/0,
         dtls_listen_two_sockets_5/1,
         dtls_listen_two_sockets_6/0,
         dtls_listen_two_sockets_6/1,
         client_restarts/0, client_restarts/1
        ]).

-include_lib("ssl/src/ssl_internal.hrl").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() ->
    [
     replay_window,
     {group, 'dtlsv1.2'},
     {group, 'dtlsv1'}
    ].

groups() ->
    [
     {'dtlsv1.2', [],  api_tests()},
     {'dtlsv1', [],  api_tests()}
    ].

api_tests() ->
    [
     dtls_listen_owner_dies,
     dtls_listen_close,
     dtls_listen_reopen,
     dtls_listen_two_sockets_1,
     dtls_listen_two_sockets_2,
     dtls_listen_two_sockets_3,
     dtls_listen_two_sockets_4,
     dtls_listen_two_sockets_5,
     dtls_listen_two_sockets_6,
     client_restarts
    ].

init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
	    ssl_test_lib:clean_start(),
	    ssl_test_lib:make_rsa_cert(Config0)
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:unload(ssl),
    application:stop(crypto).


init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group(GroupName, Config).

end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(Testcase, Config)
  when Testcase =:= dtls_listen_two_sockets_1 orelse
       Testcase =:= dtls_listen_two_sockets_5 orelse
       Testcase =:= dtls_listen_two_sockets_6 ->
    case ssl:listen(0, [{protocol, dtls}, {ip, {127,0,0,2}}]) of
        {ok, S} ->
            test_listen_on_all_interfaces(S, Config),
            ssl:close(S),
            ssl_test_lib:ct_log_supported_protocol_versions(Config),
            ct:timetrap({seconds, 10}),
            maybe_skip_tc_on_windows(Testcase, Config);
        {error, _} ->
            {skip, "127.0.0.x address not available"}
    end;
init_per_testcase(_TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 10}),
    Config.

end_per_testcase(_TestCase, Config) ->     
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

dtls_listen_owner_dies() ->
    [{doc, "Test that you can start new DTLS 'listner' if old owner dies"}].

dtls_listen_owner_dies(Config) when is_list(Config) ->    
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Port = ssl_test_lib:inet_port(ServerNode),
    Test = self(),
    Pid = spawn(fun() -> {ok, _} =
                             ssl:listen(Port, [{protocol, dtls} | ServerOpts]),
                         {error, _} = ssl:listen(Port, [{protocol, dtls} | ServerOpts]),
                         Test ! {self(), listened}
                end),
    receive
        {Pid, listened} ->
            ok
    end,
    {ok, LSocket} = ssl:listen(Port, [{protocol, dtls} | ServerOpts]),
    spawn(fun() -> 
                  {ok, ASocket} = ssl:transport_accept(LSocket),
                  {ok, Socket} = ssl:handshake(ASocket),
                   receive 
                       {ssl, Socket, "from client"} ->
                           ssl:send(Socket, "from server"),
                           ssl:close(Socket)
                   end
          end),
    {ok, Client} = ssl:connect(Hostname, Port, ClientOpts),

    ssl:send(Client, "from client"),
    receive 
        {ssl, Client, "from server"} ->
            ssl:close(Client)
    end.


dtls_listen_close() ->
    [{doc, "Test that you close a DTLS 'listner' socket"}].

dtls_listen_close(Config) when is_list(Config) ->    
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {_, ServerNode, _Hostname} = ssl_test_lib:run_where(Config),

    Port = ssl_test_lib:inet_port(ServerNode),
    {ok, ListenSocket} = ssl:listen(Port, [{protocol, dtls} | ServerOpts]),
    ok = ssl:close(ListenSocket).


dtls_listen_reopen() ->
    [{doc, "Test that you close a DTLS 'listner' socket and open a new one for the same port"}].

dtls_listen_reopen(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Port = ssl_test_lib:inet_port(ServerNode),
    {ok, LSocket0} = ssl:listen(Port, [{protocol, dtls} | ServerOpts]),
     spawn(fun() ->
                  {ok, ASocket} = ssl:transport_accept(LSocket0),
                   {ok, Socket} = ssl:handshake(ASocket),
                   receive
                       {ssl, Socket, "from client"} ->
                           ssl:send(Socket, "from server 1"),
                           ssl:close(Socket)
                   end
           end),
    {ok, Client1} = ssl:connect(Hostname, Port, ClientOpts),
    ok = ssl:close(LSocket0),
    {ok, LSocket1} = ssl:listen(Port, [{protocol, dtls} | ServerOpts]),
    spawn(fun() ->
                  {ok, ASocket} = ssl:transport_accept(LSocket1),
                  {ok, Socket} = ssl:handshake(ASocket),
                  receive
                      {ssl, Socket, "from client"} ->
                          ssl:send(Socket, "from server 2"),
                          ssl:close(Socket)
                   end
          end),
    {ok, Client2} = ssl:connect(Hostname, Port, [{protocol, dtls} | ClientOpts]),
    ssl:send(Client2, "from client"),
    ssl:send(Client1, "from client"),
    receive
        {ssl, Client1, "from server 1"} ->
            ssl:close(Client1)
    end,
    receive
        {ssl, Client2, "from server 2"} ->
            ssl:close(Client2)
    end.

dtls_listen_two_sockets_1() ->
    [{doc, "Test with two DTLS dockets: 127.0.0.2:Port, 127.0.0.3:Port"}].
dtls_listen_two_sockets_1(_Config) when is_list(_Config) ->
    {ok, S1} = ssl:listen(0, [{protocol, dtls}, {ip, {127,0,0,2}}]),
    {ok, {_, Port}} = ssl:sockname(S1),
    {ok, S2} = ssl:listen(Port, [{protocol, dtls}, {ip, {127,0,0,3}}]),
    ssl:close(S1),
    ssl:close(S2),
    ok.

dtls_listen_two_sockets_2() ->
    [{doc, "Test with two DTLS dockets: <all_interfaces>:Port, <all_interfaces>:Port"}].
dtls_listen_two_sockets_2(_Config) when is_list(_Config) ->
    {ok, S1} = ssl:listen(0, [{protocol, dtls}]),
    {ok, {_, Port}} = ssl:sockname(S1),
    {error, already_listening} =
        ssl:listen(Port, [{protocol, dtls}]),
    ssl:close(S1),
    ok.

dtls_listen_two_sockets_3() ->
    [{doc, "Test with two DTLS dockets: <all_interfaces>:Port, <all_interfaces>:Port"}].
dtls_listen_two_sockets_3(_Config) when is_list(_Config) ->
    {ok, S1} = ssl:listen(0, [{protocol, dtls}]),
    {ok, {_, Port}} = ssl:sockname(S1),
    {error, already_listening} =
        ssl:listen(Port, [{protocol, dtls}]),
    ssl:close(S1),
    {ok, S2} = ssl:listen(Port, [{protocol, dtls}]),
    ssl:close(S2),
    ok.

dtls_listen_two_sockets_4() ->
  [{doc, "Test with two DTLS dockets: process1 - <all_interfaces>:Port, process2 - <all_interfaces>:Port"}].
dtls_listen_two_sockets_4(_Config) when is_list(_Config) ->
    Test = self(),
    Pid = spawn(fun() ->
                  {ok, S1} = ssl:listen(0, [{protocol, dtls}]),
                  {ok, {_, Port0}} = ssl:sockname(S1),
                  Test ! {self(), Port0}
                end),
    Port =
        receive
            {Pid, Port1} ->
                Port1
        end,
    {ok, S2} =
        ssl:listen(Port, [{protocol, dtls}]),
    ssl:close(S2),
    ok.

dtls_listen_two_sockets_5() ->
    [{doc, "Test with two DTLS dockets: <all_interfaces>:Port, 127.0.0.3:Port"}].
dtls_listen_two_sockets_5(_Config) when is_list(_Config) ->
    {ok, S1} = ssl:listen(0, [{protocol, dtls}]),
    {ok, {_, Port}} = ssl:sockname(S1),
    {error, already_listening} =
        ssl:listen(Port, [{protocol, dtls}, {ip, {127,0,0,3}}]),
    ssl:close(S1),
    {ok, S2} =
        ssl:listen(Port, [{protocol, dtls}, {ip, {127,0,0,3}}]),
    {error, already_listening} =
        ssl:listen(Port, [{protocol, dtls}]),
    ssl:close(S2),
    ok.

dtls_listen_two_sockets_6() ->
    [{doc, "Test with two DTLS dockets: 127.0.0.3:Port, 0.0.0.0:Port"}].
dtls_listen_two_sockets_6(_Config) when is_list(_Config) ->
    {ok, S1} = ssl:listen(0, [{protocol, dtls}, {ip, {127,0,0,3}}]),
    {ok, {_, Port}} = ssl:sockname(S1),
    {error, already_listening} =
        ssl:listen(Port, [{protocol, dtls}, {ip, {0,0,0,0}}]),
    ssl:close(S1),
    ok.

replay_window() ->
    [{doc, "Whitebox test of replay window"}].
replay_window(_Config) ->
    W0 = dtls_record:init_replay_window(),
    Size = 58,
    true = replay_window(0, 0, Size-1, [], W0),
    ok.

replay_window(N, Top, Sz, Used, W0) when N < 99000 ->
    Bottom = max(0, Top - Sz),
    Seq = max(0, Bottom + rand:uniform(Top-Bottom+10)-5),
    IsReplay = (Seq < Bottom) orelse lists:member(Seq, Used),
    case dtls_record:is_replay(Seq,W0) of
        true when IsReplay ->
            replay_window(N+1, Top, Sz, Used, W0);
        false when (not IsReplay) ->
            #{replay_window:=W1} = dtls_record:update_replay_window(Seq, #{replay_window=>W0}),
            NewTop = if Seq > Top -> Seq;
                        true -> Top
                     end,
            NewBottom = max(0, (NewTop - Sz)),
            NewUsed = lists:dropwhile(fun(S) -> S < NewBottom end,
                                      lists:sort([Seq|Used])),
            replay_window(N+1, NewTop, Sz, NewUsed, W1);
        Replay ->
            io:format("Try: ~p Top: ~w Sz: ~p Used:~p State: ~w~n", [N, Top, Sz, length(Used), W0]),
            io:format("Seq: ~w Replay: ~p (~p)~n ~w~n ~w~n",
                      [Seq, Replay, IsReplay, Used, bits_to_list(W0)]),
            {fail, Replay, Seq, W0}
    end;
replay_window(N, Top, Sz, Used, W0) ->
    io:format("Try: ~p Top: ~w Sz: ~p Used:~p State: ~w~n", [N, Top, Sz, length(Used), W0]),
    io:format("Match ~w ~n", [bits_to_list(W0) =:= Used]),
    bits_to_list(W0) =:= Used.

bits_to_list(#{mask := Bits, bottom:= Bottom}) ->
    bits_to_list(Bits, Bottom, []).

bits_to_list(0, _, Is) ->
    lists:reverse(Is);
bits_to_list(Bits, I, Acc) ->
    case Bits band 1 of
        1 -> bits_to_list(Bits bsr 1, I+1, [I|Acc]);
        0 -> bits_to_list(Bits bsr 1, I+1, Acc)
    end.

client_restarts() ->
    [{doc, "Test re-connection "}].

client_restarts(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
                                   {mfa, {ssl_test_lib, no_result, []}},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client0 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib, no_result, []}},
                                         {from, self()},
                                         {options, [{reuse_sessions, save} | ClientOpts]}]),
    ReConnect =  %% Whitebox re-connect test
        fun({sslsocket, {gen_udp,_,dtls_gen_connection}, [Pid]} = Socket, ssl) ->
                ct:log("~p Client Socket: ~p ~n", [self(), Socket]),
                {ok, {{Address,CPort},UDPSocket}=IntSocket} = gen_statem:call(Pid, {downgrade, self()}),

                ct:log("Info: ~p~n", [inet:info(UDPSocket)]),

                {ok, #config{transport_info = CbInfo, connection_cb = ConnectionCb,
                             ssl = SslOpts0}} = ssl:handle_options(ClientOpts, client, Address),
                SslOpts = {SslOpts0, #socket_options{}, undefined},

                ct:sleep(250),
                ct:log("Client second connect: ~p ~p~n", [Socket, CbInfo]),
                Res = ssl_gen_statem:connect(ConnectionCb, Address, CPort, IntSocket, SslOpts, self(), CbInfo, infinity),
                {Res, Pid}
        end,

    Client0 ! {apply, self(), ReConnect},
    receive
        {apply_res, {Res, _Prev}} ->
            ct:log("Apply res: ~p~n", [Res]),
            ok;
        Msg ->
            ct:log("Unhandled: ~p~n", [Msg]),
            ct:fail({wrong_msg, Msg})
    end,

    receive
        Msg2 ->
            ct:log("Unhandled: ~p~n", [Msg2]),
            ct:fail({wrong_msg, Msg2})
    after 200 ->
            ct:log("Nothing received~n", [])
    end,

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client0),

    ok.

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------

%% Helper function for init_per_testcase.
test_listen_on_all_interfaces(S0, Config) ->
    {ok, {_, Port}} = ssl:sockname(S0),
    case ssl:listen(Port, [{protocol, dtls}, {ip, {0,0,0,0}}]) of
        {ok, S1} ->
            ssl:close(S0),
            ssl:close(S1),
            {skip, "Testcase is not supported on this OS."};
        {error, _} ->
            Config
    end.

maybe_skip_tc_on_windows(Testcase, Config)
  when Testcase =:= dtls_listen_two_sockets_5 orelse
       Testcase =:= dtls_listen_two_sockets_6 ->
    case os:type() of
        {win32, _} ->
            {skip, "Testcase not supported in Windows"};
        _ ->
            Config
    end;
maybe_skip_tc_on_windows(_, Config) ->
    Config.
