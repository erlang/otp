%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2019-2025. All Rights Reserved.
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

-module(ssl_socket_SUITE).

-behaviour(ct_suite).

-include("ssl_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

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
-export([getstat/0,
         getstat/1,
         socket_options/0,
         socket_options/1,
         invalid_inet_get_option/0,
         invalid_inet_get_option/1,
         invalid_inet_get_option_not_list/0,
         invalid_inet_get_option_not_list/1,
         invalid_inet_get_option_improper_list/0,
         invalid_inet_get_option_improper_list/1,
         invalid_inet_set_option/0,
         invalid_inet_set_option/1,
         invalid_inet_set_option_not_list/0,
         invalid_inet_set_option_not_list/1,
         invalid_inet_set_option_improper_list/0,
         invalid_inet_set_option_improper_list/1,
         raw_inet_option/0,
         raw_inet_option/1,
         packet_option_round_trip/0,
         packet_option_round_trip/1,
         packet_option_initialization/0,
         packet_option_initialization/1,
         watermark_options_round_trip/0,
         watermark_options_round_trip/1,
         watermark_options_initialization/0,
         watermark_options_initialization/1,
         watermark_options_upgrade/0,
         watermark_options_upgrade/1,
         send_after_close_returns_closed/0,
         send_after_close_returns_closed/1
        ]).

%% Apply export
-export([socket_options_result/5,
         get_invalid_inet_option/1,
         get_invalid_inet_option_not_list/1,
         get_invalid_inet_option_improper_list/1,
         set_invalid_inet_option/1,
         set_invalid_inet_option_not_list/1,
         set_invalid_inet_option_improper_list/1
        ]).

-define(TIMEOUT, {seconds, 5}).
-define(SLEEP, 500).
%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() -> 
    [
     {group, tls},
     {group, dtls},
     {group, transport_socket}
    ].

groups() ->
    [
     {tls,[], socket_tests() ++ raw_inet_opt() ++ shared_opts_tests()},
     {dtls,[], socket_tests()},
     {transport_socket, [], socket_tests() ++ raw_inet_opt() ++ shared_opts_tests()
      ++ emulated_watermark_tests()}
    ].

socket_tests() ->
    [
     getstat,
     socket_options,
     invalid_inet_get_option,
     invalid_inet_get_option_not_list,
     invalid_inet_get_option_improper_list,
     invalid_inet_set_option,
     invalid_inet_set_option_not_list,
     invalid_inet_set_option_improper_list
    ].

raw_inet_opt() ->
    [
     raw_inet_option
    ].

%% Options that ssl:send/2 and the sender process read without asking the
%% connection process.
shared_opts_tests() ->
    [
     packet_option_round_trip,
     packet_option_initialization,
     send_after_close_returns_closed
    ].

%% The watermarks are emulated only on the tls_socket_tcp transport.
emulated_watermark_tests() ->
    [
     watermark_options_round_trip,
     watermark_options_initialization,
     watermark_options_upgrade
    ].


init_per_suite(Config0) ->
    catch application:stop(crypto),
    try application:start(crypto) of
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

init_per_group(dtls, Config) ->
    [{group_opts, [{protocol, dtls}]} | proplists:delete(group_opts, Config)];
init_per_group(tls, Config) ->
    [{group_opts, [{protocol, tls}]} | proplists:delete(group_opts, Config)];
init_per_group(transport_socket, Config) ->
    [{group_opts, [{protocol, tls}, {cb_info, tls_socket_tcp:cb_info()}]}| Config].

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(raw_inet_option, Config) ->
    ct:timetrap(?TIMEOUT),
    case os:type() of
        {unix,linux} ->
            Config;
        _ ->
            {skip, "Raw options are platform-specific"}
    end;
init_per_testcase(_TestCase, Config) ->
    ct:timetrap(?TIMEOUT),
    Config.

end_per_testcase(_TestCase, Config) ->     
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

getstat() ->
    [{doc,"Test API function getstat/2"}].

getstat(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server1 =
        ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                   {from, self()},
                                   {mfa, {ssl_test_lib, send_recv_result, []}},
                                   {options,  [{active, false} | ServerOpts]}]),
    Port1 = ssl_test_lib:inet_port(Server1),
    Server2 =
        ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                   {from, self()},
                                   {mfa, {ssl_test_lib, send_recv_result, []}},
                                   {options,  [{active, false} | ServerOpts]}]),
    Port2 = ssl_test_lib:inet_port(Server2),
    {ok, ActiveC} = rpc:call(ClientNode, ssl, connect,
                          [Hostname,Port1,[{active, once}|ClientOpts]]),
    {ok, PassiveC} = rpc:call(ClientNode, ssl, connect,
                          [Hostname,Port2,[{active, false}|ClientOpts]]),

    ?CT_LOG("Servers ~p, ~p ~n", [Server1, Server2]),

    %% We only check that the values are non-zero initially
    %% (due to the handshake), and that sending more changes the values.

    %% Passive socket.

    {ok, InitialStats} = ssl:getstat(PassiveC),
    ?CT_LOG("InitialStats  ~p~n", [InitialStats]),
    [true] = lists:usort([0 =/= proplists:get_value(Name, InitialStats)
        || Name <- [recv_cnt, recv_oct, recv_avg, recv_max, send_cnt, send_oct, send_avg, send_max]]),

    ok = ssl:send(PassiveC, "Hello world"),
    wait_for_send(PassiveC),
    {ok, SStats} = ssl:getstat(PassiveC, [send_cnt, send_oct]),
    ?CT_LOG("SStats  ~p~n", [SStats]),
    [true] = lists:usort([proplists:get_value(Name, SStats) =/= proplists:get_value(Name, InitialStats)
        || Name <- [send_cnt, send_oct]]),

    %% Active socket.

    {ok, InitialAStats} = ssl:getstat(ActiveC),
    ?CT_LOG("InitialAStats  ~p~n", [InitialAStats]),
    [true] = lists:usort([0 =/= proplists:get_value(Name, InitialAStats)
        || Name <- [recv_cnt, recv_oct, recv_avg, recv_max, send_cnt, send_oct, send_avg, send_max]]),

    _ = receive
        {ssl, ActiveC, _} ->
            ok
    after
        ?SLEEP ->
            exit(timeout)
    end,

    ok = ssl:send(ActiveC, "Hello world"),
    wait_for_send(ActiveC),
    {ok, ASStats} = ssl:getstat(ActiveC, [send_cnt, send_oct]),
    ?CT_LOG("ASStats  ~p~n", [ASStats]),
    [true] = lists:usort([proplists:get_value(Name, ASStats) =/= proplists:get_value(Name, InitialAStats)
        || Name <- [send_cnt, send_oct]]),

    ok.
%%--------------------------------------------------------------------
socket_options() ->
    [{doc,"Test API function getopts/2 and setopts/2"}].

socket_options(Config) when is_list(Config) -> 
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Values = [{mode, list}, {active, true}],    
    %% Shall be the reverse order of Values! 
    Options = [active, mode],
    
    NewValues = [{mode, binary}, {active, once}],
    %% Shall be the reverse order of NewValues! 
    NewOptions = [active, mode],
    
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
			   {mfa, {?MODULE, socket_options_result, 
				  [Options, Values, NewOptions, NewValues]}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
			   {from, self()}, 
			   {mfa, {?MODULE, socket_options_result, 
				  [Options, Values, NewOptions, NewValues]}},
			   {options, ClientOpts}]),
    
    ssl_test_lib:check_result(Server, ok, Client, ok),

    ssl_test_lib:close(Server),
    
    {ok, Listen} = ssl:listen(0, ServerOpts),
    {ok,[{mode,list}]} = ssl:getopts(Listen, [mode]),
    ok = ssl:setopts(Listen, [{mode, binary}]),
    {ok,[{mode, binary}]} = ssl:getopts(Listen, [mode]),
    {ok,[{recbuf, _}]} = ssl:getopts(Listen, [recbuf]),
    ssl:close(Listen).

%%--------------------------------------------------------------------
raw_inet_option() ->
    [{doc,"Ensure that a single 'raw' option is passed to ssl:listen correctly."}].

raw_inet_option(Config) when is_list(Config) ->
    % 'raw' option values are platform-specific; these are the Linux values:
    IpProtoTcp = 6,
    % Use TCP_KEEPIDLE, because (e.g.) TCP_MAXSEG can't be read back reliably.
    TcpKeepIdle = 4,
    KeepAliveTimeSecs = 55,
    LOptions = [{raw, IpProtoTcp, TcpKeepIdle, <<KeepAliveTimeSecs:32/native>>}],
    {ok, LSocket} = ssl:listen(0, LOptions),
    % Per http://www.erlang.org/doc/man/inet.html#getopts-2, we have to specify
    % exactly which raw option we want, and the size of the buffer.
    {ok, [{raw, IpProtoTcp, TcpKeepIdle, <<KeepAliveTimeSecs:32/native>>}]} = 
        ssl:getopts(LSocket, [{raw, IpProtoTcp, TcpKeepIdle, 4}]).

%%--------------------------------------------------------------------

invalid_inet_get_option() ->
    [{doc,"Test handling of invalid inet options in getopts"}].

invalid_inet_get_option(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, get_invalid_inet_option, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, no_result, []}},
			   {options, ClientOpts}]),

    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
invalid_inet_get_option_not_list() ->
    [{doc,"Test handling of invalid type in getopts"}].

invalid_inet_get_option_not_list(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, get_invalid_inet_option_not_list, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, no_result, []}},
			   {options, ClientOpts}]),

    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),
    
    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
invalid_inet_get_option_improper_list() ->
    [{doc,"Test handling of invalid type in getopts"}].

invalid_inet_get_option_improper_list(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, get_invalid_inet_option_improper_list, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, no_result, []}},
			   {options, ClientOpts}]),

    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
invalid_inet_set_option() ->
    [{doc,"Test handling of invalid inet options in setopts"}].

invalid_inet_set_option(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, set_invalid_inet_option, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, no_result, []}},
			   {options, ClientOpts}]),

    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
invalid_inet_set_option_not_list() ->
    [{doc,"Test handling of invalid type in setopts"}].

invalid_inet_set_option_not_list(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, set_invalid_inet_option_not_list, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, no_result, []}},
			   {options, ClientOpts}]),

    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
invalid_inet_set_option_improper_list() ->
    [{doc,"Test handling of invalid type in setopts"}].

invalid_inet_set_option_improper_list(Config) when is_list(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
			   {mfa, {?MODULE, set_invalid_inet_option_improper_list, []}},
			   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
			   {from, self()},
			   {mfa, {ssl_test_lib, no_result, []}},
			   {options, ClientOpts}]),

    ?CT_LOG("Client ~p  Server ~p ~n", [Client, Server]),

    ssl_test_lib:check_result(Server, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


%%--------------------------------------------------------------------
%% Callback functions ------------------------------------------------
%%--------------------------------------------------------------------
socket_options_result(Socket, Options, DefaultValues, NewOptions, NewValues) ->
    %% Test get/set emulated opts
    {ok, DefaultValues} = ssl:getopts(Socket, Options), 
    ssl:setopts(Socket, NewValues),
    {ok, NewValues} = ssl:getopts(Socket, NewOptions),
    %% Test get/set inet opts
    {ok,[{reuseaddr, _}]} = ssl:getopts(Socket, [reuseaddr]),  
    {ok, All} = ssl:getopts(Socket, []),
    ?CT_LOG("All opts ~p~n", [All]),
    ok.

get_invalid_inet_option(Socket) ->
    {error, {options, {socket_options, foo, _}}} = ssl:getopts(Socket, [foo]),
    ok.

get_invalid_inet_option_not_list(Socket) ->
    {error, {options, {socket_options, some_invalid_atom_here}}}
        = ssl:getopts(Socket, some_invalid_atom_here),
     ok.

get_invalid_inet_option_improper_list(Socket) ->
    {error, {options, {socket_options, foo,_}}} = ssl:getopts(Socket, [packet | foo]),
    ok.

set_invalid_inet_option(Socket) ->
    {error, {options, {socket_options, {packet, foo}}}} = ssl:setopts(Socket, [{packet, foo}]),
    {error, {options, {socket_options, {header, foo}}}} = ssl:setopts(Socket, [{header, foo}]),
    {error, {options, {socket_options, {active, foo}}}} = ssl:setopts(Socket, [{active, foo}]),
    {error, {options, {socket_options, {mode, foo}}}}   = ssl:setopts(Socket, [{mode, foo}]),
    ok.

set_invalid_inet_option_not_list(Socket) ->
    {error, {options, {not_a_proplist, some_invalid_atom_here}}}
	= ssl:setopts(Socket, some_invalid_atom_here),
    ok.

set_invalid_inet_option_improper_list(Socket) ->
    {error, {options, {not_a_proplist, [{packet, 0} | {foo, 2}]}}} =
	ssl:setopts(Socket, [{packet, 0} | {foo, 2}]),
    ok.

wait_for_send(Socket) ->
    %% Make sure TLS process processed send message event
    _ = ssl:connection_information(Socket).

%%--------------------------------------------------------------------
packet_option_round_trip() ->
    [{doc,"setopts/getopts of every packet type on an established socket, "
      "and ssl:send/2 encodes with the type set last"}].

packet_option_round_trip(Config) when is_list(Config) ->
    {Server, Client} = connected_pair(Config),
    Types = [raw, 0, 1, 2, 4, asn1, cdr, sunrm, fcgi, tpkt, line,
             http, httph, http_bin, httph_bin],
    lists:foreach(
      fun(Type) ->
              ok = ssl:setopts(Server, [{packet, Type}]),
              {ok, [{packet, Type}]} = ssl:getopts(Server, [packet])
      end, Types),
    %% Only length-prefixed packet modes add a header when sending.
    lists:foreach(
      fun({Type, Expected}) ->
              ok = ssl:setopts(Server, [{packet, Type}]),
              ok = ssl:send(Server, <<"hello">>),
              {ok, Expected} = ssl:recv(Client, byte_size(Expected), 5000)
      end, [{1, <<5, "hello">>}, {2, <<0, 5, "hello">>},
            {4, <<0, 0, 0, 5, "hello">>},
            {raw, <<"hello">>}, {line, <<"hello">>}]),
    ok = ssl:close(Server),
    ok = ssl:close(Client).

%%--------------------------------------------------------------------
packet_option_initialization() ->
    [{doc,"Packet values accepted at initialization must not crash the shared "
      "options store; sending uses no header unless the value is 1, 2 or 4"}].

packet_option_initialization(Config) when is_list(Config) ->
    lists:foreach(
      fun({Version, Packet}) ->
              {Server, Client} = connected_pair(
                                   Config, [{versions, [Version]}, {packet, Packet}]),
              {ok, [{packet, Packet}]} = ssl:getopts(Server, [packet]),
              {ok, [{packet, Packet}]} = ssl:getopts(Client, [packet]),
              %% Decode raw data at the receiving end. The sender keeps the
              %% original packet value, which must map to no send header.
              ok = ssl:setopts(Client, [{packet, raw}]),
              ok = ssl:send(Server, <<"hello">>),
              {ok, <<"hello">>} = ssl:recv(Client, 5, 5000),
              ok = ssl:close(Server),
              ok = ssl:close(Client)
      end, [{Version, Packet} || Version <- ['tlsv1.2', 'tlsv1.3'],
                                Packet <- [3, future_packet_type, 1 bsl 64]]).

%%--------------------------------------------------------------------
watermark_options_round_trip() ->
    [{doc,"high_watermark and low_watermark on the tls_socket_tcp transport: "
      "defaults, signed values, and rejection outside the atomics range"}].

watermark_options_round_trip(Config) when is_list(Config) ->
    {Server, Client} = connected_pair(Config),
    %% tls_socket:default_inet_values(tls_socket_tcp) seeds both at init.
    {ok, [{high_watermark, 8192}]} = ssl:getopts(Server, [high_watermark]),
    {ok, [{low_watermark, 4096}]} = ssl:getopts(Server, [low_watermark]),
    Min = -(1 bsl 63),
    Max = (1 bsl 63) - 1,
    lists:foreach(
      fun(Name) ->
              lists:foreach(
                fun(Value) ->
                        Opt = {Name, Value},
                        ok = ssl:setopts(Server, [Opt]),
                        {ok, [Opt]} = ssl:getopts(Server, [Name])
                end, [Min, -1, 0, 100000, Max]),
              lists:foreach(
                fun(Value) ->
                        Opt = {Name, Value},
                        {error, {options, {socket_options, Opt}}} =
                            ssl:setopts(Server, [Opt]),
                        {ok, [{Name, Max}]} = ssl:getopts(Server, [Name])
                end, [Min - 1, Max + 1, (1 bsl 64) - 1, 1 bsl 64,
                      1.5, bad_watermark])
      end, [high_watermark, low_watermark]),
    %% Rejected options must leave the connection usable, including the sender.
    ok = ssl:setopts(Server, [{high_watermark, 100000}, {low_watermark, 0}]),
    ok = ssl:send(Server, <<"hello">>),
    {ok, <<"hello">>} = ssl:recv(Client, 5, 5000),
    ok = ssl:close(Server),
    ok = ssl:close(Client).

%%--------------------------------------------------------------------
watermark_options_initialization() ->
    [{doc,"Initialization accepts signed watermarks and rejects values "
      "outside the atomics range before starting a connection process"}].

watermark_options_initialization(Config) when is_list(Config) ->
    Min = -(1 bsl 63),
    Max = (1 bsl 63) - 1,
    {Server, Client} = connected_pair(
                         Config, [{high_watermark, Max}, {low_watermark, Min}]),
    {ok, [{high_watermark, Max}]} = ssl:getopts(Server, [high_watermark]),
    {ok, [{low_watermark, Min}]} = ssl:getopts(Server, [low_watermark]),
    {ok, [{high_watermark, Max}]} = ssl:getopts(Client, [high_watermark]),
    {ok, [{low_watermark, Min}]} = ssl:getopts(Client, [low_watermark]),
    ok = ssl:close(Server),
    ok = ssl:close(Client),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    GroupOpts = proplists:get_value(group_opts, Config, []),
    lists:foreach(
      fun({Name, Value} = Opt) ->
              Expected = {error, {options, {Name, Value}}},
              Expected = ssl:listen(0, [Opt | GroupOpts ++ ServerOpts]),
              Expected = ssl:connect("localhost", 0,
                                     [Opt | GroupOpts ++ ClientOpts], 1000)
      end, [{Name, Value} || Name <- [high_watermark, low_watermark],
                            Value <- [Min - 1, Max + 1, (1 bsl 64) - 1,
                                      1 bsl 64, 1.5, bad_watermark]]).

%%--------------------------------------------------------------------
watermark_options_upgrade() ->
    [{doc,"Raw-socket upgrades inherit transport watermark defaults "
      "and preserve explicitly supplied watermarks"}].

watermark_options_upgrade(Config) when is_list(Config) ->
    lists:foreach(
      fun({Opts, High, Low}) ->
              {Server, Client} = upgraded_pair(Config, Opts),
              lists:foreach(
                fun(Socket) ->
                        {ok, [{high_watermark, High}]} =
                            ssl:getopts(Socket, [high_watermark]),
                        {ok, [{low_watermark, Low}]} =
                            ssl:getopts(Socket, [low_watermark])
                end, [Server, Client]),
              ok = ssl:send(Server, <<"server">>),
              {ok, <<"server">>} = ssl:recv(Client, 6, 5000),
              ok = ssl:send(Client, <<"client">>),
              {ok, <<"client">>} = ssl:recv(Server, 6, 5000),
              ok = ssl:close(Server),
              ok = ssl:close(Client)
      end, [{[], 8192, 4096},
            {[{high_watermark, -1}], -1, 4096},
            {[{low_watermark, -1}], 8192, -1},
            {[{high_watermark, 100000}, {low_watermark, 0}], 100000, 0}]).

%%--------------------------------------------------------------------
send_after_close_returns_closed() ->
    [{doc,"ssl:send/2 on a socket its owner has closed returns {error, closed}"}].

send_after_close_returns_closed(Config) when is_list(Config) ->
    {Server, Client} = connected_pair(Config),
    ok = ssl:close(Server),
    {error, closed} = ssl:send(Server, <<"after close">>),
    ok = ssl:close(Client).

%%--------------------------------------------------------------------
%% Both ends of a TLS connection, owned by the calling process, passive and
%% in binary mode.
connected_pair(Config) ->
    connected_pair(Config, []).

connected_pair(Config, ExtraOpts) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    GroupOpts = proplists:get_value(group_opts, Config, []),
    {_ClientNode, _ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Common = ExtraOpts ++ [{active, false}, {mode, binary} | GroupOpts],
    {ok, Listen} = ssl:listen(0, Common ++ ServerOpts),
    {ok, {_, Port}} = ssl:sockname(Listen),
    Self = self(),
    _Connector = spawn_link(
                   fun() ->
                           {ok, C} = ssl:connect(Hostname, Port, Common ++ ClientOpts, 10000),
                           ok = ssl:controlling_process(C, Self),
                           Self ! {client_socket, C}
                   end),
    {ok, Accepted} = ssl:transport_accept(Listen, 10000),
    {ok, Server} = ssl:handshake(Accepted, 10000),
    Client = receive
                 {client_socket, C} -> C
             after 10000 ->
                     exit(client_connect_timeout)
             end,
    ok = ssl:close(Listen),
    {Server, Client}.

%% Upgrade raw sockets at both ends, bypassing ssl:listen/2 and its option
%% tracker so that initialization must supply the emulated defaults.
upgraded_pair(Config, ExtraOpts) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {_ClientNode, _ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Common = ExtraOpts ++ [{active, false}, {mode, binary}],
    {ok, Listen} = tls_socket_tcp:listen(0, []),
    {ok, Port} = tls_socket_tcp:port(Listen),
    Self = self(),
    _Connector = spawn_link(
                   fun() ->
                           {ok, Raw} = tls_socket_tcp:connect(Hostname, Port, [], 10000),
                           {ok, C} = ssl:connect(Raw, Common ++ ClientOpts, 10000),
                           ok = ssl:controlling_process(C, Self),
                           Self ! {client_socket, C}
                   end),
    {ok, Accepted} = tls_socket_tcp:accept(Listen, 10000),
    {ok, Server} = ssl:handshake(Accepted, Common ++ ServerOpts, 10000),
    Client = receive
                 {client_socket, C} -> C
             after 10000 ->
                     exit(client_connect_timeout)
             end,
    ok = socket:close(Listen),
    {Server, Client}.
