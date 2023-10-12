%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2023. All Rights Reserved.
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
-module(ssl_mfl_SUITE).

-behaviour(ct_suite).

-include("ssl_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").
-include("ssl_record.hrl").

%% Common test
-export([all/0,
         groups/0,
         init_per_suite/1, 
         init_per_group/2, 
         init_per_testcase/2,
         end_per_suite/1,
         end_per_group/2,
         end_per_testcase/2]).

%% Testcases
-export([client_option/1, 
         server_option/1, 
         reuse_session/1,
         handshake_continue/1]).

%% export for use in ssl_test_lib
-export([assert_mfl_and_send_first/3, 
         assert_mfl_and_recv_first/3]).

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
    [{'tlsv1.3', [], common_tests()},
     {'tlsv1.2', [], common_tests() ++ pre_tls_1_3()},
     {'tlsv1.1', [], common_tests() ++ pre_tls_1_3()},
     {'tlsv1', [], common_tests() ++ pre_tls_1_3()},
     {'dtlsv1.2', [], common_tests() ++ pre_tls_1_3()},
     {'dtlsv1', [], common_tests() ++ pre_tls_1_3()}
    ].

common_tests() ->
    [client_option, server_option, handshake_continue].

pre_tls_1_3() ->
    [reuse_session].

init_per_suite(Config0) ->
    Config1 = ssl_test_lib:init_per_suite(Config0, openssl),
    Config = ssl_test_lib:make_rsa_cert(Config1),
    ssl_test_lib:cert_options(Config).

end_per_suite(Config) ->
    ssl_test_lib:end_per_suite(Config).

init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group(GroupName, Config).

end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

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
client_option(Config) when is_list(Config) ->
    ok = run_mfl_handshake(Config,  512),
    ok = run_mfl_handshake(Config, 1024),
    ok = run_mfl_handshake(Config, 2048),
    ok = run_mfl_handshake(Config, 4096),
    ok = run_mfl_handshake(Config, undefined),
    ok.

%--------------------------------------------------------------------------------
%% check default max_fragment_length both sides can successfully send > 512 bytes
server_option(Config) when is_list(Config) ->
    Data = "mfl_server_options " ++ lists:duplicate(512, $x),
    run_mfl_handshake(Config, undefined, Data, [], []).

%--------------------------------------------------------------------------------
%% check max_fragment_length option on the client is accepted and reused
reuse_session(Config) when is_list(Config) ->
    MFL = 512,
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config) ++
        [{max_fragment_length, MFL}],
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),

    ssl_test_lib:reuse_session(ClientOpts, ServerOpts, Config).

%%--------------------------------------------------------------------
handshake_continue(Config) when is_list(Config) ->
    ok = run_mfl_handshake_continue(Config, 1024),
    ok = run_mfl_handshake_continue(Config, undefined),
    ok.
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
    run_mfl_handshake(Config, MFL, Data, ClientExtraOpts, ServerExtraOpts, 
                      [], [], fun(_,_) -> ok end).

run_mfl_handshake(Config, MFL, Data, ClientExtraOpts, ServerExtraOpts, 
                  ClientExtraStartOpts, ServerExtraStartOpts,
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
                            ?CT_LOG("Server handshake Ext ~p~n", [ServerExt]),
                            MflEnum = maps:get(max_frag_enum, ServerExt, undefined)
                    end,
                    receive {Client, {ext, ClientExt}} ->
                            ?CT_LOG("Client handshake Ext ~p~n", [ClientExt]),
                            case maps:get(server_hello_selected_version, ClientExt, undefined) of
                                ?TLS_1_3 ->
                                    %% For TLS 1.3 the ssl {handshake, hello} API is inconsistent:
                                    %% the server gets all the extensions CH+EE, but the client only CH
                                    ignore;
                                _ ->
                                    MflEnum = maps:get(max_frag_enum, ClientExt, undefined)
                            end
                    end,
                    ok
            end,

    run_mfl_handshake(Config, MFL, Data, ClientExtraOpts, ServerExtraOpts, 
                      ExtraStartOpts, ExtraStartOpts, PostF).

assert_mfl_and_send_first(Socket, MFL, Data) ->
    ssl_test_lib:assert_mfl(Socket, MFL),
    ssl_send(Socket, Data),
    ssl_receive(Socket, "Got it"++lists:reverse(Data)).

assert_mfl_and_recv_first(Socket, MFL, Data) ->
    ssl_test_lib:assert_mfl(Socket, MFL),
    ssl_receive(Socket, Data),
    ssl_send(Socket, lists:reverse(Data)).

ssl_send(Socket, Data) ->
    ssl:send(Socket, Data).

ssl_receive(Socket, Data) ->
    ssl_receive(Socket, Data, []).

ssl_receive(Socket, Data, Buffer) ->
    receive
        {ssl, Socket, MoreData} ->
            ?CT_LOG("Received ~p~n",[MoreData]),
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

%% RFC 6066
mfl_enum(512) -> 1;
mfl_enum(1024) -> 2;
mfl_enum(2048) -> 3;
mfl_enum(4096) -> 4;
mfl_enum(undefined) -> undefined.
