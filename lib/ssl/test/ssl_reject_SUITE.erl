%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021-2024. All Rights Reserved.
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

-module(ssl_reject_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("ssl_record.hrl").
-include_lib("ssl/src/ssl_alert.hrl").
-include_lib("ssl/src/ssl_handshake.hrl").

%% Common test
-export([all/0,
         groups/0,
         init_per_suite/1,
         init_per_group/2,
         init_per_testcase/2,
         end_per_suite/1,
         end_per_group/2,
         end_per_testcase/2
        ]).

%% Test cases
-export([reject_prev/0,
         reject_prev/1,
         reject_sslv2/0,
         reject_sslv2/1,
         reject_sslv3/0,
         reject_sslv3/1,
         accept_sslv3_record_hello/0,
         accept_sslv3_record_hello/1
        ]).

-define(TLS_MAJOR,     (element(1, ?TLS_1_2))).
-define(SSL_3_0_MAJOR, (element(1, ?SSL_3_0))).
-define(SSL_3_0_MINOR, (element(2, ?SSL_3_0))).
-define(TLS_1_0_MINOR, (element(2, ?TLS_1_0))).
-define(TLS_1_1_MINOR, (element(2, ?TLS_1_1))).
-define(TLS_1_2_MINOR, (element(2, ?TLS_1_2))).
-define(TLS_1_3_MINOR, (element(2, ?TLS_1_3))).
-define(SSL_2_0_MAJOR, (element(1, ?SSL_2_0))).
-define(SSL_2_0_MINOR, (element(2, ?SSL_2_0))).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [
     {group, 'tlsv1.3'},
     {group, 'tlsv1.2'},
     {group, 'tlsv1.1'},
     {group, 'tlsv1'},
     {group, 'dtlsv1.2'}
    ].

groups() ->
    [{'tlsv1.3', [], [reject_prev] ++ all_tls_version_tests()},
     {'tlsv1.2', [],  [reject_prev] ++ all_tls_version_tests()},
     {'tlsv1.1', [],  [reject_prev] ++ all_tls_version_tests()},
     {'tlsv1', [], all_tls_version_tests()},
     {'dtlsv1.2', [], [reject_prev]}
    ].

all_tls_version_tests() ->
    [
     reject_sslv2,
     reject_sslv3,
     accept_sslv3_record_hello
    ].

init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
        ok ->
            ssl_test_lib:clean_start(),
            ssl_test_lib:make_rsa_cert(Config0)
    catch _:_  ->
            {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto),
    application:stop(ssl).

init_per_group(GroupName, Config) ->
    ssl_test_lib:init_per_group(GroupName, Config).

end_per_group(GroupName, Config) ->
    ssl_test_lib:end_per_group(GroupName, Config).

init_per_testcase(_TestCase, Config) ->
    ct:timetrap({seconds, 10}),
    Config.

end_per_testcase(_, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test cases  -----------------------------------
%%--------------------------------------------------------------------

reject_sslv2() ->
    [{doc,"Test that SSL v2 clients are rejected"}].

reject_sslv2(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),

    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
                                              {from, self()},
                                              {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    %% SSL-2.0 Hello
    ClientHello = <<128,43,?CLIENT_HELLO, ?SSL_2_0_MAJOR, ?SSL_2_0_MINOR,
                    0,18,0,0,0,16,7,0,192,3,0,128,1,0,128,6,0,64,4,0,
                    128,2,0,128,115,245,33,148,17,175,69,226,204,214,132,216,182,
                    41,238,196>>,

    {ok, Socket} = gen_tcp:connect(Hostname, Port, [{active, false}]),

    gen_tcp:send(Socket, ClientHello),

    %% v2 is considered total garbage
    ssl_test_lib:check_server_alert(Server, unexpected_message),
    client_rejected(Socket, unexpected_message).

reject_sslv3() ->
    [{doc,"Test that SSL v3 clients are rejected"}].

reject_sslv3(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
                                              {from, self()},
                                              {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    %% SSL-3.0 Hello
    ClientHello =
        <<?HANDSHAKE, ?SSL_3_0_MAJOR, ?SSL_3_0_MINOR,0,162, ?CLIENT_HELLO, 0,0,158,
          ?TLS_MAJOR, ?SSL_3_0_MINOR, 97,160,130,59,226,182,64,143,134,112,117,
          64,10,57,164,101,182,215,0,199,145,232,172,194,45,242,48,176,5,153,
          101,54,0,0,26,0,255,192,10,192,20,192,5,192,15,192,9,192,19,192,4,192,
          14,0,57,0,56,0,51,0,50,1,0,0,91,0,0,0,19,0,17,0,0,14,119,119,119,46,101,
          114,108,97,110,103,46,111,114,103,0,10,0,58,0,56,0,14,0,13,0,25,0,28,0,
          11,0,12,0,27,0,24,0,9,0,10,0,26,0,22,0,23,0,8,0,6,0,7,0,20,0,21,0,4,0,5,
          0,18,0,19,0,1,0,2,0,3,0,15,0,16,0,17,0,11,0,2,1,0>>,

    {ok, Socket} = gen_tcp:connect(Hostname, Port, [{active, false}]),
    gen_tcp:send(Socket, ClientHello),
    %% v3 is not a supported protocol version (but hello record could have 3.0 for legacy interop)
    ssl_test_lib:check_server_alert(Server, protocol_version),
    client_rejected(Socket, protocol_version).

accept_sslv3_record_hello() ->
    [{doc,"Test that ssl v3 record in clients hellos are ignored when higher version are advertised"}].

accept_sslv3_record_hello(Config) when is_list(Config) ->
    Version = proplists:get_value(version, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    {_, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Allversions = all_versions(),

    AllSigAlgs = ssl:signature_algs(all, 'tlsv1.3'),
    Ciphers = ssl:filter_cipher_suites(ssl:cipher_suites(all, 'tlsv1.3'),
                                       [{key_exchange, fun(srp_rsa) -> false;
                                                          (srp_dss) -> false;
                                                          (_) -> true
                                                       end}]),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
                                        {from, self()},
                                        {options, [{versions, Allversions}, 
                                                   {signature_algs, AllSigAlgs}, {ciphers, Ciphers} | ServerOpts]}]),
    Port = ssl_test_lib:inet_port(Server),

    %% TLS-1.X Hello with SSL-3.0 record version
    ClientHello = hello_with_3_0_record(Version),

    {ok, Socket} = gen_tcp:connect(Hostname, Port, [{active, false}]),
    gen_tcp:send(Socket, ClientHello),
    TLS_Major = ?TLS_MAJOR,
    case gen_tcp:recv(Socket, 3, 5000) of
        %% Minor needs to be a TLS version that is a version
        %% above SSL-3.0
        {ok, [?HANDSHAKE, TLS_Major, Minor]} when Minor > ?SSL_3_0_MINOR ->
            ok;
        {error, timeout} ->
            ct:fail(ssl3_record_not_accepted)
    end.


reject_prev() ->
    [{doc,"Test that prev version is rejected, for all version where there"
      "exists possible support a previous version, that is not configured"}].

reject_prev(Config) when is_list(Config) ->
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Version = proplists:get_value(version, Config),
    PrevVersion = prev_version(Version),

    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					      {from, self()},
					      {mfa, {ssl_test_lib,
						     no_result, []}},
					      {options, ServerOpts}]),
    Port  = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
					      {host, Hostname},
					      {from, self()},
					      {mfa, {ssl_test_lib,
						     no_result, []}},
					      {options,[{versions, [PrevVersion]} | ClientOpts]}]),
    ssl_test_lib:check_client_alert(Server, Client, protocol_version).

%%--------------------------------------------------------------------
%% Internal functions -----------------------------------
%%--------------------------------------------------------------------

all_versions() ->
    [Version || Version  <- ['tlsv1.3', 'tlsv1.2', 'tlsv1.1', tlsv1],
                ssl_test_lib:sufficient_crypto_support(Version)].

client_rejected(Socket, Alert) ->
    Num = alert_num(Alert),
    case gen_tcp:recv(Socket, 7, 6000) of
        {ok,[?ALERT, _Major, _Minor, _Len1, _Len2, _Level, Num]} ->
            ok;
        Other ->
            ct:fail(Other)
    end.

alert_num(unexpected_message) ->
    ?UNEXPECTED_MESSAGE;
alert_num(illegal_parameter) ->
    ?ILLEGAL_PARAMETER;
alert_num(protocol_version) ->
    ?PROTOCOL_VERSION.

hello_with_3_0_record('tlsv1') ->
    <<?HANDSHAKE, ?TLS_MAJOR, ?SSL_3_0_MINOR, 0,162, ?CLIENT_HELLO,0,0,158,
      ?TLS_MAJOR, ?TLS_1_0_MINOR, 97,160,130,59,226,182,64,143,134,112,117,
      64,10,57,164,101,182,215,0,199,145,232,172,194,45,242,48,176,5,153,
      101,54,0,0,26,0,255,192,10,192,20,192,5,192,15,192,9,192,19,192,4,192,
      14,0,57,0,56,0,51,0,50,1,0,0,91,0,0,0,19,0,17,0,0,14,119,119,119,46,101,
      114,108,97,110,103,46,111,114,103,0,10,0,58,0,56,0,14,0,13,0,25,0,28,0,
      11,0,12,0,27,0,24,0,9,0,10,0,26,0,22,0,23,0,8,0,6,0,7,0,20,0,21,0,4,0,5,
      0,18,0,19,0,1,0,2,0,3,0,15,0,16,0,17,0,11,0,2,1,0>>;
hello_with_3_0_record('tlsv1.1') ->
    <<?HANDSHAKE, ?TLS_MAJOR, ?SSL_3_0_MINOR, 0,162, ?CLIENT_HELLO, 0,0,158,
      ?TLS_MAJOR, ?TLS_1_1_MINOR, 97,160,130,59,226,182,64,143,134,112,117,
      64,10,57,164,101,182,215,0,199,145,232,172,194,45,242,48,176,5,153,
      101,54,0,0,26,0,255,192,10,192,20,192,5,192,15,192,9,192,19,192,4,192,
      14,0,57,0,56,0,51,0,50,1,0,0,91,0,0,0,19,0,17,0,0,14,119,119,119,46,101,
      114,108,97,110,103,46,111,114,103,0,10,0,58,0,56,0,14,0,13,0,25,0,28,0,
      11,0,12,0,27,0,24,0,9,0,10,0,26,0,22,0,23,0,8,0,6,0,7,0,20,0,21,0,4,0,5,
      0,18,0,19,0,1,0,2,0,3,0,15,0,16,0,17,0,11,0,2,1,0>>;
hello_with_3_0_record('tlsv1.2') ->
    <<?HANDSHAKE, ?TLS_MAJOR, ?SSL_3_0_MINOR, 0,252, ?CLIENT_HELLO, 0,0,248,
      ?TLS_MAJOR, ?TLS_1_2_MINOR, 97,160,7,242,30,221,248,238,18,3,225,13,40,18,16,117,30,
       159,250,156,175,90,184,65,177,226,217,125,205,227,110,154,0,0,88,0,255,
       192,44,192,48,192,173,192,175,192,36,192,40,204,169,204,168,192,43,192,
       47,192,172,192,174,192,46,192,50,192,38,192,42,192,45,192,49,192,35,192,
       39,192,37,192,41,0,159,0,163,0,107,0,106,0,158,0,162,204,170,0,103,0,64,
       192,10,192,20,192,5,192,15,192,9,192,19,192,4,192,14,0,57,0,56,0,51,0,50,
       1,0,0,119,0,0,0,19,0,17,0,0,14,119,119,119,46,101,114,108,97,110,103,46,
       111,114,103,0,13,0,24,0,22,6,3,6,1,5,3,5,1,4,3,4,1,3,3,3,1,2,3,2,1,2,2,0,
       10,0,58,0,56,0,14,0,13,0,25,0,28,0,11,0,12,0,27,0,24,0,9,0,10,0,26,0,22,
       0,23,0,8,0,6,0,7,0,20,0,21,0,4,0,5,0,18,0,19,0,1,0,2,0,3,0,15,0,16,0,17,
       0,11,0,2,1,0>>;
hello_with_3_0_record('tlsv1.3') ->
    <<?HANDSHAKE, ?TLS_MAJOR, ?SSL_3_0_MINOR, 0,219,?CLIENT_HELLO, 0,0,215,
      ?TLS_MAJOR, ?TLS_1_2_MINOR, %% TLS_1.3 has LEGACY version TLS-1.2 here
      97,160,140,70,177,254,168,106,75,198,216,169,71,146,133,144,28,135,
      35,26,222,109,13,169,12,61,229,79,110,238,192,242,32,179,104,86,13,116,
      85,208,242,78,97,216,13,252,63,99,225,0,237,43,221,117,25,238,128,174,
      158,218,232,249,211,93,96,0,12,0,255,19,2,19,1,19,3,19,4,19,5,1,0,0,
      130,0,0,0,19,0,17,0,0,14,119,119,119,46,101,114,108,97,110,103,46,111,
      114,103,0,13,0,34,0,32,6,3,5,3,4,3,8,11,8,10,8,9,8,6,8,5,8,4,8,7,8,8,6,
      1,5,1,4,1,2,3,2,1,0,51,0,38,0,36,0,29,0,32,171,47,137,226,39,16,202,89,
      42,42,32,73,84,134,110,74,110,163,140,111,177,126,133,118,141,2,153,
      156,157,205,101,69,0,10,0,10,0,8,0,29,0,30,0,23,0,24,0,11,0,2,1,0,0,
      ?SUPPORTED_VERSIONS_EXT,0,3,2,?TLS_MAJOR, ?TLS_1_3_MINOR>>.

prev_version('tlsv1.3') ->
    'tlsv1.2';
prev_version('tlsv1.2') ->
    'tlsv1.1';
prev_version('tlsv1.1') ->
    'tlsv1';
prev_version('dtlsv1.2') ->
    'dtlsv1'.
