%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2020. All Rights Reserved.
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

-module(ssh_renegotiate_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/file.hrl").
-include("ssh_test_lib.hrl").

-export([
         suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2
        ]).

-export([
         norekey_limit_client/0,
         norekey_limit_client/1,
         norekey_limit_daemon/0,
         norekey_limit_daemon/1,
         rekey0/0,
         rekey0/1,
         rekey1/0,
         rekey1/1,
         rekey2/0,
         rekey2/1,
         rekey3/0,
         rekey3/1,
         rekey4/0,
         rekey4/1,
         rekey_limit_client/0,
         rekey_limit_client/1,
         rekey_limit_daemon/0,
         rekey_limit_daemon/1,
         rekey_time_limit_client/0,
         rekey_time_limit_client/1,
         rekey_time_limit_daemon/0,
         rekey_time_limit_daemon/1,
         renegotiate1/1,
         renegotiate2/1
        ]).

-define(NEWLINE, <<"\r\n">>).

-define(REKEY_DATA_TMO, 1 * 60000). % Should be multiples of 60000

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,90}}].

all() -> 
    [{group, renegotiate}
    ].

groups() ->
    [{renegotiate, [parallel], [rekey0,
                                rekey1,
                                rekey2,
                                rekey3,
                                rekey4,
                                rekey_limit_client,
                                rekey_limit_daemon,
                                rekey_time_limit_client,
                                rekey_time_limit_daemon,
                                norekey_limit_client,
                                norekey_limit_daemon,
                                renegotiate1,
                                renegotiate2]}
    ].

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ?CHECK_CRYPTO(begin
                      ssh:start(),
                      ct:log("Pub keys setup for: ~p",
                             [ssh_test_lib:setup_all_user_host_keys(Config)]),
                      [{preferred_algorithms,ssh_transport:supported_algorithms()}
                       | Config]
                  end).

end_per_suite(_Config) ->
    ssh:stop().


init_per_group(_, Config) -> Config.

end_per_group(_, Config) -> Config.
%%----------------------------------------------------------------------------
%%% Idle timeout test
rekey0() -> [{timetrap,{seconds,120}}].
rekey1() -> [{timetrap,{seconds,120}}].
rekey2() -> [{timetrap,{seconds,120}}].
rekey3() -> [{timetrap,{seconds,120}}].
rekey4() -> [{timetrap,{seconds,120}}].
    
rekey0(Config) -> ssh_dbg:start(), ssh_dbg:on(renegotiation),
                  R = rekey_chk(Config, 0,                   0),
                  ssh_dbg:stop(),
                  R.
rekey1(Config) -> rekey_chk(Config, infinity,            0).
rekey2(Config) -> rekey_chk(Config, {infinity,infinity}, 0).
rekey3(Config) -> rekey_chk(Config, 0,                   infinity).
rekey4(Config) -> rekey_chk(Config, 0,                   {infinity,infinity}).

rekey_chk(Config, RLdaemon, RLclient) ->
    {Pid, Host, Port} = ssh_test_lib:std_daemon(Config,	[{rekey_limit, RLdaemon}]),
    ConnectionRef = ssh_test_lib:std_connect(Config, Host, Port, [{rekey_limit, RLclient}]),
    Kex1 = ssh_test_lib:get_kex_init(ConnectionRef),

    %% Make both sides send something:
    {ok, _SftpPid} = ssh_sftp:start_channel(ConnectionRef),

    %% Check rekeying
    timer:sleep(?REKEY_DATA_TMO),
    ?wait_match(false, Kex1==ssh_test_lib:get_kex_init(ConnectionRef), [], 2000, 10),

    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
%%% Test rekeying by data volume

rekey_limit_client() -> [{timetrap,{seconds,500}}].
rekey_limit_client(Config) ->
    Limit = 6000,
    UserDir = proplists:get_value(priv_dir, Config),
    DataFile = filename:join(UserDir, "rekey.data"),
    Data = lists:duplicate(Limit+10,1),
    Algs = proplists:get_value(preferred_algorithms, Config),
    {Pid, Host, Port} = ssh_test_lib:std_daemon(Config,[{max_random_length_padding,0},
							{preferred_algorithms,Algs}]),

    ConnectionRef = ssh_test_lib:std_connect(Config, Host, Port, [{rekey_limit, Limit},
								  {max_random_length_padding,0}]),
    {ok, SftpPid} = ssh_sftp:start_channel(ConnectionRef),

    %% Check that it doesn't rekey without data transfer
    Kex1 = ssh_test_lib:get_kex_init(ConnectionRef),
    timer:sleep(?REKEY_DATA_TMO),
    true = (Kex1 == ssh_test_lib:get_kex_init(ConnectionRef)),

    %% Check that datatransfer triggers rekeying
    ok = ssh_sftp:write_file(SftpPid, DataFile, Data),
    timer:sleep(?REKEY_DATA_TMO),
    ?wait_match(false, Kex1==(Kex2=ssh_test_lib:get_kex_init(ConnectionRef)), Kex2, 2000, 10),

    %% Check that datatransfer continues to trigger rekeying
    ok = ssh_sftp:write_file(SftpPid, DataFile, Data),
    timer:sleep(?REKEY_DATA_TMO),
    ?wait_match(false, Kex2==(Kex3=ssh_test_lib:get_kex_init(ConnectionRef)), Kex3, 2000, 10),

    %% Check that it doesn't rekey without data transfer
    timer:sleep(?REKEY_DATA_TMO),
    true = (Kex3 == ssh_test_lib:get_kex_init(ConnectionRef)),

    %% Check that it doesn't rekey on a small datatransfer
    ok = ssh_sftp:write_file(SftpPid, DataFile, "hi\n"),
    timer:sleep(?REKEY_DATA_TMO),
    true = (Kex3 == ssh_test_lib:get_kex_init(ConnectionRef)),

    %% Check that it doesn't rekey without data transfer
    timer:sleep(?REKEY_DATA_TMO),
    true = (Kex3 == ssh_test_lib:get_kex_init(ConnectionRef)),

    ssh_sftp:stop_channel(SftpPid),
    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).



rekey_limit_daemon() -> [{timetrap,{seconds,500}}].
rekey_limit_daemon(Config) ->
    Limit = 6000,
    UserDir = proplists:get_value(priv_dir, Config),
    DataFile1 = filename:join(UserDir, "rekey1.data"),
    DataFile2 = filename:join(UserDir, "rekey2.data"),
    file:write_file(DataFile1, lists:duplicate(Limit+10,1)),
    file:write_file(DataFile2, "hi\n"),

    Algs = proplists:get_value(preferred_algorithms, Config),
    {Pid, Host, Port} = ssh_test_lib:std_daemon(Config,[{rekey_limit, Limit},
                                                        {max_random_length_padding,0},
							{preferred_algorithms,Algs}]),
    ConnectionRef = ssh_test_lib:std_connect(Config, Host, Port, [{max_random_length_padding,0}]),
    {ok, SftpPid} = ssh_sftp:start_channel(ConnectionRef),

    %% Check that it doesn't rekey without data transfer
    Kex1 = ssh_test_lib:get_kex_init(ConnectionRef),
    timer:sleep(?REKEY_DATA_TMO),
    Kex1 = ssh_test_lib:get_kex_init(ConnectionRef),

    %% Check that datatransfer triggers rekeying
    {ok,_} = ssh_sftp:read_file(SftpPid, DataFile1),
    timer:sleep(?REKEY_DATA_TMO),
    ?wait_match(false, Kex1==(Kex2=ssh_test_lib:get_kex_init(ConnectionRef)), Kex2, 2000, 10),

    %% Check that datatransfer continues to trigger rekeying
    {ok,_} = ssh_sftp:read_file(SftpPid, DataFile1),
    timer:sleep(?REKEY_DATA_TMO),
    ?wait_match(false, Kex2==(Kex3=ssh_test_lib:get_kex_init(ConnectionRef)), Kex3, 2000, 10),

    %% Check that it doesn't rekey without data transfer
    timer:sleep(?REKEY_DATA_TMO),
    true = (Kex3 == ssh_test_lib:get_kex_init(ConnectionRef)),

    %% Check that it doesn't rekey on a small datatransfer
    {ok,_} = ssh_sftp:read_file(SftpPid, DataFile2),
    timer:sleep(?REKEY_DATA_TMO),
    true = (Kex3 == ssh_test_lib:get_kex_init(ConnectionRef)),

    %% Check that it doesn't rekey without data transfer
    timer:sleep(?REKEY_DATA_TMO),
    true = (Kex3 == ssh_test_lib:get_kex_init(ConnectionRef)),

    ssh_sftp:stop_channel(SftpPid),
    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).


%%--------------------------------------------------------------------
%% Check that datatransfer in the other direction does not trigger re-keying
norekey_limit_client() -> [{timetrap,{seconds,500}}].
norekey_limit_client(Config) ->
    Limit = 6000,
    UserDir = proplists:get_value(priv_dir, Config),
    DataFile = filename:join(UserDir, "rekey3.data"),
    file:write_file(DataFile, lists:duplicate(Limit+10,1)),

    Algs = proplists:get_value(preferred_algorithms, Config),
    {Pid, Host, Port} = ssh_test_lib:std_daemon(Config,[{max_random_length_padding,0},
							{preferred_algorithms,Algs}]),

    ConnectionRef = ssh_test_lib:std_connect(Config, Host, Port, [{rekey_limit, Limit},
								  {max_random_length_padding,0}]),
    {ok, SftpPid} = ssh_sftp:start_channel(ConnectionRef),

    Kex1 = ssh_test_lib:get_kex_init(ConnectionRef),
    timer:sleep(?REKEY_DATA_TMO),
    true = (Kex1 == ssh_test_lib:get_kex_init(ConnectionRef)),
    
    {ok,_} = ssh_sftp:read_file(SftpPid, DataFile),
    timer:sleep(?REKEY_DATA_TMO),
    true = (Kex1 == ssh_test_lib:get_kex_init(ConnectionRef)),
    
    ssh_sftp:stop_channel(SftpPid),
    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%% Check that datatransfer in the other direction does not trigger re-keying
norekey_limit_daemon() -> [{timetrap,{seconds,500}}].
norekey_limit_daemon(Config) ->
    Limit = 6000,
    UserDir = proplists:get_value(priv_dir, Config),
    DataFile = filename:join(UserDir, "rekey4.data"),

    Algs = proplists:get_value(preferred_algorithms, Config),
    {Pid, Host, Port} = ssh_test_lib:std_daemon(Config,[{rekey_limit, Limit},
                                                        {max_random_length_padding,0},
							{preferred_algorithms,Algs}]),

    ConnectionRef = ssh_test_lib:std_connect(Config, Host, Port, [{max_random_length_padding,0}]),
    {ok, SftpPid} = ssh_sftp:start_channel(ConnectionRef),

    Kex1 = ssh_test_lib:get_kex_init(ConnectionRef),
    timer:sleep(?REKEY_DATA_TMO),
    true = (Kex1 == ssh_test_lib:get_kex_init(ConnectionRef)),
    
    ok = ssh_sftp:write_file(SftpPid, DataFile, lists:duplicate(Limit+10,1)),
    timer:sleep(?REKEY_DATA_TMO),
    true = (Kex1 == ssh_test_lib:get_kex_init(ConnectionRef)),
    
    ssh_sftp:stop_channel(SftpPid),
    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
%%% Test rekeying by time

rekey_time_limit_client() -> [{timetrap,{seconds,500}}].
rekey_time_limit_client(Config) ->
    Minutes = ?REKEY_DATA_TMO div 60000,
    GB = 1024*1000*1000,
    Algs = proplists:get_value(preferred_algorithms, Config),
    {Pid, Host, Port} = ssh_test_lib:std_daemon(Config,[{max_random_length_padding,0},
							{preferred_algorithms,Algs}]),
    ConnectionRef = ssh_test_lib:std_connect(Config, Host, Port, [{rekey_limit, {Minutes, GB}},
                                                                  {max_random_length_padding,0}]),
    rekey_time_limit(Pid, ConnectionRef).

rekey_time_limit_daemon() -> [{timetrap,{seconds,500}}].
rekey_time_limit_daemon(Config) ->
    Minutes = ?REKEY_DATA_TMO div 60000,
    GB = 1024*1000*1000,
    Algs = proplists:get_value(preferred_algorithms, Config),
    {Pid, Host, Port} = ssh_test_lib:std_daemon(Config,[{rekey_limit, {Minutes, GB}},
                                                        {max_random_length_padding,0},
							{preferred_algorithms,Algs}]),
    ConnectionRef = ssh_test_lib:std_connect(Config, Host, Port, [{max_random_length_padding,0}]),
    rekey_time_limit(Pid, ConnectionRef).


rekey_time_limit(Pid, ConnectionRef) ->
    {ok, SftpPid} = ssh_sftp:start_channel(ConnectionRef),
    Kex1 = ssh_test_lib:get_kex_init(ConnectionRef),

    timer:sleep(5000),
    true = (Kex1 == ssh_test_lib:get_kex_init(ConnectionRef)),

    %% Check that it rekeys when the max time + 30s has passed
    timer:sleep(?REKEY_DATA_TMO + 30*1000),
    ?wait_match(false, Kex1==(Kex2=ssh_test_lib:get_kex_init(ConnectionRef)), Kex2, 2000, 10),

    %% Check that it does not rekey when nothing is transferred
    timer:sleep(?REKEY_DATA_TMO + 30*1000),
    ?wait_match(false, Kex2==ssh_test_lib:get_kex_init(ConnectionRef), [], 2000, 10),

    ssh_sftp:stop_channel(SftpPid),
    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------

%%% Test rekeying with simultaneous send request

renegotiate1(Config) ->
    UserDir = proplists:get_value(priv_dir, Config),
    DataFile = filename:join(UserDir, "renegotiate1.data"),

    Algs = proplists:get_value(preferred_algorithms, Config),
    {Pid, Host, DPort} = ssh_test_lib:std_daemon(Config,[{max_random_length_padding,0},
							 {preferred_algorithms,Algs}]),

    {ok,RelayPid,_,RPort} = ssh_relay:start_link({0,0,0,0}, 0, Host, DPort),

    ConnectionRef = ssh_test_lib:std_connect(Config, Host, RPort, [{max_random_length_padding,0}]),
    {ok, SftpPid} = ssh_sftp:start_channel(ConnectionRef),

    Kex1 = ssh_test_lib:get_kex_init(ConnectionRef),

    {ok, Handle} = ssh_sftp:open(SftpPid, DataFile, [write]),

    ok = ssh_sftp:write(SftpPid, Handle, "hi\n"),

    ssh_relay:hold(RelayPid, rx, 20, 1000),
    ssh_connection_handler:renegotiate(ConnectionRef),
    spawn(fun() -> ok=ssh_sftp:write(SftpPid, Handle, "another hi\n") end),

    timer:sleep(2000),

    Kex2 = ssh_test_lib:get_kex_init(ConnectionRef),

    false = (Kex2 == Kex1),
    
    ssh_relay:stop(RelayPid),
    ssh_sftp:stop_channel(SftpPid),
    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------

%%% Test rekeying with inflight messages from peer

renegotiate2(Config) ->
    UserDir = proplists:get_value(priv_dir, Config),
    DataFile = filename:join(UserDir, "renegotiate2.data"),

    Algs = proplists:get_value(preferred_algorithms, Config),
    {Pid, Host, DPort} = ssh_test_lib:std_daemon(Config,[{max_random_length_padding,0},
							 {preferred_algorithms,Algs}]),

    {ok,RelayPid,_,RPort} = ssh_relay:start_link({0,0,0,0}, 0, Host, DPort),

    ConnectionRef = ssh_test_lib:std_connect(Config, Host, RPort, [{max_random_length_padding,0}]),
    {ok, SftpPid} = ssh_sftp:start_channel(ConnectionRef),

    Kex1 = ssh_test_lib:get_kex_init(ConnectionRef),

    {ok, Handle} = ssh_sftp:open(SftpPid, DataFile, [write]),

    ok = ssh_sftp:write(SftpPid, Handle, "hi\n"),

    ssh_relay:hold(RelayPid, rx, 20, infinity),
    spawn(fun() -> ok=ssh_sftp:write(SftpPid, Handle, "another hi\n") end),
    %% need a small pause here to ensure ssh_sftp:write is executed
    ct:sleep(10),
    ssh_connection_handler:renegotiate(ConnectionRef),
    ssh_relay:release(RelayPid, rx),

    timer:sleep(2000),

    Kex2 = ssh_test_lib:get_kex_init(ConnectionRef),

    false = (Kex2 == Kex1),

    ssh_relay:stop(RelayPid),
    ssh_sftp:stop_channel(SftpPid),
    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
