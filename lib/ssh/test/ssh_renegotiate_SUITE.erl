%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

-module(ssh_renegotiate_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("ssh_test_lib.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(REKEY_DATA_TMO, 65000).
%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() -> [{ct_hooks,[ts_install_cth]},
	    {timetrap,{seconds,40}}].

all() -> [{group,default_algs},
	  {group,aes_gcm}
	 ].

groups() -> [{default_algs, [], tests()},
	     {aes_gcm,      [], tests()}
	    ].

tests() -> [rekey, rekey_limit, renegotiate1, renegotiate2].

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ?CHECK_CRYPTO(Config).

end_per_suite(_Config) ->
    ssh:stop().

%%--------------------------------------------------------------------
init_per_group(aes_gcm, Config) ->
    case lists:member({client2server,['aes128-gcm@openssh.com']},
		      ssh_transport:supported_algorithms(cipher)) of
	true ->
	    [{preferred_algorithms, [{cipher,[{client2server,['aes128-gcm@openssh.com']},
					      {server2client,['aes128-gcm@openssh.com']}]}]}
	     | Config];
	false ->
	    {skip, "aes_gcm not supported"}
    end;
init_per_group(_, Config) ->
    [{preferred_algorithms, ssh:default_algorithms()} | Config].


end_per_group(_, Config) ->
    Config.

%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    ssh:start(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ssh:stop(),
    ok.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

%%% Idle timeout test
rekey() -> [{timetrap,{seconds,90}}].
    
rekey(Config) ->
    {Pid, Host, Port} = 
	ssh_test_lib:std_daemon(Config,
				[{rekey_limit, 0}]),
    ConnectionRef =
	ssh_test_lib:std_connect(Config, Host, Port, 
				 [{rekey_limit, 0}]),
    Kex1 = get_kex_init(ConnectionRef),
    receive
    after ?REKEY_DATA_TMO ->
	    %%By this time rekeying would have been done
	    Kex2 = get_kex_init(ConnectionRef),
	    false = (Kex2 == Kex1),
	    ssh:close(ConnectionRef),
	    ssh:stop_daemon(Pid)
    end.

%%--------------------------------------------------------------------

%%% Test rekeying by data volume

rekey_limit() -> [{timetrap,{seconds,400}}].

rekey_limit(Config) ->
    UserDir = proplists:get_value(priv_dir, Config),
    DataFile = filename:join(UserDir, "rekey.data"),

    Algs = proplists:get_value(preferred_algorithms, Config),
    {Pid, Host, Port} = ssh_test_lib:std_daemon(Config,[{max_random_length_padding,0},
							{preferred_algorithms,Algs}]),

    ConnectionRef = ssh_test_lib:std_connect(Config, Host, Port, [{rekey_limit, 6000},
								  {max_random_length_padding,0}]),
    {ok, SftpPid} = ssh_sftp:start_channel(ConnectionRef),

    Kex1 = get_kex_init(ConnectionRef),

    timer:sleep(?REKEY_DATA_TMO),
    Kex1 = get_kex_init(ConnectionRef),

    Data = lists:duplicate(159000,1),
    ok = ssh_sftp:write_file(SftpPid, DataFile, Data),

    timer:sleep(?REKEY_DATA_TMO),
    Kex2 = get_kex_init(ConnectionRef),

    false = (Kex2 == Kex1),

    timer:sleep(?REKEY_DATA_TMO),
    Kex2 = get_kex_init(ConnectionRef),

    ok = ssh_sftp:write_file(SftpPid, DataFile, "hi\n"),

    timer:sleep(?REKEY_DATA_TMO),
    Kex2 = get_kex_init(ConnectionRef),

    false = (Kex2 == Kex1),

    timer:sleep(?REKEY_DATA_TMO),
    Kex2 = get_kex_init(ConnectionRef),

    ssh_sftp:stop_channel(SftpPid),
    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------

%%% Test rekeying with simulataneous send request

renegotiate1(Config) ->
    UserDir = proplists:get_value(priv_dir, Config),
    DataFile = filename:join(UserDir, "renegotiate1.data"),

    Algs = proplists:get_value(preferred_algorithms, Config),
    {Pid, Host, DPort} = ssh_test_lib:std_daemon(Config,[{max_random_length_padding,0},
							 {preferred_algorithms,Algs}]),

    RPort = ssh_test_lib:inet_port(),
    {ok,RelayPid} = ssh_relay:start_link({0,0,0,0}, RPort, Host, DPort),


    ConnectionRef = ssh_test_lib:std_connect(Config, Host, RPort, [{max_random_length_padding,0}]),
    {ok, SftpPid} = ssh_sftp:start_channel(ConnectionRef),

    Kex1 = get_kex_init(ConnectionRef),

    {ok, Handle} = ssh_sftp:open(SftpPid, DataFile, [write]),

    ok = ssh_sftp:write(SftpPid, Handle, "hi\n"),

    ssh_relay:hold(RelayPid, rx, 20, 1000),
    ssh_connection_handler:renegotiate(ConnectionRef),
    spawn(fun() -> ok=ssh_sftp:write(SftpPid, Handle, "another hi\n") end),

    timer:sleep(2000),

    Kex2 = get_kex_init(ConnectionRef),

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

    RPort = ssh_test_lib:inet_port(),
    {ok,RelayPid} = ssh_relay:start_link({0,0,0,0}, RPort, Host, DPort),

    ConnectionRef = ssh_test_lib:std_connect(Config, Host, RPort, [{max_random_length_padding,0}]),
    {ok, SftpPid} = ssh_sftp:start_channel(ConnectionRef),

    Kex1 = get_kex_init(ConnectionRef),

    {ok, Handle} = ssh_sftp:open(SftpPid, DataFile, [write]),

    ok = ssh_sftp:write(SftpPid, Handle, "hi\n"),

    ssh_relay:hold(RelayPid, rx, 20, infinity),
    spawn(fun() -> ok=ssh_sftp:write(SftpPid, Handle, "another hi\n") end),
    %% need a small pause here to ensure ssh_sftp:write is executed
    ct:sleep(10),
    ssh_connection_handler:renegotiate(ConnectionRef),
    ssh_relay:release(RelayPid, rx),

    timer:sleep(2000),

    Kex2 = get_kex_init(ConnectionRef),

    false = (Kex2 == Kex1),

    ssh_relay:stop(RelayPid),
    ssh_sftp:stop_channel(SftpPid),
    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
%% get_kex_init - helper function to get key_exchange_init_msg
get_kex_init(Conn) ->
    %% First, validate the key exchange is complete (StateName == connected)
    {{connected,_},S} = sys:get_state(Conn),
    %% Next, walk through the elements of the #state record looking
    %% for the #ssh_msg_kexinit record. This method is robust against
    %% changes to either record. The KEXINIT message contains a cookie
    %% unique to each invocation of the key exchange procedure (RFC4253)
    SL = tuple_to_list(S),
    case lists:keyfind(ssh_msg_kexinit, 1, SL) of
	false ->
	    throw(not_found);
	KexInit ->
	    KexInit
    end.

