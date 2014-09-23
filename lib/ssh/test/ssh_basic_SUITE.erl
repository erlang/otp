%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2014. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%

-module(ssh_basic_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/inet.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(NEWLINE, <<"\r\n">>).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() -> 
    [app_test,
     appup_test,
     {group, dsa_key},
     {group, rsa_key},
     {group, dsa_pass_key},
     {group, rsa_pass_key},
     {group, internal_error},
     daemon_already_started,
     server_password_option,
     server_userpassword_option,
     double_close,
     ssh_connect_timeout,
     ssh_connect_arg4_timeout,
     {group, hardening_tests}
    ].

groups() ->
    [{dsa_key, [], basic_tests()},
     {rsa_key, [], basic_tests()},
     {dsa_pass_key, [], [pass_phrase]},
     {rsa_pass_key, [], [pass_phrase]},
     {internal_error, [], [internal_error]},
     {hardening_tests, [], [ssh_connect_nonegtimeout_connected_parallel,
			    ssh_connect_nonegtimeout_connected_sequential,
			    ssh_connect_negtimeout_parallel,
			    ssh_connect_negtimeout_sequential,
			    max_sessions_ssh_connect_parallel,
			    max_sessions_ssh_connect_sequential,
			    max_sessions_sftp_start_channel_parallel,
			    max_sessions_sftp_start_channel_sequential
			   ]}
    ].


basic_tests() ->
    [send, close, peername_sockname,
     exec, exec_compressed, shell, cli, known_hosts, 
     idle_time, rekey, openssh_zlib_basic_test,
     misc_ssh_options, inet_option].


%%--------------------------------------------------------------------
init_per_suite(Config) ->
    case catch crypto:start() of
	ok ->
	    Config;
	_Else ->
	    {skip, "Crypto could not be started!"}
    end.
end_per_suite(_Config) ->
    ssh:stop(),
    crypto:stop().
%%--------------------------------------------------------------------
init_per_group(hardening_tests, Config) ->
    init_per_group(dsa_key, Config);
init_per_group(dsa_key, Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:setup_dsa(DataDir, PrivDir),
    Config;
init_per_group(rsa_key, Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:setup_rsa(DataDir, PrivDir),
    Config;
init_per_group(rsa_pass_key, Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:setup_rsa_pass_pharse(DataDir, PrivDir, "Password"),
    [{pass_phrase, {rsa_pass_phrase, "Password"}}| Config];
init_per_group(dsa_pass_key, Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:setup_dsa_pass_pharse(DataDir, PrivDir, "Password"),
    [{pass_phrase, {dsa_pass_phrase, "Password"}}| Config];
init_per_group(internal_error, Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:setup_dsa(DataDir, PrivDir),
    file:delete(filename:join(PrivDir, "system/ssh_host_dsa_key")),
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(hardening_tests, Config) ->
    end_per_group(dsa_key, Config);
end_per_group(dsa_key, Config) ->
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:clean_dsa(PrivDir),
    Config;
end_per_group(rsa_key, Config) ->
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:clean_rsa(PrivDir),
    Config;
end_per_group(dsa_pass_key, Config) ->
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:clean_dsa(PrivDir),
    Config;
end_per_group(rsa_pass_key, Config) ->
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:clean_rsa(PrivDir),
    Config;
end_per_group(internal_error, Config) ->
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:clean_dsa(PrivDir),
    Config;

end_per_group(_, Config) ->
    Config.
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    ssh:start(),
    Config.

end_per_testcase(TestCase, Config) when TestCase == server_password_option;
					TestCase == server_userpassword_option ->
    UserDir = filename:join(?config(priv_dir, Config), nopubkey),
    ssh_test_lib:del_dirs(UserDir),
    end_per_testcase(Config);
end_per_testcase(_TestCase, Config) ->
    end_per_testcase(Config).
end_per_testcase(_Config) ->    
    ssh:stop(),
    ok.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
app_test() ->
    [{doc, "App lication consistency test."}].
app_test(Config) when is_list(Config) ->
    ?t:app_test(ssh),
    ok.
%%--------------------------------------------------------------------
appup_test() ->
    [{doc, "Appup file consistency test."}].
appup_test(Config) when is_list(Config) ->
    ok = ?t:appup_test(ssh).
%%--------------------------------------------------------------------
misc_ssh_options() ->
    [{doc, "Test that we can set some misc options not tested elsewhere, "
      "some options not yet present are not decided if we should support or "
      "if they need thier own test case."}].
misc_ssh_options(Config) when is_list(Config) ->  
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),
    
    CMiscOpt0 = [{connect_timeout, 1000}, {user_dir, UserDir}],
    CMiscOpt1 = [{connect_timeout, infinity}, {user_dir, UserDir}],
    SMiscOpt0 =  [{user_dir, UserDir}, {system_dir, SystemDir}],
    SMiscOpt1 =  [{user_dir, UserDir}, {system_dir, SystemDir}],

    basic_test([{client_opts, CMiscOpt0}, {server_opts, SMiscOpt0}]),
    basic_test([{client_opts, CMiscOpt1}, {server_opts, SMiscOpt1}]).

%%--------------------------------------------------------------------
inet_option() ->
    [{doc, "Test configuring IPv4"}].
inet_option(Config) when is_list(Config) ->   
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),
    
    ClientOpts =  [{silently_accept_hosts, true},
		   {user_dir, UserDir},
		   {user_interaction, false}],
    ServerOpts = [{system_dir, SystemDir},
		  {user_dir, UserDir},
		  {failfun, fun ssh_test_lib:failfun/2}], 

    basic_test([{client_opts, [{inet, inet} | ClientOpts]}, 
		{server_opts, [{inet, inet} | ServerOpts]}]).

%%--------------------------------------------------------------------
inet6_option() ->
    [{doc, "Test configuring IPv6"}].
inet6_option(Config) when is_list(Config) ->   
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),
    
    ClientOpts =  [{silently_accept_hosts, true},
		   {user_dir, UserDir},
		   {user_interaction, false}],
    ServerOpts = [{system_dir, SystemDir},
		  {user_dir, UserDir},
		  {failfun, fun ssh_test_lib:failfun/2}], 

    basic_test([{client_opts, [{inet, inet6} | ClientOpts]}, 
		{server_opts, [{inet, inet6} | ServerOpts]}]).

%%--------------------------------------------------------------------
exec() ->
    [{doc, "Test api function ssh_connection:exec"}].
exec(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),
    
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
					  {user_interaction, false}]),
    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "1+1.", infinity),
    Data0 = {ssh_cm, ConnectionRef, {data, ChannelId0, 0, <<"2\n">>}},
    case ssh_test_lib:receive_exec_result(Data0) of
	expected ->
	    ok;
	Other0 ->
	    ct:fail(Other0)
    end,
    ssh_test_lib:receive_exec_end(ConnectionRef, ChannelId0),

    %% Test that it is possible to start a new channel and
    %% run an other exec on the same connection.
    {ok, ChannelId1} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId1,
				  "2+2.", infinity),
    Data1 = {ssh_cm, ConnectionRef, {data, ChannelId1, 0, <<"4\n">>}},
    case ssh_test_lib:receive_exec_result(Data1) of
	expected ->
	    ok;
	Other1 ->
	    ct:fail(Other1)
    end,
    ssh_test_lib:receive_exec_end(ConnectionRef, ChannelId1),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
exec_compressed() ->
    [{doc, "Test that compression option works"}].
exec_compressed(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config), 

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},{user_dir, UserDir},
					     {compression, zlib},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
					  {user_interaction, false}]),
    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId,
				  "1+1.", infinity),
    Data = {ssh_cm, ConnectionRef, {data, ChannelId, 0, <<"2\n">>}},
    case ssh_test_lib:receive_exec_result(Data) of
	expected ->
	    ok;
	Other ->
	    ct:fail(Other)
    end,
    ssh_test_lib:receive_exec_end(ConnectionRef, ChannelId),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
idle_time() ->
    [{doc, "Idle timeout test"}].
idle_time(Config) ->
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
					  {user_interaction, false},
					  {idle_time, 2000}]),
    {ok, Id} = ssh_connection:session_channel(ConnectionRef, 1000),
    ssh_connection:close(ConnectionRef, Id),
    receive
    after 10000 ->
	    {error, closed} = ssh_connection:session_channel(ConnectionRef, 1000)
    end,
    ssh:stop_daemon(Pid).
%%--------------------------------------------------------------------
rekey() ->
    [{doc, "Idle timeout test"}].
rekey(Config) ->
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir},
					     {failfun, fun ssh_test_lib:failfun/2},
					     {rekey_limit, 0}]),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
					  {user_interaction, false},
					  {rekey_limit, 0}]),
    receive
    after 200000 ->
	    %%By this time rekeying would have been done
	    ssh:close(ConnectionRef),
	    ssh:stop_daemon(Pid)
    end.
%%--------------------------------------------------------------------
shell() ->
    [{doc, "Test that ssh:shell/2 works"}].
shell(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),
   
    {_Pid, _Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},{user_dir, UserDir},
					       {failfun, fun ssh_test_lib:failfun/2}]),
    ct:sleep(500),

    IO = ssh_test_lib:start_io_server(),
    Shell = ssh_test_lib:start_shell(Port, IO, UserDir),
    receive
	{'EXIT', _, _} ->
	    ct:fail(no_ssh_connection);  
	ErlShellStart ->
	    ct:pal("Erlang shell start: ~p~n", [ErlShellStart]),
	    do_shell(IO, Shell)
    end.
    
%%--------------------------------------------------------------------
cli() ->
    [{doc, ""}].
cli(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),
   
    {_Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},{user_dir, UserDir},
					       {password, "morot"},
					       {ssh_cli, {ssh_test_cli, [cli]}}, 
					       {subsystems, []},
					       {failfun, fun ssh_test_lib:failfun/2}]),
    ct:sleep(500),
    
    ConnectionRef = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
						      {user, "foo"},
						      {password, "morot"},
						      {user_interaction, false},
						      {user_dir, UserDir}]),
    
    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    ssh_connection:shell(ConnectionRef, ChannelId),
    ok = ssh_connection:send(ConnectionRef, ChannelId, <<"q">>),
    receive 
	{ssh_cm, ConnectionRef,
	 {data,0,0, <<"\r\nYou are accessing a dummy, type \"q\" to exit\r\n\n">>}} ->
	    ok = ssh_connection:send(ConnectionRef, ChannelId, <<"q">>)
    end,
    
    receive 
     	{ssh_cm, ConnectionRef,{closed, ChannelId}} ->
     	    ok
    end.

%%--------------------------------------------------------------------
daemon_already_started() ->
    [{doc, "Test that get correct error message if you try to start a daemon",
      "on an adress that already runs a daemon see also seq10667"}].
daemon_already_started(Config) when is_list(Config) ->
    SystemDir = ?config(data_dir, Config),
    UserDir = ?config(priv_dir, Config),

    {Pid, _Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					      {user_dir, UserDir},
					      {failfun, fun ssh_test_lib:failfun/2}]),
    {error, eaddrinuse} = ssh_test_lib:daemon(Port, [{system_dir, SystemDir},
						     {user_dir, UserDir},
						     {failfun,
						      fun ssh_test_lib:failfun/2}]),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
server_password_option() ->
    [{doc, "validate to server that uses the 'password' option"}].
server_password_option(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"}]),

    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_interaction, false},
					  {user_dir, UserDir}]),

    Reason = "Unable to connect using the available authentication methods",
    
    {error, Reason} =
	ssh:connect(Host, Port, [{silently_accept_hosts, true},
				 {user, "vego"},
				 {password, "foo"},
				 {user_interaction, false},
				 {user_dir, UserDir}]),
    
    ct:pal("Test of wrong password: Error msg: ~p ~n", [Reason]),

    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------

server_userpassword_option() ->
    [{doc, "validate to server that uses the 'password' option"}].
server_userpassword_option(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),	  
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, PrivDir},
					     {user_passwords, [{"vego", "morot"}]}]),

    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "vego"},
					  {password, "morot"},
					  {user_interaction, false},
					  {user_dir, UserDir}]),
    ssh:close(ConnectionRef),

    Reason = "Unable to connect using the available authentication methods",

    {error, Reason} =
	ssh:connect(Host, Port, [{silently_accept_hosts, true},
				 {user, "foo"},
				 {password, "morot"},
				 {user_interaction, false},
				 {user_dir, UserDir}]),
    {error, Reason} =
	ssh:connect(Host, Port, [{silently_accept_hosts, true},
				 {user, "vego"},
				 {password, "foo"},
				 {user_interaction, false},
				 {user_dir, UserDir}]),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
known_hosts() ->
    [{doc, "check that known_hosts is updated correctly"}].
known_hosts(Config) when is_list(Config) ->
    SystemDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config), 
    
    {Pid, Host, Port} = ssh_test_lib:daemon([{user_dir, PrivDir},{system_dir, SystemDir},
					     {failfun, fun ssh_test_lib:failfun/2}]),

    KnownHosts = filename:join(PrivDir, "known_hosts"),
    file:delete(KnownHosts),
    {error, enoent} = file:read_file(KnownHosts),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{user_dir, PrivDir},
					  {user_interaction, false},
					  silently_accept_hosts]),
    {ok, _Channel} = ssh_connection:session_channel(ConnectionRef, infinity),
    ok = ssh:close(ConnectionRef),
    {ok, Binary} = file:read_file(KnownHosts),
    Lines = string:tokens(binary_to_list(Binary), "\n"),
    [Line] = Lines,
    [HostAndIp, Alg, _KeyData] = string:tokens(Line, " "),
    [Host, _Ip] = string:tokens(HostAndIp, ","),
    "ssh-" ++ _ = Alg,
    ssh:stop_daemon(Pid).
%%--------------------------------------------------------------------

pass_phrase() ->
    [{doc, "Test that we can use keyes protected by pass phrases"}].
pass_phrase(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),
    PhraseArg = ?config(pass_phrase, Config),

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  PhraseArg,
					  {user_dir, UserDir},
					  {user_interaction, false}]),
    {ok, _ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------

internal_error() ->
    [{doc,"Test that client does not hang if disconnects due to internal error"}].
internal_error(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),
    
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    {error, Error} =
	ssh:connect(Host, Port, [{silently_accept_hosts, true},
				 {user_dir, UserDir},
				 {user_interaction, false}]),
    check_error(Error),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
send() ->
    [{doc, "Test ssh_connection:send/3"}].
send(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
					  {user_interaction, false}]),
    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    ok = ssh_connection:send(ConnectionRef, ChannelId, <<"Data">>),
    ok = ssh_connection:send(ConnectionRef, ChannelId, << >>),
    ssh:stop_daemon(Pid).


%%--------------------------------------------------------------------
peername_sockname() ->
    [{doc, "Test ssh:connection_info([peername, sockname])"}].
peername_sockname(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),

    {_Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir},
					     {subsystems, [{"peername_sockname",
							    {ssh_peername_sockname_server, []}}
							  ]}
					    ]),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
					  {user_interaction, false}]),
    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:subsystem(ConnectionRef, ChannelId, "peername_sockname", infinity),
    [{peer, {_Name, {HostPeerClient,PortPeerClient} = ClientPeer}}] =
	ssh:connection_info(ConnectionRef, [peer]),
    [{sockname, {HostSockClient,PortSockClient} = ClientSock}] =
	ssh:connection_info(ConnectionRef, [sockname]),
    ct:pal("Client: ~p ~p", [ClientPeer, ClientSock]),
    receive
	{ssh_cm, ConnectionRef, {data, ChannelId, _, Response}} ->
	    {PeerNameSrv,SockNameSrv} = binary_to_term(Response),
	    {HostPeerSrv,PortPeerSrv} = PeerNameSrv,
	    {HostSockSrv,PortSockSrv} = SockNameSrv,
	    ct:pal("Server: ~p ~p", [PeerNameSrv, SockNameSrv]),
	    host_equal(HostPeerSrv, HostSockClient),
	    PortPeerSrv = PortSockClient,
	    host_equal(HostSockSrv, HostPeerClient),
	    PortSockSrv = PortPeerClient,
	    host_equal(HostSockSrv, Host),
	    PortSockSrv = Port
    after 10000 ->
	    throw(timeout)
    end.

host_equal(H1, H2) ->
    not ordsets:is_disjoint(ips(H1), ips(H2)).

ips(IP) when is_tuple(IP) -> ordsets:from_list([IP]);
ips(Name) when is_list(Name) ->
    {ok,#hostent{h_addr_list=IPs4}} = inet:gethostbyname(Name,inet),
    {ok,#hostent{h_addr_list=IPs6}} = inet:gethostbyname(Name,inet6),
    ordsets:from_list(IPs4++IPs6).

%%--------------------------------------------------------------------

close() ->
    [{doc, "Client receives close when server closes"}].
close(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),
    
    {Server, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    Client =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
					  {user_interaction, false}]),
    {ok, ChannelId} = ssh_connection:session_channel(Client, infinity),
    
    ssh:stop_daemon(Server),
    receive 
	{ssh_cm, Client,{closed, ChannelId}} ->  
	    ok
    after 5000 ->
	    ct:fail(timeout)
    end.

%%--------------------------------------------------------------------
double_close() ->
    [{doc, "Simulate that we try to close an already closed connection"}].
double_close(Config) when is_list(Config) ->
    SystemDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config), 
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),

    {_Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir},
					     {user_passwords, [{"vego", "morot"}]},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    {ok, CM} = ssh:connect(Host, Port, [{silently_accept_hosts, true},
					   {user_dir, UserDir},
					    {user, "vego"},
					    {password, "morot"},
					    {user_interaction, false}]),
    
    exit(CM, {shutdown, normal}),
    ok = ssh:close(CM).

%%--------------------------------------------------------------------
ssh_connect_timeout() ->
    [{doc, "Test connect_timeout option in ssh:connect/4"}].
ssh_connect_timeout(_Config) ->
    ConnTimeout = 2000,
    {error,{faked_transport,connect,TimeoutToTransport}} = 
	ssh:connect("localhost", 12345, 
		    [{transport,{tcp,?MODULE,tcp_closed}},
		     {connect_timeout,ConnTimeout}],
		    1000),
    case TimeoutToTransport of
	ConnTimeout -> ok;
	Other -> 
	    ct:log("connect_timeout is ~p but transport received ~p",[ConnTimeout,Other]),
	    {fail,"ssh:connect/4 wrong connect_timeout received in transport"}
    end.
    
%% Help for the test above
connect(_Host, _Port, _Opts, Timeout) ->
    {error, {faked_transport,connect,Timeout}}.


%%--------------------------------------------------------------------
ssh_connect_arg4_timeout() ->
    [{doc, "Test fourth argument in ssh:connect/4"}].
ssh_connect_arg4_timeout(_Config) ->
    Timeout = 1000,
    Parent = self(),
    %% start the server
    Server = spawn(fun() ->
			   {ok,Sl} = gen_tcp:listen(0,[]),
			   {ok,{_,Port}} = inet:sockname(Sl),
			   Parent ! {port,self(),Port},
			   Rsa = gen_tcp:accept(Sl),
			   ct:log("Server gen_tcp:accept got ~p",[Rsa]),
			   receive after 2*Timeout -> ok end %% let client timeout first
		   end),

    %% Get listening port
    Port = receive
	       {port,Server,ServerPort} -> ServerPort
	   end,

    %% try to connect with a timeout, but "supervise" it
    Client = spawn(fun() ->
			   T0 = now(),
			   Rc = ssh:connect("localhost",Port,[],Timeout),
			   ct:log("Client ssh:connect got ~p",[Rc]),
			   Parent ! {done,self(),Rc,T0}
		   end),

    %% Wait for client reaction on the connection try:
    receive
	{done, Client, {error,_E}, T0} ->
	    Msp = ms_passed(T0, now()),
	    exit(Server,hasta_la_vista___baby),
	    Low = 0.9*Timeout,
	    High =  1.1*Timeout,
	    ct:log("Timeout limits: ~p--~p, timeout was ~p, expected ~p",[Low,High,Msp,Timeout]),
	    if
		Low<Msp, Msp<High -> ok;
		true -> {fail, "timeout not within limits"}
	    end;
	{done, Client, {ok,_Ref}, _T0} ->
	    {fail,"ssh-connected ???"}
    after
	5000 ->
	    exit(Server,hasta_la_vista___baby),
	    exit(Client,hasta_la_vista___baby),
	    {fail, "Didn't timeout"}
    end.


%% Help function
%% N2-N1
ms_passed(N1={_,_,M1}, N2={_,_,M2}) ->
    {0,{0,Min,Sec}} = calendar:time_difference(calendar:now_to_local_time(N1),
					       calendar:now_to_local_time(N2)),
    1000 * (Min*60 + Sec + (M2-M1)/1000000).

%%--------------------------------------------------------------------
ssh_connect_negtimeout_parallel(Config) -> ssh_connect_negtimeout(Config,true).
ssh_connect_negtimeout_sequential(Config) -> ssh_connect_negtimeout(Config,false).
    
ssh_connect_negtimeout(Config, Parallel) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),
    NegTimeOut = 2000,				% ms
    ct:log("Parallel: ~p",[Parallel]),

    {_Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},{user_dir, UserDir},
					      {parallel_login, Parallel},
					      {negotiation_timeout, NegTimeOut},
					      {failfun, fun ssh_test_lib:failfun/2}]),
    
    {ok,Socket} = gen_tcp:connect(Host, Port, []),
    ct:pal("And now sleeping 1.2*NegTimeOut (~p ms)...", [round(1.2 * NegTimeOut)]),
    receive after round(1.2 * NegTimeOut) -> ok end,
    
    case inet:sockname(Socket) of
	{ok,_} -> ct:fail("Socket not closed");
	{error,_} -> ok
    end.

%%--------------------------------------------------------------------
ssh_connect_nonegtimeout_connected_parallel() ->
    [{doc, "Test that ssh connection does not timeout if the connection is established (parallel)"}].
ssh_connect_nonegtimeout_connected_parallel(Config) ->
    ssh_connect_nonegtimeout_connected(Config, true).

ssh_connect_nonegtimeout_connected_sequential() ->
    [{doc, "Test that ssh connection does not timeout if the connection is established (non-parallel)"}].
ssh_connect_nonegtimeout_connected_sequential(Config) ->
    ssh_connect_nonegtimeout_connected(Config, false).


ssh_connect_nonegtimeout_connected(Config, Parallel) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),
    NegTimeOut = 20000,				% ms
    ct:log("Parallel: ~p",[Parallel]),
   
    {_Pid, _Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},{user_dir, UserDir},
					       {parallel_login, Parallel},
					       {negotiation_timeout, NegTimeOut},
					       {failfun, fun ssh_test_lib:failfun/2}]),
    ct:pal("~p Listen ~p:~p",[_Pid,_Host,Port]),
    ct:sleep(500),

    IO = ssh_test_lib:start_io_server(),
    Shell = ssh_test_lib:start_shell(Port, IO, UserDir),
    receive
	Error = {'EXIT', _, _} ->
	    ct:pal("~p",[Error]),
	    ct:fail(no_ssh_connection);  
	ErlShellStart ->
	    ct:pal("---Erlang shell start: ~p~n", [ErlShellStart]),
	    one_shell_op(IO, NegTimeOut),
	    one_shell_op(IO, NegTimeOut),
	    ct:pal("And now sleeping 1.2*NegTimeOut (~p ms)...", [round(1.2 * NegTimeOut)]),
	    receive after round(1.2 * NegTimeOut) -> ok end,
	    one_shell_op(IO, NegTimeOut)
    end,
    exit(Shell, kill).


one_shell_op(IO, TimeOut) ->
    ct:pal("One shell op: Waiting for prompter"),
    receive
	ErlPrompt0 -> ct:log("Erlang prompt: ~p~n", [ErlPrompt0])
	after TimeOut -> ct:fail("Timeout waiting for promter")
    end,

    IO ! {input, self(), "2*3*7.\r\n"},
    receive
	Echo0 -> ct:log("Echo: ~p ~n", [Echo0])
	after TimeOut -> ct:fail("Timeout waiting for echo")
    end,

    receive
	?NEWLINE -> ct:log("NEWLINE received", [])
    after TimeOut -> 
	    receive Any1 -> ct:log("Bad NEWLINE: ~p",[Any1])
	    after 0 -> ct:fail("Timeout waiting for NEWLINE")
	    end
    end,

    receive
	Result0 -> ct:log("Result: ~p~n", [Result0])
	after TimeOut ->  ct:fail("Timeout waiting for result")
    end.

%%--------------------------------------------------------------------

openssh_zlib_basic_test() ->
    [{doc, "Test basic connection with openssh_zlib"}].
openssh_zlib_basic_test(Config) ->
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
					  {user_interaction, false},
					  {compression, openssh_zlib}]),
    ok = ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------

max_sessions_ssh_connect_parallel(Config) -> 
    max_sessions(Config, true, connect_fun(ssh__connect,Config)).
max_sessions_ssh_connect_sequential(Config) -> 
    max_sessions(Config, false, connect_fun(ssh__connect,Config)).

max_sessions_sftp_start_channel_parallel(Config) -> 
    max_sessions(Config, true, connect_fun(ssh_sftp__start_channel, Config)).
max_sessions_sftp_start_channel_sequential(Config) -> 
    max_sessions(Config, false, connect_fun(ssh_sftp__start_channel, Config)).


%%%---- helpers:
connect_fun(ssh__connect, Config) ->
    fun(Host,Port) ->
	    ssh_test_lib:connect(Host, Port, 
				 [{silently_accept_hosts, true},
				  {user_dir, ?config(priv_dir,Config)},
				  {user_interaction, false},
				  {user, "carni"},
				  {password, "meat"}
				 ])
	    %% ssh_test_lib returns R when ssh:connect returns {ok,R}
    end;
connect_fun(ssh_sftp__start_channel, _Config) ->
    fun(Host,Port) ->
	    {ok,_Pid,ConnRef} =
		ssh_sftp:start_channel(Host, Port, 
				       [{silently_accept_hosts, true},
					{user, "carni"},
					{password, "meat"}
				       ]),
	    ConnRef
    end.


max_sessions(Config, ParallelLogin, Connect0) when is_function(Connect0,2) ->
    Connect = fun(Host,Port) ->
		      R = Connect0(Host,Port),
		      ct:pal("Connect(~p,~p) -> ~p",[Host,Port,R]),
		      R
	      end,
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),
    MaxSessions = 5,
    {Pid, Host, Port} = ssh_test_lib:daemon([
					     {system_dir, SystemDir},
					     {user_dir, UserDir},
					     {user_passwords, [{"carni", "meat"}]},
					     {parallel_login, ParallelLogin},
					     {max_sessions, MaxSessions}
					    ]),
    ct:pal("~p Listen ~p:~p for max ~p sessions",[Pid,Host,Port,MaxSessions]),
    try [Connect(Host,Port) || _ <- lists:seq(1,MaxSessions)]
    of
	Connections ->
	    %% Step 1 ok: could set up max_sessions connections
	    ct:log("Connections up: ~p",[Connections]),
	    [_|_] = Connections,

	    %% Now try one more than alowed:
	    ct:pal("Info Report might come here...",[]),
	    try Connect(Host,Port)
	    of
		_ConnectionRef1 ->
		    ssh:stop_daemon(Pid),
		    {fail,"Too many connections accepted"}
	    catch
		error:{badmatch,{error,"Connection closed"}} ->
		    %% Step 2 ok: could not set up max_sessions+1 connections
		    %% This is expected
		    %% Now stop one connection and try to open one more
		    ok = ssh:close(hd(Connections)),
		    try Connect(Host,Port)
		    of
			_ConnectionRef1 ->
			    %% Step 3 ok: could set up one more connection after killing one
			    %% Thats good.
			    ssh:stop_daemon(Pid),
			    ok
		    catch
			error:{badmatch,{error,"Connection closed"}} ->
			    %% Bad indeed. Could not set up one more connection even after killing
			    %% one existing. Very bad.
			    ssh:stop_daemon(Pid),
			    {fail,"Does not decrease # active sessions"}
		    end
	    end
    catch
	error:{badmatch,{error,"Connection closed"}} ->
	    ssh:stop_daemon(Pid),
	    {fail,"Too few connections accepted"}
    end.

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
  
%% Due to timing the error message may or may not be delivered to
%% the "tcp-application" before the socket closed message is recived
check_error("Internal error") ->
    ok;
check_error("Connection closed") ->
    ok;
check_error(Error) ->
    ct:fail(Error).

basic_test(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    
    {Pid, Host, Port} = ssh_test_lib:daemon(ServerOpts),
    {ok, CM} = ssh:connect(Host, Port, ClientOpts),
    ok = ssh:close(CM),
    ssh:stop_daemon(Pid).

do_shell(IO, Shell) ->
    receive
	ErlPrompt0 ->
	    ct:pal("Erlang prompt: ~p~n", [ErlPrompt0])
    end,
    IO ! {input, self(), "1+1.\r\n"},
     receive
	Echo0 ->
	     ct:pal("Echo: ~p ~n", [Echo0])
    end,
    receive
	?NEWLINE ->
	    ok
    end,
    receive
	Result0 = <<"2">> ->
	    ct:pal("Result: ~p~n", [Result0])
    end,
    receive
	?NEWLINE ->
	    ok
    end,
    receive
	ErlPrompt1 ->
	    ct:pal("Erlang prompt: ~p~n", [ErlPrompt1])
    end,
    exit(Shell, kill).
    %%Does not seem to work in the testserver!
    %% 	IO ! {input, self(), "q().\r\n"},
    %% receive
    %%  	?NEWLINE ->
    %%  	    ok
    %% end,
    %% receive
    %%  	Echo1 ->
    %% 	    ct:pal("Echo: ~p ~n", [Echo1])
    %% end,
    %% receive
    %% 	?NEWLINE ->
    %%  	    ok
    %% end,
    %% receive
    %%  	Result1 ->
    %%  	    ct:pal("Result: ~p~n", [Result1])
    %%      end,
    %% receive
    %% 	{'EXIT', Shell, killed} ->
    %% 	    ok
    %% end.
