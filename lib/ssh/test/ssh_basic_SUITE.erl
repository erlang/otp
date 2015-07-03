%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2015. All Rights Reserved.
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

-module(ssh_basic_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/file.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(NEWLINE, <<"\r\n">>).

-define(REKEY_DATA_TMO, 65000).
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
     connectfun_disconnectfun_server,
     connectfun_disconnectfun_client,
     {group, renegotiate},
     daemon_already_started,
     server_password_option,
     server_userpassword_option,
     {group, dir_options},
     double_close,
     ssh_connect_timeout,
     ssh_connect_arg4_timeout,
     packet_size_zero,
     ssh_daemon_minimal_remote_max_packet_size_option,
     ssh_msg_debug_fun_option_client,
     ssh_msg_debug_fun_option_server,
     disconnectfun_option_server,
     disconnectfun_option_client,
     unexpectedfun_option_server,
     unexpectedfun_option_client,
     preferred_algorithms,
     id_string_no_opt_client,
     id_string_own_string_client,
     id_string_random_client,
     id_string_no_opt_server,
     id_string_own_string_server,
     id_string_random_server,
     {group, hardening_tests},
     ssh_info_print
    ].

groups() ->
    [{dsa_key, [], basic_tests()},
     {rsa_key, [], basic_tests()},
     {dsa_pass_key, [], [pass_phrase]},
     {rsa_pass_key, [], [pass_phrase]},
     {internal_error, [], [internal_error]},
     {renegotiate, [], [rekey, rekey_limit, renegotiate1, renegotiate2]},
     {hardening_tests, [], [ssh_connect_nonegtimeout_connected_parallel,
			    ssh_connect_nonegtimeout_connected_sequential,
			    ssh_connect_negtimeout_parallel,
			    ssh_connect_negtimeout_sequential,
			    max_sessions_ssh_connect_parallel,
			    max_sessions_ssh_connect_sequential,
			    max_sessions_sftp_start_channel_parallel,
			    max_sessions_sftp_start_channel_sequential
			   ]},
     {dir_options, [], [user_dir_option,
			system_dir_option]}
    ].


basic_tests() ->
    [send, close, peername_sockname,
     exec, exec_compressed, 
     shell, shell_no_unicode, shell_unicode_string,
     cli, known_hosts, 
     idle_time, openssh_zlib_basic_test, misc_ssh_options, inet_option].


%%--------------------------------------------------------------------
init_per_suite(Config) ->
    catch crypto:stop(),
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
init_per_group(dir_options, Config) ->
    PrivDir = ?config(priv_dir, Config),
    %% Make unreadable dir:
    Dir_unreadable = filename:join(PrivDir, "unread"),
    ok = file:make_dir(Dir_unreadable),
    {ok,F1} = file:read_file_info(Dir_unreadable),
    ok = file:write_file_info(Dir_unreadable, 
			      F1#file_info{mode = F1#file_info.mode band (bnot 8#00444)}),
    %% Make readable file:
    File_readable = filename:join(PrivDir, "file"),
    ok = file:write_file(File_readable, <<>>),

    %% Check:
    case {file:read_file_info(Dir_unreadable), 
	  file:read_file_info(File_readable)} of
	{{ok, Id=#file_info{type=directory, access=Md}},
	 {ok, If=#file_info{type=regular,   access=Mf}}} ->
	    AccessOK =
		case {Md,                Mf} of
		    {read,               _} -> false;
		    {read_write,         _} -> false;
		    {_,               read} -> true;
		    {_,         read_write} -> true;
		    _ -> false
		end,

	    case AccessOK of
		true ->
		    %% Save:
		    [{unreadable_dir, Dir_unreadable},
		     {readable_file, File_readable} 
		     | Config];
		false ->
		    ct:log("File#file_info : ~p~n"
			   "Dir#file_info  : ~p",[If,Id]),
		    {skip, "File or dir mode settings failed"}
	    end;

	NotDirFile ->
	    ct:log("{Dir,File} -> ~p",[NotDirFile]),
	    {skip, "File/Dir creation failed"}
    end;
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
init_per_testcase(TC, Config) when TC==shell_no_unicode ; 
				   TC==shell_unicode_string ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = ?config(priv_dir, Config),
    SysDir =  ?config(data_dir, Config),
    ssh:start(),
    Sftpd = {_Pid, _Host, Port} =       
	ssh_test_lib:daemon([{system_dir, SysDir},
			     {user_dir, PrivDir},
			     {user_passwords, [{"foo", "bar"}]}]),
    ct:sleep(500),
    IO = ssh_test_lib:start_io_server(),
    Shell = ssh_test_lib:start_shell(Port, IO, UserDir,
				     [{silently_accept_hosts, true},
				      {user,"foo"},{password,"bar"}]),
    ct:pal("IO=~p, Shell=~p, self()=~p",[IO,Shell,self()]),
    ct:pal("file:native_name_encoding() = ~p,~nio:getopts() = ~p",
	   [file:native_name_encoding(),io:getopts()]),
    wait_for_erlang_first_line([{io,IO}, {shell,Shell}, {sftpd, Sftpd}  | Config]);
init_per_testcase(_TestCase, Config) ->
    ssh:start(),
    Config.

end_per_testcase(TestCase, Config) when TestCase == server_password_option;
					TestCase == server_userpassword_option ->
    UserDir = filename:join(?config(priv_dir, Config), nopubkey),
    ssh_test_lib:del_dirs(UserDir),
    end_per_testcase(Config);
end_per_testcase(TC, Config) when TC==shell_no_unicode ; 
				  TC==shell_unicode_string ->
    case ?config(sftpd, Config) of
	{Pid, _, _} ->
	    ssh:stop_daemon(Pid),
	    ssh:stop();
	_ ->
	    ssh:stop()
    end;
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
					     {preferred_algorithms,[{compression, [zlib]}]},
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
    SystemDir = ?config(data_dir, Config),
    UserDir = ?config(priv_dir, Config),

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
    					     {user_dir, UserDir},
					     {failfun, fun ssh_test_lib:failfun/2},
    					     {user_passwords,
    					      [{"simon", "says"}]},
					     {rekey_limit, 0}]),

    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
					  {user, "simon"},
					  {password, "says"},
					  {user_interaction, false},
					  {rekey_limit, 0}]),
    receive
    after ?REKEY_DATA_TMO ->
	    %%By this time rekeying would have been done
	    ssh:close(ConnectionRef),
	    ssh:stop_daemon(Pid)
    end.
%%--------------------------------------------------------------------
rekey_limit() ->
    [{doc, "Test rekeying by data volume"}].
rekey_limit(Config) ->
    SystemDir = ?config(data_dir, Config),
    UserDir = ?config(priv_dir, Config),
    DataFile = filename:join(UserDir, "rekey.data"),

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
    					     {user_dir, UserDir},
    					     {user_passwords,
    					      [{"simon", "says"}]}]),
    {ok, SftpPid, ConnectionRef} =
    	ssh_sftp:start_channel(Host, Port, [{system_dir, SystemDir},
    					    {user_dir, UserDir},
    					    {user, "simon"},
    					    {password, "says"},
    					    {rekey_limit, 2500},
    					    {user_interaction, false},
    					    {silently_accept_hosts, true}]),

    Kex1 = get_kex_init(ConnectionRef),

    timer:sleep(?REKEY_DATA_TMO),
    Kex1 = get_kex_init(ConnectionRef),

    Data = lists:duplicate(9000,1),
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
renegotiate1() ->
    [{doc, "Test rekeying with simulataneous send request"}].
renegotiate1(Config) ->
    SystemDir = ?config(data_dir, Config),
    UserDir = ?config(priv_dir, Config),
    DataFile = filename:join(UserDir, "renegotiate1.data"),

    {Pid, Host, DPort} = ssh_test_lib:daemon([{system_dir, SystemDir},
					       {user_dir, UserDir},
					       {user_passwords,
						[{"simon", "says"}]}]),
    RPort = ssh_test_lib:inet_port(),

    {ok,RelayPid} = ssh_relay:start_link({0,0,0,0}, RPort, Host, DPort),

    {ok, SftpPid, ConnectionRef} =
	ssh_sftp:start_channel(Host, RPort, [{system_dir, SystemDir},
					     {user_dir, UserDir},
					     {user, "simon"},
					     {password, "says"},
					     {user_interaction, false},
					     {silently_accept_hosts, true}]),

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
renegotiate2() ->
    [{doc, "Test rekeying with inflight messages from peer"}].
renegotiate2(Config) ->
    SystemDir = ?config(data_dir, Config),
    UserDir = ?config(priv_dir, Config),
    DataFile = filename:join(UserDir, "renegotiate1.data"),

    {Pid, Host, DPort} = ssh_test_lib:daemon([{system_dir, SystemDir},
					       {user_dir, UserDir},
					       {user_passwords,
						[{"simon", "says"}]}]),
    RPort = ssh_test_lib:inet_port(),

    {ok,RelayPid} = ssh_relay:start_link({0,0,0,0}, RPort, Host, DPort),

    {ok, SftpPid, ConnectionRef} =
	ssh_sftp:start_channel(Host, RPort, [{system_dir, SystemDir},
					     {user_dir, UserDir},
					     {user, "simon"},
					     {password, "says"},
					     {user_interaction, false},
					     {silently_accept_hosts, true}]),

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
system_dir_option(Config) ->
    DirUnread = proplists:get_value(unreadable_dir,Config),
    FileRead = proplists:get_value(readable_file,Config),

    case ssh_test_lib:daemon([{system_dir, DirUnread}]) of
	{error,{eoptions,{{system_dir,DirUnread},eacces}}} ->
	    ok;
	{Pid1,_Host1,Port1} when is_pid(Pid1),is_integer(Port1) ->
	    ssh:stop_daemon(Pid1),
	    ct:fail("Didn't detect that dir is unreadable", [])
	end,
    
    case ssh_test_lib:daemon([{system_dir, FileRead}]) of
	{error,{eoptions,{{system_dir,FileRead},enotdir}}} ->
	    ok;
	{Pid2,_Host2,Port2} when is_pid(Pid2),is_integer(Port2) ->
	    ssh:stop_daemon(Pid2),
	    ct:fail("Didn't detect that option is a plain file", [])
    end.


user_dir_option(Config) ->
    DirUnread = proplists:get_value(unreadable_dir,Config),
    FileRead = proplists:get_value(readable_file,Config),
    %% Any port will do (beware, implementation knowledge!):
    Port = 65535,

    case ssh:connect("localhost", Port, [{user_dir, DirUnread}]) of
	{error,{eoptions,{{user_dir,DirUnread},eacces}}} ->
	    ok;
	{error,econnrefused} ->
	    ct:fail("Didn't detect that dir is unreadable", [])
    end,

    case ssh:connect("localhost", Port, [{user_dir, FileRead}]) of
	{error,{eoptions,{{user_dir,FileRead},enotdir}}} ->
	    ok;
	{error,econnrefused} ->
	    ct:fail("Didn't detect that option is a plain file", [])
    end.

%%--------------------------------------------------------------------
ssh_msg_debug_fun_option_client() ->
    [{doc, "validate client that uses the 'ssh_msg_debug_fun' option"}].
ssh_msg_debug_fun_option_client(Config) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    Parent = self(),
    DbgFun = fun(ConnRef,Displ,Msg,Lang) -> Parent ! {msg_dbg,{ConnRef,Displ,Msg,Lang}} end,
		
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_dir, UserDir},
					  {user_interaction, false},
					  {ssh_msg_debug_fun,DbgFun}]),
    %% Beware, implementation knowledge:
    gen_fsm:send_all_state_event(ConnectionRef,{ssh_msg_debug,false,<<"Hello">>,<<>>}),
    receive
	{msg_dbg,X={ConnectionRef,false,<<"Hello">>,<<>>}} ->
	    ct:log("Got expected dbg msg ~p",[X]),
	    ssh:stop_daemon(Pid);
	{msg_dbg,X={_,false,<<"Hello">>,<<>>}} ->
	    ct:log("Got dbg msg but bad ConnectionRef (~p expected) ~p",[ConnectionRef,X]),
	    ssh:stop_daemon(Pid),
	    {fail, "Bad ConnectionRef received"};
	{msg_dbg,X} ->
	    ct:log("Got bad dbg msg ~p",[X]),
	    ssh:stop_daemon(Pid),
	    {fail,"Bad msg received"}
    after 1000 ->
	    ssh:stop_daemon(Pid),
	    {fail,timeout}
    end.

%%--------------------------------------------------------------------
connectfun_disconnectfun_server(Config) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),

    Parent = self(),
    Ref = make_ref(),
    ConnFun = fun(_,_,_) -> Parent ! {connect,Ref} end,
    DiscFun = fun(R) -> Parent ! {disconnect,Ref,R} end,

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {failfun, fun ssh_test_lib:failfun/2},
					     {disconnectfun, DiscFun},
					     {connectfun, ConnFun}]),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_dir, UserDir},
					  {user_interaction, false}]),
    receive
	{connect,Ref} ->
	    ssh:close(ConnectionRef),
	    receive
		{disconnect,Ref,R} ->
		    ct:log("Disconnect result: ~p",[R]),
		    ssh:stop_daemon(Pid)
	    after 2000 ->
		    {fail, "No disconnectfun action"}
	    end
    after 2000 ->
	    {fail, "No connectfun action"}
    end.

%%--------------------------------------------------------------------
connectfun_disconnectfun_client(Config) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),

    Parent = self(),
    Ref = make_ref(),
    DiscFun = fun(R) -> Parent ! {disconnect,Ref,R} end,

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    _ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_dir, UserDir},
					  {disconnectfun, DiscFun},
					  {user_interaction, false}]),
    ssh:stop_daemon(Pid),
    receive
	{disconnect,Ref,R} ->
	    ct:log("Disconnect result: ~p",[R])
    after 2000 ->
	    {fail, "No disconnectfun action"}
    end.

%%--------------------------------------------------------------------
ssh_msg_debug_fun_option_server() ->
    [{doc, "validate client that uses the 'ssh_msg_debug_fun' option"}].
ssh_msg_debug_fun_option_server(Config) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),

    Parent = self(),
    DbgFun = fun(ConnRef,Displ,Msg,Lang) -> Parent ! {msg_dbg,{ConnRef,Displ,Msg,Lang}} end,
    ConnFun = fun(_,_,_) -> Parent ! {connection_pid,self()} end,

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {failfun, fun ssh_test_lib:failfun/2},
					     {connectfun, ConnFun},
					     {ssh_msg_debug_fun, DbgFun}]),
    _ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_dir, UserDir},
					  {user_interaction, false}]),
    receive
	{connection_pid,Server} ->
	    %% Beware, implementation knowledge:
	    gen_fsm:send_all_state_event(Server,{ssh_msg_debug,false,<<"Hello">>,<<>>}),
	    receive
		{msg_dbg,X={_,false,<<"Hello">>,<<>>}} ->
		    ct:log("Got expected dbg msg ~p",[X]),
		    ssh:stop_daemon(Pid);
		{msg_dbg,X} ->
		    ct:log("Got bad dbg msg ~p",[X]),
		    ssh:stop_daemon(Pid),
		    {fail,"Bad msg received"}
	    after 3000 ->
		    ssh:stop_daemon(Pid),
		    {fail,timeout2}
	    end
    after 3000 ->
	    ssh:stop_daemon(Pid),
	    {fail,timeout1}
    end.

%%--------------------------------------------------------------------
disconnectfun_option_server(Config) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),

    Parent = self(),
    DisConnFun = fun(Reason) -> Parent ! {disconnect,Reason} end,

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {failfun, fun ssh_test_lib:failfun/2},
					     {disconnectfun, DisConnFun}]),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_dir, UserDir},
					  {user_interaction, false}]),
    ssh:close(ConnectionRef),
    receive
	{disconnect,Reason} ->
	    ct:log("Server detected disconnect: ~p",[Reason]),
	    ssh:stop_daemon(Pid),
	    ok
    after 3000 ->
	    receive
		X -> ct:log("received ~p",[X])
	    after 0 -> ok
	    end,
	    {fail,"Timeout waiting for disconnect"}
    end.

%%--------------------------------------------------------------------
disconnectfun_option_client(Config) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),

    Parent = self(),
    DisConnFun = fun(Reason) -> Parent ! {disconnect,Reason} end,

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    _ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_dir, UserDir},
					  {user_interaction, false},
					  {disconnectfun, DisConnFun}]),
    ssh:stop_daemon(Pid),
    receive
	{disconnect,Reason} ->
	    ct:log("Client detected disconnect: ~p",[Reason]),
	    ok
    after 3000 ->
	    receive
		X -> ct:log("received ~p",[X])
	    after 0 -> ok
	    end,
	    {fail,"Timeout waiting for disconnect"}
    end.

%%--------------------------------------------------------------------
unexpectedfun_option_server(Config) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),

    Parent = self(),
    ConnFun = fun(_,_,_) -> Parent ! {connection_pid,self()} end,
    UnexpFun = fun(Msg,Peer) ->
		       Parent ! {unexpected,Msg,Peer,self()},
		       skip
	       end,

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {failfun, fun ssh_test_lib:failfun/2},
					     {connectfun, ConnFun},
					     {unexpectedfun, UnexpFun}]),
    _ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_dir, UserDir},
					  {user_interaction, false}]),
    receive
	{connection_pid,Server} ->
	    %% Beware, implementation knowledge:
	    Server ! unexpected_message,
	    receive
		{unexpected, unexpected_message, {{_,_,_,_},_}, _} -> ok;
		{unexpected, unexpected_message, Peer, _} -> ct:fail("Bad peer ~p",[Peer]);
		M = {unexpected, _, _, _} -> ct:fail("Bad msg ~p",[M])
	    after 3000 ->
		    ssh:stop_daemon(Pid),
		    {fail,timeout2}
	    end
    after 3000 ->
	    ssh:stop_daemon(Pid),
	    {fail,timeout1}
    end.

%%--------------------------------------------------------------------
unexpectedfun_option_client(Config) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),

    Parent = self(),
    UnexpFun = fun(Msg,Peer) -> 
		       Parent ! {unexpected,Msg,Peer,self()},
		       skip
	       end,

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_dir, UserDir},
					  {user_interaction, false},
					  {unexpectedfun, UnexpFun}]),
    %% Beware, implementation knowledge:
    ConnectionRef ! unexpected_message,

    receive
	{unexpected, unexpected_message, {{_,_,_,_},_}, ConnectionRef} ->
	    ok;
	{unexpected, unexpected_message, Peer, ConnectionRef} ->
	    ct:fail("Bad peer ~p",[Peer]);
	M = {unexpected, _, _, _} ->
	    ct:fail("Bad msg ~p",[M])
    after 3000 ->
	    ssh:stop_daemon(Pid),
	    {fail,timeout}
    end.

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
			   T0 = erlang:monotonic_time(),
			   Rc = ssh:connect("localhost",Port,[],Timeout),
			   ct:log("Client ssh:connect got ~p",[Rc]),
			   Parent ! {done,self(),Rc,T0}
		   end),

    %% Wait for client reaction on the connection try:
    receive
	{done, Client, {error,timeout}, T0} ->
	    Msp = ms_passed(T0),
	    exit(Server,hasta_la_vista___baby),
	    Low = 0.9*Timeout,
	    High =  1.1*Timeout,
	    ct:log("Timeout limits: ~.4f - ~.4f ms, timeout "
                   "was ~.4f ms, expected ~p ms",[Low,High,Msp,Timeout]),
	    if
		Low<Msp, Msp<High -> ok;
		true -> {fail, "timeout not within limits"}
	    end;

	{done, Client, {error,Other}, _T0} ->
	    ct:log("Error message \"~p\" from the client is unexpected.",[{error,Other}]),
	    {fail, "Unexpected error message"};

	{done, Client, {ok,_Ref}, _T0} ->
	    {fail,"ssh-connected ???"}
    after
	5000 ->
	    exit(Server,hasta_la_vista___baby),
	    exit(Client,hasta_la_vista___baby),
	    {fail, "Didn't timeout"}
    end.

%% Help function, elapsed milliseconds since T0
ms_passed(T0) ->
    %% OTP 18
    erlang:convert_time_unit(erlang:monotonic_time() - T0,
			     native,
			     micro_seconds) / 1000.

%%--------------------------------------------------------------------
packet_size_zero(Config) ->
    SystemDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config), 
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),

    {Server, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
						{user_dir, UserDir},
						{user_passwords, [{"vego", "morot"}]}]),
    Conn =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
					  {user_interaction, false},
					  {user, "vego"},
					  {password, "morot"}]),

    {ok,Chan} = ssh_connection:session_channel(Conn, 1000, _MaxPacketSize=0, 60000),
    ok = ssh_connection:shell(Conn, Chan),

    ssh:close(Conn),
    ssh:stop_daemon(Server),

    receive
	{ssh_cm,Conn,{data,Chan,_Type,_Msg1}} = M ->
	    ct:pal("Got ~p",[M]),
	    ct:fail(doesnt_obey_max_packet_size_0)
    after 5000 ->
	    ok
    end.    
    
%%--------------------------------------------------------------------
ssh_daemon_minimal_remote_max_packet_size_option(Config) ->
    SystemDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config), 
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    
    {Server, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
						{user_dir, UserDir},
						{user_passwords, [{"vego", "morot"}]},
						{failfun, fun ssh_test_lib:failfun/2},
						{minimal_remote_max_packet_size, 14}]),
    Conn =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
					  {user_interaction, false},
					  {user, "vego"},
					  {password, "morot"}]),

    %% Try the limits of the minimal_remote_max_packet_size:
    {ok, _ChannelId} = ssh_connection:session_channel(Conn, 100, 14, infinity),
    {open_error,_,"Maximum packet size below 14 not supported",_} = 
	ssh_connection:session_channel(Conn, 100, 13, infinity),

    ssh:close(Conn),
    ssh:stop_daemon(Server).
    
%%--------------------------------------------------------------------
%% This test try every algorithm by connecting to an Erlang server
preferred_algorithms(Config) ->
    SystemDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),

    {Server, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
						{user_dir, UserDir},
						{user_passwords, [{"vego", "morot"}]},
						{failfun, fun ssh_test_lib:failfun/2}]),
    Available = ssh:default_algorithms(),
    Tests = [[{Tag,[Alg]}] || {Tag, SubAlgs} <- Available,
			  is_atom(hd(SubAlgs)),
			  Alg <- SubAlgs]
	++  [[{Tag,[{T1,[A1]},{T2,[A2]}]}] || {Tag, [{T1,As1},{T2,As2}]} <- Available,
					      A1 <- As1,
					      A2 <- As2],
    ct:log("TESTS: ~p",[Tests]),
    [connect_exec_channel(Host,Port,PrefAlgs)  || PrefAlgs <- Tests],
    ssh:stop_daemon(Server).


connect_exec_channel(_Host, Port, Algs) ->
    ct:log("Try ~p",[Algs]),
    ConnectionRef = ssh_test_lib:connect(Port, [{silently_accept_hosts, true},
						{user_interaction, false},
						{user, "vego"},
						{password, "morot"},
						{preferred_algorithms,Algs}
					       ]),
    chan_exec(ConnectionRef, "2*21.", <<"42\n">>),
    ssh:close(ConnectionRef).

chan_exec(ConnectionRef, Cmnd, Expected) ->
    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId0,Cmnd, infinity),
    Data0 = {ssh_cm, ConnectionRef, {data, ChannelId0, 0, Expected}},
    case ssh_test_lib:receive_exec_result(Data0) of
	expected ->
	    ssh_test_lib:receive_exec_end(ConnectionRef, ChannelId0);
	{unexpected_msg,{ssh_cm, ConnectionRef, {exit_status, ChannelId0, 0}}
	 = ExitStatus0} ->
	    ct:pal("0: Collected data ~p", [ExitStatus0]),
	    ssh_test_lib:receive_exec_result(Data0,
					     ConnectionRef, ChannelId0);
	Other0 ->
	    ct:fail(Other0)
    end.

%%--------------------------------------------------------------------
id_string_no_opt_client(Config) ->
    {Server, _Host, Port} = fake_daemon(Config),
    {error,_} = ssh:connect("localhost", Port, [], 1000),
    receive
	{id,Server,"SSH-2.0-Erlang/"++Vsn} ->
	    true = expected_ssh_vsn(Vsn);
	{id,Server,Other} ->
	    ct:fail("Unexpected id: ~s.",[Other])
    after 5000 ->
	    {fail,timeout}
    end.

%%--------------------------------------------------------------------
id_string_own_string_client(Config) ->
    {Server, _Host, Port} = fake_daemon(Config),
    {error,_} = ssh:connect("localhost", Port, [{id_string,"Pelle"}], 1000),
    receive
	{id,Server,"SSH-2.0-Pelle\r\n"} ->
	    ok;
	{id,Server,Other} ->
	    ct:fail("Unexpected id: ~s.",[Other])
    after 5000 ->
	    {fail,timeout}
    end.

%%--------------------------------------------------------------------
id_string_random_client(Config) ->
    {Server, _Host, Port} = fake_daemon(Config),
    {error,_} = ssh:connect("localhost", Port, [{id_string,random}], 1000),
    receive
	{id,Server,Id="SSH-2.0-Erlang"++_} ->
	    ct:fail("Unexpected id: ~s.",[Id]);
	{id,Server,Rnd="SSH-2.0-"++_} ->
	    ct:log("Got correct ~s",[Rnd]);
	{id,Server,Id} ->
	    ct:fail("Unexpected id: ~s.",[Id])
    after 5000 ->
	    {fail,timeout}
    end.

%%--------------------------------------------------------------------
id_string_no_opt_server(Config) ->
    {_Server, Host, Port} = std_daemon(Config, []),
    {ok,S1}=gen_tcp:connect(Host,Port,[{active,false},{packet,line}]),
    {ok,"SSH-2.0-Erlang/"++Vsn} = gen_tcp:recv(S1, 0, 2000),
    true = expected_ssh_vsn(Vsn).

%%--------------------------------------------------------------------
id_string_own_string_server(Config) ->
    {_Server, Host, Port} = std_daemon(Config, [{id_string,"Olle"}]),
    {ok,S1}=gen_tcp:connect(Host,Port,[{active,false},{packet,line}]),
    {ok,"SSH-2.0-Olle\r\n"} = gen_tcp:recv(S1, 0, 2000).

%%--------------------------------------------------------------------
id_string_random_server(Config) ->
    {_Server, Host, Port} = std_daemon(Config, [{id_string,random}]),
    {ok,S1}=gen_tcp:connect(Host,Port,[{active,false},{packet,line}]),
    {ok,"SSH-2.0-"++Rnd} = gen_tcp:recv(S1, 0, 2000),
    case Rnd of
	"Erlang"++_ -> ct:log("Id=~p",[Rnd]),
		       {fail,got_default_id};
	"Olle\r\n" -> {fail,got_previous_tests_value};
	_ -> ct:log("Got ~s.",[Rnd])
    end.

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

    Factor = 2,
    ct:pal("And now sleeping ~p*NegTimeOut (~p ms)...", [Factor, round(Factor * NegTimeOut)]),
    ct:sleep(round(Factor * NegTimeOut)),
    
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

	    Factor = 2,
	    ct:pal("And now sleeping ~p*NegTimeOut (~p ms)...", [Factor, round(Factor * NegTimeOut)]),
	    ct:sleep(round(Factor * NegTimeOut)),
    
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
shell_no_unicode(Config) ->
    new_do_shell(?config(io,Config),
		 [new_prompt,
		  {type,"io:format(\"hej ~p~n\",[42])."},
		  {expect,"hej 42"}
		 ]).
	      
%%--------------------------------------------------------------------
shell_unicode_string(Config) ->
    new_do_shell(?config(io,Config),
		 [new_prompt,
		  {type,"io:format(\"こにちわ~ts~n\",[\"四二\"])."},
		  {expect,"こにちわ四二"},
		  {expect,"ok"}
		 ]).

%%--------------------------------------------------------------------
openssh_zlib_basic_test() ->
    [{doc, "Test basic connection with openssh_zlib"}].
openssh_zlib_basic_test(Config) ->
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir},
					     {preferred_algorithms,[{compression, ['zlib@openssh.com']}]},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
					  {user_interaction, false},
					  {preferred_algorithms,[{compression, ['zlib@openssh.com',
										none]}]}
					 ]),
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
		    receive after 250 -> ok end, % sleep so the supervisor has time to count down. Not nice...
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
ssh_info_print(Config) ->
    %% Just check that ssh_print:info() crashes
    PrivDir = ?config(priv_dir, Config),
    PrintFile = filename:join(PrivDir,info),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),

    Parent = self(),
    UnexpFun = fun(Msg,_Peer) ->
		       Parent ! {unexpected,Msg,self()},
		       skip
	       end,
    ConnFun = fun(_,_,_) -> Parent ! {connect,self()} end,

    {DaemonRef, Host, Port} = 
	ssh_test_lib:daemon([{system_dir, SysDir},
			     {user_dir, UserDir},
			     {password, "morot"},
			     {unexpectedfun, UnexpFun},
			     {connectfun, ConnFun},
			     {failfun, fun ssh_test_lib:failfun/2}]),
    ClientConnRef1 =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_dir, UserDir},
					  {unexpectedfun, UnexpFun},
					  {user_interaction, false}]),
    ClientConnRef2 =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_dir, UserDir},
					  {unexpectedfun, UnexpFun},
					  {user_interaction, false}]),
    receive
	{connect,DaemonConnRef} ->
	    ct:log("DaemonRef=~p, DaemonConnRef=~p, ClientConnRefs=~p",[DaemonRef, DaemonConnRef, 
									[ClientConnRef1,ClientConnRef2]
								       ])
    after 2000 ->
	    ok
    end,

    {ok,D} = file:open(PrintFile, write),
    ssh_info:print(D),
    ok = file:close(D),

    {ok,Bin} = file:read_file(PrintFile),
    ct:log("~s",[Bin]),

    receive
	{unexpected, Msg, Pid} ->
	    ct:log("~p got unexpected msg ~p",[Pid,Msg]),
	    ct:log("process_info(~p) = ~n~p",[Pid,process_info(Pid)]),
	    ok = ssh:close(ClientConnRef1),
	    ok = ssh:close(ClientConnRef2),
	    ok = ssh:stop_daemon(DaemonRef),
	    {fail,"unexpected msg"}
    after 1000 ->
	    ok = ssh:close(ClientConnRef1),
	    ok = ssh:close(ClientConnRef2),
	    ok = ssh:stop_daemon(DaemonRef)
    end.


%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
  
%% Due to timing the error message may or may not be delivered to
%% the "tcp-application" before the socket closed message is recived
check_error("Invalid state") ->
    ok;
check_error("Connection closed") ->
    ok;
check_error("Selection of key exchange algorithm failed") ->
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


%%--------------------------------------------------------------------
wait_for_erlang_first_line(Config) ->
    receive
	{'EXIT', _, _} ->
	    {fail,no_ssh_connection};
	<<"Eshell ",_/binary>> = _ErlShellStart ->
	    ct:pal("Erlang shell start: ~p~n", [_ErlShellStart]),
	    Config;
	Other ->
	    ct:pal("Unexpected answer from ssh server: ~p",[Other]),
	    {fail,unexpected_answer}
    after 10000 ->
	    ct:pal("No answer from ssh-server"),
	    {fail,timeout}
    end.



new_do_shell(IO, List) -> new_do_shell(IO, 0, List).

new_do_shell(IO, N, [new_prompt|More]) ->
    new_do_shell(IO, N+1, More);

new_do_shell(IO, N, Ops=[{Order,Arg}|More]) ->
    Pfx = prompt_prefix(),
    PfxSize = size(Pfx),
    receive
	_X = <<"\r\n">> ->
	    ct:pal("Skip newline ~p",[_X]),
	    new_do_shell(IO, N, Ops);
	
	<<Pfx:PfxSize/binary,P1,"> ">> when (P1-$0)==N -> 
	    new_do_shell_prompt(IO, N, Order, Arg, More);

	<<Pfx:PfxSize/binary,P1,P2,"> ">> when (P1-$0)*10 + (P2-$0) == N -> 
	    new_do_shell_prompt(IO, N, Order, Arg, More);

	<<Pfx:PfxSize/binary,P1,P2,P3,"> ">> when (P1-$0)*100 + (P2-$0)*10 + (P3-$0) == N -> 
	    new_do_shell_prompt(IO, N, Order, Arg, More);

	Err when element(1,Err)==error ->
	    ct:fail("new_do_shell error: ~p~n",[Err]);

	RecBin when Order==expect ; Order==expect_echo ->
	    ct:pal("received ~p",[RecBin]),
	    RecStr = string:strip(unicode:characters_to_list(RecBin)),
	    ExpStr = string:strip(Arg),
	    case lists:prefix(ExpStr, RecStr) of
		true when Order==expect ->
		    ct:pal("Matched ~ts",[RecStr]),
		    new_do_shell(IO, N, More);
		true when Order==expect_echo ->
		    ct:pal("Matched echo ~ts",[RecStr]),
		    new_do_shell(IO, N, More);
		false ->
		    ct:fail("*** Expected ~p, but got ~p",[string:strip(ExpStr),RecStr])
	    end
    after 30000 ->
	    ct:log("Meassage queue of ~p:~n~p",
		   [self(), erlang:process_info(self(), messages)]),
	    case Order of
		expect -> ct:fail("timeout, expected ~p",[string:strip(Arg)]);
		type ->  ct:fail("timeout, no prompt")
	    end
    end;

new_do_shell(_, _, []) ->
    ok.

prompt_prefix() ->
    case node() of
	nonode@nohost -> <<>>;
	Node -> list_to_binary(
		  lists:concat(["(",Node,")"]))
    end.
	    

new_do_shell_prompt(IO, N, type, Str, More) ->
    ct:pal("Matched prompt ~p to trigger sending of next line to server",[N]),
    IO ! {input, self(), Str++"\r\n"},
    ct:pal("Promt '~p> ', Sent ~ts",[N,Str++"\r\n"]),
    new_do_shell(IO, N, [{expect_echo,Str}|More]); % expect echo of the sent line
new_do_shell_prompt(IO, N, Op, Str, More) ->
    ct:pal("Matched prompt ~p",[N]),
    new_do_shell(IO, N, [{Op,Str}|More]).
  
%%--------------------------------------------------------------------


std_daemon(Config, ExtraOpts) ->
    SystemDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config), 
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    {_Server, _Host, _Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
						   {user_dir, UserDir},
						   {failfun, fun ssh_test_lib:failfun/2} | ExtraOpts]).

expected_ssh_vsn(Str) ->
    try
	{ok,L} = application:get_all_key(ssh),
	proplists:get_value(vsn,L,"")++"\r\n"
    of
	Str -> true;
	"\r\n" -> true;
	_ -> false
    catch
	_:_ -> true %% ssh not started so we dont't know
    end.
	    

fake_daemon(_Config) ->
    Parent = self(),
    %% start the server
    Server = spawn(fun() ->
			   {ok,Sl} = gen_tcp:listen(0,[{packet,line}]),
			   {ok,{Host,Port}} = inet:sockname(Sl),
			   ct:log("fake_daemon listening on ~p:~p~n",[Host,Port]),
			   Parent ! {sockname,self(),Host,Port},
			   Rsa = gen_tcp:accept(Sl),
			   ct:log("Server gen_tcp:accept got ~p",[Rsa]),
			   {ok,S} = Rsa,
			   receive
			       {tcp, S, Id} -> Parent ! {id,self(),Id}
			   end
		   end),
    %% Get listening host and port
    receive
	{sockname,Server,ServerHost,ServerPort} -> {Server, ServerHost, ServerPort}
    end.

%% get_kex_init - helper function to get key_exchange_init_msg
get_kex_init(Conn) ->
    %% First, validate the key exchange is complete (StateName == connected)
    {connected,S} = sys:get_state(Conn),
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
