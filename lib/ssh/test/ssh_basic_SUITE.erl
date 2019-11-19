%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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
-include("ssh_test_lib.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(NEWLINE, <<"\r\n">>).

-define(REKEY_DATA_TMO, 1 * 60000). % Should be multiples of 60000

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,40}}].

all() -> 
    [{group, all_tests},
     daemon_already_started
    ].

%%%-define(PARALLEL, ).
-define(PARALLEL, parallel).

groups() ->
    [{all_tests, [?PARALLEL], [{group, ssh_renegotiate_SUITE},
                              {group, ssh_basic_SUITE}
                             ]},
     {ssh_basic_SUITE, [], [app_test,
                            appup_test,
                            {group, dsa_key},
                            {group, rsa_key},
                            {group, ecdsa_sha2_nistp256_key},
                            {group, ecdsa_sha2_nistp384_key},
                            {group, ecdsa_sha2_nistp521_key},
                            {group, ed25519_key},
                            {group, ed448_key},
                            {group, dsa_pass_key},
                            {group, rsa_pass_key},
                            {group, ecdsa_sha2_nistp256_pass_key},
                            {group, ecdsa_sha2_nistp384_pass_key},
                            {group, ecdsa_sha2_nistp521_pass_key},
                            {group, host_user_key_differs},
                            {group, key_cb},
                            {group, internal_error},
                            {group, rsa_host_key_is_actualy_ecdsa},
                            daemon_already_started,
                            double_close,
                            daemon_opt_fd,
                            multi_daemon_opt_fd,
                            packet_size,
                            ssh_info_print,
                            {group, login_bad_pwd_no_retry},
                            shell_exit_status
                           ]},

     {ssh_renegotiate_SUITE, [?PARALLEL], [rekey0,
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
                                          renegotiate2]},

     {dsa_key, [], [{group, basic}]},
     {rsa_key, [], [{group, basic}]},
     {ecdsa_sha2_nistp256_key, [], [{group, basic}]},
     {ecdsa_sha2_nistp384_key, [], [{group, basic}]},
     {ecdsa_sha2_nistp521_key, [], [{group, basic}]},
     {ed25519_key, [], [{group, basic}]},
     {ed448_key,   [], [{group, basic}]},
     {rsa_host_key_is_actualy_ecdsa, [], [fail_daemon_start]},
     {host_user_key_differs, [?PARALLEL], [exec_key_differs1,
                                          exec_key_differs2,
                                          exec_key_differs3,
                                          exec_key_differs_fail]},
     {dsa_pass_key, [], [pass_phrase]},
     {rsa_pass_key, [], [pass_phrase]},
     {ecdsa_sha2_nistp256_pass_key, [], [pass_phrase]},
     {ecdsa_sha2_nistp384_pass_key, [], [pass_phrase]},
     {ecdsa_sha2_nistp521_pass_key, [], [pass_phrase]},
     {key_cb, [?PARALLEL], [key_callback, key_callback_options]},
     {internal_error, [], [internal_error]},
     {login_bad_pwd_no_retry, [?PARALLEL], [login_bad_pwd_no_retry1,
                                           login_bad_pwd_no_retry2,
                                           login_bad_pwd_no_retry3,
                                           login_bad_pwd_no_retry4,
                                           login_bad_pwd_no_retry5
                                          ]},
     
     {basic, [], [{group,p_basic},
                  shell, shell_no_unicode, shell_unicode_string,
                  close, 
                  known_hosts
                 ]},
     {p_basic, [?PARALLEL], [send, peername_sockname,
                            exec, exec_compressed, 
                            exec_with_io_out, exec_with_io_in,
                            cli,
                            idle_time_client, idle_time_server, openssh_zlib_basic_test, 
                            misc_ssh_options, inet_option, inet6_option]}
    ].


        


%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ?CHECK_CRYPTO(begin
                      ssh:start(),
                      Config
                  end).

end_per_suite(_Config) ->
    ssh:stop().

%%--------------------------------------------------------------------
init_per_group(ssh_renegotiate_SUITE, Config) ->
    [{preferred_algorithms, ssh:default_algorithms()} | Config];
init_per_group(dsa_key, Config) ->
    case lists:member('ssh-dss',
		      ssh_transport:default_algorithms(public_key)) of
	true ->
            DataDir = proplists:get_value(data_dir, Config),
            PrivDir = proplists:get_value(priv_dir, Config),
            ssh_test_lib:setup_dsa(DataDir, PrivDir),
            Config;
	false ->
	    {skip, unsupported_pub_key}
    end;
init_per_group(rsa_key, Config) ->
    case lists:member('ssh-rsa',
		      ssh_transport:default_algorithms(public_key)) of
	true ->
            DataDir = proplists:get_value(data_dir, Config),
            PrivDir = proplists:get_value(priv_dir, Config),
            ssh_test_lib:setup_rsa(DataDir, PrivDir),
            Config;
	false ->
	    {skip, unsupported_pub_key}
    end;
init_per_group(rsa_host_key_is_actualy_ecdsa, Config) ->
    case 
        lists:member('ssh-rsa',
		      ssh_transport:default_algorithms(public_key)) and
        lists:member('ecdsa-sha2-nistp256',
                     ssh_transport:default_algorithms(public_key))
    of
	true ->
            DataDir = proplists:get_value(data_dir, Config),
            PrivDir = proplists:get_value(priv_dir, Config),
	    ssh_test_lib:setup_ecdsa("256", DataDir, PrivDir),
            %% The following sets up bad rsa keys:
            begin
                UserDir = PrivDir,
                System = filename:join(UserDir, "system"),
                file:copy(filename:join(DataDir, "id_rsa"), filename:join(UserDir, "id_rsa")),
                file:rename(filename:join(System, "ssh_host_ecdsa_key"), filename:join(System, "ssh_host_rsa_key")),
                file:rename(filename:join(System, "ssh_host_ecdsa_key.pub"), filename:join(System, "ssh_host_rsa_key.pub")),
                ssh_test_lib:setup_rsa_known_host(DataDir, UserDir),
                ssh_test_lib:setup_rsa_auth_keys(DataDir, UserDir)
            end,
            Config;
	false ->
	    {skip, unsupported_pub_key}
    end;
init_per_group(ecdsa_sha2_nistp256_key, Config) ->
    case lists:member('ecdsa-sha2-nistp256',
		      ssh_transport:default_algorithms(public_key)) of
	true ->
	    DataDir = proplists:get_value(data_dir, Config),
	    PrivDir = proplists:get_value(priv_dir, Config),
	    ssh_test_lib:setup_ecdsa("256", DataDir, PrivDir),
	    Config;
	false ->
	    {skip, unsupported_pub_key}
    end;
init_per_group(ecdsa_sha2_nistp384_key, Config) ->
    case lists:member('ecdsa-sha2-nistp384',
		      ssh_transport:default_algorithms(public_key)) of
	true ->
	    DataDir = proplists:get_value(data_dir, Config),
	    PrivDir = proplists:get_value(priv_dir, Config),
	    ssh_test_lib:setup_ecdsa("384", DataDir, PrivDir),
	    Config;
	false ->
	    {skip, unsupported_pub_key}
    end;
init_per_group(ecdsa_sha2_nistp521_key, Config) ->
    case lists:member('ecdsa-sha2-nistp521',
		      ssh_transport:default_algorithms(public_key)) of
	true ->
	    DataDir = proplists:get_value(data_dir, Config),
	    PrivDir = proplists:get_value(priv_dir, Config),
	    ssh_test_lib:setup_ecdsa("521", DataDir, PrivDir),
	    Config;
	false ->
	    {skip, unsupported_pub_key}
    end;
init_per_group(ed25519_key, Config) ->
    case lists:member('ssh-ed25519',
		      ssh_transport:default_algorithms(public_key)) of
	true ->
	    DataDir = proplists:get_value(data_dir, Config),
	    PrivDir = proplists:get_value(priv_dir, Config),
            ssh_test_lib:setup_eddsa(ed25519, DataDir, PrivDir),
	    Config;
	false ->
	    {skip, unsupported_pub_key}
    end;
init_per_group(ed448_key, Config) ->
    case lists:member('ssh-ed448',
		      ssh_transport:default_algorithms(public_key)) of
	true ->
	    DataDir = proplists:get_value(data_dir, Config),
	    PrivDir = proplists:get_value(priv_dir, Config),
            ssh_test_lib:setup_eddsa(ed448, DataDir, PrivDir),
	    Config;
	false ->
	    {skip, unsupported_pub_key}
    end;
init_per_group(rsa_pass_key, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    case lists:member('ssh-rsa',
		      ssh_transport:default_algorithms(public_key))
        andalso
        ssh_test_lib:setup_rsa_pass_phrase(DataDir, PrivDir, "Password")
    of
	true ->
            [{pass_phrase, {rsa_pass_phrase, "Password"}}| Config];
	false ->
	    {skip, unsupported_pub_key}
    end;
init_per_group(dsa_pass_key, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    case lists:member('ssh-dss',
		      ssh_transport:default_algorithms(public_key))
    andalso
        ssh_test_lib:setup_dsa_pass_phrase(DataDir, PrivDir, "Password")
    of
	true ->
            [{pass_phrase, {dsa_pass_phrase, "Password"}}| Config];
	false ->
	    {skip, unsupported_pub_key}
    end;
init_per_group(ecdsa_sha2_nistp256_pass_key, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    case lists:member('ecdsa-sha2-nistp256',
		      ssh_transport:default_algorithms(public_key))
        andalso
        ssh_test_lib:setup_ecdsa_pass_phrase("256", DataDir, PrivDir, "Password")
    of
	true ->
	    [{pass_phrase, {ecdsa_pass_phrase, "Password"}}| Config];
	false ->
	    {skip, unsupported_pub_key}
    end;
init_per_group(ecdsa_sha2_nistp384_pass_key, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    case lists:member('ecdsa-sha2-nistp384',
		      ssh_transport:default_algorithms(public_key))
        andalso
        ssh_test_lib:setup_ecdsa_pass_phrase("384", DataDir, PrivDir, "Password")
    of
	true ->
	    [{pass_phrase, {ecdsa_pass_phrase, "Password"}}| Config];
	false ->
	    {skip, unsupported_pub_key}
    end;
init_per_group(ecdsa_sha2_nistp521_pass_key, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    case lists:member('ecdsa-sha2-nistp521',
		      ssh_transport:default_algorithms(public_key))
        andalso
        ssh_test_lib:setup_ecdsa_pass_phrase("521", DataDir, PrivDir, "Password")
    of
	true ->
	    [{pass_phrase, {ecdsa_pass_phrase, "Password"}}| Config];
	false ->
	    {skip, unsupported_pub_key}
    end;
init_per_group(host_user_key_differs, Config) ->
    Data = proplists:get_value(data_dir, Config),
    Sys = filename:join(proplists:get_value(priv_dir, Config), system_rsa),
    SysUsr = filename:join(Sys, user),
    Usr = filename:join(proplists:get_value(priv_dir, Config), user_ecdsa_256),
    file:make_dir(Sys),
    file:make_dir(SysUsr),
    file:make_dir(Usr),
    file:copy(filename:join(Data, "ssh_host_rsa_key"),     filename:join(Sys, "ssh_host_rsa_key")),
    file:copy(filename:join(Data, "ssh_host_rsa_key.pub"), filename:join(Sys, "ssh_host_rsa_key.pub")),
    file:copy(filename:join(Data, "id_ecdsa256"),         filename:join(Usr, "id_ecdsa")),
    file:copy(filename:join(Data, "id_ecdsa256.pub"),     filename:join(Usr, "id_ecdsa.pub")),
    ssh_test_lib:setup_ecdsa_auth_keys("256", Data, SysUsr),
    ssh_test_lib:setup_rsa_known_host(Sys, Usr),
    Config;
init_per_group(key_cb, Config) ->
    case lists:member('ssh-rsa',
		      ssh_transport:default_algorithms(public_key)) of
	true ->
            DataDir = proplists:get_value(data_dir, Config),
            PrivDir = proplists:get_value(priv_dir, Config),
            ssh_test_lib:setup_rsa(DataDir, PrivDir),
            Config;
	false ->
	    {skip, unsupported_pub_key}
    end;
init_per_group(internal_error, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    ssh_test_lib:setup_dsa(DataDir, PrivDir),
    %% In the test case the key will be deleted after the daemon start:
    %% ... file:delete(filename:join(PrivDir, "system/ssh_host_dsa_key")),
    Config;
init_per_group(dir_options, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
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


end_per_group(dsa_key, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    ssh_test_lib:clean_dsa(PrivDir),
    Config;
end_per_group(rsa_key, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    ssh_test_lib:clean_rsa(PrivDir),
    Config;
end_per_group(dsa_pass_key, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    ssh_test_lib:clean_dsa(PrivDir),
    Config;
end_per_group(rsa_pass_key, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    ssh_test_lib:clean_rsa(PrivDir),
    Config;
end_per_group(key_cb, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    ssh_test_lib:clean_rsa(PrivDir),
    Config;
end_per_group(internal_error, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    ssh_test_lib:clean_dsa(PrivDir),
    Config;

end_per_group(_, Config) ->
    Config.
%%--------------------------------------------------------------------
init_per_testcase(TC, Config) when TC==shell_no_unicode ; 
				   TC==shell_unicode_string ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = proplists:get_value(priv_dir, Config),
    SysDir =  proplists:get_value(data_dir, Config),
    Sftpd = {_Pid, _Host, Port} =       
	ssh_test_lib:daemon([{system_dir, SysDir},
			     {user_dir, PrivDir},
			     {user_passwords, [{"foo", "bar"}]}]),
    ct:sleep(500),
    IO = ssh_test_lib:start_io_server(),
    Shell = ssh_test_lib:start_shell(Port, IO, [{user_dir,UserDir},
						{silently_accept_hosts, true},
						{user,"foo"},{password,"bar"}]),
    ct:log("IO=~p, Shell=~p, self()=~p",[IO,Shell,self()]),
    ct:log("file:native_name_encoding() = ~p,~nio:getopts() = ~p",
	   [file:native_name_encoding(),io:getopts()]),
    wait_for_erlang_first_line([{io,IO}, {shell,Shell}, {sftpd, Sftpd}  | Config]);

init_per_testcase(inet6_option, Config) ->
    case ssh_test_lib:has_inet6_address() of
	true ->
	    init_per_testcase('__default__', Config);
	false ->
	    {skip,"No ipv6 interface address"}
    end;
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(TestCase, Config) when TestCase == server_password_option;
					TestCase == server_userpassword_option ->
    UserDir = filename:join(proplists:get_value(priv_dir, Config), nopubkey),
    ssh_test_lib:del_dirs(UserDir),
    end_per_testcase(Config);
end_per_testcase(TC, Config) when TC==shell_no_unicode ; 
				  TC==shell_unicode_string ->
    case proplists:get_value(sftpd, Config) of
	{Pid, _, _} ->
	    catch ssh:stop_daemon(Pid);
	_ ->
	    ok
    end,
    end_per_testcase(Config);
end_per_testcase(_TestCase, Config) ->
    end_per_testcase(Config).

end_per_testcase(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
%%% Application consistency test.
app_test(Config) when is_list(Config) ->
    ?t:app_test(ssh),
    ok.
%%--------------------------------------------------------------------
%%% Appup file consistency test.
appup_test(Config) when is_list(Config) ->
    ok = ?t:appup_test(ssh).
%%--------------------------------------------------------------------
%%% Test that we can set some misc options not tested elsewhere
%%% some options not yet present are not decided if we should support or
%%% if they need thier own test case.
misc_ssh_options(Config) when is_list(Config) ->  
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),
    
    CMiscOpt0 = [{connect_timeout, 1000}, {user_dir, UserDir}, {silently_accept_hosts, true}],
    CMiscOpt1 = [{connect_timeout, infinity}, {user_dir, UserDir}, {silently_accept_hosts, true}],
    SMiscOpt0 =  [{user_dir, UserDir}, {system_dir, SystemDir}],
    SMiscOpt1 =  [{user_dir, UserDir}, {system_dir, SystemDir}],

    basic_test([{client_opts, CMiscOpt0}, {server_opts, SMiscOpt0}]),
    basic_test([{client_opts, CMiscOpt1}, {server_opts, SMiscOpt1}]).

%%--------------------------------------------------------------------
%%% Test configuring IPv4
inet_option(Config) when is_list(Config) ->   
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),
    
    ClientOpts =  [{silently_accept_hosts, true},
		   {user_dir, UserDir},
		   {user_interaction, false}],
    ServerOpts = [{system_dir, SystemDir},
		  {user_dir, UserDir},
		  {failfun, fun ssh_test_lib:failfun/2}], 

    basic_test([{client_opts, [{inet, inet} | ClientOpts]}, 
		{server_opts, [{inet, inet} | ServerOpts]}]).

%%--------------------------------------------------------------------
%%% Test configuring IPv6
inet6_option(Config) when is_list(Config) ->   
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),
    
    ClientOpts =  [{silently_accept_hosts, true},
		   {user_dir, UserDir},
		   {user_interaction, false}],
    ServerOpts = [{system_dir, SystemDir},
		  {user_dir, UserDir},
		  {failfun, fun ssh_test_lib:failfun/2}], 

    basic_test([{client_opts, [{inet, inet6} | ClientOpts]}, 
		{server_opts, [{inet, inet6} | ServerOpts]}]).

%%--------------------------------------------------------------------
%%% Test api function ssh_connection:exec
exec(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),
    
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
    Data0 = {ssh_cm, ConnectionRef, {data, ChannelId0, 0, <<"2">>}},
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
    Data1 = {ssh_cm, ConnectionRef, {data, ChannelId1, 0, <<"4">>}},
    case ssh_test_lib:receive_exec_result(Data1) of
	expected ->
	    ok;
	Other1 ->
	    ct:fail(Other1)
    end,
    ssh_test_lib:receive_exec_end(ConnectionRef, ChannelId1),
    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
%%% Test api function ssh_connection:exec with erlang server and the Command
%%% makes io
exec_with_io_out(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),
    
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
					  {user_interaction, false}]),
    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "io:write(hej).", infinity),
    case ssh_test_lib:receive_exec_result(
           [{ssh_cm, ConnectionRef, {data, ChannelId0, 0, <<"hej">>}},
            {ssh_cm, ConnectionRef, {data, ChannelId0, 0, <<"ok">>}}]) of
        expected ->
            ok;
        Other0 ->
	    ct:fail(Other0)
    end,
    ssh_test_lib:receive_exec_end(ConnectionRef, ChannelId0),
    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

exec_with_io_in(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),
    
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    C = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
					  {user_interaction, false}]),
    {ok, Ch} = ssh_connection:session_channel(C, infinity),
    ssh_connection:exec(C, Ch, "io:read('% ').", 1000),

    ssh_test_lib:receive_exec_result_or_fail({ssh_cm, C, {data,Ch,0,<<"% ">>}}),
    ok = ssh_connection:send(C, Ch, "hej.\n", 10000),

    ssh_test_lib:receive_exec_result_or_fail({ssh_cm, C, {data,Ch,0,<<"{ok,hej}">>}}),
    ssh_test_lib:receive_exec_end(C, Ch),
    ssh:close(C),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
%%% Test that compression option works
exec_compressed(Config) when is_list(Config) ->
    case ssh_test_lib:ssh_supports(zlib, compression) of
	false ->
	    {skip, "zlib compression is not supported"};

	true ->
	    process_flag(trap_exit, true),
	    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
	    UserDir = proplists:get_value(priv_dir, Config), 

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
	    Data = {ssh_cm, ConnectionRef, {data, ChannelId, 0, <<"2">>}},
	    case ssh_test_lib:receive_exec_result(Data) of
		expected ->
		    ok;
		Other ->
		    ct:fail(Other)
	    end,
	    ssh_test_lib:receive_exec_end(ConnectionRef, ChannelId),
	    ssh:close(ConnectionRef),
	    ssh:stop_daemon(Pid)
    end.

%%--------------------------------------------------------------------
%%% Idle timeout test, client 
idle_time_client(Config) ->
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),

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
%%% Idle timeout test, server
idle_time_server(Config) ->
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir},
                                             {idle_time, 2000},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
					  {user_interaction, false}]),
    {ok, Id} = ssh_connection:session_channel(ConnectionRef, 1000),
    ssh_connection:close(ConnectionRef, Id),
    receive
    after 10000 ->
	    {error, closed} = ssh_connection:session_channel(ConnectionRef, 1000)
    end,
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
%%% Test that ssh:shell/2 works
shell(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),
   
    {_Pid, _Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},{user_dir, UserDir},
					       {failfun, fun ssh_test_lib:failfun/2}]),
    ct:sleep(500),

    IO = ssh_test_lib:start_io_server(),
    Shell = ssh_test_lib:start_shell(Port, IO, [{user_dir,UserDir}]),
    receive
	{'EXIT', _, _} ->
	    ct:fail(no_ssh_connection);  
	ErlShellStart ->
	    ct:log("Erlang shell start: ~p~n", [ErlShellStart]),
	    do_shell(IO, Shell)
    after 
	30000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.
    
%%--------------------------------------------------------------------
%%% Test that we could user different types of host pubkey and user pubkey
exec_key_differs1(Config) -> exec_key_differs(Config, ['ecdsa-sha2-nistp256']).

exec_key_differs2(Config) -> exec_key_differs(Config, ['ssh-dss','ecdsa-sha2-nistp256']).

exec_key_differs3(Config) -> exec_key_differs(Config, ['ecdsa-sha2-nistp384','ecdsa-sha2-nistp256']).



exec_key_differs(Config, UserPKAlgs) ->
    case lists:usort(['ssh-rsa'|UserPKAlgs])
	-- ssh_transport:supported_algorithms(public_key)
    of
	[] ->
	    process_flag(trap_exit, true),
	    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system_rsa),
	    SystemUserDir = filename:join(SystemDir, user),
	    UserDir = filename:join(proplists:get_value(priv_dir, Config), user_ecdsa_256),
   
	    {_Pid, _Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
						       {user_dir, SystemUserDir},
						       {preferred_algorithms,
							[{public_key,['ssh-rsa'|UserPKAlgs]}]}]),
	    ct:sleep(500),

	    IO = ssh_test_lib:start_io_server(),
	    Shell = ssh_test_lib:start_shell(Port, IO, [{user_dir,UserDir},
							{preferred_algorithms,[{public_key,['ssh-rsa']}]},
							{pref_public_key_algs,UserPKAlgs}
						       ]),


	    receive
		{'EXIT', _, _} ->
		    ct:fail(no_ssh_connection);  
		ErlShellStart ->
		    ct:log("Erlang shell start: ~p~n", [ErlShellStart]),
		    do_shell(IO, Shell)
	    after 
		30000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
	    end;

	UnsupportedPubKeys ->
	    {skip, io_lib:format("~p unsupported",[UnsupportedPubKeys])}
    end.
    
%%--------------------------------------------------------------------
exec_key_differs_fail(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system_rsa),
    SystemUserDir = filename:join(SystemDir, user),
    UserDir = filename:join(proplists:get_value(priv_dir, Config), user_ecdsa_256),
   
    {_Pid, _Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					       {user_dir, SystemUserDir},
					       {preferred_algorithms,
						[{public_key,['ssh-rsa']}]}]),
    ct:sleep(500),

    IO = ssh_test_lib:start_io_server(),
    ssh_test_lib:start_shell(Port, IO, [{user_dir,UserDir},
                                        {recv_ext_info, false},
					{preferred_algorithms,[{public_key,['ssh-rsa']}]},
					{pref_public_key_algs,['ssh-dss']}]),
    receive
	{'EXIT', _, _} ->
	    ok;  
	ErlShellStart ->
	    ct:log("Erlang shell start: ~p~n", [ErlShellStart]),
	    ct:fail(connection_not_rejected)
    after 
	30000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.
    
%%--------------------------------------------------------------------
cli(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),
    
    TmpDir = filename:join(proplists:get_value(priv_dir,Config), "tmp"),
    ok = ssh_test_lib:del_dirs(TmpDir),
    ok = file:make_dir(TmpDir),

    {_Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},{user_dir, UserDir},
					       {password, "morot"},
					       {ssh_cli, {ssh_test_cli, [cli,TmpDir]}}, 
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
    ssh_connection:send(ConnectionRef, ChannelId, <<"q">>),
    receive 
	{ssh_cm, ConnectionRef,
	 {data,0,0, <<"\r\nYou are accessing a dummy, type \"q\" to exit\r\n\n">>}} ->
	    ssh_connection:send(ConnectionRef, ChannelId, <<"q">>)
    after 
	30000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end,
    
    receive 
     	{ssh_cm, ConnectionRef,{closed, ChannelId}} ->
     	    ok
    after 
	30000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.

%%--------------------------------------------------------------------
%%% Test that get correct error message if you try to start a daemon
%%% on an adress that already runs a daemon see also seq10667
daemon_already_started(Config) when is_list(Config) ->
    SystemDir = proplists:get_value(data_dir, Config),
    UserDir = proplists:get_value(priv_dir, Config),

    {Pid, _Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					      {user_dir, UserDir},
					      {failfun, fun ssh_test_lib:failfun/2}]),
    {error, eaddrinuse} = ssh_test_lib:daemon(Port, [{system_dir, SystemDir},
						     {user_dir, UserDir},
						     {failfun,
						      fun ssh_test_lib:failfun/2}]),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
%%% Test that a failed daemon start does not leave the port open
daemon_error_closes_port(Config) ->
    GoodSystemDir = proplists:get_value(data_dir, Config),
    Port = ssh_test_lib:inet_port(),
    {error,_} = ssh_test_lib:daemon(Port, []), % No system dir
    case ssh_test_lib:daemon(Port, [{system_dir, GoodSystemDir}]) of
        {error,eaddrinuse} ->
            {fail, "Port leakage"};
        {error,Error} ->
            ct:log("Strange error: ~p",[Error]),
            {fail, "Strange error"};
        {Pid, _Host, Port} ->
            %% Ok
            ssh:stop_daemon(Pid)
    end.
    

%%--------------------------------------------------------------------
%%% check that known_hosts is updated correctly
known_hosts(Config) when is_list(Config) ->
    SystemDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config), 
    
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
    [StoredHost, _Ip] = string:tokens(HostAndIp, ","),
    true = ssh_test_lib:match_ip(StoredHost, Host),
    "ssh-" ++ _ = Alg,
    ssh:stop_daemon(Pid).
%%--------------------------------------------------------------------

%%% Test that we can use keyes protected by pass phrases
pass_phrase(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),
    PhraseArg = proplists:get_value(pass_phrase, Config),

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
%%% Test that we can use key callback
key_callback(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),
    NoPubKeyDir = filename:join(UserDir, "nopubkey"),
    file:make_dir(NoPubKeyDir),

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir},
					     {failfun, fun ssh_test_lib:failfun/2}]),

    ConnectOpts = [{silently_accept_hosts, true},
                   {user_dir, NoPubKeyDir},
                   {user_interaction, false},
                   {key_cb, ssh_key_cb}],

    ConnectionRef = ssh_test_lib:connect(Host, Port, ConnectOpts),

    {ok, _ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    ssh:stop_daemon(Pid).


%%--------------------------------------------------------------------
%%% Test that we can use key callback with callback options
key_callback_options(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),

    NoPubKeyDir = filename:join(UserDir, "nopubkey"),
    file:make_dir(NoPubKeyDir),

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
                                             {user_dir, UserDir},
                                             {failfun, fun ssh_test_lib:failfun/2}]),

    {ok, PrivKey} = file:read_file(filename:join(UserDir, "id_rsa")),

    ConnectOpts = [{silently_accept_hosts, true},
                   {user_dir, NoPubKeyDir},
                   {user_interaction, false},
                   {key_cb, {ssh_key_cb_options, [{priv_key, PrivKey}]}}],

    ConnectionRef = ssh_test_lib:connect(Host, Port, ConnectOpts),

    {ok, _ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    ssh:stop_daemon(Pid).


%%--------------------------------------------------------------------
%%% Test that client does not hang if disconnects due to internal error
internal_error(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = proplists:get_value(priv_dir, Config),
    SystemDir = filename:join(PrivDir, system),
    
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
                                             {user_dir, UserDir},
                                             {failfun, fun ssh_test_lib:failfun/2}]),

    %% Now provoke an error in the following connect:
    file:delete(filename:join(PrivDir, "system/ssh_host_dsa_key")), 

    {error, Error} =
        ssh:connect(Host, Port, [{silently_accept_hosts, true},
                                 {user_dir, UserDir},
                                 {user_interaction, false}]),
    check_error(Error),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
%%% Test ssh_connection:send/3
send(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),

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
%%%
fail_daemon_start(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),

    {error,_} = ssh_test_lib:daemon([{system_dir, SystemDir},
                                     {user_dir, UserDir},
                                     {failfun, fun ssh_test_lib:failfun/2}]).

%%--------------------------------------------------------------------
%%% Test ssh:connection_info([peername, sockname])
peername_sockname(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),

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
    ct:log("Client: ~p ~p", [ClientPeer, ClientSock]),
    receive
	{ssh_cm, ConnectionRef, {data, ChannelId, _, Response}} ->
	    {PeerNameSrv,SockNameSrv} = binary_to_term(Response),
	    {HostPeerSrv,PortPeerSrv} = PeerNameSrv,
	    {HostSockSrv,PortSockSrv} = SockNameSrv,
	    ct:log("Server: ~p ~p", [PeerNameSrv, SockNameSrv]),
	    host_equal(HostPeerSrv, HostSockClient),
	    PortPeerSrv = PortSockClient,
	    host_equal(HostSockSrv, HostPeerClient),
	    PortSockSrv = PortPeerClient,
	    host_equal(HostSockSrv, Host),
	    PortSockSrv = Port
    after 10000 ->
	    ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.

host_equal(H1, H2) ->
    not ordsets:is_disjoint(ips(H1), ips(H2)).

ips(IP) when is_tuple(IP) -> ordsets:from_list([IP]);
ips(Name) when is_list(Name) ->
    {ok,#hostent{h_addr_list=IPs4}} = inet:gethostbyname(Name,inet),
    {ok,#hostent{h_addr_list=IPs6}} = inet:gethostbyname(Name,inet6),
    ordsets:from_list(IPs4++IPs6).

%%--------------------------------------------------------------------

%%% Client receives close when server closes
close(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),
    
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
	    ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.

%%--------------------------------------------------------------------
%%% Simulate that we try to close an already closed connection
double_close(Config) when is_list(Config) ->
    SystemDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config), 
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
daemon_opt_fd(Config) ->
    SystemDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config), 
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),

    {ok,S1} = gen_tcp:listen(0,[]),
    {ok,Fd1} = prim_inet:getfd(S1),
    
    {ok,Pid1} = ssh:daemon(0, [{system_dir, SystemDir},
			       {fd,Fd1},
			       {user_dir, UserDir},
			       {user_passwords, [{"vego", "morot"}]},
			       {failfun, fun ssh_test_lib:failfun/2}]),
    
    {ok,{_Host1,Port1}} = inet:sockname(S1),
    {ok, C1} = ssh:connect("localhost", Port1, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
					  {user, "vego"},
					  {password, "morot"},
					  {user_interaction, false}]),
    exit(C1, {shutdown, normal}),
    ssh:stop_daemon(Pid1),
    gen_tcp:close(S1).


%%--------------------------------------------------------------------
multi_daemon_opt_fd(Config) ->
    SystemDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config), 
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),

    Test = 
	fun() ->
		{ok,S} = gen_tcp:listen(0,[]),
		{ok,Fd} = prim_inet:getfd(S),

		{ok,Pid} = ssh:daemon(0, [{system_dir, SystemDir},
					  {fd,Fd},
					  {user_dir, UserDir},
					  {user_passwords, [{"vego", "morot"}]},
					  {failfun, fun ssh_test_lib:failfun/2}]),

		{ok,{_Host,Port}} = inet:sockname(S),
		{ok, C} = ssh:connect("localhost", Port, [{silently_accept_hosts, true},
							  {user_dir, UserDir},
							  {user, "vego"},
							  {password, "morot"},
							  {user_interaction, false}]),
		{S,Pid,C}
	end,

    Tests = [Test(),Test(),Test(),Test(),Test(),Test()],

    [begin 
	 gen_tcp:close(S),
	 ssh:stop_daemon(Pid),
	 exit(C, {shutdown, normal})
     end || {S,Pid,C} <- Tests].

%%--------------------------------------------------------------------
packet_size(Config) ->
    SystemDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config), 
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
    lists:foreach(
      fun(MaxPacketSize) ->
              ct:log("Try max_packet_size=~p",[MaxPacketSize]),
              {ok,Ch} = ssh_connection:session_channel(Conn, 1000, MaxPacketSize, 60000),
              ok = ssh_connection:shell(Conn, Ch),
              rec(Server, Conn, Ch, MaxPacketSize),
              ssh_connection:close(Conn, Ch)
      end, [0, 1, 10, 25]),

    ssh:close(Conn),
    ssh:stop_daemon(Server),
    ok.


rec(Server, Conn, Ch, MaxSz) ->
    receive
        {ssh_cm,Conn,{data,Ch,_,M}} when size(M) =< MaxSz ->
            ct:log("~p: ~p",[MaxSz,M]),
            rec(Server, Conn, Ch, MaxSz);
        {ssh_cm,Conn,{data,Ch,_,_}} = M ->
            ct:log("Max pkt size=~p. Got ~p",[MaxSz,M]),
            ssh:close(Conn),
            ssh:stop_daemon(Server),
            ct:fail("Does not obey max_packet_size=~p",[MaxSz])
    after
        2000 -> 
            ct:log("~p: ok!",[MaxSz]),
            ok
    end.

%%--------------------------------------------------------------------
shell_no_unicode(Config) ->
    new_do_shell(proplists:get_value(io,Config),
		 [new_prompt,
		  {type,"io:format(\"hej ~p~n\",[42])."},
		  {expect,"hej 42"},
		  {expect,"ok"},
		  new_prompt,
		  {type,"exit()."}
		 ]).
	      
%%--------------------------------------------------------------------
shell_unicode_string(Config) ->
    new_do_shell(proplists:get_value(io,Config),
		 [new_prompt,
		  {type,"io:format(\"こにちわ~ts~n\",[\"四二\"])."},
		  {expect,"こにちわ四二"},
		  {expect,"ok"},
		  new_prompt,
		  {type,"exit()."}
		 ]).

%%--------------------------------------------------------------------
%%% Test basic connection with openssh_zlib
openssh_zlib_basic_test(Config) ->
    case ssh_test_lib:ssh_supports(['zlib@openssh.com',none], compression) of
	{false,L} ->
	    {skip, io_lib:format("~p compression is not supported",[L])};

	true ->
	    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
	    UserDir = proplists:get_value(priv_dir, Config),

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
	    ssh:stop_daemon(Pid)
    end.

%%--------------------------------------------------------------------
ssh_info_print(Config) ->
    %% Just check that ssh_print:info() crashes
    PrivDir = proplists:get_value(priv_dir, Config),
    PrintFile = filename:join(PrivDir,info),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),

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
%% Check that a basd pwd is not tried more times. Could cause lock-out
%% on server

login_bad_pwd_no_retry1(Config) ->
    login_bad_pwd_no_retry(Config, "keyboard-interactive,password").

login_bad_pwd_no_retry2(Config) ->
    login_bad_pwd_no_retry(Config, "password,keyboard-interactive").

login_bad_pwd_no_retry3(Config) ->
    login_bad_pwd_no_retry(Config, "password,publickey,keyboard-interactive").

login_bad_pwd_no_retry4(Config) ->
    login_bad_pwd_no_retry(Config, "password,keyboard-interactive").

login_bad_pwd_no_retry5(Config) ->
    login_bad_pwd_no_retry(Config, "password,keyboard-interactive,password,password").


login_bad_pwd_no_retry(Config, AuthMethods) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),

    Parent = self(),
    PwdFun = fun(_, _, _, undefined) -> {false, 1};
		(_, _, _,         _) -> Parent ! retry_bad_pwd,
					false
	     end,

    {DaemonRef, _Host, Port} = 
	ssh_test_lib:daemon([{system_dir, SysDir},
			     {user_dir, UserDir},
			     {auth_methods, AuthMethods},
			     {user_passwords, [{"foo","somepwd"}]},
			     {pwdfun, PwdFun}
			    ]),

    ConnRes = ssh:connect("localhost", Port,
			  [{silently_accept_hosts, true},
			   {user, "foo"},
			   {password, "badpwd"},
			   {user_dir, UserDir},
			   {user_interaction, false}]),

    receive
	retry_bad_pwd ->
	    ssh:stop_daemon(DaemonRef),
	    {fail, "Retry bad password"}
    after 0 ->
	    case ConnRes of
		{error,"Unable to connect using the available authentication methods"} ->
		    ssh:stop_daemon(DaemonRef),
		    ok;
		{ok,Conn} ->
		    ssh:close(Conn),
		    ssh:stop_daemon(DaemonRef),
		    {fail, "Connect erroneosly succeded"}
	    end
    end.


%%----------------------------------------------------------------------------
%%% Test that when shell REPL exit with reason normal client receives status 0
shell_exit_status(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = proplists:get_value(data_dir, Config),
    UserDir = proplists:get_value(priv_dir, Config),

    ShellFun = fun (_User) -> spawn(fun() -> ok end) end,
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
                                             {user_dir, UserDir},
                                             {user_passwords, [{"vego", "morot"}]},
                                             {shell, ShellFun},
                                             {failfun, fun ssh_test_lib:failfun/2}]),
    ConnectionRef =
        ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
                                          {user_dir, UserDir},
                                          {user, "vego"},
                                          {password, "morot"},
                                          {user_interaction, false}]),

    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    ok = ssh_connection:shell(ConnectionRef, ChannelId),
    ssh_test_lib:receive_exec_end(ConnectionRef, ChannelId),
    ssh:stop_daemon(Pid).


%%----------------------------------------------------------------------------
%%% Idle timeout test
rekey0() -> [{timetrap,{seconds,90}}].
rekey1() -> [{timetrap,{seconds,90}}].
rekey2() -> [{timetrap,{seconds,90}}].
rekey3() -> [{timetrap,{seconds,90}}].
rekey4() -> [{timetrap,{seconds,90}}].
    
rekey0(Config) -> rekey_chk(Config, 0,                   0).
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

rekey_limit_client() -> [{timetrap,{seconds,400}}].
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



rekey_limit_daemon() -> [{timetrap,{seconds,400}}].
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
norekey_limit_client() -> [{timetrap,{seconds,400}}].
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
norekey_limit_daemon() -> [{timetrap,{seconds,400}}].
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

rekey_time_limit_client() -> [{timetrap,{seconds,400}}].
rekey_time_limit_client(Config) ->
    Minutes = ?REKEY_DATA_TMO div 60000,
    GB = 1024*1000*1000,
    Algs = proplists:get_value(preferred_algorithms, Config),
    {Pid, Host, Port} = ssh_test_lib:std_daemon(Config,[{max_random_length_padding,0},
							{preferred_algorithms,Algs}]),
    ConnectionRef = ssh_test_lib:std_connect(Config, Host, Port, [{rekey_limit, {Minutes, GB}},
                                                                  {max_random_length_padding,0}]),
    rekey_time_limit(Pid, ConnectionRef).

rekey_time_limit_daemon() -> [{timetrap,{seconds,400}}].
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

    RPort = ssh_test_lib:inet_port(),
    {ok,RelayPid} = ssh_relay:start_link({0,0,0,0}, RPort, Host, DPort),


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

    RPort = ssh_test_lib:inet_port(),
    {ok,RelayPid} = ssh_relay:start_link({0,0,0,0}, RPort, Host, DPort),

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
%% Due to timing the error message may or may not be delivered to
%% the "tcp-application" before the socket closed message is recived
check_error("Invalid state") -> ok;
check_error("Connection closed") -> ok;
check_error("Selection of key exchange algorithm failed"++_) -> ok;
check_error("No host key available") -> ok;
check_error(Error) -> ct:fail(Error).

basic_test(Config) ->
    ClientOpts = proplists:get_value(client_opts, Config),
    ServerOpts = proplists:get_value(server_opts, Config),
    
    {Pid, Host, Port} = ssh_test_lib:daemon(ServerOpts),
    {ok, CM} = ssh:connect(Host, Port, ClientOpts),
    ok = ssh:close(CM),
    ssh:stop_daemon(Pid).

do_shell(IO, Shell) ->
    receive
	ErlPrompt0 ->
	    ct:log("Erlang prompt: ~p~n", [ErlPrompt0])
    end,
    IO ! {input, self(), "1+1.\r\n"},
     receive
	Echo0 ->
	     ct:log("Echo: ~p ~n", [Echo0])
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end,
    receive
	?NEWLINE ->
	    ok
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end,
    receive
	Result0 = <<"2">> ->
	    ct:log("Result: ~p~n", [Result0])
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end,
    receive
	?NEWLINE ->
	    ok
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end,
    receive
	ErlPrompt1 ->
	    ct:log("Erlang prompt: ~p~n", [ErlPrompt1])
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
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
    %% 	    ct:log("Echo: ~p ~n", [Echo1])
    %% end,
    %% receive
    %% 	?NEWLINE ->
    %%  	    ok
    %% end,
    %% receive
    %%  	Result1 ->
    %%  	    ct:log("Result: ~p~n", [Result1])
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
	    ct:log("Erlang shell start: ~p~n", [_ErlShellStart]),
	    Config;
	Other ->
	    ct:log("Unexpected answer from ssh server: ~p",[Other]),
	    {fail,unexpected_answer}
    after 10000 ->
	    ct:log("No answer from ssh-server"),
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
	    ct:log("Skip newline ~p",[_X]),
	    new_do_shell(IO, N, Ops);
	
	<<P1,"> ">> when (P1-$0)==N -> 
	    new_do_shell_prompt(IO, N, Order, Arg, More);
	<<"(",Pfx:PfxSize/binary,")",P1,"> ">> when (P1-$0)==N -> 
	    new_do_shell_prompt(IO, N, Order, Arg, More);
	<<"('",Pfx:PfxSize/binary,"')",P1,"> ">> when (P1-$0)==N -> 
	    new_do_shell_prompt(IO, N, Order, Arg, More);

	<<P1,P2,"> ">> when (P1-$0)*10 + (P2-$0) == N -> 
	    new_do_shell_prompt(IO, N, Order, Arg, More);
	<<"(",Pfx:PfxSize/binary,")",P1,P2,"> ">> when (P1-$0)*10 + (P2-$0) == N -> 
	    new_do_shell_prompt(IO, N, Order, Arg, More);
	<<"('",Pfx:PfxSize/binary,"')",P1,P2,"> ">> when (P1-$0)*10 + (P2-$0) == N -> 
	    new_do_shell_prompt(IO, N, Order, Arg, More);

	<<P1,P2,P3,"> ">> when (P1-$0)*100 + (P2-$0)*10 + (P3-$0) == N -> 
	    new_do_shell_prompt(IO, N, Order, Arg, More);
	<<"(",Pfx:PfxSize/binary,")",P1,P2,P3,"> ">> when (P1-$0)*100 + (P2-$0)*10 + (P3-$0) == N -> 
	    new_do_shell_prompt(IO, N, Order, Arg, More);
	<<"('",Pfx:PfxSize/binary,"')",P1,P2,P3,"> ">> when (P1-$0)*100 + (P2-$0)*10 + (P3-$0) == N -> 
	    new_do_shell_prompt(IO, N, Order, Arg, More);

	Err when element(1,Err)==error ->
	    ct:fail("new_do_shell error: ~p~n",[Err]);

	RecBin when Order==expect ; Order==expect_echo ->
	    ct:log("received ~p",[RecBin]),
	    RecStr = string:strip(unicode:characters_to_list(RecBin)),
	    ExpStr = string:strip(Arg),
	    case lists:prefix(ExpStr, RecStr) of
		true when Order==expect ->
		    ct:log("Matched ~ts",[RecStr]),
		    new_do_shell(IO, N, More);
		true when Order==expect_echo ->
		    ct:log("Matched echo ~ts",[RecStr]),
		    new_do_shell(IO, N, More);
		false ->
		    ct:fail("*** Expected ~p, but got ~p",[string:strip(ExpStr),RecStr])
	    end
    after 30000 ->
	    ct:log("Message queue of ~p:~n~p",
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
                  atom_to_list(Node))
    end.
	    

new_do_shell_prompt(IO, N, type, Str, More) ->
    ct:log("Matched prompt ~p to trigger sending of next line to server",[N]),
    IO ! {input, self(), Str++"\r\n"},
    ct:log("Promt '~p> ', Sent ~ts",[N,Str++"\r\n"]),
    new_do_shell(IO, N, [{expect_echo,Str}|More]); % expect echo of the sent line
new_do_shell_prompt(IO, N, Op, Str, More) ->
    ct:log("Matched prompt ~p",[N]),
    new_do_shell(IO, N, [{Op,Str}|More]).
  
%%--------------------------------------------------------------------
