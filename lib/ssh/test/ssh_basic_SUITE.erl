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

-module(ssh_basic_SUITE).

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
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2
        ]).

-export([
         always_ok/1,
         app_test/1,
         appup_test/1,
         basic_test/1,
         check_error/1,
         cli/1,
         close/1,
         daemon_already_started/1,
         daemon_error_closes_port/1,
         daemon_opt_fd/1,
         double_close/1,
         exec/1,
         exec_compressed/1,
         exec_with_io_in/1,
         exec_with_io_out/1,
         host_equal/2,
         idle_time_client/1,
         idle_time_server/1,
         inet6_option/0,
         inet6_option/1,
         inet_option/1,
         internal_error/1,
         ips/1,
         key_callback/1,
         key_callback_options/1,
         known_hosts/1,
         login_bad_pwd_no_retry1/1,
         login_bad_pwd_no_retry2/1,
         login_bad_pwd_no_retry3/1,
         login_bad_pwd_no_retry4/1,
         login_bad_pwd_no_retry5/1,
         misc_ssh_options/1,
         multi_daemon_opt_fd/1,
         openssh_zlib_basic_test/1,
         packet_size/1,
         pass_phrase/1,
         peername_sockname/1,
         send/1,
         setopts_getopts/1,
         shell/1,
         shell_exit_status/1,
         shell_no_unicode/1,
         shell_socket/1,
         shell_ssh_conn/1,
         shell_unicode_string/1,
         ssh_file_is_auth_key/1,
         ssh_file_is_host_key/0,
         ssh_file_is_host_key/1,
         ssh_file_is_host_key_misc/1,
         ssh_info_print/1
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
    [{group, all_tests}
    ].

%%%-define(PARALLEL, ).
-define(PARALLEL, parallel).

groups() ->
    [{all_tests, [?PARALLEL], [{group, sequential},
                               {group, p_basic},
                               {group, internal_error},
                               {group, login_bad_pwd_no_retry},
                               {group, key_cb}
                              ]},

     {sequential, [], [app_test,
                       appup_test,
                       daemon_already_started,
                       daemon_error_closes_port, % Should be re-written..
                       double_close,
                       daemon_opt_fd,
                       multi_daemon_opt_fd,
                       packet_size,
                       ssh_info_print,
                       shell_exit_status,
                       setopts_getopts,
                       known_hosts,
                       ssh_file_is_host_key,
                       ssh_file_is_host_key_misc,
                       ssh_file_is_auth_key
                      ]},

     {key_cb, [?PARALLEL], [key_callback, key_callback_options]},

     {internal_error, [?PARALLEL], [internal_error]},

     {login_bad_pwd_no_retry, [?PARALLEL], [login_bad_pwd_no_retry1,
                                            login_bad_pwd_no_retry2,
                                            login_bad_pwd_no_retry3,
                                            login_bad_pwd_no_retry4,
                                            login_bad_pwd_no_retry5
                                           ]},
     
     {p_basic, [?PARALLEL], [send, peername_sockname,
                             exec, exec_compressed, 
                             exec_with_io_out, exec_with_io_in,
                             cli,
                             idle_time_client, idle_time_server, openssh_zlib_basic_test, 
                             misc_ssh_options, inet_option, inet6_option,
                             shell, shell_socket, shell_ssh_conn, shell_no_unicode, shell_unicode_string,
                             close
                            ]}
    ].

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ?CHECK_CRYPTO(begin
                      ssh:start(),
                      ct:log("Pub keys setup for: ~p",
                             [ssh_test_lib:setup_all_user_host_keys(Config)]),
                      Config
                  end).

end_per_suite(_Config) ->
    ssh:stop().

%%--------------------------------------------------------------------
init_per_group(key_cb, Config) ->
    case lists:member('ssh-rsa',
		      ssh_transport:supported_algorithms(public_key)) of
	true ->
            DataDir = proplists:get_value(data_dir, Config),
            PrivDir = proplists:get_value(priv_dir, Config),
            ssh_test_lib:setup_user_key('ssh-rsa', DataDir, PrivDir),
            ssh_test_lib:setup_host_key_create_dir('ssh-rsa', DataDir, PrivDir),
            Config;
	false ->
	    {skip, unsupported_pub_key}
    end;
init_per_group(_, Config) ->
    Config.


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
inet6_option() -> [{timetrap,{seconds,30}}].
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
            {ssh_cm, ConnectionRef, {data, ChannelId0, 0, <<"ok">>}},
            {ssh_cm, ConnectionRef, {eof, ChannelId0}},
            {ssh_cm, ConnectionRef, {exit_status, ChannelId0, 0}},
            {ssh_cm, ConnectionRef, {closed, ChannelId0}}
           ]) of
        expected ->
            ok;
        Other0 ->
	    ct:fail(Other0)
    end,
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
	{'EXIT', _, _} = Exit ->
            ct:log("~p:~p ~p", [?MODULE,?LINE,Exit]),
	    ct:fail(no_ssh_connection);  
	ErlShellStart ->
	    ct:log("Erlang shell start: ~p~n", [ErlShellStart]),
	    do_shell(IO, Shell)
    after 
	30000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.
    
%%--------------------------------------------------------------------
%%% Test that ssh:shell/2 works when attaching to a open TCP-connection
shell_socket(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),

    {_Pid, Host0, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},{user_dir, UserDir},
					       {failfun, fun ssh_test_lib:failfun/2}]),
    Host = ssh_test_lib:mangle_connect_address(Host0),
    ct:sleep(500),

    %% First test with active mode:
    {ok,ActiveSock} = gen_tcp:connect(Host,
                                      Port,
                                      [{active,true}]),
    {error,not_passive_mode} = ssh:shell(ActiveSock),
    ct:log("~p:~p active tcp socket failed ok", [?MODULE,?LINE]),
    gen_tcp:close(ActiveSock),

    %% Secondly, test with an UDP socket:
    {ok,BadSock} = gen_udp:open(0),
    {error,not_tcp_socket} = ssh:shell(BadSock),
    ct:log("~p:~p udp socket failed ok", [?MODULE,?LINE]),
    gen_udp:close(BadSock),

    %% And finaly test with passive mode (which should work):
    IO = ssh_test_lib:start_io_server(),
    {ok,Sock} = gen_tcp:connect(Host, Port, [{active,false}]),
    Shell = ssh_test_lib:start_shell(Sock, IO, [{user_dir,UserDir}]),
    gen_tcp:controlling_process(Sock, Shell),
    Shell ! start,

    receive
	{'EXIT', _, _} = Exit ->
            ct:log("~p:~p ~p", [?MODULE,?LINE,Exit]),
	    ct:fail(no_ssh_connection);
	ErlShellStart ->
	    ct:log("Erlang shell start: ~p~n", [ErlShellStart]),
	    do_shell(IO, Shell)
    after
	30000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.

%%--------------------------------------------------------------------
%%% Test that ssh:shell/2 works when attaching to a open SSH-connection
shell_ssh_conn(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),

    {_Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},{user_dir, UserDir},
					       {failfun, fun ssh_test_lib:failfun/2}]),
    ct:sleep(500),

    IO = ssh_test_lib:start_io_server(),
    {ok,C} = ssh:connect(Host, Port, [{silently_accept_hosts, true},
                                      {user_dir, UserDir},
                                      {user_interaction, false}]),
    Shell = ssh_test_lib:start_shell(C, IO, undefined),
    receive
	{'EXIT', _, _} = Exit ->
            ct:log("~p:~p ~p", [?MODULE,?LINE,Exit]),
	    ct:fail(no_ssh_connection);
	ErlShellStart ->
	    ct:log("Erlang shell start: ~p~n", [ErlShellStart]),
	    do_shell(IO, Shell)
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

%%%%%%%%%%%%%%%%%%%%%% REWRITE! %%%%%%%%%%%%%%%%%%%%
%%% 1) check that {error,_} is not  {error,eaddrinuse}
%%% 2) instead of ssh_test_lib:daemon second time, use gen_tcp:listen

daemon_error_closes_port(Config) ->
    GoodSystemDir = proplists:get_value(data_dir, Config),
    Port = inet_port(),
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
    
inet_port() ->
    {ok, Socket} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(Socket),
    gen_tcp:close(Socket),
    Port.

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
    ct:log("known_hosts:~n~p",[Binary]),
    Lines = string:tokens(binary_to_list(Binary), "\n"),
    [Line] = Lines,
    [HostAndIp, Alg, _KeyData] = string:tokens(Line, " "),

    {StoredHost,StoredPort} =
        case HostAndIp of
            "["++X -> [Hpart,":"++Pstr] = string:tokens(X, "]"),
                      {Hpart,list_to_integer(Pstr)};
            _ -> {HostAndIp,Port}
        end,
    
    true = ssh_test_lib:match_ip(StoredHost, Host) andalso (Port==StoredPort),
    "ssh-" ++ _ = Alg,
    NLines = length(binary:split(Binary, <<"\n">>, [global,trim_all])),
    ct:log("NLines = ~p~n~p", [NLines,Binary]),
    if
        NLines>1 -> ct:fail("wrong num lines", []);
        NLines<1 -> ct:fail("wrong num lines", []);
        true -> ok
    end,

    _ConnectionRef2 =
	ssh_test_lib:connect(Host, Port, [{user_dir, PrivDir},
					  {user_interaction, false},
					  silently_accept_hosts]),
    {ok, Binary2} = file:read_file(KnownHosts),
    case Binary of
        Binary2 -> ok;
        _ -> ct:log("2nd differ~n~p", [Binary2]),
             ct:fail("wrong num lines", [])
    end,

    Binary3 = <<"localhost,",Binary/binary>>,
    ok = file:write_file(KnownHosts, Binary3),
     _ConnectionRef3 =
	ssh_test_lib:connect(Host, Port, [{user_dir, PrivDir},
					  {user_interaction, false},
					  silently_accept_hosts]),
    ct:log("New known_hosts:~n~p",[Binary3]),
    {ok, Binary4} = file:read_file(KnownHosts),
    case Binary3 of
        Binary4 -> ok;
        _ -> ct:log("2nd differ~n~p", [Binary4]),
             ct:fail("wrong num lines", [])
    end,


    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
ssh_file_is_host_key() -> [{timetrap,{seconds,240}}]. % Some machines are S L O W !
ssh_file_is_host_key(Config) ->
    Dir = ssh_test_lib:create_random_dir(Config),
    ct:log("Dir = ~p", [Dir]),
    KnownHosts = filename:join(Dir, "known_hosts"),

    Key1 = {ed_pub,ed25519,<<73,72,235,162,96,101,154,59,217,114,123,192,96,105,250,29,
                             214,76,60,63,167,21,221,118,246,168,152,2,7,172,137,125>>},
    Key2 = {ed_pub,ed448,<<95,215,68,155,89,180,97,253,44,231,135,236,97,106,212,106,29,
                           161,52,36,133,167,14,31,138,14,167,93,128,233,103,120,237,241,
                           36,118,155,70,199,6,27,214,120,61,241,229,15,108,209,250,26,
                           190,175,232,37,97,128>>},
    Key3 = {'RSAPublicKey',26565213557098441060571713941539431805641814292761836797158846333985276408616038302348064841541244792430014595960643885863857366044141899534486816837416587694213836843799730043696945690516841209754307951050689906601353687467659852190777927968674989320642319504162787468947018505175948989102544757855693228490011564030927714896252701919941617689227585365348356580525802093985552564228730275431222515673065363441446158870936027338182083252824862151536327733046243804704721201548991176621134884093279416695997338124856506800535228380202243308550318880784741179703553922258881924287662178348044420509921666661119986374777,
            65537},

    FileContents = <<"h11,h12,[h13]:*,h14 ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIElI66JgZZo72XJ7wGBp+h3WTDw/pxXddvaomAIHrIl9\n",
                     "h21,[h22]:2345,h23 ssh-ed448 AAAACXNzaC1lZDQ0OAAAADlf10SbWbRh/Sznh+xhatRqHaE0JIWnDh"
                                                             "+KDqddgOlneO3xJHabRscGG9Z4PfHlD2zR+hq+r+glYYA=\n",
                     "  \n",
                     "\n",
                     "h31 ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDSb+D77XKvkMDWGu05CD6gWlEXJ+exSvxmegU1pvicPds090qTK3HwSzV7Hg1YVEV6bUiO74Om9Da4EMQponiSeLfVlIkBY5Ko4am4HMNOPTi5Ac4zR1B36nPvyTJluHKOZiCE0ZkSjKYvLEua0Y4Gqd+4RS93Q6r31OO8ukEVM+gG7z0tvhVLkAo8G5QnGRPW0z11tkfEeyjJzhk8H+4lmNjJRK4m6z71P0ACAEBJCpYKpKY3+AjksWuEZnWLgfuk9aPI4q8tI/TO3lF1BmyTPj7/QTFMiWgL7lNM94oaRHTjZ1CdB0UAW1+TMABu155z5KxVUIzrMoVKGBmJPhh5"
                   >>,
    ok = file:write_file(KnownHosts, FileContents),

    true = ssh_file:is_host_key(Key1, "h11",   22, 'ssh-ed25519', [{user_dir,Dir}]),
    true = ssh_file:is_host_key(Key1, "h12",   22, 'ssh-ed25519', [{user_dir,Dir}]),
    true = ssh_file:is_host_key(Key1, "h13", 1234, 'ssh-ed25519', [{user_dir,Dir}]),
    true = ssh_file:is_host_key(Key1, "h13",   22, 'ssh-ed25519', [{user_dir,Dir}]),
    true = ssh_file:is_host_key(Key1, "h14",   22, 'ssh-ed25519', [{user_dir,Dir}]),
    
    true = ssh_file:is_host_key(Key1, ["h11","noh1"],        22, 'ssh-ed25519', [{user_dir,Dir}]),
    true = ssh_file:is_host_key(Key1, ["noh1","h11"],        22, 'ssh-ed25519', [{user_dir,Dir}]),
    true = ssh_file:is_host_key(Key1, ["noh1","h12","noh2"], 22, 'ssh-ed25519', [{user_dir,Dir}]),

    true = ssh_file:is_host_key(Key2, "h21",   22, 'ssh-ed448', [{user_dir,Dir}]),
    false= ssh_file:is_host_key(Key2, "h22",   22, 'ssh-ed448', [{user_dir,Dir}]),
    true = ssh_file:is_host_key(Key2, "h22", 2345, 'ssh-ed448', [{user_dir,Dir}]),
    false= ssh_file:is_host_key(Key2, "h22", 1234, 'ssh-ed448', [{user_dir,Dir}]),
    true = ssh_file:is_host_key(Key2, "h23",   22, 'ssh-ed448', [{user_dir,Dir}]),
    
    false =  ssh_file:is_host_key(Key2, "h11", 22, 'ssh-ed448', [{user_dir,Dir}]),
    false =  ssh_file:is_host_key(Key1, "h21", 22, 'ssh-ed25519', [{user_dir,Dir}]),

    true = ssh_file:is_host_key(Key3, "h31",   22, 'ssh-rsa',     [{user_dir,Dir}]),
    true = ssh_file:is_host_key(Key3, "h31",   22, 'rsa-sha2-256',[{user_dir,Dir}]),

    ok.

%%--------------------------------------------------------------------
ssh_file_is_host_key_misc(Config) ->
    Dir = ssh_test_lib:create_random_dir(Config),
    ct:log("Dir = ~p", [Dir]),
    KnownHosts = filename:join(Dir, "known_hosts"),

    Key1 = {ed_pub,ed25519,<<73,72,235,162,96,101,154,59,217,114,123,192,96,105,250,29,
                             214,76,60,63,167,21,221,118,246,168,152,2,7,172,137,125>>},
    Key2 = {ed_pub,ed448,<<95,215,68,155,89,180,97,253,44,231,135,236,97,106,212,106,29,
                           161,52,36,133,167,14,31,138,14,167,93,128,233,103,120,237,241,
                           36,118,155,70,199,6,27,214,120,61,241,229,15,108,209,250,26,
                           190,175,232,37,97,128>>},

    FileContents = <<"h11,h12,!h12 ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIElI66JgZZo72XJ7wGBp+h3WTDw/pxXddvaomAIHrIl9\n",
                     %% Key revoked later in file:
                     "h22 ssh-ed448 AAAACXNzaC1lZDQ0OAAAADlf10SbWbRh/Sznh+xhatRqHaE0JIWnDh"
                                                             "+KDqddgOlneO3xJHabRscGG9Z4PfHlD2zR+hq+r+glYYA=\n",
                     "@revoked h22 ssh-ed448 AAAACXNzaC1lZDQ0OAAAADlf10SbWbRh/Sznh+xhatRqHaE0JIWnDh"
                                                             "+KDqddgOlneO3xJHabRscGG9Z4PfHlD2zR+hq+r+glYYA=\n",
                     "h21 ssh-ed448 AAAACXNzaC1lZDQ0OAAAADlf10SbWbRh/Sznh+xhatRqHaE0JIWnDh"
                                                             "+KDqddgOlneO3xJHabRscGG9Z4PfHlD2zR+hq+r+glYYA=\n"
                   >>,
    ok = file:write_file(KnownHosts, FileContents),

    true = ssh_file:is_host_key(Key1, "h11",   22, 'ssh-ed25519', [{user_dir,Dir}]),
    true = ssh_file:is_host_key(Key2, "h21",   22, 'ssh-ed448',   [{user_dir,Dir}]),

    true = ssh_file:is_host_key(Key2, "h21",   22, 'ssh-ed448',   [{user_dir,Dir},
                                                                   {key_cb_private,[{optimize,space}]}]),
    %% Check revoked key:
    {error,revoked_key} =
        ssh_file:is_host_key(Key2, "h22",   22, 'ssh-ed448',   [{user_dir,Dir}]),
    {error,revoked_key} =
        ssh_file:is_host_key(Key2, "h22",   22, 'ssh-ed448',   [{user_dir,Dir},
                                                                {key_cb_private,[{optimize,space}]}]),
    %% Check key with "!" in pattern:
    false= ssh_file:is_host_key(Key1, "h12",   22, 'ssh-ed25519', [{user_dir,Dir}]),

    ok.

%%--------------------------------------------------------------------
ssh_file_is_auth_key(Config) ->
    Dir = ssh_test_lib:create_random_dir(Config),
    ct:log("Dir = ~p", [Dir]),
    AuthKeys = filename:join(Dir, "authorized_keys"),

    Key1 = {ed_pub,ed25519,<<73,72,235,162,96,101,154,59,217,114,123,192,96,105,250,29,
                             214,76,60,63,167,21,221,118,246,168,152,2,7,172,137,125>>},
    Key2 = {ed_pub,ed448,<<95,215,68,155,89,180,97,253,44,231,135,236,97,106,212,106,29,
                           161,52,36,133,167,14,31,138,14,167,93,128,233,103,120,237,241,
                           36,118,155,70,199,6,27,214,120,61,241,229,15,108,209,250,26,
                           190,175,232,37,97,128>>},

    FileContents = <<" \n",
                     "# A test file\n",
                     "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIElI66JgZZo72XJ7wGBp+h3WTDw/pxXddvaomAIHrIl9 foo@example.com\n",
                     "no-X11-forwarding,pty ssh-ed448 AAAACXNzaC1lZDQ0OAAAADlf10SbWbRh/Sznh+xhatRqHaE0JIWnDh"
                                                             "+KDqddgOlneO3xJHabRscGG9Z4PfHlD2zR+hq+r+glYYA= bar@example.com\n"
                   >>,
    ok = file:write_file(AuthKeys, FileContents),

    true = ssh_file:is_auth_key(Key1, "donald_duck", [{user_dir,Dir}]),
    true = ssh_file:is_auth_key(Key2, "mickey_mouse", [{user_dir,Dir}]),

    true = ssh_file:is_auth_key(Key1, "donald_duck", [{user_dir,Dir},{key_cb_private,[{optimize,space}]}]),
    true = ssh_file:is_auth_key(Key2, "mickey_mouse", [{user_dir,Dir},{key_cb_private,[{optimize,space}]}]),

    ok.

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
    file:delete(filename:join(PrivDir, "system/ssh_host_rsa_key")), 
    file:delete(filename:join(PrivDir, "system/ssh_host_dsa_key")), 
    file:delete(filename:join(PrivDir, "system/ssh_host_ecdsa_key")),
    file:delete(filename:join(PrivDir, "system/ssh_host_ed25519_key")), 
    file:delete(filename:join(PrivDir, "system/ssh_host_ed448_key")), 

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
                                             {preferred_algorithms, ssh_transport:supported_algorithms()},
					     {user_dir, UserDir},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{preferred_algorithms, ssh_transport:supported_algorithms()},
                                          {silently_accept_hosts, true},
					  {user_dir, UserDir},
					  {user_interaction, false}]),
    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    ok = ssh_connection:send(ConnectionRef, ChannelId, <<"Data">>),
    ok = ssh_connection:send(ConnectionRef, ChannelId, << >>),
    ssh:stop_daemon(Pid).


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

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
                                             {user_dir, UserDir},
                                             {user_passwords, [{"vego", "morot"}]},
                                             {shell, {?MODULE,always_ok,[]}},
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

always_ok(_) -> ok.
    
%%----------------------------------------------------------------------------
setopts_getopts(Config) ->
    process_flag(trap_exit, true),
    SystemDir = proplists:get_value(data_dir, Config),
    UserDir = proplists:get_value(priv_dir, Config),

    ShellFun = fun (_User, _Peer) -> spawn(fun() -> ok end) end,
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
                                             {user_dir, UserDir},
                                             {user_passwords, [{"vego", "morot"}]},
                                             {shell, ShellFun},
                                             {failfun, fun ssh_test_lib:failfun/2}]),
    ConnectionRef =
        ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
                                          {quiet_mode, true}, % Just to use quiet_mode once
                                          {user_dir, UserDir},
                                          {user, "vego"},
                                          {password, "morot"},
                                          {user_interaction, false}]),
    %% Test get_sock_opts
    {ok,[{active,once},{deliver,term},{mode,binary},{packet,0}]} =
        ssh:get_sock_opts(ConnectionRef, [active, deliver, mode, packet]),

    %% Test to set forbidden opts
    {error,{not_allowed,[active,deliver,mode,packet]}} =
        ssh:set_sock_opts(ConnectionRef, [{active,once},{deliver,term},{mode,binary},{packet,0}]),
    
    %% Test to set some other opt
    {ok,[{delay_send,DS0}]} =
        ssh:get_sock_opts(ConnectionRef, [delay_send]),
    DS1 = not DS0,
    ok = ssh:set_sock_opts(ConnectionRef, [{delay_send,DS1}]),
    {ok,[{delay_send,DS1}]} =
        ssh:get_sock_opts(ConnectionRef, [delay_send]),
    
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
	{'EXIT', _, _} = Exit ->
            ct:log("~p:~p ~p", [?MODULE,?LINE,Exit]),
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
  
