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
-module(ssh_connection_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("ssh/src/ssh_connect.hrl").
-include("ssh_test_lib.hrl").

-compile(export_all).

-define(SSH_DEFAULT_PORT, 22).
-define(EXEC_TIMEOUT, 10000).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

%% suite() ->
%%     [{ct_hooks,[ts_install_cth]}].

suite() ->
    [{timetrap,{seconds,40}}].

all() ->
    [
     {group, openssh},
     small_interrupted_send,
     interrupted_send,
     exec_erlang_term,
     exec_erlang_term_non_default_shell,
     start_shell,
     start_shell_exec,
     start_shell_exec_fun,
     start_shell_exec_fun2,
     start_shell_exec_fun3,
     start_shell_exec_direct_fun,
     start_shell_exec_direct_fun2,
     start_shell_exec_direct_fun3,
     start_shell_exec_direct_fun1_error,
     start_shell_exec_direct_fun1_error_type,
     start_shell_sock_exec_fun,
     start_shell_sock_daemon_exec,
     connect_sock_not_tcp,
     daemon_sock_not_tcp,
     gracefull_invalid_version,
     gracefull_invalid_start,
     gracefull_invalid_long_start,
     gracefull_invalid_long_start_no_nl,
     stop_listener,
     start_subsystem_on_closed_channel,
     max_channels_option
    ].
groups() ->
    [{openssh, [], payload() ++ ptty() ++ sock()}].

payload() ->
    [simple_exec,
     simple_exec_sock,
     small_cat,
     big_cat,
     send_after_exit].

ptty() ->
    [ptty_alloc_default,
     ptty_alloc,
     ptty_alloc_pixel].

sock() ->
    [connect_sock_not_passive,
     daemon_sock_not_passive
    ].

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ?CHECK_CRYPTO(Config).

end_per_suite(Config) ->
    catch ssh:stop(),
    Config.

%%--------------------------------------------------------------------
init_per_group(openssh, Config) ->
    case ssh_test_lib:gen_tcp_connect("localhost", 22, []) of
	{error,econnrefused} ->
	    {skip,"No openssh deamon (econnrefused)"};
	{ok, Socket} ->
	    gen_tcp:close(Socket),
	    ssh_test_lib:openssh_sanity_check(Config)
    end;
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    %% To make sure we start clean as it is not certain that
    %% end_per_testcase will be run!
    end_per_testcase(Config),
    ssh:start(),
    Config.

end_per_testcase(_Config) ->
    ssh:stop().

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
simple_exec() ->
    [{doc, "Simple openssh connectivity test for ssh_connection:exec"}].

simple_exec(Config) when is_list(Config) ->
    ConnectionRef = ssh_test_lib:connect(?SSH_DEFAULT_PORT, [{silently_accept_hosts, true},
							     {user_interaction, false}]),
    do_simple_exec(ConnectionRef).


simple_exec_sock(_Config) ->
    {ok, Sock} = ssh_test_lib:gen_tcp_connect("localhost", ?SSH_DEFAULT_PORT, [{active,false}]),
    {ok, ConnectionRef} = ssh:connect(Sock, [{silently_accept_hosts, true},
					     {user_interaction, false}]),
    do_simple_exec(ConnectionRef).
    


do_simple_exec(ConnectionRef) ->
    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "echo testing", infinity),
    %% receive response to input
    receive
	{ssh_cm, ConnectionRef, {data, ChannelId0, 0, <<"testing\n">>}} ->
	    ok
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end,

    %% receive close messages
    receive
	{ssh_cm, ConnectionRef, {eof, ChannelId0}} ->
	    ok
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end,
    receive
	{ssh_cm, ConnectionRef, {exit_status, ChannelId0, 0}} ->
	    ok
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end,
    receive
	{ssh_cm, ConnectionRef,{closed, ChannelId0}} ->
	    ok
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.

%%--------------------------------------------------------------------
connect_sock_not_tcp(_Config) ->
    {ok,Sock} = gen_udp:open(0, []), 
    {error, not_tcp_socket} = ssh:connect(Sock, []),
    gen_udp:close(Sock).

%%--------------------------------------------------------------------
daemon_sock_not_tcp(_Config) ->
    {ok,Sock} = gen_udp:open(0, []), 
    {error, not_tcp_socket} = ssh:daemon(Sock),
    gen_udp:close(Sock).

%%--------------------------------------------------------------------
connect_sock_not_passive(_Config) ->
    {ok,Sock} = ssh_test_lib:gen_tcp_connect("localhost", ?SSH_DEFAULT_PORT, []), 
    {error, not_passive_mode} = ssh:connect(Sock, []),
    gen_tcp:close(Sock).

%%--------------------------------------------------------------------
daemon_sock_not_passive(_Config) ->
    {ok,Sock} = ssh_test_lib:gen_tcp_connect("localhost", ?SSH_DEFAULT_PORT, []), 
    {error, not_passive_mode} = ssh:daemon(Sock),
    gen_tcp:close(Sock).

%%--------------------------------------------------------------------
small_cat() ->
    [{doc, "Use 'cat' to echo small data block back to us."}].

small_cat(Config) when is_list(Config) ->
    ConnectionRef = ssh_test_lib:connect(?SSH_DEFAULT_PORT, [{silently_accept_hosts, true},
							     {user_interaction, false}]),
    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "cat", infinity),

    Data = <<"I like spaghetti squash">>,
    ok = ssh_connection:send(ConnectionRef, ChannelId0, Data),
    ok = ssh_connection:send_eof(ConnectionRef, ChannelId0),

    %% receive response to input
    receive
	{ssh_cm, ConnectionRef, {data, ChannelId0, 0, Data}} ->
	    ok
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end,

    %% receive close messages
    receive
	{ssh_cm, ConnectionRef, {eof, ChannelId0}} ->
	    ok
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end,
    receive
	{ssh_cm, ConnectionRef, {exit_status, ChannelId0, 0}} ->
	    ok
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end,
    receive
	{ssh_cm, ConnectionRef,{closed, ChannelId0}} ->
	    ok
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.
%%--------------------------------------------------------------------
big_cat() ->
    [{doc,"Use 'cat' to echo large data block back to us."}].

big_cat(Config) when is_list(Config) ->
    ConnectionRef = ssh_test_lib:connect(?SSH_DEFAULT_PORT, [{silently_accept_hosts, true},
							     {user_interaction, false}]),
    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "cat", infinity),

    %% build 10MB binary
    Data = << <<X:32>> || X <- lists:seq(1,2500000)>>,

    %% pre-adjust receive window so the other end doesn't block
    ssh_connection:adjust_window(ConnectionRef, ChannelId0, size(Data)),

    ct:log("sending ~p byte binary~n",[size(Data)]),
    ok = ssh_connection:send(ConnectionRef, ChannelId0, Data, 10000),
    ok = ssh_connection:send_eof(ConnectionRef, ChannelId0),

    %% collect echoed data until eof
    case big_cat_rx(ConnectionRef, ChannelId0) of
	{ok, Data} ->
	    ok;
	{ok, Other} ->
	    case size(Data) =:= size(Other) of
		true ->
		    ct:log("received and sent data are same"
			   "size but do not match~n",[]);
		false ->
		    ct:log("sent ~p but only received ~p~n",
			   [size(Data), size(Other)])
	    end,
	    ct:fail(receive_data_mismatch);
	Else ->
	    ct:fail(Else)
    end,

    %% receive close messages (eof already consumed)
    receive
	{ssh_cm, ConnectionRef, {exit_status, ChannelId0, 0}} ->
	    ok 
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end,
    receive
	{ssh_cm, ConnectionRef,{closed, ChannelId0}} ->
	    ok
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.

%%--------------------------------------------------------------------
send_after_exit() ->
    [{doc, "Send channel data after the channel has been closed."}].

send_after_exit(Config) when is_list(Config) ->
    ConnectionRef = ssh_test_lib:connect(?SSH_DEFAULT_PORT, [{silently_accept_hosts, true},
							     {user_interaction, false}]),
    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),
    Data = <<"I like spaghetti squash">>,

    %% Shell command "false" will exit immediately
    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "false", infinity),
    receive
	{ssh_cm, ConnectionRef, {eof, ChannelId0}} ->
	    ok
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end,
    receive
	{ssh_cm, ConnectionRef, {exit_status, ChannelId0, _ExitStatus}} ->
	    ok
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end,
    receive
	{ssh_cm, ConnectionRef,{closed, ChannelId0}} ->
	    ok
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end,
    case ssh_connection:send(ConnectionRef, ChannelId0, Data, 2000) of
	{error, closed} -> ok;
	ok ->
	    ct:fail({expected,{error,closed}, {got, ok}});
	{error, timeout} ->
	    ct:fail({expected,{error,closed}, {got, {error, timeout}}});
	Else ->
	    ct:fail(Else)
    end.

%%--------------------------------------------------------------------
ptty_alloc_default() ->
    [{doc, "Test sending PTTY alloc message with only defaults."}].

ptty_alloc_default(Config) when is_list(Config) ->
    ConnectionRef = ssh_test_lib:connect(?SSH_DEFAULT_PORT, [{silently_accept_hosts, true},
							     {user_interaction, false}]),
    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:ptty_alloc(ConnectionRef, ChannelId, []),
    ssh:close(ConnectionRef).

%%--------------------------------------------------------------------
ptty_alloc() ->
    [{doc, "Test sending PTTY alloc message with width,height options."}].

ptty_alloc(Config) when is_list(Config) ->
    ConnectionRef = ssh_test_lib:connect(?SSH_DEFAULT_PORT, [{silently_accept_hosts, true},
							     {user_interaction, false}]),
    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:ptty_alloc(ConnectionRef, ChannelId, 
					[{term, os:getenv("TERM", ?DEFAULT_TERMINAL)}, {width, 70}, {height, 20}]),
    ssh:close(ConnectionRef).


%%--------------------------------------------------------------------
ptty_alloc_pixel() ->
    [{doc, "Test sending PTTY alloc message pixel options."}].

ptty_alloc_pixel(Config) when is_list(Config) ->
    ConnectionRef = ssh_test_lib:connect(?SSH_DEFAULT_PORT, [{silently_accept_hosts, true},
							     {user_interaction, false}]),
    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:ptty_alloc(ConnectionRef, ChannelId, 
					[{term, os:getenv("TERM", ?DEFAULT_TERMINAL)}, {pixel_widh, 630}, {pixel_hight, 470}]),
    ssh:close(ConnectionRef).

%%--------------------------------------------------------------------
small_interrupted_send(Config) -> 
    K = 1024,
    M = K*K,
    do_interrupted_send(Config, 10*M, 4*K).
interrupted_send(Config) ->
    M = 1024*1024,
    do_interrupted_send(Config, 10*M, 4*M).

do_interrupted_send(Config, SendSize, EchoSize) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    EchoSS_spec = {ssh_echo_server, [EchoSize,[{dbg,true}]]},
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {subsystems, [{"echo_n",EchoSS_spec}]}]),
    
    ct:log("~p:~p connect", [?MODULE,?LINE]),
    ConnectionRef = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
						      {user, "foo"},
						      {password, "morot"},
						      {user_interaction, false},
						      {user_dir, UserDir}]),
    ct:log("~p:~p connected", [?MODULE,?LINE]),

    %% build big binary
    Data = << <<X:32>> || X <- lists:seq(1,SendSize div 4)>>,

    %% expect remote end to send us EchoSize back
    <<ExpectedData:EchoSize/binary, _/binary>> = Data,

    %% Spawn listener. Otherwise we could get a deadlock due to filled buffers
    Parent = self(),
    ResultPid = spawn(
		  fun() ->
			  ct:log("~p:~p open channel",[?MODULE,?LINE]),
			  {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
			  ct:log("~p:~p start subsystem", [?MODULE,?LINE]),
			  case ssh_connection:subsystem(ConnectionRef, ChannelId, "echo_n", infinity) of
			      success ->
				  Parent ! {self(), channelId, ChannelId},
				  
				  Result = 
				      try collect_data(ConnectionRef, ChannelId, EchoSize)
				      of
					  ExpectedData -> 
					      ct:log("~p:~p got expected data",[?MODULE,?LINE]),
					      ok;
					  Other ->
					      ct:log("~p:~p unexpect: ~p", [?MODULE,?LINE,Other]),
					      {fail,"unexpected result in listener"}
				      catch
					  Class:Exception ->
					      {fail, io_lib:format("Listener exception ~p:~p",[Class,Exception])}
				      end,
				  Parent ! {self(), result, Result};
			      Other ->
				  Parent ! {self(), channelId, error, Other}
			  end
		  end),
    
    receive
	{ResultPid, channelId, error, Other} ->
	    ct:log("~p:~p channelId error ~p", [?MODULE,?LINE,Other]),
	    ssh:close(ConnectionRef),
	    ssh:stop_daemon(Pid),
	    {fail, "ssh_connection:subsystem"};

	{ResultPid, channelId, ChannelId} ->
	    ct:log("~p:~p ~p going to send ~p bytes", [?MODULE,?LINE,self(),size(Data)]),
	    SenderPid = spawn(fun() ->
				      Parent ! {self(),  ssh_connection:send(ConnectionRef, ChannelId, Data, 30000)}
			      end),
	    receive
	    	{ResultPid, result, {fail, Fail}} ->
		    ct:log("~p:~p Listener failed: ~p", [?MODULE,?LINE,Fail]),
		    {fail, Fail};

		{ResultPid, result, Result} ->
		    ct:log("~p:~p Got result: ~p", [?MODULE,?LINE,Result]),
		    ssh:close(ConnectionRef),
		    ssh:stop_daemon(Pid),
		    ct:log("~p:~p Check sender", [?MODULE,?LINE]),
		    receive
			{SenderPid, {error, closed}} ->
			    ct:log("~p:~p {error,closed} - That's what we expect :)",[?MODULE,?LINE]),
			    ok;
			Msg ->
			    ct:log("~p:~p Not expected send result: ~p",[?MODULE,?LINE,Msg]),
			    {fail, "Not expected msg"}
		    end;

		{SenderPid, {error, closed}} ->
		    ct:log("~p:~p {error,closed} - That's what we expect, but client channel handler has not reported yet",[?MODULE,?LINE]),
		    receive
			{ResultPid, result, Result} ->
			    ct:log("~p:~p Now got the result: ~p", [?MODULE,?LINE,Result]),
			    ssh:close(ConnectionRef),
			    ssh:stop_daemon(Pid),
			    ok;
			Msg ->
			    ct:log("~p:~p Got an unexpected msg ~p",[?MODULE,?LINE,Msg]),
			    {fail, "Un-expected msg"}
		    end;

		Msg ->
		    ct:log("~p:~p Got unexpected ~p",[?MODULE,?LINE,Msg]),
		    {fail, "Unexpected msg"}
	    end
    end.

%%--------------------------------------------------------------------
start_shell() ->
    [{doc, "Start a shell"}].

start_shell(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {shell, fun(U, H) -> start_our_shell(U, H) end} ]),

    ConnectionRef = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
						      {user, "foo"},
						      {password, "morot"},
						      {user_interaction, true},
						      {user_dir, UserDir}]),

    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),
    ok = ssh_connection:shell(ConnectionRef,ChannelId0),

    receive
	{ssh_cm,ConnectionRef, {data, ChannelId0, 0, <<"Enter command\r\n">>}} ->
	    ok
    after 5000 ->
	    ct:fail("CLI Timeout")
    end,

    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).
%%--------------------------------------------------------------------
start_shell_exec() ->
    [{doc, "start shell to exec command"}].

start_shell_exec(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {exec, {?MODULE,ssh_exec_echo,[]}} ]),

    ConnectionRef = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
						      {user, "foo"},
						      {password, "morot"},
						      {user_interaction, true},
						      {user_dir, UserDir}]),

    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),

    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "testing", infinity),
    receive
	{ssh_cm, ConnectionRef, {data, _ChannelId, 0, <<"echo testing\r\n">>}} ->
	    ok
    after 5000 ->
	    ct:fail("Exec Timeout")
    end,

    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
exec_erlang_term(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"}
                                            ]),

    ConnectionRef = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
						      {user, "foo"},
						      {password, "morot"},
						      {user_interaction, true},
						      {user_dir, UserDir}]),

    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),

    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "1+2.", infinity),
    TestResult =
        receive
            {ssh_cm, ConnectionRef, {data, _ChannelId, 0, <<"3",_/binary>>}} = R ->
                ct:log("Got expected ~p",[R]);
            Other ->
                ct:log("Got unexpected ~p",[Other])
        after 5000 ->
                {fail,"Exec Timeout"}
        end,

    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid),
    TestResult.

%%--------------------------------------------------------------------
exec_erlang_term_non_default_shell(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
                                             {shell, fun(U, H) -> start_our_shell(U, H) end}
                                            ]),

    ConnectionRef = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
						      {user, "foo"},
						      {password, "morot"},
						      {user_interaction, true},
						      {user_dir, UserDir}
                                                     ]),

    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),

    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "1+2.", infinity),
    TestResult =
        receive
            {ssh_cm, ConnectionRef, {data, _ChannelId, 0, <<"3",_/binary>>}} = R ->
                ct:log("Got unexpected ~p",[R]),
                {fail,"Could exec erlang term although non-erlang shell"};
            Other ->
                ct:log("Got expected ~p",[Other])
        after 5000 ->
                {fail, "Exec Timeout"}
        end,

    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid),
    TestResult.

%%--------------------------------------------------------------------
start_shell_exec_fun(Config) ->
    do_start_shell_exec_fun(fun ssh_exec_echo/1,
                            "testing", <<"echo testing\r\n">>, 0,
                            Config).

start_shell_exec_fun2(Config) ->
    do_start_shell_exec_fun(fun ssh_exec_echo/2,
                            "testing", <<"echo foo testing\r\n">>, 0,
                            Config).

start_shell_exec_fun3(Config) ->
    do_start_shell_exec_fun(fun ssh_exec_echo/3,
                            "testing", <<"echo foo testing\r\n">>, 0,
                            Config).

start_shell_exec_direct_fun(Config) ->
    do_start_shell_exec_fun({direct, fun ssh_exec_direct_echo/1},
                            "testing", <<"echo testing\n">>, 0,
                            Config).

start_shell_exec_direct_fun2(Config) ->
    do_start_shell_exec_fun({direct, fun ssh_exec_direct_echo/2},
                            "testing", <<"echo foo testing">>, 0,
                            Config).

start_shell_exec_direct_fun3(Config) ->
    do_start_shell_exec_fun({direct, fun ssh_exec_direct_echo/3},
                            "testing", <<"echo foo testing">>, 0,
                            Config).

start_shell_exec_direct_fun1_error(Config) ->
    do_start_shell_exec_fun({direct, fun ssh_exec_direct_echo_error_return/1},
                            "testing", <<"Error in \"testing\": {bad}\n">>, 1,
                            Config).

start_shell_exec_direct_fun1_error_type(Config) ->
    do_start_shell_exec_fun({direct, fun ssh_exec_direct_echo_error_return_type/1},
                            "testing", <<"Error in \"testing\": Bad exec-plugin return: very_bad\n">>, 1,
                            Config).



do_start_shell_exec_fun(Fun, Command, Expect, ExpectType, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {exec, Fun}]),

    ConnectionRef = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
						      {user, "foo"},
						      {password, "morot"},
						      {user_interaction, true},
						      {user_dir, UserDir}]),

    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),

    success = ssh_connection:exec(ConnectionRef, ChannelId0, Command, infinity),

    receive
	{ssh_cm, ConnectionRef, {data, _ChannelId, ExpectType, Expect}} ->
	    ok
    after 5000 ->
            receive
                Other ->
                    ct:pal("Received other:~n~p",[Other]),
                    ct:fail("Unexpected response")
            after 0 ->
                    ct:fail("Exec Timeout")
            end
    end,

    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
start_shell_sock_exec_fun() ->
    [{doc, "start shell on tcp-socket to exec command"}].

start_shell_sock_exec_fun(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    {Pid, HostD, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
                                              {user_dir, UserDir},
                                              {password, "morot"},
                                              {exec, fun ssh_exec_echo/1}]),
    Host = ssh_test_lib:ntoa(ssh_test_lib:mangle_connect_address(HostD)),

    {ok, Sock} = ssh_test_lib:gen_tcp_connect(Host, Port, [{active,false}]),
    {ok,ConnectionRef} = ssh:connect(Sock, [{silently_accept_hosts, true},
					    {user, "foo"},
					    {password, "morot"},
					    {user_interaction, true},
					    {user_dir, UserDir}]),

    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),

    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "testing", infinity),

    receive
	{ssh_cm, ConnectionRef, {data, _ChannelId, 0, <<"echo testing\r\n">>}} ->
	    ok
    after 5000 ->
	    ct:fail("Exec Timeout")
    end,

    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
start_shell_sock_daemon_exec(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),

    {ok,Sl} = gen_tcp:listen(0, [{active,false}]),
    {ok,{_IP,Port}} = inet:sockname(Sl),	% _IP is likely to be {0,0,0,0}. Win don't like...
    
    spawn_link(fun() ->
		       {ok,Ss} = ssh_test_lib:gen_tcp_connect("localhost", Port, [{active,false}]),
		       {ok, _Pid} = ssh:daemon(Ss, [{system_dir, SysDir},
						    {user_dir, UserDir},
						    {password, "morot"},
						    {exec, fun ssh_exec_echo/1}])
	       end),
    {ok,Sc} = gen_tcp:accept(Sl),
    {ok,ConnectionRef} = ssh:connect(Sc, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_interaction, true},
					  {user_dir, UserDir}]),
    
    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),

    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "testing", infinity),

    receive
	{ssh_cm, ConnectionRef, {data, _ChannelId, 0, <<"echo testing\r\n">>}} ->
	    ok
    after 5000 ->
	    ct:fail("Exec Timeout")
    end,

    ssh:close(ConnectionRef).
    
%%--------------------------------------------------------------------
gracefull_invalid_version(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    
    {_Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
                                               {user_dir, UserDir},
                                               {password, "morot"}]),

    {ok, S} = ssh_test_lib:gen_tcp_connect(Host, Port, []),
    ok = gen_tcp:send(S,  ["SSH-8.-1","\r\n"]),
    receive
	Verstring ->
	    ct:log("Server version: ~p~n", [Verstring]),
	    receive
		{tcp_closed, S} ->
		    ok
	    end
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.

gracefull_invalid_start(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    {_Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
                                               {user_dir, UserDir},
                                               {password, "morot"}]),

    {ok, S} = ssh_test_lib:gen_tcp_connect(Host, Port, []),
    ok = gen_tcp:send(S,  ["foobar","\r\n"]),
    receive
	Verstring ->
	    ct:log("Server version: ~p~n", [Verstring]),
	    receive
		{tcp_closed, S} ->
		    ok
	    end
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.

gracefull_invalid_long_start(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    {_Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
                                               {user_dir, UserDir},
                                               {password, "morot"}]),

    {ok, S} = ssh_test_lib:gen_tcp_connect(Host, Port, []),
    ok = gen_tcp:send(S, [lists:duplicate(257, $a), "\r\n"]),
    receive
	Verstring ->
	    ct:log("Server version: ~p~n", [Verstring]),
	    receive
		{tcp_closed, S} ->
		    ok
	    end
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.


gracefull_invalid_long_start_no_nl(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    {_Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
                                               {user_dir, UserDir},
                                               {password, "morot"}]),

    {ok, S} = ssh_test_lib:gen_tcp_connect(Host, Port, []),
    ok = gen_tcp:send(S, [lists:duplicate(257, $a), "\r\n"]),
    receive
	Verstring ->
	    ct:log("Server version: ~p~n", [Verstring]),
	    receive
		{tcp_closed, S} ->
		    ok
	    end
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.

stop_listener() ->
    [{doc, "start ssh daemon, setup connections, stop listener, restart listner"}].

stop_listener(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),

    {Pid0, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					      {user_dir, UserDir},
					      {password, "morot"},
					      {exec, fun ssh_exec_echo/1}]),

    ConnectionRef0 = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
						       {user, "foo"},
						       {password, "morot"},
						       {user_interaction, true},
						       {user_dir, UserDir}]),

    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef0, infinity),

    ssh:stop_listener(Host, Port),

    {error, _} = ssh:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_interaction, true},
					  {user_dir, UserDir}]),
    success = ssh_connection:exec(ConnectionRef0, ChannelId0,
				  "testing", infinity),
    receive
	{ssh_cm, ConnectionRef0, {data, ChannelId0, 0, <<"echo testing\r\n">>}} ->
	    ok
    after 5000 ->
	    ct:fail("Exec Timeout")
    end,

    case ssh_test_lib:daemon(Port, [{system_dir, SysDir},
                                    {user_dir, UserDir},
                                    {password, "potatis"},
                                    {exec, fun ssh_exec_echo/1}]) of
	{Pid1, Host, Port} ->
	    ConnectionRef1 = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
							       {user, "foo"},
							       {password, "potatis"},
							       {user_interaction, true},
							       {user_dir, UserDir}]),
	    {error, _} = ssh:connect(Host, Port, [{silently_accept_hosts, true},
                                                  {user, "foo"},
                                                  {password, "morot"},
                                                  {user_interaction, true},
                                                  {user_dir, UserDir}]),
	    ssh:close(ConnectionRef0),
	    ssh:close(ConnectionRef1),
	    ssh:stop_daemon(Pid0),
	    ssh:stop_daemon(Pid1);
	Error ->
	    ssh:close(ConnectionRef0),
	    ssh:stop_daemon(Pid0),
	    ct:fail({unexpected, Error})
    end.

start_subsystem_on_closed_channel(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {subsystems, [{"echo_n", {ssh_echo_server, [4000000]}}]}]),

    ConnectionRef = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
						      {user, "foo"},
						      {password, "morot"},
						      {user_interaction, false},
						      {user_dir, UserDir}]),


    {ok, ChannelId1} = ssh_connection:session_channel(ConnectionRef, infinity),
    ok = ssh_connection:close(ConnectionRef, ChannelId1),
    {error, closed} = ssh_connection:ptty_alloc(ConnectionRef, ChannelId1, []),
    {error, closed} = ssh_connection:subsystem(ConnectionRef, ChannelId1, "echo_n", 5000),
    {error, closed} = ssh_connection:exec(ConnectionRef, ChannelId1, "testing1.\n", 5000),
    {error, closed} = ssh_connection:send(ConnectionRef, ChannelId1, "exit().\n", 5000),

    %% Test that there could be a gap between close and an operation (Bugfix OTP-14939):
    {ok, ChannelId2} = ssh_connection:session_channel(ConnectionRef, infinity),
    ok = ssh_connection:close(ConnectionRef, ChannelId2),
    timer:sleep(2000),
    {error, closed} = ssh_connection:ptty_alloc(ConnectionRef, ChannelId2, []),
    {error, closed} = ssh_connection:subsystem(ConnectionRef, ChannelId2, "echo_n", 5000),
    {error, closed} = ssh_connection:exec(ConnectionRef, ChannelId2, "testing1.\n", 5000),
    {error, closed} = ssh_connection:send(ConnectionRef, ChannelId2, "exit().\n", 5000),

    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
max_channels_option() ->
    [{doc, "Test max_channels option"}].

max_channels_option(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {max_channels, 3},
					     {subsystems, [{"echo_n", {ssh_echo_server, [4000000]}}]}
					    ]),

    ConnectionRef = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
						      {user, "foo"},
						      {password, "morot"},
						      {user_interaction, true},
						      {user_dir, UserDir}]),

    %% Allocate a number of ChannelId:s to play with. (This operation is not
    %% counted by the max_channel option).
    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),
    {ok, ChannelId1} = ssh_connection:session_channel(ConnectionRef, infinity),
    {ok, ChannelId2} = ssh_connection:session_channel(ConnectionRef, infinity),
    {ok, ChannelId3} = ssh_connection:session_channel(ConnectionRef, infinity),
    {ok, ChannelId4} = ssh_connection:session_channel(ConnectionRef, infinity),
    {ok, ChannelId5} = ssh_connection:session_channel(ConnectionRef, infinity),
    {ok, ChannelId6} = ssh_connection:session_channel(ConnectionRef, infinity),
    {ok, _ChannelId7} = ssh_connection:session_channel(ConnectionRef, infinity),

    %% Now start to open the channels (this is counted my max_channels) to check that
    %% it gives a failure at right place

    %%%---- Channel 1(3): shell
    ok = ssh_connection:shell(ConnectionRef,ChannelId0),
    receive
	{ssh_cm,ConnectionRef, {data, ChannelId0, 0, <<"Eshell",_/binary>>}} ->
	    ok
    after 5000 ->
	    ct:fail("CLI Timeout")
    end,

    %%%---- Channel 2(3): subsystem "echo_n"
    success = ssh_connection:subsystem(ConnectionRef, ChannelId1, "echo_n", infinity),

    %%%---- Channel 3(3): exec. This closes itself.
    success = ssh_connection:exec(ConnectionRef, ChannelId2, "testing1.\n", infinity),
    receive
	{ssh_cm, ConnectionRef, {data, ChannelId2, 0, <<"testing1",_/binary>>}} ->
	    ok
    after 5000 ->
	    ct:fail("Exec #1 Timeout")
    end,

    %%%---- Channel 3(3): subsystem "echo_n" (Note that ChannelId2 should be closed now)
    ?wait_match(success, ssh_connection:subsystem(ConnectionRef, ChannelId3, "echo_n", infinity)),

    %%%---- Channel 4(3) !: exec  This should fail
    failure = ssh_connection:exec(ConnectionRef, ChannelId4, "testing2.\n", infinity),

    %%%---- close the shell (Frees one channel)
    ok = ssh_connection:send(ConnectionRef, ChannelId0, "exit().\n", 5000),
    
    %%%---- wait for the subsystem to terminate
    receive
	{ssh_cm,ConnectionRef,{closed,ChannelId0}} -> ok
    after 5000 ->
	    ct:log("Timeout waiting for '{ssh_cm,~p,{closed,~p}}'~n"
		   "Message queue:~n~p",
		   [ConnectionRef,ChannelId0,erlang:process_info(self(),messages)]),
	    ct:fail("exit Timeout",[])
    end,

    %%---- Try that we can open one channel instead of the closed one
    ?wait_match(success, ssh_connection:subsystem(ConnectionRef, ChannelId5, "echo_n", infinity)),

    %%---- But not a fourth one...
    failure = ssh_connection:subsystem(ConnectionRef, ChannelId6, "echo_n", infinity),

    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
big_cat_rx(ConnectionRef, ChannelId) ->
    big_cat_rx(ConnectionRef, ChannelId, []).

big_cat_rx(ConnectionRef, ChannelId, Acc) ->
    receive
	{ssh_cm, ConnectionRef, {data, ChannelId, 0, Data}} ->
	    %% ssh_connection:adjust_window(ConnectionRef, ChannelId, size(Data)),
	    %% window was pre-adjusted, don't adjust again here
	    big_cat_rx(ConnectionRef, ChannelId, [Data | Acc]);
	{ssh_cm, ConnectionRef, {eof, ChannelId}} ->
	    {ok, iolist_to_binary(lists:reverse(Acc))}
    after ?EXEC_TIMEOUT ->
	    timeout
    end.

collect_data(ConnectionRef, ChannelId, EchoSize) ->
    ct:log("~p:~p Listener ~p running! ConnectionRef=~p, ChannelId=~p",[?MODULE,?LINE,self(),ConnectionRef,ChannelId]),
    collect_data(ConnectionRef, ChannelId, EchoSize, [], 0).

collect_data(ConnectionRef, ChannelId, EchoSize, Acc, Sum) ->
    TO = 5000,
    receive
	{ssh_cm, ConnectionRef, {data, ChannelId, 0, Data}} when is_binary(Data) ->
	    ct:log("~p:~p collect_data: received ~p bytes. total ~p bytes,  want ~p more",
		   [?MODULE,?LINE,size(Data),Sum+size(Data),EchoSize-Sum]),
	    ssh_connection:adjust_window(ConnectionRef, ChannelId, size(Data)),
	    collect_data(ConnectionRef, ChannelId, EchoSize, [Data | Acc], Sum+size(Data));
	{ssh_cm, ConnectionRef, Msg={eof, ChannelId}} ->
	    collect_data_report_end(Acc, Msg, EchoSize);

	{ssh_cm, ConnectionRef, Msg={closed,ChannelId}} ->
	    collect_data_report_end(Acc, Msg, EchoSize);

	Msg ->
	    ct:log("~p:~p collect_data: ***** unexpected message *****~n~p",[?MODULE,?LINE,Msg]),
	    collect_data(ConnectionRef, ChannelId, EchoSize, Acc, Sum)

    after TO ->
	    ct:log("~p:~p collect_data: ----- Nothing received for ~p seconds -----~n",[?MODULE,?LINE,TO]),
	    collect_data(ConnectionRef, ChannelId, EchoSize, Acc, Sum)
    end.

collect_data_report_end(Acc, Msg, EchoSize) ->
    try
	iolist_to_binary(lists:reverse(Acc))
    of
	Bin ->
	    ct:log("~p:~p collect_data: received ~p.~nGot in total ~p bytes,  want ~p more",
		   [?MODULE,?LINE,Msg,size(Bin),EchoSize,size(Bin)]),
	    Bin
    catch
	C:E ->
	    ct:log("~p:~p collect_data: received ~p.~nAcc is strange...~nException=~p:~p~nAcc=~p",
		   [?MODULE,?LINE,Msg,C,E,Acc]),
	    {error,{C,E}}
    end.

%%%-------------------------------------------------------------------
%% This is taken from the ssh example code.
start_our_shell(_User, _Peer) ->
    spawn(fun() ->
		  io:format("Enter command\n")
		  %% Don't actually loop, just exit
          end).


ssh_exec_echo(Cmd) ->
    spawn(fun() ->
                  io:format("echo ~s\n", [Cmd])
          end).

ssh_exec_echo(Cmd, User) ->
    spawn(fun() ->
                  io:format("echo ~s ~s\n",[User,Cmd])
          end).
ssh_exec_echo(Cmd, User, _PeerAddr) ->
    ssh_exec_echo(Cmd,User).

ssh_exec_direct_echo(Cmd) -> {ok, io_lib:format("echo ~s~n",[Cmd])}.
ssh_exec_direct_echo(Cmd, User) -> {ok, io_lib:format("echo ~s ~s",[User,Cmd])}.
ssh_exec_direct_echo(Cmd, User, _PeerAddr) -> ssh_exec_direct_echo(Cmd,User).

ssh_exec_direct_echo_error_return(_Cmd) -> {error, {bad}}.
ssh_exec_direct_echo_error_return_type(_Cmd) -> very_bad.
