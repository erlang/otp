%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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
-include("ssh_connect.hrl").
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
         big_cat/1,
         connect_sock_not_passive/1,
         connect_sock_not_tcp/1,
         connect2_invalid_options/1,
         connect_invalid_port/1,
         connect_invalid_timeout_0/1,
         connect_invalid_timeout_1/1,
         connect_invalid_options/1,
         connect3_invalid_port/1,
         connect3_invalid_options/1,
         connect3_invalid_timeout_0/1,
         connect3_invalid_timeout_1/1,
         connect3_invalid_both/1,
         connect4_invalid_two_0/1,
         connect4_invalid_two_1/1,
         connect4_invalid_two_2/1,
         connect4_invalid_three/1,
         connect_timeout/1,
         daemon_sock_not_passive/1,
         daemon_sock_not_tcp/1,
         do_interrupted_send/3,
         do_simple_exec/1,
         encode_decode_pty_opts/1,
         exec_disabled/1,
         exec_erlang_term/1,
         exec_erlang_term_non_default_shell/1,
         exec_shell_disabled/1,
         gracefull_invalid_long_start/1,
         gracefull_invalid_long_start_no_nl/1,
         gracefull_invalid_start/1,
         gracefull_invalid_version/1,
         kex_error/1,
         interrupted_send/1,
         max_channels_option/1,
         no_sensitive_leak/1,
         ptty_alloc/1,
         ptty_alloc_default/1,
         ptty_alloc_pixel/1,
         read_write_loop/1,
         read_write_loop1/2,
         send_after_exit/1,
         simple_eval/1,
         simple_exec/1,
         simple_exec_sock/1,
         simple_exec_two_socks/1,
         small_cat/1,
         small_interrupted_send/1,
         start_exec_direct_fun1_read_write/1,
         start_exec_direct_fun1_read_write_advanced/1,
         start_shell/1,
         new_shell_dumb_term/1,
         new_shell_xterm_term/1,
         start_shell_pty/1,
         start_shell_exec/1,
         start_shell_exec_direct_fun/1,
         start_shell_exec_direct_fun1_error/1,
         start_shell_exec_direct_fun1_error_type/1,
         start_shell_exec_direct_fun2/1,
         start_shell_exec_direct_fun3/1,
         start_shell_exec_fun/1,
         start_shell_exec_fun2/1,
         start_shell_exec_fun3/1,
         start_shell_sock_daemon_exec/1,
         start_shell_sock_daemon_exec_multi/1,
         start_shell_sock_exec_fun/1,
         start_subsystem_on_closed_channel/1,
         stop_listener/1,
         ssh_exec_echo/2 % called as an MFA
        ]).

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
     exec_disabled,
     exec_shell_disabled,
     start_shell,
     new_shell_dumb_term,
     new_shell_xterm_term,
     start_shell_pty,
     start_shell_exec,
     start_shell_exec_fun,
     start_shell_exec_fun2,
     start_shell_exec_fun3,
     start_shell_exec_direct_fun,
     start_shell_exec_direct_fun2,
     start_shell_exec_direct_fun3,
     start_shell_exec_direct_fun1_error,
     start_shell_exec_direct_fun1_error_type,
     start_exec_direct_fun1_read_write,
     start_exec_direct_fun1_read_write_advanced,
     start_shell_sock_exec_fun,
     start_shell_sock_daemon_exec,
     start_shell_sock_daemon_exec_multi,
     encode_decode_pty_opts,
     connect_sock_not_tcp,
     connect2_invalid_options,
     connect_invalid_port,
     connect_invalid_options,
     connect_invalid_timeout_0,
     connect_invalid_timeout_1,
     connect3_invalid_port,
     connect3_invalid_options,
     connect3_invalid_timeout_0,
     connect3_invalid_timeout_1,
     connect3_invalid_both,
     connect4_invalid_two_0,
     connect4_invalid_two_1,
     connect4_invalid_two_2,
     connect4_invalid_three,
     connect_timeout,
     daemon_sock_not_tcp,
     gracefull_invalid_version,
     gracefull_invalid_start,
     gracefull_invalid_long_start,
     gracefull_invalid_long_start_no_nl,
     kex_error,
     stop_listener,
     no_sensitive_leak,
     start_subsystem_on_closed_channel,
     max_channels_option
    ].
groups() ->
    [{openssh, [], payload() ++ ptty() ++ sock()}].

payload() ->
    [simple_exec,
     simple_exec_sock,
     simple_exec_two_socks,
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
    ?CHECK_CRYPTO(
       [{ptty_supported, ssh_test_lib:ptty_supported()}
        | Config]
      ).

end_per_suite(_Config) ->
    catch ssh:stop(),
    ok.

%%--------------------------------------------------------------------
init_per_group(openssh, Config) ->
    case ssh_test_lib:gen_tcp_connect(?SSH_DEFAULT_PORT, []) of
	{error,econnrefused} ->
	    {skip,"No openssh daemon (econnrefused)"};
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
    end_per_testcase(any, Config),
    ssh:start(),
    ssh_test_lib:verify_sanity_check(Config).

end_per_testcase(_TestCase, _Config) ->
    ssh:stop().

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
simple_exec(Config) when is_list(Config) ->
    ConnectionRef = ssh_test_lib:connect(?SSH_DEFAULT_PORT, []),
    do_simple_exec(ConnectionRef).

%%--------------------------------------------------------------------
simple_exec_sock(_Config) ->
    {ok, Sock} = ssh_test_lib:gen_tcp_connect(?SSH_DEFAULT_PORT, [{active,false}]),
    {ok, ConnectionRef} = ssh:connect(Sock, [{save_accepted_host, false},
                                             {silently_accept_hosts, true},
                                             {user_interaction, true}
                                            ]),
    do_simple_exec(ConnectionRef).

%%--------------------------------------------------------------------
simple_exec_two_socks(_Config) ->
    Parent = self(),
    F = fun() ->
                spawn_link(
                  fun() ->
                          {ok, Sock} = ssh_test_lib:gen_tcp_connect(?SSH_DEFAULT_PORT, [{active,false}]),
                          {ok, ConnectionRef} = ssh:connect(Sock, [{save_accepted_host, false},
                                                                   {silently_accept_hosts, true},
                                                                   {user_interaction, true}]),
                          Parent ! {self(),do_simple_exec(ConnectionRef)}
                  end)
        end,
    Pid1 = F(),
    Pid2 = F(),
    receive
        {Pid1,ok} -> ok
    end,
    receive
        {Pid2,ok} -> ok
    end.

%%--------------------------------------------------------------------
connect2_invalid_options(_Config) ->
    {error, invalid_options} = ssh:connect(bogus_socket, {bad, option}).

connect3_invalid_port(_Config) ->
    {error, invalid_port} = ssh:connect(bogus_host, noport, [{key, value}]).

connect3_invalid_options(_Config) ->
    {error, invalid_options} = ssh:connect(bogus_host, 1337, bad_options).

connect3_invalid_timeout_0(_Config) ->
    {error, invalid_timeout} =
        ssh:connect(bogus_socket, [{key, value}], short).

connect3_invalid_timeout_1(_Config) ->
    {error, invalid_timeout} =
        ssh:connect(bogus_socket, [{key, value}], -1).

connect3_invalid_both(_Config) ->
    %% The actual reason is implementation dependent.
    {error, _Reason} =
        ssh:connect(bogus, no_list_or_port, no_list_or_timeout).

connect_invalid_port(Config) ->
    {Pid, Host, _Port, UserDir} = daemon_start(Config),

    {error, invalid_port} =
        ssh:connect(Host, undefined,
                    [{silently_accept_hosts, true},
                     {user, "foo"},
                     {password, "morot"},
                     {user_interaction, false},
                     {user_dir, UserDir}],
                    infinity),

    ssh:stop_daemon(Pid).

connect_invalid_timeout_0(Config) ->
    {Pid, Host, Port, UserDir} = daemon_start(Config),

    {error, invalid_timeout} =
        ssh:connect(Host, Port,
                    [{silently_accept_hosts, true},
                     {user, "foo"},
                     {password, "morot"},
                     {user_interaction, false},
                     {user_dir, UserDir}],
                   longer),

    ssh:stop_daemon(Pid).

connect_invalid_timeout_1(Config) ->
    {Pid, Host, Port, UserDir} = daemon_start(Config),

    {error, invalid_timeout} =
        ssh:connect(Host, Port,
                    [{silently_accept_hosts, true},
                     {user, "foo"},
                     {password, "morot"},
                     {user_interaction, false},
                     {user_dir, UserDir}],
                   -1),

    ssh:stop_daemon(Pid).

connect_invalid_options(Config) ->
    {Pid, Host, Port, _UserDir} = daemon_start(Config),

    {error, invalid_options} =
        ssh:connect(Host, Port,
                    {user, "foo"},
                    infinity),

    ssh:stop_daemon(Pid).

%% Two out three arguments incorrect.  Three possibilities.
connect4_invalid_two_0(Config) ->
    {Pid, Host, _Port, _UserDir} = daemon_start(Config),

    %% Actual error implementation dependent
    {error, _} =
        ssh:connect(Host, noport,
                    {user, "foo"},
                    infinity),

    ssh:stop_daemon(Pid).

connect4_invalid_two_1(Config) ->
    {Pid, Host, Port, _UserDir} = daemon_start(Config),

    %% Actual error implementation dependent
    {error, _} =
        ssh:connect(Host, Port,
                    {user, "foo"},
                    short),

    ssh:stop_daemon(Pid).

connect4_invalid_two_2(Config) ->
    {Pid, Host, _Port, _UserDir} = daemon_start(Config),

    %% Actual error implementation dependent
    {error, _} =
        ssh:connect(Host, newport,
                    [{user, "foo"}],
                    -1),

    ssh:stop_daemon(Pid).

%% All three args incorrect
connect4_invalid_three(Config) ->
    {Pid, Host, _Port, _UserDir} = daemon_start(Config),

    %% Actual error implementation dependent
    {error, _} =
        ssh:connect(Host, teleport,
                    {user, "foo"},
                    fortnight),

    ssh:stop_daemon(Pid).

daemon_start(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
                                             {user_dir, UserDir},
                                             {password, "morot"},
                                             {exec, fun ssh_exec_echo/1}]),
    {Pid, Host, Port, UserDir}.

%%--------------------------------------------------------------------
connect_sock_not_tcp(_Config) ->
    {ok,Sock} = gen_udp:open(0, []), 
    {error, not_tcp_socket} = ssh:connect(Sock, [{save_accepted_host, false},
                                                 {silently_accept_hosts, true},
                                                 {user_interaction, true}]),
    gen_udp:close(Sock).

%%--------------------------------------------------------------------
connect_timeout(_Config) ->
    {ok,Sl} = gen_tcp:listen(0, []),
    {ok, {_,Port}} = inet:sockname(Sl),
    {error,timeout} = ssh:connect(loopback, Port, [{connect_timeout,2000},
                                                   {save_accepted_host, false},
                                                   {silently_accept_hosts, true}]),
    gen_tcp:close(Sl).

%%--------------------------------------------------------------------
daemon_sock_not_tcp(_Config) ->
    {ok,Sock} = gen_udp:open(0, []), 
    {error, not_tcp_socket} = ssh:daemon(Sock),
    gen_udp:close(Sock).

%%--------------------------------------------------------------------
connect_sock_not_passive(_Config) ->
    {ok,Sock} = ssh_test_lib:gen_tcp_connect(?SSH_DEFAULT_PORT, []), 
    {error, not_passive_mode} = ssh:connect(Sock, [{save_accepted_host, false},
                                                   {silently_accept_hosts, true},
                                                   {user_interaction, true}]),
    gen_tcp:close(Sock).

%%--------------------------------------------------------------------
daemon_sock_not_passive(_Config) ->
    {ok,Sock} = ssh_test_lib:gen_tcp_connect(?SSH_DEFAULT_PORT, []), 
    {error, not_passive_mode} = ssh:daemon(Sock),
    gen_tcp:close(Sock).

%%--------------------------------------------------------------------
small_cat(Config) when is_list(Config) ->
    ConnectionRef = ssh_test_lib:connect(?SSH_DEFAULT_PORT, []),
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
send_after_exit(Config) when is_list(Config) ->
    ConnectionRef = ssh_test_lib:connect(?SSH_DEFAULT_PORT, []),
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
	{ssh_cm, ConnectionRef, {exit_status, ChannelId0, ExitStatus}} when ExitStatus=/=0 ->
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
encode_decode_pty_opts(_Config) ->
    Tags =
        [vintr, vquit, verase, vkill, veof, veol, veol2, vstart, vstop, vsusp, vdsusp,
         vreprint, vwerase, vlnext, vflush, vswtch, vstatus, vdiscard, ignpar, parmrk,
         inpck, istrip, inlcr, igncr, icrnl, iuclc, ixon, ixany, ixoff, imaxbel, isig,
         icanon, xcase, echo, echoe, echok, echonl, noflsh, tostop, iexten, echoctl,
         echoke, pendin, opost, olcuc, onlcr, ocrnl, onocr, onlret, cs7, cs8, parenb,
         parodd, tty_op_ispeed, tty_op_ospeed],
    Opts =
        lists:zip(Tags,
                  lists:seq(1, length(Tags))),
    
    case ssh_connection:encode_pty_opts(Opts) of
        Bin when is_binary(Bin) ->
            case ssh_connection:decode_pty_opts(Bin) of
                Opts ->
                    ok;
                Other ->
                    ct:log("Expected ~p~nGot ~p~nBin = ~p",[Opts,Other,Bin]),
                    ct:fail("Not the same",[])
            end;
        Other ->
            ct:log("encode -> ~p",[Other]),
            ct:fail("Encode failed",[])
    end.

%%--------------------------------------------------------------------
ptty_alloc_default(Config) when is_list(Config) ->
    ConnectionRef = ssh_test_lib:connect(?SSH_DEFAULT_PORT, []),
    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    Expect = case proplists:get_value(ptty_supported, Config) of
                 true -> success;
                 false -> failure
             end,
    Expect = ssh_connection:ptty_alloc(ConnectionRef, ChannelId, []),
    ssh:close(ConnectionRef).

%%--------------------------------------------------------------------
ptty_alloc(Config) when is_list(Config) ->
    ConnectionRef = ssh_test_lib:connect(?SSH_DEFAULT_PORT, []),
    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    Expect = case proplists:get_value(ptty_supported, Config) of
                 true -> success;
                 false -> failure
             end,
    Expect = ssh_connection:ptty_alloc(ConnectionRef, ChannelId, 
                                       [{term, os:getenv("TERM", ?DEFAULT_TERMINAL)}, {width, 70}, {height, 20}]),
    ssh:close(ConnectionRef).


%%--------------------------------------------------------------------
ptty_alloc_pixel(Config) when is_list(Config) ->
    ConnectionRef = ssh_test_lib:connect(?SSH_DEFAULT_PORT, []),
    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    Expect = case proplists:get_value(ptty_supported, Config) of
                 true -> success;
                 false -> failure
             end,
    Expect = ssh_connection:ptty_alloc(ConnectionRef, ChannelId, 
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
					      ct:log("~p:~p unexpected: ~p", [?MODULE,?LINE,Other]),
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
    test_shell_is_enabled(ConnectionRef, <<"Enter command">>), % No pty alloc by erl client
    test_exec_is_disabled(ConnectionRef),
    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
new_shell_dumb_term(Config) when is_list(Config) ->
    new_shell_helper(#{term => "dumb",
                       cmds => ["one_atom_please.\n",
                                "\^R" % attempt to trigger history search
                               ],
                       exp_output =>
                           [<<"Enter command\r\n">>,
                            <<"1> ">>,
                            <<"one_atom_please.\r\n">>,
                            <<"{simple_eval,one_atom_please}\r\n">>,
                            <<"2> ">>],
                       unexp_output =>
                           [<<"\e[;1;4msearch:\e[0m ">>]},
                    Config).

%%--------------------------------------------------------------------
new_shell_xterm_term(Config) when is_list(Config) ->
    new_shell_helper(#{term => "xterm",
                       cmds => ["one_atom_please.\n",
                                "\^R" % attempt to trigger history search
                               ],
                       exp_output =>
                           [<<"Enter command\r\n">>,
                            <<"1> ">>,
                            <<"one_atom_please.\r\n\e[1022D\e[1B">>,
                            <<"{simple_eval,one_atom_please}\r\n">>,
                            <<"2> ">>,
                            <<"\e[3D\e[J">>,
                            <<"\e[;1;4msearch:\e[0m ">>,
                            <<"\r\n  one_atom_please.">>]},
                    Config).

new_shell_helper(#{term := Term, cmds := Cmds,
                   exp_output := ExpectedOutput} = Settings, Config) ->
    UnexpectedOutput = maps:get(unexp_output, Settings, []),
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
                                             {subsystems, []},
                                             {keepalive, true},
                                             {nodelay, true},
                                             {shell, fun(U, H) ->
                                                             start_our_shell2(U, H)
                                                     end}
                                            ]),
    ConnectionRef = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
						      {user, "foo"},
						      {password, "morot"},
						      {user_dir, UserDir}]),
    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    success =
        ssh_connection:ptty_alloc(ConnectionRef, ChannelId,
                                  [{term, Term}, {hight, 24}, {width,1023}],
                                  infinity),
    ok = ssh_connection:shell(ConnectionRef,ChannelId),
    [ssh_connection:send(ConnectionRef, ChannelId, C) || C <- Cmds],
    GetTuple = fun(Bin) -> {ssh_cm, ConnectionRef, {data,ChannelId,0,Bin}} end,
    Msgs = [GetTuple(B) || B <- ExpectedOutput],
    expected = ssh_test_lib:receive_exec_result(Msgs),
    UnexpectedMsgs = [GetTuple(C) || C <- UnexpectedOutput],
    flush_msgs(UnexpectedMsgs),

    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%-------------------------------------------------------------------
start_shell_pty(Config) when is_list(Config) ->
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
    test_shell_is_enabled(ConnectionRef, <<"Enter command\r\n">>, [{pty_opts,[{onlcr,1}]}]), % alloc pty 
    test_exec_is_disabled(ConnectionRef),
    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).


%%--------------------------------------------------------------------
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
    test_shell_is_enabled(ConnectionRef),
    test_exec_is_enabled(ConnectionRef,  "testing",  <<"echo testing\r\n">>),
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
    test_shell_is_enabled(ConnectionRef),
    test_exec_is_enabled(ConnectionRef),
    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

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
    test_exec_is_disabled(ConnectionRef),
    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
exec_disabled(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
                                             {exec, disabled}
                                            ]),
    ConnectionRef = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
						      {user, "foo"},
						      {password, "morot"},
						      {user_interaction, true},
						      {user_dir, UserDir}
                                                     ]),
    test_shell_is_enabled(ConnectionRef),
    test_exec_is_disabled(ConnectionRef),
    ssh:stop_daemon(Pid).


exec_shell_disabled(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
                                             {shell, disabled}
                                            ]),
    ConnectionRef = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
						      {user, "foo"},
						      {password, "morot"},
						      {user_interaction, true},
						      {user_dir, UserDir}
                                                     ]),
    test_shell_is_disabled(ConnectionRef),
    test_exec_is_enabled(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
start_shell_exec_fun(Config) ->
    do_start_shell_exec_fun(fun(Cmd) ->
                                    spawn(fun() ->
                                                  io:format("echo ~s\n", [Cmd])
                                          end)
                            end,
                            "testing", <<"echo testing\n">>, 0,
                            Config).

start_shell_exec_fun2(Config) ->
    do_start_shell_exec_fun(fun(Cmd, User) ->
                                    spawn(fun() ->
                                                  io:format("echo ~s ~s\n",[User,Cmd])
                                          end)
                            end,
                            "testing", <<"echo foo testing\n">>, 0,
                            Config).

start_shell_exec_fun3(Config) ->
    do_start_shell_exec_fun(fun(Cmd, User, _PeerAddr) ->
                                    spawn(fun() ->
                                                  io:format("echo ~s ~s\n",[User,Cmd])
                                          end)
                            end,
                            "testing", <<"echo foo testing\n">>, 0,
                            Config).

start_shell_exec_direct_fun(Config) ->
    do_start_shell_exec_fun({direct, fun(Cmd) -> {ok, io_lib:format("echo ~s~n",[Cmd])} end},
                            "testing", <<"echo testing\n">>, 0,
                            Config).

start_shell_exec_direct_fun2(Config) ->
    do_start_shell_exec_fun({direct, fun(Cmd,User) -> {ok, io_lib:format("echo ~s ~s",[User,Cmd])} end},
                            "testing", <<"echo foo testing">>, 0,
                            Config).

start_shell_exec_direct_fun3(Config) ->
    do_start_shell_exec_fun({direct, fun(Cmd,User,_PeerAddr) -> {ok, io_lib:format("echo ~s ~s",[User,Cmd])} end},
                            "testing", <<"echo foo testing">>, 0,
                            Config).

start_shell_exec_direct_fun1_error(Config) ->
    do_start_shell_exec_fun({direct, fun(_Cmd) -> {error, {bad}} end},
                            "testing", <<"**Error** {bad}">>, 1,
                            Config).

start_shell_exec_direct_fun1_error_type(Config) ->
    do_start_shell_exec_fun({direct, fun(_Cmd) -> very_bad end},
                            "testing", <<"**Error** Bad exec fun in server. Invalid return value: very_bad">>, 1,
                            Config).

start_exec_direct_fun1_read_write(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {exec, {direct,fun read_write_loop/1}}]),

    C = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
                                          {user, "foo"},
                                          {password, "morot"},
                                          {user_interaction, true},
                                          {user_dir, UserDir}]),

    {ok, Ch} = ssh_connection:session_channel(C, infinity),

    success = ssh_connection:exec(C, Ch, "> ", infinity),
    ssh_test_lib:receive_exec_result_or_fail({ssh_cm,C,{data,Ch,0,<<"Tiny read/write test\n">>}}),

    ssh_test_lib:receive_exec_result_or_fail({ssh_cm,C,{data,Ch,0,<<"1> ">>}}),
    ok = ssh_connection:send(C, Ch, "hej.\n", 5000),
    ssh_test_lib:receive_exec_result_or_fail({ssh_cm,C,{data,Ch,0,<<"{simple_eval,hej}\n">>}}),

    ssh_test_lib:receive_exec_result_or_fail({ssh_cm,C,{data,Ch,0,<<"2> ">>}}),
    ok = ssh_connection:send(C, Ch, "quit.\n", 5000),
    ssh_test_lib:receive_exec_result_or_fail({ssh_cm,C,{data,Ch,0,<<"{1,inputs}">>}}),
    receive
        {ssh_cm,C,{exit_status,Ch,0}} -> ok
    after 5000 -> go_on
    end,
    receive
        {ssh_cm,C,{eof,Ch}} -> ok
    after 5000 -> go_on
    end,
    receive
        {ssh_cm,C,{closed,Ch}} -> ok
    after 5000 -> go_on
    end,
    receive
        X -> ct:fail("remaining messages"),
             ct:log("remaining message: ~p",[X])
    after 0 -> go_on
    end,
    ssh:stop_daemon(Pid).


start_exec_direct_fun1_read_write_advanced(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {exec, {direct,fun read_write_loop/1}}]),

    C = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
                                          {user, "foo"},
                                          {password, "morot"},
                                          {user_interaction, true},
                                          {user_dir, UserDir}]),

    {ok, Ch} = ssh_connection:session_channel(C, infinity),

    success = ssh_connection:exec(C, Ch, "> ", infinity),
    ssh_test_lib:receive_exec_result_or_fail({ssh_cm,C,{data,Ch,0,<<"Tiny read/write test\n">>}}),

    ssh_test_lib:receive_exec_result_or_fail({ssh_cm,C,{data,Ch,0,<<"1> ">>}}),
    ok = ssh_connection:send(C, Ch, "hej.\n", 5000),
    ssh_test_lib:receive_exec_result_or_fail({ssh_cm,C,{data,Ch,0,<<"{simple_eval,hej}\n">>}}),

    ssh_test_lib:receive_exec_result_or_fail({ssh_cm,C,{data,Ch,0,<<"2> ">>}}),
    ok = ssh_connection:send(C, Ch, "'Hi ", 5000),
    ok = ssh_connection:send(C, Ch, "there", 5000),
    ok = ssh_connection:send(C, Ch, "'", 5000),
    ok = ssh_connection:send(C, Ch, ".\n", 5000),
    ssh_test_lib:receive_exec_result_or_fail({ssh_cm,C,{data,Ch,0,<<"{simple_eval,'Hi there'}\n">>}}),
    ssh_test_lib:receive_exec_result_or_fail({ssh_cm,C,{data,Ch,0,<<"3> ">>}}),
    ok = ssh_connection:send(C, Ch, "bad_input.\n", 5000),
    ssh_test_lib:receive_exec_result_or_fail({ssh_cm,C,{data,Ch,1,<<"**Error** {bad_input,3}">>}}),
    receive
        {ssh_cm,C,{exit_status,Ch,255}} -> ok
    after 5000 -> go_on
    end,
    receive
        {ssh_cm,C,{eof,Ch}} -> ok
    after 5000 -> go_on
    end,
    receive
        {ssh_cm,C,{closed,Ch}} -> ok
    after 5000 -> go_on
    end,
    receive
        X -> ct:fail("remaining messages"),
             ct:log("remaining message: ~p",[X])
    after 0 -> go_on
    end,
    ssh:stop_daemon(Pid).



    
%% A tiny read-write loop ended by a 'quit.\n'
read_write_loop(Prompt) ->
    io:format("Tiny read/write test~n", []),
    read_write_loop1(Prompt, 1).

read_write_loop1(Prompt, N) ->
    case io:read(lists:concat([N,Prompt])) of
        {ok, quit} ->
            {ok, {N-1, inputs}};
        {ok, bad_input} ->
            {error, {bad_input,N}};
        {ok,Inp} ->
            io:format("~p~n",[simple_eval(Inp)]),
            read_write_loop1(Prompt, N+1)
    end.
    
simple_eval(Inp) -> {simple_eval,Inp}.


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
                    ct:log("Received other:~n~p~nExpected: ~p~n",
                           [Other, {ssh_cm, ConnectionRef, {data, '_ChannelId', ExpectType, Expect}} ]),
                    ct:fail("Unexpected response")
            after 0 ->
                    ct:fail("Exec Timeout")
            end
    end,

    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
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
                                            {save_accepted_host, false},
					    {user, "foo"},
					    {password, "morot"},
					    {user_interaction, true},
					    {user_dir, UserDir}]),

    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),

    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "testing", infinity),

    receive
	{ssh_cm, ConnectionRef, {data, _ChannelId, 0, <<"echo testing\n">>}} ->
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

    %% Listening tcp socket at the client side
    {ok,Sl} = gen_tcp:listen(0, [{active,false}]),
    {ok,{_IP,Port}} = inet:sockname(Sl),	% _IP is likely to be {0,0,0,0}. Win don't like...

    %% A server tcp-contects to the listening socket and starts an ssh daemon
    spawn_link(fun() ->
		       {ok,Ss} = ssh_test_lib:gen_tcp_connect(Port, [{active,false}]),
		       {ok, _Pid} = ssh:daemon(Ss, [{system_dir, SysDir},
						    {user_dir, UserDir},
						    {password, "morot"},
						    {exec, fun ssh_exec_echo/1}])
	       end),

    %% The client accepts the tcp connection from the server and ssh-connects to it
    {ok,Sc} = gen_tcp:accept(Sl),
    {ok,ConnectionRef} = ssh:connect(Sc, [{silently_accept_hosts, true},
                                          {save_accepted_host, false},
					  {user, "foo"},
					  {password, "morot"},
					  {user_interaction, true},
					  {user_dir, UserDir}]),

    %% And runs some commands
    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),

    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "testing", infinity),

    receive
	{ssh_cm, ConnectionRef, {data, _ChannelId, 0, <<"echo testing\n">>}} ->
	    ok
    after 5000 ->
	    ct:fail("Exec Timeout")
    end,

    ssh:close(ConnectionRef).

%%--------------------------------------------------------------------
start_shell_sock_daemon_exec_multi(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),

    NumConcurent = 5,

    %% Listening tcp socket at the client side
    {ok,Sl} = gen_tcp:listen(0, [{active,false}]),
    {ok,{_IP,Port}} = inet:sockname(Sl),	% _IP is likely to be {0,0,0,0}. Win don't like...

    DaemonOpts = [{system_dir, SysDir},
                  {user_dir, UserDir},
                  {password, "morot"},
                  {exec, fun ssh_exec_echo/1}],

    %% Servers tcp-contects to the listening socket and starts an ssh daemon
    Pids =
        [spawn_link(fun() ->
                            {ok,Ss} = ssh_test_lib:gen_tcp_connect(Port, [{active,false}]),
                            {ok, _Pid} = ssh:daemon(Ss, DaemonOpts)
                    end)
         || _ <- lists:seq(1,NumConcurent)],
    ct:log("~p:~p: ~p daemons spawned!", [?MODULE,?LINE,length(Pids)]),

    %% The client accepts the tcp connections from the servers and ssh-connects to it
    ConnectionRefs =
        [begin
             {ok,Sc} = gen_tcp:accept(Sl),
             {ok,ConnectionRef} = ssh:connect(Sc, [{silently_accept_hosts, true},
                                                   {save_accepted_host, false},
                                                   {user, "foo"},
                                                   {password, "morot"},
                                                   {user_interaction, true},
                                                   {user_dir, UserDir}]),
             ConnectionRef
         end || _Pid <- Pids],
    ct:log("~p:~p: ~p connections accepted!", [?MODULE,?LINE,length(ConnectionRefs)]),

    %% And runs some exec commands
    Parent = self(),
    ClientPids =
        lists:map(
          fun(ConnectionRef) ->
                  spawn_link(
                    fun() ->
                            {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),
                            success = ssh_connection:exec(ConnectionRef, ChannelId0, "testing", infinity),
                            ct:log("~p:~p: exec on connection ~p", [?MODULE,?LINE,ConnectionRef]),
                            receive
                                {ssh_cm, ConnectionRef, {data, _ChannelId, 0, <<"echo testing\n">>}} ->
                                    Parent ! {answer_received,self()},
                                    ct:log("~p:~p: received result on connection ~p", [?MODULE,?LINE,ConnectionRef])
                            after 5000 ->
                                    ct:fail("Exec Timeout")
                            end
                    end)
          end, ConnectionRefs),
    ct:log("~p:~p: ~p clients spawned!", [?MODULE,?LINE,length(ClientPids)]),

    lists:foreach(fun(P) ->
                          receive
                              {answer_received,P} -> ok
                          end
                  end, ClientPids),
    ct:log("~p:~p: All answers received!", [?MODULE,?LINE]),

    lists:foreach(fun ssh:close/1, ConnectionRefs).

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

kex_error(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    [Kex1,Kex2|_] = proplists:get_value(kex, ssh:default_algorithms()),
    {_Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
                                              {user_dir, UserDir},
                                              {password, "morot"},
                                              {preferred_algorithms,[{kex,[Kex1]}]}
                                             ]),
    Ref = make_ref(),
    ok = ssh_log_h:add_fun(kex_error,
                           fun(#{msg:={report,#{format:=Fmt,args:=As,label:={error_logger,_}}}}, Pid) ->
                                   true = (erlang:process_info(Pid) =/= undefined), % remove handler if we are dead
                                   Pid ! {Ref, lists:flatten(io_lib:format(Fmt,As))};
                              (_,Pid) ->
                                   true = (erlang:process_info(Pid) =/= undefined), % remove handler if we are dead
                                   ok % Other msg
                           end,
                           self()),
    try
        ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
                                          {user, "foo"},
                                          {password, "morot"},
                                          {user_interaction, false},
                                          {user_dir, UserDir},
                                          {preferred_algorithms,[{kex,[Kex2]}]}
                                         ])
    of
        _ ->
            ok = logger:remove_handler(kex_error),
            ct:fail("expected failure", [])
    catch
        error:{badmatch,{error,"Key exchange failed"}} ->
            %% ok
            receive
                {Ref, ErrMsgTxt} ->
                    ok = logger:remove_handler(kex_error),
                    ct:log("ErrMsgTxt = ~n~s", [ErrMsgTxt]),
                    Lines = lists:map(fun string:trim/1, string:tokens(ErrMsgTxt, "\n")),
                    OK = (lists:all(fun(S) -> lists:member(S,Lines) end,
                                    ["Disconnects with code = 3 [RFC4253 11.1]: Key exchange failed",
                                     "Details:",
                                     "No common key exchange algorithm,",
                                     "we have:",
                                     "peer have:"]) andalso
                          string:find(ErrMsgTxt, atom_to_list(Kex1)) =/= nomatch andalso
                          string:find(ErrMsgTxt, atom_to_list(Kex2)) =/= nomatch),
                    case OK of
                        true ->
                            ok;
                        false ->
                            ct:fail("unexpected error text msg", [])
                    end
            after 20000 ->
                    ok = logger:remove_handler(kex_error),
                    ct:fail("timeout", [])
            end;

        error:{badmatch,{error,_}} ->
            ok = logger:remove_handler(kex_error),
            ct:fail("unexpected error msg", [])
    end.

stop_listener(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    %% to make sure we don't use public-key-auth
    UserDir = filename:join(PrivDir, nopubkey),
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),

    {Pid0, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					      {user_dir, UserDir},
					      {password, "morot"},
					      {exec, fun ssh_exec_echo/1}]),

    ConnectionRef0 = ssh_test_lib:connect(Host, Port,
                                          [{silently_accept_hosts, true},
                                           {user, "foo"},
                                           {password, "morot"},
                                           {user_interaction, false},
                                           {user_dir, UserDir}]),
    {ok, ChannelId0} =
        ssh_connection:session_channel(ConnectionRef0, infinity),

    ssh:stop_listener(Host, Port),

    {error, _} = ssh:connect(Host, Port, [{silently_accept_hosts, true},
                                          {save_accepted_host, false},
                                          {save_accepted_host, false},
					  {user, "foo"},
					  {password, "morot"},
					  {user_interaction, true},
					  {user_dir, UserDir}]),
    success = ssh_connection:exec(ConnectionRef0, ChannelId0,
				  "testing", infinity),
    receive
	{ssh_cm, ConnectionRef0,
         {data, ChannelId0, 0, <<"echo testing\n">>}} ->
	    ok
    after 5000 ->
	    ct:fail("Exec Timeout")
    end,

    case ssh_test_lib:daemon(Port, [{system_dir, SysDir},
                                    {user_dir, UserDir},
                                    {password, "potatis"},
                                    {exec, fun ssh_exec_echo/1}]) of
	{Pid1, Host, Port} ->
	    ConnectionRef1 =
                ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
                                                  {user, "foo"},
                                                  {password, "potatis"},
                                                  {user_interaction, true},
                                                  {user_dir, UserDir}]),
	    {error, _} = ssh:connect(Host, Port, [{silently_accept_hosts, true},
                                                  {save_accepted_host, false},
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

no_sensitive_leak(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),

    %% Save old, and set new log level:
    #{level := Level} = logger:get_primary_config(),
    logger:set_primary_config(level, info),
    %% Define a collect fun:
    Ref = make_ref(),
    Collect = 
        fun G(N,Nf,Nt) ->
                receive
                    {Ref, false, _} ->
                        G(N+1, Nf+1, Nt);
                    {Ref, true, R} ->
                        ct:log("Report leaked:~n~p", [R]),
                        G(N+1, Nf, Nt+1)
                after 100 ->
                        {N, Nf, Nt}
                end
        end,
 
    %% Install the test handler:
    Hname = no_sensitive_leak,
    ok = ssh_log_h:add_fun(Hname,
                           fun(#{msg := {report,#{report := Rep}}}, Pid) ->
                                   true = (erlang:process_info(Pid, status) =/= undefined), % remove handler if we are dead
                                   Pid ! {Ref,ssh_log_h:sensitive_in_opt(Rep),Rep};
                              (_,Pid) ->
                                   true = (erlang:process_info(Pid, status) =/= undefined), % remove handler if we are dead
                                   ok
                           end,
                           self()),

    {_Pid0, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					      {user_dir, UserDir},
					      {password, "morot"},
					      {exec, fun ssh_exec_echo/1}]),

    _ConnectionRef0 = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
						       {user, "foo"},
						       {password, "morot"},
						       {user_interaction, true},
						       {user_dir, UserDir}]),
    %% Kill acceptor to make it restart:
    [true|_] =
        [exit(Pacc,kill) || {{ssh_system_sup,_},P1,supervisor,_} <- supervisor:which_children(sshd_sup),
                            {{ssh_acceptor_sup,_},P2,supervisor,_} <- supervisor:which_children(P1),
                            {{ssh_acceptor_sup,_},Pacc,worker,_} <- supervisor:which_children(P2)],
    
    %% Remove the test handler and reset the logger level:
    timer:sleep(500),
    logger:remove_handler(Hname),
    logger:set_primary_config(Level),

    case Collect(0, 0, 0) of
        {0, 0,   0} -> ct:fail("Logging failed, line = ~p", [?LINE]);
        {_, _,   0} -> ok;
        {_, _, Nt0} -> ct:fail("Leak in ~p cases!", [Nt0])
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
flush_msgs() ->
    flush_msgs([]).

flush_msgs(Unexpected) ->
    receive
        M ->
            case lists:member(M, Unexpected) of
                true ->
                    ct:fail("Unexpected message found:  ~p", [M]);
                _ ->
                    flush_msgs()
            end
    after
        500 -> ok
    end.

%%--------------------------------------------------------------------
test_shell_is_disabled(ConnectionRef) ->
    test_shell_is_disabled(ConnectionRef, <<"Prohibited.">>, <<"Eshell V">>).

test_shell_is_disabled(ConnectionRef, Expect, NotExpect) ->
    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    ok = ssh_connection:shell(ConnectionRef, ChannelId),
    ExpSz = size(Expect),
    NotExpSz = size(NotExpect),
    receive
        {ssh_cm, ConnectionRef, {data, ChannelId, 1, <<Expect:ExpSz/binary, _/binary>>}} ->
            flush_msgs();

        {ssh_cm, ConnectionRef, {data, ChannelId, 0, <<NotExpect:NotExpSz/binary, _/binary>>}} ->
            ct:fail("Could start disabled shell!");

        R ->
            ct:log("~p:~p Got unexpected ~p~nExpect: ~p~n",
                   [?MODULE,?LINE,R, {ssh_cm, ConnectionRef, {data, ChannelId, '0|1', Expect}} ]),
            ct:fail("Strange shell response")

    after 5000 ->
            ct:fail("Shell Timeout")
    end.

%%--------------------------------------------------------------------
test_exec_is_disabled(ConnectionRef) ->
    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId, "1+2.", infinity),
    receive
        {ssh_cm, ConnectionRef, {data,ChannelId,1,<<"Prohibited.">>}} ->
            flush_msgs();
        R ->
            ct:log("~p:~p Got unexpected ~p~nExpect: ~p~n",
                   [?MODULE,?LINE,R, {ssh_cm, ConnectionRef, {data,ChannelId,1,<<"Prohibited.">>}} ]),
            ct:fail("Could exec erlang term although non-erlang shell")
    after 5000 ->
            ct:fail("Exec Timeout")
    end.

%%--------------------------------------------------------------------
test_shell_is_enabled(ConnectionRef) ->
    test_shell_is_enabled(ConnectionRef, <<"Eshell V">>).

test_shell_is_enabled(ConnectionRef, Expect) ->
    test_shell_is_enabled(ConnectionRef, Expect, []).

test_shell_is_enabled(ConnectionRef, Expect, PtyOpts) ->
    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    case PtyOpts of
        [] ->
            no_alloc;
        _ ->
            success = ssh_connection:ptty_alloc(ConnectionRef, ChannelId, PtyOpts)
    end,
    ok = ssh_connection:shell(ConnectionRef,ChannelId),

    ExpSz = size(Expect),
    receive
	{ssh_cm,ConnectionRef, {data, ChannelId, 0, <<Expect:ExpSz/binary, _/binary>>}} ->
	    flush_msgs();

        R ->
            ct:log("~p:~p Got unexpected ~p~nExpect: ~p~n",
                   [?MODULE,?LINE,R, {ssh_cm, ConnectionRef, {data, ChannelId, 0, Expect}} ]),
            ct:fail("Strange shell response")

    after 5000 ->
	    ct:fail("CLI Timeout")
    end.


test_exec_is_enabled(ConnectionRef) ->
    test_exec_is_enabled(ConnectionRef, "1+2.", <<"3">>).

test_exec_is_enabled(ConnectionRef, Exec, Expect) ->
    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId, Exec, infinity),
    ExpSz = size(Expect),
    receive
        {ssh_cm, ConnectionRef, {data, ChannelId, 0, <<Expect:ExpSz/binary, _/binary>>}} = R ->
            ct:log("~p:~p Got expected ~p",[?MODULE,?LINE,R]);
        Other ->
            ct:log("~p:~p Got unexpected ~p~nExpect: ~p~n",
                   [?MODULE,?LINE, Other, {ssh_cm, ConnectionRef, {data, ChannelId, 0, Expect}} ])
    after 5000 ->
            {fail,"Exec Timeout"}
    end.

%%%----------------------------------------------------------------
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

start_our_shell2(_User, _Peer) ->
    spawn(fun() ->
                  io:format("Enter command\n"),
                  read_write_loop1("> ", 1)
          end).

ssh_exec_echo(Cmd) ->
    spawn(fun() ->
                  io:format("echo ~s\n", [Cmd])
          end).

ssh_exec_echo(Cmd, User) ->
    spawn(fun() ->
                  io:format("echo ~s ~s\n",[User,Cmd])
          end).
