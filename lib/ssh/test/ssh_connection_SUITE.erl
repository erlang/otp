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
-module(ssh_connection_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("ssh/src/ssh_connect.hrl").

-compile(export_all).

-define(SSH_DEFAULT_PORT, 22).
-define(EXEC_TIMEOUT, 10000).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

%% suite() ->
%%     [{ct_hooks,[ts_install_cth]}].

all() ->
    [
     {group, openssh},
     interrupted_send,
     start_shell,
     start_shell_exec,
     start_shell_exec_fun,
     gracefull_invalid_version,
     gracefull_invalid_start,
     gracefull_invalid_long_start,
     gracefull_invalid_long_start_no_nl,
     stop_listener,
     start_subsystem_on_closed_channel
    ].
groups() ->
    [{openssh, [], payload() ++ ptty()}].

payload() ->
    [simple_exec,
     small_cat,
     big_cat,
     send_after_exit].

ptty() ->
    [ptty_alloc_default,
     ptty_alloc,
     ptty_alloc_pixel].

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
    crypto:stop().

%%--------------------------------------------------------------------
init_per_group(openssh, Config) ->
    case gen_tcp:connect("localhost", 22, []) of
	{error,econnrefused} ->
	    {skip,"No openssh deamon"};
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
    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "echo testing", infinity),
    %% receive response to input
    receive
	{ssh_cm, ConnectionRef, {data, ChannelId0, 0, <<"testing\n">>}} ->
	    ok
    end,

    %% receive close messages
    receive
	{ssh_cm, ConnectionRef, {eof, ChannelId0}} ->
	    ok
    end,
    receive
	{ssh_cm, ConnectionRef, {exit_status, ChannelId0, 0}} ->
	    ok
    end,
    receive
	{ssh_cm, ConnectionRef,{closed, ChannelId0}} ->
	    ok
    end.

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
    end,

    %% receive close messages
    receive
	{ssh_cm, ConnectionRef, {eof, ChannelId0}} ->
	    ok
    end,
    receive
	{ssh_cm, ConnectionRef, {exit_status, ChannelId0, 0}} ->
	    ok
    end,
    receive
	{ssh_cm, ConnectionRef,{closed, ChannelId0}} ->
	    ok
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

    ct:pal("sending ~p byte binary~n",[size(Data)]),
    ok = ssh_connection:send(ConnectionRef, ChannelId0, Data, 10000),
    ok = ssh_connection:send_eof(ConnectionRef, ChannelId0),

    %% collect echoed data until eof
    case big_cat_rx(ConnectionRef, ChannelId0) of
	{ok, Data} ->
	    ok;
	{ok, Other} ->
	    case size(Data) =:= size(Other) of
		true ->
		    ct:pal("received and sent data are same"
			   "size but do not match~n",[]);
		false ->
		    ct:pal("sent ~p but only received ~p~n",
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
    end,
    receive
	{ssh_cm, ConnectionRef,{closed, ChannelId0}} ->
	    ok
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
    end,
    receive
	{ssh_cm, ConnectionRef, {exit_status, ChannelId0, _ExitStatus}} ->
	    ok
    end,
    receive
	{ssh_cm, ConnectionRef,{closed, ChannelId0}} ->
	    ok
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

interrupted_send() ->
    [{doc, "Use a subsystem that echos n char and then sends eof to cause a channel exit partway through a large send."}].

interrupted_send(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {subsystems, [{"echo_n", {ssh_echo_server, [4000000]}}]}]),

    ConnectionRef = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
						      {user, "foo"},
						      {password, "morot"},
						      {user_interaction, false},
						      {user_dir, UserDir}]),

    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),

    success = ssh_connection:subsystem(ConnectionRef, ChannelId, "echo_n", infinity),

    %% build 10MB binary
    Data = << <<X:32>> || X <- lists:seq(1,2500000)>>,

    %% expect remote end to send us 4MB back
    <<ExpectedData:4000000/binary, _/binary>> = Data,

    %% pre-adjust receive window so the other end doesn't block
    ssh_connection:adjust_window(ConnectionRef, ChannelId, size(ExpectedData) + 1),

    case ssh_connection:send(ConnectionRef, ChannelId, Data, 10000) of
	{error, closed} ->
	    ok;
	Msg ->
	    ct:fail({expected,{error,closed}, got, Msg})
    end,
    receive_data(ExpectedData, ConnectionRef, ChannelId),
    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
start_shell() ->
    [{doc, "Start a shell"}].

start_shell(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),
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
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {exec, {?MODULE,ssh_exec,[]}} ]),

    ConnectionRef = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
						      {user, "foo"},
						      {password, "morot"},
						      {user_interaction, true},
						      {user_dir, UserDir}]),

    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),

    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "testing", infinity),
    receive
	{ssh_cm, ConnectionRef, {data, _ChannelId, 0, <<"testing\r\n">>}} ->
	    ok
    after 5000 ->
	    ct:fail("Exec Timeout")
    end,

    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
start_shell_exec_fun() ->
    [{doc, "start shell to exec command"}].

start_shell_exec_fun(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {exec, fun ssh_exec/1}]),

    ConnectionRef = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
						      {user, "foo"},
						      {password, "morot"},
						      {user_interaction, true},
						      {user_dir, UserDir}]),

    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),

    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "testing", infinity),

    receive
	{ssh_cm, ConnectionRef, {data, _ChannelId, 0, <<"testing\r\n">>}} ->
	    ok
    after 5000 ->
	    ct:fail("Exec Timeout")
    end,

    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------

gracefull_invalid_version(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),
    
    {_Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"}]),

    {ok, S} = gen_tcp:connect(Host, Port, []),
    ok = gen_tcp:send(S,  ["SSH-8.-1","\r\n"]),
    receive
	Verstring ->
	    ct:pal("Server version: ~p~n", [Verstring]),
	    receive
		{tcp_closed, S} ->
		    ok
	    end
    end.

gracefull_invalid_start(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),
    {_Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"}]),

    {ok, S} = gen_tcp:connect(Host, Port, []),
    ok = gen_tcp:send(S,  ["foobar","\r\n"]),
    receive
	Verstring ->
	    ct:pal("Server version: ~p~n", [Verstring]),
	    receive
		{tcp_closed, S} ->
		    ok
	    end
    end.

gracefull_invalid_long_start(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),
    {_Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"}]),

    {ok, S} = gen_tcp:connect(Host, Port, []),
    ok = gen_tcp:send(S, [lists:duplicate(257, $a), "\r\n"]),
    receive
	Verstring ->
	    ct:pal("Server version: ~p~n", [Verstring]),
	    receive
		{tcp_closed, S} ->
		    ok
	    end
    end.


gracefull_invalid_long_start_no_nl(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),
    {_Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"}]),

    {ok, S} = gen_tcp:connect(Host, Port, []),
    ok = gen_tcp:send(S, [lists:duplicate(257, $a), "\r\n"]),
    receive
	Verstring ->
	    ct:pal("Server version: ~p~n", [Verstring]),
	    receive
		{tcp_closed, S} ->
		    ok
	    end
    end.

stop_listener() ->
    [{doc, "start ssh daemon, setup connections, stop listener, restart listner"}].

stop_listener(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),

    {Pid0, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					      {user_dir, UserDir},
					      {password, "morot"},
					      {exec, fun ssh_exec/1}]),

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
	{ssh_cm, ConnectionRef0, {data, ChannelId0, 0, <<"testing\r\n">>}} ->
	    ok
    after 5000 ->
	    ct:fail("Exec Timeout")
    end,

    {ok, HostAddr} = inet:getaddr(Host, inet),
    case ssh_test_lib:daemon(HostAddr, Port, [{system_dir, SysDir},
							     {user_dir, UserDir},
							     {password, "potatis"},
							     {exec, fun ssh_exec/1}]) of
	{Pid1, HostAddr, Port} ->
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
	    ct:fail({unexpected, Error})
    end.

start_subsystem_on_closed_channel(Config) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {subsystems, [{"echo_n", {ssh_echo_server, [4000000]}}]}]),

    ConnectionRef = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
						      {user, "foo"},
						      {password, "morot"},
						      {user_interaction, false},
						      {user_dir, UserDir}]),

    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),

    ok = ssh_connection:close(ConnectionRef, ChannelId),

    {error, closed} = ssh_connection:subsystem(ConnectionRef, ChannelId, "echo_n", infinity),

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

receive_data(ExpectedData, ConnectionRef, ChannelId) ->
    ExpectedData = collect_data(ConnectionRef, ChannelId).

collect_data(ConnectionRef, ChannelId) ->
    collect_data(ConnectionRef, ChannelId, []).

collect_data(ConnectionRef, ChannelId, Acc) ->
    receive
	{ssh_cm, ConnectionRef, {data, ChannelId, 0, Data}} ->
	    collect_data(ConnectionRef, ChannelId, [Data | Acc]);
	{ssh_cm, ConnectionRef, {eof, ChannelId}} ->
	    iolist_to_binary(lists:reverse(Acc))
    after 5000 ->
	    timeout
    end.

%%%-------------------------------------------------------------------
%% This is taken from the ssh example code.
start_our_shell(_User, _Peer) ->
    spawn(fun() ->
		  io:format("Enter command\n")
		  %% Don't actually loop, just exit
          end).

ssh_exec(Cmd) ->
    spawn(fun() ->
		  io:format(Cmd ++ "\n")
          end).
