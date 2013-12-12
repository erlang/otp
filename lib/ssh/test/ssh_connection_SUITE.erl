%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2013. All Rights Reserved.
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
-module(ssh_connection_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

-define(SSH_DEFAULT_PORT, 22).
-define(EXEC_TIMEOUT, 10000).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() ->
    [
     {group, openssh_payload},
     interrupted_send
    ].
groups() ->
    [{openssh_payload, [], [simple_exec,
			    small_cat,
			    big_cat,
			    send_after_exit
			   ]}].
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    case catch crypto:start() of
	ok ->
	    Config;
	_Else ->
	    {skip, "Crypto could not be started!"}
    end.

end_per_suite(_Config) ->
    crypto:stop().

%%--------------------------------------------------------------------
init_per_group(openssh_payload, _Config) ->
    case gen_tcp:connect("localhost", 22, []) of
	{error,econnrefused} ->
	    {skip,"No openssh deamon"};
	{ok, Socket} ->
	    gen_tcp:close(Socket)
     end;
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    %% To make sure we start clean as it is not certain that
    %% end_per_testcase will be run!
    ssh:stop(),
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
