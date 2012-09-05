%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2012. All Rights Reserved.
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
-include("test_server_line.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(SSH_DEFAULT_PORT, 22).
-define(EXEC_TIMEOUT, 10000).

%% Test server callback functions
%%--------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Initialization before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    case catch crypto:start() of
	ok ->
	    case gen_tcp:connect("localhost", 22, []) of
		{error,econnrefused} ->
		    {skip,"No openssh deamon"};
		_ ->
		    Config
	    end;
	_Else ->
	    {skip,"Could not start crypto!"}
    end.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    crypto:stop(),
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config) -> Config
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initialization before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%% Description: Initialization before each test case
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    ssh:start(),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ssh:stop(),
    ok.

%%--------------------------------------------------------------------
%% Function: all(Clause) -> TestCases
%% Clause - atom() - suite | doc
%% TestCases - [Case]
%% Case - atom()
%%   Name of a test case.
%% Description: Returns a list of all test cases in this test suite
%%--------------------------------------------------------------------
all() ->
    case os:find_executable("ssh") of
	false ->
	    {skip, "openSSH not installed on host"};
	_ ->
	    [{group, erlang_client}
	     ]
    end.

groups() ->
	[{erlang_client, [], [
						  simple_exec,
						  small_cat,
						  big_cat,
						  send_after_exit,
						  interrupted_send
						 ]}].

init_per_group(erlang_server, Config) ->
    DataDir = ?config(data_dir, Config),
    UserDir = ?config(priv_dir, Config),
    ssh_test_lib:setup_dsa_known_host(DataDir, UserDir),
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(erlang_server, Config) ->
    UserDir = ?config(priv_dir, Config),
    ssh_test_lib:clean_dsa(UserDir),
    Config;
end_per_group(_, Config) ->
    Config.

%% TEST cases starts here.
%--------------------------------------------------------------------
simple_exec(doc) ->
    ["Simple openssh connectivity test for ssh_connection:exec"];

simple_exec(suite) ->
    [];

simple_exec(Config) when is_list(Config) ->
    ConnectionRef = ssh_test_lib:connect(?SSH_DEFAULT_PORT, [{silently_accept_hosts, true},
							     {user_interaction, false}]),
    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "echo testing", infinity),

	%% receive response to input
	receive
		{ssh_cm, ConnectionRef, {data, ChannelId0, 0, <<"testing\n">>}} -> ok
		after ?EXEC_TIMEOUT -> test_server:fail()
	end,

	%% receive close messages
	receive
		{ssh_cm, ConnectionRef, {eof, ChannelId0}} -> ok
		after ?EXEC_TIMEOUT -> test_server:fail()
	end,
	receive
		{ssh_cm, ConnectionRef, {exit_status, ChannelId0, 0}} -> ok
		after ?EXEC_TIMEOUT -> test_server:fail()
	end,
	receive
		{ssh_cm, ConnectionRef,{closed, ChannelId0}} -> ok
		after ?EXEC_TIMEOUT -> test_server:fail()
	end.


%--------------------------------------------------------------------
small_cat(doc) ->
    ["Use 'cat' to echo small data block back to us."];

small_cat(suite) ->
    [];

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
		{ssh_cm, ConnectionRef, {data, ChannelId0, 0, Data}} -> ok
		after ?EXEC_TIMEOUT -> test_server:fail()
	end,

	%% receive close messages
	receive
		{ssh_cm, ConnectionRef, {eof, ChannelId0}} -> ok
		after ?EXEC_TIMEOUT -> test_server:fail()
	end,
	receive
		{ssh_cm, ConnectionRef, {exit_status, ChannelId0, 0}} -> ok
		after ?EXEC_TIMEOUT -> test_server:fail()
	end,
	receive
		{ssh_cm, ConnectionRef,{closed, ChannelId0}} -> ok
		after ?EXEC_TIMEOUT -> test_server:fail()
	end.

%--------------------------------------------------------------------
big_cat(doc) ->
    ["Use 'cat' to echo large data block back to us."];

big_cat(suite) ->
    [];

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

	test_server:format("sending ~p byte binary~n",[size(Data)]),
	ok = ssh_connection:send(ConnectionRef, ChannelId0, Data, 10000),
	%timer:sleep(3000),
	ok = ssh_connection:send_eof(ConnectionRef, ChannelId0),

	%% collect echoed data until eof
	case big_cat_rx(ConnectionRef, ChannelId0) of
		{ok, Data} -> ok;
		{ok, Other} ->
			case size(Data) =:= size(Other) of
				true ->
					test_server:format("received and sent data are same size but do not match~n",[]);
				false ->
					test_server:format("sent ~p but only received ~p~n",[size(Data), size(Other)])
			end,
			test_server:fail(receive_data_mismatch);
		Else ->
			test_server:fail(Else)
	end,

	%% receive close messages (eof already consumed)
	receive
		{ssh_cm, ConnectionRef, {exit_status, ChannelId0, 0}} -> ok
		after ?EXEC_TIMEOUT -> test_server:fail()
	end,
	receive
		{ssh_cm, ConnectionRef,{closed, ChannelId0}} -> ok
		after ?EXEC_TIMEOUT -> test_server:fail()
	end.

big_cat_rx(ConnectionRef, ChannelId) ->
	big_cat_rx(ConnectionRef, ChannelId, []).

big_cat_rx(ConnectionRef, ChannelId, Acc) ->
	receive
		{ssh_cm, ConnectionRef, {data, ChannelId, 0, Data}} ->
			%% ssh_connection:adjust_window(ConnectionRef, ChannelId, size(Data)),  % window was pre-adjusted, don't adjust again here
			big_cat_rx(ConnectionRef, ChannelId, [Data | Acc]);
		{ssh_cm, ConnectionRef, {eof, ChannelId}} ->
			{ok, iolist_to_binary(lists:reverse(Acc))}
		after ?EXEC_TIMEOUT -> timeout
	end.

%--------------------------------------------------------------------
send_after_exit(doc) ->
    ["Send channel data after the channel has been closed."];

send_after_exit(suite) ->
    [];

send_after_exit(Config) when is_list(Config) ->
    ConnectionRef = ssh_test_lib:connect(?SSH_DEFAULT_PORT, [{silently_accept_hosts, true},
							     {user_interaction, false}]),
    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),

	%% Shell command "false" will exit immediately
	success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "false", infinity),

	timer:sleep(2000), %% Allow incoming eof/close/exit_status ssh messages to be processed

	Data = <<"I like spaghetti squash">>,
	case ssh_connection:send(ConnectionRef, ChannelId0, Data, 2000) of
		{error, closed} -> ok;
		ok -> test_server:fail({expected,{error,closed}});
		{error, timeout} -> test_server:fail({expected,{error,closed}});
		Else -> test_server:fail(Else)
	end,

	%% receive close messages
	receive
		{ssh_cm, ConnectionRef, {eof, ChannelId0}} -> ok
		after ?EXEC_TIMEOUT -> test_server:fail()
	end,
	receive
		{ssh_cm, ConnectionRef, {exit_status, ChannelId0, _}} -> ok
		after ?EXEC_TIMEOUT -> test_server:fail()
	end,
	receive
		{ssh_cm, ConnectionRef,{closed, ChannelId0}} -> ok
		after ?EXEC_TIMEOUT -> test_server:fail()
	end.

%--------------------------------------------------------------------
interrupted_send(doc) ->
    ["Use 'head' to cause a channel exit partway through a large send."];

interrupted_send(suite) ->
    [];

interrupted_send(Config) when is_list(Config) ->
    ConnectionRef = ssh_test_lib:connect(?SSH_DEFAULT_PORT, [{silently_accept_hosts, true},
							     {user_interaction, false}]),
    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "head -c 4000000", infinity),

	%% build 10MB binary
	Data = << <<X:32>> || X <- lists:seq(1,2500000)>>,

	%% expect remote end to send us 4MB back
	<<ExpectedData:4000000/binary, _/binary>> = Data,

	%% pre-adjust receive window so the other end doesn't block
	ssh_connection:adjust_window(ConnectionRef, ChannelId0, size(Data)),

	test_server:format("sending ~p byte binary~n",[size(Data)]),

	case ssh_connection:send(ConnectionRef, ChannelId0, Data, 10000) of
		{error, closed} -> ok;
		ok -> test_server:fail({expected,{error,closed}});
		{error, timeout} -> test_server:fail({expected,{error,closed}});
		SendElse -> test_server:fail(SendElse)
	end,

	case ssh_connection:send_eof(ConnectionRef, ChannelId0) of
		{error, closed} -> ok;
		ok -> test_server:fail({expected,{error,closed}});
		EofElse -> test_server:fail(EofElse)
	end,

	%% collect echoed data until eof
	case interrupted_send_rx(ConnectionRef, ChannelId0) of
		{ok, ExpectedData} -> ok;
		{ok, Other} ->
			case size(ExpectedData) =:= size(Other) of
				true ->
					test_server:format("received expected number of bytes, but bytes do not match~n",[]);
				false ->
					test_server:format("expected ~p but only received ~p~n",[size(ExpectedData), size(Other)])
			end,
			test_server:fail(receive_data_mismatch);
		RxElse ->
			test_server:fail(RxElse)
	end,

	%% receive close messages (eof already consumed)
	receive
		{ssh_cm, ConnectionRef, {exit_status, ChannelId0, 0}} -> ok
		after ?EXEC_TIMEOUT -> test_server:fail()
	end,
	receive
		{ssh_cm, ConnectionRef,{closed, ChannelId0}} -> ok
		after ?EXEC_TIMEOUT -> test_server:fail()
	end.

interrupted_send_rx(ConnectionRef, ChannelId) ->
	interrupted_send_rx(ConnectionRef, ChannelId, []).

interrupted_send_rx(ConnectionRef, ChannelId, Acc) ->
	receive
		{ssh_cm, ConnectionRef, {data, ChannelId, 0, Data}} ->
			%% ssh_connection:adjust_window(ConnectionRef, ChannelId, size(Data)),  % window was pre-adjusted, don't adjust again here
			interrupted_send_rx(ConnectionRef, ChannelId, [Data | Acc]);
		{ssh_cm, ConnectionRef, {eof, ChannelId}} ->
			{ok, iolist_to_binary(lists:reverse(Acc))}
		after ?EXEC_TIMEOUT -> timeout
	end.
