%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2011. All Rights Reserved.
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
-module(ssh_to_openssh_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("test_server_line.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(TIMEOUT, 50000).
-define(SSH_DEFAULT_PORT, 22).

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
	    ssh_test_lib:make_dsa_files(Config),
	    Config;
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
	    [erlang_shell_client_openssh_server,
	     erlang_client_openssh_server_exec,
	     erlang_client_openssh_server_exec_compressed,
	     erlang_server_openssh_client_exec,
	     erlang_server_openssh_client_exec_compressed,
	     erlang_client_openssh_server_setenv,
	     erlang_client_openssh_server_publickey_rsa,
	     erlang_client_openssh_server_publickey_dsa,
	     erlang_server_openssh_client_pulic_key_dsa,
	     erlang_client_openssh_server_password]
    end.

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.

%% TEST cases starts here.
%%--------------------------------------------------------------------
erlang_shell_client_openssh_server(doc) ->
    ["Test that ssh:shell/2 works"];

erlang_shell_client_openssh_server(suite) ->
    [];

erlang_shell_client_openssh_server(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    IO = ssh_test_lib:start_io_server(),
    Shell = ssh_test_lib:start_shell(?SSH_DEFAULT_PORT, IO),
    IO ! {input, self(), "echo Hej\n"},
    receive_hej(),
    IO ! {input, self(), "exit\n"},
    receive
	<<"logout">> ->
	    receive
		<<"Connection closed">> ->
		    ok
	    end;
	Other0 ->
	    test_server:fail({unexpected_msg, Other0})
    end,
    receive
	{'EXIT', Shell, normal} ->
	    ok;
	Other1 ->
	    test_server:fail({unexpected_msg, Other1})
    end.

%--------------------------------------------------------------------
erlang_client_openssh_server_exec(doc) ->
    ["Test api function ssh_connection:exec"];

erlang_client_openssh_server_exec(suite) ->
    [];

erlang_client_openssh_server_exec(Config) when is_list(Config) ->
    ConnectionRef = ssh_test_lib:connect(?SSH_DEFAULT_PORT, [{silently_accept_hosts, true},
							     {user_interaction, false}]),
    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "echo testing", infinity),
    Data0 = {ssh_cm, ConnectionRef, {data, ChannelId0, 0, <<"testing\n">>}},
    case ssh_test_lib:receive_exec_result(Data0) of
	expected ->
	    ssh_test_lib:receive_exec_end(ConnectionRef, ChannelId0);
	{unexpected_msg,{ssh_cm, ConnectionRef, {exit_status, ChannelId0, 0}}
	 = ExitStatus0} ->
	    test_server:format("0: Collected data ~p", [ExitStatus0]),
	    ssh_test_lib:receive_exec_result(Data0,
					     ConnectionRef, ChannelId0);
	Other0 ->
	    test_server:fail(Other0)
    end,

    {ok, ChannelId1} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId1,
				  "echo testing1", infinity),
    Data1 = {ssh_cm, ConnectionRef, {data, ChannelId1, 0, <<"testing1\n">>}},
    case ssh_test_lib:receive_exec_result(Data1) of
	expected ->
	    ssh_test_lib:receive_exec_end(ConnectionRef, ChannelId1);
	{unexpected_msg,{ssh_cm, ConnectionRef, {exit_status, ChannelId1, 0}}
	 = ExitStatus1} ->
	    test_server:format("0: Collected data ~p", [ExitStatus1]),
	    ssh_test_lib:receive_exec_result(Data1,
					     ConnectionRef, ChannelId1);
	Other1 ->
	    test_server:fail(Other1)
    end.

%%--------------------------------------------------------------------
erlang_client_openssh_server_exec_compressed(doc) ->
    ["Test that compression option works"];

erlang_client_openssh_server_exec_compressed(suite) ->
    [];

erlang_client_openssh_server_exec_compressed(Config) when is_list(Config) ->
    ConnectionRef = ssh_test_lib:connect(?SSH_DEFAULT_PORT, [{silently_accept_hosts, true},
							     {user_interaction, false},
							     {compression, zlib}]),
    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId,
				  "echo testing", infinity),
    Data = {ssh_cm, ConnectionRef, {data, ChannelId, 0, <<"testing\n">>}},
    case ssh_test_lib:receive_exec_result(Data) of
	expected ->
	    ssh_test_lib:receive_exec_end(ConnectionRef, ChannelId);
	{unexpected_msg,{ssh_cm, ConnectionRef,
			 {exit_status, ChannelId, 0}} = ExitStatus} ->
	    test_server:format("0: Collected data ~p", [ExitStatus]),
	    ssh_test_lib:receive_exec_result(Data,  ConnectionRef, ChannelId);
	Other ->
	    test_server:fail(Other)
    end.

%%--------------------------------------------------------------------
erlang_server_openssh_client_exec(doc) ->
    ["Test that exec command works."];

erlang_server_openssh_client_exec(suite) ->
    [];

erlang_server_openssh_client_exec(Config) when is_list(Config) ->
    SystemDir = ?config(data_dir, Config),
    
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    

    test_server:sleep(500),

    Cmd = "ssh -p " ++ integer_to_list(Port) ++
	" -o StrictHostKeyChecking=no "++ Host ++ " 1+1.",
    SshPort = open_port({spawn, Cmd}, [binary]),

    receive
        {SshPort,{data, <<"2\n">>}} ->
	    ok
    after ?TIMEOUT ->
	    test_server:fail("Did not receive answer")

    end,
     ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
erlang_server_openssh_client_exec_compressed(doc) ->
    ["Test that exec command works."];

erlang_server_openssh_client_exec_compressed(suite) ->
    [];

erlang_server_openssh_client_exec_compressed(Config) when is_list(Config) ->
    SystemDir = ?config(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {compression, zlib},
					     {failfun, fun ssh_test_lib:failfun/2}]),

    test_server:sleep(500),

    Cmd = "ssh -p " ++ integer_to_list(Port) ++
	" -o StrictHostKeyChecking=no -C "++ Host ++ " 1+1.",
    SshPort = open_port({spawn, Cmd}, [binary]),

    receive
        {SshPort,{data, <<"2\n">>}} ->
	    ok
    after ?TIMEOUT ->
	    test_server:fail("Did not receive answer")

    end,
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
erlang_client_openssh_server_setenv(doc) ->
    ["Test api function ssh_connection:setenv"];

erlang_client_openssh_server_setenv(suite) ->
    [];

erlang_client_openssh_server_setenv(Config) when is_list(Config) ->
    ConnectionRef =
	ssh_test_lib:connect(?SSH_DEFAULT_PORT, [{silently_accept_hosts, true},
						 {user_interaction, false}]),
    {ok, ChannelId} =
	ssh_connection:session_channel(ConnectionRef, infinity),
    Env = case ssh_connection:setenv(ConnectionRef, ChannelId,
				     "ENV_TEST", "testing_setenv",
				     infinity) of
	      success ->
		  <<"tesing_setenv\n">>;
	      failure ->
		  <<"\n">>
	  end,
    success = ssh_connection:exec(ConnectionRef, ChannelId,
				  "echo $ENV_TEST", infinity),
    Data = {ssh_cm, ConnectionRef, {data, ChannelId, 0, Env}},
    case ssh_test_lib:receive_exec_result(Data) of
	expected ->
	    ssh_test_lib:receive_exec_end(ConnectionRef, ChannelId);
	{unexpected_msg,{ssh_cm, ConnectionRef,
			 {data,0,1, UnxpectedData}}} ->
	    %% Some os may return things as
	    %% ENV_TEST: Undefined variable.\n"
	    test_server:format("UnxpectedData: ~p", [UnxpectedData]),
	    ssh_test_lib:receive_exec_end(ConnectionRef, ChannelId);
	{unexpected_msg,{ssh_cm, ConnectionRef, {exit_status, ChannelId, 0}}
	 = ExitStatus} ->
	    test_server:format("0: Collected data ~p", [ExitStatus]),
	    ssh_test_lib:receive_exec_result(Data,
					     ConnectionRef, ChannelId);
	Other ->
	    test_server:fail(Other)
    end.

%%--------------------------------------------------------------------

%% setenv not meaningfull on erlang ssh daemon!

%%--------------------------------------------------------------------
erlang_client_openssh_server_publickey_rsa(doc) ->
    ["Validate using rsa publickey."];
erlang_client_openssh_server_publickey_rsa(suite) ->
    [];
erlang_client_openssh_server_publickey_rsa(Config) when is_list(Config) ->
    {ok,[[Home]]} = init:get_argument(home),
    SrcDir =  filename:join(Home, ".ssh"),
    UserDir = ?config(priv_dir, Config),
    case ssh_test_lib:copyfile(SrcDir, UserDir, "id_rsa") of
	{ok, _} ->
	    ConnectionRef =
		ssh_test_lib:connect(?SSH_DEFAULT_PORT,
				     [{user_dir, UserDir},
				      {public_key_alg, ssh_rsa},
				      {user_interaction, false},
				      silently_accept_hosts]),
	    {ok, Channel} =
		ssh_connection:session_channel(ConnectionRef, infinity),
	    ok = ssh_connection:close(ConnectionRef, Channel),
	    ok = ssh:close(ConnectionRef),
	    ok = file:delete(filename:join(UserDir, "id_rsa"));
	{error, enoent} ->
	    {skip, "no ~/.ssh/id_rsa"}
    end.

%%--------------------------------------------------------------------
erlang_client_openssh_server_publickey_dsa(doc) ->
    ["Validate using dsa publickey."];
erlang_client_openssh_server_publickey_dsa(suite) ->
    [];
erlang_client_openssh_server_publickey_dsa(Config) when is_list(Config) ->
    {ok,[[Home]]} = init:get_argument(home),
    SrcDir =  filename:join(Home, ".ssh"),
    UserDir = ?config(priv_dir, Config),
    case ssh_test_lib:copyfile(SrcDir, UserDir, "id_dsa") of
	{ok, _} ->
	    ConnectionRef =
		ssh_test_lib:connect(?SSH_DEFAULT_PORT,
				     [{user_dir, UserDir},
				      {public_key_alg, ssh_dsa},
				      {user_interaction, false},
				      silently_accept_hosts]),
	    {ok, Channel} =
		ssh_connection:session_channel(ConnectionRef, infinity),
	    ok = ssh_connection:close(ConnectionRef, Channel),
	    ok = ssh:close(ConnectionRef),
	    ok = file:delete(filename:join(UserDir, "id_dsa"));
	{error, enoent} ->
	    {skip, "no ~/.ssh/id_dsa"}
    end.

%%--------------------------------------------------------------------
erlang_server_openssh_client_pulic_key_dsa(doc) ->
    ["Validate using dsa publickey."];

erlang_server_openssh_client_pulic_key_dsa(suite) ->
    [];

erlang_server_openssh_client_pulic_key_dsa(Config) when is_list(Config) ->
    SystemDir = ?config(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {public_key_alg, ssh_dsa},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    
    test_server:sleep(500),

    Cmd = "ssh -p " ++ integer_to_list(Port) ++
	" -o StrictHostKeyChecking=no "++ Host ++ " 1+1.",
    SshPort = open_port({spawn, Cmd}, [binary]),

    receive
        {SshPort,{data, <<"2\n">>}} ->
	    ok
    after ?TIMEOUT ->
	    test_server:fail("Did not receive answer")

    end,
     ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
erlang_client_openssh_server_password(doc) ->
    ["Test client password option"];

erlang_client_openssh_server_password(suite) ->
    [];

erlang_client_openssh_server_password(Config) when is_list(Config) ->
    %% to make sure we don't public-key-auth
    UserDir = ?config(data_dir, Config),
    {error, Reason0} =
	ssh_test_lib:connect(?SSH_DEFAULT_PORT, [{silently_accept_hosts, true},
						 {user, "foo"},
						 {password, "morot"},
						 {user_interaction, false},
						 {user_dir, UserDir}]),
    
    test_server:format("Test of user foo that does not exist. "
		       "Error msg: ~p~n", [Reason0]),

    User = string:strip(os:cmd("whoami"), right, $\n),

    case length(string:tokens(User, " ")) of
	1 ->
	    {error, Reason1} =
		ssh_test_lib:connect(?SSH_DEFAULT_PORT,
				     [{silently_accept_hosts, true},
				      {user, User},
				      {password, "foo"},
				      {user_interaction, false},
				      {user_dir, UserDir}]),
	    test_server:format("Test of wrong Pasword.  "
			       "Error msg: ~p~n", [Reason1]);
	_ ->
	    test_server:format("Whoami failed reason: ~n", [])
	end.

%%--------------------------------------------------------------------
%
%% Not possible to send password with openssh without user interaction
%%
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
receive_hej() ->
    receive
	<<"Hej\n">> = Hej->
	    test_server:format("Expected result: ~p~n", [Hej]);
	Info ->
	    test_server:format("Extra info: ~p~n", [Info]),
	    receive_hej()
    end.
