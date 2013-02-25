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
-module(ssh_to_openssh_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(TIMEOUT, 50000).
-define(SSH_DEFAULT_PORT, 22).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() -> 
    case os:find_executable("ssh") of
	false -> 
	    {skip, "openSSH not installed on host"};
	_ ->
	    [{group, erlang_client},
	     {group, erlang_server}
	     ]
    end.

groups() -> 
    [{erlang_client, [], [erlang_shell_client_openssh_server,
			  erlang_client_openssh_server_exec,
			  erlang_client_openssh_server_exec_compressed,
			  erlang_client_openssh_server_setenv,
			  erlang_client_openssh_server_publickey_rsa,
			  erlang_client_openssh_server_publickey_dsa,
			  erlang_client_openssh_server_password,
			  erlang_client_openssh_server_nonexistent_subsystem
			 ]},
     {erlang_server, [], [erlang_server_openssh_client_exec,
			  erlang_server_openssh_client_exec_compressed,
			  erlang_server_openssh_client_pulic_key_dsa]}
    ].

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

end_per_suite(_Config) ->
    crypto:stop(),
    ok.

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

init_per_testcase(_TestCase, Config) ->
    ssh:start(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ssh:stop(),
    ok.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

erlang_shell_client_openssh_server() ->
    [{doc, "Test that ssh:shell/2 works"}].

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
	    ct:fail({unexpected_msg, Other0})
    end,
    receive
	{'EXIT', Shell, normal} ->
	    ok;
	Other1 ->
	    ct:fail({unexpected_msg, Other1})
    end.

%--------------------------------------------------------------------
erlang_client_openssh_server_exec() ->
    [{doc, "Test api function ssh_connection:exec"}].

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
	    ct:pal("0: Collected data ~p", [ExitStatus0]),
	    ssh_test_lib:receive_exec_result(Data0,
					     ConnectionRef, ChannelId0);
	Other0 ->
	    ct:fail(Other0)
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
	    ct:pal("0: Collected data ~p", [ExitStatus1]),
	    ssh_test_lib:receive_exec_result(Data1,
					     ConnectionRef, ChannelId1);
	Other1 ->
	    ct:fail(Other1)
    end.

%%--------------------------------------------------------------------
erlang_client_openssh_server_exec_compressed() ->
    [{doc, "Test that compression option works"}].

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
	    ct:pal("0: Collected data ~p", [ExitStatus]),
	    ssh_test_lib:receive_exec_result(Data,  ConnectionRef, ChannelId);
	Other ->
	    ct:fail(Other)
    end.

%%--------------------------------------------------------------------
erlang_server_openssh_client_exec() ->
    [{doc, "Test that exec command works."}].

erlang_server_openssh_client_exec(Config) when is_list(Config) ->
    SystemDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    KnownHosts = filename:join(PrivDir, "known_hosts"),

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    

    ct:sleep(500),

    Cmd = "ssh -p " ++ integer_to_list(Port) ++
	" -o UserKnownHostsFile=" ++ KnownHosts ++ " " ++ Host ++ " 1+1.",

    ct:pal("Cmd: ~p~n", [Cmd]),

    SshPort = open_port({spawn, Cmd}, [binary]),

    receive
        {SshPort,{data, <<"2\n">>}} ->
	    ok
    after ?TIMEOUT ->
	    ct:fail("Did not receive answer")

    end,
     ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
erlang_server_openssh_client_exec_compressed() ->
    [{doc, "Test that exec command works."}].

erlang_server_openssh_client_exec_compressed(Config) when is_list(Config) ->
    SystemDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    KnownHosts = filename:join(PrivDir, "known_hosts"),

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {compression, zlib},
					     {failfun, fun ssh_test_lib:failfun/2}]),

    ct:sleep(500),

    Cmd = "ssh -p " ++ integer_to_list(Port) ++
	" -o UserKnownHostsFile=" ++ KnownHosts ++ " -C "++ Host ++ " 1+1.",
    SshPort = open_port({spawn, Cmd}, [binary]),

    receive
        {SshPort,{data, <<"2\n">>}} ->
	    ok
    after ?TIMEOUT ->
	    ct:fail("Did not receive answer")

    end,
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
erlang_client_openssh_server_setenv() ->
    [{doc, "Test api function ssh_connection:setenv"}].

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
	    ct:pal("UnxpectedData: ~p", [UnxpectedData]),
	    ssh_test_lib:receive_exec_end(ConnectionRef, ChannelId);
	{unexpected_msg,{ssh_cm, ConnectionRef, {exit_status, ChannelId, 0}}
	 = ExitStatus} ->
	    ct:pal("0: Collected data ~p", [ExitStatus]),
	    ssh_test_lib:receive_exec_result(Data,
					     ConnectionRef, ChannelId);
	Other ->
	    ct:fail(Other)
    end.

%%--------------------------------------------------------------------

%% setenv not meaningfull on erlang ssh daemon!

%%--------------------------------------------------------------------
erlang_client_openssh_server_publickey_rsa() ->
    [{doc, "Validate using rsa publickey."}].
erlang_client_openssh_server_publickey_rsa(Config) when is_list(Config) ->
    {ok,[[Home]]} = init:get_argument(home),
    KeyFile =  filename:join(Home, ".ssh/id_rsa"),
    case file:read_file(KeyFile) of
	{ok, Pem} ->
	    case public_key:pem_decode(Pem) of
		[{_,_, not_encrypted}] ->
		    ConnectionRef =
			ssh_test_lib:connect(?SSH_DEFAULT_PORT,
					     [{public_key_alg, ssh_rsa},
					      {user_interaction, false},
					      silently_accept_hosts]),
		    {ok, Channel} =
			ssh_connection:session_channel(ConnectionRef, infinity),
		    ok = ssh_connection:close(ConnectionRef, Channel),
		    ok = ssh:close(ConnectionRef);
		_ ->
		    {skip, {error, "Has pass phrase can not be used by automated test case"}} 
	    end;
	_ ->
	    {skip, "no ~/.ssh/id_rsa"}  
    end.
	

%%--------------------------------------------------------------------
erlang_client_openssh_server_publickey_dsa() ->
    [{doc, "Validate using dsa publickey."}].
erlang_client_openssh_server_publickey_dsa(Config) when is_list(Config) ->
    {ok,[[Home]]} = init:get_argument(home),
    KeyFile =  filename:join(Home, ".ssh/id_dsa"),
    case file:read_file(KeyFile) of
	{ok, Pem} ->
	    case public_key:pem_decode(Pem) of
		[{_,_, not_encrypted}] ->
		    ConnectionRef =
			ssh_test_lib:connect(?SSH_DEFAULT_PORT,
					     [{public_key_alg, ssh_dsa},
					      {user_interaction, false},
					      silently_accept_hosts]),
		    {ok, Channel} =
			ssh_connection:session_channel(ConnectionRef, infinity),
		    ok = ssh_connection:close(ConnectionRef, Channel),
		    ok = ssh:close(ConnectionRef);
		_ ->
		    {skip, {error, "Has pass phrase can not be used by automated test case"}} 
	    end;
	_ ->
	    {skip, "no ~/.ssh/id_dsa"}  
    end.
%%--------------------------------------------------------------------
erlang_server_openssh_client_pulic_key_dsa() ->
    [{doc, "Validate using dsa publickey."}].
erlang_server_openssh_client_pulic_key_dsa(Config) when is_list(Config) ->
    SystemDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    KnownHosts = filename:join(PrivDir, "known_hosts"),

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {public_key_alg, ssh_dsa},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    
    ct:sleep(500),

    Cmd = "ssh -p " ++ integer_to_list(Port) ++
	" -o UserKnownHostsFile=" ++ KnownHosts ++
	" " ++ Host ++ " 1+1.",
    SshPort = open_port({spawn, Cmd}, [binary]),

    receive
        {SshPort,{data, <<"2\n">>}} ->
	    ok
    after ?TIMEOUT ->
	    ct:fail("Did not receive answer")
    end,
     ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
erlang_client_openssh_server_password() ->
    [{doc, "Test client password option"}].
erlang_client_openssh_server_password(Config) when is_list(Config) ->
    %% to make sure we don't public-key-auth
    UserDir = ?config(data_dir, Config),
    {error, Reason0} =
	ssh:connect(any, ?SSH_DEFAULT_PORT, [{silently_accept_hosts, true},
						 {user, "foo"},
						 {password, "morot"},
						 {user_interaction, false},
						 {user_dir, UserDir}]),
    
    ct:pal("Test of user foo that does not exist. "
		       "Error msg: ~p~n", [Reason0]),

    User = string:strip(os:cmd("whoami"), right, $\n),

    case length(string:tokens(User, " ")) of
	1 ->
	    {error, Reason1} =
		ssh:connect(any, ?SSH_DEFAULT_PORT,
			    [{silently_accept_hosts, true},
			     {user, User},
			     {password, "foo"},
			     {user_interaction, false},
			     {user_dir, UserDir}]),
	    ct:pal("Test of wrong Pasword.  "
			       "Error msg: ~p~n", [Reason1]);
	_ ->
	    ct:pal("Whoami failed reason: ~n", [])
	end.

%%--------------------------------------------------------------------

erlang_client_openssh_server_nonexistent_subsystem() ->
    [{doc, "Test client password option"}].
erlang_client_openssh_server_nonexistent_subsystem(Config) when is_list(Config) ->

    ConnectionRef = ssh_test_lib:connect(?SSH_DEFAULT_PORT,
					 [{user_interaction, false},
					  silently_accept_hosts]),

    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),

    failure = ssh_connection:subsystem(ConnectionRef, ChannelId, "foo", infinity).

%%--------------------------------------------------------------------
%
%% Not possible to send password with openssh without user interaction
%%
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%%% Internal functions -----------------------------------------------
%%--------------------------------------------------------------------
receive_hej() ->
    receive
	<<"Hej\n">> = Hej->
	    ct:pal("Expected result: ~p~n", [Hej]);
	Info ->
	    ct:pal("Extra info: ~p~n", [Info]),
	    receive_hej()
    end.
