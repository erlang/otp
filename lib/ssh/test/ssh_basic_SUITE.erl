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

-module(ssh_basic_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("test_server_line.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(NEWLINE, <<"\r\n">>).

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
	    Dir = ?config(priv_dir, Config),
	    {ok, _} =  ssh_test_lib:get_id_keys(Dir),
	    ssh_test_lib:make_dsa_files(Config),
	    Config;
	_Else ->
	    {skip, "Crypto could not be started!"}
    end.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(Config) ->
    Dir = ?config(priv_dir, Config),
    crypto:stop(),
    ssh_test_lib:remove_id_keys(Dir),
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
    ssh_test_lib:known_hosts(backup),
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
    ssh_test_lib:known_hosts(restore),
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
    [exec, exec_compressed, shell, daemon_already_started,
     server_password_option, server_userpassword_option,
     known_hosts].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.

%% Test cases starts here.
%%--------------------------------------------------------------------
sign_and_verify_rsa(doc) ->
    ["Test api function ssh:sign_data and ssh:verify_data"];

sign_and_verify_rsa(suite) ->
    [];
sign_and_verify_rsa(Config) when is_list(Config) ->
    Data = ssh:sign_data(<<"correct data">>, "ssh-rsa"),
    ok = ssh:verify_data(<<"correct data">>, Data, "ssh-rsa"),
    {error,invalid_signature} = ssh:verify_data(<<"incorrect data">>, Data,"ssh-rsa").


exec(doc) ->
    ["Test api function ssh_connection:exec"];

exec(suite) ->
    [];

exec(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = ?config(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_interaction, false}]),
    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId0,
				  "1+1.", infinity),
    Data0 = {ssh_cm, ConnectionRef, {data, ChannelId0, 0, <<"2\n">>}},
    case ssh_test_lib:receive_exec_result(Data0) of
	expected ->
	    ok;
	Other0 ->
	    test_server:fail(Other0)
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
	    test_server:fail(Other1)
    end,
    ssh_test_lib:receive_exec_end(ConnectionRef, ChannelId1),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
exec_compressed(doc) ->
    ["Test that compression option works"];

exec_compressed(suite) ->
    [];

exec_compressed(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = ?config(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {compression, zlib},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_interaction, false}]),
    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId,
				  "1+1.", infinity),
    Data = {ssh_cm, ConnectionRef, {data, ChannelId, 0, <<"2\n">>}},
    case ssh_test_lib:receive_exec_result(Data) of
	expected ->
	    ok;
	Other ->
	    test_server:fail(Other)
    end,
    ssh_test_lib:receive_exec_end(ConnectionRef, ChannelId),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------

shell(doc) ->
    ["Test that ssh:shell/2 works"];

shell(suite) ->
    [];

shell(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = ?config(data_dir, Config),
    {_Pid, _Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					       {failfun, fun ssh_test_lib:failfun/2}]),
    test_server:sleep(500),

    IO = ssh_test_lib:start_io_server(),
    Shell = ssh_test_lib:start_shell(Port, IO),
    receive
	ErlShellStart ->
	    test_server:format("Erlang shell start: ~p~n", [ErlShellStart])
    end,
    receive
	ErlPrompt0 ->
	    test_server:format("Erlang prompt: ~p~n", [ErlPrompt0])
    end,
    IO ! {input, self(), "1+1.\r\n"},
     receive
	Echo0 ->
	     test_server:format("Echo: ~p ~n", [Echo0])
    end,
    receive
	?NEWLINE ->
	    ok
    end,
    receive
	Result0 = <<"2">> ->
	    test_server:format("Result: ~p~n", [Result0])
    end,
    receive
	?NEWLINE ->
	    ok
    end,
    receive
	ErlPrompt1 ->
	    test_server:format("Erlang prompt: ~p~n", [ErlPrompt1])
    end,
    exit(Shell, kill),
    %% Does not seem to work in the testserver!
    %%   IO ! {input, self(), "q().\r\n"},
    %%     receive
    %% 	?NEWLINE ->
    %% 	    ok
    %%     end,
    %%     receive
    %% 	Echo1 ->
    %% 	     test_server:format("Echo: ~p ~n", [Echo1])
    %%     end,
    %%     receive
    %% 	?NEWLINE ->
    %% 	    ok
    %%     end,
    %%     receive
    %% 	Result1 ->
    %% 	    test_server:format("Result: ~p~n", [Result1])
    %%     end,
    receive
	{'EXIT', Shell, killed} ->
	    ok
    end.

%%--------------------------------------------------------------------
daemon_already_started(doc) ->
    ["Test that get correct error message if you try to start a daemon",
    "on an adress that already runs a daemon see also seq10667" ];

daemon_already_started(suite) ->
    [];

daemon_already_started(Config) when is_list(Config) ->
    SystemDir = ?config(data_dir, Config),
    {Pid, _Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					      {failfun, fun ssh_test_lib:failfun/2}]),
    {error, eaddrinuse} = ssh_test_lib:daemon(Port, [{system_dir, SystemDir},
						     {failfun,
						      fun ssh_test_lib:failfun/2}]),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
server_password_option(doc) ->
    ["validate to server that uses the 'password' option"];
server_password_option(suite) ->
    [];
server_password_option(Config) when is_list(Config) ->
    UserDir = ?config(data_dir, Config), % to make sure we don't use
    SysDir = ?config(data_dir, Config),	 % public-key-auth
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {password, "morot"}]),

    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_interaction, false},
					  {user_dir, UserDir}]),
    {error, Reason} =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "vego"},
					  {password, "foo"},
					  {user_interaction, false},
					  {user_dir, UserDir}]),
    
    test_server:format("Test of wrong password: Error msg: ~p ~n", [Reason]),

    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------

server_userpassword_option(doc) ->
    ["validate to server that uses the 'password' option"];
server_userpassword_option(suite) ->
    [];
server_userpassword_option(Config) when is_list(Config) ->
    UserDir = ?config(data_dir, Config),  % to make sure we don't use
    SysDir = ?config(data_dir, Config),	  % public-key-auth
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_passwords, [{"vego", "morot"}]}]),

    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "vego"},
					  {password, "morot"},
					  {user_interaction, false},
					  {user_dir, UserDir}]),
    ssh:close(ConnectionRef),

    {error, Reason0} =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_interaction, false},
					  {user_dir, UserDir}]),
    
    test_server:format("Test of user foo that does not exist. "
		       "Error msg: ~p ~n", [Reason0]),

    {error, Reason1} =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "vego"},
					  {password, "foo"},
					  {user_interaction, false},
					  {user_dir, UserDir}]),
    test_server:format("Test of wrong Password. "
		       "Error msg: ~p ~n", [Reason1]),
    
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
known_hosts(doc) ->
    ["check that known_hosts is updated correctly"];
known_hosts(suite) ->
    [];
known_hosts(Config) when is_list(Config) ->
    SystemDir = ?config(data_dir, Config),
    UserDir = ?config(priv_dir, Config),

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {failfun, fun ssh_test_lib:failfun/2}]),

    KnownHosts = filename:join(UserDir, "known_hosts"),
    file:delete(KnownHosts),
    {error, enoent} = file:read_file(KnownHosts),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{user_dir, UserDir},
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
%% Internal functions
%%--------------------------------------------------------------------
