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
end_per_suite(_Config) ->
    ssh:stop(),
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

end_per_testcase(TestCase, Config) when TestCase == server_password_option;
					TestCase == server_userpassword_option ->
    UserDir = filename:join(?config(priv_dir, Config), nopubkey),
    ssh_test_lib:del_dirs(UserDir),
    end_per_testcase(Config);
end_per_testcase(_TestCase, Config) ->
    end_per_testcase(Config).
end_per_testcase(_Config) ->    
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
    [app_test,
     {group, dsa_key},
     {group, rsa_key},
     {group, dsa_pass_key},
     {group, rsa_pass_key},
     {group, internal_error},
     daemon_already_started,
     server_password_option, server_userpassword_option,
     close].

groups() -> 
    [{dsa_key, [], [exec, exec_compressed, shell, known_hosts]},
     {rsa_key, [], [exec, exec_compressed, shell, known_hosts]},     
     {dsa_pass_key, [], [pass_phrase]},
     {rsa_pass_key, [], [pass_phrase]},
     {internal_error, [], [internal_error]}
    ].

init_per_group(dsa_key, Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:setup_dsa(DataDir, PrivDir),
    Config;
init_per_group(rsa_key, Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:setup_rsa(DataDir, PrivDir),
    Config;
init_per_group(rsa_pass_key, Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:setup_rsa_pass_pharse(DataDir, PrivDir, "Password"),
    [{pass_phrase, {rsa_pass_phrase, "Password"}}| Config];
init_per_group(dsa_pass_key, Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:setup_dsa_pass_pharse(DataDir, PrivDir, "Password"),
    [{pass_phrase, {dsa_pass_phrase, "Password"}}| Config];
init_per_group(internal_error, Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:setup_dsa(DataDir, PrivDir),
    file:delete(filename:join(PrivDir, "system/ssh_host_dsa_key")),
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(dsa_key, Config) ->
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:clean_dsa(PrivDir),
    Config;
end_per_group(rsa_key, Config) ->
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:clean_rsa(PrivDir),
    Config;
end_per_group(dsa_pass_key, Config) ->
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:clean_dsa(PrivDir),
    Config;
end_per_group(rsa_pass_key, Config) ->
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:clean_rsa(PrivDir),
    Config;
end_per_group(internal_error, Config) ->
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:clean_dsa(PrivDir),
    Config;

end_per_group(_, Config) ->
    Config.

%% Test cases starts here.
%%--------------------------------------------------------------------
app_test(suite) ->
    [];
app_test(doc) ->
    ["Application consistency test."];
app_test(Config) when is_list(Config) ->
    ?t:app_test(ssh),
    ok.
%%--------------------------------------------------------------------
misc_ssh_options(doc) ->
    ["Test that we can set some misc options not tested elsewhere, "
     "some options not yet present are not decided if we should support or "
     "if they need thier own test case."];
misc_ssh_options(suite) ->
    [];
misc_ssh_options(Config) when is_list(Config) ->  
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),
    
    CMiscOpt0 = [{connecect_timeout, 1000}, {ip_v6_disabled, false}, {user_dir, UserDir}],
    CMiscOpt1 = [{connecect_timeout, infinity}, {ip_v6_disabled, true}, {user_dir, UserDir}],
    SMiscOpt0 =  [{ip_v6_disabled, false}, {user_dir, UserDir}, {system_dir, SystemDir}],
    SMiscOpt1 =  [{ip_v6_disabled, true}, {user_dir, UserDir}, {system_dir, SystemDir}],
    
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),

    basic_test([{client_opts, CMiscOpt0 ++ ClientOpts}, {server_opts, SMiscOpt0 ++ ServerOpts}]),
    basic_test([{client_opts, CMiscOpt1 ++ ClientOpts}, {server_opts, SMiscOpt1 ++ ServerOpts}]).

%%--------------------------------------------------------------------
exec(doc) ->
    ["Test api function ssh_connection:exec"];

exec(suite) ->
    [];

exec(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),
    
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
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config), 

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},{user_dir, UserDir},
					     {compression, zlib},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
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
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),
   
    {_Pid, _Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},{user_dir, UserDir},
					       {failfun, fun ssh_test_lib:failfun/2}]),
    test_server:sleep(500),

    IO = ssh_test_lib:start_io_server(),
    Shell = ssh_test_lib:start_shell(Port, IO, UserDir),
    receive
	{'EXIT', _, _} ->
	    test_server:fail(no_ssh_connection);  
	ErlShellStart ->
	    test_server:format("Erlang shell start: ~p~n", [ErlShellStart]),
	    do_shell(IO, Shell)
    end.
    
do_shell(IO, Shell) ->
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
    UserDir = ?config(priv_dir, Config),

    {Pid, _Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					      {user_dir, UserDir},
					      {failfun, fun ssh_test_lib:failfun/2}]),
    {error, eaddrinuse} = ssh_test_lib:daemon(Port, [{system_dir, SystemDir},
						     {user_dir, UserDir},
						     {failfun,
						      fun ssh_test_lib:failfun/2}]),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
server_password_option(doc) ->
    ["validate to server that uses the 'password' option"];
server_password_option(suite) ->
    [];
server_password_option(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"}]),

    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_interaction, false},
					  {user_dir, UserDir}]),

    Reason = "Unable to connect using the available authentication methods",
    
    {error, Reason} =
	ssh:connect(Host, Port, [{silently_accept_hosts, true},
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
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = ?config(data_dir, Config),	  
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, PrivDir},
					     {user_passwords, [{"vego", "morot"}]}]),

    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "vego"},
					  {password, "morot"},
					  {user_interaction, false},
					  {user_dir, UserDir}]),
    ssh:close(ConnectionRef),

    Reason = "Unable to connect using the available authentication methods",

    {error, Reason} =
	ssh:connect(Host, Port, [{silently_accept_hosts, true},
				 {user, "foo"},
				 {password, "morot"},
				 {user_interaction, false},
				 {user_dir, UserDir}]),
    {error, Reason} =
	ssh:connect(Host, Port, [{silently_accept_hosts, true},
				 {user, "vego"},
				 {password, "foo"},
				 {user_interaction, false},
				 {user_dir, UserDir}]),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
known_hosts(doc) ->
    ["check that known_hosts is updated correctly"];
known_hosts(suite) ->
    [];
known_hosts(Config) when is_list(Config) ->
    SystemDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config), 
    
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
    [Host, _Ip] = string:tokens(HostAndIp, ","),
    "ssh-" ++ _ = Alg,
    ssh:stop_daemon(Pid).
%%--------------------------------------------------------------------

pass_phrase(doc) ->
    ["Test that we can use keyes protected by pass phrases"];

pass_phrase(suite) ->
    [];

pass_phrase(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),
    PhraseArg = ?config(pass_phrase, Config),

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

internal_error(doc) ->
    ["Test that client does not hang if disconnects due to internal error"];

internal_error(suite) ->
    [];

internal_error(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(?config(priv_dir, Config), system),
    UserDir = ?config(priv_dir, Config),
    
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					     {user_dir, UserDir},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    {error,"Internal error"} =
	ssh:connect(Host, Port, [{silently_accept_hosts, true},
				 {user_dir, UserDir},
				 {user_interaction, false}]),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
close(doc) ->
    ["Simulate that we try to close an already closed connection"];

close(suite) ->
    [];

close(Config) when is_list(Config) ->
    SystemDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config), 
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
%% Internal functions
%%--------------------------------------------------------------------
  
basic_test(Config) ->
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    
    {Pid, Host, Port} = ssh_test_lib:daemon(ServerOpts),
    {ok, CM} = ssh:connect(Host, Port, ClientOpts),
    ok = ssh:close(CM),
    ssh:stop_daemon(Pid).
