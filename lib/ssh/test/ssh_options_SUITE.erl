%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

-module(ssh_options_SUITE).

%%% This test suite tests different options for the ssh functions


-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").
-include("ssh_test_lib.hrl").

%%% Test cases
-export([connectfun_disconnectfun_client/1, 
	 disconnectfun_option_client/1, 
	 disconnectfun_option_server/1, 
	 id_string_no_opt_client/1, 
	 id_string_no_opt_server/1, 
	 id_string_own_string_client/1, 
	 id_string_own_string_server/1, 
	 id_string_random_client/1, 
	 id_string_random_server/1, 
	 max_sessions_sftp_start_channel_parallel/1, 
	 max_sessions_sftp_start_channel_sequential/1, 
	 max_sessions_ssh_connect_parallel/1, 
	 max_sessions_ssh_connect_sequential/1, 
	 server_password_option/1, 
	 server_userpassword_option/1, 
	 server_pwdfun_option/1,
	 server_pwdfun_4_option/1,
	 server_pwdfun_4_option_repeat/1,
	 ssh_connect_arg4_timeout/1, 
	 ssh_connect_negtimeout_parallel/1, 
	 ssh_connect_negtimeout_sequential/1, 
	 ssh_connect_nonegtimeout_connected_parallel/1,
	 ssh_connect_nonegtimeout_connected_sequential/1,
	 ssh_connect_timeout/1, connect/4,
	 ssh_daemon_minimal_remote_max_packet_size_option/1, 
	 ssh_msg_debug_fun_option_client/1, 
	 ssh_msg_debug_fun_option_server/1, 
	 system_dir_option/1, 
	 unexpectedfun_option_client/1, 
	 unexpectedfun_option_server/1, 
	 user_dir_option/1, 
	 connectfun_disconnectfun_server/1,
	 hostkey_fingerprint_check/1,
	 hostkey_fingerprint_check_md5/1,
	 hostkey_fingerprint_check_sha/1,
	 hostkey_fingerprint_check_sha256/1,
	 hostkey_fingerprint_check_sha384/1,
	 hostkey_fingerprint_check_sha512/1,
	 hostkey_fingerprint_check_list/1
	]).

%%% Common test callbacks
-export([suite/0, all/0, groups/0, 
	 init_per_suite/1, end_per_suite/1, 
	 init_per_group/2, end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2
	]).


-define(NEWLINE, <<"\r\n">>).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,30}}].

all() -> 
    [connectfun_disconnectfun_server,
     connectfun_disconnectfun_client,
     server_password_option,
     server_userpassword_option,
     server_pwdfun_option,
     server_pwdfun_4_option,
     server_pwdfun_4_option_repeat,
     {group, dir_options},
     ssh_connect_timeout,
     ssh_connect_arg4_timeout,
     ssh_daemon_minimal_remote_max_packet_size_option,
     ssh_msg_debug_fun_option_client,
     ssh_msg_debug_fun_option_server,
     disconnectfun_option_server,
     disconnectfun_option_client,
     unexpectedfun_option_server,
     unexpectedfun_option_client,
     hostkey_fingerprint_check,
     hostkey_fingerprint_check_md5,
     hostkey_fingerprint_check_sha,
     hostkey_fingerprint_check_sha256,
     hostkey_fingerprint_check_sha384,
     hostkey_fingerprint_check_sha512,
     hostkey_fingerprint_check_list,
     id_string_no_opt_client,
     id_string_own_string_client,
     id_string_random_client,
     id_string_no_opt_server,
     id_string_own_string_server,
     id_string_random_server,
     {group, hardening_tests}
    ].

groups() ->
    [{hardening_tests, [], [ssh_connect_nonegtimeout_connected_parallel,
			    ssh_connect_nonegtimeout_connected_sequential,
			    ssh_connect_negtimeout_parallel,
			    ssh_connect_negtimeout_sequential,
			    max_sessions_ssh_connect_parallel,
			    max_sessions_ssh_connect_sequential,
			    max_sessions_sftp_start_channel_parallel,
			    max_sessions_sftp_start_channel_sequential
			   ]},
     {dir_options, [], [user_dir_option,
			system_dir_option]}
    ].


%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ?CHECK_CRYPTO(Config).

end_per_suite(_Config) ->
    ssh:stop().

%%--------------------------------------------------------------------
init_per_group(hardening_tests, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    ssh_test_lib:setup_dsa(DataDir, PrivDir),
    ssh_test_lib:setup_rsa(DataDir, PrivDir),
    Config;
init_per_group(dir_options, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    %% Make unreadable dir:
    Dir_unreadable = filename:join(PrivDir, "unread"),
    ok = file:make_dir(Dir_unreadable),
    {ok,F1} = file:read_file_info(Dir_unreadable),
    ok = file:write_file_info(Dir_unreadable, 
			      F1#file_info{mode = F1#file_info.mode band (bnot 8#00444)}),
    %% Make readable file:
    File_readable = filename:join(PrivDir, "file"),
    ok = file:write_file(File_readable, <<>>),

    %% Check:
    case {file:read_file_info(Dir_unreadable), 
	  file:read_file_info(File_readable)} of
	{{ok, Id=#file_info{type=directory, access=Md}},
	 {ok, If=#file_info{type=regular,   access=Mf}}} ->
	    AccessOK =
		case {Md,                Mf} of
		    {read,               _} -> false;
		    {read_write,         _} -> false;
		    {_,               read} -> true;
		    {_,         read_write} -> true;
		    _ -> false
		end,

	    case AccessOK of
		true ->
		    %% Save:
		    [{unreadable_dir, Dir_unreadable},
		     {readable_file, File_readable} 
		     | Config];
		false ->
		    ct:log("File#file_info : ~p~n"
			   "Dir#file_info  : ~p",[If,Id]),
		    {skip, "File or dir mode settings failed"}
	    end;

	NotDirFile ->
	    ct:log("{Dir,File} -> ~p",[NotDirFile]),
	    {skip, "File/Dir creation failed"}
    end;
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    ssh:start(),
    Config.

end_per_testcase(TestCase, Config) when TestCase == server_password_option;
					TestCase == server_userpassword_option;
					TestCase == server_pwdfun_option;
					TestCase == server_pwdfun_4_option ->
    UserDir = filename:join(proplists:get_value(priv_dir, Config), nopubkey),
    ssh_test_lib:del_dirs(UserDir),
    end_per_testcase(Config);
end_per_testcase(_TestCase, Config) ->
    end_per_testcase(Config).

end_per_testcase(_Config) ->    
    ssh:stop(),
    ok.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------

%%% validate to server that uses the 'password' option
server_password_option(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
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
    
    ct:log("Test of wrong password: Error msg: ~p ~n", [Reason]),

    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------

%%% validate to server that uses the 'password' option
server_userpassword_option(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),	  
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
%%% validate to server that uses the 'pwdfun' option
server_pwdfun_option(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),	  
    CHKPWD = fun("foo",Pwd) -> Pwd=="bar";
		(_,_) -> false
	     end,
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, PrivDir},
					     {pwdfun,CHKPWD}]),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "bar"},
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
%%% validate to server that uses the 'pwdfun/4' option
server_pwdfun_4_option(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),	  
    PWDFUN = fun("foo",Pwd,{_,_},undefined) -> Pwd=="bar";
		("fie",Pwd,{_,_},undefined) -> {Pwd=="bar",new_state};
		("bandit",_,_,_) -> disconnect;
		(_,_,_,_) -> false
	     end,
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, PrivDir},
					     {pwdfun,PWDFUN}]),
    ConnectionRef1 =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "bar"},
					  {user_interaction, false},
					  {user_dir, UserDir}]),
    ssh:close(ConnectionRef1),

    ConnectionRef2 =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "fie"},
					  {password, "bar"},
					  {user_interaction, false},
					  {user_dir, UserDir}]),
    ssh:close(ConnectionRef2),

    Reason = "Unable to connect using the available authentication methods",
    
    {error, Reason} =
	ssh:connect(Host, Port, [{silently_accept_hosts, true},
				 {user, "foo"},
				 {password, "morot"},
				 {user_interaction, false},
				 {user_dir, UserDir}]),
    {error, Reason} =
	ssh:connect(Host, Port, [{silently_accept_hosts, true},
				 {user, "fie"},
				 {password, "morot"},
				 {user_interaction, false},
				 {user_dir, UserDir}]),
    {error, Reason} =
	ssh:connect(Host, Port, [{silently_accept_hosts, true},
				 {user, "vego"},
				 {password, "foo"},
				 {user_interaction, false},
				 {user_dir, UserDir}]),

    {error, Reason} =
	ssh:connect(Host, Port, [{silently_accept_hosts, true},
				 {user, "bandit"},
				 {password, "pwd breaking"},
				 {user_interaction, false},
				 {user_dir, UserDir}]),
    ssh:stop_daemon(Pid).

    
%%--------------------------------------------------------------------
server_pwdfun_4_option_repeat(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),	  
    %% Test that the state works
    Parent = self(),
    PWDFUN = fun("foo",P="bar",_,S) -> Parent!{P,S},true; 
		(_,P,_,S=undefined) -> Parent!{P,S},{false,1}; 
		(_,P,_,S) -> Parent!{P,S},          {false,S+1}
	     end,
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, PrivDir},
					     {auth_methods,"keyboard-interactive"},
					     {pwdfun,PWDFUN}]),

    %% Try with passwords "incorrect", "Bad again" and finally "bar"
    KIFFUN = fun(_,_,_) -> 
		     K={k,self()},
		     case get(K) of 
			 undefined -> 
			     put(K,1),
			     ["incorrect"]; 
			 2 ->
			     put(K,3),
			     ["bar"];
			 S->
			     put(K,S+1),
			     ["Bad again"]
		     end
	     end,
    
    ConnectionRef2 = 
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {keyboard_interact_fun, KIFFUN},
					  {user_dir, UserDir}]),
    ssh:close(ConnectionRef2),
    ssh:stop_daemon(Pid),

    lists:foreach(fun(Expect) ->
			  receive
			      Expect -> ok;
			      Other -> ct:fail("Expect: ~p~nReceived ~p",[Expect,Other])
			  after
			      2000 -> ct:fail("Timeout expecting ~p",[Expect])
			  end
		  end, [{"incorrect",undefined},
			{"Bad again",1},
			{"bar",2}]).
			
%%--------------------------------------------------------------------
system_dir_option(Config) ->
    DirUnread = proplists:get_value(unreadable_dir,Config),
    FileRead = proplists:get_value(readable_file,Config),

    case ssh_test_lib:daemon([{system_dir, DirUnread}]) of
	{error,{eoptions,{{system_dir,DirUnread},eacces}}} ->
	    ok;
	{Pid1,_Host1,Port1} when is_pid(Pid1),is_integer(Port1) ->
	    ssh:stop_daemon(Pid1),
	    ct:fail("Didn't detect that dir is unreadable", [])
	end,
    
    case ssh_test_lib:daemon([{system_dir, FileRead}]) of
	{error,{eoptions,{{system_dir,FileRead},enotdir}}} ->
	    ok;
	{Pid2,_Host2,Port2} when is_pid(Pid2),is_integer(Port2) ->
	    ssh:stop_daemon(Pid2),
	    ct:fail("Didn't detect that option is a plain file", [])
    end.


user_dir_option(Config) ->
    DirUnread = proplists:get_value(unreadable_dir,Config),
    FileRead = proplists:get_value(readable_file,Config),
    %% Any port will do (beware, implementation knowledge!):
    Port = 65535,

    case ssh:connect("localhost", Port, [{user_dir, DirUnread}]) of
	{error,{eoptions,{{user_dir,DirUnread},eacces}}} ->
	    ok;
	{error,econnrefused} ->
	    ct:fail("Didn't detect that dir is unreadable", [])
    end,

    case ssh:connect("localhost", Port, [{user_dir, FileRead}]) of
	{error,{eoptions,{{user_dir,FileRead},enotdir}}} ->
	    ok;
	{error,econnrefused} ->
	    ct:fail("Didn't detect that option is a plain file", [])
    end.

%%--------------------------------------------------------------------
%%% validate client that uses the 'ssh_msg_debug_fun' option
ssh_msg_debug_fun_option_client(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    Parent = self(),
    DbgFun = fun(ConnRef,Displ,Msg,Lang) -> Parent ! {msg_dbg,{ConnRef,Displ,Msg,Lang}} end,
		
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_dir, UserDir},
					  {user_interaction, false},
					  {ssh_msg_debug_fun,DbgFun}]),
    %% Beware, implementation knowledge:
    gen_statem:cast(ConnectionRef,{ssh_msg_debug,false,<<"Hello">>,<<>>}),
    receive
	{msg_dbg,X={ConnectionRef,false,<<"Hello">>,<<>>}} ->
	    ct:log("Got expected dbg msg ~p",[X]),
	    ssh:stop_daemon(Pid);
	{msg_dbg,X={_,false,<<"Hello">>,<<>>}} ->
	    ct:log("Got dbg msg but bad ConnectionRef (~p expected) ~p",[ConnectionRef,X]),
	    ssh:stop_daemon(Pid),
	    {fail, "Bad ConnectionRef received"};
	{msg_dbg,X} ->
	    ct:log("Got bad dbg msg ~p",[X]),
	    ssh:stop_daemon(Pid),
	    {fail,"Bad msg received"}
    after 1000 ->
	    ssh:stop_daemon(Pid),
	    {fail,timeout}
    end.

%%--------------------------------------------------------------------
connectfun_disconnectfun_server(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),

    Parent = self(),
    Ref = make_ref(),
    ConnFun = fun(_,_,_) -> Parent ! {connect,Ref} end,
    DiscFun = fun(R) -> Parent ! {disconnect,Ref,R} end,

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {failfun, fun ssh_test_lib:failfun/2},
					     {disconnectfun, DiscFun},
					     {connectfun, ConnFun}]),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_dir, UserDir},
					  {user_interaction, false}]),
    receive
	{connect,Ref} ->
	    ssh:close(ConnectionRef),
	    receive
		{disconnect,Ref,R} ->
		    ct:log("Disconnect result: ~p",[R]),
		    ssh:stop_daemon(Pid)
	    after 5000 ->
		    receive
			X -> ct:log("received ~p",[X])
		    after 0 -> ok
		    end,
		    {fail, "No disconnectfun action"}
	    end
    after 5000 ->
	    receive
		X -> ct:log("received ~p",[X])
	    after 0 -> ok
	    end,
	    {fail, "No connectfun action"}
    end.

%%--------------------------------------------------------------------
connectfun_disconnectfun_client(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),

    Parent = self(),
    Ref = make_ref(),
    DiscFun = fun(R) -> Parent ! {disconnect,Ref,R} end,

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    _ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_dir, UserDir},
					  {disconnectfun, DiscFun},
					  {user_interaction, false}]),
    ssh:stop_daemon(Pid),
    receive
	{disconnect,Ref,R} ->
	    ct:log("Disconnect result: ~p",[R])
    after 2000 ->
	    {fail, "No disconnectfun action"}
    end.

%%--------------------------------------------------------------------
%%% validate client that uses the 'ssh_msg_debug_fun' option
ssh_msg_debug_fun_option_server(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),

    Parent = self(),
    DbgFun = fun(ConnRef,Displ,Msg,Lang) -> Parent ! {msg_dbg,{ConnRef,Displ,Msg,Lang}} end,
    ConnFun = fun(_,_,_) -> Parent ! {connection_pid,self()} end,

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {failfun, fun ssh_test_lib:failfun/2},
					     {connectfun, ConnFun},
					     {ssh_msg_debug_fun, DbgFun}]),
    _ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_dir, UserDir},
					  {user_interaction, false}]),
    receive
	{connection_pid,Server} ->
	    %% Beware, implementation knowledge:
	    gen_statem:cast(Server,{ssh_msg_debug,false,<<"Hello">>,<<>>}),
	    receive
		{msg_dbg,X={_,false,<<"Hello">>,<<>>}} ->
		    ct:log("Got expected dbg msg ~p",[X]),
		    ssh:stop_daemon(Pid);
		{msg_dbg,X} ->
		    ct:log("Got bad dbg msg ~p",[X]),
		    ssh:stop_daemon(Pid),
		    {fail,"Bad msg received"}
	    after 3000 ->
		    ssh:stop_daemon(Pid),
		    {fail,timeout2}
	    end
    after 3000 ->
	    ssh:stop_daemon(Pid),
	    {fail,timeout1}
    end.

%%--------------------------------------------------------------------
disconnectfun_option_server(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),

    Parent = self(),
    DisConnFun = fun(Reason) -> Parent ! {disconnect,Reason} end,

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {failfun, fun ssh_test_lib:failfun/2},
					     {disconnectfun, DisConnFun}]),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_dir, UserDir},
					  {user_interaction, false}]),
    ssh:close(ConnectionRef),
    receive
	{disconnect,Reason} ->
	    ct:log("Server detected disconnect: ~p",[Reason]),
	    ssh:stop_daemon(Pid),
	    ok
    after 5000 ->
	    receive
		X -> ct:log("received ~p",[X])
	    after 0 -> ok
	    end,
	    {fail,"Timeout waiting for disconnect"}
    end.

%%--------------------------------------------------------------------
disconnectfun_option_client(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),

    Parent = self(),
    DisConnFun = fun(Reason) -> Parent ! {disconnect,Reason} end,

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    _ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_dir, UserDir},
					  {user_interaction, false},
					  {disconnectfun, DisConnFun}]),
    ssh:stop_daemon(Pid),
    receive
	{disconnect,Reason} ->
	    ct:log("Client detected disconnect: ~p",[Reason]),
	    ok
    after 3000 ->
	    receive
		X -> ct:log("received ~p",[X])
	    after 0 -> ok
	    end,
	    {fail,"Timeout waiting for disconnect"}
    end.

%%--------------------------------------------------------------------
unexpectedfun_option_server(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),

    Parent = self(),
    ConnFun = fun(_,_,_) -> Parent ! {connection_pid,self()} end,
    UnexpFun = fun(Msg,Peer) ->
		       Parent ! {unexpected,Msg,Peer,self()},
		       skip
	       end,

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {failfun, fun ssh_test_lib:failfun/2},
					     {connectfun, ConnFun},
					     {unexpectedfun, UnexpFun}]),
    _ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_dir, UserDir},
					  {user_interaction, false}]),
    receive
	{connection_pid,Server} ->
	    %% Beware, implementation knowledge:
	    Server ! unexpected_message,
	    receive
		{unexpected, unexpected_message, {{_,_,_,_},_}, _} -> ok;
		{unexpected, unexpected_message, Peer, _} -> ct:fail("Bad peer ~p",[Peer]);
		M = {unexpected, _, _, _} -> ct:fail("Bad msg ~p",[M])
	    after 3000 ->
		    ssh:stop_daemon(Pid),
		    {fail,timeout2}
	    end
    after 3000 ->
	    ssh:stop_daemon(Pid),
	    {fail,timeout1}
    end.

%%--------------------------------------------------------------------
unexpectedfun_option_client(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),

    Parent = self(),
    UnexpFun = fun(Msg,Peer) -> 
		       Parent ! {unexpected,Msg,Peer,self()},
		       skip
	       end,

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {password, "morot"},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "morot"},
					  {user_dir, UserDir},
					  {user_interaction, false},
					  {unexpectedfun, UnexpFun}]),
    %% Beware, implementation knowledge:
    ConnectionRef ! unexpected_message,

    receive
	{unexpected, unexpected_message, {{_,_,_,_},_}, ConnectionRef} ->
	    ok;
	{unexpected, unexpected_message, Peer, ConnectionRef} ->
	    ct:fail("Bad peer ~p",[Peer]);
	M = {unexpected, _, _, _} ->
	    ct:fail("Bad msg ~p",[M])
    after 3000 ->
	    ssh:stop_daemon(Pid),
	    {fail,timeout}
    end.

%%--------------------------------------------------------------------
hostkey_fingerprint_check(Config) ->
    do_hostkey_fingerprint_check(Config, old).

hostkey_fingerprint_check_md5(Config) ->
    do_hostkey_fingerprint_check(Config, md5).

hostkey_fingerprint_check_sha(Config) ->
    do_hostkey_fingerprint_check(Config, sha).

hostkey_fingerprint_check_sha256(Config) ->
    do_hostkey_fingerprint_check(Config, sha256).

hostkey_fingerprint_check_sha384(Config) ->
    do_hostkey_fingerprint_check(Config, sha384).

hostkey_fingerprint_check_sha512(Config) ->
    do_hostkey_fingerprint_check(Config, sha512).

hostkey_fingerprint_check_list(Config) ->
    do_hostkey_fingerprint_check(Config, [sha,md5,sha256]).

%%%----
do_hostkey_fingerprint_check(Config, HashAlg) ->
    case supported_hash(HashAlg) of
	true ->
	    really_do_hostkey_fingerprint_check(Config, HashAlg);
	false ->
	    {skip,{unsupported_hash,HashAlg}}
    end.

supported_hash(old) -> true;
supported_hash(HashAlg) ->
    Hs = if is_atom(HashAlg) -> [HashAlg];
            is_list(HashAlg) -> HashAlg
         end,
    [] == (Hs -- proplists:get_value(hashs, crypto:supports(), [])).


really_do_hostkey_fingerprint_check(Config, HashAlg) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDirServer = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDirServer),
    SysDir = proplists:get_value(data_dir, Config),

    UserDirClient = 
	ssh_test_lib:create_random_dir(Config), % Ensure no 'known_hosts' disturbs

    %% All host key fingerprints.  Trust that public_key has checked the ssh_hostkey_fingerprint
    %% function since that function is used by the ssh client...
    FPs0 = [case HashAlg of
	       old -> public_key:ssh_hostkey_fingerprint(Key);
	       _ -> public_key:ssh_hostkey_fingerprint(HashAlg, Key)
	   end
	   || FileCandidate <- begin
				   {ok,KeyFileCands} = file:list_dir(SysDir),
				   KeyFileCands
			       end,
	      nomatch =/= re:run(FileCandidate, ".*\\.pub", []),
	      {Key,_Cmnts} <- begin
				  {ok,Bin} = file:read_file(filename:join(SysDir, FileCandidate)),
				  try public_key:ssh_decode(Bin, public_key)
				  catch
				      _:_ -> []
				  end
			      end],
    FPs = if is_atom(HashAlg) -> FPs0;
             is_list(HashAlg) -> lists:concat(FPs0)
          end,
    ct:log("Fingerprints(~p) = ~p",[HashAlg,FPs]),

    %% Start daemon with the public keys that we got fingerprints from
    {Pid, Host0, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDirServer},
					     {password, "morot"}]),
    Host = ssh_test_lib:ntoa(Host0),
    FP_check_fun = fun(PeerName, FP) ->
			   ct:log("PeerName = ~p, FP = ~p",[PeerName,FP]),
			   HostCheck = ssh_test_lib:match_ip(Host, PeerName),
			   FPCheck = 
                               if is_atom(HashAlg) -> lists:member(FP, FPs);
                                  is_list(HashAlg) -> lists:all(fun(FP1) -> lists:member(FP1,FPs) end,
                                                                FP)
                               end,
			   ct:log("check ~p == ~p (~p) and ~n~p~n in ~p (~p)~n",
				  [PeerName,Host,HostCheck,FP,FPs,FPCheck]),
			   HostCheck and FPCheck
		   end,
    
    ssh_test_lib:connect(Host, Port, [{silently_accept_hosts,
				       case HashAlg of
					   old -> FP_check_fun;
					   _ -> {HashAlg, FP_check_fun}
				       end},
				      {user, "foo"},
				      {password, "morot"},
				      {user_dir, UserDirClient},
				      {user_interaction, false}]),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
%%% Test connect_timeout option in ssh:connect/4
ssh_connect_timeout(_Config) ->
    ConnTimeout = 2000,
    {error,{faked_transport,connect,TimeoutToTransport}} = 
	ssh:connect("localhost", 12345, 
		    [{transport,{tcp,?MODULE,tcp_closed}},
		     {connect_timeout,ConnTimeout}],
		    1000),
    case TimeoutToTransport of
	ConnTimeout -> ok;
	Other -> 
	    ct:log("connect_timeout is ~p but transport received ~p",[ConnTimeout,Other]),
	    {fail,"ssh:connect/4 wrong connect_timeout received in transport"}
    end.
    
%% Plugin function for the test above
connect(_Host, _Port, _Opts, Timeout) ->
    {error, {faked_transport,connect,Timeout}}.

%%--------------------------------------------------------------------
%%% Test fourth argument in ssh:connect/4
ssh_connect_arg4_timeout(_Config) ->
    Timeout = 1000,
    Parent = self(),
    %% start the server
    Server = spawn(fun() ->
			   {ok,Sl} = gen_tcp:listen(0,[]),
			   {ok,{_,Port}} = inet:sockname(Sl),
			   Parent ! {port,self(),Port},
			   Rsa = gen_tcp:accept(Sl),
			   ct:log("Server gen_tcp:accept got ~p",[Rsa]),
			   receive after 2*Timeout -> ok end %% let client timeout first
		   end),

    %% Get listening port
    Port = receive
	       {port,Server,ServerPort} -> ServerPort
	   after 
	       10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
	   end,

    %% try to connect with a timeout, but "supervise" it
    Client = spawn(fun() ->
			   T0 = erlang:monotonic_time(),
			   Rc = ssh:connect("localhost",Port,[],Timeout),
			   ct:log("Client ssh:connect got ~p",[Rc]),
			   Parent ! {done,self(),Rc,T0}
		   end),

    %% Wait for client reaction on the connection try:
    receive
	{done, Client, {error,timeout}, T0} ->
	    Msp = ms_passed(T0),
	    exit(Server,hasta_la_vista___baby),
	    Low = 0.9*Timeout,
	    High =  2.5*Timeout,
	    ct:log("Timeout limits: ~.4f - ~.4f ms, timeout "
                   "was ~.4f ms, expected ~p ms",[Low,High,Msp,Timeout]),
	    if
		Low<Msp, Msp<High -> ok;
		true -> {fail, "timeout not within limits"}
	    end;

	{done, Client, {error,Other}, _T0} ->
	    ct:log("Error message \"~p\" from the client is unexpected.",[{error,Other}]),
	    {fail, "Unexpected error message"};

	{done, Client, {ok,_Ref}, _T0} ->
	    {fail,"ssh-connected ???"}
    after
	5000 ->
	    exit(Server,hasta_la_vista___baby),
	    exit(Client,hasta_la_vista___baby),
	    {fail, "Didn't timeout"}
    end.

%% Help function, elapsed milliseconds since T0
ms_passed(T0) ->
    %% OTP 18
    erlang:convert_time_unit(erlang:monotonic_time() - T0,
			     native,
			     micro_seconds) / 1000.

%%--------------------------------------------------------------------
ssh_daemon_minimal_remote_max_packet_size_option(Config) ->
    SystemDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config), 
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    
    {Server, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
						{user_dir, UserDir},
						{user_passwords, [{"vego", "morot"}]},
						{failfun, fun ssh_test_lib:failfun/2},
						{minimal_remote_max_packet_size, 14}]),
    Conn =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
					  {user_interaction, false},
					  {user, "vego"},
					  {password, "morot"}]),

    %% Try the limits of the minimal_remote_max_packet_size:
    {ok, _ChannelId} = ssh_connection:session_channel(Conn, 100, 14, infinity),
    {open_error,_,"Maximum packet size below 14 not supported",_} = 
	ssh_connection:session_channel(Conn, 100, 13, infinity),

    ssh:close(Conn),
    ssh:stop_daemon(Server).
    
%%--------------------------------------------------------------------
%% This test try every algorithm by connecting to an Erlang server
id_string_no_opt_client(Config) ->
    {Server, _Host, Port} = fake_daemon(Config),
    {error,_} = ssh:connect("localhost", Port, [], 1000),
    receive
	{id,Server,"SSH-2.0-Erlang/"++Vsn} ->
	    true = expected_ssh_vsn(Vsn);
	{id,Server,Other} ->
	    ct:fail("Unexpected id: ~s.",[Other])
    after 5000 ->
	    {fail,timeout}
    end.

%%--------------------------------------------------------------------
id_string_own_string_client(Config) ->
    {Server, _Host, Port} = fake_daemon(Config),
    {error,_} = ssh:connect("localhost", Port, [{id_string,"Pelle"}], 1000),
    receive
	{id,Server,"SSH-2.0-Pelle\r\n"} ->
	    ok;
	{id,Server,Other} ->
	    ct:fail("Unexpected id: ~s.",[Other])
    after 5000 ->
	    {fail,timeout}
    end.

%%--------------------------------------------------------------------
id_string_random_client(Config) ->
    {Server, _Host, Port} = fake_daemon(Config),
    {error,_} = ssh:connect("localhost", Port, [{id_string,random}], 1000),
    receive
	{id,Server,Id="SSH-2.0-Erlang"++_} ->
	    ct:fail("Unexpected id: ~s.",[Id]);
	{id,Server,Rnd="SSH-2.0-"++_} ->
	    ct:log("Got correct ~s",[Rnd]);
	{id,Server,Id} ->
	    ct:fail("Unexpected id: ~s.",[Id])
    after 5000 ->
	    {fail,timeout}
    end.

%%--------------------------------------------------------------------
id_string_no_opt_server(Config) ->
    {_Server, Host, Port} = ssh_test_lib:std_daemon(Config, []),
    {ok,S1}=ssh_test_lib:gen_tcp_connect(Host,Port,[{active,false},{packet,line}]),
    {ok,"SSH-2.0-Erlang/"++Vsn} = gen_tcp:recv(S1, 0, 2000),
    true = expected_ssh_vsn(Vsn).

%%--------------------------------------------------------------------
id_string_own_string_server(Config) ->
    {_Server, Host, Port} = ssh_test_lib:std_daemon(Config, [{id_string,"Olle"}]),
    {ok,S1}=ssh_test_lib:gen_tcp_connect(Host,Port,[{active,false},{packet,line}]),
    {ok,"SSH-2.0-Olle\r\n"} = gen_tcp:recv(S1, 0, 2000).

%%--------------------------------------------------------------------
id_string_random_server(Config) ->
    {_Server, Host, Port} = ssh_test_lib:std_daemon(Config, [{id_string,random}]),
    {ok,S1}=ssh_test_lib:gen_tcp_connect(Host,Port,[{active,false},{packet,line}]),
    {ok,"SSH-2.0-"++Rnd} = gen_tcp:recv(S1, 0, 2000),
    case Rnd of
	"Erlang"++_ -> ct:log("Id=~p",[Rnd]),
		       {fail,got_default_id};
	"Olle\r\n" -> {fail,got_previous_tests_value};
	_ -> ct:log("Got ~s.",[Rnd])
    end.

%%--------------------------------------------------------------------
ssh_connect_negtimeout_parallel(Config) -> ssh_connect_negtimeout(Config,true).
ssh_connect_negtimeout_sequential(Config) -> ssh_connect_negtimeout(Config,false).
    
ssh_connect_negtimeout(Config, Parallel) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),
    NegTimeOut = 2000,				% ms
    ct:log("Parallel: ~p",[Parallel]),

    {_Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},{user_dir, UserDir},
                                               {parallel_login, Parallel},
                                               {negotiation_timeout, NegTimeOut},
                                               {failfun, fun ssh_test_lib:failfun/2}]),

    {ok,Socket} = ssh_test_lib:gen_tcp_connect(Host, Port, []),

    Factor = 2,
    ct:log("And now sleeping ~p*NegTimeOut (~p ms)...", [Factor, round(Factor * NegTimeOut)]),
    ct:sleep(round(Factor * NegTimeOut)),
    
    case inet:sockname(Socket) of
	{ok,_} -> 
	    %% Give it another chance...
	    ct:log("Sleep more...",[]),
	    ct:sleep(round(Factor * NegTimeOut)),
	    case inet:sockname(Socket) of
		{ok,_} -> ct:fail("Socket not closed");
		{error,_} -> ok
	    end;
	{error,_} -> ok
    end.

%%--------------------------------------------------------------------
%%% Test that ssh connection does not timeout if the connection is established (parallel)
ssh_connect_nonegtimeout_connected_parallel(Config) ->
    ssh_connect_nonegtimeout_connected(Config, true).

%%% Test that ssh connection does not timeout if the connection is established (non-parallel)
ssh_connect_nonegtimeout_connected_sequential(Config) ->
    ssh_connect_nonegtimeout_connected(Config, false).


ssh_connect_nonegtimeout_connected(Config, Parallel) ->
    process_flag(trap_exit, true),
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),
    NegTimeOut = 2000,				% ms
    ct:log("Parallel: ~p",[Parallel]),
   
    {_Pid, _Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},{user_dir, UserDir},
					       {parallel_login, Parallel},
					       {negotiation_timeout, NegTimeOut},
					       {failfun, fun ssh_test_lib:failfun/2}]),
    ct:log("~p Listen ~p:~p",[_Pid,_Host,Port]),
    ct:sleep(500),

    IO = ssh_test_lib:start_io_server(),
    Shell = ssh_test_lib:start_shell(Port, IO, [{user_dir,UserDir}]),
    receive
	Error = {'EXIT', _, _} ->
	    ct:log("~p",[Error]),
	    ct:fail(no_ssh_connection);  
	ErlShellStart ->
	    ct:log("---Erlang shell start: ~p~n", [ErlShellStart]),
	    one_shell_op(IO, NegTimeOut),
	    one_shell_op(IO, NegTimeOut),

	    Factor = 2,
	    ct:log("And now sleeping ~p*NegTimeOut (~p ms)...", [Factor, round(Factor * NegTimeOut)]),
	    ct:sleep(round(Factor * NegTimeOut)),
    
	    one_shell_op(IO, NegTimeOut)
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end,
    exit(Shell, kill).


one_shell_op(IO, TimeOut) ->
    ct:log("One shell op: Waiting for prompter"),
    receive
	ErlPrompt0 -> ct:log("Erlang prompt: ~p~n", [ErlPrompt0])
    after TimeOut -> ct:fail("Timeout waiting for promter")
    end,

    IO ! {input, self(), "2*3*7.\r\n"},
    receive
	Echo0 -> ct:log("Echo: ~p ~n", [Echo0])
    after TimeOut -> ct:fail("Timeout waiting for echo")
    end,

    receive
	?NEWLINE -> ct:log("NEWLINE received", [])
    after TimeOut -> 
	    receive Any1 -> ct:log("Bad NEWLINE: ~p",[Any1])
	    after 0 -> ct:fail("Timeout waiting for NEWLINE")
	    end
    end,

    receive
	Result0 -> ct:log("Result: ~p~n", [Result0])
    after TimeOut ->  ct:fail("Timeout waiting for result")
    end.

%%--------------------------------------------------------------------
max_sessions_ssh_connect_parallel(Config) -> 
    max_sessions(Config, true, connect_fun(ssh__connect,Config)).
max_sessions_ssh_connect_sequential(Config) -> 
    max_sessions(Config, false, connect_fun(ssh__connect,Config)).

max_sessions_sftp_start_channel_parallel(Config) -> 
    max_sessions(Config, true, connect_fun(ssh_sftp__start_channel, Config)).
max_sessions_sftp_start_channel_sequential(Config) -> 
    max_sessions(Config, false, connect_fun(ssh_sftp__start_channel, Config)).


%%%---- helpers:
connect_fun(ssh__connect, Config) ->
    fun(Host,Port) ->
	    ssh_test_lib:connect(Host, Port, 
				 [{silently_accept_hosts, true},
				  {user_dir, proplists:get_value(priv_dir,Config)},
				  {user_interaction, false},
				  {user, "carni"},
				  {password, "meat"}
				 ])
	    %% ssh_test_lib returns R when ssh:connect returns {ok,R}
    end;
connect_fun(ssh_sftp__start_channel, _Config) ->
    fun(Host,Port) ->
	    {ok,_Pid,ConnRef} =
		ssh_sftp:start_channel(Host, Port, 
				       [{silently_accept_hosts, true},
					{user, "carni"},
					{password, "meat"}
				       ]),
	    ConnRef
    end.


max_sessions(Config, ParallelLogin, Connect0) when is_function(Connect0,2) ->
    Connect = fun(Host,Port) ->
		      R = Connect0(Host,Port),
		      ct:log("Connect(~p,~p) -> ~p",[Host,Port,R]),
		      R
	      end,
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),
    MaxSessions = 5,
    {Pid, Host, Port} = ssh_test_lib:daemon([
					     {system_dir, SystemDir},
					     {user_dir, UserDir},
					     {user_passwords, [{"carni", "meat"}]},
					     {parallel_login, ParallelLogin},
					     {max_sessions, MaxSessions}
					    ]),
    ct:log("~p Listen ~p:~p for max ~p sessions",[Pid,Host,Port,MaxSessions]),
    try [Connect(Host,Port) || _ <- lists:seq(1,MaxSessions)]
    of
	Connections ->
	    %% Step 1 ok: could set up max_sessions connections
	    ct:log("Connections up: ~p",[Connections]),
	    [_|_] = Connections,

	    %% Now try one more than alowed:
	    ct:log("Info Report might come here...",[]),
	    try Connect(Host,Port)
	    of
		_ConnectionRef1 ->
		    ssh:stop_daemon(Pid),
		    {fail,"Too many connections accepted"}
	    catch
		error:{badmatch,{error,"Connection closed"}} ->
		    %% Step 2 ok: could not set up max_sessions+1 connections
		    %% This is expected
		    %% Now stop one connection and try to open one more
		    ok = ssh:close(hd(Connections)),
		    try_to_connect(Connect, Host, Port, Pid)
	    end
    catch
	error:{badmatch,{error,"Connection closed"}} ->
	    ssh:stop_daemon(Pid),
	    {fail,"Too few connections accepted"}
    end.


try_to_connect(Connect, Host, Port, Pid) ->
    {ok,Tref} = timer:send_after(3000, timeout_no_connection), % give the supervisors some time...
    try_to_connect(Connect, Host, Port, Pid, Tref, 1). % will take max 3300 ms after 11 tries

try_to_connect(Connect, Host, Port, Pid, Tref, N) ->
     try Connect(Host,Port)
     of
	 _ConnectionRef1 ->
	     %% Step 3 ok: could set up one more connection after killing one
	     %% Thats good.
	     timer:cancel(Tref),
	     ssh:stop_daemon(Pid),
	     receive % flush. 
		 timeout_no_connection -> ok
	     after 0 -> ok
	     end
     catch
	 error:{badmatch,{error,"Connection closed"}} ->
	     %% Could not set up one more connection. Try again until timeout.
	     receive
		 timeout_no_connection ->
		     ssh:stop_daemon(Pid),
		     {fail,"Does not decrease # active sessions"}
	     after N*50 -> % retry after this time
		     try_to_connect(Connect, Host, Port, Pid, Tref, N+1)
	     end
     end.

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
  
expected_ssh_vsn(Str) ->
    try
	{ok,L} = application:get_all_key(ssh),
	proplists:get_value(vsn,L,"")++"\r\n"
    of
	Str -> true;
	"\r\n" -> true;
	_ -> false
    catch
	_:_ -> true %% ssh not started so we dont't know
    end.
	    

fake_daemon(_Config) ->
    Parent = self(),
    %% start the server
    Server = spawn(fun() ->
			   {ok,Sl} = gen_tcp:listen(0,[{packet,line}]),
			   {ok,{Host,Port}} = inet:sockname(Sl),
			   ct:log("fake_daemon listening on ~p:~p~n",[Host,Port]),
			   Parent ! {sockname,self(),Host,Port},
			   Rsa = gen_tcp:accept(Sl),
			   ct:log("Server gen_tcp:accept got ~p",[Rsa]),
			   {ok,S} = Rsa,
			   receive
			       {tcp, S, Id} -> Parent ! {id,self(),Id}
			   after 
			       10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
			   end
		   end),
    %% Get listening host and port
    receive
	{sockname,Server,ServerHost,ServerPort} -> {Server, ServerHost, ServerPort}
    after 
	10000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.
