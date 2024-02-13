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

-module(ssh_options_SUITE).

%%% This test suite tests different options for the ssh functions


-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").
-include("ssh_test_lib.hrl").

%%% Test cases
-export([
         auth_method_kb_interactive_data_tuple/1,
         auth_method_kb_interactive_data_fun3/1,
         auth_method_kb_interactive_data_fun4/1,
         auth_none/1,
         connectfun_disconnectfun_client/1, 
	 disconnectfun_option_client/1, 
	 disconnectfun_option_server/1, 
	 id_string_no_opt_client/1, 
	 id_string_no_opt_server/1, 
	 id_string_own_string_client/1, 
	 id_string_own_string_client_trail_space/1, 
	 id_string_own_string_server/1, 
	 id_string_own_string_server_trail_space/1, 
	 id_string_random_client/1, 
	 id_string_random_server/1, 
         max_log_item_len/1,
	 max_sessions_sftp_start_channel_parallel/1, 
	 max_sessions_sftp_start_channel_sequential/1, 
	 max_sessions_ssh_connect_parallel/1, 
	 max_sessions_ssh_connect_sequential/1, 
         max_sessions_drops_tcp_connects/1,
         max_sessions_drops_tcp_connects/0,
	 server_password_option/1, 
	 server_userpassword_option/1, 
	 server_pwdfun_option/1,
	 server_pwdfun_4_option/1,
	 server_keyboard_interactive/1,
	 server_keyboard_interactive_extra_msg/1,
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
	 user_dir_fun_option/1,
	 connectfun_disconnectfun_server/1,
	 hostkey_fingerprint_check/1,
	 hostkey_fingerprint_check_md5/1,
	 hostkey_fingerprint_check_sha/1,
	 hostkey_fingerprint_check_sha256/1,
	 hostkey_fingerprint_check_sha384/1,
	 hostkey_fingerprint_check_sha512/1,
	 hostkey_fingerprint_check_list/1,
         save_accepted_host_option/1,
         raw_option/1,
         config_file/1,
         config_file_modify_algorithms_order/1,
         daemon_replace_options_simple/1,
         daemon_replace_options_algs/1,
         daemon_replace_options_algs_connect/1,
         daemon_replace_options_algs_conf_file/1
	]).

%%% Common test callbacks
-export([suite/0, all/0, groups/0, 
	 init_per_suite/1, end_per_suite/1, 
	 init_per_group/2, end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2
	]).

%%% For test nodes
-export([get_preferred_algorithms/2
        ]).

-define(NEWLINE, <<"\r\n">>).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,60}}].

all() -> 
    [connectfun_disconnectfun_server,
     connectfun_disconnectfun_client,
     server_password_option,
     server_userpassword_option,
     server_pwdfun_option,
     server_pwdfun_4_option,
     server_keyboard_interactive,
     server_keyboard_interactive_extra_msg,
     auth_method_kb_interactive_data_tuple,
     auth_method_kb_interactive_data_fun3,
     auth_method_kb_interactive_data_fun4,
     auth_none,
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
     id_string_own_string_client_trail_space,
     id_string_random_client,
     id_string_no_opt_server,
     id_string_own_string_server,
     id_string_own_string_server_trail_space,
     id_string_random_server,
     max_log_item_len,
     save_accepted_host_option,
     raw_option,
     config_file,
     config_file_modify_algorithms_order,
     daemon_replace_options_simple,
     daemon_replace_options_algs,
     daemon_replace_options_algs_connect,
     daemon_replace_options_algs_conf_file,
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
			    max_sessions_sftp_start_channel_sequential,
                            max_sessions_drops_tcp_connects
			   ]},
     {dir_options, [], [user_dir_option,
                        user_dir_fun_option,
			system_dir_option]}
    ].


%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ?CHECK_CRYPTO(Config).

end_per_suite(_Config) ->
    ssh:stop().

%%--------------------------------------------------------------------
init_per_group(hardening_tests, Config) ->
    ct:log("Pub keys setup for: ~p",
           [ssh_test_lib:setup_all_user_host_keys(Config)]),
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
    %% Create a clean user_dir
    UserDir = filename:join(proplists:get_value(priv_dir, Config), nopubkey),
    ssh_test_lib:del_dirs(UserDir),
    file:make_dir(UserDir),
    [{user_dir,UserDir}|Config].

end_per_testcase(_TestCase, _Config) ->
    ssh:stop(),
    ok.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

%%% validate to server that uses the 'password' option
server_password_option(Config) when is_list(Config) ->
    UserDir = proplists:get_value(user_dir, Config),
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
                                 {save_accepted_host, false},
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
    UserDir = proplists:get_value(user_dir, Config),
    SysDir = proplists:get_value(data_dir, Config),	  
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
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
                                 {save_accepted_host, false},
				 {user, "foo"},
				 {password, "morot"},
				 {user_interaction, false},
				 {user_dir, UserDir}]),
    {error, Reason} =
	ssh:connect(Host, Port, [{silently_accept_hosts, true},
                                 {save_accepted_host, false},
				 {user, "vego"},
				 {password, "foo"},
				 {user_interaction, false},
				 {user_dir, UserDir}]),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
%%% validate to server that uses the 'pwdfun' option
server_pwdfun_option(Config) ->
    UserDir = proplists:get_value(user_dir, Config),
    SysDir = proplists:get_value(data_dir, Config),	  
    CHKPWD = fun("foo",Pwd) -> Pwd=="bar";
		(_,_) -> false
	     end,
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
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
                                 {save_accepted_host, false},
				 {user, "foo"},
				 {password, "morot"},
				 {user_interaction, false},
				 {user_dir, UserDir}]),
    {error, Reason} =
	ssh:connect(Host, Port, [{silently_accept_hosts, true},
                                 {save_accepted_host, false},
				 {user, "vego"},
				 {password, "foo"},
				 {user_interaction, false},
				 {user_dir, UserDir}]),
    ssh:stop_daemon(Pid).

    
%%--------------------------------------------------------------------
%%% validate to server that uses the 'pwdfun/4' option
server_pwdfun_4_option(Config) ->
    UserDir = proplists:get_value(user_dir, Config),
    SysDir = proplists:get_value(data_dir, Config),	  
    PWDFUN = fun("foo",Pwd,{_,_},undefined) -> Pwd=="bar";
		("fie",Pwd,{_,_},undefined) -> {Pwd=="bar",new_state};
		("bandit",_,_,_) -> disconnect;
		(_,_,_,_) -> false
	     end,
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
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
                                 {save_accepted_host, false},
				 {user, "foo"},
				 {password, "morot"},
				 {user_interaction, false},
				 {user_dir, UserDir}]),
    {error, Reason} =
	ssh:connect(Host, Port, [{silently_accept_hosts, true},
                                 {save_accepted_host, false},
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
                                 {save_accepted_host, false},
				 {user, "bandit"},
				 {password, "pwd breaking"},
				 {user_interaction, false},
				 {user_dir, UserDir}]),
    ssh:stop_daemon(Pid).

    
%%--------------------------------------------------------------------
server_keyboard_interactive(Config) ->
    UserDir = proplists:get_value(user_dir, Config),
    SysDir = proplists:get_value(data_dir, Config),
    %% Test that the state works
    Parent = self(),
    PWDFUN = fun("foo",P="bar",_,S) -> Parent!{P,S},true; 
		(_,P,_,S=undefined) -> Parent!{P,S},{false,1}; 
		(_,P,_,S) -> Parent!{P,S},          {false,S+1}
	     end,
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {auth_methods,"keyboard-interactive"},
					     {pwdfun,PWDFUN}]),

    %% Try with passwords "incorrect", "Bad again" and finally "bar"
    KIFFUN = fun(_Name, _Instr, _PromptInfos) ->
		     K={k,self()},
                     Answer =
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
                         end,
                     ct:log("keyboard_interact_fun:~n"
                            " Name        = ~p~n"
                            " Instruction = ~p~n"
                            " Prompts     = ~p~n"
                            "~nAnswer:~n  ~p~n",
                            [_Name, _Instr, _PromptInfos, Answer]),

                     Answer
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
server_keyboard_interactive_extra_msg(Config) ->
    UserDir = proplists:get_value(user_dir, Config),
    SysDir = proplists:get_value(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {auth_methods,"keyboard-interactive"},
                                             {tstflg, [{one_empty,true}]},
                                             {user_passwords, [{"foo","bar"}]}
                                            ]),

    ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {password, "bar"},
					  {user_dir, UserDir}]),
    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
auth_method_kb_interactive_data_tuple(Config) ->
    T = {"abc1", "def1", "ghi1: ", true},
    amkid(Config, T, T).

auth_method_kb_interactive_data_fun3(Config) ->
    T = {"abc2", "def2", "ghi2: ", true},
    amkid(Config, T,
          fun(_Peer, _User, _Service) -> T end
         ).

auth_method_kb_interactive_data_fun4(Config) ->
    T = {"abc3", "def3", "ghi3: ", true},
    amkid(Config, T,
          fun(_Peer, _User, _Service, _State) -> T end
         ).

amkid(Config, {ExpectName,ExpectInstr,ExpectPrompts,ExpectEcho}, OptVal) ->
    UserDir = proplists:get_value(user_dir, Config),
    SysDir = proplists:get_value(data_dir, Config),
    %% Test that the state works
    Parent = self(),
    PWDFUN = fun("foo",P="bar",_,S) -> Parent!{P,S},true;
		(_,P,_,S=undefined) -> Parent!{P,S},{false,1};
		(_,P,_,S) -> Parent!{P,S},          {false,S+1}
	     end,
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {auth_methods,"keyboard-interactive"},
					     {pwdfun,PWDFUN},
                                             {auth_method_kb_interactive_data,OptVal}
                                            ]),

    KIFFUN = fun(Name, Instr, PromptInfos) ->
		     K={k,self()},
                     Answer =
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
                         end,
                     ct:log("keyboard_interact_fun:~n"
                            " Name        = ~p~n"
                            " Instruction = ~p~n"
                            " Prompts     = ~p~n"
                            "~nAnswer:~n  ~p~n",
                            [Name, Instr, PromptInfos, Answer]),
                     case {binary_to_list(Name),
                           binary_to_list(Instr),
                           [{binary_to_list(PI),Echo} || {PI,Echo} <- PromptInfos]
                          } of
                         {ExpectName, ExpectInstr, [{ExpectPrompts,ExpectEcho}]} ->
                             ct:log("Match!", []),
                             Answer;
                         _ ->
                             ct:log("Not match!~n"
                                    " ExpectName        = ~p~n"
                                    " ExpectInstruction = ~p~n"
                                    " ExpectPrompts     = ~p~n",
                                    [ExpectName, ExpectInstr, [{ExpectPrompts,ExpectEcho}]]),
                             ct:fail("no_match")
                     end
	     end,
    ssh_dbg:start(), ssh_dbg:on(authentication), %% Test dbg code
    ConnectionRef2 =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {keyboard_interact_fun, KIFFUN},
					  {user_dir, UserDir}]),
    ssh_dbg:stop(),
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
auth_none(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    {DaemonRef, Host, Port} =
	ssh_test_lib:daemon([{system_dir, SysDir},
			     {user_dir, UserDir},
			     {auth_methods, "password"}, % to make even more sure we don't use public-key-auth
			     {user_passwords, [{"foo","somepwd"}]}, % Not to be used
                             {no_auth_needed, true} % we test this
			    ]),
    ClientConnRef1 =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "some-other-user"},
					  {password, "wrong-pwd"},
					  {user_dir, UserDir},
					  {user_interaction, false}]),
    "some-other-user" =
        proplists:get_value(user, ssh:connection_info(ClientConnRef1, [user])),
    ok = ssh:close(ClientConnRef1),
    ok = ssh:stop_daemon(DaemonRef).

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

%%--------------------------------------------------------------------
user_dir_option(Config) ->
    DirUnread = proplists:get_value(unreadable_dir,Config),
    FileRead = proplists:get_value(readable_file,Config),
    %% Any port will do (beware, implementation knowledge!):
    Port = 65535,

    case ssh:connect("localhost", Port, [{user_dir, DirUnread},
                                         {save_accepted_host, false}]) of
	{error,{eoptions,{{user_dir,DirUnread},eacces}}} ->
	    ok;
	{error,econnrefused} ->
	    ct:fail("Didn't detect that dir is unreadable", [])
    end,

    case ssh:connect("localhost", Port, [{user_dir, FileRead},
                                         {save_accepted_host, false}]) of
	{error,{eoptions,{{user_dir,FileRead},enotdir}}} ->
	    ok;
	{error,econnrefused} ->
	    ct:fail("Didn't detect that option is a plain file", [])
    end.

%%--------------------------------------------------------------------
user_dir_fun_option(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    SysDir = filename:join(PrivDir,"system"),
    ssh_test_lib:setup_all_host_keys(DataDir, SysDir),
    UserDir = filename:join(PrivDir,"user"),
    ssh_test_lib:setup_all_user_keys(DataDir, UserDir),

    Parent = self(),
    Ref = make_ref(),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir_fun, fun(User) ->
                                                                    ct:log("user_dir_fun called ~p",[User]),
                                                                    Parent ! {user,Ref,User},
                                                                    UserDir
                                                            end},
					     {failfun, fun ssh_test_lib:failfun/2}]),
    _ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user, "foo"},
					  {user_dir, UserDir},
                                          {auth_methods,"publickey"},
					  {user_interaction, false}]),
    receive
        {user,Ref,"foo"} ->
            ssh:stop_daemon(Pid),
            ok;
        {user,Ref,What} ->
            ssh:stop_daemon(Pid),
            ct:log("Got ~p",[What]),
            {fail, bad_userid}
    after 2000 ->
            ssh:stop_daemon(Pid),
            {fail,timeout_in_receive}
    end.


%%--------------------------------------------------------------------
%%% validate client that uses the 'ssh_msg_debug_fun' option
ssh_msg_debug_fun_option_client(Config) ->
    UserDir = proplists:get_value(user_dir, Config),
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
    UserDir = proplists:get_value(user_dir, Config),
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
	    after 10000 ->
		    receive
			X -> ct:log("received ~p",[X])
		    after 0 -> ok
		    end,
		    {fail, "No disconnectfun action"}
	    end
    after 10000 ->
	    receive
		X -> ct:log("received ~p",[X])
	    after 0 -> ok
	    end,
	    {fail, "No connectfun action"}
    end.

%%--------------------------------------------------------------------
connectfun_disconnectfun_client(Config) ->
    UserDir = proplists:get_value(user_dir, Config),
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
    UserDir = proplists:get_value(user_dir, Config),
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
    UserDir = proplists:get_value(user_dir, Config),
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
    UserDir = proplists:get_value(user_dir, Config),
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
    UserDir = proplists:get_value(user_dir, Config),
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
    UserDir = proplists:get_value(user_dir, Config),
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
	false when HashAlg == old ->
	    {skip,{unsupported_hash,md5}};% Happen to know that ssh:hostkey_fingerprint/1 uses md5...
	false ->
	    {skip,{unsupported_hash,HashAlg}}
    end.

supported_hash(old) ->
    supported_hash(md5); % Happen to know that ssh:hostkey_fingerprint/1 uses md5...
supported_hash(HashAlg) ->
    Hs = if is_atom(HashAlg) -> [HashAlg];
            is_list(HashAlg) -> HashAlg
         end,
    [] == (Hs -- proplists:get_value(hashs, crypto:supports(), [])).


really_do_hostkey_fingerprint_check(Config, HashAlg) ->
    UserDir = proplists:get_value(user_dir, Config),
    SysDir = proplists:get_value(data_dir, Config),

    %% All host key fingerprints.  Trust that public_key has checked the hostkey_fingerprint
    %% function since that function is used by the ssh client...
    FPs0 = [case HashAlg of
	       old -> ssh:hostkey_fingerprint(Key);
	       _ -> ssh:hostkey_fingerprint(HashAlg, Key)
	   end
	   || FileCandidate <- begin
				   {ok,KeyFileCands} = file:list_dir(SysDir),
				   KeyFileCands
			       end,
	      nomatch =/= re:run(FileCandidate, ".*\\.pub", []),
	      {Key,_Cmnts} <- begin
				  {ok,Bin} = file:read_file(filename:join(SysDir, FileCandidate)),
				  try ssh_file:decode(Bin, public_key)
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
					     {user_dir, UserDir},
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
				      {user_dir, UserDir},
                                      {save_accepted_host, false}, % Ensure no 'known_hosts' disturbs
				      {user_interaction, false}]),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
%%% Test connect_timeout option in ssh:connect/4
ssh_connect_timeout(_Config) ->
    ConnTimeout = 2000,
    {error,{faked_transport,connect,TimeoutToTransport}} = 
	ssh:connect("localhost", 12345, 
		    [{transport,{tcp,?MODULE,tcp_closed}},
                     {save_accepted_host, false},
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
			   Rc = ssh:connect("localhost",Port,[{save_accepted_host, false}],Timeout),
			   ct:log("Client ssh:connect got ~p",[Rc]),
			   Parent ! {done,self(),Rc,T0}
		   end),

    %% Wait for client reaction on the connection try:
    receive
	{done, Client, {error,timeout}, T0} ->
	    Msp = ms_passed(T0),
	    exit(Server,hasta_la_vista___baby),
	    Low = 0.9*Timeout,
	    High =  4.0*Timeout,
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
    UserDir = proplists:get_value(user_dir, Config), 
    
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
    {error,_} = ssh:connect("localhost", Port, [{save_accepted_host, false}], 1000),
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
    {error,_} = ssh:connect("localhost", Port, [{id_string,"Pelle"},
                                                {save_accepted_host, false}
                                               ], 1000),
    receive
	{id,Server,"SSH-2.0-Pelle\r\n"} ->
	    ok;
	{id,Server,Other} ->
	    ct:fail("Unexpected id: ~s.",[Other])
    after 5000 ->
	    {fail,timeout}
    end.

%%--------------------------------------------------------------------
id_string_own_string_client_trail_space(Config) ->
    {Server, _Host, Port} = fake_daemon(Config),
    {error,_} = ssh:connect("localhost", Port, [{id_string,"Pelle "},
                                                {save_accepted_host, false}], 1000),
    receive
	{id,Server,"SSH-2.0-Pelle \r\n"} ->
	    ok;
	{id,Server,Other} ->
	    ct:fail("Unexpected id: ~s.",[Other])
    after 5000 ->
	    {fail,timeout}
    end.

%%--------------------------------------------------------------------
id_string_random_client(Config) ->
    {Server, _Host, Port} = fake_daemon(Config),
    {error,_} = ssh:connect("localhost", Port, [{id_string,random},
                                                {save_accepted_host, false}], 1000),
    receive
	{id,Server,Id="SSH-2.0-Erlang"++_} ->
	    ct:fail("Unexpected id: ~s.",[Id]);
	{id,Server,Rnd="SSH-2.0-"++ID} when 4=<length(ID),length(ID)=<7 -> %% Add 2 for CRLF
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
id_string_own_string_server_trail_space(Config) ->
    {_Server, Host, Port} = ssh_test_lib:std_daemon(Config, [{id_string,"Olle "}]),
    {ok,S1}=ssh_test_lib:gen_tcp_connect(Host,Port,[{active,false},{packet,line}]),
    {ok,"SSH-2.0-Olle \r\n"} = gen_tcp:recv(S1, 0, 2000).

%%--------------------------------------------------------------------
id_string_random_server(Config) ->
    %% Check undocumented format of id_string. First a bad variant:
    {error,{eoptions,_}} = ssh:daemon(0, [{id_string,{random,8,6}}]),
    %% And then a correct one:
    {_Server, Host, Port} = ssh_test_lib:std_daemon(Config, [{id_string,{random,6,8}}]),
    {ok,S1}=ssh_test_lib:gen_tcp_connect(Host,Port,[{active,false},{packet,line}]),
    {ok,"SSH-2.0-"++Rnd} = gen_tcp:recv(S1, 0, 2000),
    case Rnd of
	"Erlang"++_ -> ct:log("Id=~p",[Rnd]),
		       {fail,got_default_id};
	"Olle\r\n" -> {fail,got_previous_tests_value};
	_ when 8=<length(Rnd),length(Rnd)=<10 -> %% Add 2 for CRLF
	    ct:log("Got correct ~s",[Rnd]);
	_ ->
            ct:log("Got wrong sized ~s.",[Rnd]),
            {fail,got_wrong_size}
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
	Result0 -> ct:log("Result: ~p~n", [Result0])
    after TimeOut ->  ct:fail("Timeout waiting for result")
    end.

%%--------------------------------------------------------------------
max_log_item_len(Config) ->
    %% Find a supported algorithm (to be removed from the daemon):
    {ok, {Type,Alg}} = select_alg( ssh:default_algorithms() ),

    %% Start a test daemon without support for {Type,Alg}
    SystemDir = proplists:get_value(data_dir, Config),
    UserDir = proplists:get_value(priv_dir, Config),
    {_Pid, Host0, Port} =
        ssh_test_lib:daemon([
                             {system_dir, SystemDir},
                             {user_dir, UserDir},
                             {user_passwords, [{"carni", "meat"}]},
                             {modify_algorithms, [{rm, [{Type,[Alg]}]}]},
                             {max_log_item_len, 10}
                            ]),
    Host = ssh_test_lib:mangle_connect_address(Host0),
    ct:log("~p:~p Listen ~p:~p. Mangled Host = ~p",
           [?MODULE,?LINE,Host0,Port,Host]),

    {ok,ReportHandlerPid} = ssh_eqc_event_handler:add_report_handler(),

    %% Connect to it with the {Type,Alg} to force a failure and log entry:
    {error,_} = R =
        ssh:connect(Host, Port, 
                    [{preferred_algorithms, [{Type,[Alg]}]},
                     {max_log_item_len, 10},
                     {silently_accept_hosts, true},
                     {save_accepted_host, false},
                     {user_dir, UserDir},
                     {user_interaction, false},
                     {user, "carni"},
                     {password, "meat"}
                    ]),

    {ok, Reports} = ssh_eqc_event_handler:get_reports(ReportHandlerPid),
    ct:log("~p:~p ssh:connect -> ~p~n~p", [?MODULE,?LINE,R,Reports]),

    [ok] =
        lists:usort(
          [check_skip_part(
             string:tokens(
               lists:flatten(io_lib:format(Fmt,Args)),
               " \n"))
           || {info_msg,_,{_,Fmt,Args}} <- Reports]
          ).


check_skip_part(["Disconnect","...","("++_NumSkipped, "bytes","skipped)"]) ->
    ok;
check_skip_part([_|T]) ->
    check_skip_part(T);
check_skip_part([]) ->
    error.

select_alg([{Type,[A,_|_]}|_]) when is_atom(A) -> {ok, {Type,A}};
select_alg([{Type,[{Dir,[A,_|_]}, _]}|_]) when is_atom(A), is_atom(Dir) -> {ok, {Type,A}};
select_alg([{Type,[_,{Dir,[A,_|_]}]}|_]) when is_atom(A), is_atom(Dir) -> {ok, {Type,A}};
select_alg([_|Algs]) -> select_alg(Algs);
select_alg([]) -> false.

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
                                        {save_accepted_host, false},
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

	    %% N w try one more than allowed:
	    ct:log("Info Report expected here (if not disabled) ...",[]),
	    try Connect(Host,Port)
	    of
		_ConnectionRef1 ->
		    ssh:stop_daemon(Pid),
		    {fail,"Too many connections accepted"}
	    catch
		error:{badmatch,{error,"Connection closed"}} ->
                    ct:log("Step 2 ok: could not set up too many connections. Good.",[]),
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
    {ok,Tref} = timer:send_after(30000, timeout_no_connection), % give the supervisors some time...
    try_to_connect(Connect, Host, Port, Pid, Tref, 1). % will take max 3300 ms after 11 tries

try_to_connect(Connect, Host, Port, Pid, Tref, N) ->
     try Connect(Host,Port)
     of
	 _ConnectionRef1 ->
	     timer:cancel(Tref),
             ct:log("Step 3 ok: could set up one more connection after killing one. That's good.",[]),
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
max_sessions_drops_tcp_connects() ->
    [{timetrap,{minutes,20}}].

max_sessions_drops_tcp_connects(Config) ->
    MaxSessions = 20,
    UseSessions = 2, % Must be =< MaxSessions
    FloodSessions = 1000,
    ParallelLogin = true,
    NegTimeOut = 8*1000,
    HelloTimeOut = 200,

    %% Start a test daemon
    SystemDir = filename:join(proplists:get_value(priv_dir, Config), system),
    UserDir = proplists:get_value(priv_dir, Config),
    {Pid, Host0, Port} =
        ssh_test_lib:daemon([
                             {system_dir, SystemDir},
                             {user_dir, UserDir},
                             {user_passwords, [{"carni", "meat"}]},
                             {parallel_login, ParallelLogin},
                             {hello_timeout, HelloTimeOut},
                             {negotiation_timeout, NegTimeOut},
                             {max_sessions, MaxSessions}
                            ]),
    Host = ssh_test_lib:mangle_connect_address(Host0),
    ct:log("~p:~p ~p Listen ~p:~p for max ~p sessions. Mangled Host = ~p",
           [?MODULE,?LINE,Pid,Host0,Port,MaxSessions,Host]),
    
    %% Log in UseSessions connections
    SSHconnect = fun(N) ->
                         R = ssh:connect(Host, Port, 
                                         [{silently_accept_hosts, true},
                                          {save_accepted_host, false},
                                          {user_dir, proplists:get_value(priv_dir,Config)},
                                          {user_interaction, false},
                                          {user, "carni"},
                                          {password, "meat"}
                                         ]),
                         ct:log("~p:~p ~p: ssh:connect -> ~p", [?MODULE,?LINE,N,R]),
                         R
                 end,

    L1 = oks([SSHconnect(N) || N <- lists:seq(1,UseSessions)]),
    case length(L1) of
        UseSessions ->
            %% As expected
            %% Try gen_tcp:connect
            [ct:log("~p:~p ~p: gen_tcp:connect -> ~p", 
                    [?MODULE,?LINE, N, gen_tcp:connect(Host, Port, [])])
             || N <- lists:seq(UseSessions+1, MaxSessions)
            ],

            ct:log("~p:~p Now try ~p gen_tcp:connect to be rejected", [?MODULE,?LINE,FloodSessions]),
            [ct:log("~p:~p ~p: gen_tcp:connect -> ~p", 
                    [?MODULE,?LINE, N, gen_tcp:connect(Host, Port, [])])
             || N <- lists:seq(MaxSessions+1, MaxSessions+1+FloodSessions)
            ],
            
            ct:log("~p:~p try ~p ssh:connect", [?MODULE,?LINE, MaxSessions - UseSessions]),
            try_ssh_connect(MaxSessions - UseSessions, NegTimeOut, SSHconnect);

        Len1 ->
            {fail, Len1}
    end.

try_ssh_connect(N, NegTimeOut, F) when N>0 ->
    case F(N) of
        {ok,_} ->
            try_ssh_connect(N-1, NegTimeOut, F);
        {error,_} when N==1 ->
            try_ssh_connect(N, NegTimeOut, F);
        {error,_} ->
            timer:sleep(NegTimeOut),
            try_ssh_connect(N, NegTimeOut, F)
    end;
try_ssh_connect(_N, _NegTimeOut, _F) ->
    done.


oks(L) -> lists:filter(fun({ok,_}) -> true;
                          (_) -> false
                       end, L).
    
%%--------------------------------------------------------------------
save_accepted_host_option(Config) ->
    UserDir = proplists:get_value(user_dir, Config),
    KnownHosts = filename:join(UserDir, "known_hosts"),
    SysDir = proplists:get_value(data_dir, Config),	  
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
					     {user_dir, UserDir},
					     {user_passwords, [{"vego", "morot"}]}
                                            ]),
    {error,enoent} = file:read_file(KnownHosts),

    {ok,_C1} = ssh:connect(Host, Port, [{silently_accept_hosts, true},
                                        {save_accepted_host, false},
                                        {user, "vego"},
                                        {password, "morot"},
                                        {user_interaction, false},
                                        {user_dir, UserDir}]),
    {error,enoent} = file:read_file(KnownHosts),
    
    {ok,_C2} = ssh:connect(Host, Port, [{silently_accept_hosts, true},
                                        {user, "vego"},
                                        {password, "morot"},
                                        {user_interaction, false},
                                        {user_dir, UserDir}]),
    {ok,_} = file:read_file(KnownHosts),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
raw_option(_Config) ->
    Opts = [{raw,1,2,3,4}],
    #{socket_options := Opts} = ssh_options:handle_options(client, Opts),
    #{socket_options := Opts} = ssh_options:handle_options(server, Opts).

%%--------------------------------------------------------------------
config_file(Config) ->
    %% First find common algs:
    ServerAlgs = ssh_test_lib:default_algorithms(sshd),
    OurAlgs = ssh_transport:supported_algorithms(), % Incl disabled but supported
    CommonAlgs = ssh_test_lib:intersection(ServerAlgs, OurAlgs),
    ct:log("ServerAlgs =~n~p~n~nOurAlgs =~n~p~n~nCommonAlgs =~n~p",[ServerAlgs,OurAlgs,CommonAlgs]),   
    Nkex = length(proplists:get_value(kex, CommonAlgs, [])),

    %% Adjust for very old ssh daemons that only supports ssh-rsa and ssh-dss:
    AdjustClient =
        case proplists:get_value(public_key,ServerAlgs,[]) -- ['ssh-rsa','ssh-dss'] of
            [] ->
                %% Old, let the client support them also:
                ct:log("Adjust the client's public_key set", []),
                [{public_key, ['ssh-rsa','ssh-dss']}];
            [_|_] ->
                %% Ok, let the client be un-modified:
                []
        end,

    case {ServerAlgs, ssh_test_lib:some_empty(CommonAlgs)} of
        {[],_} ->
            {skip, "No server algorithms found"};
        {_,true} ->
            {fail, "Missing common algorithms"};
        _ when Nkex<3 ->
            {skip, "Not enough number of common kex"};
        _ ->
            %% Then find three common kex and one common cipher:
            [K1a,K1b,K2a|_] = proplists:get_value(kex, CommonAlgs),
            [{_,[Ch1|_]}|_] = proplists:get_value(cipher, CommonAlgs),

            %% Make config file:
            {ok,ConfFile} = 
                make_config_file_in_privdir(
                  "c2.config", Config,
                  [{ssh, [{preferred_algorithms,
                           [{cipher, [Ch1]},
                            {kex,    [K1a]}
                           ] ++ AdjustClient},
                          {client_options,
                           [{modify_algorithms,
                             [{rm,     [{kex, [K1a]}]},
                              {append, [{kex, [K1b]}]}
                             ]}
                           ]}
                         ]}
                  ]),

            %% Start the slave node with the configuration just made:
            {ok, Peer, Node} = ?CT_PEER(["-config", ConfFile]),

            R0 = rpc:call(Node, ssh, default_algorithms, []),
            ct:log("R0 = ~p",[R0]),
            R0 = ssh:default_algorithms(),

            %% Start ssh on the slave. This should apply the ConfFile:
            rpc:call(Node, ssh, start, []),

            R1 = rpc:call(Node, ssh, default_algorithms, []),
            ct:log("R1 = ~p",[R1]),
            [{kex,[K1a]},
             {public_key,_},
             {cipher,[{_,[Ch1]},
                      {_,[Ch1]}]} | _] = R1,

            %% First connection. The client_options should be applied:
            {ok,C1} = rpc:call(Node, ssh, connect, [loopback, ?SSH_DEFAULT_PORT,
                                                    [{silently_accept_hosts, true},
                                                     {save_accepted_host, false},
                                                     {user_interaction, false}
                                                    ]]),
            ct:log("C1 = ~n~p", [C1]),
            {algorithms,As1} = rpc:call(Node, ssh, connection_info, [C1, algorithms]),
            K1b = proplists:get_value(kex, As1),
            Ch1 = proplists:get_value(encrypt, As1),
            Ch1 = proplists:get_value(decrypt, As1),
            {options,Os1} = rpc:call(Node, ssh, connection_info, [C1, options]),
            ct:log("C1 algorithms:~n~p~n~noptions:~n~p", [As1,Os1]),

            %% Second connection, the Options take precedence:
            C2_Opts = [{modify_algorithms,[{rm,[{kex,[K1b]}]}, % N.B.
                                           {append, [{kex,[K2a]}]}]},
                       {silently_accept_hosts, true},
                       {save_accepted_host, false},
                       {user_interaction, false}
                      ],
            {ok,C2} = rpc:call(Node, ssh, connect, [loopback, ?SSH_DEFAULT_PORT, C2_Opts]),
            {algorithms,As2} = rpc:call(Node, ssh, connection_info, [C2, algorithms]),
            K2a = proplists:get_value(kex, As2),
            Ch1 = proplists:get_value(encrypt, As2),
            Ch1 = proplists:get_value(decrypt, As2),
            {options,Os2} = rpc:call(Node, ssh, connection_info, [C2, options]),
            ct:log("C2 opts:~n~p~n~nalgorithms:~n~p~n~noptions:~n~p", [C2_Opts,As2,Os2]),

            peer:stop(Peer)
    end.
    
%%%----------------------------------------------------------------
config_file_modify_algorithms_order(Config) ->
    %% First find common algs:
    ServerAlgs = ssh_test_lib:default_algorithms(sshd),
    OurAlgs = ssh_transport:supported_algorithms(), % Incl disabled but supported
    CommonAlgs = ssh_test_lib:intersection(ServerAlgs, OurAlgs),
    ct:log("ServerAlgs =~n~p~n~nOurAlgs =~n~p~n~nCommonAlgs =~n~p",[ServerAlgs,OurAlgs,CommonAlgs]),
    Nkex = length(proplists:get_value(kex, CommonAlgs, [])),
    case {ServerAlgs, ssh_test_lib:some_empty(CommonAlgs)} of
        {[],_} ->
            {skip, "No server algorithms found"};
        {_,true} ->
            {fail, "Missing common algorithms"};
        _ when Nkex<3 ->
            {skip, "Not enough number of common kex"};
        _ ->
            %% Then find three common kex and one common cipher:
            [K1,K2,K3|_] = proplists:get_value(kex, CommonAlgs),
            [{_,[Ch1|_]}|_] = proplists:get_value(cipher, CommonAlgs),

            %% Make config file:
            {ok, ConfFile} =
                make_config_file_in_privdir(
                  "c3.config", Config,
                  [{ssh, [{preferred_algorithms,
                           [{cipher, [Ch1]},
                            {kex,    [K1]}
                           ]},
                          {server_options,
                           [{modify_algorithms,
                             [{rm,     [{kex, [K1]}]},
                              {append, [{kex, [K2]}]}
                             ]}
                           ]},
                          {client_options,
                           [{modify_algorithms,
                             [{rm,     [{kex, [K1]}]},
                              {append, [{kex, [K3]}]}
                             ]}
                           ]}
                         ]}
                  ]),

            %% Start the slave node with the configuration just made:
            {ok, Peer, Node} = ?CT_PEER(["-config", ConfFile]),
    
            R0 = rpc:call(Node, ssh, default_algorithms, []),
            ct:log("R0 = ~p",[R0]),
            R0 = ssh:default_algorithms(),

            %% Start ssh on the slave. This should apply the ConfFile:
            ok = rpc:call(Node, ssh, start, []),
            R1 = rpc:call(Node, ssh, default_algorithms, []),
            ct:log("R1 = ~p",[R1]),
            [{kex,[K1]} | _] = R1,

            %% Start a daemon
            {Server, Host, Port} = rpc:call(Node, ssh_test_lib, std_daemon, [Config, []]),
            {ok,ServerInfo} = rpc:call(Node, ssh, daemon_info, [Server]),
            ct:log("ServerInfo =~n~p", [ServerInfo]),

            %% Test that the server_options env key works:
            [K2] = proplists:get_value(kex,
                   proplists:get_value(preferred_algorithms,
                   proplists:get_value(options, ServerInfo))),
            
            {badrpc, {'EXIT', {{badmatch,ExpectedError}, _}}} =
                %% No common kex algorithms expected. 
                rpc:call(Node, ssh_test_lib, std_connect, [Config, Host, Port, []]), 
            {error,"Key exchange failed"} = ExpectedError,

            C = rpc:call(Node, ssh_test_lib, std_connect,
                         [Config, Host, Port, 
                          [{modify_algorithms,[{append,[{kex,[K2]}]}]}]]),
            ConnInfo = rpc:call(Node, ssh, connection_info, [C]),
            ct:log("ConnInfo =~n~p", [ConnInfo]),
            Algs = proplists:get_value(algorithms, ConnInfo),
            ct:log("Algs =~n~p", [Algs]),
            ConnOptions = proplists:get_value(options, ConnInfo),
            ConnPrefAlgs = proplists:get_value(preferred_algorithms, ConnOptions),

            %% And now, are all levels applied in right order:
            [K3,K2] = proplists:get_value(kex, ConnPrefAlgs),

            peer:stop(Peer)
    end.

    
%%--------------------------------------------------------------------
daemon_replace_options_simple(Config) ->
    SysDir = proplists:get_value(data_dir, Config),

    UserDir1 = proplists:get_value(user_dir, Config),
    UserDir2 = filename:join(UserDir1, "foo"),
    file:make_dir(UserDir2),

    {Pid, _Host, _Port} = ssh_test_lib:daemon([{system_dir, SysDir},
                                               {user_dir, UserDir1}
                                              ]),
    {ok,Opts1} = ssh:daemon_info(Pid),
    UserDir1 = proplists:get_value(user_dir, proplists:get_value(options,Opts1,[])),

    {ok, Pid} = ssh:daemon_replace_options(Pid, [{user_dir,UserDir2}]),
    {ok,Opts2} = ssh:daemon_info(Pid),
    case proplists:get_value(user_dir, proplists:get_value(options,Opts2,[])) of
        UserDir2 ->
            ok;
        UserDir1 ->
            ct:log("~p:~p Got old value ~p~nExpected ~p", [?MODULE,?LINE,UserDir1,UserDir2]),
            {fail, "Not changed"};
        Other ->
            ct:log("~p:~p Got ~p~nExpected ~p", [?MODULE,?LINE,Other,UserDir2]),
            {fail, "Strange value"}
    end.

%%--------------------------------------------------------------------
daemon_replace_options_algs(Config) ->
    SysDir = proplists:get_value(data_dir, Config),
    UserDir = proplists:get_value(user_dir, Config),

    DefaultKex =
        ssh_transport:default_algorithms(kex),
    NonDefaultKex =
        ssh_transport:supported_algorithms(kex) -- DefaultKex,

    case NonDefaultKex of
        [A1|_] ->
            [A2,A3|_] = DefaultKex,
            {Pid, _Host, _Port} = ssh_test_lib:daemon([{system_dir, SysDir},
                                                       {user_dir, UserDir},
                                                       {preferred_algorithms,[{kex,[A1]}]}
                                                      ]),
            [A1] = get_preferred_algorithms(Pid, kex),
            {ok, Pid} =
                ssh:daemon_replace_options(Pid, [{modify_algorithms,
                                                  [{prepend,[{kex,[A2]}]}]
                                                 }
                                                ]),
            [A2,A1] = get_preferred_algorithms(Pid, kex),

            {ok, Pid} =
                ssh:daemon_replace_options(Pid, [{preferred_algorithms,[{kex,[A3]}]
                                                 }
                                                ]),
            [A2,A3] = get_preferred_algorithms(Pid, kex)
            ;
        [] ->
            {skip, "No non-default kex"}
    end.

%%--------------------------------------------------------------------
daemon_replace_options_algs_connect(Config) ->
    [A1,A2|_] =
        ssh_transport:default_algorithms(kex),

    {Pid, Host, Port} =
        ssh_test_lib:std_daemon(Config,
                                [{preferred_algorithms,[{kex,[A1]}]}
                                ]),
    [A1] = get_preferred_algorithms(Pid, kex),

    %% Open a connection with A1 as kex and test it
    C1 =
        ssh_test_lib:std_connect(Config, Host, Port,
                                 [{preferred_algorithms,[{kex,[A1]}]}
                                 ]),
    ok = test_connection(C1),
    ok = test_not_connect(Config, Host, Port,
                          [{preferred_algorithms,[{kex,[A2]}]}
                          ]),

    %% Change kex to A2
    {ok, Pid} =
        ssh:daemon_replace_options(Pid,
                                   [{preferred_algorithms,[{kex,[A2]}]}]),
    [A2] = get_preferred_algorithms(Pid, kex),

    %% and open the second connection with this kex, and test it
    C2 =
        ssh_test_lib:std_connect(Config, Host, Port,
                                 [{preferred_algorithms,[{kex,[A2]}]}
                                 ]),
    ok = test_connection(C2),
    ok = test_not_connect(Config, Host, Port,
                          [{preferred_algorithms,[{kex,[A1]}]}
                          ]),

    %% Test that the first connection is still alive:
    ok = test_connection(C1),

    ssh:close(C1),
    ssh:close(C2),
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
daemon_replace_options_algs_conf_file(Config) ->
    SysDir = proplists:get_value(data_dir, Config),
    UserDir = proplists:get_value(user_dir, Config),

    DefaultKex =
        ssh_transport:default_algorithms(kex),
    NonDefaultKex =
        ssh_transport:supported_algorithms(kex) -- DefaultKex,

    case NonDefaultKex of
        [A0,A1|_] ->
            %% Make config file:
            {ok,ConfFile} =
                make_config_file_in_privdir(
                  "c4.config", Config,
                  [{ssh, [{modify_algorithms,
                           %% Whatever happens, always put A0 first in the kex list:
                           [{prepend, [{kex, [A0]}]}
                           ]}
                         ]}
                  ]),

            [A2|_] = DefaultKex,
            ct:log("[A0, A1, A2] = ~p", [[A0, A1, A2]]),

            %% Start the slave node with the configuration just made:
            {ok, Peer, Node} = ?CT_PEER(["-config", ConfFile]),

            %% Start ssh on the slave. This should apply the ConfFile:
            rpc:call(Node, ssh, start, []),

            {Pid, _Host, _Port} =
                rpc:call(Node, ssh_test_lib, daemon,
                         [
                          [{system_dir, SysDir},
                           {user_dir, UserDir},
                           {preferred_algorithms,[{kex,[A1]}]}
                          ]
                         ]),

            [A0,A1] =
                rpc:call(Node, ?MODULE, get_preferred_algorithms, [Pid, kex]),
            {ok, Pid} =
                rpc:call(Node, ssh, daemon_replace_options,
                         [Pid,
                          [{modify_algorithms,
                            [{prepend,[{kex,[A2]}]}]
                           }
                          ]
                         ]),

            %% Check that the precedens order is fulfilled:
            [A2,A0,A1] =
                rpc:call(Node, ?MODULE, get_preferred_algorithms, [Pid, kex]),

            peer:stop(Peer);
        [] ->
            {skip, "No non-default kex"}
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
	_:_ -> true %% ssh not started so we don't know
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


make_config_file_in_privdir(FileName, Config, Contents) ->
    %% write the file:
    PrivDir = proplists:get_value(priv_dir, Config),
    ConfFile = filename:join(PrivDir, FileName),
    {ok,D} = file:open(ConfFile, [write]),
    io:format(D, "~p.~n", [Contents]),
    file:close(D),
    {ok,Cnfs} = file:read_file(ConfFile),
    ct:log("Config file ~p :~n~s", [ConfFile,Cnfs]),
    {ok,ConfFile}.


get_preferred_algorithms(Pid, Type) ->
    {ok,#{preferred_algorithms:=As}} = ssh_system_sup:get_acceptor_options(Pid),
    proplists:get_value(Type, As).

test_connection(C) ->
    {ok, Ch} = ssh_connection:session_channel(C, infinity),
    A = rand:uniform(100),
    B = rand:uniform(100),
    A_plus_B = lists:concat([A,"+",B,"."]),
    Sum = integer_to_binary(A+B),
    success = ssh_connection:exec(C, Ch, A_plus_B, infinity),
    expected = ssh_test_lib:receive_exec_result(
                 {ssh_cm, C, {data, Ch, 0, Sum}} ),
    ssh_test_lib:receive_exec_end(C, Ch),
    ok.

test_not_connect(Config, Host, Port, Opts) ->
    try
        ssh_test_lib:std_connect(Config, Host, Port, Opts)
    of
        Cx when is_pid(Cx) -> {error, connected}
    catch
        error:{badmatch, {error,_}} -> ok
    end.

