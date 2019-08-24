%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2018. All Rights Reserved.
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

-module(ssh_sup_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("ssh/src/ssh.hrl").
-include("ssh_test_lib.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(USER, "Alladin").
-define(PASSWD, "Sesame").

-define(WAIT_FOR_SHUTDOWN, 500).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,100}}].

all() -> 
    [default_tree, sshc_subtree, sshd_subtree, sshd_subtree_profile,
     killed_acceptor_restarts,
     shell_channel_tree
    ].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_suite(Config) ->
    ?CHECK_CRYPTO(
       begin
	   Port = ssh_test_lib:inet_port(node()),
	   PrivDir = proplists:get_value(priv_dir, Config),
	   UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
	   file:make_dir(UserDir),
	   [{userdir, UserDir},{port, Port}, {host, "localhost"}, {host_ip, any} | Config]
       end).

end_per_suite(_) ->
    ok.

init_per_testcase(sshc_subtree, Config) ->  
    ssh:start(),
    SystemDir = proplists:get_value(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
					      {failfun, fun ssh_test_lib:failfun/2},
					      {user_passwords,
					       [{?USER, ?PASSWD}]}]),
    [{server, {Pid, Host, Port}} | Config];
init_per_testcase(Case, Config) ->
    end_per_testcase(Case, Config),
    ssh:start(),
    Config.
end_per_testcase(sshc_subtree, Config) ->
    {Pid,_,_} = proplists:get_value(server, Config), 
    ssh:stop_daemon(Pid),
    ssh:stop();
end_per_testcase(_, _Config) ->
    ssh:stop().

%%-------------------------------------------------------------------------
%% Test cases 
%%-------------------------------------------------------------------------
default_tree() ->
    [{doc, "Makes sure the correct processes are started and linked," 
     "in the default case."}].
default_tree(Config) when is_list(Config) ->
    TopSupChildren = supervisor:which_children(ssh_sup),
    2 = length(TopSupChildren),
    {value, {sshc_sup, _, supervisor,[sshc_sup]}} =
	lists:keysearch(sshc_sup, 1, TopSupChildren),
    {value, {sshd_sup, _,supervisor,[sshd_sup]}} = 
	lists:keysearch(sshd_sup, 1, TopSupChildren),
    ?wait_match([], supervisor:which_children(sshc_sup)),
    ?wait_match([], supervisor:which_children(sshd_sup)).

%%-------------------------------------------------------------------------
sshc_subtree() ->
    [{doc, "Make sure the sshc subtree is correct"}].
sshc_subtree(Config) when is_list(Config) ->
    {_Pid, Host, Port} = proplists:get_value(server, Config),
    UserDir = proplists:get_value(userdir, Config),

    ?wait_match([], supervisor:which_children(sshc_sup)),

    {ok, Pid1} = ssh:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_interaction, false},
					  {user, ?USER}, {password, ?PASSWD},{user_dir, UserDir}]),

    ?wait_match([{_, _,worker,[ssh_connection_handler]}],
		supervisor:which_children(sshc_sup)),

    {ok, Pid2} = ssh:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_interaction, false},
					  {user, ?USER}, {password, ?PASSWD}, {user_dir, UserDir}]),
    ?wait_match([{_,_,worker,[ssh_connection_handler]}, 
		 {_,_,worker,[ssh_connection_handler]}],
		supervisor:which_children(sshc_sup)),

    ssh:close(Pid1),
    ?wait_match([{_,_,worker,[ssh_connection_handler]}],
		supervisor:which_children(sshc_sup)),
    ssh:close(Pid2),
    ?wait_match([], supervisor:which_children(sshc_sup)).

%%-------------------------------------------------------------------------
sshd_subtree() ->
    [{doc, "Make sure the sshd subtree is correct"}].
sshd_subtree(Config) when is_list(Config) ->
    HostIP = proplists:get_value(host_ip, Config),
    Port = proplists:get_value(port, Config),
    SystemDir = proplists:get_value(data_dir, Config),
    {ok,Daemon} = ssh:daemon(HostIP, Port, [{system_dir, SystemDir},
                                            {failfun, fun ssh_test_lib:failfun/2},
                                            {user_passwords,
                                             [{?USER, ?PASSWD}]}]),

    ct:log("Expect HostIP=~p, Port=~p, Daemon=~p",[HostIP,Port,Daemon]),
    ?wait_match([{{server,ssh_system_sup, ListenIP, Port, ?DEFAULT_PROFILE},
		  Daemon, supervisor,
		  [ssh_system_sup]}],
		supervisor:which_children(sshd_sup),
		[ListenIP,Daemon]),
    true = ssh_test_lib:match_ip(HostIP, ListenIP),
    check_sshd_system_tree(Daemon, Config),
    ssh:stop_daemon(HostIP, Port),
    ct:sleep(?WAIT_FOR_SHUTDOWN),
    ?wait_match([], supervisor:which_children(sshd_sup)).

%%-------------------------------------------------------------------------
sshd_subtree_profile() ->
    [{doc, "Make sure the sshd subtree using profile option is correct"}].	
sshd_subtree_profile(Config) when is_list(Config) ->
    HostIP = proplists:get_value(host_ip, Config),
    Port = proplists:get_value(port, Config),
    Profile = proplists:get_value(profile, Config), 
    SystemDir = proplists:get_value(data_dir, Config),

    {ok, Daemon} = ssh:daemon(HostIP, Port, [{system_dir, SystemDir},
                                             {failfun, fun ssh_test_lib:failfun/2},
                                             {user_passwords,
                                              [{?USER, ?PASSWD}]},
                                             {profile, Profile}]),
    ct:log("Expect HostIP=~p, Port=~p, Profile=~p, Daemon=~p",[HostIP,Port,Profile,Daemon]),
    ?wait_match([{{server,ssh_system_sup, ListenIP,Port,Profile},
		  Daemon, supervisor,
		  [ssh_system_sup]}],
		supervisor:which_children(sshd_sup),
		[ListenIP,Daemon]),
    true = ssh_test_lib:match_ip(HostIP, ListenIP),
    check_sshd_system_tree(Daemon, Config),
    ssh:stop_daemon(HostIP, Port, Profile),
    ct:sleep(?WAIT_FOR_SHUTDOWN),
    ?wait_match([], supervisor:which_children(sshd_sup)).

%%-------------------------------------------------------------------------
killed_acceptor_restarts(Config) ->
    Profile = proplists:get_value(profile, Config), 
    SystemDir = proplists:get_value(data_dir, Config),
    UserDir = proplists:get_value(userdir, Config),
    {ok, DaemonPid} = ssh:daemon(0, [{system_dir, SystemDir},
                                     {failfun, fun ssh_test_lib:failfun/2},
                                     {user_passwords, [{?USER, ?PASSWD}]},
                                     {profile, Profile}]),

    {ok, DaemonPid2} = ssh:daemon(0, [{system_dir, SystemDir},
                                     {failfun, fun ssh_test_lib:failfun/2},
                                     {user_passwords, [{?USER, ?PASSWD}]},
                                     {profile, Profile}]),

    Port  = ssh_test_lib:daemon_port(DaemonPid),
    Port2 = ssh_test_lib:daemon_port(DaemonPid2),
    true = (Port /= Port2),

    {ok,[{AccPid,ListenAddr,Port}]} = acceptor_pid(DaemonPid),
    {ok,[{AccPid2,ListenAddr,Port2}]} = acceptor_pid(DaemonPid2),

    true = (AccPid /= AccPid2),

    %% Connect first client and check it is alive:
    {ok,C1} = ssh:connect("localhost", Port, [{silently_accept_hosts, true},
                                              {user_interaction, false},
                                              {user, ?USER},
                                              {password, ?PASSWD},
                                              {user_dir, UserDir}]),
    [{client_version,_}] = ssh:connection_info(C1,[client_version]),

    ct:log("~s",[lists:flatten(ssh_info:string())]),

    %% Make acceptor restart:
    exit(AccPid, kill),
    ?wait_match(undefined, process_info(AccPid)),

    %% Check it is a new acceptor and wait if it is not:
    ?wait_match({ok,[{AccPid1,ListenAddr,Port}]}, AccPid1=/=AccPid,
                acceptor_pid(DaemonPid),
                AccPid1,
                500, 30),

    true = (AccPid1 =/= AccPid2),

    %% Connect second client and check it is alive:
    C2 =
        case ssh:connect("localhost", Port, [{silently_accept_hosts, true},
                                             {user_interaction, false},
                                             {user, ?USER},
                                             {password, ?PASSWD},
                                             {user_dir, UserDir}]) of
            {ok,_C2} ->
                _C2;
            _Other ->
                ct:log("new connect failed: ~p~n~n~s",[_Other,lists:flatten(ssh_info:string())]),
                ct:fail("Re-connect failed!", [])
        end,

    [{client_version,_}] = ssh:connection_info(C2,[client_version]),
    
    ct:log("~s",[lists:flatten(ssh_info:string())]),

    %% Check first client is still alive:
    [{client_version,_}] = ssh:connection_info(C1,[client_version]),
    
    ok = ssh:stop_daemon(DaemonPid2),
    ?wait_match(undefined, process_info(DaemonPid2), 1000, 30),
    [{client_version,_}] = ssh:connection_info(C1,[client_version]),
    [{client_version,_}] = ssh:connection_info(C2,[client_version]),
    
    ok = ssh:stop_daemon(DaemonPid),
    ?wait_match(undefined, process_info(DaemonPid), 1000, 30),
    ?wait_match({error,closed}, ssh:connection_info(C1,[client_version]), 1000, 5),
    ?wait_match({error,closed}, ssh:connection_info(C2,[client_version]), 1000, 5).

%%-------------------------------------------------------------------------
shell_channel_tree(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    SysDir = proplists:get_value(data_dir, Config),
    TimeoutShell =
        fun() ->
                io:format("TimeoutShell started!~n",[]),
                timer:sleep(5000),
                ct:log("~p TIMEOUT!",[self()])
        end,
    {Daemon, Host, Port} = ssh_test_lib:daemon([{system_dir, SysDir},
                                                {user_dir, UserDir},
                                                {password, "morot"},
                                                {shell, fun(_User) ->
                                                                spawn(TimeoutShell)
                                                        end
                                                }
                                            ]),
    ConnectionRef = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
						      {user, "foo"},
						      {password, "morot"},
						      {user_interaction, true},
						      {user_dir, UserDir}]),

    [ChannelSup|_] = Sups0 = chk_empty_con_daemon(Daemon),
    
    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),
    ok = ssh_connection:shell(ConnectionRef,ChannelId0),

    ?wait_match([{_, GroupPid,worker,[ssh_server_channel]}],
		supervisor:which_children(ChannelSup),
               [GroupPid]),
    {links,GroupLinks} = erlang:process_info(GroupPid, links),
    [ShellPid] = GroupLinks--[ChannelSup],
    ct:log("GroupPid = ~p, ShellPid = ~p",[GroupPid,ShellPid]),

    receive
        {ssh_cm,ConnectionRef, {data, ChannelId0, 0, <<"TimeoutShell started!\r\n">>}} ->
            receive
                %%---- wait for the subsystem to terminate
                {ssh_cm,ConnectionRef,{closed,ChannelId0}} ->
                    ct:log("Subsystem terminated",[]),
                    case {chk_empty_con_daemon(Daemon),
                          process_info(GroupPid),
                          process_info(ShellPid)} of
                        {Sups0, undefined, undefined} ->
                            %% SUCCESS
                            ssh:stop_daemon(Daemon);
                        {Sups0, _, undefined}  ->
                            ssh:stop_daemon(Daemon),
                            ct:fail("Group proc lives!");
                        {Sups0, undefined, _}  ->
                            ssh:stop_daemon(Daemon),
                            ct:fail("Shell proc lives!");
                        _ ->
                            ssh:stop_daemon(Daemon),
                            ct:fail("Sup tree changed!")
                    end
            after 10000 ->
                    ssh:close(ConnectionRef),
                    ssh:stop_daemon(Daemon),
                    ct:fail("CLI Timeout")
            end
    after 10000 ->
            ssh:close(ConnectionRef),
            ssh:stop_daemon(Daemon),
            ct:fail("CLI Timeout")
    end.


chk_empty_con_daemon(Daemon) ->
    ?wait_match([{_,SubSysSup, supervisor,[ssh_subsystem_sup]},
		 {{ssh_acceptor_sup,_,_,_}, AccSup, supervisor,[ssh_acceptor_sup]}],
		supervisor:which_children(Daemon),
                [SubSysSup,AccSup]),
    ?wait_match([{{server,ssh_connection_sup, _,_},
		  ConnectionSup, supervisor,
		  [ssh_connection_sup]},
		 {{server,ssh_server_channel_sup,_ ,_},
		  ChannelSup,supervisor,
		  [ssh_server_channel_sup]}],
		supervisor:which_children(SubSysSup),
		[ConnectionSup,ChannelSup]),
    ?wait_match([{{ssh_acceptor_sup,_,_,_},_,worker,[ssh_acceptor]}],
		supervisor:which_children(AccSup)),
    ?wait_match([{_, _, worker,[ssh_connection_handler]}],
		supervisor:which_children(ConnectionSup)),
    ?wait_match([], supervisor:which_children(ChannelSup)),
    [ChannelSup, ConnectionSup, SubSysSup, AccSup].

%%-------------------------------------------------------------------------
%% Help functions
%%-------------------------------------------------------------------------
check_sshd_system_tree(Daemon, Config) -> 
    Host = proplists:get_value(host, Config),
    Port = proplists:get_value(port, Config),
    UserDir = proplists:get_value(userdir, Config),
    {ok, Client} = ssh:connect(Host, Port, [{silently_accept_hosts, true},
                                            {user_interaction, false},
                                            {user, ?USER},
                                            {password, ?PASSWD},
                                            {user_dir, UserDir}]),
    
    ?wait_match([{_,SubSysSup, supervisor,[ssh_subsystem_sup]},
		 {{ssh_acceptor_sup,_,_,_}, AccSup, supervisor,[ssh_acceptor_sup]}],
		supervisor:which_children(Daemon),
                [SubSysSup,AccSup]),
    
    ?wait_match([{{server,ssh_connection_sup, _,_},
		  ConnectionSup, supervisor,
		  [ssh_connection_sup]},
		 {{server,ssh_server_channel_sup,_ ,_},
		  ChannelSup,supervisor,
		  [ssh_server_channel_sup]}],
		supervisor:which_children(SubSysSup),
		[ConnectionSup,ChannelSup]),
    
    ?wait_match([{{ssh_acceptor_sup,_,_,_},_,worker,[ssh_acceptor]}],
		supervisor:which_children(AccSup)),
    
    ?wait_match([{_, _, worker,[ssh_connection_handler]}],
		supervisor:which_children(ConnectionSup)),
    
    ?wait_match([], supervisor:which_children(ChannelSup)),
    
    ssh_sftp:start_channel(Client),

    ?wait_match([{_, _,worker,[ssh_server_channel]}],
		supervisor:which_children(ChannelSup)),
    ssh:close(Client).

acceptor_pid(DaemonPid) ->
    Parent = self(),
    Pid = spawn(fun() ->
                        Parent ! {self(), supsearch,
                                  [{AccPid,ListenAddr,Port}

                                   || {{server,ssh_system_sup,ListenAddr,Port,NS},
                                       DPid,supervisor,
                                       [ssh_system_sup]} <- supervisor:which_children(sshd_sup),
                                      DPid == DaemonPid,

                                      {{ssh_acceptor_sup,L1,P1,NS1},
                                       AccSupPid,supervisor,
                                       [ssh_acceptor_sup]} <- supervisor:which_children(DaemonPid),
                                      L1 == ListenAddr,
                                      P1 == Port,
                                      NS1 == NS1,

                                      {{ssh_acceptor_sup,L2,P2,NS2},
                                       AccPid,worker,
                                       [ssh_acceptor]} <- supervisor:which_children(AccSupPid),
                                      L2 == ListenAddr,
                                      P2 == Port,
                                      NS2 == NS]}
                end),
    receive {Pid, supsearch, L} -> {ok,L}
    after 2000 -> timeout
    end.

