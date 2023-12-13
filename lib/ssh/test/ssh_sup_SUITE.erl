%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2023. All Rights Reserved.
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
-include("ssh.hrl").
-include("ssh_test_lib.hrl").

-export([suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([default_tree/1,
         killed_acceptor_restarts/1,
         shell_channel_tree/1,
         sshc_subtree/1,
         sshd_subtree/1,
         sshd_subtree_profile/1]).

-define(USER, "Alladin").
-define(PASSWD, "Sesame").

-define(WAIT_FOR_SHUTDOWN, 500).

-define(SSHC_SUP(Pid), {sshc_sup, Pid, supervisor, [supervisor]}).
-define(SSHD_SUP(Pid), {sshd_sup, Pid, supervisor, [supervisor]}).
-define(SYSTEM_SUP(Pid,Address),
        {{ssh_system_sup, Address}, Pid, supervisor,[ssh_system_sup]}).
-define(SUB_SYSTEM_SUP(Pid), {_,Pid, supervisor,[ssh_subsystem_sup]}).
-define(ACCEPTOR_SUP(Pid,Address),
        {{ssh_acceptor_sup,Address},Pid,supervisor,[ssh_acceptor_sup]}).
-define(ACCEPTOR_WORKER(Pid,Address),
        {{ssh_acceptor_sup,Address},Pid,worker,[ssh_acceptor]}).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,100}}].

all() ->
    [default_tree, sshc_subtree, sshd_subtree, sshd_subtree_profile,
     killed_acceptor_restarts, shell_channel_tree].

groups() ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_suite(Config) ->
    ?CHECK_CRYPTO(
       begin
	   PrivDir = proplists:get_value(priv_dir, Config),
	   UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
	   file:make_dir(UserDir),
	   [{userdir, UserDir} | Config]
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
default_tree(Config) when is_list(Config) ->
    TopSupChildren = supervisor:which_children(ssh_sup),
    2 = length(TopSupChildren),
    {value, ?SSHC_SUP(_)} = lists:keysearch(sshc_sup, 1, TopSupChildren),
    {value, ?SSHD_SUP(_)} = lists:keysearch(sshd_sup, 1, TopSupChildren),
    ?wait_match([], supervisor:which_children(sshc_sup)),
    ?wait_match([], supervisor:which_children(sshd_sup)).

%%-------------------------------------------------------------------------
sshc_subtree(Config) when is_list(Config) ->
    {_Pid, Host, Port} = proplists:get_value(server, Config),
    UserDir = proplists:get_value(userdir, Config),
    ?wait_match([], supervisor:which_children(sshc_sup)),
    Pid1 = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
                                             {save_accepted_host, false},
                                             {user_interaction, false},
                                             {user, ?USER},
                                             {password, ?PASSWD},
                                             {user_dir, UserDir}]),
    ?wait_match([?SYSTEM_SUP(SysSup,
                             #address{address=LocalIP,
                                      port=LocalPort,
                                      profile=?DEFAULT_PROFILE})],
		supervisor:which_children(sshc_sup),
                [SysSup, LocalIP, LocalPort]),
    check_sshc_system_tree(SysSup, Pid1, LocalIP, LocalPort, Config),
    Pid2 = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
                                             {save_accepted_host, false},
                                             {user_interaction, false},
                                             {user, ?USER},
                                             {password, ?PASSWD},
                                             {user_dir, UserDir}]),
    ?wait_match([?SYSTEM_SUP(_,_),
                 ?SYSTEM_SUP(_,_)
                ],
		supervisor:which_children(sshc_sup)),
    ssh:close(Pid1),
    ?wait_match([?SYSTEM_SUP(_,_)
                ],
		supervisor:which_children(sshc_sup)),
    ssh:close(Pid2),
    ?wait_match([], supervisor:which_children(sshc_sup)).

%%-------------------------------------------------------------------------
sshd_subtree(Config) when is_list(Config) ->
    SystemDir = proplists:get_value(data_dir, Config),
    {Daemon, HostIP, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
                                                  {failfun, fun ssh_test_lib:failfun/2},
                                                  {user_passwords,
                                                   [{?USER, ?PASSWD}]}]),
    ct:log("Expect HostIP=~p, Port=~p, Daemon=~p",[HostIP,Port,Daemon]),
    ?wait_match([?SYSTEM_SUP(Daemon, #address{address=ListenIP,
                                              port=Port,
                                              profile=?DEFAULT_PROFILE})],
		supervisor:which_children(sshd_sup),
		[ListenIP,Daemon]),
    true = ssh_test_lib:match_ip(HostIP, ListenIP),
    check_sshd_system_tree(Daemon, HostIP, Port, Config),
    ssh:stop_daemon(HostIP, Port),
    ct:sleep(?WAIT_FOR_SHUTDOWN),
    ?wait_match([], supervisor:which_children(sshd_sup)).

%%-------------------------------------------------------------------------
sshd_subtree_profile(Config) when is_list(Config) ->
    Profile = proplists:get_value(profile, Config),
    SystemDir = proplists:get_value(data_dir, Config),
    {Daemon, HostIP, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
                                                  {failfun, fun ssh_test_lib:failfun/2},
                                                  {user_passwords,
                                                   [{?USER, ?PASSWD}]},
                                                  {profile, Profile}]),
    ct:log("Expect HostIP=~p, Port=~p, Profile=~p, Daemon=~p",[HostIP,Port,Profile,Daemon]),
    ?wait_match([?SYSTEM_SUP(Daemon, #address{address=ListenIP,
                                              port=Port,
                                              profile=Profile})],
		supervisor:which_children(sshd_sup),
		[ListenIP,Daemon]),
    true = ssh_test_lib:match_ip(HostIP, ListenIP),
    check_sshd_system_tree(Daemon, HostIP, Port, Config),
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
    C1 = ssh_test_lib:connect("localhost", Port, [{silently_accept_hosts, true},
                                                   {save_accepted_host, false},
                                              {user_interaction, false},
                                              {user, ?USER},
                                              {password, ?PASSWD},
                                              {user_dir, UserDir}]),
    [{client_version,_}] = ssh:connection_info(C1,[client_version]),

    ct:log("~s",[lists:flatten(ssh_info:string())]),

    %% Make acceptor restart:
    ct:log("Expect a SUPERVISOR REPORT with offender {pid,~p}....~n", [AccPid]),
    exit(AccPid, kill),
    ?wait_match(undefined, process_info(AccPid)),

    %% Check it is a new acceptor and wait if it is not:
    ?wait_match({ok,[{AccPid1,ListenAddr,Port}]}, AccPid1=/=AccPid,
                acceptor_pid(DaemonPid),
                AccPid1,
                500, 30),

    ct:log("... now there should not be any SUPERVISOR REPORT.~n", []),

    true = (AccPid1 =/= AccPid2),

    %% Connect second client and check it is alive:
    C2 =
        case ssh:connect("localhost", Port, [{silently_accept_hosts, true},
                                             {save_accepted_host, false},
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

    [SubSysSup,_ChPid|_] = Sups0 = chk_empty_con_daemon(Daemon),

    {ok, ChannelId0} = ssh_connection:session_channel(ConnectionRef, infinity),
    ok = ssh_connection:shell(ConnectionRef,ChannelId0),
    success = ssh_connection:ptty_alloc(ConnectionRef, ChannelId0, [{pty_opts,[{onlcr,1}]}]),

    ?wait_match([{_,_, supervisor,[ssh_tcpip_forward_acceptor_sup]},
                 {_,ChSup,supervisor,[ssh_channel_sup]},
                 {connection,_,worker,[ssh_connection_handler]}
                ],
		supervisor:which_children(SubSysSup),
                [ChSup]),
    ?wait_match([{_,GroupPid,worker,[ssh_server_channel]}
                ],
                supervisor:which_children(ChSup),
                [GroupPid]),


    {links,GroupLinks} = erlang:process_info(GroupPid, links),
    ct:log("GroupPid = ~p, GroupLinks = ~p Sups0 = ~p",[GroupPid,GroupLinks,Sups0]),
    [ShellPid] = GroupLinks--[ChSup],
    ct:log("GroupPid = ~p, ShellPid = ~p",[GroupPid,ShellPid]),

    receive
        {ssh_cm,ConnectionRef, {data, ChannelId0, 0, <<"TimeoutShell started!",Rest/binary>>}} ->
            ct:log("TimeoutShell started. Rest = ~p", [Rest]),
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
                    ct:log("~p:~p  Flush unexpected: ~p", [?MODULE,?LINE,flush_rest()]),
                    ssh:close(ConnectionRef),
                    ssh:stop_daemon(Daemon),
                    ct:fail("CLI Timeout 1")
            end
    after 10000 ->
            ct:log("~p:~p  Flush unexpected: ~p", [?MODULE,?LINE,flush_rest()]),
            ssh:close(ConnectionRef),
            ssh:stop_daemon(Daemon),
            ct:fail("CLI Timeout 2")
    end.

chk_empty_con_daemon(Daemon) ->
    ?wait_match([?SUB_SYSTEM_SUP(SubSysSup),
		 ?ACCEPTOR_SUP(AccSup,_)
                ],
		supervisor:which_children(Daemon),
                [SubSysSup,AccSup]),
    ?wait_match([{_,FwdAccSup, supervisor,[ssh_tcpip_forward_acceptor_sup]},
                 {_,ChSup,supervisor,[ssh_channel_sup]},
                 {connection,ServerConnPid,worker,[ssh_connection_handler]}
                ],
		supervisor:which_children(SubSysSup),
		[ChSup,FwdAccSup,ServerConnPid]),
    ?wait_match([], supervisor:which_children(FwdAccSup)),
    ?wait_match([], supervisor:which_children(ChSup)),
    ?wait_match([?ACCEPTOR_WORKER(_,_)],
                supervisor:which_children(AccSup),
                []),
    [SubSysSup, ChSup, ServerConnPid, AccSup, FwdAccSup].

%%-------------------------------------------------------------------------
%% Help functions
%%-------------------------------------------------------------------------
check_sshd_system_tree(Daemon, Host, Port, Config) ->
    UserDir = proplists:get_value(userdir, Config),
    ClientConn = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
                                                   {user_interaction, false},
                                                   {user, ?USER},
                                                   {password, ?PASSWD},
                                                   {user_dir, UserDir}]),
    ?wait_match([?SUB_SYSTEM_SUP(SubSysSup),
		 ?ACCEPTOR_SUP(AccSup,_)],
		supervisor:which_children(Daemon),
                [SubSysSup,AccSup]),
    ?wait_match([{_,FwdAccSup, supervisor,[ssh_tcpip_forward_acceptor_sup]},
                 {_,_,supervisor,[ssh_channel_sup]},
                 {connection,ServerConn,worker,[ssh_connection_handler]}],
		supervisor:which_children(SubSysSup),
		[FwdAccSup,ServerConn]),
    ?wait_match([], supervisor:which_children(FwdAccSup)),
    ?wait_match([?ACCEPTOR_WORKER(_,_)], supervisor:which_children(AccSup)),
    {ok,PidC} = ssh_sftp:start_channel(ClientConn),
    ?wait_match([{_,FwdAccSup, supervisor,[ssh_tcpip_forward_acceptor_sup]},
                 {_,ChSup,supervisor,[ssh_channel_sup]},
                 {connection,ServerConn,worker,[ssh_connection_handler]}],
		supervisor:which_children(SubSysSup),
		[ChSup,ServerConn]),

    ?wait_match([{_,PidS,worker,[ssh_server_channel]}],
                supervisor:which_children(ChSup),
                [PidS]),
    true = (PidS =/= PidC),
    ?wait_match([], supervisor:which_children(FwdAccSup)),
    ssh:close(ClientConn).


check_sshc_system_tree(SysSup, Connection, _LocalIP, _LocalPort, _Config) ->
    ?wait_match([?SUB_SYSTEM_SUP(SubSysSup)],
                supervisor:which_children(SysSup),
                [SubSysSup]),
    ?wait_match([{_,FwdAccSup, supervisor,[ssh_tcpip_forward_acceptor_sup]},
                 {_,_,supervisor,[ssh_channel_sup]},
                 {connection,Connection,worker,[ssh_connection_handler]}
                ],
		supervisor:which_children(SubSysSup),
		[FwdAccSup]),
    ?wait_match([], supervisor:which_children(FwdAccSup)),

    {ok,ChPid1} = ssh_sftp:start_channel(Connection),
    ?wait_match([{_,FwdAccSup, supervisor,[ssh_tcpip_forward_acceptor_sup]},
                 {_,ChSup,supervisor, [ssh_channel_sup]},
                 {connection,Connection,worker,[ssh_connection_handler]}
                ],
		supervisor:which_children(SubSysSup),
		[ChSup,FwdAccSup]),

    ?wait_match([{_,ChPid1,worker,[ssh_client_channel]}
                ],
                supervisor:which_children(ChSup),
                [ChPid1]),

    {ok,ChPid2} = ssh_sftp:start_channel(Connection),
    ?wait_match([{_,FwdAccSup, supervisor,[ssh_tcpip_forward_acceptor_sup]},
                 {_,ChSup,supervisor, [ssh_channel_sup]},
                 {connection,Connection,worker,[ssh_connection_handler]}
                ],
		supervisor:which_children(SubSysSup),
		[ChSup,FwdAccSup]),

    ?wait_match([{_,ChPid2,worker,[ssh_client_channel]},
                 {_,ChPid1,worker,[ssh_client_channel]}
                ],
                supervisor:which_children(ChSup),
                [ChPid1,ChPid2]),

    ct:log("Expect a SUPERVISOR REPORT with offender {pid,~p}....~n", [ChPid1]),
    exit(ChPid1, kill),
    ?wait_match([{_,FwdAccSup, supervisor,[ssh_tcpip_forward_acceptor_sup]},
                 {_,ChSup,supervisor, [ssh_channel_sup]},
                 {connection,Connection,worker,[ssh_connection_handler]}
                ],
		supervisor:which_children(SubSysSup),
		[ChSup,FwdAccSup]),

    ?wait_match([{_,ChPid2,worker,[ssh_client_channel]}
                ],
                supervisor:which_children(ChSup),
                [ChPid2]),

    ct:log("Expect a SUPERVISOR REPORT with offender {pid,~p}....~n", [ChPid2]),
    exit(ChPid2, kill),
    ?wait_match([{_,FwdAccSup, supervisor,[ssh_tcpip_forward_acceptor_sup]},
                 {_,ChSup,supervisor, [ssh_channel_sup]},
                 {connection,Connection,worker,[ssh_connection_handler]}
                ],
		supervisor:which_children(SubSysSup),
		[ChSup,FwdAccSup]),

    ?wait_match([], supervisor:which_children(ChSup)),

    ct:log("... now there should not be any SUPERVISOR REPORT.~n", []).



acceptor_pid(DaemonPid) ->
    Parent = self(),
    Pid = spawn(fun() ->
                        Parent ! {self(), supsearch,
                                  [{AccPid,ListenAddr,Port}

                                   || ?SYSTEM_SUP(DPid,#address{address=ListenAddr,port=Port,profile=NS})
                                          <- supervisor:which_children(sshd_sup),
                                      DPid == DaemonPid,

                                      ?ACCEPTOR_SUP(AccSupPid,_)
                                          <- supervisor:which_children(DaemonPid),

                                      ?ACCEPTOR_WORKER(AccPid, #address{address=L2,
                                                                        port=P2,
                                                                        profile=NS2})
                                          <- supervisor:which_children(AccSupPid),
                                      L2 == ListenAddr,
                                      P2 == Port,
                                      NS2 == NS]}
                end),
    receive {Pid, supsearch, L} -> {ok,L}
    after 2000 -> timeout
    end.

%%%----------------------------------------------------------------
flush_rest() -> lists:reverse(flush_rest([])).

flush_rest(Acc) ->
    receive Any -> [Any|Acc]
    after 0 -> Acc
    end.
