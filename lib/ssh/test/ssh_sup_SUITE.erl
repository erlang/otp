%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2016. All Rights Reserved.
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
    [default_tree, sshc_subtree, sshd_subtree, sshd_subtree_profile].

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

sshd_subtree() ->
    [{doc, "Make sure the sshd subtree is correct"}].
sshd_subtree(Config) when is_list(Config) ->
    HostIP = proplists:get_value(host_ip, Config),
    Port = proplists:get_value(port, Config),
    SystemDir = proplists:get_value(data_dir, Config),
    ssh:daemon(HostIP, Port, [{system_dir, SystemDir},
			      {failfun, fun ssh_test_lib:failfun/2},
			      {user_passwords,
			       [{?USER, ?PASSWD}]}]),

    ?wait_match([{{server,ssh_system_sup, HostIP, Port, ?DEFAULT_PROFILE},
		  Daemon, supervisor,
		  [ssh_system_sup]}],
		supervisor:which_children(sshd_sup),
		Daemon),
    check_sshd_system_tree(Daemon, Config),
    ssh:stop_daemon(HostIP, Port),
    ct:sleep(?WAIT_FOR_SHUTDOWN),
    ?wait_match([], supervisor:which_children(sshd_sup)).

sshd_subtree_profile() ->
    [{doc, "Make sure the sshd subtree using profile option is correct"}].	
sshd_subtree_profile(Config) when is_list(Config) ->
    HostIP = proplists:get_value(host_ip, Config),
    Port = proplists:get_value(port, Config),
    Profile = proplists:get_value(profile, Config), 
    SystemDir = proplists:get_value(data_dir, Config),

    {ok, _} = ssh:daemon(HostIP, Port, [{system_dir, SystemDir},
				  {failfun, fun ssh_test_lib:failfun/2},
				  {user_passwords,
				   [{?USER, ?PASSWD}]},
				  {profile, Profile}]),
    ?wait_match([{{server,ssh_system_sup, HostIP,Port,Profile},
		  Daemon, supervisor,
		  [ssh_system_sup]}],
		supervisor:which_children(sshd_sup),
		Daemon),
    check_sshd_system_tree(Daemon, Config),
    ssh:stop_daemon(HostIP, Port, Profile),
    ct:sleep(?WAIT_FOR_SHUTDOWN),
    ?wait_match([], supervisor:which_children(sshd_sup)).


check_sshd_system_tree(Daemon, Config) -> 
    Host = proplists:get_value(host, Config),
    Port = proplists:get_value(port, Config),
    UserDir = proplists:get_value(userdir, Config),
    {ok, Client} = ssh:connect(Host, Port, [{silently_accept_hosts, true},
						   {user_interaction, false},
						   {user, ?USER}, {password, ?PASSWD},{user_dir, UserDir}]),
    
    ?wait_match([{_,SubSysSup, supervisor,[ssh_subsystem_sup]},
		 {{ssh_acceptor_sup,_,_,_}, AccSup, supervisor,[ssh_acceptor_sup]}],
		supervisor:which_children(Daemon),
	       [SubSysSup,AccSup]),
    
    ?wait_match([{{server,ssh_connection_sup, _,_},
		  ConnectionSup, supervisor,
		  [ssh_connection_sup]},
		 {{server,ssh_channel_sup,_ ,_},
		  ChannelSup,supervisor,
		  [ssh_channel_sup]}],
		supervisor:which_children(SubSysSup),
		[ConnectionSup,ChannelSup]),
    
    ?wait_match([{{ssh_acceptor_sup,_,_,_},_,worker,[ssh_acceptor]}],
		supervisor:which_children(AccSup)),
    
    ?wait_match([{_, _, worker,[ssh_connection_handler]}],
		supervisor:which_children(ConnectionSup)),
    
    ?wait_match([], supervisor:which_children(ChannelSup)),
    
    ssh_sftp:start_channel(Client),

    ?wait_match([{_, _,worker,[ssh_channel]}],
		supervisor:which_children(ChannelSup)),
    ssh:close(Client).
  
