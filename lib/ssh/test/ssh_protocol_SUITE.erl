%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2015. All Rights Reserved.
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

-module(ssh_protocol_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/inet.hrl").
-include_lib("ssh/src/ssh.hrl").		% ?UINT32, ?BYTE, #ssh{} ...
-include_lib("ssh/src/ssh_transport.hrl").
-include_lib("ssh/src/ssh_auth.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(NEWLINE, <<"\r\n">>).
-define(REKEY_DATA_TMO, 65000).

-define(v(Key, Config), proplists:get_value(Key, Config)).
-define(v(Key, Config, Default), proplists:get_value(Key, Config, Default)).


%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() -> 
    [{group,tool_tests},
     {group,kex}
    ].

groups() ->
    [{tool_tests, [], [lib_works_as_client,
		       lib_works_as_server,
		       lib_match,
		       lib_no_match
		      ]},
     {kex, [], [no_common_alg_server_disconnects,
		no_common_alg_client_disconnects,
		gex_client_init_default_noexact,
		gex_client_init_default_exact,
		gex_client_init_option_groups,
		gex_client_init_option_groups_file
		]}
    ].


init_per_suite(Config) ->
    start_std_daemon( setup_dirs( start_apps(Config))).
    
end_per_suite(Config) ->
    stop_apps(Config).



init_per_testcase(no_common_alg_server_disconnects, Config) ->
    start_std_daemon(Config, [{preferred_algorithms,[{public_key,['ssh-rsa']}]}]);

init_per_testcase(TC, Config) when TC == gex_client_init_default_noexact ;
				   TC == gex_client_init_default_exact ;
				   TC == gex_client_init_option_groups ;
				   TC == gex_client_init_option_groups_file ->
    Opts = case TC of
	       gex_client_init_option_groups ->
		   [{dh_gex_groups, [{2345, 3, 41}]}];
	       gex_client_init_option_groups_file ->
		   DataDir = ?config(data_dir, Config),
		   F = filename:join(DataDir, "dh_group_test"),
		   [{dh_gex_groups, {file,F}}];
	       _ ->
		   []
	   end,
    start_std_daemon(Config,
		     [{preferred_algorithms, ssh:default_algorithms()}
		      | Opts]);
init_per_testcase(_TestCase, Config) ->
    check_std_daemon_works(Config, ?LINE).

end_per_testcase(no_common_alg_server_disconnects, Config) ->
    stop_std_daemon(Config);
end_per_testcase(TC, Config) when TC == gex_client_init_default_noexact ;
				  TC == gex_client_init_default_exact ;
				  TC == gex_client_init_option_groups ;
				  TC == gex_client_init_option_groups_file ->
    stop_std_daemon(Config);
end_per_testcase(_TestCase, Config) ->
    check_std_daemon_works(Config, ?LINE).

%%%--------------------------------------------------------------------
%%% Test Cases --------------------------------------------------------
%%%--------------------------------------------------------------------

%%%--------------------------------------------------------------------
%%% Connect to an erlang server and check that the testlib acts as a client.
lib_works_as_client(Config) ->
    %% Connect and negotiate keys
    {ok,InitialState} =
	ssh_trpt_test_lib:exec(
	  [{set_options, [print_ops, print_seqnums, print_messages]},
	   {connect,
	    server_host(Config),server_port(Config),
	    [{preferred_algorithms,[{kex,['diffie-hellman-group1-sha1']}]},
	     {silently_accept_hosts, true},
	     {user_dir, user_dir(Config)},
	     {user_interaction, false}]},
	   receive_hello,
	   {send, hello},
	   {send, ssh_msg_kexinit},
	   {match, #ssh_msg_kexinit{_='_'}, receive_msg},
	   {send, ssh_msg_kexdh_init},
	   {match,# ssh_msg_kexdh_reply{_='_'}, receive_msg},
	   {send, #ssh_msg_newkeys{}},
	   {match, #ssh_msg_newkeys{_='_'}, receive_msg}
	  ]
	 ),

    %% Do the authentcation
    {User,Pwd} = server_user_password(Config),
    {ok,EndState} =
	ssh_trpt_test_lib:exec(
	  [{send, #ssh_msg_service_request{name = "ssh-userauth"}},
	   {match, #ssh_msg_service_accept{name = "ssh-userauth"}, receive_msg},
	   {send, #ssh_msg_userauth_request{user = User,
					    service = "ssh-connection",
					    method = "password",
					    data = <<?BOOLEAN(?FALSE),
						     ?STRING(unicode:characters_to_binary(Pwd))>>
					   }},
	   {match, #ssh_msg_userauth_success{_='_'}, receive_msg}
	  ], InitialState),

    %% Disconnect
    {ok,_} =
	ssh_trpt_test_lib:exec(
	  [{send, #ssh_msg_disconnect{code = ?SSH_DISCONNECT_BY_APPLICATION,
				      description = "End of the fun",
				      language = ""
				     }},
	   close_socket
	  ], EndState).


%%--------------------------------------------------------------------
%%% Connect an erlang client and check that the testlib can act as a server.
lib_works_as_server(Config) ->
    {User,_Pwd} = server_user_password(Config),

    %% Create a listening socket as server socket:
    {ok,InitialState} = ssh_trpt_test_lib:exec(listen),
    HostPort = ssh_trpt_test_lib:server_host_port(InitialState),

    %% Start a process handling one connection on the server side:
    spawn_link(
      fun() ->
	      {ok,_} =
		  ssh_trpt_test_lib:exec(
		    [{set_options, [print_ops, print_messages]},
		     {accept, [{system_dir, system_dir(Config)},
			       {user_dir, user_dir(Config)}]},
		     receive_hello,
		     {send, hello},

		     {send, ssh_msg_kexinit},
		     {match, #ssh_msg_kexinit{_='_'}, receive_msg},

		     {match, #ssh_msg_kexdh_init{_='_'}, receive_msg},
		     {send, ssh_msg_kexdh_reply},

		     {send, #ssh_msg_newkeys{}},
		     {match,  #ssh_msg_newkeys{_='_'}, receive_msg},

		     {match, #ssh_msg_service_request{name="ssh-userauth"}, receive_msg},
		     {send, #ssh_msg_service_accept{name="ssh-userauth"}},

		     {match, #ssh_msg_userauth_request{service="ssh-connection",
						       method="none",
						       user=User,
						       _='_'}, receive_msg},

		     {send, #ssh_msg_userauth_failure{authentications = "password",
						      partial_success = false}},

		     {match, #ssh_msg_userauth_request{service="ssh-connection",
						       method="password",
						       user=User,
						       _='_'}, receive_msg},
		     {send, #ssh_msg_userauth_success{}},
		     close_socket,
		     print_state
		    ],
		    InitialState)
      end),

    %% and finally connect to it with a regular Erlang SSH client:
    {ok,_} = std_connect(HostPort, Config, 
			 [{preferred_algorithms,[{kex,['diffie-hellman-group1-sha1']}]}]
			).

%%--------------------------------------------------------------------
%%% Matching
lib_match(_Config) ->
    {ok,_} =
	ssh_trpt_test_lib:exec([{set_options, [print_ops]},
				{match, abc, abc},
				{match, '$a', {cde,fgh}},
				{match, {cde,fgh}, '$a'},
				{match, '_', {cde,fgh}},
				{match, [a,'$a',b], [a,{cde,fgh},b]},
				{match, [a,'$a'|'$b'], [a,{cde,fgh},b,c]},
				{match, '$b', [b,c]}
			       ]).

%%--------------------------------------------------------------------
%%% Not matching
lib_no_match(_Config) ->
    case ssh_trpt_test_lib:exec([{set_options, [print_ops]},
				 {match, '$x', b},
				 {match, a, '$x'}])
    of
	{ok,_} -> {fail,"Unexpected match"};
	{error, {_Op,{expected,a,b},_State}} -> ok
    end.

%%--------------------------------------------------------------------
%%% Algo negotiation fail.  This should result in a ssh_msg_disconnect
%%% being sent from the server.
no_common_alg_server_disconnects(Config) ->
    {ok,_} =
	ssh_trpt_test_lib:exec(
	  [{set_options, [print_ops, {print_messages,detail}]},
	   {connect,
	    server_host(Config),server_port(Config),
	    [{silently_accept_hosts, true},
	     {user_dir, user_dir(Config)},
	     {user_interaction, false},
	     {preferred_algorithms,[{public_key,['ssh-dss']}]}
	    ]},
	   receive_hello,
	   {send, hello},
	   {match, #ssh_msg_kexinit{_='_'}, receive_msg},
	   {send, ssh_msg_kexinit},  % with server unsupported 'ssh-dss' !
	   {match,
	    {'or',[#ssh_msg_disconnect{code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,  _='_'},
                  tcp_closed]},
	    receive_msg}
	  ]
	 ).

%%--------------------------------------------------------------------
%%% Algo negotiation fail.  This should result in a ssh_msg_disconnect
%%% being sent from the client.
no_common_alg_client_disconnects(Config) ->
    %% Create a listening socket as server socket:
    {ok,InitialState} = ssh_trpt_test_lib:exec(listen),
    HostPort = ssh_trpt_test_lib:server_host_port(InitialState),
    Parent = self(),

    %% Start a process handling one connection on the server side:
    Pid =
	spawn_link(
	  fun() ->
		  Parent !
		      {result,self(),
		       ssh_trpt_test_lib:exec(
			 [{set_options, [print_ops, {print_messages,detail}]},
			  {accept, [{system_dir, system_dir(Config)},
				    {user_dir, user_dir(Config)}]},
			  receive_hello,
			  {send, hello},
			  {match, #ssh_msg_kexinit{_='_'}, receive_msg},
			  {send,  #ssh_msg_kexinit{ % with unsupported "SOME-UNSUPPORTED"
				     cookie = 247381486335508958743193106082599558706,
				     kex_algorithms = ["diffie-hellman-group1-sha1"],
				     server_host_key_algorithms = ["SOME-UNSUPPORTED"],  % SIC!
				     encryption_algorithms_client_to_server = ["aes128-ctr"],
				     encryption_algorithms_server_to_client = ["aes128-ctr"],
				     mac_algorithms_client_to_server = ["hmac-sha2-256"],
				     mac_algorithms_server_to_client = ["hmac-sha2-256"],
				     compression_algorithms_client_to_server = ["none"],
				     compression_algorithms_server_to_client = ["none"],
				     languages_client_to_server = [],
				     languages_server_to_client = [],
				     first_kex_packet_follows = false,
				     reserved = 0
				    }},
                     	 {match,
                     	  {'or',[#ssh_msg_disconnect{code = ?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,  _='_'},
                                tcp_closed]},
                     	  receive_msg}
			 ],
			 InitialState)
		      }
	  end),

    %% and finally connect to it with a regular Erlang SSH client
    %% which of course does not support SOME-UNSUPPORTED as pub key algo:
    Result = std_connect(HostPort, Config, [{preferred_algorithms,[{public_key,['ssh-dss']}]}]),
    ct:log("Result of connect is ~p",[Result]),

    receive
	{result,Pid,{ok,_}} -> 
	    ok;
	{result,Pid,{error,{Op,ExecResult,S}}} ->
	    ct:log("ERROR!~nOp = ~p~nExecResult = ~p~nState =~n~s",
		   [Op,ExecResult,ssh_trpt_test_lib:format_msg(S)]),
	    {fail, ExecResult};
	X -> 
	    ct:log("¤¤¤¤¤"),
	    ct:fail(X)
    after 
	30000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.

%%%--------------------------------------------------------------------
gex_client_init_default_noexact(Config) ->
    do_gex_client_init(Config, {2000, 3000, 4000},
		       %% Warning, app knowledege:
		       ?dh_group15).


gex_client_init_default_exact(Config) ->
    do_gex_client_init(Config, {2000, 2048, 4000},
		       %% Warning, app knowledege:
		       ?dh_group14).


gex_client_init_option_groups(Config) ->
    do_gex_client_init(Config, {2000, 2048, 4000}, 
		       {'n/a',{3,41}}).


gex_client_init_option_groups_file(Config) ->
    do_gex_client_init(Config, {2000, 2048, 4000},
		       {'n/a',{5,61}}).

do_gex_client_init(Config, {Min,N,Max}, {_,{G,P}}) ->
    {ok,_} =
	ssh_trpt_test_lib:exec(
	  [{set_options, [print_ops, print_seqnums, print_messages]},
	   {connect,
	    server_host(Config),server_port(Config),
	    [{silently_accept_hosts, true},
	     {user_dir, user_dir(Config)},
	     {user_interaction, false},
	     {preferred_algorithms,[{kex,['diffie-hellman-group-exchange-sha1']}]}
	    ]},
	   receive_hello,
	   {send, hello},
	   {send, ssh_msg_kexinit},
	   {match, #ssh_msg_kexinit{_='_'}, receive_msg},
	   {send, #ssh_msg_kex_dh_gex_request{min = Min, 
					      n = N,
					      max = Max}},
	   {match, #ssh_msg_kex_dh_gex_group{p=P, g=G, _='_'},  receive_msg}
	  ]
	 ).

%%%================================================================
%%%==== Internal functions ========================================
%%%================================================================

%%%---- init_suite and end_suite ---------------------------------------	
start_apps(Config) ->
    catch crypto:stop(),
    case catch crypto:start() of
	ok ->
	    catch ssh:stop(),
	    ok = ssh:start(),
	    [{stop_apps, 
	      fun() ->
		      ssh:stop(),
		      crypto:stop()
	      end} | Config];
	_Else ->
	    {skip, "Crypto could not be started!"}
    end.
    

stop_apps(Config) ->
    (?v(stop_apps, Config, fun()-> ok end))(),
    ssh:stop().


setup_dirs(Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    ssh_test_lib:setup_rsa(DataDir, PrivDir),
    Config.

system_dir(Config) -> filename:join(?config(priv_dir, Config), system).

user_dir(Config) -> ?config(priv_dir, Config).

%%%----------------------------------------------------------------
start_std_daemon(Config) ->	
    start_std_daemon(Config, []).

start_std_daemon(Config, ExtraOpts) ->
    PrivDir = ?config(priv_dir, Config), 
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    UserPasswords = [{"user1","pwd1"}],
    Options = [%%{preferred_algorithms,[{public_key,['ssh-rsa']}]}, %% For some test cases
	       {system_dir, system_dir(Config)},
	       {user_dir, UserDir},
	       {user_passwords, UserPasswords},
	       {failfun, fun ssh_test_lib:failfun/2}
	       | ExtraOpts],
    Ref = {Server, Host, Port} = ssh_test_lib:daemon(Options),
    ct:log("Std server ~p started at ~p:~p~nOptions=~p",[Server, Host, Port, Options]),
    [{server,Ref}, {user_passwords, UserPasswords} | Config].


stop_std_daemon(Config) ->
    ssh:stop_daemon(server_pid(Config)),
    ct:log("Std server ~p at ~p:~p stopped", [server_pid(Config), server_host(Config), server_port(Config)]),
    lists:keydelete(server, 1, Config).


check_std_daemon_works(Config, Line) ->
    case std_connect(Config) of
	{ok,C} ->
	    ct:log("Server ~p:~p ~p is ok at line ~p",
		   [server_host(Config), server_port(Config), 
		    server_pid(Config), Line]),
	    ok = ssh:close(C),
	    Config;
	Error = {error,_} ->
	    ct:fail("Standard server ~p:~p ~p is ill at line ~p: ~p",
		    [server_host(Config), server_port(Config), 
		     server_pid(Config), Line, Error])
    end.

server_pid(Config)  -> element(1,?v(server,Config)).
server_host(Config) -> element(2,?v(server,Config)).
server_port(Config) -> element(3,?v(server,Config)).

server_user_password(Config) -> server_user_password(1, Config).

server_user_password(N, Config) -> lists:nth(N, ?v(user_passwords,Config)).
    

std_connect(Config) -> 
    std_connect({server_host(Config), server_port(Config)}, Config).
    
std_connect({Host,Port}, Config) ->
    std_connect({Host,Port}, Config, []).

std_connect({Host,Port}, Config, Opts) ->
    std_connect(Host, Port, Config, Opts).

std_connect(Host, Port, Config, Opts) ->
    {User,Pwd} = server_user_password(Config),
    ssh:connect(Host, Port, 
		%% Prefere User's Opts to the default opts
		[O || O = {Tag,_} <- [{user,User},{password,Pwd},
				      {silently_accept_hosts, true},
				      {user_dir, user_dir(Config)},
				      {user_interaction, false}],
		      not lists:keymember(Tag, 1, Opts)
		] ++ Opts,
		30000).
    
%%%----------------------------------------------------------------	
