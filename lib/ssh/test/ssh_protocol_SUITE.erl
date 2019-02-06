%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
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
-include("ssh_test_lib.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(NEWLINE, <<"\r\n">>).
-define(REKEY_DATA_TMO, 65000).

-define(DEFAULT_KEX, 'diffie-hellman-group14-sha256').
-define(EXTRA_KEX, 'diffie-hellman-group1-sha1').

-define(CIPHERS, ['aes256-ctr','aes192-ctr','aes128-ctr','aes128-cbc','3des-cbc']).
-define(DEFAULT_CIPHERS, [{client2server,?CIPHERS}, {server2client,?CIPHERS}]).

-define(v(Key, Config), proplists:get_value(Key, Config)).
-define(v(Key, Config, Default), proplists:get_value(Key, Config, Default)).


%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,40}}].

all() -> 
    [{group,tool_tests},
     client_info_line,
     {group,kex},
     {group,service_requests},
     {group,authentication},
     {group,packet_size_error},
     {group,field_size_error},
     {group,ext_info},
     {group,preferred_algorithms}
    ].

groups() ->
    [{tool_tests, [], [lib_works_as_client,
		       lib_works_as_server,
		       lib_match,
		       lib_no_match
		      ]},
     {packet_size_error, [], [packet_length_too_large,
			      packet_length_too_short]},
      
     {field_size_error, [], [service_name_length_too_large,
			     service_name_length_too_short]},
      
     {kex, [], [no_common_alg_server_disconnects,
		no_common_alg_client_disconnects,
		gex_client_init_option_groups,
		gex_server_gex_limit,
		gex_client_init_option_groups_moduli_file,
		gex_client_init_option_groups_file,
		gex_client_old_request_exact,
		gex_client_old_request_noexact
		]},
     {service_requests, [], [bad_service_name,
			     bad_long_service_name,
			     bad_very_long_service_name,
			     empty_service_name,
			     bad_service_name_then_correct
			    ]},
     {authentication, [], [client_handles_keyboard_interactive_0_pwds
			  ]},
     {ext_info, [], [no_ext_info_s1,
                     no_ext_info_s2,
                     ext_info_s,
                     ext_info_c
                    ]},
     {preferred_algorithms, [], [preferred_algorithms,
                                 modify_append,
                                 modify_prepend,
                                 modify_rm,
                                 modify_combo
                                ]}
    ].


init_per_suite(Config) ->
    ?CHECK_CRYPTO(start_std_daemon( setup_dirs( start_apps(Config)))).
    
end_per_suite(Config) ->
    stop_apps(Config).



init_per_testcase(no_common_alg_server_disconnects, Config) ->
    start_std_daemon(Config, [{preferred_algorithms,[{public_key,['ssh-rsa']},
                                                     {cipher,?DEFAULT_CIPHERS}
                                                    ]}]);

init_per_testcase(TC, Config) when TC == gex_client_init_option_groups ;
				   TC == gex_client_init_option_groups_moduli_file ;
				   TC == gex_client_init_option_groups_file ;
				   TC == gex_server_gex_limit ;
				   TC == gex_client_old_request_exact ;
				   TC == gex_client_old_request_noexact ->
    Opts = case TC of
	       gex_client_init_option_groups ->
		   [{dh_gex_groups, 
                     [{1023, 5, 
                       16#D9277DAA27DB131C03B108D41A76B4DA8ACEECCCAE73D2E48CEDAAA70B09EF9F04FB020DCF36C51B8E485B26FABE0337E24232BE4F4E693548310244937433FB1A5758195DC73B84ADEF8237472C46747D79DC0A2CF8A57CE8DBD8F466A20F8551E7B1B824B2E4987A8816D9BC0741C2798F3EBAD3ADEBCC78FCE6A770E2EC9F
                      }]}];
	       gex_client_init_option_groups_file ->
		   DataDir = proplists:get_value(data_dir, Config),
		   F = filename:join(DataDir, "dh_group_test"),
		   [{dh_gex_groups, {file,F}}];
	       gex_client_init_option_groups_moduli_file ->
		   DataDir = proplists:get_value(data_dir, Config),
		   F = filename:join(DataDir, "dh_group_test.moduli"),
		   [{dh_gex_groups, {ssh_moduli_file,F}}];
	       _ when TC == gex_server_gex_limit ;
		      TC == gex_client_old_request_exact ;
		      TC == gex_client_old_request_noexact ->
		    [{dh_gex_groups, 
                      [{1023, 2, 16#D9277DAA27DB131C03B108D41A76B4DA8ACEECCCAE73D2E48CEDAAA70B09EF9F04FB020DCF36C51B8E485B26FABE0337E24232BE4F4E693548310244937433FB1A5758195DC73B84ADEF8237472C46747D79DC0A2CF8A57CE8DBD8F466A20F8551E7B1B824B2E4987A8816D9BC0741C2798F3EBAD3ADEBCC78FCE6A771225323},
                       {1535, 5, 16#D1391174233D315398FE2830AC6B2B66BCCD01B0A634899F339B7879F1DB85712E9DC4E4B1C6C8355570C1D2DCB53493DF18175A9C53D1128B592B4C72D97136F5542FEB981CBFE8012FDD30361F288A42BD5EBB08BAB0A5640E1AC48763B2ABD1945FEE36B2D55E1D50A1C86CED9DD141C4E7BE2D32D9B562A0F8E2E927020E91F58B57EB9ACDDA106A59302D7E92AD5F6E851A45FA1CFE86029A0F727F65A8F475F33572E2FDAB6073F0C21B8B54C3823DB2EF068927E5D747498F96E1E827},
                       {3071, 2, 16#DFAA35D35531E0F524F0099877A482D2AC8D589F374394A262A8E81A8A4FB2F65FADBAB395E05D147B29D486DFAA41F41597A256DA82A8B6F76401AED53D0253F956CEC610D417E42E3B287F7938FC24D8821B40BFA218A956EB7401BED6C96C68C7FD64F8170A8A76B953DD2F05420118F6B144D8FE48060A2BCB85056B478EDEF96DBC70427053ECD2958C074169E9550DD877779A3CF17C5AC850598C7586BEEA9DCFE9DD2A5FB62DF5F33EA7BC00CDA31B9D2DD721F979EA85B6E63F0C4E30BDDCD3A335522F9004C4ED50B15DC537F55324DD4FA119FB3F101467C6D7E1699DE4B3E3C478A8679B8EB3FA5C9B826B44530FD3BE9AD3063B240B0C853EBDDBD68DD940332D98F148D5D9E1DC977D60A0D23D0CA1198637FEAE4E7FAAC173AF2B84313A666CFB4EE6972811921D0AD867CE57F3BBC8D6CB057E3B66757BB46C9F72662624D44E14528327E3A7100E81A12C43C4E236118318CD90C8AA185BBB0C764826DAEAEE8DD245C5B451B4944E6122CC522D1C335C2EEF9429825A2B}
                      ]},
                     {dh_gex_limits, {1023,2000}}
		    ];
	       _ ->
		   []
	   end,
    start_std_daemon(Config,
		     [{preferred_algorithms,[{cipher,?DEFAULT_CIPHERS}
                                            ]}
		      | Opts]);
init_per_testcase(_TestCase, Config) ->
    check_std_daemon_works(Config, ?LINE).

end_per_testcase(no_common_alg_server_disconnects, Config) ->
    stop_std_daemon(Config);
end_per_testcase(TC, Config) when TC == gex_client_init_option_groups ;
				  TC == gex_client_init_option_groups_moduli_file ;
				  TC == gex_client_init_option_groups_file ;
				  TC == gex_server_gex_limit ;
				  TC == gex_client_old_request_exact ;
				  TC == gex_client_old_request_noexact ->
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
    {ok,InitialState} = ssh_trpt_test_lib:exec(
			  [{set_options, [print_ops, print_seqnums, print_messages]}]
			 ),
    {ok,AfterKexState} = connect_and_kex(Config, InitialState),

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
	  ], AfterKexState),

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
			 [{preferred_algorithms,[{kex,[?DEFAULT_KEX]},
                                                 {cipher,?DEFAULT_CIPHERS}
                                                ]}
                         ]
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
	     {preferred_algorithms,[{public_key,['ssh-dss']},
                                    {cipher,?DEFAULT_CIPHERS}
                                   ]}
	    ]},
	   receive_hello,
	   {send, hello},
	   {match, #ssh_msg_kexinit{_='_'}, receive_msg},
	   {send, ssh_msg_kexinit},  % with server unsupported 'ssh-dss' !
	   {match, disconnect(), receive_msg}
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
				     cookie = <<80,158,95,51,174,35,73,130,246,141,200,49,180,190,82,234>>,
				     kex_algorithms = [atom_to_list(?DEFAULT_KEX)],
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
			  {match, disconnect(?SSH_DISCONNECT_KEY_EXCHANGE_FAILED), receive_msg}
			 ],
			 InitialState)
		      }
	  end),

    %% and finally connect to it with a regular Erlang SSH client
    %% which of course does not support SOME-UNSUPPORTED as pub key algo:
    Result = std_connect(HostPort, Config, [{preferred_algorithms,[{public_key,['ssh-dss']},
                                                                   {cipher,?DEFAULT_CIPHERS}
                                                                  ]}]),
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
gex_client_init_option_groups(Config) ->
    do_gex_client_init(Config, {512, 2048, 4000},
		       {5,16#D9277DAA27DB131C03B108D41A76B4DA8ACEECCCAE73D2E48CEDAAA70B09EF9F04FB020DCF36C51B8E485B26FABE0337E24232BE4F4E693548310244937433FB1A5758195DC73B84ADEF8237472C46747D79DC0A2CF8A57CE8DBD8F466A20F8551E7B1B824B2E4987A8816D9BC0741C2798F3EBAD3ADEBCC78FCE6A770E2EC9F}
                      ).

gex_client_init_option_groups_file(Config) ->
    do_gex_client_init(Config, {2000, 2048, 4000},
                       {5, 16#DFAA35D35531E0F524F0099877A482D2AC8D589F374394A262A8E81A8A4FB2F65FADBAB395E05D147B29D486DFAA41F41597A256DA82A8B6F76401AED53D0253F956CEC610D417E42E3B287F7938FC24D8821B40BFA218A956EB7401BED6C96C68C7FD64F8170A8A76B953DD2F05420118F6B144D8FE48060A2BCB85056B478EDEF96DBC70427053ECD2958C074169E9550DD877779A3CF17C5AC850598C7586BEEA9DCFE9DD2A5FB62DF5F33EA7BC00CDA31B9D2DD721F979EA85B6E63F0C4E30BDDCD3A335522F9004C4ED50B15DC537F55324DD4FA119FB3F101467C6D7E1699DE4B3E3C478A8679B8EB3FA5C9B826B44530FD3BE9AD3063B240B0C853EBDDBD68DD940332D98F148D5D9E1DC977D60A0D23D0CA1198637FEAE4E7FAAC173AF2B84313A666CFB4EE6972811921D0AD867CE57F3BBC8D6CB057E3B66757BB46C9F72662624D44E14528327E3A7100E81A12C43C4E236118318CD90C8AA185BBB0C764826DAEAEE8DD245C5B451B4944E6122CC522D1C335C2EEF9424273F1F}
                      ).

gex_client_init_option_groups_moduli_file(Config) ->
    do_gex_client_init(Config, {2000, 2048, 4000},
                       {5, 16#DD2047CBDBB6F8E919BC63DE885B34D0FD6E3DB2887D8B46FE249886ACED6B46DFCD5553168185FD376122171CD8927E60120FA8D01F01D03E58281FEA9A1ABE97631C828E41815F34FDCDF787419FE13A3137649AA93D2584230DF5F24B5C00C88B7D7DE4367693428C730376F218A53E853B0851BAB7C53C15DA7839CBE1285DB63F6FA45C1BB59FE1C5BB918F0F8459D7EF60ACFF5C0FA0F3FCAD1C5F4CE4416D4F4B36B05CDCEBE4FB879E95847EFBC6449CD190248843BC7EDB145FBFC4EDBB1A3C959298F08F3BA2CFBE231BBE204BE6F906209D28BD4820AB3E7BE96C26AE8A809ADD8D1A5A0B008E9570FA4C4697E116B8119892C604293683A9635F}
                       ).

gex_server_gex_limit(Config) ->
    do_gex_client_init(Config, {1000, 3000, 4000},
		       %% {7,91}).
                       {5, 16#D1391174233D315398FE2830AC6B2B66BCCD01B0A634899F339B7879F1DB85712E9DC4E4B1C6C8355570C1D2DCB53493DF18175A9C53D1128B592B4C72D97136F5542FEB981CBFE8012FDD30361F288A42BD5EBB08BAB0A5640E1AC48763B2ABD1945FEE36B2D55E1D50A1C86CED9DD141C4E7BE2D32D9B562A0F8E2E927020E91F58B57EB9ACDDA106A59302D7E92AD5F6E851A45FA1CFE86029A0F727F65A8F475F33572E2FDAB6073F0C21B8B54C3823DB2EF068927E5D747498F96E1E827}
                       ).


do_gex_client_init(Config, {Min,N,Max}, {G,P}) ->
    {ok,_} =
	ssh_trpt_test_lib:exec(
	  [{set_options, [print_ops, print_seqnums, print_messages]},
	   {connect,
	    server_host(Config),server_port(Config),
	    [{silently_accept_hosts, true},
	     {user_dir, user_dir(Config)},
	     {user_interaction, false},
	     {preferred_algorithms,[{kex,['diffie-hellman-group-exchange-sha1']},
                                    {cipher,?DEFAULT_CIPHERS}
                                   ]}
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

%%%--------------------------------------------------------------------
gex_client_old_request_exact(Config)  ->
    do_gex_client_init_old(Config, 1023,
                           {2, 16#D9277DAA27DB131C03B108D41A76B4DA8ACEECCCAE73D2E48CEDAAA70B09EF9F04FB020DCF36C51B8E485B26FABE0337E24232BE4F4E693548310244937433FB1A5758195DC73B84ADEF8237472C46747D79DC0A2CF8A57CE8DBD8F466A20F8551E7B1B824B2E4987A8816D9BC0741C2798F3EBAD3ADEBCC78FCE6A771225323}
                           ).

gex_client_old_request_noexact(Config) ->
    do_gex_client_init_old(Config, 1400,
                           {5, 16#D1391174233D315398FE2830AC6B2B66BCCD01B0A634899F339B7879F1DB85712E9DC4E4B1C6C8355570C1D2DCB53493DF18175A9C53D1128B592B4C72D97136F5542FEB981CBFE8012FDD30361F288A42BD5EBB08BAB0A5640E1AC48763B2ABD1945FEE36B2D55E1D50A1C86CED9DD141C4E7BE2D32D9B562A0F8E2E927020E91F58B57EB9ACDDA106A59302D7E92AD5F6E851A45FA1CFE86029A0F727F65A8F475F33572E2FDAB6073F0C21B8B54C3823DB2EF068927E5D747498F96E1E827}
                           ).
    
do_gex_client_init_old(Config, N, {G,P}) ->
    {ok,_} =
	ssh_trpt_test_lib:exec(
	  [{set_options, [print_ops, print_seqnums, print_messages]},
	   {connect,
	    server_host(Config),server_port(Config),
	    [{silently_accept_hosts, true},
	     {user_dir, user_dir(Config)},
	     {user_interaction, false},
	     {preferred_algorithms,[{kex,['diffie-hellman-group-exchange-sha1']},
                                    {cipher,?DEFAULT_CIPHERS}
                                   ]}
	    ]},
	   receive_hello,
	   {send, hello},
	   {send, ssh_msg_kexinit},
	   {match, #ssh_msg_kexinit{_='_'}, receive_msg},
	   {send, #ssh_msg_kex_dh_gex_request_old{n = N}},
	   {match, #ssh_msg_kex_dh_gex_group{p=P, g=G, _='_'},  receive_msg}
	  ]
	 ).

%%%--------------------------------------------------------------------
bad_service_name(Config) -> 
    bad_service_name(Config, "kfglkjf").
    
bad_long_service_name(Config) -> 
    bad_service_name(Config, 
		     lists:duplicate(?SSH_MAX_PACKET_SIZE div 2, $a)).

bad_very_long_service_name(Config) -> 
    bad_service_name(Config,
		     lists:duplicate(?SSH_MAX_PACKET_SIZE+5, $a)).

empty_service_name(Config) ->
    bad_service_name(Config, "").
    
bad_service_name_then_correct(Config) ->
    {ok,InitialState} = connect_and_kex(Config),
    {ok,_} =
	ssh_trpt_test_lib:exec(
	  [{set_options, [print_ops, print_seqnums, print_messages]},
	   {send, #ssh_msg_service_request{name = "kdjglkfdjgkldfjglkdfjglkfdjglkj"}},
	   {send, #ssh_msg_service_request{name = "ssh-connection"}},
	   {match, disconnect(), receive_msg}
	   ], InitialState).


bad_service_name(Config, Name) ->
    {ok,InitialState} = connect_and_kex(Config),
    {ok,_} =
	ssh_trpt_test_lib:exec(
	  [{set_options, [print_ops, print_seqnums, print_messages]},
	   {send, #ssh_msg_service_request{name = Name}},
	   {match, disconnect(), receive_msg}
	  ], InitialState).

%%%--------------------------------------------------------------------
packet_length_too_large(Config) -> bad_packet_length(Config, +4).

packet_length_too_short(Config) -> bad_packet_length(Config, -4).
    
bad_packet_length(Config, LengthExcess) ->
    PacketFun = 
	fun(Msg, Ssh) ->
		BinMsg = ssh_message:encode(Msg),
		ssh_transport:pack(BinMsg, Ssh, LengthExcess)
	end,
    {ok,InitialState} = connect_and_kex(Config),
    {ok,_} =
	ssh_trpt_test_lib:exec(
	  [{set_options, [print_ops, print_seqnums, print_messages]},
	   {send, {special,
		   #ssh_msg_service_request{name="ssh-userauth"},
		   PacketFun}},
	   %% Prohibit remote decoder starvation:	   
	   {send, #ssh_msg_service_request{name="ssh-userauth"}},
	   {match, disconnect(), receive_msg}
	  ], InitialState).

%%%--------------------------------------------------------------------
service_name_length_too_large(Config) -> bad_service_name_length(Config, +4).

service_name_length_too_short(Config) -> bad_service_name_length(Config, -4).


bad_service_name_length(Config, LengthExcess) ->
    PacketFun = 
	fun(#ssh_msg_service_request{name=Service}, Ssh) ->
		BinName = list_to_binary(Service),
		BinMsg = 
		    <<?BYTE(?SSH_MSG_SERVICE_REQUEST),
		      %% A bad string encoding of Service:
		      ?UINT32(size(BinName)+LengthExcess), BinName/binary
		    >>,
		ssh_transport:pack(BinMsg, Ssh)
	end,
    {ok,InitialState} = connect_and_kex(Config),
    {ok,_} =
	ssh_trpt_test_lib:exec(
	  [{set_options, [print_ops, print_seqnums, print_messages]},
	   {send, {special,
		   #ssh_msg_service_request{name="ssh-userauth"}, 
		   PacketFun} },
	   %% Prohibit remote decoder starvation:	   
	   {send, #ssh_msg_service_request{name="ssh-userauth"}},
	   {match, disconnect(), receive_msg}
	  ], InitialState).
    
%%%--------------------------------------------------------------------
%%% This is due to a fault report (OTP-13255) with OpenSSH-6.6.1
client_handles_keyboard_interactive_0_pwds(Config) ->
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
		     {send, #ssh_msg_userauth_failure{authentications = "keyboard-interactive",
						      partial_success = false}},
		     
		     {match, #ssh_msg_userauth_request{service="ssh-connection",
						       method="keyboard-interactive",
						       user=User,
						       _='_'}, receive_msg},
		     {send, #ssh_msg_userauth_info_request{name = "",
							   instruction = "",
							   language_tag = "",
							   num_prompts = 1,
							   data = <<0,0,0,10,80,97,115,115,119,111,114,100,58,32,0>>
							  }},
		     {match, #ssh_msg_userauth_info_response{num_responses = 1,
							     _='_'}, receive_msg},
		      
		     %% the next is strange, but openssh 6.6.1 does this and this is what this testcase is about
		     {send, #ssh_msg_userauth_info_request{name = "",
							   instruction = "",
							   language_tag = "",
							   num_prompts = 0,
							   data = <<>>
							  }},
		     {match, #ssh_msg_userauth_info_response{num_responses = 0,
							     data = <<>>,
							     _='_'}, receive_msg},
		     %% Here we know that the tested fault is fixed
		     {send, #ssh_msg_userauth_success{}},
		     close_socket,
		     print_state
		    ],
		    InitialState)
      end),

    %% and finally connect to it with a regular Erlang SSH client:
    {ok,_} = std_connect(HostPort, Config, 
			 [{preferred_algorithms,[{kex,[?DEFAULT_KEX]},
                                                 {cipher,?DEFAULT_CIPHERS}
                                                ]}]
			).



%%%--------------------------------------------------------------------
client_info_line(Config) ->
    %% A client must not send an info-line. If it does, the server should handle
    %% handle this gracefully
    {ok,Pid} = ssh_eqc_event_handler:add_report_handler(),
    DataDir = proplists:get_value(data_dir, Config),
    {_, _, Port} = ssh_test_lib:daemon([{system_dir,DataDir}]),

    %% Fake client:
    {ok,S} = gen_tcp:connect("localhost",Port,[]),
    gen_tcp:send(S,"An illegal info-string\r\n"),
    gen_tcp:close(S),

    %% wait for server to react:
    timer:sleep(1000),

    %% check if a badmatch was received:
    {ok, Reports} = ssh_eqc_event_handler:get_reports(Pid),
    case lists:any(fun({error_report,_,{_,supervisor_report,L}}) when is_list(L) -> 
			   lists:member({reason,{badmatch,{error,closed}}}, L);
		      (_) ->
			   false
		   end, Reports) of
	true ->
	    ct:fail("Bad error report on info_line from client");
	false ->
	    ok
    end.
	
%%%--------------------------------------------------------------------
%%% The server does not send the extension because
%%% the client does not tell the server to send it
no_ext_info_s1(Config) ->
    %% Start the dameon
    Server = {Pid,_,_} = ssh_test_lib:daemon([{send_ext_info,true},
                                              {system_dir, system_dir(Config)}]),
    {ok,AfterKexState} = connect_and_kex([{server,Server}|Config]),
    {ok,_} = 
        ssh_trpt_test_lib:exec(
          [{send, #ssh_msg_service_request{name = "ssh-userauth"}},
	   {match, #ssh_msg_service_accept{name = "ssh-userauth"}, receive_msg}
          ], AfterKexState),
    ssh:stop_daemon(Pid).

%%%--------------------------------------------------------------------
%%% The server does not send the extension because
%%% the server is not configured to send it
no_ext_info_s2(Config) ->    
    %% Start the dameon
    Server = {Pid,_,_} = ssh_test_lib:daemon([{send_ext_info,false},
                                              {system_dir, system_dir(Config)}]),
    {ok,AfterKexState} = connect_and_kex([{extra_options,[{recv_ext_info,true}]},
                                          {server,Server}
                                          | Config]),
    {ok,_} =
        ssh_trpt_test_lib:exec(
          [{send, #ssh_msg_service_request{name = "ssh-userauth"}},
	   {match, #ssh_msg_service_accept{name = "ssh-userauth"}, receive_msg}
          ], AfterKexState),
    ssh:stop_daemon(Pid).

%%%--------------------------------------------------------------------
%%% The server sends the extension
ext_info_s(Config) ->    
    %% Start the dameon
    Server = {Pid,_,_} = ssh_test_lib:daemon([{send_ext_info,true},
                                              {system_dir, system_dir(Config)}]),
    {ok,AfterKexState} = connect_and_kex([{extra_options,[{recv_ext_info,true}]},
                                          {server,Server}
                                          | Config]),
    {ok,_} =
        ssh_trpt_test_lib:exec(
          [{match, #ssh_msg_ext_info{_='_'}, receive_msg}
          ],
          AfterKexState),
    ssh:stop_daemon(Pid).

%%%--------------------------------------------------------------------
%%% The client sends the extension
ext_info_c(Config) ->    
    %% Create a listening socket as server socket:
    {ok,InitialState} = ssh_trpt_test_lib:exec(listen),
    HostPort = ssh_trpt_test_lib:server_host_port(InitialState),

    Parent = self(),
    %% Start a process handling one connection on the server side:
    Pid =
        spawn_link(
          fun() ->
                  Result =
                      ssh_trpt_test_lib:exec(
                        [{set_options, [print_ops, print_messages]},
                         {accept, [{system_dir, system_dir(Config)},
                                   {user_dir, user_dir(Config)},
                                   {recv_ext_info, true}
                                  ]},
                         receive_hello,
                         {send, hello},
                         
                         {send, ssh_msg_kexinit},
                         {match, #ssh_msg_kexinit{_='_'}, receive_msg},
                         
                         {match, #ssh_msg_kexdh_init{_='_'}, receive_msg},
                         {send, ssh_msg_kexdh_reply},

                         {send, #ssh_msg_newkeys{}},
                         {match,  #ssh_msg_newkeys{_='_'}, receive_msg},

                         {match, #ssh_msg_ext_info{_='_'}, receive_msg},

                         close_socket,
                         print_state
                        ],
                        InitialState),
                  Parent ! {result,self(),Result}
          end),

    %% connect to it with a regular Erlang SSH client
    %% (expect error due to the close_socket in daemon):
    {error,_} = std_connect(HostPort, Config, 
                            [{preferred_algorithms,[{kex,[?DEFAULT_KEX]},
                                                    {cipher,?DEFAULT_CIPHERS}
                                                   ]},
                             {tstflg, [{ext_info_client,true}]},
                             {send_ext_info, true}
                            ]
                           ),
    
    %% Check that the daemon got expected result:
    receive
        {result, Pid, {ok,_}} -> ok;
        {result, Pid, Error} -> ct:fail("Error: ~p",[Error])
    end.


%%%----------------------------------------------------------------
%%%
preferred_algorithms(Config) ->
    Ciphers = filter_supported(cipher, ?CIPHERS),
    {error,{eoptions,{{preferred_algorithms,{kex,[some_unknown_algo]}},
                      "Unsupported value(s) found"}}} =
        chk_pref_algs(Config,
                      [?DEFAULT_KEX],
                      Ciphers,
                      [{preferred_algorithms, [{kex,[some_unknown_algo,?DEFAULT_KEX]},
                                               {cipher,Ciphers}
                                              ]}
                      ]).

%%%----------------------------------------------------------------
%%%
modify_append(Config) ->
    Ciphers = filter_supported(cipher, ?CIPHERS),
    {ok,_} =
        chk_pref_algs(Config,
                      [?DEFAULT_KEX, ?EXTRA_KEX],
                      Ciphers,
                      [{preferred_algorithms, [{kex,[?DEFAULT_KEX]},
                                               {cipher,Ciphers}
                                              ]},
                       {modify_algorithms, [{append,[{kex,[some_unknown_algo,?EXTRA_KEX]}]}]}
                      ]).

%%%----------------------------------------------------------------
%%%
modify_prepend(Config) ->
    Ciphers = filter_supported(cipher, ?CIPHERS),
    {ok,_} =
        chk_pref_algs(Config,
                      [?EXTRA_KEX, ?DEFAULT_KEX],
                      Ciphers,
                      [{preferred_algorithms, [{kex,[?DEFAULT_KEX]},
                                               {cipher,Ciphers}
                                              ]},
                       {modify_algorithms, [{prepend,[{kex,[some_unknown_algo,?EXTRA_KEX]}]}]}
                      ]).

%%%----------------------------------------------------------------
%%%
modify_rm(Config) ->
    Ciphers = filter_supported(cipher, ?CIPHERS),
    {ok,_} =
        chk_pref_algs(Config,
                      [?DEFAULT_KEX],
                      tl(Ciphers),
                      [{preferred_algorithms, [{kex,[?DEFAULT_KEX,?EXTRA_KEX]},
                                               {cipher,Ciphers}
                                              ]},
                       {modify_algorithms, [{rm,[{kex,[some_unknown_algo,?EXTRA_KEX]},
                                                 {cipher,[hd(Ciphers)]}
                                                ]}
                                           ]}
                      ]).


%%%----------------------------------------------------------------
%%%
modify_combo(Config) ->
    Ciphers = filter_supported(cipher, ?CIPHERS),
    LastC = lists:last(Ciphers),
    {ok,_} =
        chk_pref_algs(Config,
                      [?DEFAULT_KEX],
                      [LastC] ++ (tl(Ciphers)--[LastC]) ++ [hd(Ciphers)],
                      [{preferred_algorithms, [{kex,[?DEFAULT_KEX,?EXTRA_KEX]},
                                               {cipher,Ciphers}
                                              ]},
                       {modify_algorithms, [{rm,[{kex,[some_unknown_algo,?EXTRA_KEX]}
                                                ]},
                                            {prepend,[{cipher,[{server2client,[LastC]}]}
                                                     ]},
                                            {append,[{cipher,[a,hd(Ciphers),b]}
                                                    ]}
                                           ]}
                      ]).

%%%================================================================
%%%==== Internal functions ========================================
%%%================================================================

chk_pref_algs(Config,
              ExpectedKex,
              ExpectedCiphers,
              ServerPrefOpts) ->
    %% Start the dameon
    case ssh_test_lib:daemon(
                      [{send_ext_info,false},
                       {recv_ext_info,false},
                       {system_dir, system_dir(Config)}
                       | ServerPrefOpts])
    of
        {_,Host,Port} ->
            %% Check the Kex part
            ssh_trpt_test_lib:exec(
              [{set_options, [print_ops, {print_messages,detail}]},
               {connect, Host, Port,
                [{silently_accept_hosts, true},
                 {user_dir, user_dir(Config)},
                 {user_interaction, false}
                ]},
               {send, hello},
               receive_hello,
               {match,
                #ssh_msg_kexinit{
                   kex_algorithms = to_lists(ExpectedKex),
                   encryption_algorithms_server_to_client = to_lists(ExpectedCiphers),
                   _   = '_'},
                receive_msg}
              ]);
        Error ->
            Error
    end.


filter_supported(K, Algs) -> Algs -- (Algs--supported(K)).

supported(_K) -> proplists:get_value(
                   server2client,
                   ssh_transport:supported_algorithms(cipher)).

to_lists(L) -> lists:map(fun erlang:atom_to_list/1, L).
    

%%%---- init_suite and end_suite ---------------------------------------	
start_apps(Config) ->
    catch ssh:stop(),
    ok = ssh:start(),
    Config.

stop_apps(_Config) ->
    ssh:stop().


setup_dirs(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    ssh_test_lib:setup_dsa(DataDir, PrivDir),
    ssh_test_lib:setup_rsa(DataDir, PrivDir),
    Config.

system_dir(Config) -> filename:join(proplists:get_value(priv_dir, Config), system).

user_dir(Config) -> proplists:get_value(priv_dir, Config).

%%%----------------------------------------------------------------
start_std_daemon(Config) ->	
    start_std_daemon(Config, []).

start_std_daemon(Config, ExtraOpts) ->
    PrivDir = proplists:get_value(priv_dir, Config), 
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
connect_and_kex(Config) ->
    connect_and_kex(Config, ssh_trpt_test_lib:exec([]) ).

connect_and_kex(Config, InitialState) ->
    ssh_trpt_test_lib:exec(
      [{connect,
	server_host(Config),server_port(Config),
	[{preferred_algorithms,[{kex,[?DEFAULT_KEX]},
                                {cipher,?DEFAULT_CIPHERS}
                               ]},
         {silently_accept_hosts, true},
         {recv_ext_info, false},
	 {user_dir, user_dir(Config)},
	 {user_interaction, false}
         | proplists:get_value(extra_options,Config,[])
        ]},
       receive_hello,
       {send, hello},
       {send, ssh_msg_kexinit},
       {match, #ssh_msg_kexinit{_='_'}, receive_msg},
       {send, ssh_msg_kexdh_init},
       {match,# ssh_msg_kexdh_reply{_='_'}, receive_msg},
       {send, #ssh_msg_newkeys{}},
       {match, #ssh_msg_newkeys{_='_'}, receive_msg}
      ],
      InitialState).

%%%----------------------------------------------------------------

%%% For matching peer disconnection
disconnect() ->
    disconnect('_').

disconnect(Code) ->
    {'or',[#ssh_msg_disconnect{code = Code,
			       _='_'},
	   tcp_closed,
	   {tcp_error,econnaborted}
	  ]}.
