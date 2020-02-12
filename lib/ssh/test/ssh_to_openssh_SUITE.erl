%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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
-module(ssh_to_openssh_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("ssh_test_lib.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(SSH_DEFAULT_PORT, 22).
-define(REKEY_DATA_TMO, 65000).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [{timetrap,{seconds,60}}].

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
    [{erlang_client, [], [tunnel_in_erlclient_erlserver,
                          tunnel_out_erlclient_erlserver,
                          {group, tunnel_distro_server},
                          erlang_shell_client_openssh_server
			 ]},
     {tunnel_distro_server, [], [tunnel_in_erlclient_openssh_server,
                                 tunnel_out_erlclient_openssh_server]},
     {erlang_server, [], [{group, tunnel_distro_client},
                          erlang_server_openssh_client_renegotiate,
                          exec_with_io_in_sshc,
                          exec_direct_with_io_in_sshc
			 ]},
     {tunnel_distro_client, [], [tunnel_in_non_erlclient_erlserver,
                                 tunnel_out_non_erlclient_erlserver]}
    ].

init_per_suite(Config) ->
    ?CHECK_CRYPTO(
       case gen_tcp:connect("localhost", 22, []) of
	   {error,econnrefused} ->
	       {skip,"No openssh deamon (econnrefused)"};
	   _ ->
               ssh_test_lib:openssh_sanity_check(
                 [{ptty_supported, ssh_test_lib:ptty_supported()}
                  | Config]
                )
       end
      ).

end_per_suite(_Config) ->
    ok.

init_per_group(erlang_server, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    UserDir = proplists:get_value(priv_dir, Config),
    ssh_test_lib:setup_dsa_known_host(DataDir, UserDir),
    ssh_test_lib:setup_rsa_known_host(DataDir, UserDir),
    Config;
init_per_group(G, Config) when G==tunnel_distro_server ;
                               G==tunnel_distro_client ->
    case no_forwarding() of
        true ->
            {skip, "port forwarding disabled in external ssh"};
        false ->
            Config
    end;
init_per_group(erlang_client, Config) ->
    CommonAlgs = ssh_test_lib:algo_intersection(
		   ssh:default_algorithms(),
		   ssh_test_lib:default_algorithms(sshd)),
    [{common_algs,CommonAlgs} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(erlang_server, Config) ->
    UserDir = proplists:get_value(priv_dir, Config),
    ssh_test_lib:clean_dsa(UserDir),
    ssh_test_lib:clean_rsa(UserDir),
    Config;
end_per_group(_, Config) ->
    Config.


init_per_testcase(erlang_server_openssh_client_renegotiate, Config) ->
    case os:type() of
	{unix,_} -> ssh:start(), Config;
	Type -> {skip, io_lib:format("Unsupported test on ~p",[Type])}
    end;
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
    Prev = lists:usort(supervisor:which_children(sshc_sup)),
    Shell = ssh_test_lib:start_shell(?SSH_DEFAULT_PORT, IO),
    IO ! {input, self(), "echo Hej\n"},
    case proplists:get_value(ptty_supported, Config) of
        true ->
            ct:log("~p:~p  ptty supported", [?MODULE,?LINE]),
            receive_data("Hej", undefined),
            IO ! {input, self(), "exit\n"},
            receive_logout(),
            receive_normal_exit(Shell),
            %% Check that the connection is closed:
            ct:log("Expects ~p", [Prev]),
            ?wait_match(Prev, lists:usort(supervisor:which_children(sshc_sup)));
        false ->
            ct:log("~p:~p  ptty unsupported", [?MODULE,?LINE]),
            receive_exit(Shell,
                         fun({{badmatch,failure},
                              [{ssh,shell,_,_} | _]}) -> true;
                            (_) ->
                                 false
                         end)
    end.

%%--------------------------------------------------------------------
%% Test that the server could redirect stdin and stdout from/to an
%% OpensSSH client when handling an exec request
exec_with_io_in_sshc(Config) when is_list(Config) ->
    SystemDir = proplists:get_value(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
                                             {failfun, fun ssh_test_lib:failfun/2}]),
    ct:sleep(500),

    ExecStr = "\"io:read('% ').\"",
    Cmd =  "echo howdy. | " ++ ssh_test_lib:open_sshc_cmd(Host, Port,
                                                          "-x", % Disable X forwarding
                                                          ExecStr),
    ct:pal("Cmd = ~p~n",[Cmd]),
    case os:cmd(Cmd) of
        "% {ok,howdy}" -> ok;
        "{ok,howdy}% " -> ok; % Could happen if the client sends the piped
                              % input before receiving the prompt ("% ").
        Other -> ct:fail("Received ~p",[Other])
    end,
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
%% Test that the server could redirect stdin and stdout from/to an
%% OpensSSH client when handling an direct exec request
exec_direct_with_io_in_sshc(Config) when is_list(Config) ->
    SystemDir = proplists:get_value(data_dir, Config),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
                                             {failfun, fun ssh_test_lib:failfun/2},
                                             {exec,{direct,fun(Cmnd) ->
                                                                   {ok,X} = io:read(Cmnd),
                                                                   {ok,{X,lists:reverse(atom_to_list(X))}}
                                                           end}}
                                            ]),
    ct:sleep(500),

    Cmd =  "echo ciao. | " ++ ssh_test_lib:open_sshc_cmd(Host, Port,
                                                         "-x", % Disable X forwarding
                                                         "'? '"),
    ct:pal("Cmd = ~p~n",[Cmd]),
    case os:cmd(Cmd) of
        "? {ciao,\"oaic\"}" -> ok;
        "'? '{ciao,\"oaic\"}" -> ok; % WSL
        "{ciao,\"oaic\"}? " -> ok; % Could happen if the client sends the piped
                                   % input before receiving the prompt ("? ").
        Other -> ct:fail("Received ~p",[Other])
    end,
    ssh:stop_daemon(Pid).

%%--------------------------------------------------------------------
%% Test that the Erlang/OTP server can renegotiate with openSSH
erlang_server_openssh_client_renegotiate(Config) ->
    _PubKeyAlg = ssh_rsa,
    SystemDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    KnownHosts = filename:join(PrivDir, "known_hosts"),

    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
                                             {failfun, fun ssh_test_lib:failfun/2}]),
    ct:sleep(500),

    RenegLimitK = 3,
    DataFile = filename:join(PrivDir, "renegotiate_openssh_client.data"),
    Data =  lists:duplicate(trunc(1.1*RenegLimitK*1024), $a),
    ok = file:write_file(DataFile, Data),

    Cmd = ssh_test_lib:open_sshc_cmd(Host, Port,
                                     [" -o UserKnownHostsFile=", KnownHosts,
                                      " -o StrictHostKeyChecking=no",
                                      " -o RekeyLimit=",integer_to_list(RenegLimitK),"K"]),


    OpenSsh = ssh_test_lib:open_port({spawn, Cmd++" < "++DataFile}),

    Expect = fun({data,R}) -> 
		     try
			 NonAlphaChars = [C || C<-lists:seq(1,255), 
					       not lists:member(C,lists:seq($a,$z)),
					       not lists:member(C,lists:seq($A,$Z))
					 ],
			 Lines = string:tokens(binary_to_list(R), NonAlphaChars),
			 lists:any(fun(L) -> length(L)>1 andalso lists:prefix(L, Data) end,
				   Lines)
		     catch
			 _:_ -> false
		     end;

		({exit_status,E}) when E=/=0 ->
		     ct:log("exit_status ~p",[E]),
		     throw({skip,"exit status"});

		(_) ->
		     false
	     end,
    
    try 
	ssh_test_lib:rcv_expected(Expect, OpenSsh, ?TIMEOUT)
    of
	_ ->
	    %% Unfortunately we can't check that there has been a renegotiation, just trust OpenSSH.
	    ssh:stop_daemon(Pid)
    catch
	throw:{skip,R} -> {skip,R}
    end.

%%--------------------------------------------------------------------
tunnel_out_non_erlclient_erlserver(Config) ->
    SystemDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    KnownHosts = filename:join(PrivDir, "known_hosts"),

    {_Pid, Host, Port} = ssh_test_lib:daemon([{tcpip_tunnel_out, true},
                                             {system_dir, SystemDir},
                                             {failfun, fun ssh_test_lib:failfun/2}]),
    {ToSock, _ToHost, ToPort} = tunneling_listner(),

    ListenHost = {127,0,0,1},
    ListenPort = 2345,

    Cmd = ssh_test_lib:open_sshc_cmd(Host, Port,
                                     [" -o UserKnownHostsFile=", KnownHosts,
                                      " -o StrictHostKeyChecking=no",
                                      " -R ",integer_to_list(ListenPort),":127.0.0.1:",integer_to_list(ToPort)]),
    spawn(fun() ->
                  ct:log(["ssh command:\r\n  ",Cmd],[]),
                  R = os:cmd(Cmd),
                  ct:log(["ssh returned:\r\n",R],[])
          end),

    ct:sleep(1000),
    test_tunneling(ToSock, ListenHost, ListenPort).
    
%%--------------------------------------------------------------------
tunnel_in_non_erlclient_erlserver(Config) ->
    SystemDir = proplists:get_value(data_dir, Config),
    UserDir = proplists:get_value(priv_dir, Config),
    KnownHosts = filename:join(UserDir, "known_hosts"),
    {_Pid, Host, Port} = ssh_test_lib:daemon([{tcpip_tunnel_in, true},
                                              {system_dir, SystemDir},
                                              {failfun, fun ssh_test_lib:failfun/2}]),
    {ToSock, _ToHost, ToPort} = tunneling_listner(),
    
    ListenHost = {127,0,0,1},
    ListenPort = 2345,

    Cmd =
        ssh_test_lib:open_sshc_cmd(Host, Port,
                                   [" -o UserKnownHostsFile=", KnownHosts,
                                    " -o StrictHostKeyChecking=no",
                                    " -L ",integer_to_list(ListenPort),":127.0.0.1:",integer_to_list(ToPort)]),
    spawn(fun() ->
                  ct:log(["ssh command:\r\n  ",Cmd],[]),
                  R = os:cmd(Cmd),
                  ct:log(["ssh returned:\r\n",R],[])
          end),
    ct:sleep(1000),
    test_tunneling(ToSock, ListenHost, ListenPort).

%%--------------------------------------------------------------------
tunnel_in_erlclient_erlserver(Config) ->
    SystemDir = proplists:get_value(data_dir, Config),
    UserDir = proplists:get_value(priv_dir, Config),
    {_Pid, Host, Port} = ssh_test_lib:daemon([{tcpip_tunnel_in, true},
                                              {system_dir, SystemDir},
                                              {user_dir, UserDir},
                                              {user_passwords, [{"foo", "bar"}]},
                                              {failfun, fun ssh_test_lib:failfun/2}]),
    C = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
                                          {user_dir, UserDir},
                                          {user,"foo"},{password,"bar"},
                                          {user_interaction, false}]),
    {ToSock, ToHost, ToPort} = tunneling_listner(),
    
    ListenHost = {127,0,0,1},
    {ok,ListenPort} = ssh:tcpip_tunnel_to_server(C, ListenHost,0, ToHost,ToPort, 2000),

    test_tunneling(ToSock, ListenHost, ListenPort).

%%--------------------------------------------------------------------
tunnel_in_erlclient_openssh_server(_Config) ->
    C = ssh_test_lib:connect(loopback, 22, [{silently_accept_hosts, true},
                                            {user_interaction, false}]),
    {ToSock, ToHost, ToPort} = tunneling_listner(),
    
    ListenHost = {127,0,0,1},
    {ok,ListenPort} = ssh:tcpip_tunnel_to_server(C, ListenHost,0, ToHost,ToPort, 5000),

    test_tunneling(ToSock, ListenHost, ListenPort).

%%--------------------------------------------------------------------
tunnel_out_erlclient_erlserver(Config) ->
    SystemDir = proplists:get_value(data_dir, Config),
    UserDir = proplists:get_value(priv_dir, Config),
    {_Pid, Host, Port} = ssh_test_lib:daemon([{tcpip_tunnel_out, true},
                                              {system_dir, SystemDir},
                                              {user_dir, UserDir},
                                              {user_passwords, [{"foo", "bar"}]},
                                              {failfun, fun ssh_test_lib:failfun/2}]),
    C = ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
                                          {user_dir, UserDir},
                                          {user,"foo"},{password,"bar"},
                                          {user_interaction, false}]),
    {ToSock, ToHost, ToPort} = tunneling_listner(),
    
    ListenHost = {127,0,0,1},
    {ok,ListenPort} = ssh:tcpip_tunnel_from_server(C, ListenHost,0, ToHost,ToPort, 5000),

    test_tunneling(ToSock, ListenHost, ListenPort).

%%--------------------------------------------------------------------
tunnel_out_erlclient_openssh_server(_Config) ->
    C = ssh_test_lib:connect(loopback, 22, [{silently_accept_hosts, true},
                                            {user_interaction, false}]),
    {ToSock, ToHost, ToPort} = tunneling_listner(),
    
    ListenHost = {127,0,0,1},
    {ok,ListenPort} = ssh:tcpip_tunnel_from_server(C, ListenHost,0, ToHost,ToPort, 5000),

    test_tunneling(ToSock, ListenHost, ListenPort).

%%--------------------------------------------------------------------
%%% Internal functions -----------------------------------------------
%%--------------------------------------------------------------------
tunneling_listner() ->
    {ok,LSock} = gen_tcp:listen(0, [{active,false}]),
    {ok, {LHost,LPort}} = inet:sockname(LSock),
    {LSock, LHost, LPort}.

test_tunneling(ListenSocket, Host, Port) ->
    {ok,Client1} = gen_tcp:connect(Host, Port, [{active,false}]),
    {ok,Server1} = gen_tcp:accept(ListenSocket),
    {ok,Client2} = gen_tcp:connect(Host, Port, [{active,false}]),
    {ok,Server2} = gen_tcp:accept(ListenSocket),
    send_rcv("Hi!", Client1, Server1),
    send_rcv("Happy to see you!", Server1, Client1),
    send_rcv("Hi, you to!", Client2, Server2),
    send_rcv("Happy to see you also!", Server2, Client2),
    close_and_check(Client1, Server1),
    send_rcv("Still there?", Client2, Server2),
    send_rcv("Yes!", Server2, Client2),
    close_and_check(Server2, Client2).
    
    
tcp_connect(Host, Port, Options) ->
    tcp_connect(Host, Port, Options, 0).
tcp_connect(Host, Port, Options, Timeout) ->
    ct:log("Try connect to ~p:~p ~p Timeout=~p", [Host, Port, Options, Timeout]),
    case gen_tcp:connect(Host, Port, Options, Timeout) of
        {error,econnrefused} ->
            timer:sleep( 2*max(Timeout,250)),
            tcp_connect(Host, Port, Options, 2*max(Timeout,250));
        {error,timeout} ->
            timer:sleep( 2*max(Timeout,250)),
            tcp_connect(Host, Port, Options, 2*max(Timeout,250));
        {ok,S} ->
            ct:log("connect to ~p:~p ~p Timeout=~p -> ~p", [Host, Port, Options, Timeout, S]),
            {ok,S}
    end.

close_and_check(OneSide, OtherSide) ->
    ok = gen_tcp:close(OneSide),
    ok = chk_closed(OtherSide).
    
    
chk_closed(Sock) ->
    chk_closed(Sock, 0).
chk_closed(Sock, Timeout) ->
    case gen_tcp:recv(Sock, 0, Timeout) of
        {error,closed} ->
            ok;
        {error,timeout} ->
            chk_closed(Sock, 2*max(Timeout,250));
        Other ->
            Other
    end.
    
send_rcv(Txt, From, To) ->
    ct:log("Send ~p from ~p to ~p", [Txt, From, To]),
    ok = gen_tcp:send(From, Txt),
    ct:log("Recv ~p on ~p", [Txt, To]),
    {ok,Txt} = gen_tcp:recv(To, 0, 5000),
    ok.    

%%--------------------------------------------------------------------
receive_data(Data, Conn) ->
    receive
	Info when is_binary(Info) ->
	    Lines = string:tokens(binary_to_list(Info), "\r\n "),
	    case lists:member(Data, Lines) of
		true ->
		    ct:log("~p:~p  Expected result ~p found in lines: ~p~n", [?MODULE,?LINE,Data,Lines]),
		    ok;
		false ->
		    ct:log("~p:~p  Extra info: ~p~n", [?MODULE,?LINE,Info]),
		    receive_data(Data, Conn)
	    end;
	Other ->
	    ct:log("~p:~p  Unexpected: ~p",[?MODULE,?LINE,Other]),
	    receive_data(Data, Conn)
    after
	30000 ->
             {State, _} = case Conn of
                              undefined -> {'??','??'};
                              _ -> sys:get_state(Conn)
                          end,
            ct:log("timeout ~p:~p~nExpect ~p~nState = ~p",[?MODULE,?LINE,Data,State]),
            ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.	

receive_logout() ->
    receive
	<<"logout">> ->
	    extra_logout(),
	    receive
		<<"Connection closed">> ->
		    ok
	    after 
		30000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
	    end;
	Info ->
	    ct:log("Extra info when logging out: ~p~n", [Info]),
	    receive_logout()
    after 
	30000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.


receive_normal_exit(Shell) ->
    receive_exit(Shell, fun(Reason) -> Reason == normal end).


receive_exit(Shell, F) when is_function(F,1) ->
    receive
        {'EXIT', Shell, Reason} ->
            case F(Reason) of
                true ->
                    ok;
                false ->
                    ct:fail({unexpected_exit, Reason})
            end;

        <<"\r\n">> ->
            receive_normal_exit(Shell);

        Other ->
            ct:fail({unexpected_msg, Other})

        after 
	30000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.


extra_logout() ->
    receive 	
	<<"logout">> ->
	    ok
    after 500 -> 
	    ok
    end.

%%--------------------------------------------------------------------
%% Check if we have a "newer" ssh client that supports these test cases
check_ssh_client_support(Config) ->
    case ssh_test_lib:ssh_client_supports_Q() of
	true ->
	    ssh:start(),
	    Config;
	_ ->
	    {skip, "test case not supported by ssh client"}
    end.

comment(AtomList) ->
    ct:comment(
      string:join(lists:map(fun erlang:atom_to_list/1, AtomList),
		", ")).

%%%----------------------------------------------------------------
no_forwarding() ->
    %%% Check if the ssh of the OS has tunneling enabled
    Cmnd = "ssh -R 0:localhost:4567 localhost exit",
    FailRegExp =
        "Port forwarding is disabled"
        "|remote port forwarding failed"
        "|Bad.*specification"
        "|Bad forwarding port",
    {Result,TheText} =
        try
            Parent = self(),
            Pid = spawn(fun() ->
                                Parent ! {self(), os:cmd(Cmnd)}
                        end),
            receive
                {Pid, Txt} ->
                    case re:run(Txt, FailRegExp) of
                        {match,_} -> {true,Txt};
                        _ -> {false,Txt}
                end
        after 10000 ->
                ct:log("*** TIMEOUT ***",[]),
                {true,""}
        end
    catch C:E:S ->
            ct:log("Exception in no_forwarding():~n~p:~p~n~p~n", [C,E,S]),
            {true, ""}
    end,
    ct:log("---- os:cmd(~p) returned:~n~s~n"
           "~n"
           "---- Checking with regexp~n"
           "~p~n"
           "~n"
           "---- The function no_forwarding() returns ~p",
           [Cmnd,TheText, FailRegExp, Result]),
    Result.
