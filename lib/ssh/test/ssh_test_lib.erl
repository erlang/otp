%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
-module(ssh_test_lib).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("public_key/include/public_key.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("ssh/src/ssh_transport.hrl").


-define(TIMEOUT, 50000).

%%%----------------------------------------------------------------
connect(Port, Options) when is_integer(Port) ->
    connect(hostname(), Port, Options).

connect(any, Port, Options) ->
    connect(hostname(), Port, Options);
connect(Host, Port, Options) ->
    R = ssh:connect(Host, Port, Options),
    ct:log("~p:~p ssh:connect(~p, ~p, ~p)~n -> ~p",[?MODULE,?LINE,Host, Port, Options, R]),
    {ok, ConnectionRef} = R,
    ConnectionRef.

%%%----------------------------------------------------------------
daemon(Options) ->
    daemon(any, 0, Options).

daemon(Port, Options) when is_integer(Port) ->
    daemon(any, Port, Options);
daemon(Host, Options) ->
    daemon(Host, 0, Options).


daemon(Host, Port, Options) ->
    ct:log("~p:~p Calling ssh:daemon(~p, ~p, ~p)",[?MODULE,?LINE,Host,Port,Options]),
    case ssh:daemon(Host, Port, Options) of
	{ok, Pid} ->
            {ok,L} = ssh:daemon_info(Pid),
            ListenPort = proplists:get_value(port, L),
            ListenIP = proplists:get_value(ip, L),
	    {Pid, ListenIP, ListenPort};
	Error ->
	    ct:log("ssh:daemon error ~p",[Error]),
	    Error
    end.

%%%----------------------------------------------------------------
daemon_port(Pid) -> daemon_port(0, Pid).
    

daemon_port(0, Pid) -> {ok,Dinf} = ssh:daemon_info(Pid),
		       proplists:get_value(port, Dinf);
daemon_port(Port, _) -> Port.

%%%----------------------------------------------------------------
gen_tcp_connect(Host0, Port, Options) ->
    Host = ssh_test_lib:ntoa(ssh_test_lib:mangle_connect_address(Host0)),
    ct:log("~p:~p gen_tcp:connect(~p, ~p, ~p)~nHost0 = ~p",
           [?MODULE,?LINE, Host, Port, Options, Host0]),
    Result = gen_tcp:connect(Host, Port, Options),
    ct:log("~p:~p Result = ~p", [?MODULE,?LINE, Result]),
    Result.

%%%----------------------------------------------------------------
open_sshc(Host0, Port, OptStr) ->
    open_sshc(Host0, Port, OptStr, "").

open_sshc(Host0, Port, OptStr, ExecStr) ->
    Cmd = open_sshc_cmd(Host0, Port, OptStr, ExecStr),
    Result = os:cmd(Cmd),
    ct:log("~p:~p Result = ~p", [?MODULE,?LINE, Result]),
    Result.


open_sshc_cmd(Host, Port, OptStr) ->
    open_sshc_cmd(Host, Port, OptStr, "").

open_sshc_cmd(Host0, Port, OptStr, ExecStr) ->
    Host = ssh_test_lib:ntoa(ssh_test_lib:mangle_connect_address(Host0)),
    Cmd = lists:flatten(["ssh -p ", integer_to_list(Port),
                         " ", OptStr,
                         " ", Host,
                         " ", ExecStr]),
    ct:log("~p:~p OpenSSH Cmd = ~p", [?MODULE,?LINE, Cmd]),
    Cmd.

%%%----------------------------------------------------------------
std_daemon(Config, ExtraOpts) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    std_daemon1(Config, 
		ExtraOpts ++
		    [{user_dir, UserDir},
		     {user_passwords, [{"usr1","pwd1"}]}]).

std_daemon1(Config, ExtraOpts) ->
    SystemDir = proplists:get_value(data_dir, Config),
    {_Server, _Host, _Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
						   {failfun, fun ssh_test_lib:failfun/2}
						   | ExtraOpts]).

%%%----------------------------------------------------------------
std_connect(Config, Host, Port, ExtraOpts) ->
    UserDir = proplists:get_value(priv_dir, Config),
    _ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
					  {user, "usr1"},
					  {password, "pwd1"},
					  {user_interaction, false}
					  | ExtraOpts]).

%%%----------------------------------------------------------------
std_simple_sftp(Host, Port, Config) ->
    std_simple_sftp(Host, Port, Config, []).

std_simple_sftp(Host, Port, Config, Opts) ->
    UserDir = proplists:get_value(priv_dir, Config),
    DataFile = filename:join(UserDir, "test.data"),
    ConnectionRef = ssh_test_lib:std_connect(Config, Host, Port, Opts),
    {ok, ChannelRef} = ssh_sftp:start_channel(ConnectionRef),
    Data = crypto:strong_rand_bytes(proplists:get_value(std_simple_sftp_size,Config,10)),
    ok = ssh_sftp:write_file(ChannelRef, DataFile, Data),
    {ok,ReadData} = file:read_file(DataFile),
    ok = ssh:close(ConnectionRef),
    Data == ReadData.

%%%----------------------------------------------------------------
std_simple_exec(Host, Port, Config) ->
    std_simple_exec(Host, Port, Config, []).

std_simple_exec(Host, Port, Config, Opts) ->
    ct:log("~p:~p std_simple_exec",[?MODULE,?LINE]),
    ConnectionRef = ssh_test_lib:std_connect(Config, Host, Port, Opts),
    ct:log("~p:~p connected! ~p",[?MODULE,?LINE,ConnectionRef]),
    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    ct:log("~p:~p session_channel ok ~p",[?MODULE,?LINE,ChannelId]),
    ExecResult = ssh_connection:exec(ConnectionRef, ChannelId, "23+21-2.", infinity),
    ct:log("~p:~p exec ~p",[?MODULE,?LINE,ExecResult]),
    case ExecResult of
	success ->
	    Expected = {ssh_cm, ConnectionRef, {data,ChannelId,0,<<"42\n">>}},
	    case receive_exec_result(Expected) of
		expected ->
		    ok;
		Other ->
		    ct:fail(Other)
	    end,
	    receive_exec_end(ConnectionRef, ChannelId),
	    ssh:close(ConnectionRef);
	_ ->
	    ct:fail(ExecResult)
    end.

%%%----------------------------------------------------------------
start_shell(Port, IOServer) ->
    start_shell(Port, IOServer, []).

start_shell(Port, IOServer, ExtraOptions) ->
    spawn_link(
      fun() ->
	      Host = hostname(),
	      Options = [{user_interaction, false},
			 {silently_accept_hosts,true} | ExtraOptions],
	      group_leader(IOServer, self()),
	      ssh:shell(Host, Port, Options)
      end).


%%%----------------------------------------------------------------
start_io_server() ->
    spawn_link(?MODULE, init_io_server, [self()]).

init_io_server(TestCase) ->
    process_flag(trap_exit, true),
    loop_io_server(TestCase, []).

loop_io_server(TestCase, Buff0) ->
     receive
	 {input, TestCase, Line} ->
	     loop_io_server(TestCase, Buff0 ++ [Line]);
	 {io_request, From, ReplyAs, Request} ->
	     {ok, Reply, Buff} = io_request(Request, TestCase, From,
					    ReplyAs, Buff0),
	     io_reply(From, ReplyAs, Reply),
	     loop_io_server(TestCase, Buff);
	 {'EXIT',_, _} = _Exit ->
%%	     ct:log("ssh_test_lib:loop_io_server/2 got ~p",[_Exit]),
	     ok
    after 
	30000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.

io_request({put_chars, Chars}, TestCase, _, _, Buff) ->
    reply(TestCase, Chars),
    {ok, ok, Buff};
io_request({put_chars, unicode, Chars}, TestCase, _, _, Buff) when is_binary(Chars) ->
    reply(TestCase, Chars),
    {ok, ok, Buff};
io_request({put_chars, unicode, io_lib, format, [Fmt,Args]}, TestCase, _, _, Buff) ->
    reply(TestCase, io_lib:format(Fmt,Args)),
    {ok, ok, Buff};
io_request({put_chars, Enc, Chars}, TestCase, _, _, Buff) ->
    reply(TestCase, unicode:characters_to_binary(Chars,Enc,latin1)),
    {ok, ok, Buff};

io_request({get_line, _} = Request, _, From, ReplyAs, [] = Buff) ->
    erlang:send_after(1000, self(), {io_request, From, ReplyAs, Request}),
    {ok, [], Buff};
io_request({get_line, _Enc, _Prompt} = Request, _, From, ReplyAs, [] = Buff) ->
    erlang:send_after(1000, self(), {io_request, From, ReplyAs, Request}),
    {ok, [], Buff};

io_request({get_line, _Enc,_}, _, _, _, [Line | Buff]) ->
    {ok, Line, Buff}.

io_reply(_, _, []) ->
    ok;
io_reply(From, ReplyAs, Reply) ->
%%ct:log("io_reply ~p sending ~p ! ~p",[self(),From, {io_reply, ReplyAs, Reply}]),
    From ! {io_reply, ReplyAs, Reply}.

reply(_, []) ->
    ok;
reply(TestCase, Result) ->
%%ct:log("reply ~p sending ~p ! ~p",[self(), TestCase, Result]),
    TestCase ! Result.

%%%----------------------------------------------------------------
rcv_expected(Expect, SshPort, Timeout) ->
    receive
	{SshPort, Recvd} when is_function(Expect) ->
	    case Expect(Recvd) of
		true ->
		    ct:log("Got expected ~p from ~p",[Recvd,SshPort]),
		    catch port_close(SshPort),
		    rcv_lingering(50);
		false ->
		    ct:log("Got UNEXPECTED ~p~n",[Recvd]),
		    rcv_expected(Expect, SshPort, Timeout)
	    end;
	{SshPort, Expect} ->
	    ct:log("Got expected ~p from ~p",[Expect,SshPort]),
	    catch port_close(SshPort),
	    rcv_lingering(50);
	Other ->
	    ct:log("Got UNEXPECTED ~p~nExpect ~p",[Other, {SshPort,Expect}]),
	    rcv_expected(Expect, SshPort, Timeout)

    after Timeout ->
	    catch port_close(SshPort),
	    ct:fail("Did not receive answer")
    end.

rcv_lingering(Timeout) ->
    receive
	Msg ->
	    ct:log("Got LINGERING ~p",[Msg]),
	    rcv_lingering(Timeout)

    after Timeout ->
	    ct:log("No more lingering messages",[]),
	    ok
    end.


receive_exec_result(Msg) ->
    ct:log("Expect data! ~p", [Msg]),
    receive
	{ssh_cm,_,{data,_,1, Data}} ->
	    ct:log("StdErr: ~p~n", [Data]),
	    receive_exec_result(Msg);
	Msg ->
	    ct:log("1: Collected data ~p", [Msg]),
	    expected;
	Other ->
	    ct:log("Other ~p", [Other]),
	    {unexpected_msg, Other}
    after 
	30000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.


receive_exec_end(ConnectionRef, ChannelId) ->
    Eof = {ssh_cm, ConnectionRef, {eof, ChannelId}},
    ExitStatus = {ssh_cm, ConnectionRef, {exit_status, ChannelId, 0}},
    Closed =  {ssh_cm, ConnectionRef,{closed, ChannelId}},
    case receive_exec_result(ExitStatus) of
	{unexpected_msg, Eof} -> %% Open ssh seems to not allways send these messages
	    %% in the same order!
	    ct:log("2: Collected data ~p", [Eof]),
	    case receive_exec_result(ExitStatus) of
		expected ->
		    expected = receive_exec_result(Closed);
		{unexpected_msg, Closed} ->
		    ct:log("3: Collected data ~p", [Closed])
	    end;
	expected ->
	    ct:log("4: Collected data ~p", [ExitStatus]),
	    expected = receive_exec_result(Eof),
	    expected = receive_exec_result(Closed);
	Other ->
	    ct:fail({unexpected_msg, Other})
    end.

receive_exec_result(Data, ConnectionRef, ChannelId) ->
    Eof = {ssh_cm, ConnectionRef, {eof, ChannelId}},
    Closed =  {ssh_cm, ConnectionRef,{closed, ChannelId}},
    expected = receive_exec_result(Data),
    expected = receive_exec_result(Eof),
    expected = receive_exec_result(Closed).


inet_port()->
    {ok, Socket} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(Socket),
    gen_tcp:close(Socket),
    Port.

setup_ssh_auth_keys(RSAFile, DSAFile, Dir) ->
    Entries = ssh_file_entry(RSAFile) ++ ssh_file_entry(DSAFile),
    AuthKeys = public_key:ssh_encode(Entries , auth_keys),
    AuthKeysFile = filename:join(Dir, "authorized_keys"),
    file:write_file(AuthKeysFile, AuthKeys).

ssh_file_entry(PubFile) ->
    case file:read_file(PubFile) of
	{ok, Ssh} ->
	    [{Key, _}] = public_key:ssh_decode(Ssh, public_key), 
	    [{Key, [{comment, "Test"}]}];
	_ ->
	    []
    end. 
	    
failfun(_User, {authmethod,none}) ->
    ok;
failfun(User, Reason) ->
    error_logger:format("~p failed XXX to login: ~p~n", [User, Reason]).

hostname() ->
    {ok,Host} = inet:gethostname(),
    Host.

known_hosts(BR) ->
    KnownHosts = ssh_file:file_name(user, "known_hosts", []),
    B = KnownHosts ++ "xxx",
    case BR of
	backup ->
	    file:rename(KnownHosts, B);
	restore ->
	    file:delete(KnownHosts),
	    file:rename(B, KnownHosts)
    end.

setup_dsa(DataDir, UserDir) ->
    file:copy(filename:join(DataDir, "id_dsa"), filename:join(UserDir, "id_dsa")),
    System = filename:join(UserDir, "system"),
    file:make_dir(System),
    file:copy(filename:join(DataDir, "ssh_host_dsa_key"), filename:join(System, "ssh_host_dsa_key")),
    file:copy(filename:join(DataDir, "ssh_host_dsa_key.pub"), filename:join(System, "ssh_host_dsa_key.pub")),
ct:log("DataDir ~p:~n ~p~n~nSystDir ~p:~n ~p~n~nUserDir ~p:~n ~p",[DataDir, file:list_dir(DataDir), System, file:list_dir(System), UserDir, file:list_dir(UserDir)]),
    setup_dsa_known_host(DataDir, UserDir),
    setup_dsa_auth_keys(DataDir, UserDir).
    
setup_rsa(DataDir, UserDir) ->
    file:copy(filename:join(DataDir, "id_rsa"), filename:join(UserDir, "id_rsa")),
    System = filename:join(UserDir, "system"),
    file:make_dir(System),
    file:copy(filename:join(DataDir, "ssh_host_rsa_key"), filename:join(System, "ssh_host_rsa_key")),
    file:copy(filename:join(DataDir, "ssh_host_rsa_key.pub"), filename:join(System, "ssh_host_rsa_key.pub")),
ct:log("DataDir ~p:~n ~p~n~nSystDir ~p:~n ~p~n~nUserDir ~p:~n ~p",[DataDir, file:list_dir(DataDir), System, file:list_dir(System), UserDir, file:list_dir(UserDir)]),
    setup_rsa_known_host(DataDir, UserDir),
    setup_rsa_auth_keys(DataDir, UserDir).

setup_ecdsa(Size, DataDir, UserDir) ->
    file:copy(filename:join(DataDir, "id_ecdsa"++Size), filename:join(UserDir, "id_ecdsa")),
    System = filename:join(UserDir, "system"),
    file:make_dir(System),
    file:copy(filename:join(DataDir, "ssh_host_ecdsa_key"++Size), filename:join(System, "ssh_host_ecdsa_key")),
    file:copy(filename:join(DataDir, "ssh_host_ecdsa_key"++Size++".pub"), filename:join(System, "ssh_host_ecdsa_key.pub")),
ct:log("DataDir ~p:~n ~p~n~nSystDir ~p:~n ~p~n~nUserDir ~p:~n ~p",[DataDir, file:list_dir(DataDir), System, file:list_dir(System), UserDir, file:list_dir(UserDir)]),
    setup_ecdsa_known_host(Size, System, UserDir),
    setup_ecdsa_auth_keys(Size, UserDir, UserDir).

clean_dsa(UserDir) ->
    del_dirs(filename:join(UserDir, "system")),
    file:delete(filename:join(UserDir,"id_dsa")),
    file:delete(filename:join(UserDir,"known_hosts")),
    file:delete(filename:join(UserDir,"authorized_keys")).

clean_rsa(UserDir) ->
    del_dirs(filename:join(UserDir, "system")),
    file:delete(filename:join(UserDir,"id_rsa")),
    file:delete(filename:join(UserDir,"known_hosts")),
    file:delete(filename:join(UserDir,"authorized_keys")).

setup_dsa_pass_pharse(DataDir, UserDir, Phrase) ->
    {ok, KeyBin} = file:read_file(filename:join(DataDir, "id_dsa")),
    setup_pass_pharse(KeyBin, filename:join(UserDir, "id_dsa"), Phrase),
    System = filename:join(UserDir, "system"),
    file:make_dir(System),
    file:copy(filename:join(DataDir, "ssh_host_dsa_key"), filename:join(System, "ssh_host_dsa_key")),
    file:copy(filename:join(DataDir, "ssh_host_dsa_key.pub"), filename:join(System, "ssh_host_dsa_key.pub")),
    setup_dsa_known_host(DataDir, UserDir),
    setup_dsa_auth_keys(DataDir, UserDir).

setup_rsa_pass_pharse(DataDir, UserDir, Phrase) ->
    {ok, KeyBin} = file:read_file(filename:join(DataDir, "id_rsa")),
    setup_pass_pharse(KeyBin, filename:join(UserDir, "id_rsa"), Phrase),
    System = filename:join(UserDir, "system"),
    file:make_dir(System),
    file:copy(filename:join(DataDir, "ssh_host_rsa_key"), filename:join(System, "ssh_host_rsa_key")),
    file:copy(filename:join(DataDir, "ssh_host_rsa_key.pub"), filename:join(System, "ssh_host_rsa_key.pub")),
    setup_rsa_known_host(DataDir, UserDir),
    setup_rsa_auth_keys(DataDir, UserDir).

setup_pass_pharse(KeyBin, OutFile, Phrase) ->
    [{KeyType, _,_} = Entry0] = public_key:pem_decode(KeyBin),
    Key =  public_key:pem_entry_decode(Entry0),
    Salt = crypto:strong_rand_bytes(8),
    Entry = public_key:pem_entry_encode(KeyType, Key,
					{{"DES-CBC", Salt}, Phrase}),
    Pem = public_key:pem_encode([Entry]),
    file:write_file(OutFile, Pem).

setup_dsa_known_host(SystemDir, UserDir) ->
    {ok, SshBin} = file:read_file(filename:join(SystemDir, "ssh_host_dsa_key.pub")),
    [{Key, _}] = public_key:ssh_decode(SshBin, public_key),
    setup_known_hosts(Key, UserDir).

setup_rsa_known_host(SystemDir, UserDir) ->
    {ok, SshBin} = file:read_file(filename:join(SystemDir, "ssh_host_rsa_key.pub")),
    [{Key, _}] = public_key:ssh_decode(SshBin, public_key),
    setup_known_hosts(Key, UserDir).

setup_ecdsa_known_host(_Size, SystemDir, UserDir) ->
    {ok, SshBin} = file:read_file(filename:join(SystemDir, "ssh_host_ecdsa_key.pub")),
    [{Key, _}] = public_key:ssh_decode(SshBin, public_key),
    setup_known_hosts(Key, UserDir).

setup_known_hosts(Key, UserDir) ->
    {ok, Hostname} = inet:gethostname(),
    {ok, {A, B, C, D}} = inet:getaddr(Hostname, inet),
    IP = lists:concat([A, ".", B, ".", C, ".", D]),
    HostNames = [{hostnames,[Hostname, IP]}],
    KnownHosts = [{Key, HostNames}],
    KnownHostsEnc = public_key:ssh_encode(KnownHosts, known_hosts),
    KHFile = filename:join(UserDir, "known_hosts"),
    file:write_file(KHFile, KnownHostsEnc).

setup_dsa_auth_keys(Dir, UserDir) ->
    {ok, Pem} = file:read_file(filename:join(Dir, "id_dsa")),
    DSA = public_key:pem_entry_decode(hd(public_key:pem_decode(Pem))),
    PKey = DSA#'DSAPrivateKey'.y,
    P = DSA#'DSAPrivateKey'.p,
    Q = DSA#'DSAPrivateKey'.q,
    G = DSA#'DSAPrivateKey'.g,
    Dss = #'Dss-Parms'{p=P, q=Q, g=G},
    setup_auth_keys([{{PKey, Dss}, [{comment, "Test"}]}], UserDir).

setup_rsa_auth_keys(Dir, UserDir) ->
    {ok, Pem} = file:read_file(filename:join(Dir, "id_rsa")),
    RSA = public_key:pem_entry_decode(hd(public_key:pem_decode(Pem))),
    #'RSAPrivateKey'{publicExponent = E, modulus = N} = RSA,
    PKey = #'RSAPublicKey'{publicExponent = E, modulus = N},
    setup_auth_keys([{ PKey, [{comment, "Test"}]}], UserDir).

setup_ecdsa_auth_keys(_Size, Dir, UserDir) ->
    {ok, Pem} = file:read_file(filename:join(Dir, "id_ecdsa")),
    ECDSA = public_key:pem_entry_decode(hd(public_key:pem_decode(Pem))),
    #'ECPrivateKey'{publicKey = Q,
		    parameters = Param = {namedCurve,_Id0}} = ECDSA,
    PKey = #'ECPoint'{point = Q},
    setup_auth_keys([{ {PKey,Param}, [{comment, "Test"}]}], UserDir).

setup_auth_keys(Keys, Dir) ->
    AuthKeys = public_key:ssh_encode(Keys, auth_keys),
    AuthKeysFile = filename:join(Dir, "authorized_keys"),
    file:write_file(AuthKeysFile, AuthKeys).


del_dirs(Dir) ->
    case file:list_dir(Dir) of
	{ok, []} ->
	    file:del_dir(Dir);
	{ok, Files} ->
	    lists:foreach(fun(File) ->
				  FullPath = filename:join(Dir,File),
				  case filelib:is_dir(FullPath) of
				      true ->
					  del_dirs(FullPath),
					  file:del_dir(FullPath);
				      false ->
					  file:delete(FullPath)
				  end
			  end, Files);
	_ ->
	    ok
    end.

inet_port(Node) ->
    {Port, Socket} = do_inet_port(Node),
     rpc:call(Node, gen_tcp, close, [Socket]),
     Port.

do_inet_port(Node) ->
    {ok, Socket} = rpc:call(Node, gen_tcp, listen, [0, [{reuseaddr, true}]]),
    {ok, Port} = rpc:call(Node, inet, port, [Socket]),
    {Port, Socket}.

openssh_sanity_check(Config) ->
    ssh:start(),
    case ssh:connect("localhost", 22, [{password,""}]) of
	{ok, Pid} ->
	    ssh:close(Pid),
	    ssh:stop(),
	    Config;
	Err ->
	    Str = lists:append(io_lib:format("~p", [Err])),
	    ssh:stop(),
	    {skip, Str}
    end.

openssh_supports(ClientOrServer, Tag, Alg) when ClientOrServer == sshc ;
						ClientOrServer == sshd ->
    SSH_algos = ssh_test_lib:default_algorithms(ClientOrServer),
    L = proplists:get_value(Tag, SSH_algos, []),
    lists:member(Alg, L) orelse 
	lists:member(Alg, proplists:get_value(client2server, L, [])) orelse
	lists:member(Alg, proplists:get_value(server2client, L, [])).

%%--------------------------------------------------------------------
%% Check if we have a "newer" ssh client that supports these test cases

ssh_client_supports_Q() ->
    0 == check_ssh_client_support2(
	   ?MODULE:open_port({spawn, "ssh -Q cipher"})
	  ).

check_ssh_client_support2(P) ->
    receive
	{P, {data, _A}} ->
	    check_ssh_client_support2(P);
	{P, {exit_status, E}} ->
	    E
    after 5000 ->

	    ct:log("Openssh command timed out ~n"),
	    -1
    end.

%%%--------------------------------------------------------------------
%%% Probe a server or a client about algorithm support

default_algorithms(sshd) ->
    default_algorithms(sshd, "localhost", 22);

default_algorithms(sshc) ->
    default_algorithms(sshc, []).

default_algorithms(sshd, Host, Port) ->
    try run_fake_ssh(
	  ssh_trpt_test_lib:exec(
	    [{connect,Host,Port, [{silently_accept_hosts, true},
				  {user_interaction, false}]}]))
    catch
	_C:_E ->
	    ct:log("***~p:~p: ~p:~p",[?MODULE,?LINE,_C,_E]),
	    []
    end.

default_algorithms(sshc, DaemonOptions) ->
    Parent = self(),
    %% Start a process handling one connection on the server side:
    Srvr =
	spawn_link(
	  fun() ->
		  Parent ! 
		      {result, self(),
		       try
			   {ok,InitialState} = ssh_trpt_test_lib:exec(listen),
			   Parent ! {hostport,self(),ssh_trpt_test_lib:server_host_port(InitialState)},
			   run_fake_ssh(
			     ssh_trpt_test_lib:exec([{accept, DaemonOptions}],
						    InitialState))
		       catch
			   _C:_E ->
			       ct:log("***~p:~p: ~p:~p",[?MODULE,?LINE,_C,_E]),
			       []
		       end}
	  end),
   
    receive
	{hostport,Srvr,{_Host,Port}} ->
	    spawn(fun()-> os:cmd(lists:concat(["ssh -o \"StrictHostKeyChecking no\" -p ",Port," localhost"])) end)
    after ?TIMEOUT ->
	    ct:fail("No server respons 1")
    end,

    receive
	{result,Srvr,L} ->
	    L
    after ?TIMEOUT ->
	    ct:fail("No server respons 2")
    end.

run_fake_ssh({ok,InitialState}) ->
    KexInitPattern =
	#ssh_msg_kexinit{
	   kex_algorithms = '$kex_algorithms',
	   server_host_key_algorithms = '$server_host_key_algorithms',
	   encryption_algorithms_client_to_server = '$encryption_algorithms_client_to_server',
	   encryption_algorithms_server_to_client = '$encryption_algorithms_server_to_client',
	   mac_algorithms_client_to_server = '$mac_algorithms_client_to_server',
	   mac_algorithms_server_to_client = '$mac_algorithms_server_to_client',
	   compression_algorithms_client_to_server = '$compression_algorithms_client_to_server',
	   compression_algorithms_server_to_client = '$compression_algorithms_server_to_client',
	   _ = '_'
	  },
    {ok,E} = ssh_trpt_test_lib:exec([{set_options,[silent]},
				     {send, hello},
				     receive_hello,
				     {send, ssh_msg_kexinit},
				     {match, KexInitPattern, receive_msg},
				     close_socket
				    ],
				    InitialState),
     [Kex, PubKey, EncC2S, EncS2C, MacC2S, MacS2C, CompC2S, CompS2C] =
	ssh_trpt_test_lib:instantiate(['$kex_algorithms',
				       '$server_host_key_algorithms',
				       '$encryption_algorithms_client_to_server',
				       '$encryption_algorithms_server_to_client',
				       '$mac_algorithms_client_to_server',
				       '$mac_algorithms_server_to_client',
				       '$compression_algorithms_client_to_server',
				       '$compression_algorithms_server_to_client'
				      ], E),
    [{kex, to_atoms(Kex)},
     {public_key, to_atoms(PubKey)},
     {cipher, [{client2server, to_atoms(EncC2S)},
	       {server2client, to_atoms(EncS2C)}]},
     {mac, [{client2server, to_atoms(MacC2S)},
	    {server2client, to_atoms(MacS2C)}]},
     {compression, [{client2server, to_atoms(CompC2S)},
		    {server2client, to_atoms(CompS2C)}]}].
    

%%%----------------------------------------------------------------
extract_algos(Spec) ->
    [{Tag,get_atoms(List)} || {Tag,List} <- Spec].

get_atoms(L) ->
    lists:usort(
      [ A || X <- L,
	     A <- case X of
		      {_,L1} when is_list(L1) -> L1;
		      Y when is_atom(Y) -> [Y]
		  end]).


intersection(AlgoSpec1, AlgoSpec2) -> intersect(sort_spec(AlgoSpec1), sort_spec(AlgoSpec2)).

intersect([{Tag,S1}|Ss1], [{Tag,S2}|Ss2]) ->
    [{Tag,intersect(S1,S2)} | intersect(Ss1,Ss2)];
intersect(L1=[A1|_], L2=[A2|_]) when is_atom(A1),is_atom(A2) -> 
    Diff = L1 -- L2,
    L1 -- Diff;
intersect(_, _) -> 
    [].

intersect_bi_dir([{Tag,[{client2server,L1},{server2client,L2}]}|T]) ->
    [{Tag,intersect(L1,L2)} | intersect_bi_dir(T)];
intersect_bi_dir([H={_,[A|_]}|T]) when is_atom(A) ->
    [H | intersect_bi_dir(T)];
intersect_bi_dir([]) ->
    [].
    

sort_spec(L = [{_,_}|_] ) ->  [{Tag,sort_spec(Es)} || {Tag,Es} <- L];
sort_spec(L) -> lists:usort(L).

%%--------------------------------------------------------------------
sshc(Tag) -> 
    to_atoms(
      string:tokens(os:cmd(lists:concat(["ssh -Q ",Tag])), "\n")
     ).

ssh_type() ->
    Parent = self(),
    Pid = spawn(fun() -> 
			Parent ! {ssh_type,self(),ssh_type1()}
		end),
    MonitorRef = monitor(process, Pid),
    receive
	{ssh_type, Pid, Result} ->
	    demonitor(MonitorRef),
	    Result;
	{'DOWN', MonitorRef, process, Pid, _Info} ->
	    ct:log("~p:~p Process DOWN",[?MODULE,?LINE]),
	    not_found
    after
	10000 ->
	    ct:log("~p:~p Timeout",[?MODULE,?LINE]),
	    demonitor(MonitorRef),
	    not_found
    end.


ssh_type1() ->
    try 
        ct:log("~p:~p os:find_executable(\"ssh\")",[?MODULE,?LINE]),
	case os:find_executable("ssh") of
	    false -> 
		ct:log("~p:~p Executable \"ssh\" not found",[?MODULE,?LINE]),
		not_found;
	    Path ->
		ct:log("~p:~p Found \"ssh\" at ~p",[?MODULE,?LINE,Path]),
		case os:cmd("ssh -V") of
		    Version = "OpenSSH" ++ _ ->
                        ct:log("~p:~p Found OpenSSH  ~p",[?MODULE,?LINE,Version]),
			openSSH;
		    Str -> 
			ct:log("ssh client ~p is unknown",[Str]),
			unknown
		end
	end
    catch
	Class:Exception -> 
	    ct:log("~p:~p Exception ~p:~p",[?MODULE,?LINE,Class,Exception]),
	    not_found
    end.

		   

algo_intersection([], _) -> [];
algo_intersection(_, []) -> [];
algo_intersection(L1=[A1|_], L2=[A2|_]) when is_atom(A1), is_atom(A2) -> 
    true = lists:all(fun erlang:is_atom/1, L1++L2),
    lists:foldr(fun(A,Acc) ->
			case lists:member(A,L2) of
			    true -> [A|Acc];
			    false -> Acc
			end
		end, [], L1);
algo_intersection([{K,V1}|T1], L2) ->
    case lists:keysearch(K,1,L2) of
	{value, {K,V2}} ->
	    [{K,algo_intersection(V1,V2)} | algo_intersection(T1,L2)];
	false ->
	    algo_intersection(T1,L2)
    end;
algo_intersection(_, _) ->
    [].


to_atoms(L) -> lists:map(fun erlang:list_to_atom/1, L).
    
%%%----------------------------------------------------------------    
ssh_supports(Alg, SshDefaultAlg_tag) ->
    SupAlgs = 
	case proplists:get_value(SshDefaultAlg_tag,
				 ssh:default_algorithms()) of
	    [{_K1,L1}, {_K2,L2}] ->
		lists:usort(L1++L2);
	    L ->
		L
	end,
    if 
	is_atom(Alg) ->
	    lists:member(Alg, SupAlgs);
	is_list(Alg) ->
	    case Alg--SupAlgs of
		[] ->
		    true;
		UnSup ->
		    {false,UnSup}
	    end
    end.

%%%----------------------------------------------------------------
has_inet6_address() ->
    try 
	[throw(6) || {ok,L} <- [inet:getifaddrs()],
		     {_,L1} <- L,
		     {addr,{_,_,_,_,_,_,_,_}} <- L1]
    of
	[] -> false
    catch
	throw:6 -> true
    end.

%%%----------------------------------------------------------------
open_port(Arg1) ->
    ?MODULE:open_port(Arg1, []).

open_port(Arg1, ExtraOpts) ->
    erlang:open_port(Arg1,
		     [binary,
		      stderr_to_stdout,
		      exit_status,
		      use_stdio,
		      overlapped_io, hide %only affects windows
		      | ExtraOpts]).

%%%----------------------------------------------------------------
%%% Sleeping

%%% Milli sec
sleep_millisec(Nms) -> receive after Nms -> ok end.

%%% Micro sec
sleep_microsec(Nus) ->
   busy_wait(Nus, erlang:system_time(microsecond)).

busy_wait(Nus, T0) ->
    T = erlang:system_time(microsecond) - T0,
    Tleft = Nus - T,
    if
	Tleft > 2000 -> 
	    sleep_millisec((Tleft-1500) div 1000), % μs -> ms
	    busy_wait(Nus,T0);
	Tleft > 1 ->
	    busy_wait(Nus, T0);
	true ->
	    T
    end.

%%%----------------------------------------------------------------
%% get_kex_init - helper function to get key_exchange_init_msg

get_kex_init(Conn) ->
    Ref = make_ref(),
    {ok,TRef} = timer:send_after(15000, {reneg_timeout,Ref}),
    get_kex_init(Conn, Ref, TRef).

get_kex_init(Conn, Ref, TRef) ->
    %% First, validate the key exchange is complete (StateName == connected)
    {State, S} = sys:get_state(Conn),
    case expected_state(State) of
	true ->
	    timer:cancel(TRef),
	    %% Next, walk through the elements of the #state record looking
	    %% for the #ssh_msg_kexinit record. This method is robust against
	    %% changes to either record. The KEXINIT message contains a cookie
	    %% unique to each invocation of the key exchange procedure (RFC4253)
	    SL = tuple_to_list(S),
	    case lists:keyfind(ssh_msg_kexinit, 1, SL) of
		false ->
		    throw(not_found);
		KexInit ->
		    KexInit
	    end;

	false ->
	    ct:log("Not in 'connected' state: ~p",[State]),
	    receive
		{reneg_timeout,Ref} -> 
		    ct:log("S = ~p", [S]),
		    ct:fail(reneg_timeout)
	    after 0 ->
		    timer:sleep(100), % If renegotiation is complete we do not
				      % want to exit on the reneg_timeout
		    get_kex_init(Conn, Ref, TRef)
	    end
    end.
    
expected_state({ext_info,_,_}) -> true;
expected_state({connected,_}) -> true;
expected_state(_) -> false.

%%%----------------------------------------------------------------
%%% Return a string with N random characters
%%%
random_chars(N) -> [crypto:rand_uniform($a,$z) || _<-lists:duplicate(N,x)].


create_random_dir(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Name = filename:join(PrivDir, random_chars(15)),
    case file:make_dir(Name) of
	ok -> 
	    Name;
	{error,eexist} ->
	    %% The Name already denotes an existing file system object, try again.
	    %% The likelyhood of always generating an existing file name is low
	    create_random_dir(Config)
    end.

%%%----------------------------------------------------------------
match_ip(A, B) -> 
    R = match_ip0(A,B) orelse match_ip0(B,A),      
    ct:log("match_ip(~p, ~p) -> ~p",[A, B, R]),
    R.

match_ip0(A, A) ->
    true;
match_ip0(any, _) ->
    true;
match_ip0(A, B) ->
    case match_ip1(A, B) of
        true ->
            true;
        false when is_list(A) ->
            case inet:parse_address(A) of
                {ok,IPa} -> match_ip0(IPa, B);
                _ -> false
            end;
        false when is_list(B) ->
            case inet:parse_address(B) of
                {ok,IPb} -> match_ip0(A, IPb);
                _ -> false
            end;
        false ->
            false
    end.
            
match_ip1(any, _) -> true;
match_ip1(loopback,  {127,_,_,_}) ->  true;
match_ip1({0,0,0,0}, {127,_,_,_}) ->  true;
match_ip1(loopback,          {0,0,0,0,0,0,0,1}) ->  true;
match_ip1({0,0,0,0,0,0,0,0}, {0,0,0,0,0,0,0,1}) ->  true;
match_ip1(_, _) -> false.

%%%----------------------------------------------------------------
mangle_connect_address(A) ->
    mangle_connect_address(A, []).

mangle_connect_address(A, SockOpts) ->
    mangle_connect_address1(A, proplists:get_value(inet6,SockOpts,false)).

loopback(true) -> {0,0,0,0,0,0,0,1};
loopback(false) ->      {127,0,0,1}.

mangle_connect_address1( loopback,     V6flg) -> loopback(V6flg);
mangle_connect_address1(      any,     V6flg) -> loopback(V6flg);
mangle_connect_address1({0,0,0,0},         _) -> loopback(false);
mangle_connect_address1({0,0,0,0,0,0,0,0}, _) -> loopback(true);
mangle_connect_address1(       IP,     _) when is_tuple(IP) -> IP;
mangle_connect_address1(A, _) ->
    case catch inet:parse_address(A) of
        {ok,         {0,0,0,0}} -> loopback(false);
        {ok, {0,0,0,0,0,0,0,0}} -> loopback(true);
        _ -> A
    end.

%%%----------------------------------------------------------------
ntoa(A) ->
    try inet:ntoa(A)
    of
        {error,_} when is_atom(A) -> atom_to_list(A);
        {error,_} when is_list(A) -> A;
        S when is_list(S) -> S
    catch
        _:_ when is_atom(A) -> atom_to_list(A);
        _:_ when is_list(A) -> A
    end.
    
