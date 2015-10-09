%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2015. All Rights Reserved.
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

connect(Options) ->
    connect(hostname(), inet_port(), Options).

connect(Port, Options) when is_integer(Port) ->
    connect(hostname(), Port, Options);
connect(any, Options) ->
    connect(hostname(), inet_port(), Options);
connect(Host, Options) ->
    connect(Host, inet_port(), Options).

connect(any, Port, Options) ->
    connect(hostname(), Port, Options);
connect(Host, Port, Options) ->
    {ok, ConnectionRef} = ssh:connect(Host, Port, Options),
    ConnectionRef.

daemon(Options) ->
    daemon(any, inet_port(), Options).

daemon(Port, Options) when is_integer(Port) ->
    daemon(any, Port, Options);
daemon(Host, Options) ->
    daemon(Host, inet_port(), Options).

daemon(Host, Port, Options) ->
    case ssh:daemon(Host, Port, Options) of
	{ok, Pid} when Host == any ->
	    {Pid, hostname(), Port};
	{ok, Pid} ->
	    {Pid, Host, Port};
	Error ->
	    Error
    end.


std_daemon(Config, ExtraOpts) ->
    PrivDir = ?config(priv_dir, Config),
    UserDir = filename:join(PrivDir, nopubkey), % to make sure we don't use public-key-auth
    file:make_dir(UserDir),
    std_daemon1(Config, 
		ExtraOpts ++
		    [{user_dir, UserDir},
		     {user_passwords, [{"usr1","pwd1"}]}]).

std_daemon1(Config, ExtraOpts) ->
    SystemDir = ?config(data_dir, Config),
    {_Server, _Host, _Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
						   {failfun, fun ssh_test_lib:failfun/2}
						   | ExtraOpts]).

std_connect(Config, Host, Port, ExtraOpts) ->
    UserDir = ?config(priv_dir, Config),
    _ConnectionRef =
	ssh_test_lib:connect(Host, Port, [{silently_accept_hosts, true},
					  {user_dir, UserDir},
					  {user, "usr1"},
					  {password, "pwd1"},
					  {user_interaction, false}
					  | ExtraOpts]).

std_simple_sftp(Host, Port, Config) ->
    UserDir = ?config(priv_dir, Config),
    DataFile = filename:join(UserDir, "test.data"),
    ConnectionRef = ssh_test_lib:std_connect(Config, Host, Port, []),
    {ok, ChannelRef} = ssh_sftp:start_channel(ConnectionRef),
    Data = crypto:rand_bytes(proplists:get_value(std_simple_sftp_size,Config,10)),
    ok = ssh_sftp:write_file(ChannelRef, DataFile, Data),
    {ok,ReadData} = file:read_file(DataFile),
    ok = ssh:close(ConnectionRef),
    Data == ReadData.

std_simple_exec(Host, Port, Config) ->
    ConnectionRef = ssh_test_lib:std_connect(Config, Host, Port, []),
    {ok, ChannelId} = ssh_connection:session_channel(ConnectionRef, infinity),
    success = ssh_connection:exec(ConnectionRef, ChannelId, "23+21-2.", infinity),
    Data = {ssh_cm, ConnectionRef, {data, ChannelId, 0, <<"42\n">>}},
    case ssh_test_lib:receive_exec_result(Data) of
	expected ->
	    ok;
	Other ->
	    ct:fail(Other)
    end,
    ssh_test_lib:receive_exec_end(ConnectionRef, ChannelId).


start_shell(Port, IOServer, UserDir) ->
    start_shell(Port, IOServer, UserDir, []).

start_shell(Port, IOServer, UserDir, Options) ->
    spawn_link(?MODULE, init_shell, [Port, IOServer, [{user_dir, UserDir}|Options]]).

start_shell(Port, IOServer) ->
    spawn_link(?MODULE, init_shell, [Port, IOServer, []]).

init_shell(Port, IOServer, UserDir) ->
    Host = hostname(),
    Options = [{user_interaction, false}, {silently_accept_hosts,
					   true}] ++ UserDir,
    group_leader(IOServer, self()),
    loop_shell(Host, Port, Options).

loop_shell(Host, Port, Options) ->
    ssh:shell(Host, Port, Options).

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
%%ct:log("~p",[{io_request, From, ReplyAs, Request}]),
	     {ok, Reply, Buff} = io_request(Request, TestCase, From,
					    ReplyAs, Buff0),
%%ct:log("io_request(~p)-->~p",[Request,{ok, Reply, Buff}]),
	     io_reply(From, ReplyAs, Reply),
	     loop_io_server(TestCase, Buff);
	 {'EXIT',_, _} ->
	     erlang:display('ssh_test_lib:loop_io_server/2 EXIT'),
	     ok
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
    setup_dsa_known_host(DataDir, UserDir),
    setup_dsa_auth_keys(DataDir, UserDir).
    
setup_rsa(DataDir, UserDir) ->
    file:copy(filename:join(DataDir, "id_rsa"), filename:join(UserDir, "id_rsa")),
    System = filename:join(UserDir, "system"),
    file:make_dir(System),
    file:copy(filename:join(DataDir, "ssh_host_rsa_key"), filename:join(System, "ssh_host_rsa_key")),
    file:copy(filename:join(DataDir, "ssh_host_rsa_key"), filename:join(System, "ssh_host_rsa_key.pub")),
    setup_rsa_known_host(DataDir, UserDir),
    setup_rsa_auth_keys(DataDir, UserDir).

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
    Salt = crypto:rand_bytes(8),
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

%%--------------------------------------------------------------------
%% Check if we have a "newer" ssh client that supports these test cases

ssh_client_supports_Q() ->
    ErlPort = open_port({spawn, "ssh -Q cipher"}, [exit_status, stderr_to_stdout]),
    0 == check_ssh_client_support2(ErlPort).

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

default_algorithms(Host, Port) ->
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

    try ssh_trpt_test_lib:exec(
	   [{connect,Host,Port, [{silently_accept_hosts, true},
				 {user_interaction, false}]},
	    {send,hello},
	    receive_hello, 
	    {send, ssh_msg_kexinit},
	    {match, KexInitPattern, receive_msg},
	    close_socket])
    of
	{ok,E} ->
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
			    {server2client, to_atoms(CompS2C)}]}];
	_ ->
	    []
    catch
	_:_ ->
	    []
    end.


default_algorithms(sshd) ->
    default_algorithms("localhost", 22);
default_algorithms(sshc) ->
    case os:find_executable("ssh") of
	false -> 
	    [];
	_ ->
	    Cipher = sshc(cipher),
	    Mac = sshc(mac),
	    [{kex, sshc(kex)},
	     {public_key, sshc(key)},
	     {cipher, [{client2server, Cipher},
		       {server2client, Cipher}]},
	     {mac, [{client2server, Mac},
		    {server2client, Mac}]}
	    ]
    end.

sshc(Tag) -> 
    to_atoms(
      string:tokens(os:cmd(lists:concat(["ssh -Q ",Tag])), "\n")
     ).

ssh_type() ->
     case os:find_executable("ssh") of
	 false -> not_found;
	 _ ->
	     case os:cmd("ssh -V") of
		 "OpenSSH" ++ _ ->
		     openSSH;
		 Str -> 
		     ct:log("ssh client ~p is unknown",[Str]),
		     unknown
	     end
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
    
    
