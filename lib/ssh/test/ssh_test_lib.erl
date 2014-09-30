%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2012. All Rights Reserved.
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
%%----------------------------------------------------------------------
-module(ssh_test_lib).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("public_key/include/public_key.hrl").
-include_lib("common_test/include/ct.hrl").

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
%%ct:pal("~p",[{io_request, From, ReplyAs, Request}]),
	     {ok, Reply, Buff} = io_request(Request, TestCase, From,
					    ReplyAs, Buff0),
%%ct:pal("io_request(~p)-->~p",[Request,{ok, Reply, Buff}]),
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
%%ct:pal("io_reply ~p sending ~p ! ~p",[self(),From, {io_reply, ReplyAs, Reply}]),
    From ! {io_reply, ReplyAs, Reply}.

reply(_, []) ->
    ok;
reply(TestCase, Result) ->
%%ct:pal("reply ~p sending ~p ! ~p",[self(), TestCase, Result]),
    TestCase ! Result.

receive_exec_result(Msg) ->
    ct:pal("Expect data! ~p", [Msg]),
    receive
	{ssh_cm,_,{data,_,1, Data}} ->
	    ct:pal("StdErr: ~p~n", [Data]),
	    receive_exec_result(Msg);
	Msg ->
	    ct:pal("1: Collected data ~p", [Msg]),
	    expected;
	Other ->
	    ct:pal("Other ~p", [Other]),
	    {unexpected_msg, Other}
    end.


receive_exec_end(ConnectionRef, ChannelId) ->
    Eof = {ssh_cm, ConnectionRef, {eof, ChannelId}},
    ExitStatus = {ssh_cm, ConnectionRef, {exit_status, ChannelId, 0}},
    Closed =  {ssh_cm, ConnectionRef,{closed, ChannelId}},
    case receive_exec_result(ExitStatus) of
	{unexpected_msg, Eof} -> %% Open ssh seems to not allways send these messages
	    %% in the same order!
	    ct:pal("2: Collected data ~p", [Eof]),
	    case receive_exec_result(ExitStatus) of
		expected ->
		    expected = receive_exec_result(Closed);
		{unexpected_msg, Closed} ->
		    ct:pal("3: Collected data ~p", [Closed])
	    end;
	expected ->
	    ct:pal("4: Collected data ~p", [ExitStatus]),
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
