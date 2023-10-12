%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2023. All Rights Reserved.
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

-export([
connect/2,
connect/3,
daemon/1,
daemon/2,
daemon/3,
daemon_port/1,
daemon_port/2,
gen_tcp_connect/2,
gen_tcp_connect/3,
open_sshc/3,
open_sshc/4,
open_sshc_cmd/3,
open_sshc_cmd/4,
std_daemon/2,
std_daemon1/2,
std_connect/4,
std_simple_sftp/3,
std_simple_sftp/4,
std_simple_exec/3,
std_simple_exec/4,
start_shell/2,
start_shell/3,
start_io_server/0,
init_io_server/1,
loop_io_server/2,
io_request/5,
io_reply/3,
reply/2,
rcv_expected/3,
rcv_lingering/1,
receive_exec_result/1,
receive_exec_result_or_fail/1,
receive_exec_end/2,
receive_exec_end/3,
receive_exec_result/3,
failfun/2,
hostname/0,
del_dirs/1,
del_dir_contents/1,
do_del_files/2,
openssh_sanity_check/1,
default_algorithms/1,
default_algorithms/3,
default_algorithms/2,
run_fake_ssh/1,
extract_algos/1,
get_atoms/1,
intersection/2,
intersect/2,
intersect_bi_dir/1,
some_empty/1,
sort_spec/1,
sshc/1,
ssh_type/0,
ssh_type1/0,
installed_ssh_version/1,
algo_intersection/2,
to_atoms/1,
ssh_supports/2,
has_inet6_address/0,
open_port/1,
open_port/2,
sleep_millisec/1,
sleep_microsec/1,
busy_wait/2,
get_kex_init/1,
get_kex_init/3,
expected_state/1,
random_chars/1,
create_random_dir/1,
match_ip/2,
match_ip0/2,
match_ip1/2,
mangle_connect_address/1,
mangle_connect_address/2,
loopback/1,
mangle_connect_address1/2,
ntoa/1,
try_enable_fips_mode/0,
is_cryptolib_fips_capable/0,
report/2,
lc_name_in/1,
ptty_supported/0,
has_WSL/0,
winpath_to_linuxpath/1,
copy_recursive/2,
mk_dir_path/1,
setup_all_user_host_keys/1,
setup_all_user_host_keys/2,
setup_all_user_host_keys/3,
setup_all_host_keys/1,
setup_all_host_keys/2,
setup_all_user_keys/2,
setup_user_key/3,
setup_host_key_create_dir/3,
setup_host_key/3,
setup_known_host/3,
get_addr_str/0,
file_base_name/2
        ]).

-include_lib("common_test/include/ct.hrl").
-include("ssh_transport.hrl").
-include_lib("kernel/include/file.hrl").
-include("ssh_test_lib.hrl").

%%%----------------------------------------------------------------
connect(Port, Options) when is_integer(Port) ->
    connect(hostname(), Port, Options).

connect(any, Port, Options) ->
    connect(hostname(), Port, Options);

connect(Host, ?SSH_DEFAULT_PORT, Options0) ->
    Options =
        set_opts_if_not_set([{silently_accept_hosts, true},
                             {save_accepted_host, false},
                             {user_interaction, false}
                            ], Options0),
    do_connect(Host, ?SSH_DEFAULT_PORT, Options);

connect(Host, Port, Options0) ->
    Options =
        case proplists:get_value(user_dir,Options0) of
            undefined ->
                %% Avoid uppdating the known_hosts if it is the default one
                set_opts_if_not_set([{save_accepted_host, false}], Options0);
            _ ->
                Options0
        end,
    do_connect(Host, Port, Options).


do_connect(Host, Port, Options) ->
    R = ssh:connect(Host, Port, Options),
    ct:log("~p:~p ssh:connect(~p, ~p, ~p)~n -> ~p",[?MODULE,?LINE,Host, Port, Options, R]),
    {ok, ConnectionRef} = R,
    ConnectionRef.

set_opts_if_not_set(OptsToSet, Options0) ->
    lists:foldl(fun({K,V}, Opts) ->
                        case proplists:get_value(K, Opts) of
                            undefined ->
                                [{K,V} | Opts];
                            _ ->
                                Opts
                        end
                end, Options0, OptsToSet).

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
            R = ssh:daemon_info(Pid),
            ct:log("~p:~p ssh:daemon_info(~p) ->~n ~p",[?MODULE,?LINE,Pid,R]),
            {ok,L} = R,
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
gen_tcp_connect(Port, Options) ->
    gen_tcp_connect("localhost", Port, Options).

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
    {Data == ReadData, ConnectionRef}.

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
	    Expected = {ssh_cm, ConnectionRef, {data,ChannelId,0,<<"42">>}},
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
              ct:log("~p:~p:~p ssh_test_lib:start_shell(~p, ~p, ~p)",
                     [?MODULE,?LINE,self(), Port, IOServer, ExtraOptions]),
	      Options = [{user_interaction, false},
			 {silently_accept_hosts,true},
                         {save_accepted_host,false}
                         | ExtraOptions],
              try
                  group_leader(IOServer, self()),
                  case Port of
                      22 ->
                          Host = hostname(),
                          ct:log("Port==22 Call ssh:shell(~p, ~p)",
                                 [Host, Options]),
                          ssh:shell(Host, Options);
                      _ when is_integer(Port) ->
                          Host = hostname(),
                          ct:log("is_integer(Port) Call ssh:shell(~p, ~p, ~p)",
                                 [Host, Port, Options]),
                          ssh:shell(Host, Port, Options);
                      ConnRef when is_pid(ConnRef) ->
                          ct:log("is_pid(ConnRef) Call ssh:shell(~p)",
                                 [ConnRef]),
                          ssh:shell(ConnRef); % Options were given in ssh:connect
                      Socket ->
                          receive
                              start -> ok
                          end,
                          ct:log("Socket Call ssh:shell(~p, ~p)",
                                 [Socket, Options]),
                          ssh:shell(Socket, Options)
                  end
              of
                  R ->
                      ct:log("~p:~p ssh_test_lib:start_shell(~p, ~p, ~p) -> ~p",
                             [?MODULE,?LINE,Port, IOServer, ExtraOptions, R])
              catch
                  C:E:S ->
                      ct:log("Exception ~p:~p~n~p", [C,E,S]),
                      ct:fail("Exception",[])
              end
      end).


%%%----------------------------------------------------------------
start_io_server() ->
    spawn_link(?MODULE, init_io_server, [self()]).

init_io_server(TestCase) ->
    process_flag(trap_exit, true),
    loop_io_server(TestCase, []).

loop_io_server(TestCase, Buff0) ->
     receive
	 {input, TestCase, Line} = _INP ->
             %%ct:log("io_server ~p:~p ~p got ~p",[?MODULE,?LINE,self(),_INP]),
	     loop_io_server(TestCase, Buff0 ++ [Line]);
	 {io_request, From, ReplyAs, Request} = _REQ->
             %%ct:log("io_server ~p:~p ~p got ~p",[?MODULE,?LINE,self(),_REQ]),
	     {ok, Reply, Buff} = io_request(Request, TestCase, From,
					    ReplyAs, Buff0),
             %%ct:log("io_server ~p:~p ~p going to reply ~p",[?MODULE,?LINE,self(),Reply]),
	     io_reply(From, ReplyAs, Reply),
	     loop_io_server(TestCase, Buff);
	 {'EXIT',_, _} = _Exit ->
	     ct:log("ssh_test_lib:loop_io_server/2 got ~p",[_Exit]),
	     ok
    after 
	30000 -> ct:fail("timeout ~p:~p",[?MODULE,?LINE])
    end.

io_request(getopts,_TestCase, _, _, Buff) ->
    {ok, [], Buff};
io_request({get_geometry,columns},_TestCase, _, _, Buff) ->
    {ok, 80, Buff};
io_request({get_geometry,rows},_TestCase, _, _, Buff) ->
    {ok, 24, Buff};
io_request({put_chars, Chars}, TestCase, _, _, Buff) ->
    reply(TestCase, Chars),
    {ok, ok, Buff};
io_request({put_chars, unicode, Chars}, TestCase, _, _, Buff) when is_binary(Chars) ->
    reply(TestCase, Chars),
    {ok, ok, Buff};
io_request({put_chars, unicode, io_lib, format, [Fmt,Args]}, TestCase, _, _, Buff) ->
    reply(TestCase,  unicode:characters_to_binary(io_lib:format(Fmt,Args))),
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


receive_exec_result([]) ->
    expected;
receive_exec_result(Msgs) when is_list(Msgs) ->
    ct:log("~p:~p Expect data! ~p", [?MODULE,?FUNCTION_NAME,Msgs]),
    receive
        Msg ->
            case lists:member(Msg, Msgs)
                orelse lists:member({optional,Msg}, Msgs)
            of
                true ->
                    ct:log("~p:~p Collected data ~p", [?MODULE,?FUNCTION_NAME,Msg]),
                    receive_exec_result(Msgs--[Msg,{optional,Msg}]);
                false ->
                    case Msg of
                        {ssh_cm,_,{data,_,1, Data}} ->
                            ct:log("~p:~p unexpected StdErr: ~p~n~p~n", [?MODULE,?FUNCTION_NAME,Data,Msg]),
                            receive_exec_result(Msgs);
                        Other ->
                            ct:log("~p:~p unexpected Other ~p", [?MODULE,?FUNCTION_NAME,Other]),
                            receive_exec_result(Msgs)
                    end
            end
    after 
	30000 ->
            case lists:all(fun(M) ->
                                   is_tuple(M) andalso (element(1,M) == optional)
                           end, Msgs)
            of
                false ->
                    ct:fail("timeout ~p:~p",[?MODULE,?FUNCTION_NAME]);
                true ->
                    ct:log("~p:~p Only optional messages expected!~n ~p", [?MODULE,?FUNCTION_NAME,Msgs]),
                    expected
            end
    end;
receive_exec_result(Msg) ->
    receive_exec_result([Msg]).


receive_exec_result_or_fail(Msg) ->
    case receive_exec_result(Msg) of
        expected -> expected;
        Other -> ct:fail(Other)
    end.

receive_exec_end(ConnectionRef, ChannelId) ->
    receive_exec_end(ConnectionRef, ChannelId, 0).

receive_exec_end(ConnectionRef, ChannelId, ExitStatus) ->
    receive_exec_result(
      [{ssh_cm, ConnectionRef, {eof, ChannelId}},
       {optional, {ssh_cm, ConnectionRef, {exit_status, ChannelId, ExitStatus}}},
       {ssh_cm, ConnectionRef, {closed, ChannelId}}
      ]).

receive_exec_result(Data, ConnectionRef, ChannelId) ->
    Eof = {ssh_cm, ConnectionRef, {eof, ChannelId}},
    Closed =  {ssh_cm, ConnectionRef,{closed, ChannelId}},
    expected = receive_exec_result(Data),
    expected = receive_exec_result(Eof),
    expected = receive_exec_result(Closed).


failfun(_User, {authmethod,none}) ->
    ok;
failfun(User, Reason) ->
    error_logger:format("~p failed XXX to login: ~p~n", [User, Reason]).

hostname() ->
    {ok,Host} = inet:gethostname(),
    Host.

del_dirs(Dir) ->
    del_dir_contents(Dir),
    file:del_dir(Dir),
    ok.


del_dir_contents(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            do_del_files(Dir, Files);
        _ ->
            ok
    end.

do_del_files(Dir, Files) ->
    lists:foreach(fun(File) ->
                          FullPath = filename:join(Dir,File),
                          case filelib:is_dir(FullPath) of
                              true ->
                                  del_dirs(FullPath);
                              false ->
                                  file:delete(FullPath)
                          end
                  end, Files).


openssh_sanity_check(Config) ->
    ssh:start(),
    case ssh:connect("localhost", ?SSH_DEFAULT_PORT,
                     [{password,""},
                      {silently_accept_hosts, true},
                      {save_accepted_host, false},
                      {user_interaction, false}
                     ]) of
	{ok, Pid} ->
	    ssh:close(Pid),
	    ssh:stop(),
	    Config;
	Err ->
	    Str = lists:append(io_lib:format("~p", [Err])),
	    ssh:stop(),
	    {skip, Str}
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
                                  {save_accepted_host, false},
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
	    ct:fail("No server response (timeout) 1")
    end,

    receive
	{result,Srvr,L} ->
	    L
    after ?TIMEOUT ->
	    ct:fail("No server response (timeout) 2")
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
    
some_empty([]) ->
    false;
some_empty([{_,[]}|_]) ->
    true;
some_empty([{_,L}|T]) when is_atom(hd(L)) ->
    some_empty(T);
some_empty([{_,L}|T]) when is_tuple(hd(L)) ->
    some_empty(L) orelse some_empty(T).


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
                case installed_ssh_version(timeout) of
		    Version = "OpenSSH" ++ _ ->
                        ct:log("~p:~p Found OpenSSH  ~p",[?MODULE,?LINE,Version]),
			openSSH;
                    Other ->
			ct:log("ssh client ~p is unknown",[Other]),
			unknown
		end
	end
    catch
	Class:Exception -> 
	    ct:log("~p:~p Exception ~p:~p",[?MODULE,?LINE,Class,Exception]),
	    not_found
    end.

installed_ssh_version(TimeoutReturn) ->
    Parent = self(),
    Pid = spawn(fun() ->
                        Parent ! {open_ssh_version, os:cmd("ssh -V")}
                end),
    receive
        {open_ssh_version, V} ->
            V
    after ?TIMEOUT ->
            exit(Pid, kill),
            TimeoutReturn
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
	    receive
		{reneg_timeout,Ref} -> 
                    ct:log("~p:~p Not in 'connected' state: ~p but reneg_timeout received. Fail.",
                           [?MODULE,?LINE,State]),
		    ct:log("S = ~p", [S]),
		    ct:fail(reneg_timeout)
	    after 0 ->
                    ct:log("~p:~p Not in 'connected' state: ~p, Will try again after 100ms",[?MODULE,?LINE,State]),
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
random_chars(N) -> [($a-1)+rand:uniform($z-$a) || _<-lists:duplicate(N,x)].


create_random_dir(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Name = filename:join(PrivDir, random_chars(15)),
    case file:make_dir(Name) of
	ok -> 
	    Name;
	{error,eexist} ->
	    %% The Name already denotes an existing file system object, try again.
	    %% The likelihood of always generating an existing file name is low
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
    
%%%----------------------------------------------------------------
try_enable_fips_mode() ->
    case crypto:info_fips() of
        enabled ->
            report("FIPS mode already enabled", ?LINE),
            ok;
        not_enabled ->
            %% Erlang/crypto configured with --enable-fips
            case crypto:enable_fips_mode(true) of
		true ->
                    %% and also the cryptolib is fips enabled
                    report("FIPS mode enabled", ?LINE),
		    enabled = crypto:info_fips(),
		    ok;
		false ->
                    case is_cryptolib_fips_capable() of
                        false ->
                            report("No FIPS mode in cryptolib", ?LINE),
                            {skip, "FIPS mode not supported in cryptolib"};
                        true ->
                            ct:fail("Failed to enable FIPS mode", [])
                    end
	    end;
        not_supported ->
            report("FIPS mode not supported by Erlang/OTP", ?LINE),
            {skip, "FIPS mode not supported"}
    end.

is_cryptolib_fips_capable() ->
    [{_,_,Inf}] = crypto:info_lib(),
    nomatch =/= re:run(Inf, "(F|f)(I|i)(P|p)(S|s)").

report(Comment, Line) ->
    ct:comment(Comment),
    ct:log("~p:~p  try_enable_fips_mode~n"
           "crypto:info_lib() = ~p~n"
           "crypto:info_fips() = ~p~n"
           "crypto:supports() =~n~p~n", 
           [?MODULE, Line,
            crypto:info_lib(),
            crypto:info_fips(),
            crypto:supports()]).

%%%----------------------------------------------------------------
lc_name_in(Names) ->
    case inet:gethostname() of
        {ok,Name} ->
            lists:member(string:to_lower(Name), Names);
        Other ->
            ct:log("~p:~p  inet:gethostname() returned ~p", [?MODULE,?LINE,Other]),
            false
    end.

ptty_supported() -> not lc_name_in([]). %%["fobi"]).

%%%----------------------------------------------------------------
has_WSL() ->
    os:getenv("WSLENV") =/= false. % " =/= false" =/= "== true" :)

winpath_to_linuxpath(Path) ->
    case {has_WSL(), Path} of
        {true, [_,$:|WithoutWinInit]} ->
            "/mnt/c" ++ WithoutWinInit;
        _ ->
            Path
    end.
    
%%%----------------------------------------------------------------
copy_recursive(Src, Dst) ->
    {ok,S} = file:read_file_info(Src),
    case S#file_info.type of
        directory ->
            %%ct:log("~p:~p copy dir  ~ts -> ~ts", [?MODULE,?LINE,Src,Dst]),
            {ok,Names} = file:list_dir(Src),
            mk_dir_path(Dst),
            %%ct:log("~p:~p Names = ~p", [?MODULE,?LINE,Names]),
            lists:foreach(fun(Name) ->
                                  copy_recursive(filename:join(Src, Name),
                                                 filename:join(Dst, Name))
                          end, Names);
        _ ->
            %%ct:log("~p:~p copy file ~ts -> ~ts", [?MODULE,?LINE,Src,Dst]),
            {ok,_NumBytesCopied} = file:copy(Src, Dst)
    end.

%%%----------------------------------------------------------------
%% Make a directory even if parts of the path does not exist

mk_dir_path(DirPath) ->
    case file:make_dir(DirPath) of
        {error,eexist} ->
            %%ct:log("~p:~p dir exists ~ts", [?MODULE,?LINE,DirPath]),
            ok;
        {error,enoent} ->
            %%ct:log("~p:~p try make dirname of ~ts", [?MODULE,?LINE,DirPath]),
            case mk_dir_path( filename:dirname(DirPath) ) of
                ok ->
                    %%ct:log("~p:~p redo ~ts", [?MODULE,?LINE,DirPath]),
                    file:make_dir(DirPath);
                Error ->
                    %%ct:log("~p:~p return Error ~p ~ts", [?MODULE,?LINE,Error,DirPath]),
                    Error
            end;
        Other ->
            %%ct:log("~p:~p return Other ~p ~ts", [?MODULE,?LINE,Other,DirPath]),
            Other
    end.

%%%----------------------------------------------------------------
%%% New

setup_all_user_host_keys(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    setup_all_user_host_keys(DataDir, PrivDir).

setup_all_user_host_keys(DataDir, PrivDir) ->
    setup_all_user_host_keys(DataDir, PrivDir, filename:join(PrivDir,"system")).

setup_all_user_host_keys(DataDir, UserDir, SysDir) ->
    lists:foldl(fun(Alg, OkAlgs) ->
                        try
                            ok = ssh_test_lib:setup_user_key(Alg, DataDir, UserDir),
                            ok = ssh_test_lib:setup_host_key(Alg, DataDir, SysDir)
                        of
                            ok -> [Alg|OkAlgs]
                        catch
                            error:{badmatch, {error,enoent}} ->
                                OkAlgs;
                            C:E:S ->
                                ct:log("Exception in ~p:~p for alg ~p:  ~p:~p~n~p",
                                       [?MODULE,?FUNCTION_NAME,Alg,C,E,S]),
                                OkAlgs
                        end
                end, [], ssh_transport:supported_algorithms(public_key)).


setup_all_host_keys(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    setup_all_host_keys(DataDir, filename:join(PrivDir,"system")).

setup_all_host_keys(DataDir, SysDir) ->
    lists:foldl(fun(Alg, OkAlgs) ->
                        try
                            ok = ssh_test_lib:setup_host_key(Alg, DataDir, SysDir)
                        of
                            ok -> [Alg|OkAlgs]
                        catch
                            error:{badmatch, {error,enoent}} ->
                                OkAlgs;
                            C:E:S ->
                                ct:log("Exception in ~p:~p for alg ~p:  ~p:~p~n~p",
                                       [?MODULE,?FUNCTION_NAME,Alg,C,E,S]),
                                OkAlgs
                        end
                end, [], ssh_transport:supported_algorithms(public_key)).


setup_all_user_keys(DataDir, UserDir) ->
    lists:foldl(fun(Alg, OkAlgs) ->
                        try
                            ok = ssh_test_lib:setup_user_key(Alg, DataDir, UserDir)
                        of
                            ok -> [Alg|OkAlgs]
                        catch
                            error:{badmatch, {error,enoent}} ->
                                OkAlgs;
                            C:E:S ->
                                ct:log("Exception in ~p:~p for alg ~p:  ~p:~p~n~p",
                                       [?MODULE,?FUNCTION_NAME,Alg,C,E,S]),
                                OkAlgs
                        end
                end, [], ssh_transport:supported_algorithms(public_key)).


setup_user_key(SshAlg, DataDir, UserDir) ->
    file:make_dir(UserDir),
    %% Copy private user key to user's dir
    {ok,_} = file:copy(filename:join(DataDir, file_base_name(user_src,SshAlg)),
                       filename:join(UserDir, file_base_name(user,SshAlg))),
    %% Setup authorized_keys in user's dir
    {ok,Pub} = file:read_file(filename:join(DataDir, file_base_name(user_src,SshAlg)++".pub")),
    ok = file:write_file(filename:join(UserDir, "authorized_keys"),
                         io_lib:format("~n~s~n",[Pub]),
                         [append]),
    ?ct_log_show_file( filename:join(DataDir, file_base_name(user_src,SshAlg)++".pub") ),
    ?ct_log_show_file( filename:join(UserDir, "authorized_keys") ),
    ok.

setup_host_key_create_dir(SshAlg, DataDir, BaseDir) ->
    SysDir = filename:join(BaseDir,"system"),
    ct:log("~p:~p  SshAlg=~p~nDataDir = ~p~nBaseDir = ~p~nSysDir = ~p",[?MODULE,?LINE,SshAlg, DataDir, BaseDir,SysDir]),
    file:make_dir(SysDir),
    setup_host_key(SshAlg, DataDir, SysDir),
    SysDir.

setup_host_key(SshAlg, DataDir, SysDir) ->
    mk_dir_path(SysDir),
    %% Copy private host key to system's dir
    {ok,_} = file:copy(filename:join(DataDir, file_base_name(system_src,SshAlg)),
                       filename:join(SysDir,  file_base_name(system,SshAlg))),
    ?ct_log_show_file( filename:join(SysDir,  file_base_name(system,SshAlg)) ),
    ok.

setup_known_host(SshAlg, DataDir, UserDir) ->
    {ok,Pub} = file:read_file(filename:join(DataDir, file_base_name(system_src,SshAlg)++".pub")),
    S = lists:join(" ", lists:reverse(tl(lists:reverse(string:tokens(binary_to_list(Pub), " "))))),
    ok = file:write_file(filename:join(UserDir, "known_hosts"),
                         io_lib:format("~p~n",[S])),
    ?ct_log_show_file( filename:join(UserDir, "known_hosts") ),
    ok.


get_addr_str() ->
    {ok, Hostname} = inet:gethostname(),
    {ok, {A, B, C, D}} = inet:getaddr(Hostname, inet),
    IP = lists:concat([A, ".", B, ".", C, ".", D]),
    lists:concat([Hostname,",",IP]).


file_base_name(user,   'ecdsa-sha2-nistp256') -> "id_ecdsa";
file_base_name(user,   'ecdsa-sha2-nistp384') -> "id_ecdsa";
file_base_name(user,   'ecdsa-sha2-nistp521') -> "id_ecdsa";
file_base_name(user,   'rsa-sha2-256'       ) -> "id_rsa";
file_base_name(user,   'rsa-sha2-384'       ) -> "id_rsa";
file_base_name(user,   'rsa-sha2-512'       ) -> "id_rsa";
file_base_name(user,   'ssh-dss'            ) -> "id_dsa";
file_base_name(user,   'ssh-ed25519'        ) -> "id_ed25519";
file_base_name(user,   'ssh-ed448'          ) -> "id_ed448";
file_base_name(user,   'ssh-rsa'            ) -> "id_rsa";

file_base_name(user_src, 'ecdsa-sha2-nistp256') -> "id_ecdsa256";
file_base_name(user_src, 'ecdsa-sha2-nistp384') -> "id_ecdsa384";
file_base_name(user_src, 'ecdsa-sha2-nistp521') -> "id_ecdsa521";
file_base_name(user_src, Alg) -> file_base_name(user, Alg);

file_base_name(system, 'ecdsa-sha2-nistp256') -> "ssh_host_ecdsa_key";
file_base_name(system, 'ecdsa-sha2-nistp384') -> "ssh_host_ecdsa_key";
file_base_name(system, 'ecdsa-sha2-nistp521') -> "ssh_host_ecdsa_key";
file_base_name(system, 'rsa-sha2-256'       ) -> "ssh_host_rsa_key";
file_base_name(system, 'rsa-sha2-384'       ) -> "ssh_host_rsa_key";
file_base_name(system, 'rsa-sha2-512'       ) -> "ssh_host_rsa_key";
file_base_name(system, 'ssh-dss'            ) -> "ssh_host_dsa_key";
file_base_name(system, 'ssh-ed25519'        ) -> "ssh_host_ed25519_key";
file_base_name(system, 'ssh-ed448'          ) -> "ssh_host_ed448_key";
file_base_name(system, 'ssh-rsa'            ) -> "ssh_host_rsa_key";

file_base_name(system_src, 'ecdsa-sha2-nistp256') -> "ssh_host_ecdsa_key256";
file_base_name(system_src, 'ecdsa-sha2-nistp384') -> "ssh_host_ecdsa_key384";
file_base_name(system_src, 'ecdsa-sha2-nistp521') -> "ssh_host_ecdsa_key521";
file_base_name(system_src, Alg) -> file_base_name(system, Alg).

%%%----------------------------------------------------------------
