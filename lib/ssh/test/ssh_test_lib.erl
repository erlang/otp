%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2010. All Rights Reserved.
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

start_shell(Port, IOServer) ->
    spawn_link(?MODULE, init_shell, [Port, IOServer]).

init_shell(Port, IOServer) ->
    Host = ssh_test_lib:hostname(),
    UserDir = ssh_test_lib:get_user_dir(),
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
	     %io:format("~p~n",[{input, TestCase, Line}]),
	     loop_io_server(TestCase, Buff0 ++ [Line]);
	 {io_request, From, ReplyAs, Request} ->
	     %io:format("request -> ~p~n",[Request]),
	     {ok, Reply, Buff} = io_request(Request, TestCase, From,
					    ReplyAs, Buff0),
	     %io:format("reply -> ~p~n",[Reply]),
	     io_reply(From, ReplyAs, Reply),
	     loop_io_server(TestCase, Buff);
	 {'EXIT',_, _} ->
	     erlang:display('EXIT'),
	     ok
     end.

io_request({put_chars, Chars}, TestCase, _, _, Buff) ->
    reply(TestCase, Chars),
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
    From ! {io_reply, ReplyAs, Reply}.

reply(_, []) ->
    ok;
reply(TestCase, Result) ->
    TestCase ! Result.

receive_exec_result(Msg) ->
    test_server:format("Expect data! ~p", [Msg]),
    receive
	Msg ->
	    test_server:format("1: Collected data ~p", [Msg]),
	    expected;
	Other ->
	    {unexpected_msg, Other}
    end.
receive_exec_end(ConnectionRef, ChannelId) ->
    Eof = {ssh_cm, ConnectionRef, {eof, ChannelId}},
    ExitStatus = {ssh_cm, ConnectionRef, {exit_status, ChannelId, 0}},
    Closed =  {ssh_cm, ConnectionRef,{closed, ChannelId}},
    case receive_exec_result(ExitStatus) of
	{unexpected_msg, Eof} -> %% Open ssh seems to not allways send these messages
	    %% in the same order!
	    test_server:format("2: Collected data ~p", [Eof]),
	    case receive_exec_result(ExitStatus) of
		expected ->
		    expected = receive_exec_result(Closed);
		{unexpected_msg, Closed} ->
		    test_server:format("3: Collected data ~p", [Closed])
	    end;
	expected ->
	    test_server:format("4: Collected data ~p", [ExitStatus]),
	    expected = receive_exec_result(Eof),
	    expected = receive_exec_result(Closed);
	Other ->
	    test_server:fail({unexpected_msg, Other})
    end.

receive_exec_result(Data, ConnectionRef, ChannelId) ->
    Eof = {ssh_cm, ConnectionRef, {eof, ChannelId}},
    Closed =  {ssh_cm, ConnectionRef,{closed, ChannelId}},
    expected = ssh_test_lib:receive_exec_result(Data),
    expected = ssh_test_lib:receive_exec_result(Eof),
    expected = ssh_test_lib:receive_exec_result(Closed).


inet_port()->
    {ok, Socket} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(Socket),
    gen_tcp:close(Socket),
    Port.


%% copy private keys to given dir from ~/.ssh
get_id_keys(DstDir) ->
    SrcDir = filename:join(os:getenv("HOME"), ".ssh"),
    RsaOk = copyfile(SrcDir, DstDir, "id_rsa"),
    DsaOk = copyfile(SrcDir, DstDir, "id_dsa"),
    case {RsaOk, DsaOk} of
	{{ok, _}, {ok, _}} -> {ok, both};
	{{ok, _}, _} -> {ok, rsa};
	{_, {ok, _}} -> {ok, dsa};
	{Error, _} -> Error
    end.

remove_id_keys(Dir) ->
    file:delete(filename:join(Dir, "id_rsa")),
    file:delete(filename:join(Dir, "id_dsa")).

copyfile(SrcDir, DstDir, Fn) ->
    file:copy(filename:join(SrcDir, Fn),
	      filename:join(DstDir, Fn)).

failfun(_User, {authmethod,none}) ->
    ok;
failfun(User, Reason) ->
    error_logger:format("~p failed XXX to login: ~p~n", [User, Reason]).

hostname() ->
    {ok,Host} = inet:gethostname(),
    Host.

save_known_hosts(PrivDir) ->
    Src = ssh_file:file_name(user, "known_hosts", []),
    Dst = filename:join(PrivDir, "kh_save"),
    Ok = file:copy(Src, Dst),
    io:format("save ~p -> ~p : ~p", [Src, Dst, Ok]).

restore_known_hosts(_PrivDir) ->
    %% Race condition.
    ok.
%%     Src = filename:join(PrivDir, "kh_save"),
%%     Dst = ssh_file:file_name(user, "known_hosts", []),
%%     D1 = file:delete(Dst),
%%     C = file:copy(Src, Dst),
%%     D2 = file:delete(Src),
%%     io:format("restore ~p -> ~p : ~p ~p ~p\n", [Src, Dst, D1, C, D2]).

get_user_dir() ->
    case os:type() of
	{win32, _} ->
	    [{user_dir, filename:join([os:getenv("HOME"), ".ssh"])}];
	_ ->
	    []
    end.
