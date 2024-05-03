%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2024. All Rights Reserved.
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
-module(rtnode).

-export([run/1, run/2, run/3, run/4, start/1, start/3, start/4, send_commands/4, stop/1,
         start_runerl_command/3,
         check_logs/3, check_logs/4, read_logs/1, dump_logs/1,
         get_default_shell/0, get_progs/0, create_tempdir/0, timeout/1]).

-include_lib("common_test/include/ct.hrl").

%% -define(debug, true).

-ifdef(debug).
-define(dbg(Data),io:format(standard_error, "DBG: ~p\r\n",[Data])).
-else.
-define(dbg(Data),noop).
-endif.

-export([toerl_server/4]).

%%
%% Tool for running interactive shell, used by interactive_shell and io_proto SUITE
%%
run(C) ->
    run(C, [], [], []).

run(C, N) ->
    run(C, N, [], []).

run(Commands, Nodename, ErlPrefix) ->
    run(Commands, Nodename, ErlPrefix, []).

run(Commands, Nodename, ErlPrefix, Args) ->
    case start(Nodename, ErlPrefix, Args) of
        {ok, _SPid, CPid, Node, RTState} ->
            Res = catch send_commands(Node, CPid, Commands, 1),
            Logs = stop(RTState),
            case Res of
                ok ->
                    dump_logs(Logs),
                    ok;
                _ ->
                    dump_logs(Logs),
                    ok = Res
            end,
            {ok, Logs};
        Skip ->
            Skip
    end.

start(Args) ->
    start([], " ", Args, []).
start(Nodename, ErlPrefix, Args) ->
    start(Nodename, ErlPrefix, Args, []).
start(Nodename, ErlPrefix, Args, Options) ->
    case get_progs(Options) of
        {error,Reason} ->
            {skip,Reason};
        {RunErl,ToErl,[Erl|ErlArgs] = ErlWArgs} ->
            case create_tempdir() of
                {error, Reason2} ->
                    {skip, Reason2};
                Tempdir when ErlPrefix =/= [] ->
                    {SPid, Node} =
                        start_runerl_node(RunErl,
                                          ErlPrefix++"\\\""++Erl++"\\\" "++
                                              lists:join($\s, ErlArgs),
					  Tempdir,Nodename,Args),
		    CPid = start_toerl_server(ToErl,Tempdir,undefined),
                    {ok, SPid, CPid, Node, {CPid, SPid, ToErl, Tempdir}};
                Tempdir ->
                    {SPid, Node} = start_peer_runerl_node(RunErl,ErlWArgs,Tempdir,Nodename,Args),
                    CPid = start_toerl_server(ToErl,Tempdir,SPid),
                    {ok, SPid, CPid, Node, {CPid, SPid, ToErl, Tempdir}}
            end
    end.

stop({CPid, SPid, ToErl, Tempdir}) ->
    %% Unlink from peer so that we don't crash when peer quits
    unlink(SPid),
    case stop_runerl_node(CPid) of
        {error,_} ->
            catch stop_try_harder(ToErl, Tempdir, SPid);
        _ ->
            ok
    end,
    wait_for_runerl_server(SPid),
    Logs = read_logs(Tempdir),
    file:del_dir_r(Tempdir),
    Logs.

stop_try_harder(ToErl, Tempdir, SPid) ->
    CPid = start_toerl_server(ToErl, Tempdir, SPid),
    ok = send_commands(undefined, CPid,
                       [{putline,[7]},
                        {expect, " --> $"},
                        {putline, "s"},
                         {putline, "c"},
                         {putline, ""}], 1),
                       stop_runerl_node(CPid).

timeout(longest) ->
    timeout(long) + timeout(normal);
timeout(long) ->
    2 * timeout(normal);
timeout(short) ->
    timeout(normal) div 10;
timeout(normal) ->
    10000 * test_server:timetrap_scale_factor().

send_commands(Node, CPid, [{sleep, X}|T], N) ->
    ?dbg({sleep, X}),
    receive
    after X ->
	    send_commands(Node, CPid, T, N+1)
    end;
send_commands(Node, CPid, [{expect, Expect}|T], N) when is_list(Expect) ->
    send_commands(Node, CPid, [{expect, unicode, Expect}|T], N);
send_commands(Node, CPid, [{expect, Encoding, Expect}|T], N) when is_list(Expect) ->
    ?dbg({expect, Expect}),
    case command(CPid, {expect, Encoding, [Expect], timeout(normal)}) of
        ok ->
            send_commands(Node, CPid, T, N + 1);
        {expect_timeout, Got} ->
            ct:log("expect timed out waiting for ~p\ngot: ~p\n", [Expect,Got]),
            {error, timeout};
        Other ->
            Other
    end;
send_commands(Node, CPid, [{putline, Line}|T], N) ->
    send_commands(Node, CPid, [{putdata, Line ++ "\n"}|T], N);
send_commands(Node, CPid, [{putdata, Data}|T], N) ->
    ?dbg({putdata, Data}),
    case command(CPid, {send_data, Data}) of
        ok ->
	    send_commands(Node, CPid, T, N+1);
        Error ->
            Error
    end;
send_commands(Node, CPid, [{eval, Fun}|T], N) ->
    ?dbg({eval, Node, Fun}),
    case erpc:call(Node, Fun) of
        ok ->
            ?dbg({eval, ok}),
            send_commands(Node, CPid, T, N+1);
         Error ->
            ?dbg({eval, Error}),
             Error
     end;
send_commands(_Node, _CPid, [], _) ->
    ok.

command(Pid, Req) ->
    Timeout = timeout(longest),
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, Req},
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, _, _, Reason} ->
            {error, Reason}
    after Timeout ->
            io:format("timeout while executing ~p\n", [Req]),
            {error, timeout}
    end.

wait_for_runerl_server(SPid) ->
    Ref = erlang:monitor(process, SPid),
    Timeout = timeout(long),
    receive
        {'DOWN', Ref, process, SPid, _Reason} ->
            ok
    after Timeout ->
            {error, runerl_server_timeout}
    end.

stop_runerl_node(CPid) ->
    Ref = erlang:monitor(process, CPid),
    CPid ! {self(), kill_emulator},
    Timeout = timeout(longest),
    receive
        {'DOWN', Ref, process, CPid, noproc} ->
            ok;
        {'DOWN', Ref, process, CPid, normal} ->
            ok;
        {'DOWN', Ref, process, CPid, {error, Reason}} ->
            {error, Reason}
    after Timeout ->
            {error, toerl_server_timeout}
    end.

get_progs() ->
    case os:type() of
        {unix,freebsd} ->
            {error,"Can't use run_erl on FreeBSD"};
        {unix,openbsd} ->
            {error,"Can't use run_erl on OpenBSD"};
        {unix,_} ->
            RunErl = find_executable("run_erl"),
            ToErl = find_executable("to_erl"),
            Erl = string:split(ct:get_progname()," ",all),
            {RunErl, ToErl, Erl};
        _ ->
            {error,"Not a Unix OS"}
    end.
get_progs(Opts) ->
    case get_progs() of
        {RunErl, ToErl, Erl} ->
            case proplists:get_value(release, Opts) of
                undefined -> {RunErl, ToErl, Erl};
                Release ->
                    case test_server_node:find_release(Release) of
                        none -> {error, "Could not find release "++Release};
                        R -> {RunErl, ToErl, [R]}
                    end
            end;
        E -> E
    end.


find_executable(Name) ->
    case os:find_executable(Name) of
        Prog when is_list(Prog) ->
            Prog;
        false ->
            throw("Could not find " ++ Name)
    end.

create_tempdir() ->
    create_tempdir(filename:join(["/tmp","rtnode"++os:getpid()]),$A).

create_tempdir(Dir,X) when X > $Z, X < $a ->
    create_tempdir(Dir,$a);
create_tempdir(Dir,X) when X > $z -> 
    Estr = lists:flatten(
             io_lib:format("Unable to create ~s, reason eexist",
                           [Dir++[$z]])),
    {error, Estr};
create_tempdir(Dir0, Ch) ->
    %% Expect fairly standard unix.
    Dir = Dir0++[Ch],
    case file:make_dir(Dir) of
        {error, eexist} ->
            create_tempdir(Dir0, Ch+1);
        {error, Reason} ->
            Estr = lists:flatten(
                     io_lib:format("Unable to create ~s, reason ~p",
                                   [Dir,Reason])),
            {error,Estr};
        ok ->
            Dir
    end.

start_runerl_node(RunErl,Erl,Tempdir,Nodename,Args) ->
    {XArg, Node} =
        case Nodename of
            [] ->
                {[], undefined};
            _ ->
                NodenameStr = if is_atom(Nodename) -> atom_to_list(Nodename);
                                 true -> Nodename
                              end,
                [_Name,Host] = string:split(atom_to_list(node()), "@"),
                {" -sname "++ NodenameStr ++
                     " -setcookie "++atom_to_list(erlang:get_cookie()),
                 list_to_atom(NodenameStr ++ "@" ++ Host)}
        end,
    {spawn(fun() -> start_runerl_command(RunErl, Tempdir, Erl ++ XArg ++ " " ++ Args) end),
     Node}.

start_runerl_command(RunErl, Tempdir, Cmd) ->
    FullCmd = "\""++RunErl++"\" "++Tempdir++"/ "++Tempdir++" \""++Cmd++"\"",
    ct:log("~ts",[FullCmd]),
    os:cmd(FullCmd).

start_peer_runerl_node(RunErl,Erl,Tempdir,[],Args) ->
    start_peer_runerl_node(RunErl,Erl,Tempdir,peer:random_name(),Args);
start_peer_runerl_node(RunErl,Erl,Tempdir,Nodename,Args) ->
    {ok, Peer, Node} =
        ?CT_PEER(#{ name => Nodename,
                    exec => {RunErl,Erl},
                    detached => false,
                    shutdown => 10000,
                    post_process_args =>
                        fun(As) ->
                                [Tempdir++"/",Tempdir,
                                 lists:flatten(
                                   lists:join(
                                     " ",[[$',A,$'] || A <- As]))]
                        end,
                    args => ["-connect_all","false"|Args] }),
    Self = self(),
    TraceLog = filename:join(Tempdir,Nodename++".trace"),
    ct:pal("Link to trace: file://~ts",[TraceLog]),

    spawn(Node,
          fun() ->
                  try
                      %% {ok, _} = dbg:tracer(file, TraceLog),
                      %% dbg:p(whereis(user_drv),[c,m,timestamp]),
                      %% dbg:p(whereis(user_drv_reader),[c,m,timestamp]),
                      %% dbg:p(whereis(user_drv_writer),[c,m,timestamp]),
                      %% dbg:p(whereis(user),[c,m,timestamp]),
                      %% dbg:tp(user_drv,x),
                      %% dbg:tp(prim_tty,x),
                      %% dbg:tpl(prim_tty,read_nif,x),
                      Ref = monitor(process, Self),
                      receive {'DOWN',Ref,_,_,_} -> ok end
                  catch E:R:ST ->
                          io:format(user,"~p:~p:~p",[E,R,ST]),
                          erlang:raise(E,R,ST)
                  end
          end),
    {Peer, Node}.

start_toerl_server(ToErl,Tempdir,SPid) ->
    Pid = spawn(?MODULE,toerl_server,[self(),ToErl,Tempdir,SPid]),
    receive
        {Pid,started} ->
            Pid;
        {Pid,error,Reason} ->
            {error,Reason}
    end.

try_to_erl(_Command, 0) ->
    {error, cannot_to_erl};
try_to_erl(Command, N) ->
    ?dbg({?LINE,N}),
    Port = open_port({spawn, Command},[eof]),
    Timeout = timeout(short) div 2,
    receive
        {Port, eof} ->
            timer:sleep(Timeout),
            try_to_erl(Command, N-1)
    after Timeout ->
            ?dbg(Port),
            Port
    end.

toerl_server(Parent, ToErl, TempDir, SPid) ->
    Port = try_to_erl("\""++ToErl++"\" "++TempDir++"/ 2>/dev/null", 8),
    case Port of
        P when is_port(P) ->
            Parent ! {self(),started};
        {error,Other} ->
            Parent ! {self(),error,Other},
            exit(Other)
    end,

    {ok, InitialData} = file:read_file(filename:join(TempDir,"erlang.log.1")),

    State = #{port => Port, acc => unicode:characters_to_list(InitialData), spid => SPid},
    case toerl_loop(State) of
        normal ->
            ok;
        {error, Reason} ->
            error_logger:error_msg("toerl_server exit with reason ~p~n",
                                   [Reason]),
            exit(Reason)
    end.

toerl_loop(#{port := Port} = State0) ->
    ?dbg({toerl_loop, Port, map_get(acc, State0),
          maps:get(match, State0, nomatch)}),

    State = handle_expect(State0),

    receive
        {Port,{data,Data}} when is_port(Port) ->
            ?dbg({?LINE,Port,{data,Data}}),
            toerl_loop(State#{acc => map_get(acc, State) ++ Data});
        {Pid, Ref, {expect, Encoding, Expect, Timeout}} ->
            toerl_loop(init_expect(Pid, Ref, Encoding, Expect, Timeout, State));
        {Pid, Ref, {send_data, Data}} ->
            ?dbg({?LINE,Port,{send_data,Data}}),
            Port ! {self(), {command, Data}},
            Pid ! {Ref, ok},
            toerl_loop(State);
        {_Pid, kill_emulator} ->
            kill_emulator(State);
        {timeout,Timer,expect_timeout} ->
            toerl_loop(handle_expect_timeout(Timer, State));
        {Port, eof} ->
            {error, unexpected_eof};
        Other ->
            {error, {unexpected, Other}}
    end.

kill_emulator(#{spid := SPid, port := Port}) when is_pid(SPid) ->
    catch peer:stop(SPid),
    wait_for_eof(Port);
kill_emulator(#{port := Port}) ->
    %% If the line happens to end in a ".", issuing "init:stop()."
    %% will result in a syntax error.  To avoid that, issue a "\n"
    %% before "init:stop().".
    Port ! {self(),{command, "\ninit:stop().\n"}},
    wait_for_eof(Port).

wait_for_eof(Port) ->
    receive
        {Port,eof} ->
            normal;
        _Other ->
            wait_for_eof(Port)
    after
        timeout(long) ->
            {error, kill_timeout}
    end.

init_expect(Pid, Ref, Encoding, ExpectList, Timeout, State) ->
    try compile_expect(ExpectList, Encoding) of
        Expect ->
            Exp = #{expect => Expect,
                    ref => Ref,
                    source => ExpectList,
                    timer => erlang:start_timer(Timeout, self(), expect_timeout),
                    from => Pid},
            State#{expect => Exp}
    catch
        Class:Reason:Stk ->
            io:put_chars("Compilation of expect pattern failed:"),
            io:format("~p\n", [ExpectList]),
            io:put_chars(erl_error:format_exception(Class, Reason, Stk)),
            exit(expect_pattern_error)
    end.

handle_expect(#{acc := Acc, expect := Exp} = State) ->
    #{expect := Expect, from := Pid, ref := Ref} = Exp,
    case Expect(Acc) of
        nomatch ->
            State;
        {matched, Eaten, Result} ->
            ?dbg({matched, Eaten, Result}),
            Pid ! {Ref, Result},
            finish_expect(Eaten, State)
    end;
handle_expect(State) ->
    State.

handle_expect_timeout(Timer, State) ->
    #{acc := Acc, expect := Exp} = State,
    #{expect := Expect, timer := Timer, from := Pid, ref := Ref} = Exp,
    case Expect({timeout, Acc}) of
        nomatch ->
            Result = {expect_timeout, Acc},
            Pid ! {Ref, Result},
            finish_expect(0, State);
        {matched, Eaten, Result} ->
            Pid ! {Ref, Result},
            finish_expect(Eaten, State)
    end.

finish_expect(Eaten, #{acc := Acc0,
                       expect := #{timer := Timer}}=State) ->
    erlang:cancel_timer(Timer),
    receive
        {timeout,Timer,timeout} ->
            ok
    after 0 ->
            ok
    end,
    Acc = lists:nthtail(Eaten, Acc0),
    maps:remove(expect, State#{acc := Acc}).

compile_expect([{timeout,Action}|T], E) when is_function(Action, 1) ->
    Next = compile_expect(T, E),
    fun({timeout, _}=Tm) ->
            {matched, 0, Action(Tm)};
       (Subject) ->
            Next(Subject)
    end;
compile_expect([{{re,RE0},Action}|T], E) when is_binary(RE0), is_function(Action, 1) ->
    {ok, RE} = re:compile(RE0, [unicode || E =:= unicode]),
    Next = compile_expect(T, E),
    fun({timeout, _}=Subject) ->
            Next(Subject);
       (Subject) ->
            BinarySubject = if
                                E =:= unicode ->
                                    unicode:characters_to_binary(list_to_binary(Subject));
                                E =:= latin1 ->
                                    list_to_binary(Subject)
                            end,
            case re:run(BinarySubject, RE, [{capture,first,index}]) of
                nomatch ->
                    Next(Subject);
                {match, [{Pos,Len}]} ->
                    Matched = binary:part(BinarySubject, Pos, Len),
                    {matched, Pos+Len, Action(Matched)}
            end
    end;
compile_expect([RE|T], E) when is_list(RE) ->
    Ok = fun(_) -> ok end,
    compile_expect([{{re,unicode:characters_to_binary(RE, unicode, E)},Ok}|T], E);
compile_expect([], _E) ->
    fun(_) ->
            nomatch
    end.

check_logs(Logname, Pattern, Logs) ->
    check_logs(Logname, Pattern, true, Logs).
check_logs(Logname, Pattern, Match, Logs) ->
    case re:run(maps:get(Logname, Logs), Pattern) of
        {match, [_]} when Match ->
            ok;
        nomatch when not Match ->
            ok;
        _ ->
            dump_logs(Logs),
            ct:fail("~p not found in log ~ts",[Pattern, Logname])
    end.

dump_logs(Logs) ->
    maps:foreach(
      fun(File, Data) ->
              try re:replace(Data,"\e","\\\\e",[unicode,global]) of
                  D -> ct:log("~ts: ~ts",[File, D])
              catch error:badarg ->
                      ct:log("~ts: ~s",[File, re:replace(Data,"\e","\\\\e",[global])])
              end
      end, Logs).

read_logs(Tempdir) ->
    {ok, LogFiles0} = file:list_dir(Tempdir),

    %% Make sure that we only read log files and not any named pipes.
    LogFiles = [F || F <- LogFiles0,
                     case F of
                         "erlang.log" ++ _ -> true;
%                         "peer" ++ _ -> true;
                         _ -> false
                     end],

    lists:foldl(
      fun(File, Acc) ->
              case file:read_file(filename:join(Tempdir, File)) of
                  {ok, Data} ->
                      Acc#{ File => Data };
                  _ ->
                      Acc
              end
      end, #{}, LogFiles).

get_default_shell() ->
    case get_progs() of
        {error,_} ->
            noshell;
        _ ->
            try
                run([{putline,""},
                     {putline, "is_pid(whereis(user_drv))."},
                     {expect, "true\r\n"}]),
                new
            catch _E:_R ->
                    old
            end
    end.
