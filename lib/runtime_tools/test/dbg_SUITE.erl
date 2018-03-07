%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
-module(dbg_SUITE).

%% Test functions
-export([all/0, suite/0,
         big/1, tiny/1, simple/1, message/1, distributed/1, port/1,
	 send/1, recv/1,
         ip_port/1, file_port/1, file_port2/1,
         ip_port_busy/1, wrap_port/1, wrap_port_time/1,
         with_seq_trace/1, dead_suspend/1, local_trace/1,
         saved_patterns/1, tracer_exit_on_stop/1,
         erl_tracer/1, distributed_erl_tracer/1]).
-export([tracee1/1, tracee2/1]).
-export([dummy/0, exported/1]).
-export([enabled/3, trace/5, load_nif/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() -> 
    [big, tiny, simple, message, distributed, port, ip_port,
     send, recv,
     file_port, file_port2, ip_port_busy,
     wrap_port, wrap_port_time, with_seq_trace, dead_suspend,
     local_trace, saved_patterns, tracer_exit_on_stop,
     erl_tracer, distributed_erl_tracer].


%% Rudimentary interface test
big(Config) when is_list(Config) ->
    {ok,OldCurDir} = file:get_cwd(),
    Datadir=proplists:get_value(data_dir, Config),
    Privdir=proplists:get_value(priv_dir, Config),
    ok=file:set_cwd(Privdir),
    try
        %% make sure dbg is stopped (and returns correctly)
        ok = dbg:stop(),

        %% compile test module and make sure it is loaded.
        {ok,Mod} = compile:file(Datadir++"/dbg_test",[trace]),
        code:purge(dbg_test),
        {module, Mod}=code:load_file(dbg_test),

        %% run/debug a named test function.
        Pid = spawn_link(dbg_test, loop, [Config]),
        true = register(dbg_test_loop, Pid),
        {ok,_} = dbg:tracer(),
        {ok,[{matched, _node, 1}]} = dbg:p(dbg_test_loop, [m,p,c]),
        ok = dbg:c(dbg_test, test, [Config]),
        ok = dbg:i(),
        dbg_test_loop ! {dbg_test, stop},
        unregister(dbg_test_loop),
        ok = dbg:stop(),

        %% run/debug a Pid.
        Pid2=spawn_link(dbg_test,loop,[Config]),
        {ok,_} = dbg:tracer(),
        {ok,[{matched, _node, 1}]} = dbg:p(Pid2,[s,r,p]),
        ok = dbg:c(dbg_test, test, [Config]),
        ok = dbg:i(),
        Pid2 ! {dbg_test, stop},

        ok=file:set_cwd(OldCurDir)
    after
        dbg:stop()
    end,
    ok.


%% Rudimentary interface test
tiny(Config) when is_list(Config) ->
    {ok,OldCurDir} = file:get_cwd(),
    Datadir=proplists:get_value(data_dir, Config),
    Privdir=proplists:get_value(priv_dir, Config),
    ok=file:set_cwd(Privdir),
    try
        %% compile test module and make sure it is loaded.
        {ok, Mod} = compile:file(Datadir++"/dbg_test",[trace]),
        code:purge(dbg_test),
        {module, Mod}=code:load_file(dbg_test),

        Pid=spawn_link(dbg_test,loop,[Config]),
        if
            is_pid(Pid) ->
                dbg:tracer(),
                {ok,[{matched, _node, 1}]} = dbg:p(Pid,[s,r,m,p,c]),
                ok = dbg:c(dbg_test,test,[Config]),
                ok = dbg:i(),
                Pid ! {dbg_test, stop};
            true ->
                ok=file:set_cwd(OldCurDir),
                ct:fail("Could not spawn external test process.~n"),
                failure
        end
    after
        ok = dbg:stop(),
        ok = file:set_cwd(OldCurDir)
    end,
    ok.

%% Simple interface test with own handler
simple(Config) when is_list(Config) ->
    try
        start(),
        dbg:p(self(),call),
        dbg:tp(dbg,ltp,[]),
        dbg:ltp(),
        stop(),
        S = self(),
        [{trace,S,call,{dbg,ltp,[]}}] = flush()
    after
        dbg:stop()
    end,
    ok.

%% Simple interface test with pam code that appends a message
message(Config) when is_list(Config) ->
    {ok, _} = start(),
    try
        {ok, [{matched, _node, 1}]} = dbg:p(self(),call),
        {ok, X} = dbg:tp(dbg,ltp,[{'_',[],[{message, {self}}]}]),
        {value, {saved, Saved}} = lists:keysearch(saved, 1, X),
        {ok, Y} = dbg:tp(dbg,ln,Saved),
        {value, {saved, Saved}} = lists:keysearch(saved, 1, Y),
        ok = dbg:ltp(),
        ok = dbg:ln()
    after
        stop()
    end,
    S = self(),
    [{trace,S,call,{dbg,ltp,[]},S},
     {trace,S,call,{dbg,ln,[]},S}] = flush(),
    ok.

send(Config) when is_list(Config) ->
    {ok, _} = start(),
    Node = start_slave(),
    rpc:call(Node, code, add_patha,
             [filename:join(proplists:get_value(data_dir, Config), "..")]),
    try
        Echo = fun F() ->
                       receive {From, M} ->
                               From ! M,
                               F()
                       end
               end,
	Rcvr = spawn_link(Echo),
        RemoteRcvr = spawn_link(Node, Echo),

	{ok, [{matched, _, 1}]} = dbg:p(Rcvr, send),

        send_test(Rcvr, make_ref(), true),

        %% Test that the test case process is the receiving process
        send_test(Rcvr, [{[self(),'_'],[],[]}]),

        %% Test that self() is not the receiving process
        {ok, [{matched, _node, 1}, {saved, 2}]} =
            dbg:tpe(send, [{['$1','_'],[{'==','$1',{self}}],[]}]),
        send_test(Rcvr, make_ref(), false),

        %% Test that self() is the sending process
        send_test(Rcvr, [{['_','_'],[{'==',Rcvr,{self}}],[]}]),

        %% Test attaching a message
        send_test(Rcvr, [{['_','_'],[{'==',Rcvr,{self}}],[{message, hello}]}],
                  make_ref(), hello),

        %% Test using a saved trace pattern
        send_test(Rcvr, 2, make_ref(), false),

        %% Test clearing of trace pattern
        {ok, [{matched, _node, 1}]} = dbg:ctpe(send),
        send_test(Rcvr, make_ref(), true),

        %% Test complex message inspection
        Ref = make_ref(),
        send_test(Rcvr,
                  [{['_','$2'],[{'==',Ref,{element,1,{element,2,'$2'}}}],[]}],
                  {test, {Ref}, <<0:(8*1024)>>}, true),

        send_test(Rcvr, {test, {make_ref()}, <<0:(8*1024)>>}, false),

        %% Test send to remote process
        remote_send_test(Rcvr, RemoteRcvr, [], make_ref(), true),

        remote_send_test(Rcvr, RemoteRcvr,
                         [{['$1','_'],[{'==',{node, '$1'},{node}}],[]}],
                         make_ref(), false),

        remote_send_test(Rcvr, RemoteRcvr,
                         [{['$1','_'],[{'==',{node, '$1'},Node}],[]}],
                         make_ref(), true),

        %% Test that distributed dbg works
        dbg:tracer(Node, process, {fun myhandler/2, self()}),
        Rcvr2 = spawn_link(Echo),
        RemoteRcvr2 = spawn_link(Node, Echo),
        dbg:p(Rcvr2, [send]),
        dbg:p(RemoteRcvr2, [send]),
        dbg:tpe(send, [{['_', hej],[],[]}]),

        send_test(Rcvr2, make_ref(), false),
        send_test(RemoteRcvr2, make_ref(), false),
        send_test(Rcvr2, hej, true),
        send_test(RemoteRcvr2, hej, true),

        ok

    after
	stop()
    end.

send_test(Pid, Pattern, Msg, TraceEvent) ->
    {ok, [{matched, _, _}, _]} = dbg:tpe(send, Pattern),
    send_test(Pid, Msg, TraceEvent).
send_test(Pid, Pattern) ->
    send_test(Pid, Pattern, make_ref(), true).
send_test(Pid, Msg, TraceEvent) ->
    S = self(),
    Pid ! {S, Msg},
    receive Msg -> ok end,
    send_test_rcv(Pid, Msg, S, TraceEvent).

remote_send_test(Pid, RPid, Pattern, Msg, TraceEvent) ->
    dbg:tpe(send, Pattern),
    TMsg = {self(), Msg},
    Pid ! {RPid, TMsg},
    receive Msg -> ok end,
    send_test_rcv(Pid, TMsg, RPid, TraceEvent).

send_test_rcv(Pid, Msg, S, TraceEvent) ->
    case flush() of
        [] when not TraceEvent ->
            ok;
        [{trace, Pid, send, Msg, S}] when TraceEvent ->
            ok;
        [{trace, Pid, send, Msg, S, Message}] when TraceEvent == Message ->
            ok;
        Else ->
            ct:fail({got_unexpected_message, Else})
    end.

recv(Config) when is_list(Config) ->
    {ok, _} = start(),
    Node = start_slave(),
    rpc:call(Node, code, add_patha,
             [filename:join(proplists:get_value(data_dir, Config), "..")]),
    try
        Echo = fun F() ->
                       receive {From, M} ->
                               From ! M,
                               F()
                       end
               end,
	Rcvr = spawn_link(Echo),
        RemoteRcvr = spawn_link(Node, Echo),

	{ok, [{matched, _, 1}]} = dbg:p(Rcvr, 'receive'),

        recv_test(Rcvr, make_ref(), true),

        %% Test that the test case process is the sending process
        recv_test(Rcvr, [{[node(), self(), '_'],[],[]}]),

        %% Test that self() is the not sending process
        {ok, [{matched, _node, 1}, {saved, 2}]} =
            dbg:tpe('receive', [{[node(), '$1','_'],[{'==','$1',{self}}],[]}]),
        recv_test(Rcvr, make_ref(), false),

        %% Test that self() is the receiving process
        recv_test(Rcvr, [{'_',[{'==',Rcvr,{self}}],[]}]),

        %% Test attaching a message
        recv_test(Rcvr, [{'_',[{'==',Rcvr,{self}}],[{message, hello}]}],
                  make_ref(), hello),

        %% Test using a saved trace pattern
        recv_test(Rcvr, 2, make_ref(), false),

        %% Test clearing of trace pattern
        {ok, [{matched, _node, 1}]} = dbg:ctpe('receive'),
        recv_test(Rcvr, make_ref(), true),

        %% Test complex message inspection
        Ref = make_ref(),
        recv_test(Rcvr,
                  [{[node(), '_','$2'],[{'==',Ref,{element,1,
                                                   {element,2,
                                                    {element,2,'$2'}}}}],[]}],
                  {test, {Ref}, <<0:(8*1024)>>}, true),

        recv_test(Rcvr, {test, {make_ref()}, <<0:(8*1024)>>}, false),

        %% Test recv to remote process
        remote_recv_test(RemoteRcvr, Rcvr, [], make_ref(), true),

        remote_recv_test(RemoteRcvr, Rcvr,
                         [{['$1',undefined,'_'],[{'==','$1',{node}}],[]}],
                         make_ref(), false),

        remote_recv_test(RemoteRcvr, Rcvr,
                         [{['$1',undefined,'_'],[{'==','$1',Node}],[]}],
                         make_ref(), true),

        %% Test that distributed dbg works
        dbg:tracer(Node, process, {fun myhandler/2, self()}),
        Rcvr2 = spawn_link(Echo),
        RemoteRcvr2 = spawn_link(Node, Echo),
        dbg:p(Rcvr2, ['receive']),
        dbg:p(RemoteRcvr2, ['receive']),
        dbg:tpe('receive', [{[node(), '_', '$1'],[{'==',{element,2,'$1'}, hej}],[]}]),

        recv_test(Rcvr2, make_ref(), false),
        recv_test(RemoteRcvr2, make_ref(), false),
        recv_test(Rcvr2, hej, true),
        recv_test(RemoteRcvr2, hej, true),

        ok

    after
	stop()
    end.

recv_test(Pid, Pattern, Msg, TraceEvent) ->
    {ok, [{matched, _, _}, _]} = dbg:tpe('receive', Pattern),
    recv_test(Pid, Msg, TraceEvent).
recv_test(Pid, Pattern) ->
    recv_test(Pid, Pattern, make_ref(), true).
recv_test(Pid, Msg, TraceEvent) ->
    S = self(),
    Pid ! {S, Msg},
    receive Msg -> ok end,
    recv_test_rcv(Pid, {S, Msg}, TraceEvent).

remote_recv_test(RPid, Pid, Pattern, Msg, TraceEvent) ->
    dbg:tpe('receive', Pattern),
    TMsg = {self(), Msg},
    RPid ! {Pid, TMsg},
    receive Msg -> ok end,
    recv_test_rcv(Pid, TMsg, TraceEvent).

recv_test_rcv(Pid, Msg, TraceEvent) ->
    case flush() of
        [] when not TraceEvent ->
            ok;
        [{trace, Pid, 'receive', Msg}] when TraceEvent ->
            ok;
        [{trace, Pid, 'receive', Msg, Message}] when TraceEvent == Message ->
            ok;
        Else ->
            ct:fail({got_unexpected_message, Else})
    end.

%% Simple test of distributed tracing
distributed(Config) when is_list(Config) ->
    {ok, _} = start(),
    Node = start_slave(),
    try
        RexPid = rpc:call(Node, erlang, whereis, [rex]),
        RexPidList = pid_to_list(RexPid),
        {ok, Node} = dbg:n(Node),
        {ok, X} = dbg:p(all,call),
        {value, {matched, Node, _}} = lists:keysearch(Node, 2, X),
        {ok, Y} = dbg:p(RexPidList, s),
        {value, {matched, Node, 1}} = lists:keysearch(Node, 2, Y),
        {ok, Z} = dbg:tp(dbg,ltp,[]),
        {value, {matched, Node, 1}} = lists:keysearch(Node, 2, Z),
        dbg:cn(Node),
        dbg:tp(dbg,ln,[]),
        ok = rpc:block_call(Node, dbg, ltp, []),
        ok = rpc:block_call(Node, dbg, ln, []),
        ok = dbg:ln(),
        S = self(),
        {TraceSend, TraceCall} =
        lists:partition(fun ({trace,RP,send,_,_}) when RP =:= RexPid -> true;
                            (_) -> false end,
                        flush()),
        [_|_] = TraceSend,
        [{trace,Pid,call,{dbg,ltp,[]}},
         {trace,S,call,{dbg,ln,[]}}] = TraceCall,
        Node = node(Pid),
        %%
        stop()
    after
        stop_slave(Node),
        stop()
    end,
    ok.


%% Tests tracing of local function calls.
local_trace(Config) when is_list(Config) ->
    {ok, _} = start(),
    try
        S = self(),
        %% Split "<X.Y.Z>" into {X, Y, Z}
        "<"++L1 = L = pid_to_list(S),
        NoDot = fun ($.) -> false; (_) -> true end,
        {LX,"."++L2} = lists:splitwith(NoDot, L1),
        {LY,"."++L3} = lists:splitwith(NoDot, L2),
        ">"++L4 = lists:reverse(L3),
        LZ = lists:reverse(L4),
        X = 0 = list_to_integer(LX),
        Y = list_to_integer(LY),
        Z = list_to_integer(LZ),
        XYZ = {X, Y, Z},
        io:format("Self = ~w = ~w~n", [S,XYZ]),
        {ok, [{matched, _node, 1}]} = dbg:p(S,call),
        {ok, [{matched, _node, 1}]} = dbg:p(XYZ,call),
        if Z =:= 0 ->
               {ok, [{matched, _node, 1}]} = dbg:p(Y,call);
           true -> ok
        end,
        {ok, [{matched, _node, 1}]} = dbg:p(L,call),
        {ok, _} = dbg:tpl(?MODULE,not_exported,[]),
        4 = not_exported(2),
        [{trace,S,call,{?MODULE,not_exported,[2]}}] = flush(),
        {ok, _} = dbg:tp(?MODULE,exported,[]),
        4 = ?MODULE:exported(2),
        [{trace,S,call,{?MODULE,exported,[2]}},
         {trace,S,call,{?MODULE,not_exported,[2]}}] = flush(),
        {ok, _} = dbg:ctpl(?MODULE),
        4 = ?MODULE:exported(2),
        [{trace,S,call,{?MODULE,exported,[2]}}] = flush(),
        {ok, _} = dbg:tpl(?MODULE,not_exported,[]),
        {ok, _} = dbg:ctp(?MODULE),
        4 = ?MODULE:exported(2),
        [] = flush(),
        {ok, _} = dbg:tpl(?MODULE,not_exported,x),
        catch ?MODULE:exported(x),
        [{trace,S,call,{dbg_SUITE,not_exported,[x]}},
         {trace,S,exception_from,
          {dbg_SUITE,not_exported,1},
          {error,badarith}}] = flush()
    after
        stop()
    end,
    ok.

%% Test that tracing on port works
port(Config) when is_list(Config) ->
    try
        S = self(),
        start(),
        TestFile = filename:join(proplists:get_value(priv_dir, Config),"port_test"),
        Fun = dbg:trace_port(file, TestFile),

        %% Do a run to get rid of all extra port operations
        port_close(Fun()),

        dbg:p(new,ports),
        Port = Fun(),
        port_close(Port),
        stop(),

        TraceFileDrv = list_to_atom(lists:flatten(["trace_file_drv n ",TestFile])),
        [{trace,Port,open,S,TraceFileDrv},
         {trace,Port,getting_linked,S},
         {trace,Port,closed,normal}] = flush()
    after
        dbg:stop()
    end,
    ok.

%% Tests saving of match_spec's.
saved_patterns(Config) when is_list(Config) ->
    dbg:stop(),
    {ok,[{saved,1}]} =
    dbg:tp(dbg,ctp,1,[{'_',[],[{message, blahonga}]}]),
    {ok,[{saved,2}]} =
    dbg:tp(dbg,ctp,1,[{['_'],[],[{message, blahonga}]}]),
    Privdir=proplists:get_value(priv_dir, Config),
    file:make_dir(Privdir),
    File = filename:join([Privdir, "blahonga.ms"]),
    dbg:wtp(File),
    dbg:stop(),
    dbg:ctp('_','_','_'),
    {ok, _} = start(),
    try
        dbg:rtp(File),
        {ok,[{matched,_node,1},{saved,1}]} = dbg:tp(dbg,ltp,0,1),
        {ok, [{matched, _node, 1}]} = dbg:p(self(),call),
        dbg:ltp(),
        S = self(),
        [{trace,S,call,{dbg,ltp,[]},blahonga}] = flush()
    after
        stop()
    end,
    ok.



not_exported(N) ->
    N * 2.

exported(N) ->
    not_exported(N).

%% Test tracing to IP port
ip_port(Config) when is_list(Config) ->
    stop(),
    Port = dbg:trace_port(ip, 0),
    {ok, _} = dbg:tracer(port, Port),
    try
        {ok, [{matched, _node, 1}]} = dbg:p(self(),call),
        {ok, X} = dbg:tp(dbg, ltp,[{'_',[],[{message, {self}}]}]),
        {value, {saved, _Saved}} = lists:keysearch(saved, 1, X),
        {ok, Y} = dbg:tp(dbg, ln, [{'_',[],[{message, hej}]}]),
        {value, {saved, _}} = lists:keysearch(saved, 1, Y),
        ok = dbg:ltp(),
        ok = dbg:ln(),
        {ok, IpPort} = dbg:trace_port_control(get_listen_port),
        io:format("IpPort = ~p~n", [IpPort]),
        dbg:trace_client(ip, IpPort, {fun myhandler/2, self()}),
        S = self(),
        [{trace,S,call,{dbg,ltp,[]},S},
         {trace,S,call,{dbg,ln,[]},hej}] = flush()
    after
        stop()
    end,
    ok.



%% Test that the dbg server does not hang if the tracer don't start (OTP-3592)
ip_port_busy(Config) when is_list(Config) ->
    stop(),
    Tracer = dbg:trace_port(ip, 4745),
    Port = Tracer(),
    {error, Reason} = dbg:tracer(port, Tracer),
    try
        io:format("Error reason = ~p~n", [Reason]),
        true = port_close(Port)
    after
        dbg:stop()
    end,
    ok.



%% Test tracing to file port (simple)
file_port(Config) when is_list(Config) ->
    stop(),
    {A,B,C} = erlang:now(),
    FTMP =  atom_to_list(?MODULE) ++ integer_to_list(A) ++ "-" ++
    integer_to_list(B) ++ "-" ++ integer_to_list(C),
    FName = filename:join([proplists:get_value(data_dir, Config), FTMP]),
    Port = dbg:trace_port(file, FName),
    {ok, _} = dbg:tracer(port, Port),
    try
        {ok, [{matched, _node, 1}]} = dbg:p(self(),call),
        {ok, X} = dbg:tp(dbg, ltp,[{'_',[],[{message, {self}}]}]),
        {value, {saved, _Saved}} = lists:keysearch(saved, 1, X),
        {ok, Y} = dbg:tp(dbg, ln, [{'_',[],[{message, hej}]}]),
        {value, {saved, _}} = lists:keysearch(saved, 1, Y),
        ok = dbg:ltp(),
        ok = dbg:ln(),
        stop(),
        dbg:trace_client(file, FName, {fun myhandler/2, self()}),
        S = self(),
        [{trace,S,call,{dbg,ltp,[]},S},
         {trace,S,call,{dbg,ln,[]},hej},
         end_of_trace] = flush()
    after
        stop(),
        file:delete(FName)
    end,
    ok.

%% Test tracing to file port with 'follow_file'
file_port2(Config) when is_list(Config) ->
    stop(),
    {A,B,C} = erlang:now(),
    FTMP =  atom_to_list(?MODULE) ++ integer_to_list(A) ++
    "-" ++ integer_to_list(B) ++ "-" ++ integer_to_list(C),
    FName = filename:join([proplists:get_value(data_dir, Config), FTMP]),
    %% Ok, lets try with flush and follow_file, not a chance on VxWorks
    %% with NFS caching...
    Port2 = dbg:trace_port(file, FName),
    {ok, _} = dbg:tracer(port, Port2),
    try
        {ok, [{matched, _node, 1}]} = dbg:p(self(),call),
        {ok, _} = dbg:tp(dbg, ltp,[{'_',[],[{message, {self}}]}]),
        {ok, _} = dbg:tp(dbg, ln, [{'_',[],[{message, hej}]}]),
        ok = dbg:ltp(),
        ok = dbg:flush_trace_port(),
        dbg:trace_client(follow_file, FName,
                         {fun myhandler/2, self()}),
        S = self(),
        [{trace,S,call,{dbg,ltp,[]},S}] = flush(),
        ok = dbg:ln(),
        ok = dbg:flush_trace_port(),
        receive after 1000 -> ok end, %% Polls every second...
        [{trace,S,call,{dbg,ln,[]},hej}] = flush(),
        stop(),
        [] = flush()
    after
        stop(),
        file:delete(FName)
    end,
    ok.

%% Test tracing to wrapping file port
wrap_port(Config) when is_list(Config) ->
    Self = self(),
    stop(),
    {A,B,C} = erlang:now(),
    FTMP =  atom_to_list(?MODULE) ++ integer_to_list(A) ++ "-" ++
    integer_to_list(B) ++ "-" ++ integer_to_list(C) ++ "-",
    FName = filename:join([proplists:get_value(data_dir, Config), FTMP]),
    FNameWildcard = FName++"*"++".trace",
    %% WrapSize=0 and WrapCnt=11 will force the trace to wrap after
    %% every trace message, and to contain only the last 10 entries
    %% after trace stop since the last file will be empty waiting
    %% for its first trace message.
    WrapSize = 0,
    WrapCnt = 11,
    WrapFilesSpec = {FName, wrap, ".trace", WrapSize, WrapCnt},
    wrap_port_init(WrapFilesSpec),
    %% The number of iterations, N, is tested to place wrap the log,
    %% giving a gap in the filename sequence at index 3.
    %% This should be a difficult case for
    %% the trace_client file sorting functionality.
    N = 7,
    lists:foreach(
      fun(Cnt) ->
              ?MODULE:tracee1(Cnt),
              ?MODULE:tracee2(Cnt)
      end,
      lists:seq(1, N)),
    stop(),
    try
        Files1 = filelib:wildcard(FNameWildcard),
        io:format("~p~n", [Files1]),
        Tc1 = dbg:trace_client(file, WrapFilesSpec,
                               {fun myhandler/2, {wait_for_go,Self}}),
        Tref1 = erlang:monitor(process, Tc1),
        Tc1 ! {go,Self},
        [{'DOWN',Tref1,_,_,normal},
         end_of_trace
         |Result] = lists:reverse(flush()),
        M = N - (WrapCnt-1) div 2,
        M = wrap_port_result(Result, Self, N),
        %%
        %% Start a new wrap log with the same name to verify that
        %% all files are cleared at wrap log start. Only produce
        %% two trace messages to also place the gap at index 3,
        %% so the trace log will be misinterpreted.
        %%
        wrap_port_init(WrapFilesSpec),
        Files2 = filelib:wildcard(FNameWildcard),
        io:format("~p~n", [Files2]),
        -1 = ?MODULE:tracee1(-1),
        -1 = ?MODULE:tracee2(-1),
        stop(),
        Files = filelib:wildcard(FNameWildcard),
        io:format("~p~n", [Files]),
        Tc2 = dbg:trace_client(file, WrapFilesSpec,
                               {fun myhandler/2, {wait_for_go,Self}}),
        Tref2 = erlang:monitor(process, Tc2),
        Tc2 ! {go,Self},
        [{trace,Self,call,{?MODULE,tracee1,[-1]},Self},
         {trace,Self,call,{?MODULE,tracee2,[-1]},hej},
         end_of_trace,
         {'DOWN',Tref2,_,_,normal}] = flush(),
        %%
        lists:map(fun(F) -> file:delete(F) end, Files)
    after
        stop()
    end,
    ok.

wrap_port_init(WrapFilesSpec) ->
    Port = dbg:trace_port(file, WrapFilesSpec),
    {ok, _} = dbg:tracer(port, Port),
    {ok, [{matched, _node, 1}]} = dbg:p(self(),call),
    {ok, X} = dbg:tp(?MODULE, tracee1,[{'_',[],[{message, {self}}]}]),
    {value, {saved, _Saved}} = lists:keysearch(saved, 1, X),
    {ok, Y} = dbg:tp(?MODULE, tracee2, [{'_',[],[{message, hej}]}]),
    {value, {saved, _}} = lists:keysearch(saved, 1, Y),
    ok.

tracee1(X) ->
    X.

tracee2(X) ->
    X.

wrap_port_result([], _S, M) ->
    M;
wrap_port_result([{trace, S, call, {?MODULE, tracee2, [M]}, hej},
                  {trace, S, call, {?MODULE, tracee1, [M]}, S} | Tail],
                 S,
                 M) ->
    wrap_port_result(Tail, S, M-1).


%% Test tracing to time limited wrapping file port
wrap_port_time(Config) when is_list(Config) ->
    stop(),
    {A,B,C} = erlang:now(),
    FTMP =  atom_to_list(?MODULE) ++ integer_to_list(A) ++ "-" ++
    integer_to_list(B) ++ "-" ++ integer_to_list(C) ++ "-",
    FName = filename:join([proplists:get_value(data_dir, Config), FTMP]),
    %% WrapTime=2 and WrapCnt=4 will force the trace to wrap after
    %% every 2 seconds, and to contain between 3*2 and 4*2 seconds
    %% of trace entries.
    WrapFilesSpec = {FName, wrap, ".trace", {time, 2000}, 4},
    Port = dbg:trace_port(file, WrapFilesSpec),
    {ok, _} = dbg:tracer(port, Port),
    try
        {ok, [{matched, _node, 1}]} = dbg:p(self(),call),
        {ok, X} = dbg:tp(?MODULE, tracee1,[{'_',[],[{message, {self}}]}]),
        {value, {saved, _Saved1}} = lists:keysearch(saved, 1, X),
        {ok, Y} = dbg:tp(?MODULE, tracee2, [{'_',[],[{message, hej}]}]),
        {value, {saved, _Saved2}} = lists:keysearch(saved, 1, Y),
        %% The delays in the iterations places two trace messages in each
        %% trace file, but the last which is empty waiting for its first
        %% trace message. The number of iterations is chosen so that
        %% one trace file has been wasted, and therefore the first pair
        %% of trace messages.
        lists:foreach(
          fun(Cnt) ->
                  receive after 1000 -> ok end,
                  ?MODULE:tracee1(Cnt),
                  ?MODULE:tracee2(Cnt),
                  receive after 1100 -> ok end
          end,
          lists:seq(1, 4)),
        stop(),
        Files = filelib:wildcard(FName ++ "*" ++ ".trace"),
        io:format("~p~n", [Files]),
        dbg:trace_client(file, WrapFilesSpec, {fun myhandler/2, self()}),
        S = self(),
        [{trace, S, call, {?MODULE, tracee1, [2]}, S},
         {trace, S, call, {?MODULE, tracee2, [2]}, hej},
         {trace, S, call, {?MODULE, tracee1, [3]}, S},
         {trace, S, call, {?MODULE, tracee2, [3]}, hej},
         {trace, S, call, {?MODULE, tracee1, [4]}, S},
         {trace, S, call, {?MODULE, tracee2, [4]}, hej},
         end_of_trace] = flush(),
        lists:map(fun(F) -> file:delete(F) end, Files)
    after
        stop()
    end,
    ok.

%% Test ordinary tracing combined with seq_trace
with_seq_trace(Config) when is_list(Config) ->
    try
        {ok, Server} = start(),
        {ok, Tracer} = dbg:get_tracer(),
        {ok, X} = dbg:tp(dbg, get_tracer, [{[],[],
                                            [{set_seq_token, send, true}]}]),
        {value, {saved, _}} = lists:keysearch(saved, 1, X),
        {ok, [{matched, _node, 1}]} = dbg:p(self(),call),
        seq_trace:set_system_tracer(Tracer),
        dbg:get_tracer(),
        receive
        after 1 ->
                  ok
        end,
        S = self(),
        ThisNode = node(),
        [{trace,S,call,{dbg,get_tracer,[]}},
         {seq_trace,0,{send,_,S,Server,{S,{get_tracer,ThisNode}}}},
         {seq_trace,0,{send,_,Server,S,{dbg,{ok,Tracer}}}}] =
        flush()
    after
        stop()
    end,
    ok.


%% Test that trace messages concerning a now dead process does not crash dbg.
dead_suspend(Config) when is_list(Config) ->
    start(),
    try
        survived = run_dead_suspend()
    after
        stop()
    end.

run_dead_suspend() ->
    dbg:p(new, call),
    dbg:tp(?MODULE, dummy, []),
    spawn(?MODULE, dummy, []),
    spawn(?MODULE, dummy, []),
    spawn(?MODULE, dummy, []),
    spawn(?MODULE, dummy, []),
    spawn(?MODULE, dummy, []),
    receive after 1000 -> ok end,
    case whereis(dbg) of
        undefined ->
            died;
        _ ->
            survived
    end.

dummy() ->
    ok.

%% Test that a tracer process does not ignore an exit signal message when it has
%% received (but not handled) trace messages
tracer_exit_on_stop(_) ->
    %% Tracer blocks waiting for fun to complete so that the trace message and
    %% the exit signal message from the dbg process are in its message queue.
    Fun = fun() ->
                  ?MODULE:dummy(),
                  Ref = erlang:trace_delivered(self()),
                  receive {trace_delivered, _, Ref} -> stop() end
          end,
    {ok, _} = dbg:tracer(process, {fun spawn_once_handler/2, {self(), Fun}}),
    {ok, Tracer} = dbg:get_tracer(),
    MRef = monitor(process, Tracer),
    {ok, _} = dbg:p(self(), [call]),
    {ok, _} = dbg:p(new, [call]),
    {ok, _} = dbg:tp(?MODULE, dummy, []),
    ?MODULE:dummy(),
    receive {'DOWN', MRef, _, _, normal} -> ok end,
    [{trace,_,call,{?MODULE, dummy,[]}},
     {trace,_,call,{?MODULE, dummy,[]}}] = flush(),
    ok.

spawn_once_handler(Event, {Pid, done} = State) ->
    Pid ! Event,
    State;
spawn_once_handler(Event, {Pid, Fun}) ->
    {_, Ref} = spawn_monitor(Fun),
    receive
        {'DOWN', Ref, _, _, _} ->
            Pid ! Event,
            {Pid, done}
    end.

%% Test that erl_tracer modules work correctly
erl_tracer(Config) ->
    stop(),

    ok = load_nif(Config),

    Self = self(),
    {ok, _} = dbg:tracer(module, {?MODULE, Self}),
    {ok, {?MODULE, Self}} = dbg:get_tracer(),
    {ok, _} = dbg:p(self(), [c, timestamp]),
    {ok, _} = dbg:tp(?MODULE, dummy, []),
    ok = ?MODULE:dummy(),
    [{Self, call, Self, Self, {?MODULE, dummy, []}, #{}}] = flush(),
    ok.

%% Test that distributed erl_tracer modules work
distributed_erl_tracer(Config) ->
    stop(),

    S = self(),

    ok = load_nif(Config),

    LNode = node(),
    RNode = start_slave(),
    true = rpc:call(RNode, code, add_patha, [filename:join(proplists:get_value(data_dir, Config), "..")]),
    ok = rpc:call(RNode, ?MODULE, load_nif, [Config]),

    NifProxy = fun() ->
                       register(nif_proxy, self()),
                       receive M -> S ! M end
               end,

    LNifProxy = spawn_link(LNode, NifProxy),
    RNifProxy = spawn_link(RNode, NifProxy),

    TracerFun = fun() -> {?MODULE, whereis(nif_proxy)} end,

    {ok, _} = dbg:tracer(LNode, module, TracerFun),
    {ok, _} = dbg:tracer(RNode, module, TracerFun),

    {ok, [{matched, _, _}, {matched, _, _}]} = dbg:p(all,c),
    {ok, [_, _]} = dbg:tp(?MODULE, dummy, []),

    {ok, {?MODULE, LNifProxy}} = dbg:get_tracer(LNode),
    {ok, {?MODULE, RNifProxy}} = dbg:get_tracer(RNode),

    LCall = spawn_link(LNode, fun() -> ?MODULE:dummy() end),
    [{LCall, call, LNifProxy, LCall, {?MODULE, dummy, []}, #{}}] = flush(),

    RCall = spawn_link(RNode, fun() -> ?MODULE:dummy() end),
    [{RCall, call, RNifProxy, RCall, {?MODULE, dummy, []}, #{}}] = flush(),


    ok.

load_nif(Config) ->
    SoFile = atom_to_list(?MODULE),
    DataDir = proplists:get_value(data_dir, Config),
    case erlang:load_nif(filename:join(DataDir, SoFile) , 0) of
        {error, {reload, _}} ->
            ok;
        ok ->
            ok
    end.

enabled(_, _, _) ->
    erlang:nif_error(nif_not_loaded).

trace(_, _, _, _, _) ->
    erlang:nif_error(nif_not_loaded).

%%
%% Support functions
%%

start_slave() ->
    {A, B, C} = now(),
    Name = "asdkxlkmd" ++ integer_to_list(A+B+C),
    {ok, Node} = test_server:start_node(Name,slave,[]),
    ok = wait_node(Node, 15),
    Node.

stop_slave(Node) ->
    test_server:stop_node(Node).

wait_node(_,0) ->
    no;
wait_node(Node, N) ->
    case net_adm:ping(Node) of
        pong ->
            ok;
        pang ->
            receive
            after 1000 ->
                      ok
            end,
            wait_node(Node, N - 1)
    end.

myhandler(Message, {wait_for_go,Pid}) ->
    receive
        {go,Pid} ->
            myhandler(Message, Pid)
    end;
myhandler(Message, Relay) ->
    Relay ! Message,
    case Message of
        end_of_trace ->
            ok;
        _ ->
            Relay
    end.

flush() ->
    flush([]).
flush(Acc) ->
    receive
        X ->
            flush(Acc ++ [X])
    after 1000 ->
              Acc
    end.

start() ->
    stop(),
    dbg:tracer(process, {fun myhandler/2, self()}).

stop() ->
    dbg:stop().



schedstat_handler(TraceMsg, {Parent, Tag, Data} = State) ->
    case TraceMsg of
        {trace_ts, Pid, in, _, Ts} ->
            NewData =
            case lists:keysearch(Pid, 1, Data) of
                {value, {Pid, Acc}} ->
                    [{Pid, Acc, Ts} | lists:keydelete(Pid, 1, Data)];
                false ->
                    [{Pid, {0, 0, 0}, Ts} | Data];
                Other ->
                    exit(Parent, {?MODULE, ?LINE, Other}),
                    erlang:display({?MODULE, ?LINE, Other}),
                    Data
            end,
            {Parent, Tag, NewData};
        {trace_ts, Pid, out, _, {A3, B3, C3}} ->
            NewData =
            case lists:keysearch(Pid, 1, Data) of
                {value, {Pid, {A1, B1, C1}, {A2, B2, C2}}} ->
                    [{Pid, {A3-A2+A1, B3-B2+B1, C3-C2+C1}} |
                     lists:keydelete(Pid, 1, Data)];
                Other ->
                    exit(Parent, {?MODULE, ?LINE, Other}),
                    erlang:display({?MODULE, ?LINE, Other}),
                    Data
            end,
            {Parent, Tag, NewData};
        {trace_ts,_Pid,spawned,_OtherPid,_,_Ts} ->
            State;
        {trace_ts,_Pid,getting_linked,_OtherPid,_Ts} ->
            State;
        {trace_ts, Pid, exit, normal, {A3, B3, C3}} ->
            NewData =
            case lists:keysearch(Pid, 1, Data) of
                {value, {Pid, {A1, B1, C1}, {A2, B2, C2}}} ->
                    [{Pid, {A3-A2+A1, B3-B2+B1, C3-C2+C1}} |
                     lists:keydelete(Pid, 1, Data)];
                {value, {Pid, _Acc}} ->
                    Data;
                false ->
                    [{Pid, {0, 0, 0}} | Data];
                Other ->
                    exit(Parent, {?MODULE, ?LINE, Other}),
                    erlang:display({?MODULE, ?LINE, Other}),
                    Data
            end,
            {Parent, Tag, NewData};
        {trace_ts, _Pid, send, _Msg, _OtherPid, _Ts} ->
            State;
        end_of_trace ->
            Parent ! {Tag, Data},
            State
    end.



pass_token(Token, Next, Loops) ->
    receive
        {Token, 1} = Msg ->
            sendloop(Loops),
            Next ! Msg;
        {Token, _Cnt} = Msg->
            sendloop(Loops),
            Next ! Msg,
            pass_token(Token, Next, Loops)
    end.

pass_token(Token, Final, Cnt, Loops) ->
    receive
        {Token, start, Next} ->
            sendloop(Loops),
            Msg = {Token, Cnt},
            Next ! Msg,
            pass_token(Token, Final, Next, Cnt, Loops)
    end.

pass_token(Token, Final, Next, Cnt, Loops) ->
    receive
        {Token, 1} ->
            sendloop(Loops),
            Msg = {Token, done},
            Final ! Msg;
        {Token, Cnt} ->
            sendloop(Loops),
            NextCnt = Cnt-1,
            Msg = {Token, NextCnt},
            Next ! Msg,
            pass_token(Token, Final, Next, NextCnt, Loops)
    end.

sendloop(Loops) ->
    sendloop(make_ref(), Loops).

sendloop(_Tag, 0) ->
    ok;
sendloop(Tag, Loops) ->
    self() ! {Tag, Loops},
    receive {Tag, Loops} -> ok end,
    sendloop(Tag, Loops-1).

token_volleyball(N, Cnt, Loops)
  when is_integer(N), N >= 1, is_integer(Cnt), Cnt >= 1,
       is_integer(Loops), Loops >= 0 ->
    Self = self(),
    Token = make_ref(),
    Last = spawn_link(fun() -> pass_token(Token, Self, Cnt, Loops) end),
    First = token_volleyball(Token, Last, N-1, Loops),
    Last ! {Token, start, First},
    receive {Token, done} -> ok end.

token_volleyball(Token, Next, 1, Loops) ->
    spawn_link(fun() -> pass_token(Token, Next, Loops) end);
token_volleyball(Token, Next, N, Loops) ->
    Pid = spawn_link(fun() -> pass_token(Token, Next, Loops) end),
    token_volleyball(Token, Pid, N-1, Loops).
