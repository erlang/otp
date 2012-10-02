%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2012. All Rights Reserved.
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
-module(dbg_SUITE).

%% Test functions
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 big/1, tiny/1, simple/1, message/1, distributed/1,
	 ip_port/1, file_port/1, file_port2/1, file_port_schedfix/1,
	 ip_port_busy/1, wrap_port/1, wrap_port_time/1,
	 with_seq_trace/1, dead_suspend/1, local_trace/1,
	 saved_patterns/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([tracee1/1, tracee2/1]).
-export([dummy/0, exported/1]).

-include_lib("test_server/include/test_server.hrl").
-define(default_timeout, ?t:minutes(1)).

init_per_testcase(_Case, Config) ->
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].
end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [big, tiny, simple, message, distributed, ip_port,
     file_port, file_port2, file_port_schedfix, ip_port_busy,
     wrap_port, wrap_port_time, with_seq_trace, dead_suspend,
     local_trace, saved_patterns].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


big(suite) -> [];
big(doc) -> ["Rudimentary interface test"];
big(Config) when is_list(Config) ->
	?line {ok,OldCurDir} = file:get_cwd(),
	Datadir=?config(data_dir, Config),
	Privdir=?config(priv_dir, Config),
	?line ok=file:set_cwd(Privdir),
    try
	%% make sure dbg is stopped (and returns correctly)
	?line ok = dbg:stop(),

	%% compile test module and make sure it is loaded.
	?line {ok,Mod} = compile:file(Datadir++"/dbg_test",[trace]),
	?line code:purge(dbg_test),
	?line {module, Mod}=code:load_file(dbg_test),

	%% run/debug a named test function.
	?line Pid = spawn_link(dbg_test, loop, [Config]),
	?line true = register(dbg_test_loop, Pid),
	?line {ok,_} = dbg:tracer(),
	?line {ok,[{matched, _node, 1}]} = dbg:p(dbg_test_loop, [m,p,c]),
	?line ok = dbg:c(dbg_test, test, [Config]),
	?line ok = dbg:i(),
	?line dbg_test_loop ! {dbg_test, stop},
	unregister(dbg_test_loop),
	?line ok = dbg:stop(),

	%% run/debug a Pid.
	?line Pid2=spawn_link(dbg_test,loop,[Config]),
	?line {ok,_} = dbg:tracer(),
	?line {ok,[{matched, _node, 1}]} = dbg:p(Pid2,[s,r,p]),
	?line ok = dbg:c(dbg_test, test, [Config]),
	?line ok = dbg:i(),
	?line Pid2 ! {dbg_test, stop},

	?line ok=file:set_cwd(OldCurDir)
    after
	?line dbg:stop()
    end,
    ok.


tiny(suite) -> [];
tiny(doc) -> ["Rudimentary interface test"];
tiny(Config) when is_list(Config) ->
    ?line {ok,OldCurDir} = file:get_cwd(),
    Datadir=?config(data_dir, Config),
    Privdir=?config(priv_dir, Config),
    ?line ok=file:set_cwd(Privdir),
    try
	%% compile test module and make sure it is loaded.
	?line {ok, Mod} = compile:file(Datadir++"/dbg_test",[trace]),
	?line code:purge(dbg_test),
	?line {module, Mod}=code:load_file(dbg_test),

	?line Pid=spawn_link(dbg_test,loop,[Config]),
	if
	    is_pid(Pid) ->
		?line dbg:tracer(),
		?line {ok,[{matched, _node, 1}]} = dbg:p(Pid,[s,r,m,p,c]),
		?line ok = dbg:c(dbg_test,test,[Config]),
		?line ok = dbg:i(),
		?line Pid ! {dbg_test, stop};
	    true ->
		?line ok=file:set_cwd(OldCurDir),
		?t:fail("Could not spawn external test process.~n"),
		failure
	end
    after
	?line ok = dbg:stop(),
        ?line ok = file:set_cwd(OldCurDir)
    end,
    ok.

simple(suite) ->
    [];
simple(doc) ->
    ["Simple interface test with own handler"];
simple(Config) when is_list(Config) ->
    try
	?line start(),
	?line dbg:p(self(),call),
	?line dbg:tp(dbg,ltp,[]),
	?line dbg:ltp(),
	?line stop(),
	?line S = self(),
	?line [{trace,S,call,{dbg,ltp,[]}}] = flush()
    after
	?line dbg:stop()
    end,
    ok.

message(suite) ->
    [];
message(doc) ->
    ["Simple interface test with pam code that appends a message"];
message(Config) when is_list(Config) ->
    ?line {ok, _} = start(),
    try
	?line {ok, [{matched, _node, 1}]} = dbg:p(self(),call),
	?line {ok, X} = dbg:tp(dbg,ltp,[{'_',[],[{message, {self}}]}]),
	?line {value, {saved, Saved}} = lists:keysearch(saved, 1, X),
	?line {ok, Y} = dbg:tp(dbg,ln,Saved),
	?line {value, {saved, Saved}} = lists:keysearch(saved, 1, Y),
	?line ok = dbg:ltp(),
	?line ok = dbg:ln()
    after
	?line stop()
    end,
    ?line S = self(),
    ?line [{trace,S,call,{dbg,ltp,[]},S},
	   {trace,S,call,{dbg,ln,[]},S}] = flush(),
    ok.

distributed(suite) ->
    [];
distributed(doc) ->
    ["Simple test of distributed tracing"];
distributed(Config) when is_list(Config) ->
    ?line {ok, _} = start(),
    ?line Node = start_slave(),
    try
	?line RexPid = rpc:call(Node, erlang, whereis, [rex]),
	?line RexPidList = pid_to_list(RexPid),
	?line {ok, Node} = dbg:n(Node),
	?line {ok, X} = dbg:p(all,call),
	?line {value, {matched, Node, _}} = lists:keysearch(Node, 2, X),
	?line {ok, Y} = dbg:p(RexPidList, s),
	?line {value, {matched, Node, 1}} = lists:keysearch(Node, 2, Y),
	?line {ok, Z} = dbg:tp(dbg,ltp,[]),
	?line {value, {matched, Node, 1}} = lists:keysearch(Node, 2, Z),
	?line dbg:cn(Node),
	?line dbg:tp(dbg,ln,[]),
	?line ok = rpc:call(Node, dbg, ltp, []),
	?line ok = rpc:call(Node, dbg, ln, []),
	?line ok = dbg:ln(),
	?line S = self(),
	?line {TraceSend, TraceCall} =
	lists:partition(fun ({trace,RP,send,_,_}) when RP =:= RexPid -> true;
			    (_) -> false end,
			flush()),
	?line [_|_] = TraceSend,
	?line [{trace,Pid,call,{dbg,ltp,[]}},
	       {trace,S,call,{dbg,ln,[]}}] = TraceCall,
	?line Node = node(Pid),
	%%
	?line stop()
    after
	?line stop_slave(Node),
        ?line stop()
    end,
    ok.


local_trace(suite) ->
    [];
local_trace(doc) ->
    ["Tests tracing of local function calls."];
local_trace(Config) when is_list(Config) ->
    ?line {ok, _} = start(),
    try
	?line S = self(),
	?line %% Split "<X.Y.Z>" into {X, Y, Z}
	?line "<"++L1 = L = pid_to_list(S),
	?line NoDot = fun ($.) -> false; (_) -> true end,
	?line {LX,"."++L2} = lists:splitwith(NoDot, L1),
	?line {LY,"."++L3} = lists:splitwith(NoDot, L2),
	?line ">"++L4 = lists:reverse(L3),
	?line LZ = lists:reverse(L4),
	?line X = 0 = list_to_integer(LX),
	?line Y = list_to_integer(LY),
	?line Z = list_to_integer(LZ),
	?line XYZ = {X, Y, Z},
	?line io:format("Self = ~w = ~w~n", [S,XYZ]),
	?line {ok, [{matched, _node, 1}]} = dbg:p(S,call),
	?line {ok, [{matched, _node, 1}]} = dbg:p(XYZ,call),
	if Z =:= 0 ->
		?line {ok, [{matched, _node, 1}]} = dbg:p(Y,call);
	   true -> ok
	end,
	?line {ok, [{matched, _node, 1}]} = dbg:p(L,call),
	?line {ok, _} = dbg:tpl(?MODULE,not_exported,[]),
	?line 4 = not_exported(2),
	?line [{trace,S,call,{?MODULE,not_exported,[2]}}] = flush(),
	?line {ok, _} = dbg:tp(?MODULE,exported,[]),
	?line 4 = ?MODULE:exported(2),
	?line [{trace,S,call,{?MODULE,exported,[2]}},
	       {trace,S,call,{?MODULE,not_exported,[2]}}] = flush(),
	?line {ok, _} = dbg:ctpl(?MODULE),
	?line 4 = ?MODULE:exported(2),
	?line [{trace,S,call,{?MODULE,exported,[2]}}] = flush(),
	?line {ok, _} = dbg:tpl(?MODULE,not_exported,[]),
	?line {ok, _} = dbg:ctp(?MODULE),
	?line 4 = ?MODULE:exported(2),
	?line [] = flush(),
	?line {ok, _} = dbg:tpl(?MODULE,not_exported,x),
	?line catch ?MODULE:exported(x),
	?line [{trace,S,call,{dbg_SUITE,not_exported,[x]}},
	       {trace,S,exception_from,
		{dbg_SUITE,not_exported,1},
		{error,badarith}}] = flush()
    after
	?line stop()
    end,
    ok.

saved_patterns(suite) ->
    [];
saved_patterns(doc) ->
    ["Tests saving of match_spec's."];
saved_patterns(Config) when is_list(Config) ->
    ?line dbg:stop(),
    ?line {ok,[{saved,1}]} =
	dbg:tp(dbg,ctp,1,[{'_',[],[{message, blahonga}]}]),
    ?line {ok,[{saved,2}]} =
	dbg:tp(dbg,ctp,1,[{['_'],[],[{message, blahonga}]}]),
    ?line Privdir=?config(priv_dir, Config),
    ?line file:make_dir(Privdir),
    ?line File = filename:join([Privdir, "blahonga.ms"]),
    ?line dbg:wtp(File),
    ?line dbg:stop(),
    ?line dbg:ctp('_','_','_'),
    ?line {ok, _} = start(),
    try
	?line dbg:rtp(File),
	?line {ok,[{matched,_node,1},{saved,1}]} = dbg:tp(dbg,ltp,0,1),
	?line {ok, [{matched, _node, 1}]} = dbg:p(self(),call),
	?line dbg:ltp(),
	?line S = self(),
	?line [{trace,S,call,{dbg,ltp,[]},blahonga}] = flush()
    after
	?line stop()
    end,
    ok.



not_exported(N) ->
    N * 2.

exported(N) ->
    not_exported(N).

ip_port(suite) ->
    [];
ip_port(doc) ->
    ["Test tracing to IP port"];
ip_port(Config) when is_list(Config) ->
    ?line stop(),
    ?line Port = dbg:trace_port(ip, 0),
    ?line {ok, _} = dbg:tracer(port, Port),
    try
	?line {ok, [{matched, _node, 1}]} = dbg:p(self(),call),
	?line {ok, X} = dbg:tp(dbg, ltp,[{'_',[],[{message, {self}}]}]),
	?line {value, {saved, _Saved}} = lists:keysearch(saved, 1, X),
	?line {ok, Y} = dbg:tp(dbg, ln, [{'_',[],[{message, hej}]}]),
	?line {value, {saved, _}} = lists:keysearch(saved, 1, Y),
	?line ok = dbg:ltp(),
	?line ok = dbg:ln(),
	?line {ok, IpPort} = dbg:trace_port_control(get_listen_port),
	?line io:format("IpPort = ~p~n", [IpPort]),
	?line dbg:trace_client(ip, IpPort, {fun myhandler/2, self()}),
	?line S = self(),
	?line [{trace,S,call,{dbg,ltp,[]},S},
	       {trace,S,call,{dbg,ln,[]},hej}] = flush()
    after
	?line stop()
    end,
    ok.



ip_port_busy(suite) ->
    [];
ip_port_busy(doc) ->
    ["Test that the dbg server does not hang if the tracer don't start ",
     "(OTP-3592)"];
ip_port_busy(Config) when is_list(Config) ->
    ?line stop(),
    ?line Tracer = dbg:trace_port(ip, 4745),
    ?line Port = Tracer(),
    ?line {error, Reason} = dbg:tracer(port, Tracer),
    try
	?line io:format("Error reason = ~p~n", [Reason]),
	?line true = port_close(Port)
    after
	?line dbg:stop()
    end,
    ?line ok.



file_port(suite) ->
    [];
file_port(doc) ->
    ["Test tracing to file port (simple)"];
file_port(Config) when is_list(Config) ->
    ?line stop(),
    ?line {A,B,C} = erlang:now(),
    ?line FTMP =  atom_to_list(?MODULE) ++ integer_to_list(A) ++ "-" ++
	integer_to_list(B) ++ "-" ++ integer_to_list(C),
    ?line FName = filename:join([?config(data_dir, Config), FTMP]),
    ?line Port = dbg:trace_port(file, FName),
    ?line {ok, _} = dbg:tracer(port, Port),
    try
	?line {ok, [{matched, _node, 1}]} = dbg:p(self(),call),
	?line {ok, X} = dbg:tp(dbg, ltp,[{'_',[],[{message, {self}}]}]),
	?line {value, {saved, _Saved}} = lists:keysearch(saved, 1, X),
	?line {ok, Y} = dbg:tp(dbg, ln, [{'_',[],[{message, hej}]}]),
	?line {value, {saved, _}} = lists:keysearch(saved, 1, Y),
	?line ok = dbg:ltp(),
	?line ok = dbg:ln(),
	?line stop(),
	?line dbg:trace_client(file, FName, {fun myhandler/2, self()}),
	?line S = self(),
	?line [{trace,S,call,{dbg,ltp,[]},S},
	       {trace,S,call,{dbg,ln,[]},hej},
	       end_of_trace] = flush()
    after
	?line stop(),
        ?line file:delete(FName)
    end,
    ok.

file_port2(suite) ->
    [];
file_port2(doc) ->
    ["Test tracing to file port with 'follow_file'"];
file_port2(Config) when is_list(Config) ->
    stop(),
    {A,B,C} = erlang:now(),
    FTMP =  atom_to_list(?MODULE) ++ integer_to_list(A) ++
	"-" ++ integer_to_list(B) ++ "-" ++ integer_to_list(C),
    FName = filename:join([?config(data_dir, Config), FTMP]),
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

file_port_schedfix(suite) ->
    [];
file_port_schedfix(doc) ->
    ["Test that the scheduling timestamp fix for trace flag 'running' works."];
file_port_schedfix(Config) when is_list(Config) ->
    ?line case (catch erlang:system_info(smp_support)) of
	      true ->
		  {skip, "No schedule fix on SMP"};
	      _ ->
		  try
		      file_port_schedfix1(Config)
		  after
		      dbg:stop()
		  end
	  end.
file_port_schedfix1(Config) when is_list(Config) ->
    ?line stop(),
    ?line {A,B,C} = erlang:now(),
    ?line FTMP =  atom_to_list(?MODULE) ++ integer_to_list(A) ++
	"-" ++ integer_to_list(B) ++ "-" ++ integer_to_list(C),
    ?line FName = filename:join([?config(data_dir, Config), FTMP]),
    %%
    ?line Port = dbg:trace_port(file, {FName, wrap, ".wraplog", 8*1024, 4}),
    ?line {ok, _} = dbg:tracer(port, Port),
    ?line {ok,[{matched,_node,0}]} = dbg:p(new,[running,procs,send,timestamp]),
    %%
    %% Generate the trace data
    %%
    %% This starts 3 processes that sends a message to each other in a ring,
    %% 4 laps. Prior to sending the message to the next in the ring, each
    %% process send 8 messages to itself, just to generate some trace data,
    %% and to lower the possibility that the trace log wraps just after
    %% a schedule out message (which would not burden any process and hence
    %% not show up in the result)
    %%
    %% The wrap file trace is used because it burns a lot of time when the
    %% driver swaps files, a lot more than the regular file trace. The test
    %% case is dimensioned so that the log fills two files and just starts
    %% on the third (out of four wrap files). This gives two file swaps,
    %% and there are three processes, so one process will NOT be burdened.
    %% The criterion for trace success is then that the max process
    %% execution time must not be more than twice the min process
    %% execution time. Wallclock. A normal result is about 10 times more
    %% without schedule in - schedule out compensation (OTP-3938).
    %%
    ?line ok = token_volleyball(3, 4, 8),
    %%
    ?line {ok,[{matched,_,_}]} = dbg:p(all, [clear]),
    ?line stop(),
    % Some debug code to run on all platforms, for finding the fault on genny
    % Dont touch please /PaN
    ?line io:format("Trace dump by PaN BEGIN~n"),
    ?line dbg:trace_client(file,{FName, wrap, ".wraplog"},{fun(end_of_trace,Pid)-> Pid ! done; (Mesg,Pid) -> io:format("~w~n",[Mesg]),Pid end,self()}),
    receive done -> ok end,
    ?line io:format("Trace dump by PaN END~n"),
      %%
    %% Get the trace result
    %%
    ?line Tag = make_ref(),
    ?line dbg:trace_client(file, {FName, wrap, ".wraplog"},
			   {fun schedstat_handler/2, {self(), Tag, []}}),
    ?line Result =
	receive
	    {Tag, D} ->
		lists:map(
		  fun({Pid, {A1, B1, C1}}) ->
			  {Pid, C1/1000000 + B1 + A1*1000000}
		  end,
		  D)
	end,
    ?line ok = io:format("Result=~p", [Result]),
%    erlang:display({?MODULE, ?LINE, Result}),
    %%
    %% Analyze the result
    %%
    ?line {Min, Max} =
	lists:foldl(
	  fun({_Pid, M}, {Mi, Ma}) ->
		  {if M < Mi -> M; true -> Mi end,
		   if M > Ma -> M; true -> Ma end}
	  end,
	  {void, 0},
	  Result),
    % More PaN debug
    ?line io:format("Min = ~f, Max = ~f~n",[Min,Max]),
    %%
    %% Cleanup
    %%
    ?line ToBeDeleted = filelib:wildcard(FName++"*"++".wraplog"),
    ?line lists:map(fun file:delete/1, ToBeDeleted),
%    io:format("ToBeDeleted=~p", [ToBeDeleted]),
    %%
    %% Present the result
    %%
    P = (Max / Min - 1) * 100,
    BottomLine = lists:flatten(io_lib:format("~.2f %", [P])),
    if P > 100 ->
	    Reason = {BottomLine, '>', "100%"},
	    erlang:display({file_port_schedfix, fail, Reason}),
	    test_server:fail(Reason);
       true ->
	    {comment, BottomLine}
    end.

wrap_port(suite) ->
    [];
wrap_port(doc) ->
    ["Test tracing to wrapping file port"];
wrap_port(Config) when is_list(Config) ->
    ?line Self = self(),
    ?line stop(),
    ?line {A,B,C} = erlang:now(),
    ?line FTMP =  atom_to_list(?MODULE) ++ integer_to_list(A) ++ "-" ++
	integer_to_list(B) ++ "-" ++ integer_to_list(C) ++ "-",
    ?line FName = filename:join([?config(data_dir, Config), FTMP]),
    ?line FNameWildcard = FName++"*"++".trace",
    %% WrapSize=0 and WrapCnt=11 will force the trace to wrap after
    %% every trace message, and to contain only the last 10 entries
    %% after trace stop since the last file will be empty waiting
    %% for its first trace message.
    ?line WrapSize = 0,
    ?line WrapCnt = 11,
    ?line WrapFilesSpec = {FName, wrap, ".trace", WrapSize, WrapCnt},
    ?line wrap_port_init(WrapFilesSpec),
    %% The number of iterations, N, is tested to place wrap the log,
    %% giving a gap in the filename sequence at index 3.
    %% This should be a difficult case for
    %% the trace_client file sorting functionality.
    N = 7,
    ?line lists:foreach(
	    fun(Cnt) ->
		    ?MODULE:tracee1(Cnt),
		    ?MODULE:tracee2(Cnt)
	    end,
	    lists:seq(1, N)),
    ?line stop(),
    try
	?line Files1 = filelib:wildcard(FNameWildcard),
	?line io:format("~p~n", [Files1]),
	?line Tc1 = dbg:trace_client(file, WrapFilesSpec,
				     {fun myhandler/2, {wait_for_go,Self}}),
	?line Tref1 = erlang:monitor(process, Tc1),
	Tc1 ! {go,Self},
	?line [{'DOWN',Tref1,_,_,normal},
	       end_of_trace
	       |Result] = lists:reverse(flush()),
	?line M = N - (WrapCnt-1) div 2,
	?line M = wrap_port_result(Result, Self, N),
	%%
	%% Start a new wrap log with the same name to verify that
	%% all files are cleared at wrap log start. Only produce
	%% two trace messages to also place the gap at index 3,
	%% so the trace log will be misinterpreted.
	%%
	?line wrap_port_init(WrapFilesSpec),
	?line Files2 = filelib:wildcard(FNameWildcard),
	?line io:format("~p~n", [Files2]),
	?line -1 = ?MODULE:tracee1(-1),
	?line -1 = ?MODULE:tracee2(-1),
	?line stop(),
	?line Files = filelib:wildcard(FNameWildcard),
	?line io:format("~p~n", [Files]),
	?line Tc2 = dbg:trace_client(file, WrapFilesSpec,
				     {fun myhandler/2, {wait_for_go,Self}}),
	?line Tref2 = erlang:monitor(process, Tc2),
	Tc2 ! {go,Self},
	?line [{trace,Self,call,{?MODULE,tracee1,[-1]},Self},
	       {trace,Self,call,{?MODULE,tracee2,[-1]},hej},
	       end_of_trace,
	       {'DOWN',Tref2,_,_,normal}] = flush(),
	%%
	?line lists:map(fun(F) -> file:delete(F) end, Files)
    after
	?line stop()
    end,
    ok.

wrap_port_init(WrapFilesSpec) ->
    ?line Port = dbg:trace_port(file, WrapFilesSpec),
    ?line {ok, _} = dbg:tracer(port, Port),
    ?line {ok, [{matched, _node, 1}]} = dbg:p(self(),call),
    ?line {ok, X} = dbg:tp(?MODULE, tracee1,[{'_',[],[{message, {self}}]}]),
    ?line {value, {saved, _Saved}} = lists:keysearch(saved, 1, X),
    ?line {ok, Y} = dbg:tp(?MODULE, tracee2, [{'_',[],[{message, hej}]}]),
    ?line {value, {saved, _}} = lists:keysearch(saved, 1, Y),
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


wrap_port_time(suite) ->
    [];
wrap_port_time(doc) ->
    ["Test tracing to time limited wrapping file port"];
wrap_port_time(Config) when is_list(Config) ->
    ?line stop(),
    ?line {A,B,C} = erlang:now(),
    ?line FTMP =  atom_to_list(?MODULE) ++ integer_to_list(A) ++ "-" ++
	integer_to_list(B) ++ "-" ++ integer_to_list(C) ++ "-",
    ?line FName = filename:join([?config(data_dir, Config), FTMP]),
    %% WrapTime=2 and WrapCnt=4 will force the trace to wrap after
    %% every 2 seconds, and to contain between 3*2 and 4*2 seconds
    %% of trace entries.
    ?line WrapFilesSpec = {FName, wrap, ".trace", {time, 2000}, 4},
    ?line Port = dbg:trace_port(file, WrapFilesSpec),
    ?line {ok, _} = dbg:tracer(port, Port),
    try
	?line {ok, [{matched, _node, 1}]} = dbg:p(self(),call),
	?line {ok, X} = dbg:tp(?MODULE, tracee1,[{'_',[],[{message, {self}}]}]),
	?line {value, {saved, _Saved1}} = lists:keysearch(saved, 1, X),
	?line {ok, Y} = dbg:tp(?MODULE, tracee2, [{'_',[],[{message, hej}]}]),
	?line {value, {saved, _Saved2}} = lists:keysearch(saved, 1, Y),
	%% The delays in the iterations places two trace messages in each
	%% trace file, but the last which is empty waiting for its first
	%% trace message. The number of iterations is chosen so that
	%% one trace file has been wasted, and therefore the first pair
	%% of trace messages.
	?line lists:foreach(
		fun(Cnt) ->
			receive after 1000 -> ok end,
			?MODULE:tracee1(Cnt),
			?MODULE:tracee2(Cnt),
			receive after 1100 -> ok end
		end,
		lists:seq(1, 4)),
	?line stop(),
	?line Files = filelib:wildcard(FName ++ "*" ++ ".trace"),
	?line io:format("~p~n", [Files]),
	?line dbg:trace_client(file, WrapFilesSpec, {fun myhandler/2, self()}),
	?line S = self(),
	?line [{trace, S, call, {?MODULE, tracee1, [2]}, S},
	       {trace, S, call, {?MODULE, tracee2, [2]}, hej},
	       {trace, S, call, {?MODULE, tracee1, [3]}, S},
	       {trace, S, call, {?MODULE, tracee2, [3]}, hej},
	       {trace, S, call, {?MODULE, tracee1, [4]}, S},
	       {trace, S, call, {?MODULE, tracee2, [4]}, hej},
	       end_of_trace] = flush(),
	?line lists:map(fun(F) -> file:delete(F) end, Files)
    after
	?line stop()
    end,
    ok.

with_seq_trace(suite) ->
    [];
with_seq_trace(doc) ->
    ["Test ordinary tracing combined with seq_trace"];
with_seq_trace(Config) when is_list(Config) ->
    try
	?line {ok, Server} = start(),
	?line {ok, Tracer} = dbg:get_tracer(),
	?line {ok, X} = dbg:tp(dbg, get_tracer, [{[],[],
						  [{set_seq_token, send, true}]}]),
	?line {value, {saved, _}} = lists:keysearch(saved, 1, X),
	?line {ok, [{matched, _node, 1}]} = dbg:p(self(),call),
	?line seq_trace:set_system_tracer(Tracer),
	?line dbg:get_tracer(),
	receive
	    after 1 ->
		    ok
	    end,
	?line S = self(),
	?line ThisNode = node(),
	?line [{trace,S,call,{dbg,get_tracer,[]}},
	       {seq_trace,0,{send,_,S,Server,{S,{get_tracer,ThisNode}}}},
	       {seq_trace,0,{send,_,Server,S,{dbg,{ok,Tracer}}}}] =
	flush()
    after
	?line stop()
    end,
    ok.

dead_suspend(suite) ->
    [];
dead_suspend(doc) ->
    ["Test that trace messages concerning a now dead process does "
     "not crash dbg."];

dead_suspend(Config) when is_list(Config) ->
    ?line start(),
    try
	survived = run_dead_suspend()
    after
	?line stop()
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
