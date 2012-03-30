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

%%%-------------------------------------------------------------------
%%% File    : ethread_SUITE.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : 
%%%
%%% Created : 17 Jun 2004 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
-module(ethread_SUITE).
-author('rickard.s.green@ericsson.com').

%-define(line_trace, 1).

-define(DEFAULT_TIMEOUT, ?t:minutes(10)).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2]).

-export([create_join_thread/1,
	 equal_tids/1,
	 mutex/1,
	 try_lock_mutex/1,
	 cond_wait/1,
	 broadcast/1,
	 detached_thread/1,
	 max_threads/1,
	 tsd/1,
	 spinlock/1,
	 rwspinlock/1,
	 rwmutex/1,
	 atomic/1,
	 dw_atomic_massage/1]).

-include_lib("test_server/include/test_server.hrl").

tests() ->
    [create_join_thread,
     equal_tids,
     mutex,
     try_lock_mutex,
     cond_wait,
     broadcast,
     detached_thread,
     max_threads,
     tsd,
     spinlock,
     rwspinlock,
     rwmutex,
     atomic,
     dw_atomic_massage].

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    tests().

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

%%
%%
%% The test-cases
%%
%%

create_join_thread(doc) ->
    ["Tests ethr_thr_create and ethr_thr_join."];
create_join_thread(suite) ->
    [];
create_join_thread(Config) ->
    run_case(Config, "create_join_thread", "").

equal_tids(doc) ->
    ["Tests ethr_equal_tids."];
equal_tids(suite) ->
    [];
equal_tids(Config) ->
    run_case(Config, "equal_tids", "").

mutex(doc) ->
    ["Tests mutexes."];
mutex(suite) ->
    [];
mutex(Config) ->
    run_case(Config, "mutex", "").

try_lock_mutex(doc) ->
    ["Tests try lock on mutex."];
try_lock_mutex(suite) ->
    [];
try_lock_mutex(Config) ->
    run_case(Config, "try_lock_mutex", "").

%% Remove dead code?

% wd_dispatch(P) ->
%     receive
% 	bye ->
% 	    ?line true = port_command(P, "-1 "),
% 	    ?line bye;
% 	L when is_list(L) ->
% 	    ?line true = port_command(P, L),
% 	    ?line wd_dispatch(P)
%     end.
% 
% watchdog(Port) ->
%     ?line process_flag(priority, max),
%     ?line receive after 500 -> ok end,
% 
%     ?line random:seed(),
%     ?line true = port_command(Port, "0 "),
%     ?line lists:foreach(fun (T) ->
% 				erlang:send_after(T,
% 						  self(),
% 						  integer_to_list(T)
% 						  ++ " ")
% 			end,
% 			lists:usort(lists:map(fun (_) ->
% 						      random:uniform(4500)+500
% 					      end,
% 					      lists:duplicate(50,0)))),
%     ?line erlang:send_after(5100, self(), bye),
% 
%     wd_dispatch(Port).

cond_wait(doc) ->
    ["Tests ethr_cond_wait with ethr_cond_signal and ethr_cond_broadcast."];
cond_wait(suite) ->
    [];
cond_wait(Config) ->
    run_case(Config, "cond_wait", "").

broadcast(doc) ->
    ["Tests that a ethr_cond_broadcast really wakes up all waiting threads"];
broadcast(suite) ->
    [];
broadcast(Config) ->
    run_case(Config, "broadcast", "").

detached_thread(doc) ->
    ["Tests detached threads."];
detached_thread(suite) ->
    [];
detached_thread(Config) ->
    case {os:type(), os:version()} of
	{{unix,darwin}, {9, _, _}} ->
	    %% For some reason pthread_create() crashes when more
	    %% threads cannot be created, instead of returning an
	    %% error code on our MacOS X Leopard machine...
	    {skipped, "MacOS X Leopard cannot cope with this test..."};
	_ ->
	    run_case(Config, "detached_thread", "")
    end.

max_threads(doc) ->
    ["Tests maximum number of threads."];
max_threads(suite) ->
    [];
max_threads(Config) ->
    case {os:type(), os:version()} of
	{{unix,darwin}, {9, _, _}} ->
	    %% For some reason pthread_create() crashes when more
	    %% threads cannot be created, instead of returning an
	    %% error code on our MacOS X Leopard machine...
	    {skipped, "MacOS X Leopard cannot cope with this test..."};
	_ ->
	    run_case(Config, "max_threads", "")
    end.

tsd(doc) ->
    ["Tests thread specific data."];
tsd(suite) ->
    [];
tsd(Config) ->
    run_case(Config, "tsd", "").

spinlock(doc) ->
    ["Tests spinlocks."];
spinlock(suite) ->
    [];
spinlock(Config) ->
    run_case(Config, "spinlock", "").

rwspinlock(doc) ->
    ["Tests rwspinlocks."];
rwspinlock(suite) ->
    [];
rwspinlock(Config) ->
    run_case(Config, "rwspinlock", "").

rwmutex(doc) ->
    ["Tests rwmutexes."];
rwmutex(suite) ->
    [];
rwmutex(Config) ->
    run_case(Config, "rwmutex", "").

atomic(doc) ->
    ["Tests atomics."];
atomic(suite) ->
    [];
atomic(Config) ->
    run_case(Config, "atomic", "").

dw_atomic_massage(doc) ->
    ["Massage double word atomics"];
dw_atomic_massage(suite) ->
    [];
dw_atomic_massage(Config) ->
    run_case(Config, "dw_atomic_massage", "").

%%
%%
%% Auxiliary functions
%%
%%

init_per_testcase(Case, Config) ->
    case inet:gethostname() of
	{ok,"fenris"} when Case == max_threads ->
	    %% Cannot use os:type+os:version as not all
	    %% solaris10 machines are buggy.
	    {skip, "This machine is buggy"};
	_Else ->
	    Dog = ?t:timetrap(?DEFAULT_TIMEOUT),
	    [{watchdog, Dog}|Config]
    end.

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

-define(TESTPROG, "ethread_tests").
-define(FAILED_MARKER, $E,$T,$H,$R,$-,$T,$E,$S,$T,$-,$F,$A,$I,$L,$U,$R,$E).
-define(SKIPPED_MARKER, $E,$T,$H,$R,$-,$T,$E,$S,$T,$-,$S,$K,$I,$P).
-define(SUCCESS_MARKER, $E,$T,$H,$R,$-,$T,$E,$S,$T,$-,$S,$U,$C,$C,$E,$S,$S).
-define(PID_MARKER, $E,$T,$H,$R,$-,$T,$E,$S,$T,$-,$P,$I,$D).

port_prog_killer(EProc, OSProc) when is_pid(EProc), is_list(OSProc) ->
    ?line process_flag(trap_exit, true),
    ?line Ref = erlang:monitor(process, EProc),
    ?line receive
	      {'DOWN', Ref, _, _, Reason} when is_tuple(Reason),
					       element(1, Reason)
					       == timetrap_timeout ->
		  ?line Cmd = "kill -9 " ++ OSProc,
		  ?line ?t:format("Test case timed out. "
				  "Trying to kill port program.~n"
				  "  Executing: ~p~n", [Cmd]),
		  ?line case os:cmd(Cmd) of
			    [] ->
				ok;
			    OsCmdRes ->
				?line ?t:format("             ~s", [OsCmdRes])
			end;
	      {'DOWN', Ref, _, _, _} ->
		  %% OSProc is assumed to have terminated by itself
		  ?line ok 
	  end.

get_line(_Port, eol, Data) ->
    ?line Data;
get_line(Port, noeol, Data) ->
    ?line receive
	      {Port, {data, {Flag, NextData}}} ->
		  ?line get_line(Port, Flag, Data ++ NextData);
	      {Port, eof} ->
		  ?line ?t:fail(port_prog_unexpectedly_closed)
	  end.

read_case_data(Port, TestCase) ->
    ?line receive
	      {Port, {data, {eol, [?SUCCESS_MARKER]}}} ->
		  ?line ok;
	      {Port, {data, {Flag, [?SUCCESS_MARKER | CommentStart]}}} ->
		  ?line {comment, get_line(Port, Flag, CommentStart)};
	      {Port, {data, {Flag, [?SKIPPED_MARKER | CommentStart]}}} ->
		  ?line {skipped, get_line(Port, Flag, CommentStart)};
	      {Port, {data, {Flag, [?FAILED_MARKER | ReasonStart]}}} ->
		  ?line ?t:fail(get_line(Port, Flag, ReasonStart));
	      {Port, {data, {eol, [?PID_MARKER | PidStr]}}} ->
		  ?line ?t:format("Port program pid: ~s~n", [PidStr]),
		  ?line CaseProc = self(),
		  ?line _ = list_to_integer(PidStr), % Sanity check
		  spawn_opt(fun () ->
				    port_prog_killer(CaseProc, PidStr)
			    end,
			    [{priority, max}, link]),
		  read_case_data(Port, TestCase);
	      {Port, {data, {Flag, LineStart}}} ->
		  ?line ?t:format("~s~n", [get_line(Port, Flag, LineStart)]),
		  read_case_data(Port, TestCase);
	      {Port, eof} ->
		  ?line ?t:fail(port_prog_unexpectedly_closed)
	  end.

run_case(Config, Test, TestArgs) ->
    run_case(Config, Test, TestArgs, fun (_Port) -> ok end).

run_case(Config, Test, TestArgs, Fun) ->
    TestProg = filename:join([?config(data_dir, Config), ?TESTPROG]),
    Cmd = TestProg ++ " " ++ Test ++ " " ++ TestArgs,
    case catch open_port({spawn, Cmd}, [stream,
					use_stdio,
					stderr_to_stdout,
					eof,
					{line, 1024}]) of
	Port when is_port(Port) ->
	    ?line Fun(Port),
	    ?line CaseResult = read_case_data(Port, Test),
	    ?line receive
		      {Port, eof} ->
			  ?line ok
		  end,
	    ?line CaseResult;
	Error ->
	    ?line ?t:fail({open_port_failed, Error})
    end.
	    



