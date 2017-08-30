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

%%%-------------------------------------------------------------------
%%% File    : ethread_SUITE.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : 
%%%
%%% Created : 17 Jun 2004 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
-module(ethread_SUITE).
-author('rickard.s.green@ericsson.com').

-export([all/0, suite/0, init_per_testcase/2, end_per_testcase/2]).

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

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 10}}].

all() ->
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

init_per_testcase(Case, Config) ->
    case inet:gethostname() of
	{ok,"fenris"} when Case == max_threads ->
	    %% Cannot use os:type+os:version as not all
	    %% solaris10 machines are buggy.
	    {skip, "This machine is buggy"};
	_Else ->
            Config
    end.

end_per_testcase(_Case, _Config) ->
    ok.

%%
%%
%% The test-cases
%%
%%

%% Tests ethr_thr_create and ethr_thr_join.
create_join_thread(Config) ->
    run_case(Config, "create_join_thread", "").

%% Tests ethr_equal_tids.
equal_tids(Config) ->
    run_case(Config, "equal_tids", "").

%% Tests mutexes.
mutex(Config) ->
    run_case(Config, "mutex", "").

%% Tests try lock on mutex.
try_lock_mutex(Config) ->
    run_case(Config, "try_lock_mutex", "").

%% Tests ethr_cond_wait with ethr_cond_signal and ethr_cond_broadcast.
cond_wait(Config) ->
    run_case(Config, "cond_wait", "").

%% Tests that a ethr_cond_broadcast really wakes up all waiting threads
broadcast(Config) ->
    run_case(Config, "broadcast", "").

%% Tests detached threads.
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

%% Tests maximum number of threads.
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

%% Tests thread specific data.
tsd(Config) ->
    run_case(Config, "tsd", "").

%% Tests spinlocks.
spinlock(Config) ->
    run_case(Config, "spinlock", "").

%% Tests rwspinlocks.
rwspinlock(Config) ->
    run_case(Config, "rwspinlock", "").

%% Tests rwmutexes.
rwmutex(Config) ->
    run_case(Config, "rwmutex", "").

%% Tests atomics.
atomic(Config) ->
    run_case(Config, "atomic", "").

%% Massage double word atomics
dw_atomic_massage(Config) ->
    run_case(Config, "dw_atomic_massage", "").

%%
%%
%% Auxiliary functions
%%
%%

-define(TESTPROG, "ethread_tests").
-define(FAILED_MARKER, $E,$T,$H,$R,$-,$T,$E,$S,$T,$-,$F,$A,$I,$L,$U,$R,$E).
-define(SKIPPED_MARKER, $E,$T,$H,$R,$-,$T,$E,$S,$T,$-,$S,$K,$I,$P).
-define(SUCCESS_MARKER, $E,$T,$H,$R,$-,$T,$E,$S,$T,$-,$S,$U,$C,$C,$E,$S,$S).
-define(PID_MARKER, $E,$T,$H,$R,$-,$T,$E,$S,$T,$-,$P,$I,$D).

port_prog_killer(EProc, OSProc) when is_pid(EProc), is_list(OSProc) ->
    process_flag(trap_exit, true),
    Ref = erlang:monitor(process, EProc),
    receive
        {'DOWN', Ref, _, _, Reason} when is_tuple(Reason),
                                         element(1, Reason)
                                         == timetrap_timeout ->
            Cmd = "kill -9 " ++ OSProc,
            io:format("Test case timed out. "
                      "Trying to kill port program.~n"
                      "  Executing: ~p~n", [Cmd]),
            case os:cmd(Cmd) of
                [] ->
                    ok;
                OsCmdRes ->
                    io:format("             ~s", [OsCmdRes])
            end;
        %% OSProc is assumed to have terminated by itself
        {'DOWN', Ref, _, _, _} ->
            ok
    end.

get_line(_Port, eol, Data) ->
    Data;
get_line(Port, noeol, Data) ->
    receive
	      {Port, {data, {Flag, NextData}}} ->
		  get_line(Port, Flag, Data ++ NextData);
	      {Port, eof} ->
		  ct:fail(port_prog_unexpectedly_closed)
	  end.

read_case_data(Port, TestCase) ->
    receive
        {Port, {data, {eol, [?SUCCESS_MARKER]}}} ->
            ok;
        {Port, {data, {Flag, [?SUCCESS_MARKER | CommentStart]}}} ->
            {comment, get_line(Port, Flag, CommentStart)};
        {Port, {data, {Flag, [?SKIPPED_MARKER | CommentStart]}}} ->
            {skipped, get_line(Port, Flag, CommentStart)};
        {Port, {data, {Flag, [?FAILED_MARKER | ReasonStart]}}} ->
            ct:fail(get_line(Port, Flag, ReasonStart));
        {Port, {data, {eol, [?PID_MARKER | PidStr]}}} ->
            io:format("Port program pid: ~s~n", [PidStr]),
            CaseProc = self(),
            _ = list_to_integer(PidStr), % Sanity check
            spawn_opt(fun () ->
                              port_prog_killer(CaseProc, PidStr)
                      end,
                      [{priority, max}, link]),
            read_case_data(Port, TestCase);
        {Port, {data, {Flag, LineStart}}} ->
            io:format("~s~n", [get_line(Port, Flag, LineStart)]),
            read_case_data(Port, TestCase);
        {Port, eof} ->
            ct:fail(port_prog_unexpectedly_closed)
    end.

run_case(Config, Test, TestArgs) ->
    run_case(Config, Test, TestArgs, fun (_Port) -> ok end).

run_case(Config, Test, TestArgs, Fun) ->
    TestProg = filename:join([proplists:get_value(data_dir, Config), ?TESTPROG]),
    Cmd = TestProg ++ " " ++ Test ++ " " ++ TestArgs,
    case catch open_port({spawn, Cmd}, [stream,
					use_stdio,
					stderr_to_stdout,
					eof,
					{line, 1024}]) of
	Port when is_port(Port) ->
	    Fun(Port),
	    CaseResult = read_case_data(Port, Test),
            receive
                {Port, eof} ->
                    ok
            end,
	    CaseResult;
	Error ->
	    ct:fail({open_port_failed, Error})
    end.
