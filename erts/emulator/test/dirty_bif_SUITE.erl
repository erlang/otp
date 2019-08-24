%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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

-module(dirty_bif_SUITE).

%%-define(line_trace,true).
-define(CHECK(Exp,Got), check(Exp,Got,?LINE)).
%%-define(CHECK(Exp,Got), Exp = Got).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2,
	 dirty_bif/1, dirty_bif_exception/1,
	 dirty_bif_multischedule/1,
	 dirty_bif_multischedule_exception/1,
	 dirty_scheduler_exit/1,
	 dirty_call_while_terminated/1,
	 dirty_heap_access/1,
	 dirty_process_info/1,
	 dirty_process_register/1,
	 dirty_process_trace/1,
	 code_purge/1,
         otp_15688/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

%%
%% All these tests utilize the debug BIFs:
%% - erts_debug:dirty_cpu/2 - Statically determined
%%   to (begin to) execute on a dirty CPU scheduler.
%% - erts_debug:dirty_io/2 - Statically determined
%%   to (begin to) execute on a dirty IO scheduler.
%% - erts_debug:dirty/3
%% Their implementations are located in
%% $ERL_TOP/erts/emulator/beam/beam_debug.c
%%

all() ->
    [dirty_bif,
     dirty_bif_multischedule,
     dirty_bif_exception,
     dirty_bif_multischedule_exception,
     dirty_scheduler_exit,
     dirty_call_while_terminated,
     dirty_heap_access,
     dirty_process_info,
     dirty_process_register,
     dirty_process_trace,
     code_purge,
     otp_15688].

init_per_suite(Config) ->
    case erlang:system_info(dirty_cpu_schedulers) of
	N when N > 0 ->
	    Config;
        _ ->
	    {skipped, "No dirty scheduler support"}
    end.

end_per_suite(_Config) ->
    ok.

init_per_testcase(Case, Config) ->
    [{testcase, Case} | Config].

end_per_testcase(_Case, _Config) ->
    ok.

dirty_bif(Config) when is_list(Config) ->
    dirty_cpu = erts_debug:dirty_cpu(scheduler,type),
    dirty_io = erts_debug:dirty_io(scheduler,type),
    normal = erts_debug:dirty(normal,scheduler,type),
    dirty_cpu = erts_debug:dirty(dirty_cpu,scheduler,type),
    dirty_io = erts_debug:dirty(dirty_io,scheduler,type),
    ok.

dirty_bif_multischedule(Config) when is_list(Config) ->
    ok = erts_debug:dirty_cpu(reschedule,1000),
    ok = erts_debug:dirty_io(reschedule,1000),
    ok = erts_debug:dirty(normal,reschedule,1000),
    ok.
    

dirty_bif_exception(Config) when is_list(Config) ->
    lists:foreach(fun (Error) ->
			  ErrorType = case Error of
					  _ when is_atom(Error) -> Error;
					  _ -> badarg
				      end,
			  try
			      erts_debug:dirty_cpu(error, Error),
			      ct:fail(expected_exception)
			  catch
			      error:ErrorType:Stk1 ->
				  [{erts_debug,dirty_cpu,[error, Error],_}|_] = Stk1,
				  ok
			  end,
			  try
			      apply(erts_debug,dirty_cpu,[error, Error]),
			      ct:fail(expected_exception)
			  catch
			      error:ErrorType:Stk2 ->
				  [{erts_debug,dirty_cpu,[error, Error],_}|_] = Stk2,
				  ok
			  end,
			  try
			      erts_debug:dirty_io(error, Error),
			      ct:fail(expected_exception)
			  catch
			      error:ErrorType:Stk3 ->
				  [{erts_debug,dirty_io,[error, Error],_}|_] = Stk3,
				  ok
			  end,
			  try
			      apply(erts_debug,dirty_io,[error, Error]),
			      ct:fail(expected_exception)
			  catch
			      error:ErrorType:Stk4 ->
				  [{erts_debug,dirty_io,[error, Error],_}|_] = Stk4,
				  ok
			  end,
			  try
			      erts_debug:dirty(normal, error, Error),
			      ct:fail(expected_exception)
			  catch
			      error:ErrorType:Stk5 ->
				  [{erts_debug,dirty,[normal, error, Error],_}|_] = Stk5,
				  ok
			  end,
			  try
			      apply(erts_debug,dirty,[normal, error, Error]),
			      ct:fail(expected_exception)
			  catch
			      error:ErrorType:Stk6 ->
				  [{erts_debug,dirty,[normal, error, Error],_}|_] = Stk6,
				  ok
			  end,
			  try
			      erts_debug:dirty(dirty_cpu, error, Error),
			      ct:fail(expected_exception)
			  catch
			      error:ErrorType:Stk7 ->
				  [{erts_debug,dirty,[dirty_cpu, error, Error],_}|_] = Stk7,
				  ok
			  end,
			  try
			      apply(erts_debug,dirty,[dirty_cpu, error, Error]),
			      ct:fail(expected_exception)
			  catch
			      error:ErrorType:Stk8 ->
				  [{erts_debug,dirty,[dirty_cpu, error, Error],_}|_] = Stk8,
				  ok
			  end,
			  try
			      erts_debug:dirty(dirty_io, error, Error),
			      ct:fail(expected_exception)
			  catch
			      error:ErrorType:Stk9 ->
				  [{erts_debug,dirty,[dirty_io, error, Error],_}|_] = Stk9,
				  ok
			  end,
			  try
			      apply(erts_debug,dirty,[dirty_io, error, Error]),
			      ct:fail(expected_exception)
			  catch
			      error:ErrorType:Stk10 ->
				  [{erts_debug,dirty,[dirty_io, error, Error],_}|_] = Stk10,
				  ok
			  end
		  end,
		  [badarg, undef, badarith, system_limit, noproc,
		   make_ref(), {another, "heap", term_to_binary("term")}]),
    ok.


dirty_bif_multischedule_exception(Config) when is_list(Config) ->
    try
	erts_debug:dirty_cpu(reschedule,1001)
    catch
	error:badarg:Stk1 ->
	    [{erts_debug,dirty_cpu,[reschedule, 1001],_}|_] = Stk1,
	    ok
    end,
    try
	erts_debug:dirty_io(reschedule,1001)
    catch
	error:badarg:Stk2 ->
	    [{erts_debug,dirty_io,[reschedule, 1001],_}|_] = Stk2,
	    ok
    end,
    try
	erts_debug:dirty(normal,reschedule,1001)
    catch
	error:badarg:Stk3 ->
	    [{erts_debug,dirty,[normal,reschedule,1001],_}|_] = Stk3,
	    ok
    end.

dirty_scheduler_exit(Config) when is_list(Config) ->
    {ok, Node} = start_node(Config, "+SDio 1"),
    [ok] = mcall(Node,
                 [fun() ->
                          %% Perform a dry run to ensure that all required code
                          %% is loaded. Otherwise the test will fail since code
                          %% loading is done through dirty IO and it won't make
                          %% any progress during this test.
                          _DryRun = test_dirty_scheduler_exit(),
			  Start = erlang:monotonic_time(millisecond),
                          ok = test_dirty_scheduler_exit(),
			  End = erlang:monotonic_time(millisecond),
			  io:format("Time=~p ms~n", [End-Start]),
			  ok
                  end]),
    stop_node(Node),
    ok.

test_dirty_scheduler_exit() ->
    process_flag(trap_exit,true),
    test_dse(10,[]).
test_dse(0,Pids) ->
    timer:sleep(100),
    kill_dse(Pids,[]);
test_dse(N,Pids) ->
    Pid = spawn_link(fun () -> erts_debug:dirty_io(wait, 1000) end),
    test_dse(N-1,[Pid|Pids]).

kill_dse([],Killed) ->
    wait_dse(Killed, ok);
kill_dse([Pid|Pids],AlreadyKilled) ->
    exit(Pid,kill),
    kill_dse(Pids,[Pid|AlreadyKilled]).

wait_dse([], Result) ->
    Result;
wait_dse([Pid|Pids], Result) ->
    receive
        {'EXIT', Pid, killed} -> wait_dse(Pids, Result);
        {'EXIT', Pid, _Other} -> wait_dse(Pids, failed)
    end.

dirty_call_while_terminated(Config) when is_list(Config) ->
    Me = self(),
    Bin = list_to_binary(lists:duplicate(4711, $r)),
    {value, {BinAddr, 4711, 1}} = lists:keysearch(4711, 2,
						  element(2,
							  process_info(self(),
								       binary))),
    {Dirty, DM} = spawn_opt(fun () ->
				    erts_debug:dirty_cpu(alive_waitexiting, Me),
				    blipp:blupp(Bin)
			    end,
			    [monitor,link]),
    receive {alive, Dirty} -> ok end,
    {value, {BinAddr, 4711, 2}} = lists:keysearch(4711, 2,
						  element(2,
							  process_info(self(),
								       binary))),
    Reason = die_dirty_process,
    OT = process_flag(trap_exit, true),
    exit(Dirty, Reason),
    receive
	{'DOWN', DM, process, Dirty, R0} ->
	    R0 = Reason
    end,
    receive
	{'EXIT', Dirty, R1} ->
	    R1 = Reason
    end,
    undefined = process_info(Dirty),
    undefined = process_info(Dirty, status),
    false = erlang:is_process_alive(Dirty),
    false = lists:member(Dirty, processes()),
    %% Binary still refered by Dirty process not yet cleaned up
    %% since the dirty bif has not yet returned...
    {value, {BinAddr, 4711, 2}} = lists:keysearch(4711, 2,
						  element(2,
							  process_info(self(),
								       binary))),
    receive after 2000 -> ok end,
    receive
	Msg ->
	    ct:fail({unexpected_message, Msg})
    after
	0 ->
	    ok
    end,
    {value, {BinAddr, 4711, 1}} = lists:keysearch(4711, 2,
						  element(2,
							  process_info(self(),
								       binary))),
    process_flag(trap_exit, OT),
    try
	blipp:blupp(Bin)
    catch
	_ : _ -> ok
    end.

dirty_heap_access(Config) when is_list(Config) ->
    {ok, Node} = start_node(Config),
    Me = self(),
    RGL = rpc:call(Node,erlang,whereis,[init]),
    Ref = rpc:call(Node,erlang,make_ref,[]),
    Dirty = spawn_link(fun () ->
			       Res = erts_debug:dirty_cpu(copy, Ref),
			       garbage_collect(),
			       Me ! {self(), Res},
			       receive after infinity -> ok end
		       end),
    {N, R} = access_dirty_heap(Dirty, RGL, 0, 0),
    receive
	{_Pid, Res} ->
	    1000 = length(Res),
	    lists:foreach(fun (X) -> Ref = X end, Res)
    end,
    unlink(Dirty),
    exit(Dirty, kill),
    stop_node(Node),
    {comment, integer_to_list(N) ++ " GL change loops; "
     ++ integer_to_list(R) ++ " while running dirty"}.

access_dirty_heap(Dirty, RGL, N, R) ->
    case process_info(Dirty, status) of
	{status, waiting} ->
	    {N, R};
	{status, Status} ->
	    {group_leader, GL} = process_info(Dirty, group_leader),
	    true = group_leader(RGL, Dirty),
	    {group_leader, RGL} = process_info(Dirty, group_leader),
	    true = group_leader(GL, Dirty),
	    {group_leader, GL} = process_info(Dirty, group_leader),
	    access_dirty_heap(Dirty, RGL, N+1, case Status of
						   running ->
						       R+1;
						   _ ->
						       R
					       end)
    end.

%% These tests verify that processes that access a process executing a
%% dirty BIF where the main lock is needed for that access do not get
%% blocked. Each test passes its pid to dirty_sleeper, which sends an
%% 'alive' message when it's running on a dirty scheduler and just before
%% it starts a 6 second sleep. When it receives the message, it verifies
%% that access to the dirty process is as it expects.  After the dirty
%% process finishes its 6 second sleep but before it returns from the dirty
%% scheduler, it sends a 'done' message. If the tester already received
%% that message, the test fails because it means attempting to access the
%% dirty process waited for that process to return to a regular scheduler,
%% so verify that we haven't received that message, and also verify that
%% the dirty process is still alive immediately after accessing it.
dirty_process_info(Config) when is_list(Config) ->
    access_dirty_process(
      Config,
      fun() -> ok end,
      fun(BifPid) ->
	      PI = process_info(BifPid),
	      {current_function,{erts_debug,dirty_io,2}} =
		  lists:keyfind(current_function, 1, PI),
	      ok
      end,
      fun(_) -> ok end).

dirty_process_register(Config) when is_list(Config) ->
    access_dirty_process(
      Config,
      fun() -> ok end,
      fun(BifPid) ->
	      register(test_dirty_process_register, BifPid),
	      BifPid = whereis(test_dirty_process_register),
	      unregister(test_dirty_process_register),
	      false = lists:member(test_dirty_process_register,
				   registered()),
	      ok
      end,
      fun(_) -> ok end).

dirty_process_trace(Config) when is_list(Config) ->
    access_dirty_process(
      Config,
      fun() ->
	      erlang:trace_pattern({erts_debug,dirty_io,2},
				   [{'_',[],[{return_trace}]}],
				   [local,meta]),
	      ok
      end,
      fun(BifPid) ->
	      erlang:trace(BifPid, true, [call,timestamp]),
	      ok
      end,
      fun(BifPid) ->
	      receive
		  {done, BifPid} ->
		      receive
			  {trace_ts,BifPid,call,{erts_debug,dirty_io,_},_} ->
			      ok
		      after
			  0 ->
			      error(missing_trace_call_message)
		      end %%,
		      %% receive
		      %% 	  {trace_ts,BifPid,return_from,{erts_debug,dirty_io,2},
		      %% 	   ok,_} ->
		      %% 	      ok
		      %% after
		      %% 	  100 ->
		      %% 	      error(missing_trace_return_message)
		      %% end
	      after
		  6500 ->
		      error(missing_done_message)
	      end,
	      ok
      end).

dirty_code_test_code() ->
    "
-module(dirty_code_test).

-export([func/1]).

func(Fun) ->
  Fun(),
  blipp:blapp().

".

code_purge(Config) when is_list(Config) ->
    Path = ?config(data_dir, Config),
    File = filename:join(Path, "dirty_code_test.erl"),
    ok = file:write_file(File, dirty_code_test_code()),
    {ok, dirty_code_test, Bin} = compile:file(File, [binary]),
    {module, dirty_code_test} = erlang:load_module(dirty_code_test, Bin),
    Start = erlang:monotonic_time(),
    {Pid1, Mon1} = spawn_monitor(fun () ->
				       dirty_code_test:func(fun () ->
								    %% Sleep for 6 seconds
								    %% in dirty bif...
								    erts_debug:dirty_io(wait,6000)
							    end)
			       end),
    {module, dirty_code_test} = erlang:load_module(dirty_code_test, Bin),
    {Pid2, Mon2} = spawn_monitor(fun () ->
				       dirty_code_test:func(fun () ->
								    %% Sleep for 6 seconds
								    %% in dirty bif...
								    erts_debug:dirty_io(wait,6000)
							    end)
				 end),
    receive
	{'DOWN', Mon1, process, Pid1, _} ->
	    ct:fail(premature_death)
    after 100 ->
	    ok
    end,
    true = erlang:purge_module(dirty_code_test),
    receive
	{'DOWN', Mon1, process, Pid1, Reason1} ->
	    killed = Reason1
    end,
    receive
	{'DOWN', Mon2, process, Pid2, _} ->
	    ct:fail(premature_death)
    after 100 ->
	    ok
    end,
    true = erlang:delete_module(dirty_code_test),
    receive
	{'DOWN', Mon2, process, Pid2, _} ->
	    ct:fail(premature_death)
    after 100 ->
	    ok
    end,
    true = erlang:purge_module(dirty_code_test),
    receive
	{'DOWN', Mon2, process, Pid2, Reason2} ->
	    killed = Reason2
    end,
    End = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(End-Start, native, milli_seconds),
    io:format("Time=~p~n", [Time]),
    true = Time =< 1000,
    ok.

otp_15688(Config) when is_list(Config) ->
    ImBack = make_ref(),
    {See, SeeMon} = spawn_monitor(fun () ->
                                          erts_debug:dirty_io(wait, 2000),
                                          exit(ImBack)
                                  end),
    wait_until(fun () ->
                       [{current_function, {erts_debug, dirty_io, 2}},
                        {status, running}]
                           == process_info(See,
                                           [current_function, status])
               end),
    {Ser1, Ser1Mon} = spawn_monitor(fun () ->
                                            erlang:suspend_process(See,
                                                                   [asynchronous])
                                    end),
    erlang:suspend_process(See, [asynchronous]),
    receive {'DOWN', Ser1Mon, process, Ser1, normal} -> ok end,

    %% Verify that we sent the suspend request while it was executing dirty...
    [{current_function, {erts_debug, dirty_io, 2}},
     {status, running}] = process_info(See, [current_function, status]),

    wait_until(fun () ->
                       {status, suspended} == process_info(See, status)
               end),
    erlang:resume_process(See),

    receive
        {'DOWN', SeeMon, process, See, Reason} ->
            ImBack = Reason
    after 4000 ->
            %% Resume bug seems to have hit us...
            PI = process_info(See),
            exit(See, kill),
            ct:fail({suspendee_stuck, PI})
    end.
    

%%
%% Internal...
%%

wait_until(Fun) ->
    case Fun() of
        true ->
            ok;
        _ ->
            receive after 100 -> ok end,
            wait_until(Fun)
    end.

access_dirty_process(Config, Start, Test, Finish) ->
    {ok, Node} = start_node(Config, ""),
    [ok] = mcall(Node,
		 [fun() ->
			  ok = test_dirty_process_access(Start, Test, Finish)
		  end]),
    stop_node(Node),
    ok.

test_dirty_process_access(Start, Test, Finish) ->
    ok = Start(),
    Self = self(),
    BifPid = spawn_link(fun() ->
				ok = erts_debug:dirty_io(ready_wait6_done, Self)
			end),
    ok = receive
	     {ready, BifPid} ->
		 ok = Test(BifPid),
		 receive
		     {done, BifPid} ->
			 error(dirty_process_info_blocked)
		 after
		     0 ->
			 true = erlang:is_process_alive(BifPid),
			 ok
		 end
	 after
	     3000 ->
		 error(timeout)
	 end,
    ok = Finish(BifPid).

start_node(Config) ->
    start_node(Config, "").

start_node(Config, Args) when is_list(Config) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Name = list_to_atom(atom_to_list(?MODULE)
			++ "-"
			++ atom_to_list(proplists:get_value(testcase, Config))
			++ "-"
			++ integer_to_list(erlang:system_time(second))
			++ "-"
			++ integer_to_list(erlang:unique_integer([positive]))),
    test_server:start_node(Name, slave, [{args, "-pa "++Pa++" "++Args}]).

stop_node(Node) ->
    test_server:stop_node(Node).

mcall(Node, Funs) ->
    Parent = self(),
    Refs = lists:map(fun (Fun) ->
                             Ref = make_ref(),
                             spawn_link(Node,
                                        fun () ->
                                                Res = Fun(),
                                                unlink(Parent),
                                                Parent ! {Ref, Res}
                                        end),
                             Ref
                     end, Funs),
    lists:map(fun (Ref) ->
                      receive
                          {Ref, Res} ->
                              Res
                      end
              end, Refs).
