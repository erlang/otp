%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2014. All Rights Reserved.
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

-module(dirty_nif_SUITE).

%%-define(line_trace,true).
-define(CHECK(Exp,Got), check(Exp,Got,?LINE)).
%%-define(CHECK(Exp,Got), Exp = Got).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2,
	 dirty_nif/1, dirty_nif_send/1,
	 dirty_nif_exception/1, call_dirty_nif_exception/1,
	 dirty_scheduler_exit/1, dirty_call_while_terminated/1,
	 dirty_heap_access/1, dirty_process_info/1,
	 dirty_process_register/1, dirty_process_trace/1]).

-define(nif_stub,nif_stub_error(?LINE)).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [dirty_nif,
     dirty_nif_send,
     dirty_nif_exception,
     dirty_scheduler_exit,
     dirty_call_while_terminated,
     dirty_heap_access,
     dirty_process_info,
     dirty_process_register,
     dirty_process_trace].

init_per_suite(Config) ->
    try erlang:system_info(dirty_cpu_schedulers) of
	N when is_integer(N), N > 0 ->
	    case lib_loaded() of
		false ->
		    ok = erlang:load_nif(
			   filename:join(?config(data_dir, Config),
					 "dirty_nif_SUITE"), []);
		true ->
		    ok
	    end,
	    Config
    catch _:_ ->
	    {skipped, "No dirty scheduler support"}
    end.

end_per_suite(_Config) ->
    ok.

init_per_testcase(Case, Config) ->
    [{testcase, Case} | Config].

end_per_testcase(_Case, _Config) ->
    ok.

dirty_nif(Config) when is_list(Config) ->
    Val1 = 42,
    Val2 = "Erlang",
    Val3 = list_to_binary([Val2, 0]),
    {Val1, Val2, Val3} = call_dirty_nif(Val1, Val2, Val3),
    LargeArray = lists:duplicate(1000, ok),
    LargeArray = call_dirty_nif_zero_args(),
    ok.

dirty_nif_send(Config) when is_list(Config) ->
    Parent = self(),
    Pid = spawn_link(fun() ->
			     Self = self(),
			     {ok, Self} = receive_any(),
			     Parent ! {ok, Self}
		     end),
    {ok, Pid} = send_from_dirty_nif(Pid),
    {ok, Pid} = receive_any(),
    ok.

dirty_nif_exception(Config) when is_list(Config) ->
    try
	%% this checks that the expected exception occurs when the
	%% dirty NIF returns the result of enif_make_badarg
	%% directly
	call_dirty_nif_exception(1),
	ct:fail(expected_badarg)
    catch
	error:badarg ->
	    [{?MODULE,call_dirty_nif_exception,[1],_}|_] =
		erlang:get_stacktrace(),
	    ok
    end,
    try
	%% this checks that the expected exception occurs when the
	%% dirty NIF calls enif_make_badarg at some point but then
	%% returns a value that isn't an exception
	call_dirty_nif_exception(0),
	ct:fail(expected_badarg)
    catch
	error:badarg ->
	    [{?MODULE,call_dirty_nif_exception,[0],_}|_] =
		erlang:get_stacktrace(),
	    ok
    end,
    %% this checks that a dirty NIF can raise various terms as
    %% exceptions
    ok = nif_raise_exceptions(call_dirty_nif_exception).

nif_raise_exceptions(NifFunc) ->
    ExcTerms = [{error, test}, "a string", <<"a binary">>,
                42, [1,2,3,4,5], [{p,1},{p,2},{p,3}]],
    lists:foldl(fun(Term, ok) ->
                        try
                            erlang:apply(?MODULE,NifFunc,[Term]),
                            ct:fail({expected,Term})
                        catch
                            error:Term ->
                                [{?MODULE,NifFunc,[Term],_}|_] = erlang:get_stacktrace(),
                                ok
                        end
                end, ok, ExcTerms).

dirty_scheduler_exit(Config) when is_list(Config) ->
    {ok, Node} = start_node(Config, "+SDio 1"),
    Path = proplists:get_value(data_dir, Config),
    NifLib = filename:join(Path, atom_to_list(?MODULE)),
    [ok] = mcall(Node,
                 [fun() ->
                          ok = erlang:load_nif(NifLib, []),
			  Start = erlang:monotonic_time(milli_seconds),
                          ok = test_dirty_scheduler_exit(),
			  End = erlang:monotonic_time(milli_seconds),
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
    Pid = spawn_link(fun dirty_sleeper/0),
    test_dse(N-1,[Pid|Pids]).

kill_dse([],Killed) ->
    wait_dse(Killed);
kill_dse([Pid|Pids],AlreadyKilled) ->
    exit(Pid,kill),
    kill_dse(Pids,[Pid|AlreadyKilled]).

wait_dse([]) ->
    ok;
wait_dse([Pid|Pids]) ->
    receive
        {'EXIT',Pid,Reason} ->
	    killed = Reason
    end,
    wait_dse(Pids).

dirty_call_while_terminated(Config) when is_list(Config) ->
    Me = self(),
    Bin = list_to_binary(lists:duplicate(4711, $r)),
    {value, {BinAddr, 4711, 1}} = lists:keysearch(4711, 2,
						  element(2,
							  process_info(self(),
								       binary))),
    {Dirty, DM} = spawn_opt(fun () ->
				    dirty_call_while_terminated_nif(Me),
				    blipp:blupp(Bin)
			    end,
			    [monitor,link]),
    receive {dirty_alive, _Pid} -> ok end,
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
    %% since the dirty nif has not yet returned...
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
    ok.

dirty_heap_access(Config) when is_list(Config) ->
    {ok, Node} = start_node(Config),
    Me = self(),
    RGL = rpc:call(Node,erlang,whereis,[init]),
    Ref = rpc:call(Node,erlang,make_ref,[]),
    Dirty = spawn_link(fun () ->
			       Res = dirty_heap_access_nif(Ref),
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
%% dirty NIF where the main lock is needed for that access do not get
%% blocked. Each test passes its pid to dirty_sleeper, which sends a
%% 'ready' message when it's running on a dirty scheduler and just before
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
      fun(NifPid) ->
	      PI = process_info(NifPid),
	      {current_function,{?MODULE,dirty_sleeper,1}} =
		  lists:keyfind(current_function, 1, PI),
	      ok
      end,
      fun(_) -> ok end).

dirty_process_register(Config) when is_list(Config) ->
    access_dirty_process(
      Config,
      fun() -> ok end,
      fun(NifPid) ->
	      register(test_dirty_process_register, NifPid),
	      NifPid = whereis(test_dirty_process_register),
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
	      erlang:trace_pattern({?MODULE,dirty_sleeper,1},
				   [{'_',[],[{return_trace}]}],
				   [local,meta]),
	      ok
      end,
      fun(NifPid) ->
	      erlang:trace(NifPid, true, [call,timestamp]),
	      ok
      end,
      fun(NifPid) ->
	      receive
		  done ->
		      receive
			  {trace_ts,NifPid,call,{?MODULE,dirty_sleeper,_},_} ->
			      ok
		      after
			  0 ->
			      error(missing_trace_call_message)
		      end,
		      receive
			  {trace_ts,NifPid,return_from,{?MODULE,dirty_sleeper,1},
			   ok,_} ->
			      ok
		      after
			  100 ->
			      error(missing_trace_return_message)
		      end
	      after
		  6500 ->
		      error(missing_done_message)
	      end,
	      ok
      end).

%%
%% Internal...
%%

access_dirty_process(Config, Start, Test, Finish) ->
    {ok, Node} = start_node(Config, ""),
    [ok] = mcall(Node,
		 [fun() ->
                          Path = ?config(data_dir, Config),
                          Lib = atom_to_list(?MODULE),
                          ok = erlang:load_nif(filename:join(Path,Lib), []),
			  ok = test_dirty_process_access(Start, Test, Finish)
		  end]),
    stop_node(Node),
    ok.

test_dirty_process_access(Start, Test, Finish) ->
    ok = Start(),
    Self = self(),
    NifPid = spawn_link(fun() ->
				ok = dirty_sleeper(Self)
			end),
    ok = receive
	     ready ->
		 ok = Test(NifPid),
		 receive
		     done ->
			 error(dirty_process_info_blocked)
		 after
		     0 ->
			 true = erlang:is_process_alive(NifPid),
			 ok
		 end
	 after
	     3000 ->
		 error(timeout)
	 end,
    ok = Finish(NifPid).

receive_any() ->
    receive M -> M end.

start_node(Config) ->
    start_node(Config, "").

start_node(Config, Args) when is_list(Config) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Name = list_to_atom(atom_to_list(?MODULE)
			++ "-"
			++ atom_to_list(proplists:get_value(testcase, Config))
			++ "-"
			++ integer_to_list(erlang:system_time(seconds))
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

%% The NIFs:
lib_loaded() -> false.
call_dirty_nif(_,_,_) -> ?nif_stub.
send_from_dirty_nif(_) -> ?nif_stub.
call_dirty_nif_exception(_) -> ?nif_stub.
call_dirty_nif_zero_args() -> ?nif_stub.
dirty_call_while_terminated_nif(_) -> ?nif_stub.
dirty_sleeper() -> ?nif_stub.
dirty_sleeper(_) -> ?nif_stub.
dirty_heap_access_nif(_) -> ?nif_stub.

nif_stub_error(Line) ->
    exit({nif_not_loaded,module,?MODULE,line,Line}).
