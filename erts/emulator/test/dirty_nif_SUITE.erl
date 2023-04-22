%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2023. All Rights Reserved.
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
         init_per_group/2, end_per_group/2, groups/0,
	 dirty_nif/1, dirty_nif_send/1,
	 dirty_nif_exception/1, call_dirty_nif_exception/1,
	 dirty_scheduler_exit/1, dirty_call_while_terminated/1,
	 dirty_heap_access/1, dirty_process_info/1,
	 dirty_process_register/1, dirty_process_trace/1,
	 code_purge/1, literal_area/1, dirty_nif_send_traced/1,
	 nif_whereis/1, nif_whereis_parallel/1, nif_whereis_proxy/1,
         set_halt_options_from_nif/1,
         delay_halt/1,
         delay_halt_old_code/1,
         delay_halt_old_and_new_code/1,
         flush_false/1,
         on_halt/1,
         on_halt_old_code/1,
         on_halt_old_and_new_code/1,
         sync_halt/1,
         many_delay_halt/1,
         many_on_halt/1]).

-export([load_nif/2]).

-nifs([lib_loaded/0,
       call_dirty_nif/3,
       send_from_dirty_nif/1,
       send_wait_from_dirty_nif/1,
       call_dirty_nif_exception/1,
       call_dirty_nif_zero_args/0,
       dirty_call_while_terminated_nif/1,
       dirty_sleeper/0,
       dirty_sleeper/1,
       dirty_heap_access_nif/1,
       whereis_term/2,
       whereis_send/3,
       dirty_terminating_literal_access/2,
       delay_halt_normal/3,
       delay_halt_io_bound/3,
       delay_halt_cpu_bound/3,
       sync_halt_io_bound/2,
       sync_halt_cpu_bound/2,
       set_halt_option_from_nif_normal/1,
       set_halt_option_from_nif_io_bound/1,
       set_halt_option_from_nif_cpu_bound/1]).

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
     dirty_process_trace,
     code_purge,
     literal_area,
     dirty_nif_send_traced,
     nif_whereis,
     nif_whereis_parallel,
     {group, halt_normal},
     {group, halt_dirty_cpu},
     {group, halt_dirty_io},
     {group, halt_misc}].

halt_sched_tests() ->
    [set_halt_options_from_nif, delay_halt, delay_halt_old_code, delay_halt_old_and_new_code].
halt_dirty_sched_tests() ->
    [sync_halt, flush_false].

groups() ->
    [{halt_normal, [parallel], halt_sched_tests()},
     {halt_dirty_cpu, [parallel], halt_sched_tests()++halt_dirty_sched_tests()},
     {halt_dirty_io, [parallel], halt_sched_tests()++halt_dirty_sched_tests()},
     {halt_misc, [parallel], [on_halt, on_halt_old_code, on_halt_old_and_new_code, many_on_halt, many_delay_halt]}].

init_per_group(Group, Config) ->
    [{group, Group} | Config].

end_per_group(_, Config) ->
    proplists:delete(group, Config).

init_per_suite(Config) ->
    case lib_loaded() of
        false ->
            ok = erlang:load_nif(filename:join(?config(data_dir, Config),
                                               "dirty_nif_SUITE"), []);
        true ->
            ok
    end,
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(Case, Config) ->
    [{testcase, Case} | Config].

end_per_testcase(_Case, _Config) ->
    ok.

load_nif(NifLib, LibInfo) ->
    erlang:load_nif(NifLib, LibInfo).

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
	error:badarg:Stk1 ->
	    [{?MODULE,call_dirty_nif_exception,[1],_}|_] = Stk1,
	    ok
    end,
    try
	%% this checks that the expected exception occurs when the
	%% dirty NIF calls enif_make_badarg at some point but then
	%% returns a value that isn't an exception
	call_dirty_nif_exception(0),
	ct:fail(expected_badarg)
    catch
	error:badarg:Stk2 ->
	    [{?MODULE,call_dirty_nif_exception,[0],_}|_] = Stk2,
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
                            error:Term:Stk ->
                                [{?MODULE,NifFunc,[Term],_}|_] = Stk,
                                ok
                        end
                end, ok, ExcTerms).

dirty_scheduler_exit(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(["+SDio", "1"]),
    Path = proplists:get_value(data_dir, Config),
    NifLib = filename:join(Path, atom_to_list(?MODULE)),
    [ok] = mcall(Node,
                 [fun() ->
                          ok = erlang:load_nif(NifLib, []),
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
    peer:stop(Peer),
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
    %% Binary still referred by Dirty process not yet cleaned up
    %% since the dirty nif has not yet returned...
    {value, {BinAddr, 4711, 2}} = lists:keysearch(4711, 2,
						  element(2,
							  process_info(self(),
								       binary))),
    receive
	Msg ->
	    ct:fail({unexpected_message, Msg})
    after
	1000 ->
	    ok
    end,
    ok = wait_until(fun() ->
                       {value, {BinAddr, 4711, 1}} ==
                           lists:keysearch(4711, 2,
                                           element(2,
                                                   process_info(self(),
                                                                binary)))
               end,
               10000),
    process_flag(trap_exit, OT),
    try
	blipp:blupp(Bin)
    catch
	_ : _ -> ok
    end.

dirty_heap_access(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(),
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
    peer:stop(Peer),
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
%% it starts a 2 second sleep. When it receives the message, it verifies
%% that access to the dirty process is as it expects.  After the dirty
%% process finishes its 2 second sleep but before it returns from the dirty
%% scheduler, it sends a 'done' message. If the tester already received
%% that message, the test fails because it means attempting to access the
%% dirty process waited for that process to return to a regular scheduler,
%% so verify that we haven't received that message, and also verify that
%% the dirty process is still alive immediately after accessing it.
dirty_process_info(Config) when is_list(Config) ->
    access_dirty_process(
      ?FUNCTION_NAME,
      ?config(data_dir, Config),
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
      ?FUNCTION_NAME,
      ?config(data_dir, Config),
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
      ?FUNCTION_NAME,
      ?config(data_dir, Config),
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
		  2500 ->
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
								    %% Sleep for 2 seconds
								    %% in dirty nif...
								    dirty_sleeper()
							    end)
			       end),
    {module, dirty_code_test} = erlang:load_module(dirty_code_test, Bin),
    {Pid2, Mon2} = spawn_monitor(fun () ->
				       dirty_code_test:func(fun () ->
								    %% Sleep for 2 seconds
								    %% in dirty nif...
								    dirty_sleeper()
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
    literal_area_collector_test:check_idle(5000),
    ok.

dirty_nif_send_traced(Config) when is_list(Config) ->
    Parent = self(),
    Rcvr = spawn_link(fun() ->
			      Self = self(),
			      receive {ok, Self} -> ok end,
			      Parent ! {Self, received}
		      end),
    Sndr = spawn_link(fun () ->
			      receive {Parent, go} -> ok end,
			      {ok, Rcvr} = send_wait_from_dirty_nif(Rcvr),
			      Parent ! {self(), sent}
		      end),
    1 = erlang:trace(Sndr, true, [send, running, exiting]),
    Start = erlang:monotonic_time(),
    Sndr ! {self(), go},

    receive {Rcvr, received} -> ok end,
    End1 = erlang:monotonic_time(),
    Time1 = erlang:convert_time_unit(End1-Start, native, 1000),
    io:format("Time1: ~p milliseconds~n", [Time1]),
    true = Time1 < 500,
    receive {Sndr, sent} -> ok end,
    End2 = erlang:monotonic_time(),
    Time2 = erlang:convert_time_unit(End2-Start, native, 1000),
    io:format("Time2: ~p milliseconds~n", [Time2]),
    true = Time2 >= 1900,

    %% Make sure that the send trace is
    %% in between an in and and out trace
    (fun F() ->
             %% We got an in trace, look for out or send
             {trace,Sndr,in,_} = recv_trace_from(Sndr),
             case recv_trace_from(Sndr) of
                 {trace,Sndr,out,_} ->
                     %% We got an out, look for another in
                     F();
                 {trace,Sndr,send,_,_} ->
                     %% We got a send, look for another out
                     {trace,Sndr,out,_} = recv_trace_from(Sndr),
                     ok
             end
     end)(),

    ok.

recv_trace_from(Sndr) ->
    receive
        M when element(1, M) =:= trace;
               element(1, M) =:= trace_ts,
               element(2, M) =:= Sndr ->
            M
    end.

dirty_literal_test_code() ->
    "
-module(dirty_literal_code_test).

-export([get_literal/0]).

get_literal() ->
    {0,1,2,3,4,5,6,7,8,9}.

".

literal_area(Config) when is_list(Config) ->
    NifTMO = 3000,
    ExtraTMO = 1000,
    TotTMO = NifTMO+ExtraTMO,
    Path = ?config(data_dir, Config),
    File = filename:join(Path, "dirty_literal_code_test.erl"),
    ok = file:write_file(File, dirty_literal_test_code()),
    {ok, dirty_literal_code_test, Bin} = compile:file(File, [binary]),
    {module, dirty_literal_code_test} = erlang:load_module(dirty_literal_code_test, Bin),
    Me = self(),
    Fun = fun () ->
                  dirty_terminating_literal_access( 
                    Me,
                    dirty_literal_code_test:get_literal())
          end,
    {Pid, Mon} = spawn_monitor(Fun),
    receive {dirty_alive, Pid} -> ok end,
    exit(Pid, kill),
    Start = erlang:monotonic_time(millisecond),
    receive {'DOWN', Mon, process, Pid, killed} -> ok end,
    true = erlang:delete_module(dirty_literal_code_test),
    true = erlang:purge_module(dirty_literal_code_test),
    End = erlang:monotonic_time(millisecond),
    %% Wait for dirty_nif to do its access...
    TMO = case End - Start of
              T when T < TotTMO ->
                  TotTMO-T;
              _ ->
                  0
          end,
    receive after TMO -> ok end,
    literal_area_collector_test:check_idle(5000),
    {comment, "Waited "++integer_to_list(TMO)++" milliseconds after purge"}.

set_halt_options_from_nif(Config) when is_list(Config) ->
    case ?config(group, Config) of
        halt_normal ->
            error = set_halt_option_from_nif_normal(set_on_halt_handler),
            error = set_halt_option_from_nif_normal(delay_halt);
        halt_dirty_cpu ->
            error = set_halt_option_from_nif_cpu_bound(set_on_halt_handler),
            error = set_halt_option_from_nif_cpu_bound(delay_halt);
        halt_dirty_io ->
            error = set_halt_option_from_nif_io_bound(set_on_halt_handler),
            error = set_halt_option_from_nif_io_bound(delay_halt)
    end,
    ok.

delay_halt(Config) when is_list(Config) ->
    delay_halt(Config, new_code).

delay_halt_old_code(Config) when is_list(Config) ->
    delay_halt(Config, old_code).

delay_halt_old_and_new_code(Config) when is_list(Config) ->
    delay_halt(Config, old_and_new_code).

delay_halt(Config, Type) ->
    Suite = filename:join(?config(data_dir, Config), ?MODULE_STRING),
    Priv = proplists:get_value(priv_dir, Config),
    TypeSuffix = "_"++atom_to_list(Type),
    {Fun, FileName} = case ?config(group, Config) of
                          halt_normal ->
                              {fun delay_halt_normal/3, "delay_halt_normal"++TypeSuffix};
                          halt_dirty_io ->
                              {fun delay_halt_io_bound/3, "delay_halt_io_bound"++TypeSuffix};
                          halt_dirty_cpu ->
                              {fun delay_halt_cpu_bound/3, "delay_halt_cpu_bound"++TypeSuffix}
                      end,
    Tester = self(),
    NifFileName = filename:join(Priv, FileName),
    {ok, Peer, Node} = ?CT_PEER(),
    Mon = erlang:monitor(process, Peer),
    ok = erpc:call(Node, ?MODULE, load_nif, [Suite, {delay_halt}]),
    ok = erpc:call(Node, file, set_cwd, [Priv]),
    Proxy = spawn_link(Node,
                       fun () ->
                               receive
                                   {delay_halt, _} = Msg ->
                                       unlink(Tester),
                                       Tester ! Msg
                               end
                       end),
    Start = erlang:monotonic_time(millisecond),
    ok = erpc:cast(Node, fun () -> Fun(Proxy, FileName, 2) end),
    receive {delay_halt, Pid} when is_pid(Pid), Node == node(Pid) -> ok end,
    case Type of
        new_code ->
            ok;
        old_code ->
            true = erpc:call(Node, erlang, delete_module, [dirty_nif_SUITE]);
        old_and_new_code ->
            true = erpc:call(Node, erlang, delete_module, [dirty_nif_SUITE]),
            ok = erpc:call(Node, ?MODULE, load_nif, [Suite, {delay_halt}]),
            Proxy2 = spawn_link(Node,
                                fun () ->
                                        receive
                                            {delay_halt, _} = Msg ->
                                                unlink(Tester),
                                                Tester ! Msg
                                        end
                                end),
            ok = erpc:cast(Node, fun () -> Fun(Proxy2, FileName++"_new_code", 2) end),
            receive {delay_halt, Pid2} when is_pid(Pid2), Node == node(Pid2) -> ok end
    end,
    ok = erpc:cast(Node, erlang, halt, []),
    ok = wait_until(fun () ->
                            {ok, <<"ok">>} == file:read_file(NifFileName)
                                andalso
                                  (Type /= old_and_new_code
                                   orelse {ok, <<"ok">>} == file:read_file(NifFileName++"_new_code"))
                    end,
                    6000),
    Time = erlang:monotonic_time(millisecond) - Start,
    ct:log("~s time=~pms", [FileName, Time]),
    true = Time >= 2000,
    receive {'DOWN', Mon, process, Peer, _} -> ok end,
    ok.

flush_false(Config) when is_list(Config) ->
    Suite = filename:join(?config(data_dir, Config), ?MODULE_STRING),
    Priv = proplists:get_value(priv_dir, Config),
    {Fun, FileName} = case ?config(group, Config) of
                          halt_dirty_io ->
                              {fun delay_halt_io_bound/3, "flush_false_io_bound"};
                          halt_dirty_cpu ->
                              {fun delay_halt_cpu_bound/3, "flush_false_cpu_bound"}
                      end,
    Tester = self(),
    NifFileName = filename:join(Priv, FileName),
    OnHaltBaseName = FileName++"_on_halt",
    OnHaltFileName = filename:join(Priv, OnHaltBaseName),
    {ok, Peer, Node} = ?CT_PEER(),
    Mon = erlang:monitor(process, Peer),
    ok = erpc:call(Node, ?MODULE, load_nif, [Suite, {sync_halt, OnHaltBaseName, 1}]),
    ok = erpc:call(Node, file, set_cwd, [Priv]),
    Proxy = spawn_link(Node,
                       fun () ->
                               receive
                                   {delay_halt, _} = Msg ->
                                       unlink(Tester),
                                       Tester ! Msg
                               end
                       end),
    Start = erlang:monotonic_time(millisecond),
    ok = erpc:cast(Node, fun () -> Fun(Proxy, FileName, 1) end),
    receive {delay_halt, Pid} when is_pid(Pid), Node == node(Pid) -> ok end,
    ok = erpc:cast(Node, erlang, halt, [0, [{flush,false}]]),
    receive {'DOWN', Mon, process, Peer, _} -> ok end,
    Time = erlang:monotonic_time(millisecond) - Start,
    ct:log("~s time=~pms", [FileName, Time]),
    Wait = 3000-Time,
    if Wait > 0 -> receive after Wait -> ok end;
       true -> ok
    end,
    {error,enoent} = file:read_file(NifFileName),
    {error,enoent} = file:read_file(OnHaltFileName),
    ok.

many_delay_halt(Config) when is_list(Config) ->
    try
        many_delay_halt_test(Config)
    catch
        throw:{skip, _} = Skip ->
            Skip
    end.

many_delay_halt_test(Config) ->
    Suite = filename:join(?config(data_dir, Config), ?MODULE_STRING),
    Priv = proplists:get_value(priv_dir, Config),
    Tester = self(),
    {ok, Peer, Node} = ?CT_PEER(),
    Chk = fun () ->
                  case erlang:system_info(schedulers_online) of
                      1 -> throw({skip, "Too few schedulers online"});
                      _ -> ok
                  end,
                  case erlang:system_info(dirty_cpu_schedulers_online) of
                      1 -> throw({skip, "Too few dirty cpu schedulers online"});
                      _ -> ok
                  end,
                  case erlang:system_info(dirty_io_schedulers) of
                      1 -> throw({skip, "Too few dirty io schedulers online"});
                      _ -> ok
                  end
          end,
    try
        erpc:call(Node, Chk)
    catch
        throw:{skip, _} = Skip ->
            peer:stop(Peer),
            throw(Skip)
    end,
    Mon = erlang:monitor(process, Peer),
    ok = erpc:call(Node, ?MODULE, load_nif, [Suite, {delay_halt}]),
    ok = erpc:call(Node, file, set_cwd, [Priv]),
    ProxyFun = fun (Tag) ->
                       fun () ->
                               receive
                                   {delay_halt, _} = Msg ->
                                       unlink(Tester),
                                       Tester ! {Tag, Msg}
                               end
                       end
               end,
    [P1, P2, P3, P4, P5] = [spawn_link(Node, ProxyFun(X)) || X <- lists:seq(1, 5)],
    Start = erlang:monotonic_time(millisecond),
    ok = erpc:cast(Node, fun () -> delay_halt_io_bound(P1, "many_delay_halt_io2", 2) end),
    ok = erpc:cast(Node, fun () -> delay_halt_io_bound(P2, "many_delay_halt_io1", 1) end),
    ok = erpc:cast(Node, fun () -> delay_halt_cpu_bound(P3, "many_delay_halt_cpu1", 1) end),
    ok = erpc:cast(Node, fun () -> delay_halt_cpu_bound(P4, "many_delay_halt_cpu2", 2) end),
    _ = [receive
             {X, {delay_halt, Pid}} when is_pid(Pid), Node == node(Pid) ->
                 ok
         end || X <- lists:seq(1, 4)],
    ok = erpc:cast(Node, fun () -> delay_halt_normal(P5, "many_delay_halt_normal", 1) end),
    receive
        {5, {delay_halt, Pid}} when is_pid(Pid), Node == node(Pid) ->
            ok
    end,
    ok = erpc:cast(Node, erlang, halt, []),
    ok = wait_until(fun () ->
                            {ok, <<"ok">>} == file:read_file(filename:join(Priv, "many_delay_halt_io2"))
                                andalso {ok, <<"ok">>} == file:read_file(filename:join(Priv, "many_delay_halt_cpu2"))
                    end,
                    3000),
    {ok, <<"ok">>} = file:read_file(filename:join(Priv, "many_delay_halt_cpu1")),
    {ok, <<"ok">>} = file:read_file(filename:join(Priv, "many_delay_halt_io1")),
    {ok, <<"ok">>} = file:read_file(filename:join(Priv, "many_delay_halt_normal")),
    Time = erlang:monotonic_time(millisecond) - Start,
    ct:log("many_delay_halt time=~pms", [Time]),
    true = Time >= 2000,
    receive {'DOWN', Mon, process, Peer, _} -> ok end,
    ok.

on_halt(Config) when is_list(Config) ->
    on_halt(Config, new_code).

on_halt_old_code(Config) when is_list(Config) ->
    on_halt(Config, old_code).

on_halt_old_and_new_code(Config) when is_list(Config) ->
    on_halt(Config, old_and_new_code).

on_halt(Config, Type) ->
    Suite = filename:join(?config(data_dir, Config), ?MODULE_STRING),
    Priv = proplists:get_value(priv_dir, Config),
    FileName = "on_halt_"++atom_to_list(Type),
    OnHaltFileName = filename:join(Priv, FileName),
    {ok, Peer, Node} = ?CT_PEER(),
    Mon = erlang:monitor(process, Peer),
    ok = erpc:call(Node, ?MODULE, load_nif, [Suite, {on_halt, FileName, 1}]),
    case Type of
        new_code ->
            ok;
        old_code ->
            true = erpc:call(Node, erlang, delete_module, [dirty_nif_SUITE]);
        old_and_new_code ->
            true = erpc:call(Node, erlang, delete_module, [dirty_nif_SUITE]),
            ok = erpc:call(Node, ?MODULE, load_nif, [Suite, {on_halt, FileName++"_new", 1}])
    end,
    ok = erpc:call(Node, file, set_cwd, [Priv]),
    Start = erlang:monotonic_time(millisecond),
    ok = erpc:cast(Node, erlang, halt, []),
    ok = wait_until(fun () ->
                            {ok, <<"ok">>} == file:read_file(OnHaltFileName)
                                andalso (Type /= old_and_new_code
                                         orelse {ok, <<"ok">>} == file:read_file(OnHaltFileName++"_new"))
                    end,
                    3000),
    Time = erlang:monotonic_time(millisecond) - Start,
    ct:log("~s time=~pms", [FileName, Time]),
    if Type == old_and_new_code -> true = Time >= 2000;
       true -> true = Time >= 1000
       end,
    receive {'DOWN', Mon, process, Peer, _} -> ok end,
    ok.

on_halt_module_code_format() ->
    lists:flatten(["-module(~s).~n",
                   "-export([load/1, lib_loaded/0]).~n",
                   "-nifs([lib_loaded/0]).~n",
                   "load(SoFile) -> erlang:load_nif(SoFile, ?MODULE_STRING).~n",
                   "lib_loaded() -> false.~n"]).

many_on_halt(Config) when is_list(Config) ->
    DDir = ?config(data_dir, Config),
    Priv = proplists:get_value(priv_dir, Config),
    OnHaltModules = ["on_halt_a","on_halt_b","on_halt_c","on_halt_d","on_halt_e","on_halt_f"],
    DeleteOnHaltModules = ["on_halt_a","on_halt_c","on_halt_d","on_halt_f"],
    PurgeOnHaltModules = DeleteOnHaltModules -- ["on_halt_d"],
    ActiveOnHaltModules = OnHaltModules -- PurgeOnHaltModules,
    lists:foreach(fun (ModStr) ->
                          Code = io_lib:format(on_halt_module_code_format(), [ModStr]),
                          ok = file:write_file(filename:join(DDir, ModStr++".erl"), Code)
                  end,
                  OnHaltModules),
    {ok, Peer, Node} = ?CT_PEER(),
    Mon = erlang:monitor(process, Peer),
    ok = erpc:call(Node,
                   fun () ->
                           ok = file:set_cwd(Priv),
                           lists:foreach(fun (ModStr) ->
                                                 AbsModStr = filename:join(DDir, ModStr),
                                                 {ok,Mod,Bin} = compile:file(AbsModStr, [binary]),
                                                 {module, Mod} = erlang:load_module(Mod, Bin),
                                                 ok = Mod:load(AbsModStr),
                                                 true = Mod:lib_loaded()
                                         end,
                                         OnHaltModules),
                           lists:foreach(fun (ModStr) ->
                                                 Mod = list_to_atom(ModStr),
                                                 true = erlang:delete_module(Mod)
                                         end, DeleteOnHaltModules),
                           lists:foreach(fun (ModStr) ->
                                                 Mod = list_to_atom(ModStr),
                                                 true = erlang:purge_module(Mod)
                                         end, PurgeOnHaltModules),
                           ok
                   end),
    Start = erlang:monotonic_time(millisecond),
    ok = erpc:cast(Node, erlang, halt, []),
    ok = wait_until(fun () ->
                            try
                                lists:foreach(fun (ModStr) ->
                                                      FileName = filename:join(Priv, ModStr),
                                                      {ok, <<"ok">>} = file:read_file(FileName)
                                              end, ActiveOnHaltModules),
                                true
                            catch
                                _:_ ->
                                    false
                            end
                    end,
                    1000*(length(ActiveOnHaltModules)+1)),
    Time = erlang:monotonic_time(millisecond) - Start,
    ct:log("many_on_halt time=~pms", [Time]),
    true = Time >= length(ActiveOnHaltModules)*1000,
    receive {'DOWN', Mon, process, Peer, _} -> ok end,
    ok.

sync_halt(Config) when is_list(Config) ->
    Suite = filename:join(?config(data_dir, Config), ?MODULE_STRING),
    Priv = proplists:get_value(priv_dir, Config),
    {Fun, FileName} = case ?config(group, Config) of
                          halt_dirty_io ->
                              {fun sync_halt_io_bound/2, "sync_halt_io_bound"};
                          halt_dirty_cpu ->
                              {fun sync_halt_cpu_bound/2, "sync_halt_cpu_bound"}
                      end,
    Tester = self(),
    NifFileName = filename:join(Priv, FileName),
    OnHaltBaseFileName = FileName++".onhalt",
    OnHaltFileName = filename:join(Priv, OnHaltBaseFileName),
    {ok, Peer, Node} = ?CT_PEER(),
    Mon = erlang:monitor(process, Peer),
    ok = erpc:call(Node, ?MODULE, load_nif, [Suite, {sync_halt, OnHaltBaseFileName, 1}]),
    ok = erpc:call(Node, file, set_cwd, [Priv]),
    Proxy = spawn_link(Node,
                       fun () ->
                               receive
                                   {sync_halt, _} = Msg ->
                                       unlink(Tester),
                                       Tester ! Msg
                               end
                       end),
    ok = erpc:cast(Node, fun () -> Fun(Proxy, FileName) end),
    receive {sync_halt, Pid} when is_pid(Pid), Node == node(Pid) -> ok end,
    Start = erlang:monotonic_time(millisecond),
    ok = erpc:cast(Node, erlang, halt, []),
    ok = wait_until(fun () ->
                            {ok, <<"ok">>} == file:read_file(OnHaltFileName)
                    end,
                    2000),
    ok = wait_until(fun () ->
                            {ok, <<"ok">>} == file:read_file(NifFileName)
                    end,
                    4000),
    Time = erlang:monotonic_time(millisecond) - Start,
    ct:log("~s time=~pms", [FileName, Time]),
    true = Time >= 1000,
    receive {'DOWN', Mon, process, Peer, _} -> ok end,
    ok.

%%
%% Internal...
%%

access_dirty_process(TestCase, Path, Start, Test, Finish) ->
    {ok, Peer, Node} = ?CT_PEER(#{name => ?CT_PEER_NAME(TestCase)}),
    [ok] = mcall(Node,
		 [fun() ->
                          Lib = atom_to_list(?MODULE),
                          ok = erlang:load_nif(filename:join(Path,Lib), []),
			  ok = test_dirty_process_access(Start, Test, Finish)
		  end]),
    peer:stop(Peer),
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
	     1000 ->
		 error(timeout)
	 end,
    ok = Finish(NifPid).

receive_any() ->
    receive M -> M end.

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

%% Test enif_whereis_...
%% These tests are mostly identical to their counterparts in nif_SUITE.erl,
%% with just name and count changes in the first few lines.

nif_whereis(Config) when is_list(Config) ->
    erl_ddll:try_load(?config(data_dir, Config), echo_drv, []),

    RegName = dirty_nif_whereis_test_thing,
    undefined = erlang:whereis(RegName),
    false = whereis_term(pid, RegName),

    Mgr = self(),
    Ref = make_ref(),
    ProcMsg = {Ref, ?LINE},
    PortMsg = ?MODULE_STRING " whereis hello\n",

    {Pid, Mon} = spawn_monitor(?MODULE, nif_whereis_proxy, [Ref]),
    true = register(RegName, Pid),
    Pid = erlang:whereis(RegName),
    Pid = whereis_term(pid, RegName),
    false = whereis_term(port, RegName),
    false = whereis_term(pid, [RegName]),

    ok = whereis_send(pid, RegName, {forward, Mgr, ProcMsg}),
    ok = receive ProcMsg -> ok end,

    Pid ! {Ref, quit},
    ok = receive {'DOWN', Mon, process, Pid, normal} -> ok end,
    undefined = erlang:whereis(RegName),
    false = whereis_term(pid, RegName),

    Port = open_port({spawn, echo_drv}, [eof]),
    true = register(RegName, Port),
    Port = erlang:whereis(RegName),
    Port = whereis_term(port, RegName),
    false = whereis_term(pid, RegName),
    false = whereis_term(port, [RegName]),

    ok = whereis_send(port, RegName, PortMsg),
    ok = receive {Port, {data, PortMsg}} -> ok end,

    port_close(Port),
    undefined = erlang:whereis(RegName),
    false = whereis_term(port, RegName),
    ok.

nif_whereis_parallel(Config) when is_list(Config) ->

    %% try to be at least a little asymmetric
    NProcs = trunc(3.5 * erlang:system_info(schedulers)),
    NSeq = lists:seq(1, NProcs),
    Names = [list_to_atom("dirty_nif_whereis_proc_" ++ integer_to_list(N))
            || N <- NSeq],
    Mgr = self(),
    Ref = make_ref(),

    NotReg = fun(Name) ->
        erlang:whereis(Name) == undefined
    end,
    PidReg = fun({Name, Pid, _Mon}) ->
        erlang:whereis(Name) == Pid andalso whereis_term(pid, Name) == Pid
    end,
    RecvDown = fun({_Name, Pid, Mon}) ->
        receive {'DOWN', Mon, process, Pid, normal} -> true
        after   1500 -> false end
    end,
    RecvNum = fun(N) ->
        receive {N, Ref} -> true
        after   1500 -> false end
    end,

    true = lists:all(NotReg, Names),

    %% {Name, Pid, Mon}
    Procs = lists:map(
        fun(N) ->
            Name = lists:nth(N, Names),
            Prev = lists:nth((if N == 1 -> NProcs; true -> (N - 1) end), Names),
            Next = lists:nth((if N == NProcs -> 1; true -> (N + 1) end), Names),
            {Pid, Mon} = spawn_monitor(
                ?MODULE, nif_whereis_proxy, [{N, Ref, Mgr, [Prev, Next]}]),
            true = register(Name, Pid),
            {Name, Pid, Mon}
        end, NSeq),

    true = lists:all(PidReg, Procs),

    %% tell them all to 'fire' as fast as we can
    [P ! {Ref, send_proc} || {_, P, _} <- Procs],

    %% each gets forwarded through two processes
    true = lists:all(RecvNum, NSeq),
    true = lists:all(RecvNum, NSeq),

    %% tell them all to 'quit' by name
    [N ! {Ref, quit} || {N, _, _} <- Procs],
    true = lists:all(RecvDown, Procs),
    true = lists:all(NotReg, Names),
    ok.

%% exported to be spawned by MFA by whereis tests
nif_whereis_proxy({N, Ref, Mgr, Targets} = Args) ->
    receive
        {forward, To, Data} ->
            To ! Data,
            nif_whereis_proxy(Args);
        {Ref, quit} ->
            ok;
        {Ref, send_port} ->
            Msg = ?MODULE_STRING " whereis " ++ integer_to_list(N) ++ "\n",
            lists:foreach(
                fun(T) ->
                    ok = whereis_send(port, T, Msg)
                end, Targets),
            nif_whereis_proxy(Args);
        {Ref, send_proc} ->
            lists:foreach(
                fun(T) ->
                    ok = whereis_send(pid, T, {forward, Mgr, {N, Ref}})
                end, Targets),
            nif_whereis_proxy(Args)
    end;
nif_whereis_proxy(Ref) ->
    receive
        {forward, To, Data} ->
            To ! Data,
            nif_whereis_proxy(Ref);
        {Ref, quit} ->
            ok
    end.

wait_until(Fun, infinity) ->
    wait_until_aux(Fun, infinity);
wait_until(Fun, MaxTime) ->
    End = erlang:monotonic_time(millisecond) + MaxTime,
    wait_until_aux(Fun, End).

wait_until_aux(Fun, End) ->
    case Fun() of
        true ->
            ok;
        _ ->
            if End == infinity ->
                    receive after 100 -> ok end,
                    wait_until_aux(Fun, infinity);
               true ->
                    Now = erlang:monotonic_time(millisecond),
                    case End =< Now of
                        true ->
                            timeout;
                        _ ->
                            Wait = case End - Now of
                                       Short when End - Now < 100 ->
                                           Short;
                                       _ ->
                                           100
                                   end,
                            receive after Wait -> ok end,
                            wait_until_aux(Fun, End)
                    end
            end
    end.


%% The NIFs:
lib_loaded() -> false.
call_dirty_nif(_,_,_) -> ?nif_stub.
send_from_dirty_nif(_) -> ?nif_stub.
send_wait_from_dirty_nif(_) -> ?nif_stub.
call_dirty_nif_exception(_) -> ?nif_stub.
call_dirty_nif_zero_args() -> ?nif_stub.
dirty_call_while_terminated_nif(_) -> ?nif_stub.
dirty_sleeper() -> ?nif_stub.
dirty_sleeper(_) -> ?nif_stub.
dirty_heap_access_nif(_) -> ?nif_stub.
whereis_term(_Type,_Name) -> ?nif_stub.
whereis_send(_Type,_Name,_Msg) -> ?nif_stub.
dirty_terminating_literal_access(_Me, _Literal) -> ?nif_stub.
delay_halt_normal(_Pid, _FileName, _Delay) -> ?nif_stub.
delay_halt_io_bound(_Pid, _FileName, _Delay) -> ?nif_stub.
delay_halt_cpu_bound(_Pid, _FileName, _Delay) -> ?nif_stub.
sync_halt_io_bound(_Pid, _FileName) -> ?nif_stub.
sync_halt_cpu_bound(_Pid, _FileName) -> ?nif_stub.
set_halt_option_from_nif_normal(_Op) -> ?nif_stub.
set_halt_option_from_nif_io_bound(_Op) -> ?nif_stub.
set_halt_option_from_nif_cpu_bound(_Op) -> ?nif_stub.

nif_stub_error(Line) ->
    exit({nif_not_loaded,module,?MODULE,line,Line}).
