%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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
%%% File    : signal_SUITE.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : Test signals
%%%
%%% Created : 10 Jul 2006 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
-module(signal_SUITE).
-author('rickard.s.green@ericsson.com').

%-define(line_trace, 1).
-include_lib("common_test/include/ct.hrl").
-export([all/0, suite/0,init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

% Test cases
-export([xm_sig_order/1,
	 pending_exit_unlink_process/1,
	 pending_exit_unlink_dist_process/1,
	 pending_exit_unlink_port/1,
	 pending_exit_trap_exit/1,
	 pending_exit_receive/1,
	 pending_exit_exit/1,
	 pending_exit_gc/1,
	 pending_exit_is_process_alive/1,
	 pending_exit_process_display/1,
	 pending_exit_process_info_1/1,
	 pending_exit_process_info_2/1,
	 pending_exit_group_leader/1,
	 exit_before_pending_exit/1]).

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    available_internal_state(true),
    [{testcase, Func}|Config].

end_per_testcase(_Func, Config) ->
    ok.

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    available_internal_state(true),
    catch erts_debug:set_internal_state(not_running_optimization, true),
    available_internal_state(false).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() -> 
    [xm_sig_order, pending_exit_unlink_process,
     pending_exit_unlink_dist_process,
     pending_exit_unlink_port, pending_exit_trap_exit,
     pending_exit_receive, pending_exit_trap_exit,
     pending_exit_gc, pending_exit_is_process_alive,
     pending_exit_process_display,
     pending_exit_process_info_1,
     pending_exit_process_info_2, pending_exit_group_leader,
     exit_before_pending_exit].


%% Test that exit signals and messages are received in correct order
xm_sig_order(Config) when is_list(Config) ->
    LNode = node(),
    repeat(fun () -> xm_sig_order_test(LNode) end, 1000),
    {ok, RNode} = start_node(Config),
    repeat(fun () -> xm_sig_order_test(RNode) end, 1000),
    stop_node(RNode),
    ok.
    

xm_sig_order_test(Node) ->
    P = spawn(Node, fun () -> xm_sig_order_proc() end),
    M = erlang:monitor(process, P),
    P ! may_reach,
    P ! may_reach,
    P ! may_reach,
    exit(P, good_signal_order),
    P ! may_not_reach,
    P ! may_not_reach,
    P ! may_not_reach,
    receive
	      {'DOWN', M, process, P, R} ->
		  good_signal_order = R
	  end.

xm_sig_order_proc() ->
    receive
	may_not_reach -> exit(bad_signal_order);
	may_reach -> ok
    after 0 -> ok
    end,
    xm_sig_order_proc().

pending_exit_unlink_process(Config) when is_list(Config) ->
    pending_exit_test(self(), unlink).

pending_exit_unlink_dist_process(Config) when is_list(Config) ->
    {ok, Node} = start_node(Config),
    From = spawn(Node, fun () -> receive after infinity -> ok end end),
    Res = pending_exit_test(From, unlink),
    stop_node(Node),
    Res.

pending_exit_unlink_port(Config) when is_list(Config) ->
    pending_exit_test(hd(erlang:ports()), unlink).

pending_exit_trap_exit(Config) when is_list(Config) ->
    pending_exit_test(self(), trap_exit).

pending_exit_receive(Config) when is_list(Config) ->
    pending_exit_test(self(), 'receive').

pending_exit_exit(Config) when is_list(Config) ->
    pending_exit_test(self(), exit).

pending_exit_gc(Config) when is_list(Config) ->
    pending_exit_test(self(), gc).

pending_exit_test(From, Type) ->
    case catch erlang:system_info(smp_support) of
        true ->
            OTE = process_flag(trap_exit, true),
            Ref = make_ref(),
            Master = self(),
            ExitBySignal = case Type of
                               gc ->
                                   lists:duplicate(10000,
                                                   exit_by_signal);
                               _ ->
                                   exit_by_signal
                           end,
            Pid = spawn_link(
                    fun () ->
                            receive go -> ok end,
                            false = have_pending_exit(),
                            exit = fake_exit(From,
                                             self(),
                                             ExitBySignal),
                            true = have_pending_exit(),
                            Master ! {self(), Ref, Type},
                            case Type of
                                gc ->
                                    force_gc(),
                                    erlang:yield();
                                unlink ->
                                    unlink(From);
                                trap_exit ->
                                    process_flag(trap_exit, true);
                                'receive' ->
                                    receive _ -> ok
                                    after 0 -> ok
                                    end;
                                exit ->
                                    ok
                            end,
                            exit(exit_by_myself)
                    end),
            Mon = erlang:monitor(process, Pid),
            Pid ! go,
            Reason = receive
                         {'DOWN', Mon, process, Pid, R} ->
                             receive
                                 {Pid, Ref, Type} ->
                                     ok
                             after 0 ->
                                       ct:fail(premature_exit)
                             end,
                             case Type of
                                 exit ->
                                     exit_by_myself = R;
                                 _ ->
                                     ExitBySignal = R
                             end
                     end,
            receive
                {'EXIT', Pid, R2} ->
                    Reason = R2
            end,
            process_flag(trap_exit, OTE),
            ok,
            {comment, "Test only valid with current SMP emulator."};
        _ ->
            {skipped, "SMP support not enabled. Test only valid with current SMP emulator."}
    end.



exit_before_pending_exit(Config) when is_list(Config) ->
    %% This is a testcase testcase very specific to the smp
    %% implementation as it is of the time of writing.
    %%
    %% The testcase tries to check that a process can
    %% exit by itself even though it has a pending exit.
    OTE = process_flag(trap_exit, true),
    Master = self(),
    Tester = spawn_link(
               fun () ->
                       Opts = case {erlang:system_info(run_queues),
                                    erlang:system_info(schedulers_online)} of
                                  {RQ, SO} when RQ =:= 1; SO =:= 1 -> [];
                                  _ ->
                                      process_flag(scheduler, 1),
                                      [{scheduler, 2}]
                              end,
                       P = self(),
                       Exiter = spawn_opt(fun () ->
                                                  receive
                                                      {exit_me, P, R} ->
                                                          exit(P, R)
                                                  end
                                          end, Opts),
                       erlang:yield(),
                       Exiter ! {exit_me, self(), exited_by_exiter},
                       %% We want to get a pending exit
                       %% before we exit ourselves. We
                       %% don't want to be scheduled out
                       %% since we will then see the
                       %% pending exit.
                       %%
                       %% Do something that takes
                       %% relatively long time but
                       %% consumes few reductions...
                       repeat(fun() -> erlang:system_info(procs) end,10),
                       %% ... then exit.
                       Master ! {self(),
                                 pending_exit,
                                 have_pending_exit()},
                       exit(exited_by_myself)
               end),
    PendingExit = receive {Tester, pending_exit, PE} -> PE end,
    receive
	      {'EXIT', Tester, exited_by_myself} ->
		  process_flag(trap_exit, OTE),
		  ok;
	      Msg ->
		  ct:fail({unexpected_message, Msg})
	  end,
    NoScheds = integer_to_list(erlang:system_info(schedulers_online)),
    {comment,
     "Was "
     ++ case PendingExit of
	    true -> "";
	    false ->"*not*"
	end ++ " able to trigger a pending exit. "
     ++ "Running on " ++ NoScheds ++ " scheduler(s). "
     ++ "This test is only interesting with at least two schedulers."}.

-define(PE_INFO_REPEAT, 100).

pending_exit_is_process_alive(Config) when is_list(Config) ->
    S = exit_op_test_init(),
    TestFun = fun (P) -> false = is_process_alive(P) end,
    repeated_exit_op_test(TestFun, ?PE_INFO_REPEAT),
    verify_pending_exit_success(S),
    comment().

pending_exit_process_info_1(Config) when is_list(Config) ->
    S = exit_op_test_init(),
    TestFun = fun (P) ->
			    undefined = process_info(P)
		    end,
    repeated_exit_op_test(TestFun, ?PE_INFO_REPEAT),
    verify_pending_exit_success(S),
    comment().

pending_exit_process_info_2(Config) when is_list(Config) ->
    S0 = exit_op_test_init(),
    repeated_exit_op_test(fun (P) ->
					undefined = process_info(P, messages)
				end, ?PE_INFO_REPEAT),
    S1 = verify_pending_exit_success(S0),
    repeated_exit_op_test(fun (P) ->
					undefined = process_info(P, status)
				end, ?PE_INFO_REPEAT),
    S2 = verify_pending_exit_success(S1),
    repeated_exit_op_test(fun (P) ->
					undefined = process_info(P, links)
				end, ?PE_INFO_REPEAT),
    S3 = verify_pending_exit_success(S2),
    repeated_exit_op_test(fun (P) ->
					undefined = process_info(P, [messages])
				end, ?PE_INFO_REPEAT),
    S4 = verify_pending_exit_success(S3),
    repeated_exit_op_test(fun (P) ->
					undefined = process_info(P, [status])
				end, ?PE_INFO_REPEAT),
    S5 = verify_pending_exit_success(S4),
    repeated_exit_op_test(fun (P) ->
					undefined = process_info(P, [links])
				end, ?PE_INFO_REPEAT),
    S6 = verify_pending_exit_success(S5),
    repeated_exit_op_test(fun (P) ->
					undefined = process_info(P, [status,
								     links])
				end, ?PE_INFO_REPEAT),
    S7 = verify_pending_exit_success(S6),
    repeated_exit_op_test(fun (P) ->
					undefined = process_info(P, [messages,
								     status])
				end, ?PE_INFO_REPEAT),
    S8 = verify_pending_exit_success(S7),
    repeated_exit_op_test(fun (P) ->
					undefined = process_info(P, [messages,
								     links])
				end, ?PE_INFO_REPEAT),
    S9 = verify_pending_exit_success(S8),
    repeated_exit_op_test(
	    fun (P) ->
		    undefined = process_info(P, [message_queue_len,
						 status])
	    end, ?PE_INFO_REPEAT),
    S10 = verify_pending_exit_success(S9),
    repeated_exit_op_test(fun (P) ->
					undefined = process_info(P, [messages,
								     links,
								     status])
				end, ?PE_INFO_REPEAT),
    verify_pending_exit_success(S10),
    comment().

pending_exit_process_display(Config) when is_list(Config) ->
    S = exit_op_test_init(),
    TestFun = fun (P) ->
			    badarg = try
					 erlang:process_display(P, backtrace)
				     catch
					 error:badarg -> badarg
				     end
		    end,
    repeated_exit_op_test(TestFun, ?PE_INFO_REPEAT),
    verify_pending_exit_success(S),
    comment().

pending_exit_group_leader(Config) when is_list(Config) ->
    S = exit_op_test_init(),
    TestFun = fun (P) ->
			    badarg = try
					 group_leader(self(), P)
				     catch
					 error:badarg -> badarg
				     end
		    end,
    repeated_exit_op_test(TestFun, ?PE_INFO_REPEAT),
    verify_pending_exit_success(S),
    comment().

%%
%% -- Internal utils --------------------------------------------------------
%%
exit_op_test_init() ->
    put(no_pending_exit_success, 0),
    put(no_pending_exit_tries, 0),
    {case {erlang:system_info(run_queues),
	   erlang:system_info(schedulers_online)} of
	 {RQ, SO} when RQ =:= 1; SO =:= 1 -> false;
	 _ -> true
     end, 0, 0}.

verify_pending_exit_success({false, _, _} = S) ->
    S;
verify_pending_exit_success({true, S, T}) ->
    NewS = get(no_pending_exit_success),
    NewT = get(no_pending_exit_tries),
    case NewT =:= T of
	true -> ok;
	_ -> case NewS > S of
		 true -> ok;
		 _ -> exit(no_pending_exits)
	     end
    end,
    {true, NewS, NewT}.

comment() ->
    {comment,
     "Pending exit trigger ratio "
     ++ integer_to_list(get(no_pending_exit_success))
     ++ "/"
     ++ integer_to_list(get(no_pending_exit_tries))
     ++ "."
     ++ case get(not_running_opt_test) of
	    true -> " No 'not running optimization' to disable.";
	    _ -> ""
	end}.

repeated_exit_op_test(TestFun, N) ->
    WorkFun0 = fun () ->
		       lists:sort(lists:reverse(lists:seq(1, 1000)))
	       end,
    repeat(fun () -> exit_op_test(TestFun, WorkFun0) end, N),
    try erts_debug:set_internal_state(not_running_optimization, false) of
	Bool when Bool == true; Bool == false -> 
	    WorkFun1 = fun () ->
			       erts_debug:set_internal_state(sleep, 0),
			       lists:sort(lists:reverse(lists:seq(1, 1000)))
			    end,
	    repeat(fun () ->
			   exit_op_test(TestFun, WorkFun1)
		   end, N)
    catch
	error:notsup -> put(not_running_opt_test, true)
    after
	catch erts_debug:set_internal_state(not_running_optimization, true)
    end.

exit_op_test(TestFun, WorkFun) ->
    Opts = case {erlang:system_info(run_queues),
		 erlang:system_info(schedulers_online)} of
	       {RQ, SO} when RQ =:= 1; SO =:= 1 -> [];
	       _ ->
		   process_flag(scheduler, 1),
		   [{scheduler, 2}]
	   end,
    Master = self(),
    Going = make_ref(),
    P = spawn_opt(fun () ->
			  loop(10, WorkFun),
			  Master ! Going,
			  loop(infinity, WorkFun)
		  end, Opts),
    receive Going -> ok end,
    loop(10, WorkFun),
    erlang:yield(),
    exit(P, bang),
    PE0 = have_pending_exit(P),
    TestFun(P),
    PE = case PE0 of
	     true -> true;
	     _ -> false
	 end,
    case {PE, get(no_pending_exit_success), get(no_pending_exit_tries)} of
	{true, undefined, undefined} ->
	    put(no_pending_exit_success, 1),
	    put(no_pending_exit_tries, 1);
	{false, undefined, undefined} ->
	    put(no_pending_exit_success, 0),
	    put(no_pending_exit_tries, 1);
	{true, S, T} ->
	    put(no_pending_exit_success, S+1),
	    put(no_pending_exit_tries, T+1);
	{false, _S, T} ->
	    put(no_pending_exit_tries, T+1)
    end,
    ok.

loop(infinity, WorkFun) ->
    do_loop(infinity, WorkFun);
loop(0, _WorkFun) ->
    ok;
loop(N, WorkFun) when is_integer(N) ->
    do_loop(N-1, WorkFun).

do_loop(N, WorkFun) ->
    WorkFun(),
    loop(N, WorkFun).

repeat(_Fun, N) when is_integer(N), N =< 0 ->
    ok;
repeat(Fun, N) when is_integer(N)  ->
    Fun(),
    repeat(Fun, N-1).

start_node(Config) ->
    Name = list_to_atom(atom_to_list(?MODULE)
			++ "-" ++ atom_to_list(proplists:get_value(testcase, Config))
			++ "-" ++ integer_to_list(erlang:system_time(seconds))
			++ "-" ++ integer_to_list(erlang:unique_integer([positive]))),
    Pa = filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, slave, [{args,  "-pa " ++ Pa}]).

stop_node(Node) ->
    test_server:stop_node(Node).

have_pending_exit() ->
    have_pending_exit(self()).

have_pending_exit(Pid) ->
    erts_debug:get_internal_state({have_pending_exit, Pid}).

force_gc() ->
    erts_debug:set_internal_state(force_gc, self()).

fake_exit(From, To, Reason) ->
    erts_debug:set_internal_state(send_fake_exit_signal, {From, To, Reason}).

available_internal_state(Bool) when Bool == true; Bool == false ->
    case {Bool,
          (catch erts_debug:get_internal_state(available_internal_state))} of
        {true, true} ->
            true;
        {false, true} ->
            erts_debug:set_internal_state(available_internal_state, false),
            true;
        {true, _} ->
            erts_debug:set_internal_state(available_internal_state, true),
            false;
        {false, _} ->
            false
    end.
